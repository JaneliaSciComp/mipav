package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.util.DoubleDouble;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;


/**
 * <p>
 * This module computes Bessel functions of complex arguments and a nonnegative order. This work is the port of FORTRAN
 * code written by Donald E. Amos at the Sandia National Laboratories. This is an installation of his Algorithm 644,
 * Collected Algorithms form ACM. The original FORTRAN code was downloaded from http://www.netlib.org/toms/644. The
 * decision to port this code was prompted by the fact that the MATLAB links to this Amos FORTRAN package. For
 * DoubleDoublechecking this code 2 possibilities exist: 1.) The 6 self tests included in the package - zqcbh, zqcbi, zqcbj,
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
public class BesselEP {

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
    private DoubleDouble alim;

    /** DOCUMENT ME! */
    private final int BesselType;

    /** DOCUMENT ME! */
    private DoubleDouble[] cyi;

    /** DOCUMENT ME! */
    private DoubleDouble[] cyr;

    /** order of derivative, 0 or 1. */
    private int derivativeOrder;

    /** DOCUMENT ME! */
    private boolean doTest = false;

    /** I1MACH(14) = 106. */
    private int DoubleDoubleDigits = 106;

    /** DOCUMENT ME! */
    private DoubleDouble elim;

    /** I1MACH(16) = 1024 The largest exponent for DoubleDouble precision. */
    private int emax = 1024;

    /** I1MACH(15) = -1021; The smallest exponent for DoubleDouble precision. */
    private int emin = -1021;

    /** D1MACH(4). */
    /**
	 * The smallest representable relative difference between two {link @ DoubleDouble} values
	 */
	private DoubleDouble epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */
	

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
    private DoubleDouble fnul;

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

    /** D1MACH(5) = log10(2). = log10(e) * loge(2.0)*/
    //private DoubleDouble r1m5 = DoubleDouble.valueOf(0.4342944819032518276511289).multiply((DoubleDouble.valueOf(2.0)).log());
    private DoubleDouble r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);

    /** DOCUMENT ME! */
    private DoubleDouble rl;

    /**
     * UNSCALED_FUNCTION returns cy[j-1] = besselFunction(initialOrder+j-1,z), j = 1,...,sequenceNumber SCALED_FUNCTION
     * returns cy[j-1] = besselFunction(initialOrder+j-1,z)*exp(-abs(Real(z))), j = 1,...,sequenceNumber.
     */
    private int scalingOption;

    /** number of members of the sequence. */
    private int sequenceNumber;

    /** 2**-1022 = D1MACH(1). */
    //private DoubleDouble tiny = Math.pow(2, -1022);
    //private DoubleDouble tiny = (DoubleDouble.valueOf(2.0)).pow(DoubleDouble.valueOf(-1022.0));
    private DoubleDouble tiny = DoubleDouble.valueOf(2.2250738585072014E-308);
    

    /** DOCUMENT ME! */
    private DoubleDouble tol;

    /** DOCUMENT ME! */
    private double zi;

    /** zr and zi are the real and imaginary parts of the complex argument. */
    private double zr;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new BesselEP object.
     * 
     * @param BesselType DOCUMENT ME!
     * @param overflowTest DOCUMENT ME!
     */
    public BesselEP(final int BesselType, final boolean overflowTest) {
        this.BesselType = BesselType;
        this.overflowTest = overflowTest;
        doTest = true;
    }

    /**
     * This constructor is used for Ai and Bi Airy functions.
     * 
     * @param BesselType int
     * @param zr double
     * @param zi double
     * @param derivativeOrder int
     * @param scalingOption int
     * @param cyr DoubleDouble[]
     * @param cyi DoubleDouble[]
     * @param nz int[]
     * @param errorFlag int[]
     */
    public BesselEP(final int BesselType, final double zr, final double zi, final int derivativeOrder,
            final int scalingOption, final DoubleDouble[] cyr, final DoubleDouble cyi[], final int[] nz, final int[] errorFlag) {
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
     * This constructor used for I, J, K, and Y Bessel functions.
     * 
     * @param BesselType int
     * @param zr double
     * @param zi double
     * @param initialOrder double
     * @param scalingOption int
     * @param sequenceNumber int
     * @param cyr DoubleDouble[]
     * @param cyi DoubleDouble[]
     * @param nz int[]
     * @param errorFlag int[]
     */
    public BesselEP(final int BesselType, final double zr, final double zi, final double initialOrder,
            final int scalingOption, final int sequenceNumber, final DoubleDouble[] cyr, final DoubleDouble[] cyi, final int[] nz,
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
     * @param cyr DoubleDouble[]
     * @param cyi DoubleDouble[]
     * @param nz int[]
     * @param errorFlag int[]
     */
    public BesselEP(final int BesselType, final double zr, final double zi, final double initialOrder,
            final int scalingOption, final int HankelKind, final int sequenceNumber, final DoubleDouble[] cyr,
            final DoubleDouble[] cyi, final int[] nz, final int[] errorFlag) {
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
        DoubleDouble[] cwrkr;
        DoubleDouble[] cwrki;

        if (doTest) {
            runTest();

            return;
        }

        if (errorFlag == null) {
            MipavUtil.displayError("Array errorFlag must not be null");

            return;
        }

        if ( (BesselType != BesselEP.BESSEL_H) && (BesselType != BesselEP.BESSEL_I) && (BesselType != BesselEP.BESSEL_J)
                && (BesselType != BesselEP.BESSEL_K) && (BesselType != BesselEP.BESSEL_Y) && (BesselType != BesselEP.AIRY_AI)
                && (BesselType != BesselEP.AIRY_BI)) {
            MipavUtil
                    .displayError("Bessel type must be Airy Ai, Airy BI, BESSEL_H, BESSEL_I, BESSEL_J, BESSEL_K, or BESSEL_Y");
            errorFlag[0] = 1;

            return;
        }

        if ( (BesselType != BesselEP.AIRY_AI) && (BesselType != BesselEP.AIRY_BI)) {

            if (initialOrder < 0) {
                MipavUtil.displayError("initialOrder must be >= 0.0");
                errorFlag[0] = 1;

                return;
            }
        }

        if ( (BesselType == BesselEP.AIRY_AI) || (BesselType == BesselEP.AIRY_BI)) {

            if ( (derivativeOrder < 0) || (derivativeOrder > 1)) {
                MipavUtil.displayError("Derivative order can only be 0 or 1");
                errorFlag[0] = 1;

                return;
            }
        }

        if ( (scalingOption != BesselEP.UNSCALED_FUNCTION) && (scalingOption != BesselEP.SCALED_FUNCTION)) {
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

        if (BesselType != BesselEP.AIRY_BI) {

            if (nz == null) {
                MipavUtil.displayError("Array nz must not be null");
                errorFlag[0] = 1;

                return;
            }
        }

        errorFlag[0] = 0;

        if (BesselType != BesselEP.AIRY_BI) {
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

                zbesh(DoubleDouble.valueOf(zr), DoubleDouble.valueOf(zi), DoubleDouble.valueOf(initialOrder),
                		scalingOption, HankelKind, sequenceNumber, cyr, cyi, nz, errorFlag);
                break;

            case BESSEL_I:
                zbesi(DoubleDouble.valueOf(zr), DoubleDouble.valueOf(zi), DoubleDouble.valueOf(initialOrder), 
                		scalingOption, sequenceNumber, cyr, cyi, nz, errorFlag);
                break;

            case BESSEL_J:
                zbesj(DoubleDouble.valueOf(zr), DoubleDouble.valueOf(zi), DoubleDouble.valueOf(initialOrder), 
                		scalingOption, sequenceNumber, cyr, cyi, nz, errorFlag);
                break;

            case BESSEL_K:
                if ( (zr == 0.0) && (zi == 0.0)) {
                    MipavUtil.displayError("BesselK must not have both zr and zi = 0.0");
                    errorFlag[0] = 1;

                    return;
                } // if ((zr == 0.00 && (zi == 0.0))

                zbesk(DoubleDouble.valueOf(zr), DoubleDouble.valueOf(zi), DoubleDouble.valueOf(initialOrder),
                		scalingOption, sequenceNumber, cyr, cyi, nz, errorFlag);
                break;

            case BESSEL_Y:
                cwrkr = new DoubleDouble[sequenceNumber];
                cwrki = new DoubleDouble[sequenceNumber];
                if ( (zr == 0.0) && (zi == 0.0)) {
                    MipavUtil.displayError("BesselY must not have both zr and zi = 0.0");
                    errorFlag[0] = 1;

                    return;
                } // if ((zr == 0.00 && (zi == 0.0))

                zbesy(DoubleDouble.valueOf(zr), DoubleDouble.valueOf(zi), DoubleDouble.valueOf(initialOrder), 
                		scalingOption, sequenceNumber, cyr, cyi, nz, cwrkr, cwrki, errorFlag);
                break;

            case AIRY_AI:
                zairy(DoubleDouble.valueOf(zr), DoubleDouble.valueOf(zi), derivativeOrder, scalingOption, cyr, cyi, nz, errorFlag);
                break;

            case AIRY_BI:
                zbiry(DoubleDouble.valueOf(zr), DoubleDouble.valueOf(zi), derivativeOrder, scalingOption, cyr, cyi, errorFlag);
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
     * @param z DoubleDouble, z > 0.0
     * @param ierr int[] error flag IERR=0, NORMAL RETURN, COMPUTATION COMPLETED IERR=1, Z.LE.0.0D0, NO COMPUTATION
     * 
     * @return DoubleDouble NATURAL LOG OF THE GAMMA FUNCTION AT Z.NE.0.0D0
     */
    private DoubleDouble dgamln(final DoubleDouble z, final int[] ierr) {
        final DoubleDouble[] gln = new DoubleDouble[] {DoubleDouble.valueOf(0.00000000000000000E+00), 
        		DoubleDouble.valueOf(0.00000000000000000E+00), DoubleDouble.valueOf(6.93147180559945309E-01),
                DoubleDouble.valueOf(1.79175946922805500E+00), DoubleDouble.valueOf(3.17805383034794562E+00),
                DoubleDouble.valueOf(4.78749174278204599E+00), DoubleDouble.valueOf(6.57925121201010100E+00),
                DoubleDouble.valueOf(8.52516136106541430E+00), DoubleDouble.valueOf(1.06046029027452502E+01),
                DoubleDouble.valueOf(1.28018274800814696E+01), DoubleDouble.valueOf(1.51044125730755153E+01),
                DoubleDouble.valueOf(1.75023078458738858E+01), DoubleDouble.valueOf(1.99872144956618861E+01), 
                DoubleDouble.valueOf(2.25521638531234229E+01), DoubleDouble.valueOf(2.51912211827386815E+01),
                DoubleDouble.valueOf(2.78992713838408916E+01), DoubleDouble.valueOf(3.06718601060806728E+01),
                DoubleDouble.valueOf(3.35050734501368889E+01), DoubleDouble.valueOf(3.63954452080330536E+01),
                DoubleDouble.valueOf(3.93398841871994940E+01), DoubleDouble.valueOf(4.23356164607534850E+01),
                DoubleDouble.valueOf(4.53801388984769080E+01), DoubleDouble.valueOf(4.84711813518352239E+01),
                DoubleDouble.valueOf(5.16066755677643736E+01), DoubleDouble.valueOf(5.47847293981123192E+01),
                DoubleDouble.valueOf(5.80036052229805199E+01), DoubleDouble.valueOf(6.12617017610020020E+01),
                DoubleDouble.valueOf(6.45575386270063311E+01), DoubleDouble.valueOf(6.78897431371815350E+01),
                DoubleDouble.valueOf(7.12570389671680090E+01), DoubleDouble.valueOf(7.46582363488301644E+01),
                DoubleDouble.valueOf(7.80922235533153106E+01), DoubleDouble.valueOf(8.15579594561150372E+01), 
                DoubleDouble.valueOf(8.50544670175815174E+01), DoubleDouble.valueOf(8.85808275421976788E+01),
                DoubleDouble.valueOf(9.21361756036870925E+01), DoubleDouble.valueOf(9.57196945421432025E+01), 
                DoubleDouble.valueOf(9.93306124547874269E+01), DoubleDouble.valueOf(1.02968198614513813E+02),
                DoubleDouble.valueOf(1.06631760260643459E+02), DoubleDouble.valueOf(1.10320639714757395E+02), 
                DoubleDouble.valueOf(1.14034211781461703E+02), DoubleDouble.valueOf(1.17771881399745072E+02),
                DoubleDouble.valueOf(1.21533081515438634E+02), DoubleDouble.valueOf(1.25317271149356895E+02), 
                DoubleDouble.valueOf(1.29123933639127215E+02), DoubleDouble.valueOf(1.32952575035616310E+02),  
                DoubleDouble.valueOf(1.36802722637326368E+02), DoubleDouble.valueOf(1.40673923648234259E+02), 
                DoubleDouble.valueOf(1.44565743946344886E+02), DoubleDouble.valueOf(1.48477766951773032E+02),
                DoubleDouble.valueOf(1.52409592584497358E+02), DoubleDouble.valueOf(1.56360836303078785E+02),
                DoubleDouble.valueOf(1.60331128216630907E+02), DoubleDouble.valueOf(1.64320112263195181E+02),
                DoubleDouble.valueOf(1.68327445448427652E+02), DoubleDouble.valueOf(1.72352797139162802E+02),
                DoubleDouble.valueOf(1.76395848406997352E+02), DoubleDouble.valueOf(1.80456291417543771E+02),
                DoubleDouble.valueOf(1.84533828861449491E+02), DoubleDouble.valueOf(1.88628173423671591E+02),
                DoubleDouble.valueOf(1.92739047287844902E+02), DoubleDouble.valueOf(1.96866181672889994E+02),
                DoubleDouble.valueOf(2.01009316399281527E+02), DoubleDouble.valueOf(2.05168199482641199E+02), 
                DoubleDouble.valueOf(2.09342586752536836E+02), DoubleDouble.valueOf(2.13532241494563261E+02),
                DoubleDouble.valueOf(2.17736934113954227E+02), DoubleDouble.valueOf(2.21956441819130334E+02),
                DoubleDouble.valueOf(2.26190548323727593E+02), DoubleDouble.valueOf(2.30439043565776952E+02),
                DoubleDouble.valueOf(2.34701723442818268E+02), DoubleDouble.valueOf(2.38978389561834323E+02),
                DoubleDouble.valueOf(2.43268849002982714E+02), DoubleDouble.valueOf(2.47572914096186884E+02),
                DoubleDouble.valueOf(2.51890402209723194E+02), DoubleDouble.valueOf(2.56221135550009525E+02), 
                DoubleDouble.valueOf(2.60564940971863209E+02), DoubleDouble.valueOf(2.64921649798552801E+02),
                DoubleDouble.valueOf(2.69291097651019823E+02), DoubleDouble.valueOf(2.73673124285693704E+02), 
                DoubleDouble.valueOf(2.78067573440366143E+02), DoubleDouble.valueOf(2.82474292687630396E+02),
                DoubleDouble.valueOf(2.86893133295426994E+02), DoubleDouble.valueOf(2.91323950094270308E+02),
                DoubleDouble.valueOf(2.95766601350760624E+02), DoubleDouble.valueOf(3.00220948647014132E+02),
                DoubleDouble.valueOf(3.04686856765668715E+02), DoubleDouble.valueOf(3.09164193580146922E+02),
                DoubleDouble.valueOf(3.13652829949879062E+02), DoubleDouble.valueOf(3.18152639620209327E+02),
                DoubleDouble.valueOf(3.22663499126726177E+02), DoubleDouble.valueOf(3.27185287703775217E+02),
                DoubleDouble.valueOf(3.31717887196928473E+02), DoubleDouble.valueOf(3.36261181979198477E+02),
                DoubleDouble.valueOf(3.40815058870799018E+02), DoubleDouble.valueOf(3.45379407062266854E+02),
                DoubleDouble.valueOf(3.49954118040770237E+02), DoubleDouble.valueOf(3.54539085519440809E+02),
                DoubleDouble.valueOf(3.59134205369575399E+02)};
        final DoubleDouble[] cf = new DoubleDouble[] {DoubleDouble.valueOf(8.33333333333333333E-02),
        		DoubleDouble.valueOf(-2.77777777777777778E-03), DoubleDouble.valueOf(7.93650793650793651E-04),
                DoubleDouble.valueOf(-5.95238095238095238E-04), DoubleDouble.valueOf(8.41750841750841751E-04), 
                DoubleDouble.valueOf(-1.91752691752691753E-03), DoubleDouble.valueOf(6.41025641025641026E-03),
                DoubleDouble.valueOf(-2.95506535947712418E-02), DoubleDouble.valueOf(1.79644372368830573E-01), 
                DoubleDouble.valueOf(-1.39243221690590112E+00), DoubleDouble.valueOf(1.34028640441683920E+01),
                DoubleDouble.valueOf(-1.56848284626002017E+02), DoubleDouble.valueOf(2.19310333333333333E+03),
                DoubleDouble.valueOf(-3.61087712537249894E+04), DoubleDouble.valueOf(6.91472268851313067E+05),
                DoubleDouble.valueOf(-1.52382215394074162E+07), DoubleDouble.valueOf(3.82900751391414141E+08), 
                DoubleDouble.valueOf(-1.08822660357843911E+10), DoubleDouble.valueOf(3.47320283765002252E+11),
                DoubleDouble.valueOf(-1.23696021422692745E+13), DoubleDouble.valueOf(4.88788064793079335E+14), 
                DoubleDouble.valueOf(-2.13203339609193739E+16)};

        // ln(2*PI)
        final DoubleDouble con = DoubleDouble.TWO_PI.log();
        //final DoubleDouble con = 1.83787706640934548E+00;
        int nz;
        DoubleDouble fz;
        DoubleDouble wdtol;
        int i1m;
        DoubleDouble rln;
        DoubleDouble fln;
        DoubleDouble zm;
        int mz;
        DoubleDouble zmin;
        DoubleDouble zdmy;
        DoubleDouble zinc;
        DoubleDouble zp;
        DoubleDouble t1;
        DoubleDouble s;
        DoubleDouble zsq;
        DoubleDouble tst;
        DoubleDouble trm;
        DoubleDouble tlg;
        int k;
        int i;
        DoubleDouble gamln = DoubleDouble.valueOf(0.0);
        ierr[0] = 0;

        if (z.le(DoubleDouble.valueOf(0.0))) {
            ierr[0] = 1;

            return DoubleDouble.valueOf(0.0);
        }

        nz = (int) z.intValue();

        if (z.le(DoubleDouble.valueOf(101.0))) {
            fz = z.subtract(DoubleDouble.valueOf(nz));

            if ( (fz.le(DoubleDouble.valueOf(0.0))) && (nz <= 100)) {
                return gln[nz - 1];
            }
        } // if (z <= 101.0)

        wdtol = (DoubleDouble)epsilon.clone();
        wdtol = wdtol.max(DoubleDouble.valueOf(0.5e-18));
        i1m = DoubleDoubleDigits;
        rln = r1m5.multiply(DoubleDouble.valueOf(i1m));
        fln = rln.min(DoubleDouble.valueOf(20.0));
        fln = fln.max(DoubleDouble.valueOf(3.0));
        fln = fln.subtract(DoubleDouble.valueOf(3.0));
        zm = (DoubleDouble.valueOf(1.8)).add((DoubleDouble.valueOf(0.3875)).multiply(fln));
        mz = zm.intValue() + 1;
        zmin = DoubleDouble.valueOf(mz);
        zdmy = (DoubleDouble)z.clone();
        zinc = DoubleDouble.valueOf(0.0);

        if (z.lt(zmin)) {
            zinc = zmin.subtract(DoubleDouble.valueOf(nz));
            zdmy = z.add(zinc);
        } // if (z < zmin)

        zp = zdmy.reciprocal();
        t1 = cf[0].multiply(zp);
        s = (DoubleDouble)t1.clone();

        if (zp.ge(wdtol)) {
            zsq = zp.multiply(zp);
            tst = t1.multiply(wdtol);

            for (k = 1; k <= 21; k++) {
                zp = zp.multiply(zsq);
                trm = cf[k].multiply(zp);

                if ((trm.abs()).lt(tst)) {
                    break;
                }

                s = s.add(trm);
            } // for (k = 1; k <= 21; k++)
        } // if (zp >= wdtol)

        if (zinc.equals(DoubleDouble.valueOf(0.0))) {
            tlg = z.log();
            gamln = ((z.multiply(tlg.subtract(DoubleDouble.valueOf(1.0)))).
            		add((DoubleDouble.valueOf(0.5)).multiply(con.subtract(tlg)))).add(s);

            return gamln;
        } // if (zinc == 0.0)

        zp = DoubleDouble.valueOf(1.0);
        nz = zinc.intValue();

        for (i = 1; i <= nz; i++) {
            zp = zp.multiply((z.add(DoubleDouble.valueOf( i - 1.0))));
        }

        tlg = zdmy.log();
        gamln = (((zdmy.multiply(tlg.subtract(DoubleDouble.valueOf(1.0)))).subtract(zp.log()))
        		.add((DoubleDouble.valueOf(0.5)).multiply(con.subtract(tlg)))).add(s);

        return gamln;
    }

    /**
     * DOCUMENT ME!
     */
    private void runTest() {

        if (BesselType == BesselEP.BESSEL_H) {

            if (overflowTest) {
                zqcbh(2);
            } else {
                zqcbh(1);
            }
        } // if (BesselType == BESSEL_H)
        else if (BesselType == BesselEP.BESSEL_I) {

            if (overflowTest) {
                zqcbi(2);
            } else {
                zqcbi(1);
            }
        } // else if (BesselType == BESSEL_I);
        else if (BesselType == BesselEP.BESSEL_J) {

            if (overflowTest) {
                zqcbj(2);
            } else {
                zqcbj(1);
            }
        } // else if (BesselType == BESSEL_J);
        else if (BesselType == BesselEP.BESSEL_K) {

            if (overflowTest) {
                zqcbk(2);
            } else {
                zqcbk(1);
            }
        } // else if (BesselType == BESSEL_K)
        else if (BesselType == BesselEP.BESSEL_Y) {

            if (overflowTest) {
                zqcby(2);
            } else {
                zqcby(1);
            }
        } // else if (BesselType == BESSEL_Y)
        else if ( (BesselType == BesselEP.AIRY_AI) || (BesselType == BesselEP.AIRY_BI)) {

            if (overflowTest) {
                zqcai(2);
            } else {
                zqcai(1);
            }
        } // else if ((BesselType == AIRY_AI)|| (BesselType == AIRY_BI))

    }

    /**
     * zabs computes the absolute value or magnitude of a DoubleDouble precision complex variable zr + j*zi.
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * 
     * @return DoubleDouble
     */
    private DoubleDouble zabs(final DoubleDouble zr, final DoubleDouble zi) {
        DoubleDouble u, v, q, s;
        u = zr.abs();
        v = zi.abs();
        s = u.add(v);

        // s * 1.0 makes an unnormalized underflow on CDC machines into a true
        // floating zero
        //s = s * 1.0;

        if (s.equals(DoubleDouble.valueOf(0.0))) {
            return DoubleDouble.valueOf(0.0);
        } else if (u.gt(v)) {
        	q = v.divide(u);
            return (u.multiply(((DoubleDouble.valueOf(1.0)).add(q.multiply(q))).sqrt()));
        } else {
        	q = u.divide(v);
            return (v.multiply(((DoubleDouble.valueOf(1.0)).add(q.multiply(q))).sqrt()));
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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param mr int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     */
    private void zacai(DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int mr, final int n,
            final DoubleDouble[] yr, final DoubleDouble[] yi, final int[] nz) {
        final DoubleDouble[] cyr = new DoubleDouble[2];
        final DoubleDouble[] cyi = new DoubleDouble[2];
        DoubleDouble znr;
        DoubleDouble zni;
        DoubleDouble az;
        int nn;
        DoubleDouble dfnu;
        final int[] nw = new int[1];
        DoubleDouble fmr;
        DoubleDouble sgn;
        DoubleDouble csgnr;
        DoubleDouble csgni;
        DoubleDouble yy;
        int inu;
        DoubleDouble arg;
        DoubleDouble cspnr;
        DoubleDouble cspni;
        final DoubleDouble[] c1r = new DoubleDouble[1];
        final DoubleDouble[] c1i = new DoubleDouble[1];
        final DoubleDouble[] c2r = new DoubleDouble[1];
        final DoubleDouble[] c2i = new DoubleDouble[1];
        final int[] iuf = new int[1];
        DoubleDouble ascle;

        nz[0] = 0;
        znr = zr.negate();
        zni = zi.negate();
        az = zabs(zr, zi);
        nn = n;
        dfnu = fnu.add(DoubleDouble.valueOf(n - 1.0));

        if ( (az.le(DoubleDouble.valueOf(2.0))) || ( (az.gt(DoubleDouble.valueOf(2.0))) &&
        		( ((az.multiply(az)).multiply(DoubleDouble.valueOf(0.25)).le(dfnu.add(DoubleDouble.valueOf(1.0))))))) {

            // Power series for the I function
            zseri(znr, zni, fnu, kode, nn, yr, yi, nw);
        } // if (az <= 2.0)
        else {

            if (az.ge(rl)) {

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

        fmr = DoubleDouble.valueOf(mr);

        if (fmr.ge(DoubleDouble.valueOf(0.0))) {
            sgn = DoubleDouble.PI.negate();
        } else {
            sgn = DoubleDouble.PI;
        }

        csgnr = DoubleDouble.valueOf(0.0);
        csgni = (DoubleDouble)sgn.clone();

        if (kode != 1) {
            yy = zni.negate();
            csgnr = (csgni.negate()).multiply(yy.sin());
            csgni = csgni.multiply(yy.cos());
        } // if (kode != 1)

        // Calculate cspn = exp(fnu*PI*i) to minimize losses of significance
        // when fnu is large
        inu = fnu.intValue();
        arg = (fnu.subtract(DoubleDouble.valueOf(inu))).multiply(sgn);
        cspnr = arg.cos();
        cspni = arg.sin();

        if ( (inu % 2) != 0) {
            cspnr = cspnr.negate();
            cspni = cspni.negate();
        } // if ((inu%2) != 0)

        c1r[0] = (DoubleDouble)cyr[0].clone();
        c1i[0] = (DoubleDouble)cyi[0].clone();
        c2r[0] = (DoubleDouble) yr[0].clone();
        c2i[0] = (DoubleDouble)yi[0].clone();

        if (kode != 1) {
            iuf[0] = 0;
            ascle = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).divide(tol);
            zs1s2(znr, zni, c1r, c1i, c2r, c2i, nw, ascle, iuf);
            nz[0] = nz[0] + nw[0];
        } // if (kode != 1)

        yr[0] = (((cspnr.multiply(c1r[0])).subtract(cspni.multiply(c1i[0]))).add(csgnr.multiply(c2r[0])))
        		.subtract(csgni.multiply(c2i[0]));
        yi[0] = (((cspnr.multiply(c1i[0])).add(cspni.multiply(c1r[0]))).add(csgnr.multiply(c2i[0])))
        		.add(csgni.multiply(c2r[0]));

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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param mr int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     */
    private void zacon(DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int mr, final int n,
            final DoubleDouble[] yr, final DoubleDouble[] yi, final int[] nz) {
        final DoubleDouble[] cyr = new DoubleDouble[2];
        final DoubleDouble[] cyi = new DoubleDouble[2];
        final DoubleDouble[] cssr = new DoubleDouble[3];
        final DoubleDouble[] csrr = new DoubleDouble[3];
        final DoubleDouble[] bry = new DoubleDouble[3];
        DoubleDouble znr;
        DoubleDouble zni;
        int nn;
        final int[] nw = new int[1];
        DoubleDouble s1r;
        DoubleDouble s1i;
        DoubleDouble s2r;
        DoubleDouble s2i;
        DoubleDouble fmr;
        DoubleDouble sgn;
        final DoubleDouble[] csgnr = new DoubleDouble[1];
        final DoubleDouble[] csgni = new DoubleDouble[1];
        DoubleDouble yy;
        DoubleDouble cpn;
        DoubleDouble spn;
        int inu;
        DoubleDouble arg;
        DoubleDouble cspnr;
        DoubleDouble cspni;
        final int[] iuf = new int[1];
        final DoubleDouble[] c1r = new DoubleDouble[1];
        final DoubleDouble[] c1i = new DoubleDouble[1];
        final DoubleDouble[] c2r = new DoubleDouble[1];
        final DoubleDouble[] c2i = new DoubleDouble[1];
        DoubleDouble ascle;
        DoubleDouble sc1r;
        DoubleDouble sc1i;
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        final DoubleDouble[] ptr = new DoubleDouble[1];
        final DoubleDouble[] pti = new DoubleDouble[1];
        DoubleDouble sc2r = DoubleDouble.valueOf(0.0);
        DoubleDouble sc2i = DoubleDouble.valueOf(0.0);
        DoubleDouble azn;
        DoubleDouble razn;
        DoubleDouble rzr;
        DoubleDouble rzi;
        DoubleDouble fn;
        DoubleDouble ckr;
        DoubleDouble cki;
        DoubleDouble cscl;
        DoubleDouble cscr;
        DoubleDouble as2;
        int kflag;
        DoubleDouble bscle;
        DoubleDouble csr;
        int i;
        DoubleDouble c1m;

        nz[0] = 0;
        znr = zr.negate();
        zni = zi.negate();
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

        s1r = (DoubleDouble)cyr[0].clone();
        s1i = (DoubleDouble)cyi[0].clone();
        fmr = DoubleDouble.valueOf(mr);

        if (fmr.ge(DoubleDouble.valueOf(0.0))) {
            sgn = DoubleDouble.PI.negate();
        } else {
            sgn = DoubleDouble.PI;
        }

        csgnr[0] = DoubleDouble.valueOf(0.0);
        csgni[0] = (DoubleDouble)sgn.clone();

        if (kode != 1) {
            yy = zni.negate();
            cpn = yy.cos();
            spn = yy.sin();
            zmlt(csgnr[0], csgni[0], cpn, spn, csgnr, csgni);
        } // if (kode != 1)

        // Calculate cspn = exp(fnu*PI*i) to minimize losses of significance
        // when fnu is large
        inu = fnu.intValue();
        arg = (fnu.subtract(DoubleDouble.valueOf(inu))).multiply(sgn);
        cpn = arg.cos();
        spn = arg.sin();
        cspnr = (DoubleDouble)cpn.clone();
        cspni = (DoubleDouble)spn.clone();

        if ( (inu % 2) != 0) {
            cspnr = cspnr.negate();
            cspni = cspni.negate();
        } // if ((inu%2) != 0)

        iuf[0] = 0;
        c1r[0] = (DoubleDouble)s1r.clone();
        c1i[0] = (DoubleDouble)s1i.clone();
        c2r[0] = (DoubleDouble)yr[0].clone();
        c2i[0] = (DoubleDouble)yi[0].clone();
        ascle = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).divide(tol);

        if (kode != 1) {
            zs1s2(znr, zni, c1r, c1i, c2r, c2i, nw, ascle, iuf);
            nz[0] = nz[0] + nw[0];
            sc1r = (DoubleDouble)c1r[0].clone();
            sc1i = (DoubleDouble)c1i[0].clone();
        } // if (kode != 1)

        zmlt(cspnr, cspni, c1r[0], c1i[0], str, sti);
        zmlt(csgnr[0], csgni[0], c2r[0], c2i[0], ptr, pti);
        yr[0] = str[0].add(ptr[0]);
        yi[0] = sti[0].add(pti[0]);

        if (n == 1) {
            return;
        } // if (n == 1)

        cspnr = cspnr.negate();
        cspni = cspni.negate();
        s2r = (DoubleDouble)cyr[1].clone();
        s2i = (DoubleDouble)cyi[1].clone();
        c1r[0] = (DoubleDouble)s2r.clone();
        c1i[0] = (DoubleDouble)s2i.clone();
        c2r[0] = (DoubleDouble)yr[1].clone();
        c2i[0] = (DoubleDouble)yi[1].clone();

        if (kode != 1) {
            zs1s2(znr, zni, c1r, c1i, c2r, c2i, nw, ascle, iuf);
            nz[0] = nz[0] + nw[0];
            sc2r = (DoubleDouble)c1r[0].clone();
            sc2i = (DoubleDouble)c1i[0].clone();
        } // if (kode != 1)

        zmlt(cspnr, cspni, c1r[0], c1i[0], str, sti);
        zmlt(csgnr[0], csgni[0], c2r[0], c2i[0], ptr, pti);
        yr[1] = str[0].add(ptr[0]);
        yi[1] = sti[0].add(pti[0]);

        if (n == 2) {
            return;
        } // if (n == 2)

        cspnr = cspnr.negate();
        cspni = cspni.negate();
        azn = zabs(znr, zni);
        razn = azn.reciprocal();
        str[0] = znr.multiply(razn);
        sti[0] = (zni.negate()).multiply(razn);
        rzr = (str[0].add(str[0])).multiply(razn);
        rzi = (sti[0].add(sti[0])).multiply(razn);
        fn = fnu.add(DoubleDouble.valueOf(1.0));
        ckr = fn.multiply(rzr);
        cki = fn.multiply(rzi);

        // Scale near exponent extremes during recurrence on K functions
        cscl = tol.reciprocal();
        cscr = (DoubleDouble)tol.clone();
        cssr[0] = (DoubleDouble)cscl.clone();
        cssr[1] = DoubleDouble.valueOf(1.0);
        cssr[2] = (DoubleDouble)cscr.clone();
        csrr[0] = (DoubleDouble)cscr.clone();
        csrr[1] = DoubleDouble.valueOf(1.0);
        csrr[2] = (DoubleDouble)cscl.clone();
        bry[0] = (DoubleDouble)ascle.clone();
        bry[1] = ascle.reciprocal();
        bry[2] = DoubleDouble.valueOf(Double.MAX_VALUE);
        as2 = zabs(s2r, s2i);
        kflag = 2;

        if (as2.le(bry[0])) {
            kflag = 1;
        } // if (as2 <= bry[0])
        else if ( (as2.gt(bry[0])) && (as2.ge(bry[1]))) {
            kflag = 3;
        } // else if ((as2 > bry[0]) && (as2 >= bry[1]))

        bscle = (DoubleDouble)bry[kflag - 1].clone();
        s1r = s1r.multiply(cssr[kflag - 1]);
        s1i = s1i.multiply(cssr[kflag - 1]);
        s2r = s2r.multiply(cssr[kflag - 1]);
        s2i = s2i.multiply(cssr[kflag - 1]);
        csr = (DoubleDouble)csrr[kflag - 1].clone();

        for (i = 3; i <= n; i++) {
            str[0] = (DoubleDouble)s2r.clone();
            sti[0] = (DoubleDouble)s2i.clone();
            s2r = ((ckr.multiply(str[0])).subtract(cki.multiply(sti[0]))).add(s1r);
            s2i = ((ckr.multiply(sti[0])).add(cki.multiply(str[0]))).add(s1i);
            s1r = (DoubleDouble)str[0].clone();
            s1i = (DoubleDouble)sti[0].clone();
            c1r[0] = s2r.multiply(csr);
            c1i[0] = s2i.multiply(csr);
            str[0] = (DoubleDouble)c1r[0].clone();
            sti[0] = (DoubleDouble)c1i[0].clone();
            c2r[0] = (DoubleDouble)yr[i - 1].clone();
            c2i[0] = (DoubleDouble)yi[i - 1].clone();

            if ( (kode != 1) && (iuf[0] >= 0)) {
                zs1s2(znr, zni, c1r, c1i, c2r, c2i, nw, ascle, iuf);
                nz[0] = nz[0] + nw[0];
                sc1r = (DoubleDouble)sc2r.clone();
                sc1i = (DoubleDouble)sc2i.clone();
                sc2r = (DoubleDouble)c1r[0].clone();
                sc2i = (DoubleDouble)c1i[0].clone();

                if (iuf[0] == 3) {
                    iuf[0] = -4;
                    s1r = sc1r.multiply(cssr[kflag - 1]);
                    s1i = sc1i.multiply(cssr[kflag - 1]);
                    s2r = sc2r.multiply(cssr[kflag - 1]);
                    s2i = sc2i.multiply(cssr[kflag - 1]);
                    str[0] = (DoubleDouble)sc2r.clone();
                    sti[0] = (DoubleDouble)sc2i.clone();
                } // if (iuf[0] == 3)
            } // if ((kode != 1) && (iuf[0] >= 0))

            ptr[0] = (cspnr.multiply(c1r[0])).subtract(cspni.multiply(c1i[0]));
            pti[0] = (cspnr.multiply(c1i[0])).add(cspni.multiply(c1r[0]));
            yr[i - 1] = (ptr[0].add(csgnr[0].multiply(c2r[0]))).subtract(csgni[0].multiply(c2i[0]));
            yi[i - 1] = (pti[0].add(csgnr[0].multiply(c2i[0]))).add(csgni[0].multiply(c2r[0]));
            ckr = ckr.add(rzr);
            cki = cki.add(rzi);
            cspnr = cspnr.negate();
            cspni = cspni.negate();

            if (kflag >= 3) {
                continue;
            } // if (kflag >= 3)

            ptr[0] = c1r[0].abs();
            pti[0] = c1i[0].abs();
            c1m = ptr[0].max(pti[0]);

            if (c1m.le(bscle)) {
                continue;
            } // if (c1m.le(bscle))

            kflag = kflag + 1;
            bscle = (DoubleDouble)bry[kflag - 1].clone();
            s1r = s1r.multiply(csr);
            s1i = s1i.multiply(csr);
            s2r = (DoubleDouble)str[0].clone();
            s2i = (DoubleDouble)sti[0].clone();
            s1r = s1r.multiply(cssr[kflag - 1]);
            s1i = s1i.multiply(cssr[kflag - 1]);
            s2r = s2r.multiply(cssr[kflag - 1]);
            s2i = s2i.multiply(cssr[kflag - 1]);
            csr = (DoubleDouble)csrr[kflag - 1].clone();
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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param id int
     * @param kode int
     * @param air DoubleDouble[]
     * @param aii DoubleDouble[]
     * @param nz int[]
     * @param ierr int[]
     */
    private void zairy(final DoubleDouble zr, final DoubleDouble zi, final int id, final int kode, final DoubleDouble[] air,
            final DoubleDouble[] aii, final int[] nz, final int[] ierr) {
        final DoubleDouble[] cyr = new DoubleDouble[1];
        final DoubleDouble[] cyi = new DoubleDouble[1];
        final DoubleDouble tth = (DoubleDouble.valueOf(2.0)).divide(DoubleDouble.valueOf(3.0));
        final DoubleDouble c1 = DoubleDouble.valueOf(3.55028053887817240E-01);
        DoubleDouble c2 = DoubleDouble.valueOf(2.58819403792806799E-01);
        final DoubleDouble coef = DoubleDouble.valueOf(1.83776298473930683E-01);
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        final DoubleDouble[] csqr = new DoubleDouble[1];
        final DoubleDouble[] csqi = new DoubleDouble[1];
        final int[] nn = new int[1];
        DoubleDouble az;
        DoubleDouble fid;
        DoubleDouble s1r;
        DoubleDouble s1i;
        DoubleDouble s2r;
        DoubleDouble s2i;
        DoubleDouble aa;
        DoubleDouble trm1r;
        DoubleDouble trm1i;
        DoubleDouble trm2r;
        DoubleDouble trm2i;
        DoubleDouble atrm;
        DoubleDouble z3r;
        DoubleDouble z3i;
        DoubleDouble az3;
        DoubleDouble ak;
        DoubleDouble bk;
        DoubleDouble ck;
        DoubleDouble dk;
        DoubleDouble d1;
        DoubleDouble d2;
        DoubleDouble ad;
        int k;
        DoubleDouble ztar;
        DoubleDouble ztai;
        DoubleDouble ptr;
        DoubleDouble cc;
        DoubleDouble fnu;
        int k1;
        DoubleDouble dig;
        DoubleDouble alaz;
        DoubleDouble bb;
        int iflag;
        DoubleDouble sfac;
        int mr;

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
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */

        tol = epsilon.max(DoubleDouble.valueOf(1.0E-18));
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);
        fid = DoubleDouble.valueOf(id);

        if (az.le(DoubleDouble.valueOf(1.0))) {

            // Power series for cabs(z) <= 1
            s1r = DoubleDouble.valueOf(1.0);
            s1i = DoubleDouble.valueOf(0.0);
            s2r = DoubleDouble.valueOf(1.0);
            s2i = DoubleDouble.valueOf(0.0);

            if (az.lt(tol)) {
                aa = (DoubleDouble.valueOf(1.0E3)).multiply(tiny);
                s1r = DoubleDouble.valueOf(0.0);
                s1i = DoubleDouble.valueOf(0.0);

                if (id == 1) {
                    air[0] = c2.negate();
                    aii[0] = DoubleDouble.valueOf(0.0);
                    aa = aa.sqrt();

                    if (az.gt(aa)) {
                        s1r = (DoubleDouble.valueOf(0.5)).multiply( (zr.multiply(zr)).subtract(zi.multiply(zi)));
                        s1i = zr.multiply(zi);
                    } // if (az > aa)

                    air[0] = air[0].add(c1.multiply(s1r));
                    aii[0] = aii[0].add(c1.multiply(s1i));

                    return;
                } // if (id == 1)
                else { // id != 1

                    if (az.gt(aa)) {
                        s1r = c2.multiply(zr);
                        s1i = c2.multiply(zi);
                    } // if (az > aa)

                    air[0] = c1.subtract(s1r);
                    aii[0] = s1i.negate();

                    return;
                } // else id != 1
            } // if (az < tol)

            aa = az.multiply(az);

            if (aa.ge(tol.divide(az))) {
                trm1r = DoubleDouble.valueOf(1.0);
                trm1i = DoubleDouble.valueOf(0.0);
                trm2r = DoubleDouble.valueOf(1.0);
                trm2i = DoubleDouble.valueOf(0.0);
                atrm = DoubleDouble.valueOf(1.0);
                str[0] = (zr.multiply(zr)).subtract(zi.multiply(zi));
                sti[0] = (DoubleDouble.valueOf(2.0)).multiply(zr).multiply(zi);
                z3r = (str[0].multiply(zr)).subtract(sti[0].multiply(zi));
                z3i = (str[0].multiply(zi)).add(sti[0].multiply(zr));
                az3 = az.multiply(aa);
                ak = (DoubleDouble.valueOf(2.0)).add(fid);
                bk = ((DoubleDouble.valueOf(3.0)).subtract(fid)).subtract(fid);
                ck = (DoubleDouble.valueOf(4.0)).subtract(fid);
                dk = ((DoubleDouble.valueOf(3.0)).add(fid)).add(fid);
                d1 = ak.multiply(dk);
                d2 = bk.multiply(ck);
                ad = d1.min(d2);
                ak = (DoubleDouble.valueOf(24.0)).add((DoubleDouble.valueOf(9.0)).multiply(fid));
                bk = (DoubleDouble.valueOf(30.0)).subtract((DoubleDouble.valueOf(9.0)).multiply(fid));

                for (k = 1; k <= 25; k++) {
                    str[0] = ( (trm1r.multiply(z3r)).subtract(trm1i.multiply(z3i))).divide(d1);
                    trm1i = ( (trm1r.multiply(z3i)).add(trm1i.multiply(z3r))).divide(d1);
                    trm1r = (DoubleDouble)str[0].clone();
                    s1r = s1r.add(trm1r);
                    s1i = s1i.add(trm1i);
                    str[0] = ( (trm2r.multiply(z3r)).subtract(trm2i.multiply(z3i))).divide(d2);
                    trm2i = ( (trm2r.multiply(z3i)).add(trm2i.multiply(z3r))).divide(d2);
                    trm2r = (DoubleDouble)str[0].clone();
                    s2r = s2r.add(trm2r);
                    s2i = s2i.add(trm2i);
                    atrm = (atrm.multiply(az3)).divide(ad);
                    d1 = d1.add(ak);
                    d2 = d2.add(bk);

                    if (atrm.lt(tol.multiply(ad))) {
                        break;
                    } // if (atrm < (tol*ad))

                    ak = ak.add(DoubleDouble.valueOf(18.0));
                    bk = bk.add(DoubleDouble.valueOf(18.0));
                } // for (k = 1; k <= 25; k++)
            } // if (aa >= (tol/az))

            if (id != 1) {
                air[0] = (s1r.multiply(c1)).subtract(c2.multiply( (zr.multiply(s2r)).subtract(zi.multiply(s2i))));
                aii[0] = (s1i.multiply(c1)).subtract(c2.multiply( (zr.multiply(s2i)).add(zi.multiply(s2r))));

                if (kode == 1) {
                    return;
                } // if (kode == 1)

                zsqrt(zr, zi, str, sti);
                ztar = tth.multiply( (zr.multiply(str[0])).subtract(zi.multiply(sti[0])));
                ztai = tth.multiply( (zr.multiply(sti[0])).add(zi.multiply(str[0])));
                zexp(ztar, ztai, str, sti);
                ptr = (air[0].multiply(str[0])).subtract(aii[0].multiply(sti[0]));
                aii[0] = (air[0].multiply(sti[0])).add(aii[0].multiply(str[0]));
                air[0] = (DoubleDouble)ptr.clone();

                return;
            } // if (id != 1)

            air[0] = (s2r.negate()).multiply(c2);
            aii[0] = (s2i.negate()).multiply(c2);

            if (az.gt(tol)) {
                str[0] = (zr.multiply(s1r)).subtract(zi.multiply(s1i));
                sti[0] = (zr.multiply(s1i)).add(zi.multiply(s1r));
                cc = c1.divide((DoubleDouble.valueOf(1.0)).add(fid));
                air[0] = air[0].add(cc.multiply( (str[0].multiply( zr)).subtract(sti[0].multiply(zi))));
                aii[0] = aii[0].add(cc.multiply( (str[0].multiply(zi)).add(sti[0].multiply(zr))));
            } // if (az > tol)

            if (kode == 1) {
                return;
            } // if (kode == 1)

            zsqrt(zr, zi, str, sti);
            ztar = tth.multiply( (zr.multiply(str[0])).subtract(zi.multiply(sti[0])));
            ztai = tth.multiply( (zr.multiply(sti[0])).add(zi.multiply(str[0])));
            zexp(ztar, ztai, str, sti);
            ptr = (str[0].multiply(air[0])).subtract(sti[0].multiply(aii[0]));
            aii[0] = (str[0].multiply(aii[0])).add(sti[0].multiply(air[0]));
            air[0] = (DoubleDouble)ptr.clone();

            return;
        } // if (az <= 1.0)

        // Case for CABS(z) > 1.0
        fnu = ((DoubleDouble.valueOf(1.0)).add(fid)).divide(DoubleDouble.valueOf(3.0));

        // SET PARAMETERS RELATED TO MACHINE CONSTANTS.
        // TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18.
        // ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT.
        // EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
        // EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR
        // UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC IS DONE.
        // RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z.
        // DIG = NUMBER OF BASE 10 DIGITS IN TOL = 10**(-DIG).
        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));

        // DoubleDoubleDigits is I1MACH(14), the number of base-B digits for DoubleDouble
        // precision. IEEE 754 DoubleDouble precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        DoubleDoubleDigits = 106;
        k1 = DoubleDoubleDigits - 1;
        aa = r1m5.multiply(DoubleDouble.valueOf(k1));
        dig = aa.min(DoubleDouble.valueOf( 18.0));
        aa = (DoubleDouble.valueOf(2.303)).multiply(aa);
        alim = elim.add( (aa.negate()).max(DoubleDouble.valueOf(-41.45)));
        rl = ((DoubleDouble.valueOf(1.2)).multiply(dig)).add(DoubleDouble.valueOf(3.0));
        alaz = az.log();

        // Test for proper range
        aa = (DoubleDouble.valueOf(0.5)).divide(tol);
        bb = (DoubleDouble.valueOf(0.5)).multiply(DoubleDouble.valueOf(Integer.MAX_VALUE));
        aa = aa.min(bb);
        aa = aa.pow(tth);

        if (az.gt(aa)) {
            ierr[0] = 4;
            nz[0] = 0;

            return;
        } // if (az > aa)

        aa = aa.sqrt();

        if (az.gt(aa)) {
            ierr[0] = 3;
        } // if (az > aa)

        zsqrt(zr, zi, csqr, csqi);
        ztar = tth.multiply( (zr.multiply(csqr[0])).subtract(zi.multiply(csqi[0])));
        ztai = tth.multiply( (zr.multiply(csqi[0])).add(zi.multiply(csqr[0])));

        // Re(zta) <= 0 when Re(z) < 0, especially when Im(z) is small
        iflag = 0;
        sfac = DoubleDouble.valueOf(1.0);
        ak = (DoubleDouble)ztai.clone();

        if (zr.lt(DoubleDouble.valueOf(0.0))) {
            bk = (DoubleDouble)ztar.clone();
            ck = (bk.abs()).negate();
            ztar = (DoubleDouble)ck.clone();
            ztai = (DoubleDouble)ak.clone();
        } // if (zr < 0.0)

        if ( (zi.equals(DoubleDouble.valueOf(0.0))) && (zr.le(DoubleDouble.valueOf( 0.0)))) {
            ztar = DoubleDouble.valueOf(0.0);
            ztai = (DoubleDouble)ak.clone();
        } // if ((zi == 0.0) && (zr <= 0.0))

        aa = (DoubleDouble)ztar.clone();

        if ( (aa.ge(DoubleDouble.valueOf(0.0))) && (zr.gt(DoubleDouble.valueOf(0.0)))) {

            if ( (kode != 2) && (aa.ge(alim))) {
                aa = (aa.negate()).subtract((DoubleDouble.valueOf(0.25)).multiply(alaz));
                iflag = 2;
                sfac = tol.reciprocal();

                if (aa.lt(elim.negate())) {
                    nz[0] = 1;
                    air[0] = DoubleDouble.valueOf(0.0);
                    aii[0] = DoubleDouble.valueOf(0.0);

                    return;
                } // if (aa < (-elim))
            } // if ((kode != 2) && (aa >= alim))

            zbknu(ztar, ztai, fnu, kode, 1, cyr, cyi, nz);
            s1r = cyr[0].multiply(coef);
            s1i = cyi[0].multiply(coef);

            if (iflag != 0) {
                s1r = s1r.multiply(sfac);
                s1i = s1i.multiply(sfac);

                if (id == 1) {
                    str[0] = ( (s1r.multiply(zr)).subtract(s1i.multiply(zi))).negate();
                    s1i = ( (s1r.multiply(zi)).add(s1i.multiply(zr))).negate();
                    s1r = (DoubleDouble)str[0].clone();
                    air[0] = s1r.divide(sfac);
                    aii[0] = s1i.divide(sfac);

                    return;
                } // if (id == 1)
                else { // id != 1
                    str[0] = (s1r.multiply(csqr[0])).subtract(s1i.multiply(csqi[0]));
                    s1i = (s1r.multiply(csqi[0])).add(s1i.multiply(csqr[0]));
                    s1r = (DoubleDouble)str[0].clone();
                    air[0] = s1r.divide(sfac);
                    aii[0] = s1i.divide(sfac);

                    return;
                } // else id != 1
            } // if (iflag != 0)
            else if (id == 1) {
                air[0] = ( (zr.multiply(s1r)).subtract(zi.multiply(s1i))).negate();
                aii[0] = ( (zr.multiply(s1i)).add(zi.multiply(s1r))).negate();

                return;
            } // else if (id == 1)
            else {
                air[0] = (csqr[0].multiply(s1r)).subtract(csqi[0].multiply(s1i));
                aii[0] = (csqr[0].multiply(s1i)).add(csqi[0].multiply(s1r));

                return;
            } // else
        } // if ((aa >= 0.0) && (zr > 0.0))

        if ( (kode != 2) && (aa.le(alim.negate()))) {
            aa = (aa.negate()).add((DoubleDouble.valueOf(0.25)).multiply(alaz));
            iflag = 1;
            sfac = (DoubleDouble)tol.clone();

            if (aa.gt(elim)) {
                nz[0] = 0;
                ierr[0] = 2;

                return;
            } // if (aa > elim)
        } // if ((kode != 2) && (aa <= (-alim)))

        // CBKNU and CACON return exp(zta)*K(fnu,zta) on kode = 2
        mr = 1;

        if (zi.lt(DoubleDouble.valueOf(0.0))) {
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
        s1r = cyr[0].multiply(coef);
        s1i = cyi[0].multiply(coef);

        if (iflag != 0) {
            s1r = s1r.multiply(sfac);
            s1i = s1i.multiply(sfac);

            if (id == 1) {
                str[0] = ( (s1r.multiply(zr)).subtract(s1i.multiply(zi))).negate();
                s1i = ( (s1r.multiply(zi)).add(s1i.multiply(zr))).negate();
                s1r = (DoubleDouble)str[0].clone();
                air[0] = s1r.divide(sfac);
                aii[0] = s1i.divide(sfac);

                return;
            } // if (id == 1)
            else { // id != 1
                str[0] = (s1r.multiply(csqr[0])).subtract(s1i.multiply(csqi[0]));
                s1i = (s1r.multiply(csqi[0])).add(s1i.multiply(csqr[0]));
                s1r = (DoubleDouble)str[0].clone();
                air[0] = s1r.divide(sfac);
                aii[0] = s1i.divide(sfac);

                return;
            } // else id != 1
        } // if (iflag != 0)
        else if (id == 1) {
            air[0] = ( (zr.multiply(s1r)).subtract(zi.multiply(s1i))).negate();
            aii[0] = ( (zr.multiply(s1i)).add(zi.multiply(s1r))).negate();

            return;
        } // else if (id == 1)
        else {
            air[0] = (csqr[0].multiply(s1r)).subtract(csqi[0].multiply(s1i));
            aii[0] = (csqr[0].multiply(s1i)).add(csqi[0].multiply(s1r));

            return;
        } // else
    }

    /**
     * ZASYI COMPUTES THE I BESSEL FUNCTION FOR REAL(Z).GE.0.0 BY MEANS OF THE ASYMPTOTIC EXPANSION FOR LARGE CABS(Z) IN
     * THE REGION CABS(Z).GT.MAX(RL,fnu*fnu/2). NZ=0 IS A NORMAL RETURN. NZ.LT.0 INDICATES AN OVERFLOW ON
     * kode=UNSCALED_FUNCTION.
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     */
    private void zasyi(final DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n, final DoubleDouble[] yr,
            final DoubleDouble[] yi, final int[] nz) {
        final DoubleDouble rtpi = DoubleDouble.valueOf(0.159154943091895336);
        DoubleDouble az;
        DoubleDouble arm;
        DoubleDouble rtr1;
        int il;
        DoubleDouble dfnu;
        DoubleDouble raz;
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        final DoubleDouble[] ak1r = new DoubleDouble[1];
        final DoubleDouble[] ak1i = new DoubleDouble[1];
        DoubleDouble czr;
        DoubleDouble czi;
        DoubleDouble dnu2;
        int koded;
        DoubleDouble fdn;
        DoubleDouble ezr;
        DoubleDouble ezi;
        DoubleDouble aez;
        DoubleDouble s;
        int jl;
        DoubleDouble p1r;
        DoubleDouble p1i;
        int inu;
        DoubleDouble arg;
        DoubleDouble ak;
        DoubleDouble bk;
        int k;
        DoubleDouble sqk;
        DoubleDouble atol;
        DoubleDouble sgn;
        DoubleDouble cs1r;
        DoubleDouble cs1i;
        DoubleDouble cs2r;
        DoubleDouble cs2i;
        final DoubleDouble[] ckr = new DoubleDouble[1];
        final DoubleDouble[] cki = new DoubleDouble[1];
        DoubleDouble aa;
        DoubleDouble bb;
        DoubleDouble dkr;
        DoubleDouble dki;
        int j;
        DoubleDouble s2r;
        DoubleDouble s2i;
        DoubleDouble tzr;
        DoubleDouble tzi;
        int m;
        int nn;
        DoubleDouble rzr;
        DoubleDouble rzi;
        int ib;
        int i;

        nz[0] = 0;
        az = zabs(zr, zi);
        arm = (DoubleDouble.valueOf(1.0E3)).multiply(tiny);
        rtr1 = arm.sqrt();
        il = Math.min(2, n);
        dfnu = fnu.add(DoubleDouble.valueOf(n - il));

        // Overflow test
        raz = az.reciprocal();
        str[0] = zr.multiply(raz);
        sti[0] = (zi.negate()).multiply(raz);
        ak1r[0] = (rtpi.multiply(str[0])).multiply(raz);
        ak1i[0] = (rtpi.multiply(sti[0])).multiply(raz);
        zsqrt(ak1r[0], ak1i[0], ak1r, ak1i);
        czr = (DoubleDouble)zr.clone();
        czi = (DoubleDouble)zi.clone();

        if (kode == 2) {
            czr = DoubleDouble.valueOf(0.0);
        }

        if ((czr.abs()).gt(elim)) {
            nz[0] = -1;

            return;
        }

        dnu2 = dfnu.add(dfnu);
        koded = 1;

        if ( ((czr.abs()).le(alim)) || (n <= 2)) {
            koded = 0;
            zexp(czr, czi, str, sti);
            zmlt(ak1r[0], ak1i[0], str[0], sti[0], ak1r, ak1i);
        }

        fdn = DoubleDouble.valueOf(0.0);

        if (dnu2.gt(rtr1)) {
            fdn = dnu2.multiply(dnu2);
        }

        ezr = (DoubleDouble.valueOf(8.0)).multiply(zr);
        ezi = (DoubleDouble.valueOf(8.0)).multiply(zi);

        // When z is imaginary, the error test must be made relative to the
        // first reciprocal power since this is the leading term of the
        // expansion for the imaginary part
        aez = (DoubleDouble.valueOf(8.0)).multiply(az);
        s = tol.divide(aez);
        jl = (rl.add(rl)).intValue() + 2;
        p1r = DoubleDouble.valueOf(0.0);
        p1i = DoubleDouble.valueOf(0.0);

        if (zi.ne(DoubleDouble.valueOf(0.0))) {

            // Calculate exp(PI*(0.5+fnu+n-il)*i) to minimize losses of
            // significance when fnu of n is large
            inu = fnu.intValue();
            arg = (fnu.subtract(DoubleDouble.valueOf(inu))).multiply(DoubleDouble.PI);
            inu = inu + n - il;
            ak = (arg.sin()).negate();
            bk = arg.cos();

            if (zi.lt(DoubleDouble.valueOf(0.0))) {
                bk = bk.negate();
            }

            p1r = (DoubleDouble)ak.clone();
            p1i = (DoubleDouble)bk.clone();

            if ( (inu % 2) == 1) {
                p1r = p1r.negate();
                p1i = p1i.negate();
            }
        } // if (zi != 0.0)

        for (k = 1; k <= il; k++) {
            sqk = fdn.subtract(DoubleDouble.valueOf(1.0));
            atol = s.multiply(sqk.abs());
            sgn = DoubleDouble.valueOf(1.0);
            cs1r = DoubleDouble.valueOf(1.0);
            cs1i = DoubleDouble.valueOf(0.0);
            cs2r = DoubleDouble.valueOf(1.0);
            cs2i = DoubleDouble.valueOf(0.0);
            ckr[0] = DoubleDouble.valueOf(1.0);
            cki[0] = DoubleDouble.valueOf(0.0);
            ak = DoubleDouble.valueOf(0.0);
            aa = DoubleDouble.valueOf(1.0);
            bb = (DoubleDouble)aez.clone();
            dkr = (DoubleDouble)ezr.clone();
            dki = (DoubleDouble)ezi.clone();

            group: {

                for (j = 1; j <= jl; j++) {
                    zdiv(ckr[0], cki[0], dkr, dki, str, sti);
                    ckr[0] = str[0].multiply(sqk);
                    cki[0] = sti[0].multiply(sqk);
                    cs2r = cs2r.add(ckr[0]);
                    cs2i = cs2i.add(cki[0]);
                    sgn = sgn.negate();
                    cs1r = cs1r.add(ckr[0].multiply(sgn));
                    cs1i = cs1i.add(cki[0].multiply(sgn));
                    dkr = dkr.add(ezr);
                    dki = dki.add(ezi);
                    aa = (aa.multiply(sqk.abs())).divide(bb);
                    bb = bb.add(aez);
                    ak = ak.add(DoubleDouble.valueOf(8.0));
                    sqk = sqk.subtract(ak);

                    if (aa.le(atol)) {
                        break group;
                    }
                } // for (j = 1; j <= jl; j++)

                nz[0] = -2;

                return;
            } // group

            s2r = (DoubleDouble)cs1r.clone();
            s2i = (DoubleDouble)cs1i.clone();

            if ( (zr.add(zr)).lt(elim)) {
                tzr = zr.add(zr);
                tzi = zi.add(zi);
                zexp( tzr.negate(), tzi.negate(), str, sti);
                zmlt(str[0], sti[0], p1r, p1i, str, sti);
                zmlt(str[0], sti[0], cs2r, cs2i, str, sti);
                s2r = s2r.add(str[0]);
                s2i = s2i.add(sti[0]);
            } // if ((zr+zr) < elim)

            fdn = (fdn.add((DoubleDouble.valueOf(8.0)).multiply(dfnu))).add(DoubleDouble.valueOf(4.0));
            p1r = p1r.negate();
            p1i = p1i.negate();
            m = n - il + k;
            yr[m - 1] = (s2r.multiply(ak1r[0])).subtract(s2i.multiply(ak1i[0]));
            yi[m - 1] = (s2r.multiply(ak1i[0])).add(s2i.multiply(ak1r[0]));
        } // for (k = 1; k <= il; k++)

        if (n <= 2) {
            return;
        }

        nn = n;
        k = nn - 2;
        ak = DoubleDouble.valueOf(k);
        str[0] = zr.multiply(raz);
        sti[0] = (zi.negate()).multiply(raz);
        rzr = (str[0].add(str[0])).multiply(raz);
        rzi = (sti[0].add(sti[0])).multiply(raz);
        ib = 3;

        for (i = ib; i <= nn; i++) {
            yr[k - 1] = ( (ak.add(fnu)).multiply( (rzr.multiply(yr[k])).subtract(rzi.multiply(yi[k])))).add(yr[k + 1]);
            yi[k - 1] = ( (ak.add(fnu)).multiply( (rzr.multiply(yi[k])).add(rzi.multiply(yr[k])))).add(yi[k + 1]);
            ak = ak.subtract(DoubleDouble.valueOf(1.0));
            k = k - 1;
        } // for (i = ib; i <= nn; i++)

        if (koded == 0) {
            return;
        }

        zexp(czr, czi, ckr, cki);

        for (i = 0; i < nn; i++) {
            str[0] = (yr[i].multiply(ckr[0])).subtract(yi[i].multiply(cki[0]));
            yi[i] = (yr[i].multiply(cki[0])).add(yi[i].multiply(ckr[0]));
            yr[i] = (DoubleDouble)str[0].clone();
        }

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param m int
     * @param n int
     * @param cyr DoubleDouble[]
     * @param cyi DoubleDouble[]
     * @param nz int[]
     * @param ierr int[]
     */
    private void zbesh(final DoubleDouble zr, final DoubleDouble zi, final DoubleDouble fnu, final int kode, final int m, final int n,
            final DoubleDouble[] cyr, final DoubleDouble[] cyi, final int[] nz, final int[] ierr) {

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
        int k;
        int k1;
        DoubleDouble aa;
        DoubleDouble dig;
        DoubleDouble hpi = DoubleDouble.PI_2;
        DoubleDouble fn;
        int mm;
        DoubleDouble znr;
        DoubleDouble zni;
        DoubleDouble az;
        DoubleDouble bb;
        DoubleDouble ufl;
        DoubleDouble arg;
        DoubleDouble aln;
        final int[] nuf = new int[1];
        int mr;
        final int[] nw = new int[1];
        DoubleDouble sgn;
        int inu;
        int inuh;
        int ir;
        DoubleDouble rhpi;
        DoubleDouble csgnr;
        DoubleDouble csgni;
        DoubleDouble zti;
        DoubleDouble rtol;
        DoubleDouble ascle;
        DoubleDouble atol;
        DoubleDouble str;
        DoubleDouble sti;
        DoubleDouble fmm;
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
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */
        
        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));

        // DoubleDoubleDigits is I1MACH(14), the number of base-B digits for DoubleDouble
        // precision. IEEE 754 DoubleDouble precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // In DoubleDouble, the mantissa effectively has 106 bits of resolution.
        DoubleDoubleDigits = 106;
        k1 = DoubleDoubleDigits - 1;
        aa = r1m5.multiply(DoubleDouble.valueOf(k1));
        dig = aa.min(DoubleDouble.valueOf(18.0));
        aa = (DoubleDouble.valueOf(2.303)).multiply(aa);
        alim = elim.add( (aa.negate()).max(DoubleDouble.valueOf(-41.45)));
        fnul = (DoubleDouble.valueOf(10.0)).add((DoubleDouble.valueOf(6.0)).multiply(dig.subtract(DoubleDouble.valueOf(3.0))));
        rl = ((DoubleDouble.valueOf(1.2)).multiply(dig)).add(DoubleDouble.valueOf(3.0));
        fn = fnu.add(DoubleDouble.valueOf(nn - 1.0));
        mm = 3 - m - m;
        fmm = DoubleDouble.valueOf(mm);
        znr = fmm.multiply(zi);
        zni = (fmm.negate()).multiply(zr);

        // Test for proper range
        az = zabs(zr, zi);
        aa = (DoubleDouble.valueOf(0.5)).divide(tol);
        bb = (DoubleDouble.valueOf(0.5)).multiply(DoubleDouble.valueOf(Integer.MAX_VALUE));
        aa = aa.min(bb);

        if ( (az.gt(aa)) || (fn.gt(aa))) {
            nz[0] = 0;
            ierr[0] = 4;

            return;
        } // if ((az > aa) || (fn > aa))

        aa = aa.sqrt();

        if ( (az.gt(aa)) || (fn.gt(aa))) {
            ierr[0] = 3;
        } // if ((az > aa) || (fn > aa))

        // Overflow test on the last member of the sequence
        ufl = (DoubleDouble.valueOf(1.0E3)).multiply(tiny);

        if (az.lt(ufl)) {
            nz[0] = 0;
            ierr[0] = 2;

            return;
        } // if (az < ufl)

        if (fnu.le(fnul)) {

            if ( (fn.gt(DoubleDouble.valueOf(1.0))) && (fn.le(DoubleDouble.valueOf(2.0))) && (az.le(tol))) {
                arg = (DoubleDouble.valueOf(0.5)).multiply(az);
                aln = (fn.negate()).multiply(arg.log());

                if (aln.gt(elim)) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (aln > elim)
            } // if ((fn > 1.0) && (fn <= 2.0) && (az <= tol))
            else if (fn.gt(DoubleDouble.valueOf(2.0))) {
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

                    if (znr.lt(DoubleDouble.valueOf(0.0))) {
                        nz[0] = 0;
                        ierr[0] = 2;
                    } // if (znr < 0.0)

                    return;
                } // if (nn == 0)
            } // else if (fn > 2.0)

            if ( (znr.ge(DoubleDouble.valueOf(0.0))) && ( (znr.ne(DoubleDouble.valueOf(0.0))) || (zni.ge(DoubleDouble.valueOf(0.0)))
            		|| (m != 2))) {

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

            if ( (znr.lt(DoubleDouble.valueOf(0.0))) || ( (znr.equals(DoubleDouble.valueOf(0.0))) &&
            		(zni.lt(DoubleDouble.valueOf(0.0))) && (m == 2))) {
                mr = -mm;

                if ( (znr.equals(DoubleDouble.valueOf(0.0))) && (zni.lt(DoubleDouble.valueOf(0.0)))) {
                    znr = znr.negate();
                    zni = zni.negate();
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
        if ( (fmm.negate()).ge(DoubleDouble.valueOf(0.0))) {
            sgn = (DoubleDouble)hpi.clone();
        } else {
            sgn = hpi.negate();
        }

        // Caclulate exp(fnu*hpi*i) to minimize losses of significance when
        // fnu is large
        inu = fnu.intValue();
        inuh = inu / 2;
        ir = inu - (2 * inuh);
        arg = (fnu.subtract(DoubleDouble.valueOf(inu - ir))).multiply(sgn);
        rhpi = sgn.reciprocal();
        csgni = rhpi.multiply(arg.cos());
        csgnr = (rhpi.negate()).multiply(arg.sin());

        if ( (inuh % 2) != 0) {
            csgnr = csgnr.negate();
            csgni = csgni.negate();
        } // if ((inuh%2) != 0)

        zti = fmm.negate();
        rtol = tol.reciprocal();
        ascle = ufl.multiply(rtol);

        for (i = 1; i <= nn; i++) {
            aa = (DoubleDouble)cyr[i - 1].clone();
            bb = (DoubleDouble)cyi[i - 1].clone();
            atol = DoubleDouble.valueOf(1.0);

            if (((aa.abs()).max(bb.abs())).le(ascle)) {
                aa = aa.multiply(rtol);
                bb = bb.multiply(rtol);
                atol = (DoubleDouble)tol.clone();
            } // if (Math.max(Math.abs(aa),Math.abs(bb)) <= ascle)

            str = (aa.multiply(csgnr)).subtract(bb.multiply(csgni));
            sti = (aa.multiply(csgni)).add(bb.multiply(csgnr));
            cyr[i - 1] = str.multiply(atol);
            cyi[i - 1] = sti.multiply(atol);
            str = (csgni.negate()).multiply(zti);
            csgni = csgnr.multiply(zti);
            csgnr = (DoubleDouble)str.clone();
        } // for (i = 1; i <= nn; i++)

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param cyr DoubleDouble[]
     * @param cyi DoubleDouble[]
     * @param nz int[]
     * @param ierr int[]
     */
    private void zbesi(DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n, final DoubleDouble[] cyr,
            final DoubleDouble[] cyi, final int[] nz, final int[] ierr) {

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
        int k;
        int k1;
        DoubleDouble aa;
        DoubleDouble dig;
        DoubleDouble az;
        DoubleDouble fn;
        DoubleDouble bb;
        DoubleDouble znr;
        DoubleDouble zni;
        DoubleDouble csgnr;
        DoubleDouble csgni;
        int inu;
        DoubleDouble arg;
        int nn;
        DoubleDouble rtol;
        DoubleDouble ascle;
        DoubleDouble atol;
        DoubleDouble str;
        DoubleDouble sti;
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
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */
        
        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));

        // DoubleDoubleDigits is I1MACH(14), the number of base-B digits for DoubleDouble
        // precision. IEEE 754 DoubleDouble precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // In DoubleDouble, the mantissa effectively has 106 bits of resolution.
        DoubleDoubleDigits = 106;
        k1 = DoubleDoubleDigits - 1;
        aa = r1m5.multiply(DoubleDouble.valueOf(k1));
        dig = aa.min(DoubleDouble.valueOf(18.0));
        aa = (DoubleDouble.valueOf(2.303)).multiply(aa);
        alim = elim.add( (aa.negate()).max(DoubleDouble.valueOf(-41.45)));
        rl = ((DoubleDouble.valueOf(1.2)).multiply(dig)).add(DoubleDouble.valueOf(3.0));
        fnul = (DoubleDouble.valueOf(10.0)).add((DoubleDouble.valueOf(6.0)).multiply(dig.subtract(DoubleDouble.valueOf(3.0))));

        // test for proper range
        az = zabs(zr, zi);
        fn = fnu.add(DoubleDouble.valueOf(n - 1.0));
        aa = (DoubleDouble.valueOf(0.5)).divide(tol);

        // I1MACH(9) = Integer.MAX_VALUE;
        bb = (DoubleDouble.valueOf(0.5)).multiply(DoubleDouble.valueOf(Integer.MAX_VALUE));
        aa = aa.min(bb);

        if ( (az.gt(aa)) || (fn.gt(aa))) {
            nz[0] = 0;
            ierr[0] = 4;

            return;
        }

        aa = aa.sqrt();

        if ( (az.gt(aa)) || (fn.gt(aa))) {
            ierr[0] = 3;
        }

        znr = (DoubleDouble)zr.clone();
        zni = (DoubleDouble)zi.clone();
        csgnr = DoubleDouble.valueOf(1.0);
        csgni = DoubleDouble.valueOf(0.0);

        if (zr.lt(DoubleDouble.valueOf(0.0))) {
            znr = zr.negate();
            zni = zi.negate();

            // Calculate csgn = exp(fnu * PI* i) to minimize losses
            // of significance when fnu is large
            inu = fnu.intValue();
            arg = DoubleDouble.PI.multiply(fnu.subtract(DoubleDouble.valueOf(inu)));

            if (zi.lt(DoubleDouble.valueOf(0.0))) {
                arg = arg.negate();
            }

            csgnr = arg.cos();
            csgni = arg.sin();

            if ( (inu % 2) != 0) {
                csgnr = csgnr.negate();
                csgni = csgni.negate();
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
        } else if (zr.ge(DoubleDouble.valueOf(0.0))) {
            return;
        }

        // Analytic continuation to the left half plane
        nn = n - nz[0];

        if (nn == 0) {
            return;
        }

        rtol = tol.reciprocal();
        ascle = (tiny.multiply(rtol)).multiply(DoubleDouble.valueOf(1.0e3));

        for (i = 0; i < nn; i++) {
            aa = (DoubleDouble)cyr[i].clone();
            bb = (DoubleDouble)cyi[i].clone();
            atol = DoubleDouble.valueOf(1.0);

            if (((aa.abs()).max(bb.abs())).le(ascle)) {
                aa = aa.multiply(rtol);
                bb = bb.multiply(rtol);
                atol = (DoubleDouble)tol.clone();
            }

            str = (aa.multiply(csgnr)).subtract(bb.multiply(csgni));
            sti = (aa.multiply(csgni)).add(bb.multiply(csgnr));
            cyr[i] = str.multiply(atol);
            cyi[i] = sti.multiply(atol);
            csgnr = csgnr.negate();
            csgni = csgni.negate();
        } // for (i = 0; i < nn; i++)

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param cyr DoubleDouble[]
     * @param cyi DoubleDouble[]
     * @param nz int[]
     * @param ierr int[]
     */
    private void zbesj(DoubleDouble zr, final DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n, final DoubleDouble[] cyr,
            final DoubleDouble[] cyi, final int[] nz, final int[] ierr) {

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
        int k;
        int k1;
        DoubleDouble aa;
        DoubleDouble dig;
        DoubleDouble az;
        DoubleDouble fn;
        DoubleDouble bb;
        DoubleDouble znr;
        DoubleDouble zni;
        DoubleDouble csgnr;
        DoubleDouble csgni;
        int inu;
        DoubleDouble arg;
        DoubleDouble cii;
        int inuh;
        int ir;
        int nl;
        DoubleDouble rtol;
        DoubleDouble ascle;
        DoubleDouble str;
        DoubleDouble sti;
        final DoubleDouble hpi = DoubleDouble.PI_2;
        int i;
        DoubleDouble atol;

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
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */

        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));

        // DoubleDoubleDigits is I1MACH(14), the number of base-B digits for DoubleDouble
        // precision. IEEE 754 DoubleDouble precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // In DoubleDouble, the mantissa effectively has 106 bits of resolution.
        DoubleDoubleDigits = 106;
        k1 = DoubleDoubleDigits - 1;
        aa = r1m5.multiply(DoubleDouble.valueOf(k1));
        dig = aa.min(DoubleDouble.valueOf(18.0));
        aa = (DoubleDouble.valueOf(2.303)).multiply(aa);
        alim = elim.add( (aa.negate()).max(DoubleDouble.valueOf(-41.45)));
        rl = ((DoubleDouble.valueOf(1.2)).multiply(dig)).add(DoubleDouble.valueOf(3.0));
        fnul = (DoubleDouble.valueOf(10.0)).add((DoubleDouble.valueOf(6.0)).multiply(dig.subtract(DoubleDouble.valueOf(3.0))));

        // test for proper range
        az = zabs(zr, zi);
        fn = fnu.add(DoubleDouble.valueOf(n - 1.0));
        aa = (DoubleDouble.valueOf(0.5)).divide(tol);

        // I1MACH(9) = Integer.MAX_VALUE;
        bb = (DoubleDouble.valueOf(0.5)).multiply(DoubleDouble.valueOf(Integer.MAX_VALUE));
        aa = aa.min(bb);

        if ( (az.gt(aa)) || (fn.gt(aa))) {
            nz[0] = 0;
            ierr[0] = 4;

            return;
        }

        aa = aa.sqrt();

        if ( (az.gt(aa)) || (fn.gt(aa))) {
            ierr[0] = 3;
        }

        // Calculate csgn = exp(fnu*hpi*i) to minimize losses of significance
        // when fnu is large
        cii = DoubleDouble.valueOf(1.0);
        inu = fnu.intValue();
        inuh = inu / 2;
        ir = inu - (2 * inuh);
        arg = (fnu.subtract(DoubleDouble.valueOf(inu - ir))).multiply(hpi);
        csgnr = arg.cos();
        csgni = arg.sin();

        if ( (inuh % 2) != 0) {
            csgnr = csgnr.negate();
            csgni = csgni.negate();
        } // if ((inuh%2) != 0)

        // zn is in the right half plane
        znr = (DoubleDouble)zi.clone();
        zni = zr.negate();

        if (zi.lt(DoubleDouble.valueOf(0.0))) {
            znr = znr.negate();
            zni = zni.negate();
            csgni = csgni.negate();
            cii = cii.negate();
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

        rtol = tol.reciprocal();
        ascle = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).multiply(rtol);

        for (i = 1; i <= nl; i++) {
            aa = (DoubleDouble)cyr[i - 1].clone();
            bb = (DoubleDouble)cyi[i - 1].clone();
            atol = DoubleDouble.valueOf(1.0);

            if (((aa.abs()).max(bb.abs())).le(ascle)) {
                aa = aa.multiply(rtol);
                bb = bb.multiply(rtol);
                atol = (DoubleDouble)tol.clone();
            } // if (Math.max(Math.abs(aa),Math.abs(bb)) <= ascle)

            str = (aa.multiply(csgnr)).subtract(bb.multiply(csgni));
            sti = (aa.multiply(csgni)).add(bb.multiply(csgnr));
            cyr[i - 1] = str.multiply(atol);
            cyi[i - 1] = sti.multiply(atol);
            str = (csgni.negate()).multiply(cii);
            csgni = csgnr.multiply(cii);
            csgnr = (DoubleDouble)str.clone();
        } // for (i = 1; i <= nl; i++)

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param cyr DoubleDouble[]
     * @param cyi DoubleDouble[]
     * @param nz int[]
     * @param ierr int[]
     */
    private void zbesk(final DoubleDouble zr, final DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n,
            final DoubleDouble[] cyr, final DoubleDouble[] cyi, final int[] nz, final int[] ierr) {

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
        int k;
        int k1;
        DoubleDouble aa;
        DoubleDouble bb;
        DoubleDouble dig;
        DoubleDouble az;
        DoubleDouble fn;
        DoubleDouble ufl;
        DoubleDouble arg;
        DoubleDouble aln;
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
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */

        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));

        // DoubleDoubleDigits is I1MACH(14), the number of base-B digits for DoubleDouble
        // precision. IEEE 754 DoubleDouble precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // In DoubleDouble, the mantissa effectively has 106 bits of resolution.
        DoubleDoubleDigits = 106;
        k1 = DoubleDoubleDigits - 1;
        aa = r1m5.multiply(DoubleDouble.valueOf(k1));
        dig = aa.min(DoubleDouble.valueOf(18.0));
        aa = (DoubleDouble.valueOf(2.303)).multiply(aa);
        alim = elim.add((aa.negate()).max(DoubleDouble.valueOf(-41.45)));
        rl = ((DoubleDouble.valueOf(1.2)).multiply(dig)).add(DoubleDouble.valueOf(3.0));
        fnul = (DoubleDouble.valueOf(10.0)).add((DoubleDouble.valueOf(6.0)).multiply(dig.subtract(DoubleDouble.valueOf(3.0))));

        // test for proper range
        az = zabs(zr, zi);
        fn = fnu.add(DoubleDouble.valueOf(nn - 1.0));
        aa = (DoubleDouble.valueOf(0.5)).divide(tol);

        // I1MACH(9) = Integer.MAX_VALUE;
        bb = (DoubleDouble.valueOf(0.5)).multiply(DoubleDouble.valueOf(Integer.MAX_VALUE));
        aa = aa.min(bb);

        if ( (az.gt(aa)) || (fn.gt(aa))) {
            nz[0] = 0;
            ierr[0] = 4;

            return;
        }

        aa = aa.sqrt();

        if ( (az.gt(aa)) || (fn.gt(aa))) {
            ierr[0] = 3;
        }

        // Overflow test on the last member of the sequence
        ufl = (DoubleDouble.valueOf(1.0E3)).multiply(tiny);

        if (az.lt(ufl)) {
            nz[0] = 0;
            ierr[0] = 2;

            return;
        } // if (az < ufl)

        if (fnu.gt(fnul)) {

            // Uniform asymptotic expansions for fnu > fnul
            mr = 0;

            if (zr.lt(DoubleDouble.valueOf(0.0))) {
                mr = 1;

                if (zi.lt(DoubleDouble.valueOf(0.0))) {
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

        if (fn.le(DoubleDouble.valueOf(1.0))) {

            if (zr.lt(DoubleDouble.valueOf(0.0))) {

                // Left half plane computation
                // PI/2 < arg(z) <= PI and -PI < arg(z) < -PI/2
                if (nz[0] != 0) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (nz[0] != 0)

                mr = 1;

                if (zi.lt(DoubleDouble.valueOf(0.0))) {
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

        if (fn.gt(DoubleDouble.valueOf(2.0))) {
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

                if (zr.lt(DoubleDouble.valueOf(0.0))) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (zr < 0.0)

                return;
            } // if (nn == 0)

            if (zr.lt(DoubleDouble.valueOf(0.0))) {

                // Left half plane computation
                // PI/2 < arg(z) <= PI and -PI < arg(z) < -PI/2
                if (nz[0] != 0) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (nz[0] != 0)

                mr = 1;

                if (zi.lt(DoubleDouble.valueOf(0.0))) {
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

        if (az.gt(tol)) {

            if (zr.lt(DoubleDouble.valueOf(0.0))) {

                // Left half plane computation
                // PI/2 < arg(z) <= PI and -PI < arg(z) < -PI/2
                if (nz[0] != 0) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (nz[0] != 0)

                mr = 1;

                if (zi.lt(DoubleDouble.valueOf(0.0))) {
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

        arg = (DoubleDouble.valueOf(0.5)).multiply(az);
        aln = (fn.negate()).multiply(arg.log());

        if (aln.gt(elim)) {
            nz[0] = 0;
            ierr[0] = 2;

            return;
        } // if (aln > elim)

        if (zr.lt(DoubleDouble.valueOf(0.0))) {

            // Left half plane computation
            // PI/2 < arg(z) <= PI and -PI < arg(z) < -PI/2
            if (nz[0] != 0) {
                nz[0] = 0;
                ierr[0] = 2;

                return;
            } // if (nz[0] != 0)

            mr = 1;

            if (zi.lt(DoubleDouble.valueOf(0.0))) {
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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param cyr DoubleDouble[]
     * @param cyi DoubleDouble[]
     * @param nz int[]
     * @param cwrkr DoubleDouble[]
     * @param cwrki DoubleDouble[]
     * @param ierr int[]
     */
    private void zbesy(final DoubleDouble zr, final DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n,
            final DoubleDouble[] cyr, final DoubleDouble[] cyi, final int[] nz, final DoubleDouble[] cwrkr, final DoubleDouble[] cwrki,
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
        final DoubleDouble[] cipr = new DoubleDouble[] {DoubleDouble.valueOf(1.0), DoubleDouble.valueOf(0.0),
        		                     DoubleDouble.valueOf(-1.0), DoubleDouble.valueOf(0.0)};
        final DoubleDouble[] cipi = new DoubleDouble[] {DoubleDouble.valueOf(0.0), DoubleDouble.valueOf(1.0),
        		                     DoubleDouble.valueOf(0.0), DoubleDouble.valueOf(-1.0)};
        final DoubleDouble hpi = DoubleDouble.PI_2;
        DoubleDouble zzr;
        DoubleDouble zzi;
        DoubleDouble znr;
        DoubleDouble zni;
        final int[] nz1 = new int[1];
        final int[] nz2 = new int[1];
        int ifnu;
        DoubleDouble ffnu;
        DoubleDouble arg;
        DoubleDouble csgnr;
        DoubleDouble csgni;
        int i4;
        DoubleDouble str;
        DoubleDouble sti;
        DoubleDouble rhpi;
        int i;
        DoubleDouble exr;
        DoubleDouble exi;
        DoubleDouble ey;
        DoubleDouble tay;
        DoubleDouble rtol;
        DoubleDouble ascle;
        DoubleDouble zvr;
        DoubleDouble zvi;
        DoubleDouble atol;
        DoubleDouble zur;
        DoubleDouble zui;
        DoubleDouble cspnr;
        DoubleDouble cspni;
        int k;

        zzr = (DoubleDouble)zr.clone();
        zzi = (DoubleDouble)zi.clone();

        if (zi.lt(DoubleDouble.valueOf(0.0))) {
            zzi = zzi.negate();
        } // if (zi < 0.0)

        znr = (DoubleDouble)zzi.clone();
        zni = zzr.negate();
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
        ifnu = fnu.intValue();
        ffnu = fnu.subtract(DoubleDouble.valueOf(ifnu));
        arg = hpi.multiply(ffnu);
        csgnr = arg.cos();
        csgni = arg.sin();
        i4 = (ifnu % 4) + 1;
        str = (csgnr.multiply(cipr[i4 - 1])).subtract(csgni.multiply(cipi[i4 - 1]));
        csgni = (csgnr.multiply(cipi[i4 - 1])).add(csgni.multiply(cipr[i4 - 1]));
        csgnr = (DoubleDouble)str.clone();
        rhpi = hpi.reciprocal();
        cspnr = csgnr.multiply(rhpi);
        cspni = (csgni.negate()).multiply(rhpi);
        str = csgni.negate();
        csgni = (DoubleDouble)csgnr.clone();
        csgnr = (DoubleDouble)str.clone();

        if (kode != 2) {

            for (i = 1; i <= n; i++) {
                str = (csgnr.multiply(cyr[i - 1])).subtract(csgni.multiply(cyi[i - 1]));
                str = str.subtract( (cspnr.multiply(cwrkr[i - 1])).subtract(cspni.multiply(cwrki[i - 1])));
                sti = (csgnr.multiply(cyi[i - 1])).add(csgni.multiply(cyr[i - 1]));
                sti = sti.subtract( (cspnr.multiply(cwrki[i - 1])).add(cspni.multiply(cwrkr[i - 1])));
                cyr[i - 1] = (DoubleDouble)str.clone();
                cyi[i - 1] = (DoubleDouble)sti.clone();
                str = csgni.negate();
                csgni = (DoubleDouble)csgnr.clone();
                csgnr = (DoubleDouble)str.clone();
                str = (DoubleDouble)cspni.clone();
                cspni = cspnr.negate();
                cspnr = (DoubleDouble)str.clone();
            } // for (i = 1; i <= n; i++)

            if (zi.lt(DoubleDouble.valueOf(0.0))) {

                for (i = 1; i <= n; i++) {
                    cyi[i - 1] = cyi[i - 1].negate();
                } // for (i = 1; i <= n; i++)
            } // if (zi < 0.0)

            return;
        } // if (kode != 2)

        // here kode == 2
        exr = zr.cos();
        exi = zr.sin();

        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */

        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));

        // elim is the approximate under- and overflow limit
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));
        ey = DoubleDouble.valueOf(0.0);
        tay = (zi.add(zi)).abs();

        if (tay.lt(elim)) {
            ey = (tay.negate()).exp();
        } // if (tay < elim)

        str = ( (exr.multiply(cspnr)).subtract(exi.multiply(cspni))).multiply(ey);
        cspni = ( (exr.multiply(cspni)).add(exi.multiply(cspnr))).multiply(ey);
        cspnr = (DoubleDouble)str.clone();
        nz[0] = 0;
        rtol = tol.reciprocal();
        ascle = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).multiply(rtol);

        for (i = 1; i <= n; i++) {

            // cy[i-1] = csgn*cy[i-1] - cspn*cwrk[i-1]: products are computed in
            // scaled mode if cy[i-1] or cwrk[i-1] are close to underflow to
            // prevent underflow in an intermediate computation
            zvr = (DoubleDouble)cwrkr[i - 1].clone();
            zvi = (DoubleDouble)cwrki[i - 1].clone();
            atol = DoubleDouble.valueOf(1.0);

            if (((zvr.abs()).max(zvi.abs())).le(ascle)) {
                zvr = zvr.multiply(rtol);
                zvi = zvi.multiply(rtol);
                atol = (DoubleDouble)tol.clone();
            } // if (Math.max(Math.abs(zvr),Math.abs(zvi)) <= ascle)

            str = ( (zvr.multiply(cspnr)).subtract(zvi.multiply(cspni))).multiply(atol);
            zvi = ( (zvr.multiply(cspni)).add(zvi.multiply(cspnr))).multiply(atol);
            zvr = (DoubleDouble)str.clone();
            zur = (DoubleDouble)cyr[i - 1].clone();
            zui = (DoubleDouble)cyi[i - 1].clone();
            atol = DoubleDouble.valueOf(1.0);

            if (((zur.abs()).max(zui.abs())).le(ascle)) {
                zur = zur.multiply(rtol);
                zui = zui.multiply(rtol);
                atol = (DoubleDouble)tol.clone();
            } // if (Math.max(Math.abs(zur),Math.abs(zui)) <= ascle)

            str = ( (zur.multiply(csgnr)).subtract(zui.multiply(csgni))).multiply(atol);
            zui = ( (zur.multiply(csgni)).add(zui.multiply(csgnr))).multiply(atol);
            zur = (DoubleDouble)str.clone();
            cyr[i - 1] = zur.subtract(zvr);
            cyi[i - 1] = zui.subtract(zvi);

            if (zi.lt(DoubleDouble.valueOf(0.0))) {
                cyi[i - 1] = cyi[i - 1].negate();
            } // if (zi < 0.0)

            if ( (cyr[i - 1].equals(DoubleDouble.valueOf(0.0))) && (cyi[i - 1].equals(DoubleDouble.valueOf(0.0))) &&
            		(ey.equals(DoubleDouble.valueOf(0.0)))) {
                nz[0] = nz[0] + 1;
            } // if ((cyr[i-1] == 0.0) && (cyi[i-1] == 0.0) && (ey == 0.0))

            str = csgni.negate();
            csgni = (DoubleDouble)csgnr.clone();
            csgnr = (DoubleDouble)str.clone();
            str = (DoubleDouble)cspni.clone();
            cspni = cspnr.negate();
            cspnr = (DoubleDouble)str.clone();
        } // for (i = 1; i <= n; i++)

        return;
    }

    /**
     * An older version of zbesy used by the ZQCBY quick check routine.
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param cyr DoubleDouble[]
     * @param cyi DoubleDouble[]
     * @param nz int[]
     * @param cwrkr DoubleDouble[]
     * @param cwrki DoubleDouble[]
     * @param ierr int[]
     */
    private void zbesyh(final DoubleDouble zr, final DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n,
            final DoubleDouble[] cyr, final DoubleDouble[] cyi, final int[] nz, final DoubleDouble[] cwrkr, final DoubleDouble[] cwrki,
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
        DoubleDouble hcii;
        final int[] nz1 = new int[1];
        final int[] nz2 = new int[1];
        int i;
        DoubleDouble str;
        DoubleDouble sti;
        int k;
        DoubleDouble exr;
        DoubleDouble exi;
        DoubleDouble ey;
        DoubleDouble tay;
        DoubleDouble c1r;
        DoubleDouble c1i;
        DoubleDouble c2r;
        DoubleDouble c2i;
        DoubleDouble rtol;
        DoubleDouble ascle;
        DoubleDouble aa;
        DoubleDouble bb;
        DoubleDouble atol;

        ierr[0] = 0;
        nz[0] = 0;

        if ( (zr.equals(DoubleDouble.valueOf(0.0))) && (zi.equals(DoubleDouble.valueOf(0.0)))) {
            ierr[0] = 1;
        } // if ((zr == 0.0) && (zi == 0.0))

        if (fnu.lt(DoubleDouble.valueOf(0.0))) {
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

        hcii = DoubleDouble.valueOf(0.5);
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
                str = cwrkr[i - 1].subtract(cyr[i - 1]);
                sti = cwrki[i - 1].subtract(cyi[i - 1]);
                cyr[i - 1] = (sti.negate()).multiply(hcii);
                cyi[i - 1] = str.multiply(hcii);
            } // for (i = 1; i <= n; i++)

            return;
        } // if (kode != 2)

        // Here kode = 2
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */

        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));

        // elim is the approximate under- and overflow limit
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));
        exr = zr.cos();
        exi = zr.sin();
        ey = DoubleDouble.valueOf(0.0);
        tay = (zi.add(zi)).abs();

        if (tay.lt(elim)) {
            ey = (tay.negate()).exp();
        } // if (tay < elim)

        if (zi.lt(DoubleDouble.valueOf(0.0))) {
            c1r = (DoubleDouble)exr.clone();
            c1i = (DoubleDouble)exi.clone();
            c2r = exr.multiply(ey);
            c2i = (exi.negate()).multiply(ey);
        } // if (zi < 0.0)
        else { // zi >= 0.0
            c1r = exr.multiply(ey);
            c1i = exi.multiply(ey);
            c2r = (DoubleDouble)exr.clone();
            c2i = exi.negate();
        } // else zi >= 0.0

        nz[0] = 0;
        rtol = tol.reciprocal();
        ascle = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).multiply(rtol);

        for (i = 1; i <= n; i++) {
            aa = (DoubleDouble)cwrkr[i - 1].clone();
            bb = (DoubleDouble)cwrki[i - 1].clone();
            atol = DoubleDouble.valueOf(1.0);

            if (((aa.abs()).max(bb.abs())).le(ascle)) {
                aa = aa.multiply(rtol);
                bb = bb.multiply(rtol);
                atol = (DoubleDouble)tol.clone();
            } // if (Math.max(Math.abs(aa),Math.abs(bb)) <= ascle)

            str = ( (aa.multiply(c2r)).subtract(bb.multiply(c2i))).multiply(atol);
            sti = ( (aa.multiply(c2i)).add(bb.multiply(c2r))).multiply(atol);
            aa = (DoubleDouble)cyr[i - 1].clone();
            bb = (DoubleDouble)cyi[i - 1].clone();
            atol = DoubleDouble.valueOf(1.0);

            if (((aa.abs()).max(bb.abs())).le(ascle)) {
                aa = aa.multiply(rtol);
                bb = bb.multiply(rtol);
                atol = (DoubleDouble)tol.clone();
            } // if (Math.max(Math.abs(aa),Math.abs(bb)) <= ascle)

            str = str.subtract( ( (aa.multiply(c1r)).subtract(bb.multiply(c1i))).multiply(atol));
            sti = sti.subtract( ( (aa.multiply(c1i)).add(bb.multiply(c1r))).multiply(atol));
            cyr[i - 1] = (sti.negate()).multiply(hcii);
            cyi[i - 1] = str.multiply(hcii);

            if ( (str.equals(DoubleDouble.valueOf(0.0))) && (sti.equals(DoubleDouble.valueOf(0.0))) &&
            		(ey.equals(DoubleDouble.valueOf(0.0)))) {
                nz[0] = nz[0] + 1;
            } // if ((str == 0.0) && (sti== 0.0) && (ey == 0.0))
        } // for (i = 1; i <= n; i++)

        return;
    }

    /**
     * zbinu calculates the I function in the right half z plane.
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param cyr DoubleDouble[]
     * @param cyi DoubleDouble[]
     * @param nz int[]
     */
    private void zbinu(final DoubleDouble zr, final DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n,
            final DoubleDouble[] cyr, final DoubleDouble[] cyi, final int[] nz) {
        DoubleDouble az;
        int nn;
        DoubleDouble dfnu;
        final int[] nw = new int[1];
        int inw;
        boolean doMiller = true;
        boolean doOverflow = true;
        boolean doTest = true;
        final DoubleDouble[] cwr = new DoubleDouble[2];
        final DoubleDouble[] cwi = new DoubleDouble[2];
        int i;
        int nui;
        final int[] nlast = new int[1];

        nz[0] = 0;
        az = zabs(zr, zi);
        nn = n;
        dfnu = fnu.add(DoubleDouble.valueOf(n - 1.0));

        if ( (az.le(DoubleDouble.valueOf(2.0))) || 
        		( (((DoubleDouble.valueOf(0.25)).multiply(az)).multiply(az)).le(dfnu.add(DoubleDouble.valueOf(1.0))))) {

            // power series
            zseri(zr, zi, fnu, kode, nn, cyr, cyi, nw);
            inw = Math.abs(nw[0]);
            nz[0] = nz[0] + inw;
            nn = nn - inw;

            if ( (nn == 0) || (nw[0] >= 0)) {
                return;
            }

            dfnu = fnu.add(DoubleDouble.valueOf(nn - 1.0));
        } // if ((az <= 2.0) || (0.25*az*az <= (dfnu+1.0)))

        if ( (az.ge(rl)) && ( (dfnu.le(DoubleDouble.valueOf(1.0))) ||
        		( (dfnu.gt(DoubleDouble.valueOf(1.0))) && ( ((DoubleDouble.valueOf(2.0)).multiply(az)).ge(dfnu.multiply(dfnu)))))) {

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

        if ( ( (az.ge(rl)) && (dfnu.gt(DoubleDouble.valueOf(1.0))) && 
        		( ((DoubleDouble.valueOf(2.0)).multiply(az)).lt(dfnu.multiply(dfnu)))) ||
        		( (az.lt(rl)) && (dfnu.gt(DoubleDouble.valueOf(1.0))))) {

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

            dfnu = fnu.add(DoubleDouble.valueOf(nn - 1.0));

            if ( (dfnu.gt(fnul)) || (az.gt(fnul))) {
                doMiller = false;
                doOverflow = false;
            }
        } //
        else {
            doTest = false;
        }

        while (true) {

            if (doTest && (az.gt(rl))) {
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
                        cyr[i] = DoubleDouble.valueOf(0.0);
                        cyi[i] = DoubleDouble.valueOf(0.0);
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
            nui = (fnul.subtract(dfnu)).intValue() + 1;
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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param id int
     * @param kode int
     * @param bir DoubleDouble[]
     * @param bii DoubleDouble[]
     * @param ierr int[]
     */
    private void zbiry(final DoubleDouble zr, final DoubleDouble zi, final int id, final int kode, final DoubleDouble[] bir,
            final DoubleDouble[] bii, final int[] ierr) {

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
        final DoubleDouble[] cyr = new DoubleDouble[2];
        final DoubleDouble[] cyi = new DoubleDouble[2];
        final DoubleDouble tth = (DoubleDouble.valueOf(2.0)).divide(DoubleDouble.valueOf(3.0));
        final DoubleDouble c1 = DoubleDouble.valueOf(6.14926627446000736E-01);
        final DoubleDouble c2 = DoubleDouble.valueOf(4.48288357353826359E-01);
        final DoubleDouble coef = DoubleDouble.valueOf(5.77350269189625765E-01);
        final int[] nz = new int[1];
        DoubleDouble az;
        DoubleDouble fid;
        DoubleDouble s1r;
        DoubleDouble s1i;
        DoubleDouble s2r;
        DoubleDouble s2i;
        DoubleDouble aa;
        DoubleDouble trm1r;
        DoubleDouble trm1i;
        DoubleDouble trm2r;
        DoubleDouble trm2i;
        DoubleDouble atrm;
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        DoubleDouble z3r;
        DoubleDouble z3i;
        DoubleDouble az3;
        DoubleDouble ak;
        DoubleDouble bk;
        DoubleDouble ck;
        DoubleDouble dk;
        DoubleDouble d1;
        DoubleDouble d2;
        DoubleDouble ad;
        int k;
        DoubleDouble ztar;
        DoubleDouble ztai;
        DoubleDouble eaa;
        DoubleDouble cc;
        DoubleDouble dig;
        DoubleDouble bb;
        final DoubleDouble[] csqr = new DoubleDouble[1];
        final DoubleDouble[] csqi = new DoubleDouble[1];
        DoubleDouble sfac;
        DoubleDouble fmr;
        DoubleDouble fnu;
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
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */

        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));
        fid = DoubleDouble.valueOf(id);

        if (az.le(DoubleDouble.valueOf(1.0))) {

            // Power series for CABS(z) <= 1
            s1r = DoubleDouble.valueOf(1.0);
            s1i = DoubleDouble.valueOf(0.0);
            s2r = DoubleDouble.valueOf(1.0);
            s2i = DoubleDouble.valueOf(0.0);

            if (az.lt(tol)) {
                aa = (c1.multiply((DoubleDouble.valueOf(1.0)).subtract(fid))).add(fid.multiply(c2));
                bir[0] = (DoubleDouble)aa.clone();
                bii[0] = DoubleDouble.valueOf(0.0);

                return;
            } // if (az < tol)

            aa = az.multiply(az);

            if (aa.ge(tol.divide(az))) {
                trm1r = DoubleDouble.valueOf(1.0);
                trm1i = DoubleDouble.valueOf(0.0);
                trm2r = DoubleDouble.valueOf(1.0);
                trm2i = DoubleDouble.valueOf(0.0);
                atrm = DoubleDouble.valueOf(1.0);
                str[0] = (zr.multiply(zr)).subtract(zi.multiply(zi));
                sti[0] = ((DoubleDouble.valueOf(2.0)).multiply(zr)).multiply(zi);
                z3r = (str[0].multiply(zr)).subtract(sti[0].multiply(zi));
                z3i = (str[0].multiply(zi)).add(sti[0].multiply(zr));
                az3 = az.multiply(aa);
                ak = (DoubleDouble.valueOf(2.0)).add(fid);
                bk = ((DoubleDouble.valueOf(3.0)).subtract(fid)).subtract(fid);
                ck = (DoubleDouble.valueOf(4.0)).subtract(fid);
                dk = ((DoubleDouble.valueOf(3.0)).add(fid)).add(fid);
                d1 = ak.multiply(dk);
                d2 = bk.multiply(ck);
                ad = d1.min(d2);
                ak = (DoubleDouble.valueOf(24.0)).add((DoubleDouble.valueOf(9.0)).multiply(fid));
                bk = (DoubleDouble.valueOf(30.0)).subtract((DoubleDouble.valueOf(9.0)).multiply(fid));

                for (k = 1; k <= 25; k++) {
                    str[0] = ( (trm1r.multiply(z3r)).subtract(trm1i.multiply(z3i))).divide(d1);
                    trm1i = ( (trm1r.multiply(z3i)).add(trm1i.multiply(z3r))).divide(d1);
                    trm1r = (DoubleDouble)str[0].clone();
                    s1r = s1r.add(trm1r);
                    s1i = s1i.add(trm1i);
                    str[0] = ( (trm2r.multiply(z3r)).subtract(trm2i.multiply(z3i))).divide(d2);
                    trm2i = ( (trm2r.multiply(z3i)).add(trm2i.multiply(z3r))).divide(d2);
                    trm2r = (DoubleDouble)str[0].clone();
                    s2r = s2r.add(trm2r);
                    s2i = s2i.add(trm2i);
                    atrm = (atrm.multiply(az3)).divide(ad);
                    d1 = d1.add(ak);
                    d2 = d2.add(bk);
                    ad = d1.min(d2);

                    if (atrm.lt(tol.multiply(ad))) {
                        break;
                    } // if (atrm < (tol*ad))

                    ak = ak.add(DoubleDouble.valueOf(18.0));
                    bk = bk.add(DoubleDouble.valueOf(18.0));
                } // for (k = 1; k <= 25; k++)
            } // if (aa >= (tol/az))

            if (id != 1) {
                bir[0] = (c1.multiply(s1r)).add(c2.multiply( (zr.multiply(s2r)).subtract(zi.multiply(s2i))));
                bii[0] = (c1.multiply(s1i)).add(c2.multiply( (zr.multiply(s2i)).add(zi.multiply(s2r))));

                if (kode == 1) {
                    return;
                } // if (kode == 1)

                zsqrt(zr, zi, str, sti);
                ztar = tth.multiply( (zr.multiply(str[0])).subtract(zi.multiply(sti[0])));
                ztai = tth.multiply( (zr.multiply(sti[0])).add(zi.multiply(str[0])));
                aa = (DoubleDouble)ztar.clone();
                aa = (aa.abs()).negate();
                eaa = aa.exp();
                bir[0] = bir[0].multiply(eaa);
                bii[0] = bii[0].multiply(eaa);

                return;
            } // if (id != 1)

            bir[0] = s2r.multiply(c2);
            bii[0] = s2i.multiply(c2);

            if (az.gt(tol)) {
                cc = c1.divide((DoubleDouble.valueOf(1.0)).add(fid));
                str[0] = (s1r.multiply(zr)).subtract(s1i.multiply(zi));
                sti[0] = (s1r.multiply(zi)).add(s1i.multiply(zr));
                bir[0] = bir[0].add(cc.multiply( (str[0].multiply(zr)).subtract(sti[0].multiply(zi))));
                bii[0] = bii[0].add(cc.multiply( (str[0].multiply(zi)).add(sti[0].multiply(zr))));
            } // if (az > tol)

            if (kode == 1) {
                return;
            } // if (kode == 1)

            zsqrt(zr, zi, str, sti);
            ztar = tth.multiply( (zr.multiply(str[0])).subtract(zi.multiply(sti[0])));
            ztai = tth.multiply( (zr.multiply(sti[0])).add(zi.multiply(str[0])));
            aa = (DoubleDouble)ztar.clone();
            aa = (aa.abs()).negate();
            eaa = aa.exp();
            bir[0] = bir[0].multiply(eaa);
            bii[0] = bii[0].multiply(eaa);

            return;
        } // if (az <= 1.0)

        // Case for CABS(z) > 1.0
        fnu = ((DoubleDouble.valueOf(1.0)).add(fid)).divide(DoubleDouble.valueOf(3.0));

        /**
         * SET PARAMETERS RELATED TO MACHINE CONSTANTS. C TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18. C
         * ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT. C EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
         * C EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR C UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED
         * ARITHMETIC IS DONE. C RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. C DIG = NUMBER OF
         * BASE 10 DIGITS IN TOL = 10**(-DIG).C FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU.
         */

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));

        // DoubleDoubleDigits is I1MACH(14), the number of base-B digits for DoubleDouble
        // precision. IEEE 754 DoubleDouble precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // In DoubleDouble, the mantissa effectively has 106 bits of resolution.
        DoubleDoubleDigits = 106;
        k1 = DoubleDoubleDigits - 1;
        aa = r1m5.multiply(DoubleDouble.valueOf(k1));
        dig = aa.min(DoubleDouble.valueOf(18.0));
        aa = (DoubleDouble.valueOf(2.303)).multiply(aa);
        alim = elim.add((aa.negate()).max(DoubleDouble.valueOf(-41.45)));
        rl = ((DoubleDouble.valueOf(1.2)).multiply(dig)).add(DoubleDouble.valueOf(3.0));
        fnul = (DoubleDouble.valueOf(10.0)).add((DoubleDouble.valueOf(6.0)).multiply(dig.subtract(DoubleDouble.valueOf(3.0))));

        // Test for range
        aa = (DoubleDouble.valueOf(0.5)).divide(tol);

        // I1MACH(9) = Integer.MAX_VALUE;
        bb = (DoubleDouble.valueOf(0.5)).multiply(DoubleDouble.valueOf(Integer.MAX_VALUE));
        aa = aa.min(bb);
        aa = aa.pow(tth);

        if (az.gt(aa)) {
            ierr[0] = 4;
            nz[0] = 0;

            return;
        } // if (az > aa)

        aa = aa.sqrt();

        if (az.gt(aa)) {
            ierr[0] = 3;
        } // if (az > aa)

        zsqrt(zr, zi, csqr, csqi);
        ztar = tth.multiply( (zr.multiply(csqr[0])).subtract(zi.multiply(csqi[0])));
        ztai = tth.multiply( (zr.multiply(csqi[0])).add(zi.multiply(csqr[0])));

        // Re(zta) <= 0 when Re(z) < 0, especially when Im(z) is small
        sfac = DoubleDouble.valueOf(1.0);
        ak = (DoubleDouble)ztai.clone();

        if (zr.lt(DoubleDouble.valueOf(0.0))) {
            bk = (DoubleDouble)ztar.clone();
            ck = (bk.abs()).negate();
            ztar = (DoubleDouble)ck.clone();
            ztai = (DoubleDouble)ak.clone();
        } // if (zr < 0.0)

        if ( (zi.equals(DoubleDouble.valueOf(0.0))) && (zr.le(DoubleDouble.valueOf(0.0)))) {
            ztar = DoubleDouble.valueOf(0.0);
            ztai = (DoubleDouble)ak.clone();
        } // if ((zi == 0.0) && (zr <= 0.0))

        aa = (DoubleDouble)ztar.clone();

        if (kode != 2) {

            // Overflow test
            bb = aa.abs();

            if (bb.ge(alim)) {
                bb = bb.add((DoubleDouble.valueOf(0.25)).multiply(az.log()));
                sfac = (DoubleDouble)tol.clone();

                if (bb .gt(elim)) {
                    ierr[0] = 2;
                    nz[0] = 0;

                    return;
                } // if (bb > elim)
            } // if (bb >= alim)
        } // if (kode != 2)

        fmr = DoubleDouble.valueOf(0.0);

        if ( (aa.lt(DoubleDouble.valueOf(0.0))) || (zr.le(DoubleDouble.valueOf(0.0)))) {
            fmr = DoubleDouble.PI;

            if (zi.lt(DoubleDouble.valueOf(0.0))) {
                fmr = (DoubleDouble.PI).negate();
            } // if (zi < 0.0)

            ztar = ztar.negate();
            ztai = ztai.negate();
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

        aa = fmr.multiply(fnu);
        z3r = (DoubleDouble)sfac.clone();
        str[0] = aa.cos();
        sti[0] = aa.sin();
        s1r = ( (str[0].multiply(cyr[0])).subtract(sti[0].multiply(cyi[0]))).multiply(z3r);
        s1i = ( (str[0].multiply(cyi[0])).add(sti[0].multiply(cyr[0]))).multiply(z3r);
        fnu = ((DoubleDouble.valueOf(2.0)).subtract(fid)).divide(DoubleDouble.valueOf(3.0));
        zbinu(ztar, ztai, fnu, kode, 2, cyr, cyi, nz);
        cyr[0] = cyr[0].multiply(z3r);
        cyi[0] = cyi[0].multiply(z3r);
        cyr[1] = cyr[1].multiply(z3r);
        cyi[1] = cyi[1].multiply(z3r);

        // Backward recur one step for orders -1/3 or -2/3
        zdiv(cyr[0], cyi[0], ztar, ztai, str, sti);
        s2r = ( (fnu.add(fnu)).multiply(str[0])).add(cyr[1]);
        s2i = ( (fnu.add(fnu)).multiply(sti[0])).add(cyi[1]);
        aa = fmr.multiply(fnu.subtract(DoubleDouble.valueOf(1.0)));
        str[0] = aa.cos();
        sti[0] = aa.sin();
        s1r = coef.multiply(s1r.add(s2r.multiply(str[0])).subtract(s2i.multiply(sti[0])));
        s1i = coef.multiply(s1i.add(s2r.multiply(sti[0])).add(s2i.multiply(str[0])));

        if (id != 1) {
            str[0] = (csqr[0].multiply(s1r)).subtract(csqi[0].multiply(s1i));
            s1i = (csqr[0].multiply(s1i)).add(csqi[0].multiply(s1r));
            s1r = (DoubleDouble)str[0].clone();
            bir[0] = s1r.divide(sfac);
            bii[0] = s1i.divide(sfac);

            return;
        } // if (id != 1)

        // id == 1
        str[0] = (zr.multiply(s1r)).subtract(zi.multiply(s1i));
        s1i = (zr.multiply(s1i)).add(zi.multiply(s1r));
        s1r = (DoubleDouble)str[0].clone();
        bir[0] = s1r.divide(sfac);
        bii[0] = s1i.divide(sfac);

        return;
    }

    /**
     * zbknu computes the k Bessel function in the right half z plane.
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     */
    private void zbknu(DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n, final DoubleDouble[] yr,
            final DoubleDouble[] yi, final int[] nz) {
        final int kmax = 30;
        final DoubleDouble ctwor = DoubleDouble.valueOf(2.0);
        final DoubleDouble r1 = DoubleDouble.valueOf(2.0);
        final DoubleDouble rthpi = DoubleDouble.valueOf(1.25331413731550025);
        final DoubleDouble spi = DoubleDouble.valueOf(1.90985931710274403);
        final DoubleDouble fpi = DoubleDouble.valueOf(1.89769999331517738);
        final DoubleDouble tth = (DoubleDouble.valueOf(2.0)).divide(DoubleDouble.valueOf(3.0));
        final DoubleDouble[] cc = new DoubleDouble[] {DoubleDouble.valueOf(5.77215664901532861E-01), 
        		DoubleDouble.valueOf(-4.20026350340952355E-02), DoubleDouble.valueOf(-4.21977345555443367E-02),
                DoubleDouble.valueOf(7.21894324666309954E-03), DoubleDouble.valueOf(-2.15241674114950973E-04),
                DoubleDouble.valueOf(-2.01348547807882387E-05), DoubleDouble.valueOf(1.13302723198169588E-06),
                DoubleDouble.valueOf(6.11609510448141582E-09)};
        DoubleDouble caz;
        DoubleDouble csclr;
        DoubleDouble crscr;
        final DoubleDouble[] cssr = new DoubleDouble[3];
        final DoubleDouble[] csrr = new DoubleDouble[3];
        final DoubleDouble[] bry = new DoubleDouble[3];
        int iflag;
        int koded;
        DoubleDouble rcaz;
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        DoubleDouble rzr;
        DoubleDouble rzi;
        int inu;
        DoubleDouble dnu;
        DoubleDouble dnu2 = DoubleDouble.valueOf(0.0);
        DoubleDouble fc;
        final DoubleDouble[] smur = new DoubleDouble[1];
        final DoubleDouble[] smui = new DoubleDouble[1];
        final int[] idum = new int[1];
        DoubleDouble fmur;
        DoubleDouble fmui;
        final DoubleDouble[] cshr = new DoubleDouble[1];
        final DoubleDouble[] cshi = new DoubleDouble[1];
        final DoubleDouble[] cchr = new DoubleDouble[1];
        final DoubleDouble[] cchi = new DoubleDouble[1];
        DoubleDouble a2;
        DoubleDouble t2;
        DoubleDouble t1;
        DoubleDouble ak = DoubleDouble.valueOf(0.0);
        DoubleDouble s;
        int k;
        DoubleDouble tm;
        DoubleDouble g1;
        DoubleDouble g2;
        final DoubleDouble[] fr = new DoubleDouble[1];
        final DoubleDouble[] fi = new DoubleDouble[1];
        DoubleDouble pr;
        DoubleDouble pi;
        final DoubleDouble[] ptr = new DoubleDouble[1];
        final DoubleDouble[] pti = new DoubleDouble[1];
        DoubleDouble qr;
        DoubleDouble qi;
        final DoubleDouble[] s1r = new DoubleDouble[1];
        final DoubleDouble[] s1i = new DoubleDouble[1];
        final DoubleDouble[] s2r = new DoubleDouble[1];
        final DoubleDouble[] s2i = new DoubleDouble[1];
        DoubleDouble a1;
        DoubleDouble ckr = DoubleDouble.valueOf(0.0);
        DoubleDouble cki = DoubleDouble.valueOf(0.0);
        DoubleDouble bk;
        final DoubleDouble[] czr = new DoubleDouble[1];
        final DoubleDouble[] czi = new DoubleDouble[1];
        DoubleDouble rak;
        final DoubleDouble[] yr0 = new DoubleDouble[1];
        final DoubleDouble[] yi0 = new DoubleDouble[1];
        int kflag = 0;
        final DoubleDouble[] coefr = new DoubleDouble[1];
        final DoubleDouble[] coefi = new DoubleDouble[1];
        DoubleDouble fhs = DoubleDouble.valueOf(0.0);
        DoubleDouble etest;
        DoubleDouble fk = DoubleDouble.valueOf(0.0);
        DoubleDouble fks;
        DoubleDouble cbr;
        DoubleDouble p2r;
        DoubleDouble p2i;
        DoubleDouble p1r;
        DoubleDouble p1i;
        DoubleDouble aa;
        DoubleDouble bb;
        DoubleDouble cbi;
        DoubleDouble csr;
        DoubleDouble csi;
        DoubleDouble zdr = DoubleDouble.valueOf(0.0);
        DoubleDouble zdi = DoubleDouble.valueOf(0.0);
        int inub = 0;
        DoubleDouble ascle;
        DoubleDouble p2m;
        int kk = 1;
        int i;
        DoubleDouble helim;
        DoubleDouble elm;
        DoubleDouble celmr;
        int ic;
        int j;
        DoubleDouble as;
        DoubleDouble alas;
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
        csclr = tol.reciprocal();
        crscr = (DoubleDouble)tol.clone();
        cssr[0] = (DoubleDouble)csclr.clone();
        cssr[1] = DoubleDouble.valueOf(1.0);
        cssr[2] = (DoubleDouble)crscr.clone();
        csrr[0] = (DoubleDouble)crscr.clone();
        csrr[1] = DoubleDouble.valueOf(1.0);
        csrr[2] = (DoubleDouble)csclr.clone();
        bry[0] = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).divide(tol);
        bry[1] = bry[0].reciprocal();
        bry[2] = DoubleDouble.valueOf(Double.MAX_VALUE);
        nz[0] = 0;
        iflag = 0;
        koded = kode;
        rcaz = caz.reciprocal();
        str[0] = zr.multiply(rcaz);
        sti[0] = (zi.negate()).multiply(rcaz);
        rzr = (str[0].add(str[0])).multiply(rcaz);
        rzi = (sti[0].add(sti[0])).multiply(rcaz);
        inu = (fnu.add(DoubleDouble.valueOf(0.5))).intValue();
        dnu = fnu.subtract(DoubleDouble.valueOf(inu));

        if ((dnu.abs()).ne(DoubleDouble.valueOf(0.5))) {
            dnu2 = DoubleDouble.valueOf(0.0);

            if ((dnu.abs()).gt(tol)) {
                dnu2 = dnu.multiply(dnu);
            } // if (Math.abs(dnu) > tol)

            if (caz.le(r1)) {

                // Series for CABS(z) <= r1
                fc = DoubleDouble.valueOf(1.0);
                zlog(rzr, rzi, smur, smui, idum);
                fmur = smur[0].multiply(dnu);
                fmui = smui[0].multiply(dnu);
                zshch(fmur, fmui, cshr, cshi, cchr, cchi);

                if (dnu.ne(DoubleDouble.valueOf(0.0))) {
                    fc = dnu.multiply(DoubleDouble.PI);
                    fc = fc.divide(fc.sin());
                    smur[0] = cshr[0].divide(dnu);
                    smui[0] = cshi[0].divide(dnu);
                } // if (dnu != 0.0)

                a2 = (DoubleDouble.valueOf(1.0)).add(dnu);

                // gam(1-z)*gam(1+z) = PI*z/sin(PI*z), t1 = 1/gam(1-dnu),
                // t2 = 1/(gam+dnu)
                t2 = (dgamln(a2, idum).negate()).exp();
                t1 = (t2.multiply(fc)).reciprocal();

                if ((dnu.abs()).le(DoubleDouble.valueOf(0.1))) {

                    // Series for F0 to resolve indeterminancy for small ABS(dnu)
                    ak = DoubleDouble.valueOf(1.0);
                    s = (DoubleDouble)cc[0].clone();

                    for (k = 2; k <= 8; k++) {
                        ak = ak.multiply(dnu2);
                        tm = cc[k - 1].multiply(ak);
                        s = s.add(tm);

                        if ((tm.abs()).lt(tol)) {
                            break;
                        } // if (math.abs(tm) < tol)
                    } // for (k = 2; k <= 8; k++)

                    g1 = s.negate();
                } // if (Math.abs(dnu) <= 0.1)
                else {
                    g1 = (t1.subtract(t2)).divide(dnu.add(dnu));
                } // else

                g2 = (DoubleDouble.valueOf(0.5)).multiply(t1.add(t2));
                fr[0] = fc.multiply( (cchr[0].multiply(g1)).add(smur[0].multiply(g2)));
                fi[0] = fc.multiply( (cchi[0].multiply(g1)).add(smui[0].multiply(g2)));
                zexp(fmur, fmui, str, sti);
                pr = ((DoubleDouble.valueOf(0.5)).multiply(str[0])).divide(t2);
                pi = ((DoubleDouble.valueOf(0.5)).multiply(sti[0])).divide(t2);
                zdiv(DoubleDouble.valueOf(0.5), DoubleDouble.valueOf(0.0), str[0], sti[0], ptr, pti);
                qr = ptr[0].divide(t1);
                qi = pti[0].divide(t1);
                s1r[0] = (DoubleDouble)fr[0].clone();
                s1i[0] = (DoubleDouble)fi[0].clone();
                s2r[0] = (DoubleDouble)pr.clone();
                s2i[0] = (DoubleDouble)pi.clone();
                ak = DoubleDouble.valueOf(1.0);
                a1 = DoubleDouble.valueOf(1.0);
                ckr = DoubleDouble.valueOf(1.0);
                cki = DoubleDouble.valueOf(0.0);
                bk = (DoubleDouble.valueOf(1.0)).subtract(dnu2);

                if ( (inu <= 0) && (n <= 1)) {

                    // Generate K(fnu,z), 0.0 <= fnu < 0.5 and n=1
                    if (caz.ge(tol)) {
                        zmlt(zr, zi, zr, zi, czr, czi);
                        czr[0] = (DoubleDouble.valueOf(0.25)).multiply(czr[0]);
                        czi[0] = (DoubleDouble.valueOf(0.25)).multiply(czi[0]);
                        t1 = ((DoubleDouble.valueOf(0.25)).multiply(caz)).multiply(caz);

                        while (true) {
                            fr[0] = ( ((fr[0].multiply(ak)).add(pr)).add(qr)).divide(bk);
                            fi[0] = ( ((fi[0].multiply(ak)).add(pi)).add(qi)).divide(bk);
                            str[0] = (ak.subtract(dnu)).reciprocal();
                            pr = pr.multiply(str[0]);
                            pi = pi.multiply(str[0]);
                            str[0] = (ak.add(dnu)).reciprocal();
                            qr = qr.multiply(str[0]);
                            qi = qi.multiply(str[0]);
                            str[0] = (ckr.multiply(czr[0])).subtract(cki.multiply(czi[0]));
                            rak = ak.reciprocal();
                            cki = ( (ckr.multiply(czi[0])).add(cki.multiply(czr[0]))).multiply(rak);
                            ckr = str[0].multiply(rak);
                            s1r[0] = ((ckr.multiply(fr[0])).subtract(cki.multiply(fi[0]))).add(s1r[0]);
                            s1i[0] = ((ckr.multiply(fi[0])).add(cki.multiply(fr[0]))).add(s1i[0]);
                            a1 = (a1.multiply(t1)).multiply(rak);
                            bk = ((bk.add(ak)).add(ak)).add(DoubleDouble.valueOf(1.0));
                            ak = ak.add(DoubleDouble.valueOf(1.0));

                            if (a1.le(tol)) {
                                break;
                            } // if (a1 <= tol)
                        } // while (true)
                    } // if (caz >= tol)

                    yr[0] = (DoubleDouble)s1r[0].clone();
                    yi[0] = (DoubleDouble)s1i[0].clone();

                    if (koded == 1) {
                        return;
                    } // if (koded == 1)

                    zexp(zr, zi, str, sti);
                    zmlt(s1r[0], s1i[0], str[0], sti[0], yr0, yi0);
                    yr[0] = (DoubleDouble)yr0[0].clone();
                    yi[0] = (DoubleDouble)yi0[0].clone();

                    return;
                } // if ((inu <= 0) && (n <= 1)

                if (caz.ge(tol)) {
                    zmlt(zr, zi, zr, zi, czr, czi);
                    czr[0] = (DoubleDouble.valueOf(0.25)).multiply(czr[0]);
                    czi[0] = (DoubleDouble.valueOf(0.25)).multiply(czi[0]);
                    t1 = ((DoubleDouble.valueOf(0.25)).multiply(caz)).multiply(caz);

                    while (true) {
                        fr[0] = ( ((fr[0].multiply(ak)).add(pr)).add(qr)).divide(bk);
                        fi[0] = ( ((fi[0].multiply(ak)).add(pi)).add(qi)).divide(bk);
                        str[0] = (ak.subtract(dnu)).reciprocal();
                        pr = pr.multiply(str[0]);
                        pi = pi.multiply(str[0]);
                        str[0] = (ak.add(dnu)).reciprocal();
                        qr = qr.multiply(str[0]);
                        qi = qi.multiply(str[0]);
                        str[0] = (ckr.multiply(czr[0])).subtract(cki.multiply(czi[0]));
                        rak = ak.reciprocal();
                        cki = ( (ckr.multiply(czi[0])).add(cki.multiply(czr[0]))).multiply(rak);
                        ckr = str[0].multiply(rak);
                        s1r[0] = ((ckr.multiply(fr[0])).subtract(cki.multiply(fi[0]))).add(s1r[0]);
                        s1i[0] = ((ckr.multiply(fi[0])).add(cki.multiply(fr[0]))).add(s1i[0]);
                        str[0] = pr.subtract(fr[0].multiply(ak));
                        sti[0] = pi.subtract(fi[0].multiply(ak));
                        s2r[0] = ((ckr.multiply(str[0])).subtract(cki.multiply(sti[0]))).add(s2r[0]);
                        s2i[0] = ((ckr.multiply(sti[0])).add(cki.multiply(str[0]))).add(s2i[0]);
                        a1 = (a1.multiply(t1)).multiply(rak);
                        bk = ((bk.add(ak)).add(ak)).add(DoubleDouble.valueOf(1.0));
                        ak = ak.add(DoubleDouble.valueOf(1.0));

                        if (a1.le(tol)) {
                            break;
                        } // if (a1 <= tol)
                    } // while (true)
                } // if (caz >= tol)

                kflag = 2;
                a1 = fnu.add(DoubleDouble.valueOf(1.0));
                ak = a1.multiply(smur[0].abs());

                if (ak.gt(alim)) {
                    kflag = 3;
                } // if (ak > alim)

                str[0] = (DoubleDouble)cssr[kflag - 1].clone();
                p2r = s2r[0].multiply(str[0]);
                p2i = s2i[0].multiply(str[0]);
                zmlt(p2r, p2i, rzr, rzi, s2r, s2i);
                s1r[0] = s1r[0].multiply(str[0]);
                s1i[0] = s1i[0].multiply(str[0]);

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
            zdiv(rthpi, DoubleDouble.valueOf(0.0), str[0], sti[0], coefr, coefi);
            kflag = 2;

            if (koded != 2) {

                if (zr.gt(alim)) {

                    // scale by exp(z), iflag = 1 cases
                    koded = 2;
                    iflag = 1;
                    kflag = 2;
                } // if (zr > alim)
                else {
                    str[0] = ((zr.negate()).exp()).multiply(cssr[kflag - 1]);
                    sti[0] = (str[0].negate()).multiply(zi.sin());
                    str[0] = str[0].multiply(zi.cos());
                    zmlt(coefr[0], coefi[0], str[0], sti[0], coefr, coefi);
                } // else
            } // if (koded != 2)

            if ((dnu.abs()).equals(DoubleDouble.valueOf(0.5))) {
                s1r[0] = (DoubleDouble)coefr[0].clone();
                s1i[0] = (DoubleDouble)coefi[0].clone();
                s2r[0] = (DoubleDouble)coefr[0].clone();
                s2i[0] = (DoubleDouble)coefi[0].clone();
                seg2 = false;
            } // if (Math.abs(dnu) == 0.5)
            else {

                // Miller algorithm for cabs(z) > r1
                ak = ((DoubleDouble.PI).multiply(dnu)).cos();
                ak = (ak.abs());

                if (ak.equals(DoubleDouble.valueOf(0.0))) {
                    s1r[0] = (DoubleDouble)coefr[0].clone();
                    s1i[0] = (DoubleDouble)coefi[0].clone();
                    s2r[0] = (DoubleDouble)coefr[0].clone();
                    s2i[0] = (DoubleDouble)coefi[0].clone();
                    seg2 = false;
                } // if (ak == 0.0)
                else {
                    fhs = ((DoubleDouble.valueOf(0.25)).subtract(dnu2)).abs();

                    if (fhs.equals(DoubleDouble.valueOf(0.0))) {
                        s1r[0] = (DoubleDouble)coefr[0].clone();
                        s1i[0] = (DoubleDouble)coefi[0].clone();
                        s2r[0] = (DoubleDouble)coefr[0].clone();
                        s2i[0] = (DoubleDouble)coefi[0].clone();
                        seg2 = false;
                    } // if (fhs == 0.0)
                } // else
            } // else

            if (seg2) {

                // Compute r2 = F(e). If CABS(Z) >= r2, use forward recurrence to
                // determine the bakcward index k. r2 = F(e) is a straight line on
                // 12 <= e <= 60. e is computed from 2**(-e) = 2**(1 - I1MACH(14)) =
                // tol
                t1 = DoubleDouble.valueOf(DoubleDoubleDigits - 1.0);
                t1 = (t1.multiply(r1m5)).multiply(DoubleDouble.valueOf(3.321928094));
                t1 = t1.max(DoubleDouble.valueOf(12.0));
                t1 = t1.min(DoubleDouble.valueOf(60.0));
                t2 = (tth.multiply(t1)).subtract(DoubleDouble.valueOf(6.0));

                if (zr.equals(DoubleDouble.valueOf(0.0))) {
                    t1 = DoubleDouble.PI_2;
                } // if (zr == 0.0)
                else { // zr != 0.0
                    t1 = (zi.divide(zr)).atan();
                    t1 = t1.abs();
                } // else zr != 0.0

                if (t2.le(caz)) {

                    // Forward recurrence loop when cabs(z) >= r2
                    etest = ak.divide(((DoubleDouble.PI).multiply(caz)).multiply(tol));
                    fk = DoubleDouble.valueOf(1.0);

                    if (etest.lt(DoubleDouble.valueOf(1.0))) {
                        seg3 = false;
                        seg4 = false;
                    }

                    if (seg3) {
                        fks = (DoubleDouble)ctwor.clone();
                        ckr = (caz.add(caz)).add(ctwor);
                        p1r = DoubleDouble.valueOf(0.0);
                        p2r = DoubleDouble.valueOf(1.0);

                        group: {

                            for (i = 1; i <= kmax; i++) {
                                ak = fhs.divide(fks);
                                cbr = ckr.divide(fk.add(DoubleDouble.valueOf(1.0)));
                                ptr[0] = (DoubleDouble)p2r.clone();
                                p2r = (cbr.multiply(p2r)).subtract(p1r.multiply(ak));
                                p1r = (DoubleDouble)ptr[0].clone();
                                ckr = ckr.add(ctwor);
                                fks = ((fks.add(fk)).add(fk)).add(ctwor);
                                fhs = (fhs.add(fk)).add(fk);
                                fk = fk.add(DoubleDouble.valueOf(1.0));
                                str[0] = (p2r.abs()).multiply(fk);

                                if (etest.lt(str[0])) {
                                    break group;
                                }
                            } // for (i = 1; i <= kmax; i++)

                            nz[0] = -2;

                            return;
                        } // group

                        fk = fk.add((spi.multiply(t1)).multiply((t2.divide(caz)).sqrt()));
                        fhs = ((DoubleDouble.valueOf(0.25)).subtract(dnu2)).abs();
                        seg4 = false;
                    } // if (seg3)

                    seg3 = true;
                } // if (t2 <= caz)

                if (seg4) {

                    // Compute backward index k for CABS(z) < r2
                    a2 = caz.sqrt();
                    ak = (fpi.multiply(ak)).divide(tol.multiply(a2.sqrt()));
                    aa = ((DoubleDouble.valueOf(3.0)).multiply(t1)).divide((DoubleDouble.valueOf(1.0)).add(caz));
                    bb = ((DoubleDouble.valueOf(14.7)).multiply(t1)).divide((DoubleDouble.valueOf(28.0)).add(caz));
                    ak = ((ak.log()).add((caz.multiply(aa.cos())).divide((DoubleDouble.valueOf(1.0)).add
                    		((DoubleDouble.valueOf(0.008)).multiply(caz))))).divide(bb.cos());
                    fk = ((((DoubleDouble.valueOf(0.12125)).multiply(ak)).multiply(ak)).divide(caz)).add(DoubleDouble.valueOf(1.5));
                } // if (seg4)

                seg4 = true;

                // Backward recurrence loop for Miller algorithm
                k = fk.intValue();
                fk = DoubleDouble.valueOf(k);
                fks = fk.multiply(fk);
                p1r = DoubleDouble.valueOf(0.0);
                p1i = DoubleDouble.valueOf(0.0);
                p2r = (DoubleDouble)tol.clone();
                p2i = DoubleDouble.valueOf(0.0);
                csr = (DoubleDouble)p2r.clone();
                csi = (DoubleDouble)p2i.clone();

                for (i = 1; i <= k; i++) {
                    a1 = fks.subtract(fk);
                    ak = (fks.add(fk)).divide(a1.add(fhs));
                    rak = (DoubleDouble.valueOf(2.0)).divide(fk.add(DoubleDouble.valueOf(1.0)));
                    cbr = (fk.add(zr)).multiply(rak);
                    cbi = zi.multiply(rak);
                    ptr[0] = (DoubleDouble)p2r.clone();
                    pti[0] = (DoubleDouble)p2i.clone();
                    p2r = ( (ptr[0].multiply(cbr)).subtract(pti[0].multiply(cbi)).subtract(p1r)).multiply(ak);
                    p2i = ( (pti[0].multiply(cbr)).add(ptr[0].multiply(cbi)).subtract(p1i)).multiply(ak);
                    p1r = (DoubleDouble)ptr[0].clone();
                    p1i = (DoubleDouble)pti[0].clone();
                    csr = csr.add(p2r);
                    csi = csi.add(p2i);
                    fks = (a1.subtract(fk)).add(DoubleDouble.valueOf(1.0));
                    fk = fk.subtract(DoubleDouble.valueOf(1.0));
                } // for (i = 1; i <= k; i++)

                // Compute (p2/cs) = (p2/CABS(cs))*(CONJG(cs)/CABS(cs)) for better
                // scaling
                tm = zabs(csr, csi);
                ptr[0] = tm.reciprocal();
                s1r[0] = p2r.multiply(ptr[0]);
                s1i[0] = p2i.multiply(ptr[0]);
                csr = csr.multiply(ptr[0]);
                csi = (csi.negate()).multiply(ptr[0]);
                zmlt(coefr[0], coefi[0], s1r[0], s1i[0], str, sti);
                zmlt(str[0], sti[0], csr, csi, s1r, s1i);

                if ( (inu <= 0) && (n <= 1)) {
                    zdr = (DoubleDouble)zr.clone();
                    zdi = (DoubleDouble)zi.clone();
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
                    ptr[0] = tm.reciprocal();
                    p1r = p1r.multiply(ptr[0]);
                    p1i = p1i.multiply(ptr[0]);
                    p2r = p2r.multiply(ptr[0]);
                    p2i = (p2i.negate()).multiply(ptr[0]);
                    zmlt(p1r, p1i, p2r, p2i, ptr, pti);
                    str[0] = (dnu.add(DoubleDouble.valueOf(0.5))).subtract(ptr[0]);
                    sti[0] = pti[0].negate();
                    zdiv(str[0], sti[0], zr, zi, str, sti);
                    str[0] = str[0].add(DoubleDouble.valueOf(1.0));
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
            str[0] = dnu.add(DoubleDouble.valueOf(1.0));
            ckr = str[0].multiply(rzr);
            cki = str[0].multiply(rzi);

            if (n == 1) {
                inu = inu - 1;
            } // if (n == 1)

            if (inu <= 0) {

                if (n <= 1) {
                    s1r[0] = (DoubleDouble)s2r[0].clone();
                    s1i[0] = (DoubleDouble)s2i[0].clone();
                } // if (n <= 1)

                zdr = (DoubleDouble)zr.clone();
                zdi = (DoubleDouble)zi.clone();
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
                p1r = (DoubleDouble)csrr[kflag - 1].clone();
                ascle = (DoubleDouble)bry[kflag - 1].clone();

                for (i = inub; i <= inu; i++) {
                    str[0] = (DoubleDouble)s2r[0].clone();
                    sti[0] = (DoubleDouble)s2i[0].clone();
                    s2r[0] = ((ckr.multiply(str[0])).subtract(cki.multiply(sti[0]))).add(s1r[0]);
                    s2i[0] = ((ckr.multiply(sti[0])).add(cki.multiply(str[0]))).add(s1i[0]);
                    s1r[0] = (DoubleDouble)str[0].clone();
                    s1i[0] = (DoubleDouble)sti[0].clone();
                    ckr = ckr.add(rzr);
                    cki = cki.add(rzi);

                    if (kflag >= 3) {
                        continue;
                    } // if (kflag >= 3)

                    p2r = s2r[0].multiply(p1r);
                    p2i = s2i[0].multiply(p1r);
                    str[0] = p2r.abs();
                    sti[0] = p2i.abs();
                    p2m = str[0].max(sti[0]);

                    if (p2m.le(ascle)) {
                        continue;
                    } // if (p2m <= ascle)

                    kflag = kflag + 1;
                    ascle = (DoubleDouble)bry[kflag - 1].clone();
                    s1r[0] = s1r[0].multiply(p1r);
                    s1i[0] = s1i[0].multiply(p1r);
                    s2r[0] = (DoubleDouble)p2r.clone();
                    s2i[0] = (DoubleDouble)p2i.clone();
                    str[0] = cssr[kflag - 1];
                    s1r[0] = s1r[0].multiply(str[0]);
                    s1i[0] = s1i[0].multiply(str[0]);
                    s2r[0] = s2r[0].multiply(str[0]);
                    s2i[0] = s2i[0].multiply(str[0]);
                    p1r = (DoubleDouble)csrr[kflag - 1].clone();
                } // for (i = inub; i<= inu; i++)

                if (n == 1) {
                    s1r[0] = (DoubleDouble)s2r[0].clone();
                    s1i[0] = (DoubleDouble)s2i[0].clone();
                } // if (n == 1)
            } // if (seg7)

            seg7 = true;

            if (seg8) {
                str[0] = (DoubleDouble)csrr[kflag - 1].clone();
                yr[0] = s1r[0].multiply(str[0]);
                yi[0] = s1i[0].multiply(str[0]);

                if (n == 1) {
                    return;
                } // if (n == 1)

                yr[1] = s2r[0].multiply(str[0]);
                yi[1] = s2i[0].multiply(str[0]);

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

                p1r = (DoubleDouble)csrr[kflag - 1].clone();
                ascle = (DoubleDouble)bry[kflag - 1].clone();

                for (i = kk; i <= n; i++) {
                    p2r = (DoubleDouble)s2r[0].clone();
                    p2i = (DoubleDouble)s2i[0].clone();
                    s2r[0] = ((ckr.multiply(p2r)).subtract(cki.multiply(p2i))).add(s1r[0]);
                    s2i[0] = ((cki.multiply(p2r)).add(ckr.multiply(p2i))).add(s1i[0]);
                    s1r[0] = (DoubleDouble)p2r.clone();
                    s1i[0] = (DoubleDouble)p2i.clone();
                    ckr = ckr.add(rzr);
                    cki = cki.add(rzi);
                    p2r = s2r[0].multiply(p1r);
                    p2i = s2i[0].multiply(p1r);
                    yr[i - 1] = (DoubleDouble)p2r.clone();
                    yi[i - 1] = (DoubleDouble)p2i.clone();

                    if (kflag >= 3) {
                        continue;
                    } // if (kflag >= 3)

                    str[0] = p2r.abs();
                    sti[0] = p2i.abs();
                    p2m = str[0].max(sti[0]);

                    if (p2m.le(ascle)) {
                        continue;
                    } // if (p2m <= ascle)

                    kflag = kflag + 1;
                    ascle = (DoubleDouble)bry[kflag - 1].clone();
                    s1r[0] = s1r[0].multiply(p1r);
                    s1i[0] = s1i[0].multiply(p1r);
                    s2r[0] = (DoubleDouble)p2r.clone();
                    s2i[0] = (DoubleDouble)p2i.clone();
                    str[0] = (DoubleDouble)cssr[kflag - 1].clone();
                    s1r[0] = s1r[0].multiply(str[0]);
                    s1i[0] = s1i[0].multiply(str[0]);
                    s2r[0] = s2r[0].multiply(str[0]);
                    s2i[0] = s2i[0].multiply(str[0]);
                    p1r = (DoubleDouble)csrr[kflag - 1].clone();
                } // for (i = kk; i <= n; i++)

                return;
            } // if (seg9)

            seg9 = true;

            // iflag = 1 cases, forward recurrence on scaled values on underflow
            if (seg10) {
                helim = (DoubleDouble.valueOf(0.5)).multiply(elim);
                elm = (elim.negate()).exp();
                celmr = (DoubleDouble)elm.clone();
                ascle = (DoubleDouble)bry[0].clone();
                zdr = (DoubleDouble)zr.clone();
                zdi = (DoubleDouble)zi.clone();
                ic = -1;
                j = 2;

                group2: {

                    for (i = 1; i <= inu; i++) {
                        str[0] = (DoubleDouble)s2r[0].clone();
                        sti[0] = (DoubleDouble)s2i[0].clone();
                        s2r[0] = ((str[0].multiply(ckr)).subtract(sti[0].multiply(cki))).add(s1r[0]);
                        s2i[0] = ((sti[0].multiply(ckr)).add(str[0].multiply(cki))).add(s1i[0]);
                        s1r[0] = (DoubleDouble)str[0].clone();
                        s1i[0] = (DoubleDouble)sti[0].clone();
                        ckr = ckr.add(rzr);
                        cki = cki.add(rzi);
                        as = zabs(s2r[0], s2i[0]);
                        alas = as.log();
                        p2r = (zdr.negate()).add(alas);

                        if (p2r.ge(elim.negate())) {
                            zlog(s2r[0], s2i[0], str, sti, idum);
                            p2r = (zdr.negate()).add(str[0]);
                            p2i = (zdi.negate()).add(sti[0]);
                            p2m = (p2r.exp()).divide(tol);
                            p1r = p2m.multiply(p2i.cos());
                            p1i = p2m.multiply(p2i.sin());
                            zuchk(p1r, p1i, nw, ascle);

                            if (nw[0] == 0) {
                                j = 3 - j;
                                cyr[j - 1] = (DoubleDouble)p1r.clone();
                                cyi[j - 1] = (DoubleDouble)p1i.clone();

                                if (ic == (i - 1)) {
                                    break group2;
                                } // if (ic == (i-1))

                                ic = i;

                                continue;
                            } // if (nw[0] == 0)
                        } // if (p2r >= (-elim))

                        if (alas.lt(helim)) {
                            continue;
                        } // if (alas < helim)

                        zdr = zdr.subtract(elim);
                        s1r[0] = s1r[0].multiply(celmr);
                        s1i[0] = s1i[0].multiply(celmr);
                        s2r[0] = s2r[0].multiply(celmr);
                        s2i[0] = s2i[0].multiply(celmr);
                    } // for (i = 1; i <= inu; i++)

                    if (n == 1) {
                        s1r[0] = (DoubleDouble)s2r[0].clone();
                        s1i[0] = (DoubleDouble)s2i[0].clone();
                    } // if (n == 1)

                    seg11 = false;
                } // group2

                if (seg11) {
                    kflag = 1;
                    inub = i + 1;
                    s2r[0] = (DoubleDouble)cyr[j - 1].clone();
                    s2i[0] = (DoubleDouble)cyi[j - 1].clone();
                    j = 3 - j;
                    s1r[0] = (DoubleDouble)cyr[j - 1].clone();
                    s1i[0] = (DoubleDouble)cyi[j - 1].clone();

                    if (inub <= inu) {
                        continue;
                    } // if (inub <= inu)

                    if (n != 1) {
                        seg7 = false;

                        continue;
                    } // if (n != 1)

                    s1r[0] = (DoubleDouble)s2r[0].clone();
                    s1i[0] = (DoubleDouble)s2i[0].clone();
                    seg7 = false;

                    continue;
                } // if (seg11)

                seg11 = true;
            } // if (seg10)

            seg10 = true;
            yr[0] = (DoubleDouble)s1r[0].clone();
            yi[0] = (DoubleDouble)s1i[0].clone();

            if (n != 1) {
                yr[1] = (DoubleDouble)s2r[0].clone();
                yi[1] = (DoubleDouble)s2i[0].clone();
            } // if (n != 1)

            ascle = (DoubleDouble)bry[0].clone();
            zkscl(zdr, zdi, fnu, n, yr, yi, nz, rzr, rzi, ascle);
            inu = n - nz[0];

            if (inu <= 0) {
                return;
            } // if (inu <= 0)

            kk = nz[0] + 1;
            s1r[0] = (DoubleDouble)yr[kk - 1].clone();
            s1i[0] = (DoubleDouble)yi[kk - 1].clone();
            yr[kk - 1] = s1r[0].multiply(csrr[0]);
            yi[kk - 1] = s1i[0].multiply(csrr[0]);

            if (inu == 1) {
                return;
            } // if (inu == 1)

            kk = nz[0] + 2;
            s2r[0] = (DoubleDouble)yr[kk - 1].clone();
            s2i[0] = (DoubleDouble)yi[kk - 1].clone();
            yr[kk - 1] = s2r[0].multiply(csrr[0]);
            yi[kk - 1] = s2i[0].multiply(csrr[0]);

            if (inu == 2) {
                return;
            } // if (inu == 2)

            t2 = fnu.add(DoubleDouble.valueOf(kk - 1.0));
            ckr = t2.multiply(rzr);
            cki = t2.multiply(rzi);
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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     * @param nui int
     * @param nlast int[]
     * @param fnul DoubleDouble
     */
    private void zbuni(final DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n, final DoubleDouble[] yr,
            final DoubleDouble[] yi, final int[] nz, final int nui, final int[] nlast, final DoubleDouble fnul) {
        DoubleDouble ax;
        DoubleDouble ay;
        int iform;
        DoubleDouble fnui;
        DoubleDouble dfnu;
        final DoubleDouble[] cyr = new DoubleDouble[2];
        final DoubleDouble[] cyi = new DoubleDouble[2];
        final DoubleDouble[] bry = new DoubleDouble[3];
        DoubleDouble gnu;
        final int[] nw = new int[1];
        DoubleDouble str;
        int iflag;
        DoubleDouble ascle;
        DoubleDouble csclr;
        DoubleDouble cscrr;
        DoubleDouble s1r;
        DoubleDouble s1i;
        DoubleDouble s2r;
        DoubleDouble s2i;
        DoubleDouble raz;
        DoubleDouble sti;
        DoubleDouble rzr;
        DoubleDouble rzi;
        DoubleDouble c1r;
        DoubleDouble c1i;
        DoubleDouble c1m;
        int nl;
        int k;
        int i;

        nz[0] = 0;
        ax = (DoubleDouble.valueOf(1.7321)).multiply(zr.abs());
        ay = zi.abs();
        iform = 1;

        if (ay.gt(ax)) {
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

        fnui = DoubleDouble.valueOf(nui);
        dfnu = fnu.add(DoubleDouble.valueOf(n - 1.0));
        gnu = dfnu.add(fnui);

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
        bry[0] = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).divide(tol);
        bry[1] = bry[0].reciprocal();
        bry[2] = (DoubleDouble)bry[1].clone();
        iflag = 2;
        ascle = (DoubleDouble)bry[1].clone();
        csclr = DoubleDouble.valueOf(1.0);

        if (str.le(bry[0])) {
            iflag = 1;
            ascle = (DoubleDouble)bry[0].clone();
            csclr = tol.reciprocal();
        } // if (str <= bry[0])
        else if (str.ge(bry[1])) {
            iflag = 3;
            ascle = (DoubleDouble)bry[2].clone();
            csclr = (DoubleDouble)tol.clone();
        } // else if (str >= bry[1])

        cscrr = csclr.reciprocal();
        s1r = cyr[1].multiply(csclr);
        s1i = cyi[1].multiply(csclr);
        s2r = cyr[0].multiply(csclr);
        s2i = cyi[0].multiply(csclr);
        raz = (zabs(zr, zi)).reciprocal();
        str = zr.multiply(raz);
        sti = (zi.negate()).multiply(raz);
        rzr = (str.add(str)).multiply(raz);
        rzi = (sti.add(sti)).multiply(raz);

        for (i = 1; i <= nui; i++) {
            str = (DoubleDouble)s2r.clone();
            sti = (DoubleDouble)s2i.clone();
            s2r = ( (dfnu.add(fnui)).multiply( (rzr.multiply(str)).subtract(rzi.multiply(sti)))).add(s1r);
            s2i = ( (dfnu.add(fnui)).multiply( (rzr.multiply(sti)).add(rzi.multiply(str)))).add(s1i);
            s1r = (DoubleDouble)str.clone();
            s1i = (DoubleDouble)sti.clone();
            fnui = fnui.subtract(DoubleDouble.valueOf(1.0));

            if (iflag >= 3) {
                continue;
            } // if (iflag >= 3)

            str = s2r.multiply(cscrr);
            sti = s2i.multiply(cscrr);
            c1r = str.abs();
            c1i = sti.abs();
            c1m = c1r.max(c1i);

            if (c1m.le(ascle)) {
                continue;
            } // if (c1m <= ascle)

            iflag = iflag + 1;
            ascle = (DoubleDouble)bry[iflag - 1].clone();
            s1r = s1r.multiply(cscrr);
            s1i = s1i.multiply(cscrr);
            s2r = (DoubleDouble)str.clone();
            s2i = (DoubleDouble)sti.clone();
            csclr = csclr.multiply(tol);
            cscrr = csclr.reciprocal();
            s1r = s1r.multiply(csclr);
            s1i = s1i.multiply(csclr);
            s2r = s2r.multiply(csclr);
            s2i = s2i.multiply(csclr);
        } // for (i = 1; i <= nui; i++)

        yr[n - 1] = s2r.multiply(cscrr);
        yi[n - 1] = s2i.multiply(cscrr);

        if (n == 1) {
            return;
        } // if (n == 1)

        nl = n - 1;
        fnui = DoubleDouble.valueOf(nl);
        k = nl;

        for (i = 1; i <= nl; i++) {
            str = (DoubleDouble)s2r.clone();
            sti = (DoubleDouble)s2i.clone();
            s2r = ( (fnu.add(fnui)).multiply( (rzr.multiply(str)).subtract(rzi.multiply(sti)))).add(s1r);
            s2i = ( (fnu.add(fnui)).multiply( (rzr.multiply(sti)).add(rzi.multiply(str)))).add(s1i);
            s1r = (DoubleDouble)str.clone();
            s1i = (DoubleDouble)sti.clone();
            str = s2r.multiply(cscrr);
            sti = s2i.multiply(cscrr);
            yr[k - 1] = (DoubleDouble)str.clone();
            yi[k - 1] = (DoubleDouble)sti.clone();
            fnui = fnui.subtract(DoubleDouble.valueOf(1.0));
            k = k - 1;

            if (iflag >= 3) {
                continue;
            } // if (iflag >= 3)

            c1r = str.abs();
            c1i = sti.abs();
            c1m = c1r.max(c1i);

            if (c1m.le(ascle)) {
                continue;
            } // if (c1m <= ascle)

            iflag = iflag + 1;
            ascle = (DoubleDouble)bry[iflag - 1].clone();
            s1r = s1r.multiply(cscrr);
            s1i = s1i.multiply(cscrr);
            s2r = (DoubleDouble)str.clone();
            s2i = (DoubleDouble)sti.clone();
            csclr = csclr.multiply(tol);
            cscrr = csclr.reciprocal();
            s1r = s1r.multiply(csclr);
            s1i = s1i.multiply(csclr);
            s2r = s2r.multiply(csclr);
            s2i = s2i.multiply(csclr);
        } // for (i = 1; i <= nl i++)

        return;
    }

    /**
     * ZBUNK COMPUTES THE K BESSEL FUNCTION FOR FNU.GT.FNUL. ACCORDING TO THE UNIFORM ASYMPTOTIC EXPANSION FOR K(FNU,Z)
     * IN ZUNK1 AND THE EXPANSION FOR H(2,FNU,Z) IN ZUNK2
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param mr int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     */
    private void zbunk(final DoubleDouble zr, final DoubleDouble zi, final DoubleDouble fnu, final int kode, final int mr, final int n,
            final DoubleDouble[] yr, final DoubleDouble[] yi, final int[] nz) {
        DoubleDouble ax;
        DoubleDouble ay;

        nz[0] = 0;
        ax = (DoubleDouble.valueOf(1.7321)).multiply(zr.abs());
        ay = zi.abs();

        if (ay.le(ax)) {

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
     * @param ar DoubleDouble
     * @param ai DoubleDouble
     * @param br DoubleDouble
     * @param bi DoubleDouble
     * @param cr DoubleDouble[]
     * @param ci DoubleDouble[]
     */
    private void zdiv(final DoubleDouble ar, final DoubleDouble ai, final DoubleDouble br, final DoubleDouble bi, final DoubleDouble[] cr,
            final DoubleDouble[] ci) {
        DoubleDouble bm, cc, cd, ca, cb;

        bm = (zabs(br, bi)).reciprocal();
        cc = br.multiply(bm);
        cd = bi.multiply(bm);
        ca = ( (ar.multiply(cc)).add(ai.multiply(cd))).multiply(bm);
        cb = ( (ai.multiply(cc)).subtract(ar.multiply(cd))).multiply(bm);
        cr[0] = (DoubleDouble)ca.clone();
        ci[0] = (DoubleDouble)cb.clone();

        return;
    }

    /**
     * complex exponential function b = exp(a).
     * 
     * @param ar DoubleDouble
     * @param ai DoubleDouble
     * @param br DoubleDouble[]
     * @param bi DoubleDouble[]
     */
    private void zexp(final DoubleDouble ar, final DoubleDouble ai, final DoubleDouble[] br, final DoubleDouble[] bi) {
        DoubleDouble zm, ca, cb;
        zm = ar.exp();
        ca = zm.multiply(ai.cos());
        cb = zm.multiply(ai.sin());
        br[0] = (DoubleDouble)ca.clone();
        bi[0] = (DoubleDouble)cb.clone();

        return;
    }

    /**
     * Set K functions to zero on underflow, continue recurrence on scaled functions until two members come on scale,
     * then return with min(zn[0]+2,n) values scaled by 1/tol.
     * 
     * @param zrr DoubleDouble
     * @param zri DoubleDouble
     * @param fnu DoubleDouble
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     * @param rzr DoubleDouble
     * @param rzi DoubleDouble
     * @param ascle DoubleDouble
     */
    private void zkscl(DoubleDouble zrr, final DoubleDouble zri, final DoubleDouble fnu, final int n, final DoubleDouble[] yr,
            final DoubleDouble[] yi, final int[] nz, final DoubleDouble rzr, final DoubleDouble rzi, final DoubleDouble ascle) {
        int ic;
        int nn;
        int i;
        DoubleDouble s1r;
        DoubleDouble s1i;
        final DoubleDouble[] cyr = new DoubleDouble[2];
        final DoubleDouble[] cyi = new DoubleDouble[2];
        DoubleDouble as;
        DoubleDouble acs;
        final DoubleDouble[] csr = new DoubleDouble[1];
        final DoubleDouble[] csi = new DoubleDouble[1];
        final int[] idum = new int[1];
        DoubleDouble str;
        DoubleDouble fn;
        DoubleDouble ckr;
        DoubleDouble cki;
        DoubleDouble s2r;
        DoubleDouble s2i;
        DoubleDouble helim;
        DoubleDouble elm;
        DoubleDouble celmr;
        DoubleDouble zdr;
        DoubleDouble zdi;
        int kk;
        DoubleDouble alas;
        final int[] nw = new int[1];
        int i2;

        nz[0] = 0;
        ic = 0;
        nn = Math.min(2, n);

        for (i = 1; i <= nn; i++) {
            s1r = (DoubleDouble)yr[i - 1].clone();
            s1i = (DoubleDouble)yi[i - 1].clone();
            cyr[i - 1] = (DoubleDouble)s1r.clone();
            cyi[i - 1] = (DoubleDouble)s1i.clone();
            as = zabs(s1r, s1i);
            acs = (zrr.negate()).add(as.log());
            nz[0] = nz[0] + 1;
            yr[i - 1] = DoubleDouble.valueOf(0.0);
            yi[i - 1] = DoubleDouble.valueOf(0.0);

            if (acs.lt(elim.negate())) {
                continue;
            } // if (acs < (-elim))

            zlog(s1r, s1i, csr, csi, idum);
            csr[0] = csr[0].subtract(zrr);
            csi[0] = csi[0].subtract(zri);
            str = (csr[0].exp()).divide(tol);
            csr[0] = str.multiply(csi[0].cos());
            csi[0] = str.multiply(csi[0].sin());
            zuchk(csr[0], csi[0], nw, ascle);

            if (nw[0] != 0) {
                continue;
            } // if (nw[0] != 0)

            yr[i - 1] = (DoubleDouble)csr[0].clone();
            yi[i - 1] = (DoubleDouble)csi[0].clone();
            ic = i;
            nz[0] = nz[0] - 1;
        } // for (i = 1; i <= nn; i++)

        if (n == 1) {
            return;
        } // if (n == 1)

        if (ic <= 1) {
            yr[0] = DoubleDouble.valueOf(0.0);
            yi[0] = DoubleDouble.valueOf(0.0);
            nz[0] = 2;
        } // if (ic <= 1)

        if ( (n == 2) || (nz[0] == 0)) {
            return;
        } // if ((n == 2) || (nz[0] == 0))

        fn = fnu.add(DoubleDouble.valueOf(1.0));
        ckr = fn.multiply(rzr);
        cki = fn.multiply(rzi);
        s1r = (DoubleDouble)cyr[0].clone();
        s1i = (DoubleDouble)cyi[0].clone();
        s2r = (DoubleDouble)cyr[1].clone();
        s2i = (DoubleDouble)cyi[1].clone();
        helim = (DoubleDouble.valueOf(0.5)).multiply(elim);
        elm = (elim.negate()).exp();
        celmr = (DoubleDouble)elm.clone();
        zdr = (DoubleDouble)zrr.clone();
        zdi = (DoubleDouble)zri.clone();

        // Find two consecutive Y values on scale. Scale recurrence if s2
        // gets larger than exp(elim/2)
        for (i = 3; i <= n; i++) {
            kk = i;
            csr[0] = (DoubleDouble)s2r.clone();
            csi[0] = (DoubleDouble)s2i.clone();
            s2r = ((ckr.multiply(csr[0])).subtract(cki.multiply(csi[0]))).add(s1r);
            s2i = ((cki.multiply(csr[0])).add(ckr.multiply(csi[0]))).add(s1i);
            s1r = (DoubleDouble)csr[0].clone();
            s1i = (DoubleDouble)csi[0].clone();
            ckr = ckr.add(rzr);
            cki = cki.add(rzi);
            as = zabs(s2r, s2i);
            alas = as.log();
            acs = (zdr.negate()).add(alas);
            nz[0] = nz[0] + 1;
            yr[i - 1] = DoubleDouble.valueOf(0.0);
            yi[i - 1] = DoubleDouble.valueOf(0.0);

            if (acs.ge(elim.negate())) {
                zlog(s2r, s2i, csr, csi, idum);
                csr[0] = csr[0].subtract(zdr);
                csi[0] = csi[0].subtract(zdi);
                str = (csr[0].exp()).divide(tol);
                csr[0] = str.multiply(csi[0].cos());
                csi[0] = str.multiply(csi[0].sin());
                zuchk(csr[0], csi[0], nw, ascle);

                if (nw[0] == 0) {
                    yr[i - 1] = (DoubleDouble)csr[0].clone();
                    yi[i - 1] = (DoubleDouble)csi[0].clone();
                    nz[0] = nz[0] - 1;

                    if (ic == (kk - 1)) {
                        nz[0] = kk - 2;

                        for (i2 = 1; i2 <= nz[0]; i2++) {
                            yr[i2 - 1] = DoubleDouble.valueOf(0.0);
                            yi[i2 - 1] = DoubleDouble.valueOf(0.0);
                        } // for (i2 = 1; i2 <= nz[0]; i2++)

                        return;
                    } // if (ic == (kk-1)

                    ic = kk;

                    continue;
                } // if (nw[0] == 0)
            } // if (acs >= (-elim))

            if (alas.lt(helim)) {
                continue;
            } // if (alas < helim)

            zdr = zdr.subtract(elim);
            s1r = s1r.multiply(celmr);
            s1i = s1i.multiply(celmr);
            s2r = s2r.multiply(celmr);
            s2i = s2i.multiply(celmr);
        } // for (i = 3; i <= n; i++)

        nz[0] = n;

        if (ic == n) {
            nz[0] = n - 1;
        } // if (ic == n)

        for (i = 1; i <= nz[0]; i++) {
            yr[i - 1] = DoubleDouble.valueOf(0.0);
            yi[i - 1] = DoubleDouble.valueOf(0.0);
        } // for (i = 1; i <= nz[0]; i++)

        return;
    }

    /**
     * complex logarithm b = clog(a).
     * 
     * @param ar DoubleDouble
     * @param ai DoubleDouble
     * @param br DoubleDouble[]
     * @param bi DoubleDouble[]
     * @param ierr int[] ierr = 0, normal return ierr = 1, z = cmplx(0.0, 0.0)
     */
    private void zlog(final DoubleDouble ar, final DoubleDouble ai, final DoubleDouble[] br, final DoubleDouble[] bi, final int[] ierr) {
        DoubleDouble theta;
        DoubleDouble zm;
        ierr[0] = 0;

        if (ar.equals(DoubleDouble.valueOf(0.0))) {

            if (ai.equals(DoubleDouble.valueOf(0.0))) {
                ierr[0] = 1;

                return;
            } // if (ai == 0.0)
            else {

                if (ai.gt(DoubleDouble.valueOf(0.0))) {
                    bi[0] = DoubleDouble.PI_2;
                } else {
                    bi[0] = (DoubleDouble.PI_2).negate();
                }

                br[0] = (ai.abs()).log();

                return;
            }
        } // if (ar == 0.0)
        else if (ai.equals(DoubleDouble.valueOf(0.0))) {

            if (ar.gt(DoubleDouble.valueOf(0.0))) {
                br[0] = ar.log();
                bi[0] = DoubleDouble.valueOf(0.0);

                return;
            } else {
                br[0] = (ar.abs()).log();
                bi[0] = DoubleDouble.PI;

                return;
            }
        } // else if (ai == 0.0)

        theta = (ai.divide(ar)).atan();

        if ( (theta.le(DoubleDouble.valueOf(0.0))) && (ar.lt(DoubleDouble.valueOf(0.0)))) {
            theta = theta.add(DoubleDouble.PI);
        } else if (ar.lt(DoubleDouble.valueOf(0.0))) {
            theta = theta.subtract(DoubleDouble.PI);
        }

        zm = zabs(ar, ai);
        br[0] = zm.log();
        bi[0] = (DoubleDouble)theta.clone();

        return;
    }

    /**
     * zmlri computes the I Bessel function for the Real(z) >= 0.0 by the Miller Algorithm normalized by a Neumann
     * series.
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     */
    private void zmlri(final DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n, final DoubleDouble[] yr,
            final DoubleDouble[] yi, final int[] nz) {
        DoubleDouble scle;
        DoubleDouble az;
        int iaz;
        int ifnu;
        int inu;
        DoubleDouble at;
        DoubleDouble raz;
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        DoubleDouble ckr;
        DoubleDouble cki;
        DoubleDouble rzr;
        DoubleDouble rzi;
        DoubleDouble p1r;
        DoubleDouble p1i;
        DoubleDouble p2r;
        DoubleDouble p2i;
        DoubleDouble ack;
        DoubleDouble rho;
        DoubleDouble rho2;
        DoubleDouble tst;
        DoubleDouble ak;
        int i;
        DoubleDouble ptr;
        DoubleDouble pti;
        DoubleDouble ap;
        int k;
        int itime;
        DoubleDouble flam;
        DoubleDouble fkap;
        int kk;
        DoubleDouble fkk;
        DoubleDouble fnf;
        DoubleDouble tfnf;
        DoubleDouble bk;
        final int[] idum = new int[1];
        DoubleDouble sumr;
        DoubleDouble sumi;
        int km;
        int m;
        final DoubleDouble[] cnormr = new DoubleDouble[1];
        final DoubleDouble[] cnormi = new DoubleDouble[1];

        scle = tiny.divide(tol);
        nz[0] = 0;
        az = zabs(zr, zi);
        iaz = az.intValue();
        ifnu = fnu.intValue();
        inu = ifnu + n - 1;
        at = DoubleDouble.valueOf(iaz + 1.0);
        raz = az.reciprocal();
        str[0] = zr.multiply(raz);
        sti[0] = (zi.negate()).multiply(raz);
        ckr = (str[0].multiply(at)).multiply(raz);
        cki = (sti[0].multiply(at)).multiply(raz);
        rzr = (str[0].add(str[0])).multiply(raz);
        rzi = (sti[0].add(sti[0])).multiply(raz);
        p1r = DoubleDouble.valueOf(0.0);
        p1i = DoubleDouble.valueOf(0.0);
        p2r = DoubleDouble.valueOf(1.0);
        p2i = DoubleDouble.valueOf(0.0);
        ack = (at.add(DoubleDouble.valueOf(1.0))).multiply(raz);
        rho = ack.add(( (ack.multiply(ack)).subtract(DoubleDouble.valueOf(1.0))).sqrt());
        rho2 = rho.multiply(rho);
        tst = (rho2.add(rho2)).divide( (rho2.subtract(DoubleDouble.valueOf(1.0))).multiply(rho.subtract(DoubleDouble.valueOf(1.0))));
        tst = tst.divide(tol);

        // Compute relative truncation error index for series
        ak = (DoubleDouble)at.clone();

        group: {

            for (i = 1; i <= 80; i++) {
                ptr = (DoubleDouble)p2r.clone();
                pti = (DoubleDouble)p2i.clone();
                p2r = p1r.subtract( (ckr.multiply(ptr)).subtract(cki.multiply(pti)));
                p2i = p1i.subtract( (cki.multiply(ptr)).add(ckr.multiply(pti)));
                p1r = (DoubleDouble)ptr.clone();
                p1i = (DoubleDouble)pti.clone();
                ckr = ckr.add(rzr);
                cki = cki.add(rzi);
                ap = zabs(p2r, p2i);

                if (ap.gt((tst.multiply(ak)).multiply(ak))) {
                    break group;
                } // if (ap > (tst*ak*ak))

                ak = ak.add(DoubleDouble.valueOf(1.0));
            } // for (i = 1; i <= 80; i++)

            nz[0] = -2;

            return;
        } // group

        i = i + 1;
        k = 0;

        if (inu >= iaz) {

            // Compute relative truncation error for ratios
            p1r = DoubleDouble.valueOf(0.0);
            p1i = DoubleDouble.valueOf(0.0);
            p2r = DoubleDouble.valueOf(1.0);
            p2i = DoubleDouble.valueOf(0.0);
            at = DoubleDouble.valueOf(inu + 1.0);
            str[0] = zr.multiply(raz);
            sti[0] = (zi.negate()).multiply(raz);
            ckr = (str[0].multiply(at)).multiply(raz);
            cki = (sti[0].multiply(at)).multiply(raz);
            ack = at.multiply(raz);
            tst = (ack.divide(tol)).sqrt();
            itime = 1;

            group2: {

                for (k = 1; k <= 80; k++) {
                    ptr = (DoubleDouble)p2r.clone();
                    pti = (DoubleDouble)p2i.clone();
                    p2r = p1r.subtract( (ckr.multiply(ptr)).subtract(cki.multiply(pti)));
                    p2i = p1i.subtract( (ckr.multiply(pti)).add(cki.multiply(ptr)));
                    p1r = (DoubleDouble)ptr.clone();
                    p1i = (DoubleDouble)pti.clone();
                    ckr = ckr.add(rzr);
                    cki = cki.add(rzi);
                    ap = zabs(p2r, p2i);

                    if (ap.lt(tst)) {
                        continue;
                    } // if (ap < tst)

                    if (itime == 2) {
                        break group2;
                    } // if (itime == 2)

                    ack = zabs(ckr, cki);
                    flam = ack.add(( (ack.multiply(ack)).subtract(DoubleDouble.valueOf(1.0))).sqrt());
                    fkap = ap.divide(zabs(p1r, p1i));
                    rho = flam.min(fkap);
                    tst = tst.multiply((rho.divide( (rho.multiply(rho)).subtract(DoubleDouble.valueOf(1.0)))).sqrt());
                    itime = 2;
                } // for (k = 1; k <= 80; k++)

                nz[0] = -2;

                return;
            } // group2
        } // if (inu >= iaz)

        // Backward recurrence and sum normalizing relation
        k = k + 1;
        kk = Math.max(i + iaz, k + inu);
        fkk = DoubleDouble.valueOf(kk);
        p1r = DoubleDouble.valueOf(0.0);
        p1i = DoubleDouble.valueOf(0.0);

        // Scale p2 and sum by scle
        p2r = (DoubleDouble)scle.clone();
        p2i = DoubleDouble.valueOf(0.0);
        fnf = fnu.subtract(DoubleDouble.valueOf(ifnu));
        tfnf = fnf.add(fnf);
        bk = dgamln((fkk.add(tfnf)).add(DoubleDouble.valueOf(1.0)), idum).subtract(dgamln(fkk.add(DoubleDouble.valueOf(1.0)), idum)).
        		subtract(dgamln(tfnf.add(DoubleDouble.valueOf(1.0)), idum));
        bk = bk.exp();
        sumr = DoubleDouble.valueOf(0.0);
        sumi = DoubleDouble.valueOf(0.0);
        km = kk - inu;

        for (i = 1; i <= km; i++) {
            ptr = (DoubleDouble)p2r.clone();
            pti = (DoubleDouble)p2i.clone();
            p2r = p1r.add( (fkk.add(fnf)).multiply( (rzr.multiply(ptr)).subtract(rzi.multiply(pti))));
            p2i = p1i.add( (fkk.add(fnf)).multiply( (rzi.multiply(ptr)).add(rzr.multiply(pti))));
            p1r = (DoubleDouble)ptr.clone();
            p1i = (DoubleDouble)pti.clone();
            ak = (DoubleDouble.valueOf(1.0)).subtract(tfnf.divide(fkk.add(tfnf)));
            ack = bk.multiply(ak);
            sumr = sumr.add( (ack.add(bk)).multiply(p1r));
            sumi = sumi.add( (ack.add(bk)).multiply(p1i));
            bk = (DoubleDouble)ack.clone();
            fkk = fkk.subtract(DoubleDouble.valueOf(1.0));
        } // for (i = 1; i <= km; i++)

        yr[n - 1] = (DoubleDouble)p2r.clone();
        yi[n - 1] = (DoubleDouble)p2i.clone();

        if (n != 1) {

            for (i = 2; i <= n; i++) {
                ptr = (DoubleDouble)p2r.clone();
                pti = (DoubleDouble)p2i.clone();
                p2r = p1r.add( (fkk.add(fnf)).multiply( (rzr.multiply(ptr)).subtract(rzi.multiply(pti))));
                p2i = p1i.add( (fkk.add(fnf)).multiply( (rzi.multiply(ptr)).add(rzr.multiply(pti))));
                p1r = (DoubleDouble)ptr.clone();
                p1i = (DoubleDouble)pti.clone();
                ak = (DoubleDouble.valueOf(1.0)).subtract(tfnf.divide(fkk.add(tfnf)));
                ack = bk.multiply(ak);
                sumr = sumr.add( (ack.add(bk)).multiply(p1r));
                sumi = sumi.add( (ack.add(bk)).multiply(p1i));
                bk = (DoubleDouble)ack.clone();
                fkk = fkk.subtract(DoubleDouble.valueOf(1.0));
                m = n - i + 1;
                yr[m - 1] = (DoubleDouble)p2r.clone();
                yi[m - 1] = (DoubleDouble)p2i.clone();
            } // for (i = 2; i <= n; i++)
        } // if (n != 1)

        if (ifnu > 0) {

            for (i = 1; i <= ifnu; i++) {
                ptr = (DoubleDouble)p2r.clone();
                pti = (DoubleDouble)p2i.clone();
                p2r = p1r.add( (fkk.add(fnf)).multiply( (rzr.multiply(ptr)).subtract(rzi.multiply(pti))));
                p2i = p1i.add( (fkk.add(fnf)).multiply( (rzr.multiply(pti)).add(rzi.multiply(ptr))));
                p1r = (DoubleDouble)ptr.clone();
                p1i = (DoubleDouble)pti.clone();
                ak = (DoubleDouble.valueOf(1.0)).subtract(tfnf.divide(fkk.add(tfnf)));
                ack = bk.multiply(ak);
                sumr = sumr.add( (ack.add(bk)).multiply(p1r));
                sumi = sumi.add( (ack.add(bk)).multiply(p1i));
                bk = (DoubleDouble)ack.clone();
                fkk = fkk.subtract(DoubleDouble.valueOf(1.0));
            } // for (i = 1; i <= ifnu; i++)
        } // if (ifnu > 0)

        ptr = (DoubleDouble)zr.clone();
        pti = (DoubleDouble)zi.clone();

        if (kode == 2) {
            ptr = DoubleDouble.valueOf(0.0);
        } // if (kode == 2)

        zlog(rzr, rzi, str, sti, idum);
        p1r = ( (fnf.negate()).multiply(str[0])).add(ptr);
        p1i = ( (fnf.negate()).multiply(sti[0])).add(pti);
        ap = dgamln(fnf.add(DoubleDouble.valueOf(1.0)), idum);
        ptr = p1r.subtract(ap);
        pti = (DoubleDouble)p1i.clone();

        // The division cexp(pt)/(sum+p2) is altered to avoid overflow
        // in the denominator by squaring large quantities
        p2r = p2r.add(sumr);
        p2i = p2i.add(sumi);
        ap = zabs(p2r, p2i);
        p1r = ap.reciprocal();
        zexp(ptr, pti, str, sti);
        ckr = str[0].multiply(p1r);
        cki = sti[0].multiply(p1r);
        ptr = p2r.multiply(p1r);
        pti = (p2i.negate()).multiply(p1r);
        zmlt(ckr, cki, ptr, pti, cnormr, cnormi);

        for (i = 1; i <= n; i++) {
            str[0] = (yr[i - 1].multiply(cnormr[0])).subtract(yi[i - 1].multiply(cnormi[0]));
            yi[i - 1] = (yr[i - 1].multiply(cnormi[0])).add(yi[i - 1].multiply(cnormr[0]));
            yr[i - 1] = (DoubleDouble)str[0].clone();
        } // for (i = 1; i <= n; i++)

        return;
    }

    /**
     * complex multiply c = a * b.
     * 
     * @param ar DoubleDouble
     * @param ai DoubleDouble
     * @param br DoubleDouble
     * @param bi DoubleDouble
     * @param cr DoubleDouble[]
     * @param ci DoubleDouble[]
     */
    private void zmlt(final DoubleDouble ar, final DoubleDouble ai, final DoubleDouble br, final DoubleDouble bi, final DoubleDouble[] cr,
            final DoubleDouble[] ci) {
        DoubleDouble ca, cb;

        ca = (ar.multiply(br)).subtract(ai.multiply(bi));
        cb = (ar.multiply(bi)).add(ai.multiply(br));
        cr[0] = (DoubleDouble)ca.clone();
        ci[0] = (DoubleDouble)cb.clone();

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
        final DoubleDouble[] er = new DoubleDouble[5];
        final DoubleDouble[] t = new DoubleDouble[20];
        final DoubleDouble[] yr = new DoubleDouble[20];
        final DoubleDouble[] yi = new DoubleDouble[20];
        final DoubleDouble[] yyr = new DoubleDouble[20];
        final DoubleDouble[] yyi = new DoubleDouble[20];
        final DoubleDouble[] wr = new DoubleDouble[20];
        final DoubleDouble[] wi = new DoubleDouble[20];
        final int[] keps = new int[20];
        final int[] kdo = new int[20];
        int k;
        int k1;
        DoubleDouble aa;
        DoubleDouble dig;
        DoubleDouble slak;
        DoubleDouble ertol;
        DoubleDouble rm;
        DoubleDouble atol;
        final DoubleDouble rpi = (DoubleDouble.PI).reciprocal();
        final DoubleDouble spi = (DoubleDouble.PI).divide(DoubleDouble.valueOf(6.0));
        DoubleDouble con1r;
        DoubleDouble con1i;
        DoubleDouble con2r;
        DoubleDouble con2i;
        DoubleDouble pi3;
        DoubleDouble c13;
        DoubleDouble c23;
        DoubleDouble c43;
        DoubleDouble cavr;
        DoubleDouble cavi;
        DoubleDouble chir;
        DoubleDouble chii;
        DoubleDouble cir;
        DoubleDouble cii;
        int icl;
        int il;
        int i;
        DoubleDouble eps;
        DoubleDouble film;
        DoubleDouble ts;
        int itl;
        int lflg;
        int icase;
        int kode;
        int irset;
        int irb;
        int ir;
        DoubleDouble r;
        int it;
        DoubleDouble ct;
        DoubleDouble st;
        DoubleDouble zr;
        DoubleDouble zi;
        final int[] nz = new int[1];
        final int[] ierr = new int[1];
        final DoubleDouble[] cwr = new DoubleDouble[1];
        final DoubleDouble[] cwi = new DoubleDouble[1];
        final DoubleDouble[] cyr = new DoubleDouble[1];
        final DoubleDouble[] cyi = new DoubleDouble[1];
        final DoubleDouble[] cvr = new DoubleDouble[1];
        final DoubleDouble[] cvi = new DoubleDouble[1];
        final DoubleDouble[] car = new DoubleDouble[1];
        final DoubleDouble[] cai = new DoubleDouble[1];
        DoubleDouble zrr;
        DoubleDouble zri;
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        DoubleDouble ptr;
        int jb;
        int jl;
        DoubleDouble ab;
        DoubleDouble zwr = DoubleDouble.valueOf(0.0);
        DoubleDouble zwi = DoubleDouble.valueOf(0.0);
        DoubleDouble scr = DoubleDouble.valueOf(0.0);
        DoubleDouble sci = DoubleDouble.valueOf(0.0);
        DoubleDouble conar;
        DoubleDouble conai;
        DoubleDouble conbr;
        DoubleDouble conbi;
        DoubleDouble concr;
        DoubleDouble conci;
        DoubleDouble condr;
        DoubleDouble condi;
        final DoubleDouble[] yr0 = new DoubleDouble[1];
        final DoubleDouble[] yi0 = new DoubleDouble[1];
        final DoubleDouble[] yr1 = new DoubleDouble[1];
        final DoubleDouble[] yi1 = new DoubleDouble[1];
        final DoubleDouble[] yr2 = new DoubleDouble[1];
        final DoubleDouble[] yi2 = new DoubleDouble[1];
        final DoubleDouble[] yr3 = new DoubleDouble[1];
        final DoubleDouble[] yi3 = new DoubleDouble[1];
        final DoubleDouble[] yr4 = new DoubleDouble[1];
        final DoubleDouble[] yi4 = new DoubleDouble[1];
        final DoubleDouble[] wr0 = new DoubleDouble[1];
        final DoubleDouble[] wi0 = new DoubleDouble[1];
        final DoubleDouble[] wr1 = new DoubleDouble[1];
        final DoubleDouble[] wi1 = new DoubleDouble[1];
        int j;
        
        for (i = 0; i < 5; i++) {
        	er[i] = DoubleDouble.valueOf(0.0);
        }

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
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */

        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));

        // DoubleDoubleDigits is I1MACH(14), the number of base-B digits for DoubleDouble
        // precision. IEEE 754 DoubleDouble precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // In DoubleDouble, the mantissa effectively has 106 bits of resolution.
        DoubleDoubleDigits = 106;
        k1 = DoubleDoubleDigits - 1;
        aa = r1m5.multiply(DoubleDouble.valueOf(k1));
        dig = aa.min(DoubleDouble.valueOf(18.0));
        aa = (DoubleDouble.valueOf(2.303)).multiply(aa);
        alim = elim.add((aa.negate()).max(DoubleDouble.valueOf(-41.45)));
        rl = ((DoubleDouble.valueOf(1.2)).multiply(dig)).add(DoubleDouble.valueOf(3.0));
        fnul = (DoubleDouble.valueOf(10.0)).add((DoubleDouble.valueOf(6.0)).multiply(dig.subtract(DoubleDouble.valueOf(3.0))));
        slak = (DoubleDouble.valueOf(3.0)).add((DoubleDouble.valueOf(4.0)).
        		multiply( ( (DoubleDouble.valueOf(-0.434294481903251)).multiply(tol.log())).subtract(DoubleDouble.valueOf(7.0))).
        		divide(DoubleDouble.valueOf(11.0)));
        slak = slak.max(DoubleDouble.valueOf(3.0));
        ertol = tol.multiply((DoubleDouble.valueOf(10.0)).pow(slak));
        rm = (DoubleDouble.valueOf(0.5)).multiply(alim.add(elim));
        rm = rm.min(DoubleDouble.valueOf(200.0));
        rm = rm.max(rl.add(DoubleDouble.valueOf(10.0)));
        Preferences.debug("Quick check routine for the AIRY functions from zairy and zbiry\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("elim = " + elim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("alim = " + alim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rl = " + rl + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("fnul = " + fnul + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("dig = " + dig + "\n", Preferences.DEBUG_ALGORITHM);
        atol = (DoubleDouble.valueOf(100.0)).multiply(tol);
        con1r = spi.cos();
        con1i = spi.sin();
        con2r = (DoubleDouble)con1r.clone();
        con2i = con1i.negate();
        pi3 = (DoubleDouble.PI).divide(DoubleDouble.valueOf(3.0));
        c13 = (DoubleDouble.valueOf(3.0)).reciprocal();
        c23 = c13.add(c13);
        c43 = c23.add(c23);
        cavr = c13.sqrt();
        cavi = DoubleDouble.valueOf(0.0);
        chir = DoubleDouble.valueOf(0.0);
        chii = DoubleDouble.valueOf(0.5);
        cir = DoubleDouble.valueOf(0.0);
        cii = DoubleDouble.valueOf(1.0);
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
        eps = DoubleDouble.valueOf(0.01);
        film = DoubleDouble.valueOf(il - 1.0);
        t[0] = ((DoubleDouble.PI).negate()).add(eps);

        for (k = 2; k <= il; k++) {

            if (kdo[k - 1] == 0) {
                t[i - 1] = ((DoubleDouble.PI).multiply(DoubleDouble.valueOf( -il + (2.0 * k) - 1.0))).divide(film);

                if (keps[k - 1] != 0) {
                    ts = (DoubleDouble)t[i - 1].clone();
                    t[i - 1] = ts.subtract(eps);
                    i = i + 1;
                    t[i - 1] = ts.add(eps);
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
                            r = ( ((DoubleDouble.valueOf(0.2)).multiply(DoubleDouble.valueOf(4.0 - ir))).
                            		add((DoubleDouble.valueOf(2.0)).multiply(DoubleDouble.valueOf(ir - 1.0)))).
                            		divide(DoubleDouble.valueOf(3.0));
                        } // if (irset == 1)
                        else if (irset == 2) {
                            r = ( ((DoubleDouble.valueOf(2.0)).multiply(DoubleDouble.valueOf(4.0 - ir))).
                            		add(rl.multiply(DoubleDouble.valueOf(ir - 1.0)))).divide(DoubleDouble.valueOf(3.0));
                        } // else if (irset == 2)
                        else {
                            r = ( (rl.multiply(DoubleDouble.valueOf(4.0 - ir))).
                            		add(rm.multiply(DoubleDouble.valueOf(ir - 1.0)))).divide(DoubleDouble.valueOf(3.0));
                        } // else

                        for (it = 1; it <= itl; it++) {
                            ct = t[it - 1].cos();
                            st = t[it - 1].sin();

                            if ((ct.abs()).lt(atol)) {
                                ct = DoubleDouble.valueOf(0.0);
                            } // if (Math.abs(ct) < atol)

                            if ((st.abs()).lt(atol)) {
                                st = DoubleDouble.valueOf(0.0);
                            } // if (Math.abs(st) < atol)

                            zr = r.multiply(ct);
                            zi = r.multiply(st);

                            if ((t[it - 1].abs()).le(pi3)) {

                                // Wronskian check in -pi/3 < arg(z) < pi/3, test #1
                                zairy(zr, zi, 0, kode, yr0, yi0, nz, ierr);
                                yr[0] = (DoubleDouble)yr0[0].clone();
                                yi[0] = (DoubleDouble)yi0[0].clone();

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                zairy(zr, zi, 1, kode, yr1, yi1, nz, ierr);
                                yr[1] = (DoubleDouble)yr1[0].clone();
                                yi[1] = (DoubleDouble)yi1[0].clone();

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                zbiry(zr, zi, 0, kode, wr0, wi0, ierr);
                                wr[0] = (DoubleDouble)wr0[0].clone();
                                wi[0] = (DoubleDouble)wi0[0].clone();
                                zbiry(zr, zi, 1, kode, wr1, wi1, ierr);
                                wr[1] = (DoubleDouble)wr1[0].clone();
                                wi[1] = (DoubleDouble)wi1[0].clone();
                                cwr[0] = (yr[0].multiply(wr[1])).subtract(yi[0].multiply(wi[1]));
                                cwi[0] = (yr[0].multiply(wi[1])).add(yi[0].multiply(wr[1]));
                                cyr[0] = (yr[1].multiply(wr[0])).subtract(yi[1].multiply(wi[0]));
                                cyi[0] = (yr[1].multiply(wi[0])).add(yi[1].multiply(wr[0]));
                                cvr[0] = (DoubleDouble)rpi.clone();
                                cvi[0] = DoubleDouble.valueOf(0.0);

                                if (kode == 2) {
                                    zsqrt(zr, zi, car, cai);
                                    zrr = ( (zr.multiply(car[0])).subtract(zi.multiply(cai[0]))).multiply(c23);
                                    zri = ( (zr.multiply(cai[0])).add(zi.multiply(car[0]))).multiply(c23);
                                    aa = zrr.abs();
                                    car[0] = zrr.subtract(aa);
                                    cai[0] = (DoubleDouble)zri.clone();
                                    zexp(car[0], cai[0], str, sti);
                                    ptr = (str[0].multiply(cvr[0])).subtract(str[0].multiply(cvi[0]));
                                    cvi[0] = (str[0].multiply(cvi[0])).add(sti[0].multiply(cvr[0]));
                                    cvr[0] = (DoubleDouble)ptr.clone();
                                } // if (kode == 2)

                                cyr[0] = (cwr[0].subtract(cyr[0])).subtract(cvr[0]);
                                cyi[0] = (cwi[0].subtract(cyi[0])).subtract(cvi[0]);
                                er[0] = (zabs(cyr[0], cyi[0])).divide(zabs(cvr[0], cvi[0]));
                                jb = 1;
                                jl = 1;
                            } // if (Math.abs(t[it-1] <= pi3)
                            else { // Math.abs(t[it-1] > pi3
                                zairy(zr, zi, 0, kode, yr1, yi1, nz, ierr);
                                yr[1] = (DoubleDouble)yr1[0].clone();
                                yi[1] = (DoubleDouble)yi1[0].clone();

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                zrr = zr.negate();
                                zri = zi.negate();
                                zsqrt(zrr, zri, cvr, cvi);
                                ptr = ( (zrr.multiply(cvr[0])).subtract(zri.multiply(cvi[0]))).multiply(c23);
                                zri = ( (zrr.multiply(cvi[0])).add(zri.multiply(cvr[0]))).multiply(c23);
                                zrr = (DoubleDouble)ptr.clone();
                                zbesj(zrr, zri, c23, kode, 2, yyr, yyi, nz, ierr);

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                str[0] = yyr[0].multiply(c43);
                                sti[0] = yyi[0].multiply(c43);
                                zdiv(str[0], sti[0], zrr, zri, str, sti);
                                cyr[0] = str[0].subtract(yyr[1]);
                                cyi[0] = sti[0].subtract(yyi[1]);
                                car[0] = (DoubleDouble)yyr[0].clone();
                                cai[0] = (DoubleDouble)yyi[0].clone();
                                zbesj(zrr, zri, c13, kode, 2, yyr, yyi, nz, ierr);

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                if (kode == 2) {
                                    ab = zri.abs();
                                    zsqrt(zr, zi, cwr, cwi);
                                    zwr = ( (zr.multiply(cwr[0])).subtract(zi.multiply(cwi[0]))).multiply(c23);
                                    zwi = ( (zr.multiply(cwi[0])).add(zi.multiply(cwr[0]))).multiply(c23);
                                    cwr[0] = zwr.add(ab);
                                    cwi[0] = (DoubleDouble)zwi.clone();
                                    zexp(cwr[0], cwi[0], str, sti);
                                    cwr[0] = (DoubleDouble)str[0].clone();
                                    cwi[0] = (DoubleDouble)sti[0].clone();
                                    str[0] = (yyr[0].multiply(cwr[0])).subtract(yyi[0].multiply(cwi[0]));
                                    yyi[0] = (yyr[0].multiply(cwi[0])).add(yyi[0].multiply(cwr[0]));
                                    yyr[0] = (DoubleDouble)str[0].clone();
                                    str[0] = (yyr[1].multiply(cwr[0])).subtract(yyi[1].multiply(cwi[0]));
                                    yyi[1] = (yyr[1].multiply(cwi[0])).add(yyi[1].multiply(cwr[0]));
                                    yyr[1] = (DoubleDouble)str[0].clone();
                                    str[0] = (cyr[0].multiply(cwr[0])).subtract(cyi[0].multiply(cwi[0]));
                                    cyi[0] = (cyr[0].multiply(cwi[0])).add(cyi[0].multiply(cwr[0]));
                                    cyr[0] = (DoubleDouble)str[0].clone();
                                    str[0] = (car[0].multiply(cwr[0])).subtract(cai[0].multiply(cwi[0]));
                                    cai[0] = (car[0].multiply(cwi[0])).add(cai[0].multiply(cwr[0]));
                                    car[0] = (DoubleDouble)str[0].clone();
                                    scr = (DoubleDouble)cwr[0].clone();
                                    sci = (DoubleDouble)cwi[0].clone();
                                } // if (kode == 2)

                                cwr[0] = cvr[0].multiply(c13);
                                cwi[0] = cvi[0].multiply(c13);
                                wr[1] = (cwr[0].multiply(yyr[0].add(cyr[0]))).subtract(cwi[0].multiply(yyi[0].add(cyi[0])));
                                wi[1] = (cwr[0].multiply(yyi[0].add(cyi[0]))).add(cwi[0].multiply(yyr[0].add(cyr[0])));
                                str[0] = yr[1].subtract(wr[1]);
                                sti[0] = yi[1].subtract(wi[1]);
                                er[1] = zabs(str[0], sti[0]);

                                if ( (zi.ne(DoubleDouble.valueOf(0.0))) || (zr.ge(DoubleDouble.valueOf(0.0)))) {
                                    er[1] = er[1].divide(zabs(yr[1], yi[1]));
                                } // if ((zi != 0.0) || (zr >= 0.0))
                                else if (kode == 2) {
                                    er[1] = er[1].divide(zabs(scr, sci));
                                } // else if (kode == 2)

                                // Check Ai' Test #3
                                str[0] = yyr[0].multiply(c23);
                                sti[0] = yyi[0].multiply(c23);
                                zdiv(str[0], sti[0], zrr, zri, cyr, cyi);
                                cyr[0] = (cyr[0].subtract(yyr[1])).subtract(car[0]);
                                cyi[0] = (cyi[0].subtract(yyi[1])).subtract(cai[0]);
                                wr[2] = ( (zr.multiply(cyr[0])).subtract(zi.multiply(cyi[0]))).multiply(c13);
                                wi[2] = ( (zr.multiply(cyi[0])).add(zi.multiply(cyr[0]))).multiply(c13);
                                zairy(zr, zi, 1, kode, yr2, yi2, nz, ierr);
                                yr[2] = (DoubleDouble)yr2[0].clone();
                                yi[2] = (DoubleDouble)yi2[0].clone();
                                str[0] = yr[2].subtract(wr[2]);
                                sti[0] = yi[2].subtract(wi[2]);

                                if ( (zi.ne(DoubleDouble.valueOf(0.0))) || (zr.ge(DoubleDouble.valueOf(0.0)))) {
                                    er[2] = er[2].divide(zabs(yr[2], yi[2]));
                                } // if ((zi != 0.0) || (zr >= 0.0))
                                else if (kode == 2) {
                                    er[2] = er[2].divide(zabs(scr, sci));
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

                                conar = (DoubleDouble)con1r.clone();
                                conai = (DoubleDouble)con1i.clone();
                                conbr = (DoubleDouble)con2r.clone();
                                conbi = (DoubleDouble)con2i.clone();
                                concr = (DoubleDouble)con2r.clone();
                                conci = (DoubleDouble)con2i.clone();
                                condr = (DoubleDouble)con1r.clone();
                                condi = (DoubleDouble)con1i.clone();
                                
                                if (kode == 2) {
                                    aa = zwr.abs();
                                    zwr = ((cir.multiply(zrr)).subtract(cii.multiply(zri))).subtract(aa);
                                    zwi = (cir.multiply(zri)).add(cii.multiply(zrr));
                                    zexp(zwr, zwi, cwr, cwi);
                                    str[0] = (conar.multiply(cwr[0])).subtract(conai.multiply(cwi[0]));
                                    conai = (conar.multiply(cwi[0])).add(conai.multiply(cwr[0]));
                                    conar = (DoubleDouble)str[0].clone();
                                    str[0] = (concr.multiply(cwr[0])).subtract(conci.multiply(cwi[0]));
                                    conci = (concr.multiply(cwi[0])).add(conci.multiply(cwr[0]));
                                    concr = (DoubleDouble)str[0].clone();
                                    zwr = (( (cir.multiply(zrr)).subtract(cii.multiply(zri))).negate()).subtract(aa);
                                    zwi = ( (cir.multiply(zri)).add(cii.multiply(zrr))).negate();
                                    zexp(zwr, zwi, cwr, cwi);
                                    str[0] = (conbr.multiply(cwr[0])).subtract(conbi.multiply(cwi[0]));
                                    conbi = (conbr.multiply(cwi[0])).add(conbi.multiply(cwr[0]));
                                    conbr = (DoubleDouble)str[0].clone();
                                    str[0] = (condr.multiply(cwr[0])).subtract(condi.multiply(cwi[0]));
                                    condi = (condr.multiply(cwi[0])).add(condi.multiply(cwr[0]));
                                    condr = (DoubleDouble)str[0].clone();
                                    scr = (DoubleDouble)cwr[0].clone();
                                    sci = (DoubleDouble)cwi[0].clone();
                                } // if (kode == 2)

                                cwr[0] = (conar.multiply(yr[0])).subtract(conai.multiply(yi[0]));
                                cwi[0] = (conar.multiply(yi[0])).add(conai.multiply(yr[0]));
                                cwr[0] = cwr[0].subtract( (conbr.multiply(yyr[0])).subtract(conbi.multiply(yyi[0])));
                                cwi[0] = cwi[0].subtract( (conbr.multiply(yyi[0])).add(conbi.multiply(yyr[0])));
                                str[0] = (cvr[0].multiply(cavr)).subtract(cvi[0].multiply(cavi));
                                sti[0] = (cvr[0].multiply(cavi)).add(cvi[0].multiply(cavr));
                                ptr = (str[0].multiply(cwr[0])).subtract(sti[0].multiply(cwi[0]));
                                cwi[0] = (str[0].multiply(cwi[0])).add(sti[0].multiply(cwr[0]));
                                cwr[0] = (DoubleDouble)ptr.clone();
                                wr[3] = (cwr[0].multiply(chir)).subtract(cwi[0].multiply(chii));
                                wi[3] = (cwr[0].multiply(chii)).add(cwi[0].multiply(chir));
                                zbiry(zr, zi, 0, kode, yr3, yi3, ierr);
                                yr[3] = (DoubleDouble)yr3[0].clone();
                                yi[3] = (DoubleDouble)yi3[0].clone();
                                str[0] = yr[3].subtract(wr[3]);
                                sti[0] = yi[3].subtract(wi[3]);
                                er[3] = zabs(str[0], sti[0]);

                                if ( (zi.ne(DoubleDouble.valueOf(0.0))) || (zr.ge(DoubleDouble.valueOf(0.0)))) {
                                    er[3] = er[3].divide(zabs(yr[3], yi[3]));
                                } // if ((zi != 0.0) || (zr >= 0.0))
                                else if (kode == 2) {
                                    er[3] = er[3].divide(zabs(scr, sci));
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

                                cwr[0] = (concr.multiply(yr[0])).subtract(conci.multiply(yi[0]));
                                cwi[0] = (concr.multiply(yi[0])).add(conci.multiply(yr[0]));
                                cwr[0] = cwr[0].subtract( (condr.multiply(yyr[0])).subtract(condi.multiply(yyi[0])));
                                cwi[0] = cwi[0].subtract( (condr.multiply(yyi[0])).add(condi.multiply(yyr[0])));
                                str[0] = ( (zr.multiply(cavr)).subtract(zi.multiply(cavi))).negate();
                                sti[0] = ( (zr.multiply(cavi)).add(zi.multiply(cavr))).negate();
                                ptr = (str[0].multiply(cwr[0])).subtract(sti[0].multiply(cwi[0]));
                                cwi[0] = (str[0].multiply(cwi[0])).add(sti[0].multiply(cwr[0]));
                                cwr[0] = (DoubleDouble)ptr.clone();
                                wr[4] = (cwr[0].multiply(chir)).subtract(cwi[0].multiply(chii));
                                wi[4] = (cwr[0].multiply(chii)).add(cwi[0].multiply(chir));
                                zbiry(zr, zi, 1, kode, yr4, yi4, ierr);
                                yr[4] = (DoubleDouble)yr4[0].clone();
                                yi[4] = (DoubleDouble)yi4[0].clone();
                                str[0] = yr[4].subtract(wr[4]);
                                sti[0] = yi[4].subtract(wi[4]);
                                er[4] = zabs(str[0], sti[0]);

                                if ( (zi.ne(DoubleDouble.valueOf(0.0))) || (zr.ge(DoubleDouble.valueOf(0.0)))) {
                                    er[4] = er[4].divide(zabs(yr[4], yi[4]));
                                } // if ((zi != 0.00 || (zr >= 0.0))
                                else if (kode == 2) {
                                    er[4] = er[4].divide(zabs(yr[4], yi[4]));
                                } // else if (kode == 2)

                                jb = 2;
                                jl = 5;
                            } // else Math.abs(t[it-1] > pi3)

                            for (j = jb; j <= jl; j++) {

                                if (er[j - 1].lt(ertol)) {
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
        final DoubleDouble[] t = new DoubleDouble[20];
        final DoubleDouble[] aer = new DoubleDouble[20];
        final DoubleDouble[] xnu = new DoubleDouble[20];
        final DoubleDouble[] ur = new DoubleDouble[20];
        final DoubleDouble[] ui = new DoubleDouble[20];
        final DoubleDouble[] vr = new DoubleDouble[20];
        final DoubleDouble[] vi = new DoubleDouble[20];
        final DoubleDouble[] wr = new DoubleDouble[20];
        final DoubleDouble[] wi = new DoubleDouble[20];
        final DoubleDouble[] yr = new DoubleDouble[20];
        final DoubleDouble[] yi = new DoubleDouble[20];
        final int[] keps = new int[20];
        final int[] kdo = new int[20];
        int k;
        int k1;
        DoubleDouble aa;
        DoubleDouble dig;
        DoubleDouble slak;
        DoubleDouble ertol;
        DoubleDouble rm;
        DoubleDouble r2;
        DoubleDouble atol;
        int nL;
        int il;
        int i;
        int nul;
        DoubleDouble eps;
        DoubleDouble film;
        DoubleDouble ts;
        int itl;
        int lflg;
        int kode;
        int n;
        int nu;
        DoubleDouble fnu;
        int icase;
        int irb;
        int ir;
        DoubleDouble r;
        int it;
        DoubleDouble ct;
        DoubleDouble st;
        DoubleDouble zr;
        DoubleDouble zi;
        int m;
        final int[] ierr = new int[1];
        final int[] nz1 = new int[1];
        int ihp;
        final DoubleDouble[] znr = new DoubleDouble[1];
        final DoubleDouble[] zni = new DoubleDouble[1];
        final int[] nz2 = new int[1];
        final int[] nz3 = new int[1];
        DoubleDouble ab;
        DoubleDouble csgnr;
        DoubleDouble csgni;
        int mflg;
        DoubleDouble av;
        DoubleDouble er;
        int kk;
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        DoubleDouble err;
        DoubleDouble cwr;
        DoubleDouble cwi;

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
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */

        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preserves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));

        // DoubleDoubleDigits is I1MACH(14), the number of base-B digits for DoubleDouble
        // precision. IEEE 754 DoubleDouble precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // In DoubleDouble, the mantissa effectively has 106 bits of resolution.
        DoubleDoubleDigits = 106;
        k1 = DoubleDoubleDigits - 1;
        aa = r1m5.multiply(DoubleDouble.valueOf(k1));
        dig = aa.min(DoubleDouble.valueOf(18.0));
        aa = (DoubleDouble.valueOf(2.303)).multiply(aa);
        alim = elim.add( (aa.negate()).max(DoubleDouble.valueOf(-41.45)));
        rl = ((DoubleDouble.valueOf(1.2)).multiply(dig)).add(DoubleDouble.valueOf(3.0));
        fnul = (DoubleDouble.valueOf(10.0)).add((DoubleDouble.valueOf(6.0)).multiply(dig.subtract(DoubleDouble.valueOf(3.0))));
        slak = (DoubleDouble.valueOf(3.0)).add((DoubleDouble.valueOf(4.0)).
        		multiply( ( (DoubleDouble.valueOf(-0.434294481903251)).multiply(tol.log())).subtract(DoubleDouble.valueOf(7.0))).
        		divide(DoubleDouble.valueOf(11.0)));
        slak = slak.max(DoubleDouble.valueOf(3.0));
        ertol = tol.multiply((DoubleDouble.valueOf(10.0)).pow(slak));
        rm = (DoubleDouble.valueOf(0.5)).multiply(alim.add(elim));
        rm = rm.min(DoubleDouble.valueOf(200.0));
        rm = rm.max(rl.add(DoubleDouble.valueOf(10.0)));
        r2 = rm.min(fnul);
        Preferences.debug("Quick check routine for the H Bessel function from ZBESH\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("elim = " + elim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("alim = " + alim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rl = " + rl + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("fnul = " + fnul + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("dig = " + dig + "\n", Preferences.DEBUG_ALGORITHM);
        atol = (DoubleDouble.valueOf(100.0)).multiply(tol);
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
            xnu[0] = DoubleDouble.valueOf(0.0);
            xnu[1] = DoubleDouble.valueOf(1.0);
            xnu[2] = DoubleDouble.valueOf(2.0);
            xnu[3] = (DoubleDouble.valueOf(0.5)).multiply(fnul);
            xnu[4] = fnul.add(DoubleDouble.valueOf(1.1));
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
            xnu[0] = DoubleDouble.valueOf(0.0);
            xnu[1] = DoubleDouble.valueOf(0.6);
            xnu[2] = DoubleDouble.valueOf(1.3);
            xnu[3] = DoubleDouble.valueOf(2.0);
            xnu[4] = (DoubleDouble.valueOf(0.5)).multiply(fnul);
            xnu[5] = fnul.add(DoubleDouble.valueOf(1.1));
        } // else mqc == 2

        i = 2;
        eps = DoubleDouble.valueOf(0.01);
        film = DoubleDouble.valueOf(il - 1.0);
        t[0] = ((DoubleDouble.PI).negate()).add(eps);

        for (k = 2; k <= il; k++) {

            if (kdo[k - 1] == 0) {
                t[i - 1] = (DoubleDouble.PI).multiply(DoubleDouble.valueOf( -il + (2.0 * k) - 1.0)).divide(film);

                if (keps[k - 1] != 0) {
                    ts = (DoubleDouble)t[i - 1].clone();
                    t[i - 1] = ts.subtract(eps);
                    i = i + 1;
                    t[i - 1] = ts.add(eps);
                } // if (keps[k-1] != 0)

                i = i + 1;
            } // if (kdo[k-1] == 0)
        } // for (k = 2; k <= il; k++)

        itl = i - 1;
        lflg = 0;

        for (kode = 1; kode <= 2; kode++) {

            for (n = 1; n <= nL; n++) {

                for (nu = 1; nu <= nul; nu++) {
                    fnu = (DoubleDouble)xnu[nu - 1].clone();

                    group: for (icase = 1; icase <= 3; icase++) {
                        irb = Math.min(icase, 2);

                        for (ir = irb; ir <= 3; ir++) {

                            if (icase == 1) {
                                r = ( (eps.multiply(DoubleDouble.valueOf(3.0 - ir))).
                                		add((DoubleDouble.valueOf(2.0)).multiply(DoubleDouble.valueOf(ir - 1.0)))).
                                		divide(DoubleDouble.valueOf(2.0));
                            } // if (icase == 1)
                            else if (icase == 2) {
                                r = ( ((DoubleDouble.valueOf(2.0)).multiply(DoubleDouble.valueOf(3.0 - ir))).
                                		add(r2.multiply(DoubleDouble.valueOf(ir - 1.0)))).divide(DoubleDouble.valueOf(2.0));
                            } // else if (icase == 2)
                            else if ( (icase == 3) && (r2.ge(rm))) {
                                continue group;
                            } // else if ((icase == 3) && (r2 >= rm))
                            else {
                                r = ( (r2.multiply(DoubleDouble.valueOf(3.0 - ir))).
                                		add(rm.multiply(DoubleDouble.valueOf(ir - 1.0)))).divide(DoubleDouble.valueOf(2.0));
                            } // else

                            for (it = 1; it <= itl; it++) {
                                ct = t[it - 1].cos();
                                st = t[it - 1].sin();

                                if ((ct.abs()).lt( atol)) {
                                    ct = DoubleDouble.valueOf(0.0);
                                } // if (Math.abs(ct) < atol)

                                if ((st.abs()).lt(atol)) {
                                    st = DoubleDouble.valueOf(0.0);
                                } // if (Math.abs(st) < atol)

                                zr = r.multiply(ct);
                                zi = r.multiply(st);

                                if (kode == 1) {
                                    m = 2;
                                    zbesh(zr, zi, fnu, kode, m, n, yr, yi, nz1, ierr);

                                    if ( (ierr[0] != 0) || (nz1[0] != 0)) {
                                        continue;
                                    } // if ((ierr[0] != 0) || (nz1[0] != 0)

                                    if ( (st.lt(DoubleDouble.valueOf(0.0))) ||
                                    		( (st.equals(DoubleDouble.valueOf(0.0))) && (ct.gt(DoubleDouble.valueOf(0.0))))) {
                                        ihp = 1;
                                        znr[0] = zr.negate();
                                        zni[0] = zi.negate();
                                        m = 1;
                                        zbesh(znr[0], zni[0], fnu, kode, m, n, wr, wi, nz2, ierr);

                                        if ( (ierr[0] != 0) || (nz2[0] != 0)) {
                                            continue;
                                        } // if ((ierr[0] != 0) || (nz2[0] != 0))
                                    } // if ((st < 0.0) || ((st == 0.0) && (ct > 0.0)))
                                    else {
                                        ihp = 2;
                                        znr[0] = zr.negate();
                                        zni[0] = zi.negate();
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

                                    ab = (fnu.mod(DoubleDouble.valueOf(2.0))).multiply(DoubleDouble.PI);
                                    csgnr = ab.cos();
                                    csgni = ab.sin();
                                    mflg = 0;

                                    for (i = 1; i <= n; i++) {
                                        ab = fnu.add(DoubleDouble.valueOf(i - 1.0));
                                        aa = (DoubleDouble.valueOf(0.5)).max(ab);

                                        if (ihp == 1) {
                                            vr[i - 1] = ( (csgnr.multiply(wr[i - 1])).subtract(csgni.multiply(wi[i - 1]))).negate();
                                            vi[i - 1] = ( (csgnr.multiply(wi[i - 1])).add(csgni.multiply(wr[i - 1]))).negate();
                                            cwr = yr[i - 1].subtract(vr[i - 1]);
                                            cwi = yi[i - 1].subtract(vi[i - 1]);
                                        } // if (ihp == 1)
                                        else { // ihp != 1
                                            cwr = csgnr.add(csgnr);
                                            str[0] = ((cwr.multiply(wr[i - 1])).add(csgnr.multiply(vr[i - 1]))).
                                            		subtract(csgni.multiply(vi[i - 1]));
                                            vi[i - 1] = ((cwr.multiply(wi[i - 1])).add(csgnr.multiply(vi[i - 1]))).
                                            		add(csgni.multiply(vr[i - 1]));
                                            vr[i - 1] = (DoubleDouble)str[0].clone();
                                            cwr = yr[i - 1].subtract(vr[i - 1]);
                                            cwi = yi[i - 1].subtract(vi[i - 1]);
                                        } // else ihp != 1

                                        av = zabs(yr[i - 1], yi[i - 1]);
                                        er = zabs(cwr, cwi);

                                        if (zi.equals(DoubleDouble.valueOf(0.0))) {

                                            if ((zr.abs()).lt(aa)) {
                                                er = er.divide(av);
                                            } // if (Math.abs(zr) < aa)
                                        } // if (zi == 0.0)
                                        else { // zi != 0.0
                                            er = er.divide(av);
                                        } // else zi != 0.0

                                        aer[i - 1] = er;

                                        if (er.gt(ertol)) {
                                            mflg = 1;
                                        } // if (er > ertol)

                                        csgnr = csgnr.negate();
                                        csgni = csgni.negate();
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

                                    znr[0] = zi.negate();
                                    zni[0] = (DoubleDouble)zr.clone();
                                    zexp(znr[0], zni[0], znr, zni);
                                    mflg = 0;

                                    for (i = 1; i <= n; i++) {
                                        ab = fnu.add(DoubleDouble.valueOf(i - 1.0));
                                        aa = (DoubleDouble.valueOf(0.5)).max(ab);
                                        zdiv(ur[i - 1], ui[i - 1], znr[0], zni[0], str, sti);
                                        cwr = str[0].subtract(vr[i - 1]);
                                        cwi = sti[0].subtract(vi[i - 1]);
                                        av = zabs(vr[i - 1], vi[i - 1]);
                                        er = zabs(cwr, cwi);

                                        if (zi.equals(DoubleDouble.valueOf(0.0))) {

                                            if ((zr.abs()).lt(aa)) {
                                                er = er.divide(av);
                                            } // if (Math.abs(zr) < aa)
                                        } // if (zi == 0.0)
                                        else { // zi != 0.0
                                            er = er.divide(av);
                                        } // else zi != 0.0

                                        err = (DoubleDouble)er.clone();

                                        if (er.gt(ertol)) {
                                            mflg = 1;
                                        } // if (er > ertol)

                                        cwr = ((znr[0].multiply(wr[i - 1])).subtract(zni[0].multiply(wi[i - 1]))).subtract(yr[i - 1]);
                                        cwi = ((znr[0].multiply(wi[i - 1])).add(zni[0].multiply(wr[i - 1]))).subtract(yi[i - 1]);
                                        av = zabs(yr[i - 1], yi[i - 1]);
                                        er = zabs(cwr, cwi);

                                        if (zi.equals(DoubleDouble.valueOf(0.0))) {

                                            if ((zr.abs()).lt(aa)) {
                                                er = er.divide(av);
                                            } // if (Math.abs(zr) < aa)
                                        } // if (zi == 0.0)
                                        else { // zi != 0.0
                                            er = er.divide(av);
                                        } // else zi != 0.0

                                        if (er.gt(ertol)) {
                                            mflg = 1;
                                        } // if (er > ertol)

                                        aer[i - 1] = er.add(err);
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
        int k;
        int k1;
        DoubleDouble aa;
        DoubleDouble dig;
        DoubleDouble slak;
        DoubleDouble ertol;
        DoubleDouble rm;
        DoubleDouble r2;
        DoubleDouble atol;
        int nL;
        int il;
        final DoubleDouble[] t = new DoubleDouble[20];
        final DoubleDouble[] aer = new DoubleDouble[20];
        final DoubleDouble[] yr = new DoubleDouble[20];
        final DoubleDouble[] yi = new DoubleDouble[20];
        final DoubleDouble[] wr = new DoubleDouble[20];
        final DoubleDouble[] wi = new DoubleDouble[20];
        final DoubleDouble[] xnu = new DoubleDouble[20];
        final int[] keps = new int[20];
        final int[] kdo = new int[20];
        int i;
        int nul;
        DoubleDouble eps;
        DoubleDouble film;
        DoubleDouble ts;
        int itl;
        int lflg;
        int kode;
        int n = 20;
        DoubleDouble fnu;
        int ifnu;
        DoubleDouble ffnu;
        DoubleDouble arg;
        DoubleDouble csgnr;
        DoubleDouble csgni;
        int icase;
        int irb;
        int ir;
        DoubleDouble r;
        int it;
        DoubleDouble ct;
        DoubleDouble st;
        DoubleDouble zr;
        DoubleDouble zi;
        final int[] nz = new int[1];
        final int[] ierr = new int[1];
        final DoubleDouble[] cvr = new DoubleDouble[1];
        final DoubleDouble[] cvi = new DoubleDouble[1];
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        DoubleDouble ccr;
        DoubleDouble zrr;
        DoubleDouble zri;
        DoubleDouble cwr;
        DoubleDouble cwi;
        int mflg;
        int kk;
        DoubleDouble er;
        DoubleDouble gnu;
        int nu;
        int nONE;
        DoubleDouble cyr;
        DoubleDouble cyi;
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
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */

        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));

        // DoubleDoubleDigits is I1MACH(14), the number of base-B digits for DoubleDouble
        // precision. IEEE 754 DoubleDouble precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // In DoubleDouble, the mantissa effectively has 106 bits of resolution.
        DoubleDoubleDigits = 106;
        k1 = DoubleDoubleDigits - 1;
        aa = r1m5.multiply(DoubleDouble.valueOf(k1));
        dig = aa.min(DoubleDouble.valueOf(18.0));
        aa = (DoubleDouble.valueOf(2.303)).multiply(aa);
        alim = elim.add((aa.negate()).max(DoubleDouble.valueOf(-41.45)));
        rl = ((DoubleDouble.valueOf(1.2)).multiply(dig)).add(DoubleDouble.valueOf(3.0));
        fnul = (DoubleDouble.valueOf(10.0)).add((DoubleDouble.valueOf(6.0)).multiply(dig.subtract(DoubleDouble.valueOf(3.0))));
        slak = (DoubleDouble.valueOf(3.0)).add((DoubleDouble.valueOf(4.0)).
        		multiply( ( (DoubleDouble.valueOf(-0.434294481903251)).multiply(tol.log())).subtract(DoubleDouble.valueOf(7.0))).
        		divide(DoubleDouble.valueOf(11.0)));
        slak = slak.max(DoubleDouble.valueOf(3.0));
        ertol = tol.multiply((DoubleDouble.valueOf(10.0)).pow(slak));
        rm = (DoubleDouble.valueOf(0.5)).multiply(alim.add(elim));
        rm = rm.min(DoubleDouble.valueOf(200.0));
        rm = rm.max(rl.add(DoubleDouble.valueOf(10.0)));
        r2 = rm.min(fnul);
        Preferences.debug("Quick check routine for the I Bessel function from ZBESI\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("elim = " + elim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("alim = " + alim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rl = " + rl + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("fnul = " + fnul + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("dig = " + dig + "\n", Preferences.DEBUG_ALGORITHM);
        atol = (DoubleDouble.valueOf(100.0)).multiply(tol);
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
            xnu[0] = DoubleDouble.valueOf(0.0);
            xnu[1] = DoubleDouble.valueOf(1.0);
            xnu[2] = DoubleDouble.valueOf(2.0);
            xnu[3] = (DoubleDouble.valueOf(0.5)).multiply(fnul);
            xnu[4] = fnul.add(DoubleDouble.valueOf(1.1));
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
            xnu[0] = DoubleDouble.valueOf(0.0);
            xnu[1] = DoubleDouble.valueOf(0.6);
            xnu[2] = DoubleDouble.valueOf(1.3);
            xnu[3] = DoubleDouble.valueOf(2.0);
            xnu[4] = (DoubleDouble.valueOf(0.5)).multiply(fnul);
            xnu[5] = fnul.add(DoubleDouble.valueOf(1.1));
        } // else mqc == 2

        i = 2;
        eps = DoubleDouble.valueOf(0.01);
        film = DoubleDouble.valueOf(il - 1.0);
        t[0] = ((DoubleDouble.PI).negate()).add(eps);

        for (k = 2; k <= il; k++) {

            if (kdo[k - 1] == 0) {
                t[i - 1] = ((DoubleDouble.PI).multiply(DoubleDouble.valueOf( -il + (2.0 * k) - 1.0))).divide(film);

                if (keps[k - 1] != 0) {
                    ts = (DoubleDouble)t[i - 1].clone();
                    t[i - 1] = ts.subtract(eps);
                    i = i + 1;
                    t[i - 1] = ts.add(eps);
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
                    fnu = (DoubleDouble)xnu[nu - 1].clone();
                    ifnu = fnu.intValue();
                    ffnu = fnu.subtract(DoubleDouble.valueOf(ifnu));
                    arg = (DoubleDouble.PI).multiply(ffnu);
                    csgnr = arg.cos();
                    csgni = arg.sin();

                    if ( (ifnu % 2) != 0) {
                        csgnr = csgnr.negate();
                        csgni = csgni.negate();
                    } // if ((ifnu%2) != 0)

                    for (icase = 1; icase <= 3; icase++) {
                        irb = Math.min(2, icase);

                        for (ir = irb; ir <= 4; ir++) {

                            if (icase == 1) {
                                r = ( ((DoubleDouble.valueOf(0.2)).multiply(DoubleDouble.valueOf(4.0 - ir))).
                                		add((DoubleDouble.valueOf(2.0)).multiply(DoubleDouble.valueOf(ir - 1.0)))).
                                		divide(DoubleDouble.valueOf(3.0));
                            } // if (icase == 1)
                            else if (icase == 2) {
                                r = ( ((DoubleDouble.valueOf(2.0)).multiply(DoubleDouble.valueOf(4.0 - ir))).
                                		add(r2.multiply(DoubleDouble.valueOf(ir - 1.0)))).divide(DoubleDouble.valueOf(3.0));
                            } // else if (icase == 2)
                            else if ( (icase == 3) && (r2 == rm)) {
                                continue group;
                            } // else if ((icase == 3) && (r2 == rm))
                            else { // icase == 3
                                r = ( (r2.multiply(DoubleDouble.valueOf(4.0 - ir))).
                                		add(rm.multiply(DoubleDouble.valueOf(ir - 1.0)))).divide(DoubleDouble.valueOf(3.0));
                            } // else icase == 3

                            for (it = 1; it <= itl; it++) {
                                ct = t[it - 1].cos();
                                st = t[it - 1].sin();

                                if ((ct.abs()).lt(atol)) {
                                    ct = DoubleDouble.valueOf(0.0);
                                } // if (Math.abs(ct) < atol)

                                if ((st.abs()).lt(atol)) {
                                    st = DoubleDouble.valueOf(0.0);
                                } // if (Math.abs(st) < atol)

                                zr = r.multiply(ct);
                                zi = r.multiply(st);

                                if (ct.ge(DoubleDouble.valueOf(0.0))) {

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
                                    zdiv(DoubleDouble.valueOf(1.0), DoubleDouble.valueOf(0.0), zr, zi, cvr, cvi);

                                    if (kode == 2) {
                                        str[0] = zi.cos();
                                        sti[0] = zi.sin();
                                        aa = (str[0].multiply(cvr[0])).subtract(sti[0].multiply(cvi[0]));
                                        cvi[0] = (str[0].multiply(cvi[0])).add(sti[0].multiply(cvr[0]));
                                        cvr[0] = (DoubleDouble)aa.clone();
                                    } // if (kode == 2)

                                    ccr = DoubleDouble.valueOf(1.0);
                                } // if (ct >= 0.0)
                                else { // ct < 0.0

                                    // Wronskian checks in the left half plane
                                    zrr = zr.negate();
                                    zri = zi.negate();
                                    zbesi(zr, zi, fnu, kode, nONE, yr, yi, nz, ierr);

                                    if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                        continue;
                                    } // if ((nz[0] != 0) || (ierr[0] != 0))

                                    zbesk(zrr, zri, fnu, kode, nONE, wr, wi, nz, ierr);

                                    if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                        continue;
                                    } // if ((nz[0] != 0) || (ierr[0] != 0))

                                    cvr[0] = (DoubleDouble)csgnr.clone();
                                    cvi[0] = (DoubleDouble)csgni.clone();

                                    if (zi.lt(DoubleDouble.valueOf(0.0))) {
                                        cvi[0] = cvi[0].negate();
                                    } // if (zi < 0.0)

                                    zdiv(cvr[0], cvi[0], zr, zi, cvr, cvi);

                                    if (kode == 2) {

                                        // Adjustments to Wronskian due to scaling of I and K
                                        // functions on kode = 2. Scale factor = exp(-i*yy)
                                        // for Re(z) < 0.0
                                        cwr = zi.cos();
                                        cwi = (zi.sin()).negate();
                                        str[0] = (cvr[0].multiply(cwr)).subtract(cvi[0].multiply(cwi));
                                        cvi[0] = (cvr[0].multiply(cwi)).add(cvi[0].multiply(cwr));
                                        cvr[0] = (DoubleDouble)str[0].clone();
                                    } // if (kode == 2)

                                    ccr = DoubleDouble.valueOf(-1.0);
                                } // else ct < 0.0

                                mflg = 0;
                                kk = 0;

                                for (i = 1; i <= n; i++) {
                                    cwr = (wr[i - 1].multiply(yr[i])).subtract(wi[i - 1].multiply(yi[i]));
                                    cwi = (wr[i - 1].multiply(yi[i])).add(wi[i - 1].multiply(yr[i]));
                                    cyr = (yr[i - 1].multiply(wr[i])).subtract(yi[i - 1].multiply(wi[i]));
                                    cyi = (yr[i - 1].multiply(wi[i])).add(yi[i - 1].multiply(wr[i]));
                                    cyr = ccr.multiply(cyr);
                                    cyi = ccr.multiply(cyi);
                                    cyr = (cyr.add(cwr)).subtract(cvr[0]);
                                    cyi = (cyi.add(cwi)).subtract(cvi[0]);
                                    er = (zabs(cyr, cyi)).divide(zabs(cvr[0], cvi[0]));
                                    aer[i - 1] = (DoubleDouble)er.clone();

                                    if ( (er.gt(ertol)) && (kk == 0)) {
                                        mflg = 1;
                                        kk = i;
                                    } // if ((er > ertol) && (kk == 0))

                                    if (ct.lt(DoubleDouble.valueOf(0.0))) {
                                        cvr[0] = cvr[0].negate();
                                        cvi[0] = cvi[0].negate();
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
        zr = DoubleDouble.valueOf(1.4);
        zi = DoubleDouble.valueOf(1.4);
        iprnt = 0;

        for (i = 1; i <= 2; i++) {
            fnu = DoubleDouble.valueOf(10.2);
            kode = 1;
            n = 20;

            while (true) {
                zbesi(zr, zi, fnu, kode, n, yr, yi, nz, ierr);

                if (nz[0] == 0) {
                    fnu = fnu.add(DoubleDouble.valueOf(5.0));

                    continue;
                } // if (nz[0] == 0)

                if (nz[0] < 10) {
                    break;
                } // if (nz[0] < 10)

                fnu = fnu.subtract(DoubleDouble.valueOf(1.0));
            } // while (true)

            zbesk(zr, zi, fnu, kode, 2, wr, wi, nz, ierr);
            zdiv(DoubleDouble.valueOf(1.0), DoubleDouble.valueOf(0.0), zr, zi, str, sti);
            cyr = (wr[0].multiply(yr[1])).subtract(wi[0].multiply(yi[1]));
            cyi = (wr[0].multiply(yi[1])).add(wi[0].multiply(yr[1]));
            cwr = (wr[1].multiply(yr[0])).subtract(wi[1].multiply(yi[0]));
            cwi = (wr[1].multiply(yi[0])).add(wi[1].multiply(yr[0]));
            cwr = (cwr.add(cyr)).subtract(str[0]);
            cwi = (cwi.add(cyi)).subtract(sti[0]);
            er = (zabs(cwr, cwi)).divide(zabs(str[0], sti[0]));

            if (er.ge(ertol)) {

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

            zr = rl.add(rl);
            zi = DoubleDouble.valueOf(0.0);
        } // for (i = 1; i <= 2; i++)

        // Check near overflow limits
        zr = (DoubleDouble)elim.clone();
        zi = DoubleDouble.valueOf(0.0);
        fnu = DoubleDouble.valueOf(0.0);

        while (true) {
            zbesk(zr, zi, fnu, kode, n, yr, yi, nz, ierr);

            if (nz[0] < 10) {
                break;
            } // if (nz[0] < 10)

            if (nz[0] == n) {
                fnu = fnu.add(DoubleDouble.valueOf(3.0));
            } // if (nz[0] == n)

            fnu = fnu.add(DoubleDouble.valueOf(2.0));
        } // while (true)

        gnu = fnu.add(DoubleDouble.valueOf(n - 2.0));
        zbesi(zr, zi, gnu, kode, 2, wr, wi, nz, ierr);
        zdiv(DoubleDouble.valueOf(1.0), DoubleDouble.valueOf(0.0), zr, zi, str, sti);
        cyr = (yr[n - 2].multiply(wr[1])).subtract(yi[n - 2].multiply(wi[1]));
        cyi = (yr[n - 2].multiply(wi[1])).add(yi[n - 2].multiply(wr[1]));
        cwr = (yr[n - 1].multiply(wr[0])).subtract(yi[n - 1].multiply(wi[0]));
        cwi = (yr[n - 1].multiply(wi[0])).add(yi[n - 1].multiply(wr[0]));
        cwr = (cwr.add(cyr)).subtract(str[0]);
        cwi = (cwi.add(cyi)).subtract(sti[0]);
        er = (zabs(cwr, cwi)).divide(zabs(str[0], sti[0]));

        if (er.ge(ertol)) {

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
        final DoubleDouble[] t = new DoubleDouble[20];
        final DoubleDouble[] aer = new DoubleDouble[25];
        final DoubleDouble[] xnu = new DoubleDouble[20];
        final DoubleDouble[] cjr = new DoubleDouble[20];
        final DoubleDouble[] cji = new DoubleDouble[20];
        final DoubleDouble[] chr = new DoubleDouble[20];
        final DoubleDouble[] chi = new DoubleDouble[20];
        final int[] keps = new int[20];
        final int[] kdo = new int[20];
        int k;
        int k1;
        DoubleDouble aa;
        DoubleDouble dig;
        DoubleDouble slak;
        DoubleDouble ertol;
        DoubleDouble rm;
        DoubleDouble r2;
        DoubleDouble atol;
        final DoubleDouble hpi = DoubleDouble.PI_2;
        int nL;
        int il;
        int i;
        int nul;
        DoubleDouble eps;
        DoubleDouble film;
        DoubleDouble ts;
        int itl;
        int lflg;
        int kode;
        DoubleDouble conr;
        DoubleDouble coni;
        int n;
        int np;
        DoubleDouble fnu;
        int nu;
        DoubleDouble gnu;
        int icase;
        int irb;
        int ir;
        DoubleDouble r;
        int it;
        DoubleDouble ct;
        DoubleDouble st;
        DoubleDouble zr;
        DoubleDouble zi;
        final DoubleDouble[] wrr = new DoubleDouble[1];
        final DoubleDouble[] wri = new DoubleDouble[1];
        int m;
        final int[] nzj = new int[1];
        final int[] ierrj = new int[1];
        final int[] nzh = new int[1];
        final int[] ierrh = new int[1];
        DoubleDouble sgn;
        DoubleDouble str;
        DoubleDouble sti;
        DoubleDouble t1r;
        int kk;
        int mflg;
        DoubleDouble t1i;
        DoubleDouble t2r;
        DoubleDouble t2i;
        DoubleDouble cerr;
        DoubleDouble ceri;
        DoubleDouble er;

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
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */

        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));

        // DoubleDoubleDigits is I1MACH(14), the number of base-B digits for DoubleDouble
        // precision. IEEE 754 DoubleDouble precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // In DoubleDouble, the mantissa effectively has 106 bits of resolution.
        DoubleDoubleDigits = 106;
        k1 = DoubleDoubleDigits - 1;
        aa = r1m5.multiply(DoubleDouble.valueOf(k1));
        dig = aa.min(DoubleDouble.valueOf(18.0));
        aa = (DoubleDouble.valueOf(2.303)).multiply(aa);
        alim = elim.add( (aa.negate()).max(DoubleDouble.valueOf(-41.45)));
        rl = ((DoubleDouble.valueOf(1.2)).multiply(dig)).add(DoubleDouble.valueOf(3.0));
        fnul = (DoubleDouble.valueOf(10.0)).add((DoubleDouble.valueOf(6.0)).multiply(dig.subtract(DoubleDouble.valueOf(3.0))));
        slak = (DoubleDouble.valueOf(3.0)).add((DoubleDouble.valueOf(4.0)).
        		multiply( ( (DoubleDouble.valueOf(-0.434294481903251)).multiply(tol.log())).subtract(DoubleDouble.valueOf(7.0))).
        		divide(DoubleDouble.valueOf(11.0)));
        slak = slak.max(DoubleDouble.valueOf(3.0));
        ertol = tol.multiply((DoubleDouble.valueOf(10.0)).pow(slak));
        rm = (DoubleDouble.valueOf(0.5)).multiply(alim.add(elim));
        rm = rm.min(DoubleDouble.valueOf(200.0));
        rm = rm.max(rl.add(DoubleDouble.valueOf(10.0)));
        r2 = rm.min(fnul);
        Preferences.debug("Quick check routine for the J Bessel function from ZBESJ\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("elim = " + elim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("alim = " + alim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rl = " + rl + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("fnul = " + fnul + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("dig = " + dig + "\n", Preferences.DEBUG_ALGORITHM);
        atol = (DoubleDouble.valueOf(100.0)).multiply(tol);
        conr = DoubleDouble.valueOf(0.0);
        coni = (DoubleDouble.valueOf(-1.0)).divide(hpi);
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
            xnu[0] = DoubleDouble.valueOf(0.0);
            xnu[1] = DoubleDouble.valueOf(1.0);
            xnu[2] = DoubleDouble.valueOf(2.0);
            xnu[3] = (DoubleDouble.valueOf(0.5)).multiply(fnul);
            xnu[4] = fnul.add(DoubleDouble.valueOf(1.1));
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
            xnu[0] = DoubleDouble.valueOf(0.0);
            xnu[1] = DoubleDouble.valueOf(0.6);
            xnu[2] = DoubleDouble.valueOf(1.3);
            xnu[3] = DoubleDouble.valueOf(2.0);
            xnu[4] = (DoubleDouble.valueOf(0.5)).multiply(fnul);
            xnu[5] = fnul.add(DoubleDouble.valueOf(1.1));
        } // else mqc == 2

        i = 2;
        eps = DoubleDouble.valueOf(0.01);
        film = DoubleDouble.valueOf(il - 1.0);
        t[0] = ((DoubleDouble.PI).negate()).add(eps);

        for (k = 2; k <= il; k++) {

            if (kdo[k - 1] == 0) {
                t[i - 1] = ((DoubleDouble.PI).multiply(DoubleDouble.valueOf( -il + (2.0 * k) - 1.0))).divide(film);

                if (keps[k - 1] != 0) {
                    ts = (DoubleDouble)t[i - 1].clone();
                    t[i - 1] = ts.subtract(eps);
                    i = i + 1;
                    t[i - 1] = ts.add(eps);
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
                    fnu = (DoubleDouble)xnu[nu - 1].clone();
                    gnu = fnu.add(DoubleDouble.valueOf(n));
                    gnu = gnu.sqrt();
                    gnu = gnu.min((DoubleDouble.valueOf(0.5)).multiply(rl));

                    group: for (icase = 1; icase <= 3; icase++) {
                        irb = Math.min(2, icase);

                        for (ir = irb; ir <= 4; ir++) {

                            if (icase == 1) {
                            	 r = ( (gnu.multiply(DoubleDouble.valueOf(4.0 - ir))).
                            			 add(DoubleDouble.valueOf(2.0 * (ir - 1.0)))).divide(DoubleDouble.valueOf(3.0));
                            } // if (icase == 1)
                            else if (icase == 2) {
                                r = ( (DoubleDouble.valueOf(2.0 * (4.0 - ir))).
                                		add(r2.multiply(DoubleDouble.valueOf(ir - 1.0)))).divide(DoubleDouble.valueOf(3.0));
                            } // else if (icase == 2)
                            else if ( (icase == 3) && (r2.ge(rm))) {
                                continue group;
                            } // else if ((icase == 3) && (r2 >= rm))
                            else {
                                r = ( (r2.multiply(DoubleDouble.valueOf(4.0 - ir))).
                                		add(rm.multiply(DoubleDouble.valueOf(ir - 1.0)))).divide(DoubleDouble.valueOf(3.0));
                            } // else

                            for (it = 1; it <= itl; it++) {
                                ct = t[it - 1].cos();
                                st = t[it - 1].sin();

                                if ((ct.abs()).lt(atol)) {
                                    ct = DoubleDouble.valueOf(0.0);
                                } // if Math.abs(ct) < atol)

                                if ((st.abs()).lt(atol)) {
                                    st = DoubleDouble.valueOf(0.0);
                                } // if (Math.abs(st) < atol)

                                zr = r.multiply(ct);
                                zi = r.multiply(st);

                                if ( (zr.equals(DoubleDouble.valueOf(0.0))) && (zi.equals(DoubleDouble.valueOf(0.0)))) {
                                    continue;
                                } // if ((zr == 0.0) && (zi == 0.0))

                                zdiv(conr, coni, zr, zi, wrr, wri);
                                m = 1;

                                if (zi.lt(DoubleDouble.valueOf(0.0))) {
                                    m = 2;
                                    wrr[0] = wrr[0].negate();
                                    wri[0] = wri[0].negate();
                                } // if (zi < 0.0)

                                zbesj(zr, zi, fnu, kode, np, cjr, cji, nzj, ierrj);
                                zbesh(zr, zi, fnu, kode, m, np, chr, chi, nzh, ierrh);

                                if ( (nzj[0] != 0) || (nzh[0] != 0) || (ierrj[0] != 0) || (ierrh[0] != 0)) {
                                    continue;
                                }

                                if (kode == 2) {
                                    sgn = DoubleDouble.valueOf(3.0 - (2.0 * m));
                                    str = zr.cos();
                                    sti = (sgn.negate()).multiply(zr.sin());
                                    t1r = (wrr[0].multiply(str)).subtract(wri[0].multiply(sti));
                                    wri[0] = (wrr[0].multiply(sti)).add(wri[0].multiply(str));
                                    wrr[0] = (DoubleDouble)t1r.clone();
                                } // if (kode == 2)

                                kk = 0;
                                mflg = 0;

                                for (i = 1; i <= n; i++) {
                                    str = (cjr[i - 1].multiply(chr[i])).subtract(cji[i - 1].multiply(chi[i]));
                                    t1i = (cjr[i - 1].multiply(chi[i])).add(cji[i - 1].multiply(chr[i]));
                                    t1r = (DoubleDouble)str.clone();
                                    str = (cjr[i].multiply(chr[i - 1])).subtract(cji[i].multiply(chi[i - 1]));
                                    t2i = (cjr[i].multiply(chi[i - 1])).add(cji[i].multiply(chr[i - 1]));
                                    t2r = (DoubleDouble)str.clone();
                                    cerr = (t1r.subtract(t2r)).subtract(wrr[0]);
                                    ceri = (t1i.subtract(t2i)).subtract(wri[0]);
                                    er = (zabs(cerr, ceri)).divide(zabs(wrr[0], wri[0]));

                                    if ( (er.gt(ertol)) && (mflg == 0)) {
                                        mflg = 1;
                                        kk = i;
                                    } // if ((er > ertol) && (mflg == 0))

                                    aer[i - 1] = (DoubleDouble)er.clone();
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
        int k;
        int k1;
        DoubleDouble aa;
        DoubleDouble dig;
        DoubleDouble slak;
        DoubleDouble ertol;
        DoubleDouble rm;
        DoubleDouble r2;
        DoubleDouble atol;
        DoubleDouble hpi = DoubleDouble.PI_2;
        int nL;
        int il;
        final DoubleDouble[] t = new DoubleDouble[20];
        final DoubleDouble[] aer = new DoubleDouble[25];
        final DoubleDouble[] xnu = new DoubleDouble[20];
        final DoubleDouble[] vr = new DoubleDouble[20];
        final DoubleDouble[] vi = new DoubleDouble[20];
        final DoubleDouble[] yr = new DoubleDouble[20];
        final DoubleDouble[] yi = new DoubleDouble[20];
        final DoubleDouble[] wr = new DoubleDouble[20];
        final DoubleDouble[] wi = new DoubleDouble[20];
        final int[] keps = new int[20];
        final int[] kdo = new int[20];
        final DoubleDouble[] cipr = new DoubleDouble[] {DoubleDouble.valueOf(1.0), DoubleDouble.valueOf(0.0), 
        		DoubleDouble.valueOf(-1.0), DoubleDouble.valueOf(0.0)};
        final DoubleDouble[] cipi = new DoubleDouble[] {DoubleDouble.valueOf(0.0), DoubleDouble.valueOf(1.0), 
        		DoubleDouble.valueOf(0.0), DoubleDouble.valueOf(-1.0)};
        int i;
        int nul;
        DoubleDouble eps;
        DoubleDouble film;
        DoubleDouble ts;
        int itl;
        int lflg;
        int n;
        int nONE;
        int nu;
        DoubleDouble fnu;
        int ifnu;
        DoubleDouble ffnu;
        DoubleDouble arg;
        DoubleDouble csgnr;
        DoubleDouble csgni;
        int i4;
        DoubleDouble str;
        int kode;
        int icase;
        int irb;
        int ir;
        DoubleDouble r;
        int it;
        DoubleDouble ct;
        DoubleDouble st;
        DoubleDouble zr;
        DoubleDouble zi;
        final int[] nz = new int[1];
        final int[] ierr = new int[1];
        final DoubleDouble[] cvr = new DoubleDouble[1];
        final DoubleDouble[] cvi = new DoubleDouble[1];
        int mflg;
        int kk;
        DoubleDouble er;
        DoubleDouble zzr;
        DoubleDouble zzi;
        DoubleDouble zrr;
        DoubleDouble zri;
        int m;
        DoubleDouble coei;
        DoubleDouble c1r;
        DoubleDouble c1i;
        DoubleDouble c2r;
        DoubleDouble c2i;
        DoubleDouble c3r;
        DoubleDouble c3i;
        DoubleDouble c4r;
        DoubleDouble c4i;
        DoubleDouble ab;
        DoubleDouble cyr;
        DoubleDouble cyi;
        DoubleDouble sti;
        DoubleDouble cwr;
        DoubleDouble cwi;

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
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */

        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));

        // DoubleDoubleDigits is I1MACH(14), the number of base-B digits for DoubleDouble
        // precision. IEEE 754 DoubleDouble precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // In DoubleDouble, the mantissa effectively has 106 bits of resolution.
        DoubleDoubleDigits = 106;
        k1 = DoubleDoubleDigits - 1;
        aa = r1m5.multiply(DoubleDouble.valueOf(k1));
        dig = aa.min(DoubleDouble.valueOf(18.0));
        aa = (DoubleDouble.valueOf(2.303)).multiply(aa);
        alim = elim.add( (aa.negate()).max(DoubleDouble.valueOf(-41.45)));
        rl = ((DoubleDouble.valueOf(1.2)).multiply(dig)).add(DoubleDouble.valueOf(3.0));
        fnul = (DoubleDouble.valueOf(10.0)).add((DoubleDouble.valueOf(6.0)).multiply(dig.subtract(DoubleDouble.valueOf(3.0))));
        slak = (DoubleDouble.valueOf(3.0)).add((DoubleDouble.valueOf(4.0)).
        		multiply( ( (DoubleDouble.valueOf(-0.434294481903251)).multiply(tol.log())).subtract(DoubleDouble.valueOf(7.0))).
        		divide(DoubleDouble.valueOf(11.0)));
        slak = slak.max(DoubleDouble.valueOf(3.0));
        ertol = tol.multiply((DoubleDouble.valueOf(10.0)).pow(slak));
        rm = (DoubleDouble.valueOf(0.5)).multiply(alim.add(elim));
        rm = rm.min(DoubleDouble.valueOf(200.0));
        rm = rm.max(rl.add(DoubleDouble.valueOf(10.0)));
        r2 = rm.min(fnul);
        Preferences.debug("QUICK CHECK ROUTINE FOR THE K BESSEL FUNCTION FROM\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("ZBESK and ZHESK\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("elim = " + elim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("alim = " + alim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rl = " + rl + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("fnul = " + fnul + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("dig = " + dig + "\n", Preferences.DEBUG_ALGORITHM);
        atol = (DoubleDouble.valueOf(100.0)).multiply(tol);
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
            xnu[0] = DoubleDouble.valueOf(0.0);
            xnu[1] = DoubleDouble.valueOf(1.0);
            xnu[2] = DoubleDouble.valueOf(2.0);
            xnu[3] = (DoubleDouble.valueOf(0.5)).multiply(fnul);
            xnu[4] = fnul.add(DoubleDouble.valueOf(1.1));
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
            xnu[0] = DoubleDouble.valueOf(0.0);
            xnu[1] = DoubleDouble.valueOf(0.6);
            xnu[2] = DoubleDouble.valueOf(1.3);
            xnu[3] = DoubleDouble.valueOf(2.0);
            xnu[4] = (DoubleDouble.valueOf(0.5)).multiply(fnul);
            xnu[5] = fnul.add(DoubleDouble.valueOf(1.1));
        } // else mqc == 2

        i = 2;
        eps = DoubleDouble.valueOf(0.01);
        film = DoubleDouble.valueOf(il - 1.0);
        t[0] = ((DoubleDouble.PI).negate()).add(eps);

        for (k = 2; k <= il; k++) {

            if (kdo[k - 1] == 0) {
                t[i - 1] = (DoubleDouble.PI).multiply(DoubleDouble.valueOf( -il + (2.0 * k) - 1.0)).divide(film);

                if (keps[k - 1] != 0) {
                    ts = (DoubleDouble)t[i - 1].clone();
                    t[i - 1] = ts.subtract(eps);
                    i = i + 1;
                    t[i - 1] = ts.add(eps);
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
                    fnu = (DoubleDouble)xnu[nu - 1].clone();
                    ifnu = fnu.intValue();
                    ffnu = fnu.subtract(DoubleDouble.valueOf(ifnu));
                    arg = hpi.multiply(ffnu);
                    csgnr = arg.cos();
                    csgni = arg.sin();
                    i4 = (ifnu % 4) + 1;
                    str = (csgnr.multiply(cipr[i4 - 1])).subtract(csgni.multiply(cipi[i4 - 1]));
                    csgni = (csgnr.multiply(cipi[i4 - 1])).add(csgni.multiply(cipr[i4 - 1]));
                    csgnr = (DoubleDouble)str.clone();

                    for (icase = 1; icase <= 3; icase++) {
                        irb = Math.min(2, icase);

                        for (ir = irb; ir <= 4; ir++) {

                            if (icase == 1) {
                                r = ( ((DoubleDouble.valueOf(0.2)).multiply(DoubleDouble.valueOf(4.0 - ir))).
                                		add((DoubleDouble.valueOf(2.0)).multiply(DoubleDouble.valueOf(ir - 1.0)))).
                                		divide(DoubleDouble.valueOf(3.0));
                            } // if (icase == 1)
                            else if (icase == 2) {
                                r = ( ((DoubleDouble.valueOf(2.0)).multiply(DoubleDouble.valueOf(4.0 - ir))).
                                		add(r2.multiply(DoubleDouble.valueOf(ir - 1.0)))).divide(DoubleDouble.valueOf(3.0));
                            } // else if (icase == 2)
                            else if ( (icase == 3) && (r2 == rm)) {
                                continue group;
                            } // else if ((icase == 3) && (r2 == rm))
                            else { // icase == 3
                                r = ( (r2.multiply(DoubleDouble.valueOf(4.0 - ir))).
                                		add(rm.multiply(DoubleDouble.valueOf(ir - 1.0)))).divide(DoubleDouble.valueOf(3.0));
                            } // else icase == 3

                            group2: for (it = 1; it <= itl; it++) {
                                ct = t[it - 1].cos();
                                st = t[it - 1].sin();

                                if ((ct.abs()).lt(atol)) {
                                    ct = DoubleDouble.valueOf(0.0);
                                } // if (Math.abs(ct) < atol)

                                if ((st.abs()).lt(atol)) {
                                    st = DoubleDouble.valueOf(0.0);
                                } // if (Math.abs(st) < atol)

                                zr = r.multiply(ct);
                                zi = r.multiply(st);

                                if (zr.ge(DoubleDouble.valueOf(0.0))) {

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
                                    zdiv(DoubleDouble.valueOf(1.0), DoubleDouble.valueOf(0.0), zr, zi, cvr, cvi);

                                    if (kode == 2) {
                                        str = zi.cos();
                                        sti = zi.sin();
                                        aa = (str.multiply(cvr[0])).subtract(sti.multiply(cvi[0]));
                                        cvi[0] = (str.multiply(cvi[0])).add(sti.multiply(cvr[0]));
                                        cvr[0] = (DoubleDouble)aa.clone();
                                    } // if (kode == 2)

                                    mflg = 0;
                                    kk = 0;

                                    for (i = 1; i <= n; i++) {
                                        cwr = (wr[i - 1].multiply(yr[i])).subtract(wi[i - 1].multiply(yi[i]));
                                        cwi = (wr[i - 1].multiply(yi[i])).add(wi[i - 1].multiply(yr[i]));
                                        cyr = (yr[i - 1].multiply(wr[i])).subtract(yi[i - 1].multiply(wi[i]));
                                        cyi = (yr[i - 1].multiply(wi[i])).add(yi[i - 1].multiply(wr[i]));
                                        cyr = (cyr.add(cwr)).subtract(cvr[0]);
                                        cyi = (cyi.add(cwi)).subtract(cvi[0]);
                                        er = (zabs(cyr, cyi)).divide(zabs(cvr[0], cvi[0]));
                                        aer[i - 1] = (DoubleDouble)er.clone();

                                        if ( (er.gt(ertol)) && (kk == 0)) {
                                            mflg = 1;
                                            kk = i;
                                        } // if (er > ertol) && (kk == 0))
                                    } // for (i = 1; i <= n; i++)
                                } // if (zr >= 0.0)
                                else { // zr < 0.0

                                    // Analytic continuation formula checks for left half
                                    // plane in terms of H(fnu,1,z) and H(fnu,2,z)
                                    zzr = (DoubleDouble)zr.clone();
                                    zzi = (DoubleDouble)zi.clone();

                                    if (zi.lt(DoubleDouble.valueOf(0.0))) {
                                        zzi = zzi.negate();
                                    } // if (zi < 0.0)

                                    zrr = zzi.negate();
                                    zri = (DoubleDouble)zzr.clone();
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

                                    coei = hpi.negate();
                                    mflg = 0;
                                    kk = 0;
                                    aa = (DoubleDouble.valueOf(2.0)).multiply(((DoubleDouble.PI).multiply(ffnu)).cos());

                                    if ( (ifnu % 2) != 0) {
                                        aa = aa.negate();
                                    } // if ((ifnu%2) != 0)

                                    c1r = (DoubleDouble)aa.clone();
                                    c1i = DoubleDouble.valueOf(0.0);
                                    c2r = (DoubleDouble)csgnr.clone();
                                    c2i = (DoubleDouble)csgni.clone();

                                    for (i = 1; i <= n; i++) {
                                        c3r = (DoubleDouble)c1r.clone();
                                        c3i = (DoubleDouble)c1i.clone();
                                        c4r = (DoubleDouble)c2r.clone();
                                        c4i = (DoubleDouble)c2i.clone();

                                        if (kode == 2) {

                                            // Adjustments to coefficients due to scaling of
                                            // H(fnu,1,z) and H(fnu,2,z) functions on kode = 2.
                                            ab = zabs(vr[i - 1], vi[i - 1]);
                                            aa = ((ab.log()).add(zr)).add(zr);

                                            if (aa.gt(elim)) {
                                                continue group2;
                                            } // if (aa > elim)

                                            if (aa.lt(elim.negate())) {
                                                c3r = DoubleDouble.valueOf(0.0);
                                                c3i = DoubleDouble.valueOf(0.0);
                                            } // if (aa < -elim)
                                            else { // aa >= -elim
                                                str = zzr.add(zzr);
                                                sti = zzi.add(zzi);
                                                zexp(str, sti, cvr, cvi);
                                                str = (c3r.multiply(cvr[0])).subtract(c3i.multiply(cvi[0]));
                                                c3i = (c3r.multiply(cvi[0])).add(c3i.multiply(cvr[0]));
                                                c3r = (DoubleDouble)str.clone();
                                            } // else aa >= -elim
                                        } // if (kode == 2)

                                        str = (c3r.multiply(c2r)).add(c3i.multiply(c2i));
                                        sti = ( (c3r.negate()).multiply(c2i)).add(c3i.multiply(c2r));
                                        cyr = (str.multiply(vr[i - 1])).subtract(sti.multiply(vi[i - 1]));
                                        cyi = (str.multiply(vi[i - 1])).add(sti.multiply(vr[i - 1]));
                                        cyr = (cyr.add(c4r.multiply(wr[i - 1]))).subtract(c4i.multiply(wi[i - 1]));
                                        cyi = (cyi.add(c4r.multiply(wi[i - 1]))).add(c4i.multiply(wr[i - 1]));
                                        str = (cyi.negate()).multiply(coei);
                                        cyi = cyr.multiply(coei);
                                        cyr = (DoubleDouble)str.clone();

                                        if (zi.lt(DoubleDouble.valueOf(0.0))) {
                                            cyi = cyi.negate();
                                        } // if (zi < 0.0)

                                        str = cyr.subtract(yr[i - 1]);
                                        sti = cyi.subtract(yi[i - 1]);
                                        er = (zabs(str, sti)).divide(zabs(yr[i - 1], yi[i - 1]));
                                        aer[i - 1] = (DoubleDouble)er.clone();

                                        if ( (er.gt(ertol)) && (kk == 0)) {
                                            mflg = 1;
                                            kk = i;
                                        } // if (er > ertol) && (kk == 0)

                                        str = c2i.negate();
                                        c2i = (DoubleDouble)c2r.clone();
                                        c2r = (DoubleDouble)str.clone();
                                        c1r = c1r.negate();
                                        c1i = c1i.negate();
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
        int k;
        int k1;
        DoubleDouble aa;
        DoubleDouble dig;
        DoubleDouble slak;
        DoubleDouble ertol;
        DoubleDouble rm;
        DoubleDouble r2;
        DoubleDouble atol;
        int nL;
        int il;
        final DoubleDouble[] t = new DoubleDouble[20];
        final DoubleDouble[] aer = new DoubleDouble[20];
        final DoubleDouble[] xnu = new DoubleDouble[20];
        final DoubleDouble[] wr = new DoubleDouble[20];
        final DoubleDouble[] wi = new DoubleDouble[20];
        final DoubleDouble[] vr = new DoubleDouble[20];
        final DoubleDouble[] vi = new DoubleDouble[20];
        final DoubleDouble[] cwrkr = new DoubleDouble[20];
        final DoubleDouble[] cwrki = new DoubleDouble[20];
        final int[] kdo = new int[20];
        final int[] keps = new int[20];
        int i;
        int nul;
        DoubleDouble eps;
        DoubleDouble film;
        DoubleDouble ts;
        int itl;
        int lflg;
        int kode;
        int n;
        int nu;
        DoubleDouble fnu;
        int ifnu;
        DoubleDouble ffnu;
        int icase;
        int irb;
        int ir;
        DoubleDouble r;
        int it;
        DoubleDouble ct;
        DoubleDouble st;
        DoubleDouble zr;
        DoubleDouble zi;
        final int[] nz2 = new int[1];
        final int[] ierr = new int[1];
        final int[] nz1 = new int[1];
        int mflg;
        DoubleDouble ab;
        DoubleDouble cwr;
        DoubleDouble cwi;
        DoubleDouble av;
        DoubleDouble er;
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
        // epsilon is called the largest relative spacing
        epsilon = DoubleDouble.valueOf(1.23259516440783e-32);  /* = 2^-106 */

        tol = epsilon.max(DoubleDouble.valueOf(1.0e-18));

        // emin, the smallest exponent E for DoubleDouble precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        //tiny = (DoubleDouble.valueOf(2.0)).pow(-1022.0);
        tiny = DoubleDouble.valueOf(2.2250738585072014E-308);

        // emax, the largest exponent E for DoubleDouble precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-DoubleDoubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = DoubleDouble.valueOf(0.30102999566398118760076720571112);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = (DoubleDouble.valueOf(2.303)).multiply( ((DoubleDouble.valueOf(k)).multiply(r1m5)).subtract(DoubleDouble.valueOf(3.0)));

        // DoubleDoubleDigits is I1MACH(14), the number of base-B digits for DoubleDouble
        // precision. IEEE 754 DoubleDouble precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // In DoubleDouble, the mantissa effectively has 106 bits of resolution.
        DoubleDoubleDigits = 106;
        k1 = DoubleDoubleDigits - 1;
        aa = r1m5.multiply(DoubleDouble.valueOf(k1));
        dig = aa.min(DoubleDouble.valueOf(18.0));
        aa = (DoubleDouble.valueOf(2.303)).multiply(aa);
        alim = elim.add( (aa.negate()).max(DoubleDouble.valueOf(-41.45)));
        rl = ((DoubleDouble.valueOf(1.2)).multiply(dig)).add(DoubleDouble.valueOf(3.0));
        fnul = (DoubleDouble.valueOf(10.0)).add((DoubleDouble.valueOf(6.0)).multiply(dig.subtract(DoubleDouble.valueOf(3.0))));
        slak = (DoubleDouble.valueOf(3.0)).add((DoubleDouble.valueOf(4.0)).
        		multiply( ( (DoubleDouble.valueOf(-0.434294481903251)).multiply(tol.log())).subtract(DoubleDouble.valueOf(7.0))).
        		divide(DoubleDouble.valueOf(11.0)));
        slak = slak.max(DoubleDouble.valueOf(3.0));
        ertol = tol.multiply((DoubleDouble.valueOf(10.0)).pow(slak));
        rm = (DoubleDouble.valueOf(0.5)).multiply(alim.add(elim));
        rm = rm.min(DoubleDouble.valueOf(200.0));
        rm = rm.max(rl.add(DoubleDouble.valueOf(10.0)));
        r2 = rm.min(fnul);
        Preferences.debug("Quick check routine for the Y Bessel function from ZBESY\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("elim = " + elim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("alim = " + alim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rl = " + rl + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("fnul = " + fnul + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("dig = " + dig + "\n", Preferences.DEBUG_ALGORITHM);
        atol = (DoubleDouble.valueOf(100.0)).multiply(tol);
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
            xnu[0] = DoubleDouble.valueOf(0.0);
            xnu[1] = DoubleDouble.valueOf(1.0);
            xnu[2] = DoubleDouble.valueOf(2.0);
            xnu[3] = (DoubleDouble.valueOf(0.5)).multiply(fnul);
            xnu[4] = fnul.add(DoubleDouble.valueOf(1.2));
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
            xnu[0] = DoubleDouble.valueOf(0.0);
            xnu[1] = DoubleDouble.valueOf(0.6);
            xnu[2] = DoubleDouble.valueOf(1.3);
            xnu[3] = DoubleDouble.valueOf(2.0);
            xnu[4] = (DoubleDouble.valueOf(0.5)).multiply(fnul);
            xnu[5] = fnul.add(DoubleDouble.valueOf(1.2));
        } // else mqc == 2

        i = 2;
        eps = DoubleDouble.valueOf(0.01);
        film = DoubleDouble.valueOf(il - 1.0);
        t[0] =((DoubleDouble.PI).negate()).add(eps);

        for (k = 2; k <= il; k++) {

            if (kdo[k - 1] == 0) {
                t[i - 1] = (DoubleDouble.PI).multiply(DoubleDouble.valueOf( -il + (2.0 * k) - 1.0)).divide(film);

                if (keps[k - 1] != 0) {
                    ts = (DoubleDouble)t[i - 1].clone();
                    t[i - 1] = ts.subtract(eps);
                    i = i + 1;
                    t[i - 1] = ts.add(eps);
                } // if (keps[k-1] != 0)

                i = i + 1;
            } // if (kdo[k-1] == 0)
        } // for (k = 2; k <= il; k++)

        itl = i - 1;
        lflg = 0;

        for (kode = 1; kode <= 2; kode++) {

            for (n = 1; n <= nL; n++) {

                for (nu = 1; nu <= nul; nu++) {
                    fnu = (DoubleDouble)xnu[nu - 1].clone();
                    ifnu = fnu.intValue();
                    ffnu = fnu.subtract(DoubleDouble.valueOf(ifnu));

                    group: for (icase = 1; icase <= 3; icase++) {
                        irb = Math.min(2, icase);

                        for (ir = irb; ir <= 4; ir++) {

                            if (icase == 1) {
                                r = ( (eps.multiply(DoubleDouble.valueOf(4.0 - ir))).
                                		add((DoubleDouble.valueOf(2.0)).multiply(DoubleDouble.valueOf(ir - 1.0)))).
                                		divide(DoubleDouble.valueOf(3.0));
                            } // if (icase == 1)
                            else if (icase == 2) {
                                r = ( ((DoubleDouble.valueOf(2.0)).multiply(DoubleDouble.valueOf(4.0 - ir))).
                                		add(r2.multiply(DoubleDouble.valueOf(ir - 1.0)))).divide(DoubleDouble.valueOf(3.0));
                            } // else if (icase == 2)
                            else if ( (icase == 3) && (rm == r2)) {
                                continue group;
                            } // else if ((icase == 3) && (rm == r2))
                            else {
                                r = ( (r2.multiply(DoubleDouble.valueOf(4.0 - ir))).
                                		add(rm.multiply(DoubleDouble.valueOf(ir - 1.0)))).divide(DoubleDouble.valueOf(3.0));
                            } // else

                            for (it = 1; it <= itl; it++) {
                                ct = t[it - 1].cos();
                                st = t[it - 1].sin();

                                if ((ct.abs()).lt(atol)) {
                                    ct = DoubleDouble.valueOf(0.0);
                                } // if (Math.abs(ct) < atol)

                                if ((st.abs()).lt(atol)) {
                                    st = DoubleDouble.valueOf(0.0);
                                } // if (Math.abs(st) < atol)

                                zr = r.multiply(ct);
                                zi = r.multiply(st);
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
                                    ab = fnu.add(DoubleDouble.valueOf(i - 1.0));
                                    aa = (DoubleDouble.valueOf(0.5)).max(ab);
                                    cwr = wr[i - 1].subtract(vr[i - 1]);
                                    cwi = wi[i - 1].subtract(vi[i - 1]);
                                    av = zabs(vr[i - 1], vi[i - 1]);
                                    er = zabs(cwr, cwi);

                                    if (av.ne(DoubleDouble.valueOf(0.0))) {

                                        if (zi.equals(DoubleDouble.valueOf(0.0))) {

                                            if (zr.gt(DoubleDouble.valueOf(0.0))) {

                                                if ((zr.abs()).lt(aa)) {
                                                    er = er.divide(av);
                                                } // if (Math.abs(zr) < aa)
                                            } // if (zr > 0.0)
                                            else { // zr <= 0.0

                                                if (((ffnu.subtract(DoubleDouble.valueOf(0.5))).abs()).lt(DoubleDouble.valueOf(0.125))) {

                                                    if ((zr.abs()).lt(aa)) {
                                                        er = er.divide(av);
                                                    } // if Math.abs(zr) < aa)
                                                } // if (Math.abs(ffnu - 0.5) < 0.125)
                                                else { // Math.abs(ffnu - 0.5) >= 0.125) {
                                                    er = er.divide(av);
                                                } // else Math.abs(ffnu - 0.5) >= 0.125)
                                            } // else zr <= 0.0
                                        } // if (zi == 0.0)
                                        else { // (zi != 0.0)
                                            er = er.divide(av);
                                        } // else (zi != 0.0)
                                    } // if (av != 0.0)

                                    aer[i - 1] = (DoubleDouble)er.clone();

                                    if (er.gt(ertol)) {
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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param n int
     * @param cyr DoubleDouble[]
     * @param cyi DoubleDouble[]
     */
    private void zrati(final DoubleDouble zr, final DoubleDouble zi, final DoubleDouble fnu, final int n, final DoubleDouble[] cyr,
            final DoubleDouble[] cyi) {
        final DoubleDouble rt2 = (DoubleDouble.valueOf(2.0)).sqrt();
        DoubleDouble az;
        int inu;
        int idnu;
        int magz;
        DoubleDouble amagz;
        DoubleDouble fdnu;
        DoubleDouble fnup;
        int id;
        int itime;
        int k;
        DoubleDouble ptr;
        DoubleDouble rzr;
        DoubleDouble rzi;
        DoubleDouble t1r;
        DoubleDouble t1i;
        DoubleDouble p2r;
        DoubleDouble p2i;
        DoubleDouble p1r;
        DoubleDouble p1i;
        DoubleDouble ap2;
        DoubleDouble ap1;
        DoubleDouble arg;
        DoubleDouble test1;
        DoubleDouble test;
        DoubleDouble rap1;
        DoubleDouble ak;
        DoubleDouble flam;
        DoubleDouble rho;
        int kk;
        DoubleDouble dfnu;
        DoubleDouble pti;
        DoubleDouble ttr;
        DoubleDouble tti;
        final DoubleDouble[] cyrnm1 = new DoubleDouble[1];
        final DoubleDouble[] cyinm1 = new DoubleDouble[1];
        DoubleDouble cdfnur;
        DoubleDouble cdfnui;
        DoubleDouble rak;
        int i;

        az = zabs(zr, zi);
        inu = fnu.intValue();
        idnu = inu + n - 1;
        magz = az.intValue();
        amagz = DoubleDouble.valueOf(magz + 1.0);
        fdnu = DoubleDouble.valueOf(idnu);
        fnup = amagz.max(fdnu);
        id = idnu - magz - 1;
        itime = 1;
        k = 1;
        ptr = az.reciprocal();
        rzr = (ptr.multiply(zr.add(zr))).multiply(ptr);
        rzi = ((ptr.negate()).multiply(zi.add(zi))).multiply(ptr);
        t1r = rzr.multiply(fnup);
        t1i = rzi.multiply(fnup);
        p2r = t1r.negate();
        p2i = t1i.negate();
        p1r = DoubleDouble.valueOf(1.0);
        p1i = DoubleDouble.valueOf(0.0);
        t1r = t1r.add(rzr);
        t1i = t1i.add(rzi);

        if (id > 0) {
            id = 0;
        } // if (id > 0)

        ap2 = zabs(p2r, p2i);
        ap1 = zabs(p1r, p1i);

        // The overflow test on K(fnu+i-1,z) before the call to cbknu
        // guarantees that p2 is on scale. Scale test1 and all subsequent
        // p2 values by ap1 to ensure that an overflow does not occur
        // prematurely.
        arg = (ap2.add(ap2)).divide(ap1.multiply(tol));
        test1 = arg.sqrt();
        test = (DoubleDouble)test1.clone();
        rap1 = ap1.reciprocal();
        p1r = p1r.multiply(rap1);
        p1i = p1i.multiply(rap1);
        p2r = p2r.multiply(rap1);
        p2i = p2i.multiply(rap1);
        ap2 = ap2.multiply(rap1);

        while (true) {
            k = k + 1;
            ap1 = (DoubleDouble)ap2.clone();
            ptr = (DoubleDouble)p2r.clone();
            pti = (DoubleDouble)p2i.clone();
            p2r = p1r.subtract( (t1r.multiply(ptr)).subtract(t1i.multiply(pti)));
            p2i = p1i.subtract( (t1r.multiply(pti)).add(t1i.multiply(ptr)));
            p1r = (DoubleDouble)ptr.clone();
            p1i = (DoubleDouble)pti.clone();
            t1r = t1r.add(rzr);
            t1i = t1i.add(rzi);
            ap2 = zabs(p2r, p2i);

            if (ap1.le(test)) {
                continue;
            } // if (ap1 <= test)

            if (itime == 2) {
                break;
            } // if (itime == 2)

            ak = (DoubleDouble.valueOf(0.5)).multiply(zabs(t1r, t1i));
            flam = ak.add(( (ak.multiply(ak)).subtract(DoubleDouble.valueOf(1.0))).sqrt());
            rho = (ap2.divide(ap1)).min(flam);
            test = test1.multiply((rho.divide( (rho.multiply(rho)).subtract(DoubleDouble.valueOf(1.0)))).sqrt());
            itime = 2;
        } // while (true)

        kk = k + 1 - id;
        ak = DoubleDouble.valueOf(kk);
        t1r = (DoubleDouble)ak.clone();
        t1i = DoubleDouble.valueOf(0.0);
        dfnu = fnu.add(DoubleDouble.valueOf(n - 1.0));
        p1r = ap2.reciprocal();
        p1i = DoubleDouble.valueOf(0.0);
        p2r = DoubleDouble.valueOf(0.0);
        p2i = DoubleDouble.valueOf(0.0);

        for (i = 1; i <= kk; i++) {
            ptr = (DoubleDouble)p1r.clone();
            pti = (DoubleDouble)p1i.clone();
            rap1 = dfnu.add(t1r);
            ttr = rzr.multiply(rap1);
            tti = rzi.multiply(rap1);
            p1r = ( (ptr.multiply(ttr)).subtract(pti.multiply(tti))).add(p2r);
            p1i = ( (ptr.multiply(tti)).add(pti.multiply(ttr))).add(p2i);
            p2r = (DoubleDouble)ptr.clone();
            p2i = (DoubleDouble)pti.clone();
            t1r = t1r.subtract(DoubleDouble.valueOf(1.0));
        } // for ( i = 1; i <= kk; i++)

        if ( (p1r.equals(DoubleDouble.valueOf(0.0))) && (p1i.equals(DoubleDouble.valueOf(0.0)))) {
            p1r = (DoubleDouble)tol.clone();
            p1i = (DoubleDouble)tol.clone();
        } // if ((p1r == 0.0) && (p1i == 0.0))

        zdiv(p2r, p2i, p1r, p1i, cyrnm1, cyinm1);
        cyr[n - 1] = (DoubleDouble)cyrnm1[0].clone();
        cyi[n - 1] = (DoubleDouble)cyinm1[0].clone();

        if (n == 1) {
            return;
        } // if (n == 1)

        k = n - 1;
        ak = DoubleDouble.valueOf(k);
        t1r = (DoubleDouble)ak.clone();
        t1i = DoubleDouble.valueOf(0.0);
        cdfnur = fnu.multiply(rzr);
        cdfnui = fnu.multiply(rzi);

        for (i = 2; i <= n; i++) {
            ptr = (cdfnur.add( (t1r.multiply(rzr)).subtract(t1i.multiply(rzi)))).add(cyr[k]);
            pti = (cdfnui.add( (t1r.multiply(rzi)).add(t1i.multiply(rzr)))).add(cyi[k]);
            ak = zabs(ptr, pti);

            if (ak.equals(DoubleDouble.valueOf(0.0))) {
                ptr = (DoubleDouble)tol.clone();
                pti = (DoubleDouble)tol.clone();
                ak = tol.multiply(rt2);
            } // if (ak == 0.0)

            rak = ak.reciprocal();
            cyr[k - 1] = (rak.multiply(ptr)).multiply(rak);
            cyi[k - 1] = ((rak.negate()).multiply(pti)).multiply(rak);
            t1r = t1r.subtract(DoubleDouble.valueOf(1.0));
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
     * @param zrr DoubleDouble
     * @param zri DoubleDouble
     * @param s1r DoubleDouble[]
     * @param s1i DoubleDouble[]
     * @param s2r DoubleDouble[]
     * @param s2i DoubleDouble[]
     * @param nz int[]
     * @param ascle DoubleDouble
     * @param iuf int[]
     */
    private void zs1s2(DoubleDouble zrr, final DoubleDouble zri, final DoubleDouble[] s1r, final DoubleDouble[] s1i, final DoubleDouble[] s2r,
            final DoubleDouble[] s2i, final int[] nz, final DoubleDouble ascle, final int[] iuf) {
        DoubleDouble as1;
        DoubleDouble as2;
        DoubleDouble aln;
        DoubleDouble s1dr;
        DoubleDouble s1di;
        final DoubleDouble[] c1r = new DoubleDouble[1];
        final DoubleDouble[] c1i = new DoubleDouble[1];
        final int[] idum = new int[1];
        DoubleDouble aa;

        nz[0] = 0;
        as1 = zabs(s1r[0], s1i[0]);
        as2 = zabs(s2r[0], s2i[0]);

        if ( ( (s1r[0].ne(DoubleDouble.valueOf(0.0))) || (s1i[0].ne(DoubleDouble.valueOf(0.0)))) &&
        		(as1.ne(DoubleDouble.valueOf(0.0)))) {
            aln = ((zrr.negate()).subtract(zrr)).add(as1.log());
            s1dr = (DoubleDouble)s1r[0].clone();
            s1di = (DoubleDouble)s1i[0].clone();
            s1r[0] = DoubleDouble.valueOf(0.0);
            s1i[0] = DoubleDouble.valueOf(0.0);
            as1 = DoubleDouble.valueOf(0.0);

            if (aln.ge(alim.negate())) {
                zlog(s1dr, s1di, c1r, c1i, idum);
                c1r[0] = (c1r[0].subtract(zrr)).subtract(zrr);
                c1i[0] = (c1i[0].subtract(zri)).subtract(zri);
                zexp(c1r[0], c1i[0], s1r, s1i);
                as1 = zabs(s1r[0], s1i[0]);
                iuf[0] = iuf[0] + 1;
            } // if (aln >= (-alim))
        } // (((s1r[0] != 0.0) || (s1i[0] != 0.0)) && (as1 != 0.0))

        aa = as1.max(as2);

        if (aa.gt(ascle)) {
            return;
        } // if (aa > ascle)

        s1r[0] = DoubleDouble.valueOf(0.0);
        s1i[0] = DoubleDouble.valueOf(0.0);
        s2r[0] = DoubleDouble.valueOf(0.0);
        s2i[0] = DoubleDouble.valueOf(0.0);
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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     */
    private void zseri(final DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n, final DoubleDouble[] yr,
            final DoubleDouble[] yi, final int[] nz) {
        DoubleDouble az;
        int i;
        DoubleDouble arm;
        DoubleDouble rtr1;
        DoubleDouble crscr;
        int iflag;
        DoubleDouble hzr;
        DoubleDouble hzi;
        final DoubleDouble[] czr = new DoubleDouble[1];
        final DoubleDouble[] czi = new DoubleDouble[1];
        DoubleDouble acz;
        int nn;
        final DoubleDouble[] ckr = new DoubleDouble[1];
        final DoubleDouble[] cki = new DoubleDouble[1];
        final int[] idum = new int[1];
        DoubleDouble dfnu = DoubleDouble.valueOf(0.0);
        DoubleDouble fnup = DoubleDouble.valueOf(1.0);
        DoubleDouble ak1r = DoubleDouble.valueOf(0.0);
        DoubleDouble ak1i = DoubleDouble.valueOf(0.0);
        DoubleDouble ak = DoubleDouble.valueOf(0.0);
        boolean seg1 = true;
        boolean seg2 = true;
        boolean seg3 = true;
        boolean seg4 = true;
        boolean seg5 = true;
        DoubleDouble ss = DoubleDouble.valueOf(1.0);
        DoubleDouble ascle = DoubleDouble.valueOf(0.0);
        DoubleDouble aa;
        DoubleDouble coefr;
        DoubleDouble coefi;
        DoubleDouble atol;
        int il;
        DoubleDouble s1r = DoubleDouble.valueOf(0.0);
        DoubleDouble s1i = DoubleDouble.valueOf(0.0);
        DoubleDouble s = DoubleDouble.valueOf(1.0);
        DoubleDouble rs;
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        final DoubleDouble[] wr = new DoubleDouble[2];
        final DoubleDouble[] wi = new DoubleDouble[2];
        int m;
        int k;
        DoubleDouble raz;
        DoubleDouble rzr;
        DoubleDouble rzi;
        int ib = 0;
        DoubleDouble s2r;
        DoubleDouble s2i;
        final int[] nw = new int[1];
        int l;

        nz[0] = 0;
        az = zabs(zr, zi);

        if (az.equals(DoubleDouble.valueOf(0.0))) {
            yr[0] = DoubleDouble.valueOf(0.0);
            yi[0] = DoubleDouble.valueOf(0.0);

            if (fnu.equals(DoubleDouble.valueOf(0.0))) {
                yr[0] = DoubleDouble.valueOf(1.0);
            }

            if (n == 1) {
                return;
            }

            for (i = 1; i < n; i++) {
                yr[i] = DoubleDouble.valueOf(0.0);
                yi[i] = DoubleDouble.valueOf(0.0);
            }

            return;
        } // if (az == 0.0)

        arm = (DoubleDouble.valueOf(1.0E3)).multiply(tiny);
        rtr1 = arm.sqrt();
        crscr = DoubleDouble.valueOf(1.0);
        iflag = 0;

        if (az.lt(arm)) {
            nz[0] = n;

            if (fnu.equals(DoubleDouble.valueOf(0.0))) {
                nz[0] = nz[0] - 1;
            }

            yr[0] = DoubleDouble.valueOf(0.0);
            yi[0] = DoubleDouble.valueOf(0.0);

            if (fnu.equals(DoubleDouble.valueOf(0.0))) {
                yr[0] = DoubleDouble.valueOf(1.0);
            }

            if (n == 1) {
                return;
            }

            for (i = 1; i < n; i++) {
                yr[i] = DoubleDouble.valueOf(0.0);
                yi[i] = DoubleDouble.valueOf(0.0);
            }

            return;
        } // if (az < arm)

        hzr = (DoubleDouble.valueOf(0.5)).multiply(zr);
        hzi = (DoubleDouble.valueOf(0.5)).multiply(zi);
        czr[0] = DoubleDouble.valueOf(0.0);
        czi[0] = DoubleDouble.valueOf(0.0);

        if (az.gt(rtr1)) {
            zmlt(hzr, hzi, hzr, hzi, czr, czi);
        }

        acz = zabs(czr[0], czi[0]);
        nn = n;
        zlog(hzr, hzi, ckr, cki, idum);

        loop: while (true) {

            if (seg1) {
                dfnu = fnu.add(DoubleDouble.valueOf(nn - 1.0));
                fnup = dfnu.add(DoubleDouble.valueOf(1.0));

                // underflow test
                ak1r = ckr[0].multiply(dfnu);
                ak1i = cki[0].multiply(dfnu);
                ak = dgamln(fnup, idum);
                ak1r = ak1r.subtract(ak);

                if (kode == 2) {
                    ak1r = ak1r.subtract(zr);
                }

                if (ak1r.gt(elim.negate())) {
                    seg2 = false;
                }
            } // if (seg1)

            seg1 = true;

            if (seg2) {
                nz[0] = nz[0] + 1;
                yr[nn - 1] = DoubleDouble.valueOf(0.0);
                yi[nn - 1] = DoubleDouble.valueOf(0.0);

                if (acz.gt(dfnu)) {
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

            if (ak1r.le(alim.negate())) {
                iflag = 1;
                ss = tol.reciprocal();
                crscr = (DoubleDouble)tol.clone();
                ascle = arm.multiply(ss);
            } // if (ak1r <= (-alim))

            aa = ak1r.exp();

            if (iflag == 1) {
                aa = aa.multiply(ss);
            }

            coefr = aa.multiply(ak1i.cos());
            coefi = aa.multiply(ak1i.sin());
            atol = (tol.multiply(acz)).divide(fnup);
            il = Math.min(2, nn);

            for (i = 1; i <= il; i++) {
                dfnu = fnu.add(DoubleDouble.valueOf(nn - i));
                fnup = dfnu.add(DoubleDouble.valueOf(1.0));
                s1r = DoubleDouble.valueOf(1.0);
                s1i = DoubleDouble.valueOf(0.0);

                if (acz.lt(tol.multiply(fnup))) {
                    seg3 = false;
                    seg4 = false;
                }

                if (seg3) {
                    ak1r = DoubleDouble.valueOf(1.0);
                    ak1i = DoubleDouble.valueOf(0.0);
                    ak = fnup.add(DoubleDouble.valueOf(2.0));
                    s = (DoubleDouble)fnup.clone();
                    aa = DoubleDouble.valueOf(2.0);
                } // if (seg3)

                seg3 = true;

                if (seg4) {

                    do {
                        rs = s.reciprocal();
                        str[0] = (ak1r.multiply(czr[0])).subtract(ak1i.multiply(czi[0]));
                        sti[0] = (ak1r.multiply(czi[0])).add(ak1i.multiply(czr[0]));
                        ak1r = str[0].multiply(rs);
                        ak1i = sti[0].multiply(rs);
                        s1r = s1r.add(ak1r);
                        s1i = s1i.add(ak1i);
                        s = s.add(ak);
                        ak = ak.add(DoubleDouble.valueOf(2.0));
                        aa = (aa.multiply(acz)).multiply(rs);
                    } while (aa.gt(atol));
                } // if (seg4)

                seg4 = true;
                s2r = (s1r.multiply(coefr)).subtract(s1i.multiply(coefi));
                s2i = (s1r.multiply(coefi)).add(s1i.multiply(coefr));
                wr[i - 1] = (DoubleDouble)s2r.clone();
                wi[i - 1] = (DoubleDouble)s2i.clone();

                if (iflag != 0) {
                    zuchk(s2r, s2i, nw, ascle);

                    if (nw[0] != 0) {
                        seg1 = false;

                        continue loop;
                    }
                } // if (iflag != 0)

                m = nn - i + 1;
                yr[m - 1] = s2r.multiply(crscr);
                yi[m - 1] = s2i.multiply(crscr);

                if (i != il) {
                    zdiv(coefr, coefi, hzr, hzi, str, sti);
                    coefr = str[0].multiply(dfnu);
                    coefi = sti[0].multiply(dfnu);
                } // if (i != il)
            } // for (i = 1; i <= il; i++)

            break loop;
        } // while (true)

        if (nn <= 2) {
            return;
        }

        k = nn - 2;
        ak = DoubleDouble.valueOf(k);
        raz = az.reciprocal();
        str[0] = zr.multiply(raz);
        sti[0] = (zi.negate()).multiply(raz);
        rzr = ((DoubleDouble.valueOf(2.0)).multiply(str[0])).multiply(raz);
        rzi = ((DoubleDouble.valueOf(2.0)).multiply(sti[0])).multiply(raz);

        if (iflag == 1) {
            seg5 = false;
        } // if (iflag == 1)

        if (seg5) {
            ib = 3;

            for (i = ib; i <= nn; i++) {
                yr[k - 1] = ( (ak.add(fnu)).multiply( (rzr.multiply(yr[k])).subtract(rzi.multiply(yi[k])))).add(yr[k + 1]);
                yi[k - 1] = ( (ak.add(fnu)).multiply( (rzr.multiply(yi[k])).add(rzi.multiply(yr[k])))).add(yi[k + 1]);
                ak = ak.subtract(DoubleDouble.valueOf(1.0));
                k = k - 1;
            } // for (i = ib; i <= nn; i++)

            return;
        } // if (seg5)

        seg5 = true;

        // Recur backward with scaled values
        // exp(-alim) = exp(-elim)/tol = approximately one precision above the
        // underflow limit = ascle = D1MACH(1) * ss * 1.0E+3
        s1r = (DoubleDouble)wr[0].clone();
        s1i = (DoubleDouble)wi[0].clone();
        s2r = (DoubleDouble)wr[1].clone();
        s2i = (DoubleDouble)wi[1].clone();

        group: {

            for (l = 3; l <= nn; l++) {
                ckr[0] = (DoubleDouble)s2r.clone();
                cki[0] = (DoubleDouble)s2i.clone();
                s2r = s1r.add( (ak.add(fnu)).multiply( (rzr.multiply(ckr[0])).subtract(rzi.multiply(cki[0]))));
                s2i = s1i.add( (ak.add(fnu)).multiply( (rzr.multiply(cki[0])).add(rzi.multiply(ckr[0]))));
                s1r = (DoubleDouble)ckr[0].clone();
                s1i = (DoubleDouble)cki[0].clone();
                ckr[0] = s2r.multiply(crscr);
                cki[0] = s2i.multiply(crscr);
                yr[k - 1] = (DoubleDouble)ckr[0].clone();
                yi[k - 1] = (DoubleDouble)cki[0].clone();
                ak = ak.subtract(DoubleDouble.valueOf(1.0));
                k = k - 1;

                if ((zabs(ckr[0], cki[0])).gt(ascle)) {
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
            yr[k - 1] = ( (ak.add(fnu)).multiply( (rzr.multiply(yr[k])).subtract(rzi.multiply(yi[k])))).add(yr[k + 1]);
            yi[k - 1] = ( (ak.add(fnu)).multiply( (rzr.multiply(yi[k])).add(rzi.multiply(yr[k])))).add(yi[k + 1]);
            ak = ak.subtract(DoubleDouble.valueOf(1.0));
            k = k - 1;
        } // for (i = ib; i <= nn; i++)

        return;
    }

    /**
     * zshch computes the complex hyperbolic functions csh = sinh(x+i*y) and cch = cosh(x+i*y).
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param cshr DoubleDouble[]
     * @param cshi DoubleDouble[]
     * @param cchr DoubleDouble[]
     * @param cchi DoubleDouble[]
     */
    private void zshch(DoubleDouble zr, final DoubleDouble zi, final DoubleDouble[] cshr, final DoubleDouble[] cshi, final DoubleDouble[] cchr,
            final DoubleDouble[] cchi) {
        DoubleDouble pexp;
        DoubleDouble mexp;
        DoubleDouble sh;
        DoubleDouble ch;
        DoubleDouble sn;
        DoubleDouble cn;

        pexp = zr.exp();
        mexp = (zr.negate()).exp();
        sh = (DoubleDouble.valueOf(0.5)).multiply(pexp.subtract(mexp));
        ch = (DoubleDouble.valueOf(0.5)).multiply(pexp.add(mexp));
        sn = zi.sin();
        cn = zi.cos();
        cshr[0] = sh.multiply(cn);
        cshi[0] = ch.multiply(sn);
        cchr[0] = ch.multiply(cn);
        cchi[0] = sh.multiply(sn);

        return;
    }

    /**
     * complex square root b = csqrt(a).
     * 
     * @param ar DoubleDouble
     * @param ai DoubleDouble
     * @param br DoubleDouble[]
     * @param bi DoubleDouble[]
     */
    private void zsqrt(final DoubleDouble ar, final DoubleDouble ai, final DoubleDouble[] br, final DoubleDouble[] bi) {
        final DoubleDouble drt = (DoubleDouble.valueOf(1.0)).divide((DoubleDouble.valueOf(2.0)).sqrt());
        DoubleDouble zm;
        DoubleDouble theta;

        zm = zabs(ar, ai);
        zm = zm.sqrt();

        if (ar.equals(DoubleDouble.valueOf(0.0))) {

            if (ai.equals(DoubleDouble.valueOf(0.0))) {
                br[0] = DoubleDouble.valueOf(0.0);
                bi[0] = DoubleDouble.valueOf(0.0);

                return;
            } // if (ai == 0.0)
            else if (ai.gt(DoubleDouble.valueOf(0.0))) {
                br[0] = zm.multiply(drt);
                bi[0] = zm.multiply(drt);

                return;
            } // else if (ai > 0.0)
            else { // ai < 0.0
                br[0] = zm.multiply(drt);
                bi[0] = (zm.negate()).multiply(drt);

                return;
            } // else ai < 0.0
        } // if (ar == 0.0)
        else if (ai.equals(DoubleDouble.valueOf(0.0))) {

            if (ar.gt(DoubleDouble.valueOf(0.0))) {
                br[0] = ar.sqrt();
                bi[0] = DoubleDouble.valueOf(0.0);

                return;
            } // if (ar > 0.0)
            else { // ar < 0.0
                br[0] = DoubleDouble.valueOf(0.0);
                bi[0] = (ar.abs()).sqrt();

                return;
            } // ar < 0.0
        } // else if (ai == 0.0)

        theta = (ai.divide(ar)).atan();

        if (theta.le(DoubleDouble.valueOf(0.0))) {

            if (ar.lt(DoubleDouble.valueOf(0.0))) {
                theta = theta.add(DoubleDouble.PI);
            }
        } else if (ar.lt(DoubleDouble.valueOf(0.0))) {
            theta = theta.subtract(DoubleDouble.PI);
        }

        theta = (DoubleDouble.valueOf(0.5)).multiply(theta);
        br[0] = zm.multiply(theta.cos());
        bi[0] = zm.multiply(theta.sin());

        return;
    }

    /**
     * Y ENTERS AS A SCALED QUANTITY WHOSE MAGNITUDE IS GREATER THAN EXP(-ALIM)=ASCLE=1.0E+3*D1MACH(1)/TOL. THE TEST IS
     * MADE TO SEE IF THE MAGNITUDE OF THE REAL OR IMAGINARY PART WOULD UNDERFLOW WHEN Y IS SCALED (BY TOL) TO ITS
     * PROPER VALUE. Y IS ACCEPTED IF THE UNDERFLOW IS AT LEAST ONE PRECISION BELOW THE MAGNITUDE OF THE LARGEST
     * COMPONENT; OTHERWISE THE PHASE ANGLE DOES NOT HAVE ABSOLUTE ACCURACY AND AN UNDERFLOW IS ASSUMED.
     * 
     * @param yr DoubleDouble
     * @param yi DoubleDouble
     * @param nz int[]
     * @param ascle DoubleDouble
     */
    private void zuchk(final DoubleDouble yr, final DoubleDouble yi, final int[] nz, final DoubleDouble ascle) {
        DoubleDouble wr, wi, ss, st;
        nz[0] = 0;
        wr = yr.abs();
        wi = yi.abs();
        st = wr.min(wi);

        if (st.gt(ascle)) {
            return;
        }

        ss = wr.max(wi);
        st = st.divide(tol);

        if (ss.lt(st)) {
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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param ipmtr int
     * @param phir DoubleDouble[]
     * @param phii DoubleDouble[]
     * @param argr DoubleDouble[]
     * @param argi DoubleDouble[]
     * @param zeta1r DoubleDouble[]
     * @param zeta1i DoubleDouble[]
     * @param zeta2r DoubleDouble[]
     * @param zeta2i DoubleDouble[]
     * @param asumr DoubleDouble[]
     * @param asumi DoubleDouble[]
     * @param bsumr DoubleDouble[]
     * @param bsumi DoubleDouble[]
     */
    private void zunhj(final DoubleDouble zr, final DoubleDouble zi, final DoubleDouble fnu, final int ipmtr, final DoubleDouble[] phir,
            final DoubleDouble[] phii, final DoubleDouble[] argr, final DoubleDouble[] argi, final DoubleDouble[] zeta1r,
            final DoubleDouble[] zeta1i, final DoubleDouble[] zeta2r, final DoubleDouble[] zeta2i, final DoubleDouble[] asumr,
            final DoubleDouble[] asumi, final DoubleDouble[] bsumr, final DoubleDouble[] bsumi) {
        final DoubleDouble[] ar = new DoubleDouble[] {DoubleDouble.valueOf(1.00000000000000000E+00), 
        		DoubleDouble.valueOf(1.04166666666666667E-01), DoubleDouble.valueOf(8.35503472222222222E-02),
                DoubleDouble.valueOf(1.28226574556327160E-01), DoubleDouble.valueOf(2.91849026464140464E-01),
                DoubleDouble.valueOf(8.81627267443757652E-01), DoubleDouble.valueOf(3.32140828186276754E+00),
                DoubleDouble.valueOf(1.49957629868625547E+01), DoubleDouble.valueOf(7.89230130115865181E+01),
                DoubleDouble.valueOf(4.74451538868264323E+02), DoubleDouble.valueOf(3.20749009089066193E+03),
                DoubleDouble.valueOf(2.40865496408740049E+04), DoubleDouble.valueOf(1.98923119169509794E+05),
                DoubleDouble.valueOf(1.79190200777534383E+06)};
        final DoubleDouble[] br = new DoubleDouble[] {DoubleDouble.valueOf(1.00000000000000000E+00),
        		DoubleDouble.valueOf(-1.45833333333333333E-01), DoubleDouble.valueOf(-9.87413194444444444E-02),
                DoubleDouble.valueOf(-1.43312053915895062E-01), DoubleDouble.valueOf(-3.17227202678413548E-01),
                DoubleDouble.valueOf(-9.42429147957120249E-01), DoubleDouble.valueOf(-3.51120304082635426E+00),
                DoubleDouble.valueOf(-1.57272636203680451E+01), DoubleDouble.valueOf(-8.22814390971859444E+01),
                DoubleDouble.valueOf(-4.92355370523670524E+02), DoubleDouble.valueOf(-3.31621856854797251E+03),
                DoubleDouble.valueOf(-2.48276742452085896E+04), DoubleDouble.valueOf(-2.04526587315129788E+05),
                DoubleDouble.valueOf(-1.83844491706820990E+06)};
        final DoubleDouble[] c = new DoubleDouble[] {DoubleDouble.valueOf(1.00000000000000000E+00),
        		DoubleDouble.valueOf(-2.08333333333333333E-01), DoubleDouble.valueOf(1.25000000000000000E-01),
                DoubleDouble.valueOf(3.34201388888888889E-01), DoubleDouble.valueOf(-4.01041666666666667E-01),
                DoubleDouble.valueOf(7.03125000000000000E-02), DoubleDouble.valueOf(-1.02581259645061728E+00),
                DoubleDouble.valueOf(1.84646267361111111E+00), DoubleDouble.valueOf(-8.91210937500000000E-01),
                DoubleDouble.valueOf(7.32421875000000000E-02), DoubleDouble.valueOf(4.66958442342624743E+00),
                DoubleDouble.valueOf(-1.12070026162229938E+01), DoubleDouble.valueOf(8.78912353515625000E+00),
                DoubleDouble.valueOf(-2.36408691406250000E+00), DoubleDouble.valueOf(1.12152099609375000E-01),
                DoubleDouble.valueOf(-2.82120725582002449E+01), DoubleDouble.valueOf(8.46362176746007346E+01), 
                DoubleDouble.valueOf(-9.18182415432400174E+01), DoubleDouble.valueOf(4.25349987453884549E+01),
                DoubleDouble.valueOf(-7.36879435947963170E+00), DoubleDouble.valueOf(2.27108001708984375E-01), 
                DoubleDouble.valueOf(2.12570130039217123E+02), DoubleDouble.valueOf(-7.65252468141181642E+02),
                DoubleDouble.valueOf(1.05999045252799988E+03), DoubleDouble.valueOf(-6.99579627376132541E+02),
                DoubleDouble.valueOf(2.18190511744211590E+02), DoubleDouble.valueOf(-2.64914304869515555E+01),
                DoubleDouble.valueOf(5.72501420974731445E-01), DoubleDouble.valueOf(-1.91945766231840700E+03),
                DoubleDouble.valueOf(8.06172218173730938E+03), DoubleDouble.valueOf(-1.35865500064341374E+04),
                DoubleDouble.valueOf(1.16553933368645332E+04), DoubleDouble.valueOf(-5.30564697861340311E+03),
                DoubleDouble.valueOf(1.20090291321635246E+03), DoubleDouble.valueOf(-1.08090919788394656E+02),
                DoubleDouble.valueOf(1.72772750258445740E+00), DoubleDouble.valueOf(2.02042913309661486E+04),
                DoubleDouble.valueOf(-9.69805983886375135E+04), DoubleDouble.valueOf(1.92547001232531532E+05),
                DoubleDouble.valueOf(-2.03400177280415534E+05), DoubleDouble.valueOf(1.22200464983017460E+05),
                DoubleDouble.valueOf(-4.11926549688975513E+04), DoubleDouble.valueOf(7.10951430248936372E+03),
                DoubleDouble.valueOf(-4.93915304773088012E+02), DoubleDouble.valueOf(6.07404200127348304E+00),
                DoubleDouble.valueOf(-2.42919187900551333E+05), DoubleDouble.valueOf(1.31176361466297720E+06),
                DoubleDouble.valueOf(-2.99801591853810675E+06), DoubleDouble.valueOf(3.76327129765640400E+06), 
                DoubleDouble.valueOf(-2.81356322658653411E+06), DoubleDouble.valueOf(1.26836527332162478E+06),
                DoubleDouble.valueOf(-3.31645172484563578E+05), DoubleDouble.valueOf(4.52187689813627263E+04), 
                DoubleDouble.valueOf(-2.49983048181120962E+03), DoubleDouble.valueOf(2.43805296995560639E+01),
                DoubleDouble.valueOf(3.28446985307203782E+06), DoubleDouble.valueOf(-1.97068191184322269E+07),
                DoubleDouble.valueOf(5.09526024926646422E+07), DoubleDouble.valueOf(-7.41051482115326577E+07),
                DoubleDouble.valueOf(6.63445122747290267E+07), DoubleDouble.valueOf(-3.75671766607633513E+07), 
                DoubleDouble.valueOf(1.32887671664218183E+07), DoubleDouble.valueOf(-2.78561812808645469E+06),
                DoubleDouble.valueOf(3.08186404612662398E+05), DoubleDouble.valueOf(-1.38860897537170405E+04),
                DoubleDouble.valueOf(1.10017140269246738E+02), DoubleDouble.valueOf(-4.93292536645099620E+07),
                DoubleDouble.valueOf(3.25573074185765749E+08), DoubleDouble.valueOf(-9.39462359681578403E+08),
                DoubleDouble.valueOf(1.55359689957058006E+09), DoubleDouble.valueOf(-1.62108055210833708E+09),
                DoubleDouble.valueOf(1.10684281682301447E+09), DoubleDouble.valueOf(-4.95889784275030309E+08), 
                DoubleDouble.valueOf(1.42062907797533095E+08), DoubleDouble.valueOf(-2.44740627257387285E+07),
                DoubleDouble.valueOf(2.24376817792244943E+06), DoubleDouble.valueOf(-8.40054336030240853E+04),
                DoubleDouble.valueOf(5.51335896122020586E+02), DoubleDouble.valueOf(8.14789096118312115E+08),
                DoubleDouble.valueOf(-5.86648149205184723E+09), DoubleDouble.valueOf(1.86882075092958249E+10),
                DoubleDouble.valueOf(-3.46320433881587779E+10), DoubleDouble.valueOf(4.12801855797539740E+10),
                DoubleDouble.valueOf(-3.30265997498007231E+10), DoubleDouble.valueOf(1.79542137311556001E+10),
                DoubleDouble.valueOf(-6.56329379261928433E+09), DoubleDouble.valueOf(1.55927986487925751E+09),
                DoubleDouble.valueOf(-2.25105661889415278E+08), DoubleDouble.valueOf(1.73951075539781645E+07), 
                DoubleDouble.valueOf(-5.49842327572288687E+05), DoubleDouble.valueOf(3.03809051092238427E+03),
                DoubleDouble.valueOf(-1.46792612476956167E+10), DoubleDouble.valueOf(1.14498237732025810E+11),
                DoubleDouble.valueOf(-3.99096175224466498E+11), DoubleDouble.valueOf(8.19218669548577329E+11),
                DoubleDouble.valueOf(-1.09837515608122331E+12), DoubleDouble.valueOf(1.00815810686538209E+12),
                DoubleDouble.valueOf(-6.45364869245376503E+11),DoubleDouble.valueOf(2.87900649906150589E+11),
                DoubleDouble.valueOf(-8.78670721780232657E+10), DoubleDouble.valueOf(1.76347306068349694E+10),
                DoubleDouble.valueOf(-2.16716498322379509E+09), DoubleDouble.valueOf(1.43157876718888981E+08),
                DoubleDouble.valueOf(-3.87183344257261262E+06), DoubleDouble.valueOf(1.82577554742931747E+04)};
        final DoubleDouble[] alfa = new DoubleDouble[] { DoubleDouble.valueOf(-4.44444444444444444E-03), 
        		DoubleDouble.valueOf(-9.22077922077922078E-04),DoubleDouble.valueOf(-8.84892884892884893E-05), 
        		DoubleDouble.valueOf(1.65927687832449737E-04), DoubleDouble.valueOf(2.46691372741792910E-04), 
        		DoubleDouble.valueOf(2.65995589346254780E-04), DoubleDouble.valueOf(2.61824297061500945E-04), 
        		DoubleDouble.valueOf(2.48730437344655609E-04), DoubleDouble.valueOf(2.32721040083232098E-04), 
        		DoubleDouble.valueOf(2.16362485712365082E-04), DoubleDouble.valueOf(2.00738858762752355E-04), 
        		DoubleDouble.valueOf(1.86267636637545172E-04), DoubleDouble.valueOf(1.73060775917876493E-04), 
        		DoubleDouble.valueOf(1.61091705929015752E-04), DoubleDouble.valueOf(1.50274774160908134E-04),
        		DoubleDouble.valueOf(1.40503497391269794E-04), DoubleDouble.valueOf(1.31668816545922806E-04), 
        		DoubleDouble.valueOf(1.23667445598253261E-04), DoubleDouble.valueOf(1.16405271474737902E-04),
        		DoubleDouble.valueOf(1.09798298372713369E-04), DoubleDouble.valueOf(1.03772410422992823E-04), 
        		DoubleDouble.valueOf(9.82626078369363448E-05), DoubleDouble.valueOf(9.32120517249503256E-05),
        		DoubleDouble.valueOf(8.85710852478711718E-05), DoubleDouble.valueOf(8.42963105715700223E-05), 
        		DoubleDouble.valueOf(8.03497548407791151E-05), DoubleDouble.valueOf(7.66981345359207388E-05), 
        		DoubleDouble.valueOf(7.33122157481777809E-05), DoubleDouble.valueOf(7.01662625163141333E-05), 
        		DoubleDouble.valueOf(6.72375633790160292E-05), DoubleDouble.valueOf(6.93735541354588974E-04), 
        		DoubleDouble.valueOf(2.32241745182921654E-04), DoubleDouble.valueOf(-1.41986273556691197E-05), 
        		DoubleDouble.valueOf(-1.16444931672048640E-04), DoubleDouble.valueOf(-1.50803558053048762E-04), 
        		DoubleDouble.valueOf(-1.55121924918096223E-04), DoubleDouble.valueOf(-1.46809756646465549E-04), 
        		DoubleDouble.valueOf(-1.33815503867491367E-04), DoubleDouble.valueOf(-1.19744975684254051E-04), 
        		DoubleDouble.valueOf(-1.06184319207974020E-04), DoubleDouble.valueOf(-9.37699549891194492E-05),
        		DoubleDouble.valueOf(-8.26923045588193274E-05), DoubleDouble.valueOf(-7.29374348155221211E-05),
        		DoubleDouble.valueOf(-6.44042357721016283E-05), DoubleDouble.valueOf(-5.69611566009369048E-05), 
        		DoubleDouble.valueOf(-5.04731044303561628E-05), DoubleDouble.valueOf(-4.48134868008882786E-05),
        		DoubleDouble.valueOf(-3.98688727717598864E-05), DoubleDouble.valueOf(-3.55400532972042498E-05), 
        		DoubleDouble.valueOf(-3.17414256609022480E-05), DoubleDouble.valueOf(-2.83996793904174811E-05), 
        		DoubleDouble.valueOf(-2.54522720634870566E-05), DoubleDouble.valueOf(-2.28459297164724555E-05),
        		DoubleDouble.valueOf(-2.05352753106480604E-05), DoubleDouble.valueOf(-1.84816217627666085E-05), 
        		DoubleDouble.valueOf(-1.66519330021393806E-05), DoubleDouble.valueOf(-1.50179412980119482E-05),
        		DoubleDouble.valueOf(-1.35554031379040526E-05), DoubleDouble.valueOf(-1.22434746473858131E-05), 
        		DoubleDouble.valueOf(-1.10641884811308169E-05), DoubleDouble.valueOf(-3.54211971457743841E-04), 
        		DoubleDouble.valueOf(-1.56161263945159416E-04), DoubleDouble.valueOf(3.04465503594936410E-05), 
        		DoubleDouble.valueOf(1.30198655773242693E-04), DoubleDouble.valueOf(1.67471106699712269E-04), 
        		DoubleDouble.valueOf(1.70222587683592569E-04), DoubleDouble.valueOf(1.56501427608594704E-04), 
        		DoubleDouble.valueOf(1.36339170977445120E-04), DoubleDouble.valueOf(1.14886692029825128E-04), 
        		DoubleDouble.valueOf(9.45869093034688111E-05), DoubleDouble.valueOf(7.64498419250898258E-05),
        		DoubleDouble.valueOf(6.07570334965197354E-05), DoubleDouble.valueOf(4.74394299290508799E-05), 
        		DoubleDouble.valueOf(3.62757512005344297E-05), DoubleDouble.valueOf(2.69939714979224901E-05), 
        		DoubleDouble.valueOf(1.93210938247939253E-05), DoubleDouble.valueOf(1.30056674793963203E-05),
        		DoubleDouble.valueOf(7.82620866744496661E-06), DoubleDouble.valueOf(3.59257485819351583E-06), 
        		DoubleDouble.valueOf(1.44040049814251817E-07), DoubleDouble.valueOf(-2.65396769697939116E-06), 
        		DoubleDouble.valueOf(-4.91346867098485910E-06), DoubleDouble.valueOf(-6.72739296091248287E-06), 
        		DoubleDouble.valueOf(-8.17269379678657923E-06), DoubleDouble.valueOf(-9.31304715093561232E-06), 
        		DoubleDouble.valueOf(-1.02011418798016441E-05), DoubleDouble.valueOf(-1.08805962510592880E-05), 
        		DoubleDouble.valueOf(-1.13875481509603555E-05), DoubleDouble.valueOf(-1.17519675674556414E-05), 
        		DoubleDouble.valueOf(-1.19987364870944141E-05), DoubleDouble.valueOf(3.78194199201772914E-04), 
        		DoubleDouble.valueOf(2.02471952761816167E-04), DoubleDouble.valueOf(-6.37938506318862408E-05), 
        		DoubleDouble.valueOf(-2.38598230603005903E-04), DoubleDouble.valueOf(-3.10916256027361568E-04), 
        		DoubleDouble.valueOf(-3.13680115247576316E-04), DoubleDouble.valueOf(-2.78950273791323387E-04), 
        		DoubleDouble.valueOf(-2.28564082619141374E-04), DoubleDouble.valueOf(-1.75245280340846749E-04), 
        		DoubleDouble.valueOf(-1.25544063060690348E-04), DoubleDouble.valueOf(-8.22982872820208365E-05), 
        		DoubleDouble.valueOf(-4.62860730588116458E-05), DoubleDouble.valueOf(-1.72334302366962267E-05), 
        		DoubleDouble.valueOf(5.60690482304602267E-06), DoubleDouble.valueOf(2.31395443148286800E-05), 
        		DoubleDouble.valueOf(3.62642745856793957E-05), DoubleDouble.valueOf(4.58006124490188752E-05), 
        		DoubleDouble.valueOf(5.24595294959114050E-05), DoubleDouble.valueOf(5.68396208545815266E-05),
        		DoubleDouble.valueOf(5.94349820393104052E-05), DoubleDouble.valueOf(6.06478527578421742E-05), 
        		DoubleDouble.valueOf(6.08023907788436497E-05), DoubleDouble.valueOf(6.01577894539460388E-05), 
        		DoubleDouble.valueOf(5.89199657344698500E-05), DoubleDouble.valueOf(5.72515823777593053E-05), 
        		DoubleDouble.valueOf(5.52804375585852577E-05), DoubleDouble.valueOf(5.31063773802880170E-05), 
        		DoubleDouble.valueOf(5.08069302012325706E-05), DoubleDouble.valueOf(4.84418647620094842E-05), 
        		DoubleDouble.valueOf(4.60568581607475370E-05), DoubleDouble.valueOf(-6.91141397288294174E-04), 
        		DoubleDouble.valueOf(-4.29976633058871912E-04), DoubleDouble.valueOf(1.83067735980039018E-04), 
        		DoubleDouble.valueOf(6.60088147542014144E-04), DoubleDouble.valueOf(8.75964969951185931E-04), 
        		DoubleDouble.valueOf(8.77335235958235514E-04), DoubleDouble.valueOf(7.49369585378990637E-04),
        		DoubleDouble.valueOf(5.63832329756980918E-04), DoubleDouble.valueOf(3.68059319971443156E-04), 
        		DoubleDouble.valueOf(1.88464535514455599E-04), DoubleDouble.valueOf(3.70663057664904149E-05), 
        		DoubleDouble.valueOf(-8.28520220232137023E-05), DoubleDouble.valueOf(-1.72751952869172998E-04), 
        		DoubleDouble.valueOf(-2.36314873605872983E-04), DoubleDouble.valueOf(-2.77966150694906658E-04), 
        		DoubleDouble.valueOf(-3.02079514155456919E-04), DoubleDouble.valueOf(-3.12594712643820127E-04), 
        		DoubleDouble.valueOf(-3.12872558758067163E-04), DoubleDouble.valueOf(-3.05678038466324377E-04),
        		DoubleDouble.valueOf(-2.93226470614557331E-04), DoubleDouble.valueOf(-2.77255655582934777E-04), 
        		DoubleDouble.valueOf(-2.59103928467031709E-04), DoubleDouble.valueOf(-2.39784014396480342E-04),
        		DoubleDouble.valueOf(-2.20048260045422848E-04), DoubleDouble.valueOf(-2.00443911094971498E-04), 
        		DoubleDouble.valueOf(-1.81358692210970687E-04), DoubleDouble.valueOf(-1.63057674478657464E-04), 
        		DoubleDouble.valueOf(-1.45712672175205844E-04), DoubleDouble.valueOf(-1.29425421983924587E-04), 
        		DoubleDouble.valueOf(-1.14245691942445952E-04), DoubleDouble.valueOf(1.92821964248775885E-03), 
        		DoubleDouble.valueOf(1.35592576302022234E-03), DoubleDouble.valueOf(-7.17858090421302995E-04), 
        		DoubleDouble.valueOf(-2.58084802575270346E-03), DoubleDouble.valueOf(-3.49271130826168475E-03), 
        		DoubleDouble.valueOf(-3.46986299340960628E-03), DoubleDouble.valueOf(-2.82285233351310182E-03), 
        		DoubleDouble.valueOf(-1.88103076404891354E-03), DoubleDouble.valueOf(-8.89531718383947600E-04), 
        		DoubleDouble.valueOf(3.87912102631035228E-06), DoubleDouble.valueOf(7.28688540119691412E-04), 
        		DoubleDouble.valueOf(1.26566373053457758E-03), DoubleDouble.valueOf(1.62518158372674427E-03), 
        		DoubleDouble.valueOf(1.83203153216373172E-03), DoubleDouble.valueOf(1.91588388990527909E-03), 
        		DoubleDouble.valueOf(1.90588846755546138E-03), DoubleDouble.valueOf(1.82798982421825727E-03), 
        		DoubleDouble.valueOf(1.70389506421121530E-03), DoubleDouble.valueOf(1.55097127171097686E-03), 
        		DoubleDouble.valueOf(1.38261421852276159E-03), DoubleDouble.valueOf(1.20881424230064774E-03), 
        		DoubleDouble.valueOf(1.03676532638344962E-03), DoubleDouble.valueOf(8.71437918068619115E-04), 
        		DoubleDouble.valueOf(7.16080155297701002E-04), DoubleDouble.valueOf(5.72637002558129372E-04), 
        		DoubleDouble.valueOf(4.42089819465802277E-04), DoubleDouble.valueOf(3.24724948503090564E-04), 
        		DoubleDouble.valueOf(2.20342042730246599E-04), DoubleDouble.valueOf(1.28412898401353882E-04), 
        		DoubleDouble.valueOf(4.82005924552095464E-05)};
        final DoubleDouble[] beta = new DoubleDouble[] {DoubleDouble.valueOf(1.79988721413553309E-02),
        		DoubleDouble.valueOf(5.59964911064388073E-03), DoubleDouble.valueOf(2.88501402231132779E-03),
        		DoubleDouble.valueOf(1.80096606761053941E-03), DoubleDouble.valueOf(1.24753110589199202E-03), 
        		DoubleDouble.valueOf(9.22878876572938311E-04), DoubleDouble.valueOf(7.14430421727287357E-04),
        		DoubleDouble.valueOf(5.71787281789704872E-04), DoubleDouble.valueOf(4.69431007606481533E-04), 
        		DoubleDouble.valueOf(3.93232835462916638E-04), DoubleDouble.valueOf(3.34818889318297664E-04),
        		DoubleDouble.valueOf(2.88952148495751517E-04), DoubleDouble.valueOf(2.52211615549573284E-04), 
        		DoubleDouble.valueOf(2.22280580798883327E-04), DoubleDouble.valueOf(1.97541838033062524E-04),
        		DoubleDouble.valueOf(1.76836855019718004E-04), DoubleDouble.valueOf(1.59316899661821081E-04), 
        		DoubleDouble.valueOf(1.44347930197333986E-04), DoubleDouble.valueOf(1.31448068119965379E-04),
        		DoubleDouble.valueOf(1.20245444949302884E-04), DoubleDouble.valueOf(1.10449144504599392E-04), 
        		DoubleDouble.valueOf(1.01828770740567258E-04), DoubleDouble.valueOf(9.41998224204237509E-05),
        		DoubleDouble.valueOf(8.74130545753834437E-05), DoubleDouble.valueOf(8.13466262162801467E-05), 
        		DoubleDouble.valueOf(7.59002269646219339E-05), DoubleDouble.valueOf(7.09906300634153481E-05),
        		DoubleDouble.valueOf(6.65482874842468183E-05), DoubleDouble.valueOf(6.25146958969275078E-05), 
        		DoubleDouble.valueOf(5.88403394426251749E-05), DoubleDouble.valueOf(-1.49282953213429172E-03),
        		DoubleDouble.valueOf(-8.78204709546389328E-04), DoubleDouble.valueOf(-5.02916549572034614E-04),
        		DoubleDouble.valueOf(-2.94822138512746025E-04), DoubleDouble.valueOf(-1.75463996970782828E-04),
        		DoubleDouble.valueOf(-1.04008550460816434E-04), DoubleDouble.valueOf(-5.96141953046457895E-05), 
        		DoubleDouble.valueOf(-3.12038929076098340E-05), DoubleDouble.valueOf(-1.26089735980230047E-05),
        		DoubleDouble.valueOf(-2.42892608575730389E-07), DoubleDouble.valueOf(8.05996165414273571E-06), 
        		DoubleDouble.valueOf(1.36507009262147391E-05), DoubleDouble.valueOf(1.73964125472926261E-05),
        		DoubleDouble.valueOf(1.98672978842133780E-05), DoubleDouble.valueOf(2.14463263790822639E-05), 
        		DoubleDouble.valueOf(2.23954659232456514E-05), DoubleDouble.valueOf(2.28967783814712629E-05),
        		DoubleDouble.valueOf(2.30785389811177817E-05), DoubleDouble.valueOf(2.30321976080909144E-05), 
        		DoubleDouble.valueOf(2.28236073720348722E-05), DoubleDouble.valueOf(2.25005881105292418E-05),
        		DoubleDouble.valueOf(2.20981015361991429E-05), DoubleDouble.valueOf(2.16418427448103905E-05), 
        		DoubleDouble.valueOf(2.11507649256220843E-05), DoubleDouble.valueOf(2.06388749782170737E-05),
        		DoubleDouble.valueOf(2.01165241997081666E-05), DoubleDouble.valueOf(1.95913450141179244E-05),
        		DoubleDouble.valueOf(1.90689367910436740E-05), DoubleDouble.valueOf(1.85533719641636667E-05),
        		DoubleDouble.valueOf(1.80475722259674218E-05), DoubleDouble.valueOf(5.52213076721292790E-04), 
        		DoubleDouble.valueOf(4.47932581552384646E-04), DoubleDouble.valueOf(2.79520653992020589E-04),
        		DoubleDouble.valueOf(1.52468156198446602E-04), DoubleDouble.valueOf(6.93271105657043598E-05), 
        		DoubleDouble.valueOf(1.76258683069991397E-05), DoubleDouble.valueOf(-1.35744996343269136E-05),
        		DoubleDouble.valueOf(-3.17972413350427135E-05), DoubleDouble.valueOf(-4.18861861696693365E-05),
        		DoubleDouble.valueOf(-4.69004889379141029E-05), DoubleDouble.valueOf(-4.87665447413787352E-05),
        		DoubleDouble.valueOf(-4.87010031186735069E-05), DoubleDouble.valueOf(-4.74755620890086638E-05), 
        		DoubleDouble.valueOf(-4.55813058138628452E-05), DoubleDouble.valueOf(-4.33309644511266036E-05),
        		DoubleDouble.valueOf(-4.09230193157750364E-05), DoubleDouble.valueOf(-3.84822638603221274E-05), 
        		DoubleDouble.valueOf(-3.60857167535410501E-05), DoubleDouble.valueOf(-3.37793306123367417E-05),
        		DoubleDouble.valueOf(-3.15888560772109621E-05), DoubleDouble.valueOf(-2.95269561750807315E-05),
        		DoubleDouble.valueOf(-2.75978914828335759E-05), DoubleDouble.valueOf(-2.58006174666883713E-05),
        		DoubleDouble.valueOf(-2.41308356761280200E-05), DoubleDouble.valueOf(-2.25823509518346033E-05), 
        		DoubleDouble.valueOf(-2.11479656768912971E-05), DoubleDouble.valueOf(-1.98200638885294927E-05),
        		DoubleDouble.valueOf(-1.85909870801065077E-05), DoubleDouble.valueOf(-1.74532699844210224E-05), 
        		DoubleDouble.valueOf(-1.63997823854497997E-05), DoubleDouble.valueOf(-4.74617796559959808E-04),
        		DoubleDouble.valueOf(-4.77864567147321487E-04), DoubleDouble.valueOf(-3.20390228067037603E-04), 
        		DoubleDouble.valueOf(-1.61105016119962282E-04), DoubleDouble.valueOf(-4.25778101285435204E-05),
        		DoubleDouble.valueOf(3.44571294294967503E-05), DoubleDouble.valueOf(7.97092684075674924E-05), 
        		DoubleDouble.valueOf(1.03138236708272200E-04), DoubleDouble.valueOf(1.12466775262204158E-04),
        		DoubleDouble.valueOf(1.13103642108481389E-04), DoubleDouble.valueOf(1.08651634848774268E-04), 
        		DoubleDouble.valueOf(1.01437951597661973E-04), DoubleDouble.valueOf(9.29298396593363896E-05),
        		DoubleDouble.valueOf(8.40293133016089978E-05), DoubleDouble.valueOf(7.52727991349134062E-05), 
        		DoubleDouble.valueOf(6.69632521975730872E-05), DoubleDouble.valueOf(5.92564547323194704E-05),
        		DoubleDouble.valueOf(5.22169308826975567E-05), DoubleDouble.valueOf(4.58539485165360646E-05), 
        		DoubleDouble.valueOf(4.01445513891486808E-05), DoubleDouble.valueOf(3.50481730031328081E-05),
        		DoubleDouble.valueOf(3.05157995034346659E-05), DoubleDouble.valueOf(2.64956119950516039E-05), 
        		DoubleDouble.valueOf(2.29363633690998152E-05), DoubleDouble.valueOf(1.97893056664021636E-05),
        		DoubleDouble.valueOf(1.70091984636412623E-05), DoubleDouble.valueOf(1.45547428261524004E-05), 
        		DoubleDouble.valueOf(1.23886640995878413E-05), DoubleDouble.valueOf(1.04775876076583236E-05),
        		DoubleDouble.valueOf(8.79179954978479373E-06), DoubleDouble.valueOf(7.36465810572578444E-04), 
        		DoubleDouble.valueOf(8.72790805146193976E-04), DoubleDouble.valueOf(6.22614862573135066E-04),
        		DoubleDouble.valueOf(2.85998154194304147E-04), DoubleDouble.valueOf(3.84737672879366102E-06), 
        		DoubleDouble.valueOf(-1.87906003636971558E-04), DoubleDouble.valueOf(-2.97603646594554535E-04),
        		DoubleDouble.valueOf(-3.45998126832656348E-04), DoubleDouble.valueOf(-3.53382470916037712E-04), 
        		DoubleDouble.valueOf(-3.35715635775048757E-04), DoubleDouble.valueOf(-3.04321124789039809E-04),
        		DoubleDouble.valueOf(-2.66722723047612821E-04), DoubleDouble.valueOf(-2.27654214122819527E-04), 
        		DoubleDouble.valueOf(-1.89922611854562356E-04), DoubleDouble.valueOf(-1.55058918599093870E-04),
        		DoubleDouble.valueOf(-1.23778240761873630E-04), DoubleDouble.valueOf(-9.62926147717644187E-05), 
        		DoubleDouble.valueOf(-7.25178327714425337E-05), DoubleDouble.valueOf(-5.22070028895633801E-05),
        		DoubleDouble.valueOf(-3.50347750511900522E-05), DoubleDouble.valueOf(-2.06489761035551757E-05), 
        		DoubleDouble.valueOf(-8.70106096849767054E-06), DoubleDouble.valueOf(1.13698686675100290E-06),
        		DoubleDouble.valueOf(9.16426474122778849E-06), DoubleDouble.valueOf(1.56477785428872620E-05), 
        		DoubleDouble.valueOf(2.08223629482466847E-05), DoubleDouble.valueOf(2.48923381004595156E-05),
        		DoubleDouble.valueOf(2.80340509574146325E-05), DoubleDouble.valueOf(3.03987774629861915E-05), 
        		DoubleDouble.valueOf(3.21156731406700616E-05), DoubleDouble.valueOf(-1.80182191963885708E-03),
        		DoubleDouble.valueOf(-2.43402962938042533E-03), DoubleDouble.valueOf(-1.83422663549856802E-03), 
        		DoubleDouble.valueOf(-7.62204596354009765E-04), DoubleDouble.valueOf(2.39079475256927218E-04),
        		DoubleDouble.valueOf(9.49266117176881141E-04), DoubleDouble.valueOf(1.34467449701540359E-03), 
        		DoubleDouble.valueOf(1.48457495259449178E-03), DoubleDouble.valueOf(1.44732339830617591E-03),
        		DoubleDouble.valueOf(1.30268261285657186E-03), DoubleDouble.valueOf(1.10351597375642682E-03), 
        		DoubleDouble.valueOf(8.86047440419791759E-04), DoubleDouble.valueOf(6.73073208165665473E-04),
        		DoubleDouble.valueOf(4.77603872856582378E-04), DoubleDouble.valueOf(3.05991926358789362E-04), 
        		DoubleDouble.valueOf(1.60315694594721630E-04), DoubleDouble.valueOf(4.00749555270613286E-05),
        		DoubleDouble.valueOf(-5.66607461635251611E-05), DoubleDouble.valueOf(-1.32506186772982638E-04), 
        		DoubleDouble.valueOf(-1.90296187989614057E-04), DoubleDouble.valueOf(-2.32811450376937408E-04),
        		DoubleDouble.valueOf(-2.62628811464668841E-04), DoubleDouble.valueOf(-2.82050469867598672E-04), 
        		DoubleDouble.valueOf(-2.93081563192861167E-04), DoubleDouble.valueOf(-2.97435962176316616E-04),
        		DoubleDouble.valueOf(-2.96557334239348078E-04), DoubleDouble.valueOf(-2.91647363312090861E-04), 
        		DoubleDouble.valueOf(-2.83696203837734166E-04), DoubleDouble.valueOf(-2.73512317095673346E-04),
        		DoubleDouble.valueOf(-2.61750155806768580E-04), DoubleDouble.valueOf(6.38585891212050914E-03), 
        		DoubleDouble.valueOf(9.62374215806377941E-03), DoubleDouble.valueOf(7.61878061207001043E-03),
        		DoubleDouble.valueOf(2.83219055545628054E-03), DoubleDouble.valueOf(-2.09841352012720090E-03), 
        		DoubleDouble.valueOf(-5.73826764216626498E-03), DoubleDouble.valueOf(-7.70804244495414620E-03),
        		DoubleDouble.valueOf(-8.21011692264844401E-03), DoubleDouble.valueOf(-7.65824520346905413E-03), 
        		DoubleDouble.valueOf(-6.47209729391045177E-03), DoubleDouble.valueOf(-4.99132412004966473E-03),
        		DoubleDouble.valueOf(-3.45612289713133280E-03), DoubleDouble.valueOf(-2.01785580014170775E-03), 
        		DoubleDouble.valueOf(-7.59430686781961401E-04), DoubleDouble.valueOf(2.84173631523859138E-04),
        		DoubleDouble.valueOf(1.10891667586337403E-03), DoubleDouble.valueOf(1.72901493872728771E-03), 
        		DoubleDouble.valueOf(2.16812590802684701E-03), DoubleDouble.valueOf(2.45357710494539735E-03),
        		DoubleDouble.valueOf(2.61281821058334862E-03), DoubleDouble.valueOf(2.67141039656276912E-03), 
        		DoubleDouble.valueOf(2.65203073395980430E-03), DoubleDouble.valueOf(2.57411652877287315E-03),
        		DoubleDouble.valueOf(2.45389126236094427E-03), DoubleDouble.valueOf(2.30460058071795494E-03), 
        		DoubleDouble.valueOf(2.13684837686712662E-03), DoubleDouble.valueOf(1.95896528478870911E-03),
        		DoubleDouble.valueOf(1.77737008679454412E-03), DoubleDouble.valueOf(1.59690280765839059E-03), 
        		DoubleDouble.valueOf(1.42111975664438546E-03)};
        final DoubleDouble[] gama = new DoubleDouble[] {DoubleDouble.valueOf(6.29960524947436582E-01), 
        		DoubleDouble.valueOf(2.51984209978974633E-01), DoubleDouble.valueOf(1.54790300415655846E-01),
        		DoubleDouble.valueOf(1.10713062416159013E-01), DoubleDouble.valueOf(8.57309395527394825E-02), 
        		DoubleDouble.valueOf(6.97161316958684292E-02), DoubleDouble.valueOf(5.86085671893713576E-02),
        		DoubleDouble.valueOf(5.04698873536310685E-02), DoubleDouble.valueOf(4.42600580689154809E-02), 
        		DoubleDouble.valueOf(3.93720661543509966E-02), DoubleDouble.valueOf(3.54283195924455368E-02),
        		DoubleDouble.valueOf(3.21818857502098231E-02), DoubleDouble.valueOf(2.94646240791157679E-02), 
        		DoubleDouble.valueOf(2.71581677112934479E-02), DoubleDouble.valueOf(2.51768272973861779E-02),
        		DoubleDouble.valueOf(2.34570755306078891E-02), DoubleDouble.valueOf(2.19508390134907203E-02), 
        		DoubleDouble.valueOf(2.06210828235646240E-02), DoubleDouble.valueOf(1.94388240897880846E-02),
        		DoubleDouble.valueOf(1.83810633800683158E-02), DoubleDouble.valueOf(1.74293213231963172E-02), 
        		DoubleDouble.valueOf(1.65685837786612353E-02), DoubleDouble.valueOf(1.57865285987918445E-02),
        		DoubleDouble.valueOf(1.50729501494095594E-02), DoubleDouble.valueOf(1.44193250839954639E-02), 
        		DoubleDouble.valueOf(1.38184805735341786E-02), DoubleDouble.valueOf(1.32643378994276568E-02),
        		DoubleDouble.valueOf(1.27517121970498651E-02), DoubleDouble.valueOf(1.22761545318762767E-02),
        		DoubleDouble.valueOf(1.18338262398482403E-02)};
        final DoubleDouble ex1 = (DoubleDouble.valueOf(1.0)).divide(DoubleDouble.valueOf(3.0));
        final DoubleDouble ex2 = (DoubleDouble.valueOf(2.0)).divide(DoubleDouble.valueOf(3.0));
        final DoubleDouble hpi = DoubleDouble.PI_2;
        final DoubleDouble thpi = (DoubleDouble.valueOf(1.5)).multiply(DoubleDouble.PI);
        DoubleDouble rfnu;
        DoubleDouble test;
        DoubleDouble ac;
        DoubleDouble zbr;
        DoubleDouble zbi;
        DoubleDouble rfnu2;
        DoubleDouble fn13;
        DoubleDouble fn23;
        DoubleDouble rfn13;
        DoubleDouble w2r;
        DoubleDouble w2i;
        DoubleDouble aw2;
        int k;
        final DoubleDouble[] pr = new DoubleDouble[30];
        final DoubleDouble[] pi = new DoubleDouble[30];
        DoubleDouble sumar;
        DoubleDouble sumai;
        final DoubleDouble[] ap = new DoubleDouble[30];
        int kmax;
        final DoubleDouble[] zar = new DoubleDouble[1];
        final DoubleDouble[] zai = new DoubleDouble[1];
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        DoubleDouble sumbr;
        DoubleDouble sumbi;
        int l1;
        int l2;
        DoubleDouble btol;
        DoubleDouble atol;
        DoubleDouble pp;
        int ias;
        int ibs;
        int m;
        final DoubleDouble[] wr = new DoubleDouble[1];
        final DoubleDouble[] wi = new DoubleDouble[1];
        final DoubleDouble[] zcr = new DoubleDouble[1];
        final DoubleDouble[] zci = new DoubleDouble[1];
        final int[] idum = new int[1];
        DoubleDouble zthr;
        DoubleDouble zthi;
        DoubleDouble azth;
        DoubleDouble ang;
        DoubleDouble zetar;
        DoubleDouble zetai;
        final DoubleDouble[] rtztr = new DoubleDouble[1];
        final DoubleDouble[] rtzti = new DoubleDouble[1];
        DoubleDouble tzar;
        DoubleDouble tzai;
        DoubleDouble raw;
        DoubleDouble tfnr;
        DoubleDouble tfni;
        DoubleDouble razth;
        DoubleDouble rzthr;
        DoubleDouble rzthi;
        DoubleDouble raw2;
        DoubleDouble t2r;
        DoubleDouble t2i;
        final DoubleDouble[] upr = new DoubleDouble[14];
        final DoubleDouble[] upi = new DoubleDouble[14];
        DoubleDouble przthr;
        DoubleDouble przthi;
        DoubleDouble ptfnr;
        DoubleDouble ptfni;
        int ks;
        int kp1;
        int l;
        int lr;
        int lrp1;
        final DoubleDouble[] crr = new DoubleDouble[14];
        final DoubleDouble[] cri = new DoubleDouble[14];
        final DoubleDouble[] drr = new DoubleDouble[14];
        final DoubleDouble[] dri = new DoubleDouble[14];
        int ju;
        int jr;
        int is;
        int j;

        rfnu = fnu.reciprocal();

        // Overflow test (z/fnu too small)
        test = (DoubleDouble.valueOf(1.0E3)).multiply(tiny);
        ac = fnu.multiply(test);

        if ( ((zr.abs()).le(ac)) && ((zi.abs()).le(ac))) {
            zeta1r[0] = ((DoubleDouble.valueOf(2.0)).multiply((test.log()).abs())).add(fnu);
            zeta1i[0] = DoubleDouble.valueOf(0.0);
            zeta2r[0] = (DoubleDouble)fnu.clone();
            zeta2i[0] = DoubleDouble.valueOf(0.0);
            phir[0] = DoubleDouble.valueOf(1.0);
            phii[0] = DoubleDouble.valueOf(0.0);
            argr[0] = DoubleDouble.valueOf(1.0);
            argi[0] = DoubleDouble.valueOf(0.0);

            return;
        } // if ((Math.abs(zr) <= ac) && (Math.abs(zi) <= ac))

        zbr = zr.multiply(rfnu);
        zbi = zi.multiply(rfnu);
        rfnu2 = rfnu.multiply(rfnu);

        // Compute in the fourth quadrant
        fn13 = fnu.pow(ex1);
        fn23 = fn13.multiply(fn13);
        rfn13 = fn13.reciprocal();
        w2r = ((DoubleDouble.valueOf(1.0)).subtract(zbr.multiply(zbr))).add(zbi.multiply(zbi));
        w2i = ((DoubleDouble.valueOf(-2.0)).multiply(zbr)).multiply(zbi);
        aw2 = zabs(w2r, w2i);

        if (aw2.le(DoubleDouble.valueOf(0.25))) {

            // Power series for cabs(w2) <= 0.25
            k = 1;
            pr[0] = DoubleDouble.valueOf(1.0);
            pi[0] = DoubleDouble.valueOf(0.0);
            sumar = (DoubleDouble)gama[0].clone();
            sumai = DoubleDouble.valueOf(0.0);
            ap[0] = DoubleDouble.valueOf(1.0);

            group: {

                if (aw2.ge(tol)) {

                    for (k = 2; k <= 30; k++) {
                        pr[k - 1] = (pr[k - 2].multiply(w2r)).subtract(pi[k - 2].multiply(w2i));
                        pi[k - 1] = (pr[k - 2].multiply(w2i)).add(pi[k - 2].multiply(w2r));
                        sumar = sumar.add(pr[k - 1].multiply(gama[k - 1]));
                        sumai = sumai.add(pi[k - 1].multiply(gama[k - 1]));
                        ap[k - 1] = ap[k - 2].multiply(aw2);

                        if (ap[k - 1].lt(tol)) {
                            break group;
                        }
                    } // for (k = 2; k <= 30; k++)

                    k = 30;
                } // if (aw2 >= tol)
            } // group

            kmax = k;
            zetar = (w2r.multiply(sumar)).subtract(w2i.multiply(sumai));
            zetai = (w2r.multiply(sumai)).add(w2i.multiply(sumar));
            argr[0] = zetar.multiply(fn23);
            argi[0] = zetai.multiply(fn23);
            zsqrt(sumar, sumai, zar, zai);
            zsqrt(w2r, w2i, str, sti);
            zeta2r[0] = str[0].multiply(fnu);
            zeta2i[0] = sti[0].multiply(fnu);
            str[0] = (DoubleDouble.valueOf(1.0)).add(ex2.multiply( (zetar.multiply(zar[0])).subtract(zetai.multiply(zai[0]))));
            sti[0] = ex2.multiply( (zetar.multiply(zai[0])).add(zetai.multiply(zar[0])));
            zeta1r[0] = (str[0].multiply(zeta2r[0])).subtract(sti[0].multiply(zeta2i[0]));
            zeta1i[0] = (str[0].multiply(zeta2i[0])).add(sti[0].multiply(zeta2r[0]));
            zar[0] = zar[0].add(zar[0]);
            zai[0] = zai[0].add(zai[0]);
            zsqrt(zar[0], zai[0], str, sti);
            phir[0] = str[0].multiply(rfn13);
            phii[0] = sti[0].multiply(rfn13);

            if (ipmtr == 1) {
                return;
            }

            // Sum series for asum and bsum
            sumbr = DoubleDouble.valueOf(0.0);
            sumbi = DoubleDouble.valueOf(0.0);

            for (k = 1; k <= kmax; k++) {
                sumbr = sumbr.add(pr[k - 1].multiply(beta[k - 1]));
                sumbi = sumbi.add(pi[k - 1].multiply(beta[k - 1]));
            } // for (k = 1; k <= kmax; k++)

            asumr[0] = DoubleDouble.valueOf(0.0);
            asumi[0] = DoubleDouble.valueOf(0.0);
            bsumr[0] = (DoubleDouble)sumbr.clone();
            bsumi[0] = (DoubleDouble)sumbi.clone();
            l1 = 0;
            l2 = 30;
            btol = tol.multiply((bsumr[0].abs()).add(bsumi[0].abs()));
            atol = (DoubleDouble)tol.clone();
            pp = DoubleDouble.valueOf(1.0);
            ias = 0;
            ibs = 0;

            if (rfnu2.lt(tol)) {
                asumr[0] = asumr[0].add(DoubleDouble.valueOf(1.0));
                pp = rfnu.multiply(rfn13);
                bsumr[0] = bsumr[0].multiply(pp);
                bsumi[0] = bsumi[0].multiply(pp);

                return;
            } // if (rfnu2 < tol)

            for (is = 2; is <= 7; is++) {
                atol = atol.divide(rfnu2);
                pp = pp.multiply(rfnu2);

                if (ias != 1) {
                    sumar = DoubleDouble.valueOf(0.0);
                    sumai = DoubleDouble.valueOf(0.0);

                    for (k = 1; k <= kmax; k++) {
                        m = l1 + k;
                        sumar = sumar.add(pr[k - 1].multiply(alfa[m - 1]));
                        sumai = sumai.add(pi[k - 1].multiply(alfa[m - 1]));

                        if (ap[k - 1].lt(atol)) {
                            break;
                        }
                    } // for (k = 1; k <= kmax; k++)

                    asumr[0] = asumr[0].add(sumar.multiply(pp));
                    asumi[0] = asumi[0].add(sumai.multiply(pp));

                    if (pp.lt(tol)) {
                        ias = 1;
                    }
                } // if (ias != 1)

                if (ibs != 1) {
                    sumbr = DoubleDouble.valueOf(0.0);
                    sumbi = DoubleDouble.valueOf(0.0);

                    for (k = 1; k <= kmax; k++) {
                        m = l2 + k;
                        sumbr = sumbr.add(pr[k - 1].multiply(beta[m - 1]));
                        sumbi = sumbi.add(pi[k - 1].multiply(beta[m - 1]));

                        if (ap[k - 1].lt(atol)) {
                            break;
                        }
                    } // for (k = 1; k <= kmax; k++)

                    bsumr[0] = bsumr[0].add(sumbr.multiply(pp));
                    bsumi[0] = bsumi[0].add(sumbi.multiply(pp));

                    if (pp.lt(btol)) {
                        ibs = 1;
                    }
                } // if (ibs != 1)

                if ( (ias == 1) && (ibs == 1)) {
                    asumr[0] = asumr[0].add(DoubleDouble.valueOf(1.0));
                    pp = rfnu.multiply(rfn13);
                    bsumr[0] = bsumr[0].multiply(pp);
                    bsumi[0] = bsumi[0].multiply(pp);

                    return;
                } // if ((ias == 1) && (ibs == 1))

                l1 = l1 + 30;
                l2 = l2 + 30;
            } // for (is = 2; is <= 7; is++)

            asumr[0] = asumr[0].add(DoubleDouble.valueOf(1.0));
            pp = rfnu.multiply(rfn13);
            bsumr[0] = bsumr[0].multiply(pp);
            bsumi[0] = bsumi[0].multiply(pp);

            return;
        } // if (aw2 <= 0.25)

        // CABS(w2) > 0.25
        zsqrt(w2r, w2i, wr, wi);

        if (wr[0].lt(DoubleDouble.valueOf(0.0))) {
            wr[0] = DoubleDouble.valueOf(0.0);
        }

        if (wi[0].lt(DoubleDouble.valueOf(0.0))) {
            wi[0] = DoubleDouble.valueOf(0.0);
        }

        str[0] = wr[0].add(DoubleDouble.valueOf(1.0));
        sti[0] = (DoubleDouble)wi[0].clone();
        zdiv(str[0], sti[0], zbr, zbi, zar, zai);
        zlog(zar[0], zai[0], zcr, zci, idum);

        if (zci[0].lt(DoubleDouble.valueOf(0.0))) {
            zci[0] = DoubleDouble.valueOf(0.0);
        }

        if (zci[0].gt(hpi)) {
            zci[0] = (DoubleDouble)hpi.clone();
        }

        if (zcr[0].lt(DoubleDouble.valueOf(0.0))) {
            zcr[0] = DoubleDouble.valueOf(0.0);
        }

        zthr = (zcr[0].subtract(wr[0])).multiply(DoubleDouble.valueOf(1.5));
        zthi = (zci[0].subtract(wi[0])).multiply(DoubleDouble.valueOf(1.5));
        zeta1r[0] = zcr[0].multiply(fnu);
        zeta1i[0] = zci[0].multiply(fnu);
        zeta2r[0] = wr[0].multiply(fnu);
        zeta2i[0] = wi[0].multiply(fnu);
        azth = zabs(zthr, zthi);
        ang = (DoubleDouble)thpi.clone();

        if ( (zthr.lt(DoubleDouble.valueOf(0.0))) || (zthi.ge(DoubleDouble.valueOf(0.0)))) {
            ang = (DoubleDouble)hpi.clone();

            if (zthr.ne(DoubleDouble.valueOf(0.0))) {
                ang = (zthi.divide(zthr)).atan();

                if (zthr.lt(DoubleDouble.valueOf(0.0))) {
                    ang = ang.add(DoubleDouble.PI);
                }
            } // if (zthr != 0.0)
        } // if ((zthr < 0.0) || (zthi >= 0.0))

        pp = azth.pow(ex2);
        ang = ang.multiply(ex2);
        zetar = pp.multiply(ang.cos());
        zetai = pp.multiply(ang.sin());

        if (zetai.lt(DoubleDouble.valueOf(0.0))) {
            zetai = DoubleDouble.valueOf(0.0);
        }

        argr[0] = zetar.multiply(fn23);
        argi[0] = zetai.multiply(fn23);
        zdiv(zthr, zthi, zetar, zetai, rtztr, rtzti);
        zdiv(rtztr[0], rtzti[0], wr[0], wi[0], zar, zai);
        tzar = zar[0].add(zar[0]);
        tzai = zai[0].add(zai[0]);
        zsqrt(tzar, tzai, str, sti);
        phir[0] = str[0].multiply(rfn13);
        phii[0] = sti[0].multiply(rfn13);

        if (ipmtr == 1) {
            return;
        }

        raw = (aw2.sqrt()).reciprocal();
        str[0] = wr[0].multiply(raw);
        sti[0] = (wi[0].negate()).multiply(raw);
        tfnr = (str[0].multiply(rfnu)).multiply(raw);
        tfni = (sti[0].multiply(rfnu)).multiply(raw);
        razth = azth.reciprocal();
        str[0] = zthr.multiply(razth);
        sti[0] = (zthi.negate()).multiply(razth);
        rzthr = (str[0].multiply(razth)).multiply(rfnu);
        rzthi = (sti[0].multiply(razth)).multiply(rfnu);
        zcr[0] = rzthr.multiply(ar[1]);
        zci[0] = rzthi.multiply(ar[1]);
        raw2 = aw2.reciprocal();
        str[0] = w2r.multiply(raw2);
        sti[0] = (w2i.negate()).multiply(raw2);
        t2r = str[0].multiply(raw2);
        t2i = sti[0].multiply(raw2);
        str[0] = (t2r.multiply(c[1])).add(c[2]);
        sti[0] = t2i.multiply(c[1]);
        upr[1] = (str[0].multiply(tfnr)).subtract(sti[0].multiply(tfni));
        upi[1] = (str[0].multiply(tfni)).add(sti[0].multiply(tfnr));
        bsumr[0] = upr[1].add(zcr[0]);
        bsumi[0] = upi[1].add(zci[0]);
        asumr[0] = DoubleDouble.valueOf(0.0);
        asumi[0] = DoubleDouble.valueOf(0.0);

        if (rfnu.lt(tol)) {
            asumr[0] = asumr[0].add(DoubleDouble.valueOf(1.0));
            str[0] = (bsumr[0].negate()).multiply(rfn13);
            sti[0] = (bsumi[0].negate()).multiply(rfn13);
            zdiv(str[0], sti[0], rtztr[0], rtzti[0], bsumr, bsumi);

            return;
        } // if (rfnu < tol)

        przthr = (DoubleDouble)rzthr.clone();
        przthi = (DoubleDouble)rzthi.clone();
        ptfnr = (DoubleDouble)tfnr.clone();
        ptfni = (DoubleDouble)tfni.clone();
        upr[0] = DoubleDouble.valueOf(1.0);
        upi[0] = DoubleDouble.valueOf(0.0);
        pp = DoubleDouble.valueOf(1.0);
        btol = tol.multiply((bsumr[0].abs()).add(bsumi[0].abs()));
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
                zar[0] = (DoubleDouble)c[l - 1].clone();
                zai[0] = DoubleDouble.valueOf(0.0);

                for (j = 2; j <= kp1; j++) {
                    l = l + 1;
                    str[0] = ((zar[0].multiply(t2r)).subtract(t2i.multiply(zai[0]))).add(c[l - 1]);
                    zai[0] = (zar[0].multiply(t2i)).add(zai[0].multiply(t2r));
                    zar[0] = (DoubleDouble)str[0].clone();
                } // for (j = 2; j <= kp1; j++)

                str[0] = (ptfnr.multiply(tfnr)).subtract(ptfni.multiply(tfni));
                ptfni = (ptfnr.multiply(tfni)).add(ptfni.multiply(tfnr));
                ptfnr = (DoubleDouble)str[0].clone();
                upr[kp1 - 1] = (ptfnr.multiply(zar[0])).subtract(ptfni.multiply(zai[0]));
                upi[kp1 - 1] = (ptfni.multiply(zar[0])).add(ptfnr.multiply(zai[0]));
                crr[ks - 1] = przthr.multiply(br[ks]);
                cri[ks - 1] = przthi.multiply(br[ks]);
                str[0] = (przthr.multiply(rzthr)).subtract(przthi.multiply(rzthi));
                przthi = (przthr.multiply(rzthi)).add(przthi.multiply(rzthr));
                przthr = (DoubleDouble)str[0].clone();
                drr[ks - 1] = przthr.multiply(ar[ks + 1]);
                dri[ks - 1] = przthi.multiply(ar[ks + 1]);
            } // for (k = lr; k <= lrp1; k++)

            pp = pp.multiply(rfnu2);

            if (ias != 1) {
                sumar = (DoubleDouble)upr[lrp1 - 1].clone();
                sumai = (DoubleDouble)upi[lrp1 - 1].clone();
                ju = lrp1;

                for (jr = 1; jr <= lr; jr++) {
                    ju = ju - 1;
                    sumar = (sumar.add(crr[jr - 1].multiply(upr[ju - 1]))).subtract(cri[jr - 1].multiply(upi[ju - 1]));
                    sumai = (sumai.add(crr[jr - 1].multiply(upi[ju - 1]))).add(cri[jr - 1].multiply(upr[ju - 1]));
                } // for (jr = 1; jr <= lr; jr++)

                asumr[0] = asumr[0].add(sumar);
                asumi[0] = asumi[0].add(sumai);
                test = (sumar.abs()).add(sumai.abs());

                if ( (pp.lt(tol)) && (test.lt(tol))) {
                    ias = 1;
                }
            } // if (ias != 1)

            if (ibs != 1) {
                sumbr = (upr[lr + 1].add(upr[lrp1 - 1].multiply(zcr[0]))).subtract(upi[lrp1 - 1].multiply(zci[0]));
                sumbi = (upi[lr + 1].add(upr[lrp1 - 1].multiply(zci[0]))).add(upi[lrp1 - 1].multiply(zcr[0]));
                ju = lrp1;

                for (jr = 1; jr <= lr; jr++) {
                    ju = ju - 1;
                    sumbr = (sumbr.add(drr[jr - 1].multiply(upr[ju - 1]))).subtract(dri[jr - 1].multiply(upi[ju - 1]));
                    sumbi = (sumbi.add(drr[jr - 1].multiply(upi[ju - 1]))).add(dri[jr - 1].multiply(upr[ju - 1]));
                } // for (jr = 1; jr <= lr; jr++)

                bsumr[0] = bsumr[0].add(sumbr);
                bsumi[0] = bsumi[0].add(sumbi);
                test = (sumbr.abs()).add(sumbi.abs());

                if ( (pp.lt(btol)) && (test.lt(btol))) {
                    ibs = 1;
                }
            } // if (ibs != 1)

            if ( (ias == 1) && (ibs == 1)) {
                break;
            }
        } // for (lr = 2; lr <= 12; lr+=2)

        asumr[0] = asumr[0].add(DoubleDouble.valueOf(1.0));
        str[0] = (bsumr[0].negate()).multiply(rfn13);
        sti[0] = (bsumi[0].negate()).multiply(rfn13);
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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     * @param nlast int[]
     * @param fnul DoubleDouble
     */
    private void zuni1(final DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n, final DoubleDouble[] yr,
            final DoubleDouble[] yi, final int[] nz, final int[] nlast, final DoubleDouble fnul) {
        final DoubleDouble[] bry = new DoubleDouble[3];
        final DoubleDouble[] cwrkr = new DoubleDouble[16];
        final DoubleDouble[] cwrki = new DoubleDouble[16];
        final DoubleDouble[] cssr = new DoubleDouble[3];
        final DoubleDouble[] csrr = new DoubleDouble[3];
        final DoubleDouble[] cyr = new DoubleDouble[2];
        final DoubleDouble[] cyi = new DoubleDouble[2];
        int nd;
        DoubleDouble cscl;
        DoubleDouble crsc;
        DoubleDouble fn;
        int init[] = new int[1];
        final DoubleDouble[] phir = new DoubleDouble[1];
        final DoubleDouble[] phii = new DoubleDouble[1];
        final DoubleDouble[] zeta1r = new DoubleDouble[1];
        final DoubleDouble[] zeta1i = new DoubleDouble[1];
        final DoubleDouble[] zeta2r = new DoubleDouble[1];
        final DoubleDouble[] zeta2i = new DoubleDouble[1];
        final DoubleDouble[] sumr = new DoubleDouble[1];
        final DoubleDouble[] sumi = new DoubleDouble[1];
        DoubleDouble str;
        DoubleDouble sti;
        DoubleDouble rast;
        DoubleDouble s1r;
        DoubleDouble s1i;
        DoubleDouble rs1;
        DoubleDouble aphi;
        DoubleDouble s2r;
        DoubleDouble s2i;
        final int[] nw = new int[1];
        int m;
        DoubleDouble rzr;
        DoubleDouble rzi;
        DoubleDouble c1r;
        DoubleDouble ascle;
        int k;
        int i;
        DoubleDouble c2r;
        DoubleDouble c2i;
        DoubleDouble c2m;
        final int[] nuf = new int[1];
        int nn;
        int iflag = 1;

        nz[0] = 0;
        nd = n;
        nlast[0] = 0;

        // Computed values with exponents between alim and elim in magnitude
        // are scaled to keep intermediate arithmetic on scale,
        // exp(alim) = tol * exp(elim)
        cscl = tol.reciprocal();
        crsc = (DoubleDouble)tol.clone();
        cssr[0] = (DoubleDouble)cscl.clone();
        cssr[1] = DoubleDouble.valueOf(1.0);
        cssr[2] = (DoubleDouble)crsc.clone();
        csrr[0] = (DoubleDouble)crsc.clone();
        csrr[1] = DoubleDouble.valueOf(1.0);
        csrr[2] = (DoubleDouble)cscl.clone();
        bry[0] = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).divide(tol);

        // Check for underflow and overflow on first member
        fn = fnu.max(DoubleDouble.valueOf(1.0));
        init[0] = 0;
        zunik(zr, zi, fn, 1, 1, init, phir, phii, zeta1r, zeta1i, zeta2r, zeta2i, sumr, sumi, cwrkr, cwrki);

        if (kode != 1) {
            str = zr.add(zeta2r[0]);
            sti = zi.add(zeta2i[0]);
            rast = fn.divide(zabs(str, sti));
            str = (str.multiply(rast)).multiply(rast);
            sti = ((sti.negate()).multiply(rast)).multiply(rast);
            s1r = (zeta1r[0].negate()).add(str);
            s1i = (zeta1i[0].negate()).add(sti);
        } // if (kode != 1)
        else { // kode == 1
            s1r = (zeta1r[0].negate()).add(zeta2r[0]);
            s1i = (zeta1i[0].negate()).add(zeta2i[0]);
        } // else kode == 1

        rs1 = (DoubleDouble)s1r.clone();

        if ((rs1.abs()).gt(elim)) {

            if (rs1.gt(DoubleDouble.valueOf(0.0))) {
                nz[0] = -1;

                return;
            } // if (rs1 > 0.0)

            nz[0] = n;

            for (i = 1; i <= n; i++) {
                yr[i - 1] = DoubleDouble.valueOf(0.0);
                yi[i - 1] = DoubleDouble.valueOf(0.0);
            } // for (i = 1; i <= n; i++)

            return;
        } // if (Math.abs(rs1) > elim)

        while (true) {
            nn = Math.min(2, nd);

            group: {

                for (i = 1; i <= nn; i++) {
                    fn = fnu.add(DoubleDouble.valueOf(nd - i));
                    init[0] = 0;
                    zunik(zr, zi, fn, 1, 0, init, phir, phii, zeta1r, zeta1i, zeta2r, zeta2i, sumr, sumi, cwrkr, cwrki);

                    if (kode != 1) {
                        str = zr.add(zeta2r[0]);
                        sti = zi.add(zeta2i[0]);
                        rast = fn.divide(zabs(str, sti));
                        str = (str.multiply(rast)).multiply(rast);
                        sti = ((sti.negate()).multiply(rast)).multiply(rast);
                        s1r = (zeta1r[0].negate()).add(str);
                        s1i = ((zeta1i[0].negate()).add(sti)).add(zi);
                    } // if (kode != 1)
                    else { // kode == 1
                        s1r = (zeta1r[0].negate()).add(zeta2r[0]);
                        s1i = (zeta1i[0].negate()).add(zeta2i[0]);
                    } // else kode == 1

                    // Test for underflow and overflow
                    rs1 = (DoubleDouble)s1r.clone();

                    if ((rs1.abs()).gt(elim)) {
                        break group;
                    } // if (Math.abs(rs1) > elim)

                    if (i == 1) {
                        iflag = 2;
                    } // if (i == 1)

                    if ((rs1.abs()).ge(alim)) {

                        // Refine test and scale
                        aphi = zabs(phir[0], phii[0]);
                        rs1 = rs1.add(aphi.log());

                        if ((rs1.abs()).gt(elim)) {
                            break group;
                        } // if (Math.abs(rs1) > elim)

                        if (i == 1) {
                            iflag = 1;
                        } // if (i == 1)

                        if ( (rs1.ge(DoubleDouble.valueOf(0.0))) && (i == 1)) {
                            iflag = 3;
                        } // if ((rs1 >= 0.0) && (i == 1))
                    } // if (Math.abs(rs1) >= alim)

                    // Scale s1 if cabs(s1) < ascle
                    s2r = (phir[0].multiply(sumr[0])).subtract(phii[0].multiply(sumi[0]));
                    s2i = (phir[0].multiply(sumi[0])).add(phii[0].multiply(sumr[0]));
                    str = (s1r.exp()).multiply(cssr[iflag - 1]);
                    s1r = str.multiply(s1i.cos());
                    s1i = str.multiply(s1i.sin());
                    str = (s2r.multiply(s1r)).subtract(s2i.multiply(s1i));
                    s2i = (s2r.multiply(s1i)).add(s2i.multiply(s1r));
                    s2r = (DoubleDouble)str.clone();

                    if (iflag == 1) {
                        zuchk(s2r, s2i, nw, bry[0]);

                        if (nw[0] != 0) {
                            break group;
                        } // if (nw[0] != 0)
                    } // if (iflag == 1)

                    cyr[i - 1] = (DoubleDouble)s2r.clone();
                    cyi[i - 1] = (DoubleDouble)s2i.clone();
                    m = nd - i + 1;
                    yr[m - 1] = s2r.multiply(csrr[iflag - 1]);
                    yi[m - 1] = s2i.multiply(csrr[iflag - 1]);
                } // for (i = 1; i <= nn; i++)

                if (nd <= 2) {
                    return;
                } // if (nd <= 2)

                rast = (zabs(zr, zi)).reciprocal();
                str = zr.multiply(rast);
                sti = (zi.negate()).multiply(rast);
                rzr = (str.add(str)).multiply(rast);
                rzi = (sti.add(sti)).multiply(rast);
                bry[1] = bry[0].reciprocal();
                bry[2] = DoubleDouble.valueOf(Double.MAX_VALUE);
                s1r = (DoubleDouble)cyr[0].clone();
                s1i = (DoubleDouble)cyi[0].clone();
                s2r = (DoubleDouble)cyr[1].clone();
                s2i = (DoubleDouble)cyi[1].clone();
                c1r = (DoubleDouble)csrr[iflag - 1].clone();
                ascle = (DoubleDouble)bry[iflag - 1].clone();
                k = nd - 2;
                fn = DoubleDouble.valueOf(k);

                for (i = 3; i <= nd; i++) {
                    c2r = (DoubleDouble)s2r.clone();
                    c2i = (DoubleDouble)s2i.clone();
                    s2r = s1r.add( (fnu.add(fn)).multiply( (rzr.multiply(c2r)).subtract(rzi.multiply(c2i))));
                    s2i = s1i.add( (fnu.add(fn)).multiply( (rzr.multiply(c2i)).add(rzi.multiply(c2r))));
                    s1r = (DoubleDouble)c2r.clone();
                    s1i = (DoubleDouble)c2i.clone();
                    c2r = s2r.multiply(c1r);
                    c2i = s2i.multiply(c1r);
                    yr[k - 1] = (DoubleDouble)c2r.clone();
                    yi[k - 1] = (DoubleDouble)c2i.clone();
                    k = k - 1;
                    fn = fn.subtract(DoubleDouble.valueOf(1.0));

                    if (iflag >= 3) {
                        continue;
                    } // if (iflag >= 3)

                    str = c2r.abs();
                    sti = c2i.abs();
                    c2m = str.max(sti);

                    if (c2m.le(ascle)) {
                        continue;
                    } // if (c2m <= ascle)

                    iflag = iflag + 1;
                    ascle = (DoubleDouble)bry[iflag - 1].clone();
                    s1r = s1r.multiply(c1r);
                    s1i = s1i.multiply(c1r);
                    s2r = (DoubleDouble)c2r.clone();
                    s2i = (DoubleDouble)c2i.clone();
                    s1r = s1r.multiply(cssr[iflag - 1]);
                    s1i = s1i.multiply(cssr[iflag - 1]);
                    s2r = s2r.multiply(cssr[iflag - 1]);
                    s2i = s2i.multiply(cssr[iflag - 1]);
                    c1r = (DoubleDouble)csrr[iflag - 1].clone();
                } // for (i = 3; i <= nd; i++)

                return;
            } // group

            // Set underflow and update parameters
            if (rs1.gt(DoubleDouble.valueOf(0.0))) {
                nz[0] = -1;

                return;
            } // if (rs1 > 0.0)

            yr[nd - 1] = DoubleDouble.valueOf(0.0);
            yi[nd - 1] = DoubleDouble.valueOf(0.0);
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

            fn = fnu.add(DoubleDouble.valueOf(nd - 1.0));

            if (fn.lt(fnul)) {
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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     * @param nlast int[]
     * @param fnul DoubleDouble
     */
    private void zuni2(DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int n, final DoubleDouble[] yr,
            final DoubleDouble[] yi, final int[] nz, final int[] nlast, final DoubleDouble fnul) {
        final DoubleDouble[] bry = new DoubleDouble[3];
        final DoubleDouble[] cipr = new DoubleDouble[] {DoubleDouble.valueOf(1.0), DoubleDouble.valueOf(0.0), 
        		DoubleDouble.valueOf(-1.0), DoubleDouble.valueOf(0.0)};
        final DoubleDouble[] cipi = new DoubleDouble[] {DoubleDouble.valueOf(0.0), DoubleDouble.valueOf(1.0), 
        		DoubleDouble.valueOf(0.0), DoubleDouble.valueOf(-1.0)};
        final DoubleDouble[] cssr = new DoubleDouble[3];
        final DoubleDouble[] csrr = new DoubleDouble[3];
        final DoubleDouble[] cyr = new DoubleDouble[2];
        final DoubleDouble[] cyi = new DoubleDouble[2];
        final DoubleDouble hpi = DoubleDouble.PI_2;
        final DoubleDouble aic = DoubleDouble.valueOf(1.265512123484645396);
        int nd;
        DoubleDouble cscl;
        DoubleDouble crsc;
        DoubleDouble znr;
        DoubleDouble zni;
        DoubleDouble zbr;
        DoubleDouble zbi;
        DoubleDouble cidi;
        int inu;
        DoubleDouble ang;
        DoubleDouble c2r;
        DoubleDouble c2i;
        DoubleDouble car;
        DoubleDouble sar;
        int in;
        DoubleDouble str;
        DoubleDouble fn;
        final DoubleDouble[] phir = new DoubleDouble[1];
        final DoubleDouble[] phii = new DoubleDouble[1];
        final DoubleDouble[] argr = new DoubleDouble[1];
        final DoubleDouble[] argi = new DoubleDouble[1];
        final DoubleDouble[] zeta1r = new DoubleDouble[1];
        final DoubleDouble[] zeta1i = new DoubleDouble[1];
        final DoubleDouble[] zeta2r = new DoubleDouble[1];
        final DoubleDouble[] zeta2i = new DoubleDouble[1];
        final DoubleDouble[] asumr = new DoubleDouble[1];
        final DoubleDouble[] asumi = new DoubleDouble[1];
        final DoubleDouble[] bsumr = new DoubleDouble[1];
        final DoubleDouble[] bsumi = new DoubleDouble[1];
        DoubleDouble sti;
        DoubleDouble rast;
        DoubleDouble s1r;
        DoubleDouble s1i;
        DoubleDouble rs1;
        int nn;
        int i;
        int iflag = 1;
        DoubleDouble aphi;
        DoubleDouble aarg;
        final int[] nw = new int[1];
        int j;
        DoubleDouble c1r;
        DoubleDouble ascle;
        int k;
        DoubleDouble c2m;
        final int[] idum = new int[1];
        final DoubleDouble[] air = new DoubleDouble[1];
        final DoubleDouble[] aii = new DoubleDouble[1];
        final int[] nai = new int[1];
        final DoubleDouble[] dair = new DoubleDouble[1];
        final DoubleDouble[] daii = new DoubleDouble[1];
        final int[] ndai = new int[1];
        DoubleDouble s2r;
        DoubleDouble s2i;
        DoubleDouble raz;
        DoubleDouble rzr;
        DoubleDouble rzi;
        final int[] nuf = new int[1];

        nz[0] = 0;
        nd = n;
        nlast[0] = 0;

        // Computed values with exponents between alim and elim in magnitude
        // are scaled to keep intermediate arithmetic on scale,
        // exp(alim) = tol * exp(elim)
        cscl = tol.reciprocal();
        crsc = (DoubleDouble)tol.clone();
        cssr[0] = (DoubleDouble)cscl.clone();
        cssr[1] = DoubleDouble.valueOf(1.0);
        cssr[2] = (DoubleDouble)crsc.clone();
        csrr[0] = (DoubleDouble)crsc.clone();
        csrr[1] = DoubleDouble.valueOf(1.0);
        csrr[2] = (DoubleDouble)cscl.clone();
        bry[0] = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).divide(tol);

        // zn is in the right half plane after rotation by ci or -ci
        znr = (DoubleDouble)zi.clone();
        zni = zr.negate();
        zbr = (DoubleDouble)zr.clone();
        zbi = (DoubleDouble)zi.clone();
        cidi = DoubleDouble.valueOf(-1.0);
        inu = fnu.intValue();
        ang = hpi.multiply(fnu.subtract(DoubleDouble.valueOf(inu)));
        c2r = ang.cos();
        c2i = ang.sin();
        car = (DoubleDouble)c2r.clone();
        sar = (DoubleDouble)c2i.clone();
        in = inu + n - 1;
        in = (in % 4) + 1;
        str = (c2r.multiply(cipr[in - 1])).subtract(c2i.multiply(cipi[in - 1]));
        c2i = (c2r.multiply(cipi[in - 1])).add(c2i.multiply(cipr[in - 1]));
        c2r = (DoubleDouble)str.clone();

        if (zi.le(DoubleDouble.valueOf(0.0))) {
            znr = znr.negate();
            zbi = zbi.negate();
            cidi = cidi.negate();
            c2i = c2i.negate();
        } // if (zi <= 0.0)

        // Check for underflow and overflow on first member
        fn = fnu.max(DoubleDouble.valueOf(1.0));
        zunhj(znr, zni, fn, 1, phir, phii, argr, argi, zeta1r, zeta1i, zeta2r, zeta2i, asumr, asumi, bsumr, bsumi);

        if (kode != 1) {
            str = zbr.add(zeta2r[0]);
            sti = zbi.add(zeta2i[0]);
            rast = fn.divide(zabs(str, sti));
            str = (str.multiply(rast)).multiply(rast);
            sti = ((sti.negate()).multiply(rast)).multiply(rast);
            s1r = (zeta1r[0].negate()).add(str);
            s1i = (zeta1i[0].negate()).add(sti);
        } // if (kode != 1)
        else { // kode == 1
            s1r = (zeta1r[0].negate()).add(zeta2r[0]);
            s1i = (zeta1i[0].negate()).add(zeta2i[0]);
        } // else kode == 1

        rs1 = (DoubleDouble)s1r.clone();

        if ((rs1.abs()).gt(elim)) {

            if (rs1.gt(DoubleDouble.valueOf(0.0))) {
                nz[0] = -1;

                return;
            } // if (rs1 > 0.0)

            nz[0] = n;

            for (i = 1; i <= n; i++) {
                yr[i - 1] = DoubleDouble.valueOf(0.0);
                yi[i - 1] = DoubleDouble.valueOf(0.0);
            } // for (i = 1; i <= n; i++)

            return;
        } // if (Math.abs(rs1) > elim)

        while (true) {
            nn = Math.min(2, nd);

            group: {

                for (i = 1; i <= nn; i++) {
                    fn = fnu.add(DoubleDouble.valueOf(nd - i));
                    zunhj(znr, zni, fn, 0, phir, phii, argr, argi, zeta1r, zeta1i, zeta2r, zeta2i, asumr, asumi, bsumr,
                            bsumi);

                    if (kode != 1) {
                        str = zbr.add(zeta2r[0]);
                        sti = zbi.add(zeta2i[0]);
                        rast = fn.divide(zabs(str, sti));
                        str = (str.multiply(rast)).multiply(rast);
                        sti = ((sti.negate()).multiply(rast)).multiply(rast);
                        s1r = (zeta1r[0].negate()).add(str);
                        s1i = ((zeta1i[0].negate()).add(sti)).add(zi.abs());
                    } // if (kode != 1)
                    else { // kode == 1
                        s1r = (zeta1r[0].negate()).add(zeta2r[0]);
                        s1i = (zeta1i[0].negate()).add(zeta2i[0]);
                    } // else kode == 1

                    // Test for underflow and overflow
                    rs1 = (DoubleDouble)s1r.clone();

                    if ((rs1.abs()).gt(elim)) {
                        break group;
                    } // if (Math.abs(rs1) > elim)

                    if (i == 1) {
                        iflag = 2;
                    } // if (i == 1)

                    if ((rs1.abs()).ge(alim)) {

                        // Refine test and scale
                        aphi = zabs(phir[0], phii[0]);
                        aarg = zabs(argr[0], argi[0]);
                        rs1 = ((rs1.add(aphi.log())).subtract((DoubleDouble.valueOf(0.25)).multiply(aarg.log()))).subtract(aic);

                        if ((rs1.abs()).gt(elim)) {
                            break group;
                        } // if (Math.abs(rs1) > elim)

                        if (i == 1) {
                            iflag = 1;
                        } // if (i == 1)

                        if ( (rs1.ge(DoubleDouble.valueOf(0.0))) && (i == 1)) {
                            iflag = 3;
                        } // if ((rs1 >= 0.0) && (i == 1))
                    } // if Math.abs(rs1) >= alim)

                    // Scale s1 to keep intermediate artihmetic on scale near
                    // exponent extremes
                    zairy(argr[0], argi[0], 0, 2, air, aii, nai, idum);
                    zairy(argr[0], argi[0], 1, 2, dair, daii, ndai, idum);
                    str = (dair[0].multiply(bsumr[0])).subtract(daii[0].multiply(bsumi[0]));
                    sti = (dair[0].multiply(bsumi[0])).add(daii[0].multiply(bsumr[0]));
                    str = str.add( (air[0].multiply(asumr[0])).subtract(aii[0].multiply(asumi[0])));
                    sti = sti.add( (air[0].multiply(asumi[0])).add(aii[0].multiply(asumr[0])));
                    s2r = (phir[0].multiply(str)).subtract(phii[0].multiply(sti));
                    s2i = (phir[0].multiply(sti)).add(phii[0].multiply(str));
                    str = (s1r.exp()).multiply(cssr[iflag - 1]);
                    s1r = str.multiply(s1i.cos());
                    s1i = str.multiply(s1i.sin());
                    str = (s2r.multiply(s1r)).subtract(s2i.multiply(s1i));
                    s2i = (s2r.multiply(s1i)).add(s2i.multiply(s1r));
                    s2r = (DoubleDouble)str.clone();

                    if (iflag == 1) {
                        zuchk(s2r, s2i, nw, bry[0]);

                        if (nw[0] != 0) {
                            break group;
                        } // if (nw[0] != 0)
                    } // if (iflag == 1)

                    if (zi.le(DoubleDouble.valueOf(0.0))) {
                        s2i = s2i.negate();
                    } // if (zi <= 0.0)

                    str = (s2r.multiply(c2r)).subtract(s2i.multiply(c2i));
                    s2i = (s2r.multiply(c2i)).add(s2i.multiply(c2r));
                    s2r = (DoubleDouble)str.clone();
                    cyr[i - 1] = (DoubleDouble)s2r.clone();
                    cyi[i - 1] = (DoubleDouble)s2i.clone();
                    j = nd - i + 1;
                    yr[j - 1] = s2r.multiply(csrr[iflag - 1]);
                    yi[j - 1] = s2i.multiply(csrr[iflag - 1]);
                    str = (c2i.negate()).multiply(cidi);
                    c2i = c2r.multiply(cidi);
                    c2r = (DoubleDouble)str.clone();
                } // for (i = 1; i <= nn; i++)

                if (nd <= 2) {
                    return;
                } // if (nd <= 2)

                raz = (zabs(zr, zi)).reciprocal();
                str = zr.multiply(raz);
                sti = (zi.negate()).multiply(raz);
                rzr = (str.add(str)).multiply(raz);
                rzi = (sti.add(sti)).multiply(raz);
                bry[1] = bry[0].reciprocal();
                bry[2] = DoubleDouble.valueOf(Double.MAX_VALUE);
                s1r = (DoubleDouble)cyr[0].clone();
                s1i = (DoubleDouble)cyi[0].clone();
                s2r = (DoubleDouble)cyr[1].clone();
                s2i = (DoubleDouble)cyi[1].clone();
                c1r = (DoubleDouble)csrr[iflag - 1].clone();
                ascle = (DoubleDouble)bry[iflag - 1].clone();
                k = nd - 2;
                fn = DoubleDouble.valueOf(k);

                for (i = 3; i <= nd; i++) {
                    c2r = (DoubleDouble)s2r.clone();
                    c2i = (DoubleDouble)s2i.clone();
                    s2r = s1r.add( (fnu.add(fn)).multiply( (rzr.multiply(c2r)).subtract(rzi.multiply(c2i))));
                    s2i = s1i.add( (fnu.add(fn)).multiply( (rzr.multiply(c2i)).add(rzi.multiply(c2r))));
                    s1r = (DoubleDouble)c2r.clone();
                    s1i = (DoubleDouble)c2i.clone();
                    c2r = s2r.multiply(c1r);
                    c2i = s2i.multiply(c1r);
                    yr[k - 1] = (DoubleDouble)c2r.clone();
                    yi[k - 1] = (DoubleDouble)c2i.clone();
                    k = k - 1;
                    fn = fn.subtract(DoubleDouble.valueOf(1.0));

                    if (iflag >= 3) {
                        continue;
                    } // if (iflag >= 3)

                    str = c2r.abs();
                    sti = c2i.abs();
                    c2m = str.max(sti);

                    if (c2m.le(ascle)) {
                        continue;
                    } // if (c2m <= ascle)

                    iflag = iflag + 1;
                    ascle = (DoubleDouble)bry[iflag - 1].clone();
                    s1r = s1r.multiply(c1r);
                    s1i = s1i.multiply(c1r);
                    s2r = (DoubleDouble)c2r.clone();
                    s2i = (DoubleDouble)c2i.clone();
                    s1r = s1r.multiply(cssr[iflag - 1]);
                    s1i = s1i.multiply(cssr[iflag - 1]);
                    s2r = s2r.multiply(cssr[iflag - 1]);
                    s2i = s2i.multiply(cssr[iflag - 1]);
                    c1r = (DoubleDouble)csrr[iflag - 1].clone();
                } // for (i = 3; i <= nd; i++)

                return;
            } // group

            if (rs1.gt(DoubleDouble.valueOf(0.0))) {
                nz[0] = -1;

                return;
            } // if (rs1 > 0.0)

            // Set underflow and update parameters
            yr[nd - 1] = DoubleDouble.valueOf(0.0);
            yi[nd - 1] = DoubleDouble.valueOf(0.0);
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

            fn = fnu.add(DoubleDouble.valueOf(nd - 1.0));

            if (fn.lt(fnul)) {
                nlast[0] = nd;

                return;
            } // if (fn < fnul)

            in = inu + nd - 1;
            in = (in % 4) + 1;
            c2r = (car.multiply(cipr[in - 1])).subtract(sar.multiply(cipi[in - 1]));
            c2i = (car.multiply(cipi[in - 1])).add(sar.multiply(cipr[in - 1]));

            if (zi.le(DoubleDouble.valueOf(0.0))) {
                c2i = c2i.negate();
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
     * @param zrr DoubleDouble
     * @param zri DoubleDouble
     * @param fnu DoubleDouble
     * @param ikflg int
     * @param ipmtr int
     * @param init int
     * @param phir DoubleDouble[]
     * @param phii DoubleDouble[]
     * @param zeta1r DoubleDouble[]
     * @param zeta1i DoubleDouble[]
     * @param zeta2r DoubleDouble[]
     * @param zeta2i DoubleDouble[]
     * @param sumr DoubleDouble[]
     * @param sumi DoubleDouble[]
     * @param cwrkr DoubleDouble[]
     * @param cwrki DoubleDouble[]
     */
    private void zunik(final DoubleDouble zrr, final DoubleDouble zri, final DoubleDouble fnu, final int ikflg, final int ipmtr,
            int init[], final DoubleDouble[] phir, final DoubleDouble[] phii, final DoubleDouble[] zeta1r, final DoubleDouble[] zeta1i,
            final DoubleDouble[] zeta2r, final DoubleDouble[] zeta2i, final DoubleDouble[] sumr, final DoubleDouble[] sumi,
            final DoubleDouble[] cwrkr, final DoubleDouble[] cwrki) {
        final DoubleDouble[] con = new DoubleDouble[] {DoubleDouble.valueOf(3.98942280401432678E-01), 
        		DoubleDouble.valueOf(1.25331413731550025)};
        final DoubleDouble[] c = new DoubleDouble[] {DoubleDouble.valueOf(1.00000000000000000E+00), 
        		DoubleDouble.valueOf(-2.08333333333333333E-01), DoubleDouble.valueOf(1.25000000000000000E-01),
        		DoubleDouble.valueOf(3.34201388888888889E-01), DoubleDouble.valueOf(-4.01041666666666667E-01), 
        		DoubleDouble.valueOf(7.03125000000000000E-02), DoubleDouble.valueOf(-1.02581259645061728E+00),
        		DoubleDouble.valueOf(1.84646267361111111E+00), DoubleDouble.valueOf(-8.91210937500000000E-01),
        		DoubleDouble.valueOf(7.32421875000000000E-02), DoubleDouble.valueOf(4.66958442342624743E+00),
        		DoubleDouble.valueOf(-1.12070026162229938E+01), DoubleDouble.valueOf(8.78912353515625000E+00), 
        		DoubleDouble.valueOf(-2.36408691406250000E+00), DoubleDouble.valueOf(1.12152099609375000E-01),
        		DoubleDouble.valueOf(-2.82120725582002449E+01), DoubleDouble.valueOf(8.46362176746007346E+01), 
        		DoubleDouble.valueOf(-9.18182415432400174E+01), DoubleDouble.valueOf(4.25349987453884549E+01),
        		DoubleDouble.valueOf(-7.36879435947963170E+00), DoubleDouble.valueOf(2.27108001708984375E-01), 
        		DoubleDouble.valueOf(2.12570130039217123E+02), DoubleDouble.valueOf(-7.65252468141181642E+02),
        		DoubleDouble.valueOf(1.05999045252799988E+03), DoubleDouble.valueOf(-6.99579627376132541E+02), 
        		DoubleDouble.valueOf(2.18190511744211590E+02), DoubleDouble.valueOf(-2.64914304869515555E+01),
        		DoubleDouble.valueOf(5.72501420974731445E-01), DoubleDouble.valueOf(-1.91945766231840700E+03), 
        		DoubleDouble.valueOf(8.06172218173730938E+03), DoubleDouble.valueOf(-1.35865500064341374E+04),
        		DoubleDouble.valueOf(1.16553933368645332E+04), DoubleDouble.valueOf(-5.30564697861340311E+03), 
        		DoubleDouble.valueOf(1.20090291321635246E+03), DoubleDouble.valueOf(-1.08090919788394656E+02),
        		DoubleDouble.valueOf(1.72772750258445740E+00), DoubleDouble.valueOf(2.02042913309661486E+04), 
        		DoubleDouble.valueOf(-9.69805983886375135E+04), DoubleDouble.valueOf(1.92547001232531532E+05),
        		DoubleDouble.valueOf(-2.03400177280415534E+05), DoubleDouble.valueOf(1.22200464983017460E+05), 
        		DoubleDouble.valueOf(-4.11926549688975513E+04), DoubleDouble.valueOf(7.10951430248936372E+03),
        		DoubleDouble.valueOf(-4.93915304773088012E+02), DoubleDouble.valueOf(6.07404200127348304E+00), 
        		DoubleDouble.valueOf(-2.42919187900551333E+05), DoubleDouble.valueOf(1.31176361466297720E+06),
        		DoubleDouble.valueOf(-2.99801591853810675E+06), DoubleDouble.valueOf(3.76327129765640400E+06), 
        		DoubleDouble.valueOf(-2.81356322658653411E+06), DoubleDouble.valueOf(1.26836527332162478E+06),
        		DoubleDouble.valueOf(-3.31645172484563578E+05), DoubleDouble.valueOf(4.52187689813627263E+04), 
        		DoubleDouble.valueOf(-2.49983048181120962E+03), DoubleDouble.valueOf(2.43805296995560639E+01),
        		DoubleDouble.valueOf(3.28446985307203782E+06), DoubleDouble.valueOf(-1.97068191184322269E+07), 
        		DoubleDouble.valueOf(5.09526024926646422E+07), DoubleDouble.valueOf(-7.41051482115326577E+07),
        		DoubleDouble.valueOf(6.63445122747290267E+07), DoubleDouble.valueOf(-3.75671766607633513E+07), 
        		DoubleDouble.valueOf(1.32887671664218183E+07), DoubleDouble.valueOf(-2.78561812808645469E+06),
        		DoubleDouble.valueOf(3.08186404612662398E+05), DoubleDouble.valueOf(-1.38860897537170405E+04), 
        		DoubleDouble.valueOf(1.10017140269246738E+02), DoubleDouble.valueOf(-4.93292536645099620E+07),
        		DoubleDouble.valueOf(3.25573074185765749E+08), DoubleDouble.valueOf(-9.39462359681578403E+08), 
        		DoubleDouble.valueOf(1.55359689957058006E+09), DoubleDouble.valueOf(-1.62108055210833708E+09),
        		DoubleDouble.valueOf(1.10684281682301447E+09), DoubleDouble.valueOf(-4.95889784275030309E+08),
        		DoubleDouble.valueOf(1.42062907797533095E+08), DoubleDouble.valueOf(-2.44740627257387285E+07),
        		DoubleDouble.valueOf(2.24376817792244943E+06), DoubleDouble.valueOf(-8.40054336030240853E+04), 
        		DoubleDouble.valueOf(5.51335896122020586E+02), DoubleDouble.valueOf(8.14789096118312115E+08),
        		DoubleDouble.valueOf(-5.86648149205184723E+09), DoubleDouble.valueOf(1.86882075092958249E+10), 
        		DoubleDouble.valueOf(-3.46320433881587779E+10), DoubleDouble.valueOf(4.12801855797539740E+10),
        		DoubleDouble.valueOf(-3.30265997498007231E+10), DoubleDouble.valueOf(1.79542137311556001E+10),
        		DoubleDouble.valueOf(-6.56329379261928433E+09), DoubleDouble.valueOf(1.55927986487925751E+09),
        		DoubleDouble.valueOf(-2.25105661889415278E+08), DoubleDouble.valueOf(1.73951075539781645E+07),
        		DoubleDouble.valueOf(-5.49842327572288687E+05), DoubleDouble.valueOf(3.03809051092238427E+03),
        		DoubleDouble.valueOf(-1.46792612476956167E+10), DoubleDouble.valueOf(1.14498237732025810E+11), 
        		DoubleDouble.valueOf(-3.99096175224466498E+11), DoubleDouble.valueOf(8.19218669548577329E+11),
        		DoubleDouble.valueOf(-1.09837515608122331E+12), DoubleDouble.valueOf(1.00815810686538209E+12), 
        		DoubleDouble.valueOf(-6.45364869245376503E+11), DoubleDouble.valueOf(2.87900649906150589E+11),
        		DoubleDouble.valueOf(-8.78670721780232657E+10), DoubleDouble.valueOf(1.76347306068349694E+10), 
        		DoubleDouble.valueOf(-2.16716498322379509E+09), DoubleDouble.valueOf(1.43157876718888981E+08),
        		DoubleDouble.valueOf(-3.87183344257261262E+06), DoubleDouble.valueOf(1.82577554742931747E+04), 
        		DoubleDouble.valueOf(2.86464035717679043E+11), DoubleDouble.valueOf(-2.40629790002850396E+12),
        		DoubleDouble.valueOf(9.10934118523989896E+12), DoubleDouble.valueOf(-2.05168994109344374E+13), 
        		DoubleDouble.valueOf(3.05651255199353206E+13), DoubleDouble.valueOf(-3.16670885847851584E+13),
        		DoubleDouble.valueOf(2.33483640445818409E+13), DoubleDouble.valueOf(-1.23204913055982872E+13), 
        		DoubleDouble.valueOf(4.61272578084913197E+12), DoubleDouble.valueOf(-1.19655288019618160E+12),
        		DoubleDouble.valueOf(2.05914503232410016E+11), DoubleDouble.valueOf(-2.18229277575292237E+10), 
        		DoubleDouble.valueOf(1.24700929351271032E+09), DoubleDouble.valueOf(-2.91883881222208134E+07),
        		DoubleDouble.valueOf(1.18838426256783253E+05)};
        DoubleDouble rfn;
        DoubleDouble test;
        DoubleDouble ac;
        final DoubleDouble[] tr = new DoubleDouble[1];
        final DoubleDouble[] ti = new DoubleDouble[1];
        DoubleDouble sr;
        DoubleDouble si;
        final DoubleDouble[] srr = new DoubleDouble[1];
        final DoubleDouble[] sri = new DoubleDouble[1];
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        final DoubleDouble[] znr = new DoubleDouble[1];
        final DoubleDouble[] zni = new DoubleDouble[1];
        int l;
        int k;
        int j;
        final DoubleDouble[] t2r = new DoubleDouble[1];
        final DoubleDouble[] t2i = new DoubleDouble[1];
        final DoubleDouble[] cwrkr15 = new DoubleDouble[1];
        final DoubleDouble[] cwrki15 = new DoubleDouble[1];
        final int[] idum = new int[1];
        DoubleDouble crfnr;
        DoubleDouble crfni;
        int i;

        if (init[0] == 0) {

            // initialize all variables
            rfn = fnu.reciprocal();

            // Overflow test (zr/fnu too small)
            test = (DoubleDouble.valueOf(1.0E3)).multiply(tiny);
            ac = fnu.multiply(test);

            if ( ((zrr.abs()).le(ac)) && ((zri.abs()).le(ac))) {
                zeta1r[0] = ((DoubleDouble.valueOf(2.0)).multiply((test.log()).abs())).add(fnu);
                zeta1i[0] = DoubleDouble.valueOf(0.0);
                zeta2r[0] = (DoubleDouble)fnu.clone();
                zeta2i[0] = DoubleDouble.valueOf(0.0);
                phir[0] = DoubleDouble.valueOf(1.0);
                phii[0] = DoubleDouble.valueOf(0.0);

                return;
            } // if ((Math.abs(zrr) <= ac) && (Math.abs(zri) <= ac))

            tr[0] = zrr.multiply(rfn);
            ti[0] = zri.multiply(rfn);
            sr = (DoubleDouble.valueOf(1.0)).add( (tr[0].multiply(tr[0])).subtract(ti[0].multiply(ti[0])));
            si = ((DoubleDouble.valueOf(2.0)).multiply(tr[0])).multiply(ti[0]);
            zsqrt(sr, si, srr, sri);
            str[0] = (DoubleDouble.valueOf(1.0)).add(srr[0]);
            sti[0] = (DoubleDouble)sri[0].clone();
            zdiv(str[0], sti[0], tr[0], ti[0], znr, zni);
            zlog(znr[0], zni[0], str, sti, idum);
            zeta1r[0] = fnu.multiply(str[0]);
            zeta1i[0] = fnu.multiply(sti[0]);
            zeta2r[0] = fnu.multiply(srr[0]);
            zeta2i[0] = fnu.multiply(sri[0]);
            zdiv(DoubleDouble.valueOf(1.0), DoubleDouble.valueOf(0.0), srr[0], sri[0], tr, ti);
            srr[0] = tr[0].multiply(rfn);
            sri[0] = ti[0].multiply(rfn);
            zsqrt(srr[0], sri[0], cwrkr15, cwrki15);
            cwrkr[15] = (DoubleDouble)cwrkr15[0].clone();
            cwrki[15] = (DoubleDouble)cwrki15[0].clone();
            phir[0] = cwrkr[15].multiply(con[ikflg - 1]);
            phii[0] = cwrki[15].multiply(con[ikflg - 1]);

            if (ipmtr != 0) {
                return;
            }

            zdiv(DoubleDouble.valueOf(1.0),DoubleDouble.valueOf(0.0), sr, si, t2r, t2i);
            cwrkr[0] = DoubleDouble.valueOf(1.0);
            cwrki[0] = DoubleDouble.valueOf(0.0);
            crfnr = DoubleDouble.valueOf(1.0);
            crfni = DoubleDouble.valueOf(0.0);
            ac = DoubleDouble.valueOf(1.0);
            l = 1;

            group: {

                for (k = 2; k <= 15; k++) {
                    sr = DoubleDouble.valueOf(0.0);
                    si = DoubleDouble.valueOf(0.0);

                    for (j = 1; j <= k; j++) {
                        l = l + 1;
                        str[0] = ((sr.multiply(t2r[0])).subtract(si.multiply(t2i[0]))).add(c[l - 1]);
                        si = (sr.multiply(t2i[0])).add(si.multiply(t2r[0]));
                        sr = (DoubleDouble)str[0].clone();
                    } // for (j = 1; j <= k; j++)

                    str[0] = (crfnr.multiply(srr[0])).subtract(crfni.multiply(sri[0]));
                    crfni = (crfnr.multiply(sri[0])).add(crfni.multiply(srr[0]));
                    crfnr = (DoubleDouble)str[0].clone();
                    cwrkr[k - 1] = (crfnr.multiply(sr)).subtract(crfni.multiply(si));
                    cwrki[k - 1] = (crfnr.multiply(si)).add(crfni.multiply(sr));
                    ac = ac.multiply(rfn);
                    test = (cwrkr[k - 1].abs()).add(cwrki[k - 1].abs());

                    if ( (ac.lt(tol)) && (test.lt(tol))) {
                        break group;
                    }
                } // for (k = 2; k <= 15; k++)

                k = 15;
            } // group

            init[0] = k;
        } // if (init == 0)

        if (ikflg != 2) {

            // Compute sum for the I function
            sr = DoubleDouble.valueOf(0.0);
            si = DoubleDouble.valueOf(0.0);

            for (i = 1; i <= init[0]; i++) {
                sr = sr.add(cwrkr[i - 1]);
                si = si.add(cwrki[i - 1]);
            } // for (i = 1; i <= init; i++)

            sumr[0] = (DoubleDouble)sr.clone();
            sumi[0] = (DoubleDouble)si.clone();
            phir[0] = cwrkr[15].multiply(con[0]);
            phii[0] = cwrki[15].multiply(con[0]);

            return;
        } // if (ikflg != 2)

        // Compute sum for the k function
        sr = DoubleDouble.valueOf(0.0);
        si = DoubleDouble.valueOf(0.0);
        tr[0] = DoubleDouble.valueOf(1.0);

        for (i = 1; i <= init[0]; i++) {
            sr = sr.add(tr[0].multiply(cwrkr[i - 1]));
            si = si.add(tr[0].multiply(cwrki[i - 1]));
            tr[0] = tr[0].negate();
        } // for (i = 1; i <= init; i++)

        sumr[0] = (DoubleDouble)sr.clone();
        sumi[0] = (DoubleDouble)si.clone();
        phir[0] = cwrkr[15].multiply(con[1]);
        phii[0] = cwrki[15].multiply(con[1]);

        return;
    }

    /**
     * ZUNK1 COMPUTES K(FNU,Z) AND ITS ANALYTIC CONTINUATION FROM THE RIGHT HALF PLANE TO THE LEFT HALF PLANE BY MEANS
     * OF THE UNIFORM ASYMPTOTIC EXPANSION. MR INDICATES THE DIRECTION OF ROTATION FOR ANALYTIC CONTINUATION. NZ=-1
     * MEANS AN OVERFLOW WILL OCCUR
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param mr int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     */
    private void zunk1(DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int mr, final int n,
            final DoubleDouble[] yr, final DoubleDouble[] yi, final int[] nz) {
        final DoubleDouble[] bry = new DoubleDouble[3];
        final int[] init = new int[2];
        final DoubleDouble[] sumr = new DoubleDouble[2];
        final DoubleDouble[] sumi = new DoubleDouble[2];
        final DoubleDouble[] zeta1r = new DoubleDouble[2];
        final DoubleDouble[] zeta1i = new DoubleDouble[2];
        final DoubleDouble[] zeta2r = new DoubleDouble[2];
        final DoubleDouble[] zeta2i = new DoubleDouble[2];
        final DoubleDouble[] cyr = new DoubleDouble[2];
        final DoubleDouble[] cyi = new DoubleDouble[2];
        final DoubleDouble[][] cwrkr = new DoubleDouble[16][3];
        final DoubleDouble[][] cwrki = new DoubleDouble[16][3];
        final DoubleDouble[] cssr = new DoubleDouble[3];
        final DoubleDouble[] csrr = new DoubleDouble[3];
        final DoubleDouble[] phir = new DoubleDouble[2];
        final DoubleDouble[] phii = new DoubleDouble[2];
        DoubleDouble cscl;
        DoubleDouble crsc;
        DoubleDouble zrr;
        DoubleDouble zri;
        int j;
        int i;
        DoubleDouble fn = DoubleDouble.valueOf(0.0);
        final DoubleDouble[] phirj = new DoubleDouble[1];
        final DoubleDouble[] phiij = new DoubleDouble[1];
        final DoubleDouble[] zeta1rj = new DoubleDouble[1];
        final DoubleDouble[] zeta1ij = new DoubleDouble[1];
        final DoubleDouble[] zeta2rj = new DoubleDouble[1];
        final DoubleDouble[] zeta2ij = new DoubleDouble[1];
        final DoubleDouble[] sumrj = new DoubleDouble[1];
        final DoubleDouble[] sumij = new DoubleDouble[1];
        final DoubleDouble[] cwrkrj = new DoubleDouble[16];
        final DoubleDouble[] cwrkij = new DoubleDouble[16];
        DoubleDouble str;
        DoubleDouble sti;
        final DoubleDouble[] s1r = new DoubleDouble[1];
        final DoubleDouble[] s1i = new DoubleDouble[1];
        DoubleDouble rast;
        DoubleDouble rs1;
        int kdflg;
        int kflag = 0;
        final DoubleDouble[] s2r = new DoubleDouble[1];
        final DoubleDouble[] s2i = new DoubleDouble[1];
        final int[] nw = new int[1];
        DoubleDouble razr;
        DoubleDouble rzr;
        DoubleDouble rzi;
        DoubleDouble ckr;
        DoubleDouble cki;
        int ib;
        int ipard;
        int initd[] = new int[1];
        final DoubleDouble[] phidr = new DoubleDouble[1];
        final DoubleDouble[] phidi = new DoubleDouble[1];
        final DoubleDouble[] zet1dr = new DoubleDouble[1];
        final DoubleDouble[] zet1di = new DoubleDouble[1];
        final DoubleDouble[] zet2dr = new DoubleDouble[1];
        final DoubleDouble[] zet2di = new DoubleDouble[1];
        final DoubleDouble[] sumdr = new DoubleDouble[1];
        final DoubleDouble[] sumdi = new DoubleDouble[1];
        final DoubleDouble[] cwrkr3 = new DoubleDouble[16];
        final DoubleDouble[] cwrki3 = new DoubleDouble[16];
        DoubleDouble aphi;
        final DoubleDouble[] c1r = new DoubleDouble[1];
        final DoubleDouble[] c1i = new DoubleDouble[1];
        DoubleDouble ascle;
        final DoubleDouble[] c2r = new DoubleDouble[1];
        final DoubleDouble[] c2i = new DoubleDouble[1];
        DoubleDouble c2m;
        DoubleDouble fmr;
        DoubleDouble sgn;
        DoubleDouble csgni;
        int inu;
        DoubleDouble fnf;
        int ifn;
        DoubleDouble ang;
        DoubleDouble cspnr;
        DoubleDouble cspni;
        DoubleDouble asc;
        final int[] iuf = new int[1];
        int kk;
        int ic;
        final DoubleDouble[] cwrkrm = new DoubleDouble[16];
        final DoubleDouble[] cwrkim = new DoubleDouble[16];
        int iflag = 0;
        int il;
        DoubleDouble csr;
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
        int tempInit[] = new int[1];
        
        for (i = 0; i < 16; i++) {
        	cwrkrj[i] = DoubleDouble.valueOf(0.0);
        	cwrkij[i] = DoubleDouble.valueOf(0.0);
        	cwrkrm[i] = DoubleDouble.valueOf(0.0);
        	cwrkim[i] = DoubleDouble.valueOf(0.0);
        	cwrkr3[i] = DoubleDouble.valueOf(0.0);
        	cwrki3[i] = DoubleDouble.valueOf(0.0);
        }

        kdflg = 1;
        nz[0] = 0;

        // exp(-alim) = exp(-elim)/tol = approximately one precision greater
        // than the underflow limit
        cscl = tol.reciprocal();
        crsc = (DoubleDouble)tol.clone();
        cssr[0] = (DoubleDouble)cscl.clone();
        cssr[1] = DoubleDouble.valueOf(1.0);
        cssr[2] = (DoubleDouble)crsc.clone();
        csrr[0] = (DoubleDouble)crsc.clone();
        csrr[1] = DoubleDouble.valueOf(1.0);
        csrr[2] = (DoubleDouble)cscl.clone();
        bry[0] = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).divide(tol);
        bry[1] = bry[0].reciprocal();
        bry[2] = DoubleDouble.valueOf(Double.MAX_VALUE);
        zrr = (DoubleDouble)zr.clone();
        zri = (DoubleDouble)zi.clone();

        if (zr.lt(DoubleDouble.valueOf(0.0))) {
            zrr = zr.negate();
            zri = zi.negate();
        } // if (zr < 0.0)

        j = 2;

        group: {

            for (i = 1; i <= n; i++) {

                // j flip flops between 1 and 2 in j = 3 - j
                j = 3 - j;
                fn = fnu.add(DoubleDouble.valueOf(i - 1.0));
                init[j - 1] = 0;
                tempInit[0] = init[j-1];
                zunik(zrr, zri, fn, 2, 0, tempInit, phirj, phiij, zeta1rj, zeta1ij, zeta2rj, zeta2ij, sumrj, sumij,
                        cwrkrj, cwrkij);
                init[j-1] = tempInit[0];
                phir[j - 1] = (DoubleDouble)phirj[0].clone();
                phii[j - 1] = (DoubleDouble)phiij[0].clone();
                zeta1r[j - 1] = (DoubleDouble)zeta1rj[0].clone();
                zeta1i[j - 1] = (DoubleDouble)zeta1ij[0].clone();
                zeta2r[j - 1] = (DoubleDouble)zeta2rj[0].clone();
                zeta2i[j - 1] = (DoubleDouble)zeta2ij[0].clone();
                sumr[j - 1] = (DoubleDouble)sumrj[0].clone();
                sumi[j - 1] = (DoubleDouble)sumij[0].clone();

                for (p = 0; p < 16; p++) {
                    cwrkr[p][j - 1] = (DoubleDouble)cwrkrj[p].clone();
                    cwrki[p][j - 1] = (DoubleDouble)cwrkij[p].clone();
                }

                if (kode != 1) {
                    str = zrr.add(zeta2r[j - 1]);
                    sti = zri.add(zeta2i[j - 1]);
                    rast = fn.divide(zabs(str, sti));
                    str = (str.multiply(rast)).multiply(rast);
                    sti = ((sti.negate()).multiply(rast)).multiply(rast);
                    s1r[0] = zeta1r[j - 1].subtract(str);
                    s1i[0] = zeta1i[j - 1].subtract(sti);
                } // if (kode != 1)
                else { // kode == 1
                    s1r[0] = zeta1r[j - 1].subtract(zeta2r[j - 1]);
                    s1i[0] = zeta1i[j - 1].subtract(zeta2i[j - 1]);
                } // else kode == 1

                rs1 = (DoubleDouble)s1r[0].clone();

                // Test for underflow and overflow
                if ((rs1.abs()).le(elim)) {

                    if (kdflg == 1) {
                        kflag = 2;
                    } // if (kdflg == 1)

                    if ((rs1.abs()).ge(alim)) {

                        // Refine test and scale
                        aphi = zabs(phir[j - 1], phii[j - 1]);
                        rs1 = rs1.add(aphi.log());

                        if ((rs1.abs()).gt(elim)) {
                            seg1 = false;
                            seg2 = false;
                        } // if (Math.abs(rs1) > elim)

                        if (seg1) {

                            if (kdflg == 1) {
                                kflag = 1;
                            } // if (kdflg == 1)

                            if ( (rs1.ge(DoubleDouble.valueOf(0.0))) && (kdflg == 1)) {
                                kflag = 3;
                            } // if ((rs1 >= 0.0) && (kdflg == 1))
                        } // if (seg1)

                        seg1 = true;
                    } // if (Math.abs(rs1) >= alim)

                    if (seg2) {

                        // Scale s1 to keep intermediate arithmetic on scale near
                        // exponent extremes
                        s2r[0] = (phir[j - 1].multiply(sumr[j - 1])).subtract(phii[j - 1].multiply(sumi[j - 1]));
                        s2i[0] = (phir[j - 1].multiply(sumi[j - 1])).add(phii[j - 1].multiply(sumr[j - 1]));
                        str = (s1r[0].exp()).multiply(cssr[kflag - 1]);
                        s1r[0] = str.multiply(s1i[0].cos());
                        s1i[0] = str.multiply(s1i[0].sin());
                        str = (s2r[0].multiply(s1r[0])).subtract(s2i[0].multiply(s1i[0]));
                        s2i[0] = (s1r[0].multiply(s2i[0])).add(s2r[0].multiply(s1i[0]));
                        s2r[0] = (DoubleDouble)str.clone();

                        if (kflag == 1) {
                            zuchk(s2r[0], s2i[0], nw, bry[0]);

                            if (nw[0] != 0) {
                                seg3 = false;
                            } // if (nw[0] != 0)
                        } // if (kflag == 1)

                        if (seg3) {
                            cyr[kdflg - 1] = (DoubleDouble)s2r[0].clone();
                            cyi[kdflg - 1] = (DoubleDouble)s2i[0].clone();
                            yr[i - 1] = s2r[0].multiply(csrr[kflag - 1]);
                            yi[i - 1] = s2i[0].multiply(csrr[kflag - 1]);

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

                if (rs1.gt(DoubleDouble.valueOf(0.0))) {
                    nz[0] = -1;

                    return;
                } // if (rs1 > 0.0)

                // For zr < 0.0, the I function to be added will overflow
                if (zr.lt(DoubleDouble.valueOf(0.0))) {
                    nz[0] = -1;

                    return;
                } // if (zr < 0.0)

                kdflg = 1;
                yr[i - 1] = DoubleDouble.valueOf(0.0);
                yi[i - 1] = DoubleDouble.valueOf(0.0);
                nz[0] = nz[0] + 1;

                if (i == 1) {
                    continue;
                } // if (i == 1)

                if ( (yr[i - 2].equals(DoubleDouble.valueOf(0.0))) && (yi[i - 2].equals(DoubleDouble.valueOf(0.0)))) {
                    continue;
                } // if ((yr[i-2] == 0.0) && (yi[i-2] == 0.0))

                yr[i - 2] = DoubleDouble.valueOf(0.0);
                yi[i - 2] = DoubleDouble.valueOf(0.0);
                nz[0] = nz[0] + 1;
            } // for (i = 1; i <= n; i++)

            i = n;
        } // group

        razr = (zabs(zrr, zri)).reciprocal();
        str = zrr.multiply(razr);
        sti = (zri.negate()).multiply(razr);
        rzr = (str.add(str)).multiply(razr);
        rzi = (sti.add(sti)).multiply(razr);
        ckr = fn.multiply(rzr);
        cki = fn.multiply(rzi);
        ib = i + 1;

        if (n >= ib) {

            // test last member for underflow and overflow. Set sequence to zero
            // on underflow
            fn = fnu.add(DoubleDouble.valueOf(n - 1.0));
            ipard = 1;

            if (mr != 0) {
                ipard = 0;
            } // if (mr != 0)

            initd[0] = 0;
            zunik(zrr, zri, fn, 2, ipard, initd, phidr, phidi, zet1dr, zet1di, zet2dr, zet2di, sumdr, sumdi, cwrkr3,
                    cwrki3);

            for (p = 0; p < 16; p++) {
                cwrkr[p][2] = (DoubleDouble)cwrkr3[p].clone();
                cwrki[p][2] = (DoubleDouble)cwrki3[p].clone();
            }

            if (kode != 1) {
                str = zrr.add(zet2dr[0]);
                sti = zri.add(zet2di[0]);
                rast = fn.divide(zabs(str, sti));
                str = (str.multiply(rast)).multiply(rast);
                sti = ((sti.negate()).multiply(rast)).multiply(rast);
                s1r[0] = zet1dr[0].subtract(str);
                s1i[0] = zet1di[0].subtract(sti);
            } // if (kode != 1)
            else { // kode == 1
                s1r[0] = zet1dr[0].subtract(zet2dr[0]);
                s1i[0] = zet1di[0].subtract(zet2di[0]);
            } // else kode == 1

            rs1 = (DoubleDouble)s1r[0].clone();

            if ((rs1.abs()).le(elim)) {

                if ((rs1.abs()).lt(alim)) {
                    seg4 = false;
                    seg5 = false;
                } // if (Math.abs(rs1) < alim)

                if (seg4) {

                    // Refine estimate and test
                    aphi = zabs(phidr[0], phidi[0]);
                    rs1 = rs1.add(aphi.log());

                    if ((rs1.abs()).lt(elim)) {
                        seg5 = false;
                    } // if (Math.abs(rs1) < elim)
                } // if (seg4)

                seg4 = true;
            } // if (Math.abs(rs1) <= elim)

            if (seg5) {

                if ((rs1.abs()).gt(DoubleDouble.valueOf(0.0))) {
                    nz[0] = -1;

                    return;
                } // if (Math.abs(rs1) > 0.0)

                // For zr < 0.0, the I function to be added will overflow
                if (zr.lt(DoubleDouble.valueOf(0.0))) {
                    nz[0] = -1;

                    return;
                } // if (zr < 0.0)

                nz[0] = n;

                for (i = 1; i <= n; i++) {
                    yr[i - 1] = DoubleDouble.valueOf(0.0);
                    yi[i - 1] = DoubleDouble.valueOf(0.0);
                } // for (i = 1; i <= n; i++)

                return;
            } // if (seg5)

            seg5 = true;

            // Forward recur for remainder of the sequence
            s1r[0] = (DoubleDouble)cyr[0].clone();
            s1i[0] = (DoubleDouble)cyi[0].clone();
            s2r[0] = (DoubleDouble)cyr[1].clone();
            s2i[0] = (DoubleDouble)cyi[1].clone();
            c1r[0] = (DoubleDouble)csrr[kflag - 1].clone();
            ascle = (DoubleDouble)bry[kflag - 1].clone();

            for (i = ib; i <= n; i++) {
                c2r[0] = (DoubleDouble)s2r[0].clone();
                c2i[0] = (DoubleDouble)s2i[0].clone();
                s2r[0] = ((ckr.multiply(c2r[0])).subtract(cki.multiply(c2i[0]))).add(s1r[0]);
                s2i[0] = ((ckr.multiply(c2i[0])).add(cki.multiply(c2r[0]))).add(s1i[0]);
                s1r[0] = (DoubleDouble)c2r[0].clone();
                s1i[0] = (DoubleDouble)c2i[0].clone();
                ckr = ckr.add(rzr);
                cki = cki.add(rzi);
                c2r[0] = s2r[0].multiply(c1r[0]);
                c2i[0] = s2i[0].multiply(c1r[0]);
                yr[i - 1] = (DoubleDouble)c2r[0].clone();
                yi[i - 1] = (DoubleDouble)c2i[0].clone();

                if (kflag >= 3) {
                    continue;
                } // if (kflag >= 3)

                str = c2r[0].abs();
                sti = c2i[0].abs();
                c2m = str.max(sti);

                if (c2m.le(ascle)) {
                    continue;
                } // if (c2m <= ascle)

                kflag = kflag + 1;
                ascle = (DoubleDouble)bry[kflag - 1].clone();
                s1r[0] = s1r[0].multiply(c1r[0]);
                s1i[0] = s1i[0].multiply(c1r[0]);
                s2r[0] = (DoubleDouble)c2r[0].clone();
                s2i[0] = (DoubleDouble)c2i[0].clone();
                s1r[0] = s1r[0].multiply(cssr[kflag - 1]);
                s1i[0] = s1i[0].multiply(cssr[kflag - 1]);
                s2r[0] = s2r[0].multiply(cssr[kflag - 1]);
                s2i[0] = s2i[0].multiply(cssr[kflag - 1]);
                c1r[0] = (DoubleDouble)csrr[kflag - 1].clone();
            } // for (i = ib; i <= n; i++)
        } // if (n >= ib)

        if (mr == 0) {
            return;
        } // if (mr == 0)

        // Analytic continuation for Re(z) < 0.0
        nz[0] = 0;
        fmr = DoubleDouble.valueOf(mr);

        if (fmr.ge(DoubleDouble.valueOf(0.0))) {
            sgn = (DoubleDouble.PI).negate();
        } else {
            sgn = DoubleDouble.PI;
        }

        // cspn and csgn are coeff of K and I function resp.
        csgni = (DoubleDouble)sgn.clone();
        inu = fnu.intValue();
        fnf = fnu.subtract(DoubleDouble.valueOf(inu));
        ifn = inu + n - 1;
        ang = fnf.multiply(sgn);
        cspnr = ang.cos();
        cspni = ang.sin();

        if ( (ifn % 2) != 0) {
            cspnr = cspnr.negate();
            cspni = cspni.negate();
        } // if ((ifn%2) != 0)

        asc = (DoubleDouble)bry[0].clone();
        iuf[0] = 0;
        kk = n;
        kdflg = 1;
        ib = ib - 1;
        ic = ib - 1;

        group2: {

            for (k = 1; k <= n; k++) {
                fn = fnu.add(DoubleDouble.valueOf(kk - 1.0));

                // Logic to sort out cases whose parameters were set for the K
                // function above
                m = 3;

                if (n > 2) {
                    seg6 = false;
                } // if (n > 2)

                while (true) {

                    if (seg6) {
                        initd[0] = init[j - 1];
                        phidr[0] = (DoubleDouble)phir[j - 1].clone();
                        phidi[0] = (DoubleDouble)phii[j - 1].clone();
                        zet1dr[0] = (DoubleDouble)zeta1r[j - 1].clone();
                        zet1di[0] = (DoubleDouble)zeta1i[j - 1].clone();
                        zet2dr[0] = (DoubleDouble)zeta2r[j - 1].clone();
                        zet2di[0] = (DoubleDouble)zeta2i[j - 1].clone();
                        sumdr[0] = (DoubleDouble)sumr[j - 1].clone();
                        sumdi[0] = (DoubleDouble)sumi[j - 1].clone();
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

                    initd[0] = 0;

                    break;
                } // while (true)

                zunik(zrr, zri, fn, 1, 0, initd, phidr, phidi, zet1dr, zet1di, zet2dr, zet2di, sumdr, sumdi, cwrkrm,
                        cwrkim);

                for (p = 0; p < 16; p++) {
                    cwrkr[p][m - 1] = (DoubleDouble)cwrkrm[p].clone();
                    cwrki[p][m - 1] = (DoubleDouble)cwrkim[p].clone();
                }

                if (kode != 1) {
                    str = zrr.add(zet2dr[0]);
                    sti = zri.add(zet2di[0]);
                    rast = fn.divide(zabs(str, sti));
                    str = (str.multiply(rast)).multiply(rast);
                    sti = ((sti.negate()).multiply(rast)).multiply(rast);
                    s1r[0] = (zet1dr[0].negate()).add(str);
                    s1i[0] = (zet1di[0].negate()).add(sti) ;
                } // if (kode != 1)
                else { // kode == 1
                    s1r[0] = (zet1dr[0].negate()).add(zet2dr[0]);
                    s1i[0] = (zet1di[0].negate()).add(zet2di[0]);
                } // else kode == 1

                // Test for underflow and overflow
                rs1 = (DoubleDouble)s1r[0].clone();

                if ((rs1.abs()).gt(elim)) {
                    seg7 = false;
                    seg9 = false;
                    seg10 = false;
                } // if (Math.abs(rs1) > elim)

                if (seg7) {

                    if (kdflg == 1) {
                        iflag = 2;
                    } // if (kdflg == 1)

                    if ((rs1.abs()).ge(alim)) {

                        // Refine test and scale
                        aphi = zabs(phidr[0], phidi[0]);
                        rs1 = rs1.add(aphi.log());

                        if ((rs1.abs()).gt(elim)) {
                            seg8 = false;
                            seg9 = false;
                            seg10 = false;
                        } // if (Math.abs(rs1) > elim)

                        if (seg8) {

                            if (kdflg == 1) {
                                iflag = 1;
                            } // if (kdflg == 1)

                            if ( (rs1.ge(DoubleDouble.valueOf(0.0))) && (kdflg == 1)) {
                                iflag = 3;
                            } // if ((rs1 >= 0.0) && (kdflg == 1))
                        } // if (seg8)

                        seg8 = true;
                    } // if (Math.abs(rs1) >= alim)
                } // if (seg7)

                seg7 = true;

                if (seg9) {
                    str = (phidr[0].multiply(sumdr[0])).subtract(phidi[0].multiply(sumdi[0]));
                    sti = (phidr[0].multiply(sumdi[0])).add(phidi[0].multiply(sumdr[0]));
                    s2r[0] = (csgni.negate()).multiply(sti);
                    s2i[0] = csgni.multiply(str);
                    str = (s1r[0].exp()).multiply(cssr[iflag - 1]);
                    s1r[0] = str.multiply(s1i[0].cos());
                    s1i[0] = str.multiply(s1i[0].sin());
                    str = (s2r[0].multiply(s1r[0])).subtract(s2i[0].multiply(s1i[0]));
                    s2i[0] = (s2r[0].multiply(s1i[0])).add(s2i[0].multiply(s1r[0]));
                    s2r[0] = (DoubleDouble)str.clone();

                    if (iflag == 1) {
                        zuchk(s2r[0], s2i[0], nw, bry[0]);

                        if (nw[0] != 0) {
                            s2r[0] = DoubleDouble.valueOf(0.0);
                            s2i[0] = DoubleDouble.valueOf(0.0);
                        } // if (nw[0] != 0)
                    } // if (iflag == 1)
                } // if (seg9)

                seg9 = true;

                while (true) {

                    if (seg10) {
                        cyr[kdflg - 1] = (DoubleDouble)s2r[0].clone();
                        cyi[kdflg - 1] = (DoubleDouble)s2i[0].clone();
                        c2r[0] = (DoubleDouble)s2r[0].clone();
                        c2i[0] = (DoubleDouble)s2i[0].clone();
                        s2r[0] = s2r[0].multiply(csrr[iflag - 1]);
                        s2i[0] = s2i[0].multiply(csrr[iflag - 1]);

                        // Add I and K functions, K sequence in Y(i), i = 1,n
                        s1r[0] = (DoubleDouble)yr[kk - 1].clone();
                        s1i[0] = (DoubleDouble)yi[kk - 1].clone();

                        if (kode != 1) {
                            zs1s2(zrr, zri, s1r, s1i, s2r, s2i, nw, asc, iuf);
                            nz[0] = nz[0] + nw[0];
                        } // if (kode != 1)

                        yr[kk - 1] = ((s1r[0].multiply(cspnr)).subtract(s1i[0].multiply(cspni))).add(s2r[0]);
                        yi[kk - 1] = ((cspnr.multiply(s1i[0])).add(cspni.multiply(s1r[0]))).add(s2i[0]);
                        kk = kk - 1;
                        cspnr = cspnr.negate();
                        cspni = cspni.negate();

                        if ( (c2r[0].equals(DoubleDouble.valueOf(0.0))) && (c2i[0].equals(DoubleDouble.valueOf(0.0)))) {
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

                    if (rs1.gt(DoubleDouble.valueOf(0.0))) {
                        nz[0] = -1;

                        return;
                    } // if (rs1 > 0.0)

                    s2r[0] = DoubleDouble.valueOf(0.0);
                    s2i[0] = DoubleDouble.valueOf(0.0);
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
        s1r[0] = (DoubleDouble)cyr[0].clone();
        s1i[0] = (DoubleDouble)cyi[0].clone();
        s2r[0] = (DoubleDouble)cyr[1].clone();
        s2i[0] = (DoubleDouble)cyi[1].clone();
        csr = (DoubleDouble)csrr[iflag - 1].clone();
        ascle = (DoubleDouble)bry[iflag - 1].clone();
        fn = DoubleDouble.valueOf(inu + il);

        for (i = 1; i <= il; i++) {
            c2r[0] = (DoubleDouble)s2r[0].clone();
            c2i[0] = (DoubleDouble)s2i[0].clone();
            s2r[0] = s1r[0].add( (fn.add(fnf)).multiply( (rzr.multiply(c2r[0])).subtract(rzi.multiply(c2i[0]))));
            s2i[0] = s1i[0].add( (fn.add(fnf)).multiply( (rzr.multiply(c2i[0])).add(rzi.multiply(c2r[0]))));
            s1r[0] = (DoubleDouble)c2r[0].clone();
            s1i[0] = (DoubleDouble)c2i[0].clone();
            fn = fn.subtract(DoubleDouble.valueOf(1.0));
            c2r[0] = s2r[0].multiply(csr);
            c2i[0] = s2i[0].multiply(csr);
            ckr = (DoubleDouble)c2r[0].clone();
            cki = (DoubleDouble)c2i[0].clone();
            c1r[0] = (DoubleDouble)yr[kk - 1].clone();
            c1i[0] = (DoubleDouble)yi[kk - 1].clone();

            if (kode != 1) {
                zs1s2(zrr, zri, c1r, c1i, c2r, c2i, nw, asc, iuf);
                nz[0] = nz[0] + nw[0];
            } // if (kode != 1)

            yr[kk - 1] = ((c1r[0].multiply(cspnr)).subtract(c1i[0].multiply(cspni))).add(c2r[0]);
            yi[kk - 1] = ((c1r[0].multiply(cspni)).add(c1i[0].multiply(cspnr))).add(c2i[0]);
            kk = kk - 1;
            cspnr = cspnr.negate();
            cspni = cspni.negate();

            if (iflag >= 3) {
                continue;
            } // if (iflag >= 3)

            c2r[0] = ckr.abs();
            c2i[0] = cki.abs();
            c2m = c2r[0].max(c2i[0]);

            if (c2m.le(ascle)) {
                continue;
            } // if (c2m <= ascle)

            iflag = iflag + 1;
            ascle = (DoubleDouble)bry[iflag - 1].clone();
            s1r[0] = s1r[0].multiply(csr);
            s1i[0] = s1i[0].multiply(csr);
            s2r[0] = (DoubleDouble)ckr.clone();
            s2i[0] = (DoubleDouble)cki.clone();
            s1r[0] = s1r[0].multiply(cssr[iflag - 1]);
            s1i[0] = s1i[0].multiply(cssr[iflag - 1]);
            s2r[0] = s2r[0].multiply(cssr[iflag - 1]);
            s2i[0] = s2i[0].multiply(cssr[iflag - 1]);
            csr = (DoubleDouble)csrr[iflag - 1].clone();
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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param mr int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     */
    private void zunk2(DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int mr, final int n,
            final DoubleDouble[] yr, final DoubleDouble[] yi, final int[] nz) {
        final DoubleDouble cr1r = DoubleDouble.valueOf(1.0);
        final DoubleDouble cr1i = DoubleDouble.valueOf(1.73205080756887729);
        final DoubleDouble cr2r = DoubleDouble.valueOf(-0.5);
        final DoubleDouble cr2i = DoubleDouble.valueOf(-8.66025403784438647E-01);
        DoubleDouble hpi = DoubleDouble.PI_2;
        final DoubleDouble aic = DoubleDouble.valueOf(1.26551212348464539);
        final DoubleDouble[] cipr = new DoubleDouble[] {DoubleDouble.valueOf(1.0), DoubleDouble.valueOf(0.0), 
        		DoubleDouble.valueOf(-1.0), DoubleDouble.valueOf(0.0)};
        final DoubleDouble[] cipi = new DoubleDouble[] {DoubleDouble.valueOf(0.0), DoubleDouble.valueOf(-1.0), 
        		DoubleDouble.valueOf(0.0), DoubleDouble.valueOf(1.0)};
        final DoubleDouble[] bry = new DoubleDouble[3];
        final DoubleDouble[] asumr = new DoubleDouble[2];
        final DoubleDouble[] asumi = new DoubleDouble[2];
        final DoubleDouble[] bsumr = new DoubleDouble[2];
        final DoubleDouble[] bsumi = new DoubleDouble[2];
        final DoubleDouble[] phir = new DoubleDouble[2];
        final DoubleDouble[] phii = new DoubleDouble[2];
        final DoubleDouble[] argr = new DoubleDouble[2];
        final DoubleDouble[] argi = new DoubleDouble[2];
        final DoubleDouble[] zeta1r = new DoubleDouble[2];
        final DoubleDouble[] zeta1i = new DoubleDouble[2];
        final DoubleDouble[] zeta2r = new DoubleDouble[2];
        final DoubleDouble[] zeta2i = new DoubleDouble[2];
        final DoubleDouble[] cyr = new DoubleDouble[2];
        final DoubleDouble[] cyi = new DoubleDouble[2];
        final DoubleDouble[] cssr = new DoubleDouble[3];
        final DoubleDouble[] csrr = new DoubleDouble[3];
        int kdflg;
        DoubleDouble cscl;
        DoubleDouble crsc;
        DoubleDouble zrr;
        DoubleDouble zri;
        DoubleDouble yy;
        DoubleDouble znr;
        DoubleDouble zni;
        DoubleDouble zbr;
        DoubleDouble zbi;
        int inu;
        DoubleDouble fnf;
        DoubleDouble ang;
        DoubleDouble car;
        DoubleDouble sar;
        final DoubleDouble[] c2r = new DoubleDouble[1];
        final DoubleDouble[] c2i = new DoubleDouble[1];
        int kk;
        DoubleDouble str;
        DoubleDouble sti;
        DoubleDouble csr;
        DoubleDouble csi;
        int j;
        int i;
        DoubleDouble fn = DoubleDouble.valueOf(0.0);
        final DoubleDouble[] phirj = new DoubleDouble[1];
        final DoubleDouble[] phiij = new DoubleDouble[1];
        final DoubleDouble[] argrj = new DoubleDouble[1];
        final DoubleDouble[] argij = new DoubleDouble[1];
        final DoubleDouble[] zeta1rj = new DoubleDouble[1];
        final DoubleDouble[] zeta1ij = new DoubleDouble[1];
        final DoubleDouble[] zeta2rj = new DoubleDouble[1];
        final DoubleDouble[] zeta2ij = new DoubleDouble[1];
        final DoubleDouble[] asumrj = new DoubleDouble[1];
        final DoubleDouble[] asumij = new DoubleDouble[1];
        final DoubleDouble[] bsumrj = new DoubleDouble[1];
        final DoubleDouble[] bsumij = new DoubleDouble[1];
        DoubleDouble rast;
        final DoubleDouble[] s1r = new DoubleDouble[1];
        final DoubleDouble[] s1i = new DoubleDouble[1];
        DoubleDouble rs1;
        int kflag = 0;
        DoubleDouble aphi;
        DoubleDouble aarg;
        final DoubleDouble[] air = new DoubleDouble[1];
        final DoubleDouble[] aii = new DoubleDouble[1];
        final int[] nai = new int[1];
        final int[] idum = new int[1];
        final DoubleDouble[] dair = new DoubleDouble[1];
        final DoubleDouble[] daii = new DoubleDouble[1];
        final int[] ndai = new int[1];
        DoubleDouble ptr;
        DoubleDouble pti;
        final DoubleDouble[] s2r = new DoubleDouble[1];
        final DoubleDouble[] s2i = new DoubleDouble[1];
        final int[] nw = new int[1];
        DoubleDouble razr;
        DoubleDouble rzr;
        DoubleDouble rzi;
        DoubleDouble ckr;
        DoubleDouble cki;
        int ib;
        int ipard;
        final DoubleDouble[] phidr = new DoubleDouble[1];
        final DoubleDouble[] phidi = new DoubleDouble[1];
        final DoubleDouble[] argdr = new DoubleDouble[1];
        final DoubleDouble[] argdi = new DoubleDouble[1];
        final DoubleDouble[] zet1dr = new DoubleDouble[1];
        final DoubleDouble[] zet1di = new DoubleDouble[1];
        final DoubleDouble[] zet2dr = new DoubleDouble[1];
        final DoubleDouble[] zet2di = new DoubleDouble[1];
        final DoubleDouble[] asumdr = new DoubleDouble[1];
        final DoubleDouble[] asumdi = new DoubleDouble[1];
        final DoubleDouble[] bsumdr = new DoubleDouble[1];
        final DoubleDouble[] bsumdi = new DoubleDouble[1];
        DoubleDouble c2m;
        DoubleDouble fmr;
        DoubleDouble sgn;
        DoubleDouble csgni;
        int ifn;
        DoubleDouble cspnr;
        DoubleDouble cspni;
        int in;
        DoubleDouble asc;
        int ic;
        int iflag = 0;
        int il;
        final DoubleDouble[] c1r = new DoubleDouble[1];
        final DoubleDouble[] c1i = new DoubleDouble[1];
        DoubleDouble ascle;
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
        cscl = tol.reciprocal();
        crsc = (DoubleDouble)tol.clone();
        cssr[0] = (DoubleDouble)cscl.clone();
        cssr[1] = DoubleDouble.valueOf(1.0);
        cssr[2] = (DoubleDouble)crsc.clone();
        csrr[0] = (DoubleDouble)crsc.clone();
        csrr[1] = DoubleDouble.valueOf(1.0);
        csrr[2] = (DoubleDouble)cscl.clone();
        bry[0] = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).divide(tol);
        bry[1] = bry[0].reciprocal();
        bry[2] = DoubleDouble.valueOf(Double.MAX_VALUE);
        zrr = (DoubleDouble)zr.clone();
        zri = (DoubleDouble)zi.clone();

        if (zr.lt(DoubleDouble.valueOf(0.0))) {
            zrr = zr.negate();
            zri = zi.negate();
        } // if (zr < 0.0)

        yy = (DoubleDouble)zri.clone();
        znr = (DoubleDouble)zri.clone();
        zni = zrr.negate();
        zbr = (DoubleDouble)zrr.clone();
        zbi = (DoubleDouble)zri.clone();
        inu = fnu.intValue();
        fnf = fnu.subtract(DoubleDouble.valueOf(inu));
        ang = (hpi.negate()).multiply(fnf);
        car = ang.cos();
        sar = ang.sin();
        c2r[0] = hpi.multiply(sar);
        c2i[0] = (hpi.negate()).multiply(car);
        kk = (inu % 4) + 1;
        str = (c2r[0].multiply(cipr[kk - 1])).subtract(c2i[0].multiply(cipi[kk - 1]));
        sti = (c2r[0].multiply(cipi[kk - 1])).add(c2i[0].multiply(cipr[kk - 1]));
        csr = (cr1r.multiply(str)).subtract(cr1i.multiply(sti));
        csi = (cr1r.multiply(sti)).add(cr1i.multiply(str));

        if (yy.le(DoubleDouble.valueOf(0.0))) {
            znr = znr.negate();
            zbi = zbi.negate();
        } // if (yy <= 0.0)

        // K(fnu,z) is computed form H(2,fnu,-i*z) where z is in the first
        // quadrant. Fourth quadrant values (yy <= 0.0) are computed by
        // conjugation since the K function is real on the positive real axis.
        j = 2;

        group: {

            for (i = 1; i <= n; i++) {

                // j flip flops between 1 and 2 in j = 3 - j
                j = 3 - j;
                fn = fnu.add(DoubleDouble.valueOf(i - 1.0));
                zunhj(znr, zni, fn, 0, phirj, phiij, argrj, argij, zeta1rj, zeta1ij, zeta2rj, zeta2ij, asumrj, asumij,
                        bsumrj, bsumij);
                phir[j - 1] = (DoubleDouble)phirj[0].clone();
                phii[j - 1] = (DoubleDouble)phiij[0].clone();
                argr[j - 1] = (DoubleDouble)argrj[0].clone();
                argi[j - 1] = (DoubleDouble)argij[0].clone();
                zeta1r[j - 1] = (DoubleDouble)zeta1rj[0].clone();
                zeta1i[j - 1] = (DoubleDouble)zeta1ij[0].clone();
                zeta2r[j - 1] = (DoubleDouble)zeta2rj[0].clone();
                zeta2i[j - 1] = (DoubleDouble)zeta2ij[0].clone();
                asumr[j - 1] = (DoubleDouble)asumrj[0].clone();
                asumi[j - 1] = (DoubleDouble)asumij[0].clone();
                bsumr[j - 1] = (DoubleDouble)bsumrj[0].clone();
                bsumi[j - 1] = (DoubleDouble)bsumij[0].clone();

                if (kode != 1) {
                    str = zbr.add(zeta2r[j - 1]);
                    sti = zbi.add(zeta2i[j - 1]);
                    rast = fn.divide(zabs(str, sti));
                    str = (str.multiply(rast)).multiply(rast);
                    sti = ((sti.negate()).multiply(rast)).multiply(rast);
                    s1r[0] = zeta1r[j - 1].subtract(str);
                    s1i[0] = zeta1i[j - 1].subtract(sti);
                } // if (kode != 1)
                else { // kode == 1
                    s1r[0] = zeta1r[j - 1].subtract(zeta2r[j - 1]);
                    s1i[0] = zeta1i[j - 1].subtract(zeta2i[j - 1]);
                } // else kode == 1

                // Test for underflow and overflow
                rs1 = (DoubleDouble)s1r[0].clone();

                if ((rs1.abs()).le(elim)) {

                    if (kdflg == 1) {
                        kflag = 2;
                    } // if (kdflg == 1)

                    if ((rs1.abs()).ge(alim)) {

                        // Refine test and scale
                        aphi = zabs(phir[j - 1], phii[j - 1]);
                        aarg = zabs(argr[j - 1], argi[j - 1]);
                        rs1 = ((rs1.add(aphi.log())).subtract((DoubleDouble.valueOf(0.25)).multiply(aarg.log()))).subtract(aic);

                        if ((rs1.abs()).gt(elim)) {
                            seg1 = false;
                            seg2 = false;
                        } // if (Math.abs(rs1) > elim)

                        if (seg1) {

                            if (kdflg == 1) {
                                kflag = 1;
                            } // if (kdflg == 1)

                            if ( (rs1.ge(DoubleDouble.valueOf(0.0))) && (kdflg == 1)) {
                                kflag = 3;
                            } // if ((rs1 >= 0.0) && (kdflg == 1))
                        } // if (seg1)

                        seg1 = true;
                    } // if (Math.abs(rs1) >= alim)

                    if (seg2) {

                        // Scale s1 to keep intermediate arithmetic on scale near
                        // exponent extremes
                        c2r[0] = (argr[j - 1].multiply(cr2r)).subtract(argi[j - 1].multiply(cr2i));
                        c2i[0] = (argr[j - 1].multiply(cr2i)).add(argi[j - 1].multiply(cr2r));
                        zairy(c2r[0], c2i[0], 0, 2, air, aii, nai, idum);
                        zairy(c2r[0], c2i[0], 1, 2, dair, daii, ndai, idum);
                        str = (dair[0].multiply(bsumr[j - 1])).subtract(daii[0].multiply(bsumi[j - 1]));
                        sti = (dair[0].multiply(bsumi[j - 1])).add(daii[0].multiply(bsumr[j - 1]));
                        ptr = (str.multiply(cr2r)).subtract(sti.multiply(cr2i));
                        pti = (str.multiply(cr2i)).add(sti.multiply(cr2r));
                        str = ptr.add( (air[0].multiply(asumr[j - 1])).subtract(aii[0].multiply(asumi[j - 1])));
                        sti = pti.add( (air[0].multiply(asumi[j - 1])).add(aii[0].multiply(asumr[j - 1])));
                        ptr = (str.multiply(phir[j - 1])).subtract(sti.multiply(phii[j - 1]));
                        pti = (str.multiply(phii[j - 1])).add(sti.multiply(phir[j - 1]));
                        s2r[0] = (ptr.multiply(csr)).subtract(pti.multiply(csi));
                        s2i[0] = (ptr.multiply(csi)).add(pti.multiply(csr));
                        str = (s1r[0].exp()).multiply(cssr[kflag - 1]);
                        s1r[0] = str.multiply(s1i[0].cos());
                        s1i[0] = str.multiply(s1i[0].sin());
                        str = (s2r[0].multiply(s1r[0])).subtract(s2i[0].multiply(s1i[0]));
                        s2i[0] = (s1r[0].multiply(s2i[0])).add(s2r[0].multiply(s1i[0]));
                        s2r[0] = (DoubleDouble)str.clone();

                        if (kflag == 1) {
                            zuchk(s2r[0], s2i[0], nw, bry[0]);

                            if (nw[0] != 0) {
                                seg3 = false;
                            } // if (nw[0] != 0)
                        } // if (kflag == 1)

                        if (seg3) {

                            if (yy.le(DoubleDouble.valueOf(0.0))) {
                                s2i[0] = s2i[0].negate();
                            } // if (yy <= 0.0)

                            cyr[kdflg - 1] = (DoubleDouble)s2r[0].clone();
                            cyi[kdflg - 1] = (DoubleDouble)s2i[0].clone();
                            yr[i - 1] = s2r[0].multiply(csrr[kflag - 1]);
                            yi[i - 1] = s2i[0].multiply(csrr[kflag - 1]);
                            str = (DoubleDouble)csi.clone();
                            csi = csr.negate();
                            csr = (DoubleDouble)str.clone();

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

                if (rs1.gt(DoubleDouble.valueOf(0.0))) {
                    nz[0] = -1;

                    return;
                } // if (rs1 > 0.0)

                // For zr < 0.0, the I function to be added will overflow
                if (zr.lt(DoubleDouble.valueOf(0.0))) {
                    nz[0] = -1;

                    return;
                } // if (zr < 0.0)

                kdflg = 1;
                yr[i - 1] = DoubleDouble.valueOf(0.0);
                yi[i - 1] = DoubleDouble.valueOf(0.0);
                nz[0] = nz[0] + 1;
                str = (DoubleDouble)csi.clone();
                csi = csr.negate();
                csr = (DoubleDouble)str.clone();

                if (i == 1) {
                    continue;
                } // if (i == 1)

                if ( (yr[i - 2].equals(DoubleDouble.valueOf(0.0))) && (yi[i - 2].equals(DoubleDouble.valueOf(0.0)))) {
                    continue;
                } // if ((yr[i-2] == 0.0) && (yi[i-2] == 0.0))

                yr[i - 2] = DoubleDouble.valueOf(0.0);
                yi[i - 2] = DoubleDouble.valueOf(0.0);
                nz[0] = nz[0] + 1;
            } // for (i = 1; i <= n; i++)

            i = n;
        } // group

        razr = (zabs(zrr, zri)).reciprocal();
        str = zrr.multiply(razr);
        sti = (zri.negate()).multiply(razr);
        rzr = (str.add(str)).multiply(razr);
        rzi = (sti.add(sti)).multiply(razr);
        ckr = fn.multiply(rzr);
        cki = fn.multiply(rzi);
        ib = i + 1;

        if (n >= ib) {

            // Test last member for underflow and overflow. Set sequence to zero
            // on underflow
            fn = fnu.add(DoubleDouble.valueOf(n - 1.0));
            ipard = 1;

            if (mr != 0) {
                ipard = 0;
            } // if (mr != 0)

            zunhj(znr, zni, fn, ipard, phidr, phidi, argdr, argdi, zet1dr, zet1di, zet2dr, zet2di, asumdr, asumdi,
                    bsumdr, bsumdi);

            if (kode != 1) {
                str = zbr.add(zet2dr[0]);
                sti = zbi.add(zet2di[0]);
                rast = fn.divide(zabs(str, sti));
                str = (str.multiply(rast)).multiply(rast);
                sti = ((sti.negate()).multiply(rast)).multiply(rast);
                s1r[0] = zet1dr[0].subtract(str);
                s1i[0] = zet1di[0].subtract(sti);
            } // if (kode != 1)
            else { // kode == 1
                s1r[0] = zet1dr[0].subtract(zet2dr[0]);
                s1i[0] = zet1di[0].subtract(zet2di[0]);
            } // else kode == 1

            rs1 = (DoubleDouble)s1r[0].clone();

            if ((rs1.abs()).le(elim)) {

                if ((rs1.abs()).lt(alim)) {
                    seg4 = false;
                    seg5 = false;
                } // if (Math.abs(rs1) < alim)

                if (seg4) {

                    // Refine estimate and test
                    aphi = zabs(phidr[0], phidi[0]);
                    rs1 = rs1.add(aphi.log());

                    if ((rs1.abs()).lt(elim)) {
                        seg5 = false;
                    } // if (Math.abs(rs1) < elim)
                } // if (seg4)

                seg4 = true;
            } // if (Math.abs(rs1) <= elim)

            if (seg5) {

                if (rs1.gt(DoubleDouble.valueOf(0.0))) {
                    nz[0] = -1;

                    return;
                } // if (rs1 > 0.0)

                // For zr < 0.0, the I function to be added will overflow
                if (zr.lt(DoubleDouble.valueOf(0.0))) {
                    nz[0] = -1;

                    return;
                } // if (zr < 0.0)

                nz[0] = n;

                for (i = 1; i <= n; i++) {
                    yr[i - 1] = DoubleDouble.valueOf(0.0);
                    yi[i - 1] = DoubleDouble.valueOf(0.0);
                } // for (i = 1; i <= n; i++)

                return;
            } // if (seg5)

            seg5 = true;
            s1r[0] = (DoubleDouble)cyr[0].clone();
            s1i[0] = (DoubleDouble)cyi[0].clone();
            s2r[0] = (DoubleDouble)cyr[1].clone();
            s2i[0] = (DoubleDouble)cyi[1].clone();
            c1r[0] = (DoubleDouble)csrr[kflag - 1].clone();
            ascle = (DoubleDouble)bry[kflag - 1].clone();

            for (i = ib; i <= n; i++) {
                c2r[0] = (DoubleDouble)s2r[0].clone();
                c2i[0] = (DoubleDouble)s2i[0].clone();
                s2r[0] = ((ckr.multiply(c2r[0])).subtract(cki.multiply(c2i[0]))).add(s1r[0]);
                s2i[0] = ((ckr.multiply(c2i[0])).add(cki.multiply(c2r[0]))).add(s1i[0]);
                s1r[0] = (DoubleDouble)c2r[0].clone();
                s1i[0] = (DoubleDouble)c2i[0].clone();
                ckr = ckr.add(rzr);
                cki = cki.add(rzi);
                c2r[0] = s2r[0].multiply(c1r[0]);
                c2i[0] = s2i[0].multiply(c1r[0]);
                yr[i - 1] = (DoubleDouble)c2r[0].clone();
                yi[i - 1] = (DoubleDouble)c2i[0].clone();

                if (kflag >= 3) {
                    continue;
                } // if (kflag >= 3)

                str = c2r[0].abs();
                sti = c2i[0].abs();
                c2m = str.max(sti);

                if (c2m.le(ascle)) {
                    continue;
                } // if (c2m <= ascle)

                kflag = kflag + 1;
                ascle = (DoubleDouble)bry[kflag - 1].clone();
                s1r[0] = s1r[0].multiply(c1r[0]);
                s1i[0] = s1i[0].multiply(c1r[0]);
                s2r[0] = (DoubleDouble)c2r[0].clone();
                s2i[0] = (DoubleDouble)c2i[0].clone();
                s1r[0] = s1r[0].multiply(cssr[kflag - 1]);
                s1i[0] = s1i[0].multiply(cssr[kflag - 1]);
                s2r[0] = s2r[0].multiply(cssr[kflag - 1]);
                s2i[0] = s2i[0].multiply(cssr[kflag - 1]);
                c1r[0] = (DoubleDouble)csrr[kflag - 1].clone();
            } // for (i = ib; i <= n; i++)
        } // if (n >= ib)

        if (mr == 0) {
            return;
        } // if (mr == 0)

        // Analytic continuation for Re(z) < 0.0
        nz[0] = 0;
        fmr = DoubleDouble.valueOf(mr);

        if (fmr.ge(DoubleDouble.valueOf(0.0))) {
            sgn = (DoubleDouble.PI).negate();
        } else {
            sgn = DoubleDouble.PI;
        }

        // cspn and csgn are coeff of K and I functions respectively
        csgni = (DoubleDouble)sgn.clone();

        if (yy.le(DoubleDouble.valueOf(0.0))) {
            csgni = csgni.negate();
        } // if (yy <= 0.0)

        ifn = inu + n - 1;
        ang = fnf.multiply(sgn);
        cspnr = ang.cos();
        cspni = ang.sin();

        if ( (ifn % 2) != 0) {
            cspnr = cspnr.negate();
            cspni = cspni.negate();
        } // if ((ifn%2) != 0)

        // cs = coeff of the J function to get the I function. I(fnu,z) is
        // computed from exp(i*fnu*hpi)*J(fnu,-i*z) where z is in the first
        // quadrant. Fourth quadrant values (yy <= 0.0) are computed by
        // conjugation since the I function is real on the positive real axis
        csr = sar.multiply(csgni);
        csi = car.multiply(csgni);
        in = (ifn % 4) + 1;
        c2r[0] = (DoubleDouble)cipr[in - 1].clone();
        c2i[0] = (DoubleDouble)cipi[in - 1].clone();
        str = (csr.multiply(c2r[0])).add(csi.multiply(c2i[0]));
        csi = ( (csr.negate()).multiply(c2i[0])).add(csi.multiply(c2r[0]));
        csr = (DoubleDouble)str.clone();
        asc = (DoubleDouble)bry[0].clone();
        iuf[0] = 0;
        kk = n;
        kdflg = 1;
        ib = ib - 1;
        ic = ib - 1;

        group2: {

            for (k = 1; k <= n; k++) {
                fn = fnu.add(DoubleDouble.valueOf(kk - 1.0));

                // Logic to sort out cases whose parameters were set for the K
                // function above
                if (n > 2) {
                    seg6 = false;
                } // if (n > 2)

                while (true) {

                    if (seg6) {
                        phidr[0] = (DoubleDouble)phir[j - 1].clone();
                        phidi[0] = (DoubleDouble)phii[j - 1].clone();
                        argdr[0] = (DoubleDouble)argr[j - 1].clone();
                        argdi[0] = (DoubleDouble)argi[j - 1].clone();
                        zet1dr[0] = (DoubleDouble)zeta1r[j - 1].clone();
                        zet1di[0] = (DoubleDouble)zeta1i[j - 1].clone();
                        zet2dr[0] = (DoubleDouble)zeta2r[j - 1].clone();
                        zet2di[0] = (DoubleDouble)zeta2i[j - 1].clone();
                        asumdr[0] = (DoubleDouble)asumr[j - 1].clone();
                        asumdi[0] = (DoubleDouble)asumi[j - 1].clone();
                        bsumdr[0] = (DoubleDouble)bsumr[j - 1].clone();
                        bsumdi[0] = (DoubleDouble)bsumi[j - 1].clone();
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
                    str = zbr.add(zet2dr[0]);
                    sti = zbi.add(zet2di[0]);
                    rast = fn.divide(zabs(str, sti));
                    str = (str.multiply(rast)).multiply(rast);
                    sti = ((sti.negate()).multiply(rast)).multiply(rast);
                    s1r[0] = (zet1dr[0].negate()).add(str);
                    s1i[0] = (zet1di[0].negate()).add(sti);
                } // if (kode != 1)
                else { // kode == 1
                    s1r[0] = (zet1dr[0].negate()).add(zet2dr[0]);
                    s1i[0] = (zet1di[0].negate()).add(zet2di[0]);
                } // else kode == 1

                // Test for underflow and overflow
                rs1 = (DoubleDouble)s1r[0].clone();

                if ((rs1.abs()).gt(elim)) {
                    seg7 = false;
                    seg8 = false;
                } // if (Math.abs(rs1) > elim)

                if (seg7) {

                    if (kdflg == 1) {
                        iflag = 2;
                    } // if (kdflg == 1)

                    if ((rs1.abs()).ge(alim)) {

                        // Refine test and scale
                        aphi = zabs(phidr[0], phidi[0]);
                        aarg = zabs(argdr[0], argdi[0]);
                        rs1 = ((rs1.add(aphi.log())).subtract((DoubleDouble.valueOf(0.25)).multiply(aarg.log()))).subtract(aic);

                        if ((rs1.abs()).gt(elim)) {
                            seg8 = false;
                            seg9 = false;
                            seg10 = false;
                        } // if (Math.abs(rs1) > elim)

                        if (seg9) {

                            if (kdflg == 1) {
                                iflag = 1;
                            } // if (kdflg == 1)

                            if ( (rs1.ge(DoubleDouble.valueOf(0.0))) && (kdflg == 1)) {
                                iflag = 3;
                            } // if (rs1 >= 0.0) && (kdflg == 1)
                        } // if (seg9)

                        seg9 = true;
                    } // if (Math.abs(rs1) >= alim)

                    if (seg10) {
                        zairy(argdr[0], argdi[0], 0, 2, air, aii, nai, idum);
                        zairy(argdr[0], argdi[0], 1, 2, dair, daii, ndai, idum);
                        str = (dair[0].multiply(bsumdr[0])).subtract(daii[0].multiply(bsumdi[0]));
                        sti = (dair[0].multiply(bsumdi[0])).add(daii[0].multiply(bsumdr[0]));
                        str = str.add( (air[0].multiply(asumdr[0])).subtract(aii[0].multiply(asumdi[0])));
                        sti = sti.add( (air[0].multiply(asumdi[0])).add(aii[0].multiply(asumdr[0])));
                        ptr = (str.multiply(phidr[0])).subtract(sti.multiply(phidi[0]));
                        pti = (str.multiply(phidi[0])).add(sti.multiply(phidr[0]));
                        s2r[0] = (ptr.multiply(csr)).subtract(pti.multiply(csi));
                        s2i[0] = (ptr.multiply(csi)).add(pti.multiply(csr));
                        str = (s1r[0].exp()).multiply(cssr[iflag - 1]);
                        s1r[0] = str.multiply(s1i[0].cos());
                        s1i[0] = str.multiply(s1i[0].sin());
                        str = (s2r[0].multiply(s1r[0])).subtract(s2i[0].multiply(s1i[0]));
                        s2i[0] = (s2r[0].multiply(s1i[0])).add(s2i[0].multiply(s1r[0]));
                        s2r[0] = (DoubleDouble)str.clone();

                        if (iflag == 1) {
                            zuchk(s2r[0], s2i[0], nw, bry[0]);

                            if (nw[0] != 0) {
                                s2r[0] = DoubleDouble.valueOf(0.0);
                                s2i[0] = DoubleDouble.valueOf(0.0);
                            } // if nw[0] != 0)
                        } // if (iflag == 1)
                    } // if (seg10)

                    seg10 = true;
                } // if (seg7)

                seg7 = true;

                while (true) {

                    if (seg8) {

                        if (yy.le(DoubleDouble.valueOf(0.0))) {
                            s2i[0] = s2i[0].negate();
                        } // if (yy <= 0.0)

                        cyr[kdflg - 1] = (DoubleDouble)s2r[0].clone();
                        cyi[kdflg - 1] = (DoubleDouble)s2i[0].clone();
                        c2r[0] = (DoubleDouble)s2r[0].clone();
                        c2i[0] = (DoubleDouble)s2i[0].clone();
                        s2r[0] = s2r[0].multiply(csrr[iflag - 1]);
                        s2i[0] = s2i[0].multiply(csrr[iflag - 1]);

                        // Add I and K functions, K sequence in Y(i), i = 1,n
                        s1r[0] = (DoubleDouble)yr[kk - 1].clone();
                        s1i[0] = (DoubleDouble)yi[kk - 1].clone();

                        if (kode != 1) {
                            zs1s2(zrr, zri, s1r, s1i, s2r, s2i, nw, asc, iuf);
                            nz[0] = nz[0] + nw[0];
                        } // if (kode != 1)

                        yr[kk - 1] = ((s1r[0].multiply(cspnr)).subtract(s1i[0].multiply(cspni))).add(s2r[0]);
                        yi[kk - 1] = ((s1r[0].multiply(cspni)).add(s1i[0].multiply(cspnr))).add(s2i[0]);
                        kk = kk - 1;
                        cspnr = cspnr.negate();
                        cspni = cspni.negate();
                        str = (DoubleDouble)csi.clone();
                        csi = csr.negate();
                        csr = (DoubleDouble)str.clone();

                        if ( (c2r[0].equals(DoubleDouble.valueOf(0.0))) && (c2i[0].equals(DoubleDouble.valueOf(0.0)))) {
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

                    if (rs1.gt(DoubleDouble.valueOf(0.0))) {
                        nz[0] = -1;

                        return;
                    } // if (rs1 > 0.0)

                    s2r[0] = DoubleDouble.valueOf(0.0);
                    s2i[0] = DoubleDouble.valueOf(0.0);
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
        s1r[0] = (DoubleDouble)cyr[0].clone();
        s1i[0] = (DoubleDouble)cyi[0].clone();
        s2r[0] = (DoubleDouble)cyr[1].clone();
        s2i[0] = (DoubleDouble)cyi[1].clone();
        csr = (DoubleDouble)csrr[iflag - 1].clone();
        ascle = (DoubleDouble)bry[iflag - 1].clone();
        fn = DoubleDouble.valueOf(inu + il);

        for (i = 1; i <= il; i++) {
            c2r[0] = (DoubleDouble)s2r[0].clone();
            c2i[0] = (DoubleDouble)s2i[0].clone();
            s2r[0] = s1r[0].add( (fn.add(fnf)).multiply( (rzr.multiply(c2r[0])).subtract(rzi.multiply(c2i[0]))));
            s2i[0] = s1i[0].add( (fn.add(fnf)).multiply( (rzr.multiply(c2i[0])).add(rzi.multiply(c2r[0]))));
            s1r[0] = (DoubleDouble)c2r[0].clone();
            s1i[0] = (DoubleDouble)c2i[0].clone();
            fn = fn.subtract(DoubleDouble.valueOf(1.0));
            c2r[0] = s2r[0].multiply(csr);
            c2i[0] = s2i[0].multiply(csr);
            ckr = (DoubleDouble)c2r[0].clone();
            cki = (DoubleDouble)c2i[0].clone();
            c1r[0] = (DoubleDouble)yr[kk - 1].clone();
            c1i[0] = (DoubleDouble)yi[kk - 1].clone();

            if (kode != 1) {
                zs1s2(zrr, zri, c1r, c1i, c2r, c2i, nw, asc, iuf);
                nz[0] = nz[0] + nw[0];
            } // if (kode != 1)

            yr[kk - 1] = ((c1r[0].multiply(cspnr)).subtract(c1i[0].multiply(cspni))).add(c2r[0]);
            yi[kk - 1] = ((c1r[0].multiply(cspni)).add(c1i[0].multiply(cspnr))).add(c2i[0]);
            kk = kk - 1;
            cspnr = cspnr.negate();
            cspni = cspni.negate();

            if (iflag >= 3) {
                continue;
            } // if (iflag >= 3)

            c2r[0] = ckr.abs();
            c2i[0] = cki.abs();
            c2m = c2r[0].max(c2i[0]);

            if (c2m.le(ascle)) {
                continue;
            } // if (c2m <= ascle)

            iflag = iflag + 1;
            ascle = (DoubleDouble)bry[iflag - 1].clone();
            s1r[0] = s1r[0].multiply(csr);
            s1i[0] = s1i[0].multiply(csr);
            s2r[0] = (DoubleDouble)ckr.clone();
            s2i[0] = (DoubleDouble)cki.clone();
            s1r[0] = s1r[0].multiply(cssr[iflag - 1]);
            s1i[0] = s1i[0].multiply(cssr[iflag - 1]);
            s2r[0] = s2r[0].multiply(cssr[iflag - 1]);
            s2i[0] = s2i[0].multiply(cssr[iflag - 1]);
            csr = (DoubleDouble)csrr[iflag - 1].clone();
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
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param ikflg int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nuf int[]
     */
    private void zuoik(DoubleDouble zr, DoubleDouble zi, final DoubleDouble fnu, final int kode, final int ikflg, final int n,
            final DoubleDouble[] yr, final DoubleDouble[] yi, final int[] nuf) {
        final DoubleDouble aic = DoubleDouble.valueOf(1.265512123484645396);
        int nn;
        DoubleDouble zrr;
        DoubleDouble zri;
        DoubleDouble zbr;
        DoubleDouble zbi;
        DoubleDouble ax;
        DoubleDouble ay;
        int iform;
        DoubleDouble gnu;
        DoubleDouble fnn;
        DoubleDouble gnn;
        int init[] = new int[1];
        DoubleDouble czr;
        DoubleDouble czi;
        DoubleDouble znr = DoubleDouble.valueOf(0.0);
        DoubleDouble zni = DoubleDouble.valueOf(0.0);
        final DoubleDouble[] zeta1r = new DoubleDouble[1];
        final DoubleDouble[] zeta2r = new DoubleDouble[1];
        final DoubleDouble[] zeta1i = new DoubleDouble[1];
        final DoubleDouble[] zeta2i = new DoubleDouble[1];
        final DoubleDouble[] cwrkr = new DoubleDouble[16];
        final DoubleDouble[] cwrki = new DoubleDouble[16];
        final DoubleDouble[] phir = new DoubleDouble[1];
        final DoubleDouble[] phii = new DoubleDouble[1];
        final DoubleDouble[] argr = new DoubleDouble[1];
        final DoubleDouble[] argi = new DoubleDouble[1];
        DoubleDouble aphi;
        DoubleDouble rcz;
        final DoubleDouble[] sumr = new DoubleDouble[1];
        final DoubleDouble[] sumi = new DoubleDouble[1];
        final DoubleDouble[] asumr = new DoubleDouble[1];
        final DoubleDouble[] asumi = new DoubleDouble[1];
        final DoubleDouble[] bsumr = new DoubleDouble[1];
        final DoubleDouble[] bsumi = new DoubleDouble[1];
        int i;
        DoubleDouble ascle;
        final int[] idum = new int[1];
        DoubleDouble aarg = DoubleDouble.valueOf(1.0);
        boolean seg1 = true;
        boolean seg2 = true;
        boolean seg3 = true;
        boolean seg4 = true;
        final DoubleDouble[] str = new DoubleDouble[1];
        final DoubleDouble[] sti = new DoubleDouble[1];
        final int[] nw = new int[1];

        nuf[0] = 0;
        nn = n;
        zrr = (DoubleDouble)zr.clone();
        zri = (DoubleDouble)zi.clone();

        if (zr.lt(DoubleDouble.valueOf(0.0))) {
            zrr = zr.negate();
            zri = zi.negate();
        }

        zbr = (DoubleDouble)zrr.clone();
        zbi = (DoubleDouble)zri.clone();
        ax = (DoubleDouble.valueOf(1.7321)).multiply(zr.abs());
        ay = zi.abs();
        iform = 1;

        if (ay.gt(ax)) {
            iform = 2;
        }

        gnu = fnu.max(DoubleDouble.valueOf(1.0));

        if (ikflg != 1) {
            fnn = DoubleDouble.valueOf(nn);
            gnn = (fnu.add(fnn)).subtract(DoubleDouble.valueOf(1.0));
            gnu = gnn.max(fnn);
        } // if (ikflg != 1)

        // Only the magnitude of arg and phi are needed along with the real
        // parts of zeta1, zeta2, and zb. No attempt is made to get the sign
        // of the imaginary part correct
        if (iform != 2) {
            init[0] = 0;
            zunik(zrr, zri, gnu, ikflg, 1, init, phir, phii, zeta1r, zeta1i, zeta2r, zeta2i, sumr, sumi, cwrkr, cwrki);
            czr = (zeta1r[0].negate()).add(zeta2r[0]);
            czi = (zeta1i[0].negate()).add(zeta2i[0]);
        } // if (iform != 2)
        else { // iform == 2
            znr = (DoubleDouble)zri.clone();
            zni = zrr.negate();

            if (zi.le(DoubleDouble.valueOf(0.0))) {
                znr = znr.negate();
            }

            zunhj(znr, zni, gnu, 1, phir, phii, argr, argi, zeta1r, zeta1i, zeta2r, zeta2i, asumr, asumi, bsumr, bsumi);
            czr = (zeta1r[0].negate()).add(zeta2r[0]);
            czi = (zeta1i[0].negate()).add(zeta2i[0]);
            aarg = zabs(argr[0], argi[0]);
        } // else iform == 2

        if (kode != 1) {
            czr = czr.subtract(zbr);
            czi = czi.subtract(zbi);
        } // if (kode != 1)

        if (ikflg != 1) {
            czr = czr.negate();
            czi = czi.negate();
        } // if (ikflg != 1)

        aphi = zabs(phir[0], phii[0]);
        rcz = (DoubleDouble)czr.clone();

        // Overflow test
        if (rcz.gt(elim)) {
            nuf[0] = -1;

            return;
        } // if (rcz > elim)

        while (true) {

            if (seg1) {

                if (rcz.ge(alim)) {
                    rcz = rcz.add(aphi.log());

                    if (iform == 2) {
                        rcz = (rcz.subtract((DoubleDouble.valueOf(0.25)).multiply(aarg.log()))).subtract(aic);
                    } // if (iform == 2)

                    if (rcz.gt(elim)) {
                        nuf[0] = -1;

                        return;
                    } // if (rcz > elim)

                    break;
                } // if (rcz >= alim)

                // Underflow test
                if (rcz.ge(elim.negate())) {

                    if (rcz.gt(alim.negate())) {
                        break;
                    }

                    rcz = rcz.add(aphi.log());

                    if (iform == 2) {
                        rcz = (rcz.subtract((DoubleDouble.valueOf(0.25)).multiply(aarg.log()))).subtract(aic);
                    } // if (iform == 2)

                    if (rcz.gt(elim.negate())) {
                        seg2 = false;
                    }
                } // if (rcz >= -elim)
            } // if (seg1)

            seg1 = true;

            if (seg2) {

                for (i = 1; i <= nn; i++) {
                    yr[i - 1] = DoubleDouble.valueOf(0.0);
                    yi[i - 1] =DoubleDouble.valueOf( 0.0);
                } // for (i = 1; i <= nn; i++)

                nuf[0] = nn;

                return;
            } // if (seg2)

            seg2 = true;
            ascle = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).divide(tol);
            zlog(phir[0], phii[0], str, sti, idum);
            czr = czr.add(str[0]);
            czi = czi.add(sti[0]);

            if (iform != 1) {
                zlog(argr[0], argi[0], str, sti, idum);
                czr = (czr.subtract((DoubleDouble.valueOf(0.25)).multiply(str[0]))).subtract(aic);
                czi = czi.subtract((DoubleDouble.valueOf(0.25)).multiply(sti[0]));
            } // if (iform != 1)

            ax = (rcz.exp()).divide(tol);
            ay = (DoubleDouble)czi.clone();
            czr = ax.multiply(ay.cos());
            czi = ax.multiply(ay.sin());
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
                gnu = fnu.add(DoubleDouble.valueOf(nn - 1.0));

                if (iform != 2) {
                    init[0] = 0;
                    zunik(zrr, zri, gnu, ikflg, 1, init, phir, phii, zeta1r, zeta1i, zeta2r, zeta2i, sumr, sumi, cwrkr,
                            cwrki);
                    czr = (zeta1r[0].negate()).add(zeta2r[0]);
                    czi = (zeta1i[0].negate()).add(zeta2i[0]);
                } // if (iform != 2)
                else {
                    zunhj(znr, zni, gnu, 1, phir, phii, argr, argi, zeta1r, zeta1i, zeta2r, zeta2i, asumr, asumi,
                            bsumr, bsumi);
                    czr = (zeta1r[0].negate()).add(zeta2r[0]);
                    czi = (zeta1i[0].negate()).add(zeta2i[0]);
                    aarg = zabs(argr[0], argi[0]);
                } // else

                if (kode != 1) {
                    czr = czr.subtract(zbr);
                    czi = czi.subtract(zbi);
                } // if (kode != 1)

                aphi = zabs(phir[0], phii[0]);
                rcz = (DoubleDouble)czr.clone();

                if (rcz.ge(elim.negate())) {

                    if (rcz.gt(alim.negate())) {
                        return;
                    } // if (rcz > (-alim)

                    rcz = rcz.add(aphi.log());

                    if (iform == 2) {
                        rcz = (rcz.subtract((DoubleDouble.valueOf(0.25)).multiply(aarg.log()))).subtract(aic);
                    }

                    if (rcz.gt(elim.negate())) {
                        seg4 = false;
                    } // if (rcz > (-elim))
                } // if (rcz >= (-elim))
            } // if (seg3)

            seg3 = true;

            if (seg4) {
                yr[nn - 1] = DoubleDouble.valueOf(0.0);
                yi[nn - 1] = DoubleDouble.valueOf(0.0);
                nn = nn - 1;
                nuf[0] = nuf[0] + 1;

                if (nn == 0) {
                    return;
                } // if (nn == 0)

                continue;
            } // if (seg4)

            seg4 = true;
            ascle = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).divide(tol);
            zlog(phir[0], phii[0], str, sti, idum);
            czr = czr.add(str[0]);
            czi = czi.add(sti[0]);

            if (iform != 1) {
                zlog(argr[0], argi[0], str, sti, idum);
                czr = (czr.subtract((DoubleDouble.valueOf(0.25)).multiply(str[0]))).subtract(aic);
                czi = czi.subtract((DoubleDouble.valueOf(0.25)).multiply(sti[0]));
            } // if (iform != 1)

            ax = (rcz.exp()).divide(tol);
            ay = (DoubleDouble)czi.clone();
            czr = ax.multiply(ay.cos());
            czi = ax.multiply(ay.sin());
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
     * @param zrr DoubleDouble
     * @param zri DoubleDouble
     * @param fnu DoubleDouble
     * @param kode int
     * @param n int
     * @param yr DoubleDouble[]
     * @param yi DoubleDouble[]
     * @param nz int[]
     * @param cwr DoubleDouble[]
     * @param cwi DoubleDouble[]
     */
    private void zwrsk(final DoubleDouble zrr, final DoubleDouble zri, final DoubleDouble fnu, final int kode, final int n,
            final DoubleDouble[] yr, final DoubleDouble[] yi, final int[] nz, final DoubleDouble[] cwr, final DoubleDouble[] cwi) {
        final int[] nw = new int[1];
        DoubleDouble cinur;
        DoubleDouble cinui;
        DoubleDouble acw;
        DoubleDouble ascle;
        DoubleDouble csclr;
        DoubleDouble c1r;
        DoubleDouble c1i;
        DoubleDouble c2r;
        DoubleDouble c2i;
        DoubleDouble str;
        DoubleDouble sti;
        DoubleDouble ptr;
        DoubleDouble pti;
        DoubleDouble ctr;
        DoubleDouble cti;
        DoubleDouble act;
        DoubleDouble ract;
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
        cinur = DoubleDouble.valueOf(1.0);
        cinui = DoubleDouble.valueOf(0.0);

        if (kode != 1) {
            cinur = zri.cos();
            cinui = zri.sin();
        } // if (kode != 1)

        // On low exponent machines the k functions can be close to both the
        // under and overflow limits and the normalization must be scaled to
        // prevent over or underflow. zuoik has determined that the result is
        // on scale.
        acw = zabs(cwr[1], cwi[1]);
        ascle = ((DoubleDouble.valueOf(1.0E3)).multiply(tiny)).divide(tol);
        csclr = DoubleDouble.valueOf(1.0);

        if (acw.le(ascle)) {
            csclr = tol.reciprocal();
        } // if (acw <= ascle
        else { // acw > ascle
            ascle = ascle.reciprocal();

            if (acw.ge(ascle)) {
                csclr = (DoubleDouble)tol.clone();
            } // if (acw >= ascle)
        } // else acw > ascle

        c1r = cwr[0].multiply(csclr);
        c1i = cwi[0].multiply(csclr);
        c2r = cwr[1].multiply(csclr);
        c2i = cwi[1].multiply(csclr);
        str = (DoubleDouble)yr[0].clone();
        sti = (DoubleDouble)yi[0].clone();

        // cinu = cinu*(CONJG(ct)/CABS(ct))*(1.0/CABS(ct)) prevents
        // under- or overflow prematurely by squaring CABS(ct)
        ptr = (str.multiply(c1r)).subtract(sti.multiply(c1i));
        pti = (str.multiply(c1i)).add(sti.multiply(c1r));
        ptr = ptr.add(c2r);
        pti = pti.add(c2i);
        ctr = (zrr.multiply(ptr)).subtract(zri.multiply(pti));
        cti = (zrr.multiply(pti)).add(zri.multiply(ptr));
        act = zabs(ctr, cti);
        ract = act.reciprocal();
        ctr = ctr.multiply(ract);
        cti = (cti.negate()).multiply(ract);
        ptr = cinur.multiply(ract);
        pti = cinui.multiply(ract);
        cinur = (ptr.multiply(ctr)).subtract(pti.multiply(cti));
        cinui = (ptr.multiply(cti)).add(pti.multiply(ctr));
        yr[0] = cinur.multiply(csclr);
        yi[0] = cinui.multiply(csclr);

        if (n == 1) {
            return;
        } // if (n == 1)

        for (i = 2; i <= n; i++) {
            ptr = (str.multiply(cinur)).subtract(sti.multiply(cinui));
            cinui = (str.multiply(cinui)).add(sti.multiply(cinur));
            cinur = (DoubleDouble)ptr.clone();
            str = (DoubleDouble)yr[i - 1].clone();
            sti = (DoubleDouble)yi[i - 1].clone();
            yr[i - 1] = cinur.multiply(csclr);
            yi[i - 1] = cinui.multiply(csclr);
        } // for (i = 2; i <= n; i++)

        return;
    }

}
