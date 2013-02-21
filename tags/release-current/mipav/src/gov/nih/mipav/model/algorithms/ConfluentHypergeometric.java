package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;


/**
 * <p>
 * This code calculates the confluent hypergeometric function of the first and second kinds For the confluent
 * hypergeometric function of the first kind a typical usage for the routine requiring real parameters and a real
 * argument would be:
 * 
 * <pre>
 * double result[] = new double[1];
 * ConfluentHypergeometric ch = new ConfluentHypergeometric( -0.5, 1, 1.0, result);
 * ch.run();
 * Preferences.debug(&quot;Confluent hypergeomtric result = &quot; + result[0] + &quot;\n&quot;);
 * UI.setDataText(&quot;Confluent hypergeometric result = &quot; + result[0] + &quot;\n&quot;);
 * </pre>
 * 
 * </p>
 * <p>
 * For the confluent hypergeometric function of the first kind a typcial usage for the routine requiring real parameters
 * and a complex argument would be:
 * 
 * <pre>
 * ConfluentHypergeometric cf = new ConfluentHypergeometric( -0.5, 1.0, realZ, imagZ, realResult, imagResult);
 * ch.run();
 * System.out.println(&quot;realResult[0] = &quot; + realResult[0]);
 * System.out.println(&quot;imagResult[0] = &quot; + imagResult[0]);
 * </pre>
 * 
 * </p>
 * <p>
 * For the confluent hypergeometric function of the first kind a typical usage for the routine allowing complex
 * parameters and a complex argument would be:
 * 
 * <pre>
 * ConfluentHypergeometric cf;
 * int Lnchf = 0;
 * int ip = 700;
 * double realResult[] = new double[1];
 * double imagResult[] = new double[1];
 * cf = new ConfluentHypergeometric( -0.5, 0.0, 1.0, 0.0, 10.0, 0.0, Lnchf, ip, realResult, imagResult);
 * cf.run();
 * System.out.println(&quot;realResult[0] = &quot; + realResult[0]);
 * System.out.println(&quot;imagResult[0] = &quot; + imagResult[0]);
 * </pre>
 * 
 * </p>
 * <p>
 * Since this complex routine allows large magnitude x, it should generally be used instead of the real version routine.
 * </p>
 * <p>
 * For the confluent hypergeometric function of the second kind a typical usage for the routine requiring real
 * parameters and a real argument would be:
 * 
 * <pre>
 *   double result[] = new double[1];
 *   int method[] = new int[1];
 *   ConfluentHypergeometric ch = new ConfluentHypergeometric(-0.5, 1, 1.0, result, method);
 *   ch.run();
 *   Preferences.debug(&quot;Confluent hypergeomtric result = &quot; + result[0] +  &quot; method = &quot; + 
 *                       method[0] + \n&quot;);
 * </pre>
 * 
 * </p>
 * <p>
 * For 1F1(-1/2, 1, x) tested with Shanjie Zhang and Jianming Jin Computation of Special Functions CHGM routine and ACM
 * Algorithm 707 conhyp routine by Mark Nardin, W. F. Perger, and Atul Bhalla the results were:
 * <ul>
 * <li>From x = -706 to x = +184 the results of the 2 routines matched to at least 1 part in 1E5.</li>
 * <li>For x = -3055 to x = +184 the ACM routine seemed to give valid results.</li>
 * <li>For x <= -3056 the ACM routine results oscillated wildly and at x = -3200 the result was frozen at 1.0E75.</li>
 * <li>For x >= +185 the ACM result was frozen at 1.0E75.</li>
 * <li>The log result was also seen to oscillate for x <= -3056.</li>
 * 
 * <li>For x = -706 to x = +781 the CHGM routine seemed to give valid results.</li>
 * <li>For x = -707 to x = -745 the CHGM routine gave infinity and for x <= -746 the CHGM routine gave NaN.</li>
 * <li>For x >= +783 the CHGM routine gave a result of -infinity.</li>
 * 
 * <li>For 1F1(-1/2, 2, x) the results were very similar:
 * <li>From x = -706 to x = +189 the results of the 2 routines matched to at least 1 part in 1E5.</li>
 * 
 * <li>From x = -3058 to x = +189 the ACM routine seemed to give valid results.</li>
 * <li>At x <= -3059 the ACM results oscillated wildly.</li>
 * <li>For x > +189 the ACM result was frozen at 1.0E75.</li>
 * 
 * <li>For x = -706 to x = +791 the CHGM routine seemed to give valid results.</li>
 * <li>For x = -707 to x = -745 the CHGM routine gave infinity and for x <= -746 the CHGM routine gave NaN.</li>
 * <li>For x >= +792 the CHGM routine gave a result of -infinity.</li>
 * </ul>
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
 * 
 * <hr>
 * 
 * <p>
 * Excerpt from Appendix C of Computation of Special Functions by Shanjie Zhang and Jianming Jin:
 * 
 * <blockquote> All the programs and subroutines contained in this diskette are copyrighted. However, we give permission
 * to the reader who purchases this book to incorporate any of these programs into his or her programs provided that the
 * copyright is acknowledged. </blockquote>
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
 * </p>
 */
public class ConfluentHypergeometric {
    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Confluent Hypergeometric Function of the First Kind. */
    public static final int CONFLUENT_HYPERGEOMETRIC_FIRST_KIND = 1;

    /** Confluent Hypergeometric Function of the Second Kind */
    public static final int CONFLUENT_HYPERGEOMETRIC_SECOND_KIND = 2;

    public static final int REAL_VERSION = 1;

    public static final int COMPLEX_VERSION = 2;

    public static final int REALPARAM_COMPLEXARG_VERSION = 3;

    /** Tells whether confluent hypergeometric function is of first or second kind */
    private final int kind;

    /** Tells whether real number of complex number version */
    private final int version;

    /** Input parameter */
    private double a;

    /** Input parameter */
    private double b;

    /** Input argument */
    private double x;

    /** Outputted result */
    private double result[];

    /** Method used in calculating the confluent hypergeometric function of the second kind. */
    private int method[];

    /** Input parameter */
    private double realA;

    private double imagA;

    /** Input parameter */
    private double realB;

    private double imagB;

    /** Input argument */
    private double realZ;

    private double imagZ;

    /**
     * Tells how the result should be represented A '0' will return the result in standard exponential form. A '1' will
     * return the log of the result.
     */
    private int Lnchf;

    /**
     * Specifies how many array positions are desired (usually 10 is sufficient). More difficult cases may require this
     * parameter to be 100 or more, and it is not always trivial to predict which cases these might be. One pragmatic
     * approach is to simply run the evaluator using a value of 10, then increase ip, run the evaluator again and
     * compare the returned results. Overwriting memory may occur if ip exceeds 776. Setting IP = 0 causes the program
     * to estimate the number of array positions
     */
    private int ip;

    /** outputted result */
    private double realResult[];

    private double imagResult[];

    /** Create arrays of size length+2 */
    private final int length = 777;

    /** Size of the arrays */
    private int L;

    /** Number of digits required to represent the numbers with the required accuracy */
    private double rmax;

    private int bit;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * @param a input parameter
     * @param b input parameter
     * @param x input argument
     * @param result outputted result
     */
    public ConfluentHypergeometric(final int kind, final double a, final double b, final double x,
            final double result[]) {
        this.a = a;
        this.b = b;
        this.x = x;
        this.result = result;
        this.kind = ConfluentHypergeometric.CONFLUENT_HYPERGEOMETRIC_FIRST_KIND;
        this.version = ConfluentHypergeometric.REAL_VERSION;
    }

    /**
     * @param a input parameter
     * @param b input parameter
     * @param x input argument, x > 0
     * @param result outputted result
     * @param method method code used
     */
    public ConfluentHypergeometric(final double a, final double b, final double x, final double result[],
            final int method[]) {
        this.a = a;
        this.b = b;
        this.x = x;
        this.result = result;
        this.method = method;
        this.kind = ConfluentHypergeometric.CONFLUENT_HYPERGEOMETRIC_SECOND_KIND;
        this.version = ConfluentHypergeometric.REAL_VERSION;
    }

    /**
     * @param a input real parameter
     * @param b input real parameter
     * @param realZ input real part of argument
     * @param imagZ input imaginary part of argument
     * @param realResult real part of outputted result
     * @param imagResult imaginary part of outputted result
     */
    public ConfluentHypergeometric(final double a, final double b, final double realZ, final double imagZ,
            final double realResult[], final double imagResult[]) {
        this.a = a;
        this.b = b;
        this.realZ = realZ;
        this.imagZ = imagZ;
        this.realResult = realResult;
        this.imagResult = imagResult;
        this.kind = ConfluentHypergeometric.CONFLUENT_HYPERGEOMETRIC_FIRST_KIND;
        this.version = ConfluentHypergeometric.REALPARAM_COMPLEXARG_VERSION;
    }

    /**
     * @param realA Real part of first input parameter
     * @param imagA Imaginary part of first input parameter
     * @param realB Real part of second input parameter
     * @param imagB Imaginary part of second input parameter
     * @param realZ Real part of input argument
     * @param imagZ Imaginary part of input argument
     * @param Lnchf = 0 for standard result, = 1 for log of result
     * @param ip Number of array positions desired
     * @param realResult Real part of outputted result
     * @param imagResult Imaginary part of outputted result
     */
    public ConfluentHypergeometric(final double realA, final double imagA, final double realB, final double imagB,
            final double realZ, final double imagZ, final int Lnchf, final int ip, final double realResult[],
            final double imagResult[]) {
        this.realA = realA;
        this.imagA = imagA;
        this.realB = realB;
        this.imagB = imagB;
        this.realZ = realZ;
        this.imagZ = imagZ;
        this.Lnchf = Lnchf;
        this.ip = ip;
        this.realResult = realResult;
        this.imagResult = imagResult;
        this.kind = ConfluentHypergeometric.CONFLUENT_HYPERGEOMETRIC_FIRST_KIND;
        this.version = ConfluentHypergeometric.COMPLEX_VERSION;
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
        if ( (kind == ConfluentHypergeometric.CONFLUENT_HYPERGEOMETRIC_FIRST_KIND)
                && (version == ConfluentHypergeometric.REAL_VERSION)) {
            firstKindRealArgument();
        } else if ( (kind == ConfluentHypergeometric.CONFLUENT_HYPERGEOMETRIC_FIRST_KIND)
                && (version == ConfluentHypergeometric.REALPARAM_COMPLEXARG_VERSION)) {
            firstKindComplexArgument();
        } else if ( (kind == ConfluentHypergeometric.CONFLUENT_HYPERGEOMETRIC_FIRST_KIND)
                && (version == ConfluentHypergeometric.COMPLEX_VERSION)) {
            firstKindComplex();
        } else if (kind == ConfluentHypergeometric.CONFLUENT_HYPERGEOMETRIC_SECOND_KIND) {
            secondKindRealArgument();
        } else {
            MipavUtil.displayError("Illegal kind argument");
            return;
        }
    } // run()

    /**
     * Port of Algorithm 707, Collected Algorithms from ACM. Original Work published in Transactions on Mathematical
     * Software, Vol. 18. No. 3, September, 1992, pp. 345-349. This is a port of Solution to the Confluent
     * Hypergeometric Function by Mark Nardin, W. F. Perger, and Atul Bhalla, Michigan Technological University,
     * Copyright 1989. Description: A numerical evaluator for the confluent hypergeometric function for complex
     * arguments with large magnitudes using a direct summation of the Kummer series. The method used allows an accuracy
     * of up to thirteen decimal places through the use of large arrays and a single final division. The confluent
     * hypergeometric function of the first kind is the solution to the equation: zf"(z) + (a-z)f'(z) - bf(z) = 0
     * Reference: Numerical evaluation of the confluent hypergeometric function for complex arguments of large
     * magnitudes by Mark Nardin, W. F. Perger, and Atul Bhalla, Journal of Computational and Applied Mathematics, Vol.
     * 39, 1992, pp. 193-200.
     */
    private void firstKindComplex() {
        double ang;
        double nterm;
        double fx;
        double term1;
        double max;
        double term2;
        double realTerm2;
        double imagTerm2;
        double denom;
        double realTemp;
        int i;
        double sigfig;
        double ar2;
        double ar;
        double ai2;
        double ai;
        double cr2;
        double cr;
        double ci2;
        double ci;
        double xr2;
        double xr;
        double xi2;
        double xi;
        final double sumr[] = new double[length + 2];
        final double sumi[] = new double[length + 2];
        final double numr[] = new double[length + 2];
        final double numi[] = new double[length + 2];
        final double denomr[] = new double[length + 2];
        final double denomi[] = new double[length + 2];
        double cnt;
        double mx1;
        double mx2;
        final double qr1[] = new double[length + 2];
        final double qr2[] = new double[length + 2];
        final double qi1[] = new double[length + 2];
        final double qi2[] = new double[length + 2];
        double realNum;
        double imagNum;
        double absVal;
        int nmach;
        int icount;
        int ixcnt;

        if ( (realZ != 0.0) && (imagZ != 0.0)) {
            ang = Math.atan2(imagZ, realZ);
        } else {
            ang = 1.0;
        }
        if (Math.abs(ang) < (0.5 * Math.PI)) {
            ang = 1.0;
        } else {
            ang = Math.sin(Math.abs(ang) - (0.5 * Math.PI)) + 1.0;
        }
        max = 0.0;
        nterm = 0.0;
        fx = 0.0;
        term1 = 0.0;
        while (true) {
            nterm = nterm + 1;
            realTerm2 = (realA + nterm - 1.0) * realZ - imagA * imagZ;
            imagTerm2 = (realA + nterm - 1.0) * imagZ + imagA * realZ;
            realTerm2 = realTerm2 / nterm;
            imagTerm2 = imagTerm2 / nterm;
            denom = (realB + nterm - 1.0) * (realB + nterm - 1.0) + imagB * imagB;
            realTerm2 = realTerm2 / denom;
            imagTerm2 = imagTerm2 / denom;
            realTemp = realTerm2 * (realB * nterm - 1.0) + imagTerm2 * imagB;
            imagTerm2 = -realTerm2 * imagB + imagTerm2 * (realB * nterm - 1.0);
            term2 = Math.sqrt(realTemp * realTemp + imagTerm2 * imagTerm2);
            if (term2 == 0.0) {
                break;
            } // if (term2 == 0.0)
            if (term2 < 1.0) {
                if ( (realA + nterm) > 2.0) {
                    if ( (realB + nterm) > 2.0) {
                        if (term2 < term1) {
                            break;
                        } // if (term2 < term1)
                    } // if ((realB + nterm) > 2.0)
                } // if ((realA + nterm) > 2.0)
            } // if (term2 < 1.0)
            fx = fx + Math.log(term2);
            if (fx > max) {
                max = fx;
            } // if (fx > max)
            term1 = term2;
        } // while (true)
        bit = bits();
        // Preferences.debug("bit = " + bit + "\n", Preferences.DEBUG_ALGORITHM);
        max = max * 2.0 / (bit * 0.693147181);
        L = (int) (max * ang) + 7;
        if (L < 5) {
            L = 5;
        } // (if L < 5)
        if (ip > L) {
            L = ip;
        } // if (ip > L)
        // Preferences.debug("L = " + L + "\n", Preferences.DEBUG_ALGORITHM);

        // Sum the Kummer series and return the solution of the confluent hypergeometric
        // function
        rmax = Math.pow(2.0, bit / 2);
        sigfig = Math.pow(2.0, bit / 4);

        // Set to zero any arguments which are below the precision of the machine
        ar2 = realA * sigfig;
        ar = Math.floor(ar2 + 0.5);
        ar2 = Math.rint( (ar2 - ar) * rmax);
        ai2 = imagA * sigfig;
        ai = Math.floor(ai2 + 0.5);
        ai2 = Math.rint( (ai2 - ai) * rmax);
        cr2 = realB * sigfig;
        cr = Math.floor(cr2 + 0.5);
        cr2 = Math.rint( (cr2 - cr) * rmax);
        ci2 = imagB * sigfig;
        ci = Math.floor(ci2 + 0.5);
        ci2 = Math.rint( (ci2 - ci) * rmax);
        xr2 = realZ * sigfig;
        xr = Math.floor(xr2 + 0.5);
        xr2 = Math.rint( (xr2 - xr) * rmax);
        xi2 = imagZ * sigfig;
        xi = Math.floor(xi2 + 0.5);
        xi2 = Math.rint( (xi2 - xi) * rmax);

        // Warn the user that the input value was so close to zero that it
        // was set equal to zero.
        if ( (realA != 0.0) && (ar == 0.0) && (ar2 == 0.0)) {
            Preferences.debug("Warning! Real part of A was set to zero\n", Preferences.DEBUG_ALGORITHM);
        }
        if ( (imagA != 0.0) && (ai == 0.0) && (ai2 == 0.0)) {
            Preferences.debug("Warning! Imaginary part of A was set to zero\n", Preferences.DEBUG_ALGORITHM);
        }
        if ( (realB != 0.0) && (cr == 0.0) && (cr2 == 0.0)) {
            Preferences.debug("Warning! Real part of B was set to zero\n", Preferences.DEBUG_ALGORITHM);
        }
        if ( (imagB != 0.0) && (ci == 0.0) && (ci2 == 0.0)) {
            Preferences.debug("Warning! Imaginary part of B was set to zero\n", Preferences.DEBUG_ALGORITHM);
        }
        if ( (realZ != 0.0) && (xr == 0.0) && (xr2 == 0.0)) {
            Preferences.debug("Warning! Real part of Z was set to zero\n", Preferences.DEBUG_ALGORITHM);
        }
        if ( (imagZ != 0.0) && (xi == 0.0) && (xi2 == 0.0)) {
            Preferences.debug("Warning! Imaginary part of Z was set to zero\n", Preferences.DEBUG_ALGORITHM);
        }

        // Screening of the case when B is zero or A negative integer
        if ( (cr == 0.0) && (cr2 == 0.0) && (ci == 0.0) && (ci2 == 0.0)) {
            MipavUtil.displayError("Error! Argument B was equal to zero");
            return;
        }
        nmach = (int) (0.4342944819 * Math.log(Math.pow(2.0, bit)));
        if ( (ci == 0.0) && (ci2 == 0.0) && (realB < 0.0)) {
            if (Math.abs(realB - Math.rint(realB)) < Math.pow(10.0, -nmach)) {
                MipavUtil.displayError("Error! Argument B was a negative integer");
                return;
            }
        }
        sumr[0] = 1.0;
        sumi[0] = 1.0;
        numr[0] = 1.0;
        numi[0] = 1.0;
        denomr[0] = 1.0;
        denomi[0] = 1.0;
        for (i = 1; i <= L + 2; i++) {
            sumr[i] = 0.0;
            sumi[i] = 0.0;
            numr[i] = 0.0;
            numi[i] = 0.0;
            denomr[i] = 0.0;
            denomi[i] = 0.0;
        } // for (i = 1; i <= L+2; i++)
        sumr[2] = 1.0;
        numr[2] = 1.0;
        denomr[2] = 1.0;
        cnt = sigfig;
        icount = -1;
        if ( (ai == 0.0) && (ai2 == 0.0) && (realA < 0.0)) {
            if (Math.abs(realA - Math.rint(realA)) < Math.pow(10.0, ( -nmach))) {
                icount = -(int) Math.round(realA);
            }
        }
        ixcnt = 0;
        while (true) {
            if (sumr[2] < 0.5) {
                mx1 = sumi[L + 2];
            } // if (sumr[2] < 0.5)
            else if (sumi[2] < 0.5) {
                mx1 = sumr[L + 2];
            } // else if (sumi[2] < 0.5)
            else {
                mx1 = Math.max(sumr[L + 2], sumi[L + 2]);
            } // else
            if (numr[2] < 0.5) {
                mx2 = numi[L + 2];
            } // if (numr[2] < 0.5)
            else if (numi[2] < 0.5) {
                mx2 = numr[L + 2];
            } // else if (numi[2] < 0.5)
            else {
                mx2 = Math.max(numr[L + 2], numi[L + 2]);
            } // else
            if ( (mx1 - mx2) > 2.0) {
                if (cr > 0.0) {
                    realNum = (ar * xr - ai * xi) / cnt;
                    imagNum = (ar * xi + ai * xr) / cnt;
                    denom = cr * cr + ci * ci;
                    realNum = realNum / denom;
                    imagNum = imagNum / denom;
                    realTemp = realNum * cr + imagNum * ci;
                    imagNum = -realNum * ci + imagNum * cr;
                    absVal = Math.sqrt(realTemp * realTemp + imagNum * imagNum);
                    if (absVal <= 1.0) {
                        break;
                    } // if (absVal <= 1.0)
                } // if (cr > 0.0)
            } // if ((mx1 - mx2) > 2.0)
            if (ixcnt == icount) {
                break;
            }
            ixcnt = ixcnt + 1;
            cmpmul(sumr, sumi, cr, ci, qr1, qi1);
            cmpmul(sumr, sumi, cr2, ci2, qr2, qi2);
            qr2[L + 2] = qr2[L + 2] - 1;
            qi2[L + 2] = qi2[L + 2] - 1;
            cmpadd(qr1, qi1, qr2, qi2, sumr, sumi);

            armult(sumr, cnt, sumr);
            armult(sumi, cnt, sumi);
            cmpmul(denomr, denomi, cr, ci, qr1, qi1);
            cmpmul(denomr, denomi, cr2, ci2, qr2, qi2);
            qr2[L + 2] = qr2[L + 2] - 1;
            qi2[L + 2] = qi2[L + 2] - 1;
            cmpadd(qr1, qi1, qr2, qi2, denomr, denomi);

            armult(denomr, cnt, denomr);
            armult(denomi, cnt, denomi);
            cmpmul(numr, numi, ar, ai, qr1, qi1);
            cmpmul(numr, numi, ar2, ai2, qr2, qi2);
            qr2[L + 2] = qr2[L + 2] - 1;
            qi2[L + 2] = qi2[L + 2] - 1;
            cmpadd(qr1, qi1, qr2, qi2, numr, numi);

            cmpmul(numr, numi, xr, xi, qr1, qi1);
            cmpmul(numr, numi, xr2, xi2, qr2, qi2);
            qr2[L + 2] = qr2[L + 2] - 1;
            qi2[L + 2] = qi2[L + 2] - 1;
            cmpadd(qr1, qi1, qr2, qi2, numr, numi);

            cmpadd(sumr, sumi, numr, numi, sumr, sumi);
            cnt = cnt + sigfig;
            ar = ar + sigfig;
            cr = cr + sigfig;
        } // while (true)
        arydiv(sumr, sumi, denomr, denomi);
        return;
    } // firstKindComplex

    /**
     * Returns the complex number resulting from the division of four arrays, representing two complex numbers. The
     * number returned will be in one of two different forms. Either standard scientific or as the log of the number.
     * 
     * @param ar
     * @param ai
     * @param br
     * @param bi
     */
    private void arydiv(final double ar[], final double ai[], final double br[], final double bi[]) {
        int rexp;
        int ir10;
        int ii10;
        double phi;
        final double n1[] = new double[1];
        final double n2[] = new double[1];
        final double n3[] = new double[1];
        final double e1[] = new double[1];
        final double e2[] = new double[1];
        final double e3[] = new double[1];
        double rr10;
        double ri10;
        double x;
        final double ae[][] = new double[2][2];
        final double be[][] = new double[2][2];
        double x1;
        double x2;
        double dum1;
        double dum2;
        final double ce[][] = new double[2][2];
        final double logRatio = Math.log(2.0) / Math.log(10.0);

        rexp = bit / 2;
        x = rexp * (ar[L + 2] - 2);
        rr10 = x * logRatio;
        ir10 = (int) rr10;
        rr10 = rr10 - ir10;
        x = rexp * (ai[L + 2] - 2);
        ri10 = x * logRatio;
        ii10 = (int) ri10;
        ri10 = ri10 - ii10;
        if (ar[0] >= 0) {
            dum1 = Math.abs(ar[2] * rmax * rmax + ar[3] * rmax + ar[4]);
        } else {
            dum1 = -Math.abs(ar[2] * rmax * rmax + ar[3] * rmax + ar[4]);
        }
        if (ai[0] >= 0) {
            dum2 = Math.abs(ai[2] * rmax * rmax + ai[3] * rmax + ai[4]);
        } else {
            dum2 = -Math.abs(ai[2] * rmax * rmax + ai[3] * rmax + ai[4]);
        }
        dum1 = dum1 * Math.pow(10.0, rr10);
        dum2 = dum2 * Math.pow(10.0, ri10);
        conv12(dum1, dum2, ae);
        ae[0][1] = ae[0][1] + ir10;
        ae[1][1] = ae[1][1] + ii10;
        x = rexp * (br[L + 2] - 2);
        rr10 = x * logRatio;
        ir10 = (int) rr10;
        rr10 = rr10 - ir10;
        x = rexp * (bi[L + 2] - 2);
        ri10 = x * logRatio;
        ii10 = (int) ri10;
        ri10 = ri10 - ii10;
        if (br[0] >= 0) {
            dum1 = Math.abs(br[2] * rmax * rmax + br[3] * rmax + br[4]);
        } else {
            dum1 = -Math.abs(br[2] * rmax * rmax + br[3] * rmax + br[4]);
        }
        if (bi[0] >= 0) {
            dum2 = Math.abs(bi[2] * rmax * rmax + bi[3] * rmax + bi[4]);
        } else {
            dum2 = -Math.abs(bi[2] * rmax * rmax + bi[3] * rmax + bi[4]);
        }
        dum1 = dum1 * Math.pow(10.0, rr10);
        dum2 = dum2 * Math.pow(10.0, ri10);
        conv12(dum1, dum2, be);
        be[0][1] = be[0][1] + ir10;
        be[1][1] = be[1][1] + ii10;
        ecpdiv(ae, be, ce);
        if (Lnchf == 0) {
            conv21(ce);
        } else {
            emult(ce[0][0], ce[0][1], ce[0][0], ce[0][1], n1, e1);
            emult(ce[1][0], ce[1][1], ce[1][0], ce[1][1], n2, e2);
            eadd(n1[0], e1[0], n2[0], e2[0], n3, e3);
            n1[0] = ce[0][0];
            e1[0] = ce[0][1] - ce[1][1];
            x2 = ce[1][0];
            if (e1[0] > 74.0) {
                x1 = 1.0E75;
            } else if (e1[0] < -74.0) {
                x1 = 0;
            } else {
                x1 = n1[0] * Math.pow(10.0, e1[0]);
            }
            phi = Math.atan2(x2, x1);
            realResult[0] = 0.50 * (Math.log(n3[0]) + e3[0] * Math.log(10.0));
            imagResult[0] = phi;
        } // else
        return;
    } // arydiv

    /**
     * Converts a number represented in a 2x2 real array to the form of a real and imaginary pair.
     * 
     * @param cae
     */
    private void conv21(final double cae[][]) {
        if ( (cae[0][1] > 75) || (cae[1][1] > 75)) {
            realResult[0] = 1.0E75;
            imagResult[0] = 1.0E75;
        } else if (cae[1][1] < -75) {
            realResult[0] = cae[0][0] * Math.pow(10.0, cae[0][1]);
            imagResult[0] = 0.0;
        } else {
            realResult[0] = cae[0][0] * Math.pow(10.0, cae[0][1]);
            imagResult[0] = cae[1][0] * Math.pow(10.0, cae[1][1]);
        }
        return;
    } // conv21

    /**
     * Takes one base and exponent and multplies it by another number's base and exponent to give the product in the
     * form of base and exponent
     * 
     * @param n1
     * @param e1
     * @param n2
     * @param e2
     * @param nf
     * @param ef
     */
    private void emult(final double n1, final double e1, final double n2, final double e2, final double nf[],
            final double ef[]) {
        nf[0] = n1 * n2;
        ef[0] = e1 + e2;
        if (Math.abs(nf[0]) >= 10.0) {
            nf[0] = nf[0] / 10.0;
            ef[0] = ef[0] + 1.0;
        }
        return;
    } // emult

    /**
     * Returns the solution in the form of the base and exponent of the division of two exponential numbers.
     * 
     * @param n1
     * @param e1
     * @param n2
     * @param e2
     * @param nf
     * @param ef
     */
    private void ediv(final double n1, final double e1, final double n2, final double e2, final double nf[],
            final double ef[]) {
        nf[0] = n1 / n2;
        ef[0] = e1 - e2;
        if ( (Math.abs(nf[0]) < 1.0) && (nf[0] != 0.0)) {
            nf[0] = nf[0] * 10.0;
            ef[0] = ef[0] - 1.0;
        }
        return;
    } // ediv

    /**
     * Returns the sum of two numbers in the form of a base and an exponent.
     * 
     * @param n1
     * @param e1
     * @param n2
     * @param e2
     * @param nf
     * @param ef
     */
    private void eadd(final double n1, final double e1, final double n2, final double e2, final double nf[],
            final double ef[]) {
        double ediff;

        ediff = e1 - e2;
        if (ediff > 36.0) {
            nf[0] = n1;
            ef[0] = e1;
        } else if (ediff < -36.0) {
            nf[0] = n2;
            ef[0] = e2;
        } else {
            nf[0] = n1 * Math.pow(10.0, ediff) + n2;
            ef[0] = e2;
            while (true) {
                if (Math.abs(nf[0]) < 10.0) {
                    break;
                }
                nf[0] = nf[0] / 10.0;
                ef[0] = ef[0] + 1.0;
            } // while (true)
            while (true) {
                if ( (Math.abs(nf[0]) >= 1.0) || (nf[0] == 0.0)) {
                    return;
                }
                nf[0] = nf[0] * 10.0;
                ef[0] = ef[0] - 1.0;
            } // while (true)
        } // else
    } // eadd

    /**
     * Returns the solution to the subtraction of two numbers in the form of base and exponent.
     * 
     * @param n1
     * @param e1
     * @param n2
     * @param e2
     * @param nf
     * @param ef
     */
    private void esub(final double n1, final double e1, final double n2, final double e2, final double nf[],
            final double ef[]) {
        eadd(n1, e1, n2 * ( -1.0), e2, nf, ef);
        return;
    } // esub

    /**
     * Multiplies two numbers which are each represented in the form of a two by two array and returns the solution in
     * the same form
     * 
     * @param a
     * @param b
     * @param c
     */
    private void ecpmul(final double a[][], final double b[][], final double c[][]) {
        final double n1[] = new double[1];
        final double e1[] = new double[1];
        final double n2[] = new double[1];
        final double e2[] = new double[1];
        final double c00[] = new double[1];
        final double c01[] = new double[1];
        final double c10[] = new double[1];
        final double c11[] = new double[1];

        emult(a[0][0], a[0][1], b[0][0], b[0][1], n1, e1);
        emult(a[1][0], a[1][1], b[1][0], b[1][1], n2, e2);
        esub(n1[0], e1[0], n2[0], e2[0], c00, c01);
        emult(a[0][0], a[0][1], b[1][0], b[1][1], n1, e1);
        emult(a[1][0], a[1][1], b[0][0], b[0][1], n2, e2);
        eadd(n1[0], e1[0], n2[0], e2[0], c10, c11);
        c[0][0] = c00[0];
        c[0][1] = c01[0];
        c[1][0] = c10[0];
        c[1][1] = c11[0];
        return;
    } // ecpmul

    /**
     * Divides two numbers and returns the solution. All numbers are represented by a 2x2 array.
     * 
     * @param a
     * @param b
     * @param c
     */
    private void ecpdiv(final double a[][], final double b[][], final double c[][]) {
        final double b2[][] = new double[2][2];
        final double c2[][] = new double[2][2];
        final double n1[] = new double[1];
        final double e1[] = new double[1];
        final double n2[] = new double[1];
        final double e2[] = new double[1];
        final double n3[] = new double[1];
        final double e3[] = new double[1];
        final double c00[] = new double[1];
        final double c01[] = new double[1];
        final double c10[] = new double[1];
        final double c11[] = new double[1];

        b2[0][0] = b[0][0];
        b2[0][1] = b[0][1];
        b2[1][0] = -1.0 * b[1][0];
        b2[1][1] = b[1][1];
        ecpmul(a, b2, c2);
        emult(b[0][0], b[0][1], b[0][0], b[0][1], n1, e1);
        emult(b[1][0], b[1][1], b[1][0], b[1][1], n2, e2);
        eadd(n1[0], e1[0], n2[0], e2[0], n3, e3);
        ediv(c2[0][0], c2[0][1], n3[0], e3[0], c00, c01);
        ediv(c2[1][0], c2[1][1], n3[0], e3[0], c10, c11);
        c[0][0] = c00[0];
        c[0][1] = c01[0];
        c[1][0] = c10[0];
        c[1][1] = c11[0];
        return;
    } // ecpdiv

    /**
     * Converts a real and imaginary number pair to a form of a 2x2 real array.
     * 
     * @param realCN
     * @param imagCN
     * @param cae
     */
    private void conv12(final double realCN, final double imagCN, final double cae[][]) {
        cae[0][0] = realCN;
        cae[0][1] = 0.0;
        while (true) {
            if (Math.abs(cae[0][0]) < 10.0) {
                break;
            }
            cae[0][0] = cae[0][0] / 10.0;
            cae[0][1] = cae[0][1] + 1.0;
        } // while (true)
        while (true) {
            if ( (Math.abs(cae[0][0]) >= 1.0) || (cae[0][0] == 0.0)) {
                break;
            }
            cae[0][0] = cae[0][0] * 10.0;
            cae[0][1] = cae[0][1] - 1.0;
        } // while (true)
        cae[1][0] = imagCN;
        cae[1][1] = 0.0;
        while (true) {
            if (Math.abs(cae[1][0]) < 10.0) {
                break;
            }
            cae[1][0] = cae[1][0] / 10.0;
            cae[1][1] = cae[1][1] + 1.0;
        } // while (true)
        while (true) {
            if ( (Math.abs(cae[1][0]) >= 1.0) || (cae[1][0] == 0.0)) {
                return;
            }
            cae[1][0] = cae[1][0] * 10.0;
            cae[1][1] = cae[1][1] - 1.0;
        } // while (true)
    } // conv12

    /**
     * Takes two arrays representing one real and one imaginary part, and adds two arrays representing another complex
     * number and returns two arrays holding the complex sum. (CR, CI) = (AR+BR, AI+BI)
     * 
     * @param ar
     * @param ai
     * @param br
     * @param bi
     * @param cr
     * @param ci
     */
    private void cmpadd(final double ar[], final double ai[], final double br[], final double bi[], final double cr[],
            final double ci[]) {
        aradd(ar, br, cr);
        aradd(ai, bi, ci);
        return;
    } // cmpadd

    /**
     * Determines the number of significant figures of machine precision to arrive at the size of the array the numbers
     * must be stored in to get the accuracy of the solution.
     */
    private int bits() {
        double bit, bit2;
        int count;

        bit = 1.0;
        count = 0;
        do {
            count = count + 1;
            bit2 = store(2.0 * bit);
            bit = store(bit2 + 1.0);
        } while ( (bit - bit2) != 0.0);
        return count;
    } // bits

    /**
     * This function forces its argument x to be stored in a memory location, thus providing a means of determining
     * floating point number characteristics (such as the machine precision) when it is necessary to avoid computation
     * in high precision registers.
     * 
     * @param x The value to be stored
     * @return The value of x after it has been stored and possibly truncated or rounded to the double precision word
     *         length.
     */
    private double store(final double x) {
        double y;
        double storeVar;
        y = x;
        storeVar = y;
        return storeVar;
    }

    /**
     * Takes 2 arrays, ar and ai representing one real and one imaginary part, and multiplies it with br and bi,
     * representing a complex number and returns the complex product
     * 
     * @param ar
     * @param ai
     * @param br
     * @param bi
     * @param cr
     * @param ci
     */
    private void cmpmul(final double ar[], final double ai[], final double br, final double bi, final double cr[],
            final double ci[]) {
        final double d1[] = new double[length + 2];
        final double d2[] = new double[length + 2];

        armult(ar, br, d1);
        armult(ai, bi, d2);
        arsub(d1, d2, cr);
        armult(ar, bi, d1);
        armult(ai, br, d2);
        aradd(d1, d2, ci);
        return;
    } // cmpmul

    /**
     * Accepts array a and scalar b, and returns the product array c.
     * 
     * @param a
     * @param b
     * @param c
     */
    private void armult(final double a[], final double b, final double c[]) {
        final double z[] = new double[length + 2];
        double b2;
        double carry;
        int i;

        if (b >= 0.0) {
            z[0] = a[0];
        } else {
            z[0] = -a[0];
        }
        b2 = Math.abs(b);
        z[L + 2] = a[L + 2];
        for (i = 1; i <= L + 1; i++) {
            z[i] = 0.0;
        } // for (i = 1; i <= L+1; i++)
        if ( (b2 <= 1.0E-10) || (a[2] <= 1.0E-10)) {
            z[0] = 1.0;
            z[L + 2] = 0.0;
            for (i = 0; i <= L + 2; i++) {
                c[i] = z[i];
            }
            if (c[2] < 0.5) {
                c[0] = 1.0;
                c[L + 2] = 0.0;
            } // if (c[2] < 0.5)
            return;
        } // if (b2 <= 1.0E-10) || (a[2] <= 1.0E-10))
        for (i = L + 1; i >= 2; i--) {
            z[i] = a[i] * b2 + z[i];
            if (z[i] >= rmax) {
                carry = (int) (z[i] / rmax);
                z[i] = z[i] - carry * rmax;
                z[i - 1] = carry;
            } // if (z[i] >= rmax)
        } // for (i = L+1; i >= 2; i--)
        if (z[1] >= 0.5) {
            for (i = L + 1; i >= 2; i--) {
                z[i] = z[i - 1];
            } // for (i = L+1; i >=2; i--)
            z[L + 2] = z[L + 2] + 1.0;
            z[1] = 0.0;
        } // if (z[1] >= 0.5)

        for (i = 0; i <= L + 2; i++) {
            c[i] = z[i];
        } // for (i = 0; i <= L+2; i++)
        if (c[2] < 0.5) {
            c[0] = 1.0;
            c[L + 2] = 0.0;
        } // if (c[2] < 0.5)
        return;
    } // armult

    /**
     * Accepts two arrays of numbers, a and b, and returns the sum of the array c.
     * 
     * @param a
     * @param b
     * @param c
     */
    private void aradd(final double a[], final double b[], final double c[]) {
        final double z[] = new double[length + 2];
        int ediff;
        int i;
        int j;
        boolean seg1 = true;
        boolean seg2 = true;
        boolean seg3 = true;
        boolean seg4 = true;
        boolean seg5 = true;
        boolean seg6 = true;
        boolean seg7 = true;

        for (i = 1; i <= L + 2; i++) {
            z[i] = 0.0;
        }
        ediff = (int) Math.round(a[L + 2] - b[L + 2]);
        if ( (Math.abs(a[2]) < 0.5) || (ediff <= -L)) {
            for (i = 0; i <= L + 2; i++) {
                c[i] = b[i];
            } // for (i = 0; i <= L+2; i++)
            if (c[2] < 0.5) {
                c[0] = 1.0;
                c[L + 2] = 0.0;
            } // if (c[2] < 0.5)
            return;
        } // if ((Math.abs(a[2]) < 0.5) || (ediff <= -L))
        if ( (Math.abs(b[2]) < 0.5) || (ediff >= L)) {
            for (i = 0; i <= L + 2; i++) {
                c[i] = a[i];
            } // for (i = 0; i <= L+2; i++)
            if (c[2] < 0.5) {
                c[0] = 1.0;
                c[L + 2] = 0.0;
            } // if (c[2] < 0.5)
            return;
        } // if ((Math.abs(b[2]) < 0.5) || (ediff >= L))
        z[0] = a[0];
        if (Math.abs(a[0] - b[0]) >= 0.5) {
            if (ediff > 0) {
                z[L + 2] = a[L + 2];
                seg1 = false;
                seg2 = false;
                seg3 = false;
                seg4 = false;
            } // if (ediff > 0)
            if (seg1) {
                if (ediff < 0) {
                    z[L + 2] = b[L + 2];
                    z[0] = b[0];
                    seg2 = false;
                    seg3 = false;
                    seg4 = false;
                    seg5 = false;
                    seg6 = false;
                } // if (ediff < 0)
            } // if (seg1)
            if (seg2) {
                for (i = 2; i <= L + 1; i++) {
                    if (a[i] > b[i]) {
                        z[L + 2] = a[L + 2];
                        seg3 = false;
                        seg4 = false;
                        break;
                    } // if (a[i] > b[i])
                    if (a[i] < b[i]) {
                        z[L + 2] = b[L + 2];
                        z[0] = b[0];
                        seg3 = false;
                        seg4 = false;
                        seg5 = false;
                        seg6 = false;
                        break;
                    } // if (a[i] < b[i])
                } // for (i = 2; i <= L+1; i++)
            } // if (seg2)
            if (seg3) {
                for (i = 0; i <= L + 2; i++) {
                    c[i] = z[i];
                } // for (i = 0; i <= L+2; i++)
                if (c[2] < 0.5) {
                    c[0] = 1.0;
                    c[L + 2] = 0.0;
                } // if (c[2] < 0.5)
                return;
            } // if (seg3)
        } // if (Math.abs(a[0] - b[0]) >= 0.5)

        if (seg4) {
            if (ediff == 0) {
                z[L + 2] = a[L + 2];
                for (i = L + 1; i >= 2; i--) {
                    z[i] = a[i] + b[i] + z[i];
                    if (z[i] >= rmax) {
                        z[i] = z[i] - rmax;
                        z[i - 1] = 1.0;
                    } // if (z[i] >= rmax)
                } // for (i = L+1; i >= 2; i--)
                if (z[1] > 0.5) {
                    for (i = L + 1; i >= 2; i--) {
                        z[i] = z[i - 1];
                    } // for (i = L+1; i >= 2; i--)
                    z[L + 2] = z[L + 2] + 1.0;
                    z[1] = 0.0;
                } // if (z[1] > 0.5)
                for (i = 0; i <= L + 2; i++) {
                    c[i] = z[i];
                } // for (i = 0; i <= L+2; i++)
                if (c[2] < 0.5) {
                    c[0] = 1.0;
                    c[L + 2] = 0.0;
                } // if (c[2] < 0.5)
                return;
            } // if (ediff == 0)
            else if (ediff > 0) {
                z[L + 2] = a[L + 2];
                for (i = L + 1; i >= 2 + ediff; i--) {
                    z[i] = a[i] + b[i - ediff] + z[i];
                    if (z[i] >= rmax) {
                        z[i] = z[i] - rmax;
                        z[i - 1] = 1.0;
                    } // if (z[i] >= rmax)
                } // for (i = L+1; i >= 2+ediff; i--)
                for (i = ediff + 1; i >= 2; i--) {
                    z[i] = a[i] + z[i];
                    if (z[i] >= rmax) {
                        z[i] = z[i] - rmax;
                        z[i - 1] = 1.0;
                    } // if (z[i] >= rmax)
                } // for (i = ediff+1; i >= 2; i--)
                if (z[1] > 0.5) {
                    for (i = L + 1; i >= 2; i--) {
                        z[i] = z[i - 1];
                    } // for (i = L+1; i >= 2; i--)
                    z[L + 2] = z[L + 2] + 1.0;
                    z[1] = 0.0;
                } // if (z[1] > 0.5)
                for (i = 0; i <= L + 2; i++) {
                    c[i] = z[i];
                } // for (i = 0; i <= L+2; i++)
                if (c[2] < 0.5) {
                    c[0] = 1.0;
                    c[L + 2] = 0.0;
                } // if (c[2] < 0.5)
                return;
            } // else if (ediff > 0)
            else { // ediff < 0
                z[L + 2] = b[L + 2];
                for (i = L + 1; i >= 2 - ediff; i--) {
                    z[i] = a[i + ediff] + b[i] + z[i];
                    if (z[i] >= rmax) {
                        z[i] = z[i] - rmax;
                        z[i - 1] = 1.0;
                    } // if (z[i] >= rmax)
                } // for (i = L+1; i >= 2 - ediff; i--)
                for (i = 1 - ediff; i >= 2; i--) {
                    z[i] = b[i] + z[i];
                    if (z[i] >= rmax) {
                        z[i] = z[i] - rmax;
                        z[i - 1] = 1.0;
                    } // if (z[i] >= rmax)
                } // for (i = 1-ediff; i >= 2; i--)
                if (z[1] > 0.5) {
                    for (i = L + 1; i >= 2; i--) {
                        z[i] = z[i - 1];
                    } // for (i = L+1; i >= 2; i--)
                    z[L + 2] = z[L + 2] + 1.0;
                    z[1] = 0.0;
                } // if (z[1] > 0.5)
                for (i = 0; i <= L + 2; i++) {
                    c[i] = z[i];
                } // for (i = 0; i <= L+2; i++)
                if (c[2] < 0.5) {
                    c[0] = 1.0;
                    c[L + 2] = 0.0;
                } // if (c[2] < 0.5)
                return;
            } // else ediff < 0
        } // if (seg4)

        if (seg5) {
            if (ediff <= 0) {
                for (i = L + 1; i >= 2; i--) {
                    z[i] = a[i] - b[i] + z[i];
                    if (z[i] < 0.0) {
                        z[i] = z[i] + rmax;
                        z[i - 1] = -1.0;
                    } // if (z[i] < 0.0)
                } // for (i = L+1; i >= 2; i--)
                seg6 = false;
                seg7 = false;
            } // if (ediff <= 0)
        } // if (seg5)
        if (seg6) {
            for (i = L + 1; i >= 2 + ediff; i--) {
                z[i] = a[i] - b[i - ediff] + z[i];
                if (z[i] < 0.0) {
                    z[i] = z[i] + rmax;
                    z[i - 1] = -1.0;
                } // if (z[i] < 0.0)
            } // for (i = L+1; i >= 2+ediff; i--)
            for (i = ediff + 1; i >= 2; i--) {
                z[i] = a[i] + z[i];
                if (z[i] < 0.0) {
                    z[i] = z[i] + rmax;
                    z[i - 1] = -1.0;
                } // if (z[i] < 0.0)
            } // for (i = ediff+1; i >= 2; i--)
            seg7 = false;
        } // if (seg6)

        if (seg7) {
            if (ediff >= 0) {
                for (i = L + 1; i >= 2; i--) {
                    z[i] = b[i] - a[i] + z[i];
                    if (z[i] < 0.0) {
                        z[i] = z[i] + rmax;
                        z[i - 1] = -1.0;
                    } // if (z[i] < 0.0)
                } // for (i = L+1; i >= 2; i--
            } // if (ediff >= 0)
            else { // ediff < 0
                for (i = L + 1; i >= 2 - ediff; i--) {
                    z[i] = b[i] - a[i + ediff] + z[i];
                    if (z[i] < 0.0) {
                        z[i] = z[i] + rmax;
                        z[i - 1] = -1.0;
                    } // if (z[i] < 0.0)
                } // for (i = L+1; i >= 2 - ediff; i--)
                for (i = 1 - ediff; i >= 2; i--) {
                    z[i] = b[i] + z[i];
                    if (z[i] < 0.0) {
                        z[i] = z[i] + rmax;
                        z[i - 1] = -1.0;
                    } // if (z[i] < 0.0)
                } // for (i = 1-ediff; i >= 2; i--)
            } // else ediff < 0
        } // if (seg7)

        if (z[2] <= 0.5) {
            i = 1;
            do {
                i = i + 1;
            } while ( (z[i + 1] < 0.5) && (i < (L + 1)));
            if (i == (L + 1)) {
                z[0] = 1.0;
                z[L + 2] = 0.0;
                for (i = 0; i <= L + 2; i++) {
                    c[i] = z[i];
                } // for (i = 0; i <= L+2; i++)
                if (c[2] < 0.5) {
                    c[0] = 1.0;
                    c[L + 2] = 0.0;
                } // if (c[2] < 0.5)
                return;
            } // if (i == (L+2))
            for (j = 2; j <= L + 2 - i; j++) {
                z[j] = z[j + i - 1];
            } // for (j = 2; j <= L+2-i; j++)
            for (j = L + 3 - i; j <= L + 1; j++) {
                z[j] = 0.0;
            } // for (j = L+3-i; j <= L+1; j++)
            z[L + 2] = z[L + 2] - i + 1;
        } // if (z[2] <= 0.5)
        for (i = 0; i <= L + 2; i++) {
            c[i] = z[i];
        } // for (i = 0; i <= L+2; i++)
        if (c[2] < 0.5) {
            c[0] = 1.0;
            c[L + 2] = 0.0;
        } // if (c[2] < 0.5)
        return;
    } // aradd

    /**
     * Accepts two arrays and subtracts each element in the second array b from the element in the first array a and
     * returns the solution c
     * 
     * @param a
     * @param b
     * @param c
     */
    private void arsub(final double a[], final double b[], final double c[]) {
        int i;
        final double b2[] = new double[length + 2];

        for (i = 0; i <= L + 2; i++) {
            b2[i] = b[i];
        }
        b2[0] = -1.0 * b2[0];
        aradd(a, b2, c);
        return;
    } // arsub

    /**
     * This code is a port of the FORTRAN routine CHGM from the book Computation of Special Functions by Shanjie Zhang
     * and Jianming Jin, John Wiley & Sons, Inc., 1996, pp. 398-400. It computes confluent hypergeometric functions of
     * the first kind for real parameters and argument. It works well except for the case of a << -1 when x > 0 and a >>
     * 1 when x < 0. In this exceptional case, the evaluation involves the summation of a partially alternating series
     * which results in a loss of significant digits.
     */
    private void firstKindRealArgument() {
        double a0;
        double a1;
        double x0;
        int m;
        double r;
        int k;
        int nL;
        int L = 0;
        double rg;
        int j;
        double xg;
        double sum1;
        double sum2;
        double r1;
        double r2;
        double hg1;
        double hg2;
        int L0;
        double y0 = 0.0;
        double y1 = 0.0;
        final double ta[] = new double[1];
        final double tb[] = new double[1];
        final double tba[] = new double[1];
        int i;

        a0 = a;
        a1 = a;
        x0 = x;
        result[0] = 0.0;
        if ( (b == 0.0) || (b == -Math.abs((int) b))) {
            result[0] = Double.POSITIVE_INFINITY;
        } else if ( (a == 0.0) || (x == 0.0)) {
            result[0] = 1.0;
        } else if (a == -1.0) {
            result[0] = 1.0 - x / b;
        } else if (a == b) {
            result[0] = Math.exp(x);
        } else if ( (a - b) == 1.0) {
            result[0] = (1.0 + x / b) * Math.exp(x);
        } else if ( (a == 1.0) && (b == 2.0)) {
            result[0] = (Math.exp(x) - 1.0) / x;
        } else if ( (a == (int) a) && (a < 0.0)) {
            m = (int) ( -a);
            r = 1.0;
            result[0] = 1.0;
            for (k = 1; k <= m; k++) {
                r = r * (a + k - 1.0) / k / (b + k - 1.0) * x;
                result[0] = result[0] + r;
            }
        } // else if ((a == (int)a) && (a < 0.0))
        if (result[0] != 0.0) {
            return;
        }
        if (x < 0.0) {
            // 1F1(a, b, z) = exp(z)1F1(b-a, b, -z);
            a = b - a;
            a0 = a;
            x = Math.abs(x);
        } // if (x < 0.0)
        if (a < 2.0) {
            nL = 0;
        } else {
            // (2a - b + z)1F1(a, b, z) = a*1F1(a+1, b, z) - (b - a)*1F1(a-1, b, z)
            nL = 1;
            L = (int) a;
            a = a - L - 1.0;
        } // else (a >= 2.0)
        for (L0 = 0; L0 <= nL; L0++) {
            if (a0 >= 2.0) {
                a = a + 1.0;
            }
            if ( (x <= 30.0 + Math.abs(b)) || (a < 0.0)) {
                // 1F1(a, b, z) = sum from k = 0 to infinity of
                // Pochhammer(a,k)*(z**k)/((k!)*Pochhammer(b,k))
                result[0] = 1.0;
                rg = 1.0;
                for (j = 1; j <= 500; j++) {
                    rg = rg * (a + j - 1.0) / (j * (b + j - 1.0)) * x;
                    result[0] = result[0] + rg;
                    if (Math.abs(rg / result[0]) < 1.0E-15) {
                        break;
                    }
                } // for (j = 1; j <= 500; j++)
            } // if ((x <= 30.0 + Math.abs(b)) || (a < 0.0))
            else {
                Gamma gam = new Gamma(a, ta);
                gam.run();
                gam = new Gamma(b, tb);
                gam.run();
                xg = b - a;
                gam = new Gamma(xg, tba);
                gam.run();
                sum1 = 1.0;
                sum2 = 1.0;
                r1 = 1.0;
                r2 = 1.0;
                for (i = 1; i <= 8; i++) {
                    r1 = -r1 * (a + i - 1.0) * (a - b + i) / (x * i);
                    r2 = -r2 * (b - a + i - 1.0) * (a - i) / (x * i);
                    sum1 = sum1 + r1;
                    sum2 = sum2 + r2;
                } // for (i = 1; i <= 8; i++)
                hg1 = tb[0] / tba[0] * Math.pow(x, -a) * Math.cos(Math.PI * a) * sum1;
                hg2 = tb[0] / ta[0] * Math.exp(x) * Math.pow(x, a - b) * sum2;
                result[0] = hg1 + hg2;
            } // else
            if (L0 == 0) {
                y0 = result[0];
            }
            if (L0 == 1) {
                y1 = result[0];
            }
        } // for (L0 = 0; L0 <= nL; L0++)
        if (a0 >= 2.0) {
            // (2a - b + z)1F1(a, b, z) = a*1F1(a+1, b, z) - (b - a)*1F1(a-1, b, z)
            for (i = 1; i <= L - 1; i++) {
                result[0] = ( (2.0 * a - b + x) * y1 + (b - a) * y0) / a;
                y0 = y1;
                y1 = result[0];
                a = a + 1.0;
            } // for (i = 1; i <= L-1; i++)
        } // if (a0 >= 2.0)
        if (x0 < 0.0) {
            // 1F1(a, b, z) = exp(z)1F1(b-a, b, -z);
            result[0] = result[0] * Math.exp(x0);
            a0 = a1;
        } // if (x0 < 0.0)
        a = a0;
        x = x0;
        return;
    } // firstKindRealArgument

    /**
     * This code is a port of the FORTRAN routine CCHG from the book Computation of Special Functions by Shanjie Zhang
     * and Jianming Jin, John Wiley & Sons, Inc., 1996, pp. 400-402. It computes confluent hypergeometric functions of
     * the first kind for real parameters and a complex argument. It works well except for the case of a << -1 when x >
     * 0 and a >> 1 when x < 0. In this exceptional case, the evaluation involves the summation of a partially
     * alternating series which results in a loss of significant digits.
     */
    private void firstKindComplexArgument() {
        double realCI;
        double imagCI;
        double a0;
        double a1;
        double realZ0;
        double imagZ0;
        int m;
        double realCR;
        double imagCR;
        int k;
        double x0;
        int nL;
        int La = 0;
        int n;
        int j;
        double realCHW = 0.0;
        double imagCHW = 0.0;
        final double g1[] = new double[1];
        final double g2[] = new double[1];
        double ba;
        final double g3[] = new double[1];
        double realCS1;
        double imagCS1;
        double realCS2;
        double imagCS2;
        double realCR1;
        double imagCR1;
        double realCR2;
        double imagCR2;
        int i;
        double phi;
        int ns;
        double realCFAC;
        double imagCFAC;
        double realCHG1;
        double imagCHG1;
        double realCHG2;
        double imagCHG2;
        double realCY0 = 0.0;
        double imagCY0 = 0.0;
        double realCY1 = 0.0;
        double imagCY1 = 0.0;
        double denom;
        double realCRG;
        double imagCRG;
        double realEPS;
        double imagEPS;
        Gamma gam;
        double realTemp;
        double magZ;
        double angZ;
        double realZtoPow;
        double imagZtoPow;
        double realVar;
        double imagVar;

        realCI = 0.0;
        imagCI = 1.0;
        a0 = a;
        a1 = a;
        realZ0 = realZ;
        imagZ0 = imagZ;
        if ( (b == 0.0) || (b == -(int) Math.abs(b))) {
            // For b = 0, -1, -2,...
            realResult[0] = Double.POSITIVE_INFINITY;
            imagResult[0] = 0.0;
        } else if ( (a == 0.0) || ( (realZ == 0.0) && (imagZ == 0.0))) {
            // 1F1(a, b, 0) = 1F1(0, b, z) = 1
            realResult[0] = 1.0;
            imagResult[0] = 0.0;
        } else if (a == -1.0) {
            // Special case for a = -1
            realResult[0] = 1.0 - realZ / b;
            imagResult[0] = -imagZ / b;
        } else if (a == b) {
            // Special case for a = b
            realResult[0] = Math.exp(realZ) * Math.cos(imagZ);
            imagResult[0] = Math.exp(realZ) * Math.sin(imagZ);
        } else if ( (a - b) == 1.0) {
            // Special case for a = b + 1
            realResult[0] = (1.0 + realZ / b) * Math.exp(realZ) * Math.cos(imagZ) - (imagZ / b) * Math.exp(realZ)
                    * Math.sin(imagZ);
            imagResult[0] = (1.0 + realZ / b) * Math.exp(realZ) * Math.sin(imagZ) + (imagZ / b) * Math.exp(realZ)
                    * Math.cos(imagZ);
        } else if ( (a == 1.0) && (b == 2.0)) {
            // Special case for a = 1 and b = 2
            denom = realZ * realZ + imagZ * imagZ;
            realResult[0] = ( (Math.exp(realZ) * Math.cos(imagZ) - 1.0) * realZ + (Math.exp(realZ) * Math.sin(imagZ) * imagZ))
                    / denom;
            imagResult[0] = ( (Math.exp(realZ) * Math.cos(imagZ) - 1.0) * ( -imagZ) + (Math.exp(realZ)
                    * Math.sin(imagZ) * realZ))
                    / denom;
        } else if ( (a == (int) a) && (a < 0.0)) {
            // When a is a negative integer
            // 1F1(-n, b, z) = sum from k = 0 to n of
            // (Pochhammer(-n,k)*(z**k))/(k! * Pochammer(b,k))) for n > 0
            m = (int) ( -a);
            realCR = 1.0;
            imagCR = 0.0;
            realResult[0] = 1.0;
            imagResult[0] = 0.0;
            for (k = 1; k <= m; k++) {
                realTemp = realCR * (a + k - 1.0) / k / (b + k - 1.0) * realZ - imagCR * (a + k - 1.0) / k
                        / (b + k - 1.0) * imagZ;
                imagCR = realCR * (a + k - 1.0) / k / (b + k - 1.0) * imagZ + imagCR * (a + k - 1.0) / k
                        / (b + k - 1.0) * realZ;
                realCR = realTemp;
                realResult[0] = realResult[0] + realCR;
                imagResult[0] = imagResult[0] + imagCR;
            } // for (k = 1; k <= m; k++)
        } // else if ((a == (int)a) && (a < 0.0))
        else {
            x0 = realZ;
            if (x0 < 0.0) {
                // For realZ < 0 use 1F1(a, b, z) = exp(z)*1F1(b-a, b, -z)
                a = b - a;
                a0 = a;
                realZ = -realZ;
                imagZ = -imagZ;
            } // if (x0 < 0.0)
            if (a < 2.0) {
                nL = 0;
            } else { // a >= 2.0
                // For a >= 2 use
                // (2a - b + z)*1F1(a, b, z) = a*1F1(a+1, b, z) - (b-a)*1F1(a-1, b, z)
                nL = 1;
                La = (int) a;
                a = a - La - 1.0;
            } // else (a >= 2.0)
            for (n = 0; n <= nL; n++) {
                if (a0 >= 2.0) {
                    a = a + 1.0;
                }
                if ( (Math.sqrt(realZ * realZ + imagZ * imagZ) < 20.0 + Math.abs(b)) || (a < 0.0)) {
                    // 1F1(a, b, z) = sum from k = 0 to n of
                    // (Pochhammer(a,k)*(z**k))/(k! * Pochammer(b,k))) for b not an integer
                    realResult[0] = 1.0;
                    imagResult[0] = 0.0;
                    realCRG = 1.0;
                    imagCRG = 0.0;
                    for (j = 1; j <= 500; j++) {
                        realTemp = realCRG * (a + j - 1.0) / (j * (b + j - 1.0)) * realZ - imagCRG * (a + j - 1.0)
                                / (j * (b + j - 1.0)) * imagZ;
                        imagCRG = realCRG * (a + j - 1.0) / (j * (b + j - 1.0)) * imagZ + imagCRG * (a + j - 1.0)
                                / (j * (b + j - 1.0)) * realZ;
                        realCRG = realTemp;
                        realResult[0] = realResult[0] + realCRG;
                        imagResult[0] = imagResult[0] + imagCRG;
                        realEPS = realResult[0] - realCHW;
                        imagEPS = imagResult[0] - imagCHW;
                        if (Math.sqrt( (realEPS * realEPS + imagEPS * imagEPS)
                                / (realResult[0] * realResult[0] + imagResult[0] * imagResult[0])) < 1.0E-15) {
                            break;
                        }
                        realCHW = realResult[0];
                        imagCHW = imagResult[0];
                    } // for (j = 1; j <= 500; j++)
                } // if ((Math.sqrt(realZ*realZ + imagZ*imagZ) < 20.0 + Math.abs(b)) || (a < 0.0))
                else {
                    gam = new Gamma(a, g1);
                    gam.run();
                    gam = new Gamma(b, g2);
                    gam.run();
                    ba = b - a;
                    gam = new Gamma(ba, g3);
                    gam.run();
                    realCS1 = 1.0;
                    imagCS1 = 0.0;
                    realCS2 = 1.0;
                    imagCS2 = 0.0;
                    realCR1 = 1.0;
                    imagCR1 = 0.0;
                    realCR2 = 1.0;
                    imagCR2 = 0.0;
                    for (i = 1; i <= 8; i++) {
                        realTemp = -realCR1 * (a + i - 1.0) * (a - b + i) / (realZ * i) + imagCR1 * (a + i - 1.0)
                                * (a - b + i) / (imagZ * i);
                        imagCR1 = -realCR1 * (a + i - 1.0) * (a - b + i) / (imagZ * i) - imagCR1 * (a + i - 1.0)
                                * (a - b + i) / (realZ * i);
                        realCR1 = realTemp;
                        realTemp = realCR2 * (b - a + i - 1.0) * (i - a) / (realZ * i) - imagCR2 * (b - a + i - 1.0)
                                * (i - a) / (imagZ * i);
                        imagCR2 = realCR2 * (b - a + i - 1.0) * (i - a) / (imagZ * i) + imagCR2 * (b - a + i - 1.0)
                                * (i - a) / (realZ * i);
                        realCR2 = realTemp;
                        realCS1 = realCS1 + realCR1;
                        imagCS1 = imagCS1 + imagCR1;
                        realCS2 = realCS2 + realCR2;
                        imagCS2 = imagCS2 + imagCR2;
                    } // for (i = 1; i <= 8; i++)
                    if ( (realZ == 0.0) && (imagZ >= 0.0)) {
                        phi = 0.5 * Math.PI;
                    } else if ( (realZ == 0.0) && (imagZ < 0.0)) {
                        phi = -0.5 * Math.PI;
                    } else {
                        phi = Math.atan(imagZ / realZ);
                    }
                    if ( (phi > -0.5 * Math.PI) && (phi < 1.5 * Math.PI)) {
                        ns = 1;
                    } else {
                        ns = -1;
                    }
                    if (imagZ != 0.0) {
                        realCFAC = Math.exp(ns * realCI * Math.PI * a) * Math.cos(ns * imagCI * Math.PI * a);
                        imagCFAC = Math.exp(ns * realCI * Math.PI * a) * Math.sin(ns * imagCI * Math.PI * a);
                    } else { // else imagZ == 0.0
                        realCFAC = Math.cos(Math.PI * a);
                        imagCFAC = 0.0;
                    } // else imagZ == 0.0
                    magZ = Math.sqrt(realZ * realZ + imagZ * imagZ);
                    angZ = Math.atan2(imagZ, realZ);
                    realZtoPow = Math.pow(magZ, -a) * Math.cos( -a * angZ);
                    imagZtoPow = Math.pow(magZ, -a) * Math.sin( -a * angZ);
                    realVar = g2[0] / g3[0] * realZtoPow * realCFAC - g2[0] / g3[0] * imagZtoPow * imagCFAC;
                    imagVar = g2[0] / g3[0] * realZtoPow * imagCFAC + g2[0] / g3[0] * imagZtoPow * realCFAC;
                    realCHG1 = realVar * realCS1 - imagVar * imagCS1;
                    imagCHG1 = realVar * imagCS1 + imagVar * realCS1;
                    realZtoPow = Math.pow(magZ, a - b) * Math.cos( (a - b) * angZ);
                    imagZtoPow = Math.pow(magZ, b - a) * Math.sin( (a - b) * angZ);
                    realVar = g2[0] / g1[0] * Math.exp(realZ) * Math.cos(imagZ) * realZtoPow - g2[0] / g1[0]
                            * Math.exp(realZ) * Math.sin(imagZ) * imagZtoPow;
                    imagVar = g2[0] / g1[0] * Math.exp(realZ) * Math.cos(imagZ) * imagZtoPow + g2[0] / g1[0]
                            * Math.exp(realZ) * Math.sin(imagZ) * realZtoPow;
                    realCHG2 = realVar * realCS2 - imagVar * imagCS2;
                    imagCHG2 = realVar * imagCS2 + imagVar * realCS2;
                    realResult[0] = realCHG1 + realCHG2;
                    imagResult[0] = imagCHG1 + imagCHG2;
                } // else
                if (n == 0) {
                    realCY0 = realResult[0];
                    imagCY0 = imagResult[0];
                } // if (n == 0)
                if (n == 1) {
                    realCY1 = realResult[0];
                    imagCY1 = imagResult[0];
                } // if (n == 1)
            } // for (n = 0; n <= nL; n++)
            if (a0 >= 2.0) {
                // For a0 >= 2 use
                // (2a - b + z)*1F1(a, b, z) = a*1F1(a+1, b, z) - (b-a)*1F1(a-1, b, z)
                for (i = 1; i <= La - 1; i++) {
                    realResult[0] = ( (2.0 * a - b + realZ) * realCY1 - imagZ * imagCY1 + (b - a) * realCY0) / a;
                    imagResult[0] = ( (2.0 * a - b + realZ) * imagCY1 + imagZ * realCY1 + (b - a) * imagCY0) / a;
                    realCY0 = realCY1;
                    imagCY0 = imagCY1;
                    realCY1 = realResult[0];
                    imagCY1 = imagResult[0];
                    a = a + 1.0;
                } // for (i = 1; i <= La-1; i++)
            } // if (a0 >= 2.0)
            if (x0 < 0.0) {
                realTemp = realResult[0] * Math.exp( -realZ) * Math.cos( -imagZ) - imagResult[0] * Math.exp( -realZ)
                        * Math.sin( -imagZ);
                imagResult[0] = realResult[0] * Math.exp( -realZ) * Math.sin( -imagZ) + imagResult[0]
                        * Math.exp( -realZ) * Math.cos( -imagZ);
                realResult[0] = realTemp;
            } // if (x0 < 0.0)
        } // else
        a = a1;
        realZ = realZ0;
        imagZ = imagZ0;
        return;
    } // firstKindComplexArgument

    /**
     * This code is a port of the FORTRAN routine CHGU from the book Computation of Special Functions by Shanjie Zhang
     * and Jianming Jin, John Wiley & Sons, Inc., 1996, pp. 403-405. It computes confluent hypergeometric functions of
     * the second kind for real parameters and argument. It works well except for the case of a << -1 because in this
     * case the evaluation involves a summation of a partially alternating series which results in a loss of significant
     * digits. Routines called: chgus for small x method = 1 chgul for large x method = 2 chgubi for integer b method =
     * 3 chguit for numerical integration method = 4
     */
    private void secondKindRealArgument() {
        boolean iL1;
        boolean iL2;
        boolean iL3;
        boolean bL1;
        boolean bL2;
        boolean bL3;
        boolean bn;
        double aa;
        final int id1[] = new int[1];
        double hu1 = 0.0;
        final int id[] = new int[1];
        double a00;
        double b00;

        aa = a - b + 1.0;
        iL1 = ( (a == (int) a) && (a <= 0.0));
        iL2 = ( (aa == (int) aa) && (aa <= 0.0));
        iL3 = Math.abs(a * (a - b + 1.0)) / x <= 2.0;
        bL1 = (x <= 5.0 || ( (x <= 10.0) && (a <= 2.0)));
        bL2 = ( ( (x > 5.0) && (x <= 12.5)) && ( (a >= 1.0) && (b >= a + 4.0)));
        bL3 = (x > 12.5) && (a >= 5.0) && (b >= a + 5.0);
        bn = (b == (int) b) && (b != 0.0);
        id1[0] = -100;
        if (b != (int) b) {
            chgus(id1);
            method[0] = 1;
            if (id1[0] >= 6) {
                return;
            }
            hu1 = result[0];
        } // if (b != (int)b)
        if (iL1 || iL2 || iL3) {
            chgul(id);
            method[0] = 2;
            if (id[0] >= 6) {
                return;
            }
            if (id1[0] > id[0]) {
                method[0] = 1;
                id[0] = id1[0];
                result[0] = hu1;
            } // if (id1[0] > id[0])
        } // if (iL1 || iL2 || iL3)
        if (a >= 0.0) {
            if (bn && (bL1 || bL2 || bL3)) {
                chgubi(id);
                method[0] = 3;
            } else {
                chguit(id);
                method[0] = 4;
            }
        } // if (a >= 0.0)
        else { // a < 0.0
            if (b <= a) {
                a00 = a;
                b00 = b;
                a = a - b + 1.0;
                b = 2.0 - b;
                chguit(id);
                result[0] = Math.pow(x, 1.0 - b00) * result[0];
                a = a00;
                b = b00;
                method[0] = 4;
            } // if (b <= a)
            else if (bn && ( !iL1)) {
                chgubi(id);
                method[0] = 3;
            }
        } // else a < 0.0
        if (id[0] < 6) {
            Preferences.debug("No accurate result obtained for confluent hypergeometric function" + "\n", 
            		Preferences.DEBUG_ALGORITHM);
            MipavUtil.displayError("No accurate result obtained");
        }
        return;
    } // secondKindRealArgument

    /*
     * This code is a port of the FORTRAN routine CHGUS from the book Computation of Special Functions by Shanjie Zhang
     * and Jianming Jin, John Wiley & Sons, Inc., 1996, pp. 405-406. It computes confluent hypergeometric functions of
     * the second kind for small argument x. Parameter b is not equal to 0, -1, -2, ... @param id Estimated number of
     * significant digits
     */
    private void chgus(final int id[]) {
        final double ga[] = new double[1];
        final double gb[] = new double[1];
        double xg1;
        final double gab[] = new double[1];
        double xg2;
        final double gb2[] = new double[1];
        double hu0;
        double r1;
        double r2;
        double hmax;
        double hmin;
        int j;
        double h0 = 0.0;
        double d1;
        double d2 = 0.0;
        Gamma gam;
        double hua;

        id[0] = -100;
        // For b != integer use
        // U(a, b, z) = (PI/sin(PI*b))* [(M(a, b, z)/(gamma(1 + a - b)*gamma(b)) - z**(1-b)
        // * (M(a+1-b,2-b,z)/(gamma(a)*gamma(2-b))]
        gam = new Gamma(a, ga);
        gam.run();
        gam = new Gamma(b, gb);
        gam.run();
        xg1 = 1.0 + a - b;
        gam = new Gamma(xg1, gab);
        gam.run();
        xg2 = 2.0 - b;
        gam = new Gamma(xg2, gb2);
        gam.run();
        hu0 = Math.PI / Math.sin(Math.PI * b);
        r1 = hu0 / (gab[0] * gb[0]);
        r2 = hu0 * Math.pow(x, 1.0 - b) / (ga[0] * gb2[0]);
        result[0] = r1 - r2;
        hmax = 0.0;
        hmin = Double.MAX_VALUE;
        for (j = 1; j <= 150; j++) {
            r1 = r1 * (a + j - 1.0) / (j * (b + j - 1.0)) * x;
            r2 = r2 * (a - b + j) / (j * (1.0 - b + j)) * x;
            result[0] = result[0] + r1 - r2;
            hua = Math.abs(result[0]);
            if (hua > hmax) {
                hmax = hua;
            }
            if (hua < hmin) {
                hmin = hua;
            }
            if (Math.abs(result[0] - h0) < Math.abs(result[0]) * 1.0E-15) {
                break;
            }
            h0 = result[0];
        } // for (j = 1; j <= 150; j++)
        d1 = 0.4342944819 * Math.log(hmax);
        if (hmin != 0.0) {
            d2 = 0.4342944819 * Math.log(hmin);
        }
        id[0] = (int) (15 - Math.abs(d1 - d2));
        return;
    } // chgus

    /*
     * This code is a port of the FORTRAN routine CHGUL from the book Computation of Special Functions by Shanjie Zhang
     * and Jianming Jin, John Wiley & Sons, Inc., 1996, pp. 406-407. It computes confluent hypergeometric functions of
     * the second kind for a large argument x. @param id Estimated number of significant digits
     */
    private void chgul(final int id[]) {
        boolean iL1;
        boolean iL2;
        double aa;
        double r;
        int k;
        int nm = 0;
        double ra = 0.0;
        double r0 = 0.0;

        id[0] = -100;
        aa = a - b + 1.0;
        iL1 = (a == (int) a) && (a <= 0.0);
        iL2 = (aa == (int) aa) && (aa <= 0.0);
        if (iL1) {
            nm = (int) Math.abs(a);
        }
        if (iL2) {
            nm = (int) Math.abs(aa);
        }
        if (iL1 || iL2) {
            // For a = -m or a-b+1 = -m, m = 0, 1, 2,...
            // U(a, b, z) = z**(-a)*sum from k = 0 to R-1 of
            // Pochhammer(a,k)*Pochhammer(1+a-b,k)*(-z**-k)/k!
            result[0] = 1.0;
            r = 1.0;
            for (k = 1; k <= nm; k++) {
                r = -r * (a + k - 1.0) * (a - b + k) / (k * x);
                result[0] = result[0] + r;
            } // for (k = 1; k <= nm; k++)
            result[0] = Math.pow(x, -a) * result[0];
            id[0] = 10;
        } // if (iL1 || iL2)
        else {
            // For x >= 0.5a*(a-b+1) use
            // U(a, b, z) = z**(-a)*sum from k = 0 to R-1 of
            // Pochhammer(a,k)*Pochhammer(1+a-b,k)*(-z**-k)/k!
            result[0] = 1.0;
            r = 1.0;
            for (k = 1; k <= 25; k++) {
                r = -r * (a + k - 1.0) * (a - b + k) / (k * x);
                ra = Math.abs(r);
                if (k > 5 && ra >= r0 || ra < 1.0E-15) {
                    break;
                }
                r0 = ra;
                result[0] = result[0] + r;
            } // for (k = 1; k <= 25; k++)
            id[0] = (int) Math.abs(0.4342944819 * Math.log(ra));
            result[0] = Math.pow(x, -a) * result[0];
        } // else
        return;
    } // chgul

    /*
     * This code is a port of the FORTRAN routine CHGUBI from the book Computation of Special Functions by Shanjie Zhang
     * and Jianming Jin, John Wiley & Sons, Inc., 1996, pp. 407-409. It computes confluent hypergeometric functions of
     * the second kind when parameter b is an integer, b = +-1, +-2, ... @param id Estimated number of significant
     * digits
     */
    private void chgubi(final int id[]) {
        double eL;
        int n;
        double rn1;
        double rn;
        int j;
        final double ps[] = new double[1];
        final double ga[] = new double[1];
        double a0;
        double a1;
        double a2;
        final double ga1[] = new double[1];
        double ua;
        double ub;
        double hm1;
        double r;
        double hmax;
        double hmin;
        int k;
        double h0 = 0.0;
        double da1;
        double da2 = 0.0;
        double s0;
        int m;
        double hm2;
        double s1;
        double s2;
        double hw;
        double hu2;
        double db1;
        double db2 = 0.0;
        int id1;
        double hm3;
        double sa;
        double sb;
        Psi psi;
        Gamma gam;
        double hu1;
        int id2 = 0;

        id[0] = -100;
        eL = 0.5772156649015329;
        n = (int) Math.abs(b - 1.0);
        rn1 = 1.0;
        rn = 1.0;
        for (j = 1; j <= n; j++) {
            rn = rn * j;
            if (j == (n - 1)) {
                rn1 = rn;
            }
        } // for (j = 1; j <= n; j++)
        psi = new Psi(a, ps);
        psi.run();
        gam = new Gamma(a, ga);
        gam.run();
        if (b > 0.0) {
            a0 = a;
            a1 = a - n;
            a2 = a1;
            gam = new Gamma(a1, ga1);
            gam.run();
            ua = Math.pow( -1, n - 1) / (rn * ga1[0]);
            ub = rn1 / ga[0] * Math.pow(x, -n);
        } // if (b > 0.0)
        else {
            a0 = a + n;
            a1 = a0;
            a2 = a;
            gam = new Gamma(a1, ga1);
            gam.run();
            ua = Math.pow( -1, n - 1) / (rn * ga[0]) * Math.pow(x, n);
            ub = rn1 / ga1[0];
        } // else
        hm1 = 1.0;
        r = 1.0;
        hmax = 0.0;
        hmin = Double.MAX_VALUE;
        for (k = 1; k <= 150; k++) {
            r = r * (a0 + k - 1.0) * x / ( (n + k) * k);
            hm1 = hm1 + r;
            hu1 = Math.abs(hm1);
            if (hu1 > hmax) {
                hmax = hu1;
            }
            if (hu1 < hmin) {
                hmin = hu1;
            }
            if (Math.abs(hm1 - h0) < Math.abs(hm1) * 1.0E-15) {
                break;
            }
            h0 = hm1;
        } // for (k = 1; k <= 150; k++)
        da1 = 0.4342944819 * Math.log(hmax);
        if (hmin != 0.0) {
            da2 = 0.4342944819 * Math.log(hmin);
        }
        id[0] = (int) (15 - Math.abs(da1 - da2));
        hm1 = hm1 * Math.log(x);
        s0 = 0.0;
        for (m = 1; m <= n; m++) {
            if (b >= 0.0) {
                s0 = s0 - 1.0 / m;
            } else {
                s0 = s0 + (1.0 - a) / (m * (a + m - 1.0));
            }
        } // for (m = 1; m <= n; m++)
        hm2 = ps[0] + 2.0 * eL + s0;
        r = 1.0;
        hmax = 0.0;
        hmin = Double.MAX_VALUE;
        for (k = 1; k <= 150; k++) {
            s1 = 0.0;
            s2 = 0.0;
            if (b > 0.0) {
                for (m = 1; m <= k; m++) {
                    s1 = s1 - (m + 2.0 * a - 2.0) / (m * (m + a - 1.0));
                }
                for (m = 1; m <= n; m++) {
                    s2 = s2 + 1.0 / (k + m);
                }
            } // if (b > 0.0)
            else {
                for (m = 1; m <= k + n; m++) {
                    s1 = s1 + (1.0 - a) / (m * (m + a - 1.0));
                }
                for (m = 1; m <= k; m++) {
                    s2 = s2 + 1.0 / m;
                }
            } // else
            hw = 2.0 * eL + ps[0] + s1 - s2;
            r = r * (a0 + k - 1.0) * x / ( (n + k) * k);
            hm2 = hm2 + r * hw;
            hu2 = Math.abs(hm2);
            if (hu2 > hmax) {
                hmax = hu2;
            }
            if (hu2 < hmin) {
                hmin = hu2;
            }
            if (Math.abs( (hm2 - h0) / hm2) < 1.0E-15) {
                break;
            }
            h0 = hm2;
        } // for (k = 1; k <= 150; k++)
        db1 = 0.4342944819 * Math.log(hmax);
        if (hmin != 0.0) {
            db2 = 0.4342944819 * Math.log(hmin);
        }
        id1 = (int) (15 - Math.abs(db1 - db2));
        if (id1 < id[0]) {
            id[0] = id1;
        }
        hm3 = 1.0;
        if (n == 0) {
            hm3 = 0.0;
        }
        r = 1.0;
        for (k = 1; k <= n - 1; k++) {
            r = r * (a2 + k - 1.0) / ( (k - n) * k) * x;
            hm3 = hm3 + r;
        } // for (k = 1; k <= n-1; k++)
        sa = ua * (hm1 + hm2);
        sb = ub * hm3;
        result[0] = sa + sb;
        if (sa != 0.0) {
            id1 = (int) (0.4342944819 * Math.log(Math.abs(sa)));
        }
        if (result[0] != 0.0) {
            id2 = (int) (0.4342944819 * Math.log(Math.abs(result[0])));
        }
        if (sa * sb < 0.0) {
            id[0] = (id[0] - Math.abs(id1 - id2));
        }
        return;
    } // chgubi

    /*
     * This code is a port of the FORTRAN routine CHGUIT from the book Computation of Special Functions by Shanjie Zhang
     * and Jianming Jin, John Wiley & Sons, Inc., 1996, pp. 409-411. It computes confluent hypergeometric functions of
     * the second kind using Gaussian-Legendre integration Parameter a >= 0 Parameter x > 0 @param id Estimated number
     * of significant digits
     */
    private void chguit(final int id[]) {
        final double t[] = new double[] {2.59597723012478E-2, 7.78093339495366E-2, 1.29449135396945E-1,
                1.80739964873425E-1, 2.31543551376029E-1, 2.81722937423262E-1, 3.31142848268448E-1,
                3.79670056576798E-1, 4.27173741583078E-1, 4.73525841761707E-1, 5.18601400058570E-1,
                5.62278900753945E-1, 6.04440597048510E-1, 6.44972828489477E-1, 6.83766327381356E-1,
                7.20716513355730E-1, 7.55723775306586E-1, 7.88693739932264E-1, 8.19537526162146E-1,
                8.48171984785930E-1, 8.74519922646898E-1, 8.98510310810046E-1, 9.20078476177628E-1,
                9.39166276116423E-1, 9.55722255839996E-1, 9.69701788765053E-1, 9.81067201752598E-1,
                9.89787895222222E-1, 9.95840525118838E-1, 9.99210123227436E-1};
        final double w[] = new double[] {5.19078776312206E-2, 5.17679431749102E-2, 5.14884515009810E-2,
                5.10701560698557E-2, 5.05141845325094E-2, 4.98220356905502E-2, 4.89955754557568E-2,
                4.80370318199712E-2, 4.69489888489122E-2, 4.57343797161145E-2, 4.43964787957872E-2,
                4.29388928359356E-2, 4.13655512355848E-2, 3.96806954523808E-2, 3.78888675692434E-2,
                3.59948980510845E-2, 3.40038927249464E-2, 3.19212190192963E-2, 2.97524915007890E-2,
                2.75035567499248E-2, 2.51804776215213E-2, 2.27895169439978E-2, 2.03371207294572E-2,
                1.78299010142074E-2, 1.52746185967848E-2, 1.26781664768159E-2, 1.00475571822880E-2,
                7.38993116334531E-3, 4.71272992695363E-3, 2.02681196887362E-3};
        double a1;
        double b1;
        double c;
        int m;
        double hu1 = 0.0;
        double g;
        int j;
        double s;
        int k;
        double t1;
        double t2;
        double f1;
        double f2;
        double d;
        double hu0 = 0.0;
        final double ga[] = new double[1];
        Gamma gam;
        double hu2 = 0.0;
        double t3;
        double t4;

        id[0] = 7;
        a1 = a - 1.0;
        b1 = b - a - 1.0;
        c = 12.0 / x;
        for (m = 10; m <= 100; m += 5) {
            hu1 = 0.0;
            g = 0.5 * c / m;
            d = g;
            for (j = 1; j <= m; j++) {
                s = 0.0;
                for (k = 1; k <= 30; k++) {
                    t1 = d + g * t[k - 1];
                    t2 = d - g * t[k - 1];
                    f1 = Math.exp( -x * t1) * Math.pow(t1, a1) * Math.pow(1.0 + t1, b1);
                    f2 = Math.exp( -x * t2) * Math.pow(t2, a1) * Math.pow(1.0 + t2, b1);
                    s = s + w[k - 1] * (f1 + f2);
                } // for (k = 1; k <= 30; k++)
                hu1 = hu1 + s * g;
                d = d + 2.0 * g;
            } // for (j = 1; j <= m; j++)
            if (Math.abs(1.0 - hu0 / hu1) < 1.0E-7) {
                break;
            }
            hu0 = hu1;
        } // for (m = 10; m <= 100; m += 5)
        gam = new Gamma(a, ga);
        gam.run();
        hu1 = hu1 / ga[0];
        for (m = 2; m <= 10; m += 2) {
            hu2 = 0.0;
            g = 0.5 / m;
            d = g;
            for (j = 1; j <= m; j++) {
                s = 0.0;
                for (k = 1; k <= 30; k++) {
                    t1 = d + g * t[k - 1];
                    t2 = d - g * t[k - 1];
                    t3 = c / (1.0 - t1);
                    t4 = c / (1.0 - t2);
                    f1 = t3 * t3 / c * Math.exp( -x * t3) * Math.pow(t3, a1) * Math.pow(1.0 + t3, b1);
                    f2 = t4 * t4 / c * Math.exp( -x * t4) * Math.pow(t4, a1) * Math.pow(1.0 + t4, b1);
                    s = s + w[k - 1] * (f1 + f2);
                } // for (k = 1; k <= 30; k++)
                hu2 = hu2 + s * g;
                d = d + 2.0 * g;
            } // for (j = 1; j <= m; j++)
            if (Math.abs(1.0 - hu0 / hu2) < 1.0E-7) {
                break;
            }
            hu0 = hu2;
        } // for (m = 2; m <= 10; m += 2)
        gam = new Gamma(a, ga);
        gam.run();
        hu2 = hu2 / ga[0];
        result[0] = hu1 + hu2;
        return;
    } // chguit
}
