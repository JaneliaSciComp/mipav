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
    private int kind;

    /** Tells whether real number of complex number version */
    private int version;

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
    private final int length = 1400;

    /** Size of the arrays */
    private int L;

    /** Number of digits required to represent the numbers with the required accuracy */
    private double rmax;

    private int bit;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
    public ConfluentHypergeometric() {
    	
    }

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
    
    public void complexTestFirstKind() {
    	realResult = new double[1];
    	imagResult = new double[1];
    	int errorsDetected = 0;
        boolean errorFound;
    	double realATest[] = new double[]{0.1,1.0E-8,1.0,50.0,5.0,-5.0,5.0,-5.0,1.0,-5.0,2.0};
    	double imagATest[] = new double[]{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,0.0,8.0};
    	double realBTest[] = new double[]{0.2,1.0E-12,1.0,10.0,0.1,0.1,2.0,2.0,1.0,-5.0 + 1.0E-9,-150.0};
    	double imagBTest[] = new double[]{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,-5.0 + 1.0E-9,1.0};
    	double realZTest[] = new double[]{-0.5,-1.0E-10,10.0,0.0,-2.0,2.0,100.0,-100.0,1.0,-1.0,150.0};
    	double imagZTest[] = new double[]{1.0,1.0E-12,1.0E-9,200.0,300.0,300.0,-1000.0,1000.0,-1.0,0.0,0.0};
    	double realAnswer[] = new double[]{0.667236640109150,0.999999,2.202646579480672E4,-3.000605782805072E35,
    			7.20855632163922E10,2.897045042631838E10,7.002864442038879E50,7.196140446954445E11,
    			1.468693939915885,0.507421537454510,-9.853780031496243E135};
    	double imagAnswer[] = new double[]{0.274769720129335,1.0E-8,2.202646579480672E-5,3.046849261045972E35,
    			-1.55028911922414E10,-8.276253515853658E11,8.973775767458327E50,-1.233790613611111E12,
    			-2.287355287178842,0.298577267504408,3.293888962100131E136};
    	int i;
    	for (i = 0; i < 8; i++) {
        	errorFound = false;
        	a = realATest[i];
        	b = realBTest[i]; 
        	realZ = realZTest[i];
        	imagZ = imagZTest[i];
	        firstKindComplexArgument();
	        Preferences.debug("a = " + a + " b = " + b + " realZ = " + realZ + " imagZ = " + imagZ + " realResult = " + realResult[0] + 
	        		" imagResult = " + imagResult[0] + " realAnswer = " + realAnswer[i] + 
	        		" imagAnswer = " + imagAnswer[i] + "\n",Preferences.DEBUG_ALGORITHM);
	        if (Double.isNaN(realResult[0])) {
	        	Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
	     		errorsDetected++;	
	     		errorFound = true;
	        }
	        else if (realAnswer[i] != 0.0) {
		        if ((realResult[0]/realAnswer[i] < 1-1.0E-7) || (realResult[0]/realAnswer[i] > 1+1.0E-7)) {
		     		   Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
		     		   errorsDetected++;
		     		   errorFound = true;
		        }
	        }
	        else if (Math.abs(realResult[0]) > 1.0E-7) {
        		Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
	     		errorsDetected++;	
	     		errorFound = true;
	        }
	        if (Double.isNaN(imagResult[0])) {
	        	Preferences.debug("Error detected in imaginary part\n",Preferences.DEBUG_ALGORITHM);
	        	if (!errorFound) {
	     		    errorsDetected++;	
	        	}
	        }
	        else if (imagAnswer[i] != 0.0) {
		        if ((imagResult[0]/imagAnswer[i] < 1-1.0E-7) || (imagResult[0]/imagAnswer[i] > 1+1.0E-7)) {
		     		   Preferences.debug("Error detected in imaginary part\n",Preferences.DEBUG_ALGORITHM);
		     		   if (!errorFound) {
		     		       errorsDetected++;
		     		   }
		        }
	        }
	        else if (Math.abs(imagResult[0]) > 1.0E-7) {
	        	Preferences.debug("Error detected in imaginary part\n",Preferences.DEBUG_ALGORITHM);
	        	if (!errorFound) {
	        		errorsDetected++;
	        	}
	        }
        }
        Preferences.debug(errorsDetected + " errors detected in 8 tests on firstKindComplexArgument()\n", Preferences.DEBUG_ALGORITHM);
        System.out.println(errorsDetected + " errors detected in 8 tests on firstKindComplexArgument()");
        
        errorsDetected = 0;
        for (i = 0; i < realATest.length; i++) {
        	errorFound = false;
        	realA = realATest[i];
        	imagA = imagATest[i];
        	realB = realBTest[i]; 
        	imagB = imagBTest[i];
        	realZ = realZTest[i];
        	imagZ = imagZTest[i];
	        firstKindComplex();
	        Preferences.debug("realA = " + realA + "imagA = " + imagA + " realB = " + realB + 
	        		"imagB = " + imagB + " realZ = " + realZ + " imagZ = " + imagZ + " realResult = " + realResult[0] + 
	        		" imagResult = " + imagResult[0] + " realAnswer = " + realAnswer[i] + 
	        		" imagAnswer = " + imagAnswer[i] + "\n",Preferences.DEBUG_ALGORITHM);
	        if (Double.isNaN(realResult[0])) {
	        	Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
	     		errorsDetected++;	
	     		errorFound = true;
	        }
	        else if (realAnswer[i] != 0.0) {
		        if ((realResult[0]/realAnswer[i] < 1-1.0E-7) || (realResult[0]/realAnswer[i] > 1+1.0E-7)) {
		     		   Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
		     		   errorsDetected++;
		     		   errorFound = true;
		        }
	        }
	        else if (Math.abs(realResult[0]) > 1.0E-7) {
        		Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
	     		errorsDetected++;	
	     		errorFound = true;
	        }
	        if (Double.isNaN(imagResult[0])) {
	        	Preferences.debug("Error detected in imaginary part\n",Preferences.DEBUG_ALGORITHM);
	        	if (!errorFound) {
	     		    errorsDetected++;	
	        	}
	        }
	        else if (imagAnswer[i] != 0.0) {
		        if ((imagResult[0]/imagAnswer[i] < 1-1.0E-7) || (imagResult[0]/imagAnswer[i] > 1+1.0E-7)) {
		     		   Preferences.debug("Error detected in imaginary part\n",Preferences.DEBUG_ALGORITHM);
		     		   if (!errorFound) {
		     		       errorsDetected++;
		     		   }
		        }
	        }
	        else if (Math.abs(imagResult[0]) > 1.0E-7) {
	        	Preferences.debug("Error detected in imaginary part\n",Preferences.DEBUG_ALGORITHM);
	        	if (!errorFound) {
	        		errorsDetected++;
	        	}
	        }
        }
	        
        Preferences.debug(errorsDetected + " errors detected in " + realATest.length + " tests on firstKindComplex()\n", Preferences.DEBUG_ALGORITHM);
        System.out.println(errorsDetected + " errors detected in " + realATest.length + " tests on firstKindComplex()");
    }
    
    public void realTestFirstKind() {
    	// a = 500, b = 511, x = 10 only works for firstKindComplex()
    	// a = 1, b = 2, x = 600 works on firstKindRealArgument() and firstKindComplexArgument() but not on firstKindComplex()
    	// a = -60, b = 1, x = 10 only works for firstKindComplex()
    	// a = 60, b = 1, x = -10 only works for firstKindComplex()
    	// a = 1.0E-3, b = 2, x = 700 works on firstKindRealArgument() and firstKindComplexArgument() but not on firstKindComplex()
    	// a = 500, b = 1, x = -5 very close for firstKindComplex() but does not work at all for others
    	// a = -500, b = 1, x = 5 only works for firstKindComplex()
    	// a = 20, b = -10 + 1E-9, x = -2.5 works on firstKindRealArgument() and firstKindComplexArgument() but slight error in firstKindComplex()
    	// a = -20, b = -10 + 1.0E-12, x = 2.5 works on firstKindRealArgument() and firstKindComplexArgument() but not on firstKindComplex()
    	// a = 1, b = 1.0E-12, x = 1 slight error on firstKindRealArgument() and firstKindComplexArgument() but significant error on firstKindComplex()
    	// a = 10, b = 1.0E-12, x = 10 slight error on firstKindRealArgument() and firstKindComplexArgument() but significant error on firstKindComplex()
    	// a = 1000, b = 1, x = -1000 fails for all 3 functions
    	// a = -1000, b = 1, x = 1000 fails for all 3 functions
    	result = new double[1];
    	realResult = new double[1];
    	imagResult = new double[1];
    	imagA = 0;
    	imagB = 0;
    	imagZ = 0;
        int errorsDetected = 0;
        boolean errorFound;
    	double atest[] = new double[]{-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,
    			-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,
    			-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,
    			2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,
    			-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,
    			-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,
    			2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,
    			-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,
    			-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,
    			1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,
    			-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,
    			-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,
    			1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,
    			-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,
    			-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,
    			1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,
    			-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,
    			-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,
    			1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,
    			-5.0,-4.5,-4.0,-3.5,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-2.5,-2.0,-1.5,
    			-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,
    			3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,
    			-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,
    			-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,
    			3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,
    			-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,
    			-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,
    			3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,
    			-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,
    			-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,
    			3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,
    			-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,
    			-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,
    			3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,
    			-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,
    			-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,
    			3.5,4.0,4.5,5.0,0.1,-0.1,1.0E-8,500,8.1,1,100,-60,60,60,-60,1000,1.0E-3,500,-500,20,20,-20,4,-4,1,10,1000,-1000};
        double btest[] = new double[]{0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,
        		1.5,1.5,1.5,1.5,1.5,1.5,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,
        		3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,
        		3.5,3.5,3.5,3.5,3.5,3.5,3.5,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
        		0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.5,1.5,
        		1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,
        		2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.5,
        		3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,
        		4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,
        		1.5,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,
        		3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,
        		3.5,3.5,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
        		0.5,0.5,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,
        		1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,
        		3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,
        		3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,
        		4.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
        		0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,
        		2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,
        		3.0,3.0,3.0,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,4.0,4.0,4.0,4.0,4.0,4.0,4.0,
        		4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,
        		1.5,1.5,1.5,1.5,1.5,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,
        		3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,
        		3.5,3.5,3.5,3.5,3.5,3.5,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,0.2,0.2,1.0E-8,511,10.1,2,1.5,1,1,1,1,1,1,1,1,-10+1.0E-9,
        		10 - 1.0E-9,-10 + 1.0E-12,80,500,1.0E-12,1.0E-12,1,1};
        double xtest[] = new double[]{0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
        		0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
        		0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
        		0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
        		0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
        		0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,0.5,0.5,1.0E-10,10,100,600,2.5,10,10,-10,-10,1.0E-3,700,-5,5,-2.5,2.5,
        		2.5,200,300,1,10,-1000,1000};
        double answer[] = new double[]{1.2807585E-1,2.0153742E-1,2.7788190E-1,3.5717084E-1,4.3946667E-1,5.2483273E-1,6.1333333E-1,7.0503369E-1,
        		8.0000000E-1,8.9829940E-1,1.0,1.1051709,1.2138822,1.3262051,1.4422116,1.5619749,1.6855692,1.8130697,1.9445528,2.0800959,
        		2.2197773,5.4835408E-1,5.8829148E-1,6.2933750E-1,6.7151156E-1,7.1483333E-1,7.5932275E-1,8.0500000E-1,8.5188552E-1,9.0000000E-1,
        		9.4936442E-1,1.0,1.0519282,1.1051709,1.1597501,1.2156880,1.2730073,1.3317310,1.3918820,1.4534840,1.5165605,1.5811358,
        		6.9257986E-1,7.2050416E-1,7.4903026E-1,7.7816713E-1,8.0792381E-1,8.3830946E-1,8.6933333E-1,9.0100480E-1,9.3333333E-1,
        		9.6632850E-1,1.0,1.0343576,1.0694112,1.1051709,1.1416467,1.1788490,1.2167880,1.2554742,1.2949182,1.3351307,1.3761225,
        		7.6625415E-1,7.8785361E-1,8.0983417E-1,8.3220075E-1,8.5495833E-1,8.7811195E-1,9.0166667E-1,9.2562762E-1,9.5000000E-1,
        		9.7478903E-1,1.0,1.0256383,1.0517092,1.0782182,1.1051709,1.1325728,1.1604295,1.1887466,1.2175300,1.2467853,1.2765185,
        		8.4150138E-1,8.5645381E-1,8.7160028E-1,8.8694278E-1,9.0248333E-1,9.1822395E-1,9.3416667E-1,9.5031355E-1,9.6666667E-1,
        		9.8322811E-1,1.0,1.0169845,1.0341836,1.0515997,1.0692347,1.0870911,1.1051709,1.1234765,1.1420099,1.1607737,1.1797700,
        		8.6337751E-1,8.7635325E-1,8.8947781E-1,9.0275257E-1,9.1617893E-1,9.2975829E-1,9.4349206E-1,9.5738168E-1,9.7142857E-1,
        		9.8563419E-1,1.0,1.0145275,1.0292181,1.0440733,1.0590947,1.0742837,1.0896420,1.1051709,1.1208722,1.1367472,1.1527977,
        		8.7991726E-1,8.9138311E-1,9.0296679E-1,9.1466930E-1,9.2649167E-1,9.3843489E-1,9.5050000E-1,9.6268802E-1,9.7500000E-1,
        		9.8743697E-1,1.0,1.0126901,1.0255085,1.0384560,1.0515339,1.0647433,1.0780852,1.0915607,1.1051709,1.1189171,1.1328002,
        		9.0328601E-1,9.1259390E-1,9.2198101E-1,9.3144793E-1,9.4099524E-1,9.5062351E-1,9.6033333E-1,9.7012530E-1,9.8000000E-1,
        		9.8995803E-1,1.0,1.0101265,1.0203382,1.0306356,1.0410193,1.0514901,1.0620485,1.0726952,1.0834307,1.0942557,1.1051709,
        		-2.7195767E-1,-6.3346394E-1,-9.8095238E-1,-1.2906962,-1.5333333,-1.6730484,-1.6666667,-1.4626517,-1.0000000,-2.0702166E-1,
        		1.0,2.7182818,5.0601569,8.1548455,1.2150392E1,1.7215785E1,2.3543265E1,3.1350850E1,4.0885081E1,5.2424007E1,6.6280440E1,
        		-4.6666667E-1,-5.5579111E-1,-6.2500000E-1,-6.6545223E-1,-6.6666667E-1,-6.1631590E-1,-5.0000000E-1,-3.0099867E-1,0.0000000,
        		4.2519583E-1,1.0,1.7533877,2.7182818,3.9319711,5.4365637,7.2794797,9.5139864,1.2199777E1,1.5403597E1,1.9199925E1,2.3671704E1,
        		-3.4699375E-1,-3.5910090E-1,-3.5449735E-1,-3.2861613E-1,-2.7619048E-1,-1.9117612E-1,-6.6666667E-2,1.0519834E-1,3.3333333E-1,
        		6.2781504E-1,1.0,1.4626517,2.0300785,2.7182818,3.5451177,4.5304697,5.6964364,7.0675328,8.6709078,1.0536578E1,1.2697679E1,
        		-2.0972222E-1,-1.9077567E-1,-1.5833333E-1,-1.0966112E-1,-4.1666667E-2,4.9136336E-2,1.6666667E-1,3.1531723E-1,5.0000000E-1,
        		7.2619449E-1,1.0,1.3281918,1.7182818,2.1785835,2.7182818,3.3475086,4.0774227,4.9202970,5.8896106,7.0001484,8.2681072,
        		1.3492063E-2,5.3612239E-2,1.0277778E-1,1.6222909E-1,2.3333333E-1,3.1759492E-1,4.1666667E-1,5.3236179E-1,6.6666667E-1,
        		8.2175453E-1,1.0,1.2039947,1.4365637,1.7007833,2.0000000,2.3378502,2.7182818,3.1455769,3.6243758,4.1597027,4.7569932,
        		9.9552300E-2,1.4283991E-1,1.9369519E-1,2.5301755E-1,3.2178932E-1,4.0108211E-1,4.9206349E-1,5.9600420E-1,7.1428571E-1,
        		8.4840838E-1,1.0,1.1708250,1.3627943,1.5779752,1.8186029,2.0870918,2.3860478,2.7182818,3.0868230,3.4949338,3.9461256,
        		1.7247024E-1,2.1695522E-1,2.6785714E-1,3.2585054E-1,3.9166667E-1,4.6609750E-1,5.5000000E-1,6.4430060E-1,7.5000000E-1,
        		8.6817823E-1,1.0,1.1467204,1.3096910,1.4903659,1.6903090,1.9112007,2.1548455,2.4231801,2.7182818,3.0423774,3.3978523,
        		2.8862434E-1,3.3269843E-1,3.8154762E-1,4.3558128E-1,4.9523810E-1,5.6098784E-1,6.3333333E-1,7.1281239E-1,8.0000000E-1,
        		8.9551051E-1,1.0,1.1141688,1.2387639,1.3745820,1.5224722,1.6833391,1.8581458,2.0479177,2.2537454,2.4767890,2.7182818,
        		-1.1962963E1,-1.3052775E1,-1.0428571E1,-4.1521298,4.3333333,1.1951650E1,1.4333333E1,7.3728857,-9.0000000,-2.3308419E1,
        		1.0,1.4841316E2,5.8828914E2,1.6325448E3,3.8233794E3,8.0637816E3,1.5808146E4,2.9336334E4,5.2142360E4,8.9479000E4,1.4911275E5,
        		-3.1666667,-2.6332103,-1.2916667,6.4792079E-1,2.6666667,3.9409168,3.5000000,6.6927761E-1,-4.0000000,-7.0143797,1.0,
        		4.0078446E1,1.4841316E2,3.9377008E2,8.9047895E2,1.8242342E3,3.4877092E3,6.3309811E3,1.1032045E4,1.8594345E4,3.0480353E4,
        		-9.4805195E-1,-5.0421943E-1,1.5343915E-1,8.9006448E-1,1.4761905,1.6103779,1.0000000,-4.5787640E-1,-2.3333333,-3.0681304,
        		1.0,1.7172158E1,5.8728914E1,1.4841316E2,3.2350903E2,6.4312369E2,1.1984766E3,2.1272553E3,3.6334215E3,6.0142666E3,9.6970388E3,
        		-2.1527778E-1,5.8146851E-2,3.7500000E-1,6.5622622E-1,7.9166667E-1,6.5859920E-1,1.6666667E-1,-6.5432784E-1,-1.5000000,
        		-1.5367315,1.0,9.4185650,2.9482632E1,7.0738326E1,1.4841316E2,2.8609283E2,5.1944606E2,9.0134938E2,1.5088671E3,2.4526727E3,
        		3.8896615E3,1.0714286E-1,1.8351332E-1,2.3611111E-1,2.3923175E-1,1.6666667E-1,9.4919182E-4,-2.5000000E-1,-5.2517081E-1,
        		-6.6666667E-1,-3.5296145E-1,1.0,4.3821186,1.1393053E1,2.4527904E1,4.7572211E1,8.6141800E1,1.4841316E2,2.4610262E2,
        		3.9576842E2,6.2052934E2,9.5231777E2,1.1014911E-1,1.4115091E-1,1.4751915E-1,1.1531307E-1,3.3189033E-2,-1.0109557E-1,
        		-2.6984127E-1,-4.1828517E-1,-4.2857143E-1,-8.1299951E-2,1.0,3.4275813,8.1593371,1.6650107E1,3.1057680E1,5.4520429E1,
        		9.1528122E1,1.4841316E2,2.3399659E2,3.6043196E2,5.4430088E2,8.8541667E-2,9.3463962E-2,7.7380952E-2,3.3431058E-2,-4.1666667E-2,
        		-1.4296953E-1,-2.5000000E-1,-3.1567200E-1,-2.5000000E-1,1.0332562E-1,1.0,2.8410480,6.2358316,1.2087471E1,2.1707495E1,
        		3.6968337E1,6.0504569E1,9.5976492E1,1.4841316E2,2.2465603E2,3.3392961E2,3.4391534E-2,1.8557340E-2,-8.9285714E-3,-4.8026323E-2,
        		-9.5238095E-2,-1.4112047E-1,-1.6666667E-1,-1.3816198E-1,0.0000000,3.3519810E-1,1.0,2.1901779,4.1886653,7.3971387,
        		1.2377331E1,1.9904693E1,3.1037659E1,4.7206524E1,7.0326872E1,1.0294363E2,1.4841316E2,1.3380423E2,1.7741394E2,1.1147619E2,
        		-4.6423680E1,-1.6959902E2,9.4333333E1,3.6098666E2,-1.9000000E1,-1.3381435E3,1.0,2.2026466E4,1.2345819E5,4.6255578E5,
        		1.4197687E6,3.8399472E6,9.4908451E6,2.1901649E7,4.7852892E7,9.9959248E7,2.0105191E8,3.4333333E1,3.3028302E1,1.1000000E1,
        		-2.2969710E1,-2.9339411E1,3.1000000E1,7.9100651E1,-9.0000000,-2.6750359E2,1.0,4.0427554E3,2.2026466E4,8.0587605E4,
        		2.4229112E5,6.4335326E5,1.5638791E6,3.5544257E6,7.6578679E6,1.5789265E7,3.1373029E7,1.1149110E1,7.6687013,-1.1164021,
        		-1.1191881E1,-6.1587671,1.4333333E1,2.6529284E1,-5.6666667,-8.4956507E1,1.0,1.1682305E3,6.1728596E3,2.2026466E4,
        		6.4815525E4,1.6886957E5,4.0355382E5,9.0308510E5,1.9181024E6,3.9028800E6,7.6599511E6,3.7777778,1.4234903,-2.3333333,
        		-5.5998012,-6.3697006E-1,7.6666667,1.0844006E1,-4.0000000,-3.4660424E1,1.0,4.3102590E2,2.2025466E3,7.6544850E3,2.2026466E4,
        		5.6276565E4,1.3215879E5,2.9110724E5,6.0939889E5,1.2234839E6,2.3715162E6,2.0634921E-1,-5.3445873E-1,-1.2222222,-1.4046583,
        		-6.6666667E-1,9.9256622E-1,2.6666667,2.2961952,-2.3333333,-9.1008860,1.0,9.3137265E1,4.4030932E2,1.4446918E3,3.9647838E3,
        		9.7244160E3,2.2026466E4,4.6966135E4,9.5448018E4,1.8647534E5,3.5242345E5,-1.1370111E-1,-4.9487194E-1,-7.4936175E-1,
        		-6.5611179E-1,-6.7821068E-2,8.9601362E-1,1.6349206,1.0370601,-1.8571429,-5.4065191,1.0,5.1175229E1,2.3119473E2,7.3518931E2,
        		1.9676552E3,4.7244326E3,1.0503586E4,2.2026466E4,4.4092884E4,8.4959225E4,1.5852376E5,-1.9047619E-1,-3.7041941E-1,-4.2857143E-1,
        		-2.6105986E-1,1.6666667E-1,7.1916735E-1,1.0000000,3.9108871E-1,-1.5000000,-3.4191244,1.0,3.0671445E1,1.3179279E2,
        		4.0546636E2,1.0573424E3,2.4839173E3,5.4185046E3,1.1172516E4,2.2026466E4,4.1852761E4,7.7092630E4,-1.3756614E-1,-1.5115653E-1,
        		-9.5238095E-2,4.3743819E-2,2.3809524E-1,3.9209089E-1,3.3333333E-1,-1.3123146E-1,-1.0000000,-1.5240852,1.0,1.3636228E1,
        		5.2317118E1,1.4991797E2,3.7021983E2,8.3138036E2,1.7444649E3,3.4754394E3,6.6431845E3,1.2272098E4,2.2026466E4,-2.3987360E4,
        		-4.0856651E4,1.0355286E4,7.6683959E4,-2.7856667E3,-2.2717797E5,4.5433333E2,1.2256115E6,-3.9000000E1,-1.3178822E7,
        		1.0,4.8516520E8,3.8457369E9,1.9891773E10,8.2683344E10,2.9805315E11,9.6864499E11,2.9050075E12,8.1645796E12,2.1739582E13,
        		5.5283920E13,-4.7656667E3,-6.2693181E3,2.4543333E3,1.1972874E4,-7.9233333E2,-3.2989302E4,1.6100000E2,1.6797807E5,
        		-1.9000000E1,-1.7367839E6,1.0,6.2020286E7,4.8516520E8,2.4790747E9,1.0188469E10,3.6339088E10,1.1692481E11,3.4736780E11,
        		9.6758112E11,2.5544900E12,6.4434790E12,-1.4001544E3,-1.4410014E3,8.5856614E2,2.9385153E3,-3.2852381E2,-7.5965482E3,
        		8.1000000E1,3.6319736E4,-1.2333333E1,-3.6011084E5,1.0,1.2458600E7,9.6143424E7,4.8516520E8,1.9709402E9,6.9540345E9,
        		2.2149041E10,6.5173858E10,1.7989837E11,4.7086437E11,1.1779835E12,-4.9344444E2,-3.93604523E2,3.6100000E2,9.1210961E2,
        		-1.6233333E2,-2.2481088E3,4.7666667E1,1.0048369E4,-9.0000000,-9.5238098E4,1.0,3.1878535E6,2.4258260E7,1.2085272E8,
        		4.8516520E8,1.6930007E9,5.3368171E9,1.5551436E10,4.2532815E10,1.1035611E11,2.7379489E11,-7.9952381E1,-3.0713490E1,
        		8.5444444E1,1.3057141E2,-5.2333333E1,-3.1602184E2,2.1000000E1,1.2296477E3,-5.6666667,-1.0528647E4,1.0,3.2830916E5,
        		2.4258259E6,1.1766487E7,4.6090694E7,1.5721480E8,4.8516520E8,1.3858435E9,3.7195998E9,9.4804676E9,2.3126208E10,-3.4083435E1,
        		-5.1321867,4.5275724E1,5.5295786E1,-3.2304473E1,-1.3982419E2,1.4968254E1,5.1048138E2,-4.7142857,-4.1282518E3,1.0,1.2389195E5,
        		9.0134446E5,4.3114489E6,1.6674875E7,5.6214025E7,1.7159348E8,4.8516520E8,1.2897302E9,3.2575377E9,7.8781484E9,-1.4476190E1,
        		2.2302452,2.4809524E1,2.4192736E1,-2.0666667E1,-6.6988988E1,1.1000000E1,2.3185044E2,-4.0000000,-1.7637442E3,1.0,5.0825671E4,
        		3.6387373E5,1.7157266E6,6.5497302E6,2.1817246E7,6.5861175E7,1.8429431E8,4.8516520E8,1.2141936E9,2.9109912E9,-2.0687831,
        		3.3749673,7.8571429,4.3924981,-9.0952381,-1.8236345E1,6.3333333,5.9767885E1,-3.0000000,-3.9911892E2,1.0,1.0517883E4,
        		7.2774546E4,3.3298019E5,1.2371713E6,4.0203040E6,1.1862289E7,3.2495412E7,8.3860804E7,2.0597986E8,4.8516520E8,-3.3801329E5,
        		-4.3614457E7,7.2789571E4,1.8132965E8,-1.0979000E4,-1.1158455E9,1.0810000E3,1.0717690E10,-5.9000000E1,-1.8785807E11,1.0,
        		1.0686475E13,1.0374568E14,6.5187495E14,3.2679889E15,1.4116833E16,5.4661005E16,1.9429079E17,6.4409857E17,2.0140753E18,
        		5.9905957E18,-7.4399000E4,-5.2158323E6,1.8331000E4,2.0761431E7,-3.2390000E3,-1.2379436E8,3.9100000E2,1.1593598E9,
        		-2.9000000E1,-1.9889246E10,1.0,1.1103197E12,1.0686475E13,6.6599293E13,3.3128071E14,1.4204148E15,5.4607885E15,1.9277682E16,
        		6.3488345E16,1.9727015E17,5.8317160E17,-2.4504195E4,-9.8728412E5,6.8467143E3,3.7490684E6,-1.3961429E3,-2.1619586E7,
        		2.0100000E2,1.9722560E8,-1.9000000E1,-3.3095960E9,1.0,1.8123888E11,1.7290947E12,1.0686475E13,5.2737387E13,2.2441597E14,
        		8.5655027E14,3.0028994E15,9.8239594E15,3.0329742E16,8.9108286E16,-9.8240000E3,-2.3986232E5,3.0910000E3,8.6590879E5,
        		-7.1900000E2,-4.8185262E6,1.2100000E2,4.2771805E7,-1.4000000E1,-7.0162020E8,1.0,3.7673632E10,3.5621582E11,2.1829658E12,
        		1.0686475E13,4.5127184E13,1.7098359E14,5.9524223E14,1.9342519E15,5.9330821E15,1.7322775E16,-2.1918571E3,-2.2768202E4,
        		8.6100000E2,7.3718074E4,-2.5400000E2,-3.7896233E5,5.6000000E1,3.1726888E6,-9.0000000,-4.9626133E7,1.0,2.5583501E9,
        		2.3747721E10,1.4301948E11,6.8868392E11,2.8629479E12,1.0686475E13,3.6674337E13,1.1755122E14,3.5585599E14,1.0259016E15,
        		-1.1487502E3,-8.3228550E3,5.0137962E2,2.5502120E4,-1.6497403E2,-1.2543753E5,4.1000000E1,1.0175577E6,-7.5714286,-1.5523612E7,
        		1.0,7.8356373E8,7.2045611E9,4.3002863E10,2.0532999E11,8.4676773E11,3.1366858E12,1.0686475E13,3.4014984E13,1.0228483E14,
        		2.9298715E14,-6.3114286E2,-3.3181041E3,3.0528571E2,9.6486276E3,-1.1150000E2,-4.5268041E4,3.1000000E1,3.5516511E5,
        		-6.5000000,-5.2798822E6,1.0,2.6079763E8,2.3747721E9,1.4046113E10,6.6493620E10,2.7199284E11,9.9977907E11,3.3811389E12,
        		1.0686475E13,3.1918166E13,9.0835034E13,-2.1114286E2,-6.4616204E2,1.2485714E2,1.7288976E3,-5.5571429E1,-7.3222225E3,
        		1.9000000E1,5.3391087E4,-5.0000000,-7.5133965E5,1.0,3.5477001E7,3.1663628E8,1.8380420E9,8.5491797E9,3.4392897E10,
        		1.2443806E11,4.1455281E11,1.2915594E12,3.8049369E12,1.0686475E13,1.317627178278510,0.695536565102261,1.000000000100000,
        		1.779668553337393E4,1.724131075992688E41,6.288367168216566E257,2.748892975858683E12,-10.048954112964948,1.818086887618945E22,
        		-6.713066845459067E-4,1.233142540998589E18,2.279929853828663,1.461353307199289E298,0.001053895943365,0.251406264291805,
        		8.857934344815256E9,98.353133058093164,-1.051351454763442E14,3.448551506216654E27,0.024906201315854,2.718281828457880E12,
        		1.332534440778499E23,1.805334147110282E-53,2.593820783362006E215};
        int i;
        for (i = 0; i < atest.length; i++) {
        	a = atest[i];
        	b = btest[i]; 
        	x = xtest[i];
	        firstKindRealArgument();
	        Preferences.debug("a = " + a + " b = " + b + " x = " + x + " result = " + result[0] + " answer = " + answer[i] + "\n",Preferences.DEBUG_ALGORITHM);
	        if (Double.isNaN(result[0])) {
	        	Preferences.debug("Error detected\n",Preferences.DEBUG_ALGORITHM);
	     		errorsDetected++;	
	        }
	        else if (answer[i] != 0.0) {
		        if ((result[0]/answer[i] < 1-1.0E-7) || (result[0]/answer[i] > 1+1.0E-7)) {
		     		   Preferences.debug("Error detected\n",Preferences.DEBUG_ALGORITHM);
		     		   errorsDetected++;
		        }
	        }
	        else {
	        	if (Math.abs(result[0]) > 1.0E-7) {
	        		Preferences.debug("Error detected\n",Preferences.DEBUG_ALGORITHM);
		     		   errorsDetected++;	
	        	}
	        }
        }
        Preferences.debug(errorsDetected + " errors detected in " + atest.length + " tests on firstKindRealArgument()\n", Preferences.DEBUG_ALGORITHM);
        System.out.println(errorsDetected + " errors detected in " + atest.length + " tests on firstKindRealArgument()");
        
        errorsDetected = 0;
        for (i = 0; i < atest.length; i++) {
        	errorFound = false;
        	a = atest[i];
        	b = btest[i]; 
        	realZ = xtest[i];
	        firstKindComplexArgument();
	        Preferences.debug("a = " + a + " b = " + b + " realZ = " + realZ + " realResult = " + realResult[0] + 
	        		" imagResult = " + imagResult[0] + " answer = " + answer[i] + "\n",Preferences.DEBUG_ALGORITHM);
	        if (Double.isNaN(realResult[0])) {
	        	Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
	     		errorsDetected++;	
	     		errorFound = true;
	        }
	        else if (answer[i] != 0.0) {
		        if ((realResult[0]/answer[i] < 1-1.0E-7) || (realResult[0]/answer[i] > 1+1.0E-7)) {
		     		   Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
		     		   errorsDetected++;
		     		   errorFound = true;
		        }
	        }
	        else {
	        	if (Math.abs(realResult[0]) > 1.0E-7) {
	        		Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
		     		errorsDetected++;	
		     		errorFound = true;
	        	}
	        }
	        if (Math.abs(imagResult[0]) > 1.0E-7) {
	        	Preferences.debug("Error detected in imaginary part\n",Preferences.DEBUG_ALGORITHM);
	        	if (!errorFound) {
	        		errorsDetected++;
	        	}
	        }
        }
        Preferences.debug(errorsDetected + " errors detected in " + atest.length + " tests on firstKindComplexArgument()\n", Preferences.DEBUG_ALGORITHM);
        System.out.println(errorsDetected + " errors detected in " + atest.length + " tests on firstKindComplexArgument()");
        
        errorsDetected = 0;
        for (i = 0; i < atest.length; i++) {
        	errorFound = false;
        	realA = atest[i];
        	realB = btest[i]; 
        	realZ = xtest[i];
	        firstKindComplex();
	        Preferences.debug("realA = " + realA + " realB = " + realB + " realZ = " + realZ + " realResult = " + realResult[0] + 
	        		" imagResult = " + imagResult[0] + " answer = " + answer[i] + "\n",Preferences.DEBUG_ALGORITHM);
	        if (Double.isNaN(realResult[0])) {
	        	Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
	     		errorsDetected++;
	     		errorFound = true;
	        }
	        if (answer[i] != 0.0) {
		        if ((realResult[0]/answer[i] < 1-1.0E-7) || (realResult[0]/answer[i] > 1+1.0E-7)) {
		     		   Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
		     		   errorsDetected++;
		     		   errorFound = true;
		        }
	        }
	        else {
	        	if (Math.abs(realResult[0]) > 1.0E-7) {
	        		Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
		     		errorsDetected++;	
		     		errorFound = true;
	        	}
	        }
	        if (Math.abs(imagResult[0]) > 1.0E-7) {
	        	Preferences.debug("Error detected in imaginary part\n",Preferences.DEBUG_ALGORITHM);
	        	if (!errorFound) {
	        		errorsDetected++;
	        	}
	        }
        }
        Preferences.debug(errorsDetected + " errors detected in " + atest.length + " tests on firstKindComplex()\n", Preferences.DEBUG_ALGORITHM);
        System.out.println(errorsDetected + " errors detected in " + atest.length + " tests on firstKindComplex()");	
    }

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
                    denom = realZ*realZ + imagZ*imagZ;
                    for (i = 1; i <= 8; i++) {
                        realTemp = -realCR1 * (a + i - 1.0) * (a - b + i) * realZ/ (denom * i) + imagCR1 * (a + i - 1.0)
                                * (a - b + i) * imagZ/ (denom * i);
                        imagCR1 = realCR1 * (a + i - 1.0) * (a - b + i) * imagZ/ (denom * i) - imagCR1 * (a + i - 1.0)
                                * (a - b + i) * realZ / (denom * i);
                        realCR1 = realTemp;
                        realTemp = realCR2 * (b - a + i - 1.0) * (i - a) * realZ/ (denom * i) - imagCR2 * (b - a + i - 1.0)
                                * (i - a) * imagZ/ (denom * i);
                        imagCR2 = -realCR2 * (b - a + i - 1.0) * (i - a) * imagZ / (denom * i) + imagCR2 * (b - a + i - 1.0)
                                * (i - a) * realZ/ (denom * i);
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
                        phi = Math.atan2(imagZ,realZ);
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
    
    public void realTestSecondKind() {
    	result = new double[1];
    	method = new int[1];
        int errorsDetected = 0;
    	double atest[] = new double[]{-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,
    			-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,
    			-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,
    			2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,
    			-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,
    			-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,
    			2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,
    			-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,
    			-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,
    			1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,
    			-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,
    			-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,
    			1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,
    			-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,
    			-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,
    			1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,
    			-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,
    			-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,
    			1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,
    			-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,
    			-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,
    			3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,
    			-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,
    			-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,
    			3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,
    			-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,
    			-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,
    			3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,
    			-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,
    			-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,
    			3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,
    			-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,
    			-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,
    			3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,
    			-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,
    			-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,-5.0,-4.5,-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0,
    			3.5,4.0,4.5,5.0};
        double btest[] = new double[]{0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,
        		1.5,1.5,1.5,1.5,1.5,1.5,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,
        		3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,
        		3.5,3.5,3.5,3.5,3.5,3.5,3.5,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
        		0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.5,1.5,
        		1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,
        		2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.5,
        		3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,
        		4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,
        		1.5,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,
        		3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,
        		3.5,3.5,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
        		0.5,0.5,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,
        		1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,
        		3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,
        		3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,
        		4.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
        		0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,
        		2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,
        		3.0,3.0,3.0,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,4.0,4.0,4.0,4.0,4.0,4.0,4.0,
        		4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,
        		1.5,1.5,1.5,1.5,1.5,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,
        		3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,
        		3.5,3.5,3.5,3.5,3.5,3.5,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0};
        double xtest[] = new double[]{0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
        		0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
        		0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
        		0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
        		0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
        		0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
        		5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0, 
        		10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,
        		20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,
        		30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0,30.0};
        double answer[] = new double[]{-3.782240,1.398979E1,1.823600,-3.353279,-8.240000E-1,1.030903,4.600000E-1,-4.427189E-1,-4.000000E-1,
        		3.162278E-1,1.0,1.282509,1.188870,9.065557E-1,6.014612E-1,3.581785E-1,1.952781E-1,9.880872E-2,4.686629E-2,2.099707E-2,8.939207E-3,
        		-6.580249E1,1.897985E1,1.510410E1,-3.268712,-4.289000,5.586103E-1,1.610000,-4.334181E-3,-9.000000E-1,-2.407587E-1,1.0,1.847103,
        		2.014643,1.701876,1.216107,7.674829E-1,4.388221E-1,2.311686E-1,1.135429E-1,5.246088E-2,2.295828E-2,-2.249802E2,-1.196049E1,
        		4.423960E1,5.766730,-1.060400E1,-2.605717,3.260000,1.454648,-1.400000,-1.264911,1.0,3.162278,4.055651,3.759537,2.866781,
        		1.901987,1.132660,6.175235E-1,3.124606E-1,1.482042E-1,6.639858E-2,-5.517030E2,-1.731992E2,9.718010E1,4.270645E1,-2.051900E1,
        		-1.313576E1,5.410000,5.477749,-1.900000,-3.654722,1.0,6.827927,1.000000E1,9.961648,7.985357,5.506515,3.384625,1.895613,
        		9.819344E-1,4.755555E-1,2.170979E-1,-2.120583E3,-2.949083E3,3.137761E2,6.168630E2,-5.414900E1,-1.640447E2,1.121000E1,6.036359E1,
        		-2.900000,-3.659056E1,1.0,6.587168E1,1.100000E2,1.180875E2,1.000000E2,7.208390E1,4.600732E1,2.663096E1,1.420757E1,7.067241,
        		3.306408,-3.646016E3,-1.162241E4,5.008316E2,2.411368E3,-7.936400E1,-6.407533E2,1.486000E1,2.376768E2,-3.400000,-1.467297E2,
        		1.0,2.719559E2,4.641738E2,5.059644E2,4.338956E2,3.162278E2,2.038365E2,1.190609E2,6.405286E1,3.211186E1,1.513444E1,-5.913044E3,
        		-5.067279E4,7.584921E2,1.060527E4,-1.111790E2,-2.853830E3,1.901000E1,1.075914E3,-3.900000,-6.770337E2,1.0,1.280886E3,2.210000E3,
        		2.430029E3,2.100000E3,1.541295E3,1.000000E3,5.876843E2,3.179976E2,1.603009E2,7.594750E1,-1.365768E4,-1.349675E6,1.548928E3,
        		2.886672E5,-1.976090E2,-7.944628E4,2.881000E1,3.063698E4,-4.900000,-1.970738E4,1.0,3.806067E4,6.631000E4,7.355960E4,6.410000E4,
        		4.741971E4,3.100000E4,1.835137E4,1.000000E4,5.075338E3,2.420501E3,8.031250,-2.093750E1,-6.437500,3.625000,2.875000,-2.500000E-1,
        		-1.250000,-5.000000E-1,5.000000E-1,1.000000,1.0,7.578722E-1,4.842557E-1,2.736165E-1,1.404261E-1,6.659516E-2,2.953239E-2,
        		1.235426E-2,4.908040E-3,1.861554E-3,6.769968E-4,5.600000E1,-1.529334E1,-1.500000E1,4.500701E-2,4.000000,1.222718,-1.000000,
        		-9.853753E-1,0.000000,7.704036E-1,1.0,8.598866E-1,5.963474E-1,3.579321E-1,1.926947E-1,9.507096E-2,4.360788E-2,1.878763E-2,
        		7.661398E-3,2.974895E-3,1.105206E-3,1.127188E2,8.031250,-2.093750E1,-6.437500,3.625000,2.875000,-2.500000E-1,-1.250000,-5.000000E-1,
        		5.000000E-1,1.0,1.000000,7.578722E-1,4.842557E-1,2.736165E-1,1.404261E-1,6.659516E-2,2.953239E-2,1.235426E-2,4.908040E-3,
        		1.861554E-3,1.510000E2,5.261530E1,-1.900000E1,-1.509081E1,1.000000,4.324519,1.000000,-1.240720,-1.000000,1.702301E-1,1.0,
        		1.200347,1.000000,6.809206E-1,4.036526E-1,2.153257E-1,1.054790E-1,4.810188E-2,2.062369E-2,8.375500E-3,3.240573E-3,-3.400000E1,
        		1.893440E2,3.700000E1,-3.038415E1,-1.400000E1,4.369526,5.000000,-1.800281E-2,-2.000000,-8.151451E-1,1.0,1.970751,2.000000,
        		1.540807,1.000000,5.732578E-1,2.981737E-1,1.431728E-1,6.423157E-2,2.716313E-2,1.090197E-2,-4.204063E2,2.436484E2,1.090625E2,
        		-2.820313E1,-2.787500E1,1.156250,7.750000,1.562500,-2.50000,-1.625000,1.0,2.750000,3.068404,2.500000,1.689468,1.000000,
        		5.342021E-1,2.624682E-1,1.201594E-1,5.174481E-2,2.111248E-2,-1.159000E3,2.260325E2,2.250000E2,-8.152999,-4.700000E1,-6.351757,
        		1.100000E1,4.288513,-3.000000,-2.871011,1.0,4.111731,5.000000,4.281961,3.000000,1.827436,1.000000,5.016713E-1,2.339421E-1,
        		1.024281E-1,4.242763E-2,-4.364000E3,-5.159499E2,6.410000E2,1.648850E2,-1.040000E2,-4.943942E1,1.900000E1,1.723507E1,-4.000000,
        		-8.631035,1.0,1.152005E1,1.600000E1,1.481663E1,1.100000E1,7.023116,4.000000,2.078272,1.000000,4.504573E-1,1.915145E-1,
        		3.532813E2,2.026437E1,-6.843750E1,-4.332382E1,-8.125000,8.385255,1.075000E1,7.826238,4.500000,2.236068,1.0,4.117876E-1,
        		1.584297E-1,5.752803E-2,1.986208E-2,6.557534E-3,2.079596E-3,6.358052E-4,1.879694E-4,5.387320E-5,1.500116E-5,3.800000E2,
        		1.038567E2,-3.100000E1,-4.069455E1,-1.600000E1,1.487916,7.000000,6.273062,4.000000,2.126732,1.0,4.285295E-1,1.704222E-1,
        		6.366319E-2,2.253306E-2,7.605683E-3,2.460571E-3,7.660747E-4,2.302951E-4,6.703172E-5,1.893560E-5,3.079687E2,1.579922E2,
        		9.062500,-3.060618E1,-1.937500E1,-3.633610,3.750000,4.807546,3.500000,2.012461,1.0,4.472136E-1,1.841570E-1,7.085192E-2,
        		2.572732E-2,8.882593E-3,2.932618E-3,9.300237E-4,2.843407E-4,8.406245E-5,2.409283E-5,1.550000E2,1.751986E2,4.500000E1,
        		-1.585376E1,-1.900000E1,-7.097368,1.000000,3.434112,3.000000,1.892632,1.0,4.681992E-1,2.000000E-1,7.933957E-2,2.957782E-2,
        		1.045093E-2,3.522383E-3,1.138099E-3,3.539372E-4,1.062927E-4,3.091053E-5,-2.700000E2,9.599598E1,8.500000E1,1.760059E1,-1.000000E1,
        		-9.558385,-3.000000,9.844061E-1,2.000000,1.633139,1.0,5.189861E-1,2.400000E-1,1.015738E-1,4.000000E-2,1.482282E-2,5.211088E-3,
        		1.748756E-3,5.629017E-4,1.744735E-4,5.224112E-5,-4.651562E2,9.994525,8.306250E1,3.189611E1,-2.875000,-8.829673,-4.250000,
        		-7.938041E-2,1.500000,1.491457,1.0,5.500727E-1,2.655247E-1,1.162755E-1,4.710901E-2,1.788854E-2,6.425167E-3,2.197871E-3,
        		7.198296E-4,2.266749E-4,6.886908E-5,-5.950000E2,-9.336383E1,6.500000E1,4.207996E1,5.000000,-6.994106,-5.000000,-1.025711,
        		1.000000,1.340078,1.0,5.861208E-1,2.960000E-1,1.342694E-1,5.600000E-2,2.179704E-2,8.000000E-3,2.789689E-3,9.296373E-4,
        		2.974092E-4,9.168389E-5,-5.200000E2,-2.933761E2,-1.500000E1,4.444717E1,2.000000E1,-6.763466E-1,-5.000000,-2.527103,0.000000,
        		1.000927,1.0,6.783003E-1,3.776000E-1,1.843589E-1,8.160000E-2,3.339299E-2,1.280000E-2,4.638378E-3,1.600000E-3,5.281968E-4,
        		1.675907E-4,-3.951406E3,-2.085127E2,7.315625E2,6.304791E2,3.606250E2,1.699724E2,7.075000E1,2.687936E1,9.500000,3.162278,1.0,
        		3.023411E-1,8.782677E-2,2.460849E-2,6.671935E-3,1.754988E-3,4.488699E-4,1.118457E-4,2.719502E-5,6.461881E-6,1.502391E-6,
        		-4.120000E3,-9.051516E2,2.640000E2,4.088197E2,2.740000E2,1.406360E2,6.200000E1,2.459589E1,9.000000,3.084144,1.0,3.090673E-1,
        		9.156333E-2,2.611591E-2,7.196673E-3,1.921581E-3,4.983549E-4,1.257961E-4,3.096107E-5,7.441425E-6,1.748954E-6,-3.621719E3,
        		-1.249544E3,-6.593750E1,2.313404E2,1.993750E2,1.140396E2,5.375000E1,2.237311E1,8.500000,3.004164,1.0,3.162278E-1,9.560866E-2,
        		2.777326E-2,7.781887E-3,2.109851E-3,5.549760E-4,1.419451E-4,3.536873E-5,8.599821E-6,2.043426E-6,-2.720000E3,-1.325693E3,
        		-2.800000E2,9.345371E1,1.360000E2,9.010457E1,4.600000E1,2.021257E1,8.000000,2.922210,1.0,3.238679E-1,1.000000E-1,2.960094E-2,
        		8.436666E-3,2.323354E-3,6.199964E-4,1.607091E-4,4.054717E-5,9.975116E-6,2.396525E-6,-5.200000E2,-9.604292E2,-4.400000E2,
        		-8.116979E1,4.000000E1,4.989243E1,3.200000E1,1.608486E1,7.000000,2.751810,1.0,3.408012E-1,1.100000E-1,3.386683E-2,1.000000E-2,
        		2.843926E-3,7.816670E-4,2.082290E-4,5.389020E-5,1.357713E-5,3.335759E-6,4.801563E2,-6.392034E2,-4.219375E2,-1.263471E2,
        		5.875000,3.344652E1,2.575000E1,1.412135E1,6.500000,2.663033,1.0,3.502223E-1,1.157171E-1,3.636619E-2,1.093663E-2,3.162278E-3,
        		8.824301E-4,2.383745E-4,6.249592E-5,1.593670E-5,3.960124E-6,1.280000E3,-2.908144E2,-3.600000E2,-1.488033E2,-2.000000E1,
        		1.932386E1,2.000000E1,1.222743E1,6.000000,2.571619,1.0,3.603813E-1,1.220000E-1,3.916014E-2,1.200000E-2,3.528879E-3,
        		1.000000E-3,2.739812E-4,7.277768E-5,1.8786333E-5,4.721868E-6,2.080000E3,3.422631E2,-1.600000E2,-1.406839E2,-5.000000E1,
        		-2.319822,1.000000E1,8.657471,5.000000,2.379971,1.0,3.832954E-1,1.366000E-1,4.582817E-2,1.460000E-2,4.445346E-3,
        		1.300000E-3,3.665869E-4,1.000000E-4,2.645880E-5,6.805581E-6,7.083767E5,2.267778E5,6.795656E4,1.928329E4,5.223125E3,
        		1.358411E3,3.407500E2,8.273452E1,1.950000E1,4.472136,1.0,2.183910E-1,4.665131E-2,9.760018E-3,2.002127E-3,4.031151E-4,
        		7.973669E-5,1.550727E-5,2.967449E-6,5.591084E-7,1.037868E-7,5.718800E5,1.904661E5,5.890400E4,1.715960E4,4.754000E3,
        		1.261151E3,3.220000E2,7.944305E1,1.900000E1,4.416571,1.0,2.209547E-1,4.771854E-2,1.008682E-2,2.089455E-3,4.246118E-4,
        		8.473226E-5,1.661800E-5,3.205685E-6,6.086717E-7,1.138273E-7,4.548314E5,1.583978E5,5.070906E4,1.519555E4,4.311875E3,
        		1.167926E3,3.037500E2,7.619402E1,1.850000E1,4.360333,1.0,2.236068E-1,4.883372E-2,1.043155E-2,2.182406E-3,4.476893E-4,
        		9.013929E-5,1.782967E-5,3.467531E-6,6.635418E-7,1.250204E-7,3.552800E5,1.302371E5,4.332000E4,1.338422E4,3.896000E3,1.078682E3,
        		2.860000E2,7.2987895E1,1.800000E1,4.303395,1.0,2.263524E-1,5.000000E-2,1.079556E-2,2.281455E-3,4.724951E-4,9.599954E-5,
        		1.915334E-5,3.755760E-6,7.243824E-7,1.375188E-7,2.014800E5,8.437082E4,3.076000E4,1.019252E4,3.140000E3,9.119142E2,
        		2.520000E2,6.670697E1,1.700000E1,4.187322,1.0,2.321462E-1,5.250000E-2,1.158751E-2,2.500000E-3,5.279657E-4,1.092727E-4,
        		2.218825E-5,4.424401E-6,8.671191E-7,1.671602E-7,1.439333E5,6.606874E4,2.549306E4,8.798801E3,2.798375E3,8.342797E2,
        		2.357500E2,6.363322E1,1.650000E1,4.128131,1.0,2.352064E-1,5.384156E-2,1.201887E-2,2.620718E-3,5.590170E-4,1.167973E-4,
        		2.392958E-5,4.812478E-6,9.508797E-7,1.847389E-7,9.728000E4,5.048083E4,2.084000E4,7.531109E3,2.480000E3,7.604022E2,2.200000E2,
        		6.060478E1,1.600000E1,4.068130,1.0,2.383844E-1,5.525000E-2,1.247637E-2,2.750000E-3,5.925747E-4,1.250000E-4,2.584358E-5,
        		5.242417E-6,1.044379E-6,2.045040E-7,3.128000E4,2.641390E4,1.320000E4,5.348207E3,1.910000E3,6.236862E2,1.900000E2,5.468642E1,
        		1.500000E1,3.945568,1.0,2.451238E-1,5.828750E-2,1.347877E-2,3.037500E-3,6.682618E-4,1.437500E-4,3.027482E-5,6.250000E-6,
        		1.266070E-6,2.518957E-7,9.981955E6,2.214903E6,4.776816E5,1.003667E5,2.058563E4,4.128459E3,8.107500E2,1.561009E2,2.950000E1,
        		5.477226,1.0,1.796720E-1,3.179141E-2,5.543292E-3,9.530366E-4,1.616477E-4,2.706222E-5,4.473943E-6,7.306936E-7,1.179423E-7,
        		1.882149E-8,8.927880E6,2.011628E6,4.399440E5,9.363200E4,1.943400E4,3.940928E3,7.820000E2,1.520442E2,2.900000E1,5.431768,
        		1.0,1.811062E-1,3.228974E-2,5.671357E-3,9.819015E-4,1.676686E-4,2.825292E-5,4.700129E-6,7.722946E-7,1.253895E-7,2.012379E-8,
        		7.960035E6,1.822447E6,4.043841E5,8.721232E4,1.832438E4,3.758404E3,7.537500E2,1.480220E2,2.850000E1,5.385938,1.0,1.825742E-1,
        		3.280348E-2,5.804292E-3,1.012062E-3,1.739999E-4,2.951270E-5,4.940863E-6,8.168265E-7,1.334058E-7,2.153323E-8,7.073280E6,
        		1.646682E6,3.709200E5,8.109905E4,1.725600E4,3.580842E3,7.260000E2,1.440346E2,2.800000E1,5.339729,1.0,1.840774E-1,
        		3.333333E-2,5.942350E-3,1.043595E-3,1.806618E-4,3.084652E-5,5.197277E-6,8.645345E-7,1.420422E-7,2.305998E-8,5.523480E6,
        		1.332773E6,3.099600E5,6.975756E4,1.524000E4,3.240428E3,6.720000E2,1.361654E2,2.700000E1,5.246132,1.0,1.871948E-1,
        		3.444444E-2,6.234951E-3,1.111111E-3,1.950673E-4,3.375827E-5,5.762197E-6,9.705817E-7,1.614057E-7,2.651182E-8,4.851136E6,
        		1.193365E6,2.823081E5,6.451242E4,1.429088E4,3.077487E3,6.457500E2,1.322842E2,2.650000E1,5.198724,1.0,1.888121E-1,
        		3.502734E-2,6.390097E-3,1.147278E-3,2.028602E-4,3.534839E-5,6.073526E-6,1.029541E-6,1.722631E-7,2.846332E-8,4.241280E6,
        		1.064843E6,2.564400E5,5.953990E4,1.338000E4,2.919330E3,6.200000E2,1.284391E2,2.600000E1,5.150896,1.0,1.904706E-1,3.562963E-2,
        		6.551575E-3,1.185185E-3,2.110828E-4,3.703704E-5,6.406207E-6,1.092923E-6,1.840029E-7,3.058527E-8,3.192480E6,8.381345E5,
        		2.097600E5,5.037975E4,1.167000E4,2.617185E3,5.700000E2,1.208582E2,2.500000E1,5.053937,1.0,1.939181E-1,3.689630E-2,
        		6.894985E-3,1.266667E-3,2.289400E-4,4.074074E-5,7.142864E-6,1.234568E-6,2.104735E-7,3.541125E-8};
        int i;
        for (i = 0; i < answer.length; i++) {
        	a = atest[i];
        	b = btest[i]; 
        	x = xtest[i];
	        secondKindRealArgument();
	        Preferences.debug("a = " + a + " b = " + b + " x = " + x + " result = " + result[0] + " answer = " + answer[i] + 
	        		" method = " + method[0] + "\n",Preferences.DEBUG_ALGORITHM);
	        if (Double.isNaN(result[0])) {
	        	Preferences.debug("Error detected\n",Preferences.DEBUG_ALGORITHM);
	     		errorsDetected++;	
	        }
	        else if (answer[i] != 0.0) {
		        if ((result[0]/answer[i] < 1-1.0E-6) || (result[0]/answer[i] > 1+1.0E-6)) {
		     		   Preferences.debug("Error detected\n",Preferences.DEBUG_ALGORITHM);
		     		   errorsDetected++;
		        }
	        }
	        else {
	        	if (Math.abs(result[0]) > 1.0E-6) {
	        		Preferences.debug("Error detected\n",Preferences.DEBUG_ALGORITHM);
		     		errorsDetected++;	
	        	}
	        }
        }
        Preferences.debug(errorsDetected + " errors detected in " + answer.length + " tests on secondKindRealArgument()\n", Preferences.DEBUG_ALGORITHM);
        System.out.println(errorsDetected + " errors detected in " + answer.length + " tests on secondKindRealArgument()");
    }

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
