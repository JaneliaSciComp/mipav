package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

/**  This code calculates the confluent hypergeometric function of input parameters
 *   a and b and input argument x
 *   A typcial usage would be:
 *   double result[] = new double[1];
 *   ConfluentHypergeometric ch = new ConfluentHypergeometric(CONFLUENT_HYPERGEOMETRIC_FIRST_KIND,
 *                                                            -0.5, 1, 1.0, result);
 *   ch.run();
 *   Preferences.debug("Confluent hypergeomtric result = " + result[0] + "\n");
 *   UI.setDataText("Confluent hypergeometric result = " + result[0] + "\n");
*/

public class ConfluentHypergeometric {
//  ~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Confluent Hypergeometric Function of the First Kind. */
    public static final int CONFLUENT_HYPERGEOMETRIC_FIRST_KIND = 1;
    
    /** Confluent Hypergeometric Function of the Second Kind */
    public static final int CONFLUENT_HYPERGEOMETRIC_SECOND_KIND = 2;
    
    public static final int REAL_VERSION = 1;
    
    public static final int COMPLEX_VERSION = 2;
    
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
   
    /** Input parameter */
    private double realA;
    private double imagA;
    
    /** Input parameter */
    private double realB;
    private double imagB;
    
    /** Input argument */
    private double realZ;
    private double imagZ;
    
    /** Tells how the result should be represented
     *  A '0' will return the result in standard exponential form.
     *  A '1' will return the log of the result. */
    private int Lnchf;
    
    /** Specifies how many array positions are desired (usually 10 is sufficient).
     * More difficult cases may require this parameter to be 100 or more, and it
     * is not always trivial to predict which cases these might be.
     * One pragmatic approach is to simply run the evaluator using a value of 10,
     * then increase ip, run the evaluator again and compare the returned results.
     * Nardin et al. state that overwriting memory may occur if ip exceeds 777.
     * Setting IP = 0 causes the program to estimate the number of array positions */
    private int ip;
    
    /** outputted result */
    private double realResult[];
    private double imagResult[];
    
    /** Create arrays of size length+2 */
    private int length = 777;
    
    /** Size of the arrays */
    private int L;
    
    /** Number of digits required to represent the numbers with the required accuracy */
    private double rmax;
    
    
//  ~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * @param kind Tells whether confluent hypergeometric function is of first or second kind
     * @param a input parameter
     * @param b input parameter
     * @param x input argument
     * @param result outputted result
     */
    public ConfluentHypergeometric(int kind, double a, double b, double x, double result[]) {
        this.kind = kind;
        this.a = a;
        this.b = b;
        this.x = x;
        this.result = result;
        this.version = REAL_VERSION;
    }
    
    /**
     * @param realA  Real part of first input parameter
     * @param imagA  Imaginary part of first input parameter
     * @param realB  Real part of second input parameter
     * @param imagB  Imaginary part of second input parameter
     * @param realZ  Real part of input argument
     * @param imagZ  Imaginary part of input argument
     * @param Lnchf  = 0  for standard result, = 1 for log of result
     * @param ip     Number of array positions desired
     * @param realResult Real part of outputted result 
     * @param imagResult Imaginary part of outputted result
     */
    public ConfluentHypergeometric(double realA, double imagA, double realB, double imagB,
                                   double realZ, double imagZ, int Lnchf, int ip, 
                                   double realResult[], double imagResult[]) {
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
        this.kind = CONFLUENT_HYPERGEOMETRIC_FIRST_KIND;
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
        if ((kind == CONFLUENT_HYPERGEOMETRIC_FIRST_KIND) && (version == REAL_VERSION)) {
            firstKindRealArgument();
        }
        else if ((kind == CONFLUENT_HYPERGEOMETRIC_FIRST_KIND) && (version == COMPLEX_VERSION)) {
            firstKindComplex();
        }
        else if (kind == CONFLUENT_HYPERGEOMETRIC_SECOND_KIND) {
            MipavUtil.displayError("Second kind not implemented");
            return;
        }
        else {
            MipavUtil.displayError("Illegal kind argument");
            return;
        }
    } // run()
    
    /**
     * Port of Algorithm 707, Collected Algorithms from ACM.
     * Original Work published in Transactions on Mathematical Software, Vol. 18. No. 3,
     * September, 1992, pp. 345-349.
     * This is a port of Solution to the Confluent Hypergeometric Function by Mark Nardin,
     * W. F. Perger, and Atul Bhalla, Michigan Technological University, Copyright 1989.
     * Description: A numerical evaluator for the confluent hypergeometric function for 
     * complex arguments with large magnitudes using a direct summation of the Kummer
     * series.  The method used allows an accuracy of up to thirteen decimal places through
     * the use of large arrays and a single final division.
     * The confluent hypergeometric function of the first kind is the solution to the
     * equation:
     * zf"(z) + (a-z)f'(z) - bf(z) = 0
     * Reference: Numerical evaluation of the confluent hypergeometric function for complex
     * arguments of large magnitudes by Mark Nardin, W. F. Perger, and Atul Bhalla, Journal
     * of Computational and Applied Mathematics, Vol. 39, 1992, pp. 193-200.
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
        int bit;
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
        double sumr[] = new double[length+2];
        double sumi[] = new double[length+2];
        double numr[] = new double[length+2];
        double numi[] = new double[length+2];
        double denomr[] = new double[length+2];
        double denomi[] = new double[length+2];
        double cnt;
        double mx1;
        double mx2;
        double qr1[] = new double[length+2];
        double qr2[] = new double[length+2];
        double qi1[] = new double[length+2];
        double qi2[] = new double[length+2];
        double realNum;
        double imagNum;
        double absVal;
        
        if ((realZ != 0.0) && (imagZ != 0.0)) {
            ang = Math.atan2(imagZ, realZ);   
        }
        else {
            ang = 1.0;
        }
        if (Math.abs(ang) < (0.5*Math.PI)) {
            ang = 1.0;
        }
        else {
            ang = Math.sin(Math.abs(ang) - (0.5*Math.PI)) + 1.0;
        }
        max = 0.0;
        nterm = 0.0;
        fx = 0.0;
        term1 = 0.0;
        while (true) {
            nterm = nterm + 1;
            realTerm2 = (realA + nterm - 1.0)* realZ  - imagA * imagZ;
            imagTerm2 = (realA + nterm - 1.0)* imagZ + imagA * realZ;
            realTerm2 = realTerm2 / nterm;
            imagTerm2 = imagTerm2 / nterm;
            denom = (realB + nterm - 1.0)*(realB + nterm - 1.0) + imagB*imagB;
            realTerm2 = realTerm2 / denom;
            imagTerm2 = imagTerm2 / denom;
            realTemp = realTerm2 * (realB * nterm - 1.0) + imagTerm2 * imagB;
            imagTerm2 = -realTerm2 * imagB + imagTerm2 * (realB * nterm - 1.0);
            term2 = Math.sqrt(realTemp*realTemp + imagTerm2*imagTerm2);
            if (term2 == 0.0) {
                break;
            } // if (term2 == 0.0)
            if (term2 < 1.0) {
                if ((realA + nterm) > 2.0) {
                    if ((realB + nterm) > 2.0) {
                        if ((term2 - term1) < 0.0) {
                            break;
                        } // if ((term2 - term1) < 0.0)
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
        max = max * 2.0/(bit * 0.693147181);
        L = (int)(max*ang) + 7;
        if (L < 5) {
            L = 5;
        } // (if L < 5)
        if (ip > L) {
            L = ip;
        } // if (ip > L)
        
        // Sum the Kummer series and return the solution of the confluent hypergeometric
        // function
        rmax = Math.pow(2.0,bit/2);
        sigfig = Math.pow(2.0, bit/4);
        ar2 = realA * sigfig;
        ar = (int)ar2;
        ar2 = (int)((ar2-ar)*rmax + 0.5);
        ai2 = imagA * sigfig;
        ai = (int)ai2;
        ai2 = (int)((ai2-ai)*rmax + 0.5);
        cr2 = realB * sigfig;
        cr = (int)cr2;
        cr2 = (int)((cr2-cr)*rmax + 0.5);
        ci2 = imagB * sigfig;
        ci = (int)ci2;
        ci2 = (int)((ci2-ci)*rmax + 0.5);
        xr2 = realZ * sigfig;
        xr = (int)xr2;
        xr2 = (int)((xr2-xr)*rmax + 0.5);
        xi2 = imagZ * sigfig;
        xi = (int)xi2;
        xi2 = (int)((xi2-xi)*rmax + 0.5);
        sumr[0] = 1.0;
        sumi[0] = 1.0;
        numr[0] = 1.0;
        numi[0] = 1.0;
        denomr[0] = 1.0;
        denomi[0] = 1.0;
        for (i = 1; i <= L+2; i++) {
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
        while (true) {
            if (sumr[2] < 0.5) {
                mx1 = sumi[L+2];
            } // if (sumr[2] < 0.5)
            else if (sumi[2] < 0.5) {
                mx1 = sumr[L+2];
            } // else if (sumi[2] < 0.5)
            else {
                mx1 = Math.max(sumr[L+2],sumi[L+2]);
            } // else
            if (numr[2] < 0.5) {
                mx2 = numi[L+2];
            } // if (numr[2] < 0.5)
            else if (numi[2] < 0.5) {
                mx2 = numr[L+2];
            } // else if (numi[2] < 0.5)
            else {
                mx2 = Math.max(numr[L+2],numi[L+2]);
            } // else
            if ((mx1 - mx2) > 2.0) {
                if (cr > 0.0) {
                    realNum = (ar*xr - ai*xi)/cnt;
                    imagNum = (ar*xi + ai*xr)/cnt;
                    denom = cr*cr + ci*ci;
                    realNum = realNum / denom;
                    imagNum = imagNum / denom;
                    realTemp = realNum * cr + imagNum * ci;
                    imagNum = -realNum*ci + imagNum*cr;
                    absVal = Math.sqrt(realTemp*realTemp + imagNum*imagNum);
                    if (absVal <= 1.0) {
                        break;
                    } // if (absVal <= 1.0)
                } // if (cr > 0.0)
            } // if ((mx1 - mx2) > 2.0)
            cmpmul(sumr, sumi, cr, ci, qr1, qi1);
            cmpmul(sumr, sumi, cr2, ci2, qr2, qi2);
            qr2[L+2] = qr2[L+2] - 1;
            qi2[L+2] = qi2[L+2] - 1;
            cmpadd(qr1, qi1, qr2, qi2, sumr, sumi);
            
            armult(sumr, cnt, sumr);
            armult(sumi, cnt, sumi);
            cmpmul(denomr, denomi, cr, ci, qr1, qi1);
            cmpmul(denomr, denomi, cr2, ci2, qr2, qi2);
            qr2[L+2] = qr2[L+2] - 1;
            qi2[L+2] = qi2[L+2] - 1;
            cmpadd(qr1, qi1, qr2, qi2, denomr, denomi);
            
            armult(denomr, cnt, denomr);
            armult(denomi, cnt, denomi);
            cmpmul(numr, numi, ar, ai, qr1, qi1);
            cmpmul(numr, numi, ar2, ai2, qr2, qi2);
            qr2[L+2] = qr2[L+2] - 1;
            qi2[L+2] = qi2[L+2] - 1;
            cmpadd(qr1, qi1, qr2, qi2, numr, numi);
            
            cmpmul(numr, numi, xr, xi, qr1, qi1);
            cmpmul(numr, numi, xr2, xi2, qr2, qi2);
            qr2[L+2] = qr2[L+2] - 1;
            qi2[L+2] = qi2[L+2] - 1;
            cmpadd(qr1, qi1, qr2, qi2, numr, numi);
            
            cmpadd(sumr, sumi, numr, numi, sumr, sumi);
            cnt = cnt + sigfig;
            ar = ar + sigfig;
            cr = cr + sigfig;
        } // while (true)
    } // firstKindComplex
    
    /**
     * Takes two arrays representing one real and one imaginary part, and adds two arrays 
     * representing another complex number and returns two arrays holding the complex sum.
     * (CR, CI) = (AR+BR, AI+BI)
     * @param ar
     * @param ai
     * @param br
     * @param bi
     * @param cr
     * @param ci
     */
    private void cmpadd(double ar[], double ai[], double br[], double bi[],
                        double cr[], double ci[]) {
        aradd(ar, br, cr);
        aradd(ai, bi, ci);
        return;
    } // cmpadd
    
    /** 
     * Determines the number of significant figures of machine precision to arrive at the
     * size of the array the numbers must be stored to get the accuracy of the solution.
     */
    private int bits() {
        double bit, bit2;
        int count;
         
        bit = 1.0;
        count = 0;
        do {
            count = count + 1;
            bit2 = store(2.0*bit);
            bit = store(bit2+1.0);
        } while ((bit - bit2) != 0.0);
        return count;
    } // bits
    
    /**
     * This funciton forces its argument x to be stored in a memory location, thus providing
     * a means of determining floating point number characteristics (such as the machine 
     * precision) when it is necessary to avoid computation in high precision registers.
     * @param x The value to be stored
     * @return The value of x after it has been stored and possibly truncated or rounded to
     * the double precision word length.
     */
    private double store(double x) {
        double y;
        double storeVar;
        y = x;
        storeVar = y;
        return storeVar;
    }
    
    /**
     * Takes 2 arrays, ar and ai representing one real and one imaginary part, and
     * multiplies it with br and bi, representing a complex
     * number and returns the complex product
     * @param ar
     * @param ai
     * @param br
     * @param bi
     * @param cr
     * @param ci
     */
    private void cmpmul (double ar[], double ai[], double br, double bi,
                         double cr[], double ci[]) {
        double d1[] = new double[length+2];
        double d2[] = new double[length+2];
        
        armult(ar, br, d1);
        armult(ai, bi, d2);
        arsub(d1, d2, cr);
        armult(ar, bi, d1);
        armult(ai, br, d2);
        aradd(d1, d2, ci);
        return;
    } // cmpmul
    
    /**
     * Accepts two array a and scalar b, and returns the product array c.
     * @param a
     * @param b
     * @param c
     */
    private void armult (double a[], double b, double c[]) {
        double z[] = new double[length+2];
        double b2;
        double carry;
        double rmax2;
        int i;
        
        rmax2 = 1.0/rmax;
        if (b >= 0.0) {
            z[0] = a[0];
        }
        else {
            z[0] = -a[0];
        }
        b2 = Math.abs(b);
        z[L+2] = a[L+2];
        for (i = 1; i <= L+1; i++) {
            z[i] = 0.0;
        } // for (i = 1; i <= L+1; i++)
        if ((b2 <= 1.0E-10) || (a[2] <= 1.0E-10)) {
            z[0] = 1.0;
            z[L+2] = 0.0;
            for (i = 0; i <= L+2; i++) {
                c[i] = z[i];
            }
            if (c[2] < 0.5) {
                c[0] = 1.0;
                c[L+2] = 0.0;
            } // if (c[2] < 0.5)
            return;
        } // if (b2 <= 1.0E-10) || (a[2] <= 1.0E-10))
        for (i = L+1; i >= 2; i--) {
            z[i] = a[i]*b2 + z[i];
            if (z[i] >= rmax) {
                carry = (int)(z[i]/rmax);
                z[i] = z[i] - carry*rmax;
                z[i-1] = carry;
            } // if (z[i] >= rmax)
        } // for (i = L+1; i >= 2; i--)
        if (z[1] >= 0.5) {
            for (i = L+1; i >= 2; i--) {
                z[i] = z[i-1];
            } // for (i = L+1; i >=2; i--)
            z[L+2] = z[L+2] + 1.0;
            z[1] = 0.0;
        } // if (z[1] >= 0.5)
        
        for (i = 0; i <= L+2; i++) {
            c[i] = z[i];
        } // for (i = 0; i <= L+2; i++)
        if (c[2] < 0.5) {
            c[0] = 1.0;
            c[L+2] = 0.0;
        } // if (c[2] < 0.5)
        return;
    } // armult
    
    /**
     * Accepts two arrays of numbers, a and b, and returns the sum of the array c.
     * @param a
     * @param b
     * @param c
     */
    private void aradd(double a[], double b[], double c[]) {
        double z[] = new double[length+2];
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
        boolean seg8 = true;
        
        for (i = 1; i <= L+2; i++) {
            z[i] = 0.0;
        }
        ediff = (int)(a[L+2] - b[L+2] + 0.5);
        if ((Math.abs(a[2]) < 0.5) || (ediff <= -L)) {
            for (i = 0; i <= L+2; i++) {
                c[i] = b[i];
            } // for (i = 0; i <= L+2; i++)
            if (c[2] < 0.5) {
                c[0] = 1.0;
                c[L+2] = 0.0;
            } // if (c[2] < 0.5)
            return;
        } // if ((Math.abs(a[2]) < 0.5) || (ediff <= -L))
        if ((Math.abs(b[2]) < 0.5) || (ediff >= L)) {
            for (i = 0; i <= L+2; i++) {
                c[i] = a[i];
            } // for (i = 0; i <= L+2; i++)
            if (c[2] < 0.5) {
                c[0] = 1.0;
                c[L+2] = 0.0;
            } // if (c[2] < 0.5)
            return;
        } // if ((Math.abs(b[2]) < 0.5) || (ediff >= L))
        z[0] = a[0];
        if (Math.abs(a[0] - b[0]) >= 0.5) {
            if (ediff > 0) {
                z[L+2] = a[L+2];
                seg1 = false;
                seg2 = false;
                seg3 = false;
                seg4 = false;
            } // if (ediff > 0)
            if (seg1) {
                if (ediff < 0) {
                    z[L+2] = b[L+2];
                    z[0] = b[0];
                    seg2 = false;
                    seg3 = false;
                    seg4 = false;
                    seg5 = false;
                    seg6 = false;
                } // if (ediff < 0)
            } // if (seg1)
            if (seg2) {
                for (i = 2; i <= L+1; i++) {
                    if (a[i] > b[i]) {
                        z[L+2] = a[L+2];
                        seg3 = false;
                        seg4 = false;
                        break;
                    } // if (a[i] > b[i])
                    if (a[i] < b[i]) {
                        z[L+2] = b[L+2];
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
                for (i = 0; i <= L+2; i++) {
                    c[i] = z[i];
                } // for (i = 0; i <= L+2; i++)
                if (c[2] < 0.5) {
                    c[0] = 1.0;
                    c[L+2] = 0.0;
                } // if (c[2] < 0.5)
                return;
            } // if (seg3)
        } // if (Math.abs(a[0] - b[0]) >= 0.5)
        
        if (seg4) {
            if (ediff == 0) {
                z[L+2] = a[L+2];
                for (i = L+1; i >= 2; i--) {
                    z[i] = a[i] + b[i] + z[i];
                    if (z[i] >= rmax) {
                        z[i] = z[i] - rmax;
                        z[i-1] = 1.0;
                    } // if (z[i] >= rmax)
                } // for (i = L+1; i >= 2; i--)
                if (z[0] > 0.5) {
                    for (i = L+2; i >= 2; i--) {
                        z[i] = z[i-1];
                    } // for (i = L+2; i >= 2; i--)
                    z[L+2] = z[L+2] + 1.0;
                    z[1] = 0.0;
                } // if (z[0] > 0.5)
                for (i = 0; i <= L+2; i++) {
                    c[i] = z[i];
                } // for (i = 0; i <= L+2; i++)
                if (c[2] < 0.5) {
                    c[0] = 1.0;
                    c[L+2] = 0.0;
                } // if (c[2] < 0.5)
                return;
            } // if (ediff == 0)
            else if (ediff > 0.0) {
                z[L+2] = a[L+2];
                for (i = L+1; i >= 2+ediff; i--) {
                    z[i] = a[i] + b[i-ediff] + z[i];
                    if (z[i] >= rmax) {
                        z[i] = z[i] - rmax;
                        z[i-1] = 1.0;
                    } // if (z[i] >= rmax)
                } // for (i = L+1; i >= 2+ediff; i--)
                for (i = ediff+1; i >= 2; i--) {
                    z[i] = a[i] + z[i];
                    if (z[i] >= rmax) {
                        z[i] = z[i] -rmax;
                        z[i-1] = 1.0;
                    } // if (z[i] >= rmax)
                } // for (i = ediff+1; i>= 2; i--)
                if (z[1] > 0.5) {
                    for (i = L+1; i >= 2; i--) {
                        z[i] = z[i-1];
                    } // for (i = L+1; i >= 2; i--)
                    z[L+2] = z[L+2] + 1.0;
                    z[1] = 0.0;
                } // if (z[1] > 0.5)
                for (i = 0; i <= L+2; i++) {
                    c[i] = z[i];
                } // for (i = 0; i <= L+2; i++)
                if (c[2] < 0.5) {
                    c[0] = 1.0;
                    c[L+2] = 0.0;
                } // if (c[2] < 0.5)
                return;
            } // else if (ediff > 0.0)
            else { // ediff < 0.0
                z[L+2] = b[L+2];
                for (i = L+1; L >= 2 - ediff; i--) {
                    z[i] = a[i+ediff] + b[i] + z[i];
                    if (z[i] >= rmax) {
                        z[i] = z[i] - rmax;
                        z[i-1] = 1.0;
                    } // if (z[i] >= rmax)
                } // for (i = L+1; L >= 2 - ediff; i--)
                for (i = 1-ediff; i >= 2; i--) {
                    z[i] = b[i] + z[i];
                    if (z[i] >= rmax) {
                        z[i] = z[i] - rmax;
                        z[i-1] = 1.0;
                    } // if (z[i] >= rmax)
                } // for (i = 1-ediff; i >= 2; i--)
                if (z[1] > 0.5) {
                    for (i = L+1; i >= 2; i--) {
                        z[i] = z[i-1];
                    } // for (i = L+1; i >= 2; i--)
                    z[L+2] = z[L+2] + 1.0;
                    z[1] = 0.0;
                } // if (z[1] > 0.5)
                for (i = 0; i <= L+2; i++) {
                    c[i] = z[i];
                } // for (i = 0; i <= L+2; i++)
                if (c[2] < 0.5) {
                    c[0] = 1.0;
                    c[L+2] = 0.0;
                } // if (c[2] < 0.5)
                return;
            } // else ediff < 0.0
        } // if (seg4)
        
        if (seg5) {
            if (ediff <= 0) {
                for (i = L+1; i >= 2; i--) {
                    z[i] = a[i] - b[i] + z[i];
                    if (z[i] < 0.0) {
                        z[i] = z[i] + rmax;
                        z[i-1] = -1.0;
                    } // if (z[i] < 0.0)
                } // for (i = L+1;i >= 2; i--)
                seg6 = false;
                seg7 = false;
                seg8 = false;
            } // if (ediff <= 0)
        } // if (seg5)
        if (seg6) {
            for (i = L+1; i >= 2+ediff; i--) {
                z[i] = a[i] - b[i-ediff] + z[i];
                if (z[i] < 0.0) {
                    z[i] = z[i] +rmax;
                    z[i-1] = -1.0;
                } // if (z[i] < 0.0)
            } // for (i = L+1; i >= 2+ediff; i--)
            for (i = ediff+1; i >= 2; i--) {
                z[i] = a[i] + z[i];
                if (z[i] < 0.0) {
                    z[i] = z[i] + rmax;
                    z[i-1] = -1.0;
                } // if (z[i] < 0.0)
            } // for (i = ediff+1; i >= 2; i--)
            seg7 = false;
            seg8 = false;
        } // if (seg6)
        
        if (seg7) {
            if (ediff >= 0) {
                for (i = L+1; i >= 2; i--) {
                    z[i] = b[i] - a[i] + z[i];
                    if (z[i] < 0.0) {
                        z[i] = z[i] + rmax;
                        z[i-1] = -1.0;
                    } // if (z[i] < 0.0)
                } // for (i = L+1; i >= 2; i--)
                seg8 = false;
            } // if (ediff >= 0)
        } // if (seg7)
        
        if (seg8) {
            for (i = L+1; i >= 2 - ediff; i--) {
                z[i] = b[i] - a[i+ediff] + z[i];
                if(z[i] < 0.0) {
                    z[i] = z[i] + rmax;
                    z[i-1] = -1.0;
                } // if (z[i] < 0.0)
            } // for (i = L+1; i >= 2 - ediff; i--)
            for (i = 1-ediff; i >= 2; i--) {
                z[i] = b[i] + z[i];
                if (z[i] < 0.0) {
                    z[i] = z[i] + rmax;
                    z[i-1] = -1.0;
                } // if (z[i] < 0.0)
            } // for (i = 1-ediff; i >= 2; i--)
        } // if (seg8)
        
        if (z[2] <= 0.5) {
            i = 1;
            do {
                i = i + 1;
            } while ((z[i+1] < 0.5) && (i < (L+1)));
            if (i == (L+1)) {
                z[0] = 1.0;
                z[L+2] = 0.0;
                for (i = 0; i <= L+2; i++) {
                    c[i] = z[i];
                } // for (i = 0; i <= L+2; i++)
                if (c[2] < 0.5) {
                    c[0] = 1.0;
                    c[L+2] = 0.0;
                } // if (c[2] < 0.5)
                return;
            } // if (i == (L+1))
            for (j = 2; j <= L+2-i; j++) {
                z[j] = z[j+i-1];
            } // for (j = 2; j <= L+2-i; j++)
            for (j = L+3-i; j <= L+1; j++) {
                z[j] = 0.0;
            } // for (j = L+3-i; j <= L+1; j++)
            z[L+2] = z[L+2] - i + 1;
        } // if (z[2] <= 0.5)
        for (i = 0; i <= L+2; i++) {
            c[i] = z[i];
        } // for (i = 0; i <= L+2; i++)
        if (c[2] < 0.5) {
            c[0] = 1.0;
            c[L+2] = 0.0;
        } // if (c[2] < 0.5)
        return;
    } // aradd
    
    /**
     * Accepts two arrays and subtracts each element in the second array b from
     * the element in the first array a and returns the solution c
     * @param a
     * @param b
     * @param c
     */
    private void arsub (double a[], double b[], double c[]) {
        int i;
        double b2[] = new double[length+2];
        
        for (i = 0; i <= L+2; i++) {
            b2[i] = b[i];
        }
        b2[0] = -1.0 * b2[0];
        aradd(a, b2, c);
        return;
    } // arsub
    
    /**
     * This code is a port of the FORTRAN routine CHGM from the book Computation of
     * Special Functions by Shanjie Zhang and Jianming Jin, John Wiley & Sons, Inc.,
     * 1996, pp. 398-400.  It computes confluent hypergeometric functions of the first
     * kind for real parameters and argument.  It works well except for the case of 
     * a << -1 when x > 0 and a >> 1 when x < 0. In this exceptional case, the evaluation
     * involves the summation of a partially alternating series which results in a loss
     * of significant digits.
     * Note that with a = -0.5, b = 1, x = 798.2, this code resulted in 
     * Math.exp(x) == infinite.  So this code cannot be used for large x.
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
        double ta[] = new double[1];
        double tb[] = new double[1];
        double tba[] = new double[1];
        int i;
        
        a0 = a;
        a1 = a;
        x0 = x;
        result[0] = 0.0;
        if ((b == 0.0) || (b == -Math.abs((int)b))) {
            result[0] = Double.POSITIVE_INFINITY;
        }
        else if ((a == 0.0) || (x == 0.0)) {
            result[0] = 1.0;
        }
        else if (a == -1.0) {
            result[0] = 1.0 - x/b;
        }
        else if (a == b) {
            result[0] = Math.exp(x);
        }
        else if ((a-b) == 1.0) {
            result[0] = (1.0 + x/b)*Math.exp(x);
        }
        else if ((a == 1.0) && (b == 2.0)) {
            result[0] = (Math.exp(x) - 1.0)/x;
        }
        else if ((a == (int)a) && (a < 0.0)) {
            m = (int)(-a);
            r = 1.0;
            result[0] = 1.0;
            for (k = 1; k <= m; k++) {
                r = r * (a + k - 1.0)/k/(b+k-1.0)*x;
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
        }
        else {
            // (2a - b + z)1F1(a, b, z) = a*1F1(a+1, b, z) - (b - a)*1F1(a-1, b, z)
            nL = 1;
            L = (int)a;
            a = a - L - 1.0;
        } // else (a >= 2.0)
        for (L0 = 0; L0 <= nL; L0++) {
            if (a0 >= 2.0) {
                a = a + 1.0;
            }
            if ((x <= 30.0 + Math.abs(b)) || (a < 0.0)) {
                // 1F1(a, b, z) = sum from k = 0 to infinity of
                // Pochhammer(a,k)*(z**k)/((k!)*Pochhammer(b,k))
                result[0] = 1.0;
                rg = 1.0;
                for (j = 1; j <= 500; j++) {
                    rg = rg * (a+j-1.0)/(j*(b+j-1.0))*x;
                    result[0] = result[0] + rg;
                    if (Math.abs(rg/result[0]) < 1.0E-15) {
                        break;
                    }
                } // for (j = 1; j <= 500; j++)
            } //if ((x <= 30.0 + Math.abs(b)) || (a < 0.0))
            else {
               Gamma gam = new Gamma(a, ta);
               gam.run();
               gam = new Gamma(b, tb);
               gam.run();
               xg = b - a;
               gam = new Gamma(xg, tba);
               gam.run();
               sum1= 1.0;
               sum2 = 1.0;
               r1 = 1.0;
               r2 = 1.0;
               for (i = 1; i <= 8; i++) {
                   r1 = -r1 * (a + i - 1.0) * (a - b + i)/ (x * i);
                   r2 = -r2 * (b - a + i - 1.0) * (a - i)/ (x * i);
                   sum1 = sum1 + r1;
                   sum2 = sum2 + r2;
               } // for (i = 1; i <= 8; i++)
               hg1 = tb[0]/tba[0]*Math.pow(x,-a)*Math.cos(Math.PI*a)*sum1;
               hg2 = tb[0]/ta[0]*Math.exp(x)*Math.pow(x,a-b)*sum2;
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
            for (i = 1; i <= L-1; i++) {
                result[0] = ((2.0*a - b + x)*y1 + (b-a)*y0)/a;
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
}