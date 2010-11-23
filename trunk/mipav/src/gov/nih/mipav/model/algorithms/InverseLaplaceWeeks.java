package gov.nih.mipav.model.algorithms;


import de.jtem.numericalMethods.calculus.function.RealFunctionOfOneVariable;
import de.jtem.numericalMethods.calculus.minimizing.Brent;
import gov.nih.mipav.model.algorithms.filters.*;

import gov.nih.mipav.view.*;


/**
 * <p>This Weeks method of inverting the Laplace Transform is a port of MATLAB code. It is obtained from a web page of
 * Andre Weideman called <a href="http://dip.sun.ac.za/~weideman/research/weeks.html">ILT M-Files</a>.</p>
 * 
 * Note that in Software for an Implementation of Weeks's Method for the Inverse Laplace Transform
 * Problem by B. S. Garbow, G. Giunta, J. N. Lyness, and A. Murli states that for f(t) an entire function,
 * the results are good.  Weeks method does not work where f(t) has a discontinuity, a sqrt(t) singularity at
 * the origin, a log(t) singularity at the origin, or an isolated essential singularity at the origin.
 * 
 *Apparently Java rounding does not always occur the same way and InverseLaplaceWeeks magnifies the small
  rounding differences into 2 totally different paths.  For 2 runs on test case 6 with nLaguerreWeeks = 128.
  The print outs of 2 different runs were identical for the first 19 pages.  Then on page 20 for run 1:
  Entering werr2t b = 5.729490377339999 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 7.49999552373415 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 6.514404300769063 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 6.302208848022489 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 6.890866637840145 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 6.658200113810756 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 6.433352852465265 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 6.569329412293346 sig = 1.3525492793447342
  Exiting werr2t

  On page 20 for run 2:
  Entering werr2t b = 5.729490377339999 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 7.4999955225879775 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 6.5037420898569716 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 3.541019855868949 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 0.9066039647410506 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 0.41104422954889436 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 0.6003311993613015 sig = 1.3525492793447342
  Exiting werr2t
  Entering werr2t b = 0.2540393093650214 sig = 1.3525492793447342
  Exiting werr2t
  A difference in b starts in the tenth significant digit with run 1 having
  b = 7.49999552373415 and run 2 having b = 7.4999955225879775.  The difference
  quickly takes the 2 runs down 2 entirely different paths. 
 *
 * <p>References:</p>
 *
 * <ol>
 *   <li>"Algorithms for Parameter Selection in the Weeks Method for Inverting the Laplace Transform" by J. A. C.
 *     Weideman, SIAM Journal on Scientific Computing, Vol. 21, No. 1, 1999, pp. 111-128.</li>
 *   <li>"Software for an implementation of Weeks' method for the inverse of the Laplace Transform" by Garbow, Giunta,
 *     Lyness, and Murli, ACM TOMS Vol. 14, 1988, pp. 163 - 170.</li>
 *   <li>"Numercial Recipes in C The Art of Scientific Computing Second Edition" by William H. Press, Saul A. Teukolsky,
 *     William T. Vetterling, and Brian P. Flannery, Chapter 10.1 Golden Section Search in One Dimension, pp. 397-402.
 *   </li>
 * </ol>
 */

public abstract class InverseLaplaceWeeks implements RealFunctionOfOneVariable {
    // This Weeks method of inverting the Laplace Transform is a port of MATLAB
    // code obtained from a web page of Andre Weideman called ILT M-Files at
    // http://dip.sun.ac.za/~weideman/research/weeks.html
    // References:
    // 1.) "Algorithms for Parameter Selection in the Weeks Method for Inverting
    // the Laplace Transform" by J. A. C. Weideman, SIAM Journal on
    // Scientific Computing, Vol. 21, No. 1, 1999, pp. 111-128.
    // 2.) "Software for an implementation of Weeks' method for the inverse of
    // the Laplace Transform" by Garbow, Giunta, Lyness, and Murli,
    // ACM TOMS Vol. 14, 1988, pp. 163 - 170.
    // 3.)  "Numercial Recipes in C The Art of Scientific Computing Second
    // Edition" by William H. Press, Saul A. Teukolsky, William T.
    // Vetterling, and Brian P. Flannery, Chapter 10.1 Golden Section
    // Search in One Dimension, pp. 397-402.

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double ax, bx, cx;

    /** DOCUMENT ME! */
    private double ax2, bx2, cx2;

    /** b > 0. */
    private double b;

    /** DOCUMENT ME! */
    private double bmax;

    /** DOCUMENT ME! */
    private double bOpt;

    /** eps is the distance from 1.0 to the next largest double-precision number. */
    private double eps = Math.pow(2.0, -52.0);

    /** Estimate of the absolute error at time. */
    private double[] estimatedError = null;

    /** Number of terms in the Laguerre expansion. */
    private int nLaguerre;

    /** Free parameters in Weeks method sig and b may be estimated by wpar1 and wpar2 sig > sig_0. */
    private double sig;

    /** DOCUMENT ME! */
    private double sig0;

    /** DOCUMENT ME! */
    private double sigmax;

    /** DOCUMENT ME! */
    private double sigOpt;

    /**
     * Independent variable for which the inverse Laplace transform has to be computed. In running the inverse Laplace
     * transform the array can have 1 or multiple points. In calculating the optimal bOpt and sigOpt the time array can
     * only have 1 point.
     */
    private double[] time;

    /** resulting vector of real-space values. */
    private double[] timeFunction = null;

    /** DOCUMENT ME! */
    private double tolb = 1.0e-6;

    /** DOCUMENT ME! */
    private double tols = 1.0e-6;

    private boolean do_werr2t = false;
    private double dSig = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    // Examples are given below:
    /* private void runLapTestWeeks() {
     *    // This routine tests the numerical inverse laplace transform of a   // function in InverseLaplace with an
     * analytically known inverse.  The   // function to be transformed is exp(a*t)   // parameter a in exp(a*t)
     * double a = -1.0;   int n = 20;   // vector of times to invert for   double[] t = new double[n];   // true value
     * of the function exp(a*t)   double[] ftrue = new double[n];   int i;   FitExpModelWeeks lmod;   int nLaguerre =
     * 16;   double sse = 0;   double diff;   double rms;   double b = 1.0;   double sig = 1.0;
     *
     * double[] timeFunction = null;
     *
     *
     * for (i = 0; i < n; i++) {     t[i] = (i+1)*0.1;     ftrue[i] = Math.exp(a*t[i]);   }
     *
     * lmod = new FitExpModelWeeks(t, nLaguerre, sig, b);   lmod.driver();   timeFunction = lmod.getTimeFunction();
     * for ( i = 0; i < n; i++ ) {       Preferences.debug(               "time = " + t[i] + " routineFunction = " +
     * timeFunction[i] + " trueFunction = " + ftrue[i] + "\n" );   }
     *
     * sse = 0.0;   for (i = 0; i < n; i++) {     diff = timeFunction[i] - ftrue[i];     sse = sse + diff*diff;   }
     * rms = Math.sqrt(sse/(n-1));   Preferences.debug("rms error = " + rms + "\n");
     *
     *} */

    /*class FitExpModelWeeks extends InverseLaplaceWeeks {
     *
     * public FitExpModelWeeks(double time[], int nLaguerre, double sig,                          double b) {
     * super(time, nLaguerre, sig, b);  }
     *
     * public double[][] fitToLaplace( double realS, double[] imagS ) {      // The Laplace transform of exp(at) =
     * 1/(p-a)      double[][] ans = new double[imagS.length][2];      double a = -1.0;      int i;      double denom;
     *    double realDenom;      realDenom = ( realS - a ) * ( realS - a );      for ( i = 0; i < imagS.length; i++ ) {
     *         denom = realDenom + imagS[i] * imagS[i];          // real part          ans[i][0] = ( realS - a ) /
     * denom;          ans[i][1] = -imagS[i] / denom;      }      return ans;  }
     *
     * }*/

    /* private void runLapTestWeeks() {
     *    // This routine tests the numerical inverse laplace transform of a   // function in InverseLaplace with an
     * analytically known inverse.   // number of times to invert for   int n = 1;   // vector of times to invert for
     * double[] t = new double[n];   // true value of the function   double[] ftrue = new double[n];   int i;
     * FitModelWeeks3 lmod3;   double sig0 = 0.0;   double sigmax = 30.0;   double bmax = 30.0;   int nLaguerre = 32;
     * double tols = 1.0e-9;   double tolb = 1.0e-9;
     *
     * double b = 1.0;   double sig = 1.0;
     *
     *
     * double[] timeFunction = null;
     *
     *
     *
     * t[0] = 1.0;   ftrue[0] = 2.0 * Math.exp(-4.0/t[0])/Math.sqrt(Math.PI * t[0] *t[0] * t[0]);   lmod3 = new
     * FitModelWeeks3(t, nLaguerre, sig0, sigmax, bmax, tols, tolb);   lmod3.wpar2();   b = lmod3.getBOpt();   sig =
     * lmod3.getSigOpt();   Preferences.debug("b = " + b + " sig = " + sig);
     *
     * lmod3 = new FitModelWeeks3(t, nLaguerre, sig, b);   lmod3.driver();   timeFunction = lmod3.getTimeFunction();
     * for ( i = 0; i < n; i++ ) {       Preferences.debug(               "time = " + t[i] + " routineFunction = " +
     * timeFunction[i] + " trueFunction = " + ftrue[i] + "\n" );   }
     *
     *} */


    /*class FitModelWeeks3 extends InverseLaplaceWeeks {
     *
     * public FitModelWeeks3( double time[], int nLaguerre, double sig0,                          double sigmax, double
     * bmax,                          double tols, double tolb ) {     super( time, nLaguerre, sig0, sigmax, bmax, tols,
     * tolb ); }
     *
     * public FitModelWeeks3(double time[], int nLaguerre, double sig,                         double b) {   super(time,
     * nLaguerre, sig, b); }
     *
     * public double[][] fitToLaplace( double realS, double[] imagS ) {     // The Laplace transform of exp(at) = 1/(p-a)
     *    double[][] ans = new double[imagS.length][2];     int i;     double realPart;     double imagPart;     double
     * br[] = new double[1];     double bi[] = new double[1];     for ( i = 0; i < imagS.length; i++ ) {
     * zsqrt(realS, imagS[i], br, bi);         realPart = -4.0 * br[0];         imagPart = -4.0 * bi[0];
     * zexp(realPart, imagPart, br, bi);         // real part         ans[i][0] = br[0];         ans[i][1] = bi[0];
     * }     br = null;     bi = null;     return ans; }}*/

    /**
     * This constructor is used with driver.
     *
     * @param  time       double[]
     * @param  nLaguerre  int
     * @param  sig        double
     * @param  b          double
     */
    public InverseLaplaceWeeks(double[] time, int nLaguerre, double sig, double b) {
        this.time = time;
        this.nLaguerre = nLaguerre;
        this.sig = sig;
        this.b = b;
    }

    /**
     * This constructor is used with wpar2.
     *
     * @param  time       double[]
     * @param  nLaguerre  int
     * @param  sig0       double
     * @param  sigmax     double
     * @param  bmax       double
     * @param  tols       double
     * @param  tolb       double
     */
    public InverseLaplaceWeeks(double[] time, int nLaguerre, double sig0, double sigmax, double bmax, double tols,
                               double tolb) {
        this.time = time;
        this.nLaguerre = nLaguerre;
        this.sig0 = sig0;
        this.sigmax = sigmax;
        this.bmax = bmax;
        this.tols = tols;
        this.tolb = tolb;

        if (time.length != 1) {
            MipavUtil.displayError("The time array may only contain 1 point in b and sig calculation");
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * The second index is 0 for the real part and 1 for the imaginary part.
     *
     * @param   realS  DOCUMENT ME!
     * @param   imagS  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public abstract double[][] fitToLaplace(double realS, double[] imagS);

    /**
     * driver.
     */
    public void driver() {
        int M;
        int i;
        double[] a1 = new double[nLaguerre];
        double[] a2 = new double[nLaguerre];
        double[][] a;
        double sa1 = 0.0;
        double sa2 = 0.0;
        double[] L;
        double[] timeMat = new double[time.length];
        estimatedError = new double[time.length];
        timeFunction = new double[time.length];
        M = 2 * nLaguerre;

        // Compute the coefficients
        a = wcoef(M, sig, b);

        for (i = 0; i < nLaguerre; i++) {
            a1[i] = a[i + (2 * nLaguerre)][0];

            // Compute the sum of |a_n|, n = 0,...,nLaguerre-1
            sa1 = sa1 + Math.abs(a1[i]);
            a2[i] = a[i + (3 * nLaguerre)][0];

            // Compute the sum of |a_n|, n = nLaguerre,...,2*nLaguerre-1
            sa2 = sa2 + Math.abs(a2[i]);
        }

        for (i = 0; i < time.length; i++) {
            timeMat[i] = 2.0 * b * time[i];
        }

        // Evaluate the Laguerre series
        L = laguer(a1, timeMat);

        for (i = 0; i < time.length; i++) {

            // Evaluate Weeks expansion
            timeFunction[i] = L[i] * Math.exp((sig - b) * time[i]);

            // Compute error estimate
            estimatedError[i] = Math.exp(sig * time[i]) * (sa2 + (eps * sa1));
        }
    }

    /**
     * getBOpt.
     *
     * @return  bOpt
     */
    public double getBOpt() {
        return bOpt;
    }

    /**
     * getEstimatedError.
     *
     * @return  estimatedError
     */
    public double[] getEstimatedError() {
        return estimatedError;
    }

    /**
     * getSigOpt.
     *
     * @return  sigOpt
     */
    public double getSigOpt() {
        return sigOpt;
    }


    /**
     * getTimeFunction.
     *
     * @return  timeFunction
     */
    public double[] getTimeFunction() {
        return timeFunction;
    }

    /**
     * DOCUMENT ME!
     */
    public void wpar2() {
        double[] X = new double[2];
        ax2 = sig0;
        bx2 = (sig0 + sigmax) / 2.0;
        cx2 = sigmax;
        boolean do_werr2tSave = do_werr2t;
        do_werr2t = false;
        Brent.search( ax2, bx2, cx2, X, this, tols);
        sigOpt = X[0];
        do_werr2t = do_werr2tSave;

        ax = 0;
        bx = bmax / 2.0;
        cx = bmax;
        do_werr2tSave = do_werr2t;
        do_werr2t = true;
        dSig = sigOpt;
        Brent.search( ax, bx, cx, X, this, tolb);
        bOpt = X[0];
        do_werr2t = do_werr2tSave;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   a  DOCUMENT ME!
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] laguer(double[] a, double[] x) {

        // The function laguer evaluates a Laguerre polynomial expansion
        // with coefficients a at the value x, by using Clenshaw's algorithm.
        // The vector x cold be a scalar or vector, asnd the coefficients in
        // the vector a are ordered in increasing order of the index
        int N = a.length - 1;
        double[] unp1 = new double[x.length];
        double[] un = new double[x.length];
        double[] unm1 = new double[x.length];
        int i;
        int j;

        for (i = 0; i < x.length; i++) {
            unp1[i] = 0.0;
            un[i] = a[N];
        }

        for (j = N; j >= 1; j--) {

            for (i = 0; i < x.length; i++) {
                unm1[i] = ((1.0 / j) * ((2.0 * j) - 1 - x[i]) * un[i]) - (j / (j + 1.0) * unp1[i]) + a[j - 1];
                unp1[i] = un[i];
                un[i] = unm1[i];
            }
        }

        return unm1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   N    DOCUMENT ME!
     * @param   sig  DOCUMENT ME!
     * @param   b    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[][] wcoef(int N, double sig, double b) {

        // The function wcoef computes the Weeks coefficients of
        // the Laplace Function by using the midpoint version of fft
        int M = 2 * N;
        int[] nGrid = new int[M];
        int i;
        double h = Math.PI / N;
        double[] th = new double[M];
        double[] y = new double[M];
        double[][] ff;
        double[] fftR = new double[M];
        double[] fftI = new double[M];
        double tempR;
        double tempI;
        int iComp;
        FFTUtilityEP fft;
        double[][] a = new double[M][2];
        double arg;

        for (i = 0; i < M; i++) {
            nGrid[i] = -N + i;
            th[i] = h * (nGrid[i] + 0.5);
            y[i] = b / Math.tan(th[i] / 2.0);
        }

        ff = fitToLaplace(sig, y);

        for (i = 0; i < M; i++) {
            fftR[i] = (ff[i][0] * b) - (ff[i][1] * y[i]);
            fftI[i] = (ff[i][0] * y[i]) + (ff[i][1] * b);
        }

        for (i = 0; i < N; i++) {
            tempR = fftR[i];
            tempI = fftI[i];
            iComp = i + N;
            fftR[i] = fftR[iComp];
            fftI[i] = fftI[iComp];
            fftR[iComp] = tempR;
            fftI[iComp] = tempI;
        }

        fft = new FFTUtilityEP(fftR, fftI, 1, M, 1, -1, FFTUtility.FFT);
        fft.run();
        fft.finalize();
        fft = null;

        for (i = 0; i < N; i++) {
            tempR = fftR[i];
            tempI = fftI[i];
            iComp = i + N;
            fftR[i] = fftR[iComp];
            fftI[i] = fftI[iComp];
            fftR[iComp] = tempR;
            fftI[iComp] = tempI;
        }

        for (i = 0; i < M; i++) {
            a[i][0] = fftR[i] / M;
            a[i][1] = fftI[i] / M;
            arg = nGrid[i] * h / 2.0;
            tempR = (Math.cos(arg) * a[i][0]) + (Math.sin(arg) * a[i][1]);
            a[i][1] = (Math.cos(arg) * a[i][1]) - (Math.sin(arg) * a[i][0]);
            a[i][0] = tempR;
        }

        return a;

    }

    /**
     * DOCUMENT ME!
     *
     * @param   sig  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double werr2e(double sig) {
        // werr2e computes the error bound = truncation plus conditioning
        // error on the optimal curve b = b(sig)

        int M;
        double[][] a;
        double[][] a1 = new double[nLaguerre][2];
        double[][] a2 = new double[nLaguerre][2];
        double sa1 = 0.0;
        double sa2 = 0.0;
        int i;
        double error;

        ax = 0;
        bx = bmax / 2.0;
        cx = bmax;
        boolean do_werr2tSave = do_werr2t;
        do_werr2t = true;
        double dSigSave = dSig;
        dSig = sig;
        double[] X = new double[2];
        Brent.search( ax, bx, cx, X, this, tolb);        
        bOpt = X[0];
        do_werr2t = do_werr2tSave;
        dSig = dSigSave;
        
        b = bOpt;

        M = 2 * nLaguerre;
        a = wcoef(M, sig, b);

        for (i = 0; i < nLaguerre; i++) {
            a1[i][0] = a[i + (2 * nLaguerre)][0];
            a1[i][1] = a[i + (2 * nLaguerre)][1];
            sa1 = sa1 + zabs(a1[i][0], a1[i][1]);
            a2[i][0] = a[i + (3 * nLaguerre)][0];
            a2[i][1] = a[i + (3 * nLaguerre)][1];
            sa2 = sa2 + zabs(a2[i][0], a2[i][1]);
        } // for (i = 0; i < nLaguerre; i++)

        error = (sig * time[0]) + Math.log(sa2 + (eps * sa1));

        return error;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   b    DOCUMENT ME!
     * @param   sig  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double werr2t(double b, double sig) {

        // The function werr2t computes the estimate for the truncation error
        // in the Weeks method
        int M = 2 * nLaguerre;
        double[][] a;
        int i;
        double sa2 = 0.0;

        a = wcoef(M, sig, b);

        for (i = 3 * nLaguerre; i < (4 * nLaguerre); i++) {
            sa2 = sa2 + zabs(a[i][0], a[i][1]);
        }

        return sa2;
    }

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

    @Override
    public double eval(double x) {
    	if ( do_werr2t )
    		return werr2t(x, dSig);
    	return werr2e(x);
    }

}
