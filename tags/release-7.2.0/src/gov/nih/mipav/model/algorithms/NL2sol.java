package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

import java.util.Calendar;
import java.util.Formatter;

/**
 * This is a port of the source code nl2sol.f90 and the nl2sol_prb2.f90 extensive set of tests from FORTRAN into Java.
 * Unconstrained non-linear least squares solver algorithm.
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
public abstract class NL2sol {
	
private boolean testMode = false;
	
	// emin, the smallest exponent E for double precision, is I1MACH(15)
    // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022) = 2.225073858507201E-308
    // D1MACH(1) is the smallest normalized number, which preserves the
    // full precision of the mantissa.
    // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number = 4.9E-324,
    // which preserves only a portion of the fraction's precision.
	/** 2**-1022 = D1MACH(1). */
    private double tiny = Math.pow(2, -1022);
    
    // epsilon = D1MACH(4)
    // Machine epsilon is the smallest positive epsilon such that
    // (1.0 + epsilon) != 1.0.
    // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
    // epsilon = 2.2204460e-16
    // epsilon is called the largest relative spacing
    private double epsilon = Math.pow(2, -52);
    private double huge = Double.MAX_VALUE; 
    
    private double expmin_calcjTest = 0.0;
    private double uftolg_calcjTest = 0.0;
    private double ukow_calcjTest[] = new double[] {0.0, 4.0, 2.0, 1.0, 0.5, 
            0.25, 0.167, 0.125, 0.1, 0.0833, 0.0714, 0.0625};
    
    private double expmax_calcrTest = 0.0;
	private double expmin_calcrTest = 0.0;
	private double uftolg_calcrTest = 0.0;
	private double ukow_calcrTest[] = new double[]{0.0, 4.0,   2.0,   1.0, 0.5,    0.25, 
		    0.167, 0.125, 0.1, 0.0833, 0.0714, 0.0625};
	private double ybard_calcrTest[] = new double[]{ 0.0, 0.14, 0.18, 0.22, 0.25, 0.29,
                   0.32, 0.35, 0.39, 0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39};
    private double ykow_calcrTest[] = new double[]{ 0.0, 1.957E-01, 1.947E-01, 1.735E-01, 1.600E-01, 8.44E-02,
             6.27E-02,  4.56E-02,  3.42E-02,  3.23E-02,  2.35E-02, 2.46E-02};
    private double ymeyer_calcrTest[] = new double[]{ 0.0, 3.478E+04, 2.861E+04, 2.365E+04, 1.963E+04, 1.637E+04,
      1.372E+04, 1.154E+04, 9.744E+03, 8.261E+03, 7.030E+03, 6.005E+03, 5.147E+03, 4.427E+03, 3.820E+03, 3.307E+03,
       2.872E+03};
    private double yosb1_calcrTest[] = new double[] {0.0,
        8.44E-01, 9.08E-01, 9.32E-01, 9.36E-01, 9.25E-01,
        9.08E-01, 8.81E-01, 8.50E-01, 8.18E-01, 7.84E-01,
        7.51E-01, 7.18E-01, 6.85E-01, 6.58E-01, 6.28E-01,
        6.03E-01, 5.80E-01, 5.58E-01, 5.38E-01, 5.22E-01,
        5.06E-01, 4.90E-01, 4.78E-01, 4.67E-01, 4.57E-01,
        4.48E-01, 4.38E-01, 4.31E-01, 4.24E-01, 4.20E-01,
        4.14E-01, 4.11E-01, 4.06E-01};
    private double yosb2_calcrTest[] = new double[] {0.0,
        1.366, 1.191, 1.112, 1.013, 9.91E-01,
        8.85E-01,  8.31E-01,  8.47E-01,  7.86E-01,  7.25E-01,
        7.46E-01,  6.79E-01,  6.08E-01,  6.55E-01,  6.16E-01,
        6.06E-01,  6.02E-01,  6.26E-01,  6.51E-01,  7.24E-01,
        6.49E-01,  6.49E-01,  6.94E-01,  6.44E-01,  6.24E-01,
        6.61E-01,  6.12E-01,  5.58E-01,  5.33E-01,  4.95E-01,
        5.00E-01,  4.23E-01,  3.95E-01,  3.75E-01,  3.72E-01,
        3.91E-01,  3.96E-01,  4.05E-01,  4.28E-01,  4.29E-01,
        5.23E-01,  5.62E-01,  6.07E-01,  6.53E-01,  6.72E-01,
        7.08E-01,  6.33E-01,  6.68E-01,  6.45E-01,  6.32E-01,
        5.91E-01,  5.59E-01,  5.97E-01,  6.25E-01,  7.39E-01,
        7.10E-01,  7.29E-01,  7.20E-01,  6.36E-01,  5.81E-01,
        4.28E-01,  2.92E-01,  1.62E-01,  9.8E-02,   5.4E-02}; 
	
    
	private double sqteta_dotprd = 0.0;
	
	private double dgxfac_gqtstp = 0.0;
	
	private double ix_lsvmin = 2;
	
	private double hlim_nl2sno = 0.0;
	
	private double big_parchk = 0.0;
	private double teensy_parchk = 1.0;
	
	private double rktol_qrfact = 0.0;
	private double ufeta_qrfact = 0.0;
	
	private double sqteta_v2norm = 0.0;
	
	private int n;
	private int p;
	private double x[];
	private int iv[];
	private double v[];
	private boolean useAnalyticJacobian;
	private int uiparm[];
	private double urparm[];
	private boolean detailedSummaryPrinting = false;
	private final int prob_max = 60;
	private String irc[];
	private int is[][];
	private String name[];
	private int nout;
	private int nprob;
	private double rs[][];
	private int nex; // the index of the test problem
	
	public NL2sol() {
		testMode = true;
		main();
	}
	
	
	private void main() {
		
		/* MAIN is the main program for NL2SOL_PRB2.
		!
		!  Discussion:
		!
		!    NL2SOL_PRB2 runs NL2SOL on various test problems.
		!
		!    This main program calls NLTEST to run NL2SOL, the nonlinear
		!    least-squares solver, on various test problems.
		!
		!    The test problems used are from the references.  Some additional 
		!    test problems were suggested by Jorge More.  Calls passing 
		!    these problems to NLTEST have been commented out (since 
		!    there are enough other problems), but not removed, since 
		!    they may be of interest to other researchers.
		!
		!  Modified:
		!
		!    02 April 2006
		!
		!  Reference:
		!
		!    K M Brown,
		!    A Quadratically Convergent Newton-like Method Based upon 
		!    Gaussian Elimination,
		!    SIAM Journal on Numerical Analysis,
		!    Volume 6, pages 560-569, 1969.
		!
		!    John Dennis, David Gay and Roy Welsch,
		!    Algorithm 573: An Adaptive Nonlinear Least-squares Algorithm,
		!    ACM Transactions on Mathematical Software,
		!    Volume 7, Number 3, pages 348-368, 1981.
		!
		!    Philip Gill and Walter Murray,
		!    Algorithms for the Solution of the Non-linear Least-squares Problem, 
		!    SIAM Journal on Numerical Analysis,
		!    Volume 15, Number 5, pages 977-991, 1978.
		!
		!    R R Meyer, 
		!    Theoretical and Computational Aspects of Nonlinear Regression, 
		!    in Nonlinear Programming,
		!    edited by J B Rosen, O L Mangasarian, and K Ritter,
		!    pages 465-486,
		!    Academic Press, New York, 1970.
		*/
		irc = new String[prob_max+1];
		is = new int[7][prob_max+1];
		iv = new int[101];
		int j;
		String jtyp[] = new String[3];
		int k;
		int mxfcsv;
		int mxitsv;
		name = new String[prob_max+1];
		@SuppressWarnings("unused")
		int pu;
		rs = new double[6][prob_max+1];
		boolean rstart;
		v = new double[5001];
		String version;
		int xscal1;
		int xscal2;
	    Preferences.debug("Test the NL2sol package\n", Preferences.DEBUG_ALGORITHM);
	    timestamp();
	    rstart = false;
	    jtyp[1] = " ";
	    jtyp[2] = "*";
	    version = "NL2SOL version 2.2";
	    // Assign default values to iv and v
	    dfault(iv, v);
	    nout = 6;
	    if (detailedSummaryPrinting) {
	    	iv[21] = 1;
	    }
	    else {
	    	iv[21] = 0;
	    }
	    pu = iv[21];
	    nprob = 0;
	    
	    // Rosenbrock OK
	    // Rosenbrock correct answer is f = 0 at (x1 = 1, x2 = 1)
	    // For standard starting point Java port gave:
	    // niter = 18 nf = 29 ng = 19 Absolute function convergence final f = 0.0
	    // Original FORTRAN gave:
	    // niter = 18 nf = 29 ng = 19 Absolute convergence final f = 0.0
	    // For 10 times the standard starting point Java port gave:
	    // niter = 45 nf = 65 ng = 46 Absolute function convergence final f = 0.0
	    // Original FORTRAN gave:
	    // niter = 45 nf = 66 ng = 46 Absolute function convergence final f = 0.0
	    // For 100 times the standard starting point Java port gave:
	    // niter = 113 nf = 142 ng = 114 Absolute function convergence final f = 0.0
	    // Original FORTRAN gave:
	    // niter = 113 nf = 143 ng = 114 Absolute function convergence final f = 0.0
	    
	    n = 2;
	    p = 2;
	    nex = 1;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Rosenbrock", rstart, xscal1, xscal2);
	    
	    // Helix OK
	    // Helix correct answer is f = 0 at (x1 = 1, x2 = 0 x3 = 0)
	    // For standard starting point Java port gave:
	    // niter = 10 nf = 13 ng = 11 Absolute function convergence final f = 2.76021E-21
	    // Original FORTRAN gave:
	    // niter = 10 nf = 13 ng = 11 Absolute function convergence final f = 2.76017E-21
	    // For 10 times the standard starting point Java port gave:
	    // niter = 15 nf = 19 ng = 16 Absolute function convergence final f = 1.19609E-21
	    // Original FORTRAN gave:
	    // niter = 15 nf = 19 ng = 16 Absolute convergence final f = 1.19609E-21
	    // For 100 times the standard starting point Java port gave:
	    // niter = 21 nf = 33 ng = 22 Absolute function convergence final f = 1.03680E-29
	    // Original FORTRAN gave:
	    // niter = 21 nf = 33 ng = 22 Absolute function convergence final f = 1.03668E-29
	    n = 3;
	    p = 3;
	    nex = 2;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Helix", rstart, xscal1, xscal2);
	    
	    // Singular OK
	    // Singular function
	    // For standard starting point Java port gave:
	    // niter = 19 nf = 20 ng = 20 Absolute function convergence final f = 1.06541E-21
	    // Original FORTRAN gave:
	    // niter = 19 nf = 20 ng = 20 Absolute function convergence final f = 1.06541E-21
	    // For 10 times the standard starting point Java port gave:
	    // niter = 25 nf = 34 ng = 26 Absolute function convergence final f = 3.19823E-21
	    // Original FORTRAN gave:
	    // niter = 25 nf = 35 ng = 26 Absolute function convergence final f = 3.19823E-21
	    // For 100 times the standard starting point Java port gave:
	    // niter = 30 nf = 53 ng = 31 Absolute function convergence final f = 1.58785E-21
	    // Original FORTRAN gave:
	    // niter = 30 nf = 53 ng = 31 Absolute function convergence final f = 1.58785E-21
	    
	    n = 4;
	    p = 4;
	    nex = 3;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Singular", rstart, xscal1, xscal2);
	    
	    // Woods OK.
	    // Woods
	    // For the standard starting point Java port gave:
	    // niter = 52 nf = 98 ng = 53  X-convergence final f = 0.0
	    // Original FORTRAN gave:
	    // niter = 53 nf = 108 ng = 54 X-convergence final f = 0.0
	    // For 10 times the original starting point Java port gave:
	    // niter = 50 nf = 97 ng = 51 X-convergence final f = 0.0
	    // Original FORTRAN gave:
	    // niter = 50 nf = 98 ng = 51 X-convergence final f = 0.0
	    // x1 = -0.97188, x2 = 0.954692, x3 = -0.965661, x4 = 0.943790
	    // For 100 times the original starting point Java port gave:
	    // niter = 30 nf = 45 ng = 31 Absolute function convergence final f = 6.31807E-21
	    // Original FORTRAN gave:
	    // niter = 30 nf = 45 nf = 31 Absolute function convergence final f = 6.31798E-21
	    n = 7;
	    p = 4;
	    nex = 4;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Woods", rstart, xscal1, xscal2);
	    
	    // Zangwill OK
	    // Zangwill f = 0 at origin
	    // For original starting point the Java port gave:
	    // niter = 2 nf = 3 ng = 3 Absolute function convergence final f = 9.38744E-28
	    // Original FORTRAN gave:
	    // niter = 2 nf = 3 ng = 3 Absolute function convergence final f = 9.38744E-28
	    n = 3;
	    p = 3;
	    nex = 5;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 1;
	    nltest("Zangwill", rstart, xscal1, xscal2);
	    
	    // Engvall OK
	    // Engvall
	    // For original starting point the Java port gave:
	    // niter = 12 nf = 19 ng = 13 Absolute convergence final f = 6.88422E-21
	    // Original FORTRAN gave:
	    // niter = 12 nf = 19 ng = 13 Absolute function convergence final f = 6.88422E-21
	    // x1 = 1.29206E-7, x2 = -1.19195E-7, x3 = 1.0
	    // For 10 times the original starting point the Java port gave:
	    // niter = 16 nf = 23 ng = 17 Absolute convergence final f = 8.78728E-26
	    // Original FORTRAN gave:
	    // niter = 16 nf = 23 ng = 17 Absolute function convergence final f = 8.78728E-26
	    // For 100 times the original starting point the Java port gave:
	    // niter = 30 nf = 44 nf = 31 X-convergence final f = 1.00980E-28
	    // Original FORTRAN gave:
	    // niter = 30 nf = 44 ng = 31 X-convergence final f = 6.16298E-33
	    n = 5;
	    p = 3;
	    nex = 6;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Engvall", rstart, xscal1, xscal2);
	    
	    // Branin OK
	    // Branin correct f = 0 at x1 = 0, x2 = 0
	    // For the standard starting point Java port gave:
	    // niter = 1 nf = 2 ng = 2 Absolute function convergence final f = 8.53732E-30
	    // Original FORTRAN gave:
	    // niter = 1 nf = 2 ng = 2 Absolute function convergence final f = 8.53732E-30
	    // For 10 times the standard starting point Java port gave:
	    // niter = 26 nf = 43 ng = 27 Absolute function convergence final f = 1.31337E-27
	    // Original FORTRAN gave:
	    // niter = 26 nf = 43 ng = 27 Absolute function convergence final f = 1.31337E-27
	    // For 100 times the standard starting point Java port gave:
	    // niter = 36 nf = 58 ng = 37 Absolute function convergence final f = 2.68109E-31
	    // Original FORTRAN gave:
	    // niter = 36 nf = 58 ng = 37 Absolute function convergence final f = 2.68109E-31
	    n = 2;
	    p = 2;
	    nex = 7;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Branin", rstart, xscal1, xscal2);
	    
	    // Beale correct for f = 0 for x1 = 3, x2 = 0.5
	    // For standard starting point Java port gave:
	    // niter = 8 nf = 10 ng = 9 Absolute function convergence final f = 8.90567E-27
	    // Original FORTRAN gave:
	    // niter = 8 nf = 10 ng = 9 Absolute convergence final f = 8.94590E-27
	    // For 10 times standard starting point Java port gave:
	    // niter = 5 nf = 6 ng = 6 Absolute function convergence final f = 1.47766E-22
	    // Original FORTRAN gave:
	    // niter = 5 nf = 6 ng = 6 Absolute convergence final f = 1.47825E-22
	    n = 3;
	    p = 2;
	    nex = 8;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 2;
	    nltest("Beale", rstart, xscal1, xscal2);
	    
	    // Cragg and Levy OK
	    // Cragg and Levy
	    // For standard starting point Java port gave:
	    // niter = 22 nf = 24 ng = 23 Absolute function convergence final f = 2.16879E-21
	    // Original FORTRAN gave:
	    // niter = 22 nf = 24 ng = 23 Absolute function convergence final f = 2.16879E-21
	    // For 10 times standard starting point Java port gave:
	    // niter = 85 nf = 178 ng = 86 Relative function convergence final f = 2.54185E4
	    // Original FORTRAN gave:
	    // niter = 78 nf = 181 ng = 79 Relative function convergence final f = 1.60583E5
	    
	    n = 5;
	    p = 4;
	    nex = 9;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 2;
	    nltest("Cragg", rstart, xscal1, xscal2);
	    
	    // Box OK
	    // Box correct solutions f = 0 at (1,10,1), (10,1,-1),
	    // and wherever x1 = x2 and x3 = 0
	    // For standard starting point with Java port:
	    // niter = 6 nf = 7 ng = 7 X-convergence final f = 3.08149E-32
	    // Original FORTRAN gave:
	    // niter = 6 nf = 7 ng = 7 X-convergence final f = 1.84889E-32
	    // For 10 times standard starting point with Java port:
	    // niter = 10 nf = 12 nf 10 Singular convergence final f = 0.03779437
	    // Original FORTRAN gave:
	    // niter = 10 nf = 12 ng = 10 Singular convergence final f = 0.0377944
	    n = 10;
	    p = 3;
	    nex = 10;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 2;
	    nltest("Box", rstart, xscal1, xscal2);
	    
	    // Davidon 1 OK
	    // Davidon 1
	    // For standard starting point with Java port:
	    // niter = 15 nf = 20 ng = 15 Function evaluation limit final f = 8.14061E-7
	    // Original FORTRAN gave:
	    // niter = 15 nf = 20 ng = 15 Function evaluation limit final f = 8.14061E-7
	    n = 15;
	    p = 15;
	    nex = 11;
	    useAnalyticJacobian = true;
	    mxfcsv = iv[17];
	    mxitsv = iv[18];
	    iv[17] = 20;
	    iv[18] = 15;
	    xscal1 = 1;
	    xscal2 = 1;
	    nltest("Davidon1", rstart, xscal1, xscal2);
	    
	    // Freudenstein and Roth OK
	    // Freudenstein and Roth correct for 
	    // f = 0 for x1 = 5, x2 = 4
	    // f = 24.4921... for x1 = 11.41..., x2 = -0.8968...
	    // For standard starting point with Java port:
	    // niter = 7 nf = 9 ng = 8 Relative function convergence final f = 24.4921
	    // Original FORTRAN gave:
	    // niter = 7 nf = 9 ng = 8 Relative function convergence final f = 24.4921
	    // For 10 times standard starting point with Java port:
	    // niter = 17 nf = 29 ng = 18 X- and relative function convergence final f = 24.4921
	    // Original FORTRAN gave:
	    // niter = 17 nf = 29 ng = 18 X- and relative function convergence final f = 24.4921
	    // For 100 times the standard starting point with Java port:
	    // niter = 26 nf = 42 ng = 27 X- and relative function convergence final f = 24.4921
	    // Original FORTRAN gave:
	    // niter = 26 nf = 42 ng = 27 X- and relative function convergence final f = 24.4921
	    n = 2;
	    p = 2;
	    nex = 12;
	    useAnalyticJacobian = true;
	    iv[17] = mxfcsv;
	    iv[18] = mxitsv;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Freudenstein", rstart, xscal1, xscal2);
	    
	    // Watson6 OK
	    // Watson6 correct for f = 1.14383E-3 
	    // For standard starting point with Java port:
	    // niter = 10 nf = 13 ng = 10 X- and relative function convergence final f = 1.143835E-3
	    // Original FORTRAN gave:
	    // niter = 10 nf = 13 ng = 11 X- and relative function convergence final f = 1.14384E-3
	    n = 31;
	    p = 6;
	    nex = 13;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 1;
	    nltest("Watson6", rstart, xscal1, xscal2);
	    
	    // Watson9 OK
	    // Watson9 correct for f = 6.9988E-7
	    // For standard starting point with Java port:
	    // niter = 9 nf = 15 ng = 9 X- and relative function convergence final f = 6.9988E-7
	    // Original FORTRAN gave:
	    // niter = 10 nf = 15 ng = 10 X- and relative function convergence final f = 6.99880E-7
	    n = 31;
	    p = 9;
	    nex = 14;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 1;
	    nltest("Watson9", rstart, xscal1, xscal2);
	    
	    // Watson12 OK
	    // Watson12 correct for f = 2.36119E-10
	    // At standard starting point with Java port:
	    // niter = 14 nf = 16 ng = 14 Relative function convergence final f = 2.36119E-10
	    // Original FORTRAN gave:
	    // niter = 14 nf = 16 ng = 14 X- and relative function convergence final f = 2.36119E-10
	    n = 31;
	    p = 12;
	    nex = 15;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 1;
	    nltest("Watson12", rstart, xscal1, xscal2);
	    
	    // Watson20 OK
	    // Watson20
	    // At standard starting point with Java port:
	    // niter = 15 nf = 19 ng = 16 Iteration limit final f = 6.99050E-17
	    // Original FORTRAN gave:
	    // niter = 15 nf = 18 ng = 16 Iteration limit final f = 5.62350E-17
	    // At 10 times standard starting point with Java port:
	    // niter = 15 nf = 19 ng = 16 Iteration limit final f = 6.99050E-17
	    // Original FORTRAN gave:
	    // niter = 15 nf = 18 ng = 16 Iteration limit final f = 5.62350E-17
	    // At 100 times standard starting point with Java port:
	    // niter = 15 nf = 19 ng = 16 Iteration limit final f = 6.99050E-17
	    // Original FORTRAN gave:
	    // niter = 15 nf = 18 ng = 16 Iteration limit final f = 5.62350E-17
	    n = 31;
	    p = 20;
	    nex = 16;
	    useAnalyticJacobian = true;
	    mxfcsv = iv[17];
	    iv[17] = 20;
	    mxitsv = iv[18];
	    iv[18] = 15;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Watson20", rstart, xscal1, xscal2 );
	    
	    // Chebyquad OK
	    // Chebyquad correct for 1.75843E-3
	    // At standard starting point Java port gave:
	    // niter = 16 nf = 25 ng = 17 Relative funciton convergence final f = 1.75844E-3
	    // Original FORTRAN gave
	    // niter = 16 nf = 25 ng = 17 Relative function convergence final f = 1.75844E-3
	    // At 10 times standard starting point Java port gave:
	    // niter = 57 nf = 88 ng = 58 X- and relative function convergence final f = 1.75844E-3
	    // Original FORTRAN gave
	    // niter = 57 nf = 88 ng = 58 X- and relative function convergence final f = 1.75844E-3
	    n = 8;
	    p = 8;
	    nex = 17;
	    useAnalyticJacobian = true;
	    iv[17] = mxfcsv;
	    iv[18] = mxitsv;
	    xscal1 = 1;
	    xscal2 = 2;
	    nltest("Chebyquad", rstart, xscal1, xscal2);
	    
	    // Brown and Dennis OK
	    // Brown and Dennis correct for f = 42911.1
	    // At standard starting point Java port gave:
	    // niter = 16 nf = 27 nf = 17  Relative function convergence final f = 42911.1
	    // Original FORTRAN gave:
	    // niter = 16 nf = 27 ng = 17 Relative function convergence final f = 4.29111E4
	    // At 10 times standard starting point Java port gave:
	    // niter = 16 nf = 30 ng = 17 Relative function convergence final f = 42911.1
	    // Original FORTRAN gave:
	    // niter = 16 nf = 30 ng = 17 Relative function convergence final f = 4.29111E4
	    // At 100 times standard starting point Java port gave:
	    // niter = 19 nf = 36 ng = 20 Relative function convergence final f = 42911.1
	    // Original FORTRAN gave:
	    // niter = 19 nf = 35 ng = 20 final Relative function convergence f = 4.29111E4
	    n = 20;
	    p = 4;
	    nex = 18;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Brown", rstart, xscal1, xscal2);
	    
	    // Bard both original FORTRAN and Java port incorrect at 10 times starting point
	    // Bard correct for f = 4.10743E-3
	    // Correct for 8.7143 for (x1 = 0.8406..., -infinity, -infinity)
	    // At standard starting point Java port gave:
	    // niter = 6 nf = 7 ng = 7 Relative function convergence final f = 4.1074386E-3
	    // Original FORTRAN gave:
	    // niter = 6 nf = 7 ng = 7 Relative function convergence final f = 4.10744E-3
	    // At 10 times the standard starting point Java port gave:
	    // niter = 47 nf = 110 ng = 47 Singular convergence final f = 5.73985E-2
	    // Original FORTRAN gave:
	    // niter = 52 nf = 116 ng = 52 Singular convergence final f = 5.73985E-2
	    // At 100 times the standard starting point Java port gave:
	    // niter = 37 nf = 94 ng = 38 Relative function convergence final f = 4.1074386E-3
	    // Original FORTRAN gave:
	    // niter = 36 nf = 92 ng = 37 Relative function convergence f = 4.10744E-3
	    n = 15;
	    p = 3;
	    nex = 19;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Bard", rstart, xscal1, xscal2);
	    
	    // Jennrich and Sampson OK
	    // Jennrich and Sampson correct answer f = 62.181... at x1 = x2 = 0.2578...
	    // At standard starting point Java port gave:
	    // niter = 12 nf = 15 ng = 13 Relative function convergence final f = 62.18109
	    // Original FORTRAN gave:
	    // niter = 12 nf = 15 ng = 13 Relative function convergence  final f = 62.1811
	    n = 10;
	    p = 2;
	    nex = 20;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 1;
	    nltest("Jennrich", rstart, xscal1, xscal2);
	    
	    // Kowalik and Osborne problem with both original and port at 10 times standard
	    // starting point
	    // Kowalik and Osborne correct for f = 1.537525E-4
	    // f = 5.1367...E-4 at x1 = +infinity, x2 = -14.07..., x3 = -infinity, x4 = -infinity
	    // At standard starting point Java port gave:
	    // niter = 9 nf = 11 ng = 10 Relative function convergence final f = 1.537528019E-4
	    // Original FORTRAN gave:
	    // niter = 9 nf = 11 ng = 10 Relative function convergence f = 1.53753E-4
	    // At 10 times the standard starting point Java port gave:
	    // niter = 85 nf = 182 ng = 86 Singular convergence final f = 5.13673E-4
	    // x1 = 6485.66, x2 = -14.0755, x3 = -2.93514E5, x4 = -1.83094E5
	    // Original FORTRAN gave:
	    // x1 = 6486.17, x2 = -14.0755, x3 = -2.93537E5, x4 = -1.83109E5
	    // niter = 86 nf = 185 ng = 86 Singular convergence final f = 5.13673E-4
	    // At 100 times the standard starting point Java port gave:
	    // niter = 67 nf = 110 ng = 68 Relative convergence final f = 1.537528019E-4
	    // Original FORTRAN gave:
	    // niter = 67 nf = 110 ng = 68 Relative function convergence f = 1.53753E-4
	    n = 11;
	    p = 4;
	    nex = 21;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Kowalik", rstart, xscal1, xscal2);
	    
	    // Osborne 1 correct for f = 2.732445E-5
	    // At standard starting point Java port gave:
	    // niter = 21 nf = 27 g = 22 Relative function convergence final f = 2.7324473487E-5
	    // Original FORTRAN gave:
	    // niter = 21 nf = 27 ng = 22 Relative function convergence final f = 2.73244E-5
	    n = 33;
	    p = 5;
	    nex = 22;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 1;
	    nltest("Osborne1", rstart, xscal1, xscal2);
	    
	    // Both Java port and original FORTRAN fail at 10 times standard starting point
	    // Osborne 2 correct for 2.00688...E-2
	    // At standard starting point Java port gave:
	    // niter = 15 nf = 17 ng = 16 X- and relative function convergence final f = 0.0200688681
	    // Original FORTRAN gave:
	    // niter = 15 nf = 17 ng = 16 X- and relative function convergence final f = 2.00689E-2
	    // At 10 times standard starting point Java port gave:
	    // niter = 11 nf = 26 ng = 12 Singular convergence final f = 0.89490679
	    // Original FORTRAN gave:
	    // niter = 11 nf = 26 ng = 12 Singular convergence final f = 0.894907
	    n = 65;
	    p = 11;
	    nex = 23;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 2;
	    nltest("Osborne2", rstart, xscal1, xscal2);
	    
	    // Madsen
	    // At standard starting point Java port gave:
	    // niter = 11 nf = 12 ng = 12 Relative function convergence final f = 0.386600
	    // Original FORTRAN gave:
	    // niter = 11 nf = 12 ng = 12 Relative function convergence final f = 0.386600
	    // At 10 times standard starting point Java port gave:
	    // niter = 15 nf = 25 ng = 16 Relative function convergence final f = 0.386600
	    // Original FORTRAN gave:
	    // niter = 15 nf = 25 ng = 16 Relative function convergence final f = 0.386600
	    // At 100 times standard starting point Java port gave:
	    // niter = 20 nf = 33 ng = 21 Relative function convergence final f = 0.386600
	    // Original FORTRAN gave:
	    // niter = 20 nf = 33 ng = 21 Relative convergence final f = 0.386600
	    n = 3;
	    p = 2;
	    nex = 24;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Madsen", rstart, xscal1, xscal2);
	    
	    // Meyer correct for f = 43.9729...
	    // At standard starting point Java port gave:
	    // niter = 194 nf = 314 ng = 195 X-convergence final f = 43.972927585
	    // Original FORTRAN gave:
	    // niter = 194 nf = 314 ng = 195 X-convergence final f = 43.9729
	    // At 10 times the standard starting point Java port gave:
	    // niter = 300 nf = 343 ng = 301 Iteration limit final f =  290670.4255
	    // Original FORTRAN gave:
	    // niter = 300 nf = 343 ng = 301 Iteration limit final f = 2.80274E5
	    // At 100 times the standard starting point Java port gave:
	    // niter = 300 nf = 378 ng = 301 Iteration limit final f = 1.3588368E8
	    // Original FORTRAN gave:
	    // niter = 300 nf = 378 ng = 301 Iteration limit final f = 1.37831E8
	    n = 16;
	    p = 3;
	    nex = 25;
	    useAnalyticJacobian = true;
	    iv[17] = 400;
	    iv[18] = 300;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Meyer", rstart, xscal1, xscal2);
	    
	    // Brown5 OK.
	    // Brown5, an example of BROWN_ALMOST_LINEAR
	    // Correct solution f = 0 at (alpha, ..., alpha, alpha**(1-n))
	    // where alpha satisfies n*alpha**n - (n+1)*alpha**(n-1) + 1 = 0
	    // in particular, alpha = 1
	    // f = 1 at (0,...,0,n+1)
	    // At the standard starting point Java port gave:
	    // niter = 5 nf = 8 ng = 6 Absolute convergence final f = 4.39173E-21
	    // Original FORTRAN gave:
	    // niter = 5 nf = 8 ng = 6 Absolute convergence final f = 4.39173E-21
	    // At 10 times the standard starting point Java port gave:
	    // niter = 14 nf = 26 ng = 15 X-convergence final f = 4.93038E-31
	    // Original FORTRAN gave:
	    // niter = 14 nf = 26 ng = 15 X-convergence final f = 4.93038E-31
	    // At 100 times the standard starting point Java port gave:
	    // niter = 29 nf = 72 ng = 30 X-convergence final f = 0.0
	    // Original FORTRAN gave:
	    // niter = 29 nf = 72 ng = 30 X-convergence final f = 0.0
	    n = 5;
	    p = 5;
	    nex = 26;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Brown5", rstart, xscal1, xscal2);
	    
	    // Brown10 OK for both only at standard starting point and 10 * std.
	    // Brown10, an example of BROWN_ALMOST_LINEAR
	    // At standard starting point Java port gave:
	    // niter = 5 nf = 7 ng = 6 Absolute convergence final f = 9.927118E-21
	    // Original FORTRAN gave:
	    // niter = 5 nf = 7 ng = 6 Absolute convergence final f = 9.92712E-21
	    // At 10 times standard starting point Java port gave:
	    // niter = 32 nf = 61 ng = 33 Absolute convergence final f = 7.46036E-27
	    // Original FORTRAN gave:
	    // niter = 32 nf = 61 ng = 33 Absolute convergence final f = 7.46036E-27
	    // At 100 times standard starting point Java port gave:
	    // niter = 1 nf = 2 ng = 2 Singular convergence final f = 4.768371E33
	    // Original FORTRAN gave:
	    // niter = 1 nf = 2 ng = 2 Singular convergence final f = 4.76837E33
	    n = 10;
	    p = 10;
	    nex = 27;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Brown10", rstart, xscal1, xscal2);
	    
	    // Brown30 okay for both only at standard starting point
	    // Brown30, an example of BROWN_ALMOST_LINEAR
	    // At standard starting point Java port gave:
	    // niter = 8 nf = 12 ng = 9 X-convergence final f = 7.09975E-29
	    // Original FORTRAN gave:
	    // niter = 8 nf = 12 ng = 9 X-convergence final f = 7.09975E-29
	    // At 10 times standard starting point Java port gave:
	    // niter = 1 nf = 2 ng = 1 Singular convergence final f = 4.33681E41
	    // Original FORTRAN gave:
	    // niter = 1 nf = 2 ng =1 Singular convergence final f = 4.33681E41
	    // At 100 times standard starting point Java port gave:
	    // niter = 1 nf = 2 ng = 1 Singular convergence final f = 4.336808E101
	    // Original FORTRAN gave:
	    // niter = 1 nf = 2 ng = 1 Singular convergence final f = 4.33681E101
	    // Need to increase v and iv dimensions for this problem?
	    // Even so, initial sum of squares overflows!
	    n = 30;
	    p = 30;
	    nex = 28;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Brown30", rstart, xscal1, xscal2);
	    
	    // Brown40
	    // At the standard starting point Java port gave:
	    // niter = 9 nf = 15 ng = 10 Absolute convergence final f = 3.93830E-27
	    // Original FORTRAN gave:
	    // niter = 9 nf = 15 ng = 10 Absolute convergence final f = 3.93830E-27
	    // At 10 times the standard starting point Java port gave:
	    // niter = 1 nf = 2 ng = 1 Singular convergence final f = 4.13590E55
	    // Original FORTRAN gave:
	    // niter = 1 nf = 2 ng = 1 Singular convergence final f = 4.13590E55
	    // At 100 times standard starting point Java port gave:
	    // niter = 1 nf = 2 ng = 1 Singular convergence final f = 4.13590E135
	    // Original FORTRAN gave:
	    // niter = 1 nf = 2 ng = 1 Singular convergence final f = 4.13590E135
	    n = 40;
	    p = 40;
	    nex = 29;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Brown40", rstart, xscal1, xscal2);
	    
	    // Bard + 10 Ok
	    // Bard + 10
	    // At standard starting point Java port gave:
	    // niter = 6 nf = 7 ng = 7 X- and relative function convergence final f = 4.10744E-3
	    // Original FORTRAN gave:
	    // niter = 6 nf = 7 ng = 7 X- and relative function convergence f = 4.10744E-3
	    // At 10 times standard starting point Java port gave:
	    // niter = 10 nf = 16 ng = 11 X- and relative function convergence final f = 4.10744E-3
	    // Original FORTRAN gave:
	    // niter = 10 nf = 16 ng = 11 X- and relative function convergence final f = 4.10744E-3
	    // At 100 times standard starting point Java port gave:
	    // niter = 41 nf = 95 ng = 42 X- and relative function convergence final f = 4.10744E-3
	    // Original FORTRAN gave:
	    // niter = 30 nf = 72 ng = 31 Relative convergence final f = 4.10744E-3
	    n = 15;
	    p = 3;
	    nex = 30;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Bard+10", rstart, xscal1, xscal2);
	    
	    // Kowalik and Osborne + 10.
	    // At standard starting port Java port gave:
	    // niter = 249 nf = 400 ng = 250 Function evaluation limit final f = 2.52832E-3
	    // Original FORTRAN gave:
	    // niter = 300 nf = 321 ng = 301 Iteration limit final f = 2.44316E-3
	    // At 10 times standard starting point Java port gave:
	    // niter = 30 nf = 49 ng = 31 Relative convergence f = 2.66312E-4
	    // Original FORTRAN gave:
	    // niter = 30 nf = 49 ng = 31 Relative convergence final f = 2.66312E-4
	    // At 100 times standard starting point Java port gave:
	    // niter = 65 nf = 82 ng = 66 Relative convergence final f = 2.66312E-4
	    // Original FORTRAN gave:
	    // niter = 69 nf = 101 ng = 70 Relative convergence final f = 2.66312E-4
	    n = 11;
	    p = 4;
	    nex = 31;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Kowalik+10", rstart, xscal1, xscal2);
	    
	    // Meyer + 10 Both Java and FORTRAN only worked at standard starting point
	    // Meyer + 10
	    // At standard starting point Java port gave:
	    // niter = 220 nf = 353 ng = 220 X- and relative function convergence final f = 40.2836
	    // Original FORTRAN gave:
	    // niter = 220 nf = 352 ng = 220 X- and relative function convergence final f = 40.2836
	    // At 10 times standard starting point Java port gave:
	    // niter = 300 nf = 345 ng = 301 Iteration limit final f = 2.81535E5
	    // Original FORTRAN gave:
	    // niter = 300 nf = 343 ng = 301 Iteration limit final f = 2.87019E5
	    // At 100 times standard starting point Java port gave:
	    // niter = 300 nf = 381 ng = 301 Iteration limit final f = 1.39776E8
	    // Original FORTRAN gave:
	    // niter = 300 nf = 382 ng = 301 Iteration limit final f = 1.39919E8
	    // 
	    n = 16;
	    p = 3;
	    nex = 32;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Meyer+10", rstart, xscal1, xscal2);
	    
	    // Watson6 + 10
	    // At standard starting point Java port gave:
	    // niter = 11 nf = 16 ng = 12 Relative function convergence final f = 14.3398
	    // Original FORTRAN gave:
	    // niter = 11 nf = 16 ng = 12 Relative function convergence final f = 14.3398
	    // At 10 times standard starting point Java port gave:
	    // niter = 11 nf = 16 ng = 12 Relative function convergence final f = 14.3398
	    // Original FORTRAN gave:
	    // niter = 11 nf = 16 ng = 12 Relative function convergence final f = 14.3398
	    // At 100 times standard starting point Java port gave:
	    // niter = 11 nf = 16 ng = 12 Relative function convergence final f = 14.3398
	    // Original FORTRAN gave:
	    // niter = 11 nf = 16 ng = 12 Relative function convergence final f = 14.3398
	    n = 31;
	    p = 6;
	    nex = 33;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Watson6+10", rstart, xscal1, xscal2);
	    
	    // Watson9 + 10
	    // At standard starting point Java port gave:
	    // niter = 15 nf = 24 ng = 16 Relative function convergence final f = 4.21317
	    // Original FORTRAN gave:
	    // niter = 12 nf = 24 ng = 16 Relative function convergence final f = 4.21317
	    // At 10 times standard starting point Java port gave:
	    // niter = 15 nf = 24 ng = 16 Relative function convergence final f = 4.21317
	    // Original FORTRAN gave:
	    // niter = 15 nf = 24 ng = 16 Relative function convergence final f = 4.21317
	    // At 100 times standard starting point Java port gave:
	    // niter = 15 nf = 24 ng = 16 Relative function convergence final f = 4.21317
	    // Original FORTRAN gave:
	    // niter = 15 nf = 24 ng = 16 Relative function convergence final f = 4.21317
	    n = 31;
	    p = 9;
	    nex = 34;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Watson9+10", rstart, xscal1, xscal2);
	    
	    // Watson12 + 10
	    // At standard starting point Java port gave:
	    // niter = 23 nf = 36 ng = 23 X- and relative function convergence final f = 0.323211
	    // Original FORTRAN gave:
	    // niter = 20 nf = 33 ng = 20 Relative function convergence f = 0.323211
	    // At 10 times standard starting point Java port gave:
	    // niter = 23 nf = 36 ng = 23 X- and relative function convergence final f = 0.323211
	    // Original FORTRAN gave:
	    // niter = 20 nf = 33 ng = 20 Relative function convergence final f = 0.323211
	    // At 100 times standard starting point Java port gave:
	    // niter = 23 nf = 36 ng = 23 X- and relative function convergence final f = 0.323211
	    // Original FORTRAN gave:
	    // niter = 20 nf = 33 ng = 20 Relative function convergence final f = 0.323211
	    n = 31;
	    p = 12;
	    nex = 35;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Watson12+10", rstart, xscal1, xscal2);
	    
	    // Watson20 + 10
	    // At standard starting point Java port gave:
	    // niter = 194 nf = 400 nf = 194 Function evaluation limit final f = 0.00780931
	    // Original FORTRAN gave:
	    // niter = 93 nf = 200 ng = 93  Function evaluation limit final f = 1.50354E-2
	    // At 10 times standard starting point Java port gave:
	    // niter = 194 nf = 400 nf = 194 Function evaluation limit final f = 0.00780931
	    // Original FORTRAN gave:
	    // niter = 93 nf = 200 nf = 93 Function evaluation limit final f = 1.50354E-2
	    // At 100 times standard starting point Java port gave:
	    // niter = 194 nf = 400 ng = 194 Function evaluation limit final f = 0.00780931
	    // Original FORTRAN gave:
	    // niter = 93 nf = 200 ng = 93 Function evaluation limit final f = 1.50354E-2
	    n = 31;
	    p = 20;
	    nex = 36;
	    useAnalyticJacobian = true;
	    xscal1 = 1;
	    xscal2 = 3;
	    nltest("Watson20+10", rstart, xscal1, xscal2);
	    
	    // Repeat Rosenbrock with finite-difference jacobian.
	    // At standard starting point Java port gave:
	    // niter = 18 nf = 29 ng = 19 Absolute convergence final f = 7.35208E-9
	    // Original FORTRAN gave:
	    // niter = 18 nf = 29 ng = 19 Absolute convergence final f = 7.91461E-22
	    n = 2;
	    p = 2;
	    nex = 1;
	    useAnalyticJacobian = false;
	    xscal1 = 1;
	    xscal2 = 1;
	    iv[17] = 50;
	    iv[18] = 40;
	    nltest("Rosenbrock", rstart, xscal1, xscal2);
	    
	    // Repeat Brown with finite-difference Jacobian.
	    // At standard starting point Java port gave:
	    // niter = 16 nf = 27 ng = 17 Relative function convergence final f = 42911.1
	    // Original FORTRAN gave:
	    // niter = 16 nf = 26 ng = 17 Relative function convergence final f = 42911.1
	    // At 10 times standard starting point Java port gave:
	    // niter = 16 nf = 30 ng = 17 Relative function convergence final f = 42911.1
	    // Original FORTRAN gave:
	    // niter = 16 nf = 30 ng = 17 Relative function convergence final f = 42911.1
	    // At 100 times standard starting point Java port gave:
	    // niter = 15 nf = 30 ng = 15 Function evaluation limit final f = 43372.9
	    // Original FORTRAN gave:
	    // niter = 14 nf = 30 ng = 14 Function evaluation limit final f = 53322.4
	    n = 20;
	    p = 4;
	    nex = 18;
	    useAnalyticJacobian = false;
	    xscal1 = 1;
	    xscal2 = 3;
	    v[29] = Math.max(1.0E-7, v[29]);
	    iv[17] = 30;
	    iv[18] = 20;
	    nltest("Brown", rstart, xscal1, xscal2);
	    
	    // Print summary
	    Preferences.debug(version + "\n", Preferences.DEBUG_ALGORITHM);
	    Preferences.debug("Summary of test runs\n", Preferences.DEBUG_ALGORITHM);
	    for (k = 1; k <= nprob; k++) {
	    	j = is[6][k];
	    	if (j == 1) {
	    		Preferences.debug("Problem = " + name[k] + "     analytic jacobian\n", Preferences.DEBUG_ALGORITHM);
	    	}
	    	else {
	    		Preferences.debug("Problem = " + name[k] + "     finite difference jacobian\n", Preferences.DEBUG_ALGORITHM);
	    	}
	    	Preferences.debug("n = " + is[1][k] + "\n", Preferences.DEBUG_ALGORITHM);
	    	Preferences.debug("p = " + is[2][k] + "\n", Preferences.DEBUG_ALGORITHM);
	    	Preferences.debug("niter = " + is[3][k] + "\n", Preferences.DEBUG_ALGORITHM);
	    	Preferences.debug("nf = " + is[4][k] + "\n", Preferences.DEBUG_ALGORITHM);
	    	Preferences.debug("ng = " + is[5][k] + "\n", Preferences.DEBUG_ALGORITHM);
	    	Preferences.debug("iv1 = " + irc[k] + "\n", Preferences.DEBUG_ALGORITHM);
	    	Preferences.debug("x0scal = " + rs[1][k] + "\n", Preferences.DEBUG_ALGORITHM);
	    	Preferences.debug("final f = " + rs[2][k] + "\n", Preferences.DEBUG_ALGORITHM);
	    	Preferences.debug("preldf = " + rs[3][k] + "\n", Preferences.DEBUG_ALGORITHM);
	    	Preferences.debug("nreldf = " + rs[4][k] + "\n", Preferences.DEBUG_ALGORITHM);
	    	Preferences.debug("reldx = " + rs[5][k] + "\n\n\n", Preferences.DEBUG_ALGORITHM);
	    } // for (k = 1; k <= nprob; k++)
	    
	    Preferences.debug("Normal end of execution\n", Preferences.DEBUG_ALGORITHM);
	    timestamp();
	    
	} // main
	
	private void nltest ( String title, boolean rstart, int xscal1, int xscal2 ) {

	/*****************************************************************************80
	!
	!! NLTEST calls NL2SOL for a given problem.
	!
	!  Modified:
	!
	!    28 March 2006
	!
	!  Parameters:
	!
	!    Input, integer N, the number of equations or functions.
	!
	!    Input, integer P, the number of variables.
	!
	!    Input, integer NEX, the index of the problem.
	!
	!    Input, String TITLE, the title of the problem.
	!
	!    Input, logical RSTART, ?
	!
	!    Input, integer XSCAL1, XSCAL2, control the number of runs,
	!    and the scaling used.  The problem will be solved for 
	!    IRUN = XSCAL1 to XSCAL2, and the scaling used will be 10**(IRUN-1).
	*/

	  final int p_max = 40;

	  int i;
	  int irun;
	  int jac;
	  final int prunit = 21;
	  int pu;
	  String rc[] = new String[11];
	  final int reldx = 17;
	  boolean rstrt;
	  uiparm = new int[2];
	  urparm = new double[2];
	  String version;
	  x = new double[p_max+1];
	  double x0scal = 1.0;

	  String alg[] = new String[3];
	  String jtyp[] = new String[3];

	  final int f = 10;
	  final int f0 = 13;
	  final int nfcall = 6;
	  final int nfcov = 40;
	  final int ngcall = 30;
	  final int ngcov = 41;
	  final int niter = 31;
	  final int nreduc = 6;
	  final int preduc = 7;

	  alg[1] = "ol";
	  alg[2] = "no";
	  if (useAnalyticJacobian) {
		  jac = 1;
	  }
	  else {
		  jac = 2;
	  }
	  jtyp[1] = " ";
	  jtyp[2] = "*";
	  rc[1] = ".";
	  rc[2] = "+";
	  rc[3] = "x";
	  rc[4] = "r";
	  rc[5] = "b";
	  rc[6] = "a";
	  rc[7] = "s";
	  rc[8] = "f";
	  rc[9] = "e";
	  rc[10] = "i";

	  uiparm[1] = nex;
	  rstrt = rstart;
	  version = "NL2SOL version 2.2";

	  if ( ! rstrt ) {
	    pu = iv[prunit];
	    if ( pu != 0 ) {
	      Preferences.debug("nl2s" + alg[jac] + " on problem " + title + "      " + version + "\n", 
	    		  Preferences.DEBUG_ALGORITHM );
	      
	    }
	  } // if (!rstrt)

	  for (irun = xscal1; irun <= xscal2; irun++) {
	//
	//  Initialize the solution vector X.
	//
	    if ( ! rstrt ) {

	      iv[1] = 12;
	      x0scal = Math.pow(10.0, (irun - 1.0));

	      xinit ( p, x, nex );

	      for ( i = 1; i <= p; i++) {
	        x[i] = x0scal * x[i];
	      }

	    } // if (!rstrt)

	    if ( jac == 1 ) {
	      nl2sol ( );
	    }
	    else if ( jac == 2 ) {
	      nl2sno ( );
	    }

	    if ( ! rstrt && nprob < 50 ) {
	      nprob = nprob + 1;
	    }

	    name[nprob] = title;

	    is[1][nprob] = n;
	    is[2][nprob] = p;
	    is[3][nprob] = iv[niter];
	    is[4][nprob] = iv[nfcall] - iv[nfcov];
	    is[5][nprob] = iv[ngcall] - iv[ngcov];

	    i = iv[1];
	    irc[nprob] = rc[i];
	    is[6][nprob] = jac;
	    rs[1][nprob] = x0scal;
	    rs[2][nprob] = v[f];
	    if ( v[f0] <= 0.0 ) {
	      rs[3][nprob] = 1.0;
	    }
	    else {
	      rs[3][nprob] = v[preduc] / v[f0];
	    }
	    if ( v[f0] <= 0.0 ) {
	      rs[4][nprob] = 1.0;
	    }
	    else {
	      rs[4][nprob] = v[nreduc] / v[f0];
	    }      
	    rs[5][nprob] = v[reldx];

	    rstrt = false;

	    if ( nout != 0 ) {

	      if (nprob == 1) {
	          Preferences.debug(version + "\n", Preferences.DEBUG_ALGORITHM);
	      }
	      Preferences.debug("Problem = " + title + "  test summary\n", Preferences.DEBUG_ALGORITHM);
	      if (useAnalyticJacobian) {
              Preferences.debug("Analytic jacobian\n", Preferences.DEBUG_ALGORITHM);
	      }
	      else {
	    	  Preferences.debug("Finite difference jacobian\n", Preferences.DEBUG_ALGORITHM);
	      }
	      Preferences.debug("n = " + is[1][nprob] + "\n", Preferences.DEBUG_ALGORITHM);
	      Preferences.debug("p = " + is[2][nprob] + "\n", Preferences.DEBUG_ALGORITHM);
	      Preferences.debug("niter = " + is[3][nprob] + "\n", Preferences.DEBUG_ALGORITHM);
	      Preferences.debug("nf = " + is[4][nprob] + "\n", Preferences.DEBUG_ALGORITHM);
	      Preferences.debug("ng = " + is[5][nprob] + "\n", Preferences.DEBUG_ALGORITHM);
	      Preferences.debug("iv1 = " + irc[nprob] + "\n", Preferences.DEBUG_ALGORITHM);
	      Preferences.debug("x0scal = " + rs[1][nprob] + "\n", Preferences.DEBUG_ALGORITHM);
	      Preferences.debug("final f = " + rs[2][nprob] + "\n", Preferences.DEBUG_ALGORITHM);
	      Preferences.debug("preldf = " + rs[3][nprob] + "\n", Preferences.DEBUG_ALGORITHM);
	      Preferences.debug("nreldf = " + rs[4][nprob] + "\n", Preferences.DEBUG_ALGORITHM);
	      Preferences.debug("reldx = " + rs[5][nprob] + "\n", Preferences.DEBUG_ALGORITHM);
	    } // if (nout != 0)

	  } // for (irun = xscal1; irun <= xscal2; irun++)

	  return;
	} // private void nltest
	
	private void xinit ( int p, double x[], int nex ) {

	/*****************************************************************************80
	!
	!! XINIT initializes the solution vector X.
	!
	!  Discussion:
	!
	!    This routine initializes the solution vector X.
	!
	!  Modified:
	!
	!    27 March 2006
	!
	!  Parameters:
	!
	!    Input, integer P, the  number of parameters.
	!
	!    Output, real X(P), the initial value for the solution vector.
	!
	!    Input, integer NEX, the test problem number, between 1 and 36.
	*/

	  int i;
	//
	//  Rosenbrock.
	//
	  if ( nex == 1 ) {

	    x[1] = -1.2;
	    x[2] = 1.0;
	  }
	//
	//  Helix.
	//
	  else if ( nex == 2 ) {

	    x[1] = -1.0;
	    x[2] = 0.0;
	    x[3] = 0.0;
	  }
	//
	//  Singular.
	//
	  else if ( nex == 3 ) {

	    x[1] = 3.0;
	    x[2] = -1.0;
	    x[3] = 0.0;
	    x[4] = 1.0;
	  }
	//
	//  Woods.
	//
	  else if ( nex == 4 ) {

	    x[1] = -3.0;
	    x[2] = -1.0;
	    x[3] = -3.0;
	    x[4] = -1.0;
	  }
	//
	//  Zangwill.
	//
	  else if ( nex == 5 ) {

	    x[1] = 100.0;
	    x[2] = -1.0;
	    x[3] = 2.5;
	  }
	//
	//  Engvall.
	//
	  else if ( nex == 6 ) {

	    x[1] = 1.0;
	    x[2] = 2.0;
	    x[3] = 0.0;
	  }
	//
    //  Branin.
	//
	  else if ( nex == 7 ) {

	    x[1] = 2.0;
	    x[2] = 0.0;
	  }
	//
	// Beale.
	//
	  else if ( nex == 8 ) {

	    x[1] = 0.1;
	    x[2] = 0.1;
	  }
	//
	//  Cragg and Levy.
	//
	  else if ( nex == 9 ) {

	    x[1] = 1.0;
	    x[2] = 2.0;
	    x[3] = 2.0;
	    x[4] = 2.0;
	  }
	//
	//  Box.
	//
	  else if ( nex == 10 ) {

	    x[1] = 0.0;
	    x[2] = 10.0;
	    x[3] = 20.0;
	  }
	//
	//  Davidon 1.
	//
	  else if ( nex == 11 ) {

	    for (i = 1; i <= p; i++) {
	      x[i] = 0.0;
	    }
	  }
	//
	//  Freudenstein and Roth.
	//
	  else if ( nex == 12 ) {

	    x[1] = 15.0;
	    x[2] = -2.0;
	  }
	//
	//  Watson6.
	//  Watson9.
	//  Watson12.
	//  Watson20.
	//
	  else if ( nex >= 13  && nex <= 16 ) {

	    for  (i = 1; i <= p; i++) {
	      x[i] = 0.0;
	    }
	  }
	//
	//  Chebyquad.
	//
	  else if ( nex == 17 ) {

	    for  (i = 1; i <= p; i++) {
	      x[i] = ((double) i) / ((double) ( p + 1));
	    }
	  }
	//
	//  Brown and Dennis.
	//
	  else if ( nex == 18 ) {

	    x[1] = 25.0;
	    x[2] = 5.0;
	    x[3] = -5.0;
	    x[4] = -1.0;
	  }
	//
	//  Bard.
	//
	  else if ( nex == 19 ) {

	    x[1] = 1.0;
	    x[2] = 1.0;
	    x[3] = 1.0;
	  }
	//
	//  Jennrich and Sampson.
	//
	  else if ( nex == 20 ) {

	    x[1] = 0.3;
	    x[2] = 0.4;
	  }
	//
	//  Kowalik and Osborne.
	//
	  else if ( nex == 21 ) {

	    x[1] = 0.25;
	    x[2] = 0.39;
	    x[3] = 0.415;
	    x[4] = 0.39;
	  }
	//
	//  Osborne 1.
	//
	  else if ( nex == 22 ) {

	    x[1] = 0.5;
	    x[2] = 1.5;
	    x[3] = -1.0;
	    x[4] = 0.01;
	    x[5] = 0.02;
	  }
	//
	//  Osborne 2.
	//
	  else if ( nex == 23 ) {

	    x[1] = 1.3;
	    x[2] = 0.65;
	    x[3] = 0.65;
	    x[4] = 0.7;
	    x[5] = 0.60;
	    x[6] = 3.0;
	    x[7] = 5.0;
	    x[8] = 7.0;
	    x[9] = 2.0;
	    x[10] = 4.5;
	    x[11] = 5.5;
	  }
	//
	//  Madsen.
	//
	  else if ( nex == 24 ) {

	    x[1] = 3.0;
	    x[2] = 1.0;
	  }
	//
	//  Meyer.
	//
	  else if ( nex == 25 ) {

	    x[1] = 0.02;
	    x[2] = 4000.0;
	    x[3] = 250.0;
	  }
	//
	//  Brown5.
	//  Brown10.
	//  Brown30.
	//  Brown40.
	//
	  else if ( nex >= 26 && nex <= 29) {
        for (i = 1; i <= p; i++) {
	        x[i] = 0.5;
        }
	  }
	//
	//  Bard + 10.
	//
	  else if ( nex == 30 ) {

	    x[1] = 1.0;
	    x[2] = 1.0;
	    x[3] = 1.0;
	  }
	//
	//  Kowalik and Osborne + 10.
	//
	  else if ( nex == 31 ) {

	    x[1] = 0.25;
	    x[2] = 0.39;
	    x[3] = 0.415;
	    x[4] = 0.39;
	  }
	//
	//  Meyer + 10.
	//
	  else if ( nex == 32 ) {

	    x[1] = 0.02;
	    x[2] = 4000.0;
	    x[3] = 250.0;
	  }
	//
	//  Watson6 + 10.
	//  Watson9 + 10.
	//  Watson12 + 10.
	//  Watson20 + 10.
	//
	  else if ( nex >= 33 && nex <= 36) {

	    for  (i = 1; i <= p; i++) {
	      x[i] = 0.0;
	    }
	  }
	//
	//  Illegal value.
	//
	  else {
        System.out.println("XINIT - fatal error");
        Preferences.debug("XINIT - fatal error\n", Preferences.DEBUG_ALGORITHM);
        System.out.println("Illegal index nex = " + nex);
        Preferences.debug("Illegal index nex = " + nex + "\n", Preferences.DEBUG_ALGORITHM);
        System.exit(-1);
	  }

	  return;
	} // private void xinit
	
	// n, input, the number of observations, that is the number of components in r(x).
	//    p <= n
	// p, input, the number of parameters of component in x.  p must be positive
	// x, input/output of dimension p+1.  On input x is an initial guess at the desired
	//    parameter estimate.  On output, x contains the best parameter estimate found.
	// iv, input/output of dimension 61 + p, helps control the NL2sol algorithm and
	//    is used to store various intermediate quantities.  Of particular interest are
	//    the initialization/return code iv[1] and the entries that are used to control
	//    printing and limit the number of iterations and function evaluations.
	// v, input/output of dimension at least 94 + n*p + 3*n + p*(3*p+33)/2 that helps
	//    control the nl2sol solution algorithm and that is used to store various 
	//    intermediate quantities.  Of particular interest are entries in v that limit
	//    the length of the first step attempted(lmax0), specify convergence tolerances
	//    (afctol, rfctol, xctol, xftol), and help choose the step size used in computing
	//    the covariance matrix (delta0).
	// useAnalyticJacobian, input, if true call nl2sol which use an analytic jacobian.
	//    If false, call nl2sno which uses a finite difference jacobian
	// uiparm, input, user integer parameter array
	// urparm, input, user double parameter array
	public NL2sol(int n, int p, double x[], int iv[], double v[], boolean useAnalyticJacobian,
			      int uiparm[], double urparm[]) {
	    this.n = n;
	    this.p = p;
	    this.x = x;
	    this.iv = iv;
	    this.v = v;
	    this.useAnalyticJacobian = useAnalyticJacobian;
	    this.uiparm = uiparm;
	    this.urparm = urparm;
	}
	
	public void driver () {
		dfault(iv, v);
		iv[17] = 4000;
		iv[18] = 3000;
		if (useAnalyticJacobian) {
		    nl2sol();	
		}
		else {
			nl2sno();
		}
	}
	
	/**
     * getChiSquared - accessor to chi-squared value (goodness-of-fit measure).
     *
     * @return  the value of chi squared
     */
    public double getChiSquared() {
        return 2.0 * v[10];
    }

    
	
	public void nl2sol() {
		/***********************************************************************
		!
		!! NL2SOL minimizes a nonlinear sum of squares using an analytic jacobian.
		!
		!  Purpose:
		!
		!    Given a P-vector X of parameters, CALCR computes an N-vector
		!    R = R(X) of residuals corresponding to X.  R(X) probably arises
		!    from a nonlinear model involving P parameters and N observations.
		!
		!    This routine interacts with NL2ITR to seek a parameter vector X
		!    that minimizes the sum of the squares of the components of R(X),
		!    i.e., that minimizes the sum-of-squares function
		!    F(X) = R(X)' * R(X) / 2.  R(X) is assumed to be a twice
		!    continuously differentiable function of X.
		!
		!    See reference 1 for a description of the algorithm used.
		!    On problems which are naturally well scaled, better performance 
		!    may be obtained by setting V(D0INIT) = 1.0 and IV(DTYPE) = 0,
		!    which will cause the scale vector D to be set to all ones.
		!
		!    After a return with IV(1) <= 11, it is possible to restart,
		!    that is, to change some of the IV and V input values and continue 
		!    the algorithm from the point where it was interrupted.  IV(1) 
		!    should not be changed, nor should any entries of IV
		!    and V other than the input values (those supplied by DFAULT).
		!
		!    Those who do not wish to write a CALCJ which computes the jacobian 
		!    matrix analytically should call NL2SNO rather than NL2SOL.
		!    NL2SNO uses finite differences to compute an approximate jacobian.
		!
		!    Those who would prefer to provide R and J (the residual and
		!    jacobian) by reverse communication rather than by writing subroutines
		!    CALCR and CALCJ may call on NL2ITR directly.  See the comments at the 
		!    beginning of NL2ITR.
		!
		!    Those who use NL2SOL interactively may wish to supply their
		!    own STOPX function, which should return TRUE if the break key
		!    has been pressed since stopx was last invoked.  This makes it possible
		!    to externally interrupt NL2SOL (which will return with
		!    IV(1) = 11 if STOPX returns TRUE).
		!
		!    Storage for J is allocated at the end of V.  Thus the caller
		!    may make V longer than specified above and may allow CALCJ to use
		!    elements of J beyond the first N*P as scratch storage.
		!
		!  Modified:
		!
		!    04 April 2006
		!
		!  Author:
		!
		!    David Gay
		!
		!  Reference:
		!
		!    John Dennis, David Gay, Roy Welsch,
		!    An Adaptive Nonlinear Least Squares Algorithm,
		!    ACM Transactions on Mathematical Software,
		!    Volume 7, Number 3, 1981.
		!
		!  Parameters:
		!
		!    Input, integer N, the number of observations, that is, the number of
		!    components in R(X).  P <= N.
		!
		!    Input, integer P, the number of parameters, or components in X.  P must
		!    be positive.
		!
		!    Input/output, real X(P).  On input, X is an initial guess at the
		!    desired parameter estimate.  On output, X contains the best parameter 
		!    estimate found.
		!
		!    Input, external CALCR, a subroutine which, given X, computes R(X).
		!    CALCR must be declared external in the calling program.
		!    It is invoked by
		!      call calcr ( n, p, x, nf, r, uiparm, urparm, ufparm )
		!    When CALCR is called, NF is the invocation count for CALCR.  It is 
		!    included for possible use with CALCJ.  If X is out of bounds, for
		!    instance, if it would cause overflow in computing R(X), then CALCR 
		!    should set NF to 0.  This will cause a shorter step to be attempted.  
		!    The other parameters are as described above and below.  CALCR 
		!    should not change N, P, or X.
		!
		!    Input, external CALCJ, a subroutine which, given X, computes the 
		!    jacobian matrix J of R at X, that is, the N by P matrix whose
		!    (I,K) entry is the partial derivative of the I-th component of R 
		!    with respect to X(K).  CALCJ must be declared external in the 
		!    calling program.  It is invoked by
		!      call calcj(n,p,x,nf,j,uiparm,urparm,ufparm)
		!    NF is the invocation count for CALCR at the time R(X) was evaluated.  
		!    The X passed to CALCJ is usually the one passed to CALC on either its 
		!    most recent invocation or the one prior to it.  If CALCR saves 
		!    intermediate results for use by CALCJ, then it is possible to tell 
		!    from NF whether they are valid for the current X (or which copy is 
		!    valid if two copies are kept).  If J cannot be computed at X,
		!    then CALCJ should set NF to 0.  In this case, NL2SOL will return 
		!    with IV(1) = 15.  The other parameters to CALCJ are as described 
		!    above and below.  CALCJ should not change N, P, or X.
		!
		!    Input/output, integer IV(60+P), helps control the NL2SOL algorithm 
		!    and is used to store various intermediate quantities.  Of particular 
		!    interest are the initialization/return code IV(1) and the entries 
		!    in that control printing and limit the number of iterations and 
		!    function evaluations.  See the section on IV input values.
		!
		! v........ (input/output) a floating-point value array of length at
		!                  least 93 + n*p + 3*n + p*(3*p+33)/2 that helps con-
		!                  trol the nl2sol algorithm and that is used to store
		!                  various intermediate quantities.  of particular in-
		!                  terest are the entries in v that limit the length of
		!                  the first step attempted (lmax0), specify conver-
		!                  gence tolerances (afctol, rfctol, xctol, xftol),
		!                  and help choose the step size used in computing the
		!                  covariance matrix (delta0).  see the section on
		!                  (selected) v input values below.
		!
		! uiparm... (input) user integer parameter array passed without change
		!                  to calcr and calcj.
		!
		! urparm... (input) user floating-point parameter array passed without
		!                  change to calcr and calcj.
		!
		! ufparm... (input) user external subroutine or function passed without
		!                  change to calcr and calcj.
		!
		!  iv input values (from subroutine dfault)
		!
		! iv(1)...  on input, iv(1) should have a value between 0 and 12......
		!             0 and 12 mean this is a fresh start.  0 means that
		!             dfault(iv, v) is to be called to provide all default
		!             values to iv and v.  12 (the value that dfault assigns to
		!             iv(1)) means the caller has already called dfault(iv, v)
		!             and has possibly changed some iv and/or v entries to non-
		!             default values.  default = 12.
		! iv(covprt)... iv(14) = 1 means print a covariance matrix at the solu-
		!             tion.  (this matrix is computed just before a return with
		!             iv(1) = 3, 4, 5, 6.)
		!             iv(covprt) = 0 means skip this printing.  default = 1.
		! iv(covreq)... iv(15) = nonzero means compute a covariance matrix
		!             just before a return with iv(1) = 3, 4, 5, 6.  in
		!             this case, an approximate covariance matrix is obtained
		!             in one of several ways.  let k = abs(iv(covreq)) and let
		!             scale = 2*f(x)/max(1,n-p),  where 2*f(x) is the residual
		!             sum of squares.  if k = 1 or 2, then a finite-difference
		!             hessian approximation h is obtained.  if h is positive
		!             definite (or, for k = 3, if the jacobian matrix j at x
		!             is nonsingular), then one of the following is computed...
		!                  k = 1....  scale * h**-1 * (j**t * j) * h**-1.
		!                  k = 2....  scale * h**-1.
		!                  k = 3....  scale * (j**t * j)**-1.
		!             (j**t is the transpose of j, while **-1 means inverse.)
		!             if iv(covreq) is positive, then both function and grad-
		!             ient values (calls on calcr and calcj) are used in com-
		!             puting h (with step sizes determined using v(delta0) --
		!             see below), while if iv(covreq) is negative, then only
		!             function values (calls on calcr) are used (with step
		!             sizes determined using v(dltfdc) -- see below).  if
		!             iv(covreq) = 0, then no attempt is made to compute a co-
		!             variance matrix (unless iv(covprt) = 1, in which case
		!             iv(covreq) = 1 is assumed).  see iv(covmat) below.
		!             default = 1.
		! iv(dtype).... iv(16) tells how the scale vector D (see ref. 1) should
		!             be chosen.  iv(dtype) >= 1 means choose d as described
		!             below with v(dfac).  iv(dtype) <= 0 means the caller
		!             has chosen d and has stored it in v starting at
		!             v(94 + 2*n + p*(3*p + 31)/2).  default = 1.
		! iv(inits).... iv(25) tells how the S matrix (see ref. 1) should be
		!             initialized.  0 means initialize S to 0 (and start with
		!             the Gauss-Newton model).  1 and 2 mean that the caller
		!             has stored the lower triangle of the initial S rowwise in
		!             v starting at v(87+2*p).  iv(inits) = 1 means start with
		!             the Gauss-Newton model, while iv(inits) = 2 means start
		!             with the augmented model (see ref. 1).  default = 0.
		! iv(mxfcal)... iv(17) gives the maximum number of function evaluations
		!             (calls on calcr, excluding those used to compute the co-
		!             variance matrix) allowed.  if this number does not suf-
		!             fice, then nl2sol returns with iv(1) = 9.  default = 200.
		! iv(mxiter)... iv(18) gives the maximum number of iterations allowed.
		!             it also indirectly limits the number of gradient evalua-
		!             tions (calls on calcj, excluding those used to compute
		!             the covariance matrix) to iv(mxiter) + 1.  if iv(mxiter)
		!             iterations do not suffice, then nl2sol returns with
		!             iv(1) = 10.  default = 150.
		! iv(outlev)... iv(19) controls the number and length of iteration sum-
		!             mary lines printed (by itsmry).  iv(outlev) = 0 means do
		!             not print any summary lines.  otherwise, print a summary
		!             line after each abs(iv(outlev)) iterations.  if iv(outlev)
		!             is positive, then summary lines of length 117 (plus carri-
		!             age control) are printed, including the following...  the
		!             iteration and function evaluation counts, current func-
		!             tion value (v(f) = half the sum of squares), relative
		!             difference in function values achieved by the latest step
		!             (i.e., reldf = (f0-v(f))/f0, where f0 is the function
		!             value from the previous iteration), the relative function
		!             reduction predicted for the step just taken (i.e.,
		!             preldf = v(preduc) / f0, where v(preduc) is described
		!             below), the scaled relative change in x (see v(reldx)
		!             below), the models used in the current iteration (g =
		!             Gauss-Newton, s=augmented), the Marquardt parameter
		!             STPPAR used in computing the last step, the sizing factor
		!             used in updating s, the 2-norm of the scale vector d
		!             times the step just taken (see ref. 1), and npreldf, i.e.,
		!             v(nreduc)/f0, where v(nreduc) is described below -- if
		!             npreldf is positive, then it is the relative function
		!             reduction predicted for a Newton step (one with
		!             STPPAR = 0).  if npreldf is zero, either the gradient
		!             vanishes (as does preldf) or else the augmented model
		!             is being used and its hessian is indefinite (with preldf
		!             positive).  if npreldf is negative, then it is the nega-
		!             of the relative function reduction predicted for a step
		!             computed with step bound v(lmax0) for use in testing for
		!             singular convergence.
		!                  if iv(outlev) is negative, then lines of maximum
		!             length 79 (or 55 is iv(covprt) = 0) are printed, includ-
		!             ing only the first 6 items listed above (through reldx).
		!             default = 1.
		! iv(parprt)... iv(20) = 1 means print any nondefault v values on a
		!             fresh start or any changed v values on a restart.
		!             iv(parprt) = 0 means skip this printing.  default = 1.
		! iv(prunit)... iv(21) is the output unit number on which all printing
		!             is done.  iv(prunit) = 0 means suppress all printing.
		!             (setting iv(prunit) to 0 is the only way to suppress the
		!             one-line termination reason message printed by itsmry.)
		!             default = standard output unit (unit 6 on most systems).
		! iv(solprt)... iv(22) = 1 means print out the value of x returned (as
		!             well as the corresponding gradient and scale vector d).
		!             iv(solprt) = 0 means skip this printing.  default = 1.
		! iv(statpr)... iv(23) = 1 means print summary statistics upon return-
		!             ing.  these consist of the function value (half the sum
		!             of squares) at x, v(reldx) (see below), the number of
		!             function and gradient evaluations (calls on calcr and
		!             calcj respectively, excluding any calls used to compute
		!             the covariance), the relative function reductions predict-
		!             ed for the last step taken and for a Newton step (or per-
		!             haps a step bounded by v(lmax0) -- see the descriptions
		!             of preldf and npreldf under iv(outlev) above), and (if an
		!             attempt was made to compute the covariance) the number of
		!             calls on calcr and calcj used in trying to compute the
		!             covariance.  iv(statpr) = 0 means skip this printing.
		!             default = 1.
		! iv(x0prt).... iv(24) = 1 means print the initial x and scale vector d
		!             (on a fresh start only).  iv(x0prt) = 0 means skip this
		!             printing.  default = 1.
		!
		!  (selected) iv output values
		!
		! iv(1)........ on output, iv(1) is a return code....
		!             3 = x-convergence.  the scaled relative difference be-
		!                  tween the current parameter vector x and a locally
		!                  optimal parameter vector is very likely at most
		!                  v(xctol).
		!             4 = relative function convergence.  the relative differ-
		!                  ence between the current function value and its lo-
		!                  cally optimal value is very likely at most v(rfctol).
		!             5 = both x- and relative function convergence (i.e., the
		!                  conditions for iv(1) = 3 and iv(1) = 4 both hold).
		!             6 = absolute function convergence.  the current function
		!                  value is at most v(afctol) in absolute value.
		!             7 = singular convergence.  the hessian near the current
		!                  iterate appears to be singular or nearly so, and a
		!                  step of length at most v(lmax0) is unlikely to yield
		!                  a relative function decrease of more than v(rfctol).
		!             8 = false convergence.  the iterates appear to be converg-
		!                  ing to a noncritical point.  this may mean that the
		!                  convergence tolerances (v(afctol), v(rfctol),
		!                  v(xctol)) are too small for the accuracy to which
		!                  the function and gradient are being computed, that
		!                  there is an error in computing the gradient, or that
		!                  the function or gradient is discontinuous near x.
		!             9 = function evaluation limit reached without other con-
		!                  vergence (see iv(mxfcal)).
		!            10 = iteration limit reached without other convergence
		!                  (see iv(mxiter)).
		!            11 = stopx returned .true. (external interrupt).  see the
		!                  usage notes below.
		!            13 = f(x) cannot be computed at the initial x.
		!            14 = bad parameters passed to assess (which should not
		!                  occur).
		!            15 = the jacobian could not be computed at x (see calcj
		!                  above).
		!            16 = n or p (or parameter nn to nl2itr) out of range --
		!                  p <= 0 or n < p or nn < n.
		!            17 = restart attempted with n or p (or par. nn to nl2itr)
		!                  changed.
		!            18 = iv(inits) is out of range.
		!            19...45 = v(iv(1)) is out of range.
		!            50 = iv(1) was out of range.
		!            87...(86+p) = jtol(iv(1)-86) (i.e., v(iv(1)) is not
		!                  positive (see v(dfac) below).
		! iv(covmat)... iv(26) tells whether a covariance matrix was computed.
		!             if (iv(covmat) is positive, then the lower triangle of
		!             the covariance matrix is stored rowwise in v starting at
		!             v(iv(covmat)).  if iv(covmat) = 0, then no attempt was
		!             made to compute the covariance.  if iv(covmat) = -1,
		!             then the finite-difference hessian was indefinite.  and
		!             and if iv(covmat) = -2, then a successful finite-differ-
		!             encing step could not be found for some component of x
		!             (i.e., calcr set nf to 0 for each of two trial steps).
		!             note that iv(covmat) is reset to 0 after each successful
		!             step, so if such a step is taken after a restart, then
		!             the covariance matrix will be recomputed.
		! iv(d)........ iv(27) is the starting subscript in v of the current
		!             scale vector d.
		! iv(g)........ iv(28) is the starting subscript in v of the current
		!             least-squares gradient vector (j**t)*r.
		! iv(nfcall)... iv(6) is the number of calls so far made on calcr (i.e.,
		!             function evaluations, including those used in computing
		!             the covariance).
		! iv(nfcov).... iv(40) is the number of calls made on calcr when
		!             trying to compute covariance matrices.
		! iv(ngcall)... iv(30) is the number of gradient evaluations (calls on
		!             calcj) so far done (including those used for computing
		!             the covariance).
		! iv(ngcov).... iv(41) is the number of calls made on calcj when
		!             trying to compute covariance matrices.
		! iv(niter).... iv(31) is the number of iterations performed.
		! iv(r)........ iv(50) is the starting subscript in v of the residual
		!             vector r corresponding to x.
		!
		! (selected) v input values (from subroutine dfault)
		!
		! v(afctol)... v(31) is the absolute function convergence tolerance.
		!             if nl2sol finds a point where the function value (half
		!             the sum of squares) is less than v(afctol), and if nl2sol
		!             does not return with iv(1) = 3, 4, or 5, then it returns
		!             with iv(1) = 6.  default = max(10**-20, machep**2), where
		!             machep is the unit roundoff.
		! v(delta0)... v(44) is a factor used in choosing the finite-difference
		!             step size used in computing the covariance matrix when
		!             iv(covreq) = 1 or 2.  for component i, step size
		!                  v(delta0) * max(abs(x(i)), 1/d(i)) * sign(x(i))
		!             is used, where d is the current scale vector (see ref. 1).
		!             (if this step results in calcr setting nf to 0, then -0.5
		!             times this step is also tried.)  default = machep**0.5,
		!             where machep is the unit roundoff.
		! v(dfac)..... v(41) and the d0 and jtol arrays (see v(d0init) and
		!             v(jtinit)) are used in updating the scale vector d when
		!             iv(dtype) > 0.  (d is initialized according to
		!             v(dinit).)  let d1(i) =
		!               max(sqrt(jcnorm(i)**2 + max(s(i,i),0)), v(dfac)*d(i)),
		!             where jcnorm(i) is the 2-norm of the i-th column of the
		!             current jacobian matrix and s is the s matrix of ref. 1.
		!             if iv(dtype) = 1, then d(i) is set to d1(i) unless
		!             d1(i) < jtol(i), in which case d(i) is set to
		!                                max(d0(i), jtol(i)).
		!             if iv(dtype) >= 2, then d is updated during the first
		!             iteration as for iv(dtype) = 1 (after any initialization
		!             due to v(dinit)) and is left unchanged thereafter.
		!             default = 0.6.
		! v(dinit).... v(38), if nonnegative, is the value to which the scale
		!             vector d is initialized.  default = 0.
		! v(dltfdc)... v(40) helps choose the step size used when computing the
		!             covariance matrix when iv(covreq) = -1 or -2.  for
		!             differences involving x(i), the step size first tried is
		!                       v(dltfdc) * max(abs(x(i)), 1/d(i)),
		!             where d is the current scale vector (see ref. 1).  (if
		!             this step is too big the first time it is tried, i.e., if
		!             calcr sets nf to 0, then -0.5 times this step is also
		!             tried.)  default = machep**(1/3), where machep is the
		!             unit roundoff.
		! v(d0init)... v(37), if positive, is the value to which all components
		!             of the d0 vector (see v(dfac)) are initialized.  if
		!             v(dfac) = 0, then it is assumed that the caller has
		!             stored d0 in v starting at v(p+87).  default = 1.0.
		! v(jtinit)... v(39), if positive, is the value to which all components
		!             of the jtol array (see v(dfac)) are initialized.  if
		!             v(jtinit) = 0, then it is assumed that the caller has
		!             stored jtol in v starting at v(87).  default = 10**-6.
		! v(lmax0).... v(35) gives the maximum 2-norm allowed for d times the
		!             very first step that nl2sol attempts.  it is also used
		!             in testing for singular convergence -- if the function
		!             reduction predicted for a step of length bounded by
		!             v(lmax0) is at most v(rfctol) * abs(f0), where  f0  is
		!             the function value at the start of the current iteration,
		!             and if nl2sol does not return with iv(1) = 3, 4, 5, or 6,
		!             then it returns with iv(1) = 7.    default = 100.
		! v(rfctol)... v(32) is the relative function convergence tolerance.
		!             if the current model predicts a maximum possible function
		!             reduction (see v(nreduc)) of at most v(rfctol)*abs(f0) at
		!             the start of the current iteration, where  f0  is the
		!             then current function value, and if the last step attempt-
		!             ed achieved no more than twice the predicted function
		!             decrease, then nl2sol returns with iv(1) = 4 (or 5).
		!             default = max(10**-10, machep**(2/3)), where machep is
		!             the unit roundoff.
		! v(tuner1)... v(26) helps decide when to check for false convergence
		!             and to consider switching models.  this is done if the
		!             actual function decrease from the current step is no more
		!             than v(tuner1) times its predicted value.  default = 0.1.
		! v(xctol).... v(33) is the x-convergence tolerance.  if a Newton step
		!             (see v(nreduc)) is tried that has v(reldx) <= v(xctol)
		!             and if this step yields at most twice the predicted func-
		!             tion decrease, then nl2sol returns with iv(1) = 3 (or 5).
		!             (see the description of v(reldx) below.)
		!             default = machep**0.5, where machep is the unit roundoff.
		! v(xftol).... v(34) is the false convergence tolerance.  if a step is
		!             tried that gives no more than v(tuner1) times the predict-
		!             ed function decrease and that has v(reldx) <= v(xftol),
		!             and if nl2sol does not return with iv(1) = 3, 4, 5, 6, or
		!             7, then it returns with iv(1) = 8.  (see the description
		!             of v(reldx) below.)  default = 100*machep, where
		!             machep is the unit roundoff.
		! v(*)........ dfault supplies to v a number of tuning constants, with
		!             which it should ordinarily be unnecessary to tinker.  see
		!             version 2.2 of the nl2sol usage summary (which is an
		!             appendix to ref. 1).
		!
		!  (selected) v output values
		!
		! v(dgnorm)... v(1) is the 2-norm of (d**-1)*g, where g is the most re-
		!             cently computed gradient and d is the corresponding scale
		!             vector.
		! v(dstnrm)... v(2) is the 2-norm of d * step, where step is the most re-
		!             cently computed step and d is the current scale vector.
		! v(f)........ v(10) is the current function value (half the sum of
		!             squares).
		! v(f0)....... v(13) is the function value at the start of the current
		!             iteration.
		! v(nreduc)... v(6), if positive, is the maximum function reduction
		!             possible according to the current model, i.e., the func-
		!             tion reduction predicted for a Newton step (i.e.,
		!             step = -h**-1 * g,  where  g = (j**t) * r  is the current
		!             gradient and h is the current hessian approximation --
		!             h = (j**t)*j  for the Gauss-Newton model and
		!             h = (j**t)*j + s  for the augmented model).
		!                  v(nreduc) = zero means h is not positive definite.
		!                  if v(nreduc) is negative, then it is the negative of
		!             the function reduction predicted for a step computed with
		!             a step bound of v(lmax0) for use in testing for singular
		!             convergence.
		! v(preduc)... v(7) is the function reduction predicted (by the current
		!             quadratic model) for the current step.  this (divided by
		!             v(f0)) is used in testing for relative function
		!             convergence.
		! v(reldx).... v(17) is the scaled relative change in x caused by the
		!             current step, computed as
		!                  max(abs(d(i)*(x(i)-x0(i)), 1 <= i <= p) /
		!                     max(d(i)*(abs(x(i))+abs(x0(i))), 1 <= i <= p),
		!             where x = x0 + step.
		*/
		  
		  final int d = 27;
		  int d1;
		  final int j = 33;
		  int j1;
		  int nf;
		  final int nfcall = 6;
		  final int nfgcal = 7;
		  final int r = 50;
		  int r1;
		  boolean strted;
		  final int toobig = 2;
		  boolean do10 = false;
		  boolean do30 = false;
		  boolean do40 = false;
		  double arr[];
		  double arr2[];
		  double arr2D[][];
		  int i;
		  int k;
		  int m;

		  d1 = 94 + 2*n + ( p * ( 3 * p + 31 ) ) / 2;
		  iv[d] = d1;
		  r1 = d1 + p;
		  iv[r] = r1;
		  j1 = r1 + n;
		  iv[j] = j1;
		  strted = true;
		  
	      if ( iv[1] != 0 && iv[1] != 12 ) {
	    	  do40 = true;
	      }
	      else {
	    	 do10 = true;
	         strted = false;
	         iv[nfcall] = 1;
	         iv[nfgcal] = 1;
	      }

		  while (true) {
	      
		  if (do10) {
              do10 = false;
		      nf = iv[nfcall];
              arr = new double[n+1];
              if (testMode) {
		          calcrTest(n, p, x, nf, arr, uiparm, urparm);
              }
              else {
            	  calcr(n, p, x, nf, arr, uiparm, urparm);  
              }
              for (k = 1; k <= n; k++) {
            	  v[r1+k-1] = arr[k];
              }
		      if ( strted ) {

		        if ( nf <= 0 ) {
		          iv[toobig] = 1;
		        }

		        do40 = true;

		      } // if (strted)
		      else if ( nf <= 0 ) {
		        iv[1] = 13;
		        arr = new double[p+1];
		        for (k = 1; k <= p; k++) {
		        	arr[k] = v[d1+k-1];
		        }
		        itsmry ( arr, iv, p, v, x );
		        return;
		      } // else if (nf <= 0)
		      else {
		    	  do30 = true;
		      }
		  } // if (do10)
          if (do30) {
              do30 = false;
              arr2D = new double[n+1][p+1];
              if (testMode) {
		          calcjTest(n, p, x, iv[nfgcal], arr2D, uiparm, urparm);
              }
              else {
            	  calcj(n, p, x, iv[nfgcal], arr2D, uiparm, urparm);  
              }
		      i = 0;
		      for (k = 1; k <= p; k++) {
            	  for (m = 1; m <= n; m++) {
            	      v[j1+i] = arr2D[m][k];
            	      i++;
            	  }
              }

		      if ( iv[nfgcal] == 0 ) {
		        iv[1] = 15;
		        arr = new double[p+1];
		        for (k = 1; k <= p; k++) {
		        	arr[k] = v[d1+k-1];
		        }
		        itsmry ( arr, iv, p, v, x );
		        return;
		      }

		      strted = true;
		      do40 = true;
          } // if (do30)

		 if (do40) {
             do40 = false;
             arr = new double[p+1];
             arr2 = new double[n+1];
             arr2D = new double[n+1][p+1];
             for (k = 1; k <= p; k++) {
            	 arr[k] = v[d1+k-1];
             }
             i = 0;
		      for (k = 1; k <= p; k++) {
           	      for (m = 1; m <= n; m++) {
           	          arr2D[m][k] = v[j1+i];
           	          i++;
           	      }
             }
		      for (k = 1; k <= n; k++) {
		    	  arr2[k] = v[r1+k-1];
		      }
		     nl2itr ( arr, iv, arr2D, n, n, p, arr2, v, x );
		     for (k = 1; k <= p; k++) {
            	 v[d1+k-1] = arr[k];
             }
		     i = 0;
		      for (k = 1; k <= p; k++) {
           	      for (m = 1; m <= n; m++) {
           	          v[j1+i] = arr2D[m][k];
           	          i++;
           	      }
             } 
		     for (k = 1; k <= n; k++) {
		    	 v[r1+k-1] = arr2[k];
		     }
		  if ( iv[1] == 2 ) {
		    do30 = true;
		  }
		  else if ( iv[1] < 2 ) {
		    do10 = true;
		  }
		  else {
		      return;
		  }
		  } // if (do40) 
	    } // while (true)
	} // nl2sol
	
	public void nl2sno() {
		/***********************************************************************
		!
		!! NL2SNO is like NL2SOL, but uses a finite difference jacobian.
		!
		!  Discussion:
		!
		!    NL2SNO is like NL2SOL, but without calcj -- minimize nonlinear sum of
		!    squares using finite-difference jacobian approximations
		!
		!    The parameters for NL2SNO are the same as those for NL2SOL
		!    except that CALCJ is omitted.  Instead of calling
		!    CALCJ to obtain the jacobian matrix of R at X, NL2SNO computes
		!    an approximation to it by forward finite differences.  See
		!    V(DLTFDJ) below.  NL2SNO uses function values only when comput-
		!    the covariance matrix, rather than the functions and gradients
		!    that NL2SOL may use.  To do so, NL2SNO sets IV(COVREQ) to -1 if
		!    IV(COVPRT) = 1 with IV(COVREQ) = 0 and to minus its absolute
		!    value otherwise.  Thus V(DELTA0) is never referenced and only
		!    V(DLTFDC) matters.  See NL2SOL for a description of V(DLTFDC).
		!
		!    The number of extra calls on CALCR used in computing the jacobian
		!    approximation are not included in the function evaluation
		!    count IV(NFCALL) and are not otherwise reported.
		!
		!  Modified:
		!
		!    04 April 2006
		!
		!  Author:
		!
		!    David Gay
		!
		!  Reference:
		!
		!    John Dennis, David Gay, Roy Welsch,
		!    An Adaptive Nonlinear Least Squares Algorithm,
		!    ACM Transactions on Mathematical Software,
		!    Volume 7, Number 3, 1981.
		!
		!  Parameters:
		!
		!    V(DLTFDJ) helps choose the step size used when computing the
		!    finite-difference jacobian matrix.  For differences involving X(I),
		!    the step size first tried is
		!      V(DLTFDJ) * max ( abs ( X(I) ), 1/D(I)),
		!    where D is the current scale vector; see reference 1.  If this step is 
		!    too big, so that CALCR sets NF to 0, then smaller steps are tried 
		!    until the step size is shrunk below 1000 * MACHEP, where MACHEP 
		!    is the unit roundoff.  Default = sqrt ( MACHEP ).
		*/
		  
		  final int covprt = 14;
		  final int covreq = 15;
		  final int d = 27;
		  int d1;
		  int dk;
		  final int dltfdj = 36;
		  final int dtype = 16;
		  double h;
		  final double hfac = 1000.0;
		  int i;
		  final int j = 33;
		  int j1;
		  int j1k;
		  int k;
		  int nf;
		  final int nfcall = 6;
		  final int nfgcal = 7;
		  final int r = 50;
		  int r1;
		  int rn;
		  boolean strted;
		  final int toobig = 2;
		  double xk;
		  boolean do10 = false;
		  boolean do30 = false;
		  boolean do80 = false;
		  int m;
		  int q;
		  int index;
		  double arr[];
		  double arr2[];
		  double arr2D[][];

		  d1 = 94 + 2 * n + ( p * ( 3 * p + 31 ) ) / 2;
		  iv[d] = d1;
		  r1 = d1 + p;
		  iv[r] = r1;
		  j1 = r1 + n;
		  iv[j] = j1;
		  rn = j1 - 1;

		  if ( iv[1] == 0 ) {
		    dfault ( iv, v );
		  }

		  iv[covreq] = -Math.abs ( iv[covreq] );
		  if ( iv[covprt] != 0 && iv[covreq] == 0 ) {
		    iv[covreq] = -1;
		  }

		  strted = true;

		  if (iv[1] != 12) {
			  do80 = true;
		  }
		  else {
              do10 = true;
			  strted = false;
			  iv[nfcall] = 1;
			  iv[nfgcal] = 1;
			//
			//  Initialize scale vector D to ones for computing initial jacobian.
			//
			  if ( 0 < iv[dtype] ) {
				for (m = 0; m < p; m++) {
			        v[d1+m] = 1.0;
				}
			  } // if (0 < iv[dtype]
		  } // else

        while (true) {
		  if (do10) {
          do10 = false;
		  nf = iv[nfcall];
          arr = new double[n+1];
          if (testMode) {
		      calcrTest ( n, p, x, nf, arr, uiparm, urparm);
          }
          else {
        	  calcr ( n, p, x, nf, arr, uiparm, urparm);  
          }
		  for (m = 1; m <= n; m++) {
			  v[r1+m-1] = arr[m];
		  }

		  if ( strted ) {

		    if ( nf <= 0 ) {
		      iv[toobig] = 1;
		    }

		    do80 = true;

		  } // if (strted)
		  else if ( nf <= 0 ) {
		    iv[1] = 13;
		    arr = new double[p+1];
		    for (m = 1; m <= p; m++) {
		    	arr[m] = v[d1+m-1];
		    }
		    itsmry ( arr, iv, p, v, x );
		    return;
		  }
		  else {
			  do30 = true;
		  }
		  } // if (do10)
		  if (do30) {
			  do30 = false;
		//
		//  Compute finite-difference jacobian.
		//

		  j1k = j1;
		  dk = d1;

		  for (k = 1; k <= p; k++) {

		    xk = x[k];
		    h = v[dltfdj] * Math.max ( Math.abs ( xk ), 1.0 / v[dk] );
		    dk = dk + 1;

		      x[k] = xk + h;
		      nf = iv[nfgcal];
		      arr = new double[n+1];
		      if (testMode) {
		          calcrTest ( n, p, x, nf, arr, uiparm, urparm);
		      }
		      else {
		    	  calcr ( n, p, x, nf, arr, uiparm, urparm);	  
		      }
		      for (m = 1; m <= n; m++) {
		    	  v[j1k+m-1] = arr[m];
		      }

		      if (nf <= 0) {

		      if ( hlim_nl2sno == 0.0 ) {
		        hlim_nl2sno = hfac * epsilon;
		      }

		      h = -0.5 * h;

		      if ( Math.abs ( h ) < hlim_nl2sno ) {
		        iv[1] = 15;
		        arr = new double[p+1];
		        for (m = 1; m <= p; m++) {
		        	arr[m] = v[d1+m-1];
		        }
		        itsmry ( arr, iv, p, v, x );
		        return;
		      } // if (Math.abs(h) < hlim_nl2sno)
		      } // if (nf <= 0)

		    x[k] = xk;

		    for (i = r1; i <= rn; i++) {
		      v[j1k] = ( v[j1k] - v[i] ) / h;
		      j1k = j1k + 1;
		    } // for (i = r1; i <= rn; i++)

		  } // for (k = 1; k <= p; k++)

		  strted = true;
		  do80 = true;
		  } // if (do30)

		if (do80) {
          do80 = false;
          arr = new double[p+1];
          for (m = 1; m <= p; m++) {
        	  arr[m] = v[d1+m-1];
          }
          arr2D = new double[n+1][p+1];
          index = 0;
          for (m = 1; m <= p; m++) {
        	  for (q = 1; q <= n; q++) {
        		  arr2D[q][m] = v[j1+index];
        		  index++;
        	  }
          }
          arr2 = new double[n+1];
          for (m = 1; m <= n; m++) {
        	  arr2[m] = v[r1+m-1];
          }
		  nl2itr ( arr, iv, arr2D, n, n, p, arr2, v, x );
		  for (m = 1; m <= p; m++) {
        	  v[d1+m-1] = arr[m];
          }
		  index = 0;
          for (m = 1; m <= p; m++) {
        	  for (q = 1; q <= n; q++) {
        		  v[j1+index] = arr2D[q][m];
        		  index++;
        	  }
          }
          for (m = 1; m <= n; m++) {
        	  v[r1+m-1] = arr2[m];
          }

		  if ( iv[1] < 2 ) {
		    do10 = true;
		  }
		  else if ( iv[1] == 2 ) {
		    do30 = true;
		  }
		  else {
		    return;
		  }
		} // if (do80)
	   } // loop1: while (true)
	} // nl2sno
	
	private void assess(double d[], int iv[], int p, double step[], double stlstg[], 
			            double v[], double x[], double x0[]) {
		/***********************************************************************
		!
		!! ASSESS assesses a candidate step.
		!
		!  Discussion:
		!
		!    This subroutine is called by an unconstrained minimization
		!    routine to assess the next candidate step.  It may recommend one
		!    of several courses of action, such as accepting the step, 
		!    recomputing it using the same or a new quadratic model, or 
		!    halting due to convergence or false convergence.  See the return 
		!    code listing below.
		!
		!    This routine is called as part of the NL2SOL (nonlinear
		!    least-squares) package.  It may be used in any unconstrained
		!    minimization solver that uses dogleg, Goldfeld-Quandt-Trotter,
		!    or Levenberg-Marquardt steps.
		!
		!    See Dennis, Gay and Welsch for further discussion of the assessing 
		!    and model switching strategies.  While NL2SOL considers only two 
		!    models, ASSESS is designed to handle any number of models.
		!
		!    On the first call of an iteration, only the I/O variables
		!    step, X, IV(IRC), IV(MODEL), V(F), V(DSTNRM), V(GTSTEP), and
		!    V(PREDUC) need have been initialized.  Between calls, no I/O
		!    values execpt STEP, X, IV(MODEL), V(G) and the stopping tolerances 
		!    should be changed.
		!
		!    After a return for convergence or false convergence, one can
		!    change the stopping tolerances and call ASSESS again, in which
		!    case the stopping tests will be repeated.
		!
		!  Modified:
		!
		!    04 April 2006
		!
		!  Author:
		!
		!    David Gay
		!
		!  Reference:
		!
		!    John Dennis, David Gay, Roy Welsch,
		!    An Adaptive Nonlinear Least Squares Algorithm,
		!    ACM Transactions on Mathematical Software,
		!    Volume 7, Number 3, 1981.
		!
		!    M J D Powell,
		!    A FORTRAN Subroutine for Solving Systems of Nonlinear Algebraic Equations,
		!    in Numerical Methods for Nonlinear Algebraic Equations, 
		!    edited by Philip Rabinowitz, 
		!    Gordon and Breach, London, 1970.
		!
		!  Parameters:
		!
		!     iv (i/o) integer parameter and scratch vector -- see description
		!             below of iv values referenced.
		!
		!    Input, real D(P), a scale vector used in computing V(RELDX).
		!
		!    Input, integer P, the number of parameters being optimized.
		!
		!   step (i/o) on input, step is the step to be assessed.  it is un-
		!             changed on output unless a previous step achieved a
		!             better objective function reduction, in which case stlstg
		!             will have been copied to step.
		!
		! stlstg (i/o) when assess recommends recomputing step even though the
		!             current (or a previous) step yields an objective func-
		!             tion decrease, it saves in stlstg the step that gave the
		!             best function reduction seen so far (in the current itera-
		!             tion).  if the recomputed step yields a larger function
		!             value, then step is restored from stlstg and
		!             x = x0 + step is recomputed.
		!
		!      v (i/o) real parameter and scratch vector -- see description
		!             below of v values referenced.
		!
		!      x (i/o) on input, x = x0 + step is the point at which the objec-
		!             tive function has just been evaluated.  if an earlier
		!             step yielded a bigger function decrease, then x is
		!             restored to the corresponding earlier value.  otherwise,
		!             if the current step does not give any function decrease,
		!             then x is restored to x0.
		!
		!     x0 (in)  initial objective function parameter vector (at the
		!             start of the current iteration).
		!
		!  iv values referenced
		!
		!    iv(irc) (i/o) on input for the first step tried in a new iteration,
		!             iv(irc) should be set to 3 or 4 (the value to which it is
		!             set when step is definitely to be accepted).  on input
		!             after step has been recomputed, iv(irc) should be
		!             unchanged since the previous return of assess.
		!                on output, iv(irc) is a return code having one of the
		!             following values...
		!                  1 = switch models or try smaller step.
		!                  2 = switch models or accept step.
		!                  3 = accept step and determine v(radfac) by gradient
		!                       tests.
		!                  4 = accept step, v(radfac) has been determined.
		!                  5 = recompute step (using the same model).
		!                  6 = recompute step with radius = v(lmax0) but do not
		!                       evaulate the objective function.
		!                  7 = x-convergence (see v(xctol)).
		!                  8 = relative function convergence (see v(rfctol)).
		!                  9 = both x- and relative function convergence.
		!                 10 = absolute function convergence (see v(afctol)).
		!                 11 = singular convergence (see v(lmax0)).
		!                 12 = false convergence (see v(xftol)).
		!                 13 = iv(irc) was out of range on input.
		!             return code i has precdence over i+1 for i = 9, 10, 11.
		! iv(mlstgd) (i/o) saved value of iv(model).
		!  iv(model) (i/o) on input, iv(model) should be an integer identifying
		!             the current quadratic model of the objective function.
		!             if a previous step yielded a better function reduction,
		!             then iv(model) will be set to iv(mlstgd) on output.
		! iv(nfcall) (in)  invocation count for the objective function.
		! iv(nfgcal) (i/o) value of iv(nfcall) at step that gave the biggest
		!             function reduction this iteration.  iv(nfgcal) remains
		!             unchanged until a function reduction is obtained.
		! iv(radinc) (i/o) the number of radius increases (or minus the number
		!             of decreases) so far this iteration.
		! iv(restor) (out) set to 0 unless x and v(f) have been restored, in
		!             which case assess sets iv(restor) = 1.
		!  iv(stage) (i/o) count of the number of models tried so far in the
		!             current iteration.
		! iv(stglim) (in)  maximum number of models to consider.
		! iv(switch) (out) set to 0 unless a new model is being tried and it
		!             gives a smaller function value than the previous model,
		!             in which case assess sets iv(switch) = 1.
		! iv(toobig) (in)  is nonzero if step was too big (e.g. if it caused
		!             overflow).
		!   iv(xirc) (i/o) value that iv(irc) would have in the absence of
		!             convergence, false convergence, and oversized steps.
		!
		!  v values referenced
		!
		! v(afctol) (in)  absolute function convergence tolerance.  if the
		!             absolute value of the current function value v(f) is less
		!             than v(afctol), then assess returns with iv(irc) = 10.
		! v(decfac) (in)  factor by which to decrease radius when iv(toobig) is
		!             nonzero.
		! v(dstnrm) (in)  the 2-norm of d * step.
		! v(dstsav) (i/o) value of v(dstnrm) on saved step.
		!   v(dst0) (in)  the 2-norm of d times the Newton step (when defined,
		!             i.e., for v(nreduc) >= 0).
		!      v(f) (i/o) on both input and output, v(f) is the objective func-
		!             tion value at x.  if x is restored to a previous value,
		!             then v(f) is restored to the corresponding value.
		!   v(fdif) (out) the function reduction v(f0) - v(f) (for the output
		!             value of v(f) if an earlier step gave a bigger function
		!             decrease, and for the input value of v(f) otherwise).
		! v(flstgd) (i/o) saved value of v(f).
		!     v(f0) (in)  objective function value at start of iteration.
		! v(gtslst) (i/o) value of v(gtstep) on saved step.
		! v(gtstep) (in)  inner product between step and gradient.
		! v(incfac) (in)  minimum factor by which to increase radius.
		!  v(lmax0) (in)  maximum reasonable step size (and initial step bound).
		!             if the actual function decrease is no more than twice
		!             what was predicted, if a return with iv(irc) = 7, 8, 9,
		!             or 10 does not occur, if v(dstnrm) > v(lmax0), and if
		!             v(preduc) <= v(rfctol) * abs(v(f0)), then assess re-
		!             turns with iv(irc) = 11.  if so doing appears worthwhile,
		!             then assess repeats this test with v(preduc) computed for
		!             a step of length v(lmax0) (by a return with iv(irc) = 6).
		! v(nreduc) (i/o)  function reduction predicted by quadratic model for
		!             Newton step.  if assess is called with iv(irc) = 6, i.e.,
		!             if v(preduc) has been computed with radius = v(lmax0) for
		!             use in the singular convervence test, then v(nreduc) is
		!             set to -v(preduc) before the latter is restored.
		! v(plstgd) (i/o) value of v(preduc) on saved step.
		! v(preduc) (i/o) function reduction predicted by quadratic model for
		!             current step.
		! v(radfac) (out) factor to be used in determining the new radius,
		!             which should be v(radfac)*dst, where  dst  is either the
		!             output value of v(dstnrm) or the 2-norm of
		!             diag(newd) * step  for the output value of step and the
		!             updated version, newd, of the scale vector d.  for
		!             iv(irc) = 3, v(radfac) = 1.0 is returned.
		! v(rdfcmn) (in)  minimum value for v(radfac) in terms of the input
		!             value of v(dstnrm) -- suggested value = 0.1.
		! v(rdfcmx) (in)  maximum value for v(radfac) -- suggested value = 4.0.
		!  v(reldx) (out) scaled relative change in x caused by step, computed
		!             by function  reldst  as
		!                 max (d(i)*abs(x(i)-x0(i)), 1 <= i <= p) /
		!                    max (d(i)*(abs(x(i))+abs(x0(i))), 1 <= i <= p).
		!             if an acceptable step is returned, then v(reldx) is com-
		!             puted using the output (possibly restored) values of x
		!             and step.  otherwise it is computed using the input
		!             values.
		! v(rfctol) (in)  relative function convergence tolerance.  if the
		!             actual function reduction is at most twice what was pre-
		!             dicted and  v(nreduc) <= v(rfctol)*abs(v(f0)),  then
		!             assess returns with iv(irc) = 8 or 9.  see also v(lmax0).
		! v(STPPAR) (in)  Marquardt parameter -- 0 means full Newton step.
		! v(tuner1) (in)  tuning constant used to decide if the function
		!             reduction was much less than expected.  suggested
		!             value = 0.1.
		! v(tuner2) (in)  tuning constant used to decide if the function
		!             reduction was large enough to accept step.  suggested
		!             value = 10**-4.
		! v(tuner3) (in)  tuning constant used to decide if the radius
		!             should be increased.  suggested value = 0.75.
		!  v(xctol) (in)  x-convergence criterion.  if step is a Newton step
		!             (v(STPPAR) = 0) having v(reldx) <= v(xctol) and giving
		!             at most twice the predicted function decrease, then
		!             assess returns iv(irc) = 7 or 9.
		!  v(xftol) (in)  false convergence tolerance.  if step gave no or only
		!             a small function decrease and v(reldx) <= v(xftol),
		!             then assess returns with iv(irc) = 12.
		*/
		final int afctol = 31;
		final int decfac = 22;
		double emax;
		boolean goodx;
		double gts;
		int i;
		int j;
		final int irc = 3;
		final int lmax0 = 35;
		int nfc;
		final int nreduc = 6;
		final int plstgd = 15;
		final int preduc = 7;
		final int radfac = 16;
		final int rdfcmn = 24;
		final int rdfcmx = 25;
		final int reldx = 17;
		double reldx1;
		double rfac1;
		final int rfctol = 32;
		final int stppar = 5;
		final int tuner1 = 26;
		final int tuner2 = 27;
		final int tuner3 = 28;
		final int xctol = 33;
		final int xftol = 34;
		final int xirc = 13;
		double xmax;
		
		// subscripts for iv and v
		final int mlstgd = 4;
		final int model = 5;
		final int nfcall = 6;
		final int nfgcal = 7;
		final int radinc = 8;
		final int restor = 9;
		final int stage = 10;
		final int stglim = 11;
		final int switchConstant = 12;
		final int toobig = 2;
		final int dstnrm = 2;
		final int dst0 = 3;
		final int dstsav = 18;
		final int f = 10;
		final int fdif = 11;
		final int flstgd = 12;
		final int f0 = 13;
		final int gtslst = 14;
		final int gtstep = 4;
		final int incfac = 23;
		
		boolean do10 = false;
		boolean do20 = false;
		boolean do30 = false;
		boolean do40 = false;
		boolean do50 = false;
		boolean do70 = false;
		boolean do80 = false;
		boolean do90 = false;
		boolean do140 = false;
		boolean do160 = false;
		boolean do200 = false;
		boolean do230 = false;
		boolean do260 = false;
		boolean do290 = false;
		boolean do300 = false;
		boolean do310 = false;
		boolean do360 = false;
		
		nfc = iv[nfcall];
		iv[switchConstant] = 0;
		iv[restor] = 0;
		rfac1 = 1.0;
		goodx = true;
		i = iv[irc];
		
		if ((i < 1) || (i > 12)) {
			iv[irc] = 13;
			return;
		}
		
	    switch(i) {
		    case 1:
		    	do20 = true;
		    	break;
		    case 2:
		    	do30 = true;
		    	break;
		    case 3:
		    case 4:
		    	do10 = true;
		    	break;
		    case 5:
		    	do40 = true;
		    	break;
		    case 6:
		    	do360 = true;
		    	break;
		    case 7:
		    case 8:
		    case 9:
		    case 10:
		    case 11:
		    	do290 = true;
		    	break;
		    case 12:
		    	do140 = true; 
	    } // switch(i)
	    
	    while (true) {
	    	
	    	if (do10) {
	    		// Initialize for new iteration
	    		do10 = false;
	    	    iv[stage] = 1;
	    	    iv[radinc] = 0;
	    	    v[flstgd] = v[f0];
	    	    
	    	    if (iv[toobig] != 0) {
	    	    	iv[stage] = -1;
	    	    	iv[xirc] = i;
	    	    	v[radfac] = v[decfac];
	    	    	iv[radinc] = iv[radinc] - 1;
	    	    	iv[irc] = 5;
	    	    	return;
	    	    }
	    	    do90 = true;
	    	} // if (do10)
	    	
	    	if (do20) {
	    		do20 = false;
	    		// Step was recomputed with new model or smaller radius.
	    		// First decide which
	    		
	    		// Old model retained, smaller radius tried.
	    		// Do not consider any more new models this iteration.
	    		
	    		if (iv[model] == iv[mlstgd]) {
	    			iv[stage] = iv[stglim];
	    			iv[radinc] = -1;
	    			do90 = true;
	    		} // if (iv[model] == iv[mlstgd])
	    		else {
	    		    do30 = true;
	    		}
	    	} // if (do20)
	    	
	    	if (do30) {
	    		do30 = false;
	    		// A new model is being tried.  Decide whether to keep it.
	    		iv[stage] = iv[stage] + 1;
	    		do40 = true;
	    	} // if (do30)
	    	
	    	if (do40) {
	    		do40 = false;
	    		// Now we add the possibility that step was recomputed with
	    		// the same model, perhaps because of an oversized step.
	    		if (iv[stage] > 0) {
	    			do50 = true;
	    		}
	    		else if (iv[toobig] != 0) {
	    			v[radfac] = v[decfac];
	    			iv[radinc] = iv[radinc] - 1;
	    			iv[irc] = 5;
	    			return;
	    		} // else if (iv[toobig] != 0)
	    		else {
	    			// Restore iv[stage] adn pick up where we left off.
	    			iv[stage] = -iv[stage];
	    			i = iv[xirc];
	    			switch(i) {
		    			case 1:
		    				do20 = true;
		    				break;
		    			case 2:
		    				do30 = true;
		    				break;
		    			case 3:
		    			case 4:
		    				do90 = true;
		    				break;
		    			case 5:
		    				do70 = true;
		    				break;
	    			    default:
	    			    	do50 = true;
	    			} // switch(i)
	    		}// else
	    	} // if (do40)
	    	
	    	if (do50) {
	    		do50 = false;
	    		if (iv[toobig] == 0) {
	    			do70 = true;
	    		}
	    		else if (iv[radinc] <= 0) {
	    			// Handle oversize step.
	    			iv[stage] = -iv[stage];
	    			iv[xirc] = iv[irc];
	    			v[radfac] = v[decfac];
	    			iv[radinc] = iv[radinc] - 1;
	    			iv[irc] = 5;
	    			return;
	    		} // else if (iv[radinc] <= 0)
	    		else {
	    			do80 = true;
	    		}
	    	} // if (do50)
	    	
	    	if (do70) {
	    		do70 = false;
	    		if (v[f] < v[flstgd]) {
	    			do90 = true;
	    		}
	    		// The new step is a loser.  Restore old model.
	    		else {
	    			do80 = true;
	    			if (iv[model] != iv[mlstgd]) {
	    				iv[model] = iv[mlstgd];
	    				iv[switchConstant] = 1;
	    			}
	    		}
	    	} // if (do70)
	    	
	    	if (do80) {
	    		do80 = false;
	    		do90 = true;
	    		// Restore step, etc. only if a previous step decreased V(F).
	    		if (v[flstgd] < v[f0]) {
	                iv[restor] = 1;
	                v[f] = v[flstgd];
	                v[preduc] = v[plstgd];
	                v[gtstep] = v[gtslst];
	                if (iv[switchConstant] == 0) {
	                	rfac1 = v[dstnrm]/v[dstsav];
	                }
	                v[dstnrm] = v[dstsav];
	                nfc = iv[nfgcal];
	                goodx = false;
	    		} // if (v[flstgd] < v[f0])
	    	} // if (do80)
	    	
	    	if (do90) {
	    		do90 = false;
	    		// Compute relative change in X by current step.
	    		reldx1 = reldst(p, d, x, x0);
	    		
	    		// Restore X and STEP if necessary.
	    		if (!goodx) {
	    		    for (j = 1; j <= p; j++) {
	    		    	step[j] = stlstg[j];
	    		    	x[j] = x0[j] + stlstg[j];
	    		    }
	    		} // if (!goodx)
	    		
	    		v[fdif] = v[f0] - v[f];
	    		// No (or only a trivial) function decrease,
	    		// so try new model or smaller radius.
	    		if (v[fdif] <= v[tuner2] * v[preduc]) {
	    			v[reldx] = reldx1;
	    			
	    			if (v[f0] <= v[f]) {
	    				iv[mlstgd] = iv[model];
	    				v[flstgd] = v[f];
	    				v[f] = v[f0];
	    				for (j = 1; j <= p; j++) {
	    					x[j] = x0[j];
	    				}
	    				iv[restor] = 1;
	    			} // if (v[f0] <= v[f])
	    			else {
	    				iv[nfgcal] = nfc;
	    			}
	    			
	    			iv[irc] = 1;
	    			if (iv[stglim] <= iv[stage]) {
	    				iv[irc] = 5;
	    				iv[radinc] = iv[radinc] - 1;
	    			} // if (iv[stglim] <= iv[stage] 
	    		} // if (v[fdif] <= v[tuner2] * v[preduc])
	    		else {
	    			// Nontrivial function decrease achieved.
	    			iv[nfgcal] = nfc;
	    			rfac1 = 1.0;
	    			if (goodx) {
	    				v[reldx] = reldx1;
	    			}
	    			v[dstsav] = v[dstnrm];
	    			
	    			if (v[preduc] * v[tuner1] < v[fdif]) {
	    				do200 = true;
	    			}
	    			// Decrease was much less than predicted: either change models
	    			// or accept step with decreased radius.
	    			else if (iv[stage] < iv[stglim]) {
	    				iv[irc] = 2;
	    			}
	    			else {
	    				iv[irc] = 4;
	    			}
	    		} // else
	    		if (!do200) {
	    			do140 = true;
	    			// Set V[radfac] to Fletcher's decrease factor.
	    			iv[xirc] = iv[irc];
	    			emax = v[gtstep] + v[fdif];
	    			v[radfac] = 0.5 * rfac1;
	    			
	    			if (emax < v[gtstep]) {
	    				v[radfac] = rfac1 * Math.max(v[rdfcmn], 0.5 * v[gtstep] / emax);
	    			}
	    		} // if (!do200)
	    	} // if (do90)
	    	
	    	if (do140) {
	    		do140 = false;
	    		// Do a false convergence test
	    		if (v[reldx] < v[xftol]) {
	    			do160 = true;
	    		}
	    		else {
	    			iv[irc] = iv[xirc];
	    			if (v[f] < v[f0]) {
	    				do230 = true;
	    			}
	    			else {
	    				do300 = true;
	    			}
	    		} // else
	    	} // if (do140)
	    	
	    	if (do160) {
	    		do160 = false;
	    		iv[irc] = 12;
	    		do310 = true;
	    	} // if (do160)
	    	
	    	if (do200) {
	    		do200 = false;
	    		// Handle good function decrease,
	    		if (v[fdif] < (-v[tuner3] * v[gtstep])) {
	    			do260 = true;
	    		}
	    		// Increasing radius looks worthwhile.  See if we just
	    		// recomputed step with a decreased radius or restored step
	    		// after recomputing it with a larger radius.
	    		else if (iv[radinc] < 0) {
	    			do260 = true;
	    		}
	    		else if (iv[restor] == 1) {
	    			do260 = true;
	    		}
	    		else {
	    			// We did not.  Try a longer step unless this was a Newton step.
	    			v[radfac] = v[rdfcmx];
	    			gts = v[gtstep];
	    			if (v[fdif] < (0.5/v[radfac] - 1.0) * gts) {
	    				v[radfac] = Math.max(v[incfac], 0.5 * gts/(gts + v[fdif]));
	    			}
	    			iv[irc] = 4;
	    			
	    			if (v[stppar] == 0.0) {
	    				do300 = true;
	    			}
	    			else {
	    				do230 = true;
	    				// Step was not a Newton step.  Recompute it with a larger radius.
	    				iv[irc] = 5;
	    				iv[radinc] = iv[radinc] + 1;
	    			}
	    		} // else
	    	} // if (do200)
	    	
	    	if (do230) {
	    		do230 = false;
	    	    // Save values corresponding to good step.
	    		v[flstgd] = v[f];
	    		iv[mlstgd] = iv[model];
	    		for (j = 1; j <= p; j++) {
	    			stlstg[j] = step[j];
	    		}
	    		v[dstsav] = v[dstnrm];
	    		iv[nfgcal] = nfc;
	    		v[plstgd] = v[preduc];
	    		v[gtslst] = v[gtstep];
	    		do300 = true;
	    	} // if (do230)
	    	
	    	if (do260) {
	    		do260 = false;
	    		// Accept step with radius unchanged.
	    		v[radfac] = 1.0;
	    		iv[irc] = 3;
	    		do300 = true;
	    	} // if (do260)
	    	
	    	if (do290) {
	    		do290 = false;
	    		// Come here for a restart after convergence.
	    		iv[irc] = iv[xirc];
	    		if (v[dstsav] < 0.0) {
	    			iv[irc] = 12;
	    		}
	    		do310 = true;
	    	} // if (do290)
	    	
	    	// Perform convergence tests.
	    	
	    	if (do300) {
	    		do300 = false;
	    		iv[xirc] = iv[irc];
	    		do310 = true;
	    	} // if (do300)
	    	
	    	if (do310) {
	    		do310 = false;
	    		if (Math.abs(v[f]) < v[afctol]) {
	    			iv[irc] = 10;
	    		}
	    		
	    		if (0.5 * v[fdif] > v[preduc]) {
	    			return;
	    		}
	    		
	    		emax = v[rfctol] * Math.abs(v[f0]);
	    		
	    		if (v[dstnrm] > v[lmax0] && v[preduc] <= emax) {
	    			iv[irc] = 11;
	    		}
	    		
	    		if (0.0 <= v[dst0]) {
	    			if ((v[nreduc] > 0.0 && v[nreduc] <= emax) ||
	    			    (v[nreduc] == 0.0 && v[preduc] == 0.0)) {
	    			    i = 2;
	    			}
	    			else {
	    				i = 0;
	    			}
	    			
	    			if (v[stppar] == 0.0 && v[reldx] < v[xctol] && goodx) {
	    				i = i + 1;
	    			}
	    			
	    			if (i > 0) {
	    				iv[irc] = i + 6;
	    			}
	    		} // if (0.0 <= v[dst0])
	    		
	    		// Consider recomputing step of length V{LMAX0) for singular convergence tests.
	    		if (Math.abs(iv[irc]-3) > 2 && iv[irc] != 12) {
	    			return;
	    		}
	    		
	    		if (v[lmax0] < v[dstnrm]) {
	    		    if (0.5 * v[dstnrm] <= v[lmax0]) {
	    		    	return;
	    		    }
	    		    
	    		    xmax = v[lmax0]/v[dstnrm];
	    		    
	    		    if (emax <= xmax * (2.0 - xmax) * v[preduc]) {
	    		    	return;
	    		    }
	    		} // if (v[max0] < v[dstnrm])
	    		else {
	    			if (emax <= v[preduc]) {
	    				return;
	    			}
	    			
	    			if (0.0 < v[dst0]) {
	    				
	    				if (0.5 * v[dst0] <= v[lmax0]) {
	    					return;
	    				}
	    			} // if (0.0 < v[dst0])
	    		} // else
	    		
	    		if (v[nreduc] < 0.0) {
	    			if (-v[nreduc] <= v[rfctol] * Math.abs(v[f0])) {
	    				iv[irc] = 11;
	    			}
	    			return;
	    		} // if (v[nreduc] < 0.0)
	    		
	    		// Recompute V(PREDUC) for use in singular convergence test.
	    		v[gtslst] = v[gtstep];
	    		v[dstsav] = v[dstnrm];
	    		if (iv[irc] == 12) {
	    			v[dstsav] = -v[dstsav];
	    		}
	    		v[plstgd] = v[preduc];
	    		iv[irc] = 6;
	    		for (j = 1; j <= p; j++) {
	    			stlstg[j] = step[j];
	    		}
	    		return;
	    	} // if (do310)
	    	
	    	// Perform singular convergence test with recomputed V(PREDUC).
	    	if (do360) {
	    		do360 = false;
	    		v[gtstep] = v[gtslst];
	    		v[dstnrm] = Math.abs(v[dstsav]);
	    		for (j = 1; j <= p; j++) {
	    			step[j] = stlstg[j];
	    		}
	    		
	    		if (v[dstsav] <= 0.0) {
	    			iv[irc] = 12;
	    		}
	    		else {
	    			iv[irc] = iv[xirc];
	    		}
	    		
	    		v[nreduc] = -v[preduc];
	    		v[preduc] = v[plstgd];
	    		
	    		if (-v[nreduc] <= v[rfctol] * Math.abs(v[f0])) {
	    			iv[irc] = 11;
	    		}
	    		
	    		return;
	    	} // if (do360)
	    } // while (true)		
	} // assess
	
	// meqn, input, the number of functions
	// nvar, input, the number of variables
	// x, input of dimension nvar, the current value of the variables
	// nf, input, the number of times the residual routine has been called so far.
	// jac, output of dimension [meqn][nvar], the jacobian matrix. jac[i][j] is
	//      the derivative of function i with repsect to variable j.
	// uiparm, input, an integer user array
	// urparm, input, a double user array
	public abstract void calcj(int meqn, int nvar, double x[], int nf, double jac[][], 
			                   int uiparm[], double urparm[]);
	
	private void calcjTest(int n, int p, double x[], int nfcall, double jac[][],
			              int uiparm[], double urparm[]) {
		/*****************************************************************************80
		!
		!! TESTJ evaluates the jacobian matrix.
		!
		!  Discussion:
		!
		!    This routine evaluates the jacobian matrix J for the various
		!    test problems listed in the references.
		!
		!  Reference:
		!
		!    Philip Gill and Walter Murray,
		!    Algorithms for the Solution of the Non-linear Least-squares Problem, 
		!    SIAM Journal on Numerical Analysis,
		!    Volume 15, Number 5, pages 977-991, 1978.
		!
		!    R R Meyer, 
		!    Theoretical and Computational Aspects of Nonlinear Regression, 
		!    in Nonlinear Programming,
		!    edited by J B Rosen, O L Mangasarian, and K Ritter,
		!    pages 465-486,
		!    Academic Press, New York, 1970.
		!
		!    K M Brown,
		!    A Quadratically Convergent Newton-like Method Based upon 
		!    Gaussian Elimination,
		!    SIAM Journal on Numerical Analysis,
		!    Volume 6, pages 560-569, 1969.
		!
		!  Parameters:
		!
		!    Input, integer N, is the number of functions, the length of R,
		!    and the number of rows used in J.
		!
		!    Input, integer P, the number of variables.
		!
		!    Input, real X(P), the point at which the jacobian is to be evaluated.
		!
		!    Input, integer NFCALL, is the invocation count of TESTR.  It is
		!    not needed by this routine.
		!
		!    Output, real JAC(N,P), the jacobian matrix at X.
		!
		!    Input, integer UIPARM(1), contains the value of NEX, the index
		!    of the problem being solved.
		!
		!    Input, real URPARM(*), is a user parameter vector, which is
		!    not needed by this routine.
		!
		!    Input, external UFPARM, is the name of a user-chosen function,
		!    which is not needed here.
		*/
		  
		  double e;
		  int i;
		  int j;
		  int k;
		  int nex;
		  double r2;
		  double t;
		  double temp;
		  double theta;
		  double ti;
		  double tim1;
		  double tip1;
		  double tpi;
		  double tpim1;
		  double tpip1;
		  final double twopi = 2.0E+00 * 3.141592653589793;
		  double u;
		  double v;
		  double w;
		  double z;

		  nex = uiparm[1];

		  for (j = 1; j <= p; j++) {
		    for ( i = 1; i <= n; i++) {
		      jac[i][j] = 0.0;
		    }
		  } // for (j = 1; j <= p; j++)
		//
		//  Rosenbrock.
		//
		  if ( nex == 1 ) {

		    jac[1][1] = -20.0 * x[1];
		    jac[1][2] = 10.0;
		    jac[2][1] = -1.0;
		    jac[2][2] = 0.0;
		  }
		//
		// Helix.
		//
		  else if ( nex == 2 ) {

		    t = x[1]*x[1] + x[2]*x[2];
		    ti = 100.0 / ( twopi * t );
		    jac[1][1] = ti * x[2];
		    t = 10.0 / Math.sqrt ( t );
		    jac[2][1] = x[1] * t;
		    jac[3][1] = 0.0;
		    jac[1][2] = -ti * x[1];
		    jac[2][2] = x[2] * t;
		    jac[3][2] = 0.0;
		    jac[1][3] = 10.0;
		    jac[2][3] = 0.0;
		    jac[3][3] = 1.0;
		  }
		//
		//  Singular.
		//
		  else if ( nex == 3 ) {

		    jac[1][1] = 1.0;
		    jac[1][2] = 10.0;
		    jac[2][3] = Math.sqrt ( 5.0 );
		    jac[2][4] = -jac[2][3];
		    jac[3][2] = 2.0 * ( x[2] - 2.0 * x[3] );
		    jac[3][3] = -2.0 * jac[3][2];
		    jac[4][1] = Math.sqrt( 40.0 ) * ( x[1] - x[4] );
		    jac[4][4] = -jac[4][1];
		  }
		//
		//  Woods.
		//
		  else if ( nex == 4 ) {

		    jac[1][1] = -20.0 * x[1];
		    jac[1][2] = 10.0;
		    jac[2][1] = -1.0;
		    jac[3][4] = Math.sqrt ( 90.0 );
		    jac[3][3] = -2.0 * x[3] * jac[3][4];
		    jac[4][3] = -1.0;
		    jac[5][2] = Math.sqrt ( 9.9 );
		    jac[5][4] = jac[5][2];
		    jac[6][2] = Math.sqrt ( 0.2 );
		    jac[7][4] = jac[6][2];
		  }
		//
		//  Zangwill.
		//
		  else if ( nex == 5 ) {

		    for (k = 1; k <= 3; k++) {
		      for (i = 1; i <= 3; i++) {
		        jac[i][k] = 1.0;
		      }
		    }
		    jac[1][2] = -1.0;
		    jac[2][1] = -1.0;
		    jac[3][3] = -1.0;
		  }
		//
		//  Engvall.
		//
		  else if ( nex == 6 ) {

		    jac[1][1] = 2.0 * x[1];
		    jac[1][2] = 2.0 * x[2];
		    jac[1][3] = 2.0 * x[3];
		    jac[2][1] = jac[1][1];
		    jac[2][2] = jac[1][2];
		    jac[2][3] = 2.0 * ( x[3] - 2.0 );
		    jac[3][1] = 1.0;
		    jac[3][2] = 1.0;
		    jac[3][3] = 1.0;
		    jac[4][1] = 1.0;
		    jac[4][2] = 1.0;
		    jac[4][3] = -1.0;
		    t = 2.0 * ( 5.0 * x[3] - x[1] + 1.0 );
		    jac[5][1] = 3.0 * x[1]*x[1] - t;
		    jac[5][2] = 6.0 * x[2];
		    jac[5][3] = 5.0 * t;
		  }
		//
		//  Branin.
		//
		  else if ( nex == 7 ) {

		    jac[1][1] = 4.0;
		    jac[1][2] = 4.0;
		    jac[2][1] = 3.0 + (x[1] - 2.0 ) * ( 3.0 * x[1] - 2.0 * x[2] - 2.0 ) + x[2] * x[2];
		    jac[2][2] = 1.0 + 2.0 * ( 2.0 * x[1] - x[2] * x[2] ) - ( x[1] - x[2] )*(x[1] - x[2]);
		  }
		//
		//  Beale.
		//
		  else if ( nex == 8 ) {

		    jac[1][1] = x[2] - 1.0;
		    jac[1][2] = x[1];
		    jac[2][1] = x[2]*x[2] - 1.0;
		    jac[2][2] = 2.0 * x[1] * x[2];
		    jac[3][1] = x[2]*x[2]*x[2] - 1.0;
		    jac[3][2] = 3.0 * x[1] * x[2]*x[2];
		  }
		//
		//  Cragg and Levy.
		//
		  else if ( nex == 9 ) {
		 
		    t = Math.exp ( x[1] );
		    jac[1][2] = -2.0 * ( t - x[2] );
		    jac[1][1] = -t * jac[1][2];
		    jac[2][2] = 30.0 * ( x[2] - x[3] )*(x[2] - x[3]);
		    jac[2][3] = -jac[2][2];
		    temp = Math.cos(x[3] - x[4]);
		    jac[3][3] = 2.0 * Math.sin ( x[3] - x[4] ) /(temp*temp*temp);
		    jac[3][4] = -jac[3][3];
		    jac[4][1] = 4.0 * x[1]*x[1]*x[1];
		    jac[5][4] = 1.0;
		  }
		//
		//  Box.
		//
		  else if ( nex == 10 ) {

		    if ( expmin_calcjTest == 0.0 ) {
		      expmin_calcjTest = 1.999 * Math.log ( tiny );
		    }

		    for (i = 1; i <= 10; i++) {

		      ti = - 0.1 * i;

		      t = x[1] * ti;

		      if ( t < expmin_calcjTest ) {
		        e = 0.0;
		      }
		      else {
		        e = Math.exp ( t );
		      }

		      jac[i][1] = ti * e;

		      t = x[2] * ti;
		      if ( t < expmin_calcjTest ) {
		        e = 0.0;
		      }
		      else {
		        e = Math.exp ( t );
		      }

		      jac[i][2] = -ti * e;
		      jac[i][3] = Math.exp ( 10.0 * ti ) - Math.exp ( ti );

		    } // for (i = 1; i <= 10; i++)
		  }
		//
		//  Davidon 1.
		//
		  else if ( nex == 11 ) {

		    for (i = 1; i <= n-1; i++) {
		      ti = (double) i;
		      t = 1.0;
		      for (k = 1; k <= p; k++) {
		        jac[i][k] = t;
		        t = t * ti;
		      }
		    }

		    jac[n][1] = 1.0;
		    for  (k = 2; k <= p; k++) {
		      jac[n][k] = 0.0;
		    }
		  }
		//
		//  Freudenstein and Roth.
		//
		  else if ( nex == 12 ) {

		    jac[1][1] = 1.0;
		    jac[1][2] = -2.0 + x[2] * ( 10.0 - 3.0 * x[2] );
		    jac[2][1] = 1.0;
		    jac[2][2] = -14.0 + x[2] * ( 2.0 + 3.0 * x[2] );
		  }
		//
		//  Watson6.
		//  Watson9.
		//  Watson12.
		//  Watson20.
		//
		  else if ( nex >= 13 && nex <= 16 ) {

		    for (i = 1; i <= 29; i++) {
		      ti = ((double) ( i)) / 29.0;
		      r2 = x[1];
		      t = 1.0;
		      for (k = 2; k <= p; k++) {
		        t = t * ti;
		        r2 = r2 + t * x[k];
		      }
		      r2 = -2.0 * r2;
		      jac[i][1] = r2;
		      t = 1.0;
		      r2 = ti * r2;
		      for (k = 2; k <= p; k++) {
		        jac[i][k] = t * (( k - 1.0) + r2 );
		        t = t * ti;
		      }
		    } // for (i = 1; i <= 29; i++)

		    jac[30][1] = 1.0;
		    jac[31][1] = -2.0 * x[1];
		    jac[31][2] = 1.0;
		  }
		//
		//  Chebyquad.
		//
		  else if ( nex == 17 ) {

		    for (k = 1; k <= n; k++) {
		      tim1 = -1.0 / ((double)n);
		      z = 2.0 * x[k] - 1.0;
		      ti = z * tim1;
		      tpim1 = 0.0;
		      tpi = 2.0 * tim1;
		      z = z + z;
		      for (i = 1; i <= n; i++) {
		        jac[i][k] = tpi;
		        tpip1 = 4.0 * ti + z * tpi - tpim1;
		        tpim1 = tpi; 
		        tpi = tpip1;
		        tip1 = z * ti - tim1;
		        tim1 = ti;
		        ti = tip1;
		      } // for (i = 1; i <= n; i++)
		    } // for (k = 1; k <= n; k++)
		  }
		//
		//  Brown and Dennis.
		//
		  else if ( nex == 18 ) {

		    for (i = 1; i <= n; i++) {
		      ti = 0.2 * i;
		      jac[i][1] = 2.0 * ( x[1] + x[2] * ti - Math.exp ( ti ) );
		      jac[i][2] = ti * jac[i][1];
		      t = Math.sin ( ti );
		      jac[i][3] = 2.0 * ( x[3] + x[4] * t - Math.cos ( ti ) );
		      jac[i][4] = t * jac[i][3];
		    } // for (i = 1; i <= n; i++)
		  }
		//
		//  Bard.
		//
		  else if ( nex == 19 ) {

		    for (i = 1; i <= 15; i++) {
		      jac[i][1] = -1.0;
		      u = (double)i;
		      v = 16.0 - u;
		      w = Math.min ( u, v );
		      temp = x[2] * v + x[3] * w;
		      t = u / (temp*temp);
		      jac[i][2] = v * t;
		      jac[i][3] = w * t;
		    } // for (i = 1; i <= 15; i++)
		  }
		//
		//  Jennrich and Sampson.
		//
		  else if ( nex == 20 ) {

		    for (i = 1; i <= 10; i++) {
		      ti = (double)i;
		      jac[i][1] = -ti * Math.exp ( ti * x[1] );
		      jac[i][2] = -ti * Math.exp ( ti * x[2] );
		    }
		  }
		//
		//  Kowalik and Osborne.
		//
		  else if ( nex == 21 ) {

		    for (i = 1; i <= 11; i++) {
		      t = -1.0 / ( ukow_calcjTest[i]*ukow_calcjTest[i] + x[3] * ukow_calcjTest[i] + x[4] );
		      jac[i][1] = t * ( ukow_calcjTest[i]*ukow_calcjTest[i] + x[2] * ukow_calcjTest[i] );
		      jac[i][2] = x[1] * ukow_calcjTest[i] * t;
		      t = t * jac[i][1] * x[1];
		      jac[i][3] = ukow_calcjTest[i] * t;
		      jac[i][4] = t;
		    } // for (i = 1; i <= 11; i++)
		  }
		//
		//  Osborne 1.
		//
		  else if ( nex == 22 ) {

		    for (i = 1; i <= 33; i++) {
		      ti = 10.0 * ( 1.0 - i);
		      jac[i][1] = -1.0;
		      jac[i][2] = -Math.exp ( x[4] * ti );
		      jac[i][3] = -Math.exp ( x[5] * ti );
		      jac[i][4] = ti * x[2] * jac[i][2];
		      jac[i][5] = ti * x[3] * jac[i][3];
		    } // for (i = 1; i <= 33; i++)
		  }
		//
		//  Osborne 2.
		//
		//  UFTOLG is a machine-dependent constant.  It is just slightly
		//  larger than the log of the smallest positive machine number.
		//
		  else if ( nex == 23 ) {

		    if ( uftolg_calcjTest == 0.0 ) {
		      uftolg_calcjTest = 1.999 * Math.log ( tiny  );
		    }

		    for (i = 1; i <= 65; i++) {
		      ti = ( 1.0 - i) * 0.1;
		      jac[i][1] = -Math.exp ( x[5] * ti );
		      jac[i][5] = x[1] * ti * jac[i][1];
		      for (k = 2; k <= 4; k++) {
		        t = x[k + 7] + ti;
		        theta = -x[k+4] * t * t;
		        if ( theta <= uftolg_calcjTest ) {
		          r2 = 0.0;
		        }
		        else {
		          r2 = -Math.exp ( theta );
		        }
		        jac[i][k] = r2;
		        r2 = -t * r2 * x[k];
		        jac[i][k+4] = r2 * t;
		        jac[i][k+7] = 2.0 * x[k+4] * r2;
		      } // for (k = 2; k <= 4; k++)
		    } // for (i = 1; i <= 65; i++)
		  }
		//
		//  Madsen.
		//
		  else if ( nex == 24 ) {

		    jac[1][1] = 2.0 * x[1] + x[2];
		    jac[1][2] = 2.0 * x[2] + x[1];
		    jac[2][1] = Math.cos ( x[1] );
		    jac[2][2] = 0.0;
		    jac[3][1] = 0.0;
		    jac[3][2] = -Math.sin ( x[2] );
		  }
		//
		//  Meyer.
		//
		  else if ( nex == 25 ) {

		    for (i = 1; i <= 16; i++) {
		      ti = 5.0 * i + 45.0;
		      u = ti + x[3];
		      t = Math.exp ( x[2] / u );
		      jac[i][1] = t;
		      jac[i][2] = x[1] * t / u;
		      jac[i][3] = -x[1] * x[2] * t / ( u * u );
		    } // for (i = 1; i <= 16; i++)
		  }
		//
		//  Brown5.
		//  Brown10.
		//  Brown30.
		//  Brown40.
		//
		  else if ( nex >= 26 && nex <= 29) {

		    for (k = 1; k <= n; k++) {
		      for (i = 1; i <= n-1; i++) {
		        if ( i == k ) {
		          jac[i][k] = 2.0;
		        }
		        else {
		          jac[i][k] = 1.0;
		        }
		      }
		    } // for (k = 1; k <= n; k++)

		    for (k = 1; k <= n; k++) {
		      t = 1.0;
		      for (i = 1; i <= n; i++) {
		        if ( i != k ) {
		          t = t * x[i];
		        }
		      }
		      jac[n][k] = t;
		    } // for (k = 1; k <= n; k++)
		  }
		//
		//  Bard + 10.
		//
		  else if ( nex == 30 ) {

		    for (i = 1; i <= 15; i++) {
		      jac[i][1] = -1.0;
		      u = (double)i;
		      v = 16.0 - u;
		      w = Math.min ( u, v );
		      temp = x[2] * v + x[3] * w;
		      t = u / ( temp * temp);
		      jac[i][2] = v * t;
		      jac[i][3] = w * t;
		    } // for (i = 1; i <= 15; i++)
		  }
		//
		//  Kowalik and Osborne + 10.
		//
		  else if ( nex == 31 ) {

		    for (i = 1; i <= 11; i++) {
		      t = -1.0 / ( ukow_calcjTest[i]*ukow_calcjTest[i] + x[3] * ukow_calcjTest[i] + x[4] );
		      jac[i][1] = t * ( ukow_calcjTest[i]*ukow_calcjTest[i] + x[2] * ukow_calcjTest[i] );
		      jac[i][2] = x[1] * ukow_calcjTest[i] * t;
		      t = t * jac[i][1] * x[1];
		      jac[i][3] = ukow_calcjTest[i] * t;
		      jac[i][4] = t;
		    } // for (i = 1; i <= 11; i++)
		  }
		//
		//  Meyer + 10.
		//
		  else if ( nex == 32 ) {

		    for (i = 1; i <= 16; i++) {
		      ti = 5.0 * i + 45.0;
		      u = ti + x[3];
		      t = Math.exp ( x[2] / u );
		      jac[i][1] = t;
		      jac[i][2] = x[1] * t / u;
		      jac[i][3] = -x[1] * x[2] * t / ( u * u );
		    } // for (i = 1; i <= 16; i++)
		  }
		//
		//  Watson6 + 10.
		//  Watson9 + 10.
		//  Watson12 + 10.
		//  Watson20 + 10.
		//
		  else if ( nex >= 33 && nex <= 36) {

		    for  (i = 1; i <= 29; i++) {
		      ti = ((double)i)/ 29.0;
		      r2 = x[1];
		      t = 1.0;
		      for  (k = 2; k <= p; k++) {
		        t = t * ti;
		        r2 = r2 + t * x[k];
		      }
		      r2 = -2.0 * r2;
		      jac[i][1] = r2;
		      t = 1.0;
		      r2 = ti * r2;
		      for  (k = 2; k <= p; k++) {
		        jac[i][k] = t * (( k - 1.0) + r2 );
		        t = t * ti;
		      }
		    } // for (i = 1; i <= 29; i++)

		    jac[30][1] = 1.0;
		    jac[31][1] = -2.0 * x[1];
		    jac[31][2] = 1.0;
		  }

		  else {

		    System.out.println("calcjTest - fatal error");
		    Preferences.debug("calcjTest - fatal error\n", Preferences.DEBUG_ALGORITHM);
		    System.out.println("Illegal index nex = " + nex);
		    Preferences.debug("Illegal index nex = " + nex + "\n", Preferences.DEBUG_ALGORITHM);
		    System.exit(-1);

		  }

		  return;
		
	} // private void calcjTest
	
	// meqn, input, the number of functions
	// nvar, input, the number of variables
	// x, input of dimension nvar, the current value of the variables
	// nf, input, the number of times the residual routine has been called so far.
	// r, output of dimension meqn, the residual vector, that is, the value of the
	//    functions for the given input value of the variables.
	// uiparm, input, an integer user array
	// urparm, input, a double user array
	public abstract void calcr(int meqn, int nvar, double x[], int nf, double r[], 
			                   int uiparm[], double urparm[]);
	
	private void calcrTest(int n, int p, double x[], int nfcall, double r[],
			              int uiparm[], double urparm[]) {
		/*****************************************************************************80
		!
		!! TESTR evaluates the residual function.
		!
		!  Discussion:
		!
		!    This routine evaluates the residual vector R for the various 
		!    test functions in the references, as well as for some variations 
		!    suggested by Jorge More in a private communication
		!    on some of these test problems, for 30 <= NEX.
		!
		!  Modified:
		!
		!    28 March 2006
		!
		!  Reference:
		!
		!    Philip Gill and Walter Murray,
		!    Algorithms for the Solution of the Non-linear Least-squares Problem, 
		!    SIAM Journal on Numerical Analysis,
		!    Volume 15, Number 5, pages 977-991, 1978.
		!
		!    R R Meyer, 
		!    Theoretical and Computational Aspects of Nonlinear Regression, 
		!    in Nonlinear Programming,
		!    edited by J B Rosen, O L Mangasarian, and K Ritter,
		!    pages 465-486,
		!    Academic Press, New York, 1970.
		!
		!    K M Brown,
		!    A Quadratically Convergent Newton-like Method Based upon 
		!    Gaussian Elimination,
		!    SIAM Journal on Numerical Analysis,
		!    Volume 6, pages 560-569, 1969.
		!
		!  Parameters:
		!
		!    Input, integer N, the number of equations or functions.
		!
		!    Input, integer P, the number of variables.
		!
		!    Input, real X(P), the point at which the residual vector
		!    is to be evaluated.
		!
		!    Input/output, integer NFCALL; on input, the invocation count 
		!    of this routine.  In exceptional cases, NFCALL may be reset to
		!    -1 on output to indicate an error occurred which prevented the
		!    evaluation of R.
		!
		!    Input, integer UIPARM(1), contains the value of NEX, the index
		!    of the problem being solved.
		!
		!    Input, real URPARM(*), is a user parameter vector, which is
		!    not needed by this routine.
		!
		!    Input, external UFPARM, is the name of a user-chosen function,
		!    which is not needed here.
		!
		!    Output, real R(N), the residual vector at X.
		*/

		  double e1;
		  double e2;
		  int i;
		  int j;
		  int nex;
		  double r1;
		  double r2;
		  double ri;
		  double t;
		  double t1;
		  double t2;
		  double theta;
		  double ti;
		  double tim1;
		  double tip1;
		  final double twopi = 2.0 * 3.141592653589793;
		  double u;
		  double v;
		  double w;
		  double z;
		  double temp;
		  double temp2;
		  double sum;
		  double prod;

		  nex = uiparm[1];
		//
		//  Rosenbrock.
		//
		  if ( nex == 1 ) {

		    r[1] = 10.0 * ( x[2] - x[1]*x[1] );
		    r[2] = 1.0 - x[1];
		  }
		//
		//  Helix.
		//
		  else if ( nex == 2 ) {

		    theta = Math.atan2 ( x[2], x[1] ) / twopi;

		    if ( x[1] <= 0.0 && x[2] <= 0.0 ) {
		      theta = theta + 1.0;
		    }

		    r[1] = 10.0 * ( x[3] - 10.0 * theta );
		    r[2] = 10.0 * ( Math.sqrt ( x[1]*x[1] + x[2]*x[2] ) - 1.0 );
		    r[3] = x[3];
		  }
		//
		//  Singular.
		//
		  else if ( nex == 3 ) {

		    r[1] = x[1] + 10.0 * x[2];
		    r[2] = Math.sqrt ( 5.0 ) * ( x[3] - x[4] );
		    temp = ( x[2] - 2.0 * x[3] );
		    r[3] = temp * temp;
		    temp = x[1] - x[4];
		    r[4] = Math.sqrt ( 10.0 ) * temp * temp;
		  }
		//
		//  Woods.
		//
		  else if ( nex == 4 ) {

		    r[1] = 10.0 * ( x[2] - x[1]*x[1] );
		    r[2] = 1.0 - x[1];
		    r[3] = Math.sqrt ( 90.0 ) * ( x[4] - x[3]*x[3] );
		    r[4] = 1.0 - x[3];
		    r[5] = Math.sqrt ( 9.9 ) * ( x[2] + x[4] - 2.0 );
		    t = Math.sqrt ( 0.2 );
		    r[6] = t * ( x[2] - 1.0 );
		    r[7] = t * ( x[4] - 1.0 );
		  }
		//
		//  Zangwill.
		//
		  else if ( nex == 5 ) {

		    r[1] =  x[1] - x[2] + x[3];
		    r[2] = -x[1] + x[2] + x[3];
		    r[3] =  x[1] + x[2] - x[3];
		  }
		//
		//  Engvall.
		//
		  else if ( nex == 6 ) {

		    r[1] = x[1]*x[1] + x[2]*x[2] + x[3]*x[3] - 1.0;
		    r[2] = x[1]*x[1] + x[2]*x[2] + ( x[3] - 2.0 )*(x[3] - 2.0) - 1.0;
		    r[3] = x[1] + x[2] + x[3] - 1.0;
		    r[4] = x[1] + x[2] - x[3] + 1.0;
		    r[5] = x[1]*x[1]*x[1] + 3.0 * x[2]*x[2]
		      + ( 5.0 * x[3] - x[1] + 1.0 )*(5.0 * x[3] - x[1] + 1.0) - 36.0;
		  }
		//
		//  Branin.
		//
		  else if ( nex == 7 ) {

		    r[1] = 4.0 * ( x[1] + x[2] );
		    r[2] = r[1] + ( x[1] - x[2] ) * ( ( x[1] - 2.0 )*(x[1] - 2.0) + 
		           x[2]*x[2] - 1.0 );
		  }
		//
		//  Beale.
		//
		  else if ( nex == 8 ) {

		    r[1] = 1.5   - x[1] * ( 1.0 - x[2]    );
		    r[2] = 2.25  - x[1] * ( 1.0 - x[2]*x[2] );
		    r[3] = 2.625 - x[1] * ( 1.0 - x[2]*x[2]*x[2] );
		  }
		//
		//  Cragg and Levy.
		//
		  else if ( nex == 9 ) {

		    temp = ( Math.exp ( x[1] ) - x[2] );
		    r[1] = temp * temp;
		    temp = x[2] - x[3];
		    r[2] = 10.0 * temp * temp * temp;
		    temp = Math.sin(x[3] - x[4])/Math.cos(x[3] - x[4]);
		    r[3] = temp * temp;
		    r[4] = x[1]*x[1]*x[1]*x[1];
		    r[5] = x[4] - 1.0;
		  }
		//
		//  Box.
		//
		  else if ( nex == 10 ) {

		    if ( expmax_calcrTest == 0.0 ) {
		      expmax_calcrTest = 1.999 * Math.log ( huge );
		    }

		    if ( expmin_calcrTest == 0.0 ) {
		      expmin_calcrTest = 1.999 * Math.log ( tiny );
		    }

		    if ( Math.min ( x[1], Math.min(x[2], x[3]) ) <= -expmax_calcrTest ) {
		      nfcall = -1;
		      return;
		    }

		    for (i = 1; i <= 10; i++) {

		      ti = -0.1 * i; 

		      t1 = ti * x[1];

		      if ( t1 <= expmin_calcrTest ) {
		        e1 = 0.0;
		      }
		      else {
		        e1 = Math.exp ( t1 );
		      }

		      t2 = ti * x[2];

		      if ( t2 <= expmin_calcrTest ) {
		        e2 = 0.0;
		      }
		      else {
		        e2 = Math.exp ( t2 );
		      }

		      r[i] = ( e1 - e2 ) - x[3] * ( Math.exp ( ti ) - Math.exp ( 10.0 * ti ) );

		    }
		  }
		//
		//  Davidon 1.
		//
		  else if ( nex == 11 ) {

		    for (i = 1; i <= n-1; i++) {
		      r1 = 0.0;
		      ti = (double)i;
		      t = 1.0;
		      for (j = 1; j <= p; j++) {
		        r1 = r1 + t * x[j];
		        t = t * ti;
		      }
		      r[i] = r1;
		    } // for (i = 1; i <= n-1; i++)
		    r[n] = x[1] - 1.0;
		  }
		//
		//  Freudenstein and Roth.
		//
		  else if ( nex == 12 ) {

		    r[1] = -13.0 + x[1] -  2.0 * x[2] + 5.0 * x[2]*x[2] - x[2]*x[2]*x[2];
		    r[2] = -29.0 + x[1] - 14.0 * x[2] + x[2]*x[2] + x[2]*x[2]*x[2];
		  }
		//
		//  Watson6.
		//  Watson9.
		//  Watson12.
		//  Watson20.
		//
		  else if ( nex >= 13 && nex <= 16) {

		    for (i = 1; i <= 29; i++) {
		      ti = ((double)i) / 29.0;
		      r1 = 0.0;
		      r2 = x[1];
		      t = 1.0;
		      for (j = 2; j <= p; j++) {
		        r1 = r1 + ( j - 1.0) * t * x[j];
		        t = t * ti;
		        r2 = r2 + t * x[j];
		      } // for (j = 2; j <= p; j++)
		      r[i] = r1 - r2 * r2 - 1.0;
		    } // for (i = 1; i <= 29; i++)
		    r[30] = x[1];
		    r[31] = x[2] - x[1]*x[1] - 1.0;
		  }
		//
		//  Chebyquad.
		//
		  else if ( nex == 17 ) {
            for (i = 1; i <= n; i++) {
		        r[i] = 0.0;
            }

		    for (j = 1; j <= n; j++) {
		      tim1 = 1.0;
		      ti = 2.0 * x[j] - 1.0;
		      z = ti + ti;
		      for (i = 1; i <= n; i++) {
		        r[i] = r[i] + ti;
		        tip1 = z * ti - tim1;
		        tim1 = ti;
		        ti = tip1;
		      } // for (i = 1; i <= n; i++)
		    } // for (j = 1; j <= n; j++)

		    for (i = 1; i <= n; i++) {
		      ti = 0.0;
		      if ( ( i % 2 ) == 0 ) {
		        ti = -1.0 /  ( i * i - 1.0 );
		      }
		      r[i] = ti - r[i] / ((double)n);
		    } // for (i = 1; i <= n; i++)
		  }
		//
		//  Brown and Dennis.
		//
		  else if ( nex == 18 ) {

		    for (i = 1; i <= n; i++) {
		      ti = 0.2 * i;
		      temp = (x[1] + x[2] * ti - Math.exp(ti));
		      temp2 = (x[3] + x[4] * Math.sin(ti) - Math.cos(ti));
		      r[i] = temp * temp + temp2 * temp2;
		    }
		  }
		//
		//  Bard.
		//
		  else if ( nex == 19 ) {

		    for (i = 1; i <= 15; i++) {
		      u = (double)i;
		      v = 16.0 - u;
		      w = Math.min ( u, v );
		      r[i] = ybard_calcrTest[i] - ( x[1] + u / ( x[2] * v + x[3] * w ) );
		    } // for (i = 1; i <= 15; i++)
		  }
		//
		//  Jennrich and Sampson.
		//
		  else if ( nex == 20 ) {

		    for (i = 1; i <= 10; i++) {
		      ti = (double)i;
		      r[i] = 2.0 + 2.0 * ti - ( Math.exp ( ti * x[1] ) + Math.exp ( ti * x[2] ) );
		    }
		  }
		//
		//  Kowalik and Osborne.
		//
		  else if ( nex == 21 ) {

		     for (i = 1; i <= 11; i++) {
		       r[i] = ykow_calcrTest[i] 
		       - x[1] * ( ukow_calcrTest[i]*ukow_calcrTest[i] + x[2] * ukow_calcrTest[i] )
		         / ( ukow_calcrTest[i]*ukow_calcrTest[i] + x[3] * ukow_calcrTest[i] + x[4] );
		     } // for (i = 1; i <= 11; i++)
		  }
		//
		//  Osborne 1.
		//
		  else if ( nex == 22 ) {

		    for (i = 1; i <= 33; i++) {
		      ti = 10.0 * ( 1.0 - i);
		      r[i] = yosb1_calcrTest[i] - ( x[1] + x[2] * Math.exp ( x[4] * ti ) +
		        x[3] * Math.exp ( x[5] * ti ) );
		    }
		  }
		//
		//  Osborne 2.
		//
		//  UFTOLG is a machine-dependent constant.  It is just slightly
		//  larger than the log of the smallest positive machine number.
		//
		  else if ( nex == 23 ) {

		    if ( uftolg_calcrTest == 0.0 ) {
		      uftolg_calcrTest = 1.999 * Math.log ( tiny );
		    }

		    for (i = 1; i <= 65; i++) {
		      ti = 0.1 * ( 1.0 - i);
		      ri = x[1] * Math.exp ( x[5] * ti );
		      for (j = 2; j <= 4; j++) {
		        theta = -x[j+4] * ( ti + x[j+7] )*(ti + x[j+7]);
		        if ( theta <= uftolg_calcrTest ) {
		          t = 0.0;
		        }
		        else {
		          t = Math.exp ( theta );
		        }
		        ri = ri + x[j] * t;
		      } // for (j = 2; j <= 4; j++)
		      r[i] = yosb2_calcrTest[i] - ri;
		    } // for (i = 1; i <= 65; i++)
		  }
		//
		//  Madsen.
		//
		  else if ( nex == 24 ) {

		    r[1] = x[1]*x[1] + x[2]*x[2] + x[1] * x[2];
		    r[2] = Math.sin ( x[1] );
		    r[3] = Math.cos ( x[2] );
		  }
		//
		//  Meyer.
		//
		  else if ( nex == 25 ) {

		    for (i = 1; i <= 16; i++) {
		      ti =  5.0 * i + 45.0;
		      r[i] = x[1] * Math.exp ( x[2] / ( ti + x[3] ) ) - ymeyer_calcrTest[i];
		    } // for (i = 1; i <= 16; i++)
		  }
		//
		//  Brown5.
		//  Brown10.
		//  Brown30.
		//  Brown40.
		//
		  else if ( nex >= 26 && nex <= 29) {
            sum = x[1];
            prod = x[1];
            for (i = 2; i <= n; i++) {
            	sum = sum + x[i];
            	prod = prod * x[i];
            }
            for (i = 1; i <= n-1; i++) {
		        r[i] = x[i] + sum - ( n + 1.0);
            }
		    r[n] = prod - 1.0;
		  }
		//
		//  Bard + 10.
		//
		  else if ( nex == 30 ) {

		    for (i = 1; i <= 15; i++) {
		      u = (double)i;
		      v = 16.0 - u;
		      w = Math.min ( u, v );
		      r[i] = ybard_calcrTest[i] - ( x[1] + u / ( x[2] * v + x[3] * w ) ) + 10.0;
		    } // for (i = 1; i <= 15; i++)
		  }
		//
		//  Kowalik and Osborne + 10.
		//
		  else if ( nex == 31 ) {

		    for (i = 1; i <= 11; i++) {
		      r[i] = ykow_calcrTest[i] 
		      - x[1] * ( ukow_calcrTest[i]*ukow_calcrTest[i] + x[2] * ukow_calcrTest[i] )
		        / ( ukow_calcrTest[i]*ukow_calcrTest[i] + x[3] * ukow_calcrTest[i] + x[4] ) + 10.0;
		    } // for (i = 1; i <= 11; i++)
		  }
		//
		//  Meyer + 10.
		//
		  else if ( nex == 32 ) {

		    for (i = 1; i <= 16; i++) {
		      ti = 5.0 * i + 45.0;
		      r[i] = x[1] * Math.exp ( x[2] / ( ti + x[3] ) ) - ymeyer_calcrTest[i] + 10.0;
		    } // for (i = 1; i <= 16; i++)
		  }
		//
		//  Watson6 + 10.
		//  Watson9 + 10.
		//  Watson12 + 10.
		//  Watson20 + 10.
		//
		  else if ( nex >= 33 && nex <= 36) {

		    for (i = 1; i <= 29; i++) {
		      ti = ((double)i) / 29.0;
		      r1 = 0.0;
		      r2 = x[1];
		      t = 1.0;
		      for (j = 2; j <= p; j++) {
		        r1 = r1 + ( j - 1.0) * t * x[j];
		        t = t * ti;
		        r2 = r2 + t*x[j];
		      } // for (j = 2; j <= p; j++)
		      r[i] = r1 - r2 * r2 - 1.0 + 10.0;
		    } // for (i = 1; i <= 29; i++)
		    r[30] = x[1] + 10.0;
		    r[31] = x[2] - x[1]*x[1] - 1.0 + 10.0;
		  }

		  else {

		    System.out.println("calcrTest - Fatal error");
		    Preferences.debug("calcrTest - Fatal error\n", Preferences.DEBUG_ALGORITHM);
		    System.out.println("Illegal problem index nex = " + nex);
		    Preferences.debug("Illegal problem index nex = " + nex + "\n", Preferences.DEBUG_ALGORITHM);
		    System.exit(-1);

		  }

		  return;	
	} // private void calcrTest
	
	private void covclc ( int covirc[], double d[], int iv[], double j[][], int n, int nn, int p, 
			              double r[], double v[], double x[] ) {

	/***********************************************************************
	!
	!! COVCLC computes the covariance matrix for NL2ITR.
	!
	!  Discussion:
	!
	!    Let K = abs ( IV(COVREQ) ).  
	!
	!    For K <= 2, a finite-difference hessian H is computed,
	!    * using function and gradient values if IV(COVREQ) is nonnegative,
	!    * using only function values if IV(COVREQ) is negative).  
	!
	!    Let
	!      SCALE = 2 * F(X) / max ( 1, N - P ),
	!    where 2 * F(X) is the residual sum of squares.
	!
	!    COVCLC computes:
	!      K = 0 or 1:  SCALE * inverse ( H ) * ( J' * J ) * inverse ( H ).
	!      K = 2:       SCALE * inverse ( H );
	!      K >= 3:      SCALE * inverse ( J' * J ).
	!
	!  Modified:
	!
	!    13 April 2006
	!
	!  Parameters:
	!
	!    ?, integer COVIRC, ?
	!
	!    Input, real D(P), the scaling vector.
	!
	!    Input/output, integer IV(*), the NL2SOL integer parameter vector.
	!
	!    Input, real J(NN,P), the N by P Jacobian matrix.
	!
	!    Input, integer N, the number of functions.
	!
	!    Input, integer NN, the leading dimension of J.
	!
	!    Input, integer P, the number of variables.
	!
	!    ?, real R(N), ?
	!
	!    Input, real V(*), the NL2SOL real parameter array.
	!
	!    ?, real X(P), ?
	*/
	  
	  int ii;
      int cov = 0;
	  final int covmat = 26;
	  final int covreq = 15;
	  double del;
	  final int delta = 50;
	  final int delta0 = 44;
	  final int dltfdc = 40;
	  final int f = 10;
	  final int fx = 46;
	  final int g = 28;
	  int g1;
	  int gp;
	  int gsave1;
	  final int h = 44;
	  boolean havej;
	  int hc;
	  int hmi;
	  int hpi;
	  int hpm;
	  int i;
	  final int ierr = 32;
	  final int ipiv0 = 60;
	  int ipivi;
	  int ipivk;
	  final int ipivot = 61;
	  int irc[] = new int[1];
	  int k;
	  final int kagqt = 35;
	  final int kalm = 36;
	  int kind;
	  int kl;
	  int l;
	  final int lmat = 58;
	  int m;
	  int mm1;
	  int mm1o2;
	  final int mode = 38;
	  final int nfgcal = 7;
	  int pp1o2;
	  final int qtr = 49;
	  int qtr1;
	  final int rd = 51;
	  int rd1;
	  final int rsave = 52;
	  final int savei = 54;
	  int stp0;
	  int stpi;
	  int stpm;
	  final int switchConstant = 12;
	  double t;
	  final int toobig = 2;
	  final int w = 59;
	  int w0;
	  int w1;
	  double wk;
	  int wl;
	  final int xmsave = 49;
	  double alpha[];
	  int ipivotArr[];
	  int ierrArr[];
	  double sum[];
	  double arr[];
	  double arr2[];
	  double arr3[];
	  boolean do350 = true;

	  covirc[0] = 4;
	  kind = iv[covreq];
	  m = iv[mode];

	  loop1: while (true) {
	  if ( m <= 0 ) { // #1

	    iv[kagqt] = -1;

	    if ( 0 < iv[kalm] ) {
	      iv[kalm] = 0;
	    }

	    if ( 3 <= Math.abs ( kind ) ) {

	      rd1 = iv[rd];

	      if ( iv[kalm] == -1 ) {
	        qtr1 = iv[qtr];
	        for (ii = 1; ii <= n; ii++) {
	        	v[qtr1 + ii - 1] = r[ii];
	        }
	        w1 = iv[w] + p;
	        alpha = new double[p+1];
	        ipivotArr = new int[p+1];
	        ierrArr = new int[1];
	        sum = new double[p+1];
	        qrfact ( nn, n, p, j, alpha, ipivotArr, ierrArr, 0, sum );
	        for (ii = 1; ii <= p; ii++) {
	        	v[rd1 + ii - 1] = alpha[ii];
	        	iv[ipivot + ii - 1] = ipivotArr[ii];
	        }
	        iv[ierr] = ierrArr[0];
	        iv[kalm] = -2;
	      } // if (iv[kalm] == -1)

	      iv[covmat] = -1;

	      if (iv[ierr] != 0) {
	        return;
	      }

	      cov = iv[lmat];
	      hc = Math.abs ( iv[h] );
	      iv[h] = -hc;
	//
	//  Set HC = R matrix from QRFACT.
	//
	      l = hc;
	      for ( i = 1; i <= p; i++) {
	        if ( 1 < i ) {
	          for (ii = 1; ii <= i-1; ii++) {
	        	  v[l+ii-1] = j[ii][i];
	          }
	        }
	        l = l + i - 1;
	        v[l] = v[rd1];
	        l = l + 1;
	        rd1 = rd1 + 1;
	      } // for (i = 1; i <= p; i++)

	      break loop1;

	    } // if ( 3 <= Math.abs ( kind ) )

	    v[fx] = v[f];
	    k = iv[rsave];
	    for (ii = 1; ii <= n; ii++) {
	        v[k+ii-1] = r[ii];
	    }

	  } // if (m <= 0) #1

	  if ( m <= p ) { // #1
     loop2: while (true) {		  
     loop3: while (true) {
	    if ( kind < 0 ) {
	    	break loop3;
	    }
	//
	//  Compute finite-difference hessian using both function and
	//  gradient values.
	//
	    gsave1 = iv[w] + p;
	    g1 = iv[g];
	//
	//  First call on COVCLC.  Set GSAVE = G, take first step.
	//
	    if ( m <= 0 ) { // #2
          for (ii = 0; ii <= p-1; ii++) {
	          v[gsave1+ii] = v[g1+ii];
          }
	      iv[switchConstant] = iv[nfgcal];
	    }
	    else {

	      del = v[delta];
	      x[m] = v[xmsave];
	//
	//  Handle oversize V(DELTA).
	//
	      if ( iv[toobig] != 0 ) {

	        if ( 0.0 < del * x[m] ) {
	          del = -0.5 * del;
	          x[m] = x[m] + del;
	          v[delta] = del;
	          covirc[0] = 2;
	          return;
	        } // if (0.0 < del * x[m])

	        iv[covmat] = -2;
	        break loop2;

	      } // if (iv[toobig] != 0)

	      cov = iv[lmat];
	      gp = g1 + p - 1;
	//
	//  Set G = ( G - GSAVE ) / DEL.
	//
	      for (i = g1; i <= gp; i++) {
	        v[i] = (v[i] - v[gsave1]) / del;
	        gsave1 = gsave1 + 1;
	      }
	//
	//  Add G as new column to finite-difference hessian matrix.
	//
	      k = cov + ( m * ( m - 1 ) ) / 2;
	      l = k + m - 2;
	//
	//  Set H(1:M-1,M) = 0.5 * (H(1:M-1,m) + G(1:M-1)).
	//
	      if ( m != 1 ) {

	        for ( i = k; i <= l; i++) {
	          v[i] = 0.5 * ( v[i] + v[g1] );
	          g1 = g1 + 1;
	        } 

	      } // if (m != 1)
	//
	//  Add H(M:P,M) = G(M:P).
	//
	      l = l + 1;
	      for (i = m; i <= p; i++) {
	        v[l] = v[g1];
	        l = l + i;
	        g1 = g1 + 1;
	      }

	    } // if (m <= 0) #2

	    m = m + 1;
	    iv[mode] = m;

	    if ( p < m ) {
	      break loop2;
	    }
	//
	//  Choose next finite-difference step, return to get G there.
	//
	    del = v[delta0] * Math.max ( 1.0 / d[m], Math.abs ( x[m] ) );
	    if ( x[m] < 0.0 ) {
	      del = -del;
	    }
	    v[xmsave] = x[m];
	    x[m] = x[m] + del;
	    v[delta] = del;
	    covirc[0] = 2;
	    return;
	//
	//  Compute finite-difference hessian using function values only.
	//
      } // loop3: while(true)

	    stp0 = iv[w] + p - 1;
	    mm1 = m - 1;
	    mm1o2 = m * mm1 / 2;
	//
	//  First call on COVCLC.
	//
	    if ( m <= 0 ) { // #3

	      iv[savei] = 0;
	    }
	    else {

	      i = iv[savei];
	 
	      if ( i <= 0 ) {
	//
	//  Handle oversize step.
	//
	        if ( iv[toobig] != 0 ) {

	          stpm = stp0 + m;
	          del = v[stpm];
	//
	//  We already tried shrinking the step, so quit.
	//
	          if ( del * v[xmsave] <= 0.0 ) {
	            iv[covmat] = -2;
	            return;
	          }
	//
	//  Try shrinking the step.
	//
	          del = -0.5 * del;
	          x[m] = v[xmsave] + del;
	          v[stpm] = del;
	          covirc[0] = 1;
	          return;

	        } // if (iv[toobig] != 0)
	//
	//  Save F(X + STP(M)*E(M)) in H(P,M).
	//
	        pp1o2 = ( p * ( p - 1 ) ) / 2;
	        cov = iv[lmat];
	        hpm = cov + pp1o2 + mm1;
	        v[hpm] = v[f];
	//
	//  Start computing row M of the finite-difference hessian H.
	//
	        hmi = cov + mm1o2;
	        hpi = cov + pp1o2;

	        for ( i = 1; i <= mm1; i++) {
	          v[hmi] = v[fx] - (v[f] + v[hpi]);
	          hmi = hmi + 1;
	          hpi = hpi + 1;
	        }

	        v[hmi] = v[f] - 2.0 * v[fx];
	//
	//  Compute function values needed to complete row M of H.
	//
	        i = 1;

	        iv[savei] = i;
	        stpi = stp0 + i;
	        v[delta] = x[i];
	        x[i] = x[i] + v[stpi];
	        if ( i == m ) {
	          x[i] = v[xmsave] - v[stpi];
	        }
	        covirc[0] = 1;
	        return;

	      } // if (i <= 0)

	      x[i] = v[delta];
	//
	//  Punt in the event of an oversize step.
	//
	      if ( iv[toobig] != 0 ) {
	        iv[covmat] = -2;
	        return;
	      }
	//
	//  Finish computing H(M,I).
	//
	      stpi = stp0 + i;
	      hmi = cov + mm1o2 + i - 1;
	      stpm = stp0 + m;
	      v[hmi] = ( v[hmi] + v[f] ) / ( v[stpi] * v[stpm] );
	      i = i + 1;

	      if ( i <= m ) {
	        iv[savei] = i;
	        stpi = stp0 + i;
	        v[delta] = x[i];
	        x[i] = x[i] + v[stpi];
	        if ( i == m ) {
	          x[i] = v[xmsave] - v[stpi];
	        }
	        covirc[0] = 1;
	        return;
	      } // if (i <= m)

	      iv[savei] = 0;
	      x[m] = v[xmsave];

	    } // if (m <= 0) #3

	    m = m + 1;
	    iv[mode] = m;
	//
	//  Prepare to compute row M of the finite-difference hessian H.
	//  Compute the M-th step size STP(M), then return to obtain
	//  F(X + STP(M)*E(M)), where E(M) = M-th standard unit vector.
	//
	    if ( m <= p ) {
	      del = v[dltfdc] * Math.max ( 1.0 / d[m], Math.abs(x[m]) );
	      if (x[m] < 0.0 ) {
	        del = -del;
	      }
	      v[xmsave] = x[m];
	      x[m] = x[m] + del;
	      stpm = stp0 + m;
	      v[stpm] = del;
	      covirc[0] = 1;
	      return;
	    } // if (m <= p)
     } // loop2: while(true)
	//
	//  Restore R, V(F), etc.
	//

	    k = iv[rsave];
	    for (ii = 1; ii <= n; ii++) {
	        r[ii] = v[k+ii-1];
	    }
	    v[f] = v[fx];

	    if ( 0 <= kind ) {

	      iv[nfgcal] = iv[switchConstant];
	      qtr1 = iv[qtr];
	      for (ii = 1; ii <= n; ii++) {
	          v[qtr1+ii-1] = r[ii];
	      }
	 
	      if ( 0 <= iv[covmat] ) {
	        covirc[0] = 3;
	      }
	 
	      return;
	    } // if (0 <= kind)

	  } // if (m <= p) #1

	  cov = iv[lmat];
	//
	//  The complete finite-difference hessian is now stored at V(COV).
	//  Use it to compute the requested covariance matrix.
	//
	//  Compute Cholesky factor C of H = C * C' and store it at V(HC).
	//
	  hc = cov;

	  if ( Math.abs ( kind ) != 2 ) {
	    hc = Math.abs ( iv[h] );
	    iv[h] = -hc;
	  }

	  arr = new double[p*(p+1)/2 + 1];
	  arr2 = new double[p*(p+1)/2 + 1];
	  for (ii = 1; ii <= p*(p+1)/2; ii++) {
		  arr2[ii] = v[cov + ii - 1];
	  }
	  lsqrt ( 1, p, arr, arr2, irc );
	  for (ii = 1; ii <= p*(p+1)/2; ii++) {
		  v[hc + ii - 1] = arr[ii];
	  }
	  iv[covmat] = -1;

	  if ( irc[0] != 0 ) {
	    return;
	  }

	  w1 = iv[w] + p;

	  if ( 1 < Math.abs ( kind ) ) {
	    break loop1;
	  }
	//
	//  Covariance = SCALE * inverse ( H ) * (J' * J) * inverse ( H ).
	//
	  for (ii = 0; ii <= p*(p+1)/2; ii++) {
		  v[cov+ii] = 0.0;
	  }
	  havej = iv[kalm] == (-1);
	//
	//  HAVEJ = .true. means J is in its original form, while
	// HAVEJ = .false. means QRFACT has been applied to J.
	//
	  if ( havej ) {
	    m = n;
	  }
	  else {
	    m = p;
	  }
	  w0 = w1 - 1;
	  rd1 = iv[rd];

	  for ( i = 1; i <= m; i++) {
	//
	//  Set W = IPIVOT * (row I of R matrix from QRFACT).
	//
	    if ( ! havej ) {

	      for (ii = 0; ii <= p-1; ii++) {
	          v[w1+ii] = 0.0;
	      }
	      ipivi = ipiv0 + i;
	      l = w0 + iv[ipivi];
	      v[l] = v[rd1];
	      rd1 = rd1 + 1;

	      for (k = i+1; k <= p; k++) {
	        ipivk = ipiv0 + k;
	        l = w0 + iv[ipivk];
	        v[l] = j[i][k];
	      } // for (k = i+1; k <= p; k++)
	   } // if (!havej)
	//
	//  Set W = (row I of J).
    //
	    else {

	      l = w0;
	      for (k = 1; k <= p; k++) {
	        l = l + 1;
	        v[l] = j[i][k];
	      } // for (k = 1; k <= p; k++)

	    } // else
	//
	//  Set W = inverse ( H ) * W.
	//
	    arr = new double[p+1];
	    arr2 = new double[p*(p+1)/2 + 1];
	    arr3 = new double[p+1];
	    for (ii = 1; ii <= p*(p+1)/2; ii++) {
	    	arr2[ii] = v[hc+ii-1];
	    }
	    for (ii = 1; ii <= p; ii++) {
	    	arr3[ii] = v[w1+ii-1];
	    }
	    livmul ( p, arr, arr2, arr3 );
	    for (ii = 1; ii <= p; ii++) {
	    	v[w1+ii-1] = arr[ii];
	    }
	    for (ii = 1; ii <= p*(p+1)/2; ii++) {
	    	arr2[ii] = v[hc+ii-1];
	    }
	    for (ii = 1; ii <= p; ii++) {
	    	arr3[ii] = v[w1+ii-1];
	    }
	    litvmu ( p, arr, arr2, arr3 );
	    for (ii = 1; ii <= p; ii++) {
	    	v[w1+ii-1] = arr[ii];
	    }
	//
	//  Add W * W' to covariance matrix.
	//
	    kl = cov;
	    for (k = 1; k <= p; k++) {
	      l = w0 + k;
	      wk = v[l];
	      for (l = 1; l <= k; l++) {
	        wl = w0 + l;
	        v[kl] = v[kl]  +  wk * v[wl];
	        kl = kl + 1;
	      } // for (l = 1; l <= k; l++)
	    } // for (k = 1; k <= p; k++)

	  } // for (i = 1; i <= m; i++)

	  
	  do350 = false;
	  break loop1;
	  } // loop1: while(true)
	//
	//  The Cholesky factor C of the unscaled inverse covariance matrix
	//  (or permutation thereof) is stored at V(HC).
	//
	//  Set C = inverse ( C ).
	//
	if (do350) {
      arr = new double[p*(p+1)/2 + 1];
      arr2 = new double[p*(p+1)/2 + 1];
      for (ii = 1; ii <= p*(p+1)/2; ii++) {
    	  arr2[ii] = v[hc+ii-1];
      }
	  linvrt ( p, arr, arr2 );
	  for (ii = 1; ii <= p*(p+1)/2; ii++) {
		  v[hc+ii-1] = arr[ii];
	  }
	//
	//  Set C = C' * C.
	//
	  for (ii = 1; ii <= p*(p+1)/2; ii++) {
    	  arr2[ii] = v[hc+ii-1];
      }
	  ltsqar ( p, arr, arr2 );
	  for (ii = 1; ii <= p*(p+1)/2; ii++) {
		  v[hc+ii-1] = arr[ii];
	  }
	//
    //  C = permuted, unscaled covariance.
	//  Set COV = IPIVOT * C * IPIVOT'.
	//
	  if ( hc != cov ) {

	    for (i = 1; i <= p; i++) {
	      m = ipiv0 + i;
	      ipivi = iv[m];
	      kl = cov-1 + ( ipivi * (ipivi-1) ) / 2;
	      for (k = 1; k <= i; k++) {
	        m = ipiv0 + k;
	        ipivk = iv[m];
	        l = kl + ipivk;
	        if ( ipivi < ipivk ) {
	          l = l + ( (ipivk-ipivi) * (ipivk+ipivi-3) ) / 2;
	        }
	        v[l] = v[hc];
	        hc = hc + 1;
	      } // for (k = 1; k <= i; k++)
	    } // for (i = 1; i <= p; i++)

	  } // if (hc != cov)
	} // if (do350)

	  iv[covmat] = cov;
	//
	//  Apply scale factor = (residual sum of squares) / max(1,n-p).
	//
	  t = v[f] / ( 0.5 * (double) ( Math.max ( 1, n-p ) ) );
	  k = cov - 1 + ( p * ( p + 1 ) ) / 2;

	  for (ii = cov; ii <= k; ii++) {
	      v[ii] = t * v[ii];
	  }

	  return;
	} // private void covclc
	
	private void dfault ( int iv[], double v[] ) {

	/***********************************************************************
	!
	!! DFAULT supplies default values to IV and V.
	!
	!  Discussion:
	!
	!    Only entries in the first 25 positions of IV and the first 45
	!    positions of V are reset.
	! 
	!  Modified:
	!
	!    05 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Output, integer IV(25), contains default values for specific entries.
	!
	!    Output, real V(45), contains default values for specific values.
	*/

	  int afctol = 31;
	  int cosmin = 43;
	  int covprt = 14;
	  int covreq = 15;
	  int d0init = 37;
	  int decfac = 22;
	  int delta0 = 44;
	  int dfac = 41;
	  int dinit = 38;
	  int dltfdc = 40;
	  int dltfdj = 36;
	  int dtype = 16;
	  int inits = 25;
	  int epslon = 19;
	  int fuzz = 45;
	  int incfac = 23;
	  int jtinit = 39;
	  int lmax0 = 35;
	  double machep;
	  double mepcrt;
	  int mxfcal = 17;
	  int mxiter = 18;
	  int outlev = 19;
	  int parprt = 20;
	  int phmnfc = 20;
	  int phmxfc = 21;
	  int prunit = 21;
	  int rdfcmn = 24;
	  int rdfcmx = 25;
	  int rfctol = 32;
	  int rlimit = 42;
	  int solprt = 22;
	  double sqteps;
	  int statpr = 23;
	  int tuner1 = 26;
	  int tuner2 = 27;
	  int tuner3 = 28;
	  int tuner4 = 29;
	  int tuner5 = 30;
	  int x0prt = 24;
	  int xctol = 33;
	  int xftol = 34;

	  iv[1] = 12;
	  iv[covprt] = 1;
	  iv[covreq] = 1;
	  iv[dtype] = 1;
	  iv[inits] = 0;
	  iv[mxfcal] = 200;
	  iv[mxiter] = 150;
	  iv[outlev] = -1;
	  iv[parprt] = 1;
	  iv[prunit] = 6;
	  iv[solprt] = 1;
	  iv[statpr] = 1;
	  iv[x0prt] = 1;

	  machep = epsilon;
	  v[afctol] = 1.0e-20;
	  if ( 1.0e-10 < machep ) { 
	    v[afctol] = machep*machep;
	  }
	  v[cosmin] = Math.max ( 1.0e-06, 1.0e+02 * machep );
	  v[decfac] = 0.5;
	  sqteps = Math.sqrt (epsilon);
	  v[delta0] = sqteps;
	  v[dfac] = 0.6;
	  v[dinit] = 0.0;
	  mepcrt = Math.pow(machep ,( 1.0 / 3.0) );
	  v[dltfdc] = mepcrt;
	  v[dltfdj] = sqteps;
	  v[d0init] = 1.0;
	  v[epslon] = 0.1;
	  v[fuzz] = 1.5;
	  v[incfac] = 2.0;
	  v[jtinit] = 1.0e-6;
	  v[lmax0] = 100.0;
	  v[phmnfc] = -0.1;
	  v[phmxfc] = 0.1;
	  v[rdfcmn] = 0.1;
	  v[rdfcmx] = 4.0;
	  v[rfctol] = Math.max ( 1.0E-10, mepcrt*mepcrt );
	  v[rlimit] = Math.sqrt ( 0.999 * huge);
	  v[tuner1] = 0.1;
	  v[tuner2] = 1.0e-4;
	  v[tuner3] = 0.75;
	  v[tuner4] = 0.5;
	  v[tuner5] = 0.75;
	  v[xctol] = sqteps;
	  v[xftol] = 1.0e+2 * machep;

	  return;
	} // private void dfault
	
	private double dotprd ( int p, double x[], double y[] ) {
	
	/***********************************************************************
	!
	!! DOTPRD returns the inner product of two vectors.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer P, the number of entries in the vectors.
	!
	!    Input, real X(P), Y(P), the vectors.
	!
	!    Output, real DOTPRD, the dot product of X and Y.
	*/

	  double result;
	  int i;
	  double t;

	  result = 0.0;

	  if ( p <= 0 ) {
	    return result;
	  }

	  if ( sqteta_dotprd == 0.0 ) {
	    sqteta_dotprd = Math.sqrt ( 1.001 * tiny);
	  }

	  for ( i = 1; i <= p; i++) {

	    t = Math.max ( Math.abs ( x[i] ), Math.abs ( y[i] ) );

	    if ( t < sqteta_dotprd ) {
	    	
	    }
	    else if ( 1.0 < t ) {

	      result = result + x[i] * y[i];

	    }
	    else {
	      t = ( x[i] / sqteta_dotprd ) * y[i];

	      if ( sqteta_dotprd <= Math.abs ( t ) ) {
	        result = result + x[i] * y[i];
	      }

	    } // else

	  } // for (i = 1; i <= p; i++)

	  return result;
	} // private double dotprd
	
	private void dupdat ( double d[], int iv[], double j[][], int n, int nn, int p, double v[] ) {

	/***********************************************************************
	!
	!! DUPDAT updates the scale vector for NL2ITR.
	!
	!  Modified:
	!
	!    05 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input/output, real D(P), the scale vector.
	!
	!    Input, integer IV(*), the NL2SOL integer array.
	!
	!    Input, real J(NN,P), the N by P Jacobian matrix.
	!
	!    Input, integer N, the number of functions.
	!
	!    Input, integer NN, the leading dimension of J.
	!
	!    Input, integer P, the number of variables.
	!
	!    Input, real V(*), the NL2SOL real array.
	*/
	  
	  int d0;
	  int dfac = 41; 
	  int dtype = 16;
	  int i;
	  int jtol0 = 86;
	  int jtoli;
	  int niter = 31;
	  int s = 53;
	  int s1;
	  double sii;
	  double t;
	  double vdfac;
	  double arr[] = new double[n+1];
	  int ii;

	  i = iv[dtype];

	  if ( i != 1 ) {

	    if ( 0 < iv[niter] ) {
	      return;
	    }

	  } // if (i != 1)

	  vdfac = v[dfac];
	  d0 = jtol0 + p;
	  s1 = iv[s] - 1;

	  for (i = 1; i <= p; i++) {

	    s1 = s1 + i;
	    sii = v[s1];
	    for (ii = 1; ii <= n; ii++) {
	    	arr[ii] = j[ii][i];
	    }
	    t = v2norm ( n, arr );

	    if ( 0.0 < sii ) {
	      t = Math.sqrt ( t * t + sii );
	    }

	    jtoli = jtol0 + i;
	    d0 = d0 + 1;

	    if ( t < v[jtoli] ) {
	      t = Math.max ( v[d0], v[jtoli] );
	    }

	    d[i] = Math.max ( vdfac * d[i], t );

	  } // for (i = 1; i <= p; i++)

	  return;
	} // private void dupdat
	
	private void gqtstp ( double d[], double dig[], double dihdi[], int ka[], double l[],
			              int p, double step[], double v[], double w[] ) {

	/***********************************************************************
	!
	!! GQTSTP computes the Goldfeld-Quandt-Trotter step by More-Hebden technique.
	!
	!  Discussion:
	!
	!    Given the compactly stored lower triangle of a scaled
	!    hessian approximation and a nonzero scaled gradient vector,
	!    this subroutine computes a Goldfeld-Quandt-Trotter step of
	!    approximate length V(RADIUS) by the More-Hebden technique.
	!
	!    In other words, STEP is computed to approximately minimize
	!      PSI(STEP) = G' * STEP + 0.5 * STEP' * H * STEP  
	!    such that the 2-norm of D * STEP is at most approximately V(RADIUS),
	!    where G is the gradient, H is the hessian, and D is a diagonal
	!    scale matrix whose diagonal is stored in the parameter D.
	!
	!    GQTSTP assumes:
	!
	!      DIG = inverse ( D ) * G,
	!      DIHDI = inverse ( D ) * H * inverse ( D ).
	!
	!    If G = 0, however, STEP = 0 is returned, even at a saddle point.
	!
	!    If it is desired to recompute STEP using a different value of
	!    V(RADIUS), then this routine may be restarted by calling it
	!    with all parameters unchanged except V(RADIUS).  This explains
	!    why STEP and W are listed as I/O.  On an initial call, with
	!    KA < 0, STEP and W need not be initialized and only components
	!    V(EPSLON), V(STPPAR), V(PHMNFC), V(PHMXFC), V(RADIUS), and
	!    V(RAD0) of V must be initialized.  To compute STEP from a saddle
	!    point, where the true gradient vanishes and H has a negative
	!    eigenvalue, a nonzero G with small components should be passed.
	!
	!    This routine is called as part of the NL2SOL package, but it could 
	!    be used in solving any unconstrained minimization problem.
	!
	!    The desired G-Q-T step (references 2, 3, 4) satisfies
	!    (H + ALPHA*D**2) * STEP = -G  for some nonnegative ALPHA such that
	!    H + ALPHA*D**2 is positive semidefinite.  ALPHA and STEP are
	!    computed by a scheme analogous to the one described in reference 5.
	!    Estimates of the smallest and largest eigenvalues of the hessian
	!    are obtained from the Gerschgorin circle theorem enhanced by a
	!    simple form of the scaling described in reference 6.  
	!
	!    Cases in which H + ALPHA*D**2 is nearly or exactly singular are 
	!    handled by the technique discussed in reference 2.  In these 
	!    cases, a step of exact length V(RADIUS) is returned for which 
	!    PSI(STEP) exceeds its optimal value by less than 
	!    -V(EPSLON)*PSI(STEP).
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Reference:
	!
	!    John Dennis, David Gay, Roy Welsch,
	!    An Adaptive Nonlinear Least Squares Algorithm,
	!    ACM Transactions on Mathematical Software,
	!    Volume 7, Number 3, 1981.
	!
	!    David Gay,
	!    Computing Optimal Locally Constrained Steps,
	!    SIAM Journal on Scientific and Statistical Computing, 
	!    Volume 2, Number 2, pages 186-197, 1981.
	!
	!    S M Goldfeld, R E Quandt, H F Trotter,
	!    Maximization by Quadratic Hill-climbing, 
	!    Econometrica,
	!    Volume 34, pages 541-551, 1966.
	!
	!    M D Hebden,
	!    An Algorithm for Minimization using Exact Second Derivatives, 
	!    Report TP515, 
	!    Theoretical Physics Division, 
	!    AERE, Harwell, Oxon., England, 1973.
	!
	!    Jorge More,
	!    The Levenberg-Marquardt Algorithm, Implementation and Theory, 
	!    in Springer Lecture Notes in Mathematics, Number 630, 
	!    edited by G A Watson,
	!    Springer Verlag, Berlin and New York, pages 105-116, 1978.
	!
	!    Richard Varga, 
	!    Minimal Gerschgorin Sets, 
	!    Pacific Journal of Mathematics, 
	!    Volume 15, pages 719-729, 1965.
	!
	!  Parameters:
	!
	!    Input, real D(P), the scale vector, that is, the diagonal of the scale
	!    matrix D mentioned above.
	!
	!    Input, real DIG(P), the scaled gradient vector, inverse ( D ) * G.  
	!    If G = 0, then STEP = 0 and V(STPPAR) = 0 are returned.
	!
	!    Input, real DIHDI((P*(P+1))/2), the lower triangle of the scaled 
	!    hessian approximation, that is, 
	!      inverse ( D ) * H * inverse ( D ),
	!    stored compactly by rows, in the order (1,1), (2,1), (2,2), (3,1), 
	!    (3,2), and so on.
	!
	!    Input/output, integer KA, the number of Hebden iterations taken so
	!    far to determine STEP.  KA < 0 on input means this is the first
	!    attempt to determine STEP for the present DIG and DIHDI.
	!    KA is initialized to 0 in this case.  Output with KA = 0  or 
	!    V(STPPAR) = 0 means STEP = -inverse(H)*G.
	!
	!     l (i/o) = workspace of length p*(p+1)/2 for cholesky factors.
	!
	!     p (in)  = number of parameters -- the hessian is a  p x p  matrix.
	!
	!  step (i/o) = the step computed.
	!
	!     v (i/o) contains various constants and variables described below.
	!
	!     w (i/o) = workspace of length 4*p + 6.
	!
	!  entries in v
	!
	! v(dgnorm) (i/o) = 2-norm of (d**-1)*g.
	! v(dstnrm) (output) = 2-norm of d * step.
	! v(dst0)   (i/o) = 2-norm of d*(h**-1)*g (for pos. def. h only), or
	!             overestimate of smallest eigenvalue of (d**-1)*h*(d**-1).
	! v(epslon) (in)  = max. relative error allowed for psi(step).  for the
	!             step returned, psi(step) will exceed its optimal value
	!             by less than -v(epslon)*psi(step).  suggested value = 0.1.
	! v(gtstep) (out) = inner product between g and step.
	! v(nreduc) (out) = psi(-(h**-1)*g) = psi(Newton step)  (for pos. def.
	!             h only -- v(nreduc) is set to zero otherwise).
	! v(phmnfc) (in)  = tol. (together with v(phmxfc)) for accepting step
	!             (More's sigma).  the error v(dstnrm) - v(radius) must lie
	!             between v(phmnfc)*v(radius) and v(phmxfc)*v(radius).
	! v(phmxfc) (in)  (see v(phmnfc).)
	!             suggested values -- v(phmnfc) = -0.25, v(phmxfc) = 0.5.
	! v(preduc) (out) = psi(step) = predicted obj. func. reduction for step.
	! v(radius) (in)  = radius of current (scaled) trust region.
	! v(rad0)   (i/o) = value of v(radius) from previous call.
	! v(STPPAR) (i/o) is normally the Marquardt parameter, i.e. the alpha
	!             described below under algorithm notes.  if h + alpha*d**2
	!             (see algorithm notes) is (nearly) singular, however,
	!             then v(STPPAR) = -alpha.
	*/
	  

	  double aki;
	  double akk;
	  double alphak = 0.0;
	  double delta = 0.0;
	  int dggdmx;
	  final int dgnorm = 1;
	  int diag;
	  int diag0;
	  double dst = 0.0;
	  final int dst0 = 3;
	  final int dstnrm = 2;
	  int dstsav;
	  int emax;
	  int emin;
	  final double epsfac = 50.0;
	  final int epslon = 19;
	  double epso6;
	  final int gtstep = 4;
	  int i;
	  int inc;
	  int irc[] = new int[1];
	  int j;
	  int k;
	  int k1;
	  int kalim;
	  final double kappa = 2.0;
	  double lk = 0.0;
	  int lk0;
	  final int nreduc = 6;
	  double oldphi;
	  double phi = 0.0;
	  double phimax;
	  double phimin;
	  int phipin;
	  final int phmnfc = 20;
	  final int phmxfc = 21;
	  final int preduc = 7;
	  double psifac;
	  int q;
	  int q0;
	  double rad;
	  final int rad0 = 9;
	  final int radius = 8;
	  boolean restrt;
	  double root;
	  double si;
	  double sk;
	  final int stppar = 5;
	  double sw;
	  double t;
	  double t1;
	  double twopsi = 0.0;
	  double uk = 0.0;
	  int uk0;
	  double wi;
	  int x;
	  int x0;
	  int ii;
	  double arr[];
	  double arr2[];
	  boolean do20 = true;
	  boolean do40 = false;
	  boolean do60 = false;
	  boolean do70 = false;
	  boolean do210 = false;
	  boolean do260 = false;
	  boolean do270 = false;
	  boolean do290 = false;
	//
	//  Store largest absolute entry in inverse(D)*H*inverse(D) at W(DGGDMX).
	//
	  dggdmx = p + 1;
	//
	//  Store Gerschgorin over- and underestimates of the largest
	//  and smallest eigenvalues of inverse(D)*H*inverse(D) at W(EMAX)
	//  and W(EMIN) respectively.
	//
	  emax = dggdmx + 1;
	  emin = emax + 1;
	//
	//  For use in recomputing step, the final values of LK, UK, DST,
	//  and the inverse derivative of More's PHI at 0, for positive definite
	//  H, are stored in W(LK0), W(UK0), W(DSTSAV), and W(PHIPIN)
	//  respectively.
	//
	  lk0 = emin + 1;
	  phipin = lk0 + 1;
	  uk0 = phipin + 1;
	  dstsav = uk0 + 1;
	//
	//  Store diagonal of inverse(D)*H*inverse(D) in W(DIAG:DIAG+P-1).
	//
	  diag0 = dstsav;
	  diag = diag0 + 1;
	//
	//  Store -D * STEP in W(Q:Q+P-1).
	//
	  q0 = diag0 + p;
	  q = q0 + 1;
	  rad = v[radius];
	//
	//  PHITOL = maximum error allowed in DST = V(DSTNRM) = 2-norm of
	//  D * STEP.
	//
	  phimax = v[phmxfc] * rad;
	  phimin = v[phmnfc] * rad;
	//
	//  EPSO6 and PSIFAC are used in checking for the special case
	//  of nearly singular H + ALPHA*D**2.  See reference 2.
	//
	  psifac = 2.0 * v[epslon] / ( 3.0 * ( 4.0 * ( v[phmnfc] + 1.0 ) * 
	    ( kappa + 1.0 )  +  kappa  +  2.0 ) * rad*rad );
	//
	//  OLDPHI is used to detect limits of numerical accuracy.  If
	//  we recompute step and it does not change, then we accept it.
	//
	  oldphi = 0.0;
	  epso6 = v[epslon] / 6.0;
	  irc[0] = 0;
	  restrt = false;
	  kalim = ka[0] + 50;
	//
	//  Start or restart, depending on KA.
	//
	  if ( 0 <= ka[0] ) {
		  do20 = false;
	  loop1: while (true) {
	//
	//  Restart with new radius.
	//
	//  Prepare to return Newton step.
	//
	    if ( 0.0 < v[dst0] && v[dst0] - rad <= phimax ) {

	      restrt = true;
	      ka[0] = ka[0] + 1;
	      k = 0;
	      for (i = 1; i <= p; i++) {
	        k = k + i;
	        j = diag0 + i;
	        dihdi[k] = w[j];
	      } // for (i = 1; i <= p; i++)
	      uk = -1.0;
	      do40 = true;
	      break loop1;

	    } // if ( 0.0 < v[dst0] && v[dst0] - rad <= phimax )

	    if ( ka[0] == 0 ) {
	      do60 = true;
	      break loop1;
	    }

	    dst = w[dstsav];
	    alphak = Math.abs ( v[stppar] );
	    phi = dst - rad;
	    t = v[dgnorm] / rad;
	//
	//  Smaller radius.
	//
	    if ( rad <= v[rad0] ) {

	      uk = t - w[emin];
	      lk = 0.0;
	      if ( 0.0 < alphak ) {
	        lk = w[lk0];
	      }
	      lk = Math.max ( lk, t - w[emax] );
	      if ( 0.0 < v[dst0] ) {
	        lk = Math.max ( lk, ( v[dst0] - rad ) * w[phipin] );
	      }
	    } // if (rad <= v[rad0])
	//
	//  Bigger radius.
	//
	    else {

	      uk = t - w[emin];
	      if ( 0.0 < alphak ) {
	        uk = Math.min ( uk, w[uk0] );
	      }
	      lk = Math.max ( 0.0, Math.max(-v[dst0], t - w[emax] ));
	      if ( 0.0 < v[dst0] ) {
	        lk = Math.max ( lk, (v[dst0]-rad)*w[phipin] );
	      }
	  
	    } // else for if (rad <= v[rad0])

	    do260 = true;
	    break loop1;
	  } // loop1: while(true)
	  } // if (0 <= ka[0])
	
	if (do20) {
	//
	//  Fresh start.
	//
	  k = 0;
	  uk = -1.0;
	  ka[0] = 0;
	  kalim = 50;
	//
	//  Store diagonal of DIHDI in W(DIAG0+1:DIAG0+P).
	//
	  j = 0;
	  for ( i = 1; i <= p; i++) {
	    j = j + i;
	    k1 = diag0 + i;
	    w[k1] = dihdi[j];
	  } // for (i = 1; i <= p; i++)
	//
	//  Determine W(DGGDMX), the largest element of DIHDI.
	//
	  t1 = 0.0;
	  j = p * (p + 1) / 2;
	  for (i = 1; i <= j; i++) {
	    t = Math.abs(dihdi[i]);
	    t1 = Math.max ( t1, t );
	  } // for (i = 1; i <= j; i++)
	  w[dggdmx] = t1;
	  do40 = true;
	} // if (do20)
	
	if (do40) {
	//
	//  Try ALPHA = 0.
	//

	  lsqrt ( 1, p, l, dihdi, irc );
	//
	//  Indefinite H.  Underestimate smallest eigenvalue, use this
	//  estimate to initialize lower bound LK on ALPHA.
    //
	  if ( irc[0] == 0 ) {
		  do60 = true;
	  }
	  else {
	      j = ( irc[0] * ( irc[0] + 1 ) ) / 2;
	      t = l[j];
	      l[j] = 1.0;
	      for (ii = 1; ii <= irc[0]-1; ii++) {
	          w[ii] = 0.0;
	      }
	      w[irc[0]] = 1.0;
	      litvmu(irc[0], w, l, w);
	      t1 = v2norm(irc[0], w);
	      lk = -t / t1 / t1;
	      v[dst0] = -lk;

	      if (restrt) {
	    	  do210 = true;
	      }
	      else {
	          v[nreduc] = 0.0;
	          do70 = true;
	      }
	  } // else
	} // if (do40)
	if (do60) {
	//
	//  Positive definite H.  Compute unmodified Newton step.
	//

	  lk = 0.0;
	  arr = new double[p+1];
	  livmul(p, arr, l, dig);
	  for (ii = 1; ii <= p; ii++) {
		  w[q+ii-1] = arr[ii];
	  }
	  for (ii = 1; ii <= p; ii++) {
		  arr[ii] = w[q+ii-1];
	  }
	  v[nreduc] = 0.5 * dotprd(p, arr, arr);
	  arr2 = new double[p+1];
	  litvmu(p, arr2, l, arr);
	  for (ii = 1; ii <= p; ii++) {
		  w[q + ii - 1] = arr2[ii];
	  }
	  dst = v2norm(p, arr2);
	  v[dst0] = dst;
	  phi = dst - rad;

	  if ( phi <= phimax ) {
	    alphak = 0.0;
	    do290 = true;
	  }
	  else if (restrt) {
		  do210 = true;
	  }
	  else {
		  do70 = true;
	  }
	} // if (do60)
	if (do70) {
	//
	//  Prepare to compute Gerschgorin estimates of largest and
	//  smallest eigenvalues.
	//

	  v[dgnorm] = v2norm ( p, dig );

	  if ( v[dgnorm] == 0.0 ) {
	    v[stppar] = 0.0;
	    v[preduc] = 0.0;
	    v[dstnrm] = 0.0;
	    v[gtstep] = 0.0;
	    for (ii = 1; ii <= p; ii++) {
	        step[ii] = 0.0;
	    }
	    return;
	  } // if (v[dgnorm] == 0.0)

	  k = 0;
	  for (i = 1; i <= p; i++) {
	    wi = 0.0;
	    for (j = 1; j <= i - 1; j++) {
	      k = k + 1;
	      t = Math.abs ( dihdi[k] );
	      wi = wi + t;
	      w[j] = w[j] + t;
	    } // for (j = 1; j <= i - 1; j++)
	    w[i] = wi;
	    k = k + 1;
	  } // for (i = 1; i <= p; i++)
	//
	//  Underestimate smallest eigenvalue of inverse(D)*H*inverse(D).
	//
	  k = 1;
	  t1 = w[diag] - w[1];

	  for ( i = 2; i <= p; i++) {
	    j = diag0 + i;
	    t = w[j] - w[i];
	    if ( t < t1 ) {
	      t1 = t;
	      k = i;
	    }
	  } // for (i = 1; i <= p; i++)
	  
	  sk = w[k];
	  j = diag0 + k;
	  akk = w[j];
	  k1 = ( k * ( k - 1 ) ) / 2 + 1;
	  inc = 1;
	  t = 0.0;

	  for (i = 1; i <= p; i++) {

	    if ( i == k ) {
	      inc = i;
	      k1 = k1 + inc;
	    }
	    else {
	      aki = Math.abs(dihdi[k1]);
	      si = w[i];
	      j = diag0 + i;
	      t1 = 0.5 * (akk - w[j] + si - aki);
	      t1 = t1 + Math.sqrt(t1*t1 + sk*aki);
	      if (t < t1) {
	    	  t = t1;
	      }
	      if ( k <= i ) {
	        inc = i;
	      }
	      k1 = k1 + inc;
	    } // else

	  } // for (i = 1; i <= p; i++)

	  w[emin] = akk - t;
	  uk = v[dgnorm] / rad - w[emin];
	//
	//  Compute Gerschgorin overestimate of largest eigenvalue.
	//
	  k = 1;
	  t1 = w[diag] + w[1];

	  for (i = 2; i <= p; i++) {
	    j = diag0 + i;
	    t = w[j] + w[i];
	    if ( t1 < t ) {
	      t1 = t;
	      k = i;
	    }
	  } // for (i = 2; i <= p; i++)

	  sk = w[k];
	  j = diag0 + k;
	  akk = w[j];
	  k1 = ( k * ( k - 1 ) ) / 2 + 1;
	  inc = 1;
	  t = 0.0;

	  for ( i = 1; i <= p; i++) {
	    if (i == k) {
	      inc = i;
	      k1 = k1 + inc;
	    }
	    else {
	      aki = Math.abs ( dihdi[k1] );
	      si = w[i];
	      j = diag0 + i;
	      t1 = 0.5 * ( w[j] + si - aki - akk );
	      t1 = t1 + Math.sqrt ( t1 * t1 + sk * aki );
	      if (t < t1) {
	    	  t = t1;
	      }
	      if ( k <= i ) {
	        inc = i;
	      }
	      k1 = k1 + inc;
	    } // else
	  } // for (i = 1; i <= p; i++)

	  w[emax] = akk + t;
	  lk = Math.max ( lk, v[dgnorm] / rad - w[emax] );
	//
	//  ALPHAK = current value of ALPHA.  We
	//  use More's scheme for initializing it.
	//
	  alphak = Math.abs ( v[stppar] ) * v[rad0] / rad;
	//
	//  Compute L0 for positive definite H.
	//
	  if ( irc[0] == 0 ) {
        arr = new double[p+1];
        for (ii = 1; ii <= p; ii++) {
        	arr[ii] = w[q + ii - 1];
        }
	    livmul(p, w, l, arr);
	    t = v2norm(p, w);
	    w[phipin] = dst / t / t;
	    lk = Math.max ( lk, phi * w[phipin] );

	  }
	  do210 = true;
	} // if (do70)
	
	loop2: while (true) {
    if (do210) {
    	do210 = false;
	//
	//  Safeguard ALPHAK and add ALPHAK*IDENTITY to inverse(D)*H*inverse(D).
	//

	  ka[0] = ka[0] + 1;

	  if ( -v[dst0] >= alphak || alphak < lk || alphak >= uk ) {
	    alphak = uk * Math.max ( 0.001, Math.sqrt ( lk / uk ) );
	  }

	  k = 0;
	  for ( i = 1; i <= p; i++) {
	    k = k + i;
	    j = diag0 + i;
	    dihdi[k] = w[j] + alphak;
      } // for (i = 1; i <= p; i++) 
	//
	//  Try computing Cholesky decomposition.
	//
	  lsqrt(1, p, l, dihdi, irc);
	//
	//  inverse(D)*H*inverse(D) + ALPHAK*IDENTITY  is indefinite.  Overestimate
	//  smallest eigenvalue for use in updating LK.
	//
	  if ( irc[0] != 0 ) {

	    j = ( irc[0] * ( irc[0] + 1 ) ) / 2;
	    t = l[j];
	    l[j] = 1.0;
	    for (ii = 1; ii < irc[0]; ii++) {
	        w[ii] = 0.0;
	    }
	    w[irc[0]] = 1.0;
	    litvmu ( irc[0], w, l, w );
	    t1 = v2norm ( irc[0], w );
	    lk = alphak - t / t1 / t1;
	    v[dst0] = -lk;
	    do210 = true;
        continue loop2;
	  }
	//
	//  ALPHAK makes inverse(D)*H*inverse(D) positive definite.
	//  Compute Q = -D * STEP, check for convergence.
	//
	  arr = new double[p+1];
	  livmul(p, arr, l, dig);
	  for (ii = 1; ii <= p; ii++) {
		  w[q + ii - 1] = arr[ii];
	  }
	  arr2 = new double[p+1];
	  litvmu(p, arr2, l, arr);
	  for (ii = 1; ii <= p; ii++) {
		  w[q + ii - 1] = arr2[ii];
	  }
	  dst = v2norm(p, arr2);
	  phi = dst - rad;

	  if (phi <= phimax && phi >= phimin) {
		  do290 = true;
	  }
	  else if (phi == oldphi) {
		  do290 = true;
	  }
	  else {
	      oldphi = phi;

	      if ( phi > 0.0) {
	    	  do260 = true;
	      }
	      //
          //  Check for the special case of H + ALPHA*D**2  (nearly)
          //  singular.  delta is >= the smallest eigenvalue of
          //  inverse(D)*H*inverse(D) + ALPHAK*IDENTITY.
          //
	      else if ( v[dst0] > 0.0 ) {
	    	  do260 = true;
	      }
	      else {
	          delta = alphak + v[dst0];
	          arr = new double[p+1];
	          for (ii = 1; ii <= p; ii++) {
	        	  arr[ii] = w[q + ii - 1];
	          }
	          twopsi = alphak * dst * dst + dotprd ( p, dig, arr );

	          if ( delta < psifac*twopsi ) {
	              do270 = true;
	          }
	          else {
	        	  do260 = true;
	          }
	      } // else
	  } // else
    } // if (do210)
	if (do260) {
		do260 = false;
	//
	//  Unacceptable ALPHAK.  Update LK, UK, ALPHAK.
	//

	if (ka[0] >= kalim) {
		do290 = true;
	}
	else {
	  arr = new double[p+1];
	  for (ii = 1; ii <= p; ii++) {
		  arr[ii] = w[q + ii - 1];
	  }
	  livmul(p, w, l, arr);
	  t1 = v2norm(p, w);
	  //
	  //  The following min is necessary because of restarts.
	  //
	  if ( phi < 0.0 ) {
	    uk = Math.min ( uk, alphak );
	  }

	  alphak = alphak + ( phi / t1 ) * ( dst / t1 ) * ( dst / rad );
	  lk = Math.max ( lk, alphak );
	  do210 = true;
	  continue;
	  } // else
	} // if (do260)
	if (do270) {
		do270 = false;
	//
	//  Decide how to handle nearly singular H + ALPHA*D**2.
	//
	//  If not yet available, obtain machine dependent value dgxfac.
	//

	  if ( dgxfac_gqtstp == 0.0 ) {
	    dgxfac_gqtstp = epsfac * epsilon;
	  }
	//
	//  Is DELTA so small we cannot handle the special case in
	//  the available arithmetic?  If so, accept STEP as it is.
	//
	  if ( dgxfac_gqtstp * w[dggdmx] < delta ) {
		loop4: while (true) {
	    //
	    //  Handle nearly singular H + ALPHA*D**2.
	    // Negate ALPHAK to indicate special case.
	    //
	    alphak = -alphak;
	    //
	    //  Allocate storage for scratch vector X.
	    //
	    x0 = q0 + p;
	    x = x0 + 1;
	    //
	    //  Use inverse power method with start from LSVMIN to obtain
	    //  approximate eigenvector corresponding to smallest eigenvalue
	    //  of inverse ( D ) * H * inverse ( D ).
	    //
	    delta = kappa * delta;
	    arr = new double[p+1];
	    t = lsvmin(p, l, arr, w);
	    for (ii = 1; ii <= p; ii++) {
	    	w[x + ii - 1] = arr[ii];
	    }
	    k = 0;
	loop3: while (true) {
	//
	//  Normalize W.
	//
        for (ii = 1; ii <= p; ii++) {
	      w[ii] = t * w[ii];
        }
	//
	//  Complete current inverse power iteration.  
	//  Replace W by inverse ( L' ) * W.
	//
	      litvmu ( p, w, l, w );
	      t1 = 1.0 / v2norm(p, w);
	      t = t1 * t;

	      if ( t <= delta ) {
	        break loop3;
	      }

	      if ( 30 < k ) {
	        do290 = true;
	        break loop4;
	      }

	      k = k + 1;
	//
	//  Start next inverse power iteration by storing normalized W in X.
	//
	      for (i = 1; i <= p; i++) {
	        j = x0 + i;
	        w[j] = t1 * w[i];
	      }
	//
	//  Compute W = inverse ( L ) * X.
	//
	      arr = new double[p+1];
	      for (ii = 1; ii <= p; ii++) {
	    	  arr[ii] = w[x + ii - 1];
	      }
	      livmul(p, w, l, arr);
	      t = 1.0 / v2norm(p, w);
	} // loop3: while(true);

	    for (ii = 1; ii <= p; ii++) {
	        w[ii] = t1 * w[ii];
	    }
	//
	//  Now W is the desired approximate unit eigenvector and
	//  T * X = ( inverse(D) * H * inverse(D) + ALPHAK * I ) * W.
	//
	    arr = new double[p+1];
	    for (ii = 1; ii <= p; ii++) {
	    	arr[ii] = w[q + ii - 1];
	    }
	    sw = dotprd ( p, arr, w );
	    t1 = ( rad + dst ) * ( rad - dst );
	    root = Math.sqrt ( sw * sw + t1 );
	    if ( sw < 0.0 ) {
	      root = -root;
	    }
	    si = t1 / (sw + root);
	//
	//  Accept current step if adding SI * W would lead to a
	//  further relative reduction in PSI of less than V(EPSLON) / 3.
	//
	    v[preduc] = 0.5 * twopsi;
	    t1 = 0.0; 
	    for (ii = 1; ii <= p; ii++) {
	    	arr[ii] = w[x + ii - 1];
	    }
	    t = si * ( alphak * sw
	      - 0.5 * si * ( alphak + t * dotprd ( p, arr, w ) ) );

	    if ( epso6 * twopsi <= t ) {
	      v[preduc] = v[preduc] + t;
	      dst = rad;
	      t1 = -si;
	    }

	    for (i = 1; i <= p; i++) {
	      j = q0 + i;
	      w[j] = t1 * w[i] - w[j];
	      step[i] = w[j] / d[i];
	    }

	    for (ii = 1; ii <= p; ii++) {
	    	arr[ii] = w[q + ii - 1];
	    }
	    v[gtstep] = dotprd ( p, dig, arr );
	//
	//  Save values for use in a possible restart.
	//
	    v[dstnrm] = dst;
	    v[stppar] = alphak;
	    w[lk0] = lk;
	    w[uk0] = uk;
	    v[rad0] = rad;
	    w[dstsav] = dst;
	//
	//  Restore diagonal of DIHDI.
	//
	    j = 0;
	    for ( i = 1; i <= p; i++) {
	      j = j + i;
	      k = diag0 + i;
	      dihdi[j] = w[k];
	    }

	    return;
		} // loop4: while (true)

	  } // if ( dgxfac_gqtstp * w[dggdmx] < delta )
	  do290 = true;
	} // if (do270)
	if (do290) {
	//
	//  Successful step.  Compute STEP = - inverse ( D ) * Q.
	//

	  for (i = 1; i <= p; i++) {
	    j = q0 + i;
	    step[i] = -w[j] / d[i];
	  }
	  arr = new double[p+1];
	  for (ii = 1; ii <= p; ii++) {
		  arr[ii] = w[q + ii - 1];
	  }
	  v[gtstep] = -dotprd(p, dig, arr);
	  v[preduc] = 0.5 * ( Math.abs ( alphak ) *dst*dst - v[gtstep]);
	//
	//  Save values for use in a possible restart.
	//
	  v[dstnrm] = dst;
	  v[stppar] = alphak;
	  w[lk0] = lk;
	  w[uk0] = uk;
	  v[rad0] = rad;
	  w[dstsav] = dst;
	//
	//  Restore diagonal of DIHDI.
	//
	  j = 0;
	  for ( i = 1; i <= p; i++) {
	    j = j + i;
	    k = diag0 + i;
	    dihdi[j] = w[k];
	  }

	  return;
	} // if (do290)
	} // loop2: while(true)
	} // private void gqtstp
	
	private void itsmry ( double d[], int iv[], int p, double v[], double x[] ) {

	/***********************************************************************
	!
	!! ITSMRY prints an iteration summary.
	!
	!  Modified:
	!
	!    06 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, real D(P), the scale vector.
	!
	!    Input/output, integer IV(*), the NL2SOL integer parameter array.
	!
	!    Input, integer P, the number of variables.
	!
	!    Input, real V(*), the NL2SOL real array.
	!
	!    Input, real X(P), the current estimate of the minimizer.
	*/
	  

	  int cov1;
	  int covmat = 26;
	  int covprt = 14;
	  int covreq = 15;
	  int dstnrm = 2;
	  int f = 10;
	  int f0 = 13;
	  int fdif = 11;
	  int g = 28;
	  int g1;
	  int i;
	  int i1;
	  int ii;
	  int iv1;
	  int j;
	  int m;
	  String model[] = new String[7];
	  int needhd = 39;
	  int nf;
	  int nfcall = 6;
	  int nfcov = 40;
	  int ng;
	  int ngcall = 30;
	  int ngcov = 41;
	  int niter = 31;
	  int nreduc = 6;
	  double nreldf;
	  int ol;
	  double oldf;
	  int outlev = 19;
	  int preduc = 7;
	  double preldf;
	  int prntit = 48;
	  int prunit = 21;
	  int pu;
	  double reldf;
	  int reldx = 17;
	  int size = 47;
	  int solprt = 22;
	  int statpr = 23;
	  int stppar = 5;
	  int sused = 57;
	  int x0prt = 24;
	  boolean do180 = true;

	  model[1] = new String("      G");
	  model[2] = new String("      S");
	  model[3] = new String("    G-S");
	  model[4] = new String("    S-G");
	  model[5] = new String("  G-S-G");
	  model[6] = new String("  S-G-S");
	   

	  pu = iv[prunit];

	  if ( pu == 0 ) {
	    return;
	  }

	  iv1 = iv[1];
	  ol = iv[outlev];

	  if ( iv1 < 2 || 15 < iv1 ) {
	    Preferences.debug( "IV(1) = " +  iv1 + "\n", Preferences.DEBUG_ALGORITHM);
	    return;
	  }

	  
	 if ((ol != 0) && (iv1 < 12)  && ((iv1 < 10) || (iv[prntit] != 0))) {

	  if ( iv1 <= 2 ) {
	    iv[prntit] = iv[prntit] + 1;
	    if (iv[prntit] < Math.abs ( ol ) ) {
	      return;
	    }
	  } // if (iv1 <= 2)

	      nf = iv[nfcall] - Math.abs ( iv[nfcov] );
	      iv[prntit] = 0;
	      reldf = 0.0;
	      preldf = 0.0;
	      oldf = v[f0];

	      if ( 0.0 < oldf ) {
	         reldf = v[fdif] / oldf;
	         preldf = v[preduc] / oldf;
	      }
	//
	//  Print short summary line.
	//
	      if ( ol <= 0 ) {
	         iv[needhd] = 0;
	         Preferences.debug("iv[niter] = iv["+niter+"] = " + iv[niter] + "\n", Preferences.DEBUG_ALGORITHM);
	         Preferences.debug("nf = " + nf + "\n", Preferences.DEBUG_ALGORITHM);
	         Preferences.debug("v[f] = v["+f+"] = " + v[f] + "\n", Preferences.DEBUG_ALGORITHM);
	         Preferences.debug("reldf = " + reldf + "\n", Preferences.DEBUG_ALGORITHM);
	         Preferences.debug("preldf = " + preldf + "\n", Preferences.DEBUG_ALGORITHM);
	         Preferences.debug("v[reldx] = v["+ reldx + "] = " + v[reldx] + "\n", Preferences.DEBUG_ALGORITHM);
	      } // if (ol <= 0)
	//
    //  Print long summary line.
	//
	      else {
		      iv[needhd] = 0;
		      m = iv[sused];
		      if ( 0.0 < oldf ) {
		        nreldf = v[nreduc] / oldf;
		      }
		      else {
		        nreldf = 0.0;
		      }
	
		      Preferences.debug("iv[niter] = iv["+niter+"] = " + iv[niter] + "\n", Preferences.DEBUG_ALGORITHM);
		      Preferences.debug("nf = " + nf + "\n", Preferences.DEBUG_ALGORITHM);
		      Preferences.debug("v[f] = v["+f+"] = " + v[f] + "\n", Preferences.DEBUG_ALGORITHM);
		      Preferences.debug("reldf = " + reldf + "\n", Preferences.DEBUG_ALGORITHM);
	          Preferences.debug("preldf = " + preldf + "\n", Preferences.DEBUG_ALGORITHM);
	          Preferences.debug("v[reldx] = v["+ reldx + "] = " + v[reldx] + "\n", Preferences.DEBUG_ALGORITHM);
	          Preferences.debug("model[m] = model["+m+"] = " + model[m] + "\n", Preferences.DEBUG_ALGORITHM);
	          Preferences.debug("v[stppar] = v["+stppar+"] = " + v[stppar] + "\n", Preferences.DEBUG_ALGORITHM);
	          Preferences.debug("v[size] = v["+size+"] = " + v[size] + "\n", Preferences.DEBUG_ALGORITHM);
	          Preferences.debug("v[dstnrm] = v["+dstnrm+"] = " + v[dstnrm] + "\n", Preferences.DEBUG_ALGORITHM);
	          Preferences.debug("nreldf = " + nreldf + "\n", Preferences.DEBUG_ALGORITHM);
	      } // else print the long summary line
	  } // if ((ol != 0) && (iv1 < 12)  && ((iv1 < 10) || (iv[prntit] != 0)))

	  if ( iv1 == 1 ) {

	    return;
	  }
	  else if ( iv1 == 2 ) {

	    return;
	  }
	  else if ( iv1 == 3) {
	    Preferences.debug("X-convergence.\n", Preferences.DEBUG_ALGORITHM);
	  }
	  else if ( iv1 == 4 ) {
	    Preferences.debug("Relative function convergence.\n", Preferences.DEBUG_ALGORITHM);
	  }
	  else if ( iv1 == 5 ) {
	    Preferences.debug("X- and relative function convergence.\n", Preferences.DEBUG_ALGORITHM);
	  }
	  else if ( iv1 == 6 ) {
	    Preferences.debug("Absolute function convergence.\n", Preferences.DEBUG_ALGORITHM);
	  }
	  else if ( iv1 == 7 ) {
	    Preferences.debug("Singular convergence.\n", Preferences.DEBUG_ALGORITHM);
	  }
	  else if ( iv1 == 8 ) {
	    Preferences.debug("False convergence.\n", Preferences.DEBUG_ALGORITHM);
	  }
	  else if ( iv1 == 9 ) {
	    Preferences.debug("Function evaluation limit.\n", Preferences.DEBUG_ALGORITHM);
	  }
	  else if ( iv1 == 10 ) {
	    Preferences.debug("Iteration limit.\n", Preferences.DEBUG_ALGORITHM);
	  }
	  else if ( iv1 == 11 ) {
	    Preferences.debug("Stopx.\n", Preferences.DEBUG_ALGORITHM);
	  }
	  else if ( iv1 == 14 ) {
	    Preferences.debug("Bad parameters to ASSESS.\n", Preferences.DEBUG_ALGORITHM);
	    return;
	  }
	//
	//  Initial call on ITSMRY.
	//
	  else if ( iv1 == 12 || iv1 == 13 || iv1 == 15 ) {
        loop1: while(true) {
	    if ( iv1 == 15 ) {
	      Preferences.debug("J could not be computed.\n", Preferences.DEBUG_ALGORITHM);
	      if ( 0 < iv[niter] ) {
	        do180 = false;
	        break loop1;
	      }
	    }

	    if ( iv1 == 13 ) {
	      Preferences.debug("Initial sum of squares overflows.\n", Preferences.DEBUG_ALGORITHM);
	    }

	    if ( iv[x0prt] != 0 ) {
	      for (i = 1; i <= p; i++) {
	    	  Preferences.debug("i = " + i + " initial x[" + i + "] = " + x[i] + " d[" + i + "] = " + d[i] + "\n", 
	    			  Preferences.DEBUG_ALGORITHM);
	      }
	    }


	    if ( iv1 == 13 ) {
	      return;
	    }

	    iv[needhd] = 0;
	    iv[prntit] = 0;

	    if ( ol == 0 ) {
	      return;
	    }
        Preferences.debug("it = 0\n", Preferences.DEBUG_ALGORITHM);
	    Preferences.debug("nf = 1\n", Preferences.DEBUG_ALGORITHM);
	    Preferences.debug("v[" + f + "] = " + v[f] + "\n", Preferences.DEBUG_ALGORITHM);
	    return;
        } //loop1: while(true)
	  } // else if ( iv1 == 12 || iv1 == 13 || iv1 == 15 )
	  else {
	    return;

	  }
	if (do180) {
	//
	//  Print various information requested on solution.
	//

	      iv[needhd] = 1;

	      if ( iv[statpr] != 0 ) {

	         oldf = v[f0];

	         if ( 0.0 < oldf ) {
	           preldf = v[preduc] / oldf;
	           nreldf = v[nreduc] / oldf;
	         }
	         else {
	           preldf = 0.0;
	           nreldf = 0.0;
	         }

	         nf = iv[nfcall] - iv[nfcov];
	         ng = iv[ngcall] - iv[ngcov];
	         Preferences.debug("function v[f] = v["+f+"] = " + v[f] + "\n", Preferences.DEBUG_ALGORITHM);
	         Preferences.debug("v[reldx] = v[" + reldx + "] = " + v[reldx] + "\n", Preferences.DEBUG_ALGORITHM);
	         Preferences.debug("func evals nf = " + nf + "\n", Preferences.DEBUG_ALGORITHM);
	         Preferences.debug("grad evals ng = " + ng + "\n", Preferences.DEBUG_ALGORITHM);
	         Preferences.debug("preldf = " + preldf + "\n", Preferences.DEBUG_ALGORITHM);
	         Preferences.debug("nreldf = " + nreldf + "\n", Preferences.DEBUG_ALGORITHM);

	         if ( 0 < iv[nfcov] ) {
	           Preferences.debug("Extra function evaluations for covariance iv[nfcov] = iv[" + nfcov + "] = " +
	        		   iv[nfcov] + "\n", Preferences.DEBUG_ALGORITHM);
	         }

	         if ( 0 < iv[ngcov] ) {
	           Preferences.debug("Extra gradient evaluations for covariance = iv[ngcov] = iv[" + ngcov + "] = " +
	        		   iv[ngcov] + "\n", Preferences.DEBUG_ALGORITHM);
	         }
	      } // if (iv[statpr] != 0)
	} // if (do180)

	      if ( iv[solprt] != 0 ) {

	         iv[needhd] = 1;
	         g1 = iv[g];

	         for (i = 1; i <= p; i++) {
	           Preferences.debug("i = " + i + " final x[" + i + "] = " + x[i] + " d[" + i + "] = " + d[i] +
	        		   " v[" + g1 + "] = " + v[g1] + "\n", Preferences.DEBUG_ALGORITHM);
	           g1 = g1 + 1;
	         } // for (i = 1; i <= p; i++)

	      } // if (iv[solprt] != 0)

	      if ( iv[covprt] == 0 ) {
	        return;
	      }

	      cov1 = iv[covmat];
	      iv[needhd] = 1;

	      if ( cov1 < 0 ) {

	        if ( -1 == cov1 ) {
	          Preferences.debug("Indefinite covariance matrix\n", Preferences.DEBUG_ALGORITHM);
	        }
	        else if (-2 == cov1) {
	          Preferences.debug("Oversize steps in computing covariance\n", Preferences.DEBUG_ALGORITHM);
	        }
	      } // if (cov1 < 0)
	      else if ( cov1 == 0 ) {
	        Preferences.debug("Covariance matrix not computed\n", Preferences.DEBUG_ALGORITHM);
	      }
	      else if ( 0 < cov1 ) {

	        i = Math.abs ( iv[covreq] );
	        if ( i <= 1 ) {
	          Preferences.debug("Covariance = scale * H**-1 * (J'' * J) * H**-1\n", Preferences.DEBUG_ALGORITHM);
	        }
	        else if ( i == 2 ) {
	          Preferences.debug("Covariance = scale * inverse ( H )\n", Preferences.DEBUG_ALGORITHM);
	        }
	        else if ( 3 <= i ) {
	          Preferences.debug("Covariance = scale * inverse ( J'' * J )\n", Preferences.DEBUG_ALGORITHM);
	        }

	        ii = cov1 - 1;
	        if ( ol <= 0 ) {
	          for (i = 1; i <= p; i++) {
	            i1 = ii + 1;
	            ii = ii + i;
	            Preferences.debug("row i = " + i + "\n", Preferences.DEBUG_ALGORITHM);
	            for (j = i1; j <= ii; j++) {
	            	Preferences.debug("v[" + j + "] = " + v[j] + "\n", Preferences.DEBUG_ALGORITHM);
	            }
	          }
	        } // if (o1 <= 0)
	        else {

	          for ( i = 1;i <= p; i++) {
	            i1 = ii + 1;
	            ii = ii + i;
	            Preferences.debug("row i = " + i + "\n", Preferences.DEBUG_ALGORITHM);
	            for (j = i1; j <= ii; j++) {
	            	Preferences.debug("v[" + j + "] = " + v[j] + "\n", Preferences.DEBUG_ALGORITHM);
	            }
	          }

	        } // else

	      } // else if (0 < cov1)

	  return;
	} // private void itsmry
	
	private void linvrt ( int n, double lin[], double l[] ) {

	/***********************************************************************
	!
	!! LINVRT computes the inverse of a lower triangular matrix.
	!
	!  Discussion:
	!
	!    LIN = inverse ( L ), both N by N lower triangular matrices stored
	!    compactly by rows.  LIN and L may share the same storage.
	!
	!  Modified:
	!
	!    05 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer N, the order of L and LIN.
	!
	!    Output, real LIN((N*(N+1))/2), the inverse of L, a lower triangular
	!    matrix stored by rows.
	!
	!    Input, real L((N*(N+1))/2), a lower triangular matrix stored by rows.
	*/
	  

	  int i;
	  int ii;
	  int j0;
	  int j1;
	  int jj;
	  int k;
	  int k0;
	  double t;

	  j0 = ( n * ( n + 1 ) ) / 2;

	  for (ii = 1; ii <= n; ii++) {

	    i = n + 1 - ii;
	    lin[j0] = 1.0 / l[j0];

	    if ( i <= 1 ) {
	      return;
	    }

	    j1 = j0;

	    for (jj = 1; jj <= i - 1; jj++) {

	      t = 0.0;
	      j0 = j1;
	      k0 = j1 - jj;

	      for (k = 1; k <= jj; k++) {
	        t = t - l[k0] * lin[j0];
	        j0 = j0 - 1;
	        k0 = k0 + k - i;
	      }

	      lin[j0] = t / l[k0];

	    } // for (jj = 1; jj <= i-1; jj++)

	    j0 = j0 - 1;

	  } // for (ii = 1; ii <= n; ii++)

	  return;
	} // private void linvrt
	
	private void litvmu ( int n, double x[], double l[], double y[] ) {

	/***********************************************************************
	!
	!! LITVMU solves L' * X = Y, where L is a lower triangular matrix.
	!
	!  Discussion:
	!
	!    This routine solves L' * X = Y, where L is an N by N lower
	!    triangular matrix stored compactly by rows.  X and Y may occupy 
	!    the same storage.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer N, the order of L.
	!
	!    Output, real X(N), the solution.
	!
	!    Input, real L((N*(N+1))/2), the lower triangular matrix, stored
	!    by rows.
	!
	!    Input, real Y(N), the right hand side.
	*/

	  int i;
	  int i0;
	  int ii;
	  int ij;
	  int j;
	  double xi;
	  int jj;

	  for (jj = 1; jj <= n; jj++) {
	      x[jj] = y[jj];
	  }
	  i0 = ( n * ( n + 1 ) ) / 2;

	  for (ii = 1; ii <= n; ii++) {

	    i = n + 1 - ii;
	    xi = x[i] / l[i0];
	    x[i] = xi;

	    if ( i <= 1 ) {
	      return;
	    }

	    i0 = i0 - i;

	    if ( xi != 0.0 ) {

	      for ( j = 1; j <= i - 1; j++) {
	        ij = i0 + j;
	        x[j] = x[j] - xi * l[ij];
	      }

	    } // if (xi != 0.0)

	  } // for (ii = 1; ii <= n; ii++)

	  return;
	} // private void litvmu
	
	private void livmul ( int n, double x[], double l[], double y[] ) {

	/***********************************************************************
	!
	!! LIVMUL solves L * X = Y, where L is a lower triangular matrix.
	!
	!  Discussion:
	!
	!    This routine solves L * X = Y, where L is an N by N lower 
	!    triangular matrix stored compactly by rows.  X and Y may occupy 
	!    the same storage.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer N, the order of L.
	!
	!    Output, real X(N), the solution.
	!
	!    Input, real L((N*(N+1))/2), the lower triangular matrix, stored
	!    by rows.
	!
	!    Input, real Y(N), the right hand side.
	*/

	  int i;
	  int j;
	  double t;
	  double arr[];
	  int ii;

	  x[1] = y[1] / l[1];

	  j = 1;

	  for ( i = 2; i <= n; i++) {
		arr = new double[i];
		for (ii = 1; ii <= i-1; ii++) {
			arr[ii] = l[j+ii];
		}
	    t = dotprd ( i-1, arr, x );
	    j = j + i;
	    x[i] = ( y[i] - t ) / l[j];
	  }

	  return;
	} // private void livmul
	
	private void lmstep ( double d[], double g[], int ierr[], int ipivot[],
			              int ka[], int p, double qtr[], double r[], double step[],
			              double v[], double w[] ) {

	/***********************************************************************
	!
	!! LMSTEP computes a Levenberg-Marquardt step by More-Hebden techniques.
	!
	!  Discussion:
	!
	!    Given the R matrix from the QR decomposition of a jacobian
	!    matrix, J, as well as Q' times the corresponding
	!    residual vector, RESID, this subroutine computes a Levenberg-
	!    Marquardt step of approximate length V(RADIUS) by the More
	!    technique.
	!
	!    If it is desired to recompute step using a different value of
	!    V(RADIUS), then this routine may be restarted by calling it
	!    with all parameters unchanged except V(RADIUS).  This explains
	!    why many parameters are listed as I/O.  On an initial call
	!    with KA = -1, the caller need only have initialized D, G, KA, P,
	!    QTR, R, V(EPSLON), V(PHMNFC), V(PHMXFC), V(RADIUS), and V(RAD0).
	!
	!    This code implements the step computation scheme described in
	!    refs. 2 and 4.  Fast Givens transformations (see reference 3, 
	!    pages 60-62) are used to compute step with a nonzero Marquardt 
	!    parameter.
	!
	!    A special case occurs if J is nearly singular and V(RADIUS)
	!    is sufficiently large.  In this case the step returned is such
	!    that  twonorm(R)**2 - twonorm(R - J * STEP)**2  differs from its
	!    optimal value by less than V(EPSLON) times this optimal value,
	!    where J and R denote the original jacobian and residual.  See
	!    reference 2 for more details.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Reference:
	!
	!    John Dennis, David Gay, Roy Welsch,
	!    An Adaptive Nonlinear Least Squares Algorithm,
	!    ACM Transactions on Mathematical Software,
	!    Volume 7, Number 3, 1981.
	!
	!    David Gay,
	!    Computing Optimal Locally Constrained Steps,
	!    SIAM Journal on Scientific and Statistical Computing, 
	!    Volume 2, Number 2, pages 186-197, 1981.
	!
	!    Charles Lawson and Richard Hanson,
	!    Solving Least Squares Problems,
	!    Prentice Hall, 1974.
	!
	!    Jorge More,
	!    The Levenberg-Marquardt Algorithm, Implementation and Theory, 
	!    in Springer Lecture Notes in Mathematics, Number 630, 
	!    edited by G A Watson,
	!    Springer Verlag, Berlin and New York, pages 105-116, 1978.
	!
	!  Parameters:
	!
	!    Input, real D(P), the scale vector.
	!
	!    Input, real G(P), the gradient vector J'*R.
	!
	!   ierr (i/o) = return code from QRFACT or QRFGS -- 0 means r has
	!             full rank.
	!
	! ipivot (i/o) = permutation array from QRFACT or QRFGS, which compute
	!             qr decompositions with column pivoting.
	!
	!     ka (i/o).  ka < 0 on input means this is the first call on
	!             lmstep for the current r and qtr.  on output ka con-
	!             tains the number of Hebden iterations needed to determine
	!             step.  ka = 0 means a Gauss-Newton step.
	!
	!      p (in)  = number of parameters.
	!
	!    qtr (in)  = Q' * residual.
	!
	!      r (in)  = the R matrix, stored compactly by columns.
	!
	!   step (out) = the Levenberg-Marquardt step computed.
	!
	!      v (i/o) contains various constants and variables described below.
	!
	!      w (i/o) = workspace of length p*(p+5)/2 + 4.
	!
	!  entries in v
	!
	! v(dgnorm) (i/o) = 2-norm of (d**-1)*g.
	! v(dstnrm) (i/o) = 2-norm of d * step.
	! v(dst0)   (i/o) = 2-norm of Gauss-Newton step (for nonsing. j).
	! v(epslon) (in) = max. relative error allowed in twonorm(r)**2 minus
	!             twonorm(r - j * step)**2.  (see algorithm notes below.)
	! v(gtstep) (out) = inner product between G and STEP.
	! v(nreduc) (out) = half the reduction in the sum of squares predicted
	!             for a Gauss-Newton step.
	! v(phmnfc) (in)  = tol. (together with v(phmxfc)) for accepting step
	!             (More's sigma).  the error v(dstnrm) - v(radius) must lie
	!             between v(phmnfc)*v(radius) and v(phmxfc)*v(radius).
	! v(phmxfc) (in)  (see v(phmnfc).)
	! v(preduc) (out) = half the reduction in the sum of squares predicted
	!             by the step returned.
	! v(radius) (in)  = radius of current (scaled) trust region.
	! v(rad0)   (i/o) = value of v(radius) from previous call.
	! v(STPPAR) (i/o) = Marquardt parameter (or its negative if the special
	!             case mentioned below in the algorithm notes occurs).
	*/

	  double a;
	  double adi = 0.0;
	  double alphak = 0.0;
	  double b;
	  double d1 = 0.0;
	  double d2 = 0.0;
	  final double dfac = 256.0;
	  double dfacsq;
	  final int dgnorm = 1;
	  double dst = 0.0;
	  final int dst0 = 3;
	  final int dstnrm = 2;
	  int dstsav;
	  double dtol;
	  final int epslon = 19;
	  int i;
	  int i1;
	  int j1;
	  int k;
	  int kalim;
	  int l = 1;
	  double lk;
	  int lk0;
	  double oldphi;  
	  double phi = 0.0;
	  double phimax;
	  double phimin;
	  int phipin;
	  int pp1o2;
	  double psifac = 0.0;
	  double rad;
	  final int rad0 = 9;
	  int res;
	  int res0;
	  int rmat;
	  int rmat0;
	  double si = 0.0;
	  double sj;
	  double sqrtak = 0.0;
	  final int stppar = 5;
	  double t;
	  double twopsi;
	  double uk;
	  int uk0;
	  double wl = 0.0;
	  int m;
	  double arr[];
	  double arr2[];
	  double arr3[];
	  boolean do5 = false;
	  boolean do10 = false;
	  boolean do20 = false;
	  boolean do30 = false;
	  boolean do110 = false;
	  boolean do120 = false;
	  boolean do130 = false;
	  boolean do150 = false;
	  boolean do170 = false;
	  boolean do180 = false;
	  boolean do200 = false;
	  boolean do220 = false;
	  boolean do240 = false;
	  boolean do260 = false;
	  boolean do280 = false;
	  boolean do320 = false;
	  boolean do370 = false;
	  boolean do410 = false;
	  boolean do430 = false;
	  boolean do440 = false;
	//
	//  subscripts for v
	//
	      final int gtstep = 4;
	      final int nreduc = 6;
	      final int phmnfc = 20;
	      final int phmxfc = 21;
	      final int preduc = 7;
	      final int radius = 8;
	      
	//
	//  For use in recomputing STEP, the final values of LK and UK,
	//  the inverse derivative of More's PHI at 0 (for nonsingular J)
	//  and the value returned as V(DSTNRM) are stored at W(LK0),
	//  W(UK0), W(PHIPIN), and W(DSTSAV) respectively.
	//
	  lk0 = p + 1;
	  phipin = lk0 + 1;
	  uk0 = phipin + 1;
	  dstsav = uk0 + 1;
	  rmat0 = dstsav;
	//
	//  A copy of the R matrix from the QR decomposition of J is
	//  stored in W starting at W(RMAT), and a copy of the residual
	//  vector is stored in W starting at W(RES).  The loops below
	//  that update the QR decomposition for a nonzero Marquardt parameter
	//  work on these copies.
	//
	  rmat = rmat0 + 1;
	  pp1o2 = ( p * ( p + 1 ) ) / 2;
	  res0 = pp1o2 + rmat0;
	  res = res0 + 1;
	  rad = v[radius];
	  if ( 0.0 < rad ) {
	    psifac = v[epslon] / ( ( 8.0 * ( v[phmnfc] + 1.0 ) + 3.0 ) * rad*rad);
	  }
	  phimax = v[phmxfc] * rad;
	  phimin = v[phmnfc] * rad;
	//
	//  DTOL, DFAC, and DFACSQ are used in rescaling the fast Givens
	//  representation of the updated QR decomposition.
	//
	  dtol = 1.0 / dfac;
	  dfacsq = dfac * dfac;
	//
	//  OLDPHI is used to detect limits of numerical accuracy.  If
	//  we recompute STEP and it does not change, then we accept it.
	//
	  oldphi = 0.0;
	  lk = 0.0;
	  uk = 0.0;
	  kalim = ka[0] + 12;
	//
	//  Start or restart, depending on KA.
	//
	  do5 = true;
	  loop1: while (true) {
      if (do5) {
    	  do5 = false;
		  if ( 0 < ka[0] ) {
		    do370 = true;
		  }
		  else {
			  do10 = true;
		  }
      } // if (do5)
	  if (do10) {
		  do10 = false;
	//
	//  Fresh start.  Compute V(NREDUC).
	//
	  if ( ka[0] < 0 ) {
	    ka[0] = 0;
	    kalim = 12;
	    k = p;
	    if ( ierr[0] != 0 ) {
	      k = Math.abs ( ierr[0] ) - 1;
	    }
	    v[nreduc] = 0.5 * dotprd ( k, qtr, qtr );
	  } // if (ka[0] < 0) 
	  do20 =true;
	  } // if (do10)
	//
	//  Set up to try initial Gauss-Newton step.
	//
	if (do20) {
	 do20 = false;

	  v[dst0] = -1.0;
	//
	//  Compute Gauss-Newton step.
	//
	//  Note that the R matrix is stored compactly by columns in
	//  R(1), R(2), R(3), ...  It is the transpose of a
	//  lower triangular matrix stored compactly by rows, and we
	//  treat it as such when using LITVMU and LIVMUL.
	//
	  if ( ierr[0] == 0 ) {

	    litvmu ( p, w, r, qtr );
	//
	//  Temporarily store permuted -D * STEP in STEP.
	//
	    for ( i = 1; i <= p; i++) {
	      j1 = ipivot[i];
	      step[i] = d[j1] * w[i];
	    } // for (i = 1; i <= p; i++)

	    dst = v2norm(p, step);
	    v[dst0] = dst;
	    phi = dst - rad;

	    if ( phi <= phimax ) {
	      do410 = true;
	      break loop1;
	    }
	//
	//  If this is a restart, go to 110.
	//
	    if ( 0 < ka[0] ) {
	      do110 = true;
	    }
	    else {
	    	do30 = true;
			//
			//  Gauss-Newton step was unacceptable.  Compute L0.
			//
		    for (i = 1; i <= p; i++) {
		      j1 = ipivot[i];
		      step[i] = d[j1] * ( step[i] / dst );
		    } // for (i = 1; i <= p; i++)
	
		    livmul ( p, step, r, step );
		    t = 1.0 / v2norm(p, step);
		    w[phipin] = ( t / dst ) * t;
		    lk = phi * w[phipin];
	    }
	  } // if (ierr[0] == 0)
	  else {
		  do30 = true;
	  }
	} // if (do20)
	if (do30) {
		do30 = false;
	//
	//  Compute U0.
	//
      for (m = 1; m <= p; m++) {
	      w[m] = g[m] / d[m];
      }
	  v[dgnorm] = v2norm(p, w);
	  uk = v[dgnorm] / rad;
	//
	//  Special case.  RAD <= 0 or (G = 0 and J is singular).
	//
	  if ( uk <= 0.0 ) {
	    v[stppar] = 0.0;
	    dst = 0.0;
	    lk = 0.0;
	    uk = 0.0;
	    v[gtstep] = 0.0;
	    v[preduc] = 0.0;
	    for (m = 1; m <= p; m++) {
	        step[m] = 0.0;
	    }
	    v[dstnrm] = dst;
	    w[dstsav] = dst;
	    w[lk0] = lk;
	    w[uk0] = uk;
	    v[rad0] = rad;
	    return;
	  } // if (uk <= 0.0)
	//
	// ALPHAK will be used as the current Marquardt parameter.  We
    //  use More's scheme for initializing it.
	//
	  alphak = Math.abs ( v[stppar] ) * v[rad0] / rad;
	  do110 = true;
	} // if (do30)
	//
	//  Top of loop.  Increment KA, copy R to RMAT, QTR to RES.
	//
	if (do110) {
		do110 = false;

	  ka[0] = ka[0] + 1;
	  for (m = 1; m <= pp1o2; m++) {
	      w[rmat+m-1] = r[m];
	  }
	  for (m = 1; m <= p; m++) {
	      w[res+m-1] = qtr[m];
	  }
	//
	//  Safeguard ALPHAK and initialize fast Givens scale vector.
	//
	      if (alphak <= 0.0 || alphak < lk || alphak >= uk ) {
	        alphak = uk * Math.max ( 0.001, Math.sqrt ( lk / uk ) );
	      }

	      sqrtak = Math.sqrt(alphak);
	      for (m = 1; m <= p; m++) {
	          w[m] = 1.0;
	      }
	      do120 = true;
	} // if (do110)
	//
	//  Add ALPHAK * D and update QR decomposition using fast Givens transform.
	//
	      forloop: for (i = 1; (i <= p) && (!do320) && (!do370); i++) {
	      loop2: while (true) {
	      if (do120) {
	    	  do120 = false;
	//
	//  Generate, apply first Givens transformation for row I of ALPHAK * D.
	//  Use STEP to store temporary row.
	//
	         l = ( i * ( i + 1 ) ) / 2 + rmat0;
	         wl = w[l];
	         d2 = 1.0;
	         d1 = w[i];
	         j1 = ipivot[i];
	         adi = sqrtak*d[j1];

	         if ( Math.abs(wl) <= adi ) {
	        	 do150 = true;
	         }
	         else {
	        	 do130 = true;
	         }
	} // if (do120)
     if (do130) {
	     do130 = false;

	         a = adi / wl;
	         b = d2 * a / d1;
	         t = a * b + 1.0;

	         if ( t <= 2.5 ) {

	           w[i] = d1 / t;
	           d2 = d2 / t;
	           w[l] = t * wl;
	           a = -a;
	           for (j1 = i; j1 <= p; j1++) {
	              l = l + j1;
	              step[j1] = a * w[l];
	           }
	           do170 = true;

	         } // if (t <= 2.5)
	         else {
	        	 do150 = true;
	         }
     } // if (do130)
     if (do150) {
	     do150 = false;

	         b = wl / adi;
	         a = d1 * b / d2;
	         t = a * b + 1.0;

	         if (t > 2.5) {
	        	 do130 = true;
	        	 continue loop2;
	         }
	         else {
		         do170 = true;
		         w[i] = d2 / t;
		         d2 = d1 / t;
		         w[l] = t * adi;
		         for (j1 = i;j1 <= p; j1++) {
		              l = l + j1;
		              wl = w[l];
		              step[j1] = -wl;
		              w[l] = a * wl;
		         } // (j1 = i; j1 <= p; j1++)
	         } // else

     } // if (do150)
     if (do170) {
	   do170 = false;

	         if ( i == p ) {
	           do280 = true;
	           break forloop;
	         }
	//
	//  Now use Givens transformations to zero elements of temporary row.
	//
	         do180 = true;
     } // if (do170)
	         for (i1 = i + 1; i1 <= p; i1++) {
	         loop3: while (true) {
	         if (do180) {
	        	 do180 = false;
	              l = ( i1 * ( i1 + 1 ) ) / 2 + rmat0;
	              wl = w[l];
	              si = step[i1-1];
	              d1 = w[i1];
	//
	//  Rescale row I1 if necessary.
	//
	              if ( d1 < dtol ) {
	                d1 = d1 * dfacsq;
	                wl = wl / dfac;
	                k = l;
	                for (j1 = i1; j1 <= p; j1++) {
	                  k = k + j1;
	                  w[k] = w[k] / dfac;
	                }
	              } // if (d1 < tol)
	//
	//  Use Givens transformations to zero next element of temporary row.
	//
	              if (Math.abs(si) > Math.abs(wl)) {
	            	  do220 = true;
	              }
	              else if (si == 0.0) {
	            	  do260 = true;
	              }
	              else {
	            	  do200 = true;
	              }
     } // if (do180)
     if (do200) {
	     do200 = false;

	              a = si / wl;
	              b = d2 * a / d1;
	              t = a * b + 1.0;

	              if ( t <= 2.5 ) {

	                w[l] = t * wl;
	                w[i1] = d1 / t;
	                d2 = d2 / t;
	                for (j1 = i1; j1 <= p; j1++) {
	                   l = l + j1;
	                   wl = w[l];
	                   sj = step[j1];
	                   w[l] = wl + b * sj;
	                   step[j1] = sj - a*wl;
	                } // for (j1 = i1; j1 <= p; j1++)
	                do240 = true;

	              } // if (t <= 2.5)
	              else {
	            	  do220 = true;
	              }
     } // if (do200)
     if (do220) {
    	 do220 = false;
	              b = wl / si;
	              a = d1 * b / d2;
	              t = a * b + 1.0;

	              if (t > 2.5 ) {
	            	  do200 = true;
	            	  continue loop3;
	              }

	              w[i1] = d2 / t;
	              d2 = d1 / t;
	              w[l] = t * si;
	              for (j1 = i1; j1 <= p; j1++) {
	                   l = l + j1;
	                   wl = w[l];
	                   sj = step[j1];
	                   w[l] = a * wl + sj;
	                   step[j1] = b * sj - wl;
	              } // for (j1 = i1; j1 <= p; j1++)
	              do240 = true;
     } // if (do220)
     if (do240) {
	//
	//  Rescale temporary row if necessary.
	//
	     do240  = false;

	              if ( d2 < dtol ) {
	                   d2 = d2*dfacsq;
	                   for (m = i1; m <= p; m++) {
	                       step[m] = step[m] / dfac;
	                   }
	              }
	              do260 = true;
     } // if (do240)
     if (do260) {
    	 do260 = false;
    	 break loop3;
     } // if (do260)
	         } // loop3: while (true)
	         if (i1 < p){
	        	 do180 = true;
	         }
            } // for (i1 = i + 1; i1 <= p; i1++)
	        if (!do130) {
                break loop2;
	        }
	      } // loop2: while (true)
	      if (i < p) {
	    	  do120 = true;
	      }
	      else {
	    	  do280 = true;
	      }
	      } // for (i = 1; (i <= p) && (!do320) && (!do370); i++)
	//
	//  Compute step.
	//
	 if (do280) {
	     do280 = false;
          arr = new double[p+1];
          arr2 = new double[p*(p+1)/2 + 1];
          arr3 = new double[p+1];
          for (m = 1; m <= p*(p+1)/2; m++) {
        	  arr2[m] = w[rmat + m - 1];
          }
          for (m = 1; m <= p; m++) {
        	  arr3[m] = w[res + m - 1];
          }
	      litvmu ( p, arr, arr2, arr3 );
	      for (m = 1; m <= p; m++) {
	    	  w[res + m - 1] = arr[m];
	      }
	//
    //  Recover STEP and store permuted -D * STEP at W(RES).
	//
	      for ( i = 1; i <= p; i++) {
	         j1 = ipivot[i];
	         k = res0 + i;
	         t = w[k];
	         step[j1] = -t;
	         w[k] = t * d[j1];
	      } // for (i = 1; i <= p; i++)
          for (m = 1; m <= p; m++) {
        	  arr[m] = w[res + m - 1];
          }
	      dst = v2norm(p, arr);
	      phi = dst - rad;
	      if (phi <= phimax && phi >= phimin) {
	    	  do430 = true;
	    	  break loop1;
	      }
	      if (oldphi == phi) {
	    	  do430 = true;
	    	  break loop1;
	      }
	      oldphi = phi;
	//
	//  Check for and handle special case.
	//
	      if ( phi <= 0.0 ) {

	        if ( kalim <= ka[0] ) {
	          do430 = true;
	          break loop1;
	        }

	        twopsi = alphak * dst * dst - dotprd ( p, step, g );

	        if ( alphak < twopsi * psifac ) {
	          v[stppar] = -alphak;
	          do440 = true;
	          break loop1;
	        }

	      } // if (phi <= 0.0)

	      if ( phi < 0.0 ) {
	        uk = alphak;
	      }
	      do320 = true;
	 } // if (do280)

	 if (do320) {
	     do320 = false;

	      for ( i = 1; i <= p; i++) {
	         j1 = ipivot[i];
	         k = res0 + i;
	         step[i] = d[j1] * ( w[k] / dst );
	      } // for (i = 1; i <= p; i++)
          arr = new double[p*(p+1)/2 + 1];
          for (m = 1; m <= p*(p+1)/2; m++) {
        	  arr[m] = w[rmat + m - 1];
          }
	      livmul(p, step, arr, step);
	      for (m = 1; m <= p; m++) {
	          step[m] = step[m] / Math.sqrt ( w[m] );
	      }
	      t = 1.0 / v2norm(p, step);
	      alphak = alphak + t * phi * t / rad;
	      lk = Math.max ( lk, alphak );
	      do110 = true;
	      continue loop1;
	 } // if (do320)
	//
	//  Restart.
	//
	 if (do370) {
	     do370 = false;

	      lk = w[lk0];
	      uk = w[uk0];

	      if (v[dst0] > 0.0 && v[dst0] - rad <= phimax) {
	        do20 = true;
	        continue loop1;
	      }

	      alphak = Math.abs ( v[stppar] );
	      dst = w[dstsav];
	      phi = dst - rad;
	      t = v[dgnorm] / rad;
	//
	//  Smaller radius.
	//
	      if ( rad <= v[rad0] ) {
	         uk = t;
	         if ( alphak <= 0.0 ) {
	           lk = 0.0;
	         }
	         if (v[dst0] > 0.0) lk = Math.max ( lk, (v[dst0]-rad)*w[phipin] );
	         if ( phi < 0.0 ) {
	           uk = Math.min ( uk, alphak );
	         }
	         do320 = true;
	         continue loop1;
	      }
	//
	//  Bigger radius.
	//
	      if (alphak <= 0.0 || uk > t) {
	        uk = t;
	      }

	      if (v[dst0] > 0.0) {
	        lk = Math.max ( lk, (v[dst0]-rad)*w[phipin] );
	      }
	      else {
	        lk = 0.0;
	      }

	      if ( phi < 0.0 ) {
	        uk = Math.min ( uk, alphak );
	      }
	      do320 = true;
	      continue loop1;
	 } // if (do370)
	  } // loop1: while (true)
	//
	//  Acceptable Gauss-Newton step.  Recover step from W.
	//
	 if (do410) {

	      alphak = 0.0;
	      for ( i = 1; i <= p; i++) {
	         j1 = ipivot[i];
	         step[j1] = -w[i];
	      }
	      do430 = true;
	 } // if (do410)
	// 
	//  Save values for use in a possible restart.
	//
	 if (do430) {

	  v[stppar] = alphak;
	  do440 = true;
	 } // if (do430)

	 if (do440) {

	  v[gtstep] = dotprd ( p, step, g );
	  v[preduc] = 0.5 * (alphak*dst*dst - v[gtstep]);
	  v[dstnrm] = dst;
	  w[dstsav] = dst;
	  w[lk0] = lk;
	  w[uk0] = uk;
	  v[rad0] = rad;

	  return;
	 } // if (do440)
	} // private void lmstep
	
	private void lsqrt ( int n1, int n, double l[], double a[], int irc[] ) {

	/***********************************************************************
	!
	!! LSQRT computes the Cholesky factor of a lower triangular matrix.
	!
	!  Discussion:
	!
	!    Compute rows N1 through N of the Cholesky factor L of
	!    A = L * L', where L and the lower triangle of A are both
	!    stored compactly by rows, and may occupy the same storage.
	!
	!    IRC = 0 means all went well.  IRC = J means the leading
	!    principal J x J submatrix of A is not positive definite,
	!    and L(J*(J+1)/2) contains the nonpositive reduced J-th diagonal.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer N1, N, the first and last rows to be computed.
	!
	!    Output, real L((N*(N+1))/2), contains rows N1 through N of the
	!    Cholesky factorization of A, stored compactly by rows as a lower 
	!    triangular matrix.
	!
	!    Input, real A((N*(N+1))/2), the matrix whose Cholesky factorization
	!    is desired.
	!
	!    Output, integer IRC, an error flag.  If IRC = 0, then the factorization
	!    was carried out successfully.  Otherwise, the principal J x J subminor
	!    of A was not positive definite.
	*/
	  
	  int i;
	  int i0;
	  int ij;
	  int ik;
	  int j;
	  int j0;
	  int jk;
	  int k;
	  double t;
	  double td;

	  i0 = ( n1 * ( n1 - 1 ) ) / 2;

	  for (i = n1; i <= n; i++) {

	    td = 0.0;
	    j0 = 0;

	    for ( j = 1; j <=  i - 1; j++) {

	      t = 0.0;

	      for (k = 1; k <= j - 1; k++) {
	        ik = i0 + k;
	        jk = j0 + k;
	        t = t + l[ik] * l[jk];
	      } // for (k = 1; k <= j - 1; k++)

	      ij = i0 + j;
	      j0 = j0 + j;
	      t = ( a[ij] - t ) / l[j0];
	      l[ij] = t;
	      td = td + t * t;

	    } // for (j = 1; j <= i-1; j++)

	    i0 = i0 + i;
	    t = a[i0] - td;

	    if ( t <= 0.0 ) {
	      l[i0] = t;
	      irc[0] = i;
	      return;
	    } // if (t <= 0.0)

	    l[i0] = Math.sqrt ( t );

	  } // for (i = n1; i <= n; i++)

	  irc[0] = 0;

	  return;
	} // private void lsqrt
	
	private double lsvmin ( int p, double l[], double x[], double y[] ) {

/***********************************************************************
!
!! LSVMIN estimates the smallest singular value of a lower triangular matrix.
!
!  Discussion:
!
!    This function returns a good over-estimate of the smallest
!    singular value of the packed lower triangular matrix L.
!
!    The matrix L is a lower triangular matrix, stored compactly by rows.
!
!    The algorithm is based on Cline, Moler, Stewart and Wilkinson, 
!    with the additional provision that LSVMIN = 0 is returned if the 
!    smallest diagonal element of L in magnitude is not more than the unit 
!    roundoff times the largest.  
!
!    The algorithm uses a random number generator proposed by Smith, 
!    which passes the spectral test with flying colors; see Hoaglin and
!    Knuth.
!
!  Modified:
!
!    04 April 2006
!
!  Author:
!
!    David Gay
!
!  Reference:
!
!    A Cline, Cleve Moler, Pete Stewart, James Wilkinson,
!    An Estimate of the Condition Number of a Matrix,
!    Report TM-310,
!    Applied Math Division,
!    Argonne National Laboratory, 1977.
!
!    D C Hoaglin,
!    Theoretical Properties of Congruential Random-Number Generators,
!    An Empirical View,
!    Memorandum NS-340,
!    Department of Statistics,
!    Harvard University, 1976.
!
!    D E Knuth,
!    The Art of Computer Programming,
!    Volume 2, Seminumerical Algorithms,
!    Addison Wesley, 1969.
!
!    C S Smith,
!    Multiplicative Pseudo-Random Number Generators with Prime Modulus, 
!    Journal of the Association for Computing Machinery,
!    Volume 19, pages 586-593, 1971.
!
!  Parameters:
!
!    Input, integer P, the order of L.
!
!    Input, real L((P*(P+1))/2), the elements of the lower triangular
!    matrix in row order, that is, L(1,1), L(2,1), L(2,2), L(3,1), L(3,2),
!    L(3,3), and so on.
!
!    Output, real X(P).  If LSVMIN returns a positive value, then X 
!    is a normalized approximate left singular vector corresponding to 
!    the smallest singular value.  This approximation may be very
!    crude.  If LSVMIN returns zero, then some components of X are zero 
!    and the rest retain their input values.
!
!    Output, real Y(P).  If LSVMIN returns a positive value, then 
!    Y = inverse ( L ) * X is an unnormalized approximate right singular 
!    vector corresponding to the smallest singular value.  This 
!    approximation may be crude.  If LSVMIN returns zero, then Y 
!    retains its input value.  The caller may pass the same vector for X
!    and Y, in which case Y overwrites X, for nonzero LSVMIN returns.
*/

  double b;
  int i;
  int ii;
  int j;
  int j0;
  int ji;
  int jj;
  double result;
  double psj;
  double sminus;
  double splus;
  double t;
  double xminus;
  double xplus;
  int k;
//
//  First check whether to return LSVMIN = 0 and initialize X.
//
  ii = 0;

  for (i = 1; i <= p; i++) {

    x[i] = 0.0;
    ii = ii + i;

    if ( l[ii] == 0.0 ) {
      result = 0.0;
      return result;
    }

  } // for (i = 1; i <= p; i++)

  if (( ix_lsvmin % 9973 ) == 0 ) {
    ix_lsvmin = 2;
  }
//
//  Solve L' * X = B, where the components of B have randomly
//  chosen magnitudes in ( 0.5, 1 ) with signs chosen to make X large.
//
  for ( j = p; j >= 1; j--) {
//
//  Determine X(J) in this iteration.  Note for I = 1, 2,..., J
//  that X(I) holds the current partial sum for row I.
//
    ix_lsvmin = ( 3432 * ix_lsvmin % 9973 );
    b = 0.5 * ( 1.0 + ((double) ( ix_lsvmin )) / 9973.0 );
    xplus = ( b - x[j] );
    xminus = ( -b - x[j] );
    splus = Math.abs ( xplus );
    sminus = Math.abs ( xminus );
    j0 = ( j * ( j - 1 ) ) / 2;
    jj = j0 + j;
    xplus = xplus / l[jj];
    xminus = xminus / l[jj];

    for ( i = 1; i <= j - 1; i++) {
      ji = j0 + i;
      splus = splus + Math.abs ( x[i] + l[ji] * xplus );
      sminus = sminus + Math.abs ( x[i] + l[ji] * xminus );
    }

    if ( splus < sminus ) {
      xplus = xminus;
    }

    x[j] = xplus;
//
//  Update partial sums.
//
    for ( i = 1; i <= j - 1; i++) {
      ji = j0 + i;
      x[i] = x[i] + l[ji] * xplus;
    }

  } // for (j = p; j >= 1; j--)
//
//  Normalize X.
//
  t = 1.0 / v2norm ( p, x );
  for (k = 1; k <= p; k++) {
      x[k] = t * x[k];
  }
//
//  Solve L * Y = X.
//  return SVMIN = 1 / twonorm ( Y ).
//
  for (j = 1; j <= p; j++) {

    psj = 0.0;
    j0 = ( j * ( j - 1 ) ) / 2;

    for ( i = 1; i <= j - 1; i++) {
      ji = j0 + i;
      psj = psj + l[ji] * y[i];
    }

    jj = j0 + j;
    y[j] = ( x[j] - psj ) / l[jj];

  }

  result = 1.0 / v2norm ( p, y );

  return result;
	} // private double lsvmin
	
	private void ltsqar ( int n, double a[], double l[] ) {
	
	/***********************************************************************
	!
	!! LTSQAR sets A to the lower triangle of L' * L.
	!
	!  Discussion:
	!
	!    L is an N by N lower triangular matrix, stored by rows.
	!
	!    A is also stored by rows, and may share storage with L.
	!
	!  Modified:
	!
	!    03 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer N, the order of L and A.
	!
	!    Output, real A((N*(N+1))/2), the lower triangle of L' * L,
	!    stored by rows.
	!
	!    Input, real L((N*(N+1))/2), the lower triangular matrix,
	!    stored by rows.
	*/
	  
	  int i;
	  int i1;
	  int ii;
	  int j;
	  int k;
	  int m;

	  ii = 0;

	  for ( i = 1; i <= n; i++) {

	    i1 = ii + 1;
	    ii = ii + i;
	    m = 1;

	    for (j = i1; j <= ii - 1; j++) {
	      for (k = i1; k <= j; k++) {
	        a[m] = a[m] + l[j] * l[k];
	        m = m + 1;
	      }
	    }

	    for ( j = i1; j <= ii; j++) {
	      a[j] = l[ii] * l[j];
	    }

	  } // for (i = 1; i <= n; i++)

	  return;
	} // private void ltsqar
	
	private void nl2itr (double d[], int iv[], double j[][], int n, int nn,
			             int p, double r[], double v[], double x[] ) {

	/***********************************************************************
	!
	!! NL2ITR carries out iterations for NL2SOL.
	!
	!  Discussion:
	!
	!    Parameters IV, N, P, V, and X are the same as the corresponding 
	!    ones to NL2SOL, except that V can be shorter, since the part of V 
	!    that NL2SOL uses for storing D, J, and R is not needed.  
	!
	!    Moreover, compared with NL2SOL, IV(1) may have the
	!    two additional output values 1 and 2, which are explained below,
	!    as is the use of IV(TOOBIG) and IV(NFGCAL).  The values IV(D),
	!    IV(J), and IV(R), which are output values from NL2SOL (and
	!    NL2SNO), are not referenced by NL2ITR or the subroutines it calls.
	!
	!    On a fresh start, that is, a call on NL2ITR with IV(1) = 0 or 12,
	!    NL2ITR assumes that R = R(X), the residual at X, and J = J(X),
	!    the corresponding jacobian matrix of R at X.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	! iv(1) = 1 means the caller should set r to r(x), the residual at x,
	!             and call nl2itr again, having changed none of the other
	!             parameters.  an exception occurs if r cannot be evaluated
	!             at x (e.g. if r would overflow), which may happen because
	!             of an oversized step.  in this case the caller should set
	!             iv(toobig) = iv(2) to 1, which will cause nl2itr to ig-
	!             nore r and try a smaller step.  the parameter nf that
	!             nl2sol passes to CALCR (for possible use by calcj) is a
	!             copy of iv(nfcall) = iv(6).
	! iv(1) = 2 means the caller should set j to j(x), the jacobian matrix
	!             of r at x, and call nl2itr again.  the caller may change
	!             d at this time, but should not change any of the other
	!             parameters.  the parameter nf that nl2sol passes to
	!             calcj is iv(nfgcal) = iv(7).  if j cannot be evaluated
	!             at x, then the caller may set iv(nfgcal) to 0, in which
	!             case nl2itr will return with iv(1) = 15.
	!
	!  Parameters:
	!
	!    Input, real D(p), the scale vector.
	!
	!    Input/output, integer IV(*), the NL2SOL integer parameter array.
	!
	!    j   n by p jacobian matrix (lead dimension nn).
	!
	!    n   number of observations (components in r).
	!
	!    nn  lead dimension of j.
	!
	!    p   number of parameters (components in x).
	!
	!    r   residual vector.
	!
	!    v   floating-point value array.
	!
	!    x   parameter vector.
	*/
	  
	  int dummy[] = new int[1];
	  int dig1;
	  double e;
	  int g01;
	  int g1;
	  int h0;
	  int h1;
	  int i;
	  int ipiv1;
	  int ipivi;
	  int ipivk;
	  int ipk;
	  int k = 0;
	  int km1;
	  int l = 0;
	  int lky1;
	  int lmat1;
	  int lstgst;
	  int m;
	  int pp1o2;
	  int qtr1;
	  int rd0;
	  int rd1;
	  int rdk;
	  double rdof1;
	  int rsave1;
	  int s1;
	  int smh;
	  int sstep;
	  int step1 = 0;
	  int stpmod = 0;
	  final int stppar = 5;
	  double sttsst;
	  double t;
	  double t1;
	  int temp1;
	  int temp2;
	  int w1;
	  int x01;
	
	//
	// iv subscript values
	//
	final int cnvcod=34;
	final int covmat=26;
	final int covprt=14;
	final int covreq=15;
	final int dig=43;
	final int dtype=16;
	final int g=28;
	final int h=44;
	final int ierr=32;
	final int inits=25;
	final int ipivot=61;
	final int ipiv0=60;
	final int irc=3;
	final int kagqt=35;
	final int kalm=36;
	final int lky=37;
	final int lmat=58;
	final int mode=38;
	final int model=5;
	final int mxfcal=17;
	final int mxiter=18;
	final int nfcall=6;
	final int nfgcal=7;
	final int nfcov=40;
	final int ngcov=41;
	final int ngcall=30;
	final int niter=31;
	final int qtr=49;
	final int radinc=8;
	final int rd=51;
	final int restor=9;
	final int rsave=52;
	final int s=53;
	final int step=55;
	final int stglim=11;
	final int stlstg=56;
	final int sused=57;
	final int switchConstant=12;
	final int toobig=2;
	final int w=59;
	final int xirc=13;
	final int x0=60;
	//
	// v subscript values.
	//
	final int cosmin=43;
	final int dgnorm=1;
	final int dinit=38;
	final int dstnrm=2;
	final int d0init=37;
	final int f=10;
	final int fdif=11;
	final int fuzz=45;
	final int f0=13;
	final int gtstep=4;
	final int incfac=23;
	final int jtinit=39;
	final int jtol1=87;
	final int lmax0=35;
	final int nvsave=9;
	final int phmxfc=21;
	final int preduc=7;
	final int radfac=16;
	final int radius=8;
	final int rad0=9;
	final int rlimit=42;
	final int size=47;
	final int tuner4=29;
	final int tuner5=30;
	final int vsave1=78;
	final int wscale=48;
	int ii;
	boolean do5 = false;
	boolean do10 = false;
	boolean do20 = false;
	boolean do30 = false;
	boolean do40 = false;
	boolean do50 = false;
	boolean do60 = false;
	boolean do150 = false;
	boolean do160 = false;
	boolean do170 = false;
	boolean do190 = false;
	boolean do195 = false;
	boolean do200 = false;
	boolean do205 = false;
	boolean do210 = false;
	boolean do310 = false;
	boolean do350 = false;
	boolean do360 = false;
	boolean do390 = false;
	boolean do400 = false;
	boolean do410 = false;
	boolean do420 = false;
	boolean do440 = false;
	boolean do450 = false;
	boolean do480 = false;
	boolean do510 = false;
	boolean do560 = false;
	boolean do570 = false;
	boolean do640 = false;
	boolean do700 = false;
	boolean do710 = false;
	boolean do730 = false;
	double arr[];
	double arr2[];
	double arr3[];
	double arr4[];
	double arr5[];
	double arr6[];
	double arr7[];
	int iarr[];
	int iarr2[];
	int iarr3[];

	      i = iv[1];
	      if (i == 1) {
	    	  do20 = true;
	      }
	      else if (i == 2) {
	    	  do50 = true;
	      } 
	      else {
	    	  do5 = true;
	      }
	//
	//  Check validity of iv and v input values.
	//
	//  If iv(1) = 0, then PARCHK calls dfault(iv, v).
	//
	if (do5) {
	      parchk ( iv, n, nn, p, v );
	      i = iv[1] - 2;

	      if ( 10 < i ) {
	        return;
	      }
	      
	      switch(i) {
	          case 1:
	          case 2:
	          case 3:
	          case 4:
	          case 5:
	          case 6:
	        	  do350 = true;
	        	  break;
	          case 7:
	          case 9:
	        	  do195 = true;
	        	  break;
	          case 8:
	        	  do160 = true;
	        	  break;
	          case 10:
	        	  do10 = true;
	      }
	} // if (do5)

	      
	//
	//  Initialization and storage allocation.
	//
	 if (do10) {

	      iv[niter] = 0;
	      iv[nfcall] = 1;
	      iv[ngcall] = 1;
	      iv[nfgcal] = 1;
	      iv[mode] = -1;
	      iv[stglim] = 2;
	      iv[toobig] = 0;
	      iv[cnvcod] = 0;
	      iv[covmat] = 0;
	      iv[nfcov] = 0;
	      iv[ngcov] = 0;
	      iv[kalm] = -1;
	      iv[radinc] = 0;
	      iv[s] = jtol1 + 2*p;
	      pp1o2 = p * (p + 1) / 2;
	      iv[x0] = iv[s] + pp1o2;
	      iv[step] = iv[x0] + p;
	      iv[stlstg] = iv[step] + p;
	      iv[dig] = iv[stlstg] + p;
	      iv[g] = iv[dig] + p;
	      iv[lky] = iv[g] + p;
	      iv[rd] = iv[lky] + p;
	      iv[rsave] = iv[rd] + p;
	      iv[qtr] = iv[rsave] + n;
	      iv[h] = iv[qtr] + n;
	      iv[w] = iv[h] + pp1o2;
	      iv[lmat] = iv[w] + 4*p + 7;
	//
	//  Length of w = p*(p+9)/2 + 7.  lmat is contained in w.
	//
	      if (v[dinit] >= 0.0 ) {
	    	for (ii = 1; ii <= p; ii++) {
	            d[ii] = v[dinit];
	    	}
	      }

	      if (v[jtinit] > 0.0 ) {
	    	for (ii = 0; ii < p; ii++) {
	            v[jtol1+ii] = v[jtinit];
	    	}
	      }

	      i = jtol1 + p;

	      if (v[d0init] > 0.0 ) {
	    	for (ii = 0; ii < p; ii++) {
	            v[i+ii] = v[d0init];
	    	}
	      }

	      v[rad0] = 0.0;
	      v[stppar] = 0.0;
	      v[radius] = v[lmax0] / ( 1.0 + v[phmxfc] );
	//
	//  Set initial model and S matrix.
	//
	      iv[model] = 1;
	      if (iv[inits] == 2) {
	    	  iv[model] = 2;
	      }
	      s1 = iv[s];
	      if (iv[inits] == 0) {
	    	for (ii = 0; ii < pp1o2; ii++) {
	            v[s1+ii] = 0.0;
	    	}
	      }
	      do20 = true;
	 } // if (do10)
	//
	//  Compute function value (half the sum of squares).
	//
	 if (do20) {
	      t = v2norm(n, r);

	      if ( v[rlimit] < t ) {
	        iv[toobig] = 1;
	      }

	      if ( iv[toobig] == 0 ) {
	        v[f] = 0.5 * t*t;
	      }
	      do30 = true;
	 } // if (do20)

	 if (do30) {
	      if ( iv[mode] == 0 ) {
	        do350 = true;
	      }
	      else if ( 0 < iv[mode] ) {
	        do730 = true;
	      }
	      else {
	    	  do40 = true;
	      }
	 } // if (do30)

	 if (do40) {
	      if ( iv[toobig] != 0 ) {
	         iv[1] = 13;
	         itsmry ( d, iv, p, v, x );
	         return;
	      }

	      do60 = true;
	 } // if (do40)
	 if (do50) {
	//
	//  Make sure jacobian could be computed.
	//

	      if ( iv[nfgcal] == 0 ) {
	         iv[1] = 15;
	         itsmry ( d, iv, p, v, x );
	         return;
	      }
	      do60 = true;
	 } // if (do50)
	if (do60) {
	//
	//  Compute gradient.
	//

	      iv[kalm] = -1;
	      g1 = iv[g];
	      arr = new double[n+1];
	      for (i = 1; i <= p; i++) {
	    	 for (ii = 1; ii <= n; ii++) {
	    		 arr[ii] = j[ii][i];
	    	 }
	         v[g1] = dotprd (n, r, arr );
	         g1 = g1 + 1;
	      } // for (i = 1; i <= p; i++)

	      if ( 0 < iv[mode] ) {
	        do710 = true;
	      }
	      else { // iv[mode] <= 0
	//
	//  Update D and make copies of R for possible use later.
	//
	      if ( 0 < iv[dtype] ) {
	        dupdat(d, iv, j, n, nn, p, v);
	      }

	      rsave1 = iv[rsave];
	      for (ii = 1; ii <= n; ii++) {
	          v[rsave1+ii-1] = r[ii];
	      }
	      qtr1 = iv[qtr];
	      for (ii = 1; ii <= n; ii++) {
	          v[qtr1+ii-1] = r[ii];
	      }
	//
	//  Compute inverse ( D ) * gradient.
	//
	      g1 = iv[g];
	      dig1 = iv[dig];
	      k = dig1;

	      for (i = 1; i <= p; i++) {
	         v[k] = v[g1] / d[i];
	         k = k + 1;
	         g1 = g1 + 1;
	      } // for (i = 1; i <= p; i++)

	      arr = new double[p+1];
	      for (ii = 1; ii <= p; ii++) {
	    	  arr[ii] = v[dig1+ii-1];
	      }
	      v[dgnorm] = v2norm(p, arr);

	      if (iv[cnvcod] != 0) {
	    	  do700 = true;
	      }
	      else if (iv[mode] == 0) {
	    	  do570 = true;
	      }
	      else {
	          iv[mode] = 0;
	          do150 = true;
	      }
	      } // else iv[mode] <= 0
	} // if (do60)
	//
	//  Main loop.
	//
	//  Print iteration summary, check iteration limit.
    //
	loop1: while ((!do700) && (!do710) && (!do730)) {
     if (do150) {
	     do150 = false;

	      itsmry(d, iv, p, v, x);
	      do160 = true;
     } // if (do150);
     if (do160) {
    	 do160 = false;
	     k = iv[niter];

	      if ( iv[mxiter] <= k ) {
	         iv[1] = 10;
	         itsmry ( d, iv, p, v, x );
	         return;
	      }
	      do170 = true;
     } // if (do160)

	if (do170) {
          do170 = false;
	      iv[niter] = k + 1;
	//
	//  Update radius.
	//
	      if ( k != 0 ) {

	        step1 = iv[step];
	        for (i = 1; i <= p; i++) {
	          v[step1] = d[i] * v[step1];
	          step1 = step1 + 1;
	        } // for (i = 1; i <= p; i++)
	        step1 = iv[step];
	        arr = new double[p+1];
	        for (ii = 1; ii <= p; ii++) {
	        	arr[ii] = v[step1+ii-1];
	        }
	        v[radius] = v[radfac] * v2norm(p, arr);

	      } // if (k != 0)
	//
	//  Initialize for start of next iteration.
	//
	      x01 = iv[x0];
	      v[f0] = v[f];
	      iv[kagqt] = -1;
	      iv[irc] = 4;
	      iv[h] = -Math.abs ( iv[h] );
	      iv[sused] = iv[model];
	//
	//  Copy X to X0.
	//
	      for (ii = 1; ii <= p; ii++) {
	          v[x01+ii-1] = x[ii];
	      }
	      do190 = true;
	} // if (do170)
	if (do190) {
		do190 = false;
	//
	//  Check STOPX and function evaluation limit.
	//
	      if ( ! stopx ( dummy ) ) {
	    	  do200 = true;
	      }
	      else {
	         iv[1] = 11;
	         do205 = true;
	      }
	} // if (do190)
	if (do195) {
		do195 = false;
	//
	//  Come here when restarting after function evaluation limit or STOPX.
	//

	      if ( v[f] < v[f0] ) {
	         v[radfac] = 1.0;
	         k = iv[niter];
	         do170 = true;
	         continue loop1;
	      }
	      else {
	    	  do200 = true;
	      }
	} // if (do195)

	 if (do200) {
         do200 = false;
	      if (iv[nfcall] < iv[mxfcal] + iv[nfcov]) {
	    	  do210 = true;
	      }
	      else {
	    	  do205 = true;
	    	  iv[1] = 9;
	      }
	 } // if (do200)
	 if (do205) {
         do205 = false;
	         if (v[f] >= v[f0]) {
	           itsmry ( d, iv, p, v, x );
	           return;
	         }
	//
	//  In case of STOPX or function evaluation limit with
	//  improved V(F), evaluate the gradient at X.
	//
	         iv[cnvcod] = iv[1];
	         do560 = true;
	 } // if (do205)
	 if (do210) {
		 do210 = false;
	//
	//  Compute candidate step.
	//

	      step1 = iv[step];
	      w1 = iv[w];
	//
	//  Compute Levenberg-Marquardt step.
	//
	      if (iv[model] != 2) {

	         qtr1 = iv[qtr];

	         if ( iv[kalm] < 0 ) {

	           rd1 = iv[rd];

	           if (-1 == iv[kalm]) {
	        	 arr = new double[p+1];
	        	 iarr = new int[p+1];
	        	 iarr2 = new int[1];
	        	 arr2 = new double[p+1];
	             qrfact ( nn, n, p, j, arr, iarr, iarr2, 0, arr2 );
	             for (ii = 1; ii <= p; ii++) {
	            	 v[rd1+ii-1] = arr[ii];
	            	 iv[ipivot+ii-1] = iarr[ii];
	             }
	             iv[ierr] = iarr2[0];
	           }

	           arr = new double[n+1];
	           for (ii = 1; ii <= n; ii++) {
	        	   arr[ii] = v[qtr1+ii-1];
	           }
	           qapply ( nn, n, p, j, arr, iv[ierr] );
               for (ii = 1; ii <= n; ii++) {
            	   v[qtr1+ii-1] = arr[ii];
               }
	         } // if (iv[kalm] < 0)

	         h1 = iv[h];
	//
	//  Copy R matrix to H.
	//
	         if ( h1 <= 0 ) {

	              h1 = -h1;
	              iv[h] = h1;
	              k = h1;
	              rd1 = iv[rd];
	              v[k] = v[rd1];

	              for (i = 2; i <= p; i++) {
	            	   for (ii = 0; ii < i-1; ii++) {
	            		   v[k+1+ii] = j[1+ii][i];
	            	   }
	                   k = k + i;
	                   rd1 = rd1 + 1;
	                   v[k] = v[rd1];
	              } // for (i = 2; i <= p; i++)
	         } // if (h1 <= 0)

	         g1 = iv[g];
	         arr = new double[p+1];
	         iarr = new int[1];
	         iarr2 = new int[p+1];
	         iarr3 = new int[1];
	         arr2 = new double[p+1];
	         arr3 = new double[p*(p+1)/2 + 1];
	         arr4 = new double[p+1];
	         arr5 = new double[p*(p+5)/2 + 5];
	         for (ii = 1; ii <= p; ii++) {
	        	 arr[ii] = v[g1+ii-1];
	        	 iarr2[ii] = iv[ipivot+ii-1];
	        	 arr2[ii] = v[qtr1+ii-1];
	         }
	         for (ii = 1; ii <= p*(p+1)/2; ii++) {
	        	 arr3[ii] = v[h1+ii-1];
	         }
	         for (ii = 1; ii <= p*(p+5)/2 + 4; ii++) {
	        	 arr5[ii] = v[w1+ii-1];
	         }
	         iarr[0] = iv[ierr];
	         iarr3[0] = iv[kalm];
	         lmstep(d, arr, iarr, iarr2, iarr3, p,
	                     arr2, arr3, arr4, v, arr5);
	         iv[ierr] = iarr[0];
	         for (ii = 1; ii <= p; ii++) {
	        	 iv[ipivot+ii-1] = iarr2[ii];
	        	 v[step1+ii-1] = arr4[ii];
	         }
	         iv[kalm] = iarr3[0];
	         for (ii = 1; ii <= p*(p+5)/2 + 4; ii++) {
	        	 v[w1+ii-1] = arr5[ii];
	         }
	      } // if (iv[model] != 2)
	//
	//  Compute Goldfeld-Quandt-Trotter step (augmented model).
	//
	      else { // iv[model] == 2

	      if ( iv[h] <= 0 ) {
	//
	//  Set H to inverse ( D ) * ( J' * J + s) ) * inverse ( D ).
	//
	        h1 = -iv[h];
	        iv[h] = h1;
	        s1 = iv[s];
	//
	//  J is in its original form.
    //
	        if ( iv[kalm] == -1 ) {

	            arr = new double[n+1];
	            arr2 = new double[n+1];
	        	for ( i = 1; i <= p; i++) {
	            t = 1.0 / d[i];
	            for (k = 1; k <= i; k++) {
	              for (ii = 1; ii <= n; ii++) {
	            	  arr[ii] = j[ii][i];
	            	  arr2[ii] = j[ii][k];
	              }
	              v[h1] = t * (dotprd(n,arr,arr2)+v[s1]) / d[k];
	              h1 = h1 + 1;
	              s1 = s1 + 1;
	            } // for (k = 1; k <= i; k++)
	        	} // for (i = 1; i <= p; i++)
	        } // if (iv[kalm] == -1)
	//
	//  LMSTEP has applied QRFACT to J.
	//
	        else { // iv[kalm] != -1

	          smh = s1 - h1;
	          h0 = h1 - 1;
	          ipiv1 = iv[ipivot];
	          t1 = 1.0 / d[ipiv1];
	          rd0 = iv[rd] - 1;
	          rdof1 = v[rd0 + 1];

	          for ( i = 1; i <= p; i++) {

	            l = ipiv0 + i;
	            ipivi = iv[l];
	            h1 = h0 + ( ipivi*(ipivi-1) ) / 2;
	            l = h1 + ipivi;
	            m = l + smh;
	//
	//  v(l) = h(ipivot(i), ipivot(i))
	//  v(m) = s(ipivot(i), ipivot(i))
	//
	            t = 1.0 / d[ipivi];
	            rdk = rd0 + i;
	            e = v[rdk]*v[rdk];
	            if ( 1 < i ) {
	              arr = new double[i];
	              for (ii = 1; ii < i; ii++) {
	            	  arr[ii] = j[ii][i];
	              }
	              e = e + dotprd(i-1, arr, arr);
	            }
	            v[l] = (e + v[m]) * t*t;

	            if ( i != 1 ) {

	              l = h1 + ipiv1;
	              if (ipivi < ipiv1) {
	                l = l + ((ipiv1-ipivi)*(ipiv1+ipivi-3)) / 2;
	              }
	              m = l + smh;
	//
	//  v(l) = h(ipivot(i), ipivot(1))
	//  v(m) = s(ipivot(i), ipivot(1))
	//
	              v[l] = t * (rdof1 * j[1][i]  +  v[m]) * t1;

	              for (k = 2; k <= i - 1; k++) {
	                ipk = ipiv0 + k;
	                ipivk = iv[ipk];
	                l = h1 + ipivk;
	                if (ipivi < ipivk) {
	                  l = l + ((ipivk-ipivi)*(ipivk+ipivi-3)) / 2;
	                }
	                m = l + smh;
	//
	//  v(l) = h(ipivot(i), ipivot(k))
	//  v(m) = s(ipivot(i), ipivot(k))
	//
	                km1 = k - 1;
	                rdk = rd0 + k;
	                arr = new double[k];
	                arr2 = new double[k];
	                for (ii = 1; ii <= km1; ii++) {
	                	arr[ii] = j[ii][i];
	                	arr2[ii] = j[ii][k];
	                }
	                v[l] = t * (dotprd(km1, arr, arr2) + v[rdk]*j[k][i] + v[m]) / d[ipivk];
	              } // for (i = 2; k <= i-1; k++)

	            } // if (i != 1)

	          } // for (i = 1; i <= p; i++)

	        } // else iv[kalm] != -1

	      } // if (iv[h] <= 0)
	//
	//  Compute actual Goldfeld-Quandt-Trotter step.
	//
	      h1 = iv[h];
	      dig1 = iv[dig];
	      lmat1 = iv[lmat];
	      arr = new double[p+1];
	      arr2 = new double[p*(p+1)/2 + 1];
	      iarr = new int[1];
	      arr3 = new double[p*(p+1)/2 + 1];
	      arr4 = new double[p+1];
	      arr5 = new double[4*p+8];
	      for (ii = 1; ii <= p; ii++) {
	    	  arr[ii] = v[dig1+ii-1];
	    	  arr4[ii] = v[step1+ii-1];
	      }
	      for (ii = 1; ii <= p*(p+1)/2; ii++) {
	    	  arr2[ii] = v[h1+ii-1];
	    	  arr3[ii] = v[lmat1+ii-1];
	      }
	      iarr[0] = iv[kagqt];
	      for (ii = 1; ii <= 4*p+7; ii++) {
	    	  arr5[ii] = v[w1+ii-1];
	      }
	      gqtstp(d, arr, arr2, iarr, arr3, p, arr4, v, arr5);
	      iv[kagqt] = iarr[0];
	      for (ii = 1; ii <= p*(p+1)/2; ii++) {
	    	 v[lmat1+ii-1] = arr3[ii]; 
	      }
	      for (ii = 1; ii <= p; ii++) {
	    	  v[step1+ii-1] = arr4[ii];
	      }
	      for (ii = 1; ii <= 4*p+7; ii++) {
	    	  v[w1+ii-1] = arr5[ii];
	      }
	      } // else if[model] == 2
	      do310 = true;
	 } // if (do210)
	if (do310) {
		do310 = false;
	//
	//  Compute R(X0 + STEP).
	//

	      if ( iv[irc] != 6 ) {
	        x01 = iv[x0];
	        step1 = iv[step];
	        for (ii = 1; ii <= p; ii++) {
	            x[ii] = v[step1+ii-1] + v[x01+ii-1];
	        }
	        iv[nfcall] = iv[nfcall] + 1;
	        iv[1] = 1;
	        iv[toobig] = 0;
	        return;
	      }
	      do350 = true;
	} // if (do310)
	if (do350) {
		do350 = false;
	//
	//  Assess candidate step.
	//

	      step1 = iv[step];
	      lstgst = iv[stlstg];
	      x01 = iv[x0];
	      arr = new double[p+1];
	      arr2 = new double[p+1];
	      arr3 = new double[p+1];
	      for (ii = 1; ii <= p; ii++) {
	    	  arr[ii] = v[step1+ii-1];
	    	  arr2[ii] = v[lstgst+ii-1];
	    	  arr3[ii] = v[x01+ii-1];
	      }
 	      assess(d, iv, p, arr, arr2, v, x, arr3);
 	      for (ii = 1; ii <= p; ii++) {
 	    	  v[step1+ii-1] = arr[ii];
 	    	  v[lstgst+ii-1] = arr2[ii];
 	      }
	//
	//  If necessary, switch models and/or restore R.
	//
	      if ( iv[switchConstant] != 0 ) {
	        iv[h] = -Math.abs ( iv[h] );
	        iv[sused] = iv[sused] + 2;
	        for (ii = 1; ii <= nvsave; ii++) {
	            v[ii] = v[vsave1+ii-1];
	        }
	      } // if (iv[switchConstant] != 0)
	      do360 = true;
	} // if (do350)

	 if (do360) {
          do360 = false;
	      if ( iv[restor] != 0 ) {
	         rsave1 = iv[rsave];
	         for (ii = 1; ii <= n; ii++) {
	             r[ii] = v[rsave1+ii-1];
	         }
	      } // if (iv[restor] != 0)
	      do390 = true;
	 } // if (do360)

	 if (do390) {
         do390 = false;
	      l = iv[irc] - 4;
	      stpmod = iv[model];

	      if (l > 0) {
	    	  switch(l) {
	    	  case 1:
	    		  do410 = true;
	    		  break;
	    	  case 2:
	    		  do440 = true;
	    		  break;
	    	  case 3:
	    	  case 4:
	    	  case 5:
	    	  case 6:
	    	  case 7:
	    	  case 8:
	    		  do450 = true;
	    		  break;
	    	  case 9:
	    		  do640 = true;
	    		  break loop1;
	    	  case 10:
	    		  do570 = true;
	    	  }
	      } // if (l > 0)
	      else { // else l <= 0
	//
	//  Decide whether to change models.
	//
	      e = v[preduc] - v[fdif];
	      sstep = iv[lky];
	      s1 = iv[s];
	      arr = new double[p+1];
	      arr2 = new double[p*(p+1)/2 + 1];
	      arr3 = new double[p+1];
	      for (ii = 1; ii <= p*(p+1)/2; ii++) {
	    	  arr2[ii] = v[s1+ii-1];
	      }
	      for (ii = 1; ii <= p; ii++) {
	    	  arr3[ii] = v[step1 + ii - 1];
	      }
	      slvmul ( p, arr, arr2, arr3 );
	      for (ii = 1; ii <= p; ii++) {
	    	  v[sstep+ii-1] = arr[ii];
	      }
	      sttsst = 0.5 * dotprd(p, arr3, arr);

	      if ( iv[model] == 1 ) {
	        sttsst = -sttsst;
	      }
	//
	//  Switch models.
	//
	      if (Math.abs(e + sttsst) * v[fuzz] >= Math.abs(e)) {
	    	  do400 = true;
	      }
	      else {
	         iv[model] = 3 - iv[model];
	         if (iv[model] == 1) {
	        	 iv[kagqt] = -1;
	         }
	         if (iv[model] == 2 && iv[kalm] > 0) {
	        	 iv[kalm] = 0;
	         }
	         if (-2 < l) {
	        	 do480 = true;
	         }
	         else {
	              iv[h] = -Math.abs ( iv[h] );
	              iv[sused] = iv[sused] + 2;
	              for (ii = 1; ii <= nvsave; ii++) {
	                  v[vsave1+ii-1] = v[ii];
	              }
	              do420 = true;
	         } // else
	      } // else
	      } // else if l <= 0
	 } // if (do390)
     if (do400) {
    	 do400 = false;
	     if (-3 < l) {
	    	 do480 = true;
	     }
	     else {
	//
	//  Recompute STEP with decreased radius.
	//
	         v[radius] = v[radfac] * v[dstnrm];
	         do190 = true;
	         continue loop1;
	     } // else
     } // if (do400)
     if (do410) {
    	 do410 = false;
	//
	//  Recompute STEP, saving V values and R if necessary.
	//

	      v[radius] = v[radfac] * v[dstnrm];
	      do420 = true;
     } // if (do410)

	 if (do420) {
         do420 = false;
	      if ( v[f] < v[f0] ) {
	        rsave1 = iv[rsave];
	        for (ii = 1; ii <= n; ii++) {
	            v[rsave1+ii-1] = r[ii];
	        }
	      }

	      do190 = true;
	      continue loop1;
	 } // if (do420
	 if (do440) {
		 do440 = false;
	//
	//  Compute step of length V(LMAX0) for singular convergence test.
	//

	      v[radius] = v[lmax0];
	      do210 = true;
	      continue loop1;
	 } // if (do440)
	 if (do450) {
		 do450 = false;
	//
	//  Convergence or false convergence.
	//

	      iv[cnvcod] = l;
	      if (v[f] >= v[f0]) {
	    	  do700 = true;
	    	  break loop1;
	      }
	      else if (iv[xirc] == 14) {
	    	  do700 = true;
	    	  break loop1;
	      }
	      else {
	          iv[xirc] = 14;
	          do480 = true;
	      }
	 } // if (do450)
	 if (do480) {
		 do480 = false;
	//
	//  Process acceptable step.
	//
	     iv[covmat] = 0;
	//
	//  Set LKY = J(X0)' * R(X).
	//
	      lky1 = iv[lky];
	//
	//  Jacobian has not been modified.
	//
	      if ( iv[kalm] < 0 ) {

	         for (i = 1; i <= p; i++) {
	           arr = new double[n+1];
	           for (ii = 1; ii <= n; ii++) {
	        	   arr[ii] = j[ii][i];
	           }
	           v[lky1] = dotprd(n, arr, r);
	           lky1 = lky1 + 1;
	         } // for (i = 1; i <= p; i++)
	      } // if (iv[kalm] < 0)
	//
	//  QRFACT has been applied to J.  Store copy of R in QTR and
	//  apply Q to it.
	//
	      else {

	        qtr1 = iv[qtr];
	        for (ii = 1; ii <= n; ii++) {
	            v[qtr1+ii-1] = r[ii];
	        }
	        arr = new double[n+1];
	        for (ii = 1; ii <= n; ii++) {
	        	arr[ii] = v[qtr1+ii-1];
	        }
	        qapply(nn, n, p, j, arr, iv[ierr]);
	        for (ii = 1; ii <= n; ii++) {
	        	v[qtr1+ii-1] = arr[ii];
	        }
	//
	//  Multiply top P-vector in QTR by permuted upper triangle
	//  stored by QRFACT in J and RD.
	//
	        rd1 = iv[rd];
	        temp1 = iv[stlstg];
	        iarr = new int[p+1];
	        arr = new double[p+1];
	        arr2 = new double[p+1];
	        arr3 = new double[p+1];
	        arr4 = new double[p+1];
	        for (ii = 1; ii <= p; ii++) {
	        	iarr[ii] = iv[ipivot+ii-1];
	        	arr[ii] = v[rd1+ii-1];
	        	arr2[ii] = v[qtr1+ii-1];
	        }
	        rptmul(3, iarr, j, nn, p, arr, arr2, arr3, arr4);
	        for (ii = 1; ii <= p; ii++) {
	        	v[lky1+ii-1] = arr3[ii];
	        }

	      } // else
	      do510 = true;
	 } // if (do480)
	 if (do510) {
		 do510 = false;
	//
	//  See whether to set V(RADFAC) by gradient tests.
	//

	      if (iv[irc] == 3 ) {

	        step1 = iv[step];
	        temp1 = iv[stlstg];
	        temp2 = iv[x0];
	//
	//  Set TEMP1 = hessian * STEP for use in gradient tests
	//
	//  STEP computed using Gauss-Newton model.
	//  QRFACT has been applied to J.
	//
	        if ( stpmod != 2 ) {

	          rd1 = iv[rd];
	          iarr = new int[p+1];
	          arr = new double[p+1];
	          arr2 = new double[p+1];
	          arr3 = new double[p+1];
	          arr4 = new double[p+1];
	          for (ii = 1; ii <= p; ii++) {
	        	  iarr[ii] = iv[ipivot+ii-1];
	        	  arr[ii] = v[rd1+ii-1];
	        	  arr2[ii] = v[step1+ii-1];
	          }
	          rptmul(2, iarr, j, nn, p, arr, arr2, arr3, arr4);
	          for (ii = 1; ii <= p; ii++) {
	        	  v[temp1+ii-1] = arr3[ii];
	          }
	        } // if (stpmod != 2)
	//
	//  STEP computed using augmented model.
	//
	        else { // stpmod == 2

	          h1 = iv[h];
	          k = temp2;

	          for  (i = 1; i <= p; i++) {
	            v[k] = d[i] * v[step1];
	            k = k + 1;
	            step1 = step1 + 1;
	          } // for (i = 1; i <= p; i++)

	          arr = new double[p+1];
	          arr2 = new double[p*(p+1)/2 + 1];
	          arr3 = new double[p+1];
	          for (ii = 1; ii <= p*(p+1)/2; ii++) {
	        	  arr2[ii] = v[h1+ii-1];
	          }
	          for (ii = 1; ii <= p; ii++) {
	        	  arr3[ii] = v[temp2+ii-1];
	          }
	          slvmul(p, arr, arr2, arr3);
	          for (ii = 1; ii <= p; ii++) {
	        	  v[temp1+ii-1] = arr[ii];
	          }

	          for (i = 1; i <= p; i++) {
	            v[temp1] = d[i] * v[temp1];
	            temp1 = temp1 + 1;
	          } // for (i = 1; i <= p; i++)

	        } // else stpmod == 2

	      } // if (iv[irc] == 3)
	      do560 = true;
	 } // if (do510)
	 if (do560) {
		 do560 = false;
	//
	//  Save old gradient and compute new one.
	//

	      iv[ngcall] = iv[ngcall] + 1;
	      g1 = iv[g];
	      g01 = iv[w];
	      for (ii = 0; ii < p; ii++) {
	          v[g01+ii] = v[g1+ii];
	      }
	      iv[1] = 2;
	      return;
	 } // if (do560)
	 if (do570) {
		 do570 = false;
	//
	//  Initializations -- g0 = g - g0, etc.
	//

	      g01 = iv[w];
	      g1 = iv[g];
	      for (ii = 0; ii < p; ii++) {
	          v[g01+ii] = - v[g01+ii] + v[g1+ii];
	      }
	      step1 = iv[step];
	      temp1 = iv[stlstg];
	      temp2 = iv[x0];
	//
	//  Set V(RADFAC) by gradient tests.
	//
	//  Set TEMP1 = d**-1 * (hessian * STEP  +  ( G(x0) - G(x) ) ).
	//
	      if ( iv[irc] == 3 ) {

	         k = temp1;
	         l = g01;
	         for (i = 1; i <= p; i++) {
	           v[k] = (v[k] - v[l]) / d[i];
	           k = k + 1;
	           l = l + 1;
	         } // for (i = 1; i <= p; i++)
	//
	//  Do gradient tests.
	//
	         arr = new double[p+1];
	         arr2 = new double[p+1];
	         arr3 = new double[p+1];
	         for (ii = 1; ii <= p; ii++) {
	        	 arr[ii] = v[temp1+ii-1];
	        	 arr2[ii] = v[g1+ii-1];
	        	 arr3[ii] = v[step1+ii-1];
	         }
	         if ( v2norm(p, arr) <= v[dgnorm] * v[tuner4] ||
	           dotprd(p, arr2, arr3) < v[gtstep] * v[tuner5] ) {
	           v[radfac] = v[incfac];
	         }

	      } // if (iv[irc] == 3)
	//
	//  Finish computing LKY = ( J(X) - J(X0) )' * R.
	//
	//  Currently LKY = J(X0)' * R.
	//
	      lky1 = iv[lky];
	      for (ii = 0; ii < p; ii++) {
	          v[lky1+ii] = - v[lky1+ii] + v[g1+ii];
	      }
	//
	//  Determine sizing factor V(SIZE).
	//
	//  Set TEMP1 = S * STEP.
	//
	      s1 = iv[s];
	      arr = new double[p+1];
	      arr2 = new double[p*(p+1)/2 + 1];
	      arr3 = new double[p+1];
	      for (ii = 1; ii <= p*(p+1)/2; ii++) {
	    	  arr2[ii] = v[s1+ii-1];
	      }
	      for (ii = 1; ii <= p; ii++) {
	    	  arr3[ii] = v[step1+ii-1];
	      }
	      slvmul(p, arr, arr2, arr3);
	      for (ii = 1; ii <= p; ii++) {
	    	  v[temp1+ii-1] = arr[ii];
	      }

	      t1 = Math.abs(dotprd(p, arr3, arr));
	      for (ii = 1; ii <= p; ii++) {
	    	  arr[ii] = v[lky1+ii-1];
	      }
	      t = Math.abs(dotprd(p, arr3, arr));
	      v[size] = 1.0;

	      if ( t < t1 ) {
	        v[size] = t / t1;
	      }
	//
	//  Update S.
	//
	      arr = new double[p*(p+1)/2 + 1];
	      for (ii = 1;  ii <= p*(p+1)/2; ii++) {
	    	  arr[ii] = v[s1+ii-1];
	      }
	      arr2 = new double[p+1];
	      arr3 = new double[p+1];
	      arr4 = new double[p+1];
	      arr5 = new double[p+1];
	      arr6 = new double[1];
	      arr7 = new double[p+1];
	      for (ii = 1; ii <= p; ii++) {
	    	  arr2[ii] = v[step1 + ii - 1];
	    	  arr3[ii] = v[temp1 + ii - 1];
	    	  arr4[ii] = v[temp2 + ii - 1];
	    	  arr5[ii] = v[g01 +ii - 1];
	    	  arr7[ii] = v[lky1+ii-1];
	      }
	      arr6[0] = v[wscale];
	      slupdt(arr, v[cosmin], p, v[size], arr2, arr3,
	             arr4, arr5, arr6, arr7);
	      for (ii = 1; ii <= p*(p+1)/2; ii++) {
	    	  v[s1+ii-1] = arr[ii];
	      }
	      for (ii = 1; ii <= p; ii++) {
	    	  v[step1+ii-1] = arr2[ii];
	    	  v[temp1+ii-1] = arr3[ii];
	    	  v[temp2+ii-1] = arr4[ii];
	      }
	      v[wscale] = arr6[0];
	      iv[1] = 2;
	      do150 = true;
	 } // if (do570)
	} // loop1: while ((!do700) && (!do710) && (!do730))
	//
	//  Bad parameters to ASSESS.
	//
	if (do640) {
	      iv[1] = 14;
	      itsmry ( d, iv, p, v, x );
	      return;
	} // if (do640)
	if (do700) {
	//
	//  Convergence obtained.  Compute covariance matrix if desired.
	//

	      if ( ( iv[covreq] == 0 && iv[covprt] == 0 ) || 
	        iv[covmat] != 0 || iv[cnvcod] >= 7 ) {
	            iv[1] = iv[cnvcod];
	            iv[cnvcod] = 0;
	            itsmry(d, iv, p, v, x);
	            return;
	      } // if

	      iv[mode] = 0;
	      do710 = true;
	} // if (do700)
     loop2: while (true) {
    	 if (do710) {
    		 do710 = false;
          iarr = new int[1];
          iarr[0] = i;
	      covclc(iarr, d, iv, j, n, nn, p, r, v, x);
	      i = iarr[0];

	      if ( i == 3 ) {

	        iv[ngcov] = iv[ngcov] + 1;
	        iv[ngcall] = iv[ngcall] + 1;
	        iv[1] = 2;

	      } // if (i == 3);
	      else if ( i == 4 ) {

	        if ( iv[niter] == 0 ) {
	          iv[mode] = -1;
	        }
	        else {
	          iv[mode] = 0;
	        }

	        iv[1] = iv[cnvcod];
	        iv[cnvcod] = 0;

	        itsmry(d, iv, p, v, x);
	      } // else if (i == 4) {
	      else { 

	        iv[nfcov] = iv[nfcov] + 1;
	        iv[nfcall] = iv[nfcall] + 1;
	        iv[restor] = i;
	        iv[1] = 1;

	      } // else

	      return;
    	 } // if (do710)
         if (do730) {
	        do730 = false;

	         if ( iv[restor] == 1 || iv[toobig] != 0 ) {
	          do710 = true;
	         }
	         else {
	        	 break loop2;
	         }
         } // if (do730)
     } // loop2: while (true)

	  iv[nfgcal] = iv[nfcall];
	  iv[ngcov] = iv[ngcov] + 1;
	  iv[ngcall] = iv[ngcall] + 1;
	  iv[1] = 2;

	  return;
	} // private void nl2itr
	
	private void parchk (int iv[], int n, int nn, int p, double v[] ) {

	/***********************************************************************
	!
	!! PARCHK checks the NL2SOL parameters.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	*/

	  int i;
	  int iv1;
	  int jtolp;
	  int k;
	  int l;
	  int m;
	  double machep;
	  final int nvdflt = 27;
	  final int parsv1 = 51;
	  final int prunit = 21;
	  int pu;
	  double vk;
	  double vm[] = new double[]{
			   0.0, 1.0e-3,-0.99, 1.0e-3, 1.0e-2,
	           1.2, 1.0e-2, 1.2, 0.0,
	           0.0, 1.0e-3, -1.0, 0.0, 0.0, 0.0, 0.0,
	           0.0, 0.0, 0.0, 0.0, -10.0, 0.0,
	           0.0, 0.0, 1.0e+10, 0.0, 0.0, 1.01};
	  String vn[] = new String[]{
			     "        ","epslon..", "phmnfc..", "phmxfc..", "decfac..", "incfac..",
			    "rdfcmn..", "rdfcmx..", "tuner1..", "tuner2..", "tuner3..", 
			    "tuner4..", "tuner5..", "afctol..", "rfctol..", "xctol...",
			    "xftol...", "lmax0...", "dltfdj..", "d0init..", "dinit...",
			    "jtinit..", "dltfdc..", "dfac....", "rlimit..", "cosmin..",
			    "delta0..", "fuzz...."};
	  double vx[] = new double[]{
			   0.0, 0.9, -1.0e-3, 1.0e+1, 0.8, 
	           1.0e+2, 0.8, 1.0e+2, 0.5, 
	           0.5, 1.0, 1.0, 0.0, 0.0, 0.1,
	           1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 
	           1.0, 0.0, 1.0, 1.0, 1.0e+2};

	  String cngd[] = new String[]{"    ","---c","hang","ed v"};
	  String dflt[] = new String[]{"    ","nond","efau","lt v"};
	  String which[] = new String[4];
	//
	// iv and v subscripts
	//
	      

	     final int dtype=16;
	     final int dtype0=29;
	     final int d0init=37;
	     final int epslon=19;
	     final int inits=25;
	     final int jtinit=39;
	     final int jtol0=86;
	     final int jtol1=87;
	     final int oldn=45;
	     final int oldnn=46;
	     final int oldp=47;
	     final int parprt=20;
	     double arr[];
	     int iarr[];
         int j;
	     

	  if ( iv[1] == 0 ) {
	    dfault ( iv, v );
	  }

	  pu = iv[prunit];
	  iv1 = iv[1];

	  if ( iv1 == 12) {

	    if ( nn < n || n < p || p < 1) {
	      iv[1] = 16;
	      if ( pu != 0 ) {
	        Preferences.debug("Bad nn, n or p: nn = " +  nn + " n = " + n + " p = " + p + "\n", Preferences.DEBUG_ALGORITHM);
	      }
	      return;
	    }

	    k = iv[21];
	    iarr = new int[26];
	    arr = new double[46];
	    for (j = 1; j <= 25; j++) {
	    	iarr[j] = iv[20 + j];
	    }
	    for (j = 1; j <= 45; j++) {
	    	arr[j] = v[32 + j];
	    }
	    dfault(iarr, arr);
	    for (j = 1; j <= 25; j++) {
	    	iv[20 + j] = iarr[j];
	    }
	    for (j = 1; j <= 45; j++) {
	    	v[32 + j] = arr[j];
	    }
	    iv[21] = k;
	    iv[dtype0] = iv[dtype+20];
	    iv[oldn] = n;
	    iv[oldnn] = nn;
	    iv[oldp] = p;
	    which[1] = dflt[1];
	    which[2] = dflt[2];
	    which[3] = dflt[3];

	  } // if (iv1 == 12)
	    else { // iv1 != 12

	    if ( n  != iv[oldn]  || nn != iv[oldnn] || p  != iv[oldp] ) {

	      iv[1] = 17;

	      if ( pu != 0 ) {
	        Preferences.debug("(NN,N,P) changed from:\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("NN = " +  iv[oldnn] + " N = " +  iv[oldn] + " P = " + iv[oldp] + "\n", 
	        		Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("to:\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("NN = " +  nn + " N = " +  n + " P = " +  p + "\n", Preferences.DEBUG_ALGORITHM);
	      } // if (pu != 0)
	      return;

	    } // if ( n  != iv[oldn]  || nn != iv(oldnn) || p  != iv[oldp] )

	    if ( iv1 < 1 || 11 < iv1 ) {
	      iv[1] = 50;
	      if (pu != 0) {
	        Preferences.debug("iv1 = " + iv1 + " should be >=1 and <= 11\n", Preferences.DEBUG_ALGORITHM);
	      } // if (pu != 0)
	      return;
	    } // if ( iv1 < 1 || 11 < iv1 )

	    which[1] = cngd[1];
	    which[2] = cngd[2];
	    which[3] = cngd[3];

	    } // else iv1 != 12

	    teensy_parchk = tiny;
	    machep = epsilon;
	    big_parchk = huge;
	    vm[12] = machep;
	    vx[12] = big_parchk;
	    vm[13] = teensy_parchk;
	    vx[13] = big_parchk;
	    vm[14] = machep;
	    vm[17] = teensy_parchk;
	    vx[17] = big_parchk;
	    vm[18] = machep;
	    vx[19] = big_parchk;
	    vx[20] = big_parchk;
	    vx[21] = big_parchk;
	    vm[22] = machep;
	    vx[24] = Math.sqrt ( 0.999E+00 * huge );
	    vm[25] = machep;
	    vm[26] = machep;

	  m = 0;

	  if (iv[inits] < 0 || iv[inits] > 2) {
	         m = 18;
	         if (pu != 0) {
	         Preferences.debug("iv[inits] = " + iv[inits] + " should be >= 0 and <= 2\n", Preferences.DEBUG_ALGORITHM);
	         }
	  } // if (iv[inits] < 0 || iv[inits] > 2)
	  
	  k = epslon;

	  for ( i = 1; i <= nvdflt; i++) {
	    vk = v[k];
	    if (vk < vm[i] || vk > vx[i]) {
	      m = k;
	      if (pu != 0) {
	    	  Preferences.debug(vn[i] + ".. v[" + k + "] = " + vk + " should be between " + vm[i] +
	    			  " and " + vx[i] + "\n", Preferences.DEBUG_ALGORITHM);
	      }
	    } // if (vk < vm[i] || vk > vx[i])
	    k = k + 1;

	  } // for ( i = 1; i <= nvdflt; i++)
	//
	//  Check JTOL values.
	//
	  if ( iv1 != 12 ||  v[jtinit] <= 0.0) {

	    jtolp = jtol0 + p;
	    for (i = jtol1; i <= jtolp; i++) {
	      if ( v[i] <= 0.0 ) {
	        k = i - jtol0;
	        if (pu != 0)  {
	        	Preferences.debug("jtol[" + k + "] = v[" + i + "] = " + v[i] +
	        			" should be positive\n", Preferences.DEBUG_ALGORITHM);
	        }
	        m = i;
	      } // if ( v[i] <= 0.0 )
	    } // for (i = jtol1; i <= jtolp; i++)

	  } // if ( iv1 != 12 ||  v[jtinit] <= 0.0)

	  if ( m != 0 ) {
	    iv[1] = m;
	    return;
	  } // if (m != 0)

	  if ( pu == 0 || iv[parprt] == 0 ) {
	    return;
	  }

	  if ( iv1 == 12 && iv[inits] != 0) {
	    m = 1;
	    Preferences.debug("nondefault values....inits..... iv(25) = " + iv[inits] + "\n", Preferences.DEBUG_ALGORITHM);
	  }

	  if ( iv[dtype] != iv[dtype0] ) {
	    if (m == 0) {
	    	Preferences.debug(which[1]+which[2]+which[3]+"alues....\n", Preferences.DEBUG_ALGORITHM);
	    }
	    m = 1;
	    Preferences.debug("DTYPE..... IV(16) = " +  iv[dtype] + "\n", Preferences.DEBUG_ALGORITHM);
	  }

	  k = epslon;
	  l = parsv1;

	  for ( i = 1; i <= nvdflt; i++) {

	    if ( v[k] != v[l] ) {
	      if (m == 0){
	    	  Preferences.debug(which[1]+which[2]+which[3]+"alues....\n", Preferences.DEBUG_ALGORITHM);	 
	      }
	      m = 1;
	      Preferences.debug(vn[i] + "..v[" + k + "] = " + v[k] + "\n", Preferences.DEBUG_ALGORITHM);
	    } // if ( v[k] != v[l] )

	    k = k + 1;
	    l = l + 1;

	  } // for ( i = 1; i <= nvdflt; i++)

	  iv[dtype0] = iv[dtype];
	  for (j = 0; j < nvdflt; j++) {
	      v[parsv1+j] = v[epslon+j];
	  }

	  if ( iv1 != 12 ) {
	    return;
	  }

	  if ( v[jtinit] <= 0.0 ) {
	    Preferences.debug("(Initial) JTOL array\n", Preferences.DEBUG_ALGORITHM);
	    for (j = jtol1; j <= jtol0+p; j++) {
	        Preferences.debug(v[j] + "\n", Preferences.DEBUG_ALGORITHM);
	    }
	  }

	  if ( v[d0init] <= 0.0 ) {
	    k = jtol1 + p;
	    Preferences.debug("(Initial) D0 array\n", Preferences.DEBUG_ALGORITHM);
	    for (j = 0; j <= p-1; j++) {
	        Preferences.debug(v[k+j] + "\n", Preferences.DEBUG_ALGORITHM);
	    }
	  }

	  return;
	} // private void parchk
	
	private void qapply ( int nn, int n, int p, double j[][], double r[], int ierr ) {

	/***********************************************************************
	!
	!! QAPPLY applies orthogonal transformation to the residual R.
	!
	!  Discussion:
	!
	!    This subroutine applies to R the orthogonal transformations
	!    stored in J by QRFACT.
	!
	!    The vectors U which determine the Householder transformations
	!    are normalized so that their 2-norm squared is 2.  The use of
	!    these transformations here is in the spirit of Businger and Golub.
	!
	!  Modified:
	!
	!    06 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Reference:
	!
	!    P A Businger and Gene Golub,
	!    Linear Least Squares Solutions by Householder Transformations, 
	!    Numerische Mathematik, 
	!    Volume 7, pages 269-276, 1965.
	!
	!  Parameters:
	!
	!    Input, integer NN, the row dimension of the matrix J as declared in
	!    the calling program dimension statement
	!
	!    Input, integer N, the number of rows of J and the size of the R.
	!
	!    Input, integer P, the number of columns of J and the size of SIGMA.
	!
	!    Input, real J(NN,P), an N by P matrix.  It contains on its diagonal
	!    and below its diagonal the column vectors U which determine the 
	!    Householder transformations (identity - U*U').
	!
	!    Input/output, real R(N).  On input, the right hand side vector to 
	!    which the orthogonal transformations will be applied.  On output,
	!    R has been transformed.
	!
	!    Input, integer IERR, if non-zero, indicates that not all the 
	!    transformations were successfully determined and only the first
	!    abs(IERR) - 1 transformations will be used.
	*/
	  
	  int k;
	  int l;
	  int nl1;
	  double t;
	  double arr[];
	  double arr2[];
	  int m;

	  if ( ierr != 0 ) {
	    k = Math.abs(ierr) - 1;
	  }
	  else {
	    k = p;
	  }

	  for (l = 1; l <= k; l++) {

	    nl1 = n - l + 1;
	    arr = new double[nl1+1];
	    arr2 = new double[nl1+1];
	    for (m = 1; m <= nl1; m++) {
	    	arr[m] = j[l+m-1][l];
	    	arr2[m] = r[l+m-1];
	    }
	    t = -dotprd ( nl1, arr, arr2 );
	    for (m = l; m <= n; m++) {
	        r[m] = r[m] + t * j[m][l];
	    }

	  } // for (l = 1; l <= k; l++)

	  return;
	} // private void qapply
	
	private void qrfact ( int nm, int m, int n, double qr[][], double alpha[], int ipivot[],
			              int ierr[], int nopivk, double sum[] ) {

	/***********************************************************************
	!
	!! QRFACT computes the QR decomposition of a matrix.
	!
	!  Discussion:
	!
	!    This subroutine does a QR decomposition on the M x N matrix QR,
	!    with an optionally modified column pivoting, and returns the
	!    upper triangular R matrix, as well as the orthogonal vectors
	!    used in the transformations.
	!
	!    This may be used when solving linear least-squares problems.
	!    See subroutine QR1 of ROSEPACK.  It is called for this purpose
	!    in the NL2SOL package.
	!
	!    This version of QRFACT tries to eliminate the occurrence of
	!    underflows during the accumulation of inner products.  RKTOL1
	!    is chosen below so as to insure that discarded terms have no
	!    effect on the computed two-norms.
	!
	!    This routine was adapted from Businger and Golub's ALGOL 
	!    routine "SOLVE".
	!
	!    This routine is equivalent to the subroutine QR1 of ROSEPACK 
	!    with RKTOL1 used in place of RKTOL below, with V2NORM used 
	!    to initialize (and sometimes update) the sum array, and 
	!    with calls on DOTPRD in place of some loops.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Reference:
	!
	!    P Businger and Gene Golub,
	!    Linear Least Squares Solutions by Householder Transformations,
	!    in Handbook for Automatic Computation,
	!    Volume II, Linear Algebra,
	!    edited by James Wilkinson and C Reinsch,
	!    Springer Verlag, pages 111-118, 1971;
	!    prepublished in Numerische Mathematik,
	!    Volume 7, pages 269-276, 1965.
	!
	!  Parameters:
	!
	!    Input, integer NM, the row dimension of the two dimensional
	!    array parameters as declared in the calling program dimension statement.
	!
	!    Input, integer M, the number of rows in the matrix.
	!
	!    Input, integer N, the number of columns in the matrix.
	!
	!    Input/output, real QR(NM,N), on input, the M by N rectangular matrix
	!    to be decomposed.  On output, contains the non-diagonal elements of 
	!    the R matrix in the strict upper triangle.  The vectors U, which
	!    define the Householder transformations (Identity - U*U'), are in the 
	!    columns of the lower triangle.  These vectors U are scaled so that 
	!    the square of their 2-norm is 2.0.
	!
	!    Output, real ALPHA(N), the diagonal elements of R.
	!
	!    Output, integer IPIVOT(N), reflects the column pivoting performed 
	!    on the input matrix to accomplish the decomposition.  The J-th
	!    element of IPIVOT gives the column of the original matrix which was 
	!    pivoted into column J during the decomposition.
	!
	!    Output, integer IERR, error flag.
	!    0, for normal return,
	!    K, if no non-zero pivot could be found for the K-th transformation,
	!    -K, for an error exit on the K-th transformation.
	!    If an error exit was taken, the first (K - 1) transformations are correct.
	!
	!    Input, integer NOPIVK, controls pivoting.  Columns 1 through NOPIVK
	!    will remain fixed in position.
	!
	!    Workspace, real SUM(N).
	!
	!  Local Parameters:
	!
	!    Local, real UFETA, the smallest positive floating point number
	!    such that UFETA and -UFETA can both be represented.
	!
	!    Local, real RKTOL, the square root of the relative precision
	!    of floating point arithmetic (MACHEP).
	*/

	  double alphak;
	  double beta;
	  int i;
	  int j;
	  int jbar;
	  int k;
	  int minum;
	  int mk1;
	  double qrkk;
	  double qrkmax;
	  double rktol1;
	  double sigma;
	  double sumj;
	  double temp;
	  int ii;
	  double arr[];
	  double arr2[];

	  if ( ufeta_qrfact <= 0.0E+00 ) {
	    ufeta_qrfact = tiny;
	    rktol_qrfact = Math.sqrt ( 0.999E+00 * epsilon);
	  }

	  ierr[0] = 0;
	  rktol1 = 0.01E+00 * rktol_qrfact;

      arr = new double[m+1];	  
	  for (j = 1; j <= n; j++) {
		for (ii = 1; ii <= m; ii++) {
			arr[ii] = qr[ii][j];
		}
	    sum[j] = v2norm ( m, arr );
	    ipivot[j] = j;
	  }

	  minum = Math.min ( m, n );

	  for ( k = 1; k <= minum; k++) {

	    mk1 = m - k + 1;
	//
	//  K-th Householder transformation.
    //
	    sigma = 0.0;
	    jbar = 0;
	//
    //  Find largest column sum.
	//
	    if ( nopivk < k ) {

	      for (j = k; j <= n; j++) {
	        if ( sigma < sum[j] ) {
	          sigma = sum[j];
	          jbar = j;
	        }
	      } // for (j = k; j <= n; j++)

	      if ( jbar == 0 ) {
	        ierr[0] = k;
	        for (i = k; i <= n; i++) {
	          alpha[i] = 0.0;
	          if ( k < i ) {
	        	for (ii = k; ii <= i-1; ii++) {
	                qr[ii][i] = 0.0;
	        	}
	          } // if (k < i)
	        } // for (i = k; i <= n; i++)
	        return;
	      } // if (jbar == 0)
	//
	//  Column interchange.
	//
	      i = ipivot[k];
	      ipivot[k] = ipivot[jbar];
	      ipivot[jbar] = i;

	      sum[jbar] = sum[k];
	      sum[k] = sigma;

	      for (i = 1; i <= m; i++) {
	        sigma = qr[i][k];
	        qr[i][k] = qr[i][jbar];
	        qr[i][jbar] = sigma;
	      } // for (i = 1; i <= m; i++)

	    } // if (nopivk < k)
	//
	//  Second inner product.
	//
	    qrkmax = Math.abs(qr[k][k]);
	    for (ii = k+1; ii <= m; ii++) {
	    	qrkmax = Math.max(qrkmax, Math.abs(qr[ii][k]));
	    }

	    if ( qrkmax < ufeta_qrfact ) {
	      ierr[0] = -k;
	      for (i = k; i <= n; i++) {
	        alpha[i] = 0.0;
	        if ( k < i ) {
	          for (ii = k; ii <= i-1; ii++) {
	              qr[ii][i] = 0.0;
	          }
	        } // if (k < i)
	      } // for (i = k; i <= n; i++)
	      return;
	    } // if (qrkmax < ufeta_qrfact)

	    arr = new double[mk1+1];
	    for (ii = 1; ii <= mk1; ii++) {
	    	arr[ii] = qr[k+ii-1][k];
	    }
	    alphak = v2norm ( mk1, arr ) / qrkmax;
	    sigma = alphak*alphak;
	//
	//  End second inner product.
	//
	    qrkk = qr[k][k];
	    if ( 0.0 <= qrkk ) {
	      alphak = -alphak;
	    }

	    alpha[k] = alphak * qrkmax;
	    beta = qrkmax * Math.sqrt ( sigma - ( qrkk * alphak / qrkmax ) );
	    qr[k][k] = qrkk - alpha[k];
	    for (ii = k; ii <= m; ii++) {
	        qr[ii][k] =  qr[ii][k] / beta;
	    }

	    for (j = k + 1; j <= n; j++) {
          arr = new double[mk1+1];
          arr2 = new double[mk1+1];
          for (ii = 1; ii <= mk1; ii++) {
        	  arr[ii] = qr[k+ii-1][k];
        	  arr2[ii] = qr[k+ii-1][j];
          }
	      temp = -dotprd ( mk1, arr, arr2 );

	      for (ii = k; ii <= m; ii++) {
	          qr[ii][j] = qr[ii][j] + temp * qr[ii][k];
	      }

	      if ( k + 1 <= m ) {
	        sumj = sum[j];
	        if ( ufeta_qrfact <= sumj ) {
	          temp = Math.abs ( qr[k][j] / sumj );
	          if ( rktol1 <= temp ) {
	            if ( 0.99 <= temp ) {
	              arr = new double[m-k+1];
	              for (ii = 1; ii <= m-k; ii++) {
	            	  arr[ii] = qr[k+ii][j];
	              }
	              sum[j] = v2norm ( m-k, arr );
	            }
	            else {
	              sum[j] = sumj * Math.sqrt ( 1.0 - temp*temp );
	            }

	          } // if (rktol1 <= temp)
	        } // if (ufeta_qrfact <= sumj)
	      } // if (k + 1 <= m)

	    }// for (j = k + 1; j <= n; j++)

	  } // for ( k = 1; k <= minum; k++)

	  return;
	} // private void qrfact
	
	double reldst ( int p, double d[], double x[], double x0[] ) {

	/***********************************************************************
	!
	!! RELDST computes the relative difference between two real values.
	!
	!  Modified:
	!
	!    03 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer P, the length of the vectors.
	!
	!    Input, real D(P), a scaling vector.
	!
	!    Input, real X(P), X0(P), two vectors whose relative difference
	!    is desired.
	!
	!    Output, real reldstVal, the relative difference between X and X0.
	*/
	  
	  double emax;
	  int i;
	  double reldstVal;
	  double xmax;

	  emax = 0.0E+00;
	  xmax = 0.0E+00;
	  for ( i = 1; i <= p; i++) {
	    emax = Math.max ( emax, Math.abs ( d[i] * ( x[i] - x0[i] ) ) );
	    xmax = Math.max ( xmax, d[i] * ( Math.abs ( x[i] ) + Math.abs ( x0[i] ) ) );
	  }

	  if ( 0.0E+00 < xmax ) {
	    reldstVal = emax / xmax;
	  }
	  else {
	    reldstVal = 0.0E+00;
	  }

	  return reldstVal;
	} // double reldst ( int p, double d[], double x[], double x0[] )
	
	private void rptmul ( int func, int ipivot[], double j[][], int nn, int p, 
			              double rd[], double x[], double y[], double z[] ) {

	/***********************************************************************
	!
	!! RPTMUL multiplies the R factor times a vector X.
	!
	!  Discussion:
	!
	!    This routine computes one of:
	!
	!      Y = R * P' * X 
	!      Y = P * R' * R * P' * X 
	!      Y = P * R' * X.
	!
	!    where P is a permutation matrix represented by a permutation 
	!    vector, and R is an upper triangular matrix, the R factor of 
	!    a QR factorization.
	!
	!    The strict upper triangle of R is stored in the strict upper triangle 
	!    of the array J, and the diagonal of R is stored in the vector RD.
	!
	!    X and Y may share storage.
	!
	!  Modified:
	!
	!    10 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer FUNC, determines which product to compute:
	!    1, Y =                RMAT * PERM' * X.
	!    2, Y = PERM * PERM' * RMAT * PERM' * X.
	!    3, Y = PERM *                PERM' * X.
	!
	!    Input, integer IPIVOT(P), the permutation vector.
	!
	!    Input, real J(NN,P), contains the strict upper triangle of the
	!    matrix RMAT.
	!
	!    Input, integer NN, the leading dimension of J.
	!
	!    Input, integer P, the length of X and Y, and the order of RMAT.
	!
	!    Input, real RD(P), the diagonal elements of the matrix RMAT.
	!
	!    Input, real X(P), the input vector.
	!
	!    Output, real Y(P), the output vector.
	!
	!    Workspace, real Z(P).
	*/
	  
	  int i;
	  int k;
	  double zk;
	  int m;
	  double arr[];

	  if ( func <= 2 ) {
	//
	//  Set Z = PERM' * X.
	//
	    for ( i = 1; i <= p; i++) {
	      k = ipivot[i];
	      z[i] = x[k];
	    } // for (i = 1; i <= p; i++)
	//
	//  Set Y = RMAT * Z.
	//
	    y[1] = z[1] * rd[1];

	    for (k = 2; k <= p; k++) {
	      zk = z[k];
	      for( i = 1; i <= k-1; i++) {
	        y[i] = y[i] + j[i][k] * zk;
	      }
	      y[k] = zk * rd[k];
	    } // for (k = 2; k <= p; k++)

	    if ( func <= 1 ) {
	      return;
	    }
	  } // if (func <= 2)
	  else { // func > 2
        for (m = 1; m <= p; m++) {
	        y[m] = x[m];
        }

	  } // else func > 2
	//
	//  Set Z = RMAT' * Y.
	//
	  z[1] = y[1] * rd[1];

	  for (i = 2; i <= p; i++) {
		  arr = new double[i];
		  for (m = 1; m <= i-1; m++) {
			  arr[m] = j[m][i];
		  }
	    z[i] = y[i] * rd[i] + dotprd ( i-1, arr, y );
	  }
	//
	//  Set Y = PERM * Z.
	//
	  for ( i = 1; i <= p; i++) {
	    k = ipivot[i];
	    y[k] = z[i];
	  }

	  return;
	} // private void rptmul
	
	private void slupdt ( double a[], double cosmin, int p, double size, double step[],
			              double u[], double w[], double wchmtd[], double wscale[], double y[] ) {

	/***********************************************************************
	!
	!! SLUPDT updates a symmetric matrix A so that A * STEP = Y.
	!
	!  Discussion:
	!
	!    Update the symmetric matrix A so that A * STEP = Y.  Only the lower
	!    triangle of A is stored, by rows.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	*/
	  
	  double denmin;
	  int i;
	  int j;
	  int k;
	  double sdotwm;
	  double t;
	  int m;

	  sdotwm = dotprd ( p, step, wchmtd );

	  denmin = cosmin * v2norm ( p, step ) * v2norm ( p, wchmtd );

	  if ( denmin != 0.0 ) {
	    wscale[0] = Math.min ( 1.0, Math.abs ( sdotwm / denmin ) );
	  }
	  else {
	    wscale[0] = 1.0;
	  }

	  if ( sdotwm != 0.0 ) {
	    t = wscale[0] / sdotwm;
	  }
	  else {
	    t = 0.0;
	  }

	  for (m = 1; m <= p; m++) {
	      w[m] = t * wchmtd[m];
	  }

	  slvmul ( p, u, a, step );

	  t = 0.5 * ( size * dotprd ( p, step, u ) - dotprd ( p, step, y ) );

	  for (m = 1; m <= p; m++) {
	      u[m] = t * w[m] + y[m] - size * u[m];
	  }
	//
	//  Set A = A + U * W' + W * U'.
	//
	  k = 1;
	  for ( i = 1; i <= p; i++) {
	    for ( j = 1; j <= i; j++) {
	      a[k] = size * a[k] + u[i] * w[j] + w[i] * u[j];
	      k = k + 1;
	    } // for (j = 1; j <= i; j++)
	  } // for (i = 1; i <= p; i++)

	  return;
	} // private void slupdt
	
	private void slvmul ( int p, double y[], double s[], double x[] ) {

	/***********************************************************************
	!
	!! SLVMUL sets Y = S * X, where S is a P by P symmetric matrix.
	!
	!  Discussion:
	!
	!    This routine sets Y = S * X,  where X is a given vector and
	!    S is a P by P symmetric matrix.  The lower triangle of S is
	!    stored by rows.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer P, the order of S, X and Y.
	!
	!    Output, real Y(P), the product S * X.
	!
	!    Input, real S((P*(P+1))/2), the P by P symmetric matrix.  Only the
	!    lower triangle is stored, by rows.
	!
	!    Input, real X(P), the vector to be multiplied by S.
	*/

	  int i;
	  int j;
	  int k;
	  double arr[];
	  int m;
	//
	//  Compute the lower triangle of S times X.
	//
	  j = 1;
	  for ( i = 1; i <= p; i++) {
		  arr = new double[i+1];
		  for (m = 1; m <= i; m++) {
			  arr[m] = s[j+m-1];
		  }
	    y[i] = dotprd ( i, arr, x );
	    j = j + i;
	  } // for (i = 1; i <= p; i++)
	//
	//  Compute the strict upper triangle of S times X.
	//
	  j = 1;
	  for ( i = 2; i <= p; i++) {
	    j = j + 1;
	    for (k = 1; k <= i - 1; k++) {
	      y[k] = y[k] + s[j] * x[i];
	      j = j + 1;
	    }
	  } // for (i = 1; i <= p; i++)

	  return;
	} // private void slvmul
	
	private boolean stopx ( int idummy[] ) {
 
	/***********************************************************************
	!
	!! STOPX is called to stop execution.
	!
	!  Discussion:
	!
	!    This function may serve as the STOPX (asynchronous interruption)
	!    function for the NL2SOL package at those installations which do not 
	!    wish to implement a dynamic STOPX.
	!
	!    At installations where the NL2SOL system is used
	!    interactively, this dummy STOPX should be replaced by a
	!    function that returns TRUE if and only if the interrupt
	!    (break) key has been pressed since the last call on STOPX.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	*/
	  boolean result;

	  result = false;

	  return result;
	} // private boolean stopx
	
	private void timestamp() {
	    Calendar dateTime = Calendar.getInstance();
	    Formatter formatter = new Formatter();
	    // The conversion character t is used to print dates and times in various formats
	    // 1$ indicates that all format specifiers in the formt string used the first argument
	    //    after the format string in the argument list
	    // B full name of the month
	    // d Display the day of the month with 2 digits, padding with leading zeros as necessary
	    // Y Display the year with 4 digits
	    // Z Display the abbreviation for the time zone
	    // H Display hour in a 12-hour clock with a leading zero as necessary
	    // M Display minute with a leading zero as necessary
	    // S Display second with a leading zero as necessary
	    formatter.format("%1$tB %1$td, %1$tY    %1$tZ %1$tH:%1$tM:%1$tS\n", dateTime);
	    Preferences.debug(formatter.toString(), Preferences.DEBUG_ALGORITHM);
	}
	
	private double v2norm ( int p, double x[] ) {

	/***********************************************************************
	!
	!! V2NORM computes the L2 norm of a vector.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer P, the length of the vector.
	!
	!    Input, real X(P), the vector.
	!
	!    Output, real V2NORM, the Euclidean norm of the vector.
	! 
	!  Local Parameters:
	!
	!    SQTETA is (slightly larger than) the square root of the
	!    smallest positive floating point number on the machine.
	!    The tests involving SQTETA are done to prevent underflows.
	*/

	  int i;
	  int j;
	  double r;
	  double scale;
	  
	  double t;
	  double result;
	  double xi;

	  if ( p <= 0 ) {
	    result = 0.0;
	    return result;
	  }

	  i = 0;

	  for ( j = 1; j <= p; j++) {
	    if ( x[j] != 0.0 ) {
	      i = j;
	      break;
	    } //if (x[j] != 0.0)
	  } // for (j = 1; j <= p; j++)

	  if ( i == 0 ) {
	    result = 0.0;
	    return result;
	  }

	  scale = Math.abs ( x[i] );

	  t = 1.0;

	  if ( sqteta_v2norm == 0.0 ) {
	    sqteta_v2norm = Math.sqrt ( 1.001 * tiny);
	  }

	  j = i + 1;

	  for (i = j; i <= p; i++) {

	    xi = Math.abs ( x[i] );

	    if ( xi <= scale ) {

	      r = xi / scale;

	      if ( sqteta_v2norm < r ) {
	        t = t + r * r;
	      }

	    } // if (xi <= scale)
	    else {

	      r = scale / xi;

	      if ( sqteta_v2norm < r ) {
	        t = 1.0E+00 + t * r * r;
	      }
	      else {
	        t = 1.0E+00;
	      }

	      scale = xi;

	    } // else

	  } // for (i = j; i <= p; i++)

	  result = scale * Math.sqrt ( t );

	  return result;
	} // private double v2norm
	
	public void statusMessageNL2sol(final int status, final int numParam) {
        if (status == 3) {
            Preferences.debug("X-convergence.  The scaled relative difference between the current parameter\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("vector x and a locally optimal parameter vector is very likely at most v[xctol].\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 4) {
            Preferences.debug("Relative function convergence.  The relative difference between the current\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("function value and its locally optimal value is very likely at most v(rfctol).\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 5) {
            Preferences.debug("Both x- and relative function convergence (i.e., the conditions for iv(1) = 3 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("iv(1) = 4 both hold).\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 6) {
            Preferences.debug("Absolute function convergence.  the current function value is at most v(afctol)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("in absolute value.\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 7) {
            Preferences.debug("Singular convergence.  The hessian near the current iterate appears to be singular\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("or nearly so, and a step of length at most v(lmax0) is unlikely to yield a relative\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("function decrease of more than v(rfctol).\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 8) {
            Preferences.debug("False convergence.  The iterates appear to be converging to a noncritical point.\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("This may mean that the convergence tolerances (v(afctol), v(rfctol), v(xctol)) are\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("too small for the accuracy to which the function and gradient are being computed,\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("that there is an error in computing the gradient, or that the function or gradient\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("is discontinuous near x.\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 9) {
            Preferences.debug("Function evaluation limit reached without other convergence (see iv(mxfcal)).\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 10) {
            Preferences.debug("Iteration limit reached without other convergence (see iv(mxiter)).\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 11) {
            Preferences.debug("stopx returned .true. (external interrupt).\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 13) {
            Preferences.debug("f(x) cannot be computed at the initial x.\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 14) {
            Preferences.debug("Bad parameters passed to assess (which should not occur).\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 15) {
            Preferences.debug("The jacobian could not be computed at x\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 16) {
            Preferences.debug("n or p (or parameter nn to nl2itr) out of range --\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("p <= 0 or n < p or nn < n.\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 17) {
            Preferences.debug("Restart attempted with n or p (or par. nn to nl2itr) changed.\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 18) {
            Preferences.debug("iv(inits) is out of range.\n", Preferences.DEBUG_ALGORITHM);
        } else if ( (status >= 19) && (status <= 45)) {
            Preferences.debug("v(iv(1)) is out of range.\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 50) {
            Preferences.debug("iv(1) was out of range.\n", Preferences.DEBUG_ALGORITHM);
        } else if ( (status >= 87) && (status <= 86 + numParam)) {
            Preferences.debug("jtol(iv(1)-86) (i.e., v(iv(1)) is not positive\n", Preferences.DEBUG_ALGORITHM);
        }
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    }
}