package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.Preferences;

public class InverseLaplaceTest extends AlgorithmBase {
	// Number of times to invert for
	int n = 30;
	
	// vector of times to invert for
    double[] t = new double[n];

    // true values of the 16 functions
    double[][] ftrue = new double[16][n];
    
	public InverseLaplaceTest() {
		
	}
	
	public void runAlgorithm() {
	    int i;
	    double realArg;
    	double imaginaryArg = 0.0;
    	double initialOrder = 0.0;
    	int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
    	double realResult[] = new double[1];
    	double imagResult[] = new double[1];
    	int[] nz = new int[1]; // number of components set to zero due to underflow
        int[] errorFlag = new int[1]; // zero if no error
        // Euler's constant gamma
        double gamma = 0.57721566490153286061;
        // An estimate for the maximum of the real parts of the singularities
        // of F. If unknown, set largestPole = 0.0
        double largestPoledeHoog = 1.0;

        // numerical tolerance of approaching pole (default = 1.0e-9)
        double toldeHoog = 1.0e-9;
        FitdeHoog1 moddeHoog1;
        
        double abscissaPiessens = 0.0;
        double relEpsPiessens = 1.0E-12;
        double absEpsPiessens = 1.0E-12;
        double[] resultPiessens = new double[n];
        double[] estErrPiessens = new double[n];
        int[] evaluationsPiessens = new int[n];
        int[] errStatusPiessens = new int[n];
        FitPiessens1 modPiessens1;
        
        // An estimate for the maximum of the real parts of the singularities
        // of F. If unknown, set largestPole = 0.0
        double largestPoleqd = 0.0;
        // numerical tolerance of approaching pole (default = 1.0e-9)
        double tolqd = 1.0e-9;
        int matrixSizeParameterqd = 3;
        // Must include t = 0 point in an array of evenly spaced points
        double tqd[] = new double[10*n+1];
        Fitqd1 modqd1;
        
        double pretWeeks[] = new double[1];
        double sig0Weeks = 0.0;
        double sigmaxWeeks = 30.0;
        double bmaxWeeks = 30.0;
        int nLaguerreWeeks = 32;
        double tolsWeeks = 1.0e-9;
        double tolbWeeks = 1.0e-9;
        double bWeeks;
        double sigWeeks;
        FitWeeks1 modPreWeeks;
        FitWeeks1 modWeeks1;
        
        double[] timeFunction = null;
        double diff;
        double square;
        double sum;
        double expsum;
        double ledenom;
        double L;
        double Le;
	    for (i = 1; i <= n; i++) {
	        t[i-1] = 0.5*i;
	        
	        // f1(t) = J0(t)
	        realArg = t[i-1];
            
        	Bessel bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                    sequenceNumber, realResult, imagResult, nz, errorFlag);
        	bes.run();
        	if (errorFlag[0] != 0) {
        	    displayError("Bessel_J error for realArg = " + realArg);
        	    setCompleted(false);
        	    return;
        	}
	        ftrue[0][i-1] = realResult[0];
	        
	        // f2(t) = ((PI*t)**-1/2)*cos(2*sqrt(t))
	        ftrue[1][i-1] = Math.cos(2.0*Math.sqrt(t[i-1]))/Math.sqrt(Math.PI*t[i-1]);
	        
	        ftrue[2][i-1] = Math.exp(-0.5*t[i-1]);
	        
	        ftrue[3][i-1] = Math.exp(-0.2*t[i-1])*Math.sin(t[i-1]);
	        
	        ftrue[4][i-1] = 1.0;
	        
	        ftrue[5][i-1] = t[i-1];
	        
	        ftrue[6][i-1] = t[i-1]*Math.exp(-t[i-1]);
	        
	        ftrue[7][i-1] = Math.sin(t[i-1]);
	        
	        ftrue[8][i-1] = 1.0/Math.sqrt(Math.PI*t[i-1]);
	        
	        if (i >= 10) {
	        	ftrue[9][i-1] = 1.0;
	        }
	        else {
	        	ftrue[9][i-1] = 0.0;
	        }
	        
	        ftrue[10][i-1] = -gamma - Math.log(t[i-1]);
	        
	        // square wave
	        // 1 for 2n < x < (2n+1)
	        // 0 for (2n+1) < x < (2n+2)
	        if (((i % 4) == 0) || (( i % 4) == 1)) {
	        	ftrue[11][i-1] = 1.0;
	        }
	        else {
	            ftrue[11][i-1] = 0.0;
	        }
	        
	        ftrue[12][i-1] = t[i-1] * Math.cos(t[i-1]);
	        
	        ftrue[13][i-1] = (Math.exp(-0.25*t[i-1]) - Math.exp(-0.5*t[i-1]))/Math.sqrt(4.0*Math.PI*t[i-1]*t[i-1]*t[i-1]);
	        
	        ftrue[14][i-1] = 2.0*Math.exp(-4.0/t[i-1])/Math.sqrt(Math.PI*t[i-1]*t[i-1]*t[i-1]);
	        
	        ftrue[15][i-1] = Math.sin(t[i-1])/t[i-1];
	    } // for (i = 1; i <= n; i++)
	    
	    for (i = 0; i < n; i++) {
        	tqd[10*(i+1)] = 15.0*i/(n-1);
        }
	    
	    ledenom = 0.0;
	    for (i = 0; i < n; i++) {
	    	ledenom = ledenom + Math.exp(-t[i]);
	    }
	    
	    Preferences.debug("F1(s) = (s*s + 1)**-1/2\n");
	    Preferences.debug("f1(t) = J0(t)\n\n");
	    
	    Preferences.debug("deHoog algorithm\n");
	    // L = 1.695E-8 Le = 1.493E-10
	    
	    moddeHoog1 = new FitdeHoog1(t, largestPoledeHoog, toldeHoog);
        moddeHoog1.driver();
        timeFunction = moddeHoog1.getTimeFunction();

        sum = 0.0;
        expsum = 0.0;
        for (i = 0; i < n; i++) {
            Preferences.debug("time = " + t[i] + " inverseLaplaceFunction = " + timeFunction[i] + " trueFunction = " +
                              ftrue[0][i] + "\n");
            diff = ftrue[0][i] - timeFunction[i];
            square = diff * diff;
            sum = sum + square;
            expsum = expsum + square * Math.exp(-t[i]);
        } 
        L = Math.sqrt(sum/n);
        Le = Math.sqrt(expsum/ledenom);
        Preferences.debug("Root mean square error = " + L + "\n");
        Preferences.debug("Exponential weighted root mean square error = " + Le + "\n");
        
        Preferences.debug("Piessens algorithm\n");
        // L = 3.905E-14 Le = 3.216E-14
        
        modPiessens1 = new FitPiessens1(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
        		                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
        modPiessens1.driver();

        sum = 0.0;
        expsum = 0.0;
        for (i = 0; i < n; i++) {

            if (errStatusPiessens[i] == 2) {
                Preferences.debug("time[" + i + "] is illegally <= 0\n");
            } else if (errStatusPiessens[i] == 1) {
                Preferences.debug("time[" + i + "] had computations terminated\n");
                Preferences.debug("The maximum bound on Laplace evaluations was reached\n");
            } else {
                Preferences.debug("time = " + t[i] + " inverseLaplaceFunction = " + resultPiessens[i] + " trueFunction = " +
                                  ftrue[0][i] + "\n");
                diff = ftrue[0][i] - resultPiessens[i];
                square = diff * diff;
                sum = sum + square;
                expsum = expsum + square * Math.exp(-t[i]);

            }
        } // for (i = 0; i < n; i++)
        L = Math.sqrt(sum/n);
        Le = Math.sqrt(expsum/ledenom);
        Preferences.debug("Root mean square error = " + L + "\n");
        Preferences.debug("Exponential weighted root mean square error = " + Le + "\n");
        
        Preferences.debug("Brancik algorithm\n");
        
        // Include tqd[0] = 0 in calculation
        // Increase number of time points from 30 to 301.
        // Increasing number of evenly spaced time points from 31 to 301 takes
        // L = 1.300E-6 Le = 4.333E-6 to L = 1.876E-10 Le = 5.679E-10 for 
        // largestPoleqd = 0.0, tolqd = 1.0E-9, matrixSizeParameterqd = 3.
        // Taking tolqd to 1.0E-10 gives L = 1.699E-10 Le = 5.829E-10
        
        modqd1 = new Fitqd1(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
        modqd1.driver();
        timeFunction = modqd1.getTimeFunction();
        
        sum = 0.0;
        expsum = 0.0;
        for (i = 0; i < n; i++) {
            Preferences.debug("time = " + t[i] + " inverseLaplaceFunction = " + timeFunction[10*(i+1)] + " trueFunction = " +
                              ftrue[0][i] + "\n");
            diff = ftrue[0][i] - timeFunction[10*(i+1)];
            square = diff * diff;
            sum = sum + square;
            expsum = expsum + square * Math.exp(-t[i]);
        } 
        L = Math.sqrt(sum/n);
        Le = Math.sqrt(expsum/ledenom);
        Preferences.debug("Root mean square error = " + L + "\n");
        Preferences.debug("Exponential weighted root mean square error = " + Le + "\n");
        
        Preferences.debug("Weeks algorithm\n");
        // Answers vary slightly from run to run
        // Typical is L = 4.885E-8 Le = 1.377E-10
        
        pretWeeks[0] = 1.0;
        modPreWeeks = new FitWeeks1(pretWeeks, nLaguerreWeeks, sig0Weeks, sigmaxWeeks, bmaxWeeks, tolsWeeks, tolbWeeks);
        modPreWeeks.wpar2();
        bWeeks = modPreWeeks.getBOpt();
        sigWeeks = modPreWeeks.getSigOpt();
        Preferences.debug("bWeeks = " + bWeeks + " sigWeeks = " + sigWeeks + "\n");
        
        modWeeks1 = new FitWeeks1(t, nLaguerreWeeks, sigWeeks, bWeeks);
        modWeeks1.driver();
        timeFunction = modWeeks1.getTimeFunction();
        
        sum = 0.0;
        expsum = 0.0;
        for (i = 0; i < n; i++) {
            Preferences.debug("time = " + t[i] + " inverseLaplaceFunction = " + timeFunction[i] + " trueFunction = " +
                              ftrue[0][i] + "\n");
            diff = ftrue[0][i] - timeFunction[i];
            square = diff * diff;
            sum = sum + square;
            expsum = expsum + square * Math.exp(-t[i]);
        } 
        L = Math.sqrt(sum/n);
        Le = Math.sqrt(expsum/ledenom);
        Preferences.debug("Root mean square error = " + L + "\n");
        Preferences.debug("Exponential weighted root mean square error = " + Le + "\n");
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
     * @param  ar  double
     * @param  ai  double
     * @param  br  double[]
     * @param  bi  double[]
     */
    private void zexp(double ar, double ai, double[] br, double[] bi) {
        double zm, ca, cb;
        zm = Math.exp(ar);
        ca = zm * Math.cos(ai);
        cb = zm * Math.sin(ai);
        br[0] = ca;
        bi[0] = cb;

        return;
    }
    
    /**
     * complex square root b = csqrt(a).
     *
     * @param  ar  double
     * @param  ai  double
     * @param  br  double[]
     * @param  bi  double[]
     */
    private void zsqrt(double ar, double ai, double[] br, double[] bi) {
        double drt = 1.0 / Math.sqrt(2.0);
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
	 * 
	 * @param testNum
	 * @param largestPole An estimate for the maximum of the real parts of the singularities
              of F. If unknown, set largestPole = 0.0
	 * @param tol numerical tolerance of approaching pole (default = 1.0e-9)
	 */
	private void runLapTest(int testNum, double largestPole, double tol) {
		// This routine tests the numerical inverse laplace transform of a
        // function in InverseLaplace with an analytically known inverse.  The
        // function to be transformed is exp(a*t)
        // parameter a in exp(a*t)
        double a = 1.0;
        
		// number of times to invert for
        int n = 30;

        // vector of times to invert for
        double[] t = new double[n];

        // true value of the function
        double[] ftrue = new double[n];
        int i;
        FitExpModel lmod;

        double[] timeFunction = null;

        for (i = 1; i <= n; i++) {
            t[i - 1] = Math.pow(10.0, (0.5 * (double) i) - 13.0);
            ftrue[i - 1] = Math.exp(a * t[i - 1]);
        }

        lmod = new FitExpModel(t, largestPole, tol);
        lmod.driver();
        timeFunction = lmod.getTimeFunction();

        for (i = 0; i < n; i++) {
            Preferences.debug("time = " + t[i] + " routineFunction = " + timeFunction[i] + " trueFunction = " +
                              ftrue[i] + "\n");
        } 	
	}
	
	private void runLapTest() {

        // This routine tests the numerical inverse laplace transform of a
        // function in InverseLaplace with an analytically known inverse.  The
        // function to be transformed is exp(a*t)
        // parameter a in exp(a*t)
        double a = 1.0;

        // An estimate for the maximum of the real parts of the singularities
        // of F. If unknown, set largestPole = 0.0
        double largestPole = 1.0;

        // numerical tolerance of approaching pole (default = 1.0e-9)
        double tol = 1.0e-9;

        // number of times to invert for
        int n = 30;

        // vector of times to invert for
        double[] t = new double[n];

        // true value of the function exp(a*t)
        double[] ftrue = new double[n];
        int i;
        FitExpModel lmod;

        double[] timeFunction = null;

        for (i = 1; i <= n; i++) {
            t[i - 1] = Math.pow(10.0, (0.5 * (double) i) - 13.0);
            ftrue[i - 1] = Math.exp(a * t[i - 1]);
        }

        lmod = new FitExpModel(t, largestPole, tol);
        lmod.driver();
        timeFunction = lmod.getTimeFunction();

        for (i = 0; i < n; i++) {
            Preferences.debug("time = " + t[i] + " routineFunction = " + timeFunction[i] + " trueFunction = " +
                              ftrue[i] + "\n");
        } 

    }
	
	private void runLapTest2() {

        // This routine tests the numerical inverse Laplace transform of a
        // function in InverseLaplace2 with an analytically known inverse.  The
        // function to be transformed is sin(t)
        double[] time = new double[] {
                            0.1, 1.0, 2.0, 3.0, 4.0, 5.0, 1.0E1, 2.0E1, 3.0E1, 4.0E1, 5.0E1, 6.0E1, 7.0E1, 8.0E1, 9.0E1,
                            1.0E2
                        };
        double abscissa = 0.0;
        double relEps = 1.0E-12;
        double absEps = 1.0E-12;
        double[] result = new double[time.length];
        double[] estErr = new double[time.length];
        int[] evaluations = new int[time.length];
        int[] errStatus = new int[time.length];
        FitSineModel smod;
        int i;

        smod = new FitSineModel(time, abscissa, relEps, absEps, result, estErr, evaluations, errStatus);
        smod.driver();

        for (i = 0; i < time.length; i++) {

            if (errStatus[i] == 2) {
                Preferences.debug("time[" + i + "] is illegally <= 0\n");
            } else if (errStatus[i] == 1) {
                Preferences.debug("time[" + i + "] had computations terminated\n");
                Preferences.debug("The maximum bound on Laplace evaluations was reached\n");
            } else {
                Preferences.debug("time = " + time[i] + " routineFunction = " + result[i] + " trueFunction = " +
                                  Math.sin(time[i]) + "\n");

            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void runLapTestqd() {

        // This routine tests the numerical inverse laplace transform of a
        // function in InverseLaplace with an analytically known inverse.  The
        // function to be transformed is exp(a*t)
        // parameter a in exp(a*t)
        double a = 1.0;

        // An estimate for the maximum of the real parts of the singularities
        // of F. If unknown, set largestPole = 0.0
        double largestPole = 1.0;

        // numerical tolerance of approaching pole (default = 1.0e-9)
        double tol = 1.0e-9;

        // number of times to invert for
        int n = 103;
        int matrixSizeParameter = 3;

        // vector of times to invert for
        double[] t = new double[n];

        // true value of the function exp(a*t)
        double[] ftrue = new double[n];
        int i;
        FitExpModelqd lmod;
        double sse = 0;
        double diff;
        double rms;

        double[] timeFunction = null;


        for (i = 0; i < n; i++) {
            t[i] = i * 0.1;
            ftrue[i] = Math.exp(a * t[i]);
        }

        Preferences.debug("matrixSizeParameter = " + matrixSizeParameter + "\n");
        Preferences.debug("tol = " + tol + "\n");
        lmod = new FitExpModelqd(n, t[n - 1], largestPole, tol, matrixSizeParameter);
        lmod.driver();
        timeFunction = lmod.getTimeFunction();

        for (i = 0; i < n; i++) {
            Preferences.debug("time = " + t[i] + " routineFunction = " + timeFunction[i] + " trueFunction = " +
                              ftrue[i] + "\n");
        }

        sse = 0.0;

        for (i = 1; i < n; i++) {
            diff = timeFunction[i] - ftrue[i];
            sse = sse + (diff * diff);
        }

        rms = Math.sqrt(sse / (n - 1));
        Preferences.debug("rms error = " + rms + "\n");

    }

    /**
     * DOCUMENT ME!
     */
    private void runLapTestWeeks() {

        // This routine tests the numerical inverse laplace transform of a
        // function in InverseLaplace with an analytically known inverse.  The
        // function to be transformed is exp(a*t)
        // parameter a in exp(a*t)
        double a = -1.0;

        // An estimate for the maximum of the real parts of the singularities
        // of F. If unknown, set largestPole = 0.0
        double largestPole = 1.0;

        // numerical tolerance of approaching pole (default = 1.0e-9)
        double tol = 1.0e-9;

        // number of times to invert for
        int n = 1;
        int matrixSizeParameter = 3;

        // vector of times to invert for
        double[] t = new double[n];

        // true value of the function exp(a*t)
        double[] ftrue = new double[n];
        int i;
        //FitExpModelWeeks lmod;
        //FitBesselModelWeeks lmodB;
        FitModelWeeks3 lmod3;
        double sig0 = 0.0;
        double sigmax = 30.0;
        double bmax = 30.0;
        int nLaguerre = 32;
        double tols = 1.0e-9;
        double tolb = 1.0e-9;
        double sse = 0;
        double diff;
        double rms;
        double b = 1.0;
        double sig = 1.0;
        double[] tOne = new double[1];

        double[] timeFunction = null;
        Bessel testBessel;
        double[] cyr = new double[1];
        double[] cyi = new double[1];
        int[] nz = new int[1];
        int[] errorFlag = new int[1];

        /*for (i = 0; i < n; i++) {
         * t[i] = (i+1)*0.1; ftrue[i] = Math.exp(a*t[i]); }
         *
         * tOne[0] = t[t.length/2]; lmod = new FitExpModelWeeks(tOne, nLaguerre, sig0, sigmax, bmax, tols,
         *         tolb);*/
        t[0] = 1.0;
        ftrue[0] = 2.0 * Math.exp(-4.0 / t[0]) / Math.sqrt(Math.PI * t[0] * t[0] * t[0]);
        lmod3 = new FitModelWeeks3(t, nLaguerre, sig0, sigmax, bmax, tols, tolb);
        lmod3.wpar2();
        b = lmod3.getBOpt();
        sig = lmod3.getSigOpt();
        Preferences.debug("b = " + b + " sig = " + sig);
        /*sig = 1.0;
         * b = 1.0; t[0] = 0.1; t[1] = 1.0; t[2] = 10.0; for (i = 0; i < n; i++) { testBessel =   new
         * Bessel(Bessel.BESSEL_J, t[i], 0.0, 0.0, Bessel.UNSCALED_FUNCTION,              1, cyr, cyi, nz, errorFlag);
         * testBessel.run(); ftrue[i] = cyr[0];}*/

        // lmod = new FitExpModelWeeks(t, nLaguerre, sig, b);
        // lmodB = new FitBesselModelWeeks(t, nLaguerre, sig, b);
        lmod3 = new FitModelWeeks3(t, nLaguerre, sig, b);
        lmod3.driver();
        timeFunction = lmod3.getTimeFunction();

        for (i = 0; i < n; i++) {
            Preferences.debug("time = " + t[i] + " routineFunction = " + timeFunction[i] + " trueFunction = " +
                              ftrue[i] + "\n");
        }

        /*sse = 0.0;
         * for (i = 0; i < n; i++) { diff = timeFunction[i] - ftrue[i]; sse = sse + diff*diff; } rms =
         * Math.sqrt(sse/(n-1));Preferences.debug("rms error = " + rms + "\n");*/

    }
    
    class FitdeHoog1 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog1(double[] time, double largestPole, double tol) {
            super(time, largestPole, tol);
        }

        /**
         * DOCUMENT ME!
         *
         * @param   realS  DOCUMENT ME!
         * @param   imagS  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[][] fitToLaplace(double realS, double[] imagS) {

            // The Laplace transform of J0(t) = (s*s + 1)**-1/2
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zmlt(realS,imagS[i],realS,imagS[i],realPart,imagPart);
            	realPart[0] = realPart[0] + 1.0;
            	zdiv(1.0, 0.0, realPart[0], imagPart[0], realPart2, imagPart2);
            	zsqrt(realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
        }

    }
	
	class FitExpModel extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitExpModel(double[] time, double largestPole, double tol) {
            super(time, largestPole, tol);
        }

        /**
         * DOCUMENT ME!
         *
         * @param   realS  DOCUMENT ME!
         * @param   imagS  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[][] fitToLaplace(double realS, double[] imagS) {

            // The Laplace transform of exp(at) = 1/(p-a)
            double[][] ans = new double[imagS.length][2];
            double a = 1.0;
            int i;
            double denom;
            double realDenom;
            realDenom = (realS - a) * (realS - a);

            for (i = 0; i < imagS.length; i++) {
                denom = realDenom + (imagS[i] * imagS[i]);

                // real part
                ans[i][0] = (realS - a) / denom;
                ans[i][1] = -imagS[i] / denom;
            }

            return ans;
        }

    }
	
	class FitPiessens1 extends InverseLaplace2 {

        /**
         * Creates a new FitSineModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  abscissa     DOCUMENT ME!
         * @param  relEps       DOCUMENT ME!
         * @param  absEps       DOCUMENT ME!
         * @param  result       DOCUMENT ME!
         * @param  estErr       DOCUMENT ME!
         * @param  evaluations  DOCUMENT ME!
         * @param  errStatus    DOCUMENT ME!
         */
        public FitPiessens1(double[] time, double abscissa, double relEps, double absEps, double[] result,
                            double[] estErr, int[] evaluations, int[] errStatus) {
            super(time, abscissa, relEps, absEps, result, estErr, evaluations, errStatus);

        }

        /**
         * DOCUMENT ME!
         *
         * @param  realIn   DOCUMENT ME!
         * @param  imagIn   DOCUMENT ME!
         * @param  realOut  DOCUMENT ME!
         * @param  imagOut  DOCUMENT ME!
         */
        public void fitToLaplace(double realIn, double imagIn, double[] realOut, double[] imagOut) {
        	
        	// The Laplace transform of J0(t) = (s*s + 1)**-1/2
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

        	zmlt(realIn,imagIn,realIn,imagIn,realPart,imagPart);
        	realPart[0] = realPart[0] + 1.0;
        	zdiv(1.0, 0.0, realPart[0], imagPart[0], realPart2, imagPart2);
        	zsqrt(realPart2[0], imagPart2[0], realOut, imagOut);

            return;
        }
    }
	
	class FitSineModel extends InverseLaplace2 {

        /**
         * Creates a new FitSineModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  abscissa     DOCUMENT ME!
         * @param  relEps       DOCUMENT ME!
         * @param  absEps       DOCUMENT ME!
         * @param  result       DOCUMENT ME!
         * @param  estErr       DOCUMENT ME!
         * @param  evaluations  DOCUMENT ME!
         * @param  errStatus    DOCUMENT ME!
         */
        public FitSineModel(double[] time, double abscissa, double relEps, double absEps, double[] result,
                            double[] estErr, int[] evaluations, int[] errStatus) {
            super(time, abscissa, relEps, absEps, result, estErr, evaluations, errStatus);

        }

        /**
         * DOCUMENT ME!
         *
         * @param  realIn   DOCUMENT ME!
         * @param  imagIn   DOCUMENT ME!
         * @param  realOut  DOCUMENT ME!
         * @param  imagOut  DOCUMENT ME!
         */
        public void fitToLaplace(double realIn, double imagIn, double[] realOut, double[] imagOut) {

            // 1/(s**2 + 1) is the Laplace transform of sin(t)
            double c, d;
            c = (realIn * realIn) - (imagIn * imagIn) + 1.0;
            d = (c * c) + (4.0 * realIn * realIn * imagIn * imagIn);
            realOut[0] = c / d;
            imagOut[0] = -2.0 * realIn * imagIn / d;

            return;
        }
    }
	
	class Fitqd1 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd1(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
            super(timePoints, endTime, largestPole, tol, matrixSizeParameter);
        }

        /**
         * DOCUMENT ME!
         *
         * @param   realS  DOCUMENT ME!
         * @param   imagS  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[][] fitToLaplace(double realS, double[] imagS) {
        	// The Laplace transform of J0(t) = (s*s + 1)**-1/2
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zmlt(realS,imagS[i],realS,imagS[i],realPart,imagPart);
            	realPart[0] = realPart[0] + 1.0;
            	zdiv(1.0, 0.0, realPart[0], imagPart[0], realPart2, imagPart2);
            	zsqrt(realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
        }
	}
	
	class FitExpModelqd extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public FitExpModelqd(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
            super(timePoints, endTime, largestPole, tol, matrixSizeParameter);
        }

        /**
         * DOCUMENT ME!
         *
         * @param   realS  DOCUMENT ME!
         * @param   imagS  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[][] fitToLaplace(double realS, double[] imagS) {

            // The Laplace transform of exp(at) = 1/(p-a)
            double[][] ans = new double[imagS.length][2];
            double a = 1.0;
            int i;
            double denom;
            double realDenom;
            realDenom = (realS - a) * (realS - a);

            for (i = 0; i < imagS.length; i++) {
                denom = realDenom + (imagS[i] * imagS[i]);

                // real part
                ans[i][0] = (realS - a) / denom;
                ans[i][1] = -imagS[i] / denom;
            }

            return ans;
        }

    }
	
	class FitWeeks1 extends InverseLaplaceWeeks {

        /**
         * Creates a new FitExpModelWeeks object.
         *
         * @param  time       DOCUMENT ME!
         * @param  nLaguerre  DOCUMENT ME!
         * @param  sig        DOCUMENT ME!
         * @param  b          DOCUMENT ME!
         */
        public FitWeeks1(double[] time, int nLaguerre, double sig, double b) {
            super(time, nLaguerre, sig, b);
        }

        /**
         * Creates a new FitExpModelWeeks object.
         *
         * @param  time       DOCUMENT ME!
         * @param  nLaguerre  DOCUMENT ME!
         * @param  sig0       DOCUMENT ME!
         * @param  sigmax     DOCUMENT ME!
         * @param  bmax       DOCUMENT ME!
         * @param  tols       DOCUMENT ME!
         * @param  tolb       DOCUMENT ME!
         */
        public FitWeeks1(double[] time, int nLaguerre, double sig0, double sigmax, double bmax, double tols,
                                double tolb) {
            super(time, nLaguerre, sig0, sigmax, bmax, tols, tolb);
        }

        /**
         * DOCUMENT ME!
         *
         * @param   realS  DOCUMENT ME!
         * @param   imagS  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[][] fitToLaplace(double realS, double[] imagS) {
        	// The Laplace transform of J0(t) = (s*s + 1)**-1/2
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zmlt(realS,imagS[i],realS,imagS[i],realPart,imagPart);
            	realPart[0] = realPart[0] + 1.0;
            	zdiv(1.0, 0.0, realPart[0], imagPart[0], realPart2, imagPart2);
            	zsqrt(realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
        }

    }
	
	class FitExpModelWeeks extends InverseLaplaceWeeks {

        /**
         * Creates a new FitExpModelWeeks object.
         *
         * @param  time       DOCUMENT ME!
         * @param  nLaguerre  DOCUMENT ME!
         * @param  sig        DOCUMENT ME!
         * @param  b          DOCUMENT ME!
         */
        public FitExpModelWeeks(double[] time, int nLaguerre, double sig, double b) {
            super(time, nLaguerre, sig, b);
        }

        /**
         * Creates a new FitExpModelWeeks object.
         *
         * @param  time       DOCUMENT ME!
         * @param  nLaguerre  DOCUMENT ME!
         * @param  sig0       DOCUMENT ME!
         * @param  sigmax     DOCUMENT ME!
         * @param  bmax       DOCUMENT ME!
         * @param  tols       DOCUMENT ME!
         * @param  tolb       DOCUMENT ME!
         */
        public FitExpModelWeeks(double[] time, int nLaguerre, double sig0, double sigmax, double bmax, double tols,
                                double tolb) {
            super(time, nLaguerre, sig0, sigmax, bmax, tols, tolb);
        }

        /**
         * DOCUMENT ME!
         *
         * @param   realS  DOCUMENT ME!
         * @param   imagS  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[][] fitToLaplace(double realS, double[] imagS) {

            // The Laplace transform of exp(at) = 1/(p-a)
            double[][] ans = new double[imagS.length][2];
            double a = -1.0;
            int i;
            double denom;
            double realDenom;
            realDenom = (realS - a) * (realS - a);

            for (i = 0; i < imagS.length; i++) {
                denom = realDenom + (imagS[i] * imagS[i]);

                // real part
                ans[i][0] = (realS - a) / denom;
                ans[i][1] = -imagS[i] / denom;
            }

            return ans;
        }

    }
	
	class FitModelWeeks3 extends InverseLaplaceWeeks {

        /**
         * Creates a new FitModelWeeks3 object.
         *
         * @param  time       DOCUMENT ME!
         * @param  nLaguerre  DOCUMENT ME!
         * @param  sig        DOCUMENT ME!
         * @param  b          DOCUMENT ME!
         */
        public FitModelWeeks3(double[] time, int nLaguerre, double sig, double b) {
            super(time, nLaguerre, sig, b);
        }

        /**
         * Creates a new FitModelWeeks3 object.
         *
         * @param  time       DOCUMENT ME!
         * @param  nLaguerre  DOCUMENT ME!
         * @param  sig0       DOCUMENT ME!
         * @param  sigmax     DOCUMENT ME!
         * @param  bmax       DOCUMENT ME!
         * @param  tols       DOCUMENT ME!
         * @param  tolb       DOCUMENT ME!
         */
        public FitModelWeeks3(double[] time, int nLaguerre, double sig0, double sigmax, double bmax, double tols,
                              double tolb) {
            super(time, nLaguerre, sig0, sigmax, bmax, tols, tolb);
        }

        /**
         * DOCUMENT ME!
         *
         * @param   realS  DOCUMENT ME!
         * @param   imagS  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[][] fitToLaplace(double realS, double[] imagS) {

            // The Laplace transform of exp(at) = 1/(p-a)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart;
            double imagPart;
            double[] br = new double[1];
            double[] bi = new double[1];

            for (i = 0; i < imagS.length; i++) {
                zsqrt(realS, imagS[i], br, bi);
                realPart = -4.0 * br[0];
                imagPart = -4.0 * bi[0];
                zexp(realPart, imagPart, br, bi);

                // real part
                ans[i][0] = br[0];
                ans[i][1] = bi[0];
            }

            br = null;
            bi = null;

            return ans;
        }

        


    }
}