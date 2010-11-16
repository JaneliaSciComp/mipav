package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.Preferences;

public class InverseLaplaceTest extends AlgorithmBase {
	// This is an implementation of the 16 test cases listed in Numerical Inversion of the Laplace Transform:
	// a Survey of and Comparison of Methods by Brian Davies and Brian Martin, Journal of Computational 
	// Physics, Vol. 33, pp. 1- 32, 1979.
	// Note that in Software for an Implementation of Weeks's Method for the Inverse Laplace Transform
	// Problem by B. S. Garbow, G. Giunta, J. N. Lyness, and A. Murli states that for
	// f(t) an entire function, cases #1, #3-#8, #13, and #16, the results are good.
	// Weeks method does not work where f(t) has a discontinuity (#10, #12), a sqrt(t) singularity at
	// the origin (#2, #9, #14), a log(t) singularity at the origin (#11), or an isolated essential
	// singularity at the origin(#15).
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
        FitdeHoog2 moddeHoog2;
        FitdeHoog3 moddeHoog3;
        FitdeHoog4 moddeHoog4;
        FitdeHoog5 moddeHoog5;
        
        double abscissaPiessens = 0.0;
        double relEpsPiessens = 1.0E-12;
        double absEpsPiessens = 1.0E-12;
        double[] resultPiessens = new double[n];
        double[] estErrPiessens = new double[n];
        int[] evaluationsPiessens = new int[n];
        int[] errStatusPiessens = new int[n];
        FitPiessens1 modPiessens1;
        FitPiessens2 modPiessens2;
        FitPiessens3 modPiessens3;
        FitPiessens4 modPiessens4;
        FitPiessens5 modPiessens5;
        
        // An estimate for the maximum of the real parts of the singularities
        // of F. If unknown, set largestPole = 0.0
        double largestPoleqd = 0.0;
        // numerical tolerance of approaching pole (default = 1.0e-9)
        double tolqd = 1.0e-9;
        int matrixSizeParameterqd = 3;
        // Must include t = 0 point in an array of evenly spaced points
        double tqd[] = new double[10*n+1];
        Fitqd1 modqd1;
        Fitqd2 modqd2;
        Fitqd3 modqd3;
        Fitqd4 modqd4;
        Fitqd5 modqd5;
        
        double pretWeeks[] = new double[1];
        double sig0Weeks = 0.0;
        double sigmaxWeeks = 30.0;
        double bmaxWeeks = 30.0;
        int nLaguerreWeeks = 32;
        double tolsWeeks = 1.0e-9;
        double tolbWeeks = 1.0e-9;
        double bWeeks;
        double sigWeeks;
        FitWeeks1 modPreWeeks1;
        FitWeeks1 modWeeks1;
        FitWeeks2 modPreWeeks2;
        FitWeeks2 modWeeks2;
        FitWeeks3 modPreWeeks3;
        FitWeeks3 modWeeks3;
        FitWeeks4 modPreWeeks4;
        FitWeeks4 modWeeks4;
        FitWeeks5 modPreWeeks5;
        FitWeeks5 modWeeks5;
        
        double[] timeFunction = null;
        double diff;
        double square;
        double sum;
        double expsum;
        double ledenom;
        double L;
        double Le;
        
        int test;
        
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
	        // 1 for 2n < t < (2n+1)
	        // 0 for (2n+1) < t < (2n+2)
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
	    
	    for (test = 1; test <= 5; test++) {
	    
		    switch (test) {
		    case 1:
	    	    Preferences.debug("F1(s) = (s*s + 1)**-1/2\n");
		        Preferences.debug("f1(t) = J0(t)\n\n");
		        break;
		    case 2:
		    	Preferences.debug("F2(s) = (s**-1/2)*exp(-p**-1)\n");
		    	Preferences.debug("f2(t) = cos(2*sqrt(t))/sqrt(PI*t)\n");
		    	break;
		    case 3:
		    	Preferences.debug("F3(s) = 1.0/(s + 0.5)\n");
		    	Preferences.debug("f3(t) = exp(-0.5*t)\n");
		    	break;
		    case 4:
		    	Preferences.debug("F4(s) = 1.0/((s + 0.2)**2 + 1)\n");
		    	Preferences.debug("f4(t) = exp(-0.2*t)*sin(t)\n");
		    	break;
		    case 5:
		    	Preferences.debug("F5(s) = 1.0/s\n");
		    	Preferences.debug("f5(t) = 1.0\n");
		    	break;
		    case 6:
		    	Preferences.debug("F6(s) = 1/(s*s)\n");
		    	Preferences.debug("f6(t) = t\n");
		    	break;
		    case 7:
		    	Preferences.debug("F7(s) = (s + 1)**-2\n");
		    	Preferences.debug("f7(t) = t*exp(-t)\n");
		    	break;
		    case 8:
		    	Preferences.debug("F8(s) = 1.0/(s*s + 1)\n");
		    	Preferences.debug("f8(t) = sin(t)\n");
		    	break;
		    case 9:
		    	Preferences.debug("F9(s) = 1.0/sqrt(s)\n");
		    	Preferences.debug("f9(t) = 1.0/sqrt(PI*t)\n");
		    	break;
		    case 10:
		    	Preferences.debug("F10(s) = (s**-1)*exp(-5*s)\n");
		    	Preferences.debug("f10(t) = step from 0 to 1 at t = 5\n");
		    	break;
		    case 11:
		    	Preferences.debug("F11(s) = (s**-1)*ln(s)\n");
		    	Preferences.debug("f11(t) = -gamma - ln(t)\n");
		    	break;
		    case 12:
		    	Preferences.debug("F12(s) = (s*(1 + exp(-s)))**-1\n");
		    	Preferences.debug("f12(t) = square wave = 1 for 2*n < t < 2*n + 1\n");
		    	Preferences.debug("f12(t) = 0 for 2*n + 1 < t < 2*n + 2\n");
		    	break;
		    case 13:
		    	Preferences.debug("F13(s) = (s*s - 1.0)*((s*s + 1.0)**-2)\n");
		    	Preferences.debug("f13(t) = t*cos(t)\n");
		    	break;
		    case 14:
		    	Preferences.debug("F14(s) = sqrt(s + 0.5) - sqrt(s + 0.25)\n");
		    	Preferences.debug("f14(t) = (exp(-0.25*t) - exp(-0.5*t))/sqrt(4.0*PI*t*t*t)\n");
		    	break;
		    case 15:
		    	Preferences.debug("F15(s) = exp(-4.0*sqrt(s))\n");
		    	Preferences.debug("f15(t) = 2.0*exp(-4.0/t)/sqrt(PI*t*t*t)\n");
		    	break;
		    case 16:
		    	Preferences.debug("F16(s) = arctan(1.0/s)\n");
		    	Preferences.debug("f16(t) = sin(t)/t\n");
		    	break;
		    } // switch (test)
		    
		    Preferences.debug("deHoog algorithm\n");
		    // test 1: L = 1.695E-8 Le = 1.493E-10
		    // test 2: L = 4.744E-9 Le = 3.864E-10
		    // test 3: L = 2.255E-10 Le = 6.113E-10
		    // test 4: L = 6.479E-9 Le = 8.911E-11
		    // test 5: L = 1.089E-8 Le = 9.231E-11
		    
		    switch (test) {
		    case 1:
		        moddeHoog1 = new FitdeHoog1(t, largestPoledeHoog, toldeHoog);
	            moddeHoog1.driver();
	            timeFunction = moddeHoog1.getTimeFunction();
	            break;
		    case 2:
		    	moddeHoog2 = new FitdeHoog2(t, largestPoledeHoog, toldeHoog);
	            moddeHoog2.driver();
	            timeFunction = moddeHoog2.getTimeFunction();
	            break;
		    case 3:
		    	moddeHoog3 = new FitdeHoog3(t, -0.5, toldeHoog);
	            moddeHoog3.driver();
	            timeFunction = moddeHoog3.getTimeFunction();
	            break;
		    case 4:
		    	moddeHoog4 = new FitdeHoog4(t, largestPoledeHoog, toldeHoog);
	            moddeHoog4.driver();
	            timeFunction = moddeHoog4.getTimeFunction();
	            break;
		    case 5:
		    	moddeHoog5 = new FitdeHoog5(t, largestPoledeHoog, toldeHoog);
	            moddeHoog5.driver();
	            timeFunction = moddeHoog5.getTimeFunction();
	            break;
		    }
	
	        sum = 0.0;
	        expsum = 0.0;
	        for (i = 0; i < n; i++) {
	            Preferences.debug("time = " + t[i] + " inverseLaplaceFunction = " + timeFunction[i] + " trueFunction = " +
	                              ftrue[test-1][i] + "\n");
	            diff = ftrue[test-1][i] - timeFunction[i];
	            square = diff * diff;
	            sum = sum + square;
	            expsum = expsum + square * Math.exp(-t[i]);
	        } 
	        L = Math.sqrt(sum/n);
	        Le = Math.sqrt(expsum/ledenom);
	        Preferences.debug("Root mean square error = " + L + "\n");
	        Preferences.debug("Exponential weighted root mean square error = " + Le + "\n");
	        
	        Preferences.debug("Piessens algorithm\n");
	        // test 1: L = 3.905E-14 Le = 3.216E-14
	        // test 2: L = 1.069E-13 Le = 6.240E-14
	        // test 3: L = 4.066E-14 Le = 5.748E-14
	        // test 4: L = 6.309E-14 Le = 4.352E-14
	        // test 5: L = 2.599E-14 Le = 2.498E-14
	        
	        switch (test) {
	        case 1:
		        modPiessens1 = new FitPiessens1(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
		        		                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
		        modPiessens1.driver();
		        break;
	        case 2:
	        	modPiessens2 = new FitPiessens2(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens2.driver();
                break;
	        case 3:
	        	modPiessens3 = new FitPiessens3(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens3.driver();
                break;
	        case 4:
	        	modPiessens4 = new FitPiessens4(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens4.driver();
                break;
	        case 5:
	        	modPiessens5 = new FitPiessens5(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens5.driver();
                break;
	        }
	
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
	                                  ftrue[test-1][i] + "\n");
	                diff = ftrue[test-1][i] - resultPiessens[i];
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
	        // For test 1:
	        // Increasing number of evenly spaced time points from 31 to 301 takes
	        // L = 1.300E-6 Le = 4.333E-6 to L = 1.876E-10 Le = 5.679E-10 for 
	        // largestPoleqd = 0.0, tolqd = 1.0E-9, matrixSizeParameterqd = 3.
	        // Taking tolqd to 1.0E-10 gives L = 1.699E-10 Le = 5.829E-10
	        // test 2: L = 5.726E-10 Le = 1.953E-9
	        // test 3: L = 1.742E-10 Le = 3.668E-10
	        // test 4: L = 6.345E-13 Le = 6.744E-13
	        // test 5: L = 9.819E-10 Le = 7.686E-10
	        
	        switch(test) {
	        case 1:
		        modqd1 = new Fitqd1(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd1.driver();
		        timeFunction = modqd1.getTimeFunction();
		        break;
	        case 2:
	        	modqd2 = new Fitqd2(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd2.driver();
		        timeFunction = modqd2.getTimeFunction();
		        break;
	        case 3:
	        	modqd3 = new Fitqd3(10*n+1, tqd[10*n], -0.5, tolqd, matrixSizeParameterqd);
		        modqd3.driver();
		        timeFunction = modqd3.getTimeFunction();
		        break;
	        case 4:
	        	modqd4 = new Fitqd4(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd4.driver();
		        timeFunction = modqd4.getTimeFunction();
		        break;
	        case 5:
	        	modqd5 = new Fitqd5(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd5.driver();
		        timeFunction = modqd5.getTimeFunction();
		        break;
	        }
	        
	        sum = 0.0;
	        expsum = 0.0;
	        for (i = 0; i < n; i++) {
	            Preferences.debug("time = " + t[i] + " inverseLaplaceFunction = " + timeFunction[10*(i+1)] + " trueFunction = " +
	                              ftrue[test-1][i] + "\n");
	            diff = ftrue[test-1][i] - timeFunction[10*(i+1)];
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
	        // test 1: Typical is L = 8.899E-8 Le = 3.166E-10
	        // test 2: Does not work with sqrt(t) singularity at the origin
	        // test 3: L = 1.731E-14 Le = 1.138E-16
	        // test 4: nLaguerreWeeks 32: L = 5.245E7 Le = 129433
	        //         nLaguerreWeeks 64: L = 18.60 Le = 0.05301
	        //         nLaguerreWeeks 512: L = 6.5688E-6 Le = 1.629E-8
	        //         nLaguerreWeeks 1024: L = 2.969E-7 Le = 8.012E-10
	        //         nLaguerreWeeks 2048: L = 5.084E-4 Le = 1.303E-6
	        // test 5: L = 3.023E-13 Le = 1.305E-15 
	        
	        pretWeeks[0] = 0.5;
	        switch(test) {
	        case 1:
		        modPreWeeks1 = new FitWeeks1(pretWeeks, nLaguerreWeeks, sig0Weeks, sigmaxWeeks, bmaxWeeks, tolsWeeks, tolbWeeks);
		        modPreWeeks1.wpar2();
		        bWeeks = modPreWeeks1.getBOpt();
		        sigWeeks = modPreWeeks1.getSigOpt();
		        Preferences.debug("bWeeks = " + bWeeks + " sigWeeks = " + sigWeeks + "\n");
		        
		        modWeeks1 = new FitWeeks1(t, nLaguerreWeeks, sigWeeks, bWeeks);
		        modWeeks1.driver();
		        timeFunction = modWeeks1.getTimeFunction();
		        break;
	        case 2:
	        	Preferences.debug("InverseLaplaceWeeks does not work due with sqrt(t) singularity at the origin\n");
	        	modPreWeeks2 = new FitWeeks2(pretWeeks, nLaguerreWeeks, sig0Weeks, sigmaxWeeks, bmaxWeeks, tolsWeeks, tolbWeeks);
		        modPreWeeks2.wpar2();
		        bWeeks = modPreWeeks2.getBOpt();
		        sigWeeks = modPreWeeks2.getSigOpt();
		        Preferences.debug("bWeeks = " + bWeeks + " sigWeeks = " + sigWeeks + "\n");
		        
		        modWeeks2 = new FitWeeks2(t, nLaguerreWeeks, sigWeeks, bWeeks);
		        modWeeks2.driver();
		        timeFunction = modWeeks2.getTimeFunction();
		        break;
	        case 3:
	        	modPreWeeks3 = new FitWeeks3(pretWeeks, nLaguerreWeeks, sig0Weeks, sigmaxWeeks, bmaxWeeks, tolsWeeks, tolbWeeks);
			    modPreWeeks3.wpar2();
			    bWeeks = modPreWeeks3.getBOpt();
			    sigWeeks = modPreWeeks3.getSigOpt();
			    Preferences.debug("bWeeks = " + bWeeks + " sigWeeks = " + sigWeeks + "\n");
			        
			    modWeeks3 = new FitWeeks3(t, nLaguerreWeeks, sigWeeks, bWeeks);
			    modWeeks3.driver();
			    timeFunction = modWeeks3.getTimeFunction();
	        case 4:
	        	modPreWeeks4 = new FitWeeks4(pretWeeks, 1024, sig0Weeks, sigmaxWeeks, bmaxWeeks, tolsWeeks, tolbWeeks);
		        modPreWeeks4.wpar2();
		        bWeeks = modPreWeeks4.getBOpt();
		        sigWeeks = modPreWeeks4.getSigOpt();
		        Preferences.debug("bWeeks = " + bWeeks + " sigWeeks = " + sigWeeks + "\n");
		        
		        modWeeks4 = new FitWeeks4(t, 1024, sigWeeks, bWeeks);
		        modWeeks4.driver();
		        timeFunction = modWeeks4.getTimeFunction();
		        break;
	        case 5:
	        	modPreWeeks5 = new FitWeeks5(pretWeeks, nLaguerreWeeks, sig0Weeks, sigmaxWeeks, bmaxWeeks, tolsWeeks, tolbWeeks);
			    modPreWeeks5.wpar2();
			    bWeeks = modPreWeeks5.getBOpt();
			    sigWeeks = modPreWeeks5.getSigOpt();
			    Preferences.debug("bWeeks = " + bWeeks + " sigWeeks = " + sigWeeks + "\n");
			        
			    modWeeks5 = new FitWeeks5(t, nLaguerreWeeks, sigWeeks, bWeeks);
			    modWeeks5.driver();
			    timeFunction = modWeeks5.getTimeFunction();
	        }
	        
	        sum = 0.0;
	        expsum = 0.0;
	        for (i = 0; i < n; i++) {
	            Preferences.debug("time = " + t[i] + " inverseLaplaceFunction = " + timeFunction[i] + " trueFunction = " +
	                              ftrue[test-1][i] + "\n");
	            diff = ftrue[test-1][i] - timeFunction[i];
	            square = diff * diff;
	            sum = sum + square;
	            expsum = expsum + square * Math.exp(-t[i]);
	        } 
	        L = Math.sqrt(sum/n);
	        Le = Math.sqrt(expsum/ledenom);
	        Preferences.debug("Root mean square error = " + L + "\n");
	        Preferences.debug("Exponential weighted root mean square error = " + Le + "\n");
        
	    } // for (test = 1; test <= 2; test++)
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
    
    class FitdeHoog2 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog2(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of cos(2.0*sqrt(t))/sqrt(PI*t) = (p**-1/2)*exp(-1/p)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];
            double realPart4[] = new double[1];
            double imagPart4[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zdiv(1.0, 0.0, realS, imagS[i], realPart, imagPart);
            	zsqrt(realPart[0], imagPart[0], realPart2, imagPart2);
            	zexp(-realPart[0], -imagPart[0], realPart3, imagPart3);
            	zmlt(realPart2[0], imagPart2[0], realPart3[0], imagPart3[0], realPart4, imagPart4);
                // real part
                ans[i][0] = realPart4[0];
                ans[i][1] = imagPart4[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog3 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog3(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of exp(-t/2) = (p + 1/2)**-1
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	realPart[0] = realS + 0.5;
            	zdiv(1.0, 0.0, realPart[0], imagS[i], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog4 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog4(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of exp(-0.2*t)*sin(t) = ((s + 0.2)**2 + 1)**-1
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	realPart[0] = realS + 0.2;
            	zmlt(realPart[0], imagS[i], realPart[0], imagS[i], realPart2, imagPart2);
            	realPart2[0] = realPart2[0] + 1.0;
            	zdiv(1.0, 0.0, realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog5 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog5(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of 1 = 1/s
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zdiv(1.0, 0.0, realS, imagS[i], realPart, imagPart);

                // real part
                ans[i][0] = realPart[0];
                ans[i][1] = imagPart[0];
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
	
	class FitPiessens2 extends InverseLaplace2 {

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
        public FitPiessens2(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	
        	// The Laplace transform of cos(2.0*sqrt(t))/sqrt(PI*t) = (p**-1/2)*exp(-1/p)
        	double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];

        	zdiv(1.0, 0.0, realIn, imagIn, realPart, imagPart);
        	zsqrt(realPart[0], imagPart[0], realPart2, imagPart2);
        	zexp(-realPart[0], -imagPart[0], realPart3, imagPart3);
        	zmlt(realPart2[0], imagPart2[0], realPart3[0], imagPart3[0], realOut, imagOut);
               
            return;
        }
    }
	
	class FitPiessens3 extends InverseLaplace2 {

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
        public FitPiessens3(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	
        	// The Laplace transform of exp(-t/2) = (p + 1/2)**-1
            double realPart[] = new double[1];

        	realPart[0] = realIn + 0.5;
        	zdiv(1.0, 0.0, realPart[0], imagIn, realOut, imagOut);

            return;
        }
    }
	
	class FitPiessens4 extends InverseLaplace2 {

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
        public FitPiessens4(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	// The Laplace transform of exp(-0.2*t)*sin(t) = ((s + 0.2)**2 + 1)**-1
            double realPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

        	realPart[0] = realIn + 0.2;
        	zmlt(realPart[0], imagIn, realPart[0], imagIn, realPart2, imagPart2);
        	realPart2[0] = realPart2[0] + 1.0;
        	zdiv(1.0, 0.0, realPart2[0], imagPart2[0], realOut, imagOut);

            return;
        }
    }
	
	class FitPiessens5 extends InverseLaplace2 {

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
        public FitPiessens5(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	// The Laplace transform of 1 = 1/s
            
        	zdiv(1.0, 0.0, realIn, imagIn, realOut, imagOut);

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
	
	class Fitqd2 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd2(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of cos(2.0*sqrt(t))/sqrt(PI*t) = (p**-1/2)*exp(-1/p)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];
            double realPart4[] = new double[1];
            double imagPart4[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zdiv(1.0, 0.0, realS, imagS[i], realPart, imagPart);
            	zsqrt(realPart[0], imagPart[0], realPart2, imagPart2);
            	zexp(-realPart[0], -imagPart[0], realPart3, imagPart3);
            	zmlt(realPart2[0], imagPart2[0], realPart3[0], imagPart3[0], realPart4, imagPart4);
                // real part
                ans[i][0] = realPart4[0];
                ans[i][1] = imagPart4[0];
            }

            return ans;
        	
        }
	}
	
	class Fitqd3 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd3(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of exp(-t/2) = (p + 1/2)**-1
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	realPart[0] = realS + 0.5;
            	zdiv(1.0, 0.0, realPart[0], imagS[i], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
        	
        }
	}
	
	class Fitqd4 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd4(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of exp(-0.2*t)*sin(t) = ((s + 0.2)**2 + 1)**-1
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	realPart[0] = realS + 0.2;
            	zmlt(realPart[0], imagS[i], realPart[0], imagS[i], realPart2, imagPart2);
            	realPart2[0] = realPart2[0] + 1.0;
            	zdiv(1.0, 0.0, realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
        	
        }
	}
	
	class Fitqd5 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd5(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of 1 = 1/s
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zdiv(1.0, 0.0, realS, imagS[i], realPart, imagPart);

                // real part
                ans[i][0] = realPart[0];
                ans[i][1] = imagPart[0];
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
	
	class FitWeeks2 extends InverseLaplaceWeeks {

        /**
         * Creates a new FitExpModelWeeks object.
         *
         * @param  time       DOCUMENT ME!
         * @param  nLaguerre  DOCUMENT ME!
         * @param  sig        DOCUMENT ME!
         * @param  b          DOCUMENT ME!
         */
        public FitWeeks2(double[] time, int nLaguerre, double sig, double b) {
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
        public FitWeeks2(double[] time, int nLaguerre, double sig0, double sigmax, double bmax, double tols,
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
        	// The Laplace transform of cos(2.0*sqrt(t))/sqrt(PI*t) = (p**-1/2)*exp(-1/p)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];
            double realPart4[] = new double[1];
            double imagPart4[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zdiv(1.0, 0.0, realS, imagS[i], realPart, imagPart);
            	zsqrt(realPart[0], imagPart[0], realPart2, imagPart2);
            	zexp(-realPart[0], -imagPart[0], realPart3, imagPart3);
            	zmlt(realPart2[0], imagPart2[0], realPart3[0], imagPart3[0], realPart4, imagPart4);
                // real part
                ans[i][0] = realPart4[0];
                ans[i][1] = imagPart4[0];
            }

            return ans;
        	
        }

    }
	
	class FitWeeks3 extends InverseLaplaceWeeks {

        /**
         * Creates a new FitExpModelWeeks object.
         *
         * @param  time       DOCUMENT ME!
         * @param  nLaguerre  DOCUMENT ME!
         * @param  sig        DOCUMENT ME!
         * @param  b          DOCUMENT ME!
         */
        public FitWeeks3(double[] time, int nLaguerre, double sig, double b) {
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
        public FitWeeks3(double[] time, int nLaguerre, double sig0, double sigmax, double bmax, double tols,
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
        	// The Laplace transform of exp(-t/2) = (p + 1/2)**-1
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	realPart[0] = realS + 0.5;
            	zdiv(1.0, 0.0, realPart[0], imagS[i], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
        	
        }

    }
	
	class FitWeeks4 extends InverseLaplaceWeeks {

        /**
         * Creates a new FitExpModelWeeks object.
         *
         * @param  time       DOCUMENT ME!
         * @param  nLaguerre  DOCUMENT ME!
         * @param  sig        DOCUMENT ME!
         * @param  b          DOCUMENT ME!
         */
        public FitWeeks4(double[] time, int nLaguerre, double sig, double b) {
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
        public FitWeeks4(double[] time, int nLaguerre, double sig0, double sigmax, double bmax, double tols,
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
        	// The Laplace transform of exp(-0.2*t)*sin(t) = ((s + 0.2)**2 + 1)**-1
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	realPart[0] = realS + 0.2;
            	zmlt(realPart[0], imagS[i], realPart[0], imagS[i], realPart2, imagPart2);
            	realPart2[0] = realPart2[0] + 1.0;
            	zdiv(1.0, 0.0, realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
        	
        }

    }
	
	class FitWeeks5 extends InverseLaplaceWeeks {

        /**
         * Creates a new FitExpModelWeeks object.
         *
         * @param  time       DOCUMENT ME!
         * @param  nLaguerre  DOCUMENT ME!
         * @param  sig        DOCUMENT ME!
         * @param  b          DOCUMENT ME!
         */
        public FitWeeks5(double[] time, int nLaguerre, double sig, double b) {
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
        public FitWeeks5(double[] time, int nLaguerre, double sig0, double sigmax, double bmax, double tols,
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
        	// The Laplace transform of 1 = 1/s
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zdiv(1.0, 0.0, realS, imagS[i], realPart, imagPart);

                // real part
                ans[i][0] = realPart[0];
                ans[i][1] = imagPart[0];
            }

            return ans;
        	
        }

    }
	
	
}