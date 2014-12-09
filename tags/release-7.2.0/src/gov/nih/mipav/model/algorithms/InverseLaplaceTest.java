package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.Preferences;

public class InverseLaplaceTest extends AlgorithmBase {
	/** This is an implementation of the 16 test cases listed in Numerical Inversion of the Laplace Transform:
	    a Survey of and Comparison of Methods by Brian Davies and Brian Martin, Journal of Computational 
	    Physics, Vol. 33, pp. 1- 32, 1979.
	 */

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
        FitdeHoog6 moddeHoog6;
        FitdeHoog7 moddeHoog7;
        FitdeHoog8 moddeHoog8;
        FitdeHoog9 moddeHoog9;
        FitdeHoog10 moddeHoog10;
        FitdeHoog11 moddeHoog11;
        FitdeHoog12 moddeHoog12;
        FitdeHoog13 moddeHoog13;
        FitdeHoog14 moddeHoog14;
        FitdeHoog15 moddeHoog15;
        FitdeHoog16 moddeHoog16;
        
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
        FitPiessens6 modPiessens6;
        FitPiessens7 modPiessens7;
        FitPiessens8 modPiessens8;
        FitPiessens9 modPiessens9;
        FitPiessens10 modPiessens10;
        FitPiessens11 modPiessens11;
        FitPiessens12 modPiessens12;
        FitPiessens13 modPiessens13;
        FitPiessens14 modPiessens14;
        FitPiessens15 modPiessens15;
        FitPiessens16 modPiessens16;
        
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
        Fitqd6 modqd6;
        Fitqd7 modqd7;
        Fitqd8 modqd8;
        Fitqd9 modqd9;
        Fitqd10 modqd10;
        Fitqd11 modqd11;
        Fitqd12 modqd12;
        Fitqd13 modqd13;
        Fitqd14 modqd14;
        Fitqd15 modqd15;
        Fitqd16 modqd16;
        
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
	    
	    for (test = 1; test <= 16; test++) {
	    
		    switch (test) {
		    case 1:
	    	    Preferences.debug("F1(s) = (s*s + 1)**-1/2\n", Preferences.DEBUG_ALGORITHM);
		        Preferences.debug("f1(t) = J0(t)\n\n", Preferences.DEBUG_ALGORITHM);
		        break;
		    case 2:
		    	Preferences.debug("F2(s) = (s**-1/2)*exp(-p**-1)\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f2(t) = cos(2*sqrt(t))/sqrt(PI*t)\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 3:
		    	Preferences.debug("F3(s) = 1.0/(s + 0.5)\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f3(t) = exp(-0.5*t)\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 4:
		    	Preferences.debug("F4(s) = 1.0/((s + 0.2)**2 + 1)\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f4(t) = exp(-0.2*t)*sin(t)\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 5:
		    	Preferences.debug("F5(s) = 1.0/s\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f5(t) = 1.0\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 6:
		    	Preferences.debug("F6(s) = 1/(s*s)\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f6(t) = t\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 7:
		    	Preferences.debug("F7(s) = (s + 1)**-2\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f7(t) = t*exp(-t)\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 8:
		    	Preferences.debug("F8(s) = 1.0/(s*s + 1)\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f8(t) = sin(t)\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 9:
		    	Preferences.debug("F9(s) = 1.0/sqrt(s)\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f9(t) = 1.0/sqrt(PI*t)\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 10:
		    	Preferences.debug("F10(s) = (s**-1)*exp(-5*s)\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f10(t) = step from 0 to 1 at t = 5\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 11:
		    	Preferences.debug("F11(s) = (s**-1)*ln(s)\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f11(t) = -gamma - ln(t)\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 12:
		    	Preferences.debug("F12(s) = (s*(1 + exp(-s)))**-1\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f12(t) = square wave = 1 for 2*n < t < 2*n + 1\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f12(t) = 0 for 2*n + 1 < t < 2*n + 2\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 13:
		    	Preferences.debug("F13(s) = (s*s - 1.0)*((s*s + 1.0)**-2)\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f13(t) = t*cos(t)\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 14:
		    	Preferences.debug("F14(s) = sqrt(s + 0.5) - sqrt(s + 0.25)\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f14(t) = (exp(-0.25*t) - exp(-0.5*t))/sqrt(4.0*PI*t*t*t)\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 15:
		    	Preferences.debug("F15(s) = exp(-4.0*sqrt(s))\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f15(t) = 2.0*exp(-4.0/t)/sqrt(PI*t*t*t)\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    case 16:
		    	Preferences.debug("F16(s) = arctan(1.0/s)\n", Preferences.DEBUG_ALGORITHM);
		    	Preferences.debug("f16(t) = sin(t)/t\n", Preferences.DEBUG_ALGORITHM);
		    	break;
		    } // switch (test)
		    
		    Preferences.debug("deHoog algorithm\n", Preferences.DEBUG_ALGORITHM);
		    // test 1: L = 1.695E-8 Le = 1.493E-10
		    // test 2: L = 4.744E-9 Le = 3.864E-10
		    // test 3: L = 2.255E-10 Le = 6.113E-10
		    // test 4: L = 6.479E-9 Le = 8.911E-11
		    // test 5: L = 1.089E-8 Le = 9.231E-11
		    // test 6: L = 1.709E-8 Le = 2.175E-10
		    // test 7: L = 6.052E-9 Le = 1.274E-10
		    // test 8: L = 2.123E-8 Le = 7.364E-11
		    // test 9: L = 2.042E-8 Le = 1.718E-10
		    // test 10: L = 0.1098 Le = 0.03976
		    //          Values good except for 0.3985 at t = 5.0
		    // test 11: L = 2.419E-9 Le = 2.506E-10
		    // test 12: L = 0.3619 Le = 0.2655
		    // test 13: L = 2.735E-9 Le = 1.702E-10
		    // test 14: L = 1.726E-9 Le = 9.355E-11
		    // test 15: L = 2.318E-10 Le = 2.000E-11
		    // test 16: L = 5.066E-9 Le = 1.401E-10
		    
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
		    case 6:
		    	moddeHoog6 = new FitdeHoog6(t, largestPoledeHoog, toldeHoog);
	            moddeHoog6.driver();
	            timeFunction = moddeHoog6.getTimeFunction();
	            break;
		    case 7:
		    	moddeHoog7 = new FitdeHoog7(t, largestPoledeHoog, toldeHoog);
	            moddeHoog7.driver();
	            timeFunction = moddeHoog7.getTimeFunction();
	            break;
		    case 8:
		    	moddeHoog8 = new FitdeHoog8(t, largestPoledeHoog, toldeHoog);
	            moddeHoog8.driver();
	            timeFunction = moddeHoog8.getTimeFunction();
	            break;
		    case 9:
		    	moddeHoog9 = new FitdeHoog9(t, largestPoledeHoog, toldeHoog);
	            moddeHoog9.driver();
	            timeFunction = moddeHoog9.getTimeFunction();
	            break;
		    case 10:
		    	moddeHoog10 = new FitdeHoog10(t, largestPoledeHoog, toldeHoog);
	            moddeHoog10.driver();
	            timeFunction = moddeHoog10.getTimeFunction();
	            break;
		    case 11:
		    	moddeHoog11 = new FitdeHoog11(t, largestPoledeHoog, toldeHoog);
	            moddeHoog11.driver();
	            timeFunction = moddeHoog11.getTimeFunction();
	            break;
		    case 12:
		    	moddeHoog12 = new FitdeHoog12(t, largestPoledeHoog, toldeHoog);
	            moddeHoog12.driver();
	            timeFunction = moddeHoog12.getTimeFunction();
	            break;
		    case 13:
		    	moddeHoog13 = new FitdeHoog13(t, largestPoledeHoog, toldeHoog);
	            moddeHoog13.driver();
	            timeFunction = moddeHoog13.getTimeFunction();
	            break;
		    case 14:
		    	moddeHoog14 = new FitdeHoog14(t, largestPoledeHoog, toldeHoog);
	            moddeHoog14.driver();
	            timeFunction = moddeHoog14.getTimeFunction();
	            break;
		    case 15:
		    	moddeHoog15 = new FitdeHoog15(t, largestPoledeHoog, toldeHoog);
	            moddeHoog15.driver();
	            timeFunction = moddeHoog15.getTimeFunction();
	            break;
		    case 16:
		    	moddeHoog16 = new FitdeHoog16(t, largestPoledeHoog, toldeHoog);
	            moddeHoog16.driver();
	            timeFunction = moddeHoog16.getTimeFunction();
	            break;
		    }
	
	        sum = 0.0;
	        expsum = 0.0;
	        for (i = 0; i < n; i++) {
	            Preferences.debug("time = " + t[i] + " inverseLaplaceFunction = " + timeFunction[i] + " trueFunction = " +
	                              ftrue[test-1][i] + "\n", Preferences.DEBUG_ALGORITHM);
	            diff = ftrue[test-1][i] - timeFunction[i];
	            square = diff * diff;
	            sum = sum + square;
	            expsum = expsum + square * Math.exp(-t[i]);
	        } 
	        L = Math.sqrt(sum/n);
	        Le = Math.sqrt(expsum/ledenom);
	        Preferences.debug("Root mean square error = " + L + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("Exponential weighted root mean square error = " + Le + "\n", Preferences.DEBUG_ALGORITHM);
	        
	        Preferences.debug("Piessens algorithm\n", Preferences.DEBUG_ALGORITHM);
	        // test 1: L = 3.905E-14 Le = 3.216E-14
	        // test 2: L = 1.069E-13 Le = 6.240E-14
	        // test 3: L = 4.066E-14 Le = 5.748E-14
	        // test 4: L = 6.309E-14 Le = 4.352E-14
	        // test 5: L = 2.599E-14 Le = 2.498E-14
	        // test 6: L = 1.729E-13 Le = 3.880E-14
	        // test 7: L = 1.799E-13 Le = 2.037E-13
	        // test 8: L = 4.202E-14 Le = 3.609E-14
	        // test 9: L = 5.664E-14 Le = 1.231E-13
	        // test 10: For times 0.5 to 5.0 computations terminated
	        //          Maximum bound on Laplace evaluations was reached
	        // test 11: L = 1.463E-13 Le = 5.781E-14
	        // test 12: For times 1.0 to 9.5 except for 2.5 computations terminated
	        //          Maximum bound on Laplace evaluations was reached
	        // test 13: L = 2.285E-13 Le = 4.760E-14
	        // test 14: L = 1.223E-14 Le = 1.909E-14
	        // test 15: L = 5.748E-14 Le = 5.310E-14
	        // test 16: L = 2.979E-14 Le = 3.062E-14
	        
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
	        case 6:
	        	modPiessens6 = new FitPiessens6(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens6.driver();
                break;
	        case 7:
	        	modPiessens7 = new FitPiessens7(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens7.driver();
                break;
	        case 8:
	        	modPiessens8 = new FitPiessens8(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens8.driver();
                break;
	        case 9:
	        	modPiessens9 = new FitPiessens9(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens9.driver();
                break;
	        case 10:
	        	modPiessens10 = new FitPiessens10(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens10.driver();
                break;
	        case 11:
	        	modPiessens11 = new FitPiessens11(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens11.driver();
                break;
	        case 12:
	        	modPiessens12 = new FitPiessens12(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens12.driver();
                break;
	        case 13:
	        	modPiessens13 = new FitPiessens13(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens13.driver();
                break;
	        case 14:
	        	modPiessens14 = new FitPiessens14(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens14.driver();
                break;
	        case 15:
	        	modPiessens15 = new FitPiessens15(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens15.driver();
                break;
	        case 16:
	        	modPiessens16 = new FitPiessens16(t, abscissaPiessens, relEpsPiessens, absEpsPiessens,
                        resultPiessens, estErrPiessens, evaluationsPiessens, errStatusPiessens);
                modPiessens16.driver();
                break;
	        }
	
	        sum = 0.0;
	        expsum = 0.0;
	        for (i = 0; i < n; i++) {
	
	            if (errStatusPiessens[i] == 2) {
	                Preferences.debug("time[" + i + "] is illegally <= 0\n", Preferences.DEBUG_ALGORITHM);
	            } else if (errStatusPiessens[i] == 1) {
	                Preferences.debug("time[" + i + "] had computations terminated\n", Preferences.DEBUG_ALGORITHM);
	                Preferences.debug("The maximum bound on Laplace evaluations was reached\n", Preferences.DEBUG_ALGORITHM);
	            } else {
	                Preferences.debug("time = " + t[i] + " inverseLaplaceFunction = " + resultPiessens[i] + " trueFunction = " +
	                                  ftrue[test-1][i] + "\n", Preferences.DEBUG_ALGORITHM);
	                diff = ftrue[test-1][i] - resultPiessens[i];
	                square = diff * diff;
	                sum = sum + square;
	                expsum = expsum + square * Math.exp(-t[i]);
	
	            }
	        } // for (i = 0; i < n; i++)
	        L = Math.sqrt(sum/n);
	        Le = Math.sqrt(expsum/ledenom);
	        Preferences.debug("Root mean square error = " + L + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("Exponential weighted root mean square error = " + Le + "\n", Preferences.DEBUG_ALGORITHM);
	        
	        Preferences.debug("Brancik algorithm\n", Preferences.DEBUG_ALGORITHM);
	        
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
	        // test 6: L = 3.809E-8 Le = 3.138E-8
	        // test 7: L = 2.280E-13 Le = 5.185E-13
	        // test 8: L = 6.915E-10 Le = 6.081E-10
	        // test 9: L = 6.042E-10 Le = 2.055E-9
	        // test 10: L = 0.09160 Le = 0.0033
	        //          Values good except for 0.4982 at t = 5.0.
	        // test 11: L = 4.345E-9 Le = 5.477E-9
	        // test 12: L = 0.3535 Le = 0.3072
	        // test 13: L = 2.743E-8 Le = 2.485E-8
	        // test 14: L = 7.641E-11 Le = 2.604E-10
	        // test 15: L = 4.510E-12 Le = 5.666E-12
	        // test 16: L = 1.685E-10 Le = 5.7578E-10
	        
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
	        case 6:
	        	modqd6 = new Fitqd6(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd6.driver();
		        timeFunction = modqd6.getTimeFunction();
		        break;
	        case 7:
	        	modqd7 = new Fitqd7(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd7.driver();
		        timeFunction = modqd7.getTimeFunction();
		        break;
	        case 8:
	        	modqd8 = new Fitqd8(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd8.driver();
		        timeFunction = modqd8.getTimeFunction();
		        break;
	        case 9:
	        	modqd9 = new Fitqd9(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd9.driver();
		        timeFunction = modqd9.getTimeFunction();
		        break;
	        case 10:
	        	modqd10 = new Fitqd10(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd10.driver();
		        timeFunction = modqd10.getTimeFunction();
		        break;
	        case 11:
	        	modqd11 = new Fitqd11(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd11.driver();
		        timeFunction = modqd11.getTimeFunction();
		        break;
	        case 12:
	        	modqd12 = new Fitqd12(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd12.driver();
		        timeFunction = modqd12.getTimeFunction();
		        break;
	        case 13:
	        	modqd13 = new Fitqd13(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd13.driver();
		        timeFunction = modqd13.getTimeFunction();
		        break;
	        case 14:
	        	modqd14 = new Fitqd14(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd14.driver();
		        timeFunction = modqd14.getTimeFunction();
		        break;
	        case 15:
	        	modqd15 = new Fitqd15(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd15.driver();
		        timeFunction = modqd15.getTimeFunction();
		        break;
	        case 16:
	        	modqd16 = new Fitqd16(10*n+1, tqd[10*n], largestPoleqd, tolqd, matrixSizeParameterqd);
		        modqd16.driver();
		        timeFunction = modqd16.getTimeFunction();
		        break;
	        }
	        
	        sum = 0.0;
	        expsum = 0.0;
	        for (i = 0; i < n; i++) {
	            Preferences.debug("time = " + t[i] + " inverseLaplaceFunction = " + timeFunction[10*(i+1)] + " trueFunction = " +
	                              ftrue[test-1][i] + "\n", Preferences.DEBUG_ALGORITHM);
	            diff = ftrue[test-1][i] - timeFunction[10*(i+1)];
	            square = diff * diff;
	            sum = sum + square;
	            expsum = expsum + square * Math.exp(-t[i]);
	        } 
	        L = Math.sqrt(sum/n);
	        Le = Math.sqrt(expsum/ledenom);
	        Preferences.debug("Root mean square error = " + L + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("Exponential weighted root mean square error = " + Le + "\n", Preferences.DEBUG_ALGORITHM);
	        
		   
	        
	        
	    }
        
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
    
    class FitdeHoog6 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog6(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of t = 1/(s*s)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zmlt(realS, imagS[i], realS, imagS[i], realPart, imagPart);
            	zdiv(1.0, 0.0, realPart[0], imagPart[0], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog7 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog7(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of t*exp(-t) = 1/(s+1)**2
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zmlt(realS + 1.0, imagS[i], realS + 1.0, imagS[i], realPart, imagPart);
            	zdiv(1.0, 0.0, realPart[0], imagPart[0], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog8 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog8(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of sin(t) = 1/(s*s + 1)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zmlt(realS, imagS[i], realS, imagS[i], realPart, imagPart);
            	zdiv(1.0, 0.0, realPart[0] + 1.0, imagPart[0], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog9 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog9(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of 1/sqrt(PI*t) = 1/sqrt(s)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zsqrt(realS, imagS[i], realPart, imagPart);
            	zdiv(1.0, 0.0, realPart[0], imagPart[0], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog10 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog10(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of unit step at (t = 5)  = (1/s)*exp(-5s)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zdiv(1.0, 0.0, realS, imagS[i], realPart, imagPart);
            	zexp(-5.0*realS,-5.0*imagS[i], realPart2, imagPart2);
            	zmlt(realPart[0], imagPart[0], realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog11 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog11(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of -gamma - ln(t)  = (1/s)*ln(s)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];
            int ierr[] = new int[1];

            for (i = 0; i < imagS.length; i++) {
            	zdiv(1.0, 0.0, realS, imagS[i], realPart, imagPart);
            	zlog(realS, imagS[i], realPart2, imagPart2, ierr);
            	zmlt(realPart[0], imagPart[0], realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog12 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog12(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of square wave with 1 for 2n < t < (2n+1)
	        // 0 for (2n+1) < t < (2n+2) = (s*(1 + exp(-s))**-1
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zexp(-realS,-imagS[i], realPart, imagPart);
            	zmlt(realS, imagS[i], realPart[0] + 1.0, imagPart[0], realPart2, imagPart2);
            	zdiv(1.0, 0.0, realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog13 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog13(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of t*cos(t) = (s*s - 1)/(s*s + 1)**2
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
            	zmlt(realPart[0] + 1.0, imagPart[0], realPart[0] + 1.0, imagPart[0], realPart2, imagPart2);
            	zdiv(realPart[0] - 1.0, imagPart[0], realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog14 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog14(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of (exp(-0.25*t) - exp(-0.5*t))/sqrt(4*PI*t*t*t) = sqrt(s + 0.5) - sqrt(s + 0.25)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zsqrt(realS + 0.5, imagS[i], realPart, imagPart);
            	zsqrt(realS + 0.25, imagS[i], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart[0] - realPart2[0];
                ans[i][1] = imagPart[0] - imagPart2[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog15 extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog15(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of 2*exp(-4/t)/sqrt(pi*t*t*t) = exp(-4*sqrt(s))
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zsqrt(realS, imagS[i], realPart, imagPart);
            	zexp(-4.0*realPart[0], -4.0*imagPart[0], realPart2, imagPart2);
            	
                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
        }

    }
    
    class FitdeHoog16 extends InverseLaplace {

        /**
         * Creates a new FitdeHoog16 object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitdeHoog16(double[] time, double largestPole, double tol) {
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

            // The Laplace transform of sin(t)/t = arctan(1/s)
        	// arctan(x) = (i/2)*ln((i + x)/(i - x))
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
            int ierr[] = new int[1];

            for (i = 0; i < imagS.length; i++) {
            	zdiv(1.0, 0.0, realS, imagS[i], realPart, imagPart);
            	zdiv(realPart[0], imagPart[0] + 1.0, -realPart[0], -imagPart[0] + 1.0, realPart2, imagPart2);
            	zlog(realPart2[0], imagPart2[0], realPart3, imagPart3, ierr);
            	zmlt(0.0, 0.5, realPart3[0], imagPart3[0], realPart4, imagPart4);

                // real part
                ans[i][0] = realPart4[0];
                ans[i][1] = imagPart4[0];
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
	
	class FitPiessens6 extends InverseLaplace2 {

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
        public FitPiessens6(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	// The Laplace transform of t = 1/(s*s)
        	double realPart[] = new double[1];
        	double imagPart[] = new double[1];
            zmlt(realIn, imagIn, realIn, imagIn, realPart, imagPart);
        	zdiv(1.0, 0.0, realPart[0], imagPart[0], realOut, imagOut);

            return;
        }
    }
	
	class FitPiessens7 extends InverseLaplace2 {

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
        public FitPiessens7(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	// The Laplace transform of t*exp(-t) = 1/(s+1)**2
        	double realPart[] = new double[1];
        	double imagPart[] = new double[1];
            zmlt(realIn + 1.0, imagIn, realIn + 1.0, imagIn, realPart, imagPart);
        	zdiv(1.0, 0.0, realPart[0], imagPart[0], realOut, imagOut);

            return;
        }
    }
	
	class FitPiessens8 extends InverseLaplace2 {

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
        public FitPiessens8(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	// The Laplace transform of sin(t) = 1/(s*s + 1)
        	double realPart[] = new double[1];
        	double imagPart[] = new double[1];
            zmlt(realIn, imagIn, realIn, imagIn, realPart, imagPart);
        	zdiv(1.0, 0.0, realPart[0] + 1.0, imagPart[0], realOut, imagOut);

            return;
        }
    }
	
	class FitPiessens9 extends InverseLaplace2 {

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
        public FitPiessens9(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	// The Laplace transform of 1/sqrt(pi*t) = 1/sqrt(s)
        	double realPart[] = new double[1];
        	double imagPart[] = new double[1];
            zsqrt(realIn, imagIn, realPart, imagPart);
        	zdiv(1.0, 0.0, realPart[0], imagPart[0], realOut, imagOut);

            return;
        }
    }
	
	class FitPiessens10 extends InverseLaplace2 {

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
        public FitPiessens10(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	// The Laplace transform of unit step at (t = 5) = (1/s)*exp(-5.0*s)
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
        	zdiv(1.0, 0.0, realIn, imagIn, realPart, imagPart);
            zexp(-5.0*realIn, -5.0*imagIn, realPart2, imagPart2);
            zmlt(realPart[0], imagPart[0], realPart2[0], imagPart2[0], realOut, imagOut);
            return;
        }
    }
	
	class FitPiessens11 extends InverseLaplace2 {

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
        public FitPiessens11(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	// The Laplace transform of -gamma - ln(t) = (1/s)*ln(s)
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            int ierr[]= new int[1];
        	zdiv(1.0, 0.0, realIn, imagIn, realPart, imagPart);
            zlog(realIn, imagIn, realPart2, imagPart2, ierr);
            zmlt(realPart[0], imagPart[0], realPart2[0], imagPart2[0], realOut, imagOut);
            return;
        }
    }
	
	class FitPiessens12 extends InverseLaplace2 {

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
        public FitPiessens12(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	// The Laplace transform of square wave with 1 for 2n < t < (2n+1)
	        // 0 for (2n+1) < t < (2n+2) = (s*(1 + exp(-s))**-1
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            zexp(-realIn, -imagIn, realPart, imagPart);
            zmlt(realIn, imagIn, realPart[0] + 1.0, imagPart[0], realPart2, imagPart2);
            zdiv(1.0, 0.0, realPart2[0], imagPart2[0], realOut, imagOut);
            return;
        }
    }
	
	class FitPiessens13 extends InverseLaplace2 {

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
        public FitPiessens13(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	
        	// The Laplace transform of t*cos(t) = (s*s - 1)/(s*s + 1)**2
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

        	zmlt(realIn,imagIn,realIn,imagIn,realPart,imagPart);
        	zmlt(realPart[0] + 1.0, imagPart[0], realPart[0] + 1.0, imagPart[0], realPart2, imagPart2);
        	zdiv(realPart[0] - 1.0, imagPart[0], realPart2[0], imagPart2[0], realOut, imagOut);

            return;
        }
    }
	
	class FitPiessens14 extends InverseLaplace2 {

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
        public FitPiessens14(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	
        	// The Laplace transform of (exp(-0.25*t) - exp(-0.5*t))/sqrt(4*PI*t*t*t) = sqrt(s + 0.5) - sqrt(s + 0.25)
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

        	zsqrt(realIn + 0.5, imagIn, realPart, imagPart);
        	zsqrt(realIn + 0.25, imagIn, realPart2, imagPart2);
            realOut[0] = realPart[0] - realPart2[0];
            imagOut[0] = imagPart[0] - imagPart2[0];

            return;
        }
    }
	
	class FitPiessens15 extends InverseLaplace2 {

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
        public FitPiessens15(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	
            // The Laplace transform of 2*exp(-4/t)/sqrt(pi*t*t*t) = exp(-4*sqrt(s))
            double realPart[] = new double[1];
            double imagPart[] = new double[1];

        	zsqrt(realIn,imagIn,realPart,imagPart);
        	zexp(-4.0*realPart[0], -4.0*imagPart[0], realOut, imagOut);
        	
            return;
        }
    }
	
	class FitPiessens16 extends InverseLaplace2 {

        /**
         * Creates a new FitPiessens16 object.
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
        public FitPiessens16(double[] time, double abscissa, double relEps, double absEps, double[] result,
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
        	
        	// The Laplace transform of sin(t)/t = arctan(1/s)
        	// arctan(x) = (i/2)*ln((i + x)/(i - x))
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];
            int ierr[] = new int[1];

        	zdiv(1.0, 0.0, realIn, imagIn, realPart, imagPart);
        	zdiv(realPart[0], imagPart[0] + 1.0, -realPart[0], -imagPart[0] + 1.0, realPart2, imagPart2);
        	zlog(realPart2[0], imagPart2[0], realPart3, imagPart3, ierr);
            zmlt(realPart3[0], imagPart3[0], 0.0, 0.5, realOut,imagOut);

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
	
	class Fitqd6 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd6(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of t = 1/(s*s)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zmlt(realS, imagS[i], realS, imagS[i], realPart, imagPart);
            	zdiv(1.0, 0.0, realPart[0], imagPart[0], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
        	
        }
	}
	
	class Fitqd7 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd7(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of t*exp(-t) = 1/(s+1)**2
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zmlt(realS + 1.0, imagS[i], realS + 1.0, imagS[i], realPart, imagPart);
            	zdiv(1.0, 0.0, realPart[0], imagPart[0], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
        	
        }
	}
	
	class Fitqd8 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd8(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of sin(t) = 1(s*s + 1)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zmlt(realS, imagS[i], realS, imagS[i], realPart, imagPart);
            	zdiv(1.0, 0.0, realPart[0] + 1.0, imagPart[0], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
        	
        }
	}
	
	class Fitqd9 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd9(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of 1/sqrt(PI*t) = 1/sqrt(s)
        	double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zsqrt(realS, imagS[i], realPart, imagPart);
            	zdiv(1.0, 0.0, realPart[0], imagPart[0], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
            
        }
	}
	
	class Fitqd10 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd10(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of unit step at (t = 5)  = (1/s)*exp(-5s)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zdiv(1.0, 0.0, realS, imagS[i], realPart, imagPart);
            	zexp(-5.0*realS,-5.0*imagS[i], realPart2, imagPart2);
            	zmlt(realPart[0], imagPart[0], realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
            
        }
	}
	
	class Fitqd11 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd11(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of -gamma - ln(t)  = (1/s)*ln(s)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];
            int ierr[] = new int[1];

            for (i = 0; i < imagS.length; i++) {
            	zdiv(1.0, 0.0, realS, imagS[i], realPart, imagPart);
            	zlog(realS, imagS[i], realPart2, imagPart2, ierr);
            	zmlt(realPart[0], imagPart[0], realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }
        	
            return ans;
            
        }
	}
	
	class Fitqd12 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd12(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of square wave with 1 for 2n < t < (2n+1)
	        // 0 for (2n+1) < t < (2n+2) = (s*(1 + exp(-s))**-1
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];
            double realPart3[] = new double[1];
            double imagPart3[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zexp(-realS,-imagS[i], realPart, imagPart);
            	zmlt(realS, imagS[i], realPart[0] + 1.0, imagPart[0], realPart2, imagPart2);
            	zdiv(1.0, 0.0, realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
        	
        }
	}
	
	class Fitqd13 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd13(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of t*cos(t) = (s*s - 1)/(s*s + 1)**2
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
            	zmlt(realPart[0] + 1.0, imagPart[0], realPart[0] + 1.0, imagPart[0], realPart2, imagPart2);
            	zdiv(realPart[0] - 1.0, imagPart[0], realPart2[0], imagPart2[0], realPart3, imagPart3);

                // real part
                ans[i][0] = realPart3[0];
                ans[i][1] = imagPart3[0];
            }

            return ans;
        	
        }
	}
	
	class Fitqd14 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd14(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of (exp(-0.25*t) - exp(-0.5*t))/sqrt(4*PI*t*t*t) = sqrt(s + 0.5) - sqrt(s + 0.25)
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zsqrt(realS + 0.5, imagS[i], realPart, imagPart);
            	zsqrt(realS + 0.25, imagS[i], realPart2, imagPart2);

                // real part
                ans[i][0] = realPart[0] - realPart2[0];
                ans[i][1] = imagPart[0] - imagPart2[0];
            }

            return ans;
        	
        }
	}
	
	class Fitqd15 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd15(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of 2*exp(-4/t)/sqrt(pi*t*t*t) = exp(-4*sqrt(s))
            double[][] ans = new double[imagS.length][2];
            int i;
            double realPart[] = new double[1];
            double imagPart[] = new double[1];
            double realPart2[] = new double[1];
            double imagPart2[] = new double[1];

            for (i = 0; i < imagS.length; i++) {
            	zsqrt(realS, imagS[i], realPart, imagPart);
            	zexp(-4.0*realPart[0], -4.0*imagPart[0], realPart2, imagPart2);
            	
                // real part
                ans[i][0] = realPart2[0];
                ans[i][1] = imagPart2[0];
            }

            return ans;
        	
        }
	}
	
	class Fitqd16 extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public Fitqd16(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
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
        	// The Laplace transform of sin(t)/t = arctan(1/s)
        	// arctan(x) = (i/2)*ln((i + x)/(i - x))
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
            int ierr[] = new int[1];

            for (i = 0; i < imagS.length; i++) {
            	zdiv(1.0, 0.0, realS, imagS[i], realPart, imagPart);
            	zdiv(realPart[0], imagPart[0] + 1.0, -realPart[0], -imagPart[0] + 1.0, realPart2, imagPart2);
            	zlog(realPart2[0], imagPart2[0], realPart3, imagPart3, ierr);
            	zmlt(0.0, 0.5, realPart3[0], imagPart3[0], realPart4, imagPart4);

                // real part
                ans[i][0] = realPart4[0];
                ans[i][1] = imagPart4[0];
            }

            return ans;
        	
        }
	}
	
}