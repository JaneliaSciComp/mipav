package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

public abstract class LevmarBoxConstraint {
	// This is a port of LEVMAR_BC_DER.  It requires a function for computing the Jacobian.
	// If no such function is available, use LEVMAR_BC_DIF rather than LEVMAR_BC_DER.
	// Results were worse than those of ELSUNC
	// Unconstrained DRAPER24D INCORRECT No further reduction possible.  Restart with increased mu
	// Constrained HOCK25 CORRECT
	// Unconstrained BARD at standard start CORRECT
	// Constrained BARD at 10 * standard start CORRECT
	// Constrained BARD at 100 * standard start INCORRECT.  Stopped by small Dp.
	// Unconstrained KOWALIK_AND_OSBORNE at standard start CORRECT.
	// Constrained KOWALIK_AND_OSBORNE at 10 * standard start INCORRECT Stopped by small Dp.
	// Constrained KOWALIK_AND_OSBORNE at 100 * standard start INCORRECT Stopped by small Dp.
	// Unconstrained MEYER at standard start CORRECT.
	// Constrained MEYER at 10 * standard start INCORRECT maxIterations.
	// Unconstrained OSBORNE1 CORRECT
	// Unconstrained OSBORNE2 INCORRECT. Stopped by small Dp.
	private final double INIT_MU = 1.0E-3;
	
	private final double STOP_THRESH = 1.0E-17;
	
	private final double DIFF_DELTA = 1.0E-6;
	
	private final int LM_ERROR = -1;
	
	private final int BLOCKSZ = 32; /* block size for cache-friendly matrix-matrix multiply. It should be
     * such that __BLOCKSZ__^2*sizeof(LM_REAL) is smaller than the CPU (L1)
     * data cache size. Notice that a value of 32 when LM_REAL=double assumes
     * an 8Kb L1 data cache (32*32*8=8K). This is a conservative choice since
     * newer Pentium 4s have a L1 data cache of size 16K, capable of holding
     * up to 45x45 double blocks.
     */
    private final int BLOCKSZ_SQ  = BLOCKSZ * BLOCKSZ;
    
    private final double ONE_THIRD = 1.0/3.0;
    
    private final int LSITMAX = 150; // maximum iterations for line search
	
	// On input the parameter estimates.  On output the estimated solution
	private double[] param; 
	
	private double[] xSeries;
	
	// The measurement vector.  null implies a zero vector
	private double[] ySeries;
	
	private int paramNum;
	
	// Measurement vector dimension
	private int nPts;
	
	// Vector of lower bounds.  If null, no lower bounds apply.
	private double lb[];
	
	// Vector of upper bounds.  If null, no upper bounds apply.
	private double ub[];
	
	private int maxIterations;
	
	// 4 minimization options mu, epsilon1, epsilon2, epsilon3
	// Respectively the scale factor for initial \mu,
    // stopping thresholds for ||J^T e||_inf, ||Dp||_2 and ||e||_2. Set to NULL for defaults to be used.
    // Note that ||J^T e||_inf is computed on free (not equal to lb[i] or ub[i]) variables only.
	private double opts[];
	
	// Information regarding minimization size 10  Set to null if don't care.
	// info[0]= ||e||_2 at initial p.
    // info[1-4]=[ ||e||_2, ||J^T e||_inf,  ||Dp||_2, mu/max[J^T J]_ii ], all computed at estimated p.
    // info[5]= # iterations,
    // info[6]=reason for terminating: 1 - stopped by small gradient J^T e
    //                                 2 - stopped by small Dp
    //                                3 - stopped by itmax
    //                                 4 - singular matrix. Restart from current p with increased mu 
    //                                5 - no further error reduction is possible. Restart with increased mu
    //                                6 - stopped by small ||e||_2
    //                                7 - stopped by invalid (i.e. NaN or Inf) "func" values. This is a user error
    // info[7]= # function evaluations
    // info[8]= # Jacobian evaluations
    // info[9]= # linear systems solved, i.e. # attempts for reducing error
	private double info[];
	
	double DOUBLE_EPSILON;
	
	private boolean outputMes = false;
	
    private boolean testMode = false; 
    
    private boolean  analyticalJacobian = true;
    
    private int testCase;
    
    private final int DRAPER24D = 0;
    
    private final int BARD = 8;
    
    private final int MEYER = 10;
    
    private final int KOWALIK_AND_OSBORNE = 15;
    
    private final int OSBORNE1 = 17;
    
    private final int OSBORNE2 = 19;
    
    private final int HOCK25 = 25;
	
	public LevmarBoxConstraint() {
    	int i;
    	// Below is an example used to fit y = a0 - a1*(a2**x)
    	// This example implements the solution of problem D of chapter 24 of Applied Regression Analysis, Third Edition by
    	// Norman R. Draper and Harry Smith */
    	// The correct answer is a0 = 72.4326,  a1 = 28.2519, a2 = 0.5968
    	// Incorrect with statement no further reduction is possible.  Restart with increased mu.
    	info = new double[10];
    	outputMes = true;
    	Preferences.debug("Draper problem 24D y = a0 - a1*(a2**x) constrained\n");
    	Preferences.debug("Correct answer is a0 = 72.4326, a1 = 28.2519, a2 = 0.5968\n");
    	testMode = true;
    	testCase = DRAPER24D;
    	nPts = 5;
    	paramNum = 3;
    	maxIterations = 1000 * paramNum;
        xSeries = new double[5];
        ySeries = new double[nPts];
        param = new double[paramNum];
        xSeries[0] = 0.0;
        xSeries[1] = 1.0;
        xSeries[2] = 2.0;
        xSeries[3] = 3.0;
        xSeries[4] = 4.0;
        ySeries[0] = 44.4;
        ySeries[1] = 54.6;
        ySeries[2] = 63.8;
        ySeries[3] = 65.7;
        ySeries[4] = 68.9;
        param[0] = 0.0;
        param[1] = 10.0;
        param[2] = 0.2;
        
        lb = new double[paramNum];
        ub = new double[paramNum];
        lb[0] = -1000.0;
        ub[0] = 1000.0;

        // Constrain a1
        lb[1] = -1000.0;
        ub[1] = 1000.0;

        // Constrain a2
        lb[2] = 0.0;
        ub[2] = 1.0;
        driver();
        dumpTestResults();
        
        // Below is an example used to fit y = 100 * (a2 - a1)**2 + 
        
        // Below is an example used to fit y = (a0 * log(0.01*i)**(a1) + a2
    	// where a0 = -50, a1 = 2.0/3.0, a2 = 25.0
    	// Variant of test example 25 from Hock and Schittkowski
    	// Works correctly.
        Preferences.debug("Test example 25 from Hock and Schittkowski constrained\n");
        Preferences.debug("y = (a0 * log(0.01*i)**(a1) + a2\n");
        Preferences.debug("Correct answer is a0 = -50, a1 = 2.0/3.0, a3 = 25.0\n");
        testMode = true;
        testCase = HOCK25;
        nPts = 99;
        paramNum = 3;
        maxIterations = 1000 * paramNum;
    	xSeries = new double[99];
    	ySeries = new double[nPts];
    	param = new double[paramNum];
    	for (i = 1; i <= 99; i++) {
    		xSeries[i-1] = 0.01 * i;
    		ySeries[i-1] = Math.pow((-50.0 * Math.log(xSeries[i-1])),2.0/3.0) + 25.0;
    	}
    	param[0] = -100.0;
    	param[1] = 1.0/3.0;
    	param[2] = 12.5;
    	
        lb = new double[paramNum];
        ub = new double[paramNum];
        // Constrain a[0]
        lb[0] = -200.0;
        ub[0] = -0.1;

        // Constrain a[1]
        lb[1] = 0.0;
        ub[1] = 5.0;

        // Constrain a[2]
        lb[2] = 0.0;
        ub[2] = 25.6;
        driver();
        dumpTestResults();
        // Below is an example to fit y = a0 + x/(a1*(16 - x) + a2*min(x,16-x))
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Unconstrained worked at the standard starting point. 
        // a[1] is a root in the denominator and so with unconstrained for large
        // ranges a[1] = 0 will result in an infinity.  Constrained with a[1] >= 0.1
        // works at 10.0 * starting point but not at 100.0 * starting point.
        Preferences.debug("Bard function standard starting point unconstrained\n");
        Preferences.debug("y = a0 + x/(a1*(16-x) + a2*min(x,16-x))\n");
        Preferences.debug("Correct answer is a0 = 0.08241, a1 = 1.133, a2 = 2.344\n");
        Preferences.debug("Correct answer has Chi-squared = 8.21487E-3\n");
        testMode = true;
        testCase = BARD;
        nPts = 15;
        paramNum = 3;
        maxIterations = 1000 * paramNum;
        xSeries = new double[15];
        for (i = 1; i <= 15; i++) {
        	xSeries[i-1] = i;
        }
        ySeries = new double[]{0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 
        		               0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39};
        param = new double[paramNum];
        param[0] = 1.0;
        param[1] = 1.0;
        param[2] = 1.0;
        
        lb = null;
        ub = null;
        
        driver();
        dumpTestResults();
        // Below is an example to fit y = a0 + x/(a1*(16 - x) + a2*min(x,16-x))
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Unconstrained worked at the standard  starting point.
        // a[1] is a root in the denominator and so with unconstrained for large
        // ranges a[1] = 0 will result in an infinity.  Constrained with a[1] >= 0.1
        // works at 10.0 * starting point but not at 100.0 * starting point.
        Preferences.debug("Bard function 10 * standard starting point constrained\n");
        Preferences.debug("y = a0 + x/(a1*(16-x) + a2*min(x,16-x))\n");
        Preferences.debug("Correct answer is a0 = 0.08241, a1 = 1.133, a2 = 2.344\n");
        Preferences.debug("Correct answer has Chi-squared = 8.21487E-3\n");
        testMode = true;
        testCase = BARD;
        nPts = 15;
        paramNum = 3;
        maxIterations = 1000 * paramNum;
        xSeries = new double[15];
        for (i = 1; i <= 15; i++) {
        	xSeries[i-1] = i;
        }
        ySeries = new double[]{0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 
        		               0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39};
        param = new double[paramNum];
        param[0] = 10.0;
        param[1] = 10.0;
        param[2] = 10.0;
        
        
        lb = new double[paramNum];
        ub = new double[paramNum];
        lb[0] = 0.0;
        ub[0] = 20.0;
        lb[1] = 0.1;
        ub[1] = 20.0;
        lb[2] = 0.0;
        ub[2] = 20.0;
        driver();
        dumpTestResults();
        // Below is an example to fit y = a0 + x/(a1*(16 - x) + a2*min(x,16-x))
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Unconstrained worked at the standard  starting point.
        // a[1] is a root in the denominator and so with unconstrained for large
        // ranges a[1] = 0 will result in an infinity.  Constrained with a[1] >= 0.1
        // works at 10.0 * starting point but not at 100.0 * starting point.
        Preferences.debug("Bard function 100 * standard starting point constrained\n");
        Preferences.debug("y = a0 + x/(a1*(16-x) + a2*min(x,16-x))\n");
        Preferences.debug("Correct answer is a0 = 0.08241, a1 = 1.133, a2 = 2.344\n");
        Preferences.debug("Correct answer has Chi-squared = 8.21487E-3\n");
        testMode = true;
        testCase = BARD;
        nPts = 15;
        paramNum = 3;
        maxIterations = 1000 * paramNum;
        xSeries = new double[15];
        for (i = 1; i <= 15; i++) {
        	xSeries[i-1] = i;
        }
        ySeries = new double[]{0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 
        		               0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39};
        param = new double[paramNum];
        param[0] = 100.0;
        param[1] = 100.0;
        param[2] = 100.0;
        
        lb = new double[paramNum];
        ub = new double[paramNum];
        lb[0] = 0.0;
        ub[0] = 200.0;
        lb[1] = 0.1;
        ub[1] = 200.0;
        lb[2] = 0.0;
        ub[2] = 200.0;
        driver();
        dumpTestResults();
        // Below is an example to fit y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // In fact unconstrained only worked at the standard 
        // starting point and constrained would work not at 10 * standard starting point 
        // and would not work at 100 * standard starting point.
        Preferences.debug("Kowalik and Osborne function standard starting point unconstrained\n");
        Preferences.debug("y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)\n");
        Preferences.debug("Correct answer is a0 = 0.1928, a1 = 0.1913, a2 = 0.1231, a3 = 0.1361\n");
        Preferences.debug("Correct answer has Chi-squared = 3.07505E-4\n");
        testMode = true;
        testCase = KOWALIK_AND_OSBORNE;
        nPts = 11;
        paramNum = 4;
        maxIterations = 1000 * paramNum;
        xSeries = new double[]{4.0, 2.0, 1.0, 0.5, 0.25, 0.167, 0.125, 0.1, 0.0833, 0.0714, 0.0625};
        ySeries = new double[]{0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 
        		               0.0323, 0.0235, 0.0246};
        param = new double[paramNum];
        param[0] = 0.25;
        param[1] = 0.39;
        param[2] = 0.415;
        param[3] = 0.39;
        
        lb = null;
        ub = null;
        
        driver();
        dumpTestResults();
        // Below is an example to fit y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // In fact unconstrained only worked at the standard 
        // starting point and constrained would not work at 10 * standard starting point 
        // and would not work at 100 * standard starting point.
        Preferences.debug("Kowalik and Osborne function 10 * standard starting point constrained\n");
        Preferences.debug("y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)\n");
        Preferences.debug("Correct answer is a0 = 0.1928, a1 = 0.1913, a2 = 0.1231, a3 = 0.1361\n");
        Preferences.debug("Correct answer has Chi-squared = 3.07505E-4\n");
        testMode = true;
        testCase = KOWALIK_AND_OSBORNE;
        nPts = 11;
        paramNum = 4;
        maxIterations = 1000 * paramNum;
        xSeries = new double[]{4.0, 2.0, 1.0, 0.5, 0.25, 0.167, 0.125, 0.1, 0.0833, 0.0714, 0.0625};
        ySeries = new double[]{0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 
        		               0.0323, 0.0235, 0.0246};
        param = new double[paramNum];
        param[0] = 2.5;
        param[1] = 3.9;
        param[2] = 4.15;
        param[3] = 3.9;
        
        lb = new double[paramNum];
        ub = new double[paramNum];
        lb[0] = 0;
        ub[0] = 10.0;
        lb[1] = 0;
        ub[1] = 10.0;
        lb[2] = 0;
        ub[2] = 10.0;
        lb[3] = 0;
        ub[3] = 10.0;
        driver();
        dumpTestResults();
        // Below is an example to fit y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // In fact unconstrained only worked at the standard 
        // starting point and constrained would not work at 10 * standard starting point 
        // and would not work at 100 * standard starting point.
        Preferences.debug("Kowalik and Osborne function 100 * standard starting point constrained\n");
        Preferences.debug("y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)\n");
        Preferences.debug("Correct answer is a0 = 0.1928, a1 = 0.1913, a2 = 0.1231, a3 = 0.1361\n");
        Preferences.debug("Correct answer has Chi-squared = 3.07505E-4\n");
        testMode = true;
        testCase = KOWALIK_AND_OSBORNE;
        nPts = 11;
        paramNum = 4;
        maxIterations = 1000 * paramNum;
        xSeries = new double[]{4.0, 2.0, 1.0, 0.5, 0.25, 0.167, 0.125, 0.1, 0.0833, 0.0714, 0.0625};
        ySeries = new double[]{0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 
        		               0.0323, 0.0235, 0.0246};
        param = new double[paramNum];
        param[0] = 25.0;
        param[1] = 39.0;
        param[2] = 41.5;
        param[3] = 39.0;
        
        lb = new double[paramNum];
        ub = new double[paramNum];
        lb[0] = 0;
        ub[0] = 100.0;
        lb[1] = 0;
        ub[1] = 100.0;
        lb[2] = 0;
        ub[2] = 100.0;
        lb[3] = 0;
        ub[3] = 100.0;
        driver();
        dumpTestResults();
        // Below is an example to fit y = a0*exp[a1/(x + a2)]
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Meyer function standard starting point unconstrained\n");
        Preferences.debug("Y = a0*exp[a1/(x + a2)]\n");
        Preferences.debug("Correct answers are a0 = 0.0056096, a1 = 6181.3, a2 = 345.22\n");
        Preferences.debug("Correct answer has Chi-squared = 87.9458\n");
        testMode = true;
        testCase = MEYER;
        nPts = 16;
        paramNum = 3;
        maxIterations = 1000 * paramNum;
        xSeries = new double[]{50.0, 55.0, 60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0,
        		               100.0, 105.0, 110.0, 115.0, 120.0, 125.0};
        ySeries = new double[]{34780.0, 28610.0, 23650.0, 19630.0, 16370.0, 13720.0, 11540.0,
        		               9744.0, 8261.0, 7030.0, 6005.0, 5147.0, 4427.0, 3820.0,
        		               3307.0, 2872.0};
        param = new double[paramNum];
        param[0] = 0.02;
        param[1] = 4000.0;
        param[2] = 250.0;
        
        lb = null;
        ub = null;
        
        driver();
        dumpTestResults();
        // Below is an example to fit y = a0*exp[a1/(x + a2)]
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Meyer function 10 * standard starting point constrained\n");
        Preferences.debug("Y = a0*exp[a1/(x + a2)]\n");
        Preferences.debug("Correct answers are a0 = 0.0056096, a1 = 6181.3, a2 = 345.22\n");
        Preferences.debug("Correct answer has Chi-squared = 87.9458\n");
        testMode = true;
        testCase = MEYER;
        nPts = 16;
        paramNum = 3;
        maxIterations = 1000 * paramNum;
        xSeries = new double[]{50.0, 55.0, 60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0,
        		               100.0, 105.0, 110.0, 115.0, 120.0, 125.0};
        ySeries = new double[]{34780.0, 28610.0, 23650.0, 19630.0, 16370.0, 13720.0, 11540.0,
        		               9744.0, 8261.0, 7030.0, 6005.0, 5147.0, 4427.0, 3820.0,
        		               3307.0, 2872.0};
        param = new double[paramNum];
        param[0] = 0.2;
        param[1] = 40000.0;
        param[2] = 2500.0;
        
        lb = new double[paramNum];
        ub = new double[paramNum];
        lb[0] = 1.0E-3;
        ub[0] = 1.0;
        lb[1] = 100.0;
        ub[1] = 100000.0;
        lb[2] = 100.0;
        ub[2] = 3000.0;                                             
        driver();
        dumpTestResults();
        // Below is an example to fit y = a0 + a1*exp(-a3*x) + a2*exp(-a4*x)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Osborne 1 function unconstrained\n");
        Preferences.debug("y = a0 + a1*exp(-a3*x) + a2*exp(-a4*x)\n");
        Preferences.debug("Correct answer is a0 = 0.37541, a1 = 1.9358, a2 = -1.4647, a3 = 0.012868, a4 = 0.022123\n");
        Preferences.debug("Correct answer has Chi-squared = 5.46489E-5\n");
        testMode = true;
        testCase = OSBORNE1;
        nPts = 33;
        paramNum = 5;
        maxIterations = 1000 * paramNum;
        xSeries = new double[33];
        for (i = 1; i <= 33; i++) {
        	xSeries[i-1] = 10.0*(i-1);
        }
        ySeries = new double[]{0.844, 0.908, 0.932, 0.936, 0.925, 0.908, 0.881, 0.850, 0.818,
        		  0.784, 0.751, 0.718, 0.685, 0.658, 0.628, 0.603, 0.580, 0.558, 0.538, 0.522,
        		  0.506, 0.490, 0.478, 0.467, 0.457, 0.448, 0.438, 0.431, 0.424, 0.420, 0.414,
        		  0.411, 0.406};
        param = new double[paramNum];
        param[0] = 0.5;
        param[1] = 1.5;
        param[2] = -1.0;
        param[3] = 0.01;
        param[4] = 0.02;
        
        lb = null;
        ub = null;
        
        driver();
        dumpTestResults();
        // Below is an example to fit y = a0*exp(-a4*x) + a1*exp(-a5*(x-a8)**2) 
        // + a2*exp(-a6*(x-a9)**2) + a3*exp(-a7*(x-a10)**2)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Osborne 2 function unconstrained\n");
        Preferences.debug("y = a0*exp(-a4*x) + a1*exp(-a5*(x-a8)**2) \n");
        Preferences.debug("    + a2*exp(-a6*(x-a9)**2) + a3*exp(-a7*(x-a10)**2)\n");
        Preferences.debug("Correct answer has a0 = 1.3100, a1 = 0.43155, a2 = 0.63366, a3 = 0.59943\n");
        Preferences.debug("a4 = 0.75418, a5 = 0.90429, a6 = 1.3658, a7 = 4.8237, a8 = 2.3987\n");
        Preferences.debug("a9 = 4.5689, a10 = 5.6753\n");
        Preferences.debug("Correct answer has Chi-squared = 4.01377E-2\n");
        testMode = true;
        testCase = OSBORNE2;
        nPts = 65;
        paramNum = 11;
        maxIterations = 1000 * paramNum;
        xSeries = new double[65];
        for (i = 1; i <= 65; i++) {
        	xSeries[i-1] = (i-1)/10.0;
        }
        ySeries = new double[]{1.366, 1.191, 1.112, 1.013, 0.991, 0.885, 0.831, 0.847, 0.786,
        		  0.725, 0.746, 0.679, 0.608, 0.655, 0.616, 0.606, 0.602, 0.626, 0.651, 0.724,
        		  0.649, 0.649, 0.694, 0.644, 0.624, 0.661, 0.612, 0.558, 0.533, 0.495, 0.500,
        		  0.423, 0.395, 0.375, 0.372, 0.391, 0.396, 0.405, 0.428, 0.429, 0.523, 0.562,
        		  0.607, 0.653, 0.672, 0.708, 0.633, 0.668, 0.645, 0.632, 0.591, 0.559, 0.597,
        		  0.625, 0.739, 0.710, 0.729, 0.720, 0.636, 0.581, 0.428, 0.292, 0.162, 0.098,
        		  0.054};
        param = new double[paramNum];
        param[0] = 1.3;
        param[1] = 0.65;
        param[2] = 0.65;
        param[3] = 0.7;
        param[4] = 0.6;
        param[5] = 3.0;
        param[6] = 5.0;
        param[7] = 7.0;
        param[8] = 2.0;
        param[9] = 4.5;
        param[10] = 5.5;
        
        lb = null;
        ub = null;
        
        driver();
        dumpTestResults();
    }
	
	public LevmarBoxConstraint(double param[], double ySeries[], int paramNum, int nPts,
			                   double lb[], double ub[], int maxIterations, 
			                   double opts[], double info[], boolean outputMes) {
		try {
			this.param = param;
			this.ySeries = ySeries;
			this.paramNum = paramNum;
			this.nPts = nPts;
			this.lb = lb;
			this.ub = ub;
			this.maxIterations = maxIterations;
			this.opts = opts;
			this.info = info;
			this.outputMes = outputMes;
		}
		catch (OutOfMemoryError error) { }
	} // public LevmarBoxConstraint
	
	public abstract void fitToFunction(double[] param, double[] hx, int paramNum, int nPts);
	
	private void fitToTestFunction(double[] param, double[] hx, int paramNum, int nPts) {
		int i;
		switch(testCase) {
		    case DRAPER24D:
		    	for (i = 0; i < nPts; i++) {
                    hx[i] = param[0] - (param[1] * Math.pow(param[2], xSeries[i]));
                }
		    	break;
		    case HOCK25:
		    	for (i = 0; i < nPts; i++) {
                    hx[i] = Math.pow((param[0] * Math.log(xSeries[i])),param[1]) + param[2];
                }
		    	break;
		    case BARD:
		    	for (i = 0; i < nPts; i++) {
                    hx[i] = param[0] + xSeries[i]/(param[1]*(16.0 - xSeries[i]) 
                    		 + param[2]*Math.min(xSeries[i], 16.0 - xSeries[i]));
                }
		    	break;
		    case KOWALIK_AND_OSBORNE:
		    	for (i = 0; i < nPts; i++) {
                    hx[i] = param[0]*(xSeries[i]*xSeries[i] + param[1]*xSeries[i])/
                            (xSeries[i]*xSeries[i] + param[2]*xSeries[i] + param[3]);
                }
		    	break;
		    case MEYER: 
		    	for (i = 0; i < nPts; i++) {
		    		hx[i] = param[0]*Math.exp(param[1]/(xSeries[i] + param[2]));
		    	}
		    	break;
		    case OSBORNE1:
		    	for (i = 0; i < nPts; i++) {
                    hx[i] = param[0] + param[1]*Math.exp(-param[3]*xSeries[i]) + param[2]*Math.exp(-param[4]*xSeries[i]);
                }
		    	break;
		    case OSBORNE2:
		    	for (i = 0; i < nPts; i++) {
                    hx[i] = param[0]*Math.exp(-param[4]*xSeries[i]) 
                           + param[1]*Math.exp(-param[5]*(xSeries[i] - param[8])*(xSeries[i] - param[8]))
                           + param[2]*Math.exp(-param[6]*(xSeries[i] - param[9])*(xSeries[i] - param[9]))
                           + param[3]*Math.exp(-param[7]*(xSeries[i] - param[10])*(xSeries[i] - param[10]));
                }
		    	break;
		}
	}
	
	public abstract void fitToJacobian(double[] param, double[] jac, int paramNum, int nPts);
	
	private void fitToTestJacobian(double[] param, double[] jac, int paramNum, int nPts) {
		int i, j;
		double exponent;
		double denom;
		double top;
	    switch(testCase) {
	        case DRAPER24D:
	        	for (i = 0, j = 0; i < nPts; i++) {
                    jac[j++] = 1.0;
                    jac[j++] = -Math.pow(param[2], xSeries[i]);
                    jac[j++] = -xSeries[i] * param[1] * Math.pow(param[2], xSeries[i] - 1.0);
                }
	    	    break;
	        case HOCK25:
	        	for (i = 0, j = 0; i < nPts; i++) {
                    jac[j++] = param[1]*Math.pow((param[0] * Math.log(xSeries[i])),param[1]-1.0) * Math.log(xSeries[i]);
                    jac[j++] = Math.log(param[0] * Math.log(xSeries[i])) * Math.pow((param[0] * Math.log(xSeries[i])),param[1]);
                    jac[j++] = 1.0;
                }
	        	break;
	        case BARD:
	        	for (i = 0, j = 0; i < nPts; i++) {
                	denom = (param[1]*(16.0 - xSeries[i]) + param[2]*Math.min(xSeries[i], 16.0 - xSeries[i]));
                    jac[j++] = 1.0;
                    jac[j++] = -xSeries[i]*(16.0 - xSeries[i])/(denom*denom);
                    jac[j++] = -xSeries[i]*Math.min(xSeries[i], 16.0 - xSeries[i])/(denom*denom);
                }
	        	break;
	        case KOWALIK_AND_OSBORNE:
	        	for (i = 0, j = 0; i < nPts; i++) {
                	denom = (xSeries[i]*xSeries[i] + param[2]*xSeries[i] + param[3]);
                	top = (xSeries[i]*xSeries[i] + param[1]*xSeries[i]);
                    jac[j++] = top/denom;
                    jac[j++] = param[0]*xSeries[i]/denom;
                    jac[j++] = -param[0]*xSeries[i]*top/(denom*denom);
                    jac[j++] = -param[0]*top/(denom*denom);
                }
	        	break;
	        case MEYER:
	        	for (i = 0, j = 0; i < nPts; i++) {
                	exponent = Math.exp(param[1]/(xSeries[i] + param[2]));
                    jac[j++] = exponent;
                    jac[j++] = (param[0]/(xSeries[i] + param[2]))* exponent;
                    jac[j++] = -(param[0]*param[1]/((xSeries[i] + param[2])*(xSeries[i] + param[2]))) * exponent;
                }
	    	    break;
	        case OSBORNE1:
	        	for (i = 0, j = 0; i < nPts; i++) {
                    jac[j++] = 1.0;
                    jac[j++] = Math.exp(-param[3]*xSeries[i]);
                    jac[j++] = Math.exp(-param[4]*xSeries[i]);
                    jac[j++] = -param[1]*xSeries[i]*Math.exp(-param[3]*xSeries[i]);
                    jac[j++] = -param[2]*xSeries[i]*Math.exp(-param[4]*xSeries[i]);
                }
	        	break;
	        case OSBORNE2:
	        	for (i = 0, j = 0; i < nPts; i++) {
                    jac[j++] = Math.exp(-param[4]*xSeries[i]);
                    jac[j++] = Math.exp(-param[5]*(xSeries[i] - param[8])*(xSeries[i] - param[8]));
                    jac[j++] = Math.exp(-param[6]*(xSeries[i] - param[9])*(xSeries[i] - param[9]));
                    jac[j++] = Math.exp(-param[7]*(xSeries[i] - param[10])*(xSeries[i] - param[10]));
                    jac[j++] = -param[0]*xSeries[i]*Math.exp(-param[4]*xSeries[i]) ;
                    jac[j++] = -param[1]*(xSeries[i] - param[8])*(xSeries[i] - param[8])
                                     *Math.exp(-param[5]*(xSeries[i] - param[8])*(xSeries[i] - param[8]));
                    jac[j++] = -param[2]*(xSeries[i] - param[9])*(xSeries[i] - param[9])
                                     *Math.exp(-param[6]*(xSeries[i] - param[9])*(xSeries[i] - param[9]));
                    jac[j++] = -param[3]*(xSeries[i] - param[10])*(xSeries[i] - param[10])
                                     *Math.exp(-param[7]*(xSeries[i] - param[10])*(xSeries[i] - param[10]));
                    jac[j++] = 2.0*param[1]*param[5]*(xSeries[i] - param[8])
                                     *Math.exp(-param[5]*(xSeries[i] - param[8])*(xSeries[i] - param[8]));
                    jac[j++] = 2.0*param[2]*param[6]*(xSeries[i] - param[9])
                                     *Math.exp(-param[6]*(xSeries[i] - param[9])*(xSeries[i] - param[9]));
                    jac[j++] = 2.0*param[3]*param[7]*(xSeries[i] - param[10])
                                      *Math.exp(-param[7]*(xSeries[i] - param[10])*(xSeries[i] - param[10]));
                }
	        	break;
	    }
	}
	
	private void dumpTestResults() {
		int i;
		int termReason;
		for (i = 0; i < paramNum; i++) {
			Preferences.debug("param[" + i + "] = " + param[i] + "\n");
		}
		if (info != null) {
		    Preferences.debug("||e||_2 at initial p = " + info[0] + "\n");
		    Preferences.debug("||e||_2 at estimated p = " + info[1] + "\n");
		    Preferences.debug("||J^T e||_inf at estimated p = " + info[2] + "\n");
		    Preferences.debug("||Dp||_2 at estimated p = " + info[3] + "\n");
		    Preferences.debug("mu/max[J^T J]_ii at estimated p = " + info[4] + "\n");
		    Preferences.debug("Iterations = " + Math.round(info[5]) + "\n");
		    Preferences.debug("Reason for terminating:\n");
		    termReason = (int)Math.round(info[6]);
		    switch(termReason) {
		        case 1:
		            Preferences.debug("Stopped by small gradient J^T e\n");
		            break;
		        case 2:
		        	Preferences.debug("Stopped by small Dp\n");
		        	break;
		        case 3:
		        	Preferences.debug("Stopped by maxIterations\n");
		        	break;
		        case 4:
		        	Preferences.debug("Singular matrix.  Restart from current param with increased mu\n");
		        	break;
		        case 5:
		        	Preferences.debug("No further reduction is possible.  Restart with increased mu\n");
		        	break;
		        case 6:
		        	Preferences.debug("Stopped by small ||e||_2\n");
		        	break;
		        case 7:
		        	Preferences.debug("Stopped by invalid (i.e. NaN of inf) func values\n");
		        	break;
		    } // switch(termReason)
		    Preferences.debug("Function evaluations = " + Math.round(info[7]) + "\n");
		    Preferences.debug("Jacobian evaluations = " + Math.round(info[8]) + "\n");
		    Preferences.debug("Linear systems solved = " + Math.round(info[9]) + "\n");
		} // if (info != null)
		Preferences.debug("\n");
	}

	
	public int driver() {
		int i, j, k, l;
		int issolved;
		/* temp work arrays */
		double e[] = new double[nPts];
		double hx[] = new double[nPts];
		double jacTe[] = new double[paramNum];
		double jac[] = new double[nPts * paramNum];
		double jacTjac[] = new double[paramNum * paramNum];
		double Dp[] = new double[paramNum];
		// diagonal of J^T J
		double diag_jacTjac[] = new double[paramNum];
		double pDp[] = new double[paramNum];
		// damping constant
		double mu;
		// mainly used in matrix and vector multiplications
		double tmp;
		// ||e(p)||_2
		double p_eL2;
		// ||J^T e||_inf
		double jacTe_inf;
		// ||e(p+Dp) ||_2
		double pDp_eL2[] = new double[1];
		double p_L2;
		double Dp_L2 = Double.MAX_VALUE;
		double dF;
		double dL;
		double tau;
		double eps1;
		double eps2;
		double eps2_sq;
		double eps3;
		double init_p_eL2;
		int nu = 2;
		int nu2;
		int stop = 0;
		int nfev[] = new int[1];
		int njev = 0;
		int nlss = 0;
		final int nm = nPts * paramNum;
		
		/* variables for constrained LM */
		int fstate_nPts;
		int fstate_nfev[];
		double fstate_hx[];
		double fstate_ySeries[];
		double alpha = 1.0E-4;
		double beta = 0.9;
		double gamma = 0.99995;
		double gamma_sq = gamma * gamma;
	    double rho = 1.0E-8;
	    double t;
	    double t0;
	    releps();
	    double steptl = 1.0E3 * Math.sqrt(DOUBLE_EPSILON);
	    double jacTeDp;
	    /* Minimum step length for LS and PG steps */
	    double tming = 1.0E-18;
	    /* Initial step length for the LS and PG steps */
	    final double tini = 1.0;
	    int nLMsteps = 0;
	    int nLSsteps = 0;
	    int nPGsteps = 0;
	    int gprevtaken = 0;
	    int numactive;
	    boolean dogradproj = false;
	    
	    mu = 0.0;
	    jacTe_inf = 0.0;
	    t = 0.0;
	    
	    if (nPts < paramNum) {
	    	MipavUtil.displayError("Cannot solve a problem with fewer measurements = " + nPts +
	    			              " than unknowns = " + paramNum);
	    	return LM_ERROR;
	    }
	    
	    if ((lb != null) && (ub != null)) {
	    	for (i = 0; i < paramNum; i++) {
	    		if (lb[i] > ub[i]) {
	    		    MipavUtil.displayError("At least one lower bound exceeds the upper one");
	    		    return LM_ERROR;
	    		}
	    	}
	    } // if ((lb != null) && (ub != null))
	    
	    if (opts != null) {
	    	tau = opts[0];
	    	eps1 = opts[1];
	    	eps2 = opts[2];
	    	eps2_sq = opts[2] * opts[2];
	    	eps3 = opts[3];
	    }
	    else {
	    	// use default values
	    	tau = INIT_MU;
	    	eps1 = STOP_THRESH;
	    	eps2 = STOP_THRESH;
	    	eps2_sq = STOP_THRESH * STOP_THRESH;
	    	eps3 = STOP_THRESH;
	    }
	    
	    fstate_nPts = nPts;
	    fstate_hx = hx;
	    fstate_ySeries = ySeries;
	    fstate_nfev = nfev;
	    
	    /* See if starting point is within the feasible set */
	    for (i = 0; i < paramNum; ++i) {
	    	pDp[i] = param[i];
	    }
	    /* project to feasible set */
	    boxProject(param, lb, ub, paramNum);
	    if (outputMes) {
		    for (i = 0; i < paramNum; ++i) {
		    	if (pDp[i] != param[i]) {
		    		Preferences.debug("Warning: component " + i + " of starting point not feasible in LevmarBoxConstraint\n");
		    		Preferences.debug(pDp[i] + " projected to " + param[i] + "\n");
		    	} 
		    }
	    } // if (outputMes)
	    /* Compute e = ySeries - f(param) and its L2 norm */
	    if (testMode) {
	    	fitToTestFunction(param, hx, paramNum, nPts);
	    }
	    else {
	        fitToFunction(param, hx, paramNum, nPts);
	    }
	    nfev[0] = 1;
	    /* e = ySeries - hx, p_eL2 = ||e|| */
	    p_eL2 = LEVMAR_L2NRMXMY(e, ySeries, hx, nPts);
	    init_p_eL2 = p_eL2;
	    if ((Double.isInfinite(p_eL2)) || (Double.isNaN(p_eL2))) {
	    	stop = 7;
	    }
	    
	    mainLoop:
	    for (k = 0; k < maxIterations && (stop == 0); ++k) {
	        /* Note that param and e have been updated at a previous iteration */
	    	
	    	if (p_eL2 <= eps3) { // error is small
	    		stop = 6;
	    		break;
	    	}
	    	
	    	/* Compute the Jacobian J at p,  J^T J,  J^T e,  ||J^T e||_inf and ||p||^2.
	         * Since J^T J is symmetric, its computation can be sped up by computing
	         * only its upper triangular part and copying it to the lower part
	         */
	    	
	    	if (testMode) {
	    		fitToTestJacobian(param, jac, paramNum, nPts);
	    	}
	    	else {
	    	    fitToJacobian(param, jac, paramNum, nPts);
	    	}
	    	++njev;
	    	
	    	/* J^T J, J^T e */
	        if(nm < BLOCKSZ_SQ){ // this is a small problem
	          /* J^T*J_ij = \sum_l J^T_il * J_lj = \sum_l J_li * J_lj.
	           * Thus, the product J^T J can be computed using an outer loop for
	           * l that adds J_li*J_lj to each element ij of the result. Note that
	           * with this scheme, the accesses to J and JtJ are always along rows,
	           * therefore induces less cache misses compared to the straightforward
	           * algorithm for computing the product (i.e., l loop is innermost one).
	           * A similar scheme applies to the computation of J^T e.
	           * However, for large minimization problems (i.e., involving a large number
	           * of unknowns and measurements) for which J/J^T J rows are too large to
	           * fit in the L1 cache, even this scheme incures many cache misses. In
	           * such cases, a cache-efficient blocking scheme is preferable.
	           *
	           * Thanks to John Nitao of Lawrence Livermore Lab for pointing out this
	           * performance problem.
	           *
	           * Note that the non-blocking algorithm is faster on small
	           * problems since in this case it avoids the overheads of blocking. 
	           */
	          int im;
	          int jaclmIndex;
	          
	          /* looping downwards saves a few computations */
	          for(i=paramNum*paramNum; i-->0; )
	            jacTjac[i]=0.0;
	          for(i=paramNum; i-->0; )
	            jacTe[i]=0.0;

	          for(l=nPts; l-->0; ){
	            jaclmIndex = l*paramNum;
	            for(i=paramNum; i-->0; ){
	              im=i*paramNum;
	              alpha=jac[jaclmIndex + i]; //jac[l*m+i];
	              for(j=i+1; j-->0; ) /* j<=i computes lower triangular part only */
	                jacTjac[im+j]+=jac[jaclmIndex + j]*alpha; //jac[l*m+j]

	              /* J^T e */
	              jacTe[i]+=alpha*e[l];
	            }
	          }

	          for(i=paramNum; i-->0; ) /* copy to upper part */
	            for(j=i+1; j<paramNum; ++j)
	              jacTjac[i*paramNum+j]=jacTjac[j*paramNum+i];
	        } // if(nm < BLOCKSZ_SQ){ // this is a small problem
	        else{ // this is a large problem
	            /* Cache efficient computation of J^T J based on blocking
	             */
	            LEVMAR_TRANS_MAT_MAT_MULT(jac, jacTjac, nPts, paramNum);

	            /* cache efficient computation of J^T e */
	            for(i=0; i<paramNum; ++i)
	              jacTe[i]=0.0;

	            for(i=0; i<nPts; ++i){
	              int jacrowIndex;

	              for(l=0, jacrowIndex=i*paramNum, tmp=e[i]; l<paramNum; ++l)
	                jacTe[l]+=jac[jacrowIndex+l]*tmp;
	            }
	          }
	        
	        /* Compute ||J^T e||_inf and ||p||^2. Note that ||J^T e||_inf
	         * is computed for free (i.e. inactive) variables only. 
	         * At a local minimum, if p[i]==ub[i] then g[i]>0;
	         * if p[i]==lb[i] g[i]<0; otherwise g[i]=0 
	         */
	        for(i=j=numactive=0, p_L2=jacTe_inf=0.0; i<paramNum; ++i){
	          if((ub != null) && (param[i]==ub[i])){ ++numactive; if(jacTe[i]>0.0) ++j; }
	          else if((lb != null) && (param[i]==lb[i])){ ++numactive; if(jacTe[i]<0.0) ++j; }
	          else if(jacTe_inf < (tmp=Math.abs(jacTe[i]))) jacTe_inf=tmp;

	          diag_jacTjac[i]=jacTjac[i*paramNum+i]; /* save diagonal entries so that augmentation can be later canceled */
	          p_L2+=param[i]*param[i];
	        }
	        //p_L2=sqrt(p_L2);
	        
	        if (outputMes && ((k % 100) == 0)) {
	        	Preferences.debug("Current estimate:\n");
	        	for (i = 0; i < paramNum; ++i) {
	        		Preferences.debug("param[" + i + "] = " + param[i] + "\n");
	        	}
	        	Preferences.debug("jacTe_inf = " + jacTe_inf + "\n");
	        	Preferences.debug("p_eL2 = " + p_eL2 + "\n");
	        	Preferences.debug("numactive = " + numactive + "\n");
	        	Preferences.debug("j = " + j + "\n");
	        } // if(outputMes && ((k % 100) == 0))
	        /* check for convergence */
	        if(j==numactive && (jacTe_inf <= eps1)){
	          Dp_L2=0.0; /* no increment for p in this case */
	          stop=1;
	          break;
	        }

	       /* compute initial damping factor */
	        if(k==0){
	          if((lb == null) && (ub == null)){ /* no bounds */
	            for(i=0, tmp= -Double.MAX_VALUE; i<paramNum; ++i)
	              if(diag_jacTjac[i]>tmp) tmp=diag_jacTjac[i]; /* find max diagonal element */
	            mu=tau*tmp;
	          }
	          else 
	            mu=0.5*tau*p_eL2; /* use Kanzow's starting mu */
	        }

	        /* determine increment using a combination of adaptive damping, line search and projected gradient search */
	        while(true){
	          /* augment normal equations */
	          for(i=0; i<paramNum; ++i)
	            jacTjac[i*paramNum+i]+=mu;

	          /* solve augmented equations */
	    //#ifdef HAVE_LAPACK
	          /* 6 alternatives are available: LU, Cholesky, 2 variants of QR decomposition, SVD and LDLt.
	           * Cholesky is the fastest but might be inaccurate; QR is slower but more accurate;
	           * SVD is the slowest but most accurate; LU offers a tradeoff between accuracy and speed
	           */

	          //issolved=AX_EQ_B_BK(jacTjac, jacTe, Dp, m); ++nlss; linsolver=AX_EQ_B_BK;
	          //issolved=AX_EQ_B_LU(jacTjac, jacTe, Dp, m); ++nlss; linsolver=AX_EQ_B_LU;
	          //issolved=AX_EQ_B_CHOL(jacTjac, jacTe, Dp, m); ++nlss; linsolver=AX_EQ_B_CHOL;
	          //issolved=AX_EQ_B_QR(jacTjac, jacTe, Dp, m); ++nlss; linsolver=AX_EQ_B_QR;
	          //issolved=AX_EQ_B_QRLS(jacTjac, jacTe, Dp, m, m); ++nlss; linsolver=(int (*)(LM_REAL *A, LM_REAL *B, LM_REAL *x, int m))AX_EQ_B_QRLS;
	          //issolved=AX_EQ_B_SVD(jacTjac, jacTe, Dp, m); ++nlss; linsolver=AX_EQ_B_SVD;

	    //#else
	          /* use the LU included with levmar */
	          issolved=AX_EQ_B_LU(jacTjac, jacTe, Dp, paramNum); ++nlss; 
	          //linsolver=AX_EQ_B_LU;
	    //#endif /* HAVE_LAPACK */

	          if(issolved != 0){
	            for(i=0; i<paramNum; ++i)
	              pDp[i]=param[i] + Dp[i];

	            /* compute p's new estimate and ||Dp||^2 */
	            boxProject(pDp, lb, ub, paramNum); /* project to feasible set */
	            for(i=0, Dp_L2=0.0; i<paramNum; ++i){
	              Dp[i]=tmp=pDp[i]-param[i];
	              Dp_L2+=tmp*tmp;
	            }
	            //Dp_L2=sqrt(Dp_L2);

	            if(Dp_L2<=eps2_sq*p_L2){ /* relative change in p is small, stop */
	              stop=2;
	              break;
	            }

	            if(Dp_L2>=(p_L2+eps2)/(DOUBLE_EPSILON*DOUBLE_EPSILON)){ /* almost singular */
	              stop=4;
	              break;
	            }
                
	            /* evaluate function at p + Dp */
	            if (testMode) {
	            	fitToTestFunction(pDp, hx, paramNum, nPts);
	            }
	            else {
	                fitToFunction(pDp, hx, paramNum, nPts);
	            }
	            ++nfev[0]; 
	            /* ### hx=x-hx, pDp_eL2=||hx|| */
	            pDp_eL2[0]=LEVMAR_L2NRMXMY(hx, ySeries, hx, nPts);
	            if((Double.isInfinite(pDp_eL2[0])) || (Double.isNaN(pDp_eL2[0]))){
	              stop=7;
	              break;
	            }

	            if(pDp_eL2[0]<=gamma_sq*p_eL2){
	              for(i=0, dL=0.0; i<paramNum; ++i)
	                dL+=Dp[i]*(mu*Dp[i]+jacTe[i]);

	              if(dL>0.0){
	                dF=p_eL2-pDp_eL2[0];
	                tmp=(2.0*dF/dL-1.0);
	                tmp=1.0-tmp*tmp*tmp;
	                mu=mu*( (tmp>=ONE_THIRD)? tmp : ONE_THIRD );
	              }
	              else
	                mu=(mu>=pDp_eL2[0])? pDp_eL2[0] : mu; /* pDp_eL2 is the new pDp_eL2 */
	    

	              nu=2;

	              for(i=0 ; i<paramNum; ++i) /* update p's estimate */
	                param[i]=pDp[i];

	              for(i=0; i<nPts; ++i) /* update e and ||e||_2 */
	                e[i]=hx[i];
	              p_eL2=pDp_eL2[0];
	              ++nLMsteps;
	              gprevtaken=0;
	              break;
	            }
	          }
	          else{

	          /* the augmented linear system could not be solved, increase mu */

	            mu*=nu;
	            nu2=nu<<1; // 2*nu;
	            if(nu2<=nu){ /* nu has wrapped around (overflown). Thanks to Frank Jordan for spotting this case */
	              stop=5;
	              break;
	            }
	            nu=nu2;

	            for(i=0; i<paramNum; ++i) /* restore diagonal J^T J entries */
	              jacTjac[i*paramNum+i]=diag_jacTjac[i];

	            continue; /* solve again with increased nu */
	          }

	          /* if this point is reached, the LM step did not reduce the error;
	           * see if it is a descent direction
	           */

	          /* negate jacTe (i.e. g) & compute g^T * Dp */
	          for(i=0, jacTeDp=0.0; i<paramNum; ++i){
	            jacTe[i]=-jacTe[i];
	            jacTeDp+=jacTe[i]*Dp[i];
	          }

	          if(jacTeDp<=-rho*Math.pow(Dp_L2, 2.1/2.0)){
	        	dogradproj = false;
	            /* Dp is a descent direction; do a line search along it */
	            int mxtake[] = new int[1];
	            int iretcd[] = new int[1];
	            double stepmx;

	            tmp=Math.sqrt(p_L2); stepmx=1e3*( (tmp>=1.0)? tmp : 1.0 );

	            /* use Schnabel's backtracking line search; it requires fewer "func" evaluations */
	            LNSRCH(paramNum, param, p_eL2, jacTe, Dp, alpha, pDp, pDp_eL2, fstate_nPts,
	            	   fstate_nfev, fstate_hx, fstate_ySeries,
	                   mxtake, iretcd, stepmx, steptl, null); /* NOTE: LNSRCH() updates hx */
	            if(iretcd[0]!=0) {
	            	dogradproj = true; /* rather inelegant but effective way to handle LNSRCH() failures... */
	            }
	    
	            ++nLSsteps;
	            gprevtaken=0;

	            /* NOTE: new estimate for p is in pDp, associated error in hx and its norm in pDp_eL2.
	             * These values are used below to update their corresponding variables 
	             */
	          }
	          else {
	        	  dogradproj = true;
	          }
	          if (dogradproj){
	    gradproj: /* Note that this point can also be reached via a goto when LNSRCH() fails */

	            /* jacTe is a descent direction; make a projected gradient step */

	            /* if the previous step was along the gradient descent, try to use the t employed in that step */
	            /* compute ||g|| */
	            for(i=0, tmp=0.0; i<paramNum; ++i)
	              tmp+=jacTe[i]*jacTe[i];
	            tmp=Math.sqrt(tmp);
	            tmp=100.0/(1.0+tmp);
	            t0=(tmp<=tini)? tmp : tini; /* guard against poor scaling & large steps; see (3.50) in C.T. Kelley's book */

	            for(t = (gprevtaken != 0)? t : t0; t>tming; t*=beta){
	              for(i=0; i<paramNum; ++i)
	                pDp[i]=param[i] - t*jacTe[i];
	              boxProject(pDp, lb, ub, paramNum); /* project to feasible set */
	              for(i=0; i<paramNum; ++i)
	                Dp[i]=pDp[i]-param[i];
	              /* evaluate function at p - t*g */
	              if (testMode) {
	            	  fitToTestFunction(pDp, hx, paramNum, nPts);
	              }
	              else {
	                  fitToFunction(pDp, hx, paramNum, nPts);
	              }
	              ++nfev[0];
	              /* compute ||e(pDp)||_2 */
	              /* ### hx=x-hx, pDp_eL2=||hx|| */
	              pDp_eL2[0]=LEVMAR_L2NRMXMY(hx, ySeries, hx, nPts);
	    
	              if((Double.isInfinite(pDp_eL2[0])) || (Double.isNaN(pDp_eL2[0]))){
	                stop=7;
	                break mainLoop;
	              }

	              for(i=0, tmp=0.0; i<paramNum; ++i) /* compute ||g^T * Dp|| */
	                tmp+=jacTe[i]*Dp[i];

	              if((gprevtaken != 0) && pDp_eL2[0]<=p_eL2 + 2.0*0.99999*tmp){ /* starting t too small */
	                t=t0;
	                gprevtaken=0;
	                continue;
	              }
	              //if(LM_CNST(0.5)*pDp_eL2<=LM_CNST(0.5)*p_eL2 + alpha*tmp) break;
	              if(pDp_eL2[0]<=p_eL2 + 2.0*alpha*tmp) break;
	            }

	            ++nPGsteps;
	            gprevtaken=1;
	            /* NOTE: new estimate for p is in pDp, associated error in hx and its norm in pDp_eL2 */
	          }

	          /* update using computed values */

	          for(i=0, Dp_L2=0.0; i<paramNum; ++i){
	            tmp=pDp[i]-param[i];
	            Dp_L2+=tmp*tmp;
	          }
	          //Dp_L2=sqrt(Dp_L2);

	          if(Dp_L2<=eps2_sq*p_L2){ /* relative change in p is small, stop */
	            stop=2;
	            break;
	          }

	          for(i=0 ; i<paramNum; ++i) /* update p's estimate */
	            param[i]=pDp[i];

	          for(i=0; i<nPts; ++i) /* update e and ||e||_2 */
	            e[i]=hx[i];
	          p_eL2=pDp_eL2[0];
	          break;
	        } /* inner loop */
	      }

	      if(k>=maxIterations) stop=3;

	      for(i=0; i<paramNum; ++i) /* restore diagonal J^T J entries */
	        jacTjac[i*paramNum+i]=diag_jacTjac[i];

	      if(info != null){
	        info[0]=init_p_eL2;
	        info[1]=p_eL2;
	        info[2]=jacTe_inf;
	        info[3]=Dp_L2;
	        for(i=0, tmp=-Double.MAX_VALUE; i<paramNum; ++i)
	          if(tmp<jacTjac[i*paramNum+i]) tmp=jacTjac[i*paramNum+i];
	        info[4]=mu/tmp;
	        info[5]=(double)k;
	        info[6]=(double)stop;
	        info[7]=(double)nfev[0];
	        info[8]=(double)njev;
	        info[9]=(double)nlss;
	      }

	      
        if (outputMes) {
	        Preferences.debug( nLMsteps + " LM steps, " +  nLSsteps + " line search, " +  nPGsteps + " projected gradient\n");
        }

	    return (stop!=4 && stop!=7)?  k : LM_ERROR;
	} // public void driver()
	
	private void
	LNSRCH(int m, double x[], double f, double g[], double p[], double alpha, double xpls[],
	       double ffpls[], int fstate_nPts, int fstate_nfev[], double fstate_hx[], double fstate_ySeries[],
	       int mxtake[], int iretcd[], double stepmx, double steptl, double sx[])
	{
	/* Find a next newton iterate by backtracking line search.
	 * Specifically, finds a \lambda such that for a fixed alpha<0.5 (usually 1e-4),
	 * f(x + \lambda*p) <= f(x) + alpha * \lambda * g^T*p
	 *
	 * Translated (with minor changes) from Schnabel, Koontz & Weiss uncmin.f,  v1.3

	 * PARAMETERS :

	 *	m       --> dimension of problem (i.e. number of variables)
	 *	x(m)    --> old iterate:	x[k-1]
	 *	f       --> function value at old iterate, f(x)
	 *	g(m)    --> gradient at old iterate, g(x), or approximate
	 *	p(m)    --> non-zero newton step
	 *	alpha   --> fixed constant < 0.5 for line search (see above)
	 *	xpls(m) <--	 new iterate x[k]
	 *	ffpls   <--	 function value at new iterate, f(xpls)
	 *	func    --> name of subroutine to evaluate function
	 *	state   <--> information other than x and m that func requires.
	 *			    state is not modified in xlnsrch (but can be modified by func).
	 *	iretcd  <--	 return code
	 *	mxtake  <--	 boolean flag indicating step of maximum length used
	 *	stepmx  --> maximum allowable step size
	 *	steptl  --> relative step size at which successive iterates
	 *			    considered close enough to terminate algorithm
	 *	sx(m)	  --> diagonal scaling matrix for x, can be NULL

	 *	internal variables

	 *	sln		 newton length
	 *	rln		 relative length of newton step
	*/

	    int i, j;
	    int firstback = 1;
	    double disc;
	    double a3, b;
	    double t1, t2, t3, lambda, tlmbda, rmnlmb;
	    double scl, rln, sln, slp;
	    double tmp1, tmp2;
	    double fpls, pfpls = 0., plmbda = 0.; /* -Wall */

	    f*=0.5;
	    mxtake[0] = 0;
	    iretcd[0] = 2;
	    tmp1 = 0.;
	    if(sx == null) /* no scaling */
	      for (i = 0; i < m; ++i)
	        tmp1 += p[i] * p[i];
	    else
	      for (i = 0; i < m; ++i)
	        tmp1 += sx[i] * sx[i] * p[i] * p[i];
	    sln = Math.sqrt(tmp1);
	    if (sln > stepmx) {
		  /*	newton step longer than maximum allowed */
		    scl = stepmx / sln;
	      for(i=0; i<m; ++i) /* p * scl */
	        p[i]*=scl;
		    sln = stepmx;
	    }
	    for(i=0, slp=0.; i<m; ++i) /* g^T * p */
	      slp+=g[i]*p[i];
	    rln = 0.;
	    if(sx == null) /* no scaling */
	      for (i = 0; i < m; ++i) {
		      tmp1 = (Math.abs(x[i])>=1.)? Math.abs(x[i]) : 1.;
		      tmp2 = Math.abs(p[i])/tmp1;
		      if(rln < tmp2) rln = tmp2;
	      }
	    else
	      for (i = 0; i < m; ++i) {
		      tmp1 = (Math.abs(x[i])>=1./sx[i])? Math.abs(x[i]) : 1./sx[i];
		      tmp2 = Math.abs(p[i])/tmp1;
		      if(rln < tmp2) rln = tmp2;
	      }
	    rmnlmb = steptl / rln;
	    lambda = 1.0;

	    /*	check if new iterate satisfactory.  generate new lambda if necessary. */

	    for(j=LSITMAX; j>=0; --j) {
		    for (i = 0; i < m; ++i)
		      xpls[i] = x[i] + lambda * p[i];

	      /* evaluate function at new point */
		  if (testMode) {
			  fitToTestFunction(xpls, fstate_hx, m, fstate_nPts);
		  }
		  else {
	          fitToFunction(xpls, fstate_hx, m, fstate_nPts);
		  }
	      ++(fstate_nfev[0]);
	      /* ### state.hx=state.x-state.hx, tmp1=||state.hx|| */
	      tmp1=LEVMAR_L2NRMXMY(fstate_hx, fstate_ySeries, fstate_hx, fstate_nPts);
	
	      fpls=0.5*tmp1; ffpls[0]=tmp1;

		    if (fpls <= f + slp * alpha * lambda) { /* solution found */
		      iretcd[0] = 0;
		      if (lambda == 1. && sln > stepmx * .99) mxtake[0] = 1;
		      return;
		    }

		    /* else : solution not (yet) found */

	      /* First find a point with a finite value */

		    if (lambda < rmnlmb) {
		      /* no satisfactory xpls found sufficiently distinct from x */

		      iretcd[0] = 1;
		      return;
		    }
		    else { /*	calculate new lambda */

		      /* modifications to cover non-finite values */
		      if ((Double.isInfinite(fpls)) || (Double.isNaN(fpls))){
			      lambda *= 0.1;
			      firstback = 1;
		      }
		      else {
			      if (firstback != 0) { /*	first backtrack: quadratic fit */
			        tlmbda = -lambda * slp / ((fpls - f - slp) * 2.);
			        firstback = 0;
			      }
			      else { /*	all subsequent backtracks: cubic fit */
			        t1 = fpls - f - lambda * slp;
			        t2 = pfpls - f - plmbda * slp;
			        t3 = 1. / (lambda - plmbda);
			        a3 = 3. * t3 * (t1 / (lambda * lambda)
					      - t2 / (plmbda * plmbda));
			        b = t3 * (t2 * lambda / (plmbda * plmbda)
				          - t1 * plmbda / (lambda * lambda));
			        disc = b * b - a3 * slp;
			        if (disc > b * b)
				      /* only one positive critical point, must be minimum */
				        tlmbda = (-b + ((a3 < 0)? -Math.sqrt(disc): Math.sqrt(disc))) /a3;
			        else
				      /* both critical points positive, first is minimum */
				        tlmbda = (-b + ((a3 < 0)? Math.sqrt(disc): -Math.sqrt(disc))) /a3;

			        if (tlmbda > lambda * .5)
				        tlmbda = lambda * .5;
			      }
			      plmbda = lambda;
			      pfpls = fpls;
			      if (tlmbda < lambda * .1)
			        lambda *= .1;
			      else
			        lambda = tlmbda;
	        }
		    }
	    }
	    /* this point is reached when the iterations limit is exceeded */
		  iretcd[0] = 1; /* failed */
		  return;
	} /* LNSRCH */
	
	void LEVMAR_TRANS_MAT_MAT_MULT(double a[], double b[], int n, int m)
	{
	//#ifdef HAVE_LAPACK /* use BLAS matrix multiply */

	//double alpha=1.0;
	//double beta=0.0;
	  /* Fool BLAS to compute a^T*a avoiding transposing a: a is equivalent to a^T in column major,
	   * therefore BLAS computes a*a^T with a and a*a^T in column major, which is equivalent to
	   * computing a^T*a in row major!
	   */
	  //GEMM("N", "T", &m, &m, &n, &alpha, a, &m, a, &m, &beta, b, &m);

	//#else /* no LAPACK, use blocking-based multiply */

	int i, j, k, jj, kk;
	double sum;
	int bimIndex, akmIndex;
	final int bsize= BLOCKSZ;

	

	  /* compute upper triangular part using blocking */
	  for(jj=0; jj<m; jj+=bsize){
	    for(i=0; i<m; ++i){
	      bimIndex=i*m;
	      for(j=Math.max(jj, i); j< Math.min(jj+bsize, m); ++j)
	        b[bimIndex+j]=0.0; //b[i*m+j]=0.0;
	    }

	    for(kk=0; kk<n; kk+=bsize){
	      for(i=0; i<m; ++i){
	        bimIndex=i*m;
	        for(j=Math.max(jj, i); j< Math.min(jj+bsize, m); ++j){
	          sum=0.0;
	          for(k=kk; k< Math.min(kk+bsize, n); ++k){
	            akmIndex=k*m;
	            sum+=a[akmIndex+i]*a[akmIndex+j]; //a[k*m+i]*a[k*m+j];
	          }
	          b[bimIndex+j]+=sum; //b[i*m+j]+=sum;
	        }
	      }
	    }
	  }

	  /* copy upper triangular part to the lower one */
	  for(i=0; i<m; ++i)
	    for(j=0; j<i; ++j)
	      b[i*m+j]=b[j*m+i];

	//#endif /* HAVE_LAPACK */
	}
	
	/*
	 * This function returns the solution of Ax = b
	 *
	 * The function employs LU decomposition followed by forward/back substitution (see 
	 * also the LAPACK-based LU solver above)
	 *
	 * A is mxm, b is mx1
	 *
	 * The function returns 0 in case of error, 1 if successful
	 *
	 * This function is often called repetitively to solve problems of identical
	 * dimensions. To avoid repetitive malloc's and free's, allocated memory is
	 * retained between calls and free'd-malloc'ed when not of the appropriate size.
	 * A call with NULL as the first argument forces this memory to be released.
	 */
	int AX_EQ_B_LU(double A[], double B[], double x[], int m)
	{
	int i, j, k;
	int idx[], maxi=-1, idx_sz, a_sz, work_sz;
	double a[], work[], max, sum, tmp;
	   
	  /* calculate required memory size */
	  idx_sz=m;
	  a_sz=m*m;
	  work_sz=m;

	
	  a = new double[a_sz];
	  work=new double[work_sz];
	  idx= new int[idx_sz];

	  /* avoid destroying A, B by copying them to a, x resp. */
	  for(i=0; i<m; ++i){ // B & 1st row of A
	    a[i]=A[i];
	    x[i]=B[i];
	  }
	  for(  ; i<a_sz; ++i) a[i]=A[i]; // copy A's remaining rows
	  /****
	  for(i=0; i<m; ++i){
	    for(j=0; j<m; ++j)
	      a[i*m+j]=A[i*m+j];
	    x[i]=B[i];
	  }
	  ****/

	  /* compute the LU decomposition of a row permutation of matrix a; the permutation itself is saved in idx[] */
		for(i=0; i<m; ++i){
			max=0.0;
			for(j=0; j<m; ++j)
				if((tmp=Math.abs(a[i*m+j]))>max)
	        max=tmp;
			  if(max==0.0){
		          if (outputMes) {
	                  Preferences.debug("Singular matrix A in AX_EQ_B_LU\n");
		          }

	        return 0;
	      }
			  work[i]=1.0/max;
		}

		for(j=0; j<m; ++j){
			for(i=0; i<j; ++i){
				sum=a[i*m+j];
				for(k=0; k<i; ++k)
	        sum-=a[i*m+k]*a[k*m+j];
				a[i*m+j]=sum;
			}
			max=0.0;
			for(i=j; i<m; ++i){
				sum=a[i*m+j];
				for(k=0; k<j; ++k)
	        sum-=a[i*m+k]*a[k*m+j];
				a[i*m+j]=sum;
				if((tmp=work[i]*Math.abs(sum))>=max){
					max=tmp;
					maxi=i;
				}
			}
			if(j!=maxi){
				for(k=0; k<m; ++k){
					tmp=a[maxi*m+k];
					a[maxi*m+k]=a[j*m+k];
					a[j*m+k]=tmp;
				}
				work[maxi]=work[j];
			}
			idx[j]=maxi;
			if(a[j*m+j]==0.0)
	      a[j*m+j]=DOUBLE_EPSILON;
			if(j!=m-1){
				tmp=1.0/(a[j*m+j]);
				for(i=j+1; i<m; ++i)
	        a[i*m+j]*=tmp;
			}
		}

	  /* The decomposition has now replaced a. Solve the linear system using
	   * forward and back substitution
	   */
		for(i=k=0; i<m; ++i){
			j=idx[i];
			sum=x[j];
			x[j]=x[i];
			if(k!=0)
				for(j=k-1; j<i; ++j)
	        sum-=a[i*m+j]*x[j];
			else
	      if(sum!=0.0)
				  k=i+1;
			x[i]=sum;
		}

		for(i=m-1; i>=0; --i){
			sum=x[i];
			for(j=i+1; j<m; ++j)
	      sum-=a[i*m+j]*x[j];
			x[i]=sum/a[i*m+i];
		}

	  return 1;
	}
	
	/* Compute e=x-y for two n-vectors x and y and return the squared L2 norm of e.
	 * e can coincide with either x or y; x can be NULL, in which case it is assumed
	 * to be equal to the zero vector.
	 * Uses loop unrolling and blocking to reduce bookkeeping overhead & pipeline
	 * stalls and increase instruction-level parallelism; see http://www.abarnett.demon.co.uk/tutorial.html
	 */

	private double LEVMAR_L2NRMXMY(double e[], double x[], double y[], int n)
	{
	final int blocksize=8, bpwr=3; /* 8=2^3 */
	int i;
	int j1, j2, j3, j4, j5, j6, j7;
	int blockn;
	double sum0=0.0, sum1=0.0, sum2=0.0, sum3=0.0;

	  /* n may not be divisible by blocksize, 
	   * go as near as we can first, then tidy up.
	   */ 
	  blockn = (n>>bpwr)<<bpwr; /* (n / blocksize) * blocksize; */

	  /* unroll the loop in blocks of `blocksize'; looping downwards gains some more speed */
	  if(x != null){
	    for(i=blockn-1; i>0; i-=blocksize){
	              e[i ]=x[i ]-y[i ]; sum0+=e[i ]*e[i ];
	      j1=i-1; e[j1]=x[j1]-y[j1]; sum1+=e[j1]*e[j1];
	      j2=i-2; e[j2]=x[j2]-y[j2]; sum2+=e[j2]*e[j2];
	      j3=i-3; e[j3]=x[j3]-y[j3]; sum3+=e[j3]*e[j3];
	      j4=i-4; e[j4]=x[j4]-y[j4]; sum0+=e[j4]*e[j4];
	      j5=i-5; e[j5]=x[j5]-y[j5]; sum1+=e[j5]*e[j5];
	      j6=i-6; e[j6]=x[j6]-y[j6]; sum2+=e[j6]*e[j6];
	      j7=i-7; e[j7]=x[j7]-y[j7]; sum3+=e[j7]*e[j7];
	    }

	   /*
	    * There may be some left to do.
	    * This could be done as a simple for() loop, 
	    * but a switch is faster (and more interesting) 
	    */ 

	    i=blockn;
	    if(i<n){ 
	      /* Jump into the case at the place that will allow
	       * us to finish off the appropriate number of items. 
	       */ 

	      switch(n - i){ 
	        case 7 : e[i]=x[i]-y[i]; sum0+=e[i]*e[i]; ++i;
	        case 6 : e[i]=x[i]-y[i]; sum1+=e[i]*e[i]; ++i;
	        case 5 : e[i]=x[i]-y[i]; sum2+=e[i]*e[i]; ++i;
	        case 4 : e[i]=x[i]-y[i]; sum3+=e[i]*e[i]; ++i;
	        case 3 : e[i]=x[i]-y[i]; sum0+=e[i]*e[i]; ++i;
	        case 2 : e[i]=x[i]-y[i]; sum1+=e[i]*e[i]; ++i;
	        case 1 : e[i]=x[i]-y[i]; sum2+=e[i]*e[i]; //++i;
	      }
	    }
	  }
	  else{ /* x==0 */
	    for(i=blockn-1; i>0; i-=blocksize){
	              e[i ]=-y[i ]; sum0+=e[i ]*e[i ];
	      j1=i-1; e[j1]=-y[j1]; sum1+=e[j1]*e[j1];
	      j2=i-2; e[j2]=-y[j2]; sum2+=e[j2]*e[j2];
	      j3=i-3; e[j3]=-y[j3]; sum3+=e[j3]*e[j3];
	      j4=i-4; e[j4]=-y[j4]; sum0+=e[j4]*e[j4];
	      j5=i-5; e[j5]=-y[j5]; sum1+=e[j5]*e[j5];
	      j6=i-6; e[j6]=-y[j6]; sum2+=e[j6]*e[j6];
	      j7=i-7; e[j7]=-y[j7]; sum3+=e[j7]*e[j7];
	    }

	   /*
	    * There may be some left to do.
	    * This could be done as a simple for() loop, 
	    * but a switch is faster (and more interesting) 
	    */ 

	    i=blockn;
	    if(i<n){ 
	      /* Jump into the case at the place that will allow
	       * us to finish off the appropriate number of items. 
	       */ 

	      switch(n - i){ 
	        case 7 : e[i]=-y[i]; sum0+=e[i]*e[i]; ++i;
	        case 6 : e[i]=-y[i]; sum1+=e[i]*e[i]; ++i;
	        case 5 : e[i]=-y[i]; sum2+=e[i]*e[i]; ++i;
	        case 4 : e[i]=-y[i]; sum3+=e[i]*e[i]; ++i;
	        case 3 : e[i]=-y[i]; sum0+=e[i]*e[i]; ++i;
	        case 2 : e[i]=-y[i]; sum1+=e[i]*e[i]; ++i;
	        case 1 : e[i]=-y[i]; sum2+=e[i]*e[i]; //++i;
	      }
	    }
	  }

	  return sum0+sum1+sum2+sum3;
	}

	
	private void boxProject(double p[], double lb[], double ub[], int m) {
		int i;
		if (lb == null) { // no lower bounds
			if (ub == null) { // no upper bounds
				return;
			}
			else { // upper bounds only
				for (i = 0; i < m; ++i) {
					if (p[i] > ub[i]) {
						p[i] = ub[i];
					}
				}
			}
		} // if (lb == null) 
		else {
			if (ub == null) { // lower bounds only
				for (i = 0; i < m; ++i) {
					if (p[i] < lb[i]) {
						p[i] = lb[i];
					}
				}
			} // if (ub == null)
			else { // box bounds
				for (i = 0; i < m; ++i) {
					p[i] = median3(lb[i], p[i], ub[i]);
				}
			}
		} // else
	} // boxProject
	
	private double median3(double a, double b, double c) {
		if (a >= b) {
			if (c >= a) {
				return a;
			} // if (c >= a)
			else if (c <= b) {
				return b;
			}
			else {
				return c;
			}
		} // if (a >= b)
		else if (c >= b) {
			return b;
		}
		else if (c <= a) {
			return a;
		}
		else {
			return c;
		}
	}
	
	private void releps() {
        // COMPUTE SRELPR = DOUBLE RELATIVE PRECISION FOR A BINARY
        // MACHINE   I.E.
        // DETERMINE THE SMALLEST POSITIVE NUMBER 0.5**K FOR WHICH
        // (1.0+0.5**K) > 1.0  AND  (1.0+0.5**(K+1)) = 1.0
        // WHERE K IS A POSITIVE INTEGER

        double temp, frac;
        boolean loop = true;
        frac = 1.0;

        while (loop) {
            frac = 0.5 * frac;
            temp = frac + 1.0;

            if (temp == 1.0) {
                break;
            }
        } // while (loop)

        DOUBLE_EPSILON = 2.0 * frac;

        return;
    }

}