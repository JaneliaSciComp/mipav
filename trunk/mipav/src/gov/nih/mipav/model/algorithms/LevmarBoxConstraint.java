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
	
	// Double precision routine variables found in routine dlamch
    private double base;
    private double emax;
    private double emin;
    private double eps;
    private boolean first = true;
    private double prec;
    private double rmax;
    private double rmin;
    private double rnd;
    private double sfmin;
    private double t;
	private double info[];
	
	/** Double precison machine variables found in routine dlartg. */
    private boolean first_dlartg = true;
    private double safmin;
    private double safmn2;
    private double safmx2;
    
    private boolean dlasq2Error = false;
    private double dOrg[];
    private double dSort[];
	
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
	
	/*
	 * This function returns the solution of Ax = b
	 *
	 * The function is based on SVD decomposition:
	 * If A=U D V^T with U, V orthogonal and D diagonal, the linear system becomes
	 * (U D V^T) x = b or x=V D^{-1} U^T b
	 * Note that V D^{-1} U^T is the pseudoinverse A^+
	 *
	 * A is mxm, b is mx1.
	 *
	 * The function returns 0 in case of error, 1 if successful
	 *
	 * This function is often called repetitively to solve problems of identical
	 * dimensions. To avoid repetitive malloc's and free's, allocated memory is
	 * retained between calls and free'd-malloc'ed when not of the appropriate size.
	 * A call with NULL as the first argument forces this memory to be released.
	 */
	int AX_EQ_B_SVD(double A[], double B[], double x[], int m)
	{

	int i, j;
	double a[][], u[][], s[], vt[][], work[];
	int a_sz, u_sz, s_sz, vt_sz, tot_sz;
	double thresh[] = new double[1];
	double one_over_denom;
	double sum;
	int info[] = new int[1];
	int rank, worksz, iwork[], iworksz;
	
	if (A ==  null) {
		return 1;
	}
	   
	   
	   
	  /* calculate required memory size */
	//#if 1 /* use optimal size */
	  worksz=-1; // workspace query. Keep in mind that GESDD requires more memory than GESVD
	  /* note that optimal work size is returned in thresh */
	  dgesvd('A', 'A', m, m, null, m, null, null, m, null, m, thresh, worksz, info);
	  //GESDD("A", (int *)&m, (int *)&m, NULL, (int *)&m, NULL, NULL, (int *)&m, NULL, (int *)&m, (LM_REAL *)&thresh, (int *)&worksz, NULL, &info);
	  worksz=(int)thresh[0];
	//#else /* use minimum size */
	  worksz=5*m; // min worksize for GESVD
	  //worksz=m*(7*m+4); // min worksize for GESDD
	//#endif
	  iworksz=8*m;
	  a_sz=m*m;
	  u_sz=m*m; s_sz=m; vt_sz=m*m;

	  

	  a=new double[m][m];
	  u=new double[m][m];
	  s=new double[s_sz];
	  vt=new double[m][m];
	  work=new double[worksz];
	  iwork=new int[iworksz];

	  /* store A (column major!) into a */
	  for(i=0; i<m; i++)
	    for(j=0; j<m; j++)
	      a[i][j]=A[i*m+j];

	  /* SVD decomposition of A */
	  dgesvd('A', 'A', m, m, a, m, s, u, m, vt, m, work, worksz, info);
	  //GESDD("A", (int *)&m, (int *)&m, a, (int *)&m, s, u, (int *)&m, vt, (int *)&m, work, (int *)&worksz, iwork, &info);

	  /* error treatment */
	  if(info[0]!=0){
	    if(info[0]<0){
	      Preferences.debug("LAPACK error: illegal value for argument " + (-info[0]) +  " of GESVD in AX_EQ_B_SVD\n");
	      return info[0];
	    }
	    else{
	      Preferences.debug("LAPACK error: dgesdd (dbdsdc)/dgesvd (dbdsqr) failed to converge in AX_EQ_B_SVD, info[0]= "
	    		            + info[0] + "\n");
	
	      return 0;
	    }
	  }

	  

	  /* compute the pseudoinverse in a */
		for(i=0; i<m; i++)
			for (j = 0; j < m; j++)
			    a[i][j]=0.0; /* initialize to zero */
	  for(rank=0, thresh[0]=DOUBLE_EPSILON*s[0]; rank<m && s[rank]>thresh[0]; rank++){
	    one_over_denom=1.0/s[rank];

	    for(j=0; j<m; j++)
	      for(i=0; i<m; i++)
	        a[j][i]+=vt[rank][i]*u[j][rank]*one_over_denom;
	  }

		/* compute A^+ b in x */
		for(i=0; i<m; i++){
		  for(j=0, sum=0.0; j<m; j++)
	      sum+=a[j][i]*B[j];
	    x[i]=sum;
	  }

		return 1;
	}
	
    private void dgesvd(char JOBU, char JOBVT, int M, int N, double A[][], int LDA, double S[],
    		            double U[][], int LDU, double VT[][], int LDVT,
    	                    double WORK[], int LWORK, int INFO[] ) {
    	/*
    	*-- LAPACK driver routine (version 3.2) --
    	*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    	*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    	*     November 2006
    	*
    	*     .. Scalar Arguments ..
    	      CHARACTER          JOBU, JOBVT
    	      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
    	*     ..
    	*     .. Array Arguments ..
    	      DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
    	     $                   VT( LDVT, * ), WORK( * )
    	*     ..
    	*
    	*  Purpose
    	*  =======
    	*
    	*  DGESVD computes the singular value decomposition (SVD) of a real
    	*  M-by-N matrix A, optionally computing the left and/or right singular
    	*  vectors. The SVD is written
    	*
    	*       A = U * SIGMA * transpose(V)
    	*
    	*  where SIGMA is an M-by-N matrix which is zero except for its
    	*  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
    	*  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
    	*  are the singular values of A; they are real and non-negative, and
    	*  are returned in descending order.  The first min(m,n) columns of
    	*  U and V are the left and right singular vectors of A.
    	*
    	*  Note that the routine returns V**T, not V.
    	*
    	*  Arguments
    	*  =========
    	*
    	*  JOBU    (input) CHARACTER*1
    	*          Specifies options for computing all or part of the matrix U:
    	*          = 'A':  all M columns of U are returned in array U:
    	*          = 'S':  the first min(m,n) columns of U (the left singular
    	*                  vectors) are returned in the array U;
    	*          = 'O':  the first min(m,n) columns of U (the left singular
    	*                  vectors) are overwritten on the array A;
    	*          = 'N':  no columns of U (no left singular vectors) are
    	*                  computed.
    	*
    	*  JOBVT   (input) CHARACTER*1
    	*          Specifies options for computing all or part of the matrix
    	*          V**T:
    	*          = 'A':  all N rows of V**T are returned in the array VT;
    	*          = 'S':  the first min(m,n) rows of V**T (the right singular
    	*                  vectors) are returned in the array VT;
    	*          = 'O':  the first min(m,n) rows of V**T (the right singular
    	*                  vectors) are overwritten on the array A;
    	*          = 'N':  no rows of V**T (no right singular vectors) are
    	*                  computed.
    	*
    	*          JOBVT and JOBU cannot both be 'O'.
    	*
    	*  M       (input) INTEGER
    	*          The number of rows of the input matrix A.  M >= 0.
    	*
    	*  N       (input) INTEGER
    	*          The number of columns of the input matrix A.  N >= 0.
    	*
    	*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
    	*          On entry, the M-by-N matrix A.
    	*          On exit,
    	*          if JOBU = 'O',  A is overwritten with the first min(m,n)
    	*                          columns of U (the left singular vectors,
    	*                          stored columnwise);
    	*          if JOBVT = 'O', A is overwritten with the first min(m,n)
    	*                          rows of V**T (the right singular vectors,
    	*                          stored rowwise);
    	*          if JOBU .ne. 'O' and JOBVT .ne. 'O', the contents of A
    	*                          are destroyed.
    	*
    	*  LDA     (input) INTEGER
    	*          The leading dimension of the array A.  LDA >= max(1,M).
    	*
    	*  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
    	*          The singular values of A, sorted so that S(i) >= S(i+1).
    	*
    	*  U       (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
    	*          (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
    	*          If JOBU = 'A', U contains the M-by-M orthogonal matrix U;
    	*          if JOBU = 'S', U contains the first min(m,n) columns of U
    	*          (the left singular vectors, stored columnwise);
    	*          if JOBU = 'N' or 'O', U is not referenced.
    	*
    	*  LDU     (input) INTEGER
    	*          The leading dimension of the array U.  LDU >= 1; if
    	*          JOBU = 'S' or 'A', LDU >= M.
    	*
    	*  VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
    	*          If JOBVT = 'A', VT contains the N-by-N orthogonal matrix
    	*          V**T;
    	*          if JOBVT = 'S', VT contains the first min(m,n) rows of
    	*          V**T (the right singular vectors, stored rowwise);
    	*          if JOBVT = 'N' or 'O', VT is not referenced.
    	*
    	*  LDVT    (input) INTEGER
    	*          The leading dimension of the array VT.  LDVT >= 1; if
    	*          JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
    	*
    	*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
    	*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
    	*          if INFO > 0, WORK(2:MIN(M,N)) contains the unconverged
    	*          superdiagonal elements of an upper bidiagonal matrix B
    	*          whose diagonal is in S (not necessarily sorted). B
    	*          satisfies A = U * B * VT, so it has the same singular values
    	*          as A, and singular vectors related by U and VT.
    	*
    	*  LWORK   (input) INTEGER
    	*          The dimension of the array WORK.
    	*          LWORK >= MAX(1,3*MIN(M,N)+MAX(M,N),5*MIN(M,N)).
    	*          For good performance, LWORK should generally be larger.
    	*
    	*          If LWORK = -1, then a workspace query is assumed; the routine
    	*          only calculates the optimal size of the WORK array, returns
    	*          this value as the first entry of the WORK array, and no error
    	*          message related to LWORK is issued by XERBLA.
    	*
    	*  INFO    (output) INTEGER
    	*          = 0:  successful exit.
    	*          < 0:  if INFO = -i, the i-th argument had an illegal value.
    	*          > 0:  if DBDSQR did not converge, INFO specifies how many
    	*                superdiagonals of an intermediate bidiagonal form B
    	*                did not converge to zero. See the description of WORK
    	*                above for details.
    	*
    	*  =====================================================================
    	*/
    	
    	      boolean            LQUERY, WNTUA, WNTUAS, WNTUN, WNTUO, WNTUS,
    	                         WNTVA, WNTVAS, WNTVN, WNTVO, WNTVS;
    	      int MNTHR = 0;
    	      int BDSPAC = 0;
    	      int WRKBL = 0;
    	      int NCU = 0;
    	      int NCVT = 0;
    	      int NRU = 0;
    	      int NRVT = 0;
    	      int                BLK, CHUNK, I, IE, IR, ISCL,
    	                         ITAU, ITAUP, ITAUQ, IU, IWORK, LDWRKR, LDWRKU,
    	                         MAXWRK, MINMN, MINWRK;
    	      int IERR[] = new int[1];
    	      double   ANRM, BIGNUM, SMLNUM;
    	      double WORK2[];
    	      double WORK3[];
    	      double WORK4[];
    	      double WORK5[];
    	      double WORK6[];
    	      double BUF[];
    	      double ARRAY[][];
    	      double ARRAY2[][];
    	      double ARRAY3[][];
    	      int IROW;
    	      int ICOL;
    	      int i, j, k;
    	
    	      double   DUM[] = new double[1];
    	      double   DUM2[][] = new double[1][0];
    	      String name;
    	      String opts;
    	      char optsC[] = new char[2];
    	
    	
    	//     Test the input arguments
    	      INFO[0] = 0;
    	      MINMN = Math.min( M, N );
    	      WNTUA = (( JOBU == 'A' ) || (JOBU == 'a'));
    	      WNTUS = ( (JOBU =='S' ) || (JOBU == 's'));
    	      WNTUAS = WNTUA || WNTUS;
    	      WNTUO = (( JOBU == 'O' ) || (JOBU == 'o'));
    	      WNTUN = (( JOBU == 'N' ) || (JOBU == 'n'));
    	      WNTVA = (( JOBVT == 'A' ) || (JOBVT == 'a'));
    	      WNTVS = (( JOBVT == 'S' ) || (JOBVT == 's'));
    	      WNTVAS = WNTVA || WNTVS;
    	      WNTVO = (( JOBVT == 'O' ) || (JOBVT == 'o'));
    	      WNTVN = (( JOBVT == 'N' ) || (JOBVT == 'n'));
    	      LQUERY = ( LWORK == -1 );
    	
    	      if( !( WNTUA || WNTUS || WNTUO || WNTUN ) ) {
    	         INFO[0] = -1;
    	      }
    	      else if(!( WNTVA || WNTVS || WNTVO || WNTVN ) ||
    	              ( WNTVO && WNTUO ) ) {
    	         INFO[0] = -2;
    	      }
    	      else if( M < 0 ) {
    	         INFO[0] = -3;
    	      }
    	      else if ( N < 0 ) {
    	         INFO[0] = -4;
    	      }
    	      else if ( LDA < Math.max( 1, M ) ) {
    	         INFO[0] = -6;
    	      }
    	      else if ( LDU < 1 || ( WNTUAS && LDU < M ) ) {
    	         INFO[0] = -9;
    	      }
    	      else if ( LDVT < 1 || ( WNTVA && LDVT < N ) ||
    	              ( WNTVS && LDVT < MINMN ) ) {
    	         INFO[0] = -11;
    	      }
    	/*
    	*     Compute workspace
    	*      (Note: Comments in the code beginning "Workspace:" describe the
    	*       minimal amount of workspace needed at that point in the code,
    	*       as well as the preferred amount for good performance.
    	*       NB refers to the optimal block size for the immediately
    	*       following subroutine, as returned by ILAENV.)
    	*/
    	      if( INFO[0] == 0 ) {
    	         MINWRK = 1;
    	         MAXWRK = 1;
    	         if ( M >= N && MINMN > 0 ) {
    	
    	//           Compute space needed for DBDSQR
    	
    	             name = new String("DGESVD");
    	             optsC[0] = JOBU;
    	             optsC[1] = JOBVT;
    	             opts = new String(optsC);
    	        	 MNTHR = ilaenv( 6, name, opts, M, N, 0, 0 );
    	            BDSPAC = 5*N;
    	            if( M >= MNTHR ) {
    	               if( WNTUN ) {
    	
    	//                 Path 1 (M much larger than N, JOBU='N')
    	                  name = new String("DGEQRF");
    	                  opts = new String(" ");
    	                  MAXWRK = N + N*ilaenv( 1, name, opts, M, N, -1, -1 );
    	                  MAXWRK = Math.max( MAXWRK, 3*N+2*N*
    	                          ilaenv( 1, "DGEBRD", " " , N, N, -1, -1 ) );
    	                  if( WNTVO || WNTVAS ) {
    	                    MAXWRK = Math.max( MAXWRK, 3*N+( N-1 )*
    	                             ilaenv( 1, "DORGBR", "P", N, N, N, -1 ) );
    	                  }
    	                  MAXWRK = Math.max( MAXWRK, BDSPAC );
    	                  MINWRK = Math.max( 4*N, BDSPAC );
    	               }
    	               else if ( WNTUO && WNTVN ) {
    	
    	//                Path 2 (M much larger than N, JOBU='O', JOBVT='N')
    	
    	                  WRKBL = N + N*ilaenv( 1, "DGEQRF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, N+N*ilaenv( 1, "DORGQR", " ", M, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+2*N* ilaenv( 1, "DGEBRD", " ", N, N, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+N*ilaenv( 1, "DORGBR", "Q", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = Math.max( N*N+WRKBL, N*N+M*N+N );
    	                  MINWRK = Math.max( 3*N+M, BDSPAC );
    	               }
    	               else if ( WNTUO && WNTVAS ) {
    	
    	//                Path 3 (M much larger than N, JOBU='O', JOBVT='S' or 'A')
    	
    	                  WRKBL = N + N*ilaenv( 1, "DGEQRF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, N+N*ilaenv( 1, "DORGQR", " ", M, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+2*N*ilaenv( 1, "DGEBRD", " ", N, N, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+N*ilaenv( 1, "DORGBR", "Q", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+( N-1 )*ilaenv( 1, "DORGBR", "P", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = Math.max( N*N+WRKBL, N*N+M*N+N );
    	                  MINWRK = Math.max( 3*N+M, BDSPAC );
    	               }
    	               else if ( WNTUS && WNTVN ) {
    	
    	//                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
    	
    	                  WRKBL = N + N*ilaenv( 1, "DGEQRF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, N+N*ilaenv( 1, "DORGQR", " ", M, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+2*N*ilaenv( 1, "DGEBRD", " ", N, N, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+N*ilaenv( 1, "DORGBR", "Q", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = N*N + WRKBL;
    	                  MINWRK = Math.max( 3*N+M, BDSPAC );
    	               }
    	               else if ( WNTUS && WNTVO ) {
    	
    	//                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
    	
    	                  WRKBL = N + N*ilaenv( 1, "DGEQRF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, N+N*ilaenv( 1, "DORGQR", " ", M, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+2*N*ilaenv( 1, "DGEBRD", " ", N, N, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+N*ilaenv( 1, "DORGBR", "Q", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+( N-1 )*ilaenv( 1, "DORGBR", "P", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = 2*N*N + WRKBL;
    	                  MINWRK = Math.max( 3*N+M, BDSPAC );
    	               }
    	               else if ( WNTUS && WNTVAS ) {
    	
    	//                 Path 6 (M much larger than N, JOBU='S', JOBVT='S' or 'A')
    	
    	                  WRKBL = N + N*ilaenv( 1, "DGEQRF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, N+N*ilaenv( 1, "DORGQR", " ", M, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+2*N*ilaenv( 1, "DGEBRD", " ", N, N, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+N*ilaenv( 1, "DORGBR", "Q", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+( N-1 )*ilaenv( 1, "DORGBR", "P", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = N*N + WRKBL;
    	                  MINWRK = Math.max( 3*N+M, BDSPAC );
    	               }
    	               else if (WNTUA && WNTVN ) {
    	
    	//                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
    	
    	                  WRKBL = N + N*ilaenv( 1, "DGEQRF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, N+M*ilaenv( 1, "DORGQR", " ", M, M, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+2*N*ilaenv( 1, "DGEBRD", " ", N, N, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+N*ilaenv( 1, "DORGBR", "Q", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = N*N + WRKBL;
    	                  MINWRK = Math.max( 3*N+M, BDSPAC );
    	               }
    	               else if( WNTUA && WNTVO ) {
    	
    	//                Path 8 (M much larger than N, JOBU='A', JOBVT='O')
    	
    	                  WRKBL = N + N*ilaenv( 1, "DGEQRF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, N+M*ilaenv( 1, "DORGQR", " ", M, M, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+2*N*ilaenv( 1, "DGEBRD", " ", N, N, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+N*ilaenv( 1, "DORGBR", "Q", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+( N-1 )*ilaenv( 1, "DORGBR", "P", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = 2*N*N + WRKBL;
    	                  MINWRK = Math.max( 3*N+M, BDSPAC );
    	               }
    	               else if( WNTUA && WNTVAS ) {
    	
    	//                 Path 9 (M much larger than N, JOBU='A', JOBVT='S' or 'A')
    	
    	                  WRKBL = N + N*ilaenv( 1, "DGEQRF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, N+M*ilaenv( 1, "DORGQR", " ", M, M, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+2*N*ilaenv( 1, "DGEBRD", " ", N, N, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+N*ilaenv( 1, "DORGBR", "Q", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*N+( N-1 )*ilaenv( 1, "DORGBR", "P", N, N, N, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = N*N + WRKBL;
    	                  MINWRK = Math.max( 3*N+M, BDSPAC );
    	               }
    	            }
    	            else {
    	
    	//              Path 10 (M at least N, but not much larger)
    	
    	               MAXWRK = 3*N + ( M+N )*ilaenv( 1, "DGEBRD", " ", M, N, -1, -1 );
    	               if ( WNTUS || WNTUO ) {
    	                  MAXWRK = Math.max( MAXWRK, 3*N+N*ilaenv( 1, "DORGBR", "Q", M, N, N, -1 ) );
    	               }
    	               if ( WNTUA ) {
    	                   MAXWRK = Math.max( MAXWRK, 3*N+M*ilaenv( 1, "DORGBR", "Q", M, M, N, -1 ) );
    	               }
    	               if( !WNTVN ) {
    	                  MAXWRK = Math.max( MAXWRK, 3*N+( N-1 )*ilaenv( 1, "DORGBR", "P", N, N, N, -1 ) );
    	               }
    	               MAXWRK = Math.max( MAXWRK, BDSPAC );
    	               MINWRK = Math.max( 3*N+M, BDSPAC );
    	            }
    	         }
    	         else if ( MINMN > 0 ) {
    	
    	//           Compute space needed for DBDSQR
    	        	 optsC[0] = JOBU;
    	             optsC[1] = JOBVT;
    	             opts = new String(optsC);
    	            MNTHR = ilaenv( 6, "DGESVD", opts, M, N, 0, 0 );
    	            BDSPAC = 5*M;
    	            if( N >= MNTHR ) {
    	               if ( WNTVN ) {
    	
    	//                 Path 1t(N much larger than M, JOBVT='N')
    	
    	                  MAXWRK = M + M*ilaenv( 1, "DGELQF", " ", M, N, -1, -1 );
    	                  MAXWRK = Math.max( MAXWRK, 3*M+2*M*ilaenv( 1, "DGEBRD", " ", M, M, -1, -1 ) );
    	                  if ( WNTUO || WNTUAS ) {
    	                      MAXWRK = Math.max( MAXWRK, 3*M+M*ilaenv( 1, "DORGBR", "Q", M, M, M, -1 ) );
    	                  }
    	                  MAXWRK = Math.max( MAXWRK, BDSPAC );
    	                  MINWRK = Math.max( 4*M, BDSPAC );
    	               }
    	               else if ( WNTVO && WNTUN ) {
    	
    	//                Path 2t(N much larger than M, JOBU='N', JOBVT='O')
    	
    	                  WRKBL = M + M*ilaenv( 1, "DGELQF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, M+M*ilaenv( 1, "DORGLQ", " ", M, N, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+2*M*ilaenv( 1, "DGEBRD", " ", M, M, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+( M-1 )*ilaenv( 1, "DORGBR", "P", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = Math.max( M*M+WRKBL, M*M+M*N+M );
    	                  MINWRK = Math.max( 3*M+N, BDSPAC );
    	               }
    	               else if ( WNTVO && WNTUAS ) {
    	
    	//                 Path 3t(N much larger than M, JOBU='S' or 'A', JOBVT='O')
    	
    	                  WRKBL = M + M*ilaenv( 1, "DGELQF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, M+M*ilaenv( 1, "DORGLQ", " ", M, N, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+2*M*ilaenv( 1, "DGEBRD", " ", M, M, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+( M-1 )*ilaenv( 1, "DORGBR", "P", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+M*ilaenv( 1, "DORGBR", "Q", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = Math.max( M*M+WRKBL, M*M+M*N+M );
    	                  MINWRK = Math.max( 3*M+N, BDSPAC );
    	               }
    	               else if ( WNTVS && WNTUN ) {
    	
    	//                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
    	
    	                  WRKBL = M + M*ilaenv( 1, "DGELQF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, M+M*ilaenv( 1, "DORGLQ", " ", M, N, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+2*M*ilaenv( 1, "DGEBRD", " ", M, M, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+( M-1 )*ilaenv( 1, "DORGBR", "P", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = M*M + WRKBL;
    	                  MINWRK = Math.max( 3*M+N, BDSPAC );
    	               }
    	               else if ( WNTVS && WNTUO ) {
    	
    	//                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
    	
    	                  WRKBL = M + M*ilaenv( 1, "DGELQF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, M+M*ilaenv( 1, "DORGLQ", " ", M, N, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+2*M*ilaenv( 1, "DGEBRD", " ", M, M, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+( M-1 )*ilaenv( 1, "DORGBR", "P", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+M*ilaenv( 1, "DORGBR", "Q", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = 2*M*M + WRKBL;
    	                  MINWRK = Math.max( 3*M+N, BDSPAC );
    	               }
    	               else if ( WNTVS && WNTUAS ) {
    	
    	//                Path 6t(N much larger than M, JOBU='S' or 'A', JOBVT='S')
    	
    	                  WRKBL = M + M*ilaenv( 1, "DGELQF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, M+M*ilaenv( 1, "DORGLQ", " ", M, N, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+2*M*ilaenv( 1, "DGEBRD", " ", M, M, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+( M-1 )*ilaenv( 1, "DORGBR", "P", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+M*ilaenv( 1, "DORGBR", "Q", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = M*M + WRKBL;
    	                  MINWRK = Math.max( 3*M+N, BDSPAC );
    	               }
    	               else if ( WNTVA && WNTUN ) {
    	
    	//                Path 7t(N much larger than M, JOBU='N', JOBVT='A')
    	
    	                  WRKBL = M + M*ilaenv( 1, "DGELQF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, M+N*ilaenv( 1, "DORGLQ", " ", N, N, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+2*M*ilaenv( 1, "DGEBRD", " ", M, M, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+( M-1 )*ilaenv( 1, "DORGBR", "P", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = M*M + WRKBL;
    	                  MINWRK = Math.max( 3*M+N, BDSPAC );
    	               }
    	               else if ( WNTVA && WNTUO ) {
    	
    	//                Path 8t(N much larger than M, JOBU='O', JOBVT='A')
    	
    	                  WRKBL = M + M*ilaenv( 1, "DGELQF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, M+N*ilaenv( 1, "DORGLQ", " ", N, N, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+2*M*ilaenv( 1, "DGEBRD", " ", M, M, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+( M-1 )*ilaenv( 1, "DORGBR", "P", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+M*ilaenv( 1, "DORGBR", "Q", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = 2*M*M + WRKBL;
    	                  MINWRK = Math.max( 3*M+N, BDSPAC );
    	               }
    	               else if( WNTVA && WNTUAS ) {
    	
    	//                Path 9t(N much larger than M, JOBU='S' or 'A', JOBVT='A')
    	
    	                  WRKBL = M + M*ilaenv( 1, "DGELQF", " ", M, N, -1, -1 );
    	                  WRKBL = Math.max( WRKBL, M+N*ilaenv( 1, "DORGLQ", " ", N, N, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+2*M*ilaenv( 1, "DGEBRD", " ", M, M, -1, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+( M-1 )*ilaenv( 1, "DORGBR", "P", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, 3*M+M*ilaenv( 1, "DORGBR", "Q", M, M, M, -1 ) );
    	                  WRKBL = Math.max( WRKBL, BDSPAC );
    	                  MAXWRK = M*M + WRKBL;
    	                  MINWRK = Math.max( 3*M+N, BDSPAC );
    	               }
    	            }
    	            else {
    	
    	//             Path 10t(N greater than M, but not much larger)
    	
    	               MAXWRK = 3*M + ( M+N )*ilaenv( 1, "DGEBRD", " ", M, N, -1, -1 );
    	               if ( WNTVS || WNTVO ) {
    	                  MAXWRK = Math.max( MAXWRK, 3*M+M*ilaenv( 1, "DORGBR", "P", M, N, M, -1 ) );
    	               }
    	               if ( WNTVA ) {
    	                  MAXWRK = Math.max( MAXWRK, 3*M+N*ilaenv( 1, "DORGBR", "P", N, N, M, -1 ) );
    	               }
    	               if ( !WNTUN ) {
    	                  MAXWRK = Math.max( MAXWRK, 3*M+( M-1 )*ilaenv( 1, "DORGBR", "Q", M, M, M, -1 ) );
    	               }
    	               MAXWRK = Math.max( MAXWRK, BDSPAC );
    	               MINWRK = Math.max( 3*M+N, BDSPAC );
    	            }
    	         }
    	         MAXWRK = Math.max( MAXWRK, MINWRK );
    	         WORK[0] = MAXWRK;
    	
    	         if ( LWORK < MINWRK &&  !LQUERY ) {
    	            INFO[0] = -13;
    	         }
    	      } // if (info[0] == 0)
    	
    	      if ( INFO[0] != 0 ) {
    	    	  Preferences.debug("In DGESVD info[0] = " + info[0] + "\n");
    	    	  return;
    	      }
    	      else if ( LQUERY ) {
    	         return;
    	      }
    	
    	//     Quick return if possible
    	
    	      if ( M == 0 || N == 0 ) {
    	         return;
    	      }
    	
    	//    Get machine constants
    	
    	      eps = dlamch( 'P' );
    	      SMLNUM = Math.sqrt( dlamch( 'S' ) ) / eps;
    	      BIGNUM = 1.0 / SMLNUM;
    	
    	//     Scale A if max element outside range [SMLNUM,BIGNUM]
    	
    	      ANRM = dlange( 'M', M, N, A, LDA, DUM );
    	      ISCL = 0;
    	      if ( ANRM > 0.0 && ANRM < SMLNUM ) {
    	         ISCL = 1;
    	         dlascl( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, IERR );
    	      }
    	      else if( ANRM > BIGNUM ) {
    	         ISCL = 1;
    	         dlascl( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, IERR );
    	      }
    	
    	      
    	      if ( M >= N ) {
    	      	
    	      	/*        A has at least as many rows as columns. If A has sufficiently
    	      	*        more rows than columns, first reduce using the QR
    	      	*        decomposition (if sufficient workspace available)
    	      	*/
    	      	         if ( M >= MNTHR ) {
    	      	
    	      	            if ( WNTUN ) {
    	      	
    	      	//             Path 1 (M much larger than N, JOBU='N')
    	      	//             No left singular vectors to be computed
    	      	
    	      	               ITAU = 1;
    	      	               IWORK = ITAU + N;
    	      	
    	      	//             Compute A=Q*R
    	      	//             (Workspace: need 2*N, prefer N+N*NB)
    	      	               WORK2 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	               dgeqrf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	
    	      	//              Zero out below R
    	      	               IROW = Math.max(1, N-1);
    	      	               ARRAY = new double[IROW][N-1];
    	      	               for (i = 0; i < IROW; i++) {
    	      	            	   for (j = 0; j < N-1; j++) {
    	      	            		   ARRAY[i][j] = A[1+i][j];
    	      	            	   }
    	      	               }
    	      	               dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY, IROW );
    	      	               for (i = 0; i < IROW; i++) {
    	      	            	   for (j = 0; j < N-1; j++) {
    	      	            		   A[1+i][j] = ARRAY[i][j];
    	      	            	   }
    	      	               }
    	      	               IE = 1;
    	      	               ITAUQ = IE + N;
    	      	               ITAUP = ITAUQ + N;
    	      	               IWORK = ITAUP + N;
    	      	
    	      	//             Bidiagonalize R in A
    	      	//             (Workspace: need 4*N, prefer 3*N+2*N*NB)
    	      	               WORK2 = new double[N];
    	      	               WORK3 = new double[N];
    	      	               WORK4 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	               dgebrd( N, N, A, LDA, S, WORK, WORK2, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	               NCVT = 0;
    	      	               if ( WNTVO || WNTVAS ) {
    	      	/*
    	      	*                 If right singular vectors desired, generate P'.
    	      	*                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    	      	*/
    	      	                  dorgbr( 'P', N, N, N, A, LDA, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	                  NCVT = N;
    	      	               }
    	      	               IWORK = IE + N;
    	      	/*
    	      	*              Perform bidiagonal QR iteration, computing right
    	      	*              singular vectors of A in A if desired
    	      	*              (Workspace: need BDSPAC)
    	      	*/
    	      	               WORK4 = new double[4*N];
    	      	               dbdsqr( 'U', N, NCVT, 0, 0, S, WORK, A, LDA,
    	      	                            DUM2, 1, DUM2, 1, WORK4, INFO );
    	      	
    	      	//              If right singular vectors desired in VT, copy them there
    	      	
    	      	               if ( WNTVAS ) {
    	      	                  dlacpy( 'F', N, N, A, LDA, VT, LDVT );
    	      	               }
    	      	            }
    	      	
    	      	            else if ( WNTUO && WNTVN ) {
    	      	/*
    	      	*              Path 2 (M much larger than N, JOBU='O', JOBVT='N')
    	      	*              N left singular vectors to be overwritten on A and
    	      	*              no right singular vectors to be computed
    	      	*/
    	      	               if( LWORK >= N*N+Math.max( 4*N, BDSPAC ) ) {
    	      	
    	      	//                Sufficient workspace for a fast algorithm
    	      	
    	      	                  IR = 1;
    	      	                  if( LWORK >= Math.max( WRKBL, LDA*N+N )+LDA*N ) {
    	      	
    	      	//                   WORK(IU) is LDA by N, WORK(IR) is LDA by N
    	      
    	      	                     LDWRKU = LDA;
    	      	                     LDWRKR = LDA;
    	      	                  }
    	      	                  else if ( LWORK >= Math.max( WRKBL, LDA*N+N )+N*N ) {
    	      	
    	      	//                   WORK(IU) is LDA by N, WORK(IR) is N by N
    	      	
    	      	                     LDWRKU = LDA;
    	      	                     LDWRKR = N;
    	      	                  }
    	      	                  else {
    	      	
    	      	//                   WORK(IU) is LDWRKU by N, WORK(IR) is N by N
    	      	
    	      	                     LDWRKU = ( LWORK-N*N-N ) / N;
    	      	                     LDWRKR = N;
    	      	                  }
    	      	                  ITAU = IR + LDWRKR*N;
    	      	                  IWORK = ITAU + N;
    	      	/*
    	      	*                 Compute A=Q*R
    	      	*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    	      	*/
    	      	                  WORK2 = new double[Math.min(M,N)];
    	      	                  WORK3 = new double[Math.max(1, LWORK - IWORK + 1)];
    	      	                  dgeqrf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	
    	      	//                Copy R to WORK(IR) and zero out below it
    	      	                  ARRAY = new double[LDWRKR][N];
    	      	                  dlacpy( 'U', N, N, A, LDA, ARRAY, LDWRKR );
    	      	                  j = 0;
    	      	                  for (ICOL = 0; ICOL < N; ICOL++) {
    	      	                	  for (IROW = 0; IROW < LDWRKR; IROW++) {
    	      	                          WORK[j] = ARRAY[IROW][ICOL];
    	      	                          j++;
    	      	                	  }
    	      	                  }
    	      	                  ARRAY2 = new double[LDWRKR][N-1];
    	      	                  j = 0;
    	      	                  for (ICOL = 0; ICOL < N-1; ICOL++) {
    	      	                	  for (IROW = 0; IROW < LDWRKR; IROW++) {
    	      	                          ARRAY2[IROW][ICOL] = WORK[j+1];
    	      	                          j++;
    	      	                	  }
    	      	                  }
    	      	                  dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY2, LDWRKR );
    	      	                  j = 0;
    	      	                  for (ICOL = 0; ICOL < N-1; ICOL++) {
    	      	                	  for (IROW = 0; IROW < LDWRKR; IROW++) {
    	      	                          WORK[j + 1] = ARRAY2[IROW][ICOL];
    	      	                          j++;
    	      	                	  }
    	      	                  }
    	      	/*
    	      	*                 Generate Q in A
    	      	*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    	      	*/
    	      	                  dorgqr( M, N, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                  IE = ITAU;
    	      	                  ITAUQ = IE + N;
    	      	                  ITAUP = ITAUQ + N;
    	      	                  IWORK = ITAUP + N;
    	      	/*
    	      	*                 Bidiagonalize R in WORK(IR)
    	      	*                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
    	      	*/                j = 0;
    	  					      for (ICOL = 0; ICOL < N; ICOL++) {
    	  					      	for (IROW = 0; IROW < LDWRKR; IROW++) {
    	  					              ARRAY[IROW][ICOL] = WORK[j];
    	  					              j++;
    	  					       }
    	  					      }
    	  					      WORK2 = new double[N-1];
    	  					      WORK3 = new double[N];
    	  					      WORK4 = new double[N];
    	  					      WORK5 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                  dgebrd( N, N, ARRAY, LDWRKR, S, WORK2, WORK3, WORK4,
    	      	                               WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Generate left vectors bidiagonalizing R
    	      	*                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
    	      	*/
    	      	                  dorgbr( 'Q', N, N, N, ARRAY, LDWRKR, WORK3, WORK5,
    	      	                          LWORK-IWORK+1, IERR );
    	      	                  IWORK = IE + N;
    	      	/*
    	      	*                 Perform bidiagonal QR iteration, computing left
    	      	*                 singular vectors of R in WORK(IR)
    	      	*                 (Workspace: need N*N+BDSPAC)
    	      	*/
    	      	                  WORK3 = new double[4*N];
    	      	                  dbdsqr( 'U', N, 0, N, 0, S, WORK2, DUM2, 1, ARRAY, LDWRKR, DUM2, 1,
    	      	                               WORK3, INFO );
    	      	                  IU = IE + N;
    	      	/*
    	      	*                 Multiply Q in A by left singular vectors of R in
    	      	*                 WORK(IR), storing result in WORK(IU) and copying to A
    	      	*                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
    	      	*/
    	      	                  for ( I = 1; I <= M; I += LDWRKU) {
    	      	                     CHUNK = Math.min( M-I+1, LDWRKU );
    	      	                     IROW = Math.max(1, CHUNK);
    	      	                     ARRAY2 = new double[IROW][N];
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < N; j++) {
    	      	                    		 ARRAY2[i][j] = A[I-1+i][j];
    	      	                    	 }
    	      	                     }
    	      	                     ARRAY3 = new double[IROW][N];
    	      	                     dgemm( 'N', 'N', CHUNK, N, N, 1.0, ARRAY2,
    	      	                            IROW, ARRAY, LDWRKR, 0.0, ARRAY3, IROW );
    	      	                     dlacpy( 'F', CHUNK, N, ARRAY3, IROW, ARRAY2, IROW);
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < N; j++) {
    	      	                    		 A[I-1+i][j] = ARRAY2[i][j];
    	      	                    	 }
    	      	                     }
    	      	                  } // for ( I = 1; I <= M; I += LDWRKU)
    	      	                  j = 0;
    	      	                  for (ICOL = 0; ICOL < N; ICOL++) {
    	      	                	  for (IROW = 0; IROW < LDWRKR; IROW++) {
    	      	                          WORK[j] = ARRAY[IROW][ICOL];
    	      	                          j++;
    	      	                	  }
    	      	                  }
    	      	               }
    	      	
    	      	               else {
    	      	
    	      	//                 Insufficient workspace for a fast algorithm
    	      	
    	      	                  IE = 1;
    	      	                  ITAUQ = IE + N;
    	      	                  ITAUP = ITAUQ + N;
    	      	                  IWORK = ITAUP + N;
    	      	/*
    	      	*                 Bidiagonalize A
    	      	*                 (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
    	      	*/                WORK2 = new double[Math.min(M,N)];
    	      	                  WORK3 = new double[Math.min(M,N)];
    	      	                  WORK4 = new double[Math.max(1, LWORK - IWORK + 1)];
    	      	                  dgebrd( M, N, A, LDA, S, WORK, WORK2, WORK3,
    	      	                               WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Generate left vectors bidiagonalizing A
    	      	*                 (Workspace: need 4*N, prefer 3*N+N*NB)
    	      	*/
    	      	                  dorgbr( 'Q', M, N, N, A, LDA, WORK2,
    	      	                               WORK4, LWORK-IWORK+1, IERR );
    	      	                  IWORK = IE + N;
    	      	/*
    	      	*                 Perform bidiagonal QR iteration, computing left
    	      	*                 singular vectors of A in A
    	      	*                 (Workspace: need BDSPAC)
    	      	*/                WORK2 = new double[4*N];
    	      	                  dbdsqr( 'U', N, 0, M, 0, S, WORK, DUM2, 1,
    	      	                               A, LDA, DUM2, 1, WORK2, INFO );
    	      	
    	      	               }
    	      	            }
    	      	            else if( WNTUO && WNTVAS ) {
    	      	/*
    	      	*              Path 3 (M much larger than N, JOBU='O', JOBVT='S' or 'A')
    	      	*              N left singular vectors to be overwritten on A and
    	      	*              N right singular vectors to be computed in VT
    	      	*/
    	      	               if( LWORK >= N*N+Math.max( 4*N, BDSPAC ) ) {
    	      	/*
    	      	*                 Sufficient workspace for a fast algorithm
    	      	*/
    	      	                  IR = 1;
    	      	                  if( LWORK >= Math.max( WRKBL, LDA*N+N )+LDA*N ) {
    	      	/*
    	      	*                    WORK(IU) is LDA by N and WORK(IR) is LDA by N
    	      	*/
    	      	                     LDWRKU = LDA;
    	      	                     LDWRKR = LDA;
    	      	                  }
    	      	                  else if ( LWORK >= Math.max( WRKBL, LDA*N+N )+N*N ) {
    	      	/*
    	      	*                    WORK(IU) is LDA by N and WORK(IR) is N by N
    	      	*/
    	      	                     LDWRKU = LDA;
    	      	                     LDWRKR = N;
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    WORK(IU) is LDWRKU by N and WORK(IR) is N by N
    	      	*/
    	      	                     LDWRKU = ( LWORK-N*N-N ) / N;
    	      	                     LDWRKR = N;
    	      	                  }
    	      	                  ITAU = IR + LDWRKR*N;
    	      	                  IWORK = ITAU + N;
    	      	/*
    	      	*                 Compute A=Q*R
    	      	*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    	      	*/
    	      	                  WORK2 = new double[Math.min(M,N)];
    	      	                  WORK3 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                  dgeqrf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Copy R to VT, zeroing out below it
    	      	*/
    	      	                  dlacpy( 'U', N, N, A, LDA, VT, LDVT );
    	      	                  if ( N > 1 ) {
    	      	                	 IROW = Math.max(1, N-1);
    	      	                	 ARRAY = new double[IROW][N-1];
    	      	                	 for (i = 0; i < IROW; i++) {
    	      	                		 for (j = 0; j < N-1; j++) {
    	      	                			 ARRAY[i][j] = VT[1+i][j];
    	      	                		 }
    	      	                	 }
    	      	                     dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY, IROW );
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                		 for (j = 0; j < N-1; j++) {
    	      	                			 VT[1+i][j] = ARRAY[i][j];
    	      	                		 }
    	      	                	 }
    	      	                  }
    	      	/*
    	      	*                 Generate Q in A
    	      	*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    	      	*/
    	      	                  dorgqr( M, N, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                  IE = ITAU;
    	      	                  ITAUQ = IE + N;
    	      	                  ITAUP = ITAUQ + N;
    	      	                  IWORK = ITAUP + N;
    	      	/*
    	      	*                 Bidiagonalize R in VT, copying result to WORK(IR)
    	      	*                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
    	      	*/
    	      	                  WORK2 = new double[N-1];
    	      	                  WORK3 = new double[N];
    	      	                  WORK4 = new double[N];
    	      	                  WORK5 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                  dgebrd( N, N, VT, LDVT, S, WORK2, WORK3, WORK4,
    	      	                               WORK5, LWORK-IWORK+1, IERR );
    	      	                  ARRAY = new double[LDWRKR][N];
    	      	                  dlacpy( 'L', N, N, VT, LDVT, ARRAY, LDWRKR );
    	      	/*
    	      	*                 Generate left vectors bidiagonalizing R in WORK(IR)
    	      	*                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
    	      	*/
    	      	                  dorgbr( 'Q', N, N, N, ARRAY, LDWRKR, WORK3, WORK5,
    	      	                               LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Generate right vectors bidiagonalizing R in VT
    	      	*                 (Workspace: need N*N+4*N-1, prefer N*N+3*N+(N-1)*NB)
    	      	*/
    	      	                  dorgbr( 'P', N, N, N, VT, LDVT, WORK4,
    	      	                               WORK5, LWORK-IWORK+1, IERR );
    	      	                  IWORK = IE + N;
    	      	/*
    	      	*                 Perform bidiagonal QR iteration, computing left
    	      	*                 singular vectors of R in WORK(IR) and computing right
    	      	*                 singular vectors of R in VT
    	      	*                 (Workspace: need N*N+BDSPAC)
    	      	*/
    	      	                  WORK3 = new double[4*N];
    	      	                  dbdsqr( 'U', N, N, N, 0, S, WORK2, VT, LDVT,
    	      	                               ARRAY, LDWRKR, DUM2, 1, WORK3, INFO );
    	      	                  IU = IE + N;
    	      	/*
    	      	*                 Multiply Q in A by left singular vectors of R in
    	      	*                 WORK(IR), storing result in WORK(IU) and copying to A
    	      	*                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
    	      	*/
    	      	                  for( I = 1; I <= M; I += LDWRKU) {
    	      	                     CHUNK = Math.min( M-I+1, LDWRKU );
    	      	                     IROW = Math.max(1, CHUNK); 
    	      	                     ARRAY2 = new double[IROW][N];
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < N; j++) {
    	      	                    		 ARRAY2[i][j] = A[I-1+i][j];
    	      	                    	 }
    	      	                     }
    	      	                     ARRAY3 = new double[IROW][N];
    	      	                     dgemm( 'N', 'N', CHUNK, N, N, 1.0, ARRAY2,
    	      	                                 IROW, ARRAY, LDWRKR, 0.0, ARRAY3, IROW );
    	      	                     dlacpy( 'F', CHUNK, N, ARRAY3, IROW, ARRAY2, IROW );
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < N; j++) {
    	      	                    		 A[I-1+i][j] = ARRAY2[i][j];
    	      	                    	 }
    	      	                     }
    	      	                  } // for( I = 1; I <= M; I += LDWRKU)
    	      	                  j = 0;
    	      	                  for (ICOL = 0; ICOL < N; ICOL++) {
    	      	                	  for (IROW = 0; IROW < LDWRKR; IROW++) {
    	      	                          WORK[j] = ARRAY[IROW][ICOL];
    	      	                          j++;
    	      	                	  }
    	      	                  }
    	      	               }
    	      	               else {
    	      	/*
    	      	*                 Insufficient workspace for a fast algorithm
    	      	*/
    	      	                  ITAU = 1;
    	      	                  IWORK = ITAU + N;
    	      	/*
    	      	*                 Compute A=Q*R
    	      	*                 (Workspace: need 2*N, prefer N+N*NB)
    	      	*/
    	      	                  WORK2 = new double[Math.min(M,N)];
    	      	                  WORK3 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                  dgeqrf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Copy R to VT, zeroing out below it
    	      	*/
    	      	                  dlacpy( 'U', N, N, A, LDA, VT, LDVT );
    	      	                  if ( N > 1 ) {
    	      	                	  IROW = Math.max(1,N-1);
    	      	                	  ARRAY = new double[IROW][N-1];
    	      	                	  for (i = 0; i < IROW; i++) {
    	      	                		  for (j = 0; j < N-1; j++) {
    	      	                			  ARRAY[i][j] = VT[1+i][j];
    	      	                		  }
    	      	                	  }
    	      	                      dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY, IROW );
    	      	                      for (i = 0; i < IROW; i++) {
    	      	                		  for (j = 0; j < N-1; j++) {
    	      	                			  VT[1+i][j] = ARRAY[i][j];
    	      	                		  }
    	      	                	  }
    	      	                  } // if (N > 1)
    	      	/*
    	      	*                 Generate Q in A
    	      	*                 (Workspace: need 2*N, prefer N+N*NB)
    	      	*/
    	      	                  dorgqr( M, N, N, A, LDA, WORK2,
    	      	                               WORK3, LWORK-IWORK+1, IERR );
    	      	                  IE = ITAU;
    	      	                  ITAUQ = IE + N;
    	      	                  ITAUP = ITAUQ + N;
    	      	                  IWORK = ITAUP + N;
    	      	/*
    	      	*                 Bidiagonalize R in VT
    	      	*                 (Workspace: need 4*N, prefer 3*N+2*N*NB)
    	      	*/
    	      	                  WORK2 = new double[N-1];
    	      	                  WORK3 = new double[N];
    	      	                  WORK4 = new double[N];
    	      	                  WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                  dgebrd( N, N, VT, LDVT, S, WORK2,
    	      	                               WORK3, WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Multiply Q in A by left vectors bidiagonalizing R
    	      	*                 (Workspace: need 3*N+M, prefer 3*N+M*NB)
    	      	*/
    	      	                  dormbr( 'Q', 'R', 'N', M, N, N, VT, LDVT,
    	      	                               WORK3, A, LDA, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Generate right vectors bidiagonalizing R in VT
    	      	*                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    	      	*/
    	      	                  dorgbr( 'P', N, N, N, VT, LDVT, WORK4,
    	      	                               WORK5, LWORK-IWORK+1, IERR );
    	      	                  IWORK = IE + N;
    	      	/*
    	      	*                 Perform bidiagonal QR iteration, computing left
    	      	*                 singular vectors of A in A and computing right
    	      	*                 singular vectors of A in VT
    	      	*                 (Workspace: need BDSPAC)
    	      	*/
    	      	                  WORK3 = new double[4*N];
    	      	                  dbdsqr( 'U', N, N, M, 0, S, WORK2, VT, LDVT,
    	      	                               A, LDA, DUM2, 1, WORK3, INFO );
    	      	               }
    	      	            }
    	      	            else if ( WNTUS ) {
    	      	
    	      	               if ( WNTVN ) {
    	      	/*
    	      	*                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
    	      	*                 N left singular vectors to be computed in U and
    	      	*                 no right singular vectors to be computed
    	      	*/
    	      	                  if ( LWORK >= N*N+Math.max( 4*N, BDSPAC ) ) {
    	      	/*
    	      	*                    Sufficient workspace for a fast algorithm
    	      	*/
    	      	                     IR = 1;
    	      	                     if ( LWORK >= WRKBL+LDA*N ) {
    	      	/*
    	      	*                       WORK(IR) is LDA by N
    	      	*/
    	      	                        LDWRKR = LDA;
    	      	                     }
    	      	                     else {
    	      	/*
    	      	*                       WORK(IR) is N by N
    	      	*/
    	      	                        LDWRKR = N;
    	      	                     }
    	      	                     ITAU = IR + LDWRKR*N;
    	      	                     IWORK = ITAU + N;
    	      	/*
    	      	*                    Compute A=Q*R
    	      	*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M,N)];
    	      	                     WORK3 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgeqrf( M, N, A, LDA, WORK2,
    	      	                                  WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy R to WORK(IR), zeroing out below it
    	      	*/
    	      	                     ARRAY = new double[LDWRKR][N];
    	      	                     dlacpy( 'U', N, N, A, LDA, ARRAY, LDWRKR );
    	      	                     j = 0;
    	  	       	                  for (ICOL = 0; ICOL < N; ICOL++) {
    	  	       	                	  for (IROW = 0; IROW < LDWRKR; IROW++) {
    	  	       	                          WORK[j] = ARRAY[IROW][ICOL];
    	  	       	                          j++;
    	  	       	                	  }
    	  	       	                  }
    	  	       	                  ARRAY2 = new double[LDWRKR][N-1];
    	  	       	                  j = 0;
    	  	       	                  for (ICOL = 0; ICOL < N-1; ICOL++) {
    	  	       	                	  for (IROW = 0; IROW < LDWRKR; IROW++) {
    	  	       	                          ARRAY2[IROW][ICOL] = WORK[j+1];
    	  	       	                          j++;
    	  	       	                	  }
    	  	       	                  }
    	      	                     dlaset( 'L', N-1, N-1, 0.0, 0.0,
    	      	                                  ARRAY2, LDWRKR );
    	      	                     j = 0;
    	  	       	                  for (ICOL = 0; ICOL < N-1; ICOL++) {
    	  	       	                	  for (IROW = 0; IROW < LDWRKR; IROW++) {
    	  	       	                          WORK[j+1] = ARRAY2[IROW][ICOL];
    	  	       	                          j++;
    	  	       	                	  }
    	  	       	                  }
    	      	/*
    	      	*                    Generate Q in A
    	      	*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    	      	*/
    	      	                     dorgqr( M, N, N, A, LDA, WORK2,
    	      	                                  WORK3, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + N;
    	      	                     ITAUP = ITAUQ + N;
    	      	                     IWORK = ITAUP + N;
    	      	/*
    	      	*                    Bidiagonalize R in WORK(IR)
    	      	*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
    	      	*/
    	      	                     ARRAY = new double[LDWRKR][N];
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                         for (i = 0; i < LDWRKR; i++) {
    	      	                			 ARRAY[i][j] = WORK[k++];
    	      	                		 }
    	      	                	 }
    	      	                     WORK2 = new double[Math.min(M,N)-1];
    	      	                     WORK3 = new double[Math.min(M,N)];
    	      	                     WORK4 = new double[Math.min(M,N)];
    	      	                     WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( N, N, ARRAY, LDWRKR, S, WORK2, WORK3,
    	      	                                  WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate left vectors bidiagonalizing R in WORK(IR)
    	      	*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
    	      	*/
    	      	                     dorgbr( 'Q', N, N, N, ARRAY, LDWRKR, WORK3, WORK5,
    	      	                                  LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + N;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of R in WORK(IR)
    	      	*                    (Workspace: need N*N+BDSPAC)
    	      	*/
    	      	                     WORK4 = new double[4*N];
    	      	                     dbdsqr( 'U', N, 0, N, 0, S, WORK3, DUM2,
    	      	                                  1, ARRAY, LDWRKR, DUM2, 1, WORK4, INFO );
    	      	/*
    	      	*                    Multiply Q in A by left singular vectors of R in
    	      	*                    WORK(IR), storing result in U
    	      	*                    (Workspace: need N*N)
    	      	*/
    	      	                     dgemm( 'N', 'N', M, N, N, 1.0, A, LDA,
    	      	                                 ARRAY, LDWRKR, 0.0, U, LDU );
    	      	                     j = 0;
    	  	       	                  for (ICOL = 0; ICOL < N; ICOL++) {
    	  	       	                	  for (IROW = 0; IROW < LDWRKR; IROW++) {
    	  	       	                          ARRAY[IROW][ICOL] = WORK[j];
    	  	       	                          j++;
    	  	       	                	  }
    	  	       	                  }
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    Insufficient workspace for a fast algorithm
    	      	*/
    	      	                     ITAU = 1;
    	      	                     IWORK = ITAU + N;
    	      	/*
    	      	*                    Compute A=Q*R, copying result to U
    	      	*                    (Workspace: need 2*N, prefer N+N*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M,N)];
    	      	                     WORK3 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgeqrf( M, N, A, LDA, WORK2,
    	      	                                  WORK3, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'L', M, N, A, LDA, U, LDU );
    	      	/*
    	      	*                    Generate Q in U
    	      	*                    (Workspace: need 2*N, prefer N+N*NB)
    	      	*/
    	      	                     dorgqr( M, N, N, U, LDU, WORK2,
    	      	                                  WORK3, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + N;
    	      	                     ITAUP = ITAUQ + N;
    	      	                     IWORK = ITAUP + N;
    	      	/*
    	      	*                    Zero out below R in A
    	      	*/
    	      	                     IROW = Math.max(1,N-1);
    	      	                     ARRAY = new double[IROW][N-1];
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < N-1; j++) {
    	      	                    		 ARRAY[i][j] = A[1+i][j];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY, IROW );
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < N-1; j++) {
    	      	                    		 A[1+i][j] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Bidiagonalize R in A
    	      	*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
    	      	*/
    	      	                     WORK2 = new double[N-1];
    	      	                     WORK3 = new double[N];
    	      	                     WORK4 = new double[N];
    	      	                     WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( N, N, A, LDA, S, WORK2, WORK3, WORK4,
    	      	                                  WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Multiply Q in U by left vectors bidiagonalizing R
    	      	*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
    	      	*/
    	      	                     dormbr( 'Q', 'R', 'N', M, N, N, A, LDA, WORK3, U, LDU, WORK5,
    	      	                                  LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + N;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of A in U
    	      	*                    (Workspace: need BDSPAC)
    	      	*/
    	      	                     WORK3 = new double[4*N];
    	      	                     dbdsqr( 'U', N, 0, M, 0, S, WORK2, DUM2,
    	      	                                  1, U, LDU, DUM2, 1, WORK3, INFO );
    	      	
    	      	                  }
    	      	               }
    	      	               else if ( WNTVO ) {
    	      	/*
    	      	*                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
    	      	*                 N left singular vectors to be computed in U and
    	      	*                 N right singular vectors to be overwritten on A
    	      	*/
    	      	                  if ( LWORK >= 2*N*N+Math.max( 4*N, BDSPAC ) ) {
    	      	/*
    	      	*                    Sufficient workspace for a fast algorithm
    	      	*/
    	      	                     IU = 1;
    	      	                     if( LWORK >= WRKBL+2*LDA*N ) {
    	      	/*
    	      	*                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
    	      	*/
    	      	                        LDWRKU = LDA;
    	      	                        IR = IU + LDWRKU*N;
    	      	                        LDWRKR = LDA;
    	      	                     }
    	      	                     else if ( LWORK >= WRKBL+( LDA+N )*N ) {
    	      	/*
    	      	*                       WORK(IU) is LDA by N and WORK(IR) is N by N
    	      	*/
    	      	                        LDWRKU = LDA;
    	      	                        IR = IU + LDWRKU*N;
    	      	                        LDWRKR = N;
    	      	                     }
    	      	                     else {
    	      	/*
    	      	*                       WORK(IU) is N by N and WORK(IR) is N by N
    	      	*/
    	      	                        LDWRKU = N;
    	      	                        IR = IU + LDWRKU*N;
    	      	                        LDWRKR = N;
    	      	                     }
    	      	                     ITAU = IR + LDWRKR*N;
    	      	                     IWORK = ITAU + N;
    	      	/*
    	      	*                    Compute A=Q*R
    	      	*                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M,N)];
    	      	                     WORK3 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgeqrf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy R to WORK(IU), zeroing out below it
    	      	*/
    	      	                     ARRAY = new double[LDWRKU][N];
    	      	                     dlacpy( 'U', N, N, A, LDA, ARRAY, LDWRKU );
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                             WORK[k++] = ARRAY[i][j]; 
    	      	                    	 }
    	      	                     }
    	      	                     ARRAY2 = new double[LDWRKR][N-1];
    	  	       	                  j = 0;
    	  	       	                  for (ICOL = 0; ICOL < N-1; ICOL++) {
    	  	       	                	  for (IROW = 0; IROW < LDWRKR; IROW++) {
    	  	       	                          ARRAY2[IROW][ICOL] = WORK[j+1];
    	  	       	                          j++;
    	  	       	                	  }
    	  	       	                  }
    	      	                     dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY2, LDWRKU );
    	      	                     j = 0;
    	  	       	                  for (ICOL = 0; ICOL < N-1; ICOL++) {
    	  	       	                	  for (IROW = 0; IROW < LDWRKR; IROW++) {
    	  	       	                          WORK[j+1] = ARRAY2[IROW][ICOL];
    	  	       	                          j++;
    	  	       	                	  }
    	  	       	                  }
    	      	/*
    	      	*                    Generate Q in A
    	      	*                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
    	      	*/
    	      	                     dorgqr( M, N, N, A, LDA, WORK2,
    	      	                                  WORK3, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + N;
    	      	                     ITAUP = ITAUQ + N;
    	      	                     IWORK = ITAUP + N;
    	      	/*
    	      	*                    Bidiagonalize R in WORK(IU), copying result to
    	      	*                    WORK(IR)
    	      	*                    (Workspace: need 2*N*N+4*N,
    	      	*                                prefer 2*N*N+3*N+2*N*NB)
    	      	*/
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                             ARRAY[i][j] = WORK[k++]; 
    	      	                    	 }
    	      	                     }
    	      	                     WORK2 = new double[N-1];
    	      	                     WORK3 = new double[N];
    	      	                     WORK4 = new double[N];
    	      	                     WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( N, N, ARRAY, LDWRKU, S, WORK2, WORK3,
    	      	                                  WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                     ARRAY2 = new double[LDWRKR][N];
    	      	                     dlacpy( 'U', N, N, ARRAY, LDWRKU, ARRAY2, LDWRKR );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors in WORK(IU)
    	      	*                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
    	      	*/
    	      	                     dorgbr( 'Q', N, N, N, ARRAY, LDWRKU,
    	      	                                  WORK3, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate right bidiagonalizing vectors in WORK(IR)
    	      	*                    (Workspace: need 2*N*N+4*N-1,
    	      	*                                prefer 2*N*N+3*N+(N-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', N, N, N, ARRAY2, LDWRKR,
    	      	                                  WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + N;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of R in WORK(IU) and computing
    	      	*                    right singular vectors of R in WORK(IR)
    	      	*                    (Workspace: need 2*N*N+BDSPAC)
    	      	*/
    	      	                     WORK5 = new double[4*N];
    	      	                     dbdsqr( 'U', N, N, N, 0, S, WORK2,
    	      	                                  ARRAY2, LDWRKR, ARRAY, LDWRKU, DUM2, 1, WORK5, INFO );
    	      	/*
    	      	*                    Multiply Q in A by left singular vectors of R in
    	      	*                    WORK(IU), storing result in U
    	      	*                    (Workspace: need N*N)
    	      	*/
    	      	                     dgemm( 'N', 'N', M, N, N, 1.0, A, LDA,
    	      	                                 ARRAY, LDWRKU, 0.0, U, LDU );
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                             WORK[k++] = ARRAY[i][j]; 
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Copy right singular vectors of R to A
    	      	*                    (Workspace: need N*N)
    	      	*/
    	      	                     dlacpy( 'F', N, N, ARRAY2, LDWRKR, A, LDA );
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    Insufficient workspace for a fast algorithm
    	      	*/
    	      	                     ITAU = 1;
    	      	                     IWORK = ITAU + N;
    	      	/*
    	      	*                    Compute A=Q*R, copying result to U
    	      	*                    (Workspace: need 2*N, prefer N+N*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M,N)];
    	      	                     WORK3 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgeqrf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'L', M, N, A, LDA, U, LDU );
    	      	/*
    	      	*                    Generate Q in U
    	      	*                    (Workspace: need 2*N, prefer N+N*NB)
    	      	*/
    	      	                     dorgqr( M, N, N, U, LDU, WORK2,
    	      	                                  WORK3, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + N;
    	      	                     ITAUP = ITAUQ + N;
    	      	                     IWORK = ITAUP + N;
    	      	/*
    	      	*                    Zero out below R in A
    	      	*/
    	      	                     IROW = Math.max(1,N-1);
    	      	                     ARRAY = new double[IROW][N-1];
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < N-1; j++) {
    	      	                    		 ARRAY[i][j] = A[i+1][j];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY, IROW );
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < N-1; j++) {
    	      	                    		 A[i+1][j] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Bidiagonalize R in A
    	      	*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
    	      	*/
    	      	                     WORK = new double[N-1];
    	      	                     WORK2 = new double[N];
    	      	                     WORK3 = new double[N];
    	      	                     WORK4 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgebrd( N, N, A, LDA, S, WORK,
    	      	                                  WORK2, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Multiply Q in U by left vectors bidiagonalizing R
    	      	*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
    	      	*/
    	      	                     dormbr( 'Q', 'R', 'N', M, N, N, A, LDA,
    	      	                                  WORK2, U, LDU, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate right vectors bidiagonalizing R in A
    	      	*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', N, N, N, A, LDA, WORK3,
    	      	                                  WORK4, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + N;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of A in U and computing right
    	      	*                    singular vectors of A in A
    	      	*                    (Workspace: need BDSPAC)
    	      	*/
    	      	                     WORK4 = new double[4*N];
    	      	                     dbdsqr( 'U', N, N, M, 0, S, WORK, A,
    	      	                                 LDA, U, LDU, DUM2, 1, WORK4, INFO );
    	      	
    	      	                  }
    	      	               }
    	      	               else if ( WNTVAS ) {
    	      	/*
    	      	*                 Path 6 (M much larger than N, JOBU='S', JOBVT='S'
    	      	*                         or 'A')
    	      	*                 N left singular vectors to be computed in U and
    	      	*                 N right singular vectors to be computed in VT
    	      	*/
    	      	                  if( LWORK >= N*N+Math.max( 4*N, BDSPAC ) ) {
    	      	/*
    	      	*                    Sufficient workspace for a fast algorithm
    	      	*/
    	      	                     IU = 1;
    	      	                     if ( LWORK >= WRKBL+LDA*N ) {
    	      	/*
    	      	*                       WORK(IU) is LDA by N
    	      	*/
    	      	                        LDWRKU = LDA;
    	      	                     }
    	      	                     else {
    	      	/*
    	      	*                       WORK(IU) is N by N
    	      	*/
    	      	                        LDWRKU = N;
    	      	                     }
    	      	                     ITAU = IU + LDWRKU*N;
    	      	                     IWORK = ITAU + N;
    	      	/*
    	      	*                    Compute A=Q*R
    	      	*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M,N)];
    	      	                     WORK3 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgeqrf( M, N, A, LDA, WORK2,
    	      	                                  WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy R to WORK(IU), zeroing out below it
    	      	*/
    	      	                     ARRAY = new double[LDWRKU][N];
    	      	                     dlacpy( 'U', N, N, A, LDA, ARRAY, LDWRKU );
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 WORK[k++] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                     ARRAY2 = new double[LDWRKU][N-1];
    	      	                     k = 0;
    	      	                     for (j = 0; j < N-1; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 ARRAY2[i][j] = WORK[k+1];
    	      	                    		 k++;
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY2, LDWRKU );
    	      	/*
    	      	*                    Generate Q in A
    	      	*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    	      	*/
    	      	                     dorgqr( M, N, N, A, LDA, WORK2,
    	      	                                  WORK3, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + N;
    	      	                     ITAUP = ITAUQ + N;
    	      	                     IWORK = ITAUP + N;
    	      	/*
    	      	*                    Bidiagonalize R in WORK(IU), copying result to VT
    	      	*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
    	      	*/
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 ARRAY[i][j] = WORK[k++];
    	      	                    	 }
    	      	                     }
    	      	                     WORK2 = new double[N-1];
    	      	                     WORK3 = new double[N];
    	      	                     WORK4 = new double[N];
    	      	                     WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( N, N, ARRAY, LDWRKU, S,
    	      	                                  WORK2, WORK3, WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'U', N, N, ARRAY, LDWRKU, VT, LDVT );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors in WORK(IU)
    	      	*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
    	      	*/
    	      	                     dorgbr( 'Q', N, N, N, ARRAY, LDWRKU,
    	      	                                  WORK3, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate right bidiagonalizing vectors in VT
    	      	*                    (Workspace: need N*N+4*N-1,
    	      	*                                prefer N*N+3*N+(N-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', N, N, N, VT, LDVT, WORK4,
    	      	                                  WORK5, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + N;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of R in WORK(IU) and computing
    	      	*                    right singular vectors of R in VT
    	      	*                    (Workspace: need N*N+BDSPAC)
    	      	*/
    	      	                     WORK5 = new double[4*N];
    	      	                     dbdsqr( 'U', N, N, N, 0, S, WORK2, VT,
    	      	                                  LDVT, ARRAY, LDWRKU, DUM2, 1, WORK5, INFO );
    	      	/*
    	      	*                    Multiply Q in A by left singular vectors of R in
    	      	*                    WORK(IU), storing result in U
    	      	*                    (Workspace: need N*N)
    	      	*/
    	      	                     dgemm( 'N', 'N', M, N, N, 1.0, A, LDA,
    	      	                                ARRAY, LDWRKU, 0.0, U, LDU );
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 WORK[k++] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    Insufficient workspace for a fast algorithm
    	      	*/
    	      	                     ITAU = 1;
    	      	                     IWORK = ITAU + N;
    	      	/*
    	      	*                    Compute A=Q*R, copying result to U
    	      	*                    (Workspace: need 2*N, prefer N+N*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgeqrf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'L', M, N, A, LDA, U, LDU );
    	      	/*
    	      	*                    Generate Q in U
    	      	*                    (Workspace: need 2*N, prefer N+N*NB)
    	      	*/
    	      	                     dorgqr( M, N, N, U, LDU, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy R to VT, zeroing out below it
    	      	*/
    	      	                     dlacpy( 'U', N, N, A, LDA, VT, LDVT );
    	      	                     if ( N > 1 ) {
    	      	                    	IROW = Math.max(1,N-1);
    	      	                    	ARRAY = new double[IROW][N-1];
    	      	                    	for (i = 0; i < IROW; i++) {
    	      	                    		for (j = 0; j < N-1; j++) {
    	      	                    			ARRAY[i][j] = VT[i+1][j];
    	      	                    		}
    	      	                    	}
    	      	                        dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY, LDVT );
    	      	                        for (i = 0; i < IROW; i++) {
    	      	                    		for (j = 0; j < N-1; j++) {
    	      	                    			VT[i+1][j] = ARRAY[i][j];
    	      	                    		}
    	      	                    	}
    	      	                     }
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + N;
    	      	                     ITAUP = ITAUQ + N;
    	      	                     IWORK = ITAUP + N;
    	      	/*
    	      	*                    Bidiagonalize R in VT
    	      	*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
    	      	*/
    	      	                     WORK2 = new double[N];
    	      	                     WORK3 = new double[N];
    	      	                     WORK4 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( N, N, VT, LDVT, S, WORK, WORK2, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Multiply Q in U by left bidiagonalizing vectors
    	      	*                    in VT
    	      	*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
    	      	*/
    	      	                     dormbr( 'Q', 'R', 'N', M, N, N, VT, LDVT,
    	      	                            WORK2, U, LDU, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate right bidiagonalizing vectors in VT
    	      	*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', N, N, N, VT, LDVT, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + N;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of A in U and computing right
    	      	*                    singular vectors of A in VT
    	      	*                    (Workspace: need BDSPAC)
    	      	*/
    	      	                     WORK4 = new double[4*N];
    	      	                     dbdsqr( 'U', N, N, M, 0, S, WORK, VT,
    	      	                                  LDVT, U, LDU, DUM2, 1, WORK4, INFO );
    	      	                  }
    	      	               }
    	      	            }
    	      	            else if ( WNTUA ) {
    	      	               if ( WNTVN ) {
    	      	/*
    	      	*                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
    	      	*                 M left singular vectors to be computed in U and
    	      	*                 no right singular vectors to be computed
    	      	*/
    	      	                  if ( LWORK >= N*N+Math.max( N+M, Math.max(4*N, BDSPAC) ) ) {
    	      	/*
    	      	*                    Sufficient workspace for a fast algorithm
    	      	*/
    	      	                     IR = 1;
    	      	                     if ( LWORK >= WRKBL+LDA*N ) {
    	      	/*
    	      	*                       WORK(IR) is LDA by N
    	      	*/
    	      	                        LDWRKR = LDA;
    	      	                     }
    	      	                     else {
    	      	/*
    	      	*                       WORK(IR) is N by N
    	      	*/
    	      	                        LDWRKR = N;
    	      	                     }
    	      	                     ITAU = IR + LDWRKR*N;
    	      	                     IWORK = ITAU + N;
    	      	/*
    	      	*                    Compute A=Q*R, copying result to U
    	      	*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M,N)];
    	      	                     WORK3 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgeqrf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'L', M, N, A, LDA, U, LDU );
    	      	/*
    	      	*                    Copy R to WORK(IR), zeroing out below it
    	      	*/
    	      	                     ARRAY = new double[LDWRKR][N];
    	      	                     dlacpy( 'U', N, N, A, LDA, ARRAY, LDWRKR );
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                    	 for (i = 0; i < LDWRKR; i++) {
    	      	                    		 WORK[k++] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                     ARRAY2 = new double[LDWRKR][N-1];
    	      	                     k = 0;
    	      	                     for (j = 0; j < N-1; j++) {
    	      	                    	 for (i = 0; i < LDWRKR; i++) {
    	      	                    		 ARRAY2[i][j] = WORK[k+1];
    	      	                    		 k++;
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY2, LDWRKR );
    	      	                     k = 0;
    	      	                     for (j = 0; j < N-1; j++) {
    	      	                    	 for (i = 0; i < LDWRKR; i++) {
    	      	                    		 WORK[k+1] = ARRAY2[i][j];
    	      	                    		 k++;
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Generate Q in U
    	      	*                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
    	      	*/
    	      	                     dorgqr( M, M, N, U, LDU, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + N;
    	      	                     ITAUP = ITAUQ + N;
    	      	                     IWORK = ITAUP + N;
    	      	/*
    	      	*                    Bidiagonalize R in WORK(IR)
    	      	*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
    	      	*/
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                    	 for (i = 0; i < LDWRKR; i++) {
    	      	                    		 ARRAY[i][j] = WORK[k++];
    	      	                    	 }
    	      	                     }
    	      	                     WORK2 = new double[N-1];
    	      	                     WORK3 = new double[N];
    	      	                     WORK4 = new double[N];
    	      	                     WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( N, N, ARRAY, LDWRKR, S, WORK2, WORK3,
    	      	                                  WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors in WORK(IR)
    	      	*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
    	      	*/
    	      	                     dorgbr( 'Q', N, N, N, ARRAY, LDWRKR,
    	      	                                  WORK3, WORK5, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + N;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of R in WORK(IR)
    	      	*                    (Workspace: need N*N+BDSPAC)
    	      	*/
    	      	                     WORK5 = new double[4*N];
    	      	                     dbdsqr( 'U', N, 0, N, 0, S, WORK2, DUM2,
    	      	                                  1, ARRAY, LDWRKR, DUM2, 1, WORK5, INFO );
    	      	/*
    	      	*                    Multiply Q in U by left singular vectors of R in
    	      	*                    WORK(IR), storing result in A
    	      	*                    (Workspace: need N*N)
    	      	*/
    	      	                     dgemm( 'N', 'N', M, N, N, 1.0, U, LDU, ARRAY, LDWRKR, 0.0, A, LDA );
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                    	 for (i = 0; i < LDWRKR; i++) {
    	      	                    		 WORK[k++] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Copy left singular vectors of A from A to U
    	      	*/
    	      	                     dlacpy( 'F', M, N, A, LDA, U, LDU );
    	      	                  }
    	      	                  else {  
    	      	/*
    	      	*                    Insufficient workspace for a fast algorithm
    	      	*/
    	      	                     ITAU = 1;
    	      	                     IWORK = ITAU + N;
    	      	/*
    	      	*                    Compute A=Q*R, copying result to U
    	      	*                    (Workspace: need 2*N, prefer N+N*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgeqrf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'L', M, N, A, LDA, U, LDU );
    	      	/*
    	      	*                    Generate Q in U
    	      	*                    (Workspace: need N+M, prefer N+M*NB)
    	      	*/
    	      	                     dorgqr( M, M, N, U, LDU, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + N;
    	      	                     ITAUP = ITAUQ + N;
    	      	                     IWORK = ITAUP + N;
    	      	/*
    	      	*                    Zero out below R in A
    	      	*/
    	      	                     IROW = Math.max(1,N-1);
    	      	                     ARRAY = new double[IROW][N-1];
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < N-1; j++) {
    	      	                    		 ARRAY[i][j] = A[i+1][j];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY, IROW );
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < N-1; j++) {
    	      	                    		 A[i+1][j] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Bidiagonalize R in A
    	      	*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
    	      	*/
    	      	                     WORK2 = new double[N];
    	      	                     WORK3 = new double[N];
    	      	                     WORK4 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( N, N, A, LDA, S, WORK, WORK2, WORK3,
    	      	                                  WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Multiply Q in U by left bidiagonalizing vectors
    	      	*                    in A
    	      	*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
    	      	*/
    	      	                     dormbr( 'Q', 'R', 'N', M, N, N, A, LDA,
    	      	                                  WORK2, U, LDU, WORK4, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + N;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of A in U
    	      	*                    (Workspace: need BDSPAC)
    	      	*/
    	      	                     WORK4 = new double[4*N];
    	      	                     dbdsqr( 'U', N, 0, M, 0, S, WORK, DUM2,
    	      	                                 1, U, LDU, DUM2, 1, WORK4, INFO );
    	      	                  }
    	      	               }
    	      	            else if ( WNTVO ) {
    	      	/*
    	      	*                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
    	      	*                 M left singular vectors to be computed in U and
    	      	*                 N right singular vectors to be overwritten on A
    	      	*/
    	      	                  if ( LWORK >= 2*N*N+Math.max( N+M, Math.max(4*N, BDSPAC) ) ) {
    	      	/*
    	      	*                    Sufficient workspace for a fast algorithm
    	      	*/
    	      	                     IU = 1;
    	      	                     if ( LWORK >= WRKBL+2*LDA*N ) {
    	      	/*
    	      	*                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
    	      	*/
    	      	                        LDWRKU = LDA;
    	      	                        IR = IU + LDWRKU*N;
    	      	                        LDWRKR = LDA;
    	      	                     }
    	      	                     else if( LWORK >= WRKBL+( LDA+N )*N ) {
    	      	/*
    	      	*                       WORK(IU) is LDA by N and WORK(IR) is N by N
    	      	*/
    	      	                        LDWRKU = LDA;
    	      	                        IR = IU + LDWRKU*N;
    	      	                        LDWRKR = N;
    	      	                     }
    	      	                     else {
    	      	/*
    	      	*                       WORK(IU) is N by N and WORK(IR) is N by N
    	      	*/
    	      	                        LDWRKU = N;
    	      	                        IR = IU + LDWRKU*N;
    	      	                        LDWRKR = N;
    	      	                     }
    	      	                     ITAU = IR + LDWRKR*N;
    	      	                     IWORK = ITAU + N;
    	      	/*
    	      	*                    Compute A=Q*R, copying result to U
    	      	*                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M, N)];
    	      	                     WORK3 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgeqrf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'L', M, N, A, LDA, U, LDU );
    	      	/*
    	      	*                    Generate Q in U
    	      	*                    (Workspace: need 2*N*N+N+M, prefer 2*N*N+N+M*NB)
    	      	*/
    	      	                     dorgqr( M, M, N, U, LDU, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy R to WORK(IU), zeroing out below it
    	      	*/
    	      	                     ARRAY = new double[LDWRKU][N];
    	      	                     dlacpy( 'U', N, N, A, LDA, ARRAY, LDWRKU );
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 WORK[k++] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                     ARRAY2 = new double[LDWRKU][N-1];
    	      	                     k = 0;
    	      	                     for (j = 0; j < N-1; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 ARRAY2[i][j] = WORK[k+1];
    	      	                    		 k++;
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY2, LDWRKU );
    	      	                     k = 0;
    	      	                     for (j = 0; j < N-1; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 WORK[k+1] = ARRAY2[i][j];
    	      	                    		 k++;
    	      	                    	 }
    	      	                     }
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + N;
    	      	                     ITAUP = ITAUQ + N;
    	      	                     IWORK = ITAUP + N;
    	      	/*
    	      	*                    Bidiagonalize R in WORK(IU), copying result to
    	      	*                    WORK(IR)
    	      	*                    (Workspace: need 2*N*N+4*N,
    	      	*                                prefer 2*N*N+3*N+2*N*NB)
    	      	*/
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 ARRAY[i][j] = WORK[k++];
    	      	                    	 }
    	      	                     }
    	      	                     WORK2 = new double[N-1];
    	      	                     WORK3 = new double[N];
    	      	                     WORK4 = new double[N];
    	      	                     WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( N, N, ARRAY, LDWRKU, S, WORK2, WORK3,
    	      	                                  WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                     ARRAY2 = new double[LDWRKR][N];
    	      	                     dlacpy( 'U', N, N, ARRAY, LDWRKU, ARRAY2, LDWRKR );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors in WORK(IU)
    	      	*                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
    	      	*/
    	      	                     dorgbr( 'Q', N, N, N, ARRAY, LDWRKU,
    	      	                                  WORK3, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate right bidiagonalizing vectors in WORK(IR)
    	      	*                    (Workspace: need 2*N*N+4*N-1,
    	      	*                                prefer 2*N*N+3*N+(N-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', N, N, N, ARRAY2, LDWRKR, WORK4, WORK5,
    	      	                                  LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + N;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of R in WORK(IU) and computing
    	      	*                    right singular vectors of R in WORK(IR)
    	      	*                    (Workspace: need 2*N*N+BDSPAC)
    	      	*/
    	      	                     WORK5 = new double[4*N];
    	      	                     dbdsqr( 'U', N, N, N, 0, S, WORK2,
    	      	                                  ARRAY2, LDWRKR, ARRAY, LDWRKU, DUM2, 1, WORK5, INFO );
    	      	/*
    	      	*                    Multiply Q in U by left singular vectors of R in
    	      	*                    WORK(IU), storing result in A
    	      	*                    (Workspace: need N*N)
    	      	*/
    	      	                     dgemm( 'N', 'N', M, N, N, 1.0, U, LDU,
    	      	                                 ARRAY, LDWRKU, 0.0, A, LDA );
    	      	/*
    	      	*                    Copy left singular vectors of A from A to U
    	      	*/
    	      	                     dlacpy( 'F', M, N, A, LDA, U, LDU );
    	      	/*
    	      	*                    Copy right singular vectors of R from WORK(IR) to A
    	      	*/
    	      	                     dlacpy( 'F', N, N, ARRAY2, LDWRKR, A, LDA );
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 WORK[k++] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    Insufficient workspace for a fast algorithm
    	      	*/
    	      	                     ITAU = 1;
    	      	                     IWORK = ITAU + N;
    	      	/*
    	      	*                    Compute A=Q*R, copying result to U
    	      	*                    (Workspace: need 2*N, prefer N+N*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgeqrf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'L', M, N, A, LDA, U, LDU );
    	      	/*
    	      	*                    Generate Q in U
    	      	*                    (Workspace: need N+M, prefer N+M*NB)
    	      	*/
    	      	                     dorgqr( M, M, N, U, LDU, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + N;
    	      	                     ITAUP = ITAUQ + N;
    	      	                     IWORK = ITAUP + N;
    	      	/*
    	      	*                    Zero out below R in A
    	      	*/
    	      	                     IROW = Math.max(1,N-1);
    	      	                     ARRAY = new double[IROW][N-1];
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < N-1; j++) {
    	      	                    		 ARRAY[i][j] = A[i+1][j];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY, IROW );
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < N-1; j++) {
    	      	                    		 A[i+1][j] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Bidiagonalize R in A
    	      	*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
    	      	*/
    	      	                     WORK2 = new double[N];
    	      	                     WORK3 = new double[N];
    	      	                     WORK4 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgebrd( N, N, A, LDA, S, WORK,
    	      	                                  WORK2, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Multiply Q in U by left bidiagonalizing vectors
    	      	*                    in A
    	      	*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
    	      	*/
    	      	                     dormbr( 'Q', 'R', 'N', M, N, N, A, LDA,
    	      	                                  WORK2, U, LDU, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate right bidiagonalizing vectors in A
    	      	*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', N, N, N, A, LDA, WORK3,
    	      	                                  WORK4, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + N;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of A in U and computing right
    	      	*                    singular vectors of A in A
    	      	*                    (Workspace: need BDSPAC)
    	      	*/
    	      	                     WORK4 = new double[4*N];
    	      	                     dbdsqr( 'U', N, N, M, 0, S, WORK, A,
    	      	                                  LDA, U, LDU, DUM2, 12, WORK4, INFO );
    	      	                  }
    	      	            }
    	      	            else if ( WNTVAS ) {
    	      	/*
    	      	*                 Path 9 (M much larger than N, JOBU='A', JOBVT='S'
    	      	*                         or 'A')
    	      	*                 M left singular vectors to be computed in U and
    	      	*                 N right singular vectors to be computed in VT
    	      	*/
    	      	                  if ( LWORK >= N*N+Math.max( N+M, Math.max(4*N, BDSPAC) ) ) {
    	      	/*
    	      	*                    Sufficient workspace for a fast algorithm
    	      	*/
    	      	                     IU = 1;
    	      	                     if ( LWORK >= WRKBL+LDA*N ) {
    	      	/*
    	      	*                       WORK(IU) is LDA by N
    	      	*/
    	      	                        LDWRKU = LDA;
    	      	                     }
    	      	                     else {
    	      	/*
    	      	*                       WORK(IU) is N by N
    	      	*/
    	      	                        LDWRKU = N;
    	      	                     }
    	      	                     ITAU = IU + LDWRKU*N;
    	      	                     IWORK = ITAU + N;
    	      	/*
    	      	*                    Compute A=Q*R, copying result to U
    	      	*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M, N)];
    	      	                     WORK3 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgeqrf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'L', M, N, A, LDA, U, LDU );
    	      	/*
    	      	*                    Generate Q in U
    	      	*                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
    	      	*/
    	      	                     dorgqr( M, M, N, U, LDU, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy R to WORK(IU), zeroing out below it
    	      	*/
    	      	                     ARRAY = new double[LDWRKU][N];
    	      	                     dlacpy( 'U', N, N, A, LDA, ARRAY, LDWRKU );
    	      	                     ARRAY2 = new double[LDWRKU][N-1];
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                         for (i = 0; i < LDWRKU; i++) {
    	      	                        	 WORK[k++] = ARRAY[i][j];
    	      	                         }
    	      	                     }
    	      	                     k = 0;
    	      	                     for (j = 0; j < N-1; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 ARRAY2[i][j] = WORK[k+1];
    	      	                    		 k++;
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY2, LDWRKU );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + N;
    	      	                     ITAUP = ITAUQ + N;
    	      	                     IWORK = ITAUP + N;
    	      	/*
    	      	*                    Bidiagonalize R in WORK(IU), copying result to VT
    	      	*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
    	      	*/
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                         for (i = 0; i < LDWRKU; i++) {
    	      	                        	 ARRAY[i][j] = WORK[k++];
    	      	                         }
    	      	                     }
    	      	                     WORK2 = new double[N-1];
    	      	                     WORK3 = new double[N];
    	      	                     WORK4 = new double[N];
    	      	                     WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( N, N, ARRAY, LDWRKU, S, WORK2, WORK3,
    	      	                                  WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'U', N, N, ARRAY, LDWRKU, VT, LDVT );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors in WORK(IU)
    	      	*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
    	      	*/
    	      	                     dorgbr( 'Q', N, N, N, ARRAY, LDWRKU,
    	      	                                  WORK3, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate right bidiagonalizing vectors in VT
    	      	*                    (Workspace: need N*N+4*N-1,
    	      	*                                prefer N*N+3*N+(N-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', N, N, N, VT, LDVT, WORK4,
    	      	                                  WORK5, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + N;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of R in WORK(IU) and computing
    	      	*                    right singular vectors of R in VT
    	      	*                    (Workspace: need N*N+BDSPAC)
    	      	*/
    	      	                     WORK5 = new double[4*N];
    	      	                     dbdsqr( 'U', N, N, N, 0, S, WORK2, VT,
    	      	                                  LDVT, ARRAY, LDWRKU, DUM2, 1, WORK5, INFO );
    	      	/*
    	      	*                    Multiply Q in U by left singular vectors of R in
    	      	*                    WORK(IU), storing result in A
    	      	*                    (Workspace: need N*N)
    	      	*/
    	      	                     dgemm( 'N', 'N', M, N, N, 1.0, U, LDU, ARRAY, LDWRKU, 0.0, A, LDA );
    	      	                     k = 0;
    	      	                     for (j = 0; j < N; j++) {
    	      	                         for (i = 0; i < LDWRKU; i++) {
    	      	                        	 WORK[k++] = ARRAY[i][j];
    	      	                         }
    	      	                     }
    	      	/*
    	      	*                    Copy left singular vectors of A from A to U
    	      	*/
    	      	                     dlacpy( 'F', M, N, A, LDA, U, LDU );
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    Insufficient workspace for a fast algorithm
    	      	*/
    	      	                     ITAU = 1;
    	      	                     IWORK = ITAU + N;
    	      	/*
    	      	*                    Compute A=Q*R, copying result to U
    	      	*                    (Workspace: need 2*N, prefer N+N*NB)
    	      	*/ 
    	      	                     WORK2 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgeqrf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'L', M, N, A, LDA, U, LDU );
    	      	/*
    	      	*                    Generate Q in U
    	      	*                    (Workspace: need N+M, prefer N+M*NB)
    	      	*/
    	      	                     dorgqr( M, M, N, U, LDU, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy R from A to VT, zeroing out below it
    	      	*/
    	      	                     dlacpy( 'U', N, N, A, LDA, VT, LDVT );
    	      	                     if ( N > 1 ) {
    	      	                       IROW = Math.max(1,N-1);
    	      	                       ARRAY = new double[IROW][N-1];
    	      	                       for (i = 0; i < IROW; i++) {
    	      	                    	   for (j = 0; j < N-1; j++) {
    	      	                    		   ARRAY[i][j] = VT[1+i][j];
    	      	                    	   }
    	      	                       }
    	      	                       dlaset( 'L', N-1, N-1, 0.0, 0.0, ARRAY, IROW );
    	      	                       for (i = 0; i < IROW; i++) {
    	      	                    	   for (j = 0; j < N-1; j++) {
    	      	                    		   VT[1+i][j] = ARRAY[i][j];
    	      	                    	   }
    	      	                       }
    	      	                     }
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + N;
    	      	                     ITAUP = ITAUQ + N;
    	      	                     IWORK = ITAUP + N;
    	      	/*
    	      	*                    Bidiagonalize R in VT
    	      	*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
    	      	*/
    	      	                     WORK2 = new double[N];
    	      	                     WORK3 = new double[N];
    	      	                     WORK4 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( N, N, VT, LDVT, S, WORK,
    	      	                                  WORK2, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Multiply Q in U by left bidiagonalizing vectors
    	      	*                    in VT
    	      	*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
    	      	*/
    	      	                     dormbr( 'Q', 'R', 'N', M, N, N, VT, LDVT,
    	      	                                  WORK2, U, LDU, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate right bidiagonalizing vectors in VT
    	      	*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', N, N, N, VT, LDVT, WORK3,
    	      	                                  WORK4, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + N;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of A in U and computing right
    	      	*                    singular vectors of A in VT
    	      	*                    (Workspace: need BDSPAC)
    	      	*/
    	      	                     WORK4 = new double[4*N];
    	      	                     dbdsqr( 'U', N, N, M, 0, S, WORK, VT,
    	      	                                  LDVT, U, LDU, DUM2, 1, WORK4, INFO );
    	      	                  }
    	      	               }
    	      	            }
    	      	         }
    	      	         else {
    	      	/*
    	      	*           M .LT. MNTHR
    	      	*
    	      	*           Path 10 (M at least N, but not much larger)
    	      	*           Reduce to bidiagonal form without QR decomposition
    	      	*/
    	      	            IE = 1;
    	      	            ITAUQ = IE + N;
    	      	            ITAUP = ITAUQ + N;
    	      	            IWORK = ITAUP + N;
    	      	/*
    	      	*           Bidiagonalize A
    	      	*           (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
    	      	*/
    	      	            WORK2 = new double[Math.min(M,N)];
    	      	            WORK3 = new double[Math.min(M, N)];
    	      	            WORK4 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	            dgebrd( M, N, A, LDA, S, WORK, WORK2,
    	      	                         WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	            if ( WNTUAS ) {
    	      	/*
    	      	*              If left singular vectors desired in U, copy result to U
    	      	*              and generate left bidiagonalizing vectors in U
    	      	*              (Workspace: need 3*N+NCU, prefer 3*N+NCU*NB)
    	      	*/
    	      	               dlacpy( 'L', M, N, A, LDA, U, LDU );
    	      	               if ( WNTUS ) {
    	      	                  NCU = N;
    	      	               }
    	      	               if ( WNTUA ) {
    	      	                  NCU = M;
    	      	               }
    	      	               dorgbr( 'Q', M, NCU, N, U, LDU, WORK2, WORK4, LWORK-IWORK+1, IERR );
    	      	            }
    	      	            if ( WNTVAS ) {
    	      	/*
    	      	*              If right singular vectors desired in VT, copy result to
    	      	*              VT and generate right bidiagonalizing vectors in VT
    	      	*              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    	      	*/
    	      	               dlacpy( 'U', N, N, A, LDA, VT, LDVT );
    	      	               dorgbr( 'P', N, N, N, VT, LDVT, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	            }
    	      	            if( WNTUO ) {
    	      	/*
    	      	*              If left singular vectors desired in A, generate left
    	      	*              bidiagonalizing vectors in A
    	      	*              (Workspace: need 4*N, prefer 3*N+N*NB)
    	      	*/
    	      	               dorgbr( 'Q', M, N, N, A, LDA, WORK2, WORK4, LWORK-IWORK+1, IERR );
    	      	            }
    	      	            if ( WNTVO ) {
    	      	/*
    	      	*              If right singular vectors desired in A, generate right
    	      	*              bidiagonalizing vectors in A
    	      	*              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    	      	*/
    	      	               dorgbr( 'P', N, N, N, A, LDA, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	            }
    	      	            IWORK = IE + N;
    	      	            if ( WNTUAS || WNTUO ) {
    	      	               NRU = M;
    	      	            }
    	      	            if ( WNTUN ) {
    	      	               NRU = 0;
    	      	            }
    	      	            if ( WNTVAS || WNTVO ) {
    	      	               NCVT = N;
    	      	            }
    	      	            if ( WNTVN ) {
    	      	               NCVT = 0;
    	      	            }
    	      	            if ( ( !WNTUO ) && ( !WNTVO ) ) {
    	      	/*
    	      	*              Perform bidiagonal QR iteration, if desired, computing
    	      	*              left singular vectors in U and computing right singular
    	      	*              vectors in VT
    	      	*              (Workspace: need BDSPAC)
    	      	*/
    	      	               WORK4 = new double[4*N];
    	      	               dbdsqr( 'U', N, NCVT, NRU, 0, S, WORK, VT,
    	      	                            LDVT, U, LDU, DUM2, 1, WORK4, INFO );
    	      	            }
    	      	            else if ( ( !WNTUO ) && WNTVO ) {
    	      	/*
    	      	*              Perform bidiagonal QR iteration, if desired, computing
    	      	*              left singular vectors in U and computing right singular
    	      	*              vectors in A
    	      	*              (Workspace: need BDSPAC)
    	      	*/
    	      	               WORK4 = new double[4*N];
    	      	               dbdsqr( 'U', N, NCVT, NRU, 0, S, WORK, A, LDA,
    	      	                            U, LDU, DUM2, 1, WORK4, INFO );
    	      	            }
    	      	            else {
    	      	/*
    	      	*              Perform bidiagonal QR iteration, if desired, computing
    	      	*              left singular vectors in A and computing right singular
    	      	*              vectors in VT
    	      	*              (Workspace: need BDSPAC)
    	      	*/
    	      	               WORK4 = new double[4*N];
    	      	               dbdsqr( 'U', N, NCVT, NRU, 0, S, WORK, VT,
    	      	                            LDVT, A, LDA, DUM2, 1, WORK4, INFO );
    	      	            }
    	      	         }
    	                }
    	                else { 
    	      	/*
    	      	*        A has more columns than rows. If A has sufficiently more
    	      	*        columns than rows, first reduce using the LQ decomposition (if
    	      	*        sufficient workspace available)
    	      	*/
    	      	         if ( N >= MNTHR ) {
    	      	
    	      	            if ( WNTVN ) {
    	      	/*
    	      	*              Path 1t(N much larger than M, JOBVT='N')
    	      	*              No right singular vectors to be computed
    	      	*/
    	      	               ITAU = 1;
    	      	               IWORK = ITAU + M;
    	      	/*
    	      	*              Compute A=L*Q
    	      	*              (Workspace: need 2*M, prefer M+M*NB)
    	      	*/
    	      	               WORK2  = new double[Math.max(1, LWORK-IWORK+1)];
    	      	               dgelqf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*              Zero out above L
    	      	*/
    	      	               IROW = Math.max(1, M-1);
    	      	               ARRAY = new double[IROW][M-1];
    	      	               for (i = 0; i < IROW; i++) {
    	      	            	   for (j = 0; j < M-1; j++) {
    	      	            		   ARRAY[i][j] = A[i][j+1];
    	      	            	   }
    	      	               }
    	      	               dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY, IROW );
    	      	               for (i = 0; i < IROW; i++) {
    	      	            	   for (j = 0; j < M-1; j++) {
    	      	            		   A[i][j+1] = ARRAY[i][j];
    	      	            	   }
    	      	               }
    	      	               IE = 1;
    	      	               ITAUQ = IE + M;
    	      	               ITAUP = ITAUQ + M;
    	      	               IWORK = ITAUP + M;
    	      	/*
    	      	*              Bidiagonalize L in A
    	      	*              (Workspace: need 4*M, prefer 3*M+2*M*NB)
    	      	*/
    	      	               WORK2 = new double[M];
    	      	               WORK3 = new double[M];
    	      	               WORK4 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	               dgebrd( M, M, A, LDA, S, WORK, WORK2,
    	      	                            WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	               if ( WNTUO || WNTUAS ) {
    	      	/*
    	      	*                 If left singular vectors desired, generate Q
    	      	*                 (Workspace: need 4*M, prefer 3*M+M*NB)
    	      	*/
    	      	                  dorgbr( 'Q', M, M, M, A, LDA, WORK2,
    	      	                               WORK4, LWORK-IWORK+1, IERR );
    	      	               }
    	      	               IWORK = IE + M;
    	      	               NRU = 0;
    	      	               if ( WNTUO || WNTUAS ) {
    	      	                  NRU = M;
    	      	               }
    	      	/*
    	      	*              Perform bidiagonal QR iteration, computing left singular
    	      	*              vectors of A in A if desired
    	      	*              (Workspace: need BDSPAC)
    	      	*/
    	      	               WORK4 = new double[4*M];
    	      	               dbdsqr( 'U', M, 0, NRU, 0, S, WORK, DUM2, 1, A,
    	      	                            LDA, DUM2, 1, WORK4, INFO );
    	      	/*
    	      	*              If left singular vectors desired in U, copy them there
    	      	*/
    	      	               if ( WNTUAS ) {
    	      	                  dlacpy( 'F', M, M, A, LDA, U, LDU );
    	      	               }
    	      	            }
    	      	            else if ( WNTVO && WNTUN ) {
    	      	/*
    	      	*              Path 2t(N much larger than M, JOBU='N', JOBVT='O')
    	      	*              M right singular vectors to be overwritten on A and
    	      	*              no left singular vectors to be computed
    	      	*/
    	      	               if ( LWORK >= M*M+Math.max( 4*M, BDSPAC ) ) {
    	      	/*
    	      	*                 Sufficient workspace for a fast algorithm
    	      	*/
    	      	                  IR = 1;
    	      	                  if ( LWORK >= Math.max( WRKBL, LDA*N+M )+LDA*M ) {
    	      	/*
    	      	*                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
    	      	*/
    	      	                     LDWRKU = LDA;
    	      	                     CHUNK = N;
    	      	                     LDWRKR = LDA;
    	      	                  }
    	      	                  else if ( LWORK >= Math.max( WRKBL, LDA*N+M )+M*M ) {
    	      	/*
    	      	*                    WORK(IU) is LDA by N and WORK(IR) is M by M
    	      	*/
    	      	                     LDWRKU = LDA;
    	      	                     CHUNK = N;
    	      	                     LDWRKR = M;
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
    	      	*/
    	      	                     LDWRKU = M;
    	      	                     CHUNK = ( LWORK-M*M-M ) / M;
    	      	                     LDWRKR = M;
    	      	                  }
    	      	                  ITAU = IR + LDWRKR*M;
    	      	                  IWORK = ITAU + M;
    	      	/*
    	      	*                 Compute A=L*Q
    	      	*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    	      	*/
    	      	                  WORK2 = new double[Math.min(M,N)];
    	      	                  WORK3 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                  dgelqf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Copy L to WORK(IR) and zero out above it
    	      	*/
    	      	                  ARRAY = new double[LDWRKR][M];
    	      	                  dlacpy( 'L', M, M, A, LDA, ARRAY, LDWRKR );
    	      	                  ARRAY2 = new double[LDWRKR][M-1];
    	      	                  for (i = 0; i < LDWRKR; i++) {
    	      	                	  for (j = 0; j < M-1; j++) {
    	      	                		  ARRAY2[i][j] = ARRAY[i][j+1]; 
    	      	                	  }
    	      	                  }
    	      	                  dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY2, LDWRKR );
    	      	                  for (i = 0; i < LDWRKR; i++) {
    	      	                	  for (j = 0; j < M-1; j++) {
    	      	                		  ARRAY[i][j+1] = ARRAY2[i][j]; 
    	      	                	  }
    	      	                  }
    	      	/*
    	      	*                 Generate Q in A
    	      	*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    	      	*/
    	      	                  dorglq( M, N, M, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                  IE = ITAU;
    	      	                  ITAUQ = IE + M;
    	      	                  ITAUP = ITAUQ + M;
    	      	                  IWORK = ITAUP + M;
    	      	/*
    	      	*                 Bidiagonalize L in WORK(IR)
    	      	*                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
    	      	*/
    	      	                  WORK2 = new double[M-1];
    	      	                  WORK3 = new double[M];
    	      	                  WORK4 = new double[M];
    	      	                  WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                  dgebrd( M, M, ARRAY, LDWRKR, S, WORK2,
    	      	                               WORK3, WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Generate right vectors bidiagonalizing L
    	      	*                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
    	      	*/
    	      	                  dorgbr( 'P', M, M, M, ARRAY, LDWRKR,
    	      	                               WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                  IWORK = IE + M;
    	      	/*
    	      	*                 Perform bidiagonal QR iteration, computing right
    	      	*                 singular vectors of L in WORK(IR)
    	      	*                 (Workspace: need M*M+BDSPAC)
    	      	*/
    	      	                  WORK5 = new double[4*M];
    	      	                  dbdsqr( 'U', M, M, 0, 0, S, WORK2,
    	      	                               ARRAY, LDWRKR, DUM2, 1, DUM2, 1, WORK5, INFO );
    	      	                  IU = IE + M;
    	      	/*
    	      	*                 Multiply right singular vectors of L in WORK(IR) by Q
    	      	*                 in A, storing result in WORK(IU) and copying to A
    	      	*                 (Workspace: need M*M+2*M, prefer M*M+M*N+M)
    	      	*/
    	      	                  for ( I = 1; I <= N; I += CHUNK) {
    	      	                     BLK = Math.min( N-I+1, CHUNK );
    	      	                     ARRAY2 = new double[LDA][BLK];
    	      	                     ARRAY3 = new double[LDWRKU][BLK];
    	      	                     for (i = 0; i < LDA; i++) {
    	      	                    	 for (j = 0; j < BLK; j++) {
    	      	                    		 ARRAY2[i][j] = A[i][j + I - 1];
    	      	                    	 }
    	      	                     }
    	      	                     dgemm( 'N', 'N', M, BLK, M, 1.0, ARRAY,
    	      	                                 LDWRKR, ARRAY2, LDA, 0.0, ARRAY3, LDWRKU );
    	      	                     dlacpy( 'F', M, BLK, ARRAY3, LDWRKU, ARRAY2, LDA );
    	      	                     for (i = 0; i < LDA; i++) {
    	      	                    	 for (j = 0; j < BLK; j++) {
    	      	                    		 A[i][j + I - 1] = ARRAY2[i][j];
    	      	                    	 }
    	      	                     }
    	      	                  } // for ( I = 1; I <= N; I += CHUNK)
    	      	                  k = 0;
    	      	                  for (j = 0; j < M; j++) {
    	      	                	  for (i = 0; i < LDWRKR; i++) {
    	      	                		  WORK[k++] = ARRAY[i][j];
    	      	                	  }
    	      	                  }
    	      	               }
    	      	               else {
    	      	/*
    	      	*                 Insufficient workspace for a fast algorithm
    	      	*/
    	      	                  IE = 1;
    	      	                  ITAUQ = IE + M;
    	      	                  ITAUP = ITAUQ + M;
    	      	                  IWORK = ITAUP + M;
    	      	/*
    	      	*                 Bidiagonalize A
    	      	*                 (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
    	      	*/
    	      	                  WORK2 = new double[Math.min(M,N)];
    	      	                  WORK3 = new double[Math.min(M, N)];
    	      	                  WORK4 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                  dgebrd( M, N, A, LDA, S, WORK,
    	      	                               WORK2, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Generate right vectors bidiagonalizing A
    	      	*                 (Workspace: need 4*M, prefer 3*M+M*NB)
    	      	*/
    	      	                  dorgbr( 'P', M, N, M, A, LDA, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	                  IWORK = IE + M;
    	      	/*
    	      	*                 Perform bidiagonal QR iteration, computing right
    	      	*                 singular vectors of A in A
    	      	*                 (Workspace: need BDSPAC)
    	      	*/
    	      	                  WORK = new double[4*M];
    	      	                  dbdsqr( 'L', M, N, 0, 0, S, WORK, A, LDA, DUM2, 1, DUM2, 1, WORK4, INFO );
    	      	               }
    	      	            }
    	      	            else if ( WNTVO && WNTUAS ) {
    	      	/*
    	      	*              Path 3t(N much larger than M, JOBU='S' or 'A', JOBVT='O')
    	      	*              M right singular vectors to be overwritten on A and
    	      	*              M left singular vectors to be computed in U
    	      	*/
    	      	               if ( LWORK >= M*M+Math.max( 4*M, BDSPAC ) ) {
    	      	/*
    	      	*                 Sufficient workspace for a fast algorithm
    	      	*/
    	      	                  IR = 1;
    	      	                  if ( LWORK >= Math.max( WRKBL, LDA*N+M )+LDA*M ) {
    	      	/*
    	      	*                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
    	      	*/
    	      	                     LDWRKU = LDA;
    	      	                     CHUNK = N;
    	      	                     LDWRKR = LDA;
    	      	                  }
    	      	                  else if ( LWORK >= Math.max( WRKBL, LDA*N+M )+M*M ) {
    	      	/*
    	      	*                    WORK(IU) is LDA by N and WORK(IR) is M by M
    	      	*/
    	      	                     LDWRKU = LDA;
    	      	                     CHUNK = N;
    	      	                     LDWRKR = M;
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
    	      	*/
    	      	                     LDWRKU = M;
    	      	                     CHUNK = ( LWORK-M*M-M ) / M;
    	      	                     LDWRKR = M;
    	      	                  }
    	      	                  ITAU = IR + LDWRKR*M;
    	      	                  IWORK = ITAU + M;
    	      	/*
    	      	*                 Compute A=L*Q
    	      	*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    	      	*/
    	      	                  WORK2 = new double[Math.min(M, N)];
    	      	                  WORK3 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                  dgelqf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Copy L to U, zeroing about above it
    	      	*/
    	      	                  dlacpy( 'L', M, M, A, LDA, U, LDU );
    	      	                  IROW = Math.max(1,M-1);
    	      	                  ARRAY = new double[IROW][M-1];
    	      	                  for (i = 0; i < IROW; i++) {
    	      	                	  for (j = 0; j < M-1; j++) {
    	      	                		  ARRAY[i][j] = U[i][j+1];
    	      	                	  }
    	      	                  }
    	      	                  dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY, IROW );
    	      	                  for (i = 0; i < IROW; i++) {
    	      	                	  for (j = 0; j < M-1; j++) {
    	      	                		  U[i][j+1] = ARRAY[i][j];
    	      	                	  }
    	      	                  }
    	      	/*
    	      	*                 Generate Q in A
    	      	*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    	      	*/
    	      	                  dorglq( M, N, M, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                  IE = ITAU;
    	      	                  ITAUQ = IE + M;
    	      	                  ITAUP = ITAUQ + M;
    	      	                  IWORK = ITAUP + M;
    	      	/*
    	      	*                 Bidiagonalize L in U, copying result to WORK(IR)
    	      	*                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
    	      	*/
    	      	                  WORK2 = new double[M-1];
    	      	                  WORK3 = new double[M];
    	      	                  WORK4 = new double[M];
    	      	                  WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                  dgebrd( M, M, U, LDU, S, WORK2,
    	      	                               WORK3, WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                  ARRAY = new double[LDWRKR][M];
    	      	                  dlacpy( 'U', M, M, U, LDU, ARRAY, LDWRKR );
    	      	/*
    	      	*                 Generate right vectors bidiagonalizing L in WORK(IR)
    	      	*                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
    	      	*/
    	      	                  dorgbr( 'P', M, M, M, ARRAY, LDWRKR,
    	      	                               WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Generate left vectors bidiagonalizing L in U
    	      	*                 (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
    	      	*/
    	      	                  dorgbr( 'Q', M, M, M, U, LDU, WORK3, WORK5, LWORK-IWORK+1, IERR );
    	      	                  IWORK = IE + M;
    	      	/*
    	      	*                 Perform bidiagonal QR iteration, computing left
    	      	*                 singular vectors of L in U, and computing right
    	      	*                 singular vectors of L in WORK(IR)
    	      	*                 (Workspace: need M*M+BDSPAC)
    	      	*/
    	      	                  WORK5 = new double[4*M];
    	      	                  dbdsqr( 'U', M, M, M, 0, S, WORK2,
    	      	                               ARRAY, LDWRKR, U, LDU, DUM2, 1, WORK5, INFO );
    	      	                  IU = IE + M;
    	      	/*
    	      	*                 Multiply right singular vectors of L in WORK(IR) by Q
    	      	*                 in A, storing result in WORK(IU) and copying to A
    	      	*                 (Workspace: need M*M+2*M, prefer M*M+M*N+M))
    	      	*/
    	      	                  for (I = 1; I <= N; I += CHUNK) {
    	      	                     BLK = Math.min( N-I+1, CHUNK );
    	      	                     ARRAY2 = new double[LDA][BLK];
    	      	                     ARRAY3 = new double[LDWRKU][BLK];
    	      	                     for (i = 0; i < LDA; i++) {
    	      	                    	 for (j = 0; j < BLK; j++) {
    	      	                    		 ARRAY2[i][j] = A[i][j+I-1];
    	      	                    	 }
    	      	                     }
    	      	                     dgemm( 'N', 'N', M, BLK, M, 1.0, ARRAY,
    	      	                                 LDWRKR, ARRAY2, LDA, 0.0, ARRAY3, LDWRKU );
    	      	                     dlacpy( 'F', M, BLK, ARRAY3, LDWRKU, ARRAY2, LDA );
    	      	                     for (i = 0; i < LDA; i++) {
    	      	                    	 for (j = 0; j < BLK; j++) {
    	      	                    		 A[i][j+I-1] = ARRAY2[i][j];
    	      	                    	 }
    	      	                     }
    	      	                  } // for (I = 1; I <= N; I += CHUNK)
    	      	                  k = 0;
    	      	                  for (j = 0; j < M; j++) {
    	      	                	  for (i = 0; i < LDWRKR; i++) {
    	      	                		  WORK[k++] = ARRAY[i][j];
    	      	                	  }
    	      	                  }
    	      	               }
    	      	               else {
    	      	/*
    	      	*                 Insufficient workspace for a fast algorithm
    	      	*/
    	      	                  ITAU = 1;
    	      	                  IWORK = ITAU + M;
    	      	/*
    	      	*                 Compute A=L*Q
    	      	*                 (Workspace: need 2*M, prefer M+M*NB)
    	      	*/
    	      	                  WORK2 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                  dgelqf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Copy L to U, zeroing out above it
    	      	*/
    	      	                  dlacpy( 'L', M, M, A, LDA, U, LDU );
    	      	                  IROW = Math.max(1,M-1);
    	      	                  ARRAY = new double[IROW][M-1];
    	      	                  for (i = 0; i < IROW; i++) {
    	      	                	  for (j = 0; j < M-1; j++) {
    	      	                		  ARRAY[i][j] = U[i][j+1];
    	      	                	  }
    	      	                  }
    	      	                  dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY, IROW );
    	      	                  for (i = 0; i < IROW; i++) {
    	      	                	  for (j = 0; j < M-1; j++) {
    	      	                		  U[i][j+1] = ARRAY[i][j];
    	      	                	  }
    	      	                  }
    	      	/*
    	      	*                 Generate Q in A
    	      	*                 (Workspace: need 2*M, prefer M+M*NB)
    	      	*/
    	      	                  dorglq( M, N, M, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                  IE = ITAU;
    	      	                  ITAUQ = IE + M;
    	      	                  ITAUP = ITAUQ + M;
    	      	                  IWORK = ITAUP + M;
    	      	/*
    	      	*                 Bidiagonalize L in U
    	      	*                 (Workspace: need 4*M, prefer 3*M+2*M*NB)
    	      	*/
    	      	                  WORK2 = new double[M];
    	      	                  WORK3 = new double[M];
    	      	                  WORK4 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                  dgebrd( M, M, U, LDU, S, WORK,
    	      	                               WORK2, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Multiply right vectors bidiagonalizing L by Q in A
    	      	*                 (Workspace: need 3*M+N, prefer 3*M+N*NB)
    	      	*/
    	      	                  dormbr( 'P', 'L', 'T', M, N, M, U, LDU,
    	      	                               WORK3, A, LDA, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                 Generate left vectors bidiagonalizing L in U
    	      	*                 (Workspace: need 4*M, prefer 3*M+M*NB)
    	      	*/
    	      	                  dorgbr( 'Q', M, M, M, U, LDU, WORK2,
    	      	                               WORK4, LWORK-IWORK+1, IERR );
    	      	                  IWORK = IE + M;
    	      	/*
    	      	*                 Perform bidiagonal QR iteration, computing left
    	      	*                 singular vectors of A in U and computing right
    	      	*                 singular vectors of A in A
    	      	*                 (Workspace: need BDSPAC)
    	      	*/
    	      	                  WORK4 = new double[4*M];
    	      	                  dbdsqr( 'U', M, N, M, 0, S, WORK, A, LDA,
    	      	                               U, LDU, DUM2, 1, WORK4, INFO );
    	      	               }
    	      	            }
    	      	            else if ( WNTVS ) {
    	      
    	      	               if ( WNTUN ) {
    	      	/*
    	      	*                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
    	      	*                 M right singular vectors to be computed in VT and
    	      	*                 no left singular vectors to be computed
    	      	*/
    	      	                  if ( LWORK >= M*M+Math.max( 4*M, BDSPAC ) ) {
    	      	/*
    	      	*                    Sufficient workspace for a fast algorithm
    	      	*/
    	      	                     IR = 1;
    	      	                     if ( LWORK >= WRKBL+LDA*M ) {
    	      	/*
    	      	*                       WORK(IR) is LDA by M
    	      	*/
    	      	                        LDWRKR = LDA;
    	      	                     }
    	      	                     else {
    	      	/*
    	      	*                       WORK(IR) is M by M
    	      	*/
    	      	                        LDWRKR = M;
    	      	                     }
    	      	                     ITAU = IR + LDWRKR*M;
    	      	                     IWORK = ITAU + M;
    	      	/*
    	      	*                    Compute A=L*Q
    	      	*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M, N)];
    	      	                     WORK3 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgelqf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy L to WORK(IR), zeroing out above it
    	      	*/
    	      	                     ARRAY = new double[LDWRKR][M];
    	      	                     dlacpy( 'L', M, M, A, LDA, ARRAY, LDWRKR );
    	                               IROW = Math.max(1, M-1);
    	                               ARRAY2 = new double[IROW][M-1];
    	                               for (j = 0; j < M-1; j++) {
    	                              	 for (i = 0; i < IROW; i++) {
    	                              		 ARRAY2[i][j] = ARRAY[i][j+1];
    	                              	 }
    	                               }
    	      	                     dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY2, LDWRKR );
    	      	                     for (j = 0; j < M-1; j++) {
    	                              	 for (i = 0; i < IROW; i++) {
    	                              		 ARRAY[i][j+1] = ARRAY2[i][j];
    	                              	 }
    	                               }
    	      	/*
    	      	*                    Generate Q in A
    	      	*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    	      	*/
    	      	                     dorglq( M, N, M, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + M;
    	      	                     ITAUP = ITAUQ + M;
    	      	                     IWORK = ITAUP + M;
    	      	/*
    	      	*                    Bidiagonalize L in WORK(IR)
    	      	*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
    	      	*/
    	      	                     WORK2 = new double[M-1];
    	      	                     WORK3 = new double[M];
    	      	                     WORK4 = new double[M];
    	      	                     WORK5 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgebrd( M, M, ARRAY, LDWRKR, S, WORK2, WORK3,
    	      	                                  WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate right vectors bidiagonalizing L in
    	      	*                    WORK(IR)
    	      	*                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', M, M, M, ARRAY, LDWRKR,
    	      	                                  WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + M;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing right
    	      	*                    singular vectors of L in WORK(IR)
    	      	*                    (Workspace: need M*M+BDSPAC)
    	      	*/
    	      	                     WORK5 = new double[4*M];
    	      	                     dbdsqr( 'U', M, M, 0, 0, S, WORK2,
    	      	                                  ARRAY, LDWRKR, DUM2, 1, DUM2, 1, WORK5, INFO );
    	      	/*
    	      	*                    Multiply right singular vectors of L in WORK(IR) by
    	      	*                    Q in A, storing result in VT
    	      	*                    (Workspace: need M*M)
    	      	*/
    	      	                     dgemm( 'N', 'N', M, N, M, 1.0, ARRAY, LDWRKR, A, LDA, 0.0, VT, LDVT );
    	      	                     k = 0;
    	      	                     for (j = 0; j < M; j++) {
    	      	                    	 for (i = 0; i < LDWRKR; i++) {
    	      	                    		 WORK[k++] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    Insufficient workspace for a fast algorithm
    	      	*/
    	      	                     ITAU = 1;
    	      	                     IWORK = ITAU + M;
    	      	/*
    	      	*                    Compute A=L*Q
    	      	*                    (Workspace: need 2*M, prefer M+M*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgelqf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy result to VT
    	      	*/
    	      	                     dlacpy( 'U', M, N, A, LDA, VT, LDVT );
    	      	/*
    	      	*                    Generate Q in VT
    	      	*                    (Workspace: need 2*M, prefer M+M*NB)
    	      	*/
    	      	                     dorglq( M, N, M, VT, LDVT, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + M;
    	      	                     ITAUP = ITAUQ + M;
    	      	                     IWORK = ITAUP + M;
    	      	/*
    	      	*                    Zero out above L in A
    	      	*/
    	      	                     IROW = Math.max(1, M-1);
    	      	                     ARRAY = new double[IROW][M-1];
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                             ARRAY[i][j] = A[i][j+1]; 
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY, IROW );
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                             A[i][j+1] = ARRAY[i][j]; 
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Bidiagonalize L in A
    	      	*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
    	      	*/
    	      	                     WORK2 = new double[M];
    	      	                     WORK3 = new double[M];
    	      	                     WORK4 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgebrd( M, M, A, LDA, S, WORK,
    	      	                                  WORK2, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Multiply right vectors bidiagonalizing L by Q in VT
    	      	*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
    	      	*/
    	      	                     dormbr( 'P', 'L', 'T', M, N, M, A, LDA,
    	      	                                  WORK3, VT, LDVT, WORK4, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + M;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing right
    	      	*                    singular vectors of A in VT
    	      	*                    (Workspace: need BDSPAC)
    	      	*/
    	      	                     WORK4 = new double[4*M];
    	      	                     dbdsqr( 'U', M, N, 0, 0, S, WORK, VT,
    	      	                                  LDVT, DUM2, 1, DUM2, 1, WORK4, INFO );
    	      	                  }
    	      	               }
    	      	               else if ( WNTUO ) {
    	      	/*
    	      	*                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
    	      	*                 M right singular vectors to be computed in VT and
    	      	*                 M left singular vectors to be overwritten on A
    	      	*/
    	      	                  if ( LWORK >= 2*M*M+Math.max( 4*M, BDSPAC ) ) {
    	      	/*
    	      	*                    Sufficient workspace for a fast algorithm
    	      	*/
    	      	                     IU = 1;
    	      	                     if ( LWORK >= WRKBL+2*LDA*M ) {
    	      	/*
    	      	*                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
    	      	*/
    	      	                        LDWRKU = LDA;
    	      	                        IR = IU + LDWRKU*M;
    	      	                        LDWRKR = LDA;
    	      	                     }
    	      	                     else if ( LWORK >= WRKBL+( LDA+M )*M ) {
    	      	/*
    	      	*                       WORK(IU) is LDA by M and WORK(IR) is M by M
    	      	*/
    	      	                        LDWRKU = LDA;
    	      	                        IR = IU + LDWRKU*M;
    	      	                        LDWRKR = M;
    	      	                     }
    	      	                     else {
    	      	/*
    	      	*                       WORK(IU) is M by M and WORK(IR) is M by M
    	      	*/
    	      	                        LDWRKU = M;
    	      	                        IR = IU + LDWRKU*M;
    	      	                        LDWRKR = M;
    	      	                     }
    	      	                     ITAU = IR + LDWRKR*M;
    	      	                     IWORK = ITAU + M;
    	      	/*
    	      	*                    Compute A=L*Q
    	      	*                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M, N)];
    	      	                     WORK3 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgelqf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy L to WORK(IU), zeroing out below it
    	      	*/
    	      	                     ARRAY = new double[LDWRKU][M];
    	      	                     dlacpy( 'L', M, M, A, LDA, ARRAY, LDWRKU );
    	      	                     ARRAY2 = new double[LDWRKU][M-1];
    	      	                     for (i = 0; i < LDWRKU; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY2[i][j] = ARRAY[i][j+1];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY2, LDWRKU );
    	      	                     for (i = 0; i < LDWRKU; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY[i][j+1] = ARRAY2[i][j];
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Generate Q in A
    	      	*                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
    	      	*/
    	      	                     dorglq( M, N, M, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + M;
    	      	                     ITAUP = ITAUQ + M;
    	      	                     IWORK = ITAUP + M;
    	      	/*
    	      	*                    Bidiagonalize L in WORK(IU), copying result to
    	      	*                    WORK(IR)
    	      	*                    (Workspace: need 2*M*M+4*M,
    	      	*                                prefer 2*M*M+3*M+2*M*NB)
    	      	*/
    	      	                     WORK2 = new double[M-1];
    	      	                     WORK3 = new double[M];
    	      	                     WORK4 = new double[M];
    	      	                     WORK5 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgebrd( M, M, ARRAY, LDWRKU, S, WORK2, WORK3,
    	      	                                  WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                     ARRAY2 = new double[LDWRKR][M];
    	      	                     dlacpy( 'L', M, M, ARRAY, LDWRKU, ARRAY2, LDWRKR );
    	      	/*
    	      	*                    Generate right bidiagonalizing vectors in WORK(IU)
    	      	*                    (Workspace: need 2*M*M+4*M-1,
    	      	*                                prefer 2*M*M+3*M+(M-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', M, M, M, ARRAY, LDWRKU,
    	      	                                  WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors in WORK(IR)
    	      	*                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
    	      	*/
    	      	                     dorgbr( 'Q', M, M, M, ARRAY2, LDWRKR,
    	      	                                  WORK3, WORK5, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + M;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of L in WORK(IR) and computing
    	      	*                    right singular vectors of L in WORK(IU)
    	      	*                    (Workspace: need 2*M*M+BDSPAC)
    	      	*/
    	      	                     WORK5 = new double[4*M];
    	      	                     dbdsqr( 'U', M, M, M, 0, S, WORK2,
    	      	                             ARRAY, LDWRKU, ARRAY2, LDWRKR, DUM2, 1, WORK5, INFO );
    	      	/*
    	      	*                    Multiply right singular vectors of L in WORK(IU) by
    	      	*                    Q in A, storing result in VT
    	      	*                    (Workspace: need M*M)
    	      	*/
    	      	                     dgemm( 'N', 'N', M, N, M, 1.0, ARRAY,
    	      	                                 LDWRKU, A, LDA, 0.0, VT, LDVT );
    	      	/*
    	      	*                    Copy left singular vectors of L to A
    	      	*                    (Workspace: need M*M)
    	      	*/
    	      	                     dlacpy( 'F', M, M, ARRAY2, LDWRKR, A, LDA );
    	      	                     k = 0;
    	      	                     for (j = 0; j < M; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 WORK[k++] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    Insufficient workspace for a fast algorithm
    	      	*/
    	      	                     ITAU = 1;
    	      	                     IWORK = ITAU + M;
    	      	/*
    	      	*                    Compute A=L*Q, copying result to VT
    	      	*                    (Workspace: need 2*M, prefer M+M*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgelqf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'U', M, N, A, LDA, VT, LDVT );
    	      	/*
    	      	*                    Generate Q in VT
    	      	*                    (Workspace: need 2*M, prefer M+M*NB)
    	      	*/
    	      	                     dorglq( M, N, M, VT, LDVT, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + M;
    	      	                     ITAUP = ITAUQ + M;
    	      	                     IWORK = ITAUP + M;
    	      	/*
    	      	*                    Zero out above L in A
    	      	*/
    	      	                     IROW = Math.max(1,M-1);
    	      	                     ARRAY = new double[IROW][M-1];
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY[i][j] = A[i][j+1];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY, IROW );
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 A[i][j+1] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Bidiagonalize L in A
    	      	*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
    	      	*/
    	      	                     WORK2 = new double[M];
    	      	                     WORK3 = new double[M];
    	      	                     WORK4 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( M, M, A, LDA, S, WORK,
    	      	                             WORK2, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Multiply right vectors bidiagonalizing L by Q in VT
    	      	*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
    	      	*/
    	      	                     dormbr( 'P', 'L', 'T', M, N, M, A, LDA,
    	      	                              WORK3, VT, LDVT, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors of L in A
    	      	*                    (Workspace: need 4*M, prefer 3*M+M*NB)
    	      	*/
    	      	                     dorgbr( 'Q', M, M, M, A, LDA, WORK2, WORK4, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + M;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, compute left
    	      	*                    singular vectors of A in A and compute right
    	      	*                    singular vectors of A in VT
    	      	*                    (Workspace: need BDSPAC)
    	      	*/
    	      	                     WORK4 = new double[4*M];
    	      	                     dbdsqr( 'U', M, N, M, 0, S, WORK, VT,
    	      	                             LDVT, A, LDA, DUM2, 1, WORK4, INFO );
    	      	                  }
    	      	               }
    	      	               else if ( WNTUAS ) {
    	      	/*
    	      	*                 Path 6t(N much larger than M, JOBU='S' or 'A',
    	      	*                         JOBVT='S')
    	      	*                 M right singular vectors to be computed in VT and
    	      	*                 M left singular vectors to be computed in U
    	      	*/
    	      	                  if ( LWORK >= M*M+Math.max( 4*M, BDSPAC ) ) {
    	      	/*
    	      	*                    Sufficient workspace for a fast algorithm
    	      	*/
    	      	                     IU = 1;
    	      	                     if ( LWORK >= WRKBL+LDA*M ) {
    	      	/*
    	      	*                       WORK(IU) is LDA by N
    	      	*/
    	      	                        LDWRKU = LDA;
    	      	                     }
    	      	                     else {
    	      	/*
    	      	*                       WORK(IU) is LDA by M
    	      	*/
    	      	                        LDWRKU = M;
    	      	                     }
    	      	                     ITAU = IU + LDWRKU*M;
    	      	                     IWORK = ITAU + M;
    	      	/*
    	      	*                    Compute A=L*Q
    	      	*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M,N)];
    	      	                     WORK3 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgelqf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy L to WORK(IU), zeroing out above it
    	      	*/
    	      	                     ARRAY = new double[LDWRKU][M];
    	      	                     dlacpy( 'L', M, M, A, LDA, ARRAY, LDWRKU );
    	      	                     ARRAY2 = new double[LDWRKU][M-1];
    	      	                     for (i = 0; i < LDWRKU; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY2[i][j] = ARRAY[i][j+1];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY2, LDWRKU );
    	      	                     for (i = 0; i < LDWRKU; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY[i][j+1] = ARRAY2[i][j];
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Generate Q in A
    	      	*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    	      	*/
    	      	                     dorglq( M, N, M, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + M;
    	      	                     ITAUP = ITAUQ + M;
    	      	                     IWORK = ITAUP + M;
    	      	/*
    	      	*                    Bidiagonalize L in WORK(IU), copying result to U
    	      	*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
    	      	*/
    	      	                     WORK2 = new double[M-1];
    	      	                     WORK3 = new double[M];
    	      	                     WORK4 = new double[M];
    	      	                     WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( M, M, ARRAY, LDWRKU, S, WORK2, WORK3,
    	      	                             WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'L', M, M, ARRAY, LDWRKU, U, LDU );
    	      	/*
    	      	*                    Generate right bidiagonalizing vectors in WORK(IU)
    	      	*                    (Workspace: need M*M+4*M-1,
    	      	*                                prefer M*M+3*M+(M-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', M, M, M, ARRAY, LDWRKU,
    	      	                             WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors in U
    	      	*                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
    	      	*/
    	      	                     dorgbr( 'Q', M, M, M, U, LDU, WORK3,
    	      	                             WORK5, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + M;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of L in U and computing right
    	      	*                    singular vectors of L in WORK(IU)
    	      	*                    (Workspace: need M*M+BDSPAC)
    	      	*/
    	      	                     WORK5 = new double[4*M];
    	      	                     dbdsqr( 'U', M, M, M, 0, S, WORK2,
    	      	                             ARRAY, LDWRKU, U, LDU, DUM2, 1, WORK5, INFO );
    	      	/*
    	      	*                    Multiply right singular vectors of L in WORK(IU) by
    	      	*                    Q in A, storing result in VT
    	      	*                    (Workspace: need M*M)
    	      	*/
    	      	                     dgemm( 'N', 'N', M, N, M, 1.0, ARRAY,
    	      	                            LDWRKU, A, LDA, 0.0, VT, LDVT );
    	      	                     k = 0;
    	      	                     for (j = 0; j < M; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 WORK[k++] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    Insufficient workspace for a fast algorithm
    	      	*/
    	      	                     ITAU = 1;
    	      	                     IWORK = ITAU + M;
    	      	/*
    	      	*                    Compute A=L*Q, copying result to VT
    	      	*                    (Workspace: need 2*M, prefer M+M*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgelqf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'U', M, N, A, LDA, VT, LDVT );
    	      	/*
    	      	*                    Generate Q in VT
    	      	*                    (Workspace: need 2*M, prefer M+M*NB)
    	      	*/
    	      	                     dorglq( M, N, M, VT, LDVT, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy L to U, zeroing out above it
    	      	*/
    	      	                     dlacpy( 'L', M, M, A, LDA, U, LDU );
    	      	                     IROW = Math.max(1,M-1);
    	      	                     ARRAY = new double[IROW][M-1];
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY[i][j] = U[i][j+1];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY, IROW );
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 U[i][j+1] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + M;
    	      	                     ITAUP = ITAUQ + M;
    	      	                     IWORK = ITAUP + M;
    	      	/*
    	      	*                    Bidiagonalize L in U
    	      	*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
    	      	*/
    	      	                     WORK2 = new double[M];
    	      	                     WORK3 = new double[M];
    	      	                     WORK4 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( M, M, U, LDU, S, WORK,
    	      	                             WORK2, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Multiply right bidiagonalizing vectors in U by Q
    	      	*                    in VT
    	      	*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
    	      	*/
    	      	                     dormbr( 'P', 'L', 'T', M, N, M, U, LDU,
    	      	                             WORK3, VT, LDVT, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors in U
    	      	*                    (Workspace: need 4*M, prefer 3*M+M*NB)
    	      	*/
    	      	                     dorgbr( 'Q', M, M, M, U, LDU, WORK2,
    	      	                             WORK4, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + M;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of A in U and computing right
    	      	*                    singular vectors of A in VT
    	      	*                    (Workspace: need BDSPAC)
    	      	*/
    	      	                     WORK4 = new double[4*M];
    	      	                     dbdsqr( 'U', M, N, M, 0, S, WORK, VT,
    	      	                             LDVT, U, LDU, DUM2, 1, WORK4, INFO );
    	      	                  }
    	      	               }
    	      	            }
    	      	            else if( WNTVA ) {
    	      	
    	      	               if ( WNTUN ) {
    	      	/*
    	      	*                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
    	      	*                 N right singular vectors to be computed in VT and
    	      	*                 no left singular vectors to be computed
    	      	*/
    	      	                  if ( LWORK >= M*M+Math.max( N+M, Math.max(4*M, BDSPAC) ) ) {
    	      	/*
    	      	*                    Sufficient workspace for a fast algorithm
    	      	*/
    	      	                     IR = 1;
    	      	                     if ( LWORK >= WRKBL+LDA*M ) {
    	      	/*
    	      	*                       WORK(IR) is LDA by M
    	      	*/
    	      	                        LDWRKR = LDA;
    	      	                     }
    	      	                     else {
    	      	/*
    	      	*                       WORK(IR) is M by M
    	      	*/
    	      	                        LDWRKR = M;
    	      	                     }
    	      	                     ITAU = IR + LDWRKR*M;
    	      	                     IWORK = ITAU + M;
    	      	/*
    	      	*                    Compute A=L*Q, copying result to VT
    	      	*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M, N)];
    	      	                     WORK3 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgelqf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'U', M, N, A, LDA, VT, LDVT );
    	      	/*
    	      	*                    Copy L to WORK(IR), zeroing out above it
    	      	*/
    	      	                     ARRAY = new double[LDWRKR][M];
    	      	                     dlacpy( 'L', M, M, A, LDA, ARRAY, LDWRKR );
    	      	                     ARRAY2 = new double[LDWRKR][M-1];
    	      	                     for (i = 0; i < LDWRKR; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY2[i][j] = ARRAY[i][j+1];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY2, LDWRKR );
    	      	                     for (i = 0; i < LDWRKR; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY[i][j+1] = ARRAY2[i][j];
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Generate Q in VT
    	      	*                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
    	      	*/
    	      	                     dorglq( N, N, M, VT, LDVT, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + M;
    	      	                     ITAUP = ITAUQ + M;
    	      	                     IWORK = ITAUP + M;
    	      	/*
    	      	*                    Bidiagonalize L in WORK(IR)
    	      	*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
    	      	*/
    	      	                     WORK2 = new double[M-1];
    	      	                     WORK3 = new double[M];
    	      	                     WORK4 = new double[M];
    	      	                     WORK5 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgebrd( M, M, ARRAY, LDWRKR, S,
    	      	                             WORK2, WORK3, WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate right bidiagonalizing vectors in WORK(IR)
    	      	*                    (Workspace: need M*M+4*M-1,
    	      	*                                prefer M*M+3*M+(M-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', M, M, M, ARRAY, LDWRKR,
    	      	                             WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + M;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing right
    	      	*                    singular vectors of L in WORK(IR)
    	      	*                    (Workspace: need M*M+BDSPAC)
    	      	*/
    	      	                     WORK5 = new double[4*M];
    	      	                     dbdsqr( 'U', M, M, 0, 0, S, WORK2,
    	      	                             ARRAY, LDWRKR, DUM2, 1, DUM2, 1, WORK5, INFO );
    	      	/*
    	      	*                    Multiply right singular vectors of L in WORK(IR) by
    	      	*                    Q in VT, storing result in A
    	      	*                    (Workspace: need M*M)
    	      	*/
    	      	                     dgemm( 'N', 'N', M, N, M, 1.0, ARRAY,
    	      	                            LDWRKR, VT, LDVT, 0.0, A, LDA );
    	      	/*
    	      	*                    Copy right singular vectors of A from A to VT
    	      	*/
    	      	                     dlacpy( 'F', M, N, A, LDA, VT, LDVT );
    	      	                     k = 0;
    	      	                     for (j = 0; j < M; j++) {
    	      	                    	 for (i = 0; i < LDWRKR; i++) {
    	      	                    		 WORK[k++] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    Insufficient workspace for a fast algorithm
    	      	*/
    	      	                     ITAU = 1;
    	      	                     IWORK = ITAU + M;
    	      	/*
    	      	*                    Compute A=L*Q, copying result to VT
    	      	*                    (Workspace: need 2*M, prefer M+M*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgelqf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'U', M, N, A, LDA, VT, LDVT );
    	      	/*
    	      	*                    Generate Q in VT
    	      	*                    (Workspace: need M+N, prefer M+N*NB)
    	      	*/
    	      	                     dorglq( N, N, M, VT, LDVT, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + M;
    	      	                     ITAUP = ITAUQ + M;
    	      	                     IWORK = ITAUP + M;
    	      	/*
    	      	*                    Zero out above L in A
    	      	*/
    	      	                     IROW = Math.max(1,M-1);
    	      	                     ARRAY = new double[IROW][M-1];
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY[i][j] = A[i][j+1];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY, IROW );
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 A[i][j+1] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Bidiagonalize L in A
    	      	*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
    	      	*/
    	      	                     WORK2 = new double[M];
    	      	                     WORK3 = new double[M];
    	      	                     WORK4 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( M, M, A, LDA, S, WORK,
    	      	                             WORK2, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Multiply right bidiagonalizing vectors in A by Q
    	      	*                    in VT
    	      	*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
    	      	*/
    	      	                     dormbr( 'P', 'L', 'T', M, N, M, A, LDA,
    	      	                             WORK3, VT, LDVT, WORK4, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + M;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing right
    	      	*                    singular vectors of A in VT
    	      	*                    (Workspace: need BDSPAC)
    	      	*/
    	      	                     WORK4 = new double[4*M];
    	      	                     dbdsqr( 'U', M, N, 0, 0, S, WORK, VT,
    	      	                             LDVT, DUM2, 1, DUM2, 1, WORK4, INFO );
    	      	                  }
    	      	               }
    	      	               else if( WNTUO ) {
    	      	/*
    	      	*                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
    	      	*                 N right singular vectors to be computed in VT and
    	      	*                 M left singular vectors to be overwritten on A
    	      	*/
    	      	                  if ( LWORK >= 2*M*M+Math.max( N+M, Math.max(4*M, BDSPAC) ) ) {
    	      	/*
    	      	*                    Sufficient workspace for a fast algorithm
    	      	*/
    	      	                     IU = 1;
    	      	                     if( LWORK >= WRKBL+2*LDA*M ) {
    	      	/*
    	      	*                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
    	      	*/
    	      	                        LDWRKU = LDA;
    	      	                        IR = IU + LDWRKU*M;
    	      	                        LDWRKR = LDA;
    	      	                     }
    	      	                     else if ( LWORK >= WRKBL+( LDA+M )*M ) {
    	      	/*
    	      	*                       WORK(IU) is LDA by M and WORK(IR) is M by M
    	      	*/
    	      	                        LDWRKU = LDA;
    	      	                        IR = IU + LDWRKU*M;
    	      	                        LDWRKR = M;
    	      	                     }
    	      	                     else {
    	      	/*
    	      	*                       WORK(IU) is M by M and WORK(IR) is M by M
    	      	*/
    	      	                        LDWRKU = M;
    	      	                        IR = IU + LDWRKU*M;
    	      	                        LDWRKR = M;
    	      	                     }
    	      	                     ITAU = IR + LDWRKR*M;
    	      	                     IWORK = ITAU + M;
    	      	/*
    	      	*                    Compute A=L*Q, copying result to VT
    	      	*                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M, N)];
    	      	                     WORK3 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgelqf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'U', M, N, A, LDA, VT, LDVT );
    	      	/*
    	      	*                    Generate Q in VT
    	      	*                    (Workspace: need 2*M*M+M+N, prefer 2*M*M+M+N*NB)
    	      	*/
    	      	                     dorglq( N, N, M, VT, LDVT, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy L to WORK(IU), zeroing out above it
    	      	*/
    	      	                     ARRAY = new double[LDWRKU][M];
    	      	                     dlacpy( 'L', M, M, A, LDA, ARRAY, LDWRKU );
    	      	                     ARRAY2 = new double[LDWRKU][M-1];
    	      	                     for (i = 0; i < LDWRKU; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY2[i][j] = ARRAY[i][j+1];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY2, LDWRKU );
    	      	                     for (i = 0; i < LDWRKU; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY[i][j+1] = ARRAY2[i][j];
    	      	                    	 }
    	      	                     }
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + M;
    	      	                     ITAUP = ITAUQ + M;
    	      	                     IWORK = ITAUP + M;
    	      	/*
    	      	*                    Bidiagonalize L in WORK(IU), copying result to
    	      	*                    WORK(IR)
    	      	*                    (Workspace: need 2*M*M+4*M,
    	      	*                                prefer 2*M*M+3*M+2*M*NB)
    	      	*/
    	      	                     WORK2 = new double[M-1];
    	      	                     WORK3 = new double[M];
    	      	                     WORK4 = new double[M];
    	      	                     WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( M, M, ARRAY, LDWRKU, S, WORK2, WORK3,
    	      	                             WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                     ARRAY2 = new double[LDWRKR][M];
    	      	                     dlacpy( 'L', M, M, ARRAY, LDWRKU, ARRAY2, LDWRKR );
    	      	/*
    	      	*                    Generate right bidiagonalizing vectors in WORK(IU)
    	      	*                    (Workspace: need 2*M*M+4*M-1,
    	      	*                                prefer 2*M*M+3*M+(M-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', M, M, M, ARRAY, LDWRKU,
    	      	                             WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors in WORK(IR)
    	      	*                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
    	      	*/
    	      	                     dorgbr( 'Q', M, M, M, ARRAY2, LDWRKR,
    	      	                             WORK3, WORK5, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + M;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of L in WORK(IR) and computing
    	      	*                    right singular vectors of L in WORK(IU)
    	      	*                    (Workspace: need 2*M*M+BDSPAC)
    	      	*/
    	      	                     WORK5 = new double[4*M];
    	      	                     dbdsqr( 'U', M, M, M, 0, S, WORK2,
    	      	                             ARRAY, LDWRKU, ARRAY2, LDWRKR, DUM2, 1, WORK5, INFO );
    	      	/*
    	      	*                    Multiply right singular vectors of L in WORK(IU) by
    	      	*                    Q in VT, storing result in A
    	      	*                    (Workspace: need M*M)
    	      	*/
    	      	                     dgemm( 'N', 'N', M, N, M, 1.0, ARRAY,
    	      	                            LDWRKU, VT, LDVT, 0.0, A, LDA );
    	      	/*
    	      	*                    Copy right singular vectors of A from A to VT
    	      	*/
    	      	                     dlacpy( 'F', M, N, A, LDA, VT, LDVT );
    	      	/*
    	      	*                    Copy left singular vectors of A from WORK(IR) to A
    	      	*/
    	      	                     dlacpy( 'F', M, M, ARRAY2, LDWRKR, A, LDA );
    	      	                     k = 0;
    	      	                     for (j = 0; j < M; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 WORK[k++] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    Insufficient workspace for a fast algorithm
    	      	*/
    	      	                     ITAU = 1;
    	      	                     IWORK = ITAU + M;
    	      	/*
    	      	*                    Compute A=L*Q, copying result to VT
    	      	*                    (Workspace: need 2*M, prefer M+M*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgelqf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'U', M, N, A, LDA, VT, LDVT );
    	      	/*
    	      	*                    Generate Q in VT
    	      	*                    (Workspace: need M+N, prefer M+N*NB)
    	      	*/
    	      	                     dorglq( N, N, M, VT, LDVT, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + M;
    	      	                     ITAUP = ITAUQ + M;
    	      	                     IWORK = ITAUP + M;
    	      	/*
    	      	*                    Zero out above L in A
    	      	*/
    	      	                     IROW = Math.max(1,M-1);
    	      	                     ARRAY = new double[IROW][M-1];
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY[i][j] = A[i][j+1];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY, IROW );
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 A[i][j+1] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	/*
    	      	*                    Bidiagonalize L in A
    	      	*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
    	      	*/
    	      	                     WORK2 = new double[M];
    	      	                     WORK3 = new double[M];
    	      	                     WORK4 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( M, M, A, LDA, S, WORK, WORK2, WORK3,
    	      	                             WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Multiply right bidiagonalizing vectors in A by Q
    	      	*                    in VT
    	      	*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
    	      	*/
    	      	                     dormbr( 'P', 'L', 'T', M, N, M, A, LDA,
    	      	                             WORK3, VT, LDVT, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors in A
    	      	*                    (Workspace: need 4*M, prefer 3*M+M*NB)
    	      	*/
    	      	                     dorgbr( 'Q', M, M, M, A, LDA, WORK2,
    	      	                             WORK4, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + M;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of A in A and computing right
    	      	*                    singular vectors of A in VT
    	      	*                    (Workspace: need BDSPAC)
    	      	*/
    	      	                     WORK4 = new double[4*M];
    	      	                     dbdsqr( 'U', M, N, M, 0, S, WORK, VT,
    	      	                             LDVT, A, LDA, DUM2, 1, WORK4, INFO );
    	      	                  }
    	      	               }
    	      	               else if ( WNTUAS ) {
    	      	/*
    	      	*                 Path 9t(N much larger than M, JOBU='S' or 'A',
    	      	*                         JOBVT='A')
    	      	*                 N right singular vectors to be computed in VT and
    	      	*                 M left singular vectors to be computed in U
    	      	*/
    	      	                  if ( LWORK >= M*M+Math.max( N+M, Math.max(4*M, BDSPAC) ) ) {
    	      	/*
    	      	*                    Sufficient workspace for a fast algorithm
    	      	*/
    	      	                     IU = 1;
    	      	                     if ( LWORK >= WRKBL+LDA*M ) {
    	      	/*
    	      	*                       WORK(IU) is LDA by M
    	      	*/
    	      	                        LDWRKU = LDA;
    	      	                     }
    	      	                     else {
    	      	/*
    	      	*                       WORK(IU) is M by M
    	      	*/
    	      	                        LDWRKU = M;
    	      	                     }
    	      	                     ITAU = IU + LDWRKU*M;
    	      	                     IWORK = ITAU + M;
    	      	/*
    	      	*                    Compute A=L*Q, copying result to VT
    	      	*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.min(M, N)];
    	      	                     WORK3 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	                     dgelqf( M, N, A, LDA, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'U', M, N, A, LDA, VT, LDVT );
    	      	/*
    	      	*                    Generate Q in VT
    	      	*                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
    	      	*/
    	      	                     dorglq( N, N, M, VT, LDVT, WORK2, WORK3, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy L to WORK(IU), zeroing out above it
    	      	*/
    	      	                     ARRAY = new double[LDWRKU][M];
    	      	                     dlacpy( 'L', M, M, A, LDA, ARRAY, LDWRKU );
    	      	                     ARRAY2 = new double[LDWRKU][M-1];
    	      	                     for (i = 0; i < LDWRKU; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY2[i][j] = ARRAY[i][j+1];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY2, LDWRKU );
    	      	                     for (i = 0; i < LDWRKU; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY[i][j+1] = ARRAY2[i][j];
    	      	                    	 }
    	      	                     }
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + M;
    	      	                     ITAUP = ITAUQ + M;
    	      	                     IWORK = ITAUP + M;
    	      	/*
    	      	*                    Bidiagonalize L in WORK(IU), copying result to U
    	      	*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
    	      	*/
    	      	                     WORK2 = new double[M-1];
    	      	                     WORK3 = new double[M];
    	      	                     WORK4 = new double[M];
    	      	                     WORK5 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( M, M, ARRAY, LDWRKU, S, WORK2, WORK3,
    	      	                             WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'L', M, M, ARRAY, LDWRKU, U, LDU );
    	      	/*
    	      	*                    Generate right bidiagonalizing vectors in WORK(IU)
    	      	*                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
    	      	*/
    	      	                     dorgbr( 'P', M, M, M, ARRAY, LDWRKU,
    	      	                             WORK4, WORK5, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors in U
    	      	*                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
    	      	*/
    	      	                     dorgbr( 'Q', M, M, M, U, LDU, WORK3, WORK5,
    	      	                    		 LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + M;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of L in U and computing right
    	      	*                    singular vectors of L in WORK(IU)
    	      	*                    (Workspace: need M*M+BDSPAC)
    	      	*/
    	      	                     WORK5 = new double[4*M];
    	      	                     dbdsqr( 'U', M, M, M, 0, S, WORK2,
    	      	                             ARRAY, LDWRKU, U, LDU, DUM2, 1, WORK5, INFO );
    	      	/*
    	      	*                    Multiply right singular vectors of L in WORK(IU) by
    	      	*                    Q in VT, storing result in A
    	      	*                    (Workspace: need M*M)
    	      	*/
    	      	                     dgemm( 'N', 'N', M, N, M, 1.0, ARRAY,
    	      	                            LDWRKU, VT, LDVT, 0.0, A, LDA );
    	      	/*
    	      	*                    Copy right singular vectors of A from A to VT
    	      	*/
    	      	                     dlacpy( 'F', M, N, A, LDA, VT, LDVT );
    	      	                     k = 0;
    	      	                     for (j  = 0; j < M; j++) {
    	      	                    	 for (i = 0; i < LDWRKU; i++) {
    	      	                    		 WORK[k++] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                  }
    	      	                  else {
    	      	/*
    	      	*                    Insufficient workspace for a fast algorithm
    	      	*/
    	      	                     ITAU = 1;
    	      	                     IWORK = ITAU + M;
    	      	/*
    	      	*                    Compute A=L*Q, copying result to VT
    	      	*                    (Workspace: need 2*M, prefer M+M*NB)
    	      	*/
    	      	                     WORK2 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgelqf( M, N, A, LDA, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	                     dlacpy( 'U', M, N, A, LDA, VT, LDVT );
    	      	/*
    	      	*                    Generate Q in VT
    	      	*                    (Workspace: need M+N, prefer M+N*NB)
    	      	*/
    	      	                     dorglq( N, N, M, VT, LDVT, WORK, WORK2, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Copy L to U, zeroing out above it
    	      	*/
    	      	                     dlacpy( 'L', M, M, A, LDA, U, LDU );
    	      	                     IROW = Math.max(1,M-1);
    	      	                     ARRAY = new double[IROW][M-1];
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 ARRAY[i][j] = U[i][j+1];
    	      	                    	 }
    	      	                     }
    	      	                     dlaset( 'U', M-1, M-1, 0.0, 0.0, ARRAY, IROW );
    	      	                     for (i = 0; i < IROW; i++) {
    	      	                    	 for (j = 0; j < M-1; j++) {
    	      	                    		 U[i][j+1] = ARRAY[i][j];
    	      	                    	 }
    	      	                     }
    	      	                     IE = ITAU;
    	      	                     ITAUQ = IE + M;
    	      	                     ITAUP = ITAUQ + M;
    	      	                     IWORK = ITAUP + M;
    	      	/*
    	      	*                    Bidiagonalize L in U
    	      	*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
    	      	*/
    	      	                     WORK2 = new double[M];
    	      	                     WORK3 = new double[M];
    	      	                     WORK4 = new double[Math.max(1,LWORK-IWORK+1)];
    	      	                     dgebrd( M, M, U, LDU, S, WORK, WORK2, WORK3,
    	      	                             WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Multiply right bidiagonalizing vectors in U by Q
    	      	*                    in VT
    	      	*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
    	      	*/
    	      	                     dormbr( 'P', 'L', 'T', M, N, M, U, LDU,
    	      	                             WORK3, VT, LDVT, WORK4, LWORK-IWORK+1, IERR );
    	      	/*
    	      	*                    Generate left bidiagonalizing vectors in U
    	      	*                    (Workspace: need 4*M, prefer 3*M+M*NB)
    	      	*/
    	      	                     dorgbr( 'Q', M, M, M, U, LDU, WORK2, WORK4, LWORK-IWORK+1, IERR );
    	      	                     IWORK = IE + M;
    	      	/*
    	      	*                    Perform bidiagonal QR iteration, computing left
    	      	*                    singular vectors of A in U and computing right
    	      	*                    singular vectors of A in VT
    	      	*                    (Workspace: need BDSPAC)
    	      	*/
    	      	                     WORK4 = new double[4*M];
    	      	                     dbdsqr( 'U', M, N, M, 0, S, WORK, VT,
    	      	                             LDVT, U, LDU, DUM2, 1, WORK4, INFO );
    	      	                  }
    	      	               }
    	      	            }
    	      	         }
    	      	         else {
    	      	/*
    	      	*           N .LT. MNTHR
    	      	*
    	      	*           Path 10t(N greater than M, but not much larger)
    	      	*           Reduce to bidiagonal form without LQ decomposition
    	      	*/
    	      	            IE = 1;
    	      	            ITAUQ = IE + M;
    	      	            ITAUP = ITAUQ + M;
    	      	            IWORK = ITAUP + M;
    	      	/*
    	      	*           Bidiagonalize A
    	      	*           (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
    	      	*/
    	      	            WORK2 = new double[Math.min(M, N)];
    	      	            WORK3 = new double[Math.min(M, N)];
    	      	            WORK4 = new double[Math.max(1, LWORK-IWORK+1)];
    	      	            dgebrd( M, N, A, LDA, S, WORK, WORK2,
    	      	                    WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	            if ( WNTUAS ) {
    	      	/*
    	      	*              If left singular vectors desired in U, copy result to U
    	      	*              and generate left bidiagonalizing vectors in U
    	      	*              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
    	      	*/
    	      	               dlacpy( 'L', M, M, A, LDA, U, LDU );
    	      	               dorgbr( 'Q', M, M, N, U, LDU, WORK2, WORK4, LWORK-IWORK+1, IERR );
    	      	            }
    	      	            if ( WNTVAS ) {
    	      	/*
    	      	*              If right singular vectors desired in VT, copy result to
    	      	*              VT and generate right bidiagonalizing vectors in VT
    	      	*              (Workspace: need 3*M+NRVT, prefer 3*M+NRVT*NB)
    	      	*/
    	      	               dlacpy( 'U', M, N, A, LDA, VT, LDVT );
    	      	               if ( WNTVA ) {
    	      	                  NRVT = N;
    	      	               }
    	      	               if ( WNTVS ) {
    	      	                  NRVT = M;
    	      	               }
    	      	               dorgbr( 'P', NRVT, N, M, VT, LDVT, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	            }
    	      	            if ( WNTUO ) {
    	      	/*
    	      	*              If left singular vectors desired in A, generate left
    	      	*              bidiagonalizing vectors in A
    	      	*              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
    	      	*/
    	      	               dorgbr( 'Q', M, M, N, A, LDA, WORK2, WORK4, LWORK-IWORK+1, IERR );
    	      	            }
    	      	            if ( WNTVO ) {
    	      	/*
    	      	*              If right singular vectors desired in A, generate right
    	      	*              bidiagonalizing vectors in A
    	      	*              (Workspace: need 4*M, prefer 3*M+M*NB)
    	      	*/
    	      	               dorgbr( 'P', M, N, M, A, LDA, WORK3, WORK4, LWORK-IWORK+1, IERR );
    	      	            }
    	      	            IWORK = IE + M;
    	      	            if ( WNTUAS || WNTUO ) {
    	      	               NRU = M;
    	      	            }
    	      	            if ( WNTUN ) {
    	      	               NRU = 0;
    	      	            }
    	      	            if ( WNTVAS || WNTVO ) {
    	      	               NCVT = N;
    	      	            }
    	      	            if ( WNTVN ) {
    	      	               NCVT = 0;
    	      	            }
    	      	            if ( ( !WNTUO ) && ( !WNTVO ) ) {
    	      	/*
    	      	*              Perform bidiagonal QR iteration, if desired, computing
    	      	*              left singular vectors in U and computing right singular
    	      	*              vectors in VT
    	      	*              (Workspace: need BDSPAC)
    	      	*/
    	      	               WORK = new double[4*M];
    	      	               dbdsqr( 'L', M, NCVT, NRU, 0, S, WORK, VT,
    	      	                       LDVT, U, LDU, DUM2, 1, WORK4, INFO );
    	      	            }
    	      	            else if( ( !WNTUO ) && WNTVO ) {
    	      	/*
    	      	*              Perform bidiagonal QR iteration, if desired, computing
    	      	*              left singular vectors in U and computing right singular
    	      	*              vectors in A
    	      	*              (Workspace: need BDSPAC)
    	      	*/
    	      	               WORK4 = new double[4*M];
    	      	               dbdsqr( 'L', M, NCVT, NRU, 0, S, WORK, A, LDA,
    	      	                       U, LDU, DUM2, 1, WORK4, INFO );
    	      	            }
    	      	            else {
    	      	/*
    	      	*              Perform bidiagonal QR iteration, if desired, computing
    	      	*              left singular vectors in A and computing right singular
    	      	*              vectors in VT
    	      	*              (Workspace: need BDSPAC)
    	      	*/
    	      	               WORK4 = new double[4*M];
    	      	               dbdsqr( 'L', M, NCVT, NRU, 0, S, WORK, VT,
    	      	                       LDVT, A, LDA, DUM2, 1, WORK4, INFO );
    	      	            }
    	      	         }
    	              }
    	/*     If DBDSQR failed to converge, copy unconverged superdiagonals
    	*     to WORK( 2:MINMN )
    	*
    	      IF( INFO.NE.0 ) THEN
    	         IF( IE.GT.2 ) THEN
    	            DO 50 I = 1, MINMN - 1
    	               WORK( I+1 ) = WORK( I+IE-1 )
    	   50       CONTINUE
    	         END IF
    	         IF( IE.LT.2 ) THEN
    	            DO 60 I = MINMN - 1, 1, -1
    	               WORK( I+1 ) = WORK( I+IE-1 )
    	   60       CONTINUE
    	         END IF
    	      END IF
    	*
    	*     Undo scaling if necessary
    	*
    	      IF( ISCL.EQ.1 ) THEN
    	         IF( ANRM.GT.BIGNUM )
    	     $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
    	     $                   IERR )
    	         IF( INFO.NE.0 .AND. ANRM.GT.BIGNUM )
    	     $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN-1, 1, WORK( 2 ),
    	     $                   MINMN, IERR )
    	         IF( ANRM.LT.SMLNUM )
    	     $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
    	     $                   IERR )
    	         IF( INFO.NE.0 .AND. ANRM.LT.SMLNUM )
    	     $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN-1, 1, WORK( 2 ),
    	     $                   MINMN, IERR )
    	      END IF
    	*
    	*     Return optimal workspace in WORK(1)
    	*
    	      WORK( 1 ) = MAXWRK
    	*/
    	      return;
    } // dgesvd
    	            
    /** This is a port of version 3.2 LAPACK routine DGELQF
     *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
     *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
     *     November 2006
     *
     *     .. Scalar Arguments ..
           INTEGER            INFO, LDA, LWORK, M, N
     *     ..
     *     .. Array Arguments ..
           DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
     *     ..
     *
     *  Purpose
     *  =======
     *
     *  DGELQF computes an LQ factorization of a real M-by-N matrix A:
     *  A = L * Q.
     *
     *  Arguments
     *  =========
     *
     *  M       (input) INTEGER
     *          The number of rows of the matrix A.  M >= 0.
     *
     *  N       (input) INTEGER
     *          The number of columns of the matrix A.  N >= 0.
     *
     *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
     *          On entry, the M-by-N matrix A.
     *          On exit, the elements on and below the diagonal of the array
     *          contain the m-by-min(m,n) lower trapezoidal matrix L (L is
     *          lower triangular if m <= n); the elements above the diagonal,
     *          with the array TAU, represent the orthogonal matrix Q as a
     *          product of elementary reflectors (see Further Details).
     *
     *  LDA     (input) INTEGER
     *          The leading dimension of the array A.  LDA >= max(1,M).
     *
     *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
     *          The scalar factors of the elementary reflectors (see Further
     *          Details).
     *
     *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
     *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
     *
     *  LWORK   (input) INTEGER
     *          The dimension of the array WORK.  LWORK >= max(1,M).
     *          For optimum performance LWORK >= M*NB, where NB is the
     *          optimal blocksize.
     *
     *          If LWORK = -1, then a workspace query is assumed; the routine
     *          only calculates the optimal size of the WORK array, returns
     *          this value as the first entry of the WORK array, and no error
     *          message related to LWORK is issued by XERBLA.
     *
     *  INFO    (output) INTEGER
     *          = 0:  successful exit
     *          < 0:  if INFO = -i, the i-th argument had an illegal value
     *
     *  Further Details
     *  ===============
     *
     *  The matrix Q is represented as a product of elementary reflectors
     *
     *     Q = H(k) . . . H(2) H(1), where k = min(m,n).
     *
     *  Each H(i) has the form
     *
     *     H(i) = I - tau * v * v'
     *
     *  where tau is a real scalar, and v is a real vector with
     *  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
     *  and tau in TAU(i).
     */
     private void dgelqf(int m, int n, double A[][], int lda, double tau[], double work[],
                         int lwork, int info[]) {
         boolean lquery;
         int i;
         int ib;
         int iinfo[] = new int[1];
         int iws;
         int k;
         int ldwork;
         int lwkopt;
         int nb;
         int nbmin;
         int nx;
         String name;
         String opts;
         int row1;
         double array1[][];
         int p;
         int q;
         double v1[];
         double work2[][];
         double array2[][];
         int row2;
         double work3[][];
         
         // Test the input arguments
         
         info[0] = 0;
         name = new String("DGELQF");
         opts = new String(" ");
         nb = ilaenv(1, name, opts, m, n, -1, -1);
         lwkopt = m*nb;
         work[0] = lwkopt;
         lquery = (lwork == -1);
         if (m < 0) {
             info[0] = -1;
         }
         else if (n < 0) {
             info[0] = -2;
         }
         else if (lda < Math.max(1, m)) {
             info[0] = -4;
         }
         else if (lwork < Math.max(1, m) && (!lquery)) {
             info[0] = -7;
         }
         if (info[0] != 0) {
              MipavUtil.displayError("Error dgelqf had info[0] = " + info[0]);
              return;
         }
         else if (lquery) {
             return;
         }
         
         // Quick return if possible
         k = Math.min(m, n);
         if (k == 0) {
             work[0] = 1;
             return;
         }
         
         nbmin = 2;
         nx = 0;
         iws = m;
         if ((nb > 1) && (nb < k)) {
             // Determine when to cross over from blocked to unblocked code.
             nx = Math.max(0, ilaenv(3, name, opts, m, n, -1, -1));
             if (nx < k) {
                 // Determine if workspace is large enough for blocked code.
                 ldwork = m;
                 iws = ldwork * nb;
                 if (lwork < iws) {
                     //  Not enough workspace to used optimal nb:  reduce nb and
                     // determine the minimum value of nb;
                     nb = lwork/ldwork;
                     nbmin = Math.max(2, ilaenv(2, name, opts, m, n, -1, -1));
                 } // if (lwork < iws)
             } // if (nx < k)
         } // if ((nb > 1) && (nb < k))
         
         if ((nb >= nbmin) && (nb < k) && (nx < k)) {
             // Use blocked code initially
             for (i = 1; i <= k - nx; i += nb) {
                 ib = Math.min(k-i+1, nb);
                 // Compute the LQ factorization of the current block A(i:i+ib-1,i:n)
                 row1 = Math.max(1, ib);
                 array1 = new double[row1][n-i+1];
                 for (p = 0; p < row1; p++) {
                     for (q = 0; q < n-i+1; q++) {
                         array1[p][q] = A[i-1+p][i-1+q];    
                     }
                 }
                 v1 = new double[Math.min(ib, n-i+1)];
                 dgelq2(ib, n-i+1, array1, row1, v1, work, iinfo);
                 for (p = 0; p < row1; p++) {
                     for (q = 0; q < n-i+1; q++) {
                         A[i-1+p][i-1+q] = array1[p][q];    
                     }
                 }
                 for (p = 0; p < Math.min(ib, n-i+1); p++) {
                     tau[i-1+p] = v1[p];
                 }
                 if (i+ib <= m) {
                     // Form the triangular factor of the block reflector
                     // H = H(i) H(i+1) ... H(i+ib-1)
                     v1 = new double[ib];
                     for (p = 0; p < ib; p++) {
                         v1[p] = tau[i-1+p];
                     }
                     work2 = new double[ib][ib];
                     dlarft('F', 'R', n-i+1, ib, array1, ib, v1, work2, ib);
                     for (p = 0; p < ib; p++) {
                         for (q = 0; q < n-i+1; q++) {
                             A[i-1+p][i-1+q] = array1[p][q];    
                         }
                     }
                     for (q = 0; q < ib; q++) {
                         for (p = 0; p < ib; p++) {
                             work[p + ib*q] = work2[p][q];
                         }
                     }
                     
                     // Apply H to A(i+ib:m, i:n) from the right
                     row2 = Math.max(1,m-i-ib+1);
                     array2 = new double[row2][n-i+1];
                     for (p = 0; p < row2; p++) {
                         for (q = 0; q < n-i+1; q++) {
                             array2[p][q] = A[i+ib-1+p][i-1+q];
                         }
                     }
                     work3 = new double[row2][ib];
                     dlarfb('R', 'N', 'F', 'R', m-i-ib+1, n-i+1, ib, array1, ib, work2, ib,
                             array2, row2, work3, row2);
                     for (p = 0; p < row2; p++) {
                         for (q = 0; q < n-i+1; q++) {
                             A[i+ib-1+p][i-1+q] = array2[p][q];
                         }
                     }
                 } // if (i+ib <= m)
             } // for (i = 1; i <= k - nx; i+= nb)
         } // if ((nb >= nbmin) && (nb < k) && (nx < k))
         else {
             i = 1;
         }
         
         // Use unblocked code to factor the last or only block
         if (i <= k) {
             row1 = Math.max(1, m-i+1);
             array1 = new double[row1][n-i+1];
             for (p = 0; p < row1; p++) {
                 for (q = 0; q < n-i+1; q++) {
                     array1[p][q] = A[i-1+p][i-1+q];    
                 }
             }
             v1 = new double[Math.min(m-i+1, n-i+1)];
             dgelq2(m-i+1, n-i+1, array1, row1, v1, work, iinfo);
             for (p = 0; p < row1; p++) {
                 for (q = 0; q < n-i+1; q++) {
                     A[i-1+p][i-1+q] = array1[p][q];    
                 }
             }
             for (p = 0; p < Math.min(m-i+1, n-i+1); p++) {
                 tau[i-1+p] = v1[p];
             }
         } // if (i <= k)
         
         work[0] = iws;
         return;
     } // dgelqf

     
     /** This is a port of version 3.2 LAPACK routine DGELQ2
     *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
     *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
     *     November 2006
     *
     *     .. Scalar Arguments ..
           INTEGER            INFO, LDA, M, N
     *     ..
     *     .. Array Arguments ..
           DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
     *     ..
     *
     *  Purpose
     *  =======
     *
     *  DGELQ2 computes an LQ factorization of a real m by n matrix A:
     *  A = L * Q.
     *
     *  Arguments
     *  =========
     *
     *  M       (input) INTEGER
     *          The number of rows of the matrix A.  M >= 0.
     *
     *  N       (input) INTEGER
     *          The number of columns of the matrix A.  N >= 0.
     *
     *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
     *          On entry, the m by n matrix A.
     *          On exit, the elements on and below the diagonal of the array
     *          contain the m by min(m,n) lower trapezoidal matrix L (L is
     *          lower triangular if m <= n); the elements above the diagonal,
     *          with the array TAU, represent the orthogonal matrix Q as a
     *          product of elementary reflectors (see Further Details).
     *
     *  LDA     (input) INTEGER
     *          The leading dimension of the array A.  LDA >= max(1,M).
     *
     *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
     *          The scalar factors of the elementary reflectors (see Further
     *          Details).
     *
     *  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
     *
     *  INFO    (output) INTEGER
     *          = 0: successful exit
     *          < 0: if INFO = -i, the i-th argument had an illegal value
     *
     *  Further Details
     *  ===============
     *
     *  The matrix Q is represented as a product of elementary reflectors
     *
     *     Q = H(k) . . . H(2) H(1), where k = min(m,n).
     *
     *  Each H(i) has the form
     *
     *     H(i) = I - tau * v * v'
     *
     *  where tau is a real scalar, and v is a real vector with
     *  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
     *  and tau in TAU(i).
     */
     private void dgelq2(int m, int n, double A[][], int lda, double tau[], double work[], int info[]) {
         int i;
         int k;
         double aii;
         double v1[] = new double[1];
         double v2[];
         double v3[] = new double[1];
         double array1[][];
         int j;
         int p;
         
         // Test the input arguments
         info[0] = 0;
         if (m < 0) {
             info[0] = -1;
         }
         else if (n < 0) {
             info[0] = -2;
         }
         else if (lda < Math.max(1, m)) {
             info[0] = -4;
         }
         if (info[0] != 0) {
             MipavUtil.displayError("Error dgelq2 had info[0] = " + info[0]);
             return;
         }
         
         k = Math.min(m, n);
         for (i = 1; i <= k; i++) {
             // Generate elementary reflector H(i) to annihilate A(i,i+1:n)
             v1[0] = A[i-1][i-1];
             v2 = new double[n-i];
             for (j = 0; j < n-i; j++) {
                 v2[j] = A[i-1][Math.min(i,n-1)+j];
             }
             dlarfp(n-i+1, v1, v2, 1, v3);
             A[i-1][i-1] = v1[0];
             for (j = 0; j < n-i; j++) {
                 A[i-1][Math.min(i,n-1)+j] = v2[j];
             }
             tau[i-1] = v3[0];
             if (i < m) {
                 // Apply H(i) to A(i+1:m,i:n) from the right
                 aii = A[i-1][i-1];
                 A[i-1][i-1] = 1.0;
                 v2 = new double[n-i+1];
                 for (j = 0; j < n-i+1; j++) {
                     v2[j] = A[i-1][i-1+j];
                 }
                 array1 = new double[m-i][n-i+1];
                 for (j = 0; j < m-i; j++) {
                     for (p = 0; p < n-i+1; p++) {
                         array1[j][p] = A[i+j][i-1+p];
                     }
                 }
                 dlarf('R', m-i, n-i+1, v2, 1, v3[0], array1, m-i, work);
                 for (j = 0; j < m-i; j++) {
                     for (p = 0; p < n-i+1; p++) {
                         A[i+j][i-1+p] = array1[j][p];
                     }
                 }
                 A[i-1][i-1] = aii;
             } // if (i < m)
         } // for (i = 1; i <= k; i++)
         return;
     } // dgelq2
    	               
       /* This is a port of version 3.2 LAPACK routine dormbr.  Original DORMBR created by Univ. of Tennessee,
        * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006.
          *  Purpose
          *  =======
          *
          *  If VECT = 'Q', DORMBR overwrites the general real M-by-N matrix C
          *  with
          *                  SIDE = 'L'     SIDE = 'R'
          *  TRANS = 'N':      Q * C          C * Q
          *  TRANS = 'T':      Q**T * C       C * Q**T
          *
          *  If VECT = 'P', DORMBR overwrites the general real M-by-N matrix C
          *  with
          *                  SIDE = 'L'     SIDE = 'R'
          *  TRANS = 'N':      P * C          C * P
          *  TRANS = 'T':      P**T * C       C * P**T
          *
          *  Here Q and P**T are the orthogonal matrices determined by DGEBRD when
          *  reducing a real matrix A to bidiagonal form: A = Q * B * P**T. Q and
          *  P**T are defined as products of elementary reflectors H(i) and G(i)
          *  respectively.
          *
          *  Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
          *  order of the orthogonal matrix Q or P**T that is applied.
          *
          *  If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
          *  if nq >= k, Q = H(1) H(2) . . . H(k);
          *  if nq < k, Q = H(1) H(2) . . . H(nq-1).
          *
          *  If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
          *  if k < nq, P = G(1) G(2) . . . G(k);
          *  if k >= nq, P = G(1) G(2) . . . G(nq-1).
          *
          *  Arguments
          *  =========
          *
          *  VECT    (input) CHARACTER*1
          *          = 'Q': apply Q or Q**T;
          *          = 'P': apply P or P**T.
          *
          *  SIDE    (input) CHARACTER*1
          *          = 'L': apply Q, Q**T, P or P**T from the Left;
          *          = 'R': apply Q, Q**T, P or P**T from the Right.
          *
          *  TRANS   (input) CHARACTER*1
          *          = 'N':  No transpose, apply Q  or P;
          *          = 'T':  Transpose, apply Q**T or P**T.
          *
          *  M       (input) INTEGER
          *          The number of rows of the matrix C. M >= 0.
          *
          *  N       (input) INTEGER
          *          The number of columns of the matrix C. N >= 0.
          *
          *  K       (input) INTEGER
          *          If VECT = 'Q', the number of columns in the original
          *          matrix reduced by DGEBRD.
          *          If VECT = 'P', the number of rows in the original
          *          matrix reduced by DGEBRD.
          *          K >= 0.
          *
          *  A       (input) DOUBLE PRECISION array, dimension
          *                                (LDA,min(nq,K)) if VECT = 'Q'
          *                                (LDA,nq)        if VECT = 'P'
          *          The vectors which define the elementary reflectors H(i) and
          *          G(i), whose products determine the matrices Q and P, as
          *          returned by DGEBRD.
          *
          *  LDA     (input) INTEGER
          *          The leading dimension of the array A.
          *          If VECT = 'Q', LDA >= max(1,nq);
          *          if VECT = 'P', LDA >= max(1,min(nq,K)).
          *
          *  TAU     (input) DOUBLE PRECISION array, dimension (min(nq,K))
          *          TAU(i) must contain the scalar factor of the elementary
          *          reflector H(i) or G(i) which determines Q or P, as returned
          *          by DGEBRD in the array argument TAUQ or TAUP.
          *
          *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
          *          On entry, the M-by-N matrix C.
          *          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q
          *          or P*C or P**T*C or C*P or C*P**T.
          *
          *  LDC     (input) INTEGER
          *          The leading dimension of the array C. LDC >= max(1,M).
          *
          *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
          *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
          *
          *  LWORK   (input) INTEGER
          *          The dimension of the array WORK.
          *          If SIDE = 'L', LWORK >= max(1,N);
          *          if SIDE = 'R', LWORK >= max(1,M).
          *          For optimum performance LWORK >= N*NB if SIDE = 'L', and
          *          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
          *          blocksize.
          *
          *          If LWORK = -1, then a workspace query is assumed; the routine
          *          only calculates the optimal size of the WORK array, returns
          *          this value as the first entry of the WORK array, and no error
          *          message related to LWORK is issued by XERBLA.
          *
          *  INFO    (output) INTEGER
          *          = 0:  successful exit
          *          < 0:  if INFO = -i, the i-th argument had an illegal value
          */
       private void dormbr(char vect, char side, char trans, int m, int n, int k, double A[][],
                           int lda, double tau[], double C[][], int ldc, double work[], int lwork,
                           int info[]) {
           boolean applyq;
           boolean left;
           boolean lquery;
           boolean notran;
           char transt;
           int i1;
           int i2;
           int iinfo[] = new int[1];
           int lwkopt = 0;
           int mi;
           int nb;
           int ni;
           int nq;
           int nw;
           String name = null;
           String opts = null;
           char[] optsC = new char[2];
           int row1;
           int col1;
           double array1[][];
           int row2;
           double array2[][];
           int i;
           int j;
           
           // Test the arguments
           info[0] = 0;
           applyq = ((vect == 'Q') || (vect == 'q'));
           left = ((side == 'L') || (side == 'l'));
           notran = ((trans == 'N') || (trans == 'n'));
           lquery = (lwork == -1);
           
           // nq is the order of Q or P and nw is the minimum dimension of work
           
           if (left) {
               nq = m;
               nw = n;
           }
           else {
               nq = n;
               nw = m;
           }
           if ((!applyq) && (vect != 'P') && (vect != 'p')) {
               info[0] = -1;
           }
           else if ((!left) && (side !='R') && (side != 'r')) {
               info[0] = -2;
           }
           else if ((!notran) && (trans != 'T') && (trans != 't')) {
               info[0] = -3;
           }
           else if (m < 0) {
               info[0] = -4;
           }
           else if (n < 0) {
               info[0] = -5;
           }
           else if (k < 0) {
               info[0] = -6;
           }
           else if ((applyq && (lda < Math.max(1, nq))) || ((!applyq) && (lda < Math.max(1, Math.min(nq, k))))) {
               info[0] = -8;
           }
           else if (ldc < Math.max(1, m)) {
               info[0] = -11;
           }
           else if ((lwork < Math.max(1, nw)) && (!lquery)) {
               info[0] = -13;
           }
           
           optsC[0] = side;
           optsC[1] = trans;
           opts = new String(optsC);
           if (info[0] == 0) {
               if (applyq) {
                   name = new String("DORMQR");
                   if (left) {
                       nb = ilaenv(1, name, opts, m - 1, n, m - 1, -1);    
                   } // if (left)
                   else {
                       nb = ilaenv(1, name, opts, m, n - 1, n - 1, -1);
                   }
               } // if (applyq)
               else { // !applyq
                   name = new String("DORMLQ");
                   if (left) {
                       nb = ilaenv(1, name, opts, m - 1, n, m - 1, -1);
                   } // if (left)
                   else {
                       nb = ilaenv(1, name, opts, m, n - 1, n - 1, -1);
                   }
               } // else !applyq
               lwkopt = Math.max(1, nw) * nb;
               work[0] = lwkopt;
           } // if (info[0] == 0)
           
           if (info[0] != 0) {
               MipavUtil.displayError("Error dormbr had info[0] = " + info[0]);
               return;
           }
           else if (lquery) {
               return;
           }
           
           // Quick return if possible
           work[0] = 1;
           if ((m == 0) || (n == 0)) {
               return;
           }
           
           if (applyq) {
               // Apply Q
               if (nq >= k) {
                   // Q was determined by a call to dgebrd with nq >= k
                   dormqr(side, trans, m, n, k, A, lda, tau, C, ldc, work, lwork, iinfo);
               } // if (nq >= k)
               else if (nq > 1) {
                   // Q was determined by a call to dgebrd with nq < k
                   if (left) {
                       mi = m - 1;
                       ni = n;
                       i1 = 2;
                       i2 = 1;
                   } // if (left)
                   else {
                       mi = m;
                       ni = n - 1;
                       i1 = 1;
                       i2 = 2;
                   }
                   if (left) {
                       row1 = Math.max(1, mi);
                   }
                   else {
                       row1 = Math.max(1, ni);
                   }
                   array1 = new double[row1][nq-1];
                   for (i = 0; i < row1; i++) {
                       for (j = 0; j < nq - 1; j++) {
                           array1[i][j] = A[1 + i][j];
                       }
                   }
                   row2 = Math.max(1, mi);
                   array2 = new double[row2][ni];
                   for (i = 0; i < row2; i++) {
                       for (j = 0; j < ni; j++) {
                           array2[i][j] = C[i1-1+i][i2-1+j];
                       }
                   }
                   dormqr(side, trans, mi, ni, nq - 1, array1, row1, tau, array2, row2, work, lwork, iinfo);
                   for (i = 0; i < row2; i++) {
                       for (j = 0; j < ni; j++) {
                           C[i1-1+i][i2-1+j] = array2[i][j];
                       }
                   }
               } // else if (nq > 1)
           } // if (applyq)
           else { // !applyq
               // Apply P
               
               if (notran) {
                   transt = 'T';
               }
               else {
                   transt = 'N';
               }
               if (nq > k) {
                   // P was determined by a call to dgebrd with nq > k
                   dormlq(side, transt, m, n, k, A, lda, tau, C, ldc, work, lwork, iinfo);
               } // if (nq > k)
               else if (nq > 1) {
                   // P was determined by a call to dgebrd with nq <= k
                   
                   if (left) {
                       mi = m - 1;
                       ni = n;
                       i1 = 2;
                       i2 = 1;
                   } // if (left)
                   else {
                       mi = m;
                       ni = n - 1;
                       i1 = 1;
                       i2 = 2;
                   }
                   row1 = Math.max(1, nq - 1);
                   if (left) {
                       col1 = mi;
                   }
                   else {
                       col1 = ni;
                   }
                   array1 = new double[row1][col1];
                   for (i = 0; i < row1; i++) {
                       for (j = 0; j < col1; j++) {
                           array1[i][j] = A[i][1 + j];
                       }
                   }
                   row2 = Math.max(1, mi);
                   array2 = new double[row2][ni];
                   for (i = 0; i < row2; i++) {
                       for (j = 0; j < ni; j++) {
                           array2[i][j] = C[i1-1+i][i2-1+j];
                       }
                   }
                   dormlq(side, transt, mi, ni, nq - 1, array1, row1, tau, array2, row2, work, lwork, iinfo);
                   for (i = 0; i < row2; i++) {
                       for (j = 0; j < ni; j++) {
                           C[i1-1+i][i2-1+j] = array2[i][j];
                       }
                   }
               } // else if (nq > 1)
           } // !applyq
           work[0] = lwkopt;
           return;
       } // dormbr
       
       /**
        * This is a port of version 3.2 LAPACK routine DORMQR Original DORMQR created by Univ. of Tennessee, Univ. of
        * California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006 
        * dormqr overwrites the general real m by n matrix C with 
        *                           side = 'L'          side = 'R' 
        *           trans = 'N':      Q * C               C * Q 
        *           trans = 'T':      Q**T * C            C * Q**T 
        * where Q is a real orthogonal matrix defined as the product of k elementary reflectors 
        *                    Q = H(0) H(1) . . . H(k-1)
        * as returned by dgeqrf. Q is of order m if side = 'L' and of order n if side = 'R'.
        *
        * @param  side   input char 
        *                = 'L': apply Q or Q**T from the left 
        *                = 'R': apply Q or Q**T from the right
        * @param  trans  trans char 
        *                = 'N': No transpose, apply Q 
        *                = 'T': Transpose, apply Q**T
        * @param  m      input int The number of rows of matrix C. m >= 0.
        * @param  n      input int The number of columns of matrix C. n >= 0.
        * @param  k      input int The number of elementary reflectors whose product defines the matrix Q. 
        *                If side = 'L', m >= k >= 0 
        *                If side = 'R', n >= k >= 0
        * @param  A      input double[][] of dimension (lda,k) The i-th column must contain the vector which defines the
        *                elementary reflector H(i), for i = 0,1,...,k-1, as returned by dgeqrf in the first k columns of its
        *                array argument A. A is modified by the routine but restored on exit.
        * @param  lda    input int The leading dimension of the array A. 
        *                If side = 'L', lda >= max(1,m) 
        *                If side = 'R', lda >= max(1,n)
        * @param  tau    input double[] of dimension k tau[i] must contain the scalar factor of the elementary reflector
        *                H(i), as returned by dgeqrf
        * @param  C      (input/output) double[][] of dimension (ldc,n) 
        *                On entry, the m by n matrix C. 
        *                On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
        * @param  ldc    input int The leading dimension of the array C. ldc >= max(1,m).
        * @param  work   (workspace/output) double[] of dimension max(1, lwork). On exit, if info[0] = 0, work[0] returns the
        *                optimal lwork.
        * @param  lwork  input int The dimension of the array work. 
        *                If side = 'L', work >= max(1,n). 
        *                If side = 'R', work >= max(1,m). 
        *                For optimum performance lwork >= n*nb if side = 'L', and lwork >= m*nb if side = 'R',
        *                where nb is optimal blocksize. If lwork = -1, then a workspace query is assumed; the routine only
        *                calculates the optimal size of the work array, returns this value as the first entry of the work
        *                array, and no error message related to lwork is output.
        * @param  info   output int[] 
        *                = 0: successful exit 
        *                < 0: If info[0] = -i, the i-th argument had an illegal value
        */
       private void dormqr(char side, char trans, int m, int n, int k, double[][] A, int lda, double[] tau, double[][] C,
                           int ldc, double[] work, int lwork, int[] info) {
           int nbmax = 64;
           int ldt = nbmax + 1;
           boolean left;
           boolean lquery;
           boolean notran;
           int i;
           int i1;
           int i2;
           int i3;
           int ib;
           int ic = 1;
           int[] iinfo = new int[1];
           int iws;
           int jc = 1;
           int ldwork;
           int lwkopt = 1;
           int mi = 1;
           int nb = 1;
           int nbmin;
           int ni = 1;
           int nq;
           int nw;
           double[][] T = new double[ldt][nbmax];
           String name = null;
           String opts = null;
           char[] optsC = new char[2];
           double[][] array1;
           int p;
           int q;
           int row1;
           int row2;
           double[] x;
           double[][] array2;
           double[][] work2d;

           // Test the input arguments
           info[0] = 0;

           left = ((side == 'L') || (side == 'l'));
           notran = ((trans == 'N') || (trans == 'n'));
           lquery = (lwork == -1);

           // nq is the order of Q and nw is the minimum dimension of work
           if (left) {
               nq = m;
               nw = n;
           } else {
               nq = n;
               nw = m;
           }

           if ((!left) && (side != 'R') && (side != 'r')) {
               info[0] = -1;
           } else if ((!notran) && (trans != 'T') && (trans != 't')) {
               info[0] = -2;
           } else if (m < 0) {
               info[0] = -3;
           } else if (n < 0) {
               info[0] = -4;
           } else if ((k < 0) || (k > nq)) {
               info[0] = -5;
           } else if (lda < Math.max(1, nq)) {
               info[0] = -7;
           } else if (ldc < Math.max(1, m)) {
               info[0] = -10;
           } else if ((lwork < Math.max(1, nw)) && (!lquery)) {
               info[0] = -12;
           }

           if (info[0] == 0) {

               // Determine the block size.  nb may be at most nbmax, where nbmax
               // is used to define the local array T.
               name = new String("DORMQR");
               optsC[0] = side;
               optsC[1] = trans;
               opts = new String(optsC);
               nb = Math.min(nbmax, ilaenv(1, name, opts, m, n, k, -1));
               lwkopt = Math.max(1, nw) * nb;
               work[0] = lwkopt;
           } // if (info[0] == 0)

           if (info[0] != 0) {
               MipavUtil.displayError("Error dormqr had info[0] = " + info[0]);

               return;
           } else if (lquery) {
               return;
           }

           // Quick return if possible
           if ((m == 0) || (n == 0) || (k == 0)) {
               work[0] = 1;

               return;
           }

           nbmin = 2;
           ldwork = nw;

           if ((nb > 1) && (nb < k)) {
               iws = nw * nb;

               if (lwork < iws) {
                   nb = lwork / ldwork;
                   nbmin = Math.max(2, ilaenv(2, name, opts, m, n, k, -1));
               } // if (lwork < iws)
           } // if ((nb > 1) && (nb < k))
           else {
               iws = nw;
           }

           if ((nb < nbmin) || (nb >= k)) {

               // use unblocked code
               dorm2r(side, trans, m, n, k, A, lda, tau, C, ldc, work, iinfo);
           } // if ((nb < nbmin) || (nb >= k))
           else {

               // Use blocked code
               if ((left && (!notran)) || ((!left) && notran)) {
                   i1 = 1;
                   i2 = k;
                   i3 = nb;
               } else {
                   i1 = (((k - 1) / nb) * nb) + 1;
                   i2 = 1;
                   i3 = -nb;
               }

               if (left) {
                   ni = n;
                   jc = 1;
               } else {
                   mi = m;
                   ic = 1;
               }

               if (i3 == nb) {

                   for (i = i1; i <= i2; i += nb) {
                       ib = Math.min(nb, k - i + 1);

                       // Form the triangular factor of the block reflector
                       // H = H(i-1) H(i) .  .  .  H(i+ib-2)
                       row1 = Math.max(1, nq - i + 1);
                       array1 = new double[row1][ib];

                       for (p = 0; p < row1; p++) {

                           for (q = 0; q < ib; q++) {
                               array1[p][q] = A[i - 1 + p][i - 1 + q];
                           }
                       }

                       x = new double[ib];

                       for (p = 0; p < ib; p++) {
                           x[p] = tau[i - 1 + p];
                       }

                       dlarft('F', 'C', nq - i + 1, ib, array1, row1, x, T, ldt);

                       for (p = 0; p < row1; p++) {

                           for (q = 0; q < ib; q++) {
                               A[i - 1 + p][i - 1 + q] = array1[p][q];
                           }
                       }

                       if (left) {

                           // H or H' is applied to C(i-1:m-1,0:n-1)
                           mi = m - i + 1;
                           ic = i;
                       } else {

                           // H or H' is applied to C(0:m-1,i-1:n-1)
                           ni = n - i + 1;
                           jc = i;
                       }

                       // Apply H or H'
                       if (left) {
                           row1 = Math.max(1, mi);
                       } else {
                           row1 = Math.max(1, ni);
                       }

                       array1 = new double[row1][ib];

                       for (p = 0; p < row1; p++) {

                           for (q = 0; q < ib; q++) {
                               array1[p][q] = A[i - 1 + p][i - 1 + q];
                           }
                       }

                       row2 = Math.max(1, mi);
                       array2 = new double[row2][ni];

                       for (p = 0; p < row2; p++) {

                           for (q = 0; q < ni; q++) {
                               array2[p][q] = C[ic - 1 + p][jc - 1 + q];
                           }
                       }

                       work2d = new double[ldwork][ib];
                       dlarfb(side, trans, 'F', 'C', mi, ni, ib, array1, row1, T, ldt, array2, row2, work2d, ldwork);

                       for (p = 0; p < row2; p++) {

                           for (q = 0; q < ni; q++) {
                               C[ic - 1 + p][jc - 1 + q] = array2[p][q];
                           }
                       }
                   } // for (i = i1; i <= i2; i += nb)
               } // if (i3 == nb)
               else { // i3 == -nb

                   for (i = i1; i >= i2; i -= nb) {
                       ib = Math.min(nb, k - i + 1);

                       // Form the triangular factor of the block reflector
                       // H = H(i-1) H(i) .  .  .  H(i+ib-2)
                       row1 = Math.max(1, nq - i + 1);
                       array1 = new double[row1][ib];

                       for (p = 0; p < row1; p++) {

                           for (q = 0; q < ib; q++) {
                               array1[p][q] = A[i - 1 + p][i - 1 + q];
                           }
                       }

                       x = new double[ib];

                       for (p = 0; p < ib; p++) {
                           x[p] = tau[i - 1 + p];
                       }

                       dlarft('F', 'C', nq - i + 1, ib, array1, row1, x, T, ldt);

                       for (p = 0; p < row1; p++) {

                           for (q = 0; q < ib; q++) {
                               A[i - 1 + p][i - 1 + q] = array1[p][q];
                           }
                       }

                       if (left) {

                           // H or H' is applied to C(i-1:m-1,0:n-1)
                           mi = m - i + 1;
                           ic = i;
                       } else {

                           // H or H' is applied to C(0:m-1,i-1:n-1)
                           ni = n - i + 1;
                           jc = i;
                       }

                       // Apply H or H'
                       if (left) {
                           row1 = Math.max(1, mi);
                       } else {
                           row1 = Math.max(1, ni);
                       }

                       array1 = new double[row1][ib];

                       for (p = 0; p < row1; p++) {

                           for (q = 0; q < ib; q++) {
                               array1[p][q] = A[i - 1 + p][i - 1 + q];
                           }
                       }

                       row2 = Math.max(1, mi);
                       array2 = new double[row2][ni];

                       for (p = 0; p < row2; p++) {

                           for (q = 0; q < ni; q++) {
                               array2[p][q] = C[ic - 1 + p][jc - 1 + q];
                           }
                       }

                       work2d = new double[ldwork][ib];
                       dlarfb(side, trans, 'F', 'C', mi, ni, ib, array1, row1, T, ldt, array2, row2, work2d, ldwork);

                       for (p = 0; p < row2; p++) {

                           for (q = 0; q < ni; q++) {
                               C[ic - 1 + p][jc - 1 + q] = array2[p][q];
                           }
                       }
                   } // for (i = i1; i >= i2; i -= nb)
               } // else i3 == -nb
           }

           work[0] = lwkopt;

           return;
       } // dormqr
       
       /**
        * This is a port of the version 3.2 LAPACK routine DORM2R Original DORM2R created by Univ. of Tennessee, Univ. of
        * California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
        * dorm2r overwrites the general real m by n matrix C with 
        *     Q * C if side = 'L' and trans = 'N', or 
        *     Q'* C if side = 'L' and trans = 'T', or 
        *     C * Q if side = 'R' and trans = 'N', or 
        *     C * Q' if side = 'R' and trans = 'T',
        * where Q is a real orthogonal matrix defined as the product of k elementary reflectors Q = H(0) H(1) . . . H(k-1)
        * as returned by dgeqrf. Q is of order m if side = 'L' and of order n if side = 'R'.
        *
        * @param  side   input char 
        *                = 'L': apply Q or Q' from the left 
        *                = 'R': apply Q or Q' from the right
        * @param  trans  input char 
        *                = 'N': apply Q (no transpose) 
        *                = 'T': apply Q' (transpose)
        * @param  m      input int The number of rows of the matrix C. m >= 0.
        * @param  n      input int The number of columns of the matrix C. n >= 0.
        * @param  k      input int The number of elementary reflectors whose product defines the matrix Q. 
        *                If side = 'L', m >= k >= 0 
        *                If side = 'R', n >= k >= 0
        * @param  A      input double[][] of dimension (lda, k) The i-th column must contain the vector which defines the
        *                elementary reflector H(i), for i = 0,1,...,k-1, as returned by dgeqrf in the first k columns of
        *                its array argument A. A is modified by the routine but restored on exit.
        * @param  lda    input int The leading dimension of the array A. 
        *                If side = 'L', lda >= max(1,m) 
        *                If side = 'R', lda >= max(1,n)
        * @param  tau    input double[] of dimension (k) tau[i] must contain the scalar factor of the elementary
        *                reflector H(i), as returned by dgeqrf.
        * @param  C      (input/output) double[][] of dimension (ldc,n) On entry, the m by n matrix C. On exit, C is
        *                overwritten by Q*C or Q'*C or C*Q' or C*Q.
        * @param  ldc    input int The leading dimension of the array C. ldc >= max(1,m).
        * @param  work   (workspace) double[] of dimension 
        *                (n) if side = 'L' 
        *                (m) if side = 'R'
        * @param  info   output int[] 
        *                = 0: successful exit 
        *                < 0: If info[0] = -i, the i-th argument had an illegal value.
        */
       private void dorm2r(char side, char trans, int m, int n, int k, double[][] A, int lda, double[] tau, double[][] C,
                           int ldc, double[] work, int[] info) {
           boolean left;
           boolean notran;
           int i;
           int i1;
           int i2;
           int i3;
           int ic = 1;
           int jc = 1;
           int mi = 1;
           int ni = 1;
           int nq;
           double aii;
           int j;
           int p;
           double[] x;
           double[][] array1;
           int row1;

           // Test the input arguments
           info[0] = 0;
           left = ((side == 'L') || (side == 'l'));
           notran = ((trans == 'N') || (trans == 'n'));

           // nq is the order of Q
           if (left) {
               nq = m;
           } else {
               nq = n;
           }

           if ((!left) && (side != 'R') && (side != 'r')) {
               info[0] = -1;
           } else if ((!notran) && (trans != 'T') && (trans != 't')) {
               info[0] = -2;
           } else if (m < 0) {
               info[0] = -3;
           } else if (n < 0) {
               info[0] = -4;
           } else if ((k < 0) || (k > nq)) {
               info[0] = -5;
           } else if (lda < Math.max(1, nq)) {
               info[0] = -7;
           } else if (ldc < Math.max(1, m)) {
               info[0] = -10;
           }

           if (info[0] != 0) {
               MipavUtil.displayError("Error dorm2r had info[0] = " + info[0]);

               return;
           }

           // Quick return if possible
           if ((m == 0) || (n == 0) || (k == 0)) {
               return;
           }

           if ((left && (!notran)) || ((!left) && notran)) {
               i1 = 1;
               i2 = k;
               i3 = 1;
           } else {
               i1 = k;
               i2 = 1;
               i3 = -1;
           }

           if (left) {
               ni = n;
               jc = 1;
           } else {
               mi = m;
               ic = 1;
           }

           if (i3 == 1) {

               for (i = i1; i <= i2; i++) {

                   if (left) {

                       // H(i-1) is applied to C(i-1:m-1,0:n-1)
                       mi = m - i + 1;
                       ic = i;
                   } else {

                       // H(i-1) is applied to C(0:m-1,i-1:n-1)
                       ni = n - i + 1;
                       jc = i;
                   }

                   // Apply H(i-1)
                   aii = A[i - 1][i - 1];
                   A[i - 1][i - 1] = 1.0;

                   if (left) {
                       x = new double[mi];

                       for (j = 0; j < mi; j++) {
                           x[j] = A[i - 1 + j][i - 1];
                       }
                   } // if (left)
                   else {
                       x = new double[ni];

                       for (j = 0; j < ni; j++) {
                           x[j] = A[i - 1 + j][i - 1];
                       }
                   }

                   row1 = Math.max(1, mi);
                   array1 = new double[row1][ni];

                   for (j = 0; j < row1; j++) {

                       for (p = 0; p < ni; p++) {
                           array1[j][p] = C[ic - 1 + j][jc - 1 + p];
                       }
                   }

                   dlarf(side, mi, ni, x, 1, tau[i - 1], array1, row1, work);

                   for (j = 0; j < row1; j++) {

                       for (p = 0; p < ni; p++) {
                           C[ic - 1 + j][jc - 1 + p] = array1[j][p];
                       }
                   }

                   A[i - 1][i - 1] = aii;
               } // for (i = i1; i <= i2; i++)
           } // if (i3 == 1)
           else { // i3 == -1

               for (i = i1; i >= i2; i--) {

                   if (left) {

                       // H(i-1) is applied to C(i-1:m-1,0:n-1)
                       mi = m - i + 1;
                       ic = i;
                   } else {

                       // H(i-1) is applied to C(0:m-1,i-1:n-1)
                       ni = n - i + 1;
                       jc = i;
                   }

                   // Apply H(i-1)
                   aii = A[i - 1][i - 1];
                   A[i - 1][i - 1] = 1.0;

                   if (left) {
                       x = new double[mi];

                       for (j = 0; j < mi; j++) {
                           x[j] = A[i - 1 + j][i - 1];
                       }
                   } // if (left)
                   else {
                       x = new double[ni];

                       for (j = 0; j < ni; j++) {
                           x[j] = A[i - 1 + j][i - 1];
                       }
                   }

                   row1 = Math.max(1, mi);
                   array1 = new double[row1][ni];

                   for (j = 0; j < row1; j++) {

                       for (p = 0; p < ni; p++) {
                           array1[j][p] = C[ic - 1 + j][jc - 1 + p];
                       }
                   }

                   dlarf(side, mi, ni, x, 1, tau[i - 1], array1, row1, work);

                   for (j = 0; j < row1; j++) {

                       for (p = 0; p < ni; p++) {
                           C[ic - 1 + j][jc - 1 + p] = array1[j][p];
                       }
                   }

                   A[i - 1][i - 1] = aii;
               } // for (i = i1; i >= i2; i--)
           } // else i3 == -1

           return;
       } // dorm2r
       
       /*
        *  This is a port of version 3.2 LAPACK routine DORMLQ.  Original DORMLQ created by Univ. of Tennessee,
        *  Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
          *  Purpose
          *  =======
          *
          *  DORMLQ overwrites the general real M-by-N matrix C with
          *
          *                  SIDE = 'L'     SIDE = 'R'
          *  TRANS = 'N':      Q * C          C * Q
          *  TRANS = 'T':      Q**T * C       C * Q**T
          *
          *  where Q is a real orthogonal matrix defined as the product of k
          *  elementary reflectors
          *
          *        Q = H(k) . . . H(2) H(1)
          *
          *  as returned by DGELQF. Q is of order M if SIDE = 'L' and of order N
          *  if SIDE = 'R'.
          *
          *  Arguments
          *  =========
          *
          *  SIDE    (input) CHARACTER*1
          *          = 'L': apply Q or Q**T from the Left;
          *          = 'R': apply Q or Q**T from the Right.
          *
          *  TRANS   (input) CHARACTER*1
          *          = 'N':  No transpose, apply Q;
          *          = 'T':  Transpose, apply Q**T.
          *
          *  M       (input) INTEGER
          *          The number of rows of the matrix C. M >= 0.
          *
          *  N       (input) INTEGER
          *          The number of columns of the matrix C. N >= 0.
          *
          *  K       (input) INTEGER
          *          The number of elementary reflectors whose product defines
          *          the matrix Q.
          *          If SIDE = 'L', M >= K >= 0;
          *          if SIDE = 'R', N >= K >= 0.
          *
          *  A       (input) DOUBLE PRECISION array, dimension
          *                               (LDA,M) if SIDE = 'L',
          *                               (LDA,N) if SIDE = 'R'
          *          The i-th row must contain the vector which defines the
          *          elementary reflector H(i), for i = 1,2,...,k, as returned by
          *          DGELQF in the first k rows of its array argument A.
          *          A is modified by the routine but restored on exit.
          *
          *  LDA     (input) INTEGER
          *          The leading dimension of the array A. LDA >= max(1,K).
          *
          *  TAU     (input) DOUBLE PRECISION array, dimension (K)
          *          TAU(i) must contain the scalar factor of the elementary
          *          reflector H(i), as returned by DGELQF.
          *
          *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
          *          On entry, the M-by-N matrix C.
          *          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
          *
          *  LDC     (input) INTEGER
          *          The leading dimension of the array C. LDC >= max(1,M).
          *
          *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
          *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
          *
          *  LWORK   (input) INTEGER
          *          The dimension of the array WORK.
          *          If SIDE = 'L', LWORK >= max(1,N);
          *          if SIDE = 'R', LWORK >= max(1,M).
          *          For optimum performance LWORK >= N*NB if SIDE = 'L', and
          *          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
          *          blocksize.
          *
          *          If LWORK = -1, then a workspace query is assumed; the routine
          *          only calculates the optimal size of the WORK array, returns
          *          this value as the first entry of the WORK array, and no error
          *          message related to LWORK is issued by XERBLA.
          *
          *  INFO    (output) INTEGER
          *          = 0:  successful exit
          *          < 0:  if INFO = -i, the i-th argument had an illegal value
          */
       private void dormlq(char side, char trans, int m, int n, int k, double A[][], int lda,
                           double tau[], double C[][], int ldc, double work[], int lwork, int info[]) {
           int nbmax = 64;
           int ldt = nbmax + 1;
           boolean left;
           boolean lquery;
           boolean notran;
           char transt;
           int i;
           int i1;
           int i2;
           int i3;
           int ib;
           int ic = 0;
           int iinfo[] = new int[1];
           int iws;
           int jc = 0;
           int ldwork;
           int lwkopt = 0;
           int mi = 0;
           int nb = 0;
           int nbmin;
           int ni = 0;
           int nq;
           int nw;
           double T[][] = new double[ldt][nbmax];
           String name = null;
           String opts = null;
           char[] optsC = new char[2];
           double array1[][];
           int j;
           int p;
           double v[];
           int cdim;
           double array2[][];
           int row1;
           double work2[][];
           
           // Test the input arguments
           info[0] = 0;
           left = ((side == 'L') || (side == 'l'));
           notran = ((trans == 'N') || (trans == 'n'));
           lquery = (lwork == -1);
           
           // nq is the order of Q and nw is the minimum dimension of work
           
           if (left) {
               nq = m;
               nw = n;
           }
           else {
               nq = n;
               nw = m;
           }
           if ((!left) && (side != 'R') && (side != 'r')) {
               info[0] = -1;
           }
           else if ((!notran) && (trans != 'T') && (trans != 't')) {
               info[0] = -2;
           }
           else if (m < 0) {
               info[0] = -3;
           }
           else if (n < 0) {
               info[0] = -4;
           }
           else if ((k < 0) || (k > nq)) {
               info[0] = -5;
           }
           else if (lda < Math.max(1,k)) {
               info[0] = -7;
           }
           else if (ldc < Math.max(1, m)) {
               info[0] = -10;
           }
           else if ((lwork < Math.max(1, nw)) && (!lquery)) {
               info[0] = -12;
           }
           
           if (info[0] == 0) {
               // Determine the block size.  nb may be at most nbmax, where nbmax
               // is used to define the local array T.
               name = new String("DORMLQ");
               optsC[0] = side;
               optsC[1] = trans;
               opts = new String(optsC);
               nb = Math.min(nbmax, ilaenv(1, name, opts, m, n, k, -1));
               lwkopt = Math.max(1, nw) * nb;
               work[0] = lwkopt;
           } // if (info[0] == 0)
           
           if (info[0] != 0) {
               MipavUtil.displayError("Error dormlq had info[0] = " + info[0]);
               return;
           }
           else if (lquery) {
               return;
           }
           
           // Quick return if possible
           if ((m == 0) || (n == 0) || (k == 0)) {
               work[0] = 1;
               return;
           }
           
           nbmin = 2;
           ldwork = nw;
           if ((nb > 1) && (nb < k)) {
               iws = nw*nb;
               if (lwork < iws) {
                   nb = lwork/ldwork;
                   nbmin = Math.max(2, ilaenv(2, name, opts, m, n, k, -1));
               }
           } // if ((nb > 1) && (nb < k)) 
           else {
               iws = nw;
           }
           
           if ((nb < nbmin) || (nb >= k)) {
               // Use unblocked code
               dorml2(side, trans, m, n, k, A, lda, tau, C, ldc, work, iinfo);
           } // if (nb < nbmin) || (nb >= k))
           else {
               // Use blocked code
               if ((left && notran) || ((!left) && (!notran))) {
                   i1 = 1;
                   i2 = k;
                   i3 = nb;
               }
               else {
                   i1 = ((k-1)/nb)*nb + 1;
                   i2 = 1;
                   i3 = -nb;
               }
               
               if (left) {
                   ni = n;
                   jc = 1;
               }
               else {
                   mi = m;
                   ic = 1;
               }
               
               if (!notran) {
                   transt = 'T';
               }
               else {
                   transt = 'N';
               }
               
               if (i3 == nb) {
                   for (i = i1; i <= i2; i += nb) {
                       ib = Math.min(nb, k-i+1);
                       
                       // Form the triangular factor of the block reflector
                       // H = H(i) H(i+1) ... H(i+ib-1)
                       array1 = new double[ib][nq-i+1];
                       for (j = 0; j < ib; j++) {
                           for (p = 0; p < nq-i+1; p++) {
                               array1[j][p] = A[i-1+j][i-1+p];
                           }
                       }
                       v = new double[ib];
                       for (j = 0; j < ib; j++) {
                           v[j] = tau[i-1+j];
                       }
                       dlarft('F', 'R', nq-i+1, ib, array1, ib, v, T, ldt);
                       for (j = 0; j < ib; j++) {
                           for (p = 0; p < nq-i+1; p++) {
                               A[i-1+j][i-1+p] = array1[j][p];
                           }
                       }
                       
                       if (left) {
                           // H or H' is applied to C(i:m,1:n)
                           mi = m - i + 1;
                           ic = i;
                       }
                       else {
                           // H or H' is applied to C(1:m,i:n)
                           ni = n - i + 1;
                           jc = i;
                       }
                       
                       // Apply H or H'
                       if (left) {
                           cdim = mi;
                       }
                       else {
                           cdim = ni;
                       }
                       array1 = new double[ib][cdim];
                       for (j = 0; j < ib; j++) {
                           for (p = 0; p < cdim; p++) {
                               array1[j][p] = A[i-1+j][i-1+p];
                           }
                       }
                       row1 = Math.max(1, mi);
                       array2 = new double[row1][ni];
                       for (j = 0; j < row1; j++) {
                           for (p = 0; p < ni; p++) {
                               array2[j][p] = C[ic-1+j][jc-1+p];
                           }
                       }
                       work2 = new double[ldwork][ib];
                       dlarfb(side, transt, 'F', 'R', mi, ni, ib, array1, ib, T, ldt, array2, row1,
                              work2, ldwork);
                       for (j = 0; j < row1; j++) {
                           for (p = 0; p < ni; p++) {
                               C[ic-1+j][jc-1+p] = array2[j][p];
                           }
                       }
                   } // for (i = i1; i <= i2; i+= nb)
               } // if (i3 == nb)
               else { // i3 == -nb
                   for (i = i1; i >= i2; i -= nb) {
   ib = Math.min(nb, k-i+1);
                       
                       // Form the triangular factor of the block reflector
                       // H = H(i) H(i+1) ... H(i+ib-1)
                       array1 = new double[ib][nq-i+1];
                       for (j = 0; j < ib; j++) {
                           for (p = 0; p < nq-i+1; p++) {
                               array1[j][p] = A[i-1+j][i-1+p];
                           }
                       }
                       v = new double[ib];
                       for (j = 0; j < ib; j++) {
                           v[j] = tau[i-1+j];
                       }
                       dlarft('F', 'R', nq-i+1, ib, array1, ib, v, T, ldt);
                       for (j = 0; j < ib; j++) {
                           for (p = 0; p < nq-i+1; p++) {
                               A[i-1+j][i-1+p] = array1[j][p];
                           }
                       }
                       
                       if (left) {
                           // H or H' is applied to C(i:m,1:n)
                           mi = m - i + 1;
                           ic = i;
                       }
                       else {
                           // H or H' is applied to C(1:m,i:n)
                           ni = n - i + 1;
                           jc = i;
                       }
                       
                       // Apply H or H'
                       if (left) {
                           cdim = mi;
                       }
                       else {
                           cdim = ni;
                       }
                       array1 = new double[ib][cdim];
                       for (j = 0; j < ib; j++) {
                           for (p = 0; p < cdim; p++) {
                               array1[j][p] = A[i-1+j][i-1+p];
                           }
                       }
                       row1 = Math.max(1, mi);
                       array2 = new double[row1][ni];
                       for (j = 0; j < row1; j++) {
                           for (p = 0; p < ni; p++) {
                               array2[j][p] = C[ic-1+j][jc-1+p];
                           }
                       }
                       work2 = new double[ldwork][ib];
                       dlarfb(side, transt, 'F', 'R', mi, ni, ib, array1, ib, T, ldt, array2, row1,
                              work2, ldwork);
                       for (j = 0; j < row1; j++) {
                           for (p = 0; p < ni; p++) {
                               C[ic-1+j][jc-1+p] = array2[j][p];
                           }
                       }    
                   } // for (i = i1; i >= i2; i-= nb)
               } // else i3 == -nb
           } // else
           work[0] = lwkopt;
           return;
       } // dormlq
       
       
       
       /*
        * This is a port of  version 3.2 LAPACK routine DORML2.  Original DORML2 created by Univ. of Tennessee,
        * Univ. ov California Berkeley, Univ. Of Colorado Denver, and NAG Ltd., November, 2006
          *  Purpose
          *  =======
          *
          *  DORML2 overwrites the general real m by n matrix C with
          *
          *        Q * C  if SIDE = 'L' and TRANS = 'N', or
          *
          *        Q'* C  if SIDE = 'L' and TRANS = 'T', or
          *
          *        C * Q  if SIDE = 'R' and TRANS = 'N', or
          *
          *        C * Q' if SIDE = 'R' and TRANS = 'T',
          *
          *  where Q is a real orthogonal matrix defined as the product of k
          *  elementary reflectors
          *
          *        Q = H(k) . . . H(2) H(1)
          *
          *  as returned by DGELQF. Q is of order m if SIDE = 'L' and of order n
          *  if SIDE = 'R'.
          *
          *  Arguments
          *  =========
          *
          *  SIDE    (input) CHARACTER*1
          *          = 'L': apply Q or Q' from the Left
          *          = 'R': apply Q or Q' from the Right
          *
          *  TRANS   (input) CHARACTER*1
          *          = 'N': apply Q  (No transpose)
          *          = 'T': apply Q' (Transpose)
          *
          *  M       (input) INTEGER
          *          The number of rows of the matrix C. M >= 0.
          *
          *  N       (input) INTEGER
          *          The number of columns of the matrix C. N >= 0.
          *
          *  K       (input) INTEGER
          *          The number of elementary reflectors whose product defines
          *          the matrix Q.
          *          If SIDE = 'L', M >= K >= 0;
          *          if SIDE = 'R', N >= K >= 0.
          *
          *  A       (input) DOUBLE PRECISION array, dimension
          *                               (LDA,M) if SIDE = 'L',
          *                               (LDA,N) if SIDE = 'R'
          *          The i-th row must contain the vector which defines the
          *          elementary reflector H(i), for i = 1,2,...,k, as returned by
          *          DGELQF in the first k rows of its array argument A.
          *          A is modified by the routine but restored on exit.
          *
          *  LDA     (input) INTEGER
          *          The leading dimension of the array A. LDA >= max(1,K).
          *
          *  TAU     (input) DOUBLE PRECISION array, dimension (K)
          *          TAU(i) must contain the scalar factor of the elementary
          *          reflector H(i), as returned by DGELQF.
          *
          *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
          *          On entry, the m by n matrix C.
          *          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
          *
          *  LDC     (input) INTEGER
          *          The leading dimension of the array C. LDC >= max(1,M).
          *
          *  WORK    (workspace) DOUBLE PRECISION array, dimension
          *                                   (N) if SIDE = 'L',
          *                                   (M) if SIDE = 'R'
          *
          *  INFO    (output) INTEGER
          *          = 0: successful exit
          *          < 0: if INFO = -i, the i-th argument had an illegal value
          */
       private void dorml2(char side, char trans, int m, int n, int k, double A[][], int lda, 
                           double tau[], double C[][], int ldc, double work[], int info[]) {
           boolean left;
           boolean notran;
           int i;
           int j;
           int p;
           int i1;
           int i2;
           int i3;
           int ic = 0;
           int jc = 0;
           int mi = 0;
           int ni = 0;
           int nq;
           double aii;
           int dimv;
           double v[];
           int row1;
           double array1[][];
           
           // Test the input arguments
           info[0] = 0;
           left = ((side == 'L') || (side == 'l'));
           notran = ((trans == 'N') || (trans == 'n'));
           
           // nq is of order q
           
           if (left) {
               nq = m;
           }
           else {
               nq = n;
           }
           if ((!left ) && (side != 'R') && (side != 'r')) {
               info[0] = -1;
           }
           else if ((!notran) && (trans != 'T') && (trans != 't')) {
               info[0] = -2;
           }
           else if (m < 0) {
               info[0] = -3;
           }
           else if (n < 0) {
               info[0] = -4;
           }
           else if ((k < 0) || (k > nq)) {
               info[0] = -5;
           }
           else if (lda < Math.max(1,k)) {
               info[0] = -7;
           }
           else if (ldc < Math.max(1, m)) {
               info[0] = -10;
           }
           
           if (info[0] != 0) {
               MipavUtil.displayError("Error dorml2 had info[0] = " + info[0]);
               return;
           }
           
           // Quick return if possible
           if ((m == 0) || (n == 0) || (k == 0)) {
               return;
           }
           
           if ((left && notran) || ((!left) && (!notran))) {
               i1 = 1;
               i2 = k;
               i3 = 1;
           }
           else {
               i1 = k;
               i2 = 1;
               i3 = -1;
           }
           
           if (left) {
               ni = n;
               jc = 1;
           }
           else {
               mi = m;
               ic = 1;
           }
           
           if (i3 == 1) {
               for (i = i1; i <= i2; i++) {
                   if (left) {
                       // H(i) is applied to C(i:m,1:n)
                       mi = m - i + 1;
                       ic = i;
                   }
                   else {
                       // H(i) is applied to C(1:m,i:n)
                       ni = n - i + 1;
                       jc = i;
                   }
                   
                   // Apply H(i)
                   
                   aii = A[i-1][i-1];
                   A[i-1][i-1] = 1.0;
                   if (left) {
                       dimv = mi;
                   }
                   else {
                       dimv = ni;
                   }
                   v = new double[dimv];
                   for (j = 0; j < dimv; j++) {
                       v[j] = A[i-1][i-1+j];
                   }
                   row1 = Math.max(1, mi);
                   array1 = new double[row1][ni];
                   for (j = 0; j < row1; j++) {
                       for (p = 0; p < ni; p++) {
                           array1[j][p] = C[ic-1+j][jc-1+p];
                       }
                   }
                   dlarf(side, mi, ni, v, 1, tau[i-1], array1, row1, work);
                   for (j = 0; j < row1; j++) {
                       for (p = 0; p < ni; p++) {
                           C[ic-1+j][jc-1+p] = array1[j][p];
                       }
                   }
                   A[i-1][i-1] = aii;
               } // for (i = i1; i <= i2; i++)
           } // if (i3 == 1)
           else { // i3 == -1
               for (i = i1; i >= i2; i--) {
                   if (left) {
                       // H(i) is applied to C(i:m,1:n)
                       mi = m - i + 1;
                       ic = i;
                   }
                   else {
                       // H(i) is applied to C(1:m,i:n)
                       ni = n - i + 1;
                       jc = i;
                   }
                   
                   // Apply H(i)
                   
                   aii = A[i-1][i-1];
                   A[i-1][i-1] = 1.0;
                   if (left) {
                       dimv = mi;
                   }
                   else {
                       dimv = ni;
                   }
                   v = new double[dimv];
                   for (j = 0; j < dimv; j++) {
                       v[j] = A[i-1][i-1+j];
                   }
                   row1 = Math.max(1, mi);
                   array1 = new double[row1][ni];
                   for (j = 0; j < row1; j++) {
                       for (p = 0; p < ni; p++) {
                           array1[j][p] = C[ic-1+j][jc-1+p];
                       }
                   }
                   dlarf(side, mi, ni, v, 1, tau[i-1], array1, row1, work);
                   for (j = 0; j < row1; j++) {
                       for (p = 0; p < ni; p++) {
                           C[ic-1+j][jc-1+p] = array1[j][p];
                       }
                   }
                   A[i-1][i-1] = aii;    
               } // for (i = i1; i >= i2; i--)
           } // else i3 == -1
           return;
        } // dorml2
    	               
       
    	         
     /**
      * This is a port of LAPACK version 3.2 auxiliary routine DLACPY. Original DLACPY created by Univ. of Tennessee,
      * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
      * dlacpy copies all or part of a two-dimensional matrix A to another matrix B.
      *
      * @param  uplo  input char Specifies the part of the matrix A to be copied to B. 
      *               = 'U': Upper triangular part
      *               = 'L': Lower triangular part
      *               Otherwise: All of the matrix A
      * @param  m     input int The number of rows of the matrix A. m >= 0.
      * @param  n     input int The number of columns of the matrix A. n >= 0.
      * @param  A     input double[][] of dimension (lda,n). Has m by n matrix A. If uplo = 'U', only the upper triangle
      *               or trapezoid is accessed; if uplo = 'L', only the lower triangle or trapezoid is accessed.
      * @param  lda   input int The leading dimension of the array A. lda >= max(1,m).
      * @param  B     output double[][] of dimension (ldb,n). On exit, B = A in the locations specified by uplo.
      * @param  ldb   input int The leading dimension of the array B. ldb >= max(1,m).
      */
     private void dlacpy(char uplo, int m, int n, double[][] A, int lda, double[][] B, int ldb) {
         int i, j;

         if ((uplo == 'U') || (uplo == 'u')) {

             for (j = 0; j < n; j++) {

                 for (i = 0; i <= Math.min(j, m - 1); i++) {
                     B[i][j] = A[i][j];
                 }
             }
         } // if ((uplo == 'U') || (uplo == 'u'))
         else if ((uplo == 'L') || (uplo == 'l')) {

             for (j = 0; j < n; j++) {

                 for (i = j; i < m; i++) {
                     B[i][j] = A[i][j];
                 }
             }
         } // else if ((uplo == 'L') || (uplo == 'l'))
         else {

             for (j = 0; j < n; j++) {

                 for (i = 0; i < m; i++) {
                     B[i][j] = A[i][j];
                 }
             }
         } // else
         return;
     } // dlacpy
    	            
    /** This is a port of version 3.2 LAPACK routine DBDSQR.
     *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
     *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
     *     January 2007
     *
     *     .. Scalar Arguments ..
           CHARACTER          UPLO
           INTEGER            INFO, LDC, LDU, LDVT, N, NCC, NCVT, NRU
     *     ..
     *     .. Array Arguments ..
           DOUBLE PRECISION   C( LDC, * ), D( * ), E( * ), U( LDU, * ),
          $                   VT( LDVT, * ), WORK( * )
     *     ..
     *
     *  Purpose
     *  =======
     *
     *  DBDSQR computes the singular values and, optionally, the right and/or
     *  left singular vectors from the singular value decomposition (SVD) of
     *  a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
     *  zero-shift QR algorithm.  The SVD of B has the form
     * 
     *     B = Q * S * P**T
     * 
     *  where S is the diagonal matrix of singular values, Q is an orthogonal
     *  matrix of left singular vectors, and P is an orthogonal matrix of
     *  right singular vectors.  If left singular vectors are requested, this
     *  subroutine actually returns U*Q instead of Q, and, if right singular
     *  vectors are requested, this subroutine returns P**T*VT instead of
     *  P**T, for given real input matrices U and VT.  When U and VT are the
     *  orthogonal matrices that reduce a general matrix A to bidiagonal
     *  form:  A = U*B*VT, as computed by DGEBRD, then
     *
     *     A = (U*Q) * S * (P**T*VT)
     *
     *  is the SVD of A.  Optionally, the subroutine may also compute Q**T*C
     *  for a given real input matrix C.
     *
     *  See "Computing  Small Singular Values of Bidiagonal Matrices With
     *  Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
     *  LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
     *  no. 5, pp. 873-912, Sept 1990) and
     *  "Accurate singular values and differential qd algorithms," by
     *  B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
     *  Department, University of California at Berkeley, July 1992
     *  for a detailed description of the algorithm.
     *
     *  Arguments
     *  =========
     *
     *  UPLO    (input) CHARACTER*1
     *          = 'U':  B is upper bidiagonal;
     *          = 'L':  B is lower bidiagonal.
     *
     *  N       (input) INTEGER
     *          The order of the matrix B.  N >= 0.
     *
     *  NCVT    (input) INTEGER
     *          The number of columns of the matrix VT. NCVT >= 0.
     *
     *  NRU     (input) INTEGER
     *          The number of rows of the matrix U. NRU >= 0.
     *
     *  NCC     (input) INTEGER
     *          The number of columns of the matrix C. NCC >= 0.
     *
     *  D       (input/output) DOUBLE PRECISION array, dimension (N)
     *          On entry, the n diagonal elements of the bidiagonal matrix B.
     *          On exit, if INFO=0, the singular values of B in decreasing
     *          order.
     *
     *  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
     *          On entry, the N-1 offdiagonal elements of the bidiagonal
     *          matrix B. 
     *          On exit, if INFO = 0, E is destroyed; if INFO > 0, D and E
     *          will contain the diagonal and superdiagonal elements of a
     *          bidiagonal matrix orthogonally equivalent to the one given
     *          as input.
     *
     *  VT      (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
     *          On entry, an N-by-NCVT matrix VT.
     *          On exit, VT is overwritten by P**T * VT.
     *          Not referenced if NCVT = 0.
     *
     *  LDVT    (input) INTEGER
     *          The leading dimension of the array VT.
     *          LDVT >= max(1,N) if NCVT > 0; LDVT >= 1 if NCVT = 0.
     *
     *  U       (input/output) DOUBLE PRECISION array, dimension (LDU, N)
     *          On entry, an NRU-by-N matrix U.
     *          On exit, U is overwritten by U * Q.
     *          Not referenced if NRU = 0.
     *
     *  LDU     (input) INTEGER
     *          The leading dimension of the array U.  LDU >= max(1,NRU).
     *
     *  C       (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
     *          On entry, an N-by-NCC matrix C.
     *          On exit, C is overwritten by Q**T * C.
     *          Not referenced if NCC = 0.
     *
     *  LDC     (input) INTEGER
     *          The leading dimension of the array C.
     *          LDC >= max(1,N) if NCC > 0; LDC >=1 if NCC = 0.
     *
     *  WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)
     *
     *  INFO    (output) INTEGER
     *          = 0:  successful exit
     *          < 0:  If INFO = -i, the i-th argument had an illegal value
     *          > 0:
     *             if NCVT = NRU = NCC = 0,
     *                = 1, a split was marked by a positive value in E
     *                = 2, current block of Z not diagonalized after 30*N
     *                     iterations (in inner while loop)
     *                = 3, termination criterion of outer while loop not met 
     *                     (program created more than N unreduced blocks)
     *             else NCVT = NRU = NCC = 0,
     *                   the algorithm did not converge; D and E contain the
     *                   elements of a bidiagonal matrix which is orthogonally
     *                   similar to the input matrix B;  if INFO = i, i
     *                   elements of E have not converged to zero.
     *
     *  Internal Parameters
     *  ===================
     *
     *  TOLMUL  DOUBLE PRECISION, default = max(10,min(100,EPS**(-1/8)))
     *          TOLMUL controls the convergence criterion of the QR loop.
     *          If it is positive, TOLMUL*EPS is the desired relative
     *             precision in the computed singular values.
     *          If it is negative, abs(TOLMUL*EPS*sigma_max) is the
     *             desired absolute accuracy in the computed singular
     *             values (corresponds to relative accuracy
     *             abs(TOLMUL*EPS) in the largest singular value.
     *          abs(TOLMUL) should be between 1 and 1/EPS, and preferably
     *             between 10 (for fast convergence) and .1/EPS
     *             (for there to be some accuracy in the results).
     *          Default is to lose at either one eighth or 2 of the
     *             available decimal digits in each computed singular value
     *             (whichever is smaller).
     *
     *  MAXITR  INTEGER, default = 6
     *          MAXITR controls the maximum number of passes of the
     *          algorithm through its inner loop. The algorithms stops
     *          (and so fails to converge) if the number of passes
     *          through the inner loop exceeds MAXITR*N**2.
     */
     private void dbdsqr(char uplo, int n, int ncvt, int nru, int ncc, double d[], double e[],
                         double VT[][], int ldvt, double U[][], int ldu, double C[][], int ldc,
                         double work[], int info[]) {
         int maxitr = 6;
         boolean lower;
         boolean rotate;
         int i;
         int idir;
         int isub;
         int iter;
         int j;
         int k;
         int p;
         int ll;
         int lll;
         int m;
         int maxit;
         int nm1;
         int nm12;
         int nm13;
         int oldll;
         int oldm;
         double abse;
         double abss;
         double cosl[] = new double[1];
         double cosr[] = new double[1];
         double cs[] = new double[1];
         double eps;
         double f;
         double g;
         double h;
         double mu;
         double oldcs[] = new double[1];
         double oldsn[] = new double[1];
         double r[] = new double[1];
         double shift[] = new double[1];
         double sigmn[] = new double[1];
         double sigmx[] = new double[1];
         double sinl[] = new double[1];
         double sinr[] = new double[1];
         double sll;
         double smax;
         double smin;
         double sminl;
         double sminoa;
         double sn[] = new double[1];
         double temp;
         double thresh;
         double tol;
         double tolmul;
         double unfl;
         double w1[];
         double w2[];
         double w3[];
         double array1[][];
         int row1;
         
         // Test the input parameters.
         info[0] = 0;
         lower = ((uplo == 'L') || (uplo == 'l'));
         if ((uplo != 'U') && (uplo != 'u') && (!lower)) {
             info[0] = -1;
         }
         else if (n < 0) {
             info[0] = -2;
         }
         else if (ncvt < 0) {
             info[0] = -3;
         }
         else if (nru < 0) {
             info[0] = -4;
         }
         else if (ncc < 0) {
             info[0] = -5;
         }
         else if (((ncvt == 0) && (ldvt < 1)) || ((ncvt > 0) && (ldvt < Math.max(1,n)))) {
             info[0] = -9;
         }
         else if (ldu < Math.max(1, nru)) {
             info[0] = -11;
         }
         else if (((ncc == 0) && (ldc < 1)) || ((ncc > 0) && (ldc < Math.max(1,n)))) {
             info[0] = -13;
         }
         if (info[0] != 0) {
             MipavUtil.displayError("Error dbdsqr had info[0] = " + info[0]);
             return;
         }
         if (n == 0) {
             return;
         }
         if (n != 1) {
             // Rotate is true if any singular vectors desired, false otherwise
             rotate = (ncvt > 0) || (nru > 0) || (ncc > 0);
             
             // If no singular vectors desired, use qd algorithm
             if (!rotate) {
                 dlasq1(n, d, e, work, info);
                 return;
             }
             
             nm1 = n - 1;
             nm12 = nm1 + nm1;
             nm13 = nm12 + nm1;
             idir = 0;
             
             // Get machine constants
             
             eps = dlamch('E'); // Epsilon
             unfl = dlamch('S'); // Safe minimum
             
             // If matrix lower bidiagonal, rotate to be upper bidiagonal
             // by applying Givens rotations on the left
             
             if (lower) {
                 for (i = 1; i <= n-1; i++) {
                     dlartg(d[i-1], e[i-1], cs, sn, r);  
                     d[i-1] = r[0];
                     e[i-1] = sn[0]*d[i];
                     d[i] = cs[0]*d[i];
                     work[i-1] = cs[0];
                     work[nm1+i-1] = sn[0];
                 } // for (i = 1; i <= n-1; i++)
                 
                 // Update singular vectors if desired
                 if (nru > 0) {
                     w1 = new double[n-1];
                     w2 = new double[n-1];
                     for (k = 0; k < n-1; k++) {
                         w1[k] = work[k];
                         w2[k] = work[n-1+k];
                     }
                     dlasr('R', 'V', 'F', nru, n, w1, w2, U, ldu);
                 } // if (nru > 0)
                 
                 if (ncc > 0) {
                     w1 = new double[n-1];
                     w2 = new double[n-1];
                     for (k = 0; k < n-1; k++) {
                         w1[k] = work[k];
                         w2[k] = work[n-1+k];
                     } 
                     dlasr('L', 'V', 'F', n, ncc, w1, w2, C, ldc);
                 } // if (ncc > 0)
             } // if (lower)
             
             // Compute singular values to relative accuracy tol
             // (By setting tol to be negative, algorithm will compute
             // singular values to absolute accuracy abs(tol)*norm(input matrix))
             
             tolmul = Math.max(10.0, Math.min(100.0, Math.pow(eps, -0.125)));
             tol = tolmul * eps;
             
             // Compute approximate maximum, minimum singular values
             
             smax = 0.0;
             for (i = 1; i <= n; i++) {
                 smax = Math.max(smax, Math.abs(d[i-1]));
             }
             for (i = 1; i <= n-1; i++) {
                 smax = Math.max(smax, Math.abs(e[i-1]));
             }
             sminl = 0.0;
             if (tol >= 0.0) {
                 // Relative accuracy desired
                 sminoa = Math.abs(d[0]);
                 if (sminoa != 0.0) {
                     mu = sminoa;
                     for (i = 2; i <= n; i++) {
                         mu = Math.abs(d[i-1]) * (mu/(mu + Math.abs(e[i-2])));
                         sminoa = Math.min(sminoa, mu);
                         if (sminoa == 0.0) {
                             break;
                         }
                     } // for (i = 2; i <= n; i++)
                 } // if (sminoa != 0.0)
                 sminoa = sminoa/Math.sqrt((double)n);
                 thresh = Math.max(tol * sminoa, maxitr * n * n * unfl);
             } // if (tol >= 0.0)
             else {
                 // Absolute accuracy required
                 thresh = Math.max(Math.abs(tol)*smax, maxitr * n * n * unfl);
             }
             
             // Prepare for main iteration loop for the singular values
             // (maxit is the maximum number of passes through the inner
             // loop permitted before nonconvergence signalled.)
             
             maxit = maxitr * n * n;
             iter = 0;
             oldll = -1;
             oldm = -1;
             
             // m points to the last element of unconverged part of matrix
             
             m = n;
             
             // Begin main iteration loop
             loop1: while (true) {
                 // Check for convergence or exceeding iteration count
                 if (m <= 1) {
                     break;
                 }
                 if (iter > maxit) {
                     info[0] = 0;
                     for (i = 1; i <= n-1; i++) {
                         if (e[i-1] != 0.0) {
                             info[0] = info[0] + 1;
                         }
                     }
                     return;
                 } // if (iter > maxit)
                 
                 // Find diagonal block of matrix to work on
                 
                 if ((tol < 0.0) && (Math.abs(d[m-1]) <= thresh)) {
                     d[m-1] = 0.0;
                 }
                 smax = Math.abs(d[m-1]);
                 smin = smax;
                 loop2: {
                     for(lll = 1; lll <= m-1; lll++) {
                         ll = m - lll;
                         abss = Math.abs(d[ll-1]);
                         abse = Math.abs(e[ll-1]);
                         if ((tol < 0.0) && (abss <= thresh)) {
                             d[ll-1] = 0.0;
                         }
                         if (abse <= thresh) {
                             e[ll-1] = 0.0;
                             
                             // Matrix splits since e[ll-1] = 0.0
                             
                             if (ll == m-1) {
                                 // Convergence of bottom singular value, return to top of loop1
                                 m = m - 1;
                                 continue loop1;
                             } // if (ll == m-1)
                             break loop2;
                         } // if (abse <= thresh)
                         smin = Math.min(smin, abss);
                         smax = Math.max(smax, Math.max(abss, abse));
                     } // for(lll = 1; lll <= m-1; lll++)
                     ll = 0;
                 } // loop2:
                 
                 ll = ll + 1;
                 
                 // e[ll-1] through e[m-2] are nonzero, e[ll-2] is zero.
                 
                 if (ll == m-1) {
                     // 2 by 2 block, handle separately
                     dlasv2(d[m-2], e[m-2], d[m-1], sigmn, sigmx, sinr, cosr, sinl, cosl);
                     d[m-2] = sigmx[0];
                     e[m-2] = 0.0;
                     d[m-1] = sigmn[0];
                     
                     // Compute singular vectors if desired
                     if (ncvt > 0) {
                         w1 = new double[ncvt];
                         w2 = new double[ncvt];
                         for (k = 0; k < ncvt; k++) {
                             w1[k] = VT[m-2][k];
                             w2[k] = VT[m-1][k];
                         }
                         drot(ncvt, w1, 1, w2, 1, cosr[0], sinr[0]);
                         for (k = 0; k < ncvt; k++) {
                             VT[m-2][k] = w1[k];
                             VT[m-1][k] = w2[k];
                         }
                     } // if (ncvt > 0)
                     
                     if (nru > 0) {
                         w1 = new double[nru];
                         w2 = new double[nru];
                         for (k = 0; k < nru; k++) {
                             w1[k] = U[k][m-2];
                             w2[k] = U[k][m-1];
                         }
                         drot(nru, w1, 1, w2, 1, cosl[0], sinl[0]);
                         for (k = 0; k < nru; k++) {
                             U[k][m-2] = w1[k];
                             U[k][m-1] = w2[k];
                         }    
                     } // if (nru > 0)
                     
                     if (ncc > 0) {
                         w1 = new double[ncc];
                         w2 = new double[ncc];
                         for (k = 0; k < ncc; k++) {
                             w1[k] = C[m-2][k];
                             w2[k] = C[m-1][k];
                         }
                         drot(ncc, w1, 1, w2, 1, cosl[0], sinl[0]);
                         for (k = 0; k < ncc; k++) {
                             C[m-2][k] = w1[k];
                             C[m-1][k] = w2[k];
                         }    
                     } // if (ncc > 0)
                     
                     m = m-2;
                     continue loop1;
                 } // if (ll == m-1)
                 
                 // If working on new submatrix, choose shift direction
                 // (from larger end diagonal element towards smaller)
                 
                 if ((ll > oldm) || (m < oldll)) {
                     if (Math.abs(d[ll-1]) >= Math.abs(d[m-1])) {
                         // Chase bulge from top (big end) to bottom (small end)
                         idir = 1;
                     }
                     else {
                         // Chase bulge from bottom (big end) to top (small end)
                         idir = 2;
                     }
                 } // if ((ll > oldm) || (m < oldll))
                 
                 // Apply convergence tests
                 
                 if (idir == 1) {
                     // Run convergence test in forward direction
                     // First apply standard test to bottom of matrix
                     
                     if ((Math.abs(e[m-2]) <= Math.abs(tol)*Math.abs(d[m-1])) ||
                         ((tol < 0.0) && (Math.abs(e[m-2]) <= thresh))) {
                         e[m-2] = 0.0;
                         continue loop1;
                     }
                     
                     if (tol >= 0.0) {
                         // If relative accuracy desired,
                         // apply convergence criterion forward
                         mu = Math.abs(d[ll-1]);
                         sminl = mu;
                         for (lll = ll; lll <= m-1; lll++) {
                             if (Math.abs(e[lll-1]) <= tol*mu) {
                                 e[lll-1] = 0.0;
                                 continue loop1;
                             }
                             mu = Math.abs(d[lll]) * (mu/(mu + Math.abs(e[lll-1])));
                             sminl = Math.min(sminl, mu);
                         } // for (lll = ll; lll <= m-1; lll++)
                     } // if (tol >= 0.0)
                 } // if (idir == 1)
                 else { // idir == 2
                     // Run convergence test in backward direction
                     // First apply standard test to top of matrix
                     if ((Math.abs(e[ll-1]) <= Math.abs(tol) * Math.abs(d[ll-1])) || 
                         ((tol < 0.0) && (Math.abs(e[ll-1]) <= thresh))) {
                         e[ll-1] = 0.0;
                         continue loop1;
                     }
                     
                     if (tol >= 0.0) {
                         // If relative accuracy desired,
                         // apply convergence criterion backward
                         mu = Math.abs(d[m-1]);
                         sminl = mu;
                         for (lll = m - 1; lll >= ll; lll--) {
                             if (Math.abs(e[lll-1]) <= tol*mu) {
                                 e[lll-1] = 0.0;
                                 continue loop1;
                             }
                             mu = Math.abs(d[lll-1]) * (mu/ (mu + Math.abs(e[lll-1])));
                             sminl = Math.min(sminl, mu);
                         } // for (lll = m - 1; lll >= ll; lll--)
                     } // if (tol >= 0.0)
                 } // else idir == 2
                 oldll = ll;
                 oldm = m;
                 
                 // Compute shift.  First, test if shifting would ruin relative
                 // accuracy, and if so set the shift to zero.
                 
                 if ((tol >= 0.0) && (n*tol*(sminl/smax) <= Math.max(eps, 0.01*tol))) {
                     // Use a zero shift to avoid loss of relative accuracy
                     shift[0] = 0;
                 }
                 else {
                     // Compute the shift from 2-by-2 block at end of matrix
                     w1 = new double[1];
                     w2 = new double[1];
                     w3 = new double[1];
                     if (idir == 1) {
                         sll = Math.abs(d[ll-1]);
                         w1[0] = d[m-2];
                         w2[0] = e[m-2];
                         w3[0] = d[m-1];
                         dlas2(w1, w2, w3, shift, r);
                     }
                     else {
                         sll = Math.abs(d[m-1]);
                         w1[0] = d[ll-1];
                         w2[0] = e[ll-1];
                         w3[0] = d[ll];
                         dlas2(w1, w2, w3, shift, r);
                     }
                     
                     // Test if shift negligible, and if so set to zero
                     if (sll > 0.0) {
                         temp = shift[0]/sll;
                         if (temp*temp < eps) {
                             shift[0] = 0.0;
                         }
                     } // if (sll > 0.0)
                 } // else
                 
                 // Increment iteration count
                 iter = iter + m - ll;
                 
                 // If shift[0] = 0, do simplified QR iteration
                 if (shift[0] == 0) {
                     if (idir == 1) {
                         // Chase bulge from top to bottom
                         // Save cosines and sines for later singular vector updates
                         
                         cs[0] = 1.0;
                         oldcs[0] = 1.0;
                         w1 = new double[1];
                         for (i = ll; i <= m-1; i++) {
                             dlartg(d[i-1]*cs[0], e[i-1], cs, sn, r);
                             if (i > ll) {
                                 e[i-2] = oldsn[0] * r[0];
                             }
                             dlartg(oldcs[0]*r[0], d[i]*sn[0], oldcs, oldsn, w1);
                             d[i-1] = w1[0];
                             work[i-ll] = cs[0];
                             work[i-ll+nm1] = sn[0];
                             work[i-ll+nm12] = oldcs[0];
                             work[i-ll+nm13] = oldsn[0];
                         } // for (i = ll; i <= m-1; i++)
                         h = d[m-1] * cs[0];
                         d[m-1] = h * oldcs[0];
                         e[m-2] = h * oldsn[0];
                         
                         // Update singular vectors
                         if (ncvt > 0) {
                             w1 = new double[m-ll];
                             w2 = new double[m-ll];
                             for (k = 0; k < m-ll; k++) {
                                 w1[k] = work[k];
                                 w2[k] = work[n-1+k];
                             }
                             row1 = Math.max(1, m-ll+1);
                             array1 = new double[row1][ncvt];
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncvt; p++) {
                                     array1[k][p] = VT[ll-1+k][p];
                                 }
                             }
                             dlasr('L', 'V', 'F', m-ll+1, ncvt, w1, w2, array1, row1);
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncvt; p++) {
                                     VT[ll-1+k][p] = array1[k][p];
                                 }
                             }
                         } // if (ncvt > 0)
                         
                         if (nru > 0) {
                             w1 = new double[m-ll];
                             w2 = new double[m-ll];
                             for (k = 0; k < m-ll; k++) {
                                 w1[k] = work[k+nm12];
                                 w2[k] = work[k+nm13];
                             }
                             row1 = Math.max(1, nru);
                             array1 = new double[row1][m-ll+1];
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < m-ll+1; p++) {
                                     array1[k][p] = U[k][ll-1+p];
                                 }
                             }
                             dlasr('R', 'V', 'F', nru, m-ll+1, w1, w2, array1, row1);
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < m-ll+1; p++) {
                                     U[k][ll-1+p] = array1[k][p];
                                 }
                             }    
                         } // if (nru > 0)
                         
                         if (ncc > 0) {
                             w1 = new double[m-ll];
                             w2 = new double[m-ll];
                             for (k = 0; k < m-ll; k++) {
                                 w1[k] = work[k+nm12];
                                 w2[k] = work[k+nm13];
                             }
                             row1 = Math.max(1, m-ll+1);
                             array1 = new double[row1][ncc];
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncc; p++) {
                                     array1[k][p] = C[ll-1+k][p];
                                 }
                             }
                             dlasr('L', 'V', 'F', m-ll+1, ncc, w1, w2, array1, row1);
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncc; p++) {
                                     C[ll-1+k][p] = array1[k][p];
                                 }
                             }    
                         } // if (ncc > 0)
                         
                         // Test convergence
                         if (Math.abs(e[m-2]) <= thresh) {
                             e[m-2] = 0.0;
                         }
                     } // if (idir == 1)
                     else { // idir == 2
                         // Chase bulge from bottom to top
                         // Save cosines and sines for later singular vector updates
                         cs[0] = 1.0;
                         oldcs[0] = 1.0;
                         w1 = new double[1];
                         for (i = m; i >= ll + 1; i--) {
                             dlartg(d[i-1]*cs[0], e[i-2], cs, sn, r);
                             if (i < m) {
                                 e[i-1] = oldsn[0]*r[0];
                             }
                             dlartg(oldcs[0]*r[0], d[i-2]*sn[0], oldcs, oldsn, w1);
                             d[i-1] = w1[0];
                             work[i-ll-1] = cs[0];
                             work[i-ll+nm1-1] = -sn[0];
                             work[i-ll+nm12-1] = oldcs[0];
                             work[i-ll+nm13-1] = -oldsn[0];
                         } // for (i = m; i >= ll + 1; i--)
                         h = d[ll-1]*cs[0];
                         d[ll-1] = h * oldcs[0];
                         e[ll-1] = h * oldsn[0];
                         
                         // Update singular vectors
                         if (ncvt > 0) {
                             w1 = new double[m-ll];
                             w2 = new double[m-ll];
                             for (k = 0; k < m-ll; k++) {
                                 w1[k] = work[nm12 + k];
                                 w2[k] = work[nm13+k];
                             }
                             row1 = Math.max(1, m-ll+1);
                             array1 = new double[row1][ncvt];
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncvt; p++) {
                                     array1[k][p] = VT[ll-1+k][p];
                                 }
                             }
                             dlasr('L', 'V', 'B', m-ll+1, ncvt, w1, w2, array1, row1);
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncvt; p++) {
                                     VT[ll-1+k][p] = array1[k][p];
                                 }
                             }
                         } // if (ncvt > 0)
                         
                         if (nru > 0) {
                             w1 = new double[m-ll];
                             w2 = new double[m-ll];
                             for (k = 0; k < m-ll; k++) {
                                 w1[k] = work[k];
                                 w2[k] = work[n-1+k];
                             }
                             row1 = Math.max(1, nru);
                             array1 = new double[row1][m-ll+1];
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < m-ll+1; p++) {
                                     array1[k][p] = U[k][ll-1+p];
                                 }
                             }
                             dlasr('R', 'V', 'B', nru, m-ll+1, w1, w2, array1, row1);
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < m-ll+1; p++) {
                                     U[k][ll-1+p] = array1[k][p];
                                 }
                             }    
                         } // if (nru > 0)
                         
                         if (ncc > 0) {
                             w1 = new double[m-ll];
                             w2 = new double[m-ll];
                             for (k = 0; k < m-ll; k++) {
                                 w1[k] = work[k];
                                 w2[k] = work[n-1+k];
                             }
                             row1 = Math.max(1, m-ll+1);
                             array1 = new double[row1][ncc];
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncc; p++) {
                                     array1[k][p] = C[ll-1+k][p];
                                 }
                             }
                             dlasr('L', 'V', 'B', m-ll+1, ncc, w1, w2, array1, row1);
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncc; p++) {
                                     C[ll-1+k][p] = array1[k][p];
                                 }
                             }    
                         } // if (ncc > 0)
                         
                         // Test convergence
                         if (Math.abs(e[ll-1]) <= thresh) {
                             e[ll-1] = 0.0;
                         }
                     } // else idir == 2
                 } // if (shift[0] == 0)
                 else { // shift[0] != 0
                     // Use nonzero shift
                     
                     if (idir == 1) {
                         // Chase bulge from top to bottom
                         // Save cosines and sines for later singular vector updates
                         if (d[ll-1] >= 0) {
                             f = (Math.abs(d[ll-1]) - shift[0]) * (1.0 + shift[0]/d[ll-1]);
                         }
                         else {
                             f = (Math.abs(d[ll-1]) - shift[0]) * (-1.0 + shift[0]/d[ll-1]);
                         }
                         g = e[ll-1];
                         for (i = ll; i <= m-1; i++) {
                             dlartg(f, g, cosr, sinr, r);
                             if (i > ll) {
                                 e[i-2] = r[0];
                             }
                             f = cosr[0]*d[i-1] + sinr[0]*e[i-1];
                             e[i-1] = cosr[0]*e[i-1] - sinr[0]*d[i-1];
                             g = sinr[0]*d[i];
                             d[i] = cosr[0]*d[i];
                             dlartg(f, g, cosl, sinl, r);
                             d[i-1] = r[0];
                             f = cosl[0]*e[i-1] + sinl[0]*d[i];
                             d[i] = cosl[0]*d[i] - sinl[0]*e[i-1];
                             if (i < m-1) {
                                 g = sinl[0]*e[i];
                                 e[i] = cosl[0]*e[i];
                             }
                             work[i-ll] = cosr[0];
                             work[i-ll+nm1] = sinr[0];
                             work[i-ll+nm12] = cosl[0];
                             work[i-ll+nm13] = sinl[0];
                         } // for (i = ll; i <= m-1; i++)
                         e[m-2] = f;
                         
                         // Update singular vectors
                         if (ncvt > 0) {
                             w1 = new double[m-ll];
                             w2 = new double[m-ll];
                             for (k = 0; k < m-ll; k++) {
                                 w1[k] = work[k];
                                 w2[k] = work[n-1+k];
                             }
                             row1 = Math.max(1, m-ll+1);
                             array1 = new double[row1][ncvt];
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncvt; p++) {
                                     array1[k][p] = VT[ll-1+k][p];
                                 }
                             }
                             dlasr('L', 'V', 'F', m-ll+1, ncvt, w1, w2, array1, row1);
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncvt; p++) {
                                     VT[ll-1+k][p] = array1[k][p];
                                 }
                             }
                         } // if (ncvt > 0)
                         
                         if (nru > 0) {
                             w1 = new double[m-ll];
                             w2 = new double[m-ll];
                             for (k = 0; k < m-ll; k++) {
                                 w1[k] = work[nm12+k];
                                 w2[k] = work[nm13+k];
                             }
                             row1 = Math.max(1, nru);
                             array1 = new double[row1][m-ll+1];
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < m-ll+1; p++) {
                                     array1[k][p] = U[k][ll-1+p];
                                 }
                             }
                             dlasr('R', 'V', 'F', nru, m-ll+1, w1, w2, array1, row1);
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < m-ll+1; p++) {
                                     U[k][ll-1+p] = array1[k][p];
                                 }
                             }    
                         } // if (nru > 0)
                         
                         if (ncc > 0) {
                             w1 = new double[m-ll];
                             w2 = new double[m-ll];
                             for (k = 0; k < m-ll; k++) {
                                 w1[k] = work[nm12+k];
                                 w2[k] = work[nm13+k];
                             }
                             row1 = Math.max(1, m-ll+1);
                             array1 = new double[row1][ncc];
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncc; p++) {
                                     array1[k][p] = C[ll-1+k][p];
                                 }
                             }
                             dlasr('L', 'V', 'F', m-ll+1, ncc, w1, w2, array1, row1);
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncc; p++) {
                                     C[ll-1+k][p] = array1[k][p];
                                 }
                             }    
                         } // if (ncc > 0)
                         
                         // Test convergence
                         if (Math.abs(e[m-2]) <= thresh) {
                             e[m-2] = 0.0;
                         }
                     } // if (idir == 1)
                     else { // idir == 2
                         // Chase bulge from bottom to top
                         // Save cosines and sines for later singular vector updates
                         if (d[m-1] >= 0.0) {
                             f = (Math.abs(d[m-1]) - shift[0]) * (1.0 + shift[0]/d[m-1]);
                         }
                         else {
                             f = (Math.abs(d[m-1]) - shift[0]) * (-1.0 + shift[0]/d[m-1]);
                         }
                         g = e[m-2];
                         for (i = m; i >= ll + 1; i--) {
                             dlartg(f, g, cosr, sinr, r);
                             if (i < m) {
                                 e[i-1] = r[0];
                             }
                             f = cosr[0]*d[i-1] + sinr[0]*e[i-2];
                             e[i-2] = cosr[0]*e[i-2] - sinr[0]*d[i-1];
                             g = sinr[0]*d[i-2];
                             d[i-2] = cosr[0] * d[i-2];
                             dlartg(f, g, cosl, sinl, r);
                             d[i-1] = r[0];
                             f = cosl[0]*e[i-2] + sinl[0]*d[i-2];
                             d[i-2] = cosl[0]*d[i-2] - sinl[0]*e[i-2];
                             if (i > ll+1) {
                                 g = sinl[0]*e[i-3];
                                 e[i-3] = cosl[0]*e[i-3];
                             }
                             work[i-ll-1] = cosr[0];
                             work[i-ll+nm1-1] = -sinr[0];
                             work[i-ll+nm12-1] = cosl[0];
                             work[i-ll+nm13-1] = -sinl[0];
                         } // for (i = m;i >= ll + 1; i--)
                         e[ll-1] = f;
                         
                         // Test convergence
                         if (Math.abs(e[ll-1]) <= thresh) {
                             e[ll-1] = 0.0;
                         }
                         
                         // Update singular vectors if desired
                         
                         if (ncvt > 0) {
                             w1 = new double[m-ll];
                             w2 = new double[m-ll];
                             for (k = 0; k < m-ll; k++) {
                                 w1[k] = work[k+nm12];
                                 w2[k] = work[k+nm13];
                             }
                             row1 = Math.max(1, m-ll+1);
                             array1 = new double[row1][ncvt];
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncvt; p++) {
                                     array1[k][p] = VT[ll-1+k][p];
                                 }
                             }
                             dlasr('L', 'V', 'B', m-ll+1, ncvt, w1, w2, array1, row1);
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncvt; p++) {
                                     VT[ll-1+k][p] = array1[k][p];
                                 }
                             }
                         } // if (ncvt > 0)
                         
                         if (nru > 0) {
                             w1 = new double[m-ll];
                             w2 = new double[m-ll];
                             for (k = 0; k < m-ll; k++) {
                                 w1[k] = work[k];
                                 w2[k] = work[k+n-1];
                             }
                             row1 = Math.max(1, nru);
                             array1 = new double[row1][m-ll+1];
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < m-ll+1; p++) {
                                     array1[k][p] = U[k][ll-1+p];
                                 }
                             }
                             dlasr('R', 'V', 'B', nru, m-ll+1, w1, w2, array1, row1);
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < m-ll+1; p++) {
                                     U[k][ll-1+p] = array1[k][p];
                                 }
                             }    
                         } // if (nru > 0)
                         
                         if (ncc > 0) {
                             w1 = new double[m-ll];
                             w2 = new double[m-ll];
                             for (k = 0; k < m-ll; k++) {
                                 w1[k] = work[k];
                                 w2[k] = work[k+n-1];
                             }
                             row1 = Math.max(1, m-ll+1);
                             array1 = new double[row1][ncc];
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncc; p++) {
                                     array1[k][p] = C[ll-1+k][p];
                                 }
                             }
                             dlasr('L', 'V', 'B', m-ll+1, ncc, w1, w2, array1, row1);
                             for (k = 0; k < row1; k++) {
                                 for (p = 0; p < ncc; p++) {
                                     C[ll-1+k][p] = array1[k][p];
                                 }
                             }    
                         } // if (ncc > 0)
                     } // else idir == 2
                 } // else shift[0] != 0
                 
                 // QR iteration finished, go back and check convergence
             } // loop1: while(true)
         } // if (n != 1)
         
         // All singular values converged, so make them positive
         for (i = 1; i <= n; i++) {
             if (d[i-1] < 0.0) {
                 d[i-1] = -d[i-1];
                 
                 // Change sign of singular vectors, if desired
                 if (ncvt > 0) {
                     for (k = 0; k < ncvt; k++) {
                         VT[i-1][k] = -1.0 * VT[i-1][k];
                     }
                 } // if (ncvt > 0)
             } // if (d[i-1] < 0.0)
         } // for (i = 1; i <= n; i++)
         
         // Sort the singular values into decreasing order (insertion sort on
         // singular values, but only one transposition per singular vector)
         for (i = 1; i <= n-1; i++) {
             // Scan for the smallest d[i-1]
             
             isub = 1;
             smin = d[0];
             for (j = 2; j <= n+1-i; j++) {
                 if (d[j-1] <= smin) {
                     isub = j;
                     smin = d[j-1];
                 }
             } // for (j = 2; j <= n+1-i; j++)
             if (isub != n+i-1) {
                 // Swap singular values and vectors
                 d[isub-1] = d[n-i];
                 d[n-i] = smin;
                 if (ncvt > 0) {
                     for (k = 0; k < ncvt; k++) {
                         temp = VT[isub-1][k];
                         VT[isub-1][k] = VT[n-i][k];
                         VT[n-i][k] = temp;
                     }
                 } // if (ncvt > 0)
                 
                 if (nru > 0) {
                     for (k = 0; k < nru; k++) {
                         temp = U[k][isub-1];
                         U[k][isub-1] = U[k][n-i];
                         U[k][n-i] = temp;
                     }
                 } // if (nru > 0)
                 
                 if (ncc > 0) {
                     for (k = 0; k < ncc; k++) {
                         temp = C[isub-1][k];
                         C[isub-1][k] = C[n-i][k];
                         C[n-i][k] = temp;
                     }
                 } // if (ncc > 0)
             } // if (isub != n+i-1)
         } // for (i = 1; i <= n-1; i++)
         return;
     } // dbdsqr
     
     /**
      * This is a port of version 3.2 LAPACK auxiliary routine DLASR Original DLASR created by Univ. of Tennessee, Univ.
      * of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
      * dlasr applies a sequence of plane rotations to a real matrix A, from either the left or the right.
      * when side = 'L', the transformation takes the form
      *     A = P * A
      * and when side = 'R', the transformation takes the form
      *     A = A * P**T
      * where P is an orthogonal matrix consisting of a sequence of z plane rotations, with z = m when side = 'L' and
      * z = n when side = 'R', and P**T is the transpose of P.
      * 
      * When direct = 'F' (Forward sequence), then
      *     P = P(z-2) * ... * P(1) * P(0)
      * and when direct = 'B' (Backward sequence), then
      *     P = P(0) * P(1) * ... * P(z-2)
      * where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
      *     R(k) = (  c(k)  s(k)  )
      *            ( -s(k)  c(k)  )
      *  
      *  When pivot = 'V' (Variable pivot), the rotation is performed for the plane (k,k+1), i.e., P(k) has the form
      *     P(k) = (  1                                                )
      *            (       ...                                         )
      *            (                1                                  )
      *            (                     c(k)   s(k)                   )
      *            (                    -s(k)   c(k)                   )
      *            (                                   1               )
      *            (                                         ...       )
      *            (                                                1  )
      *  where R(k) appears as a rank-2 modification to the identity matrix in rows and columns k and k+1.
      *  
      *  When pivot = 'T' (Top pivot), the rotation is performed for the plane (1,k+1), so P(k) has the form
      *      P(k) = (  c(k)                    s(k)                  )
      *             (        1                                       )
      *             (             ...                                )
      *             (                     1                          )
      *             ( -s(k)                    c(k)                  )
      *             (                                 1              )
      *             (                                     ...        )
      *             (                                             1  )
      * where R(k) appears in rows and column 1 and k+1
      * 
      * 
      * Similarly, when pivot = 'B' (Bottom pivot), the rotation is performed for the plane (k,z), giving 
      * P(k) the form
      *     P(k) = (  1                                          )
      *            (      ...                                    )
      *            (             1                               )
      *            (                  c(k)                  s(k) )
      *            (                        1                    )
      *            (                            ...              )
      *            (                                   1         )
      *            (                 -s(k)                  c(k) )
      * where R(k) appears in rows and columns k and z.  The rotations are performed without ever forming
      * P(k) explicitly.
      *             
      *
      * @param  side    input char Specifies whether the plane rotation matrix P is applied to A on the left or the
      *                 right. 
      *                 = 'L': Left, compute A = P*A 
      *                 = 'R': Right, compute A = A*P'
      * @param  pivot   input char Specifies the plane for which p[k] is a plane rotation matrix. 
      *                            = 'V': Variable pivot, the plane (k,k+1) 
      *                            = 'T': Top pivot, the plane (1,k+1) 
      *                            = 'B': Bottom pivot, the plane (k,z)
      * @param  direct  input char Specifies whether P is a forward or backward sequence of plane rotations. 
      *                            = 'F': Forward, p = p[z-2]*...*p[1]*p[0] 
      *                            = 'B': Backward, p = p[0]*p[1]*...*p[z-2]
      * @param  m       input int The number of rows of the matrix A. If m <= 1, an immediate return is effected.
      * @param  n       input int The number of columns of the matrix A. If n <= 1, an immediate return is effected.
      * @param  c       input double[]
      * @param  s       input double[] c and s are dimension (m-1) if side = 'L', (n-1) if side = 'R' c[k] and s[k]
      *                 contain the cosine and sine that define the matrix p[k]. The two by two plane rotation part of
      *                 the matrix p[k], R[k], has the form R[k] = ( c[k] s[k])
      *                                                            (-s[k] c[k])
      * @param  A       input/output double[][] of dimension lda by n. On entry, the m by n matrix A. On exit, A is
      *                 overwritten by P*A if side = 'L' or by A*P' if side = 'R'.
      * @param  lda     input int The leading dimension of the array A. lda >= max(1,m).
      */
     private void dlasr(char side, char pivot, char direct, int m, int n, double[] c, double[] s, double[][] A,
                        int lda) {
         int i;
         int info;
         int j;
         double ctemp;
         double stemp;
         double temp;

         // Test the input parameters
         info = 0;

         if ((side != 'L') && (side != 'l') && (side != 'R') && (side != 'r')) {
             info = 1;
         } else if ((pivot != 'V') && (pivot != 'v') && (pivot != 'T') && (pivot != 't') && (pivot != 'B') &&
                        (pivot != 'b')) {
             info = 2;
         } else if ((direct != 'F') && (direct != 'f') && (direct != 'B') && (direct != 'b')) {
             info = 3;
         } else if (m < 0) {
             info = 4;
         } else if (n < 0) {
             info = 5;
         } else if (lda < Math.max(1, m)) {
             info = 9;
         }

         if (info != 0) {
             MipavUtil.displayError("Error dlasr had info = " + info);

             return;
         }

         // Quick return if possible
         if ((m == 0) || (n == 0)) {
             return;
         }

         if ((side == 'L') || (side == 'l')) {

             // Form P*A
             if ((pivot == 'V') || (pivot == 'v')) {

                 if ((direct == 'F') || (direct == 'f')) {

                     for (j = 0; j < (m - 1); j++) {
                         ctemp = c[j];
                         stemp = s[j];

                         if ((ctemp != 1.0) || (stemp != 0.0)) {

                             for (i = 0; i < n; i++) {
                                 temp = A[j + 1][i];
                                 A[j + 1][i] = (ctemp * temp) - (stemp * A[j][i]);
                                 A[j][i] = (stemp * temp) + (ctemp * A[j][i]);
                             } // for (i = 0; i < n; i++)
                         } // if ((ctemp != 1.0) || (stemp != 0.0))
                     } // for (j = 0; j < m-1; j++)
                 } // if ((direct == 'F') || (direct == 'f'))
                 else if ((direct == 'B') || (direct == 'b')) {

                     for (j = m - 2; j >= 0; j--) {
                         ctemp = c[j];
                         stemp = s[j];

                         if ((ctemp != 1.0) || (stemp != 0.0)) {

                             for (i = 0; i < n; i++) {
                                 temp = A[j + 1][i];
                                 A[j + 1][i] = (ctemp * temp) - (stemp * A[j][i]);
                                 A[j][i] = (stemp * temp) + (ctemp * A[j][i]);
                             } // for (i = 0; i < n; i++)
                         } // if ((ctemp != 1.0) || (stemp != 0.0))
                     } // for (j = m-2; j >= 0; j--)
                 } // else if ((direct == 'B') || (direct == 'b'))
             } // if ((pivot == 'V') || (pivot == 'v'))
             else if ((pivot == 'T') || (pivot == 't')) {

                 if ((direct == 'F') || (direct == 'f')) {

                     for (j = 1; j < m; j++) {
                         ctemp = c[j - 1];
                         stemp = s[j - 1];

                         if ((ctemp != 1.0) || (stemp != 0.0)) {

                             for (i = 0; i < n; i++) {
                                 temp = A[j][i];
                                 A[j][i] = (ctemp * temp) - (stemp * A[0][i]);
                                 A[0][i] = (stemp * temp) + (ctemp * A[0][i]);
                             } // for (i = 0; i < n; i++)
                         } // if ((ctemp != 1.0) || (stemp != 0.0))
                     } // for (j = 1; j < m; j++)
                 } // if ((direct == 'F') || (direct == 'f'))
                 else if ((direct == 'B') || (direct == 'b')) {

                     for (j = m - 1; j >= 1; j--) {
                         ctemp = c[j - 1];
                         stemp = s[j - 1];

                         if ((ctemp != 1.0) || (stemp != 0.0)) {

                             for (i = 0; i < n; i++) {
                                 temp = A[j][i];
                                 A[j][i] = (ctemp * temp) - (stemp * A[0][i]);
                                 A[0][i] = (stemp * temp) + (ctemp * A[0][i]);
                             } // for (i = 0; i < n; i++)
                         } // if ((ctemp != 1.0) || (stemp != 0.0))
                     } // for (j = m-1; j >= 1; j--)
                 } // else if ((direct == 'B') || (direct == 'b'))
             } // else if ((pivot == 'T') || (pivot == 't'))
             else if ((pivot == 'B') || (pivot == 'b')) {

                 if ((direct == 'F') || (direct == 'f')) {

                     for (j = 0; j < (m - 1); j++) {
                         ctemp = c[j];
                         stemp = s[j];

                         if ((ctemp != 1.0) || (stemp != 0.0)) {

                             for (i = 0; i < n; i++) {
                                 temp = A[j][i];
                                 A[j][i] = (stemp * A[m - 1][i]) + (ctemp * temp);
                                 A[m - 1][i] = (ctemp * A[m - 1][i]) - (stemp * temp);
                             } // for (i = 0; i < n; i++)
                         } // if ((ctemp != 1.0) || (stemp != 0.0))
                     } // for (j = 0; j < m-1; j++)
                 } // if ((direct == 'F') || (direct == 'f'))
                 else if ((direct == 'B') || (direct == 'b')) {

                     for (j = m - 2; j >= 0; j--) {
                         ctemp = c[j];
                         stemp = s[j];

                         if ((ctemp != 1.0) || (stemp != 0.0)) {

                             for (i = 0; i < n; i++) {
                                 temp = A[j][i];
                                 A[j][i] = (stemp * A[m - 1][i]) + (ctemp * temp);
                                 A[m - 1][i] = (ctemp * A[m - 1][i]) - (stemp * temp);
                             } // for (i = 0; i < n; i++)
                         } // if ((ctemp != 1.0) || (stemp != 0.0))
                     } // for (j = m-2; j >= 0; j--)
                 } // else if (direct == 'B') || (direct == 'b'))
             } // else if ((pivot == 'B') || (pivot == 'b'))
         } // if ((side == 'L') || (side == 'l'))
         else if ((side == 'R') || (side == 'r')) {

             // Form A * P'
             if ((pivot == 'V') || (pivot == 'v')) {

                 if ((direct == 'F') || (direct == 'f')) {

                     for (j = 0; j < (n - 1); j++) {
                         ctemp = c[j];
                         stemp = s[j];

                         if ((ctemp != 1.0) || (stemp != 0.0)) {

                             for (i = 0; i < m; i++) {
                                 temp = A[i][j + 1];
                                 A[i][j + 1] = (ctemp * temp) - (stemp * A[i][j]);
                                 A[i][j] = (stemp * temp) + (ctemp * A[i][j]);
                             } // for (i = 0; i < m; i++)
                         } // if ((ctemp != 1.0) || (stemp != 0.0))
                     } // for (j = 0; j < n-1; j++)
                 } // if ((direct == 'F') || (direct == 'f'))
                 else if ((direct == 'B') || (direct == 'b')) {

                     for (j = n - 2; j >= 0; j--) {
                         ctemp = c[j];
                         stemp = s[j];

                         if ((ctemp != 1.0) || (stemp != 0.0)) {

                             for (i = 0; i < m; i++) {
                                 temp = A[i][j + 1];
                                 A[i][j + 1] = (ctemp * temp) - (stemp * A[i][j]);
                                 A[i][j] = (stemp * temp) + (ctemp * A[i][j]);
                             } // for (i = 0; i < m; i++)
                         } // if ((ctemp != 1.0) || (stemp != 0.0))
                     } // for (j = n-2; j >= 0; j--)
                 } // else if ((direct == 'B') || (direct == 'b'))
             } // if ((pivot == 'V') || (pivot == 'v'))
             else if ((pivot == 'T') || (pivot == 't')) {

                 if ((direct == 'F') || (direct == 'f')) {

                     for (j = 1; j < n; j++) {
                         ctemp = c[j - 1];
                         stemp = s[j - 1];

                         if ((ctemp != 1.0) || (stemp != 0.0)) {

                             for (i = 0; i < m; i++) {
                                 temp = A[i][j];
                                 A[i][j] = (ctemp * temp) - (stemp * A[i][0]);
                                 A[i][0] = (stemp * temp) + (ctemp * A[i][0]);
                             } // for (i = 0; i < m; i++)
                         } // if ((ctemp != 1.0) || (stemp != 0.0))
                     } // for (j = 1; j < n; j++)
                 } // if ((direct == 'F') || (direct == 'f'))
                 else if ((direct == 'B') || (direct == 'b')) {

                     for (j = n - 1; j >= 1; j--) {
                         ctemp = c[j - 1];
                         stemp = s[j - 1];

                         if ((ctemp != 1.0) || (stemp != 0.0)) {

                             for (i = 0; i < m; i++) {
                                 temp = A[i][j];
                                 A[i][j] = (ctemp * temp) - (stemp * A[i][0]);
                                 A[i][0] = (stemp * temp) + (ctemp * A[i][0]);
                             } // for (i = 0; i < m; i++)
                         } // if ((ctemp != 1.0) || (stemp != 0.0))
                     } // for (j = n-1; j >= 1; j--)
                 } // else if ((direct == 'B') || (direct == 'b'))
             } // else if ((pivot == 'T') || (pivot == 't'))
             else if ((pivot == 'B') || (pivot == 'b')) {

                 if ((direct == 'F') || (direct == 'f')) {

                     for (j = 0; j < (n - 1); j++) {
                         ctemp = c[j];
                         stemp = s[j];

                         if ((ctemp != 1.0) || (stemp != 0.0)) {

                             for (i = 0; i < m; i++) {
                                 temp = A[i][j];
                                 A[i][j] = (stemp * A[i][n - 1]) + (ctemp * temp);
                                 A[i][n - 1] = (ctemp * A[i][n - 1]) - (stemp * temp);
                             } // for (i = 0; i < m; i++)
                         } // if ((ctemp != 1.0) || (stemp != 0.0))
                     } // for (j = 0; j < n-1; j++)
                 } // if ((direct == 'F') || (direct == 'f'))
                 else if ((direct == 'B') || (direct == 'b')) {

                     for (j = n - 2; j >= 0; j--) {
                         ctemp = c[j];
                         stemp = s[j];

                         if ((ctemp != 1.0) || (stemp != 0.0)) {

                             for (i = 0; i < m; i++) {
                                 temp = A[i][j];
                                 A[i][j] = (stemp * A[i][n - 1]) + (ctemp * temp);
                                 A[i][n - 1] = (ctemp * A[i][n - 1]) - (stemp * temp);
                             } // for (i = 0; i < m; i++)
                         } // if ((ctemp != 1.0) || (stemp != 0.0))
                     } // for (j = n-2; j >= 0; j--)
                 } // else if ((direct == 'B') || (direct == 'b'))
             } // else if ((pivot == 'B') || (pivot == 'b'))
         } // else if ((side == 'R') || (side == 'r'))

         return;
     } // dlasr
     
     /**
      * This is a port of version 3.2 LAPACK auxiliary routine DLASV2 Original DLASV2 created by Univ. of Tennessee, Univ.
      * of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
      * dlasv2 computes the singular value decomposition of a 2-by-2 triangular matrix 
      *  [ f g ]
      *  [ 0 h ]. 
      * On return, abs(ssmax[0]) is the larger singular value, abs(ssmin[0]) is the smaller singular value,
      * and (csl[0],snl[0]) and (csr[0],snr[0]) are the left and right singular vectors for abs(ssmax[0]),
      * giving the decomposition
      *  [ csl snl] [f  g ] [ csr -snr] = [ ssmax 0 ] 
      *  [-snl csl] [0  h ] [ snr csr]    [ 0 ssmin ].
      *
      * @param  f      input double The (0,0) element of a 2-by-2 matrix.
      * @param  g      input double The (0,1) element of a 2-by-2 matrix.
      * @param  h      input double The (1,1) element of a 2-by-2 matrix.
      * @param  ssmin  output double[] abs(ssmin[0]) is the smaller singular value.
      * @param  ssmax  output double[] abs(ssmax[0]) is the larger singular value.
      * @param  snr    output double[]
      * @param  csr    output double[] The vector (csr[0],snr[0]) is a unit right singular vector for the singular value
      *                abs(ssmax[0]).
      * @param  snl    output double[]
      * @param  csl    output double[] The vector (csl[0],snl[0]) is a unit left singular vector for the singular value
      *                abs(ssmax[0]).
      * Further details: Any input parameter may be aliased with any output parameter.
      *
      * <p>Barring over/underflow and assuming a guard digit in subtraction, all output quantities are
      *    correct to within a few units in the last place (ulps).</p>
      *
      * <p>In IEEE arithmetic, the code works correctly if one matrix element is infinite.</p>
      *
      * <p>Overflow will not occur unless the largest singular value itself overflows or is within a few
      *    ulps of overflow. (On machines with partial overflow, like the Cray, overflow may occur if the
      *    largest singular value is within a factor of 2 of overflow.)</p>
      *
      * <p>Underflow is harmless if underflow is gradual. Otherwise, results may correspond to a matrix
      *    modified by perturbations of size near the underflow threshold.</p>
      */
     private void dlasv2(double f, double g, double h, double[] ssmin, double[] ssmax, double[] snr, double[] csr,
                         double[] snl, double[] csl) {
         boolean gasmal;
         boolean swap;
         int pmax;
         double a;
         double clt = 0.0;
         double crt = 0.0;
         double d;
         double fa;
         double ft;
         double ga;
         double gt;
         double ha;
         double ht;
         double L;
         double m;
         double mm;
         double r;
         double s;
         double slt = 0.0;
         double srt = 0.0;
         double t;
         double temp;
         double tsign;
         double tt;

         ft = f;
         fa = Math.abs(ft);
         ht = h;
         ha = Math.abs(h);

         // pmax points to the maximum value of the matrix
         // pmax = 1 if f largest in absolute value
         // pmax = 2 if g largest in absolute value
         // pmax = 3 if h largest in absolute value
         pmax = 1;
         swap = (ha > fa);

         if (swap) {
             pmax = 3;
             temp = ft;
             ft = ht;
             ht = temp;
             temp = fa;
             fa = ha;
             ha = temp;
             // Now fa >= ha
         } // if (swap)

         gt = g;
         ga = Math.abs(gt);

         if (ga == 0.0) {

             // Diagonal matrix
             ssmin[0] = ha;
             ssmax[0] = fa;
             clt = 1.0;
             crt = 1.0;
             slt = 0.0;
             srt = 0.0;
         } // if (ga == 0.0)
         else {
             gasmal = true;

             if (ga > fa) {
                 pmax = 2;

                 if ((fa / ga) < dlamch('E')) {

                     // Case of very large ga
                     gasmal = false;
                     ssmax[0] = ga;

                     if (ha > 1.0) {
                         ssmin[0] = fa / (ga / ha);
                     } else {
                         ssmin[0] = (fa / ga) * ha;
                     }

                     clt = 1.0;
                     slt = ht / gt;
                     srt = 1.0;
                     crt = ft / gt;
                 } // if ((fa/ga) < dlamch('E'))
             } // if (ga > fa)

             if (gasmal) {

                 // Normal case
                 d = fa - ha;

                 if (d == fa) {

                     // Copes with infinite F or H
                     L = 1.0;
                 } // if (d == fa)
                 else {
                     L = d / fa;
                 } // else

                 // Note that 0 <= L <= 1
                 m = gt / ft;

                 // Note that abs(m) <= 1/macheps
                 t = 2.0 - L;

                 // Note that t >= 1
                 mm = m * m;
                 tt = t * t;
                 s = Math.sqrt(tt + mm);

                 // Note that 1 <= s <= 1 + 1/macheps
                 if (L == 0.0) {
                     r = Math.abs(m);
                 } else {
                     r = Math.sqrt((L * L) + mm);
                 }

                 // Note that 0 <= r <= 1 + 1/macheps
                 a = 0.5 * (s + r);

                 // Note that 1 <= a <= 1 + abs(m);
                 ssmin[0] = ha / a;
                 ssmax[0] = fa * a;

                 if (mm == 0.0) {

                     // Note that m is very tiny
                     if (L == 0.0) {

                         if (((ft >= 0.0) && (gt >= 0.0)) || ((ft < 0.0) && (gt < 0.0))) {
                             t = 2.0;
                         } else {
                             t = -2.0;
                         }
                     } // if (L == 0.0)
                     else if (ft >= 0.0) {
                         t = (gt / Math.abs(d)) + (m / t);
                     } // else if (ft >= 0.0)
                     else {
                         t = (-gt / Math.abs(d)) + (m / t);
                     } // else
                 } // if (mm == 0.0)
                 else {
                     t = ((m / (s + t)) + (m / (r + L))) * (1.0 + a);
                 }

                 L = Math.sqrt((t * t) + 4.0);
                 crt = 2.0 / L;
                 srt = t / L;
                 clt = (crt + (srt * m)) / a;
                 slt = (ht / ft) * srt / a;
             } // if (gasmal)
         } // else

         if (swap) {
             csl[0] = srt;
             snl[0] = crt;
             csr[0] = slt;
             snr[0] = clt;
         } // if (swap)
         else {
             csl[0] = clt;
             snl[0] = slt;
             csr[0] = crt;
             snr[0] = srt;
         } // else

         // Correct signs of ssmax and ssmin
         tsign = 1.0;

         if (pmax == 1) {

             if (csr[0] < 0.0) {
                 tsign = -tsign;
             }

             if (csl[0] < 0.0) {
                 tsign = -tsign;
             }

             if (f < 0) {
                 tsign = -tsign;
             }
         } // if (pmax == 1)

         if (pmax == 2) {

             if (snr[0] < 0.0) {
                 tsign = -tsign;
             }

             if (csl[0] < 0.0) {
                 tsign = -tsign;
             }

             if (g < 0.0) {
                 tsign = -tsign;
             }
         } // if (pmax == 2)

         if (pmax == 3) {

             if (snr[0] < 0.0) {
                 tsign = -tsign;
             }

             if (snl[0] < 0.0) {
                 tsign = -tsign;
             }

             if (h < 0.0) {
                 tsign = -tsign;
             }
         } // if (pmax == 3)

         if (tsign >= 0.0) {
             ssmax[0] = Math.abs(ssmax[0]);
         } else {
             ssmax[0] = -Math.abs(ssmax[0]);
         }

         if (f < 0.0) {
             tsign = -tsign;
         }

         if (h < 0.0) {
             tsign = -tsign;
         }

         if (tsign >= 0.0) {
             ssmin[0] = Math.abs(ssmin[0]);
         } else {
             ssmin[0] = -Math.abs(ssmin[0]);
         }

         return;
     } // dlasv2
     
     /**  This is a port of version 3.2 LAPACK auxiliary routine DLAS2.  Original DLAS2 created by 
      *   Univ. of Tennessee, Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., 
      *   November, 2006
     *  Purpose
     *  =======
     *
     *  DLAS2  computes the singular values of the 2-by-2 matrix
     *     [  F   G  ]
     *     [  0   H  ].
     *  On return, SSMIN is the smaller singular value and SSMAX is the
     *  larger singular value.
     *
     *  Arguments
     *  =========
     *
     *  F       (input) DOUBLE PRECISION
     *          The (1,1) element of the 2-by-2 matrix.
     *
     *  G       (input) DOUBLE PRECISION
     *          The (1,2) element of the 2-by-2 matrix.
     *
     *  H       (input) DOUBLE PRECISION
     *          The (2,2) element of the 2-by-2 matrix.
     *
     *  SSMIN   (output) DOUBLE PRECISION
     *          The smaller singular value.
     *
     *  SSMAX   (output) DOUBLE PRECISION
     *          The larger singular value.
     *
     *  Further Details
     *  ===============
     *
     *  Barring over/underflow, all output quantities are correct to within
     *  a few units in the last place (ulps), even in the absence of a guard
     *  digit in addition/subtraction.
     *
     *  In IEEE arithmetic, the code works correctly if one matrix element is
     *  infinite.
     *
     *  Overflow will not occur unless the largest singular value itself
     *  overflows, or is within a few ulps of overflow. (On machines with
     *  partial overflow, like the Cray, overflow may occur if the largest
     *  singular value is within a factor of 2 of overflow.)
     *
     *  Underflow is harmless if underflow is gradual. Otherwise, results
     *  may correspond to a matrix modified by perturbations of size near
     *  the underflow threshold.
     */
     private void dlas2(double f[], double g[], double h[], double ssmin[], double ssmax[]) {
         double as;
         double at;
         double au;
         double c;
         double fa;
         double fhmn;
         double fhmx;
         double ga;
         double ha;
         double temp;
         
         fa = Math.abs(f[0]);
         ga = Math.abs(g[0]);
         ha = Math.abs(h[0]);
         fhmn = Math.min(fa, ha);
         fhmx = Math.max(fa, ha);
         if (fhmn == 0.0) {
             ssmin[0] = 0.0;
             if (fhmx == 0.0) {
                 ssmax[0] = ga;
             }
             else {
                 temp = Math.min(fhmx, ga)/Math.max(fhmx, ga);
                 ssmax[0] = Math.max(fhmx, ga) * Math.sqrt(1.0 + temp*temp);
              }
         } // if (fhmn == 0.0)
         else { // fhmn != 0.0
             if (ga < fhmx) {
                 as = 1.0 + fhmn/fhmx;
                 at = (fhmx - fhmn)/fhmx;
                 au = ga/fhmx;
                 au = au * au;
                 c = 2.0/(Math.sqrt(as*as+au) + Math.sqrt(at*at+au));
                 ssmin[0] = fhmn * c;
                 ssmax[0] = fhmx/c;
             } // if (ga < fhmx)
             else { // ga >= fhmx
                 au = fhmx/ga;
                 if (au == 0.0) {
                     // Avoid possible harmful underflow if exponent range
                     // asymmetric (true ssmin[0] may not underflow even if 
                     // au underflows.)
                     ssmin[0] = (fhmn * fhmx) / ga;
                     ssmax[0] = ga;
                 } // if (au == 0.0)
                 else { // au != 0.0
                     as = 1.0 + fhmn/fhmx;
                     at = (fhmx - fhmn)/fhmx;
                     c = 1.0/(Math.sqrt(1.0 + (as*au)*(as*au)) + Math.sqrt(1.0 + (at*au)*(at*au)));
                     ssmin[0] = (fhmn*c)*au;
                     ssmin[0] = ssmin[0] + ssmin[0];
                     ssmax[0] = ga / (c+c);
                 } // else au != 0.0
             } // else ga >= fhmx
         } // else fhmn != 0.0
         return;
     } // dlas2
     
     /**
      * This is a port of the 3/11/78 linpack routine drot Original code written by Jack Dongarra.
      *
      * @param  n     int
      * @param  dx    double[]
      * @param  incx  int
      * @param  dy    double[]
      * @param  incy  int
      * @param  c     double
      * @param  s     double
      */
     private void drot(int n, double[] dx, int incx, double[] dy, int incy, double c, double s) {
         double dtemp;
         int i;
         int ix;
         int iy;

         if (n <= 0) {
             return;
         }

         if ((incx != 1) || (incy != 1)) {

             // Code for unequal increments or equal increments not equal to 1
             ix = 0;
             iy = 0;

             if (incx < 0) {
                 ix = (-n + 1) * incx;
             }

             if (incy < 0) {
                 iy = (-n + 1) * incy;
             }

             for (i = 1; i <= n; i++) {
                 dtemp = (c * dx[ix]) + (s * dy[iy]);
                 dy[iy] = (c * dy[iy]) - (s * dx[ix]);
                 dx[ix] = dtemp;
                 ix = ix + incx;
                 iy = iy + incy;
             } // for (i = 1; i <= n; i++)

             return;
         } // if ((incx != 1) || (incy != 1))

         // Code for both increments equal to 1
         for (i = 0; i < n; i++) {
             dtemp = (c * dx[i]) + (s * dy[i]);
             dy[i] = (c * dy[i]) - (s * dx[i]);
             dx[i] = dtemp;
         } // for (i = 0; i < n; i++)

         return;
     } // drot

     
     /**
      * This is a port of version 3.2 LAPACK auxiliary routine DLARTG Original DLARTG created by Univ. of Tennessee, Univ.
      * of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
      * dlartg generates a plane rotation so that 
      * [  cs  sn ] . [ f ] = [ r ] where cs*cs + sn*sn = 1.
      * [ -sn  cs ]   [ g ]   [ 0 ] 
      * If g = 0, then cs = 1 and sn = 0. 
      * If f = 0 and g != 0, then cs = 0 and sn = 1 without doing any floating point operations (saves
      * work in dbdsqr when there are zeros on the diagonal). 
      * If f exceeds g in magnitude, then cs will be positive.
      *
      * @param  f   input double The first component of the vector to be rotated.
      * @param  g   input double The second component of the vector to be rotated.
      * @param  cs  output double[] The cosine of the rotation.
      * @param  sn  output double[] The sine of the rotation.
      * @param  r   output double[] The nonzero component of the rotated vector.
      */
     private void dlartg(double f, double g, double[] cs, double[] sn, double[] r) {
         int count;
         int i;
         double eps;
         double f1;
         double g1;
         double scale;

         if (first_dlartg) {
             first_dlartg = false;
             safmin = dlamch('S');
             eps = dlamch('E');
             safmn2 = Math.pow(dlamch('B'), (int) (Math.log(safmin / eps) / Math.log(dlamch('B')) / 2.0));
             safmx2 = 1.0 / safmn2;
         } // if (first_dlartg)

         if (g == 0.0) {
             cs[0] = 1.0;
             sn[0] = 0.0;
             r[0] = f;
         } else if (f == 0.0) {
             cs[0] = 0.0;
             sn[0] = 1.0;
             r[0] = g;
         } else {
             f1 = f;
             g1 = g;
             scale = Math.max(Math.abs(f1), Math.abs(g1));

             if (scale >= safmx2) {
                 count = 0;

                 do {
                     count = count + 1;
                     f1 = f1 * safmn2;
                     g1 = g1 * safmn2;
                     scale = Math.max(Math.abs(f1), Math.abs(g1));
                 } while (scale >= safmx2);

                 r[0] = Math.sqrt((f1 * f1) + (g1 * g1));
                 cs[0] = f1 / r[0];
                 sn[0] = g1 / r[0];

                 for (i = 1; i <= count; i++) {
                     r[0] = r[0] * safmx2;
                 }
             } // if (scale >= safmx2)
             else if (scale <= safmn2) {
                 count = 0;

                 do {
                     count = count + 1;
                     f1 = f1 * safmx2;
                     g1 = g1 * safmx2;
                     scale = Math.max(Math.abs(f1), Math.abs(g1));
                 } while (scale <= safmn2);

                 r[0] = Math.sqrt((f1 * f1) + (g1 * g1));
                 cs[0] = f1 / r[0];
                 sn[0] = g1 / r[0];

                 for (i = 1; i <= count; i++) {
                     r[0] = r[0] * safmn2;
                 }
             } // else if (scale <= safmn2)
             else {
                 r[0] = Math.sqrt((f1 * f1) + (g1 * g1));
                 cs[0] = f1 / r[0];
                 sn[0] = g1 / r[0];
             }

             if ((Math.abs(f) > Math.abs(g)) && (cs[0] < 0.0)) {
                 cs[0] = -cs[0];
                 sn[0] = -sn[0];
                 r[0] = -r[0];
             }
         }

         return;
     } // dlartg
     
     /** This is a port of version 3.2 LAPACK routine DLASQ1.                                 --
     *
     *  -- Contributed by Osni Marques of the Lawrence Berkeley National   --
     *  -- Laboratory and Beresford Parlett of the Univ. of California at  --
     *  -- Berkeley                                                        --
     *  -- November 2008                                                   --
     *
     *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
     *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
     *
     *     .. Scalar Arguments ..
           INTEGER            INFO, N
     *     ..
     *     .. Array Arguments ..
           DOUBLE PRECISION   D( * ), E( * ), WORK( * )
     *     ..
     *
     *  Purpose
     *  =======
     *
     *  DLASQ1 computes the singular values of a real N-by-N bidiagonal
     *  matrix with diagonal D and off-diagonal E. The singular values
     *  are computed to high relative accuracy, in the absence of
     *  denormalization, underflow and overflow. The algorithm was first
     *  presented in
     *
     *  "Accurate singular values and differential qd algorithms" by K. V.
     *  Fernando and B. N. Parlett, Numer. Math., Vol-67, No. 2, pp. 191-230,
     *  1994,
     *
     *  and the present implementation is described in "An implementation of
     *  the dqds Algorithm (Positive Case)", LAPACK Working Note.
     *
     *  Arguments
     *  =========
     *
     *  N     (input) INTEGER
     *        The number of rows and columns in the matrix. N >= 0.
     *
     *  D     (input/output) DOUBLE PRECISION array, dimension (N)
     *        On entry, D contains the diagonal elements of the
     *        bidiagonal matrix whose SVD is desired. On normal exit,
     *        D contains the singular values in decreasing order.
     *
     *  E     (input/output) DOUBLE PRECISION array, dimension (N)
     *        On entry, elements E(1:N-1) contain the off-diagonal elements
     *        of the bidiagonal matrix whose SVD is desired.
     *        On exit, E is overwritten.
     *
     *  WORK  (workspace) DOUBLE PRECISION array, dimension (4*N)
     *
     *  INFO  (output) INTEGER
     *        = 0: successful exit
     *        < 0: if INFO = -i, the i-th argument had an illegal value
     *        > 0: the algorithm failed
     *             = 1, a split was marked by a positive value in E
     *             = 2, current block of Z not diagonalized after 30*N
     *                  iterations (in inner while loop)
     *             = 3, termination criterion of outer while loop not met 
     *                  (program created more than N unreduced blocks)
     */
     private void dlasq1(int n, double d[], double e[], double work[], int info[]) {
         int i;
         int iinfo[] = new int[1];
         double eps;
         double scale;
         double safmin;
         double sigmn[] = new double[1];
         double sigmx[] = new double[1];
         double d0[] = new double[1];
         double e0[] = new double[1];
         double d1[] = new double[1];
         double work1[][] = new double[2*n-1][1];
         double dmat[][] = new double[d.length][1];
         
         info[0] = 0;
         if (n < 0) {
             info[0] = -2;
             MipavUtil.displayError("Error dlasq1 had n < 0");
             return;
         }
         else if (n == 0) {
             return;
         }
         else if (n == 1) {
             d[0] = Math.abs(d[0]);
             return;
         }
         else if (n == 2) {
             d0[0] = d[0];
             e0[0] = e[0];
             d1[0] = d[1];
             dlas2(d0, e0, d1, sigmn, sigmx);
             d[0] = sigmx[0];
             d[1] = sigmn[0];
             return;
         }
         
         // Estimate the largest singular value.
         sigmx[0] = 0.0;
         for (i = 1; i <= n-1; i++) {
             d[i-1] = Math.abs(d[i-1]);
             sigmx[0] = Math.max(sigmx[0], Math.abs(e[i-1]));
         }
         d[n-1] = Math.abs(d[n-1]);
         
         // Early return if simgx[0] is zero (matrix is already diagonal).
         if (sigmx[0] == 0) {
             dlasrt('D', n, d, iinfo);
             return;
         }
         
         for (i = 1; i <= n; i++) {
             sigmx[0] = Math.max(sigmx[0], d[i-1]);
         }
         
         // Copy d and e int work (in the Z format) and scale (squaring the
         // input data makes scaling by a power of the radix pointless).
         eps = dlamch('P'); // Precision
         safmin = dlamch('S'); // Safe minimum
         scale = Math.sqrt(eps/safmin);
         for (i = 0; i < n; i++) {
             work1[2*i][0] = d[i];
         }
         for (i = 0; i < n-1; i++) {
             work1[2*i + 1][0] = e[i];
         }
         dlascl('G', 0, 0, sigmx[0], scale, 2*n-1, 1, work1, 2*n-1, iinfo);
         for (i = 0; i < 2*n-1; i++) {
             work[i] = work1[i][0];
         }
         
         // Compute the q's and e's
         
         for (i = 1; i <= 2*n - 1; i++) {
             work[i-1] = work[i-1] * work[i-1];
         }
         work[2*n-1] = 0.0;
         dOrg = new double[2*n];
         dSort = new double[4*n];
         int p;
         for (p = 0; p < 2*n; p++) {
             dOrg[p] = work[p];
         }
         
         dlasq2(n, work, info);
         for (p = 0; p < 4*n; p++) {
             dSort[p] = work[p];
         }
         
         if (info[0] == 0) {
             for (i = 1; i <= n; i++) {
                 dmat[i-1][0] = Math.sqrt(work[i-1]);
             }
             dlascl('G', 0, 0, scale, sigmx[0], n, 1, dmat, n, iinfo);
             for (i = 0; i < n; i++) {
                 d[i] = dmat[i][0];
             }
         } // if (info[0] == 0)
         
         return;
     } // dlasq1

     
     /** This is a port of version 3.2 LAPACK routine DLASQ2.
     *  -- Contributed by Osni Marques of the Lawrence Berkeley National   --
     *  -- Laboratory and Beresford Parlett of the Univ. of California at  --
     *  -- Berkeley                                                        --
     *  -- November 2008                                                   --
     *
     *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
     *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
     *
     *     .. Scalar Arguments ..
           INTEGER            INFO, N
     *     ..
     *     .. Array Arguments ..
           DOUBLE PRECISION   Z( * )
     *     ..
     *
     *  Purpose
     *  =======
     *
     *  DLASQ2 computes all the eigenvalues of the symmetric positive 
     *  definite tridiagonal matrix associated with the qd array Z to high
     *  relative accuracy are computed to high relative accuracy, in the
     *  absence of denormalization, underflow and overflow.
     *
     *  To see the relation of Z to the tridiagonal matrix, let L be a
     *  unit lower bidiagonal matrix with subdiagonals Z(2,4,6,,..) and
     *  let U be an upper bidiagonal matrix with 1's above and diagonal
     *  Z(1,3,5,,..). The tridiagonal is L*U or, if you prefer, the
     *  symmetric tridiagonal to which it is similar.
     *
     *  Note : DLASQ2 defines a logical variable, IEEE, which is true
     *  on machines which follow ieee-754 floating-point standard in their
     *  handling of infinities and NaNs, and false otherwise. This variable
     *  is passed to DLASQ3.
     *
     *  Arguments
     *  =========
     *
     *  N     (input) INTEGER
     *        The number of rows and columns in the matrix. N >= 0.
     *
     *  Z     (input/output) DOUBLE PRECISION array, dimension ( 4*N )
     *        On entry Z holds the qd array. On exit, entries 1 to N hold
     *        the eigenvalues in decreasing order, Z( 2*N+1 ) holds the
     *        trace, and Z( 2*N+2 ) holds the sum of the eigenvalues. If
     *        N > 2, then Z( 2*N+3 ) holds the iteration count, Z( 2*N+4 )
     *        holds NDIVS/NIN^2, and Z( 2*N+5 ) holds the percentage of
     *        shifts that failed.
     *
     *  INFO  (output) INTEGER
     *        = 0: successful exit
     *        < 0: if the i-th argument is a scalar and had an illegal
     *             value, then INFO = -i, if the i-th argument is an
     *             array and the j-entry had an illegal value, then
     *             INFO = -(i*100+j)
     *        > 0: the algorithm failed
     *              = 1, a split was marked by a positive value in E
     *              = 2, current block of Z not diagonalized after 30*N
     *                   iterations (in inner while loop)
     *              = 3, termination criterion of outer while loop not met 
     *                   (program created more than N unreduced blocks)
     *                   
     Dear LAPACK:

    The introductory comments to DLASQ2 state that if INFO = 2, the algorithm failed because
    the current block of Z was not diagonalized after 30*N iterations (in inner while loop).
    The inner while loop is only executed if NBIG > 0.  NBIG = 30 * (N0 -I0 + 1) so the
    inner while loop is only executed if I0 <= N0.  In order for the inner while loop not to fail
    I0 must become greater than N0 causing a GO TO 150 to be executed. Inside the inner while loop
    the call to DLASQ3 only uses I0 and N0 as input arguments leaving them unchanged.  In the
    IF (PP  .EQ. 0 .AND. N0 - I0 .GE. 3) clause,  SPLT can only be set to I0 - 1 or I4/4.  I4 has
    a maximum value of 4*(N0-3), so the statement I0 = SPLT +1 either leaves I0 unchanged or increases
    it up to a maximum value of N0 - 2.  In summary, the inner while loop is entered by values of
    I0 <= N0, but the execution of the inner while loop can only increase I0 up to a maximum of N0 - 2,
    so the statement IF (I0 .GT. N0) GO TO 150 needed to prevent an inner loop failure will never be
    executed.  Therefore, the inner while loop always fails.

                                     Sincerely,

                                  William Gandler

     *
     *  Further Details
     *  ===============
     *  Local Variables: I0:N0 defines a current unreduced segment of Z.
     *  The shifts are accumulated in SIGMA. Iteration count is in ITER.
     *  Ping-pong is controlled by PP (alternates between 0 and 1).
     */
     private void dlasq2(int n, double z[], int info[]) {
         double cbias = 1.50;
         boolean ieee;
         int i0;
         int i4;
         int iinfo[] = new int[1];
         int ipn4;
         int iter[] = new int[1];
         int iwhila;
         int iwhilb;
         int k;
         int kmin;
         int n0;
         int nbig;
         int ndiv[] = new int[1];
         int nfail[] = new int[1];
         int pp[] = new int[1];
         int splt;
         int ttype[] = new int[1];
         double d;
         double dee;
         double deemin;
         double desig[] = new double[1];
         double dmin[] = new double[1];
         double dmin1[] = new double[1];
         double dmin2[] = new double[1];
         double dn[] = new double[1];
         double dn1[] = new double[1];
         double dn2[] = new double[1];
         double e;
         double emax;
         double emin;
         double eps;
         double g[] = new double[1];
         double oldemn;
         double qmax;
         double qmin;
         double s;
         double safmin;
         double sigma[] = new double[1];
         double t;
         double tau[] = new double[1];
         double temp;
         double tol;
         double tol2;
         double trace;
         double zmax;
         String name;
         String opts;
         
         // Test the input arguments
         // (in case dlasq2 is not called by dlasq1)
         info[0] = 0;
         eps = dlamch('P'); // precision
         safmin = dlamch('S'); // Safe minimum
         tol = 100 * eps;
         tol2 = tol * tol;
         
         if (n < 0) {
             info[0] = -1;
             MipavUtil.displayError("Error dlasq2 had n < 0");
             return;
         }
         else if (n == 0) {
             return;
         }
         else if (n == 1) {
             // 1-by-1 case
             if (z[0] < 0.0) {
                 info[0] = -201;
                 MipavUtil.displayError("Error dlsaq2 had z[0] < 0.0");
             }
             return;
         } // else if (n == 1)
         else if (n == 2) {
             // 2-by-2 case
             if ((z[1] < 0.0) || (z[2] < 0.0)) {
                 info[0] = -2;
                 MipavUtil.displayError("Error dlasq2 had z[1] < 0.0 or z[2] < 0.0");
                 return;
             }
             else if (z[2] > z[0]) {
                 d = z[2];
                 z[2] = z[0];
                 z[0] = d;
             }
             z[4] = z[0] + z[1] + z[2];
             if (z[1] > z[2]*tol2) {
                 t = 0.5 *((z[0] - z[2]) + z[1]);
                 s = z[2] * (z[1]/t);
                 if (s <= t) {
                     s = z[2] * (z[1]/(t*(1.0 + Math.sqrt(1.0 + s/t))));
                 }
                 else {
                     s = z[2] * (z[1]/(t + Math.sqrt(t)*Math.sqrt(t+s)));
                 }
                 t = z[0] + (s + z[1]);
                 z[2] = z[2] * (z[0]/t);
                 z[0] = t;
             } // if (z[1] > z[2]*tol2)
             z[1] = z[2];
             z[5] = z[1] + z[0];
             return;
         } // else if (n == 2)
         
         // Check for negative data and compute sums of q's and e's.
         
         z[2*n-1] = 0.0;
         emin = z[1];
         qmax = 0.0;
         zmax = 0.0;
         d = 0.0;
         e = 0.0;
         
         for (k = 1; k <= 2*(n-1); k += 2) {
             if (z[k-1] < 0.0) {
                 info[0] = -(200+k);
                 MipavUtil.displayError("Error dlasq2 had info[0] = " + info[0]);
                 return;
             }
             else if (z[k] < 0.0) {
                 info[0] = -(200+k+1);
                 MipavUtil.displayError("Error dlasq2 had info[0] = " + info[0]);
                 return;
             }
             d = d + z[k-1];
             e = e + z[k];
             qmax = Math.max(qmax, z[k-1]);
             emin = Math.min(emin, z[k]);
             zmax = Math.max(qmax, Math.max(zmax, z[k]));
         } // (k = 1; k <= 2*(n-1); k += 2)
         if (z[2*n-2] < 0.0) {
             info[0] = -(200+2*n-1);
             MipavUtil.displayError("Error dlasq2 had info[0] = " + info[0]);
             return;
         }
         d = d + z[2*n-2];
         qmax = Math.max(qmax, z[2*n-2]);
         zmax = Math.max(qmax, zmax);
         
         // Check for diagonality
         
         if (e == 0.0) {
             for (k = 2; k <= n; k++) {
                 z[k-1] = z[2*k-2];
             }
             dlasrt('D', n, z, iinfo);
             z[2*n-2] = d;
             return;
         } // if (e == 0.0)
         
         trace = d + e;
         
         // Check for zero data
         
         if (trace == 0.0) {
             z[2*n-2] = 0.0;
             return;
         }
         
         // Check whether the machine is IEEE conformable.
         name = new String("DLASQ2");
         opts = new String("N");
         ieee = ((ilaenv(10, name, opts, 1, 2, 3, 4) == 1) && (ilaenv(11, name, opts, 1, 2, 3, 4) == 1));
         
         // Rearrange data for locality: z = (q1,qq1,e1,ee1,q2,qq2,e2,ee2,...).
         
         for (k = 2*n; k >= 2; k -= 2) {
             z[2*k-1] = 0.0;
             z[2*k-2] = z[k-1];
             z[2*k-3] = 0.0;
             z[2*k-4] = z[k-2];
         } // for (k = 2*n; k >= 2; k -= 2)
         
         i0 = 1;
         n0 = n;
         
         // Reverse the qd-array, if warranted
         
         if (cbias * z[4*i0-4] < z[4*n0-4]) {
             ipn4 = 4 * (i0 + n0);
             for (i4 = 4*i0; i4 <= 2*(i0+n0-1); i4 += 4) {
                 temp = z[i4-4];
                 z[i4-4] = z[ipn4 -i4 - 4];
                 z[ipn4 - i4 - 4] = temp;
                 temp = z[i4-2];
                 z[i4-2] = z[ipn4 - i4 - 6];
                 z[ipn4 - i4 - 6] = temp;
             } // for (i4 = 4*i0; i4 <= 2*(i0+n0-1); i4 += 4)
         } // if (cbias * z[4*i0-4] < z[4*n0-4])
         
         // Initial split checking via dqd and Li's test.
         
         pp[0] = 0;
         
         for (k = 1; k <= 2; k++) {
             d = z[4*n0+pp[0]-4];
             for (i4 = 4*(n0-1) + pp[0]; i4 >= 4*i0 + pp[0]; i4 -= 4) {
                 if (z[i4-2] <= tol2*d) {
                     z[i4-2] = -0.0;
                     d = z[i4-4];
                 }
                 else {
                     d = z[i4-4]*(d/(d + z[i4-2]));
                 }
             } // for (i4 = 4*(n0-1) + pp[0]; i4 >= 4*i0 + pp[0]; i4 -= 4)
             
             // dqd maps z to zz plus Li's test
             
             emin = z[4*i0+pp[0]];
             d = z[4*i0+pp[0]-4];
             for (i4 = 4*i0 + pp[0]; i4 <= 4*(n0-1) + pp[0]; i4 += 4) {
                 z[i4-2*pp[0]-3] = d + z[i4-2];
                 if (z[i4-2] <= tol2*d) {
                     z[i4-2] = -0.0;
                     z[i4-2*pp[0]-3] = d;
                     z[i4-2*pp[0]-1] = 0.0;
                     d = z[i4];
                 } // if (z[i4-2] <= tol2*d)
                 else if ((safmin * z[i4] < z[i4-2*pp[0]-3]) && (safmin*z[i4-2*pp[0]-3] < z[i4])) {
                     temp = z[i4]/z[i4-2*pp[0]-3];
                     z[i4-2*pp[0]-1] = z[i4-2]*temp;
                     d = d * temp;
                 }
                 else {
                     z[i4-2*pp[0]-1] = z[i4] *(z[i4-2]/z[i4-2*pp[0]-3]);
                     d = z[i4] * (d/z[i4-2*pp[0]-3]);
                 }
                 emin = Math.min(emin, z[i4-2*pp[0]-1]);
             } // for (i4 = 4*i0 + pp[0]; i4 <= 4*(n0-1) + pp[0]; i4 += 4)
             z[4*n0 - pp[0] - 3] = d;
             
             // Now find qmax.
             
             qmax = z[4*i0-pp[0]-3];
             for (i4 = 4*i0 - pp[0] + 2; i4 <= 4*n0 - pp[0] - 2; i4 += 4) {
                 qmax = Math.max(qmax, z[i4-1]);
             }
             
             // Prepare for the next iteration on k.
             pp[0] = 1 - pp[0];
         } // for (k = 1; k <= 2; k++)
         
         // Initialize variables to pass to dlasq3
         
         ttype[0] = 0;
         dmin1[0] = 0.0;
         dmin2[0] = 0.0;
         dn[0] = 0.0;
         dn1[0] = 0.0;
         dn2[0] = 0.0;
         g[0] = 0.0;
         tau[0] = 0.0;
         
         iter[0] = 2;
         nfail[0] = 0;
         ndiv[0] = 2*(n0 - i0);
         
         loop1: {
             loop2: for (iwhila = 1; iwhila <= n + 1; iwhila++) {
                 if (n0 < 1) {
                     break loop1;
                 }
                 
                 // While array unfinished do
                 
                 // e[n0-1] holds the value of sigma when submatrix in i0-1:n0-1
                 // splits from the rest of the array, but is negated.
                 
                 desig[0] = 0.0;
                 if (n0 == n) {
                     sigma[0] = 0.0;
                 }
                 else {
                     sigma[0] = -z[4*n0-2];
                 }
                 if (sigma[0] < 0.0) {
                     info[0] = 1;
                     return;
                 }
                 
                 // Find the last unreduced submatrix's top index i0, find qmax and
                 // emin.  Find Gershgorin-type bound if Q's much greater than E's.
                 
                 emax = 0.0;
                 if (n0 > i0) {
                     emin = Math.abs(z[4*n0-6]);
                 }
                 else {
                     emin = 0.0;
                 }
                 qmin = z[4*n0-4];
                 qmax = qmin;
                 loop3: {
                     for (i4 = 4*n0; i4 >= 8; i4 -= 4) {
                         if (z[i4-6] <= 0.0) {
                             break loop3;
                         }
                         if (qmin >= 4.0*emax) {
                             qmin = Math.min(qmin, z[i4-4]);
                             emax = Math.max(emax, z[i4-6]);
                         }
                         qmax = Math.max(qmax, z[i4-8] + z[i4-6]);
                         emin = Math.min(emin, z[i4-6]);
                     } // for (i4 = 4*n0; i4 >= 8; i4 -= 4)
                     i4 = 4;
                 } // loop3
                 
                 i0 = i4/4;
                 pp[0] = 0;
                 
                 if (n0 - i0 > 1) {
                     dee = z[4*i0 - 4];
                     deemin = dee;
                     kmin = i0;
                     for (i4 = 4*i0+1; i4 <= 4*n0-3; i4 += 4) {
                         dee = z[i4-1] * (dee/(dee + z[i4-3]));
                         if (dee <= deemin) {
                             deemin = dee;
                             kmin = (i4+3)/4;
                         }
                     } // for (i4 = 4*i0+1; i4 <= 4*n0-3; i4 += 4)
                     if ((2*(kmin - i0) < n0 - kmin) && (deemin <= 0.5 * z[4*n0-4])) {
                         ipn4 = 4*(i0+n0);
                         pp[0] = 2;
                         for (i4 = 4*i0; i4 <= 2*(i0 + n0 - 1); i4 += 4) {
                             temp = z[i4-4];
                             z[i4-4] = z[ipn4-i4-4];
                             z[ipn4-i4-4] = temp;
                             temp = z[i4-3];
                             z[i4-3] = z[ipn4-i4-3];
                             z[ipn4-i4-3] = temp;
                             temp = z[i4-2];
                             z[i4-2] = z[ipn4-i4-6];
                             z[ipn4-i4-6] = temp;
                             temp = z[i4-1];
                             z[i4-1] = z[ipn4-i4-5];
                             z[ipn4-i4-5] = temp;
                         } // for (i4 = 4*i0; i4 <= 2*(i0 + n0 - 1); i4 += 4)
                     } // if ((2*(kmin - i0) < n0 - kmin) && (deemin <= 0.5 * z[4*n0-4]))
                 } // if (n0 - i0 > 1)
                 
                 // Put -(initial shift) into dmin.
                 
                 dmin[0] = -Math.max(0.0, qmin - 2.0*Math.sqrt(qmin)*Math.sqrt(emax));
                 
                 // Now i0:n0 is unreduced.
                 // pp = 0 for ping, pp = 1 for pong.
                 // pp = 2 indicates tht flipping was applied to the z array and
                 // that the tests for deflation upon entry in dlasq3 should not
                 // be performed.
                 
                 nbig = 30*(n0 - i0 + 1);
                 for (iwhilb = 1; iwhilb <= nbig; iwhilb++) {
                     if (i0 > n0) {
                         continue loop2;
                     }
                     
                     // While submatrix unfinished take a good dqds step.
                     dlasq3(i0, n0, z, pp, dmin, sigma, desig, qmax, nfail, iter, ndiv, ieee, ttype,
                            dmin1, dmin2, dn, dn1, dn2, g, tau);
                     
                     pp[0] = 1 - pp[0];
                     
                     // When emin is very small check for splits
                     if ((pp[0] == 0) && (n0 - i0 >= 3)) {
                         if ((z[4*n0-1] <= tol2*qmax) || (z[4*n0-2] <= tol2*sigma[0])) {
                             splt = i0 - 1;
                             qmax = z[4*i0-4];
                             emin = z[4*i0-2];
                             oldemn = z[4*i0-1];
                             for (i4 = 4*i0; i4 <= 4*(n0-3); i4 += 4) {
                                 if ((z[i4-1] <= tol2*z[i4-4]) || (z[i4-2] <= tol2*sigma[0])) {
                                     z[i4-2] = -sigma[0];
                                     splt = i4/4;
                                     qmax = 0.0;
                                     emin = z[i4+2];
                                     oldemn = z[i4+3];
                                 } // if ((z[i4-1] <= tol2*z[i4-4]) || (z[i4-2] <= tol2*sigma[0]))
                                 else {
                                     qmax = Math.max(qmax, z[i4]);
                                     emin = Math.min(emin, z[i4-2]);
                                     oldemn = Math.min(oldemn, z[i4-1]);
                                 }
                             } // for (i4 = 4*i0; i4 <= 4*(n0-3); i4 += 4)
                             z[4*n0-2] = emin;
                             z[4*n0-1] = oldemn;
                             i0 = splt + 1;
                         } // if ((z[4*n0-1] <= tol2*qmax) || (z[4*n0-2] <= tol2*sigma[0]))
                     } // if ((pp[0] == 0) && (n0 - i0 >= 3))
                 } // for (iwhilb = 1; iwhilb <= nbig; iwhilb++)
                 dlasq2Error = true;
                 info[0] = 2;
                 return;
             } // loop2: for (iwhila = 1; iwhila <= n + 1; iwhila++)
             info[0] = 3;
             return;
         } // loop1
         
         // Move q's to the front.
         
         for (k = 2; k <= n; k++) {
             z[k-1] = z[4*k-4];
         }
         
         // Sort and compute sum of eigenvalues.
         
         dlasrt('D', n, z, iinfo);
         
         e = 0.0;
         for (k = n; k >= 1; k--) {
             e = e + z[k-1];
         }
         
         // Store trace, sum(eigenvalues), and information on performance.
         
         z[2*n] = trace;
         z[2*n+1] = e;
         z[2*n+2] = (double)iter[0];
         z[2*n+3] = (double)ndiv[0]/(double)(n*n);
         z[2*n+4] = 100.0 * nfail[0]/(double)iter[0];
         return;
     } // dlasq2

     
     /**
      * This is a port of the version 3.2 LAPACK auxiliary routine DLASRT Original DLASRT created by Univ. of Tennessee,
      * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
      * dlasrt sorts the numbers in d in increasing order if id == 'I' or in decreasing order if id == 'D'. Use
      * quick sort, reverting to insertion sort on arrays of size <= 20. Dimension of stack limits n to about 2**32.
      *
      * @param  id    input char 
      *               = 'I': sort d in increasing order 
      *               = 'D': sort d in decreasing order
      * @param  n     input int The length of the array d.
      * @param  d     input/output double[] of dimension n. On entry, the array to be sorted. On exit, d has been sorted
      *               into increasing order (d[0] <= ... <= d[n-1]) or into decreasing order (d[0] >= ... >= d[n-1]),
      *               depending on id.
      * @param  info  output int[] 
      *               = 0: successful exit 
      *               < 0: If info = -i, the i-th argument had an illegal value
      */
     private void dlasrt(char id, int n, double[] d, int[] info) {
         int select = 20;
         int dir;
         int endd;
         int i;
         int j;
         int start;
         int stkpnt;
         double d1;
         double d2;
         double d3;
         double dmnmx;
         double tmp;
         int[][] stack = new int[2][32];

         // Test the input parameters
         info[0] = 0;
         dir = -1;

         if ((id == 'D') || (id == 'd')) {
             dir = 0;
         } else if ((id == 'I') || (id == 'i')) {
             dir = 1;
         }

         if (dir == -1) {
             info[0] = -1;
         } else if (n < 0) {
             info[0] = -2;
         }

         if (info[0] != 0) {
             MipavUtil.displayError("Error dlasrt had info = " + info[0]);

             return;
         }

         // Quick return if possible
         if (n <= 1) {
             return;
         }

         stkpnt = 0;
         stack[0][0] = 0;
         stack[1][0] = n - 1;

         do {
             start = stack[0][stkpnt];
             endd = stack[1][stkpnt];
             stkpnt = stkpnt - 1;

             if (((endd - start) <= select) && ((endd - start) > 0)) {

                 // Do insertion sort on d(start:endd)
                 if (dir == 0) {

 // Sort into decreasing order
 loop1:
                     for (i = start + 1; i <= endd; i++) {

                         for (j = i; j >= (start + 1); j--) {

                             if (d[j] > d[j - 1]) {
                                 dmnmx = d[j];
                                 d[j] = d[j - 1];
                                 d[j - 1] = dmnmx;
                             } else {
                                 continue loop1;
                             }
                         } // for (j = i; j >= start+1; j--)
                     } // for (i = start+1; i <= endd; i++)
                 } // if (dir == 0)
                 else { // dir == 1

 // Sort into increasing order
 loop2:
                     for (i = start + 1; i <= endd; i++) {

                         for (j = i; j >= (start + 1); j--) {

                             if (d[j] < d[j - 1]) {
                                 dmnmx = d[j];
                                 d[j] = d[j - 1];
                                 d[j - 1] = dmnmx;
                             } else {
                                 continue loop2;
                             }
                         } // for (j = i; j >= start+1; j--)
                     } // for (i = start+1; i <= endd; i++)
                 } // else dir == 1
             } // if (((endd - start) <= select) && ((endd - start) > 0))
             else if ((endd - start) > select) {

                 // Partition d(start:endd) and stack parts, largest one first
                 // Choose partition entry as median of 3
                 d1 = d[start];
                 d2 = d[endd];
                 i = (start + endd) / 2;
                 d3 = d[i];

                 if (d1 < d2) {

                     if (d3 < d1) {
                         dmnmx = d1;
                     } else if (d3 < d2) {
                         dmnmx = d3;
                     } else {
                         dmnmx = d2;
                     }
                 } // if (d1 < d2)
                 else { // d1 >= d2

                     if (d3 < d2) {
                         dmnmx = d2;
                     } else if (d3 < d1) {
                         dmnmx = d3;
                     } else {
                         dmnmx = d1;
                     }
                 } // else d1 >= d2

                 if (dir == 0) {

                     // Sort into decreasing order
                     i = start - 1;
                     j = endd + 1;

 loop3:
                     do {
                         j = j - 1;

                         if (d[j] < dmnmx) {
                             continue loop3;
                         }

                         do {
                             i = i + 1;
                         } while (d[i] > dmnmx);

                         if (i < j) {
                             tmp = d[i];
                             d[i] = d[j];
                             d[j] = tmp;

                             continue loop3;
                         } // if (i < j)

                         break loop3;
                     } // loop3
                     while (true);

                     if ((j - start) > (endd - j - 1)) {
                         stkpnt = stkpnt + 1;
                         stack[0][stkpnt] = start;
                         stack[1][stkpnt] = j;
                         stkpnt = stkpnt + 1;
                         stack[0][stkpnt] = j + 1;
                         stack[1][stkpnt] = endd;
                     } // if ((j - start) > (endd - j - 1))
                     else { // ((j - start) <= (endd - j - 1))
                         stkpnt = stkpnt + 1;
                         stack[0][stkpnt] = j + 1;
                         stack[1][stkpnt] = endd;
                         stkpnt = stkpnt + 1;
                         stack[0][stkpnt] = start;
                         stack[1][stkpnt] = j;
                     } // else ((j - start) <= (endd - j - 1))
                 } // if (dir == 0)
                 else { // dir == 1

                     // Sort into increasing order
                     i = start - 1;
                     j = endd + 1;

 loop4:
                     do {
                         j = j - 1;

                         if (d[j] > dmnmx) {
                             continue loop4;
                         }

                         do {
                             i = i + 1;
                         } while (d[i] < dmnmx);

                         if (i < j) {
                             tmp = d[i];
                             d[i] = d[j];
                             d[j] = tmp;

                             continue loop4;
                         } // if (i < j)

                         break loop4;
                     } // loop4
                     while (true);

                     if ((j - start) > (endd - j - 1)) {
                         stkpnt = stkpnt + 1;
                         stack[0][stkpnt] = start;
                         stack[1][stkpnt] = j;
                         stkpnt = stkpnt + 1;
                         stack[0][stkpnt] = j + 1;
                         stack[1][stkpnt] = endd;
                     } // if ((j-start) > (endd-j-1))
                     else { // ((j-start) <= (endd-j-1))
                         stkpnt = stkpnt + 1;
                         stack[0][stkpnt] = j + 1;
                         stack[1][stkpnt] = endd;
                         stkpnt = stkpnt + 1;
                         stack[0][stkpnt] = start;
                         stack[1][stkpnt] = j;
                     } // else ((j-start) <= (endd-j-1)
                 } // else dir == 1
             } // else if ((endd - start) > select)
         } while (stkpnt > -1);

         return;
     } // dlasrt
     
     /** This is a port of version 3.2 LAPACK routine DLASQ3
        *  -- Contributed by Osni Marques of the Lawrence Berkeley National   --
        *  -- Laboratory and Beresford Parlett of the Univ. of California at  --
        *  -- Berkeley                                                        --
        *  -- November 2008                                                   --
        *
        *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
        *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
        *
        *     .. Scalar Arguments ..
              LOGICAL            IEEE
              INTEGER            I0, ITER, N0, NDIV, NFAIL, PP
              DOUBLE PRECISION   DESIG, DMIN, DMIN1, DMIN2, DN, DN1, DN2, G,
             $                   QMAX, SIGMA, TAU
        *     ..
        *     .. Array Arguments ..
              DOUBLE PRECISION   Z( * )
        *     ..
        *
        *  Purpose
        *  =======
        *
        *  DLASQ3 checks for deflation, computes a shift (TAU) and calls dqds.
        *  In case of failure it changes shifts, and tries again until output
        *  is positive.
        *
        *  Arguments
        *  =========
        *
        *  I0     (input) INTEGER
        *         First index.
        *
        *  N0     (input) INTEGER
        *         Last index.
        *
        *  Z      (input) DOUBLE PRECISION array, dimension ( 4*N )
        *         Z holds the qd array.
        *
        *  PP     (input/output) INTEGER
        *         PP=0 for ping, PP=1 for pong.
        *         PP=2 indicates that flipping was applied to the Z array   
        *         and that the initial tests for deflation should not be 
        *         performed.
        *
        *  DMIN   (output) DOUBLE PRECISION
        *         Minimum value of d.
        *
        *  SIGMA  (output) DOUBLE PRECISION
        *         Sum of shifts used in current segment.
        *
        *  DESIG  (input/output) DOUBLE PRECISION
        *         Lower order part of SIGMA
        *
        *  QMAX   (input) DOUBLE PRECISION
        *         Maximum value of q.
        *
        *  NFAIL  (output) INTEGER
        *         Number of times shift was too big.
        *
        *  ITER   (output) INTEGER
        *         Number of iterations.
        *
        *  NDIV   (output) INTEGER
        *         Number of divisions.
        *
        *  IEEE   (input) LOGICAL
        *         Flag for IEEE or non IEEE arithmetic (passed to DLASQ5).
        *
        *  TTYPE  (input/output) INTEGER
        *         Shift type.
        *
        *  DMIN1, DMIN2, DN, DN1, DN2, G, TAU (input/output) DOUBLE PRECISION
        *         These are passed as arguments in order to save their values
        *         between calls to DLASQ3.
        */
     private void dlasq3(int i0, int n0, double z[], int pp[], double dmin[], double sigma[],
                    double desig[], double qmax, int nfail[], int iter[], int ndiv[],
                    boolean ieee, int ttype[], double dmin1[], double dmin2[], double dn[],
                    double dn1[], double dn2[], double g[], double tau[]) {
         double cbias = 1.50;
         int ipn4;
         int j4;
         int n0in;
         int nn;
         double eps;
         double s;
         double t;
         double temp;
         double tol;
         double tol2;
         boolean calldlasq6;
         
         n0in = n0;
         eps = dlamch('P'); // Precision
         tol = 100.0 * eps;
         tol2 = tol * tol;
         
         // Check for deflation.
         while (true) {
             if (n0 < i0) {
                 return;
             }
             if (n0 == i0) {
                 z[4*n0-4] = z[4*n0+pp[0]-4] + sigma[0];
                 n0 = n0 - 1;
                 continue;
             }
             nn = 4*n0 + pp[0];
             if (n0 != (i0+1)) {
                 // Check whether e[n0-2] is negligible, 1 eigenvalue.
                 if ((z[nn-6] <= tol2*(sigma[0]+z[nn-4])) ||
                     (z[nn-2*pp[0]-5] <= tol2*z[nn-8])) {
                     z[4*n0-4] = z[4*n0+pp[0]-4] + sigma[0];
                     n0 = n0 - 1;
                     continue;    
                 }
                 // Check whether e[n0-3] is negligible, 2 eigenvalues.
                 if ((z[nn-10] > tol2*sigma[0]) && (z[nn-2*pp[0]-9] > tol2*z[nn-12])) {
                     break;
                 }
             } // if (n0 != (i0 + 1))
             
             if (z[nn - 4] > z[nn-8]) {
                 s = z[nn-4];
                 z[nn-4] = z[nn-8];
                 z[nn-8] = s;
             } // if (z[nn - 4] > z[nn-8])
             if (z[nn-6] > z[nn-4]*tol2) {
                 t = 0.5 * ((z[nn-8] - z[nn-4]) + z[nn-6]);
                 s = z[nn-4] * (z[nn-6]/t);
                 if (s <= t) {
                     s = z[nn-4] * (z[nn-6]/(t*(1.0 + Math.sqrt(1.0 + s/t))));
                 }
                 else {
                     s = z[nn-4] * (z[nn-6]/(t + Math.sqrt(t)*Math.sqrt(t+s)));
                 }
                 t = z[nn-8] + (s + z[nn-6]);
                 z[nn-4] = z[nn-4] * (z[nn-8]/t);
                 z[nn-8] = t;
             } // if (z[nn-6] > z[nn-4]*tol2)
             z[4*n0-8] = z[nn-8] + sigma[0];
             z[4*n0-4] = z[nn-4] + sigma[0];
             n0 = n0 - 2;
         } // while (true)
         
         if (pp[0] == 2) {
             pp[0] = 0;
         }
         
         // Reverse the qd-array, if warranted.
         
         if ((dmin[0] <= 0.0) || (n0 < n0in)) {
             if (cbias*z[4*i0+pp[0]-4] < z[4*n0+pp[0]-4]) {
                 ipn4 = 4 * (i0 + n0);
                 for (j4 = 4*i0; j4 <= 2*(i0 + n0 - 1); j4 += 4) {
                     temp = z[j4-4];
                     z[j4-4] = z[ipn4-j4-4];
                     z[ipn4-j4-4] = temp;
                     temp = z[j4-3];
                     z[j4-3] = z[ipn4-j4-3];
                     z[ipn4-j4-3] = temp;
                     temp = z[j4-2];
                     z[j4-2] = z[ipn4-j4-6];
                     z[ipn4-j4-6] = temp;
                     temp = z[j4-1];
                     z[j4-1] = z[ipn4-j4-5];
                     z[ipn4-j4-5] = temp;
                 } // for (j4 = 4*i0; j4 <= 2*(i0 + n0 - 1); j4 += 4)
                 if (n0 - i0 <= 4) {
                     z[4*n0+pp[0]-2] = z[4*i0+pp[0]-2];
                     z[4*n0-pp[0]-1] = z[4*i0-pp[0]-1];
                 }
                 dmin2[0] = Math.min(dmin2[0], z[4*n0+pp[0]-2]);
                 z[4*n0+pp[0]-2] = Math.min(z[4*n0+pp[0]-2], Math.min(z[4*i0+pp[0]-2],
                                            z[4*i0+pp[0]+2]));
                 z[4*n0-pp[0]-1] = Math.min(z[4*n0-pp[0]-1], Math.min(z[4*i0-pp[0]-1], 
                                            z[4*i0-pp[0]+3]));
                 qmax = Math.max(qmax, Math.max(z[4*i0+pp[0]-4], z[4*i0+pp[0]]));
                 dmin[0] = -0.0;
             } // if (cbias*z[4*i0+pp[0]-4] < z[4*n0+pp[0]-4])
         } // if ((dmin[0] <= 0.0) || (n0 < n0in))
         
         // Choose a shift
         dlasq4(i0, n0, z, pp[0], n0in, dmin[0], dmin1[0], dmin2[0], dn[0], dn1[0], dn2[0], tau, ttype, g);
         
         // Call dqds until dmin > 0
         
         while (true) {
             dlasq5(i0, n0, z, pp[0], tau[0], dmin, dmin1, dmin2, dn, dn1, dn2, ieee);
             
             ndiv[0] = ndiv[0] + (n0 - i0 + 2);
             iter[0] = iter[0] + 1;
             
             // Check status
             if (Double.isNaN(dmin[0])) {
                 // NaN
                 
                 if (tau[0] == 0.0) {
                     calldlasq6 = true;
                     break;
                 }
                 else {
                     tau[0] = 0.0;
                     continue;
                 }
             } // else if (Double.isNaN(dmin[0]))
             else if ((dmin[0] >= 0.0) && (dmin1[0] > 0.0)) {
                 // Success
                 calldlasq6 = false;
                 break;
             }
             else if ((dmin[0] < 0.0) && (dmin1[0] > 0.0) && 
                     (z[4*(n0-1)-pp[0]-1] < tol*(sigma[0]+dn1[0])) &&
                     (Math.abs(dn[0]) < tol*sigma[0])) {
                 // Convergence hidden by negative dn[0]
                 z[4*(n0-1)-pp[0]+1] = 0.0;
                 dmin[0] = 0.0;
                 calldlasq6 = false;
                 break;
             }
             else if (dmin[0] < 0.0) {
                 //tau[0] too big.  Select new tau[0] and try again.
                 nfail[0] = nfail[0] + 1;
                 if (ttype[0] < -22) {
                     // Failed twice.  Play it safe.
                     tau[0] = 0.0;
                 }
                 else if (dmin1[0] > 0.0) {
                     // Late failure.  Gives excellent shift.
                     tau[0] = (tau[0] + dmin[0]) * (1.0 - 2.0 * eps);
                     ttype[0] = ttype[0] - 11;
                 }
                 else {
                     // Early failure.  Divide by 4.
                     tau[0] = 0.25 * tau[0];
                     ttype[0] = ttype[0] - 12;
                 }
                 continue;
             } // else if (dmin[0] < 0.0)
             // Possible underflow.  Play it safe.
             calldlasq6 = true;
             break;
         } // while (true)
         
         if (calldlasq6) {
             // Risk of underflow
             dlasq6(i0, n0, z, pp[0], dmin, dmin1, dmin2, dn, dn1, dn2);
             ndiv[0] = ndiv[0] + (n0 - i0 + 2);
             iter[0] = iter[0] + 1;
             tau[0] = 0.0;
         } // if (calldlasq6)
         
         if (tau[0] < sigma[0]) {
             desig[0] = desig[0] + tau[0];
             t = sigma[0] + desig[0];
             desig[0] = desig[0] - (t - sigma[0]);
         }
         else {
             t = sigma[0] + tau[0];
             desig[0] = sigma[0] - (t - tau[0]) + desig[0];
         }
         sigma[0] = t;
         return;
     } // dlasq3

     
     /** This is a port of version 3.2 LAPACK routine DLASQ4.
        *  -- Contributed by Osni Marques of the Lawrence Berkeley National   --
        *  -- Laboratory and Beresford Parlett of the Univ. of California at  --
        *  -- Berkeley                                                        --
        *  -- November 2008                                                   --
        *
        *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
        *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
        *
        *     .. Scalar Arguments ..
              INTEGER            I0, N0, N0IN, PP, TTYPE
              DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DN1, DN2, G, TAU
        *     ..
        *     .. Array Arguments ..
              DOUBLE PRECISION   Z( * )
        *     ..
        *
        *  Purpose
        *  =======
        *
        *  DLASQ4 computes an approximation TAU to the smallest eigenvalue
        *  using values of d from the previous transform.
        *
        *  I0    (input) INTEGER
        *        First index.
        *
        *  N0    (input) INTEGER
        *        Last index.
        *
        *  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
        *        Z holds the qd array.
        *
        *  PP    (input) INTEGER
        *        PP=0 for ping, PP=1 for pong.
        *
        *  NOIN  (input) INTEGER
        *        The value of N0 at start of EIGTEST.
        *
        *  DMIN  (input) DOUBLE PRECISION
        *        Minimum value of d.
        *
        *  DMIN1 (input) DOUBLE PRECISION
        *        Minimum value of d, excluding D( N0 ).
        *
        *  DMIN2 (input) DOUBLE PRECISION
        *        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
        *
        *  DN    (input) DOUBLE PRECISION
        *        d(N)
        *
        *  DN1   (input) DOUBLE PRECISION
        *        d(N-1)
        *
        *  DN2   (input) DOUBLE PRECISION
        *        d(N-2)
        *
        *  TAU   (output) DOUBLE PRECISION
        *        This is the shift.
        *
        *  TTYPE (output) INTEGER
        *        Shift type.
        *
        *  G     (input/output) REAL
        *        G is passed as an argument in order to save its value between
        *        calls to DLASQ4.
        *
        *  Further Details
        *  ===============
        *  CNST1 = 9/16
        */
     private void dlasq4(int i0, int n0, double z[], int pp, int n0in, double dmin, double dmin1,
                         double dmin2, double dn, double dn1, double dn2, double tau[], int ttype[],
                         double g[]) {
         double cnst1 = 0.5630;
         double cnst2 = 1.010;
         double cnst3 = 1.050;
         double third = 0.3330;
         int i4;
         int nn;
         int np;
         double a2;
         double b1;
         double b2;
         double gam;
         double gap1;
         double gap2;
         double s = 0.0;
         
         // A negative dmin forces the shift to take that absolute value.
         // ttype records the type of shift.
         if (dmin <= 0.0) {
             tau[0] = -dmin;
             ttype[0] = -1;
             return;
         }  // if (dmin <= 0.0)
         
         nn = 4*n0 + pp;
         if (n0in == n0) {
             // No eigenvalues deflated.
             if ((dmin == dn) || (dmin == dn1)) {
                 b1 = Math.sqrt(z[nn-4]) * Math.sqrt(z[nn-6]);
                 b2 = Math.sqrt(z[nn-8]) * Math.sqrt(z[nn-10]);
                 a2 = z[nn-8] + z[nn-6];
                 
                 // Cases 2 and 3
                 
                 if ((dmin == dn) && (dmin1 == dn1)) {
                     gap2 = 0.75*dmin2 - a2;
                     if ((gap2 > 0.0) && (gap2 > b2)) {
                         gap1 = a2 - dn - (b2/gap2)*b2;
                     }
                     else {
                         gap1 = a2 - dn - (b1 + b2);
                     }
                     if ((gap1 > 0.0) && (gap1 > b1)) {
                         s = Math.max(dn-(b1/gap1)*b1, 0.5*dmin);
                         ttype[0] = -2;
                     }
                     else {
                         s = 0.0;
                         if (dn > b1) {
                             s = dn - b1;
                         }
                         if (a2 > (b1 + b2)) {
                             s = Math.min(s, a2 - (b1 + b2));
                         }
                         s = Math.max(s, third * dmin);
                         ttype[0] = -3;
                     }
                 } // if ((dmin == dn) && (dmin1 == dn1))
                 else {
                     // Case 4.
                     
                     ttype[0] = -4;
                     s = 0.25 * dmin;
                     if (dmin == dn) {
                         gam = dn;
                         a2 = 0.0;
                         if (z[nn-6] > z[nn-8]) {
                             return;
                         }
                         b2 = z[nn-6]/z[nn-8];
                         np = nn - 9;
                     } // if (dmin == dn)
                     else { // dmin != dn
                         np = nn - 2*pp;
                         b2 = z[np-3];
                         gam = dn1;
                         if (z[np-5] > z[np-3]) {
                             return;
                         }
                         a2 = z[np-5]/z[np-3];
                         if (z[nn-10] > z[nn-12]) {
                             return;
                         }
                         b2 = z[nn-10]/z[nn-12];
                         np = nn - 13;
                     } // else dmin != dn
                     // Approximate contribution to norm squared from i < nn - 1.
                     a2 = a2 + b2;
                     for (i4 = np; i4 >= 4*i0 - 1 + pp; i4 -= 4) {
                         if (b2 == 0.0) {
                             break;
                         }
                         b1 = b2;
                         if (z[i4-1] > z[i4-3]) {
                             return;
                         }
                         b2 = b2 *(z[i4-1]/z[i4-3]);
                         a2 = a2 + b2;
                         if ((100.0 * Math.max(b2, b1) < a2) || (cnst1 < a2)) {
                             break;
                         }
                     } // for (i4 = np; i4 >= 4*i0 - 1 + pp; i4 -= 4)
                     a2 = cnst3 * a2;
                     
                     // Rayleigh quotient residual bond.
                     if (a2 < cnst1) {
                         s = gam * (1.0 - Math.sqrt(a2)) / (1.0 + a2); 
                     }
                 } // else
             } // if ((dmin == dn) || (dmin == dn1))
             else if (dmin == dn2) {
                 // Case 5.
                 
                 ttype[0] = -5;
                 s = 0.25*dmin;
                 
                 // Compute contribution to norm squared from i > nn - 2.
                 np = nn - 2*pp;
                 b1 = z[np-3];
                 b2 = z[np-7];
                 gam = dn2;
                 if ((z[np-9] > b2) || (z[np-5] > b1)) {
                     return;
                 }
                 a2 = (z[np-9]/b2)* (1.0 + z[np-5]/b1);
                 
                 // Approixmate contribution to norm squared from i < nn - 2.
                 
                 if (n0 - i0 > 2) {
                     b2 = z[nn-14]/z[nn-16];
                     a2 = a2 + b2;
                     for (i4 = nn - 17; i4 >= 4*i0 - 1 + pp; i4 -= 4) {
                         if (b2 == 0.0) {
                             break;
                         }
                         b1 = b2; 
                         if (z[i4-1] > z[i4-3]) {
                             return;
                         }
                         b2 = b2 * (z[i4-1]/z[i4-3]);
                         a2 = a2 + b2;
                         if ((100.0 * Math.max(b2,b1) < a2) || (cnst1 < a2)) {
                             break;
                         }
                     } // for (i4 = nn - 17; i4 >= 4*i0 - 1 + pp[0]; i4 -= 4)
                     a2 = cnst3 * a2;
                 } // if (n0 - i0 > 2)
                 if (a2 < cnst1) {
                     s = gam * (1.0 - Math.sqrt(a2))/(1.0 + a2);
                 }
             } // else if (dmin == dn2)
             else {
                 // Case 6, no information to guide us.
                 if (ttype[0] == -6) {
                     g[0] = g[0] + third * (1.0 - g[0]);
                 }
                 else if (ttype[0] == -18) {
                     g[0] = 0.25*third;
                 }
                 else {
                     g[0] = 0.25;
                 }
                 s = g[0] * dmin;
                 ttype[0] = -6;
             } // else
         } // if (n0in == n0)
         else if (n0in == (n0 + 1)) {
             // One eigenvalue just deflated.  Use dmin, dn1 for dmin and dn.
             if ((dmin1 == dn1) && (dmin2 == dn2)) {
                 // Cases 7 and 8.
                 
                 ttype[0] = -7;
                 s = third * dmin1;
                 if (z[nn-6] > z[nn-8]) {
                     return;
                 }
                 b1 = z[nn-6]/z[nn-8];
                 b2 = b1;
                 if (b2 != 0.0) {
                     for (i4 = 4*n0 - 9 + pp; i4 >= 4*i0 - 1 + pp; i4 -= 4) {
                         a2 = b1;
                         if (z[i4-1] > z[i4-3]) {
                             return;
                         }
                         b1 = b1 *(z[i4-1]/z[i4-3]);
                         b2 = b2 + b1;
                         if (100.0 * Math.max(b1, a2) < b2) {
                             break;
                         }
                     } // for (i4 = 4*n0 - 9 + pp; i4 >= 4*i0 - 1 + pp; i4 -= 4)
                 } // if (b2 != 0.0)
                 b2 = Math.sqrt(cnst3*b2);
                 a2 = dmin1 / (1.0 + b2*b2);
                 gap2 = 0.5*dmin2 - a2;
                 if ((gap2 > 0.0) && (gap2 > b2*a2)) {
                     s = Math.max(s, a2*(1.0 - cnst2*a2*(b2/gap2)*b2));
                 }
                 else {
                     s = Math.max(s, a2 * (1.0 - cnst2 * b2));
                     ttype[0] = -8;
                 }
             } // if ((dmin1 == dn1) && (dmin2 == dn2))
             else {
                 // Case 9.
                 
                 s = 0.25 * dmin1;
                 if (dmin1 == dn1) {
                     s = 0.5 * dmin1;
                 }
                 ttype[0] = -9;
             }
         } // else if (n0in == (n0 + 1))
         else if (n0in == (n0 + 2)) {
             // Two eigenvalues deflated.  Use dmin2, dn2 for dmin and dn.
             // Cases 10 and 11
             
             if ((dmin2 == dn2) && (2.0*z[nn-6] < z[nn-8])) {
                 ttype[0] = -10;
                 s = third * dmin2;
                 if (z[nn-6] > z[nn-8]) {
                     return;
                 }
                 b1 = z[nn-6]/z[nn-8];
                 b2 = b1;
                 if (b2 != 0.0) {
                     for (i4 = 4*n0 - 9 + pp; i4 >= 4*i0 - 1 + pp; i4 -= 4) {
                         if (z[i4 - 1] > z[i4-3]) {
                             return;
                         }
                         b1 = b1 * (z[i4-1]/z[i4-3]);
                         b2 = b2 + b1;
                         if (100.0 * b1 < b2) {
                             break;
                         }
                     } // for (i4 = 4*n0 - 9 + pp; i4 >= 4*i0 - 1 + pp; i4 -= 4)
                 } // if (b2 != 0.0)
                 b2 = Math.sqrt(cnst3 * b2);
                 a2 = dmin2 / (1.0 + b2*b2);
                 gap2 = z[nn-8] + z[nn-10] - Math.sqrt(z[nn-12]) * Math.sqrt(z[nn-10]) - a2;
                 if ((gap2 > 0.0) && (gap2 > b2*a2)) {
                     s = Math.max(s, a2*(1.0 - cnst2*a2*(b2/gap2)*b2));
                 }
                 else {
                     s = Math.max(s, a2*(1.0 - cnst2 * b2));
                 }
             } // if ((dmin2 == dn2) && (2.0*z[nn-6] < z[nn-8]))
             else {
                 s = 0.25 * dmin2;
                 ttype[0] = -11;
             }
         } // else if (n0in == (n0 + 2))
         else if (n0in > (n0 + 2)) {
             // Case 12, more than two eigenvalues deflated.  No information.
             s = 0.0;
             ttype[0] = -12;
         } // else if (n0in > (n0 + 2))
         
         tau[0] = s;
         return;
     } // dlasq4

     
     /** This is a port of version 3.2 LAPACK routine DLASQ5
     *  -- Contributed by Osni Marques of the Lawrence Berkeley National   --
     *  -- Laboratory and Beresford Parlett of the Univ. of California at  --
     *  -- Berkeley                                                        --
     *  -- November 2008                                                   --
     *
     *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
     *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
     *
     *     .. Scalar Arguments ..
           LOGICAL            IEEE
           INTEGER            I0, N0, PP
           DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DNM1, DNM2, TAU
     *     ..
     *     .. Array Arguments ..
           DOUBLE PRECISION   Z( * )
     *     ..
     *
     *  Purpose
     *  =======
     *
     *  DLASQ5 computes one dqds transform in ping-pong form, one
     *  version for IEEE machines another for non IEEE machines.
     *
     *  Arguments
     *  =========
     *
     *  I0    (input) INTEGER
     *        First index.
     *
     *  N0    (input) INTEGER
     *        Last index.
     *
     *  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
     *        Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
     *        an extra argument.
     *
     *  PP    (input) INTEGER
     *        PP=0 for ping, PP=1 for pong.
     *
     *  TAU   (input) DOUBLE PRECISION
     *        This is the shift.
     *
     *  DMIN  (output) DOUBLE PRECISION
     *        Minimum value of d.
     *
     *  DMIN1 (output) DOUBLE PRECISION
     *        Minimum value of d, excluding D( N0 ).
     *
     *  DMIN2 (output) DOUBLE PRECISION
     *        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
     *
     *  DN    (output) DOUBLE PRECISION
     *        d(N0), the last value of d.
     *
     *  DNM1  (output) DOUBLE PRECISION
     *        d(N0-1).
     *
     *  DNM2  (output) DOUBLE PRECISION
     *        d(N0-2).
     *
     *  IEEE  (input) LOGICAL
     *        Flag for IEEE or non IEEE arithmetic.
     */
     private void dlasq5(int i0, int n0, double z[], int pp, double tau, double dmin[], double dmin1[],
                         double dmin2[], double dn[], double dnm1[], double dnm2[], boolean ieee)  {
         int j4;
         int j4p2;
         double d;
         double emin;
         double temp;
         
         if ((n0 - i0 - 1) <= 0) {
             return;
         }
         
         j4 = 4*i0 + pp - 3;
         emin = z[j4+3];
         d = z[j4-1] - tau;
         dmin[0] = d;
         dmin1[0] = -z[j4-1];
         
         if (ieee) {
             // Code for IEEE arithmetic.
             if (pp == 0) {
                 for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4) {
                     z[j4-3] = d + z[j4-2];
                     temp = z[j4]/z[j4-3];
                     d = d * temp - tau;
                     dmin[0] = Math.min(dmin[0], d);
                     z[j4-1] = z[j4-2] * temp;
                     emin = Math.min(z[j4-1], emin);
                 } // for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4)
             } // if (pp == 0)
             else { // pp != 0
                 for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4) {
                     z[j4-4] = d + z[j4-1];
                     temp = z[j4+1]/z[j4-4];
                     d = d * temp - tau;
                     dmin[0] = Math.min(dmin[0], d);
                     z[j4-2] = z[j4-1] * temp;
                     emin = Math.min(z[j4-2], emin);
                 } // for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4)
             } // else pp != 0
             
             // Unroll last 2 steps.
             
             dnm2[0] = d;
             dmin2[0] = dmin[0];
             j4 = 4*(n0-2) - pp;
             j4p2 = j4 + 2*pp - 1;
             z[j4-3] = dnm2[0] + z[j4p2-1];
             z[j4-1] = z[j4p2+1] * (z[j4p2-1]/z[j4-3]);
             dnm1[0] = z[j4p2+1] * (dnm2[0]/z[j4-3]) - tau;
             dmin[0] = Math.min(dmin[0], dnm1[0]);
             
             dmin1[0] = dmin[0];
             j4 = j4 + 4;
             j4p2 = j4 + 2*pp - 1;
             z[j4-3] = dnm1[0] + z[j4p2-1];
             z[j4-1] = z[j4p2+1] * (z[j4p2-1]/z[j4-3]);
             dn[0] = z[j4p2+1] * (dnm1[0]/z[j4-3]) - tau;
             dmin[0] = Math.min(dmin[0], dn[0]);
         } // if (ieee)
         else { // !ieee
             // Code for non IEEE arithmetic.
             
             if (pp == 0) {
                 for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4) {
                     z[j4-3] = d + z[j4-2];
                     if (d < 0.0) {
                         return;
                     }
                     else {
                         z[j4-1] = z[j4] * (z[j4-2]/z[j4-3]);
                         d = z[j4] * (d/z[j4-3]) - tau;
                     }
                     dmin[0] = Math.min(dmin[0], d);
                     emin = Math.min(emin, z[j4-1]);
                 } // for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4)
             } // if (pp == 0)
             else { // pp != 0
                 for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4) {
                     z[j4-4] = d + z[j4-1];
                     if (d < 0.0) {
                         return;
                     }
                     else {
                         z[j4-2] = z[j4+1] * (z[j4-1]/z[j4-4]);
                         d = z[j4+1] * (d/z[j4-4]) - tau;
                     }
                     dmin[0] = Math.min(dmin[0], d);
                     emin = Math.min(emin, z[j4-2]);
                 } // for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4)
             } // else pp != 0
             
             // Unroll last 2 steps.
             dnm2[0] = d;
             dmin2[0] = dmin[0];
             j4 = 4*(n0-2) - pp;
             j4p2 = j4 + 2*pp - 1;
             z[j4-3] = dnm2[0] + z[j4p2-1];
             if (dnm2[0] < 0.0) {
                 return;
             }
             else {
                 z[j4-1] = z[j4p2+1] * (z[j4p2-1]/z[j4-3]);
                 dnm1[0] = z[j4p2+1] * (dnm2[0]/z[j4-3]) - tau;
             }
             dmin[0] = Math.min(dmin[0], dnm1[0]);
             
             dmin1[0] = dmin[0];
             j4 = j4 + 4;
             j4p2 = j4 + 2*pp - 1;
             z[j4-3] = dnm1[0] + z[j4p2-1];
             if (dnm1[0] < 0.0) {
                 return;
             }
             else {
                 z[j4-1] = z[j4p2+1] * (z[j4p2-1]/z[j4-3]);
                 dn[0] = z[j4p2 + 1] * (dnm1[0]/z[j4-3]) - tau;
             }
             dmin[0] = Math.min(dmin[0], dn[0]);
         } // else !ieee
         
         z[j4+1] = dn[0];
         z[4*n0 - pp - 1] = emin;
         return;
     } // dlasq5
     
     /** This is a port of version 3.2 LAPACK routine DLASQ6.
      * 
     *  -- Contributed by Osni Marques of the Lawrence Berkeley National   --
     *  -- Laboratory and Beresford Parlett of the Univ. of California at  --
     *  -- Berkeley                                                        --
     *  -- November 2008                                                   --
     *
     *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
     *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
     *
     *     .. Scalar Arguments ..
           INTEGER            I0, N0, PP
           DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DNM1, DNM2
     *     ..
     *     .. Array Arguments ..
           DOUBLE PRECISION   Z( * )
     *     ..
     *
     *  Purpose
     *  =======
     *
     *  DLASQ6 computes one dqd (shift equal to zero) transform in
     *  ping-pong form, with protection against underflow and overflow.
     *
     *  Arguments
     *  =========
     *
     *  I0    (input) INTEGER
     *        First index.
     *
     *  N0    (input) INTEGER
     *        Last index.
     *
     *  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
     *        Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
     *        an extra argument.
     *
     *  PP    (input) INTEGER
     *        PP=0 for ping, PP=1 for pong.
     *
     *  DMIN  (output) DOUBLE PRECISION
     *        Minimum value of d.
     *
     *  DMIN1 (output) DOUBLE PRECISION
     *        Minimum value of d, excluding D( N0 ).
     *
     *  DMIN2 (output) DOUBLE PRECISION
     *        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
     *
     *  DN    (output) DOUBLE PRECISION
     *        d(N0), the last value of d.
     *
     *  DNM1  (output) DOUBLE PRECISION
     *        d(N0-1).
     *
     *  DNM2  (output) DOUBLE PRECISION
     *        d(N0-2).
     */
     private void dlasq6(int i0, int n0, double z[], int pp, double dmin[], double dmin1[],
                         double dmin2[], double dn[], double dnm1[], double dnm2[]) {
         int j4;
         int j4p2;
         double d;
         double emin;
         double safmin;
         double temp;
         
         if ((n0 - i0 - 1) <= 0) {
             return;
         }
         
         safmin = dlamch('S'); // safe minimum
         j4 = 4*i0 + pp - 3;
         emin = z[j4+3];
         d = z[j4-1];
         dmin[0] = d;
         
         if (pp == 0) {
             for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4) {
                 z[j4-3] = d + z[j4-2];
                 if (z[j4-3] == 0.0) {
                     z[j4-1] = 0.0;
                     d = z[j4];
                     dmin[0] = d;
                     emin = 0.0;
                 } // if (z[j4-3] == 0.0)
                 else if ((safmin*z[j4] < z[j4-3]) && (safmin*z[j4-3] < z[j4])) {
                     temp = z[j4]/z[j4-3];
                     z[j4-1] = z[j4-2] * temp;
                     d = d * temp;
                 } // else if ((safmin*z[j4] < z[j4-3]) && (safmin*z[j4-3] < z[j4]))
                 else {
                     z[j4-1] = z[j4] * (z[j4-2]/z[j4-3]);
                     d = z[j4] * (d/z[j4-3]);
                 }
                 dmin[0] = Math.min(dmin[0], d);
                 emin = Math.min(emin, z[j4-1]);
             } // for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4)
         } // if (pp == 0)
         else { // pp != 0
             for (j4 = 4*i0; j4 <= 4*(n0 - 3); j4 += 4) {
                 z[j4-4] = d + z[j4-1];
                 if (z[j4-4] == 0.0) {
                     z[j4-2] = 0.0;
                     d = z[j4+1];
                     dmin[0] = d;
                     emin = 0.0;
                 } // if (z[j4-4] == 0.0)
                 else if ((safmin*z[j4+1] < z[j4-4]) && (safmin*z[j4-4] < z[j4+1])) {
                     temp = z[j4+1]/z[j4-4];
                     z[j4-2] = z[j4-1] * temp;
                     d = d * temp;
                 } // else if ((safmin*z[j4+1] < z[j4-4]) && (safmin*z[j4-4] < z[j4+1]))
                 else {
                     z[j4-2] = z[j4+1] * (z[j4-1]/z[j4-4]);
                     d = z[j4+1] * (d/z[j4-4]);
                 }
                 dmin[0] = Math.min(dmin[0], d);
                 emin = Math.min(emin, z[j4-2]);
             } // for (j4 = 4*i0; j4 <= 4*(n0 - 3); j4 += 4)
         } // else pp != 0
         
         // Unroll last 2 steps.
         
         dnm2[0] = d;
         dmin2[0] = dmin[0];
         j4 = 4*(n0-2) - pp;
         j4p2 = j4 + 2*pp - 1;
         z[j4-3] = dnm2[0] + z[j4p2-1];
         if (z[j4-3] == 0.0) {
             z[j4-1] = 0.0;
             dnm1[0] = z[j4p2+1];
             dmin[0] = dnm1[0];
             emin = 0.0;
         } // if (z[j4-3] == 0.0)
         else if ((safmin*z[j4p2+1] < z[j4-3]) && (safmin*z[j4-3] < z[j4p2+1])) {
             temp = z[j4p2+1]/z[j4-3];
             z[j4-1] = z[j4p2-1] * temp;
             dnm1[0] = dnm2[0] * temp;
         } // else if ((safmin*z[j4p2+1] < z[j4-3]) && (safmin*z[j4-3] < z[j4p2+1]))
         else {
             z[j4-1] = z[j4p2+1] * (z[j4p2-1]/z[j4-3]);
             dnm1[0] = z[j4p2+1] * (dnm2[0]/z[j4-3]);
         }
         dmin[0] = Math.min(dmin[0], dnm1[0]);
         
         dmin1[0] = dmin[0];
         j4 = j4 + 4;
         j4p2 = j4 + 2*pp - 1;
         z[j4-3] = dnm1[0] + z[j4p2-1];
         if (z[j4-3] == 0.0) {
             z[j4-1] = 0.0;
             dn[0] = z[j4p2+1];
             dmin[0] = dn[0];
             emin = 0.0;
         } // if (z[j4-3] == 0.0)
         else if ((safmin*z[j4p2+1] < z[j4-3]) && (safmin*z[j4-3] < z[j4p2+1])) {
             temp = z[j4p2+1]/z[j4-3];
             z[j4-1] = z[j4p2-1]*temp;
             dn[0] = dnm1[0] * temp;
         } // else if ((safmin*z[j4p2+1] < z[j4-3]) && (safmin*z[j4-3] < z[j4p2+1]))
         else {
             z[j4-1] = z[j4p2+1] * (z[j4p2-1]/z[j4-3]);
             dn[0] = z[j4p2+1] * (dnm1[0]/z[j4-3]);
         }
         dmin[0] = Math.min(dmin[0], dn[0]);
         
         z[j4+1] = dn[0];
         z[4*n0 - pp - 1] = emin;
         return;
     } // dlasq6
    	            
    /** This is a port of version 3.2 LAPACK routine DORGBR.  Original DORGBR created by Univ. of Tennessee,
     *  Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
    *  Purpose
    *  =======
    *
    *  DORGBR generates one of the real orthogonal matrices Q or P**T
    *  determined by DGEBRD when reducing a real matrix A to bidiagonal
    *  form: A = Q * B * P**T.  Q and P**T are defined as products of
    *  elementary reflectors H(i) or G(i) respectively.
    *
    *  If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
    *  is of order M:
    *  if m >= k, Q = H(1) H(2) . . . H(k) and DORGBR returns the first n
    *  columns of Q, where m >= n >= k;
    *  if m < k, Q = H(1) H(2) . . . H(m-1) and DORGBR returns Q as an
    *  M-by-M matrix.
    *
    *  If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**T
    *  is of order N:
    *  if k < n, P**T = G(k) . . . G(2) G(1) and DORGBR returns the first m
    *  rows of P**T, where n >= m >= k;
    *  if k >= n, P**T = G(n-1) . . . G(2) G(1) and DORGBR returns P**T as
    *  an N-by-N matrix.
    *
    *  Arguments
    *  =========
    *
    *  VECT    (input) CHARACTER*1
    *          Specifies whether the matrix Q or the matrix P**T is
    *          required, as defined in the transformation applied by DGEBRD:
    *          = 'Q':  generate Q;
    *          = 'P':  generate P**T.
    *
    *  M       (input) INTEGER
    *          The number of rows of the matrix Q or P**T to be returned.
    *          M >= 0.
    *
    *  N       (input) INTEGER
    *          The number of columns of the matrix Q or P**T to be returned.
    *          N >= 0.
    *          If VECT = 'Q', M >= N >= min(M,K);
    *          if VECT = 'P', N >= M >= min(N,K).
    *
    *  K       (input) INTEGER
    *          If VECT = 'Q', the number of columns in the original M-by-K
    *          matrix reduced by DGEBRD.
    *          If VECT = 'P', the number of rows in the original K-by-N
    *          matrix reduced by DGEBRD.
    *          K >= 0.
    *
    *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
    *          On entry, the vectors which define the elementary reflectors,
    *          as returned by DGEBRD.
    *          On exit, the M-by-N matrix Q or P**T.
    *
    *  LDA     (input) INTEGER
    *          The leading dimension of the array A. LDA >= max(1,M).
    *
    *  TAU     (input) DOUBLE PRECISION array, dimension
    *                                (min(M,K)) if VECT = 'Q'
    *                                (min(N,K)) if VECT = 'P'
    *          TAU(i) must contain the scalar factor of the elementary
    *          reflector H(i) or G(i), which determines Q or P**T, as
    *          returned by DGEBRD in its array argument TAUQ or TAUP.
    *
    *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
    *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
    *
    *  LWORK   (input) INTEGER
    *          The dimension of the array WORK. LWORK >= max(1,min(M,N)).
    *          For optimum performance LWORK >= min(M,N)*NB, where NB
    *          is the optimal blocksize.
    *
    *          If LWORK = -1, then a workspace query is assumed; the routine
    *          only calculates the optimal size of the WORK array, returns
    *          this value as the first entry of the WORK array, and no error
    *          message related to LWORK is issued by XERBLA.
    *
    *  INFO    (output) INTEGER
    *          = 0:  successful exit
    *          < 0:  if INFO = -i, the i-th argument had an illegal value
    */
    private void dorgbr(char vect, int m, int n, int k, double A[][], int lda, double tau[],
                        double work[], int lwork, int info[]) {
        boolean lquery;
        boolean wantq;
        int i;
        int iinfo[] = new int[1];
        int j;
        int lwkopt = 0;
        int mn;
        int nb;
        double array1[][];
        int p;
        int q;
        
        // Test the input arguments
        info[0] = 0;
        wantq = ((vect == 'Q' ) || (vect == 'q'));
        mn = Math.min(m, n);
        lquery = (lwork == -1);
        if ((!wantq) && (vect != 'P') && (vect != 'p')) {
            info[0] = -1;
        }
        else if (m < 0) {
            info[0] = -2;
        }
        else if ((n < 0) || (wantq && ((n > m) || (n < Math.min(m, k)))) ||
                ((!wantq) && ((m > n) || (m < Math.min(n, k))))) {
            info[0] = -3;
        }
        else if (k < 0) {
            info[0] = -4;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -6;
        }
        else if ((lwork < Math.max(1, mn)) && (!lquery)) {
            info[0] = -9;
        }
        
        if (info[0] == 0) {
            if (wantq) {
                nb = ilaenv(1, "DORGQR", " ", m, n, k, -1);
            }
            else {
                nb = ilaenv(1, "DORGLQ", " ", m, n, k, -1);
            }
            lwkopt = Math.max(1, mn) * nb;
            work[0] = lwkopt;
        } // if (info[0] == 0)
        
        if (info[0] != 0) {
            MipavUtil.displayError("Error dorgbr had info[0] = " + info[0]);
            return;
        }
        else if (lquery) {
            return;
        }
        
        // Quick return if possible
        if ((m == 0) || (n == 0)) {
            work[0] = 1;
            return;
        }
        
        if (wantq) {
            // Form Q, determined by a call to dgebrd to reduce an m-by-k matrix
            if (m >= k) {
                // If m >= k, assume m >= n >= k
                dorgqr(m, n, k, A, lda, tau, work, lwork, iinfo);
            } // if (m >= k)
            else { // m < k
                // If m < k, assume m = n
                
                // Shift the vectors which define the elementary reflectors one 
                // column to the right, and set the first row and column of Q
                // to those of the unit matrix
                
                for (j = m; j >= 2; j--) {
                    A[0][j-1] = 0.0;
                    for (i = j+1; i <= m; i++) {
                        A[i-1][j-1] = A[i-1][j-2];
                    } // for (i = j+1; i <= m; i++)
                } // for (j = m; j >= 2; j--)
                A[0][0] = 1.0;
                for (i = 2; i <= m; i++) {
                    A[i-1][0] = 0.0;
                }
                if (m > 1) {
                    // Form Q(2:m,2:m)
                    array1 = new double[m-1][m-1];
                    for (p = 0; p < m-1; p++) {
                        for (q = 0; q < m-1; q++) {
                            array1[p][q] = A[1 + p][1+q];
                        }
                    }
                    dorgqr(m-1, m-1, m-1, array1, m-1, tau, work, lwork, iinfo);
                    for (p = 0; p < m-1; p++) {
                        for (q = 0; q < m-1; q++) {
                            A[1 + p][1+q] = array1[p][q];
                        }
                    }
                } // if (m > 1)
            } //  else m < k
        } // if (wantq)
        else { // (!wantq)
            // Form P', determined by a call to dgebrd to reduce a k-by-n matrix
            if (k < n) {
                // If k < n, assume k <= m <= n
                dorglq(m, n, k, A, lda, tau, work, lwork, iinfo);
            }
            else { // k >= n
                // If k >= n, assume m = n
                
                // Shift the vectors which define the elementary reflectors one
                // row downward, and set the first row and column of P' to
                // those of the unit matrix
                A[0][0] = 1.0;
                for (i = 1; i < n; i++) {
                    A[i][0] = 0.0;
                }
                for (j = 2; j <= n; j++) {
                    for (i = j - 1; i >= 2; i--) {
                        A[i-1][j-1] = A[i-2][j-1];
                    } // for (i = j - 1; i >= 2; i--)
                    A[0][j-1] = 0.0;
                } // for (j = 2; j <= n; j++)
                if (n > 1) {
                    // Form P'(2:n,2:n)
                    array1 = new double[n-1][n-1];
                    for (p = 0; p < n-1; p++) {
                        for (q = 0; q < n-1; q++) {
                            array1[p][q] = A[1+p][1+q];
                        }
                    }
                    dorglq(n-1, n-1, n-1, array1, n-1, tau, work, lwork, iinfo);
                    for (p = 0; p < n-1; p++) {
                        for (q = 0; q < n-1; q++) {
                            A[1+p][1+q] = array1[p][q];
                        }
                    }
                } // if (n > 1)
            } // else k >= n
        } // else (!wantq)
        work[0] = lwkopt;
        return;
    } // dorgbr 
    
    /**
     * This is a port of version 3.2 LAPACK routine DORGQR Original DORGQR created by Univ. of Tennessee, Univ. of
     * California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dorgqr generates an m-by-n real matrix Q with orthonormal columns, which is defined as the first n columns
     * of a product of k elementary reflectors of order m 
     *  Q = H[0] H[1] ... H[k-1] as returned by dgeqrf.
     *
     * @param  m      input int The number of rows of the matrix Q. m >= 0.
     * @param  n      input int The number of columns of the matrix Q. m >= n >= 0.
     * @param  k      input int The number of elementary reflectors whose product defines the matrix Q. n >= k >= 0.
     * @param  A      input/output double[][] of dimensions lda by n. On entry, the i-th column must contain the vector
     *                which defines the elementary reflector H[i], for i = 0, 1, ..., k-1, as returned by dgeqrf in the
     *                first k columns of its array argument A. On exit, the m-by-n matrix Q.
     * @param  lda    input int The first dimension of the array A. lda >= max(1,m).
     * @param  tau    input double[] of dimension k. tau[i] must contain the scalar factor of the elementary reflector
     *                H[i], as returned by dgeqrf.
     * @param  work   (worksplace/output) double[] of dimension max(1,lwork). 
     *                On exit, if info[0] = 0, work[0] returns the optimal lwork.
     * @param  lwork  input int The dimension of the array work. lwork >= max(1,n). For optimum performance lwork >=
     *                n*nb, where nb is the optimal blocksize. If lwork = -1, then a workspace query is assumed; the
     *                routine only calculates the optimal size of the work array, returns this value as the first entry
     *                of the work array, and no error message related to lwork is issued.
     * @param  info   output int[] 
     *                = 0: successful exit 
     *                < 0: If info = -i, the i-th argument has an illegal value
     */
    private void dorgqr(int m, int n, int k, double[][] A, int lda, double[] tau, double[] work, int lwork,
                        int[] info) {
        boolean lquery;
        int i;
        int ib;
        int[] iinfo = new int[1];
        int iws;
        int j;
        int ki = 1;
        int kk;
        int L;
        int ldwork;
        int lwkopt;
        int nb;
        int nbmin;
        int nx;
        double[][] array1;
        double[][] array2;
        double[][] array3;
        double[][] array4;
        double[] vector1;
        int p;
        int q;
        int row1;

        // Test the input arguments
        info[0] = 0;
        nb = ilaenv(1, "DORGQR", " ", m, n, k, -1);
        lwkopt = Math.max(1, n) * nb;
        work[0] = lwkopt;

        lquery = (lwork == -1);

        if (m < 0) {
            info[0] = -1;
        } else if ((n < 0) || (n > m)) {
            info[0] = -2;
        } else if ((k < 0) || (k > n)) {
            info[0] = -3;
        } else if (lda < Math.max(1, m)) {
            info[0] = -5;
        } else if ((lwork < Math.max(1, n)) && (!lquery)) {
            info[0] = -8;
        }

        if (info[0] != 0) {
            MipavUtil.displayError("Error dorgqr had info = " + info[0]);

            return;
        } else if (lquery) {
            return;
        }

        // Quick return if possible
        if (n <= 0) {
            work[0] = 1;

            return;
        }

        nbmin = 2;
        nx = 0;
        iws = n;

        if ((nb > 1) && (nb < k)) {

            // Determine when to cross over from blocked to unblocked code
            nx = Math.max(0, ilaenv(3, "DORGQR", " ", m, n, k, -1));

            if (nx < k) {

                // Determine if workspace is large enough for blocked code.
                ldwork = n;
                iws = ldwork * nb;

                if (lwork < iws) {

                    // Not enough workspace to use optimal nb: reduce nb and determine
                    // the minimum value of nb.
                    nb = lwork / ldwork;
                    nbmin = Math.max(2, ilaenv(2, "DORGQR", " ", m, n, k, -1));
                } // if (lwork < iws)
            } // if (nx < k)
        } // if ((nb > 1) && (nb < k))

        if ((nb >= nbmin) && (nb < k) && (nx < k)) {

            // Use blocked code after the last block.
            // The first kk columns are handled by the block method
            ki = ((k - nx - 1) / nb) * nb;
            kk = Math.min(k, ki + nb);

            // Set A(0:kk-1,kk:n-1) to zero.

            for (j = kk; j < n; j++) {

                for (i = 0; i < kk; i++) {
                    A[i][j] = 0.0;
                }
            }
        } // if ((nb >= nbmin) && (nb < k) && (nx < k))
        else {
            kk = 0;
        }

        // Use unblocked code for the last or only block
        if (kk < n) {
            row1 = Math.max(1, m - kk);
            array1 = new double[row1][n - kk];

            for (p = 0; p < row1; p++) {

                for (q = 0; q < (n - kk); q++) {
                    array1[p][q] = A[p + kk][q + kk];
                }
            }

            vector1 = new double[k - kk];

            for (p = 0; p < (k - kk); p++) {
                vector1[p] = tau[p + kk];
            }

            dorg2r(m - kk, n - kk, k - kk, array1, row1, vector1, work, iinfo);

            for (p = 0; p < row1; p++) {

                for (q = 0; q < (n - kk); q++) {
                    A[p + kk][q + kk] = array1[p][q];
                }
            }
        } // if (kk < n)

        if (kk > 0) {

            // Use blocked code
            for (i = ki + 1; i >= 1; i -= nb) {
                ib = Math.min(nb, k - i + 1);

                if ((i + ib) <= n) {

                    // Form the triangular factor of the block reflector
                    // H = H[i-1] H[i] ... H[i+ib-2]
                    array1 = new double[m - i + 1][ib];

                    for (p = 0; p < (m - i + 1); p++) {

                        for (q = 0; q < ib; q++) {
                            array1[p][q] = A[p + i - 1][q + i - 1];
                        }
                    }

                    vector1 = new double[ib];

                    for (p = 0; p < ib; p++) {
                        vector1[p] = tau[p + i - 1];
                    }

                    array2 = new double[ib][ib];
                    dlarft('F', 'C', m - i + 1, ib, array1, m - i + 1, vector1, array2, ib);

                    for (p = 0; p < (m - i + 1); p++) {

                        for (q = 0; q < ib; q++) {
                            A[p + i - 1][q + i - 1] = array1[p][q];
                        }
                    }

                    // Apply H to A(i-1:m-1, i+ib-1:n-1) from the left
                    array3 = new double[m - i + 1][n - i - ib + 1];

                    for (p = 0; p < (m - i + 1); p++) {

                        for (q = 0; q < (n - i - ib + 1); q++) {
                            array3[p][q] = A[p + i - 1][q + i + ib - 1];
                        }
                    }

                    array4 = new double[n - i - ib + 1][ib];
                    dlarfb('L', 'N', 'F', 'C', m - i + 1, n - i - ib + 1, ib, array1, m - i + 1, array2, ib, array3,
                           m - i + 1, array4, n - i - ib + 1);

                    for (p = 0; p < (m - i + 1); p++) {

                        for (q = 0; q < (n - i - ib + 1); q++) {
                            A[p + i - 1][q + i + ib - 1] = array3[p][q];
                        }
                    }
                } // if ((i+ib) <= n)

                // Apply H to rows i-1:m-1 of current block
                array1 = new double[m - i + 1][ib];

                for (p = 0; p < (m - i + 1); p++) {

                    for (q = 0; q < ib; q++) {
                        array1[p][q] = A[p + i - 1][q + i - 1];
                    }
                }

                vector1 = new double[ib];

                for (p = 0; p < ib; p++) {
                    vector1[p] = tau[p + i - 1];
                }

                dorg2r(m - i + 1, ib, ib, array1, m - i + 1, vector1, work, iinfo);

                for (p = 0; p < (m - i + 1); p++) {

                    for (q = 0; q < ib; q++) {
                        A[p + i - 1][q + i - 1] = array1[p][q];
                    }
                }

                // Set rows 0:i-2 of current block to zero
                for (j = i; j <= (i + ib - 1); j++) {

                    for (L = 1; L <= (i - 1); L++) {
                        A[L - 1][j - 1] = 0.0;
                    }
                }
            } // for (i = ki+1; i >= 1; i -= nb)
        } // if (kk > 0)

        work[0] = iws;

        return;
    } // dorgqr
    
    /**
     * This is a port of version 3.2 LAPACK routine DORG2R Original DORG2R created by Univ. of Tennessee, Univ. of
     * California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dorg2r generates an m by n real matrix Q with orthonormal columns, which is defined as the first n columns of a
     * product of k elementary reflectors of order m
     *  Q = H[0] H[1] ... H[k-1] as returned by dgeqrf.
     *
     * @param  m     input int The number of rows of the matrix Q. m >= 0.
     * @param  n     input int The number of columns of the matrix Q. m >= n >= 0.
     * @param  k     input int The number of elementary reflectors whose product defines the matrix Q. n >= k >= 0.
     * @param  A     input/output double[][] of dimension lda by n. On entry, the i-th column must contain the vector
     *               which defines the elementary reflector H[i], for i = 0, 1, ..., k-1, as returned by dgeqrf in the
     *               first k columns of its array argument A. On exit, the m-by-n matrix Q.
     * @param  lda   input int The first dimension of the array A. lda >= max(1,m).
     * @param  tau   input double[] of dimension k. tau[i] must contain the scalar factor of the elementary reflector
     *               H[i], as returned by dgeqrf.
     * @param  work  workspace double[] of dimension n.
     * @param  info  output int[] 
     *               = 0: successful exit 
     *               < 0: If info = -i, the i-th argument has an illegal value.
     */
    private void dorg2r(int m, int n, int k, double[][] A, int lda, double[] tau, double[] work, int[] info) {
        int i;
        int j;
        int L;
        double[] vector1;
        double[][] array1;
        int p;
        int q;

        // Test the input arguments
        info[0] = 0;

        if (m < 0) {
            info[0] = -1;
        } else if ((n < 0) || (n > m)) {
            info[0] = -2;
        } else if ((k < 0) || (k > n)) {
            info[0] = -3;
        } else if (lda < Math.max(1, m)) {
            info[0] = -5;
        }

        if (info[0] != 0) {
            MipavUtil.displayError("Error dorg2r had info = " + info[0]);

            return;
        }

        // Quick return if possible
        if (n <= 0) {
            return;
        }

        // Initialize columns k:n-1 to columns of the unit matrix
        for (j = k; j < n; j++) {

            for (L = 0; L < m; L++) {
                A[L][j] = 0.0;
            }

            A[j][j] = 1.0;
        } // for (j = k; j < n; j++)

        for (i = k; i >= 1; i--) {

            // Apply H[i-1] to A(i-1:m-1,i-1:n-1) from the left
            if (i < n) {
                A[i - 1][i - 1] = 1.0;
                vector1 = new double[m - i + 1];

                for (p = 0; p < (m - i + 1); p++) {
                    vector1[p] = A[p + i - 1][i - 1];
                }

                array1 = new double[m - i + 1][n - i];

                for (p = 0; p < (m - i + 1); p++) {

                    for (q = 0; q < (n - i); q++) {
                        array1[p][q] = A[p + i - 1][q + i];
                    }
                }

                dlarf('L', m - i + 1, n - i, vector1, 1, tau[i - 1], array1, m - i + 1, work);

                for (p = 0; p < (m - i + 1); p++) {

                    for (q = 0; q < (n - i); q++) {
                        A[p + i - 1][q + i] = array1[p][q];
                    }
                }
            } // if (i < n)

            if (i < m) {
                vector1 = new double[m - i];

                for (p = 0; p < (m - i); p++) {
                    vector1[p] = A[p + i][i - 1];
                }

                dscal(m - i, -tau[i - 1], vector1, 1);

                for (p = 0; p < (m - i); p++) {
                    A[p + i][i - 1] = vector1[p];
                }
            } // if (i < m)

            A[i - 1][i - 1] = 1.0 - tau[i - 1];

            // Set A(0:i-2, i-1) to zero
            for (L = 1; L <= (i - 1); L++) {
                A[L - 1][i - 1] = 0.0;
            }

        } // for (i = k; i >= 1; i--)

        return;
    } // dorg2r
    
    /** This is a port of version 3.2 LAPACK routine DORGLQ.  Original DORGLQ created by Univ. of Tennessee,
     *  Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
    *  Purpose
    *  =======
    *
    *  DORGLQ generates an M-by-N real matrix Q with orthonormal rows,
    *  which is defined as the first M rows of a product of K elementary
    *  reflectors of order N
    *
    *        Q  =  H(k) . . . H(2) H(1)
    *
    *  as returned by DGELQF.
    *
    *  Arguments
    *  =========
    *
    *  M       (input) INTEGER
    *          The number of rows of the matrix Q. M >= 0.
    *
    *  N       (input) INTEGER
    *          The number of columns of the matrix Q. N >= M.
    *
    *  K       (input) INTEGER
    *          The number of elementary reflectors whose product defines the
    *          matrix Q. M >= K >= 0.
    *
    *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
    *          On entry, the i-th row must contain the vector which defines
    *          the elementary reflector H(i), for i = 1,2,...,k, as returned
    *          by DGELQF in the first k rows of its array argument A.
    *          On exit, the M-by-N matrix Q.
    *
    *  LDA     (input) INTEGER
    *          The first dimension of the array A. LDA >= max(1,M).
    *
    *  TAU     (input) DOUBLE PRECISION array, dimension (K)
    *          TAU(i) must contain the scalar factor of the elementary
    *          reflector H(i), as returned by DGELQF.
    *
    *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
    *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
    *
    *  LWORK   (input) INTEGER
    *          The dimension of the array WORK. LWORK >= max(1,M).
    *          For optimum performance LWORK >= M*NB, where NB is
    *          the optimal blocksize.
    *
    *          If LWORK = -1, then a workspace query is assumed; the routine
    *          only calculates the optimal size of the WORK array, returns
    *          this value as the first entry of the WORK array, and no error
    *          message related to LWORK is issued by XERBLA.
    *
    *  INFO    (output) INTEGER
    *          = 0:  successful exit
    *          < 0:  if INFO = -i, the i-th argument has an illegal value
    */
    private void dorglq(int m, int n, int k, double A[][], int lda, double tau[], double work[],
                        int lwork, int info[]) {
        boolean lquery;
        int i;
        int ib;
        int iinfo[] = new int[1];
        int iws;
        int j;
        int ki = 0;
        int kk;
        int L;
        int ldwork = 0;
        int lwkopt;
        int nb; 
        int nbmin;
        int nx;
        String name;
        String opts;
        double v1[];
        int row1;
        double array1[][];
        int p;
        int q;
        double work2[][];
        double work3[][];
        int row2;
        double array2[][];
        
        // Test the input arguments
        info[0] = 0;
        name = new String("DORGLQ");
        opts = new String(" ");
        nb = ilaenv(1, name, opts, m, n, k, -1);
        lwkopt = Math.max(1, m) * nb;
        work[0] = lwkopt;
        lquery = (lwork == -1);
        if (m < 0) {
            info[0] = -1;
        }
        else if (n < m) {
            info[0] = -2;
        }
        else if ((k < 0) || (k > m)) {
            info[0] = -3;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -5;
        }
        else if ((lwork < Math.max(1, m)) && (!lquery)) {
            info[0] = -8;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("Error dorglq had info[0] = " + info[0]);
            return;
        }
        else if (lquery) {
            return;
        }
        
        // Quick return if possible
        if (m <= 0) {
            work[0] = 1;
            return;
        }
        
        nbmin = 2;
        nx = 0;
        iws = m;
        if ((nb > 1) && (nb < k)) {
            // Determine when to corss over from blocked to unblocked code
            nx = Math.max(0, ilaenv(3, name, opts, m, n, k, -1));
            if (nx < k) {
                // Determine if workspace is large enough for blocked code.
                ldwork = m;
                iws = ldwork * nb;
                if (lwork < iws) {
                    // Not enough workspace to use optimal nb: reduce nb and 
                    // determine the minimum value of nb
                    nb = lwork/ldwork;
                    nbmin = Math.max(2, ilaenv(2, name, opts, m, n, k, -1));
                } // if (lwork < iws)
            } // if (nx < k)
        } // if ((nb > 1) && (nb < k))
        
        if ((nb >= nbmin) && (nb < k) && (nx < k)) {
            // Use blocked code after the last block.
            // The first kk rows are handled by the block method.
            ki = ((k-nx-1)/nb)*nb;
            kk = Math.min(k, ki+nb);
            
            // Set A(kk+1:m,1:kk) to zero.
            
            for (j = 1; j <= kk; j++) {
                for (i = kk+1; i <= m; i++) {
                    A[i-1][j-1] = 0.0;
                }
            }
        } // if ((nb >= nbmin) && (nb < k) && (nx < k))
        else {
            kk = 0;
        }
        
        // Use unblocked code for the last or only block
        if (kk < m) {
            row1 = Math.max(1, m - kk);
            array1 = new double[row1][n-kk];
            for (p = 0; p < row1; p++) {
                for (q = 0; q < n-kk; q++) {
                    array1[p][q] = A[kk+p][kk+q];
                }
            }
            v1 = new double[k-kk];
            for (p = 0; p < k-kk; p++) {
                v1[p] = tau[kk+p];
            }
            dorgl2(m-kk, n-kk, k-kk, array1, row1, v1, work, iinfo);
            for (p = 0; p < row1; p++) {
                for (q = 0; q < n-kk; q++) {
                    A[kk+p][kk+q] = array1[p][q];
                }
            }
        } // if (kk < m)
        
        if (kk > 0) {
            // Use blocked code
            for (i = ki+1; i >= 1; i -= nb) {
                ib = Math.min(nb, k-i+1);
                if ((i+ib) <= m) {
                    // Form the triangular factor of the block reflector
                    // H = H(i) H(i+1) ... H(i+ib-1)
                    array1 = new double[ib][n-i+1];
                    for (p = 0; p < ib; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            array1[p][q] = A[i-1+p][i-1+q];
                        }
                    }
                    v1 = new double[ib];
                    for (p = 0; p < ib; p++) {
                        v1[p] = tau[i-1+p];
                    }
                    work2 = new double[ldwork][ib];
                    dlarft('F', 'R', n-i+1, ib, array1, ib, v1, work2, ldwork);
                    for (p = 0; p < ib; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            A[i-1+p][i-1+q] = array1[p][q];
                        }
                    }
                    for (q = 0; q < ib; q++) {
                        for (p = 0; p < ldwork; p++) {
                            work[p + q * ldwork] = work2[p][q];
                        }
                    }
                    
                    array1 = new double[ib][n-i+1];
                    for (p = 0; p < ib; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            array1[p][q] = A[i-1+p][i-1+q];
                        }
                    }
                    row2 = Math.max(1, m-i-ib+1);
                    array2 = new double[row2][n-i+1];
                    for (p = 0; p < row2; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            array2[p][q] = A[i+ib-1+p][i-1+q];
                        }
                    }
                    work3 = new double[row2][ib];
                    dlarfb('R', 'T', 'F', 'R', m-i-ib+1, n-i+1, ib, array1, ib, work2, ldwork,
                            array2, row2, work2, row2);
                    for (p = 0; p < row2; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            A[i+ib-1+p][i-1+q] = array2[p][q];
                        }
                    }
                } // if ((i+ib) <= m)
                
                // Apply H' to columns i:n of current block
                row1 = Math.max(1, ib);
                array1 = new double[row1][n-i+1];
                for (p = 0; p < row1; p++) {
                    for (q = 0; q < n-i+1; q++) {
                        array1[p][q] = A[i-1+p][i-1+q];
                    }
                }
                v1 = new double[ib];
                for (p = 0; p < ib; p++) {
                    v1[p] = tau[i-1+p];
                }
                dorgl2(ib, n-i+1, ib, array1, row1, v1, work, iinfo);
                for (p = 0; p < row1; p++) {
                    for (q = 0; q < n-i+1; q++) {
                        A[i-1+p][i-1+q] = array1[p][q];
                    }
                }
                
                // Set columns 1:i-1 of current block to zero.
                
                for (j = 1; j <= i-1; j++) {
                    for (L = i; L <= i+ib-1; L++) {
                        A[L-1][j-1] = 0.0;
                    }
                }
            } // for (i = ki+1; i >= 1; i -= nb)
        } // if (kk > 0)
        
        work[0] = iws;
        return;
    } // dorglq

    
    /*  This is a port of version 3.2 LAPACK routine DORGL2.  Original DORGL2 created by Univ. of Tennessee,
     *  Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
    *  Purpose
    *  =======
    *
    *  DORGL2 generates an m by n real matrix Q with orthonormal rows,
    *  which is defined as the first m rows of a product of k elementary
    *  reflectors of order n
    *
    *        Q  =  H(k) . . . H(2) H(1)
    *
    *  as returned by DGELQF.
    *
    *  Arguments
    *  =========
    *
    *  M       (input) INTEGER
    *          The number of rows of the matrix Q. M >= 0.
    *
    *  N       (input) INTEGER
    *          The number of columns of the matrix Q. N >= M.
    *
    *  K       (input) INTEGER
    *          The number of elementary reflectors whose product defines the
    *          matrix Q. M >= K >= 0.
    *
    *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
    *          On entry, the i-th row must contain the vector which defines
    *          the elementary reflector H(i), for i = 1,2,...,k, as returned
    *          by DGELQF in the first k rows of its array argument A.
    *          On exit, the m-by-n matrix Q.
    *
    *  LDA     (input) INTEGER
    *          The first dimension of the array A. LDA >= max(1,M).
    *
    *  TAU     (input) DOUBLE PRECISION array, dimension (K)
    *          TAU(i) must contain the scalar factor of the elementary
    *          reflector H(i), as returned by DGELQF.
    *
    *  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
    *
    *  INFO    (output) INTEGER
    *          = 0: successful exit
    *          < 0: if INFO = -i, the i-th argument has an illegal value
    */
    private void dorgl2(int m, int n, int k, double A[][], int lda, double tau[], double work[], int info[]) {
        int i;
        int j;
        int L;
        double v1[];
        int row1;
        double array1[][];
        int p;
        int q;
        
        // Test the input arguments
        info[0] = 0;
        if (m < 0) {
            info[0] = -1;
        }
        else if (n < m) {
            info[0] = -2;
        }
        else if ((k < 0) || (k > m)) {
            info[0] = -3;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -5;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("Error dorgl2 had info[0] = " + info[0]);
            return;
        }
        
        // Quick return if possible
        if (m <= 0) {
            return;
        }
        
        if (k < m) {
            // Initialize rows k+1:m to rows of the unit matrix
            for (j = 1; j <= n; j++) {
                for (L = k+1; L <= m; L++) {
                    A[L-1][j-1] = 0;
                }
                if ((j > k) && (j <= m)) {
                    A[j-1][j-1] = 1.0;
                }
            } // for (j = 1; j <= n; j++) 
        } // if (k < m)
        
        for (i = k; i >= 1; i--) {
            // Apply H(i) to A(i:m,i;n) from the right
            
            if (i < n) {
                if (i < m) {
                    A[i-1][i-1] = 1.0;
                    v1 = new double[n-i+1];
                    for (p = 0; p < n - i + 1; p++) {
                        v1[p] = A[i-1][i-1+p];
                    }
                    row1 = Math.max(1, m - i);
                    array1 = new double[row1][n-i+1];
                    for (p = 0; p < row1; p++) {
                        for (q = 0; q < n - i + 1; q++) {
                            array1[p][q] = A[i + p][i-1+q];
                        }
                    }
                    dlarf('R', m - i, n - i + 1, v1, 1, tau[i-1], array1, row1, work);
                    for (p = 0; p < row1; p++) {
                        for (q = 0; q < n - i + 1; q++) {
                            A[i + p][i-1+q] = array1[p][q];
                        }
                    }
                } // if (i < m)
                for (p = 0; p < n-i; p++) {
                    A[i-1][i+p] = -tau[i-1] * A[i-1][i+p];
                }
            } // if (i < n)
            A[i-1][i-1] = 1.0 - tau[i-1];
            
            // Set A(i,1:i-1) to zero
            for (L = 1; L <= i-1; L++) {
                A[i-1][L-1] = 0.0;
            }
        } // for (i = k; i >= 1; i--)
        return;
    } // dorgl2
    
    /**
     * Routine ported from 12/3/93 linpack dscal Original version written by Jack Dongarra Scales a vector by a
     * constant.
     *
     * @param  n     int
     * @param  da    double
     * @param  dx    double[]
     * @param  incx  int
     */
    private void dscal(int n, double da, double[] dx, int incx) {
        int nincx;
        int i;
        int m;
        int mp1;

        if ((n <= 0) || (incx <= 0)) {
            return;
        }

        if (incx != 1) {

            // Code for increment not equal to 1
            nincx = n * incx;

            for (i = 0; i < nincx; i += incx) {
                dx[i] = da * dx[i];
            } // for (i = 0; i < nincx; i += incx)

            return;
        } // if (incx != 1)

        // Code for increment equal to 1
        m = n % 5;

        if (m != 0) {

            for (i = 0; i < m; i++) {
                dx[i] = da * dx[i];
            }

            if (n < 5) {
                return;
            }
        } // if (m != 0)

        mp1 = m + 1;

        for (i = mp1; i <= n; i += 5) {
            dx[i - 1] = da * dx[i - 1];
            dx[i] = da * dx[i];
            dx[i + 1] = da * dx[i + 1];
            dx[i + 2] = da * dx[i + 2];
            dx[i + 3] = da * dx[i + 3];
        } // for (i = mp1; i <= n; i+= 5)

        return;
    } // dscal
    	            
    /* This is a port of version 3.2 LAPACK routine DGEBRD.  Original DGEBRD created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
       *  Purpose
       *  =======
       *
       *  DGEBRD reduces a general real M-by-N matrix A to upper or lower
       *  bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
       *
       *  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
       *
       *  Arguments
       *  =========
       *
       *  M       (input) INTEGER
       *          The number of rows in the matrix A.  M >= 0.
       *
       *  N       (input) INTEGER
       *          The number of columns in the matrix A.  N >= 0.
       *
       *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
       *          On entry, the M-by-N general matrix to be reduced.
       *          On exit,
       *          if m >= n, the diagonal and the first superdiagonal are
       *            overwritten with the upper bidiagonal matrix B; the
       *            elements below the diagonal, with the array TAUQ, represent
       *            the orthogonal matrix Q as a product of elementary
       *            reflectors, and the elements above the first superdiagonal,
       *            with the array TAUP, represent the orthogonal matrix P as
       *            a product of elementary reflectors;
       *          if m < n, the diagonal and the first subdiagonal are
       *            overwritten with the lower bidiagonal matrix B; the
       *            elements below the first subdiagonal, with the array TAUQ,
       *            represent the orthogonal matrix Q as a product of
       *            elementary reflectors, and the elements above the diagonal,
       *            with the array TAUP, represent the orthogonal matrix P as
       *            a product of elementary reflectors.
       *          See Further Details.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A.  LDA >= max(1,M).
       *
       *  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
       *          The diagonal elements of the bidiagonal matrix B:
       *          D(i) = A(i,i).
       *
       *  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
       *          The off-diagonal elements of the bidiagonal matrix B:
       *          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
       *          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
       *
       *  TAUQ    (output) DOUBLE PRECISION array dimension (min(M,N))
       *          The scalar factors of the elementary reflectors which
       *          represent the orthogonal matrix Q. See Further Details.
       *
       *  TAUP    (output) DOUBLE PRECISION array, dimension (min(M,N))
       *          The scalar factors of the elementary reflectors which
       *          represent the orthogonal matrix P. See Further Details.
       *
       *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
       *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
       *
       *  LWORK   (input) INTEGER
       *          The length of the array WORK.  LWORK >= max(1,M,N).
       *          For optimum performance LWORK >= (M+N)*NB, where NB
       *          is the optimal blocksize.
       *
       *          If LWORK = -1, then a workspace query is assumed; the routine
       *          only calculates the optimal size of the WORK array, returns
       *          this value as the first entry of the WORK array, and no error
       *          message related to LWORK is issued by XERBLA.
       *
       *  INFO    (output) INTEGER
       *          = 0:  successful exit
       *          < 0:  if INFO = -i, the i-th argument had an illegal value.
       *
       *  Further Details
       *  ===============
       *
       *  The matrices Q and P are represented as products of elementary
       *  reflectors:
       *
       *  If m >= n,
       *
       *     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
       *
       *  Each H(i) and G(i) has the form:
       *
       *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
       *
       *  where tauq and taup are real scalars, and v and u are real vectors;
       *  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
       *  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
       *  tauq is stored in TAUQ(i) and taup in TAUP(i).
       *
       *  If m < n,
       *
       *     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
       *
       *  Each H(i) and G(i) has the form:
       *
       *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
       *
       *  where tauq and taup are real scalars, and v and u are real vectors;
       *  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
       *  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
       *  tauq is stored in TAUQ(i) and taup in TAUP(i).
       *
       *  The contents of A on exit are illustrated by the following examples:
       *
       *  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
       *
       *    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
       *    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
       *    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
       *    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
       *    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
       *    (  v1  v2  v3  v4  v5 )
       *
       *  where d and e denote diagonal and off-diagonal elements of B, vi
       *  denotes an element of the vector defining H(i), and ui an element of
       *  the vector defining G(i).
       */
    private void dgebrd(int m, int n, double A[][], int lda, double d[], double e[], double tauq[],
                        double taup[], double work[], int lwork, int info[]) {
        int i;
        int iinfo[] = new int[1];
        int j;
        int ldwrkx;
        int ldwrky;
        int lwkopt;
        int minmn;
        int nb;
        int nbmin;
        int nx;
        double ws;
        String name;
        String opts;
        boolean lquery;
        int row1;
        int row3;
        double array1[][];
        double array2[][];
        double array3[][];
        int k;
        double v1[];
        double v2[];
        double v3[];
        double v4[];
        double work1[][];
        double work2[][];
        int dimv;
        
        // Test the input parameters
        info[0] = 0;
        name = new String("DGEBRD");
        opts = new String(" ");
        nb = Math.max(1, ilaenv(1, name, opts, m, n, -1, -1));
        lwkopt = (m + n) * nb;
        work[0] = lwkopt;
        lquery = (lwork == -1);
        if (m < 0) {
            info[0] = -1;
        }
        else if (n < 0) {
            info[0] = -2;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -4;
        }
        else if ((lwork < Math.max(1, Math.max(m, n))) && (!lquery)) {
            info[0] = -10;
        }
        
        if (info[0] < 0) {
            MipavUtil.displayError("dgebrd had info[0] = " + info[0]);
            return;
        }
        else if (lquery) {
            return;
        }
        
        // Quick return if possible
        minmn = Math.min(m, n);
        if (minmn == 0) {
            work[0] = 1;
            return;
        }
        
        ws = Math.max(m, n);
        ldwrkx = m;
        ldwrky = n;
        
        if ((nb > 1) && (nb < minmn)) {
            // Set the crossover point nx.
            
            nx = Math.max(nb, ilaenv(3, name, opts, m, n, -1, -1));
            
            // Determine when to switch from blocked to unblocked code.
            if (nx < minmn) {
                ws = (m + n) * nb;
                if (lwork < ws) {
                    // Not enough space for the optimal nb, consider using a smaller block size.
                    
                    nbmin = ilaenv(2, name, opts, m, n, -1, -1);
                    if (lwork >= (m+n)*nbmin) {
                        nb = lwork/(m+n);
                    }
                    else {
                        nb = 1;
                        nx = minmn;
                    }
                } // if (lwork < ws)
            } // if (nx < minmn)
        } // if ((nb > 1) && (nb < minmn))
        else {
            nx = minmn;
        }
     
        for (i = 1; i <= minmn - nx; i += nb) {
            // Reduce rows and column i:i+nb-1 to bidiagonal form and return the matrices X and Y 
            // which are needed to update the unreduced part of the matrix
            row1 = Math.max(1,m-i+1);
            array1 = new double[row1][n-i+1];
            for (j = 0; j < row1; j++) {
                for (k = 0; k < n-i+1; k++) {
                    array1[j][k] = A[i-1+j][i-1+k];
                }
            }
            v1 = new double[nb];
            v2 = new double[nb];
            v3 = new double[nb];
            v4 = new double[nb];
            work1 = new double[ldwrkx][nb];
            work2 = new double[ldwrky][nb];
            dlabrd(m-i+1, n-i+1, nb, array1, row1, v1, v2, v3, v4, work1, ldwrkx, work2, ldwrky);
            for (j = 0; j < row1; j++) {
                for (k = 0; k < n-i+1; k++) {
                    A[i-1+j][i-1+k] = array1[j][k];
                }
            }
            for (j = 0; j < nb; j++) {
                d[i-1+j] = v1[j];
                e[i-1+j] = v2[j];
                tauq[i-1+j] = v3[j];
                taup[i-1+j] = v4[j];
            }
            for (j = 0; j < ldwrkx; j++) {
                for (k = 0; k < nb; k++) {
                    work[j + k*ldwrkx] = work1[j][k];
                }
            }
            for (j = 0; j < ldwrky; j++) {
                for (k = 0; k < nb; k++) {
                    work[j + k*ldwrky + ldwrkx*nb] = work2[j][k];
                }
            }
            
            // Update the trailing submatrix A(i+nb:m,i+nb:n) using an update of the form
            // A = A - V*Y' - X*U'
            
            row1 = Math.max(1, m-i-nb+1);
            array1 = new double[row1][nb];
            for (j = 0; j < row1; j++) {
                for (k = 0; k < nb; k++) {
                    array1[j][k] = A[i+nb-1+j][i-1+k];
                }
            }
            for (j = 0; j < ldwrky; j++) {
                for (k = 0; k < nb; k++) {
                    work2[j][k] = work[j + k*ldwrky + ldwrkx*nb+nb];
                }
            }
            array2 = new double[row1][n-i-nb+1];
            for (j = 0; j < row1; j++) {
                for (k = 0; k < n-i-nb+1; k++) {
                    array2[j][k] = A[i+nb-1+j][i+nb-1+k];
                }
            }
            dgemm('N', 'T', m-i-nb+1, n-i-nb+1, nb, -1.0, array1, row1, work2, ldwrky, 1.0,
                  array2, row1);
            for (j = 0; j < row1; j++) {
                for (k = 0; k < n-i-nb+1; k++) {
                    A[i+nb-1+j][i+nb-1+k] = array2[j][k];
                }
            }
            
            for (j = 0; j < ldwrkx; j++) {
                for (k = 0; k < nb; k++) {
                    work1[j][k] = work[j + k*ldwrkx + nb];
                }
            }
            row3 = Math.max(1, nb);
            array3 = new double[row3][n-i-nb+1];
            for (j = 0; j < row3; j++) {
                for (k = 0; k < n-i-nb+1; k++) {
                    array3[j][k] = A[i-1+j][i+nb-1+k];
                }
            }
            dgemm('N', 'N', m-i-nb+1, n-i-nb+1, nb, -1.0, work1, ldwrkx, array3, row3, 1.0,
                  array2, row1);
            for (j = 0; j < row1; j++) {
                for (k = 0; k < n-i-nb+1; k++) {
                    A[i+nb-1+j][i+nb-1+k] = array2[j][k];
                }
            }
            
            // Copy diagonal and off-diagonal elemnts of B back into A
            if (m >= n) {
                for (j = i; j <= i+nb-1; j++) {
                    A[j-1][j-1] = d[j-1];
                    A[j-1][j] = e[j-1];
                }
            } // if (m >= n)
            else {
                for (j = i; j <= i + nb - 1; j++) {
                    A[j-1][j-1] = d[j-1];
                    A[j][j-1] = e[j-1];
                }
            }
            
        } // for (i = 1; i <= minmn - nx; i += nb)
        // Use unblocked code to reduce the remainder of the matrix
        row1 = Math.max(1, m-i+1);
        array1 = new double[row1][n-i+1];
        for (j = 0; j < row1; j++) {
            for (k = 0; k < n-i+1; k++) {
                array1[j][k] = A[i-1+j][i-1+k];
            }
        }
        dimv = Math.min(m-i+1,n-i+1);
        v1 = new double[dimv];
        v2 = new double[dimv-1];
        v3 = new double[dimv];
        v4 = new double[dimv];
        dgebd2(m-i+1, n-i+1, array1, row1, v1, v2, v3, v4, work, iinfo);
        for (j = 0; j < row1; j++) {
            for (k = 0; k < n-i+1; k++) {
                A[i-1+j][i-1+k] = array1[j][k];
            }
        }
        for (j = 0; j < dimv; j++) {
            d[i-1+j] = v1[j];
            tauq[i-1+j] = v3[j];
            taup[i-1+j] = v4[j];
        }
        for (j = 0; j < dimv - 1; j++) {
            e[i-1+j] = v2[j];
        }
        work[0] = ws;
        return;
    } // dgebrd
    
    /* This is a port of the version 3.2 LAPACK auxiliary routine DLABRD.  Original DLABRD created by 
     * Univ. of Tennessee, Univ. Of California Berkeley, Univ. Of Colorado Denver, and NAG Ltd.,
     * November, 2006
     
       *  Purpose
       *  =======
       *
       *  DLABRD reduces the first NB rows and columns of a real general
       *  m by n matrix A to upper or lower bidiagonal form by an orthogonal
       *  transformation Q' * A * P, and returns the matrices X and Y which
       *  are needed to apply the transformation to the unreduced part of A.
       *
       *  If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
       *  bidiagonal form.
       *
       *  This is an auxiliary routine called by DGEBRD
       *
       *  Arguments
       *  =========
       *
       *  M       (input) INTEGER
       *          The number of rows in the matrix A.
       *
       *  N       (input) INTEGER
       *          The number of columns in the matrix A.
       *
       *  NB      (input) INTEGER
       *          The number of leading rows and columns of A to be reduced.
       *
       *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
       *          On entry, the m by n general matrix to be reduced.
       *          On exit, the first NB rows and columns of the matrix are
       *          overwritten; the rest of the array is unchanged.
       *          If m >= n, elements on and below the diagonal in the first NB
       *            columns, with the array TAUQ, represent the orthogonal
       *            matrix Q as a product of elementary reflectors; and
       *            elements above the diagonal in the first NB rows, with the
       *            array TAUP, represent the orthogonal matrix P as a product
       *            of elementary reflectors.
       *          If m < n, elements below the diagonal in the first NB
       *            columns, with the array TAUQ, represent the orthogonal
       *            matrix Q as a product of elementary reflectors, and
       *            elements on and above the diagonal in the first NB rows,
       *            with the array TAUP, represent the orthogonal matrix P as
       *            a product of elementary reflectors.
       *          See Further Details.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A.  LDA >= max(1,M).
       *
       *  D       (output) DOUBLE PRECISION array, dimension (NB)
       *          The diagonal elements of the first NB rows and columns of
       *          the reduced matrix.  D(i) = A(i,i).
       *
       *  E       (output) DOUBLE PRECISION array, dimension (NB)
       *          The off-diagonal elements of the first NB rows and columns of
       *          the reduced matrix.
       *
       *  TAUQ    (output) DOUBLE PRECISION array dimension (NB)
       *          The scalar factors of the elementary reflectors which
       *          represent the orthogonal matrix Q. See Further Details.
       *
       *  TAUP    (output) DOUBLE PRECISION array, dimension (NB)
       *          The scalar factors of the elementary reflectors which
       *          represent the orthogonal matrix P. See Further Details.
       *
       *  X       (output) DOUBLE PRECISION array, dimension (LDX,NB)
       *          The m-by-nb matrix X required to update the unreduced part
       *          of A.
       *
       *  LDX     (input) INTEGER
       *          The leading dimension of the array X. LDX >= M.
       *
       *  Y       (output) DOUBLE PRECISION array, dimension (LDY,NB)
       *          The n-by-nb matrix Y required to update the unreduced part
       *          of A.
       *
       *  LDY     (input) INTEGER
       *          The leading dimension of the array Y. LDY >= N.
       *
       *  Further Details
       *  ===============
       *
       *  The matrices Q and P are represented as products of elementary
       *  reflectors:
       *
       *     Q = H(1) H(2) . . . H(nb)  and  P = G(1) G(2) . . . G(nb)
       *
       *  Each H(i) and G(i) has the form:
       *
       *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
       *
       *  where tauq and taup are real scalars, and v and u are real vectors.
       *
       *  If m >= n, v(1:i-1) = 0, v(i) = 1, and v(i:m) is stored on exit in
       *  A(i:m,i); u(1:i) = 0, u(i+1) = 1, and u(i+1:n) is stored on exit in
       *  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
       *
       *  If m < n, v(1:i) = 0, v(i+1) = 1, and v(i+1:m) is stored on exit in
       *  A(i+2:m,i); u(1:i-1) = 0, u(i) = 1, and u(i:n) is stored on exit in
       *  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
       *
       *  The elements of the vectors v and u together form the m-by-nb matrix
       *  V and the nb-by-n matrix U' which are needed, with X and Y, to apply
       *  the transformation to the unreduced part of the matrix, using a block
       *  update of the form:  A := A - V*Y' - X*U'.
       *
       *  The contents of A on exit are illustrated by the following examples
       *  with nb = 2:
       *
       *  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
       *
       *    (  1   1   u1  u1  u1 )           (  1   u1  u1  u1  u1  u1 )
       *    (  v1  1   1   u2  u2 )           (  1   1   u2  u2  u2  u2 )
       *    (  v1  v2  a   a   a  )           (  v1  1   a   a   a   a  )
       *    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
       *    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
       *    (  v1  v2  a   a   a  )
       *
       *  where a denotes an element of the original matrix which is unchanged,
       *  vi denotes an element of the vector defining H(i), and ui an element
       *  of the vector defining G(i).
       */
    private void dlabrd(int m, int n, int nb, double A[][], int lda, double d[], double e[],
                        double tauq[], double taup[], double X[][], int ldx, double Y[][], int ldy) {
        int i;
        int row1;
        double array1[][];
        double vector1[];
        double vector2[];
        int j;
        int k;
        double alpha[] = new double[1];
        double tau[] = new double[1];
        
        // Quick return if possible
        if ((m <= 0) || (n <= 0)) {
            return;
        }
        
        if (m >= n) {
            // Reduce to upper bidiagonal form
            for (i = 1; i <= nb; i++) {
                // Update A(i:m,i)
                row1 = Math.max(1,m-i+1);
                array1 = new double[row1][i-1];
                for (j = 0; j < row1; j++) {
                    for (k = 0; k < i-1; k++) {
                        array1[j][k] = A[i-1 + j][k];
                    }
                }
                vector1 = new double[i-1];
                for (j = 0; j < i-1; j++) {
                    vector1[j] = Y[i-1][j];
                }
                vector2 = new double[m-i+1];
                for (j = 0; j < m-i+1; j++) {
                    vector2[j] = A[i-1+j][i-1];
                }
                dgemv('N', m-i+1, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                for (j = 0; j < m-i+1; j++) {
                    A[i-1+j][i-1] = vector2[j];
                }
                
                for (j = 0; j < row1; j++) {
                    for (k = 0; k < i-1; k++) {
                        array1[j][k] = X[i-1 + j][k];
                    }
                }
                for (j = 0; j < i-1; j++) {
                    vector1[j] = A[j][i-1];
                }
                dgemv('N', m-i+1, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                for (j = 0; j < m-i+1; j++) {
                    A[i-1+j][i-1] = vector2[j];
                }
                
                // Generate reflection Q(i) to annihilate A(i+1:m,i)
                alpha[0] = A[i-1][i-1];
                vector1 = new double[m-i];
                for (j = 0; j < m-i; j++) {
                    vector1[j] = A[Math.min(i,m-1) + j][i-1];
                }
                dlarfg(m-i+1, alpha, vector1, 1, tau);
                A[i-1][i-1] = alpha[0];
                for (j = 0; j < m-i; j++) {
                    A[Math.min(i,m-1) + j][i-1] = vector1[j];
                }
                tauq[i-1] = tau[0];
                
                d[i-1] = A[i-1][i-1];
                if (i < n) {
                    A[i-1][i-1] = 1.0;
                    
                    // Compute Y(i+1:n,i)
                    row1 = Math.max(1,m-i+1);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[i-1+j][i+k];
                        }
                    }
                    vector1 = new double[m-i+1];
                    for (j = 0; j < m-i+1; j++) {
                        vector1[j] = A[i-1+j][i-1];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = Y[i+j][i-1];
                    }
                    dgemv('T', m-i+1, n-i, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector2[j];
                    }
                    
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = A[i-1+j][k];
                        }
                    }
                    for (j = 0; j < m-i+1; j++) {
                        vector1[j] = A[i-1+j][i-1];
                    }
                    vector2 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector2[j] = Y[j][i-1];
                    }
                    dgemv('T', m-i+1, i-1, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i-1; j++) {
                        Y[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1,n-i);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = Y[i+j][k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = Y[j][i-1];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = Y[i+j][i-1];
                    }
                    dgemv('N', n-i, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, m-i+1);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = X[i-1+j][k];
                        }
                    }
                    vector1 = new double[m-i+1];
                    for (j = 0; j < m-i+1; j++) {
                        vector1[j] = A[i-1+j][i-1];
                    }
                    vector2 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector2[j] = Y[j][i-1];
                    }
                    dgemv('T', m-i+1, i-1, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i-1; j++) {
                        Y[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, i-1);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[j][i+k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = Y[j][i-1];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = Y[i+j][i-1];
                    }
                    dgemv('T', i-1, n-i, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector2[j];
                    }
                    
                    vector1 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector1[j] = Y[i+j][i-1];
                    }
                    dscal(n-i, tauq[i-1], vector1, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector1[j];
                    }
                    
                    // update A(i,i+1:n)
                    row1 = Math.max(1,n-i);
                    array1 = new double[row1][i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i; k++) {
                            array1[j][k] = Y[i+j][k];
                        }
                    }
                    vector1 = new double[i];
                    for (j = 0; j < i; j++) {
                        vector1[j] = A[i-1][j];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = A[i-1][i+j];
                    }
                    dgemv('N', n-i, i, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        A[i-1][i+j] = vector2[j];
                    }
                    
                    row1 = Math.max(1, i-1);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[j][i+k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = X[i-1][j];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = A[i-1][i+j];
                    }
                    dgemv('T', i-1, n-i, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        A[i-1][i+j] = vector2[j];
                    }
                    
                    // Generate reflection P(I) to annihilate A(i,i+2:n)
                    alpha[0] = A[i-1][i];
                    vector1 = new double[n-i-1];
                    for (j = 0; j < n-i-1; j++) {
                        vector1[j] = A[i-1][Math.min(i+1, n-1) + j];
                    }
                    dlarfg(n-i, alpha, vector1, 1, tau);
                    A[i-1][i] = alpha[0];
                    for (j = 0; j < n-i-1; j++) {
                        A[i-1][Math.min(i+1, n-1) + j] = vector1[j];
                    }
                    taup[i-1] = tau[0];
                    
                    e[i-1] = A[i-1][i];
                    A[i-1][i] = 1.0;
                    
                    // Compute X(i+1:m,i)
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[i+j][i+k];
                        }
                    }
                    vector1 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector1[j] = A[i-1][i+j];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = X[i+j][i-1];
                    }
                    dgemv('N', m-i, n-i, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector2[j];
                    }
                    row1 = Math.max(1, n-i);
                    array1 = new double[row1][i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i; k++) {
                            array1[j][k] = Y[i+j][k];
                        }
                    }
                    vector1 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector1[j] = A[i-1][i+j];
                    }
                    vector2 = new double[i];
                    for (j = 0; j < i; j++) {
                        vector2[j] = X[j][i-1];
                    }
                    dgemv('T', n-i, i, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i; j++) {
                        X[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i; k++) {
                            array1[j][k] = A[i+j][k];
                        }
                    }
                    vector1 = new double[i];
                    for (j = 0; j < i; j++) {
                        vector1[j] = X[j][i-1];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = X[i+j][i-1];
                    }
                    dgemv('N', m-i, i, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector2[j];
                    }
                    row1 = Math.max(1, i-1);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[j][i+k];
                        }
                    }
                    vector1 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector1[j] = A[i-1][i+j];
                    }
                    vector2 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector2[j] = X[j][i-1];
                    }
                    dgemv('N', i-1, n-i, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i-1; j++) {
                        X[j][i-1] = vector2[j];
                    }
                    
                    vector1 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector1[j] = X[i+j][i-1];
                    }
                    dscal(m-i, taup[i-1], vector1, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector1[j];
                    }
                } // if (i < n)
            } // for (i = 1; i <= nb; i++)
        } // if (m >= n)
        else { // m < n
            // Reduce to lower bidiagonal form
            for (i = 1; i <= nb; i++) {
                // Update A(i,i:n)
                
                row1 = Math.max(1, n-i+1);
                array1 = new double[row1][i-1];
                for (j = 0; j < row1; j++) {
                    for (k = 0; k < i-1; k++) {
                        array1[j][k] = Y[i-1+j][k];
                    }
                }
                vector1 = new double[i-1];
                for (j = 0; j < i-1; j++) {
                    vector1[j] = A[i-1][j];
                }
                vector2 = new double[n-i+1];
                for (j = 0; j < n-i+1; j++) {
                    vector2[j] = A[i-1][i-1+j];
                }
                dgemv('N', n-i+1, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                for (j = 0; j < n-i+1; j++) {
                    A[i-1][i-1+j] = vector2[j];
                }
                
                row1 = Math.max(1, i-1);
                array1 = new double[row1][n-i+1];
                for (j = 0; j < row1; j++) {
                    for (k = 0; k < n-i+1; k++) {
                        array1[j][k] = A[j][i-1+k];
                    }
                }
                vector1 = new double[i-1];
                for (j = 0; j < i-1; j++) {
                    vector1[j] = X[i-1][j];
                }
                vector2 = new double[n-i+1];
                for (j = 0; j < n-i+1; j++) {
                    vector2[j] = A[i-1][i-1+j];
                }
                dgemv('T', i-1, n-i+1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                for (j = 0; j < n-i+1; j++) {
                    A[i-1][i-1+j] = vector2[j];
                }
                
                // Generate reflection P(i) to annihilate A(i,i+1:n)
                alpha[0] = A[i-1][i-1];
                vector1 = new double[n-i];
                for (j = 0; j < n-i; j++) {
                    vector1[j] = A[i-1][Math.min(i, n-1) + j];
                }
                dlarfg(n-i+1, alpha, vector1, 1, tau);
                A[i-1][i-1] = alpha[0];
                for (j = 0; j < n-i; j++) {
                    A[i-1][Math.min(i, n-1) + j] = vector1[j];
                }
                taup[i-1] = tau[0];
                
                d[i-1] = A[i-1][i-1];
                if (i < m) {
                    A[i-1][i-1] = 1.0;
                    
                    // Compute X(i+1:m,i)
                    
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][n-i+1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i+1; k++) {
                            array1[j][k] = A[i+j][i-1+k];
                        }
                    }
                    vector1 = new double[n-i+1];
                    for (j = 0; j < n-i+1; j++) {
                        vector1[j] = A[i-1][i-1+j];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = X[i+j][i-1];
                    }
                    dgemv('N', m-i, n-i+1, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1,n-i+1);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = Y[i-1+j][k];
                        }
                    }
                    vector1 = new double[n-i+1];
                    for (j = 0; j < n-i+1; j++) {
                        vector1[j] = A[i-1][i-1+j];
                    }
                    vector2 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector2[j] = X[j][i-1];
                    }
                    dgemv('T', n-i+1, i-1, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i-1; j++) {
                        X[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1,m-i);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = A[i+j][k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = X[j][i-1];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = X[i+j][i-1];
                    }
                    dgemv('N', m-i, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, i-i);
                    array1 = new double[row1][n-i+1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i+1; k++) {
                            array1[j][k] = A[j][i-1+k];
                        }
                    }
                    vector1 = new double[n-i+1];
                    for (j = 0; j < n-i+1; j++) {
                        vector1[j] = A[i-1][i-1+j];
                    }
                    vector2 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector2[j] = X[j][i-1];
                    }
                    dgemv('N', i-1, n-i+1, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i-1; j++) {
                        X[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = X[i+j][k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = X[j][i-1];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = X[i+j][i-1];
                    }
                    dgemv('N', m-i, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector2[j];
                    }
                    
                    vector1 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector1[j] = X[i+j][i-1];
                    }
                    dscal(m-i, taup[i-1], vector1, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector1[j];
                    } 
                    
                    // Update A(i+1:m,i)
                    
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = A[i+j][k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = Y[i-1][j];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = A[i+j][i-1];
                    }
                    dgemv('N', m-i, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        A[i+j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i; k++) {
                            array1[j][k] = X[i+j][k];
                        }
                    }
                    vector1 = new double[i];
                    for (j = 0; j < i; j++) {
                        vector1[j] = A[j][i-1];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = A[i+j][i-1];
                    }
                    dgemv('N', m-i, i, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        A[i+j][i-1] = vector2[j];
                    }
                    
                    // Generate refletion Q(i) to annihilate A(i+2:m,i)
                    alpha[0] = A[i][i-1];
                    vector1 = new double[m-i-1];
                    for (j = 0; j < m-i-1; j++) {
                        vector1[j] = A[Math.min(i+1, m-1) + j][i-1];
                    }
                    dlarfg(m-i, alpha, vector1, 1, tau);
                    A[i][i-1] = alpha[0];
                    for (j = 0; j < m-i-1; j++) {
                        A[Math.min(i+1, m-1) + j][i-1] = vector1[j];
                    }
                    tauq[i-1] = tau[0];
                    
                    e[i-1] = A[i][i-1];
                    A[i][i-1] = 1.0;
                    
                    // Compute Y(i+1:n,i)
                    
                    row1  = Math.max(1,m-i);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[i+j][i+k];
                        }
                    }
                    vector1 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector1[j] = A[i+j][i-1];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = Y[i+j][i-1];
                    }
                    dgemv('T', m-i, n-i, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1,m-i);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = A[i+j][k];
                        }
                    }
                    vector1 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector1[j] = A[i+j][i-1];
                    }
                    vector2 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector2[j] = Y[j][i-1];
                    }
                    dgemv('T', m-i, i-1, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i-1; j++) {
                        Y[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, n-i);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = Y[i+j][k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = Y[j][i-1];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = Y[i+j][i-1];
                    }
                    dgemv('N', n-i, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i; k++) {
                            array1[j][k] = X[i+j][k];
                        }
                    }
                    vector1 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector1[j] = A[i+j][i-1];
                    }
                    vector2 = new double[i];
                    for (j = 0; j < i; j++) {
                        vector2[j] = Y[j][i-1];
                    }
                    dgemv('T', m-i, i, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i; j++) {
                        Y[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, i);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[j][i+k];
                        }
                    }
                    vector1 = new double[i];
                    for (j = 0; j < i; j++) {
                        vector1[j] = Y[j][i-1];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = Y[i+j][i-1];
                    }
                    dgemv('T', i, n-i, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector2[j];
                    }
                    
                    vector1 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector1[j] = Y[i+j][i-1];
                    }
                    dscal(n-i, tauq[i-1], vector1, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector1[j];
                    }
                } // if (i < m)
            } // for (i = 1; i <= nb; i++)
        } // else m < n
        return;
    } // dlabrd

    
    /**
     * This is a port of version 3.2 LAPACK routine DGEBD2.  Original DGEBD2 created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * *
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAUP( * ),
     $                   TAUQ( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEBD2 reduces a real general m by n matrix A to upper or lower
*  bidiagonal form B by an orthogonal transformation: Q' * A * P = B.
*
*  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows in the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns in the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the m by n general matrix to be reduced.
*          On exit,
*          if m >= n, the diagonal and the first superdiagonal are
*            overwritten with the upper bidiagonal matrix B; the
*            elements below the diagonal, with the array TAUQ, represent
*            the orthogonal matrix Q as a product of elementary
*            reflectors, and the elements above the first superdiagonal,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors;
*          if m < n, the diagonal and the first subdiagonal are
*            overwritten with the lower bidiagonal matrix B; the
*            elements below the first subdiagonal, with the array TAUQ,
*            represent the orthogonal matrix Q as a product of
*            elementary reflectors, and the elements above the diagonal,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The diagonal elements of the bidiagonal matrix B:
*          D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
*          The off-diagonal elements of the bidiagonal matrix B:
*          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
*          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
*
*  TAUQ    (output) DOUBLE PRECISION array dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix Q. See Further Details.
*
*  TAUP    (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix P. See Further Details.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (max(M,N))
*
*  INFO    (output) INTEGER
*          = 0: successful exit.
*          < 0: if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The matrices Q and P are represented as products of elementary
*  reflectors:
*
*  If m >= n,
*
*     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors;
*  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
*  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  If m < n,
*
*     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors;
*  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
*  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  The contents of A on exit are illustrated by the following examples:
*
*  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
*
*    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
*    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
*    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
*    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
*    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
*    (  v1  v2  v3  v4  v5 )
*
*  where d and e denote diagonal and off-diagonal elements of B, vi
*  denotes an element of the vector defining H(i), and ui an element of
*  the vector defining G(i).
*
*/
    private void dgebd2(int m, int n, double A[][], int lda, double d[], double e[], double tauq[],
                        double taup[], double work[], int info[]) {
        int i;
        double alpha[] = new double[1];
        double x[];
        int j;
        double tau[] = new double[1];
        int row1;
        double array1[][];
        int k;
        
        // Test the input parameters
        info[0] = 0;
        if (m < 0) {
            info[0] = -1;
        }
        else if (n < 0) {
            info[0] = -2;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -4;
        }
        
        if (info[0] < 0) {
            MipavUtil.displayError("dgebd2 had info[0] = " + info[0]);
            return;
        }
        
        if (m >= n) {
            // Reduce to upper bidiagonal form
            for (i = 1; i <= n; i++) {
                // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
                alpha[0] = A[i-1][i-1];
                x = new double[m-i];
                for (j = 0; j < m-i; j++) {
                    x[j] = A[Math.min(i,m-1) + j][i-1];
                }
                dlarfg(m-i+1, alpha, x, 1, tau);
                A[i-1][i-1] = alpha[0];
                for (j = 0; j < m-i; j++) {
                    A[Math.min(i, m-1) + j][i-1] = x[j];
                }
                tauq[i-1] = tau[0];
                
                d[i-1] = A[i-1][i-1];
                A[i-1][i-1] = 1.0;
                
                // Apply H(i) to A(i:m,i+1:n) from the left
                if (i < n) {
                    x = new double[m-i+1];
                    for (j = 0; j < m-i+1; j++) {
                        x[j] = A[i-1+j][i-1];
                    }
                    row1 = Math.max(1, m-i+1);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[i-1+j][i+k];
                        }
                    }
                    dlarf('L', m-i+1, n-i, x, 1, tauq[i-1], array1, row1, work);
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            A[i-1+j][i+k] = array1[j][k];
                        }
                    }
                } // if (i < n)
                A[i-1][i-1] = d[i-1];
                
                if (i < n) {
                    // Generate elementary reflector G(i) to annihilate A(i,i+2:n)
                    alpha[0] = A[i-1][i];
                    x = new double[n-i-1];
                    for (j = 0; j < n-i-1; j++) {
                        x[j] = A[i-1][Math.min(i+1,n-1) + j];
                    }
                    dlarfg(n-i, alpha, x, 1, tau);
                    A[i-1][i] = alpha[0];
                    for (j = 0; j < n-i-1; j++) {
                        A[i-1][Math.min(i+1, n-1) + j] = x[j];
                    }
                    taup[i-1] = tau[0];
                    
                    e[i-1] = A[i-1][i];
                    A[i-1][i] = 1.0;
                    
                    // Apply G(i) to A(i+1:m,i+1:n) from the right
                    x = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        x[j] = A[i-1][i+j];
                    }
                    row1 = Math.max(1,m-i);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[i+j][i+k];
                        }
                    }
                    dlarf('R', m-i, n-i, x, 1, taup[i-1], array1, row1, work);
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            A[i+j][i+k] = array1[j][k];
                        }
                    }
                    A[i-1][i] = e[i-1];
                } // if (i < n)
                else {
                    taup[i-1] = 0.0;
                }
            } // for (i = 1; i <= n; i++)
        } // if (m >= n)
        else { // m < n
            // Reduce to lower bidiagonal form
            for (i = 1; i <= m; i++) {
                // Generate elementary reflector G(i) to annihilate A(i,i+1:n)
                alpha[0] = A[i-1][i-1];
                x = new double[n-i];
                for (j = 0; j < n-i; j++) {
                    x[j] = A[i-1][Math.min(i, n-1) + j];
                }
                dlarfg(n-i+1, alpha, x, 1, tau);
                A[i-1][i-1] = alpha[0];
                for (j = 0; j < n-i; j++) {
                    A[i-1][Math.min(i, n-1) + j] = x[j];
                }
                taup[i-1] = tau[0];
                d[i-1] = A[i-1][i-1];
                A[i-1][i-1] = 1.0;
                
                // Apply G(i) to A(i+1:m,i:n) from the right
                if (i < m) {
                    x = new double[n-i+1];
                    for (j = 0; j < n-i+1; j++) {
                        x[j] = A[i-1][i-1+j];
                    }
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][n-i+1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i+1; k++) {
                            array1[j][k] = A[i+j][i-1+k];
                        }
                    }
                    dlarf('R', m-i, n-i+1, x, 1, taup[i-1], array1, row1, work);
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i+1; k++) {
                            A[i+j][i-1+k] = array1[j][k];
                        }
                    }
                } // if (i < m)
                A[i-1][i-1] = d[i-1];
                if (i < m) {
                    // Generate elementary reflector H(i) to annihilate A(i+2:m,i)
                    alpha[0] = A[i][i-1];
                    x = new double[m-i-1];
                    for (j = 0; j < m-i-1; j++) {
                        x[j] = A[Math.min(i+1,m-1) + j][i-1];
                    }
                    dlarfg(m-i, alpha, x, 1, tau);
                    A[i][i-1] = alpha[0];
                    for (j = 0; j < m-i-1; j++) {
                        A[Math.min(i+1,m-1) + j][i-1] = x[j];
                    }
                    tauq[i-1] = tau[0];
                    
                    e[i-1] = A[i][i-1];
                    A[i][i-1] = 1.0;
                    
                    // Apply H(i) to A(i+1:m,i+1:n) from the left
                    x = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        x[j] = A[i+j][i-1];
                    }
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[i+j][i+k];
                        }
                    }
                    dlarf('L', m-i, n-i, x, 1, tauq[i-1], array1, row1, work);
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            A[i+j][i+k] = array1[j][k];
                        }
                    }
                    A[i][i-1] = e[i-1];
                } // if (i < m)
                else {
                    tauq[i-1] = 0.0;
                }
            } // for (i = 1; i <= m; i++)
        } // else m < n
    } // dgebd2
    	            
    /**
     * This is a port of version 3.2 auxiliary routine DLASET. Original DLASET created by Univ. of Tennessee, Univ. of
     * California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlaset initializes an m-by-n matrix A to beta on the diagonal and alpha on the offdiagonals.
     *
     * @param  uplo   input char Specifies the part of the matrix to be set. 
     *                = 'U': Upper triangular part is set; the strictly lower triangular part of A is not changed. 
     *                = 'L': Lower triangular part is set; the strictly upper triangular part of A is not changed.
     *                Otherwise: All of the matrix A is set.
     * @param  m      input int The number of rows of the matrix A. m >= 0.
     * @param  n      input int The number of columns of the matrix A. n >= 0.
     * @param  alpha  input double The constant to which the offdiagonal elements are to be set.
     * @param  beta   input double The constant to which the diagonal elements are to be set.
     * @param  A      input/output double[][] of dimension lda by n. On exit, the leading m-by-n submatrix of A is set
     *                as follows: 
     *                If uplo = 'U', A(i,j) = alpha, 0 <= i <= j-1, 0 <= j <= n-1,
     *                If uplo = 'L', A(i,j) = alpha, j+1 <= i <= m-1, 0 <= j <= n-1, 
     *                Otherwise, A(i,j) = alpha, 0 <= i <= m-1, 0 <= j <= n-1, i!= j
     *                and, for all uplo, A(i,i) = beta, 0 <= i <= min(m-1,n-1).
     * @param  lda    input int The leading dimension of the array A. lda >= max(1,m).
     */
    private void dlaset(char uplo, int m, int n, double alpha, double beta, double[][] A, int lda) {
        int i;
        int j;

        if ((uplo == 'U') || (uplo == 'u')) {

            // Set the srictly upper triangular or trapezoidal part of the array to
            // alpha.
            for (j = 1; j < n; j++) {

                for (i = 0; i <= Math.min(j - 1, m - 1); i++) {
                    A[i][j] = alpha;
                }
            }
        } // if ((uplo == 'U') || (uplo == 'u'))
        else if ((uplo == 'L') || (uplo == 'l')) {

            // Set the strictly lower triangular or trapezoidal part of the array to
            // alpha.
            for (j = 0; j <= Math.min(m - 1, n - 1); j++) {

                for (i = j + 1; i <= (m - 1); i++) {
                    A[i][j] = alpha;
                }
            }
        } // else if ((uplo == 'L') || (uplo == 'l'))
        else {

            // Set the leading m-by-n submatrix to alpha
            for (j = 0; j < n; j++) {

                for (i = 0; i < m; i++) {
                    A[i][j] = alpha;
                }
            }
        } // else

        // Set the first min(m,n) diagonal elements to beta
        for (i = 0; i <= Math.min(m - 1, n - 1); i++) {
            A[i][i] = beta;
        }

        return;
    } // dlaset
    	            
    /**
     * This is a port of version 3.2 LAPACK routine DGEQRF Original DGEQRF created by Univ. of Tennessee, Univ. of
     * California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dgeqrf computes a QR factorization of a real m by n matrix A:
     * A = Q * R.
     *
     * @param  m      input int The number of rows of the matrix A. m >= 0.
     * @param  n      input int The number of columns of the matrix A. n >= 0.
     * @param  A      input/output double[][] of dimension (lda,n) On entry, the m by n matrix A. On exit, the elements
     *                on and above the diagonal of the array contain the min(m,n)-by-n upper trapezoidal matrix R ( R is
     *                upper triangular if m >= n); the elements below the diagonal, with the array tau, represent the
     *                orthogonal matrix Q as a product of min(m,n) elementary reflectors. The matrix Q is represented as
     *                a product of elementary reflectors Q = H(1) H(2) . . . H(k), where k = min(m,n) Each H(i) has the
     *                form H(i) = I - tau * v * v' where tau is a real scalar, and v is a real vector with v(0:i-2) = 0
     *                and v(i-1) = 1; v(i:m-1) is stored on exit in A(i:m-1,i-1), and tau in tau[i-1].
     * @param  lda    input int The leading dimension of the array A. lda >= max(1,m).
     * @param  tau    output double[] of dimension min(m,n). The scalar factors of the elementary reflectors.
     * @param  work   (workspace/output) double[] of dimension (max(1,lwork)) On exit, if info[0] = 0, work[0] returns
     *                the optimal lwork.
     * @param  lwork  input int The dimension of the array work. lwork >= max(1,n). For optimum performance, lwork >=
     *                n*nb, where nb is the optimal blocksize. If lwork = -1, then a workspace query is assumed; the
     *                routine only calculates the optimal size of the work array, returns this value as the first entry
     *                of the work array, and no error message related to lwork is output.
     * @param  info   output int[] = 0: successful exit, < 0: If info[0] = -i, the i-th argument had an illegal value
     */
    private void dgeqrf(int m, int n, double[][] A, int lda, double[] tau, double[] work, int lwork, int[] info) {
        boolean lquery;
        int i;
        int ib;
        int[] iinfo = new int[1];
        int iws;
        int k;
        int ldwork = 1;
        int lwkopt;
        int nb;
        int nbmin;
        int nx;
        String name;
        String opts;
        double[][] array1;
        double[][] array2;
        int row1;
        int p;
        int q;
        double[] x;
        double[][] work2d;
        double[][] w2d;

        // Test the input arguments
        info[0] = 0;
        name = new String("DGEQRF");
        opts = new String(" ");
        nb = ilaenv(1, name, opts, m, n, -1, -1);
        lwkopt = n * nb;
        work[0] = lwkopt;
        lquery = (lwork == -1);

        if (m < 0) {
            info[0] = -1;
        } else if (n < 0) {
            info[0] = -2;
        } else if (lda < Math.max(1, m)) {
            info[0] = -4;
        } else if ((lwork < Math.max(1, n)) && (!lquery)) {
            info[0] = -7;
        }

        if (info[0] != 0) {
            Preferences.debug("Error dgeqrf had info[0] = " + info[0] + "\n");
            MipavUtil.displayError("Error dgeqrf had info[0] = " + info[0]);

            return;
        } else if (lquery) {
            return;
        }

        // Quick return if possible
        k = Math.min(m, n);

        if (k == 0) {
            work[0] = 1;

            return;
        } // if (k == 0)

        nbmin = 2;
        nx = 0;
        iws = n;

        if ((nb > 1) && (nb < k)) {

            // Determine when to cross over from blocked to unblocked code.
            nx = Math.max(0, ilaenv(3, name, opts, m, n, -1, -1));

            if (nx < k) {

                // Determine if workspace is large enough for blocked code.
                ldwork = n;
                iws = ldwork * nb;

                if (lwork < iws) {

                    // Not enough space to use optimal nb: reduce nb and
                    // determine the minimum value of nb.
                    nb = lwork / ldwork;
                    nbmin = Math.max(2, ilaenv(2, name, opts, m, n, -1, -1));
                } // if (lwork < iws)
            } // if  (nx < k)
        } // if ((nb > 1) && (nb < k))

        if ((nb >= nbmin) && (nb < k) && (nx < k)) {

            // Use blocked code initially.
            for (i = 1; i <= (k - nx); i += nb) {
                ib = Math.min(k - i + 1, nb);

                // Compute the QR factorization of the current block
                // A(i-1:m-1, i-1:i+ib-2)
                row1 = Math.max(1, m - i + 1);
                array1 = new double[row1][ib];

                for (p = 0; p < row1; p++) {

                    for (q = 0; q < ib; q++) {
                        array1[p][q] = A[i - 1 + p][i - 1 + q];
                    }
                }

                x = new double[Math.min(m - i + 1, ib)];
                dgeqr2(m - i + 1, ib, array1, row1, x, work, iinfo);

                for (p = 0; p < row1; p++) {

                    for (q = 0; q < ib; q++) {
                        A[i - 1 + p][i - 1 + q] = array1[p][q];
                    }
                }

                for (p = 0; p < Math.min(m - i + 1, ib); p++) {
                    tau[i - 1 + p] = x[p];
                }

                if ((i + ib) <= n) {

                    // Form the triangular factor of the block reflector
                    // H = H(i) H(i+1) . . . H(i+ib-1)
                    x = new double[ib];

                    for (p = 0; p < ib; p++) {
                        x[p] = tau[i - 1 + p];
                    }

                    work2d = new double[ldwork][ib];
                    dlarft('F', 'C', m - i + 1, ib, array1, row1, x, work2d, ldwork);

                    for (p = 0; p < row1; p++) {

                        for (q = 0; q < ib; q++) {
                            A[i - 1 + p][i - 1 + q] = array1[p][q];
                        }
                    }

                    // Apply H' to A(i-1:m-1,i+ib-1:n-1) from the left
                    array2 = new double[m - i + 1][n - i - ib + 1];

                    for (p = 0; p < (m - i + 1); p++) {

                        for (q = 0; q < (n - i - ib + 1); q++) {
                            array2[p][q] = A[i - 1 + p][i + ib - 1 + q];
                        }
                    }

                    w2d = new double[ldwork][ib];
                    dlarfb('L', 'T', 'F', 'C', m - i + 1, n - i - ib + 1, ib, array1, row1, work2d, ldwork, array2,
                           m - i + 1, w2d, ldwork);

                    for (p = 0; p < (m - i + 1); p++) {

                        for (q = 0; q < (n - i - ib + 1); q++) {
                            A[i - 1 + p][i + ib - 1 + q] = array2[p][q];
                        }
                    }
                } // if (i+ib <= n)
            } // for (i = 1; i <= k - nx; i += nb)
        } // if ((nb >= nbmin) && (nb < k) && (nx < k))
        else {
            i = 1;
        }

        // Use unblocked code to factor the last or only block
        if (i <= k) {
            x = new double[Math.min(m - i + 1, n - i + 1)];
            row1 = Math.max(1, m - i + 1);
            array1 = new double[row1][n - i + 1];

            for (p = 0; p < row1; p++) {

                for (q = 0; q < (n - i + 1); q++) {
                    array1[p][q] = A[i - 1 + p][i - 1 + q];
                }
            }

            dgeqr2(m - i + 1, n - i + 1, array1, row1, x, work, iinfo);

            for (p = 0; p < Math.min(m - i + 1, n - i + 1); p++) {
                tau[i - 1 + p] = x[p];
            }

            for (p = 0; p < row1; p++) {

                for (q = 0; q < (n - i + 1); q++) {
                    A[i - 1 + p][i - 1 + q] = array1[p][q];
                }
            }
        }

        work[0] = iws;

        return;
    } // dgeqrf
    
    /**
     * This is a port of the version 3.2 LAPACK routine DGEQR2 Original DGEQR2 created by Univ. of Tennessee, Univ. of
     * California Berkeley, Univ. of Colorado Denver, and NAG Ltd. November, 2006 
     * dgeqr2 computes a QR factorization of a real m by n matrix A: A = Q * R
     *
     * @param  m     input int The number of rows of the matrix A. m >= 0.
     * @param  n     input int The number of columns of the matrix A. n >= 0.
     * @param  A     (input/output) double[][] of dimension (lda,n) On entry, the m by n matrix A. On exit, the elements
     *               on and above the diagonal of the array contain the min(m,n) by n upper trapezoidal matrix R (R is
     *               upper triangular if m >= n). The elements below the diagonal, with the array tau, represent the
     *               orthogonal matrix Q as a product of elementary reflectors. The matrix Q is represented as a product
     *               of elementary reflectors Q = H(1) H(2) . . . H(k), where k = min(m,n). Each H(i) has the form H(i)
     *               = I - tau * v * v' where tau is a real scalar, and v is a real vector with v(0:i-2) = 0 and v(i-1)
     *               = 1; v(i:m-1) is stored on exit in A(i:m-1, i-1), and tau in tau[i-1].
     * @param  lda   input int The leading dimension of the array A. lda >= max(1,m).
     * @param  tau   output double[] of dimension min(m,n) The scalar factors of the elementary reflectors.
     * @param  work  (workspace) double[] of dimension (n)
     * @param  info  output int[] = 0: successful exit, < 0: If info[0] = -i, the i-th argument had an illegal value.
     */
    private void dgeqr2(int m, int n, double[][] A, int lda, double[] tau, double[] work, int[] info) {
        int i;
        int k;
        double aii;
        double[] alpha = new double[1];
        double[] t = new double[1];
        double[] x;
        double[][] array1;
        int row1;
        int j;
        int p;

        // Test the input arguments
        info[0] = 0;

        if (m < 0) {
            info[0] = -1;
        } else if (n < 0) {
            info[0] = -2;
        } else if (lda < Math.max(1, m)) {
            info[0] = -4;
        }

        if (info[0] != 0) {
            Preferences.debug("Error dgeqr2 had info[0] = " + info[0] + "\n");
            MipavUtil.displayError("Error dgeqr2 had info[0] = " + info[0]);

            return;
        }

        k = Math.min(m, n);

        for (i = 1; i <= k; i++) {

            // Generate elementary reflector H(i) to annihilate A(i:m-1,i-1)
            alpha[0] = A[i - 1][i - 1];
            x = new double[m - i];

            for (j = 0; j < (m - i); j++) {
                x[j] = A[Math.min(i, m - 1) + j][i - 1];
            }

            dlarfp(m - i + 1, alpha, x, 1, t);
            A[i - 1][i - 1] = alpha[0];

            for (j = 0; j < (m - i); j++) {
                A[Math.min(i, m - 1) + j][i - 1] = x[j];
            }

            tau[i - 1] = t[0];

            if (i < n) {

                // Apply H(i) to A(i-1:m-1,i:n-1) from the left
                aii = A[i - 1][i - 1];
                A[i - 1][i - 1] = 1.0;
                x = new double[m - i + 1];

                for (j = 0; j < (m - i + 1); j++) {
                    x[j] = A[i - 1 + j][i - 1];
                }

                row1 = Math.max(1, m - i + 1);
                array1 = new double[row1][n - i];

                for (j = 0; j < row1; j++) {

                    for (p = 0; p < (n - i); p++) {
                        array1[j][p] = A[i - 1 + j][i + p];
                    }
                }

                dlarf('L', m - i + 1, n - i, x, 1, t[0], array1, row1, work);

                for (j = 0; j < row1; j++) {

                    for (p = 0; p < (n - i); p++) {
                        A[i - 1 + j][i + p] = array1[j][p];
                    }
                }

                A[i - 1][i - 1] = aii;
            } // if (i < n)
        } // for (i = 1; i <= k; i++)

        return;
    } // dgeqr2
    
    /**
     * This is a port of the version 3.2 LAPACK auxiliary routine DLARF Original DLARF created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006 
     * dlarf applies a real elementary reflector H to a real m by n matrix C, from either the left or right. 
     * H is represented in the form H = I - tau * v * v' where tau is a real scalar and v is a real vector.
     * If tau = 0, then H is taken to be the unit matrix.
     *
     * @param  side  input char = 'L': form H * C, = 'R': form C * H
     * @param  m     input int The number of rows of the matrix C
     * @param  n     input int The number of columns of the matrix C.
     * @param  v     input double[] If side = 'L' dimension = (1 + (m-1)*abs(incv)) If side = 'R' dimension = (1 +
     *               (n-1)*abs(incv)) The vector v in the representation of H. v is not used if tau = 0.
     * @param  incv  input int The increment between elements of v. incv <> 0.
     * @param  tau   input double The value of tau in the representation of H.
     * @param  C     input/output double[][] of dimension ldc by n. On entry, the m by n matrix C. On exit, C is
     *               overwritten by the matrix H * C if side = 'L', or C * H if side = 'R'.
     * @param  ldc   input int The leading dimension of array C. ldc >= max(1,m).
     * @param  work  workspace double[] If side = 'L', dimension = n. If side = 'R', dimension = m.
     */
    private void dlarf(char side, int m, int n, double[] v, int incv, double tau, double[][] C, int ldc,
                       double[] work) {
        boolean applyLeft;
        int i;
        int lastV;
        int lastC;

        applyLeft = ((side == 'L') || (side == 'l'));
        lastV = 0;
        lastC = 0;
        if (tau != 0.0) {
            // Set up variables for scanning V.  lastV begins pointing to the end of v.
            if (applyLeft) {
                lastV = m;
            }
            else {
                lastV = n;
            }
            if (incv > 0) {
                i = 1 + (lastV - 1) * incv;
            }
            else {
                i = 1;
            }
            // Look for the last non-zero row in v.
            while ((lastV > 0) && (v[i-1] == 0.0)) {
                lastV = lastV - 1;
                i = i - incv;
            }
            if (applyLeft) {
                // Scan for the last non-zero column in C(0:lastv-1,:)
                lastC = iladlc(lastV, n, C, ldc);
            }
            else {
                // Scan for the last non-zero row in C(:,0:lastV-1)
                lastC = iladlr(m, lastV, C, ldc);
            }
        } // if (tau != 0.0)
        // Note that lastC == 0 renders BLAS operations null; no special case is needed at this level.
        
        if (applyLeft) {

            // Form H * C
            if (lastV > 0) {

                // w(0:lastC-1,0) = C(0:lastV-1,0:lastC-1)' * v(0:lastV-1,0)
                dgemv('T', lastV, lastC, 1.0, C, ldc, v, incv, 0.0, work, 1);

                // C(0:lastV-1,0:lastC-1) = C(...) - v(0:lastV-1,0) * w(0:lastC-1,0)'
                dger(lastV, lastC, -tau, v, incv, work, 1, C, ldc);
            } // if (lastV > 0)
        } // if (applyLeft)
        else { // !applyLeft)

            // Form C * H
            if (lastV > 0) {

                // w():lastC-1,0) = C(0:lastC-1,0:lastV-1) * v(0:lastV-1,0)
                dgemv('N', lastC, lastV, 1.0, C, ldc, v, incv, 0.0, work, 1);

                // C(0:lastC-1,0:lastV-1) = C(...) - w(0:lastC-1,0) * v(0:lastV-1,0)'
                dger(lastC, lastV, -tau, work, 1, v, incv, C, ldc);
            } // if (lastV > 0)
        } // else !applyLeft

        return;
    } // dlarf
    
    /**
     * This is a port of version 3.2 LAPACK auxiliary routine DLARFB Original DLARFB created by Univ. of Tennessee, Univ.
     * of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlarfb applies a real block reflector H or its transpose H' to a real m by n matrix C, from either the left or
     * the right.
     *
     * @param  side    input char 
     *                 = 'L': apply H or H' from the left 
     *                 = 'R': apply H or H' from the right
     * @param  trans   input char 
     *                 = 'N': Apply H (No transpose) 
     *                 = 'T': Apply H' (Transpose)
     * @param  direct  input char Indicates how H is formed from a product of elementary reflectors 
     *                 = 'F': H = H[0] H[1] ... H[k-1] (Forward) 
     *                 = 'B': H = H[k-1] ... H[1] H[0] (Backward)
     * @param  storev  input char Indicates how the vectors which define the elementary reflectors are stored: 
     *                 = 'C': Columnwise 
     *                 = 'R': Rowwise
     * @param  m       input int The number of rows of the matrix C.
     * @param  n       input int The number of columns of the matrix C.
     * @param  k       input int The order of the matrix T (= the number of elementary reflectors whose product defines
     *                 the block reflector).
     * @param  V       input double[][] If storev = 'C', dimensions are ldv by k. If storev = 'R' and side = 'L',
     *                 dimensions are ldv by m. If storev = 'R' and side = 'R', dimensions are ldv by n.
     * @param  ldv     input int The leading dimension of the array V. 
     *                 If storev = 'C' and side = 'L', ldv >= max(1,m).
     *                 If storev = 'C' and side = 'R', ldv >= max(1,n). 
     *                 If storev = 'R', ldv >= k.
     * @param  T       input double[][] of dimensions ldt by k The triangular k by k matrix T in the representation of
     *                 the block reflector.
     * @param  ldt     input int The leading dimension of the array T. ldt >= k.
     * @param  C       input/output double[][] of dimensions ldc by n. On entry, the m by n matrix C. On exit, C is
     *                 overwritten by H*C or H'*C or C*H or C*H'.
     * @param  ldc     input int The leading dimension of the array C. ldc >= max(1,m).
     * @param  work    workspace double[][] of dimensions ldwork by k
     * @param  ldwork  input int The leading dimension of the array work. 
     *                 If side = 'L', ldwork >= max(1,n). 
     *                 If side = 'R', ldwork >= max(1,m).
     */
    private void dlarfb(char side, char trans, char direct, char storev, int m, int n, int k, double[][] V, int ldv,
                        double[][] T, int ldt, double[][] C, int ldc, double[][] work, int ldwork) {
        char transt;
        int i;
        int j;
        int p;
        int q;
        int lastV;
        int lastC;
        int row1;
        int row2;
        double[][] array1;
        double[][] array2;

        // Quick return if possible
        if ((m <= 0) || (n <= 0)) {
            return;
        }

        if ((trans == 'N') || (trans == 'n')) {
            transt = 'T';
        } else {
            transt = 'N';
        }

        if ((storev == 'C') || (storev == 'c')) {

            if ((direct == 'F') || (direct == 'f')) {

                // Let V = (V1)  (first k rows)
                //         (V2)
                // where V1 is unit lower triangular
                if ((side == 'L') || (side == 'l')) {

                    // Form H * C or H' * C where C = ( C1 )
                    //                                ( C2 )
                    lastV = Math.max(k, iladlr(m, k, V, ldv));
                    lastC = iladlc(lastV, n, C, ldc);
                    // W = C' * V = (C1'*V1 + C2'*V2) (stored in work)
                    // W = C1'
                    for (j = 0; j < k; j++) {

                        for (p = 0; p < lastC; p++) {
                            work[p][j] = C[j][p];
                        }
                    } // for (j = 0; j < k; j++)

                    // W = W * V1
                    dtrmm('R', 'L', 'N', 'U', lastC, k, 1.0, V, ldv, work, ldwork);

                    if (lastV > k) {

                        // W = W + C2'* V2
                        row1 = Math.max(1, lastV - k);
                        array1 = new double[row1][lastC];

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < lastC; q++) {
                                array1[p][q] = C[p + k][q];
                            }
                        }

                        array2 = new double[row1][k];

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < k; q++) {
                                array2[p][q] = V[p + k][q];
                            }
                        }

                        dgemm('T', 'N', lastC, k, lastV - k, 1.0, array1, row1, array2, 
                                              row1, 1.0, work, ldwork);
                    } // if (lastV > k)

                    // W = W * T' or W * T
                    dtrmm('R', 'U', transt, 'N', lastC, k, 1.0, T, ldt, work, ldwork);

                    // C = C - V * W'
                    if (lastV > k) {

                        // C2 = C2 - V2 * W'
                        row1 = Math.max(1, lastV - k);
                        array1 = new double[row1][k];

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < k; q++) {
                                array1[p][q] = V[p + k][q];
                            }
                        }

                        array2 = new double[row1][lastC];

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < lastC; q++) {
                                array2[p][q] = C[p + k][q];
                            }
                        }

                        dgemm('N', 'T', lastV - k, lastC, k, -1.0, array1, row1, work, ldwork, 1.0, array2, row1);

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < lastC; q++) {
                                C[p + k][q] = array2[p][q];
                            }
                        }
                    } // if (lastV > k)

                    // W = W * V1'
                    dtrmm('R', 'L', 'T', 'U', lastC, k, 1.0, V, ldv, work, ldwork);

                    // C1 = C1 - W'
                    for (j = 0; j < k; j++) {

                        for (i = 0; i < lastC; i++) {
                            C[j][i] = C[j][i] - work[i][j];
                        }
                    }
                } // if ((side == 'L') || (side == 'l'))
                else if ((side == 'R') || (side == 'r')) {

                    // Form C * H or C * H' where C = ( C1 C2 )
                    lastV = Math.max(k, iladlr(n, k, V, ldv));
                    lastC = iladlr(m, lastV, C, ldc);
                    // W = C * V = (C1*V1 + C2*V2) (stored in work)
                    // W = C1
                    for (j = 0; j < k; j++) {

                        for (p = 0; p < lastC; p++) {
                            work[p][j] = C[p][j];
                        }
                    } // for (j = 0; j < k; j++)

                    // W = W * V1
                    dtrmm('R', 'L', 'N', 'U', lastC, k, 1.0, V, ldv, work, ldwork);

                    if (lastV > k) {

                        // W = W + C2 * V2
                        row1 = Math.max(1, lastC);
                        array1 = new double[row1][lastV - k];

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < (lastV - k); q++) {
                                array1[p][q] = C[p][q + k];
                            }
                        }

                        row2 = Math.max(1, lastV - k);
                        array2 = new double[row2][k];

                        for (p = 0; p < row2; p++) {

                            for (q = 0; q < k; q++) {
                                array2[p][q] = V[p + k][q];
                            }
                        }

                        dgemm('N', 'N', lastC, k, lastV - k, 1.0, array1, row1, array2, row2, 1.0, work, ldwork);
                    } // if (lastV > k)

                    // W = W * T or W * T'
                    dtrmm('R', 'U', trans, 'N', lastC, k, 1.0, T, ldt, work, ldwork);

                    // C = C - W * V'
                    if (lastV > k) {

                        // C2 = C2 - W * V2'
                        row1 = Math.max(1, lastV - k);
                        array1 = new double[row1][k];

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < k; q++) {
                                array1[p][q] = V[p + k][q];
                            }
                        }

                        row2 = Math.max(1, lastC);
                        array2 = new double[row2][lastV - k];

                        for (p = 0; p < row2; p++) {

                            for (q = 0; q < (lastV - k); q++) {
                                array2[p][q] = C[p][q + k];
                            }
                        }

                        dgemm('N', 'T', lastC, lastV - k, k, -1.0, work, ldwork, array1, row1, 1.0, array2, row2);

                        for (p = 0; p < row2; p++) {

                            for (q = 0; q < (lastV - k); q++) {
                                C[p][q + k] = array2[p][q];
                            }
                        }
                    } // if (lastV > k)

                    // W = W * V1'
                    dtrmm('R', 'L', 'T', 'U', lastC, k, 1.0, V, ldv, work, ldwork);

                    // C1 = C1 - W
                    for (j = 0; j < k; j++) {

                        for (i = 0; i < lastC; i++) {
                            C[i][j] = C[i][j] - work[i][j];
                        }
                    }
                } // else if ((side == 'R') || (side == 'r'))
            } // if ((direct == 'F') || (direct == 'f'))
            else { // ((direct == 'B') || (direct == 'b'))

                // Let V =  ( V1 )
                //          ( V2 )  (last k rows)
                // where V2 is unit upper triangular.
                if ((side == 'L') || (side == 'l')) {

                    // Form H * C or H' * C where C = ( C1 )
                    //                                ( C2 )
                    lastV = Math.max(k, iladlr(m, k, V, ldv));
                    lastC = iladlc(lastV, n, C, ldc);
                    // W = C' * V = (C1'*V1 + C2'*V2) (stored in work)
                    // W = C2'
                    for (j = 0; j < k; j++) {

                        for (p = 0; p < lastC; p++) {
                            work[p][j] = C[lastV - k + j][p];
                        }
                    } // for (j = 0; j < k; j++)

                    // W = W * V2
                    row1 = Math.max(1, k);
                    array1 = new double[row1][k];

                    for (p = 0; p < row1; p++) {

                        for (q = 0; q < k; q++) {
                            array1[p][q] = V[p + lastV - k][q];
                        }
                    }

                    dtrmm('R', 'U', 'N', 'U', lastC, k, 1.0, array1, row1, work, ldwork);

                    if (lastV > k) {

                        // W = W + C1' * V1
                        dgemm('T', 'N', lastC, k, lastV - k, 1.0, C, ldc, V, ldv, 1.0, work, ldwork);
                    } // if (lastV > k)

                    // W = W * T' or W * T
                    dtrmm('R', 'L', transt, 'N', lastC, k, 1.0, T, ldt, work, ldwork);

                    // C = C - V * W'
                    if (lastV > k) {

                        // C1 = C1 - V1 * W'
                        dgemm('N', 'T', lastV - k, lastC, k, -1.0, V, ldv, work, ldwork, 1.0, C, ldc);
                    } // if (lastV > k)

                    // W = W * V2'
                    row1 = Math.max(1, k);
                    array1 = new double[row1][k];

                    for (p = 0; p < row1; p++) {

                        for (q = 0; q < k; q++) {
                            array1[p][q] = V[p + lastV - k][q];
                        }
                    }

                    dtrmm('R', 'U', 'T', 'U', lastC, k, 1.0, array1, row1, work, ldwork);

                    // C2 = C2 - W'
                    for (j = 0; j < k; j++) {

                        for (i = 0; i < lastC; i++) {
                            C[lastV - k + j][i] = C[lastV - k + j][i] - work[i][j];
                        }
                    }
                } // if ((side == 'L') || (side == 'l'))
                else if ((side == 'R') || (side == 'r')) {

                    // Form C * H or C * H' where C = ( C1 C2 )
                    lastV = Math.max(k, iladlr(n, k, V, ldv));
                    lastC = iladlr(m, lastV, C, ldc);
                    // W = C * V = (C1*V1 + C2*V2) (stored in work)
                    // W = C2
                    for (j = 0; j < k; j++) {

                        for (p = 0; p < lastC; p++) {
                            work[p][j] = C[p][n - k + j];
                        }
                    }

                    // W = W * V2
                    row1 = Math.max(1, k);
                    array1 = new double[row1][k];

                    for (p = 0; p < row1; p++) {

                        for (q = 0; q < k; q++) {
                            array1[p][q] = V[p + lastV - k][q];
                        }
                    }

                    dtrmm('R', 'U', 'N', 'U', lastC, k, 1.0, array1, row1, work, ldwork);

                    if (lastV > k) {

                        // W = W + C1 * V1
                        dgemm('N', 'N', lastC, k, lastV - k, 1.0, C, ldc, V, ldv, 1.0, work, ldwork);
                    } // if (lastV > k)

                    // W = W * T or W * T'
                    dtrmm('R', 'L', trans, 'N', lastC, k, 1.0, T, ldt, work, ldwork);

                    // C = C - W * V'
                    if (lastV > k) {

                        // C1 = C1 - W * V1'
                        dgemm('N', 'T', lastC, lastV - k, k, -1.0, work, ldwork, V, ldv, 1.0, C, ldc);
                    } // if (lastV > k)

                    // W = W * V2'
                    row1 = Math.max(1,k);
                    for (p = 0; p < row1; p++) {

                        for (q = 0; q < k; q++) {
                            array1[p][q] = V[p + lastV - k][q];
                        }
                    }

                    dtrmm('R', 'U', 'T', 'U', lastC, k, 1.0, array1, row1, work, ldwork);

                    // C2 = C2 - W
                    for (j = 0; j < k; j++) {

                        for (i = 0; i < lastC; i++) {
                            C[i][lastV - k + j] = C[i][lastV - k + j] - work[i][j];
                        }
                    }
                } // else if ((side == 'R') || (side == 'r'))
            } // else ((direct == 'B') || (direct == 'b'))
        } // if ((storev == 'C') || (storev == 'c'))
        else if ((storev == 'R') || (storev == 'r')) {

            if ((direct == 'F') || (direct == 'f')) {

                // Let V = ( V1 V2 )  (V1: first k columns)
                // where V1 is unit upper triangular
                if ((side == 'L') || (side == 'l')) {

                    // Form H * C or H' * C where C = ( C1 )
                    //                                ( C2 )
                    lastV = Math.max(k, iladlc(k, m, V, ldv));
                    lastC = iladlc(lastV, n, C, ldc);
                    // W = C' * V' = (C1'*V1' + C2'V2') (stored in work)
                    // W = C1'
                    for (j = 0; j < k; j++) {

                        for (p = 0; p < lastC; p++) {
                            work[p][j] = C[j][p];
                        }
                    }

                    // W = W * V1'
                    dtrmm('R', 'U', 'T', 'U', lastC, k, 1.0, V, ldv, work, ldwork);

                    if (lastV > k) {

                        // W = W + C2'*V2'
                        row1 = Math.max(1, lastV - k);
                        array1 = new double[row1][lastC];

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < lastC; q++) {
                                array1[p][q] = C[p + k][q];
                            }
                        }

                        row2 = Math.max(1,k);
                        array2 = new double[row2][lastV - k];

                        for (p = 0; p < row2; p++) {

                            for (q = 0; q < (lastV - k); q++) {
                                array2[p][q] = V[p][q + k];
                            }
                        }

                        dgemm('T', 'T', lastC, k, lastV - k, 1.0, array1, row1, array2, row2, 1.0, work, ldwork);
                    } // if (lastV > k)

                    // W = W * T' or W * T
                    dtrmm('R', 'U', transt, 'N', lastC, k, 1.0, T, ldt, work, ldwork);

                    // C = C - V' * W'
                    if (lastV > k) {

                        // C2 = C2 - V2' * W'
                        row1 = Math.max(1, k);
                        array1 = new double[row1][lastV - k];

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < (lastV - k); q++) {
                                array1[p][q] = V[p][q + k];
                            }
                        }

                        array2 = new double[row1][lastC];

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < lastC; q++) {
                                array2[p][q] = C[p + k][q];
                            }
                        }

                        dgemm('T', 'T', lastV - k, lastC, k, -1.0, array1, row1, work, ldwork, 1.0, array2, row1);

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < lastC; q++) {
                                C[p + k][q] = array2[p][q];
                            }
                        }
                    } // if (lastV > k)

                    // W = W * V1
                    dtrmm('R', 'U', 'N', 'U', lastC, k, 1.0, V, ldv, work, ldwork);

                    // C1 = C1 - W'
                    for (j = 0; j < k; j++) {

                        for (i = 0; i < lastC; i++) {
                            C[j][i] = C[j][i] - work[i][j];
                        }
                    }
                } // if ((side == 'L') || (side == 'l'))
                else if ((side == 'R') || (side == 'r')) {

                    // Form C * H or C * H' where C = ( C1 C2 )
                    lastV = Math.max(k, iladlc(k, n, V, ldv));
                    lastC = iladlr(m, lastV, C, ldc);
                    // W = C * V' = (C1*V1' + C2*V2') (stored in work)
                    // W = C1
                    for (j = 0; j < k; j++) {

                        for (p = 0; p < lastC; p++) {
                            work[p][j] = C[p][j];
                        }
                    }

                    // W = W * V1'
                    dtrmm('R', 'U', 'T', 'U', lastC, k, 1.0, V, ldv, work, ldwork);

                    if (lastV > k) {

                        // W = W + C2 * V2'
                        row1 = Math.max(1, lastC);
                        array1 = new double[row1][lastV - k];

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < (lastV - k); q++) {
                                array1[p][q] = C[p][q + k];
                            }
                        }

                        row2 = Math.max(1,k);
                        array2 = new double[row2][lastV - k];

                        for (p = 0; p < row2; p++) {

                            for (q = 0; q < (lastV - k); q++) {
                                array2[p][q] = V[p][q + k];
                            }
                        }

                        dgemm('N', 'T', lastC, k, lastV - k, 1.0, array1, row1, array2, row2, 1.0, work, ldwork);
                    } // if (lastV > k)

                    // W = W * T or W * T'
                    dtrmm('R', 'U', trans, 'N', lastC, k, 1.0, T, ldt, work, ldwork);

                    // C = C - W * V
                    if (lastV > k) {

                        // C2 = C2 - W * V2
                        row1 = Math.max(1, k);
                        array1 = new double[row1][lastV - k];

                        for (p = 0; p < row1; p++) {

                            for (q = 0; q < (lastV - k); q++) {
                                array1[p][q] = V[p][q + k];
                            }
                        }

                        row2 = Math.max(1, lastC);
                        array2 = new double[row2][lastV - k];

                        for (p = 0; p < row2; p++) {

                            for (q = 0; q < (lastV - k); q++) {
                                array2[p][q] = C[p][q + k];
                            }
                        }

                        dgemm('N', 'N', lastC, lastV - k, k, -1.0, work, ldwork, array1, row1, 1.0, array2, row2);

                        for (p = 0; p < row2; p++) {

                            for (q = 0; q < (lastV - k); q++) {
                                C[p][q + k] = array2[p][q];
                            }
                        }
                    } // if (lastV > k)

                    // W = W * V1
                    dtrmm('R', 'U', 'N', 'U', lastC, k, 1.0, V, ldv, work, ldwork);

                    // C1 = C1 - W
                    for (j = 0; j < k; j++) {

                        for (i = 0; i < lastC; i++) {
                            C[i][j] = C[i][j] - work[i][j];
                        }
                    }
                } // else if ((side == 'R') || (side == 'r'))
            } // if ((direct == 'F') || (direct == 'f'))
            else { // ((direct == 'B') || (direct == 'B'))

                // Let V = ( V1 V2 )  (V2: last k columns)
                // where V2 is unit lower triangular
                if ((side == 'L') || (side == 'l')) {

                    // Form H * C or H' * C where C = ( C1 )
                    //                                ( C2 )
                    lastV = Math.max(k, iladlc(k, m, V, ldv));
                    lastC = iladlc(lastV, n, C, ldc);
                    // W = C' * V' = (C1'*V1' + C2'*V2') (stored in work)
                    // W = C2'
                    for (j = 0; j < k; j++) {

                        for (p = 0; p < lastC; p++) {
                            work[p][j] = C[lastV - k + j][p];
                        }
                    }

                    // W = W * V2'
                    row1 = Math.max(1,k);
                    array1 = new double[row1][k];

                    for (p = 0; p < row1; p++) {

                        for (q = 0; q < k; q++) {
                            array1[p][q] = V[p][q + lastV - k];
                        }
                    }

                    dtrmm('R', 'L', 'T', 'U', lastC, k, 1.0, array1, row1, work, ldwork);

                    if (lastV > k) {

                        // W = W + C1'*V1'
                        dgemm('T', 'T', lastC, k, lastV - k, 1.0, C, ldc, V, ldv, 1.0, work, ldwork);
                    } // if (lastV > k)

                    // W = W * T' or W * T
                    dtrmm('R', 'L', transt, 'N', lastC, k, 1.0, T, ldt, work, ldwork);

                    // C = C - V' * W'
                    if (lastV > k) {

                        // C1 = C1 - V1' * W'
                        dgemm('T', 'T', lastV - k, lastC, k, -1.0, V, ldv, work, ldwork, 1.0, C, ldc);
                    } // if (lastV > k)

                    // W = W * V2
                    row1 = Math.max(1, k);
                    array1 = new double[row1][k];

                    for (p = 0; p < row1; p++) {

                        for (q = 0; q < k; q++) {
                            array1[p][q] = V[p][q + lastV - k];
                        }
                    }

                    dtrmm('R', 'L', 'N', 'U', lastC, k, 1.0, array1, row1, work, ldwork);

                    // C2 = C2 - W'
                    for (j = 0; j < k; j++) {

                        for (i = 0; i < lastC; i++) {
                            C[lastV - k + j][i] = C[lastV - k + j][i] - work[i][j];
                        }
                    }
                } // if ((side == 'L') || (side == 'l'))
                else if ((side == 'R') || (side == 'r')) {

                    // Form C * H or C * H' where C = ( C1 C2 )
                    lastV = Math.max(k, iladlc(k, n, V, ldv));
                    lastC = iladlr(m, lastV, C, ldc);
                    // W = C * V' = (C1*V1' + C2*V2') (stored in work)
                    // W = C2
                    for (j = 0; j < k; j++) {

                        for (p = 0; p < lastC; p++) {
                            work[p][j] = C[p][lastV - k + j];
                        }
                    }

                    // W = W * V2'
                    row1 = Math.max(1, k);
                    array1 = new double[row1][k];

                    for (p = 0; p < row1; p++) {

                        for (q = 0; q < k; q++) {
                            array1[p][q] = V[p][q + lastV - k];
                        }
                    }

                    dtrmm('R', 'L', 'T', 'U', lastC, k, 1.0, array1, row1, work, ldwork);

                    if (lastV > k) {

                        // W = W + C1 * V1'
                        dgemm('N', 'T', lastC, k, lastV - k, 1.0, C, ldc, V, ldv, 1.0, work, ldwork);
                    } // if (lastV > k)

                    // W = W * T or W * T'
                    dtrmm('R', 'L', trans, 'N', lastC, k, 1.0, T, ldt, work, ldwork);

                    // C = C - W * V
                    if (lastV > k) {

                        // C1 = C1 - W * V1
                        dgemm('N', 'N', lastC, lastV - k, k, -1.0, work, ldwork, V, ldv, 1.0, C, ldc);
                    } // if (lastV > k)

                    // W = W * V2
                    row1 = Math.max(1,k);
                    for (p = 0; p < row1; p++) {

                        for (q = 0; q < k; q++) {
                            array1[p][q] = V[p][q + lastV - k];
                        }
                    }

                    dtrmm('R', 'L', 'N', 'U', lastC, k, 1.0, array1, row1, work, ldwork);

                    // C1 = C1 - W
                    for (j = 0; j < k; j++) {

                        for (i = 0; i < lastC; i++) {
                            C[i][lastV - k + j] = C[i][lastV - k + j] - work[i][j];
                        }
                    }
                } // else if ((side == 'R') || (side == 'r'))
            } // else ((direct == 'B') || (direct == 'B'))
        } // else if ((storev == 'R') || (storev == 'r'))

        return;
    } // dlarfb
    
    /**
     * This is a port of version 3.2 LAPACK auxiliary routine DLARFG Original DLARFG created by Univ. of Tennessee, Univ.
     * of California Berkeley, and NAG Ltd., November, 2006 
     * dlarfg generates a real elementary reflector H of order n,
     * such that H * (alpha) = (beta), H' * H = I. 
     *               ( x )     ( 0 ) 
     * where alpha and beta are scalars, and x is an (n-1)-element real vector. H is represented in the form 
     * H = I - tau * (1) * (1 v'), 
     *               (v) 
     * where tau is a real scalar and v is a real (n-1)-element vector. If the elements of x are all zero,
     * then tau = 0 and H is taken to be the unit matrix. Otherwise 1 <= tau <= 2.
     *
     * @param  n      input int The order of the elementary reflector.
     * @param  alpha  input/output double[] On entry, the value alpha. On exit, it is overwritten with the value beta.
     * @param  x      input/output double[] of dimension (1 + (n-2)*abs(incx)) On entry, the vector x. On exit, it is
     *                overwritten with the vector v.
     * @param  incx   input int The increment between elements of x. incx > 0
     * @param  tau    output double[] The value tau
     */
    private void dlarfg(int n, double[] alpha, double[] x, int incx, double[] tau) {
        int j;
        int knt;
        double beta;
        double rsafmn;
        double safmin;
        double xnorm;

        if (n <= 1) {
            tau[0] = 0.0;

            return;
        }

        xnorm = dnrm2(n - 1, x, incx);

        if (xnorm == 0.0) {

            // H = I
            tau[0] = 0.0;
        } // if (xnorm == 0.0)
        else { // general case

            if (alpha[0] >= 0.0) {
                beta = -Math.abs(dlapy2(alpha[0], xnorm));
            } else {
                beta = Math.abs(dlapy2(alpha[0], xnorm));
            }

            safmin = dlamch('S') / dlamch('E');
            knt = 0;
            if (Math.abs(beta) < safmin) {

                // xnorm, beta may be inaccurate; scale x and recompute them
                rsafmn = 1.0 / safmin;

                do {
                    knt = knt + 1;
                    dscal(n - 1, rsafmn, x, incx);
                    beta = beta * rsafmn;
                    alpha[0] = alpha[0] * rsafmn;
                } while (Math.abs(beta) < safmin);

                // New beta is at most 1, at least safmin
                xnorm = dnrm2(n - 1, x, incx);

                if (alpha[0] >= 0.0) {
                    beta = -Math.abs(dlapy2(alpha[0], xnorm));
                } else {
                    beta = Math.abs(dlapy2(alpha[0], xnorm));
                }
            } // if (Math.abs(beta) < safmin)

            tau[0] = (beta - alpha[0]) / beta;
            dscal(n - 1, 1.0 / (alpha[0] - beta), x, incx);
    
            // If alpha is subnormal, it may lose relative accuracy
            
            for (j = 1; j <= knt; j++) {
                beta = beta * safmin;
            }
            alpha[0] = beta;
        } // else general case

        return;
    } // dlarfg
    
    /**
     * This is a port of version 3.2 LAPACK auxiliary routine DLARFP Original DLARFP created by Univ. of Tennessee, Univ.
     * of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006 
     * dlarfp generates a real elementary reflector H of order n,
     * such that H * (alpha) = (beta), H' * H = I. 
     *               ( x )     ( 0 ) 
     * where alpha and beta are scalars, beta is non-negative, and x is an (n-1)-element real vector.
     * H is represented in the form H = I - tau * (1) * (1 v'), 
     *                                            (v)
     * where tau is a real scalar and v is a real (n-1)-element vector. If the elements of x are all zero,
     * then tau = 0 and H is taken to be the unit matrix. Otherwise 1 <= tau <= 2.
     *
     * @param  n      input int The order of the elementary reflector.
     * @param  alpha  input/output double[] On entry, the value alpha. On exit, it is overwritten with the value beta.
     * @param  x      input/output double[] of dimension (1 + (n-2)*abs(incx)) On entry, the vector x. On exit, it is
     *                overwritten with the vector v.
     * @param  incx   input int The increment between elements of x. incx > 0
     * @param  tau    output double[] The value tau
     */
    private void dlarfp(int n, double[] alpha, double[] x, int incx, double[] tau) {
        int j;
        int knt;
        double beta;
        double rsafmn;
        double safmin;
        double xnorm;

        if (n <= 0) {
            tau[0] = 0.0;

            return;
        }

        xnorm = dnrm2(n - 1, x, incx);

        if (xnorm == 0.0) {

            // H = [+/-1, 0; I], sign chosen so that alpha[0] >= 0
            if (alpha[0] >= 0.0) {
                // When tau[0] == 0.0, the vector is special cased to be all zeros in the
                // application routines.  We do not need to clear it.
                tau[0] = 0.0;
            } // if (alpha[0] >= 0.0)
            else {
                // However, the application routines rely on explicit zero checks when 
                // tau[0] != 0.0, and we must clear x.
                tau[0] = 2.0;
                for (j = 1; j<= n-1; j++) {
                    x[(j-1)*incx] = 0.0;
                }
                alpha[0] = -alpha[0];
            } // else
        } // if (xnorm == 0.0)
        else { // general case

            if (alpha[0] >= 0.0) {
                beta = Math.abs(dlapy2(alpha[0], xnorm));
            } else {
                beta = -Math.abs(dlapy2(alpha[0], xnorm));
            }

            safmin = dlamch('S') / dlamch('E');
            knt = 0;

            if (Math.abs(beta) < safmin) {

                // xnorm, beta may be inaccurate; scale x and recompute them
                rsafmn = 1.0 / safmin;

                do {
                    knt = knt + 1;
                    dscal(n - 1, rsafmn, x, incx);
                    beta = beta * rsafmn;
                    alpha[0] = alpha[0] * rsafmn;
                } while (Math.abs(beta) < safmin);

                // New beta is at most 1, at least safmin
                xnorm = dnrm2(n - 1, x, incx);

                if (alpha[0] >= 0.0) {
                    beta = Math.abs(dlapy2(alpha[0], xnorm));
                } else {
                    beta = -Math.abs(dlapy2(alpha[0], xnorm));
                }
            } // if (Math.abs(beta) < safmin)
            
            alpha[0] = alpha[0] + beta;
            if (beta < 0.0) {
                beta = -beta;
                tau[0] = -alpha[0]/beta;
            }
            else {
                alpha[0] = xnorm * (xnorm/alpha[0]);
                tau[0] = alpha[0]/beta;
                alpha[0] = -alpha[0];
            }
            dscal(n-1, 1.0/alpha[0], x, incx);
            
            for (j = 1; j <= knt; j++) {
                beta = beta * safmin;
            }
            alpha[0] = beta;
        } // else general case

        return;
    } // dlarfp
    
    /**
     * This is a port of the version 3.2 LAPACK auxiliary routine DLARFT Original DLARFT created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlarft forms the triangular factor T of a real block reflector H of order n, which is defined as the
     * product of k elementary reflectors. 
     * If direct = 'F', H = H[0] H[1] ... H[k-1] and T is upper triangular. 
     * If direct = 'B', H = H[k-1] ... H[1] H[0] and T is lower triangular. 
     * If storev = 'C', the vector which defines the elementary reflector H[i] is stored in the i-th column of the array V, and
     *     H = I - V * T * V' 
     * If storev = 'R', the vector which defines the elementary reflector H[i] is stored in the i-th row of the array V, and 
     *     H = I - V' * T * V.
     *
     * @param  direct  input char Specifies the order in which the elementary reflectors are multiplied to form the
     *                 block reflector: 
     *                 = 'F': H = H[0] H[1] ... H[k-1] (forward) 
     *                 = 'B': H = H[k-1] ... H[1] H[0] (Backward)
     * @param  storev  input char Specifies how the vectors which define the elementary reflectors are stored (see also
     *                 Further Details): 
     *                 = 'C': columnwise 
     *                 = 'R': rowwise
     * @param  n       input int The order of the block reflector H. n >= 0.
     * @param  k       input int The order of the triangular factor T ( = the number of elementary reflectors). k >= 1.
     * @param  V       (input/output) double[][] 
     *                 If storev = 'C', dimension = ldv by k. 
     *                 If storev = 'R', dimension = ldv by n. 
     *                 See further details.
     * @param  ldv     input int The leading dimension of the array V. If storev = 'C', ldv >= max(1,n). If storev =
     *                 'R', ldv >= k.
     * @param  tau     input double[] of dimension k. tau[i] must contain the scalar factor of the elementary reflector
     *                 H[i].
     * @param  T       output double[][] of dimension ldt by k. The k by k triangular factor T of the block reflector.
     *                 If direct = 'F', T is upper triangular. If direct = 'B', T is lower triangular. The rest of the
     *                 array is not used.
     * @param  ldt     input int The leading dimension of the array T. ldt >= k.
     *
     *                 <p>Further Details: The shape of the matrix V and the storage of the vectors which define the
     *                 H[i] is best illustrated by the following example with n = 5 and k = 3. The elements equal to 1
     *                 are not stored; the corresponding array elements are modified but restored on exit. The rest of
     *                 the array is not used. 
     *                 direct = 'F' and storev = 'C': 
     *                 V = ( 1       )
     *                     (v1  1    ) 
     *                     (v1 v2  1 )
     *                     (v1 v2 v3 )
     *                     (v1 v2 v3 ) 
     *                 direct = 'F' and storev = 'R': 
     *                 V = ( 1 v1 v1 v1 v1 ) 
     *                     (    1 v2 v2 v2 ) 
     *                     (       1 v3 v3 )
     *                 direct = 'B' and storev = 'C': 
     *                 V = ( v1 v2 v3 ) 
     *                     ( v1 v2 v3 ) 
     *                     (  1 v2 v3 ) 
     *                     (     1 v3 ) 
     *                     (        1 ) 
     *                 direct = 'B' and storev = 'R': 
     *                 V = ( v1 v1  1      ) 
     *                     ( v2 v2 v2  1   ) 
     *                     ( v3 v3 v3 v3 1 )</p>
     */
    private void dlarft(char direct, char storev, int n, int k, double[][] V, int ldv, double[] tau, double[][] T,
                        int ldt) {
        int i;
        int j;
        double vii;
        double[] vector1;
        double[] vector2 = null;
        double[][] array1;
        int p;
        int q;
        int lastV;
        int prevLastV;

        // Quick return if possible
        if (n == 0) {
            return;
        }

        if ((direct == 'F') || (direct == 'f')) {
            prevLastV = n;
            for (i = 1; i <= k; i++) {
                prevLastV = Math.max(i, prevLastV);
                if (tau[i - 1] == 0.0) {

                    // H[i-1] = I
                    for (j = 1; j <= i; j++) {
                        T[j - 1][i - 1] = 0.0;
                    }
                } // if (tau[i-1] == 0.0)
                else { // tau[i-1] != 0.0

                    // general case
                    vii = V[i - 1][i - 1];
                    V[i - 1][i - 1] = 1.0;

                    if ((storev == 'C') || (storev == 'c')) {
                        // Skip any trailing zeros
                        for (lastV = n; lastV >= i+1; lastV--) {
                            if (V[lastV-1][i-1] != 0.0) {
                                break;
                            }
                        }
                        j = Math.min(lastV, prevLastV);

                        // T(0:i-2,i-1) = -tau[i-1] * V(i-1:j-1,0:i-2)' * V(i-1:j-1,i-1)
                        array1 = new double[j - i + 1][i - 1];

                        for (p = 0; p < (j - i + 1); p++) {

                            for (q = 0; q < (i - 1); q++) {
                                array1[p][q] = V[p + i - 1][q];
                            }
                        }

                        vector1 = new double[j - i + 1];

                        for (p = 0; p < (j - i + 1); p++) {
                            vector1[p] = V[p + i - 1][i - 1];
                        }

                        vector2 = new double[i - 1];

                        for (p = 0; p < (i - 1); p++) {
                            vector2[p] = T[p][i - 1];
                        }

                        dgemv('T', j - i + 1, i - 1, -tau[i - 1], array1, j - i + 1, vector1, 1, 0.0, vector2, 1);

                        for (p = 0; p < (i - 1); p++) {
                            T[p][i - 1] = vector2[p];
                        }
                    } // if ((storev == 'C') || (storev == 'c'))
                    else { // ((storev == 'R') || (storev == 'r'))
                        // Skip any trailing zeros.
                        for (lastV = n; lastV >= i+1; lastV--) {
                            if (V[i-1][lastV-1] != 0.0) {
                                break;
                            }
                        }
                        j = Math.min(lastV, prevLastV);
                        // T(0:i-2,i-1) = -tau[i-1] * V(0:i-2,i-1:j-1) * V(i-1,i-1:j-1)'
                        array1 = new double[i - 1][j - i + 1];

                        for (p = 0; p < (i - 1); p++) {

                            for (q = 0; q < (j - i + 1); q++) {
                                array1[p][q] = V[p][q + i - 1];
                            }
                        }

                        vector1 = new double[j - i + 1];

                        for (p = 0; p < (j - i + 1); p++) {
                            vector1[p] = V[i - 1][p + i - 1];
                        }

                        vector2 = new double[i - 1];

                        for (p = 0; p < (i - 1); p++) {
                            vector2[p] = T[p][i - 1];
                        }

                        dgemv('N', i - 1, j - i + 1, -tau[i - 1], array1, i - 1, vector1, 1, 0.0, vector2, 1);

                        for (p = 0; p < (i - 1); p++) {
                            T[p][i - 1] = vector2[p];
                        }
                    } // else ((storev == 'R') || (storev == 'r'))

                    V[i - 1][i - 1] = vii;
                    
                    // T(0:i-2,i-1) = T(0:i-2,0:i-2) * T(0:i-2,i-1)
                    dtrmv('U', 'N', 'N', i - 1, T, ldt, vector2, 1);

                    for (p = 0; p < (i - 1); p++) {
                        T[p][i - 1] = vector2[p];
                    }

                    T[i - 1][i - 1] = tau[i - 1];
                    
                    if (i > 1) {
                        prevLastV = Math.max(prevLastV, lastV);
                    }
                    else {
                        prevLastV = lastV;
                    }
                } // else tau[i-1] != 0.0
            } // for (i = 1; i <= k; i++)
        } // if ((direct == 'F') || (direct == 'f'))
        else { // ((direct == 'B') || (direct == 'b'))
            prevLastV = 1;
            for (i = k; i >= 1; i--) {

                if (tau[i - 1] == 0.0) {

                    // H[i-1] = I
                    for (j = i; j <= k; j++) {
                        T[j - 1][i - 1] = 0.0;
                    }
                } // if (tau[i-1] == 0.0)
                else { // tau[i-1] != 0.0

                    // general case
                    if (i < k) {

                        if ((storev == 'C') || (storev == 'c')) {
                            vii = V[n - k + i - 1][i - 1];
                            V[n - k + i - 1][i - 1] = 1.0;
                            // Skip any leading zeros
                            for (lastV = 1; lastV <= i-1; lastV++) {
                                if (V[lastV-1][i-1] != 0.0) {
                                    break;
                                }
                            }
                            j = Math.max(lastV, prevLastV);
                            // T(i:k-1,i-1) = -tau[i-1] * V(j-1:n-k+i-1,i:k-1)' *
                            // V(j-1:n-k+i-1,i-1)
                            array1 = new double[n - k + i - j + 1][k - i];

                            for (p = 0; p < (n - k + i - j + 1); p++) {

                                for (q = 0; q < (k - i); q++) {
                                    array1[p][q] = V[j - 1 + p][q + i];
                                }
                            }

                            vector1 = new double[n - k + i - j + 1];

                            for (p = 0; p < (n - k + i - j + 1); p++) {
                                vector1[p] = V[j - 1 + p][i - 1];
                            }

                            vector2 = new double[k - i];

                            for (p = 0; p < (k - i); p++) {
                                vector2[p] = T[p + i][i - 1];
                            }

                            dgemv('T', n - k + i - j + 1, k - i, -tau[i - 1], array1, n - k + i - j + 1, vector1, 1, 0.0, vector2, 1);

                            for (p = 0; p < (k - i); p++) {
                                T[p + i][i - 1] = vector2[p];
                            }
                            
                            V[n - k + i - 1][i - 1] = vii;
                        } // if ((storev == 'C') || (storev == 'c'))
                        else { // ((storev == 'R') || (storev == 'r'))
                            vii = V[i - 1][n - k + i - 1];
                            V[i - 1][n - k + i - 1] = 1.0;
                            // Skip any leading zeros
                            for (lastV = 1; lastV <= i-1; lastV++) {
                                if (V[i-1][lastV-1] != 0.0) {
                                    break;
                                }
                            }
                            j = Math.max(lastV, prevLastV);

                            // T(i:k-1,i-1) = -tau[i-1] * V(i:k-1,j-1:n-k+i-1) *
                            // V(i-1,j-1:n-k+i-1)'
                            array1 = new double[k - i][n - k + i - j + 1];

                            for (p = 0; p < (k - i); p++) {

                                for (q = 0; q < (n - k + i - j + 1); q++) {
                                    array1[p][q] = V[p + i][j - 1 + q];
                                }
                            }

                            vector1 = new double[n - k + i - j + 1];

                            for (p = 0; p < (n - k + i - j + 1); p++) {
                                vector1[p] = V[i - 1][j - 1 + p];
                            }

                            vector2 = new double[k - i];

                            for (p = 0; p < (k - i); p++) {
                                vector2[p] = T[p + i][i - 1];
                            }

                            dgemv('N', k - i, n - k + i - j + 1, -tau[i - 1], array1, k - i, vector1, 1, 0.0, vector2, 1);

                            for (p = 0; p < (k - i); p++) {
                                T[p + i][i - 1] = vector2[p];
                            }

                            V[i - 1][n - k + i - 1] = vii;
                        } // else ((storev == 'R') || (storev == 'r'))

                        // T(i:k-1,i-1) = T(i:k-1,i:k-1) * T(i:k-1,i-1)
                        array1 = new double[k - i][k - i];

                        for (p = 0; p < (k - i); p++) {

                            for (q = 0; q < (k - i); q++) {
                                array1[p][q] = T[p + i][q + i];
                            }
                        }

                        dtrmv('L', 'N', 'N', k - i, array1, k - i, vector2, 1);

                        for (p = 0; p < (k - i); p++) {
                            T[p + i][i - 1] = vector2[p];
                        }
                        
                        if (i > 1) {
                            prevLastV = Math.min(prevLastV, lastV);
                        }
                        else {
                            prevLastV = lastV;
                        }
                    } // if (i < k)

                    T[i - 1][i - 1] = tau[i - 1];
                } // else tau[i-1] != 0.0
            } // for (i = k; i >= 1; i--)
        } // else ((direct == 'B') || (direct == 'b'))

        return;
    } // dlarft
    
    /**
     * This is a port of the 10/14/93 DNRM2 function Original code written by Sven Hammarling, Nag Ltd. dnrm2 returns
     * the euclidean norm of a vector via the function sqrt(x'*x)
     *
     * @param   n     int
     * @param   x     double[]
     * @param   incx  int
     *
     * @return  double
     */
    private double dnrm2(int n, double[] x, int incx) {
        int ix;
        double absxi;
        double norm;
        double scale;
        double ssq;
        double ratio;

        if ((n < 1) || (incx < 1)) {
            norm = 0.0;
        } else if (n == 1) {
            norm = Math.abs(x[0]);
        } else {
            scale = 0.0;
            ssq = 1.0;

            for (ix = 0; ix <= ((n - 1) * incx); ix += incx) {

                if (x[ix] != 0.0) {
                    absxi = Math.abs(x[ix]);

                    if (scale < absxi) {
                        ratio = scale / absxi;
                        ssq = 1.0 + (ssq * ratio * ratio);
                        scale = absxi;
                    } else {
                        ratio = absxi / scale;
                        ssq = ssq + (ratio * ratio);
                    }
                } // if (x[ix] != 0.0)
            } // for (ix = 0; ix <= (n-1)*incx; ix += incx)

            norm = scale * Math.sqrt(ssq);
        }

        return norm;
    } // dnrm2
    
    /**
     * This is a port of the version 3.2 LAPACK auxiliary routine DLAPY2 Original DLAPY2 created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlapy2 returns sqrt(x**2 + y**2), taking care not to cause unnecessary overflow.
     *
     * @param   x  input double
     * @param   y  input double
     *
     * @return  double
     */
    private double dlapy2(double x, double y) {
        double w;
        double xabs;
        double yabs;
        double z;
        double ratio;

        xabs = Math.abs(x);
        yabs = Math.abs(y);
        w = Math.max(xabs, yabs);
        z = Math.min(xabs, yabs);

        if (z == 0.0) {
            return w;
        } else {
            ratio = z / w;

            return (w * Math.sqrt(1.0 + (ratio * ratio)));
        }
    } // dlapy2
    
    /**
     * This is a port of LAPACK auxiliary routine (version 3.2.1) ILADLC, April 2009
     * Original ILADLC created by Univ. of Tennessee, Univ. of California Berkeley, Univ. of Colorado
     * Denver, and NAG Ltd.
     * 
     * iladlc scans A for its last non-zero column
     * @param m input int The number of rows in matrix A.
     * @param n input int The number of columns in matrix A.
     * @param A input double[][] of dimension lda by n.  The m by n matrix A.
     * @param lda input int The leading dimension of the array A.  lda >= max(1, m)
     */
    private int iladlc(int m, int n, double A[][], int lda) {
        int i;
        int j;
        
        // Quick test for the common case where one corner is non-zero.
        if (n == 0) {
            return n;
        }
        else if ((A[0][n-1] != 0.0) || (A[m-1][n-1] != 0.0)) {
            return n;
        }
        else {
            // Now scan each column form the end, returning with the first non-zero.
            for (j = n; j >= 1; j--) {
                for (i = 1; i <= m; i++) {
                    if (A[i-1][j-1] != 0.0) {
                        return j;
                    }
                }
            }
            return 0;
        }
    } // iladlc
    
    /**
     * This is a port of LAPACK auxiliary routine (version 3.2.1) ILADLR, April 2009
     * Original ILADLR created by Univ. of Tennessee, Univ. of California Berkeley, Univ. of Colorado
     * Denver, and NAG Ltd.
     * 
     * iladlc scans A for its last non-zero column
     * @param m input int The number of rows in matrix A.
     * @param n input int The number of columns in matrix A.
     * @param A input double[][] of dimension lda by n.  The m by n matrix A.
     * @param lda input int The leading dimension of the array A.  lda >= max(1, m)
     */
    private int iladlr(int m, int n, double A[][], int lda) {
        int i;
        int j;
        int r;
        
        // Quick test for the common case where one corner is non-zero.
        if (m == 0) {
            return m;
        }
        else if ((A[m-1][0] != 0.0) || (A[m-1][n-1] != 0.0)) {
            return m;
        }
        else {
            // Scan up each column tracking the last zero row seen.
            r = 0;
            for (j = 1; j <= n; j++) {
                for (i = m; i >= 1; i--) {
                    if (A[i-1][j-1] != 0.0) {
                        break;
                    }
                }
                r = Math.max(r, i);
            }
            return r;
        }
    } // iladlc
    
    /**
     * This is a port of the 2/8/89 Blas routine Original version written by: Jack Dongarra, Argonne National Laboratory
     * Iain Duff, AERE Harwell. Jeremy Du Croz, Numerical Algorithms Group Ltd. Sven Hammarling, Numerical Algorithms
     * Group Ltd. dgemm performs one of the matrix-matrix operations C = alpha*op(A)*op(B) + beta*C, where op(X) is one
     * of op(X) = X or op(X) = X', alpha and beta are scalars, and A, B, and C are matrices, with op(A) an m by k
     * matrix, op(B) a k by n matrix, and C an m by n matrix.
     *
     * @param  transa  input char On entry, transa specifies the form of op(A) to be used in the matrix multiplication
     *                 as follows:' = 'N' or 'n', op(A) = A. = 'T' or 't', op(A) = A'. = 'C' or 'c', op(A) = A'.
     * @param  transb  input char On entry, transb specifies the form of op(B) to be used in the matrix multiplication
     *                 as follows: = 'N' or 'n', op(B) = B. = 'T' or 't', op(B) = B'. = 'C' or 'c', op(B) = B'.
     * @param  m       input int On entry, m specifies the number of rows of the matrix op(A) and of the matrix C. m
     *                 must be at least zero.
     * @param  n       input int On entry, n specifies the number of columns of the matrix op(B) and the number of
     *                 columns of the matrix C. n must be at least zero.
     * @param  k       input int On entry, k specifies the number of columns of the matrix op(A) and the number of rows
     *                 of the matrix op(B). k must be at least zero.
     * @param  alpha   input double specified scalar
     * @param  A       input double[][] dimension lda by ka, where ka is k when transa = 'N' or 'n', and is m otherwise.
     *                 Before entry with transa = 'N' or 'n', the leading m by k part of the array A must contain the
     *                 matrix A, otherwise the leading k by m part of the array A must contain the matrix A
     * @param  lda     input int On entry, lda specifies the first dimension of A as declared in the calling (sub)
     *                 program. When transa = 'N' or 'n' then lda must be at least max(1,m), otherwise lda must be at
     *                 least max(1,k)
     * @param  B       input double[][] dimension ldb by kb, where kb is n when transb = 'N' or 'n', and is k otherwise.
     *                 Before entry with transb = 'N' or 'n', the leading k by n part of the array B must contain the
     *                 matrix B, otherwise the leading n by k part of the array B must contain the matrix B
     * @param  ldb     input int On entry, ldb specifies the first dimension of B as declared in the calling (sub)
     *                 program. When transb = 'N' or 'n' then ldb must be at least max(1,k), otherwise ldb must be at
     *                 least max(1,n).
     * @param  beta    input double specified scalar When beta is supplied as zero, then C need not be set on input.
     * @param  C       input/output double[][] dimension ldc by n. Before entry, the leading m by n part of the array C
     *                 must contain the matrix C, except when beta is zero, in which case C need not be set on entry. On
     *                 exit, the array C is overwritten by the m by n matrix (alpha*op(A)*op(B) + beta*C).
     * @param  ldc     input int On entry, ldc specifies the first dimension of C as declared in the calling (sub)
     *                 program. ldc must be at least max(1,m).
     */
    private void dgemm(char transa, char transb, int m, int n, int k, double alpha, double[][] A, int lda, double[][] B,
                       int ldb, double beta, double[][] C, int ldc) {
        boolean nota;
        boolean notb;
        int i;
        int info;
        int j;
        int L;
        int nrowa;
        int nrowb;
        double temp;

        // Set nota and notb as true if A and B respectively are not transposed
        // and set nrowa and nrowb as the number of rows of A
        // and the number of rows of B respectively.

        if ((transa == 'N') || (transa == 'n')) {
            nota = true;
        } else {
            nota = false;
        }

        if ((transb == 'N') || (transb == 'n')) {
            notb = true;
        } else {
            notb = false;
        }

        if (nota) {
            nrowa = m;
        } else {
            nrowa = k;
        }

        if (notb) {
            nrowb = k;
        } else {
            nrowb = n;
        }

        // Test the input parameters
        info = 0;

        if ((!nota) && (transa != 'C') && (transa != 'c') && (transa != 'T') && (transa != 't')) {
            info = 1;
        } else if ((!notb) && (transb != 'C') && (transb != 'c') && (transb != 'T') && (transb != 't')) {
            info = 2;
        } else if (m < 0) {
            info = 3;
        } else if (n < 0) {
            info = 4;
        } else if (k < 0) {
            info = 5;
        } else if (lda < Math.max(1, nrowa)) {
            info = 8;
        } else if (ldb < Math.max(1, nrowb)) {
            info = 10;
        } else if (ldc < Math.max(1, m)) {
            info = 13;
        }

        if (info != 0) {
            MipavUtil.displayError("Error dgemm has info = " + info);

            return;
        } // if (info != 0)

        // Quick return if possible
        if ((m == 0) || (n == 0) || (((alpha == 0.0) || (k == 0)) && (beta == 1.0))) {
            return;
        }

        if (alpha == 0.0) {

            if (beta == 0.0) {

                for (j = 0; j < n; j++) {

                    for (i = 0; i < m; i++) {
                        C[i][j] = 0.0;
                    }
                }
            } // if (beta == 0.0)
            else { // beta != 0.0

                for (j = 0; j < n; j++) {

                    for (i = 0; i < m; i++) {
                        C[i][j] = beta * C[i][j];
                    }
                }
            } // else beta != 0.0

            return;
        } // if (alpha == 0.0)

        if (notb) {

            if (nota) {

                // Form C = alpha*A*B + beta*C.
                for (j = 0; j < n; j++) {

                    if (beta == 0.0) {

                        for (i = 0; i < m; i++) {
                            C[i][j] = 0.0;
                        }
                    } // if (beta == 0.0)
                    else if (beta != 1.0) {

                        for (i = 0; i < m; i++) {
                            C[i][j] = beta * C[i][j];
                        }
                    } // else if (beta != 1.0)

                    for (L = 0; L < k; L++) {

                        if (B[L][j] != 0.0) {
                            temp = alpha * B[L][j];

                            for (i = 0; i < m; i++) {
                                C[i][j] = C[i][j] + (temp * A[i][L]);
                            }
                        } // if (B[L][j] != 0.0)
                    } // for (L = 0; L < k; L++)
                } // for (j = 0; j < n; j++)
            } // if (nota)
            else { // !nota

                // Form C = alpha*A'*B + beta*C
                for (j = 0; j < n; j++) {

                    for (i = 0; i < m; i++) {
                        temp = 0.0;

                        for (L = 0; L < k; L++) {
                            temp = temp + (A[L][i] * B[L][j]);
                        }

                        if (beta == 0.0) {
                            C[i][j] = alpha * temp;
                        } else {
                            C[i][j] = (alpha * temp) + (beta * C[i][j]);
                        }
                    } // for (i = 0; i < m; i++)
                } // for (j = 0; j < n; j++)
            } // else !nota
        } // if (notb)
        else { // !notb

            if (nota) {

                // Form C = alpha*A*B' + beta*C
                for (j = 0; j < n; j++) {

                    if (beta == 0.0) {

                        for (i = 0; i < m; i++) {
                            C[i][j] = 0.0;
                        }
                    } // if (beta == 0.0)
                    else if (beta != 1.0) {

                        for (i = 0; i < m; i++) {
                            C[i][j] = beta * C[i][j];
                        }
                    } // else if (beta != 1.0)

                    for (L = 0; L < k; L++) {

                        if (B[j][L] != 0.0) {
                            temp = alpha * B[j][L];

                            for (i = 0; i < m; i++) {
                                C[i][j] = C[i][j] + (temp * A[i][L]);
                            }
                        } // if (B[j][L] != 0.0)
                    } // for (L = 0; L < k; L++)
                } // for (j = 0; j < n; j++)
            } // if (nota)
            else { // !nota

                // Form C = alpha*A'*B' + beta*C
                for (j = 0; j < n; j++) {

                    for (i = 0; i < m; i++) {
                        temp = 0.0;

                        for (L = 0; L < k; L++) {
                            temp = temp + (A[L][i] * B[j][L]);
                        }

                        if (beta == 0.0) {
                            C[i][j] = alpha * temp;
                        } else {
                            C[i][j] = (alpha * temp) + (beta * C[i][j]);
                        }
                    } // for (i = 0; i < m; i++)
                } // for (j = 0; j < n; j++)
            } // else !nota
        } // else !notb

        return;
    } // dgemm
    
    /**
     * Routine ported from 10/22/86 blas dgemv subroutine Original version written by: Jack Dongarra, Argonne National
     * Lab. Jeremy Du Croz, Nag Central Office Sven Hammarling, Nag Central Office. Richard Hanson, Sandia National
     * Labs. dgemv performs one of the matrix-vector operations y = alpha*A*x + beta*y, or y = alpha*A'*x + beta*y,
     * where alpha and beta are scalars, x and y are vectors, and A is an m by n matrix
     *
     * @param  trans  input char On entry, trans specifies the operation to be performed as follows: = 'N' or 'n' y =
     *                alpha*A*x + beta*y = 'T' or 't' y = alpha*A'*x + beta*y = 'C' or 'c' y = alpha*A'*x + beta*y
     * @param  m      input int On entry, m specifies the mumber of rows of matrix A. m must be at least zero.
     * @param  n      input int On entry, n specifies the number of columns of matrix A. n must be at least zero.
     * @param  alpha  input double specified scalar
     * @param  A      input double[][] dimension lda by n Before entry, the leading m by n part of the array A must
     *                contain the matrix of coefficients.
     * @param  lda    input int On entry, lda specifies the first dimension of A as declared in the calling (sub)
     *                program. lda must be at least max(1, m).
     * @param  x      input double[] array of dimension at least (1 + (n-1)*abs(incx)) when trans = 'N' or 'n' and at
     *                least (1 + (m-1)*abs(incx)) otherwise. Before entry, the incremented array x must contain the
     *                vector x.
     * @param  incx   input int On entry, incx specifies the increment for the elements of x. incx must not be zero.
     * @param  beta   input double specified scalar When beta is supplied as zero, then y need not be set on input.
     * @param  y      input/output double[] array of dimension at least (1 + (m-1)*abs(incy)) when trans = 'N' or 'n'
     *                and at least (1 + (n-1)*abs(incy)) otherwise. Before entry with beta non-zero, the incremented
     *                array y must contain the vector y. On exit, array y is overwritten with the updated vector y.
     * @param  incy   input int On entry, incy specifies the increment for the elements of y. incy must not be zero.
     */
    private void dgemv(char trans, int m, int n, double alpha, double[][] A, int lda, double[] x, int incx, double beta,
                       double[] y, int incy) {
        int info;
        int lenx;
        int leny;
        int kx;
        int ky;
        int i;
        int iy;
        int jx;
        int j;
        int jy;
        int ix;
        double temp;

        // Test the input parameters
        info = 0;

        if ((trans != 'N') && (trans != 'n') && (trans != 'T') && (trans != 't') && (trans != 'C') && (trans != 'c')) {
            info = 1;
        } else if (m < 0) {
            info = 2;
        } else if (n < 0) {
            info = 3;
        } else if (lda < Math.max(1, m)) {
            info = 6;
        } else if (incx == 0) {
            info = 8;
        } else if (incy == 0) {
            info = 11;
        }

        if (info != 0) {
            MipavUtil.displayError("Error dgemv has info = " + info);

            return;
        } // if (info != 0)

        // Quick return if possible
        if ((m == 0) || (n == 0) || ((alpha == 0.0) && (beta == 1.0))) {
            return;
        }

        // Set lenx and leny, the lengths of vectors x and y, and set up the
        // start points in arrays x and y.

        if ((trans == 'N') || (trans == 'n')) {
            lenx = n;
            leny = m;
        } else {
            lenx = m;
            leny = n;
        }

        if (incx > 0) {
            kx = 1;
        } else {
            kx = 1 - ((lenx - 1) * incx);
        }

        if (incy > 0) {
            ky = 1;
        } else {
            ky = 1 - ((leny - 1) * incy);
        }

        // Start the operations.  In this version the elements of A are accessed
        // sequentially with one pass through A.
        // First form y = beta*y.
        if (beta != 1.0) {

            if (incy == 1) {

                if (beta == 0.0) {

                    for (i = 0; i < leny; i++) {
                        y[i] = 0.0;
                    }
                } // if (beta == 0.0)
                else { // beta != 0.0

                    for (i = 0; i < leny; i++) {
                        y[i] = beta * y[i];
                    }
                } // else beta != 0.0
            } // if (incy == 1)
            else { // incy != 1
                iy = ky - 1;

                if (beta == 0.0) {

                    for (i = 1; i <= leny; i++) {
                        y[iy] = 0.0;
                        iy = iy + incy;
                    }
                } // if (beta == 0.0)
                else { // beta != 0.0

                    for (i = 1; i <= leny; i++) {
                        y[iy] = beta * y[iy];
                        iy = iy + incy;
                    }
                } // else beta != 0.0
            } // else incy != 1
        } // if (beta != 1.0)

        if (alpha == 0.0) {
            return;
        }

        if ((trans == 'N') || (trans == 'n')) {

            // Form y = alpha*A*x + y.
            jx = kx - 1;

            if (incy == 1) {

                for (j = 0; j < n; j++) {

                    if (x[jx] != 0.0) {
                        temp = alpha * x[jx];

                        for (i = 0; i < m; i++) {
                            y[i] = y[i] + (temp * A[i][j]);
                        } // for (i = 0; i < m; i++)
                    } // if (x[jx] != 0.0)

                    jx = jx + incx;
                } // for (j = 0; j < n; j++)
            } // if (incy == 1)
            else { // incy != 1

                for (j = 0; j < n; j++) {

                    if (x[jx] != 0.0) {
                        temp = alpha * x[jx];
                        iy = ky - 1;

                        for (i = 0; i < m; i++) {
                            y[iy] = y[iy] + (temp * A[i][j]);
                            iy = iy + incy;
                        } // for (i = 0; i < m; i++)
                    } // if (x[jx] != 0.0)

                    jx = jx + incx;
                } // for (j = 0; j < n; j++)
            } // else incy != 1
        } // if (trans == 'N') || (trans == 'n'))
        else { // trans != 'N' && trans != 'n'

            // Form y = alpha*A'*x + y.
            jy = ky - 1;

            if (incx == 1) {

                for (j = 0; j < n; j++) {
                    temp = 0.0;

                    for (i = 0; i < m; i++) {
                        temp = temp + (A[i][j] * x[i]);
                    } // for (i = 0; i < m; i++)

                    y[jy] = y[jy] + (alpha * temp);
                    jy = jy + incy;
                } // for (j = 0; j < n; j++)
            } // if (incx == 1)
            else { // incx != 1

                for (j = 0; j < n; j++) {
                    temp = 0.0;
                    ix = kx - 1;

                    for (i = 0; i < m; i++) {
                        temp = temp + (A[i][j] * x[ix]);
                        ix = ix + incx;
                    } // for (i = 0; i < m; i++)

                    y[jy] = y[jy] + (alpha * temp);
                    jy = jy + incy;
                } // for (j = 0; j < n; j++)
            } // else incx != 1
        } // else trans != 'N' && trans != 'n'

        return;
    } // dgemv
    
    /**
     * This is a port of the 10/22/86 Blas routine DGER Original version written by: Jack Dongarra, Argonne National
     * Lab. Jeremy Du Croz, Nag Central Office. Sven Hammarling, Nag Central Office. Richard Hanson, Sandia National
     * Labs. dger performs the rank 1 operation A = alpha*x*y' + A, where alpha is a scalar, x is an m element vector, y
     * is an n element vector, and A is an m by n matrix.
     *
     * @param  m      input int On entry, m specifies the number of rows of the matrix A. m must be at least zero.
     * @param  n      input int On entry, n specifies the number of columns of the matrix A. n must be at least zero.
     * @param  alpha  input double Specified scalar
     * @param  x      input double[] of dimension at least (1 + (m-1)*abs(incx)). Before entry, the incremented array x
     *                must contain the m element vector x.
     * @param  incx   input int On entry, incx specifies the increment for the elements of x. incx must not be zero.
     * @param  y      input double[] of dimension at least (1 + (n-1)*abs(incy)). Before entry, the incremented array y
     *                must contain the n element vector y.
     * @param  incy   input int On entry, incy specifies the increment for the elements of y. incy must not be zero.
     * @param  A      double[][] of dimension lda by n. Before entry, the leading m by n part of the array A must
     *                contain the matrix of coefficients. On exit, A is overwritten by the updated matrix.
     * @param  lda    input int On entry, lda specifies the first dimension of A as declared in the calling (sub)
     *                program. lda must be at least max(1,m).
     */
    private void dger(int m, int n, double alpha, double[] x, int incx, double[] y, int incy, double[][] A, int lda) {
        double temp;
        int i;
        int info;
        int ix;
        int j;
        int jy;
        int kx;

        // Test the input parameters.
        info = 0;

        if (m < 0) {
            info = 1;
        } else if (n < 0) {
            info = 2;
        } else if (incx == 0) {
            info = 5;
        } else if (incy == 0) {
            info = 7;
        } else if (lda < Math.max(1, m)) {
            info = 9;
        }

        if (info != 0) {
            MipavUtil.displayError("Error dger had info = " + info);

            return;
        }

        // Quick return if possible
        if ((m == 0) || (n == 0) || (alpha == 0.0)) {
            return;
        }

        // Start the operations.  In this version the elements of A are accessed
        // sequentially with one pass through A.
        if (incy > 0) {
            jy = 0;
        } else {
            jy = -(n - 1) * incy;
        }

        if (incx == 1) {

            for (j = 0; j < n; j++) {

                if (y[jy] != 0.0) {
                    temp = alpha * y[jy];

                    for (i = 0; i < m; i++) {
                        A[i][j] = A[i][j] + (x[i] * temp);
                    }
                } // if (y[jy] != 0.0)

                jy = jy + incy;
            } // for (j = 0; j < n; j++)
        } // if (incx == 1)
        else { // incx != 1

            if (incx > 0) {
                kx = 1;
            } else {
                kx = 1 - ((m - 1) * incx);
            }

            for (j = 0; j < n; j++) {

                if (y[jy] != 0.0) {
                    temp = alpha * y[jy];
                    ix = kx - 1;

                    for (i = 0; i < m; i++) {
                        A[i][j] = A[i][j] + (x[ix] * temp);
                        ix = ix + incx;
                    } // for (i = 0; i < m; i++)
                } // if (y[jy] != 0.0)

                jy = jy + incy;
            } // for (j = 0; j < n; j++)
        } // else incx != 1

        return;
    } // dger
    
    /**
     * This is a port of the 2/8/89 Blas routine DTRMM Original code written by: Jack Dongarra, Argonne National
     * Laboratory Iain Duff, AERE Harwell. Jeremy Du Croz, Numerical Algorithms Group Ltd. Sven Hammarling, Numerical
     * Algorithms Group Ltd. dtrmm performs one of the matrix-matrix operations B = alpha*op(A)*B or B = alpha*B*op(A),
     * where alpha is scalar, B is an m by n matrix, A is a unit, or non-unit, upper or lower tringular matrix and op(A)
     * is one of op(A) = A or op(A) = A'.
     *
     * @param  side    input char On entry, side specifies whether op(A) multiplies B from the left or right as follows:
     *                 = 'L' or 'l' B = alpha*op(A)*B = 'R' or 'r' B = alpha*B*op(A)
     * @param  uplo    input char On entry, uplo specifies whether matrix A is an upper or lower triangular matrix as
     *                 follows: = 'U' or 'u' A is an upper triangular matrix = 'L' or 'l' A is a lower triangular matrix
     * @param  transa  input char On entry, transa specifies the form of op(A) to be used in the matrix multiplication
     *                 as follows: = 'N' or 'n' op(A) = A = 'T' or 't' op(A) = A' = 'C' or 'c' op(A) = A'
     * @param  diag    input char On entry, diag specifies whether or not A is unit triangular as follows: = 'U' or 'u'
     *                 A is assumed to be unit triangular = 'N' or 'n' A is not assumed to be unit triangular
     * @param  m       input int On entry, m specifies the number of rows of B. m must be at least zero.
     * @param  n       input int On entry, n specifies the number of columns of B. n must be at least zero.
     * @param  alpha   input double Specified scalar. When alpha is zero then A is not referenced and B need not be set
     *                 before entry.
     * @param  A       input double[][] of dimension lda by k, where k is m when side = 'L' or 'l' and is n when side =
     *                 'R' or 'r'. Before entry with uplo = 'U' or 'u', the leading k by k upper triangular part of the
     *                 array A must contain the upper triangular matrix and the strictly lower triangular part of A is
     *                 not referenced. Before entry with uplo = 'L' or 'l', the leading k by k lower triangular part of
     *                 the array A must contain the lower triangular matrix and the strictly upper triangular part of A
     *                 is not referenced. Note that when diag = 'U' or 'u', the diagonal elements of A are not
     *                 referenced either, but are assumed to be unity.
     * @param  lda     input int On entry, lda specifies the first dimension of A as declared in the calling (sub)
     *                 program. When side = 'L' or 'l' then lda must be at least max(1,m), when side = 'R' or 'r' then
     *                 lda must be at least max(1,n).
     * @param  B       input/output double[][] of dimension ldb by n Before entry, the leading m by n part of the array
     *                 B must contain the matrix B, and on exit is overwritten by the transformed matrix.
     * @param  ldb     input int On entry, ldb specifies the first dimension of B as declared in the calling (sub)
     *                 program. ldb must be at least max(1,m).
     */
    private void dtrmm(char side, char uplo, char transa, char diag, int m, int n, double alpha, double[][] A, int lda,
                       double[][] B, int ldb) {
        boolean lside;
        boolean nounit;
        boolean upper;
        int i;
        int info;
        int j;
        int k;
        int nrowa;
        double temp;

        // Test the input parameters
        if ((side == 'L') || (side == 'l')) {
            lside = true;
        } else {
            lside = false;
        }

        if (lside) {
            nrowa = m;
        } else {
            nrowa = n;
        }

        if ((diag == 'N') || (diag == 'n')) {
            nounit = true;
        } else {
            nounit = false;
        }

        if ((uplo == 'U') || (uplo == 'u')) {
            upper = true;
        } else {
            upper = false;
        }

        info = 0;

        if ((!lside) && (side != 'R') && (side != 'r')) {
            info = 1;
        } else if ((!upper) && (uplo != 'L') && (uplo != 'l')) {
            info = 2;
        } else if ((transa != 'N') && (transa != 'n') && (transa != 'T') && (transa != 't') && (transa != 'C') &&
                       (transa != 'c')) {
            info = 3;
        } else if ((diag != 'U') && (diag != 'u') && (diag != 'N') && (diag != 'n')) {
            info = 4;
        } else if (m < 0) {
            info = 5;
        } else if (n < 0) {
            info = 6;
        } else if (lda < Math.max(1, nrowa)) {
            info = 9;
        } else if (ldb < Math.max(1, m)) {
            info = 11;
        }

        if (info != 0) {
            MipavUtil.displayError("Error dtrmm had info = " + info);

            return;
        }

        // Quick return if possible
        if (n == 0) {
            return;
        }

        if (alpha == 0.0) {

            for (j = 0; j < n; j++) {

                for (i = 0; i < m; i++) {
                    B[i][j] = 0.0;
                }
            }

            return;
        } // if (alpha == 0.0)

        if (lside) {

            if ((transa == 'N') || (transa == 'n')) {

                // Form B = alpha*A*B
                if (upper) {

                    for (j = 0; j < n; j++) {

                        for (k = 0; k < m; k++) {

                            if (B[k][j] != 0.0) {
                                temp = alpha * B[k][j];

                                for (i = 0; i <= (k - 1); i++) {
                                    B[i][j] = B[i][j] + (temp * A[i][k]);
                                }

                                if (nounit) {
                                    temp = temp * A[k][k];
                                }

                                B[k][j] = temp;
                            } // if (B[k][j] != 0.0)
                        } // for (k = 0; k < m; k++)
                    } // for (j = 0; j < n; j++)
                } // if (upper)
                else { // lower

                    for (j = 0; j < n; j++) {

                        for (k = m - 1; k >= 0; k--) {

                            if (B[k][j] != 0.0) {
                                temp = alpha * B[k][j];
                                B[k][j] = temp;

                                if (nounit) {
                                    B[k][j] = B[k][j] * A[k][k];
                                }

                                for (i = k + 1; i < m; i++) {
                                    B[i][j] = B[i][j] + (temp * A[i][k]);
                                }
                            } // if (B[k][j] != 0.0)
                        } // for (k = m-1; k >= 0; k--)
                    } // for (j = 0; j < n; j++)
                } // lower
            } // if (transa == 'N') || (transa == 'n'))
            else { // ((transa != 'N') && (transa != 'n'))

                // Form B = alpha*A'*B
                if (upper) {

                    for (j = 0; j < n; j++) {

                        for (i = m - 1; i >= 0; i--) {
                            temp = B[i][j];

                            if (nounit) {
                                temp = temp * A[i][i];
                            }

                            for (k = 0; k <= (i - 1); k++) {
                                temp = temp + (A[k][i] * B[k][j]);
                            }

                            B[i][j] = alpha * temp;
                        } // for (i = m-1; i >= 0; i--)
                    } // for (j = 0; j < n; j++)
                } // if (upper)
                else { // lower

                    for (j = 0; j < n; j++) {

                        for (i = 0; i < m; i++) {
                            temp = B[i][j];

                            if (nounit) {
                                temp = temp * A[i][i];
                            }

                            for (k = i + 1; k < m; k++) {
                                temp = temp + (A[k][i] * B[k][j]);
                            }

                            B[i][j] = alpha * temp;
                        } // for (i = 0; i < m; i++)
                    } // for (j = 0; j < n; j++)
                } // lower
            } // else ((transa != 'N') && (transa != 'n'))
        } // if (lside)
        else { // !lside

            if ((transa == 'N') || (transa == 'n')) {

                // Form B = alpha*B*A
                if (upper) {

                    for (j = n - 1; j >= 0; j--) {
                        temp = alpha;

                        if (nounit) {
                            temp = temp * A[j][j];
                        }

                        for (i = 0; i < m; i++) {
                            B[i][j] = temp * B[i][j];
                        }

                        for (k = 0; k <= (j - 1); k++) {

                            if (A[k][j] != 0.0) {
                                temp = alpha * A[k][j];

                                for (i = 0; i < m; i++) {
                                    B[i][j] = B[i][j] + (temp * B[i][k]);
                                }
                            } // if (A[k][j] != 0.0)
                        } // for (k = 0; k <= j-1; k++)
                    } // for (j = n-1; j >= 0; j--)
                } // if (upper)
                else { // lower

                    for (j = 0; j < n; j++) {
                        temp = alpha;

                        if (nounit) {
                            temp = temp * A[j][j];
                        }

                        for (i = 0; i < m; i++) {
                            B[i][j] = temp * B[i][j];
                        }

                        for (k = j + 1; k < n; k++) {

                            if (A[k][j] != 0.0) {
                                temp = alpha * A[k][j];

                                for (i = 0; i < m; i++) {
                                    B[i][j] = B[i][j] + (temp * B[i][k]);
                                }
                            } // if (A[k][j] != 0.0)
                        } // for (k = j+1; k < n; k++)
                    } // for (j = 0; j < n; j++)
                } // lower
            } // if (transa == 'N') || (transa == 'n'))
            else { // ((transa != 'N') && (transa != 'n'))

                // Form B = alpha*B*A'
                if (upper) {

                    for (k = 0; k < n; k++) {

                        for (j = 0; j <= (k - 1); j++) {

                            if (A[j][k] != 0.0) {
                                temp = alpha * A[j][k];

                                for (i = 0; i < m; i++) {
                                    B[i][j] = B[i][j] + (temp * B[i][k]);
                                }
                            } // if (A[j][k] != 0.0)
                        } // for (j = 0; j <= k-1; j++)

                        temp = alpha;

                        if (nounit) {
                            temp = temp * A[k][k];
                        }

                        if (temp != 1.0) {

                            for (i = 0; i < m; i++) {
                                B[i][k] = temp * B[i][k];
                            }
                        } // if (temp != 1.0)
                    } // for (k = 0; k < n; k++)
                } // if (upper)
                else { // lower

                    for (k = n - 1; k >= 0; k--) {

                        for (j = k + 1; j < n; j++) {

                            if (A[j][k] != 0.0) {
                                temp = alpha * A[j][k];

                                for (i = 0; i < m; i++) {
                                    B[i][j] = B[i][j] + (temp * B[i][k]);
                                }
                            } // if (A[j][k] != 0.0)
                        } // for (j = k+1; j < n; j++)

                        temp = alpha;

                        if (nounit) {
                            temp = temp * A[k][k];
                        }

                        if (temp != 1.0) {

                            for (i = 0; i < m; i++) {
                                B[i][k] = temp * B[i][k];
                            } // for (i = 0; i < m; i++)
                        } // if (temp != 1.0)
                    } // for (k = n-1; k >= 0; k--)
                } // lower
            } // else ((transa != 'N') && (transa != 'n'))
        } // else !lside

        return;
    } // dtrmm
    
    /**
     * This is a port of the 10/22/86 blas routine DTRMV Original version written by: Jack Dongarra, Argonne National
     * Lab. Jeremy Du Croz, Nag Central Office Sven Hammarling, Nag Central Office Richard Hanson, Sandia National Labs.
     * dtrmv performs one of the matrix-vector operations x = A*x or x = A'*x where x is an n element vector and A is an
     * n by n unit, or non-unit, upper or lower triangular matrix
     *
     * @param  uplo   input char On entry, uplo specifies whether the matrix is an upper or lower triangular matrix as
     *                follows: = 'U' or 'u' A is an upper triangular matrix = 'L' or 'l' A is a lower triangular matrix
     * @param  trans  input char On entry, trans specifies the operation to be performed as follows: = 'N' or 'n', x =
     *                A*x = 'T' or 't', x = A'*x = 'C' or 'c', x = A'*x
     * @param  diag   input char On entry, diag specifies whether or not A is unit triangular as follows: = 'U' or 'u' A
     *                is assumed to be unit triangular. = 'N' or 'n' A is not assumed to be unit triangular.
     * @param  n      input int On entry, n specifies the order of the matrix A. n must be at least zero.
     * @param  A      input double[][] dimension lda by n Before entry with uplo = 'U' or 'u', the leading n by n upper
     *                triangular part of the array A must contain the upper triangular matrix and the strictly lower
     *                triangular part of A is not referenced. Before entry with uplo = 'L' or 'l', the leading n by n
     *                lower triangular part of the array A must contain the lower triangular matrix and the strictly
     *                upper triangular part of A is not referenced. Note that when diag = 'U' or 'u', the diagonal
     *                elements of A are not referenced either, but are assumed to be unity.
     * @param  lda    input int On entry, lda specifies the first dimension of A as declared in the calling (sub)
     *                program. lda must be at least max(1,n).
     * @param  x      input/output double[] of dimension at least (1 + (n-1)*abs(incx)) Before entry, the incremented
     *                array x must contain the n element vector x. On exit, array x is is overwritten with the
     *                transformed vector x.
     * @param  incx   input int On entry, incx specifies the increment for the elements of x. incx must not be zero.
     */
    private void dtrmv(char uplo, char trans, char diag, int n, double[][] A, int lda, double[] x, int incx) {
        double temp;
        int i;
        int info;
        int ix;
        int j;
        int jx;
        int kx = 0;
        boolean nounit;

        // Test the input parameters
        info = 0;

        if ((uplo != 'U') && (uplo != 'u') && (uplo != 'L') && (uplo != 'l')) {
            info = 1;
        } else if ((trans != 'N') && (trans != 'n') && (trans != 'T') && (trans != 't') && (trans != 'C') &&
                       (trans != 'c')) {
            info = 2;
        } else if ((diag != 'U') && (diag != 'u') && (diag != 'N') && (diag != 'n')) {
            info = 3;
        } else if (n < 0) {
            info = 4;
        } else if (lda < Math.max(1, n)) {
            info = 6;
        } else if (incx == 0) {
            info = 8;
        }

        if (info != 0) {
            MipavUtil.displayError("Error dtrmv had info = " + info);

            return;
        }

        // Quick return if possible
        if (n == 0) {
            return;
        }

        if ((diag == 'N') || (diag == 'n')) {
            nounit = true;
        } else {
            nounit = false;
        }

        // Set up the start point in x if the increment is not unity.  This will
        // be (n-1)*incx too small for descending loops.

        if (incx <= 0) {
            kx = 1 - ((n - 1) * incx);
        } else if (incx != 1) {
            kx = 1;
        }

        // Start the operations.  In this version the elements of A are accessed
        // sequentially with one pass through A.
        if ((trans == 'N') || (trans == 'n')) {

            // Form x = A*x
            if ((uplo == 'U') || (uplo == 'u')) {

                if (incx == 1) {

                    for (j = 0; j < n; j++) {

                        if (x[j] != 0.0) {
                            temp = x[j];

                            for (i = 0; i <= (j - 1); i++) {
                                x[i] = x[i] + (temp * A[i][j]);
                            }

                            if (nounit) {
                                x[j] = x[j] * A[j][j];
                            }
                        } // if (x[j] != 0.0)
                    } // for (j = 0; j < n; j++)
                } // if (incx == 1)
                else { // incx != 1
                    jx = kx - 1;

                    for (j = 0; j < n; j++) {

                        if (x[jx] != 0.0) {
                            temp = x[jx];
                            ix = kx - 1;

                            for (i = 0; i <= (j - 1); i++) {
                                x[ix] = x[ix] + (temp * A[i][j]);
                                ix = ix + incx;
                            } // for (i = 0; i <= j-1; i++)

                            if (nounit) {
                                x[jx] = x[jx] * A[j][j];
                            }
                        } // if (x[jx] != 0.0)

                        jx = jx + incx;
                    } // for (j = 0; j < n; j++)
                } // else incx != 1
            } // if ((uplo == 'U') || (uplo == 'u'))
            else { // uplo == 'L' || uplo == 'l'

                if (incx == 1) {

                    for (j = n - 1; j >= 0; j--) {

                        if (x[j] != 0.0) {
                            temp = x[j];

                            for (i = n - 1; i >= (j + 1); i--) {
                                x[i] = x[i] + (temp * A[i][j]);
                            } // for (i = n-1; i >= j+1; i--)

                            if (nounit) {
                                x[j] = x[j] * A[j][j];
                            }
                        } // if (x[j] != 0.0)
                    } // for (j = n-1; j >= 0; j--)
                } // if (incx == 1)
                else { // incx != 1
                    kx = kx + ((n - 1) * incx);
                    jx = kx - 1;

                    for (j = n - 1; j >= 0; j--) {

                        if (x[jx] != 0.0) {
                            temp = x[jx];
                            ix = kx - 1;

                            for (i = n - 1; i >= (j + 1); i--) {
                                x[ix] = x[ix] + (temp * A[i][j]);
                                ix = ix - incx;
                            } // for (i = n-1; i >= j+1; i--)

                            if (nounit) {
                                x[jx] = x[jx] * A[j][j];
                            }
                        } // if (x[jx] != 0.0)

                        jx = jx - incx;
                    } // for (j = n-1; j >= 0; j--)
                } // else incx != 1
            } // else uplo == 'L' || uplo == 'l'
        } // if ((trans == 'N') || (trans == 'n'))
        else { // trans != 'N' && trans != 'n'

            // Form x = A'*x
            if ((uplo == 'U') || (uplo == 'u')) {

                if (incx == 1) {

                    for (j = n - 1; j >= 0; j--) {
                        temp = x[j];

                        if (nounit) {
                            temp = temp * A[j][j];
                        }

                        for (i = j - 1; i >= 0; i--) {
                            temp = temp + (A[i][j] * x[i]);
                        } // for (i = j-1; i >= 0; i--)

                        x[j] = temp;
                    } // for (j = n-1; j >= 0; j--)
                } // if (incx == 1)
                else { // incx != 1
                    jx = kx + ((n - 1) * incx) - 1;

                    for (j = n - 1; j >= 0; j--) {
                        temp = x[jx];
                        ix = jx;

                        if (nounit) {
                            temp = temp * A[j][j];
                        }

                        for (i = j - 1; i >= 0; i--) {
                            ix = ix - incx;
                            temp = temp + (A[i][j] * x[ix]);
                        } // for (i = j-1; i >= 0; i--)

                        x[jx] = temp;
                        jx = jx - incx;
                    } // for (j = n-1; j >= 0; j--)
                } // else incx != 1
            } // if ((uplo == 'U') || (uplo == 'u'))
            else { // ((uplo == 'L') || (uplo == 'l')) {

                if (incx == 1) {

                    for (j = 0; j < n; j++) {
                        temp = x[j];

                        if (nounit) {
                            temp = temp * A[j][j];
                        }

                        for (i = j + 1; i < n; i++) {
                            temp = temp + (A[i][j] * x[i]);
                        } // for (i = j+1; i < n; i++)

                        x[j] = temp;
                    } // for (j = 0; j < n; j++)
                } // if (incx == 1)
                else { // incx != 1
                    jx = kx - 1;

                    for (j = 0; j < n; j++) {
                        temp = x[jx];
                        ix = jx;

                        if (nounit) {
                            temp = temp * A[j][j];
                        }

                        for (i = j + 1; i < n; i++) {
                            ix = ix + incx;
                            temp = temp + (A[i][j] * x[ix]);
                        } // for (i = j+1; i < n; i++)

                        x[jx] = temp;
                        jx = jx + incx;
                    } // for (j = 0; j < n; j++)
                } // else incx != 1
            } // else ((uplo == 'L') || (uplo == 'l'))
        } // else trans != 'N' && trans != 'n'

        return;
    } // dtrmv
    
    /**
     * This is a port of the version 3.2 LAPACK auxiliary routine DLASCL Original DLASCL created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlascl multiplies the m by n real matrix A by the real scalar cto/cfrom. This is done without
     * over/underflow as long as the final result cto*A[i][j]/cfrom does not over/underflow. type specifies that A may
     * be full, upper triangular, lower triangular, upper Hessenberg, or banded.
     *
     * @param  type   input char type indicates the storage type of the input matrix. 
     *                = 'G': A is a full matrix. 
     *                = 'L': A is a lower triangular matrix. 
     *                = 'U': A is an upper triangular matrix. 
     *                = 'H': A is an upper Hessenberg matrix. 
     *                = 'B': A is a symmetric band matrix with lower bandwidth kL and upper bandwidth
     *                       ku and with only the lower half stored. 
     *                = 'Q': A is a symmetric band matrix with lower bandwidth kL and upper bandwidth
     *                       ku and with only the upper half stored. 
     *                = 'Z': A is a band matrix with lower bandwith kL and upper bandwidth ku
     * @param  kL     input int The lower bandwidth of A. Referenced only if type = 'B', 'Q', or 'Z'.
     * @param  ku     input int The upper bandwidth of A. Referenced only if type = 'B', 'Q', or 'Z'.
     * @param  cfrom  input double
     * @param  cto    input double The matrix A is multiplied by cto/cfrom. A[i][j] is computed without over/underflow
     *                if the final result cto*A[i][j]/cfrom can be represented without over/underflow. cfrom must be
     *                nonzero.
     * @param  m      input int The number of rows of the matrix A. m >= 0.
     * @param  n      input int The number of columns of the matrix A. n >= 0.
     * @param  A      input/output double[][] of dimension lda by n. The matrix to be multiplied by cto/cfrom.
     * @param  lda    input int The leading dimension of the array A. lda >= max(1,m).
     * @param  info   output int[] 
     *                = 0: successful exit 
     *                < 0: If info = -i, the i-th argument had an illegal value
     */
    private void dlascl(char type, int kL, int ku, double cfrom, double cto, int m, int n, double[][] A, int lda,
                        int[] info) {
        boolean done;
        int i;
        int itype;
        int j;
        int k1;
        int k2;
        int k3;
        int k4;
        double bignum;
        double cfrom1;
        double cfromc;
        double cto1;
        double ctoc;
        double mul;
        double smlnum;

        // Test the input arguments
        info[0] = 0;

        if ((type == 'G') || (type == 'g')) {
            itype = 0;
        } else if ((type == 'L') || (type == 'l')) {
            itype = 1;
        } else if ((type == 'U') || (type == 'u')) {
            itype = 2;
        } else if ((type == 'H') || (type == 'h')) {
            itype = 3;
        } else if ((type == 'B') || (type == 'b')) {
            itype = 4;
        } else if ((type == 'Q') || (type == 'q')) {
            itype = 5;
        } else if ((type == 'Z') || (type == 'z')) {
            itype = 6;
        } else {
            itype = -1;
        }

        if (itype == -1) {
            info[0] = -1;
        } else if ((cfrom == 0.0) || (Double.isNaN(cfrom))) {
            info[0] = -4;
        } else if (Double.isNaN(cto)) {
            info[0] = -5;
        } else if (m < 0) {
            info[0] = -6;
        } else if ((n < 0) || ((itype == 4) && (n != m)) || ((itype == 5) && (n != m))) {
            info[0] = -7;
        } else if ((itype <= 3) && (lda < Math.max(1, m))) {
            MipavUtil.displayError("itype = " + itype + " m = " + m + " lda = " + lda);
            info[0] = -9;
        } else if (itype >= 4) {

            if ((kL < 0) || (kL > Math.max(m - 1, 0))) {
                info[0] = -2;
            } else if ((ku < 0) || (ku > Math.max(n - 1, 0)) || (((itype == 4) || (itype == 5)) && (kL != ku))) {
                info[0] = -3;
            } else if (((itype == 4) && (lda < (kL + 1))) || ((itype == 5) && (lda < (ku + 1))) ||
                           ((itype == 6) && (lda < ((2 * kL) + ku + 1)))) {
                info[0] = -9;
            }
        } // else if (itype >= 4)

        if (info[0] != 0) {
            MipavUtil.displayError("Error dlascl had info = " + info[0]);
            Preferences.debug("Error dlascl had info = " + info[0] + "\n");
            return;
        }

        // Quick return if possible
        if ((n == 0) || (m == 0)) {
            return;
        }

        // Get machine parameters
        smlnum = dlamch('S');
        bignum = 1.0 / smlnum;

        cfromc = cfrom;
        ctoc = cto;

        do {
            cfrom1 = cfromc * smlnum;
            if (cfrom1 == cfromc) {
                // cfromc is an infinity.  Multiply by a correctly signed zero for finite ctoc,
                // or a NaN if ctoc is infinite
                mul = ctoc/cfromc;
                done = true;
                cto1 = ctoc;
            } // if (cfrom1 == cfromc)
            else {
                cto1 = ctoc / bignum;
                if (cto1 == ctoc) {
                    // ctoc is either 0 or an infinity.  In both cases, ctoc itself
                    // serves as the correct multiplication factor
                    mul = ctoc;
                    done = true;
                    cfromc = 1.0;
                }
                else if ((Math.abs(cfrom1) > Math.abs(ctoc)) && (ctoc != 0.0)) {
                    mul = smlnum;
                    done = false;
                    cfromc = cfrom1;
                } else if (Math.abs(cto1) > Math.abs(cfromc)) {
                    mul = bignum;
                    done = false;
                    ctoc = cto1;
                } else {
                    mul = ctoc / cfromc;
                    done = true;
                }
            }

            if (itype == 0) {

                // Full matrix
                for (j = 0; j < n; j++) {

                    for (i = 0; i < m; i++) {
                        A[i][j] = A[i][j] * mul;
                    }
                }
            } // if (itype == 0)
            else if (itype == 1) {

                // Lower triangular matrix
                for (j = 0; j < n; j++) {

                    for (i = j; i < m; i++) {
                        A[i][j] = A[i][j] * mul;
                    }
                }
            } // else if (itype == 1)
            else if (itype == 2) {

                // Upper triangular matrix
                for (j = 0; j < n; j++) {

                    for (i = 0; i <= Math.min(j, m - 1); i++) {
                        A[i][j] = A[i][j] * mul;
                    }
                }
            } // else if (itype == 2)
            else if (itype == 3) {

                // Upper Hessenberg matrix
                for (j = 0; j < n; j++) {

                    for (i = 0; i <= Math.min(j + 1, m - 1); i++) {
                        A[i][j] = A[i][j] * mul;
                    }
                }
            } // else if (itype == 3)
            else if (itype == 4) {

                // Lower half of a symmetric band matrix
                k3 = kL + 1;
                k4 = n + 1;

                for (j = 0; j < n; j++) {

                    for (i = 0; i <= Math.min(k3 - 1, k4 - j - 2); i++) {
                        A[i][j] = A[i][j] * mul;
                    }
                }
            } // else if (itype == 4)
            else if (itype == 5) {

                // upper half of a symmetric band matrix
                k1 = ku + 2;
                k3 = ku + 1;

                for (j = 0; j < n; j++) {

                    for (i = Math.max(k1 - j - 2, 0); i <= (k3 - 1); i++) {
                        A[i][j] = A[i][j] * mul;
                    }
                }
            } // else if (itype == 5)
            else if (itype == 6) {

                // Band matrix
                k1 = kL + ku + 2;
                k2 = kL + 1;
                k3 = (2 * kL) + ku + 1;
                k4 = kL + ku + 1 + m;

                for (j = 0; j < n; j++) {

                    for (i = Math.max(k1 - j - 2, k2 - 1); i <= Math.min(k3 - 1, k4 - j - 2); i++) {
                        A[i][j] = A[i][j] * mul;
                    }
                }
            } // else if (itype == 6)
        } while (!done);

        return;
    } // dlascl
    
    /**
     * This is a port of the version 3.2 LAPACK auxiliary routine DLANGE Original DLANGE created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlange returns the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of the
     * largest absolute value of a real matrix A.
     *
     * @param   norm  input char Specifies the value to be returned from dlange as:
     *                = 'M' or 'm' returns max(abs(A[i][j])). Note that this is not a matrix norm. 
     *                = '1', 'O' or 'o' returns norm1(A), where norm1 denotes the one norm of a matrix
     *                                 (maximum column sum) 
     *                = 'I' or 'i' returns normI(A), where normI denotes the infinity norm of a matrix (maximum row sum)
     *                = 'F', 'f', 'E', or 'e' returns normF(A), where normF denotes the Frobenius norm of a matrix
     *                                       (square root of sum of squares).
     * @param   m     input int The number of rows of the matrix A. m >= 0. When m = 0, dlange returns zero.
     * @param   n     input int The number of columns of the matrix A. n >= 0. When n = 0, dlange returns zero.
     * @param   A     input double[][] array of dimension (lda,n). Contains the m by n matrix A.
     * @param   lda   input int The leading dimension of the array A. lda >= max(1,m).
     * @param   work  workspace double[] of dimension max(1, lwork), where lwork >= m when norm = 'I';
     *                otherwise, work is not referenced.
     *
     * @return  double
     */
    private double dlange(char norm, int m, int n, double[][] A, int lda, double[] work) {
        int i;
        int j;
        double[] scale = new double[1];
        double[] sum = new double[1];
        double value = 0.0;
        double[] x;

        if (Math.min(m, n) == 0) {
            value = 0.0;
        } else if ((norm == 'M') || (norm == 'm')) {
            // Find max(abs(A[i][j]))

            value = 0.0;

            for (j = 0; j < n; j++) {

                for (i = 0; i < m; i++) {
                    value = Math.max(value, Math.abs(A[i][j]));
                }
            }
        } // else if ((norm == 'M') || (norm == 'm'))
        else if ((norm == 'O') || (norm == 'o') || (norm == '1')) {

            // Find norm1(A)
            value = 0.0;

            for (j = 0; j < n; j++) {
                sum[0] = 0.0;

                for (i = 0; i < m; i++) {
                    sum[0] = sum[0] + Math.abs(A[i][j]);
                }

                value = Math.max(value, sum[0]);
            } // for (j = 0; j < n; j++)
        } // else if ((norm == 'O') || (norm == 'o') || (norm == '1'))
        else if ((norm == 'I') || (norm == 'i')) {

            // Find normI(A)
            for (i = 0; i < m; i++) {
                work[i] = 0.0;
            }

            for (j = 0; j < n; j++) {

                for (i = 0; i < m; i++) {
                    work[i] = work[i] + Math.abs(A[i][j]);
                }
            } // for (j = 0; j < n; j++)

            value = 0.0;

            for (i = 0; i < m; i++) {
                value = Math.max(value, work[i]);
            }
        } // else if ((norm == 'I') || (norm == 'i'))
        else if ((norm == 'F') || (norm == 'f') || (norm == 'E') || (norm == 'e')) {

            // Find normF(A)
            scale[0] = 0.0;
            sum[0] = 1.0;
            x = new double[m];

            for (j = 0; j < n; j++) {

                for (i = 0; i < m; i++) {
                    x[i] = A[i][j];
                }

                dlassq(m, x, 1, scale, sum);
            } // for (j = 0; j < n; j++)

            value = scale[0] * Math.sqrt(sum[0]);
        } // else if ((norm == 'F') || (norm == 'f') || (norm == 'E') ||

        return value;
    } // dlange
    
    /**
     * This is a port of version 3.2 LAPACK auxiliary routine DLASSQ Original DLASSQ created by Univ. of Tennessee, Univ.
     * of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlassq returns the values scl and smsq such that
     *  (scl**2)*smsq = x[0]**2 + x[incx]**2 + ... + x[(n-1)*incx]**2 + (scale**2)*sumsq 
     * The value of sumsq is assumed to be non-negative and scl returns the value 
     *  scl =  max(scale,abs(x[i])). 
     * scale and sumsq refer to the original supplied values in scale[] and sumsq[]. scl and smsq
     * are the returned values in scale[] and sumsq[] that overwrite the orginal values. 
     * This routine makes only one pass through the vector x.
     *
     * @param  n      input int The number of elements to be used from the vector x
     * @param  x      input double[] The vector for which a scaled sum of squares is computed, using x[0], x[incx], ...,
     *                x[(n-1)*incx]
     * @param  incx   input int The increment between successive values of the vector x. incx > 0.
     * @param  scale  input/output double[] On entry, the value scale in the equation above. On exit, scale is
     *                overwritten with scl, the scaling factor for the sum of squares
     * @param  sumsq  input/output double[] On entry, the value sumsq in the equation above. On exit, sumsq is
     *                overwritten with smsq, the basic sum of squares from which scl has been factored out.
     */
    private void dlassq(int n, double[] x, int incx, double[] scale, double[] sumsq) {
        int ix;
        double absxi;
        double ratio;

        if (n > 0) {

            for (ix = 0; ix <= ((n - 1) * incx); ix += incx) {

                if (x[ix] != 0.0) {
                    absxi = Math.abs(x[ix]);

                    if (scale[0] < absxi) {
                        ratio = scale[0] / absxi;
                        sumsq[0] = 1 + (sumsq[0] * ratio * ratio);
                        scale[0] = absxi;
                    } // if (scale[0] < absxi)
                    else { // scale[0] >= absxi
                        ratio = absxi / scale[0];
                        sumsq[0] = sumsq[0] + (ratio * ratio);
                    } // else scale[0] >= absxi
                } // if (x[ix] != 0.0)
            } // for (ix = 0; ix <= (n-1)*incx; ix += incx)
        } // if (n > 0)

        return;
    } // dlassq
    
    /**
     * This is a port of the version 3.2 LAPACK auxiliary routine DLAMCH Original DLAMCH created by Univ. of Tennessee,
     * Univ. of California Berkeley, and NAG Ltd., November, 2006
     * dlamch determines double precision machine parameters.
     *
     * @param   cmach  input char Specifies the value to be returned by dlamch
     *                 = 'E' or 'e', returns eps, relative machine precision 
     *                 = 'S' or 's', returns sfmin, safe minimum, such that 1/sfmin does not overflow
     *                 = 'B' or 'b', returns base, base of the machine
     *                 = 'P' or 'p', returns prec = eps*base
     *                 = 'N' or 'n', returns t, number of (base) digits in the mantissa
     *                 = 'R' or 'r', returns rnd = 1.0 when rounding occurs in addition, 0.0 otherwise 
     *                 = 'M' or 'm', returns emin, minimum exponent before (gradual) underflow 
     *                 = 'U' or 'u', returns rmin, underflow threshold = base**(emin-1)
     *                 = 'L' or 'l', emax, largest exponent before overflow 
     *                 = 'O' or 'o', rmax, overflow threshold = (base**emax)*(1-eps)
     *
     * @return  double
     */
    private double dlamch(char cmach) {

        boolean[] lrnd = new boolean[1];
        int[] beta = new int[1];
        int[] imax = new int[1];
        int[] imin = new int[1];
        int[] it = new int[1];
        double rmach = 0.0;
        double small;

        if (first) {
            first = false;
            dlamc2(beta, it, lrnd, imin, imax);
            base = beta[0];
            t = it[0];

            if (lrnd[0]) {
                rnd = 1.0;
                eps = Math.pow(base, (1 - it[0])) / 2.0;
            } else {
                rnd = 0.0;
                eps = Math.pow(base, (1 - it[0]));
            }

            prec = eps * base;
            emin = imin[0];
            emax = imax[0];
            sfmin = rmin;
            small = 1.0 / rmax;

            if (small >= sfmin) {

                // Use small plus a bit, to avoid the possibility of rounding causing
                // overflow when computing 1/sfmin.
                sfmin = small * (1.0 + eps);
            }
        } // if (first)

        if ((cmach == 'E') || (cmach == 'e')) {
            rmach = eps;
        } else if ((cmach == 'S') || (cmach == 's')) {
            rmach = sfmin;
        } else if ((cmach == 'B') || (cmach == 'b')) {
            rmach = base;
        } else if ((cmach == 'P') || (cmach == 'p')) {
            rmach = prec;
        } else if ((cmach == 'N') || (cmach == 'N')) {
            rmach = t;
        } else if ((cmach == 'R') || (cmach == 'r')) {
            rmach = rnd;
        } else if ((cmach == 'M') || (cmach == 'm')) {
            rmach = emin;
        } else if ((cmach == 'U') || (cmach == 'u')) {
            rmach = rmin;
        } else if ((cmach == 'L') || (cmach == 'l')) {
            rmach = emax;
        } else if ((cmach == 'O') || (cmach == 'o')) {
            rmach = rmax;
        }

        return rmach;
    } // dlamch
    
    /**
     * Port of version 3.2 LAPACK auxiliary routine DLAMC1 Original DLAMC1 created by Univ. of Tennessee, Univ. of
     * California Berkeley, and NAG Ltd., November, 2006
     * dlamc1 determines the machine parameters given by beta, t, rnd, and ieee1.
     *
     * @param  beta   output int[] The base of the machine.
     * @param  t      output int[] The number of (beta) digits in the mantissa
     * @param  rnd    output boolean[] Specifies whether proper rounding (rnd = true) or chopping (rnd = false) occurs in
     *                addition. This may not be a reliable guide to the way in which the machine performs its
     *                arithmetic.
     * @param  ieee1  output boolean[] Specifies whether rounding appears to be done in the IEEE 'round to nearest'
     *                style.
     * This routine is based on the routine ENVRON by Malcolm and incorporates suggestions by Gentleman and Marovich. See
     * Malcolm, M. A. (1972) Algorithms to reveal properties of floating-point arithmetic.  Comms. of the ACM, 15,
     * pp. 949-951.
     * Gentleman, W. M. and Marovich S. B. (1974) More on algorithms that reveal properties of floating point
     * arithmetic units.  Comms. of the ACM, 17, pp. 276-277.
     */
    private void dlamc1(int[] beta, int[] t, boolean[] rnd, boolean[] ieee1) {
        boolean lieee1;
        boolean lrnd;
        int lbeta;
        int lt;
        double a;
        double b;
        double c;
        double f;
        double one;
        double qtr;
        double savec;
        double t1;
        double t2;

        one = 1;

        // lbeta, lieee1, lt, and lrnd are the local values of beta, ieee1, t, and
        // rnd.

        // Throughout this routine we use the function dlamc3 to ensure that
        // relevant values are stored and not held in registers, or are not
        // affected by optimizers.

        // Compute a = 2.0**m with the smallest positive integer m such that
        // computed value(a + 1.0) = a.
        a = 1;
        c = 1;

        while (c == one) {
            a = 2 * a;
            c = dlamc3(a, one);
            c = dlamc3(c, -a);
        } // while (c == one)

        // Now compute b = 2.0**m with the smallest positive integer m such that
        // computed value(a + b) > a
        b = 1;
        c = dlamc3(a, b);

        while (c == a) {
            b = 2 * b;
            c = dlamc3(a, b);
        } // while (c == a)

        // Now compute the base.  a and c are neighboring floating point numbers
        // in the interval (beta**t, beta**(t+1)) and so their difference is beta.
        // Adding 0.25 to c is to ensure that it is truncated to beta and not
        // (beta - 1).

        qtr = one / 4;
        savec = c;
        c = dlamc3(c, -a);
        lbeta = (int) (c + qtr);

        // Now determine whether rounding or chopping occurs, by adding a
        // bit less than beta/2 and a bit more than beta/2 to a.

        b = lbeta;
        f = dlamc3(b / 2, -b / 100);
        c = dlamc3(f, a);

        if (c == a) {
            lrnd = true;
        } else {
            lrnd = false;
        }

        f = dlamc3(b / 2, b / 100);
        c = dlamc3(f, a);

        if ((lrnd) && (c == a)) {
            lrnd = false;
        }

        // Try and decide whether rounding is done in the IEEE 'round to nearest'
        // style. b/2 is half a unit in the last place of the two numbers a and
        // savec.  Furthermore, a is even, i.e. has last bit zero, and savec is
        // odd. Thus adding b/2 to a should not change a, but adding b/2 to savec
        // should change savec.

        t1 = dlamc3(b / 2, a);
        t2 = dlamc3(b / 2, savec);
        lieee1 = (t1 == a) && (t2 > savec) && lrnd;

        // Now find the mantissa, t.  It should be the integer part of log to the
        // base beta of a, however it is safer to determine t by powering.  So we
        // find t as the smallest positive integer for which
        // computed value(beta**t + 1.0) = 1.0.

        lt = 0;
        a = 1;
        c = 1;

        while (c == one) {
            lt = lt + 1;
            a = a * lbeta;
            c = dlamc3(a, one);
            c = dlamc3(c, -a);
        } // while (c == one)

        beta[0] = lbeta;
        t[0] = lt;
        rnd[0] = lrnd;
        ieee1[0] = lieee1;

        return;
    } // dlamc1
    
    /**
     * Port of LAPACK version 3.2 auxiliary routine DLAMC2 Original DLAMC2 created by Univ. of Tennessee, Univ. of
     * California Berkeley, nad NAG Ltd., November, 2006
     * Determines machine parameters 3 globals are determined: 1.) eps double The smallest positive number such that
     * computed value(1.0 - eps) < 1.0 2.) rmin double The smallest normalized number for the machine, given by
     * base**(emin - 1), where base is the floating point value of beta. 3.) rmax double The largest positive number for
     * the machine, given by base**emax*(1-eps), where base is the floating point value of beta.
     *
     * @param  beta  output int[] The base of the machine.
     * @param  t     output int[] The number of (beta) digits in the mantissa.
     * @param  rnd   ouptut boolean Specifies whether proper rounding (rnd == true) or chopping (rnd == false) occurs in
     *               addition. This may not be a reliable guide to the way in which the machine performs its arithmetic
     * @param  emin  output int[] The minimum exponent before (gradual) underflow occurs
     * @param  emax  output int[] The maximum exponent before overflow occurs
     * The computation of EPS is based on a routine PARANOIA by W. Kahan of the University of California at Berkeley.
     */
    private void dlamc2(int[] beta, int[] t, boolean[] rnd, int[] emin, int[] emax) {
        boolean ieee;
        boolean iwarn = false;
        boolean[] lieee1 = new boolean[1];
        boolean[] lrnd = new boolean[1];
        int[] gnmin = new int[1];
        int[] gpmin = new int[1];
        int i;
        int[] lbeta = new int[1];
        int[] lemax = new int[1];
        int lemin;
        int[] lt = new int[1];
        int[] ngnmin = new int[1];
        int[] ngpmin = new int[1];
        double a;
        double b;
        double c;
        double half;
        double leps;
        double[] lrmax = new double[1];
        double lrmin;
        double one;
        double rbase;
        double sixth;
        double small;
        double third;
        double two;
        double zero;

        zero = 0;
        one = 1;
        two = 2;

        // lbeta, lt, lrnd, leps, lemin, and lrmin are the local values of beta, t,
        // rnd, eps, emin, and rmin.

        // Throughout this routine we use the function dlamc3 to ensure that
        // relevant values are stored and not held in registers, or are not
        // affected by optimizers.

        // dlamc1 returns the parameters lbeta, lt, lrnd, and lieee1.
        dlamc1(lbeta, lt, lrnd, lieee1);

        // Start to find eps

        b = lbeta[0];
        a = Math.pow(b, -lt[0]);
        leps = a;

        // Try some tricks to see whether or not this is the correct eps.
        b = two / 3;
        half = one / 2;
        sixth = dlamc3(b, -half);
        third = dlamc3(sixth, sixth);
        b = dlamc3(third, -half);
        b = dlamc3(b, sixth);
        b = Math.abs(b);

        if (b < leps) {
            b = leps;
        }

        leps = 1;

        while ((leps > b) && (b > zero)) {
            leps = b;
            c = dlamc3(half * leps, Math.pow(two, 5.0) * (leps * leps));
            c = dlamc3(half, -c);
            b = dlamc3(half, c);
            c = dlamc3(half, -b);
            b = dlamc3(half, c);
        } // while ((leps > b) && (b > zero))

        if (a < leps) {
            leps = a;
        }

        // Computation of eps complete.

        // Now find emin.  let a = + or - 1, and + or - (1 + base**(-3)).
        // Keep dividing a by beta until (gradual) underflow occurs. This
        // is detected when we cannot recover the previous a.

        rbase = one / lbeta[0];
        small = one;

        for (i = 1; i <= 3; i++) {
            small = dlamc3(small * rbase, zero);
        }

        a = dlamc3(one, small);
        dlamc4(ngpmin, one, lbeta[0]);
        dlamc4(ngnmin, -one, lbeta[0]);
        dlamc4(gpmin, a, lbeta[0]);
        dlamc4(gnmin, -a, lbeta[0]);
        ieee = false;

        if ((ngpmin[0] == ngnmin[0]) && (gpmin[0] == gnmin[0])) {

            if (ngpmin[0] == gpmin[0]) {
                lemin = ngpmin[0];
                // Non twos-complement machnines, no gradual underflow; e.g., VAX
            } else if ((gpmin[0] - ngpmin[0]) == 3) {
                lemin = ngpmin[0] - 1 + lt[0];
                ieee = true;
                // Non twos-complement machines, with gradual underflow; e.g, IEEE
                // standard followers
            } else {
                lemin = Math.min(ngpmin[0], gpmin[0]);

                // A guess; no known machine
                iwarn = true;
            }
        } // if ((ngpmin[0] == ngnmin[0]) && (gpmin[0] == gnmin[0]))
        else if ((ngpmin[0] == gpmin[0]) && (ngnmin[0] == gnmin[0])) {

            if (Math.abs(ngpmin[0] - ngnmin[0]) == 1) {
                lemin = Math.max(ngpmin[0], ngnmin[0]);
                // Twos-complement machines, no gradual underflow, e.g., CYBER 205
            } else {
                lemin = Math.min(ngpmin[0], ngnmin[0]);

                // A guess; no known machine
                iwarn = true;
            }
        } // else if ((ngpmin[0] == gpmin[0]) && (ngnmin[0] == gnmin[0]))
        else if ((Math.abs(ngpmin[0] - ngnmin[0]) == 1) && (gpmin[0] == gnmin[0])) {

            if ((gpmin[0] - Math.min(ngpmin[0], ngnmin[0])) == 3) {
                lemin = Math.max(ngpmin[0], ngnmin[0]) - 1 + lt[0];
                // Twos-complement machines with gradual underflow; no known machine
            } else {
                lemin = Math.min(ngpmin[0], ngnmin[0]);

                // A guess; no known machine
                iwarn = true;
            }
        } // else if ((Math.abs(ngpmin[0] - ngnmin[0]) == 1) && (gpmin[0] == gnmin[0]))
        else {
            lemin = Math.min(ngpmin[0], Math.min(ngnmin[0], Math.min(gpmin[0], gnmin[0])));

            // A guess; no known machine
            iwarn = true;
        }

        if (iwarn) {
            Preferences.debug("iwarn is true in dlamc2 emin = " + lemin + "\n");
            Preferences.debug("The emin value may be incorrect\n");
        }

        // Assume IEEE arithmetic if we found denormalized numbers above, or if
        // arithmetic seems to round in the IEEE style, determined in routine
        // dlamc1.  A true IEEE machine should have both things true; however,
        // faulty macines may have one or the other.
        ieee = ieee || lieee1[0];

        // Compute rmin by successive division by beta.  We could compute rmin as
        // base**(emin-1), but some machines underflow during this computation.

        lrmin = 1;

        for (i = 1; i <= (1 - lemin); i++) {
            lrmin = dlamc3(lrmin * rbase, zero);
        }

        // Finally, call dlamc5 to compute emax and rmax
        dlamc5(lbeta[0], lt[0], lemin, ieee, lemax, lrmax);

        beta[0] = lbeta[0];
        t[0] = lt[0];
        rnd[0] = lrnd[0];
        eps = leps;
        emin[0] = lemin;
        rmin = lrmin;
        emax[0] = lemax[0];
        rmax = lrmax[0];

        return;
    } // dlamc2
    
    /**
     * This is a port of the LAPACK version 3.2 auxiliary routine DLAMC3 Original DLAMC3 created by Univ. of Tennessee,
     * Univ. of California Berkeley, and NAG Ltd., November, 2006
     * dlamc3 is intended to force a and b to be stored prior to doing the addition of a and b, for use in
     * situations where optimizers might hold one of these in a register
     *
     * @param   a  double
     * @param   b  double
     *
     * @return  double
     */
    private double dlamc3(double a, double b) {
        double answer = a + b;

        return answer;
    }
    
    /**
     * This is a port of version 3.2 LAPACK auxiliary routine DLAMC4 Original DLAMC4 created by Univ. of Tennessee, Univ.
     * of California Berkeley, and NAG Ltd., November, 2006
     * dlamc4 is a service routine for dlamc2
     *
     * @param  emin   output int[] The minimum exponent before (gradual) underflow, computed by setting a = start and
     *                dividing by base until the previous a cannot be recovered
     * @param  start  input double The starting point for determining emin.
     * @param  base   input int The base of the machine.
     */
    private void dlamc4(int[] emin, double start, int base) {
        int i;
        double a;
        double b1;
        double b2;
        double c1;
        double c2;
        double d1;
        double d2;
        double one;
        double rbase;
        double zero;

        a = start;
        one = 1;
        rbase = one / base;
        zero = 0;
        emin[0] = 1;
        b1 = dlamc3(a * rbase, zero);
        c1 = a;
        c2 = a;
        d1 = a;
        d2 = a;

        while ((c1 == a) && (c2 == a) && (d1 == a) && (d2 == a)) {
            emin[0] = emin[0] - 1;
            a = b1;
            b1 = dlamc3(a / base, zero);
            c1 = dlamc3(b1 * base, zero);
            d1 = zero;

            for (i = 1; i <= base; i++) {
                d1 = d1 + b1;
            }

            b2 = dlamc3(a * rbase, zero);
            c2 = dlamc3(b2 / rbase, zero);
            d2 = zero;

            for (i = 1; i <= base; i++) {
                d2 = d2 + b2;
            }
        } // while ((c1 == a) && (c2 == a) && (d1 == a) && (d2 == a))

        return;
    } // dlamc4
    
    /**
     * This is a port of the version 3.2 LAPACK auxiliary routine DLAMC5 Original DLAMC5 created by Univ. of Tennessee,
     * Univ. of California Berkeley, and NAG Ltd., November, 2006
     * dlamc5 attempts to compute rmax, the largest machine floating-point number, without overflow. It assumes
     * that emax + abs(emin) sum approximately to a power of 2. It will fail on machines where this assumption does not
     * hold, for example, the Cyber 205 (emin = -28625, emax = 28718). It will also fail if the value supplied for emin
     * is too large (i.e. too close to zero), probably with overflow
     *
     * @param  beta  input int The base of floating-point arithmetic.
     * @param  p     input int The number of base beta digits in the mantissa of a floating-point value.
     * @param  emin  input int The minimum exponent before (gradual) underflow.
     * @param  ieee  input boolean A logical flag specifying whether or not the arithmetic system is thought to comply
     *               with the IEEE standard.
     * @param  emax  output int[] The largest exponent before overflow.
     * @param  rmax  output double[] The largest machine floating-point number.
     */
    private void dlamc5(int beta, int p, int emin, boolean ieee, int[] emax, double[] rmax) {
        int exbits;
        int expsum;
        int i;
        int lexp;
        int nbits;
        int trya;
        int uexp;
        double oldy = 0.0;
        double recbas;
        double y;
        double z;

        // First compute lexp and uexp, two powers of 2 that bound abs(emin).  We
        // then assume that emax + abs(emin) will sum approximately to the bound
        // that is closest to abs(emin).  (emax is the exponent of the required
        // number rmax).

        lexp = 1;
        exbits = 1;
        trya = lexp * 2;

        while (trya <= (-emin)) {
            lexp = trya;
            exbits = exbits + 1;
            trya = lexp * 2;
        } // while (trya <= (-emin))

        if (lexp == -emin) {
            uexp = lexp;
        } else {
            uexp = trya;
            exbits = exbits + 1;
        }

        // Now -lexp is less than or equal to emin, and -uexp is greater than or
        // equal to emin.  exbits is the number of bits needed to store the
        // exponent.

        if ((uexp + emin) > (-lexp - emin)) {
            expsum = 2 * lexp;
        } else {
            expsum = 2 * uexp;
        }

        // expsum is the exponent range, approximately equal to emax - emin + 1
        emax[0] = expsum + emin - 1;
        nbits = 1 + exbits + p;

        // nbits is the total number of bits needed to store a floating-point
        // number.

        if (((nbits % 2) == 1) && (beta == 2)) {

            // Either there are an odd number of bits used to store a floating-point
            // number, which is unlikely, or some bits are not used in the
            // representation of numbers, which is possible, (e.g Cray machines) or
            // the mantissa has an implicit bit, (e.g. IEEE machines, Dec VAX
            // machines), which is perhaps the most likely.  We have to assume the
            // last alternative.  If this is true, then we need to reduce emax by
            // one because there must be some way of representing zero in an
            // implicit-bit system.  On machines like the Cray, we are reducing
            // emax by one unnecessarily.
            emax[0] = emax[0] - 1;
        }

        if (ieee) {

            // Assume we are on an IEEE machine which reserves one exponent for
            // infinity and NaN
            emax[0] = emax[0] - 1;
        }

        // Now create rmax, the largest machine number, which should be equal to
        // (1.0 - beta**(-p))* beta**emax.

        // First compute 1.0 - beta**(-p), being careful that the result is less
        // than 1.0.

        recbas = 1.0 / beta;
        z = beta - 1.0;
        y = 0.0;

        for (i = 1; i <= p; i++) {
            z = z * recbas;

            if (y < 1.0) {
                oldy = y;
            }

            y = dlamc3(y, z);
        } // for (i = 1; i <= p; i++)

        if (y >= 1.0) {
            y = oldy;
        }

        // Now multiply by beta**emax to get rmax

        for (i = 1; i <= emax[0]; i++) {
            y = dlamc3(y * beta, 0.0);
        }

        rmax[0] = y;

        return;
    } // dlamc5
    
    /**
     * ilaenv is ported from the version 3.2.1 LAPACK auxiliary routine Original ILAENV created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd., April, 2009
     *
     * <p>ilaenv is called from the LAPACK routines to choose problem-dependent parameters for the local environment.
     * See ispec for a description of the parameters.</p>
     *
     * <p>This version provides a set of parameters which should give good, but not optimal, performance on many of the
     * currently available computers. Users are encouraged to modify this subroutine to set the tuning parameters for
     * their particular machine using the option and problem size information in the arguments.</p>
     *
     * <p>This routine will not function correctly if it is converted to all lower case. Converting it to all upper case
     * is allowed.</p>
     *
     * @param   ispec  input integer Specifies the parameter to be returned as the value of ilaenv. 
     *                 = 1: the optimal blocksize; if this value is 1,
     *                      an unblocked algorithm will give the best performance. 
     *                 = 2: the minimum block size for which the block routine should be used;
     *                      if the usable block size is less than this value,
     *                      an unblocked routine should be used 
     *                 = 3: the crossover point ( in a block routine, for n less than this value,
     *                      an unblocked routine should be used) 
     *                 = 4: the number of shifts, used in the nonsymmetric eigenvalue routines (deprecated)
     *                 = 5: the minimum column dimension for blocking to be used; 
     *                      rectangular blocks must have dimension at least k by m,
     *                      where k is given by ilaenv(2,...) and m by ilaenv(5,...) 
     *                 = 6: the crossover point for the SVD (when reducing an m by n
     *                      matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds this value,
     *                      a QR factorization is used first to reduce the matrix to a triangular form.)
     *                 = 7: the number of processors 
     *                 = 8: the crossover point for the multishift QR method for nonsymmetric
     *                      eigenvalue problems (deprecated) 
     *                 = 9: maximum size of the subproblems at the bottom of the computation tree in the
     *                      divide-and-conquer algorithm (used by xgelsd and xgesdd) 
     *                = 10: ieee NaN arithmetic can be trusted not to trap 
     *                = 11: infinity can be trusted not to trap
     *                12 <= ispec <= 16:
     *                      xhseqr or one of its subroutines
     *                      see iparmq for detailed explanation
     * @param   name   input String The name of the calling subroutine, in either upper case or lower case.
     * @param   opts   input String The character options to the subroutine name, concatenated into a single character
     *                 string. For example, uplo = 'U', trans = 'T', and diag = 'N' for a triangular routine would be
     *                 specified as opts = 'UTN'. opts has all the character options to subroutine name, in the same
     *                 order that they appear in the argument list for name, even if they are not used in determining
     *                 the value of the parameter specified by ispec.
     * @param   n1     input integer
     * @param   n2     input integer
     * @param   n3     input integer
     * @param   n4     input integer n1 to n4 have problem dimensions for the subroutine name; these may not all be
     *                 required. The problem dimensions n1, n2, n3, and n4 are specified in the order that they appear
     *                 in the argument list for name. n1 is used first, n2 second, and so on, and unused problem
     *                 dimensions are passed a value of -1.
     *
     * @return  answer output integer 
     *                 >= 0; the value of the parameter specified by ispec 
     *                 < 0: il answer = -k, the k-th parameter had an illegal value 
     * The parameter value returned by ilaenv is checked for validity in the calling routine.
     */
    private int ilaenv(int ispec, String name, String opts, int n1, int n2, int n3, int n4) {
        String subnam;
        int answer;
        char first;
        String c1, c2, c3, c4;
        boolean sname;
        boolean cname;
        int nb;
        int nbmin;
        int nx;

        if ((ispec < 1) || (ispec > 16)) {

            // Invalid value for ispec
            return -1;
        }

        if ((ispec == 1) || (ispec == 2) || (ispec == 3)) {

            // Copy name to subnam
            // Make subnam upper case is the first character of name is lower case
            subnam = new String(name);
            first = name.charAt(0);

            if (Character.isLowerCase(first)) {
                subnam = subnam.toUpperCase();
            }

            c1 = subnam.substring(0, 1);

            if ((c1.equals("S")) || (c1.equals("D"))) {
                sname = true;
            } else {
                sname = false;
            }

            if ((c1.equals("C")) || (c1.equals("Z"))) {
                cname = true;
            } else {
                cname = false;
            }

            if (!(cname || sname)) {
                return 1;
            }

            c2 = subnam.substring(1, 3);
            c3 = subnam.substring(3, 6);
            c4 = c3.substring(1, 3);

            if (ispec == 1) {

                // block size
                // In these examples, separate code is provided for setting nb for
                // real and complex.  We assume that nb will take the same value in
                // single or double precision.
                nb = 1;

                if (c2.equals("GE")) {

                    if (c3.equals("TRF")) {
                        nb = 64;
                    } // if (c3.equals("TRF"))
                    else if ((c3.equals("QRF")) || (c3.equals("RQF")) || (c3.equals("LQF")) || (c3.equals("QLF"))) {
                        nb = 32;
                    } // else if ((c3.equals("QRF")) || (c3.equals("RQF")) ||
                    else if (c3.equals("HRD")) {
                        nb = 32;
                    } // else if (c3.equals("HRD"))
                    else if (c3.equals("BRD")) {
                        nb = 32;
                    } // else if (c3.equals("BRD"))
                    else if (c3.equals("TRI")) {
                        nb = 64;
                    } // else if (c3.equals("TRI"))
                } // if (c2.equals("GE"))
                else if (c2.equals("PO")) {

                    if (c3.equals("TRF")) {
                        nb = 64;
                    } // if (c3.equals("TRF"))
                } // else if (c2.equals("PO"))
                else if (c2.equals("SY")) {

                    if (c3.equals("TRF")) {
                        nb = 64;
                    } // if (c3.equals("TRF"))
                    else if (sname && (c3.equals("TRD"))) {
                        nb = 32;
                    } // else if (sname && (c3.equals("TRD")))
                    else if (sname && (c3.equals("GST"))) {
                        nb = 64;
                    } // else if (sname && (c3.equals("GST")))
                } // else if (c2.equals("SY"))
                else if (cname && (c2.equals("HE"))) {

                    if (c3.equals("TRF")) {
                        nb = 64;
                    } // if (c3.equals("TRF"))
                    else if (c3.equals("TRD")) {
                        nb = 32;
                    } // else if (c3.equals("TRD"))
                    else if (c3.equals("GST")) {
                        nb = 64;
                    } // else if (c3.equals("GST"))
                } // else if (cname && (c2.equals("HE")))
                else if (sname && (c2.equals("OR"))) {

                    if ((c3.substring(0, 1).equals("G")) || (c3.substring(0, 1).equals("M"))) {

                        if ((c4.equals("QR")) || (c4.equals("RQ")) || (c4.equals("LQ")) || (c4.equals("QL")) ||
                                (c4.equals("HR")) || (c4.equals("TR")) || (c4.equals("BR"))) {
                            nb = 32;
                        }
                    } // if (c3.substring(0,1).equals("G")) ||
                } // else if (sname && (c2.equals("OR")))
                else if (cname && (c2.equals("UN"))) {

                    if ((c3.substring(0, 1).equals("G")) || (c3.substring(0, 1).equals("M"))) {

                        if ((c4.equals("QR")) || (c4.equals("RQ")) || (c4.equals("LQ")) || (c4.equals("QL")) ||
                                (c4.equals("HR")) || (c4.equals("TR")) || (c4.equals("BR"))) {
                            nb = 32;
                        }
                    } // if (c3.substring(0,1).equals("G")) ||
                } // else if (cname && (c2.equals("UN")))
                else if (c2.equals("GB")) {

                    if (c3.equals("TRF")) {

                        if (n4 <= 64) {
                            nb = 1;
                        } else {
                            nb = 32;
                        }
                    } // if (c3.equals("TRF"))
                } // else if (c2.equals("GB"))
                else if (c2.equals("PB")) {

                    if (c3.equals("TRF")) {

                        if (n2 <= 64) {
                            nb = 1;
                        } else {
                            nb = 32;
                        }
                    } // if (c3.equals("TRF"))
                } // else if (c2.equals("PB"))
                else if (c2.equals("TR")) {

                    if (c3.equals("TRI")) {
                        nb = 64;
                    } // if (c3.equals("TRI"))
                } // else if (C2.equals("TR"))
                else if (c2.equals("LA")) {

                    if (c3.equals("UUM")) {
                        nb = 64;
                    } // if (c3.equals("UUM"))
                } // else if (c2.equals("LA"))
                else if (sname && (c2.equals("ST"))) {

                    if (c3.equals("EBZ")) {
                        nb = 1;
                    } // if (c3.equals("EBZ"))
                } // else if (sname && (c2.equals("ST")))

                return nb;
            } // if (ispec == 1)
            else if (ispec == 2) {
                // minimum block size

                nbmin = 2;

                if (c2.equals("GE")) {

                    if ((c3.equals("QRF")) || (c3.equals("RQF")) || (c3.equals("LQF")) || (c3.equals("QLF")) ||
                            (c3.equals("HRD")) || (c3.equals("BRD")) || (c3.equals("TRI"))) {
                        nbmin = 2;
                    } // if ((c3.equals("QRF")) || (c3.equals("RQF")) || (c3.equals("LQF")) ||
                } // if (c2.equals("GE"))
                else if (c2.equals("SY")) {

                    if (c3.equals("TRF")) {
                        nbmin = 8;
                    } // if (c3.equals("TRF"))
                    else if (sname && (c3.equals("TRD"))) {
                        nbmin = 2;
                    } // else if (sname && (c3.equals("TRD")))
                } // else if (c2.equals("SY"))
                else if (cname && (c2.equals("HE"))) {

                    if (c3.equals("TRD")) {
                        nbmin = 2;
                    } // if (c3.equals("TRD"))
                } // else if (cname && (c2.equals("HE")))
                else if (sname && (c2.equals("OR"))) {

                    if ((c3.substring(0, 1).equals("G")) || (c3.substring(0, 1).equals("M"))) {

                        if ((c4.equals("QR")) || (c4.equals("RQ")) || (c4.equals("LQ")) || (c4.equals("QL")) ||
                                (c4.equals("HR")) || (c4.equals("TR")) || (c4.equals("BR"))) {
                            nbmin = 2;
                        } // if ((c4.equals("QR")) || (c4.equals("RQ")) || (c4.equals("LQ")) ||
                    } // if ((c3.substring(0,1).equals("G")) ||
                } // else if (sname && (c2.equals("OR")))
                else if (cname && (c2.equals("UN"))) {

                    if ((c3.substring(0, 1).equals("G")) || (c3.substring(0, 1).equals("M"))) {

                        if ((c4.equals("QR")) || (c4.equals("RQ")) || (c4.equals("LQ")) || (c4.equals("QL")) ||
                                (c4.equals("HR")) || (c4.equals("TR")) || (c4.equals("BR"))) {
                            nbmin = 2;
                        } // if ((c4.equals("QR")) || (c4.equals("RQ")) || (c4.equals("LQ")) ||
                    } // if ((c3.substring(0,1).equals("G")) ||
                } // else if (cname && (c2.equals("UN")))

                return nbmin;
            } // else if (ispec == 2)
            else { // ispec == 3

                // crossover point

                nx = 0;

                if (c2.equals("GE")) {

                    if ((c3.equals("QRF")) || (c3.equals("RQF")) || (c3.equals("LQF")) || (c3.equals("QLF")) ||
                            (c3.equals("HRD")) || (c3.equals("BRD"))) {
                        nx = 128;
                    } // if ((c3.equals("QRF")) || (c3.equals("RQF")) || (c3.equals("LQF")) ||
                } // if (c2.equals("GE"))
                else if (c2.equals("SY")) {

                    if (sname && (c3.equals("TRD"))) {
                        nx = 32;
                    } // if (sname && (c3.equals("TRD")))
                } // else if (c2.equals("SY"))
                else if (cname && (c2.equals("HE"))) {

                    if (c3.equals("TRD")) {
                        nx = 32;
                    } // if (c3.equals("TRD"))
                } // else if (cname && (c2.equals("HE")))
                else if (sname && (c2.equals("OR"))) {

                    if (c3.substring(0, 1).equals("G")) {

                        if ((c4.equals("QR")) || (c4.equals("RQ")) || (c4.equals("LQ")) || (c4.equals("QL")) ||
                                (c4.equals("HR")) || (c4.equals("TR")) || (c4.equals("BR"))) {
                            nx = 128;
                        } // if ((c4.equals("QR")) || (c4.equals("RQ")) || (c4.equals("LQ")) ||
                    } // if (c3.substring(0,1).equals("G"))
                } // else if (sname && (c2.equals("OR")))
                else if (cname && (c2.equals("UN"))) {

                    if (c3.substring(0, 1).equals("G")) {

                        if ((c4.equals("QR")) || (c4.equals("RQ")) || (c4.equals("LQ")) || (c4.equals("QL")) ||
                                (c4.equals("HR")) || (c4.equals("TR")) || (c4.equals("BR"))) {
                            nx = 128;
                        } // if ((c4.equals("QR")) || (c4.equals("RQ")) || (c4.equals("LQ")) ||
                    } // if (c3.substring(0,1).equals("G"))
                } // else if (cname && (c2.equals("UN")))

                return nx;
            } // else ispec == 3
        } // if ((ispec == 1) || (ispec == 2) || (ispec == 3))
        else if (ispec == 4) {

            // number of shifts (used by xhseqr)
            return 6;
        } // else if (ispec == 4)
        else if (ispec == 5) {

            // minimum column dimension (not used)
            return 2;
        } // else if (ispec == 5)
        else if (ispec == 6) {

            // crossover point for SVD (used by xgelss and xgesvd)
            return (int) (1.6 * Math.min(n1, n2));
        } // else if (ispec == 6)
        else if (ispec == 7) {

            // number of processors (not used)
            return 1;
        } // else if (ispec == 7)
        else if (ispec == 8) {

            // crossover point for multishift (used by xhseqr)
            return 50;
        } // else if (ispec == 8)
        else if (ispec == 9) {

            // maximum size of the subproblems at the bottom of the computation
            // tree in divide-and-conquer algorithm (used by xgelsd and xgesdd)
            return 25;
        } // else if (ispec == 9)
        else if (ispec == 10) {

            // ieee NaN arithmetic can be trusted not to trap
            answer = ieeeck(1, 0.0, 1.0);

            return answer;
        } // else if (ispec == 10)
        else if (ispec == 11){

            // infinity arithmetic can be trusted not to trap
            answer = ieeeck(0, 0.0, 1.0);

            return answer;
        } // else ispec == 11
        else { // 12 <= ispec <= 16
            // answer = iparmq(ispec, name, opts, n1, n2, n3, n4);
            
            return -1;
        }
    } // ilaenv
    	
    /**
     * Version 3.2 auxiliary routine ported form LAPACK Original IEEECK created by Univ. of Tennessee, Univ. of
     * California Berkeley, University of Colorado Denver, and NAG Ltd., November, 2006
     * ieeeck is called form the ilaenv routine to verify that infinity and possibly NaN arithmetic is safe
     * (i.e. will not trap)
     *
     * @param   ispec  input int Specifies whether to test just for infinity arithmetic or whether to test for infinity
     *                 and NaN arithmetic 
     *                 = 0: Verify infinity arithmetic only. 
     *                 = 1: Verify infinity and NaN aritmetic
     * @param   zero   input double Must contain the value 0.0. This is passed to prevent the compiler from optimizing away
     *                 this code
     * @param   one    input double Must contain the value 1.0. This is passed to prevent the compiler from optimizing away
     *                 this code.
     *
     * @return  int    = 0: Arithmetic failed to produce the correct answers 
     *                 = 1: Arithmetic produced the correct answers
     */
    private int ieeeck(int ispec, double zero, double one) {
        double posinf;
        double neginf;
        double negzro;
        double newzro;
        double nan1;
        double nan2;
        double nan3;
        double nan4;
        double nan5;
        double nan6;

        posinf = one / zero;

        if (posinf <= one) {
            return 0;
        }

        neginf = -one / zero;

        if (neginf >= zero) {
            return 0;
        }

        negzro = one / (neginf + one);

        if (negzro != zero) {
            return 0;
        }

        neginf = one / negzro;

        if (neginf >= zero) {
            return 0;
        }

        newzro = negzro + zero;

        if (newzro != zero) {
            return 0;
        }

        posinf = one / newzro;

        if (posinf <= one) {
            return 0;
        }

        neginf = neginf * posinf;

        if (neginf >= zero) {
            return 0;
        }

        posinf = posinf * posinf;

        if (posinf <= one) {
            return 0;
        }

        // Return if we were only asked to check infinity arithmetic
        if (ispec == 0) {
            return 1;
        }

        nan1 = posinf + neginf;

        nan2 = posinf / neginf;

        nan3 = posinf / posinf;

        nan4 = posinf * zero;

        nan5 = neginf * negzro;

        nan6 = nan5 * 0.0;

        if (nan1 == nan1) {
            return 0;
        }

        if (nan2 == nan2) {
            return 0;
        }

        if (nan3 == nan3) {
            return 0;
        }

        if (nan4 == nan4) {
            return 0;
        }

        if (nan5 == nan5) {
            return 0;
        }

        if (nan6 == nan6) {
            return 0;
        }

        return 1;
    } // ieeeck
	
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