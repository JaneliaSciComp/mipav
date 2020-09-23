package gov.nih.mipav.model.algorithms;

import Jama.Matrix;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.LinearEquations;
import gov.nih.mipav.view.Preferences;

/**
LsqFit.jl is licensed under the MIT License:
Returns the argmin over x of `sum(f(x).^2)` using the Levenberg-Marquardt
algorithm, and an estimate of the Jacobian of `f` at x.

> Copyright (c) 2012: John Myles White and other contributors.
> 
> Permission is hereby granted, free of charge, to any person obtaining
> a copy of this software and associated documentation files (the
> "Software"), to deal in the Software without restriction, including
> without limitation the rights to use, copy, modify, merge, publish,
> distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so, subject to
> the following conditions:
> 
> The above copyright notice and this permission notice shall be
> included in all copies or substantial portions of the Software.
> 
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
> NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
> LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
> OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
> WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

public abstract class LsqFit {
	
	
	/**
	module LsqFit

    export curve_fit,
           standard_error,
           margin_error,
           confidence_interval,
           estimate_covar,
           make_hessian,
           Avv,
           # StatsBase reexports
           dof, coef, nobs, mse, rss,
           stderror, weights, residuals

    using Distributions
    using OptimBase
    using LinearAlgebra
    using ForwardDiff
    import NLSolversBase: value, jacobian
    import StatsBase
    import StatsBase: coef, dof, nobs, rss, stderror, weights, residuals

    import Base.summary

    include("geodesic.jl")
    include("levenberg_marquardt.jl")
    include("curve_fit.jl")

end
*/
	private boolean testMode = true;
	protected double x_tol =1e-8; // search tolerance in x
	protected double g_tol = 1e-12; //  search tolerance in gradient
	protected int maxIter = 100000; // maximum number of iterations
    protected double min_step_quality = 1e-3; // for steps below this quality, the trust region is shrinked
    protected double good_step_quality = 0.75; // for steps above this quality, the trust region is expanded
	protected double initial_lambda = 10; // (inverse of) initial trust region radius
    protected double tau = Double.POSITIVE_INFINITY; // set initial trust region radius using the heuristic : tau*maximum(jacobian(df)'*jacobian(df))
	protected double lambda_increase = 10.0; // lambda` is multiplied by this factor after step below min quality
	protected double lambda_decrease = 0.1; // lambda` is multiplied by this factor after good quality steps
    protected boolean show_trace = false; // print a status summary on each iteration if true
    protected double lower[] = null; // bound solution to these limits
    protected double upper[] = null;
    protected boolean geodesicAcceleration = false; // requires hessian matrix
    
    /** integer scalar containing the number of data points. */
    protected int m; 

    /** variables integer scalar containing the number of unknowns. */
    protected int n; // x.length
    protected double initial_x[];
    
    private int iterCt;
    private double x[] = null;
    private double residuals[] = null;
    private boolean converged;
    private double tdata[];
    private double ydata[];
    private int testCase;
    
    private final int DRAPER24D = 0;
    
    private final int ROSENBROCK = 1;
    
    private final int FREUDENSTEIN_AND_ROTH = 2;
    
    private final int POWELL_BADLY_SCALED = 3;
    
    private final int BROWN_BADLY_SCALED = 4;
    
    private final int BEALE = 5;
    
    private final int JENNRICH_AND_SAMPSON = 6;
    
    private final int HELICAL_VALLEY = 7;
    
    private final int BARD = 8;
    
    private final int GAUSSIAN = 9;
    
    private final int MEYER = 10;
    
    private final int GULF_RESEARCH_AND_DEVELOPMENT = 11;
    
    private final int BOX_3D = 12;
    
    private final int POWELL_SINGULAR = 13;
    
    private final int WOOD = 14;
    
    private final int KOWALIK_AND_OSBORNE = 15;
    
    private final int BROWN_AND_DENNIS = 16;
    
    private final int OSBORNE1 = 17;
    
    private final int BIGGS_EXP6 = 18;
   
    private final int OSBORNE2 = 19;
    
    private final int WATSON = 20;
    
    private final int PENALTY_FUNCTION_I = 23;
    
    private final int HOCK25 = 25;
    
    private final int BROWN_ALMOST_LINEAR = 27;
    
    private final int LINEAR_FULL_RANK = 32;
    
    private final int LINEAR_RANK1 = 33;
    
    private final int LINEAR_RANK1_WITH_ZERO_COLUMNS_AND_ROWS = 34;
    
    private final int CHEBYQUAD = 35;
    
    private final int LEVMAR_ROSENBROCK = 50;
    
    private final int MODIFIED_ROSENBROCK = 51;
    
    private final int POWELL_2_PARAMETER = 52;
    
    private final int HOCK1 = 61;
    
    private final int HOCK21_MODIFIED = 62;
    
    private final int HATFLDB = 63;
    
    private final int HATFLDC = 64;
    
    private final int EQUILIBRIUM_COMBUSTION = 65;
    
    public LsqFit(int nPts, double initial_x[]) {
    	m = nPts;
    	n = initial_x.length;
    	this.initial_x = initial_x;
    }
    
    /**
     * To run LsqFit() use:
    
    boolean fullTest = true;
	if (fullTest) {
    	FitAll fa = new FitAll();
    	return;
    }
    
    class FitAll extends LsqFit {

        
        public FitAll() {

            // nPoints data points, 3 coefficients, and exponential fitting
            super();

            
        }

       
        public void fitToFunction(double[] x, double[] residuals) {
            

            return;
        }
        
        public void fitToJacobian(double[] x, double[][] jacobian) {
        	return;
        }
        
        public void fitToHessian(double[] x, double[][][] hessian) {
        // n,n,m hessian
            return;
        }
    }
    */
	
    
    public LsqFit() {
    	// Problems not handled correctly by LsqFit with initial lambda = 10 but handled correctly by ELSUNC port NLConstrainedEngine:
    	// 1.) KOWALIK_AND_OSBORNE at 100 * standard starting point constrained converges to incorrect values
    	// 2.) MEYER at 18 * standard starting point constrained exits due to singular matrix
    	// 3.) BOX_3D converges to incorrect values
    	// 4.) LEVMAR_ROSENBROCK converges to incorrect values
    	// 5.) POWELL_2_PARAMETER does not converge to an answer
    	// 6.) HATFLDB converges to incorrect values
    	// 7.) PENALTY_FUNCTION_I does not converge
    	int i;
    	testMode = true;
    	// Below is an example used to fit y = a0 - a1*(a2**x)
    	// This example implements the solution of problem D of chapter 24 of Applied Regression Analysis, Third Edition by
    	// Norman R. Draper and Harry Smith */
    	// The correct answer is a0 = 72.4326,  a1 = 28.2519, a2 = 0.5968
    	// Works with geodesicAcceleration true and false in 11 and 15 iterations
    	Preferences.debug("Draper problem 24D y = a0 - a1*(a2**x) constrained\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("Correct answer is a0 = 72.4326, a1 = 28.2519, a2 = 0.5968\n", Preferences.DEBUG_ALGORITHM);
    	testMode = true;
    	testCase = DRAPER24D;
    	m = 5;
    	n = 3;
        tdata = new double[5];
        ydata = new double[m];
        initial_x = new double[n];
        tdata[0] = 0.0;
        tdata[1] = 1.0;
        tdata[2] = 2.0;
        tdata[3] = 3.0;
        tdata[4] = 4.0;
        ydata[0] = 44.4;
        ydata[1] = 54.6;
        ydata[2] = 63.8;
        ydata[3] = 65.7;
        ydata[4] = 68.9;
        initial_x[0] = 0.0;
        initial_x[1] = 10.0;
        initial_x[2] = 0.2;
        
        lower = new double[n];
        upper = new double[n];
        lower[0] = -1000.0;
        upper[0] = 1000.0;

        // Constrain a1
        lower[1] = -1000.0;
        upper[1] = 1000.0;

        // Constrain a2
        lower[2] = 0.0;
        upper[2] = 1.0;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example used to fit y = (a0 * log(0.01*i)**(a1) + a2
    	// where a0 = -50, a1 = 2.0/3.0, a2 = 25.0
    	// Variant of test example 25 from Hock and Schittkowski
        // geodesicAcceleration = false converges to correct values in 38 iterataions and geodesicAcceleration = true converges to correct values in 16 iterations
        Preferences.debug("Test example 25 from Hock and Schittkowski constrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = (a0 * log(0.01*i)**(a1) + a2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = -50, a1 = 2.0/3.0, a3 = 25.0\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = HOCK25;
        m = 99;
        n = 3;
    	tdata = new double[99];
    	ydata = new double[m];
    	initial_x = new double[n];
    	for (i = 1; i <= 99; i++) {
    		tdata[i-1] = 0.01 * i;
    		ydata[i-1] = Math.pow((-50.0 * Math.log(tdata[i-1])),2.0/3.0) + 25.0;
    	}
    	initial_x[0] = -100.0;
    	initial_x[1] = 1.0/3.0;
    	initial_x[2] = 12.5;
    	
        lower = new double[n];
        upper = new double[n];
        // Constrain a[0]
        lower[0] = -200.0;
        upper[0] = -0.1;

        // Constrain a[1]
        lower[1] = 0.0;
        upper[1] = 5.0;

        // Constrain a[2]
        lower[2] = 0.0;
        upper[2] = 25.6;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y = a0 + x/(a1*(16 - x) + a2*min(x,16-x))
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Unconstrained only should work at the standard  starting point.  This is
        // because a[1] is a root in the denominator and so with unconstrained for large
        // ranges a[1] = 0 will result in an infinity.  Constrained with a[1] >= 0.1
        // should work at 10.0 * starting point and at 100.0 * starting point.
        // BARD unconstrained converges to correct value for geodesicAcceleration = false in 9 iterations;
        Preferences.debug("Bard function standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0 + x/(a1*(16-x) + a2*min(x,16-x))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.08241, a1 = 1.133, a2 = 2.344\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 8.21487E-3\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BARD;
        m = 15;
        n = 3;
        tdata = new double[15];
        for (i = 1; i <= 15; i++) {
        	tdata[i-1] = i;
        }
        ydata = new double[]{0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 
        		               0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39};
        initial_x = new double[n];
        initial_x[0] = 1.0;
        initial_x[1] = 1.0;
        initial_x[2] = 1.0;
        lower = null;
        upper = null;
        
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y = a0 + x/(a1*(16 - x) + a2*min(x,16-x))
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Unconstrained should only work at the standard  starting point.  This is as expected
        // because a[1] is a root in the denominator and so with unconstrained for large
        // ranges a[1] = 0 will result in an infinity.  Constrained with a[1] >= 0.1
        // should work at 10.0 * starting point and at 100.0 * starting point.
        // BARD constrained at 10 * standard starting point converges to correct value for geodesicAcceleration = false in 17 iterations;
        Preferences.debug("Bard function 10 * standard starting point constrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0 + x/(a1*(16-x) + a2*min(x,16-x))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.08241, a1 = 1.133, a2 = 2.344\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 8.21487E-3\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BARD;
        m = 15;
        n = 3;
        tdata = new double[15];
        for (i = 1; i <= 15; i++) {
        	tdata[i-1] = i;
        }
        ydata = new double[]{0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 
        		               0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39};
        initial_x = new double[n];
        initial_x[0] = 10.0;
        initial_x[1] = 10.0;
        initial_x[2] = 10.0;
        
        
        lower = new double[n];
        upper = new double[n];
        lower[0] = 0.0;
        upper[0] = 20.0;
        lower[1] = 0.1;
        upper[1] = 20.0;
        lower[2] = 0.0;
        upper[2] = 20.0;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y = a0 + x/(a1*(16 - x) + a2*min(x,16-x))
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Unconstrained should only work at the standard  starting point.  This is as expected
        // because a[1] is a root in the denominator and so with unconstrained for large
        // ranges a[1] = 0 will result in an infinity.  Constrained with a[1] >= 0.1
        // should work at 10.0 * starting point and at 100.0 * starting point.
        // BARD constrained at 100 * standard starting point converges to correct value for geodesicAcceleration = false in 28 iterations;
        Preferences.debug("Bard function 100 * standard starting point constrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0 + x/(a1*(16-x) + a2*min(x,16-x))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.08241, a1 = 1.133, a2 = 2.344\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 8.21487E-3\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BARD;
        m = 15;
        n = 3;
        tdata = new double[15];
        for (i = 1; i <= 15; i++) {
        	tdata[i-1] = i;
        }
        ydata = new double[]{0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 
        		               0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39};
        initial_x = new double[n];
        initial_x[0] = 100.0;
        initial_x[1] = 100.0;
        initial_x[2] = 100.0;
        
        
        lower = new double[n];
        upper = new double[n];
        lower[0] = 0.0;
        upper[0] = 200.0;
        lower[1] = 0.1;
        upper[1] = 200.0;
        lower[2] = 0.0;
        upper[2] = 200.0;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // KOWALIK_AND_OSBORNE unconstrained converged to the right answer in 31 iterations
        Preferences.debug("Kowalik and Osborne function standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.1928, a1 = 0.1913, a2 = 0.1231, a3 = 0.1361\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 3.07505E-4\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = KOWALIK_AND_OSBORNE;
        m = 11;
        n = 4;
        tdata = new double[]{4.0, 2.0, 1.0, 0.5, 0.25, 0.167, 0.125, 0.1, 0.0833, 0.0714, 0.0625};
        ydata = new double[]{0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 
        		               0.0323, 0.0235, 0.0246};
        initial_x = new double[n];
        initial_x[0] = 0.25;
        initial_x[1] = 0.39;
        initial_x[2] = 0.415;
        initial_x[3] = 0.39;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // KOWALIK_AND_OSBORNE constrained at 10 * standard starting point converges to correct values in 100 iterations
        Preferences.debug("Kowalik and Osborne function 10 * standard starting point constrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.1928, a1 = 0.1913, a2 = 0.1231, a3 = 0.1361\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 3.07505E-4\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = KOWALIK_AND_OSBORNE;
        m = 11;
        n = 4;
        tdata = new double[]{4.0, 2.0, 1.0, 0.5, 0.25, 0.167, 0.125, 0.1, 0.0833, 0.0714, 0.0625};
        ydata = new double[]{0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 
        		               0.0323, 0.0235, 0.0246};
        initial_x = new double[n];
        initial_x[0] = 2.5;
        initial_x[1] = 3.9;
        initial_x[2] = 4.15;
        initial_x[3] = 3.9;
        lower = new double[n];
        upper = new double[n];
        lower[0] = 0;
        upper[0] = 10.0;
        lower[1] = 0;
        upper[1] = 10.0;
        lower[2] = 0;
        upper[2] = 10.0;
        lower[3] = 0;
        upper[3] = 10.0;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // KOWALIK_AND_OSBORNE at 100 * standard starting point constrained converges to incorrect values
        // Number of iterations: 22
        // x[0] = 0.08664898048513225
        // x[1] = 0.0156044353981859
        // x[2] = 0.007249585255261204
        // x[3] = 0.0
        // residual = 0.05689377891051171
        // converged = true
        Preferences.debug("Kowalik and Osborne function 100 * standard starting point constrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.1928, a1 = 0.1913, a2 = 0.1231, a3 = 0.1361\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 3.07505E-4\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = KOWALIK_AND_OSBORNE;
        m = 11;
        n = 4;
        tdata = new double[]{4.0, 2.0, 1.0, 0.5, 0.25, 0.167, 0.125, 0.1, 0.0833, 0.0714, 0.0625};
        ydata = new double[]{0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 
        		               0.0323, 0.0235, 0.0246};
        initial_x = new double[n];
        initial_x[0] = 25.0;
        initial_x[1] = 39.0;
        initial_x[2] = 41.5;
        initial_x[3] = 39.0;
        lower = new double[n];
        upper = new double[n];
        lower[0] = 0;
        upper[0] = 100.0;
        lower[1] = 0;
        upper[1] = 100.0;
        lower[2] = 0;
        upper[2] = 100.0;
        lower[3] = 0;
        upper[3] = 100.0;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(0) = 10.0*(a1 - a0**2)
        //                            y(1) = 1.0 - a[0]
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // ROSENBROCK unconstrained at standard starting point converges to correct values in 42 iterations
        Preferences.debug("Rosenbrock function standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = 10*(a1 - a0**2)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = 1.0 - a0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 1\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = ROSENBROCK;
        m = 2;
        n = 2;
        initial_x = new double[n];
        initial_x[0] = -1.2;
        initial_x[1] = 1.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(0) = 10.0*(a1 - a0**2)
        //                            y(1) = 1.0 - a[0]
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // ROSENBROCK unconstrained at 10 * standard starting point converges to correct values in 115 iterations
        Preferences.debug("Rosenbrock function at 10 * standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = 10*(a1 - a0**2)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = 1.0 - a0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 1\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = ROSENBROCK;
        m = 2;
        n = 2;
        initial_x = new double[n];
        initial_x[0] = -12.0;
        initial_x[1] = 10.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(0) = 10.0*(a1 - a0**2)
        //                            y(1) = 1.0 - a[0]
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // ROSENBROCK unconstrained at 100 * standard starting point converges to correct values in 734 iterations
        Preferences.debug("Rosenbrock function at 100 * standard starting point unconstrained\n",
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = 10*(a1 - a0**2)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = 1.0 - a0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 1\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = ROSENBROCK;
        m = 2;
        n = 2;
        initial_x = new double[n];
        initial_x[0] = -120.0;
        initial_x[1] = 100.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(0) = -13 + a0 + ((5 - a1)*a1 - 2)*a1
        //                            y(1) = -29 + a0 + ((a1 + 1)*a1 - 14)*a1
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Second solution obtained in 26 iterations
        Preferences.debug("Freudenstein and Roth function at standard starting point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = -13 + a0 + ((5 - a1)*a1 - 2)*a1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = -29 + a0 + ((a1 + 1)*a1 - 14)*a1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is chi-squared = 0 at a0 = 5, a1 = 4\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Also Chi-squared = 48.9842... at a0 = 11.41..., a1 = -0.8968...\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Second solution obtained by original ELSUNC\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = FREUDENSTEIN_AND_ROTH;
        m = 2;
        n = 2;
        initial_x = new double[n];
        initial_x[0] = 0.5;
        initial_x[1] = -2.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(0) = -13 + a0 + ((5 - a1)*a1 - 2)*a1
        //                            y(1) = -29 + a0 + ((a1 + 1)*a1 - 14)*a1
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Second solution obtained in 37 iterations
        Preferences.debug("Freudenstein and Roth function at 10 * standard starting point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = -13 + a0 + ((5 - a1)*a1 - 2)*a1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = -29 + a0 + ((a1 + 1)*a1 - 14)*a1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("One correct answer is chi-squared = 0 at a0 = 5, a1 = 4\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Also Chi-squared = 48.9842... at a0 = 11.41..., a1 = -0.8968...\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Second solution obtained by original ELSUNC\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = FREUDENSTEIN_AND_ROTH;
        m = 2;
        n = 2;
        initial_x = new double[n];
        initial_x[0] = 5.0;
        initial_x[1] = -20.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(0) = -13 + a0 + ((5 - a1)*a1 - 2)*a1
        //                            y(1) = -29 + a0 + ((a1 + 1)*a1 - 14)*a1
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Second solution obtained in 49 iterations
        Preferences.debug("Freudenstein and Roth function at 100 * standard starting point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = -13 + a0 + ((5 - a1)*a1 - 2)*a1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = -29 + a0 + ((a1 + 1)*a1 - 14)*a1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("One correct answer is Chi-squared = 0 at a0 = 5, a1 = 4\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Also Chi-squared = 48.9842... at a0 = 11.41..., a1 = -0.8968...\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Second solution obtained by original ELSUNC\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = FREUDENSTEIN_AND_ROTH;
        m = 2;
        n = 2;
        initial_x = new double[n];
        initial_x[0] = 50.0;
        initial_x[1] = -200.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // HELICAL_VALLEY at standard starting point unconstrained converges to correct values in 11 iterations
        Preferences.debug("Helical valley function at standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = 10*[a2 - 10*theta(a0,a1)]\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = 10*{Math.sqrt(a0*a0 + a1*a1) - 1]\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(2) = a2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("theta = (1/(2*PI))*arctan(a1/a0) if a0 > 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("theta = (1/(2*PI))*arctan(a1/a0) + 0.5 if a0 < 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("theta = 0.25 if a0 = 0 and a1 >= 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("theta = -0.25 if a0 = 0 and a1 < 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 0, a2 = 0\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = HELICAL_VALLEY;
        m = 3;
        n = 3;
        initial_x = new double[n];
        initial_x[0] = -1.0;
        initial_x[1] = 0.0;
        initial_x[2] = 0.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // HELICAL_VALLEY at 10 *standard starting point unconstrained converges to correct values in 16 iterations
        Preferences.debug("Helical valley function at 10 * standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = 10*[a2 - 10*theta(a0,a1)]\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = 10*{Math.sqrt(a0*a0 + a1*a1) - 1]\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(2) = a2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("theta = (1/(2*PI))*arctan(a1/a0) if a0 > 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("theta = (1/(2*PI))*arctan(a1/a0) + 0.5 if a0 < 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("theta = 0.25 if a0 = 0 and a1 >= 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("theta = -0.25 if a0 = 0 and a1 < 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 0, a2 = 0\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = HELICAL_VALLEY;
        m = 3;
        n = 3;
        initial_x = new double[n];
        initial_x[0] = -10.0;
        initial_x[1] = 0.0;
        initial_x[2] = 0.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // HELICAL_VALLEY at 100 *standard starting point unconstrained converges to correct values in 68 iterations
        Preferences.debug("Helical valley function at 100 * standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = 10*[a2 - 10*theta(a0,a1)]\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = 10*{Math.sqrt(a0*a0 + a1*a1) - 1]\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(2) = a2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("theta = (1/(2*PI))*arctan(a1/a0) if a0 > 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("theta = (1/(2*PI))*arctan(a1/a0) + 0.5 if a0 < 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("theta = 0.25 if a0 = 0 and a1 >= 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("theta = -0.25 if a0 = 0 and a1 < 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 0, a2 = 0\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = HELICAL_VALLEY;
        m = 3;
        n = 3;
        initial_x = new double[n];
        initial_x[0] = -100.0;
        initial_x[1] = 0.0;
        initial_x[2] = 0.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(i-1) = 2 + 2*i -(exp(i*a0) + exp(i*a1))
        // for i = 1 to 10
        // JENNRICH_AND_SAMPSON converges to the correct values in 21 iterations
        Preferences.debug("Jennrich and Sampson function at standard starting point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(i-1) = 2 + 2*i - (exp(i*a0) + exp(i*a1)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("for i = 1 to 10\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has chi-squared = 124.362 at a0 = 0.257825, a1 = 0.257825\n", 
        		Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = JENNRICH_AND_SAMPSON;
        m = 10;
        n = 2;
        initial_x = new double[n];
        initial_x[0] = 0.3;
        initial_x[1] = 0.4;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y = a0*exp[a1/(x + a2)]
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // MEYER unconstrained at standard starting point converges to the correct values in 904 iterations
        Preferences.debug("Meyer function at standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0*exp[a1/(x + a2)]\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answers are a0 = 0.0056096, a1 = 6181.3, a2 = 345.22\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 87.9458\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = MEYER;
        m = 16;
        n = 3;
        tdata = new double[]{50.0, 55.0, 60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0,
        		               100.0, 105.0, 110.0, 115.0, 120.0, 125.0};
        ydata = new double[]{34780.0, 28610.0, 23650.0, 19630.0, 16370.0, 13720.0, 11540.0,
        		               9744.0, 8261.0, 7030.0, 6005.0, 5147.0, 4427.0, 3820.0,
        		               3307.0, 2872.0};
        initial_x = new double[n];
        initial_x[0] = 0.02;
        initial_x[1] = 4000.0;
        initial_x[2] = 250.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y = a0*exp[a1/(x + a2)]
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // MEYER at 10 * standard starting point constrained exits when finding matrix JJ singular when trying to invert
        // Changing initial_lambda to 1.0 causes convergence to incorrect values:
        // Number of iterations: 25
        // x[0] = 1.0
        // x[1] = 100.0
        // x[2] = 100.0
        // residual = 3.890044452557155E9
        // converged = true
        // Changing initial_lambda to 1.0E-9 results in a correct solution:
        // Number of iterations: 239
        // x[0] = 0.005609636383286036
        // x[1] = 6181.346359304068
        // x[2] = 345.2236350621298
        // residual = 87.94585517086705
        // converged = true
        Preferences.debug("Meyer function 10 * standard starting point constrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Y = a0*exp[a1/(x + a2)]\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answers are a0 = 0.0056096, a1 = 6181.3, a2 = 345.22\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 87.9458\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = MEYER;
        m = 16;
        n = 3;
        tdata = new double[]{50.0, 55.0, 60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0,
        		               100.0, 105.0, 110.0, 115.0, 120.0, 125.0};
        ydata = new double[]{34780.0, 28610.0, 23650.0, 19630.0, 16370.0, 13720.0, 11540.0,
        		               9744.0, 8261.0, 7030.0, 6005.0, 5147.0, 4427.0, 3820.0,
        		               3307.0, 2872.0};
        initial_x = new double[n];
        initial_x[0] = 0.2;
        initial_x[1] = 40000.0;
        initial_x[2] = 2500.0;
        lower = new double[n];
        upper = new double[n];
        lower[0] = 1.0E-3;
        upper[0] = 1.0;
        lower[1] = 100.0;
        upper[1] = 100000.0;
        lower[2] = 100.0;
        upper[2] = 3000.0;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an expample to fit y(i) = exp(-ti*a0) - exp(-ti*a1) - a2*(exp(-ti) - exp(-10*ti))
        // For ti = 0.1*i for i = 1 to 10
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // BOX_3D converges to incorrect values
        // Number of iterations: 10
        // x[0] = 0.613600069431878
        // x[1] = 2688.950358659462
        // x[2] = 1.3199553801984902
        // residual = 0.07558874075503125
        // converged = true
        Preferences.debug("Box three-dimensional function unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(i) = exp(-ti*a0) = exp(-ti*a1) - a2*(exp(-ti) - exp(-10*ti))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("For ti = 0.1*i for i = 1 to 10\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has chi-squared = 0 at a0 = 1, a1 = 10, a2 = 1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Also chi-squared = 0 at a0 = 10, a1 = 1, a2 = -1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Also chi-squared = 0 wherever a0 = a1 and a2 = 0\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BOX_3D;
        m = 10;
        n = 3;
        initial_x = new double[n];
        initial_x[0] = 0.0;
        initial_x[1] = 10.0;
        initial_x[2] = 20.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(0) = a0 + 10*a1
        //                            y(1) = sqrt(5)*(a2 - a3)
        //                            y(2) = (a1 - 2*a2)**2
        //                            y(3) = sqrt(10)*(a0 - a3)**2
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Answer taken as correct
        // Number of iterations: 18
        // x[0] = 3.815409897766419E-5
        // x[1] = -3.81540989776642E-6
        // x[2] = 1.6144508147981697E-5
        // x[3] = 1.6144508147981697E-5
        // residual = 4.045836992205677E-18
        // converged = true
        Preferences.debug("Powell singular function at standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = a0 + 10*a1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = sqrt(5)*(a2 - a3)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(2) = (a1 - 2*a2)**2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(3) = sqrt(10)*(a0 - a3)**2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has chi-squared = 0 at a0 = 0, a1= 0, a2 = 0, a3 = 0\n", 
        		Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = POWELL_SINGULAR;
        m = 4;
        n = 4;
        initial_x = new double[n];
        initial_x[0] = 3.0;
        initial_x[1] = -1.0;
        initial_x[2] = 0.0;
        initial_x[3] = 1.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(0) = a0 + 10*a1
        //                            y(1) = sqrt(5)*(a2 - a3)
        //                            y(2) = (a1 - 2*a2)**2
        //                            y(3) = sqrt(10)*(a0 - a3)**2
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Answer taken as correct
        // Number of iterations: 21
        // x[0] = 3.463949915124049E-5
        // x[1] = -3.4639499151240487E-6
        // x[2] = 9.675442795064508E-6
        // x[3] = 9.675442795064508E-6
        // residual = 4.15477127904256E-18
        // converged = true
        Preferences.debug("Powell singular function at 10 * standard starting point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = a0 + 10*a1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = sqrt(5)*(a2 - a3)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(2) = (a1 - 2*a2)**2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(3) = sqrt(10)*(a0 - a3)**2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has chi-squared = 0 at a0 = 0, a1= 0, a2 = 0, a3 = 0\n", 
        		Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = POWELL_SINGULAR;
        m = 4;
        n = 4;
        initial_x = new double[n];
        initial_x[0] = 30.0;
        initial_x[1] = -10.0;
        initial_x[2] = 0.0;
        initial_x[3] = 10.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(0) = a0 + 10*a1
        //                            y(1) = sqrt(5)*(a2 - a3)
        //                            y(2) = (a1 - 2*a2)**2
        //                            y(3) = sqrt(10)*(a0 - a3)**2
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Answer taken as correct
        // Number of iterations: 25
        // x[0] = 1.8914165002182845E-5
        // x[1] = -1.8914165002182841E-6
        // x[2] = 3.5292754953574563E-6
        // x[3] = 3.5292754953574563E-6
        // residual = 5.666607197489183E-19
        // converged = true
        Preferences.debug("Powell singular function at 100 * standard starting point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = a0 + 10*a1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = sqrt(5)*(a2 - a3)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(2) = (a1 - 2*a2)**2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(3) = sqrt(10)*(a0 - a3)**2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has chi-squared = 0 at a0 = 0, a1= 0, a2 = 0, a3 = 0\n",
        		Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = POWELL_SINGULAR;
        m = 4;
        n = 4;
        initial_x = new double[n];
        initial_x[0] = 300.0;
        initial_x[1] = -100.0;
        initial_x[2] = 0.0;
        initial_x[3] = 100.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(i) = (a0 + ti*a1 - exp(ti))**2 + (a2 +a3*sin(ti) - cos(ti))**2
        // ti = 0.2*i for i = 1 to 20
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // BROWN_AND_DENNIS at standard starting point unconstrained converges to correct values in 6259 iterations
        Preferences.debug("Brown and Dennis function at standard starting point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(i) = (a0 + a1*ti - exp(ti))**2 + (a2 + a3*sin(ti) - cos(ti))**2\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("For ti = 0.2*i for i = 1 to 20\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has a0 = -11.59, a1 = 13.20, a2 = -0.4034, a3 = 0.2367\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has chi-squared = 85822.2\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BROWN_AND_DENNIS;
        m = 20;
        n = 4;
        initial_x = new double[n];
        initial_x[0] = 25.0;
        initial_x[1] = 5.0;
        initial_x[2] = -5.0;
        initial_x[3] = -1.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(i) = (a0 + ti*a1 - exp(ti))**2 + (a2 +a3*sin(ti) - cos(ti))**2
        // ti = 0.2*i for i = 1 to 20
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // BROWN_AND_DENNIS at 10 * standard starting point unconstrained converges to correct values in 6667 iterations
        Preferences.debug("Brown and Dennis function at 10 * standard starting point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(i) = (a0 + a1*ti - exp(ti))**2 + (a2 + a3*sin(ti) - cos(ti))**2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("For ti = 0.2*i for i = 1 to 20\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has a0 = -11.59, a1 = 13.20, a2 = -0.4034, a3 = 0.2367\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has chi-squared = 85822.2\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BROWN_AND_DENNIS;
        m = 20;
        n = 4;
        initial_x = new double[n];
        initial_x[0] = 250.0;
        initial_x[1] = 50.0;
        initial_x[2] = -50.0;
        initial_x[3] = -10.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y(i) = (a0 + ti*a1 - exp(ti))**2 + (a2 +a3*sin(ti) - cos(ti))**2
        // ti = 0.2*i for i = 1 to 20
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // BROWN_AND_DENNIS at 100 * standard starting point unconstrained converges to correct values in 5212 iterations
        Preferences.debug("Brown and Dennis function at 100 * standard starting point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(i) = (a0 + a1*ti - exp(ti))**2 + (a2 + a3*sin(ti) - cos(ti))**2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("For ti = 0.2*i for i = 1 to 20\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has a0 = -11.59, a1 = 13.20, a2 = -0.4034, a3 = 0.2367\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has chi-squared = 85822.2\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BROWN_AND_DENNIS;
        m = 20;
        n = 4;
        initial_x = new double[n];
        initial_x[0] = 2500.0;
        initial_x[1] = 500.0;
        initial_x[2] = -500.0;
        initial_x[3] = -100.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y = a0 + a1*exp(-a3*x) + a2*exp(-a4*x)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // OSBORNE1 converges to correct values in 61 iterations
        Preferences.debug("Osborne 1 function unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0 + a1*exp(-a3*x) + a2*exp(-a4*x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.37541, a1 = 1.9358, a2 = -1.4647, a3 = 0.012868, a4 = 0.022123\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 5.46489E-5\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = OSBORNE1;
        m = 33;
        n = 5;
        tdata = new double[33];
        for (i = 1; i <= 33; i++) {
        	tdata[i-1] = 10.0*(i-1);
        }
        ydata = new double[]{0.844, 0.908, 0.932, 0.936, 0.925, 0.908, 0.881, 0.850, 0.818,
        		  0.784, 0.751, 0.718, 0.685, 0.658, 0.628, 0.603, 0.580, 0.558, 0.538, 0.522,
        		  0.506, 0.490, 0.478, 0.467, 0.457, 0.448, 0.438, 0.431, 0.424, 0.420, 0.414,
        		  0.411, 0.406};
        initial_x = new double[n];
        initial_x[0] = 0.5;
        initial_x[1] = 1.5;
        initial_x[2] = -1.0;
        initial_x[3] = 0.01;
        initial_x[4] = 0.02;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit y = a0*exp(-a4*x) + a1*exp(-a5*(x-a8)**2) 
        // + a2*exp(-a6*(x-a9)**2) + a3*exp(-a7*(x-a10)**2)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // OSBORNE2 converges to the correct values in 15 iterations
        Preferences.debug("Osborne 2 function unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0*exp(-a4*x) + a1*exp(-a5*(x-a8)**2) \n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("    + a2*exp(-a6*(x-a9)**2) + a3*exp(-a7*(x-a10)**2)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 4.01377E-2\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = OSBORNE2;
        m = 65;
        n = 11;
        tdata = new double[65];
        for (i = 1; i <= 65; i++) {
        	tdata[i-1] = (i-1)/10.0;
        }
        ydata = new double[]{1.366, 1.191, 1.112, 1.013, 0.991, 0.885, 0.831, 0.847, 0.786,
        		  0.725, 0.746, 0.679, 0.608, 0.655, 0.616, 0.606, 0.602, 0.626, 0.651, 0.724,
        		  0.649, 0.649, 0.694, 0.644, 0.624, 0.661, 0.612, 0.558, 0.533, 0.495, 0.500,
        		  0.423, 0.395, 0.375, 0.372, 0.391, 0.396, 0.405, 0.428, 0.429, 0.523, 0.562,
        		  0.607, 0.653, 0.672, 0.708, 0.633, 0.668, 0.645, 0.632, 0.591, 0.559, 0.597,
        		  0.625, 0.739, 0.710, 0.729, 0.720, 0.636, 0.581, 0.428, 0.292, 0.162, 0.098,
        		  0.054};
        initial_x = new double[n];
        initial_x[0] = 1.3;
        initial_x[1] = 0.65;
        initial_x[2] = 0.65;
        initial_x[3] = 0.7;
        initial_x[4] = 0.6;
        initial_x[5] = 3.0;
        initial_x[6] = 5.0;
        initial_x[7] = 7.0;
        initial_x[8] = 2.0;
        initial_x[9] = 4.5;
        initial_x[10] = 5.5;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Watson function with 6 parameters
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // WATSON with 6 parameters converges to the correct values in 13 iterations.
        Preferences.debug("Watson with 6 parameters at standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct chi-squared = 2.28767E-3\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = WATSON;
        m = 31;
        n = 6;
        // Guess all parameters are 0.0.
        initial_x = new double[n];
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Watson function with 9 parameters
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // WATSON with 9 parameters converges to the correct values in 15 iterations.
        Preferences.debug("Watson with 9 parameters at standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct chi-squared = 1.39976E-6\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = WATSON;
        m = 31;
        n = 9;
        // Guess all parameters are 0.0.
        initial_x = new double[n];
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Watson function with 12 parameters
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // WATSON with 12 parameters converges to the correct values in 18 iterations.
        Preferences.debug("Watson with 12 parameters at standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct chi-squared = 4.72238E-10\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = WATSON;
        m = 31;
        n = 12;
        // Guess all parameters are 0.0.
        initial_x = new double[n];
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Brown almost linear function with 10 parameters
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // BROWN_ALMOST_LINEAR with 10 parameters at standard starting point unconstrained converges to 10 1's with residual = 0 in 11 iterations
        Preferences.debug("Brown almost linear with 10 parameters at standard staring point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = 0 at (alpha, ..., alpha, alpha**(1 - n)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("where alpha satisfies n*alpha**n - (n+1)*alpha**(n-1) + 1 = 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("in particular , alpha = 1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = 1 at (0,...,0,n+1)\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BROWN_ALMOST_LINEAR;
        m = 10;
        n = 10;
        // Guess all parameters are 0.5
        initial_x = new double[n];
        for (i = 0; i < n; i++) {
        	initial_x[i] = 0.5;
        }
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Brown almost linear function with 10 parameters
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Correct answer with
        // Number of iterations: 25
        // x[0] = 0.9794303033491398
        // x[1] = 0.9794303033491398
        // x[2] = 0.9794303033491388
        // x[3] = 0.9794303033491389
        // x[4] = 0.9794303033491396
        // x[5] = 0.9794303033491399
        // x[6] = 0.9794303033491389
        // x[7] = 0.9794303033491399
        // x[8] = 0.9794303033491397
        // x[9] = 1.2056969665086188
        // residual = 4.0598903980903047E-25
        // converged = true
        Preferences.debug("Brown almost linear with 10 parameters at 10 * standard staring point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = 0 at (alpha, ..., alpha, alpha**(1 - n)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("where alpha satisfies n*alpha**n - (n+1)*alpha**(n-1) + 1 = 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("in particular , alpha = 1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = 1 at (0,...,0,n+1)\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BROWN_ALMOST_LINEAR;
        m = 10;
        n = 10;
        // Guess all parameters are 5.0
        initial_x = new double[n];
        for (i = 0; i < n; i++) {
        	initial_x[i] = 5.0;
        }
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Brown almost linear function with 10 parameters
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Correct answer with
        // Number of iterations: 39
        // x[0] = 0.9794303033498627
        // x[1] = 0.9794303033498621
        // x[2] = 0.9794303033498617
        // x[3] = 0.9794303033498627
        // x[4] = 0.979430303349863
        // x[5] = 0.9794303033498626
        // x[6] = 0.9794303033498623
        // x[7] = 0.9794303033498619
        // x[8] = 0.9794303033498621
        // x[9] = 1.2056969665013773
        // residual = 1.3065508742723008E-29
        // converged = true
        Preferences.debug("Brown almost linear with 10 parameters at 100 * standard staring point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = 0 at (alpha, ..., alpha, alpha**(1 - n)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("where alpha satisfies n*alpha**n - (n+1)*alpha**(n-1) + 1 = 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("in particular , alpha = 1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = 1 at (0,...,0,n+1)\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BROWN_ALMOST_LINEAR;
        m = 10;
        n = 10;
        // Guess all parameters are 50.0
        initial_x = new double[n];
        for (i = 0; i < n; i++) {
        	initial_x[i] = 50.0;
        }
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Brown almost linear function with 30 parameters
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Converges to 30 1's in 15 iterations with resdual =  5.725139530714027E-26.
        Preferences.debug("Brown almost linear with 30 parameters at standard staring point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = 0 at (alpha, ..., alpha, alpha**(1 - n)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("where alpha satisfies n*alpha**n - (n+1)*alpha**(n-1) + 1 = 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("in particular , alpha = 1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = 1 at (0,...,0,n+1)\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BROWN_ALMOST_LINEAR;
        m = 30;
        n = 30;
        // Guess all parameters are 0.5
        initial_x = new double[n];
        for (i = 0; i < n; i++) {
        	initial_x[i] = 0.5;
        }
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Brown almost linear function with 40 parameters
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Converges to 40 1's in 14 iterations with residual = 1.0197259795145985E-28.
        Preferences.debug("Brown almost linear with 40 parameters at standard staring point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = 0 at (alpha, ..., alpha, alpha**(1 - n)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("where alpha satisfies n*alpha**n - (n+1)*alpha**(n-1) + 1 = 0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("in particular , alpha = 1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = 1 at (0,...,0,n+1)\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BROWN_ALMOST_LINEAR;
        m = 40;
        n = 40;
        // Guess all parameters are 0.5
        initial_x = new double[n];
        for (i = 0; i < n; i++) {
        	initial_x[i] = 0.5;
        }
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Linear full rank with 5 parameters and 10 points
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Converges to correct values in 7 iterations
        Preferences.debug("Linear full rank function with 5 parameters and 10 points\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = nPts - param at all parameters = -1\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = LINEAR_FULL_RANK;
        m = 10;
        n = 5;
        // Guess all parameters are 1
        initial_x = new double[n];
        for (i = 0; i < n; i++) {
        	initial_x[i] = 1.0;
        }
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Linear full rank with 5 parameters and 50 points
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Converges to correct values in 7 iterations
        Preferences.debug("Linear full rank function with 5 parameters and 50 points\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = nPts - param at all parameters = -1\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = LINEAR_FULL_RANK;
        m = 50;
        n = 5;
        // Guess all parameters are 1
        initial_x = new double[n];
        for (i = 0; i < n; i++) {
        	initial_x[i] = 1.0;
        }
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is the test to fit the Linear rank 1 function with 5 parameters and 10 points
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Correctly converges to residual = 15/7 = 2.142857 in 6 iterations
        Preferences.debug("Linear rank 1 function with 5 parameters and 10 points\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = nPts*(nPts-1)/(2*(2*nPts + 1))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("At any point where sum from j = 1 to param of j*x[j] = 3/(2*nPts + 1)\n", 
        		Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = LINEAR_RANK1;
        m = 10;
        n = 5;
        // Guess all parameters are 1
        initial_x = new double[n];
        for (i = 0; i < n; i++) {
        	initial_x[i] = 1.0;
        }
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is the test to fit the Linear rank 1 function with 5 parameters and 50 points
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Correctly converges to residual = 12.1287 in 6 iterations
        Preferences.debug("Linear rank 1 function with 5 parameters and 50 points\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = nPts*(nPts-1)/(2*(2*nPts + 1))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("At any point where sum from j = 1 to param of j*x[j] = 3/(2*nPts + 1)\n", 
        		Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = LINEAR_RANK1;
        m = 50;
        n = 5;
        // Guess all parameters are 1
        initial_x = new double[n];
        for (i = 0; i < n; i++) {
        	initial_x[i] = 1.0;
        }
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is the test to fit the Linear rank 1 function with zero columns and rows with 5 parameters and 10 points
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Correctly converges to residual = 3.6470588 in 6 iterations.
        Preferences.debug("Linear rank 1 with zero columns and rows function with 5 parameters and 10 points\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = (nPts**2 + 3*nPts -6)/(2*(2*nPts - 3))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("At any point where sum from j = 2 to param-1 of j*x[j] = 3/(2*nPts - 3)\n", 
        		Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = LINEAR_RANK1_WITH_ZERO_COLUMNS_AND_ROWS;
        m = 10;
        n = 5;
        // Guess all parameters are 1
        initial_x = new double[n];
        for (i = 0; i < n; i++) {
        	initial_x[i] = 1.0;
        }
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is the test to fit the Linear rank 1 function with zero columns and rows with 5 parameters and 50 points
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Correctly converges to 13.628865979 in 6 iterations
        Preferences.debug("Linear rank 1 with zero columns and rows function with 5 parameters and 50 points\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared = (nPts**2 + 3*nPts -6)/(2*(2*nPts - 3))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("At any point where sum from j = 2 to param-1 of j*x[j] = 3/(2*nPts - 3)\n", 
        		Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = LINEAR_RANK1_WITH_ZERO_COLUMNS_AND_ROWS;
        m = 50;
        n = 5;
        // Guess all parameters are 1
        initial_x = new double[n];
        for (i = 0; i < n; i++) {
        	initial_x[i] = 1.0;
        }
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Chebyquad function with 1 parameter and 8 points
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Chebyquad function with 1 parameter and 8 points\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct chi-squared equals about 3.55 variable a0\n", Preferences.DEBUG_ALGORITHM);
        // initial_x[0] = 0.0 -> residual = 3.550391 x[0] = 0.01826873 in 14 iterations
        // initial_x[0] = 0.05 -> residual = 3.491482 x[0] = 0.1024086 in 30 iterations
        // initial_x[0] = 0.5 -> residual = 3.557893 x[0] = 0.5 in 1 iteration
        // initial_x[0] = 1.0 -> residual = 3.550391 x[0] = 0.9817312 in 13 iterations
        testMode = true;
        testCase = CHEBYQUAD;
        m = 8;
        n = 1;
        initial_x = new double[n];
        initial_x[0] = 1.0;
        lower = new double[n];
        upper = new double[n];
        lower[0] = 0.0;
        upper[0] = 1.0;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Chebyquad function with 8 parameters and 8 points
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // residual = 3.51687E-3 in 59 iterations
        Preferences.debug("Chebyquad function with 8 parameters and 8 points\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct chi-squared = 3.51687E-3\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = CHEBYQUAD;
        m = 8;
        n = 8;
        initial_x = new double[n];
        for (i = 1; i <= n; i++) {
            initial_x[i-1] = i/(n + 1.0);
        }
        lower = new double[n];
        upper = new double[n];
        for (i = 0; i < n; i++) {
            lower[i] = 0.0;
            upper[i] = 1.0;
        }
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Chebyquad function with 9 parameters and 9 points
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Chebyquad function with 9 parameters and 9 points\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct chi-squared = 0.0\n", Preferences.DEBUG_ALGORITHM);
        // Actual residual value was 1.7467E-24 in 14 iterations
        testMode = true;
        testCase = CHEBYQUAD;
        m = 9;
        n = 9;
        initial_x = new double[n];
        for (i = 1; i <= n; i++) {
            initial_x[i-1] = i/(n + 1.0);
        }
        lower = new double[n];
        upper = new double[n];
        for (i = 0; i < n; i++) {
            lower[i] = 0.0;
            upper[i] = 1.0;
        }
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Below is an example to fit the Chebyquad function with 10 parameters and 10 points
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Residual = 6.50395E-3 in 31 iterations
        Preferences.debug("Chebyquad function with 10 parameters and 10 points\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct chi-squared = 6.50395E-3\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = CHEBYQUAD;
        m = 10;
        n = 10;
        initial_x = new double[n];
        for (i = 1; i <= n; i++) {
            initial_x[i-1] = i/(n + 1.0);
        }
        lower = new double[n];
        upper = new double[n];
        for (i = 0; i < n; i++) {
            lower[i] = 0.0;
            upper[i] = 1.0;
        }
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Converges to incorrect values
        // Number of iterations: 3434
        // x[0] = -1.0775294083763476
        // x[1] = 1.16106962906314
        // residual = 37.25792946722536
        // converged = true
        Preferences.debug("Rosenbrock function used as LEVMAR example standard starting point unconstrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = ((1.0 - a0)*(1.0 - a0) + 105.0*(a1 - a0*a0)*(a1 - a0*a0));\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(1) = y(0)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 1\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = LEVMAR_ROSENBROCK;
        m = 2;
        n = 2;
        initial_x = new double[n];
        initial_x[0] = -1.2;
        initial_x[1] = 1.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Does not converge to an answer
        // Number of iterations: 100000
        // x[0] = 1.8887516473664823
        // x[1] = 2.0337494109576155E-4
        // residual = 93.76366048170557
        // converged = false
        Preferences.debug("Powell's 2 parameter function\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = a0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(0) = 10.0*a0/(a0 + 0.1) + 2*a1*a1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer a0 = 0 a1 = 0\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = POWELL_2_PARAMETER;
        m = 2;
        n = 2;
        initial_x = new double[n];
        initial_x[0] = 3.0;
        initial_x[1] = 1.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Converges to correct values with residual = 10000 in 47 iterations
        Preferences.debug("Modified Rosenbrock function unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has param[0] = 1.0 param[1] = 1.0\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = MODIFIED_ROSENBROCK;
        m = 3;
        n = 2;
        initial_x = new double[n];
        initial_x[0] = -1.2;
        initial_x[1] = 1.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Converges to correct values in 80 iterations
        Preferences.debug("Wood's function\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("Correct answer has chi-squared = 0 at a0 = a1 = a2 = a3 = 1\n", Preferences.DEBUG_ALGORITHM);
    	testMode = true;
    	testCase = WOOD;
        m = 6;
        n = 4;
        initial_x = new double[n];
        initial_x[0] = -3.0;
        initial_x[1] = -1.0;
        initial_x[2] = -3.0;
        initial_x[3] = -1.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
         
        // Converges to correct values in 41 iterations
        Preferences.debug("Hock - Schittkowski problem #1\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("Correct answer has a0 = a1 = 1\n", Preferences.DEBUG_ALGORITHM);
    	testMode = true;
    	testCase = HOCK1;
    	m = 2;
    	n = 2;
    	initial_x = new double[n];
    	initial_x[0] = -2.0;
    	initial_x[1] = 1.0;
        lower = new double[n];
        upper = new double[n];
        lower[0] = -Double.MAX_VALUE;
        lower[1] = -1.5;
        upper[0] = Double.MAX_VALUE;
        upper[1] = Double.MAX_VALUE;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Take answer as correct
        // Number of iterations: 7
        // x[0] = 2.0
        // x[1] = -4.0868223095102246E-11
        // residual = 0.04000000000000001
        // converged = true
        Preferences.debug("Hock - Schittkowski problem #21 modified\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has a0 = 2.0 a1 = 0.0\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = HOCK21_MODIFIED;
        m = 2;
        n = 2;
        initial_x = new double[n];
        //initial_x[0] = -1.0;
        // Cannot have initial_x[0] < lower[0] so must modify initial_x[0] from original specifications
        initial_x[0] = 5.0;
        initial_x[1] = -1.0;
        lower = new double[n];
        upper = new double[n];
        lower[0] = 2.0;
        lower[1] = -50.0;
        upper[0] = 50.0;
        upper[1] = 50.0;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Converges to incorrect values
        // Number of iterations: 15
        // x[0] = 0.9999999999556595
        // x[1] = 0.8
        // x[2] = 0.9778329884865306
        // x[3] = 0.9561573533724991
        // residual = 0.04681159553142556
        // converged = true
        Preferences.debug("hatfldb problem\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has a0 = 0.947214 a1 = 0.8 a2 = 0.64 a3 = 0.4096\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = HATFLDB;
        m = 4;
        n = 4;
        initial_x = new double[n];
        initial_x[0] = 0.1;
        initial_x[1] = 0.1;
        initial_x[2] = 0.1;
        initial_x[3] = 0.1;
        lower = new double[n];
        upper = new double[n];
        lower[0] = 0.0;
        lower[1] = 0.0;
        lower[2] = 0.0;
        lower[3] = 0.0;
        upper[0] = Double.MAX_VALUE;
        upper[1] = 0.8;
        upper[2] = Double.MAX_VALUE;
        upper[3] = Double.MAX_VALUE;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Converges to correct values in 8 iterations
        Preferences.debug("hatfldc problem\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has a0 = 1.0 a1 = 1.0 a2 = 1.0 a3 = 1.0\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = HATFLDC;
        m = 4;
        n = 4;
        initial_x = new double[n];
        initial_x[0] = 0.9;
        initial_x[1] = 0.9;
        initial_x[2] = 0.9;
        initial_x[3] = 0.9;
        lower = new double[n];
        upper = new double[n];
        lower[0] = 0.0;
        lower[1] = 0.0;
        lower[2] = 0.0;
        lower[3] = 0.0;
        upper[0] = 10.0;
        upper[1] = 10.0;
        upper[2] = 10.0;
        upper[3] = 10.0;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        // Converges to correct values in 285 iterations
        Preferences.debug("Equilibrium combustion problem\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has a0 = 0.0034 a1 = 31.3265 a2 = 0.0684 a3 = 0.8595 a4 = 0.0370\n", 
        		Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = EQUILIBRIUM_COMBUSTION;
        m = 5;
        n = 5;
        initial_x = new double[n];
        initial_x[0] = 1.0E-4;
        initial_x[1] = 1.0E-4;
        initial_x[2] = 1.0E-4;
        initial_x[3] = 1.0E-4;
        initial_x[4] = 1.0E-4;
        lower = new double[n];
        upper = new double[n];
        lower[0] = 1.0E-4;
        lower[1] = 1.0E-4;
        lower[2] = 1.0E-4;
        lower[3] = 1.0E-4;
        lower[4] = 1.0E-4;
        upper[0] = 100.0;
        upper[1] = 100.0;
        upper[2] = 100.0;
        upper[3] = 100.0;
        upper[4] = 100.0;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        Preferences.debug("Beale problem\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is chi-squared = 0 at a0 = 3 a1 = 0.5\n", Preferences.DEBUG_ALGORITHM);
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Converges to correct answer in 8 iterations
        testMode = true;
        testCase = BEALE;
        m = 3;
        n = 2;
        ydata = new double[] {1.5, 2.25, 2.625};
        initial_x = new double[n];
        initial_x[0] = 1.0;
        initial_x[1] = 1.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        Preferences.debug("Powell badly scaled problem\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1.098...E-5 a1 = 9.106...\n", Preferences.DEBUG_ALGORITHM);
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Converges to correct answer in 294 iterations
        testMode = true;
        testCase = POWELL_BADLY_SCALED;
        m = 2;
        n = 2;
        initial_x= new double[n];
        initial_x[0] = 0;
        initial_x[1] = 1;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        Preferences.debug("Brown badly scaled problem\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1.0E6 a1 = 2.0E-6\n", Preferences.DEBUG_ALGORITHM);
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Converges to correct answer in 11 iterations
        testMode = true;
        testCase = BROWN_BADLY_SCALED;
        m = 3;
        n = 2;
        initial_x = new double[n];
        initial_x[0] = 1.0;
        initial_x[1] = 1.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        Preferences.debug("Gaussian problem\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is chi-squared = 1.12793...E-8\n", Preferences.DEBUG_ALGORITHM);
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Converges to correct answer in 7 iterations
        testMode = true;
        testCase = GAUSSIAN;
        m = 15;
        n = 3;
        tdata = new double[15];
        for (i = 0; i < 15; i++) {
        	tdata[i] = (7.0 - i)/2.0;
        }
        ydata = new double[] {0.0009,0.0044,0.0175,0.0540,0.1295,0.2420,0.3521,0.3989,
        		0.3521,0.2420,0.1295,0.0540,0.0175,0.0044,0.0009};
        initial_x = new double[n];
        initial_x[0] = 0.4;
        initial_x[1] = 1.0;
        initial_x[2] = 0.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        Preferences.debug("Gulf research and development problem\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answser is chi-squared = 0 at a0 = 50 a1 = 25 a2 = 1.5\n", Preferences.DEBUG_ALGORITHM);
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Converges to the correct values in 1821 iterations
        testMode = true;
        testCase = GULF_RESEARCH_AND_DEVELOPMENT;
        m = 3;
        n = 3;
        tdata = new double[] {0.01,0.02,0.03};
        ydata = new double[m];
        double exp = 2.0/3.0;
        for (i = 0; i < m; i++) {
            ydata[i] = 25.0 + Math.pow((-50.0*Math.log(tdata[i])),exp);	
        }
        initial_x = new double[n];
        initial_x[0] = 5.0;
        initial_x[1] = 2.5;
        initial_x[2] = 0.15;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        Preferences.debug("Biggs EXP6 problem\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has 2 minima with chi-squared = 0 and  chi-squared = 5.65565...E-3\n", Preferences.DEBUG_ALGORITHM);
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Converges to chi-squared = 0 solution in 70 iterations
        testMode = true;
        testCase = BIGGS_EXP6;
        m = 13;
        n = 6;
        tdata = new double[m];
        ydata = new double[m];
        for (i = 0; i < m; i++) {
        	tdata[i] = 0.1*(i+1.0);
        	ydata[i] = Math.exp(-tdata[i]) - 5.0 * Math.exp(-10.0 * tdata[i]) + 3.0 * Math.exp(-4.0 * tdata[i]);
        }
        initial_x = new double[n];
        initial_x[0] = 1.0;
        initial_x[1] = 2.0;
        initial_x[2] = 1.0;
        initial_x[3] = 1.0;
        initial_x[4] = 1.0;
        initial_x[5] = 1.0;
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        Preferences.debug("Penalty function I with n = 4\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has chi-squared = 2.24997...E-5\n", Preferences.DEBUG_ALGORITHM);
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Penalty function I does not converge:
        // Number of iterations: 100000
        // x[0] = 0.001536280090449339
        // x[1] = 0.9943055509283468
        // x[2] = 2.4148771867824728
        // x[3] = 3.5744519759012867
        // residual = 374.3059110827778
        // converged = false 
        // Works with ELSUNC port
        testMode = true;
        testCase = PENALTY_FUNCTION_I;
        m = 5;
        n = 4;
        initial_x = new double[n];
        for (i = 0; i < n; i++) {
        	initial_x[i] = i+1;
        }
        lower = null;
        upper = null;
        driver();
        dumpTestResults();
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    }
    
    private void dumpTestResults() {
    	 int i;
    	 Preferences.debug("Number of iterations: " + String.valueOf(iterCt) + "\n", Preferences.DEBUG_ALGORITHM);
         for (i = 0; i < n; i++) {
             Preferences.debug("x[" + i + "] = " + String.valueOf(x[i]) + "\n", Preferences.DEBUG_ALGORITHM);
         }
         // residual is chiSquared
         double residual = 0.0;
         for (i = 0; i < m; i++) {
         	residual += (residuals[i]*residuals[i]);
         }
         Preferences.debug("residual = " + residual + "\n", Preferences.DEBUG_ALGORITHM);
         Preferences.debug("converged = " + converged + "\n", Preferences.DEBUG_ALGORITHM);
    }
    
    public void fitToTestFunction(double[] x, double[] residuals) {
        int i;
        double ymodel = 0.0;
        double t[];
        double sumParam;
        double sumTerm;

        try {
            switch (testCase) {
            case DRAPER24D:

                // evaluate the residuals[i] = ymodel[i] - ydata[i]
                for (i = 0; i < m; i++) {
                    ymodel = x[0] - (x[1] * Math.pow(x[2], tdata[i]));
                    residuals[i] = ymodel - ydata[i];
                }
                break;
            case HOCK25:
            	// evaluate the residuals[i] = ymodel[i] - ydata[i]
            	for (i = 0; i < m; i++) {
                    ymodel = Math.pow((x[0] * Math.log(tdata[i])),x[1]) + x[2];
                    residuals[i] = ymodel - ydata[i];
                }
            	break;
            case BARD:
            	// evaluate the residuals[i] = ymodel[i] - ydata[i]
                for (i = 0; i < m; i++) {
                    ymodel = x[0] + tdata[i]/(x[1]*(16.0 - tdata[i]) 
                    		 + x[2]*Math.min(tdata[i], 16.0 - tdata[i]));
                    residuals[i] = ymodel - ydata[i];
                }
                break;
            case KOWALIK_AND_OSBORNE:
            	// evaluate the residuals[i] = ymodel[i] - ydata[i]
                for (i = 0; i < m; i++) {
                    ymodel = x[0]*(tdata[i]*tdata[i] + x[1]*tdata[i])/
                            (tdata[i]*tdata[i] + x[2]*tdata[i] + x[3]);
                    residuals[i] = ymodel - ydata[i];
                }
            	break;
            case ROSENBROCK:
            	residuals[0] = 10.0*(x[1] - x[0]*x[0]);
        	    residuals[1] = 1.0 - x[0];
            	break;
            case FREUDENSTEIN_AND_ROTH:
            	residuals[0] = -13.0 + x[0] + ((5.0 - x[1])*x[1] - 2.0)*x[1];
        		residuals[1] = -29.0 + x[0] + ((x[1] + 1)*x[1] - 14.0)*x[1];
            	break;
            case HELICAL_VALLEY:
            	double theta;
        		if (x[0] > 0) {
        	        theta = Math.atan(x[1]/x[0])/(2.0*Math.PI);
        		}
        		else if (x[0] < 0) {
        	    	theta = Math.atan(x[1]/x[0])/(2.0*Math.PI) + 0.5;
        	    }
        	    else if (x[1] >= 0) {
        	    	theta = 0.25;
        	    }
        	    else {
        	    	theta = -0.25;
        	    }
        	    residuals[0] = 10.0*(x[2] - 10.0*theta);
        	    residuals[1] = 10.0*(Math.sqrt(x[0]*x[0] + x[1]*x[1]) - 1.0);
        	    residuals[2] = x[2];
            	break;
            case JENNRICH_AND_SAMPSON:
            	for (i = 0; i < 10; i++) {
       	    	 residuals[i] = 2.0 + 2.0*(i+1.0) - (Math.exp((i+1.0)*x[0]) + Math.exp((i+1.0)*x[1]));
       	        }
            	break;
            case MEYER:
            	// evaluate the residuals[i] = ymodel[i] - ydata[i]
                for (i = 0; i < m; i++) {
                    ymodel = x[0]*Math.exp(x[1]/(tdata[i] + x[2]));
                    residuals[i] = ymodel - ydata[i];
                }
            	break;
            case BOX_3D:
            	t = new double[10];
        		for (i = 0; i <10; i++) {
        		    t[i] = 0.1*(i+1.0);	
        		    residuals[i] = Math.exp(-x[0]*t[i]) - Math.exp(-x[1]*t[i])
        		                   - x[2]*(Math.exp(-t[i]) - Math.exp(-10.0*t[i]));
        		}	
            	break;
            case POWELL_SINGULAR:
            	residuals[0] = x[0] + 10.0*x[1];
        	    residuals[1] = Math.sqrt(5.0)*(x[2] - x[3]);
        	    residuals[2] = (x[1] - 2.0*x[2])*(x[1] - 2.0*x[2]);
        	    residuals[3] = Math.sqrt(10.0)*(x[0] - x[3])*(x[0] - x[3]);
            	break;
            case BROWN_AND_DENNIS:
            	t = new double[20];
        		double part1;
        		double part2;
        		for (i = 0; i <20; i++) {
        		    t[i] = 0.2*(i+1.0);
        		    part1 = x[0] + x[1]*t[i] - Math.exp(t[i]);
        		    part2 = x[2] + x[3]*Math.sin(t[i]) - Math.cos(t[i]);
        		    residuals[i] = part1*part1 + part2*part2;
        		}
            	break;
            case OSBORNE1:
            	// evaluate the residuals[i] = ymodel[i] - ydata[i]
                for (i = 0; i < m; i++) {
                    ymodel = x[0] + x[1]*Math.exp(-x[3]*tdata[i]) + x[2]*Math.exp(-x[4]*tdata[i]);
                    residuals[i] = ymodel - ydata[i];
                }
            	break;
            case OSBORNE2:
            	// evaluate the residuals[i] = ymodel[i] - ydata[i]
                for (i = 0; i < m; i++) {
                    ymodel = x[0]*Math.exp(-x[4]*tdata[i]) 
                           + x[1]*Math.exp(-x[5]*(tdata[i] - x[8])*(tdata[i] - x[8]))
                           + x[2]*Math.exp(-x[6]*(tdata[i] - x[9])*(tdata[i] - x[9]))
                           + x[3]*Math.exp(-x[7]*(tdata[i] - x[10])*(tdata[i] - x[10]));
                    residuals[i] = ymodel - ydata[i];
                }
            	break;
            case WATSON:
            	double sum1;
        		double sum2;
        		t = new double[29];
        		int j;
        	    for (i = 0; i < 31; i++) {
        	    	if (i < 29) {
        	            t[i] = (i+1.0)/29.0;
            	        sum1 = 0.0;
            	        for (j = 2; j <= n; j++) {
            	            sum1 += (j - 1.0)*x[j-1]*Math.pow(t[i], j-2.0);	
            	        }
            	        sum2 = 0.0;
            	        for (j = 1; j <= n; j++) {
            	            sum2 += x[j-1]*Math.pow(t[i], j-1.0);	
            	        }
        	        	residuals[i] = sum1 - sum2*sum2 - 1.0;
        	        }
        	        else if (i == 29) {
        	        	residuals[i] = x[0];
        	        }
        	        else if (i == 30) {
        	        	residuals[i] = x[1] - x[0]*x[0] - 1.0; 
        	        }
        	    }
            	break;
            case BROWN_ALMOST_LINEAR:
            	sumParam = 0.0;
    			double prodParam = 1.0;
    			for (i = 0; i < m; i++) {
    				sumParam += x[i];
    				prodParam *= x[i];
    			}
    		    for (i = 0; i < m -1; i++) {
    		    	residuals[i] = x[i] + sumParam - (m + 1.0);
    		    } // for (i = 0; i < m - 1; i++)
    		    residuals[m-1] = prodParam - 1.0;	
            	break;
            case LINEAR_FULL_RANK:
            	sumParam = 0.0;
        	    for (i = 0 ; i < n; i++) {
        	    	sumParam += x[i];
        	    }
        	    for (i = 0; i < n; i++) {
        	    	residuals[i] = x[i] - 2.0 * sumParam / m - 1.0;
        	    }
        	    for (i = n; i < m; i++) {
        	    	residuals[i] = -2.0 * sumParam / m - 1.0;
        	    }
            	break;
            case LINEAR_RANK1:
            	sumTerm = 0.0;
        	    for (i = 0; i < n; i++) {
        	    	sumTerm += (i+1.0)*x[i];
        	    }
        	    for (i = 0; i < m; i++) {
        	    	residuals[i] = (i+1.0)*sumTerm - 1.0;
        	    }
        	    break;
            case LINEAR_RANK1_WITH_ZERO_COLUMNS_AND_ROWS:
        	    sumTerm = 0.0;
        	    residuals[0] = -1.0;
        	    residuals[m-1] = -1.0;
        	    for (i = 1; i < n-1; i++) {
        	    	sumTerm += (i+1.0)*x[i];
        	    }
        	    for (i = 1; i < m-1; i++) {
        	    	residuals[i] = i*sumTerm - 1.0;
        	    }
        	    break;
            case CHEBYQUAD:
            	double chebySum;
        	    for (i = 1; i <= m; i++) {
        	        chebySum = 0.0;
        	        for (j = 0; j < n; j++) {
        	        	chebySum += shiftedChebyshev(x[j],i);
        	        }
        	        if ((i % 2) == 1) {
        	        	residuals[i-1] = chebySum/n;
        	        }
        	        else {
        	        	residuals[i-1] = chebySum/n + 1.0/(i*i - 1.0);
        	        }
        	    }
            	break;
            case LEVMAR_ROSENBROCK:
            	residuals[0] = ((1.0 - x[0])*(1.0 - x[0]) 
		    			+ 105.0*(x[1]- x[0]*x[0])*(x[1] - x[0]*x[0]));
        		residuals[1] = residuals[0];
            	break;
            case POWELL_2_PARAMETER:
            	residuals[0] = x[0];
		    	residuals[1] = 10.0*x[0]/(x[0] + 0.1) + 2.0*x[1]*x[1];	
            	break;
            case MODIFIED_ROSENBROCK:
            	residuals[0] = 10.0*(x[1] - x[0]*x[0]);
		    	residuals[1] = 1.0 - x[0];
		    	residuals[2] = 100.0;
            	break;
            case WOOD:
            	 residuals[0] = 10.0*(x[1] - x[0]*x[0]);
                 residuals[1] = 1.0 - x[0];
                 residuals[2] = Math.sqrt(90.0)*(x[3] - x[2]*x[2]);
                 residuals[3] = 1.0 - x[2];
                 residuals[4] = Math.sqrt(10.0)*(x[1] + x[3] - 2.0);
                 residuals[5] = (x[1] - x[3])/Math.sqrt(10.0);
            	 break;
            case HOCK1:
            	residuals[0] = 10.0*(x[1] - x[0]*x[0]);
        	    residuals[1] = 1.0 - x[0];
            	break;
            case HOCK21_MODIFIED:
            	residuals[0] = x[0]/10.0;
        	    residuals[1] = x[1];
            	break;
            case HATFLDB:
            	residuals[0] = x[0] - 1.0;
        		for (i = 1; i < n; i++) {
        			residuals[i] = x[i-1] - Math.sqrt(x[i]);
        		}
            	break;
            case HATFLDC:
            	residuals[0] = x[0] - 1.0;
        		for (i = 1; i < n-1; i++) {
        			residuals[i] = x[i-1] - Math.sqrt(x[i]);
        		}
        		residuals[n-1] = x[n-1] - 1.0;
            	break;
            case EQUILIBRIUM_COMBUSTION:
            	double R, R5, R6, R7, R8, R9, R10;

      		  R=10;
      		  R5=0.193;
      		  R6=4.10622*1e-4;
      		  R7=5.45177*1e-4;
      		  R8=4.4975*1e-7;
      		  R9=3.40735*1e-5;
      		  R10=9.615*1e-7;

      		  residuals[0]=x[0]*x[1]+x[0]-3*x[4];
      		  residuals[1]=2*x[0]*x[1]+x[0]+3*R10*x[1]*x[1]+x[1]*x[2]*x[2]+R7*x[1]*x[2]+R9*x[1]*x[3]+R8*x[1]-R*x[4];
      		  residuals[2]=2*x[1]*x[2]*x[2]+R7*x[1]*x[2]+2*R5*x[2]*x[2]+R6*x[2]-8*x[4];
      		  residuals[3]=R9*x[1]*x[3]+2*x[3]*x[3]-4*R*x[4];
      		  residuals[4]=x[0]*x[1]+x[0]+R10*x[1]*x[1]+x[1]*x[2]*x[2]+R7*x[1]*x[2]
      		              +R9*x[1]*x[3]+R8*x[1]+R5*x[2]*x[2]+R6*x[2]+x[3]*x[3]-1.0;	
              break;
            case BEALE:
            	residuals[0] = x[0] - x[0]*x[1] - ydata[0];
            	residuals[1] = x[0] - x[0]*x[1]*x[1] - ydata[1];
            	residuals[2] = x[0] - x[0]*x[1]*x[1]*x[1] - ydata[2];
            	break;
            case POWELL_BADLY_SCALED:
            	residuals[0] = 1.0E4*x[0]*x[1] - 1.0;
            	residuals[1] = Math.exp(-x[0]) + Math.exp(-x[1]) - 1.0001; 
            	break;
            case BROWN_BADLY_SCALED:
            	residuals[0] = x[0] - 1.0E6;
            	residuals[1] = x[1] - 2.0E-6;
            	residuals[2] = x[0]*x[1] - 2.0;
            	break;
            case GAUSSIAN:
            	for (i = 0; i < m; i++) {
            		double diff = tdata[i] - x[2];
            	    residuals[i] = x[0]*Math.exp(-x[1]*diff*diff/2.0) - ydata[i];
            	}
            	break;
            case GULF_RESEARCH_AND_DEVELOPMENT:
            	for (i = 0; i < m ; i++) {
            		residuals[i] = Math.exp(-Math.pow((Math.abs(ydata[i]-x[1])),x[2])/x[0]) - tdata[i];
            	}
            	break;
            case BIGGS_EXP6:
            	for (i = 0; i < m; i++) {
            		residuals[i] = x[2]*Math.exp(-tdata[i]*x[0]) - x[3]*Math.exp(-tdata[i]*x[1])
            				+ x[5]*Math.exp(-tdata[i]*x[4]) - ydata[i];
            	}
            	break;
            case PENALTY_FUNCTION_I:
            	for (i = 0; i < n; i++) {
            		residuals[i] = Math.sqrt(1.0E-5)*(x[i] - 1.0);
            	}
            	residuals[n] = -0.25;
            	for (j = 0; j < n; j++) {
            		residuals[n] += (x[j]*x[j]);
            	}
            	break;
            } // switch (testCase)
            
        } catch (Exception e) {
            Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
        }

        return;
    }
    
    private void fitToTestJacobian(double x[], double J[][]) {
        int i;
        int j;
        double denom;
        double top;
        double exponent;
        double t[];
        try {
            switch (testCase) {
            case DRAPER24D:
            	for (i = 0; i < m; i++) {
                    J[i][0] = 1.0;
                    J[i][1] = -Math.pow(x[2], tdata[i]);
                    if (i == 0) {
                    	J[i][2] = 0.0;
                    }
                    else {
                        J[i][2] = -tdata[i] * x[1] * Math.pow(x[2], tdata[i] - 1.0);
                    }
                }
                break;
            case HOCK25:
            	// Calculate the Jacobian analytically
                for (i = 0; i < m; i++) {
                    J[i][0] = x[1]*Math.pow((x[0] * Math.log(tdata[i])),x[1]-1.0) * Math.log(tdata[i]);
                    J[i][1] = Math.log(x[0] * Math.log(tdata[i])) * Math.pow((x[0] * Math.log(tdata[i])),x[1]);
                    J[i][2] = 1.0;
                }
                break;
            case BARD:
                    // Calculate the Jacobian analytically
                    for (i = 0; i < m; i++) {
                    	denom = (x[1]*(16.0 - tdata[i]) + x[2]*Math.min(tdata[i], 16.0 - tdata[i]));
                        J[i][0] = 1.0;
                        J[i][1] = -tdata[i]*(16.0 - tdata[i])/(denom*denom);
                        J[i][2] = -tdata[i]*Math.min(tdata[i], 16.0 - tdata[i])/(denom*denom);
                    }
                    break;
            case KOWALIK_AND_OSBORNE:
            	 // Calculate the Jacobian analytically
                for (i = 0; i < m; i++) {
                	denom = (tdata[i]*tdata[i] + x[2]*tdata[i] + x[3]);
                	top = (tdata[i]*tdata[i] + x[1]*tdata[i]);
                    J[i][0] = top/denom;
                    J[i][1] = x[0]*tdata[i]/denom;
                    J[i][2] = -x[0]*tdata[i]*top/(denom*denom);
                    J[i][3] = -x[0]*top/(denom*denom);
                }
                break;
            case ROSENBROCK:
            	J[0][0] = -20.0*x[0];
    		    J[0][1] = 10.0;
    		    J[1][0] = -1.0;
    		    J[1][1] = 0.0;
            	break;
            case FREUDENSTEIN_AND_ROTH:
            	J[0][0] = 1.0;
    		    J[0][1] = 10.0*x[1] - 3.0*x[1]*x[1] - 2.0;
    		    J[1][0] = 1.0;
    		    J[1][1] = 3.0*x[1]*x[1] + 2.0*x[1] - 14.0;
    		    break;
            case HELICAL_VALLEY:
            	double tmp;
    			tmp = x[0]*x[0] + x[1]*x[1];
    		    J[0][0] = (50.0*x[1])/(Math.PI * tmp);
    			J[0][1] = (-50.0*x[0])/(Math.PI * tmp);
    			J[0][2] = 10.0;
    			J[1][0]= 10.0*x[0]/Math.sqrt(tmp);
    			J[1][1] = 10.0*x[1]/Math.sqrt(tmp);
    			J[1][2] = 0.0;
    			J[2][0] = 0.0;
    			J[2][1] = 0.0;
    			J[2][2] = 1.0;
            	break;
            case JENNRICH_AND_SAMPSON:
            	for (i = 0; i < 10; i++) {
    	    	    J[i][0] = -Math.exp((i+1.0)*x[0])*(i + 1.0);
    	    	    J[i][1] = -Math.exp((i+1.0)*x[1])*(i + 1.0);
    	        }
            	break;
            case MEYER:
            	for (i = 0; i < m; i++) {
	            	exponent = Math.exp(x[1]/(tdata[i] + x[2]));
	                J[i][0] = exponent;
	                J[i][1] = (x[0]/(tdata[i] + x[2]))* exponent;
	                J[i][2] = -(x[0]*x[1]/((tdata[i] + x[2])*(tdata[i] + x[2]))) * exponent;
            	}
            	break;
            case BOX_3D:
            	t = new double[10];
    			for (i = 0; i <10; i++) {
        		    t[i] = 0.1*(i+1.0);	
        		    J[i][0] = -t[i]*Math.exp(-x[0]*t[i]);
        		    J[i][1] = t[i]*Math.exp(-x[1]*t[i]);
        		    J[i][2] = Math.exp(-10.0*t[i]) - Math.exp(-t[i]);
        		}	
            	break;
            case POWELL_SINGULAR:
            	J[0][0] = 1.0;
    		    J[0][1] = 10.0;
    		    J[0][2] = 0.0;
    		    J[0][3] = 0.0;
    		    J[1][0] = 0.0;
    		    J[1][1] = 0.0;
    		    J[1][2] = Math.sqrt(5.0);
    		    J[1][3] = -Math.sqrt(5.0);
    		    J[2][0] = 0.0;
    		    J[2][1] = 2.0*x[1] - 4.0*x[2];
    		    J[2][2] = 8.0*x[2] - 4.0*x[1];
    		    J[2][3] = 0.0;
    		    J[3][0] = 2.0*Math.sqrt(10.0)*x[0] - 2.0*Math.sqrt(10.0)*x[3];
    		    J[3][1] = 0.0;
    		    J[3][2] = 0.0;
    		    J[3][3] = 2.0*Math.sqrt(10.0)*x[3] - 2.0*Math.sqrt(10.0)*x[0];
            	break;
            case BROWN_AND_DENNIS:
            	t = new double[20];
        		double part1;
        		double part2;
        		for (i = 0; i <20; i++) {
        		    t[i] = 0.2*(i+1.0);
        		    part1 = x[0] + x[1]*t[i] - Math.exp(t[i]);
        		    part2 = x[2] + x[3]*Math.sin(t[i]) - Math.cos(t[i]);
    		        J[i][0] = 2.0*part1;
    		        J[i][1] = 2.0*part1*t[i];
    		        J[i][2] = 2.0*part2;
    		        J[i][3] = 2.0*part2*Math.sin(t[i]);
        		}
            	break;
            case OSBORNE1:
            	for (i = 0; i < m; i++) {
                    J[i][0] = 1.0;
                    J[i][1] = Math.exp(-x[3]*tdata[i]);
                    J[i][2] = Math.exp(-x[4]*tdata[i]);
                    J[i][3] = -x[1]*tdata[i]*Math.exp(-x[3]*tdata[i]);
                    J[i][4] = -x[2]*tdata[i]*Math.exp(-x[4]*tdata[i]);
                }
            	break;
            case OSBORNE2:
            	// Calculate the Jacobian analytically
                for (i = 0; i < m; i++) {
                    J[i][0] = Math.exp(-x[4]*tdata[i]);
                    J[i][1] = Math.exp(-x[5]*(tdata[i] - x[8])*(tdata[i] - x[8]));
                    J[i][2] = Math.exp(-x[6]*(tdata[i] - x[9])*(tdata[i] - x[9]));
                    J[i][3] = Math.exp(-x[7]*(tdata[i] - x[10])*(tdata[i] - x[10]));
                    J[i][4] = -x[0]*tdata[i]*Math.exp(-x[4]*tdata[i]) ;
                    J[i][5] = -x[1]*(tdata[i] - x[8])*(tdata[i] - x[8])
                                     *Math.exp(-x[5]*(tdata[i] - x[8])*(tdata[i] - x[8]));
                    J[i][6] = -x[2]*(tdata[i] - x[9])*(tdata[i] - x[9])
                                     *Math.exp(-x[6]*(tdata[i] - x[9])*(tdata[i] - x[9]));
                    J[i][7] = -x[3]*(tdata[i] - x[10])*(tdata[i] - x[10])
                                     *Math.exp(-x[7]*(tdata[i] - x[10])*(tdata[i] - x[10]));
                    J[i][8] = 2.0*x[1]*x[5]*(tdata[i] - x[8])
                                     *Math.exp(-x[5]*(tdata[i] - x[8])*(tdata[i] - x[8]));
                    J[i][9] = 2.0*x[2]*x[6]*(tdata[i] - x[9])
                                     *Math.exp(-x[6]*(tdata[i] - x[9])*(tdata[i] - x[9]));
                    J[i][10] = 2.0*x[3]*x[7]*(tdata[i] - x[10])
                                      *Math.exp(-x[7]*(tdata[i] - x[10])*(tdata[i] - x[10]));
                }
            	break;
            case WATSON:
            	double sum2;
        		t = new double[29];
        	    for (i = 0; i < 31; i++) {
        	    	if (i < 29) {
        	            t[i] = (i+1.0)/29.0;
        	            sum2 = 0.0;
            	        for (j = 1; j <= n; j++) {
            	            sum2 += x[j-1]*Math.pow(t[i], j-1.0);	
            	        }
        	            J[i][0] = -2.0*sum2;
        	            for (j = 2; j <= n; j++) {
        	            	J[i][j-1] = (j-1.0)*Math.pow(t[i],j-2.0) - 2.0*sum2*Math.pow(t[i],j-1.0);
        	            }
        	    	} // if (i < 29)
        	    	else if (i == 29) {
        	    		J[i][0] = 1.0;
        	    		for (j = 1; j < n; j++) {
        	    			J[i][j] = 0.0;
        	    		}
        	    	}
        	    	else if (i == 30) {
        	    		J[i][0] = -2.0*x[0];
        	    		J[i][1] = 1.0;
        	    		for (j = 2; j < n; j++) {
        	    			J[i][j] = 0.0;
        	    		}
        	    	}
        	    } // for (i = 0; i < 31; i++)
            	break;
            case BROWN_ALMOST_LINEAR:
            	double prodParam;
    			for (i = 0; i < m - 1; i++) {
    				for (j = 0; j < m; j++) {
    				    if (i == j) {
    				    	J[i][j] = 2.0;
    				    }
    				    else {
    				    	J[i][j] = 1.0;
    				    }
    				}
    			}
    			for (i = 0; i < m; i++) {
    				prodParam = 1.0;
    				for (j = 0; j < m; j++) {
    					if (i != j) {
    						prodParam = prodParam*x[j];
    					}
    				}
    			    J[m-1][i] = prodParam;	
    			}
            	break;
            case LINEAR_FULL_RANK:
            	for (i = 0; i < n; i++) {
        		    for (j = 0; j < n; j++) {
        		        if (i == j) {
        		        	J[i][j] = 1.0 - 2.0/m;
        		        }
        		        else {
        		        	J[i][j] = -2.0/m;
        		        }
        		    }
        		} // for (i = 0; i < param; i++)
        		for (i = n; i < m; i++) {
        			for (j = 0; j < n; j++) {
        			    J[i][j] = -2.0/m;
        			}
        		} // for (i = n; i < m; i++)
            	break;
            case LINEAR_RANK1:
            	for (i = 0; i < m; i++) {
    		    	for (j = 0; j < n; j++) {
    		    		J[i][j] = (i+1.0)*(j+1.0);
    		    	}
    		    }
            	break;
            case LINEAR_RANK1_WITH_ZERO_COLUMNS_AND_ROWS:
            	for (j = 0; j < n; j++) {
    				J[0][j] = 0.0;
    				J[m-1][j] = 0.0;
    			}
    		    for (i = 1; i < m-1; i++) {
    		    	J[i][0] = 0.0;
    		    	J[i][n-1] = 0.0;
    		    	for (j = 1; j < n-1; j++) {
    		    		J[i][j] = i*(j+1.0);
    		    	}
    		    }
            	break;
            case CHEBYQUAD:
            	for (i = 1; i <= m; i++) {
        		    for (j = 0; j < n; j++) {
        		    	J[i-1][j] = shiftedChebyshevDerivative(x[j],i)/n;
        		    }
        		}
            	break;
            case LEVMAR_ROSENBROCK:
            	J[0][0] = (-2.0 + 2.0*x[0] - 4.0*105.0*(x[1] - x[0]*x[0])*x[0]);
	        	J[0][1] = (2*105.0*(x[1] - x[0]*x[0]));
	        	J[1][0] = (-2.0 + 2.0*x[0] - 4.0*105.0*(x[1] - x[0]*x[0])*x[0]);
	        	J[1][1] = (2*105.0*(x[1] - x[0]*x[0]));
            	break;
            case POWELL_2_PARAMETER:
            	J[0][0] = 1.0;
	        	J[0][1] = 0.0;
	        	J[1][0] = 1.0/((x[0] + 0.1)*(x[0] + 0.1));
	        	J[1][1] = 4.0*x[1];
            	break;
            case MODIFIED_ROSENBROCK:
            	J[0][0] = -20.0*x[0];
	        	J[0][1] = 10.0;
	        	J[1][0] = -1.0;
	        	J[1][1] = 0.0;
	        	J[2][0] = 0.0;
	        	J[2][1] = 0.0;	
            	break;
            case WOOD:
            	J[0][0] = -20.0*x[0];
	   		     J[0][1] = 10.0;
	   		     J[0][2] = 0.0;
	   		     J[0][3] = 0.0;
	   		     J[1][0] = -1.0;
	   		     J[1][1] = 0.0;
	   		     J[1][2] = 0.0;
	   		     J[1][3] = 0.0;
	   		     J[2][0] = 0.0;
	   		     J[2][1] = 0.0;
	   		     J[2][2] = -2.0*Math.sqrt(90.0)*x[2];
	   		     J[2][3] = Math.sqrt(90.0);
	   		     J[3][0] = 0.0;
	   		     J[3][1] = 0.0;
	   		     J[3][2] = -1.0;
	   		     J[3][3] = 0.0;
	   		     J[4][0] = 0.0;
	   		     J[4][1] = Math.sqrt(10.0);
	   		     J[4][2] = 0.0;
	   		     J[4][3] = Math.sqrt(10.0);
	   		     J[5][0] = 0.0;
	   		     J[5][1] = 1.0/Math.sqrt(10.0);
	   		     J[5][2] = 0.0;
	   		     J[5][3] = -1.0/Math.sqrt(10.0);
            	break;
            case HOCK1:
            	J[0][0] = -20.0*x[0];
    		    J[0][1] = 10.0;
    		    J[1][0] = -1.0;
    		    J[1][1] = 0.0;
            	break;
            case HOCK21_MODIFIED:
            	J[0][0] = 0.1;
    		    J[0][1] = 0.0;
    		    J[1][0] = 0.0;
    		    J[1][1] = 1.0;
            	break;
            case HATFLDB:
            	J[0][0] = 1.0;
    		    J[0][1] = 0.0;
    		    J[0][2] = 0.0;
    		    J[0][3] = 0.0;
    		    J[1][0] = 1.0;
    		    J[1][1] = -0.5/Math.sqrt(x[1]);
    		    J[1][2] = 0.0;
    		    J[1][3] = 0.0;
    		    J[2][0] = 0.0;
    		    J[2][1] = 1.0;
    		    J[2][2] = -0.5/Math.sqrt(x[2]);
    		    J[2][3] = 0.0;
    		    J[3][0] = 0.0;
    		    J[3][1] = 0.0;
    		    J[3][2] = 1.0;
    		    J[3][3] = -0.5/Math.sqrt(x[3]);
            	break;
            case HATFLDC:
            	J[0][0] = 1.0;
     		    J[0][1] = 0.0;
     		    J[0][2] = 0.0;
     		    J[0][3] = 0.0;
     		    J[1][0] = 1.0;
     		    J[1][1] = -0.5/Math.sqrt(x[1]);
     		    J[1][2] = 0.0;
     		    J[1][3] = 0.0;
     		    J[2][0] = 0.0;
     		    J[2][1] = 1.0;
     		    J[2][2] = -0.5/Math.sqrt(x[2]);
     		    J[2][3] = 0.0;
     		    J[3][0] = 0.0;
     		    J[3][1] = 0.0;
     		    J[3][2] = 0.0;
     		    J[3][3] = 1.0;
            	break;
            case EQUILIBRIUM_COMBUSTION:
            	double R, R5, R6, R7, R8, R9, R10;

    			  R=10;
    			  R5=0.193;
    			  R6=4.10622*1e-4;
    			  R7=5.45177*1e-4;
    			  R8=4.4975*1e-7;
    			  R9=3.40735*1e-5;
    			  R10=9.615*1e-7;

    			  for (i = 0; i < m; i++) {
    				  for (j = 0; j < n; j++) {
    					  J[i][j] = 0.0;
    				  }
    			  }

    			  J[0][0] = x[1]+1;
    			  J[0][1] = x[0];
    			  J[0][4]=-3;

    			  J[1][0]=2*x[1]+1;
    			  J[1][1]=2*x[0]+6*R10*x[1]+x[2]*x[2]+R7*x[2]+R9*x[3]+R8;
    			  J[1][2]=2*x[1]*x[2]+R7*x[1];
    			  J[1][3]=R9*x[1];
    			  J[1][4]=-R;

    			  J[2][1]=2*x[2]*x[2]+R7*x[2];
    			  J[2][2]=4*x[1]*x[2]+R7*x[1]+4*R5*x[2]+R6;
    			  J[2][4]=-8;

    			  J[3][1]=R9*x[3];
    			  J[3][3]=R9*x[1]+4*x[3];
    			  J[3][4]=-4*R;

    			  J[4][0]=x[1]+1;
    			  J[4][1]=x[0]+2*R10*x[1]+x[2]*x[2]+R7*x[2]+R9*x[3]+R8;
    			  J[4][2]=2*x[1]*x[2]+R7*x[1]+2*R5*x[2]+R6;
    			  J[4][3]=R9*x[1]+2*x[3];	
            	break;
            case BEALE:
            	J[0][0] = 1 - x[1];
            	J[0][1] = -x[0];
            	J[1][0] = 1 - x[1]*x[1];
            	J[1][1] = -2.0*x[0]*x[1];
            	J[2][0] = 1 - x[1]*x[1]*x[1];
            	J[2][1] = -3*x[0]*x[1]*x[1];
            	break;
            case POWELL_BADLY_SCALED:
            	J[0][0] = 1.0E4*x[1];
            	J[0][1] = 1.0E4*x[0];
            	J[1][0] = -Math.exp(-x[0]);
            	J[1][1] = -Math.exp(-x[1]);
            	break;
            case BROWN_BADLY_SCALED:
            	J[0][0] = 1.0;
            	J[0][1] = 0.0;
            	J[1][1] = 0.0;
            	J[1][1] = 1.0;
            	J[2][0] = x[1];
            	J[2][1] = x[0];
            	break;
            case GAUSSIAN:
            	for (i = 0; i < m; i++) {
            		double diff = tdata[i] - x[2];
            		J[i][0] = Math.exp(-x[1]*diff*diff/2.0);
            		J[i][1] = (-x[0]*diff*diff/2.0)*Math.exp(-x[1]*diff*diff/2.0);
            		J[i][2] = x[0]*x[1]*diff*Math.exp(-x[1]*diff*diff/2.0);
            	}
            	break;
            case GULF_RESEARCH_AND_DEVELOPMENT:
            	for (i = 0; i < m; i++) {
            		double absVal = Math.abs(ydata[i] - x[1]);
            		double abspow = Math.pow(absVal, x[2]);
            		double absdiv = abspow/x[0];
            		double absexp = Math.exp(-absdiv);
            		J[i][0] = abspow * absexp/(x[0]*x[0]);
            		J[i][1] = -x[2]*absexp*abspow/(x[0]*(x[1] - ydata[i]));
            		J[i][2] = -abspow * Math.log(absVal) * absexp/x[0];
            	}
            	break;
            case BIGGS_EXP6:
            	for (i = 0; i < m; i++) {
            		J[i][0] = -tdata[i]*x[2]*Math.exp(-tdata[i]*x[0]);
            		J[i][1] = tdata[i]*x[3]*Math.exp(-tdata[i]*x[1]);
            		J[i][2] = Math.exp(-tdata[i]*x[0]);
            		J[i][3] = -Math.exp(-tdata[i]*x[1]);
            		J[i][4] = -tdata[i]*x[5]*Math.exp(-tdata[i]*x[4]);
            		J[i][5] = Math.exp(-tdata[i]*x[4]);
            	}
            	break;
            case PENALTY_FUNCTION_I:
            	for (i = 0; i < n; i++) {
            		for (j = 0; j < n; j++) {
            		    J[i][j] = 0.0;
            		}
            	}
            	for (i = 0; i < n; i++) {
            		J[i][i] = Math.sqrt(1.0E-5);
            	}
            	for (j = 0; j < n; j++) {
            		J[n][j] = 2*x[j];
            	}
            	break;
            } // switch (testCase)
        } catch (Exception e) {
            Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
        }

        return;
    }
    
    private void fitToTestHessian(double x[], double hessian[][][]) {
        int i;
        try {
            switch (testCase) {
            case DRAPER24D:
            	for (i = 0; i < m; i++) {
                    hessian[0][0][i] = 0.0;
                    hessian[0][1][i] = 0.0;
                    hessian[0][2][i] = 0.0;
                    hessian[1][0][i] = 0.0;
                    hessian[1][1][i] = 0.0;
                    if (i == 0) {
                    	hessian[1][2][i] = 0.0;
                    }
                    else {
                    	hessian[1][2][i] = -tdata[i] * Math.pow(x[2], tdata[i] - 1.0);
                    }
                    hessian[2][0][i] = 0.0;
                    if (i == 0) {
                    	hessian[2][1][i] = 0.0;
                    }
                    else {
                    	hessian[2][1][i] = -tdata[i] * Math.pow(x[2], tdata[i] - 1.0);
                    }
                    if ((i == 0) || (i == 1)) {
                    	hessian[2][2][i] = 0.0;
                    }
                    else {
                    	hessian[2][2][i] = -tdata[i] * (tdata[i] - 1.0) * x[1] * Math.pow(x[2], tdata[i] - 2.0);
                    }
                }
                break;
            case HOCK25:
            	for (i = 0; i < m; i++) {
            	    hessian[0][0][i] = x[1]*(x[1]-1.0)*Math.pow((x[0] * Math.log(tdata[i])),x[1]-2.0) * Math.log(tdata[i]) * Math.log(tdata[i]);
            	    hessian[0][1][i] = Math.pow((x[0] * Math.log(tdata[i])),x[1]-1.0) * Math.log(tdata[i]) +
            	    		           x[1] *  Math.log(x[0] * Math.log(tdata[i])) * Math.pow((x[0] * Math.log(tdata[i])),x[1]-1.0) * Math.log(tdata[i]);
            	    hessian[0][2][i] = 0.0;
            	    hessian[1][0][i] = x[1]*Math.pow((x[0] * Math.log(tdata[i])),x[1]-1.0) * Math.log(tdata[i]) * Math.log(x[0] * Math.log(tdata[i])) +
            	    		           (1.0/x[0]) * Math.pow((x[0] * Math.log(tdata[i])),x[1]);
            	    hessian[1][1][i] = Math.log(x[0] * Math.log(tdata[i])) * Math.log(x[0] * Math.log(tdata[i])) * Math.pow((x[0] * Math.log(tdata[i])),x[1]);
            	    hessian[1][2][i] = 0.0;
            	    hessian[2][0][i] = 0.0;
            	    hessian[2][1][i] = 0.0;
            	    hessian[2][2][i] = 0.0;
            	}
            	break;
            } // switch (testCase)
        } catch (Exception e) {
            Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
        }

        return;
    }
    
 // Shifted Chebyshev polynomial
    // Used over the half interval 0 <= x <= 1 instead of the full Chebyshev interval of -1 <= x <= 1
    private double shiftedChebyshev(double x, int n) {
    	// T*n+1(x) = (4x-2)*T*n(x) - T*n-1(x), where T* represents a shifted Chebyshev polynomial
    	double sc = 1.0;
    	double x2, x3, x4, x5, x6, x7, x8, x9, x10;
    	switch (n) {
    	    case 0:
    	    	sc = 1.0;
    	    	break;
    	    case 1:
    	        sc = 2.0*x - 1.0;
    	        break;
    	    case 2:
    	    	x2 = x*x;
    	    	sc = 8.0*x2 - 8.0*x + 1.0;
    	    	break;
    	    case 3:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	sc = 32.0*x3 - 48.0*x2 + 18.0*x - 1.0;
    	    	break;
    	    case 4:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	sc = 128.0*x4 - 256.0*x3 + 160.0*x2 - 32.0*x + 1.0;
    	    	break;
    	    case 5:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	x5 = x4*x;
    	    	sc = 512.0*x5 - 1280.0*x4 + 1120.0*x3 - 400.0*x2 + 50.0*x - 1.0;
    	    	break;
    	    case 6:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	x5 = x4*x;
    	    	x6 = x5*x;
    	    	sc = 2048.0*x6 - 6144.0*x5 + 6912.0*x4 - 3584.0*x3 + 840.0*x2 - 72.0*x + 1.0;
    	    	break;
    	    case 7:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	x5 = x4*x;
    	    	x6 = x5*x;
    	    	x7 = x6*x;
    	    	sc = 8192.0*x7 - 28672.0*x6 + 39424.0*x5 - 26880.0*x4 + 9408.0*x3 - 1568.0*x2 + 98.0*x - 1.0;
    	    	break;
    	    case 8:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	x5 = x4*x;
    	    	x6 = x5*x;
    	    	x7 = x6*x;
    	    	x8 = x7*x;
    	    	sc = 32768.0*x8 - 131072.0*x7 + 212992.0*x6 - 180224.0*x5 + 84480.0*x4 - 21504.0*x3
    	    	     + 2688.0*x2 - 128.0*x + 1.0;
    	    	break;
    	    case 9:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	x5 = x4*x;
    	    	x6 = x5*x;
    	    	x7 = x6*x;
    	    	x8 = x7*x;
    	    	x9 = x8*x;
    	    	sc = 131072.0*x9 - 589824.0*x8 + 1105920.0*x7 - 1118208.0*x6 + 658944.0*x5
    	    	     - 228096.0*x4 + 44352.0*x3 - 4320.0*x2 + 162.0*x - 1.0;
    	    	break;
    	    case 10:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	x5 = x4*x;
    	    	x6 = x5*x;
    	    	x7 = x6*x;
    	    	x8 = x7*x;
    	    	x9 = x8*x;
    	    	x10 = x9*x;
    	    	sc = 524288.0*x10 - 2621440.0*x9 + 5570560.0*x8 - 6553600.0*x7 + 4659200.0*x6
    	    	     - 2050048.0*x5 + 549120.0*x4 - 84480.0*x3 + 6600.0*x2 - 200.0*x + 1.0;
    	} // switch (n)
    	return sc;
    } // private double shiftedChebyshev
    
 // Shifted Chebyshev polynomial derivative
    private double shiftedChebyshevDerivative(double x, int n) {
    	double sc = 0.0;
    	double x2, x3, x4, x5, x6, x7, x8, x9;
    	switch (n) {
    	    case 0:
    	    	sc = 0.0;
    	    	break;
    	    case 1:
    	    	sc = 2.0;
    	        break;
    	    case 2:
    	    	sc = 16.0*x - 8.0;
    	    	break;
    	    case 3:
    	    	x2 = x*x;
    	    	sc = 96.0*x2 - 96.0*x + 18.0;
    	    	break;
    	    case 4:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	sc = 512.0*x3 - 768.0*x2 + 320.0*x - 32.0;
    	    	break;
    	    case 5:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	sc = 2560.0*x4 - 5120.0*x3 + 3360.0*x2 - 800.0*x + 50.0;
    	    	break;
    	    case 6:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	x5 = x4*x;
    	    	sc = 12288.0*x5 - 30720.0*x4 + 27648.0*x3 - 10752.0*x2 + 1680.0*x - 72.0;
    	    	break;
    	    case 7:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	x5 = x4*x;
    	    	x6 = x5*x;
    	    	sc = 57344.0*x6 - 172032.0*x5 + 197120.0*x4 - 107520.0*x3 + 28224.0*x2 - 3136.0*x + 98.0;
    	    	break;
    	    case 8:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	x5 = x4*x;
    	    	x6 = x5*x;
    	    	x7 = x6*x;
    	    	sc = 262144.0*x7 - 917504.0*x6 + 1277952.0*x5 - 901120.0*x4 + 337920.0*x3
    	    	     - 64512.0*x2 + 5376.0*x - 128.0;
    	    	break;
    	    case 9:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	x5 = x4*x;
    	    	x6 = x5*x;
    	    	x7 = x6*x;
    	    	x8 = x7*x;
    	    	sc = 1179648.0*x8 - 4718592.0*x7 + 7741440.0*x6 - 6709248.0*x5 + 3294720.0*x4
    	    	     - 912384.0*x3 + 133056.0*x2 - 8640.0*x + 162.0;
    	    	break;
    	    case 10:
    	    	x2 = x*x;
    	    	x3 = x2*x;
    	    	x4 = x3*x;
    	    	x5 = x4*x;
    	    	x6 = x5*x;
    	    	x7 = x6*x;
    	    	x8 = x7*x;
    	    	x9 = x8*x;
    	    	sc = 5242880.0*x9 - 23592960.0*x8 + 44564480.0*x7 - 45875200.0*x6 + 27955200.0*x5
    	    	     - 10250240.0*x4 + 2196480.0*x3 - 253440.0*x2 + 13200.0*x - 200.0;
    	} // switch (n)
    	return sc;
    } // private double shiftedChebyshevDerivative
    
	/**
     * fitToFunction communicates
     *
     * @param  x          DOCUMENT ME!
     * @param  residuals  DOCUMENT ME!
     */
    public abstract void fitToFunction(double[] x, double[] residuals);
    
    /**
     * fitToJacobian communicates
     *
     * @param  x          DOCUMENT ME!
     * @param  Jacobian   DOCUMENT ME!
     */
    public abstract void fitToJacobian(double[] x, double[][] jacobian);
    
    public abstract void fitToHessian(double[] x, double[][][] hessian);
    
    private void driver() {
    	// driver is an implementation of the code in levenberg_marquardt.jl
    	// tdata independent variable of length m
    	// ydata measured dependent variable of length m
    	int i, r, c;
    	// int n = initial_x.length;
    	int f_calls = 0;
    	int df_calls = 0;
    	double x_f[] = new double[n];
    	double x_df[] = new double[n];
    	residuals = new double[m]; // model[i] - ydata[i]
    	iterCt = 0;
    	double lambda = initial_lambda;
    	
    	double J[][] = new double[m][n];
    	
    	// First evaluation
    	/*value_jacobian!!(df, initial_x)
    	value_jacobian!!(obj, x) = value_jacobian!!(obj, obj.F, obj.DF, x)
    			function value_jacobian!!(obj, F, J, x)
    			    obj.fdf(F, J, x)
    			    copyto!(obj.x_f, x)
    			    copyto!(obj.x_df, x)
    			    obj.f_calls .+= 1
    			    obj.df_calls .+= 1
    			    obj.df_calls
    			    F, J
    			end*/
    	/*# Given callables to calculate objectives and partial first derivatives
    	# create a function that calculates both.
    	function make_fdf(x, F, f!, j!)
    	    function fj!(fx, jx, x)
    	        j!(jx, x)
    	        return f!(fx, x)
    	    end
    	end*/
        f_calls += 1;
        df_calls += 1;
        for (i = 0; i < n; i++) {
        	x_f[i] = initial_x[i];
        	x_df[i] = initial_x[i];
        }
        if (testMode) {
        	fitToTestFunction(initial_x, residuals);
        	fitToTestJacobian(initial_x, J);
        }
        else {
        	fitToFunction(initial_x, residuals);
        	fitToJacobian(initial_x, J);
        }
        
        if (Double.isFinite(tau)) {
            Matrix JMat = new Matrix(J);
            double JtJ[][] = ((JMat.transpose()).times(JMat)).getArray();
            double maxVal = -Double.MAX_VALUE;
            for (r = 0; r < n; r++) {
            	for (c = 0; c < n; c++) {
            		if (JtJ[r][c] > maxVal) {
            			maxVal = JtJ[r][c];
            		}
            	}
            }
            lambda = tau * maxVal;
        }
        
        // check parameters
        if ((lower != null) && (lower.length != n)) {
        	System.err.println("lower must either be null or have a length equal to the number of parameters");
        	Preferences.debug("lower must either be null or have a length equal to the number of parameters\n", Preferences.DEBUG_ALGORITHM);
        	return;
        }
        if ((upper != null) && (upper.length != n)) {
        	System.err.println("upper must either be null or have a length equal to the number of parameters");
        	Preferences.debug("upper must either be null or have a length equal to the number of parameters\n", Preferences.DEBUG_ALGORITHM);
        	return;
        }
        if (lower != null) {
        	for (i = 0; i < n; i++) {
        		if (initial_x[i] < lower[i]) {
        			System.err.println("initial_x["+i+"] must be >= lower["+i+"]");
        			Preferences.debug("initial_x["+i+"] must be >= lower["+i+"]\n", Preferences.DEBUG_ALGORITHM);
        			return;
        		}
        	}
        	
        }
        if (upper != null) {
        	for (i = 0; i < n; i++) {
        		if (initial_x[i] > upper[i]) {
        			System.err.println("initial_x["+i+"] must be <= upper["+i+"]");
        			Preferences.debug("initial_x["+i+"] must be <= upper["+i+"]\n", Preferences.DEBUG_ALGORITHM);
        			return;
        		}
        	}
        	
        }
        
        if (min_step_quality < 0) {
        	System.err.println("min_step_quality must be >= 0");
        	Preferences.debug("min_step_quality must be >= 0\n", Preferences.DEBUG_ALGORITHM);
        	return;
        }
        if (min_step_quality >= 1) {
        	System.err.println("min_step_quality must be < 1");
        	Preferences.debug("min_step_quality must be < 1\n", Preferences.DEBUG_ALGORITHM);
        	return;
        }
        
        if (good_step_quality <= 0) {
        	System.err.println("good_step_quality must be > 0");
        	Preferences.debug("good_step_quality must be > 0\n", Preferences.DEBUG_ALGORITHM);
        	return; 
        }
        if (good_step_quality > 1) {
        	System.err.println("good_step_quality must be <= 1");
        	Preferences.debug("good_step_quality must be <= 1\n", Preferences.DEBUG_ALGORITHM);
        	return;
        }
        if (min_step_quality >= good_step_quality) {
        	System.err.println("min_step_quality must be < good_step_quality");
        	Preferences.debug("min_step_quality must be < good_step_quality\n", Preferences.DEBUG_ALGORITHM);
        	return;
        }
        
        // other constants
        double MAX_LAMBDA = 1e16; // minimum trust region radius
        double MIN_LAMBDA = 1e-16; // maximum trust region radius
        double MIN_DIAGONAL = 1e-6; // lower bound on values of diagonal matrix used to regularize the trust region step
        
        converged = false;
        boolean x_converged = false;
        boolean g_converged = false;
        x = new double[n];
        double delta_x[] = new double[n];
        for (i = 0; i < n; i++) {
        	x[i] = initial_x[i];
        	delta_x[i] = initial_x[i];
        }
        double a[] = new double[n];
        
        double trial_f[] = new double[m];
        double residual = 0.0;
        for (i = 0; i < m; i++) {
        	residual += (residuals[i]*residuals[i]);
        }
        
        // Create buffers
        double n_buffer[] = new double[n];
        double Jdelta_buffer[] = new double[m];
        double dir_deriv[] = new double[m];
        double v[] = new double[n];
        double Jres[] = new double[n];
        
        if (show_trace) {
        	Preferences.debug("iterCt = " + iterCt + "\n", Preferences.DEBUG_ALGORITHM);
        	Preferences.debug("lambda = " + lambda + "\n", Preferences.DEBUG_ALGORITHM);
        	Preferences.debug("residual = " + residual + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        double DtD[] = new double[n];
        while ((!converged) && iterCt < maxIter) {
            // jacobian! will check if x is new or not, so it is only actually
            // evaluated if x was updated last iteration.
        	/*value_jacobian!(obj, x) = value_jacobian!(obj, obj.F, obj.DF, x)
        			function value_jacobian!(obj, F, J, x)
        			    if x != obj.x_f && x != obj.x_df
        			        value_jacobian!!(obj, F, J, x)
        			    elseif x != obj.x_f
        			        value!!(obj, F, x)
        			    elseif x != obj.x_df
        			        jacobian!!(obj, J, x)
        			    end
        			    F, J
        			end */
            //jacobian!(df, x) // has alias J
        	boolean fequal = true;
        	for (i = 0; (i < n) && fequal; i++) {
        		if (x_f[i] != x[i]) {
        			fequal = false;
        		}
        	}
        	if (!fequal) {
        		f_calls += 1;
                for (i = 0; i < n; i++) {
                	x_f[i] = x[i];
                }
                if (testMode) {
                	fitToTestFunction(x, residuals);
                }
                else {
                	fitToFunction(x, residuals);
                }	
        	} // if (!fequal)
        	
        	boolean dfequal = true;
        	for (i = 0; (i < n) && dfequal; i++) {
        		if (x_df[i] != x[i]) {
        			dfequal = false;
        		}
        	}
        	if (!dfequal) {
        		df_calls += 1;
                for (i = 0; i < n; i++) {
                	x_df[i] = x[i];
                }
                if (testMode) {
                	fitToTestJacobian(x, J);
                }
                else {
                	fitToJacobian(x, J);
                }	
        	} // if (!dfequal)

            // we want to solve:
            //    argmin 0.5*||J(x)*delta_x + f(x)||^2 + lambda*||diagm(J'*J)*delta_x||^2
            // Solving for the minimum gives:
            //    (J'*J + lambda*diagm(DtD)) * delta_x == -J' * f(x), where DtD = sum(abs2, J,1)
            // Where we have used the equivalence: diagm(J'*J) = diagm(sum(abs2, J,1))
            // It is additionally useful to bound the elements of DtD below to help
            // prevent "parameter evaporation".
        	
        	for (c = 0; c < n; c++) {
        		DtD[c] = 0.0;
        	    for (r = 0; r < m; r++) {
        	    	DtD[c] += (J[r][c]*J[r][c]);
        	    }
        	}
        	for (i = 0; i < n; i++) {
	            if (DtD[i] <= MIN_DIAGONAL) {
	                DtD[i] = MIN_DIAGONAL;
	            }
        	}
        	
        	// delta_x = ( J'*J + lambda * Diagonal(DtD) ) \ ( -J'*value(df) )
        	Matrix JMat = new Matrix(J);
            double JJ[][] = ((JMat.transpose()).times(JMat)).getArray();
            for (i = 0; i < n; i++) {
            	JJ[i][i] += (lambda * DtD[i]);
            }
        	// n_buffer is delta C, JJ is g compared to Mark's code
            for (c = 0; c < n; c++) {
            	n_buffer[c] = 0.0;
            	for (r = 0; r < m; r++) {
            		n_buffer[c] += (J[r][c]*residuals[r]);
            	}
            }
            for (c = 0; c < n; c++) {
            	n_buffer[c] *= -1;
            }
            double JJinv[][] = null;
            try {
                JJinv = ((new Matrix(JJ)).inverse()).getArray();
            }
            catch (Exception e) {
            	System.err.println("Matrix JJ is singular");
            	Preferences.debug("Matrix JJ is singular\n", Preferences.DEBUG_ALGORITHM);
            	return;
            }
            for (r = 0; r < n; r++) {
            	v[r] = 0.0;
            	for (c = 0; c < n; c++) {
            		v[r] += (JJinv[r][c]*n_buffer[c]);
            	}
            }
            
            if (geodesicAcceleration) {
                double hessian[][][] = new double[n][n][m];
                if (testMode) {
                	fitToTestHessian(x, hessian);
                }
                else {
                	fitToHessian(x, hessian);
                }
                double tmp[] = new double[n];
                for (i = 0; i < m; i++) {
                    for (r = 0; r < n; r++) {
                    	tmp[r] = 0;
                    	for (c = 0; c < n; c++) {
                    	    tmp[r] += (hessian[r][c][i]*v[c]);	
                    	}	
                    }
                    dir_deriv[i] = 0.0;
                    for (r = 0; r < n; r++) {
                    	dir_deriv[i] += (v[r]*tmp[r]);
                    }
                }
                double a1[][] = new double[n][1];
                for (c = 0; c < n; c++) {
                	a1[c][0] = 0.0;
                	for (r = 0; r < m; r++) {
                		a1[c][0] += (J[r][c]*dir_deriv[r]);
                	}
                }
                for (i = 0; i < n; i++) {
                	a1[i][0] *= -1;
                }
                GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
                int info[] = new int[1];
                ge.dpotrf('U',n,JJ,n,info); // cholesky decomposition
                if (info[0] < 0) {
                	System.err.println("In ge.dpotrf argument " + (-i) + " had an illegal value");
                	Preferences.debug("In ge.dpotrf argument " + (-i) + " had an illegal value\n", Preferences.DEBUG_ALGORITHM);
                	return;
                }
                if (info[0] > 0) {
                	System.err.println("in ge.dpotrf the leading minor of order i is not positive definite, and the factorization could not be completed");
                	Preferences.debug("in ge.dpotrf the leading minor of order i is not positive definite, and the factorization could not be completed\n",
                			Preferences.DEBUG_ALGORITHM);
                	return;
                }
                LinearEquations le = new LinearEquations();
                le.dpotrs('U',n,1,JJ,n,a1,n,info); // divides a by JJ, taking into account the fact that JJ is now the `U` cholesky decoposition of what it was before
                if (info[0] < 0) {
                	System.err.println("In le.dpotrs argument " + (-i) + " had an illegal value");
                	Preferences.debug("In le.dpotrs argument " + (-i) + " had an illegal value\n", Preferences.DEBUG_ALGORITHM);
                	return;
                }
                for (i = 0; i < n; i++) {
                	a1[i][0] *= 0.5;
                	 delta_x[i] = v[i] + a1[i][0];
                }
            } // if (geodesicAcceleration)
            else {
                delta_x = v;
            }

            // apply box constraints
            if (lower != null) {
                for (i = 0; i < n; i++) {
                   delta_x[i] = Math.max(x[i] + delta_x[i], lower[i]) - x[i];
                }
            }
            if (upper != null) {
                for (i = 0; i < n; i++) {
                   delta_x[i] = Math.min(x[i] + delta_x[i], upper[i]) - x[i];
                }
            }
            
            // if the linear assumption is valid, our new residual should be:
            for (r = 0; r < m; r++) {
            	Jdelta_buffer[r] = 0.0;
            	for (c = 0; c < n; c++) {
            		Jdelta_buffer[r] += (J[r][c]*delta_x[c]);
            	}
            }
            for (r = 0; r < m; r++) {
                Jdelta_buffer[r] = Jdelta_buffer[r] + residuals[r];
            }
            double predicted_residual = 0.0;
            for (r = 0; r < m; r++) {
            	predicted_residual += (Jdelta_buffer[r]*Jdelta_buffer[r]);
            }
            
            // try the step and compute its quality
            // compute it inplace according to NLSolversBase value(obj, cache, state)
            // interface. No bang (!) because it doesn't update df besides mutating
            // the number of f_calls

            // re-use n_buffer
            for (i = 0; i < n; i++) {
                n_buffer[i] = x[i] + delta_x[i];
            }
            f_calls += 1;
            if (testMode) {
            	fitToTestFunction(n_buffer, trial_f);
            }
            else {
            	fitToFunction(n_buffer, trial_f);
            }	

            // update the sum of squares
            double trial_residual = 0.0;
            for (i = 0; i < m; i++) {
            	trial_residual += (trial_f[i]*trial_f[i]);
            }

            // step quality = residual change / predicted residual change
            double rho = (trial_residual - residual) / (predicted_residual - residual);
            if (rho > min_step_quality) {
                // apply the step to x - n_buffer is ready to be used by the delta_x
                // calculations after this step.
            	for (i = 0; i < n; i++) {
            		x[i] = n_buffer[i];
            	}
                // There should be an update_x_value to do this safely
            	for (i = 0; i < n; i++) {
            		x_f[i] = x[i];
            	}
                for (i = 0; i < m; i++) {
                	residuals[i] = trial_f[i];
                }
                residual = trial_residual;
                if (rho > good_step_quality) {
                    // increase trust region radius
                    lambda = Math.max(lambda_decrease*lambda, MIN_LAMBDA);
                }
            }
            else {
                // decrease trust region radius
                lambda = Math.min(lambda_increase*lambda, MAX_LAMBDA);
            }

            iterCt += 1;
            
            // show state
            if (show_trace) {
            	Preferences.debug("iterCt = " + iterCt + "\n", Preferences.DEBUG_ALGORITHM);
            	for (c = 0; c < n; c++) {
                 	Jres[c] = 0.0;
                 	for (r = 0; r < m; r++) {
                 		Jres[c] += (J[r][c]*residuals[r]);
                 	}
                }
            	double g_norm = 0.0;
            	for (c = 0; c < n; c++) {
            		if (Math.abs(Jres[c]) > g_norm) {
            			g_norm = Math.abs(Jres[c]);
            		}
            	}
                Preferences.debug("g_norm = " + g_norm + "\n", Preferences.DEBUG_ALGORITHM);
                for (i = 0; i < n; i++) {
                	Preferences.debug("delta_x["+i+"] = " + delta_x[i] + "\n", Preferences.DEBUG_ALGORITHM);
                }
                Preferences.debug("lambda = " + lambda + "\n", Preferences.DEBUG_ALGORITHM);
                residual = 0;
                for (i = 0; i < m; i++) {
                	residual += (residuals[i]*residuals[i]);
                }
                Preferences.debug("residual = " + residual + "\n", Preferences.DEBUG_ALGORITHM);
            }
            
            // check convergence criteria:
            // 1. Small gradient: norm(J^T * value(df), Inf) < g_tol
            // 2. Small step size: norm(delta_x) < x_tol
            for (c = 0; c < n; c++) {
             	Jres[c] = 0.0;
             	for (r = 0; r < m; r++) {
             		Jres[c] += (J[r][c]*residuals[r]);
             	}
            }
            double norm = 0.0;
            for (c = 0; c < n; c++) {
        		if (Math.abs(Jres[c]) > norm) {
        			norm = Math.abs(Jres[c]);
        		}
        	}
            if (norm < g_tol) {
                g_converged = true;
            }
            double delta_x_norm = 0.0;
            for (i = 0; i < n; i++) {
            	delta_x_norm += (delta_x[i]*delta_x[i]);
            }
            delta_x_norm = Math.sqrt(delta_x_norm);
            double x_norm = 0.0;
            for (i = 0; i < n; i++) {
            	x_norm += (x[i]*x[i]);
            }
            x_norm = Math.sqrt(x_norm);
            if (delta_x_norm < x_tol*(x_tol + x_norm)) {
                x_converged = true;
            }
            converged = g_converged || x_converged;

        } // while ((!converged) && iterCt < maxIter)
    	
    }
    
    
    public int getIterations() {
    	return iterCt;
    }

    /**
     * getParameters accessor to function parameters.
     *
     * @return  the function parameters determined by the algorithm
     */
    public double[] getParameters() {
        return x;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  double[] residuals
     */
    public double[] getResiduals() {
        return residuals;
    }
    
    public boolean getConverged() {
    	return converged;
    }

	
}