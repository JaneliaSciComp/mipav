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
	protected int maxIter = 1000; // maximum number of iterations
    protected double min_step_quality = 1e-3; // for steps below this quality, the trust region is shrinked
    protected double good_step_quality = 0.75; // for steps above this quality, the trust region is expanded
	protected double lambda = 10; // (inverse of) initial trust region radius
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
    
    private final int BARD = 8;
    
    private final int HOCK25 = 25;
    
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
        // geodesicAcceleration = false does not converge and geodesicAcceleration = true converges to incorrect values
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
        // BARD unconstrained converges to correct value for geodesicAcceleration = false in 6 iterations;
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
        // BARD constrained at 10 * standard starting point converges to correct value for geodesicAcceleration = false in 27 iterations;
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
        // BARD constrained at 100 * standard starting point converges to correct value for geodesicAcceleration = false in 12 iterations;
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
        
    }
    
    private void dumpTestResults() {
    	 int i;
    	 Preferences.debug("Number of iterations: " + String.valueOf(iterCt) + "\n", Preferences.DEBUG_ALGORITHM);
         for (i = 0; i < n; i++) {
             Preferences.debug("x[" + i + "] = " + String.valueOf(x[i]) + "\n", Preferences.DEBUG_ALGORITHM);
         }
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
            } // switch (testCase)
        } catch (Exception e) {
            Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
        }

        return;
    }
    
    private void fitToTestJacobian(double x[], double J[][]) {
        int i;
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
            	double denom;
                    // Calculate the Jacobian analytically
                    for (i = 0; i < m; i++) {
                    	denom = (x[1]*(16.0 - tdata[i]) + x[2]*Math.min(tdata[i], 16.0 - tdata[i]));
                        J[i][0] = 1.0;
                        J[i][1] = -tdata[i]*(16.0 - tdata[i])/(denom*denom);
                        J[i][2] = -tdata[i]*Math.min(tdata[i], 16.0 - tdata[i])/(denom*denom);
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
        	return;
        }
        if ((upper != null) && (upper.length != n)) {
        	System.err.println("upper must either be null or have a length equal to the number of parameters");
        	return;
        }
        if (lower != null) {
        	for (i = 0; i < n; i++) {
        		if (initial_x[i] < lower[i]) {
        			System.err.println("initial_x["+i+"] must be >= lower["+i+"]");
        			return;
        		}
        	}
        	
        }
        if (upper != null) {
        	for (i = 0; i < n; i++) {
        		if (initial_x[i] > upper[i]) {
        			System.err.println("initial_x["+i+"] must be <= upper["+i+"]");
        			return;
        		}
        	}
        	
        }
        
        if (min_step_quality < 0) {
        	System.err.println("min_step_quality must be >= 0");
        	return;
        }
        if (min_step_quality >= 1) {
        	System.err.println("min_step_quality must be < 1");
        	return;
        }
        
        if (good_step_quality <= 0) {
        	System.err.println("good_step_quality must be > 0");
        	return; 
        }
        if (good_step_quality > 1) {
        	System.err.println("good_step_quality must be <= 1");
        	return;
        }
        if (min_step_quality >= good_step_quality) {
        	System.err.println("min_step_quality must be < good_step_quality");
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
            double JJinv[][] = ((new Matrix(JJ)).inverse()).getArray();
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
                	return;
                }
                if (info[0] > 0) {
                	System.err.println("in ge.dpotrf the leading minor of order i is not positive definite, and the factorization could not be completed");
                	return;
                }
                LinearEquations le = new LinearEquations();
                le.dpotrs('U',n,1,JJ,n,a1,n,info); // divides a by JJ, taking into account the fact that JJ is now the `U` cholesky decoposition of what it was before
                if (info[0] < 0) {
                	System.err.println("In le.dpotrs argument " + (-i) + " had an illegal value");
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