package gov.nih.mipav.model.algorithms;

import Jama.Matrix;

/**
LsqFit.jl is licensed under the MIT License:

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
	private boolean testMode = false;
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
    
    /** integer scalar containing the number of data points. */
    protected int m; 

    /** variables integer scalar containing the number of unknowns. */
    protected int n; // x.length
	
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
    
    private void levenberg_marquardt(double initial_x[], double tdata[], double ydata[]) {
    	// tdata independent variable of length m
    	// ydata measured dependent variable of length m
    	int i, r, c;
    	// int n = initial_x.length;
    	int f_calls = 0;
    	int df_calls = 0;
    	double x_f[] = new double[n];
    	double x_df[] = new double[n];
    	double residuals[] = new double[m]; // model[i] - ydata[i]
    	
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
        
        boolean converged = false;
        boolean x_converged = false;
        boolean g_converged = false;
        int iterCt = 0;
        double x[] = new double[n];
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
        double JJ[][] = new double[n][n];
        double n_buffer[] = new double[n];
        double Jdelta_buffer[] = new double[m];
        double dir_deriv[] = new double[m];
        double v[] = new double[n];
    	
    }
    
    private void fitToTestJacobian(double x[], double J[][]) {
    	
    }
    
    private void fitToTestFunction(double x[], double residuals[]) {
    	
    }

	
}