package gov.nih.mipav.model.algorithms;

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
    protected int nPts;

    /** variables integer scalar containing the number of unknowns. */
    protected int param;
	
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
    
    private void levenberg_marquardt(double initial_x[]) {
    	
    }

	
}