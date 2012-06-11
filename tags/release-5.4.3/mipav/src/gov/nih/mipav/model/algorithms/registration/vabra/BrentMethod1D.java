package gov.nih.mipav.model.algorithms.registration.vabra;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Optimizes functions by using Brent's method. It will interpolate a parabola from 3 points
 * when the function is behaving nicely. The minimum of the parabola replaces the worst point, 
 * and a new parabola is created. This continues until the 3 points converge.
 * It will then switch to the more robust method of golden section search when things are not nice. 
 * Adapted from Numerical Recipes.
 * @author Hanlin Wan
 */
public class BrentMethod1D implements Optimizer1DContinuous {

	private double extrema, higher, lower, tol;
	private String statusMessage;
	private Optimizable1DContinuous myFunction;
	private int maxIterations, iterations;
	private int maxSteps;
	private boolean logOn = false;
	private ArrayList<double[]> logData;

	/**
	 * Default constructor.
	 */
	public BrentMethod1D() {
		maxIterations = 1000;
		extrema = Double.NaN;
		myFunction = null;
	}
	
	
	/**
	 * Constructor to set maximum number of iterations to run
	 * @param maxIter maximum number of iterations
	 */
	public BrentMethod1D(int maxIter) {
		maxIterations = maxIter;
		extrema = Double.NaN;
		myFunction = null;
	}
	
	/**
	 * Turns on logging for each iteration of the optimization.
	 * @param turnOn true if logging, false if not
	 * @param maxSteps maximum number of steps to log
	 */
	public void logData(boolean turnOn, int maxSteps) {
		logOn = turnOn;
		this.maxSteps = maxSteps;
		logData = new ArrayList<double[]>();
	}
	
	/**
	 * Gets the logged data
	 * @return array of logged data
	 */
	public ArrayList<double[]> getLog() {
		return logData;
	}
	
	/**
	 * Gets the extrema that was found
	 * @return the location of where the extrema is
	 */
	public double getExtrema() {
		return extrema;
	}

	/**
	 * Gets the number of iterations done.
	 * @return number of iterations
	 */
	public int getIterations() {
		return iterations-1;
	}
	
	/**
	 * Initialize the optimizer with the 1D function.
	 * @param function your Optimizable1DContinuous function
	 */
	public void initialize(Optimizable1DContinuous function) {
		myFunction = function; 
		lower = function.getDomainMin();
		higher = function.getDomainMax();
		tol = function.getDomainTolerance();
		extrema = Double.NaN;
	}

	public void initialize(Optimizable1DContinuous function,double init) {
		myFunction = function; 
		lower = function.getDomainMin();
		higher = function.getDomainMax();
		tol = function.getDomainTolerance();
		extrema = init;
	}
	/**
	 * Optimizes the function
	 * @param findMinima true if you want to find the minimum, false if you want to find the maximum
	 * @return true if extrema was found successfully, false otherwise
	 */
	public boolean optimize(boolean findMinima) {
		if (myFunction == null)  {
			statusMessage = "Not initialized.";
			return false;			
		}
		
		if (!findMinima) myFunction = new MinToMax1D(myFunction);

	    double d, e, m, p, q, r, t2, u, v, w, fu, fv, fw, fx;
	    double c = 0.5*(3.0 - Math.sqrt(5.0));
	    double eps = 1.5e-8;
	    if(Double.isNaN(extrema)) {
	    	v = w = extrema = lower + c*(higher - lower); d = e = 0.0;
	    } else{
	    	v = w = extrema;
	    	d = e = 0.0;
	    }
	    	
	    fv = fw = fx = myFunction.getValue(extrema);
		iterations = 0;

		while (iterations<maxIterations) {
		    m = 0.5*(lower + higher);
		    tol += eps*Math.abs(extrema);
		    t2 = 2.0*tol;
		    // Check stopping criteria
		    if (Math.abs(extrema - m) < t2 - 0.5*(higher - lower)) 
		    	break;
		    else {
		        if (Math.abs(e) > tol) {
		            // fit parabola
		            r = (extrema - w)*(fx - fv);
		            q = (extrema - v)*(fx - fw);
		            p = (extrema - v)*q - (extrema - w)*r;
		            q = 2.0*(q - r);
		            if (q > 0.0) p = -p;
		            q = Math.abs(q);
		            r = e;
		            e = d;
			        if (Math.abs(p) >= Math.abs(0.5*q*r) || p <= q*(lower - extrema) || p >= q*(higher - extrema)) {
			        	e = (extrema > m) ? lower - extrema : higher - extrema;
			        	d = c*e;
			        }
			        else {
			        	// A parabolic interpolation step
			            d = p/q;
			            u = extrema + d;
			            // f must not be evaluated too close to a or b
			            if (u - lower < t2 || higher - u < t2)
			                d = (extrema < m) ? tol : -tol;
			            // logging
			            if (logOn && iterations<maxSteps) {
			            	double[] temp = new double[]{v,extrema,w};
			            	Arrays.sort(temp);
			            	double[] log = new double[5];
			            	log[0]=1;
			            	log[4]=u;
			            	System.arraycopy(temp, 0, log, 1, 3);
			            	logData.add(log);
			            }
			        }
		        }
		        else {
		            // A golden section step
		            e = (extrema >= m) ? lower - extrema : higher - extrema;
		            d = c*e;
		        }
		        // f must not be evaluated too close to x
		        if (Math.abs(d) >= tol)
		            u = extrema + d;
		        else if (d > 0.0)
		            u = extrema + tol;
		        else
		            u = extrema - tol;
		        fu = myFunction.getValue(u);
		        if (logOn && iterations<maxSteps) {
		        	try {
		        		logData.get(iterations);
		        	}
		        	catch (IndexOutOfBoundsException ioobe) {
		        		if (u<extrema)
		        			logData.add(new double[]{0,lower,u,extrema,higher});
		        		else
		        			logData.add(new double[]{0,lower,extrema,u,higher});
		        	}
		        }
		        // Update variables
		        if (fu <= fx) {
		            if (u < extrema) higher = extrema;
		            else lower = extrema;
		            v = w; fv = fw; 
		            w = extrema; fw = fx; 
		            extrema = u; fx = fu;
		        }
		        else {
		            if (u < extrema) lower = u;
		            else higher = u;
		            if (fu <= fw || w == extrema) {
		                v = w; fv = fw; 
		                w = u; fw = fu;
		            }
		            else if (fu <= fv || v == extrema || v == w) {
		                v = u; fv = fu;
		            }
		        }
			    iterations++;
		    }
	    }

		if (iterations == maxIterations){
			statusMessage = "Maximum iterations reached.";
			return false;
		}
			
		statusMessage = "Convergence reached";
		return true;	
	}

	/**
	 * Gets the status message from the optimizing process
	 * @return string of the status message
	 */
	public String statusMessage() {
		return statusMessage;
	}

}
