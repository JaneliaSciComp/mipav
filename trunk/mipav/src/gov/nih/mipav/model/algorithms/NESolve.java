package gov.nih.mipav.model.algorithms;

import java.util.Vector;

import gov.nih.mipav.view.*;

/**
    This is a port of nesolve.m and supporting files coded in MATLAB by Richard T. Behrens in 1988
    NESolve produces the solution to a system of nonlinear equations
    nesolve.m coded in MATLAB by Ricaard T. Behrens, April 1988.
    Revised 11/278/88 JNL
    Hookstep option added 8/21/90 RTB.
    
*/

    public abstract class NESolve {
    	
    	private int NO_SCALING = 0;
    	private int SCALING_WITHOUT_SCALE = 1;
    	private int SCALING_WITH_SCALE = 2;
        private int scaling = NO_SCALING;
    
	    // An initial guess of the solution (starting point for iterations)
    	protected double x0[];
    	
    	// (Optional) A vector whose elements select various algorithmic options
    	// and specify various tolerances
    	// details[0] is not used
    	protected double details[] = new double[17];
    	
    	// (Optional) A set of parameters (constants) which if nonemepty
    	// is paased on to the function and Jacobian
    	protected double fparam[] = null;
    	
    	protected boolean analyticJacobian = false;
    	
    	// Typical values of X (1st column) and F (second column)
    	protected double scale[][] = null;
    	
    	// Outputs
    	// The final approximation of the solution
    	protected double xf[];
    	
    	// Indicates the stopping reason (equals 1 for normal)
    	protected int termcode;
    	
    	// (Optional) Returns the sequence of iterates
    	protected Vector<Double> path = null;
    	
    	protected double btrack[] = null;
    	
    	// Number of function evaluations
    	protected int nofun = 0;
    	
    	// Variables for trust region methods
    	private double trustvars[] = new double[4];
	    
	    public NESolve(double x0[], double fparam[], boolean analyticJacobian, double scale[][], Vector<Double> path,
	    		double btrack[], int scaling) {
	    	this.x0 = x0;
	 	    this.fparam = fparam;
	 	    this.analyticJacobian = analyticJacobian;
	 	    this.scale = scale;
	 	    this.path = path;
	 	    this.btrack = null;
	    }
	    
	    public abstract void fitToFunction();
	    
	    public void driver() {
	    	int i;
	        if (fparam != null) {
	        	details[15] = 1;
	        }
	        if (analyticJacobian) {
	        	details[4] = 1;
	        }
	        if (scaling == SCALING_WITH_SCALE) {
	            scaling = SCALING_WITHOUT_SCALE;	
	        }
	        details[16] = scaling;
	        if (path != null) {
	        	details[14] = 1;
	        }
	        if (details[14] == 1) {
	        	for (i = 0; i < x0.length; i++) {
	        		path.add(x0[i]);
	        	}
	        }
	        if (btrack == null) {
	        	details[1] = 2;
	        }
	    }
    }