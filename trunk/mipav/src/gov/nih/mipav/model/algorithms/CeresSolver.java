package gov.nih.mipav.model.algorithms;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

import Jama.Matrix;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.GeneralizedInverse2;
import gov.nih.mipav.model.structures.jama.LinearEquations;
import gov.nih.mipav.view.Preferences;

/**
 * This is a port of the C++ files in ceres-solver-1.14.0 under the BSD license:
 Ceres Solver - A fast non-linear least squares minimizer
Copyright 2015 Google Inc. All rights reserved.
http://ceres-solver.org/

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
* Neither the name of Google Inc. nor the names of its contributors may be
  used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

Ceres Solver is an open source C++ library for modeling and solving
large, complicated optimization problems. It is a feature rich, mature
and performant library which has been used in production at Google
since 2010. Ceres Solver can solve two kinds of problems.

1. Non-linear Least Squares problems with bounds constraints.
2. General unconstrained optimization problems.

Please see [ceres-solver.org](http://ceres-solver.org/) for more
information.



 * @author aailb
 *
 */

public abstract class CeresSolver {
	private boolean testMode = false;
	double epsilon = 2.2204460e-16;
	double default_relstep = Math.pow(epsilon,1.0/3.0);
	/** integer scalar containing the number of data points. */
    protected int m; 
    private int num_residuals_;

    /** variables integer scalar containing the number of unknowns. */
    protected int N0; // x.length
	// For SizedCostFunction and AutoDiffCostFunction, DYNAMIC can be
	// specified for the number of residuals. If specified, then the
	// number of residuas for that cost function can vary at runtime.
	//enum DimensionType {
	  private int DYNAMIC = -1;
	//};
	  
	// Argument type used in interfaces that can optionally take ownership
	// of a passed in argument. If TAKE_OWNERSHIP is passed, the called
	// object takes ownership of the pointer argument, and will call
	// delete on it upon completion.
	enum Ownership {
	  DO_NOT_TAKE_OWNERSHIP,
	  TAKE_OWNERSHIP
	};
	  
	  private void runExample() {
		double x[] = new double[] {0.5};
		// auto-differentiation to obtain the derivative (jacobian).
		  CostFunction cost_function =
		      new AutoDiffCostFunction(1, 1,0,0,0,0,0,0,0,0,0);
		  Problem problem = new Problem();
		  problem.AddResidualBlock(cost_function, null, x);

	  }
	  
	// A templated cost functor that implements the residual r = 10 -
	// x. The method operator() is templated so that we can then use an
	// automatic differentiation wrapper around it to generate its
	// derivatives.
	public abstract void fitToFunction(double[] x, double[] residuals);
	
    public abstract void fitToJacobian(double[] x, double[][] jacobian);
    
    public abstract void fitToHessian(double[] x, double[][][] hessian);
    
    public void fitToTestFunction(double[] x, double[] residuals) {
    	
    }
    
    private void fitToTestJacobian(double x[], double J[][]) {
    	
    }
    
    private void fitToTestHessian(double x[], double hessian[][][]) {
    	
    }
    
    private void fitToNumericalJacobian(double xinit[], double[][] jacobian) {
    	int i, j;
        double relstep = default_relstep;
        double absstep = relstep;
        double residualsplus[] = new double[num_residuals_];
        double residualsminus[] = new double[num_residuals_];
        double eps;
        double x[] = new double[xinit.length];
        for (i = 0; i < N0; i++) {
        	x[i] = xinit[i];
        }
        for (j = 0; j < N0; j++) {
    		eps = Math.max(relstep*Math.abs(x[j]), absstep);
    		x[j] = xinit[j] + eps;
    		if (testMode) {
    		    fitToTestFunction(x, residualsplus)	;
    		}
    		else {
    		    fitToFunction(x, residualsplus);	
    		}
    		x[j] = xinit[j] - eps;
    		if (testMode) {
    			fitToTestFunction(x, residualsminus);
    		}
    		else {
    			fitToFunction(x, residualsminus);
    		}
    		x[j] = xinit[j];
    		for (i = 0; i < num_residuals_; i++) {
    			jacobian[i][j] = (residualsplus[i] - residualsminus[i])/(2.0 * eps);
    		}
        }
        
    }
    
    private void fitToNumericalHessian(double xinit[], double hessian[][][]) {
    	int i, j,k;
        double relstep = default_relstep;
        double absstep = relstep;
        double residualsxpp[] = new double[num_residuals_];
        double residualsxpm[] = new double[num_residuals_];
        double residualsxmm[] = new double[num_residuals_];
        double residualsxmp[] = new double[num_residuals_];
        double residualsx[] = new double[num_residuals_];
        double epsi;
        double epsj;
        double x[] = new double[xinit.length];
        for (i = 0; i < N0; i++) {
        	x[i] = xinit[i];
        }
        for (i = 0; i < N0; i++) {
        	epsi = Math.max(relstep*Math.abs(x[i]), absstep);
    		x[i] = xinit[i] + epsi;
    		if (testMode) {
    		    fitToTestFunction(x, residualsxpp)	;
    		}
    		else {
    		    fitToFunction(x, residualsxpp);	
    		}
    		x[i] = xinit[i] - epsi;
    		if (testMode) {
    			fitToTestFunction(x, residualsxmm);
    		}
    		else {
    			fitToFunction(x, residualsxmm);
    		}
    		x[i] = xinit[i];
    		if (testMode) {
    			fitToTestFunction(x, residualsx);
    		}
    		else {
    			fitToFunction(x, residualsx);
    		}
    		for (k = 0; k < num_residuals_; k++) {
    			hessian[i][i][k] = (residualsxpp[k] - 2.0*residualsx[k] + residualsxmm[k])/(epsi*epsi);
    		}
    		for (j = i+1; j < N0; j++) {
    			epsj = Math.max(relstep*Math.abs(x[j]), absstep);
    			x[i] = xinit[i] + epsi;
    			x[j] = xinit[j] + epsj;
    			if (testMode) {
        		    fitToTestFunction(x, residualsxpp)	;
        		}
        		else {
        		    fitToFunction(x, residualsxpp);	
        		}
    			x[j] = xinit[j] - epsj;
    			if (testMode) {
        		    fitToTestFunction(x, residualsxpm)	;
        		}
        		else {
        		    fitToFunction(x, residualsxpm);	
        		}
    			x[i] = xinit[i] - epsi;
    			if (testMode) {
        		    fitToTestFunction(x, residualsxmm)	;
        		}
        		else {
        		    fitToFunction(x, residualsxmm);	
        		}
    			x[j] = xinit[j] + epsj;
    			if (testMode) {
        		    fitToTestFunction(x, residualsxmp)	;
        		}
        		else {
        		    fitToFunction(x, residualsxmp);	
        		}
    			x[i] = xinit[i];
    			x[j] = xinit[j];
    			for (k = 0; k < num_residuals_; k++) {
        			hessian[i][j][k] = (residualsxpp[k] - residualsxpm[k] + - residualsxmp[k] + residualsxmm[k])/(4.0*epsi*epsj);
        			hessian[j][i][k] = hessian[i][j][k];
        		}
    		}
        }
    }

	class CostFunction {
		//private int num_residuals_;
		private Vector<Integer>parameter_block_sizes_;
		public CostFunction() {
		    num_residuals_ = 0;	
		    parameter_block_sizes_ = new Vector<Integer>();
		}
		
		public Vector<Integer> parameter_block_sizes() {
		    return parameter_block_sizes_;
		}
		
		public int num_residuals() {
		    return num_residuals_;
		}
		
		protected Vector<Integer> mutable_parameter_block_sizes() {
		    return parameter_block_sizes_;
		}
		
		protected void set_num_residuals(int num_residuals) {
		    num_residuals_ = num_residuals;
		}

	}
	
	class SizedCostFunction extends CostFunction {
		
	    public SizedCostFunction(int kNumResiduals, int N0, int N1, int N2, int N3, int N4, int N5, int N6, int N7, int N8, int N9,
	    		int num_residuals) {
			    if (kNumResiduals != DYNAMIC) {
			       System.err.println("Cost functions must have at least one residual block.");
			       return;
			    }

			    if(!(((N1 == 0) && (N2 == 0) && (N3 == 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0) && (N9 == 0)) ||
			          ((N1 > 0) && (N2 == 0) && (N3 == 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0) && (N9 == 0)) ||
			          ((N1 > 0) && (N2 > 0) && (N3 == 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0) && (N9== 0)) ||         
			          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0) && (N9 == 0)) ||                  
			          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0) && (N9 == 0)) ||         
			          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 == 0) && (N7 == 0) && (N8 == 0) && (N9 == 0)) ||          
			          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 == 0)&& (N8 == 0) && (N9 == 0)) ||      
			          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 > 0) && (N8 == 0) && (N9 == 0)) ||        
			          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 > 0) && (N8 > 0) && (N9 == 0)) ||   
			          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 > 0) && (N8 > 0) && (N9 > 0)))) { 
			        System.err.println("Zero block cannot precede a non-zero block. Block sizes are ");
			        System.err.println ("(ignore trailing 0s): " + N0 + ", " + N1 + ", " + N2 + ", ");
			        System.err.println(N3 + ", " + N4 + ", " + N5 + ", " + N6 + ", " + N7 + ", ");
			        System.err.println(N8 + ", " + N9);
			        return;
			    }

			    set_num_residuals(num_residuals);
			    
			    if (N0 > 0) mutable_parameter_block_sizes().add(N0);
			    if (N1 > 0) mutable_parameter_block_sizes().add(N1);
			    if (N2 > 0) mutable_parameter_block_sizes().add(N2);
			    if (N3 > 0) mutable_parameter_block_sizes().add(N3);
			    if (N4 > 0) mutable_parameter_block_sizes().add(N4);
			    if (N5 > 0) mutable_parameter_block_sizes().add(N5);
			    if (N6 > 0) mutable_parameter_block_sizes().add(N6);
			    if (N7 > 0) mutable_parameter_block_sizes().add(N7);
			    if (N8 > 0) mutable_parameter_block_sizes().add(N8);
			    if (N9 > 0) mutable_parameter_block_sizes().add(N9);
	}
			    
			    public SizedCostFunction(int kNumResiduals, int N0, int N1, int N2, int N3, int N4, int N5, int N6, int N7, int N8, int N9) {
					    if (kNumResiduals <= 0) {
					       System.err.println("Cost functions must have at least one residual block.");
					       return;
					    }

					    if(!(((N1 == 0) && (N2 == 0) && (N3 == 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0) && (N9 == 0)) ||
					          ((N1 > 0) && (N2 == 0) && (N3 == 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0) && (N9 == 0)) ||
					          ((N1 > 0) && (N2 > 0) && (N3 == 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0) && (N9== 0)) ||         
					          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0) && (N9 == 0)) ||                  
					          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0) && (N9 == 0)) ||         
					          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 == 0) && (N7 == 0) && (N8 == 0) && (N9 == 0)) ||          
					          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 == 0)&& (N8 == 0) && (N9 == 0)) ||      
					          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 > 0) && (N8 == 0) && (N9 == 0)) ||        
					          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 > 0) && (N8 > 0) && (N9 == 0)) ||   
					          ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 > 0) && (N8 > 0) && (N9 > 0)))) { 
					        System.err.println("Zero block cannot precede a non-zero block. Block sizes are ");
					        System.err.println ("(ignore trailing 0s): " + N0 + ", " + N1 + ", " + N2 + ", ");
					        System.err.println(N3 + ", " + N4 + ", " + N5 + ", " + N6 + ", " + N7 + ", ");
					        System.err.println(N8 + ", " + N9);

					    set_num_residuals(kNumResiduals);
					    
					    if (N0 > 0) mutable_parameter_block_sizes().add(N0);
					    if (N1 > 0) mutable_parameter_block_sizes().add(N1);
					    if (N2 > 0) mutable_parameter_block_sizes().add(N2);
					    if (N3 > 0) mutable_parameter_block_sizes().add(N3);
					    if (N4 > 0) mutable_parameter_block_sizes().add(N4);
					    if (N5 > 0) mutable_parameter_block_sizes().add(N5);
					    if (N6 > 0) mutable_parameter_block_sizes().add(N6);
					    if (N7 > 0) mutable_parameter_block_sizes().add(N7);
					    if (N8 > 0) mutable_parameter_block_sizes().add(N8);
					    if (N9 > 0) mutable_parameter_block_sizes().add(N9);
			}
	    }
	}
	
	class AutoDiffCostFunction extends SizedCostFunction {
		//private CostFunctor functor_;
		
		// Takes ownership of functor. Uses the template-provided value for the
		// number of residuals ("kNumResiduals").
		public AutoDiffCostFunction(int kNumResiduals, int N0, int N1, int N2, int N3, int N4,
				int N5, int N6, int N7, int N8, int N9) {
			super(kNumResiduals, N0, N1, N2, N3,
		            N4, N5, N6, N7, N8, N9);
			//this.functor_ = functor;
			if (kNumResiduals == DYNAMIC) {
				System.err.println("Can't run the fixed-size constructor if the number of residuals is set to DYNAMIC.");
			}
		}
		
		  // Takes ownership of functor. Ignores the template-provided
		  // kNumResiduals in favor of the "num_residuals" argument provided.
		  //
		  // This allows for having autodiff cost functions which return varying
		  // numbers of residuals at runtime.
		  public AutoDiffCostFunction(int kNumResiduals, int N0, int N1, int N2, int N3, int N4,
					int N5, int N6, int N7, int N8, int N9, int num_residuals) {
			  super(kNumResiduals, N0, N1, N2, N3,
			            N4, N5, N6, N7, N8, N9, num_residuals);
			//this.functor_ = functor;
		    if (kNumResiduals != DYNAMIC) {
		        System.err.println("Can't run the dynamic-size constructor if the number of residuals is not DYNAMIC.");
		        return;
		    }
		    
		  }

	}
	
	class LossFunction {
		public LossFunction() {
			
		}
	}
	
	class Problem extends ProblemImpl {
		public Problem() {
		    super();	
		}
		
		
	}
	
	class ProblemImpl {
		private Vector<double[]>residual_parameters_;
		protected Options options_;
		public ProblemImpl() {
		    residual_parameters_ = new Vector<double[]>(10);
		    options_ = new Options();
		}
		
		
		public ResidualBlock[] AddResidualBlock(CostFunction cost_function, LossFunction loss_function, double x0[]) {
			  residual_parameters_.clear();
			  residual_parameters_.add(x0);
			  return AddResidualBlock(cost_function, loss_function, residual_parameters_);
		}
		
		public ResidualBlock[] AddResidualBlock(CostFunction cost_function, LossFunction loss_function, Vector<double[]> parameter_blocks) {
			int i,j;
			if (cost_function == null) {
				System.err.println("cost_function is null in AddResidualBlock");
				return null;
			}
			if (parameter_blocks.size() != cost_function.parameter_block_sizes().size()) {
				System.err.println("parameters_blocks.size() != cost_function.parameter_block_sizes().size() in AddResidualBlock");
				return null;
			}
			
			// Check the sizes match.
			Vector<Integer> parameter_block_sizes =
			      cost_function.parameter_block_sizes();
			
			if (!options_.disable_all_safety_checks) {
			    if (parameter_block_sizes.size() != parameter_blocks.size()) {
			        System.err.println("Number of blocks input is different than the number of blocks ");
			        System.err.println("that the cost function expects in AddResidualBlock");
			        return null;
			    }

			    // Check for duplicate parameter blocks.
			    double sorted_parameter_blocks[][] = new double[parameter_blocks.size()][];
			    int indexArray[] = new int[parameter_blocks.size()];
			    ArrayList<indexBlockSize> indexBlockSizeArrayList = new ArrayList<indexBlockSize>();
            	for (i = 0; i < parameter_blocks.size(); i++) {
            		indexBlockSizeArrayList.add(new indexBlockSize(i, parameter_blocks.get(i).length));
            	}
            	Collections.sort(indexBlockSizeArrayList, new indexBlockSizeComparator());
            	for (i = 0; i < indexBlockSizeArrayList.size(); i++) {
            		indexBlockSize iBS = indexBlockSizeArrayList.get(i);
            		int index = iBS.getIndex();
            		indexArray[i] = index;
            		sorted_parameter_blocks[i] = parameter_blocks.get(index).clone();
            		Arrays.sort(sorted_parameter_blocks[i]);
            	}
            	boolean foundEquals = false;
            	for (i = 1; i < sorted_parameter_blocks.length; i++) {
            		if (sorted_parameter_blocks[i].length == sorted_parameter_blocks[i-1].length) {
            		    boolean equal = true;
            		    for (j = 0; j < sorted_parameter_blocks[i].length; j++) {
            		    	if (sorted_parameter_blocks[i][j] != sorted_parameter_blocks[i-1][j]) {
            		    		equal = false;
            		    	}
            		    }
            		    if (equal) {
            		    	System.err.println("Duplicate parameter blocks in a residual parameter are not allowed.");
            		    	System.err.println("Parameter blocks " + indexArray[i-1] + " and " + indexArray[i] + " are duplicates.");
            		    }
            		}
            	}
            	if (foundEquals) {
            		return null;
            	}
			  }
              return null; // temporary place holder

	
		}
		
		class ResidualBlock {
			public ResidualBlock() {
				
			}
		}
		
		class Options {
			// These flags control whether the Problem object owns the cost
		    // functions, loss functions, and parameterizations passed into
		    // the Problem. If set to TAKE_OWNERSHIP, then the problem object
		    // will delete the corresponding cost or loss functions on
		    // destruction. The destructor is careful to delete the pointers
		    // only once, since sharing cost/loss/parameterizations is
		    // allowed.
		    public Ownership cost_function_ownership;
		    public Ownership loss_function_ownership;
		    public Ownership local_parameterization_ownership;
		    
		    // The increase in memory usage is twofold: an additonal hash set per
		    // parameter block containing all the residuals that depend on the parameter
		    // block; and a hash set in the problem containing all residuals.
		    public boolean enable_fast_removal;

		    // By default, Ceres performs a variety of safety checks when constructing
		    // the problem. There is a small but measurable performance penalty to
		    // these checks, typically around 5% of construction time. If you are sure
		    // your problem construction is correct, and 5% of the problem construction
		    // time is truly an overhead you want to avoid, then you can set
		    // disable_all_safety_checks to true.
		    //
		    // WARNING: Do not set this to true, unless you are absolutely sure of what
		    // you are doing.
		    public boolean disable_all_safety_checks;

		    // A Ceres global context to use for solving this problem. This may help to
		    // reduce computation time as Ceres can reuse expensive objects to create.
		    // The context object can be NULL, in which case Ceres may create one.
		    //
		    // Ceres does NOT take ownership of the pointer.
		    public Context context;
			public Options() {
				cost_function_ownership = Ownership.TAKE_OWNERSHIP;
		        loss_function_ownership = Ownership.TAKE_OWNERSHIP;
		        local_parameterization_ownership = Ownership.TAKE_OWNERSHIP;
		        enable_fast_removal = false;
		        disable_all_safety_checks = false;
		        context = null;	
			}
		}
		
		class Context {
			public Context() {
				
			}
		}
		
		
	}
	
	private class indexBlockSizeComparator implements Comparator<indexBlockSize> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(indexBlockSize o1, indexBlockSize o2) {
        	int a = o1.getBlockSize();
            int b = o2.getBlockSize();
            if (a < b) {
            	return -1;
            }
            else if (a > b) {
            	return 1;
            }
            else {
            	return 0;
            }
        }
	}
        
    private class indexBlockSize {
		private int index;
		private int blockSize;
		
		public indexBlockSize(int index, int blockSize) {
			this.index = index;
			this.blockSize = blockSize;
		}
		
		public int getIndex() {
			return index;
		}
		
		public int getBlockSize() {
			return blockSize;
		}
		
		
	}
	
}



