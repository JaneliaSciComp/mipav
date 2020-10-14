package gov.nih.mipav.model.algorithms;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.Map;
import java.util.Set;
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
	// It is a near impossibility that user code generates this exact
	// value in normal operation, thus we will use it to fill arrays
	// before passing them to user code. If on return an element of the
	// array still contains this value, we will assume that the user code
	// did not write to that memory location.
	private final double kImpossibleValue = 1e302;
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
		ArrayList<Double>x = new ArrayList<Double>();
		x.add(0.5);
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
	
	class Program {
		// The Program does not own the ParameterBlock or ResidualBlock objects.
		  private Vector<ParameterBlock> parameter_blocks_;
		  private Vector<ResidualBlock> residual_blocks_;	
		  public Program() {
			  parameter_blocks_ = new Vector<ParameterBlock>();
			  residual_blocks_ = new Vector<ResidualBlock>();
		  }
	}
	
	class ProblemImpl {
		private Vector<ArrayList<Double>>residual_parameters_;
		private Program program_;
		// Must ArrayList<Double> rather than the C++ double[] as keys in a TreeMap
		private TreeMap<ArrayList<Double>, ParameterBlock> parameter_block_map_;
		protected Options options_;
		// Iff enable_fast_removal is enabled, contains the current residual blocks.
		private HashSet<ResidualBlock> residual_block_set_;
		private HashMap<CostFunction, Integer> cost_function_ref_count_;
		private HashMap<LossFunction, Integer> loss_function_ref_count_;
		private int count;
		public ProblemImpl() {
		    residual_parameters_ = new Vector<ArrayList<Double>>(10);
		    options_ = new Options();
		    program_ = new Program();
		    parameter_block_map_ = new TreeMap<ArrayList<Double>, ParameterBlock>();
		    residual_block_set_ = new HashSet<ResidualBlock>();
		    cost_function_ref_count_ = new HashMap<CostFunction, Integer>();
		    loss_function_ref_count_ = new HashMap<LossFunction, Integer>();
		}
		
		
		public ResidualBlock AddResidualBlock(CostFunction cost_function, LossFunction loss_function, ArrayList<Double> x0) {
			  residual_parameters_.clear();
			  residual_parameters_.add(x0);
			  return AddResidualBlock(cost_function, loss_function, residual_parameters_);
		}
		
		public ResidualBlock AddResidualBlock(CostFunction cost_function, LossFunction loss_function, Vector<ArrayList<Double>> parameter_blocks) {
			int i,j;
			int arraySize;
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
			}

			    // Check for duplicate parameter blocks.
			    // That is, parameter blocks pointing to the same memory location
			    for (i = 0; i < parameter_blocks.size(); i++) {
			    	for (j = i+1; j < parameter_blocks.size(); j++) {
			    		if (parameter_blocks.get(i) == parameter_blocks.get(j)) {
			    			System.err.println("Duplicate parameter blocks in a residual parameter are not allowed.");
            		    	System.err.println("Parameter blocks " + i + " and " + j + " are duplicates.");	
			    		}
			    	}
			    }
			
			// Add parameter blocks and convert the double*'s to parameter blocks.
			Vector<ParameterBlock> parameter_block_ptrs = new Vector<ParameterBlock>(parameter_blocks.size());
			  for (i = 0; i < parameter_blocks.size(); ++i) {
			    parameter_block_ptrs.add(i,
			        InternalAddParameterBlock(parameter_blocks.get(i),
			                                  parameter_block_sizes.get(i)));
			  }
			  
			  if (!options_.disable_all_safety_checks) {
				    // Check that the block sizes match the block sizes expected by the
				    // cost_function.
				    for (i = 0; i < parameter_block_ptrs.size(); ++i) {
				      if (cost_function.parameter_block_sizes().get(i) !=
				               parameter_block_ptrs.get(i).Size()) {
				          System.err.println("The cost function expects parameter block " + i);
				          System.err.println(" of size " + cost_function.parameter_block_sizes().get(i));
				          System.err.println(" but was given a block of size " + parameter_block_ptrs.get(i).Size());
				      }
				    }
				  }
			  
			  ResidualBlock new_residual_block =
				      new ResidualBlock(cost_function,
				                        loss_function,
				                        parameter_block_ptrs,
				                        program_.residual_blocks_.size());

			// Add dependencies on the residual to the parameter blocks.
			  if (options_.enable_fast_removal) {
			    for (i = 0; i < parameter_blocks.size(); ++i) {
			      parameter_block_ptrs.get(i).AddResidualBlock(new_residual_block);
			    }
			  }

			  program_.residual_blocks_.add(new_residual_block);
			  
			  if (options_.enable_fast_removal) {
				    residual_block_set_.add(new_residual_block);
			  }
			  
			  if (options_.cost_function_ownership == Ownership.TAKE_OWNERSHIP) {
				    // Increment the reference count, creating an entry in the table if
				    // needed. Note: C++ maps guarantee that new entries have default
				    // constructed values; this implies integers are zero initialized.
				    count = cost_function_ref_count_.get(cost_function);
				    cost_function_ref_count_.put(cost_function, count + 1);
				  }

			  if (options_.loss_function_ownership == Ownership.TAKE_OWNERSHIP &&
				      loss_function != null) {
				  count = loss_function_ref_count_.get(loss_function);
				  loss_function_ref_count_.put(loss_function, count + 1);
		      }

				  return new_residual_block;

	
		}
		
		ParameterBlock InternalAddParameterBlock(ArrayList<Double> values, int size) {

			  if (values == null) {
				  System.err.println("Null pointer passed to AddParameterBlock for a parameter with size " + size);
				  return null;
			  }

			  // Ignore the request if there is a block for the given pointer already.
	          if (parameter_block_map_.containsKey(values)) {
	        	  if (!options_.disable_all_safety_checks) {
	        	      int existing_size = parameter_block_map_.get(values).Size();
	        	      if (size != existing_size) {
	        	          System.err.println("Tried adding a parameter block with the same ArrayList<Double>, ");
	        	          System.err.println(" twice, but with different block sizes. Original ");
	        	          System.err.println("size was " + existing_size + " but new size is " + size);
	        	          return null;
	        	      }
	        	    }
	        	    return parameter_block_map_.get(values);
	          }
	          
	          
	        	    
	          // Pass the index of the new parameter block as well to keep the index in
	          // sync with the position of the parameter in the program's parameter vector.
	          ParameterBlock new_parameter_block =
	              new ParameterBlock(values, size, program_.parameter_blocks_.size());

	          // For dynamic problems, add the list of dependent residual blocks, which is
	          // empty to start.
	          if (options_.enable_fast_removal) {
	            new_parameter_block.EnableResidualBlockDependencies();
	          }
	          parameter_block_map_.put(values,new_parameter_block);
	          program_.parameter_blocks_.add(new_parameter_block);
	          return new_parameter_block;


		}
		
		
	}
		
		class ResidualBlock {
		    private CostFunction cost_function_;
		    private LossFunction loss_function_;
		    private ParameterBlock parameter_blocks_[];

			// The index of the residual, typically in a Program. This is only to permit
			// switching from a ResidualBlock* to an index in the Program's array, needed
			// to do efficient removals.
			private int index_;
			public ResidualBlock(CostFunction cost_function,
				    LossFunction loss_function,
				    Vector<ParameterBlock> parameter_blocks,
				    int index) {
			    if (cost_function == null) {
			    	System.err.println("cost_function == null in ResidualBlock constructor");
			    }
			    cost_function_ = cost_function;
			    loss_function_ = loss_function;
			    parameter_blocks_ = new ParameterBlock[cost_function.parameter_block_sizes().size()];
			    index_ = index;
			    for (int i = 0; i < parameter_blocks.size(); i++) {
			    	parameter_blocks_[i] = parameter_blocks.get(i);
			    }
			}
		}
		
		class ParameterBlock {
			  int i, j;
			  private ArrayList<Double> user_state_;
			  private int size_;
			  boolean is_constant_;
			  LocalParameterization local_parameterization_;
			  // If non-null, contains the residual blocks this parameter block is in.
			  HashSet<ResidualBlock> residual_blocks_;

			  // The "state" of the parameter. These fields are only needed while the
			  // solver is running. While at first glance using mutable is a bad idea, this
			  // ends up simplifying the internals of Ceres enough to justify the potential
			  // pitfalls of using "mutable."
			  ArrayList<Double> state_;
			  double local_parameterization_jacobian_[];
			  //mutable scoped_array<double> local_parameterization_jacobian_;

			  // The index of the parameter. This is used by various other parts of Ceres to
			  // permit switching from a ParameterBlock* to an index in another array.
			  int index_;

			  // The offset of this parameter block inside a larger state vector.
			  int state_offset_;

			  // The offset of this parameter block inside a larger delta vector.
			  int delta_offset_;
			
			// Create a parameter block with the user state, size, and index specified.
			  // The size is the size of the parameter block and the index is the position
			  // of the parameter block inside a Program (if any).
			  public ParameterBlock(ArrayList<Double> user_state, int size, int index) {
			    Init(user_state, size, index, null);
			  }

			  public ParameterBlock(ArrayList<Double> user_state,
			                 int size,
			                 int index,
			                 LocalParameterization local_parameterization) {
			    Init(user_state, size, index, local_parameterization);
			  }
			
			// The size of the parameter block.
			public int Size() { return size_; }
			
			public int LocalSize() {
			    return (local_parameterization_ == null)
			        ? size_
			        : local_parameterization_.LocalSize();
			  }
			
			 // Set the parameterization. The parameterization can be set exactly once;
			  // multiple calls to set the parameterization to different values will crash.
			  // It is an error to pass NULL for the parameterization. The parameter block
			  // does not take ownership of the parameterization.
			  public void SetParameterization(LocalParameterization new_parameterization) {
			    if (new_parameterization == null) {
			    	System.err.println("NULL parameterization invalid.");
			    	return;
			    }
			    // Nothing to do if the new parameterization is the same as the
			    // old parameterization.
			    if (new_parameterization == local_parameterization_) {
			      return;
			    }

			    if (local_parameterization_ != null) {
			        System.err.println("Can't re-set the local parameterization; it leads to ");
			        System.err.println("ambiguous ownership. Current local parameterization is: " + local_parameterization_);
			        return;
			    }

			    if (new_parameterization.GlobalSize() != size_) {
			        System.err.println("Invalid parameterization for parameter block. The parameter block ");
			        System.err.println("has size " + size_ + " while the parameterization has a global ");
			        System.err.println("size of " + new_parameterization.GlobalSize() + ". Did you ");
			        System.err.println("accidentally use the wrong parameter block or parameterization?");
			        return;
			    }

			    if(new_parameterization.LocalSize() <= 0) {
			        System.err.println("Invalid parameterization. Parameterizations must have a positive ");
			        System.err.println("dimensional tangent space.");
			        return;
			    }

			    local_parameterization_ = new_parameterization;
			    //local_parameterization_jacobian_.reset(
			    local_parameterization_jacobian_ =
			        new double[local_parameterization_.GlobalSize() *
			                   local_parameterization_.LocalSize()];
			    if(!UpdateLocalParameterizationJacobian()) {
			        System.err.println("Local parameterization Jacobian computation failed for x: ");
			        for (i = 0; i < size_-1; i++) {
			        System.err.print(state_.get(i) + " ");	
			        }
			        System.err.println(state_.get(size_-1));
			    }
			  }

			
			void Init(ArrayList<Double> user_state,
		            int size,
		            int index,
		            LocalParameterization local_parameterization) {
		    user_state_ = user_state;
		    size_ = size;
		    index_ = index;
		    is_constant_ = false;
		    state_ = user_state_;

		    local_parameterization_ = null;
		    if (local_parameterization != null) {
		      SetParameterization(local_parameterization);
		    }

		    state_offset_ = -1;
		    delta_offset_ = -1;
		  }
			
			boolean UpdateLocalParameterizationJacobian() {
			    if (local_parameterization_ == null) {
			      return true;
			    }

			    // Update the local to global Jacobian. In some cases this is
			    // wasted effort; if this is a bottleneck, we will find a solution
			    // at that time.

			    int jacobian_size = Size() * LocalSize();
			    // jacobian is a row-major GlobalSize() x LocalSize() matrix.
			    InvalidateArray(jacobian_size,
			                    local_parameterization_jacobian_);
			    double Jstate_[] = new double[state_.size()];
			    for (i = 0; i < state_.size(); i++) {
			    	Jstate_[i] = state_.get(i);
			    }
			    if (!local_parameterization_.ComputeJacobian(
			            Jstate_,
			            local_parameterization_jacobian_)) {
			      System.err.println("Local parameterization Jacobian computation failed for x:");
			      for (i = 0; i < size_-1; i++) {
				        System.err.print(state_.get(i) + " ");	
				  }
				  System.err.println(state_.get(size_-1));
			      return false;
			    }

			    if (!IsArrayValid(jacobian_size, local_parameterization_jacobian_)) {
			      System.err.println("Local parameterization Jacobian computation returned");
			      System.err.println("an invalid matrix for x: ");
			      for (i = 0; i < size_-1; i++) {
				        System.err.print(state_.get(i) + " ");	
				  }
				  System.err.println(state_.get(size_-1));
			      System.err.println("\n Jacobian matrix : ");
			      for (i = 0; i < size_; i++) {
			          for (j = 0; j < LocalSize()-1; j++) {
			        	  System.err.print(local_parameterization_jacobian_[i * size_ + j] + " ");
			          }
			          System.err.println(local_parameterization_jacobian_[i * size_ + LocalSize()-1]);
			      }
			      return false;
			    }
			    return true;
			  }
			
			void ToString() {
				System.out.println("size_ = " + size_);
				System.out.println("is_constant_ = " + is_constant_);
				System.out.println("index_ = " + index_);
				System.out.println("state_offset_ " + state_offset_);
				System.out.println("delta_offset_ " + delta_offset_);
			}
			
			void EnableResidualBlockDependencies() {
			    if (residual_blocks_ != null) {
			        System.err.println("Ceres bug: There is already a residual block collection ");
			        System.err.println("for parameter block: ");
			        ToString();
			        return;
			    }
			    residual_blocks_ = new HashSet<ResidualBlock>();
			  }
			
			void AddResidualBlock(ResidualBlock residual_block) {
			    if (residual_blocks_ == null) {
			        System.err.println("Ceres bug: The residual block collection is null for parameter block: ");
			        ToString();
			        return;
			    }
			    residual_blocks_.add(residual_block);
			  }
		}
		
		// The class LocalParameterization defines the function Plus and its
		// Jacobian which is needed to compute the Jacobian of f w.r.t delta.
		abstract class LocalParameterization {
			  // jacobian is a row-major GlobalSize() x LocalSize() matrix.
			  public abstract boolean ComputeJacobian(double x[], double jacobian[]);
			  
			  // Size of x.
			  public abstract int GlobalSize();

			  // Size of delta.
			  public abstract int LocalSize();
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
		
		void InvalidateArray(int size, double x[]) {
			  if (x != null) {
			    for (int i = 0; i < size; ++i) {
			      x[i] = kImpossibleValue;
			    }
			  }
		}
		
		boolean IsArrayValid(int size, double x[]) {
			  if (x != null) {
			    for (int i = 0; i < size; ++i) {
			      if (!Double.isFinite(x[i]) || (x[i] == kImpossibleValue))  {
			        return false;
			      }
			    }
			  }
			  return true;
		}

		
		
	} // public abstract class CeresSolver
	
	class indexValueComparator implements Comparator<indexValue> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(indexValue o1, indexValue o2) {
        	double a = o1.getValue();
            double b = o2.getValue();
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
        
    class indexValue {
		private int index;
		private double value;
		
		public indexValue(int index, double value) {
			this.index = index;
			this.value = value;
		}
		
		public int getIndex() {
			return index;
		}
		
		public double getValue() {
			return value;
		}

		
		
	}
	



