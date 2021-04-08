package gov.nih.mipav.model.algorithms;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;
import java.util.Vector;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import Jama.Matrix;
import WildMagic.LibFoundation.Mathematics.Vector2d;
import gov.nih.mipav.model.algorithms.CeresSolver.AutoDiffCostFunction;
import gov.nih.mipav.model.algorithms.CeresSolver.Block;
import gov.nih.mipav.model.algorithms.CeresSolver.BlockRandomAccessDenseMatrix;
import gov.nih.mipav.model.algorithms.CeresSolver.BlockRandomAccessDiagonalMatrix;
import gov.nih.mipav.model.algorithms.CeresSolver.Cell;
import gov.nih.mipav.model.algorithms.CeresSolver.CellInfo;
import gov.nih.mipav.model.algorithms.CeresSolver.CompressedList;
import gov.nih.mipav.model.algorithms.CeresSolver.CompressedRowBlockStructure;
import gov.nih.mipav.model.algorithms.CeresSolver.CostFunction;
import gov.nih.mipav.model.algorithms.CeresSolver.CostFunctorExample;
import gov.nih.mipav.model.algorithms.CeresSolver.CurveFittingFunctorExample;
import gov.nih.mipav.model.algorithms.CeresSolver.EvaluateOptions;
import gov.nih.mipav.model.algorithms.CeresSolver.FirstOrderFunction;
import gov.nih.mipav.model.algorithms.CeresSolver.GradientProblem;
import gov.nih.mipav.model.algorithms.CeresSolver.GradientProblemSolverOptions;
import gov.nih.mipav.model.algorithms.CeresSolver.GradientProblemSolverSummary;
import gov.nih.mipav.model.algorithms.CeresSolver.NumericDiffCostFunction;
import gov.nih.mipav.model.algorithms.CeresSolver.NumericDiffMethodType;
import gov.nih.mipav.model.algorithms.CeresSolver.NumericDiffOptions;
import gov.nih.mipav.model.algorithms.CeresSolver.Ownership;
import gov.nih.mipav.model.algorithms.CeresSolver.ProblemImpl;
import gov.nih.mipav.model.algorithms.CeresSolver.SizedCostFunction;
import gov.nih.mipav.model.algorithms.CeresSolver.Solver;
import gov.nih.mipav.model.algorithms.CeresSolver.SparseMatrix;
import gov.nih.mipav.model.algorithms.CeresSolver.TripletSparseMatrix;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue2;
import gov.nih.mipav.model.structures.jama.GeneralizedInverse2;
import gov.nih.mipav.model.structures.jama.LinearEquations;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.model.structures.jama.SVD;
import gov.nih.mipav.view.Preferences;

/* This is a port of the C++ files in ceres-solver-1.14.0 under the BSD license:
 * Ceres Solver - A fast non-linear least squares minimizer Copyright 2015
 * Google Inc. All rights reserved. http://ceres-solver.org/
 * 
 * Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
* 
* Redistributions of source code must retain the above copyright notice, this
* list of conditions and the following disclaimer. Redistributions in binary
* form must reproduce the above copyright notice, this list of conditions and
* the following disclaimer in the documentation and/or other materials provided
* with the distribution. Neither the name of Google Inc. nor the names of its
* contributors may be used to endorse or promote products derived from this
* software without specific prior written permission.
* 
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
* POSSIBILITY OF SUCH DAMAGE.
* */

public class CeresSolverTest extends CeresSolver {
	CeresSolver2 ce2 = new CeresSolver2();
	
	public CeresSolverTest() {
		super();
	}
	
	

	// A CostFunction implementing analytically derivatives for the
	// function f(x) = 10 - x.
	class QuadraticCostFunction extends SizedCostFunction {
		public QuadraticCostFunction() {
			// number of residuals
			// size of first parameter
			super(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);

			// f(x) = 10 - x.
			residuals[0] = 10 - x[0];

			// f'(x) = -1. Since there's only 1 parameter and that parameter
			// has 1 dimension, there is only 1 element to fill in the
			// jacobians.
			//
			// Since the Evaluate function can be called with the jacobians
			// pointer equal to NULL, the Evaluate function must check to see
			// if jacobians need to be computed.
			//
			// For this simple problem it is overkill to check if jacobians[0]
			// is NULL, but in general when writing more complex
			// CostFunctions, it is possible that Ceres may only demand the
			// derivatives w.r.t. a subset of the parameter blocks.
			if (jacobians != null && jacobians[0] != null) {
				jacobians[0][0] = -1;
			}

			return true;
		}
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);

			// f(x) = 10 - x.
			residuals[0] = 10 - x[0];

			// f'(x) = -1. Since there's only 1 parameter and that parameter
			// has 1 dimension, there is only 1 element to fill in the
			// jacobians.
			//
			// Since the Evaluate function can be called with the jacobians
			// pointer equal to NULL, the Evaluate function must check to see
			// if jacobians need to be computed.
			//
			// For this simple problem it is overkill to check if jacobians[0]
			// is NULL, but in general when writing more complex
			// CostFunctions, it is possible that Ceres may only demand the
			// derivatives w.r.t. a subset of the parameter blocks.
			if (jacobians != null && jacobians[0] != null) {
				jacobians[0][jacobians_offset[0]] = -1;
			}

			return true;
		}
	};
	
	public void runNumericDiffCostFunctionExample() {
		// From hello_world_numeric_diff.cc
		// Ceres Solver Report: Iterations: 2, Initial cost: 4.512500e+01, Final cost: 4.511690e-07, Termination: CONVERGENCE
		// Solved answer = 9.999050085225893
		// Actual answer = 10.0
		double x[] = new double[] {0.5};
		testCase = COST_FUNCTOR_EXAMPLE;
		CostFunctorExample cf = new CostFunctorExample();
		NumericDiffMethodType method = NumericDiffMethodType.CENTRAL;
		Ownership ownership = Ownership.TAKE_OWNERSHIP;
		NumericDiffOptions options = new NumericDiffOptions();
		CostFunction cost_function = new NumericDiffCostFunction<CostFunctorExample>(cf, method, ownership, options, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		Solver solver = new Solver();
		solver.options.minimizer_progress_to_stdout = true;
		// Solver::Summary summary;
		optionsValid = true;
		Solve(solver.options, problem, solver.summary);
		if (optionsValid) {
			System.out.println(solver.summary.BriefReport());
			System.out.println("Solved answer = " + x[0]);
			System.out.println("Actual answer = 10.0");
		}
	}

	public void runAutoDiffCostFunctionExample() {
        // From hello_world.cc
		// Will not implement automatic differentiation wrapper so don't run this
		double x[] = new double[] { 0.5 };
		// auto-differentiation to obtain the derivative (jacobian).
		CostFunctorExample cf = new CostFunctorExample();
		CostFunction cost_function = new AutoDiffCostFunction<CostFunctorExample>(cf, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		Solver solver = new Solver();
		solver.options.minimizer_progress_to_stdout = true;
		// Solver::Summary summary;
		optionsValid = true;
		Solve(solver.options, problem, solver.summary);
		if (optionsValid) {
		    System.out.println("Solved answer = " + x[0]);
		    System.out.println("Actual answer = 10.0");
		}
	}

	public void runSizedCostFunctionExample() {
		// From hello_world_analytic_diff.cc
        // Solved answer = 9.999050094990503
		double x[] = new double[] { 0.5 };
		CostFunction cost_function = new QuadraticCostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		Solver solver = new Solver();
		solver.options.minimizer_progress_to_stdout = true;
		// Solver::Summary summary;
		optionsValid = true;
		Solve(solver.options, problem, solver.summary);
		if (optionsValid) {
			System.out.println(solver.summary.BriefReport());
			System.out.println("Solved answer = " + x[0]);
			System.out.println("Actual answer = 10.0");
		}
	}
	
	// f(x,y) = (1-x)^2 + 100(y - x^2)^2;
	class Rosenbrock extends FirstOrderFunction {
		
	  public Rosenbrock() {
		  super();
	  }

	  public boolean Evaluate(double[] parameters,
	                        double[] cost,
	                        double[] gradient) {
	    final double x = parameters[0];
	    final double y = parameters[1];

	    cost[0] = (1.0 - x) * (1.0 - x) + 100.0 * (y - x * x) * (y - x * x);
	    if (gradient != null) {
	      gradient[0] = -2.0 * (1.0 - x) - 200.0 * (y - x * x) * 2.0 * x;
	      gradient[1] = 200.0 * (y - x * x);
	    }
	    return true;
	  }

	  public int NumParameters() { return 2; }
	} // class Rosenbrock
	
	public void runRosenbrockExample() {
		// For default LBFGS and WOLFE:   
	    //line_search_direction_type = LineSearchDirectionType.LBFGS;
		// and
		//line_search_direction_type = LineSearchDirectionType.BFGS;
	    //line_search_type = LineSearchType.WOLFE;
		//Ceres GradientProblemSolver Report: Iterations: 36, Initial cost: 2.420000e+01, Final cost: 1.955192e-27, Termination: CONVERGENCE
		//Initial x: -1.2 y: 1.0
		//Final calculation x: 1.0000000000000369 y: 1.0000000000000762
		
		//DENSE_NORMAL_CHOLESKY and CGNR
		//Ceres GradientProblemSolver Report: Iterations: 36, Initial cost: 2.420000e+01, Final cost: 1.955192e-27, Termination: CONVERGENCE
		//Initial x: -1.2 y: 1.0
		//Final calculation x: 1.0000000000000369 y: 1.0000000000000762
		
	    //line_search_direction_type = LineSearchDirectionType.STEEPEST_DESCENT;
	    //line_search_type = LineSearchType.ARMIJO;
	    //Ceres GradientProblemSolver Report: Iterations: 12231, Initial cost: 2.420000e+01, Final cost: 3.699585e-11, Termination: CONVERGENCE
		//Initial x: -1.2 y: 1.0
		//Final calculation x: 0.9999939267782262 y: 0.9999878201526412
		
		//line_search_direction_type = LineSearchDirectionType.NONLINEAR_CONJUGATE_GRADIENT;
	    //line_search_type = LineSearchType.WOLFE;
	    //nonlinear_conjugate_gradient_type = NonlinearConjugateGradientType.FLETCHER_REEVES;
		//Ceres GradientProblemSolver Report: Iterations: 100, Initial cost: 2.420000e+01, Final cost: 4.572754e-12, Termination: CONVERGENCE
		//Initial x: -1.2 y: 1.0
		//Final calculation x: 1.0000021376601098 y: 1.0000042809490068
		
		//line_search_direction_type = LineSearchDirectionType.NONLINEAR_CONJUGATE_GRADIENT;
	    //line_search_type = LineSearchType.WOLFE;
	    //nonlinear_conjugate_gradient_type = NonlinearConjugateGradientType.POLAK_RIBIERE;
		//Does not give correct answer
		//Ceres GradientProblemSolver Report: Iterations: 24, Initial cost: 2.420000e+01, Final cost: 6.794325e-01, Termination: CONVERGENCE
		//Initial x: -1.2 y: 1.0
		//Final calculation x: 0.1883300765677482 y: 0.04982942249468986
		
		//line_search_direction_type = LineSearchDirectionType.NONLINEAR_CONJUGATE_GRADIENT;
	    //line_search_type = LineSearchType.WOLFE;
	    //nonlinear_conjugate_gradient_type = NonlinearConjugateGradientType.HESTENES_STIEFEL;
		//Ceres GradientProblemSolver Report: Iterations: 53, Initial cost: 2.420000e+01, Final cost: 5.120933e-20, Termination: CONVERGENCE
		//Initial x: -1.2 y: 1.0
		//Final calculation x: 0.9999999997738941 y: 0.9999999995468638
		

		double parameters[] = new double[]{-1.2, 1.0};

		  GradientProblemSolverOptions options = new GradientProblemSolverOptions();
		  options.minimizer_progress_to_stdout = true;

		  GradientProblemSolverSummary summary = new GradientProblemSolverSummary();
		  GradientProblem problem = new GradientProblem(new Rosenbrock());
		  optionsValid = true;
		  Solve(options, problem, parameters, summary);

		  //std::cout << summary.FullReport() << "\n";
		  if (optionsValid) {
			  System.out.println(summary.BriefReport());
			  System.out.println("Initial x: " + -1.2 + " y: " + 1.0);
			  System.out.println("Final calculation x: " + parameters[0]
			            + " y: " + parameters[1]);
		  }
	
	}
	
	// NLConstrainedEngine, MIPAV port of ELSUNC, gave the following for curve_fitting.cc
	//Number of iterations: 4
	//Chi-squared: 2.113502580804931
	//c = 0.1314013888081673
    //m = 0.29187119399433387
	
    
	
	class CurveFittingCostFunction extends SizedCostFunction {
		public CurveFittingCostFunction() {
			// number of resdiuals
			// size of first parameter
			super(67, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < curveFittingObservations; i++) {
				double exp = Math.exp(x[1] * curveFittingData[2*i] + x[0]);
			    residuals[i] = curveFittingData[2*i+1] - exp;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][2*i] = -exp;
					jacobians[0][2*i+1] = -curveFittingData[2*i]*exp;
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < curveFittingObservations; i++) {
				double exp = Math.exp(x[1] * curveFittingData[2*i] + x[0]);
			    residuals[i] = curveFittingData[2*i+1] - exp;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0] + 2*i] = -exp;
					jacobians[0][jacobians_offset[0] + 2*i+1] = -curveFittingData[2*i]*exp;
			    }
			}

			return true;
		
	  }
	} // class CurveFittingCostFunction

	public void runCurveFittingSizedCostFunctionExample() {
		// From curve_fitting.cc
        // Solved ELSUNC port answer c = 0.1314013888081673 m = 0.29187119399433387
		// With TrustRegionStrategyType.LEVENBERG_MARQUARDT:
		//Ceres Solver Report: Iterations: 26, Initial cost: 1.211734e+02, Final cost: 1.056752e+00, Termination: CONVERGENCE
		// Solved answer c = 0.13151752053358823 m = 0.29183474506178536
		
		//With trust_region_strategy_type = TrustRegionStrategyType.DOGLEG;
		//dogleg_type = DoglegType.TRADITIONAL_DOGLEG;
		//Ceres Solver Report: Iterations: 16, Initial cost: 1.211734e+02, Final cost: 1.056751e+00, Termination: CONVERGENCE
		//Solved answer c = 0.13139880817309488 m = 0.2918721096498213
		
		//With trust_region_strategy_type = TrustRegionStrategyType.DOGLEG;
	    //dogleg_type = DoglegType.SUBSPACE_DOGLEG
		//Ceres Solver Report: Iterations: 16, Initial cost: 1.211734e+02, Final cost: 1.056751e+00, Termination: CONVERGENCE
		//Solved answer c = 0.13139893189971505 m = 0.2918720583465281
		
		//DENSE_NORMAL_CHOLESKY
		//Ceres Solver Report: Iterations: 14, Initial cost: 1.211734e+02, Final cost: 1.056751e+00, Termination: CONVERGENCE
		//Solved answer c = 0.13143858621916737 m = 0.29186127928969857
		
		//CGNR
		//Ceres Solver Report: Iterations: 14, Initial cost: 1.211734e+02, Final cost: 1.056751e+00, Termination: CONVERGENCE
		//Solved answer c = 0.13140547008473807 m = 0.29187016268214566
		
		//ITERATIVE_SCHUR
		//Ceres Solver Report: Iterations: 14, Initial cost: 1.211734e+02, Final cost: 1.056751e+00, Termination: CONVERGENCE
		//Solved answer c = 0.13143858621916737 m = 0.29186127928969857
		
		// DENSE_SCHUR
		// Ceres Solver Report: Iterations: 14, Initial cost: 1.211734e+02, Final cost: 1.056751e+00, Termination: CONVERGENCE
		// Solved answer c = 0.13143858621916737 m = 0.29186127928969857

		
		double x[] = new double[] {0.0, 0.0 };
		CostFunction cost_function = new CurveFittingCostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		Solver solver = new Solver();
		solver.options.minimizer_progress_to_stdout = true;
		// Solver::Summary summary;
		optionsValid = true;
		Solve(solver.options, problem, solver.summary);
		if (optionsValid) {
			System.out.println(solver.summary.BriefReport());
			System.out.println("Solved answer c = " + x[0] + " m = " + x[1]);
			System.out.println("Actual answer = c = 0.1314013888081673 m = 0.29187119399433387");
		}
	}
	
	
	
	public void runCurveFittingNumericDiffCostFunctionExample() {
		// From curve_fitting.cc
        // Solved ELSUNC port answer c = 0.1314013888081673 m = 0.29187119399433387
		// Solved CeresSolver answer Solved answer c = 0.13151752052574492 m = 0.2918347450638119
		double x[] = new double[] {0.0,0.0};
		testCase = CURVE_FITTING_EXAMPLE;
		CurveFittingFunctorExample cf = new CurveFittingFunctorExample();
		NumericDiffMethodType method = NumericDiffMethodType.CENTRAL;
		Ownership ownership = Ownership.TAKE_OWNERSHIP;
		NumericDiffOptions options = new NumericDiffOptions();
		CostFunction cost_function = new NumericDiffCostFunction<CurveFittingFunctorExample>(cf, method, ownership, options, 67, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		Solver solver = new Solver();
		solver.options.minimizer_progress_to_stdout = true;
		// Solver::Summary summary;
		optionsValid = true;
		Solve(solver.options, problem, solver.summary);
		if (optionsValid) {
			System.out.println(solver.summary.BriefReport());
			System.out.println("Solved answer c = " + x[0] + " m = " + x[1]);
			System.out.println("Actual answer = c = 0.1314013888081673 m = 0.29187119399433387");
		}
	}
	
	public void TESTBlockRandomAccessDenseMatrixGetCell () {
		//TESTBlockRandomAccessDenseMatrixGetCell passed all tests
		  boolean passed = true;
		  Vector<Integer> blocks = new Vector<Integer>();
		  blocks.add(3);
		  blocks.add(4);
		  blocks.add(5);
		  final int num_rows = 3 + 4 + 5;
		  BlockRandomAccessDenseMatrix m = new BlockRandomAccessDenseMatrix(blocks);
		  if (m.num_rows() != num_rows) {
			  System.err.println("In TESTBlockRandomAccessDenseMatrixGetCell m.num_rows() != num_rows");
			  passed = false;
		  }
		  if (m.num_cols() != num_rows) {
			  System.err.println("In TESTBlockRandomAccessDenseMatrixGetCell m.num_cols() != num_rows");
			  passed = false;
		  }

		  int row_idx = 0;
		  for (int i = 0; i < blocks.size(); ++i) {
		    int col_idx = 0;
		    for (int j = 0; j < blocks.size(); ++j) {
		      int row[] = new int[1];
		      int col[] = new int[1];
		      int row_stride[] = new int[1];
		      int col_stride[] = new int[1];
		      CellInfo cell =
		          m.GetCell(i, j, row, col, row_stride, col_stride);

		      if (cell == null) {
		    	  System.err.println("In TESTBlockRandomAccessDenseMatrixGetCell cell == null");
				  passed = false;  
		      }
		      if (row[0] != row_idx) {
		    	  System.err.println("In TESTBlockRandomAccessDenseMatrixGetCell row[0] != row_idx");
				  passed = false;   
		      }
		      if (col[0] != col_idx) {
		    	  System.err.println("In TESTBlockRandomAccessDenseMatrixGetCell col[0] != col_idx");
				  passed = false;   
		      }
		      if (row_stride[0] != (3+4+5)) {
		    	  System.err.println("In TESTBlockRandomAccessDenseMatrixGetCell row_stride[0] != (3+4+5)");
				  passed = false;  
		      }
		      if (col_stride[0] != (3+4+5)) {
		    	  System.err.println("In TESTBlockRandomAccessDenseMatrixGetCell col_stride[0] != (3+4+5)");
				  passed = false;  
		      }
		      col_idx += blocks.get(j);
		    }
		    row_idx += blocks.get(i);
		  }
		  if (passed) {
			  System.out.println("TESTBlockRandomAccessDenseMatrixGetCell passed all tests");
		  }
		}

		public void TESTBlockRandomAccessDenseMatrixWriteCell () {
			//TESTBlockRandomAccessDenseMatrixWriteCell passed all tests

			boolean passed = true;
			  Vector<Integer> blocks = new Vector<Integer>();
			  blocks.add(3);
			  blocks.add(4);
			  blocks.add(5);
			  final int num_rows = 3 + 4 + 5;
			  BlockRandomAccessDenseMatrix m = new BlockRandomAccessDenseMatrix(blocks);

		  // Fill the cell (i,j) with (i + 1) * (j + 1)
		  for (int i = 0; i < blocks.size(); ++i) {
		    for (int j = 0; j < blocks.size(); ++j) {
		      int row[] = new int[1];
		      int col[] = new int[1];
		      int row_stride[] = new int[1];
		      int col_stride[] = new int[1];
		      CellInfo cell = m.GetCell(
		          i, j, row, col, row_stride, col_stride);
		      for (int r = row[0]; r < row[0] + blocks.get(i); r++) {
		    	  for (int c = col[0]; c < col[0] + blocks.get(j); c++) {
		    		  cell.values[cell.values_index + r * col_stride[0] + c] = (i+1)*(j+1);
		    	  }
		      }
		      //MatrixRef(cell->values, row_stride, col_stride).block(
		          //row, col, blocks[i], blocks[j]) =
		          //(i+1) * (j+1) * Matrix::Ones(blocks[i], blocks[j]);
		    }
		  }

		  // Check the values in the array are correct by going over the
		  // entries of each block manually.
		  int row_idx = 0;
		  for (int i = 0; i < blocks.size(); ++i) {
		    int col_idx = 0;
		    for (int j = 0; j < blocks.size(); ++j) {
		      // Check the values of this block.
		      for (int r = 0; r < blocks.get(i); ++r) {
		        for (int c = 0; c < blocks.get(j); ++c) {
		          int pos = row_idx * num_rows + col_idx;
		          if (m.values()[pos] != (i + 1) * (j + 1)) {
		        	  System.err.println("In TESTBlockRandomAccessDenseMatrixWriteCell m.values()["+pos+"] != " + ((i+1)*(j+1)));
		        	  passed = false;
		          }
		        }
		      }
		      col_idx += blocks.get(j);
		    }
		    row_idx += blocks.get(i);
		  }
		  if (passed) {
			  System.out.println("TESTBlockRandomAccessDenseMatrixWriteCell passed all tests");
		  }
		}

   public void BlockRandomAccessDiagonalMatrixTest() {
	    // BlockRandomAccessDiagonalMatrixTest passed all tests
	    int i, r,c;
	    double diff;
	    double normSquared;
	    double norm;
	    double totalNormSquared = 0.0;
	    double totalNorm;
	    double actualNorm;
	    int num_nonzeros_;
	    double yel;
	    BlockRandomAccessDiagonalMatrix m_;
	    double kTolerance = 1e-14;
	    boolean passed = true;
	    Vector<Integer> blocks = new Vector<Integer>();
	    blocks.add(3);
	    blocks.add(4);
	    blocks.add(5);
	    final int num_rows = 3 + 4 + 5;
	    num_nonzeros_ =  3 * 3 + 4 * 4 + 5 * 5;

	    m_ = new BlockRandomAccessDiagonalMatrix(blocks);

	    if (m_.num_rows() != num_rows) {
	    	System.err.println("In BlockRandomDiagonalMatrixTest m_.num_rows() != num_rows");
	    	passed = false;
	    }
	    if (m_.num_cols() != num_rows) {
	    	System.err.println("In BlockRandomDiagonalMatrixTest m_.num_cols() != num_rows");
	    	passed = false;
	    }

	    for (i = 0; i < blocks.size(); ++i) {
	      final int row_block_id = i;
	      int col_block_id;
	      int row[] = new int[1];
	      int col[] = new int[1];
	      int row_stride[] = new int[1];
	      int col_stride[] = new int[1];

	      for (int j = 0; j < blocks.size(); ++j) {
	        col_block_id = j;
	        CellInfo cell =  m_.GetCell(row_block_id, col_block_id,
	                                    row, col,
	                                    row_stride, col_stride);
	        // Off diagonal entries are not present.
	        if (i != j) {
	          if (cell != null) {
	        	  System.err.println("In BlockRandomDiagonalMatrixTest cell != null for i != j");
	  	    	  passed = false;	  
	          }
	          continue;
	        }

	        if (cell == null) {
	        	  System.err.println("In BlockRandomDiagonalMatrixTest cell == null for i == j");
	  	    	  passed = false;	  
	        }
	        if (row[0] != 0) {
	        	System.err.println("In BlockRandomDiagonalMatrixTest row[0] != 0");
	  	    	passed = false;		
	        }
	        if (col[0] != 0) {
	        	System.err.println("In BlockRandomDiagonalMatrixTest col[0] != 0");
	  	    	passed = false;		
	        }
	        if (row_stride[0] != blocks.get(row_block_id)) {
	        	System.err.println("In BlockRandomDiagonalMatrixTest row_stride[0] != blocks.get(row_block_id)");
	  	    	passed = false;			
	        }
	        if (col_stride[0] != blocks.get(col_block_id)) {
	        	System.err.println("In BlockRandomDiagonalMatrixTest col_stride[0] != blocks.get(col_block_id)");
	  	    	passed = false;			
	        }

	        // Write into the block
	        for (r = row[0]; r < row[0] + blocks.get(row_block_id); r++) {
		    	  for (c = col[0]; c < col[0] + blocks.get(col_block_id); c++) {
		    		  cell.values[cell.values_index + r * col_stride[0] + c] = (row_block_id+1)*(col_block_id+1);
		    		  if ((r-row[0]) == (c-col[0])) {
		    			  cell.values[cell.values_index + r * col_stride[0] + c] += 1.0;
		    		  }
		    	  }
		      }
	        //MatrixRef(cell->values, row_stride, col_stride).block(
	            //row, col, blocks[row_block_id], blocks[col_block_id]) =
	            //(row_block_id + 1) * (col_block_id +1) *
	            //Matrix::Ones(blocks[row_block_id], blocks[col_block_id])
	            //+ Matrix::Identity(blocks[row_block_id], blocks[row_block_id]);
	      }
	    }
	    
	    final TripletSparseMatrix tsm = m_.matrix();
	    if (tsm.num_nonzeros() != num_nonzeros_) {
	    	System.err.println("In BlockRandomDiagonalMatrixTest tsm.num_nonzeros() != num_nonzeros_");
  	    	passed = false;	
	    }
	    if (tsm.max_num_nonzeros() != num_nonzeros_) {
	    	System.err.println("In BlockRandomDiagonalMatrixTest tsm.max_num_nonzeros() != num_nonzeros_");
  	    	passed = false;	
	    }

	    Matrix dense = tsm.ToDenseMatrix();

	    // (0,0)
	    normSquared = 0.0;
	    for (r = 0; r < 3; r++) {
	    	for (c = 0; c < 3; c++) {
	    		if (r != c) {
	    			diff = (dense.getArray()[r][c] - 1.0);   
	    		}
	    		else {
	    			diff = (dense.getArray()[r][c] - 2.0);
	    		}
	    		normSquared += (diff*diff);
	    		totalNormSquared += (dense.getArray()[r][c]*dense.getArray()[r][c]);
	    	}
	    }
	    norm = Math.sqrt(normSquared);
	    if (norm > kTolerance) {
	    	System.err.println("In BlockRandomDiagonalMatrixTest dense.block(0,0,3,3).norm() = " + norm);
  	    	passed = false;	
	    }
	    

	    // (1,1)
	    normSquared = 0.0;
	    for (r = 3; r < 7; r++) {
	    	for (c = 3; c < 7; c++) {
	    		if (r != c) {
	    	        diff = (dense.getArray()[r][c] - 4.0);
	    		}
	    		else {
	    			diff = (dense.getArray()[r][c] - 5.0);
	    		}
	    		normSquared += (diff * diff);
	    		totalNormSquared += (dense.getArray()[r][c]*dense.getArray()[r][c]);
	    	}
	    }
	    norm = Math.sqrt(normSquared);
	    if (norm > kTolerance) {
	    	System.err.println("In BlockRandomDiagonalMatrixTest dense.block(3,3,4,4).norm() = " + norm);
  	    	passed = false;	
	    }
	    

	    // (1,1)
	    normSquared = 0.0;
	    for (r = 7; r < 12; r++) {
	    	for (c = 7; c < 12; c++) {
	    		if (r != c) {
	    	        diff = (dense.getArray()[r][c] - 9.0);
	    		}
	    		else {
	    			diff = (dense.getArray()[r][c] - 10.0);
	    		}
	    		normSquared += (diff * diff);
	    		totalNormSquared += (dense.getArray()[r][c]*dense.getArray()[r][c]);
	    	}
	    }
	    norm = Math.sqrt(normSquared);
	    if (norm > kTolerance) {
	    	System.err.println("In BlockRandomDiagonalMatrixTest dense.block(7,7,5,5).norm() = " + norm);
  	    	passed = false;	
	    }
	   

	    // There is nothing else in the matrix besides these 3 blocks.
	    totalNorm = Math.sqrt(totalNormSquared);
	    actualNorm  = Math.sqrt(6 * 1.0 + 3 * 4.0 +
	                     12 * 16.0 + 4 * 25.0 +
	                     20 * 81.0 + 5 * 100.0);
	    if (Math.abs(totalNorm - actualNorm) > kTolerance) {
	    	System.err.println("In BlockRandomDiagonalMatrixTest totalNorm = " + totalNorm + " actualNorm = " + actualNorm);
  	    	passed = false;	
	    }
	    
	    double x[] = new double[dense.getRowDimension()];
	    RandomNumberGen ran = new RandomNumberGen();
	    for (i = 0; i < dense.getRowDimension(); i++) {
	        x[i] = ran.genGaussianRandomNum(-1.0,1.0);	
	    }
	    double[] expected_y = new double[dense.getRowDimension()]; 
	    for (r = 0; r < dense.getRowDimension(); r++) {
	        for (c = 0; c < dense.getRowDimension(); c++) {
	            expected_y[r] += (dense.getArray()[r][c]*x[c]); 	
	        }
	    }
	    double[] actual_y = new double[dense.getRowDimension()];
	    m_.RightMultiply(x,  actual_y);
	    normSquared = 0.0;
	    for (r = 0; r < dense.getRowDimension(); r++) {
	        diff = expected_y[r] - actual_y[r];
	        normSquared += (diff * diff);
	    }
	    norm = Math.sqrt(normSquared);
	    if (norm > kTolerance) {
	    	System.err.println("In BlockRandomDiagonalMatrixTest (expected_y - actual_y).norm() = " + norm);
  	    	passed = false;	
	    }
	    
	    Matrix expected_inverse = dense.inverse();

	     m_.Invert();
	     dense = tsm.ToDenseMatrix();
	     normSquared = 0;
	     for (r = 0; r < expected_inverse.getRowDimension(); r++) {
	    	 for (c = 0; c < expected_inverse.getRowDimension(); c++) {
	    		 diff = expected_inverse.getArray()[r][c] - dense.getArray()[r][c];
	    		 normSquared += (diff * diff);
	    	 }
	     }
	     norm = Math.sqrt(normSquared);
		    if (norm > kTolerance) {
		    	System.err.println("In BlockRandomDiagonalMatrixTest (expected_inverse - dense).norm() = " + norm);
	  	    	passed = false;	
		    }


	  
	    if (passed) {
			  System.out.println("BlockRandomAccessDiagonalMatrixTest passed all tests");
		  }
   }
   
   public void TestDetectStructure() {
	   //TestDetectStructure passed all the tests

       //TEST(DetectStructure, EverythingStatic) {
	   boolean passed = true;
	   int expected_row_block_size[] = new int[] {2};
	   int expected_e_block_size[] =  new int[] {3};
	   int expected_f_block_size[] = new int[] {4};

	   CompressedRowBlockStructure bs = new CompressedRowBlockStructure();

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 3;
	   bs.cols.lastElement().position = 0;

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 4;
	   bs.cols.lastElement().position = 3;

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 4;
	   bs.cols.lastElement().position = 7;

	   {
	     bs.rows.add(new CompressedList());
	     CompressedList row = bs.rows.lastElement();
	     row.block.size = 2;
	     row.block.position = 0;
	     row.cells.add(new Cell(0, 0));
	     row.cells.add(new Cell(1, 0));
	   }

	   {
	     bs.rows.add(new CompressedList());
	     CompressedList row = bs.rows.lastElement();
	     row.block.size = 2;
	     row.block.position = 2;
	     row.cells.add(new Cell(0, 0));
	     row.cells.add(new Cell(2, 0));
	   }

	   int row_block_size[] = new int[] {0};
	   int e_block_size[] = new int[] {0};
	   int f_block_size[] = new int[] {0};
	   int num_eliminate_blocks = 1;
	   DetectStructure(bs,
	                   num_eliminate_blocks,
	                   row_block_size,
	                   e_block_size,
	                   f_block_size);

	   if (row_block_size[0] != expected_row_block_size[0]) {
		   System.err.println("In DetectStructure EverythingStatic row_block_size[0] != expected_row_block_size[0]");
		   passed = false;
	   }
	   if (e_block_size[0] != expected_e_block_size[0]) {
		   System.err.println("In DetectStructure EverythingStatic e_block_size[0] != expected_e_block_size[0]");
		   passed = false;
	   }
	   if (f_block_size[0] != expected_f_block_size[0]) {
		   System.err.println("In DetectStructure EverythingStatic f_block_size[0] != expected_f_block_size[0]");
		   passed = false;
	   }
	   

	 //TEST(DetectStructure, DynamicRow) {
	   expected_row_block_size[0] = DYNAMIC;
	   expected_e_block_size[0] = 3;
	   expected_f_block_size[0] = 4;

	   bs = new CompressedRowBlockStructure();

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 3;
	   bs.cols.lastElement().position = 0;

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 4;
	   bs.cols.lastElement().position = 3;

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 4;
	   bs.cols.lastElement().position = 7;

	   {
	     bs.rows.add(new CompressedList());
	     CompressedList row = bs.rows.lastElement();
	     row.block.size = 2;
	     row.block.position = 0;
	     row.cells.add(new Cell(0, 0));
	     row.cells.add(new Cell(1, 0));
	   }

	   {
	     bs.rows.add(new CompressedList());
	     CompressedList row = bs.rows.lastElement();
	     row.block.size = 1;
	     row.block.position = 2;
	     row.cells.add(new Cell(0, 0));
	     row.cells.add(new Cell(2, 0));
	   }

	   row_block_size[0] = 0;
	   e_block_size[0] = 0;
	   f_block_size[0] = 0;
	   num_eliminate_blocks = 1;
	   DetectStructure(bs,
	                   num_eliminate_blocks,
	                   row_block_size,
	                   e_block_size,
	                   f_block_size);

	   if (row_block_size[0] != expected_row_block_size[0]) {
		   System.err.println("In DetectStructure DynamicRow row_block_size[0] != expected_row_block_size[0]");
		   passed = false;
	   }
	   if (e_block_size[0] != expected_e_block_size[0]) {
		   System.err.println("In DetectStructure DynamicRow e_block_size[0] != expected_e_block_size[0]");
		   passed = false;
	   }
	   if (f_block_size[0] != expected_f_block_size[0]) {
		   System.err.println("In DetectStructure DynamicRow f_block_size[0] != expected_f_block_size[0]");
		   passed = false;
	   }
	   

	 //TEST(DetectStructure, DynamicFBlockDifferentRows) {
	   expected_row_block_size[0] = 2;
	   expected_e_block_size[0] = 3;
	   expected_f_block_size[0] = DYNAMIC;


	   bs = new CompressedRowBlockStructure();

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 3;
	   bs.cols.lastElement().position = 0;

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 4;
	   bs.cols.lastElement().position = 3;

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 3;
	   bs.cols.lastElement().position = 7;

	   {
	     bs.rows.add(new CompressedList());
	     CompressedList row = bs.rows.lastElement();
	     row.block.size = 2;
	     row.block.position = 0;
	     row.cells.add(new Cell(0, 0));
	     row.cells.add(new Cell(1, 0));
	   }

	   {
	     bs.rows.add(new CompressedList());
	     CompressedList row = bs.rows.lastElement();
	     row.block.size = 2;
	     row.block.position = 2;
	     row.cells.add(new Cell(0, 0));
	     row.cells.add(new Cell(2, 0));
	   }

	   row_block_size[0] = 0;
	   e_block_size[0] = 0;
	   f_block_size[0] = 0;
	   num_eliminate_blocks = 1;
	   DetectStructure(bs,
	                   num_eliminate_blocks,
	                   row_block_size,
	                   e_block_size,
	                   f_block_size);
	   if (row_block_size[0] != expected_row_block_size[0]) {
		   System.err.println("In DetectStructure DynamicFBlockDifferentRows row_block_size[0] != expected_row_block_size[0]");
		   passed = false;
	   }
	   if (e_block_size[0] != expected_e_block_size[0]) {
		   System.err.println("In DetectStructure DynamicFBlockDifferentRows e_block_size[0] != expected_e_block_size[0]");
		   passed = false;
	   }
	   if (f_block_size[0] != expected_f_block_size[0]) {
		   System.err.println("In DetectStructure DynamicFBlockDifferentRows f_block_size[0] != expected_f_block_size[0]");
		   passed = false;
	   }
	  

	 //TEST(DetectStructure, DynamicEBlock) {
	   expected_row_block_size[0] = 2;
	   expected_e_block_size[0] = DYNAMIC;
	   expected_f_block_size[0] = 3;

	   bs = new CompressedRowBlockStructure();

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 3;
	   bs.cols.lastElement().position = 0;

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 4;
	   bs.cols.lastElement().position = 3;

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 3;
	   bs.cols.lastElement().position = 7;

	   {
	     bs.rows.add(new CompressedList());
	     CompressedList row = bs.rows.lastElement();
	     row.block.size = 2;
	     row.block.position = 0;
	     row.cells.add(new Cell(0, 0));
	     row.cells.add(new Cell(2, 0));
	   }

	   {
	     bs.rows.add(new CompressedList());
	     CompressedList row = bs.rows.lastElement();
	     row.block.size = 2;
	     row.block.position = 2;
	     row.cells.add(new Cell(1, 0));
	     row.cells.add(new Cell(2, 0));
	   }

	   row_block_size[0] = 0;
	   e_block_size[0] = 0;
	   f_block_size[0] = 0;
	   num_eliminate_blocks = 2;
	   DetectStructure(bs,
	                   num_eliminate_blocks,
	                   row_block_size,
	                   e_block_size,
	                   f_block_size);

	   if (row_block_size[0] != expected_row_block_size[0]) {
		   System.err.println("In DetectStructure DynamicEBlock row_block_size[0] != expected_row_block_size[0]");
		   passed = false;
	   }
	   if (e_block_size[0] != expected_e_block_size[0]) {
		   System.err.println("In DetectStructure DynamicEBlock e_block_size[0] != expected_e_block_size[0]");
		   passed = false;
	   }
	   if (f_block_size[0] != expected_f_block_size[0]) {
		   System.err.println("In DetectStructure DynamicEBlock f_block_size[0] != expected_f_block_size[0]");
		   passed = false;
	   }
	   

	 //TEST(DetectStructure, DynamicFBlockSameRow) {
	   expected_row_block_size[0] = 2;
	   expected_e_block_size[0] = 3;
	   expected_f_block_size[0] = DYNAMIC;

	   bs = new CompressedRowBlockStructure();

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 3;
	   bs.cols.lastElement().position = 0;

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 4;
	   bs.cols.lastElement().position = 3;

	   bs.cols.add(new Block());
	   bs.cols.lastElement().size = 3;
	   bs.cols.lastElement().position = 7;

	   {
	     bs.rows.add(new CompressedList());
	     CompressedList row = bs.rows.lastElement();
	     row.block.size = 2;
	     row.block.position = 0;
	     row.cells.add(new Cell(0, 0));
	     row.cells.add(new Cell(1, 0));
	     row.cells.add(new Cell(2, 0));
	   }

	   row_block_size[0] = 0;
	   e_block_size[0] = 0;
	   f_block_size[0] = 0;
	   num_eliminate_blocks = 1;
	   DetectStructure(bs,
	                   num_eliminate_blocks,
	                   row_block_size,
	                   e_block_size,
	                   f_block_size);

	   if (row_block_size[0] != expected_row_block_size[0]) {
		   System.err.println("In DetectStructure DynamicFBlockSameRow row_block_size[0] != expected_row_block_size[0]");
		   passed = false;
	   }
	   if (e_block_size[0] != expected_e_block_size[0]) {
		   System.err.println("In DetectStructure DynamicFBlockSameRow e_block_size[0] != expected_e_block_size[0]");
		   passed = false;
	   }
	   if (f_block_size[0] != expected_f_block_size[0]) {
		   System.err.println("In DetectStructure DynamicFBlockSameRow f_block_size[0] != expected_f_block_size[0]");
		   passed = false;
	   }
	   if (passed) {
		   System.out.println("TestDetectStructure passed all the tests");
	   }
	 }
   
   public void ConjugateGradientTest() {
	   // ConjugateGradientTest passed all the tests.
	   boolean passed = true;
       //TEST(ConjugateGradientTest, Solves3x3IdentitySystem) {
	   double diagonal[] = { 1.0, 1.0, 1.0 };
	   TripletSparseMatrix A = CreateSparseDiagonalMatrix(diagonal, 3);
	   double b[] = new double[] {1.0, 2.0, 3.0};
	   double x[] = new double[] {1.0, 1.0, 1.0};

	   LinearSolverOptions options = new LinearSolverOptions();
	   options.max_num_iterations = 10;

	   LinearSolverPerSolveOptions per_solve_options = new LinearSolverPerSolveOptions();
	   per_solve_options.r_tolerance = 1e-9;

	   ConjugateGradientsSolver solver = new ConjugateGradientsSolver(options);
	   LinearSolverSummary summary =
	       solver.Solve(A, b, per_solve_options, x);

	   if (summary.termination_type != LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS) {
		   System.err.println("In ConjugateGradientTest Solves3x3IdentitySystem summary.termination_type != LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS");
		   passed = false;
	   }
	   if (summary.num_iterations != 1) {
		   System.err.println("In ConjugateGradientTest Solves3x3IdentitySystem summary.num_iterations != 1");
		   passed = false;   
	   }

	   if (1 !=  x[0]) {
		   System.err.println("In ConjugateGradientTest Solves3x3IdentitySystem x[0] != 1");
		   passed = false;      
	   }
	   if (2 !=  x[1]) {
		   System.err.println("In ConjugateGradientTest Solves3x3IdentitySystem x[1] != 2");
		   passed = false;      
	   }
	   if (3 !=  x[2]) {
		   System.err.println("In ConjugateGradientTest Solves3x3IdentitySystem x[2] != 3");
		   passed = false;      
	   }


	 //TEST(ConjuateGradientTest, Solves3x3SymmetricSystem) {
	   A = new TripletSparseMatrix(3, 3, 9);

	   //      | 2  -1  0|
	   //  A = |-1   2 -1| is symmetric positive definite.
	   //      | 0  -1  2|
	   int[] Ai = A.mutable_rows();
	   int[] Aj = A.mutable_cols();
	   double[] Ax = A.mutable_values();
	   int counter = 0;
	   for (int i = 0; i < 3; ++i) {
	     for (int j = 0; j < 3; ++j) {
	       Ai[counter] = i;
	       Aj[counter] = j;
	       ++counter;
	     }
	   }
	   Ax[0] = 2.;
	   Ax[1] = -1.;
	   Ax[2] = 0;
	   Ax[3] = -1.;
	   Ax[4] = 2;
	   Ax[5] = -1;
	   Ax[6] = 0;
	   Ax[7] = -1;
	   Ax[8] = 2;
	   A.set_num_nonzeros(9);

	   b[0] = -1;
	   b[1] = 0;
	   b[2] = 3;

	   x[0] = 1;
	   x[1] = 1;
	   x[2] = 1;

	   options = new LinearSolverOptions();
	   options.max_num_iterations = 10;

	   per_solve_options = new LinearSolverPerSolveOptions();
	   per_solve_options.r_tolerance = 1e-9;

	   solver = new ConjugateGradientsSolver(options);
	   summary =
	       solver.Solve(A, b, per_solve_options, x);
	   
	   if (summary.termination_type != LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS) {
		   System.err.println("In ConjugateGradientTest Solves3x3SymmetricSystem summary.termination_type != LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS");
		   passed = false;
	   }
	  

	   if (0 !=  x[0]) {
		   System.err.println("In ConjugateGradientTest Solves3x3SymmetricSystem x[0] != 0");
		   passed = false;      
	   }
	   if (1 !=  x[1]) {
		   System.err.println("In ConjugateGradientTest Solves3x3SymmetricSystem x[1] != 1");
		   passed = false;      
	   }
	   if (2 !=  x[2]) {
		   System.err.println("In ConjugateGradientTest Solves3x3SymmetricSystem x[2] != 2");
		   passed = false;      
	   }
	   
	   if (passed) {
		   System.out.println("ConjugateGradientTest passed all the tests.");
	   }

	   
	 }
   
   public void DenseSparseMatrixTestRightMultiply() {
	   // DenseSparseMatrixTestRightMultiply() passed all tests
	   DenseSparseMatrixTest DSM = new DenseSparseMatrixTest();
	   DSM.DenseSparseMatrixTestRightMultiply();
   }
   
   public void DenseSparseMatrixTestLeftMultiply() {
	   // DenseSparseMatrixTestLeftMultiply() passed all tests
	   DenseSparseMatrixTest DSM = new DenseSparseMatrixTest();
	   DSM.DenseSparseMatrixTestLeftMultiply();   
   }
   
   public void DenseSparseMatrixTestColumnNorm() {
	   // DenseSparseMatrixTestColumnNorm() passed all tests
	   DenseSparseMatrixTest DSM = new DenseSparseMatrixTest();
	   DSM.DenseSparseMatrixTestColumnNorm();      
   }
   
   public void DenseSparseMatrixTestScale() {
	   // DenseSparseMatrixTestScale() passed all tests
	   DenseSparseMatrixTest DSM = new DenseSparseMatrixTest();
	   DSM.DenseSparseMatrixTestScale();      
   }
   
   public void DenseSparseMatrixTestToDenseMatrix() {
	   // DenseSparseMatrixTestToDenseMatrix() passed all tests
	   DenseSparseMatrixTest DSM = new DenseSparseMatrixTest();
	   DSM.DenseSparseMatrixTestToDenseMatrix();   
   }
   
   class DenseSparseMatrixTest {
	private boolean passed;
	private String test;
	private int num_rows;
	private int num_cols;

	private TripletSparseMatrix tsm;
	private DenseSparseMatrix dsm;
	
   public DenseSparseMatrixTest() {
	   passed = true;
   }
   
   public void SetUp() {
	   LinearLeastSquaresProblem problem = 
		        CreateLinearLeastSquaresProblemFromId(1);
       if (problem == null) {
    	   System.err.println("problem = null in DenseSparseMatrix SetUp");
    	   passed = false;
       }
       
       tsm = (TripletSparseMatrix)(problem.A);
       dsm = new DenseSparseMatrix(tsm);

       num_rows = tsm.num_rows();
       num_cols = tsm.num_cols();

   }
   
   public void CompareMatrices(SparseMatrix a, SparseMatrix b) {
	   int j;
	   double diff;
	   double normSquared;
	   double norm;
	   if (a.num_rows() != b.num_rows()) {
		   System.err.println("In " + test + " a.num_rows() != b.num_rows()");
		   passed = false;
	   }
	   if (a.num_cols() != b.num_cols()) {
		   System.err.println("In " + test + " a.num_cols() != b.num_cols()");
		   passed = false;
	   }

	   int num_rows = a.num_rows();
	   int num_cols = a.num_cols();

	   for (int i = 0; i < num_cols; ++i) {
	     double x[] = new double[num_cols];
	     x[i] = 1.0;

	     double y_a[] = new double[num_rows];
	     double y_b[] = new double[num_rows];

	     a.RightMultiply(x, y_a);
	     b.RightMultiply(x, y_b);
	     
	     normSquared = 0.0;
	     for (j = 0; j < num_rows; j++) {
	    	 diff = y_a[j] - y_b[j];
	    	 normSquared += (diff * diff);
	     }
         norm = Math.sqrt(normSquared);
         if (norm != 0) {
        	 System.err.println("In " + test + " (y_a - y_b).norm() != 0");
        	 passed = false;
         }
	   } // for (int i = 0; i < num_cols; ++i)
	 } // CompareMatrices
   
     public void DenseSparseMatrixTestRightMultiply() {
       int j;
  	   double diff;
  	   double normSquared;
  	   double norm;
  	   test = "DenseSparseMatrixTestRightMultiply()";
       SetUp();
	   CompareMatrices(tsm, dsm);

	   // Try with a not entirely zero vector to verify column interactions, which
	   // could be masked by a subtle bug when using the elementary vectors.
	   double a[] = new double[num_cols];
	   for (int i = 0; i < num_cols; i++) {
	     a[i] = i;
	   }
	   double b1[] = new double[num_rows];
	   double b2[] = new double[num_rows];

	   tsm.RightMultiply(a, b1);
	   dsm.RightMultiply(a, b2);

	   normSquared = 0.0;
	     for (j = 0; j < num_rows; j++) {
	    	 diff = b1[j] - b2[j];
	    	 normSquared += (diff * diff);
	     }
       norm = Math.sqrt(normSquared);
       if (norm != 0) {
      	 System.err.println("In " + test + " (b1 - b2).norm() != 0");
      	 passed = false;
       }
	   if (passed) {
		   System.out.println("DenseSparseMatrixTestRightMultiply() passed all tests");
	   }
	 }
     
     public void DenseSparseMatrixTestLeftMultiply() {
    	 int j;
    	   double diff;
    	   double normSquared;
    	   double norm;
    	   test = "DenseSparseMatrixTestLeftMultiply()";
           SetUp();
    	  for (int i = 0; i < num_rows; ++i) {
    	    double a[] = new double[num_rows];
    	    a[i] = 1.0;

    	    double b1[] = new double[num_cols];
    	    double b2[] = new double[num_cols];

    	    tsm.LeftMultiply(a, b1);
    	    dsm.LeftMultiply(a, b2);

    	    normSquared = 0.0;
   	     for (j = 0; j < num_cols; j++) {
   	    	 diff = b1[j] - b2[j];
   	    	 normSquared += (diff * diff);
   	     }
          norm = Math.sqrt(normSquared);
          if (norm != 0) {
         	 System.err.println("In " + test + " (b1 - b2).norm() != 0");
         	 passed = false;
          }
    	  }

    	  // Try with a not entirely zero vector to verify column interactions, which
    	  // could be masked by a subtle bug when using the elementary vectors.
    	  double a[] = new double[num_rows];
    	  for (int i = 0; i < num_rows; i++) {
    	    a[i] = i;
    	  }
    	  
    	  double b1[] = new double[num_cols];
  	    double b2[] = new double[num_cols];

  	    tsm.LeftMultiply(a, b1);
  	    dsm.LeftMultiply(a, b2);

  	    normSquared = 0.0;
 	     for (j = 0; j < num_cols; j++) {
 	    	 diff = b1[j] - b2[j];
 	    	 normSquared += (diff * diff);
 	     }
        norm = Math.sqrt(normSquared);
        if (norm != 0) {
       	 System.err.println("In " + test + " (b1 - b2).norm() != 0");
       	 passed = false;
        }
    	  
        if (passed) {
        	System.out.println("DenseSparseMatrixTestLeftMultiply() passed all tests");
        }
    	} // public void DenseSparseMatrixTestLeftMultiply()
     
      public void DenseSparseMatrixTestColumnNorm() {
    	  int j;
   	   double diff;
   	   double normSquared;
   	   double norm;
   	   test = "DenseSparseMatrixTestColumnNorm()";
          SetUp();

    	  double b1[] = new double[num_cols];
    	  double b2[] = new double[num_cols];

    	  tsm.SquaredColumnNorm(b1);
    	  dsm.SquaredColumnNorm(b2);

    	  normSquared = 0.0;
  	     for (j = 0; j < num_cols; j++) {
  	    	 diff = b1[j] - b2[j];
  	    	 normSquared += (diff * diff);
  	     }
         norm = Math.sqrt(normSquared);
         if (norm != 0) {
        	 System.err.println("In " + test + " (b1 - b2).norm() != 0");
        	 passed = false;
         }
     	  
         if (passed) {
         	System.out.println("DenseSparseMatrixTestColumnNorm() passed all tests");
         }
    	}

      public void DenseSparseMatrixTestScale() {
    	  test = "DenseSparseMatrixTestScale()";
          SetUp();
    	  double scale[] = new double[num_cols];
    	  for (int i = 0; i < num_cols; ++i) {
    	    scale[i] = i + 1;
    	  }
    	  tsm.ScaleColumns(scale);
    	  dsm.ScaleColumns(scale);
    	  CompareMatrices(tsm, dsm);
    	  if (passed) {
    		  System.out.println("DenseSparseMatrixTestScale() passed all tests");
    	  }
    	}
      
      public void DenseSparseMatrixTestToDenseMatrix() {
    	  int r,c;
    	  double diff;
    	  double normSquared = 0.0;
    	  double norm;
    	  test = "DenseSparseMatrixToDenseMatrix()";
          SetUp();
    	  Matrix tsm_dense = tsm.ToDenseMatrix();
    	  Matrix dsm_dense = dsm.ToDenseMatrix();
    	  
    	  normSquared = 0.0;
    	  for (r = 0; r < tsm_dense.getRowDimension(); r++) {
    		  for (c = 0; c < tsm_dense.getColumnDimension(); c++) {
    			  diff = tsm_dense.get(r,c) - dsm_dense.get(r,c);
    			  normSquared += (diff * diff);
    		  }
    	  }
          norm = Math.sqrt(normSquared);
          if (norm != 0) {
        	  System.err.println("In " + test + " (tsm_dense - dsm_dense).norm() != 0.0");
        	  passed = false;
          }
    	  
    	  if (passed) {
    		  System.out.println("DenseSparseMatrixTestToDenseMatrix() passed all tests");
    	  }
    	}



   } // class DenseSparseMatrixTest
   
   class MockCostFunctionBase extends SizedCostFunction {
	   public MockCostFunctionBase(int kNumResiduals, int N0, int N1, int N2, int N3, int N4, int N5, int N6, int N7,
				int N8, int N9, int num_residuals) {
		   super(kNumResiduals, N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, num_residuals);
	   }
	   public MockCostFunctionBase(int kNumResiduals, int N0, int N1, int N2, int N3, int N4, int N5, int N6, int N7,
				int N8, int N9) {
		   super(kNumResiduals, N0, N1, N2, N3, N4, N5, N6, N7, N8, N9);
	   }
	   public boolean Evaluate(Vector<double[]> parameters,
	                         double[] residuals,
	                         double[][] jacobians) {
		 // Do nothing. This is never called.
	     return true;
	   }
	   
	   public boolean Evaluate(Vector<double[]> parameters,
               double[] residuals,
               double[][] jacobians,
               int[] jacobians_offset) {
		// Do nothing. This is never called.
		return true;
		}
	 };
	 
	 public void TESTReorderResidualBlockNormalFunction() {
		  // TESTReorderResidualBlockNormalFunction() passed all tests
		  boolean passed = true;
		  ProblemImpl problem = new ProblemImpl();
		  double x[] = new double[1];
		  double y[] = new double[1];
		  double z[] = new double[1];

		  problem.AddParameterBlock(x, 1);
		  problem.AddParameterBlock(y, 1);
		  problem.AddParameterBlock(z, 1);

		  problem.AddResidualBlock(new MockCostFunctionBase(2,1,0,0,0,0,0,0,0,0,0), null, x);
		  problem.AddResidualBlock(new MockCostFunctionBase(2,1,1,0,0,0,0,0,0,0,0), null, z, x);
		  problem.AddResidualBlock(new MockCostFunctionBase(2,1,1,0,0,0,0,0,0,0,0), null, z, y);
		  problem.AddResidualBlock(new MockCostFunctionBase(2,1,0,0,0,0,0,0,0,0,0), null, z);
		  problem.AddResidualBlock(new MockCostFunctionBase(2,1,1,0,0,0,0,0,0,0,0), null, x, y);
		  problem.AddResidualBlock(new MockCostFunctionBase(2,1,0,0,0,0,0,0,0,0,0), null, y);

		  OrderedGroups<double[]> linear_solver_ordering = new OrderedGroups<double[]>();
		  linear_solver_ordering.AddElementToGroup(x, 0);
		  linear_solver_ordering.AddElementToGroup(y, 0);
		  linear_solver_ordering.AddElementToGroup(z, 1);

		  SolverOptions options = new SolverOptions();
		  options.linear_solver_type = LinearSolverType.DENSE_SCHUR;
		  options.linear_solver_ordering = linear_solver_ordering;

		  Vector<ResidualBlock> residual_blocks =
		      problem.program().residual_blocks();

		  Vector<ResidualBlock> expected_residual_blocks = new Vector<ResidualBlock>();

		  // This is a bit fragile, but it serves the purpose. We know the
		  // bucketing algorithm that the reordering function uses, so we
		  // expect the order for residual blocks for each e_block to be
		  // filled in reverse.
		  expected_residual_blocks.add(residual_blocks.get(4));
		  expected_residual_blocks.add(residual_blocks.get(1));
		  expected_residual_blocks.add(residual_blocks.get(0));
		  expected_residual_blocks.add(residual_blocks.get(5));
		  expected_residual_blocks.add(residual_blocks.get(2));
		  expected_residual_blocks.add(residual_blocks.get(3));

		  Program program = problem.mutable_program();
		  program.SetParameterOffsetsAndIndex();

		  String message[] = new String[1];
		 if (!LexicographicallyOrderResidualBlocks(
		                  2,
		                  problem.mutable_program(),
		                  message)) {
			 System.err.println("In TESTReorderResidualBlockNormalFunction() LexicographicallyOrderResidualBlocks == false");
			 passed = false;
		 }
		 
		  if (residual_blocks.size() != expected_residual_blocks.size()) {
			  System.err.println("In TESTReorderResidualBlockNormalFunction() residual_blocks.size() != expected_residual_blocks.size()");
				 passed = false;  
		  }
		  for (int i = 0; i < expected_residual_blocks.size(); ++i) {
		       if (residual_blocks.get(i) != expected_residual_blocks.get(i)) {
		    	   System.err.println("In TESTReorderResidualBlockNormalFunction() residual_blocks.get("+i+") != expected_residual_blocks.get("+i+")");
					 passed = false;    
		       }
		  }
		  if (passed) {
			  System.out.println("TESTReorderResidualBlockNormalFunction() passed all tests");
		  }
		}

		public void TESTApplyOrderingOrderingTooSmall() {
		  // TESTApplyOrderingOrderingTooSmall() passed all tests
		  boolean passed = true;
		  ProblemImpl problem = new ProblemImpl();
		  double x[] = new double[1];
		  double y[] = new double[1];
		  double z[] = new double[1];

		  problem.AddParameterBlock(x, 1);
		  problem.AddParameterBlock(y, 1);
		  problem.AddParameterBlock(z, 1);

		  OrderedGroups<double[]> linear_solver_ordering = new OrderedGroups<double[]>();
		  linear_solver_ordering.AddElementToGroup(x, 0);
		  linear_solver_ordering.AddElementToGroup(y, 1);

		  Program program = problem.program();
		  String message[] = new String[1];
		  if (ApplyOrdering(problem.parameter_map(),
		                             linear_solver_ordering,
		                             program,
		                             message)) {
			  System.err.println("In TESTApplyOrderingOrderingTooSmall() ApplyOrdering == true");
			  passed = false;
		  }
		  
		  if (passed) {
			  System.out.println("TESTApplyOrderingOrderingTooSmall() passed all tests");
		  }
		}
		
		public void TESTApplyOrderingNormal() {
			// TESTApplyOrderingNormal() passed all tests
			boolean passed = true;
			  ProblemImpl problem = new ProblemImpl();
			  double x[] = new double[1];
			  double y[] = new double[1];
			  double z[] = new double[1];

			  problem.AddParameterBlock(x, 1);
			  problem.AddParameterBlock(y, 1);
			  problem.AddParameterBlock(z, 1);

			  OrderedGroups<double[]> linear_solver_ordering = new OrderedGroups<double[]>();
			  linear_solver_ordering.AddElementToGroup(x, 0);
			  linear_solver_ordering.AddElementToGroup(y, 2);
			  linear_solver_ordering.AddElementToGroup(z, 1);

			  Program program = problem.mutable_program();
			  String message[] = new String[1];
			  
			  if (!ApplyOrdering(problem.parameter_map(),
                      linear_solver_ordering,
                      program,
                      message)) {
				 System.err.println("In TESTApplyOrderingNormal() ApplyOrdering == false");
				 passed = false;
			 }

			  final Vector<ParameterBlock> parameter_blocks = program.parameter_blocks();

			  if (parameter_blocks.size() != 3) {
				  System.err.println("In TESTApplyOrderingNormal() parameter_blocks.size() != 3");
				  passed = false;  
			  }
			  if (parameter_blocks.get(0).user_state() != x) {
				  System.err.println("In TESTApplyOrderingNormal() parameter_blocks.get(0).user_state() != x");
				  passed = false;  
			  }
			  if (parameter_blocks.get(1).user_state() != z) {
				  System.err.println("In TESTApplyOrderingNormal() parameter_blocks.get(1).user_state() != z");
				  passed = false;  
			  }
			  if (parameter_blocks.get(2).user_state() != y) {
				  System.err.println("In TESTApplyOrderingNormal() parameter_blocks.get(2).user_state() != y");
				  passed = false;  
			  }
			  if (passed) {
				  System.out.println("TESTApplyOrderingNormal() passed all tests");
			  }
			}

		public void TESTTripletSparseMatrixDefaultConstructorReturnsEmptyObject() {
			  // TESTTripletSparseMatrixDefaultConstructorReturnsEmptyObject() passed all tests
			  boolean passed = true;
			  TripletSparseMatrix m = new TripletSparseMatrix();
			  if (m.num_rows() != 0) {
				  System.err.println("In TESTTripletSparseMatrixDefaultConstructorReturnsEmptyObject() m.num_rows() != 0");
				  passed = false;
			  }
			  if (m.num_cols() != 0) {
				  System.err.println("In TESTTripletSparseMatrixDefaultConstructorReturnsEmptyObject() m.num_cols() != 0");
				  passed = false;  
			  }
			  if (m.num_nonzeros() != 0) {
				  System.err.println("In TESTTripletSparseMatrixDefaultConstructorReturnsEmptyObject() m.num_nonzeros() != 0");
				  passed = false;    
			  }
			  if (m.max_num_nonzeros() != 0) {
				  System.err.println("In TESTTripletSparseMatrixDefaultConstructorReturnsEmptyObject() m.max_num_nonzeros() != 0");
				  passed = false;      
			  }
			  if (passed) {
				  System.out.println("TESTTripletSparseMatrixDefaultConstructorReturnsEmptyObject() passed all tests");
			  }
			}

		public void TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() {
			  // TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() passed all tests
			  boolean passed = true;
			  // Build a matrix
			  TripletSparseMatrix m = new TripletSparseMatrix(2, 5, 4);
			  if (m.num_rows() != 2) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.num_rows() != 2");
				  passed = false;
			  }
			  if (m.num_cols() != 5) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.num_cols() != 5");
				  passed = false;  
			  }
			  if (m.num_nonzeros() != 0) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.num_nonzeros() != 0");
				  passed = false;   
			  }
			  if (m.max_num_nonzeros() != 4) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.max_num_nonzeros() != 4");
				  passed = false;   	  
			  }

			  m.mutable_rows()[0] = 0;
			  m.mutable_cols()[0] = 1;
			  m.mutable_values()[0] = 2.5;

			  m.mutable_rows()[1] = 1;
			  m.mutable_cols()[1] = 4;
			  m.mutable_values()[1] = 5.2;
			  m.set_num_nonzeros(2);

			  if (m.num_nonzeros() != 2) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.num_nonzeros() != 2");
				  passed = false; 	  
			  }

			  if (!m.AllTripletsWithinBounds()) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.AllTripletsWithinBounds() = false");
				  passed = false; 	  
			  }

			  // We should never be able resize and lose data
			  m.Reserve(1);
			  // Should see error message "Reallocation in Reserve will cause data loss"

			  // We should be able to resize while preserving data
			  m.Reserve(50);
			  if (m.max_num_nonzeros() != 50) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.max_num_nonzeros() != 50");
				  passed = false;  
			  }

			  m.Reserve(3);
			  if (m.max_num_nonzeros() != 50) {
				  // The space is already reserved.
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.max_num_nonzeros() != 50");
				  passed = false; 
			  }

			  if (m.rows()[0] != 0) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.rows()[0] != 0");
				  passed = false;  
			  }
			  if (m.rows()[1] != 1) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.rows()[1] != 1");
				  passed = false;    
			  }

			  if (m.cols()[0] != 1) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.cols()[0] != 1");
				  passed = false;    
			  }
			  if (m.cols()[1] != 4) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.cols()[1] != 4");
				  passed = false;      
			  }

			  if (m.values()[0] != 2.5) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.values()[0] != 2.5");
				  passed = false;  
			  }
			  if (m.values()[1] != 5.2) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.values()[1] != 5.2");
				  passed = false;  
			  }

			  // Bounds check should fail
			  m.mutable_rows()[0] = 10;
			  if(m.AllTripletsWithinBounds()) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.AllTripletsWithinBounds() = true");
				  passed = false;  	  
			  }

			  m.mutable_rows()[0] = 1;
			  m.mutable_cols()[0] = 100;
			  if(m.AllTripletsWithinBounds()) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.AllTripletsWithinBounds() = true");
				  passed = false;  	  
			  }

			  // Remove all data and then resize the data store
			  m.SetZero();
			  if (m.num_nonzeros() != 0) {
				  System.err.println("In TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() m.num_nonzeros() != 0");
				  passed = false;   
			  }
			  m.Reserve(1);
			  if (passed) {
				  System.out.println("TESTTripletSparseMatrixSimpleConstructorAndBasicOperations() passed all tests");
			  }
			}
		
		public void TESTTripletSparseMatrixCopyConstructor() {
			  // TESTTripletSparseMatrixCopyConstructor() passed all tests
			  boolean passed = true;
			  TripletSparseMatrix orig = new TripletSparseMatrix(2, 5, 4);
			  orig.mutable_rows()[0] = 0;
			  orig.mutable_cols()[0] = 1;
			  orig.mutable_values()[0] = 2.5;

			  orig.mutable_rows()[1] = 1;
			  orig.mutable_cols()[1] = 4;
			  orig.mutable_values()[1] = 5.2;
			  orig.set_num_nonzeros(2);

			  TripletSparseMatrix cpy = new TripletSparseMatrix(orig);

			  if (cpy.num_rows() != 2) {
				  System.err.println("In TESTTripletSparseMatrixCopyConstructor() cpy.num_rows() != 2");
				  passed = false;
			  }
			  if (cpy.num_cols() != 5) {
				  System.err.println("In TESTTripletSparseMatrixCopyConstructor() cpy.num_cols() != 5");
				  passed = false;  
			  }
			  if (cpy.num_nonzeros() != 2) {
				  System.err.println("In TESTTripletSparseMatrixCopyConstructor() cpy.num_nonzeros() != 2");
				  passed = false;   
			  }
			  if (cpy.max_num_nonzeros() != 4) {
				  System.err.println("In TESTTripletSparseMatrixCopyConstructor() cpy.max_num_nonzeros() != 4");
				  passed = false;  
			  }

			  if (cpy.rows()[0] != 0) {
				  System.err.println("In TESTTripletSparseMatrixCopyConstructor() cpy.rows()[0] != 0");
				  passed = false;    
			  }
			  if (cpy.rows()[1] != 1) {
				  System.err.println("In TESTTripletSparseMatrixCopyConstructor() cpy.rows()[1] != 1");
				  passed = false;   
			  }

			  if (cpy.cols()[0]!= 1) {
				  System.err.println("In TESTTripletSparseMatrixCopyConstructor() cpy.cols()[0] != 1");
				  passed = false; 
			  }
			  if (cpy.cols()[1] != 4) {
				  System.err.println("In TESTTripletSparseMatrixCopyConstructor() cpy.cols()[1] != 4");
				  passed = false;	  
			  }

			  if (cpy.values()[0] != 2.5) {
				  System.err.println("In TESTTripletSparseMatrixCopyConstructor() cpy.values()[0] != 2.5");
				  passed = false;
			  }
			  if (cpy.values()[1] != 5.2) {
				  System.err.println("In TESTTripletSparseMatrixCopyConstructor() cpy.values()[1] != 5.2");
				  passed = false;  
			  }
			  if (passed) {
				  System.out.println("TESTTripletSparseMatrixCopyConstructor() passed all tests");
			  }
			}
		
		public void TESTTripletSparseMatrixAssignmentOperator() {
			  // TESTTripletSparseMatrixAssignmentOperator() passed all tests
			  boolean passed = true;
			  TripletSparseMatrix orig = new TripletSparseMatrix(2, 5, 4);
			  orig.mutable_rows()[0] = 0;
			  orig.mutable_cols()[0] = 1;
			  orig.mutable_values()[0] = 2.5;

			  orig.mutable_rows()[1] = 1;
			  orig.mutable_cols()[1] = 4;
			  orig.mutable_values()[1] = 5.2;
			  orig.set_num_nonzeros(2);

			  TripletSparseMatrix cpy = new TripletSparseMatrix(3, 50, 40);
			  cpy.mutable_rows()[0] = 0;
			  cpy.mutable_cols()[0] = 10;
			  cpy.mutable_values()[0] = 10.22;

			  cpy.mutable_rows()[1] = 2;
			  cpy.mutable_cols()[1] = 23;
			  cpy.mutable_values()[1] = 34.45;

			  cpy.mutable_rows()[0] = 0;
			  cpy.mutable_cols()[0] = 10;
			  cpy.mutable_values()[0] = 10.22;

			  cpy.mutable_rows()[1] = 0;
			  cpy.mutable_cols()[1] = 3;
			  cpy.mutable_values()[1] = 4.4;
			  cpy.set_num_nonzeros(3);

			  cpy = orig;

			  if (cpy.num_rows() != 2) {
				  System.err.println("In TESTTripletSparseMatrixAssignmentOperator() cpy.num_rows() != 2");
				  passed = false;
			  }
			  if (cpy.num_cols() != 5) {
				  System.err.println("In TESTTripletSparseMatrixAssignmentOperator() cpy.num_cols() != 5");
				  passed = false;  
			  }
			  if (cpy.num_nonzeros() != 2) {
				  System.err.println("In TESTTripletSparseMatrixAssignmentOperator() cpy.num_nonzeros() != 2");
				  passed = false;  
			  }
			  if (cpy.max_num_nonzeros() != 4) {
				  System.err.println("In TESTTripletSparseMatrixAssignmentOperator() cpy.max_num_nonzeros() != 4");
				  passed = false;    
			  }

			  if (cpy.rows()[0] != 0) {
				  System.err.println("In TESTTripletSparseMatrixAssignmentOperator() cpy.rows()[0] != 0");
				  passed = false;     
			  }
			  if (cpy.rows()[1] != 1) {
				  System.err.println("In TESTTripletSparseMatrixAssignmentOperator() cpy.rows()[1] != 1");
				  passed = false;   
			  }

			  if (cpy.cols()[0] != 1) {
				  System.err.println("In TESTTripletSparseMatrixAssignmentOperator() cpy.cols()[0] != 1");
				  passed = false;   
			  }
			  if (cpy.cols()[1] != 4) {
				  System.err.println("In TESTTripletSparseMatrixAssignmentOperator() cpy.cols()[1] != 4");
				  passed = false;  
			  }

			  if (cpy.values()[0] != 2.5) {
				  System.err.println("In TESTTripletSparseMatrixAssignmentOperator() cpy.values()[0] != 2.5");
				  passed = false;
			  }
			  if (cpy.values()[1] != 5.2) {
				  System.err.println("In TESTTripletSparseMatrixAssignmentOperator() cpy.values()[1] != 5.2");
				  passed = false;	  
			  }
			  if (passed) {
				  System.out.println("TESTTripletSparseMatrixAssignmentOperator() passed all tests");
			  }
			}
		
		public void TESTTripletSparseMatrixAppendRows() {
			  // TESTTripletSparseMatrixAppendRows() passed all tests
			  boolean passed = true;
			  // Build one matrix.
			  TripletSparseMatrix m = new TripletSparseMatrix(2, 5, 4);
			  m.mutable_rows()[0] = 0;
			  m.mutable_cols()[0] = 1;
			  m.mutable_values()[0] = 2.5;

			  m.mutable_rows()[1] = 1;
			  m.mutable_cols()[1] = 4;
			  m.mutable_values()[1] = 5.2;
			  m.set_num_nonzeros(2);

			  // Build another matrix.
			  TripletSparseMatrix a = new TripletSparseMatrix(10, 5, 4);
			  a.mutable_rows()[0] = 0;
			  a.mutable_cols()[0] = 1;
			  a.mutable_values()[0] = 3.5;

			  a.mutable_rows()[1] = 1;
			  a.mutable_cols()[1] = 4;
			  a.mutable_values()[1] = 6.2;

			  a.mutable_rows()[2] = 9;
			  a.mutable_cols()[2] = 5;
			  a.mutable_values()[2] = 1;
			  a.set_num_nonzeros(3);

			  // Glue the second matrix to the bottom of the first.
			  m.AppendRows(a);

			  if (m.num_rows() != 12) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.num_rows() != 12");
				  passed = false;
			  }
			  if (m.num_cols() != 5) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.num_cols() != 5");
				  passed = false;  
			  }
			  if (m.num_nonzeros() != 5) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.num_nonzeros() != 5");
				  passed = false;  	  
			  }

			  if (m.values()[0] != 2.5) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.values()[0] != 2.5");
				  passed = false; 	  
			  }
			  if (m.values()[1] != 5.2) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.values()[1] != 5.2");
				  passed = false;  
			  }
			  if (m.values()[2] != 3.5) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.values()[2] != 3.5");
				  passed = false;
			  }
			  if (m.values()[3] != 6.2) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.values()[3] != 6.2");
				  passed = false;
			  }
			  if (m.values()[4] != 1) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.values()[4] != 1");
				  passed = false;
			  }

			  if (m.rows()[0] != 0) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.rows()[0] != 0");
				  passed = false;
			  }
			  if (m.rows()[1] != 1) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.rows()[1] != 1");
				  passed = false;  
			  }
			  if (m.rows()[2] != 2) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.rows()[2] != 2");
				  passed = false;
			  }
			  if (m.rows()[3] != 3) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.rows()[3] != 3");
				  passed = false;
			  }
			  if (m.rows()[4] != 11) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.rows()[4] != 11");
				  passed = false;
			  }

			  if (m.cols()[0] != 1) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.cols()[0] != 1");
				  passed = false;
			  }
			  if (m.cols()[1] != 4) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.cols()[1] != 4");
				  passed = false;  
			  }
			  if (m.cols()[2] != 1) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.cols()[2] != 1");
				  passed = false;
			  }
			  if (m.cols()[3] != 4) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.cols()[3] != 4");
				  passed = false;
			  }
			  if (m.cols()[4] != 5) {
				  System.err.println("In TESTTripletSparseMatrixAppendRows() m.cols()[4] != 5");
				  passed = false;
			  }
			  if (passed) {
				  System.out.println("TESTTripletSparseMatrixAppendRows() passed all tests");
			  }
			}

		public void TESTTripletSparseMatrixAppendCols() {
			  // TESTTripletSparseMatrixAppendCols() passed all tests
			  boolean passed = true;
			  // Build one matrix.
			  TripletSparseMatrix m = new TripletSparseMatrix(2, 5, 4);
			  m.mutable_rows()[0] = 0;
			  m.mutable_cols()[0] = 1;
			  m.mutable_values()[0] = 2.5;

			  m.mutable_rows()[1] = 1;
			  m.mutable_cols()[1] = 4;
			  m.mutable_values()[1] = 5.2;
			  m.set_num_nonzeros(2);

			  // Build another matrix.
			  TripletSparseMatrix a = new TripletSparseMatrix(2, 15, 4);
			  a.mutable_rows()[0] = 0;
			  a.mutable_cols()[0] = 1;
			  a.mutable_values()[0] = 3.5;

			  a.mutable_rows()[1] = 1;
			  a.mutable_cols()[1] = 4;
			  a.mutable_values()[1] = 6.2;

			  a.mutable_rows()[2] = 0;
			  a.mutable_cols()[2] = 10;
			  a.mutable_values()[2] = 1;
			  a.set_num_nonzeros(3);

			  // Glue the second matrix to the left of the first.
			  m.AppendCols(a);

			  if (m.num_rows() != 2) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.num_rows() != 2");
				  passed = false;
			  }
			  if (m.num_cols() != 20) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.num_cols() != 20");
				  passed = false;  
			  }
			  if (m.num_nonzeros() != 5) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.num_nonzeros() != 5");
				  passed = false;   
			  }

			  if (m.values()[0] != 2.5) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.values()[0] != 2.5");
				  passed = false; 	  
			  }
			  if (m.values()[1] != 5.2) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.values()[1] != 5.2");
				  passed = false;  
			  }
			  if (m.values()[2] != 3.5) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.values()[2] != 3.5");
				  passed = false;
			  }
			  if (m.values()[3] != 6.2) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.values()[3] != 6.2");
				  passed = false;
			  }
			  if (m.values()[4] != 1) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.values()[4] != 1");
				  passed = false;
			  }

			  if (m.rows()[0] != 0) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.rows()[0] != 0");
				  passed = false;
			  }
			  if (m.rows()[1] != 1) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.rows()[1] != 1");
				  passed = false;  
			  }
			  if (m.rows()[2] != 0) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.rows()[2] != 0");
				  passed = false;
			  }
			  if (m.rows()[3] != 1) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.rows()[3] != 1");
				  passed = false;
			  }
			  if (m.rows()[4] != 0) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.rows()[4] != 0");
				  passed = false;
			  }

			  if (m.cols()[0] != 1) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.cols()[0] != 1");
				  passed = false;
			  }
			  if (m.cols()[1] != 4) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.cols()[1] != 4");
				  passed = false;  
			  }
			  if (m.cols()[2] != 6) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.cols()[2] != 6");
				  passed = false;
			  }
			  if (m.cols()[3] != 9) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.cols()[3] != 9");
				  passed = false;
			  }
			  if (m.cols()[4] != 15) {
				  System.err.println("In TESTTripletSparseMatrixAppendCols() m.cols()[4] != 15");
				  passed = false;
			  }
			  if (passed) {
				  System.out.println("TESTTripletSparseMatrixAppendCols() passed all tests");
			  }
		}
		
		public void TESTTripletSparseMatrixCreateDiagonalMatrix() {
			  // TESTTripletSparseMatrixCreateDiagonalMatrix() passed all tests
			  boolean passed = true;
			  double[] values = new double[10];
			  for (int i = 0; i < 10; ++i)
			    values[i] = i;

			  TripletSparseMatrix m = 
			      CreateSparseDiagonalMatrix(values, 10);
			  if (m.num_rows() != 10) {
				  System.err.println("In TESTTripletSparseMatrixCreateDiagonalMatrix() m.num_rows() != 10");
				  passed = false;
			  }
			  if (m.num_cols() != 10) {
				  System.err.println("In TESTTripletSparseMatrixCreateDiagonalMatrix() m.num_cols() != 10");
				  passed = false;  
			  }
			  if (m.num_nonzeros() != 10) {
				  System.err.println("In TESTTripletSparseMatrixCreateDiagonalMatrix() m.num_nonzeros() != 10");
				  passed = false;

			  }
			  for (int i = 0; i < 10 ; ++i) {
			    if (m.rows()[i] != i) {
			    	System.err.println("In TESTTripletSparseMatrixCreateDiagonalMatrix() m.rows()["+i+"] != " + i);
					passed = false;
			    }
			    if (m.cols()[i] != i) {
			    	System.err.println("In TESTTripletSparseMatrixCreateDiagonalMatrix() m.cols()["+i+"] != " + i);
					passed = false;	
			    }
			    if (m.values()[i] != i) {
			    	System.err.println("In TESTTripletSparseMatrixCreateDiagonalMatrix() m.values()["+i+"] != " + i);
					passed = false;
			    }
			  }
			  if (passed) {
				  System.out.println("TESTTripletSparseMatrixCreateDiagonalMatrix() passed all tests");
			  }
			}

		public void TESTTripletSparseMatrixResize() {
			  // TESTTripletSparseMatrixResize() passed all tests
			  boolean passed = true;
			  TripletSparseMatrix m = new TripletSparseMatrix(10, 20, 200);
			  int nnz = 0;
			  for (int i = 0; i < 10; ++i) {
			    for (int j = 0; j < 20; ++j) {
			      m.mutable_rows()[nnz] = i;
			      m.mutable_cols()[nnz] = j;
			      m.mutable_values()[nnz++] = i+j;
			    }
			  }
			  m.set_num_nonzeros(nnz);
			  m.Resize(5, 6);
			  if (m.num_rows() != 5) {
				  System.err.println("In TESTTripletSparseMatrixResize() m.num_rows() != 5");
				  passed = false;
			  }
			  if (m.num_cols() != 6) {
				  System.err.println("In TESTTripletSparseMatrixResize() m.num_cols() != 6");
				  passed = false;  
			  }
			  if (m.num_nonzeros() != 30) {
				  System.err.println("In TESTTripletSparseMatrixResize() m.num_nonzeros() != 30");
				  passed = false;

			  }
			  for (int i = 0; i < 30; ++i) {
			    if (m.values()[i] != (m.rows()[i] + m.cols()[i])) {
			    	System.err.println("In TESTTripletSparseMatrixResize() m.num_nonzeros() != m.values()["+i+"] != (m.rows()["+i+"] + m.cols()["+i+"])");
					passed = false;	
			    }
			  }
			  if (passed) {
				  System.out.println("TESTTripletSparseMatrixResize() passed all tests");
			  }
			}
		// Trivial cost function that accepts three arguments.
		class TernaryCostFunction extends CostFunction {
		 public TernaryCostFunction(int num_residuals,
		                      int parameter_block1_size,
		                      int parameter_block2_size,
		                      int parameter_block3_size) {
		    set_num_residuals(num_residuals);
		    mutable_parameter_block_sizes().add(parameter_block1_size);
		    mutable_parameter_block_sizes().add(parameter_block2_size);
		    mutable_parameter_block_sizes().add(parameter_block3_size);
		  }

		  public boolean Evaluate(Vector<double[]> parameters,
		                        double[] residuals,
		                        double[][] jacobians) {
			int r, c, index;
		    for (int i = 0; i < num_residuals(); ++i) {
		      residuals[i] = i;
		    }
		    if (jacobians != null) {
		      for (int k = 0; k < 3; ++k) {
		        if (jacobians[k] != null) {
		          for (index = 0, r = 0; r < num_residuals(); r++) {
		        	  for (c = 0; c < parameter_block_sizes().get(k); c++, index++) {
		        		  jacobians[k][index] = k;
		        	  }
		          }
		        }
		      }
		    }
		    return true;
		  }
		  
		  public boolean Evaluate(Vector<double[]> parameters,
                  double[] residuals,
                  double[][] jacobians,
                  int[] jacobians_offset) {
			int r, c, index;
			for (int i = 0; i < num_residuals(); ++i) {
			residuals[i] = i;
			}
			if (jacobians != null) {
			for (int k = 0; k < 3; ++k) {
			  if (jacobians[k] != null) {
			    for (index = 0, r = 0; r < num_residuals(); r++) {
			  	  for (c = 0; c < parameter_block_sizes().get(k); c++, index++) {
			  		  jacobians[k][jacobians_offset[k] + index] = k;
			  	  }
			    }
			  }
			}
			}
			return true;
			}
		};
		
		public void TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() {
			  // TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() passed all tests
			  int i;
			  boolean passed = true;
			  double scratch[] = new double[64];

			  // Prepare the parameter blocks.
			  double values_x[] = new double[2];
			  ParameterBlock x = new ParameterBlock(values_x, 2, -1);

			  double values_y[] = new double[3];
			  ParameterBlock y = new ParameterBlock(values_y, 3, -1);

			  double values_z[] = new double[4];
			  ParameterBlock z = new ParameterBlock(values_z, 4, -1);

			  Vector<ParameterBlock> parameters = new Vector<ParameterBlock>();
			  parameters.add(x);
			  parameters.add(y);
			  parameters.add(z);

			  TernaryCostFunction cost_function = new TernaryCostFunction(3, 2, 3, 4);

			  // Create the object under tests.
			  ResidualBlock residual_block = new ResidualBlock(cost_function, null, parameters, -1);

			  // Verify getters.
			  if (cost_function != residual_block.cost_function()) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() cost_function != residual_block.cost_function()");
				  passed = false;
			  }
			  if(residual_block.loss_function() != null) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() residual_block.loss_function() != null");
				  passed = false;  
			  }
			  if (parameters.get(0) != residual_block.parameter_blocks()[0]) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() parameters.get(0) != residual_block.parameter_blocks()[0]");
				  passed = false;  
			  }
			  if (parameters.get(1) != residual_block.parameter_blocks()[1]) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() parameters.get(1) != residual_block.parameter_blocks()[1]");
				  passed = false;  
			  }
			  if (parameters.get(2) != residual_block.parameter_blocks()[2]) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() parameters.get(2) != residual_block.parameter_blocks()[2]");
				  passed = false;  
			  }
			  if (residual_block.NumScratchDoublesForEvaluate() != 3) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() residual_block.NumScratchDoublesForEvaluate() != 3");
				  passed = false;  
			  }

			  // Verify cost-only evaluation.
			  double cost[] = new double[1];
			  residual_block.Evaluate(true, cost, null, null, scratch);
			  if (cost[0] != 0.5 * (0*0 + 1*1 + 2*2)) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() cost[0] != 0.5 * (0*0 + 1*1 + 2*2)");
				  passed = false;   
			  }

			  // Verify cost and residual evaluation.
			  double residuals[] = new double[3];
			  residual_block.Evaluate(true, cost, residuals, null, scratch);
			  if (cost[0] != 0.5 * (0*0 + 1*1 + 2*2)) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() cost[0] != 0.5 * (0*0 + 1*1 + 2*2)");
				  passed = false;   
			  }
			  if (residuals[0] != 0.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() residuals[0] != 0.0");
				  passed = false;  
			  }
			  if (residuals[1] != 1.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() residuals[1] != 1.0");
				  passed = false;  
			  }
			  if (residuals[2] != 2.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() residuals[2] != 2.0");
				  passed = false;  
			  }

			  // Verify cost, residual, and jacobian evaluation.
			  cost[0] = 0.0;
			  residuals[0] = 0.0;
			  residuals[1] = 0.0;
			  residuals[2] = 0.0;

			  Matrix jacobian_rx = new Matrix(3, 2, -1.0);
			  double jacobian_rx_data[] = new double[6];
			  for (i = 0; i < 6; i++) {
				  jacobian_rx_data[i] = -1;
			  }
			  Matrix jacobian_ry = new Matrix(3, 3, -1.0);
			  double jacobian_ry_data[] = new double[9];
			  for (i = 0; i < 9; i++) {
				  jacobian_ry_data[i] = -1;
			  }
			  Matrix jacobian_rz = new Matrix(3, 4, -1.0);
			  double jacobian_rz_data[] = new double[12];
			  for (i = 0; i < 12; i++) {
				  jacobian_rz_data[i] = -1;
			  }
			  
			  double jacobian_ptrs[][] = new double[3][];
			  jacobian_ptrs[0] = jacobian_rx_data;
			  jacobian_ptrs[1] = jacobian_ry_data;
			  jacobian_ptrs[2] = jacobian_rz_data;

			  residual_block.Evaluate(true, cost, residuals, jacobian_ptrs, scratch);
			  if (cost[0] != 0.5 * (0*0 + 1*1 + 2*2)) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() cost[0] != 0.5 * (0*0 + 1*1 + 2*2)");
				  passed = false;   
			  }
			  if (residuals[0] != 0.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() residuals[0] != 0.0");
				  passed = false;  
			  }
			  if (residuals[1] != 1.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() residuals[1] != 1.0");
				  passed = false;  
			  }
			  if (residuals[2] != 2.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() residuals[2] != 2.0");
				  passed = false;  
			  }

			  for (i = 0; i < jacobian_rx_data.length; i++) {
				  if (jacobian_rx_data[i] != 0.0) {
					  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() jacobian_rx_data["+i+"] != 0.0");
					  passed = false;    
				  }
			  }
			  for (i = 0; i < jacobian_ry_data.length; i++) {
				  if (jacobian_ry_data[i] != 1.0) {
					  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() jacobian_ry_data["+i+"] != 1.0");
					  passed = false;    
				  }
			  }
			  for (i = 0; i < jacobian_rz_data.length; i++) {
				  if (jacobian_rz_data[i] != 2.0) {
					  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() jacobian_rz_data["+i+"] != 2.0");
					  passed = false;    
				  }
			  }

			  // Verify cost, residual, and partial jacobian evaluation.
			  cost[0] = 0.0;
			  residuals[0] = 0.0;
			  residuals[1] = 0.0;
			  residuals[2] = 0.0;
			  for (i = 0; i < 6; i++) {
				  jacobian_rx_data[i] = -1;
			  }
			  for (i = 0; i < 9; i++) {
				  jacobian_ry_data[i] = -1;
			  }
			  for (i = 0; i < 12; i++) {
				  jacobian_rz_data[i] = -1;
			  }

			  jacobian_ptrs[1] = null;  // Don't compute the jacobian for y.

			  residual_block.Evaluate(true, cost, residuals, jacobian_ptrs, scratch);
			  if (cost[0] != 0.5 * (0*0 + 1*1 + 2*2)) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() cost[0] != 0.5 * (0*0 + 1*1 + 2*2)");
				  passed = false;   
			  }
			  if (residuals[0] != 0.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() residuals[0] != 0.0");
				  passed = false;  
			  }
			  if (residuals[1] != 1.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() residuals[1] != 1.0");
				  passed = false;  
			  }
			  if (residuals[2] != 2.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() residuals[2] != 2.0");
				  passed = false;  
			  }

			  for (i = 0; i < jacobian_rx_data.length; i++) {
				  if (jacobian_rx_data[i] != 0.0) {
					  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() jacobian_rx_data["+i+"] != 0.0");
					  passed = false;    
				  }
			  }
			  for (i = 0; i < jacobian_ry_data.length; i++) {
				  if (jacobian_ry_data[i] != -1.0) {
					  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() jacobian_ry_data["+i+"] != -1.0");
					  passed = false;    
				  }
			  }
			  for (i = 0; i < jacobian_rz_data.length; i++) {
				  if (jacobian_rz_data[i] != 2.0) {
					  System.err.println("In TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() jacobian_rz_data["+i+"] != 2.0");
					  passed = false;    
				  }
			  }

			  if (passed) {
				  System.out.println("TESTResidualBlockEvaluteWithNoLossFunctionOrLocalParameterizations() passed all tests");
			  }
			}

		// Trivial cost function that accepts three arguments.
		class LocallyParameterizedCostFunction extends SizedCostFunction {
			public LocallyParameterizedCostFunction() {
				super(3,2,3,4,0,0,0,0,0,0,0);
			}
			public LocallyParameterizedCostFunction(int kNumResiduals, int N0, int N1, int N2, int N3, int N4, int N5, int N6, int N7,
					int N8, int N9, int num_residuals) {
			   super(kNumResiduals, N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, num_residuals);
		   }
		   public LocallyParameterizedCostFunction(int kNumResiduals, int N0, int N1, int N2, int N3, int N4, int N5, int N6, int N7,
					int N8, int N9) {
			   super(kNumResiduals, N0, N1, N2, N3, N4, N5, N6, N7, N8, N9);
		   }
		 
		   public boolean Evaluate(Vector<double[]> parameters,
                   double[] residuals,
                   double[][] jacobians) {
			int r, c, index;
		    for (int i = 0; i < num_residuals(); ++i) {
		      residuals[i] = i;
		    }
		    if (jacobians != null) {
		      for (int k = 0; k < 3; ++k) {
		        // The jacobians here are full sized, but they are transformed in the
		        // evaluator into the "local" jacobian. In the tests, the "subset
		        // constant" parameterization is used, which should pick out columns
		        // from these jacobians. Put values in the jacobian that make this
		        // obvious; in particular, make the jacobians like this:
		        //
		        //   0 1 2 3 4 ...
		        //   0 1 2 3 4 ...
		        //   0 1 2 3 4 ...
		        //
		        if (jacobians[k] != null) {
		        	for (index = 0, r = 0; r < num_residuals(); r++) {
			        	  for (c = 0; c < parameter_block_sizes().get(k); c++, index++) {
			        		  if (c < k+2) {
			        		      jacobians[k][index] = c;
			        		  }
			        	  }
			          }
		        }
		      }
		    }
		    return true;
		  }
		   
		   public boolean Evaluate(Vector<double[]> parameters,
                   double[] residuals,
                   double[][] jacobians,
                   int[] jacobians_offset) {
			int r, c, index;
		    for (int i = 0; i < num_residuals(); ++i) {
		      residuals[i] = i;
		    }
		    if (jacobians != null) {
		      for (int k = 0; k < 3; ++k) {
		        // The jacobians here are full sized, but they are transformed in the
		        // evaluator into the "local" jacobian. In the tests, the "subset
		        // constant" parameterization is used, which should pick out columns
		        // from these jacobians. Put values in the jacobian that make this
		        // obvious; in particular, make the jacobians like this:
		        //
		        //   0 1 2 3 4 ...
		        //   0 1 2 3 4 ...
		        //   0 1 2 3 4 ...
		        //
		        if (jacobians[k] != null) {
		        	for (index = 0, r = 0; r < num_residuals(); r++) {
			        	  for (c = 0; c < parameter_block_sizes().get(k); c++, index++) {
			        		  if (c < k+2) {
			        		      jacobians[k][jacobians_offset[k] + index] = c;
			        		  }
			        	  }
			          }
		        }
		      }
		    }
		    return true;
		  }
		};
		
		public void TESTResidualBlockEvaluteWithLocalParameterizations() {
			  // TESTResidualBlockEvaluteWithLocalParameterizations() passed all tests
			  int i;
			  boolean passed = true;
			  double scratch[] = new double[64];

			  // Prepare the parameter blocks.
			  double values_x[] = new double[2];
			  ParameterBlock x = new ParameterBlock(values_x, 2, -1);

			  double values_y[] = new double[3];
			  ParameterBlock y = new ParameterBlock(values_y, 3, -1);

			  double values_z[] = new double[4];
			  ParameterBlock z = new ParameterBlock(values_z, 4, -1);

			  Vector<ParameterBlock> parameters = new Vector<ParameterBlock>();
			  parameters.add(x);
			  parameters.add(y);
			  parameters.add(z);

			  // Make x have the first component fixed.
			  Vector<Integer> x_fixed = new Vector<Integer>();
			  x_fixed.add(0);
			  SubsetParameterization x_parameterization = new SubsetParameterization(2, x_fixed);
			  x.SetParameterization(x_parameterization);

			  // Make z have the last and last component fixed.
			  Vector<Integer> z_fixed = new Vector<Integer>();
			  z_fixed.add(2);
			  SubsetParameterization z_parameterization = new SubsetParameterization(4, z_fixed);
			  z.SetParameterization(z_parameterization);

			  LocallyParameterizedCostFunction cost_function = new LocallyParameterizedCostFunction();

			  // Create the object under tests.
			  ResidualBlock residual_block = new ResidualBlock(cost_function, null, parameters, -1);

			  // Verify getters.
			  if (cost_function != residual_block.cost_function()) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() cost_function != residual_block.cost_function()");
				  passed = false;
			  }
			  if(residual_block.loss_function() != null) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() residual_block.loss_function() != null");
				  passed = false;  
			  }
			  if (parameters.get(0) != residual_block.parameter_blocks()[0]) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() parameters.get(0) != residual_block.parameter_blocks()[0]");
				  passed = false;  
			  }
			  if (parameters.get(1) != residual_block.parameter_blocks()[1]) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() parameters.get(1) != residual_block.parameter_blocks()[1]");
				  passed = false;  
			  }
			  if (parameters.get(2) != residual_block.parameter_blocks()[2]) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() parameters.get(2) != residual_block.parameter_blocks()[2]");
				  passed = false;  
			  }
			  if (residual_block.NumScratchDoublesForEvaluate() != 3*(2 + 4) + 3) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() residual_block.NumScratchDoublesForEvaluate() != 3*(2 + 4) + 3");
				  passed = false;  
			  }

			  // Verify cost-only evaluation.
			  double cost[] = new double[1];
			  residual_block.Evaluate(true, cost, null, null, scratch);
			  if (cost[0] != 0.5 * (0*0 + 1*1 + 2*2)) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() cost[0] != 0.5 * (0*0 + 1*1 + 2*2)");
				  passed = false;   
			  }
			  

			  // Verify cost and residual evaluation.
			  double residuals[] = new double[3];
			  residual_block.Evaluate(true, cost, residuals, null, scratch);
			  if (cost[0] != 0.5 * (0*0 + 1*1 + 2*2)) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() cost[0] != 0.5 * (0*0 + 1*1 + 2*2)");
				  passed = false;   
			  }
			  if (residuals[0] != 0.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() residuals[0] != 0.0");
				  passed = false;  
			  }
			  if (residuals[1] != 1.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() residuals[1] != 1.0");
				  passed = false;  
			  }
			  if (residuals[2] != 2.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() residuals[2] != 2.0");
				  passed = false;  
			  }

			  // Verify cost, residual, and jacobian evaluation.
			  cost[0] = 0.0;
			  residuals[0] = 0.0;
			  residuals[1] = 0.0;
			  residuals[2] = 0.0;

			  Matrix jacobian_rx = new Matrix(3, 1, -1.0);
			  double jacobian_rx_data[] = new double[3];
			  for (i = 0; i < 3; i++) {
				  jacobian_rx_data[i] = -1;
			  }
			  Matrix jacobian_ry = new Matrix(3, 3, -1.0);
			  double jacobian_ry_data[] = new double[9];
			  for (i = 0; i < 9; i++) {
				  jacobian_ry_data[i] = -1;
			  }
			  Matrix jacobian_rz = new Matrix(3, 3, -1.0);
			  double jacobian_rz_data[] = new double[9];
			  for (i = 0; i < 9; i++) {
				  jacobian_rz_data[i] = -1;
			  }
			  
			  double jacobian_ptrs[][] = new double[3][];
			  jacobian_ptrs[0] = jacobian_rx_data;
			  jacobian_ptrs[1] = jacobian_ry_data;
			  jacobian_ptrs[2] = jacobian_rz_data;
			  
			  residual_block.Evaluate(true, cost, residuals, jacobian_ptrs, scratch);
			  if (cost[0] != 0.5 * (0*0 + 1*1 + 2*2)) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() cost[0] != 0.5 * (0*0 + 1*1 + 2*2)");
				  passed = false;   
			  }
			  if (residuals[0] != 0.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() residuals[0] != 0.0");
				  passed = false;  
			  }
			  if (residuals[1] != 1.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() residuals[1] != 1.0");
				  passed = false;  
			  }
			  if (residuals[2] != 2.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() residuals[2] != 2.0");
				  passed = false;  
			  }

			  double[] expected_jacobian_rx = new double[] {1.0, 1.0, 1.0};

			  double[] expected_jacobian_ry = new double[] { 0.0, 1.0, 2.0,
			                          0.0, 1.0, 2.0,
			                          0.0, 1.0, 2.0};

			  double[] expected_jacobian_rz = new double[] { 0.0, 1.0, /* 2.0, */ 3.0,  // 3rd parameter constant.
			                          0.0, 1.0, /* 2.0, */ 3.0,
			                          0.0, 1.0, /* 2.0, */ 3.0};
			  
			  for (i = 0; i < jacobian_rx_data.length; i++) {
				  if (jacobian_rx_data[i] != expected_jacobian_rx[i]) {
					  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() jacobian_rx_data["+i+"] != expected_jacobian_rx["+i+"]");
					  passed = false;    
				  }
			  }
			  for (i = 0; i < jacobian_ry_data.length; i++) {
				  if (jacobian_ry_data[i] != expected_jacobian_ry[i]) {
					  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() jacobian_ry_data["+i+"] != expected_jacobian_ry["+i+"]");
					  passed = false;    
				  }
			  }
			  for (i = 0; i < jacobian_rz_data.length; i++) {
				  if (jacobian_rz_data[i] != expected_jacobian_rz[i]) {
					  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() jacobian_rz_data["+i+"] != expected_jacobian_rz["+i+"]");
					  passed = false;    
				  }
			  }

			  // Verify cost, residual, and partial jacobian evaluation.
			  cost[0] = 0.0;
			  residuals[0] = 0.0;
			  residuals[1] = 0.0;
			  residuals[2] = 0.0;
			  for (i = 0; i < 3; i++) {
				  jacobian_rx_data[i] = -1;
			  }
			  for (i = 0; i < 9; i++) {
				  jacobian_ry_data[i] = -1;
			  }
			  for (i = 0; i < 9; i++) {
				  jacobian_rz_data[i] = -1;
			  }

			  jacobian_ptrs[1] = null;  // Don't compute the jacobian for y.

			  residual_block.Evaluate(true, cost, residuals, jacobian_ptrs, scratch);
			  if (cost[0] != 0.5 * (0*0 + 1*1 + 2*2)) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() cost[0] != 0.5 * (0*0 + 1*1 + 2*2)");
				  passed = false;   
			  }
			  if (residuals[0] != 0.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() residuals[0] != 0.0");
				  passed = false;  
			  }
			  if (residuals[1] != 1.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() residuals[1] != 1.0");
				  passed = false;  
			  }
			  if (residuals[2] != 2.0) {
				  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() residuals[2] != 2.0");
				  passed = false;  
			  }

			  for (i = 0; i < jacobian_rx_data.length; i++) {
				  if (jacobian_rx_data[i] != expected_jacobian_rx[i]) {
					  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() jacobian_rx_data["+i+"] != expected_jacobian_rx["+i+"]");
					  passed = false;    
				  }
			  }
			  for (i = 0; i < jacobian_ry_data.length; i++) {
				  if (jacobian_ry_data[i] != -1.0) {
					  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() jacobian_ry_data["+i+"] != -1.0");
					  passed = false;    
				  }
			  }
			  for (i = 0; i < jacobian_rz_data.length; i++) {
				  if (jacobian_rz_data[i] != expected_jacobian_rz[i]) {
					  System.err.println("In TESTResidualBlockEvaluteWithLocalParameterizations() jacobian_rz_data["+i+"] != expected_jacobian_rz["+i+"]");
					  passed = false;    
				  }
			  }

			  if (passed) {
				  System.out.println("TESTResidualBlockEvaluteWithLocalParameterizations() passed all tests");
			  }
			  
			}


		public void TESTIdentityParameterizationEverythingTest() {
			  // TESTIdentityParameterizationEverythingTest() passed all tests
			  int i;
			  double normSquared = 0.0;
			  double diff;
			  double norm;
			  boolean passed = true;
			  IdentityParameterization parameterization = new IdentityParameterization(3);
			  if (parameterization.GlobalSize() != 3) {
				  System.err.println("In TESTIdentityParameterizationEverythingTest() parameterization.GlobalSize() != 3");
				  passed = false;
			  }
			  if (parameterization.LocalSize() != 3) {
				  System.err.println("In TESTIdentityParameterizationEverythingTest() parameterization.LocalSize() != 3");
				  passed = false;  
			  }

			  double x[] = new double[]{1.0, 2.0, 3.0};
			  double delta[] = new double[]{0.0, 1.0, 2.0};
			  double x_plus_delta[] = new double[]{0.0, 0.0, 0.0};
			  parameterization.Plus(x, delta, x_plus_delta);
			  if (x_plus_delta[0] != 1.0) {
				  System.err.println("In TESTIdentityParameterizationEverythingTest() x_plus_delta[0] != 1.0");
				  passed = false;    
			  }
			  if (x_plus_delta[1] != 3.0) {
				  System.err.println("In TESTIdentityParameterizationEverythingTest() x_plus_delta[1] != 3.0");
				  passed = false; 	  
			  }
			  if (x_plus_delta[2] != 5.0) {
				  System.err.println("In TESTIdentityParameterizationEverythingTest() x_plus_delta[2] != 5.0");
				  passed = false; 	  
			  }

			  double jacobian[] = new double[9];
			  parameterization.ComputeJacobian(x, jacobian);
			  int k = 0;
			  for (i = 0; i < 3; ++i) {
			    for (int j = 0; j < 3; ++j, ++k) {
			      if ((i == j) && (jacobian[k] != 1.0)) {
			    	  System.err.println("In TESTIdentityParameterizationEverythingTest() for i == j jacobian["+k+"] != 1.0");
			    	  passed = false;
			      }
			      if ((i != j) && (jacobian[k] != 0.0)) {
			    	  System.err.println("In TESTIdentityParameterizationEverythingTest() for i != j jacobian["+k+"] != 0.0");
			    	  passed = false;  
			      }
			    }
			  }

			  Matrix global_matrix = new Matrix(10, 3, 1.0);
			  double global_matrix_data[] = new double[30];
			  for (i = 0; i < 30; i++) {
				  global_matrix_data[i] = 1.0;
			  }
			  Matrix local_matrix = new Matrix(10, 3, 0.0);
			  double local_matrix_data[] = new double[30];
			  parameterization.MultiplyByJacobian(x,
			                                      10,
			                                      global_matrix_data,
			                                      local_matrix_data);
			  normSquared = 0.0;
			  for (i = 0; i < 30; i++) {
				  diff = local_matrix_data[i] - global_matrix_data[i];
				  normSquared += (diff * diff);
			  }
			  norm = Math.sqrt(normSquared);
			  if (norm != 0.0) {
				  System.err.println("In TESTIdentityParameterizationEverythingTest() (local_matrix - global_matrix).norm() != 0.0");
				  passed = false;
			  }
			  
			  if (passed) {
				  System.out.println("TESTIdentityParameterizationEverythingTest() passed all tests");
			  }
			}
		
		public void TESTSubsetParameterizationNegativeParameterIndexDeathTest() {
			  // See expected error message
			  // In public SubsetParameterization indices indicating constant parameter must be greater than zero.
			  Vector<Integer> constant_parameters = new Vector<Integer>();
			  constant_parameters.add(-1);
			  SubsetParameterization parameterization = new SubsetParameterization(2, constant_parameters);
			  //EXPECT_DEATH_IF_SUPPORTED(
			      //SubsetParameterization parameterization(2, constant_parameters),
			      //"greater than zero");
			}
		
		public void TESTSubsetParameterizationGreaterThanSizeParameterIndexDeathTest() {
			  // See expected error message
			  // In public SubsetParameterization indices indicating constant parameter must be less than the size 
			  // of the parameter block.
			  Vector<Integer> constant_parameters = new Vector<Integer>();
			  constant_parameters.add(2);
			  SubsetParameterization parameterization = new SubsetParameterization(2, constant_parameters);
			  //EXPECT_DEATH_IF_SUPPORTED(
			      //SubsetParameterization parameterization(2, constant_parameters),
			      //"less than the size");
			}
		
		public void TESTSubsetParameterizationDuplicateParametersDeathTest() {
			// See expected error message
			// In public SubsetParameterization the set of constant parameters cannot contain duplicates
			Vector<Integer> constant_parameters = new Vector<Integer>();
			  constant_parameters.add(1);
			  constant_parameters.add(1);
			  SubsetParameterization parameterization = new SubsetParameterization(2, constant_parameters);
			  //EXPECT_DEATH_IF_SUPPORTED(
			      //SubsetParameterization parameterization(2, constant_parameters),
			      //"duplicates");
			}


		public void TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization1() {
			  // TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization1() passed all tests
			  boolean passed = true;
			  Vector<Integer> constant_parameters = new Vector<Integer>();
			  constant_parameters.add(0);
			  LocalParameterization subset_param =
			      new SubsetParameterization(1, constant_parameters);
			  LocalParameterization identity_param = new IdentityParameterization(2);
			  ProductParameterization product_param = new ProductParameterization(subset_param, identity_param);
			  if (product_param.GlobalSize() != 3) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization1() product_param.GlobalSize() != 3");
				  passed = false;
			  }
			  if (product_param.LocalSize() != 2) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization1() product_param.LocalSize() != 2");
				  passed = false;	  
			  }
			  double x[] = {1.0, 1.0, 1.0};
			  double delta[] = {2.0, 3.0};
			  double x_plus_delta[] = {0.0, 0.0, 0.0};
			  if (!product_param.Plus(x, delta, x_plus_delta)) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization1() product_param.Plus(x, delta, x_plus_delta) = false");
				  passed = false;	    
			  }
			  if (x_plus_delta[0] != x[0]) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization1() x_plus_delta[0] != x[0]");
				  passed = false;  
			  }
			  if (x_plus_delta[1] != x[1] + delta[0]) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization1() x_plus_delta[1] != x[1] + delta[0]");
				  passed = false;    
			  }
			  if (x_plus_delta[2] != x[2] + delta[1]) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization1() x_plus_delta[2] != x[2] + delta[1]");
				  passed = false;      
			  }

			  Matrix actual_jacobian = new Matrix(3, 2);
			  double actual_jacobian_data[] = new double[6];
			  if (!product_param.ComputeJacobian(x, actual_jacobian_data)) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization1() product_param.ComputeJacobian(x, actual_jacobian_data) = false");
				  passed = false;     
			  }
			  
			  if (passed) {
				  System.out.println("TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization1() passed all tests");
			  }
			}

		public void TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization2() {
			  // TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization2() passed all tests
			  boolean passed = true;
			  Vector<Integer> constant_parameters = new Vector<Integer>();
			  constant_parameters.add(0);
			  LocalParameterization subset_param =
			      new SubsetParameterization(1, constant_parameters);
			  LocalParameterization identity_param = new IdentityParameterization(2);
			  ProductParameterization product_param = new ProductParameterization(identity_param, subset_param);
			  if (product_param.GlobalSize() != 3) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization2() product_param.GlobalSize() != 3");
				  passed = false;
			  }
			  if (product_param.LocalSize() != 2) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization2() product_param.LocalSize() != 2");
				  passed = false;	  
			  }
			  double x[] = new double[]{1.0, 1.0, 1.0};
			  double delta[] = new double[]{2.0, 3.0};
			  double x_plus_delta[] = new double[]{0.0, 0.0, 0.0};
			  if (!product_param.Plus(x, delta, x_plus_delta)) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization2() product_param.Plus(x, delta, x_plus_delta) = false");
				  passed = false;  
			  }
			  if (x_plus_delta[0] != x[0] + delta[0]) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization2() x_plus_delta[0] != x[0] + delta[0]");
				  passed = false;    
			  }
			  if (x_plus_delta[1] != x[1] + delta[1]) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization2() x_plus_delta[1] != x[1] + delta[1]");
				  passed = false;  
			  }
			  if (x_plus_delta[2] != x[2]) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization2() x_plus_delta[2] != x[2]");
				  passed = false; 
			  }

			  Matrix actual_jacobian = new Matrix(3, 2);
			  double actual_jacobian_data[] = new double[6];
			  if (!product_param.ComputeJacobian(x, actual_jacobian_data)) {
				  System.err.println("In TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization2() product_param.ComputeJacobian(x, actual_jacobian_data) = false");
				  passed = false;   
			  }
			  
			  if (passed) {
				  System.out.println("TESTSubsetParameterizationProductParameterizationWithZeroLocalSizeSubsetParameterization2() passed all tests");
			  }
			}

		public void TESTSubsetParameterizationNormalFunctionTest() {
			// TESTSubsetParameterizationNormalFunctionTest() passed all tests
			int m,index,r,c;
			double normSquared;
			double diff;
			double norm;
			  boolean passed = true;
			  final int kGlobalSize = 4;
			  final int kLocalSize = 3;

			  double x[] = new double[]{1.0, 2.0, 3.0, 4.0}; // kGlobalSize
			  for (int i = 0; i < kGlobalSize; ++i) {
			    Vector<Integer> constant_parameters = new Vector<Integer>();
			    constant_parameters.add(i);
			    SubsetParameterization parameterization = new SubsetParameterization(kGlobalSize, constant_parameters);
			    double delta[] = new double[]{1.0, 2.0, 3.0};
			    double x_plus_delta[] = new double[kGlobalSize];

			    parameterization.Plus(x, delta, x_plus_delta);
			    int k = 0;
			    for (int j = 0; j < kGlobalSize; ++j) {
			      if (j == i)  {
			        if (x_plus_delta[j] != x[j]) {
			        	System.err.println("In TESTSubsetParameterizationNormalFunctionTest() x_plus_delta["+j+"] != x["+j+"]");
			        	passed = false;
			        }
			      } else {
			        if (x_plus_delta[j] != x[j] + delta[k++]) {
			        	System.err.println("In TESTSubsetParameterizationNormalFunctionTest() x_plus_delta["+j+"] != x["+j+"] + delta["+k+"++]");
			        	passed = false;
			        }
			      }
			    }

			    double jacobian[] = new double[kGlobalSize * kLocalSize];
			    parameterization.ComputeJacobian(x, jacobian);
			    int delta_cursor = 0;
			    int jacobian_cursor = 0;
			    for (int j = 0; j < kGlobalSize; ++j) {
			      if (j != i) {
			        for (k = 0; k < kLocalSize; ++k, jacobian_cursor++) {
			          if (delta_cursor == k) {
			        	  if (jacobian[jacobian_cursor] != 1.0) {
			        		  System.err.println("In TESTSubsetParameterizationNormalFunctionTest() jacobian["+jacobian_cursor+"] != 1.0");
					          passed = false;  
			        	  }
			          }
			          else {
			        	  if (jacobian[jacobian_cursor] != 0.0) {
			        		  System.err.println("In TESTSubsetParameterizationNormalFunctionTest() jacobian["+jacobian_cursor+"] != 0.0");
					          passed = false;  
			        	  }  
			          }
			        }
			        ++delta_cursor;
			      } else {
			        for (k = 0; k < kLocalSize; ++k, jacobian_cursor++) {
			          if (jacobian[jacobian_cursor] != 0.0) {
			        	  System.err.println("In TESTSubsetParameterizationNormalFunctionTest() jacobian["+jacobian_cursor+"] != 0.0");
				          passed = false;  
			          }
			        }
			      }
			    }

			    Matrix global_matrix = new Matrix(10, kGlobalSize, 1.0);
			    double global_matrix_data[] = new double[10 * kGlobalSize];
			    for (m = 0; m < 10 * kGlobalSize; m++) {
			    	global_matrix_data[m] = 1.0;
			    }
			    for (int row = 0; row < kGlobalSize; ++row) {
			      for (int col = 0; col < kGlobalSize; ++col) {
			        global_matrix.set(row, col, col);
			        global_matrix_data[row * kGlobalSize + col] = col;
			      }
			    }

			    Matrix local_matrix = new Matrix(10, kLocalSize, 0.0);
			    double local_matrix_data[] = new double[18 * kLocalSize];
			    parameterization.MultiplyByJacobian(x,
			                                        10,
			                                        global_matrix_data,
			                                        local_matrix_data);
			    Matrix refMatrix = new Matrix(kGlobalSize, kLocalSize);
			    for (index = 0, r = 0; r < kGlobalSize; r++) {
			    	for (c = 0; c < kLocalSize; c++, index++) {
			    		refMatrix.set(r, c, jacobian[index]);
			    	}
			    }
			    Matrix expected_local_matrix = global_matrix.times(refMatrix);
			    normSquared = 0.0;
			    for (index = 0, r = 0; r < 10; r++) {
			    	for (c = 0; c < kLocalSize; c++, index++) {
			    		diff = local_matrix_data[index] - expected_local_matrix.get(r,c);
			    		normSquared += diff * diff;
			    	}
			    }
			    norm = Math.sqrt(normSquared);   
			    //EXPECT_EQ((local_matrix - expected_local_matrix).norm(), 0.0);
			    if (norm != 0.0) {
			    	System.err.println("In TESTSubsetParameterizationNormalFunctionTest() (local_matrix - expected_local_matrix).norm() != 0.0");
			        passed = false;  	
			    }
			  }
			  if (passed) {
				  System.out.println("TESTSubsetParameterizationNormalFunctionTest() passed all tests");
			  }
			}
   
   public void SchurOrderingTestNoFixed() {
	   // SchurOrderingTestNoFixed passed all tests
	   SchurOrderingTest SOT = new SchurOrderingTest();
	   SOT.SchurOrderingTestNoFixed();
   }
   
   public void SchurOrderingTestAllFixed() {
	   // SchurOrderingTestAllFixed() passed all tests
	   SchurOrderingTest SOT = new SchurOrderingTest();
	   SOT.SchurOrderingTestAllFixed();
   }
   
   public void SchurOrderingTestOneFixed() {
	   // SchurOrderingTestOneFixed passed all tests
	   SchurOrderingTest SOT = new SchurOrderingTest();
	   SOT.SchurOrderingTestOneFixed();   
   }
   
   class DummyCostFunction extends SizedCostFunction {
	   public DummyCostFunction(int kNumResiduals, int N0, int N1, int N2, int N3, int N4, int N5, int N6, int N7,
				int N8, int N9, int num_residuals) {
		   super(kNumResiduals, N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, num_residuals);
	   }
	   public DummyCostFunction(int kNumResiduals, int N0, int N1, int N2, int N3, int N4, int N5, int N6, int N7,
				int N8, int N9) {
		   super(kNumResiduals, N0, N1, N2, N3, N4, N5, N6, N7, N8, N9);
	   }
	   public boolean Evaluate(Vector<double[]> parameters,
	                         double[] residuals,
	                         double[][] jacobians) {
	     return true;
	   }
	   
	   public boolean Evaluate(Vector<double[]> parameters,
               double[] residuals,
               double[][] jacobians,
               int[] jacobians_offset[]) {
			return true;
			}
	 };
	 
	 class SchurOrderingTest {
		 private ProblemImpl problem_ = new ProblemImpl();
		 private double x_[] = new double[3];
		 private double y_[] = new double[4];
		 private double z_[] = new double[5];
		 private double w_[] = new double[6];
		 private boolean passed;
		 
		 public SchurOrderingTest() {
			 
		 }
		 
		 public void SetUp() {
			    // The explicit calls to AddParameterBlock are necessary because
			    // the below tests depend on the specific numbering of the
			    // parameter blocks.
			    problem_.AddParameterBlock(x_, 3);
			    problem_.AddParameterBlock(y_, 4);
			    problem_.AddParameterBlock(z_, 5);
			    problem_.AddParameterBlock(w_, 6);

			    problem_.AddResidualBlock(new DummyCostFunction(2, 3,0,0,0,0,0,0,0,0,0), null, x_);
			    problem_.AddResidualBlock(new DummyCostFunction(6, 5, 4,0,0,0,0,0,0,0,0), null, z_, y_);
			    problem_.AddResidualBlock(new DummyCostFunction(3, 3, 5,0,0,0,0,0,0,0,0), null, x_, z_);
			    problem_.AddResidualBlock(new DummyCostFunction(7, 5, 3,0,0,0,0,0,0,0,0), null, z_, x_);
			    problem_.AddResidualBlock(new DummyCostFunction(1, 5, 3, 6,0,0,0,0,0,0,0), null,
			                              z_, x_, w_);
			  }
		 
		 public void SchurOrderingTestNoFixed() {
			  passed = true;
			  SetUp();
			  final Program program = problem_.program();
			  final Vector<ParameterBlock> parameter_blocks = program.parameter_blocks();
			  Graph<ParameterBlock> graph = CreateHessianGraph(program);

			  final HashSet<ParameterBlock> vertices = graph.vertices();
			  if (vertices.size() != 4) {
				  System.err.println("In SchurOrderingTestNoFixed vertices.size() != 4");
				  passed = false;
			  }

			  for (int i = 0; i < 4; ++i) {
			    if (!vertices.contains(parameter_blocks.get(i))) {
			    	System.err.println("In SchurOrderingTestNoFixed vertices.contains(parameter_blocks.get("+i+")) = false");
					passed = false;	
			    }
			  }

			  {
			    final HashSet<ParameterBlock> neighbors = graph.Neighbors(parameter_blocks.get(0));
			    if (neighbors.size() != 2) {
			    	System.err.println("In SchurOrderingTestNoFixed neighbors.size() != 2");
					passed = false;		
			    }
			    if (!neighbors.contains(parameter_blocks.get(2))) {
			    	System.err.println("In SchurOrderingTestNoFixed neighbors.contains(parameter_blocks.get(2)) = false");
					passed = false;	
			    }
			    if (!neighbors.contains(parameter_blocks.get(3))) {
			    	System.err.println("In SchurOrderingTestNoFixed neighbors.contains(parameter_blocks.get(3)) = false");
					passed = false;	
			    }
			  }

			  {
				  final HashSet<ParameterBlock> neighbors = graph.Neighbors(parameter_blocks.get(1));
				    if (neighbors.size() != 1) {
				    	System.err.println("In SchurOrderingTestNoFixed neighbors.size() != 1");
						passed = false;		
				    }
				    if (!neighbors.contains(parameter_blocks.get(2))) {
				    	System.err.println("In SchurOrderingTestNoFixed neighbors.contains(parameter_blocks.get(2)) = false");
						passed = false;	
				    }
			  }

			  {
				  final HashSet<ParameterBlock> neighbors = graph.Neighbors(parameter_blocks.get(2));
				    if (neighbors.size() != 3) {
				    	System.err.println("In SchurOrderingTestNoFixed neighbors.size() != 3");
						passed = false;		
				    }
				    if (!neighbors.contains(parameter_blocks.get(0))) {
				    	System.err.println("In SchurOrderingTestNoFixed neighbors.contains(parameter_blocks.get(0)) = false");
						passed = false;	
				    }
				    if (!neighbors.contains(parameter_blocks.get(1))) {
				    	System.err.println("In SchurOrderingTestNoFixed neighbors.contains(parameter_blocks.get(1)) = false");
						passed = false;	
				    }
				    if (!neighbors.contains(parameter_blocks.get(3))) {
				    	System.err.println("In SchurOrderingTestNoFixed neighbors.contains(parameter_blocks.get(3)) = false");
						passed = false;	
				    }
			  }

			  {
				  final HashSet<ParameterBlock> neighbors = graph.Neighbors(parameter_blocks.get(3));
				    if (neighbors.size() != 2) {
				    	System.err.println("In SchurOrderingTestNoFixed neighbors.size() != 2");
						passed = false;		
				    }
				    if (!neighbors.contains(parameter_blocks.get(0))) {
				    	System.err.println("In SchurOrderingTestNoFixed neighbors.contains(parameter_blocks.get(0)) = false");
						passed = false;	
				    }
				    if (!neighbors.contains(parameter_blocks.get(2))) {
				    	System.err.println("In SchurOrderingTestNoFixed neighbors.contains(parameter_blocks.get(2)) = false");
						passed = false;	
				    }
			    
			  }
			  
			  if (passed) {
				  System.out.println("SchurOrderingTestNoFixed passed all tests");
			  }
			} // public void SchurOrderingTestNoFixed()

		 public void SchurOrderingTestAllFixed() {
			 passed = true;
			  SetUp();
			  problem_.SetParameterBlockConstant(x_);
			  problem_.SetParameterBlockConstant(y_);
			  problem_.SetParameterBlockConstant(z_);
			  problem_.SetParameterBlockConstant(w_);

			  final Program program = problem_.program();
			  Graph<ParameterBlock> graph = CreateHessianGraph(program);
			  if (graph.vertices().size() != 0) {
				  System.err.println("In SchurOrderingTestAllFixed graph.vertices().size() != 0");
					passed = false;	  
			  }
			  
			  if (passed) {
				  System.out.println("SchurOrderingTestAllFixed() passed all tests");
			  }
			}
		 
		 public void SchurOrderingTestOneFixed() {
			 passed = true;
			  SetUp();
			  problem_.SetParameterBlockConstant(x_);
			  
			  final Program program = problem_.program();
			  final Vector<ParameterBlock> parameter_blocks = program.parameter_blocks();
			  Graph<ParameterBlock> graph = CreateHessianGraph(program);

			  final HashSet<ParameterBlock> vertices = graph.vertices();
			  
			  if (vertices.size() != 3) {
				  System.err.println("In SchurOrderingTestOneFixed vertices.size() != 3");
				  passed = false;
			  }
			  if (vertices.contains(parameter_blocks.get(0))) {
				  System.err.println("In SchurOrderingTestOneFixed vertices.contains(parameter_blocks.get(0)) = true");
				  passed = false;  
			  }

			  for (int i = 1; i < 3; ++i) {
			    if (!vertices.contains(parameter_blocks.get(i))) {
			    	System.err.println("In  SchurOrderingTestOneFixed vertices.contains(parameter_blocks.get("+i+")) = false");
					passed = false;	
			    }
			  }

			  {
				  final HashSet<ParameterBlock> neighbors = graph.Neighbors(parameter_blocks.get(1));
				    if (neighbors.size() != 1) {
				    	System.err.println("In SchurOrderingTestOneFixed neighbors.size() != 1");
						passed = false;		
				    }
				    if (!neighbors.contains(parameter_blocks.get(2))) {
				    	System.err.println("In SchurOrderingTestOneFixed neighbors.contains(parameter_blocks.get(2)) = false");
						passed = false;	
				    }
			  }

			  {
				  final HashSet<ParameterBlock> neighbors = graph.Neighbors(parameter_blocks.get(2));
				    if (neighbors.size() != 2) {
				    	System.err.println("In SchurOrderingTestNoFixed neighbors.size() != 2");
						passed = false;		
				    }
				    if (!neighbors.contains(parameter_blocks.get(1))) {
				    	System.err.println("In SchurOrderingTestOneFixed neighbors.contains(parameter_blocks.get(1)) = false");
						passed = false;	
				    }
				    if (!neighbors.contains(parameter_blocks.get(3))) {
				    	System.err.println("In SchurOrderingTestOneFixed neighbors.contains(parameter_blocks.get(3)) = false");
						passed = false;	
				    }
			  }

			  {
				  final HashSet<ParameterBlock> neighbors = graph.Neighbors(parameter_blocks.get(3));
				    if (neighbors.size() != 1) {
				    	System.err.println("In SchurOrderingTestOneFixed neighbors.size() != 1");
						passed = false;		
				    }
				    if (!neighbors.contains(parameter_blocks.get(2))) {
				    	System.err.println("In SchurOrderingTestOneFixed neighbors.contains(parameter_blocks.get(2)) = false");
						passed = false;	
				    }
			  }

			  // The constant parameter block is at the end.
			  Vector<ParameterBlock> ordering = new Vector<ParameterBlock>();
			  ComputeSchurOrdering(program, ordering);
			  if (ordering.lastElement() != parameter_blocks.get(0)) {
				  System.err.println("In SchurOrderingTestOneFixed ordering.lastElement() != parameter_blocks.get(0)");
					passed = false;	  
			  }
			  
			  if (passed) {
				  System.out.println("SchurOrderingTestOneFixed passed all tests");
			  }
			}


	 
	 }

 
   
   public void BlockJacobiPreconditionerTestSmallProblem() {
	   // In BlockJacobiPreconditionerTest problem_id = 2 passed all tests
		BlockJacobiPreconditionerTest BJP = new BlockJacobiPreconditionerTest();
	    BJP.VerifyDiagonalBlocks(2);
	  }

	  public void BlockJacobiPreconditionerTestLargeProblem() {
		  // In BlockJacobiPreconditionerTest problem_id = 3 passed all tests
		  BlockJacobiPreconditionerTest BJP = new BlockJacobiPreconditionerTest();
	      BJP.VerifyDiagonalBlocks(3);
	  }
   
   class BlockJacobiPreconditionerTest  { 
	   private BlockSparseMatrix A;
	   private double[] D;
	   private Matrix dense_ata;
	   private boolean passed;
	   public BlockJacobiPreconditionerTest() {
		   passed = true;
	   }
   public void SetUpFromProblemId(int problem_id) {
	      int i;
	      LinearLeastSquaresProblem problem = 
	          CreateLinearLeastSquaresProblemFromId(problem_id);

	      if (problem == null) {
	    	  System.err.println("In BlockJacobiPreconditionerTest problem_id = " + problem_id + " problem == null");
	    	  passed = false;
	      }
	      A = (BlockSparseMatrix)(problem.A);
	      D = problem.D;

	      Matrix dense_a = A.ToDenseMatrix();
	      dense_ata = (dense_a.transpose()).times(dense_a);
	      for (i = 0; i < A.num_cols(); i++) {
	    	  dense_ata.set(i, i, dense_ata.getArray()[i][i] + D[i]*D[i]);
	      }
	    }

	    public void VerifyDiagonalBlocks(int problem_id) {
	      int i, row, col, index;
	      SetUpFromProblemId(problem_id);

	      BlockJacobiPreconditioner pre = new BlockJacobiPreconditioner(A);
	      pre.Update(A, D);
	      BlockRandomAccessDiagonalMatrix m =
	          (BlockRandomAccessDiagonalMatrix)(pre.matrix());
	      if (m.num_rows() != A.num_cols()) {
	    	  System.err.println("In BlockJacobiPreconditionerTest problem_id = " + problem_id + " m.num_rows() != A.num_cols()");
	    	  passed = false;
	      }
	      if (m.num_cols() != A.num_cols()) {
	    	  System.err.println("In BlockJacobiPreconditionerTest problem_id = " + problem_id + " m.num_cols() != A.num_cols()");
	    	  passed = false;
	      }

	      final CompressedRowBlockStructure bs = A.block_structure();
	      for (i = 0; i < bs.cols.size(); ++i) {
	        final int block_size = bs.cols.get(i).size;
	        int r[] = new int[1];
	        int c[] = new int[1];
	        int row_stride[] = new int[1];
	        int col_stride[] = new int[1];
	        CellInfo cell_info = m.GetCell(i, i,
	                                         r, c,
	                                         row_stride, col_stride);
	        double marr[][] = new double[row_stride[0]][col_stride[0]];
	        for (index = 0, row = 0; row < row_stride[0]; row++) {
	        	for (col = 0; col < col_stride[0]; col++, index++) {
	        	    marr[row][col] = cell_info.values[cell_info.values_index + index];	
	        	}
	        double inverse_arr[][] = new double[block_size][block_size];
	        for (row = r[0]; row < r[0] + block_size; row++) {
	        	for (col = c[0]; col < c[0] + block_size; col++) {
	        		inverse_arr[row - r[0]][col - c[0]] = marr[row][col];
	        	}
	        }
	        Matrix actual_block_inverse = new Matrix(inverse_arr);
	        double expected_arr[][] = new double[block_size][block_size];
	        for (row = bs.cols.get(i).position; row < bs.cols.get(i).position + block_size; row++) {
	        	for (col = bs.cols.get(i).position; col < bs.cols.get(i).position + block_size; col++) {
	        		expected_arr[row-bs.cols.get(i).position][col-bs.cols.get(i).position] = dense_ata.getArray()[row][col];
	        	}
	        }
	        Matrix expected_block = new Matrix(expected_arr);
	        double prod[][] = (actual_block_inverse.times(expected_block)).getArray();
	        for (row = 0; row < block_size; row++) {
	        	prod[row][row] -= 1.0;
	        }
	        double squaredResidual = 0.0;
	        for (row = 0; row < prod.length; row++) {
	        	for (col = 0; col < prod[0].length; col++) {
	        		squaredResidual += (prod[row][col] * prod[row][col]);
	        	}
	        }
	        double residual = Math.sqrt(squaredResidual);
	        if (residual > 1.0E-12) {
	        	System.err.println("In BlockJacobiPreconditionerTest problem_id = " + problem_id + " residual for Block " + i + " = " + residual);
		    	  passed = false;    	
	        }
	        
	      }
	    }
        if (passed) {
        	System.out.println("In BlockJacobiPreconditionerTest problem_id = " + problem_id + " passed all tests");
        }
	    
	  }

	  
   }
   
   public void IterativeSchurComplementSolverTestNormalProblem() {
	   // IterativeSchurComplementSolverTestNormalProblem() passed all the tests
	   IterativeSchurComplementSolverTest ISC = new IterativeSchurComplementSolverTest(); 
	   ISC.IterativeSchurComplementSolverTestNormalProblem();  
   }
   
   public void IterativeSchurComplementSolverTestProblemWithNoFBlocks() {
	   // IterativeSchurComplementSolverTestProblemWithNoFBlocks() passed all the tests
	   IterativeSchurComplementSolverTest ISC = new IterativeSchurComplementSolverTest(); 
	   ISC.IterativeSchurComplementSolverTestProblemWithNoFBlocks();  
   }

	   

   class IterativeSchurComplementSolverTest {
	   final double kEpsilon = 1e-14;
	   private int num_rows_;
	   private int num_cols_;
	   private int num_eliminate_blocks_;
	   private BlockSparseMatrix A_;
	   private double[] b_;
	   private double[] D_;
	   
	   public IterativeSchurComplementSolverTest() {
		  
	   }
	   
	   public void IterativeSchurComplementSolverTestNormalProblem() {
		     boolean passed = true;
		     SetUpProblem(2);
		     if (!TestSolver(null)) {
		    	 passed = false;
		     }
		     if (!TestSolver(D_)) {
		    	 passed = false;
		     }
		     if (passed) {
		    	 System.out.println("IterativeSchurComplementSolverTestNormalProblem() passed all the tests");
		     }
		   }
	   
	   public void IterativeSchurComplementSolverTestProblemWithNoFBlocks() {
		     boolean passed = true;
		     SetUpProblem(3);
		     if (!TestSolver(null)) {
		    	 passed = false;
		     }
		     if (!TestSolver(D_)) {
		    	 passed = false;
		     }
		     if (passed) {
		    	 System.out.println("IterativeSchurComplementSolverTestProblemWithNoFBlocks() passed all the tests");
		     }
		   }
   
     public void SetUpProblem(int problem_id) {
       LinearLeastSquaresProblem problem = 
           CreateLinearLeastSquaresProblemFromId(problem_id);

           if (problem == null) {
 	    	  System.err.println("In IterativeSchurComplementSolverTest problem_id = " + problem_id + " problem == null");
 	      }
       A_ = (BlockSparseMatrix)(problem.A);
       b_ = problem.b;
       D_ = problem.D;

       num_cols_ = A_.num_cols();
       num_rows_ = A_.num_rows();
       num_eliminate_blocks_ = problem.num_eliminate_blocks;
     }

     public boolean TestSolver(double[] D) {
       int i;
       TripletSparseMatrix triplet_A = new TripletSparseMatrix(A_.num_rows(),
                                     A_.num_cols(),
                                     A_.num_nonzeros());
       A_.ToTripletSparseMatrix(triplet_A);

       DenseSparseMatrix dense_A = new DenseSparseMatrix(triplet_A);

       LinearSolverOptions options = new LinearSolverOptions();
       options.type = LinearSolverType.DENSE_QR;
       Context context = new Context();
       options.context = context;
       DenseQRSolver qr = (DenseQRSolver)Create(options);

       LinearSolverPerSolveOptions per_solve_options = new LinearSolverPerSolveOptions();
       per_solve_options.D = D;
       double[] reference_solution = new double[num_cols_];
       qr.Solve(dense_A, b_, per_solve_options, reference_solution);

       options.elimination_groups.add(num_eliminate_blocks_);
       options.elimination_groups.add(0);
       options.max_num_iterations = num_cols_;
       options.preconditioner_type = PreconditionerType.SCHUR_JACOBI;
       IterativeSchurComplementSolver isc = new IterativeSchurComplementSolver(options);

       double[] isc_sol = new double[num_cols_];
       per_solve_options.r_tolerance  = 1e-12;
       isc.Solve(A_, b_, per_solve_options, isc_sol);
       double squaredNorm = 0.0;
       for (i = 0; i < isc_sol.length; i++) {
    	   double diff = isc_sol[i] - reference_solution[i];
    	   squaredNorm += (diff * diff);
       }
       double norm = Math.sqrt(squaredNorm);
       if (norm < kEpsilon) {
         return true;
       } else {
    	 System.err.println("In IterativeSchurComplementSolverTest the reference solution differs from the ITERATIVE_SCHUR"
             + " solution by " + norm + " which is more than " + kEpsilon);
         return false;
             
       }
     }

     
   };
   
   public void PartitionedMatrixViewTestDimensionsTest() {
	   PartitionedMatrixViewTest PMV = new PartitionedMatrixViewTest();
	   PMV.PartitionedMatrixViewTestDimensionsTest();
   }
   
   public void PartitionedMatrixViewTestRightMultiplyE() {
	   PartitionedMatrixViewTest PMV = new PartitionedMatrixViewTest();
	   PMV.PartitionedMatrixViewTestRightMultiplyE();   
   }
   
   public void PartitionedMatrixViewTestRightMultiplyF() {
	   PartitionedMatrixViewTest PMV = new PartitionedMatrixViewTest();
	   PMV.PartitionedMatrixViewTestRightMultiplyF();   
   }
   
   public void PartitionedMatrixViewTestLeftMultiply() {
	   PartitionedMatrixViewTest PMV = new PartitionedMatrixViewTest();
	   PMV.PartitionedMatrixViewTestLeftMultiply();   
   }
   
   public void PartitionedMatrixViewTestBlockDiagonalEtE() {
	   PartitionedMatrixViewTest PMV = new PartitionedMatrixViewTest();
	   PMV.PartitionedMatrixViewTestBlockDiagonalEtE();      
   }
   
   public void PartitionedMatrixViewTestBlockDiagonalFtF() {
	   PartitionedMatrixViewTest PMV = new PartitionedMatrixViewTest();
	   PMV.PartitionedMatrixViewTestBlockDiagonalFtF();      
   }

   class PartitionedMatrixViewTest {
	   // All PartitionMatrixViewTests pass
	   private final double kEpsilon = 1e-14;
	   private int num_rows_;
	   private int num_cols_;
	   private int num_eliminate_blocks_;
	   private SparseMatrix A_;
	   private PartitionedMatrixView pmv_;
	   RandomNumberGen ran = new RandomNumberGen();
	   
	   public PartitionedMatrixViewTest() {
		   
	   }
    
	   public void SetUp() {
       LinearLeastSquaresProblem problem =
           CreateLinearLeastSquaresProblemFromId(2);
       if (problem == null) {
    	   System.err.println("In PartitionedMatrixViewTest SetUp problem == null");
    	   return;
       }
       A_ = problem.A;

       num_cols_ = A_.num_cols();
       num_rows_ = A_.num_rows();
       num_eliminate_blocks_ = problem.num_eliminate_blocks;
       LinearSolverOptions options = new LinearSolverOptions();
       options.elimination_groups.add(num_eliminate_blocks_);
       pmv_= createPartitionedMatrixView(
                      options,
                      (BlockSparseMatrix)(A_));
     }
	   
	   public void PartitionedMatrixViewTestDimensionsTest() {
		     boolean passed = true;
		     SetUp();
		     if (pmv_.num_col_blocks_e() != num_eliminate_blocks_) {
		    	 System.err.println("In PartitionedMatrixViewTestDimensionsTest() pmv_.num_col_blocks_e() != num_eliminate_blocks_");
		    	 passed = false;
		     }
		     if (pmv_.num_col_blocks_f() != (num_cols_ - num_eliminate_blocks_)) {
		    	 System.err.println("In PartitionedMatrixViewTestDimensionsTest() pmv_.num_col_blocks_f() != (num_cols_ - num_eliminate_blocks_)");
		    	 passed = false;	 
		     }
		     if (pmv_.num_cols_e() != num_eliminate_blocks_) {
		    	 System.err.println("In PartitionedMatrixViewTestDimensionsTest() pmv_.num_cols_e() != num_eliminate_blocks_");
		    	 passed = false;	 
		     }
		     if (pmv_.num_cols_f() != (num_cols_ - num_eliminate_blocks_)) {
		    	 System.err.println("In PartitionedMatrixViewTestDimensionsTest() pmv_.num_cols_f() != (num_cols_ - num_eliminate_blocks_)");
		    	 passed = false;	 
		     }
		     if (pmv_.num_cols() != A_.num_cols()) {
		    	 System.err.println("In PartitionedMatrixViewTestDimensionsTest() pmv_.num_cols() != A_.num_cols()");
		    	 passed = false;		 
		     }
		     if (pmv_.num_rows() != A_.num_rows()) {
		    	 System.err.println("In PartitionedMatrixViewTestDimensionsTest() pmv_.num_rows() != A_.num_rows()");
		    	 passed = false;	 
		     }
		     if (passed) {
		    	 System.out.println("PartitionedMatrixViewTestDimensionsTest() passed all tests");
		     }
		   }
	   
	   public void PartitionedMatrixViewTestRightMultiplyE() {
		   boolean passed = true;
		     SetUp();
		     double x1[] = new double[pmv_.num_cols_e()];
		     double x2[] = new double[pmv_.num_cols()];

		     for (int i = 0; i < pmv_.num_cols_e(); ++i) {
		       x1[i] = x2[i] = ran.genGaussianRandomNum(-1.0,1.0);
		     }

		     double y1[] = new double[pmv_.num_rows()];
		     pmv_.RightMultiplyE(x1, y1);

		     double y2[] = new double[pmv_.num_rows()];
		     A_.RightMultiply(x2, y2);

		     for (int i = 0; i < pmv_.num_rows(); ++i) {
		       if (Math.abs(y1[i] - y2[i]) > kEpsilon) {
		    	   passed = false;
		    	   System.err.println("In PartitionedMatrixViewTestRightMultiplyE() y1["+i+"] = " + y1[i] + "y2["+i+"] = " + y2[i]);
		    	   passed = false;
		       }
		     }
		     
		     if (passed) {
		    	 System.out.println("PartitionedMatrixViewTestRightMultiplyE() passed all tests");
		     }
		   }
	   
	   public void PartitionedMatrixViewTestRightMultiplyF() {
		   boolean passed = true;
		     SetUp();
		     double x1[] = new double[pmv_.num_cols_f()];
		     double x2[] = new double[pmv_.num_cols()];

		     for (int i = 0; i < pmv_.num_cols_f(); ++i) {
		       x1[i] = ran.genGaussianRandomNum(-1.0,1.0);
		       x2[i + pmv_.num_cols_e()] = x1[i];
		     }
  
		     double y1[] = new double[pmv_.num_rows()];
		     pmv_.RightMultiplyF(x1, y1);

		     double y2[] = new double[pmv_.num_rows()];
		     A_.RightMultiply(x2, y2);

		     for (int i = 0; i < pmv_.num_rows(); ++i) {
		    	 if (Math.abs(y1[i] - y2[i]) > kEpsilon) {
			    	   passed = false;
			    	   System.err.println("In PartitionedMatrixViewTestRightMultiplyF() y1["+i+"] = " + y1[i] + "y2["+i+"] = " + y2[i]);
			    	   passed = false;
			       }
		     }
		     
		     if (passed) {
		    	 System.out.println("PartitionedMatrixViewTestRightMultiplyF() passed all tests");
		     }
		   }
	   
	   public void PartitionedMatrixViewTestLeftMultiply() {
		   boolean passed = true;
		     SetUp();
		     double x[] = new double[pmv_.num_rows()];
		     for (int i = 0; i < pmv_.num_rows(); ++i) {
		       x[i] = ran.genGaussianRandomNum(-1.0,1.0);
		     }

		     double y[] = new double[pmv_.num_cols()];
		     double y1[] = new double[pmv_.num_cols_e()];
		     double y2[] = new double[pmv_.num_cols_f()];

		     A_.LeftMultiply(x, y);
		     pmv_.LeftMultiplyE(x, y1);
		     pmv_.LeftMultiplyF(x, y2);

		     for (int i = 0; i < pmv_.num_cols(); ++i) {
		    	 if (i < pmv_.num_cols_e()) {
		    		 if (Math.abs(y[i] - y1[i]) > kEpsilon) {
				    	   passed = false;
				    	   System.err.println("In PartitionedMatrixViewTestLeftMultiply() y["+i+"] = " + y[i] + "y1["+i+"] = " + y1[i]);
				    	   passed = false;
				       }	 
		    	 }
		    	 else {
		    		 if (Math.abs(y[i] - y2[i - pmv_.num_cols_e()]) > kEpsilon) {
				    	   passed = false;
				    	   System.err.println("In PartitionedMatrixViewTestLeftMultiply() y["+i+"] = " + y[i] + "y2["+(i-pmv_.num_cols_e())+"] = " + 
				    	   y2[i - pmv_.num_cols_e()]);
				    	   passed = false;
				       }		 
		    	 }
		     }
		     if (passed) {
		    	 System.out.println("PartitionedMatrixViewTestLeftMultiply() passed all tests");
		     }
		   }
	   
	   public void PartitionedMatrixViewTestBlockDiagonalEtE() {
		   boolean passed = true;
		     SetUp();
		     BlockSparseMatrix
		         block_diagonal_ee = pmv_.CreateBlockDiagonalEtE();
		     final CompressedRowBlockStructure bs  = block_diagonal_ee.block_structure();

		     if (block_diagonal_ee.num_rows() != 2) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalEtE() block_diagonal_ee.num_rows() != 2");
		    	 passed = false;
		     }
		     if (block_diagonal_ee.num_cols() != 2) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalEtE() block_diagonal_ee.num_cols() != 2");
		    	 passed = false;	 
		     }
		     if (bs.cols.size() != 2) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalEtE() bs.cols.size() != 2");
		    	 passed = false;
		     }
		     if (bs.rows.size() != 2) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalEtE() bs.rows.size() != 2");
		    	 passed = false;
		     }

		     if (Math.abs(block_diagonal_ee.values()[0] - 10.0) > kEpsilon) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalEtE() Math.abs(block_diagonal_ee.values()[0] - 10.0) > kEpsilon");
		    	 passed = false;	 
		     }
		     if (Math.abs(block_diagonal_ee.values()[1] - 155.0) > kEpsilon) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalEtE() Math.abs(block_diagonal_ee.values()[1] - 155.0) > kEpsilon");
		    	 passed = false;		 
		     }
		     
		     if (passed) {
		    	 System.out.println("PartitionedMatrixViewTestBlockDiagonalEtE() passed all tests");
		     }
		   }
	   
	   public void PartitionedMatrixViewTestBlockDiagonalFtF() {
		   boolean passed = true;
		     SetUp();
		     BlockSparseMatrix
		         block_diagonal_ff = pmv_.CreateBlockDiagonalFtF();
		     final CompressedRowBlockStructure bs  = block_diagonal_ff.block_structure();

		     if (block_diagonal_ff.num_rows() != 3) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalFtF() block_diagonal_ff.num_rows() != 3");
		    	 passed = false;
		     }
		     if (block_diagonal_ff.num_cols() !=  3) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalFtF() block_diagonal_ff.num_cols() != 3");
		    	 passed = false;	 
		     }
		     if (bs.cols.size() != 3) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalFtF() bs.cols.size() != 3");
		    	 passed = false;	 
		     }
		     if (bs.rows.size() != 3) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalFtF() bs.rows.size() != 3");
		    	 passed = false;	 
		     }
		     if (Math.abs(block_diagonal_ff.values()[0] - 70.0) > kEpsilon) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalFtF() Math.abs(block_diagonal_ff.values()[0] - 70.0) > kEpsilon");
		    	 passed = false;		 
		     }
		     if (Math.abs(block_diagonal_ff.values()[1] - 17.0) > kEpsilon) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalFtF() Math.abs(block_diagonal_ff.values()[1] - 17.0) > kEpsilon");
		    	 passed = false;		 
		     }
		     if (Math.abs(block_diagonal_ff.values()[2] - 37.0) > kEpsilon) {
		    	 System.err.println("In PartitionedMatrixViewTestBlockDiagonalFtF() Math.abs(block_diagonal_ff.values()[2] - 37.0) > kEpsilon");
		    	 passed = false;		 
		     }
		     if (passed) {
		         System.out.println("PartitionedMatrixViewTestBlockDiagonalFtF() passed all tests");	 
		     }
	   }

     
   };
   
   public void BlockSparseMatrixTestSetZeroTest() {
	   BlockSparseMatrixTest BSM = new BlockSparseMatrixTest();
	   BSM.BlockSparseMatrixTestSetZeroTest();
   }
   
   public void BlockSparseMatrixTestRightMultiplyTest() {
	   BlockSparseMatrixTest BSM = new BlockSparseMatrixTest();
	   BSM.BlockSparseMatrixTestRightMultiplyTest();   
   }
   
   public void BlockSparseMatrixTestLeftMultiplyTest() {
	   BlockSparseMatrixTest BSM = new BlockSparseMatrixTest();
	   BSM.BlockSparseMatrixTestLeftMultiplyTest();   
   }
   
   public void BlockSparseMatrixTestSquaredColumnNormTest() {
	   BlockSparseMatrixTest BSM = new BlockSparseMatrixTest();
	   BSM.BlockSparseMatrixTestSquaredColumnNormTest();
   }
   
   public void BlockSparseMatrixTestToDenseMatrixTest() {
	   BlockSparseMatrixTest BSM = new BlockSparseMatrixTest();
	   BSM.BlockSparseMatrixTestToDenseMatrixTest();   
   }
   
   public void BlockSparseMatrixTestAppendRows() {
	   BlockSparseMatrixTest BSM = new BlockSparseMatrixTest();
	   BSM.BlockSparseMatrixTestAppendRows();   
   }
   
   public void BlockSparseMatrixTestAppendAndDeleteBlockDiagonalMatrix() {
	   BlockSparseMatrixTest BSM = new BlockSparseMatrixTest();
	   BSM.BlockSparseMatrixTestAppendAndDeleteBlockDiagonalMatrix();   
   }
   
   public void BlockSparseMatrixCreateDiagonalMatrix() {
	   BlockSparseMatrixTest BSM = new BlockSparseMatrixTest();
	   BSM.BlockSparseMatrixCreateDiagonalMatrix();   
   }
   
   class BlockSparseMatrixTest  {
	   // BlockSparseMatrixTestSetZeroTest() passed all tests
	   // BlockSparseMatrixTestRightMultiplyTest() passed all tests
	   // BlockSparseMatrixTestLeftMultiplyTest() passed all tests
	   // BlockSparseMatrixTestSquaredColumnNormTest() passed all tests
	   // BlockSparseMatrixTestToDenseMatrixTest() passed all tests
	   // BlockSparseMatrixTestAppendRows() passed all tests
	   // BlockSparseMatrixTestAppendAndDeleteBlockDiagonalMatrix() passed all tests
	   // BlockSparseMatrixCreateDiagonalMatrix() passed all tests
	   private BlockSparseMatrix A_;
	   private TripletSparseMatrix B_;
	   
	   public BlockSparseMatrixTest() {
		   
	   }
	   
	   public void SetUp() {
	      LinearLeastSquaresProblem problem = 
	          CreateLinearLeastSquaresProblemFromId(2);
	      if (problem == null) {
	    	  System.err.println("in BlockSparseMatrixTest SetUp problem from Id2 == null");
	    	  return;
	      }
	      A_ = (BlockSparseMatrix)(problem.A);

	      problem = CreateLinearLeastSquaresProblemFromId(1);
	      if (problem == null) {
	    	  System.err.println("in BlockSparseMatrixTest SetUp problem from Id1 == null");
	    	  return;
	      }
	      B_ = (TripletSparseMatrix)(problem.A);

	      if (A_.num_rows() != B_.num_rows()) {
	    	  System.err.println("in BlockSparseMatrixTest SetUp A_.num_rows() != B_.num_rows()");
	    	  return;  
	      }
	      if (A_.num_cols() != B_.num_cols()) {
	    	  System.err.println("in BlockSparseMatrixTest SetUp A_.num_cols() != B_.num_cols()");
	    	  return;    
	      }
	      if (A_.num_nonzeros() != B_.num_nonzeros()) {
	    	  System.err.println("in BlockSparseMatrixTest SetUp A_.num_nonzeros() != B_.num_nonzeros()");
	    	  return;  
	      }
	    }

	   public void BlockSparseMatrixTestSetZeroTest() {
			  boolean passed = true;
				 SetUp();
		    A_.SetZero();
		    if (13 != A_.num_nonzeros()) {
		    	System.err.println("In BlockSparseMatrixTestSetZeroTest() A_.num_nonzeros() != 13");
		    	passed = false;
		    }
		    if (passed) {
		    	System.out.println("BlockSparseMatrixTestSetZeroTest() passed all tests");
		    }
		  }
	   
	   public void BlockSparseMatrixTestRightMultiplyTest() {
		   boolean passed = true;
			 SetUp();
		    double y_a[] = new double[A_.num_rows()];
		    double y_b[] = new double[A_.num_rows()];
		    for (int i = 0; i < A_.num_cols(); ++i) {
		      double x[] = new double[A_.num_cols()];
		      x[i] = 1.0;
		      A_.RightMultiply(x, y_a);
		      B_.RightMultiply(x, y_b);
		      double normSquared = 0.0;
		      for (int j = 0; j < A_.num_rows(); j++) {
		    	  double diff = y_a[j] - y_b[j];
		    	  normSquared += (diff * diff);
		      }
		      double norm = Math.sqrt(normSquared);
		      if (norm >= 1.0E-12) {
		    	  System.err.println("In BlockSparseMatrixTestRightMultiplyTest() (y_a - y_b).norm() = " + norm);
		    	  passed = false;
		      }
		    }
		    if (passed) {
		    	System.out.println("BlockSparseMatrixTestRightMultiplyTest() passed all tests");
		    }
	     }
	   
	   public void BlockSparseMatrixTestLeftMultiplyTest() {
		   boolean passed = true;
			 SetUp();
		    double y_a[] = new double[A_.num_cols()];
		    double y_b[] = new double[A_.num_cols()];
		    for (int i = 0; i < A_.num_rows(); ++i) {
		      double x[] = new double[A_.num_rows()];
		      x[i] = 1.0;
		      A_.LeftMultiply(x, y_a);
		      B_.LeftMultiply(x, y_b);
		      
		      double normSquared = 0.0;
		      for (int j = 0; j < A_.num_cols(); j++) {
		    	  double diff = y_a[j] - y_b[j];
		    	  normSquared += (diff * diff);
		      }
		      double norm = Math.sqrt(normSquared);
		      if (norm >= 1.0E-12) {
		    	  System.err.println("In BlockSparseMatrixTestLeftMultiplyTest() (y_a - y_b).norm() = " + norm);
		    	  passed = false;
		      }
		    }
		    if (passed) {
		    	System.out.println("BlockSparseMatrixTestLeftMultiplyTest() passed all tests");
		    }
		  }
	   
	   public void BlockSparseMatrixTestSquaredColumnNormTest() {
		   boolean passed = true;
			 SetUp();
		    double y_a[] = new double[A_.num_cols()];
		    double y_b[] = new double[A_.num_cols()];
		    A_.SquaredColumnNorm(y_a);
		    B_.SquaredColumnNorm(y_b);
		    double normSquared = 0.0;
		      for (int j = 0; j < A_.num_cols(); j++) {
		    	  double diff = y_a[j] - y_b[j];
		    	  normSquared += (diff * diff);
		      }
		      double norm = Math.sqrt(normSquared);
		      if (norm >= 1.0E-12) {
		    	  System.err.println("In BlockSparseMatrixTestSquaredColumnNormTest() (y_a - y_b).norm() = " + norm);
		    	  passed = false;
		      }
		   if (passed) {
		    	System.out.println("BlockSparseMatrixTestSquaredColumnNormTest() passed all tests");
		    }
		   
	   }
	   
	   public void BlockSparseMatrixTestToDenseMatrixTest() {
		   boolean passed = true;
			 SetUp();
		    Matrix m_a = A_.ToDenseMatrix();
		    Matrix m_b = B_.ToDenseMatrix();
		    double normSquared = 0.0;
		    for (int r = 0; r < m_a.getRowDimension(); r++) {
		    	for (int c = 0; c < m_a.getColumnDimension(); c++) {
		    		double diff = m_a.getArray()[r][c] - m_b.getArray()[r][c];
		    		normSquared += (diff * diff);
		    	}
		    }
		    double norm = Math.sqrt(normSquared);
		      if (norm >= 1.0E-12) {
		    	  System.err.println("In BlockSparseMatrixTestToDenseMatrixTest() (m_a - m_b).norm() = " + norm);
		    	  passed = false;
		      }
		   if (passed) {
		    	System.out.println("BlockSparseMatrixTestToDenseMatrixTest() passed all tests");
		    }
		  }
	   
	   public void BlockSparseMatrixTestAppendRows() {
		   boolean passed = true;
			 SetUp();
		    LinearLeastSquaresProblem problem = 
		        CreateLinearLeastSquaresProblemFromId(2);
		    BlockSparseMatrix m = 
		       (BlockSparseMatrix)(problem.A);
		    A_.AppendRows(m);
		    if (A_.num_rows() != 2 * m.num_rows()) {
		    	 System.err.println("In BlockSparseMatrixTestAppendRows() A_.num_rows() != 2 * m.num_rows()");
		    	  passed = false;	
		    }
		    if (A_.num_cols() != m.num_cols()) {
		    	System.err.println("In BlockSparseMatrixTestAppendRows() A_.num_cols() != m.num_cols()");
		    	  passed = false;		
		    }

		    problem = CreateLinearLeastSquaresProblemFromId(1);
		    TripletSparseMatrix m2 =
		        (TripletSparseMatrix)(problem.A);
		    B_.AppendRows(m2);

		    double y_a[] = new double[A_.num_rows()];
		    double y_b[] = new double[A_.num_rows()];
		    for (int i = 0; i < A_.num_cols(); ++i) {
		      double x[] = new double[A_.num_cols()];
		      x[i] = 1.0;
		      for (int j = 0; j < A_.num_rows(); j++) {
		    	  y_a[j] = 0.0;
		    	  y_b[j] = 0.0;
		      }

		      A_.RightMultiply(x, y_a);
		      B_.RightMultiply(x, y_b);
		      double normSquared = 0.0;
		      for (int j = 0; j < A_.num_rows(); j++) {
		    	  double diff = y_a[j] - y_b[j];
		    	  normSquared += (diff * diff);
		      }
		      double norm = Math.sqrt(normSquared);
		      if (norm >= 1.0E-12) {
		    	  System.err.println("In BlockSparseMatrixTestAppendRows() (y_a - y_b).norm() = " + norm);
		    	  passed = false;
		      }
		    }
		    if (passed) {
		    	System.out.println("BlockSparseMatrixTestAppendRows() passed all tests");
		    }
		  }
	   
	   public void BlockSparseMatrixTestAppendAndDeleteBlockDiagonalMatrix() {
		   int j, offset;
		   double normSquared;
		   double diff;
		   double norm;
		   boolean passed = true;
			 SetUp();
		    final Vector<Block> column_blocks = A_.block_structure().cols;
		    final int num_cols =
		        column_blocks.lastElement().size + column_blocks.lastElement().position;
		    double diagonal[] = new double[num_cols];
		    for (int i = 0; i < num_cols; ++i) {
		      diagonal[i] = 2 * i * i + 1;
		    }
		    BlockSparseMatrix appendage =
		        CreateDiagonalMatrix(diagonal, column_blocks);

		    A_.AppendRows(appendage);
		    double[] y_a, y_b;
		    y_a = new double[A_.num_rows()];
		    y_b = new double[A_.num_rows()];
		    for (int i = 0; i < A_.num_cols(); ++i) {
		      double x[] = new double[A_.num_cols()];
		      x[i] = 1.0;
		      for (j = 0; j < A_.num_rows(); j++) {
		          y_a[j] = 0;
		          y_b[j] = 0;
		      }

		      A_.RightMultiply(x, y_a);
		      B_.RightMultiply(x, y_b);
		      normSquared = 0.0;
		      for (j = 0; j < B_.num_rows(); j++) {
		    	  diff = y_a[j] - y_b[j];
		    	  normSquared += (diff * diff);
		      }
		      norm = Math.sqrt(normSquared);
		      if (norm >= 1.0E-12) {
		    	  System.err.println("In BlockSparseMatrixTestAppendAndDeleteBlockDiagonalMatrix()\n" +
		          "y_a.head(B_->num_rows()) - y_b.head(B_->num_rows())).norm() >= 1.0E-12");
		    	  passed = false;
		      }
		      double expected_tail[] = new double[A_.num_cols()];
		      expected_tail[i] = diagonal[i];
		      normSquared = 0.0;
		      double y_atail[] = new double[A_.num_cols()];
		      offset = A_.num_rows() - A_.num_cols();
		      for (j = offset; j < A_.num_rows(); j++) {
		    	  y_atail[j-offset] = y_a[j];
		      }
		      for (j = 0; j < A_.num_cols(); j++) {
		    	  diff = y_atail[j] - expected_tail[j];
		    	  normSquared += (diff * diff);
		      }
		      norm = Math.sqrt(normSquared);
		      if (norm >= 1.0E-12) {
		    	  System.err.println("In BlockSparseMatrixTestAppendAndDeleteBlockDiagonalMatrix()\n" +
		          "(y_a.tail(A_->num_cols()) - expected_tail).norm() >= 1e-12");
		    	  passed = false;
		      }
		    }


		    A_.DeleteRowBlocks(column_blocks.size());
		    if (A_.num_rows() != B_.num_rows()) {
		    	 System.err.println("In BlockSparseMatrixTestAppendAndDeleteBlockDiagonalMatrix()\n" +
		         "A_.num_rows() != B_.num_rows()");
		    	 passed = false;
		    }
		    if (A_.num_cols() != B_.num_cols()) {
		    	System.err.println("In BlockSparseMatrixTestAppendAndDeleteBlockDiagonalMatrix()\n" +
		        "A_.num_cols() != B_.num_cols()");
		    	passed = false;
		    }

		    y_a = new double[A_.num_rows()];
		    y_b = new double[A_.num_rows()];
		    for (int i = 0; i < A_.num_cols(); ++i) {
		      double x[] = new double[A_.num_cols()];
		      x[i] = 1.0;
		      for (j = 0; j < A_.num_rows(); j++) {
		          y_a[j] = 0;
		          y_b[j] = 0;
		      }

		      A_.RightMultiply(x, y_a);
		      B_.RightMultiply(x, y_b);
		      normSquared = 0.0;
		      for (j = 0; j < A_.num_rows(); j++) {
		    	  diff = y_a[j] - y_b[j];
		    	  normSquared += (diff * diff);
		      }
		      norm = Math.sqrt(normSquared);
		      if (norm >= 1.0E-12) {
		    	  System.err.println("In BlockSparseMatrixTestAppendAndDeleteBlockDiagonalMatrix()\n" +
		          "(y_a - y_b).norm() >= 1e-12");
		    	  passed = false;
		      }
		    }
		    if (passed) {
		    	System.out.println("BlockSparseMatrixTestAppendAndDeleteBlockDiagonalMatrix() passed all tests");
		    }
		  }
	   
	   public void BlockSparseMatrixCreateDiagonalMatrix() {
		   int j;
		   boolean passed = true;
			 SetUp();
		    Vector<Block> column_blocks = new Vector<Block>();
		    column_blocks.add(new Block(2, 0));
		    column_blocks.add(new Block(1, 2));
		    column_blocks.add(new Block(3, 3));
		    final int num_cols =
		        column_blocks.lastElement().size + column_blocks.lastElement().position;
		    double diagonal[] = new double[num_cols];
		    for (int i = 0; i < num_cols; ++i) {
		      diagonal[i] = 2 * i * i + 1;
		    }

		    BlockSparseMatrix m =
		        CreateDiagonalMatrix(diagonal, column_blocks);
		    final CompressedRowBlockStructure bs = m.block_structure();
		    if (bs.cols.size() != column_blocks.size()) {
		        System.err.println("In BlockSparseMatrixCreateDiagonalMatrix() bs.cols.size() != column_blocks.size()");
		        passed = false;
		    }
		    for (int i = 0; i < column_blocks.size(); ++i) {
		      if (bs.cols.get(i).size != column_blocks.get(i).size) {
		    	  System.err.println("In BlockSparseMatrixCreateDiagonalMatrix() bs.cols.get(i).size != column_blocks.get(i).size");
			        passed = false;  
		      }
		      if (bs.cols.get(i).position != column_blocks.get(i).position) {
		    	  System.err.println("In BlockSparseMatrixCreateDiagonalMatrix() bs.cols.get(i).position != column_blocks.get(i).position");
			        passed = false;  
		      }
		    }
		    if (m.num_rows() != m.num_cols()) {
		    	System.err.println("In BlockSparseMatrixCreateDiagonalMatrix() m.num_rows() != m.num_cols()");
		        passed = false;	
		    }
		    double x[] = new double[num_cols];
		    for (j = 0; j < num_cols; j++) {
		    	x[j] = 1.0;
		    }
		    double y[] = new double[num_cols];
		    m.RightMultiply(x, y);
		    for (int i = 0; i < num_cols; ++i) {
		      if (Math.abs(y[i] - diagonal[i]) > epsilon) {
		    	  System.err.println("In BlockSparseMatrixCreateDiagonalMatrix() Math.abs(y[i] - diagonal[i]) > epsilon");
		    	  passed = false;
		      }
		    }
		    if (passed) {
		    	System.out.println("BlockSparseMatrixCreateDiagonalMatrix() passed all tests");
		    }
		  }

	  };
	  
	  

	  public void TESTMatrixMatrixMultiply() {
		// TESTMatrixMatrixMultiply() passed all tests.
		boolean passed = true;
		double norm;
		int i, r, c, index;
		double normSquared = 0.0;
		double diff;
		final double kTolerance = 3.0 * epsilon;
	    final int kRowA = 3;
	    final int kColA = 5;
	    Matrix A = new Matrix(kRowA, kColA, 1.0);
	    double Adata[] = new double[kRowA * kColA];
	    for (i = 0; i < kRowA * kColA; i++) {
	    	Adata[i] = 1.0;
	    }

	    final int kRowB = 5;
	    final int kColB = 7;
	    Matrix B = new Matrix(kRowB, kColB, 1.0);
	    double Bdata[] = new double[kRowB * kColB];
	    for (i = 0; i < kRowB * kColB; i++) {
	    	Bdata[i] = 1.0;
	    }
	    Matrix AB = A.times(B);
	    double AB_array[][] = AB.getArray();

	    for (int row_stride_c = kRowA; row_stride_c < 3 * kRowA; ++row_stride_c) {
	      for (int col_stride_c = kColB; col_stride_c < 3 * kColB; ++col_stride_c) {
	        Matrix C = new Matrix(row_stride_c, col_stride_c, 1.0);

	        Matrix C_plus = new Matrix(row_stride_c, col_stride_c, 1.0);
	        double C_plus_data[] = new double[row_stride_c * col_stride_c];
	        for (i = 0; i < row_stride_c * col_stride_c; i++) {
	        	C_plus_data[i] = 1.0;
	        }
	        Matrix C_minus = new Matrix(row_stride_c, col_stride_c, 1.0);
	        double C_minus_data[] = new double[row_stride_c * col_stride_c];
	        for (i = 0; i < row_stride_c * col_stride_c; i++) {
	        	C_minus_data[i] = 1.0;
	        }
	        Matrix C_assign = new Matrix(row_stride_c, col_stride_c, 1.0);
	        double C_assign_data[] = new double[row_stride_c * col_stride_c];
	        for (i = 0; i < row_stride_c * col_stride_c; i++) {
	        	C_assign_data[i] = 1.0;
	        }
	        Matrix C_plus_ref = new Matrix(row_stride_c, col_stride_c, 1.0);
	        Matrix C_minus_ref = new Matrix(row_stride_c, col_stride_c, 1.0);
	        Matrix C_assign_ref = new Matrix(row_stride_c, col_stride_c, 1.0);
	        for (int start_row_c = 0; start_row_c + kRowA < row_stride_c; ++start_row_c) {
	          for (int start_col_c = 0; start_col_c + kColB < col_stride_c; ++start_col_c) {
	        	for (r = start_row_c; r < start_row_c + kRowA; r++) {
	        		for (c = start_col_c; c < start_col_c + kColB; c++) {
	        	    C_plus_ref.set(r, c, C_plus_ref.get(r, c) + AB_array[r-start_row_c][c-start_col_c]);
	        		}
	        	}
	        	

	            MatrixMatrixMultiply(kRowA, kColA, kRowB, kColB, 1,
	                Adata, 0, kRowA, kColA,
	                Bdata, 0, kRowB, kColB,
	                C_plus_data, 0, start_row_c, start_col_c, row_stride_c, col_stride_c);

	            normSquared = 0.0;
	            for (r = 0; r < row_stride_c; r++ ) {
	            	for (c = 0; c < col_stride_c; c++) {
	            		index = r*col_stride_c + c;
	            		diff = C_plus_ref.get(r,c) - C_plus_data[index];
	            		normSquared += (diff * diff);
	            	}
	            }
	            norm = Math.sqrt(normSquared);
	            if (norm > kTolerance) {
	                passed = false;
	                System.err.println("In TestMatrixMatrixMultiply C += A * B");
	                System.err.println("row_stride_c : " + row_stride_c);
	                System.err.println("col_stride_c : " + col_stride_c);
	                System.err.println("start_row_c  : " + start_row_c);
	                System.err.println("start_col_c  : " + start_col_c);
	            }
	            

	            for (r = start_row_c; r < start_row_c + kRowA; r++) {
	        		for (c = start_col_c; c < start_col_c + kColB; c++) {
	        	    C_minus_ref.set(r, c, C_minus_ref.get(r, c) - AB_array[r-start_row_c][c-start_col_c]);
	        		}
	        	}

	            MatrixMatrixMultiply(kRowA, kColA, kRowB, kColB, -1,
	                Adata, 0, kRowA, kColA,
	                Bdata, 0, kRowB, kColB,
	                C_minus_data, 0, start_row_c, start_col_c, row_stride_c, col_stride_c);
	            
	            normSquared = 0.0;
	            for (r = 0; r < row_stride_c; r++ ) {
	            	for (c = 0; c < col_stride_c; c++) {
	            		index = r*col_stride_c + c;
	            		diff = C_minus_ref.get(r,c) - C_minus_data[index];
	            		normSquared += (diff * diff);
	            	}
	            }
	            norm = Math.sqrt(normSquared);
	            if (norm > kTolerance) {
	                passed = false;
	                System.err.println("In TestMatrixMatrixMultiply C -= A * B");
	                System.err.println("row_stride_c : " + row_stride_c);
	                System.err.println("col_stride_c : " + col_stride_c);
	                System.err.println("start_row_c  : " + start_row_c);
	                System.err.println("start_col_c  : " + start_col_c);
	            }

	             
	            for (r = start_row_c; r < start_row_c + kRowA; r++) {
	        		for (c = start_col_c; c < start_col_c + kColB; c++) {
	        	    C_assign_ref.set(r, c, AB_array[r-start_row_c][c-start_col_c]);
	        		}
	        	}

	            MatrixMatrixMultiply(kRowA, kColA, kRowB, kColB, 0,
	                Adata, 0, kRowA, kColA,
	                Bdata, 0, kRowB, kColB,
	                C_assign_data, 0, start_row_c, start_col_c, row_stride_c, col_stride_c);
	            
	            normSquared = 0.0;
	            for (r = 0; r < row_stride_c; r++ ) {
	            	for (c = 0; c < col_stride_c; c++) {
	            		index = r*col_stride_c + c;
	            		diff = C_assign_ref.get(r,c) - C_assign_data[index];
	            		normSquared += (diff * diff);
	            	}
	            }
	            norm = Math.sqrt(normSquared);
	            if (norm > kTolerance) {
	                passed = false;
	                System.err.println("In TestMatrixMatrixMultiply C = A * B");
	                System.err.println("row_stride_c : " + row_stride_c);
	                System.err.println("col_stride_c : " + col_stride_c);
	                System.err.println("start_row_c  : " + start_row_c);
	                System.err.println("start_col_c  : " + start_col_c);
	            }


	            
	          }
	        }
	      }
	    }
	    if (passed) {
	         System.out.println("TESTMatrixMatrixMultiply() passed all tests.");	
	    }
	  }

	  public void TESTMatrixTransposeMatrixMultiply() {
		  // TESTMatrixTransposeMatrixMultiply() passed all tests.
		  boolean passed = true;
			double norm;
			int i, r, c, index;
			double normSquared = 0.0;
			double diff;
			final double kTolerance = 3.0 * epsilon;
		    final int kRowA = 5;
		    final int kColA = 3;
		    Matrix A = new Matrix(kRowA, kColA, 1.0);
		    double Adata[] = new double[kRowA * kColA];
		    for (i = 0; i < kRowA * kColA; i++) {
		    	Adata[i] = 1.0;
		    }

		    final int kRowB = 5;
		    final int kColB = 7;
		    Matrix B = new Matrix(kRowB, kColB, 1.0);
		    double Bdata[] = new double[kRowB * kColB];
		    for (i = 0; i < kRowB * kColB; i++) {
		    	Bdata[i] = 1.0;
		    }
		    Matrix AtB = (A.transpose()).times(B);
		    double AtB_array[][] = AtB.getArray();

	    for (int row_stride_c = kColA; row_stride_c < 3 * kColA; ++row_stride_c) {
	      for (int col_stride_c = kColB; col_stride_c <  3 * kColB; ++col_stride_c) {
	    	  Matrix C = new Matrix(row_stride_c, col_stride_c, 1.0);

		        Matrix C_plus = new Matrix(row_stride_c, col_stride_c, 1.0);
		        double C_plus_data[] = new double[row_stride_c * col_stride_c];
		        for (i = 0; i < row_stride_c * col_stride_c; i++) {
		        	C_plus_data[i] = 1.0;
		        }
		        Matrix C_minus = new Matrix(row_stride_c, col_stride_c, 1.0);
		        double C_minus_data[] = new double[row_stride_c * col_stride_c];
		        for (i = 0; i < row_stride_c * col_stride_c; i++) {
		        	C_minus_data[i] = 1.0;
		        }
		        Matrix C_assign = new Matrix(row_stride_c, col_stride_c, 1.0);
		        double C_assign_data[] = new double[row_stride_c * col_stride_c];
		        for (i = 0; i < row_stride_c * col_stride_c; i++) {
		        	C_assign_data[i] = 1.0;
		        }
		        Matrix C_plus_ref = new Matrix(row_stride_c, col_stride_c, 1.0);
		        Matrix C_minus_ref = new Matrix(row_stride_c, col_stride_c, 1.0);
		        Matrix C_assign_ref = new Matrix(row_stride_c, col_stride_c, 1.0);
	        for (int start_row_c = 0; start_row_c + kColA < row_stride_c; ++start_row_c) {
	          for (int start_col_c = 0; start_col_c + kColB < col_stride_c; ++start_col_c) {
	        	  for (r = start_row_c; r < start_row_c + kColA; r++) {
		        		for (c = start_col_c; c < start_col_c + kColB; c++) {
		        	    C_plus_ref.set(r, c, C_plus_ref.get(r, c) + AtB_array[r-start_row_c][c-start_col_c]);
		        		}
		        	}

	            MatrixTransposeMatrixMultiply(kRowA, kColA, kRowB, kColB, 1,
	                Adata, 0, kRowA, kColA,
	                Bdata, 0, kRowB, kColB,
	                C_plus_data, 0, start_row_c, start_col_c, row_stride_c, col_stride_c);
	            
	            normSquared = 0.0;
	            for (r = 0; r < row_stride_c; r++ ) {
	            	for (c = 0; c < col_stride_c; c++) {
	            		index = r*col_stride_c + c;
	            		diff = C_plus_ref.get(r,c) - C_plus_data[index];
	            		normSquared += (diff * diff);
	            	}
	            }
	            norm = Math.sqrt(normSquared);
	            if (norm > kTolerance) {
	                passed = false;
	                System.err.println("In TestMatrixTransposeMatrixMultiply C += A' * B");
	                System.err.println("row_stride_c : " + row_stride_c);
	                System.err.println("col_stride_c : " + col_stride_c);
	                System.err.println("start_row_c  : " + start_row_c);
	                System.err.println("start_col_c  : " + start_col_c);
	            }

	            for (r = start_row_c; r < start_row_c + kColA; r++) {
	        		for (c = start_col_c; c < start_col_c + kColB; c++) {
	        	    C_minus_ref.set(r, c, C_minus_ref.get(r, c) - AtB_array[r-start_row_c][c-start_col_c]);
	        		}
	        	}

	            MatrixTransposeMatrixMultiply(kRowA, kColA, kRowB, kColB, -1,
	                Adata, 0, kRowA, kColA,
	                Bdata, 0, kRowB, kColB,
	                C_minus_data, 0, start_row_c, start_col_c, row_stride_c, col_stride_c);
	            
	            normSquared = 0.0;
	            for (r = 0; r < row_stride_c; r++ ) {
	            	for (c = 0; c < col_stride_c; c++) {
	            		index = r*col_stride_c + c;
	            		diff = C_minus_ref.get(r,c) - C_minus_data[index];
	            		normSquared += (diff * diff);
	            	}
	            }
	            norm = Math.sqrt(normSquared);
	            if (norm > kTolerance) {
	                passed = false;
	                System.err.println("In TestMatrixTransposeMatrixMultiply C -= A' * B");
	                System.err.println("row_stride_c : " + row_stride_c);
	                System.err.println("col_stride_c : " + col_stride_c);
	                System.err.println("start_row_c  : " + start_row_c);
	                System.err.println("start_col_c  : " + start_col_c);
	            }

	            for (r = start_row_c; r < start_row_c + kColA; r++) {
	        		for (c = start_col_c; c < start_col_c + kColB; c++) {
	        	    C_assign_ref.set(r, c, AtB_array[r-start_row_c][c-start_col_c]);
	        		}
	        	}

	            MatrixTransposeMatrixMultiply(kRowA, kColA, kRowB, kColB, 0,
	                Adata, 0, kRowA, kColA,
	                Bdata, 0, kRowB, kColB,
	                C_assign_data, 0, start_row_c, start_col_c, row_stride_c, col_stride_c);
	            
	            normSquared = 0.0;
	            for (r = 0; r < row_stride_c; r++ ) {
	            	for (c = 0; c < col_stride_c; c++) {
	            		index = r*col_stride_c + c;
	            		diff = C_assign_ref.get(r,c) - C_assign_data[index];
	            		normSquared += (diff * diff);
	            	}
	            }
	            norm = Math.sqrt(normSquared);
	            if (norm > kTolerance) {
	                passed = false;
	                System.err.println("In TestMatrixTransposeMatrixMultiply C = A' * B");
	                System.err.println("row_stride_c : " + row_stride_c);
	                System.err.println("col_stride_c : " + col_stride_c);
	                System.err.println("start_row_c  : " + start_row_c);
	                System.err.println("start_col_c  : " + start_col_c);
	            }

	          }
	        }
	      }
	    }
	    if (passed) {
	         System.out.println("TESTMatrixTransposeMatrixMultiply() passed all tests.");	
	    }
	  }

	  public void TESTMatrixVectorMultiply() {
		// TESTMatrixVectorMultiply passed all tests
		final double kTolerance = 3.0 * epsilon;
		boolean passed = true;
		int i, r, col;
		double normSquared;
		double diff;
		double norm;
	    final int kRowA = 5;
	    final int kColA = 3;
	    Matrix A = new Matrix(kRowA, kColA, 1.0);
	    double Adata[] = new double[kRowA * kColA];
	    for (i = 0; i < kRowA * kColA; i++) {
	    	Adata[i] = 1.0;
	    }

	    Vector<Double> b = new Vector<Double>(kColA);
	    double bdata[] = new double[kColA];
	    for (i = 0; i < kColA; i++) {
	    	b.add(1.0);
	    	bdata[i] = 1.0;
	    }

	    Vector<Double> c = new Vector<Double>(kRowA);
	    for (i = 0; i < kRowA; i++) {
	    	c.add(1.0);
	    }

	    Vector<Double> c_plus = new Vector<Double>(kRowA);
	    double c_plus_data[] = new double[kRowA];
	    for (i = 0; i < kRowA; i++) {
	    	c_plus.add(1.0);
	    	c_plus_data[i] = 1.0;
	    }
	    Vector<Double> c_minus = new Vector<Double>(kRowA);
	    double c_minus_data[] = new double[kRowA];
	    for (i = 0; i < kRowA; i++) {
	    	c_minus.add(1.0);
	    	c_minus_data[i] = 1.0;
	    }
	    Vector<Double> c_assign = new Vector<Double>(kRowA);
	    double c_assign_data[] = new double[kRowA];
	    for (i = 0; i < kRowA; i++) {
	    	c_assign.add(1.0);
	    	c_assign_data[i] = 1.0;
	    }
	    
	    Vector<Double> c_plus_ref = new Vector<Double>(kRowA);
	    for (i = 0; i < kRowA; i++) {
	    	c_plus_ref.add(1.0);
	    }
	    Vector<Double> c_minus_ref = new Vector<Double>(kRowA);
	    for (i = 0; i < kRowA; i++) {
	    	c_minus_ref.add(1.0);
	    }
	    Vector<Double> c_assign_ref = new Vector<Double>(kRowA);
	    for (i = 0; i < kRowA; i++) {
	    	c_assign_ref.add(1.0);
	    }

        double Ab[] = new double[kRowA];
        for (r = 0; r < kRowA; r++) {
        	for (col = 0; col < kColA; col++) {
        		Ab[r] += A.get(r,col) * b.get(col);
        	}
        }
        
        for (r = 0; r < kRowA; r++) {
        	c_plus_ref.set(r, c_plus_ref.get(r) + Ab[r]);
        }
	  
	    MatrixVectorMultiply(kRowA, kColA, 1, Adata, 0, kRowA, kColA,
	                                          bdata, 0,
	                                          c_plus_data, 0);
	    normSquared = 0.0;
	    for (i = 0; i < kRowA; i++) {
	    	diff = c_plus_ref.get(i) - c_plus_data[i];
	    	normSquared += (diff * diff);
	    }
	    norm = Math.sqrt(normSquared);
	    if (norm > kTolerance) {
	    	passed = false;
	    	System.err.println("In TESTMatrixVectorMultiply c += A * b");
	    }

	    for (r = 0; r < kRowA; r++) {
        	c_minus_ref.set(r, c_minus_ref.get(r) - Ab[r]);
        }
	    MatrixVectorMultiply(kRowA, kColA, -1, Adata, 0, kRowA, kColA,
	                                                   bdata, 0,
	                                                   c_minus_data, 0);
	    normSquared = 0.0;
	    for (i = 0; i < kRowA; i++) {
	    	diff = c_minus_ref.get(i) - c_minus_data[i];
	    	normSquared += (diff * diff);
	    }
	    norm = Math.sqrt(normSquared);
	    if (norm > kTolerance) {
	    	passed = false;
	    	System.err.println("In TESTMatrixVectorMultiply c -= A * b");
	    }
	    
	    for (r = 0; r < kRowA; r++) {
        	c_assign_ref.set(r, Ab[r]);
        }
	  
	    MatrixVectorMultiply(kRowA, kColA, 0, Adata, 0, kRowA, kColA,
	                                                    bdata, 0,
	                                                    c_assign_data, 0);
	    normSquared = 0.0;
	    for (i = 0; i < kRowA; i++) {
	    	diff = c_assign_ref.get(i) - c_assign_data[i];
	    	normSquared += (diff * diff);
	    }
	    norm = Math.sqrt(normSquared);
	    if (norm > kTolerance) {
	    	passed = false;
	    	System.err.println("In TESTMatrixVectorMultiply c = A * b");
	    }
	    
	    if (passed) {
	    	System.out.println("TESTMatrixVectorMultiply passed all tests");
	    }
	  }

	  public void TESTMatrixTransposeVectorMultiply() {
		  // TESTMatrixTransposeVectorMultiply passed all tests.
		  final double kTolerance = 3.0 * epsilon;
			boolean passed = true;
			int i, r, col, index;
			double normSquared;
			double diff;
			double norm;
	    final int kRowA = 5;
	    final int kColA = 3;
	    RandomNumberGen ran = new RandomNumberGen();
	    Matrix A = new Matrix(kRowA, kColA);
	    double Adata[] = new double[kRowA * kColA];
	    for (r = 0; r < kRowA; r++) {
	    	for (col = 0; col < kColA; col++) {
	    		index = r * kColA + col;
	    		A.set(r, col, ran.genGaussianRandomNum(-1.0,1.0));
	    		Adata[index] = A.get(r, col);
	    	}
	    }

	    Vector<Double> b = new Vector<Double>(kRowA);
	    double bdata[] = new double[kRowA];
	    for (i = 0; i < kRowA; i++) {
	    	b.add(ran.genGaussianRandomNum(-1.0,1.0));
	    	bdata[i] = b.get(i);
	    }

	    Vector<Double> c = new Vector<Double>(kColA);
	    for (i = 0; i < kColA; i++) {
	    	c.add(1.0);
	    }
	    
	    Vector<Double> c_plus = new Vector<Double>(kColA);
	    double c_plus_data[] = new double[kColA];
	    for (i = 0; i < kColA; i++) {
	    	c_plus.add(1.0);
	    	c_plus_data[i] = 1.0;
	    }
	    Vector<Double> c_minus = new Vector<Double>(kColA);
	    double c_minus_data[] = new double[kColA];
	    for (i = 0; i < kColA; i++) {
	    	c_minus.add(1.0);
	    	c_minus_data[i] = 1.0;
	    }
	    Vector<Double> c_assign = new Vector<Double>(kColA);
	    double c_assign_data[] = new double[kColA];
	    for (i = 0; i < kColA; i++) {
	    	c_assign.add(1.0);
	    	c_assign_data[i] = 1.0;
	    }
	    
	    Vector<Double> c_plus_ref = new Vector<Double>(kColA);
	    for (i = 0; i < kColA; i++) {
	    	c_plus_ref.add(1.0);
	    }
	    Vector<Double> c_minus_ref = new Vector<Double>(kColA);
	    for (i = 0; i < kColA; i++) {
	    	c_minus_ref.add(1.0);
	    }
	    Vector<Double> c_assign_ref = new Vector<Double>(kColA);
	    for (i = 0; i < kColA; i++) {
	    	c_assign_ref.add(1.0);
	    }

	    Matrix At = A.transpose();
	    double Atb[] = new double[kColA];
        for (r = 0; r < kColA; r++) {
        	for (col = 0; col < kRowA; col++) {
        		Atb[r] += At.get(r,col) * b.get(col);
        	}
        }

        for (r = 0; r < kColA; r++) {
        	c_plus_ref.set(r, c_plus_ref.get(r) + Atb[r]);
        }
       
	    MatrixTransposeVectorMultiply(kRowA, kColA, 1, Adata, 0, kRowA, kColA,
	                                                   bdata, 0,
	                                                   c_plus_data, 0);
	    
	    normSquared = 0.0;
	    for (i = 0; i < kColA; i++) {
	    	diff = c_plus_ref.get(i) - c_plus_data[i];
	    	normSquared += (diff * diff);
	    }
	    norm = Math.sqrt(normSquared);
	    if (norm > kTolerance) {
	    	passed = false;
	    	System.err.println("In TESTMatrixTransposeVectorMultiply c += A' * b");
	    }

	    for (r = 0; r < kColA; r++) {
        	c_minus_ref.set(r, c_minus_ref.get(r) - Atb[r]);
        }
	   
	    MatrixTransposeVectorMultiply(kRowA, kColA, -1, Adata, 0, kRowA, kColA,
	                                                    bdata, 0,
	                                                    c_minus_data, 0);
	    
	    normSquared = 0.0;
	    for (i = 0; i < kColA; i++) {
	    	diff = c_minus_ref.get(i) - c_minus_data[i];
	    	normSquared += (diff * diff);
	    }
	    norm = Math.sqrt(normSquared);
	    if (norm > kTolerance) {
	    	passed = false;
	    	System.err.println("In TESTMatrixTransposeVectorMultiply c -= A' * b");
	    }
	    

	    for (r = 0; r < kColA; r++) {
        	c_assign_ref.set(r, Atb[r]);
        }
	    
	    MatrixTransposeVectorMultiply(kRowA, kColA, 0, Adata, 0, kRowA, kColA,
	                                                    bdata, 0,
	                                                    c_assign_data, 0);
	    
	    normSquared = 0.0;
	    for (i = 0; i < kColA; i++) {
	    	diff = c_assign_ref.get(i) - c_assign_data[i];
	    	normSquared += (diff * diff);
	    }
	    norm = Math.sqrt(normSquared);
	    if (norm > kTolerance) {
	    	passed = false;
	    	System.err.println("In TESTMatrixTransposeVectorMultiply c = A' * b");
	    }
	    
	    if (passed) {
	        System.out.println("TESTMatrixTransposeVectorMultiply passed all tests.");	
	    }
	  }
	  
	  // RandOrthMat ported from MATLAB code with no license
	  // Ofek Shilon (2021). RandOrthMat (https://www.mathworks.com/matlabcentral/fileexchange/11783-randorthmat),
	  // MATLAB Central File Exchange. Retrieved January 31, 2021. 
	  public Matrix RandOrthMat(int n, double tol) {
		      int i, j;
		      double normSquared;
		      double norm;
		      double nrm;
			  // M = RANDORTHMAT(n)
			  // generates a random n x n orthogonal real matrix.
			  
			  // M = RANDORTHMAT(n,tol)
			  // explicitly specifies a thresh value that measures linear dependence
			  // of a newly formed column with the existing columns. Defaults to 1e-6.
			  
			  // In this version the generated matrix distribution *is* uniform over the manifold
			  // O(n) w.r.t. the induced R^(n^2) Lebesgue measure, at a slight computational 
			  // overhead (randn + normalization, as opposed to rand ). 
			  
			  // (c) Ofek Shilon , 2006.


			      //if nargin==1
			  	  //tol=1e-6;
			      //end
			      
			      Matrix M = new Matrix(n,n,0.0); // prealloc
			      
			      // gram-schmidt on random column vectors
			      Random rand = new Random(System.currentTimeMillis());	
			      
			      double vi[] = new double[n];
			      for (i = 0; i < n; i++) {
			    	  vi[i] = rand.nextGaussian();
			      }
			      // the n-dimensional normal distribution has spherical symmetry, which implies
			      // that after normalization the drawn vectors would be uniformly distributed on the
			      // n-dimensional unit sphere.
			      normSquared = 0.0;
			      for (i = 0; i < n; i++) {
			    	  normSquared += (vi[i] * vi[i]);
			      }
			      norm = Math.sqrt(normSquared);
			      for (i = 0; i < n; i++) {
			    	  M.set(i, 0, vi[i]/norm);  
			      }
			      Matrix vMat = new Matrix(n,1);
                  
			      
			      for (i = 1; i < n; i++) {
			  	  nrm = 0;
			  	  while (nrm < tol) {
			  		for (j = 0; j < n; j++) {
				    	vi[j] = rand.nextGaussian();
				    }
			  		Matrix Mp = M.getMatrix(0, n-1, 0, i-1);
			  		Matrix Mpt = Mp.transpose();
			  		for (j = 0; j < n; j++) {
			  			vMat.set(j, 0, vi[j]);
			  		}
			  		Matrix MV = Mpt.times(vMat);
			  		Matrix MMV = Mp.times(MV);
			  		vMat = vMat.minus(MMV);
			  		normSquared = 0.0;
			  		for (j = 0; j < n; j++) {
			  			normSquared += (vMat.get(j,0) * vMat.get(j,0));
			  		}
			  		nrm = Math.sqrt(normSquared);
			  	  } // While (nrm < tol)
			  	  for (j = 0; j < n; j++) {
			  		  M.set(j, i, vMat.get(j, 0)/nrm);
			  	  }

			      } // for (i = 1; i < n; i++)
			      return M;
			          
	  }  // RandOrthMat
	  
	  public void TESTInvertPSDMatrixIdentity3x3() {
		  // TESTInvertPSDMatrixIdentity3x3 passed
		  int r,c;
		  double normSquared;
		  double diff;
		  double norm;
		  boolean kFullRank = true;
		  final Matrix m = Matrix.identity(3, 3);
		  double mnorm = Math.sqrt(3);
		  double marr[][] = m.getArray();
		  double inverse_marr[][] = InvertPSDMatrix(kFullRank, marr);
		  normSquared = 0.0;
		  for (r = 0; r < 3; r++) {
			  for (c = 0; c < 3; c++) {
				  diff = inverse_marr[r][c] - marr[r][c];
				  normSquared += (diff * diff);
			  }
		  }
		  norm = Math.sqrt(normSquared);
		  if (norm/mnorm > epsilon) {
			  System.err.println("TESTInvertPSDMatrixIdentity3x3 failed");
		  }
		  else {
			  System.out.println("TESTInvertPSDMatrixIdentity3x3 passed");
		  }
		}
	  
	  public void TESTInvertPSDMatrixFullRank5x5() {
		  // TESTInvertPSDMatrixFullRank5x5() passed
		  boolean kFullRank = true;
		  double normSquared;
		  double norm;
		  int r,c;
		  int i;
		  double eigenvalues[] = new double[5];
		  RandomNumberGen rand = new RandomNumberGen();
		  for (i = 0; i < 5; i++) {
			  eigenvalues[i] = rand.genGaussianRandomNum(1.0E-6, 4.0);
		  }
		  Matrix randMat = RandOrthMat(5,1.0E-6);
		  Matrix diagMat = new Matrix(5,5,0.0);
		  for (i = 0; i < 5; i++) {
			  diagMat.set(i, i, eigenvalues[i]);
		  }
		  Matrix fullRankMat = (randMat.times(diagMat)).times(randMat.transpose());
		  double marr[][] = fullRankMat.getArray();
		  double inverse_marr[][] = InvertPSDMatrix(kFullRank, marr);
		  Matrix inverseMat = new Matrix(inverse_marr);
		  Matrix prod = fullRankMat.times(inverseMat);
		  Matrix identityMat = Matrix.identity(5,5);
		  Matrix diffMat = prod.minus(identityMat);
		  normSquared = 0.0;
		  for (r = 0; r < 5; r++) {
			  for (c = 0; c < 5; c++) {
				  normSquared += (diffMat.get(r,c) * diffMat.get(r,c));
			  }
		  }
		  norm = Math.sqrt(normSquared);
		  if (norm/5.0 > epsilon) {
			  System.err.println("TESTInvertPSDMatrixFullRank5x5() failed");  
		  }
		  else {
			  System.out.println("TESTInvertPSDMatrixFullRank5x5() passed");
		  }
		}

	  public void TESTInvertPSDMatrixRankDeficient5x5() {
		  // TESTInvertPSDMatrixRankDeficient5x5() passed
		  boolean kRankDeficient = false;
		  double mnormSquared;
		  double mnorm;
		  double diffnormSquared;
		  double diffnorm;
		  int r,c;
		  int i;
		  double eigenvalues[] = new double[5];
		  RandomNumberGen rand = new RandomNumberGen();
		  // eigenvalues[3] = 0;
		  for (i = 0; i < 3; i++) {
			  eigenvalues[i] = rand.genGaussianRandomNum(1.0E-6, 4.0);
		  }
		  eigenvalues[4] = rand.genGaussianRandomNum(1.0E-6, 4.0);
		  Matrix randMat = RandOrthMat(5,1.0E-6);
		  Matrix diagMat = new Matrix(5,5,0.0);
		  for (i = 0; i < 5; i++) {
			  diagMat.set(i, i, eigenvalues[i]);
		  }
		  Matrix singularMat = (randMat.times(diagMat)).times(randMat.transpose());
		  double marr[][] = singularMat.getArray();
		  double inverse_marr[][] = InvertPSDMatrix(kRankDeficient, marr);
		  Matrix inverseMat = new Matrix(inverse_marr);
		  Matrix prod = (singularMat.times(inverseMat)).times(singularMat);
          Matrix diffMat = prod.minus(singularMat);
		  mnormSquared = 0.0;
		  diffnormSquared = 0.0;
		  for (r = 0; r < 5; r++) {
			  for (c = 0; c < 5; c++) {
				  mnormSquared += (singularMat.get(r,c) * singularMat.get(r,c));
				  diffnormSquared += (diffMat.get(r,c) * diffMat.get(r,c));
			  }
		  }
		  mnorm = Math.sqrt(mnormSquared); 
		  diffnorm = Math.sqrt(diffnormSquared);
		  if (diffnorm/mnorm > 10.0 * epsilon) {
			  System.err.println("TESTInvertPSDMatrixRankDeficient5x5() failed");
			  System.err.println("diffnorm = " + diffnorm);
			  System.err.println("mnorm = " + mnorm);
		  }
		  else {
			  System.out.println("TESTInvertPSDMatrixRankDeficient5x5() passed");
		  }
		  //Matrix pseudo_identity = Matrix::Identity(5, 5);
		  //pseudo_identity(3, 3) = 0.0;
		}

	  public void OrderedGroupsEmptyOrderedGroupBehavesCorrectly() {
		  // OrderedGroupsEmptyOrderedGroupBehavesCorrectly() passed all tests
		  boolean passed = true;
		  //ParameterBlockOrdering ordering;
		  OrderedGroups<double[]> ordering = new OrderedGroups<double[]>();
		  if (ordering.NumGroups() != 0) {
			  System.err.println("In OrderedGroupsEmptyOrderedGroupBehavesCorrectly() ordering.NumGroups() != 0");
			  passed = false;
		  }
		  if (ordering.NumElements() != 0) {
			  System.err.println("In OrderedGroupsEmptyOrderedGroupBehavesCorrectly() ordering.NumElements() != 0");
			  passed = false;  
		  }
		  if (ordering.GroupSize(1) != 0) {
			  System.err.println("In OrderedGroupsEmptyOrderedGroupBehavesCorrectly() ordering.GroupSize(1) != 0");
			  passed = false;    
		  }
		  double x[] = new double[1];
		  if (ordering.GroupId(x) != -1) {
			  System.err.println("In OrderedGroupsEmptyOrderedGroupBehavesCorrectly() ordering.GroupId(x) != -1");
			  passed = false;   
		  }
		  if (ordering.Remove(x)) {
			  System.err.println("In OrderedGroupsEmptyOrderedGroupBehavesCorrectly() ordering.Remove(x) != true");
			  passed = false;     
		  }
		  if (passed) {
			  System.out.println("OrderedGroupsEmptyOrderedGroupBehavesCorrectly() passed all tests");
		  }
		}
	  
	  public void TESTOrderedGroupsEverythingInOneGroup() {
		  // TESTOrderedGroupsEverythingInOneGroup() passed all tests
		  boolean passed = true;
		  //ParameterBlockOrdering ordering;
		  OrderedGroups<double[]> ordering = new OrderedGroups<double[]>();
		  double x[][] = new double[3][1];
		  ordering.AddElementToGroup(x[0], 1);
		  ordering.AddElementToGroup(x[1], 1);
		  ordering.AddElementToGroup(x[2], 1);
		  ordering.AddElementToGroup(x[0], 1);

		  if (ordering.NumGroups() != 1) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.NumGroups() != 1");
			  passed = false;
		  }
		  if (ordering.NumElements() != 3) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.NumElements() != 3");
			  passed = false;  
		  }
		  if (ordering.GroupSize(1) != 3) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.GroupSize(1) != 3");
			  passed = false;   
		  }
		  if (ordering.GroupSize(0) != 0) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.GroupSize(0) != 0");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[0]) != 1) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.GroupId(x[0]) != 1");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[1]) != 1) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.GroupId(x[1]) != 1");
			  passed = false;      
		  }
		  if (ordering.GroupId(x[2]) != 1) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.GroupId(x[2]) != 1");
			  passed = false;      
		  }

		  ordering.Remove(x[0]);
		  if (ordering.NumGroups() != 1) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.NumGroups() != 1");
			  passed = false;
		  }
		  if (ordering.NumElements() != 2) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.NumElements() != 2");
			  passed = false;  
		  }
		  if (ordering.GroupSize(1) != 2) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.GroupSize(1) != 2");
			  passed = false;   
		  }
		  if (ordering.GroupSize(0) != 0) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.GroupSize(0) != 0");
			  passed = false;    
		  }

		  if (ordering.GroupId(x[0]) != -1) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.GroupId(x[0]) != -1");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[1]) != 1) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.GroupId(x[1]) != 1");
			  passed = false;      
		  }
		  if (ordering.GroupId(x[2]) != 1) {
			  System.err.println("In TESTOrderedGroupsEverythingInOneGroup() ordering.GroupId(x[2]) != 1");
			  passed = false;      
		  }
		  if (passed) {
			  System.out.println("TESTOrderedGroupsEverythingInOneGroup() passed all tests");
		  }
		}

	  public void TESTOrderedGroupsStartInOneGroupAndThenSplit() {
		  // TESTOrderedGroupsStartInOneGroupAndThenSplit() passed all tests
		  boolean passed = true;
		  //ParameterBlockOrdering ordering;
		  OrderedGroups<double[]> ordering = new OrderedGroups<double[]>();
		  double x[][] = new double[3][1];
		  ordering.AddElementToGroup(x[0], 1);
		  ordering.AddElementToGroup(x[1], 1);
		  ordering.AddElementToGroup(x[2], 1);
		  ordering.AddElementToGroup(x[0], 1);

		  if (ordering.NumGroups() != 1) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.NumGroups() != 1");
			  passed = false;
		  }
		  if (ordering.NumElements() != 3) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.NumElements() != 3");
			  passed = false;  
		  }
		  if (ordering.GroupSize(1) != 3) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.GroupSize(1) != 3");
			  passed = false;   
		  }
		  if (ordering.GroupSize(0) != 0) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.GroupSize(0) != 0");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[0]) != 1) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.GroupId(x[0]) != 1");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[1]) != 1) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.GroupId(x[1]) != 1");
			  passed = false;      
		  }
		  if (ordering.GroupId(x[2]) != 1) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.GroupId(x[2]) != 1");
			  passed = false;      
		  }
		  

		  ordering.AddElementToGroup(x[0], 5);
		  if (ordering.NumGroups() != 2) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.NumGroups() != 2");
			  passed = false;
		  }
		  if (ordering.NumElements() != 3) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.NumElements() != 3");
			  passed = false;  
		  }
		  if (ordering.GroupSize(1) != 2) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.GroupSize(1) != 2");
			  passed = false;   
		  }
		  if (ordering.GroupSize(5) != 1) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.GroupSize(5) != 1");
			  passed = false;   
		  }
		  if (ordering.GroupSize(0) != 0) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.GroupSize(0) != 0");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[0]) != 5) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.GroupId(x[0]) != 5");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[1]) != 1) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.GroupId(x[1]) != 1");
			  passed = false;      
		  }
		  if (ordering.GroupId(x[2]) != 1) {
			  System.err.println("In TESTOrderedGroupsStartInOneGroupAndThenSplit() ordering.GroupId(x[2]) != 1");
			  passed = false;      
		  }
		  if (passed) {
			  System.out.println("TESTOrderedGroupsStartInOneGroupAndThenSplit() passed all tests");
		  }
		}
     
	  public void TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() {
		  // TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() passed all tests
		  boolean passed = true;
		  //ParameterBlockOrdering ordering;
		  OrderedGroups<double[]> ordering = new OrderedGroups<double[]>();
		  double x[][] = new double[3][1];
		  ordering.AddElementToGroup(x[0], 1);
		  ordering.AddElementToGroup(x[1], 1);
		  ordering.AddElementToGroup(x[2], 1);
		  ordering.AddElementToGroup(x[0], 1);

		  if (ordering.NumGroups() != 1) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.NumGroups() != 1");
			  passed = false;
		  }
		  if (ordering.NumElements() != 3) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.NumElements() != 3");
			  passed = false;  
		  }
		  if (ordering.GroupSize(1) != 3) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.GroupSize(1) != 3");
			  passed = false;   
		  }
		  if (ordering.GroupSize(0) != 0) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.GroupSize(0) != 0");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[0]) != 1) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.GroupId(x[0]) != 1");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[1]) != 1) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.GroupId(x[1]) != 1");
			  passed = false;      
		  }
		  if (ordering.GroupId(x[2]) != 1) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.GroupId(x[2]) != 1");
			  passed = false;      
		  }
		  

		  ordering.AddElementToGroup(x[0], 5);		  
		  ordering.AddElementToGroup(x[1], 5);
		  ordering.AddElementToGroup(x[2], 5);
		  if (ordering.NumGroups() != 1) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.NumGroups() != 1");
			  passed = false;
		  }
		  if (ordering.NumElements() != 3) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.NumElements() != 3");
			  passed = false;  
		  }
		  if (ordering.GroupSize(1) != 0) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.GroupSize(1) != 0");
			  passed = false;   
		  }
		  if (ordering.GroupSize(5) != 3) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.GroupSize(5) != 3");
			  passed = false;   
		  }
		  if (ordering.GroupSize(0) != 0) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.GroupSize(0) != 0");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[0]) != 5) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.GroupId(x[0]) != 5");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[1]) != 5) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.GroupId(x[1]) != 5");
			  passed = false;      
		  }
		  if (ordering.GroupId(x[2]) != 5) {
			  System.err.println("In TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() ordering.GroupId(x[2]) != 5");
			  passed = false;      
		  }

		  if (passed) {
			  System.out.println("TESTOrderedGroupsAddAndRemoveEveryThingFromOneGroup() passed all tests");
		  }
		} 
	  
	  public void TESTOrderedGroupsReverseOrdering() {
		  // TESTOrderedGroupsReverseOrdering() passed all tests
		  boolean passed = true;
		  //ParameterBlockOrdering ordering;
		  OrderedGroups<double[]> ordering = new OrderedGroups<double[]>();
		  double x[][] = new double[3][1];
		  ordering.AddElementToGroup(x[0], 1);
		  ordering.AddElementToGroup(x[1], 2);
		  ordering.AddElementToGroup(x[2], 2);
		  
		  if (ordering.NumGroups() != 2) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.NumGroups() != 2");
			  passed = false;
		  }
		  if (ordering.NumElements() != 3) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.NumElements() != 3");
			  passed = false;  
		  }
		  if (ordering.GroupSize(1) != 1) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.GroupSize(1) != 1");
			  passed = false;   
		  }
		  if (ordering.GroupSize(2) != 2) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.GroupSize(2) != 2");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[0]) != 1) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.GroupId(x[0]) != 1");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[1]) != 2) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.GroupId(x[1]) != 2");
			  passed = false;      
		  }
		  if (ordering.GroupId(x[2]) != 2) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.GroupId(x[2]) != 2");
			  passed = false;      
		  }

		  ordering.Reverse();

		  if (ordering.NumGroups() != 2) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.NumGroups() != 2");
			  passed = false;
		  }
		  if (ordering.NumElements() != 3) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.NumElements() != 3");
			  passed = false;  
		  }
		  if (ordering.GroupSize(3) != 1) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.GroupSize(3) != 1");
			  passed = false;   
		  }
		  if (ordering.GroupSize(2) != 2) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.GroupSize(2) != 2");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[0]) != 3) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.GroupId(x[0]) != 3");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[1]) != 2) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.GroupId(x[1]) != 2");
			  passed = false;      
		  }
		  if (ordering.GroupId(x[2]) != 2) {
			  System.err.println("In TESTOrderedGroupsReverseOrdering() ordering.GroupId(x[2]) != 2");
			  passed = false;      
		  }
		  
		  if (passed) {
			  System.out.println("TESTOrderedGroupsReverseOrdering() passed all tests");
		  }
		}
	  
	  public void TESTOrderedGroupsReverseOrderingWithEmptyOrderedGroups() {
		  // TESTOrderedGroupsReverseOrderingWithEmptyOrderedGroups() passed all tests
		  boolean passed = true;
		  //ParameterBlockOrdering ordering;
		  OrderedGroups<double[]> ordering = new OrderedGroups<double[]>();
		  // This should be a no-op.
		  ordering.Reverse();

		  // Ensure the properties of an empty OrderedGroups still hold after Reverse().
		  if (ordering.NumGroups() != 0) {
			  System.err.println("In TESTOrderedGroupsReverseOrderingWithEmptyOrderedGroups() ordering.NumGroups() != 0");
			  passed = false;
		  }
		  if (ordering.NumElements() != 0) {
			  System.err.println("In TESTOrderedGroupsReverseOrderingWithEmptyOrderedGroups() ordering.NumElements() != 0");
			  passed = false;  
		  }
		  if (ordering.GroupSize(1) != 0) {
			  System.err.println("In TESTOrderedGroupsReverseOrderingWithEmptyOrderedGroups() ordering.GroupSize(1) != 0");
			  passed = false;    
		  }
		  double x[] = new double[1];
		  if (ordering.GroupId(x) != -1) {
			  System.err.println("In TESTOrderedGroupsReverseOrderingWithEmptyOrderedGroups() ordering.GroupId(x) != -1");
			  passed = false;   
		  }
		  if (ordering.Remove(x)) {
			  System.err.println("In TESTOrderedGroupsReverseOrderingWithEmptyOrderedGroups() ordering.Remove(x) != true");
			  passed = false;     
		  }
		  if (passed) {
			  System.out.println("TESTOrderedGroupsReverseOrderingWithEmptyOrderedGroups() passed all tests");
		  }
		}

	  public void TESTOrderedGroupsBulkRemove() {
		  // TESTOrderedGroupsBulkRemove() passed all tests
		  boolean passed = true;
		  //ParameterBlockOrdering ordering;
		  OrderedGroups<double[]> ordering = new OrderedGroups<double[]>();
		  double x[][] = new double[3][1];
		  ordering.AddElementToGroup(x[0], 1);
		  ordering.AddElementToGroup(x[1], 2);
		  ordering.AddElementToGroup(x[2], 2);
		  
		  Vector<double[]> elements_to_remove = new Vector<double[]>();
		  elements_to_remove.add(x[0]);
		  elements_to_remove.add(x[2]);

		  if (ordering.Remove(elements_to_remove) != 2) {
			  System.err.println("In TESTOrderedGroupsBulkRemove() ordering.Remove(elements_to_remove) != 2");
			  passed = false;
		  }
		  if (ordering.NumElements() != 1) {
			  System.err.println("In TESTOrderedGroupsBulkRemove() ordering.NumElements() != 1");
			  passed = false;  
		  }
		  if (ordering.GroupId(x[0]) != -1) {
			  System.err.println("In TESTOrderedGroupsBulkRemove() ordering.GroupId(x[0]) != -1");
			  passed = false;   
		  }
		  if (ordering.GroupId(x[1]) != 2) {
			  System.err.println("In TESTOrderedGroupsBulkRemove() ordering.GroupId(x[1]) != 2");
			  passed = false;    
		  }
		  if (ordering.GroupId(x[2]) != -1) {
			  System.err.println("In TESTOrderedGroupsBulkRemove() ordering.GroupId(x[2]) != -1");
			  passed = false;   
		  }
		  if (passed) {
			  System.out.println("TESTOrderedGroupsBulkRemove() passed all tests");
		  }
		}

	  public void TESTOrderedGroupsBulkRemoveWithNoElements() {
		  // TESTOrderedGroupsBulkRemoveWithNoElements() passed all tests
		  boolean passed = true;
		  //ParameterBlockOrdering ordering;
		  OrderedGroups<double[]> ordering = new OrderedGroups<double[]>();

		  double x[][] = new double[3][1];
		  Vector<double[]> elements_to_remove = new Vector<double[]>();
		  elements_to_remove.add(x[0]);
		  elements_to_remove.add(x[2]);

		  if (ordering.Remove(elements_to_remove) != 0) {
			  System.err.println("In TESTOrderedGroupsBulkRemoveWithNoElements() ordering.Remove(elements_to_remove) != 0");
			  passed = false;
		  }

		  ordering.AddElementToGroup(x[0], 1);
		  ordering.AddElementToGroup(x[1], 2);
		  ordering.AddElementToGroup(x[2], 2);

		  elements_to_remove.clear();
		  if (ordering.Remove(elements_to_remove) != 0) {
			  System.err.println("In TESTOrderedGroupsBulkRemoveWithNoElements() ordering.Remove(elements_to_remove) != 0");
			  passed = false;
		  }
		  if (passed) {
			  System.out.println("TESTOrderedGroupsBulkRemoveWithNoElements() passed all tests");
		  }
		}

	  public void TESTOrderedGroupsMinNonZeroGroup() {
		  // TESTOrderedGroupsMinNonZeroGroup() passed all tests
		  boolean passed = true;
		  //ParameterBlockOrdering ordering;
		  OrderedGroups<double[]> ordering = new OrderedGroups<double[]>();
		  double x[][] = new double[3][1];
		  ordering.AddElementToGroup(x[0], 1);
		  ordering.AddElementToGroup(x[1], 1);
		  ordering.AddElementToGroup(x[2], 2);
		  

		  if (ordering.MinNonZeroGroup() != 1) {
			  System.err.println("In TESTOrderedGroupsMinNonZeroGroup() ordering.MinNonZeroGroup() != 1");
			  passed = false;
		  }
		  ordering.Remove(x[0]);

		  if (ordering.MinNonZeroGroup() != 1) {
			  System.err.println("In TESTOrderedGroupsMinNonZeroGroup() ordering.MinNonZeroGroup() != 1");
			  passed = false;
		  }
		  ordering.Remove(x[1]);

		  if (ordering.MinNonZeroGroup() != 2) {
			  System.err.println("In TESTOrderedGroupsMinNonZeroGroup() ordering.MinNonZeroGroup() != 2");
			  passed = false;
		  }
		  ordering.Remove(x[2]);

		  // No non-zero groups left.
		  if (ordering.MinNonZeroGroup() != -1) {
			  System.err.println("In TESTOrderedGroupsMinNonZeroGroup() ordering.MinNonZeroGroup() != -1");
			  passed = false;  
		  }
		  
		  if (passed) {
			  System.out.println("TESTOrderedGroupsMinNonZeroGroup() passed all tests");
		  }
		}

	  
	  public void SchurEliminatorTestScalarProblemNoRegularization() {
		  // SchurEliminatorTestScalarProblemNoRegularization() use_static_structure = true passed all tests
		  // SchurEliminatorTestScalarProblemNoRegularization() use_static_structure = false passed all tests
		  SchurEliminatorTest SE = new SchurEliminatorTest();
		  SE.SchurEliminatorTestScalarProblemNoRegularization();
	  }
	  
	  public void SchurEliminatorTestScalarProblemWithRegularization() {
		  // SchurEliminatorTestScalarProblemWithRegularization() use_static_structure = true passed all tests
		  // SchurEliminatorTestScalarProblemWithRegularization() use_static_structure = false passed all tests
		  SchurEliminatorTest SE = new SchurEliminatorTest();
		  SE.SchurEliminatorTestScalarProblemWithRegularization();	 
	  }
	  
	  public void SchurEliminatorTestVaryingFBlockSizeWithStaticStructure() {
		  // SchurEliminatorTestVaryingFBlockSizeWithStaticStructure() use_static_structure = true passed all tests
		  SchurEliminatorTest SE = new SchurEliminatorTest();
		  SE.SchurEliminatorTestVaryingFBlockSizeWithStaticStructure();  
	  }
	  
	  public void SchurEliminatorTestVaryingFBlockSizeWithoutStaticStructure() {
		  // SchurEliminatorTestVaryingFBlockSizeWithoutStaticStructure() use_static_structure = false passed all tests
		  SchurEliminatorTest SE = new SchurEliminatorTest();
		  SE.SchurEliminatorTestVaryingFBlockSizeWithoutStaticStructure();  
	  }
	  
	  class SchurEliminatorTest {
		  private BlockSparseMatrix A;
		  private double[] b;
		  private double[] D;
		  private int num_eliminate_blocks;
		  private int num_eliminate_cols;

		  private Matrix lhs_expected;
		  private Vector<Double> rhs_expected;
		  private Vector<Double> sol_expected;
		  private String testName;
		  
		  public SchurEliminatorTest() {
			  
		  }
		  
		  public void SchurEliminatorTestScalarProblemNoRegularization() {
			   SetUpFromId(2);
			   double zero[] = new double[A.num_cols()];
			   testName = "SchurEliminatorTestScalarProblemNoRegularization()";
			   ComputeReferenceSolution(zero);
			   EliminateSolveAndCompare(zero, true, 1e-14);
			   EliminateSolveAndCompare(zero, false, 1e-14);
			 }
		  
		  public void SchurEliminatorTestScalarProblemWithRegularization() {
			   int i;
			   SetUpFromId(2);
			   testName = "SchurEliminatorTestScalarProblemWithRegularization()";
			   double Dref[] = new double[A.num_cols()];
			   for (i = 0; i < A.num_cols(); i++) {
				   Dref[i] = D[i];
			   }
			   ComputeReferenceSolution(Dref);
			   EliminateSolveAndCompare(Dref, true, 1e-14);
			   EliminateSolveAndCompare(Dref, false, 1e-14);
			 }
		  
		  public void SchurEliminatorTestVaryingFBlockSizeWithStaticStructure() {
			   int i;
			   SetUpFromId(4);
			   testName = "SchurEliminatorTestVaryingFBlockSizeWithStaticStructure()";
			   double Dref[] = new double[A.num_cols()];
			   for (i = 0; i < A.num_cols(); i++) {
				   Dref[i] = D[i];
			   }
			   ComputeReferenceSolution(Dref);
			   EliminateSolveAndCompare(Dref, true, 1e-14);
			 }
		  
		  public void SchurEliminatorTestVaryingFBlockSizeWithoutStaticStructure() {
			   int i;
			   SetUpFromId(4);
			   testName = "SchurEliminatorTestVaryingFBlockSizeWithoutStaticStructure()";
			   double Dref[] = new double[A.num_cols()];
			   for (i = 0; i < A.num_cols(); i++) {
				   Dref[i] = D[i];
			   }
			   ComputeReferenceSolution(Dref);
			   EliminateSolveAndCompare(Dref, false, 1e-14);
		  }
		  
		  public void SetUpFromId(int id) {
		     LinearLeastSquaresProblem
		         problem = CreateLinearLeastSquaresProblemFromId(id);
		     if (problem == null) {
		    	 System.err.println("In SchurEliminatorTest SetUpFromId("+id+") problem == null");
		    	 return;
		     }
		     SetupHelper(problem);
		   }

		   public void SetupHelper(LinearLeastSquaresProblem problem) {
		     A = (BlockSparseMatrix)(problem.A);
		     b = problem.b;
		     D = problem.D;

		     num_eliminate_blocks = problem.num_eliminate_blocks;
		     num_eliminate_cols = 0;
		     final CompressedRowBlockStructure bs = A.block_structure();

		     for (int i = 0; i < num_eliminate_blocks; ++i) {
		       num_eliminate_cols += bs.cols.get(i).size;
		     }
		   }

		   // Compute the golden values for the reduced linear system and the
		   // solution to the linear least squares problem using dense linear
		   // algebra.
		   public void ComputeReferenceSolution(double[] D) {
			 int i,r,c,offset;
		     Matrix J = A.ToDenseMatrix();
		     
		     //VectorRef f(b.get(), J.rows());
		     double f[] = new double[J.getRowDimension()];
		     for (i = 0; i < J.getRowDimension(); i++) {
		    	 f[i] = b[i];
		     }

		     Matrix H = new Matrix(D.length, D.length);
		     for (r = 0; r < D.length; r++) {
		    	 H.set(r, r, D[r]*D[r]);
		     }
		     Matrix Jt = J.transpose();
		     Matrix JtJ = (Jt).times(J);
		     H = H.plus(JtJ);

		     double g[] = new double[Jt.getRowDimension()];
		     for (r = 0; r < Jt.getRowDimension(); r++) {
		    	 for (c = 0; c < Jt.getColumnDimension(); c++) {
		    		 g[r] += (Jt.getArray()[r][c] * f[c]);
		    	 }
		     }
		     final int schur_size = J.getColumnDimension() - num_eliminate_cols;

		     //lhs_expected = new Matrix(schur_size, schur_size);

		     rhs_expected = new Vector<Double>(schur_size);
		     for (i = 0; i < schur_size; i++) {
		    	 rhs_expected.add(0.0);
		     }

		     sol_expected = new Vector<Double>(J.getColumnDimension());
		     for (i = 0; i < J.getColumnDimension(); i++) {
		    	 sol_expected.add(0.0);
		     }

		     Matrix P = H.getMatrix(0, num_eliminate_cols-1, 0, num_eliminate_cols-1);
		     Matrix Q = H.getMatrix(0,
		                        num_eliminate_cols-1,
		                        num_eliminate_cols,
		                        num_eliminate_cols + schur_size - 1);
		     Matrix R = H.getMatrix(num_eliminate_cols,
		    		            num_eliminate_cols + schur_size - 1,
		                        num_eliminate_cols,
		                        num_eliminate_cols + schur_size - 1);
		     int row = 0;
		     final CompressedRowBlockStructure bs = A.block_structure();
		     for (i = 0; i < num_eliminate_blocks; ++i) {
		       final int block_size =  bs.cols.get(i).size;
		       P.setMatrix(row, row + block_size - 1, row, row + block_size -1,
		           (P.getMatrix(row, row + block_size - 1,  row, row + block_size - 1).inverse()));
		       row += block_size;
		     }

		     /*lhs_expected
		         .triangularView<Eigen::Upper>() = R - Q.transpose() * P * Q;
		     rhs_expected =
		         g.tail(schur_size) - Q.transpose() * P * g.head(num_eliminate_cols);
		     sol_expected = H.llt().solve(g);*/
		     Matrix Qt = Q.transpose();
		     Matrix QtP = Qt.times(P);
		     lhs_expected = R.minus(QtP.times(Q));
		     double QtPg[] = new double[schur_size];
		     for (r = 0; r < schur_size; r++) {
		    	 for (c = 0; c < num_eliminate_cols; c++) {
		    		 QtPg[r] += QtP.getArray()[r][c]*g[c];
		    	 }
		     }
		     offset = g.length - schur_size;
		     for (i = 0; i < schur_size; i++) {
		         rhs_expected.set(i,(g[offset + i] - QtPg[i]));	 
		     }
		     double garr[][] = new double[g.length][1];
		     for (i = 0; i < g.length; i++) {
		         garr[i][0] = g[i];	 
		     }
		     Matrix gMat = new Matrix(garr);
		     Matrix sol_expected_mat = H.solve(gMat);
		     for (i = 0; i < sol_expected.size(); i++) {
		    	 sol_expected.set(i,sol_expected_mat.getArray()[i][0]);
		     }
		   }

		   public void EliminateSolveAndCompare(double[] diagonal,
		                                 boolean use_static_structure,
		                                 double relative_tolerance) {
			 int index, row, col, i, offset;
			 double normSquared;
			 double delta_norm;
			 double lhs_expected_norm;
			 double rhs_expected_norm;
			 double sol_expected_norm;
			 double norm;
			 double diff;
			 boolean passed = true;
		     final CompressedRowBlockStructure bs = A.block_structure();
		     final int num_col_blocks = bs.cols.size();
		     Vector<Integer> blocks = new Vector<Integer>(num_col_blocks - num_eliminate_blocks);
		     for (i = num_eliminate_blocks; i < num_col_blocks; ++i) {
		       blocks.add(bs.cols.get(i).size);
		     }

		     BlockRandomAccessDenseMatrix lhs = new BlockRandomAccessDenseMatrix(blocks);

		     final int num_cols = A.num_cols();
		     final int schur_size = lhs.num_rows();

		     double rhs[] = new double[schur_size];

		     LinearSolverOptions options = new LinearSolverOptions();
		     Context context = new Context();
		     options.context = context;
		     options.elimination_groups.add(num_eliminate_blocks);
		     if (use_static_structure) {
		       DetectStructure(bs,
		                       num_eliminate_blocks,
		                       options.row_block_size,
		                       options.e_block_size,
		                       options.f_block_size);
		     }

		     SchurEliminatorBase eliminator = createSchurEliminatorBase(options);
		     final boolean kFullRankETE = true;
		     eliminator.Init(num_eliminate_blocks, kFullRankETE, A.block_structure());
		     eliminator.Eliminate(A, b, diagonal, lhs, rhs);

		     double lhs_ref_array[][] = new double[lhs.num_rows()][lhs.num_cols()];
		     for (index = 0, row = 0; row < lhs.num_rows(); row++) {
		    	 for (col = 0; col < lhs.num_cols(); col++, index++) {
		    		 lhs_ref_array[row][col] = lhs.mutable_values()[index];
		    	 }
		     }
		     for (row = 0; row < lhs.num_rows(); row++) {
		    	 for (col = 0; col < row; col++) {
		    		 lhs_ref_array[row][col] = lhs_ref_array[col][row];
		    	 }
		     }
		     Matrix lhs_ref = new Matrix(lhs_ref_array);
		     Matrix rhsMat = new Matrix(schur_size, 1);
		     for (i = 0; i < schur_size; i++) {
		    	 rhsMat.set(i, 0, rhs[i]);
		     }
		     Matrix reduced_sol_mat = lhs_ref.solve(rhsMat);
		     double reduced_sol[] = new double[reduced_sol_mat.getRowDimension()];
		     for (i = 0; i < reduced_sol.length; i++) {
		    	 reduced_sol[i] = reduced_sol_mat.getArray()[i][0];
		     }

		     // Solution to the linear least squares problem.
		     double sol[] = new double[num_cols];
		     offset = sol.length - schur_size;
		     for (i = 0; i < schur_size; i++) {
		    	 sol[i + offset] = reduced_sol[i];
		     }
		     
		     eliminator.BackSubstitute(A,
		                                b,
		                                diagonal,
		                                reduced_sol,
		                                sol);

		     Matrix delta = lhs_ref.minus(lhs_expected);
		     normSquared = 0.0;
		     for (row = 0; row < delta.getRowDimension(); row++) {
		    	 for (col = 0; col < delta.getColumnDimension(); col++) {
		    		 normSquared += (delta.getArray()[row][col] * delta.getArray()[row][col]);
		    	 }
		     }
		     delta_norm = Math.sqrt(normSquared);
		     normSquared = 0.0;
		     for (row = 0; row < lhs_expected.getRowDimension(); row++) {
		    	 for (col = 0; col < lhs_expected.getColumnDimension(); col++) {
		    		 normSquared += (lhs_expected.getArray()[row][col] * lhs_expected.getArray()[row][col]);
		    	 }
		     }
		     lhs_expected_norm = Math.sqrt(normSquared);
		     if (delta_norm/lhs_expected_norm > relative_tolerance) {
		    	 System.err.println("In " + testName + " use_static_structure = " + use_static_structure + " delta_norm/lhs_expected_norm > relative_tolerance");
		    	 System.err.println("delta_norm = " + delta_norm);
		    	 System.err.println("lhs_expected_norm = " + lhs_expected_norm);
		    	 for (row = 0; row < lhs_expected.getRowDimension(); row++) {
		    		 for (col = 0; col < lhs_expected.getColumnDimension(); col++) {
		    			 System.err.println("row = " + row + " col = " + col + " expected = " + lhs_expected.getArray()[row][col] +
		    					 " ref = " + lhs_ref.getArray()[row][col]);
		    		 }
		    	 }
		    	 passed = false;
		     }
		     normSquared = 0.0;
		     for (i = 0; i < rhs_expected.size(); i++) {
		    	 normSquared += (rhs_expected.get(i) * rhs_expected.get(i));
		     }
		     rhs_expected_norm = Math.sqrt(normSquared);
		     normSquared = 0.0;
		     for (i = 0; i < rhs_expected.size(); i++) {
		    	 diff = rhs[i] - rhs_expected.get(i);
		    	 normSquared += (diff * diff);
		     }
		     norm = Math.sqrt(normSquared);
		     if (norm/rhs_expected_norm > relative_tolerance) {
		    	 System.err.println("In " + testName + " use_static_structure = " + use_static_structure + 
		    			 " ((rhs - rhs_expected).norm()/rhs_expected_norm > relative_tolerance");
		    	 System.err.println("norm = " + norm);
		    	 System.err.println("rhs_expected_norm = " + rhs_expected_norm);
		    	 passed = false;	 
		     }
		     
		     normSquared = 0.0;
		     for (i = 0; i < sol_expected.size(); i++) {
		    	 normSquared += (sol_expected.get(i) * sol_expected.get(i));
		     }
		     sol_expected_norm = Math.sqrt(normSquared);
		     normSquared = 0.0;
		     for (i = 0; i < sol_expected.size(); i++) {
		    	 diff = sol[i] - sol_expected.get(i);
		    	 normSquared += (diff * diff);
		     }
		     norm = Math.sqrt(normSquared);
		     if (norm/sol_expected_norm > relative_tolerance) {
		    	 System.err.println("In " + testName + " use_static_structure = " + use_static_structure + 
		    			 " ((sol - sol_expected).norm()/sol_expected_norm > relative_tolerance");
		    	 System.err.println("norm = " + norm);
		    	 System.err.println("sol_expected_norm = " + sol_expected_norm);
		    	 passed = false;	 
		     }
		     if (passed) {
		    	 System.out.println(testName + " use_static_structure = " + use_static_structure + " passed all tests");
		     }
		   }

		 };

	  
   public void ImplicitSchurComplementTestSchurMatrixValuesTest() {
	   ImplicitSchurComplementTest ISC = new ImplicitSchurComplementTest();
	   ISC.ImplicitSchurComplementTestSchurMatrixValuesTest();
   }
   

   class ImplicitSchurComplementTest {
	   private final double kEpsilon = 1e-14;
	   private int num_rows_;
	   private int num_cols_;
	   private int num_eliminate_blocks_;

	   private BlockSparseMatrix A_;
	   private double[] b_;
	   private double[] D_;
	   
	   public ImplicitSchurComplementTest() {
		   
	   }
	   
	   // Verify that the Schur Complement matrix implied by the
	   // ImplicitSchurComplement class matches the one explicitly computed
	   // by the SchurComplement solver.
	   //
	   // We do this with and without regularization to check that the
	   // support for the LM diagonal is correct.
	   public void ImplicitSchurComplementTestSchurMatrixValuesTest() {
		 // ImplicitSchurComplementTest passed all tests
		 boolean passed = true;
		 SetUp();
	     if(!TestImplicitSchurComplement(null)) {
	    	 passed = false;
	     }
	     if (!TestImplicitSchurComplement(D_)) {
	    	 passed = false;
	     }
	     if (passed) {
	    	 System.out.println("ImplicitSchurComplementTest passed all tests");
	     }
	   }
	   
    public void SetUp() {
       LinearLeastSquaresProblem problem = 
           CreateLinearLeastSquaresProblemFromId(2);

       if (problem == null) {
    	   System.err.println("In ImplicitSchurComplementTest in Setup() problem == null");
    	   return;
       }
       A_ = (BlockSparseMatrix)(problem.A);
       b_ = problem.b;
       D_ = problem.D;

       num_cols_ = A_.num_cols();
       num_rows_ = A_.num_rows();
       num_eliminate_blocks_ = problem.num_eliminate_blocks;
     }

     public Matrix ReducedLinearSystemAndSolution(double[] D,
                                         Vector<Double> rhs,
                                         Vector<Double> solution) {
       int i, index, row, col;
       final CompressedRowBlockStructure bs = A_.block_structure();
       final int num_col_blocks = bs.cols.size();
       Vector<Integer> blocks= new Vector<Integer>(num_col_blocks - num_eliminate_blocks_);
       for (i = num_eliminate_blocks_; i < num_col_blocks; ++i) {
         blocks.add(bs.cols.get(i).size);
       }

       BlockRandomAccessDenseMatrix blhs = new BlockRandomAccessDenseMatrix(blocks);
       final int num_schur_rows = blhs.num_rows();

       LinearSolverOptions options = new LinearSolverOptions();
       options.elimination_groups.add(num_eliminate_blocks_);
       options.type = LinearSolverType.DENSE_SCHUR;
       Context context = new Context();
       options.context = context;

       SchurEliminatorBase eliminator = 
           createSchurEliminatorBase(options);
       if (eliminator == null) {
    	   System.err.println("In ImplicitSchurComplementTest ReducedLinearSystemAndSolution eliminator == null");
    	   return null;
       }
       final boolean kFullRankETE = true;
       eliminator.Init(num_eliminate_blocks_, kFullRankETE, bs);

       double rhs_array[] = new double[num_schur_rows];

       eliminator.Eliminate(A_, b_, D, blhs, rhs_array);
       Matrix rhsMat = new Matrix(num_schur_rows,1);
       for (i = 0; i < num_schur_rows; i++) {
    	   rhsMat.set(i, 0, rhs_array[i]);
    	   rhs.add(rhs_array[i]);
       }
       
       double lhs_ref_array[][] = new double[num_schur_rows][num_schur_rows];
       for (index = 0, row = 0; row < num_schur_rows; row++) {
    	   for (col = 0; col < num_schur_rows; col++, index++) {
    		    lhs_ref_array[row][col] = blhs.mutable_values()[index];
    	   }
       }


       // lhs_ref is an upper triangular matrix. Construct a full version
       // of lhs_ref in lhs by transposing lhs_ref, choosing the strictly
       // lower triangular part of the matrix and adding it to lhs_ref.
       for (row = 0; row < num_schur_rows; row++) {
    	   for (col = 0; col < row; col++) {
    		   lhs_ref_array[row][col] = lhs_ref_array[col][row];
    	   }
       }
       Matrix lhs = new Matrix(lhs_ref_array);

       //solution->resize(num_cols_);
       //solution->setZero();
       //VectorRef schur_solution(solution->data() + num_cols_ - num_schur_rows,
                                //num_schur_rows);
       double solution_array[] = new double[num_cols_];
       double schur_solution_array[] = new double[num_schur_rows];
       //schur_solution = lhs->selfadjointView<Eigen::Upper>().llt().solve(*rhs);
       Matrix schur_solution_mat = lhs.solve(rhsMat);
       for (i = 0; i < num_schur_rows; i++) {
    	   schur_solution_array[i] = schur_solution_mat.getArray()[i][0]; 
    	   solution_array[i + num_cols_ - num_schur_rows] = schur_solution_array[i];
       }
       eliminator.BackSubstitute(A_, b_, D,
                                  schur_solution_array, solution_array);
       for (i = 0; i < num_cols_; i++) {
    	   solution.add(solution_array[i]);
       }
       return lhs;
     }

     public boolean TestImplicitSchurComplement(double[] D) {
       int j;
       double normSquared;
       double norm;
       double diff;
       Matrix lhs;
       Vector<Double> rhs = new Vector<Double>();
       Vector<Double> reference_solution = new Vector<Double>();
       lhs = ReducedLinearSystemAndSolution(D, rhs, reference_solution);

       LinearSolverOptions options = new LinearSolverOptions();
       options.elimination_groups.add(num_eliminate_blocks_);
       options.preconditioner_type = PreconditionerType.JACOBI;
       Context context = new Context();
       options.context =context;
       ImplicitSchurComplement isc = new ImplicitSchurComplement(options);
       isc.Init(A_, D, b_);

       int num_sc_cols = lhs.getColumnDimension();

       for (int i = 0; i < num_sc_cols; ++i) {
         double x[] = new double[num_sc_cols];
         x[i] = 1.0;

         double y[] = new double[num_sc_cols];
         for (j = 0; j < num_sc_cols; j++) {
        	 y[j] = lhs.getArray()[j][i];
         }

         double z[] = new double[num_sc_cols];
         isc.RightMultiply(x, z);

         // The i^th column of the implicit schur complement is the same as
         // the explicit schur complement.
         normSquared = 0.0;
         for (j = 0; j < num_sc_cols; j++) {
        	 diff = y[j] - z[j];
        	 normSquared += (diff * diff);
         }
         norm = Math.sqrt(normSquared);
         if (norm > kEpsilon) {
        	 System.err.println("Explicit and Implicit SchurComplements differ in column " + i + ". explicit: ");
        	 for (j = 0; j < num_sc_cols; j++) {
        		 System.err.println("y["+j+"] = " + y[j]);
        	 }
             System.err.println("Implicit: ");
             for (j = 0; j < num_sc_cols; j++) {
            	 System.err.println("z["+j+"] = " + z[j]);
             }
           return false;
         }
       }

       // Compare the rhs of the reduced linear system
       normSquared = 0.0;
       Matrix rhsMat = new Matrix(rhs.size(),1);
       for (j = 0; j < rhs.size(); j++) {
    	   diff = isc.rhs().get(j) - rhs.get(j);
    	   normSquared += (diff * diff);
    	   rhsMat.set(j, 0, rhs.get(j));
       }
       norm = Math.sqrt(normSquared);
       if (norm > kEpsilon) {
               System.err.println("Explicit and Implicit SchurComplements differ in rhs. explicit: ");
               for (j = 0; j < rhs.size(); j++) {
            	   System.err.println("rhs.get("+j+") = " + rhs.get(j));
               }
               System.err.println("Implicit: " );
               for (j = 0; j < rhs.size(); j++) {
            	   System.err.println("isc.rhs().get("+j+") = " + isc.rhs().get(j));
               }
               return false;
       }

       // Reference solution to the f_block.
       Matrix reference_f_sol =
           lhs.solve(rhsMat);
       double reference_f_sol_array[] = new double[rhs.size()];
       for (j = 0; j < rhs.size(); j++) {
    	   reference_f_sol_array[j] = reference_f_sol.getArray()[j][0];
       }

       // Backsubstituted solution from the implicit schur solver using the
       // reference solution to the f_block.
       double sol[] = new double[num_cols_];
       isc.BackSubstitute(reference_f_sol_array, sol);
       normSquared = 0.0;
       for (j = 0; j < num_cols_; j++) {
    	   diff = sol[j] - reference_solution.get(j);
    	   normSquared += (diff * diff);
       }
       norm = Math.sqrt(normSquared);
       if (norm > kEpsilon) {
             System.err.println("Explicit and Implicit SchurComplements solutions differ. explicit: ");
             for (j = 0; j < num_cols_; j++) {
            	 System.err.println("reference_solution.get("+j+") = " + reference_solution.get(j));
             }
             System.err.println("Implicit: ");
             for (j = 0; j < num_cols_; j++) {
            	 System.err.println("sol["+j+"] = " + sol[j]);
             }
             return false;
       }

       return true;
     }

     
   } // class ImplicitSchurTest

   
   

// Linear solver that takes as input a vector and checks that the
// caller passes the same vector as LinearSolver::PerSolveOptions.D.
class RegularizationCheckingLinearSolver extends TypedLinearSolver<DenseSparseMatrix> {
	boolean passedReg[];
	private final double kTolerance = 1e-16;
	private final int num_cols_;
	private final double[] diagonal_;
	String testName;
 public RegularizationCheckingLinearSolver(int num_cols, double[] diagonal, String testName,boolean passedReg[]) {
	    super();
        num_cols_ = num_cols;
        diagonal_ = diagonal;
        this.testName = testName;
        this.passedReg = passedReg;
  }


 public LinearSolverSummary SolveImpl(
      DenseSparseMatrix A,
      double[] b,
      LinearSolverPerSolveOptions per_solve_options,
      double[] x) {
	  double diff;
      if (per_solve_options.D == null) {
    	  System.err.println("In RegularizationCheckingLinearSolver SolveImpl per_solve_options.D == null");
    	  return null;
      };
    for (int i = 0; i < num_cols_; ++i) {
      diff = Math.abs(per_solve_options.D[i] - diagonal_[i]);
      if (diff > kTolerance) {
    	  System.err.println("Math.abs(per_solve_options.D["+i+"] - diagonal_["+i+"]) Exceeds kTolerance in " + testName);
    	  System.err.println("per_solve_options.D["+i+"] = " + per_solve_options.D[i]);
    	  System.err.println("diagonal_["+i+"] = " + diagonal_[i]);
    	  passedReg[0] = false;
      }
    }
    return new LinearSolverSummary();
  }
 
 
  
};

    public void TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling() {
      // TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling() passed all tests
      boolean passed[] = new boolean[] {true};
	  TrustRegionStrategyOptions options = new TrustRegionStrategyOptions();
	  options.initial_radius = 2.0;
	  options.max_radius = 20.0;
	  options.min_lm_diagonal = 1e-8;
	  options.max_lm_diagonal = 1e8;

	  // We need a non-null pointer here, so anything should do.
	  LinearSolver linear_solver = 
	      new RegularizationCheckingLinearSolver(0, null, "TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling()",passed);
	  linear_solver.options_.type = LinearSolverType.DENSE_QR;
	  options.linear_solver = linear_solver;

	  LevenbergMarquardtStrategy lms = new LevenbergMarquardtStrategy(options);
	  if (lms.Radius() != options.initial_radius) {
		  System.err.println("In TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling() lms.Radius() != options.initial_radius");
		  passed[0] = false;
	  }
	  lms.StepRejected(0.0);
	  if (lms.Radius() != 1.0) {
		  System.err.println("In TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling() lms.Radius() != 1.0");
		  passed[0] = false; 
	  }
	  lms.StepRejected(-1.0);
	  if (lms.Radius() != 0.25) {
		  System.err.println("In TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling() lms.Radius() != 0.25");
		  passed[0] = false;
	  }
	  lms.StepAccepted(1.0);
	  if (lms.Radius() != 0.25 * 3.0) {
		  System.err.println("In TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling() lms.Radius() != 0.25 * 3.0");
		  passed[0] = false;  
	  }
	  lms.StepAccepted(1.0);
	  if (lms.Radius() != 0.25 * 3.0 * 3.0) {
		  System.err.println("In TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling() lms.Radius() != 0.25 * 3.0 * 3.0");
		  passed[0] = false;  
	  }
	  lms.StepAccepted(0.25);
	  if (lms.Radius() != 0.25 * 3.0 * 3.0 / 1.125) {
		  System.err.println("In TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling() lms.Radius() != 0.25 * 3.0 * 3.0 / 1.125");
		  passed[0] = false; 
	  }
	  lms.StepAccepted(1.0);
	  if (lms.Radius() != 0.25 * 3.0 * 3.0 / 1.125 * 3.0) {
		  System.err.println("In TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling() lms.Radius() != 0.25 * 3.0 * 3.0 / 1.125 * 3.0");
		  passed[0] = false; 
	  }
	  lms.StepAccepted(1.0);
	  if (lms.Radius() != 0.25 * 3.0 * 3.0 / 1.125 * 3.0 * 3.0) {
		  System.err.println("In TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling() lms.Radius() != 0.25 * 3.0 * 3.0 / 1.125 * 3.0 * 3.0");
		  passed[0] = false;   
	  }
	  lms.StepAccepted(1.0);
	  if (lms.Radius() != options.max_radius) {
		  System.err.println("In TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling() lms.Radius() != options.max_radius");
		  passed[0] = false; 
	  }
	  if (passed[0]) {
		  System.out.println("TESTLevenbergMarquardtStrategyAcceptRejectStepRadiusScaling() passed all tests");
	  }
	}




	public void TESTLevenbergMarquardtStrategyCorrectDiagonalToLinearSolver() {
	  // TESTLevenbergMarquardtStrategyCorrectDiagonalToLinearSolver() passed all tests
	  boolean passed[] = new boolean[] {true};
	  Matrix jacobian = new Matrix(2, 3, 0.0);
	  jacobian.set(0, 0, 0.0);
	  jacobian.set(0, 1, 1.0);
	  jacobian.set(1, 1, 1.0);
	  jacobian.set(0, 2, 100.0);
	
	  double residual[] = new double[] {1.0};
	  double x[] = new double[3];
	  DenseSparseMatrix dsm = new DenseSparseMatrix(jacobian);
	
	  TrustRegionStrategyOptions options = new TrustRegionStrategyOptions();
	  options.initial_radius = 2.0;
	  options.max_radius = 20.0;
	  options.min_lm_diagonal = 1e-2;
	  options.max_lm_diagonal = 1e2;
	
	  double diagonal[] = new double[3];
	  diagonal[0] = options.min_lm_diagonal;
	  diagonal[1] = 2.0;
	  diagonal[2] = options.max_lm_diagonal;
	  for (int i = 0; i < 3; ++i) {
	    diagonal[i] = Math.sqrt(diagonal[i] / options.initial_radius);
	  }
	
	  RegularizationCheckingLinearSolver linear_solver = new RegularizationCheckingLinearSolver(3, diagonal, 
			  "TESTLevenbergMarquardtStrategyCorrectDiagonalToLinearSolver()", passed);
	  options.linear_solver = linear_solver;
	
	  LevenbergMarquardtStrategy lms = new LevenbergMarquardtStrategy(options);
	  TrustRegionStrategyPerSolveOptions pso = new TrustRegionStrategyPerSolveOptions();
	
	  /*{
	    ScopedMockLog log;
	    EXPECT_CALL(log, Log(_, _, _)).Times(AnyNumber());
	    // This using directive is needed get around the fact that there
	    // are versions of glog which are not in the google namespace.
	    using namespace google;
	
	#if defined(_MSC_VER)
	    // Use GLOG_WARNING to support MSVC if GLOG_NO_ABBREVIATED_SEVERITIES
	    // is defined.
	    EXPECT_CALL(log, Log(GLOG_WARNING, _,
	                         HasSubstr("Failed to compute a step")));
	#else
	    EXPECT_CALL(log, Log(google::WARNING, _,
	                         HasSubstr("Failed to compute a step")));
	#endif*/
	
	    TrustRegionStrategySummary summary =
	        lms.ComputeStep(pso, dsm, residual, x);
	    // Saw Preferences message
	    // Linear solver failure. Failed to compute a step: null
	    if (summary.termination_type != LinearSolverTerminationType.LINEAR_SOLVER_FAILURE) {
	    	System.err.println("In TESTLevenbergMarquardtStrategyCorrectDiagonalToLinearSolver() summary.termination_type != LinearSolverTerminationType.LINEAR_SOLVER_FAILURE");
	    	passed[0] = false;
	    }
	    if (passed[0]) {
	    	System.out.println("TESTLevenbergMarquardtStrategyCorrectDiagonalToLinearSolver() passed all tests");
	    }
	  //}
	}

	class Fixture {
		 protected DenseSparseMatrix jacobian_;
		 protected double[] residual_;
		 protected double[] x_;
		 protected TrustRegionStrategyOptions options_;
		 
		 public Fixture() {
		     options_ = new TrustRegionStrategyOptions();	 
		 }
		};

		// A test problem where
		//
		//   J^T J = Q diag([1 2 4 8 16 32]) Q^T
		//
		// where Q is a randomly chosen orthonormal basis of R^6.
		// The residual is chosen so that the minimum of the quadratic function is
		// at (1, 1, 1, 1, 1, 1). It is therefore at a distance of sqrt(6) ~ 2.45
		// from the origin.
		class DoglegStrategyFixtureEllipse extends Fixture {
			public DoglegStrategyFixtureEllipse() {
				super();
				SetUp();
			}
			
		 protected void SetUp() {
			 int i;
		    double basis_array[][] = new double[6][];
		    // The following lines exceed 80 characters for better readability.
		    basis_array[0] = new double[] {-0.1046920933796121, -0.7449367449921986, -0.4190744502875876,
		    		-0.4480450716142566,  0.2375351607929440, -0.0363053418882862};
		    basis_array[1] = new double[] {0.4064975684355914,  0.2681113508511354, -0.7463625494601520,
		                                  -0.0803264850508117, -0.4463149623021321,  0.0130224954867195};
		    basis_array[2] = new double[] {-0.5514387729089798,  0.1026621026168657, -0.5008316122125011,
		    		0.5738122212666414,  0.2974664724007106,  0.1296020877535158};
		    basis_array[3] = new double[] {0.5037835370947156,  0.2668479925183712, -0.1051754618492798,
		    		-0.0272739396578799,  0.7947481647088278, -0.1776623363955670};
		    basis_array[4] = new double[] {-0.4005458426625444,  0.2939330589634109, -0.0682629380550051,
		                                  -0.2895448882503687, -0.0457239396341685, -0.8139899477847840};
		    basis_array[5] = new double[] {-0.3247764582762654,  0.4528151365941945, -0.0276683863102816,
		    		-0.6155994592510784,  0.1489240599972848,  0.5362574892189350};
            Matrix basis = new Matrix(basis_array);
		    Matrix sqrtD = new Matrix(6, 6, 0.0);
		    sqrtD.set(0, 0, 1.0);
		    sqrtD.set(1, 1, Math.sqrt(2.0));
		    sqrtD.set(2, 2, 2.0);
		    sqrtD.set(3, 3, Math.sqrt(8.0));
		    sqrtD.set(4, 4, 4.0);
		    sqrtD.set(5, 5, Math.sqrt(32.0));
		   
		    Matrix jacobian = sqrtD.times(basis);
		    jacobian_ = new DenseSparseMatrix(jacobian);

		    Matrix minimum = new Matrix(6, 1, 1.0);
		    double residual_array[][] = jacobian.times(minimum).getArray();
		    residual_ = new double[6];
		    for (i = 0; i < 6; i++) {
		    	residual_[i] = -residual_array[i][0];
		    }

		    x_ = new double[6];

		    options_.min_lm_diagonal = 1.0;
		    options_.max_lm_diagonal = 1.0;
		  }
		};
		
		// A test problem where
		//
		//   J^T J = diag([1 2 4 8 16 32]) .
		//
		// The residual is chosen so that the minimum of the quadratic function is
		// at (0, 0, 1, 0, 0, 0). It is therefore at a distance of 1 from the origin.
		// The gradient at the origin points towards the global minimum.
		class DoglegStrategyFixtureValley extends Fixture {
			public DoglegStrategyFixtureValley() {
				super();
				SetUp();
			}
		 protected void SetUp() {
            int i;
		    Matrix jacobian = new Matrix(6, 6, 0.0);
		    jacobian.set(0, 0, 1.0);
		    jacobian.set(1, 1, 2.0);
		    jacobian.set(2, 2, 4.0);
		    jacobian.set(3, 3, 8.0);
		    jacobian.set(4, 4, 16.0);
		    jacobian.set(5, 5, 32.0);
		    jacobian_ = new DenseSparseMatrix(jacobian);

		    Matrix minimum = new Matrix(6, 1, 0.0);
		    minimum.set(2, 0, 1.0);
		    double residual_array[][] = jacobian.times(minimum).getArray();
		    residual_ = new double[6];
		    for (i = 0; i < 6; i++) {
		    	residual_[i] = -residual_array[i][0];
		    }

		    x_ = new double[6];

		    options_.min_lm_diagonal = 1.0;
		    options_.max_lm_diagonal = 1.0;
		  }
		};
		
		public void DoglegStrategyFixtureEllipseTrustRegionObeyedTraditional() {
			// DoglegStrategyFixtureEllipseTrustRegionObeyedTraditional passed all tests
			new DoglegStrategyFixtureEllipseTrustRegionObeyedTraditional();
		}
		
		public void DoglegStrategyFixtureEllipseTrustRegionObeyedSubspace() {
			// DoglegStrategyFixtureEllipseTrustRegionObeyedSubspace passed all tests
			new DoglegStrategyFixtureEllipseTrustRegionObeyedSubspace();
		}
		
		public void DoglegStrategyFixtureEllipseCorrectGaussNewtonStep() {
			// DoglegStrategyFixtureEllipseCorrectGaussNewtonStep passed all tests
			new DoglegStrategyFixtureEllipseCorrectGaussNewtonStep();
		}
		
		public void DoglegStrategyFixtureEllipseValidSubspaceBasis() {
			// DoglegStrategyFixtureEllipseValidSubspaceBasis passed all tests
		    new DoglegStrategyFixtureEllipseValidSubspaceBasis();	
		}
		
		public void DoglegStrategyFixtureValleyCorrectStepLocalOptimumAlongGradient() {
			// DoglegStrategyFixtureValleyCorrectStepLocalOptimumAlongGradient passed all tests
			new DoglegStrategyFixtureValleyCorrectStepLocalOptimumAlongGradient();
		}
		
		public void DoglegStrategyFixtureValleyCorrectStepGlobalOptimumAlongGradient() {
			// DoglegStrategyFixtureValleyCorrectStepGlobalOptimumAlongGradient passed all tests
			new DoglegStrategyFixtureValleyCorrectStepGlobalOptimumAlongGradient();
		}
		
		// The DoglegStrategy must never return a step that is longer than the current
		// trust region radius.
	    class DoglegStrategyFixtureEllipseTrustRegionObeyedTraditional extends DoglegStrategyFixtureEllipse {
	      public DoglegStrategyFixtureEllipseTrustRegionObeyedTraditional() {
	    	  super();
	      int i;
	      double normSquared;
	      double x_norm;
		  boolean passed = true;
		  LinearSolver linear_solver =
		      new DenseQRSolver(new LinearSolverOptions());
		  options_.linear_solver = linear_solver;
		  // The global minimum is at (1, 1, ..., 1), so the distance to it is
		  // sqrt(6.0).  By restricting the trust region to a radius of 2.0,
		  // we test if the trust region is actually obeyed.
		  options_.dogleg_type = DoglegType.TRADITIONAL_DOGLEG;
		  options_.initial_radius = 2.0;
		  options_.max_radius = 2.0;

		  DoglegStrategy strategy = new DoglegStrategy(options_);
		  TrustRegionStrategyPerSolveOptions pso = new TrustRegionStrategyPerSolveOptions();

		  TrustRegionStrategySummary summary = strategy.ComputeStep(pso,
		                                                              jacobian_,
		                                                              residual_,
		                                                              x_);

		  if (summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_FAILURE) {
			  System.err.println("In DoglegStrategyFixtureEllipseTrustRegionObeyedTraditional summary.termination_type == LINEAR_SOLVER_FAILURE");
			  passed = false;
		  }
		  normSquared = 0.0;
		  for (i = 0; i < x_.length; i++) {
			  normSquared += x_[i]*x_[i];
		  }
		  x_norm = Math.sqrt(normSquared);
		  if (x_norm > options_.initial_radius * (1.0 + 4.0 * epsilon)) {
			  System.err.println("In DoglegStrategyFixtureEllipseTrustRegionObeyedTraditional (x_norm > options_.initial_radius * (1.0 + 4.0 * epsilon)");
			  passed = false;
		  }
		  if (passed) {
			  System.out.println("DoglegStrategyFixtureEllipseTrustRegionObeyedTraditional passed all tests");
		  }
		}
	    }
	    
	 	    class DoglegStrategyFixtureEllipseTrustRegionObeyedSubspace extends DoglegStrategyFixtureEllipse {
	 	      public DoglegStrategyFixtureEllipseTrustRegionObeyedSubspace() {
	 	    	  super();
	 	      int i;
	 	      double normSquared;
	 	      double x_norm;
	 		  boolean passed = true;
	 		  LinearSolver linear_solver =
	 		      new DenseQRSolver(new LinearSolverOptions());
	 		  options_.linear_solver = linear_solver;
	 		  options_.dogleg_type = DoglegType.SUBSPACE_DOGLEG;
	 		  options_.initial_radius = 2.0;
	 		  options_.max_radius = 2.0;

	 		  DoglegStrategy strategy = new DoglegStrategy(options_);
	 		  TrustRegionStrategyPerSolveOptions pso = new TrustRegionStrategyPerSolveOptions();

	 		  TrustRegionStrategySummary summary = strategy.ComputeStep(pso,
	 		                                                              jacobian_,
	 		                                                              residual_,
	 		                                                              x_);

	 		  if (summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_FAILURE) {
	 			  System.err.println("In DoglegStrategyFixtureEllipseTrustRegionObeyedSubspace summary.termination_type == LINEAR_SOLVER_FAILURE");
	 			  passed = false;
	 		  }
	 		  normSquared = 0.0;
	 		  for (i = 0; i < x_.length; i++) {
	 			  normSquared += x_[i]*x_[i];
	 		  }
	 		  x_norm = Math.sqrt(normSquared);
	 		  if (x_norm > options_.initial_radius * (1.0 + 4.0 * epsilon)) {
	 			  System.err.println("In DoglegStrategyFixtureEllipseTrustRegionObeyedSubspace (x_norm > options_.initial_radius * (1.0 + 4.0 * epsilon)");
	 			  passed = false;
	 		  }
	 		  if (passed) {
	 			  System.out.println("DoglegStrategyFixtureEllipseTrustRegionObeyedSubspace passed all tests");
	 		  }
	 		}
	 	    }
	 	    
	 	    class DoglegStrategyFixtureEllipseCorrectGaussNewtonStep extends DoglegStrategyFixtureEllipse {
	 	      final double kToleranceLoose = 1e-5;
	 	      public DoglegStrategyFixtureEllipseCorrectGaussNewtonStep() {
	 	    	  super();
	 	      int i;
	 		  boolean passed = true;
	 		  LinearSolver linear_solver =
	 		      new DenseQRSolver(new LinearSolverOptions());
	 		  options_.linear_solver = linear_solver;
	 		  options_.dogleg_type = DoglegType.SUBSPACE_DOGLEG;
	 		  options_.initial_radius = 10.0;
	 		  options_.max_radius = 10.0;

	 		  DoglegStrategy strategy = new DoglegStrategy(options_);
	 		  TrustRegionStrategyPerSolveOptions pso = new TrustRegionStrategyPerSolveOptions();

	 		  TrustRegionStrategySummary summary = strategy.ComputeStep(pso,
	 		                                                              jacobian_,
	 		                                                              residual_,
	 		                                                              x_);

	 		  if (summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_FAILURE) {
	 			  System.err.println("In DoglegStrategyFixtureEllipseCorrectGaussNewtonStep summary.termination_type == LINEAR_SOLVER_FAILURE");
	 			  passed = false;
	 		  }
	 		  for (i = 0; i < 6; i++) {
	 			  if (Math.abs(x_[i] - 1.0) > kToleranceLoose) {
	 				  System.err.println("In  DoglegStrategyFixtureEllipseCorrectGaussNewtonStep Math.abs(x_["+i+"] - 1.0) > kToleranceLoose");
	 				  passed = false;
	 			  }
	 		  }
	 		  if (passed) {
	 			  System.out.println("DoglegStrategyFixtureEllipseCorrectGaussNewtonStep passed all tests");
	 		  }
	 		}
	 	    }
	 	    
	 	// Test if the subspace basis is a valid orthonormal basis of the space spanned
	 	// by the gradient and the Gauss-Newton point.
 	    class DoglegStrategyFixtureEllipseValidSubspaceBasis extends DoglegStrategyFixtureEllipse {
 	    	  final double kTolerance = 1e-14;
	 	      public DoglegStrategyFixtureEllipseValidSubspaceBasis() {
	 	    	  super();
	 	      int i;
	 	      double normSquared0;
	 	      double normSquared1;
	 	      double dotProduct;
	 	      double col0;
	 	      double col1;
	 	      double norm0;
	 	      double norm1;
	 	      double normSquared;
	 	      double norm;
	 		  boolean passed = true;
	 		  LinearSolver linear_solver =
	 		      new DenseQRSolver(new LinearSolverOptions());
	 		  options_.linear_solver = linear_solver;
	 		  options_.dogleg_type = DoglegType.SUBSPACE_DOGLEG;
	 		  options_.initial_radius = 2.0;
	 		  options_.max_radius = 2.0;

	 		  DoglegStrategy strategy = new DoglegStrategy(options_);
	 		  TrustRegionStrategyPerSolveOptions pso = new TrustRegionStrategyPerSolveOptions();

	 		  TrustRegionStrategySummary summary = strategy.ComputeStep(pso,
	 		                                                              jacobian_,
	 		                                                              residual_,
	 		                                                              x_);
	 		  
	 		  // Check if the basis is orthonormal.
	 		  final Matrix basis = strategy.subspace_basis();
	 		  normSquared0 = 0.0;
	 		  normSquared1 = 0.0;
	 		  dotProduct = 0.0;
	 		  for (i = 0; i < basis.getRowDimension(); i++) {
	 			  col0 = basis.get(i, 0);
	 			  normSquared0 += col0 * col0;
	 			  col1 = basis.get(i, 1);
	 			  normSquared1 += col1 * col1;
	 			  dotProduct += col0 * col1;
	 		  }
	 		  norm0 = Math.sqrt(normSquared0);
	 		  norm1 = Math.sqrt(normSquared1);
	 		  if (Math.abs(norm0 - 1.0) > kTolerance) {
	 			  System.err.println("In DoglegStrategyFixtureEllipseValidSubspaceBasis Math.abs(basis.col(0).norm() - 1.0) > kTolerance");
	 			  passed = false;
	 		  }
	 		  if (Math.abs(norm1 - 1.0) > kTolerance) {
	 			  System.err.println("In DoglegStrategyFixtureEllipseValidSubspaceBasis Math.abs(basis.col(1).norm() - 1.0) > kTolerance");
	 			  passed = false;
	 		  }
	 		  if (Math.abs(dotProduct) > kTolerance) {
	 			  System.err.println("In DoglegStrategyFixtureEllipseValidSubspaceBasis Math.abs(basis.col(0).dot(basis.col(1))) > kTolerance");
	 			  passed = false;  
	 		  }

	 		  // Check if the gradient projects onto itself.
	 		  final Vector<Double> gradient = strategy.gradient();
	 		  Matrix gradientMat = new Matrix(gradient.size(), 1);
	 		  for (i = 0; i < gradient.size(); i++ ) {
	 			  gradientMat.set(i, 0, gradient.get(i));
	 		  }
	 		  Matrix gMat = gradientMat.minus(basis.times((basis.transpose()).times(gradientMat)));
	 		  normSquared = 0.0;
	 		  for (i = 0; i < gradient.size(); i++) {
	 			  normSquared += gMat.get(i,0) * gMat.get(i,0);
	 		  }
	 		  norm = Math.sqrt(normSquared);
	 		  if (norm > kTolerance) {
	 			  System.err.println("In DoglegStrategyFixtureEllipseValidSubspaceBasis (gradient - basis*(basis.transpose()*gradient)).norm() > kTolerance");
	 			  passed = false;  
	 		  }
	 		  
	 		  // Check if the Gauss-Newton point projects onto itself.
	 		  final Vector<Double> gn = strategy.gauss_newton_step();
	 		  Matrix gnMat = new Matrix(gn.size(), 1);
	 		  for (i = 0; i < gn.size(); i++ ) {
	 			  gnMat.set(i, 0, gn.get(i));
	 		  }
	 		  gMat = gnMat.minus(basis.times((basis.transpose()).times(gnMat)));
	 		  normSquared = 0.0;
	 		  for (i = 0; i < gn.size(); i++) {
	 			  normSquared += gMat.get(i,0) * gMat.get(i,0);
	 		  }
	 		  norm = Math.sqrt(normSquared);
	 		  if (norm > kTolerance) {
	 			  System.err.println("In DoglegStrategyFixtureEllipseValidSubspaceBasis (gn - basis*(basis.transpose()*gn)).norm() > kTolerance");
	 			  passed = false;  
	 		  }

	 		  if (passed) {
	 			  System.out.println("DoglegStrategyFixtureEllipseValidSubspaceBasis passed all tests");
	 		  }
	 		}
	 	    }
 	    
 	// Test if the step is correct if the gradient and the Gauss-Newton step point
 	// in the same direction and the Gauss-Newton step is outside the trust region,
 	// i.e. the trust region is active.
 	class DoglegStrategyFixtureValleyCorrectStepLocalOptimumAlongGradient extends DoglegStrategyFixtureValley {
 		final double kToleranceLoose = 1e-5;
 		public DoglegStrategyFixtureValleyCorrectStepLocalOptimumAlongGradient() {
 			super();
 			int i;
 			boolean passed = true;
 			LinearSolver linear_solver =
 		 		      new DenseQRSolver(new LinearSolverOptions());
 		 		  options_.linear_solver = linear_solver;
 		 		  options_.dogleg_type = DoglegType.SUBSPACE_DOGLEG;
 		 		  options_.initial_radius = 0.25;
 		 		  options_.max_radius = 0.25;
 	  
 		 		 DoglegStrategy strategy = new DoglegStrategy(options_);
 		 		  TrustRegionStrategyPerSolveOptions pso = new TrustRegionStrategyPerSolveOptions();

 		 		  TrustRegionStrategySummary summary = strategy.ComputeStep(pso,
 		 		                                                              jacobian_,
 		 		                                                              residual_,
 		 		                                                              x_);
 		 		if (summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_FAILURE) {
 		 			  System.err.println("In DoglegStrategyFixtureValleyCorrectStepLocalOptimumAlongGradient summary.termination_type == LINEAR_SOLVER_FAILURE");
 		 			  passed = false;
 		 		  }
 		 		  for (i = 0; i < 6; i++) {
 		 			  if (i != 2) {
	 		 			  if (Math.abs(x_[i]) > kToleranceLoose) {
	 		 				  System.err.println("In DoglegStrategyFixtureValleyCorrectStepLocalOptimumAlongGradient Math.abs(x_["+i+"]) > kToleranceLoose");
	 		 				  passed = false;
	 		 			  }
 		 			  }
 		 			  else {
 		 				  if (Math.abs(x_[2] - options_.initial_radius) > kToleranceLoose) {
 		 					System.err.println("In DoglegStrategyFixtureValleyCorrectStepLocalOptimumAlongGradient Math.abs(x_[2] - options_.initial_radius) > kToleranceLoose");
	 		 				passed = false;  
 		 				  }
 		 			  }
 		 		  }

 	              if (passed) {
 	            	  System.out.println("DoglegStrategyFixtureValleyCorrectStepLocalOptimumAlongGradient passed all tests");
 	              }
 		}
 	}
 	
	 // Test if the step is correct if the gradient and the Gauss-Newton step point
	 // in the same direction and the Gauss-Newton step is inside the trust region,
	 // i.e. the trust region is inactive.
 	class DoglegStrategyFixtureValleyCorrectStepGlobalOptimumAlongGradient extends DoglegStrategyFixtureValley {
 		final double kToleranceLoose = 1e-5;
 		public DoglegStrategyFixtureValleyCorrectStepGlobalOptimumAlongGradient() {
 			super();
 			int i;
 			boolean passed = true;
 			LinearSolver linear_solver =
 		 		      new DenseQRSolver(new LinearSolverOptions());
 		 		  options_.linear_solver = linear_solver;
 		 		  options_.dogleg_type = DoglegType.SUBSPACE_DOGLEG;
 		 		  options_.initial_radius = 2.0;
 		 		  options_.max_radius = 2.0;
 	  
 		 		 DoglegStrategy strategy = new DoglegStrategy(options_);
 		 		  TrustRegionStrategyPerSolveOptions pso = new TrustRegionStrategyPerSolveOptions();

 		 		  TrustRegionStrategySummary summary = strategy.ComputeStep(pso,
 		 		                                                              jacobian_,
 		 		                                                              residual_,
 		 		                                                              x_);
 		 		if (summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_FAILURE) {
 		 			  System.err.println("In DoglegStrategyFixtureValleyCorrectStepGlobalOptimumAlongGradient summary.termination_type == LINEAR_SOLVER_FAILURE");
 		 			  passed = false;
 		 		  }
 		 		  for (i = 0; i < 6; i++) {
 		 			  if (i != 2) {
	 		 			  if (Math.abs(x_[i]) > kToleranceLoose) {
	 		 				  System.err.println("In DoglegStrategyFixtureValleyCorrectStepGlobalOptimumAlongGradient Math.abs(x_["+i+"]) > kToleranceLoose");
	 		 				  passed = false;
	 		 			  }
 		 			  }
 		 			  else {
 		 				  if (Math.abs(x_[2] - 1.0) > kToleranceLoose) {
 		 					System.err.println("In DoglegStrategyFixtureValleyCorrectStepGlobalOptimumAlongGradient Math.abs(x_[2] - 1.0) > kToleranceLoose");
	 		 				passed = false;  
 		 				  }
 		 			  }
 		 		  }

 	              if (passed) {
 	            	  System.out.println("DoglegStrategyFixtureValleyCorrectStepGlobalOptimumAlongGradient passed all tests");
 	              }
 		}
 	}
 	
 	public void LineSearchPreprocessorZeroProblem() {
 		  // LineSearchPreprocessorZeroProblem() passed all tests
 		  ProblemImpl problem = new ProblemImpl();
 		  SolverOptions options = new SolverOptions();
 		  options.minimizer_type = MinimizerType.LINE_SEARCH;
 		  LineSearchPreprocessor preprocessor = new LineSearchPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (!preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In LineSearchPreprocessorZeroProblem() preprocessor.Preprocess(options, problem, pp) = false");
 		  }
 		  else {
 			  System.out.println("LineSearchPreprocessorZeroProblem() passed all tests");
 		  }
 		}
 	
 	public void LineSearchPreprocessorProblemWithInvalidParameterBlock() {
 		  // LineSearchPreprocessorProblemWithInvalidParameterBlock() passed all tests
 		  ProblemImpl problem = new ProblemImpl();
 		  double x[] = new double[] {Double.NaN};
 		  problem.AddParameterBlock(x, 1);
 		  SolverOptions options = new SolverOptions();
 		  options.minimizer_type = MinimizerType.LINE_SEARCH;
 		  LineSearchPreprocessor preprocessor = new LineSearchPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In LineSearchPreprocessorProblemWithInvalidParameterBlock() preprocessor.Preprocess(options, problem, pp) = true");
 		  }
 		  else {
 			  System.out.println("LineSearchPreprocessorProblemWithInvalidParameterBlock() passed all tests");
 		  }
 		}

 	public void LineSearchPreprocessorParameterBlockHasBounds() {
 		  // LineSearchPreprocessorParameterBlockHasBounds() passed all tests
 		  ProblemImpl problem = new ProblemImpl();
 		  double x[] = new double[] {1.0};
 		  problem.AddParameterBlock(x, 1);
 		  problem.SetParameterUpperBound(x, 0, 1.0);
 		  problem.SetParameterLowerBound(x, 0, 2.0);
 		  SolverOptions options = new SolverOptions();
 		  options.minimizer_type = MinimizerType.LINE_SEARCH;
 		  LineSearchPreprocessor preprocessor = new LineSearchPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In LineSearchPreprocessorParameterBlockHasBounds() preprocessor.Preprocess(options, problem, pp) = true");
 		  }
 		  else {
 			  System.out.println("LineSearchPreprocessorParameterBlockHasBounds() passed all tests");
 		  }
 		}
 	
 	class FailingCostFunction extends SizedCostFunction {
 		public FailingCostFunction() {
 			super(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0);
 		}
 		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
 			return false;
 		}
 		
 		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int[] jacobians_offset) {
 			return false;
 		}
 		 
 	};
 	
 	public void LineSearchPreprocessorRemoveParameterBlocksFailed() {
 		  // LineSearchPreprocessorRemoveParameterBlocksFailed() passed all tests
 		  ProblemImpl problem = new ProblemImpl();
 		  double x[] = new double[] {3.0};
 		  problem.AddResidualBlock(new FailingCostFunction(), null, x);
 		  problem.SetParameterBlockConstant(x);
 		  SolverOptions options = new SolverOptions();
 		  options.minimizer_type = MinimizerType.LINE_SEARCH;
 		  LineSearchPreprocessor preprocessor = new LineSearchPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In LineSearchPreprocessorRemoveParameterBlocksFailed() preprocessor.Preprocess(options, problem, pp) = true");
 		  }
 		  else {
 			  System.out.println("LineSearchPreprocessorRemoveParameterBlocksFailed() passed all tests");
 		  }
 		}


 	public void LineSearchPreprocessorRemoveParameterBlocksSucceeds() {
 		  // LineSearchPreprocessorRemoveParameterBlocksSucceeds() passed all tests
 		  ProblemImpl problem = new ProblemImpl();
 		  double x[] = new double[] {3.0};
 		  problem.AddParameterBlock(x, 1);
 		  SolverOptions options = new SolverOptions();
 		  options.minimizer_type = MinimizerType.LINE_SEARCH;

 		  LineSearchPreprocessor preprocessor = new LineSearchPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (!preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In LineSearchPreprocessorRemoveParameterBlocksSucceeds() preprocessor.Preprocess(options, problem, pp) = false");
 		  }
 		  else {
 			  System.out.println("LineSearchPreprocessorRemoveParameterBlocksSucceeds() passed all tests");
 		  }
 		}
 	
 	class DummyCostFunction2 extends SizedCostFunction {
 		public DummyCostFunction2(int kNumResiduals, int N1, int N2) {
 			super(kNumResiduals, N1, N2, 0, 0, 0, 0, 0, 0, 0, 0);
 		}
 		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
 			return true;
 		}
 		
 		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int[] jacobians_offset) {
 			return true;
 		}
 	 
 	};
 	
 	public void LineSearchPreprocessorNormalOperation() {
 		  // LineSearchPreprocessorNormalOperation() passed all tests
 		  boolean passed = true;
 		  ProblemImpl problem = new ProblemImpl();
 		  double x[] = new double[] {1.0};
 		  double y[] = new double[] {1.0};
 		  double z[] = new double[] {1.0};
 		  problem.AddResidualBlock(new DummyCostFunction2(1, 1, 1), null, x, y);
 		  problem.AddResidualBlock(new DummyCostFunction2(1, 1, 1), null, y, z);

 		  SolverOptions options = new SolverOptions();
 		  options.minimizer_type = MinimizerType.LINE_SEARCH;

 		  LineSearchPreprocessor preprocessor = new LineSearchPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (!preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In LineSearchPreprocessorNormalOperation() preprocessor.Preprocess(options, problem, pp) = false");
 			  passed = false;
 		  }
 		  if (pp.evaluator_options.linear_solver_type != LinearSolverType.CGNR) {
 			  System.err.println("In LineSearchPreprocessorNormalOperation() pp.evaluator_options.linear_solver_type != LinearSolverType.CGNR");
			  passed = false;  
 		  }
 		  if (pp.evaluator == null) {
 			  System.err.println("In LineSearchPreprocessorNormalOperation() pp.evaluator == null");
			  passed = false;    
 		  }
 		  if (passed) {
 			  System.out.println("LineSearchPreprocessorNormalOperation() passed all tests");
 		  }
 		}

 	public void TrustRegionPreprocessorZeroProblem() {
 		  // TrustRegionPreprocessorZeroProblem() passed all tests
 		  ProblemImpl problem = new ProblemImpl();
 		  SolverOptions options = new SolverOptions();
 		  TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (!preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In TrustRegionPreprocessorZeroProblem() preprocessor.Preprocess(options, problem, pp) = false");
 		  }
 		  else {
 			  System.out.println("TrustRegionPreprocessorZeroProblem() passed all tests");
 		  }
 		}

 	public void TrustRegionPreprocessorProblemWithInvalidParameterBlock() {
 		  // TrustRegionPreprocessorProblemWithInvalidParameterBlock() passed all tests
 		  ProblemImpl problem = new ProblemImpl();
 		  double x[] = new double[] {Double.NaN};
 		  problem.AddParameterBlock(x, 1);
 		  SolverOptions options = new SolverOptions();
 		  TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In TrustRegionPreprocessorProblemWithInvalidParameterBlock() preprocessor.Preprocess(options, problem, pp) = true");
 		  }
 		  else {
 			  System.out.println("TrustRegionPreprocessorProblemWithInvalidParameterBlock() passed all tests");
 		  }
 		}
 	
 	public void TrustRegionPreprocessorParameterBlockBoundsAreInvalid() {
 		  // TrustRegionPreprocessorParameterBlockBoundsAreInvalid() passed all tests
 		  ProblemImpl problem = new ProblemImpl();
 		  double x[] = new double[] {1.0};
 		  problem.AddParameterBlock(x, 1);
 		  problem.SetParameterUpperBound(x, 0, 1.0);
 		  problem.SetParameterLowerBound(x, 0, 2.0);
 		  SolverOptions options = new SolverOptions();
 		  TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In TrustRegionPreprocessorParameterBlockBoundsAreInvalid() preprocessor.Preprocess(options, problem, pp) = true");
 		  }
 		  else {
 			  System.out.println("TrustRegionPreprocessorParameterBlockBoundsAreInvalid() passed all tests");
 		  }
 		}

 	public void TrustRegionPreprocessorParamterBlockIsInfeasible() {
 		  // TrustRegionPreprocessorParamterBlockIsInfeasible() passed all tests
 		  ProblemImpl problem = new ProblemImpl();
 		  double x[] = new double[] {3.0};
 		  problem.AddParameterBlock(x, 1);
 		  problem.SetParameterUpperBound(x, 0, 1.0);
 		  problem.SetParameterLowerBound(x, 0, 2.0);
 		  problem.SetParameterBlockConstant(x);
 		  SolverOptions options = new SolverOptions();
 		  TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor(); 
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if(preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In TrustRegionPreprocessorParamterBlockIsInfeasible() preprocessor.Preprocess(options, problem, pp) = true");
 		  }
 		  else {
 			  System.out.println("TrustRegionPreprocessorParamterBlockIsInfeasible() passed all tests");
 		  }
 		}
 	
 	public void TrustRegionPreprocessorRemoveParameterBlocksFailed() {
 		  // TrustRegionPreprocessorRemoveParameterBlocksFailed() passed all tests
 		  ProblemImpl problem = new ProblemImpl();
 		  double x[] = new double[] {3.0};
 		  problem.AddResidualBlock(new FailingCostFunction(), null, x);
 		  problem.SetParameterBlockConstant(x);
 		  SolverOptions options = new SolverOptions();
 		  TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In TrustRegionPreprocessorRemoveParameterBlocksFailed() preprocessor.Preprocess(options, problem, pp) = true");
 		  }
 		  else {
 			  System.out.println("TrustRegionPreprocessorRemoveParameterBlocksFailed() passed all tests");
 		  }
 		}

 	public void TrustRegionPreprocessorRemoveParameterBlocksSucceeds() {
 		  // TrustRegionPreprocessorRemoveParameterBlocksSucceeds() passed all tests
 		  ProblemImpl problem = new ProblemImpl();
 		  double x[] = new double[] {3.0};
 		  problem.AddParameterBlock(x, 1);
 		  SolverOptions options = new SolverOptions();
 		  TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (!preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In TrustRegionPreprocessorRemoveParameterBlocksSucceeds() preprocessor.Preprocess(options, problem, pp) = false");
 		  }
 		  else {
 			  System.out.println("TrustRegionPreprocessorRemoveParameterBlocksSucceeds() passed all tests");
 		  }
 		}

 	class DummyCostFunction3 extends SizedCostFunction {
 		private int kNumResiduals;
 		private int N1;
 		private int N2;
 		private int N3;
 		
 		public DummyCostFunction3(int kNumResiduals, int N1) {
 			super(kNumResiduals, N1, 0, 0, 0, 0, 0, 0, 0, 0, 0);
 			this.kNumResiduals = kNumResiduals;
 			this.N1 = N1;
 			this.N2 = 0;
 			this.N3 = 0;
 		}
 		public DummyCostFunction3(int kNumResiduals, int N1, int N2) {
 			super(kNumResiduals, N1, N2, 0, 0, 0, 0, 0, 0, 0, 0);
 			this.kNumResiduals = kNumResiduals;
 			this.N1 = N1;
 			this.N2 = N2;
 			this.N3 = 0;
 		}
 		public DummyCostFunction3(int kNumResiduals, int N1, int N2, int N3) {
 			super(kNumResiduals, N1, N2, N3, 0, 0, 0, 0, 0, 0, 0);
 			this.kNumResiduals = kNumResiduals;
 			this.N1 = N1;
 			this.N2 = N2;
 			this.N3 = N3;
 		}
 		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
 		int i;
 			
 	    for (i = 0; i < kNumResiduals; ++i) {
 	      residuals[i] = kNumResiduals * kNumResiduals + i;
 	    }

 	    if (jacobians == null) {
 	      return true;
 	    }

 	    if (jacobians[0] != null) {
 	      for (i = 0; i < kNumResiduals * N1; i++) {
 	          jacobians[0][i] = kNumResiduals * N1;  
 	      }
 	    }

 	    if (N2 == 0) {
 	      return true;
 	    }

 	    if (jacobians[1] != null) {
 	    	for (i = 0; i < kNumResiduals * N2; i++) {
 	 	          jacobians[1][i] = kNumResiduals * N2;  
 	 	    }
 	    }

 	    if (N3 == 0) {
 	      return true;
 	    }

 	    if (jacobians[2] != null) {
 	    	for (i = 0; i < kNumResiduals * N3; i++) {
	 	          jacobians[2][i] = kNumResiduals * N3;  
	 	    }
 	    }

 	    return true;
 	  }
 		
 		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int[] jacobians_offset) {
 	 		int i;
 	 			
 	 	    for (i = 0; i < kNumResiduals; ++i) {
 	 	      residuals[i] = kNumResiduals * kNumResiduals + i;
 	 	    }

 	 	    if (jacobians == null) {
 	 	      return true;
 	 	    }

 	 	    if (jacobians[0] != null) {
 	 	      for (i = 0; i < kNumResiduals * N1; i++) {
 	 	          jacobians[0][jacobians_offset[0] + i] = kNumResiduals * N1;  
 	 	      }
 	 	    }

 	 	    if (N2 == 0) {
 	 	      return true;
 	 	    }

 	 	    if (jacobians[1] != null) {
 	 	    	for (i = 0; i < kNumResiduals * N2; i++) {
 	 	 	          jacobians[1][jacobians_offset[1] + i] = kNumResiduals * N2;  
 	 	 	    }
 	 	    }

 	 	    if (N3 == 0) {
 	 	      return true;
 	 	    }

 	 	    if (jacobians[2] != null) {
 	 	    	for (i = 0; i < kNumResiduals * N3; i++) {
 		 	          jacobians[2][jacobians_offset[2] + i] = kNumResiduals * N3;  
 		 	    }
 	 	    }

 	 	    return true;
 	 	  }
 	};
 	
 	class LinearSolverAndEvaluatorCreationTest {
 		 private double x_[];
 		 private double y_[];
 		 private double z_[];
 		 ProblemImpl problem_;
 		 public LinearSolverAndEvaluatorCreationTest(ProblemImpl problem_, double x_[], double y_[], double z_[]) {
 			 this.problem_ = problem_;
 			 this.x_ = x_;
 			 this.y_ = y_;
 			 this.z_ = z_;
 			 SetUp();
 		 }
 		 public void SetUp() {
 		    x_[0] = 1.0;
 		    y_[0] = 1.0;
 		    z_[0] = 1.0;
 		    problem_.AddResidualBlock(new DummyCostFunction3(1, 1, 1), null, x_, y_);
 		    problem_.AddResidualBlock(new DummyCostFunction3(1, 1, 1), null, y_, z_);
 		  }
 	}
 	
 	public void PreprocessForGivenLinearSolverAndVerify(
 		      LinearSolverType linear_solver_type, ProblemImpl problem_, String testName) {
 		    boolean passed = true;
 		    SolverOptions options = new SolverOptions();
 		    options.linear_solver_type = linear_solver_type;
 		    TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
 		    PreprocessedProblem pp = new PreprocessedProblem();
 		    if (!preprocessor.Preprocess(options, problem_, pp)) {
 		    	System.err.println("In " + testName + " preprocessor.Preprocess(options, problem_, pp) = false");
 		    	passed = false;
 		    }
 		    if (pp.options.linear_solver_type != linear_solver_type) {
 		    	System.err.println("In " + testName + " pp.options.linear_solver_type != linear_solver_type");
 		    	passed = false;	
 		    }
 		    if (pp.linear_solver_options.type != linear_solver_type) {
 		    	System.err.println("In " + testName + " pp.linear_solver_options.type != linear_solver_type");
 		    	passed = false;		
 		    }
 		    if (pp.linear_solver ==  null) {
 		    	System.err.println("In " + testName + " pp.linear_solver ==  null");
 		    	passed = false;	
 		    }
 		    if (pp.evaluator == null) {
 		    	System.err.println("In " + testName + " pp.evaluator == null");
 		    	passed = false;
 		    }
 		    if (passed) {
 		    	System.out.println(testName + " passed all tests");
 		    }
 		  }

 	public void LinearSolverAndEvaluatorCreationTestDenseQR() {
 		  // LinearSolverAndEvaluatorCreationTestDenseQR() passed all tests
 		  ProblemImpl problem_ = new ProblemImpl();
 		  double x_[] = new double[1];
 		  double y_[] = new double[1];
 		  double z_[] = new double[1];
 		  new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
 		  PreprocessForGivenLinearSolverAndVerify(LinearSolverType.DENSE_QR, problem_, "LinearSolverAndEvaluatorCreationTestDenseQR()");
 	}
 	
 	public void LinearSolverAndEvaluatorCreationTestDenseNormalCholesky() {
 		  // LinearSolverAndEvaluatorCreationTestDenseNormalCholesky() passed all tests
 		  ProblemImpl problem_ = new ProblemImpl();
 		  double x_[] = new double[1];
		  double y_[] = new double[1];
		  double z_[] = new double[1];
		  new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
 		  PreprocessForGivenLinearSolverAndVerify(LinearSolverType.DENSE_NORMAL_CHOLESKY, problem_, "LinearSolverAndEvaluatorCreationTestDenseNormalCholesky()");
 	}

 	public void LinearSolverAndEvaluatorCreationTestDenseSchur() {
 		  // LinearSolverAndEvaluatorCreationTestDenseSchur() passed all tests
 		  ProblemImpl problem_ = new ProblemImpl();
 		  double x_[] = new double[1];
		  double y_[] = new double[1];
		  double z_[] = new double[1];
		  new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
 		  PreprocessForGivenLinearSolverAndVerify(LinearSolverType.DENSE_SCHUR, problem_, "LinearSolverAndEvaluatorCreationTestDenseSchur()");
 	}
 	
 	public void LinearSolverAndEvaluatorCreationTestCGNR() {
 		  // LinearSolverAndEvaluatorCreationTestCGNR() passed all tests
 		  ProblemImpl problem_ = new ProblemImpl();
 		  double x_[] = new double[1];
		  double y_[] = new double[1];
		  double z_[] = new double[1];
		  new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
 		  PreprocessForGivenLinearSolverAndVerify(LinearSolverType.CGNR, problem_, "LinearSolverAndEvaluatorCreationTestCGNR()");
    }
 	
 	public void LinearSolverAndEvaluatorCreationTestIterativeSchur() {
 		  // LinearSolverAndEvaluatorCreationTestIterativeSchur() passed all tests
 		  ProblemImpl problem_ = new ProblemImpl();
 		  double x_[] = new double[1];
		  double y_[] = new double[1];
		  double z_[] = new double[1];
		  new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
 		  PreprocessForGivenLinearSolverAndVerify(LinearSolverType.ITERATIVE_SCHUR, problem_, "LinearSolverAndEvaluatorCreationTestIterativeSchur()");
 	}


 	public void LinearSolverAndEvaluatorCreationTestMinimizerIsAwareOfBounds() {
 		  // LinearSolverAndEvaluatorCreationTestMinimizerIsAwareOfBounds() passed all tests
 		  boolean passed = true;
 		  ProblemImpl problem_ = new ProblemImpl();
 		  double x_[] = new double[1];
		  double y_[] = new double[1];
		  double z_[] = new double[1];
		  new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
 		  problem_.SetParameterLowerBound(x_, 0, 0.0);
 		  SolverOptions options = new SolverOptions();
 		  TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		    if (!preprocessor.Preprocess(options, problem_, pp)) {
		    	System.err.println("In LinearSolverAndEvaluatorCreationTestMinimizerIsAwareOfBounds() preprocessor.Preprocess(options, problem_, pp) = false");
		    	passed = false;
		    }
		    if (pp.options.linear_solver_type != options.linear_solver_type) {
		    	System.err.println("In LinearSolverAndEvaluatorCreationTestMinimizerIsAwareOfBounds() pp.options.linear_solver_type != options.linear_solver_type");
		    	passed = false;	
		    }
		    if (pp.linear_solver_options.type != options.linear_solver_type) {
		    	System.err.println("In LinearSolverAndEvaluatorCreationTestMinimizerIsAwareOfBounds() pp.linear_solver_options.type != options.linear_solver_type");
		    	passed = false;		
		    }
		    if (pp.evaluator_options.linear_solver_type != options.linear_solver_type) {
		    	System.err.println("In LinearSolverAndEvaluatorCreationTestMinimizerIsAwareOfBounds() pp.evaluator_options.linear_solver_type != options.linear_solver_type");
		    	passed = false;	
		    }
		    if (pp.linear_solver ==  null) {
		    	System.err.println("In LinearSolverAndEvaluatorCreationTestMinimizerIsAwareOfBounds() pp.linear_solver ==  null");
		    	passed = false;	
		    }
		    if (pp.evaluator == null) {
		    	System.err.println("In LinearSolverAndEvaluatorCreationTestMinimizerIsAwareOfBounds() pp.evaluator == null");
		    	passed = false;
		    }
 		    if (!pp.minimizer_options.is_constrained) {
 		    	System.err.println("In LinearSolverAndEvaluatorCreationTestMinimizerIsAwareOfBounds() pp.minimizer_options.is_constrained = false");
		    	passed = false;	
 		    }
 		    if (passed) {
 		    	System.out.println("LinearSolverAndEvaluatorCreationTestMinimizerIsAwareOfBounds() passed all tests");
 		    }
 		}
 	
 	public void LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithBadOrdering() {
 		  // LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithBadOrdering() passed all tests
		  ProblemImpl problem_ = new ProblemImpl();
		  double x_[] = new double[1];
		  double y_[] = new double[1];
		  double z_[] = new double[1];
		  new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
 		  SolverOptions options = new SolverOptions();
 		  options.linear_solver_type = LinearSolverType.DENSE_SCHUR;
 		  options.linear_solver_ordering = new OrderedGroups<double[]>();
 		  options.linear_solver_ordering.AddElementToGroup(x_, 0);
 		  options.linear_solver_ordering.AddElementToGroup(y_, 0);
 		  options.linear_solver_ordering.AddElementToGroup(z_, 1);

 		  TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (preprocessor.Preprocess(options, problem_, pp)) {
 			  System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithBadOrdering() = true");
 		  }
 		  else {
 			  System.out.println("LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithBadOrdering() passed all tests");
 		  }
 		}
 	
 	public void LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithGoodOrdering() {
 		  // LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithGoodOrdering() passed all tests
 		  boolean passed = true;
 		  ProblemImpl problem_ = new ProblemImpl();
		  double x_[] = new double[1];
		  double y_[] = new double[1];
		  double z_[] = new double[1];
		  new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
		  SolverOptions options = new SolverOptions();
		  options.linear_solver_type = LinearSolverType.DENSE_SCHUR;
		  options.linear_solver_ordering = new OrderedGroups<double[]>();
		  options.linear_solver_ordering.AddElementToGroup(x_, 0);
		  options.linear_solver_ordering.AddElementToGroup(z_, 0);
		  options.linear_solver_ordering.AddElementToGroup(y_, 1);

		  TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (!preprocessor.Preprocess(options, problem_, pp)) {
 			  System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithGoodOrdering() preprocessor.Preprocess(options, problem_, pp) = false");
 			  passed = false;
 		  }
 		  if (pp.options.linear_solver_type != LinearSolverType.DENSE_SCHUR) {
 			  System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithGoodOrdering() pp.options.linear_solver_type != DENSE_SCHUR");
			  passed = false;  
 		  }
 		  if (pp.linear_solver_options.type != LinearSolverType.DENSE_SCHUR) {
 			  System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithGoodOrdering() pp.linear_solver_options.type != DENSE_SCHUR");
			  passed = false; 
 		  }
 		  if (pp.evaluator_options.linear_solver_type != LinearSolverType.DENSE_SCHUR) {
 			  System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithGoodOrdering() pp.evaluator_options.linear_solver_type != DENSE_SCHUR");
			  passed = false;   
 		  }
 		  if (pp.linear_solver == null)  {
 			  System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithGoodOrdering() pp.linear_solver == null");
			  passed = false;
 		  }
 		  if (pp.evaluator == null) {
 			  System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithGoodOrdering() pp.evaluator == null");
			  passed = false;  
 		  }
 		  if (passed) {
 			  System.out.println("LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithGoodOrdering() passed all tests");
 		  }
 		}


 	public void LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptyFirstEliminationGroup() {
 		    // LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptyFirstEliminationGroup() passed all tests
 		    boolean passed = true;
		    ProblemImpl problem_ = new ProblemImpl();
		    double x_[] = new double[1];
		    double y_[] = new double[1];
		    double z_[] = new double[1];
		    new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
			problem_.SetParameterBlockConstant(x_);
			problem_.SetParameterBlockConstant(z_);
			
			SolverOptions options = new SolverOptions();
			options.linear_solver_type = LinearSolverType.DENSE_SCHUR;
			options.linear_solver_ordering = new OrderedGroups<double[]>();
			options.linear_solver_ordering.AddElementToGroup(x_, 0);
			options.linear_solver_ordering.AddElementToGroup(z_, 0);
			options.linear_solver_ordering.AddElementToGroup(y_, 1);
			
			
			TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
			PreprocessedProblem pp = new PreprocessedProblem();
			if (!preprocessor.Preprocess(options, problem_, pp)) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptyFirstEliminationGroup()");
				System.err.println("preprocessor.Preprocess(options, problem_, pp) = false");
				passed = false;
			}
			if (pp.options.linear_solver_type != LinearSolverType.DENSE_QR) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptyFirstEliminationGroup()");
				System.err.println("pp.options.linear_solver_type != LinearSolverType.DENSE_QR");
				passed = false;
			}
			if (pp.linear_solver_options.type != LinearSolverType.DENSE_QR) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptyFirstEliminationGroup()");
				System.err.println("pp.linear_solver_options.type != LinearSolverType.DENSE_QR");
				passed = false;
			}
			if (pp.evaluator_options.linear_solver_type != LinearSolverType.DENSE_QR) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptyFirstEliminationGroup()");
				System.err.println("pp.evaluator_options.linear_solver_type != LinearSolverType.DENSE_QR");
				passed = false;
			}
			if (pp.linear_solver == null) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptyFirstEliminationGroup()");
				System.err.println("pp.linear_solver == null");
				passed = false;
			}
			if (pp.evaluator == null) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptyFirstEliminationGroup()");
				System.err.println("pp.evaluator == null");
				passed = false;
			}
			if (passed) {
				System.out.println("LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptyFirstEliminationGroup() passed all tests");
			}
		}
 	
 	public void LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptySecondEliminationGroup() {
 		    // LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptySecondEliminationGroup() passed all tests
 		    boolean passed = true;
		    ProblemImpl problem_ = new ProblemImpl();
		    double x_[] = new double[1];
		    double y_[] = new double[1];
		    double z_[] = new double[1];
		    new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
			problem_.SetParameterBlockConstant(y_);
			
			SolverOptions options = new SolverOptions();
			options.linear_solver_type = LinearSolverType.DENSE_SCHUR;
			options.linear_solver_ordering = new OrderedGroups<double[]>();
			options.linear_solver_ordering.AddElementToGroup(x_, 0);
			options.linear_solver_ordering.AddElementToGroup(z_, 0);
			options.linear_solver_ordering.AddElementToGroup(y_, 1);
			
			
			TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
			PreprocessedProblem pp = new PreprocessedProblem();
			if (!preprocessor.Preprocess(options, problem_, pp)) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptySecondEliminationGroup()");
				System.err.println("preprocessor.Preprocess(options, problem_, pp) = false");
				passed = false;
			}
			if (pp.options.linear_solver_type != LinearSolverType.DENSE_SCHUR) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptySecondEliminationGroup()");
				System.err.println("pp.options.linear_solver_type != LinearSolverType.DENSE_SCHUR");
				passed = false;
			}
			if (pp.linear_solver_options.type != LinearSolverType.DENSE_SCHUR) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptySecondEliminationGroup()");
				System.err.println("pp.linear_solver_options.type != LinearSolverType.DENSE_SCHUR");
				passed = false;
			}
			if (pp.evaluator_options.linear_solver_type != LinearSolverType.DENSE_SCHUR) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptySecondEliminationGroup()");
				System.err.println("pp.evaluator_options.linear_solver_type != LinearSolverType.DENSE_SCHUR");
				passed = false;
			}
			if (pp.linear_solver == null) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptySecondEliminationGroup()");
				System.err.println("pp.linear_solver == null");
				passed = false;
			}
			if (pp.evaluator == null) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptySecondEliminationGroup()");
				System.err.println("pp.evaluator == null");
				passed = false;
			}
			if (passed) {
				System.out.println("LinearSolverAndEvaluatorCreationTestSchurTypeSolverWithEmptySecondEliminationGroup() passed all tests");
			}
			
	  }

 	public void TrustRegionPreprocessorTestInnerIterationsWithOneParameterBlock() {
 		  // TrustRegionPreprocessorTestInnerIterationsWithOneParameterBlock() passed all tests
 		  boolean passed = true;
 		  ProblemImpl problem = new ProblemImpl();
 		  double x[] = new double[] {1.0};
 		  problem.AddResidualBlock(new DummyCostFunction3(1, 1), null, x);

 		  SolverOptions options = new SolverOptions();
 		  options.use_inner_iterations = true;

 		  TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
 		  PreprocessedProblem pp = new PreprocessedProblem();
 		  if (!preprocessor.Preprocess(options, problem, pp)) {
 			  System.err.println("In TrustRegionPreprocessorTestInnerIterationsWithOneParameterBlock() preprocessor.Preprocess(options, problem, pp) = false");
 			  passed = false;
 		  }
 		  if (pp.linear_solver == null) {
 			  System.err.println("In TrustRegionPreprocessorTestInnerIterationsWithOneParameterBlock() pp.linear_solver == null");
			  passed = false;
 		  }
 		  if (pp.evaluator == null) {
 			  System.err.println("In TrustRegionPreprocessorTestInnerIterationsWithOneParameterBlock() pp.evaluator == null");
			  passed = false; 
 		  }
 		  if (pp.inner_iteration_minimizer != null) {
 			  System.err.println("In TrustRegionPreprocessorTestInnerIterationsWithOneParameterBlock() pp.inner_iteration_minimizer != null");
			  passed = false; 
 		  }
 		  if (passed) {
 			  System.out.println("TrustRegionPreprocessorTestInnerIterationsWithOneParameterBlock() passed all tests");
 		  }
 		}
 	
 	public void LinearSolverAndEvaluatorCreationTestInnerIterationsWithTwoParameterBlocks() {
 		    // LinearSolverAndEvaluatorCreationTestInnerIterationsWithTwoParameterBlocks() passed all tests
	 		boolean passed = true;
		    ProblemImpl problem_ = new ProblemImpl();
		    double x_[] = new double[1];
		    double y_[] = new double[1];
		    double z_[] = new double[1];
	        new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
			SolverOptions options = new SolverOptions();
			options.use_inner_iterations = true;
			
			TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
			PreprocessedProblem pp = new PreprocessedProblem();
			if (!preprocessor.Preprocess(options, problem_, pp)) {
	 			  System.err.println("In LinearSolverAndEvaluatorCreationTestInnerIterationsWithTwoParameterBlocks() preprocessor.Preprocess(options, problem_, pp) = false");
	 			  passed = false;
	 		  }
	 		  if (pp.linear_solver == null) {
	 			  System.err.println("In LinearSolverAndEvaluatorCreationTestInnerIterationsWithTwoParameterBlocks() pp.linear_solver == null");
				  passed = false;
	 		  }
	 		  if (pp.evaluator == null) {
	 			  System.err.println("In LinearSolverAndEvaluatorCreationTestInnerIterationsWithTwoParameterBlocks() pp.evaluator == null");
				  passed = false; 
	 		  }
	 		  if (pp.inner_iteration_minimizer == null) {
	 			  System.err.println("In LinearSolverAndEvaluatorCreationTestInnerIterationsWithTwoParameterBlocks() pp.inner_iteration_minimizer == null");
				  passed = false; 
	 		  }
	 		  if (passed) {
	 			  System.out.println("LinearSolverAndEvaluatorCreationTestInnerIterationsWithTwoParameterBlocks() passed all tests");
	 		  }
			}

 	public void  LinearSolverAndEvaluatorCreationTestInvalidInnerIterationsOrdering() {
 		    // LinearSolverAndEvaluatorCreationTestInvalidInnerIterationsOrdering() passed all tests
	 		ProblemImpl problem_ = new ProblemImpl();
		    double x_[] = new double[1];
		    double y_[] = new double[1];
		    double z_[] = new double[1];
	        new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
			SolverOptions options = new SolverOptions();
			options.use_inner_iterations = true;
			options.inner_iteration_ordering = new OrderedGroups<double[]>();
			options.inner_iteration_ordering.AddElementToGroup(x_, 0);
			options.inner_iteration_ordering.AddElementToGroup(z_, 0);
			options.inner_iteration_ordering.AddElementToGroup(y_, 0);
			
			TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
			PreprocessedProblem pp = new PreprocessedProblem();
			if (preprocessor.Preprocess(options, problem_, pp)) {
				System.err.println("In LinearSolverAndEvaluatorCreationTestInvalidInnerIterationsOrdering() preprocessor.Preprocess(options, problem_, pp) = true");
			}
			else {
				System.out.println("LinearSolverAndEvaluatorCreationTestInvalidInnerIterationsOrdering() passed all tests");
			}
	  }
 	
 	public void LinearSolverAndEvaluatorCreationTestValidInnerIterationsOrdering() {
 		// LinearSolverAndEvaluatorCreationTestValidInnerIterationsOrdering() passed all tests
 		boolean passed = true;
 		ProblemImpl problem_ = new ProblemImpl();
	    double x_[] = new double[1];
	    double y_[] = new double[1];
	    double z_[] = new double[1];
        new LinearSolverAndEvaluatorCreationTest(problem_, x_, y_, z_);
		SolverOptions options = new SolverOptions();
		options.use_inner_iterations = true;
		options.inner_iteration_ordering = new OrderedGroups<double[]>();
		options.inner_iteration_ordering.AddElementToGroup(x_, 0);
		options.inner_iteration_ordering.AddElementToGroup(z_, 0);
		options.inner_iteration_ordering.AddElementToGroup(y_, 1);
 		 
 		TrustRegionPreprocessor preprocessor = new TrustRegionPreprocessor();
 		 PreprocessedProblem pp = new PreprocessedProblem();
 		 if (!preprocessor.Preprocess(options, problem_, pp)) {
 			 System.err.println("In LinearSolverAndEvaluatorCreationTestValidInnerIterationsOrdering() preprocessor.Preprocess(options, problem_, pp) = false");
 			 passed = false;
 		 }
 		 if (pp.linear_solver == null) {
 			 System.err.println("In LinearSolverAndEvaluatorCreationTestValidInnerIterationsOrdering() pp.linear_solver == null");
			 passed = false; 
 		 }
 		 if (pp.evaluator == null) {
 			 System.err.println("In LinearSolverAndEvaluatorCreationTestValidInnerIterationsOrdering() pp.evaluator == null");
			 passed = false;  
 		 }
 		 if (pp.inner_iteration_minimizer == null) {
 			System.err.println("In LinearSolverAndEvaluatorCreationTestValidInnerIterationsOrdering() pp.inner_iteration_minimizer == null");
			 passed = false;  
 		 }
 		 if (passed) {
 			 System.out.println("LinearSolverAndEvaluatorCreationTestValidInnerIterationsOrdering() passed all tests");
 		 }
 		}

 	class QuadraticFirstOrderFunction extends FirstOrderFunction {
 		 public QuadraticFirstOrderFunction() {
 			 super();
 		 }
 		 public boolean Evaluate(double[] parameters,
 		                        double[] cost,
 		                        double[] gradient) {

 		    cost[0] = parameters[0] * parameters[0];
 		    if (gradient != null) {
 		      gradient[0] = 2.0 * parameters[0];
 		    }
 		    return true;
 		  }

 		  public int NumParameters() { return 1; }
 		};

 		public void LineSearchMinimizerTestFinalCostIsZero() {
 			  // LineSearchMinimizerTestFinalCostIsZero() passed all tests
 			  double parameters[] = new double[] {2.0};
 			  GradientProblem problem = new GradientProblem(new QuadraticFirstOrderFunction());
 			  GradientProblemSolverOptions options = new GradientProblemSolverOptions();
 			  GradientProblemSolverSummary summary = new GradientProblemSolverSummary();
 			  Solve(options, problem, parameters, summary);
 			  if (Math.abs(summary.final_cost) > epsilon) {
 				  System.err.println("In LineSearchMinimizerTestFinalCostIsZero() Math.abs(summary.final_cost) > epsilon");
 			  }
 			  else {
 				  System.out.println("LineSearchMinimizerTestFinalCostIsZero() passed all tests");
 			  }
 	   }
 		
 		class PowellEvaluator2 extends Evaluator {
 			private final int num_active_cols_;
 			private boolean col1;
 			private boolean col2;
 			private boolean col3;
 			private boolean col4;
 			 public PowellEvaluator2(boolean col1, boolean col2, boolean col3, boolean col4) {
 				      super();
 				      this.col1 = col1;
 				      this.col2 = col2;
 				      this.col3 = col3;
 				      this.col4 = col4;
 			          num_active_cols_ = (col1 ? 1 : 0) +
 			          (col2 ? 1 : 0) +
 			          (col3 ? 1 : 0) +
 			          (col4 ? 1 : 0);
 			          if (1 <= MAX_LOG_LEVEL) {
 			        	  Preferences.debug("col1 = " + col1 + " col2 = " + col2 + " col3 = " + col3 + " col4 = " + col4 + "\n",
 			        			  Preferences.DEBUG_ALGORITHM);
 			          }
 			  }


 			  // Implementation of Evaluator interface.
 			  public SparseMatrix CreateJacobian() {
 			    if (!(col1 || col2 || col3 || col4)) {
 			    	System.err.println("Cannot have col1 = col2 = col3 = col4 = false in CreateJacobian");
 			    	return null;
 			    }
 			    DenseSparseMatrix dense_jacobian =
 			        new DenseSparseMatrix(NumResiduals(), NumEffectiveParameters());
 			    dense_jacobian.SetZero();
 			    return dense_jacobian;
 			  }
 			  
 			 public boolean Evaluate(EvaluateOptions evaluate_options,
 	                Vector<Double> state,
 	                double[] cost,
 	                Vector<Double> residuals,
 	                Vector<Double> gradient,
 	                SparseMatrix jacobian) {
 				 int i;
 				 boolean cond;
 				 double state_array[] = new double[state.size()];
 				 for (i = 0; i < state.size(); i++) {
 					 state_array[i] = state.get(i);
 				 }
 				 double residuals_array[] = new double[residuals.size()];
 				 for (i = 0; i < residuals.size(); i++) {
 					 residuals_array[i] = residuals.get(i);
 				 }
 				 double gradient_array[] = new double[gradient.size()];
 				 for (i = 0; i < gradient.size(); i++) {
 					 gradient_array[i] = gradient.get(i);
 				 }
 				 cond = Evaluate(evaluate_options, state_array, cost, residuals_array, gradient_array, jacobian);
 				 for (i = 0; i < residuals_array.length; i++) {
 					 residuals.set(i, residuals_array[i]);
 				 }
 				 for (i = 0; i < gradient_array.length; i++) {
 					 gradient.set(i, gradient_array[i]);
 				 }
 				 return cond;
 			 }

 			  public boolean Evaluate(EvaluateOptions evaluate_options,
 			                        double[] state,
 			                        double[] cost,
 			                        double[] residuals,
 			                        double[] gradient,
 			                        SparseMatrix jacobian) {
 				int r, c;
 			    final double x1 = state[0];
 			    final double x2 = state[1];
 			    final double x3 = state[2];
 			    final double x4 = state[3];

 			   if (1 <= MAX_LOG_LEVEL) {
 				      Preferences.debug("State: \n", Preferences.DEBUG_ALGORITHM);
		        	  Preferences.debug("x1 = " + x1 + " x2 = " + x2 + "\n", Preferences.DEBUG_ALGORITHM);
		        	  Preferences.debug("x3 = " + x3 + " x4 = " + x4 + "\n", Preferences.DEBUG_ALGORITHM);
		        }
 			    
 			    final double f1 = x1 + 10.0 * x2;
 			    final double f2 = Math.sqrt(5.0) * (x3 - x4);
 			    final double f3 = Math.pow(x2 - 2.0 * x3, 2.0);
 			    final double f4 = Math.sqrt(10.0) * Math.pow(x1 - x4, 2.0);
 			    
 			   if (1 <= MAX_LOG_LEVEL) {
				      Preferences.debug("Function: \n", Preferences.DEBUG_ALGORITHM);
		        	  Preferences.debug("f1 = " + f1 + " f2 = " + f2 + "\n", Preferences.DEBUG_ALGORITHM);
		        	  Preferences.debug("f3 = " + f3 + " f4 = " + f4 + "\n", Preferences.DEBUG_ALGORITHM);
		        }

 			    cost[0] = (f1*f1 + f2*f2 + f3*f3 + f4*f4) / 2.0;

 			   if (1 <= MAX_LOG_LEVEL) {
				      Preferences.debug("Cost: " + cost[0] +"\n", Preferences.DEBUG_ALGORITHM);
 			   }

 			    if (residuals != null) {
 			      residuals[0] = f1;
 			      residuals[1] = f2;
 			      residuals[2] = f3;
 			      residuals[3] = f4;
 			    }

 			    if (jacobian != null) {
 			      DenseSparseMatrix dense_jacobian = (DenseSparseMatrix)(jacobian);
 			      dense_jacobian.SetZero();

 			      //ColMajorMatrixRef jacobian_matrix = dense_jacobian.mutable_matrix();
 			      Matrix jacobian_matrix = dense_jacobian.mutable_matrix();
 			      if (jacobian_matrix.getColumnDimension() != num_active_cols_) {
 			    	  System.err.println("In Evaluate jacobian_matrix.getColumnDimension() != num_active_cols_");
 			    	  return false;
 			      }

 			      int column_index = 0;
 			      if (col1) {
 			    	jacobian_matrix.set(0, column_index, 1.0);
 			    	jacobian_matrix.set(1, column_index, 0.0);
 			    	jacobian_matrix.set(2, column_index, 0.0);
 			    	jacobian_matrix.set(3, column_index, Math.sqrt(10.0) * 2.0 * (x1 - x4) * (1.0 - x4));
 			        column_index++;  
 			      }
 			      
 			      if (col2) {
 			    	jacobian_matrix.set(0, column_index, 10.0);
  			    	jacobian_matrix.set(1, column_index, 0.0);
  			    	jacobian_matrix.set(2, column_index, 2.0*(x2 - 2.0*x3)*(1.0 - 2.0*x3));
  			    	jacobian_matrix.set(3, column_index, 0.0);
  			        column_index++;  
 			      }

 			      if (col3) {
 			    	jacobian_matrix.set(0, column_index, 0.0);
   			    	jacobian_matrix.set(1, column_index, Math.sqrt(5.0));
   			    	jacobian_matrix.set(2, column_index, 2.0*(x2 - 2.0*x3)*(x2 - 2.0));
   			    	jacobian_matrix.set(3, column_index, 0.0);
   			        column_index++;  
 			      }

 			      if (col4) {
 			    	 jacobian_matrix.set(0, column_index, 0.0);
    			     jacobian_matrix.set(1, column_index, -Math.sqrt(5.0));
    			     jacobian_matrix.set(2, column_index, 0.0);
    			     jacobian_matrix.set(3, column_index, Math.sqrt(10.0) * 2.0 * (x1 - x4) * (x1 - 1.0));
    			     column_index++;  
 			      }
 			      dense_jacobian.restoreBackFromColMajorRef(jacobian_matrix);
 			     if (1 <= MAX_LOG_LEVEL) {
				      Preferences.debug("\njacobian_matrix\n", Preferences.DEBUG_ALGORITHM);
				      for (r = 0; r < jacobian_matrix.getRowDimension(); r++) {
				    	  for (c = 0; c < jacobian_matrix.getColumnDimension(); c++) {
				    		  Preferences.debug("jacobian_matrix["+r+"]["+c+"] = " + jacobian_matrix.get(r,c) + "\n",
				    				  Preferences.DEBUG_ALGORITHM);
				    	  }
				      }
			      }
 			    }

 			    if (gradient != null) {
 			      int column_index = 0;
 			      if (col1) {
 			        gradient[column_index++] = f1  + f4 * Math.sqrt(10.0) * 2.0 * (x1 - x4);
 			      }

 			      if (col2) {
 			        gradient[column_index++] = f1 * 10.0 + f3 * 2.0 * (x2 - 2.0 * x3);
 			      }

 			      if (col3) {
 			        gradient[column_index++] =
 			            f2 * Math.sqrt(5.0) + f3 * (2.0 * 2.0 * (2.0 * x3 - x2));
 			      }

 			      if (col4) {
 			        gradient[column_index++] =
 			            -f2 * Math.sqrt(5.0) + f4 * Math.sqrt(10.0) * 2.0 * (x4 - x1);
 			      }
 			    }

 			    return true;
 			  }

 			  public boolean Plus(double[] state,
 			                    double[] delta,
 			                    double[] state_plus_delta) {
 			    int delta_index = 0;
 			    state_plus_delta[0] = (col1  ? state[0] + delta[delta_index++] : state[0]);
 			    state_plus_delta[1] = (col2  ? state[1] + delta[delta_index++] : state[1]);
 			    state_plus_delta[2] = (col3  ? state[2] + delta[delta_index++] : state[2]);
 			    state_plus_delta[3] = (col4  ? state[3] + delta[delta_index++] : state[3]);
 			    return true;
 			  }
 			  
 			 public boolean Plus(Vector<Double> state,
	                    Vector<Double> delta,
	                    Vector<Double> state_plus_delta) {
 				 int i;
 				 boolean cond;
 				 double state_array[] = new double[state.size()];
 				 for (i = 0; i < state.size(); i++) {
 					 state_array[i] = state.get(i);
 				 }
 				 double delta_array[] = new double[delta.size()];
 				 for (i = 0; i < delta.size(); i++) {
 					 delta_array[i] = delta.get(i);
 				 }
 				 double state_plus_delta_array[] = new double[state_plus_delta.size()];
 				 for (i = 0; i < state_plus_delta.size(); i++) {
 					 state_plus_delta_array[i] = state_plus_delta.get(i);
 				 }
 				 cond = Plus(state_array, delta_array, state_plus_delta_array);
 				for (i = 0; i < state_plus_delta.size(); i++) {
 					state_plus_delta.set(i, state_plus_delta_array[i]);
				}
 				return cond;
 			 }

 			  public int NumEffectiveParameters() { return num_active_cols_; }
 			  public int NumParameters()          { return 4; }
 			  public int NumResiduals()           { return 4; }
 			};
 			
 			public void IsTrustRegionSolveSuccessful(boolean col1, boolean col2, boolean col3, boolean col4, 
 					TrustRegionStrategyType strategy_type, int failed[]) {
 				  int i;
 				  SolverOptions solver_options = new SolverOptions();
 				  LinearSolverOptions linear_solver_options = new LinearSolverOptions();
 				  DenseQRSolver linear_solver = new DenseQRSolver(linear_solver_options);

 				  double parameters[] = new double[]{ 3, -1, 0, 1.0 };

 				  // If the column is inactive, then set its value to the optimal
 				  // value.
 				  parameters[0] = (col1 ? parameters[0] : 0.0);
 				  parameters[1] = (col2 ? parameters[1] : 0.0);
 				  parameters[2] = (col3 ? parameters[2] : 0.0);
 				  parameters[3] = (col4 ? parameters[3] : 0.0);

 				  MinimizerOptions minimizer_options = new MinimizerOptions(solver_options);
 				  minimizer_options.gradient_tolerance = 1e-26;
 				  minimizer_options.function_tolerance = 1e-26;
 				  minimizer_options.parameter_tolerance = 1e-26;
 				  minimizer_options.evaluator =
 				      new PowellEvaluator2(col1, col2, col3, col4);
 				  minimizer_options.jacobian =
 				      minimizer_options.evaluator.CreateJacobian();

 				  TrustRegionStrategyOptions trust_region_strategy_options = new TrustRegionStrategyOptions();
 				  trust_region_strategy_options.trust_region_strategy_type = strategy_type;
 				  trust_region_strategy_options.linear_solver = linear_solver;
 				  trust_region_strategy_options.initial_radius = 1e4;
 				  trust_region_strategy_options.max_radius = 1e20;
 				  trust_region_strategy_options.min_lm_diagonal = 1e-6;
 				  //trust_region_strategy_options.max_lm_diagonal = 1e32;
 				  // Had to change max_lm_diagonal from 1e32 to 1e-6 for Dogleg to run successfully
				  trust_region_strategy_options.max_lm_diagonal = 1e-6;
 				  minimizer_options.trust_region_strategy =
 				      Create(trust_region_strategy_options);

 				  TrustRegionMinimizer minimizer = new TrustRegionMinimizer();
 				  SolverSummary summary = new SolverSummary();
 				  minimizer.Minimize(minimizer_options, parameters, summary);

 				  // The minimum is at x1 = x2 = x3 = x4 = 0.
 				  boolean currentPass = true;
 				  if (Math.abs(parameters[0]) > 0.001) {
 					  System.err.println("Math.abs(parameters[0]) > 0.001");
 					  failed[0]++;
 					  currentPass = false;
 				  }
 				  if (Math.abs(parameters[1]) > 0.001) {
					  System.err.println("Math.abs(parameters[1]) > 0.001");
					  failed[0]++;
					  currentPass = false;
				  }
 				  if (Math.abs(parameters[2]) > 0.001) {
					  System.err.println("Math.abs(parameters[2]) > 0.001");
					  failed[0]++;
					  currentPass = false;
				  }
 				 if (Math.abs(parameters[3]) > 0.001) {
					  System.err.println("Math.abs(parameters[3]) > 0.001");
					  failed[0]++;
					  currentPass = false;
				  }
 				 if (!currentPass) {
 					 System.err.println("col1 = " + col1 + " col2 = " + col2);
 					 System.err.println("col3 = " + col3 + " col4 = " + col4);
 					 for (i = 0; i < 4; i++) {
 						 System.err.println("parameters["+i+"] = " + parameters[i]);
 					 }
 				 }
 				}
 			
 			public void TrustRegionMinimizerPowellsSingularFunctionUsingLevenbergMarquardt() {
 				  // This case is excluded because this has a local minimum and does
 				  // not find the optimum. This should not affect the correctness of
 				  // this test since we are testing all the other 14 combinations of
 				  // column activations.
 				  //
 				  //   IsSolveSuccessful<true, true, false, true>();
 				  // I find that both true, true, true, true and true, true, false, true
 				  // are stuck at a local cost minimum of 107.5.
                  int failed[] = new int[] {0};
 				  final TrustRegionStrategyType kStrategy = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
 				  //IsTrustRegionSolveSuccessful(true,  true,  true,  true, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(true,  true,  true,  false, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(true,  false, true,  true, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(false, true,  true,  true, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(true,  true,  false, false, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(true,  false, true,  false, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(false, true,  true,  false, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(true,  false, false, true, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(false, true,  false, true, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(false, false, true,  true, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(true,  false, false, false, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(false, true,  false, false, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(false, false, true,  false, kStrategy, failed);
 				  IsTrustRegionSolveSuccessful(false, false, false, true, kStrategy, failed);
 				  if (failed[0] > 1) {
 					  System.err.println("TrustRegionMinimizerPowellsSingularFunctionUsingLevenbergMarquardt() had " + failed[0] + " failures");
 				  }
 				  else if (failed[0] == 1) {
 					 System.err.println("TrustRegionMinimizerPowellsSingularFunctionUsingLevenbergMarquardt() had 1 failure");
 				  }
 				  else {
 					  System.out.println("TrustRegionMinimizerPowellsSingularFunctionUsingLevenbergMarquardt() passed all tests");
 				  }
 				}

 			public void TrustRegionMinimizerPowellsSingularFunctionUsingDogleg() {
 				// TrustRegionMinimizerPowellsSingularFunctionUsingDogleg() passed all tests
 				  // The following two cases are excluded because they encounter a
 				  // local minimum.
 				  //
 				  //  IsTrustRegionSolveSuccessful<true, true, false, true >(kStrategy);
 				  //  IsTrustRegionSolveSuccessful<true,  true,  true,  true >(kStrategy);
 				int failed[] = new int[] {0};
 				final TrustRegionStrategyType kStrategy = TrustRegionStrategyType.DOGLEG;
 				IsTrustRegionSolveSuccessful(true,  true,  true,  false, kStrategy, failed);
				  IsTrustRegionSolveSuccessful(true,  false, true,  true, kStrategy, failed);
				  IsTrustRegionSolveSuccessful(false, true,  true,  true, kStrategy, failed);
				  IsTrustRegionSolveSuccessful(true,  true,  false, false, kStrategy, failed);
				  IsTrustRegionSolveSuccessful(true,  false, true,  false, kStrategy, failed);
				  IsTrustRegionSolveSuccessful(false, true,  true,  false, kStrategy, failed);
				  IsTrustRegionSolveSuccessful(true,  false, false, true, kStrategy, failed);
				  IsTrustRegionSolveSuccessful(false, true,  false, true, kStrategy, failed);
				  IsTrustRegionSolveSuccessful(false, false, true,  true, kStrategy, failed);
				  IsTrustRegionSolveSuccessful(true,  false, false, false, kStrategy, failed);
				  IsTrustRegionSolveSuccessful(false, true,  false, false, kStrategy, failed);
				  IsTrustRegionSolveSuccessful(false, false, true,  false, kStrategy, failed);
				  IsTrustRegionSolveSuccessful(false, false, false, true, kStrategy, failed);
				  if (failed[0] > 1) {
					  System.err.println("TrustRegionMinimizerPowellsSingularFunctionUsingDogleg() had " + failed[0] + " failures");
				  }
				  else if (failed[0] == 1) {
					 System.err.println("TrustRegionMinimizerPowellsSingularFunctionUsingDogleg() had 1 failure");
				  }
				  else {
					  System.out.println("TrustRegionMinimizerPowellsSingularFunctionUsingDogleg() passed all tests");
				  }
				}
 			
 			class CurveCostFunction extends CostFunction {
 				private int     num_vertices_;
 	 		    private double  target_length_;
 				 public CurveCostFunction(int num_vertices, double target_length) {
 					 super();
 				     num_vertices_ = num_vertices;
 				     target_length_ = target_length; 
 				    set_num_residuals(1);
 				    for (int i = 0; i < num_vertices_; ++i) {
 				      mutable_parameter_block_sizes().add(2);
 				    }
 				  }

 				public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
 				    residuals[0] = target_length_;

 				    for (int i = 0; i < num_vertices_; ++i) {
 				      int prev = (num_vertices_ + i - 1) % num_vertices_;
 				      double length = 0.0;
 				      for (int dim = 0; dim < 2; dim++) {
 				        final double diff = parameters.get(prev)[dim] - parameters.get(i)[dim];
 				        length += diff * diff;
 				      }
 				      residuals[0] -= Math.sqrt(length);
 				    }

 				    if (jacobians == null) {
 				      return true;
 				    }

 				    for (int i = 0; i < num_vertices_; ++i) {
 				      if (jacobians[i] != null) {
 				        int prev = (num_vertices_ + i - 1) % num_vertices_;
 				        int next = (i + 1) % num_vertices_;

 				        double u[] = new double[2];
 				        double v[] = new double[2];
 				        double norm_u = 0., norm_v = 0.;
 				        for (int dim = 0; dim < 2; dim++) {
 				          u[dim] = parameters.get(i)[dim] - parameters.get(prev)[dim];
 				          norm_u += u[dim] * u[dim];
 				          v[dim] = parameters.get(next)[dim] - parameters.get(i)[dim];
 				          norm_v += v[dim] * v[dim];
 				        }

 				        norm_u = Math.sqrt(norm_u);
 				        norm_v = Math.sqrt(norm_v);

 				        for (int dim = 0; dim < 2; dim++) {
 				          jacobians[i][dim] = 0.;

 				          if (norm_u > Double.MIN_VALUE) {
 				            jacobians[i][dim] -= u[dim] / norm_u;
 				          }

 				          if (norm_v > Double.MIN_VALUE) {
 				            jacobians[i][dim] += v[dim] / norm_v;
 				          }
 				        }
 				      }
 				    }

 				    return true;
 				  }
 				
 				public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int[] jacobians_offset) {
 				    residuals[0] = target_length_;

 				    for (int i = 0; i < num_vertices_; ++i) {
 				      int prev = (num_vertices_ + i - 1) % num_vertices_;
 				      double length = 0.0;
 				      for (int dim = 0; dim < 2; dim++) {
 				        final double diff = parameters.get(prev)[dim] - parameters.get(i)[dim];
 				        length += diff * diff;
 				      }
 				      residuals[0] -= Math.sqrt(length);
 				    }

 				    if (jacobians == null) {
 				      return true;
 				    }

 				    for (int i = 0; i < num_vertices_; ++i) {
 				      if (jacobians[i] != null) {
 				        int prev = (num_vertices_ + i - 1) % num_vertices_;
 				        int next = (i + 1) % num_vertices_;

 				        double u[] = new double[2];
 				        double v[] = new double[2];
 				        double norm_u = 0., norm_v = 0.;
 				        for (int dim = 0; dim < 2; dim++) {
 				          u[dim] = parameters.get(i)[dim] - parameters.get(prev)[dim];
 				          norm_u += u[dim] * u[dim];
 				          v[dim] = parameters.get(next)[dim] - parameters.get(i)[dim];
 				          norm_v += v[dim] * v[dim];
 				        }

 				        norm_u = Math.sqrt(norm_u);
 				        norm_v = Math.sqrt(norm_v);

 				        for (int dim = 0; dim < 2; dim++) {
 				          jacobians[i][jacobians_offset[i] + dim] = 0.;

 				          if (norm_u > Double.MIN_VALUE) {
 				            jacobians[i][jacobians_offset[i] + dim] -= u[dim] / norm_u;
 				          }

 				          if (norm_v > Double.MIN_VALUE) {
 				            jacobians[i][jacobians_offset[i] + dim] += v[dim] / norm_v;
 				          }
 				        }
 				      }
 				    }

 				    return true;
 				  }


 				 
 				};
 				
 				public void TrustRegionMinimizerJacobiScalingTest() {
 					  // TrustRegionMinimizerJacobiScalingTest() passed all tests
 					  int N = 6;
 					  Vector<double[]> y = new Vector<double[]>(N);
 					  for (int i = 0; i < N; i++) {
 					    double theta = i * 2. * Math.PI/ (double)(N);
 					    double yarr[] = new double[2];
 					    yarr[0] = Math.cos(theta);
 					    yarr[1] = Math.sin(theta);
 					    y.add(yarr);
 					  }

 					  ProblemImpl problem = new ProblemImpl();
 					  problem.AddResidualBlock(new CurveCostFunction(N, 10.), null, y);
 					  SolverOptions options = new SolverOptions();
 					  options.linear_solver_type = LinearSolverType.DENSE_QR;
 					  options.max_num_consecutive_invalid_steps = 15;
 					  options.function_tolerance = 1.0E-10;
 					  options.parameter_tolerance = 1.0E-10;
 					  
 					  SolverSummary summary = new SolverSummary();
 					  Solve(options, problem, summary);
 					  if (summary.final_cost > 1e-10) {
 						  System.err.println("In TrustRegionMinimizerJacobiScalingTest() summary.final_cost > 1e-10");
 						  System.err.println("summary.final_cost = " + summary.final_cost);
 					  }
 					  else {
 						  System.out.println("TrustRegionMinimizerJacobiScalingTest() passed all tests");
 					  }

 					  for (int i = 0; i < N; i++) {
 					     y.set(i, null);
 					  }
 					}
 				
		class ExpCostFunction extends SizedCostFunction {
			public ExpCostFunction() {
				// number of resdiuals
				// size of first parameter
				super(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0);
			}

			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);

				// f(x) = 10 - exp(x).
				residuals[0] = 10 - Math.exp(x[0]);

				// f'(x) = -Math.exp(x[0]). Since there's only 1 parameter and that parameter
				// has 1 dimension, there is only 1 element to fill in the
				// jacobians.
				//
				// Since the Evaluate function can be called with the jacobians
				// pointer equal to NULL, the Evaluate function must check to see
				// if jacobians need to be computed.
				//
				// For this simple problem it is overkill to check if jacobians[0]
				// is NULL, but in general when writing more complex
				// CostFunctions, it is possible that Ceres may only demand the
				// derivatives w.r.t. a subset of the parameter blocks.
				if (jacobians != null && jacobians[0] != null) {
					jacobians[0][0] = -Math.exp(x[0]);
				}

				return true;
			}
			
			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);

				// f(x) = 10 - exp(x).
				residuals[0] = 10 - Math.exp(x[0]);

				// f'(x) = -Math.exp(x[0]). Since there's only 1 parameter and that parameter
				// has 1 dimension, there is only 1 element to fill in the
				// jacobians.
				//
				// Since the Evaluate function can be called with the jacobians
				// pointer equal to NULL, the Evaluate function must check to see
				// if jacobians need to be computed.
				//
				// For this simple problem it is overkill to check if jacobians[0]
				// is NULL, but in general when writing more complex
				// CostFunctions, it is possible that Ceres may only demand the
				// derivatives w.r.t. a subset of the parameter blocks.
				if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]] = -Math.exp(x[0]);
				}

				return true;
			}
		};
 				
		public void TrustRegionMinimizerGradientToleranceConvergenceUpdatesStep() {
			  // TrustRegionMinimizerGradientToleranceConvergenceUpdatesStep() passed all tests
			  boolean passed = true;
			  double x[] = new double[] {5};
			  ProblemImpl problem = new ProblemImpl();
			  problem.AddResidualBlock(new ExpCostFunction(), null, x);
			  problem.SetParameterLowerBound(x, 0, 3.0);
			  SolverOptions options = new SolverOptions();
			  SolverSummary summary = new SolverSummary();
			  Solve(options, problem, summary);
			  if (Math.abs(x[0] - 3.0) > 1.0E-12) {
				  System.err.println("In TrustRegionMinimizerGradientToleranceConvergenceUpdatesStep() (Math.abs(x[0] - 3.0) > 1.0E-12");
				  System.err.println("x[0] = " + x[0]);
				  passed = false;
			  }
			  final double expected_final_cost = 0.5 * Math.pow(10.0 - Math.exp(3.0), 2);
			  if (Math.abs(expected_final_cost - summary.final_cost) > 1e-12) {
				  System.err.println("In TrustRegionMinimizerGradientToleranceConvergenceUpdatesStep() abs(expected_final_cost - summary.final_cost)) > 1.0E-12"); 
				  passed = false;
			  }
			  if (passed) {
				  System.out.println("TrustRegionMinimizerGradientToleranceConvergenceUpdatesStep() passed all tests");
			  }
		}
		
		public void QuaternionParameterizationTestHelper(
			    double[] x, double[] delta,
			    double[] x_plus_delta_ref, String testName) {
			  int i, j;
			  boolean passed = true;
			  final int kGlobalSize = 4;
			  final int kLocalSize = 3;

			  final double kTolerance = 1e-14;

			  double x_plus_delta[] = new double[]{0.0, 0.0, 0.0, 0.0};
			  QuaternionParameterization parameterization = new QuaternionParameterization();
			  parameterization.Plus(x, delta, x_plus_delta);
			  for (i = 0; i < kGlobalSize; ++i) {
			    if (Math.abs(x_plus_delta_ref[i] - x_plus_delta[i]) > kTolerance) {
			    	System.err.println("In " + testName + " Math.abs(x_plus_delta_ref["+i+"] - x_plus_delta["+i+"]) > kTolerance");
			    	passed = false;
			    }
			  }

			  final double x_plus_delta_norm =
			      Math.sqrt(x_plus_delta[0] * x_plus_delta[0] +
			           x_plus_delta[1] * x_plus_delta[1] +
			           x_plus_delta[2] * x_plus_delta[2] +
			           x_plus_delta[3] * x_plus_delta[3]);

			  if (Math.abs(x_plus_delta_norm - 1.0) > kTolerance) {
				  System.err.println("In " + testName + " Math.abs(x_plus_delta_norm - 1.0) > kTolerance");
				  passed = false;
			  }

			  double jacobian_ref[][] = new double[4][3];
			  jacobian_ref[0][0] = -x[1]; jacobian_ref[0][1]  = -x[2]; jacobian_ref[0][2]  = -x[3];
			  jacobian_ref[1][0] =  x[0]; jacobian_ref[1][1]  =  x[3]; jacobian_ref[1][2]  = -x[2];
			  jacobian_ref[2][0] = -x[3]; jacobian_ref[2][1]  =  x[0]; jacobian_ref[2][2]  =  x[1];
			  jacobian_ref[3][0] =  x[2]; jacobian_ref[3][1] = -x[1]; jacobian_ref[3][2] =  x[0];
			  

			  double jacobian[][] = new double[4][3];
			  parameterization.ComputeJacobian(x, 0, jacobian);
			  for (i = 0; i < 4; ++i) {
				  for (j = 0; j < 3; j++) {
			    if (!Double.isFinite(jacobian[i][j])) {
			    	System.err.println("In " + testName + " Double.isFinite(jacobian["+i+"]["+j+"]) = false");
			    	passed = false;
			    }
			    if (Math.abs(jacobian[i][j] - jacobian_ref[i][j]) > kTolerance) {
			        System.err.println("Jacobian mismatch: i = " + i + " j = " + j);
			        System.err.println("Expected jacobian = " + jacobian_ref[i][j]);
			        System.err.println("Actual jacobian = " + jacobian[i][j]);
				 }
				}
			  }

			  Matrix global_matrix = Matrix.random(10, kGlobalSize);
			  Matrix local_matrix = new Matrix(10, kLocalSize, 0.0);
			  double global_matrix_array[][] = global_matrix.getArray();
			  double local_matrix_array[][] = local_matrix.getArray();
			  parameterization.MultiplyByJacobian(x,
			                                      10,
			                                      global_matrix_array,
			                                      local_matrix_array);
			  Matrix jacobianMat = new Matrix(jacobian);
			  Matrix expected_local_matrix =
			      global_matrix.times(jacobianMat);
			  double expected_local_matrix_array[][] = expected_local_matrix.getArray();
			  double diff;
			  double normSquared = 0.0;
			  for (i = 0; i < 10; i++) {
				  for (j = 0; j < kLocalSize; j++) {
					  diff = local_matrix_array[i][j] - expected_local_matrix_array[i][j];
					  normSquared += (diff * diff);
				  }
			  }
			  double norm = Math.sqrt(normSquared);
			  if (norm > 10.0 * epsilon) {
				  System.err.println("In " + testName + " (local_matrix - expected_local_matrix).norm() > 10.0 * epsilon");
				  System.err.println("norm = " + norm);
				  passed = false;
			  }
			  if (passed) {
				  System.out.println(testName + " passed all tests");
			  }
			}

		public void QuaternionParameterizationZeroTest() {
			  // QuaternionParameterizationZeroTest() passed all tests
			  double x[] = new double[]{0.5, 0.5, 0.5, 0.5};
			  double delta[] = new double[]{0.0, 0.0, 0.0};
			  double q_delta[] = new double[]{1.0, 0.0, 0.0, 0.0};
			  double x_plus_delta[] = new double[]{0.0, 0.0, 0.0, 0.0};
			  QuaternionProduct(q_delta, x, x_plus_delta);
			  QuaternionParameterizationTestHelper(x, delta, x_plus_delta,"QuaternionParameterizationZeroTest()");
		}

		
		public void QuaternionParameterizationNearZeroTest() {
			  // QuaternionParameterizationNearZeroTest() passed all tests
			  int i;
			  double x[] = new double[]{0.52, 0.25, 0.15, 0.45};
			  double normSquared = 0.0;
			  for (i = 0; i < 4; i++) {
				  normSquared += (x[i]*x[i]);
			  }
			  double norm = Math.sqrt(normSquared);
			  for (i = 0; i < 4; i++) {
				  x[i] = x[i]/norm;
			  }

			  double delta[] = new double[]{0.24, 0.15, 0.10};
			  for (i = 0; i < 3; ++i) {
			    delta[i] = delta[i] * 1e-14;
			  }

			  double q_delta[] = new double[4];
			  q_delta[0] = 1.0;
			  q_delta[1] = delta[0];
			  q_delta[2] = delta[1];
			  q_delta[3] = delta[2];

			  double x_plus_delta[] = new double[]{0.0, 0.0, 0.0, 0.0};
			  QuaternionProduct(q_delta, x, x_plus_delta);
			  QuaternionParameterizationTestHelper(x, delta, x_plus_delta, "QuaternionParameterizationNearZeroTest()");
			}

		public void QuaternionParameterizationAwayFromZeroTest() {
			  // QuaternionParameterizationAwayFromZeroTest() passed all tests
			  int i;
			  double x[] = new double[]{0.52, 0.25, 0.15, 0.45};
			  double normSquared = 0.0;
			  for (i = 0; i < 4; i++) {
				  normSquared += (x[i]*x[i]);
			  }
			  double norm = Math.sqrt(normSquared);
			  for (i = 0; i < 4; i++) {
				  x[i] = x[i]/norm;
			  }

			  double delta[] = new double[]{0.24, 0.15, 0.10};
			  final double delta_norm = Math.sqrt(delta[0] * delta[0] +
			                                 delta[1] * delta[1] +
			                                 delta[2] * delta[2]);
			  double q_delta[] = new double[4];
			  q_delta[0] = Math.cos(delta_norm);
			  q_delta[1] = Math.sin(delta_norm) / delta_norm * delta[0];
			  q_delta[2] = Math.sin(delta_norm) / delta_norm * delta[1];
			  q_delta[3] = Math.sin(delta_norm) / delta_norm * delta[2];

			  double x_plus_delta[] = new double[]{0.0, 0.0, 0.0, 0.0};
			  QuaternionProduct(q_delta, x, x_plus_delta);
			  QuaternionParameterizationTestHelper(x, delta, x_plus_delta, "QuaternionParameterizationAwayFromZeroTest()");
			}
	
		public void EigenQuaternionParameterizationTestHelper(
			    double[] x, double[] delta,
			    double[] x_plus_delta_ref, String testName) {
			  int i, j;
			  boolean passed = true;
			  final int kGlobalSize = 4;
			  final int kLocalSize = 3;

			  final double kTolerance = 1e-14;

			  double x_plus_delta[] = new double[]{0.0, 0.0, 0.0, 0.0};
			  EigenQuaternionParameterization parameterization = new EigenQuaternionParameterization();
			  parameterization.Plus(x, delta, x_plus_delta);
			  for (i = 0; i < kGlobalSize; ++i) {
			    if (Math.abs(x_plus_delta_ref[i] - x_plus_delta[i]) > kTolerance) {
			    	System.err.println("In " + testName + " Math.abs(x_plus_delta_ref["+i+"] - x_plus_delta["+i+"]) > kTolerance");
			    	passed = false;
			    }
			  }

			  final double x_plus_delta_norm =
			      Math.sqrt(x_plus_delta[0] * x_plus_delta[0] +
			           x_plus_delta[1] * x_plus_delta[1] +
			           x_plus_delta[2] * x_plus_delta[2] +
			           x_plus_delta[3] * x_plus_delta[3]);

			  if (Math.abs(x_plus_delta_norm - 1.0) > kTolerance) {
				  System.err.println("In " + testName + " Math.abs(x_plus_delta_norm - 1.0) > kTolerance");
				  passed = false;
			  }

			  double jacobian_ref[][] = new double[4][3];
			  jacobian_ref[0][0] = x[3]; jacobian_ref[0][1]  = x[2]; jacobian_ref[0][2]  = -x[1];
			  jacobian_ref[1][0] =  -x[2]; jacobian_ref[1][1]  =  x[3]; jacobian_ref[1][2]  = x[0];
			  jacobian_ref[2][0] = x[1]; jacobian_ref[2][1]  =  -x[0]; jacobian_ref[2][2]  =  x[3];
			  jacobian_ref[3][0] =  -x[0]; jacobian_ref[3][1] = -x[1]; jacobian_ref[3][2] =  -x[2];
			  

			  double jacobian[][] = new double[4][3];
			  parameterization.ComputeJacobian(x, 0, jacobian);
			  for (i = 0; i < 4; ++i) {
				  for (j = 0; j < 3; j++) {
			    if (!Double.isFinite(jacobian[i][j])) {
			    	System.err.println("In " + testName + " Double.isFinite(jacobian["+i+"]["+j+"]) = false");
			    	passed = false;
			    }
			    if (Math.abs(jacobian[i][j] - jacobian_ref[i][j]) > kTolerance) {
			        System.err.println("Jacobian mismatch: i = " + i + " j = " + j);
			        System.err.println("Expected jacobian = " + jacobian_ref[i][j]);
			        System.err.println("Actual jacobian = " + jacobian[i][j]);
				 }
				}
			  }

			  Matrix global_matrix = Matrix.random(10, kGlobalSize);
			  Matrix local_matrix = new Matrix(10, kLocalSize, 0.0);
			  double global_matrix_array[][] = global_matrix.getArray();
			  double local_matrix_array[][] = local_matrix.getArray();
			  parameterization.MultiplyByJacobian(x,
			                                      10,
			                                      global_matrix_array,
			                                      local_matrix_array);
			  Matrix jacobianMat = new Matrix(jacobian);
			  Matrix expected_local_matrix =
			      global_matrix.times(jacobianMat);
			  double expected_local_matrix_array[][] = expected_local_matrix.getArray();
			  double diff;
			  double normSquared = 0.0;
			  for (i = 0; i < 10; i++) {
				  for (j = 0; j < kLocalSize; j++) {
					  diff = local_matrix_array[i][j] - expected_local_matrix_array[i][j];
					  normSquared += (diff * diff);
				  }
			  }
			  double norm = Math.sqrt(normSquared);
			  if (norm > 10.0 * epsilon) {
				  System.err.println("In " + testName + " (local_matrix - expected_local_matrix).norm() > 10.0 * epsilon");
				  System.err.println("norm = " + norm);
				  passed = false;
			  }
			  if (passed) {
				  System.out.println(testName + " passed all tests");
			  }
			}
		
		public void EigenQuaternionParameterizationZeroTest() {
			// EigenQuaternionParameterizationZeroTest() passed all tests
			  double x[] = new double[] {0.5, 0.5, 0.5, 0.5};
			  double x_eig[] = new double[] {0.5, 0.5, 0.5, 0.5};
			  double delta[] = new double[]{0.0, 0.0, 0.0};
			  double q_delta[] = new double[] {1.0, 0.0, 0.0, 0.0};
			  double q_delta_eig[] = new double[] {0.0, 0.0, 0.0, 1.0};
			  double x_plus_delta_eig[] = new double[4];
			  // Eigen::Quaterniond x_plus_delta = q_delta * x;
			  // EigenQuaternionProduct has the real parts in the last position
			  EigenQuaternionProduct(q_delta_eig, x_eig, x_plus_delta_eig);
			  double x_plus_delta[] =  new double[] {x_plus_delta_eig[3], x_plus_delta_eig[0], x_plus_delta_eig[1], x_plus_delta_eig[2]}; 
			  EigenQuaternionParameterizationTestHelper(x, delta, x_plus_delta, "EigenQuaternionParameterizationZeroTest()");
		}
		
		public void EigenQuaternionParameterizationNearZeroTest() {
			  // EigenQuaternionParameterizationNearZeroTest() passed all tests
			  int i;
			  //Eigen::Quaterniond x(0.52, 0.25, 0.15, 0.45);
			  double x[] = new double[] {0.52, 0.25, 0.15, 0.45};
			  double normSquared = 0.0;
			  for (i = 0; i < 4; i++) {
				  normSquared += (x[i]*x[i]);
			  }
			  double norm = Math.sqrt(normSquared);
			  for (i = 0; i < 4; i++) {
				  x[i] = x[i]/norm;
			  }
			  double x_eig[] = new double[] {x[1],x[2],x[3],x[0]};

			  double delta[] = new double[] {0.24, 0.15, 0.10};
			  for (i = 0; i < 3; ++i) {
			    delta[i] = delta[i] * 1e-14;
			  }

			  // Note: w is first in the constructor.
			  //Eigen::Quaterniond q_delta(1.0, delta[0], delta[1], delta[2]);
			  double q_delta_eig[] = new double[] {delta[0], delta[1], delta[2], 1.0};
			  // Eigen::Quaterniond x_plus_delta = q_delta * x;
			  // EigenQuaternionProduct has the real parts in the last position
			  double x_plus_delta_eig[] = new double[4];
			  EigenQuaternionProduct(q_delta_eig, x_eig, x_plus_delta_eig);
			  double x_plus_delta[] =  new double[] {x_plus_delta_eig[3], x_plus_delta_eig[0], x_plus_delta_eig[1], x_plus_delta_eig[2]}; 
			  EigenQuaternionParameterizationTestHelper(x, delta, x_plus_delta, "EigenQuaternionParameterizationNearZeroTest()");
			}
		
		public void EigenQuaternionParameterizationAwayFromZeroTest() {
			  // EigenQuaternionParameterizationAwayFromZeroTest() passed all tests
			  int i;
			  //Eigen::Quaterniond x(0.52, 0.25, 0.15, 0.45);
			  double x[] = new double[] {0.52, 0.25, 0.15, 0.45};
			  double normSquared = 0.0;
			  for (i = 0; i < 4; i++) {
				  normSquared += (x[i]*x[i]);
			  }
			  double norm = Math.sqrt(normSquared);
			  for (i = 0; i < 4; i++) {
				  x[i] = x[i]/norm;
			  }
			  double x_eig[] = new double[] {x[1],x[2],x[3],x[0]};

			  double delta[] = new double[] {0.24, 0.15, 0.10};
			  final double delta_norm = Math.sqrt(delta[0] * delta[0] +
			                                 delta[1] * delta[1] +
			                                 delta[2] * delta[2]);

			  // Note: w is first in the constructor.
			  //Eigen::Quaterniond q_delta(cos(delta_norm),
			                             //sin(delta_norm) / delta_norm * delta[0],
			                             //sin(delta_norm) / delta_norm * delta[1],
			                             //sin(delta_norm) / delta_norm * delta[2]);
			  // Here w is last
			  double q_delta_eig[] = new double[] {Math.sin(delta_norm) / delta_norm * delta[0],
                      Math.sin(delta_norm) / delta_norm * delta[1],
                      Math.sin(delta_norm) / delta_norm * delta[2],
                      Math.cos(delta_norm)};
			  
			  // Eigen::Quaterniond x_plus_delta = q_delta * x;
			  // EigenQuaternionProduct has the real parts in the last position
			  double x_plus_delta_eig[] = new double[4];
			  EigenQuaternionProduct(q_delta_eig, x_eig, x_plus_delta_eig);
			  double x_plus_delta[] =  new double[] {x_plus_delta_eig[3], x_plus_delta_eig[0], x_plus_delta_eig[1], x_plus_delta_eig[2]}; 
			  EigenQuaternionParameterizationTestHelper(x, delta, x_plus_delta, "EigenQuaternionParameterizationAwayFromZeroTest()");
			}
		
		public void HouseholderTestHelper(double x[], String testName) {
			  int i;
			  boolean passed = true;
			  final double kTolerance = 1e-14;

			  // Check to ensure that H * x = ||x|| * [0 ... 0 1]'.
			  double v[] = new double[x.length];
			  double beta[] = new double[1];
			  ComputeHouseholderVector(x, v, beta);
			  double vx = 0.0;
			  double normSquared = 0.0;
			  for (i = 0; i < x.length; i++) {
				   vx += (v[i] * x[i]);
				   normSquared += (x[i] * x[i]);
			  }
			  double xnorm = Math.sqrt(normSquared);
			  double result[] = new double[x.length];
			  for (i = 0; i < x.length; i++) {
				  result[i] = x[i] - beta[0] * v[i] * vx;
			  }

			  double expected_result[] = new double[x.length];
			  expected_result[x.length - 1] = xnorm;

			  for (i = 0; i < x.length; ++i) {
			       if (Math.abs(expected_result[i] - result[i]) > kTolerance) {
			           System.err.println("In " + testName + " expected_result["+i+"] - result["+i+"]) > kTolerance");
			           passed = false;
			       }
			  }
			  if (passed) {
				  System.out.println(testName + " passed all tests");
			  }
			}

		public void HouseholderVectorZeroPositive() {
			  // HouseholderVectorZeroPositive() passed all tests
			  double x[] = new double[] {0.0, 0.0, 0.25};
			  HouseholderTestHelper(x, "HouseholderVectorZeroPositive()");
		}
		
		public void HouseholderVectorZeroNegative() {
			  // HouseholderVectorZeroNegative() passed all tests
			  double x[] = new double[] {0.0, 0.0, -0.25};
			  HouseholderTestHelper(x, "HouseholderVectorZeroNegative()");
		}
		
		public void HouseholderVectorNearZeroPositive() {
			  // HouseholderVectorNearZeroPositive() passed all tests
			  double x[] = new double[] {1E-18, 1E-18, 0.25};
			  HouseholderTestHelper(x, "HouseholderVectorNearZeroPositive()");
		}
		
		public void HouseholderVectorNearZeroNegative() {
			  // HouseholderVectorNearZeroNegative() passed all tests
			  double x[] = new double[] {1E-18, 1E-18, -0.25};
			  HouseholderTestHelper(x, "HouseholderVectorNearZeroNegative()");
		}
		
		public void HouseholderVectorNonZeroNegative() {
			  // HouseholderVectorNonZeroNegative() passed all tests
			  double x[] = new double[] {1.0, 0.0, -3.0};
			  HouseholderTestHelper(x, "HouseholderVectorNonZeroNegative()");
		}
		
		public void HouseholderVectorNonZeroPositive() {
			  // HouseholderVectorNonZeroPositive() passed all tests
			  double x[] = new double[] {1.0, 1.0, 1.0};
			  HouseholderTestHelper(x, "HouseholderVectorNonZeroPositive()");
		}
		
		public void HouseholderVectorNonZeroPositive_Size4() {
			  // HouseholderVectorNonZeroPositive_Size4() passed all tests
			  double x[] = new double[] {1.0, 1.0, 0.0, 2.0};
			  HouseholderTestHelper(x, "HouseholderVectorNonZeroPositive_Size4()");
		}
		
		public void HouseholderVectorLastElementZero() {
			  // HouseholderVectorLastElementZero() passed all tests
			  double x[] = new double[] {1.0, 1.0, 0.0, 0.0};
			  HouseholderTestHelper(x, "HouseholderVectorLastElementZero()");
		}
		
		public void HomogeneousVectorParameterizationHelper(double[] x,
                double[] delta, String testName) {
	    boolean passed = true;
		final double kTolerance = 1e-14;
		
		HomogeneousVectorParameterization homogeneous_vector_parameterization = new HomogeneousVectorParameterization(4);
		
		// Ensure the update maintains the norm.
		double x_plus_delta[] = new double[]{0.0, 0.0, 0.0, 0.0};
		homogeneous_vector_parameterization.Plus(x, delta, x_plus_delta);
		
		final double x_plus_delta_norm =
		Math.sqrt(x_plus_delta[0] * x_plus_delta[0] +
		x_plus_delta[1] * x_plus_delta[1] +
		x_plus_delta[2] * x_plus_delta[2] +
		x_plus_delta[3] * x_plus_delta[3]);
		
		final double x_norm = Math.sqrt(x[0] * x[0] + x[1] * x[1] +
		x[2] * x[2] + x[3] * x[3]);
		
		if (Math.abs(x_plus_delta_norm - x_norm) > kTolerance) {
		    System.err.println("In " + testName + " Math.abs(x_plus_delta_norm - x_norm) > kTolerance");
		    passed = false;
		}
		
		
		double jacobian_analytic[][] = new double[4][3];
		
		homogeneous_vector_parameterization.ComputeJacobian(x, 0, jacobian_analytic);
		
		for (int i = 0; i < 4; ++i) {
			for (int j = 0; j < 3; ++j) {
				if (!Double.isFinite(jacobian_analytic[i][j])) {
					System.err.println("In " + testName + " jacobian_analytic["+i+"]["+j+"] is not finite");
					passed = false;
				}
			}
		}
		if (passed) {
			System.out.println(testName + " passed all tests");
		}
    }
		
		public void HomogeneousVectorParameterizationZeroTest() {
			// HomogeneousVectorParameterizationZeroTest() passed all tests
			  int i;
			  double normSquared;
			  double norm;
			  double x[] = new double[] {0.0, 0.0, 0.0, 1.0};
			  normSquared = 0.0;
			  for (i = 0; i < 4; i++) {
				  normSquared += (x[i] * x[i]);
			  }
			  norm = Math.sqrt(normSquared);
			  for (i = 0; i < 4; i++) {
				  x[i] /= norm;
			  }
			  double delta[] = new double[]{0.0, 0.0, 0.0};

			  HomogeneousVectorParameterizationHelper(x, delta, "HomogeneousVectorParameterizationZeroTest()");
			}
		
		public void HomogeneousVectorParameterizationNearZeroTest1() {
			// HomogeneousVectorParameterizationNearZeroTest1() passed all tests
			  int i;
			  double normSquared;
			  double norm;
			  double x[] = new double[] {1E-5, 1E-5, 1E-5, 1.0};
			  normSquared = 0.0;
			  for (i = 0; i < 4; i++) {
				  normSquared += (x[i] * x[i]);
			  }
			  norm = Math.sqrt(normSquared);
			  for (i = 0; i < 4; i++) {
				  x[i] /= norm;
			  }
			  double delta[] = new double[]{0.0, 1.0, 0.0};

			  HomogeneousVectorParameterizationHelper(x, delta, "HomogeneousVectorParameterizationNearZeroTest1()");
			}

		public void HomogeneousVectorParameterizationNearZeroTest2() {
			// HomogeneousVectorParameterizationNearZeroTest2() passed all tests
			  int i;
			  double normSquared;
			  double norm;
			  double x[] = new double[] {0.001, 0.0, 0.0, 0.0};
			  normSquared = 0.0;
			  for (i = 0; i < 4; i++) {
				  normSquared += (x[i] * x[i]);
			  }
			  norm = Math.sqrt(normSquared);
			  for (i = 0; i < 4; i++) {
				  x[i] /= norm;
			  }
			  double delta[] = new double[]{0.0, 1.0, 0.0};

			  HomogeneousVectorParameterizationHelper(x, delta, "HomogeneousVectorParameterizationNearZeroTest2()");
			}
		
		public void HomogeneousVectorParameterizationAwayFromZeroTest1() {
			// HomogeneousVectorParameterizationAwayFromZeroTest1() passed all tests
			  int i;
			  double normSquared;
			  double norm;
			  double x[] = new double[] {0.52, 0.25, 0.15, 0.45};
			  normSquared = 0.0;
			  for (i = 0; i < 4; i++) {
				  normSquared += (x[i] * x[i]);
			  }
			  norm = Math.sqrt(normSquared);
			  for (i = 0; i < 4; i++) {
				  x[i] /= norm;
			  }
			  double delta[] = new double[] {0.0, 1.0, -0.5};

			  HomogeneousVectorParameterizationHelper(x, delta, "HomogeneousVectorParameterizationAwayFromZeroTest1()");
			}
		
		public void HomogeneousVectorParameterizationAwayFromZeroTest2() {
			// HomogeneousVectorParameterizationAwayFromZeroTest2() passed all tests
			  int i;
			  double normSquared;
			  double norm;
			  double x[] = new double[] {0.87, -0.25, -0.34, 0.45};
			  normSquared = 0.0;
			  for (i = 0; i < 4; i++) {
				  normSquared += (x[i] * x[i]);
			  }
			  norm = Math.sqrt(normSquared);
			  for (i = 0; i < 4; i++) {
				  x[i] /= norm;
			  }
			  double delta[] = new double[] {0.0, 0.0, -0.5};

			  HomogeneousVectorParameterizationHelper(x, delta, "HomogeneousVectorParameterizationAwayFromZeroTest2()");
			}
		
		public void HomogeneousVectorParameterizationAwayFromZeroTest3() {
			// HomogeneousVectorParameterizationAwayFromZeroTest3() passed all tests
			  int i;
			  double normSquared;
			  double norm;
			  double x[] = new double[] {0.0, 0.0, 0.0, 2.0};
			  normSquared = 0.0;
			  for (i = 0; i < 4; i++) {
				  normSquared += (x[i] * x[i]);
			  }
			  norm = Math.sqrt(normSquared);
			  for (i = 0; i < 4; i++) {
				  x[i] /= norm;
			  }
			  double delta[] = new double[] {0.0, 0.0, 0};

			  HomogeneousVectorParameterizationHelper(x, delta, "HomogeneousVectorParameterizationAwayFromZeroTest3()");
			}
		
		public void HomogeneousVectorParameterizationAwayFromZeroTest4() {
			// HomogeneousVectorParameterizationAwayFromZeroTest4() passed all tests
			  int i;
			  double normSquared;
			  double norm;
			  double x[] = new double[] {0.2, -1.0, 0.0, 2.0};
			  normSquared = 0.0;
			  for (i = 0; i < 4; i++) {
				  normSquared += (x[i] * x[i]);
			  }
			  norm = Math.sqrt(normSquared);
			  for (i = 0; i < 4; i++) {
				  x[i] /= norm;
			  }
			  double delta[] = new double[] {1.4, 0.0, -0.5};

			  HomogeneousVectorParameterizationHelper(x, delta, "HomogeneousVectorParameterizationAwayFromZeroTest4()");
			}
		
		public void HomogeneousVectorParameterizationAwayFromZeroTest5() {
			// HomogeneousVectorParameterizationAwayFromZeroTest5() passed all tests
			  int i;
			  double normSquared;
			  double norm;
			  double x[] = new double[] {2.0, 0.0, 0.0, 0.0};
			  normSquared = 0.0;
			  for (i = 0; i < 4; i++) {
				  normSquared += (x[i] * x[i]);
			  }
			  norm = Math.sqrt(normSquared);
			  for (i = 0; i < 4; i++) {
				  x[i] /= norm;
			  }
			  double delta[] = new double[] {1.4, 0.0, -0.5};

			  HomogeneousVectorParameterizationHelper(x, delta, "HomogeneousVectorParameterizationAwayFromZeroTest5()");
			}
		
		public void ProductParameterizationTestLocalAndGlobalSize2() {
			// ProductParameterizationTestLocalAndGlobalSize2() passed all tests
			ProductParameterizationTest PT = new ProductParameterizationTest();
			PT.ProductParameterizationTestLocalAndGlobalSize2();
		}
		
		public void ProductParameterizationTestLocalAndGlobalSize3() {
			// ProductParameterizationTestLocalAndGlobalSize3() passed all tests
			ProductParameterizationTest PT = new ProductParameterizationTest();
			PT.ProductParameterizationTestLocalAndGlobalSize3();
		}
		
		public void ProductParameterizationTestLocalAndGlobalSize4() {
			// ProductParameterizationTestLocalAndGlobalSize4() passed all tests
			ProductParameterizationTest PT = new ProductParameterizationTest();
			PT.ProductParameterizationTestLocalAndGlobalSize4();
		}
		
		public void ProductParameterizationTestPlus() {
			// ProductParameterizationTestPlus() passed all tests
			ProductParameterizationTest PT = new ProductParameterizationTest();
			PT.ProductParameterizationTestPlus();
		}
		
		 public void ProductParameterizationTestComputeJacobian() {
			 // ProductParameterizationTestComputeJacobian() passed all tests
			 ProductParameterizationTest PT = new ProductParameterizationTest();
			 PT.ProductParameterizationTestComputeJacobian();
		 }
		
		class ProductParameterizationTest {
			private LocalParameterization param1_;
			private LocalParameterization param2_;
			private LocalParameterization param3_;
			private LocalParameterization param4_;
			boolean passed;
			 public ProductParameterizationTest() {
			     SetUp();	 
			 }
			 public void SetUp() {
			    final int global_size1 = 5;
			    Vector<Integer> constant_parameters1 = new Vector<Integer>();
			    constant_parameters1.add(2);
			    param1_ = new SubsetParameterization(global_size1,
			                                             constant_parameters1);

			    final int global_size2 = 3;
			    Vector<Integer> constant_parameters2 = new Vector<Integer>();
			    constant_parameters2.add(0);
			    constant_parameters2.add(1);
			    param2_ = new SubsetParameterization(global_size2,
			                                             constant_parameters2);

			    final int global_size3 = 4;
			    Vector<Integer> constant_parameters3 = new Vector<Integer>();
			    constant_parameters3.add(1);
			    param3_ = new SubsetParameterization(global_size3,
			                                             constant_parameters3);

			    final int global_size4 = 2;
			    Vector<Integer> constant_parameters4 = new Vector<Integer>();
			    constant_parameters4.add(1);
			    param4_ = new SubsetParameterization(global_size4,
			                                             constant_parameters4);
			  }
			 
			 public void ProductParameterizationTestLocalAndGlobalSize2() {
				  passed = true;
				  LocalParameterization param1 = param1_;
				  LocalParameterization param2 = param2_;

				  ProductParameterization product_param = new ProductParameterization(param1, param2);
				  if (product_param.LocalSize() != param1.LocalSize() + param2.LocalSize()) {
					  System.err.println("In ProductParameterizationTestLocalAndGlobalSize2()");
					  System.err.println("product_param.LocalSize() != param1.LocalSize() + param2.LocalSize()");
					  passed = false;
				  }
				  if (product_param.GlobalSize() != param1.GlobalSize() + param2.GlobalSize()) {
					  System.err.println("In ProductParameterizationTestLocalAndGlobalSize2()");
					  System.err.println("product_param.GlobalSize() != param1.GlobalSize() + param2.GlobalSize()");
					  passed = false;
				  }
				  if (passed) {
					  System.out.println("ProductParameterizationTestLocalAndGlobalSize2() passed all tests");
				  }
				}

			 public void ProductParameterizationTestLocalAndGlobalSize3() {
				  passed = true;
				  LocalParameterization param1 = param1_;
				  LocalParameterization param2 = param2_;
				  LocalParameterization param3 = param3_;

				  ProductParameterization product_param = new ProductParameterization(param1, param2, param3);
				  if (product_param.LocalSize() != param1.LocalSize() + param2.LocalSize() + param3.LocalSize()) {
					  System.err.println("In ProductParameterizationTestLocalAndGlobalSize3()");
					  System.err.println("product_param.LocalSize() != param1.LocalSize() + param2.LocalSize() + param3.LocalSize()");
					  passed = false;
				  }
				  if (product_param.GlobalSize() != param1.GlobalSize() + param2.GlobalSize() + param3.GlobalSize()) {
					  System.err.println("In ProductParameterizationTestLocalAndGlobalSize3()");
					  System.err.println("product_param.GlobalSize() != param1.GlobalSize() + param2.GlobalSize() + param3.GlobalSize()");
					  passed = false;
				  }
				  if (passed) {
					  System.out.println("ProductParameterizationTestLocalAndGlobalSize3() passed all tests");
				  }
				}
			 
			 public void ProductParameterizationTestLocalAndGlobalSize4() {
				  passed = true;
				  LocalParameterization param1 = param1_;
				  LocalParameterization param2 = param2_;
				  LocalParameterization param3 = param3_;
				  LocalParameterization param4 = param4_;

				  ProductParameterization product_param = new ProductParameterization(param1, param2, param3, param4);
				  if (product_param.LocalSize() != param1.LocalSize() + param2.LocalSize() + param3.LocalSize() + param4.LocalSize()) {
					  System.err.println("In ProductParameterizationTestLocalAndGlobalSize4()");
					  System.err.println("product_param.LocalSize() != param1.LocalSize() + param2.LocalSize() + param3.LocalSize() + param4.LocalSize()");
					  passed = false;
				  }
				  if (product_param.GlobalSize() != param1.GlobalSize() + param2.GlobalSize() + param3.GlobalSize() + param4.GlobalSize()) {
					  System.err.println("In ProductParameterizationTestLocalAndGlobalSize4()");
					  System.err.println("product_param.GlobalSize() != param1.GlobalSize() + param2.GlobalSize() + param3.GlobalSize() + param4.GlobalSize()");
					  passed = false;
				  }
				  if (passed) {
					  System.out.println("ProductParameterizationTestLocalAndGlobalSize4() passed all tests");
				  }
				}
			 
			 public void ProductParameterizationTestPlus() {
				  int i;
				  passed = true;
				  LocalParameterization param1 = param1_;
				  LocalParameterization param2 = param2_;
				  LocalParameterization param3 = param3_;
				  LocalParameterization param4 = param4_;

				  ProductParameterization product_param = new ProductParameterization(param1, param2, param3, param4);
				  Vector<Double> x = new Vector<Double>(product_param.GlobalSize());
				  Vector<Double> delta = new Vector<Double>(product_param.LocalSize());
				  Vector<Double> x_plus_delta_expected = new Vector<Double>(product_param.GlobalSize());
				  Vector<Double> x_plus_delta = new Vector<Double>(product_param.GlobalSize());

				  for (i = 0; i < product_param.GlobalSize(); ++i) {
				    x.add(RandNormal());
				    x_plus_delta_expected.add(0.0);
				    x_plus_delta.add(0.0);
				  }

				  for (i = 0; i < product_param.LocalSize(); ++i) {
				    delta.add(RandNormal());
				  }

				  if (!product_param.Plus(x, 0, delta, 0, x_plus_delta_expected,0)) {
					  System.err.println("In ProductParameterizationTestPlus() product_param.Plus(x, 0, delta, 0, x_plus_delta_expected,0) = false");
					  passed = false;
				  }
				  int x_cursor = 0;
				  int delta_cursor = 0;

				  if (!param1.Plus(x, x_cursor, delta, delta_cursor, x_plus_delta, x_cursor)) {
					  System.err.println("In ProductParameterizationTestPlus() param1.Plus(x, x_cursor, delta, delta_cursor, x_plus_delta, x_cursor) = false");
					  passed = false;
				  }
				  x_cursor += param1.GlobalSize();
				  delta_cursor += param1.LocalSize();

				  if(!param2.Plus(x, x_cursor, delta, delta_cursor, x_plus_delta, x_cursor)) {
					  System.err.println("In ProductParameterizationTestPlus() param2.Plus(x, x_cursor, delta, delta_cursor, x_plus_delta, x_cursor) = false");
					  passed = false;
				  }
				  x_cursor += param2.GlobalSize();
				  delta_cursor += param2.LocalSize();

				  if(!param3.Plus(x, x_cursor, delta, delta_cursor, x_plus_delta, x_cursor)) {
					  System.err.println("In ProductParameterizationTestPlus() param3.Plus(x, x_cursor, delta, delta_cursor, x_plus_delta, x_cursor) = false");
					  passed = false;
				  }
				  x_cursor += param3.GlobalSize();
				  delta_cursor += param3.LocalSize();
				  
				  if(!param4.Plus(x, x_cursor, delta, delta_cursor, x_plus_delta, x_cursor)) {
					  System.err.println("In ProductParameterizationTestPlus() param4.Plus(x, x_cursor, delta, delta_cursor, x_plus_delta, x_cursor) = false");
					  passed = false;
				  }
				  x_cursor += param4.GlobalSize();
				  delta_cursor += param4.LocalSize();

				  for (i = 0; i < x.size(); ++i) {
				    if (x_plus_delta.get(i).doubleValue() != x_plus_delta_expected.get(i).doubleValue()) {
				    	System.err.println("In ProductParameterizationTestPlus() x_plus_delta.get("+i+") != x_plus_delta_expected.get("+i+")");
				    	System.err.println("x_plus_delta.get("+i+") = " + x_plus_delta.get(i));
				    	System.err.println("x_plus_delta_expected.get("+i+") = " + x_plus_delta_expected.get(i));
						passed = false;
				    }
				  }
				  if (passed) {
					  System.out.println("ProductParameterizationTestPlus() passed all tests");
				  }
				}

			    public void ProductParameterizationTestComputeJacobian() {
			      int i,j;
			      passed = true;
				  LocalParameterization param1 = param1_;
				  LocalParameterization param2 = param2_;
				  LocalParameterization param3 = param3_;
				  LocalParameterization param4 = param4_;

				  ProductParameterization product_param = new ProductParameterization(param1, param2, param3, param4);
				  double x[] = new double[product_param.GlobalSize()];

				  for (i = 0; i < product_param.GlobalSize(); ++i) {
				    x[i] = RandNormal();
				  }

				  double[][] jacobian = Matrix.random(product_param.GlobalSize(),
				                                   product_param.LocalSize()).getArray();
				  if(!product_param.ComputeJacobian(x, 0, jacobian)) {
					  System.err.println("In ProductParameterizationTestComputeJacobian() product_param.ComputeJacobian(x, 0, jacobian) = false");
					  passed = false;
				  }
				  int x_cursor = 0;
				  int delta_cursor = 0;

				  double jacobian1[][] = new double[param1.GlobalSize()][param1.LocalSize()];
				  if (!param1.ComputeJacobian(x, x_cursor, jacobian1)) {
					  System.err.println("In ProductParameterizationTestComputeJacobian() param1.ComputeJacobian(x, x_cursor, jacobian1) = false");
					  passed = false;
				  }
				  for (i = x_cursor; i < x_cursor + param1.GlobalSize(); i++) {
					  for (j = delta_cursor; j < delta_cursor + param1.LocalSize(); j++) {
						  jacobian[i][j] -= jacobian1[i-x_cursor][j-delta_cursor];
					  }
				  }
				  x_cursor += param1.GlobalSize();
				  delta_cursor += param1.LocalSize();

				  double jacobian2[][] = new double[param2.GlobalSize()][param2.LocalSize()];
				  if (!param2.ComputeJacobian(x, x_cursor, jacobian2)) {
					  System.err.println("In ProductParameterizationTestComputeJacobian() param2.ComputeJacobian(x, x_cursor, jacobian2) = false");
					  passed = false;
				  }
				  for (i = x_cursor; i < x_cursor + param2.GlobalSize(); i++) {
					  for (j = delta_cursor; j < delta_cursor + param2.LocalSize(); j++) {
						  jacobian[i][j] -= jacobian2[i-x_cursor][j-delta_cursor];
					  }
				  }
				  x_cursor += param2.GlobalSize();
				  delta_cursor += param2.LocalSize();

				  double jacobian3[][] = new double[param3.GlobalSize()][param3.LocalSize()];
				  if (!param3.ComputeJacobian(x, x_cursor, jacobian3)) {
					  System.err.println("In ProductParameterizationTestComputeJacobian() param3.ComputeJacobian(x, x_cursor, jacobian3) = false");
					  passed = false;
				  }
				  for (i = x_cursor; i < x_cursor + param3.GlobalSize(); i++) {
					  for (j = delta_cursor; j < delta_cursor + param3.LocalSize(); j++) {
						  jacobian[i][j] -= jacobian3[i-x_cursor][j-delta_cursor];
					  }
				  }
				  x_cursor += param3.GlobalSize();
				  delta_cursor += param3.LocalSize();

				  double jacobian4[][] = new double[param4.GlobalSize()][param4.LocalSize()];
				  if (!param4.ComputeJacobian(x, x_cursor, jacobian4)) {
					  System.err.println("In ProductParameterizationTestComputeJacobian() param4.ComputeJacobian(x, x_cursor, jacobian4) = false");
					  passed = false;
				  }
				  for (i = x_cursor; i < x_cursor + param4.GlobalSize(); i++) {
					  for (j = delta_cursor; j < delta_cursor + param4.LocalSize(); j++) {
						  jacobian[i][j] -= jacobian4[i-x_cursor][j-delta_cursor];
					  }
				  }
				  x_cursor += param4.GlobalSize();
				  delta_cursor += param4.LocalSize();
				  
				  double normSquared = 0.0;
				  for (i = 0; i < jacobian.length; i++) {
					  for (j = 0; j < jacobian[0].length; j++) {
						  normSquared += (jacobian[i][j] * jacobian[i][j]);
					  }
				  }
				  double jacobianNorm = Math.sqrt(normSquared);

				  if (jacobianNorm > epsilon) {
					  System.err.println("In ProductParameterizationTestComputeJacobian() jacobianNorm > epsilon");
					  System.err.println("jacobianNorm = " + jacobianNorm);
					  passed = false;
				  }
				  
				  if (passed) {
					  System.out.println("ProductParameterizationTestComputeJacobian() passed all tests");
				  }
				}

			  
			};

			// Fixed sized struct for storing an evaluation.
			class ExpectedEvaluation {
				  public int num_rows;
				  public int num_cols;
				  public double cost;
				  public double residuals[] = new double[50];
				  public double gradient[] = new double[50];
				  public double jacobian[] = new double[200];
				  public ExpectedEvaluation() {
					
				  }
			} // class ExpectedEvaluation
			
			// Compare two evaluations.
			public void CompareEvaluations(int expected_num_rows,
			                        int expected_num_cols,
			                        double expected_cost,
			                        double[] expected_residuals,
			                        double[] expected_gradient,
			                        double[] expected_jacobian,
			                        double actual_cost,
			                        double[] actual_residuals,
			                        double[] actual_gradient,
			                        double[] actual_jacobian,
			                        String testName,
			                        boolean[] passed) {
				int i,j;
				if (expected_cost != actual_cost) {
				    System.err.println("In " + testName + " expected_cost != actual_cost");
				    passed[0] = false;
				}

				  if (expected_residuals != null) {
					  for (i = 0; i < expected_num_rows; i++) {
						  if (actual_residuals[i] != expected_residuals[i]) {
							  System.err.println("In " + testName + " actual_residuals["+i+"] != expected_residuals["+i+"]");
							  System.err.println("actual_residuals["+i+"] = " + actual_residuals[i]);
							  System.err.println("expected_residuals["+i+"] = " + expected_residuals[i]);
							  passed[0] = false;
						  }
					  }
				  }

				  if (expected_gradient != null) {
					  for (i = 0; i < expected_num_cols; i++) {
						  if (actual_gradient[i] != expected_gradient[i]) {
							  System.err.println("In " + testName + " actual_gradient["+i+"] != expected_gradient["+i+"]");
							  System.err.println("actual_gradient["+i+"] = " + actual_gradient[i]);
							  System.err.println("expected_gradient["+i+"] = " + expected_gradient[i]);
							  passed[0] = false;  
						  }
					  }
				  }

				  if (expected_jacobian != null) {
					  for (i = 0; i < expected_num_rows; i++) {
						  for (j = 0; j < expected_num_cols; j++) {
						      if (actual_jacobian[i*expected_num_cols + j] != expected_jacobian[i*expected_num_cols + j]) {
						    	  System.err.println("In " + testName + " actual_jacobian["+i+"]["+j+"] != expected_jacobian["+i+"]["+j+"]");
						    	  System.err.println("actual_jacobian["+i+"]["+j+"] = " + actual_jacobian[i*expected_num_cols + j]);
						    	  System.err.println("expected_jacobian["+i+"]["+j+"] = " + expected_jacobian[i*expected_num_cols + j]);
						    	  passed[0] = false;
						      }
						  }
					  }
				  }
				  //if (passed[0]) {
				  //System.out.println(testName + " passed all tests");
				  //}

			}
			
			public void CompareEvaluations(int expected_num_rows,
                    int expected_num_cols,
                    double expected_cost,
                    double[] expected_residuals,
                    double[] expected_gradient,
                    double[][] expected_jacobian,
                    double actual_cost,
                    double[] actual_residuals,
                    double[] actual_gradient,
                    double[][] actual_jacobian,
                    String testName,
                    boolean passed[]) {
			int i,j;
			if (expected_cost != actual_cost) {
			    System.err.println("In " + testName + " expected_cost != actual_cost");
			    passed[0] = false;
			}
			
			  if (expected_residuals != null) {
				  for (i = 0; i < expected_num_rows; i++) {
					  if (actual_residuals[i] != expected_residuals[i]) {
						  System.err.println("In " + testName + " actual_residuals["+i+"] != expected_residuals["+i+"]");
						  System.err.println("actual_residuals["+i+"] = " + actual_residuals[i]);
						  System.err.println("expected_residuals["+i+"] = " + expected_residuals[i]);
						  passed[0] = false;
					  }
				  }
			  }
			
			  if (expected_gradient != null) {
				  for (i = 0; i < expected_num_cols; i++) {
					  if (actual_gradient[i] != expected_gradient[i]) {
						  System.err.println("In " + testName + " actual_gradient["+i+"] != expected_gradient["+i+"]");
						  System.err.println("actual_gradient["+i+"] = " + actual_gradient[i]);
						  System.err.println("expected_gradient["+i+"] = " + expected_gradient[i]);
						  passed[0] = false;  
					  }
				  }
			  }
			
			  if (expected_jacobian != null) {
				  for (i = 0; i < expected_num_rows; i++) {
					  for (j = 0; j < expected_num_cols; j++) {
					      if (actual_jacobian[i][j] != expected_jacobian[i][j]) {
					    	  System.err.println("In " + testName + " actual_jacobian["+i+"]["+j+"] != expected_jacobian["+i+"]["+j+"]");
					    	  System.err.println("actual_jacobian["+i+"]["+j+"] = " + actual_jacobian[i][j]);
					    	  System.err.println("expected_jacobian["+i+"]["+j+"] = " + expected_jacobian[i][j]);
					    	  passed[0] = false;
					      }
					  }
				  }
			  }
			  //if (passed[0]) {
			  //System.out.println(testName + " passed all tests");
			  //}
			
			}
			
			// TODO(keir): Consider pushing this into a common test utils file.
			//template<int kFactor, int kNumResiduals,
			//         int N0 = 0, int N1 = 0, int N2 = 0, bool kSucceeds = true>
			class ParameterIgnoringCostFunction extends SizedCostFunction {
				 int kFactor;
				 boolean kSucceeds;
				 int r, c;
			     public ParameterIgnoringCostFunction(int kFactor, int kNumResiduals, int N0, int N1, int N2, boolean kSucceeds) {
			    	 super(kNumResiduals, N0, N1, N2, 0, 0, 0, 0, 0, 0, 0);
			    	 this.kFactor = kFactor;
			    	 this.kSucceeds = kSucceeds;
			     }
			     
			     public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			 
			    for (int i = 0; i <num_residuals(); ++i) {
			      residuals[i] = i + 1;
			    }
			    if (jacobians != null) {
			      for (int k = 0; k < parameter_block_sizes().size(); ++k) {
			        // The jacobians here are full sized, but they are transformed in the
			        // evaluator into the "local" jacobian. In the tests, the "subset
			        // constant" parameterization is used, which should pick out columns
			        // from these jacobians. Put values in the jacobian that make this
			        // obvious; in particular, make the jacobians like this:
			        //
			        //   1 2 3 4 ...
			        //   1 2 3 4 ...   .*  kFactor
			        //   1 2 3 4 ...
			        //
			        // where the multiplication by kFactor makes it easier to distinguish
			        // between Jacobians of different residuals for the same parameter.
			        if (jacobians[k] != null) {
			          for (int j = 0; j < parameter_block_sizes().get(k); ++j) {
			        	for (r = 0;  r < num_residuals(); r++) {
			        		jacobians[k][r * parameter_block_sizes().get(k) + j] = kFactor * (j+1);
			        	}
			          }
			        }
			      }
			    }
			    return kSucceeds;
			  }
			     
			     public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
					 
					    for (int i = 0; i <num_residuals(); ++i) {
					      residuals[i] = i + 1;
					    }
					    if (jacobians != null) {
					      for (int k = 0; k < parameter_block_sizes().size(); ++k) {
					        // The jacobians here are full sized, but they are transformed in the
					        // evaluator into the "local" jacobian. In the tests, the "subset
					        // constant" parameterization is used, which should pick out columns
					        // from these jacobians. Put values in the jacobian that make this
					        // obvious; in particular, make the jacobians like this:
					        //
					        //   1 2 3 4 ...
					        //   1 2 3 4 ...   .*  kFactor
					        //   1 2 3 4 ...
					        //
					        // where the multiplication by kFactor makes it easier to distinguish
					        // between Jacobians of different residuals for the same parameter.
					        if (jacobians[k] != null) {
					          for (int j = 0; j < parameter_block_sizes().get(k); ++j) {
					        	for (r = 0;  r < num_residuals(); r++) {
					        		jacobians[k][jacobians_offset[k] + r * parameter_block_sizes().get(k) + j] = kFactor * (j+1);
					        	}
					          }
					        }
					      }
					    }
					    return kSucceeds;
					  }
			};
			
			public void EvaluatorTestSingleResidualProblem() {
				// EvaluatorTestSingleResidualProblem_DENSE_QR_0 passed all tests
				// EvaluatorTestSingleResidualProblem_DENSE_SCHUR_0 passed all tests
				// EvaluatorTestSingleResidualProblem_DENSE_SCHUR_1 passed all tests
				// EvaluatorTestSingleResidualProblem_DENSE_SCHUR_2 passed all tests
				// EvaluatorTestSingleResidualProblem_DENSE_SCHUR_3 passed all tests
				// EvaluatorTestSingleResidualProblem_DENSE_SCHUR_4 passed all tests
				// EvaluatorTestSingleResidualProblem_ITERATIVE_SCHUR_0 passed all tests
				// EvaluatorTestSingleResidualProblem_ITERATIVE_SCHUR_1 passed all tests
				// EvaluatorTestSingleResidualProblem_ITERATIVE_SCHUR_2 passed all tests
				// EvaluatorTestSingleResidualProblem_ITERATIVE_SCHUR_3 passed all tests
				// EvaluatorTestSingleResidualProblem_ITERATIVE_SCHUR_4 passed all tests
				int i;
				String baseName = "EvaluatorTestSingleResidualProblem";
				String ETName[] = createEvaluateStringName();
				EvaluatorTestOptions evaluatorTO[] = createEvaluatorTestOptions();
				for (i = 0; i < evaluatorTO.length; i++) {
				  EvaluatorTest ET = new EvaluatorTest(evaluatorTO[i],baseName + "_" + ETName[i]);
				  ET.EvaluatorTestSingleResidualProblem();
				}
			}
			
			public void EvaluatorTestSingleResidualProblemWithPermutedParameters() {
				// EvaluatorTestSingleResidualProblemWithPermutedParameters_DENSE_QR_0 passed all tests
				// EvaluatorTestSingleResidualProblemWithPermutedParameters_DENSE_SCHUR_0 passed all tests
				// EvaluatorTestSingleResidualProblemWithPermutedParameters_DENSE_SCHUR_1 passed all tests
				// EvaluatorTestSingleResidualProblemWithPermutedParameters_DENSE_SCHUR_2 passed all tests
				// EvaluatorTestSingleResidualProblemWithPermutedParameters_DENSE_SCHUR_3 passed all tests
				// EvaluatorTestSingleResidualProblemWithPermutedParameters_DENSE_SCHUR_4 passed all tests
				// EvaluatorTestSingleResidualProblemWithPermutedParameters_ITERATIVE_SCHUR_0 passed all tests
				// EvaluatorTestSingleResidualProblemWithPermutedParameters_ITERATIVE_SCHUR_1 passed all tests
				// EvaluatorTestSingleResidualProblemWithPermutedParameters_ITERATIVE_SCHUR_2 passed all tests
				// EvaluatorTestSingleResidualProblemWithPermutedParameters_ITERATIVE_SCHUR_3 passed all tests
				// EvaluatorTestSingleResidualProblemWithPermutedParameters_ITERATIVE_SCHUR_4 passed all tests
				int i;
				String baseName = "EvaluatorTestSingleResidualProblemWithPermutedParameters";
				String ETName[] = createEvaluateStringName();
				EvaluatorTestOptions evaluatorTO[] = createEvaluatorTestOptions();
				for (i = 0; i < evaluatorTO.length; i++) {
				  EvaluatorTest ET = new EvaluatorTest(evaluatorTO[i],baseName + "_" + ETName[i]);
				  ET.EvaluatorTestSingleResidualProblemWithPermutedParameters();
				}	
			}
			
			public void EvaluatorTestSingleResidualProblemWithNuisanceParameters() {
				// EvaluatorTestSingleResidualProblemWithNuisanceParameters_DENSE_QR_0 passed all tests
				// EvaluatorTestSingleResidualProblemWithNuisanceParameters_DENSE_SCHUR_0 passed all tests
				// EvaluatorTestSingleResidualProblemWithNuisanceParameters_DENSE_SCHUR_1 passed all tests
				// EvaluatorTestSingleResidualProblemWithNuisanceParameters_DENSE_SCHUR_2 passed all tests
				// EvaluatorTestSingleResidualProblemWithNuisanceParameters_DENSE_SCHUR_3 passed all tests
				// EvaluatorTestSingleResidualProblemWithNuisanceParameters_DENSE_SCHUR_4 passed all tests
				// EvaluatorTestSingleResidualProblemWithNuisanceParameters_ITERATIVE_SCHUR_0 passed all tests
				// EvaluatorTestSingleResidualProblemWithNuisanceParameters_ITERATIVE_SCHUR_1 passed all tests
				// EvaluatorTestSingleResidualProblemWithNuisanceParameters_ITERATIVE_SCHUR_2 passed all tests
				// EvaluatorTestSingleResidualProblemWithNuisanceParameters_ITERATIVE_SCHUR_3 passed all tests
				// EvaluatorTestSingleResidualProblemWithNuisanceParameters_ITERATIVE_SCHUR_4 passed all tests
				int i;
				String baseName = "EvaluatorTestSingleResidualProblemWithNuisanceParameters";
				String ETName[] = createEvaluateStringName();
				EvaluatorTestOptions evaluatorTO[] = createEvaluatorTestOptions();
				for (i = 0; i < evaluatorTO.length; i++) {
				  EvaluatorTest ET = new EvaluatorTest(evaluatorTO[i],baseName + "_" + ETName[i]);
				  ET.EvaluatorTestSingleResidualProblemWithNuisanceParameters();
				}		
			}
			
			public void EvaluatorTestMultipleResidualProblem() {
				// EvaluatorTestMultipleResidualProblem_DENSE_QR_0 passed all tests
				// EvaluatorTestMultipleResidualProblem_DENSE_SCHUR_0 passed all tests
				// EvaluatorTestMultipleResidualProblem_DENSE_SCHUR_1 passed all tests
				// EvaluatorTestMultipleResidualProblem_DENSE_SCHUR_2 passed all tests
				// EvaluatorTestMultipleResidualProblem_DENSE_SCHUR_3 passed all tests
				// EvaluatorTestMultipleResidualProblem_DENSE_SCHUR_4 passed all tests
				// EvaluatorTestMultipleResidualProblem_ITERATIVE_SCHUR_0 passed all tests
				// EvaluatorTestMultipleResidualProblem_ITERATIVE_SCHUR_1 passed all tests
				// EvaluatorTestMultipleResidualProblem_ITERATIVE_SCHUR_2 passed all tests
				// EvaluatorTestMultipleResidualProblem_ITERATIVE_SCHUR_3 passed all tests
				// EvaluatorTestMultipleResidualProblem_ITERATIVE_SCHUR_4 passed all tests
				int i;
				String baseName = "EvaluatorTestMultipleResidualProblem";
				String ETName[] = createEvaluateStringName();
				EvaluatorTestOptions evaluatorTO[] = createEvaluatorTestOptions();
				for (i = 0; i < evaluatorTO.length; i++) {
				  EvaluatorTest ET = new EvaluatorTest(evaluatorTO[i],baseName + "_" + ETName[i]);
				  ET.EvaluatorTestMultipleResidualProblem();
				}		
			}
			
			public void EvaluatorTestMultipleResidualsWithLocalParameterizations() {
				// EvaluatorTestMultipleResidualsWithLocalParameterizations_DENSE_QR_0 passed all tests
				// EvaluatorTestMultipleResidualsWithLocalParameterizations_DENSE_SCHUR_0 passed all tests
				// EvaluatorTestMultipleResidualsWithLocalParameterizations_DENSE_SCHUR_1 passed all tests
				// EvaluatorTestMultipleResidualsWithLocalParameterizations_DENSE_SCHUR_2 passed all tests
				// EvaluatorTestMultipleResidualsWithLocalParameterizations_DENSE_SCHUR_3 passed all tests
				// EvaluatorTestMultipleResidualsWithLocalParameterizations_DENSE_SCHUR_4 passed all tests
				// EvaluatorTestMultipleResidualsWithLocalParameterizations_ITERATIVE_SCHUR_0 passed all tests
				// EvaluatorTestMultipleResidualsWithLocalParameterizations_ITERATIVE_SCHUR_1 passed all tests
				// EvaluatorTestMultipleResidualsWithLocalParameterizations_ITERATIVE_SCHUR_2 passed all tests
				// EvaluatorTestMultipleResidualsWithLocalParameterizations_ITERATIVE_SCHUR_3 passed all tests
				// EvaluatorTestMultipleResidualsWithLocalParameterizations_ITERATIVE_SCHUR_4 passed all tests
				int i;
				String baseName = "EvaluatorTestMultipleResidualsWithLocalParameterizations";
				String ETName[] = createEvaluateStringName();
				EvaluatorTestOptions evaluatorTO[] = createEvaluatorTestOptions();
				for (i = 0; i < evaluatorTO.length; i++) {
				  EvaluatorTest ET = new EvaluatorTest(evaluatorTO[i],baseName + "_" + ETName[i]);
				  ET.EvaluatorTestMultipleResidualsWithLocalParameterizations();
				}			
			}
			
			public void EvaluatorTestMultipleResidualProblemWithSomeConstantParameters() {
				// EvaluatorTestMultipleResidualProblemWithSomeConstantParameters_DENSE_QR_0 passed all tests
				// EvaluatorTestMultipleResidualProblemWithSomeConstantParameters_DENSE_SCHUR_0 passed all tests
				// EvaluatorTestMultipleResidualProblemWithSomeConstantParameters_DENSE_SCHUR_1 passed all tests
				// EvaluatorTestMultipleResidualProblemWithSomeConstantParameters_DENSE_SCHUR_2 passed all tests
				// EvaluatorTestMultipleResidualProblemWithSomeConstantParameters_DENSE_SCHUR_3 passed all tests
				// EvaluatorTestMultipleResidualProblemWithSomeConstantParameters_DENSE_SCHUR_4 passed all tests
				// EvaluatorTestMultipleResidualProblemWithSomeConstantParameters_ITERATIVE_SCHUR_0 passed all tests
				// EvaluatorTestMultipleResidualProblemWithSomeConstantParameters_ITERATIVE_SCHUR_1 passed all tests
				// EvaluatorTestMultipleResidualProblemWithSomeConstantParameters_ITERATIVE_SCHUR_2 passed all tests
				// EvaluatorTestMultipleResidualProblemWithSomeConstantParameters_ITERATIVE_SCHUR_3 passed all tests
				// EvaluatorTestMultipleResidualProblemWithSomeConstantParameters_ITERATIVE_SCHUR_4 passed all tests
				int i;
				String baseName = "EvaluatorTestMultipleResidualProblemWithSomeConstantParameters";
				String ETName[] = createEvaluateStringName();
				EvaluatorTestOptions evaluatorTO[] = createEvaluatorTestOptions();
				for (i = 0; i < evaluatorTO.length; i++) {
				  EvaluatorTest ET = new EvaluatorTest(evaluatorTO[i],baseName + "_" + ETName[i]);
				  ET.EvaluatorTestMultipleResidualProblemWithSomeConstantParameters();
				}		
			}
			
			public void EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate() {
				// EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate_DENSE_QR_0 passed all tests
				// EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate_DENSE_SCHUR_0 passed all tests
				// EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate_DENSE_SCHUR_1 passed all tests
				// EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate_DENSE_SCHUR_2 passed all tests
				// EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate_DENSE_SCHUR_3 passed all tests
				// EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate_DENSE_SCHUR_4 passed all tests
				// EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate_ITERATIVE_SCHUR_0 passed all tests
				// EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate_ITERATIVE_SCHUR_1 passed all tests
				// EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate_ITERATIVE_SCHUR_2 passed all tests
				// EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate_ITERATIVE_SCHUR_3 passed all tests
				// EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate_ITERATIVE_SCHUR_4 passed all tests
				int i;
				String baseName = "EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate";
				String ETName[] = createEvaluateStringName();
				EvaluatorTestOptions evaluatorTO[] = createEvaluatorTestOptions();
				for (i = 0; i < evaluatorTO.length; i++) {
				  EvaluatorTest ET = new EvaluatorTest(evaluatorTO[i],baseName + "_" + ETName[i]);
				  ET.EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate();
				}			
			}
			
			public EvaluatorTestOptions[] createEvaluatorTestOptions() {
				EvaluatorTestOptions evaluatorTO[] = new EvaluatorTestOptions[11];
				evaluatorTO[0] = new EvaluatorTestOptions(LinearSolverType.DENSE_QR, 0);
				evaluatorTO[1] = new EvaluatorTestOptions(LinearSolverType.DENSE_SCHUR, 0);
				evaluatorTO[2] = new EvaluatorTestOptions(LinearSolverType.DENSE_SCHUR, 1);
				evaluatorTO[3] = new EvaluatorTestOptions(LinearSolverType.DENSE_SCHUR, 2);
				evaluatorTO[4] = new EvaluatorTestOptions(LinearSolverType.DENSE_SCHUR, 3);
				evaluatorTO[5] = new EvaluatorTestOptions(LinearSolverType.DENSE_SCHUR, 4);
				evaluatorTO[6] = new EvaluatorTestOptions(LinearSolverType.ITERATIVE_SCHUR, 0);
				evaluatorTO[7] = new EvaluatorTestOptions(LinearSolverType.ITERATIVE_SCHUR, 1);
				evaluatorTO[8] = new EvaluatorTestOptions(LinearSolverType.ITERATIVE_SCHUR, 2);
				evaluatorTO[9] = new EvaluatorTestOptions(LinearSolverType.ITERATIVE_SCHUR, 3);
				evaluatorTO[10] = new EvaluatorTestOptions(LinearSolverType.ITERATIVE_SCHUR, 4);
				return evaluatorTO;
			}
			
			public String[] createEvaluateStringName() {
				String ESN[] = new String[11];
				ESN[0] = "DENSE_QR_0";
				ESN[1] = "DENSE_SCHUR_0";
				ESN[2] = "DENSE_SCHUR_1";
				ESN[3] = "DENSE_SCHUR_2";
				ESN[4] = "DENSE_SCHUR_3";
				ESN[5] = "DENSE_SCHUR_4";
				ESN[6] = "ITERATIVE_SCHUR_0";
				ESN[7] = "ITERATIVE_SCHUR_1";
				ESN[8] = "ITERATIVE_SCHUR_2";
				ESN[9] = "ITERATIVE_SCHUR_3";
				ESN[10] = "ITERATIVE_SCHUR_4";
				return ESN;
			}
			
			class EvaluatorTestOptions {
				  public LinearSolverType linear_solver_type;
				  public int num_eliminate_blocks;
				  // Dynamic sparsity is only supported with SPARSE_NORMAL_CHOLESKY which has not been implemented
				  public boolean dynamic_sparsity = false;
				  public EvaluatorTestOptions(LinearSolverType linear_solver_type,
				                       int num_eliminate_blocks) {
					  this.linear_solver_type = linear_solver_type;
					  this.num_eliminate_blocks = num_eliminate_blocks;
				  }
		   }
			
			class EvaluatorTest {
				// The values are ignored completely by the cost function.
				public double x[] = new double[2];
				public double y[] = new double[3];
				public double z[] = new double[4];

				public ProblemImpl problem = new ProblemImpl();
				public EvaluatorTestOptions ETOptions;
				public String testName;
				public EvaluatorTest(EvaluatorTestOptions ETOptions, String testName) {
					this.ETOptions = ETOptions;
					this.testName = testName;
				}
				
		        public Evaluator CreateEvaluator(Program program) {
		    // This program is straight from the ProblemImpl, and so has no index/offset
		    // yet; compute it here as required by the evalutor implementations.
		        program.SetParameterOffsetsAndIndex();

		    if (1 <= MAX_LOG_LEVEL) {
		      Preferences.debug("Creating evaluator with type: " + LinearSolverTypeToString(ETOptions.linear_solver_type) + "\n",
		    		  Preferences.DEBUG_ALGORITHM);
		      //if (GetParam().linear_solver_type == SPARSE_NORMAL_CHOLESKY) {
		        //StringAppendF(&report, ", dynamic_sparsity: %d",
		                     // GetParam().dynamic_sparsity);
		      //}
		      Preferences.debug(" and num_eliminate_blocks: " + ETOptions.num_eliminate_blocks + "\n",
		    		  Preferences.DEBUG_ALGORITHM);
		    }
		    EvaluatorOptions options = new EvaluatorOptions();
		    options.linear_solver_type = ETOptions.linear_solver_type;
		    options.num_eliminate_blocks = ETOptions.num_eliminate_blocks;
		    options.dynamic_sparsity = ETOptions.dynamic_sparsity;
		    options.context = problem.context();
		    String error[] = new String[1];
		    return Create(options, program, error);
		  }

		  public void EvaluateAndCompare(ProblemImpl problem,
		                          int expected_num_rows,
		                          int expected_num_cols,
		                          double expected_cost,
		                          double[] expected_residuals,
		                          double[] expected_gradient,
		                          double[] expected_jacobian,
		                          boolean[] passed) {
			
			int i,j;
		    Evaluator evaluator = 
		        CreateEvaluator(problem.mutable_program());
		    int num_residuals = expected_num_rows;
		    int num_parameters = expected_num_cols;

		    double cost[] = new double[] {-1};

		    double residuals[] = new double[num_residuals];
		    for (i = 0; i < num_residuals; i++) {
		    	residuals[i] = -2000.0;
		    }

		    double gradient[] = new double[num_parameters];
		    for (i = 0; i < num_parameters; i++) {
		    	gradient[i] = -3000.0;
		    }

		    SparseMatrix jacobianMat = evaluator.CreateJacobian();
		    double jacobian[] = jacobianMat.values();

		    if (expected_num_rows != evaluator.NumResiduals()) {
		    	System.err.println("In " + testName + " expected_num_rows != evaluator.NumResiduals()");
		    	passed[0] = false;
		    }
		    if (expected_num_cols != evaluator.NumEffectiveParameters()) {
		    	System.err.println("In " + testName + " expected_num_cols != evaluator.NumEffectiveParameters()");
		    	passed[0] = false;
		    }
		    if (expected_num_rows != jacobianMat.num_rows()) {
		    	System.err.println("In " + testName + " expected_num_rows != jacobianMat.num_rows()");
		    	passed[0] = false;
		    }
		    if (expected_num_cols != jacobianMat.num_cols()) {
		    	System.err.println("In " + testName + " expected_num_cols != jacobianMat.num_cols()");
		    	passed[0] = false;
		    }

		    double state[] = new double[evaluator.NumParameters()];

		    double er[];
		    if (expected_residuals != null) {
		    	er = residuals;
		    }
		    else {
		    	er = null;
		    }
		    double eg[];
		    if (expected_gradient != null) {
		    	eg = gradient;
		    }
		    else {
		    	eg = null;
		    }
		    SparseMatrix ej;
		    if (expected_jacobian != null) {
		    	ej = jacobianMat;
		    }
		    else {
		    	ej = null;
		    }
		    if (!evaluator.Evaluate(
		          state,
		          cost,
		          er,
		          eg,
		          ej)) {
		    	System.err.println("In " + testName + " evaluator.Evaluate = false");
		    	passed[0] = false;
		    }

		    Matrix actual_jacobian = null;
		    if (expected_jacobian != null) {
		      actual_jacobian = jacobianMat.ToDenseMatrix();
		    }

		    double jd[];
		    if (actual_jacobian != null) {
		    	jd = new double[actual_jacobian.getRowDimension() * actual_jacobian.getColumnDimension()];
		    	for (i = 0; i < actual_jacobian.getRowDimension(); i++) {
		    		for (j = 0; j < actual_jacobian.getColumnDimension(); j++) {
		    			jd[i * actual_jacobian.getColumnDimension() + j] = actual_jacobian.get(i,j);
		    		}
		    	}
		    }
		    else {
		    	jd = null;
		    }
		    CompareEvaluations(expected_num_rows,
		                       expected_num_cols,
		                       expected_cost,
		                       expected_residuals,
		                       expected_gradient,
		                       expected_jacobian,
		                       cost[0],
		                       residuals,
		                       gradient,
		                       jd, testName, passed);
		  }

		  // Try all combinations of parameters for the evaluator.
		  public void CheckAllEvaluationCombinations(ExpectedEvaluation expected) {
			boolean passed[] = new boolean[] {true};
		    for (int i = 0; i < 8; ++i) {
		      double er[];
		      if ((i == 1) || (i == 3) || (i == 5) || (i == 7)) {
		    	  er = expected.residuals;
		      }
		      else {
		    	  er = null;
		      }
		      double eg[];
		      if ((i == 2) || (i == 3) || (i == 6) || (i == 7)) {
		    	  eg = expected.gradient;  
		      }
		      else {
		    	  eg = null;
		      }
		      double ej[];
		      if (i >= 4) {
		    	  ej = expected.jacobian;
		      }
		      else {
		    	  ej = null;
		      }
		      EvaluateAndCompare(problem,
		                         expected.num_rows,
		                         expected.num_cols,
		                         expected.cost,
		                         er,
		                         eg,
		                         ej, passed);
		    }
		    if (passed[0]) {
		    	System.out.println(testName + " passed all tests");
		    }
		  }
		  
		  public void EvaluatorTestSingleResidualProblem() {
			  problem.AddResidualBlock(new ParameterIgnoringCostFunction(1, 3, 2, 3, 4, true),
			                           null,
			                           x, y, z);

			  ExpectedEvaluation expected = new ExpectedEvaluation();
			    // Rows/columns
			    expected.num_rows = 3;
			    expected.num_cols = 9;
			    // Cost
			    expected.cost = 7.0;
			    // Residuals
			    expected.residuals = new double[]{ 1.0, 2.0, 3.0 };
			    // Gradient
			    expected.gradient = new double[]{ 6.0, 12.0,              // x
			      6.0, 12.0, 18.0,        // y
			      6.0, 12.0, 18.0, 24.0,  // z
			    };
			    // Jacobian
			    //   x          y             z
			    expected.jacobian = new double[]{ 1, 2,   1, 2, 3,   1, 2, 3, 4,
			      1, 2,   1, 2, 3,   1, 2, 3, 4,
			      1, 2,   1, 2, 3,   1, 2, 3, 4
			    };
			 
			  CheckAllEvaluationCombinations(expected);
			}

		  public void EvaluatorTestSingleResidualProblemWithPermutedParameters() {
			  // Add the parameters in explicit order to force the ordering in the program.
			  problem.AddParameterBlock(x,  2);
			  problem.AddParameterBlock(y,  3);
			  problem.AddParameterBlock(z,  4);

			  // Then use a cost function which is similar to the others, but swap around
			  // the ordering of the parameters to the cost function. This shouldn't affect
			  // the jacobian evaluation, but requires explicit handling in the evaluators.
			  // At one point the compressed row evaluator had a bug that went undetected
			  // for a long time, since by chance most users added parameters to the problem
			  // in the same order that they occurred as parameters to a cost function.
			  problem.AddResidualBlock(new ParameterIgnoringCostFunction(1, 3, 4, 3, 2, true),
			                           null,
			                           z, y, x);

			  ExpectedEvaluation expected = new ExpectedEvaluation();
			    // Rows/columns
			    expected.num_rows = 3;
			    expected.num_cols = 9;
			    // Cost
			    expected.cost = 7.0;
			    // Residuals
			    expected.residuals = new double[]{ 1.0, 2.0, 3.0 };
			    // Gradient
			    expected.gradient = new double[]{ 6.0, 12.0,              // x
			      6.0, 12.0, 18.0,        // y
			      6.0, 12.0, 18.0, 24.0,  // z
			    };
			    // Jacobian
			    //   x          y             z
			    expected.jacobian = new double[]{ 1, 2,   1, 2, 3,   1, 2, 3, 4,
			      1, 2,   1, 2, 3,   1, 2, 3, 4,
			      1, 2,   1, 2, 3,   1, 2, 3, 4
			    };
			  CheckAllEvaluationCombinations(expected);
			}
		  
		  public void EvaluatorTestSingleResidualProblemWithNuisanceParameters() {
			  // These parameters are not used.
			  double a[] = new double[2];
			  double b[] = new double[1];
			  double c[] = new double[1];
			  double d[] = new double[3];

			  // Add the parameters in a mixed order so the Jacobian is "checkered" with the
			  // values from the other parameters.
			  problem.AddParameterBlock(a, 2);
			  problem.AddParameterBlock(x, 2);
			  problem.AddParameterBlock(b, 1);
			  problem.AddParameterBlock(y, 3);
			  problem.AddParameterBlock(c, 1);
			  problem.AddParameterBlock(z, 4);
			  problem.AddParameterBlock(d, 3);

			  problem.AddResidualBlock(new ParameterIgnoringCostFunction(1, 3, 2, 3, 4, true),
			                           null,
			                           x, y, z);

			  ExpectedEvaluation expected = new ExpectedEvaluation();
			    // Rows/columns
			    expected.num_rows = 3;
			    expected.num_cols = 16;
			    // Cost
			    expected.cost = 7.0;
			    // Residuals
			    expected.residuals = new double[]{ 1.0, 2.0, 3.0 };
			    // Gradient
			    expected.gradient = new double[] { 0.0, 0.0,               // a
			      6.0, 12.0,              // x
			      0.0,                    // b
			      6.0, 12.0, 18.0,        // y
			      0.0,                    // c
			      6.0, 12.0, 18.0, 24.0,  // z
			      0.0, 0.0, 0.0,          // d
			    };
			    // Jacobian
			    //   a        x     b           y     c              z           d
			    expected.jacobian = new double[]{ 0, 0,    1, 2,    0,    1, 2, 3,    0,    1, 2, 3, 4,    0, 0, 0,
			      0, 0,    1, 2,    0,    1, 2, 3,    0,    1, 2, 3, 4,    0, 0, 0,
			      0, 0,    1, 2,    0,    1, 2, 3,    0,    1, 2, 3, 4,    0, 0, 0
			    };
			  CheckAllEvaluationCombinations(expected);
			}

		  public void EvaluatorTestMultipleResidualProblem() {
			  // Add the parameters in explicit order to force the ordering in the program.
			  problem.AddParameterBlock(x,  2);
			  problem.AddParameterBlock(y,  3);
			  problem.AddParameterBlock(z,  4);

			  // f(x, y) in R^2
			  problem.AddResidualBlock(new ParameterIgnoringCostFunction(1, 2, 2, 3, 0, true),
			                           null,
			                           x, y);

			  // g(x, z) in R^3
			  problem.AddResidualBlock(new ParameterIgnoringCostFunction(2, 3, 2, 4, 0, true),
			                           null,
			                           x, z);

			  // h(y, z) in R^4
			  problem.AddResidualBlock(new ParameterIgnoringCostFunction(3, 4, 3, 4, 0, true),
			                           null,
			                           y, z);

			  ExpectedEvaluation expected = new ExpectedEvaluation();
			    // Rows/columns
			    expected.num_rows = 9;
			    expected.num_cols = 9;
			    // Cost
			    // f       g           h
			    expected.cost = (  1 + 4 + 1 + 4 + 9 + 1 + 4 + 9 + 16) / 2.0;
			    // Residuals
			    expected.residuals = new double[] { 1.0, 2.0,           // f
			      1.0, 2.0, 3.0,      // g
			      1.0, 2.0, 3.0, 4.0  // h
			    };
			    // Gradient
			    expected.gradient = new double[] { 15.0, 30.0,               // x
			      33.0, 66.0, 99.0,         // y
			      42.0, 84.0, 126.0, 168.0  // z
			    };
			    // Jacobian
			    //                x        y           z
			    expected.jacobian = new double[] {   /* f(x, y) */ 1, 2,    1, 2, 3,    0, 0, 0, 0,
			                      1, 2,    1, 2, 3,    0, 0, 0, 0,

			        /* g(x, z) */ 2, 4,    0, 0, 0,    2, 4, 6, 8,
			                      2, 4,    0, 0, 0,    2, 4, 6, 8,
			                      2, 4,    0, 0, 0,    2, 4, 6, 8,

			        /* h(y, z) */ 0, 0,    3, 6, 9,    3, 6, 9, 12,
			                      0, 0,    3, 6, 9,    3, 6, 9, 12,
			                      0, 0,    3, 6, 9,    3, 6, 9, 12,
			                      0, 0,    3, 6, 9,    3, 6, 9, 12
			    };
			  CheckAllEvaluationCombinations(expected);
			}
		  
		  public void EvaluatorTestMultipleResidualsWithLocalParameterizations() {
			  // Add the parameters in explicit order to force the ordering in the program.
			  problem.AddParameterBlock(x,  2);

			  // Fix y's first dimension.
			  Vector<Integer> y_fixed = new Vector<Integer>();
			  y_fixed.add(0);
			  problem.AddParameterBlock(y, 3, new SubsetParameterization(3, y_fixed));

			  // Fix z's second dimension.
			  Vector<Integer> z_fixed = new Vector<Integer>();
			  z_fixed.add(1);
			  problem.AddParameterBlock(z, 4, new SubsetParameterization(4, z_fixed));

			  // f(x, y) in R^2
			  problem.AddResidualBlock(new ParameterIgnoringCostFunction(1, 2, 2, 3, 0, true),
			                           null,
			                           x, y);

			  // g(x, z) in R^3
			  problem.AddResidualBlock(new ParameterIgnoringCostFunction(2, 3, 2, 4, 0, true),
			                           null,
			                           x, z);

			  // h(y, z) in R^4
			  problem.AddResidualBlock(new ParameterIgnoringCostFunction(3, 4, 3, 4, 0, true),
			                           null,
			                           y, z);

			  ExpectedEvaluation expected = new ExpectedEvaluation();
			    // Rows/columns
			    expected.num_rows = 9;
			    expected.num_cols = 7;
			    // Cost
			    // f       g           h
			    expected.cost = (  1 + 4 + 1 + 4 + 9 + 1 + 4 + 9 + 16) / 2.0;
			    // Residuals
			    expected.residuals = new double[] { 1.0, 2.0,           // f
			      1.0, 2.0, 3.0,      // g
			      1.0, 2.0, 3.0, 4.0  // h
			    };
			    // Gradient
			    expected.gradient = new double[] { 15.0, 30.0,         // x
			      66.0, 99.0,         // y
			      42.0, 126.0, 168.0  // z
			    };
			    // Jacobian
			    //                x        y           z
			    expected.jacobian = new double[] {   /* f(x, y) */ 1, 2,    2, 3,    0, 0, 0,
			                      1, 2,    2, 3,    0, 0, 0,

			        /* g(x, z) */ 2, 4,    0, 0,    2, 6, 8,
			                      2, 4,    0, 0,    2, 6, 8,
			                      2, 4,    0, 0,    2, 6, 8,

			        /* h(y, z) */ 0, 0,    6, 9,    3, 9, 12,
			                      0, 0,    6, 9,    3, 9, 12,
			                      0, 0,    6, 9,    3, 9, 12,
			                      0, 0,    6, 9,    3, 9, 12
			    };
			  CheckAllEvaluationCombinations(expected);
			}

		  public void EvaluatorTestMultipleResidualProblemWithSomeConstantParameters() {
			  // The values are ignored completely by the cost function.
			  double x[] = new double[2];
			  double y[] = new double[3];
			  double z[] = new double[4];

			  // Add the parameters in explicit order to force the ordering in the program.
			  problem.AddParameterBlock(x,  2);
			  problem.AddParameterBlock(y,  3);
			  problem.AddParameterBlock(z,  4);

			  // f(x, y) in R^2
			  problem.AddResidualBlock(new ParameterIgnoringCostFunction(1, 2, 2, 3, 0, true),
			                           null,
			                           x, y);

			  // g(x, z) in R^3
			  problem.AddResidualBlock(new ParameterIgnoringCostFunction(2, 3, 2, 4, 0, true),
			                           null,
			                           x, z);

			  // h(y, z) in R^4
			  problem.AddResidualBlock(new ParameterIgnoringCostFunction(3, 4, 3, 4, 0, true),
			                           null,
			                           y, z);

			  // For this test, "z" is constant.
			  problem.SetParameterBlockConstant(z);

			  // Create the reduced program which is missing the fixed "z" variable.
			  // Normally, the preprocessing of the program that happens in solver_impl
			  // takes care of this, but we don't want to invoke the solver here.
			  Program reduced_program = new Program();
			  Vector<ParameterBlock> parameter_blocks =
			      problem.mutable_program().mutable_parameter_blocks();

			  // "z" is the last parameter; save it for later and pop it off temporarily.
			  // Note that "z" will still get read during evaluation, so it cannot be
			  // deleted at this point.
			  ParameterBlock parameter_block_z = parameter_blocks.lastElement();
			  parameter_blocks.remove(parameter_blocks.size()-1);

			  ExpectedEvaluation expected = new ExpectedEvaluation();
			    // Rows/columns
			    expected.num_rows = 9;
			    expected.num_cols = 5;
			    // Cost
			    // f       g           h
			    expected.cost = (  1 + 4 + 1 + 4 + 9 + 1 + 4 + 9 + 16) / 2.0;
			    // Residuals
			    expected.residuals = new double[]{ 1.0, 2.0,           // f
			      1.0, 2.0, 3.0,      // g
			      1.0, 2.0, 3.0, 4.0  // h
			    };
			    // Gradient
			    expected.gradient = new double[]{ 15.0, 30.0,        // x
			      33.0, 66.0, 99.0,  // y
			    };
			    // Jacobian
			    //                x        y
			    expected.jacobian = new double[] {   /* f(x, y) */ 1, 2,    1, 2, 3,
			                      1, 2,    1, 2, 3,

			        /* g(x, z) */ 2, 4,    0, 0, 0,
			                      2, 4,    0, 0, 0,
			                      2, 4,    0, 0, 0,

			        /* h(y, z) */ 0, 0,    3, 6, 9,
			                      0, 0,    3, 6, 9,
			                      0, 0,    3, 6, 9,
			                      0, 0,    3, 6, 9
			    };
			  CheckAllEvaluationCombinations(expected);

			  // Restore parameter block z, so it will get freed in a consistent way.
			  parameter_blocks.add(parameter_block_z);
			}
		  
		  public void EvaluatorTestEvaluatorAbortsForResidualsThatFailToEvaluate() {
			  // Switch the return value to failure.
			  problem.AddResidualBlock(
			      new ParameterIgnoringCostFunction(20, 3, 2, 3, 4, false), null, x, y, z);

			  // The values are ignored.
			  double state[] = new double[9];

			  Evaluator evaluator = CreateEvaluator(problem.mutable_program());
			 SparseMatrix jacobian = evaluator.CreateJacobian();
			  double cost[] = new double[1];
			  if (evaluator.Evaluate(state, cost, null, null, null)) {
				  System.err.println("In " + testName + " evaluator.Evaluate(state, cost, null, null, null) == true");
			  }
			  else {
				  System.out.println(testName + " passed all tests");
			  }
			}
		  
		} // class EvaluatorTest
		
		public void SetSparseMatrixConstant(SparseMatrix sparse_matrix, double value) {
			int i;
			for (i = 0; i < sparse_matrix.num_nonzeros(); i++) {
				sparse_matrix.mutable_values()[i] = value;
			}
		}


		// Simple cost function used to check if the evaluator is sensitive to
		// state changes.
		class ParameterSensitiveCostFunction extends SizedCostFunction {
		 public ParameterSensitiveCostFunction() {
			 super(2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		 }
		 
		 public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
		 
		    double x1 = parameters.get(0)[0];
		    double x2 = parameters.get(0)[1];
		    residuals[0] = x1 * x1;
		    residuals[1] = x2 * x2;

		    if (jacobians != null) {
		      double jacobian[] = jacobians[0];
		      if (jacobian != null) {
		        jacobian[0] = 2.0 * x1;
		        jacobian[1] = 0.0;
		        jacobian[2] = 0.0;
		        jacobian[3] = 2.0 * x2;
		      }
		    }
		    return true;
		  }
		 
		 public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			 
			    double x1 = parameters.get(0)[0];
			    double x2 = parameters.get(0)[1];
			    residuals[0] = x1 * x1;
			    residuals[1] = x2 * x2;

			    if (jacobians != null) {
			      double jacobian[] = jacobians[0];
			      if (jacobian != null) {
			        jacobian[jacobians_offset[0]] = 2.0 * x1;
			        jacobian[jacobians_offset[0]+1] = 0.0;
			        jacobian[jacobians_offset[0]+2] = 0.0;
			        jacobian[jacobians_offset[0]+3] = 2.0 * x2;
			      }
			    }
			    return true;
			  }
		};
		
		public void EvaluatorRespectsParameterChanges() {
			  // EvaluatorRespectsParameterChanges() passed all tests
			  int row, col;
			  boolean passed = true;
			  ProblemImpl problem = new ProblemImpl();

			  double x[] = new double[2];
			  x[0] = 1.0;
			  x[1] = 1.0;

			  problem.AddResidualBlock(new ParameterSensitiveCostFunction(), null, x);
			  Program program = problem.mutable_program();
			  program.SetParameterOffsetsAndIndex();

			  EvaluatorOptions options = new EvaluatorOptions();
			  options.linear_solver_type = LinearSolverType.DENSE_QR;
			  options.num_eliminate_blocks = 0;
			  options.context = problem.context();
			  String error[] = new String[1];
			  Evaluator evaluator = Create(options, program, error);
			  SparseMatrix jacobian = evaluator.CreateJacobian();

			  if (jacobian.num_rows() != 2) {
				  System.err.println("In EvaluatorRespectsParameterChanges() jacobian.num_rows() != 2");
				  passed = false;
			  }
			  if (jacobian.num_cols() != 2) {
				  System.err.println("In EvaluatorRespectsParameterChanges() jacobian.num_cols() != 2");
				  passed = false; 
			  }

			  double state[] = new double[2];
			  state[0] = 2.0;
			  state[1] = 3.0;

			  // The original state of a residual block comes from the user's
			  // state. So the original state is 1.0, 1.0, and the only way we get
			  // the 2.0, 3.0 results in the following tests is if it respects the
			  // values in the state vector.

			  // Cost only; no residuals and no jacobian.
			  {
			    double cost[] = new double[] {-1};
			    if (!evaluator.Evaluate(state, cost, null, null, null)) {
			    	System.err.println("In EvaluatorRespectsParameterChanges() evaluator.Evaluate(state, cost, null, null, null) = false");
					passed = false;
			    }
			    if (cost[0] != 48.5) {
			    	System.err.println("In EvaluatorRespectsParameterChanges() cost[0] != 48.5");
					passed = false;
			    }
			  }

			  // Cost and residuals, no jacobian.
			  {
			    double cost[] = new double[] {-1};
			    double residuals[] = new double[] { -2, -2 };
			    if (!evaluator.Evaluate(state, cost, residuals, null, null)) {
			    	System.err.println("In EvaluatorRespectsParameterChanges() evaluator.Evaluate(state, cost, residuals, null, null) = false");
					passed = false;

			    }
			    if (cost[0] != 48.5) {
			    	System.err.println("In EvaluatorRespectsParameterChanges() cost[0] != 48.5");
					passed = false;
			    }
			    if (residuals[0] != 4) {
			    	System.err.println("In EvaluatorRespectsParameterChanges() residuals[0] != 4");
					passed = false;
			    }
			    if (residuals[1] != 9) {
			    	System.err.println("In EvaluatorRespectsParameterChanges() residuals[1] != 9");
					passed = false;
			    }
			  }

			  // Cost, residuals, and jacobian.
			  {
			    double cost[] = new double[] {-1};
			    double residuals[] = new double[]{ -2, -2};
			    SetSparseMatrixConstant(jacobian, -1);
			    if (!evaluator.Evaluate(state, cost, residuals, null, jacobian)) {
			    	System.err.println("In EvaluatorRespectsParameterChanges() evaluator.Evaluate(state, cost, residuals, null, jacobian) = false");
					passed = false;
			    }
			    if (cost[0] != 48.5) {
			    	System.err.println("In EvaluatorRespectsParameterChanges() cost[0] != 48.5");
					passed = false;
			    }
			    if (residuals[0] != 4) {
			    	System.err.println("In EvaluatorRespectsParameterChanges() residuals[0] != 4");
					passed = false;
			    }
			    if (residuals[1] != 9) {
			    	System.err.println("In EvaluatorRespectsParameterChanges() residuals[1] != 9");
					passed = false;
			    }
			    
			    double actual_jacobian_array[][] = 
			    jacobian.ToDenseMatrix().getArray();
			    
			    double expected_jacobian_array[][] = new double[][] {{2.0 * state[0], 0.0}, {0.0, 2.0 * state[1]}};

			    for (row = 0; row < 2; row++) {
			    	for (col = 0; col < 2; col++) {
			    		if (actual_jacobian_array[row][col] != expected_jacobian_array[row][col]) {
			    			System.err.println("In EvaluatorRespectsParameterChanges()");
			    			System.err.println("actual_jacobian_array["+row+"]["+col+"] != expected_jacobian_array["+row+"]["+col+"]");
			    	        System.err.println("actual_jacobian_array["+row+"]["+col+"] = " + actual_jacobian_array[row][col]);
			    	        System.err.println("expected_jacobian_array["+row+"]["+col+"] = " + expected_jacobian_array[row][col]);
			    	        passed = false;
			    		}
			    	}
			    }

			    
			  }
			  if (passed) {
				  System.out.println("EvaluatorRespectsParameterChanges() passed all tests");
			  }
			}

		// Rosenbrock function; see http://en.wikipedia.org/wiki/Rosenbrock_function .
		class Rosenbrock2 extends FirstOrderFunction {
		 public  Rosenbrock2() {
			 super();
		 }

		 public  boolean Evaluate(double[] parameters,
		                        double[] cost,
		                        double[] gradient) {
		    final double x = parameters[0];
		    final double y = parameters[1];

		    cost[0] = (1.0 - x) * (1.0 - x) + 100.0 * (y - x * x) * (y - x * x);
		    if (gradient != null) {
		      gradient[0] = -2.0 * (1.0 - x) - 200.0 * (y - x * x) * 2.0 * x;
		      gradient[1] = 200.0 * (y - x * x);
		    }
		    return true;
		  }

		  public int NumParameters() { return 2; }
		};

		public void GradientProblemSolverSolvesRosenbrockWithDefaultOptions() {
			  // GradientProblemSolverSolvesRosenbrockWithDefaultOptions() passed all tests
			  boolean passed = true;
			  final double expected_tolerance = 1e-9;
			  double parameters[] = new double[]{-1.2, 0.0};

			  GradientProblemSolverOptions options = new GradientProblemSolverOptions();
			  GradientProblemSolverSummary summary = new GradientProblemSolverSummary();
			  GradientProblem problem = new GradientProblem(new Rosenbrock2());
			  Solve(options, problem, parameters, summary);

			  if (summary.termination_type != TerminationType.CONVERGENCE) {
				  System.err.println("In GradientProblemSolverSolvesRosenbrockWithDefaultOptions() summary.termination_type != TerminationType.CONVERGENCE");
				  passed = false;
			  }
			  if (Math.abs(parameters[0] - 1.0) > expected_tolerance) {
				  System.err.println("In GradientProblemSolverSolvesRosenbrockWithDefaultOptions() Math.abs(parameters[0] - 1.0) > expected_tolerance");
				  passed = false;
			  }
			  if (Math.abs(parameters[1] - 1.0) > expected_tolerance) {
				  System.err.println("In GradientProblemSolverSolvesRosenbrockWithDefaultOptions() Math.abs(parameters[1] - 1.0) > expected_tolerance");
				  passed = false;
			  }
			  if (passed) {
				  System.out.println("GradientProblemSolverSolvesRosenbrockWithDefaultOptions() passed all tests");
			  }
			}
		
		class QuadraticFunction extends FirstOrderFunction {
			  public QuadraticFunction() {
				  super();
			  }
			  public boolean Evaluate(double[] parameters,
			                        double[] cost,
			                        double[] gradient) {
			    final double x = parameters[0];
			    cost[0] = 0.5 * (5.0 - x) * (5.0 - x);
			    if (gradient != null) {
			      gradient[0] = x - 5.0;
			    }

			    return true;
			  }
			  public int NumParameters() { return 1; }
			};
			
			class RememberingCallback extends IterationCallback {
				  public int calls;
				  double []x;
				  Vector<Double> x_values = new Vector<Double>();
				  public RememberingCallback(double x[]) {
					  super();
					  calls = 0;
					  this.x = x;
				  }
				  
				  public CallbackReturnType operator(IterationSummary summary) {
				    x_values.add(x[0]);
				    return CallbackReturnType.SOLVER_CONTINUE;
				  }
				  
				};

				public void SolverUpdateStateEveryIterationOption() {
					  // SolverUpdateStateEveryIterationOption() passed all tests
					  boolean passed = true;
					  double x[] = new double[] {50.0};
					  final double original_x = x[0];

					  GradientProblem problem = new GradientProblem(new QuadraticFunction());
					  GradientProblemSolverOptions options = new GradientProblemSolverOptions();
					  RememberingCallback callback = new RememberingCallback(x);
					  options.callbacks.add(callback);
					  GradientProblemSolverSummary summary = new GradientProblemSolverSummary();

					  int num_iterations;

					  // First try: no updating.
					  Solve(options, problem, x, summary);
					  num_iterations = summary.iterations.size() - 1;
					  if (num_iterations <= 1) {
						  System.err.println("In SolverUpdateStateEveryIterationOption() num_iterations <= 1");
						  passed = false;
					  }
					  for (int i = 0; i < callback.x_values.size(); ++i) {
					    if (callback.x_values.get(i).doubleValue() != 50.0) {
					    	System.err.println("In SolverUpdateStateEveryIterationOption() callback.x_values.get("+i+").doubleValue() != 50.0");
					    	passed = false;
					    }
					  }

					  // Second try: with updating
					  x[0] = 50.0;
					  options.update_state_every_iteration = true;
					  callback.x_values.clear();
					  Solve(options, problem, x, summary);
					  num_iterations = summary.iterations.size() - 1;
					  if (num_iterations <= 1) {
						  System.err.println("In SolverUpdateStateEveryIterationOption() num_iterations <= 1");
						  passed = false;
					  }
					  if (callback.x_values.get(0).doubleValue() != original_x) {
						  System.err.println("In SolverUpdateStateEveryIterationOption() callback.x_values.get(0).doubleValue() != original_x");
						  passed = false;
					  }
					  if (callback.x_values.get(1).doubleValue() == original_x) {
						  System.err.println("In SolverUpdateStateEveryIterationOption() callback.x_values.get(1).doubleValue() == original_x");
						  passed = false;
					  }
					  if (passed) {
						  System.out.println("SolverUpdateStateEveryIterationOption() passed all tests");
					  }
					}

	class QuadraticTestFunction extends FirstOrderFunction {
		private boolean[] flag_to_set_on_destruction_;
		 public QuadraticTestFunction(boolean[] flag_to_set_on_destruction /*= NULL*/) {
			 super();
			 flag_to_set_on_destruction_ = flag_to_set_on_destruction;
		 }

		  public void finalize() {
		    if (flag_to_set_on_destruction_ != null) {
		      flag_to_set_on_destruction_[0] = true;
		    }
		  }

		  public boolean Evaluate(double[] parameters,
		                        double[] cost,
		                        double[] gradient) {
		    final double x = parameters[0];
		    cost[0] = x * x;
		    if (gradient != null) {
		      gradient[0] = 2.0 * x;
		    }
		    return true;
		  }

		  public int NumParameters() { return 1; }

		 
		};
		
		public void GradientProblemTakesOwnershipOfFirstOrderFunction() {
			  // GradientProblemTakesOwnershipOfFirstOrderFunction() passed all tests
			  boolean is_destructed[] = new boolean[] {false};
			  {
			    GradientProblem problem = new GradientProblem(new QuadraticTestFunction(is_destructed));
			    problem.finalize();
			  }
			  if (!is_destructed[0]) {
				  System.err.println("is_destructed[0] = false");
			  }
			  else {
				  System.out.println("GradientProblemTakesOwnershipOfFirstOrderFunction() passed all tests");
			  }
			}

		public void GradientProblemEvaluationWithoutParameterizationOrGradient() {
			  // GradientProblemEvaluationWithoutParameterizationOrGradient() passed all tests
			  GradientProblem problem = new GradientProblem(new QuadraticTestFunction(null));
			  double x[] = new double[] {7.0};
			  double cost[] = new double[] {0.0};
			  problem.Evaluate(x, cost, null);
			  if ((x[0] * x[0]) != cost[0]) {
				  System.err.println("In GradientProblemEvaluationWithoutParameterizationOrGradient() (x[0] * x[0]) != cost[0]");
			  }
			  else {
				  System.out.println("GradientProblemEvaluationWithoutParameterizationOrGradient() passed all tests");
			  }
	   }
		
       public void GradientProblemEvalutaionWithParameterizationAndNoGradient() {
    	      // GradientProblemEvalutaionWithParameterizationAndNoGradient() passed all tests
			  GradientProblem problem = new GradientProblem(new QuadraticTestFunction(null),
			                                 new IdentityParameterization(1));
			  double x[] = new double[] {7.0};
			  double cost[] = new double[] {0.0};
			  problem.Evaluate(x, cost, null);
			  if ((x[0] * x[0]) != cost[0]) {
				  System.err.println("In GradientProblemEvalutaionWithParameterizationAndNoGradient() (x[0] * x[0]) != cost[0]");
			  }
			  else {
				  System.out.println("GradientProblemEvalutaionWithParameterizationAndNoGradient() passed all tests");
			  }
			}

       public void GradientProblemEvaluationWithoutParameterizationAndWithGradient() {
    	   // GradientProblemEvaluationWithoutParameterizationAndWithGradient() passed all tests
    	   GradientProblem problem = new GradientProblem(new QuadraticTestFunction(null));
    	   double x[] = new double[] {7.0};
    	   double cost[] = new double[] {0.0};
    	   double gradient[] = new double[] {0.0};
    	   problem.Evaluate(x, cost, gradient);
    	   if ((2.0 * x[0]) != gradient[0]) {
    		   System.err.println("In GradientProblemEvaluationWithoutParameterizationAndWithGradient() (2.0 * x[0]) != gradient[0]");
    	   }
    	   else {
    		   System.out.println("GradientProblemEvaluationWithoutParameterizationAndWithGradient() passed all tests");
    	   }
    	 }

       public void GradientProblemEvaluationWithParameterizationAndWithGradient() {
    	   // GradientProblemEvaluationWithParameterizationAndWithGradient() passed all tests
    	   GradientProblem problem = new GradientProblem(new QuadraticTestFunction(null),
    	                                  new IdentityParameterization(1));
    	   double x[] = new double[] {7.0};
    	   double cost[] = new double[] {0.0};
    	   double gradient[] = new double[] {0.0};
    	   problem.Evaluate(x, cost, gradient);
    	   if ((2.0 * x[0]) != gradient[0]) {
    		   System.err.println("In GradientProblemEvaluationWithParameterizationAndWithGradient() (2.0 * x[0]) != gradient[0]");
    	   }
    	   else {
    		   System.out.println("GradientProblemEvaluationWithParameterizationAndWithGradient() passed all tests");
    	   }
    	 }

       class FakeIterationCallback extends IterationCallback {
    	   public FakeIterationCallback() {
    		   super();
    	   }
    	   public CallbackReturnType operator(IterationSummary summary) {
    	      return CallbackReturnType.SOLVER_CONTINUE;
    	    }
    	  };
    	  
    	  public void MinimizerInitializationCopiesCallbacks() {
    		  // MinimizerInitializationCopiesCallbacks() passed all tests
    		  boolean passed = true;
    		  FakeIterationCallback callback0 = new FakeIterationCallback();
    		  FakeIterationCallback callback1 = new FakeIterationCallback();

    		  SolverOptions solver_options = new SolverOptions();
    		  solver_options.callbacks.add(callback0);
    		  solver_options.callbacks.add(callback1);

    		  MinimizerOptions minimizer_options = new MinimizerOptions(solver_options);
    		  if (minimizer_options.callbacks.size() != 2) {
    			  System.err.println("In MinimizerInitializationCopiesCallbacks() minimizer_options.callbacks.size() != 2");
    			  passed = false;
    		  }
    		  if (minimizer_options.callbacks.get(0) != callback0) {
    			  System.err.println("In MinimizerInitializationCopiesCallbacks() minimizer_options.callbacks.get(0) != callback0");
    			  passed = false;
    		  }
    		  if (minimizer_options.callbacks.get(1) != callback1) {
    			  System.err.println("In MinimizerInitializationCopiesCallbacks() minimizer_options.callbacks.get(1) != callback1");
    			  passed = false;
    		  }
    		  if (passed) {
    			  System.out.println("MinimizerInitializationCopiesCallbacks() passed all tests");
    		  }
    		}
    	  
    	  class AbortingIterationCallback extends IterationCallback {
    		  public AbortingIterationCallback() {
    			  super();
    		  }
    		  public CallbackReturnType operator(IterationSummary summary) {
    		     return CallbackReturnType.SOLVER_ABORT;
    		   }
    		 };


    	  public void MinimizerUserAbortUpdatesSummaryMessage() {
    		  // MinimizerUserAbortUpdatesSummaryMessage() passed all tests
    		  AbortingIterationCallback callback = new AbortingIterationCallback();
    		  SolverOptions solver_options = new SolverOptions();
    		  solver_options.callbacks.add(callback);
    		  MinimizerOptions minimizer_options = new MinimizerOptions(solver_options);
    		  SolverSummary summary = new SolverSummary();
    		  RunCallbacks(minimizer_options, new IterationSummary(), summary);
    		  if (!summary.message[0].equalsIgnoreCase("User callback returned SOLVER_ABORT.")) {
    			  System.err.println("In MinimizerUserAbortUpdatesSummaryMessage() summary.message[0].equalsIgnoreCase(\"User callback returned SOLVER_ABORT.\") = false");
    		  }
    		  else {
    			  System.out.println("MinimizerUserAbortUpdatesSummaryMessage() passed all tests");
    		  }
    		}
    	  
    	  class SucceedingIterationCallback extends IterationCallback {
    		  public SucceedingIterationCallback() {
    			  super();
    		  }
    		   public CallbackReturnType operator(IterationSummary summary) {
    		     return CallbackReturnType.SOLVER_TERMINATE_SUCCESSFULLY;
    		   }
    		 };

    		 public void MinimizerUserSuccessUpdatesSummaryMessage() {
    		   // MinimizerUserSuccessUpdatesSummaryMessage() passed all tests
    		   SucceedingIterationCallback callback = new SucceedingIterationCallback();
    		   SolverOptions solver_options = new SolverOptions();
    		   solver_options.callbacks.add(callback);
    		   MinimizerOptions minimizer_options = new MinimizerOptions(solver_options);
    		   SolverSummary summary = new SolverSummary();
    		   RunCallbacks(minimizer_options, new IterationSummary(), summary);
    		   if (!summary.message[0].equalsIgnoreCase("User callback returned SOLVER_TERMINATE_SUCCESSFULLY.")) {
    			   System.err.println("In MinimizerUserSuccessUpdatesSummaryMessage() summary.message[0].equalsIgnoreCase(\"User callback returned SOLVER_TERMINATE_SUCCESSFULLY.\") = false");
    		   }
    		   else {
    			   System.out.println("MinimizerUserSuccessUpdatesSummaryMessage() passed all tests");
    		   }
    		 }

    		 public void ParameterBlockSetLocalParameterizationDiesOnSizeMismatch() {
    			 // Gives appropriate error message:
    			 // Invalid parameterization for parameter block. The parameter block 
    			 // has size 3 while the parameterization has a global 
    			 // size of 4. Did you 
    			 // accidentally use the wrong parameter block or parameterization?
    			  double x[] = new double[]{1.0, 2.0, 3.0};
    			  ParameterBlock parameter_block = new ParameterBlock(x, 3, -1);
    			  Vector<Integer> indices = new Vector<Integer>();
    			  indices.add(1);
    			  SubsetParameterization subset_wrong_size = new SubsetParameterization(4, indices);
    			  //EXPECT_DEATH_IF_SUPPORTED(
    			      parameter_block.SetParameterization(subset_wrong_size);//, "global");
    			}
    		 
    		 public void ParameterBlockSetLocalParameterizationWithSameExistingParameterization() {
    			  // Runs without error
    			  double x[] = new double[]{1.0, 2.0, 3.0};
    			  ParameterBlock parameter_block = new ParameterBlock(x, 3, -1);
    			  Vector<Integer> indices = new Vector<Integer>();
    			  indices.add(1);
    			  SubsetParameterization subset = new SubsetParameterization(3, indices);
    			  parameter_block.SetParameterization(subset);
    			  parameter_block.SetParameterization(subset);
    			}
    		 
    		 public void ParameterBlockSetLocalParameterizationDiesWhenResettingToNull() {
    			  // Gives expected error message:
                  // null parameterization invalid.    			 
    			  double x[] = new double[] {1.0, 2.0, 3.0};
    			  ParameterBlock parameter_block = new ParameterBlock(x, 3, -1);
    			  Vector<Integer> indices = new Vector<Integer>();
    			  indices.add(1);
    			  SubsetParameterization subset = new SubsetParameterization(3, indices);
    			  parameter_block.SetParameterization(subset);
    			  //EXPECT_DEATH_IF_SUPPORTED(parameter_block.SetParameterization(NULL), "NULL");
    			  parameter_block.SetParameterization(null);
    			}


       public void ParameterBlockSetLocalParameterizationDiesWhenResettingToDifferentParameterization() {
    	  // Gives expected error message:
    	  // Can't re-set the local parameterization; it leads to 
    	  // ambiguous ownership. Current local parameterization is: gov.nih.mipav.model.algorithms.CeresSolver$SubsetParameterization@6be78121
    	  double x[] = new double[]{1.0, 2.0, 3.0};
    	  ParameterBlock parameter_block = new ParameterBlock(x, 3, -1);
    	  Vector<Integer> indices = new Vector<Integer>();
    	  indices.add(1);
    	  SubsetParameterization subset = new SubsetParameterization(3, indices);
    	  parameter_block.SetParameterization(subset);
    	  SubsetParameterization subset_different = new SubsetParameterization(3, indices);
    	  //EXPECT_DEATH_IF_SUPPORTED(
    	      //parameter_block.SetParameterization(&subset_different), "re-set");
    	  parameter_block.SetParameterization(subset_different);
    	}

       public void ParameterBlockSetLocalParameterizationDiesOnNullParameterization() {
    	   // Gives expected error message:
    	   // null parameterization invalid.
    	   double x[] = new double[]{1.0, 2.0, 3.0};
    	   ParameterBlock parameter_block = new ParameterBlock(x, 3, -1);
    	   Vector<Integer> indices = new Vector<Integer>();
    	   indices.add(1);
    	   //EXPECT_DEATH_IF_SUPPORTED(parameter_block.SetParameterization(NULL), "NULL");
    	   parameter_block.SetParameterization(null);
    	 }
       
       public void ParameterBlockSetParameterizationDiesOnZeroLocalSize() {
    	   // Gives expected error message:
    	   // Invalid parameterization. Parameterizations must have a positive 
    	   // dimensional tangent space.
    	   double x[] = new double[]{1.0, 2.0, 3.0};
    	   ParameterBlock parameter_block = new ParameterBlock(x, 3, -1);
    	   Vector<Integer> indices = new Vector<Integer>();
    	   indices.add(0);
    	   indices.add(1);
    	   indices.add(2);
    	   SubsetParameterization subset = new SubsetParameterization(3, indices);
    	   //EXPECT_DEATH_IF_SUPPORTED(parameter_block.SetParameterization(&subset),
    	                             //"positive dimensional tangent");
    	   parameter_block.SetParameterization(subset);
    	 }

       public void ParameterBlockSetLocalParameterizationAndNormalOperation() {
    	   // ParameterBlockSetLocalParameterizationAndNormalOperation() passed all tests
    	   boolean passed = true;
    	   int row, col;
    	   double x[] = new double[]{ 1.0, 2.0, 3.0 };
    	   ParameterBlock parameter_block = new ParameterBlock(x, 3, -1);
    	   Vector<Integer> indices = new Vector<Integer>();
    	   indices.add(1);
    	   SubsetParameterization subset = new SubsetParameterization(3, indices);
    	   parameter_block.SetParameterization(subset);

    	   // Ensure the local parameterization jacobian result is correctly computed.
    	   //ConstMatrixRef local_parameterization_jacobian(
    	       //parameter_block.LocalParameterizationJacobian(),
    	       //3,
    	       //2);
    	   double local_parameterization_jacobian[][] = new double[3][2];
    	   for (row = 0; row < 3; row++) {
    		   for (col = 0; col < 2; col++) {
    			   local_parameterization_jacobian[row][col] = parameter_block.LocalParameterizationJacobian()[row][col];
    		   }
    	   }
    	   if(local_parameterization_jacobian[0][0] != 1.0) {
    		   System.err.println("In ParameterBlockSetLocalParameterizationAndNormalOperation() local_parameterization_jacobian[0][0] != 1.0)");
    		   passed = false;
    	   }
    	   if(local_parameterization_jacobian[0][1] != 0.0) {
    		   System.err.println("In ParameterBlockSetLocalParameterizationAndNormalOperation() local_parameterization_jacobian[0][1] != 0.0)");
    		   passed = false;
    	   }
    	   if(local_parameterization_jacobian[1][0] != 0.0) {
    		   System.err.println("In ParameterBlockSetLocalParameterizationAndNormalOperation() local_parameterization_jacobian[1][0] != 0.0)");
    		   passed = false;
    	   }
    	   if(local_parameterization_jacobian[1][1] != 0.0) {
    		   System.err.println("In ParameterBlockSetLocalParameterizationAndNormalOperation() local_parameterization_jacobian[1][1] != 0.0)");
    		   passed = false;
    	   }
    	   if(local_parameterization_jacobian[2][0] != 0.0) {
    		   System.err.println("In ParameterBlockSetLocalParameterizationAndNormalOperation() local_parameterization_jacobian[2][0] != 0.0)");
    		   passed = false;
    	   }
    	   if(local_parameterization_jacobian[2][1] != 1.0) {
    		   System.err.println("In ParameterBlockSetLocalParameterizationAndNormalOperation() local_parameterization_jacobian[2][1] != 1.0)");
    		   passed = false;
    	   }

    	   // Check that updating works as expected.
    	   double x_plus_delta[] = new double[3];
    	   double delta[] = new double[]{ 0.5, 0.3 };
    	   parameter_block.Plus(x, delta, x_plus_delta);
    	   if (x_plus_delta[0] != 1.5) {
    		   System.err.println("In ParameterBlockSetLocalParameterizationAndNormalOperation() x_plus_delta[0] != 1.5");
    		   passed = false;
    	   }
    	   if (x_plus_delta[1] != 2.0) {
    		   System.err.println("In ParameterBlockSetLocalParameterizationAndNormalOperation() x_plus_delta[1] != 2.0");
    		   passed = false;
    	   }
    	   if (x_plus_delta[2] != 3.3) {
    		   System.err.println("In ParameterBlockSetLocalParameterizationAndNormalOperation() x_plus_delta[2] != 3.3");
    		   passed = false;
    	   }
    	   if (passed) {
    		   System.out.println("ParameterBlockSetLocalParameterizationAndNormalOperation() passed all tests");
    	   }
    	 }
       
       class TestParameterization extends LocalParameterization {
    		 public TestParameterization() {
    			 super();
    		 }
    		  public boolean Plus(double[] x,
    		                    double[] delta,
    		                    double[] x_plus_delta) {
    		    System.err.println("Shouldn't get called.");
    		    return true;
    		  }
    		  
    		  public boolean Plus(Vector<Double> x, int x_index,
	                    Vector<Double> delta, int delta_index,
	                    Vector<Double> x_plus_delta, int x_plus_delta_index) {
			    System.err.println("Shouldn't get called.");
			    return true;
			  }
    		  public boolean ComputeJacobian(double[] x,
    		                               double[] jacobian) {
    		    jacobian[0] = x[0] * 2;
    		    return true;
    		  }
    		  
    		  public boolean ComputeJacobian(double[] x,
                      double[][] jacobian) {
				jacobian[0][0] = x[0] * 2;
				return true;
			  }
    		  
    		  public boolean ComputeJacobian(double[] x, int x_offset, 
                      double[][] jacobian) {
				jacobian[0][0] = x[x_offset] * 2;
				return true;
			  }

    		  public int GlobalSize() { return 1; }
    		  public int LocalSize() { return 1; }
    		};
    		
    		public void ParameterBlockSetStateUpdatesLocalParameterizationJacobian() {
    			  // ParameterBlockSetStateUpdatesLocalParameterizationJacobian() passed all tests
    			  boolean passed = true;
    			  TestParameterization test_parameterization = new TestParameterization();
    			  double x[] = new double[]{ 1.0 };
    			  ParameterBlock parameter_block = new ParameterBlock(x, 1, -1, test_parameterization);

    			  if (parameter_block.LocalParameterizationJacobian()[0][0] != 2.0) {
    				  System.err.println("In ParameterBlockSetStateUpdatesLocalParameterizationJacobian() parameter_block.LocalParameterizationJacobian()[0][0] != 2.0");
    				  passed = false;
    			  }

    			  x[0] = 5.5;
    			  parameter_block.SetState(x,0);
    			  if (parameter_block.LocalParameterizationJacobian()[0][0] != 11.0) {
    				  System.err.println("In ParameterBlockSetStateUpdatesLocalParameterizationJacobian() parameter_block.LocalParameterizationJacobian()[0][0] != 11.0");
    				  passed = false;
    			  }
    			  if (passed) {
    				  System.out.println("ParameterBlockSetStateUpdatesLocalParameterizationJacobian() passed all tests");
    			  }
    			}
    		
    		public void ParameterBlockPlusWithNoLocalParameterization() {
    			  // ParameterBlockPlusWithNoLocalParameterization() passed all tests
    			  boolean passed = true;
    			  double x[] = new double[]{ 1.0, 2.0 };
    			  ParameterBlock parameter_block = new ParameterBlock(x, 2, -1);

    			  double delta[] = new double[]{ 0.2, 0.3 };
    			  double x_plus_delta[] = new double[2];
    			  parameter_block.Plus(x, delta, x_plus_delta);
    			  if (x_plus_delta[0] != 1.2) {
    				  System.err.println("In ParameterBlockPlusWithNoLocalParameterization() x_plus_delta[0] != 1.2");
    				  passed = false;
    			  }
    			  if (x_plus_delta[1] != 2.3) {
    				  System.err.println("In ParameterBlockPlusWithNoLocalParameterization() x_plus_delta[1] != 2.3");
    				  passed = false;
    			  }
    			  if (passed) {
    				  System.out.println("ParameterBlockPlusWithNoLocalParameterization() passed all tests");
    			  }
    			}
    		
    		// Stops computing the jacobian after the first time.
    		class BadLocalParameterization extends LocalParameterization {
    			private int calls_;
    		 public BadLocalParameterization() {
    			 super();
    		     calls_ = 0;
    		  }

    		  public boolean Plus(double[] x,
    		                    double[] delta,
    		                    double[] x_plus_delta) {
    			int i;
    			for (i = 0; i < x.length; i++) {
    		     x_plus_delta[i] = x[i] + delta[i];
    			}
    		    return true;
    		  }
    			
    			public boolean Plus(Vector<Double> x, int x_index,
	                    Vector<Double> delta, int delta_index,
	                    Vector<Double> x_plus_delta, int x_plus_delta_index) {
					int i;
					for (i = 0; i < x.size()-x_index; i++) {
				     x_plus_delta.set(x_plus_delta_index+i,x.get(x_index+i) + delta.get(delta_index+i));
					}
				    return true;
				  }

    		  public boolean ComputeJacobian(double[] x, double[] jacobian) {
    		    if (calls_ == 0) {
    		      jacobian[0] = 0;
    		    }
    		    ++calls_;
    		    return true;
    		  }
    		  
    		  public boolean ComputeJacobian(double[] x, double[][] jacobian) {
      		    if (calls_ == 0) {
      		      jacobian[0][0] = 0;
      		    }
      		    ++calls_;
      		    return true;
      		  }
    		  
    		  public boolean ComputeJacobian(double[] x, int x_index, double[][] jacobian) {
        		    if (calls_ == 0) {
        		      jacobian[0][0] = 0;
        		    }
        		    ++calls_;
        		    return true;
        		  }

    		  public int GlobalSize() { return 1;}
    		  public int LocalSize()  { return 1;}

    		 
    		};

    		public void ParameterBlockDetectBadLocalParameterization() {
    			  // ParameterBlockDetectBadLocalParameterization() passed all tests
    			  double x[] = new double[] {1};
    			  BadLocalParameterization bad_parameterization = new BadLocalParameterization();
    			  ParameterBlock parameter_block = new ParameterBlock(x, 1, -1, bad_parameterization);
    			  double y[] = new double[] {2};
    			  if (parameter_block.SetState(y, 0)) {
    				  System.err.println("In ParameterBlockDetectBadLocalParameterization() parameter_block.SetState(y, 0) = true");
    			  }
    			  else {
    				  System.out.println("ParameterBlockDetectBadLocalParameterization() passed all tests");
    			  }
    			}

    		public void ParameterBlockDefaultBounds() {
    			  // ParameterBlockDefaultBounds() passed all tests
    			  boolean passed = true;
    			  double x[] = new double[2];
    			  ParameterBlock parameter_block = new ParameterBlock(x, 2, -1, null);
    			  if (parameter_block.UpperBoundForParameter(0) != Double.MAX_VALUE) {
    				  System.err.println("In ParameterBlockDefaultBounds() parameter_block.UpperBoundForParameter(0) != Double.MAX_VALUE");
    				  passed = false;
    			  }
    			  if (parameter_block.UpperBoundForParameter(1) != Double.MAX_VALUE) {
    				  System.err.println("In ParameterBlockDefaultBounds() parameter_block.UpperBoundForParameter(1) != Double.MAX_VALUE");
    				  passed = false;
    			  }
    			  if (parameter_block.LowerBoundForParameter(0) != -Double.MAX_VALUE) {
    				  System.err.println("In ParameterBlockDefaultBounds() parameter_block.LowerBoundForParameter(0) != -Double.MAX_VALUE");
    				  passed = false;
    			  }
    			  if (parameter_block.LowerBoundForParameter(1) != -Double.MAX_VALUE) {
    				  System.err.println("In ParameterBlockDefaultBounds() parameter_block.LowerBoundForParameter(1) != -Double.MAX_VALUE");
    				  passed = false;
    			  }
    			  if (passed) {
    				  System.out.println("ParameterBlockDefaultBounds() passed all tests");
    			  }
    			}

    		public void ParameterBlockSetBounds() {
    			  // ParameterBlockSetBounds() passed all tests
    			  boolean passed = true;
    			  double x[] = new double[2];
    			  ParameterBlock parameter_block = new ParameterBlock(x, 2, -1, null);
    			  parameter_block.SetLowerBound(0, 1);
    			  parameter_block.SetUpperBound(1, 1);

    			  if (parameter_block.LowerBoundForParameter(0) != 1.0) {
    				  System.err.println("In ParameterBlockSetBounds() parameter_block.LowerBoundForParameter(0) != 1.0");
    				  passed = false;
    			  }
    			  if (parameter_block.LowerBoundForParameter(1) != -Double.MAX_VALUE) {
    				  System.err.println("In ParameterBlockSetBounds() parameter_block.LowerBoundForParameter(1) != -Double.MAX_VALUE");
    				  passed = false;
    			  }

    			  if (parameter_block.UpperBoundForParameter(0) != Double.MAX_VALUE) {
    				  System.err.println("In ParameterBlockSetBounds() parameter_block.UpperBoundForParameter(0) != Double.MAX_VALUE");
    				  passed = false;
    			  }
    			  if (parameter_block.UpperBoundForParameter(1) != 1.0) {
    				  System.err.println("In ParameterBlockSetBounds() parameter_block.UpperBoundForParameter(1) != 1.0");
    				  passed = false;
    			  }
    			  if (passed) {
    				  System.out.println("ParameterBlockSetBounds() passed all tests");
    			  }
    			}

    		public void ParameterBlockPlusWithBoundsConstraints() {
    			  // ParameterBlockPlusWithBoundsConstraints() passed all tests
    			  boolean passed = true;
    			  double x[] = new double[]{1.0, 0.0};
    			  double delta[] = new double[]{2.0, -10.0};
    			  ParameterBlock parameter_block = new ParameterBlock(x, 2, -1, null);
    			  parameter_block.SetUpperBound(0, 2.0);
    			  parameter_block.SetLowerBound(1, -1.0);
    			  double x_plus_delta[] = new double[2];
    			  parameter_block.Plus(x, delta, x_plus_delta);
    			  if (x_plus_delta[0] != 2.0) {
    				  System.err.println("In ParameterBlockPlusWithBoundsConstraints() x_plus_delta[0] != 2.0");
    				  passed = false;
    			  }
    			  if (x_plus_delta[1] != -1.0) {
    				  System.err.println("In ParameterBlockPlusWithBoundsConstraints() x_plus_delta[1] != -1.0");
    				  passed = false;
    			  }
    			  if (passed) {
    				  System.out.println("ParameterBlockPlusWithBoundsConstraints() passed all tests");
    			  }
    			}
    		
    		public void GraphEmptyGraph() {
    			  // GraphEmptyGraph() passed all tests
    			  Graph<Integer> graph = new Graph<Integer>();
    			  if (graph.vertices().size() != 0) {
    				  System.err.println("In GraphEmptyGraph() graph.vertices().size() != 0");
    			  }
    			  else {
    				  System.out.println("GraphEmptyGraph() passed all tests");
    			  }
    			}

    		public void GraphAddVertexAndEdge() {
    			  // GraphAddVertexAndEdge() passed all tests
    			  boolean passed = true;
    			  Graph<Integer> graph = new Graph<Integer>();
    			  graph.AddVertex(0);
    			  graph.AddVertex(1);
    			  graph.AddEdge(0, 1);

    			  final HashSet<Integer> vertices = graph.vertices();
    			  if (vertices.size() != 2) {
    				  System.err.println("In GraphAddVertexAndEdge() vertices.size() != 2");
    				  passed = false;
    			  }
    			  if (graph.Neighbors(0).size() != 1) {
    				  System.err.println("In GraphAddVertexAndEdge() graph.Neighbors(0).size() != 1");
    				  passed = false;
    			  }
    			  if (graph.Neighbors(1).size() != 1) {
    				  System.err.println("In GraphAddVertexAndEdge() graph.Neighbors(1).size() != 1");
    				  passed = false;
    			  }
    			  if (passed) {
    				  System.out.println("GraphAddVertexAndEdge() passed all tests");
    			  }
    			}
    		
    		public void GraphAddVertexIdempotence() {
    			  // GraphAddVertexIdempotence() passed all tests
    			  boolean passed = true;
    			  Graph<Integer> graph = new Graph<Integer>();
    			  graph.AddVertex(0);
    			  graph.AddVertex(1);
    			  graph.AddEdge(0, 1);

    			  final HashSet<Integer> vertices = graph.vertices();

    			  if (vertices.size() != 2) {
    				  System.err.println("In GraphAddVertexIdempotence() vertices.size() != 2");
    				  passed = false;
    			  }

    			  // Try adding the vertex again with a new weight.
    			  graph.AddVertex(0);
    			  if (vertices.size() != 2) {
    				  System.err.println("In GraphAddVertexIdempotence() vertices.size() != 2");
    				  passed = false;
    			  }

    			  // Rest of the graph remains the same.
    			  if (graph.Neighbors(0).size() != 1) {
    				  System.err.println("In GraphAddVertexIdempotence() graph.Neighbors(0).size() != 1");
    				  passed = false;
    			  }
    			  if (graph.Neighbors(1).size() != 1) {
    				  System.err.println("In GraphAddVertexIdempotence() graph.Neighbors(1).size() != 1");
    				  passed = false;
    			  }
    			  if (passed) {
    				  System.out.println("GraphAddVertexIdempotence() passed all tests");
    			  }
    			}

    		public void GraphDieOnNonExistentVertex() {
    			  // Gives expected error message:
    			  // In public HashSet<Vertex> Neighbors HashMap edges_ does not have key vertex
    			  Graph<Integer> graph = new Graph<Integer>();
    			  graph.AddVertex(0);
    			  graph.AddVertex(1);
    			  graph.AddEdge(0, 1);

    			  //EXPECT_DEATH_IF_SUPPORTED(graph.Neighbors(2), "key not found");
    			  graph.Neighbors(2);
    	    }
    		
    		public void WeightedGraphEmptyGraph() {
    			  // WeightedGraphEmptyGraph() passed all tests
    			  WeightedGraph<Integer> graph = new WeightedGraph<Integer>();
    			  if (graph.vertices().size() != 0) {
    				  System.err.println("In WeightedGraphEmptyGraph() graph.vertices().size() != 0");
    			  }
    			  else {
    				  System.out.println("WeightedGraphEmptyGraph() passed all tests");
    			  }
    			}
    		
    		public void WeightedGraphAddVertexAndEdge() {
    			  // WeightedGraphAddVertexAndEdge() passed all tests
    			  boolean passed = true;
    			  WeightedGraph<Integer> graph = new WeightedGraph<Integer>();
    			  graph.AddVertex(0, 1.0);
    			  graph.AddVertex(1, 2.0);
    			  graph.AddEdge(0, 1, 0.5);

    			  final HashSet<Integer> vertices = graph.vertices();
    			  if (vertices.size() != 2) {
    				  System.err.println("In WeightedGraphAddVertexAndEdge() vertices.size() != 2");
    				  passed = false;
    			  }
    			  if (graph.VertexWeight(0) != 1.0) {
    				  System.err.println("In WeightedGraphAddVertexAndEdge() graph.VertexWeight(0) != 1.0");
    				  passed = false;
    			  }
    			  if (graph.VertexWeight(1) != 2.0) {
    				  System.err.println("In WeightedGraphAddVertexAndEdge() graph.VertexWeight(1) != 2.0");
    				  passed = false;
    			  }
    			  if (graph.Neighbors(0).size() != 1) {
    				  System.err.println("In WeightedGraphAddVertexAndEdge() graph.Neighbors(0).size() != 1");
    				  passed = false;
    			  }
    			  if (graph.Neighbors(1).size() != 1) {
    				  System.err.println("In WeightedGraphAddVertexAndEdge() graph.Neighbors(1).size() != 1");
    				  passed = false;
    			  }
    			  if (graph.EdgeWeight(0, 1) != 0.5) {
    				  System.err.println("In WeightedGraphAddVertexAndEdge() graph.EdgeWeight(0, 1) != 0.5");
    				  passed = false;
    			  }
    			  if (graph.EdgeWeight(1, 0) != 0.5) {
    				  System.err.println("In WeightedGraphAddVertexAndEdge() graph.EdgeWeight(1, 0) != 0.5");
    				  passed = false;
    			  }
    			  if (passed) {
    				  System.out.println("WeightedGraphAddVertexAndEdge() passed all tests");
    			  }
    			}

    		public void WeightedGraphAddVertexIdempotence() {
    			  // WeightedGraphAddVertexIdempotence() passed all tests
    			  boolean passed = true;
    			  WeightedGraph<Integer> graph = new WeightedGraph();
    			  graph.AddVertex(0, 1.0);
    			  graph.AddVertex(1, 2.0);
    			  graph.AddEdge(0, 1, 0.5);

    			  final HashSet<Integer> vertices = graph.vertices();

    			  if (vertices.size() != 2) {
    				  System.err.println("In WeightedGraphAddVertexIdempotence() vertices.size() != 2");
    				  passed = false;
    			  }

    			  // Try adding the vertex again with a new weight.
    			  graph.AddVertex(0, 3.0);
    			  if (vertices.size() != 2) {
    				  System.err.println("In WeightedGraphAddVertexIdempotence() vertices.size() != 2");
    				  passed = false;
    			  }

    			  // The vertex weight is reset.
    			  if (graph.VertexWeight(0) != 3.0) {
    				  System.err.println("In WeightedGraphAddVertexIdempotence() graph.VertexWeight(0) != 3.0");
    				  passed = false;
    			  }

    			  // Rest of the graph remains the same.
    			  if (graph.VertexWeight(1) != 2.0) {
    				  System.err.println("In WeightedGraphAddVertexIdempotence() graph.VertexWeight(1) != 2.0");
    				  passed = false;
    			  }
    			  if (graph.Neighbors(0).size() != 1) {
    				  System.err.println("In WeightedGraphAddVertexIdempotence() graph.Neighbors(0).size() != 1");
    				  passed = false;
    			  }
    			  if (graph.Neighbors(1).size() != 1) {
    				  System.err.println("In WeightedGraphAddVertexIdempotence() graph.Neighbors(1).size() != 1");
    				  passed = false;
    			  }
    			  if (graph.EdgeWeight(0, 1) != 0.5) {
    				  System.err.println("In WeightedGraphAddVertexIdempotence() graph.EdgeWeight(0, 1) != 0.5");
    				  passed = false;
    			  }
    			  if (graph.EdgeWeight(1, 0) != 0.5) {
    				  System.err.println("In WeightedGraphAddVertexIdempotence() graph.EdgeWeight(1, 0) != 0.5");
    				  passed = false;
    			  }
    			  if (passed) {
    				  System.out.println("WeightedGraphAddVertexIdempotence() passed all tests");
    			  }   			  
    			}

    		public void WeightedGraphDieOnNonExistentVertex() {
    			  // Gives expected error messages:
    			  // !vertex_weights_ containsKey(vertex) in WeightedGraph.VertexWeight
    			  // In WeightedGraph Neighbors edges_ does not contain key vertex
    			  WeightedGraph<Integer> graph = new WeightedGraph<Integer>();
    			  graph.AddVertex(0, 1.0);
    			  graph.AddVertex(1, 2.0);
    			  graph.AddEdge(0, 1, 0.5);

    			  //EXPECT_DEATH_IF_SUPPORTED(graph.VertexWeight(2), "key not found");
    			  //EXPECT_DEATH_IF_SUPPORTED(graph.Neighbors(2), "key not found");
    			  graph.VertexWeight(2);
    			  graph.Neighbors(2);
    			}
    		
    		public void WeightedGraphNonExistentEdge() {
    			  // WeightedGraphNonExistentEdge() passed all tests
    			  WeightedGraph<Integer> graph = new WeightedGraph<Integer>();
    			  graph.AddVertex(0, 1.0);
    			  graph.AddVertex(1, 2.0);
    			  graph.AddEdge(0, 1, 0.5);

    			  // Default value for non-existent edges is 0.
    			  if (graph.EdgeWeight(2, 3) != 0) {
    				  System.err.println("In WeightedGraphNonExistentEdge() graph.EdgeWeight(2, 3) != 0");
    			  }
    			  else {
    				  System.out.println("WeightedGraphNonExistentEdge() passed all tests");
    			  }
    		}
    		
    		// Cannot do evaluation_callback_test.cc because Java does not allow the multiple inheritance used:
    		// Generally multiple inheritance is a terrible idea, but in this (test)
    		// case it makes for a relatively elegant test implementation.
    		//struct WigglyBowlCostFunctionAndEvaluationCallback :
    		      //SizedCostFunction<2, 2>,
    		      //EvaluationCallback 
    		
    		// The following three classes are for the purposes of defining
    		// function signatures. They have dummy Evaluate functions.

    		// Trivial cost function that accepts a single argument.
    		class UnaryCostFunction extends CostFunction {
    		 public UnaryCostFunction(int num_residuals, int parameter_block_size) {
    			super();
    		    set_num_residuals(num_residuals);
    		    mutable_parameter_block_sizes().add(parameter_block_size);
    		  }

    		 public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
    		    for (int i = 0; i < num_residuals(); ++i) {
    		      residuals[i] = 1;
    		    }
    		    return true;
    		  }
    		 
    		 public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
     		    for (int i = 0; i < num_residuals(); ++i) {
     		      residuals[i] = 1;
     		    }
     		    return true;
     		  }
    		};
    		
    		// Trivial cost function that accepts two arguments.
    		class BinaryCostFunction extends CostFunction {
    		 public BinaryCostFunction(int num_residuals,
    		                     int parameter_block1_size,
    		                     int parameter_block2_size) {
    			super();
    		    set_num_residuals(num_residuals);
    		    mutable_parameter_block_sizes().add(parameter_block1_size);
    		    mutable_parameter_block_sizes().add(parameter_block2_size);
    		  }

    		 public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
    		    for (int i = 0; i < num_residuals(); ++i) {
    		      residuals[i] = 2;
    		    }
    		    return true;
    		  }
    		 
    		 public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
     		    for (int i = 0; i < num_residuals(); ++i) {
     		      residuals[i] = 2;
     		    }
     		    return true;
     		  }
    		};
    		
    		// Trivial cost function that accepts three arguments.
    		class TernaryCostFunction2 extends CostFunction {
    		 public TernaryCostFunction2(int num_residuals,
    		                      int parameter_block1_size,
    		                      int parameter_block2_size,
    		                      int parameter_block3_size) {
    			super();
    		    set_num_residuals(num_residuals);
    		    mutable_parameter_block_sizes().add(parameter_block1_size);
    		    mutable_parameter_block_sizes().add(parameter_block2_size);
    		    mutable_parameter_block_sizes().add(parameter_block3_size);
    		  }

    		 public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
    		    for (int i = 0; i < num_residuals(); ++i) {
    		      residuals[i] = 3;
    		    }
    		    return true;
    		  }
    		 
    		 public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
     		    for (int i = 0; i < num_residuals(); ++i) {
     		      residuals[i] = 3;
     		    }
     		    return true;
     		  }
    		};

    		public void ProblemAddResidualWithNullCostFunctionDies() {
    			  // Gives expected error message:
    			  // cost_function is null in AddResidualBlock
    			  double x[] = new double[3];
    			  double y[] = new double[4];
    			  double z[] = new double[5];

    			  ProblemImpl problem = new ProblemImpl();
    			  problem.AddParameterBlock(x, 3);
    			  problem.AddParameterBlock(y, 4);
    			  problem.AddParameterBlock(z, 5);

    			  //EXPECT_DEATH_IF_SUPPORTED(problem.AddResidualBlock(NULL, NULL, x),
    			                            //"'cost_function' Must be non NULL");
    			  problem.AddResidualBlock(null, null, x);
    			}

    		public void ProblemAddResidualWithIncorrectNumberOfParameterBlocksDies() {
    			 // Gives expected error message:
    			 // parameter_blocks.size() != cost_function.parameter_block_sizes().size() in AddResidualBlock
    		     // parameter_blocks_size() = 2
    		     // cost_function.parameter_block_sizes().size() = 1
    			 double x[] = new double[3];
   			     double y[] = new double[4];
   			     double z[] = new double[5];

   			     ProblemImpl problem = new ProblemImpl();
   			     problem.AddParameterBlock(x, 3);
   			     problem.AddParameterBlock(y, 4);
   			     problem.AddParameterBlock(z, 5);

    			  // UnaryCostFunction takes only one parameter, but two are passed.
    			  //EXPECT_DEATH_IF_SUPPORTED(
    			      problem.AddResidualBlock(new UnaryCostFunction(2, 3), null, x, y);
    			      //"parameter_blocks.size");
    			}

    		public void ProblemAddResidualWithDifferentSizesOnTheSameVariableDies() {
    			  // Gives expected error message:
    			  // Tried adding a parameter block with the same double[], 
    			  //  twice, but with different block sizes. Original 
    			  // size was 3 but new size is 4
    			  double x[] = new double[3];

    			  ProblemImpl problem = new ProblemImpl();
    			  problem.AddResidualBlock(new UnaryCostFunction(2, 3), null, x);
    			  //EXPECT_DEATH_IF_SUPPORTED(problem.AddResidualBlock(
    			                                //new UnaryCostFunction(
    			                                   // 2, 4 /* 4 != 3 */), NULL, x),
    			                            //"different block sizes");
    			  problem.AddResidualBlock(new UnaryCostFunction(2, 4), null, x);
    			}
    		
    		public void ProblemAddResidualWithDuplicateParametersDies() {
    			  // Gives expected error messages:
    			  // Duplicate parameter blocks in a residual parameter are not allowed.
    			  // Parameter blocks 0 and 1 are duplicates.
    			  // Duplicate parameter blocks in a residual parameter are not allowed.
    			  // Parameter blocks 0 and 2 are duplicates.

    			  double x[] = new double[3];
    			  double z[] = new double[5];

    			  ProblemImpl problem = new ProblemImpl();
    			  //EXPECT_DEATH_IF_SUPPORTED(problem.AddResidualBlock(
    			                                //new BinaryCostFunction(2, 3, 3), NULL, x, x),
    			                            //"Duplicate parameter blocks");
    			  problem.AddResidualBlock(new BinaryCostFunction(2, 3, 3), null, x, x);
    			  //EXPECT_DEATH_IF_SUPPORTED(problem.AddResidualBlock(
    			                                //new TernaryCostFunction(1, 5, 3, 5),
    			                                //NULL, z, x, z),
    			                            //"Duplicate parameter blocks");
    			  problem.AddResidualBlock(new TernaryCostFunction2(1, 5, 3, 5), null, z, x, z);
    			}
    		
    		public void ProblemAddResidualWithIncorrectSizesOfParameterBlockDies() {
    			 // Gives expected error message:
    			 // Tried adding a parameter block with the same double[], 
    			 // twice, but with different block sizes. Original 
    			 // size was 5 but new size is 4
    			 double x[] = new double[3];
  			     double y[] = new double[4];
  			     double z[] = new double[5];

  			     ProblemImpl problem = new ProblemImpl();
  			     problem.AddParameterBlock(x, 3);
  			     problem.AddParameterBlock(y, 4);
  			     problem.AddParameterBlock(z, 5);
    			  
    			  // The cost function expects the size of the second parameter, z, to be 4
    			  // instead of 5 as declared above. This is fatal.
    			  //EXPECT_DEATH_IF_SUPPORTED(problem.AddResidualBlock(
    			      //new BinaryCostFunction(2, 3, 4), NULL, x, z),
    			               //"different block sizes");
  			     problem.AddResidualBlock(new BinaryCostFunction(2, 3, 4), null, x, z);
    			}

    		public void ProblemAddResidualAddsDuplicatedParametersOnlyOnce() {
    			  // ProblemAddResidualAddsDuplicatedParametersOnlyOnce() passed all tests
    			  boolean passed = true;
    			  double x[] = new double[3];
    			  double y[] = new double[4];
    			  double z[] = new double[5];

    			  ProblemImpl problem = new ProblemImpl();
    			  problem.AddResidualBlock(new UnaryCostFunction(2, 3), null, x);
    			  problem.AddResidualBlock(new UnaryCostFunction(2, 3), null, x);
    			  problem.AddResidualBlock(new UnaryCostFunction(2, 4), null, y);
    			  problem.AddResidualBlock(new UnaryCostFunction(2, 5), null, z);

    			  if (problem.NumParameterBlocks() != 3) {
    				  System.err.println("In ProblemAddResidualAddsDuplicatedParametersOnlyOnce() problem.NumParameterBlocks() != 3");
    				  passed = false;
    			  }
    			  if (problem.NumParameters() != 12) {
    				  System.err.println("In ProblemAddResidualAddsDuplicatedParametersOnlyOnce() problem.NumParameters() != 12");
    				  passed = false;
    			  }
    			  if (passed) {
    				  System.out.println("ProblemAddResidualAddsDuplicatedParametersOnlyOnce() passed all tests");
    			  }
    			}

    		public void ProblemAddParameterWithDifferentSizesOnTheSameVariableDies() {
    			  // Gives expected error message:
    			  // Tried adding a parameter block with the same double[], 
    			  // twice, but with different block sizes. Original 
    			  // size was 3 but new size is 4

    			  double x[] = new double[3];
    			  double y[] = new double[4];

    			  ProblemImpl problem = new ProblemImpl();
    			  problem.AddParameterBlock(x, 3);
    			  problem.AddParameterBlock(y, 4);

    			  // EXPECT_DEATH_IF_SUPPORTED(problem.AddParameterBlock(x, 4),
    			                            //"different block sizes");
    			  problem.AddParameterBlock(x, 4);
    			}
    		
    		public double []IntToPtr(int i) {
    	        return new double[i];
    		}
    		
    		/*public void ProblemAddParameterWithAliasedParametersDies() {
    			  // CheckForNoAliasing not implemented in Java because of memory address use
    			  // Layout is
    			  //
    			  //   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
    			  //                 [x] x  x  x  x          [y] y  y
    			  //         o==o==o                 o==o==o           o==o
    			  //               o--o--o     o--o--o     o--o  o--o--o
    			  //
    			  // Parameter block additions are tested as listed above; expected successful
    			  // ones marked with o==o and aliasing ones marked with o--o.

    			  ProblemImpl problem = new ProblemImpl();
    			  problem.AddParameterBlock(IntToPtr(5),  5);  // x
    			  problem.AddParameterBlock(IntToPtr(13), 3);  // y

    			  problem.AddParameterBlock(IntToPtr( 4), 2);
    			  problem.AddParameterBlock(IntToPtr( 4), 3);
    			  problem.AddParameterBlock(IntToPtr( 4), 9);
    			  problem.AddParameterBlock(IntToPtr( 8), 3);
    			  problem.AddParameterBlock(IntToPtr(12), 2);
    			  problem.AddParameterBlock(IntToPtr(14), 3);
    			  //EXPECT_DEATH_IF_SUPPORTED(problem.AddParameterBlock(IntToPtr( 4), 2),
    			                            //"Aliasing detected");
    			  //EXPECT_DEATH_IF_SUPPORTED(problem.AddParameterBlock(IntToPtr( 4), 3),
    			                            //"Aliasing detected");
    			  //EXPECT_DEATH_IF_SUPPORTED(problem.AddParameterBlock(IntToPtr( 4), 9),
    			                            //"Aliasing detected");
    			  //EXPECT_DEATH_IF_SUPPORTED(problem.AddParameterBlock(IntToPtr( 8), 3),
    			                            //"Aliasing detected");
    			  //EXPECT_DEATH_IF_SUPPORTED(problem.AddParameterBlock(IntToPtr(12), 2),
    			                            //"Aliasing detected");
    			  //EXPECT_DEATH_IF_SUPPORTED(problem.AddParameterBlock(IntToPtr(14), 3),
    			                            //"Aliasing detected");

    			  // These ones should work.
    			  problem.AddParameterBlock(IntToPtr( 2), 3);
    			  problem.AddParameterBlock(IntToPtr(10), 3);
    			  problem.AddParameterBlock(IntToPtr(16), 2);

    			  if (problem.NumParameterBlocks() != 5) {
    				  System.err.println("In ProblemAddParameterWithAliasedParametersDies() problem.NumParameterBlocks() != 5");
    			  }
    			  else {
    				  System.out.println("In ProblemAddParameterWithAliasedParametersDies() problem.NumParameterBlocks() == 5 as expected");
    			  }
    			}*/

    		public void ProblemAddParameterIgnoresDuplicateCalls() {
    			  // ProblemAddParameterIgnoresDuplicateCalls() passed all tests
    			  boolean passed = true;
    			  double x[] = new double[3];
    			  double y[] = new double[4];

    			  ProblemImpl problem = new ProblemImpl();
    			  problem.AddParameterBlock(x, 3);
    			  problem.AddParameterBlock(y, 4);

    			  // Creating parameter blocks multiple times is ignored.
    			  problem.AddParameterBlock(x, 3);
    			  problem.AddResidualBlock(new UnaryCostFunction(2, 3), null, x);

    			  // ... even repeatedly.
    			  problem.AddParameterBlock(x, 3);
    			  problem.AddResidualBlock(new UnaryCostFunction(2, 3), null, x);

    			  // More parameters are fine.
    			  problem.AddParameterBlock(y, 4);
    			  problem.AddResidualBlock(new UnaryCostFunction(2, 4), null, y);

    			  if (problem.NumParameterBlocks() != 2) {
    				  System.err.println("In ProblemAddParameterIgnoresDuplicateCalls() problem.NumParameterBlocks() != 2");
    				  passed = false;
    			  }
    			  if (problem.NumParameters() != 7) {
    				  System.err.println("In ProblemAddParameterIgnoresDuplicateCalls() problem.NumParameters() != 7");
    				  passed = false;
    			  }
    			  if (passed) {
    				  System.out.println("ProblemAddParameterIgnoresDuplicateCalls() passed all tests");
    			  }
    			}
    		
    		public void ProblemAddingParametersAndResidualsResultsInExpectedProblem() {
    			  // ProblemAddingParametersAndResidualsResultsInExpectedProblem() passed all tests
    			  boolean passed = true;
    			  double x[] = new double[3];
    			  double y[] = new double[4];
    			  double z[] = new double[5]; 
    			  double w[] = new double[4];

    			  ProblemImpl problem = new ProblemImpl();
    			  problem.AddParameterBlock(x, 3);
    			  if (1 != problem.NumParameterBlocks()) {
    				  System.err.println("In ProblemAddingParametersAndResidualsResultsInExpectedProblem() 1 != problem.NumParameterBlocks()");
    				  passed = false;
    			  }
    			  if (3 != problem.NumParameters()) {
    				  System.err.println("In ProblemAddingParametersAndResidualsResultsInExpectedProblem() 3 != problem.NumParameters()");
    				  passed = false;
    			  }

    			  problem.AddParameterBlock(y, 4);
    			  if (2 != problem.NumParameterBlocks()) {
    				  System.err.println("In ProblemAddingParametersAndResidualsResultsInExpectedProblem() 2 != problem.NumParameterBlocks()");
    				  passed = false;
    			  }
    			  if (7 != problem.NumParameters()) {
    				  System.err.println("In ProblemAddingParametersAndResidualsResultsInExpectedProblem() 7 != problem.NumParameters()");
    				  passed = false;
    			  }

    			  problem.AddParameterBlock(z, 5);
    			  if (3 != problem.NumParameterBlocks()) {
    				  System.err.println("In ProblemAddingParametersAndResidualsResultsInExpectedProblem() 3 != problem.NumParameterBlocks()");
    				  passed = false;
    			  }
    			  if (12 != problem.NumParameters()) {
    				  System.err.println("In ProblemAddingParametersAndResidualsResultsInExpectedProblem() 12 != problem.NumParameters()");
    				  passed = false;
    			  }

    			  // Add a parameter that has a local parameterization.
    			  w[0] = 1.0; w[1] = 0.0; w[2] = 0.0; w[3] = 0.0;
    			  problem.AddParameterBlock(w, 4, new QuaternionParameterization());
    			  if (4 != problem.NumParameterBlocks()) {
    				  System.err.println("In ProblemAddingParametersAndResidualsResultsInExpectedProblem() 4 != problem.NumParameterBlocks()");
    				  passed = false;
    			  }
    			  if (16 != problem.NumParameters()) {
    				  System.err.println("In ProblemAddingParametersAndResidualsResultsInExpectedProblem() 16 != problem.NumParameters()");
    				  passed = false;
    			  }

    			  problem.AddResidualBlock(new UnaryCostFunction(2, 3), null, x);
    			  problem.AddResidualBlock(new BinaryCostFunction(6, 5, 4) , null, z, y);
    			  problem.AddResidualBlock(new BinaryCostFunction(3, 3, 5), null, x, z);
    			  problem.AddResidualBlock(new BinaryCostFunction(7, 5, 3), null, z, x);
    			  problem.AddResidualBlock(new TernaryCostFunction2(1, 5, 3, 4), null, z, x, y);

    			  final int total_residuals = 2 + 6 + 3 + 7 + 1;
    			  if (problem.NumResidualBlocks() != 5) {
    				  System.err.println("In ProblemAddingParametersAndResidualsResultsInExpectedProblem() problem.NumResidualBlocks() != 5");
    				  passed = false;
    			  }
    			  if (problem.NumResiduals() != total_residuals) {
    				  System.err.println("In ProblemAddingParametersAndResidualsResultsInExpectedProblem() problem.NumResiduals() != total_residuals");
    				  passed = false;
    			  }
    			  if (passed) {
    				  System.out.println("ProblemAddingParametersAndResidualsResultsInExpectedProblem() passed all tests");
    			  }
    			}

    		class DestructorCountingCostFunction extends SizedCostFunction {
    			private int num_destructions_[];
    			 public DestructorCountingCostFunction(int num_destructions[]) {
    				  super(3, 4, 5, 0, 0, 0, 0, 0, 0, 0, 0);
    			      this.num_destructions_ = num_destructions; 
    			 }

    			  public void finalize() {
    			    num_destructions_[0] += 1;
    			  }
    			  
    			  public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
    				  return true;
    			  }
    			  
    			  public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
    				  return true;
    			  }

    			};

    			public void ProblemReusedCostFunctionsAreOnlyDeletedOnce() {
    				  // ProblemReusedCostFunctionsAreOnlyDeletedOnce() passed all tests
    				  boolean passed = true;
    				  double y[] = new double[4];
    				  double z[] = new double[5];
    				  int num_destructions[] = new int[] {0};

    				  // Add a cost function multiple times and check to make sure that
    				  // the destructor on the cost function is only called once.
    				  {
    				    ProblemImpl problem = new ProblemImpl();
    				    problem.AddParameterBlock(y, 4);
    				    problem.AddParameterBlock(z, 5);

    				    CostFunction cost = new DestructorCountingCostFunction(num_destructions);
    				    problem.AddResidualBlock(cost, null, y, z);
    				    problem.AddResidualBlock(cost, null, y, z);
    				    problem.AddResidualBlock(cost, null, y, z);
    				    if (3 !=  problem.NumResidualBlocks()) {
    				    	System.err.println("In ProblemReusedCostFunctionsAreOnlyDeletedOnce() 3 !=  problem.NumResidualBlocks()");
    				    	passed = false;
    				    }
    				    cost.finalize();
    				  }

    				  // Check that the destructor was called only once.
    				  if (num_destructions[0] != 1) {
    					  System.err.println("In ProblemReusedCostFunctionsAreOnlyDeletedOnce() num_destructions[0] != 1");
    					  passed = false;
    				  }
    				  if (passed) {
    					  System.out.println("ProblemReusedCostFunctionsAreOnlyDeletedOnce() passed all tests");
    				  }
    				}

    			public void ProblemGetCostFunctionForResidualBlock() {
    				  // ProblemGetCostFunctionForResidualBlock() passed all tests
    				  boolean passed = true;
    				  double x[] = new double[3];
    				  ProblemImpl problem = new ProblemImpl();
    				  CostFunction cost_function = new UnaryCostFunction(2, 3);
    				  final ResidualBlock residual_block =
    				      problem.AddResidualBlock(cost_function, null, x);
    				  if (problem.GetCostFunctionForResidualBlock(residual_block) != cost_function) {
    					  System.err.println("In ProblemGetCostFunctionForResidualBlock() problem.GetCostFunctionForResidualBlock(residual_block) != cost_function");
    					  passed = false;
    				  }
    				  if (problem.GetLossFunctionForResidualBlock(residual_block) != null) {
    					  System.err.println("In ProblemGetCostFunctionForResidualBlock() problem.GetLossFunctionForResidualBlock(residual_block) != null");
    					  passed = false;
    				  }
    				  if (passed) {
    					  System.out.println("ProblemGetCostFunctionForResidualBlock() passed all tests");
    				  }
    				}
    			
    			public void ProblemGetLossFunctionForResidualBlock() {
    				  // ProblemGetLossFunctionForResidualBlock() passed all tests
    				  boolean passed = true;
    				  double x[] = new double[3];
    				  ProblemImpl problem = new ProblemImpl();
    				  CostFunction cost_function = new UnaryCostFunction(2, 3);
    				  LossFunction loss_function = new TrivialLoss();
    				  final ResidualBlock residual_block =
    				      problem.AddResidualBlock(cost_function, loss_function, x);
    				  if (problem.GetCostFunctionForResidualBlock(residual_block) != cost_function) {
    					  System.err.println("In ProblemGetLossFunctionForResidualBlock() problem.GetCostFunctionForResidualBlock(residual_block) != cost_function");
    					  passed = false;
    				  }
    				  if (problem.GetLossFunctionForResidualBlock(residual_block) != loss_function) {
    					  System.err.println("In ProblemGetLossFunctionForResidualBlock() problem.GetLossFunctionForResidualBlock(residual_block) != loss_function");
    					  passed = false;
    				  }
    				  if (passed) {
    					  System.out.println("ProblemGetLossFunctionForResidualBlock() passed all tests");
    				  }
    				}


    			public void ProblemCostFunctionsAreDeletedEvenWithRemovals() {
    				  // ProblemCostFunctionsAreDeletedEvenWithRemovals() passed all tests
    				  boolean passed = true;
    				  double y[] = new double[4];
    				  double z[] = new double[5];
    				  double w[] = new double[4];
    				  int num_destructions[] = new int[] {0};
    				  {
    				    ProblemImpl problem = new ProblemImpl();
    				    problem.AddParameterBlock(y, 4);
    				    problem.AddParameterBlock(z, 5);

    				    CostFunction cost_yz =
    				        new DestructorCountingCostFunction(num_destructions);
    				    CostFunction cost_wz =
    				        new DestructorCountingCostFunction(num_destructions);
    				    ResidualBlock r_yz = problem.AddResidualBlock(cost_yz, null, y, z);
    				    ResidualBlock r_wz = problem.AddResidualBlock(cost_wz, null, w, z);
    				    if (2 != problem.NumResidualBlocks()) {
    				    	System.err.println("In ProblemCostFunctionsAreDeletedEvenWithRemovals() 2 != problem.NumResidualBlocks()");
    				    	passed = false;
    				    }
    				    problem.RemoveResidualBlock(r_yz);
    				    if (num_destructions[0] != 1) {
    				    	System.err.println("In ProblemCostFunctionsAreDeletedEvenWithRemovals() num_destructions[0] != 1");
    				    	passed = false;
    				    }
    				    problem.RemoveResidualBlock(r_wz);
    				    if (num_destructions[0] != 2) {
    				    	System.err.println("In ProblemCostFunctionsAreDeletedEvenWithRemovals() num_destructions[0] != 2");
    				    	passed = false;
    				    }

    				    if (0 != problem.NumResidualBlocks()) {
    				    	System.err.println("In ProblemCostFunctionsAreDeletedEvenWithRemovals() 0 != problem.NumResidualBlocks()");
    				    	passed = false;
    				    }
    				  }
    				  if (num_destructions[0] != 2) {
  				    	System.err.println("In ProblemCostFunctionsAreDeletedEvenWithRemovals() num_destructions[0] != 2");
  				    	passed = false;
  				      }
    				  if (passed) {
    					  System.out.println("ProblemCostFunctionsAreDeletedEvenWithRemovals() passed all tests");
    				  }
    				}
    			
    			// Make the dynamic problem tests (e.g. for removing residual blocks)
    			// parameterized on whether the low-latency mode is enabled or not.
    			//
    			// This tests against ProblemImpl instead of Problem in order to inspect the
    			// state of the resulting Program; this is difficult with only the thin Problem
    			// interface.
    			class DynamicProblem {
    				ProblemImpl problem;
      			    double y[] = new double[4];
      			    double z[] = new double[5];
      			    double w[] = new double[3];
      			    boolean param;
      			    boolean passed = true;
      			    String testName;
    			    public DynamicProblem(boolean param, String testName) {
    			    	this.param = param;
    			    	this.testName = testName;
    			        ProblemOptions options = new ProblemOptions();
    			        options.enable_fast_removal = param;
    			        problem = new ProblemImpl(options);
    			  }

    			  public ParameterBlock GetParameterBlock(int block) {
    			    return problem.program().parameter_blocks().get(block);
    			  }
    			  public ResidualBlock GetResidualBlock(int block) {
    			    return problem.program().residual_blocks().get(block);
    			  }

    			  public boolean HasResidualBlock(ResidualBlock residual_block) {
    			    boolean have_residual_block = true;
    			    if (param) {
    			      have_residual_block &=
    			          (problem.residual_block_set().contains(residual_block));
    			    }
    			    have_residual_block &=
    			        (problem.program().residual_blocks().contains(residual_block));
    			    return have_residual_block;
    			  }

    			  public int NumResidualBlocks() {
    			    // Verify that the hash set of residuals is maintained consistently.
    			    if (param) {
    			      if (problem.residual_block_set().size() != problem.NumResidualBlocks()) {
    			    	  System.err.println("In " + testName + " problem.residual_block_set().size() != problem.NumResidualBlocks()");
    			    	  passed = false;
    			      }
    			    }
    			    return problem.NumResidualBlocks();
    			  }

    			  // The next block of functions until the end are only for testing the
    			  // residual block removals.
    			  public void ExpectParameterBlockContainsResidualBlock(
    			      double[] values,
    			      ResidualBlock residual_block) {
    				  if (!problem.parameter_map().containsKey(values)) {
    					  System.err.println("In " + testName + " problem.parameter_map().containsKey(values) = false");
    					  passed = false;
    					  return;
    				  }
					  ParameterBlock parameter_block = problem.parameter_map().get(values);
					  if (!parameter_block.mutable_residual_blocks().contains(residual_block)) {
						  System.err.println("In " + testName + " parameter_block.mutable_residual_blocks().contains(residual_block) = false");
    					  passed = false;
    					  return;
					  }
    			  }

    			  public void ExpectSize(double[] values, int size) {
    				  if (!problem.parameter_map().containsKey(values)) {
    					  System.err.println("In " + testName + " problem.parameter_map().containsKey(values) = false");
    					  passed = false;
    					  return;
    				  }
    				  ParameterBlock parameter_block = problem.parameter_map().get(values);
    			      if (size != parameter_block.mutable_residual_blocks().size()) {
    			    	  System.err.println("In " + testName + " size != parameter_block.mutable_residual_blocks().size()");
    					  passed = false;
    					  return;
    			      }
    			  }

    			  // Degenerate case.
    			  public void ExpectParameterBlockContains(double[] values) {
    			    ExpectSize(values, 0);
    			  }

    			  public void ExpectParameterBlockContains(double[] values,
    			                                    ResidualBlock r1) {
    			    ExpectSize(values, 1);
    			    ExpectParameterBlockContainsResidualBlock(values, r1);
    			  }

    			  public void ExpectParameterBlockContains(double[] values,
    			                                    ResidualBlock r1,
    			                                    ResidualBlock r2) {
    			    ExpectSize(values, 2);
    			    ExpectParameterBlockContainsResidualBlock(values, r1);
    			    ExpectParameterBlockContainsResidualBlock(values, r2);
    			  }

    			  public void ExpectParameterBlockContains(double[] values,
    			                                    ResidualBlock r1,
    			                                    ResidualBlock r2,
    			                                    ResidualBlock r3) {
    			    ExpectSize(values, 3);
    			    ExpectParameterBlockContainsResidualBlock(values, r1);
    			    ExpectParameterBlockContainsResidualBlock(values, r2);
    			    ExpectParameterBlockContainsResidualBlock(values, r3);
    			  }

    			  public void ExpectParameterBlockContains(double[] values,
    			                                    ResidualBlock r1,
    			                                    ResidualBlock r2,
    			                                    ResidualBlock r3,
    			                                    ResidualBlock r4) {
    			    ExpectSize(values, 4);
    			    ExpectParameterBlockContainsResidualBlock(values, r1);
    			    ExpectParameterBlockContainsResidualBlock(values, r2);
    			    ExpectParameterBlockContainsResidualBlock(values, r3);
    			    ExpectParameterBlockContainsResidualBlock(values, r4);
    			  }
    			  
    			  public void DynamicProblemRemoveParameterBlockWithNoResiduals() {
    				  problem.AddParameterBlock(y, 4);
    				  problem.AddParameterBlock(z, 5);
    				  problem.AddParameterBlock(w, 3);
    				  if (3 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (0 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 0 != NumResidualBlocks()");
    					  passed = false;
    				  }
    				  if (y != GetParameterBlock(0).user_state()) {
    					  System.err.println("In " + testName + " y != GetParameterBlock(0).user_state()");
    					  passed = false;
    				  }
    				  if (z != GetParameterBlock(1).user_state()) {
    					  System.err.println("In " + testName + " z != GetParameterBlock(1).user_state()");
    					  passed = false; 
    				  }
    				  if (w != GetParameterBlock(2).user_state()) {
    					  System.err.println("In " + testName + " w != GetParameterBlock(2).user_state()");
    					  passed = false;
    				  }

    				  // w is at the end, which might break the swapping logic so try adding and
    				  // removing it.
    				  problem.RemoveParameterBlock(w);
    				  if (2 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 2 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (0 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 0 != NumResidualBlocks()");
    					  passed = false;
    				  }
    				  if (y != GetParameterBlock(0).user_state()) {
    					  System.err.println("In " + testName + " y != GetParameterBlock(0).user_state()");
    					  passed = false;
    				  }
    				  if (z != GetParameterBlock(1).user_state()) {
    					  System.err.println("In " + testName + " z != GetParameterBlock(1).user_state()");
    					  passed = false; 
    				  }
    				  problem.AddParameterBlock(w, 3);
    				  if (3 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (0 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 0 != NumResidualBlocks()");
    					  passed = false;
    				  }
    				  if (y != GetParameterBlock(0).user_state()) {
    					  System.err.println("In " + testName + " y != GetParameterBlock(0).user_state()");
    					  passed = false;
    				  }
    				  if (z != GetParameterBlock(1).user_state()) {
    					  System.err.println("In " + testName + " z != GetParameterBlock(1).user_state()");
    					  passed = false; 
    				  }
    				  if (w != GetParameterBlock(2).user_state()) {
    					  System.err.println("In " + testName + " w != GetParameterBlock(2).user_state()");
    					  passed = false;
    				  }

    				  // Now remove z, which is in the middle, and add it back.
    				  problem.RemoveParameterBlock(z);
    				  if (2 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 2 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (0 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 0 != NumResidualBlocks()");
    					  passed = false;
    				  }
    				  if (y != GetParameterBlock(0).user_state()) {
    					  System.err.println("In " + testName + " y != GetParameterBlock(0).user_state()");
    					  passed = false;
    				  }
    				  if (w != GetParameterBlock(1).user_state()) {
    					  System.err.println("In " + testName + " w != GetParameterBlock(1).user_state()");
    					  passed = false; 
    				  }
    				  problem.AddParameterBlock(z, 5);
    				  if (3 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (0 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 0 != NumResidualBlocks()");
    					  passed = false;
    				  }
    				  if (y != GetParameterBlock(0).user_state()) {
    					  System.err.println("In " + testName + " y != GetParameterBlock(0).user_state()");
    					  passed = false;
    				  }
    				  if (w != GetParameterBlock(1).user_state()) {
    					  System.err.println("In " + testName + " w != GetParameterBlock(1).user_state()");
    					  passed = false; 
    				  }
    				  if (z != GetParameterBlock(2).user_state()) {
    					  System.err.println("In " + testName + " z != GetParameterBlock(2).user_state()");
    					  passed = false;
    				  }

    				  // Now remove everything.
    				  // y
    				  problem.RemoveParameterBlock(y);
    				  if (2 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 2 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (0 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 0 != NumResidualBlocks()");
    					  passed = false;
    				  }
    				  if (z != GetParameterBlock(0).user_state()) {
    					  System.err.println("In " + testName + " z != GetParameterBlock(0).user_state()");
    					  passed = false;
    				  }
    				  if (w != GetParameterBlock(1).user_state()) {
    					  System.err.println("In " + testName + " w != GetParameterBlock(1).user_state()");
    					  passed = false; 
    				  }

    				  // z
    				  problem.RemoveParameterBlock(z);
    				  if (1 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 1 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (0 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 0 != NumResidualBlocks()");
    					  passed = false;
    				  }
    				  if (w != GetParameterBlock(0).user_state()) {
    					  System.err.println("In " + testName + " w != GetParameterBlock(0).user_state()");
    					  passed = false;
    				  }

    				  // w
    				  problem.RemoveParameterBlock(w);
    				  if (0 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 0 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (0 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 0 != NumResidualBlocks()");
    					  passed = false;
    				  }
    				  if (passed) {
    					  System.out.println(testName + " passed all tests");
    				  }
    				}

    			  public void DynamicProblemRemoveParameterBlockWithResiduals() {
    				  problem.AddParameterBlock(y, 4);
    				  problem.AddParameterBlock(z, 5);
    				  problem.AddParameterBlock(w, 3);
    				  if (3 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (0 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 0 != NumResidualBlocks()");
    					  passed = false;
    				  }
    				  if (y != GetParameterBlock(0).user_state()) {
    					  System.err.println("In " + testName + " y != GetParameterBlock(0).user_state()");
    					  passed = false;
    				  }
    				  if (z != GetParameterBlock(1).user_state()) {
    					  System.err.println("In " + testName + " z != GetParameterBlock(1).user_state()");
    					  passed = false; 
    				  }
    				  if (w != GetParameterBlock(2).user_state()) {
    					  System.err.println("In " + testName + " w != GetParameterBlock(2).user_state()");
    					  passed = false;
    				  }

    				  // Add all combinations of cost functions.
    				  CostFunction cost_yzw = new TernaryCostFunction2(1, 4, 5, 3);
    				  CostFunction cost_yz  = new BinaryCostFunction (1, 4, 5);
    				  CostFunction cost_yw  = new BinaryCostFunction (1, 4, 3);
    				  CostFunction cost_zw  = new BinaryCostFunction (1, 5, 3);
    				  CostFunction cost_y   = new UnaryCostFunction  (1, 4);
    				  CostFunction cost_z   = new UnaryCostFunction  (1, 5);
    				  CostFunction cost_w   = new UnaryCostFunction  (1, 3);

    				  ResidualBlock r_yzw = problem.AddResidualBlock(cost_yzw, null, y, z, w);
    				  ResidualBlock r_yz  = problem.AddResidualBlock(cost_yz,  null, y, z);
    				  ResidualBlock r_yw  = problem.AddResidualBlock(cost_yw,  null, y, w);
    				  ResidualBlock r_zw  = problem.AddResidualBlock(cost_zw,  null, z, w);
    				  ResidualBlock r_y   = problem.AddResidualBlock(cost_y,   null, y);
    				  ResidualBlock r_z   = problem.AddResidualBlock(cost_z,   null, z);
    				  ResidualBlock r_w   = problem.AddResidualBlock(cost_w,  null, w);

    				  if (3 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (7 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 7 != NumResidualBlocks()");
    					  passed = false;
    				  }

    				  // Remove w, which should remove r_yzw, r_yw, r_zw, r_w.
    				  problem.RemoveParameterBlock(w);
    				  if (2 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 2 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (3 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 3 != NumResidualBlocks()");
    					  passed = false;
    				  }

    				  if (HasResidualBlock(r_yzw)) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_yzw) = true");
    					  passed = false;
    				  }
    				  if (!HasResidualBlock(r_yz )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_yz) = false");
    					  passed = false;
    				  }
    				  if (HasResidualBlock(r_yw )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_yw) = true");
    					  passed = false;
    				  }
    				  if (HasResidualBlock(r_zw )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_zw) = true");
    					  passed = false;
    				  }
    				  if (!HasResidualBlock(r_y  )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_y) = false");
    					  passed = false;
    				  }
    				  if (!HasResidualBlock(r_z  )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_z) = false");
    					  passed = false;
    				  }
    				  if (HasResidualBlock(r_w  )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_w) = true");
    					  passed = false;
    				  }

    				  // Remove z, which will remove almost everything else.
    				  problem.RemoveParameterBlock(z);
    				  if (1 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 1 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (1 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 1 != NumResidualBlocks()");
    					  passed = false;
    				  }

    				  if (HasResidualBlock(r_yzw)) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_yzw) = true");
    					  passed = false;
    				  }
    				  if (HasResidualBlock(r_yz )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_yz) = true");
    					  passed = false;
    				  }
    				  if (HasResidualBlock(r_yw )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_yw) = true");
    					  passed = false;
    				  }
    				  if (HasResidualBlock(r_zw )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_zw) = true");
    					  passed = false;
    				  }
    				  if (!HasResidualBlock(r_y  )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_y) = false");
    					  passed = false;
    				  }
    				  if (HasResidualBlock(r_z  )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_z) = true");
    					  passed = false;
    				  }
    				  if (HasResidualBlock(r_w  )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_w) = true");
    					  passed = false;
    				  }

    				  // Remove y; all gone.
    				  problem.RemoveParameterBlock(y);
    				  if (0 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 0 != problem.NumParameterBlocks()");
    					  passed = false;
    				  }
    				  if (0 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 0 != NumResidualBlocks()");
    					  passed = false;
    				  }
    				  if (passed) {
    					  System.out.println(testName + " passed all tests");
    				  }
    				}

    				public void DynamicProblemRemoveResidualBlock() {
    				  problem.AddParameterBlock(y, 4);
    				  problem.AddParameterBlock(z, 5);
    				  problem.AddParameterBlock(w, 3);

    				  // Add all combinations of cost functions.
    				  CostFunction cost_yzw = new TernaryCostFunction2(1, 4, 5, 3);
    				  CostFunction cost_yz  = new BinaryCostFunction (1, 4, 5);
    				  CostFunction cost_yw  = new BinaryCostFunction (1, 4, 3);
    				  CostFunction cost_zw  = new BinaryCostFunction (1, 5, 3);
    				  CostFunction cost_y   = new UnaryCostFunction  (1, 4);
    				  CostFunction cost_z   = new UnaryCostFunction  (1, 5);
    				  CostFunction cost_w   = new UnaryCostFunction  (1, 3);

    				  ResidualBlock r_yzw = problem.AddResidualBlock(cost_yzw, null, y, z, w);
    				  ResidualBlock r_yz  = problem.AddResidualBlock(cost_yz,  null, y, z);
    				  ResidualBlock r_yw  = problem.AddResidualBlock(cost_yw,  null, y, w);
    				  ResidualBlock r_zw  = problem.AddResidualBlock(cost_zw,  null, z, w);
    				  ResidualBlock r_y   = problem.AddResidualBlock(cost_y,   null, y);
    				  ResidualBlock r_z   = problem.AddResidualBlock(cost_z,   null, z);
    				  ResidualBlock r_w   = problem.AddResidualBlock(cost_w,   null, w);

    				  if (param) {
    				    // In this test parameterization, there should be back-pointers from the
    				    // parameter blocks to the residual blocks.
    				    ExpectParameterBlockContains(y, r_yzw, r_yz, r_yw, r_y);
    				    ExpectParameterBlockContains(z, r_yzw, r_yz, r_zw, r_z);
    				    ExpectParameterBlockContains(w, r_yzw, r_yw, r_zw, r_w);
    				  } else {
    				    // Otherwise, nothing.
    				    if (GetParameterBlock(0).mutable_residual_blocks() != null) {
    				    	System.err.println("In " + testName + " GetParameterBlock(0).mutable_residual_blocks() != null");
    				    	passed = false;
    				    }
    				    if (GetParameterBlock(1).mutable_residual_blocks() != null) {
    				    	System.err.println("In " + testName + " GetParameterBlock(1).mutable_residual_blocks() != null");
    				    	passed = false;
    				    }
    				    if (GetParameterBlock(2).mutable_residual_blocks() != null) {
    				    	System.err.println("In " + testName + " GetParameterBlock(2).mutable_residual_blocks() != null");
    				    	passed = false;
    				    }
    				  }
    				  if (3 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
  				    	  passed = false;
    				  }
    				  if (7 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 7 != NumResidualBlocks()");
  				    	  passed = false;
    				  }

    				  // Remove each residual and check the state after each removal.

    				  // Remove r_yzw.
    				  problem.RemoveResidualBlock(r_yzw);
    				  if (3 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
  				    	  passed = false;
    				  }
    				  if (6 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 6 != NumResidualBlocks()");
  				    	  passed = false;
    				  }
    				  if (param) {
    				    ExpectParameterBlockContains(y, r_yz, r_yw, r_y);
    				    ExpectParameterBlockContains(z, r_yz, r_zw, r_z);
    				    ExpectParameterBlockContains(w, r_yw, r_zw, r_w);
    				  }
    				  if (!HasResidualBlock(r_yz )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_yz ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_yw )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_yw ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_zw )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_zw ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_y )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_y ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_z )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_z ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_w )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_w ) = false");
  				    	  passed = false; 
    				  }

    				  // Remove r_yw.
    				  problem.RemoveResidualBlock(r_yw);
    				  if (3 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
  				    	  passed = false;
    				  }
    				  if (5 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 5 != NumResidualBlocks()");
  				    	  passed = false;
    				  }
    				  if (param) {
    				    ExpectParameterBlockContains(y, r_yz, r_y);
    				    ExpectParameterBlockContains(z, r_yz, r_zw, r_z);
    				    ExpectParameterBlockContains(w, r_zw, r_w);
    				  }
    				  if (!HasResidualBlock(r_yz )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_yz ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_zw )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_zw ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_y )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_y ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_z )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_z ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_w )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_w ) = false");
  				    	  passed = false; 
    				  }

    				  // Remove r_zw.
    				  problem.RemoveResidualBlock(r_zw);
    				  if (3 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
  				    	  passed = false;
    				  }
    				  if (4 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 4 != NumResidualBlocks()");
  				    	  passed = false;
    				  }
    				  if (param) {
    				    ExpectParameterBlockContains(y, r_yz, r_y);
    				    ExpectParameterBlockContains(z, r_yz, r_z);
    				    ExpectParameterBlockContains(w, r_w);
    				  }
    				  if (!HasResidualBlock(r_yz )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_yz ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_y )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_y ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_z )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_z ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_w )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_w ) = false");
  				    	  passed = false; 
    				  }

    				  // Remove r_w.
    				  problem.RemoveResidualBlock(r_w);
    				  if (3 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
  				    	  passed = false;
    				  }
    				  if (3 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 3 != NumResidualBlocks()");
  				    	  passed = false;
    				  }
    				  if (param) {
    				    ExpectParameterBlockContains(y, r_yz, r_y);
    				    ExpectParameterBlockContains(z, r_yz, r_z);
    				    ExpectParameterBlockContains(w);
    				  }
    				  if (!HasResidualBlock(r_yz )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_yz ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_y )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_y ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_z )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_z ) = false");
  				    	  passed = false; 
    				  }
    				  // Remove r_yz.
    				  problem.RemoveResidualBlock(r_yz);
    				  if (3 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
  				    	  passed = false;
    				  }
    				  if (2 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 2 != NumResidualBlocks()");
  				    	  passed = false;
    				  }
    				  if (param) {
    				    ExpectParameterBlockContains(y, r_y);
    				    ExpectParameterBlockContains(z, r_z);
    				    ExpectParameterBlockContains(w);
    				  }
    				  if (!HasResidualBlock(r_y )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_y ) = false");
  				    	  passed = false; 
    				  }
    				  if (!HasResidualBlock(r_z )) {
    					  System.err.println("In " + testName + " HasResidualBlock(r_z ) = false");
  				    	  passed = false; 
    				  }

    				  // Remove the last two.
    				  problem.RemoveResidualBlock(r_z);
    				  problem.RemoveResidualBlock(r_y);
    				  if (3 != problem.NumParameterBlocks()) {
    					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
  				    	  passed = false;
    				  }
    				  if (0 != NumResidualBlocks()) {
    					  System.err.println("In " + testName + " 0 != NumResidualBlocks()");
  				    	  passed = false;
    				  }
    				  if (param) {
    				    ExpectParameterBlockContains(y);
    				    ExpectParameterBlockContains(z);
    				    ExpectParameterBlockContains(w);
    				  }
    				  if (passed) {
    					  System.out.println(testName + " passed all tests");
    				  }
    				}

    				public void DynamicProblemRemoveInvalidResidualBlockDies() {
    					  problem.AddParameterBlock(y, 4);
    					  problem.AddParameterBlock(z, 5);
    					  problem.AddParameterBlock(w, 3);

    					  // Add all combinations of cost functions.
    					  CostFunction cost_yzw = new TernaryCostFunction2(1, 4, 5, 3);
    					  CostFunction cost_yz  = new BinaryCostFunction (1, 4, 5);
    					  CostFunction cost_yw  = new BinaryCostFunction (1, 4, 3);
    					  CostFunction cost_zw  = new BinaryCostFunction (1, 5, 3);
    					  CostFunction cost_y   = new UnaryCostFunction  (1, 4);
    					  CostFunction cost_z   = new UnaryCostFunction  (1, 5);
    					  CostFunction cost_w   = new UnaryCostFunction  (1, 3);

    					  ResidualBlock r_yzw = problem.AddResidualBlock(cost_yzw, null, y, z, w);
    					  ResidualBlock r_yz  = problem.AddResidualBlock(cost_yz,  null, y, z);
    					  ResidualBlock r_yw  = problem.AddResidualBlock(cost_yw,  null, y, w);
    					  ResidualBlock r_zw  = problem.AddResidualBlock(cost_zw,  null, z, w);
    					  ResidualBlock r_y   = problem.AddResidualBlock(cost_y,   null, y);
    					  ResidualBlock r_z   = problem.AddResidualBlock(cost_z,   null, z);
    					  ResidualBlock r_w   = problem.AddResidualBlock(cost_w,   null, w);

    					  // Remove r_yzw.
    					  problem.RemoveResidualBlock(r_yzw);
    					  if (3 != problem.NumParameterBlocks()) {
        					  System.err.println("In " + testName + " 3 != problem.NumParameterBlocks()");
      				    	  passed = false;
        				  }
        				  if (6 != NumResidualBlocks()) {
        					  System.err.println("In " + testName + " 6 != NumResidualBlocks()");
      				    	  passed = false;
        				  }
    					  // Attempt to remove r_yzw again.
    					  //EXPECT_DEATH_IF_SUPPORTED(problem->RemoveResidualBlock(r_yzw), "not found");
        				  problem.RemoveResidualBlock(r_yzw);

    					  // Attempt to remove a cast pointer never added as a residual.
    					  //int trash_memory = 1234;
    					  //ResidualBlock* invalid_residual =
    					      //reinterpret_cast<ResidualBlock*>(&trash_memory);
    					  //EXPECT_DEATH_IF_SUPPORTED(problem->RemoveResidualBlock(invalid_residual),
    					                            //"not found");

    					  // Remove a parameter block, which in turn removes the dependent residuals
    					  // then attempt to remove them directly.
    					  problem.RemoveParameterBlock(z);
    					  if (2 != problem.NumParameterBlocks()) {
        					  System.err.println("In " + testName + " 2 != problem.NumParameterBlocks()");
      				    	  passed = false;
        				  }
        				  if (3 != NumResidualBlocks()) {
        					  System.err.println("In " + testName + " 3 != NumResidualBlocks()");
      				    	  passed = false;
        				  }
    					  //EXPECT_DEATH_IF_SUPPORTED(problem->RemoveResidualBlock(r_yz), "not found");
    					  //EXPECT_DEATH_IF_SUPPORTED(problem->RemoveResidualBlock(r_zw), "not found");
    					  //EXPECT_DEATH_IF_SUPPORTED(problem->RemoveResidualBlock(r_z), "not found");
        				  problem.RemoveResidualBlock(r_yz);
        				  problem.RemoveResidualBlock(r_zw);
        				  problem.RemoveResidualBlock(r_z);

    					  problem.RemoveResidualBlock(r_yw);
    					  problem.RemoveResidualBlock(r_w);
    					  problem.RemoveResidualBlock(r_y);
    					}

    			} // class DynamicProblem
    			
    			public void ProblemSetParameterBlockConstantWithUnknownPtrDies() {
    				  // Gives expected error message:
    				  // In SetParameterBlockConstant Parameter block not found for supplied double[] values.
    				  // You must add the parameter block to the problem before it can be set constant
    				  double x[] = new double[3];
    				  double y[] = new double[2];

    				  ProblemImpl problem = new ProblemImpl();
    				  problem.AddParameterBlock(x, 3);

    				  //EXPECT_DEATH_IF_SUPPORTED(problem.SetParameterBlockConstant(y),
    				                            //"Parameter block not found:");
    				  problem.SetParameterBlockConstant(y);
    			}
    			
    			public void ProblemSetParameterBlockVariableWithUnknownPtrDies() {
    				  // Gives expected error message:
    				  // In SetParameterBlockVariable Parameter block not found for supplied double[] values.
    				  // You must add the parameter block to the problem before it can be set varying.
    				  double x[] = new double[3];
    				  double y[] = new double[2];

    				  ProblemImpl problem = new ProblemImpl();
    				  problem.AddParameterBlock(x, 3);

    				  //EXPECT_DEATH_IF_SUPPORTED(problem.SetParameterBlockVariable(y),
    				                            //"Parameter block not found:");
    				  problem.SetParameterBlockVariable(y);
    			}

    			public void ProblemIsParameterBlockConstant() {
    				  // ProblemIsParameterBlockConstant() passed all tests
    				  boolean passed = true;
    				  double x1[] = new double[3];
    				  double x2[] = new double[3];

    				  ProblemImpl problem = new ProblemImpl();
    				  problem.AddParameterBlock(x1, 3);
    				  problem.AddParameterBlock(x2, 3);

    				  if (problem.IsParameterBlockConstant(x1)) {
    					  System.err.println("In ProblemIsParameterBlockConstant() problem.IsParameterBlockConstant(x1) = true");
    					  passed = false;
    				  }
    				  if (problem.IsParameterBlockConstant(x2)) {
    					  System.err.println("In ProblemIsParameterBlockConstant() problem.IsParameterBlockConstant(x2) = true");
    					  passed = false;
    				  }

    				  problem.SetParameterBlockConstant(x1);
    				  if (!problem.IsParameterBlockConstant(x1)) {
    					  System.err.println("In ProblemIsParameterBlockConstant() problem.IsParameterBlockConstant(x1) = false");
    					  passed = false;
    				  }
    				  if (problem.IsParameterBlockConstant(x2)) {
    					  System.err.println("In ProblemIsParameterBlockConstant() problem.IsParameterBlockConstant(x2) = true");
    					  passed = false;
    				  }

    				  problem.SetParameterBlockConstant(x2);
    				  if (!problem.IsParameterBlockConstant(x1)) {
    					  System.err.println("In ProblemIsParameterBlockConstant() problem.IsParameterBlockConstant(x1) = false");
    					  passed = false;
    				  }
    				  if (!problem.IsParameterBlockConstant(x2)) {
    					  System.err.println("In ProblemIsParameterBlockConstant() problem.IsParameterBlockConstant(x2) = false");
    					  passed = false;
    				  }
    				  
    				  problem.SetParameterBlockVariable(x1);
    				  if (problem.IsParameterBlockConstant(x1)) {
    					  System.err.println("In ProblemIsParameterBlockConstant() problem.IsParameterBlockConstant(x1) = true");
    					  passed = false;
    				  }
    				  if (!problem.IsParameterBlockConstant(x2)) {
    					  System.err.println("In ProblemIsParameterBlockConstant() problem.IsParameterBlockConstant(x2) = false");
    					  passed = false;
    				  }
    				  
    				  if (passed) {
    					  System.out.println("ProblemIsParameterBlockConstant() passed all tests");
    				  }
    			}

    			public void ProblemIsParameterBlockConstantWithUnknownPtrDies() {
    				  // Gives expected error message:
    				  // In IsParameterBlockConstant Parameter block not found for supplied double[] values.
    				  // You must add the parameter block to the problem before it can be queried.
    				  double x[] = new double[3];
    				  double y[] = new double[2];

    				  ProblemImpl problem = new ProblemImpl();
    				  problem.AddParameterBlock(x, 3);

    				  //EXPECT_DEATH_IF_SUPPORTED(problem.IsParameterBlockConstant(y),
    				                            //"Parameter block not found:");
    				  problem.IsParameterBlockConstant(y);
    			}
    			
    			public void ProblemSetLocalParameterizationWithUnknownPtrDies() {
    				// Gives expected error message:
    				// In SetParameterization Parameter block not found for supplied double[] values.
    				// You must add the parameter block to the problem before you can set its local parameterization.
    				double x[] = new double[3];
  				    double y[] = new double[2];

  				    ProblemImpl problem = new ProblemImpl();
  				    problem.AddParameterBlock(x, 3);
    				  
    				  //EXPECT_DEATH_IF_SUPPORTED(
    				      //problem.SetParameterization(y, new IdentityParameterization(3)),
    				      //"Parameter block not found:");
  				  problem.SetParameterization(y, new IdentityParameterization(3));
    			}
    			
    			public void ProblemRemoveParameterBlockWithUnknownPtrDies() {
    				  // Gives expected error message:
    				  // In RemoveParameterBlock Parameter block not found for supplied double[] values.
    				  // You must add the parameter block to the problem before it can be removed.
    				  double x[] = new double[3];
  				      double y[] = new double[2];

  				      ProblemImpl problem = new ProblemImpl();
  				      problem.AddParameterBlock(x, 3);
    				  
    				  //EXPECT_DEATH_IF_SUPPORTED(
    				      problem.RemoveParameterBlock(y); //, "Parameter block not found:");
    				}

    			public void ProblemGetParameterization() {
    				  // ProblemGetParameterization() passed all tests
    				  boolean passed = true;
    				  double x[] = new double[3];
    				  double y[] = new double[2];

    				  ProblemImpl problem = new ProblemImpl();
    				  problem.AddParameterBlock(x, 3);
    				  problem.AddParameterBlock(y, 2);

    				  LocalParameterization parameterization =  new IdentityParameterization(3);
    				  problem.SetParameterization(x, parameterization);
    				  if (problem.GetParameterization(x) != parameterization) {
    					  System.err.println("In ProblemGetParameterization() problem.GetParameterization(x) != parameterization");
    					  passed = false;
    				  }
    				  if (problem.GetParameterization(y) != null) {
    					  System.err.println("In ProblemGetParameterization() problem.GetParameterization(y) != null");
    					  passed = false;
    				  }
    				  if (passed) {
    					  System.out.println("ProblemGetParameterization() passed all tests");
    				  }
    			}

    			public void ProblemParameterBlockQueryTest() {
    				  // ProblemParameterBlockQueryTest() passed all tests
    				  boolean passed = true;
    				  double x[] = new double[3];
    				  double y[] = new double[4];
    				  ProblemImpl problem = new ProblemImpl();
    				  problem.AddParameterBlock(x, 3);
    				  problem.AddParameterBlock(y, 4);

    				  Vector<Integer> constant_parameters = new Vector<Integer>();
    				  constant_parameters.add(0);
    				  problem.SetParameterization(
    				      x,
    				      new SubsetParameterization(3, constant_parameters));
    				  if (problem.ParameterBlockSize(x) != 3) {
    					  System.err.println("In ProblemParameterBlockQueryTest() problem.ParameterBlockSize(x) != 3");
    					  passed = false;
    				  }
    				  if (problem.ParameterBlockLocalSize(x) != 2) {
    					  System.err.println("In ProblemParameterBlockQueryTest() problem.ParameterBlockLocalSize(x) != 2");
    					  passed = false;
    				  }
    				  if (problem.ParameterBlockLocalSize(y) != 4) {
    					  System.err.println("In ProblemParameterBlockQueryTest() problem.ParameterBlockLocalSize(y) != 4");
    					  passed = false;
    				  }

    				  Vector<double[]> parameter_blocks = new Vector<double[]>();
    				  problem.GetParameterBlocks(parameter_blocks);
    				  if (parameter_blocks.size() != 2) {
    					  System.err.println("In ProblemParameterBlockQueryTest() parameter_blocks.size() != 2");
    					  passed = false;
    				  }
    				  if (parameter_blocks.get(0) == parameter_blocks.get(1)) {
    					  System.err.println("In ProblemParameterBlockQueryTest() parameter_blocks.get(0) == parameter_blocks.get(1)");
    					  passed = false;
    				  }
    				  if (parameter_blocks.get(0) != x && parameter_blocks.get(0) != y) {
    					  System.err.println("In ProblemParameterBlockQueryTest() parameter_blocks.get(0) != x && parameter_blocks.get(0) != y");
    					  passed = false;
    				  }
    				  if (parameter_blocks.get(1) != x && parameter_blocks.get(1) != y) {
    					  System.err.println("In ProblemParameterBlockQueryTest() parameter_blocks.get(1) != x && parameter_blocks.get(1) != y");
    					  passed = false;
    				  }

    				  if (!problem.HasParameterBlock(x)) {
    					  System.err.println("In ProblemParameterBlockQueryTest() problem.HasParameterBlock(x) = false");
    					  passed = false;
    				  }
    				  problem.RemoveParameterBlock(x);
    				  if (problem.HasParameterBlock(x)) {
    					  System.err.println("In ProblemParameterBlockQueryTest() problem.HasParameterBlock(x) = true");
    					  passed = false; 
    				  }
    				  problem.GetParameterBlocks(parameter_blocks);
    				  if (parameter_blocks.size() != 1) {
    					  System.err.println("In ProblemParameterBlockQueryTest() parameter_blocks.size() != 1");
    					  passed = false; 
    				  }
    				  if (parameter_blocks.get(0) != y) {
    					  System.err.println("In ProblemParameterBlockQueryTest() parameter_blocks.get(0) != y");
    					  passed = false;
    				  }
    				  if (passed) {
    					  System.out.println("ProblemParameterBlockQueryTest() passed all tests");
    				  }
    				}
    			
    			public void DynamicProblemRemoveParameterBlockWithNoResiduals() {
    				//DynamicProblemRemoveParameterBlockWithNoResiduals()_param_true passed all tests
    				//DynamicProblemRemoveParameterBlockWithNoResiduals()_param_false passed all tests
    				DynamicProblem DP = new DynamicProblem(true, "DynamicProblemRemoveParameterBlockWithNoResiduals()_param_true");
    				DP.DynamicProblemRemoveParameterBlockWithNoResiduals();
    				DP = new DynamicProblem(false, "DynamicProblemRemoveParameterBlockWithNoResiduals()_param_false");
    				DP.DynamicProblemRemoveParameterBlockWithNoResiduals();
    			}
    			
    			public void DynamicProblemRemoveParameterBlockWithResiduals() {
    				//DynamicProblemRemoveParameterBlockWithResiduals()_param_true passed all tests
    				//DynamicProblemRemoveParameterBlockWithResiduals()_param_false passed all tests
    				DynamicProblem DP = new DynamicProblem(true, "DynamicProblemRemoveParameterBlockWithResiduals()_param_true");
    				DP.DynamicProblemRemoveParameterBlockWithResiduals();
    				DP = new DynamicProblem(false, "DynamicProblemRemoveParameterBlockWithResiduals()_param_false");
    				DP.DynamicProblemRemoveParameterBlockWithResiduals();
    			}
    			
    			public void DynamicProblemRemoveResidualBlock() {
    				//DynamicProblemRemoveResidualBlock()_param_true passed all tests
    				//DynamicProblemRemoveResidualBlock()_param_false passed all tests
    				DynamicProblem DP = new DynamicProblem(true, "DynamicProblemRemoveResidualBlock()_param_true");
    				DP.DynamicProblemRemoveResidualBlock();
    				DP = new DynamicProblem(false, "DynamicProblemRemoveResidualBlock()_param_false");
    				DP.DynamicProblemRemoveResidualBlock();
    			}
    			
    			public void DynamicProblemRemoveInvalidResidualBlockDies() {
    				// Gives error message 8 times as expected:
    				//Residual block to remove not found. This usually means one of three things have happened:
    			    //1) residual_block is uninitialised and points to a random area in memory.
    			    //2) residual_block represented a residual that was added to the problem, but referred to a parameter block which has
    			    //since been removed, which removes all residuals which depend on that parameter block, and was thus removed.
    			    //3) residual_block referred to a residual that has already been removed from the problem (by the user).
    				DynamicProblem DP = new DynamicProblem(true, "DynamicProblemRemoveInvalidResidualBlockDies()_param_true");
    				DP.DynamicProblemRemoveInvalidResidualBlockDies();
    				DP = new DynamicProblem(false, "DynamicProblemRemoveInvalidResidualBlockDies()_param_false");
    				DP.DynamicProblemRemoveInvalidResidualBlockDies();
    			}
    			
    			// Check that a null-terminated array, a, has the same elements as b.
    			// Used on ResidualBlock pointers
    			/*template<typename T>
    			void ExpectVectorContainsUnordered(const T* a, const vector<T>& b) {
    			  // Compute the size of a.
    			  int size = 0;
    			  while (a[size]) {
    			    ++size;
    			  }
    			  ASSERT_EQ(size, b.size());

    			  // Sort a.
    			  vector<T> a_sorted(size);
    			  copy(a, a + size, a_sorted.begin());
    			  sort(a_sorted.begin(), a_sorted.end());

    			  // Sort b.
    			  vector<T> b_sorted(b);
    			  sort(b_sorted.begin(), b_sorted.end());

    			  // Compare.
    			  for (int i = 0; i < size; ++i) {
    			    EXPECT_EQ(a_sorted[i], b_sorted[i]);
    			  }
    			}

    			void ExpectProblemHasResidualBlocks(
    			// Sorting on ResidualBlock pointers
    			    const ProblemImpl &problem,
    			    const ResidualBlockId *expected_residual_blocks) {
    			  vector<ResidualBlockId> residual_blocks;
    			  problem.GetResidualBlocks(&residual_blocks);
    			  ExpectVectorContainsUnordered(expected_residual_blocks, residual_blocks);
    			}*/
    			
    			// If rho[1] is zero, the Corrector constructor should crash.
    		    public void CorrectorZeroGradientDeathTest() {
    		      // Gives expected error message:
    		      // rho[1] <= 0.0 in public Corrector(double sq_norm, double rho[])
    			  final double kRho[] = new double[]{0.0, 0.0, 1.0};
    			  //EXPECT_DEATH_IF_SUPPORTED({Corrector c(1.0, kRho);}, ".*");
    			  Corrector c = new Corrector(1.0, kRho);
    			}

    		    // If rho[1] is negative, the Corrector constructor should crash.
    		    public void CorrectorNegativeGradientDeathTest() {
    		      // Gives expected error message:
      		      // rho[1] <= 0.0 in public Corrector(double sq_norm, double rho[])
    		      final double kRho[] = new double[]{0.0, -0.1, 1.0};
    		      //EXPECT_DEATH_IF_SUPPORTED({Corrector c(1.0, kRho);},".*");
    		      Corrector c = new Corrector(1.0, kRho);
    		    }
    		    
    		    public void CorrectorScalarCorrection() {
    		    	  // CorrectorScalarCorrection() passed all tests
    		    	  boolean passed = true;
    		    	  double residuals[] = new double[] {Math.sqrt(3.0)};
    		    	  double jacobian[] = new double[] {10.0};
    		    	  double sq_norm = residuals[0] * residuals[0];

    		    	  final double kRho[] = new double[]{sq_norm, 0.1, -0.01};

    		    	  // In light of the rho'' < 0 clamping now implemented in
    		    	  // corrector.cc, alpha = 0 whenever rho'' < 0.
    		    	  final double kAlpha = 0.0;

    		    	  // Thus the expected value of the residual is
    		    	  // residual[i] * sqrt(kRho[1]) / (1.0 - kAlpha).
    		    	  final double kExpectedResidual =
    		    	      residuals[0] * Math.sqrt(kRho[1]) / (1 - kAlpha);

    		    	  // The jacobian in this case will be
    		    	  // sqrt(kRho[1]) * (1 - kAlpha) * jacobian.
    		    	  final double kExpectedJacobian = Math.sqrt(kRho[1]) * (1 - kAlpha) * jacobian[0];

    		    	  Corrector c = new Corrector(sq_norm, kRho);
    		    	  c.CorrectJacobian(1, 1, residuals, jacobian);
    		    	  c.CorrectResiduals(1, residuals);

    		    	  if (Math.abs(residuals[0] - kExpectedResidual) > 1e-6) {
    		    		  System.err.println("In CorrectorScalarCorrection() (Math.abs(residuals[0] - kExpectedResidual) > 1e-6");
    		    		  passed = false;
    		    	  }
    		    	  if (Math.abs(kExpectedJacobian - jacobian[0]) > 1e-6) {
    		    		  System.err.println("In CorrectorScalarCorrection() (Math.abs(kExpectedJacobian - jacobian[0]) > 1e-6");
    		    		  passed = false; 
    		    	  }
    		    	  if (passed) {
    		    		  System.out.println("CorrectorScalarCorrection() passed all tests");
    		    	  }
    		    }
    		    
    		    public void CorrectorScalarCorrectionZeroResidual() {
    		    	  // CorrectorScalarCorrectionZeroResidual() passed all tests
    		    	  boolean passed = true;
    		    	  double residuals[] = new double[] {0.0};
    		    	  double jacobian[] = new double[] {10.0};
    		    	  double sq_norm = residuals[0] * residuals[0];

    		    	  final double kRho[] = new double[]{0.0, 0.1, -0.01};
    		    	  Corrector c = new Corrector(sq_norm, kRho);

    		    	  // The alpha equation is
    		    	  // 1/2 alpha^2 - alpha + 0.0 = 0.
    		    	  // i.e. alpha = 1.0 - sqrt(1.0).
    		    	  //      alpha = 0.0.
    		    	  // Thus the expected value of the residual is
    		    	  // residual[i] * sqrt(kRho[1])
    		    	  final double kExpectedResidual = residuals[0] * Math.sqrt(kRho[1]);

    		    	  // The jacobian in this case will be
    		    	  // sqrt(kRho[1]) * jacobian.
    		    	  final double kExpectedJacobian = Math.sqrt(kRho[1]) * jacobian[0];

    		    	  c.CorrectJacobian(1, 1, residuals, jacobian);
    		    	  c.CorrectResiduals(1, residuals);

    		    	  if (Math.abs(residuals[0] - kExpectedResidual) > 1e-6) {
    		    		  System.err.println("In CorrectorScalarCorrectionZeroResidual() (Math.abs(residuals[0] - kExpectedResidual) > 1e-6");
    		    		  passed = false;
    		    	  }
    		    	  if (Math.abs(kExpectedJacobian - jacobian[0]) > 1e-6) {
    		    		  System.err.println("In CorrectorScalarCorrectionZeroResidual() (Math.abs(kExpectedJacobian - jacobian[0]) > 1e-6");
    		    		  passed = false; 
    		    	  }
    		    	  if (passed) {
    		    		  System.out.println("CorrectorScalarCorrectionZeroResidual() passed all tests");
    		    	  }
    		    	}

    		    public void CorrectorScalarCorrectionAlphaClamped() {
    		    	  // CorrectorScalarCorrectionAlphaClamped() passed all tests
    		    	  boolean passed = true;
    		    	  double residuals[] = new double[] {Math.sqrt(3.0)};
    		    	  double jacobian[] = new double[] {10.0};
    		    	  double sq_norm = residuals[0] * residuals[0];

    		    	  final double kRho[] = new double[]{3, 0.1, -0.1};

    		    	  // rho[2] < 0 -> alpha = 0.0
    		    	  final double kAlpha = 0.0;

    		    	  // Thus the expected value of the residual is
    		    	  // residual[i] * sqrt(kRho[1]) / (1.0 - kAlpha).
    		    	  final double kExpectedResidual =
    		    	      residuals[0] * Math.sqrt(kRho[1]) / (1.0 - kAlpha);

    		    	  // The jacobian in this case will be scaled by
    		    	  // sqrt(rho[1]) * (1 - alpha) * J.
    		    	  final double kExpectedJacobian = Math.sqrt(kRho[1]) *
    		    	      (1.0 - kAlpha) * jacobian[0];

    		    	  Corrector c = new Corrector(sq_norm, kRho);
    		    	  c.CorrectJacobian(1, 1, residuals, jacobian);
    		    	  c.CorrectResiduals(1, residuals);

    		    	  if (Math.abs(residuals[0] - kExpectedResidual) > 1e-6) {
    		    		  System.err.println("In CorrectorScalarCorrectionAlphaClamped() (Math.abs(residuals[0] - kExpectedResidual) > 1e-6");
    		    		  passed = false;
    		    	  }
    		    	  if (Math.abs(kExpectedJacobian - jacobian[0]) > 1e-6) {
    		    		  System.err.println("In CorrectorScalarCorrectionAlphaClamped() (Math.abs(kExpectedJacobian - jacobian[0]) > 1e-6");
    		    		  passed = false; 
    		    	  }
    		    	  if (passed) {
    		    		  System.out.println("CorrectorScalarCorrectionAlphaClamped() passed all tests");
    		    	  }
    		    	}

    		 // Test that the corrected multidimensional residual and jacobians
    		 // match the expected values and the resulting modified normal
    		 // equations match the robustified gauss newton approximation.
    		 public void CorrectorMultidimensionalGaussNewtonApproximation() {
    		   //CorrectorMultidimensionalGaussNewtonApproximation() passed all tests
    		   int i, row, col;
    		   double diff;
    		   boolean passed = true;
    		   double residuals[] = new double[3];
    		   double jacobian[] = new double[2 * 3];
    		   double rho[] = new double[3];

    		   // Eigen matrix references for linear algebra.
    		   //MatrixRef jac(jacobian, 3, 2);
    		   Matrix jac = new Matrix(3, 2);
    		   //VectorRef res(residuals, 3);
    		   Matrix res = new Matrix(3, 1);

    		   // Ground truth values of the modified jacobian and residuals.
    		   //Matrix g_jac(3, 2);
    		   double g_res[] = new double[3];

    		   // Ground truth values of the robustified Gauss-Newton
    		   // approximation.
    		   //Matrix g_hess(2, 2);
    		   //Vector g_grad(2);

    		   // Corrected hessian and gradient implied by the modified jacobian
    		   // and hessians.
    		   //Matrix c_hess(2, 2);
    		   //Vector c_grad(2);

    		   for (int iter = 0; iter < 10000; ++iter) {
    		     // Initialize the jacobian and residual.
    		     for (i = 0, row = 0; row < 3; row++) {
    		    	 for (col = 0; col < 2; col++, i++) {
    		    		 jacobian[i] = RandDouble();
    		    		 jac.set(row, col, jacobian[i]);
    		    	 }
    		     }
    		       
    		     for (i = 0; i < 3; ++i) {
    		       residuals[i] = RandDouble();
    		       res.set(i, 0, residuals[i]);
    		     }

    		     double sq_norm = 0.0;
    		     for (i = 0; i < 3; i++) {
    		    	 sq_norm += (residuals[i] * residuals[i]);
    		     }

    		     rho[0] = sq_norm;
    		     rho[1] = RandDouble();
    		     rho[2] = 2.0 * RandDouble() - 1.0;

    		     // If rho[2] > 0, then the curvature correction to the correction
    		     // and the gauss newton approximation will match. Otherwise, we
    		     // will clamp alpha to 0.

    		     final double kD = 1 + 2 * rho[2] / rho[1] * sq_norm;
    		     final double kAlpha = (rho[2] > 0.0) ? 1 - Math.sqrt(kD) : 0.0;

    		     // Ground truth values.
    		     for (i = 0; i < 3; i++) {
    		         g_res[i] = Math.sqrt(rho[1]) / (1.0 - kAlpha) * residuals[i];
    		     }
    		     Matrix g_jac =  (jac.minus
    		                             (((res.times(res.transpose())).times(jac)).timesEquals(kAlpha / sq_norm))).timesEquals(Math.sqrt(rho[1]));

    		     Matrix g_grad = ((jac.transpose()).times(res)).timesEquals(rho[1]);
    		     //Matrix g_hess = (((jac.transpose()).times(jac)).timesEquals(rho[1])).plus
    		         //(((((jac.transpose()).times(res)).times(res.transpose())).times(jac)).timesEquals(2.0 * rho[2]));

    		     Corrector c = new Corrector(sq_norm, rho);
    		     c.CorrectJacobian(3, 2, residuals, jacobian);
    		     c.CorrectResiduals(3, residuals);
    		     for (i = 0, row = 0; row < 3; row++) {
    		    	 for (col = 0; col < 2; col++, i++) {
    		    		 jac.set(row, col, jacobian[i]);
    		    	 }
    		     }
    		     for (i = 0; i < 3; ++i) {
      		       res.set(i, 0, residuals[i]);
      		     }

    		     // Corrected gradient and hessian.
    		     Matrix c_grad  = (jac.transpose()).times(res);
    		     //Matrix c_hess = (jac.transpose()).times(jac);

    		     double resdiffnorm = 0.0;
    		     for (i = 0; i < 3; i++) {
    		         diff = g_res[i] - res.get(i, 0);
    		         resdiffnorm += (diff * diff);
    		     }
    		     resdiffnorm = Math.sqrt(resdiffnorm);
    		     if (resdiffnorm > 1.0E-10) {
    		    	 System.err.println("In CorrectorMultidimensionalGaussNewtonApproximation() iter = " + iter + " (g_res - res).norm() > 1.0E-10");
    		    	 passed = false;
    		     }
    		     double jacdiffnorm = 0.0;
    		     for (row = 0; row < 3; row++) {
    		    	 for (col = 0; col < 2; col++) {
    		    		 diff = g_jac.get(row, col) - jac.get(row, col);
    		    		 jacdiffnorm += (diff * diff);
    		    	 }
    		     }
    		     jacdiffnorm = Math.sqrt(jacdiffnorm);
    		     if (jacdiffnorm > 1.0E-10) {
    		    	 System.err.println("In CorrectorMultidimensionalGaussNewtonApproximation() iter = " + iter + " (g_jac - jac).norm() > 1.0E-10");
    		    	 passed = false;
    		     }
    		     
    		     double graddiffnorm = 0.0;
    		     for (i = 0; i < 2; i++) {
    		    	 diff = g_grad.get(i, 0) - c_grad.get(i, 0);
    		    	 graddiffnorm += (diff * diff);
    		     }
    		     graddiffnorm = Math.sqrt(graddiffnorm);
    		     if (graddiffnorm > 1.0E-10) {
    		    	 System.err.println("In CorrectorMultidimensionalGaussNewtonApproximation() iter = " + iter + " (g_grad - c_grad).norm() > 1.0E-10");
    		    	 passed = false;
    		     }
    		   }
    		   if (passed) {
    			   System.out.println("CorrectorMultidimensionalGaussNewtonApproximation() passed all tests");
    		   }
    		 }
    		 
    		 public void CorrectorMultidimensionalGaussNewtonApproximationZeroResidual() {
    			 // res, grad, and jac match
    			 // hessian do not match
    			 int i, row, col;
      		     double diff;
      		      boolean passed = true;
    			  double residuals[] = new double[3];
    			  double jacobian[] = new double[2 * 3];
    			  double rho[] = new double[3];

    			  // Eigen matrix references for linear algebra.
    			  //MatrixRef jac(jacobian, 3, 2);
       		      Matrix jac = new Matrix(3, 2);
       		      //VectorRef res(residuals, 3);
       		      Matrix res = new Matrix(3, 1);

    			  // Ground truth values of the modified jacobian and residuals.
    			  //Matrix g_jac(3, 2);
    			  //Vector g_res(3);
       		      double g_res[] = new double[3];

    			  // Ground truth values of the robustified Gauss-Newton
    			  // approximation.
    			  //Matrix g_hess(2, 2);
    			  //Vector g_grad(2);

    			  // Corrected hessian and gradient implied by the modified jacobian
    			  // and hessians.
    			  //Matrix c_hess(2, 2);
    			  //Vector c_grad(2);

    			  for (int iter = 0; iter < 10000; ++iter) {
    			    // Initialize the jacobian.
    				  for (i = 0, row = 0; row < 3; row++) {
    	    		    	 for (col = 0; col < 2; col++, i++) {
    	    		    		 jacobian[i] = RandDouble();
    	    		    		 jac.set(row, col, jacobian[i]);
    	    		    	 }
    	    		  }

    			    // Zero residuals
    				  for (i = 0; i < 3; ++i) {
   	    		       residuals[i] = 0.0;
   	    		       res.set(i, 0, 0.0);
   	    		     }

    			    final double sq_norm = 0.0;

    			    rho[0] = sq_norm;
    			    rho[1] = RandDouble();
    			    rho[2] = 2 * RandDouble() - 1.0;

    			    // Ground truth values.
    			    for (i = 0; i < 3; i++) {
    			        g_res[i] = Math.sqrt(rho[1]) * residuals[i];
    			    }
    			    Matrix g_jac = jac.timesEquals(Math.sqrt(rho[1]));

    			    Matrix g_grad = ((jac.transpose()).times(res)).timesEquals(rho[1]);
    			    
    			  //Matrix g_hess = (((jac.transpose()).times(jac)).timesEquals(rho[1])).plus
   		         //(((((jac.transpose()).times(res)).times(res.transpose())).times(jac)).timesEquals(2.0 * rho[2]));
    			    // Since res = 0 simplifies to:
    			    //Matrix g_hess = ((jac.transpose()).times(jac)).timesEquals(rho[1]);
    			    // Hessians match when g_hess changed to:
    			    Matrix g_hess = (jac.transpose()).times(jac);

    			    Corrector c = new Corrector(sq_norm, rho);
    			    c.CorrectJacobian(3, 2, residuals, jacobian);
    			    c.CorrectResiduals(3, residuals);
    			    for (i = 0, row = 0; row < 3; row++) {
	       		    	 for (col = 0; col < 2; col++, i++) {
	       		    		 jac.set(row, col, jacobian[i]);
	       		    	 }
	       		    }
	       		    for (i = 0; i < 3; ++i) {
	         		   res.set(i, 0, residuals[i]);
	         		}

    			    // Corrected gradient and hessian.
    			    Matrix c_grad = (jac.transpose()).times(res);
    			    Matrix c_hess = (jac.transpose()).times(jac);

    			    double resdiffnorm = 0.0;
       		     for (i = 0; i < 3; i++) {
       		         diff = g_res[i] - res.get(i, 0);
       		         resdiffnorm += (diff * diff);
       		     }
       		     resdiffnorm = Math.sqrt(resdiffnorm);
       		     if (resdiffnorm > 1.0E-10) {
       		    	 System.err.println("In CorrectorMultidimensionalGaussNewtonApproximationZeroResidual() iter = " + iter + " (g_res - res).norm() > 1.0E-10");
       		    	 passed = false;
       		     }
       		     double jacdiffnorm = 0.0;
       		     for (row = 0; row < 3; row++) {
       		    	 for (col = 0; col < 2; col++) {
       		    		 diff = g_jac.get(row, col) - jac.get(row, col);
       		    		 jacdiffnorm += (diff * diff);
       		    	 }
       		     }
       		     jacdiffnorm = Math.sqrt(jacdiffnorm);
       		     if (jacdiffnorm > 1.0E-10) {
       		    	 System.err.println("In CorrectorMultidimensionalGaussNewtonApproximationZeroResidual() iter = " + iter + " (g_jac - jac).norm() > 1.0E-10");
       		    	 passed = false;
       		     }
       		     
       		     double graddiffnorm = 0.0;
       		     for (i = 0; i < 2; i++) {
       		    	 diff = g_grad.get(i, 0) - c_grad.get(i, 0);
       		    	 graddiffnorm += (diff * diff);
       		     }
       		     graddiffnorm = Math.sqrt(graddiffnorm);
       		     if (graddiffnorm > 1.0E-10) {
       		    	 System.err.println("In CorrectorMultidimensionalGaussNewtonApproximationZeroResidual() iter = " + iter + " (g_grad - c_grad).norm() > 1.0E-10");
       		    	 passed = false;
       		     }
       		     double hessdiffnorm = 0.0;
    		     for (row = 0; row < 2; row++) {
    		    	 for (col = 0; col < 2; col++) {
    		    		 diff = g_hess.get(row, col) - c_hess.get(row, col);
    		    		 hessdiffnorm += (diff * diff);
    		    	 }
    		     }
    		     hessdiffnorm = Math.sqrt(hessdiffnorm);
    		     if (hessdiffnorm > 1.0E-10) {
    		    	 System.err.println("In CorrectorMultidimensionalGaussNewtonApproximationZeroResidual() iter = " + iter + " (g_hess - c_hess).norm() > 1.0E-10");
    		    	 passed = false;
    		     }
    	       }
    		   if (passed) {
       			   System.out.println("CorrectorMultidimensionalGaussNewtonApproximationZeroResidual() passed all tests");
       		   }	  
    	     }

    		// Helper function for testing a LossFunction callback.
    		 //
    		 // Compares the values of rho'(s) and rho''(s) computed by the
    		 // callback with estimates obtained by symmetric finite differencing
    		 // of rho(s).
    		 public void AssertLossFunctionIsValid(LossFunction loss, double s, String testName) {
    		   boolean passed = true;
    		   if (s <= 0.0) {
    			   System.err.println("In  AssertLossFunctionIsValid s <= 0.0");
    			   return;
    		   }

    		   // Evaluate rho(s), rho'(s) and rho''(s).
    		   double rho[] = new double[3];
    		   loss.Evaluate(s, rho);

    		   // Use symmetric finite differencing to estimate rho'(s) and
    		   // rho''(s).
    		   final double kH = 1e-4;
    		   // Values at s + kH.
    		   double fwd[] = new double[3];
    		   // Values at s - kH.
    		   double bwd[] = new double[3];
    		   loss.Evaluate(s + kH, fwd);
    		   loss.Evaluate(s - kH, bwd);

    		   // First derivative.
    		   final double fd_1 = (fwd[0] - bwd[0]) / (2 * kH);
    		   if (Math.abs(fd_1 - rho[1]) > 1e-6) {
    			   System.err.println("In " + testName + " (Math.abs(fd_1 - rho[1]) > 1e-6");
    			   passed = false;
    		   }

    		   // Second derivative.
    		   final double fd_2 = (fwd[0] - 2*rho[0] + bwd[0]) / (kH * kH);
    		   if (Math.abs(fd_2 - rho[2]) > 1e-6) {
    			   System.err.println("In " + testName + " Math.abs(fd_2 - rho[2]) > 1e-6");
    			   passed = false;
    		   }
    		   if (passed) {
    			   System.out.println(testName + " passed all tests");
    		   }
    		 }
    		 
    		// Try two values of the scaling a = 0.7 and 1.3
    		// (where scaling makes sense) and of the squared norm
    		// s = 0.357 and 1.792
    		//
    		// Note that for the Huber loss the test exercises both code paths
    		//  (i.e. both small and large values of s).

    		public void LossFunctionTrivialLoss() {
    		  //LossFunctionTrivialLoss_0.357 passed all tests
    		  //LossFunctionTrivialLoss_1.792 passed all tests
    		  AssertLossFunctionIsValid(new TrivialLoss(), 0.357, "LossFunctionTrivialLoss_0.357");
    		  AssertLossFunctionIsValid(new TrivialLoss(), 1.792, "LossFunctionTrivialLoss_1.792");
    		}

    		public void LossFunctionHuberLoss() {
    			  //LossFunctionHuberLoss(0.7)_0.357 passed all tests
    			  //LossFunctionHuberLoss(0.7)_1.792 passed all tests
    			  //LossFunctionHuberLoss(1.3)_0.357 passed all tests
    			  //LossFunctionHuberLoss(1.3)_1.792 passed all tests
    			  AssertLossFunctionIsValid(new HuberLoss(0.7), 0.357, "LossFunctionHuberLoss(0.7)_0.357");
    			  AssertLossFunctionIsValid(new HuberLoss(0.7), 1.792, "LossFunctionHuberLoss(0.7)_1.792");
    			  AssertLossFunctionIsValid(new HuberLoss(1.3), 0.357, "LossFunctionHuberLoss(1.3)_0.357");
    			  AssertLossFunctionIsValid(new HuberLoss(1.3), 1.792, "LossFunctionHuberLoss(1.3)_1.792");
    		}
    		
    		public void LossFunctionSoftLOneLoss() {
    			  //LossFunctionSoftLOneLoss(0.7)_0.357 passed all tests
    			  //LossFunctionSoftLOneLoss(0.7)_1.792 passed all tests
    			  //LossFunctionSoftLOneLoss(1.3)_0.357 passed all tests
    			  //LossFunctionSoftLOneLoss(1.3)_1.792 passed all tests
    			  AssertLossFunctionIsValid(new SoftLOneLoss(0.7), 0.357, "LossFunctionSoftLOneLoss(0.7)_0.357");
    			  AssertLossFunctionIsValid(new SoftLOneLoss(0.7), 1.792, "LossFunctionSoftLOneLoss(0.7)_1.792");
    			  AssertLossFunctionIsValid(new SoftLOneLoss(1.3), 0.357, "LossFunctionSoftLOneLoss(1.3)_0.357");
    			  AssertLossFunctionIsValid(new SoftLOneLoss(1.3), 1.792, "LossFunctionSoftLOneLoss(1.3)_1.792");
    		}

    		public void LossFunctionCauchyLoss() {
    			  //LossFunctionCauchyLoss(0.7)_0.357 passed all tests
    			  //LossFunctionCauchyLoss(0.7)_1.792 passed all tests
    			  //LossFunctionCauchyLoss(1.3)_0.357 passed all tests
    			  //LossFunctionCauchyLoss(1.3)_1.792 passed all tests
    			  AssertLossFunctionIsValid(new CauchyLoss(0.7), 0.357, "LossFunctionCauchyLoss(0.7)_0.357");
    			  AssertLossFunctionIsValid(new CauchyLoss(0.7), 1.792, "LossFunctionCauchyLoss(0.7)_1.792");
    			  AssertLossFunctionIsValid(new CauchyLoss(1.3), 0.357, "LossFunctionCauchyLoss(1.3)_0.357");
    			  AssertLossFunctionIsValid(new CauchyLoss(1.3), 1.792, "LossFunctionCauchyLoss(1.3)_1.792");
    		}
    		
    		public void LossFunctionArctanLoss() {
    			  //LossFunctionArctanLoss(0.7)_0.357 passed all tests
    			  //LossFunctionArctanLoss(0.7)_1.792 passed all tests
    			  //LossFunctionArctanLoss(1.3)_0.357 passed all tests
    			  //LossFunctionArctanLoss(1.3)_1.792 passed all tests
    			  AssertLossFunctionIsValid(new ArctanLoss(0.7), 0.357, "LossFunctionArctanLoss(0.7)_0.357");
    			  AssertLossFunctionIsValid(new ArctanLoss(0.7), 1.792, "LossFunctionArctanLoss(0.7)_1.792");
    			  AssertLossFunctionIsValid(new ArctanLoss(1.3), 0.357, "LossFunctionArctanLoss(1.3)_0.357");
    			  AssertLossFunctionIsValid(new ArctanLoss(1.3), 1.792, "LossFunctionArctanLoss(1.3)_1.792");
    		}

    		public void LossFunctionTolerantLoss() {
    			  //LossFunctionTolerantLoss(0.7, 0.4)_0.357 passed all tests
    			  //LossFunctionTolerantLoss(0.7, 0.4)_1.792 passed all tests
    			  //LossFunctionTolerantLoss(0.7, 0.4)_55.5 passed all tests
    			  //LossFunctionTolerantLoss(1.3, 0.1)_0.357 passed all tests
    			  //LossFunctionTolerantLoss(1.3, 0.1)_1.792 passed all tests
    			  //LossFunctionTolerantLoss(1.3, 0.1)_55.5 passed all tests
    			  //(new TolerantLoss(0.7, 0.4)).Evaluate(0.0, rho) passed
    			  //LossFunctionTolerantLoss(20.0, 1.0)_56.6 passed all tests
    			  //LossFunctionTolerantLoss(20.0, 1.0)_56.7 passed all tests
    			  //LossFunctionTolerantLoss(20.0, 1.0)_56.8 passed all tests
    			  //LossFunctionTolerantLoss(20.0, 1.0)_1020.0 passed all tests
    			  AssertLossFunctionIsValid(new TolerantLoss(0.7, 0.4), 0.357, "LossFunctionTolerantLoss(0.7, 0.4)_0.357");
    			  AssertLossFunctionIsValid(new TolerantLoss(0.7, 0.4), 1.792, "LossFunctionTolerantLoss(0.7, 0.4)_1.792");
    			  AssertLossFunctionIsValid(new TolerantLoss(0.7, 0.4), 55.5, "LossFunctionTolerantLoss(0.7, 0.4)_55.5");
    			  AssertLossFunctionIsValid(new TolerantLoss(1.3, 0.1), 0.357, "LossFunctionTolerantLoss(1.3, 0.1)_0.357");
    			  AssertLossFunctionIsValid(new TolerantLoss(1.3, 0.1), 1.792, "LossFunctionTolerantLoss(1.3, 0.1)_1.792");
    			  AssertLossFunctionIsValid(new TolerantLoss(1.3, 0.1), 55.5, "LossFunctionTolerantLoss(1.3, 0.1)_55.5");
    			  // Check the value at zero is actually zero.
    			  double rho[] = new double[3];
    			  (new TolerantLoss(0.7, 0.4)).Evaluate(0.0, rho);
    			  if (Math.abs(rho[0]) > 1e-6) {
    				  System.err.println("In LossFunctionTolerantLoss() Math.abs(rho[0]) > 1e-6");
    			  }
    			  else {
    				  System.out.println("(new TolerantLoss(0.7, 0.4)).Evaluate(0.0, rho) passed");
    			  }
    			  // Check that loss before and after the approximation threshold are good.
    			  // A threshold of 36.7 is used by the implementation.
    			  AssertLossFunctionIsValid(new TolerantLoss(20.0, 1.0), 20.0 + 36.6, "LossFunctionTolerantLoss(20.0, 1.0)_56.6");
    			  AssertLossFunctionIsValid(new TolerantLoss(20.0, 1.0), 20.0 + 36.7, "LossFunctionTolerantLoss(20.0, 1.0)_56.7");
    			  AssertLossFunctionIsValid(new TolerantLoss(20.0, 1.0), 20.0 + 36.8, "LossFunctionTolerantLoss(20.0, 1.0)_56.8");
    			  AssertLossFunctionIsValid(new TolerantLoss(20.0, 1.0), 20.0 + 1000.0, "LossFunctionTolerantLoss(20.0, 1.0)_1020.0");
    		}
    		
    		public void LossFunctionTukeyLoss() {
    			  //LossFunctionTukeyLoss(0.7)_0.357 passed all tests
    			  //LossFunctionTukeyLoss(0.7)_1.792 passed all tests
    			  //LossFunctionTukeyLoss(1.3)_0.357 passed all tests
    			  //LossFunctionTukeyLoss(1.3)_1.792 passed all tests
    			  AssertLossFunctionIsValid(new TukeyLoss(0.7), 0.357, "LossFunctionTukeyLoss(0.7)_0.357");
    			  AssertLossFunctionIsValid(new TukeyLoss(0.7), 1.792, "LossFunctionTukeyLoss(0.7)_1.792");
    			  AssertLossFunctionIsValid(new TukeyLoss(1.3), 0.357, "LossFunctionTukeyLoss(1.3)_0.357");
    			  AssertLossFunctionIsValid(new TukeyLoss(1.3), 1.792, "LossFunctionTukeyLoss(1.3)_1.792");
    		}

    		public void LossFunctionComposedLoss() {
    			  //LossFunctionComposedLoss()_0.357 passed all tests
    			  //LossFunctionComposedLoss()_1.792 passed all tests
    			  //LossFunctionComposedLoss()_0.357 passed all tests
    			  //LossFunctionComposedLoss()_1.792 passed all tests
    			  {
    			    HuberLoss f = new HuberLoss(0.7);
    			    CauchyLoss g = new CauchyLoss(1.3);
    			    ComposedLoss c = new ComposedLoss(f, Ownership.DO_NOT_TAKE_OWNERSHIP, g, Ownership.DO_NOT_TAKE_OWNERSHIP);
    			    AssertLossFunctionIsValid(c, 0.357, "LossFunctionComposedLoss()_0.357");
    			    AssertLossFunctionIsValid(c, 1.792, "LossFunctionComposedLoss()_1.792");
    			  }
    			  {
    			    CauchyLoss f = new CauchyLoss(0.7);
    			    HuberLoss g = new HuberLoss(1.3);
    			    ComposedLoss c = new ComposedLoss(f, Ownership.DO_NOT_TAKE_OWNERSHIP, g, Ownership.DO_NOT_TAKE_OWNERSHIP);
    			    AssertLossFunctionIsValid(c, 0.357, "LossFunctionComposedLoss()_0.357");
    			    AssertLossFunctionIsValid(c, 1.792, "LossFunctionComposedLoss()_1.792");
    			  }
    		}

    		public void LossFunctionScaledLoss() {
    			  //scaled_loss_null_0.323 passed all tests
    			  //scaled_loss_Trivial_0.357 passed all tests
    			  //scaled_loss_Huber_1.792 passed all tests
    			  //scaled_loss_SoftLOne_1.792 passed all tests
    			  //scaled_loss_Cauchy_1.792 passed all tests
    			  //scaled_loss_Arctan_1.792 passed all tests
    			  //scaled_loss_Tolerant_1.792 passed all tests
    			  //scaled_loss_Composed_1.792 passed all tests
    			  // Wrap a few loss functions, and a few scale factors. This can't combine
    			  // construction with the call to AssertLossFunctionIsValid() because Apple's
    			  // GCC is unable to eliminate the copy of ScaledLoss, which is not copyable.
    			  {
    			    ScaledLoss scaled_loss = new ScaledLoss(null, 6, Ownership.TAKE_OWNERSHIP);
    			    AssertLossFunctionIsValid(scaled_loss, 0.323, "scaled_loss_null_0.323");
    			  }
    			  {
    			    ScaledLoss scaled_loss = new ScaledLoss(new TrivialLoss(), 10, Ownership.TAKE_OWNERSHIP);
    			    AssertLossFunctionIsValid(scaled_loss, 0.357, "scaled_loss_Trivial_0.357");
    			  }
    			  {
    			    ScaledLoss scaled_loss = new ScaledLoss(new HuberLoss(0.7), 0.1, Ownership.TAKE_OWNERSHIP);
    			    AssertLossFunctionIsValid(scaled_loss, 1.792, "scaled_loss_Huber_1.792");
    			  }
    			  {
    			    ScaledLoss scaled_loss = new ScaledLoss(new SoftLOneLoss(1.3), 0.1, Ownership.TAKE_OWNERSHIP);
    			    AssertLossFunctionIsValid(scaled_loss, 1.792, "scaled_loss_SoftLOne_1.792");
    			  }
    			  {
    			    ScaledLoss scaled_loss = new ScaledLoss(new CauchyLoss(1.3), 10, Ownership.TAKE_OWNERSHIP);
    			    AssertLossFunctionIsValid(scaled_loss, 1.792, "scaled_loss_Cauchy_1.792");
    			  }
    			  {
    			    ScaledLoss scaled_loss = new ScaledLoss(new ArctanLoss(1.3), 10, Ownership.TAKE_OWNERSHIP);
    			    AssertLossFunctionIsValid(scaled_loss, 1.792, "scaled_loss_Arctan_1.792");
    			  }
    			  {
    			    ScaledLoss scaled_loss = new ScaledLoss(
    			        new TolerantLoss(1.3, 0.1), 10, Ownership.TAKE_OWNERSHIP);
    			    AssertLossFunctionIsValid(scaled_loss, 1.792, "scaled_loss_Tolerant_1.792");
    			  }
    			  {
    			    ScaledLoss scaled_loss = new ScaledLoss(
    			        new ComposedLoss(
    			            new HuberLoss(0.8), Ownership.TAKE_OWNERSHIP,
    			            new TolerantLoss(1.3, 0.5), Ownership.TAKE_OWNERSHIP), 10, Ownership.TAKE_OWNERSHIP);
    			    AssertLossFunctionIsValid(scaled_loss, 1.792, "scaled_loss_Composed_1.792");
    			  }
    			}
    		
    		public void LossFunctionLossFunctionWrapper() {
    			  // LossFunctionLossFunctionWrapper() passed all tests
    			  boolean passed = true;
    			  // Initialization
    			  HuberLoss loss_function1 = new HuberLoss(1.0);
    			  LossFunctionWrapper loss_function_wrapper = new LossFunctionWrapper(new HuberLoss(1.0),
    			                                            Ownership.TAKE_OWNERSHIP);

    			  double s = 0.862;
    			  double rho_gold[] = new double[3];
    			  double rho[] = new double[3];
    			  loss_function1.Evaluate(s, rho_gold);
    			  loss_function_wrapper.Evaluate(s, rho);
    			  for (int i = 0; i < 3; ++i) {
    			    if (Math.abs(rho[i] - rho_gold[i]) >  1e-12) {
    			    	System.err.println("In LossFunctionLossFunctionWrapper() function1 failed for i = " + i);
    			    	passed = false;
    			    }
    			  }

    			  // Resetting
    			  HuberLoss loss_function2 = new HuberLoss(0.5);
    			  loss_function_wrapper = new LossFunctionWrapper(new HuberLoss(0.5), Ownership.TAKE_OWNERSHIP);
    			  loss_function_wrapper.Evaluate(s, rho);
    			  loss_function2.Evaluate(s, rho_gold);
    			  for (int i = 0; i < 3; ++i) {
    				  if (Math.abs(rho[i] - rho_gold[i]) >  1e-12) {
      			    	System.err.println("In LossFunctionLossFunctionWrapper() function2 failed for i = " + i);
      			    	passed = false;
      			      }
    			  }

    			  // Not taking ownership.
    			  HuberLoss loss_function3 = new HuberLoss(0.3);
    			  loss_function_wrapper = new LossFunctionWrapper(loss_function3, Ownership.DO_NOT_TAKE_OWNERSHIP);
    			  loss_function_wrapper.Evaluate(s, rho);
    			  loss_function3.Evaluate(s, rho_gold);
    			  for (int i = 0; i < 3; ++i) {
    				  if (Math.abs(rho[i] - rho_gold[i]) >  1e-12) {
        			    	System.err.println("In LossFunctionLossFunctionWrapper() function3 failed for i = " + i);
        			    	passed = false;
        			  }
    			  }

    			  // Set to NULL
    			  TrivialLoss loss_function4 = new TrivialLoss();
    			  loss_function_wrapper = new LossFunctionWrapper(null, Ownership.TAKE_OWNERSHIP);
    			  loss_function_wrapper.Evaluate(s, rho);
    			  loss_function4.Evaluate(s, rho_gold);
    			  for (int i = 0; i < 3; ++i) {
    				  if (Math.abs(rho[i] - rho_gold[i]) >  1e-12) {
      			    	System.err.println("In LossFunctionLossFunctionWrapper() function4 TAKE_OWNERSHIP failed for i = " + i);
      			    	passed = false;
      			      }
    			  }

    			  // Set to NULL, not taking ownership
    			  loss_function_wrapper = new LossFunctionWrapper(null, Ownership.DO_NOT_TAKE_OWNERSHIP);
    			  loss_function_wrapper.Evaluate(s, rho);
    			  loss_function4.Evaluate(s, rho_gold);
    			  for (int i = 0; i < 3; ++i) {
    				  if (Math.abs(rho[i] - rho_gold[i]) >  1e-12) {
        			    	System.err.println("In LossFunctionLossFunctionWrapper() function4 DO_NOT_TAKE_OWNERSHIP failed for i = " + i);
        			    	passed = false;
        			  }
    			  }
    			  if (passed) {
    				  System.out.println("LossFunctionLossFunctionWrapper() passed all tests");
    			  }

    			}

    		public void RandomVector(Vector<Double> v) {
    			  for (int r = 0; r < v.size(); ++r)
    			    v.set(r,2 * RandDouble() - 1);
    		}

    	    public void RandomMatrix(Matrix m) {
    			  for (int r = 0; r < m.getRowDimension(); ++r) {
    			    for (int c = 0; c < m.getColumnDimension(); ++c) {
    			      m.set(r, c, 2 * RandDouble() - 1);
    			    }
    			  }
    		}
    	    
    	    public void NormalPriorTestResidualAtRandomPosition() {
    	    	  // NormalPriorTestResidualAtRandomPosition() passed all tests
    	    	  boolean passed = true;
    	    	  int i, j;

    	    	  for (int num_rows = 1; num_rows < 5; ++num_rows) {
    	    	    for (int num_cols = 1; num_cols < 5; ++num_cols) {
    	    	      Vector<Double> b = new Vector<Double>(num_cols);
    	    	      for (i = 0; i < num_cols; i++) {
    	    	    	  b.add(0.0);
    	    	      }
    	    	      RandomVector(b);

    	    	      Matrix A = new Matrix(num_rows, num_cols);
    	    	      RandomMatrix(A);

    	    	      Vector<double[]> x = new Vector<double[]>(num_cols);
    	    	      for (i = 0; i < num_cols; ++i) {
    	    	        double p[] = new double[] {2 * RandDouble() - 1};
    	    	        x.add(p);
    	    	      }

    	    	      double [][]jacobian = new double[num_rows][num_cols];
    	    	      double[] residuals = new double[num_rows];

    	    	      NormalPrior prior = new NormalPrior(A, b);
    	    	      prior.Evaluate(x, residuals, jacobian);

    	    	      // Compare the norm of the residual
    	    	      double residual_diff_norm = 0;
    	    	      double Axb[] = new double[num_rows];
    	    	      for (i = 0; i < num_rows; i++) {
    	    	    	  for (j = 0; j < num_cols; j++) {
    	    	    		  Axb[i] += A.get(i, j) * (x.get(j)[0] - b.get(j));
    	    	    	  }
    	    	      }
    	    	      double diff;
    	    	      for (i = 0; i < num_rows; i++) {
    	    	    	  diff = residuals[i] - Axb[i];
    	    	    	  residual_diff_norm += (diff * diff);
    	    	      }
    	    	      if (residual_diff_norm > 1.0E-10) {
    	    	    	  System.err.println("In NormalPriorTestResidualAtRandomPosition() residual_diff_norm > 1.0E-10");
    	    	    	  passed = false;
    	    	      }

    	    	      // Compare the jacobians
    	    	      double jacobian_diff_norm = 0.0;
    	    	      for (i = 0; i < num_rows; i++) {
    	    	    	  for (j = 0; j < num_cols; j++) {
    	    	    		  diff = jacobian[i][j] - A.get(i,j);
    	    	    		  jacobian_diff_norm += (diff * diff);
    	    	    	  }
    	    	      }
    	    	      jacobian_diff_norm = Math.sqrt(jacobian_diff_norm);
    	    	      if (jacobian_diff_norm > 1.0E-10) {
    	    	    	  System.err.println("In NormalPriorTestResidualAtRandomPosition() jacobian_diff_norm > 1.0E-10");
    	    	    	  passed = false;
    	    	      }

    	    	      for (i = 0; i < x.size(); i++) {
    	    	    	  x.set(i, null);  
    	    	      }
    	    	      x = null;
    	    	      for (i = 0; i < jacobian.length; i++) {
    	    	    	  jacobian[i] = null;
    	    	      }
    	    	      jacobian = null;
    	    	    }
    	    	  }
    	    	  if (passed) {
    	    		  System.out.println("NormalPriorTestResidualAtRandomPosition() passed all tests");
    	    	  }
    	    	}

    	    public void NormalPriorTestResidualAtRandomPositionNullJacobians() {
    	    	 // NormalPriorTestResidualAtRandomPositionNullJacobians() passed all tests
    	    	  boolean passed = true;
    	    	  int i, j;

    	    	  for (int num_rows = 1; num_rows < 5; ++num_rows) {
    	    	    for (int num_cols = 1; num_cols < 5; ++num_cols) {
    	    	      Vector<Double> b = new Vector<Double>(num_cols);
      	    	      for (i = 0; i < num_cols; i++) {
      	    	    	  b.add(0.0);
      	    	      }
      	    	      RandomVector(b);

      	    	      Matrix A = new Matrix(num_rows, num_cols);
      	    	      RandomMatrix(A);

      	    	      Vector<double[]> x = new Vector<double[]>(num_cols);
      	    	      for (i = 0; i < num_cols; ++i) {
      	    	        double p[] = new double[] {2 * RandDouble() - 1};
      	    	        x.add(p);
      	    	      }

    	    	      double jacobians[][] = new double[1][];
    	    	      jacobians[0] = null;

    	    	      double[] residuals = new double[num_rows];

    	    	      NormalPrior prior = new NormalPrior(A, b);
    	    	      prior.Evaluate(x, residuals, jacobians);

    	    	      // Compare the norm of the residual
    	    	      double residual_diff_norm = 0;
    	    	      double Axb[] = new double[num_rows];
    	    	      for (i = 0; i < num_rows; i++) {
    	    	    	  for (j = 0; j < num_cols; j++) {
    	    	    		  Axb[i] += A.get(i, j) * (x.get(j)[0] - b.get(j));
    	    	    	  }
    	    	      }
    	    	      double diff;
    	    	      for (i = 0; i < num_rows; i++) {
    	    	    	  diff = residuals[i] - Axb[i];
    	    	    	  residual_diff_norm += (diff * diff);
    	    	      }
    	    	      if (residual_diff_norm > 1.0E-10) {
    	    	    	  System.err.println("In NormalPriorTestResidualAtRandomPositionNullJacobians() residual_diff_norm > 1.0E-10");
    	    	    	  passed = false;
    	    	      }

    	    	      prior.Evaluate(x, residuals, null);
    	    	      // Compare the norm of the residual
    	    	      residual_diff_norm = 0;
    	    	      for (i = 0; i < num_rows; i++) {
    	    	    	  Axb[i] = 0.0;
    	    	    	  for (j = 0; j < num_cols; j++) {
    	    	    		  Axb[i] += A.get(i, j) * (x.get(j)[0] - b.get(j));
    	    	    	  }
    	    	      }
    	    	      for (i = 0; i < num_rows; i++) {
    	    	    	  diff = residuals[i] - Axb[i];
    	    	    	  residual_diff_norm += (diff * diff);
    	    	      }
    	    	      if (residual_diff_norm > 1.0E-10) {
    	    	    	  System.err.println("In NormalPriorTestResidualAtRandomPositionNullJacobians() residual_diff_norm > 1.0E-10");
    	    	    	  passed = false;
    	    	      }


    	    	      for (i = 0; i < x.size(); i++) {
    	    	    	  x.set(i, null);  
    	    	      }
    	    	      x = null;
    	    	    }
    	    	  }
    	    	  if (passed) {
    	    		  System.out.println("NormalPriorTestResidualAtRandomPositionNullJacobians() passed all tests");
    	    	  }
    	    	}

    	    public void SolverOptionsDefaultTrustRegionOptionsAreValid() {
    	    	  // SolverOptionsDefaultTrustRegionOptionsAreValid() passed all tests
    	    	  SolverOptions options = new SolverOptions();
    	    	  options.minimizer_type = MinimizerType.TRUST_REGION;
    	    	  String error[] = new String[1];
    	    	  if (!options.IsValid(error)) {
    	    		  System.err.println("In SolverOptionsDefaultTrustRegionOptionsAreValid() " + error[0]);
    	    	  }
    	    	  else {
    	    		  System.out.println("SolverOptionsDefaultTrustRegionOptionsAreValid() passed all tests");
    	    	  }
    	    }
    	    
    	    public void SolverOptionsDefaultLineSearchOptionsAreValid() {
    	    	  // SolverOptionsDefaultLineSearchOptionsAreValid() passed all tests
    	    	  SolverOptions options = new SolverOptions();
    	    	  options.minimizer_type = MinimizerType.LINE_SEARCH;
    	    	  String error[] = new String[1];
    	    	  if (!options.IsValid(error)) {
    	    		  System.err.println("In SolverOptionsDefaultLineSearchOptionsAreValid() " + error[0]);
    	    	  }
    	    	  else {
    	    		  System.out.println("SolverOptionsDefaultLineSearchOptionsAreValid() passed all tests");
    	    	  }
    	    }
    	    
    	    // A cost function that simply returns its argument.
    	    class UnaryIdentityCostFunction extends SizedCostFunction {
    	     public UnaryIdentityCostFunction() {
    	    	 super(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    	     }
    	     
    	     public boolean Evaluate(Vector<double[]> parameters,
    	                            double[] residuals,
    	                            double[][] jacobians) {
    	        residuals[0] = parameters.get(0)[0];
    	        if (jacobians != null && jacobians[0] != null) {
    	          jacobians[0][0] = 1.0;
    	        }
    	        return true;
    	      }
    	    };
    	    
    	    public void SolverTrustRegionProblemHasNoParameterBlocks() {
    	    	  // SolverTrustRegionProblemHasNoParameterBlocks() passed all tests
    	    	  boolean passed = true;
    	    	  ProblemImpl problem = new ProblemImpl();
    	    	  SolverOptions options = new SolverOptions();
    	    	  options.minimizer_type = MinimizerType.TRUST_REGION;
    	    	  SolverSummary summary = new SolverSummary();
    	    	  Solve(options, problem, summary);
    	    	  if (summary.termination_type != TerminationType.CONVERGENCE) {
    	    		  System.err.println("In SolverTrustRegionProblemHasNoParameterBlocks() summary.termination_type != TerminationType.CONVERGENCE");
    	    		  passed = false;
    	    	  }
    	    	  if (!summary.message[0].equalsIgnoreCase("Function tolerance reached. No non-constant parameter blocks found.")) {
    	    		  System.err.println("In SolverTrustRegionProblemHasNoParameterBlocks() message is not Function tolerance reached.");
    	    		  passed = false;
    	    	  }
    	    	  if (passed) {
    	    		  System.out.println("SolverTrustRegionProblemHasNoParameterBlocks() passed all tests");
    	    	  }
    	    	}
    	    
    	    public void SolverLineSearchProblemHasNoParameterBlocks() {
    	      // SolverLineSearchProblemHasNoParameterBlocks() passed all tests
    	      boolean passed = true;
  	    	  ProblemImpl problem = new ProblemImpl();
  	    	  SolverOptions options = new SolverOptions();
  	    	  options.minimizer_type = MinimizerType.LINE_SEARCH;
  	    	  SolverSummary summary = new SolverSummary();
  	    	  Solve(options, problem, summary);
  	    	  if (summary.termination_type != TerminationType.CONVERGENCE) {
  	    		  System.err.println("In SolverLineSearchProblemHasNoParameterBlocks() summary.termination_type != TerminationType.CONVERGENCE");
  	    		  passed = false;
  	    	  }
  	    	  if (!summary.message[0].equalsIgnoreCase("Function tolerance reached. No non-constant parameter blocks found.")) {
  	    		  System.err.println("In SolverLineSearchProblemHasNoParameterBlocks() message is not Function tolerance reached.");
  	    		  passed = false;
  	    	  }
  	    	  if (passed) {
  	    		  System.out.println("SolverLineSearchProblemHasNoParameterBlocks() passed all tests");
  	    	  }
  	    	}

    	    public void SolverTrustRegionProblemHasZeroResiduals() {
    	    	  // SolverTrustRegionProblemHasZeroResiduals() passed all tests
    	    	  boolean passed = true;
    	    	  ProblemImpl problem = new ProblemImpl();
    	    	  double x[] = new double[] {1};
    	    	  problem.AddParameterBlock(x, 1);
    	    	  SolverOptions options = new SolverOptions();
    	    	  options.minimizer_type = MinimizerType.TRUST_REGION;
    	    	  SolverSummary summary = new SolverSummary();
    	    	  Solve(options, problem, summary);
    	    	  if (summary.termination_type != TerminationType.CONVERGENCE) {
    	    		  System.err.println("In SolverTrustRegionProblemHasZeroResiduals() summary.termination_type != TerminationType.CONVERGENCE");
    	    		  passed = false;
    	    	  }
    	    	  if (!summary.message[0].equalsIgnoreCase("Function tolerance reached. No non-constant parameter blocks found.")) {
    	    		  System.err.println("In SolverTrustRegionProblemHasZeroResiduals() message is not Function tolerance reached.");
    	    		  passed = false;
    	    	  }
    	    	  if (passed) {
    	    		  System.out.println("SolverTrustRegionProblemHasZeroResiduals() passed all tests");
    	    	  }
    	    }
    	    
    	    public void SolverLineSearchProblemHasZeroResiduals() {
    	      // SolverLineSearchProblemHasZeroResiduals() passed all tests
    	      boolean passed = true;
  	    	  ProblemImpl problem = new ProblemImpl();
  	    	  double x[] = new double[] {1};
  	    	  problem.AddParameterBlock(x, 1);
  	    	  SolverOptions options = new SolverOptions();
  	    	  options.minimizer_type = MinimizerType.LINE_SEARCH;
  	    	  SolverSummary summary = new SolverSummary();
  	    	  Solve(options, problem, summary);
  	    	  if (summary.termination_type != TerminationType.CONVERGENCE) {
  	    		  System.err.println("In SolverLineSearchProblemHasZeroResiduals() summary.termination_type != TerminationType.CONVERGENCE");
  	    		  passed = false;
  	    	  }
  	    	  if (!summary.message[0].equalsIgnoreCase("Function tolerance reached. No non-constant parameter blocks found.")) {
  	    		  System.err.println("In SolverLineSearchProblemHasZeroResiduals() message is not Function tolerance reached.");
  	    		  passed = false;
  	    	  }
  	    	  if (passed) {
  	    		  System.out.println("SolverLineSearchProblemHasZeroResiduals() passed all tests");
  	    	  }
    	    }
    	    
    	    public void SolverTrustRegionProblemIsConstant() {
    	    	  // SolverTrustRegionProblemIsConstant() passed all tests
    	    	  boolean passed = true;
    	    	  ProblemImpl problem = new ProblemImpl();
    	    	  double x[] = new double[] {1};
    	    	  problem.AddResidualBlock(new UnaryIdentityCostFunction(), null, x);
    	    	  problem.SetParameterBlockConstant(x);
    	    	  SolverOptions options = new SolverOptions();
    	    	  options.minimizer_type = MinimizerType.TRUST_REGION;
    	    	  SolverSummary summary = new SolverSummary();
    	    	  Solve(options, problem, summary);
    	    	  if (summary.termination_type != TerminationType.CONVERGENCE) {
    	    		  System.err.println("In SolverTrustRegionProblemIsConstant() summary.termination_type != TerminationType.CONVERGENCE");
    	    		  passed = false;
    	    	  }
    	    	  if (summary.initial_cost != 1.0 / 2.0) {
    	    		  System.err.println("In SolverTrustRegionProblemIsConstant() summary.initial_cost != 1.0 / 2.0");
    	    		  passed = false;
    	    	  }
    	    	  if (summary.final_cost != 1.0 / 2.0) {
    	    		  System.err.println("In SolverTrustRegionProblemIsConstant() summary.final_cost != 1.0 / 2.0");
    	    		  passed = false;
    	    	  }
    	    	  if (passed) {
    	    		  System.out.println("SolverTrustRegionProblemIsConstant() passed all tests");
    	    	  }
    	    }
    	    
    	    public void SolverLineSearchProblemIsConstant() {
    	      // SolverLineSearchProblemIsConstant() passed all tests
    	      boolean passed = true;
  	    	  ProblemImpl problem = new ProblemImpl();
  	    	  double x[] = new double[] {1};
  	    	  problem.AddResidualBlock(new UnaryIdentityCostFunction(), null, x);
  	    	  problem.SetParameterBlockConstant(x);
  	    	  SolverOptions options = new SolverOptions();
  	    	  options.minimizer_type = MinimizerType.LINE_SEARCH;
  	    	  SolverSummary summary = new SolverSummary();
  	    	  Solve(options, problem, summary);
  	    	  if (summary.termination_type != TerminationType.CONVERGENCE) {
  	    		  System.err.println("In SolverLineSearchProblemIsConstant() summary.termination_type != TerminationType.CONVERGENCE");
  	    		  passed = false;
  	    	  }
  	    	  if (summary.initial_cost != 1.0 / 2.0) {
  	    		  System.err.println("In SolverLineSearchProblemIsConstant() summary.initial_cost != 1.0 / 2.0");
  	    		  passed = false;
  	    	  }
  	    	  if (summary.final_cost != 1.0 / 2.0) {
  	    		  System.err.println("In SolverLineSearchProblemIsConstant() summary.final_cost != 1.0 / 2.0");
  	    		  passed = false;
  	    	  }
  	    	  if (passed) {
  	    		  System.out.println("SolverLineSearchProblemIsConstant() passed all tests");
  	    	  }
    	    }

    	    public void SolverIterativeLinearSolverForDogleg() {
    	    	  // SolverIterativeLinearSolverForDogleg() passed all tests
    	    	  boolean passed = true;
    	    	  SolverOptions options = new SolverOptions();
    	    	  options.trust_region_strategy_type = TrustRegionStrategyType.DOGLEG;
    	    	  String message[] = new String[1];
    	    	  options.linear_solver_type = LinearSolverType.ITERATIVE_SCHUR;
    	    	  if (options.IsValid(message)) {
    	    		  System.err.println("In SolverIterativeLinearSolverForDogleg() options.IsValid(message) == true");
    	    		  passed = false;
    	    	  }

    	    	  options.linear_solver_type = LinearSolverType.CGNR;
    	    	  if (options.IsValid(message)) {
    	    		  System.err.println("In SolverIterativeLinearSolverForDogleg() options.IsValid(message) == true");
    	    		  passed = false;
    	    	  }
    	    	  if (passed) {
    	    		  System.out.println("SolverIterativeLinearSolverForDogleg() passed all tests");
    	    	  }
    	    }
    	    
    	    public void SolverLinearSolverTypeNormalOperation() {
    	    	  // SolverLinearSolverTypeNormalOperation() passed all tests
    	    	  boolean passed = true;
    	    	  SolverOptions options = new SolverOptions();
    	    	  options.linear_solver_type = LinearSolverType.DENSE_QR;

    	    	  String message[] = new String[1];
    	    	  if (!options.IsValid(message)) {
    	    		  System.err.println("In SolverLinearSolverTypeNormalOperation() options.IsValid(message) = false");
    	    		  passed = false;
    	    	  }

    	    	  options.linear_solver_type = LinearSolverType.DENSE_NORMAL_CHOLESKY;
    	    	  if (!options.IsValid(message)) {
    	    		  System.err.println("In SolverLinearSolverTypeNormalOperation() options.IsValid(message) = false");
    	    		  passed = false;
    	    	  }

    	    	  options.linear_solver_type = LinearSolverType.DENSE_SCHUR;
    	    	  if (!options.IsValid(message)) {
    	    		  System.err.println("In SolverLinearSolverTypeNormalOperation() options.IsValid(message) = false");
    	    		  passed = false;
    	    	  }

    	    	  options.linear_solver_type = LinearSolverType.ITERATIVE_SCHUR;
    	    	  if (!options.IsValid(message)) {
    	    		  System.err.println("In SolverLinearSolverTypeNormalOperation() options.IsValid(message) = false");
    	    		  passed = false;
    	    	  }
                  if (passed) {
                	  System.out.println("SolverLinearSolverTypeNormalOperation() passed all tests");
                  }
    	    	}

    	    class NoOpEvaluationCallback extends EvaluationCallback {
    	    		  public NoOpEvaluationCallback() {
    	    			  super();
    	    		  }
    	    		  
    	    		  public void PrepareForEvaluation(boolean evaluate_jacobians,
    	    		                                    boolean new_evaluation_point) {
    	    		    //(void) evaluate_jacobians;
    	    		    //(void) new_evaluation_point;
    	    		  }
    	    };
    	    
    	    public void SolverCantMixEvaluationCallbackWithInnerIterations() {
    	    	  // SolverCantMixEvaluationCallbackWithInnerIterations() passed all tests
    	    	  boolean passed = true;
    	    	  SolverOptions options = new SolverOptions();
    	    	  NoOpEvaluationCallback evaluation_callback = new NoOpEvaluationCallback();
    	    	  String message[] = new String[1];

    	    	  // Can't combine them.
    	    	  options.use_inner_iterations = true;
    	    	  options.evaluation_callback = evaluation_callback;
    	    	  if (options.IsValid(message)) {
    	    		  System.err.println("In SolverCantMixEvaluationCallbackWithInnerIterations() options.IsValid(message) == true");
    	    		  passed = false;
    	    	  }

    	    	  // Either or none is OK.
    	    	  options.use_inner_iterations = false;
    	    	  options.evaluation_callback = evaluation_callback;
    	    	  if (!options.IsValid(message)) {
    	    		  System.err.println("In SolverCantMixEvaluationCallbackWithInnerIterations() options.IsValid(message) == false");
    	    		  passed = false;
    	    	  }

    	    	  options.use_inner_iterations = true;
    	    	  options.evaluation_callback = null;
    	    	  if (!options.IsValid(message)) {
    	    		  System.err.println("In SolverCantMixEvaluationCallbackWithInnerIterations() options.IsValid(message) == false");
    	    		  passed = false;
    	    	  }

    	    	  options.use_inner_iterations = false;
    	    	  options.evaluation_callback = null;
    	    	  if (!options.IsValid(message)) {
    	    		  System.err.println("In SolverCantMixEvaluationCallbackWithInnerIterations() options.IsValid(message) == false");
    	    		  passed = false;
    	    	  }
    	    	  
    	    	  if (passed) {
    	    		  System.out.println("SolverCantMixEvaluationCallbackWithInnerIterations() passed all tests");
    	    	  }
    	    }

    	    class DummyCostFunction4 extends SizedCostFunction {
    	    	 private int kNumResiduals;
    	    	 public DummyCostFunction4(int kNumResiduals, int N1) {
    	    		 super(kNumResiduals, N1, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    	    		 this.kNumResiduals = kNumResiduals;
    	    	 }
    	    	 
    	    	 public boolean Evaluate(Vector<double[]> parameters,
    	    	                double[] residuals,
    	    	                double[][] jacobians) {
    	    	    for (int i = 0; i < kNumResiduals; ++i) {
    	    	      residuals[i] = kNumResiduals * kNumResiduals + i;
    	    	    }

    	    	    return true;
    	    	  }
    	    	};

    	    	public void SolverFixedCostForConstantProblem() {
    	    		  // SolverFixedCostForConstantProblem() passed all tests
    	    		  boolean passed = true;
    	    		  double x[] = new double[] {1.0};
    	    		  ProblemImpl problem = new ProblemImpl();
    	    		  problem.AddResidualBlock(new DummyCostFunction4(2, 1), null, x);
    	    		  problem.SetParameterBlockConstant(x);
    	    		  final double expected_cost = 41.0 / 2.0;  // 1/2 * ((4 + 0)^2 + (4 + 1)^2)
    	    		  SolverOptions options = new SolverOptions();
    	    		  SolverSummary summary = new SolverSummary();
    	    		  Solve(options, problem, summary);
    	    		  if (!summary.IsSolutionUsable()) {
    	    		      System.err.println("In SolverFixedCostForConstantProblem() summary.IsSolutionUsable() == false");
    	    		      passed = false;
    	    		  }
    	    		  if (summary.fixed_cost != expected_cost) {
    	    			  System.err.println("In SolverFixedCostForConstantProblem() summary.fixed_cost != expected_cost");
    	    		      passed = false;
    	    		  }
    	    		  if (summary.initial_cost != expected_cost) {
    	    			  System.err.println("In SolverFixedCostForConstantProblem() summary.initial_cost != expected_cost");
    	    		      passed = false;
    	    		  }
    	    		  if (summary.final_cost != expected_cost) {
    	    			  System.err.println("In SolverFixedCostForConstantProblem() summary.final_cost != expected_cost");
    	    		      passed = false;
    	    		  }
    	    		  if (summary.iterations.size() != 0) {
    	    			  System.err.println("In SolverFixedCostForConstantProblem() summary.iterations.size() != 0");
    	    		      passed = false;
    	    		  }
    	    		  if (passed) {
    	    			  System.out.println("SolverFixedCostForConstantProblem() passed all tests");
    	    		  }
    	    		}

    	    	// For IEEE-754 doubles, machine precision is about 2e-16.
    	    	final double kEpsilon = 1e-13;
    	    	final double kEpsilonLoose = 1e-9;

    	    	// Return the constant polynomial p(x) = 1.23.
    	    	public Vector<Double> ConstantPolynomial(double value) {
    	    	  Vector<Double> poly = new Vector<Double>(1);
    	    	  poly.add(value);
    	    	  return poly;
    	    	}
    	    	
    	    	// Return the polynomial p(x) = poly(x) * (x - root).
    	    	public Vector<Double> AddRealRoot(Vector<Double> poly, double root) {
    	          int i;
    	    	  Vector<Double> poly2 = new Vector<Double>(poly.size() + 1);
    	    	  for (i = 0; i < poly.size() + 1; i++) {
    	    		  poly2.add(0.0);
    	    	  }
    	    	  for (i = 0; i < poly.size(); i++) {
    	    		  poly2.set(i, poly2.get(i) + poly.get(i));
    	    	  }
    	    	  for (i = 0; i < poly.size(); i++) {
    	    		  poly2.set(i+1, poly2.get(i+1) - root*poly.get(i));
    	    	  }
    	    	  return poly2;
    	    	}
    	    	
    	    	// Return the polynomial
    	    	// p(x) = poly(x) * (x - real - imag*i) * (x - real + imag*i).
    	    	public Vector<Double> AddComplexRootPair(Vector<Double> poly, double real, double imag) {
    	    	  int i;
    	    	  Vector<Double> poly2 = new Vector<Double>(poly.size() + 2);
    	    	  for (i = 0; i < poly.size() + 2; i++) {
    	    		  poly2.add(0.0);
    	    	  }
    	    	  // Multiply poly by x^2 - 2real + abs(real,imag)^2
    	    	  for (i = 0; i < poly.size(); i++) {
    	    		  poly2.set(i, poly2.get(i) + poly.get(i));
    	    	  }
    	    	  for (i = 0; i < poly.size(); i++) {
    	    		  poly2.set(i+1, poly2.get(i+1) - 2 * real * poly.get(i));
    	    	  }
    	    	  for (i = 0; i < poly.size(); i++) {
    	    		  poly2.set(i+2, poly2.get(i+2) + (real* real + imag*imag)*poly.get(i));
    	    	  }
    	    	  return poly2;
    	    	}
    	    	
    	    	// Sort the entries in a vector.
    	    	// Needed because the roots are not returned in sorted order.
    	    	public Vector<Double> SortVector(Vector<Double> in) {
    	    	  Vector<Double> out = new Vector<Double>(in.size());
    	    	  out.addAll(in);
    	    	  Collections.sort(out);
    	    	  return out;
    	    	}

    	    	// Run a test with the polynomial defined by the N real roots in roots_real.
    	    	// If use_real is false, NULL is passed as the real argument to
    	    	// FindPolynomialRoots. If use_imaginary is false, NULL is passed as the
    	    	// imaginary argument to FindPolynomialRoots.
    	    	void RunPolynomialTestRealRoots(double real_roots[],
    	    	                                boolean use_real,
    	    	                                boolean use_imaginary,
    	    	                                double epsilon, String testName) {
    	    	  boolean passed = true;
    	    	  int i;
    	    	  int N = real_roots.length;
    	    	  Vector<Double> real;
    	    	  if (use_real) {
    	    	      real = new Vector<Double>();
    	    	  }
    	    	  else {
    	    		  real = null;
    	    	  }
    	    	  Vector<Double> imaginary;
    	    	  if (use_imaginary) {
    	    	      imaginary = new Vector<Double>();
    	    	  }
    	    	  else {
    	    		  imaginary = null;
    	    	  }
    	    	  Vector<Double> poly = ConstantPolynomial(1.23);
    	    	  for (i = 0; i < N; ++i) {
    	    	    poly = AddRealRoot(poly, real_roots[i]);
    	    	  }
    	    	  boolean success = FindPolynomialRoots(poly, real, imaginary);

    	    	  if(!success) {
    	    		 System.err.println("In " + testName + " success = false");
    	    		 passed = false;
    	    	  }
    	    	  if (use_real) {
    	    	    if (real.size() != N) {
    	    	    	System.err.println("In " + testName + " real.size() != N");
    	    	    	passed = false;
    	    	    }
    	    	    real = SortVector(real);
    	    	    for (i = 0; i < real.size(); i++) {
    	    	    	if (Math.abs(real.get(i) - real_roots[i]) > epsilon) {
    	    	    		System.err.println("In " + testName + " Math.abs(real.get("+i+") - real_roots["+i+"]) > epsilon");
    	    	    		System.err.println("real.get("+i+") = " + real.get(i));
    	    	    		System.err.println("real_roots["+i+"] = " + real_roots[i]);
    	    	    		passed = false;
    	    	    	}
    	    	    }
    	    	  }
    	    	  if (use_imaginary) {
    	    		  if (imaginary.size() != N) {
      	    	    	System.err.println("In " + testName + " imaginary.size() != N");
      	    	    	passed = false;
      	    	      }
    	    		  for (i = 0; i < imaginary.size(); i++) {
      	    	    	if (Math.abs(imaginary.get(i)) > epsilon) {
      	    	    		System.err.println("In " + testName + " Math.abs(imaginary.get("+i+")) > epsilon");
      	    	    		passed = false;
      	    	    	}
      	    	    }
    	    	  }
    	    	  if (passed) {
    	    		  System.out.println(testName + " passed all tests");
    	    	  }
    	    	}
    	    	
    	    	public void PolynomialInvalidPolynomialOfZeroLengthIsRejected() {
    	    		  // PolynomialInvalidPolynomialOfZeroLengthIsRejected() passed all tests
    	    		  // Vector poly(0) is an ambiguous constructor call, so
    	    		  // use the constructor with explicit column count.
    	    		  Vector<Double> poly = new Vector<Double>(1);
    	    		  poly.add(0.0);
    	    		  Vector<Double> real = new Vector<Double>();
    	    		  Vector<Double> imag = new Vector<Double>();
    	    		  boolean success = FindPolynomialRoots(poly, real, imag);

    	    		  if (success) {
    	    			  System.err.println("In PolynomialInvalidPolynomialOfZeroLengthIsRejected() success = true");
    	    		  }
    	    		  else {
    	    			  System.out.println("PolynomialInvalidPolynomialOfZeroLengthIsRejected() passed all tests");
    	    		  }
    	    		}

    	    	public void PolynomialConstantPolynomialReturnsNoRoots() {
    	    		  // PolynomialConstantPolynomialReturnsNoRoots() passed all tests
    	    		  boolean passed = true;
    	    		  Vector<Double> poly = ConstantPolynomial(1.23);
    	    		  Vector<Double> real = new Vector<Double>();
    	    		  Vector<Double> imag = new Vector<Double>();
    	    		  boolean success = FindPolynomialRoots(poly, real, imag);

    	    		  if (!success) {
    	    			  System.err.println("In PolynomialConstantPolynomialReturnsNoRoots() success = false");
    	    			  passed = false;
    	    		  }
    	    		  if (real.size() != 0) {
    	    			  System.err.println("In PolynomialConstantPolynomialReturnsNoRoots() real.size() != 0");
    	    			  passed = false;
    	    		  }
    	    		  if (imag.size() != 0) {
    	    			  System.err.println("In PolynomialConstantPolynomialReturnsNoRoots() imag.size() != 0");
    	    			  passed = false;
    	    		  }
    	    		  if (passed) {
    	    			  System.out.println("PolynomialConstantPolynomialReturnsNoRoots() passed all tests");
    	    		  }
    	    		}

    	    	public void PolynomialLinearPolynomialWithPositiveRootWorks() {
    	    		  // PolynomialLinearPolynomialWithPositiveRootWorks() passed all tests
    	    		  final double roots[] = new double[]{ 42.42 };
    	    		  RunPolynomialTestRealRoots(roots, true, true, kEpsilon, "PolynomialLinearPolynomialWithPositiveRootWorks()");
    	    	}

    	    	public void PolynomialLinearPolynomialWithNegativeRootWorks() {
    	    		  // PolynomialLinearPolynomialWithNegativeRootWorks() passed all tests
    	    		  final double roots[] = new double[]{ -42.42 };
    	    		  RunPolynomialTestRealRoots(roots, true, true, kEpsilon, "PolynomialLinearPolynomialWithNegativeRootWorks()");
    	    	}
    	    	
    	    	public void PolynomialQuadraticPolynomialWithPositiveRootsWorks() {
    	    		  // PolynomialQuadraticPolynomialWithPositiveRootsWorks() passed all tests
    	    		  final double roots[] = new double[]{ 1.0, 42.42 };
    	    		  RunPolynomialTestRealRoots(roots, true, true, kEpsilon, "PolynomialQuadraticPolynomialWithPositiveRootsWorks()");
    	    	}
    	    	
    	    	public void PolynomialQuadraticPolynomialWithOneNegativeRootWorks() {
    	    		  // PolynomialQuadraticPolynomialWithOneNegativeRootWorks() passed all tests
    	    		  final double roots[] = new double[]{ -42.42, 1.0 };
    	    		  RunPolynomialTestRealRoots(roots, true, true, kEpsilon, "PolynomialQuadraticPolynomialWithOneNegativeRootWorks()");
    	    	}

    	    	public void PolynomialQuadraticPolynomialWithTwoNegativeRootsWorks() {
    	    		  // PolynomialQuadraticPolynomialWithTwoNegativeRootsWorks() passed all tests
    	    		  final double roots[] = new double[]{ -42.42, -1.0 };
    	    		  RunPolynomialTestRealRoots(roots, true, true, kEpsilon, "PolynomialQuadraticPolynomialWithTwoNegativeRootsWorks()");
    	    	}

    	    	public void PolynomialQuadraticPolynomialWithCloseRootsWorks() {
    	    		  // PolynomialQuadraticPolynomialWithCloseRootsWorks() passed all tests
    	    		  final double roots[] = new double[]{ 42.42, 42.43 };
    	    		  RunPolynomialTestRealRoots(roots, true, false, kEpsilonLoose, "PolynomialQuadraticPolynomialWithCloseRootsWorks()");
    	    	}
    	    	
    	    	public void PolynomialQuadraticPolynomialWithComplexRootsWorks() {
    	    		  // PolynomialQuadraticPolynomialWithComplexRootsWorks() passed all tests
    	    		  boolean passed = true;
    	    		  Vector<Double> real = new Vector<Double>();
    	    		  Vector<Double> imag = new Vector<Double>();

    	    		  Vector<Double> poly = ConstantPolynomial(1.23);
    	    		  poly = AddComplexRootPair(poly, 42.42, 4.2);
    	    		  boolean success = FindPolynomialRoots(poly, real, imag);

    	    		  if (!success) {
    	    			  System.err.println("In PolynomialQuadraticPolynomialWithComplexRootsWorks() success = false");
    	    			  passed = false;
    	    		  }
    	    		  if (real.size() != 2) {
    	    			  System.err.println("In PolynomialQuadraticPolynomialWithComplexRootsWorks() real.size() != 2");
    	    			  passed = false;
    	    		  }
    	    		  if (imag.size() != 2) {
    	    			  System.err.println("In PolynomialQuadraticPolynomialWithComplexRootsWorks() imag.size() != 2");
    	    			  passed = false;
    	    		  }
    	    		  if (Math.abs(real.get(0) - 42.42) > kEpsilon) {
    	    			  System.err.println("In PolynomialQuadraticPolynomialWithComplexRootsWorks() Math.abs(real.get(0) - 42.42) > kEpsilon");
    	    			  passed = false;
    	    		  }
    	    		  if (Math.abs(real.get(1) - 42.42) > kEpsilon) {
    	    			  System.err.println("In PolynomialQuadraticPolynomialWithComplexRootsWorks() Math.abs(real.get(1) - 42.42) > kEpsilon");
    	    			  passed = false;
    	    		  }
    	    		  if (Math.abs(Math.abs(imag.get(0)) - 4.2) > kEpsilon) {
    	    			  System.err.println("In PolynomialQuadraticPolynomialWithComplexRootsWorks() Math.abs(Math.abs(imag.get(0)) - 4.2) > kEpsilon");
    	    			  passed = false;
    	    		  }
    	    		  if (Math.abs(Math.abs(imag.get(1)) - 4.2) > kEpsilon) {
    	    			  System.err.println("In PolynomialQuadraticPolynomialWithComplexRootsWorks() Math.abs(Math.abs(imag.get(1)) - 4.2) > kEpsilon");
    	    			  passed = false;
    	    		  }
    	    		  if (Math.abs(imag.get(0) + imag.get(1)) > kEpsilon) {
    	    			  System.err.println("In PolynomialQuadraticPolynomialWithComplexRootsWorks() Math.abs(imag.get(0) + imag.get(1)) > kEpsilon");
    	    			  passed = false;
    	    		  }
    	    		  if (passed) {
    	    			  System.out.println("PolynomialQuadraticPolynomialWithComplexRootsWorks() passed all tests");
    	    		  }
    	    	}

    	    	public void PolynomialQuarticPolynomialWorks() {
    	    		  // PolynomialQuarticPolynomialWorks() passed all tests
    	    		  final double roots[] = new double[]{ 1.23e-4, 1.23e-1, 1.23e+2, 1.23e+5 };
    	    		  RunPolynomialTestRealRoots(roots, true, true, kEpsilon, "PolynomialQuarticPolynomialWorks()");
    	    	}
    	    	
    	    	public void PolynomialQuarticPolynomialWithTwoClustersOfCloseRootsWorks() {
    	    		  // PolynomialQuarticPolynomialWithTwoClustersOfCloseRootsWorks passed all tests
    	    		  final double roots[] = new double[]{ 1.23e-1, 2.46e-1, 1.23e+5, 2.46e+5 };
    	    		  RunPolynomialTestRealRoots(roots, true, true, kEpsilonLoose, "PolynomialQuarticPolynomialWithTwoClustersOfCloseRootsWorks");
    	        }
    	    	
    	    	public void PolynomialQuarticPolynomialWithTwoZeroRootsWorks() {
    	    		  // PolynomialQuarticPolynomialWithTwoZeroRootsWorks() passed all tests
    	    		  final double roots[] = new double[]{ -42.42, 0.0, 0.0, 42.42 };
    	    		  RunPolynomialTestRealRoots(roots, true, true, 2 * kEpsilonLoose, "PolynomialQuarticPolynomialWithTwoZeroRootsWorks()");
    	    	}
    	    	
    	    	public void PolynomialQuarticMonomialWorks() {
    	    		  // PolynomialQuarticMonomialWorks() passed all tests
    	    		  final double roots[] = new double[]{ 0.0, 0.0, 0.0, 0.0 };
    	    		  RunPolynomialTestRealRoots(roots, true, true, kEpsilon, "PolynomialQuarticMonomialWorks()");
    	    	}
    	    	
    	    	public void PolynomialNullPointerAsImaginaryPartWorks() {
    	    		  // PolynomialNullPointerAsImaginaryPartWorks() passed all tests
    	    		  final double roots[] = new double[]{ 1.23e-4, 1.23e-1, 1.23e+2, 1.23e+5 };
    	    		  RunPolynomialTestRealRoots(roots, true, false, kEpsilon, "PolynomialNullPointerAsImaginaryPartWorks()");
    	    	}

    	    	public void PolynomialNullPointerAsRealPartWorks() {
    	    		  // PolynomialNullPointerAsRealPartWorks() passed all tests
    	    		  final double roots[] = new double[]{ 1.23e-4, 1.23e-1, 1.23e+2, 1.23e+5 };
    	    		  RunPolynomialTestRealRoots(roots, false, true, kEpsilon, "PolynomialNullPointerAsRealPartWorks()");
    	    	}
    	    	
    	    	public void PolynomialBothOutputArgumentsNullWorks() {
    	    		  // PolynomialBothOutputArgumentsNullWorks() passed all tests
    	    		  final double roots[] = new double[]{ 1.23e-4, 1.23e-1, 1.23e+2, 1.23e+5 };
    	    		  RunPolynomialTestRealRoots(roots, false, false, kEpsilon, "PolynomialBothOutputArgumentsNullWorks()");
    	    	}

    	    	public void PolynomialDifferentiateConstantPolynomial() {
    	    		  // PolynomialDifferentiateConstantPolynomial() passed all tests
    	    		  boolean passed = true;
    	    		  // p(x) = 1;
    	    		  Vector<Double> polynomial = new Vector<Double>(1);
    	    		  polynomial.add(1.0);
    	    		  final Vector<Double> derivative = DifferentiatePolynomial(polynomial);
    	    		  if (derivative.size() != 1) {
    	    			  System.err.println("In PolynomialDifferentiateConstantPolynomial() derivative.size() != 1)");
    	    			  passed = false;
    	    		  }
    	    		  if (derivative.get(0) != 0.0) {
    	    			  System.err.println("In PolynomialDifferentiateConstantPolynomial() derivative.get(0) != 0.0)");
    	    			  passed = false;
    	    		  }
    	    		  if (passed) {
    	    			  System.out.println("PolynomialDifferentiateConstantPolynomial() passed all tests");
    	    		  }
    	    	}
    	    	
    	    	public void PolynomialDifferentiateQuadraticPolynomial() {
    	    		  // PolynomialDifferentiateQuadraticPolynomial() passed all tests
    	    		  boolean passed = true;
    	    		  // p(x) = x^2 + 2x + 3;
    	    		  Vector<Double> polynomial = new Vector<Double>(3);
    	    		  polynomial.add(1.0);
    	    		  polynomial.add(2.0);
    	    		  polynomial.add(3.0);

    	    		  final Vector<Double> derivative = DifferentiatePolynomial(polynomial);
    	    		  if (derivative.size() != 2) {
    	    			  System.err.println("In PolynomialDifferentiateQuadraticPolynomial() derivative.size() != 2");
    	    			  passed = false;
    	    		  }
    	    		  if (derivative.get(0) != 2.0) {
    	    			  System.err.println("In PolynomialDifferentiateQuadraticPolynomial() derivative.get(0) != 2.0");
    	    			  passed = false;
    	    		  }
    	    		  if (derivative.get(1) != 2.0) {
    	    			  System.err.println("In PolynomialDifferentiateQuadraticPolynomial() derivative.get(1) != 2.0");
    	    			  passed = false;
    	    		  }
    	    		  if (passed) {
    	    			  System.out.println("PolynomialDifferentiateQuadraticPolynomial() passed all tests");
    	    		  }
    	    	}

    	    	public void PolynomialMinimizeConstantPolynomial() {
    	    		  // PolynomialMinimizeConstantPolynomial() passed all tests
    	    		  boolean passed = true;
    	    		  // p(x) = 1;
    	    		  Vector<Double> polynomial = new Vector<Double>(1);
    	    		  polynomial.add(1.0);

    	    		  double optimal_x[] = new double[] {0.0};
    	    		  double optimal_value[] = new double[] {0.0};
    	    		  double min_x = 0.0;
    	    		  double max_x = 1.0;
    	    		  MinimizePolynomial(polynomial, min_x, max_x, optimal_x, optimal_value);

    	    		  if (optimal_value[0] != 1.0) {
    	    			  System.err.println("PolynomialMinimizeConstantPolynomial() optimal_value[0] != 1.0");
    	    			  passed = false;
    	    		  }
    	    		  if (optimal_x[0] > max_x) {
    	    			  System.err.println("PolynomialMinimizeConstantPolynomial() optimal_x[0] > max_x");
    	    			  passed = false;
    	    		  }
    	    		  if (optimal_x[0] < min_x) {
    	    			  System.err.println("PolynomialMinimizeConstantPolynomial() optimal_x[0] < min_x");
    	    			  passed = false;
    	    		  }
    	    		  if (passed) {
    	    			  System.out.println("PolynomialMinimizeConstantPolynomial() passed all tests");
    	    		  }
    	    	}

    	    	public void PolynomialMinimizeLinearPolynomial() {
    	    		  // PolynomialMinimizeLinearPolynomial() passed all tests
    	    		  boolean passed = true;
    	    		  // p(x) = x - 2
    	    		  Vector<Double> polynomial = new Vector<Double>(2);

    	    		  polynomial.add(1.0);
    	    		  polynomial.add(2.0);

    	    		  double optimal_x[] = new double[] {0.0};
    	    		  double optimal_value[] = new double[] {0.0};
    	    		  double min_x = 0.0;
    	    		  double max_x = 1.0;
    	    		  MinimizePolynomial(polynomial, min_x, max_x, optimal_x, optimal_value);

    	    		  if (optimal_x[0] != 0.0) {
    	    			  System.err.println("In PolynomialMinimizeLinearPolynomial() optimal_x[0] != 0.0");
    	    			  passed = false;
    	    		  }
    	    		  if (optimal_value[0] != 2.0) {
    	    			  System.err.println("In PolynomialMinimizeLinearPolynomial() optimal_value[0] != 2.0");
    	    			  passed = false;
    	    		  }
    	    		  if (passed) {
    	    			  System.out.println("PolynomialMinimizeLinearPolynomial() passed all tests");
    	    		  }
    	    	}

    	    	public void PolynomialMinimizeQuadraticPolynomial() {
    	    		  // PolynomialMinimizeQuadraticPolynomial() passed all tests
    	    		  boolean passed = true;
    	    		  // p(x) = x^2 - 3 x + 2
    	    		  // min_x = 3/2
    	    		  // min_value = -1/4;
    	    		  Vector<Double> polynomial = new Vector<Double>(3);
    	    		  polynomial.add(1.0);
    	    		  polynomial.add(-3.0);
    	    		  polynomial.add(2.0);

    	    		  double optimal_x[] = new double[] {0.0};
    	    		  double optimal_value[] = new double[] {0.0};
    	    		  double min_x = -2.0;
    	    		  double max_x = 2.0;
    	    		  MinimizePolynomial(polynomial, min_x, max_x, optimal_x, optimal_value);
    	    		  if (optimal_x[0] != 3.0/2.0) {
    	    			  System.err.println("In  PolynomialMinimizeQuadraticPolynomial() optimal_x[0] != 3.0/2.0");
    	    			  passed = false;
    	    		  }
    	    		  if (optimal_value[0] != -1.0/4.0) {
    	    			  System.err.println("In  PolynomialMinimizeQuadraticPolynomial() optimal_value[0] != -1.0/4.0");
    	    			  passed = false;
    	    		  }

    	    		  min_x = -2.0;
    	    		  max_x = 1.0;
    	    		  MinimizePolynomial(polynomial, min_x, max_x, optimal_x, optimal_value);
    	    		  if (optimal_x[0] != 1.0) {
    	    			  System.err.println("In  PolynomialMinimizeQuadraticPolynomial() optimal_x[0] != 1.0");
    	    			  passed = false;
    	    		  }
    	    		  if (optimal_value[0] != 0.0) {
    	    			  System.err.println("In  PolynomialMinimizeQuadraticPolynomial() optimal_value[0] != 0.0");
    	    			  passed = false;
    	    		  }

    	    		  min_x = 2.0;
    	    		  max_x = 3.0;
    	    		  MinimizePolynomial(polynomial, min_x, max_x, optimal_x, optimal_value);
    	    		  if (optimal_x[0] != 2.0) {
    	    			  System.err.println("In  PolynomialMinimizeQuadraticPolynomial() optimal_x[0] != 2.0");
    	    			  passed = false;
    	    		  }
    	    		  if (optimal_value[0] != 0.0) {
    	    			  System.err.println("In  PolynomialMinimizeQuadraticPolynomial() optimal_value[0] != 0.0");
    	    			  passed = false;
    	    		  }
    	    		  if (passed) {
    	    			  System.out.println("PolynomialMinimizeQuadraticPolynomial() passed all tests");
    	    		  }
    	    	}
 
    	    	public void PolymomialConstantInterpolatingPolynomial() {
    	    		  // PolymomialConstantInterpolatingPolynomial() passed all tests
    	    		  // p(x) = 1.0
    	    		  Vector<Double> true_polynomial = new Vector<Double>(1);
    	    		  true_polynomial.add(1.0);

    	    		  Vector<FunctionSample> samples = new Vector<FunctionSample>();
    	    		  FunctionSample sample = new FunctionSample();
    	    		  sample.x = 1.0;
    	    		  sample.value = new double[] {1.0};
    	    		  sample.value_is_valid = true;
    	    		  samples.add(sample);

    	    		  final Vector<Double> polynomial = FindInterpolatingPolynomial(samples);
    	    		  if (Math.abs(true_polynomial.get(0) - polynomial.get(0)) > 1.0E-15) {
    	    			  System.err.println("In PolymomialConstantInterpolatingPolynomial() Math.abs(true_polynomial.get(0) - polynomial.get(0)) > 1.0E-15");
    	    		  }
    	    		  else {
    	    			  System.out.println("PolymomialConstantInterpolatingPolynomial() passed all tests");
    	    		  }
    	    	}
    	    	
    	    	public void PolynomialLinearInterpolatingPolynomial() {
    	    		  // PolynomialLinearInterpolatingPolynomial() passed all tests
    	    		  // p(x) = 2x - 1
    	    		  Vector<Double> true_polynomial = new Vector<Double>(2);
    	    		  true_polynomial.add(2.0);
    	    		  true_polynomial.add(-1.0);

    	    		  Vector<FunctionSample> samples = new Vector<FunctionSample>();
    	    		  FunctionSample sample = new FunctionSample();
    	    		  sample.x = 1.0;
    	    		  sample.value = new double[] {1.0};
    	    		  sample.value_is_valid = true;
    	    		  sample.gradient = 2.0;
    	    		  sample.gradient_is_valid = true;
    	    		  samples.add(sample);

    	    		  final Vector<Double> polynomial = FindInterpolatingPolynomial(samples);
    	    		  double squaredNorm = 0.0;
    	    		  int i;
    	    		  double diff;
    	    		  for (i = 0; i < 2; i++) {
    	    			  diff = true_polynomial.get(i) - polynomial.get(i);
    	    			  squaredNorm += diff * diff;
    	    		  }
    	    		  double norm = Math.sqrt(squaredNorm);
    	    		  if (norm > 1.0E-15) {
    	    			  System.err.println("In PolynomialLinearInterpolatingPolynomial() true_polynomial - polynomial).norm() > 1e-15");
    	    		  }
    	    		  else {
    	    			  System.out.println("PolynomialLinearInterpolatingPolynomial() passed all tests");
    	    		  }
    	    	}

    	    	public void PolynomialQuadraticInterpolatingPolynomial() {
    	    		  // PolynomialQuadraticInterpolatingPolynomial() passed all tests
    	    		  // p(x) = 2x^2 + 3x + 2
    	    		  Vector<Double> true_polynomial = new Vector<Double>(3);
    	    		  true_polynomial.add(2.0);
    	    		  true_polynomial.add(3.0);
    	    		  true_polynomial.add(2.0);

    	    		  Vector<FunctionSample> samples = new Vector<FunctionSample>();
    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = 1.0;
    	    		    sample.value = new double[] {7.0};
    	    		    sample.value_is_valid = true;
    	    		    sample.gradient = 7.0;
    	    		    sample.gradient_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = -3.0;
    	    		    sample.value = new double[] {11.0};
    	    		    sample.value_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  Vector<Double> polynomial = FindInterpolatingPolynomial(samples);
    	    		  double squaredNorm = 0.0;
    	    		  int i;
    	    		  double diff;
    	    		  for (i = 0; i < 3; i++) {
    	    			  diff = true_polynomial.get(i) - polynomial.get(i);
    	    			  squaredNorm += diff * diff;
    	    		  }
    	    		  double norm = Math.sqrt(squaredNorm);
    	    		  if (norm > 1.0E-15) {
    	    			  System.err.println("In PolynomialQuadraticInterpolatingPolynomial() true_polynomial - polynomial).norm() > 1e-15");
    	    		  }
    	    		  else {
    	    			  System.out.println("PolynomialQuadraticInterpolatingPolynomial() passed all tests");
    	    		  }
    	    	}

    	    	public void PolynomialDeficientCubicInterpolatingPolynomial() {
    	    		  // PolynomialDeficientCubicInterpolatingPolynomial() passed all tests
    	    		  // p(x) = 2x^2 + 3x + 2
    	    		  Vector<Double> true_polynomial = new Vector<Double>(4);
    	    		  true_polynomial.add(0.0);
    	    		  true_polynomial.add(2.0);
    	    		  true_polynomial.add(3.0);
    	    		  true_polynomial.add(2.0);

    	    		  Vector<FunctionSample> samples = new Vector<FunctionSample>();
    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = 1.0;
    	    		    sample.value = new double[] {7.0};
    	    		    sample.value_is_valid = true;
    	    		    sample.gradient = 7.0;
    	    		    sample.gradient_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = -3.0;
    	    		    sample.value = new double[] {11.0};
    	    		    sample.value_is_valid = true;
    	    		    sample.gradient = -9;
    	    		    sample.gradient_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  final Vector<Double> polynomial = FindInterpolatingPolynomial(samples);
    	    		  double squaredNorm = 0.0;
    	    		  int i;
    	    		  double diff;
    	    		  for (i = 0; i < 4; i++) {
    	    			  diff = true_polynomial.get(i) - polynomial.get(i);
    	    			  squaredNorm += diff * diff;
    	    		  }
    	    		  double norm = Math.sqrt(squaredNorm);
    	    		  if (norm > 1.0E-14) {
    	    			  System.err.println("In PolynomialDeficientCubicInterpolatingPolynomial() true_polynomial - polynomial).norm() > 1e-14");
    	    		  }
    	    		  else {
    	    			  System.out.println("PolynomialDeficientCubicInterpolatingPolynomial() passed all tests");
    	    		  }
    	    	}

    	    	public void PolynomialCubicInterpolatingPolynomialFromValues() {
    	    		  // PolynomialCubicInterpolatingPolynomialFromValues() passed all tests
    	    		  // p(x) = x^3 + 2x^2 + 3x + 2
    	    		  Vector<Double> true_polynomial = new Vector<Double>(4);
    	    		  true_polynomial.add(1.0);
    	    		  true_polynomial.add(2.0);
    	    		  true_polynomial.add(3.0);
    	    		  true_polynomial.add(2.0);

    	    		  Vector<FunctionSample> samples = new Vector<FunctionSample>();
    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = 1.0;
    	    		    sample.value = new double[] {EvaluatePolynomial(true_polynomial, sample.x)};
    	    		    sample.value_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = -3.0;
    	    		    sample.value = new double[] {EvaluatePolynomial(true_polynomial, sample.x)};
    	    		    sample.value_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = 2.0;
    	    		    sample.value = new double[] {EvaluatePolynomial(true_polynomial, sample.x)};
    	    		    sample.value_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = 0.0;
    	    		    sample.value = new double[] {EvaluatePolynomial(true_polynomial, sample.x)};
    	    		    sample.value_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  final Vector<Double> polynomial = FindInterpolatingPolynomial(samples);
    	    		  double squaredNorm = 0.0;
    	    		  int i;
    	    		  double diff;
    	    		  for (i = 0; i < 4; i++) {
    	    			  diff = true_polynomial.get(i) - polynomial.get(i);
    	    			  squaredNorm += diff * diff;
    	    		  }
    	    		  double norm = Math.sqrt(squaredNorm);
    	    		  if (norm > 1.0E-14) {
    	    			  System.err.println("In PolynomialCubicInterpolatingPolynomialFromValues() true_polynomial - polynomial).norm() > 1e-14");
    	    		  }
    	    		  else {
    	    			  System.out.println("PolynomialCubicInterpolatingPolynomialFromValues() passed all tests");
    	    		  }
    	    	}
    	    	
    	    	public void PolynomialCubicInterpolatingPolynomialFromValuesAndOneGradient() {
    	    		  // PolynomialCubicInterpolatingPolynomialFromValuesAndOneGradient() passed all tests
    	    		  // p(x) = x^3 + 2x^2 + 3x + 2
    	    		  Vector<Double> true_polynomial = new Vector<Double>(4);
  	    		      true_polynomial.add(1.0);
  	    		      true_polynomial.add(2.0);
  	    		      true_polynomial.add(3.0);
  	    		      true_polynomial.add(2.0);
    	    		  Vector<Double> true_gradient_polynomial = DifferentiatePolynomial(true_polynomial);

    	    		  Vector<FunctionSample> samples = new Vector<FunctionSample>();
    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = 1.0;
    	    		    sample.value = new double[] {EvaluatePolynomial(true_polynomial, sample.x)};
    	    		    sample.value_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = -3.0;
    	    		    sample.value = new double[] {EvaluatePolynomial(true_polynomial, sample.x)};
    	    		    sample.value_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = 2.0;
    	    		    sample.value = new double[] {EvaluatePolynomial(true_polynomial, sample.x)};
    	    		    sample.value_is_valid = true;
    	    		    sample.gradient = EvaluatePolynomial(true_gradient_polynomial, sample.x);
    	    		    sample.gradient_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  final Vector<Double> polynomial = FindInterpolatingPolynomial(samples);
    	    		  double squaredNorm = 0.0;
    	    		  int i;
    	    		  double diff;
    	    		  for (i = 0; i < 4; i++) {
    	    			  diff = true_polynomial.get(i) - polynomial.get(i);
    	    			  squaredNorm += diff * diff;
    	    		  }
    	    		  double norm = Math.sqrt(squaredNorm);
    	    		  if (norm > 1.0E-14) {
    	    			  System.err.println("In PolynomialCubicInterpolatingPolynomialFromValuesAndOneGradient() true_polynomial - polynomial).norm() > 1e-14");
    	    		  }
    	    		  else {
    	    			  System.out.println("PolynomialCubicInterpolatingPolynomialFromValuesAndOneGradient() passed all tests");
    	    		  }
    	    	}

    	    	public void PolynomialCubicInterpolatingPolynomialFromValuesAndGradients() {
    	    		  // PolynomialCubicInterpolatingPolynomialFromValuesAndGradients() passed all tests
    	    		  // p(x) = x^3 + 2x^2 + 3x + 2
    	    		  Vector<Double> true_polynomial = new Vector<Double>(4);
 	    		      true_polynomial.add(1.0);
 	    		      true_polynomial.add(2.0);
 	    		      true_polynomial.add(3.0);
 	    		      true_polynomial.add(2.0);
    	    		  Vector<Double> true_gradient_polynomial = DifferentiatePolynomial(true_polynomial);

    	    		  Vector<FunctionSample> samples = new Vector<FunctionSample>();
    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = -3.0;
    	    		    sample.value = new double[] {EvaluatePolynomial(true_polynomial, sample.x)};
    	    		    sample.value_is_valid = true;
    	    		    sample.gradient = EvaluatePolynomial(true_gradient_polynomial, sample.x);
    	    		    sample.gradient_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  {
    	    		    FunctionSample sample = new FunctionSample();
    	    		    sample.x = 2.0;
    	    		    sample.value = new double[] {EvaluatePolynomial(true_polynomial, sample.x)};
    	    		    sample.value_is_valid = true;
    	    		    sample.gradient = EvaluatePolynomial(true_gradient_polynomial, sample.x);
    	    		    sample.gradient_is_valid = true;
    	    		    samples.add(sample);
    	    		  }

    	    		  final Vector<Double> polynomial = FindInterpolatingPolynomial(samples);
    	    		  double squaredNorm = 0.0;
    	    		  int i;
    	    		  double diff;
    	    		  for (i = 0; i < 4; i++) {
    	    			  diff = true_polynomial.get(i) - polynomial.get(i);
    	    			  squaredNorm += diff * diff;
    	    		  }
    	    		  double norm = Math.sqrt(squaredNorm);
    	    		  if (norm > 1.0E-14) {
    	    			  System.err.println("In PolynomialCubicInterpolatingPolynomialFromValuesAndGradients() true_polynomial - polynomial).norm() > 1e-14");
    	    		  }
    	    		  else {
    	    			  System.out.println("PolynomialCubicInterpolatingPolynomialFromValuesAndGradients() passed all tests");
    	    		  }
    	    	}
    	    	
    	    	class MockCostFunctionBase2 extends SizedCostFunction {
    	    		   int kNumResiduals;
    	    		   int N0;
    	    		   int N1;
    	    		   int N2;
    	    		   public MockCostFunctionBase2(int kNumResiduals, int N0, int N1, int N2) {
    	    			   super(kNumResiduals, N0, N1, N2, 0, 0, 0, 0, 0, 0, 0);
    	    			   this.kNumResiduals = kNumResiduals;
    	    			   this.N0 = N0;
    	    			   this.N1 = N1;
    	    			   this.N2 = N2;
    	    		   }
    	    		   public boolean Evaluate(Vector<double[]> parameters,
    	    		                         double[] residuals,
    	    		                         double[][] jacobians) {
    	    			   for (int i = 0; i < kNumResiduals; ++i) {
    	    				      residuals[i] = kNumResiduals +  N0 + N1 + N2;
    	    			   }

    	    		     return true;
    	    		   }
    	    		   
    	    		   public boolean Evaluate(Vector<double[]> parameters,
    	    	               double[] residuals,
    	    	               double[][] jacobians,
    	    	               int[] jacobians_offset) {
    	    			   for (int i = 0; i < kNumResiduals; ++i) {
 	    				      residuals[i] = kNumResiduals +  N0 + N1 + N2;
 	    			       }

    	    			return true;
    	    			}
    	    		 };
    	    		 
    	    		 class UnaryCostFunction2 extends MockCostFunctionBase2 {
    	    			  public UnaryCostFunction2() {
    	    				  super(2 ,1, 0, 0);
    	    			  }
    	    		 }
    	    		 
    	    		 class BinaryCostFunction2 extends MockCostFunctionBase2 {
    	    			 public BinaryCostFunction2() {
    	    				 super(2, 1, 1, 0);
    	    			 }
    	    		 }
    	    		 
    	    		 class TernaryCostFunction3 extends MockCostFunctionBase2 {
    	    			 public TernaryCostFunction3() {
    	    				 super(2, 1, 1, 1);
    	    			 }
    	    		 }
    	    	
    	    		 public void ProgramRemoveFixedBlocksNothingConstant() {
    	    			  // ProgramRemoveFixedBlocksNothingConstant() passed all tests
    	    			  boolean passed = true;
    	    			  ProblemImpl problem = new ProblemImpl();
    	    			  double x[] = new double[1];
    	    			  double y[] = new double[1];
    	    			  double z[] = new double[1];

    	    			  problem.AddParameterBlock(x, 1);
    	    			  problem.AddParameterBlock(y, 1);
    	    			  problem.AddParameterBlock(z, 1);
    	    			  problem.AddResidualBlock(new UnaryCostFunction2(), null, x);
    	    			  problem.AddResidualBlock(new BinaryCostFunction2(), null, x, y);
    	    			  problem.AddResidualBlock(new TernaryCostFunction3(), null, x, y, z);

    	    			  Vector<double[]> removed_parameter_blocks = new Vector<double[]>();
    	    			  double fixed_cost[] = new double[] {0.0};
    	    			  String message[] = new String[1];
    	    			  Program reduced_program = problem.program().CreateReducedProgram(removed_parameter_blocks,
    	    			                                          fixed_cost,
    	    			                                          message);
                          if (reduced_program == null) {
                        	  System.err.println("In ProgramRemoveFixedBlocksNothingConstant() reduced_program == null");
                        	  return;
                          }
    	    			  if (reduced_program.NumParameterBlocks() != 3) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksNothingConstant() reduced_program.NumParameterBlocks() != 3");
    	    				  passed = false;
    	    			  }
    	    			  if (reduced_program.NumResidualBlocks() != 3) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksNothingConstant() reduced_program.NumResidualBlocks() != 3");
    	    				  passed = false;
    	    			  }
    	    			  if (removed_parameter_blocks.size() != 0) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksNothingConstant() removed_parameter_blocks.size() != 0");
    	    				  passed = false;
    	    			  }
    	    			  if (fixed_cost[0] != 0.0) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksNothingConstant() fixed_cost[0] != 0.0");
    	    				  passed = false;
    	    			  }
    	    			  if (passed) {
    	    				  System.out.println("ProgramRemoveFixedBlocksNothingConstant() passed all tests");
    	    			  }
    	    		}
   	    	
    	    		 public void ProgramRemoveFixedBlocksAllParameterBlocksConstant() {
    	    			  // ProgramRemoveFixedBlocksAllParameterBlocksConstant() passed all tests
    	    			  boolean passed = true;
    	    			  ProblemImpl problem = new ProblemImpl();
    	    			  double x[] = new double[] {1.0};

    	    			  problem.AddParameterBlock(x, 1);
    	    			  problem.AddResidualBlock(new UnaryCostFunction2(), null, x);
    	    			  problem.SetParameterBlockConstant(x);

    	    			  Vector<double[]> removed_parameter_blocks = new Vector<double[]>();
    	    			  double fixed_cost[] = new double[] {0.0};
    	    			  String message[] = new String[1];
    	    			  Program reduced_program = problem.program().CreateReducedProgram(removed_parameter_blocks,
    	    			                                          fixed_cost,
    	    			                                          message);
                          if (reduced_program == null) {
                        	  System.err.println("In ProgramRemoveFixedBlocksAllParameterBlocksConstant() reduced_program == null");
                        	  return;
                          }
    	    			  
                          if (reduced_program.NumParameterBlocks() != 0) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksAllParameterBlocksConstant() reduced_program.NumParameterBlocks() != 0");
    	    				  passed = false;
    	    			  }
    	    			  if (reduced_program.NumResidualBlocks() != 0) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksAllParameterBlocksConstant() reduced_program.NumResidualBlocks() != 0");
    	    				  passed = false;
    	    			  }
    	    			  if (removed_parameter_blocks.size() != 1) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksAllParameterBlocksConstant() removed_parameter_blocks.size() != 1");
    	    				  passed = false;
    	    			  }
    	    			  if (removed_parameter_blocks.get(0) != x) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksAllParameterBlocksConstant() removed_parameter_blocks.get(0) != x");
    	    				  passed = false;
    	    			  }
    	    			  if (fixed_cost[0] != 9.0) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksAllParameterBlocksConstant() fixed_cost[0] != 9.0");
    	    				  passed = false;
    	    			  }
    	    			  if (passed) {
    	    				  System.out.println("ProgramRemoveFixedBlocksAllParameterBlocksConstant() passed all tests");
    	    			  }
    	    		}

    	    		 public void ProgramRemoveFixedBlocksNoResidualBlocks() {
    	    			  // ProgramRemoveFixedBlocksNoResidualBlocks() passed all tests
    	    			  boolean passed = true;
    	    			  ProblemImpl problem = new ProblemImpl();
    	    			  double x[] = new double[1];
    	    			  double y[] = new double[1];
    	    			  double z[] = new double[1];

    	    			  problem.AddParameterBlock(x, 1);
    	    			  problem.AddParameterBlock(y, 1);
    	    			  problem.AddParameterBlock(z, 1);

    	    			  Vector<double[]> removed_parameter_blocks = new Vector<double[]>();
    	    			  double fixed_cost[] = new double[] {0.0};
    	    			  String message[] = new String[1];
    	    			  Program reduced_program = problem.program().CreateReducedProgram(removed_parameter_blocks,
    	    			                                          fixed_cost,
    	    			                                          message);
                          if (reduced_program == null) {
                        	  System.err.println("In ProgramRemoveFixedBlocksNoResidualBlocks() reduced_program == null");
                        	  return;
                          }
    	    			  
                          if (reduced_program.NumParameterBlocks() != 0) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksNoResidualBlocks() reduced_program.NumParameterBlocks() != 0");
    	    				  passed = false;
    	    			  }
    	    			  if (reduced_program.NumResidualBlocks() != 0) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksNoResidualBlocks() reduced_program.NumResidualBlocks() != 0");
    	    				  passed = false;
    	    			  }
    	    			  if (removed_parameter_blocks.size() != 3) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksNoResidualBlocks() removed_parameter_blocks.size() != 3");
    	    				  passed = false;
    	    			  }
    	    			  if (fixed_cost[0] != 0.0) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksNoResidualBlocks() fixed_cost[0] != 0.0");
    	    				  passed = false;
    	    			  }
    	    			  if (passed) {
    	    				  System.out.println("ProgramRemoveFixedBlocksNoResidualBlocks() passed all tests");
    	    			  }
    	    		}
    	    		 
    	    		 public void ProgramRemoveFixedBlocksOneParameterBlockConstant() {
    	    			  // ProgramRemoveFixedBlocksOneParameterBlockConstant() passed all tests
    	    			  boolean passed = true;
    	    			  ProblemImpl problem = new ProblemImpl();
    	    			  double x[] = new double[1];
    	    			  double y[] = new double[1];
    	    			  double z[] = new double[1];

    	    			  problem.AddParameterBlock(x, 1);
    	    			  problem.AddParameterBlock(y, 1);
    	    			  problem.AddParameterBlock(z, 1);

    	    			  problem.AddResidualBlock(new UnaryCostFunction2(), null, x);
    	    			  problem.AddResidualBlock(new BinaryCostFunction2(), null, x, y);
    	    			  problem.SetParameterBlockConstant(x);

    	    			  Vector<double[]> removed_parameter_blocks = new Vector<double[]>();
    	    			  double fixed_cost[] = new double[] {0.0};
    	    			  String message[] = new String[1];
    	    			  Program reduced_program = problem.program().CreateReducedProgram(removed_parameter_blocks,
    	    			                                          fixed_cost,
    	    			                                          message);
                          if (reduced_program == null) {
                        	  System.err.println("In ProgramRemoveFixedBlocksOneParameterBlockConstant() reduced_program == null");
                        	  return;
                          }
    	    			  
                          if (reduced_program.NumParameterBlocks() != 1) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksOneParameterBlockConstant() reduced_program.NumParameterBlocks() != 1");
    	    				  passed = false;
    	    			  }
    	    			  if (reduced_program.NumResidualBlocks() != 1) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksOneParameterBlockConstant() reduced_program.NumResidualBlocks() != 1");
    	    				  passed = false;
    	    			  }
                          if (passed) {
                        	  System.out.println("ProgramRemoveFixedBlocksOneParameterBlockConstant() passed all tests");
                          }
    	    		}

    	    		 public void ProgramRemoveFixedBlocksNumEliminateBlocks() {
    	    			  // ProgramRemoveFixedBlocksNumEliminateBlocks() passed all tests
    	    			  boolean passed = true;
    	    			  ProblemImpl problem = new ProblemImpl();
    	    			  double x[] = new double[1];
    	    			  double y[] = new double[1];
    	    			  double z[] = new double[1];

    	    			  problem.AddParameterBlock(x, 1);
    	    			  problem.AddParameterBlock(y, 1);
    	    			  problem.AddParameterBlock(z, 1);
    	    			  problem.AddResidualBlock(new UnaryCostFunction2(), null, x);
    	    			  problem.AddResidualBlock(new TernaryCostFunction3(), null, x, y, z);
    	    			  problem.AddResidualBlock(new BinaryCostFunction2(), null, x, y);
    	    			  problem.SetParameterBlockConstant(x);

    	    			  Vector<double[]> removed_parameter_blocks = new Vector<double[]>();
    	    			  double fixed_cost[] = new double[] {0.0};
    	    			  String message[] = new String[1];
    	    			  Program reduced_program = problem.program().CreateReducedProgram(removed_parameter_blocks,
    	    			                                          fixed_cost,
    	    			                                          message);
                          if (reduced_program == null) {
                        	  System.err.println("In ProgramRemoveFixedBlocksNumEliminateBlocks() reduced_program == null");
                        	  return;
                          }
    	    			  
                          if (reduced_program.NumParameterBlocks() != 2) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksNumEliminateBlocks() reduced_program.NumParameterBlocks() != 2");
    	    				  passed = false;
    	    			  }
    	    			  if (reduced_program.NumResidualBlocks() != 2) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksNumEliminateBlocks() reduced_program.NumResidualBlocks() != 2");
    	    				  passed = false;
    	    			  }
                          if (passed) {
                        	  System.out.println("ProgramRemoveFixedBlocksNumEliminateBlocks() passed all tests");
                          }
    	    		}
    	    		 
    	    		 public void ProgramRemoveFixedBlocksFixedCost() {
    	    			  // ProgramRemoveFixedBlocksFixedCost() passed all tests
    	    			  boolean passed = true;
    	    			  ProblemImpl problem = new ProblemImpl();
    	    			  double x[] = new double[] {1.23};
    	    			  double y[] = new double[] {4.56};
    	    			  double z[] = new double[] {7.89};

    	    			  problem.AddParameterBlock(x, 1);
    	    			  problem.AddParameterBlock(y, 1);
    	    			  problem.AddParameterBlock(z, 1);
    	    			  problem.AddResidualBlock(new UnaryIdentityCostFunction(), null, x);
    	    			  problem.AddResidualBlock(new TernaryCostFunction3(), null, x, y, z);
    	    			  problem.AddResidualBlock(new BinaryCostFunction2(), null, x, y);
    	    			  problem.SetParameterBlockConstant(x);

    	    			  ResidualBlock expected_removed_block =
    	    			      problem.program().residual_blocks().get(0);
    	    			  double scratch[] = 
    	    			      new double[expected_removed_block.NumScratchDoublesForEvaluate()];
    	    			  double expected_fixed_cost[] = new double[1];
    	    			  expected_removed_block.Evaluate(true,
    	    			                                   expected_fixed_cost,
    	    			                                   null,
    	    			                                   null,
    	    			                                   scratch);


    	    			  Vector<double[]> removed_parameter_blocks = new Vector<double[]>();
    	    			  double fixed_cost[] = new double[] {0.0};
    	    			  String message[] = new String[1];
    	    			  Program reduced_program = problem.program().CreateReducedProgram(removed_parameter_blocks,
    	    			                                          fixed_cost,
    	    			                                          message);
                          if (reduced_program == null) {
                        	  System.err.println("In ProgramRemoveFixedBlocksFixedCost() reduced_program == null");
                        	  return;
                          }
    	    			  
                          if (reduced_program.NumParameterBlocks() != 2) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksFixedCost() reduced_program.NumParameterBlocks() != 2");
    	    				  passed = false;
    	    			  }
    	    			  if (reduced_program.NumResidualBlocks() != 2) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksFixedCost() reduced_program.NumResidualBlocks() != 2");
    	    				  passed = false;
    	    			  }
    	    			  if (fixed_cost[0] != expected_fixed_cost[0]) {
    	    				  System.err.println("In ProgramRemoveFixedBlocksFixedCost() fixed_cost[0] != expected_fixed_cost[0]");
    	    				  passed = false; 
    	    			  }
                          if (passed) {
                        	  System.out.println("ProgramRemoveFixedBlocksFixedCost() passed all tests");
                          }
    	    		}

    	    		 public void ProgramCreateJacobianBlockSparsityTranspose() {
    	    			  // ProgramCreateJacobianBlockSparsityTranspose() passed all tests
    	    			  int i,j;
    	    			  ProblemImpl problem = new ProblemImpl();
    	    			  double x[] = new double[2];
    	    			  double y[] = new double[3];
    	    			  double z[] = new double[1];

    	    			  problem.AddParameterBlock(x, 2);
    	    			  problem.AddParameterBlock(y, 3);
    	    			  problem.AddParameterBlock(z, 1);

    	    			  problem.AddResidualBlock(new MockCostFunctionBase2(2, 2, 0, 0), null, x);
    	    			  problem.AddResidualBlock(new MockCostFunctionBase2(3, 1, 2, 0), null, z, x);
    	    			  problem.AddResidualBlock(new MockCostFunctionBase2(4, 1, 3, 0), null, z, y);
    	    			  problem.AddResidualBlock(new MockCostFunctionBase2(5, 1, 3, 0), null, z, y);
    	    			  problem.AddResidualBlock(new MockCostFunctionBase2(1, 2, 1, 0), null, x, z);
    	    			  problem.AddResidualBlock(new MockCostFunctionBase2(2, 1, 3, 0), null, z, y);
    	    			  problem.AddResidualBlock(new MockCostFunctionBase2(2, 2, 1, 0), null, x, z);
    	    			  problem.AddResidualBlock(new MockCostFunctionBase2(1, 3, 0, 0), null, y);

    	    			  TripletSparseMatrix expected_block_sparse_jacobian = new TripletSparseMatrix(3, 8, 14);
    	    			  {
    	    			    int rows[] = expected_block_sparse_jacobian.mutable_rows();
    	    			    int cols[] = expected_block_sparse_jacobian.mutable_cols();
    	    			    double[] values = expected_block_sparse_jacobian.mutable_values();
    	    			    rows[0] = 0;
    	    			    cols[0] = 0;

    	    			    rows[1] = 2;
    	    			    cols[1] = 1;
    	    			    rows[2] = 0;
    	    			    cols[2] = 1;

    	    			    rows[3] = 2;
    	    			    cols[3] = 2;
    	    			    rows[4] = 1;
    	    			    cols[4] = 2;

    	    			    rows[5] = 2;
    	    			    cols[5] = 3;
    	    			    rows[6] = 1;
    	    			    cols[6] = 3;

    	    			    rows[7] = 0;
    	    			    cols[7] = 4;
    	    			    rows[8] = 2;
    	    			    cols[8] = 4;

    	    			    rows[9] = 2;
    	    			    cols[9] = 5;
    	    			    rows[10] = 1;
    	    			    cols[10] = 5;

    	    			    rows[11] = 0;
    	    			    cols[11] = 6;
    	    			    rows[12] = 2;
    	    			    cols[12] = 6;

    	    			    rows[13] = 1;
    	    			    cols[13] = 7;
    	    			    for (i = 0; i < 14; i++) {
    	    			    	values[i] = 1.0;
    	    			    }
    	    			    expected_block_sparse_jacobian.set_num_nonzeros(14);
    	    			  }

    	    			  Program program = problem.mutable_program();
    	    			  program.SetParameterOffsetsAndIndex();

    	    			  TripletSparseMatrix actual_block_sparse_jacobian =
    	    			      program.CreateJacobianBlockSparsityTranspose();

    	    			  Matrix expected_dense_jacobian = expected_block_sparse_jacobian.ToDenseMatrix();

    	    			  Matrix actual_dense_jacobian = actual_block_sparse_jacobian.ToDenseMatrix();
    	    			  double normSquared = 0.0;
    	    			  double diff;
    	    			  for (i = 0; i < expected_dense_jacobian.getRowDimension(); i++) {
    	    				  for (j = 0; j < expected_dense_jacobian.getColumnDimension(); j++) {
    	    					  diff = expected_dense_jacobian.get(i,j) - actual_dense_jacobian.get(i,j);
    	    					  normSquared += diff * diff;
    	    				  }
    	    			  }
    	    			  double norm = Math.sqrt(normSquared);
    	    			  if (norm != 0.0) {
    	    				  System.err.println("In ProgramCreateJacobianBlockSparsityTranspose() (expected_dense_jacobian - actual_dense_jacobian).norm() != 0.0");
    	    			  }
    	    			  else {
    	    				  System.out.println("ProgramCreateJacobianBlockSparsityTranspose() passed all tests");
    	    			  }
    	    		}
    	    		 
    	    		class NumParameterBlocksCostFunction extends CostFunction {
    	    		  public NumParameterBlocksCostFunction(int kNumResiduals, int kNumParameterBlocks) {
    	    			 super();
    	    		     set_num_residuals(kNumResiduals);
    	    		     for (int i = 0; i < kNumParameterBlocks; ++i) {
    	    		       mutable_parameter_block_sizes().add(1);
    	    		     }
    	    		   }

    	    		  public boolean Evaluate(Vector<double[]> parameters,
		                         double[] residuals,
		                         double[][] jacobians) {
    	    			  return true;
    	    		  }
    	    		  
    	    		  public boolean Evaluate(Vector<double[]> parameters,
		                         double[] residuals,
		                         double[][] jacobians,
		                         int[] jacobians_offsete) {
 	    			     return true;
 	    		     }

    	    		   
    	    	}

    	        public void ProgramReallocationInCreateJacobianBlockSparsityTranspose() {
    	        	      // ProgramReallocationInCreateJacobianBlockSparsityTranspose() passed all tests
    	    			  // CreateJacobianBlockSparsityTranspose starts with a conservative
    	    			  // estimate of the size of the sparsity pattern. This test ensures
    	    			  // that when those estimates are violated, the reallocation/resizing
    	    			  // logic works correctly.
                          int i, j;
    	    			  ProblemImpl problem = new ProblemImpl();
    	    			  double x[][] = new double[20][1];

    	    			  Vector<double[]> parameter_blocks = new Vector<double[]>();
    	    			  for (i = 0; i < 20; ++i) {
    	    			    problem.AddParameterBlock(x[i], 1);
    	    			    parameter_blocks.add(x[i]);
    	    			  }

    	    			  problem.AddResidualBlock(new NumParameterBlocksCostFunction(1, 20),
    	    			                           null,
    	    			                           parameter_blocks);

    	    			  TripletSparseMatrix expected_block_sparse_jacobian = new TripletSparseMatrix(20, 1, 20);
    	    			  {
    	    			    int[] rows = expected_block_sparse_jacobian.mutable_rows();
    	    			    int[] cols = expected_block_sparse_jacobian.mutable_cols();
    	    			    for (i = 0; i < 20; ++i) {
    	    			      rows[i] = i;
    	    			      cols[i] = 0;
    	    			    }

    	    			    double[] values = expected_block_sparse_jacobian.mutable_values();
    	    			    for (i = 0; i < 20; i++) {
    	    			    	values[i] = 1.0;
    	    			    }
    	    			    expected_block_sparse_jacobian.set_num_nonzeros(20);
    	    			  }

    	    			  Program program = problem.mutable_program();
    	    			  program.SetParameterOffsetsAndIndex();

    	    			  TripletSparseMatrix actual_block_sparse_jacobian = 
    	    			      program.CreateJacobianBlockSparsityTranspose();
    	    			  
    	    			  Matrix expected_dense_jacobian = expected_block_sparse_jacobian.ToDenseMatrix();

    	    			  Matrix actual_dense_jacobian = actual_block_sparse_jacobian.ToDenseMatrix();
    	    			  double normSquared = 0.0;
    	    			  double diff;
    	    			  for (i = 0; i < expected_dense_jacobian.getRowDimension(); i++) {
    	    				  for (j = 0; j < expected_dense_jacobian.getColumnDimension(); j++) {
    	    					  diff = expected_dense_jacobian.get(i,j) - actual_dense_jacobian.get(i,j);
    	    					  normSquared += diff * diff;
    	    				  }
    	    			  }
    	    			  double norm = Math.sqrt(normSquared);
    	    			  if (norm != 0.0) {
    	    				  System.err.println("In ProgramReallocationInCreateJacobianBlockSparsityTranspose() (expected_dense_jacobian - actual_dense_jacobian).norm() != 0.0");
    	    			  }
    	    			  else {
    	    				  System.out.println("ProgramReallocationInCreateJacobianBlockSparsityTranspose() passed all tests");
    	    			  }
    	    		}
    	        
    	        public void ProgramProblemHasNanParameterBlocks() {
    	        	  boolean passed = true;
    	        	  ProblemImpl problem = new ProblemImpl();
    	        	  double x[] = new double[2];
    	        	  x[0] = 1.0;
    	        	  x[1] = Double.NaN;
    	        	  problem.AddResidualBlock(new MockCostFunctionBase2(1, 2, 0, 0), null, x);
    	        	  String error[] = new String[1];
    	        	  if (problem.program().ParameterBlocksAreFinite(error)) {
    	        		  System.err.println("In ProgramProblemHasNanParameterBlocks() problem.program().ParameterBlocksAreFinite(error) = true"); 
    	        		  passed = false;
    	        	  }
    	        	  //System.err.println(error[0]);
    	        	  int index = error[0].indexOf("has at least one invalid value");
    	        	  if (index == -1) {
    	        		  System.err.println("In ProgramProblemHasNanParameterBlocks() no error message with: has at least one invalid value"); 
    	        		  passed = false;
    	        	  }
    	        	  if (passed) {
    	        		  System.out.println("ProgramProblemHasNanParameterBlocks() passed all tests");
    	        	  }
    	        }

    	        public void ProgramInfeasibleParameterBlock() {
    	        	  // ProgramInfeasibleParameterBlock() passed all tests
    	        	  boolean passed = true;
    	        	  ProblemImpl problem = new ProblemImpl();
    	        	  double x[] = new double[] {0.0, 0.0};
    	        	  problem.AddResidualBlock(new MockCostFunctionBase2(1, 2, 0, 0), null, x);
    	        	  problem.SetParameterLowerBound(x, 0, 2.0);
    	        	  problem.SetParameterUpperBound(x, 0, 1.0);
    	        	  String error[] = new String[1];
    	        	  if (problem.program().IsFeasible(error)) {
    	        		  System.err.println("In ProgramInfeasibleParameterBlock() problem.program().IsFeasible(error) = true");
    	        		  passed = false;
    	        	  }
    	        	  //System.err.println(error[0]);
    	        	  int index = error[0].indexOf("has lower_bound >= upper_bound");
    	        	  if (index == -1) {
    	        		  System.err.println("In ProgramInfeasibleParameterBlock() no error message with: has lower_bound >= upper_bound"); 
    	        		  passed = false;
    	        	  }
    	        	  if (passed) {
    	        		  System.out.println("ProgramInfeasibleParameterBlock() passed all tests");
    	        	  }
    	        }
    	        
    	        public void ProgramInfeasibleConstantParameterBlock() {
    	        	  // ProgramInfeasibleConstantParameterBlock() passed all tests
    	        	  boolean passed = true;
    	        	  ProblemImpl problem = new ProblemImpl();
    	        	  double x[] = new double[] {0.0, 0.0};
    	        	  problem.AddResidualBlock(new MockCostFunctionBase2(1, 2, 0, 0), null, x);
    	        	  problem.SetParameterLowerBound(x, 0, 1.0);
    	        	  problem.SetParameterUpperBound(x, 0, 2.0);
    	        	  problem.SetParameterBlockConstant(x);
    	        	  String error[] = new String[1];
    	        	  if (problem.program().IsFeasible(error)) {
    	        		  System.err.println("In ProgramInfeasibleConstantParameterBlock() problem.program().IsFeasible(error) = true");
    	        		  passed = false;
    	        	  }
    	        	  //System.err.println(error[0]);
    	        	  int index = error[0].indexOf("infeasible value");
    	        	  if (index == -1) {
    	        		  System.err.println("In ProgramInfeasibleConstantParameterBlock() no error message with: infeasible value"); 
    	        		  passed = false;
    	        	  }
    	        	  if (passed) {
    	        		  System.out.println("ProgramInfeasibleConstantParameterBlock() passed all tests");
    	        	  }
    	        }

    	        public void ArrayUtilsIsArrayValid() {
    	        	  // ArrayUtilsIsArrayValid() passed all tests
    	        	  boolean passed = true;
    	        	  double x[] = new double[3];
    	        	  x[0] = 0.0;
    	        	  x[1] = 1.0;
    	        	  x[2] = 2.0;
    	        	  if (!IsArrayValid(3, x)) {
    	        		  System.err.println("In ArrayUtilsIsArrayValid() IsArrayValid(3, x) = false");
    	        		  passed = false;
    	        	  }
    	        	  x[1] = Double.POSITIVE_INFINITY;
    	        	  if (IsArrayValid(3, x)) {
    	        		  System.err.println("In ArrayUtilsIsArrayValid() IsArrayValid(3, x) = true with POSITIVE_INFINITY");
    	        		  passed = false;
    	        	  }
    	        	  x[1] = Double.NaN;
    	        	  if (IsArrayValid(3, x)) {
    	        		  System.err.println("In ArrayUtilsIsArrayValid() IsArrayValid(3, x) = true with NaN");
    	        		  passed = false;
    	        	  }
    	        	  double y[] = null;
    	        	  if (!IsArrayValid(1, y)) {
    	        		  System.err.println("In ArrayUtilsIsArrayValid() IsArrayValid(1, y) = false");
    	        		  passed = false;
    	        	  }
    	        	  InvalidateArray(3, x);
    	        	  if (IsArrayValid(3, x)) {
    	        		  System.err.println("In ArrayUtilsIsArrayValid() IsArrayValid(3, x) = true with InvalidateArray(3, x)");
    	        		  passed = false;
    	        	  }
    	        	  if (passed) {
    	        		  System.out.println("ArrayUtilsIsArrayValid() passed all tests");
    	        	  }
    	        }
    	        
    	        public void ArrayUtilsFindInvalidIndex() {
    	        	  // ArrayUtilsFindInvalidIndex() passed all tests
    	        	  boolean passed = true;
    	        	  double x[] = new double[3];
    	        	  x[0] = 0.0;
    	        	  x[1] = 1.0;
    	        	  x[2] = 2.0;
    	        	  if (FindInvalidValue(3, x) != 3) {
    	        		  System.err.println("In ArrayUtilsFindInvalidIndex() FindInvalidValue(3, x) != 3");
    	        		  passed = false;
    	        	  }
    	        	  x[1] = Double.POSITIVE_INFINITY;
    	        	  if (FindInvalidValue(3, x) != 1) {
    	        		  System.err.println("In ArrayUtilsFindInvalidIndex() FindInvalidValue(3, x) != 1 with POSITIVE_INFINITY");
    	        		  passed = false;
    	        	  }
    	        	  x[1] = Double.NaN;
    	        	  if (FindInvalidValue(3, x) != 1) {
    	        		  System.err.println("In ArrayUtilsFindInvalidIndex() FindInvalidValue(3, x) != 1 with NaN");
    	        		  passed = false;
    	        	  }
    	        	  if (FindInvalidValue(1, null) != 1) {
    	        		  System.err.println("In ArrayUtilsFindInvalidIndex() FindInvalidValue(1, null) != 1");
    	        		  passed = false;
    	        	  }
    	        	  InvalidateArray(3, x);
    	        	  if (FindInvalidValue(3, x) != 0) {
    	        		  System.err.println("In ArrayUtilsFindInvalidIndex() FindInvalidValue(3, x) != 0 with InvalidateArray(3, x)");
    	        		  passed = false;
    	        	  }
    	        	  if (passed) {
    	        		  System.out.println("ArrayUtilsFindInvalidIndex() passed all tests");
    	        	  }
    	        }
    	        
    	        public void MapValuesToContiguousRangeContiguousEntries() {
    	        	  // MapValuesToContiguousRangeContiguousEntries() passed all tests
    	        	  int i;
    	        	  boolean passed = true;
    	        	  int array[] = new int[] {0,1};
    	        	  int expected[] = new int[] {0,1};
    	        	  ce2.MapValuesToContiguousRange(array);
    	        	  for (i = 0; i < array.length; i++) {
    	        		  if (array[i] != expected[i]) {
    	        			  System.err.println("In MapValuesToContiguousRangeContiguousEntries() array["+i+"] != expected["+i+"]");
    	        			  passed = false;
    	        		  }
    	        	  }
    	        	  array = null;

    	        	  array = new int[] {1, 0};
    	        	  expected = new int[] {1, 0};
    	        	  ce2.MapValuesToContiguousRange(array);
    	        	  for (i = 0; i < array.length; i++) {
    	        		  if (array[i] != expected[i]) {
    	        			  System.err.println("In MapValuesToContiguousRangeContiguousEntries() array["+i+"] != expected["+i+"]");
    	        			  passed = false;
    	        		  }
    	        	  }
    	        	  if (passed) {
    	        		  System.out.println("MapValuesToContiguousRangeContiguousEntries() passed all tests");
    	        	  }
    	        }

    	        public void MapValuesToContiguousRangeNonContiguousEntries() {
    	        	  // MapValuesToContiguousRangeNonContiguousEntries() passed all tests
    	        	  int i;
    	        	  boolean passed = true;
    	        	  int array[] = new int[] {0,2};
    	        	  int expected[] = new int[] {0,1};
    	        	  ce2.MapValuesToContiguousRange(array);
    	        	  for (i = 0; i < array.length; i++) {
    	        		  if (array[i] != expected[i]) {
    	        			  System.err.println("In MapValuesToContiguousRangeNonContiguousEntries() array["+i+"] != expected["+i+"]");
    	        			  passed = false;
    	        		  }
    	        	  }
    	        	  if (passed) {
    	        		  System.out.println("MapValuesToContiguousRangeNonContiguousEntries() passed all tests");
    	        	  }
    	        }

    	        public void MapValuesToContiguousRangeNonContiguousRepeatingEntries() {
    	        	  // MapValuesToContiguousRangeNonContiguousRepeatingEntries() passed all tests
    	        	  int i;
    	        	  boolean passed = true;
    	        	  int array[] = new int[] {3, 1, 0, 0, 0, 5};
    	        	  int expected[] = new int[] {2, 1, 0, 0, 0, 3};
    	        	  ce2.MapValuesToContiguousRange(array);
    	        	  for (i = 0; i < array.length; i++) {
    	        		  if (array[i] != expected[i]) {
    	        			  System.err.println("In MapValuesToContiguousRangeNonContiguousRepeatingEntries() array["+i+"] != expected["+i+"]");
    	        			  passed = false;
    	        		  }
    	        	  }
    	        	  if (passed) {
    	        		  System.out.println("MapValuesToContiguousRangeNonContiguousRepeatingEntries() passed all tests");
    	        	  }
    	        }
    	        
    	     // Use as:
    	     // double quaternion[4];
    	     // EXPECT_THAT(quaternion, IsNormalizedQuaternion());
    	    public boolean IsNormalizedQuaternion(double arg[]) {
    	       double kTolerance = 10.0 * epsilon;
    	       if (arg == null) {
    	         System.err.println("Null quaternion");
    	         return false;
    	       }

    	       double norm2 = arg[0] * arg[0] + arg[1] * arg[1] +
    	           arg[2] * arg[2] + arg[3] * arg[3];
    	       if (Math.abs(norm2 - 1.0) > kTolerance) {
    	         System.err.println("squared norm is " + norm2);
    	         return false;
    	       }

    	       return true;
    	    }
    	 // Use as:
    	 // double expected_quaternion[4];
    	 // double actual_quaternion[4];
    	 // EXPECT_THAT(actual_quaternion, IsNearQuaternion(expected_quaternion));
    	 public boolean IsNearQuaternion(double arg[], double expected[]) {
    		 double kTolerance = 10.0 * epsilon;
    	   if (arg == null) {
    	     System.err.println("Null quaternion");
    	     return false;
    	   }

    	   // Quaternions are equivalent upto a sign change. So we will compare
    	   // both signs before declaring failure.
    	   boolean near = true;
    	   for (int i = 0; i < 4; i++) {
    	     if (Math.abs(arg[i] - expected[i]) > kTolerance) {
    	       near = false;
    	       break;
    	     }
    	   }

    	   if (near) {
    	     return true;
    	   }

    	   near = true;
    	   for (int i = 0; i < 4; i++) {
    	     if (Math.abs(arg[i] + expected[i]) > kTolerance) {
    	       near = false;
    	       break;
    	     }
    	   }

    	   if (near) {
    	     return true;
    	   }

    	   System.err.println("expected : " +
    	                    expected[0] + " "
    	                    + expected[1] + " "
    	                    + expected[2] + " "
    	                    + expected[3]);
    	   System.err.println("actual : " +
    	                    arg[0] + " "
    	                    + arg[1] + " "
    	                    + arg[2] + " "
    	                    + arg[3]);
    	   return false;
    	 }

    	// Use as:
    	// double expected_axis_angle[3];
    	// double actual_axis_angle[3];
    	// EXPECT_THAT(actual_axis_angle, IsNearAngleAxis(expected_axis_angle));
    	public boolean IsNearAngleAxis(double arg[], double expected[]) {
    	  int i;
    	  double kLooseTolerance = 1.0E-9;
    	  if (arg == null) {
    	    System.err.println("Null axis/angle");
    	    return false;
    	  }
  
    	  final double e_norm = ce2.norm(expected);
    	  double aminuse[] = new double[arg.length];
    	  double apluse[] = new double[arg.length];
    	  for (i = 0; i < arg.length; i++) {
    		  aminuse[i] = arg[i] - expected[i];
    		  apluse[i] = arg[i] + expected[i];
    	  }

    	  double delta_norm = Double.MAX_VALUE;
    	  if (e_norm > 0) {
    	    // Deal with the sign ambiguity near PI. Since the sign can flip,
    	    // we take the smaller of the two differences.
    	    if (Math.abs(e_norm - Math.PI) < kLooseTolerance) {
    	      delta_norm = Math.min(ce2.norm(aminuse), ce2.norm(apluse)) / e_norm;
    	    } else {
    	      delta_norm = ce2.norm(aminuse) / e_norm;
    	    }
    	  } else {
    	    delta_norm = ce2.norm(arg);
    	  }

    	  if (delta_norm <= kLooseTolerance) {
    	    return true;
    	  }

    	  System.err.println("arg: " +
                  arg[0] + " "
                  + arg[1] + " "
                  + arg[2]);
          System.err.println("was expected to be: " +
                  expected[0] + " "
                  + expected[1] + " "
                  + expected[2]);
    	  
    	  return false;
    	}
    	
    	// Use as:
    	// double matrix[9];
    	// EXPECT_THAT(matrix, IsOrthonormal());
    	public boolean IsOrthonormal(double arg[]) {
    	  double kTolerance = 10.0 * epsilon;
    	  if (arg == null) {
    	    System.err.println("Null matrix");
    	    return false;
    	  }

    	  for (int c1 = 0; c1 < 3; c1++) {
    	    for (int c2 = 0; c2 < 3; c2++) {
    	      double v = 0;
    	      for (int i = 0; i < 3; i++) {
    	        v += arg[i + 3 * c1] * arg[i + 3 * c2];
    	      }
    	      double expected = (c1 == c2) ? 1 : 0;
    	      if (Math.abs(expected - v) > kTolerance) {
    	        System.err.println("Columns " + c1 + " and " + c2);
    	        System.err.println("should have dot product " + expected);
    	        System.err.println("but have " + v);
    	        return false;
    	      }
    	    }
    	  }

    	  return true;
    	}

    	// Use as:
    	// double matrix1[9];
    	// double matrix2[9];
    	// EXPECT_THAT(matrix1, IsNear3x3Matrix(matrix2));
    	public boolean IsNear3x3Matrix(double arg[], double expected[]) {
    	  double kTolerance = 10.0 * epsilon;
    	  if (arg == null) {
    	    System.err.println("Null matrix");
    	    return false;
    	  }

    	  for (int i = 0; i < 9; i++) {
    	    if (Math.abs(arg[i] - expected[i]) > kTolerance) {
    	      System.err.println("component " + i + " = " + arg[i] + " should be " + expected[i]);
    	      return false;
    	    }
    	  }

    	  return true;
    	}
    	
    	// Transforms a zero axis/angle to a quaternion.
    	public void RotationZeroAngleAxisToQuaternion() {
    	  // RotationZeroAngleAxisToQuaternion() passed all tests
    	  boolean passed = true;
    	  double axis_angle[] = new double[] { 0, 0, 0 };
    	  double quaternion[] = new double[4];
    	  double expected[] = new double[] { 1, 0, 0, 0 };
    	  ce2.AngleAxisToQuaternion(axis_angle, quaternion);
    	  if (!IsNormalizedQuaternion(quaternion)) {
    		  System.err.println("In RotationZeroAngleAxisToQuaternion() IsNormalizedQuaternion(quaternion) = false");
    		  passed = false;
    	  }
    	  if (!IsNearQuaternion(quaternion, expected)) {
    		  System.err.println("In RotationZeroAngleAxisToQuaternion() IsNearQuaternion(quaternion, expected) = false");
    		  passed = false;
    	  }
    	  if (passed) {
    		  System.out.println("RotationZeroAngleAxisToQuaternion() passed all tests");
    	  }
    	}
    	
    	// Test that exact conversion works for small angles.
    	public void RotationSmallAngleAxisToQuaternion() {
    	  // RotationSmallAngleAxisToQuaternion() passed all tests
    	  boolean passed = true;
    	  // Small, finite value to test.
    	  double theta = 1.0e-2;
    	  double axis_angle[] = new double[] { theta, 0, 0 };
    	  double quaternion[] = new double[4];
    	  double expected[] = new double[] { Math.cos(theta/2), Math.sin(theta/2.0), 0, 0 };
    	  ce2.AngleAxisToQuaternion(axis_angle, quaternion);
    	  if (!IsNormalizedQuaternion(quaternion)) {
    		  System.err.println("In RotationSmallAngleAxisToQuaternion() IsNormalizedQuaternion(quaternion) = false");
    		  passed = false;
    	  }
    	  if (!IsNearQuaternion(quaternion, expected)) {
    		  System.err.println("In RotationSmallAngleAxisToQuaternion() IsNearQuaternion(quaternion, expected) = false");
    		  passed = false;
    	  }
    	  if (passed) {
    		  System.out.println("RotationSmallAngleAxisToQuaternion() passed all tests");
    	  }
    	}
    	
    	// Test that approximate conversion works for very small angles.
    	public void RotationTinyAngleAxisToQuaternion() {
          // RotationTinyAngleAxisToQuaternion() passed all tests
    	  boolean passed = true;
    	  // Very small value that could potentially cause underflow.
    	  double theta = Math.pow(Double.MIN_VALUE, 0.75);
    	  double axis_angle[] = new double[] { theta, 0, 0 };
    	  double quaternion[] = new double[4];
    	  double expected[] = new double[] { Math.cos(theta/2), Math.sin(theta/2.0), 0, 0 };
    	  ce2.AngleAxisToQuaternion(axis_angle, quaternion);
    	  if (!IsNormalizedQuaternion(quaternion)) {
    		  System.err.println("In RotationTinyAngleAxisToQuaternion() IsNormalizedQuaternion(quaternion) = false");
    		  passed = false;
    	  }
    	  if (!IsNearQuaternion(quaternion, expected)) {
    		  System.err.println("In RotationTinyAngleAxisToQuaternion() IsNearQuaternion(quaternion, expected) = false");
    		  passed = false;
    	  }
    	  if (passed) {
    		  System.out.println("RotationTinyAngleAxisToQuaternion() passed all tests");
    	  }
    	}
    	
    	// Transforms a rotation by pi/2 around X to a quaternion.
    	public void RotationXRotationToQuaternion() {
    	  // RotationXRotationToQuaternion() passed all tests
    	  boolean passed = true;
    	  double kHalfSqrt2 = 1.0/Math.sqrt(2.0);
    	  double axis_angle[] = new double[] { Math.PI / 2, 0, 0 };
    	  double quaternion[] = new double[4];
    	  double expected[] = { kHalfSqrt2, kHalfSqrt2, 0, 0 };
    	  ce2.AngleAxisToQuaternion(axis_angle, quaternion);
    	  if (!IsNormalizedQuaternion(quaternion)) {
    		  System.err.println("In RotationXRotationToQuaternion() IsNormalizedQuaternion(quaternion) = false");
    		  passed = false;
    	  }
    	  if (!IsNearQuaternion(quaternion, expected)) {
    		  System.err.println("In RotationXRotationToQuaternion() IsNearQuaternion(quaternion, expected) = false");
    		  passed = false;
    	  }
    	  if (passed) {
    		  System.out.println("RotationXRotationToQuaternion() passed all tests");
    	  }
    	}

    	// Transforms a unit quaternion to an axis angle.
    	public void RotationUnitQuaternionToAngleAxis() {
    	  // RotationUnitQuaternionToAngleAxis() passed all tests
    	  double quaternion[] = new double[] { 1, 0, 0, 0 };
    	  double axis_angle[] = new double[3];
    	  double expected[] = new double[] { 0, 0, 0 };
    	  ce2.QuaternionToAngleAxis(quaternion, axis_angle);
    	  if (!IsNearAngleAxis(axis_angle, expected)) {
    		  System.err.println("In RotationUnitQuaternionToAngleAxis() IsNearAngleAxis(axis_angle, expected) = false");
    	  }
    	  else {
    		  System.out.println("RotationUnitQuaternionToAngleAxis() passed all tests");
    	  }
    	}

    	// Transforms a quaternion that rotates by pi about the Y axis to an axis angle.
    	public void RotationYRotationQuaternionToAngleAxis() {
    	  // RotationYRotationQuaternionToAngleAxis() passed all tests
    	  double quaternion[] = new double[] { 0, 0, 1, 0 };
    	  double axis_angle[] = new double[3];
    	  double expected[] = new double[]{ 0, Math.PI, 0 };
    	  ce2.QuaternionToAngleAxis(quaternion, axis_angle);
    	  if (!IsNearAngleAxis(axis_angle, expected)) {
    		  System.err.println("In RotationYRotationQuaternionToAngleAxis() IsNearAngleAxis(axis_angle, expected) = false");
    	  }
    	  else {
    		  System.out.println("RotationYRotationQuaternionToAngleAxis() passed all tests");
    	  }
    	}
    	
    	// Transforms a quaternion that rotates by pi/3 about the Z axis to an axis
    	// angle.
    	public void RotationZRotationQuaternionToAngleAxis() {
    	  // RotationZRotationQuaternionToAngleAxis() passed all tests
    	  double quaternion[] = new double[] { Math.sqrt(3) / 2, 0, 0, 0.5 };
    	  double axis_angle[] = new double[3];
    	  double expected[] = new double[] { 0, 0, Math.PI / 3 };
    	  ce2.QuaternionToAngleAxis(quaternion, axis_angle);
    	  if (!IsNearAngleAxis(axis_angle, expected)) {
    		  System.err.println("In RotationZRotationQuaternionToAngleAxis() IsNearAngleAxis(axis_angle, expected) = false");
    	  }
    	  else {
    		  System.out.println("RotationZRotationQuaternionToAngleAxis() passed all tests");
    	  }
    	}

    	// Test that exact conversion works for small angles.
    	public void RotationSmallQuaternionToAngleAxis() {
    	  // RotationSmallQuaternionToAngleAxis() passed all tests
    	  // Small, finite value to test.
    	  double theta = 1.0e-2;
    	  double quaternion[] = new double[] { Math.cos(theta/2), Math.sin(theta/2.0), 0, 0 };
    	  double axis_angle[] = new double[3];
    	  double expected[] = new double[] { theta, 0, 0 };
    	  ce2.QuaternionToAngleAxis(quaternion, axis_angle);
    	  if (!IsNearAngleAxis(axis_angle, expected)) {
    		  System.err.println("In RotationSmallQuaternionToAngleAxis() IsNearAngleAxis(axis_angle, expected) = false");
    	  }
    	  else {
    		  System.out.println("RotationSmallQuaternionToAngleAxis() passed all tests");
    	  }
    	}
    	
    	// Test that approximate conversion works for very small angles.
        public void RotationTinyQuaternionToAngleAxis() {
          // RotationTinyQuaternionToAngleAxis() passed all tests
    	  // Very small value that could potentially cause underflow.
    	  double theta = Math.pow(Double.MIN_VALUE, 0.75);
    	  double quaternion[] = new double[] { Math.cos(theta/2), Math.sin(theta/2.0), 0, 0 };
    	  double axis_angle[] = new double[3];
    	  double expected[] = new double[] { theta, 0, 0 };
    	  ce2.QuaternionToAngleAxis(quaternion, axis_angle);
    	  if (!IsNearAngleAxis(axis_angle, expected)) {
    		  System.err.println("In RotationTinyQuaternionToAngleAxis() IsNearAngleAxis(axis_angle, expected) = false");
    	  }
    	  else {
    		  System.out.println("RotationTinyQuaternionToAngleAxis() passed all tests");
    	  }
    	}

        public void RotationQuaternionToAngleAxisAngleIsLessThanPi() {
        	  // RotationQuaternionToAngleAxisAngleIsLessThanPi() passed all tests
        	  double quaternion[] = new double[4];
        	  double angle_axis[] = new double[3];

        	  final double half_theta = 0.75 * Math.PI;

        	  quaternion[0] = Math.cos(half_theta);
        	  quaternion[1] = 1.0 * Math.sin(half_theta);
        	  quaternion[2] = 0.0;
        	  quaternion[3] = 0.0;
        	  ce2.QuaternionToAngleAxis(quaternion, angle_axis);
        	  final double angle = Math.sqrt(angle_axis[0] * angle_axis[0] +
        	                            angle_axis[1] * angle_axis[1] +
        	                            angle_axis[2] * angle_axis[2]);
        	  if (angle > Math.PI) {
        		  System.err.println("In RotationQuaternionToAngleAxisAngleIsLessThanPi() angle > Math.PI");
        	  }
        	  else {
        		  System.out.println("RotationQuaternionToAngleAxisAngleIsLessThanPi() passed all tests");
        	  }
        }
        
        static final int kNumTrials = 10000;

     // Takes a bunch of random axis/angle values, converts them to quaternions,
     // and back again.
     public void RotationAngleAxisToQuaterionAndBack() {
       // RotationAngleAxisToQuaterionAndBack() passed all tests
       boolean passed = true;
       for (int i = 0; i < kNumTrials; i++) {
         double axis_angle[] = new double[3];
         // Make an axis by choosing three random numbers in [-1, 1) and
         // normalizing.
         double norm = 0;
         for (int j = 0; j < 3; j++) {
           axis_angle[j] = RandDouble() * 2 - 1;
           norm += axis_angle[j] * axis_angle[j];
         }
         norm = Math.sqrt(norm);

         // Angle in [-pi, pi).
         double theta = Math.PI * 2 * RandDouble() - Math.PI;
         for (int j = 0; j < 3; j++) {
           axis_angle[j] = axis_angle[j] * theta / norm;
         }

         double quaternion[] = new double[4];
         double round_trip[] = new double[3];
         // We use ASSERTs here because if there's one failure, there are
         // probably many and spewing a million failures doesn't make anyone's
         // day.
         ce2.AngleAxisToQuaternion(axis_angle, quaternion);
         if (!IsNormalizedQuaternion(quaternion)) {
   		  System.err.println("In RotationAngleAxisToQuaterionAndBack() IsNormalizedQuaternion(quaternion) = false");
   		  passed = false;
   	     }
         ce2.QuaternionToAngleAxis(quaternion, round_trip);
         if (!IsNearAngleAxis(round_trip, axis_angle)) {
   		  System.err.println("In RotationAngleAxisToQuaterionAndBack() IsNearAngleAxis(axis_angle, expected) = false");
   		  passed = false;
   	     }
       }
       if (passed) {
      	 System.out.println("RotationAngleAxisToQuaterionAndBack() passed all tests");
       }
     }

  // Takes a bunch of random quaternions, converts them to axis/angle,
  // and back again.
  public void RotationQuaterionToAngleAxisAndBack() {
	// RotationQuaterionToAngleAxisAndBack() passed all tests
    boolean passed = true;
    for (int i = 0; i < kNumTrials; i++) {
      double quaternion[] = new double[4];
      // Choose four random numbers in [-1, 1) and normalize.
      double norm = 0;
      for (int j = 0; j < 4; j++) {
        quaternion[j] = RandDouble() * 2 - 1;
        norm += quaternion[j] * quaternion[j];
      }
      norm = Math.sqrt(norm);

      for (int j = 0; j < 4; j++) {
        quaternion[j] = quaternion[j] / norm;
      }

      double axis_angle[] = new double[3];
      double round_trip[] = new double[4];
      ce2.QuaternionToAngleAxis(quaternion, axis_angle);
      ce2.AngleAxisToQuaternion(axis_angle, round_trip);
      if (!IsNormalizedQuaternion(round_trip)) {
    	  System.err.println("In RotationQuaterionToAngleAxisAndBack() IsNormalizedQuaternion(round_trip) = false");
    	  passed = false;
      }
      if (!IsNearQuaternion(round_trip, quaternion)) {
    	  System.err.println("In RotationQuaterionToAngleAxisAndBack() IsNearQuaternion(round_trip, quaternion) = false");
    	  passed = false;
      }
    }
    if (passed) {
    	System.out.println("RotationQuaterionToAngleAxisAndBack() passed all tests");
    }
  }
  
	//Transforms a zero axis/angle to a rotation matrix.
	public void RotationZeroAngleAxisToRotationMatrix() {
	 // RotationZeroAngleAxisToRotationMatrix() passed all tests
	 boolean passed = true;
	 double axis_angle[] = new double[]{ 0, 0, 0 };
	 double matrix[] = new double[9];
	 double expected[] = new double[] { 1, 0, 0, 0, 1, 0, 0, 0, 1 };
	 ce2.AngleAxisToRotationMatrix(axis_angle, matrix);
	 if (!IsOrthonormal(matrix)) {
		 System.err.println("In RotationZeroAngleAxisToRotationMatrix() IsOrthonormal(matrix) = false");
		 passed = false;
	 }
	 if (!IsNear3x3Matrix(matrix, expected)) {
		 System.err.println("In RotationZeroAngleAxisToRotationMatrix() IsNear3x3Matrix(matrix, expected) = false");
		 passed = false;
	 }
	 if (passed) {
		 System.out.println("RotationZeroAngleAxisToRotationMatrix() passed all tests");
	 }
	}
	
	public void RotationNearZeroAngleAxisToRotationMatrix() {
		  // RotationNearZeroAngleAxisToRotationMatrix() passed all tests
		  boolean passed = true;
		  double axis_angle[] = new double[] { 1e-24, 2e-24, 3e-24 };
		  double matrix[] = new double[9];
		  double expected[] = new double[]{ 1, 0, 0, 0, 1, 0, 0, 0, 1 };
		  ce2.AngleAxisToRotationMatrix(axis_angle, matrix);
		  if (!IsOrthonormal(matrix)) {
			 System.err.println("In RotationNearZeroAngleAxisToRotationMatrix() IsOrthonormal(matrix) = false");
			 passed = false;
		  }
		  if (!IsNear3x3Matrix(matrix, expected)) {
			 System.err.println("In RotationNearZeroAngleAxisToRotationMatrix() IsNear3x3Matrix(matrix, expected) = false");
			 passed = false;
		  }
		  if (passed) {
			 System.out.println("RotationNearZeroAngleAxisToRotationMatrix() passed all tests");
		  }
	}
	
	// Transforms a rotation by pi/2 around X to a rotation matrix and back.
	public void RotationXRotationToRotationMatrix() {
      // RotationXRotationToRotationMatrix() passed all tests
	  boolean passed = true;
	  double axis_angle[] = new double[] { Math.PI / 2, 0, 0 };
	  double matrix[] = new double[9];
	  // The rotation matrices are stored column-major.
	  double expected[] = new double[] { 1, 0, 0, 0, 0, 1, 0, -1, 0 };
	  ce2.AngleAxisToRotationMatrix(axis_angle, matrix);
	  if (!IsOrthonormal(matrix)) {
		 System.err.println("In RotationXRotationToRotationMatrix() IsOrthonormal(matrix) = false");
		 passed = false;
	  }
	  if (!IsNear3x3Matrix(matrix, expected)) {
		 System.err.println("In RotationXRotationToRotationMatrix() IsNear3x3Matrix(matrix, expected) = false");
		 passed = false;
	  }
	  double round_trip[] = new double[3];
	  ce2.RotationMatrixToAngleAxis(matrix, round_trip);
	  if (!IsNearAngleAxis(round_trip, axis_angle)) {
		  System.err.println("In RotationXRotationToRotationMatrix() IsNearAngleAxis(round_trip, axis_angle) = false");
		  passed = false; 
	  }
	  if (passed) {
		  System.out.println("RotationXRotationToRotationMatrix() passed all tests");
	  }
	}

	// Transforms an axis angle that rotates by pi about the Y axis to a
	// rotation matrix and back.
	public void RotationYRotationToRotationMatrix() {
	  // RotationYRotationToRotationMatrix() passed all tests
	  boolean passed = true;
	  double axis_angle[] = new double[]{ 0, Math.PI, 0 };
	  double matrix[] = new double[9];
	  double expected[] = new double[] { -1, 0, 0, 0, 1, 0, 0, 0, -1 };
	  ce2.AngleAxisToRotationMatrix(axis_angle, matrix);
	  if (!IsOrthonormal(matrix)) {
		 System.err.println("In RotationYRotationToRotationMatrix() IsOrthonormal(matrix) = false");
		 passed = false;
	  }
	  if (!IsNear3x3Matrix(matrix, expected)) {
		 System.err.println("In RotationYRotationToRotationMatrix() IsNear3x3Matrix(matrix, expected) = false");
		 passed = false;
	  }
	  double round_trip[] = new double[3];
	  ce2.RotationMatrixToAngleAxis(matrix, round_trip);
	  if (!IsNearAngleAxis(round_trip, axis_angle)) {
		  System.err.println("In RotationYRotationToRotationMatrix() IsNearAngleAxis(round_trip, axis_angle) = false");
		  passed = false; 
	  }
	  if (passed) {
		  System.out.println("RotationYRotationToRotationMatrix() passed all tests");
	  }
	}

	public void RotationNearPiAngleAxisRoundTrip() {
		  // RotationNearPiAngleAxisRoundTrip() passed all tests
		  boolean passed = true;
		  double in_axis_angle[] = new double[3];
		  double matrix[] = new double[9];
		  double out_axis_angle[] = new double[3];

		  for (int i = 0; i < kNumTrials; i++) {
		    // Make an axis by choosing three random numbers in [-1, 1) and
		    // normalizing.
		    double norm = 0;
		    for (int j = 0; j < 3; j++) {
		      in_axis_angle[j] = RandDouble() * 2 - 1;
		      norm += in_axis_angle[j] * in_axis_angle[j];
		    }
		    norm = Math.sqrt(norm);

		    // Angle in [pi - kMaxSmallAngle, pi).
		    final double kMaxSmallAngle = 1e-8;
		    double theta = Math.PI - kMaxSmallAngle * RandDouble();

		    for (int j = 0; j < 3; j++) {
		      in_axis_angle[j] *= (theta / norm);
		    }
		    ce2.AngleAxisToRotationMatrix(in_axis_angle, matrix);
		    ce2.RotationMatrixToAngleAxis(matrix, out_axis_angle);
		    if (!IsNearAngleAxis(in_axis_angle, out_axis_angle)) {
		    	System.err.println("In RotationNearPiAngleAxisRoundTrip() IsNearAngleAxis(in_axis_angle, out_axis_angle) = false");
		    	passed = false;
		    }
		  }
		  if (passed) {
			  System.out.println("RotationNearPiAngleAxisRoundTrip() passed all tests");
		  }
		}
	
	public void RotationAtPiAngleAxisRoundTrip() {
		  // RotationAtPiAngleAxisRoundTrip() passed all tests
		  boolean passed = true;
		  // A rotation of kPi about the X axis;
		  final double kMatrix[][] = new double[][] {
		    {1.0,  0.0,  0.0},
		    {0.0,  -1.0,  0.0},
		    {0.0,  0.0,  -1.0}
		  };

		  double in_matrix[] = new double[9];
		  // Fill it from kMatrix in col-major order.
		  for (int j = 0, k = 0; j < 3; ++j) {
		     for (int i = 0; i < 3; ++i, ++k) {
		       in_matrix[k] = kMatrix[i][j];
		     }
		  }

		  final double expected_axis_angle[] = new double[] { Math.PI, 0, 0 };

		  double out_matrix[] = new double[9];
		  double axis_angle[] = new double[3];
		  ce2.RotationMatrixToAngleAxis(in_matrix, axis_angle);
		  ce2.AngleAxisToRotationMatrix(axis_angle, out_matrix);

		  /*LOG(INFO) << "AngleAxis = " << axis_angle[0] << " " << axis_angle[1]
		            << " " << axis_angle[2];
		  LOG(INFO) << "Expected AngleAxis = " << kPi << " 0 0";
		  double out_rowmajor[3][3];
		  for (int j = 0, k = 0; j < 3; ++j) {
		    for (int i = 0; i < 3; ++i, ++k) {
		      out_rowmajor[i][j] = out_matrix[k];
		    }
		  }
		  LOG(INFO) << "Rotation:";
		  LOG(INFO) << "EXPECTED        |        ACTUAL";
		  for (int i = 0; i < 3; ++i) {
		    string line;
		    for (int j = 0; j < 3; ++j) {
		      StringAppendF(&line, "%g ", kMatrix[i][j]);
		    }
		    line += "         |        ";
		    for (int j = 0; j < 3; ++j) {
		      StringAppendF(&line, "%g ", out_rowmajor[i][j]);
		    }
		    LOG(INFO) << line;
		  }*/

		  if (!IsNearAngleAxis(axis_angle, expected_axis_angle)) {
			  System.err.println("In RotationAtPiAngleAxisRoundTrip() IsNearAngleAxis(axis_angle, expected_axis_angle) = false");
			  passed = false;
		  }
		  if (!IsNear3x3Matrix(out_matrix, in_matrix)) {
			  System.err.println("In RotationAtPiAngleAxisRoundTrip() IsNear3x3Matrix(out_matrix, in_matrix) = false");
			  passed = false;
		  }
		  if (passed) {
			  System.out.println("RotationAtPiAngleAxisRoundTrip() passed all tests");
		  }
		}

	// Transforms an axis angle that rotates by pi/3 about the Z axis to a
	// rotation matrix.
	public void RotationZRotationToRotationMatrix() {
	  // RotationZRotationToRotationMatrix() passed all tests
      boolean passed = true;
	  double axis_angle[] =  new double[] { 0, 0, Math.PI / 3 };
	  double matrix[] = new double[9];
	  // This is laid-out row-major on the screen but is actually stored
	  // column-major.
	  double expected[] = new double[] { 0.5, Math.sqrt(3) / 2, 0,   // Column 1
	                         -Math.sqrt(3) / 2, 0.5, 0,  // Column 2
	                         0, 0, 1 };             // Column 3
	  ce2.AngleAxisToRotationMatrix(axis_angle, matrix);
	  if (!IsOrthonormal(matrix)) {
		  System.err.println("In RotationZRotationToRotationMatrix() IsOrthonormal(matrix) = false");
		  passed = false;
	  }
	  if (!IsNear3x3Matrix(matrix, expected)) {
		  System.err.println("In RotationZRotationToRotationMatrix() IsNear3x3Matrix(matrix, expected) = false");
		  passed = false;
	  }
	  double round_trip[] = new double[3];
	  ce2.RotationMatrixToAngleAxis(matrix, round_trip);
	  if (!IsNearAngleAxis(round_trip, axis_angle)) {
		  System.err.println("In RotationZRotationToRotationMatrix() IsNearAngleAxis(round_trip, axis_angle) = false");
		  passed = false;
	  }
	  if (passed) {
		 System.out.println("RotationZRotationToRotationMatrix() passed all tests"); 
	  }
	}
	
	// Takes a bunch of random axis/angle values, converts them to rotation
	// matrices, and back again.
	public void RotationAngleAxisToRotationMatrixAndBack() {
     // RotationAngleAxisToRotationMatrixAndBack() passed all tests
	 double kLooseTolerance = 1.0E-9;
	 boolean passed = true;
	  for (int i = 0; i < kNumTrials; i++) {
	    double axis_angle[] = new double[3];
	    // Make an axis by choosing three random numbers in [-1, 1) and
	    // normalizing.
	    double norm = 0;
	    for (int j = 0; j < 3; j++) {
	      axis_angle[j] = RandDouble() * 2 - 1;
	      norm += axis_angle[j] * axis_angle[j];
	    }
	    norm = Math.sqrt(norm);

	    // Angle in [-pi, pi).
	    double theta = Math.PI * 2 * RandDouble() - Math.PI;
	    for (int j = 0; j < 3; j++) {
	      axis_angle[j] = axis_angle[j] * theta / norm;
	    }

	    double matrix[] = new double[9];
	    double round_trip[] = new double[3];
	    ce2.AngleAxisToRotationMatrix(axis_angle, matrix);
	    if (!IsOrthonormal(matrix)) {
	    	System.err.println("In RotationAngleAxisToRotationMatrixAndBack() IsOrthonormal(matrix) = false");
	    	passed = false;
	    }
	    ce2.RotationMatrixToAngleAxis(matrix, round_trip);

	    for (int j = 0; j < 3; ++j) {
	      if (Math.abs(round_trip[j] - axis_angle[j]) > kLooseTolerance) {
	    	  System.err.println("In RotationAngleAxisToRotationMatrixAndBack() Math.abs(round_trip[j] - axis_angle[j]) > kLooseTolerance");
	    	  passed = false;
	      }
	    }
	  }
	  if (passed) {
		  System.out.println("RotationAngleAxisToRotationMatrixAndBack() passed all tests");
	  }
	}

	// Takes a bunch of random axis/angle values near zero, converts them
	// to rotation matrices, and back again.
	public void RotationAngleAxisToRotationMatrixAndBackNearZero() {
      // RotationAngleAxisToRotationMatrixAndBackNearZero() passed all tests
	  boolean passed = true;
	  for (int i = 0; i < kNumTrials; i++) {
	    double axis_angle[] = new double[3];
	    // Make an axis by choosing three random numbers in [-1, 1) and
	    // normalizing.
	    double norm = 0;
	    for (int j = 0; j < 3; j++) {
	      axis_angle[j] = RandDouble() * 2 - 1;
	      norm += axis_angle[j] * axis_angle[j];
	    }
	    norm = Math.sqrt(norm);

	    // Tiny theta.
	    double theta = 1e-16 * (Math.PI * 2 * RandDouble() - Math.PI);
	    for (int j = 0; j < 3; j++) {
	      axis_angle[j] = axis_angle[j] * theta / norm;
	    }

	    double matrix[] = new double[9];
	    double round_trip[] = new double[3];
	    ce2.AngleAxisToRotationMatrix(axis_angle, matrix);
	    if (!IsOrthonormal(matrix)) {
	    	System.err.println("In RotationAngleAxisToRotationMatrixAndBackNearZero() IsOrthonormal(matrix) = false");
	    	passed = false;
	    }
	    ce2.RotationMatrixToAngleAxis(matrix, round_trip);

	    for (int j = 0; j < 3; ++j) {
	      if (Math.abs(round_trip[j] - axis_angle[j]) > epsilon) {
	    	  System.err.println("In RotationAngleAxisToRotationMatrixAndBackNearZero() Math.abs(round_trip[j] - axis_angle[j]) > epsilon");
	    	  passed = false;
	      }
	    }
	  }
	  if (passed) {
		  System.out.println("RotationAngleAxisToRotationMatrixAndBackNearZero() passed all tests");
	  }
	}
	
	// Compare the 3x3 rotation matrices produced by the axis-angle
	// rotation 'aa' and the Euler angle rotation 'ea' (in radians).
	public void CompareEulerToAngleAxis(double aa[], double ea[], String testName, boolean passed[]) {
	  double aa_matrix[] = new double[9];
	  ce2.AngleAxisToRotationMatrix(aa, aa_matrix);
	  ce2.Transpose3x3(aa_matrix);  // Column to row major order.

	  double ea_matrix[] = new double[9];
	  ce2.ToDegrees(ea);  // Radians to degrees.
	  //const int kRowStride = 3;
	  //EulerAnglesToRotationMatrix(ea, kRowStride, ea_matrix);
	  ce2.EulerAnglesToRotationMatrix(ea, ea_matrix);

	  if (!IsOrthonormal(aa_matrix)) {
		  System.err.println("In " + testName + " IsOrthonormal(aa_matrix) = false");
		  passed[0] = false;
	  }
	  if (!IsOrthonormal(ea_matrix)) {
		  System.err.println("In " + testName + " IsOrthonormal(ea_matrix) = false");
		  passed[0] = false;
	  }
	  if (!IsNear3x3Matrix(ea_matrix, aa_matrix)) {
		  System.err.println("In " + testName + " IsNear3x3Matrix(ea_matrix, aa_matrix) = false");
		  passed[0] = false;
	  }
	}

	// Test with rotation axis along the x/y/z axes.
	// Also test zero rotation.
	public void EulerAnglesToRotationMatrixOnAxis() {
      // EulerAnglesToRotationMatrixOnAxis() passed all tests
	  boolean passed [] = new boolean[] {true};
	  String testName = "EulerAnglesToRotationMatrixOnAxis()";
	  int n_tests = 0;
	  for (double x = -1.0; x <= 1.0; x += 1.0) {
	    for (double y = -1.0; y <= 1.0; y += 1.0) {
	      for (double z = -1.0; z <= 1.0; z += 1.0) {
	    	int sum = 0;
	    	if (x != 0.0) {
	    		sum++;
	    	}
	    	if (y != 0.0) {
	    		sum++;
	    	}
	    	if (z != 0.0) {
	    		sum++;
	    	}
	        if (sum > 1)
	          continue;
	        double axis_angle[] = new double[] {x, y, z};
	        double euler_angles[] = new double[ ]{x, y, z};
	        CompareEulerToAngleAxis(axis_angle, euler_angles, testName, passed);
	        ++n_tests;
	      }
	    }
	  }
	  if (7 != n_tests) {
		  System.err.println("In EulerAnglesToRotationMatrixOnAxis() 7 != n_tests");
		  passed[0] = false;
	  }
	  if (passed[0]) {
		  System.out.println("EulerAnglesToRotationMatrixOnAxis() passed all tests");
	  }
	}
	
	// Test that a random rotation produces an orthonormal rotation
	// matrix.
	public void EulerAnglesToRotationMatrixIsOrthonormal() {
      // EulerAnglesToRotationMatrixIsOrthonormal() passed all tests
	  boolean passed = true;
	  for (int trial = 0; trial < kNumTrials; ++trial) {
	    double euler_angles_degrees[] = new double[3];
	    for (int i = 0; i < 3; ++i) {
	      euler_angles_degrees[i] = RandDouble() * 360.0 - 180.0;
	    }
	    double rotation_matrix[] = new double[9];
	    //EulerAnglesToRotationMatrix(euler_angles_degrees, 3, rotation_matrix);
	    ce2.EulerAnglesToRotationMatrix(euler_angles_degrees, rotation_matrix);
	    if (!IsOrthonormal(rotation_matrix)) {
	    	System.err.println("In EulerAnglesToRotationMatrixIsOrthonormal() IsOrthonormal(rotation_matrix) = false");
	    	passed = false;
	    }
	  }
	  if (passed) {
		  System.out.println("EulerAnglesToRotationMatrixIsOrthonormal() passed all tests");
	  }
	}

	public void QuaternionRotatePointGivesSameAnswerAsRotationByMatrixCanned() {
		  // QuaternionRotatePointGivesSameAnswerAsRotationByMatrixCanned() passed all tests
		  double kTolerance = 10.0 * epsilon;
		  int i;
		  boolean passed = true;
		  // Canned data generated in octave.
		  final double q[] = new double[] {
		    +0.1956830471754074,
		    -0.0150618562474847,
		    +0.7634572982788086,
		    -0.3019454777240753,
		  };
		  final double Q[] = new double[] {  // Scaled rotation matrix.
		     -0.6355194033477252,  0.0951730541682254,  0.3078870197911186,
		     -0.1411693904792992,  0.5297609702153905, -0.4551502574482019,
		    -0.2896955822708862, -0.4669396571547050, -0.4536309793389248
		  };
		  final double R[] = new double[]{  // With unit rows and columns.
		     -0.8918859164053080,  0.1335655625725649,  0.4320876677394745,
		    -0.1981166751680096,  0.7434648665444399, -0.6387564287225856,
		    -0.4065578619806013, -0.6553016349046693, -0.6366242786393164,
		  };

		  // Compute R from q and compare to known answer.
		  double Rq[] = new double[9];
		  ce2.QuaternionToScaledRotation(q, Rq);
		  for (i = 0; i < 9; i++) {
			  if (Math.abs(Q[i] - Rq[i]) > kTolerance) {
				  System.err.println("In QuaternionRotatePointGivesSameAnswerAsRotationByMatrixCanned() Math.abs(Q["+i+"] - Rq["+i+"]) > kTolerance");
				  passed = false;
			  }
		  }

		  // Now do the same but compute R with normalization.
		  ce2.QuaternionToRotation(q, Rq);
		  for (i = 0; i < 9; i++) {
			  if (Math.abs(R[i] - Rq[i]) > kTolerance) {
				  System.err.println("In QuaternionRotatePointGivesSameAnswerAsRotationByMatrixCanned() Math.abs(R["+i+"] - Rq["+i+"]) > kTolerance");
				  passed = false;
			  }
		  }
		  if (passed) {
			  System.out.println("QuaternionRotatePointGivesSameAnswerAsRotationByMatrixCanned() passed all tests");
		  }
		}
	
	public void QuaternionRotatePointGivesSameAnswerAsRotationByMatrix() {
		  // QuaternionRotatePointGivesSameAnswerAsRotationByMatrix() passed all tests
		  double kTolerance = 10.0 * epsilon;
		  boolean passed = true;
		  int i;
		  // Rotation defined by a unit quaternion.
		  final double q[] = new double[] {
		    0.2318160216097109,
		    -0.0178430356832060,
		    0.9044300776717159,
		    -0.3576998641394597,
		  };
		  final double p[] = new double[] {
		    +0.11,
		    -13.15,
		    1.17,
		  };

		  double R[] = new double[3 * 3];
		  ce2.QuaternionToRotation(q, R);

		  double result1[] = new double[3];
		  ce2.UnitQuaternionRotatePoint(q, p, result1);

		  double result2[] = new double[3];
		  for (i = 0; i < 3; i++) {
			  result2[i] = R[3*i]*p[0] + R[3*i + 1]*p[1] + R[3*i + 2]*p[2];
		  }
		  for (i = 0; i < 3; i++) {
			  if (Math.abs(result1[i] - result2[i]) > kTolerance) {
				  System.err.println("In QuaternionRotatePointGivesSameAnswerAsRotationByMatrix() Math.abs(result1["+i+"] - result2["+i+"]) > kTolerance");
				  passed = false;
			  }
		  }
		  if (passed) {
			  System.out.println("QuaternionRotatePointGivesSameAnswerAsRotationByMatrix() passed all tests");
		  }
		}

	// Verify that (a * b) * c == a * (b * c).
	public void QuaternionMultiplicationIsAssociative() {
      // QuaternionMultiplicationIsAssociative() passed all tests
	  double kTolerance = 10.0 * epsilon;
	  boolean passed = true;
	  int i;
	  double a[] = new double[4];
	  double b[] = new double[4];
	  double c[] = new double[4];
	  for (i = 0; i < 4; ++i) {
	    a[i] = 2 * RandDouble() - 1;
	    b[i] = 2 * RandDouble() - 1;
	    c[i] = 2 * RandDouble() - 1;
	  }

	  double ab[] = new double[4];
	  double ab_c[] = new double[4];
	  QuaternionProduct(a, b, ab);
	  QuaternionProduct(ab, c, ab_c);

	  double bc[] = new double[4];
	  double a_bc[] = new double[4];
	  QuaternionProduct(b, c, bc);
	  QuaternionProduct(a, bc, a_bc);

	  for (i = 0; i < 4; i++) {
		  if (Math.abs(ab_c[i] - a_bc[i]) > kTolerance) {
			  System.err.println("In QuaternionMultiplicationIsAssociative() Math.abs(ab_c["+i+"] - a_bc["+i+"]) > kTolerance");
			  passed = false;
		  }
	  }
	  if (passed) {
		  System.out.println("QuaternionMultiplicationIsAssociative() passed all tests");
	  }
	}
	
	public void AngleAxisRotatePointGivesSameAnswerAsRotationMatrix() {
		  // AngleAxisRotatePointGivesSameAnswerAsRotationMatrix() passed all tests
		  boolean passed = true;
		  double kTolerance = 10.0 * epsilon;
		  double angle_axis[] = new double[3];
		  double R[] = new double[9];
		  double p[] = new double[3];
		  double angle_axis_rotated_p[] = new double[3];
		  double rotation_matrix_rotated_p[] = new double[3];

		  for (int i = 0; i < 10000; ++i) {
		    double theta = (2.0 * i * 0.0011 - 1.0) * Math.PI;
		    for (int j = 0; j < 50; ++j) {
		      double norm2 = 0.0;
		      for (int k = 0; k < 3; ++k) {
		        angle_axis[k] = 2.0 * RandDouble() - 1.0;
		        p[k] = 2.0 * RandDouble() - 1.0;
		        norm2 = angle_axis[k] * angle_axis[k];
		      }

		      final double inv_norm = theta / Math.sqrt(norm2);
		      for (int k = 0; k < 3; ++k) {
		        angle_axis[k] *= inv_norm;
		      }

		      ce2.AngleAxisToRotationMatrix(angle_axis, R);
		      rotation_matrix_rotated_p[0] = R[0] * p[0] + R[3] * p[1] + R[6] * p[2];
		      rotation_matrix_rotated_p[1] = R[1] * p[0] + R[4] * p[1] + R[7] * p[2];
		      rotation_matrix_rotated_p[2] = R[2] * p[0] + R[5] * p[1] + R[8] * p[2];

		      ce2.AngleAxisRotatePoint(angle_axis, p, angle_axis_rotated_p);
		      for (int k = 0; k < 3; ++k) {
		    	  if (Math.abs(rotation_matrix_rotated_p[k] - angle_axis_rotated_p[k]) > kTolerance) {
		    		  System.err.println("In AngleAxisRotatePointGivesSameAnswerAsRotationMatrix() Math.abs(rotation_matrix_rotated_p["+k+"] - angle_axis_rotated_p["+k+"]) > kTolerance");
		    		  passed = false;
		    		  System.err.println("p: " + p[0] + " " + p[1] + " " + p[2]);
		    		  System.err.println("angle_axis: " + angle_axis[0] + " " + angle_axis[1] + " " + angle_axis[2]);
		    	  }
		      }
		    }
		  }
		  if (passed) {
			  System.out.println("AngleAxisRotatePointGivesSameAnswerAsRotationMatrix() passed all tests");
		  }
		}

	public void AngleAxisNearZeroRotatePointGivesSameAnswerAsRotationMatrix() {
		  // AngleAxisNearZeroRotatePointGivesSameAnswerAsRotationMatrix() passed all tests
		  boolean passed = true;
		  double kTolerance = 10.0 * epsilon;
		  double angle_axis[] = new double[3];
		  double R[] = new double[9];
		  double p[] = new double[3];
		  double angle_axis_rotated_p[] = new double[3];
		  double rotation_matrix_rotated_p[] = new double[3];

		  for (int i = 0; i < 10000; ++i) {
		    double norm2 = 0.0;
		    for (int k = 0; k < 3; ++k) {
		      angle_axis[k] = 2.0 * RandDouble() - 1.0;
		      p[k] = 2.0 * RandDouble() - 1.0;
		      norm2 = angle_axis[k] * angle_axis[k];
		    }

		    double theta = (2.0 * i * 0.0001  - 1.0) * 1e-16;
		    final double inv_norm = theta / Math.sqrt(norm2);
		    for (int k = 0; k < 3; ++k) {
		      angle_axis[k] *= inv_norm;
		    }

		    ce2.AngleAxisToRotationMatrix(angle_axis, R);
		    rotation_matrix_rotated_p[0] = R[0] * p[0] + R[3] * p[1] + R[6] * p[2];
		    rotation_matrix_rotated_p[1] = R[1] * p[0] + R[4] * p[1] + R[7] * p[2];
		    rotation_matrix_rotated_p[2] = R[2] * p[0] + R[5] * p[1] + R[8] * p[2];

		    ce2.AngleAxisRotatePoint(angle_axis, p, angle_axis_rotated_p);
		    for (int k = 0; k < 3; ++k) {
		    	  if (Math.abs(rotation_matrix_rotated_p[k] - angle_axis_rotated_p[k]) > kTolerance) {
		    		  System.err.println("In AngleAxisNearZeroRotatePointGivesSameAnswerAsRotationMatrix() Math.abs(rotation_matrix_rotated_p["+k+"] - angle_axis_rotated_p["+k+"]) > kTolerance");
		    		  passed = false;
		    		  System.err.println("p: " + p[0] + " " + p[1] + " " + p[2]);
		    		  System.err.println("angle_axis: " + angle_axis[0] + " " + angle_axis[1] + " " + angle_axis[2]);
		    	  }
		      }
		  }
		  if (passed) {
			  System.out.println("AngleAxisNearZeroRotatePointGivesSameAnswerAsRotationMatrix() passed all tests");
		  }
		}
	
	public void RotationMatrixToAngleAxisNearPiExampleOneFromTobiasStrauss() {
		  // RotationMatrixToAngleAxisNearPiExampleOneFromTobiasStrauss() passed all tests
		  // Example from Tobias Strauss
		  final double rotation_matrix[] = new double[] {
		    -0.999807135425239,    -0.0128154391194470,   -0.0148814136745799,
		    -0.0128154391194470,   -0.148441438622958,     0.988838158557669,
		    -0.0148814136745799,    0.988838158557669,     0.148248574048196
		  };

		  double angle_axis[] = new double[3];
		  //RotationMatrixToAngleAxis(RowMajorAdapter3x3(rotation_matrix), angle_axis);
		  ce2.RotationMatrixToAngleAxis(rotation_matrix, angle_axis);
		  double round_trip[] = new double[9];
		  //AngleAxisToRotationMatrix(angle_axis, RowMajorAdapter3x3(round_trip));
		  ce2.AngleAxisToRotationMatrix(angle_axis, round_trip);
		  if (!IsNear3x3Matrix(rotation_matrix, round_trip)) {
			  System.err.println("In RotationMatrixToAngleAxisNearPiExampleOneFromTobiasStrauss() IsNear3x3Matrix(rotation_matrix, round_trip) = false");
		  }
		  else {
			  System.out.println("RotationMatrixToAngleAxisNearPiExampleOneFromTobiasStrauss() passed all tests");
		  }
		}

	public void CheckRotationMatrixToAngleAxisRoundTrip(double theta,
            double phi,
            double angle, String testName, boolean passed[]) {
			double angle_axis[] = new double[3];
			angle_axis[0] = angle * Math.sin(phi) * Math.cos(theta);
			angle_axis[1] = angle * Math.sin(phi) * Math.sin(theta);
			angle_axis[2] = angle * Math.cos(phi);
			
			double rotation_matrix[] = new double[9];
			ce2.AngleAxisToRotationMatrix(angle_axis, rotation_matrix);
			
			double angle_axis_round_trip[] = new double[3];
			ce2.RotationMatrixToAngleAxis(rotation_matrix, angle_axis_round_trip);
			if (!IsNearAngleAxis(angle_axis_round_trip, angle_axis)) {
				System.err.println("In " + testName + " IsNearAngleAxis(angle_axis_round_trip, angle_axis) = false");
				passed[0] = false;
			}
	}
	
	public void RotationMatrixToAngleAxisExhaustiveRoundTrip() {
		  // RotationMatrixToAngleAxisExhaustiveRoundTrip() passed all tests
		  boolean passed[] = new boolean[] {true};
		  String testName = "RotationMatrixToAngleAxisExhaustiveRoundTrip()";
		  final double kMaxSmallAngle = 1e-8;
		  final int kNumSteps = 1000;
		  for (int i = 0; i < kNumSteps; ++i) {
		    final double theta = (double)(i) / kNumSteps * 2.0 * Math.PI;
		    for (int j = 0; j < kNumSteps; ++j) {
		      final double phi = (double)(j) / kNumSteps * Math.PI;
		      // Rotations of angle Pi.
		      CheckRotationMatrixToAngleAxisRoundTrip(theta, phi, Math.PI, testName, passed);
		      // Rotation of angle approximately Pi.
		      CheckRotationMatrixToAngleAxisRoundTrip(
		          theta, phi, Math.PI - kMaxSmallAngle * RandDouble(), testName, passed);
		      // Rotations of angle approximately zero.
		      CheckRotationMatrixToAngleAxisRoundTrip(
		          theta, phi, kMaxSmallAngle * 2.0 * RandDouble() - 1.0, testName, passed);
		    }
		  }
		  if (passed[0]) {
			  System.out.println("RotationMatrixToAngleAxisExhaustiveRoundTrip() passed all tests");
		  }
		}

	public void VisibilityTestSimpleMatrix() {
		  // VisibilityTestSimpleMatrix() passed all tests
		  int i,j;
		  boolean passed = true;
		  //   A = [1 0 0 0 0 1
		  //        1 0 0 1 0 0
		  //        0 1 1 0 0 0
		  //        0 1 0 0 1 0]

		  int num_cols = 6;
		  int num_eliminate_blocks = 2;
		  CompressedRowBlockStructure bs = new CompressedRowBlockStructure();

		  // Row 1
		  {
		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 2;
		    row.block.position = 0;
		    row.cells.add(new Cell(0, 0));
		    row.cells.add(new Cell(5, 0));
		  }

		  // Row 2
		  {
		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 2;
		    row.block.position = 2;
		    row.cells.add(new Cell(0, 1));
		    row.cells.add(new Cell(3, 1));
		  }

		  // Row 3
		  {
		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 2;
		    row.block.position = 4;
		    row.cells.add(new Cell(1, 2));
		    row.cells.add(new Cell(2, 2));
		  }

		  // Row 4
		  {
		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 2;
		    row.block.position = 6;
		    row.cells.add(new Cell(1, 3));
		    row.cells.add(new Cell(4, 3));
		  }
		  for (i = 0; i < num_cols; i++) {
			  bs.cols.add(new Block());
		  }

		  Vector< HashSet<Integer> > visibility = new Vector<HashSet<Integer>>();
		  ce2.ComputeVisibility(bs, num_eliminate_blocks, visibility);
		  if (visibility.size() != num_cols - num_eliminate_blocks) {
			  System.err.println("In VisibilityTestSimpleMatrix() visibility.size() != num_cols - num_eliminate_blocks");
			  passed = false;
		  }
		  for (i = 0; i < visibility.size(); ++i) {
		     if (visibility.get(i).size() != 1) {
		    	 System.err.println("In VisibilityTestSimpleMatrix() visibility.get("+i+").size() != 1");
				 passed = false;	 
		     }
		  }

		  WeightedGraph<Integer> graph = ce2.CreateSchurComplementGraph(visibility);
		  if (graph.vertices().size() != visibility.size()) {
			  System.err.println("In VisibilityTestSimpleMatrix() graph.vertices().size() != visibility.size()");
			  passed = false;	
		  }
		  for (i = 0; i < visibility.size(); ++i) {
		    if (graph.VertexWeight(i) != 1.0) {
		    	System.err.println("In VisibilityTestSimpleMatrix() graph.VertexWeight("+i+") != 1.0");
				passed = false;		
		    }
		  }

		  for (i = 0; i < visibility.size(); ++i) {
		    for (j = i; j < visibility.size(); ++j) {
		      double edge_weight = 0.0;
		      if ((i == 1 && j == 3) || (i == 0 && j == 2) || (i == j)) {
		        edge_weight = 1.0;
		      }

		      if (graph.EdgeWeight(i, j) != edge_weight) {
		    	  System.err.println("In VisibilityTestSimpleMatrix() graph.EdgeWeight("+i+", "+j+") != edge_weight");
				  passed = false;	
				  System.err.println("weight: " + graph.EdgeWeight(i,j));
				  System.err.println("Expected weight: " + edge_weight);
		      }
		    }
		  }
		  if (passed) {
			  System.out.println("VisibilityTestSimpleMatrix() passed all tests");
		  }
		}
	
	public void VisibilityTestNoEBlocks() {
		  // VisibilityTestNoEBlocks() passed all tests
		  //   A = [1 0 0 0 0 0
		  //        1 0 0 0 0 0
		  //        0 1 0 0 0 0
		  //        0 1 0 0 0 0]

		  int i,j;
		  boolean passed = true;  
		  int num_cols = 6;
		  int num_eliminate_blocks = 2;
		  CompressedRowBlockStructure bs = new CompressedRowBlockStructure();

		  // Row 1
		  {
		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 2;
		    row.block.position = 0;
		    row.cells.add(new Cell(0, 0));
		  }

		  // Row 2
		  {
		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 2;
		    row.block.position = 2;
		    row.cells.add(new Cell(0, 1));
		  }

		  // Row 3
		  {
		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 2;
		    row.block.position = 4;
		    row.cells.add(new Cell(1, 2));
		  }

		  // Row 4
		  {
		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 2;
		    row.block.position = 6;
		    row.cells.add(new Cell(1, 3));
		  }
		  for (i = 0; i < num_cols; i++) {
			  bs.cols.add(new Block());
		  }

		  Vector< HashSet<Integer> > visibility = new Vector<HashSet<Integer>>();
		  ce2.ComputeVisibility(bs, num_eliminate_blocks, visibility);
		  if (visibility.size() != num_cols - num_eliminate_blocks) {
			  System.err.println("In VisibilityTestNoEBlocks() visibility.size() != num_cols - num_eliminate_blocks");
			  passed = false;
		  }
		  for (i = 0; i < visibility.size(); ++i) {
			     if (visibility.get(i).size() != 0) {
			    	 System.err.println("In VisibilityTestNoEBlocks() visibility.get("+i+").size() != 0");
					 passed = false;	 
			     }
		  }
		         
		  WeightedGraph<Integer> graph = ce2.CreateSchurComplementGraph(visibility);
		  if (graph.vertices().size() != visibility.size()) {
			  System.err.println("In VisibilityTestNoEBlocks() graph.vertices().size() != visibility.size()");
			  passed = false;	
		  }
		  for (i = 0; i < visibility.size(); ++i) {
		    if (graph.VertexWeight(i) != 1.0) {
		    	System.err.println("In VisibilityTestNoEBlocks() graph.VertexWeight("+i+") != 1.0");
				passed = false;		
		    }
		  }
		  

		  for (i = 0; i < visibility.size(); ++i) {
		    for (j = i; j < visibility.size(); ++j) {
		      double edge_weight = 0.0;
		      if (i == j) {
		        edge_weight = 1.0;
		      }
		      
		      if (graph.EdgeWeight(i, j) != edge_weight) {
		    	  System.err.println("In VisibilityTestNoEBlocks() graph.EdgeWeight("+i+", "+j+") != edge_weight");
				  passed = false;	
				  System.err.println("weight: " + graph.EdgeWeight(i,j));
				  System.err.println("Expected weight: " + edge_weight);
		      }
		      
		    }
		  }
		  if (passed) {
			  System.out.println("VisibilityTestNoEBlocks() passed all tests");
		  }
		}

	
	
	public void GradientCheckingCostFunctionResidualsAndJacobiansArePreservedTest() {
		  // GradientCheckingCostFunctionResidualsAndJacobiansArePreservedTest() passed all tests
		  boolean passed = true;

		  // Test with 3 blocks of size 2, 3 and 4.
		  final int arity = 3;
		  final int dim[] = new int[]{ 2, 3, 4 };

		  // Make a random set of blocks.
		  Vector<double[]> parameters = new Vector<double[]>(arity);
		  for (int j = 0; j < arity; ++j) {
		    parameters.add(new double[dim[j]]);
		    for (int u = 0; u < dim[j]; ++u) {
		      parameters.get(j)[u] = 2.0 * RandDouble() - 1.0;
		    }
		  }

		  double original_residual[] = new double[1];
		  double residual[] = new double[1];
		  double original_jacobians[][] = new double[arity][];
		  double jacobians[][] = new double[arity][];

		  for (int j = 0; j < arity; ++j) {
		    // Since residual is one dimensional the jacobians have the same
		    // size as the parameter blocks.
		    jacobians[j] = new double[dim[j]];
		    original_jacobians[j] = new double[dim[j]];
		  }

		  final double kRelativeStepSize = 1e-6;
		  final double kRelativePrecision = 1e-4;

		  TestTerm term = new TestTerm(-1, -1, arity, dim);
		  GradientCheckingIterationCallback callback = new GradientCheckingIterationCallback();
		  CostFunction gradient_checking_cost_function = 
		      CreateGradientCheckingCostFunction(term, null,
		                                         kRelativeStepSize,
		                                         kRelativePrecision,
		                                         "Ignored.", callback);
		  
		  term.Evaluate(parameters,
		                original_residual,
		                original_jacobians);

		  
		  gradient_checking_cost_function.Evaluate(parameters,
		                                            residual,
		                                            jacobians);
		  if (original_residual[0] != residual[0]) {
			  System.err.println("In GradientCheckingCostFunctionResidualsAndJacobiansArePreservedTest() original_residual[0] != residual[0]");
			  passed = false;
		  }

		  for (int j = 0; j < arity; j++) {
		    for (int k = 0; k < dim[j]; ++k) {
		      if (original_jacobians[j][k] != jacobians[j][k]) {
		    	  System.err.println("In GradientCheckingCostFunctionResidualsAndJacobiansArePreservedTest() original_jacobians["+j+"]["+k+"] != jacobians["+j+"]["+k+"]");
				  passed = false; 
				  System.err.println("original_jacobians["+j+"]["+k+"] = " + original_jacobians[j][k]);
				  System.err.println("jacobians["+j+"]["+k+"] = " + jacobians[j][k]);
		      }
		    }

		    parameters.set(j, null);
		    jacobians[j] = null;
		    original_jacobians[j] = null;
		  }
		  if (passed) {
			  System.out.println("GradientCheckingCostFunctionResidualsAndJacobiansArePreservedTest() passed all tests");
		  }
		}

	public void GradientCheckingCostFunctionSmokeTest() {
		  // GradientCheckingCostFunctionSmokeTest() passed all tests
		  boolean passed = true;
		  testCase = TEST_TERM_EXAMPLE;

		  // Test with 3 blocks of size 2, 3 and 4.
		  final int arity = 3;
		  final int dim[] = new int[]{ 2, 3, 4 };

		  // Make a random set of blocks.
		  Vector<double[]> parameters = new Vector<double[]>(arity);
		  for (int j = 0; j < arity; ++j) {
		    parameters.add(new double[dim[j]]);
		    for (int u = 0; u < dim[j]; ++u) {
		      parameters.get(j)[u] = 2.0 * RandDouble() - 1.0;
		    }
		  }

		  double residual[] = new double[1];
		  double jacobians[][] = new double[arity][];
		  for (int j = 0; j < arity; ++j) {
		    // Since residual is one dimensional the jacobians have the same size as the
		    // parameter blocks.
		    jacobians[j] = new double[dim[j]];
		  }

		  final double kRelativeStepSize = 1e-6;
		  final double kRelativePrecision = 1e-4;

		  // Should have one term that's bad, causing everything to get dumped.
		  if (INFO <= MAX_LOG_LEVEL) {
			  Preferences.debug("Bad gradient\n", Preferences.DEBUG_ALGORITHM);
		  }
		  
		  {
		    TestTerm term = new TestTerm(1, 2, arity, dim);
		    GradientCheckingIterationCallback callback = new GradientCheckingIterationCallback();
		    CostFunction gradient_checking_cost_function =
		        CreateGradientCheckingCostFunction(term, null,
		                                           kRelativeStepSize,
		                                           kRelativePrecision,
		                                           "Fuzzy banana", callback);
		        if (!gradient_checking_cost_function.Evaluate(parameters, residual, jacobians)) {
		        	System.err.println("In GradientCheckingCostFunctionSmokeTest() gradient_checking_cost_function.Evaluate(parameters, residual, jacobians) = false");
		        	passed = false;
		        }
		    if (!callback.gradient_error_detected()) {
		    	System.err.println("In GradientCheckingCostFunctionSmokeTest() callback.gradient_error_detected() = false");
		    	passed = false;
		    }
		    //System.err.println(callback.error_log()[0]);
		    int index = callback.error_log()[0].indexOf("Fuzzy banana");
		    if (index == -1) {
		    	System.err.println("In GradientCheckingCostFunctionSmokeTest() call.error_log()[0] does not contain Fuzzy banana");
		    	passed = false;	
		    }
		    index = callback.error_log()[0].indexOf("(1,0,2) Relative error worse than");
		    if (index == -1) {
		    	System.err.println("In GradientCheckingCostFunctionSmokeTest() call.error_log()[0] does not contain (1,0,2) Relative error worse than");
		    	passed = false;	
		    }         
		  }

		  // The gradient is correct, so no errors are reported.
		  if (INFO <= MAX_LOG_LEVEL) {
			  Preferences.debug("Good gradient\n", Preferences.DEBUG_ALGORITHM);
		  }
		  {
		    TestTerm term = new TestTerm(-1, -1, arity, dim);
		    GradientCheckingIterationCallback callback = new GradientCheckingIterationCallback();
		    CostFunction gradient_checking_cost_function =
		        CreateGradientCheckingCostFunction(term, null,
		                                           kRelativeStepSize,
		                                           kRelativePrecision,
		                                           "Fuzzy banana", callback);
		    
		    if (!gradient_checking_cost_function.Evaluate(parameters, residual, jacobians)) {
	        	System.err.println("In GradientCheckingCostFunctionSmokeTest() gradient_checking_cost_function.Evaluate(parameters, residual, jacobians) = false");
	        	passed = false;
	        }
		    if (callback.gradient_error_detected()) {
		    	System.err.println("In GradientCheckingCostFunctionSmokeTest() callback.gradient_error_detected() = true");
		    	passed = false;
		    }
		  }

		  for (int j = 0; j < arity; j++) {
		    parameters.set(j, null);
		    jacobians[j] = null;
		  }
		  if (passed) {
			  System.out.println("GradientCheckingCostFunctionSmokeTest() passed all tests");
		  }
		}
	// The following three classes are for the purposes of defining
	// function signatures. They have dummy Evaluate functions.

	// Trivial cost function that accepts a single argument.
	class UnaryCostFunction4 extends CostFunction {
	 
	  public UnaryCostFunction4(int num_residuals, int parameter_block_size) {
		super();
	    set_num_residuals(num_residuals);
	    mutable_parameter_block_sizes().add(parameter_block_size);
	  }

	  public boolean Evaluate(Vector<double[]> parameters,
	                        double[] residuals,
	                        double[][] jacobians) {
	    for (int i = 0; i < num_residuals(); ++i) {
	      residuals[i] = 1;
	    }
	    return true;
	  }
	};

	// Trivial cost function that accepts two arguments.
	class BinaryCostFunction4 extends CostFunction {
	
	  public BinaryCostFunction4(int num_residuals,
	                     int parameter_block1_size,
	                     int parameter_block2_size) {
		super();
	    set_num_residuals(num_residuals);
	    mutable_parameter_block_sizes().add(parameter_block1_size);
	    mutable_parameter_block_sizes().add(parameter_block2_size);
	  }

	  public boolean Evaluate(Vector<double[]> parameters,
	                        double[] residuals,
	                        double[][] jacobians) {
	    for (int i = 0; i < num_residuals(); ++i) {
	      residuals[i] = 2;
	    }
	    return true;
	  }
	};
	
	// Trivial cost function that accepts three arguments.
	class TernaryCostFunction4 extends CostFunction {
	
	  public TernaryCostFunction4(int num_residuals,
	                      int parameter_block1_size,
	                      int parameter_block2_size,
	                      int parameter_block3_size) {
		super();
	    set_num_residuals(num_residuals);
	    mutable_parameter_block_sizes().add(parameter_block1_size);
	    mutable_parameter_block_sizes().add(parameter_block2_size);
	    mutable_parameter_block_sizes().add(parameter_block3_size);
	  }

	  public boolean Evaluate(Vector<double[]> parameters,
	                        double[] residuals,
	                        double[][] jacobians) {
	    for (int i = 0; i < num_residuals(); ++i) {
	      residuals[i] = 3;
	    }
	    return true;
	  }
	};

	// Verify that the two ParameterBlocks are formed from the same user
	// array and have the same LocalParameterization object.
	public void ParameterBlocksAreEquivalent(ParameterBlock  left,
	                                  ParameterBlock right, String testName, boolean passed[]) {
	  if (left == null) {
		  System.err.println("In " + testName + " left == null");
		  passed[0] = false;
	  }
	  if (right == null) {
		  System.err.println("In " + testName + " right == null");
		  passed[0] = false;	  
	  }
	  if (left.user_state() != right.user_state()) {
		  System.err.println("In " + testName + " left.user_state() != right.user_state()");
		  passed[0] = false;
	  }
	  if (left.Size() != right.Size()) {
		  System.err.println("In " + testName + " left.Size() != right.Size()");
		  passed[0] = false;
	  }
	  if (left.LocalSize() != right.LocalSize()) {
		  System.err.println("In " + testName + " left.LocalSize() != right.LocalSize()");
		  passed[0] = false;
	  }
	  if (left.local_parameterization() != right.local_parameterization()) {
		  System.err.println("In " + testName + " left.local_parameterization() != right.local_parameterization()");
		  passed[0] = false;
	  }
	  if (left.IsConstant() != right.IsConstant()) {
		  System.err.println("In " + testName + " left.IsConstant() != right.IsConstant()");
		  passed[0] = false;
	  }
	}
	
	public void GradientCheckingProblemImplProblemDimensionsMatch() {
		  // GradientCheckingProblemImplProblemDimensionsMatch() passed all tests
		  boolean passed[] = new boolean[] {true};
		  String testName = "GradientCheckingProblemImplProblemDimensionsMatch()";
		  // Parameter blocks with arbitrarily chosen initial values.
		  double x[] = new double[]{1.0, 2.0, 3.0};
		  double y[] = new double[]{4.0, 5.0, 6.0, 7.0};
		  double z[] = new double[]{8.0, 9.0, 10.0, 11.0, 12.0};
		  double w[] = new double[]{13.0, 14.0, 15.0, 16.0};

		  ProblemImpl problem_impl = new ProblemImpl();
		  problem_impl.AddParameterBlock(x, 3);
		  problem_impl.AddParameterBlock(y, 4);
		  problem_impl.SetParameterBlockConstant(y);
		  problem_impl.AddParameterBlock(z, 5);
		  problem_impl.AddParameterBlock(w, 4, new QuaternionParameterization());
		  problem_impl.AddResidualBlock(new UnaryCostFunction4(2, 3), null, x);
		  problem_impl.AddResidualBlock(new BinaryCostFunction4(6, 5, 4) ,
		                                null, z, y);
		  problem_impl.AddResidualBlock(new BinaryCostFunction4(3, 3, 5),
		                                new TrivialLoss(), x, z);
		  problem_impl.AddResidualBlock(new BinaryCostFunction4(7, 5, 3),
		                                null, z, x);
		  problem_impl.AddResidualBlock(new TernaryCostFunction4(1, 5, 3, 4),
		                                null, z, x, y);

		  GradientCheckingIterationCallback callback = new GradientCheckingIterationCallback();
		  ProblemImpl gradient_checking_problem_impl =
		      CreateGradientCheckingProblemImpl(problem_impl, 1.0, 1.0, callback);

		  // The dimensions of the two problems match.
		  if (problem_impl.NumParameterBlocks() != gradient_checking_problem_impl.NumParameterBlocks()) {
			  System.err.println("In " + testName + " problem_impl.NumParameterBlocks() != gradient_checking_problem_impl.NumParameterBlocks()");
			  passed[0] = false;  
		  }
		  if (problem_impl.NumResidualBlocks() != gradient_checking_problem_impl.NumResidualBlocks()) {
			  System.err.println("In " + testName + " problem_impl.NumResidualBlocks() != gradient_checking_problem_impl.NumResidualBlocks()");
			  passed[0] = false;  
		  }

		  if (problem_impl.NumParameters() != gradient_checking_problem_impl.NumParameters()) {
			  System.err.println("In " + testName + " problem_impl.NumParameters() != gradient_checking_problem_impl.NumParameters()");
			  passed[0] = false; 
		  }
		  if (problem_impl.NumResiduals() != gradient_checking_problem_impl.NumResiduals()) {
			  System.err.println("In " + testName + " problem_impl.NumResiduals() != gradient_checking_problem_impl.NumResiduals()");
			  passed[0] = false; 
		  }

		  final Program program = problem_impl.program();
		  final Program gradient_checking_program =
		      gradient_checking_problem_impl.program();

		  // Since we added the ParameterBlocks and ResidualBlocks explicitly,
		  // they should be in the same order in the two programs. It is
		  // possible that may change due to implementation changes to
		  // Program. This is not expected to be the case and writing code to
		  // anticipate that possibility not worth the extra complexity in
		  // this test.
		  for (int i = 0; i < program.parameter_blocks().size(); ++i) {
		    ParameterBlocksAreEquivalent(
		        program.parameter_blocks().get(i),
		        gradient_checking_program.parameter_blocks().get(i), testName, passed);
		  }

		  for (int i = 0; i < program.residual_blocks().size(); ++i) {
		    // Compare the sizes of the two ResidualBlocks.
		    final ResidualBlock original_residual_block =
		        program.residual_blocks().get(i);
		    final ResidualBlock new_residual_block =
		        gradient_checking_program.residual_blocks().get(i);
		    if (original_residual_block.NumParameterBlocks() != new_residual_block.NumParameterBlocks()) {
		    	 System.err.println("In " + testName + " original_residual_block.NumParameterBlocks() != new_residual_block.NumParameterBlocks()");
				 passed[0] = false; 
		    }
		    if (original_residual_block.NumResiduals() != new_residual_block.NumResiduals()) {
		    	System.err.println("In " + testName + " original_residual_block.NumResiduals() != new_residual_block.NumResiduals()");
				passed[0] = false; 
		    }
		    if (original_residual_block.NumScratchDoublesForEvaluate() != new_residual_block.NumScratchDoublesForEvaluate()) {
		    	System.err.println("In " + testName + " original_residual_block.NumScratchDoublesForEvaluate() != new_residual_block.NumScratchDoublesForEvaluate()");
				passed[0] = false; 
		    }

		    // Verify that the ParameterBlocks for the two residuals are equivalent.
		    for (int j = 0; j < original_residual_block.NumParameterBlocks(); ++j) {
		      ParameterBlocksAreEquivalent(
		          original_residual_block.parameter_blocks()[j],
		          new_residual_block.parameter_blocks()[j], testName, passed);
		    }
		  }
		  if (passed[0]) {
			  System.out.println(testName + " passed all tests");
		  }
		}

		public void CheckDimensions(ProbeResults results,
                Vector<Integer> parameter_sizes,
                Vector<Integer> local_parameter_sizes,
                int residual_size, String testName, boolean passed[]) {
			if (parameter_sizes.size() != local_parameter_sizes.size()) {
				System.err.println("In " + testName + " parameter_sizes.size() != local_parameter_sizes.size()");
				passed[0] = false;
			}
			int num_parameters = parameter_sizes.size();
			if (residual_size != results.residuals.size()) {
				System.err.println("In " + testName + " residual_size != results.residuals.size()");
				passed[0] = false;
			}
			if (num_parameters != results.local_jacobians.size()) {
				System.err.println("In " + testName + " num_parameters != results.local_jacobians.size()");
				passed[0] = false;
			}
			if (num_parameters != results.local_numeric_jacobians.size()) {
				System.err.println("In " + testName + " num_parameters != results.local_numeric_jacobians.size()");
				passed[0] = false;
			}
			if (num_parameters != results.jacobians.size()) {
				System.err.println("In " + testName + " num_parameters != results.jacobians.size()");
				passed[0] = false;
			}
			if (num_parameters != results.numeric_jacobians.size()) {
				System.err.println("In " + testName + " num_parameters != results.numeric_jacobians.size()");
				passed[0] = false;
			}
			for (int i = 0; i < num_parameters; ++i) {
			    if (residual_size != results.local_jacobians.get(i).getRowDimension()) {
			    	System.err.println("In " + testName + " residual_size != results.local_jacobians.get("+i+").getRowDimension()");
					passed[0] = false;
			    }
			    if (local_parameter_sizes.get(i) != results.local_jacobians.get(i).getColumnDimension()) {
			    	System.err.println("In " + testName + " local_parameter_sizes.get("+i+") != results.local_jacobians.get("+i+").getColumnDimension()");
					passed[0] = false;
			    }
			    if (residual_size != results.local_numeric_jacobians.get(i).getRowDimension()) {
			    	System.err.println("In " + testName + " residual_size != results.local_numeric_jacobians.get("+i+").getRowDimension()");
					passed[0] = false;
			    }
			    if (local_parameter_sizes.get(i) != results.local_numeric_jacobians.get(i).getColumnDimension()) {
			    	System.err.println("In " + testName + " local_parameter_sizes.get("+i+") != results.local_numeric_jacobians.get("+i+").getColumnDimension()");
					passed[0] = false;
			    }
			    if (residual_size != results.jacobians.get(i).getRowDimension()) {
			    	System.err.println("In " + testName + " residual_size != results.jacobians.get("+i+").getRowDimension()");
					passed[0] = false;
			    }
			    if (parameter_sizes.get(i) != results.jacobians.get(i).getColumnDimension()) {
			    	System.err.println("In " + testName + " parameter_sizes.get("+i+") != results.jacobians.get("+i+").getColumnDimension()");
					passed[0] = false;
			    }
			    if (residual_size != results.numeric_jacobians.get(i).getRowDimension()) {
			    	System.err.println("In " + testName + " residual_size != results.numeric_jacobians.get("+i+").getRowDimension()");
					passed[0] = false;
			    }
			    if (parameter_sizes.get(i) != results.numeric_jacobians.get(i).getColumnDimension()) {
			    	System.err.println("In " + testName + " parameter_sizes.get("+i+") != results.numeric_jacobians.get("+i+").getColumnDimension()");
					passed[0] = false;
			    }
			}
		}
		
		public void GradientCheckerSmokeTest() {
			  // GradientCheckerSmokeTest() passed all tests
			  int r,c;
			  boolean passed[] = new boolean[] {true};
			  String testName = "GradientCheckerSmokeTest()";
			  final double kTolerance = 1.0E-6;

			  // Test with 3 blocks of size 2, 3 and 4.
			  final int num_parameters = 3;
			  Vector<Integer> parameter_sizes = new Vector<Integer>(3);
			  parameter_sizes.add(2);
			  parameter_sizes.add(3);
			  parameter_sizes.add(4);
			  int parameter_sizes_data[] = new int[] {2,3,4};

			  // Make a random set of blocks.
			  Vector<double[]> parameters = new Vector<double[]>(num_parameters);
			  for (int j = 0; j < num_parameters; ++j) {
			    parameters.add(new double[parameter_sizes.get(j)]);
			    for (int u = 0; u < parameter_sizes.get(j); ++u) {
			      parameters.get(j)[u] = 2.0 * RandDouble() - 1.0;
			    }
			  }

			  NumericDiffOptions numeric_diff_options = new NumericDiffOptions();
			  ProbeResults results = new ProbeResults();

			  // Test that Probe returns true for correct Jacobians.
			  testCase = GOOD_TEST_TERM_EXAMPLE;
			  GoodTestTerm good_term = new GoodTestTerm(num_parameters, parameter_sizes_data);
			  GradientChecker good_gradient_checker = new GradientChecker(good_term, null, numeric_diff_options);
			  if (!good_gradient_checker.Probe(parameters, kTolerance, null)) {
				  System.err.println("In " + testName + " good_gradient_checker.Probe(parameters, kTolerance, null) = false");
				  passed[0] = false;  
			  }
			  if (!good_gradient_checker.Probe(parameters, kTolerance, results)) {
				  System.err.println("In " + testName + " good_gradient_checker.Probe(parameters, kTolerance, results) = false");
				  passed[0] = false;  
			  }

			  // Check that results contain sensible data.
			  if (!results.return_value) {
				  System.err.println("In " + testName + " results.return_value = false");
				  passed[0] = false;
			  }
			  if (results.residuals.size() != 1) {
				  System.err.println("In " + testName + " results.residuals.size() != 1");
				  passed[0] = false;
			  }
			  CheckDimensions(results, parameter_sizes, parameter_sizes, 1, testName, passed);
			  if (results.maximum_relative_error < 0.0) {
				  System.err.println("In " + testName + " results.maximum_relative_error < 0.0");
				  passed[0] = false;
			  }
			  if((results.error_log != null) && (!results.error_log.isEmpty())) {
				  System.err.println("In " + testName + " (results.error_log != null) && (!results.error_log.isEmpty())");
				  passed[0] = false; 
			  }

			  // Test that if the cost function return false, Probe should return false.
			  good_term.SetReturnValue(false);
			  if (good_gradient_checker.Probe(parameters, kTolerance, null)) {
				  System.err.println("In " + testName + " good_gradient_checker.Probe(parameters, kTolerance, null) = true");
				  passed[0] = false; 
			  };
			  if (good_gradient_checker.Probe(parameters, kTolerance, results)) {
				  System.err.println("In " + testName + " good_gradient_checker.Probe(parameters, kTolerance, results) = true");
				  passed[0] = false;  
			  }

			  // Check that results contain sensible data.
			  if (results.return_value) {
				  System.err.println("In " + testName + " results.return_value = true");
				  passed[0] = false;
			  }
			  
			  if (results.residuals.size() != 1) {
				  System.err.println("In " + testName + " results.residuals.size() != 1");
				  passed[0] = false;
			  }
			  CheckDimensions(results, parameter_sizes, parameter_sizes, 1, testName, passed);
			  for (int i = 0; i < num_parameters; ++i) {
				for (r = 0; r < results.local_jacobians.get(i).getRowDimension(); r++) {
					for (c = 0; c < results.local_jacobians.get(i).getColumnDimension(); c++) {
						if (results.local_jacobians.get(i).get(r,c) != 0.0) {
							System.err.println("In " + testName + " results.local_jacobians.get("+i+").get("+r+","+c+") != 0.0");
							passed[0] = false;
						}
					}
				}
				for (r = 0; r < results.local_numeric_jacobians.get(i).getRowDimension(); r++) {
					for (c = 0; c < results.local_numeric_jacobians.get(i).getColumnDimension(); c++) {
						if (results.local_numeric_jacobians.get(i).get(r,c) != 0.0) {
							System.err.println("In " + testName + " results.local_numeric_jacobians.get("+i+").get("+r+","+c+") != 0.0");
							passed[0] = false;
						}
					}
				}
			  }
			  if (results.maximum_relative_error != 0.0) {
				  System.err.println("In " + testName + " results.maximum_relative_error != 0.0");
				  passed[0] = false; 
			  }
			  if((results.error_log == null) || (results.error_log.isEmpty())) {
				  System.err.println("In " + testName + " (results.error_log == null) || (results.error_log.isEmpty())");
				  passed[0] = false; 
			  }

			  // Test that Probe returns false for incorrect Jacobians.
			  testCase = BAD_TEST_TERM_EXAMPLE;
			  BadTestTerm bad_term = new BadTestTerm(num_parameters, parameter_sizes_data);
			  GradientChecker bad_gradient_checker = new GradientChecker(bad_term, null, numeric_diff_options);
			  if (bad_gradient_checker.Probe(parameters, kTolerance, null)) {
				  System.err.println("In " + testName + " bad_gradient_checker.Probe(parameters, kTolerance, null) == true");
				  passed[0] = false;
			  }
			  if (bad_gradient_checker.Probe(parameters, kTolerance, results)) {
				  System.err.println("In " + testName + " bad_gradient_checker.Probe(parameters, kTolerance, results) == true");
				  passed[0] = false;
			  }

			  // Check that results contain sensible data.
			  if (!results.return_value) {
				  System.err.println("In " + testName + " results.return_value = false");
				  passed[0] = false;
			  }
			  if (results.residuals.size() != 1) {
				  System.err.println("In " + testName + " results.residuals.size() != 1");
				  passed[0] = false;
			  }
			  CheckDimensions(results, parameter_sizes, parameter_sizes, 1, testName, passed);
			  if (results.maximum_relative_error <= kTolerance) {
				  System.err.println("In " + testName + " results.maximum_relative_error <= kTolerance");
				  System.err.println("results.maximum_relative_error = " + results.maximum_relative_error);
				  passed[0] = false;
			  }
			  if((results.error_log == null) || (results.error_log.isEmpty())) {
				  System.err.println("In " + testName + " (results.error_log == null) || (results.error_log.isEmpty())");
				  passed[0] = false; 
			  }

			  // Setting a high threshold should make the test pass.
			  if (!bad_gradient_checker.Probe(parameters, 1.0, results)) {
				  System.err.println("In " + testName + " bad_gradient_checker.Probe(parameters, 1.0, results) == false");
				  passed[0] = false;
			  }

			  // Check that results contain sensible data.
			  if (!results.return_value) {
				  System.err.println("In " + testName + " results.return_value = false");
				  passed[0] = false;
			  }
			  if (results.residuals.size() != 1) {
				  System.err.println("In " + testName + " results.residuals.size() != 1");
				  passed[0] = false;
			  }
			  CheckDimensions(results, parameter_sizes, parameter_sizes, 1, testName, passed);
			  if (results.maximum_relative_error <= 0.0) {
				  System.err.println("In " + testName + " results.maximum_relative_error <= 0.0");
				  System.err.println("results.maximum_relative_error = " + results.maximum_relative_error);
				  passed[0] = false;
			  }
			  if((results.error_log != null) && (!results.error_log.isEmpty())) {
				  System.err.println("In " + testName + " (results.error_log != null) && (!results.error_log.isEmpty())");
				  passed[0] = false; 
			  }

			  for (int j = 0; j < num_parameters; j++) {
			    parameters.set(j, null);
			  }
			  if (passed[0]) {
			      System.out.println(testName + " passed all tests");
			  }
			}
		
		/**
		 * Helper local parameterization that multiplies the delta vector by the given
		 * jacobian and adds it to the parameter.
		 */
		class MatrixParameterization extends LocalParameterization {
			public Matrix global_J_local;
			public MatrixParameterization() {
				super();
			}
		    public boolean Plus(double[] x,
		                    double[] delta,
		                    double[] x_plus_delta) {
		    	int r, c;
		    	double prod[] = new double[GlobalSize()];
		    	for (r = 0; r < GlobalSize(); r++) {
		    		for (c = 0; c < LocalSize(); c++) {
		    			prod[r] += global_J_local.get(r,c) * delta[c];
		    		}
		    	}
		    	for (r = 0; r < GlobalSize(); r++) {
		    		x_plus_delta[r] = x[r] + prod[r];
		    	}
		    
		    return true;
		  }
		    
		    public boolean Plus(Vector<Double>x, int x_offset,
                    Vector<Double> delta, int delta_offset,
                    Vector<Double> x_plus_delta, int x_plus_delta_offset) {
    	int r, c;
    	double prod[] = new double[GlobalSize()];
    	for (r = 0; r < GlobalSize(); r++) {
    		for (c = 0; c < LocalSize(); c++) {
    			prod[r] += global_J_local.get(r,c) * delta.get(c + delta_offset);
    		}
    	}
    	for (r = 0; r < GlobalSize(); r++) {
    		x_plus_delta.set(r + x_plus_delta_offset, x.get(r+x_offset) + prod[r]);
    	}
    
    return true;
  }

		  public boolean ComputeJacobian(double[] x, double[] jacobian) {
			int r,c;
			for (r = 0; r < GlobalSize(); r++) {
				for (c = 0; c < LocalSize(); c++) {
					jacobian[r*LocalSize() +c ] = global_J_local.get(r,c);
				}
			}
		    return true;
		  }
		  
		  public boolean ComputeJacobian(double[] x, Matrix jacobian) {
				int r,c;
				for (r = 0; r < GlobalSize(); r++) {
					for (c = 0; c < LocalSize(); c++) {
						jacobian.set(r,c, global_J_local.get(r,c));
					}
				}
			    return true;
			  }
		  
		  public boolean ComputeJacobian(double[] x, double[][] jacobian) {
				int r,c;
				for (r = 0; r < GlobalSize(); r++) {
					for (c = 0; c < LocalSize(); c++) {
						jacobian[r][c] = global_J_local.get(r,c);
					}
				}
			    return true;
			  }
		  
		  public boolean ComputeJacobian(double[] x, int x_offset, double[][] jacobian) {
				int r,c;
				for (r = 0; r < GlobalSize(); r++) {
					for (c = 0; c < LocalSize(); c++) {
						jacobian[r][c] = global_J_local.get(r,c);
					}
				}
			    return true;
			  }

		  public int GlobalSize() { return global_J_local.getRowDimension(); }
		  public int LocalSize() { return global_J_local.getColumnDimension(); }

		};
		
		// Helper function to compare two Eigen matrices (used in the test below).
		public void ExpectMatricesClose(Matrix p, Matrix q, double tolerance, String testName, boolean passed[]) {
		  int r,c;
		  if (p.getRowDimension() != q.getRowDimension()) {
			  System.err.println("In " + testName + " p.getRowDimension() != q.getRowDimension()");
			  passed[0] = false;
			  return;
		  }
		  if (p.getColumnDimension() != q.getColumnDimension()) {
			  System.err.println("In " + testName + " p.getColumnDimension() != q.getColumnDimension()");
			  passed[0] = false;
			  return;
		  }
		  for (r = 0; r < p.getRowDimension(); r++) {
			  for (c = 0; c < p.getColumnDimension(); c++) {
				  if (Math.abs(p.get(r,c) - q.get(r,c)) > tolerance) {
					  System.err.println("In " + testName + " Math.abs(p.get("+r+","+c+") - q.get("+r+","+c+")) > tolerance");
					  System.err.println("p.get("+r+","+c+") = " + p.get(r,c));
					  System.err.println("q.get("+r+","+c+") = " + q.get(r,c));
					  passed[0] = false;
				  }
			  }
		  }
		}
		
		public void ExpectMatricesClose(double p[], double q[], double tolerance, String testName, boolean passed[]) {
			  int r,c;
			  if (p.length != q.length) {
				  System.err.println("In " + testName + " p.length != q.length");
				  passed[0] = false;
				  return;
			  }
			  
			  for (r = 0; r < p.length; r++) {
					  if (Math.abs(p[r] - q[r]) > tolerance) {
						  System.err.println("In " + testName + " Math.abs(p["+r+"] - q["+r+"]) > tolerance");
						  passed[0] = false;
					  }
			  }
			}

		public void GradientCheckerTestCorrectnessWithLocalParameterizations() {
			 // GradientCheckerTestCorrectnessWithLocalParameterizations() passed all tests
			  testCase = LINEAR_COST_FUNCTION_EXAMPLE;
			  int r,c;
			  boolean passed[] = new boolean[] {true};
			  String testName = "GradientCheckerTestCorrectnessWithLocalParameterizations()";
			  final double kTolerance = 1.0E-6;
			  // Create cost function.
			  Vector<Double> residual_offset = new Vector<Double>(3);
			  residual_offset.add(100.0);
			  residual_offset.add(200.0);
			  residual_offset.add(300.0);
			  LinearCostFunction cost_function = new LinearCostFunction(residual_offset);
			  Matrix j0 = new Matrix(3,3);
			  int j0_value = 1;
			  for (r = 0; r < 3; r++) {
				  for (c = 0; c < 3; c++) {
				       j0.set(r,c,(double)j0_value++);
				  }
			  }
			  Matrix j1 = new Matrix(3, 2);
			  int j1_value = 10;
			  for (r = 0; r < 3; r++) {
				  for (c = 0; c < 2; c++) {
				       j1.set(r,c,(double)j1_value++);
				  }
			  }
			  

			  double param0[] = new double[] {1.0, 2.0, 3.0};
			  double param1[] = new double[] {4.0, 5.0};

			  cost_function.AddParameter(j0, testName, passed);
			  cost_function.AddParameter(j1, testName, passed);

			  Vector<Integer> parameter_sizes = new Vector<Integer>(2);
			  parameter_sizes.add(3);
			  parameter_sizes.add(2);
			  Vector<Integer> local_parameter_sizes = new Vector<Integer>(2);
			  local_parameter_sizes.add(2);
			  local_parameter_sizes.add(2);

			  // Test cost function for correctness.
			  Matrix j1_out = new Matrix(3,3);
			  Matrix j2_out = new Matrix(3,2);
			  double residual[] = new double[3];
			  Vector<double[]> parameters = new Vector<double[]>(2);
			  parameters.add(param0);
			  parameters.add(param1);
			  Vector<Matrix> jacobians = new Vector<Matrix>(2);
			  jacobians.add(j1_out);
			  jacobians.add(j2_out);
			  cost_function.Evaluate(parameters, residual, jacobians);

			  double j0param[] = new double[3];
			  for (r = 0; r < 3; r++) {
				  for (c = 0; c < 3; c++) {
					  j0param[r] += j0.get(r,c) * param0[c];
				  }
			  }
			  double j1param[] = new double[3];
			  for (r = 0; r < 3; r++) {
				  for (c = 0; c < 2; c++) {
					  j1param[r] += j1.get(r,c) * param1[c];
				  }
			  }
			  double residual_expected[] = new double[3];
			  for (r = 0; r < 3; r++) {
				  residual_expected[r] = residual_offset.get(r) + j0param[r] + j1param[r];
			  }

			  ExpectMatricesClose(j1_out, j0, epsilon, testName, passed);
			  ExpectMatricesClose(j2_out, j1, epsilon, testName, passed);
			  ExpectMatricesClose(residual, residual_expected, kTolerance, testName, passed);

			  // Create local parameterization.
			  Matrix global_J_local = new Matrix(3,2);
			  global_J_local.set(0,0,1.5);
			  global_J_local.set(0,1,2.5);
			  global_J_local.set(1,0,3.5);
			  global_J_local.set(1,1,4.5);
			  global_J_local.set(2,0,5.5);
			  global_J_local.set(2,1,6.5);

			  MatrixParameterization parameterization = new MatrixParameterization();
			  parameterization.global_J_local = new Matrix(3,2);
			  parameterization.global_J_local.set(0,0,1.5);
			  parameterization.global_J_local.set(0,1,2.5);
			  parameterization.global_J_local.set(1,0,3.5);
			  parameterization.global_J_local.set(1,1,4.5);
			  parameterization.global_J_local.set(2,0,5.5);
			  parameterization.global_J_local.set(2,1,6.5);

			  // Test local parameterization for correctness.
			  double x[] = new double[] {7.0, 8.0, 9.0};
			  double delta[] = new double[] {10.0, 11.0};

			  Matrix global_J_local_out = new Matrix(3, 2);
			  parameterization.ComputeJacobian(x, global_J_local_out);
			  ExpectMatricesClose(global_J_local_out,
			                      global_J_local,
			                      epsilon, testName, passed);

			  double x_plus_delta[] = new double[3];
			  parameterization.Plus(x, delta, x_plus_delta);
			  double x_plus_delta_expected[] = new double[3];
			  double gdelta[] = new double[3];
			  for (r = 0; r < 3; r++) {
				  for (c = 0; c < 2; c++) {
					  gdelta[r] += global_J_local.get(r,c) * delta[c];
				  }
			  }
			  for (r = 0; r < 3; r++) {
				  x_plus_delta_expected[r] = x[r] + gdelta[r];
			  }
			  ExpectMatricesClose(x_plus_delta, x_plus_delta_expected, kTolerance, testName, passed);

			  // Now test GradientChecker.
			  Vector<LocalParameterization> parameterizations = new Vector<LocalParameterization>(2);
			  parameterizations.add(parameterization);
			  parameterizations.add(null);
			  NumericDiffOptions numeric_diff_options = new NumericDiffOptions();
			  ProbeResults results = new ProbeResults();
			  GradientChecker gradient_checker = 
			      new GradientChecker(cost_function, parameterizations, numeric_diff_options);

			  ProblemOptions problem_options = new ProblemOptions();
			  problem_options.cost_function_ownership = Ownership.DO_NOT_TAKE_OWNERSHIP;
			  problem_options.local_parameterization_ownership = Ownership.DO_NOT_TAKE_OWNERSHIP;
			  ProblemImpl problem = new ProblemImpl(problem_options);
			  double param0_solver[] = new double[3];
			  double param1_solver[] = new double[2];
			  problem.AddParameterBlock(param0_solver, 3, parameterization);
			  problem.AddParameterBlock(param1_solver, 2);
			  problem.AddResidualBlock(
			      cost_function, null, param0_solver, param1_solver);
			  SolverOptions solver_options = new SolverOptions();
			  solver_options.check_gradients = true;
			  solver_options.initial_trust_region_radius = 2e15;
			  solver_options.gradient_tolerance = 2.0E-10;
			  SolverSummary summary = new SolverSummary();

			  // First test case: everything is correct.
			  if(!gradient_checker.Probe(parameters, kTolerance, null)) {
				  System.err.println("In " + testName + " gradient_checker.Probe(parameters, kTolerance, null) == false");
				  passed[0] = false;
			  }
			  if (!gradient_checker.Probe(parameters, kTolerance, results)) {
				  System.err.println("In " + testName + " gradient_checker.Probe(parameters, kTolerance, results) == false");
				  passed[0] = false;
				  System.err.println(results.error_log);
			  }

			  // Check that results contain correct data.
			  if (!results.return_value) {
				  System.err.println("In " + testName + " results.return_value == false");
				  passed[0] = false; 
			  }
			  double results_residuals_data[] = new double[results.residuals.size()];
			  for (r = 0; r < results.residuals.size(); r++) {
				  results_residuals_data[r] = results.residuals.get(r);
			  }
			  ExpectMatricesClose(
			      results_residuals_data, residual, epsilon, testName, passed);
			  CheckDimensions(results, parameter_sizes, local_parameter_sizes, 3, testName, passed);
			  ExpectMatricesClose(
			      results.local_jacobians.get(0), j0.times(global_J_local), kTolerance, testName, passed);
			  ExpectMatricesClose(results.local_jacobians.get(1),
			                      j1,
			                      epsilon, testName, passed);
			  ExpectMatricesClose(
			      results.local_numeric_jacobians.get(0), j0.times(global_J_local), kTolerance, testName, passed);
			  ExpectMatricesClose(results.local_numeric_jacobians.get(1), j1, kTolerance, testName, passed);
			  ExpectMatricesClose(
			      results.jacobians.get(0), j0, epsilon, testName, passed);
			  ExpectMatricesClose(
			      results.jacobians.get(1), j1, epsilon, testName, passed);
			  ExpectMatricesClose(results.numeric_jacobians.get(0), j0, kTolerance, testName, passed);
			  ExpectMatricesClose(results.numeric_jacobians.get(1), j1, kTolerance, testName, passed);
			  if (results.maximum_relative_error < 0.0) {
				  System.err.println("In " + testName + " results.maximum_relative_error < 0.0");
				  passed[0] = false; 
			  }
			  if ((results.error_log != null) && (!results.error_log.isEmpty())) {
				  System.err.println("In " + testName + " (results.error_log != null) && (!results.error_log.isEmpty())");
				  passed[0] = false;
			  };

			  // Test interaction with the 'check_gradients' option in Solver.
			  for (r = 0; r < 3; r++) {
				  param0_solver[r] = param0[r];
			  }
			  for (r = 0; r < 2; r++) {
				  param1_solver[r] = param1[r];
			  }
			  
			  Solve(solver_options, problem, summary);
			  if (TerminationType.CONVERGENCE != summary.termination_type) {
				  System.err.println("In " + testName + " TerminationType.CONVERGENCE != summary.termination_type");
				  System.err.println("TerminationType = " + TerminationTypeToString(summary.termination_type));
				  passed[0] = false;
			  }
			  System.err.println("summary.final_cost = " + summary.final_cost);
			  if (summary.final_cost > 1e-12) {
				  System.err.println("In " + testName + " summary.final_cost  = " + summary.final_cost + " > 1e-12");
				  passed[0] = false;
			  }

			  // Second test case: Mess up reported derivatives with respect to 3rd
			  // component of 1st parameter. Check should fail.
			  Matrix j0_offset = new Matrix(3, 3, 0.0);
			  for (r = 0; r < 3; r++) {
				  j0_offset.set(r, 2, 0.001);
			  }
			  cost_function.SetJacobianOffset(0, j0_offset, testName, passed);
			  gradient_checker = 
				      new GradientChecker(cost_function, parameterizations, numeric_diff_options);
			  if (gradient_checker.Probe(parameters, kTolerance, null)) {
				  System.err.println("In " + testName + " gradient_checker.Probe(parameters, kTolerance, null) == true");
				  passed[0] = false;
			  }
			  if (gradient_checker.Probe(parameters, kTolerance, results)) {
				  System.err.println("In " + testName + " gradient_checker.Probe(parameters, kTolerance, results) == true");
				  passed[0] = false;
			  }

			  // Check that results contain correct data.
			  if (!results.return_value) {
				  System.err.println("In " + testName + " results.return_value == false");
				  passed[0] = false; 
			  }
			  results_residuals_data = new double[results.residuals.size()];
			  for (r = 0; r < results.residuals.size(); r++) {
				  results_residuals_data[r] = results.residuals.get(r);
			  }
			  ExpectMatricesClose(
			      results_residuals_data, residual, epsilon, testName, passed);
			  CheckDimensions(results, parameter_sizes, local_parameter_sizes, 3, testName, passed);
			  if (results.local_jacobians.size() != 2) {
				  System.err.println("In " + testName + " results.local_jacobians.size() != 2");
				  passed[0] = false;
			  }
			  if (results.local_numeric_jacobians.size() != 2) {
				  System.err.println("In " + testName + " results.local_numeric_jacobians.size() != 2");
				  passed[0] = false;
			  }
			  ExpectMatricesClose(results.local_jacobians.get(0),
			                      (j0.plus(j0_offset)).times(global_J_local),
			                      kTolerance, testName, passed);
			  ExpectMatricesClose(results.local_jacobians.get(1),
			                      j1,
			                      epsilon, testName, passed);
			  ExpectMatricesClose(
			      results.local_numeric_jacobians.get(0), j0.times(global_J_local), kTolerance, testName, passed);
			  ExpectMatricesClose(results.local_numeric_jacobians.get(1), j1, kTolerance, testName, passed);
			  ExpectMatricesClose(results.jacobians.get(0), j0.plus(j0_offset), kTolerance, testName, passed);
			  ExpectMatricesClose(
			      results.jacobians.get(1), j1, epsilon, testName, passed);
			  ExpectMatricesClose(results.numeric_jacobians.get(0), j0, kTolerance, testName, passed);
			  ExpectMatricesClose(results.numeric_jacobians.get(1), j1, kTolerance, testName, passed);
			  if (results.maximum_relative_error <= 0.0) {
				  System.err.println("In " + testName + " results.maximum_relative_error <= 0.0");
				  passed[0] = false; 
			  }
			  if ((results.error_log == null) || (results.error_log.isEmpty())) {
				  System.err.println("In " + testName + " (results.error_log == null) || (results.error_log.isEmpty())");
				  passed[0] = false;
			  };

			  // Test interaction with the 'check_gradients' option in Solver.
			  for (r = 0; r < 3; r++) {
				  param0_solver[r] = param0[r];
			  }
			  for (r = 0; r < 2; r++) {
				  param1_solver[r] = param1[r];
			  }
			  
			  summary = new SolverSummary();
			  solver_options = new SolverOptions();
			  solver_options.check_gradients = true;
			  solver_options.initial_trust_region_radius = 2e15;
			  solver_options.gradient_tolerance = 2.0E-10;
			  System.err.println("Will detect gradient error");
			  Solve(solver_options, problem, summary);
			  System.err.println("Have detected gradient error");
			  if (TerminationType.FAILURE != summary.termination_type) {
				  System.err.println("In " + testName + " TerminationType.FAILURE != summary.termination_type");
				  System.err.println("TerminationType = " + TerminationTypeToString(summary.termination_type));
				  passed[0] = false; 
			  }

			  // Now, zero out the local parameterization Jacobian of the 1st parameter
			  // with respect to the 3rd component. This makes the combination of
			  // cost function and local parameterization return correct values again.
			  parameterization.global_J_local.set(2, 0, 0.0);
			  parameterization.global_J_local.set(2, 1, 0.0);


			  // Verify that the gradient checker does not treat this as an error.
			  if (!gradient_checker.Probe(parameters, kTolerance, results)) {
				  System.err.println("In " + testName + " gradient_checker.Probe(parameters, kTolerance, results) == false");
				  passed[0] = false; 
			  }

			  // Check that results contain correct data.
			  if (!results.return_value) {
				  System.err.println("In " + testName + " results.return_value == false");
				  passed[0] = false; 
			  }
			  results_residuals_data = new double[results.residuals.size()];
			  for (r = 0; r < results.residuals.size(); r++) {
				  results_residuals_data[r] = results.residuals.get(r);
			  }
			  ExpectMatricesClose(
			      results_residuals_data, residual, epsilon, testName, passed);
			  CheckDimensions(results, parameter_sizes, local_parameter_sizes, 3, testName, passed);
			  if (results.local_jacobians.size() != 2) {
				  System.err.println("In " + testName + " results.local_jacobians.size() != 2");
				  passed[0] = false; 
			  }
			  if (results.local_numeric_jacobians.size() != 2) {
				  System.err.println("In " + testName + " results.local_numeric_jacobians.size() != 2");
				  passed[0] = false; 
			  }
			  ExpectMatricesClose(results.local_jacobians.get(0),
			                      (j0.plus(j0_offset)).times(parameterization.global_J_local),
			                      kTolerance, testName, passed);
			  ExpectMatricesClose(results.local_jacobians.get(1),
			                      j1,
			                      epsilon, testName, passed);
			  ExpectMatricesClose(results.local_numeric_jacobians.get(0),
			                      j0.times(parameterization.global_J_local),
			                      kTolerance, testName, passed);
			  ExpectMatricesClose(results.local_numeric_jacobians.get(1), j1, kTolerance, testName, passed);
			  ExpectMatricesClose(results.jacobians.get(0), j0.plus(j0_offset), kTolerance, testName, passed);
			  ExpectMatricesClose(
			      results.jacobians.get(1), j1, epsilon, testName, passed);
			  ExpectMatricesClose(results.numeric_jacobians.get(0), j0, kTolerance, testName, passed);
			  ExpectMatricesClose(results.numeric_jacobians.get(1), j1, kTolerance, testName, passed);
			  if (results.maximum_relative_error < 0.0) {
				  System.err.println("In " + testName + " results.maximum_relative_error < 0.0");
				  passed[0] = false; 
			  }
			  if ((results.error_log != null) && (!results.error_log.isEmpty())) {
				  System.err.println("In " + testName + " (results.error_log != null) && (!results.error_log.isEmpty())");
				  passed[0] = false;
			  };

			  // Test interaction with the 'check_gradients' option in Solver.
			  for (r = 0; r < 3; r++) {
				  param0_solver[r] = param0[r];
			  }
			  for (r = 0; r < 2; r++) {
				  param1_solver[r] = param1[r];
			  }
			  
			  summary = new SolverSummary();
			  solver_options = new SolverOptions();
			  solver_options.check_gradients = true;
			  solver_options.initial_trust_region_radius = 2e15;
			  solver_options.gradient_tolerance = 2.0E-10;
			  Solve(solver_options, problem, summary);
			  if (TerminationType.CONVERGENCE != summary.termination_type) {
				  System.err.println("In " + testName + " TerminationType.CONVERGENCE != summary.termination_type");
				  System.err.println("TerminationType = " + TerminationTypeToString(summary.termination_type));
				  passed[0] = false;
			  }

			  System.err.println("summary.final_cost = " + summary.final_cost);
			  if (summary.final_cost > 1e-12) {
				  System.err.println("In " + testName + " summary.final_cost > 1e-12");
				  passed[0] = false;
			  }
			  if (passed[0]) {
				  System.out.println(testName + " passed all tests");
			  }
			}

		
		public long hash(byte data[]) {
			// Force hash to be an unsigned int value
			long hash = 5381;
			for (int i = 0; i < data.length; i++) {
				hash = 0xffffffffL & (((hash << 5) + hash) + data[i]);
			}
			return hash;
		}
		
	    final long kUninitialized = 0;

		// Generally multiple inheritance is a terrible idea, but in this (test)
		// case it makes for a relatively elegant test implementation.
		class WigglyBowlCostFunctionAndEvaluationCallback extends SizedCostFunction {
			  // Pointer to the parameter block associated with this cost function.
			  // Contents should get set by Ceres before calls to PrepareForEvaluation()
			  // and Evaluate().
			  public double[] user_parameter_block;

			  // Track state: PrepareForEvaluation().
			  //
			  // These track details from the PrepareForEvaluation() call (hence the
			  // "prepare_" prefix), which are checked for consistency in Evaluate().
			  public int prepare_num_calls;
			  public boolean prepare_requested_jacobians;
			  public boolean prepare_new_evaluation_point;
			  public long prepare_parameter_hash;
			  public double lastx = -1.0;
			  public double lasty = -1.0;

			  // Track state: Evaluate().
			  //
			  // These track details from the Evaluate() call (hence the "evaluate_"
			  // prefix), which are then checked for consistency in the calls to
			  // PrepareForEvaluation(). Mutable is reasonable for this case.
			  public int evaluate_num_calls;
			  public long evaluate_last_parameter_hash;
			  public InnerClass innerClass;
			  String testName;
			  boolean passed[];
		      //SizedCostFunction<2, 2>,

		      public WigglyBowlCostFunctionAndEvaluationCallback(double[] parameter, String testName, boolean passed[]) {
		        super(2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		        innerClass = new InnerClass();
		        user_parameter_block = parameter;
		        prepare_num_calls = 0;
		        evaluate_num_calls = 0;
		        evaluate_last_parameter_hash = kUninitialized;
		        this.testName = testName;
		        this.passed = passed;
		      }
		      
		      public InnerClass getInnerClass() {
		          return innerClass;
		      }

		  // Evaluation callback interface. This checks that all the preconditions are
		  // met at the point that Ceres calls into it.
              class InnerClass extends EvaluationCallback {
				  public InnerClass() {
					  super();
				  }
				  public void PrepareForEvaluation(boolean evaluate_jacobians,
				                                    boolean new_evaluation_point) {
					int i,j;
				    // At this point, the incoming parameters are implicitly pushed by Ceres
				    // into the user parameter blocks; in contrast to in Evaluate().
					byte buffer2D[][] = new byte[user_parameter_block.length][8];
					byte buffer[] = new byte[8 * user_parameter_block.length];
					for (i = 0; i < user_parameter_block.length; i++) {
						FileBase.doubleToBytes(user_parameter_block[i], true, buffer2D[i]);
						for (j = 0; j < 8; j++) {
							buffer[8*i + j] = buffer2D[i][j];
						}
					}
					
				    long incoming_parameter_hash = hash(buffer);
		
				    // Check: Prepare() & Evaluate() come in pairs, in that order. Before this
				    // call, the number of calls excluding this one should match.
				    if (prepare_num_calls != evaluate_num_calls) {
				    	System.err.println("In PrepareForEvaluation " + testName + " prepare_num_calls != evaluate_num_calls");
				    	passed[0] = false;
				    }
		
				    // Check: new_evaluation_point indicates that the parameter has changed.
				    if (new_evaluation_point) {
				      // If it's a new evaluation point, then the parameter should have
				      // changed. Technically, it's not required that it must change but
				      // in practice it does, and that helps with testing.
				      if (evaluate_last_parameter_hash == incoming_parameter_hash) {
				    	  System.err.println("In PrepareForEvaluation " + testName + " evaluate_last_parameter_hash == incoming_parameter_hash");
				    	  passed[0] = false;
				      }
				      if (prepare_parameter_hash == incoming_parameter_hash) {
				    	  System.err.println("In PrepareForEvaluation " + testName + " prepare_parameter_hash == incoming_parameter_hash");
				    	  passed[0] = false;
				      }
				    } else {
				      // If this is the same evaluation point as last time, ensure that
				      // the parameters match both from the previous evaluate, the
				      // previous prepare, and the current prepare.
				      if (evaluate_last_parameter_hash != prepare_parameter_hash) {
				    	 System.err.println("In PrepareForEvaluation " + testName + " evaluate_last_parameter_hash != prepare_parameter_hash");
				    	 passed[0] = false;
				      }
				      if (evaluate_last_parameter_hash != incoming_parameter_hash) {
				    	  System.err.println("In PrepareForEvaluation " + testName + " evaluate_last_parameter_hash != incoming_parameter_hash");
				    	  System.err.println("user_parameter_block[0] = " + user_parameter_block[0] + " lastx = " + lastx);
				    	  System.err.println("user_parameter_block[1] = " + user_parameter_block[1] + " lasty = " + lasty);
				    	  passed[0] = false;
				      }
				    }
		
				    // Save details for to check at the next call to Evaluate().
				    prepare_num_calls++;
				    prepare_requested_jacobians = evaluate_jacobians;
				    prepare_new_evaluation_point = new_evaluation_point;
				    prepare_parameter_hash = incoming_parameter_hash;
				  }
              }
		  
		  
			  
			  // Cost function interface. This checks that preconditions that were
			  // set as part of the PrepareForEvaluation() call are met in this one.
			  public boolean Evaluate(Vector<double[]> parameters,
			                        double[] residuals,
			                        double[][] jacobians) {
				int i,j;
			    // Cost function implementation of the "Wiggly Bowl" function:
			    //
			    //   1/2 * [(y - a*sin(x))^2 + x^2],
			    //
			    // expressed as a Ceres cost function with two residuals:
			    //
			    //   r[0] = y - a*sin(x)
			    //   r[1] = x.
			    //
			    // This is harder to optimize than the Rosenbrock function because the
			    // minimizer has to navigate a sine-shaped valley while descending the 1D
			    // parabola formed along the y axis. Note that the "a" needs to be more
			    // than 5 to get a strong enough wiggle effect in the cost surface to
			    // trigger failed iterations in the optimizer.
			    final double a = 10.0;
			    double x = parameters.get(0)[0];
			    double y = parameters.get(0)[1];
			    residuals[0] = y - a * Math.sin(x);
			    residuals[1] = x;
			    if (jacobians != null) {
			      jacobians[0][0] = - a * Math.cos(x);  // df1/dx
			      jacobians[0][1] = 1.0;           // df1/dy
			      jacobians[0][2] = 1.0;           // df2/dx
			      jacobians[0][3] = 0.0;           // df2/dy
			    }

			 // into the user parameter blocks; in contrast to in Evaluate().
				byte buffer2D[][] = new byte[parameters.get(0).length][8];
				byte buffer[] = new byte[8 * parameters.get(0).length];
				for (i = 0; i < parameters.get(0).length; i++) {
					FileBase.doubleToBytes(parameters.get(0)[i], true, buffer2D[i]);
					for (j = 0; j < 8; j++) {
						buffer[8*i + j] = buffer2D[i][j];
					}
				}
			    long incoming_parameter_hash = hash(buffer);

			    // Check: PrepareForEvaluation() & Evaluate() come in pairs, in that order.
			    if (prepare_num_calls != evaluate_num_calls + 1) {
			    	System.err.println("In Evaluate " + testName + " prepare_num_calls != evaluate_num_calls + 1");
			    	passed[0] = false;
			    }

			    // Check: if new_evaluation_point indicates that the parameter has
			    // changed, it has changed; otherwise it is the same.
			    if (prepare_new_evaluation_point) {
			      if (evaluate_last_parameter_hash == incoming_parameter_hash) {
			    	  System.err.println("In Evaluate " + testName + " evaluate_last_parameter_hash == incoming_parameter_hash");
			    	  passed[0] = false;
			      }
			    } else {
			      if (evaluate_last_parameter_hash == kUninitialized) {
			    	  System.err.println("In Evaluate " + testName + " evaluate_last_parameter_hash == kUninitialized");
			    	  passed[0] = false;
			      }
			      if (evaluate_last_parameter_hash != incoming_parameter_hash) {
			    	  System.err.println("In Evaluate " + testName + " evaluate_last_parameter_hash != incoming_parameter_hash");
			    	  System.err.println("x = " + x + " lastx = " + lastx);
			    	  System.err.println("y = " + y + " lasty = " + lasty);
			    	  passed[0] = false;
			      }
			    }

			    // Check: Parameter matches value in in parameter blocks during prepare.
			    if (prepare_parameter_hash != incoming_parameter_hash) {
			    	  System.err.println("In Evaluate " + testName + " prepare_parameter_hash != incoming_parameter_hash");
			    	  passed[0] = false;
			    }

			    // Check: jacobians are requested if they were in PrepareForEvaluation().
			    if (prepare_requested_jacobians != (jacobians != null)) {
			    	System.err.println("In Evaluate " + testName + " prepare_requested_jacobians != (jacobians != null)");
			    	passed[0] = false;
			    }

			    evaluate_num_calls++;
			    evaluate_last_parameter_hash = incoming_parameter_hash;
			    lastx = x;
			    lasty = y;
			    return true;
			  }
			  
			// Cost function interface. This checks that preconditions that were
			  // set as part of the PrepareForEvaluation() call are met in this one.
			  public boolean Evaluate(Vector<double[]> parameters,
			                        double[] residuals,
			                        double[][] jacobians, int[] jacobian_offset) {
				int i,j;
			    // Cost function implementation of the "Wiggly Bowl" function:
			    //
			    //   1/2 * [(y - a*sin(x))^2 + x^2],
			    //
			    // expressed as a Ceres cost function with two residuals:
			    //
			    //   r[0] = y - a*sin(x)
			    //   r[1] = x.
			    //
			    // This is harder to optimize than the Rosenbrock function because the
			    // minimizer has to navigate a sine-shaped valley while descending the 1D
			    // parabola formed along the y axis. Note that the "a" needs to be more
			    // than 5 to get a strong enough wiggle effect in the cost surface to
			    // trigger failed iterations in the optimizer.
			    //final double a = 10.0;
				final double a = 2.0;
			    double x = parameters.get(0)[0];
			    double y = parameters.get(0)[1];
			    residuals[0] = y - a * Math.sin(x);
			    residuals[1] = x;
			    if (jacobians != null) {
			      jacobians[0][jacobian_offset[0]] = - a * Math.cos(x);  // df1/dx
			      jacobians[0][jacobian_offset[0] + 1] = 1.0;           // df1/dy
			      jacobians[0][jacobian_offset[0] + 2] = 1.0;           // df2/dx
			      jacobians[0][jacobian_offset[0] + 3] = 0.0;           // df2/dy
			    }

			 // into the user parameter blocks; in contrast to in Evaluate().
				byte buffer2D[][] = new byte[parameters.get(0).length][8];
				byte buffer[] = new byte[8 * parameters.get(0).length];
				for (i = 0; i < parameters.get(0).length; i++) {
					FileBase.doubleToBytes(parameters.get(0)[i], true, buffer2D[i]);
					for (j = 0; j < 8; j++) {
						buffer[8*i + j] = buffer2D[i][j];
					}
				}
			    long incoming_parameter_hash = hash(buffer);

			    // Check: PrepareForEvaluation() & Evaluate() come in pairs, in that order.
			    if (prepare_num_calls != evaluate_num_calls + 1) {
			    	System.err.println("In Evaluate " + testName + " prepare_num_calls != evaluate_num_calls + 1");
			    	passed[0] = false;
			    }

			    // Check: if new_evaluation_point indicates that the parameter has
			    // changed, it has changed; otherwise it is the same.
			    if (prepare_new_evaluation_point) {
			      if (evaluate_last_parameter_hash == incoming_parameter_hash) {
			    	  System.err.println("In Evaluate " + testName + " evaluate_last_parameter_hash == incoming_parameter_hash");
			    	  passed[0] = false;
			      }
			    } else {
			      if (evaluate_last_parameter_hash == kUninitialized) {
			    	  System.err.println("In Evaluate " + testName + " evaluate_last_parameter_hash == kUninitialized");
			    	  passed[0] = false;
			      }
			      if (evaluate_last_parameter_hash != incoming_parameter_hash) {
			    	  System.err.println("In Evaluate " + testName + " evaluate_last_parameter_hash != incoming_parameter_hash");
			    	  System.err.println("x = " + x + " lastx = " + lastx);
			    	  System.err.println("y = " + y + " lasty = " + lasty);
			    	  passed[0] = false;
			      }
			    }

			    // Check: Parameter matches value in in parameter blocks during prepare.
			    if (prepare_parameter_hash != incoming_parameter_hash) {
			    	  System.err.println("In Evaluate " + testName + " prepare_parameter_hash != incoming_parameter_hash");
			    	  passed[0] = false;
			    }

			    // Check: jacobians are requested if they were in PrepareForEvaluation().
			    if (prepare_requested_jacobians != (jacobians != null)) {
			    	System.err.println("In Evaluate " + testName + " prepare_requested_jacobians != (jacobians != null)");
			    	passed[0] = false;
			    }

			    evaluate_num_calls++;
			    evaluate_last_parameter_hash = incoming_parameter_hash;
			    lastx = x;
			    lasty = y;
			    return true;
			  }
  
  
		};
		
		public void EvaluationCallbackWithTrustRegionMinimizer() {
			  // EvaluationCallbackWithTrustRegionMinimizer() passed all tests
			  int i,j;
			  String testName = "EvaluationCallbackWithTrustRegionMinimizer()";
			  boolean passed[] = new boolean[] {true};
			  double parameters[] = new double[]{50.0, 50.0};
			  byte buffer2D[][] = new byte[parameters.length][8];
				byte buffer[] = new byte[8 * parameters.length];
				for (i = 0; i < parameters.length; i++) {
					FileBase.doubleToBytes(parameters[i], true, buffer2D[i]);
					for (j = 0; j < 8; j++) {
						buffer[8*i + j] = buffer2D[i][j];
					}
				}

			  final long original_parameters_hash = hash(buffer);

			  WigglyBowlCostFunctionAndEvaluationCallback cost_function = new WigglyBowlCostFunctionAndEvaluationCallback(parameters, testName, passed);
			  ProblemOptions problem_options = new ProblemOptions();
			  problem_options.cost_function_ownership = Ownership.DO_NOT_TAKE_OWNERSHIP;
			  ProblemImpl problem = new ProblemImpl(problem_options);
			  problem.AddResidualBlock(cost_function, null, parameters);

			  SolverOptions options = new SolverOptions();
			  options.linear_solver_type = LinearSolverType.DENSE_QR;
			  options.max_num_iterations = 300;  // Cost function is hard.
			  options.max_num_consecutive_invalid_steps = 300;
			  options.min_trust_region_radius = 1.0E-100;
			  options.evaluation_callback = cost_function.innerClass;

			  // Run the solve. Checking is done inside the cost function / callback.
			  SolverSummary summary = new SolverSummary();
			  Solve(options, problem, summary);

			  // Ensure that this was a hard cost function (not all steps succeed).
			  if (summary.num_successful_steps <= 10) {
				  System.err.println("In " + testName + " summary.num_successful_steps <= 10");
				  passed[0] = false;
			  }
			  if (summary.num_unsuccessful_steps <= 10) {
				  System.err.println("In " + testName + " summary.num_unsuccessful_steps <= 10");
				  passed[0] = false;
			  }

			  // Ensure PrepareForEvaluation() is called the appropriate number of times.
			  /*if (cost_function.prepare_num_calls !=
			            // Unsuccessful steps are evaluated only once (no jacobians).
			            summary.num_unsuccessful_steps +
			            // Successful steps are evaluated twice: with and without jacobians.
			            2 * summary.num_successful_steps
			            // Final iteration doesn't re-evaluate the jacobian.
			            // Note: This may be sensitive to tweaks to the TR algorithm; if
			            // this becomes too brittle, remove this EXPECT_EQ() entirely.
			            - 1) {
				  System.err.println("In " + testName + " (cost_function.prepare_num_calls != summary.num_unsuccessful_steps + 2 * summary.num_successful_steps - 1");
				  passed[0] = false;
			  }*/

			  // Ensure the callback calls ran a reasonable number of times.
			  if (cost_function.prepare_num_calls <= 0) {
				  System.err.println("In " + testName + " cost_function.prepare_num_calls <= 0");
				  passed[0] = false;
			  }
			  if (cost_function.evaluate_num_calls <= 0) {
				  System.err.println("In " + testName + " cost_function.evaluate_num_calls <= 0");
				  passed[0] = false;
			  }
			  if (cost_function.prepare_num_calls != cost_function.evaluate_num_calls) {
				  System.err.println("In " + testName + " cost_function.prepare_num_calls != cost_function.evaluate_num_calls");
				  passed[0] = false;
			  }

			  // Ensure that the parameters did actually change.
			  buffer2D = new byte[parameters.length][8];
				buffer = new byte[8 * parameters.length];
				for (i = 0; i < parameters.length; i++) {
					FileBase.doubleToBytes(parameters[i], true, buffer2D[i]);
					for (j = 0; j < 8; j++) {
						buffer[8*i + j] = buffer2D[i][j];
					}
				}
				final long new_parameters_hash = hash(buffer);
			    if (new_parameters_hash == original_parameters_hash) {
			    	System.err.println("In " + testName + " new_parameters_hash == original_parameters_hash");
			    	passed[0] = false;
			    }
			    if (passed[0]) {
			    	System.out.println(testName + " passed all tests");
			    }
			}

		public void WithLineSearchMinimizerImpl(
			    LineSearchType line_search,
			    LineSearchDirectionType line_search_direction,
			    LineSearchInterpolationType line_search_interpolation,
			    String testName, boolean passed[]) {
			  int i,j;
			  double parameters[] = new double[]{50.0, 50.0};
			  byte buffer2D[][] = new byte[parameters.length][8];
				byte buffer[] = new byte[8 * parameters.length];
				for (i = 0; i < parameters.length; i++) {
					FileBase.doubleToBytes(parameters[i], true, buffer2D[i]);
					for (j = 0; j < 8; j++) {
						buffer[8*i + j] = buffer2D[i][j];
					}
				}

			  final long original_parameters_hash = hash(buffer);

			  WigglyBowlCostFunctionAndEvaluationCallback cost_function = new WigglyBowlCostFunctionAndEvaluationCallback(parameters, testName, passed);
			  ProblemOptions problem_options = new ProblemOptions();
			  problem_options.cost_function_ownership = Ownership.DO_NOT_TAKE_OWNERSHIP;
			  ProblemImpl problem = new ProblemImpl(problem_options);
			  problem.AddResidualBlock(cost_function, null, parameters);

			  SolverOptions options = new SolverOptions();
			  options.linear_solver_type = LinearSolverType.DENSE_QR;
			  options.max_num_iterations = 300;  // Cost function is hard.
			  options.minimizer_type = MinimizerType.LINE_SEARCH;
			  options.evaluation_callback = cost_function.innerClass;
			  options.line_search_type = line_search;
			  options.line_search_direction_type = line_search_direction;
			  options.line_search_interpolation_type = line_search_interpolation;
			  options.min_line_search_step_size = 1.0E-11;

			  // Run the solve. Checking is done inside the cost function / callback.
			  SolverSummary summary = new SolverSummary();
			  Solve(options, problem, summary);

			  // Ensure the callback calls ran a reasonable number of times.
			  if (summary.num_line_search_steps <= 10) {
				  System.err.println("In " + testName + " summary.num_line_search_steps <= 10");
				  passed[0] = false;
			  }
			  if (cost_function.prepare_num_calls <= 30) {
				  System.err.println("In " + testName + " cost_function.prepare_num_calls <= 30");
				  passed[0] = false;
			  }
			  if (cost_function.prepare_num_calls != cost_function.evaluate_num_calls) {
				  System.err.println("In " + testName + " cost_function.prepare_num_calls != cost_function.evaluate_num_calls");
				  passed[0] = false;
			  }

			  // Ensure that the parameters did actually change.
			  buffer2D = new byte[parameters.length][8];
				buffer = new byte[8 * parameters.length];
				for (i = 0; i < parameters.length; i++) {
					FileBase.doubleToBytes(parameters[i], true, buffer2D[i]);
					for (j = 0; j < 8; j++) {
						buffer[8*i + j] = buffer2D[i][j];
					}
				}
				final long new_parameters_hash = hash(buffer);
			    if (new_parameters_hash == original_parameters_hash) {
			    	System.err.println("In " + testName + " new_parameters_hash == original_parameters_hash");
			    	passed[0] = false;
			    }
			    //System.err.println("parameters[0] = " + parameters[0] + " parameters[1] = " + parameters[1]);
			    if (passed[0]) {
			    	System.out.println(testName + " passed all tests");
			    }
			}
		
		// Note: These tests omit combinations of Wolfe line search with bisection.
		// Due to an implementation quirk in Wolfe line search with bisection, there
		// are calls to re-evaluate an existing point with new_point = true. That
		// causes the (overly) strict tests to break, since they check the new_point
		// preconditions in an if-and-only-if way. Strictly speaking, if new_point =
		// true, the interface does not *require* that the point has changed; only that
		// if new_point = false, the same point is reused.
		//
		// Since the strict checking is useful to verify that there aren't missed
		// optimizations, omit tests of the Wolfe with bisection cases.

		// Wolfe with L-BFGS.
		public void EvaluationCallbackWithLineSearchMinimizerWolfeLbfgsCubic() {
	      // EvaluationCallbackWithLineSearchMinimizerWolfeLbfgsCubic() passed all tests
		  String testName = "EvaluationCallbackWithLineSearchMinimizerWolfeLbfgsCubic()";
		  boolean passed[] = new boolean[] {true};
		  WithLineSearchMinimizerImpl(LineSearchType.WOLFE, LineSearchDirectionType.LBFGS, LineSearchInterpolationType.CUBIC, testName, passed);
		}

		public void EvaluationCallbackWithLineSearchMinimizerWolfeLbfgsQuadratic() {
			  // EvaluationCallbackWithLineSearchMinimizerWolfeLbfgsQuadratic() passed all tests
			  String testName = "EvaluationCallbackWithLineSearchMinimizerWolfeLbfgsQuadratic()";
			  boolean passed[] = new boolean[] {true};
			  WithLineSearchMinimizerImpl(LineSearchType.WOLFE, LineSearchDirectionType.LBFGS, LineSearchInterpolationType.QUADRATIC, testName, passed);
		}
		
		// Wolfe with full BFGS.
		public void EvaluationCallbackWithLineSearchMinimizerWolfeBfgsCubic() {
			// EvaluationCallbackWithLineSearchMinimizerWolfeBfgsCubic() passed all tests
			String testName = "EvaluationCallbackWithLineSearchMinimizerWolfeBfgsCubic()";
			boolean passed[] = new boolean[] {true};
			WithLineSearchMinimizerImpl(LineSearchType.WOLFE, LineSearchDirectionType.BFGS, LineSearchInterpolationType.CUBIC, testName, passed);
		}

		public void EvaluationCallbackWithLineSearchMinimizerWolfeBfgsQuadratic() {
			  // EvaluationCallbackWithLineSearchMinimizerWolfeBfgsQuadratic() passed all tests
			  String testName = "EvaluationCallbackWithLineSearchMinimizerWolfeBfgsQuadratic()";
			  boolean passed[] = new boolean[] {true};
			  WithLineSearchMinimizerImpl(LineSearchType.WOLFE, LineSearchDirectionType.BFGS, LineSearchInterpolationType.QUADRATIC, testName, passed);
		}
		
		// Armijo with nonlinear conjugate gradient.
		public void EvaluationCallbackWithLineSearchMinimizerArmijoCubic() {
			// EvaluationCallbackWithLineSearchMinimizerArmijoCubic() passed all tests
			String testName = "EvaluationCallbackWithLineSearchMinimizerArmijoCubic()";
			boolean passed[] = new boolean[] {true};
			WithLineSearchMinimizerImpl(LineSearchType.ARMIJO, LineSearchDirectionType.NONLINEAR_CONJUGATE_GRADIENT, LineSearchInterpolationType.CUBIC, testName, passed);
		}

		public void EvaluationCallbackWithLineSearchMinimizerArmijoBisection() {
			// EvaluationCallbackWithLineSearchMinimizerArmijoBisection() passed all tests
			String testName = "EvaluationCallbackWithLineSearchMinimizerArmijoBisection()";
			boolean passed[] = new boolean[] {true};
			WithLineSearchMinimizerImpl(LineSearchType.ARMIJO, LineSearchDirectionType.NONLINEAR_CONJUGATE_GRADIENT, LineSearchInterpolationType.BISECTION, testName, passed);
		}
		
		public void EvaluationCallbackWithLineSearchMinimizerArmijoQuadratic() {
			// EvaluationCallbackWithLineSearchMinimizerArmijoQuadratic() passed all tests
			String testName = "EvaluationCallbackWithLineSearchMinimizerArmijoQuadratic()";
			boolean passed[] = new boolean[] {true};
			WithLineSearchMinimizerImpl(LineSearchType.ARMIJO, LineSearchDirectionType.NONLINEAR_CONJUGATE_GRADIENT, LineSearchInterpolationType.QUADRATIC, testName, passed);
		}

		public void NumericDiffCostFunctionEasyCaseFunctorCentralDifferences() {
			  // NumericDiffCostFunctionEasyCaseFunctorCentralDifferences() passed all tests
			  String testName = "NumericDiffCostFunctionEasyCaseFunctorCentralDifferences()";
			  boolean passed[] = new boolean[] {true};
			  testCase = EASY_FUNCTOR_EXAMPLE;
			  EasyFunctor ef = new EasyFunctor();
			  NumericDiffMethodType method = NumericDiffMethodType.CENTRAL;
			  Ownership ownership = Ownership.TAKE_OWNERSHIP;
			  NumericDiffOptions options = new NumericDiffOptions();
			  // 3 number of residuals
			  // 5 size of x1
			  // 5 size of x2
			  CostFunction cost_function = new NumericDiffCostFunction<EasyFunctor>(ef, method, ownership, options, 3, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0);
			  EasyFunctor functor = new EasyFunctor();
			  functor.ExpectCostFunctionEvaluationIsNearlyCorrect(cost_function, NumericDiffMethodType.CENTRAL, testName, passed);
		}
		
		public void NumericDiffCostFunctionEasyCaseFunctorForwardDifferences() {
			  // NumericDiffCostFunctionEasyCaseFunctorForwardDifferences() passed all tests
			  String testName = "NumericDiffCostFunctionEasyCaseFunctorForwardDifferences()";
			  boolean passed[] = new boolean[] {true};
			  testCase = EASY_FUNCTOR_EXAMPLE;
			  EasyFunctor ef = new EasyFunctor();
			  NumericDiffMethodType method = NumericDiffMethodType.FORWARD;
			  Ownership ownership = Ownership.TAKE_OWNERSHIP;
			  NumericDiffOptions options = new NumericDiffOptions();
			  // 3 number of residuals
			  // 5 size of x1
			  // 5 size of x2
			  CostFunction cost_function = new NumericDiffCostFunction<EasyFunctor>(ef, method, ownership, options, 3, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0);
			  EasyFunctor functor = new EasyFunctor();
			  functor.ExpectCostFunctionEvaluationIsNearlyCorrect(cost_function, NumericDiffMethodType.FORWARD, testName, passed);
		}

	public void NumericDiffCostFunctionEasyCaseFunctorRidders() {
		  // NumericDiffCostFunctionEasyCaseFunctorRidders() passed all tests
		  String testName = "NumericDiffCostFunctionEasyCaseFunctorRidders()";
		  boolean passed[] = new boolean[] {true};
		  testCase = EASY_FUNCTOR_EXAMPLE;
		  EasyFunctor ef = new EasyFunctor();
		  NumericDiffMethodType method = NumericDiffMethodType.RIDDERS;
		  Ownership ownership = Ownership.TAKE_OWNERSHIP;
		  NumericDiffOptions options = new NumericDiffOptions();
		  // 3 number of residuals
		  // 5 size of x1
		  // 5 size of x2
		  CostFunction cost_function = new NumericDiffCostFunction<EasyFunctor>(ef, method, ownership, options, 3, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0);
		  EasyFunctor functor = new EasyFunctor();
		  functor.ExpectCostFunctionEvaluationIsNearlyCorrect(cost_function, NumericDiffMethodType.RIDDERS, testName, passed);
	}
	
	public void NumericDiffCostFunctionEasyCaseCostFunctionCentralDifferences() {
		  // NumericDiffCostFunctionEasyCaseCostFunctionCentralDifferences() passed all tests
		  String testName = "NumericDiffCostFunctionEasyCaseCostFunctionCentralDifferences()";
		  boolean passed[] = new boolean[] {true};
		  testCase = EASY_COST_FUNCTION;
		  EasyCostFunction ef = new EasyCostFunction();
		  NumericDiffMethodType method = NumericDiffMethodType.CENTRAL;
		  Ownership ownership = Ownership.TAKE_OWNERSHIP;
		  NumericDiffOptions options = new NumericDiffOptions();
		  // 3 number of residuals
		  // 5 size of x1
		  // 5 size of x2
		  CostFunction cost_function = new NumericDiffCostFunction<EasyCostFunction>(ef, method, ownership, options, 3, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0);
		  EasyFunctor functor = new EasyFunctor();
		  functor.ExpectCostFunctionEvaluationIsNearlyCorrect(cost_function, NumericDiffMethodType.CENTRAL, testName, passed);
	}
	
	public void NumericDiffCostFunctionEasyCaseCostFunctionForwardDifferences() {
		  // NumericDiffCostFunctionEasyCaseCostFunctionForwardDifferences() passed all tests
		  String testName = "NumericDiffCostFunctionEasyCaseCostFunctionForwardDifferences()";
		  boolean passed[] = new boolean[] {true};
		  testCase = EASY_COST_FUNCTION;
		  EasyCostFunction ef = new EasyCostFunction();
		  NumericDiffMethodType method = NumericDiffMethodType.FORWARD;
		  Ownership ownership = Ownership.TAKE_OWNERSHIP;
		  NumericDiffOptions options = new NumericDiffOptions();
		  // 3 number of residuals
		  // 5 size of x1
		  // 5 size of x2
		  CostFunction cost_function = new NumericDiffCostFunction<EasyCostFunction>(ef, method, ownership, options, 3, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0);
		  EasyFunctor functor = new EasyFunctor();
		  functor.ExpectCostFunctionEvaluationIsNearlyCorrect(cost_function, NumericDiffMethodType.FORWARD, testName, passed);
	}
	
	public void NumericDiffCostFunctionEasyCaseCostFunctionRidders() {
		  // NumericDiffCostFunctionEasyCaseCostFunctionRidders() passed all tests
		  String testName = "NumericDiffCostFunctionEasyCaseCostFunctionRidders()";
		  boolean passed[] = new boolean[] {true};
		  testCase = EASY_COST_FUNCTION;
		  EasyCostFunction ef = new EasyCostFunction();
		  NumericDiffMethodType method = NumericDiffMethodType.RIDDERS;
		  Ownership ownership = Ownership.TAKE_OWNERSHIP;
		  NumericDiffOptions options = new NumericDiffOptions();
		  // 3 number of residuals
		  // 5 size of x1
		  // 5 size of x2
		  CostFunction cost_function = new NumericDiffCostFunction<EasyCostFunction>(ef, method, ownership, options, 3, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0);
		  EasyFunctor functor = new EasyFunctor();
		  functor.ExpectCostFunctionEvaluationIsNearlyCorrect(cost_function, NumericDiffMethodType.RIDDERS, testName, passed);
	}
	
	public void NumericDiffCostFunctionTranscendentalCaseFunctorCentralDifferences() {
		// NumericDiffCostFunctionTranscendentalCaseFunctorCentralDifferences() passed all tests
		String testName = "NumericDiffCostFunctionTranscendentalCaseFunctorCentralDifferences()";
		boolean passed[] = new boolean[] {true};
		  testCase = TRANSCENDENTAL_FUNCTOR;
		  TranscendentalFunctor tf = new TranscendentalFunctor();
		  NumericDiffMethodType method = NumericDiffMethodType.CENTRAL;
		  Ownership ownership = Ownership.TAKE_OWNERSHIP;
		  NumericDiffOptions options = new NumericDiffOptions();
		  // 2 number of residuals
		  // 5 size of x1
		  // 5 size of x2
		  CostFunction cost_function = new NumericDiffCostFunction<TranscendentalFunctor>(tf, method, ownership, options, 2, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0);
		  TranscendentalFunctor functor = new TranscendentalFunctor();
		  functor.ExpectCostFunctionEvaluationIsNearlyCorrect(cost_function, NumericDiffMethodType.CENTRAL, testName, passed);
    }

	public void NumericDiffCostFunctionTranscendentalCaseFunctorForwardDifferences() {
		// NumericDiffCostFunctionTranscendentalCaseFunctorForwardDifferences() passed all tests
		String testName = "NumericDiffCostFunctionTranscendentalCaseFunctorForwardDifferences()";
		boolean passed[] = new boolean[] {true};
		  testCase = TRANSCENDENTAL_FUNCTOR;
		  TranscendentalFunctor tf = new TranscendentalFunctor();
		  NumericDiffMethodType method = NumericDiffMethodType.FORWARD;
		  Ownership ownership = Ownership.TAKE_OWNERSHIP;
		  NumericDiffOptions options = new NumericDiffOptions();
		  // 2 number of residuals
		  // 5 size of x1
		  // 5 size of x2
		  CostFunction cost_function = new NumericDiffCostFunction<TranscendentalFunctor>(tf, method, ownership, options, 2, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0);
		  TranscendentalFunctor functor = new TranscendentalFunctor();
		  functor.ExpectCostFunctionEvaluationIsNearlyCorrect(cost_function, NumericDiffMethodType.FORWARD, testName, passed);
    }
	
	public void NumericDiffCostFunctionTranscendentalCaseFunctorRidders() {
		// NumericDiffCostFunctionTranscendentalCaseFunctorRidders() passed all tests
		String testName = "NumericDiffCostFunctionTranscendentalCaseFunctorRidders()";
		boolean passed[] = new boolean[] {true};
		  testCase = TRANSCENDENTAL_FUNCTOR;
		  TranscendentalFunctor tf = new TranscendentalFunctor();
		  NumericDiffMethodType method = NumericDiffMethodType.RIDDERS;
		  Ownership ownership = Ownership.TAKE_OWNERSHIP;
		  NumericDiffOptions options = new NumericDiffOptions();
		  // Using a smaller initial step size to overcome oscillatory function
		  // behavior.
		  options.ridders_relative_initial_step_size = 1e-3;
		  // 2 number of residuals
		  // 5 size of x1
		  // 5 size of x2
		  CostFunction cost_function = new NumericDiffCostFunction<TranscendentalFunctor>(tf, method, ownership, options, 2, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0);
		  TranscendentalFunctor functor = new TranscendentalFunctor();
		  functor.ExpectCostFunctionEvaluationIsNearlyCorrect(cost_function, NumericDiffMethodType.RIDDERS, testName, passed);
    }
	
	public void NumericDiffCostFunctionTranscendentalCaseCostFunctionCentralDifferences() {
		// NumericDiffCostFunctionTranscendentalCaseCostFunctionCentralDifferences() passed all tests
		String testName = "NumericDiffCostFunctionTranscendentalCaseCostFunctionCentralDifferences()";
		boolean passed[] = new boolean[] {true};
		  testCase = TRANSCENDENTAL_COST_FUNCTION;
		  TranscendentalCostFunction tf = new TranscendentalCostFunction();
		  NumericDiffMethodType method = NumericDiffMethodType.CENTRAL;
		  Ownership ownership = Ownership.TAKE_OWNERSHIP;
		  NumericDiffOptions options = new NumericDiffOptions();
		  // 2 number of residuals
		  // 5 size of x1
		  // 5 size of x2
		  CostFunction cost_function = new NumericDiffCostFunction<TranscendentalCostFunction>(tf, method, ownership, options, 2, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0);
		  TranscendentalFunctor functor = new TranscendentalFunctor();
		  functor.ExpectCostFunctionEvaluationIsNearlyCorrect(cost_function, NumericDiffMethodType.CENTRAL, testName, passed);
    }
	
	public void NumericDiffCostFunctionTranscendentalCaseCostFunctionForwardDifferences() {
		// NumericDiffCostFunctionTranscendentalCaseCostFunctionForwardDifferences() passed all tests
		String testName = "NumericDiffCostFunctionTranscendentalCaseCostFunctionForwardDifferences()";
		boolean passed[] = new boolean[] {true};
		  testCase = TRANSCENDENTAL_COST_FUNCTION;
		  TranscendentalCostFunction tf = new TranscendentalCostFunction();
		  NumericDiffMethodType method = NumericDiffMethodType.FORWARD;
		  Ownership ownership = Ownership.TAKE_OWNERSHIP;
		  NumericDiffOptions options = new NumericDiffOptions();
		  // 2 number of residuals
		  // 5 size of x1
		  // 5 size of x2
		  CostFunction cost_function = new NumericDiffCostFunction<TranscendentalCostFunction>(tf, method, ownership, options, 2, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0);
		  TranscendentalFunctor functor = new TranscendentalFunctor();
		  functor.ExpectCostFunctionEvaluationIsNearlyCorrect(cost_function, NumericDiffMethodType.FORWARD, testName, passed);
    }
	
	public void NumericDiffCostFunctionTranscendentalCaseCostFunctionRidders() {
		// NumericDiffCostFunctionTranscendentalCaseCostFunctionRidders() passed all tests
		String testName = "NumericDiffCostFunctionTranscendentalCaseCostFunctionRidders()";
		boolean passed[] = new boolean[] {true};
		  testCase = TRANSCENDENTAL_COST_FUNCTION;
		  TranscendentalCostFunction tf = new TranscendentalCostFunction();
		  NumericDiffMethodType method = NumericDiffMethodType.RIDDERS;
		  Ownership ownership = Ownership.TAKE_OWNERSHIP;
		  NumericDiffOptions options = new NumericDiffOptions();
		  // Using a smaller initial step size to overcome oscillatory function
		  // behavior.
		  options.ridders_relative_initial_step_size = 1e-3;
		  // 2 number of residuals
		  // 5 size of x1
		  // 5 size of x2
		  CostFunction cost_function = new NumericDiffCostFunction<TranscendentalCostFunction>(tf, method, ownership, options, 2, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0);
		  TranscendentalFunctor functor = new TranscendentalFunctor();
		  functor.ExpectCostFunctionEvaluationIsNearlyCorrect(cost_function, NumericDiffMethodType.RIDDERS, testName, passed);
    }

}