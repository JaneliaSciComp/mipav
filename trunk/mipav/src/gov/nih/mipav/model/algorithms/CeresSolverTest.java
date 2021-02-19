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
	
	public CeresSolverTest() {
		super();
	}
	
	

	// A CostFunction implementing analytically derivatives for the
	// function f(x) = 10 - x.
	class QuadraticCostFunction extends SizedCostFunction {
		public QuadraticCostFunction() {
			// number of resdiuals
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

}