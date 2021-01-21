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
import gov.nih.mipav.model.algorithms.CeresSolver.TripletSparseMatrix;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue2;
import gov.nih.mipav.model.structures.jama.GeneralizedInverse2;
import gov.nih.mipav.model.structures.jama.LinearEquations;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.model.structures.jama.SVD;
import gov.nih.mipav.view.Preferences;

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
	    	System.err.println("In BlockRandomDIagonalMatrixTest m_.num_rows() != num_rows");
	    	passed = false;
	    }
	    if (m_.num_cols() != num_rows) {
	    	System.err.println("In BlockRandomDIagonalMatrixTest m_.num_cols() != num_rows");
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

	
}