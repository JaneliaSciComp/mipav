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
		  SchurEliminatorTest SE = new SchurEliminatorTest();
		  SE.SchurEliminatorTestScalarProblemNoRegularization();
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
		  
		  public SchurEliminatorTest() {
			  
		  }
		  
		  public void SchurEliminatorTestScalarProblemNoRegularization() {
			   SetUpFromId(2);
			   double zero[] = new double[A.num_cols()];
			   String testName = "SchurEliminatorTestScalarProblemNoRegularization()";
			   ComputeReferenceSolution(zero);
			   EliminateSolveAndCompare(zero, true, 1e-14, testName);
			   EliminateSolveAndCompare(zero, false, 1e-14, testName);
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

		     lhs_expected = new Matrix(schur_size, schur_size);

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
		                                 double relative_tolerance,
		                                 String testName) {
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

		 

		 /*TEST_F(SchurEliminatorTest, ScalarProblemWithRegularization) {
		   SetUpFromId(2);
		   ComputeReferenceSolution(VectorRef(D.get(), A->num_cols()));
		   EliminateSolveAndCompare(VectorRef(D.get(), A->num_cols()), true, 1e-14);
		   EliminateSolveAndCompare(VectorRef(D.get(), A->num_cols()), false, 1e-14);
		 }

		 TEST_F(SchurEliminatorTest, VaryingFBlockSizeWithStaticStructure) {
		   SetUpFromId(4);
		   ComputeReferenceSolution(VectorRef(D.get(), A->num_cols()));
		   EliminateSolveAndCompare(VectorRef(D.get(), A->num_cols()), true, 1e-14);
		 }

		 TEST_F(SchurEliminatorTest, VaryingFBlockSizeWithoutStaticStructure) {
		   SetUpFromId(4);
		   ComputeReferenceSolution(VectorRef(D.get(), A->num_cols()));
		   EliminateSolveAndCompare(VectorRef(D.get(), A->num_cols()), false, 1e-14);
		 }*/


	  
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

     
   };

   





	
}