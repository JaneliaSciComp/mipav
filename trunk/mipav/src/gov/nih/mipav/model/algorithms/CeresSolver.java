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
import gov.nih.mipav.model.algorithms.CeresSolver.EvaluateOptions;
import gov.nih.mipav.model.algorithms.CeresSolver.SparseMatrix;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue2;
import gov.nih.mipav.model.structures.jama.GeneralizedInverse2;
import gov.nih.mipav.model.structures.jama.LinearEquations;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.model.structures.jama.SVD;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree.Pair;

/**
 * This is a port of the C++ files in ceres-solver-1.14.0 under the BSD license:
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
 * 
 * Ceres Solver is an open source C++ library for modeling and solving large,
 * complicated optimization problems. It is a feature rich, mature and
 * performant library which has been used in production at Google since 2010.
 * Ceres Solver can solve two kinds of problems.
 * 
 * 1. Non-linear Least Squares problems with bounds constraints. 2. General
 * unconstrained optimization problems.
 * 
 * Please see [ceres-solver.org](http://ceres-solver.org/) for more information.
 * 
 * 
 * 
 * @author aailb
 *
 */

public abstract class CeresSolver {
	private GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
	private GeneralizedEigenvalue2 ge2 = new GeneralizedEigenvalue2();
	private GeneralizedInverse2 gi2 = new GeneralizedInverse2();
	private LinearEquations le = new LinearEquations();
	private LinearEquations2 le2 = new LinearEquations2();
	private SVD svd = new SVD();
	private LinearSolverType requestedLinearSolverType = LinearSolverType.DENSE_QR;
	// It is a near impossibility that user code generates this exact
	// value in normal operation, thus we will use it to fill arrays
	// before passing them to user code. If on return an element of the
	// array still contains this value, we will assume that the user code
	// did not write to that memory location.
	public int MAX_LOG_LEVEL = 3;
	// Log severity level constants.
	private int FATAL   = -3;
	private int ERROR   = -2;
	private int WARNING = -1;
	private int INFO    =  0;
	// Each package in SuiteSparse has its own separate
	// CXSparse and CHOLMOD are SuiteSparse packages

	// CXSparse is free software; you can redistribute it and/or
	// modify it under the terms of the GNU Lesser General Public
	// License as published by the Free Software Foundation; either
	// version 2.1 of the License, or (at your option) any later version.

	// CHOLMOD/Include/* files. Copyright (C) 2005-2006, either Univ. of Florida
	// or T. Davis, depending on the file.
	// Each file is licensed separately, according to the Module for which it
	// contains definitions and prototypes:
	// Include/cholmod.h LGPL
	// Include/cholmod_blas.h LGPL
	// Include/cholmod_camd.h part of Partition module
	// Include/cholmod_check.h part of Check module
	// Include/cholmod_cholesky.h part of Cholesky module
	// Include/cholmod_complexity.h LGPL
	// Include/cholmod_config.h LGPL
	// Include/cholmod_core.h part of Core module
	// Include/cholmod_function.h no license; freely usable, no restrictions
	// Include/cholmod_gpu.h part of GPU module
	// Include/cholmod_gpu_kernels.h part of GPU module
	// Include/cholmod_internal.h LGPL
	// Include/cholmod_io64.h LGPL
	// Include/cholmod_matrixops.h part of MatrixOps module
	// Include/cholmod_modify.h part of Modify module
	// Include/cholmod_partition.h part of Partition module
	// Include/cholmod_supernodal.h part of Supernodal module
	// Include/cholmod_template.h LGPL

	// SuiteSparse licensing not possible
	private boolean CERES_NO_SUITESPARSE = true;
	private boolean CERES_NO_CXSPARSE = true;
	private int SUITESPARSE_VERSION = -1;
	// Eigen is Free Software. Starting from the 3.1.1 version, it is licensed under
	// the MPL2,
	// which is a simple weak copyleft license. Common questions about the MPL2 are
	// answered in the official MPL2 FAQ.
	// Earlier versions were licensed under the LGPL3+.
	// Note that currently, one feature relies on third-party code licensed under
	// the LGPL: SimplicialCholesky, constrained_cg, which is not used by CeresSolver. 
	// Such features can be explicitly disabled by
	// compiling with the EIGEN_MPL2_ONLY
	// preprocessor symbol defined. Furthermore, Eigen provides interface classes
	// for various third-party libraries
	// (usually recognizable by the <Eigen/*Support> header name). Of course you
	// have to mind the license of the so-included library when using them.
	// Virtually any software may use Eigen. For example, closed-source software may
	// use Eigen without having to disclose its own
	// source code. Many proprietary and closed-source software projects are using
	// Eigen right now, as well as many BSD-licensed projects.
	// See the MPL2 FAQ for more information, and do not hesitate to contact us if
	// you have any questions.
	// Licensing looks possible but EIGEN SPARSE library not currently implemented
	private boolean CERES_USE_EIGEN_SPARSE = false;
	private final double kImpossibleValue = 1e302;
	private boolean testMode = false;
	double epsilon = 2.2204460e-16;
	double default_relstep = Math.pow(epsilon, 1.0 / 3.0);
	/** integer scalar containing the number of data points. */
	protected int m;
	private int num_residuals_;

	/** variables integer scalar containing the number of unknowns. */
	protected int N0; // x.length
	// For SizedCostFunction and AutoDiffCostFunction, DYNAMIC can be
	// specified for the number of residuals. If specified, then the
	// number of residuas for that cost function can vary at runtime.
	// enum DimensionType {
	protected int DYNAMIC = -1;
	// };
	private final double kLBFGSSecantConditionHessianUpdateTolerance = 1e-14;


	// Argument type used in interfaces that can optionally take ownership
	// of a passed in argument. If TAKE_OWNERSHIP is passed, the called
	// object takes ownership of the pointer argument, and will call
	// delete on it upon completion.
	enum Ownership {
		DO_NOT_TAKE_OWNERSHIP, TAKE_OWNERSHIP
	};

	enum MinimizerType {
		LINE_SEARCH, TRUST_REGION
	};

	enum LineSearchDirectionType {
		// Negative of the gradient.
		STEEPEST_DESCENT,

		// A generalization of the Conjugate Gradient method to non-linear
		// functions. The generalization can be performed in a number of
		// different ways, resulting in a variety of search directions. The
		// precise choice of the non-linear conjugate gradient algorithm
		// used is determined by NonlinerConjuateGradientType.
		NONLINEAR_CONJUGATE_GRADIENT,

		// BFGS, and it's limited memory approximation L-BFGS, are quasi-Newton
		// algorithms that approximate the Hessian matrix by iteratively refining
		// an initial estimate with rank-one updates using the gradient at each
		// iteration. They are a generalisation of the Secant method and satisfy
		// the Secant equation. The Secant equation has an infinium of solutions
		// in multiple dimensions, as there are N*(N+1)/2 degrees of freedom in a
		// symmetric matrix but only N conditions are specified by the Secant
		// equation. The requirement that the Hessian approximation be positive
		// definite imposes another N additional constraints, but that still leaves
		// remaining degrees-of-freedom. (L)BFGS methods uniquely deteremine the
		// approximate Hessian by imposing the additional constraints that the
		// approximation at the next iteration must be the 'closest' to the current
		// approximation (the nature of how this proximity is measured is actually
		// the defining difference between a family of quasi-Newton methods including
		// (L)BFGS & DFP). (L)BFGS is currently regarded as being the best known
		// general quasi-Newton method.
		//
		// The principal difference between BFGS and L-BFGS is that whilst BFGS
		// maintains a full, dense approximation to the (inverse) Hessian, L-BFGS
		// maintains only a window of the last M observations of the parameters and
		// gradients. Using this observation history, the calculation of the next
		// search direction can be computed without requiring the construction of the
		// full dense inverse Hessian approximation. This is particularly important
		// for problems with a large number of parameters, where storage of an N-by-N
		// matrix in memory would be prohibitive.
		//
		// For more details on BFGS see:
		//
		// Broyden, C.G., "The Convergence of a Class of Double-rank Minimization
		// Algorithms,"; J. Inst. Maths. Applics., Vol. 6, pp 76-90, 1970.
		//
		// Fletcher, R., "A New Approach to Variable Metric Algorithms,"
		// Computer Journal, Vol. 13, pp 317-322, 1970.
		//
		// Goldfarb, D., "A Family of Variable Metric Updates Derived by Variational
		// Means," Mathematics of Computing, Vol. 24, pp 23-26, 1970.
		//
		// Shanno, D.F., "Conditioning of Quasi-Newton Methods for Function
		// Minimization," Mathematics of Computing, Vol. 24, pp 647-656, 1970.
		//
		// For more details on L-BFGS see:
		//
		// Nocedal, J. (1980). "Updating Quasi-Newton Matrices with Limited
		// Storage". Mathematics of Computation 35 (151): 773-782.
		//
		// Byrd, R. H.; Nocedal, J.; Schnabel, R. B. (1994).
		// "Representations of Quasi-Newton Matrices and their use in
		// Limited Memory Methods". Mathematical Programming 63 (4):
		// 129-156.
		//
		// A general reference for both methods:
		//
		// Nocedal J., Wright S., Numerical Optimization, 2nd Ed. Springer, 1999.
		LBFGS, BFGS,
	};

	enum LineSearchType {
		// Backtracking line search with polynomial interpolation or
		// bisection.
		ARMIJO, WOLFE,
	};

	// Nonliner conjugate gradient methods are a generalization of the
	// method of Conjugate Gradients for linear systems. The
	// generalization can be carried out in a number of different ways
	// leading to number of different rules for computing the search
	// direction. Ceres provides a number of different variants. For more
	// details see Numerical Optimization by Nocedal & Wright.
	enum NonlinearConjugateGradientType {
		FLETCHER_REEVES, POLAK_RIBIERE, HESTENES_STIEFEL,
	};

	// TODO(keir): Considerably expand the explanations of each solver type.

	enum LinearSolverType {
		// These solvers are for general rectangular systems formed from the
		// normal equations A'A x = A'b. They are direct solvers and do not
		// assume any special problem structure.

		// Solve the normal equations using a dense Cholesky solver; based
		// on Eigen or LAPACK.
		DENSE_NORMAL_CHOLESKY,

		// Solve the normal equations using a dense QR solver; based on
		// Eigen or LAPACK
		DENSE_QR,

		// Solve the normal equations using a sparse cholesky solver; requires
		// SuiteSparse or CXSparse.
		SPARSE_NORMAL_CHOLESKY,

		// Specialized solvers, specific to problems with a generalized
		// bi-partitite structure.

		// Solves the reduced linear system using a dense Cholesky solver;
		// based on Eigen.
		// DenseSchurComplementSolver uses Eigen's Cholesky factorization.
		DENSE_SCHUR,

		// Solves the reduced linear system using a sparse Cholesky solver;
		// based on CHOLMOD.
		// This requires that you build Ceres with support for SuiteSparse, CXSparse or
		// Eigen's sparse linear algebra libraries.
		SPARSE_SCHUR,

		// Solves the reduced linear system using Conjugate Gradients, based
		// on a new Ceres implementation. Suitable for large scale
		// problems.
		ITERATIVE_SCHUR,

		// Conjugate gradients on the normal equations.
		CGNR
	};

	enum PreconditionerType {
		// Trivial preconditioner - the identity matrix.
		IDENTITY,

		// Block diagonal of the Gauss-Newton Hessian.
		JACOBI,

		// Note: The following three preconditioners can only be used with
		// the ITERATIVE_SCHUR solver. They are well suited for Structure
		// from Motion problems.

		// Block diagonal of the Schur complement. This preconditioner may
		// only be used with the ITERATIVE_SCHUR solver.
		SCHUR_JACOBI,

		// Visibility clustering based preconditioners.
		//
		// The following two preconditioners use the visibility structure of
		// the scene to determine the sparsity structure of the
		// preconditioner. This is done using a clustering algorithm. The
		// available visibility clustering algorithms are described below.
		//
		// Note: Requires SuiteSparse.
		CLUSTER_JACOBI, CLUSTER_TRIDIAGONAL
	};

	enum VisibilityClusteringType {
		// Canonical views algorithm as described in
		//
		// "Scene Summarization for Online Image Collections", Ian Simon, Noah
		// Snavely, Steven M. Seitz, ICCV 2007.
		//
		// This clustering algorithm can be quite slow, but gives high
		// quality clusters. The original visibility based clustering paper
		// used this algorithm.
		CANONICAL_VIEWS,

		// The classic single linkage algorithm. It is extremely fast as
		// compared to CANONICAL_VIEWS, but can give slightly poorer
		// results. For problems with large number of cameras though, this
		// is generally a pretty good option.
		//
		// If you are using SCHUR_JACOBI preconditioner and have SuiteSparse
		// available, CLUSTER_JACOBI and CLUSTER_TRIDIAGONAL in combination
		// with the SINGLE_LINKAGE algorithm will generally give better
		// results.
		SINGLE_LINKAGE
	};

	enum SparseLinearAlgebraLibraryType {
		// High performance sparse Cholesky factorization and approximate
		// minimum degree ordering.
		SUITE_SPARSE,

		// A lightweight replacment for SuiteSparse, which does not require
		// a LAPACK/BLAS implementation. Consequently, its performance is
		// also a bit lower than SuiteSparse.
		CX_SPARSE,

		// Eigen's sparse linear algebra routines. In particular Ceres uses
		// the Simplicial LDLT routines.
		EIGEN_SPARSE,

		// No sparse linear solver should be used. This does not necessarily
		// imply that Ceres was built without any sparse library, although that
		// is the likely use case, merely that one should not be used.
		NO_SPARSE
	};

	enum DenseLinearAlgebraLibraryType {
		EIGEN, LAPACK
	};

	// Logging options
	// The options get progressively noisier.
	enum LoggingType {
		SILENT, PER_MINIMIZER_ITERATION
	};

	// Ceres supports different strategies for computing the trust region
	// step.
	enum TrustRegionStrategyType {
		// The default trust region strategy is to use the step computation
		// used in the Levenberg-Marquardt algorithm. For more details see
		// levenberg_marquardt_strategy.h
		LEVENBERG_MARQUARDT,

		// Powell's dogleg algorithm interpolates between the Cauchy point
		// and the Gauss-Newton step. It is particularly useful if the
		// LEVENBERG_MARQUARDT algorithm is making a large number of
		// unsuccessful steps. For more details see dogleg_strategy.h.
		//
		// NOTES:
		//
		// 1. This strategy has not been experimented with or tested as
		// extensively as LEVENBERG_MARQUARDT, and therefore it should be
		// considered EXPERIMENTAL for now.
		//
		// 2. For now this strategy should only be used with exact
		// factorization based linear solvers, i.e., SPARSE_SCHUR,
		// DENSE_SCHUR, DENSE_QR and SPARSE_NORMAL_CHOLESKY.
		DOGLEG
	};

	// Ceres supports two different dogleg strategies.
	// The "traditional" dogleg method by Powell and the
	// "subspace" method described in
	// R. H. Byrd, R. B. Schnabel, and G. A. Shultz,
	// "Approximate solution of the trust region problem by minimization
	// over two-dimensional subspaces", Mathematical Programming,
	// 40 (1988), pp. 247--263
	enum DoglegType {
		// The traditional approach constructs a dogleg path
		// consisting of two line segments and finds the furthest
		// point on that path that is still inside the trust region.
		TRADITIONAL_DOGLEG,

		// The subspace approach finds the exact minimum of the model
		// constrained to the subspace spanned by the dogleg path.
		SUBSPACE_DOGLEG
	};

	enum TerminationType {
		// Minimizer terminated because one of the convergence criterion set
		// by the user was satisfied.
		//
		// 1. (new_cost - old_cost) < function_tolerance * old_cost;
		// 2. max_i |gradient_i| < gradient_tolerance
		// 3. |step|_2 <= parameter_tolerance * ( |x|_2 + parameter_tolerance)
		//
		// The user's parameter blocks will be updated with the solution.
		CONVERGENCE,

		// The solver ran for maximum number of iterations or maximum amount
		// of time specified by the user, but none of the convergence
		// criterion specified by the user were met. The user's parameter
		// blocks will be updated with the solution found so far.
		NO_CONVERGENCE,

		// The minimizer terminated because of an error. The user's
		// parameter blocks will not be updated.
		FAILURE,

		// Using an IterationCallback object, user code can control the
		// minimizer. The following enums indicate that the user code was
		// responsible for termination.
		//
		// Minimizer terminated successfully because a user
		// IterationCallback returned SOLVER_TERMINATE_SUCCESSFULLY.
		//
		// The user's parameter blocks will be updated with the solution.
		USER_SUCCESS,

		// Minimizer terminated because because a user IterationCallback
		// returned SOLVER_ABORT.
		//
		// The user's parameter blocks will not be updated.
		USER_FAILURE
	};

	// Enums used by the IterationCallback instances to indicate to the
	// solver whether it should continue solving, the user detected an
	// error or the solution is good enough and the solver should
	// terminate.
	enum CallbackReturnType {
		// Continue solving to next iteration.
		SOLVER_CONTINUE,

		// Terminate solver, and do not update the parameter blocks upon
		// return. Unless the user has set
		// SolverOptions:::update_state_every_iteration, in which case the
		// state would have been updated every iteration
		// anyways. Solver::Summary::termination_type is set to USER_ABORT.
		SOLVER_ABORT,

		// Terminate solver, update state and
		// return. Solver::Summary::termination_type is set to USER_SUCCESS.
		SOLVER_TERMINATE_SUCCESSFULLY
	};

	// The format in which linear least squares problems should be logged
	// when Solver::Options::lsqp_iterations_to_dump is non-empty.
	enum DumpFormatType {
		// Print the linear least squares problem in a human readable format
		// to stderr. The Jacobian is printed as a dense matrix. The vectors
		// D, x and f are printed as dense vectors. This should only be used
		// for small problems.
		CONSOLE,

		// Write out the linear least squares problem to the directory
		// pointed to by Solver::Options::lsqp_dump_directory as text files
		// which can be read into MATLAB/Octave. The Jacobian is dumped as a
		// text file containing (i,j,s) triplets, the vectors D, x and f are
		// dumped as text files containing a list of their values.
		//
		// A MATLAB/octave script called lm_iteration_???.m is also output,
		// which can be used to parse and load the problem into memory.
		TEXTFILE
	};

	// The differentiation method used to compute numerical derivatives in
	// NumericDiffCostFunction and DynamicNumericDiffCostFunction.
	enum NumericDiffMethodType {
		// Compute central finite difference: f'(x) ~ (f(x+h) - f(x-h)) / 2h.
		CENTRAL,

		// Compute forward finite difference: f'(x) ~ (f(x+h) - f(x)) / h.
		FORWARD,

		// Adaptive numerical differentiation using Ridders' method. Provides more
		// accurate and robust derivatives at the expense of additional cost
		// function evaluations.
		RIDDERS
	};

	enum LineSearchInterpolationType {
		BISECTION, QUADRATIC, CUBIC
	};

	enum CovarianceAlgorithmType {
		DENSE_SVD, SPARSE_QR,
	};

	enum LinearSolverTerminationType {
		// Termination criterion was met.
		LINEAR_SOLVER_SUCCESS,

		// Solver ran for max_num_iterations and terminated before the
		// termination tolerance could be satisfied.
		LINEAR_SOLVER_NO_CONVERGENCE,

		// Solver was terminated due to numerical problems, generally due to
		// the linear system being poorly conditioned.
		LINEAR_SOLVER_FAILURE,

		// Solver failed with a fatal error that cannot be recovered from,
		// e.g. CHOLMOD ran out of memory when computing the symbolic or
		// numeric factorization or an underlying library was called with
		// the wrong arguments.
		LINEAR_SOLVER_FATAL_ERROR
	};
	
	protected int testCase;
	protected final int COST_FUNCTOR_EXAMPLE = 1;
	protected final int CURVE_FITTING_EXAMPLE = 2;
	protected boolean optionsValid = true;
	
	class CostFunctorExample {
		public CostFunctorExample() {

		}

		public boolean operator(double x[], double residual[]) {
			residual[0] = 10.0 - x[0];
			return true;
		}
	};

	class CurveFittingFunctorExample {
		public CurveFittingFunctorExample() {

		}

		public boolean operator(double x[], double residual[]) {
			int i;
			for (i = 0; i < curveFittingObservations; i++) {
				double exp = Math.exp(x[1] * curveFittingData[2*i] + x[0]);
			    residual[i] = curveFittingData[2*i+1] - exp;
			}

			return true;
		}
	};
	
	protected final int curveFittingObservations = 67;
	protected double curveFittingData[] = {
	  0.000000e+00, 1.133898e+00,
	  7.500000e-02, 1.334902e+00,
	  1.500000e-01, 1.213546e+00,
	  2.250000e-01, 1.252016e+00,
	  3.000000e-01, 1.392265e+00,
	  3.750000e-01, 1.314458e+00,
	  4.500000e-01, 1.472541e+00,
	  5.250000e-01, 1.536218e+00,
	  6.000000e-01, 1.355679e+00,
	  6.750000e-01, 1.463566e+00,
	  7.500000e-01, 1.490201e+00,
	  8.250000e-01, 1.658699e+00,
	  9.000000e-01, 1.067574e+00,
	  9.750000e-01, 1.464629e+00,
	  1.050000e+00, 1.402653e+00,
	  1.125000e+00, 1.713141e+00,
	  1.200000e+00, 1.527021e+00,
	  1.275000e+00, 1.702632e+00,
	  1.350000e+00, 1.423899e+00,
	  1.425000e+00, 1.543078e+00,
	  1.500000e+00, 1.664015e+00,
	  1.575000e+00, 1.732484e+00,
	  1.650000e+00, 1.543296e+00,
	  1.725000e+00, 1.959523e+00,
	  1.800000e+00, 1.685132e+00,
	  1.875000e+00, 1.951791e+00,
	  1.950000e+00, 2.095346e+00,
	  2.025000e+00, 2.361460e+00,
	  2.100000e+00, 2.169119e+00,
	  2.175000e+00, 2.061745e+00,
	  2.250000e+00, 2.178641e+00,
	  2.325000e+00, 2.104346e+00,
	  2.400000e+00, 2.584470e+00,
	  2.475000e+00, 1.914158e+00,
	  2.550000e+00, 2.368375e+00,
	  2.625000e+00, 2.686125e+00,
	  2.700000e+00, 2.712395e+00,
	  2.775000e+00, 2.499511e+00,
	  2.850000e+00, 2.558897e+00,
	  2.925000e+00, 2.309154e+00,
	  3.000000e+00, 2.869503e+00,
	  3.075000e+00, 3.116645e+00,
	  3.150000e+00, 3.094907e+00,
	  3.225000e+00, 2.471759e+00,
	  3.300000e+00, 3.017131e+00,
	  3.375000e+00, 3.232381e+00,
	  3.450000e+00, 2.944596e+00,
	  3.525000e+00, 3.385343e+00,
	  3.600000e+00, 3.199826e+00,
	  3.675000e+00, 3.423039e+00,
	  3.750000e+00, 3.621552e+00,
	  3.825000e+00, 3.559255e+00,
	  3.900000e+00, 3.530713e+00,
	  3.975000e+00, 3.561766e+00,
	  4.050000e+00, 3.544574e+00,
	  4.125000e+00, 3.867945e+00,
	  4.200000e+00, 4.049776e+00,
	  4.275000e+00, 3.885601e+00,
	  4.350000e+00, 4.110505e+00,
	  4.425000e+00, 4.345320e+00,
	  4.500000e+00, 4.161241e+00,
	  4.575000e+00, 4.363407e+00,
	  4.650000e+00, 4.161576e+00,
	  4.725000e+00, 4.619728e+00,
	  4.800000e+00, 4.737410e+00,
	  4.875000e+00, 4.727863e+00,
	  4.950000e+00, 4.669206e+00,
	};

	public void Solve(SolverOptions options, ProblemImpl problem, SolverSummary summary) {
		if (problem == null) {
			System.err.println("ProblemImpl problem is null in Solve");
			return;
		}
		if (summary == null) {
			System.err.println("SolverSummary summary is null in Solve");
			return;
		}
		double start_time = 1.0E-3 * System.currentTimeMillis();
		//Summary();
		if (!options.IsValid(summary.message)) {
			System.err.println("Terminating: " + summary.message[0]);
			optionsValid = false;
			return;
		}

		ProblemImpl problem_impl = problem;
		Program program = problem_impl.mutable_program();
		PreSolveSummarize(options, problem_impl, summary);

		// The main thread also does work so we only need to launch num_threads - 1.
		// When compiled with C++11 threading support, resize the thread pool to have
		// at min(num_thread, num_hardware_threads) where num_hardware_threads is
		// defined by the hardware. Otherwise this call is a no-op.
		// problem_impl.context().EnsureMinimumThreads(options.num_threads - 1);

		// Make sure that all the parameter blocks states are set to the
		// values provided by the user.
		// Set the parameter block pointers to the user pointers. Since this
		// runs parameter block set state internally, which may call local
		// parameterizations, this can fail. False is returned on failure.
		program.SetParameterBlockStatePtrsToUserStatePtrs();

		// If gradient_checking is enabled, wrap all cost functions in a
		// gradient checker and install a callback that terminates if any gradient
		// error is detected.
		// scoped_ptr<internal::ProblemImpl> gradient_checking_problem;
		ProblemImpl gradient_checking_problem = new ProblemImpl();
		GradientCheckingIterationCallback gradient_checking_callback = new GradientCheckingIterationCallback();
		SolverOptions modified_options = options;
		// options.check_gradients = true;
		if (options.check_gradients) {
			modified_options.callbacks.add(gradient_checking_callback);
			gradient_checking_problem = CreateGradientCheckingProblemImpl(problem_impl,
					options.gradient_check_numeric_derivative_relative_step_size,
					options.gradient_check_relative_precision, gradient_checking_callback);
			problem_impl = gradient_checking_problem;
			program = problem_impl.mutable_program();
		}

		// scoped_ptr<Preprocessor> preprocessor(
		// Preprocessor::Create(modified_options.minimizer_type));
		Preprocessor preprocessor = CreatePreprocessor(modified_options.minimizer_type);
		PreprocessedProblem pp = new PreprocessedProblem();

		boolean status = preprocessor.Preprocess(modified_options, problem_impl, pp);
		// solver type.
		if (IsSchurType(pp.linear_solver_options.type)) {
			System.out.println("Schur type");
			// TODO(sameeragarwal): We can likely eliminate the duplicate call
			// to DetectStructure here and inside the linear solver, by
			// calling this in the preprocessor.
			int row_block_size[] = new int[1];
			int e_block_size[] = new int[1];
			int f_block_size[] = new int[1];
			
			DetectStructure(((BlockSparseMatrix)(
                    pp.minimizer_options.jacobian))
                .block_structure(),
                pp.linear_solver_options.elimination_groups.get(0),
                row_block_size,
                e_block_size,
                f_block_size);
			summary.schur_structure_given =
			    SchurStructureToString(row_block_size[0], e_block_size[0], f_block_size[0]);
			GetBestSchurTemplateSpecialization(row_block_size,
			                                             e_block_size,
			                                             f_block_size);
			summary.schur_structure_used =
			    SchurStructureToString(row_block_size[0], e_block_size[0], f_block_size[0]);

			
		}

		summary.fixed_cost = pp.fixed_cost[0];
		summary.preprocessor_time_in_seconds = 1.0E-3 * System.currentTimeMillis() - start_time;

		if (status) {
			double minimizer_start_time = 1.0E-3 * System.currentTimeMillis();
			Minimize(pp, summary);
			summary.minimizer_time_in_seconds = 1.0E-3 * System.currentTimeMillis() - minimizer_start_time;
		} else {
			summary.message = pp.error;
		}

		double postprocessor_start_time = 1.0E-3 * System.currentTimeMillis();
		problem_impl = problem;
		program = problem_impl.mutable_program();
		// On exit, ensure that the parameter blocks again point at the user
		// provided values and the parameter blocks are numbered according
		// to their position in the original user provided program.
		program.SetParameterBlockStatePtrsToUserStatePtrs();
		program.SetParameterOffsetsAndIndex();
		PostSolveSummarize(pp, summary);
		summary.postprocessor_time_in_seconds = 1.0E-3 * System.currentTimeMillis() - postprocessor_start_time;

		// If the gradient checker reported an error, we want to report FAILURE
		// instead of USER_FAILURE and provide the error log.
		if (gradient_checking_callback.gradient_error_detected()) {
			summary.termination_type = TerminationType.FAILURE;
			summary.message = gradient_checking_callback.error_log();
		}

		summary.total_time_in_seconds = 1.0E-3 * System.currentTimeMillis() - start_time;

		// System.err.println("I finish");
	} // public void Solve

	
	public void GetBestSchurTemplateSpecialization(int[] row_block_size,
            int[] e_block_size,
            int[] f_block_size) {
			LinearSolverOptions options = new LinearSolverOptions();
			options.row_block_size[0] = row_block_size[0];
			options.e_block_size[0] = e_block_size[0];
			options.f_block_size[0] = f_block_size[0];
			row_block_size[0] = DYNAMIC;
			e_block_size[0] = DYNAMIC;
			f_block_size[0] = DYNAMIC;
			// Template specializations for the Schur complement based solvers. If
			// compile time, binary size or compiler performance is an issue, you
			// may consider disabling this.
			// Disabling Schur specializations (faster compiles)
			//#ifndef CERES_RESTRICT_SCHUR_SPECIALIZATION
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 2) &&
			(options.f_block_size[0] == 2)) {
			row_block_size[0] = 2;
			e_block_size[0] = 2;
			f_block_size[0] = 2;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 2) &&
			(options.f_block_size[0] == 3)) {
			row_block_size[0] = 2;
			e_block_size[0] = 2;
			f_block_size[0] = 3;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 2) &&
			(options.f_block_size[0] == 4)) {
			row_block_size[0] = 2;
			e_block_size[0] = 2;
			f_block_size[0] = 4;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 2)) {
			row_block_size[0] = 2;
			e_block_size[0] = 2;
			f_block_size[0] = DYNAMIC;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 3) &&
			(options.f_block_size[0] == 3)) {
			row_block_size[0] = 2;
			e_block_size[0] = 3;
			f_block_size[0] = 3;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 3) &&
			(options.f_block_size[0] == 4)) {
			row_block_size[0] = 2;
			e_block_size[0] = 3;
			f_block_size[0] = 4;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 3) &&
			(options.f_block_size[0] == 6)) {
			row_block_size[0] = 2;
			e_block_size[0] = 3;
			f_block_size[0] = 6;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 3) &&
			(options.f_block_size[0] == 9)) {
			row_block_size[0] = 2;
			e_block_size[0] = 3;
			f_block_size[0] = 9;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 3)) {
			row_block_size[0] = 2;
			e_block_size[0] = 3;
			f_block_size[0] = DYNAMIC;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 4) &&
			(options.f_block_size[0] == 3)) {
			row_block_size[0] = 2;
			e_block_size[0] = 4;
			f_block_size[0] = 3;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 4) &&
			(options.f_block_size[0] == 4)) {
			row_block_size[0] = 2;
			e_block_size[0] = 4;
			f_block_size[0] = 4;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 4) &&
			(options.f_block_size[0] == 6)) {
			row_block_size[0] = 2;
			e_block_size[0] = 4;
			f_block_size[0] = 6;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 4) &&
			(options.f_block_size[0] == 8)) {
			row_block_size[0] = 2;
			e_block_size[0] = 4;
			f_block_size[0] = 8;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 4) &&
			(options.f_block_size[0] == 9)) {
			row_block_size[0] = 2;
			e_block_size[0] = 4;
			f_block_size[0] = 9;
			return;
			}
			if ((options.row_block_size[0] == 2) &&
			(options.e_block_size[0] == 4)) {
			row_block_size[0] = 2;
			e_block_size[0] = 4;
			f_block_size[0] = DYNAMIC;
			return;
			}
			if (options.row_block_size[0] == 2){
			row_block_size[0] = 2;
			e_block_size[0] = DYNAMIC;
			f_block_size[0] = DYNAMIC;
			return;
			}
			if ((options.row_block_size[0] == 4) &&
			(options.e_block_size[0] == 4) &&
			(options.f_block_size[0] == 2)) {
			row_block_size[0] = 4;
			e_block_size[0] = 4;
			f_block_size[0] = 2;
			return;
			}
			if ((options.row_block_size[0] == 4) &&
			(options.e_block_size[0] == 4) &&
			(options.f_block_size[0] == 3)) {
			row_block_size[0] = 4;
			e_block_size[0] = 4;
			f_block_size[0] = 3;
			return;
			}
			if ((options.row_block_size[0] == 4) &&
			(options.e_block_size[0] == 4) &&
			(options.f_block_size[0] == 4)) {
			row_block_size[0] = 4;
			e_block_size[0] = 4;
			f_block_size[0] = 4;
			return;
			}
			if ((options.row_block_size[0] == 4) &&
			(options.e_block_size[0] == 4)) {
			row_block_size[0] = 4;
			e_block_size[0] = 4;
			f_block_size[0] = DYNAMIC;
			return;
			}
			
			//#endif
			return;
	}

	
	public String SchurStructureToString(int row_block_size,
            int e_block_size,
            int f_block_size) {
			String row =
			(row_block_size == DYNAMIC)
			? "d" : String.format("%d", row_block_size);
			
			String e =
			(e_block_size == DYNAMIC)
			? "d" : String.format("%d", e_block_size);
			
			String f =
			(f_block_size == DYNAMIC)
			? "d" : String.format("%d", f_block_size);
			
			return String.format("%s,%s,%s", row, e, f);
	}

	
	public void DetectStructure(CompressedRowBlockStructure bs,
            int num_eliminate_blocks,
            int[] row_block_size,
            int[] e_block_size,
            int[] f_block_size) {
			int num_row_blocks = bs.rows.size();
			row_block_size[0] = 0;
			e_block_size[0] = 0;
			f_block_size[0] = 0;
			
			// Iterate over row blocks of the matrix, checking if row_block,
			// e_block or f_block sizes remain constant.
			for (int r = 0; r < num_row_blocks; ++r) {
			CompressedList row = bs.rows.get(r);
			// We do not care about the sizes of the blocks in rows which do
			// not contain e_blocks.
			if (row.cells.get(0).block_id >= num_eliminate_blocks) {
			break;
			}
			
			// Detect fixed or dynamic row block size.
			if (row_block_size[0] == 0) {
			    row_block_size[0] = row.block.size;
			} else if (row_block_size[0] != DYNAMIC &&
			      row_block_size[0] != row.block.size) {
				if (2 <= MAX_LOG_LEVEL) {
			        Preferences.debug("Dynamic row block size because the block size changed from "
			        + row_block_size[0] + " to " + row.block.size + "\n", Preferences.DEBUG_ALGORITHM);
				}
			row_block_size[0] = DYNAMIC;
			}
			
			// Detect fixed or dynamic e-block size.
			int e_block_id = row.cells.get(0).block_id;
			if (e_block_size[0] == 0) {
			e_block_size[0] = bs.cols.get(e_block_id).size;
			} else if (e_block_size[0] != DYNAMIC &&
			      e_block_size[0] != bs.cols.get(e_block_id).size) {
				if (2 <= MAX_LOG_LEVEL) {
			        Preferences.debug("Dynamic e block size because the block size changed from "
			     + e_block_size[0] + " to " + bs.cols.get(e_block_id).size + "\n", Preferences.DEBUG_ALGORITHM);
				}
			e_block_size[0] = DYNAMIC;
			}
			
			// Detect fixed or dynamic f-block size. We are only interested in
			// rows with e-blocks, and the e-block is always the first block,
			// so only rows of size greater than 1 are of interest.
			if (row.cells.size() > 1) {
			if (f_block_size[0] == 0) {
			    int f_block_id = row.cells.get(1).block_id;
			    f_block_size[0] = bs.cols.get(f_block_id).size;
			}
			
			for (int c = 1;
			  (c < row.cells.size()) && (f_block_size[0] != DYNAMIC);
			  ++c) {
		        int f_block_id = row.cells.get(c).block_id;
			if (f_block_size[0] != bs.cols.get(f_block_id).size) {
				if (2 <= MAX_LOG_LEVEL) {
			       Preferences.debug("Dynamic f block size because the block size "
			         + "changed from " + f_block_size[0] + " to "
			         + bs.cols.get(f_block_id).size + "\n", Preferences.DEBUG_ALGORITHM);
				}
			 f_block_size[0] = DYNAMIC;
			}
			}
			}
			
			boolean is_everything_dynamic = (row_block_size[0] == DYNAMIC &&
			                               e_block_size[0] == DYNAMIC &&
			                               f_block_size[0] == DYNAMIC);
			if (is_everything_dynamic) {
			break;
			}
			}
			
		    if (row_block_size[0] == 0) {
		    	System.err.println("No rows found in DetectStructure");
		    	return;
		    }
			if (e_block_size[0] == 0) {
				System.err.println("No e type blocks found in DetectStructure");
				return;
			}
			if (1 <= MAX_LOG_LEVEL) {
			    Preferences.debug("Schur complement static structure \n" +
			    "row_block_size[0] = " + row_block_size[0] + "\n" +
			    "e_block_size[0] = " + e_block_size[0] + "\n" +
			    "f_block_size[0] = " + f_block_size[0] + "\n", Preferences.DEBUG_ALGORITHM);
			}
	}


	public void PostSolveSummarize(PreprocessedProblem pp, SolverSummary summary) {
		OrderingToGroupSizes(pp.options.linear_solver_ordering, summary.linear_solver_ordering_used);
		OrderingToGroupSizes(pp.options.inner_iteration_ordering, summary.inner_iteration_ordering_used);

		summary.inner_iterations_used = pp.inner_iteration_minimizer != null; // NOLINT
		summary.linear_solver_type_used = pp.linear_solver_options.type;
		summary.num_linear_solver_threads_used = pp.options.num_threads;
		summary.num_threads_used = pp.options.num_threads;
		summary.preconditioner_type_used = pp.options.preconditioner_type;

	
		SetSummaryFinalCost(summary);
		 
		 if (pp.reduced_program != null) {
		     SummarizeReducedProgram(pp.reduced_program, summary); 
		 }
		 
		  // It is possible that no evaluator was created. This would be the
		  // case if the preprocessor failed, or if the reduced problem did
		  // not contain any parameter blocks. Thus, only extract the
		  // evaluator statistics if one exists.
		  if (pp.evaluator != null) {
		    HashMap<String, CallStatistics> evaluator_statistics =
		        pp.evaluator.Statistics();
		    {
		      CallStatistics call_stats = FindWithDefault(
		          evaluator_statistics, "Evaluator::Residual", new CallStatistics());

		      summary.residual_evaluation_time_in_seconds = call_stats.time;
		      summary.num_residual_evaluations = call_stats.calls;
		    }
		    {
		      CallStatistics call_stats = FindWithDefault(
		          evaluator_statistics, "Evaluator::Jacobian", new CallStatistics());

		      summary.jacobian_evaluation_time_in_seconds = call_stats.time;
		      summary.num_jacobian_evaluations = call_stats.calls;
		    }
		  }

		  // Again, like the evaluator, there may or may not be a linear
		  // solver from which we can extract run time statistics. In
		  // particular the line search solver does not use a linear solver.
		  if (pp.linear_solver != null) {
		    HashMap<String, CallStatistics> linear_solver_statistics =
		        pp.linear_solver.Statistics();
		    CallStatistics call_stats = FindWithDefault(
		        linear_solver_statistics, "LinearSolver::Solve", new CallStatistics());
		    summary.num_linear_solves = call_stats.calls;
		    summary.linear_solver_time_in_seconds = call_stats.time;
		  }

		
	}
	
	public void SummarizeReducedProgram(Program program,
            SolverSummary summary) {
			summary.num_parameter_blocks_reduced     = program.NumParameterBlocks();
			summary.num_parameters_reduced           = program.NumParameters();
			summary.num_effective_parameters_reduced = program.NumEffectiveParameters();
			summary.num_residual_blocks_reduced      = program.NumResidualBlocks();
			summary.num_residuals_reduced            = program.NumResiduals();
    }

	
	public void SetSummaryFinalCost(SolverSummary summary) {
	  summary.final_cost = summary.initial_cost;
	  // We need the loop here, instead of just looking at the last
	  // iteration because the minimizer maybe making non-monotonic steps.
	  for (int i = 0; i < summary.iterations.size(); ++i) {
	    IterationSummary iteration_summary = summary.iterations.get(i);
	    summary.final_cost = Math.min(iteration_summary.cost, summary.final_cost);
	  }
	}
	
	public void SetSummaryFinalCost(GradientProblemSolverSummary summary) {
		  summary.final_cost = summary.initial_cost;
		  // We need the loop here, instead of just looking at the last
		  // iteration because the minimizer maybe making non-monotonic steps.
		  for (int i = 0; i < summary.iterations.size(); ++i) {
		    IterationSummary iteration_summary = summary.iterations.get(i);
		    summary.final_cost = Math.min(iteration_summary.cost, summary.final_cost);
		  }
		}

	public void Minimize(PreprocessedProblem pp, SolverSummary summary) {
		int i;
		Program program = pp.reduced_program;
		if (pp.reduced_program.NumParameterBlocks() == 0) {
			summary.message[0] = "Function tolerance reached. No non-constant parameter blocks found.";
			summary.termination_type = TerminationType.CONVERGENCE;
			if (pp.options.logging_type != LoggingType.SILENT) {
				Preferences.debug(summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
			}
			summary.initial_cost = summary.fixed_cost;
			summary.final_cost = summary.fixed_cost;
			return;
		}

		Vector<Double> original_reduced_parameters = new Vector<Double>();
	
		 Minimizer minimizer = Create(pp.options.minimizer_type);
		 double reduced_parameters_array[] = new double[pp.reduced_parameters.size()];
		 for (i = 0; i < pp.reduced_parameters.size(); i++) {
			 reduced_parameters_array[i] = pp.reduced_parameters.get(i);
			 original_reduced_parameters.add(pp.reduced_parameters.get(i));
		 }
		 if (pp.options.minimizer_type == MinimizerType.TRUST_REGION) {
		     ((TrustRegionMinimizer)minimizer).Minimize(pp.minimizer_options, reduced_parameters_array,summary);
		 }
		 else if (pp.options.minimizer_type == MinimizerType.LINE_SEARCH) {
		     ((LineSearchMinimizer)minimizer).Minimize(pp.minimizer_options, reduced_parameters_array,summary);
		 }
		 for (i = 0; i < reduced_parameters_array.length; i++) {
			 pp.reduced_parameters.set(i,reduced_parameters_array[i]);
		 }
		 if (IsSolutionUsable(summary)) {
			 program.StateVectorToParameterBlocks(pp.reduced_parameters);
		 }
		 else {
			 program.StateVectorToParameterBlocks(original_reduced_parameters);
		 }
		
		 program.CopyParameterBlockStateToUserState();

	} // public void Minimize

	public boolean IsSchurType(LinearSolverType type) {
		return ((type == LinearSolverType.SPARSE_SCHUR) || (type == LinearSolverType.DENSE_SCHUR)
				|| (type == LinearSolverType.ITERATIVE_SCHUR));
	}

	Preprocessor CreatePreprocessor(MinimizerType minimizer_type) {
		if (minimizer_type == MinimizerType.TRUST_REGION) {
			return new TrustRegionPreprocessor();
		}

		if (minimizer_type == MinimizerType.LINE_SEARCH) {
			return new LineSearchPreprocessor();
		}

		System.err.println("Unknown minimizer_type: " + minimizer_type);
		return null;
	}

	abstract class Preprocessor {
		public Preprocessor() {

		}

		public abstract boolean Preprocess(SolverOptions options, ProblemImpl problem, PreprocessedProblem pp);

	}

	class TrustRegionPreprocessor extends Preprocessor {
		public TrustRegionPreprocessor() {
			super();
		}

		public boolean Preprocess(SolverOptions options, ProblemImpl problem, PreprocessedProblem pp) {
			if (pp == null) {
				System.err.println("PreprocessedProblem pp == null in TrustRegionPreProcessor Preprocess");
				return false;
			}
			pp.options = options; 
			// ChangeNumThreadsIfNeeded(pp.options);

			pp.problem = problem;
			
			Program program = problem.mutable_program();
			if (!IsProgramValid(program, pp.error)) {
				return false;
			}

			pp.reduced_program = program.CreateReducedProgram(pp.removed_parameter_blocks, pp.fixed_cost, pp.error);

			if (pp.reduced_program == null) {
				return false;
			}

			if (pp.reduced_program.NumParameterBlocks() == 0) {
				// The reduced problem has no parameter or residual blocks. There
				// is nothing more to do.
				return true;
			}

			
			if (!SetupLinearSolver(pp) || !SetupEvaluator(pp) ||
			    !SetupInnerIterationMinimizer(pp)) { return false; }
 
			SetupMinimizerOptions(pp);
			 
			return true;
		}
		
		// Configure and create a TrustRegionMinimizer object.
		public void SetupMinimizerOptions(PreprocessedProblem pp) {
		  SolverOptions options = pp.options;
		  SetupCommonMinimizerOptions(pp);
		  pp.minimizer_options.is_constrained =
		      pp.reduced_program.IsBoundsConstrained();
		  pp.minimizer_options.jacobian = pp.evaluator.CreateJacobian();
		  pp.minimizer_options.inner_iteration_minimizer =
		      pp.inner_iteration_minimizer;

		  TrustRegionStrategyOptions strategy_options = new TrustRegionStrategyOptions();
		  strategy_options.linear_solver = pp.linear_solver;
		  strategy_options.initial_radius =
		      options.initial_trust_region_radius;
		  strategy_options.max_radius = options.max_trust_region_radius;
		  strategy_options.min_lm_diagonal = options.min_lm_diagonal;
		  strategy_options.max_lm_diagonal = options.max_lm_diagonal;
		  strategy_options.trust_region_strategy_type =
		      options.trust_region_strategy_type;
		  strategy_options.dogleg_type = options.dogleg_type;
		  pp.minimizer_options.trust_region_strategy = Create(strategy_options);
		  if (pp.minimizer_options.trust_region_strategy == null) {
			  System.err.println("Create(strategy_options) returned null in SetupMinimizerOptions");
		  }
		}


		// Check if all the user supplied values in the parameter blocks are
		// sane or not, and if the program is feasible or not.
		boolean IsProgramValid(Program program, String error[]) {
			return (program.ParameterBlocksAreFinite(error) && program.IsFeasible(error));
		}
		
		// If the user requested inner iterations, then find an inner
		// iteration ordering as needed and configure and create a
		// CoordinateDescentMinimizer object to perform the inner iterations.
		public boolean SetupInnerIterationMinimizer(PreprocessedProblem pp) {
		  SolverOptions options = pp.options;
		  if (!options.use_inner_iterations) {
		    return true;
		  }

		  // With just one parameter block, the outer iteration of the trust
		  // region method and inner iterations are doing exactly the same
		  // thing, and thus inner iterations are not needed.
		  if (pp.reduced_program.NumParameterBlocks() == 1) {
		    Preferences.debug("Reduced problem only contains one parameter block.\n", Preferences.DEBUG_ALGORITHM);
		    Preferences.debug("Disabling inner iterations.\n", Preferences.DEBUG_ALGORITHM);
		    return true;
		  }

		  if (options.inner_iteration_ordering != null) {
		    // If the user supplied an ordering, then remove the set of
		    // inactive parameter blocks from it
		    options.inner_iteration_ordering.Remove(pp.removed_parameter_blocks);
		    if (options.inner_iteration_ordering.NumElements() == 0) {
		      Preferences.debug("No remaining elements in the inner iteration ordering.\n", Preferences.DEBUG_ALGORITHM);
		      return true;
		    }

		    // Validate the reduced ordering.
		    if (!IsOrderingValid(
		            pp.reduced_program,
		            options.inner_iteration_ordering,
		            pp.error)) {
		      return false;
		    }
		  } else {
		    // The user did not supply an ordering, so create one.
		    options.inner_iteration_ordering = CreateOrdering(pp.reduced_program);
		  }

		  pp.inner_iteration_minimizer = 
		      new CoordinateDescentMinimizer(pp.problem.context());
		  return pp.inner_iteration_minimizer.Init(pp.reduced_program,
		                                             pp.problem.parameter_map(),
		                                             options.inner_iteration_ordering,
		                                             pp.error);
		}

		
		// Configure and create the evaluator.
		public boolean SetupEvaluator(PreprocessedProblem pp) {
		  SolverOptions options = pp.options;
		  pp.evaluator_options = new EvaluatorOptions();
		  pp.evaluator_options.linear_solver_type = options.linear_solver_type;
		  pp.evaluator_options.num_eliminate_blocks = 0;
		  if (IsSchurType(options.linear_solver_type)) {
		    pp.evaluator_options.num_eliminate_blocks =
		        options
		        .linear_solver_ordering
		        .group_to_elements().values().size();
		  }

		  pp.evaluator_options.num_threads = options.num_threads;
		  pp.evaluator_options.dynamic_sparsity = options.dynamic_sparsity;
		  pp.evaluator_options.context = pp.problem.context();
		  pp.evaluator_options.evaluation_callback = options.evaluation_callback;
		  pp.evaluator = Create(pp.evaluator_options,
		                                        pp.reduced_program,
		                                        pp.error);

		  return (pp.evaluator != null);
		}


		// Configure and create a linear solver object. In doing so, if a
		// sparse direct factorization based linear solver is being used, then
		// find a fill reducing ordering and reorder the program as needed
		// too.
		public boolean SetupLinearSolver(PreprocessedProblem pp) {
			SolverOptions options = pp.options;
			if (options.linear_solver_ordering == null) {
				// If the user has not supplied a linear solver ordering, then we
				// assume that they are giving all the freedom to us in choosing
				// the best possible ordering. This intent can be indicated by
				// putting all the parameter blocks in the same elimination group.
				options.linear_solver_ordering = CreateDefaultLinearSolverOrdering(pp.reduced_program);
			} else {
				// If the user supplied an ordering, then check if the first
				// elimination group is still non-empty after the reduced problem
				// has been constructed.
				//
				// This is important for Schur type linear solvers, where the
				// first elimination group is special -- it needs to be an
				// independent set.
				//
				// If the first elimination group is empty, then we cannot use the
				// user's requested linear solver (and a preconditioner as the
				// case may be) so we must use a different one.
				OrderedGroups<double[]> ordering = options.linear_solver_ordering;
				int min_group_id = ordering.MinNonZeroGroup();
				ordering.Remove(pp.removed_parameter_blocks);
				if (IsSchurType(options.linear_solver_type) && min_group_id != ordering.MinNonZeroGroup()) {
					AlternateLinearSolverAndPreconditionerForSchurTypeLinearSolver(options);
				}
			}

			// Reorder the program to reduce fill in and improve cache coherency
			// of the Jacobian.
			if (!ReorderProgram(pp)) {
				return false;
			}

			// Configure the linear solver.
			pp.linear_solver_options.min_num_iterations = options.min_linear_solver_iterations;
			pp.linear_solver_options.max_num_iterations = options.max_linear_solver_iterations;
			pp.linear_solver_options.type = options.linear_solver_type;
			pp.linear_solver_options.preconditioner_type = options.preconditioner_type;
			pp.linear_solver_options.visibility_clustering_type = options.visibility_clustering_type;
			pp.linear_solver_options.sparse_linear_algebra_library_type = options.sparse_linear_algebra_library_type;
			pp.linear_solver_options.dense_linear_algebra_library_type = options.dense_linear_algebra_library_type;
			pp.linear_solver_options.use_explicit_schur_complement = options.use_explicit_schur_complement;
			pp.linear_solver_options.dynamic_sparsity = options.dynamic_sparsity;
			pp.linear_solver_options.num_threads = options.num_threads;
			pp.linear_solver_options.use_postordering = options.use_postordering;
			pp.linear_solver_options.context = pp.problem.context();

			if (IsSchurType(pp.linear_solver_options.type)) {
				OrderingToGroupSizes(options.linear_solver_ordering, pp.linear_solver_options.elimination_groups);

				// Schur type solvers expect at least two elimination groups. If
				// there is only one elimination group, then it is guaranteed that
				// this group only contains e_blocks. Thus we add a dummy
				// elimination group with zero blocks in it.
				if (pp.linear_solver_options.elimination_groups.size() == 1) {
					pp.linear_solver_options.elimination_groups.add(0);
				}

				if (options.linear_solver_type == LinearSolverType.SPARSE_SCHUR) {
					// When using SPARSE_SCHUR, we ignore the user's postordering
					// preferences in certain cases.
					//
					// 1. SUITE_SPARSE is the sparse linear algebra library requested
					// but cholmod_camd is not available.
					// 2. CX_SPARSE is the sparse linear algebra library requested.
					//
					// This ensures that the linear solver does not assume that a
					// fill-reducing pre-ordering has been done.
					//
					// TODO(sameeragarwal): Implement the reordering of parameter
					// blocks for CX_SPARSE.
					if ((options.sparse_linear_algebra_library_type == SparseLinearAlgebraLibraryType.SUITE_SPARSE
							&& (!IsConstrainedApproximateMinimumDegreeOrderingAvailable()))
							|| (options.sparse_linear_algebra_library_type == SparseLinearAlgebraLibraryType.CX_SPARSE)) {
						pp.linear_solver_options.use_postordering = true;
					}
				}
			}
			pp.linear_solver = Create(pp.linear_solver_options);
			pp.linear_solver.options_.type = pp.linear_solver_options.type;
			return (pp.linear_solver != null);
		}

		// The fix was actually committed in 4.1.0, but there is
		// some confusion about a silent update to the tar ball, so we are
		// being conservative and choosing the next minor version where
		// things are stable.
		public boolean IsConstrainedApproximateMinimumDegreeOrderingAvailable() {
			return (SUITESPARSE_VERSION > 4001);
		}

		// For Schur type and SPARSE_NORMAL_CHOLESKY linear solvers, reorder
		// the program to reduce fill-in and increase cache coherency.
		public boolean ReorderProgram(PreprocessedProblem pp) {
			SolverOptions options = pp.options;
			if (IsSchurType(options.linear_solver_type)) {
				return ReorderProgramForSchurTypeLinearSolver(options.linear_solver_type,
						options.sparse_linear_algebra_library_type, pp.problem.parameter_map(),
						options.linear_solver_ordering, pp.reduced_program, pp.error);
			}

			// SPARSE_NORMAL_CHOLESKY will not be used because of licensing problems
			if (options.linear_solver_type == LinearSolverType.SPARSE_NORMAL_CHOLESKY && !options.dynamic_sparsity) {
				/*
				 * return ReorderProgramForSparseNormalCholesky(
				 * options.sparse_linear_algebra_library_type, options.linear_solver_ordering,
				 * pp.reduced_program, pp.error);
				 */
			}

			return true;
		}

		public void AlternateLinearSolverAndPreconditionerForSchurTypeLinearSolver(SolverOptions options) {
			if (!IsSchurType(options.linear_solver_type)) {
				return;
			}

			LinearSolverType linear_solver_type_given = options.linear_solver_type;
			PreconditionerType preconditioner_type_given = options.preconditioner_type;
			options.linear_solver_type = LinearSolverForZeroEBlocks(linear_solver_type_given);

			String message;
			if (linear_solver_type_given == LinearSolverType.ITERATIVE_SCHUR) {
				options.preconditioner_type = PreconditionerForZeroEBlocks(preconditioner_type_given);

				message = "No E blocks. Switching from " + LinearSolverTypeToString(linear_solver_type_given) + "("
						+ PreconditionerTypeToString(preconditioner_type_given) + ") to "
						+ LinearSolverTypeToString(options.linear_solver_type) + "("
						+ PreconditionerTypeToString(options.preconditioner_type) + ").";
			} else {
				message = "No E blocks. Switching from " + LinearSolverTypeToString(linear_solver_type_given) + " to "
						+ LinearSolverTypeToString(options.linear_solver_type) + ".";
			}

			if (options.logging_type != LoggingType.SILENT) {
				Preferences.debug(message + "\n", Preferences.DEBUG_ALGORITHM);
			}
		}

		public OrderedGroups<double[]> CreateDefaultLinearSolverOrdering(Program program) {
			OrderedGroups<double[]> ordering = new OrderedGroups<>();
			Vector<ParameterBlock> parameter_blocks = program.parameter_blocks();
			for (int i = 0; i < parameter_blocks.size(); ++i) {
				ordering.AddElementToGroup(parameter_blocks.get(i).user_state(), 0);
			}
			return ordering;
		}

	} // class TrustRegionProprocessor

	public <T> void swap(T el1, T el2) {
		T temp = el1;
		el1 = el2;
		el2 = temp;
	}

	public boolean ReorderProgramForSchurTypeLinearSolver(LinearSolverType linear_solver_type,
			SparseLinearAlgebraLibraryType sparse_linear_algebra_library_type,
			HashMap<double[], ParameterBlock> parameter_map, OrderedGroups<double[]> parameter_block_ordering,
			Program program, String error[]) {
		if (parameter_block_ordering.NumElements() != program.NumParameterBlocks()) {
			error[0] = "The program has " + program.NumParameterBlocks() + " parameter blocks, but the parameter block "
					+ "\n" + "ordering has" + parameter_block_ordering.NumElements() + " parameter blocks.";
			return false;
		}

		if (parameter_block_ordering.NumGroups() == 1) {
			// If the user supplied an parameter_block_ordering with just one
			// group, it is equivalent to the user supplying NULL as an
			// parameter_block_ordering. Ceres is completely free to choose the
			// parameter block ordering as it sees fit. For Schur type solvers,
			// this means that the user wishes for Ceres to identify the
			// e_blocks, which we do by computing a maximal independent set.
			Vector<ParameterBlock> schur_ordering = new Vector<ParameterBlock>();
			int size_of_first_elimination_group = ComputeStableSchurOrdering(program, schur_ordering);

			if (schur_ordering.size() != program.NumParameterBlocks()) {
				System.err.println("Congratulations, you found a Ceres bug! Please report this error ");
				System.err.println("to the developers.");
				System.err.println(
						"schur_ordering.size() != program.NumParameterBlocks() in ReorderProgramForSchurTypeLinearSolver");
				return false;
			}

			// Update the parameter_block_ordering object.
			for (int i = 0; i < schur_ordering.size(); ++i) {
				double[] parameter_block = schur_ordering.get(i).mutable_user_state();
				int group_id = (i < size_of_first_elimination_group) ? 0 : 1;
				parameter_block_ordering.AddElementToGroup(parameter_block, group_id);
			}

			// We could call ApplyOrdering but this is cheaper and
			// simpler.
			swap(program.mutable_parameter_blocks(), schur_ordering);
		} else {
			// The user provided an ordering with more than one elimination
			// group.

			// Verify that the first elimination group is an independent set.
			Collection<Set<double[]>> col = parameter_block_ordering.group_to_elements().values();
			Iterator<Set<double[]>> it = col.iterator();
			Set<double[]> first_elimination_group = (Set<double[]>) it.next();
			if (!program.IsParameterBlockSetIndependent(first_elimination_group)) {
				error[0] = "The first elimination group in the parameter block \n" + "ordering of size "
						+ first_elimination_group.size() + " is not an independent set";
				return false;
			}

			if (!ApplyOrdering(parameter_map, parameter_block_ordering, program, error)) {
				return false;
			}
		}

		program.SetParameterOffsetsAndIndex();

		Collection<Set<double[]>> col = parameter_block_ordering.group_to_elements().values();
		Iterator<Set<double[]>> it = col.iterator();
		Set<double[]> first_elimination_group = (Set<double[]>) it.next();
		int size_of_first_elimination_group = first_elimination_group.size();

		if (linear_solver_type == LinearSolverType.SPARSE_SCHUR) {
			if (sparse_linear_algebra_library_type == SparseLinearAlgebraLibraryType.SUITE_SPARSE) {
				MaybeReorderSchurComplementColumnsUsingSuiteSparse(parameter_block_ordering, program);
			} else if (sparse_linear_algebra_library_type == SparseLinearAlgebraLibraryType.EIGEN_SPARSE) {
				MaybeReorderSchurComplementColumnsUsingEigen(size_of_first_elimination_group, parameter_map, program);
			}
		}

		// Schur type solvers also require that their residual blocks be
		// lexicographically ordered.
		if (!LexicographicallyOrderResidualBlocks(size_of_first_elimination_group, program, error)) {
			return false;
		}

		return true;
	}

	// Find the minimum index of any parameter block to the given
	// residual. Parameter blocks that have indices greater than
	// size_of_first_elimination_group are considered to have an index
	// equal to size_of_first_elimination_group.
	public int MinParameterBlock(ResidualBlock residual_block, int size_of_first_elimination_group) {
		int min_parameter_block_position = size_of_first_elimination_group;
		for (int i = 0; i < residual_block.NumParameterBlocks(); ++i) {
			ParameterBlock parameter_block = residual_block.parameter_blocks()[i];
			if (!parameter_block.IsConstant()) {
				if (parameter_block.index() == -1) {
					System.err.println("Did you forget to call Program::SetParameterOffsetsAndIndex()? ");
					System.err.println("This is a Ceres bug; please contact the developers!");
					System.err.println("In MinParameterBlock parameter_block.index() == -1");
					return -1;
				}
				min_parameter_block_position = Math.min(parameter_block.index(), min_parameter_block_position);
			}
		}
		return min_parameter_block_position;
	}

	public boolean LexicographicallyOrderResidualBlocks(int size_of_first_elimination_group, Program program,
			String error[]) {
		int i;
		int value;
		if (size_of_first_elimination_group < 1) {
			System.err.println("Congratulations, you found a Ceres bug! Please report this error ");
			System.err.println("to the developers.");
			System.err.println("In LexicographicallyOrderResidualBlocks size_of_first_elimination_group < 1");
			return false;
		}

		// Create a histogram of the number of residuals for each E block. There is an
		// extra bucket at the end to catch all non-eliminated F blocks.
		Vector<Integer> residual_blocks_per_e_block = new Vector<Integer>(size_of_first_elimination_group + 1);
		for (i = 0; i < size_of_first_elimination_group + 1; i++) {
			residual_blocks_per_e_block.add(0);
		}
		Vector<ResidualBlock> residual_blocks = program.mutable_residual_blocks();
		Vector<Integer> min_position_per_residual = new Vector<Integer>(residual_blocks.size());
		for (i = 0; i < residual_blocks.size(); ++i) {
			ResidualBlock residual_block = residual_blocks.get(i);
			int position = MinParameterBlock(residual_block, size_of_first_elimination_group);
			min_position_per_residual.add(position);
			if (position > size_of_first_elimination_group) {
				System.err
						.println("In LexicographicallyOrderResidualBlocks position > size_of_first_elimination_group");
				return false;
			}
			value = residual_blocks_per_e_block.get(position);
			residual_blocks_per_e_block.set(position, value + 1);
		}

		// Run a cumulative sum on the histogram, to obtain offsets to the start of
		// each histogram bucket (where each bucket is for the residuals for that
		// E-block).
		int partial_sum = 0;
		Vector<Integer> offsets = new Vector<Integer>(size_of_first_elimination_group + 1);
		for (i = 0; i < residual_blocks_per_e_block.size(); i++) {
			partial_sum += residual_blocks_per_e_block.get(i);
			offsets.add(partial_sum);
		}

		if (offsets.get(offsets.size() - 1) != residual_blocks.size()) {
			System.err.println("Congratulations, you found a Ceres bug! Please report this error ");
			System.err.println("to the developers.");
			System.err.println(
					"In LexicographicallyOrderResidualBlocks offsets.get(offsets.size()-1) != residual_blocks.size())");
			return false;
		}

		/*
		CHECK(find(residual_blocks_per_e_block.begin(),
             residual_blocks_per_e_block.end() - 1, 0) !=
        residual_blocks_per_e_block.end())
      << "Congratulations, you found a Ceres bug! Please report this error "
      << "to the developers.";
        Original code can never return residual_blocks_per_e_block.end()
        If it finds zero, it will return a value from residual_blocks_per_e_block.begin()
        to residual_blocks_per_e_block.end()-2.  If it does not find zero,
        it will return residual_blocks_per_e_block.end()-1.
		boolean foundZero = true;
		if (residual_blocks_per_e_block.size() >= 2) {
			foundZero = false;
			for (i = 0; i < residual_blocks_per_e_block.size()-1; i++) {
				value = residual_blocks_per_e_block.get(i);
				if (value == 0) {
				    foundZero = true;
				    break;
				}
			}
		}
		if (!foundZero) {
			System.err.println("Congratulations, you found a Ceres bug! Please report this error ");
			System.err.println("to the developers.");
			System.err.println(
					"In LexicographicallyOrderResidualBlocks zero not found in residual_blocks_per_e_block");
			return false;
		}*/
		// Fill in each bucket with the residual blocks for its corresponding E block.
		// Each bucket is individually filled from the back of the bucket to the front
		// of the bucket. The filling order among the buckets is dictated by the
		// residual blocks. This loop uses the offsets as counters; subtracting one
		// from each offset as a residual block is placed in the bucket. When the
		// filling is finished, the offset pointerts should have shifted down one
		// entry (this is verified below).
		Vector<ResidualBlock> reordered_residual_blocks = new Vector<ResidualBlock>(residual_blocks.size());
		for (i = 0; i < residual_blocks.size(); ++i) {
			reordered_residual_blocks.add(null);
		}
		for (i = 0; i < residual_blocks.size(); ++i) {
			int bucket = min_position_per_residual.get(i);

			// Decrement the cursor, which should now point at the next empty position.
			int content = offsets.get(bucket);
			offsets.set(bucket, content - 1);

			// Sanity.
			if (reordered_residual_blocks.get(offsets.get(bucket)) != null) {
				System.err.println("Congratulations, you found a Ceres bug! Please report this error ");
				System.err.println("to the developers.");
				System.err.println(
						"In LexicographicallyOrderResidualBlocks reordered_residual_blocks.get(offsets.get(bucket)) != null");
				return false;
			}

			reordered_residual_blocks.set(offsets.get(bucket), residual_blocks.get(i));
		}

		// Sanity check #1: The difference in bucket offsets should match the
		// histogram sizes.
		for (i = 0; i < size_of_first_elimination_group; ++i) {
			if (residual_blocks_per_e_block.get(i) != (offsets.get(i + 1) - offsets.get(i))) {
				System.err.println("Congratulations, you found a Ceres bug! Please report this error ");
				System.err.println("to the developers.");
				System.err.println(
						"In LexicographicallyOrderResidualBlocks residual_blocks_per_e_block.get(i) != (offsets.get(i + 1) - offsets.get(i))");
				return false;
			}
		}
		// Sanity check #2: No NULL's left behind.
		for (i = 0; i < reordered_residual_blocks.size(); ++i) {
			if (reordered_residual_blocks.get(i) == null) {
				System.err.println("Congratulations, you found a Ceres bug! Please report this error ");
				System.err.println("to the developers.");
				System.err.println("In LexicographicallyOrderResidualBlocks reordered_residual_blocks.get(i) == null");
				return false;
			}
		}

		// Now that the residuals are collected by E block, swap them in place.
		program.mutable_residual_blocks().clear();
		program.mutable_residual_blocks().addAll(reordered_residual_blocks);
		return true;
	}

	// Pre-order the columns corresponding to the schur complement if
	// possible.
	public void MaybeReorderSchurComplementColumnsUsingSuiteSparse(OrderedGroups<double[]> parameter_block_ordering,
			Program program) {
		// Cannot use SUITSPARSE because of licensing problems
		if (!CERES_NO_SUITESPARSE) {
			// SuiteSparse ss;
			/*
			 * if (!IsConstrainedApproximateMinimumDegreeOrderingAvailable()) { return; }
			 * 
			 * vector<int> constraints; vector<ParameterBlock*>& parameter_blocks =
			 * (program->mutable_parameter_blocks());
			 * 
			 * for (int i = 0; i < parameter_blocks.size(); ++i) { constraints.push_back(
			 * parameter_block_ordering.GroupId(
			 * parameter_blocks[i]->mutable_user_state())); }
			 * 
			 * // Renumber the entries of constraints to be contiguous integers as // CAMD
			 * requires that the group ids be in the range [0, // parameter_blocks.size() -
			 * 1]. MapValuesToContiguousRange(constraints.size(), &constraints[0]);
			 * 
			 * // Compute a block sparse presentation of J'. scoped_ptr<TripletSparseMatrix>
			 * tsm_block_jacobian_transpose(
			 * program->CreateJacobianBlockSparsityTranspose());
			 * 
			 * cholmod_sparse* block_jacobian_transpose =
			 * ss.CreateSparseMatrix(tsm_block_jacobian_transpose.get());
			 * 
			 * vector<int> ordering(parameter_blocks.size(), 0);
			 * ss.ConstrainedApproximateMinimumDegreeOrdering(block_jacobian_transpose,
			 * &constraints[0], &ordering[0]); ss.Free(block_jacobian_transpose);
			 * 
			 * const vector<ParameterBlock*> parameter_blocks_copy(parameter_blocks); for
			 * (int i = 0; i < program->NumParameterBlocks(); ++i) { parameter_blocks[i] =
			 * parameter_blocks_copy[ordering[i]]; }
			 * 
			 * program->SetParameterOffsetsAndIndex();
			 */
		} // if (!CERES_NO_SUITESPARSE)
	}

	public void MaybeReorderSchurComplementColumnsUsingEigen(int size_of_first_elimination_group,
			HashMap<double[], ParameterBlock> parameter_map, Program program) {
		if (/* (!EIGEN_VERSION_AT_LEAST(3, 2, 2)) || */(!CERES_USE_EIGEN_SPARSE)) {
			return;
		}
		/*
		 * #else
		 * 
		 * scoped_ptr<TripletSparseMatrix> tsm_block_jacobian_transpose(
		 * program->CreateJacobianBlockSparsityTranspose());
		 * 
		 * typedef Eigen::SparseMatrix<int> SparseMatrix; const SparseMatrix
		 * block_jacobian = CreateBlockJacobian(*tsm_block_jacobian_transpose); const
		 * int num_rows = block_jacobian.rows(); const int num_cols =
		 * block_jacobian.cols();
		 * 
		 * // Vertically partition the jacobian in parameter blocks of type E // and F.
		 * const SparseMatrix E = block_jacobian.block(0, 0, num_rows,
		 * size_of_first_elimination_group); const SparseMatrix F =
		 * block_jacobian.block(0, size_of_first_elimination_group, num_rows, num_cols -
		 * size_of_first_elimination_group);
		 * 
		 * // Block sparsity pattern of the schur complement. const SparseMatrix
		 * block_schur_complement = F.transpose() * F - F.transpose() * E *
		 * E.transpose() * F;
		 * 
		 * Eigen::AMDOrdering<int> amd_ordering;
		 * Eigen::PermutationMatrix<Eigen::Dynamic, Eigen::Dynamic, int> perm;
		 * amd_ordering(block_schur_complement, perm);
		 * 
		 * const vector<ParameterBlock*>& parameter_blocks =
		 * program->parameter_blocks(); vector<ParameterBlock*> ordering(num_cols);
		 * 
		 * // The ordering of the first size_of_first_elimination_group does // not
		 * matter, so we preserve the existing ordering. for (int i = 0; i <
		 * size_of_first_elimination_group; ++i) { ordering[i] = parameter_blocks[i]; }
		 * 
		 * // For the rest of the blocks, use the ordering computed using AMD. for (int
		 * i = 0; i < block_schur_complement.cols(); ++i) {
		 * ordering[size_of_first_elimination_group + i] =
		 * parameter_blocks[size_of_first_elimination_group + perm.indices()[i]]; }
		 * 
		 * swap(*program->mutable_parameter_blocks(), ordering);
		 * program->SetParameterOffsetsAndIndex(); #endif
		 */
	}

	public boolean ApplyOrdering(HashMap<double[], ParameterBlock> parameter_map, OrderedGroups<double[]> ordering,
			Program program, String error[]) {
		int num_parameter_blocks = program.NumParameterBlocks();
		if (ordering.NumElements() != num_parameter_blocks) {
			error[0] = "User specified ordering does not have the same \n"
					+ "number of parameters as the problem. The problem\n" + "has " + num_parameter_blocks
					+ " blocks while the ordering has " + ordering.NumElements() + " blocks.";
			return false;
		}

		Vector<ParameterBlock> parameter_blocks = program.mutable_parameter_blocks();
		parameter_blocks.clear();

		HashMap<Integer, Set<double[]>> groups = ordering.group_to_elements();
		Collection<Set<double[]>> col = groups.values();
		Iterator<Set<double[]>> group_it = col.iterator();
		Set<Integer> groupNumSet = groups.keySet();
		Iterator<Integer> num_iterator = groupNumSet.iterator();
		while (group_it.hasNext()) {
			Set<double[]> group = group_it.next();
			int groupNum = num_iterator.next();
			Iterator<double[]> parameter_block_ptr_it = group.iterator();
			while (parameter_block_ptr_it.hasNext()) {
				ParameterBlock pm = parameter_map.get(parameter_block_ptr_it.next());
				if (pm == null) {
					error[0] = "User specified ordering contains a double[] \n"
							+ "that does not have a parameter block in \n"
							+ "the problem. The invalid double is in group: " + groupNum;
					return false;
				}
				parameter_blocks.add(pm);
			}
		}
		return true;
	}
	
	class Pair<T,U>
	{
	    public T first;
	    public U second;

	    public Pair(T t, U u)
	    {
	        first = t;
	        second = u;
	    }
	    
	    public T getFirst()
	    {
	        return first; 
	    }

	    public U getSecond()
	    {
	        return second; 
	    }

	    
	    public boolean equals(Object obj) {
	      if (obj == null) return false;
	      if ((obj.getClass() != this.getClass())) { //|| (obj.hashCode() != this.hashCode())) {
	        return false;
	      }
	      
	      return (this.getFirst().equals(((Pair) obj).getFirst()) && this.getSecond().equals(((Pair) obj).getSecond()));
	    }
	    
	    /**
	     * Define a hash code based on the first and second's hash code
	     */
	    public int hashCode() {
	      return first.hashCode() ^ second.hashCode();
	    }
	    
	    public String toString() {
	      return "Pair(" + first + ", " + second + ")";
	    }
	  
	} 
	
	class WeightedGraph<Vertex> {
		private HashSet<Vertex> vertices_;
		private HashMap<Vertex, HashSet<Vertex>> edges_;
		private HashMap<Vertex, Double> vertex_weights_;
		private HashMap<Pair<Vertex, Vertex>, Double> edge_weights_;
        
		public WeightedGraph() {
			vertices_ = new HashSet<Vertex>();
			edges_ = new HashMap<Vertex, HashSet<Vertex>>();
			vertex_weights_ = new HashMap<Vertex, Double>();
			edge_weights_ = new HashMap<Pair<Vertex, Vertex>, Double>();
		}
		
		  // Add a weighted vertex. If the vertex already exists in the graph,
		  // its weight is set to the new weight.
		  public void AddVertex(Vertex vertex, double weight) {
			  if (!vertices_.contains(vertex)) {
		      vertices_.add(vertex);
		      edges_.put(vertex, new HashSet<Vertex>());
		    }
		    vertex_weights_.put(vertex, weight);
		  }
		  
		  // Uses weight = 1.0. If vertex already exists, its weight is set to
		  // 1.0.
		  public void AddVertex(Vertex vertex) {
		    AddVertex(vertex, 1.0);
		  }
		  
		  public boolean RemoveVertex(Vertex vertex) {
			  if (!vertices_.contains(vertex)) {
			      return false;
			  }
			    vertices_.remove(vertex);
			    vertex_weights_.remove(vertex);
			    HashSet<Vertex> sinks = edges_.get(vertex);
			    for (Vertex v: sinks) {
			    	if (Neighbors(vertex).size() < Neighbors(v).size()) {
			    		edge_weights_.remove(new Pair(vertex, v));
			    	}
			    	else {
			    		edge_weights_.remove(new Pair(v, vertex));
			    	}
			    	edges_.get(v).remove(vertex);
			    }

			    edges_.remove(vertex);
			    return true;
			  }
		  
		  // Add a weighted edge between the vertex1 and vertex2. Calling
		  // AddEdge on a pair of vertices which do not exist in the graph yet
		  // will result in undefined behavior.
		  //
		  // It is legal to call this method repeatedly for the same set of
		  // vertices.
		  public void AddEdge(Vertex vertex1, Vertex vertex2, double weight) {
			  if (!vertices_.contains(vertex1)) {
					System.err.println("!vertices_contains(vertex1) in WeightedGraph.AddEdge");
					return;
				}
				if (!vertices_.contains(vertex2)) {
					System.err.println("!vertices_contains(vertex2) in WeightedGraph.AddEdge");
					return;
				}

				if (edges_.get(vertex1).add(vertex2)) {
					edges_.get(vertex2).add(vertex1);
				}
		    

				// Equal Integer values cannot be ordered with ids like in C++ so must put same weight on both orderings
				//if (Neighbors(vertex1).size() < Neighbors(vertex2).size()) {
		      edge_weights_.put(new Pair(vertex1, vertex2), weight);
		    //} else {
		      edge_weights_.put(new Pair(vertex2, vertex1), weight);
		    //}
		  }
		  
		  // Uses weight = 1.0.
		  public void AddEdge(Vertex vertex1, Vertex vertex2) {
		    AddEdge(vertex1, vertex2, 1.0);
		  }
		  
		  // Calling VertexWeight on a vertex not in the graph will result in
		  // undefined behavior.
		  public double VertexWeight(Vertex vertex) {
			  if (!vertex_weights_.containsKey(vertex)) {
					System.err.println("!vertex_weights_ containsKey(vertex) in WeightedGraph.VertexWeight");
					return Double.NaN;
				}
		    return vertex_weights_.get(vertex);
		  }
		  
		// Calling EdgeWeight on a pair of vertices where either one of the
		  // vertices is not present in the graph will result in undefined
		  // behaviour. If there is no edge connecting vertex1 and vertex2,
		  // the edge weight is zero.
		  public double EdgeWeight(Vertex vertex1, Vertex vertex2) {
			  // Equal Integer values cannot be ordered as in C++
			  //if (Neighbors(vertex1).size() < Neighbors(vertex2).size()) {
				  if (edge_weights_.containsKey(new Pair(vertex1, vertex2))) {
					  return edge_weights_.get(new Pair(vertex1, vertex2));
				  }
				  else {
					  return 0.0;
				  }
				  
		      
		    //} else {
		    	//if (edge_weights_.containsKey(new Pair(vertex2, vertex1))) {
					  //return edge_weights_.get(new Pair(vertex2, vertex1));
				  //}
				  //else {
					  //return 0.0;
				  //}
		      
		    //}
		  }
		  
		// Calling Neighbors on a vertex not in the graph will result in
		  // undefined behaviour.
		  public HashSet<Vertex> Neighbors(Vertex vertex) {
			if (!edges_.containsKey(vertex)) {
				System.err.println("In WeightedGraph Neighbors edges_ does not contain key vertex");
				return null;
			}
		    return edges_.get(vertex);
		  }
		  
		  public HashSet<Vertex> vertices() {
			    return vertices_;
	      }
		  
		  public double InvalidWeight() {
			    return Double.NaN;
		  }

	}

	// A unweighted undirected graph templated over the vertex ids. Vertex
	// should be hashable.
	class Graph<Vertex> {
		private HashSet<Vertex> vertices_;
		private HashMap<Vertex, HashSet<Vertex>> edges_;

		public Graph() {
			vertices_ = new HashSet<Vertex>();
			edges_ = new HashMap<Vertex, HashSet<Vertex>>();
		}

		// Add a vertex.
		public void AddVertex(Vertex vertex) {
			if (vertices_.add(vertex)) {
				edges_.put(vertex, new HashSet<Vertex>());
			}
		}
		
		public boolean RemoveVertex(Vertex vertex) {
		    if (!vertices_.contains(vertex)) {
		      return false;
		    }
		    

		    vertices_.remove(vertex);
		    HashSet<Vertex> sinks = edges_.get(vertex);
		    for (Vertex v: sinks) {
		    	edges_.get(v).remove(vertex);
		    }

		    edges_.remove(vertex);
		    return true;
		}

		// Add an edge between the vertex1 and vertex2. Calling AddEdge on a
		// pair of vertices which do not exist in the graph yet will result
		// in undefined behavior.
		//
		// It is legal to call this method repeatedly for the same set of
		// vertices.
		public void AddEdge(Vertex vertex1, Vertex vertex2) {
			if (!vertices_.contains(vertex1)) {
				System.err.println("!vertices_contains(vertex1) in Graph.AddEdge");
				return;
			}
			if (!vertices_.contains(vertex2)) {
				System.err.println("!vertices_contains(vertex2) in Graph.AddEdge");
				return;
			}

			if (edges_.get(vertex1).add(vertex2)) {
				edges_.get(vertex2).add(vertex1);
			}
		} // public void AddEdge

		public HashSet<Vertex> vertices() {
			return vertices_;
		}

		// Calling Neighbors on a vertex not in the graph will result in
		// undefined behaviour.
		public HashSet<Vertex> Neighbors(Vertex vertex) {
			HashSet<Vertex> value = edges_.get(vertex);
			if (value == null) {
				System.err.println("In public HashSet<Vertex> Neighbors HashMap edges_ does not have key vertex");
				return null;
			}
			return value;
		}
	} // class Graph<Vertex>
	
	class indexValueItem<Vertex> {
		private int index;
		private Vertex value;
		
		public indexValueItem(int index, Vertex value) {
			this.index = index;
			this.value = value;
		}
		
		public int getIndex() {
			return index;
		}
		
		public Vertex getValue() {
			return value;
		}
		
	} 
	
	
	class VertexDegreeLessThan<Vertex> implements Comparator<Vertex> {
		private Graph<Vertex> graph_;

		public VertexDegreeLessThan(Graph<Vertex> graph) {
			graph_ = graph;
		}

		// Compare two vertices of a graph by their degrees
		public int compare(Vertex lhs, Vertex rhs) {
			return (graph_.Neighbors(lhs).size() - graph_.Neighbors(rhs).size());
		}
	} // class VertexDegreesLessThan
	
	// Compare two vertices of a graph by their degrees, if the degrees
	// are equal then order them by their ids.
	class VertexTotalOrdering<Vertex> implements Comparator<indexValueItem<Vertex>> { 
		private Graph<Vertex> graph_;
		
	 public VertexTotalOrdering(Graph<Vertex> graph) {
	      graph_ = graph;
	 }

	 public int compare(indexValueItem<Vertex> lhs, indexValueItem<Vertex> rhs) {
	    if (graph_.Neighbors(lhs.getValue()).size() == graph_.Neighbors(rhs.getValue()).size()) {
	      return (lhs.getIndex() - rhs.getIndex());
	    }
	    return (graph_.Neighbors(lhs.getValue()).size() - graph_.Neighbors(rhs.getValue()).size());
	  }

	 
	};

	public Graph<ParameterBlock> CreateHessianGraph(Program program) {
		Graph<ParameterBlock> graph = new Graph<ParameterBlock>();
		Vector<ParameterBlock> parameter_blocks = program.parameter_blocks();
		for (int i = 0; i < parameter_blocks.size(); ++i) {
			ParameterBlock parameter_block = parameter_blocks.get(i);
			if (!parameter_block.IsConstant()) {
				graph.AddVertex(parameter_block);
			}
		}

		Vector<ResidualBlock> residual_blocks = program.residual_blocks();
		for (int i = 0; i < residual_blocks.size(); ++i) {
			ResidualBlock residual_block = residual_blocks.get(i);
			int num_parameter_blocks = residual_block.NumParameterBlocks();
			ParameterBlock parameter_blocks2[] = residual_block.parameter_blocks();
			for (int j = 0; j < num_parameter_blocks; ++j) {
				if (parameter_blocks2[j].IsConstant()) {
					continue;
				}

				for (int k = j + 1; k < num_parameter_blocks; ++k) {
					if (parameter_blocks2[k].IsConstant()) {
						continue;
					}

					graph.AddEdge(parameter_blocks2[j], parameter_blocks2[k]);
				}
			}
		}

		return graph;
	}
	
	public int ComputeSchurOrdering(Program program,
            Vector<ParameterBlock> ordering) {
		if (ordering == null) {
			System.err.println("Vector<ParameterBlock> ordering == null in ComputeSchurOrdering");
			return -1;
		}
		ordering.clear();
		
		Graph<ParameterBlock> graph = CreateHessianGraph(program);
		int independent_set_size = IndependentSetOrdering(graph, ordering);
		final Vector<ParameterBlock> parameter_blocks = program.parameter_blocks();
		
		// Add the excluded blocks to back of the ordering vector.
		for (int i = 0; i < parameter_blocks.size(); ++i) {
		ParameterBlock parameter_block = parameter_blocks.get(i);
		if (parameter_block.IsConstant()) {
		ordering.add(parameter_block);
		}
		}
		
		return independent_set_size;
		}


	public int ComputeStableSchurOrdering(Program program, Vector<ParameterBlock> ordering) {
		if (ordering == null) {
			System.err.println("Vector<ParameterBlock> ordering == null in ComputeStableSchurOrdering");
			return -1;
		}
		ordering.clear();
		EventLogger event_logger = new EventLogger("ComputeStableSchurOrdering");
		// scoped_ptr<Graph< ParameterBlock*> > graph(CreateHessianGraph(program));
		Graph<ParameterBlock> graph = CreateHessianGraph(program);
		event_logger.AddEvent("CreateHessianGraph");

		Vector<ParameterBlock> parameter_blocks = program.parameter_blocks();
		HashSet<ParameterBlock> vertices = graph.vertices();
		for (int i = 0; i < parameter_blocks.size(); ++i) {
			if (vertices.contains(parameter_blocks.get(i))) {
				ordering.add(parameter_blocks.get(i));
			}
		}
		event_logger.AddEvent("Preordering");

		int independent_set_size = StableIndependentSetOrdering(graph, ordering);
		event_logger.AddEvent("StableIndependentSet");

		// Add the excluded blocks to back of the ordering vector.
		for (int i = 0; i < parameter_blocks.size(); ++i) {
			ParameterBlock parameter_block = parameter_blocks.get(i);
			if (parameter_block.IsConstant()) {
				ordering.add(parameter_block);
			}
		}
		event_logger.AddEvent("ConstantParameterBlocks");

		return independent_set_size;
	}
	
	public <Vertex> int IndependentSetOrdering(Graph<Vertex> graph,
	                           Vector<Vertex> ordering) {
	  int i;
	  HashSet<Vertex> vertices = graph.vertices();
	  int num_vertices = vertices.size();

	  if (ordering == null) {
			System.err.println("Vector<Vertex> ordering == null in IndependentSetOrdering");
			return -1;
      }
	  ordering.clear();
	  ordering.ensureCapacity(num_vertices);

	  // Colors for labeling the graph during the BFS.
	  final byte kWhite = 0;
	  final byte kGrey = 1;
	  final byte kBlack = 2;

	  // Mark all vertices white.
	  HashMap<Vertex, Byte> vertex_color = new HashMap<Vertex, Byte>();
	  Vector<Vertex> vertex_queue = new Vector<Vertex>();
	  Vector<indexValueItem<Vertex>> vector_index = new Vector<indexValueItem<Vertex>>();
	  int index = 0;
	  for (Vertex v : vertices) {
			vertex_color.put(v, kWhite);
			vector_index.add(new indexValueItem<Vertex>(index++,v));
		}
	  

	  VertexTotalOrdering<Vertex> VT = new VertexTotalOrdering<Vertex>(graph);
	  Collections.sort(vector_index, VT);
	  for (i = 0; i < vector_index.size(); i++) {
		  vertex_queue.add(vector_index.get(i).getValue());
	  }

	  // Iterate over vertex_queue. Pick the first white vertex, add it
	  // to the independent set. Mark it black and its neighbors grey.
	  for (Vertex vertex : vertex_queue) {
			if (vertex_color.get(vertex) != kWhite) {
				continue;
			}
			
			ordering.add(vertex);
			vertex_color.put(vertex, kBlack);
			HashSet<Vertex> neighbors = graph.Neighbors(vertex);
			for (Vertex it : neighbors) {
				vertex_color.put(it, kGrey);
			}
	  }

	  int independent_set_size = ordering.size();

	  // Iterate over the vertices and add all the grey vertices to the
	  // ordering. At this stage there should only be black or grey
	  // vertices in the graph.
	  for (Vertex vertex : vertex_queue) {
			if (vertex_color.get(vertex) == kWhite) {
				System.err.println("Unexpected kWhite found in vertex_color in IndependentSetOrdering");
				return -1;
			}
			if (vertex_color.get(vertex) != kBlack) {
				ordering.add(vertex);
			}
	  }
	  

	  if (ordering.size() != num_vertices) {
			System.err.println("ordering.size() != num_vertices in IndependentSetOrdering");
			return -1;
	  }
	  return independent_set_size;
	}

	// Same as above with one important difference. The ordering parameter
	// is an input/output parameter which carries an initial ordering of
	// the vertices of the graph. The greedy independent set algorithm
	// starts by sorting the vertices in increasing order of their
	// degree. The input ordering is used to stabilize this sort, i.e., if
	// two vertices have the same degree then they are ordered in the same
	// order in which they occur in "ordering".
	//
	// This is useful in eliminating non-determinism from the Schur
	// ordering algorithm over all.
	public <Vertex> int StableIndependentSetOrdering(Graph<Vertex> graph, Vector<Vertex> ordering) {
		if (ordering == null) {
			System.err.println("Vector<Vertex> ordering == null in StableIndependentSetOrdering");
			return -1;
		}
		HashSet<Vertex> vertices = graph.vertices();
		int num_vertices = vertices.size();
		if (vertices.size() != ordering.size()) {
			System.err.println("vertices.size() != ordering.size() in StableIndependentSetOrdering");
			return -1;
		}

		// Colors for labeling the graph during the BFS.
		final byte kWhite = 0;
		final byte kGrey = 1;
		final byte kBlack = 2;

		List<Vertex> vertex_queue = new ArrayList<Vertex>();
		for (int i = 0; i < ordering.size(); i++) {
			vertex_queue.add(ordering.get(i));
		}

		VertexDegreeLessThan<Vertex> VD = new VertexDegreeLessThan<Vertex>(graph);
		Collections.sort(vertex_queue, VD);

		// Mark all vertices white.
		HashMap<Vertex, Byte> vertex_color = new HashMap<Vertex, Byte>();
		for (Vertex v : vertices) {
			vertex_color.put(v, kWhite);
		}

		ordering.clear();
		ordering.ensureCapacity(num_vertices);
		// Iterate over vertex_queue. Pick the first white vertex, add it
		// to the independent set. Mark it black and its neighbors grey.
		for (Vertex vertex : vertex_queue) {
			if (vertex_color.get(vertex) != kWhite) {
				continue;
			}

			ordering.add(vertex);
			vertex_color.put(vertex, kBlack);
			HashSet<Vertex> neighbors = graph.Neighbors(vertex);
			for (Vertex it : neighbors) {
				vertex_color.put(it, kGrey);
			}
		}

		int independent_set_size = ordering.size();

		// Iterate over the vertices and add all the grey vertices to the
		// ordering. At this stage there should only be black or grey
		// vertices in the graph.
		for (Vertex vertex : vertex_queue) {
			if (vertex_color.get(vertex) == kWhite) {
				System.err.println("Unexpected kWhite found in vertex_color in StableIndependentSetOrdering");
				return -1;
			}
			if (vertex_color.get(vertex) != kBlack) {
				ordering.add(vertex);
			}
		}

		if (ordering.size() != num_vertices) {
			System.err.println("ordering.size() != num_vertices in StableIndependentSetOrdering");
			return -1;
		}
		return independent_set_size;
	}

	class EventLogger {
		private double start_time_;
		private double last_event_time_;
		String events_;

		public EventLogger(String logger_name) {
			start_time_ = 1.0E-3 * System.currentTimeMillis();
			last_event_time_ = start_time_;
			events_ = "\n" + logger_name + "\n                                   Delta   Cumulative\n";

		}

		public void AddEvent(String event_name) {
			if (3 > MAX_LOG_LEVEL) {
				return;
			}

			double current_time = 1.0E-3 * System.currentTimeMillis();
			double relative_time_delta = current_time - last_event_time_;
			double absolute_time_delta = current_time - start_time_;
			last_event_time_ = current_time;

			events_ = events_
					+ String.format("  %30s : %10.5f   %10.5f\n", event_name, relative_time_delta, absolute_time_delta);

		}
	} // class EventLogger

	public String LinearSolverTypeToString(LinearSolverType type) {
		switch (type) {
		case DENSE_NORMAL_CHOLESKY:
			return "DENSE_NORMAL_CHOLESKY";
		case DENSE_QR:
			return "DENSE_QR";
		case SPARSE_NORMAL_CHOLESKY:
			return "NORMAL_CHOLESKY";
		case DENSE_SCHUR:
			return "DENSE_SCHUR";
		case SPARSE_SCHUR:
			return "SPARSE_SCHUR";
		case ITERATIVE_SCHUR:
			return "ITERATIVE_SCHUR";
		case CGNR:
			return "CGNR";
		default:
			return "UNKNOWN";
		}
	}

	public String PreconditionerTypeToString(PreconditionerType type) {
		switch (type) {
		case IDENTITY:
			return "IDENTITY";
		case JACOBI:
			return "JACOBI";
		case SCHUR_JACOBI:
			return "SCHUR_JACOBI";
		case CLUSTER_JACOBI:
			return "CLUSTER_JACOBI";
		case CLUSTER_TRIDIAGONAL:
			return "CLUSTER_TRIDIAGONAL";
		default:
			return "UNKNOWN";
		}
	}
	
	// Options pertaining to numeric differentiation (e.g., convergence criteria,
	// step sizes).
     class NumericDiffOptions {
	    	// Numeric differentiation step size (multiplied by parameter block's
	   	  // order of magnitude). If parameters are close to zero, the step size
	   	  // is set to sqrt(machine_epsilon).
	   	  public double relative_step_size;
	
	   	  // Initial step size for Ridders adaptive numeric differentiation (multiplied
	   	  // by parameter block's order of magnitude).
	   	  // If parameters are close to zero, Ridders' method sets the step size
	   	  // directly to this value. This parameter is separate from
	   	  // "relative_step_size" in order to set a different default value.
	   	  //
	   	  // Note: For Ridders' method to converge, the step size should be initialized
	   	  // to a value that is large enough to produce a significant change in the
	   	  // function. As the derivative is estimated, the step size decreases.
	   	  public double ridders_relative_initial_step_size;
	
	   	  // Maximal number of adaptive extrapolations (sampling) in Ridders' method.
	   	  public int max_num_ridders_extrapolations;
	
	   	  // Convergence criterion on extrapolation error for Ridders adaptive
	   	  // differentiation. The available error estimation methods are defined in
	   	  // NumericDiffErrorType and set in the "ridders_error_method" field.
	   	  public double ridders_epsilon;
	
	   	  // The factor in which to shrink the step size with each extrapolation in
	   	  // Ridders' method.
	   	  public double ridders_step_shrink_factor;
		  public NumericDiffOptions() {
		    relative_step_size = 1e-6;
		    ridders_relative_initial_step_size = 1e-2;
		    max_num_ridders_extrapolations = 10;
		    ridders_epsilon = 1e-12;
		    ridders_step_shrink_factor = 2.0;
		  }

	  
	};

	public PreconditionerType PreconditionerForZeroEBlocks(PreconditionerType preconditioner_type) {
		if (preconditioner_type == PreconditionerType.SCHUR_JACOBI
				|| preconditioner_type == PreconditionerType.CLUSTER_JACOBI
				|| preconditioner_type == PreconditionerType.CLUSTER_TRIDIAGONAL) {
			return PreconditionerType.JACOBI;
		}
		return preconditioner_type;
	}

	class LineSearchPreprocessor extends Preprocessor {
		public LineSearchPreprocessor() {
			super();
		}

		public boolean Preprocess(SolverOptions options, ProblemImpl problem, PreprocessedProblem pp) {
			if (pp == null) {
				System.err.println("PreprocessedProblem pp == null in TrustRegionPreProcessor Preprocess");
				return false;
			}
			pp.options = options;
			// ChangeNumThreadsIfNeeded(pp.options);

			pp.problem = problem;
			Program program = problem.mutable_program();
			if (!IsProgramValid(program, pp.error)) {
				return false;
			}

			pp.reduced_program = program.CreateReducedProgram(pp.removed_parameter_blocks, pp.fixed_cost, pp.error);

			if (pp.reduced_program == null) {
				return false;
			}

			if (pp.reduced_program.NumParameterBlocks() == 0) {
				return true;
			}

			if (!SetupEvaluator(pp)) {
				return false;
			}

			SetupCommonMinimizerOptions(pp);

			return true;
		}

		public boolean IsProgramValid(Program program, String error[]) {
			if (program.IsBoundsConstrained()) {
				error[0] = "LINE_SEARCH Minimizer does not support bounds.";
				return false;
			}
			return program.ParameterBlocksAreFinite(error);
		}

		public boolean SetupEvaluator(PreprocessedProblem pp) {
			pp.evaluator_options = new EvaluatorOptions();
			// This ensures that we get a Block Jacobian Evaluator without any
			// requirement on orderings.
			pp.evaluator_options.linear_solver_type = LinearSolverType.CGNR;
			pp.evaluator_options.num_eliminate_blocks = 0;
			pp.evaluator_options.num_threads = pp.options.num_threads;
			pp.evaluator_options.context = pp.problem.context();
			pp.evaluator_options.evaluation_callback = pp.options.evaluation_callback;
			pp.evaluator = Create(pp.evaluator_options, pp.reduced_program, pp.error);
			return (pp.evaluator != null);
		}

	} // class LineSearchPreProcessor

	// A PreprocessedProblem is the result of running the Preprocessor on
	// a Problem and Solver::Options object.
	class PreprocessedProblem {
		String error[];
		SolverOptions options;
		LinearSolverOptions linear_solver_options;
		EvaluatorOptions evaluator_options;
		MinimizerOptions minimizer_options;

		ProblemImpl problem;
		// scoped_ptr<ProblemImpl> gradient_checking_problem;
		ProblemImpl gradient_checking_problem;
		// scoped_ptr<Program> reduced_program;
		Program reduced_program;
		// scoped_ptr<LinearSolver> linear_solver;
		LinearSolver linear_solver;
		// scoped_ptr<IterationCallback> logging_callback;
		IterationCallback logging_callback;
		// scoped_ptr<IterationCallback> state_updating_callback;
		IterationCallback state_updating_callback;

		// shared_ptr<Evaluator> evaluator;
		Evaluator evaluator;
		// shared_ptr<CoordinateDescentMinimizer> inner_iteration_minimizer;
		CoordinateDescentMinimizer inner_iteration_minimizer;

		Vector<double[]> removed_parameter_blocks;
		Vector<Double> reduced_parameters;
		double fixed_cost[];

		public PreprocessedProblem() {
			fixed_cost = new double[] { 0.0 };
			error = new String[1];
			removed_parameter_blocks = new Vector<double[]>();
			reduced_parameters = new Vector<Double>();
			linear_solver_options = new LinearSolverOptions();
			minimizer_options = new MinimizerOptions();
			evaluator_options = new EvaluatorOptions();
		}

	} // class Preprocessed

	public void SetupCommonMinimizerOptions(PreprocessedProblem pp) {
		SolverOptions options = pp.options;
		Program program = pp.reduced_program;
		// Assuming that the parameter blocks in the program have been
		// reordered as needed, extract them into a contiguous vector.
		while (pp.reduced_parameters.size() > program.NumParameters()) {
			pp.reduced_parameters.removeElementAt(pp.reduced_parameters.size() - 1);
		}
		while (pp.reduced_parameters.size() < program.NumParameters()) {
			pp.reduced_parameters.add(0.0);
		}
		program.ParameterBlocksToStateVector(pp.reduced_parameters);

		pp.minimizer_options = new MinimizerOptions(options);
		pp.minimizer_options.evaluator = pp.evaluator;

		if (options.logging_type != LoggingType.SILENT) {
			pp.logging_callback = new LoggingCallback(options.minimizer_type, options.minimizer_progress_to_stdout);
			pp.minimizer_options.callbacks.add(0, pp.logging_callback);
		}

		if (options.update_state_every_iteration) {
			pp.state_updating_callback = new StateUpdatingCallback(program, pp.reduced_parameters);
			// This must get pushed to the front of the callbacks so that it
			// is run before any of the user callbacks.
			pp.minimizer_options.callbacks.add(0, pp.state_updating_callback);
		}
	}

	class Block {
		public int size;
		public int position; // Position along the row/column.

		public Block() {
			size = -1;
			position = -1;
		}

		public Block(int size_, int position_) {
			size = size_;
			position = position_;
		}

	};

	class Cell {
		// Column or row block id as the case maybe.
		public int block_id;
		// Where in the values array of the jacobian is this cell located.
		public int position;

		public Cell() {
			block_id = -1;
			position = -1;
		}

		public Cell(int block_id_, int position_) {
			block_id = block_id_;
			position = position_;
		}
	};

	class CompressedList {
		Block block;
		Vector<Cell> cells;

		public CompressedList() {
			block = new Block();
			cells = new Vector<Cell>();
		}

		// Construct a CompressedList with the cells containing num_cells
		// entries.
		public CompressedList(int num_cells) {
			int i;
			block = new Block();
			cells = new Vector<Cell>(num_cells);
			for (i = 0; i < num_cells; i++) {
				cells.add(new Cell());
			}
		}

	};

	// typedef CompressedList CompressedRow;
	class CompressedRowBlockStructure {
		public Vector<Block> cols;
		public Vector<CompressedList> rows;

		public CompressedRowBlockStructure() {
			cols = new Vector<Block>();
			rows = new Vector<CompressedList>();
		}
	};

	class BlockSparseMatrix extends SparseMatrix {
		private int num_rows_;
		private int num_cols_;
		private int num_nonzeros_;
		private int max_num_nonzeros_;
		// scoped_array<double> values_;
		private double values_[];
		// scoped_ptr<CompressedRowBlockStructure> block_structure_;
		private CompressedRowBlockStructure block_structure_;
		public RandomMatrixOptions randomMatrixOptions;

		// Construct a block sparse matrix with a fully initialized
		// CompressedRowBlockStructure objected. The matrix takes over
		// ownership of this object and destroys it upon destruction.
		//
		// TODO(sameeragarwal): Add a function which will validate legal
		// CompressedRowBlockStructure objects.
		public BlockSparseMatrix(CompressedRowBlockStructure block_structure) {
			randomMatrixOptions = new RandomMatrixOptions();
			num_rows_ = 0;
			num_cols_ = 0;
			num_nonzeros_ = 0;
			values_ = null;
			block_structure_ = block_structure;
			if (block_structure == null) {
				System.err.println("In public BlockSparseMatrix block_structure == null");
				return;
			}

			// Count the number of columns in the matrix.
			for (int i = 0; i < block_structure_.cols.size(); ++i) {
				num_cols_ += block_structure_.cols.get(i).size;
			}

			// Count the number of non-zero entries and the number of rows in
			// the matrix.
			for (int i = 0; i < block_structure_.rows.size(); ++i) {
				int row_block_size = block_structure_.rows.get(i).block.size;
				num_rows_ += row_block_size;

				Vector<Cell> cells = block_structure_.rows.get(i).cells;
				for (int j = 0; j < cells.size(); ++j) {
					int col_block_id = cells.get(j).block_id;
					int col_block_size = block_structure_.cols.get(col_block_id).size;
					num_nonzeros_ += col_block_size * row_block_size;
				}
			}

			if (num_rows_ < 0) {
				System.err.println("In public BlockSparseMatrix num_rows_ < 0");
				return;
			}
			if (num_cols_ < 0) {
				System.err.println("In public BlockSparseMatrix num_cols_ < 0");
				return;
			}
			if (num_nonzeros_ < 0) {
				System.err.println("In public BlockSparseMatrix num_nonzeros_ < 0");
				return;
			}
			if (2 <= MAX_LOG_LEVEL) {
				Preferences.debug("Allocating values array with " + num_nonzeros_ + " doubles\n",
						Preferences.DEBUG_ALGORITHM);
			}
			values_ = new double[num_nonzeros_];
			max_num_nonzeros_ = num_nonzeros_;
		}

		public BlockSparseMatrix() {

		}

		// Implementation of SparseMatrix interface.
		public void SetZero() {
			for (int i = 0; i < num_nonzeros_; i++) {
				values_[i] = 0.0;
			}
		}

		public void RightMultiply(double x[], double y[]) {
			if (x == null) {
				System.err.println("x == null in RightMultiply");
				return;
			}
			if (y == null) {
				System.err.println("y == null in RightMultiply");
				return;
			}

			for (int i = 0; i < block_structure_.rows.size(); ++i) {
				int row_block_pos = block_structure_.rows.get(i).block.position;
				int row_block_size = block_structure_.rows.get(i).block.size;
				Vector<Cell> cells = block_structure_.rows.get(i).cells;
				for (int j = 0; j < cells.size(); ++j) {
					int col_block_id = cells.get(j).block_id;
					int col_block_size = block_structure_.cols.get(col_block_id).size;
					int col_block_pos = block_structure_.cols.get(col_block_id).position;
					MatrixVectorMultiply(DYNAMIC, DYNAMIC, 1, values_, cells.get(j).position, row_block_size,
							col_block_size, x, col_block_pos, y, row_block_pos);
				}
			}

		}

		public void LeftMultiply(double x[], double y[]) {
			if (x == null) {
				System.err.println("x == null in LeftMultiply");
				return;
			}
			if (y == null) {
				System.err.println("y == null in LeftMultiply");
				return;
			}

			for (int i = 0; i < block_structure_.rows.size(); ++i) {
				int row_block_pos = block_structure_.rows.get(i).block.position;
				int row_block_size = block_structure_.rows.get(i).block.size;
				Vector<Cell> cells = block_structure_.rows.get(i).cells;
				for (int j = 0; j < cells.size(); ++j) {
					int col_block_id = cells.get(j).block_id;
					int col_block_size = block_structure_.cols.get(col_block_id).size;
					int col_block_pos = block_structure_.cols.get(col_block_id).position;
					MatrixTransposeVectorMultiply(DYNAMIC, DYNAMIC, 1, values_, cells.get(j).position, row_block_size,
							col_block_size, x, row_block_pos, y, col_block_pos);
				}
			}

		}

		public void SquaredColumnNorm(double x[]) {
			if (x == null) {
				System.err.println("x == null in SquaredColumnNorm");
				return;
			}
			for (int i = 0; i < num_cols_; i++) {
				x[i] = 0.0;
			}
			for (int i = 0; i < block_structure_.rows.size(); ++i) {
				int row_block_size = block_structure_.rows.get(i).block.size;
				Vector<Cell> cells = block_structure_.rows.get(i).cells;
				for (int j = 0; j < cells.size(); ++j) {
					int col_block_id = cells.get(j).block_id;
					int col_block_size = block_structure_.cols.get(col_block_id).size;
					int col_block_pos = block_structure_.cols.get(col_block_id).position;
					double m[][] = new double[row_block_size][col_block_size];
					for (int row = 0; row < row_block_size; row++) {
						for (int col = 0; col < col_block_size; col++) {
							int index = cells.get(j).position + row * col_block_size + col;
							if (index < values_.length) {
								m[row][col] = values_[index];
							}
						}
					}
					double squaredNorm[] = new double[col_block_size];
					for (int col = 0; col < col_block_size; col++) {
						for (int row = 0; row < row_block_size; row++) {
							squaredNorm[col] += m[row][col] * m[row][col];
						}
					}
					for (int col = 0; col < col_block_size; col++) {
						x[col_block_pos + col] += squaredNorm[col];
					}
				}
			}

		}

		public void ScaleColumns(double scale[]) {
			if (scale == null) {
				System.err.println("scale == null in ScaleColumns");
				return;
			}

			for (int i = 0; i < block_structure_.rows.size(); ++i) {
				int row_block_size = block_structure_.rows.get(i).block.size;
				Vector<Cell> cells = block_structure_.rows.get(i).cells;
				for (int j = 0; j < cells.size(); ++j) {
					int col_block_id = cells.get(j).block_id;
					int col_block_size = block_structure_.cols.get(col_block_id).size;
					int col_block_pos = block_structure_.cols.get(col_block_id).position;
					double m[][] = new double[row_block_size][col_block_size];
					for (int row = 0; row < row_block_size; row++) {
						for (int col = 0; col < col_block_size; col++) {
							int index = cells.get(j).position + row * col_block_size + col;
							if (index < values_.length) {
								m[row][col] = values_[index];
							}
						}
					}
					double diagonal[] = new double[col_block_size];
					for (int col = 0; col < col_block_size; col++) {
						diagonal[col] = scale[col_block_pos + col];
						for (int row = 0; row < row_block_size; row++) {
							m[row][col] = m[row][col] * diagonal[col];
						}
					}
					for (int row = 0; row < row_block_size; row++) {
						for (int col = 0; col < col_block_size; col++) {
							int index = cells.get(j).position + row * col_block_size + col;
							if (index < values_.length) {
								values_[index] = m[row][col];
							}
						}
					}
				}
			}

		}

		// change return type from void to Matrix so that an input matrix of the
		// correct dimensions is no longer required
		public Matrix ToDenseMatrix() {

			double array[][] = new double[num_rows_][num_cols_];
			

			for (int i = 0; i < block_structure_.rows.size(); ++i) {
				int row_block_pos = block_structure_.rows.get(i).block.position;
				int row_block_size = block_structure_.rows.get(i).block.size;
				Vector<Cell> cells = block_structure_.rows.get(i).cells;
				for (int j = 0; j < cells.size(); ++j) {
					int col_block_id = cells.get(j).block_id;
					int col_block_size = block_structure_.cols.get(col_block_id).size;
					int col_block_pos = block_structure_.cols.get(col_block_id).position;
					int jac_pos = cells.get(j).position;
					for (int row = 0; row < row_block_size; row++) {
						for (int col = 0; col < col_block_size; col++) {
							array[row_block_pos + row][col_block_pos + col] += values_[jac_pos + row * col_block_size + col];
						}
					}
				}
			}
			Matrix m = new Matrix(array);
			return m;
		}

		public void ToTextFile(File file) {
			if (file == null) {
				System.err.println("file == null in ToTextFile");
				return;
			}
            FileWriter fw = null;
			try {
				fw = new FileWriter(file);
			} catch (IOException e) {
				System.err.println("IOException in ToTextFile on new FileWriter(file)");
				return;
			}
			for (int i = 0; i < block_structure_.rows.size(); ++i) {
				int row_block_pos = block_structure_.rows.get(i).block.position;
				int row_block_size = block_structure_.rows.get(i).block.size;
				Vector<Cell> cells = block_structure_.rows.get(i).cells;
				for (int j = 0; j < cells.size(); ++j) {
					int col_block_id = cells.get(j).block_id;
					int col_block_size = block_structure_.cols.get(col_block_id).size;
					int col_block_pos = block_structure_.cols.get(col_block_id).position;
					int jac_pos = cells.get(j).position;
					for (int r = 0; r < row_block_size; ++r) {
						for (int c = 0; c < col_block_size; ++c) {
							String str = String.format("% 10d % 10d %17f\n", row_block_pos + r, col_block_pos + c,
									values_[jac_pos++]);
							try {
								fw.write(str, 0, str.length());
							} catch (IOException e) {
								System.err.println("IOException in ToTextFile on fw.write(str,0,str.length())");
								return;
							}
						}
					}
				}
			}
			try {
			    fw.close();
			}
			catch (IOException e) {
				System.err.println("IOException in ToTextFile on fw.close()");
			}

		}

		public int num_rows() {
			return num_rows_;
		}

		public int num_cols() {
			return num_cols_;
		}

		public int num_nonzeros() {
			return num_nonzeros_;
		}

		public double[] values() {
			return values_;
		}

		public double[] mutable_values() {
			return values_;
		}
		
		public void ToTripletSparseMatrix(TripletSparseMatrix matrix) {
			  if (matrix == null) {
				  System.err.println("In ToTripletSparseMatrix matrix == null");
				  return;
			  }

			  matrix.Reserve(num_nonzeros_);
			  matrix.Resize(num_rows_, num_cols_);
			  matrix.SetZero();

			  for (int i = 0; i < block_structure_.rows.size(); ++i) {
			    int row_block_pos = block_structure_.rows.get(i).block.position;
			    int row_block_size = block_structure_.rows.get(i).block.size;
			    Vector<Cell> cells = block_structure_.rows.get(i).cells;
			    for (int j = 0; j < cells.size(); ++j) {
			      int col_block_id = cells.get(j).block_id;
			      int col_block_size = block_structure_.cols.get(col_block_id).size;
			      int col_block_pos = block_structure_.cols.get(col_block_id).position;
			      int jac_pos = cells.get(j).position;
			       for (int r = 0; r < row_block_size; ++r) {
			        for (int c = 0; c < col_block_size; ++c, ++jac_pos) {
			          matrix.mutable_rows()[jac_pos] = row_block_pos + r;
			          matrix.mutable_cols()[jac_pos] = col_block_pos + c;
			          matrix.mutable_values()[jac_pos] = values_[jac_pos];
			        }
			      }
			    }
			  }
			  matrix.set_num_nonzeros(num_nonzeros_);
	
		}
		
		public CompressedRowBlockStructure block_structure() {
			return block_structure_;
		}

		  // Append the contents of m to the bottom of this matrix. m must
		  // have the same column blocks structure as this matrix.
		  public void AppendRows(BlockSparseMatrix m) {
			  if (m.num_cols() != num_cols()) {
				  System.err.println("In AppendRows(BlockSparseMatrix m) m.num_cols != num_cols");
				  return;
			  }
			  CompressedRowBlockStructure m_bs = m.block_structure();
			  if (m_bs.cols.size() != block_structure_.cols.size()) {
				  System.err.println("In AppendRows(BlockSparseMatrix m) m_bs.cols.size() != block_structure_.cols.size()");
				  return;
			  }

			  int old_num_nonzeros = num_nonzeros_;
			  int old_num_row_blocks = block_structure_.rows.size();
			  for (int i = 0; i < m_bs.rows.size(); i++) {
				  block_structure_.rows.add(new CompressedList());
			  }

			  for (int i = 0; i < m_bs.rows.size(); ++i) {
			    CompressedList m_row = m_bs.rows.get(i);
			    CompressedList row = block_structure_.rows.get(old_num_row_blocks + i);
			    row.block.size = m_row.block.size;
			    row.block.position = num_rows_;
			    num_rows_ += m_row.block.size;
			    row.cells.clear();
			    for (int c = 0; c < m_row.cells.size(); ++c) {
			      int block_id = m_row.cells.get(c).block_id;
			      row.cells.add(new Cell());
			      row.cells.get(c).block_id = block_id;
			      row.cells.get(c).position = num_nonzeros_;
			      num_nonzeros_ += m_row.block.size * m_bs.cols.get(block_id).size;
			    }
			  }

			  if (num_nonzeros_ > max_num_nonzeros_) {
			    double new_values[] = new double[num_nonzeros_];
			    for (int i = 0; i < old_num_nonzeros; i++) {
			    	new_values[i] = values_[i];
			    }
			    values_ = new_values;
			    max_num_nonzeros_ = num_nonzeros_;
			  }

			  for (int i = 0; i < m.num_nonzeros(); i++) {
				  values_[old_num_nonzeros + i] = m.values()[i];
			  }

		  }

		  // Delete the bottom delta_rows_blocks.
		  public void DeleteRowBlocks(int delta_row_blocks) {
			  int num_row_blocks = block_structure_.rows.size();
			  int delta_num_nonzeros = 0;
			  int delta_num_rows = 0;
			  Vector<Block> column_blocks = block_structure_.cols;
			  for (int i = 0; i < delta_row_blocks; ++i) {
			    CompressedList row = block_structure_.rows.get(num_row_blocks - i - 1);
			    delta_num_rows += row.block.size;
			    for (int c = 0; c < row.cells.size(); ++c) {
			      Cell cell = row.cells.get(c);
			      delta_num_nonzeros += row.block.size * column_blocks.get(cell.block_id).size;
			    }
			  }
			  num_nonzeros_ -= delta_num_nonzeros;
			  num_rows_ -= delta_num_rows;
			  for (int i = 0; i < delta_row_blocks; i++) {
				  block_structure_.rows.remove(block_structure_.rows.size()-1);
			  }	  
		  }

		  class RandomMatrixOptions {
			  public int num_row_blocks;
			  public int min_row_block_size;
			  public int max_row_block_size;
			  public int num_col_blocks;
			  public int min_col_block_size;
			  public int max_col_block_size;

			    // 0 < block_density <= 1 is the probability of a block being
			    // present in the matrix. A given random matrix will not have
			    // precisely this density.
			    public double block_density;

			    // If col_blocks is non-empty, then the generated random matrix
			    // has this block structure and the column related options in this
			    // struct are ignored.
			    public Vector<Block> col_blocks;
		    public RandomMatrixOptions() {
		    	  col_blocks = new Vector<Block>();
		          num_row_blocks = 0;
		          min_row_block_size = 0;
		          max_row_block_size = 0;
		          num_col_blocks = 0;
		          min_col_block_size = 0;
		          max_col_block_size = 0;
		          block_density = 0.0;
		    }

		    
		  } // class RandomMatrixOptions

		 
	} // class BlockSparseMatrix extends SparseMatrix
	
	public BlockSparseMatrix CreateDiagonalMatrix(double diagonal[], Vector<Block> column_blocks) {
		  // Create the block structure for the diagonal matrix.
		  CompressedRowBlockStructure bs = new CompressedRowBlockStructure();
		  bs.cols = column_blocks;
		  int position = 0;
		  for (int i = 0; i < column_blocks.size(); i++) {
			  bs.rows.add(new CompressedList(1));
		  }
		  for (int i = 0; i < column_blocks.size(); ++i) {
		    CompressedList row = bs.rows.get(i);
		    row.block = column_blocks.get(i);
		    Cell cell = row.cells.get(0);
		    cell.block_id = i;
		    cell.position = position;
		    position += row.block.size * row.block.size;
		  }

		  // Create the BlockSparseMatrix with the given block structure.
		  BlockSparseMatrix matrix = new BlockSparseMatrix(bs);
		  matrix.SetZero();

		  // Fill the values array of the block sparse matrix.
		  int diagonal_offset = 0;
		  int values_offset = 0;
		  double values[] = matrix.mutable_values();
		  for (int i = 0; i < column_blocks.size(); ++i) {
		    int size = column_blocks.get(i).size;
		    for (int j = 0; j < size; ++j) {
		      // (j + 1) * size is compact way of accessing the (j,j) entry.
		      values[values_offset + j * (size + 1)] = diagonal[diagonal_offset + j];
		    }
		    diagonal_offset += size;
		    values_offset += size * size;
		  }

		  return matrix;
		}
	
	
	// Create a random BlockSparseMatrix whose entries are normally
	// distributed and whose structure is determined by
	// RandomMatrixOptions.
	public BlockSparseMatrix CreateRandomMatrix(BlockSparseMatrix.RandomMatrixOptions options) {
		  if (options.num_row_blocks <= 0) {
			  System.err.println("In BlockSparseMatrix CreateRandomMatrix options.num_row_blocks <= 0");
			  return null;
		  }
		  if (options.min_row_block_size <= 0) {
			  System.err.println("In BlockSparseMatrix CreateRandomMatrix options.min_row_block_size <= 0");
			  return null;
		  }
		  if (options.max_row_block_size <= 0) {
			  System.err.println("In BlockSparseMatrix CreateRandomMatrix options.max_row_block_size <= 0");
			  return null;
		  }
		  if (options.min_row_block_size > options.max_row_block_size) {
			  System.err.println("In BlockSparseMatrix CreateRandomMatrix options.min_row_block_size > options.max_row_block_size");
			  return null;  
		  }
		  if (options.block_density <= 0) {
			  System.err.println("In BlockSparseMatrix CreateRandomMatrix options.block_density <= 0");
			  return null;
		  }
		  if (options.block_density > 1.0) {
			  System.err.println("In BlockSparseMatrix CreateRandomMatrix options.block_density > 1.0");
			  return null;  
		  }

		  CompressedRowBlockStructure bs = new CompressedRowBlockStructure();
		  if (options.col_blocks.isEmpty()) {
			  if (options.num_col_blocks <= 0) {
				  System.err.println("In BlockSparseMatrix CreateRandomMatrix options.num_col_blocks <= 0");
				  return null;
			  }
			  if (options.min_col_block_size <= 0) {
				  System.err.println("In BlockSparseMatrix CreateRandomMatrix options.min_col_block_size <= 0");
				  return null;
			  }
			  if (options.max_col_block_size <= 0) {
				  System.err.println("In BlockSparseMatrix CreateRandomMatrix options.max_col_block_size <= 0");
				  return null;
			  }
			  if (options.min_col_block_size > options.max_col_block_size) {
				  System.err.println("In BlockSparseMatrix CreateRandomMatrix options.min_col_block_size > options.max_col_block_size");
				  return null;  
			  }

		    // Generate the col block structure.
		    int col_block_position = 0;
		    for (int i = 0; i < options.num_col_blocks; ++i) {
		      // Generate a random integer in [min_col_block_size, max_col_block_size]
		      int delta_block_size =
		          Uniform(options.max_col_block_size - options.min_col_block_size);
		      int col_block_size = options.min_col_block_size + delta_block_size;
		      bs.cols.add(new Block(col_block_size, col_block_position));
		      col_block_position += col_block_size;
		    }
		  } else {
		    bs.cols = options.col_blocks;
		  }

		  boolean matrix_has_blocks = false;
		  while (!matrix_has_blocks) {
			if (1 <= MAX_LOG_LEVEL) {
				Preferences.debug("Clearing\n", Preferences.DEBUG_ALGORITHM);
			}
		    bs.rows.clear();
		    int row_block_position = 0;
		    int value_position = 0;
		    for (int r = 0; r < options.num_row_blocks; ++r) {

		      int delta_block_size =
		          Uniform(options.max_row_block_size - options.min_row_block_size);
		      int row_block_size = options.min_row_block_size + delta_block_size;
		      bs.rows.add(new CompressedList());
		      CompressedList row = bs.rows.get(bs.rows.size()-1);
		      row.block.size = row_block_size;
		      row.block.position = row_block_position;
		      row_block_position += row_block_size;
		      for (int c = 0; c < bs.cols.size(); ++c) {
		        if (RandDouble() > options.block_density) continue;

		        row.cells.add(new Cell());
		        Cell cell = row.cells.get(row.cells.size()-1);
		        cell.block_id = c;
		        cell.position = value_position;
		        value_position += row_block_size * bs.cols.get(c).size;
		        matrix_has_blocks = true;
		      }
		    }
		  }

		  BlockSparseMatrix matrix = new BlockSparseMatrix(bs);
		  double values[] = matrix.mutable_values();
		  for (int i = 0; i < matrix.num_nonzeros(); ++i) {
		    values[i] = RandNormal();
		  }

		  return matrix;
		} // BlockSparseMatrix CreateRandomMatrix
	
	// An implementation of the SparseMatrix interface to store and
	// manipulate sparse matrices in triplet (i,j,s) form.  This object is
	// inspired by the design of the cholmod_triplet struct used in the
	// SuiteSparse package and is memory layout compatible with it.
	class TripletSparseMatrix extends SparseMatrix {
		  public RandomMatrixOptions randomMatrixOptions;
		  private int num_rows_;
		  private int num_cols_;
		  private int max_num_nonzeros_;
		  private int num_nonzeros_;

		  // The data is stored as three arrays. For each i, values_[i] is
		  // stored at the location (rows_[i], cols_[i]). If the there are
		  // multiple entries with the same (rows_[i], cols_[i]), the values_
		  // entries corresponding to them are summed up.
		  //scoped_array<int> rows_;
		  private int rows_[];
		  //scoped_array<int> cols_;
		  private int cols_[];
		  //scoped_array<double> values_;
		  private double values_[];
		  
		  // Options struct to control the generation of random
		  // TripletSparseMatrix objects.
		  class RandomMatrixOptions {
		    public int num_rows;
		    public int num_cols;
		    // 0 < density <= 1 is the probability of an entry being
		    // structurally non-zero. A given random matrix will not have
		    // precisely this density.
		    public double density;
		    
		    public RandomMatrixOptions() {
		    	
		    }
		  } // class RandomMatrixOptions
	 public TripletSparseMatrix() {
		  super();
		  randomMatrixOptions = new RandomMatrixOptions();
		  num_rows_ = 0;
	      num_cols_ = 0;
	      max_num_nonzeros_ = 0;
	      num_nonzeros_ = 0;
	      rows_ = null;
	      cols_ = null;
	      values_ = null;
	 }
	 
	  public TripletSparseMatrix(int num_rows, int num_cols, int max_num_nonzeros) {
		  super();
		  randomMatrixOptions = new RandomMatrixOptions();
		  num_rows_ = num_rows;
	      num_cols_ = num_cols;
	      max_num_nonzeros_ = max_num_nonzeros;
	      num_nonzeros_ = 0;
	      rows_ = null;
	      cols_ = null;
	      values_ = null;
	      // All the sizes should at least be zero
	      if (num_rows < 0) {
	    	  System.err.println("In public TripletSparseMatrix num_rows < 0");
	    	  return;
	      }
	      if (num_cols < 0) {
	    	  System.err.println("In public TripletSparseMatrix num_cols < 0");
	    	  return;
	      }
	      if (max_num_nonzeros < 0) {
	    	  System.err.println("In public TripletSparseMatrix max_num_nonzeros < 0");
	    	  return;
	      }
	      AllocateMemory();

	  }
	  
	  public TripletSparseMatrix(int num_rows,
	                      int num_cols,
	                      Vector<Integer> rows,
	                      Vector<Integer> cols,
	                      Vector<Double> values) {
		  super();
		  randomMatrixOptions = new RandomMatrixOptions();
		  num_rows_ = num_rows;
	      num_cols_ = num_cols;
	      max_num_nonzeros_ = values.size();
	      num_nonzeros_ = values.size();
	      rows_ = null;
	      cols_ = null;
	      values_ = null;
	      // All the sizes should at least be zero
	      if (num_rows < 0) {
	    	  System.err.println("In public TripletSparseMatrix num_rows < 0");
	    	  return;
	      }
	      if (num_cols < 0) {
	    	  System.err.println("In public TripletSparseMatrix num_cols < 0");
	    	  return;
	      }
	      if (rows.size() != cols.size()) {
	    	  System.err.println("In public TripletSparseMatrix rows.size() != cols.size()");
	    	  return;
	      }
	      if (rows.size() != values.size()) {
	    	  System.err.println("In public TripletSparseMatrix rows.size() != values.size()");
	    	  return;
	      }
	      AllocateMemory();
	      for (int i = 0; i < rows.size(); i++) {
	    	  rows_[i] = rows.get(i);
	    	  cols_[i] = cols.get(i);
	    	  values_[i] = values.get(i);
	      }  
	  }
	  
	  public void AllocateMemory() {
		  rows_ = new int[max_num_nonzeros_];
		  cols_ = new int[max_num_nonzeros_];
		  values_ = new double[max_num_nonzeros_];

	  }

	  public TripletSparseMatrix(TripletSparseMatrix orig) {
		  super();
		  randomMatrixOptions = new RandomMatrixOptions();
	      num_rows_ = orig.num_rows_;
	      num_cols_ = orig.num_cols_;
	      max_num_nonzeros_ = orig.max_num_nonzeros_;
	      num_nonzeros_ = orig.num_nonzeros_;
	      rows_ = null;
	      cols_ = null;
	      values_ = null;
	      AllocateMemory();
	      CopyData(orig);
	  }
	  
	  public void CopyData(TripletSparseMatrix orig) {
		  for (int i = 0; i < num_nonzeros_; ++i) {
			    rows_[i] = orig.rows_[i];
			    cols_[i] = orig.cols_[i];
			    values_[i] = orig.values_[i];
		  }
	  }

	  // Implementation of the SparseMatrix interface.
	  public void SetZero() {
		  for (int i = 0; i < max_num_nonzeros_; i++) {
			  values_[i] = 0.0;
		  }
		  num_nonzeros_ = 0;

	  }
	  
	  public void RightMultiply(double x[], double y[]) {
		  for (int i = 0; i < num_nonzeros_; ++i) {
			    y[rows_[i]] += values_[i]*x[cols_[i]];
			  }

	  }
	  
	  
	  public void LeftMultiply(double x[], double y[]) {
		  for (int i = 0; i < num_nonzeros_; ++i) {
			    y[cols_[i]] += values_[i]*x[rows_[i]];
			  }
	  }
	  
	  public void SquaredColumnNorm(double x[]) {
		  int i;
          if (x == null) {
        	  System.err.println("x == null in SquaredColumnNorm");
        	  return;
          }
          for (i = 0; i < num_cols_; i++) {
        	  x[i] = 0.0;
          }

		  for (i = 0; i < num_nonzeros_; ++i) {
		    x[cols_[i]] += values_[i] * values_[i];
		  }
	  }
	  
	  public void ScaleColumns(double scale[]) {
		  if (scale == null) {
			  System.err.println("scale == null in ScaleColumns");
			  return;
		  }
		  for (int i = 0; i < num_nonzeros_; ++i) {
		    values_[i] = values_[i] * scale[cols_[i]];
		  }
	  }
	  
	  //public void ToDenseMatrix(Matrix* dense_matrix) const;
	  public Matrix ToDenseMatrix() {
		  double array[][] = new double[num_rows_][num_cols_];
		  for (int i = 0; i < num_nonzeros_; ++i) {
		    array[rows_[i]][cols_[i]] += values_[i];
		  }
          Matrix output_dense_matrix = new Matrix(array);
          return output_dense_matrix;
	  }
	  
	  public void ToTextFile(File file) {
		  if (file == null) {
			  System.err.println("file == null in ToTextFile");
			  return;
		  }
		  FileWriter fw = null;
			try {
				fw = new FileWriter(file);
			} catch (IOException e) {
				System.err.println("IOException in ToTextFile on new FileWriter(file)");
				return;
			}
		  for (int i = 0; i < num_nonzeros_; ++i) {
		    String str = String.format( "% 10d % 10d %17f\n", rows_[i], cols_[i], values_[i]);
			try {
				fw.write(str, 0, str.length());
			} catch (IOException e) {
				System.err.println("IOException in ToTextFile on fw.write(str,0,str.length())");
				return;
			}
		  }
		  try {
			    fw.close();
			}
			catch (IOException e) {
				System.err.println("IOException in ToTextFile on fw.close()");
			}
		}

	  public int num_rows()        { return num_rows_;     }
	  public int num_cols()       { return num_cols_;     }
	  public int num_nonzeros()   { return num_nonzeros_; }
	  public double[] values()  { return values_; }
	  public double[] mutable_values()      { return values_; }
	  public void set_num_nonzeros(int num_nonzeros) {
		  if (num_nonzeros < 0) {
			  System.err.println("In set_num_nonzeros num_nonzeros < 0");
			  return;
		  }
		  if (num_nonzeros > max_num_nonzeros_) {
			  System.err.println("In set_num_nonzeros num_nonzeros > max_num_nonzeros");
			  return;
		  }
		  num_nonzeros_ = num_nonzeros;

	  }

	  // Increase max_num_nonzeros and correspondingly increase the size
	  // of rows_, cols_ and values_. If new_max_num_nonzeros is smaller
	  // than max_num_nonzeros_, then num_non_zeros should be less than or
	  // equal to new_max_num_nonzeros, otherwise data loss is possible
	  // and the method crashes.
	  public void Reserve(int new_max_num_nonzeros) {
		  if (num_nonzeros_ > new_max_num_nonzeros) {
	      System.err.println("Reallocation in Reserve will cause data loss");
	      return;
		  }

	  // Nothing to do if we have enough space already.
	  if (new_max_num_nonzeros <= max_num_nonzeros_)
	    return;

	  int new_rows[] = new int[new_max_num_nonzeros];
	  int new_cols[] = new int[new_max_num_nonzeros];
	  double new_values[] = new double[new_max_num_nonzeros];

	  for (int i = 0; i < num_nonzeros_; ++i) {
	    new_rows[i] = rows_[i];
	    new_cols[i] = cols_[i];
	    new_values[i] = values_[i];
	  }

	  rows_ = new_rows;
	  cols_ = new_cols;
	  values_ = new_values;

	  max_num_nonzeros_ = new_max_num_nonzeros;

	  }

	  // Append the matrix B at the bottom of this matrix. B should have
	  // the same number of columns as num_cols_.
	  public void AppendRows(TripletSparseMatrix B) {
		  if (B.num_cols() != num_cols_) {
			  System.err.println("In AppendRows B.num_cols() != num_cols_");
			  return;
		  }
		  Reserve(num_nonzeros_ + B.num_nonzeros_);
		  for (int i = 0; i < B.num_nonzeros_; ++i) {
		    rows_[num_nonzeros_] = B.rows()[i] + num_rows_;
		    cols_[num_nonzeros_] = B.cols()[i];
		    values_[num_nonzeros_++] = B.values()[i];
		  }
		  num_rows_ = num_rows_ + B.num_rows();

	  }

	  // Append the matrix B at the right of this matrix. B should have
	  // the same number of rows as num_rows_;
	  public void AppendCols(TripletSparseMatrix B) {
		  if (B.num_rows() !=  num_rows_) {
			  System.err.println("In AppendCols B.num_rows() != num_rows_");
			  return;
		  }
		  Reserve(num_nonzeros_ + B.num_nonzeros_);
		  for (int i = 0; i < B.num_nonzeros_; ++i, ++num_nonzeros_) {
		    rows_[num_nonzeros_] = B.rows()[i];
		    cols_[num_nonzeros_] = B.cols()[i] + num_cols_;
		    values_[num_nonzeros_] = B.values()[i];
		  }
		  num_cols_ = num_cols_ + B.num_cols();
	  }

	  // Resize the matrix. Entries which fall outside the new matrix
	  // bounds are dropped and the num_non_zeros changed accordingly.
	  public void Resize(int new_num_rows, int new_num_cols) {
		  if ((new_num_rows >= num_rows_) && (new_num_cols >= num_cols_)) {
			    num_rows_  = new_num_rows;
			    num_cols_ = new_num_cols;
			    return;
			  }

			  num_rows_ = new_num_rows;
			  num_cols_ = new_num_cols;

			  int r_ptr[] = rows_;
			  int c_ptr[] = cols_;
			  double v_ptr[] = values_;

			  int dropped_terms = 0;
			  for (int i = 0; i < num_nonzeros_; ++i) {
			    if ((r_ptr[i] < num_rows_) && (c_ptr[i] < num_cols_)) {
			      if (dropped_terms > 0) {
			        r_ptr[i-dropped_terms] = r_ptr[i];
			        c_ptr[i-dropped_terms] = c_ptr[i];
			        v_ptr[i-dropped_terms] = v_ptr[i];
			      }
			    } else {
			      ++dropped_terms;
			    }
			  }
			  num_nonzeros_ -= dropped_terms;

	  }

	  public int max_num_nonzeros() { return max_num_nonzeros_; }
	  public int[] rows()      { return rows_;       }
	  public int[] cols()      { return cols_;       }
	  public int[] mutable_rows()          { return rows_;       }
	  public int[] mutable_cols()          { return cols_;       }

	  // Returns true if the entries of the matrix obey the row, column,
	  // and column size bounds and false otherwise.
	  public boolean AllTripletsWithinBounds() {
		  for (int i = 0; i < num_nonzeros_; ++i) {
			    if ((rows_[i] < 0) || (rows_[i] >= num_rows_) ||
			        (cols_[i] < 0) || (cols_[i] >= num_cols_))
			      return false;
			  }
			  return true;
	  }

	  public boolean IsValid() { return AllTripletsWithinBounds(); }  
	} // class TripletSparseMatrix
	
	public TripletSparseMatrix operator(TripletSparseMatrix lhs,
		    TripletSparseMatrix rhs) {
		  lhs.num_rows_ = rhs.num_rows_;
		  lhs.num_cols_ = rhs.num_cols_;
		  lhs.num_nonzeros_ = rhs.num_nonzeros_;
		  lhs.max_num_nonzeros_ = rhs.max_num_nonzeros_;
		  lhs.AllocateMemory();
		  lhs.CopyData(rhs);
		  return lhs;
		}
	
	// Build a sparse diagonal matrix of size num_rows x num_rows from
    // the array values. Entries of the values array are copied into the
    // sparse matrix.
	public TripletSparseMatrix CreateSparseDiagonalMatrix(double values[], int num_rows) {
		TripletSparseMatrix m =
			      new TripletSparseMatrix(num_rows, num_rows, num_rows);
			  for (int i = 0; i < num_rows; ++i) {
			    m.mutable_rows()[i] = i;
			    m.mutable_cols()[i] = i;
			    m.mutable_values()[i] = values[i];
			  }
			  m.set_num_nonzeros(num_rows);
			  return m;
	}
	
	// Create a random CompressedRowSparseMatrix whose entries are
	// normally distributed and whose structure is determined by
	// RandomMatrixOptions.
	//
	// Caller owns the result.
	public TripletSparseMatrix CreateRandomMatrix(TripletSparseMatrix.RandomMatrixOptions options) {
		  if (options.num_rows <= 0) {
			  System.err.println("In TripletSparseMatrix CreateRandomMatrix options.num_rows <= 0");
			  return null;
		  }
		  if (options.num_cols <= 0) {
			  System.err.println("In TripletSparseMatrix CreateRandomMatrix options.num_cols <= 0");
			  return null;
		  }
		  if (options.density <= 0.0) {
			  System.err.println("In TripletSparseMatrix CreateRandomMatrix options.density <= 0.0");
			  return null;
		  }
		  if (options.density > 1.0) {
			  System.err.println("In TripletSparseMatrix CreateRandomMatrix options.density > 1.0");
			  return null;
		  }

		  Vector<Integer> rows = new Vector<Integer>();
		  Vector<Integer> cols = new Vector<Integer>();
		  Vector<Double> values = new Vector<Double>();
		  while (rows.isEmpty()) {
		    rows.clear();
		    cols.clear();
		    values.clear();
		    for (int r = 0; r < options.num_rows; ++r) {
		      for (int c = 0; c < options.num_cols; ++c) {
		        if (RandDouble() <= options.density) {
		          rows.add(r);
		          cols.add(c);
		          values.add(RandNormal());
		        }
		      }
		    }
		  }

		  return new TripletSparseMatrix(
		      options.num_rows, options.num_cols, rows, cols, values);

	}
	
	// Structure to carry a pointer to the array containing a cell and the
	// Mutex guarding it.
	class CellInfo {
      public double values[];
      public int values_index;
	  public Lock m;
	  public CellInfo() {
	      values = null;
	      values_index = 0;
	      m = new ReentrantLock();
	  }

	  public CellInfo(double ptr[], int index) {
	      values = ptr;
	      values_index = index;
	      m = new ReentrantLock();
	  }
	  
	  protected void finalize() {
		  if (m != null) {
			  m.unlock();
		  }
	  }
	};
	
	abstract class BlockRandomAccessMatrix {
		
		public BlockRandomAccessMatrix() {
			
		}

		  // If the cell (row_block_id, col_block_id) is present, then return
		  // a CellInfo with a pointer to the dense matrix containing it,
		  // otherwise return NULL. The dense matrix containing this cell has
		  // size row_stride, col_stride and the cell is located at position
		  // (row, col) within this matrix.
		  //
		  // The size of the cell is row_block_size x col_block_size is
		  // assumed known to the caller. row_block_size less than or equal to
		  // row_stride and col_block_size is upper bounded by col_stride.
		  public abstract CellInfo GetCell(int row_block_id,
		                            int col_block_id,
		                            int row[],
		                            int col[],
		                            int row_stride[],
		                            int[] col_stride);

		  // Zero out the values of the array. The structure of the matrix
		  // (size and sparsity) is preserved.
		  public abstract void SetZero();

		  // Number of scalar rows and columns in the matrix, i.e the sum of
		  // all row blocks and column block sizes respectively.
		  public abstract int num_rows();
		  public abstract int num_cols();
		}  // abstract class BlockRandomAccessMatrix
	
	// A thread safe block diagonal matrix implementation of
	// BlockRandomAccessMatrix.
	class BlockRandomAccessDiagonalMatrix extends BlockRandomAccessMatrix {
		// row/column block sizes.
		private Vector<Integer> blocks_;
		private Vector<CellInfo> layout_;

		// The underlying matrix object which actually stores the cells.
		//scoped_ptr<TripletSparseMatrix> tsm_;
		private TripletSparseMatrix tsm_;

		//friend class BlockRandomAccessDiagonalMatrixTest;
	  // blocks is an array of block sizes.
	  public BlockRandomAccessDiagonalMatrix(Vector<Integer> blocks) {
		      blocks_ = blocks;
		      layout_ = new Vector<CellInfo>();
			  // Build the row/column layout vector and count the number of scalar
			  // rows/columns.
			  int num_cols = 0;
			  int num_nonzeros = 0;
			  Vector<Integer> block_positions = new Vector<Integer>();
			  for (int i = 0; i < blocks_.size(); ++i) {
			    block_positions.add(num_cols);
			    num_cols += blocks_.get(i);
			    num_nonzeros += blocks_.get(i) * blocks_.get(i);
			  }

			  if (1 <= MAX_LOG_LEVEL) {
				  Preferences.debug("Matrix Size [" + num_cols + "," + num_cols
			          + "] " + num_nonzeros + "\n", Preferences.DEBUG_ALGORITHM);
			  }

			  tsm_ = new TripletSparseMatrix(num_cols, num_cols, num_nonzeros);
			  tsm_.set_num_nonzeros(num_nonzeros);
			  int rows[] = tsm_.mutable_rows();
			  int cols[] = tsm_.mutable_cols();
			  double values[] = tsm_.mutable_values();

			  int pos = 0;
			  for (int i = 0; i < blocks_.size(); ++i) {
			    int block_size = blocks_.get(i);
			    layout_.add(new CellInfo(values,pos));
			    int block_begin = block_positions.get(i);
			    for (int r = 0; r < block_size; ++r) {
			      for (int c = 0; c < block_size; ++c, ++pos) {
			        rows[pos] = block_begin + r;
			        cols[pos] = block_begin + c;
			      }
			    }
			  }

	  }

	  // The destructor is not thread safe. It assumes that no one is
	  // modifying any cells when the matrix is being destroyed.
	// Assume that the user does not hold any locks on any cell blocks
	// when they are calling SetZero.
	public void finalize() {
	 while (layout_.size() > 0) {
		 CellInfo cf = layout_.remove(layout_.size()-1);
		 cf.values = null;
		 cf = null;
	 }
	 layout_ = null;
	}


	  // BlockRandomAccessMatrix Interface.
	  public CellInfo GetCell(int row_block_id,
	                            int col_block_id,
	                            int row[],
	                            int col[],
	                            int row_stride[],
	                            int col_stride[]) {
		     if (row_block_id != col_block_id) {
			    return null;
			  }
			  int stride = blocks_.get(row_block_id);

			  // Each cell is stored contiguously as its own little dense matrix.
			  row[0] = 0;
			  col[0] = 0;
			  row_stride[0] = stride;
			  col_stride[0] = stride;
			  return layout_.get(row_block_id);
			}

	  // This is not a thread safe method, it assumes that no cell is
	  // locked.
	  public void SetZero() {
		  if (tsm_.num_nonzeros() > 0) {
			  for (int i = 0; i < tsm_.num_nonzeros(); i++) {
				  tsm_.mutable_values()[i] = 0;
			  }
		  }
	  }


	  // Invert the matrix assuming that each block is positive definite.
	  public void Invert() {
		  int i,r,c;
		  double values[] = tsm_.mutable_values();
		  int values_index = 0;
		  for (i = 0; i < blocks_.size(); ++i) {
		    int block_size = blocks_.get(i);
		    double arr[][] = new double[block_size][block_size];
		    for (r = 0; r < block_size; r++) {
		    	for (c = 0; c < block_size; c++) {
		    		arr[r][c] = values[values_index + r*block_size + c];
		    	}
		    }
		    Matrix block = new Matrix(arr);
		    block = block.inverse();
		    double arrinv[][] = block.getArray();
		    for (r = 0; r < block_size; r++) {
		    	for (c = 0; c < block_size; c++) {
		    		values[values_index + r*block_size + c] = arrinv[r][c];
		    	}
		    }
		    //MatrixRef block(values, block_size, block_size);
		    //block =
		    //    block
		    //    .selfadjointView<Eigen::Upper>()
		    //    .llt()
		    //    .solve(Matrix::Identity(block_size, block_size));
		    values_index += block_size * block_size;
		  }
	  }

	  // y += S * x
	  public void RightMultiply(double x[], double y[]) {
		  if (x == null) {
			  System.err.println("In RightMultiply x == null");
			  return;
		  }
		  if (y == null) {
			  System.err.println("In RightMultiply y == null");
			  return;
		  }
		  double values[] = tsm_.values();
		  int values_index = 0;
		  int xy_index = 0;
		  for (int i = 0; i < blocks_.size(); ++i) {
		    int block_size = blocks_.get(i);
		    double block[][] = new double[block_size][block_size];
		    for (int row = 0; row < block_size; row++) {
		    	for (int col = 0; col < block_size; col++, values_index++) {
		    		block[row][col] = values[values_index];
		    	}
		    }
		    double block_times_x[] = new double[block_size];
		    for (int row = 0; row < block_size; row++) {
		    	for (int col = 0; col < block_size; col++) {
		    		block_times_x[row] += (block[row][col] * x[xy_index + col]);
		    	}
		    }
		    for (int row = 0; row < block_size; row++) {
		    	y[xy_index + row] += block_times_x[row];
		    }
		    xy_index += block_size;
		  }

	  };

	  // Since the matrix is square, num_rows() == num_cols().
	  public int num_rows() { return tsm_.num_rows(); }
	  public int num_cols() { return tsm_.num_cols(); }

	  public TripletSparseMatrix matrix() { return tsm_; }
	  public TripletSparseMatrix mutable_matrix() { return tsm_; }

	  
	};

	
	public int Uniform(int n) {
		  if (n > 0) {
			Random random = new Random();
		    return random.nextInt(Integer.MAX_VALUE) % n;
		  } else {
		    return 0;
		  }
	} // public int Uniform(int n)
	
	public double RandDouble() {
		Random random = new Random();
		return ((double)random.nextInt(Integer.MAX_VALUE)/(double)(Integer.MAX_VALUE - 1));
	}

	// Box-Muller algorithm for normal random number generation.
	// http://en.wikipedia.org/wiki/Box-Muller_transform
	public double RandNormal() {
	  double x1, x2, w;
	  do {
	    x1 = 2.0 * RandDouble() - 1.0;
	    x2 = 2.0 * RandDouble() - 1.0;
	    w = x1 * x1 + x2 * x2;
	  } while ( w >= 1.0 || w == 0.0 );

	  w = Math.sqrt((-2.0 * Math.log(w)) / w);
	  return x1 * w;
	}

	abstract class SparseMatrix extends LinearOperator {
		
		public SparseMatrix() {
			super();
		}
		
		public void RightMultiply(Vector<Double> x, Vector<Double>y) {
			int i;
			double x_array[] = new double[x.size()];
			for (i = 0; i < x.size(); i++) {
				x_array[i] = x.get(i);
			}
			double y_array[] = new double[y.size()];
			for (i = 0; i < y.size(); i++) {
				y_array[i] = y.get(i);
			}
			RightMultiply(x_array,y_array);
			for (i = 0; i < y.size(); i++) {
				y.set(i,y_array[i]);
			}
		}

		// y += Ax;
		public abstract void RightMultiply(double x[], double y[]);

		// y += A'x;
		public abstract void LeftMultiply(double x[], double y[]);
		
		public void SquaredColumnNorm(Vector<Double> x) {
			int i;
			double x_array[] = new double[x.size()];
			for (i = 0; i < x.size(); i++) {
				x_array[i] = x.get(i);
			}
			SquaredColumnNorm(x_array);
			for (i = 0; i < x.size(); i++) {
				x.set(i, x_array[i]);
			}
		}

		// In MATLAB notation sum(A.*A, 1)
		public abstract void SquaredColumnNorm(double x[]);
		
		public void ScaleColumns(Vector<Double> scale) {
			int i;
			double scale_array[] = new double[scale.size()];
			for (i = 0; i < scale.size(); i++) {
				scale_array[i] = scale.get(i);
			}
			ScaleColumns(scale_array);
		}

		// A = A * diag(scale)
		public abstract void ScaleColumns(double scale[]);

		// A = 0. A->num_nonzeros() == 0 is true after this call. The
		// sparsity pattern is preserved.
		public abstract void SetZero();

		// Resize and populate dense_matrix with a dense version of the
		// sparse matrix.
		// Change from void to Matrix because Matrix dimensions can change
		public abstract Matrix ToDenseMatrix();

		// Write out the matrix as a sequence of (i,j,s) triplets. This
		// format is useful for loading the matrix into MATLAB/octave as a
		// sparse matrix.
		public abstract void ToTextFile(File file);

		// Accessors for the values array that stores the entries of the
		// sparse matrix. The exact interpreptation of the values of this
		// array depends on the particular kind of SparseMatrix being
		// accessed.
		public abstract double[] mutable_values();

		public abstract double[] values();

		public abstract int num_rows();

		public abstract int num_cols();

		public abstract int num_nonzeros();
	};

	// This is an abstract base class for linear operators. It supports
	// access to size information and left and right multiply operators.
	abstract class LinearOperator {
		public LinearOperator() {

		}

		// y = y + Ax;
		public abstract void RightMultiply(double x[], double y[]);

		// y = y + A'x;
		public abstract void LeftMultiply(double x[], double y[]);

		public abstract int num_rows();

		public abstract int num_cols();
	};
	
	class LinearSolverOptions {
		public LinearSolverType type;
		public PreconditionerType preconditioner_type;
		public VisibilityClusteringType visibility_clustering_type;
		public DenseLinearAlgebraLibraryType dense_linear_algebra_library_type;
		public SparseLinearAlgebraLibraryType sparse_linear_algebra_library_type;

		// See solver.h for information about these flags.
		public boolean use_postordering;
		public boolean dynamic_sparsity;
		public boolean use_explicit_schur_complement;

		// Number of internal iterations that the solver uses. This
		// parameter only makes sense for iterative solvers like CG.
		public int min_num_iterations;
		public int max_num_iterations;

		// If possible, how many threads can the solver use.
		public int num_threads;

		// Hints about the order in which the parameter blocks should be
		// eliminated by the linear solver.
		//
		// For example if elimination_groups is a vector of size k, then
		// the linear solver is informed that it should eliminate the
		// parameter blocks 0 ... elimination_groups[0] - 1 first, and
		// then elimination_groups[0] ... elimination_groups[1] - 1 and so
		// on. Within each elimination group, the linear solver is free to
		// choose how the parameter blocks are ordered. Different linear
		// solvers have differing requirements on elimination_groups.
		//
		// The most common use is for Schur type solvers, where there
		// should be at least two elimination groups and the first
		// elimination group must form an independent set in the normal
		// equations. The first elimination group corresponds to the
		// num_eliminate_blocks in the Schur type solvers.
		public Vector<Integer> elimination_groups;

		// Iterative solvers, e.g. Preconditioned Conjugate Gradients
		// maintain a cheap estimate of the residual which may become
		// inaccurate over time. Thus for non-zero values of this
		// parameter, the solver can be told to recalculate the value of
		// the residual using a |b - Ax| evaluation.
		public int residual_reset_period;

		// If the block sizes in a BlockSparseMatrix are fixed, then in
		// some cases the Schur complement based solvers can detect and
		// specialize on them.
		//
		// It is expected that these parameters are set programmatically
		// rather than manually.
		//
		// Please see schur_complement_solver.h and schur_eliminator.h for
		// more details.
		public int row_block_size[] = new int[1];
		public int e_block_size[] = new int[1];
		public int f_block_size[] = new int[1];

		public Context context;

		public LinearSolverOptions() {
			//type = LinearSolverType.SPARSE_NORMAL_CHOLESKY;
			type = requestedLinearSolverType;
			preconditioner_type = PreconditionerType.JACOBI;
			visibility_clustering_type = VisibilityClusteringType.CANONICAL_VIEWS;
			//dense_linear_algebra_library_type = DenseLinearAlgebraLibraryType.EIGEN;
			dense_linear_algebra_library_type = DenseLinearAlgebraLibraryType.LAPACK;
			sparse_linear_algebra_library_type = SparseLinearAlgebraLibraryType.SUITE_SPARSE;
			use_postordering = false;
			dynamic_sparsity = false;
			use_explicit_schur_complement = false;
			min_num_iterations = 1;
			max_num_iterations = 1;
			num_threads = 1;
			residual_reset_period = 10;
			row_block_size[0] = DYNAMIC;
			e_block_size[0] = DYNAMIC;
			f_block_size[0] = DYNAMIC;
			context = new Context();
			elimination_groups = new Vector<Integer>();
		}
	} // class LinearSolverOptions
	
	        // Options for the Solve method.
			class LinearSolverPerSolveOptions {
				// This option only makes sense for unsymmetric linear solvers
				// that can solve rectangular linear systems.
				//
				// Given a matrix A, an optional diagonal matrix D as a vector,
				// and a vector b, the linear solver will solve for
				//
				// | A | x = | b |
				// | D | | 0 |
				//
				// If D is null, then it is treated as zero, and the solver returns
				// the solution to
				//
				// A x = b
				//
				// In either case, x is the vector that solves the following
				// optimization problem.
				//
				// arg min_x ||Ax - b||^2 + ||Dx||^2
				//
				// Here A is a matrix of size m x n, with full column rank. If A
				// does not have full column rank, the results returned by the
				// solver cannot be relied on. D, if it is not null is an array of
				// size n. b is an array of size m and x is an array of size n.
				protected double D[];

				// This option only makes sense for iterative solvers.
				//
				// In general the performance of an iterative linear solver
				// depends on the condition number of the matrix A. For example
				// the convergence rate of the conjugate gradients algorithm
				// is proportional to the square root of the condition number.
				//
				// One particularly useful technique for improving the
				// conditioning of a linear system is to precondition it. In its
				// simplest form a preconditioner is a matrix M such that instead
				// of solving Ax = b, we solve the linear system AM^{-1} y = b
				// instead, where M is such that the condition number k(AM^{-1})
				// is smaller than the conditioner k(A). Given the solution to
				// this system, x = M^{-1} y. The iterative solver takes care of
				// the mechanics of solving the preconditioned system and
				// returning the corrected solution x. The user only needs to
				// supply a linear operator.
				//
				// A null preconditioner is equivalent to an identity matrix being
				// used a preconditioner.
				private LinearOperator preconditioner;

				// The following tolerance related options only makes sense for
				// iterative solvers. Direct solvers ignore them.

				// Solver terminates when
				//
				// |Ax - b| <= r_tolerance * |b|.
				//
				// This is the most commonly used termination criterion for
				// iterative solvers.
				protected double r_tolerance;

				// For PSD matrices A, let
				//
				// Q(x) = x'Ax - 2b'x
				//
				// be the cost of the quadratic function defined by A and b. Then,
				// the solver terminates at iteration i if
				//
				// i * (Q(x_i) - Q(x_i-1)) / Q(x_i) < q_tolerance.
				//
				// This termination criterion is more useful when using CG to
				// solve the Newton step. This particular convergence test comes
				// from Stephen Nash's work on truncated Newton
				// methods. References:
				//
				// 1. Stephen G. Nash & Ariela Sofer, Assessing A Search
				// Direction Within A Truncated Newton Method, Operation
				// Research Letters 9(1990) 219-221.
				//
				// 2. Stephen G. Nash, A Survey of Truncated Newton Methods,
				// Journal of Computational and Applied Mathematics,
				// 124(1-2), 45-59, 2000.
				//
				private double q_tolerance;

				public LinearSolverPerSolveOptions() {
					D = null;
					preconditioner = null;
					r_tolerance = 0.0;
					q_tolerance = 0.0;
				}

			} // class LinearSolverPerSolveOptions
			
			// Summary of a call to the Solve method. We should move away from
			// the true/false method for determining solver success. We should
			// let the summary object do the talking.
			class LinearSolverSummary {
				double residual_norm;
				int num_iterations;
				LinearSolverTerminationType termination_type;
				String message[];

				public LinearSolverSummary() {
					residual_norm = 0.0;
					num_iterations = -1;
					termination_type = LinearSolverTerminationType.LINEAR_SOLVER_FAILURE;
					message = new String[1];
				}

			} // class LinearSolverSummmary


	abstract class LinearSolver {
		public LinearSolverOptions options_;
		public LinearSolverSummary summary;
		public LinearSolverPerSolveOptions perSolveOptions;

		public LinearSolver() {
			options_ = new LinearSolverOptions();
			summary = new LinearSolverSummary();
			perSolveOptions = new LinearSolverPerSolveOptions();
		}
		
		  // This method returns copies instead of references so that the base
		  // class implementation does not have to worry about life time
		  // issues. Further, this calls are not expected to be frequent or
		  // performance sensitive.
		  public HashMap<String, CallStatistics> Statistics() {
		    return new HashMap<String, CallStatistics>();
		  }
		  
		  public abstract LinearSolverSummary Solve(LinearOperator A, double b[], LinearSolverPerSolveOptions per_solve_options,
					double x[]);

	} // class LinearSolver

	public LinearSolver Create(LinearSolverOptions options) {
		if (options.context == null) {
			System.err.println("options.context == null in public LinearSolver Create");
			return null;
		}

		switch (options.type) {
		 case CGNR: {
			 System.out.println("CGNR");
			 return new CgnrSolver(options); 
		 }
		 /* case SPARSE_NORMAL_CHOLESKY: if (CERES_NO_SUITESPARSE && CERES_NO_CXSPARSE &&
		 * (!CERES_USE_EIGEN_SPARSE)) { return null; } else { if
		 * (options.dynamic_sparsity) { return new
		 * DynamicSparseNormalCholeskySolver(options); }
		 * 
		 * return new SparseNormalCholeskySolver(options); }
		 * 
		 * case SPARSE_SCHUR: if (CERES_NO_SUITESPARSE && CERES_NO_CXSPARSE &&
		 * (!CERES_USE_EIGEN_SPARSE)) { return null; }
		 * 
		 * else { return new SparseSchurComplementSolver(options); }
		 */ 
		 case DENSE_SCHUR: {
			 System.out.println("DENSE_SCHUR");
			 return new DenseSchurComplementSolver(options);
		 }
		 // SparseSchurComplementServer requires either SuiteSparse, Eigen's sparse Cholesky factorization,
		 // or CXSparse.
		 case ITERATIVE_SCHUR: {
			 System.err.println("ITERATIVE_SCHUR");
			 return new IterativeSchurComplementSolver(options);
		 }
		 
		 case DENSE_QR: {
			 System.out.println("DENSE_QR");
			 return new DenseQRSolver(options);
		 }
		  
		 case DENSE_NORMAL_CHOLESKY: {
			 System.out.println("DENSE_NORMAL_CHOLESKY");
			 return new DenseNormalCholeskySolver(options);
		 }

		default:
			System.err.println("Unknown linear solver type :" + options.type);
			return null; // MSVC doesn't understand that LOG(FATAL) never returns.
		}
	}
	
	// This class uses the LDLT factorization routines from the Eigen
	// library. This solver always returns a solution, it is the user's
	// responsibility to judge if the solution is good enough for their
	// purposes.
	class DenseNormalCholeskySolver extends TypedLinearSolver<DenseSparseMatrix>  {
		
	  public DenseNormalCholeskySolver(LinearSolverOptions options) {
		  super();
	      options_ = options; 
	      options_.type = LinearSolverType.DENSE_NORMAL_CHOLESKY;
	  }
	  
	  public LinearSolverSummary Solve(LinearOperator A, double b[], LinearSolverPerSolveOptions per_solve_options,
				double x[]) {
			new ScopedExecutionTimer("LinearSolver::Solve", execution_summary_);
			if (A == null) {
				System.err.println("In TypedLinearSolver public LinearSummary.Summary Solve LinearOperator A == null");
				return null;
			}
			if (b == null) {
				System.err.println("In TypedLinearSolver public LinearSummary.Summary Solve double b[] == null");
				return null;
			}
			if (x == null) {
				System.err.println("In TypedLinearSolver public LinearSummary.Summary Solve double x[] == null");
				return null;
			}
			// return SolveImpl(down_cast<MatrixType*>(A), b, per_solve_options, x);
			return SolveImpl((DenseSparseMatrix) A, b, per_solve_options, x);
		}

	 public LinearSolverSummary SolveImpl(
	      DenseSparseMatrix A,
	      double b[],
	      LinearSolverPerSolveOptions per_solve_options,
	      double x[]) {
		 if (options_.dense_linear_algebra_library_type == DenseLinearAlgebraLibraryType.EIGEN) {
			    return SolveUsingEigen(A, b, per_solve_options, x);
		 } else {
			    return SolveUsingLAPACK(A, b, per_solve_options, x);
		 }

	 }

	 private LinearSolverSummary SolveUsingLAPACK(
	      DenseSparseMatrix A,
	      double b[],
	      LinearSolverPerSolveOptions per_solve_options,
	      double x[]) {
		 EventLogger event_logger = new EventLogger("DenseNormalCholeskySolver::Solve");

		  if (per_solve_options.D != null) {
		    // Temporarily append a diagonal block to the A matrix, but undo
		    // it before returning the matrix to the user.
		    A.AppendDiagonal(per_solve_options.D);
		  }

		  int num_cols = A.num_cols();
		  Matrix lhs = new Matrix(num_cols, num_cols);
		  event_logger.AddEvent("Setup");

		  // lhs = A'A
		  //
		  // Note: This is a bit delicate, it assumes that the stride on this
		  // matrix is the same as the number of rows.
		  SymmetricRankUpdate(A.num_rows(), num_cols, A.values(), true, 1.0, 0.0, lhs.getArray());
		  /*for (int row = 0; row < 2; row++) {
			  for (int col = 0; col < 2; col++) {
				  System.out.println("lhs["+row+"]["+col+"] = " + lhs.getArray()[row][col]);
			  }
		  }
		  double lhs2[][] = ((A.m_).transpose().times(A.m_)).getArray();
		  for (int row = 0; row < 2; row++) {
			  for (int col = 0; col < 2; col++) {
				  System.out.println("lhs2["+row+"]["+col+"] = " + lhs2[row][col]);
			  }
		  }*/

		  if (per_solve_options.D != null) {
		    // Undo the modifications to the matrix A.
		    A.RemoveDiagonal();
		  }

		  // TODO(sameeragarwal): Replace this with a gemv call for true blasness.
		  //   rhs = A'b
		  for (int col = 0; col < num_cols; col++) {
			  x[col] = 0;
			  for (int row = 0; row < A.num_rows(); row++) {
				  x[col] += A.matrix().getArray()[row][col]*b[row];
			  }
		  }
		  event_logger.AddEvent("Product");

		  summary.num_iterations = 1;
		  summary.termination_type =
		      SolveInPlaceUsingCholesky(num_cols, //lhs2,
		                                        lhs.getArray(),
		                                        x,
		                                        summary.message);
		  event_logger.AddEvent("Solve");
		  return summary;
	 
	 }

	  private LinearSolverSummary SolveUsingEigen(
	      DenseSparseMatrix A,
	      double b[],
	      LinearSolverPerSolveOptions per_solve_options,
	      double x[]) {
		  EventLogger event_logger = new EventLogger("DenseNormalCholeskySolver::Solve");

		  int num_rows = A.num_rows();
		  int num_cols = A.num_cols();

		  //ConstColMajorMatrixRef Aref = A->matrix();
		  Matrix Aref = A.matrix();
		  Matrix lhs = new Matrix(num_cols, num_cols);

		  event_logger.AddEvent("Setup");

		  //   lhs += A'A
		  //
		  // Using rankUpdate instead of GEMM, exposes the fact that its the
		  // same matrix being multiplied with itself and that the product is
		  // symmetric.
		  // Need to implement Eigen library for this
		  //lhs.selfadjointView<Eigen::Upper>().rankUpdate(Aref.transpose());

		  //   rhs = A'b
		  double rhs[] = new double[num_cols];
		  for (int col = 0; col < num_cols; col++) {
			  for (int row = 0; row < num_rows; row++) {
				  rhs[col] += (Aref.getArray()[row][col] * b[row]);
			  }
		  }

		  if (per_solve_options.D != null) {
			double D[] = new double[num_cols];
			for (int i = 0; i < num_cols; i++) {
				D[i] = per_solve_options.D[i];
				lhs.getArray()[i][i] += (D[i]*D[i]);
			}
		  }
		  event_logger.AddEvent("Product");

		  summary.num_iterations = 1;
		  summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS;
		  //Eigen::LLT<Matrix, Eigen::Upper> llt =
		      //lhs.selfadjointView<Eigen::Upper>().llt();

		  //if (llt.info() != Eigen::Success) {
		  //  summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_FAILURE;
		  //  summary.message[0] = "Eigen LLT decomposition failed.";
		 // } else {
		 //   summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS;
		  //  summary.message = "Success.";
		  //}

		  //VectorRef(x, num_cols) = llt.solve(rhs);
		  event_logger.AddEvent("Solve");
		  return summary; 
	  }

	};
	
	// This class uses the dense QR factorization routines from the Eigen
	// library. This solver always returns a solution, it is the user's
	// responsibility to judge if the solution is good enough for their
	// purposes.
	class DenseQRSolver extends TypedLinearSolver<DenseSparseMatrix> {
		// Column major matrices for DenseSparseMatrix/DenseQRSolver
		//ColMajorMatrix lhs_;
		private Matrix lhs_;
		private double rhs_[];
		private double work_[];
		
	    public DenseQRSolver(LinearSolverOptions options) {
	    	super();
	    	options_ = options;
	    	options_.type = LinearSolverType.DENSE_QR;
	        work_ = new double[1];
	    }
	    
	    public LinearSolverSummary Solve(LinearOperator A, double b[], LinearSolverPerSolveOptions per_solve_options,
				double x[]) {
			new ScopedExecutionTimer("LinearSolver::Solve", execution_summary_);
			if (A == null) {
				System.err.println("In TypedLinearSolver public LinearSummary.Summary Solve LinearOperator A == null");
				return null;
			}
			if (b == null) {
				System.err.println("In TypedLinearSolver public LinearSummary.Summary Solve double b[] == null");
				return null;
			}
			if (x == null) {
				System.err.println("In TypedLinearSolver public LinearSummary.Summary Solve double x[] == null");
				return null;
			}
			// return SolveImpl(down_cast<MatrixType*>(A), b, per_solve_options, x);
			return SolveImpl((DenseSparseMatrix) A, b, per_solve_options, x);
		}

	 public LinearSolverSummary SolveImpl(
	      DenseSparseMatrix A,
	      double b[],
	      LinearSolverPerSolveOptions per_solve_options,
	      double x[]) {
		  if (options_.dense_linear_algebra_library_type == DenseLinearAlgebraLibraryType.EIGEN) {
			    return SolveUsingEigen(A, b, per_solve_options, x);
			  } else {
			    return SolveUsingLAPACK(A, b, per_solve_options, x);
			  }
	  }

	 // Eigen library must be implemented  
	 private LinearSolverSummary SolveUsingEigen(
	      DenseSparseMatrix A,
	      double b[],
	      LinearSolverPerSolveOptions per_solve_options,
	      double x[]) {
		  EventLogger event_logger = new EventLogger("DenseQRSolver::Solve");

		  int num_rows = A.num_rows();
		  int num_cols = A.num_cols();

		  if (per_solve_options.D != null) {
		    // Temporarily append a diagonal block to the A matrix, but undo
		    // it before returning the matrix to the user.
		    A.AppendDiagonal(per_solve_options.D);
		  }

		  // rhs = [b;0] to account for the additional rows in the lhs.
		  int augmented_num_rows =
		      num_rows + ((per_solve_options.D != null) ? num_cols : 0);
		  if (rhs_.length != augmented_num_rows) {
		    rhs_ = new double[augmented_num_rows];
		  }
		  for (int i = 0; i < num_rows; i++) {
			  rhs_[i] = b[i];
		  }
		  event_logger.AddEvent("Setup");

		  // Solve the system.
		  //Must implement Eigen library for this line
		  //VectorRef(x, num_cols) = A->matrix().householderQr().solve(rhs_);
		  event_logger.AddEvent("Solve");

		  if (per_solve_options.D != null) {
		    // Undo the modifications to the matrix A.
		    A.RemoveDiagonal();
		  }

		  // We always succeed, since the QR solver returns the best solution
		  // it can. It is the job of the caller to determine if the solution
		  // is good enough or not.
		  summary.num_iterations = 1;
		  summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS;
		  summary.message[0] = "Success.";

		  event_logger.AddEvent("TearDown");
		  return summary;

	  }

	  private LinearSolverSummary SolveUsingLAPACK(
	      DenseSparseMatrix A,
	      double b[],
	      LinearSolverPerSolveOptions per_solve_options,
	      double x[]) {

		  EventLogger event_logger = new EventLogger("DenseQRSolver::Solve");

		  int num_rows = A.num_rows();
		  int num_cols = A.num_cols();

		  if (per_solve_options.D != null) {
		    // Temporarily append a diagonal block to the A matrix, but undo
		    // it before returning the matrix to the user.
		    A.AppendDiagonal(per_solve_options.D);
		  }

		  // TODO(sameeragarwal): Since we are copying anyways, the diagonal
		  // can be appended to the matrix instead of doing it on A.
		  lhs_ =  A.matrix();

		  if (per_solve_options.D != null) {
		    // Undo the modifications to the matrix A.
		    A.RemoveDiagonal();
		  }

		  // rhs = [b;0] to account for the additional rows in the lhs.
		  if ((rhs_ == null) || (rhs_.length != lhs_.getRowDimension())) {
		    rhs_ = new double[lhs_.getRowDimension()];
		  }
		  for (int i = 0; i < num_rows; i++) {
			  rhs_[i] = b[i];
		  }

		  if (work_.length == 1) {
		    int work_size =
		        EstimateWorkSizeForQR(lhs_.getRowDimension(), lhs_.getColumnDimension());
		    if (3 < MAX_LOG_LEVEL) {
		        Preferences.debug("Working double memory size for Dense QR factorization: " + work_size + "\n", 
		            Preferences.DEBUG_ALGORITHM);
		    }
		    work_ = new double[work_size];
		  }
		  

		  summary.num_iterations = 1;
		  summary.termination_type = SolveInPlaceUsingQR(lhs_.getRowDimension(),
		                                                         lhs_.getColumnDimension(),
		                                                         lhs_.getArray(),
		                                                         work_.length,
		                                                         work_,
		                                                         rhs_,
		                                                         summary.message);
		  event_logger.AddEvent("Solve");
		  if (summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS) {
			for (int i = 0; i < num_cols; i++) {
				x[i] = rhs_[i];
			}
		  }

		  event_logger.AddEvent("TearDown");
		  return summary;

	  }
	};
	
	private int EstimateWorkSizeForQR(int num_rows, int num_cols) {
		
		  char trans = 'N';
		  int nrhs = 1;
		  int lwork = -1;
		  double work[] = new double[1];
		  int info[] = new int[1];
		  gi2.dgels(trans, num_rows, num_cols, nrhs, null, num_rows,
		            null, num_rows, work, lwork, info);

		  if (info[0] < 0) {
		    System.err.println("Congratulations, you found a bug in Ceres.");
		    System.err.println("Please report it.");
		    System.err.println("LAPACK::dgels fatal error.");
		    System.err.println("Argument: " + (-info[0]) + " is invalid.");
		  }
		  return (int)work[0];
	}
	
	private void SymmetricRankUpdate(int num_rows,
            int num_cols,
            double a[],
            boolean transpose,
            double alpha,
            double beta,
            double c[][]) {
		
		char uplo = 'L';
		char trans = transpose ? 'T' : 'N';
		int n = transpose ? num_cols : num_rows;
		int k = transpose ? num_rows : num_cols;
		int lda = k;
		int ldc = n;
		double A[][] = new double[lda][n];
		int i = 0;
		for (int col = 0; col < n; col++) {
		for (int r = 0; r < k; r++) {
			    A[r][col] = a[i++];
			}
		}
		ge.dsyrk(uplo, trans, n, k, alpha, A, lda, beta, c, ldc);
		// Have only filled in lower triangle of n by n c
		for (int r = 0; r < n; r++) {
			for (int col = r+1; col < n; col++) {
				c[r][col] = c[col][r];
			}
		}
	}

	
	private LinearSolverTerminationType SolveInPlaceUsingQR(
		    int num_rows,
		    int num_cols,
		    double in_lhs[][],
		    int work_size,
		    double work[],
		    double rhs_and_solution[],
		    String message[]) {
		  char trans = 'N';
		  int m = num_rows;
		  int n = num_cols;
		  int nrhs = 1;
		  int lda = num_rows;
		  int ldb = num_rows;
		  int info[] = new int[1];
		  double B[][] = new double[ldb][1];
		  int i;
		  for (i = 0; i < Math.min(rhs_and_solution.length,ldb); i++) {
			  B[i][0] = rhs_and_solution[i];
		  }

		  gi2.dgels(trans, m, n, nrhs, in_lhs, lda,
		         B, ldb, work, work_size, info);
		  for (i = 0; i < Math.min(rhs_and_solution.length,ldb); i++) {
			  rhs_and_solution[i] = B[i][0];
		  }

		  if (info[0] < 0) {
		    System.err.println("Congratulations, you found a bug in Ceres.");
		    System.err.println("Please report it.");
		    System.err.println("LAPACK::dgels fatal error.");
		    System.err.println("Argument: " + (-info[0]) + " is invalid.");
		    return LinearSolverTerminationType.LINEAR_SOLVER_FATAL_ERROR;
		  }

		  message[0] = "Success.";
		  return LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS;
		}

	private LinearSolverTerminationType SolveInPlaceUsingCholesky(
		    int num_rows,
		    double in_lhs[][],
		    double rhs_and_solution[],
		    String message[]) {
		  char uplo = 'L';
		  int n = num_rows;
		  int info[] = new int[1];
		  int nrhs = 1;
		  double B[][] = new double[n][1];
		  int i;
		  for (i = 0; i < Math.min(rhs_and_solution.length,n); i++) {
			  B[i][0] = rhs_and_solution[i];
		  }

		  ge.dpotrf(uplo, n, in_lhs, n, info);
		  if (info[0] < 0) {
		    System.err.println("Congratulations, you found a bug in Ceres.");
		    System.err.println("Please report it.");
		    System.err.println("LAPACK::dpotrf fatal error.");
		    System.err.println("Argument: " + (-info[0]) + " is invalid.");
		    return LinearSolverTerminationType.LINEAR_SOLVER_FATAL_ERROR;
		  }

		  if (info[0] > 0) {
		    message[0] =
		        String.format(
		            "LAPACK::dpotrf numerical failure.\n" + 
		             "The leading minor of order %d is not positive definite.", info[0]);
		    return LinearSolverTerminationType.LINEAR_SOLVER_FAILURE;
		  }

		  le.dpotrs(uplo, n, nrhs, in_lhs, n, B, n, info);
		  for (i = 0; i < Math.min(rhs_and_solution.length,n); i++) {
			  rhs_and_solution[i] = B[i][0];
		  }

		  if (info[0] < 0) {
		    System.err.println("Congratulations, you found a bug in Ceres.");
		    System.err.println("Please report it.");
		    System.err.println("LAPACK::dpotrs fatal error.");
		    System.err.println("Argument: " + (-info[0]) + " is invalid.");
		    return LinearSolverTerminationType.LINEAR_SOLVER_FATAL_ERROR;
		  }

		  message[0] = "Success";
		  return LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS;
		}

	
	class DenseSparseMatrix extends SparseMatrix {
	     // Column major matrices for DenseSparseMatrix/DenseQRSolver
	     // ColMajorMatrix m_;
		 private Matrix m_;
	     private boolean has_diagonal_appended_;
	     private boolean has_diagonal_reserved_;
		  // Build a matrix with the same content as the TripletSparseMatrix
		  // m. This assumes that m does not have any repeated entries.
		  public DenseSparseMatrix(TripletSparseMatrix m) {
			  super();
			  double array[][] = new double[m.num_rows()][m.num_cols()];
			  m_ = new Matrix(array);
		      has_diagonal_appended_ = false;
		      has_diagonal_reserved_ = false;
		      double values[] = m.values();
		      int rows[] = m.rows();
		      int cols[] = m.cols();
		      int num_nonzeros = m.num_nonzeros();

		      for (int i = 0; i < num_nonzeros; ++i) {
		    	  m_.getArray()[rows[i]][cols[i]] += values[i];
		      }

		  }
		  
		  //explicit DenseSparseMatrix(const ColMajorMatrix& m);
		  public DenseSparseMatrix(Matrix m) {
		      super();	
		      m_ = m;
		      has_diagonal_appended_ = false;
		      has_diagonal_reserved_ = false; 
		  }

		  public DenseSparseMatrix(int num_rows, int num_cols) {
			  super();
			  has_diagonal_appended_ = false;
		      has_diagonal_reserved_ = false;
		      double array[][] = new double[num_rows][num_cols];
		      m_ = new Matrix(array);
		  }
		  
		  public DenseSparseMatrix(int num_rows, int num_cols, boolean reserve_diagonal) {
			  super();
			  has_diagonal_appended_ = false;
		      has_diagonal_reserved_ = reserve_diagonal;
		      double[][] array;
		      if (reserve_diagonal) {
		         // Allocate enough space for the diagonal.
		    	 array = new double[num_rows + num_cols][num_cols];
		      } else {
		    	 array = new double[num_rows][num_cols];
		      }
		      m_ = new Matrix(array);
		  }
		  
		  public void setMatrix(int r, int c, double val) {
			  m_.set(r,c,val);
		  }

		  // SparseMatrix interface.
		  public void SetZero() {
			  int rows = m_.getRowDimension();
			  int cols = m_.getColumnDimension();
			  for (int i = 0; i < rows; i++) {
				  for (int j = 0; j < cols; j++) {
					  m_.getArray()[i][j] = 0.0;
				  }
			  }
		  }
		  
		  public void RightMultiply(double x[], double y[]) {
              double array[][] = matrix().getArray();
              double mx[] = new double[num_rows()];
              for (int r = 0; r < num_rows(); r++) {
            	  for (int c = 0; c < num_cols(); c++) {
            		  mx[r] += (array[r][c] * x[c]);  
            	  }
              }
              for (int r = 0; r < num_rows(); r++) {
            	  y[r] += mx[r];
              }
		  }
		  
		  public void LeftMultiply(double x[], double y[]) {
		      double array[][] = matrix().transpose().getArray();
              double mx[] = new double[num_cols()];
              for (int r = 0; r < num_cols(); r++) {
            	  for (int c = 0; c < num_rows(); c++) {
            		  mx[r] += (array[r][c] * x[c]);
            	  }
              }
        	  for (int r = 0; r < num_cols(); r++) {
            	  y[r] += mx[r];
              }
		  }
		  
		  public void SquaredColumnNorm(double x[]) {
              double array[][] = m_.getArray();
              int rows = m_.getRowDimension();
              for (int c = 0; c < num_cols(); c++) {
            	  x[c] = 0.0;
            	  for (int r = 0; r < rows; r++) {
            		  x[c] += (array[r][c]*array[r][c]);  
            	  }
              }
		  }
		  
		  public void ScaleColumns(double[] scale) {
			  double array[][] = m_.getArray();
			  int rows = m_.getRowDimension();  
              for (int c = 0; c < num_cols(); c++) {
            	  for (int r = 0; r < rows; r++) {
            		  array[r][c] *= scale[c];  
            	  }
              }
		  }
		  
		  public Matrix ToDenseMatrix() {
			  double new_array[][] = new double[num_rows()][num_cols()];
			  for (int r = 0; r < num_rows(); r++) {
				  for (int c = 0; c < num_cols(); c++) {
					  new_array[r][c] = m_.getArray()[r][c];  
				  }
			  }
			  return new Matrix(new_array);
		  }
		  
		  public void ToTextFile(File file) {
			  if (file == null) {
				  System.err.println("file == null in ToTextFile");
				  return;
			  }
			  FileWriter fw = null;
				try {
					fw = new FileWriter(file);
				} catch (IOException e) {
					System.err.println("IOException in ToTextFile on new FileWriter(file)");
					return;
				}
			  
				int active_rows =
					      (has_diagonal_reserved_ && !has_diagonal_appended_)
					      ? (m_.getRowDimension() - m_.getColumnDimension())
					      : m_.getRowDimension();

					  for (int r = 0; r < active_rows; ++r) {
					    for (int c = 0; c < m_.getColumnDimension(); ++c) {
					      String str = String.format("% 10d % 10d %17f\n", r, c, m_.getArray()[r][c]);
					      try {
								fw.write(str, 0, str.length());
							} catch (IOException e) {
								System.err.println("IOException in ToTextFile on fw.write(str,0,str.length())");
								return;
							}
					    }
					  }
			  try {
				    fw.close();
				}
				catch (IOException e) {
					System.err.println("IOException in ToTextFile on fw.close()");
				}
  
		  }
		  
		  public int num_rows() {
			  if (has_diagonal_reserved_ && !has_diagonal_appended_) {
				    return m_.getRowDimension() - m_.getColumnDimension();
				  }
				  return m_.getRowDimension();
		  }
		  
		  public int num_cols() {
			  return m_.getColumnDimension();
		  }
		  
		  public int num_nonzeros() {
			  if (has_diagonal_reserved_ && !has_diagonal_appended_) {
				    return (m_.getRowDimension() - m_.getColumnDimension()) * m_.getColumnDimension();
				  }
				  return m_.getRowDimension() * m_.getColumnDimension();

		  }
		  public double[] values() { 
			  int rows = m_.getRowDimension();
			  int cols = m_.getColumnDimension();
			  double array[][] = m_.getArray();
			  double val[] = new double[rows * cols];
			  int index = 0;
			  for (int c = 0; c < cols; c++) {
				  for (int r = 0; r < rows; r++, index++) {
					  val[index] = array[r][c];
				  }
			  }
			  return val;
		  }
		  
		  public double[] mutable_values() {
			  int rows = m_.getRowDimension();
			  int cols = m_.getColumnDimension();
			  double array[][] = m_.getArray();
			  double val[] = new double[rows * cols];
			  int index = 0;
			  for (int c = 0; c < cols; c++) {
				  for (int r = 0; r < rows; r++, index++) {
					  val[index] = array[r][c];
				  }
			  }
			  return val;  
		  }
		  
		  public void restoreBackFromColMajorRef(Matrix new_arrayMat) {
			  int original_rows = m_.getRowDimension();
	          int original_cols = m_.getColumnDimension();
	          double original_array[][] = m_.getArray();
              int new_rows;
              if (has_diagonal_reserved_ && !has_diagonal_appended_) {
            	  new_rows = original_rows - original_cols;
              }
              else {
            	  new_rows = original_rows;
              }
              int new_cols = original_cols;
              double new_array[][] = new_arrayMat.getArray();
              for (int row = 0; row < new_rows; row++) {
            	  for (int col = 0; col < new_cols; col++) {
            		  original_array[row][col] = new_array[row][col];
            	  }
              }
		  }

		  //ConstColMajorMatrixRef matrix() const;
		  public Matrix matrix() {
			          int original_rows = m_.getRowDimension();
			          int original_cols = m_.getColumnDimension();
			          double original_array[][] = m_.getArray();
                      int new_rows;
                      if (has_diagonal_reserved_ && !has_diagonal_appended_) {
                    	  new_rows = original_rows - original_cols;
                      }
                      else {
                    	  new_rows = original_rows;
                      }
                      int new_cols = original_cols;
                      double new_array[][] = new double[new_rows][new_cols];
                      for (int row = 0; row < new_rows; row++) {
                    	  for (int col = 0; col < new_cols; col++) {
                    		  new_array[row][col] = original_array[row][col];
                    	  }
                      }
                      return new Matrix(new_array);    
		  }
		  
		  //ColMajorMatrixRef mutable_matrix();
		  public Matrix mutable_matrix() {
			  int original_rows = m_.getRowDimension();
	          int original_cols = m_.getColumnDimension();
	          double original_array[][] = m_.getArray();
              int new_rows;
              if (has_diagonal_reserved_ && !has_diagonal_appended_) {
            	  new_rows = original_rows - original_cols;
              }
              else {
            	  new_rows = original_rows;
              }
              int new_cols = original_cols;
              double new_array[][] = new double[new_rows][new_cols];
              for (int row = 0; row < new_rows; row++) {
            	  for (int col = 0; col < new_cols; col++) {
            		  new_array[row][col] = original_array[row][col];
            	  }
              }
              return new Matrix(new_array);    	  
		  }

		  // Only one diagonal can be appended at a time. The diagonal is appended to
		  // as a new set of rows, e.g.
		  //
		  // Original matrix:
		  //
		  //   x x x
		  //   x x x
		  //   x x x
		  //
		  // After append diagonal (1, 2, 3):
		  //
		  //   x x x
		  //   x x x
		  //   x x x
		  //   1 0 0
		  //   0 2 0
		  //   0 0 3
		  //
		  // Calling RemoveDiagonal removes the block. It is a fatal error to append a
		  // diagonal to a matrix that already has an appended diagonal, and it is also
		  // a fatal error to remove a diagonal from a matrix that has none.
		  public void AppendDiagonal(double d[]) {
			  if (has_diagonal_appended_) {
				  System.err.println("has_diagonal_appended = true in AppendDiagonal");
				  return;
			  }
			  
			  if (!has_diagonal_reserved_) {
				int original_rows = m_.getRowDimension();
				int original_cols = m_.getColumnDimension();
				double original_array[][] = m_.getArray();
				int new_rows = original_rows + original_cols;
				int new_cols = original_cols;
				double new_array[][] = new double[new_rows][new_cols];
				for (int r = 0; r < original_rows; r++) {
					for (int c = 0; c < original_cols; c++) {
						new_array[r][c] = original_array[r][c];
					}
				}
			    m_ = new Matrix(new_array);
			    has_diagonal_reserved_ = true;
			  }

			  for (int c = 0; c < m_.getColumnDimension(); c++) {
				  m_.getArray()[m_.getRowDimension()-m_.getColumnDimension()+c][c] = d[c];
			  }
			  has_diagonal_appended_ = true;
  
		  }
		  
		  public void RemoveDiagonal() {
			  if (!has_diagonal_appended_) {
				  System.err.println("has_diagonal_appended_ = false in RemoveDiagonal");
				  return;
			  }
			  has_diagonal_appended_ = false;
			  // Leave the diagonal reserved.

		  }

		 
		};

	// A conjugate gradients on the normal equations solver. This directly solves
	// for the solution to
	//
	// (A^T A + D^T D)x = A^T b
	//
	// as required for solving for x in the least squares sense. Currently only
	// block diagonal preconditioning is supported.
	 class CgnrSolver extends TypedLinearSolver<BlockSparseMatrix> {
	     //scoped_ptr<Preconditioner> preconditioner_;
	     private Preconditioner preconditioner_; 
	     
	     public CgnrSolver(LinearSolverOptions options) {
	       super();
	       options_ = options;
	       options_.type = LinearSolverType.CGNR;
	       preconditioner_ = null;
		     if (options_.preconditioner_type != PreconditionerType.JACOBI &&
		         options_.preconditioner_type != PreconditionerType.IDENTITY) {
		       System.err.println("CGNR only supports IDENTITY and JACOBI preconditioners.");
		     }
	     } // public CgnrSolver
	     
	     public LinearSolverSummary Solve(LinearOperator A, double b[], LinearSolverPerSolveOptions per_solve_options,
					double x[]) {
				new ScopedExecutionTimer("LinearSolver::Solve", execution_summary_);
				if (A == null) {
					System.err.println("In TypedLinearSolver public LinearSummary.Summary Solve LinearOperator A == null");
					return null;
				}
				if (b == null) {
					System.err.println("In TypedLinearSolver public LinearSummary.Summary Solve double b[] == null");
					return null;
				}
				if (x == null) {
					System.err.println("In TypedLinearSolver public LinearSummary.Summary Solve double x[] == null");
					return null;
				}
				// return SolveImpl(down_cast<MatrixType*>(A), b, per_solve_options, x);
				return SolveImpl((BlockSparseMatrix) A, b, per_solve_options, x);
			}
	     
	   public LinearSolverSummary SolveImpl(
	    		    BlockSparseMatrix A,
	    		    double b[],
	    		    LinearSolverPerSolveOptions per_solve_options,
	    		    double x[]) {
	    		  EventLogger event_logger = new EventLogger("CgnrSolver::Solve");

	    		  // Form z = Atb.
	    		  double z[] = new double[A.num_cols()];
	    		  A.LeftMultiply(b, z);

	    		  // Precondition if necessary.
	    		  LinearSolverPerSolveOptions cg_per_solve_options = per_solve_options;
	    		  if (options_.preconditioner_type == PreconditionerType.JACOBI) {
	    		    if (preconditioner_ == null) {
	    		      preconditioner_ = new BlockJacobiPreconditioner(A);
	    		    }
	    		    preconditioner_.Update(A, per_solve_options.D);
	    		    cg_per_solve_options.preconditioner = preconditioner_;
	    		  }

	    		  // Solve (AtA + DtD)x = z (= Atb).
	    		  for (int i = 0; i < A.num_cols(); i++) {
	    			  x[i] = 0.0;
	    		  }
	    		  CgnrLinearOperator lhs = new CgnrLinearOperator(A, per_solve_options.D);
	    		  event_logger.AddEvent("Setup");

	    		  ConjugateGradientsSolver conjugate_gradient_solver = new ConjugateGradientsSolver(options_);
	    		  summary = 
	    		      conjugate_gradient_solver.Solve(lhs, z, cg_per_solve_options, x);
	    		  event_logger.AddEvent("Solve");
	    		  return summary;
	    		}
	     
	 } // class CgnrSolver
	 
	 class ConjugateGradientsSolver extends LinearSolver {
		   private LinearSolverOptions options_;
		   public ConjugateGradientsSolver(LinearSolverOptions options) {
			   super();
			   options_ = options;
		   }
		   
		   public LinearSolverSummary Solve(LinearOperator A, Vector<Double> bvec, LinearSolverPerSolveOptions per_solve_options,
				   Vector<Double> xvec) {
			   int i;
			   double b[] = new double[bvec.size()];
			   for (i = 0; i < bvec.size(); i++) {
				   b[i] = bvec.get(i);
			   }
			   double x[] = new double[xvec.size()];
			   for (i = 0; i < xvec.size(); i++) {
				   x[i] = xvec.get(i);
			   }
			   LinearSolverSummary ls = Solve(A, b, per_solve_options, x);
			   for (i = 0; i < xvec.size(); i++) {
				   xvec.setElementAt(x[i],i);
			   }
			   return ls;
		   }
		   
		    public LinearSolverSummary Solve(LinearOperator A,
		                          double b[],
		                          LinearSolverPerSolveOptions per_solve_options,
		                          double x[]) {
		    	  int i;
		    	  if (A == null) {
		    		  System.err.println("LinearOperator A == null in ConjugateGradientsSolver Solve");
		    		  return null;
		    	  }
		    	  if (x == null) {
		    		  System.err.println("double x[] == null in ConjugateGradientsSolver Solve");
		    		  return null;
		    	  }
		    	  if (b == null) {
		    		  System.err.println("double b[] == null in ConjugateGradientsSolver Solve");
		    		  return null;
		    	  }
		    	  if (A.num_rows() != A.num_cols()) {
		    		  System.err.println("A.num_rows() != A.num_cols() in ConjugateGradientsSolver Solve");
		    		  return null;
		    	  }

		    	  summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_NO_CONVERGENCE;
		    	  summary.message[0] = "Maximum number of iterations reached.";
		    	  summary.num_iterations = 0;

		    	  int num_cols = A.num_cols();
		    	  //VectorRef xref(x, num_cols);
		    	  //ConstVectorRef bref(b, num_cols);

		    	  double norm_b = 0.0;
		    	  for (i = 0; i < num_cols; i++) {
		    		  norm_b += b[i]*b[i];
		    	  }
		    	  norm_b = Math.sqrt(norm_b);
		    	  if (norm_b == 0.0) {
		    		for (i = 0; i < num_cols; i++) {
		    			x[i] = 0.0;
		    		}
		    	    summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS;
		    	    summary.message[0] = "Convergence. |b| = 0.";
		    	    return summary;
		    	  }

		    	  double r[] = new double[num_cols];
		    	  double p[] = new double[num_cols];
		    	  double z[] = new double[num_cols];
		    	  double tmp[] = new double[num_cols];

		    	  double tol_r = per_solve_options.r_tolerance * norm_b;

		    	  A.RightMultiply(x, tmp);
		    	  double norm_r = 0.0;
		    	  for (i = 0; i < num_cols; i++) {
		    		  r[i] = b[i] - tmp[i];
		    		  norm_r += (r[i]*r[i]);
		    	  }
		    	  norm_r = Math.sqrt(norm_r);
		    	  if (options_.min_num_iterations == 0 && norm_r <= tol_r) {
		    	    summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS;
		    	    summary.message[0] =
		    	        String.format("Convergence. |r| = %e <= %e.", norm_r, tol_r);
		    	    return summary;
		    	  }

		    	  double rho = 1.0;

		    	  // Initial value of the quadratic model Q = x'Ax - 2 * b'x.
		    	  double Q0 = 0.0;
		    	  for (i = 0; i < num_cols; i++) {
		    		  Q0 -= (x[i]*(b[i] + r[i]));
		    	  }

		    	  for (summary.num_iterations = 1;; ++summary.num_iterations) {
		    	    // Apply preconditioner
		    	    if (per_solve_options.preconditioner != null) {
		    	      for (i = 0; i < num_cols; i++) {
		    	    	  z[i] = 0.0;
		    	      }
		    	      per_solve_options.preconditioner.RightMultiply(r, z);
		    	    } else {
		    	      for (i = 0; i < num_cols; i++) {
		    	          z[i] = r[i];
		    	      }
		    	    }

		    	    double last_rho = rho;
		    	    rho = 0.0;
		    	    for (i = 0; i < num_cols; i++) {
		    	    	rho += (r[i]*z[i]);
		    	    }
		    	    if ((Double.isInfinite(rho)) || (rho == 0.0)) {
		    	      summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_FAILURE;
		    	      summary.message[0] = String.format("Numerical failure. rho = r'z = %e.", rho);
		    	      break;
		    	    }

		    	    if (summary.num_iterations == 1) {
		    	      for (i = 0; i < num_cols; i++) {
				          p[i] = z[i];
				      }
		    	    } else {
		    	      double beta = rho / last_rho;
		    	      if ((Double.isInfinite(beta)) || (beta == 0.0)) {
		    	        summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_FAILURE;
		    	        summary.message[0] = String.format(
		    	            "Numerical failure. beta = rho_n / rho_{n-1} = %e, rho_n = %e, rho_{n-1} = %e", beta, rho, last_rho);
		    	        break;
		    	      }
		    	      for (i = 0; i < num_cols; i++) {
		    	      p[i] = z[i] + beta * p[i];
		    	      }
		    	    }

		    	    double q[] = z;
		    	    for (i = 0; i < num_cols; i++) {
		    	        q[i] = 0.0;
		    	    }
		    	    A.RightMultiply(p, q);
		    	    double pq = 0.0;
		    	    for (i = 0; i < num_cols; i++) {
		    	    	pq += (p[i]*q[i]);
		    	    }
		    	    if ((pq <= 0) || Double.isInfinite(pq))  {
		    	      summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_NO_CONVERGENCE;
		    	      double pnorm = 0.0;
		    	      double qnorm = 0.0;
		    	      for (i = 0; i < num_cols; i++) {
		    	    	  pnorm += (p[i]*p[i]);
		    	    	  qnorm += (q[i]*q[i]);
		    	      }
		    	      pnorm = Math.sqrt(pnorm);
		    	      qnorm = Math.sqrt(qnorm);
		    	      summary.message[0] = String.format(
		    	          "Matrix is indefinite, no more progress can be made. p'q = %e. |p| = %e, |q| = %e",
		    	          pq, pnorm, qnorm);
		    	      break;
		    	    }

		    	    double alpha = rho / pq;
		    	    if (Double.isInfinite(alpha)) {
		    	      summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_FAILURE;
		    	      summary.message[0] =
		    	          String.format("Numerical failure. alpha = rho / pq = %e, rho = %e, pq = %e.", alpha, rho, pq);
		    	      break;
		    	    }

		    	    for (i = 0; i < num_cols; i++) {
		    	        x[i] = x[i] + alpha * p[i];
		    	    }

		    	    // Ideally we would just use the update r = r - alpha*q to keep
		    	    // track of the residual vector. However this estimate tends to
		    	    // drift over time due to round off errors. Thus every
		    	    // residual_reset_period iterations, we calculate the residual as
		    	    // r = b - Ax. We do not do this every iteration because this
		    	    // requires an additional matrix vector multiply which would
		    	    // double the complexity of the CG algorithm.
		    	    if (summary.num_iterations % options_.residual_reset_period == 0) {
		    	      for (i = 0; i < num_cols; i++) {
		    	          tmp[i] = 0.0;
		    	      }
		    	      A.RightMultiply(x, tmp);
		    	      for (i = 0; i < num_cols; i++) {
		    	          r[i] = b[i] - tmp[i];
		    	      }
		    	    } else {
		    	    	for (i = 0; i < num_cols; i++) {
		    	            r[i] = r[i] - alpha * q[i];
		    	    	}
		    	    }

		    	    // Quadratic model based termination.
		    	    //   Q1 = x'Ax - 2 * b' x.
		    	    double Q1 = 0.0;
		    	    for (i = 0; i < num_cols; i++) {
		    	       Q1 -= (x[i]*(b[i] + r[i]));
		    	    }

		    	    // For PSD matrices A, let
		    	    //
		    	    //   Q(x) = x'Ax - 2b'x
		    	    //
		    	    // be the cost of the quadratic function defined by A and b. Then,
		    	    // the solver terminates at iteration i if
		    	    //
		    	    //   i * (Q(x_i) - Q(x_i-1)) / Q(x_i) < q_tolerance.
		    	    //
		    	    // This termination criterion is more useful when using CG to
		    	    // solve the Newton step. This particular convergence test comes
		    	    // from Stephen Nash's work on truncated Newton
		    	    // methods. References:
		    	    //
		    	    //   1. Stephen G. Nash & Ariela Sofer, Assessing A Search
		    	    //   Direction Within A Truncated Newton Method, Operation
		    	    //   Research Letters 9(1990) 219-221.
		    	    //
		    	    //   2. Stephen G. Nash, A Survey of Truncated Newton Methods,
		    	    //   Journal of Computational and Applied Mathematics,
		    	    //   124(1-2), 45-59, 2000.
		    	    //
		    	    double zeta = summary.num_iterations * (Q1 - Q0) / Q1;
		    	    if (zeta < per_solve_options.q_tolerance &&
		    	        summary.num_iterations >= options_.min_num_iterations) {
		    	      summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS;
		    	      double rnorm = 0.0;
		    	      for (i = 0; i < num_cols; i++) {
		    	    	  rnorm += (r[i]*r[i]);
		    	      }
		    	      rnorm = Math.sqrt(rnorm);
		    	      summary.message[0] =
		    	          String.format("Iteration: %d Convergence: zeta = %e < %e. |r| = %e",
		    	                       summary.num_iterations,
		    	                       zeta,
		    	                       per_solve_options.q_tolerance,
		    	                       rnorm);
		    	      break;
		    	    }
		    	    Q0 = Q1;

		    	    // Residual based termination.
		    	      norm_r = 0.0;
		    	      for (i = 0; i < num_cols; i++) {
		    	    	  norm_r += (r[i]*r[i]);
		    	      }
		    	      norm_r = Math.sqrt(norm_r);
		    	    if (norm_r <= tol_r &&
		    	        summary.num_iterations >= options_.min_num_iterations) {
		    	      summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS;
		    	      summary.message[0] =
		    	          String.format("Iteration: %d Convergence. |r| = %e <= %e.",
		    	                       summary.num_iterations,
		    	                       norm_r,
		    	                       tol_r);
		    	      break;
		    	    }

		    	    if (summary.num_iterations >= options_.max_num_iterations) {
		    	      break;
		    	    }
		    	  }

		    	  return summary;

		    }

		   
		  };
	 
	// Note: This class is not thread safe, since it uses some temporary storage.
	 class CgnrLinearOperator extends LinearOperator {
		 private LinearOperator A_;
	     private double D_[];
	     //scoped_array<double> z_;
	     private double z_[];
	  
	    public CgnrLinearOperator(LinearOperator A, double D[]) {
	       super();
	       A_ = A;
	       D_ = D; 
	       z_ = new double[A.num_rows()];
	   }

	   public void RightMultiply(double x[], double y[]) {
		 int i,j;
		 for (i = 0; i < A_.num_rows(); i++) {
			 z_[i] = 0.0;
		 }

	     // z = Ax
	     A_.RightMultiply(x, z_);

	     // y = y + Atz
	     A_.LeftMultiply(z_, y);

	     // y = y + DtDx
	     if (D_ != null) {
	       int n = A_.num_cols();
	       double DtD[][] = new double[n][n];
	       for (i = 0; i < n; i++) {
	    	   for (j = 0; j <= i; j++) {
	    		   DtD[i][j] = D_[i]*D_[j];
	    	   }
	    	   for (j = i+1; j < n; j++) {
	    		   DtD[i][j] = DtD[j][i];
	    	   }
	       }
	       double DtDx[] = new double[n];
	       for (i = 0; i < n; i++) {
	    	   for (j = 0; j < n; j++) {
	    		   DtDx[i] += DtD[i][j]*x[j];
	    	   }
	       }
	       for (i = 0; i < n; i++) {
	    	   y[i] = y[i] + DtDx[i];
	       }
	     }
	   }

	   public void LeftMultiply(double x[], double y[]) {
	     RightMultiply(x, y);
	   }

	   public int num_rows() { return A_.num_cols(); }
	   public int num_cols() { return A_.num_cols(); }
	 };
	 
	// A block Jacobi preconditioner. This is intended for use with
	// conjugate gradients, or other iterative symmetric solvers. To use
	// the preconditioner, create one by passing a BlockSparseMatrix "A"
	// to the constructor. This fixes the sparsity pattern to the pattern
	// of the matrix A^TA.
	//
	// Before each use of the preconditioner in a solve with conjugate gradients,
	// update the matrix by running Update(A, D). The values of the matrix A are
	// inspected to construct the preconditioner. The vector D is applied as the
	// D^TD diagonal term.
	class BlockJacobiPreconditioner extends TypedPreconditioner<BlockSparseMatrix> {
		//scoped_ptr<BlockRandomAccessDiagonalMatrix> m_;
		private BlockRandomAccessDiagonalMatrix m_;
	  // A must remain valid while the BlockJacobiPreconditioner is.
	  public  BlockJacobiPreconditioner(BlockSparseMatrix A) {
		  CompressedRowBlockStructure bs = A.block_structure();
		  Vector<Integer> blocks = new Vector<Integer>(bs.cols.size());
		  for (int i = 0; i < bs.cols.size(); ++i) {
		    blocks.add(bs.cols.get(i).size);
		  }

		  m_ = new BlockRandomAccessDiagonalMatrix(blocks);

	  }

	  // Preconditioner interface
	  public void RightMultiply(double x[], double y[]) {
		  m_.RightMultiply(x, y);

	  }
	  public int num_rows() { return m_.num_rows(); }
	  public int num_cols() { return m_.num_rows(); }
	  public BlockRandomAccessDiagonalMatrix matrix() { return m_; }

	  // Won't work until Invert() for BlockRandomAccessDiagonalMatrix is implemented
	  public boolean UpdateImpl(BlockSparseMatrix A, double D[]) {
		  CompressedRowBlockStructure bs = A.block_structure();
		  double values[] = A.values();
		  m_.SetZero();
		  for (int i = 0; i < bs.rows.size(); ++i) {
		    int row_block_size = bs.rows.get(i).block.size;
		    Vector<Cell> cells = bs.rows.get(i).cells;
		    for (int j = 0; j < cells.size(); ++j) {
		      int block_id = cells.get(j).block_id;
		      int col_block_size = bs.cols.get(block_id).size;

		      int r[] = new int[1];
		      int c[] = new int[1];
		      int row_stride[] = new int[1];
		      int col_stride[] = new int[1];
		      CellInfo cell_info = m_.GetCell(block_id, block_id,
		                                        r, c,
		                                        row_stride, col_stride);
		      double m[][] = new double[row_stride[0]][col_stride[0]];
		      int values_ptr = 0;
		      for (int row = 0; row < row_stride[0]; row++) {
		    	  for (int col = 0; col < col_stride[0]; col++, values_ptr++) {
		    		  m[row][col] = cell_info.values[cell_info.values_index + values_ptr];
		    	  }
		      }
		      double b[][] = new double[row_block_size][col_block_size];
		      double bT[][] = new double[col_block_size][row_block_size];
		      values_ptr = 0;
		      for (int row = 0; row < row_block_size; row++) {
		    	  for (int col = 0; col < col_block_size; col++, values_ptr++) {
		    		  b[row][col] = values[values_ptr + cells.get(j).position];
		    		  bT[col][row] = b[row][col];
		    	  }
		      }
		      double bTb[][] = new double[col_block_size][col_block_size];
		      for (int row = 0; row < col_block_size; row++) {
		    	  for (int col = 0; col < col_block_size; col++) {
		    		  for (int k = 0; k < row_block_size; k++) {
		    			  bTb[row][col] += (bT[row][k] * b[k][col]);
		    		  }
		    	  }
		      }
		      for (int row = 0; row < col_block_size; row++) {
		    	  for (int col = 0; col < col_block_size; col++) {
		    		  m[r[0] + row][c[0] + col] += bTb[row][col];
		    	  }
		      }
		      values_ptr = 0;
		      for (int row = 0; row < row_stride[0]; row++) {
		    	  for (int col = 0; col < col_stride[0]; col++, values_ptr++) {
		    		  cell_info.values[cell_info.values_index + values_ptr] = m[row][col];
		    	  }
		      }
		    }
		  }

		  if (D != null) {
		    // Add the diagonal.
		    int position = 0;
		    for (int i = 0; i < bs.cols.size(); ++i) {
		      int block_size = bs.cols.get(i).size;
		      int r[] = new int[1];
		      int c[] = new int[1];
		      int row_stride[] = new int[1];
		      int col_stride[] = new int[1];
		      CellInfo cell_info = m_.GetCell(i, i,
		                                        r, c,
		                                        row_stride, col_stride);
		      double m[][] = new double[row_stride[0]][col_stride[0]];
		      int values_ptr = 0;
		      for (int row = 0; row < row_stride[0]; row++) {
		    	  for (int col = 0; col < col_stride[0]; col++, values_ptr++) {
		    		  m[row][col] = cell_info.values[cell_info.values_index + values_ptr];
		    	  }
		      }
		      for (int k = 0; k < block_size; k++) {
		    	  m[r[0] + k][c[0] + k] += (D[k + position]*D[k + position]);
		      }
		      position += block_size;
		      values_ptr = 0;
		      for (int row = 0; row < row_stride[0]; row++) {
		    	  for (int col = 0; col < col_stride[0]; col++, values_ptr++) {
		    		  cell_info.values[cell_info.values_index + values_ptr] = m[row][col];
		    	  }
		      }
		    }
		  }

		  m_.Invert();
		  return true;  
	  }
	};
	
	// This templated subclass of Preconditioner serves as a base class for
	// other preconditioners that depend on the particular matrix layout of
	// the underlying linear operator.
	// Preconditioners that depend on acccess to the low level structure
	// of a SparseMatrix.
	//typedef TypedPreconditioner<SparseMatrix>              SparseMatrixPreconditioner; 
	//typedef TypedPreconditioner<BlockSparseMatrix>         BlockSparseMatrixPreconditioner;
	//typedef TypedPreconditioner<CompressedRowSparseMatrix> CompressedRowSparseMatrixPreconditioner;
	
	abstract class SchurEliminatorBase {

		  // Initialize the eliminator. It is the user's responsibilty to call
		  // this function before calling Eliminate or BackSubstitute. It is
		  // also the caller's responsibilty to ensure that the
		  // CompressedRowBlockStructure object passed to this method is the
		  // same one (or is equivalent to) the one associated with the
		  // BlockSparseMatrix objects below.
		  //
		  // assume_full_rank_ete controls how the eliminator inverts with the
		  // diagonal blocks corresponding to e blocks in A'A. If
		  // assume_full_rank_ete is true, then a Cholesky factorization is
		  // used to compute the inverse, otherwise a singular value
		  // decomposition is used to compute the pseudo inverse.
		  public abstract void Init(int num_eliminate_blocks,
		                    boolean assume_full_rank_ete,
		                    CompressedRowBlockStructure bs);

		  // Compute the Schur complement system from the augmented linear
		  // least squares problem [A;D] x = [b;0]. The left hand side and the
		  // right hand side of the reduced linear system are returned in lhs
		  // and rhs respectively.
		  //
		  // It is the caller's responsibility to construct and initialize
		  // lhs. Depending upon the structure of the lhs object passed here,
		  // the full or a submatrix of the Schur complement will be computed.
		  //
		  // Since the Schur complement is a symmetric matrix, only the upper
		  // triangular part of the Schur complement is computed.
		  public abstract void Eliminate(BlockSparseMatrix A,
		                         double[] b,
		                         double[] D,
		                         BlockRandomAccessMatrix lhs,
		                         double[] rhs);

		  // Given values for the variables z in the F block of A, solve for
		  // the optimal values of the variables y corresponding to the E
		  // block in A.
		  public abstract void BackSubstitute(BlockSparseMatrix A,
		                              double[] b,
		                              double[] D,
		                              double[] z,
		                              double[] y);
		  // Factory
		  //static SchurEliminatorBase* Create(const LinearSolver::Options& options);
		};
		
		//typedef std::map<int, int> BufferLayoutType;
		  class Chunk {
			  public int size;
			  public int start;
			  public HashMap<Integer, Integer> buffer_layout;
		    public Chunk() {
		    	size = 0;
		    	buffer_layout = new HashMap<Integer, Integer>();
		    }
		    
		  }
		
		//template <int kRowBlockSize = Eigen::Dynamic,
		          //int kEBlockSize = Eigen::Dynamic,
		          //int kFBlockSize = Eigen::Dynamic >
		class SchurEliminator extends SchurEliminatorBase {
			  private int kRowBlockSize;
			  private int kEBlockSize;
			  private int kFBlockSize;
			  private int num_threads_;
			  private Context context_;
			  private int num_eliminate_blocks_;
			  private boolean assume_full_rank_ete_;

			  // Block layout of the columns of the reduced linear system. Since
			  // the f blocks can be of varying size, this vector stores the
			  // position of each f block in the row/col of the reduced linear
			  // system. Thus lhs_row_layout_[i] is the row/col position of the
			  // i^th f block.
			  private Vector<Integer> lhs_row_layout_;

			  // Combinatorial structure of the chunks in A. For more information
			  // see the documentation of the Chunk object above.
			  Vector<Chunk> chunks_;

			  // TODO(sameeragarwal): The following two arrays contain per-thread
			  // storage. They should be refactored into a per thread struct.

			  // Buffer to store the products of the y and z blocks generated
			  // during the elimination phase. buffer_ is of size num_threads *
			  // buffer_size_. Each thread accesses the chunk
			  //
			  //   [thread_id * buffer_size_ , (thread_id + 1) * buffer_size_]
			  //
			  private double[] buffer_;

			  // Buffer to store per thread matrix matrix products used by
			  // ChunkOuterProduct. Like buffer_ it is of size num_threads *
			  // buffer_size_. Each thread accesses the chunk
			  //
			  //   [thread_id * buffer_size_ , (thread_id + 1) * buffer_size_ -1]
			  //
			  private double[] chunk_outer_product_buffer_;

			  int buffer_size_;
			  int uneliminated_row_begins_;

			  // Locks for the blocks in the right hand side of the reduced linear
			  // system.
			  private Vector<Lock> rhs_locks_;
			 public SchurEliminator(int kRowBlockSize, int kEBlockSize, int kFBlockSize, LinearSolverOptions options) {
				 super();
				 this.kRowBlockSize = kRowBlockSize;
				 this.kEBlockSize = kEBlockSize;
				 this.kFBlockSize = kFBlockSize;
			      num_threads_ = options.num_threads;
			      context_ = options.context; 
			      if (options.context == null) {
			    	  System.out.println("options.context == null in public SchurEliminator");
			    	  return;
			      }
			      lhs_row_layout_ = new Vector<Integer>();
			      chunks_ = new Vector<Chunk>();
			      rhs_locks_ = new Vector<Lock>();
			 }
			 
			 public void finalize() {
				 int i;
				 for (i = 0; i < rhs_locks_.size(); i++) {
					 Lock lock = rhs_locks_.get(i);
					 lock = null;
				 }
			 }
			 
			 public void Init(
					    int num_eliminate_blocks,
					    boolean assume_full_rank_ete,
					    CompressedRowBlockStructure bs) {
				      if (num_eliminate_blocks <= 0) {
					      System.err.println("SchurEliminator cannot be initialized with num_eliminate_blocks <= 0.");
					      return;
				      }

					  num_eliminate_blocks_ = num_eliminate_blocks;
					  assume_full_rank_ete_ = assume_full_rank_ete;

					  int num_col_blocks = bs.cols.size();
					  int num_row_blocks = bs.rows.size();

					  buffer_size_ = 1;
					  chunks_.clear();
					  lhs_row_layout_.clear();

					  int lhs_num_rows = 0;
					  // Add a map object for each block in the reduced linear system
					  // and build the row/column block structure of the reduced linear
					  // system.
					  lhs_row_layout_ = new Vector<Integer>(num_col_blocks - num_eliminate_blocks_);
					  for (int i = num_eliminate_blocks_; i < num_col_blocks; ++i) {
					    lhs_row_layout_.add(lhs_num_rows);
					    lhs_num_rows += bs.cols.get(i).size;
					  }

					  int r = 0;
					  // Iterate over the row blocks of A, and detect the chunks. The
					  // matrix should already have been ordered so that all rows
					  // containing the same y block are vertically contiguous. Along
					  // the way also compute the amount of space each chunk will need
					  // to perform the elimination.
					  while (r < num_row_blocks) {
					    int chunk_block_id = bs.rows.get(r).cells.get(0).block_id;
					    if (chunk_block_id >= num_eliminate_blocks_) {
					      break;
					    }

					    chunks_.add(new Chunk());
					    Chunk chunk = chunks_.get(chunks_.size()-1);
					    chunk.size = 0;
					    chunk.start = r;
					    int buffer_size = 0;
					    int e_block_size = bs.cols.get(chunk_block_id).size;

					    // Add to the chunk until the first block in the row is
					    // different than the one in the first row for the chunk.
					    while (r + chunk.size < num_row_blocks) {
					      CompressedList row = bs.rows.get(r + chunk.size);
					      if (row.cells.get(0).block_id != chunk_block_id) {
					        break;
					      }

					      // Iterate over the blocks in the row, ignoring the first
					      // block since it is the one to be eliminated.
					      for (int c = 1; c < row.cells.size(); ++c) {
					        Cell cell = row.cells.get(c);
					        if (!chunk.buffer_layout.containsKey(cell.block_id)) {
					        	chunk.buffer_layout.put(cell.block_id, buffer_size);
					        	buffer_size += e_block_size * bs.cols.get(cell.block_id).size;
					        }
					      }

					      buffer_size_ = Math.max(buffer_size, buffer_size_);
					      ++chunk.size;
					    }

					    if (chunk.size <= 0) {
					    	System.err.println("chunk.size <= 0 in SchurEliminator Init");
					    	return;
					    }
					    r += chunk.size;
					  }
					  Chunk chunk = chunks_.get(chunks_.size()-1);

					  uneliminated_row_begins_ = chunk.start + chunk.size;
					  if (num_threads_ > 1) {
						Collections.shuffle(chunks_);
					  }

					  buffer_ = new double[buffer_size_ * num_threads_];

					  // chunk_outer_product_buffer_ only needs to store e_block_size *
					  // f_block_size, which is always less than buffer_size_, so we just
					  // allocate buffer_size_ per thread.
					  chunk_outer_product_buffer_ = new double[buffer_size_ * num_threads_];

					  rhs_locks_.clear();
					  rhs_locks_ = new Vector<Lock>(num_col_blocks - num_eliminate_blocks_);
					  for (int i = 0; i < num_col_blocks - num_eliminate_blocks_; ++i) {
					    rhs_locks_.add(new ReentrantLock());
					  }
					}
			 
			 public void Eliminate(BlockSparseMatrix A,
			           double[] b,
			           double[] D,
			           BlockRandomAccessMatrix lhs,
			           double[] rhs) {
			   int i;
			   int row;
			   int col;
			   int index;
			   if (lhs.num_rows() > 0) {
			     lhs.SetZero();
			     for (i = 0; i < lhs.num_rows(); i++) {
			    	 rhs[i] = 0.0;
			     }
			   }

			   CompressedRowBlockStructure bs = A.block_structure();
			   int num_col_blocks = bs.cols.size();

			   // Add the diagonal to the schur complement.
			   if (D != null) {
			 //#ifdef CERES_USE_OPENMP
			 //#pragma omp parallel for num_threads(num_threads_) schedule(dynamic)
			 //#endif // CERES_USE_OPENMP

			 //#if !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))
			     for (i = num_eliminate_blocks_; i < num_col_blocks; ++i) {
			 //#else
			     //ParallelFor(context_, num_eliminate_blocks_, num_col_blocks, num_threads_,
			                 //[&](int i) {
			 //#endif // !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))

			       int block_id = i - num_eliminate_blocks_;
			       int r[] = new int[1];
			       int c[] = new int[1];
			       int row_stride[] = new int[1];
			       int col_stride[] = new int[1];
			       CellInfo cell_info = lhs.GetCell(block_id, block_id,
			                                          r, c,
			                                          row_stride, col_stride);
			       if (cell_info != null) {
			         int block_size = bs.cols.get(i).size;
			         double diag[] = new double[block_size];
			         for (row = bs.cols.get(i).position; row < bs.cols.get(i).position + block_size; row++) {
			             diag[row-bs.cols.get(i).position] = D[row];	
			         }

			         //CeresMutexLock l(&cell_info->m);
			         Lock l = cell_info.m;
			         l.lock();
			         double marr[][] = new double[row_stride[0]][col_stride[0]];
			         for (index = 0, row = 0; row < row_stride[0]; row++) {
			        	 for (col = 0; col < col_stride[0]; col++, index++) {
			        		 marr[row][col] = cell_info.values[cell_info.values_index + index];
			        	 }
			         }
			         //MatrixRef m(cell_info->values, row_stride, col_stride);
			         for (index = 0; index < block_size; index++) {
			        	 marr[r[0]+index][c[0]+index] += (diag[index]*diag[index]);
			         }
			         for (index = 0, row = 0; row < row_stride[0]; row++) {
			        	 for (col = 0; col < col_stride[0]; col++, index++) {
			        		 cell_info.values[cell_info.values_index + index] = marr[row][col];
			        	 }
			         }
			         //m.block(r, c, block_size, block_size).diagonal()
			             //+= diag.array().square().matrix();
			       }
			     }
			 //#if defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
			     //);
			 //#endif // defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
			   } // if (D != null)

			 //#if !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))
			   //ThreadTokenProvider thread_token_provider(num_threads_);
			 //#endif // !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))

			 //#ifdef CERES_USE_OPENMP
			   // Eliminate y blocks one chunk at a time.  For each chunk, compute
			   // the entries of the normal equations and the gradient vector block
			   // corresponding to the y block and then apply Gaussian elimination
			   // to them. The matrix ete stores the normal matrix corresponding to
			   // the block being eliminated and array buffer_ contains the
			   // non-zero blocks in the row corresponding to this y block in the
			   // normal equations. This computation is done in
			   // ChunkDiagonalBlockAndGradient. UpdateRhs then applies gaussian
			   // elimination to the rhs of the normal equations, updating the rhs
			   // of the reduced linear system by modifying rhs blocks for all the
			   // z blocks that share a row block/residual term with the y
			   // block. EliminateRowOuterProduct does the corresponding operation
			   // for the lhs of the reduced linear system.
			 //#pragma omp parallel for num_threads(num_threads_) schedule(dynamic)
			 //#endif // CERES_USE_OPENMP

			 //#if !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))
			   for (i = 0; i < chunks_.size(); ++i) {
			     //const ScopedThreadToken scoped_thread_token(&thread_token_provider);
			     //const int thread_id = scoped_thread_token.token();
				   int thread_id = 0;
			 //#else
			     //ParallelFor(context_,
			                 //0,
			                 //int(chunks_.size()),
			                 //num_threads_,
			                 //[&](int thread_id, int i) {
			 //#endif // !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))

			     //double* buffer = buffer_.get() + thread_id * buffer_size_;
				 double[] buffer = buffer_;
			     Chunk chunk = chunks_.get(i);
			     int e_block_id = bs.rows.get(chunk.start).cells.get(0).block_id;
			     int e_block_size = bs.cols.get(e_block_id).size;

			     //VectorRef(buffer, buffer_size_).setZero();
			     for (row = 0; row < buffer_size_; row++) {
			    	 buffer[row] = 0.0;
			     }

			     //typename EigenTypes<kEBlockSize, kEBlockSize>::Matrix
			         //ete(e_block_size, e_block_size);
			     double etearr[][] = new double[e_block_size][e_block_size];

			     if (D != null) {
			       double diag[] = new double[e_block_size];
			       for (row = bs.cols.get(e_block_id).position; row < bs.cols.get(e_block_id).position + e_block_size; row++) {
			    	   diag[row-bs.cols.get(e_block_id).position] = D[row];
			       }
			       for (row = 0; row < e_block_size; row++) {
			    	   etearr[row][row] = (diag[row]*diag[row]);
			       }
			       //const typename EigenTypes<kEBlockSize>::ConstVectorRef
			           //diag(D + bs->cols[e_block_id].position, e_block_size);
			       //ete = diag.array().square().matrix().asDiagonal();
			     } else {
			       //ete.setZero();
			     }

			     //FixedArray<double, 8> g(e_block_size);
			    // typename EigenTypes<kEBlockSize>::VectorRef gref(g.get(), e_block_size);
			     //gref.setZero();
			     double g[] = new double[e_block_size];

			     // We are going to be computing
			     //
			     //   S += F'F - F'E(E'E)^{-1}E'F
			     //
			     // for each Chunk. The computation is broken down into a number of
			     // function calls as below.

			     // Compute the outer product of the e_blocks with themselves (ete
			     // = E'E). Compute the product of the e_blocks with the
			     // corresonding f_blocks (buffer = E'F), the gradient of the terms
			     // in this chunk (g) and add the outer product of the f_blocks to
			     // Schur complement (S += F'F).
			     ChunkDiagonalBlockAndGradient(
			         chunk, A, b, chunk.start, etearr, g, buffer, lhs);

			     // Normally one wouldn't compute the inverse explicitly, but
			     // e_block_size will typically be a small number like 3, in
			     // which case its much faster to compute the inverse once and
			     // use it to multiply other matrices/vectors instead of doing a
			     // Solve call over and over again.
			     double inverse_ete[][] = InvertPSDMatrix(assume_full_rank_ete_, etearr);

			     // For the current chunk compute and update the rhs of the reduced
			     // linear system.
			     //
			     //   rhs = F'b - F'E(E'E)^(-1) E'b

			     double inverse_ete_g[] = new double[e_block_size];
			     double inverse_ete_data[] = new double[e_block_size*e_block_size];
			     for (index = 0, row = 0; row < e_block_size; row++) {
			    	 for (col = 0; col < e_block_size; col++, index++) {
			    		 inverse_ete_data[index] = inverse_ete[row][col];
			    	 }
			     }
			     MatrixVectorMultiply(kEBlockSize, kEBlockSize, 0,
			         inverse_ete_data, 0,
			         e_block_size,
			         e_block_size,
			         g, 0,
			         inverse_ete_g, 0);

			     UpdateRhs(chunk, A, b, chunk.start, inverse_ete_g, rhs);

			     // S -= F'E(E'E)^{-1}E'F
			     ChunkOuterProduct(
			         thread_id, bs, inverse_ete, buffer, chunk.buffer_layout, lhs);
			   }
			 //#if defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
			   //);
			 //#endif // defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)

			   // For rows with no e_blocks, the schur complement update reduces to
			   // S += F'F.
			   NoEBlockRowsUpdate(A, b,  uneliminated_row_begins_, lhs, rhs);
			 }
			 
			 public void BackSubstitute(BlockSparseMatrix A,
			                double[] b,
			                double[] D,
			                double[] z,
			                double[] y) {
			   int r;
			   int col;
			   int index;
			   CompressedRowBlockStructure bs = A.block_structure();

			 //#ifdef CERES_USE_OPENMP
			 //#pragma omp parallel for num_threads(num_threads_) schedule(dynamic)
			 //#endif // CERES_USE_OPENMP

			 //#if !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))
			   for (int i = 0; i < chunks_.size(); ++i) {
			 //#else
			   //ParallelFor(context_, 0, int(chunks_.size()), num_threads_, [&](int i) {
			 //#endif // !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))

			     Chunk chunk = chunks_.get(i);
			     int e_block_id = bs.rows.get(chunk.start).cells.get(0).block_id;
			     int e_block_size = bs.cols.get(e_block_id).size;

			     //double* y_ptr = y +  bs->cols[e_block_id].position;
			     //typename EigenTypes<kEBlockSize>::VectorRef y_block(y_ptr, e_block_size);
			     double y_block[] = new double[e_block_size];
			     for (r = bs.cols.get(e_block_id).position; r < bs.cols.get(e_block_id).position + e_block_size; r++) {
			    	 y_block[r-bs.cols.get(e_block_id).position] = y[r];
			     }

			     //typename EigenTypes<kEBlockSize, kEBlockSize>::Matrix
			         //ete(e_block_size, e_block_size);
			     double etearr[][] = new double[e_block_size][e_block_size];
			     if (D != null) {
			       double diag[] = new double[e_block_size];
			       for (r = bs.cols.get(e_block_id).position; r < bs.cols.get(e_block_id).position + e_block_size; r++) {
				    	 diag[r-bs.cols.get(e_block_id).position] = D[r];
				    }
			        for (r = 0; r < e_block_size; r++) {
			        	etearr[r][r] = (diag[r]*diag[r]);
			        }
			       //const typename EigenTypes<kEBlockSize>::ConstVectorRef
			           //diag(D + bs->cols[e_block_id].position, e_block_size);
			       //ete = diag.array().square().matrix().asDiagonal();
			     } //else {
			       //ete.setZero();
			     //}

			     double[] values = A.values();
			     for (int j = 0; j < chunk.size; ++j) {
			       CompressedList row = bs.rows.get(chunk.start + j);
			       Cell e_cell = row.cells.get(0);
			       if (e_block_id != e_cell.block_id) {
			    	   System.err.println("e_block_id != e_cell.block_id in SchurEliminator BackSubstitute");
			    	   return;
			       }

			       double sj[] = new double[row.block.size];
			       for (r = bs.rows.get(chunk.start+j).block.position; r < bs.rows.get(chunk.start+j).block.position + row.block.size; r++) {
			    	   sj[r-bs.rows.get(chunk.start+j).block.position] = b[r];
			       }

			       //typename EigenTypes<kRowBlockSize>::VectorRef(sj.get(), row.block.size) =
			           //typename EigenTypes<kRowBlockSize>::ConstVectorRef
			           //(b + bs->rows[chunk.start + j].block.position, row.block.size);

			       for (int c = 1; c < row.cells.size(); ++c) {
			         int f_block_id = row.cells.get(c).block_id;
			         int f_block_size = bs.cols.get(f_block_id).size;
			         int r_block = f_block_id - num_eliminate_blocks_;

			         MatrixVectorMultiply(kRowBlockSize, kFBlockSize, -1,
			             values, row.cells.get(c).position, row.block.size, f_block_size,
			             z, lhs_row_layout_.get(r_block),
			             sj, 0);
			       }

			       MatrixTransposeVectorMultiply(kRowBlockSize, kEBlockSize, 1,
			           values, e_cell.position, row.block.size, e_block_size,
			           sj, 0,
			           y_block, 0);

			       double etedata[] = new double[e_block_size * e_block_size];
			       for (index = 0, r = 0; r < e_block_size; r++) {
			    	   for (col = 0; col < e_block_size; col++, index++) {
			    		   etedata[index] = etearr[r][col];
			    	   }
			       }
			       MatrixTransposeMatrixMultiply
			           (kRowBlockSize, kEBlockSize, kRowBlockSize, kEBlockSize, 1,
			               values, e_cell.position, row.block.size, e_block_size,
			               values, e_cell.position, row.block.size, e_block_size,
			               etedata, 0, 0, 0, e_block_size, e_block_size);
			       for (index = 0, r = 0; r < e_block_size; r++) {
			    	   for (col = 0; col < e_block_size; col++, index++) {
			    		   etearr[r][col] = etedata[index];
			    	   }
			       } 
			     }
			     
			     double eteinv[][] = InvertPSDMatrix(assume_full_rank_ete_, etearr);
                 double yres[] = new double[e_block_size];
                 for (r = 0; r < e_block_size; r++) {
                	 for (col = 0; col < e_block_size; col++) {
                		 yres[r] += eteinv[r][col] * y_block[col];
                	 }
                 }
			     //y_block = InvertPSDMatrix<kEBlockSize>(assume_full_rank_ete_, etearr)
			         //* y_block;
			     
			     for (r = bs.cols.get(e_block_id).position; r < bs.cols.get(e_block_id).position + e_block_size; r++) {
			    	 y[r] = yres[r-bs.cols.get(e_block_id).position];
			     }
			   }
			 //#if defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
			   //);
			 //#endif // defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
			 }
			 
			// For rows with no e_blocks, the schur complement update reduces to S
			// += F'F. This function iterates over the rows of A with no e_block,
			// and calls NoEBlockRowOuterProduct on each row.
			private void NoEBlockRowsUpdate(BlockSparseMatrix A,
			                   double[] b,
			                   int row_block_counter,
			                   BlockRandomAccessMatrix lhs,
			                   double[] rhs) {
			  CompressedRowBlockStructure bs = A.block_structure();
			  double[] values = A.values();
			  for (; row_block_counter < bs.rows.size(); ++row_block_counter) {
			    CompressedList row = bs.rows.get(row_block_counter);
			    for (int c = 0; c < row.cells.size(); ++c) {
			      int block_id = row.cells.get(c).block_id;
			      int block_size = bs.cols.get(block_id).size;
			      int block = block_id - num_eliminate_blocks_;
			      MatrixTransposeVectorMultiply(DYNAMIC, DYNAMIC, 1,
			          values, row.cells.get(c).position, row.block.size, block_size,
			          b, row.block.position,
			          rhs, lhs_row_layout_.get(block));
			    }
			    NoEBlockRowOuterProduct(A, row_block_counter, lhs);
			  }
			}
			
			private void NoEBlockRowOuterProduct(BlockSparseMatrix A,
                    int row_block_index,
                    BlockRandomAccessMatrix lhs) {
			CompressedRowBlockStructure bs = A.block_structure();
			CompressedList row = bs.rows.get(row_block_index);
			double[] values = A.values();
			for (int i = 0; i < row.cells.size(); ++i) {
			    int block1 = row.cells.get(i).block_id - num_eliminate_blocks_;
			    if (block1 < 0) {
			    	System.err.println("block1 < 0 in SchurEliminator NoEBlockRowOuterProduct");
			    	return;
			    }
			
			int block1_size = bs.cols.get(row.cells.get(i).block_id).size;
			int r[] = new int[1];
			int c[] = new int[1];
			int row_stride[] = new int[1];
			int col_stride[] = new int[1];
			CellInfo cell_info = lhs.GetCell(block1, block1,
			                                   r, c,
			                                   row_stride, col_stride);
			if (cell_info != null) {
			  Lock l = cell_info.m;
			  l.lock();
			  // This multiply currently ignores the fact that this is a
			  // symmetric outer product.
			  MatrixTransposeMatrixMultiply
			      (DYNAMIC, DYNAMIC, DYNAMIC, DYNAMIC, 1,
			          values, row.cells.get(i).position, row.block.size, block1_size,
			          values, row.cells.get(i).position, row.block.size, block1_size,
			          cell_info.values, cell_info.values_index, r[0], c[0], row_stride[0], col_stride[0]);
			}
			
			for (int j = i + 1; j < row.cells.size(); ++j) {
			  int block2 = row.cells.get(j).block_id - num_eliminate_blocks_;
			  if (block2 < 0) {
				  System.err.println("block2 < 0 in SchurEliminator NoEBlockRowOuterProduct");
				  return;
			  }
			  if (block1 >= block2) {
				  System.err.println("block1 >= block2 in SchurEliminator NoEBlockRowOuterProduct");
				  return;
			  }
			  cell_info = lhs.GetCell(block1, block2,
			                                     r, c,
			                                     row_stride, col_stride);
			  if (cell_info != null) {
			    int block2_size = bs.cols.get(row.cells.get(j).block_id).size;
			    Lock l = cell_info.m;
			    l.lock();
			    MatrixTransposeMatrixMultiply
			        (DYNAMIC, DYNAMIC, DYNAMIC, DYNAMIC, 1,
			            values, row.cells.get(i).position, row.block.size, block1_size,
			            values, row.cells.get(j).position, row.block.size, block2_size,
			            cell_info.values, cell_info.values_index, r[0], c[0], row_stride[0], col_stride[0]);
			  }
			}
			}
			}
			 
			// Compute the outer product F'E(E'E)^{-1}E'F and subtract it from the
			// Schur complement matrix, i.e
			//
			//  S -= F'E(E'E)^{-1}E'F.
			private void ChunkOuterProduct(int thread_id,
			                  CompressedRowBlockStructure bs,
			                  double[][] inverse_ete,
			                  double[] buffer,
			                  HashMap<Integer, Integer> buffer_layout,
			                  BlockRandomAccessMatrix lhs) {
				int row;
				int col;
				int index;
				int i;
			  // This is the most computationally expensive part of this
			  // code. Profiling experiments reveal that the bottleneck is not the
			  // computation of the right-hand matrix product, but memory
			  // references to the left hand side.
			  int e_block_size = inverse_ete.length;
			  int colSize = inverse_ete[0].length;
			  double inverse_ete_data[] = new double[e_block_size * colSize];
			  for (index = 0, row = 0; row < e_block_size; row++) {
				  for (col = 0; col < colSize; col++, index++) {
					  inverse_ete_data[index] = inverse_ete[row][col];
				  }
			  }
			  Iterator<Entry<Integer, Integer>> it1 = buffer_layout.entrySet().iterator();
		        

			  //double* b1_transpose_inverse_ete =
			      //chunk_outer_product_buffer_.get() + thread_id * buffer_size_;
			  double[] b1_transpose_inverse_ete = chunk_outer_product_buffer_;

			  // S(i,j) -= bi' * ete^{-1} b_j
			  int numit1Next = 0;
			  while (it1.hasNext()) {
				Map.Entry<Integer, Integer> pair = (Map.Entry<Integer, Integer>) it1.next();
				numit1Next++;
				int key = pair.getKey();
				int value = pair.getValue();
			    int block1 = key - num_eliminate_blocks_;
			    int block1_size = bs.cols.get(key).size;
			    MatrixTransposeMatrixMultiply
			        (kEBlockSize, kFBlockSize, kEBlockSize, kEBlockSize, 0,
			        buffer, value, e_block_size, block1_size,
			        inverse_ete_data, 0, e_block_size, e_block_size,
			        b1_transpose_inverse_ete, 0, 0, 0, block1_size, e_block_size);

			    Iterator<Entry<Integer, Integer>> it2 = buffer_layout.entrySet().iterator();
			    for (i = 0; i < numit1Next-1; i++) {
			    	it2.next();
			    }
			    while (it2.hasNext()) {
			    	Map.Entry<Integer, Integer> pair2 = (Map.Entry<Integer, Integer>) it2.next();
					int key2 = pair2.getKey();
					int value2 = pair2.getValue();
			        int block2 = key2 - num_eliminate_blocks_;

			        int r[] = new int[1];
			        int c[] = new int[1];
			        int row_stride[] = new int[1];
			        int col_stride[] = new int[1];
			        CellInfo cell_info = lhs.GetCell(block1, block2,
			                                         r, c,
			                                         row_stride, col_stride);
			      if (cell_info != null) {
			        int block2_size = bs.cols.get(key2).size;
			        Lock l = cell_info.m;
			        l.lock();
			        MatrixMatrixMultiply
			            (kFBlockSize, kEBlockSize, kEBlockSize, kFBlockSize, -1,
			                b1_transpose_inverse_ete, 0, block1_size, e_block_size,
			                buffer, value2, e_block_size, block2_size,
			                cell_info.values, cell_info.values_index, r[0], c[0], row_stride[0], col_stride[0]);
			      }
			    }
			  }
			}

			  
			// Update the rhs of the reduced linear system. Compute
			 //
			 //   F'b - F'E(E'E)^(-1) E'b
			 private void UpdateRhs(Chunk chunk,
			           BlockSparseMatrix A,
			           double[] b,
			           int row_block_counter,
			           double[] inverse_ete_g,
			           double[] rhs) {
			   int i;
			   CompressedRowBlockStructure bs = A.block_structure();
			   int e_block_id = bs.rows.get(chunk.start).cells.get(0).block_id;
			   int e_block_size = bs.cols.get(e_block_id).size;

			   int b_pos = bs.rows.get(row_block_counter).block.position;
			   double[] values = A.values();
			   for (int j = 0; j < chunk.size; ++j) {
			     CompressedList row = bs.rows.get(row_block_counter + j);
			     Cell e_cell = row.cells.get(0);

			     double sj[] = new double[row.block.size];
			     for (i = b_pos; i < b_pos + row.block.size; i++) {
			    	 sj[i-b_pos] = b[i];
			     }
			     //typename EigenTypes<kRowBlockSize>::Vector sj =
			         //typename EigenTypes<kRowBlockSize>::ConstVectorRef
			         //(b + b_pos, row.block.size);

			     MatrixVectorMultiply(kRowBlockSize, kEBlockSize, -1,
			         values, e_cell.position, row.block.size, e_block_size,
			         inverse_ete_g, 0, sj, 0);

			     for (int c = 1; c < row.cells.size(); ++c) {
			       int block_id = row.cells.get(c).block_id;
			       int block_size = bs.cols.get(block_id).size;
			       int block = block_id - num_eliminate_blocks_;
			       Lock l = rhs_locks_.get(block);
			       l.lock();
			       MatrixTransposeVectorMultiply(kRowBlockSize, kFBlockSize, 1,
			           values, row.cells.get(c).position,
			           row.block.size, block_size,
			           sj, 0,rhs, lhs_row_layout_.get(block));
			     }
			     b_pos += row.block.size;
			   }
			 }

			 
			 private void ChunkDiagonalBlockAndGradient(
			     Chunk chunk,
			     BlockSparseMatrix A,
			     double[] b,
			     int row_block_counter,
			     //typename EigenTypes<kEBlockSize, kEBlockSize>::Matrix* ete,
			     double etearr[][],
			     double[] g,
			     double[] buffer,
			     BlockRandomAccessMatrix lhs) {
				 int r;
				 int col;
				 int index;
			   CompressedRowBlockStructure bs = A.block_structure();

			   int b_pos = bs.rows.get(row_block_counter).block.position;
			   int e_block_size = etearr.length;

			   // Iterate over the rows in this chunk, for each row, compute the
			   // contribution of its F blocks to the Schur complement, the
			   // contribution of its E block to the matrix EE' (ete), and the
			   // corresponding block in the gradient vector.
			   double[] values = A.values();
			   for (int j = 0; j < chunk.size; ++j) {
			     CompressedList row = bs.rows.get(row_block_counter + j);

			     if (row.cells.size() > 1) {
			       EBlockRowOuterProduct(A, row_block_counter + j, lhs);
			     }

			     // Extract the e_block, ETE += E_i' E_i
			     Cell e_cell = row.cells.get(0);
			     double etedata[] = new double[e_block_size * e_block_size];
			     for (index = 0, r = 0; r < e_block_size; r++) {
			    	 for (col = 0; col < e_block_size; col++, index++) {
			    	     etedata[index] = etearr[r][col];
			    	 }
			     }
			     
			     MatrixTransposeMatrixMultiply
			         (kRowBlockSize, kEBlockSize, kRowBlockSize, kEBlockSize, 1,
			             values, e_cell.position, row.block.size, e_block_size,
			             values, e_cell.position, row.block.size, e_block_size,
			             etedata, 0, 0, 0, e_block_size, e_block_size);
			     for (index = 0, r = 0; r < e_block_size; r++) {
			    	 for (col = 0; col < e_block_size; col++, index++) {
			    	     etearr[r][col] = etedata[index];
			    	 }
			     }

			     // g += E_i' b_i
			     MatrixTransposeVectorMultiply(kRowBlockSize, kEBlockSize, 1,
			         values, e_cell.position, row.block.size, e_block_size,
			         b, b_pos,
			         g, 0);


			     // buffer = E'F. This computation is done by iterating over the
			     // f_blocks for each row in the chunk.
			     for (int c = 1; c < row.cells.size(); ++c) {
			       int f_block_id = row.cells.get(c).block_id;
			       int f_block_size = bs.cols.get(f_block_id).size;
			       int buffer_index = 0;
			       if (!chunk.buffer_layout.containsKey(f_block_id)) {
			    	   System.err.println("Map key not found in SchurEliminator ChunkDiagonalBlockAndGradient");
			    	   return;
			       }
			       else {
			    	   buffer_index = chunk.buffer_layout.get(f_block_id);   
			       }
			       MatrixTransposeMatrixMultiply
			           (kRowBlockSize, kEBlockSize, kRowBlockSize, kFBlockSize, 1,
			           values, e_cell.position, row.block.size, e_block_size,
			           values, row.cells.get(c).position, row.block.size, f_block_size,
			           buffer, buffer_index, 0, 0, e_block_size, f_block_size);
			     }
			     b_pos += row.block.size;
			   }
			 }
			 
			// For a row with an e_block, compute the contribition S += F'F. This
			// function has the same structure as NoEBlockRowOuterProduct, except
			// that this function uses the template parameters.
			private void EBlockRowOuterProduct(BlockSparseMatrix A,
			                      int row_block_index,
			                      BlockRandomAccessMatrix lhs) {
			  CompressedRowBlockStructure bs = A.block_structure();
			  CompressedList row = bs.rows.get(row_block_index);
			  double[] values = A.values();
			  for (int i = 1; i < row.cells.size(); ++i) {
			    int block1 = row.cells.get(i).block_id - num_eliminate_blocks_;
			    if (block1 < 0) {
			    	System.err.println("block1 < 0 in SchurEliminator EBlockRowOuterProduct");
			    	return;
			    }

			    int block1_size = bs.cols.get(row.cells.get(i).block_id).size;
			    int r[] = new int[1];
			    int c[] = new int[1];
			    int row_stride[] = new int[1];
			    int col_stride[] = new int[1];
			    CellInfo cell_info = lhs.GetCell(block1, block1,
			                                       r, c,
			                                       row_stride, col_stride);
			    if (cell_info != null) {
			      Lock l = cell_info.m;
			      l.lock();
			      // block += b1.transpose() * b1;
			      MatrixTransposeMatrixMultiply
			          (kRowBlockSize, kFBlockSize, kRowBlockSize, kFBlockSize, 1,
			          values, row.cells.get(i).position, row.block.size, block1_size,
			          values, row.cells.get(i).position, row.block.size, block1_size,
			          cell_info.values, cell_info.values_index, r[0], c[0], row_stride[0], col_stride[0]);
			    }

			    for (int j = i + 1; j < row.cells.size(); ++j) {
			      int block2 = row.cells.get(j).block_id - num_eliminate_blocks_;
			      if (block2 < 0) {
			    	  System.err.println("block2 < 0 in SchurEliminator EBlockRowOuterProduct");
			    	  return;
			      }
			      if (block1 >= block2) {
			    	  System.err.println("block1 >= block2 in SchurEliminator EBlockRowOuterProduct");
			    	  return;
			      }
			      int block2_size = bs.cols.get(row.cells.get(j).block_id).size;
			      cell_info = lhs.GetCell(block1, block2,
			                                         r, c,
			                                         row_stride, col_stride);
			      if (cell_info != null) {
			        // block += b1.transpose() * b2;
			        Lock l = cell_info.m;
			        l.lock();
			        MatrixTransposeMatrixMultiply
			            (kRowBlockSize, kFBlockSize, kRowBlockSize, kFBlockSize, 1,
			                values, row.cells.get(i).position, row.block.size, block1_size,
			                values, row.cells.get(j).position, row.block.size, block2_size,
			                cell_info.values, cell_info.values_index, r[0], c[0], row_stride[0], col_stride[0]);
			      }
			    }
			  }
			}
		} // class SchurEliminator
		
		public double[][] InvertPSDMatrix(boolean assume_full_rank, double arr[][]) {
			int row;
			int col;
			int i,j;
			int m = arr.length;
			int n = arr[0].length;
			double arrin[][] = new double[m][n];
			for (row = 0; row < m; row++) {
				for (col = 0; col < n; col++) {
					arrin[row][col] = arr[row][col];
				}
			}
			// For a symmetric positive semi-definite matrix
		     // If full rank invert with Cholesky factorization
		     if (assume_full_rank) {
	                int info[] = new int[1];
		     // Otherwise invert with singular value decomposition with tolerance = epsilon * number of matrix rows * singularValues[0]
	                le.dpotrf('U',m,arrin,m,info); // cholesky decomposition
	                if (info[0] < 0) {
	                	System.err.println("In le.dpotrf argument " + (-info[0]) + " had an illegal value");
	                	Preferences.debug("In le.dpotrf argument " + (-info[0]) + " had an illegal value\n", Preferences.DEBUG_ALGORITHM);
	                	return null;
	                }
	                if (info[0] > 0) {
	                	System.err.println("in le.dpotrf the leading minor of order " + info[0] + " is not positive definite, and the factorization could not be completed");
	                	Preferences.debug("in le.dpotrf the leading minor of order " + info[0] + " is not positive definite, and the factorization could not be completed\n",
	                			Preferences.DEBUG_ALGORITHM);
	                	return null;
	                }
	                le.dpotri('U',m,arrin,m,info);
	                if (info[0] < 0) {
	                	System.err.println("In le.dpotri argument " + (-info[0]) + " had an illegal value");
	                	Preferences.debug("In le.dpotri argument " + (-info[0]) + " had an illegal value\n", Preferences.DEBUG_ALGORITHM);
	                	return null;
	                }
	                if (info[0] > 0) {
	                	System.err.println("in le.dpotri element ["+(info[0]-1)+"]["+(info[0]-1)+"] of the factor U is zero,and the inverse could not be computed.");
	                	Preferences.debug("in le.dpotri  element ["+(info[0]-1)+"]["+(info[0]-1)+"] of the factor U is zero,and the inverse could not be computed.\n",
	                			Preferences.DEBUG_ALGORITHM);
	                	return null;
	                }
	                for (row = 0; row < n; row++) {
	                	for (col = 0; col < row; col++) {
	                	    arrin[row][col] = arrin[col][row];
	                	}
	                }
		    	    return arrin;
		     }
		     else {
		    	 double s[] = new double[Math.min(m,n)];
		    	 int ldu = m;
		    	 double U[][] = new double[ldu][Math.min(m,n)];
		    	 int ldvt = Math.min(m,n);
		    	 double VT[][] = new double[ldvt][n];
		    	 double work[] = new double[1];
		    	 int lwork = -1;
		    	 int info[] = new int[1];
		         svd.dgesvd('S','S',m, n, arrin, m, s, U, ldu, VT, ldvt, work, lwork, info);
		         lwork = (int)work[0];
		         work = new double[lwork];
		         svd.dgesvd('S','S',m, n, arrin, m, s, U, ldu, VT, ldvt, work, lwork, info);
		         if (info[0] < 0) {
	                	System.err.println("In svd.dgesvd argument " + (-info[0]) + " had an illegal value");
	                	Preferences.debug("In svd.dgesvd argument " + (-info[0]) + " had an illegal value\n", Preferences.DEBUG_ALGORITHM);
	                	return null;
	             }
		         if (info[0] > 0) {
	                	System.err.println("In svd.dgesvd dbdsqr did not converge.");
	                	Preferences.debug("In svd.dgesvd dbdsqr did not converge.\n",
	                			Preferences.DEBUG_ALGORITHM);
	                	return null;
	             }
		         double tolerance = epsilon * m * s[0];
		         double ss;
		         for (i = 0; i < s.length; i++) {
		        	 if(s[i] > tolerance) {
		        		 ss=1.0/s[i];	 
		        	 }
		        	 else {
		        			ss= 0.0;
		        	 }
		        	 for (j = 0; j < Math.min(m,n); j++) {
		        		 U[j][i] = ss * U[j][i];
		        	 } 
		         }
		         double alpha = 1.0;
		         double beta = 0.0;
		         double invarr[][] = new double[m][m];
		         ge.dgemm('T','T',m, m, m, alpha, VT, ldvt, U, ldu, beta, invarr, m);
		         return invarr;
		     }
		}

		
		public SchurEliminatorBase createSchurEliminatorBase(LinearSolverOptions options) {
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 2) &&
			     (options.f_block_size[0] == 2)) {
			   return new SchurEliminator(2, 2, 2, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 2) &&
			     (options.f_block_size[0] == 3)) {
			   return new SchurEliminator(2, 2, 3, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 2) &&
			     (options.f_block_size[0] == 4)) {
			   return new SchurEliminator(2, 2, 4, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 2)) {
			   return new SchurEliminator(2, 2, DYNAMIC, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 3) &&
			     (options.f_block_size[0] == 3)) {
			   return new SchurEliminator(2, 3, 3, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 3) &&
			     (options.f_block_size[0] == 4)) {
			   return new SchurEliminator(2, 3, 4, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 3) &&
			     (options.f_block_size[0] == 6)) {
			   return new SchurEliminator(2, 3, 6, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 3) &&
			     (options.f_block_size[0] == 9)) {
			   return new SchurEliminator(2, 3, 9, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 3)) {
			   return new SchurEliminator(2, 3, DYNAMIC, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 3)) {
			   return new SchurEliminator(2, 4, 3, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 4)) {
			   return new SchurEliminator(2, 4, 4, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 6)) {
			   return new SchurEliminator(2, 4, 6, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 8)) {
			   return new SchurEliminator(2, 4, 8, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 9)) {
			   return new SchurEliminator(2, 4, 9, options);
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 4)) {
			   return new SchurEliminator(2, 4, DYNAMIC, options);
			 }
			 if (options.row_block_size[0] == 2){
			   return new SchurEliminator(2, DYNAMIC, DYNAMIC, options);
			 }
			 if ((options.row_block_size[0] == 4) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 2)) {
			   return new SchurEliminator(4, 4, 2, options);
			 }
			 if ((options.row_block_size[0] == 4) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 3)) {
			   return new SchurEliminator(4, 4, 3, options);
			 }
			 if ((options.row_block_size[0] == 4) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 4)) {
			   return new SchurEliminator(4, 4, 4, options);
			 }
			 if ((options.row_block_size[0] == 4) &&
			     (options.e_block_size[0] == 4)) {
			   return new SchurEliminator(4, 4, DYNAMIC, options);
			 }
			 if (1 <= MAX_LOG_LEVEL) {
			  Preferences.debug("Template specializations not found for "
			          + options.row_block_size[0] + ","
			          + options.e_block_size[0] + ","
			          + options.f_block_size[0] + "\n", Preferences.DEBUG_ALGORITHM);
			 }
			  return new SchurEliminator(DYNAMIC, DYNAMIC, DYNAMIC, options);
			}

	
	class SchurJacobiPreconditioner extends TypedPreconditioner<BlockSparseMatrix> {
		private PreconditionerOptions options_;
		private SchurEliminatorBase eliminator_;
		  // Preconditioner matrix.
		private BlockRandomAccessDiagonalMatrix m_;
		  // Initialize the symbolic structure of the preconditioner. bs is
		  // the block structure of the linear system to be solved. It is used
		  // to determine the sparsity structure of the preconditioner matrix.
		  //
		  // It has the same structural requirement as other Schur complement
		  // based solvers. Please see schur_eliminator.h for more details.
		  public SchurJacobiPreconditioner(CompressedRowBlockStructure bs,
		                            PreconditionerOptions options) {
			  super();
			  options_ = options;
			  if (options_.elimination_groups.size() <= 1) {
				  System.err.println("options_.elimination_groups.size() <= 1 in public SchurJacobiPreconditioner");
				  return;
			  }
			  if (options_.elimination_groups.get(0) <= 0) {
			      System.err.println("options_.elimination_groups.get(0) <= 0 in publicSchurJacobiPreConditioner");
			      return;
			  }
			  int num_blocks = bs.cols.size() - options_.elimination_groups.get(0);
			  if (num_blocks <= 0) {
			      System.err.println("Jacobian should have atleast 1 f_block for SCHUR_JACOBI preconditioner.");
			      return;
			  }
			  if (options_.context == null) {
				  System.err.println("options_.context == null in public SchurJacobiPreconditioner");
				  return;
			  }

			  Vector<Integer> blocks = new Vector<Integer>(num_blocks);
			  for (int i = 0; i < num_blocks; ++i) {
			    blocks.add(bs.cols.get(i + options_.elimination_groups.get(0)).size);
			  }

			  m_ = new BlockRandomAccessDiagonalMatrix(blocks);
			  InitEliminator(bs);

		  }

		  // Preconditioner interface.
		  public void RightMultiply(double[] x, double[] y) {
			  m_.RightMultiply(x, y);
		  }
		  
		  public int num_rows() {
			  return m_.num_rows();
		  }

		  private void InitEliminator(CompressedRowBlockStructure bs) {
			  LinearSolverOptions eliminator_options = new LinearSolverOptions();
			  eliminator_options.elimination_groups = options_.elimination_groups;
			  eliminator_options.num_threads = options_.num_threads;
			  eliminator_options.e_block_size[0] = options_.e_block_size;
			  eliminator_options.f_block_size[0] = options_.f_block_size;
			  eliminator_options.row_block_size[0] = options_.row_block_size;
			  eliminator_options.context = options_.context;
			  eliminator_ = createSchurEliminatorBase(eliminator_options);
			  boolean kFullRankETE = true;
			  eliminator_.Init(
			      eliminator_options.elimination_groups.get(0), kFullRankETE, bs);  
		  }
		  
		  public boolean UpdateImpl(BlockSparseMatrix A, double[] D) {
			  int i;
			  int num_rows = m_.num_rows();
			  if (num_rows <= 0) {
				  System.err.println("num_rows <= 0 in SchurJacobiPreconditioner UpdateImpl");
				  return false;
			  }

			  // We need a dummy rhs vector and a dummy b vector since the Schur
			  // eliminator combines the computation of the reduced camera matrix
			  // with the computation of the right hand side of that linear
			  // system.
			  //
			  // TODO(sameeragarwal): Perhaps its worth refactoring the
			  // SchurEliminator::Eliminate function to allow NULL for the rhs. As
			  // of now it does not seem to be worth the effort.
			  double rhs[] = new double[m_.num_rows()];
			  double b[] = new double[A.num_rows()]; 
			  // Compute a subset of the entries of the Schur complement.
			  eliminator_.Eliminate(A, b, D, m_, rhs);
			  m_.Invert();
			  return true;

		  }

		 
		};
	
	// Wrap a SparseMatrix object as a preconditioner.
	class SparseMatrixPreconditionerWrapper extends TypedPreconditioner<SparseMatrix> {
	 private SparseMatrix matrix_;
	
	  // Wrapper does NOT take ownership of the matrix pointer.
	  public SparseMatrixPreconditionerWrapper(SparseMatrix matrix) {
		  super();
		  matrix_ = matrix;
		  if (matrix == null) {
			  System.err.println("matrix == null in SparseMatrixPrconditionerWrapper");
		  }
	  }

	  // Preconditioner interface
	  public void RightMultiply(double[] x, double[] y) {
		  matrix_.RightMultiply(x, y);
	  }
	  
	  public int num_rows() {
		  return matrix_.num_rows();
	  }

	 public boolean UpdateImpl(SparseMatrix A, double[] D) {
		 return true;
	 }
	};

	abstract class TypedPreconditioner<MatrixType> extends Preconditioner {
	 public TypedPreconditioner() {
		 super();
	 }
	 public boolean Update(LinearOperator A, double D[]) {
	    return UpdateImpl((MatrixType)A, D);
	  }

	  public abstract boolean UpdateImpl(MatrixType A, double D[]);
	};
	
	 class PreconditionerOptions {
		 
		 public PreconditionerType type;
		    public VisibilityClusteringType visibility_clustering_type;
		    public SparseLinearAlgebraLibraryType sparse_linear_algebra_library_type;

		    // When using the subset preconditioner, all row blocks starting
		    // from this row block are used to construct the preconditioner.
		    //
		    // i.e., the Jacobian matrix A is horizonatally partitioned as
		    //
		    // A = [P]
		    //     [Q]
		    //
		    // where P has subset_preconditioner_start_row_block row blocks,
		    // and the preconditioner is the inverse of the matrix Q'Q.
		    public int subset_preconditioner_start_row_block;

		    // See solver.h for information about these flags.
		    public boolean use_postordering;

		    // If possible, how many threads the preconditioner can use.
		    public int num_threads;

		    // Hints about the order in which the parameter blocks should be
		    // eliminated by the linear solver.
		    //
		    // For example if elimination_groups is a vector of size k, then
		    // the linear solver is informed that it should eliminate the
		    // parameter blocks 0 ... elimination_groups[0] - 1 first, and
		    // then elimination_groups[0] ... elimination_groups[1] - 1 and so
		    // on. Within each elimination group, the linear solver is free to
		    // choose how the parameter blocks are ordered. Different linear
		    // solvers have differing requirements on elimination_groups.
		    //
		    // The most common use is for Schur type solvers, where there
		    // should be at least two elimination groups and the first
		    // elimination group must form an independent set in the normal
		    // equations. The first elimination group corresponds to the
		    // num_eliminate_blocks in the Schur type solvers.
		    public Vector<Integer> elimination_groups;

		    // If the block sizes in a BlockSparseMatrix are fixed, then in
		    // some cases the Schur complement based solvers can detect and
		    // specialize on them.
		    //
		    // It is expected that these parameters are set programmatically
		    // rather than manually.
		    //
		    // Please see schur_complement_solver.h and schur_eliminator.h for
		    // more details.
		    public int row_block_size;
		    public int e_block_size;
		    public int f_block_size;

		    public Context context;
	    public PreconditionerOptions() {
	        type = PreconditionerType.JACOBI;
	        visibility_clustering_type = VisibilityClusteringType.CANONICAL_VIEWS;
	        //sparse_linear_algebra_library_type(SUITE_SPARSE),
	        sparse_linear_algebra_library_type = SparseLinearAlgebraLibraryType.EIGEN_SPARSE;
	        subset_preconditioner_start_row_block = -1;
	        use_postordering = false;
	        num_threads = 1;
	        row_block_size = DYNAMIC;
	        e_block_size = DYNAMIC;
	        f_block_size = DYNAMIC;
	        context = new Context(); 
	    }

	    
		 
	 }
	 
	 abstract class Preconditioner extends LinearOperator {
		 public PreconditionerOptions options;
		  class Options {
			    
		  } // class Options
		  
		  public Preconditioner() {
			  super();
			  PreconditionerOptions options = new PreconditionerOptions();
		  }

		  // If the optimization problem is such that there are no remaining
		  // e-blocks, ITERATIVE_SCHUR with a Schur type preconditioner cannot
		  // be used. This function returns JACOBI if a preconditioner for
		  // ITERATIVE_SCHUR is used. The input preconditioner_type is
		  // returned otherwise.
		  /*static PreconditionerType PreconditionerForZeroEBlocks(
		      PreconditionerType preconditioner_type);*/


		  // Update the numerical value of the preconditioner for the linear
		  // system:
		  //
		  //  |   A   | x = |b|
		  //  |diag(D)|     |0|
		  //
		  // for some vector b. It is important that the matrix A have the
		  // same block structure as the one used to construct this object.
		  //
		  // D can be NULL, in which case its interpreted as a diagonal matrix
		  // of size zero.
		  public abstract boolean Update(LinearOperator A, double[] D);

		  // LinearOperator interface. Since the operator is symmetric,
		  // LeftMultiply and num_cols are just calls to RightMultiply and
		  // num_rows respectively. Update() must be called before
		  // RightMultiply can be called.
		  public abstract void RightMultiply(double[] x, double[] y);
		  public void LeftMultiply(double[] x, double[] y) {
		    RightMultiply(x, y);
		  }

		  public abstract int num_rows();
		  public int num_cols() {
		    return num_rows();
		  }
		} // abstract class Preconditioner 
	 
	 //template <int kRowBlockSize = Eigen::Dynamic,
	          //int kEBlockSize = Eigen::Dynamic,
	          //int kFBlockSize = Eigen::Dynamic >
	class PartitionedMatrixView {
		private int kRowBlockSize;
		private int kEBlockSize;
		private int kFBlockSize;
		private BlockSparseMatrix matrix_;
		private int num_row_blocks_e_;
		private int num_col_blocks_e_;
		private int num_col_blocks_f_;
		private int num_cols_e_;
		private int num_cols_f_;
	  // matrix = [E F], where the matrix E contains the first
	  // num_col_blocks_a column blocks.
	  public PartitionedMatrixView(int kRowBlockSize, int kEBlockSize, int kFBlockSize,
			  BlockSparseMatrix matrix, int num_col_blocks_e) {
		   this.kRowBlockSize = kRowBlockSize;
		   this.kEBlockSize = kEBlockSize;
		   this.kFBlockSize = kFBlockSize;
		   matrix_ = matrix;
		   num_col_blocks_e_ = num_col_blocks_e;
		   CompressedRowBlockStructure bs = matrix_.block_structure();
		   if (bs == null) {
			   System.err.println("bs == null in public PartitionedMatrixView");
			   return;
		   }
		   num_col_blocks_f_ = bs.cols.size() - num_col_blocks_e_;
		   
		   // Compute the number of row blocks in E. The number of row blocks
		   // in E maybe less than the number of row blocks in the input matrix
		   // as some of the row blocks at the bottom may not have any
		   // e_blocks. For a definition of what an e_block is, please see
		   // explicit_schur_complement_solver.h
		   num_row_blocks_e_ = 0;
		   for (int r = 0; r < bs.rows.size(); ++r) {
		     Vector<Cell> cells = bs.rows.get(r).cells;
		     if (cells.get(0).block_id < num_col_blocks_e_) {
		       ++num_row_blocks_e_;
		     }
		   }

		   // Compute the number of columns in E and F.
		   num_cols_e_ = 0;
		   num_cols_f_ = 0;

		   for (int c = 0; c < bs.cols.size(); ++c) {
		     Block block = bs.cols.get(c);
		     if (c < num_col_blocks_e_) {
		       num_cols_e_ += block.size;
		     } else {
		       num_cols_f_ += block.size;
		     }
		   }

		  if ((num_cols_e_ + num_cols_f_) != matrix_.num_cols()) {
			  System.err.println("In public PartitionedMatrixView (num_cols_e_ + num_cols_f_) != matrix_.num_cols()");
		  }
	  }

	  public void LeftMultiplyE(double[] x, double[] y) {
		  CompressedRowBlockStructure bs = matrix_.block_structure();

		  // Iterate over the first num_row_blocks_e_ row blocks, and multiply
		  // by the first cell in each row block.
		  double[] values = matrix_.values();
		  for (int r = 0; r < num_row_blocks_e_; ++r) {
		    Cell cell = bs.rows.get(r).cells.get(0);
		    int row_block_pos = bs.rows.get(r).block.position;
		    int row_block_size = bs.rows.get(r).block.size;
		    int col_block_id = cell.block_id;
		    int col_block_pos = bs.cols.get(col_block_id).position;
		    int col_block_size = bs.cols.get(col_block_id).size;
		    MatrixTransposeVectorMultiply(kRowBlockSize, kEBlockSize, 1,
		        values, cell.position, row_block_size, col_block_size,
		        x, row_block_pos,
		        y, col_block_pos);
		  }	  
	  }
	  
	  public void LeftMultiplyF(double[] x, double[] y) {
		  CompressedRowBlockStructure bs = matrix_.block_structure();

		  // Iterate over row blocks, and if the row block is in E, then
		  // multiply by all the cells except the first one which is of type
		  // E. If the row block is not in E (i.e its in the bottom
		  // num_row_blocks - num_row_blocks_e row blocks), then all the cells
		  // are of type F and multiply by them all.
		  double[] values = matrix_.values();
		  for (int r = 0; r < num_row_blocks_e_; ++r) {
		    int row_block_pos = bs.rows.get(r).block.position;
		    int row_block_size = bs.rows.get(r).block.size;
		    Vector<Cell> cells = bs.rows.get(r).cells;
		    for (int c = 1; c < cells.size(); ++c) {
		      int col_block_id = cells.get(c).block_id;
		      int col_block_pos = bs.cols.get(col_block_id).position;
		      int col_block_size = bs.cols.get(col_block_id).size;
		      MatrixTransposeVectorMultiply(kRowBlockSize, kFBlockSize, 1,
		        values, cells.get(c).position, row_block_size, col_block_size,
		        x, row_block_pos,
		        y, col_block_pos - num_cols_e_);
		    }
		  }

		  for (int r = num_row_blocks_e_; r < bs.rows.size(); ++r) {
		    int row_block_pos = bs.rows.get(r).block.position;
		    int row_block_size = bs.rows.get(r).block.size;
		    Vector<Cell> cells = bs.rows.get(r).cells;
		    for (int c = 0; c < cells.size(); ++c) {
		      int col_block_id = cells.get(c).block_id;
		      int col_block_pos = bs.cols.get(col_block_id).position;
		      int col_block_size = bs.cols.get(col_block_id).size;
		      MatrixTransposeVectorMultiply(DYNAMIC, DYNAMIC, 1,
		        values, cells.get(c).position, row_block_size, col_block_size,
		        x, row_block_pos,
		        y, col_block_pos - num_cols_e_);
		    }
		  }  
	  }
	  
	  public void RightMultiplyE(double[] x, double[] y) {
		  CompressedRowBlockStructure bs = matrix_.block_structure();

		  // Iterate over the first num_row_blocks_e_ row blocks, and multiply
		  // by the first cell in each row block.
		  double[] values = matrix_.values();
		  for (int r = 0; r < num_row_blocks_e_; ++r) {
		    Cell cell = bs.rows.get(r).cells.get(0);
		    int row_block_pos = bs.rows.get(r).block.position;
		    int row_block_size = bs.rows.get(r).block.size;
		    int col_block_id = cell.block_id;
		    int col_block_pos = bs.cols.get(col_block_id).position;
		    int col_block_size = bs.cols.get(col_block_id).size;
		    MatrixVectorMultiply(kRowBlockSize, kEBlockSize, 1,
		        values,cell.position, row_block_size, col_block_size,
		        x, col_block_pos,
		        y, row_block_pos);
		  }  
	  }
	  
	  
	  public void RightMultiplyF(double[] x, double[] y) {
		  CompressedRowBlockStructure bs = matrix_.block_structure();

		  // Iterate over row blocks, and if the row block is in E, then
		  // multiply by all the cells except the first one which is of type
		  // E. If the row block is not in E (i.e its in the bottom
		  // num_row_blocks - num_row_blocks_e row blocks), then all the cells
		  // are of type F and multiply by them all.
		  double[] values = matrix_.values();
		  for (int r = 0; r < num_row_blocks_e_; ++r) {
		    int row_block_pos = bs.rows.get(r).block.position;
		    int row_block_size = bs.rows.get(r).block.size;
		    Vector<Cell> cells = bs.rows.get(r).cells;
		    for (int c = 1; c < cells.size(); ++c) {
		      int col_block_id = cells.get(c).block_id;
		      int col_block_pos = bs.cols.get(col_block_id).position;
		      int col_block_size = bs.cols.get(col_block_id).size;
		      MatrixVectorMultiply(kRowBlockSize, kFBlockSize, 1,
		          values, cells.get(c).position, row_block_size, col_block_size,
		          x, col_block_pos - num_cols_e_,
		          y, row_block_pos);
		    }
		  }

		  for (int r = num_row_blocks_e_; r < bs.rows.size(); ++r) {
		    int row_block_pos = bs.rows.get(r).block.position;
		    int row_block_size = bs.rows.get(r).block.size;
		    Vector<Cell> cells = bs.rows.get(r).cells;
		    for (int c = 0; c < cells.size(); ++c) {
		      int col_block_id = cells.get(c).block_id;
		      int col_block_pos = bs.cols.get(col_block_id).position;
		      int col_block_size = bs.cols.get(col_block_id).size;
		      MatrixVectorMultiply(DYNAMIC, DYNAMIC, 1,
		          values, cells.get(c).position, row_block_size, col_block_size,
		          x, col_block_pos - num_cols_e_,
		          y, row_block_pos);
		    }
		  }  
	  }
	  
	  public BlockSparseMatrix CreateBlockDiagonalEtE() {
		  BlockSparseMatrix block_diagonal =
			      CreateBlockDiagonalMatrixLayout(0, num_col_blocks_e_);
			  UpdateBlockDiagonalEtE(block_diagonal);
			  return block_diagonal;	  
	  }
	  
	  public BlockSparseMatrix CreateBlockDiagonalFtF() {
		  BlockSparseMatrix block_diagonal =
			      CreateBlockDiagonalMatrixLayout(
			          num_col_blocks_e_, num_col_blocks_e_ + num_col_blocks_f_);
			  UpdateBlockDiagonalFtF(block_diagonal);
			  return block_diagonal;  
	  }
	  
	  public void UpdateBlockDiagonalEtE(BlockSparseMatrix block_diagonal) {
		  CompressedRowBlockStructure bs = matrix_.block_structure();
		  CompressedRowBlockStructure block_diagonal_structure =
		      block_diagonal.block_structure();

		  block_diagonal.SetZero();
		  double[] values = matrix_.values();
		  for (int r = 0; r < num_row_blocks_e_ ; ++r) {
		    Cell cell = bs.rows.get(r).cells.get(0);
		    int row_block_size = bs.rows.get(r).block.size;
		    int block_id = cell.block_id;
		    int col_block_size = bs.cols.get(block_id).size;
		    int cell_position =
		        block_diagonal_structure.rows.get(block_id).cells.get(0).position;

		    MatrixTransposeMatrixMultiply
		        (kRowBlockSize, kEBlockSize, kRowBlockSize, kEBlockSize, 1,
		            values, cell.position, row_block_size, col_block_size,
		            values, cell.position, row_block_size, col_block_size,
		            block_diagonal.mutable_values(), cell_position,
		            0, 0, col_block_size, col_block_size);
		  }  
	  }
	  
	  public void UpdateBlockDiagonalFtF(BlockSparseMatrix block_diagonal) {
		  CompressedRowBlockStructure bs = matrix_.block_structure();
		  CompressedRowBlockStructure block_diagonal_structure =
		      block_diagonal.block_structure();

		  block_diagonal.SetZero();
		  double[] values = matrix_.values();
		  for (int r = 0; r < num_row_blocks_e_; ++r) {
		    int row_block_size = bs.rows.get(r).block.size;
		    Vector<Cell> cells = bs.rows.get(r).cells;
		    for (int c = 1; c < cells.size(); ++c) {
		      int col_block_id = cells.get(c).block_id;
		      int col_block_size = bs.cols.get(col_block_id).size;
		      int diagonal_block_id = col_block_id - num_col_blocks_e_;
		      int cell_position =
		          block_diagonal_structure.rows.get(diagonal_block_id).cells.get(0).position;

		      MatrixTransposeMatrixMultiply
		          (kRowBlockSize, kFBlockSize, kRowBlockSize, kFBlockSize, 1,
		              values, cells.get(c).position, row_block_size, col_block_size,
		              values, cells.get(c).position, row_block_size, col_block_size,
		              block_diagonal.mutable_values(), cell_position,
		              0, 0, col_block_size, col_block_size);
		    }
		  }

		  for (int r = num_row_blocks_e_; r < bs.rows.size(); ++r) {
		    int row_block_size = bs.rows.get(r).block.size;
		    Vector<Cell> cells = bs.rows.get(r).cells;
		    for (int c = 0; c < cells.size(); ++c) {
		      int col_block_id = cells.get(c).block_id;
		      int col_block_size = bs.cols.get(col_block_id).size;
		      int diagonal_block_id = col_block_id - num_col_blocks_e_;
		      int cell_position =
		          block_diagonal_structure.rows.get(diagonal_block_id).cells.get(0).position;

		      MatrixTransposeMatrixMultiply
		          (DYNAMIC, DYNAMIC, DYNAMIC, DYNAMIC, 1,
		              values, cells.get(c).position, row_block_size, col_block_size,
		              values, cells.get(c).position, row_block_size, col_block_size,
		              block_diagonal.mutable_values(), cell_position,
		              0, 0, col_block_size, col_block_size);
		    }
		  }  
	  }
	  
	  public int num_col_blocks_e() { return num_col_blocks_e_;  }
	  public int num_col_blocks_f() { return num_col_blocks_f_;  }
	  public int num_cols_e()       { return num_cols_e_;        }
	  public int num_cols_f()       { return num_cols_f_;        }
	  public int num_rows()         { return matrix_.num_rows(); }
	  public int num_cols()         { return matrix_.num_cols(); }

	 private BlockSparseMatrix CreateBlockDiagonalMatrixLayout(int start_col_block,
	                                                     int end_col_block) {

		  CompressedRowBlockStructure bs = matrix_.block_structure();
		  CompressedRowBlockStructure block_diagonal_structure =
		      new CompressedRowBlockStructure();

		  int block_position = 0;
		  int diagonal_cell_position = 0;

		  // Iterate over the column blocks, creating a new diagonal block for
		  // each column block.
		  for (int c = start_col_block; c < end_col_block; ++c) {
		    Block block = bs.cols.get(c);
		    block_diagonal_structure.cols.add(new Block());
		    Block diagonal_block = block_diagonal_structure.cols.get(block_diagonal_structure.cols.size()-1);
		    diagonal_block.size = block.size;
		    diagonal_block.position = block_position;

		    block_diagonal_structure.rows.add(new CompressedList());
		    CompressedList row = block_diagonal_structure.rows.get(block_diagonal_structure.rows.size()-1);
		    row.block = diagonal_block;

		    row.cells.add(new Cell());
		    Cell cell = row.cells.get(row.cells.size()-1);
		    cell.block_id = c - start_col_block;
		    cell.position = diagonal_cell_position;

		    block_position += block.size;
		    diagonal_cell_position += block.size * block.size;
		  }

		  // Build a BlockSparseMatrix with the just computed block
		  // structure.
		  return new BlockSparseMatrix(block_diagonal_structure);	 
	 }
	  
	} // class PartitionedMatrixView
	
	public PartitionedMatrixView createPartitionedMatrixView(LinearSolverOptions options, BlockSparseMatrix matrix) {
		if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 2) &&
			     (options.f_block_size[0] == 2)) {
			   return new PartitionedMatrixView(2, 2, 2, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 2) &&
			     (options.f_block_size[0] == 3)) {
			   return new PartitionedMatrixView(2, 2, 3,matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 2) &&
			     (options.f_block_size[0] == 4)) {
			   return new PartitionedMatrixView(2, 2, 4,matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 2)) {
			   return new PartitionedMatrixView(2, 2, DYNAMIC, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 3) &&
			     (options.f_block_size[0] == 3)) {
			   return new PartitionedMatrixView(2, 3, 3,matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 3) &&
			     (options.f_block_size[0] == 4)) {
			   return new PartitionedMatrixView(2, 3, 4, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 3) &&
			     (options.f_block_size[0] == 6)) {
			   return new PartitionedMatrixView(2, 3, 6, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 3) &&
			     (options.f_block_size[0] == 9)) {
			   return new PartitionedMatrixView(2, 3, 9, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 3)) {
			   return new PartitionedMatrixView(2, 3, DYNAMIC, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 3)) {
			   return new PartitionedMatrixView(2, 4, 3, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 4)) {
			   return new PartitionedMatrixView(2, 4, 4, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 6)) {
			   return new PartitionedMatrixView(2, 4, 6, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 8)) {
			   return new PartitionedMatrixView(2, 4, 8, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 9)) {
			   return new PartitionedMatrixView(2, 4, 9, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 2) &&
			     (options.e_block_size[0] == 4)) {
			   return new PartitionedMatrixView(2, 4, DYNAMIC, matrix, options.elimination_groups.get(0));
			 }
			 if (options.row_block_size[0] == 2){
			   return new PartitionedMatrixView(2, DYNAMIC, DYNAMIC, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 4) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 2)) {
			   return new PartitionedMatrixView(4, 4, 2, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 4) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 3)) {
			   return new PartitionedMatrixView(4, 4, 3, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 4) &&
			     (options.e_block_size[0] == 4) &&
			     (options.f_block_size[0] == 4)) {
			   return new PartitionedMatrixView(4, 4, 4, matrix, options.elimination_groups.get(0));
			 }
			 if ((options.row_block_size[0] == 4) &&
			     (options.e_block_size[0] == 4)) {
			   return new PartitionedMatrixView(4, 4, DYNAMIC, matrix, options.elimination_groups.get(0));
			 }
			 if (1 <= MAX_LOG_LEVEL) {
			  Preferences.debug("Template specializations not found for "
			          + options.row_block_size[0] + ","
			          + options.e_block_size[0] + ","
			          + options.f_block_size[0] + "\n", Preferences.DEBUG_ALGORITHM);
			 }
			  return new PartitionedMatrixView(DYNAMIC, DYNAMIC, DYNAMIC,
			               matrix, options.elimination_groups.get(0));
	
	}
	
	class ImplicitSchurComplement extends LinearOperator {
		 private LinearSolverOptions options_;

		 private PartitionedMatrixView A_;
		 private double[] D_;
		 private double[] b_;

		 private BlockSparseMatrix block_diagonal_EtE_inverse_;
		 private BlockSparseMatrix block_diagonal_FtF_inverse_;

		 private Vector<Double> rhs_;

		  // Temporary storage vectors used to implement RightMultiply.
		 private Vector<Double> tmp_rows_;
		 private Vector<Double> tmp_e_cols_;
		 private Vector<Double> tmp_e_cols_2_;
		 private Vector<Double> tmp_f_cols_;
		  // num_eliminate_blocks is the number of E blocks in the matrix
		  // A.
		  //
		  // preconditioner indicates whether the inverse of the matrix F'F
		  // should be computed or not as a preconditioner for the Schur
		  // Complement.
		  //
		  // TODO(sameeragarwal): Get rid of the two bools below and replace
		  // them with enums.
		  public ImplicitSchurComplement(LinearSolverOptions options) {
			  super();
			  options_ = options;
			  D_ = null;
			  b_ = null;
			  rhs_ = new Vector<Double>();
			  tmp_rows_ = new Vector<Double>();
			  tmp_e_cols_ = new Vector<Double>();
			  tmp_e_cols_2_ = new Vector<Double>();
			  tmp_f_cols_ = new Vector<Double>();
		  }

		  // Initialize the Schur complement for a linear least squares
		  // problem of the form
		  //
		  //   |A      | x = |b|
		  //   |diag(D)|     |0|
		  //
		  // If D is null, then it is treated as a zero dimensional matrix. It
		  // is important that the matrix A have a BlockStructure object
		  // associated with it and has a block structure that is compatible
		  // with the SchurComplement solver.
		  public void Init(BlockSparseMatrix A, double[] D, double[] b) {
			  int i;
			// Since initialization is reasonably heavy, perhaps we can save on
			  // constructing a new object everytime.
			  if (A_ == null) {
			    A_ = createPartitionedMatrixView(options_, A);
			  }

			  D_ = D;
			  b_ = b;

			  // Initialize temporary storage and compute the block diagonals of
			  // E'E and F'E.
			  if (block_diagonal_EtE_inverse_ == null) {
			    block_diagonal_EtE_inverse_ = A_.CreateBlockDiagonalEtE();
			    if (options_.preconditioner_type == PreconditionerType.JACOBI) {
			      block_diagonal_FtF_inverse_ = A_.CreateBlockDiagonalFtF();
			    }
			    rhs_.clear();
			    for (i = 0; i < A_.num_cols_f(); i++) {
			    	rhs_.add(0.0);
			    }
			    while (tmp_rows_.size() > A_.num_rows()) {
			    	tmp_rows_.removeElementAt(tmp_rows_.size()-1);
			    }
			    while (tmp_rows_.size() < A_.num_rows()) {
			    	tmp_rows_.add(0.0);
			    }
			    while (tmp_e_cols_.size() > A_.num_cols_e()) {
			    	tmp_e_cols_.removeElementAt(tmp_e_cols_.size()-1);
			    }
			    while (tmp_e_cols_.size() < A_.num_cols_e()) {
			    	tmp_e_cols_.add(0.0);
			    }
			    while (tmp_e_cols_2_.size() > A_.num_cols_e()) {
			    	tmp_e_cols_2_.removeElementAt(tmp_e_cols_2_.size()-1);
			    }
			    while (tmp_e_cols_2_.size() < A_.num_cols_e()) {
			    	tmp_e_cols_2_.add(0.0);
			    }
			    while (tmp_f_cols_.size() > A_.num_cols_f()) {
			    	tmp_f_cols_.removeElementAt(tmp_f_cols_.size()-1);
			    }
			    while (tmp_f_cols_.size() < A_.num_cols_f()) {
			    	tmp_f_cols_.add(0.0);
			    }
			  } else {
			    A_.UpdateBlockDiagonalEtE(block_diagonal_EtE_inverse_);
			    if (options_.preconditioner_type == PreconditionerType.JACOBI) {
			      A_.UpdateBlockDiagonalFtF(block_diagonal_FtF_inverse_);
			    }
			  }

			  // The block diagonals of the augmented linear system contain
			  // contributions from the diagonal D if it is non-null. Add that to
			  // the block diagonals and invert them.
			  AddDiagonalAndInvert(D_, 0, block_diagonal_EtE_inverse_);
			  if (options_.preconditioner_type == PreconditionerType.JACOBI) {
				if (D_ == null) {
					AddDiagonalAndInvert(null, 0, block_diagonal_FtF_inverse_);
				}
				else {
					AddDiagonalAndInvert(D_, A_.num_cols_e(), block_diagonal_FtF_inverse_);
				}
			  }

			  // Compute the RHS of the Schur complement system.
			  UpdateRhs();

		  }

		  // y += Sx, where S is the Schur complement.
		  // Evaluate the product
		  //
		  //   Sx = [F'F - F'E (E'E)^-1 E'F]x
		  //
		  // By breaking it down into individual matrix vector products
		  // involving the matrices E and F. This is implemented using a
		  // PartitionedMatrixView of the input matrix A.

		  public void RightMultiply(double[] x, double[] y) {
			  int i;
			  int row;
			  int col;
			// y1 = F x
			  
			  double tmp_rows_array[] = new double[tmp_rows_.size()];
			  A_.RightMultiplyF(x, tmp_rows_array);
			  for (i = 0; i < tmp_rows_.size(); i++) {
				  tmp_rows_.setElementAt(tmp_rows_array[i],i);
			  }

			  // y2 = E' y1
			  double tmp_e_cols_array[] = new double[tmp_e_cols_.size()];
			  A_.LeftMultiplyE(tmp_rows_array, tmp_e_cols_array);
			  for (i = 0; i < tmp_e_cols_.size(); i++) {
				  tmp_e_cols_.setElementAt(tmp_e_cols_array[i],i);
			  }

			  // y3 = -(E'E)^-1 y2
			  double tmp_e_cols_2_array[] = new double[tmp_e_cols_2_.size()];
			  block_diagonal_EtE_inverse_.RightMultiply(tmp_e_cols_array,
			                                             tmp_e_cols_2_array);
			  for (i = 0; i < tmp_e_cols_2_.size(); i++) {
			      tmp_e_cols_2_array[i] *= -1.0;
			      tmp_e_cols_2_.setElementAt(tmp_e_cols_2_array[i],i);
			  }

			  // y1 = y1 + E y3
			  A_.RightMultiplyE(tmp_e_cols_2_array, tmp_rows_array);
			  for (i = 0; i < tmp_rows_.size(); i++) {
				  tmp_rows_.setElementAt(tmp_rows_array[i],i);
			  }


			  // y5 = D * x
			  if (D_ != null) {
				  double D[] = new double[num_cols()];
				  for (row = A_.num_cols_e(); row < A_.num_cols_e() + num_cols(); row++) {
					  D[row - A_.num_cols_e()] = D_[row];
				  }
				  for (row = 0; row < num_cols(); row++) {
					  y[row] = D[row] * x[row];
				  }
			  } else {
				for (i = 0; i < num_cols(); i++) {
					y[i] = 0.0;
				}
			  }

			  // y = y5 + F' y1
			  A_.LeftMultiplyF(tmp_rows_array, y);
  
		  }

		  // The Schur complement is a symmetric positive definite matrix,
		  // thus the left and right multiply operators are the same.
		  public void LeftMultiply(double[] x, double[] y) {
		    RightMultiply(x, y);
		  }

		  // y = (E'E)^-1 (E'b - E'F x). Given an estimate of the solution to
		  // the Schur complement system, this method computes the value of
		  // the e_block variables that were eliminated to form the Schur
		  // complement.
		  public void BackSubstitute(double[] x, double[] y) {
			  int i;
			  int num_cols_e = A_.num_cols_e();
			  int num_cols_f = A_.num_cols_f();
			  int num_cols =  A_.num_cols();
			  int num_rows = A_.num_rows();

			  // y1 = F x
			  double tmp_rows_array[] = new double[tmp_rows_.size()];
			  A_.RightMultiplyF(x, tmp_rows_array);

			  // y2 = b - y1
			  for (i = 0; i < num_rows; i++) {
				  tmp_rows_array[i] = b_[i] - tmp_rows_array[i];
			  }
			  for (i = 0; i < tmp_rows_.size(); i++) {
				  tmp_rows_.setElementAt(tmp_rows_array[i],i);
			  }

			  // y3 = E' y2
			  double tmp_e_cols_array[] = new double[tmp_e_cols_.size()];
			  A_.LeftMultiplyE(tmp_rows_array, tmp_e_cols_array);
			  for (i = 0; i < tmp_e_cols_.size(); i++) {
				  tmp_e_cols_.setElementAt(tmp_e_cols_array[i],i);
			  }

			  // y = (E'E)^-1 y3
			  for (i = 0; i < num_cols; i++) {
				  y[i] = 0;
			  }
			  block_diagonal_EtE_inverse_.RightMultiply(tmp_e_cols_array, y);

			  // The full solution vector y has two blocks. The first block of
			  // variables corresponds to the eliminated variables, which we just
			  // computed via back substitution. The second block of variables
			  // corresponds to the Schur complement system, so we just copy those
			  // values from the solution to the Schur complement.
			  for (i = 0; i < num_cols_f; i++) {
				  y[num_cols_e + i] = x[i];
			  }
  
		  }

		  public int num_rows() { return A_.num_cols_f(); }
		  public int num_cols() { return A_.num_cols_f(); }
		  public Vector<Double> rhs()   { return rhs_;             }

		  public BlockSparseMatrix block_diagonal_EtE_inverse() {
		    return block_diagonal_EtE_inverse_;
		  }

		  public BlockSparseMatrix block_diagonal_FtF_inverse() {
		    return block_diagonal_FtF_inverse_;
		  }

		 // Given a block diagonal matrix and an optional array of diagonal
		 // entries D, add them to the diagonal of the matrix and compute the
		 // inverse of each diagonal block.
		  private void AddDiagonalAndInvert(double[] D, int D_start, BlockSparseMatrix block_diagonal) {
			  int row;
			  int col;
			  int index;
			  CompressedRowBlockStructure block_diagonal_structure =
				      block_diagonal.block_structure();
				  for (int r = 0; r < block_diagonal_structure.rows.size(); ++r) {
				    int row_block_pos = block_diagonal_structure.rows.get(r).block.position;
				    int row_block_size = block_diagonal_structure.rows.get(r).block.size;
				    Cell cell = block_diagonal_structure.rows.get(r).cells.get(0);
				    double marray[][] = new double[row_block_size][row_block_size];
				    index = cell.position;
				    for (row = 0; row < row_block_size; row++) {
				    	for (col = 0; col < row_block_size; col++) {
				    		marray[row][col] = block_diagonal.mutable_values()[index++];
				    	}
				    }
				    //MatrixRef m(block_diagonal->mutable_values() + cell.position,
				                //row_block_size, row_block_size);

				    if (D != null) {
				      index = D_start + row_block_pos;
				      for (row = 0; row < row_block_size; row++) {
				    	  marray[row][row] += D[index];
				    	  index++;
				      }
				    }

				    Matrix m = new Matrix(marray);
				    m = m.inverse();
				    marray = m.getArray();
				    index = cell.position;
				    for (row = 0; row < row_block_size; row++) {
				    	for (col = 0; col < row_block_size; col++) {
				    		block_diagonal.mutable_values()[index++] = marray[row][col];
				    	}
				    }
				  }
  
		  }
		  
		  // Compute the RHS of the Schur complement system.
		  //
		  // rhs = F'b - F'E (E'E)^-1 E'b
		  //
		  // Like BackSubstitute, we use the block structure of A to implement
		  // this using a series of matrix vector products.
		  private void UpdateRhs() {
			  int i;
			  // y1 = E'b
			  double tmp_e_cols_array[] = new double[tmp_e_cols_.size()];
			  A_.LeftMultiplyE(b_, tmp_e_cols_array);
			  for (i = 0; i < tmp_e_cols_.size(); i++) {
				  tmp_e_cols_.setElementAt(tmp_e_cols_array[i],i);
			  }

			  // y2 = (E'E)^-1 y1
			  double y2[] = new double[A_.num_cols_e()];
			  block_diagonal_EtE_inverse_.RightMultiply(tmp_e_cols_array, y2);

			  // y3 = E y2
			  double tmp_rows_array[] = new double[tmp_rows_.size()];
			  A_.RightMultiplyE(y2, tmp_rows_array);

			  // y3 = b - y3
			  for (i = 0; i < A_.num_rows(); i++) {
				  tmp_rows_array[i] = b_[i] - tmp_rows_array[i];
			  }
			  for (i = 0; i < tmp_rows_.size(); i++) {
				  tmp_rows_.setElementAt(tmp_rows_array[i],i);
			  }

			  // rhs = F' y3
			  double rhs_array[] = new double[rhs_.size()];
			  A_.LeftMultiplyF(tmp_rows_array, rhs_array);
			  for (i = 0; i < rhs_.size(); i++) {
				  rhs_.setElementAt(rhs_array[i],i);
			  }
  
		  }

		  
		};
		
		// The two solvers can be instantiated by calling
		// LinearSolver::CreateLinearSolver with LinearSolver::Options::type
		// set to DENSE_SCHUR and SPARSE_SCHUR
		// respectively. LinearSolver::Options::elimination_groups[0] should
		// be at least 1.
		abstract class SchurComplementSolver extends TypedLinearSolver<BlockSparseMatrix> {
			  private LinearSolverOptions options_;

			  private SchurEliminatorBase eliminator_;
			  private BlockRandomAccessMatrix lhs_;
			  private double rhs_[];
		 public SchurComplementSolver(LinearSolverOptions options) {
			  super();
		      options_ = options;
		      if (options.elimination_groups.size() <= 1) {
		    	  System.err.println("In public SchurComplmentSolver options.elimination_groups.size() <= 1");
		    	  return;
		      }
		      if (options.elimination_groups.get(0) <= 0) {
		    	  System.err.println("In public SchurComplmentSolver options.elimination_groups.get(0) <= 0");
		    	  return;
		      }
		      if (options.context == null) {
		    	  System.err.println("in public SchurComplementSolver options.context == null");
		    	  return;
		      }
		  }

		  // LinearSolver methods
		 public LinearSolverSummary SolveImpl(
				    BlockSparseMatrix A,
				    double[] b,
				    LinearSolverPerSolveOptions per_solve_options,
				    double[] x) {
			      int i;
				  EventLogger event_logger = new EventLogger("SchurComplementSolver::Solve");

				  if (eliminator_ == null) {
				    InitStorage(A.block_structure());
				    DetectStructure(A.block_structure(),
				                    options_.elimination_groups.get(0),
				                    options_.row_block_size,
				                    options_.e_block_size,
				                    options_.f_block_size);
				    eliminator_ = createSchurEliminatorBase(options_);
				    if (eliminator_ == null) {
				    	System.err.println("In SchurComplementSolver SolveImpl eliminator_ == null");
				    	return null;
				    }
				    boolean kFullRankETE = true;
				    eliminator_.Init(
				        options_.elimination_groups.get(0), kFullRankETE, A.block_structure());
				  };

				  for (i = 0; i < A.num_cols(); i++) {
					  x[i] = 0.0;
				  }
				  event_logger.AddEvent("Setup");

				  eliminator_.Eliminate(A, b, per_solve_options.D, lhs_, rhs_);
				  event_logger.AddEvent("Eliminate");

				  int redlength = x.length - (A.num_cols() - lhs_.num_cols());
				  double[] reduced_solution = new double[redlength];
				  int offset = A.num_cols() - lhs_.num_cols();
				  for (i = offset; i < x.length; i++) {
					  reduced_solution[i-offset] = x[i];
				  }
				  //double* reduced_solution = x + A->num_cols() - lhs_->num_cols();
				  LinearSolverSummary summary =
				      SolveReducedLinearSystem(per_solve_options, reduced_solution);
				  for (i = offset; i < x.length; i++) {
					  x[i] = reduced_solution[i-offset];
				  }
				  event_logger.AddEvent("ReducedSolve");

				  if (summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS) {
				    eliminator_.BackSubstitute(A, b, per_solve_options.D, reduced_solution, x);
				    event_logger.AddEvent("BackSubstitute");
				  }

				  return summary;
				}

		  

		  protected LinearSolverOptions options() { return options_; }

		  protected BlockRandomAccessMatrix lhs() { return lhs_; }
		  protected void set_lhs(BlockRandomAccessMatrix lhs) { lhs_ = lhs; }
		  protected double[] rhs() { return rhs_; }
		  protected void set_rhs(double[] rhs) { rhs_ = rhs; }

		  protected abstract void InitStorage(CompressedRowBlockStructure bs);
		  protected abstract LinearSolverSummary SolveReducedLinearSystem(
		      LinearSolverPerSolveOptions per_solve_options,
		      double[] solution);
		} // abstract class SchurComplementSolver
		
		// Dense Cholesky factorization based solver.
		class DenseSchurComplementSolver extends SchurComplementSolver {
		 public DenseSchurComplementSolver(LinearSolverOptions options) {
		      super(options);
		 }

		 protected void InitStorage(CompressedRowBlockStructure bs) {
			 int num_eliminate_blocks = options().elimination_groups.get(0);
			 int num_col_blocks = bs.cols.size();

			  Vector<Integer> blocks = new Vector<Integer>(num_col_blocks - num_eliminate_blocks);
			  for (int i = num_eliminate_blocks, j = 0;
			       i < num_col_blocks;
			       ++i, ++j) {
			    blocks.add(bs.cols.get(i).size);
			  }

			  set_lhs(new BlockRandomAccessDenseMatrix(blocks));
			  set_rhs(new double[lhs().num_rows()]);
	 
		 }
		 protected LinearSolverSummary SolveReducedLinearSystem(
		      LinearSolverPerSolveOptions per_solve_options,
		      double[] solution) {
			  int i;

			  LinearSolverSummary summary = new LinearSolverSummary();
			  summary.num_iterations = 0;
			  summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS;
			  summary.message[0] = "Success.";

			  BlockRandomAccessDenseMatrix m =
			      (BlockRandomAccessDenseMatrix)(lhs());
			  int num_rows = m.num_rows();

			  // The case where there are no f blocks, and the system is block
			  // diagonal.
			  if (num_rows == 0) {
			    return summary;
			  }

			  summary.num_iterations = 1;
			  //VectorRef(solution, num_rows) = ConstVectorRef(rhs(), num_rows);
			  for (i = 0; i < num_rows; i++) {
				  solution[i] = rhs()[i];
			  }
			  double marr[][] = new double[m.values().length][1];
			  for (i = 0; i < m.values().length; i++) {
				  marr[i][0] = m.values()[i];
			  }
			    summary.termination_type =
			        SolveInPlaceUsingCholesky(num_rows,
			                                          marr,
			                                          solution,
			                                          summary.message);
			    return summary;

		 }
		};
		
		class BlockRandomAccessDenseMatrix  extends BlockRandomAccessMatrix {
			private int num_rows_;
		    private Vector<Integer> block_layout_;
		    private double[] values_;
		    private CellInfo[] cell_infos_;
			  // blocks is a vector of block sizes. The resulting matrix has
			  // blocks.size() * blocks.size() cells.
			  public BlockRandomAccessDenseMatrix(Vector<Integer> blocks) {
				  int i;
				  int num_blocks = blocks.size();
				  if (block_layout_ != null) {
				      block_layout_.clear();
				  }
				  else {
					  block_layout_ = new Vector<Integer>();
				  }
				  num_rows_ = 0;
				  for (i = 0; i < num_blocks; ++i) {
				    block_layout_.add(num_rows_);
				    num_rows_ += blocks.get(i);
				  }

				  values_ = new double[num_rows_ * num_rows_];

				  cell_infos_ = new CellInfo[num_blocks * num_blocks];
				  for (i = 0; i < num_blocks * num_blocks; ++i) {
					  cell_infos_[i] = new CellInfo();
				      cell_infos_[i].values = values_;
				      cell_infos_[i].values_index = 0;
				  }

				  SetZero();
  
  
			  }

			  // BlockRandomAccessMatrix interface.
			  public CellInfo GetCell(int row_block_id,
			                            int col_block_id,
			                            int[] row,
			                            int[] col,
			                            int[] row_stride,
			                            int[] col_stride) {
				  row[0] = block_layout_.get(row_block_id);
				  col[0] = block_layout_.get(col_block_id);
				  row_stride[0] = num_rows_;
				  col_stride[0] = num_rows_;
				  return cell_infos_[row_block_id * block_layout_.size() + col_block_id];  
			  }

			  // This is not a thread safe method, it assumes that no cell is
			  // locked.
			// Assume that the user does not hold any locks on any cell blocks
			// when they are calling SetZero.
			  public void SetZero() {
				  int i;
				  int length = num_rows_ * num_rows_;
				  for (i = 0; i < length; i++) {
					  values_[i] = 0.0;
				  }
			  }

			  // Since the matrix is square with the same row and column block
			  // structure, num_rows() = num_cols().
			  public int num_rows() { return num_rows_; }
			  public int num_cols() { return num_rows_; }

			  // The underlying matrix storing the cells.
			  public double[] values() { return values_; }
			  public double[] mutable_values() { return values_; }
			};

	 
	 class IterativeSchurComplementSolver extends TypedLinearSolver<BlockSparseMatrix> {
		  private LinearSolverOptions options_;
		  private ImplicitSchurComplement schur_complement_;
		  private Preconditioner preconditioner_;
		  private Vector<Double> reduced_linear_system_solution_;
		  
		  public IterativeSchurComplementSolver(LinearSolverOptions options) {
			  super();
		      options_ = options;
		      options_.type = LinearSolverType.ITERATIVE_SCHUR;
		      reduced_linear_system_solution_ = new Vector<Double>();
		  }

		  public LinearSolverSummary SolveImpl(
				    BlockSparseMatrix A,
				    double[] b,
				    LinearSolverPerSolveOptions per_solve_options,
				    double[] x) {
			      int i;
				  EventLogger event_logger = new EventLogger("IterativeSchurComplementSolver::Solve");

				  if (A.block_structure() == null) {
					  System.err.println("A.block_structure() == null in IterativeSchurComplementSolver SolveImpl");
					  return null;
				  }
				  int num_eliminate_blocks = options_.elimination_groups.get(0);
				  // Initialize a ImplicitSchurComplement object.
				  if (schur_complement_ == null) {
				    DetectStructure(A.block_structure(),
				                    num_eliminate_blocks,
				                    options_.row_block_size,
				                    options_.e_block_size,
				                    options_.f_block_size);
				    schur_complement_ = new ImplicitSchurComplement(options_);
				  }
				  schur_complement_.Init(A, per_solve_options.D, b);

				  int num_schur_complement_blocks =
				      A.block_structure().cols.size() - num_eliminate_blocks;
				  if (num_schur_complement_blocks == 0) {
					if (2 <= MAX_LOG_LEVEL) {
				        Preferences.debug("No parameter blocks left in the schur complement.\n", Preferences.DEBUG_ALGORITHM);
					}
				    LinearSolverSummary summary = new LinearSolverSummary();
				    summary.num_iterations = 0;
				    summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS;
				    schur_complement_.BackSubstitute(null, x);
				    return summary;
				  }

				  // Initialize the solution to the Schur complement system to zero.
				  reduced_linear_system_solution_.clear();
				  for (i = 0; i < schur_complement_.num_rows(); i++) {
					  reduced_linear_system_solution_.add(0.0);
				  }

				  LinearSolverOptions cg_options = new LinearSolverOptions();
				  cg_options.min_num_iterations = options_.min_num_iterations;
				  cg_options.max_num_iterations = options_.max_num_iterations;
				  ConjugateGradientsSolver cg_solver = new ConjugateGradientsSolver(cg_options);

				  LinearSolverPerSolveOptions cg_per_solve_options = new LinearSolverPerSolveOptions();
				  cg_per_solve_options.r_tolerance = per_solve_options.r_tolerance;
				  cg_per_solve_options.q_tolerance = per_solve_options.q_tolerance;

				  CreatePreconditioner(A);
				  if (preconditioner_ != null) {
				    if (!preconditioner_.Update(A, per_solve_options.D)) {
				      LinearSolverSummary summary = new LinearSolverSummary();
				      summary.num_iterations = 0;
				      summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_FAILURE;
				      summary.message[0] = "Preconditioner update failed.";
				      return summary;
				    }

				    cg_per_solve_options.preconditioner = preconditioner_;
				  }

				  event_logger.AddEvent("Setup");
				  LinearSolverSummary summary =
				      cg_solver.Solve(schur_complement_,
				                      schur_complement_.rhs(),
				                      cg_per_solve_options,
				                      reduced_linear_system_solution_);
				  if (summary.termination_type != LinearSolverTerminationType.LINEAR_SOLVER_FAILURE &&
				      summary.termination_type != LinearSolverTerminationType.LINEAR_SOLVER_FATAL_ERROR) {
					double reduced_linear_system_solution_array[] = new double[reduced_linear_system_solution_.size()];
					for (i = 0; i < reduced_linear_system_solution_.size(); i++) {
						reduced_linear_system_solution_array[i] = reduced_linear_system_solution_.get(i);
					}
				    schur_complement_.BackSubstitute(reduced_linear_system_solution_array,x);
				  }
				  event_logger.AddEvent("Solve");
				  return summary;
				}

				private void CreatePreconditioner(
				    BlockSparseMatrix A) {
				  if (options_.preconditioner_type == PreconditionerType.IDENTITY ||
				      preconditioner_ != null) {
				    return;
				  }

				  PreconditionerOptions preconditioner_options = new PreconditionerOptions();
				  preconditioner_options.type = options_.preconditioner_type;
				  preconditioner_options.visibility_clustering_type =
				      options_.visibility_clustering_type;
				  preconditioner_options.sparse_linear_algebra_library_type =
				      options_.sparse_linear_algebra_library_type;
				  preconditioner_options.num_threads = options_.num_threads;
				  preconditioner_options.row_block_size = options_.row_block_size[0];
				  preconditioner_options.e_block_size = options_.e_block_size[0];
				  preconditioner_options.f_block_size = options_.f_block_size[0];
				  preconditioner_options.elimination_groups = options_.elimination_groups;
				  if (options_.context == null) {
					  System.err.println("options_.context == null in IterativeSchurComplementSolver CreatePreconditioner");
					  return;
				  }
				  preconditioner_options.context = options_.context;

				  switch (options_.preconditioner_type) {
				    case JACOBI:
				      preconditioner_ = new SparseMatrixPreconditionerWrapper(
				          schur_complement_.block_diagonal_FtF_inverse());
				      break;
				    case SCHUR_JACOBI:
				      preconditioner_ = new SchurJacobiPreconditioner(
				          A.block_structure(), preconditioner_options);
				      break;
				    // Requires SuiteSparse
				    //case CLUSTER_JACOBI:
				    //case CLUSTER_TRIDIAGONAL:
				    //  preconditioner_.reset(new VisibilityBasedPreconditioner(
				    //      *A->block_structure(), preconditioner_options));
				    //  break;
				    default:
				      System.err.println("Unknown Preconditioner Type");
				  }
				};
	 }
	 
	// Linear solvers that depend on acccess to the low level structure of
	// a SparseMatrix.
	// typedef TypedLinearSolver<BlockSparseMatrix> BlockSparseMatrixSolver; 
	// typedef TypedLinearSolver<CompressedRowSparseMatrix>
	// CompressedRowSparseMatrixSolver;
	// typedef TypedLinearSolver<DenseSparseMatrix> DenseSparseMatrixSolver;
	// typedef TypedLinearSolver<TripletSparseMatrix> TripletSparseMatrixSolver;
	abstract class TypedLinearSolver<MatrixType> extends LinearSolver {
		public ExecutionSummary execution_summary_;
		
		public TypedLinearSolver() {
		    execution_summary_ = new ExecutionSummary();	
		}

		// public:
		// virtual LinearSolver::Summary Solve(
		public LinearSolverSummary Solve(LinearOperator A, double b[], LinearSolverPerSolveOptions per_solve_options,
				double x[]) {
			new ScopedExecutionTimer("LinearSolver::Solve", execution_summary_);
			if (A == null) {
				System.err.println("In TypedLinearSolver public LinearSummary.Summary Solve LinearOperator A == null");
				return null;
			}
			if (b == null) {
				System.err.println("In TypedLinearSolver public LinearSummary.Summary Solve double b[] == null");
				return null;
			}
			if (x == null) {
				System.err.println("In TypedLinearSolver public LinearSummary.Summary Solve double x[] == null");
				return null;
			}
			// return SolveImpl(down_cast<MatrixType*>(A), b, per_solve_options, x);
			return SolveImpl((MatrixType) A, b, per_solve_options, x);
		}

		// virtual std::map<std::string, CallStatistics> Statistics() const {
		public HashMap<String, CallStatistics> Statistics() {
			return execution_summary_.statistics();
		}

		public abstract LinearSolverSummary SolveImpl(MatrixType A, double b[],
				LinearSolverPerSolveOptions per_solve_options, double x[]);

	};

	public LinearSolverType LinearSolverForZeroEBlocks(LinearSolverType linear_solver_type) {
		if (!IsSchurType(linear_solver_type)) {
			return linear_solver_type;
		}

		if (linear_solver_type == LinearSolverType.SPARSE_SCHUR) {
			return LinearSolverType.SPARSE_NORMAL_CHOLESKY;
		}

		if (linear_solver_type == LinearSolverType.DENSE_SCHUR) {
			// TODO(sameeragarwal): This is probably not a great choice.
			// Ideally, we should have a DENSE_NORMAL_CHOLESKY, that can take
			// a BlockSparseMatrix as input.
			System.out.println("LinearSolverForZeroEBlocks becomes LinearSolverType.DENSE_QR");
			return LinearSolverType.DENSE_QR;
		}

		if (linear_solver_type == LinearSolverType.ITERATIVE_SCHUR) {
			return LinearSolverType.CGNR;
		}

		return linear_solver_type;
	}
	
	// Options struct to control Evaluator::Evaluate;
			class EvaluateOptions {
				// If false, the loss function correction is not applied to the
				// residual blocks.
				public boolean apply_loss_function;

				// If false, this evaluation point is the same as the last one.
				public boolean new_evaluation_point;

				public EvaluateOptions() {
					apply_loss_function = true;
					new_evaluation_point = true;
				}

			} // class EvaluateOptions
			
			class EvaluatorOptions {
				public int num_threads;
				public int num_eliminate_blocks;
				public LinearSolverType linear_solver_type;
				public boolean dynamic_sparsity;
				public Context context;
				public EvaluationCallback evaluation_callback;

				public EvaluatorOptions() {
					num_threads = 1;
					num_eliminate_blocks = -1;
					linear_solver_type = requestedLinearSolverType;
					dynamic_sparsity = false;
					context = new Context();
					evaluation_callback = null;
				}
				
				  


			} // class EvaluatorOptions


	abstract class Evaluator {
		public EvaluatorOptions options;

		public Evaluator() {
			options = new EvaluatorOptions();
		}

		
		// however, the mapping is more complicated in the case of parameterizations
		  // like quaternions. This is the same as the "Plus()" operation in
		  // local_parameterization.h, but operating over the entire state vector for a
		  // problem.
		  public abstract boolean Plus(Vector<Double> state,
		                    Vector<Double> delta,
		                    Vector<Double> state_plus_delta);
		  
		  public abstract boolean Plus(double[] state,
                  double[] delta,
                  double[] state_plus_delta);

		  // The number of parameters in the optimization problem.
		  public abstract int NumParameters();

		  // This is the effective number of parameters that the optimizer may adjust.
		  // This applies when there are parameterizations on some of the parameters.
		  public abstract int NumEffectiveParameters();

		  // The number of residuals in the optimization problem.
		  public abstract int NumResiduals();
		  
		// state is an array of size NumParameters(), cost is a pointer to a single
		  // double, and residuals is an array of doubles of size NumResiduals().
		  public abstract boolean Evaluate(EvaluateOptions evaluate_options,
		                        double[] state,
		                        double[] cost,
		                        double[] residuals,
		                        double[] gradient,
		                        SparseMatrix jacobian);
		  
		  public abstract boolean Evaluate(EvaluateOptions evaluate_options,
	                Vector<Double> state,
	                double[] cost,
	                Vector<Double> residuals,
	                Vector<Double> gradient,
	                SparseMatrix jacobian);

		  // Variant of Evaluator::Evaluate where the user wishes to use the
		  // default EvaluateOptions struct. This is mostly here as a
		  // convenience method.
		  public boolean Evaluate(double[] state,
		                double[] cost,
		                double[] residuals,
		                double[] gradient,
		                SparseMatrix jacobian) {
		    return Evaluate(new EvaluateOptions(),
		                    state,
		                    cost,
		                    residuals,
		                    gradient,
		                    jacobian);
		  }
		  
		  public HashMap<String, CallStatistics> Statistics() {
			    return new HashMap<String, CallStatistics>();
		  }
		
		// It is expected that the classes implementing this interface will be aware
		// of their client's requirements for the kind of sparse matrix storage and
		// layout that is needed for an efficient implementation. For example
		// CompressedRowOptimizationProblem creates a compressed row representation of
		// the jacobian for use with CHOLMOD, where as BlockOptimizationProblem
		// creates a BlockSparseMatrix representation of the jacobian for use in the
		// Schur complement based methods.
		public abstract SparseMatrix CreateJacobian();


	} // class Evaluator
	
	public Evaluator Create(EvaluatorOptions options, Program program, String error[]) {
		if (options.context == null) {
			System.err.println("options.context == null in public Evaluator Create");
			return null;
		}

		switch (options.linear_solver_type) {
		case DENSE_QR:
		case DENSE_NORMAL_CHOLESKY:
			DenseJacobianWriter dw = new DenseJacobianWriter(options, program);
			return new ProgramEvaluator<ScratchEvaluatePreparer, DenseJacobianWriter, NullJacobianFinalizer>(dw,
					options, program);
		case DENSE_SCHUR:
		case SPARSE_SCHUR:
		case ITERATIVE_SCHUR:
		case CGNR:
			BlockJacobianWriter bw = new BlockJacobianWriter(options, program);
			return new ProgramEvaluator<BlockEvaluatePreparer, BlockJacobianWriter, NullJacobianFinalizer>(bw,
					options, program);
		case SPARSE_NORMAL_CHOLESKY:
			if (options.dynamic_sparsity) {
				DynamicCompressedRowJacobianWriter dc = new DynamicCompressedRowJacobianWriter(options, program);
				return new ProgramEvaluator<ScratchEvaluatePreparer, DynamicCompressedRowJacobianWriter, DynamicCompressedRowJacobianFinalizer>(
						dc, options, program);
			} else {
				BlockJacobianWriter bw2 = new BlockJacobianWriter(options, program);
				return new ProgramEvaluator<BlockEvaluatePreparer, BlockJacobianWriter, NullJacobianFinalizer>(bw2,
						options, program);
			}

		default:
			error[0] = "Invalid Linear Solver Type. Unable to create evaluator.";
			return null;
		}
	}

	class DynamicCompressedRowJacobianFinalizer {
		/*
		 * void operator()(SparseMatrix* base_jacobian, int num_parameters) {
		 * DynamicCompressedRowSparseMatrix* jacobian =
		 * down_cast<DynamicCompressedRowSparseMatrix*>(base_jacobian);
		 * jacobian->Finalize(num_parameters); }
		 */
	};

	class NullJacobianFinalizer {
		public NullJacobianFinalizer() {

		}
		// void operator()(SparseMatrix* jacobian, int num_parameters) {}
	};

	class ScratchEvaluatePreparer {
		// Scratch space for the jacobians; each jacobian is packed one after another.
		// There is enough scratch to hold all the jacobians for the largest residual.
		// scoped_array<double> jacobian_scratch_;
		private double jacobian_scratch_[];

		public ScratchEvaluatePreparer() {

		}

		public void Init(int max_derivatives_per_residual_block) {
			jacobian_scratch_ = new double[max_derivatives_per_residual_block];
		}
		
		// Point the jacobian blocks into the scratch area of this evaluate preparer.
		public void Prepare(ResidualBlock residual_block,
		                                      int residual_block_index,
		                                      SparseMatrix jacobian,
		                                      double[][] jacobians) {
		  int i;
		  // residual_block_index and jacobian are not used in this routine
		  double[] jacobian_block_cursor = jacobian_scratch_;
		  int jacobian_block_cursor_index = 0;
		  int num_residuals = residual_block.NumResiduals();
		  int num_parameter_blocks = residual_block.NumParameterBlocks();
		  for (int j = 0; j < num_parameter_blocks; ++j) {
		    ParameterBlock parameter_block =
		        residual_block.parameter_blocks()[j];
		    if (parameter_block.IsConstant()) {
		      jacobians[j] = null;
		    } else {
		      jacobians[j] = new double[num_residuals * parameter_block.LocalSize()];
		      for (i = 0; i < num_residuals * parameter_block.LocalSize(); i++) {
		          jacobians[j][i] = jacobian_block_cursor[jacobian_block_cursor_index + i];
		      }
		      jacobian_block_cursor_index += num_residuals * parameter_block.LocalSize();
		    }
		  }
		}


	}

	class BlockEvaluatePreparer {
		private int jacobian_layout_[][];

		// For the case that the overall jacobian is not available, but the
		// individual jacobians are requested, use a pass-through scratch evaluate
		// preparer.
		private ScratchEvaluatePreparer scratch_evaluate_preparer_;

		public BlockEvaluatePreparer() {

		}

		public void Init(int jacobian_layout[][], int max_derivatives_per_residual_block) {
			jacobian_layout_ = jacobian_layout;
			scratch_evaluate_preparer_ = new ScratchEvaluatePreparer();
			scratch_evaluate_preparer_.Init(max_derivatives_per_residual_block);
		}
		
		public int[][] getJacobianLayout() {
			return jacobian_layout_;
		}
		
		// Point the jacobian blocks directly into the block sparse matrix.
		public void Prepare(ResidualBlock residual_block,
		                                    int residual_block_index,
		                                    SparseMatrix jacobian,
		                                    double[][] jacobians,
		                                    int[] jacobians_offset) {
		  int i;
		  // If the overall jacobian is not available, use the scratch space.
		  if (jacobian == null) {
		    scratch_evaluate_preparer_.Prepare(residual_block,
		                                       residual_block_index,
		                                       jacobian,
		                                       jacobians);
		    return;
		  }

		  double[] jacobian_values =
		      ((BlockSparseMatrix)jacobian).mutable_values();

		  int jacobian_block_offset[] = jacobian_layout_[residual_block_index];
		  int ptr = 0;
		  int num_parameter_blocks = residual_block.NumParameterBlocks();
		  for (int j = 0; j < num_parameter_blocks; ++j) {
		    if (!residual_block.parameter_blocks()[j].IsConstant()) {
                  jacobians[j] = jacobian_values;
                  jacobians_offset[j] = jacobian_block_offset[ptr++];

		      // The jacobian_block_offset can't be indexed with 'j' since the code
		      // that creates the layout strips out any blocks for inactive
		      // parameters. Instead, bump the pointer for active parameters only.
              
		    } else {
		      jacobians[j] = null;
		    }
		  }
		}

	}

	class DenseJacobianWriter {
		private Program program_;

		public DenseJacobianWriter(EvaluatorOptions options/* ignored */, Program program) {
			program_ = program;
		}

		// Since the dense matrix has different layout than that assumed by the cost
		// functions, use scratch space to store the jacobians temporarily then copy
		// them over to the larger jacobian later.
		public ScratchEvaluatePreparer[] CreateEvaluatePreparers(int num_threads) {
			return Create(program_, num_threads);
		}
		
		public SparseMatrix CreateJacobian() {
		    return new DenseSparseMatrix(program_.NumResiduals(),
		                                 program_.NumEffectiveParameters(),
		                                 true);
		  }
		
		public void Write(int residual_id,
	             int residual_offset,
	             double [][]jacobians,
	             SparseMatrix jacobian) {
		int r,c;
	    DenseSparseMatrix dense_jacobian = (DenseSparseMatrix)jacobian;
	    ResidualBlock residual_block =
	        program_.residual_blocks().get(residual_id);
	    int num_parameter_blocks = residual_block.NumParameterBlocks();
	    int num_residuals = residual_block.NumResiduals();

	    // Now copy the jacobians for each parameter into the dense jacobian matrix.
	    for (int j = 0; j < num_parameter_blocks; ++j) {
	      ParameterBlock parameter_block = residual_block.parameter_blocks()[j];

	      // If the parameter block is fixed, then there is nothing to do.
	      if (parameter_block.IsConstant()) {
	        continue;
	      }

	      int parameter_block_size = parameter_block.LocalSize();
	      //double parameter_jacobian[][] = new double[num_residuals][parameter_block_size];
	      for (r = 0; r < num_residuals; r++) {
	    	  for (c = 0; c < parameter_block_size; c++) {
	    		  dense_jacobian.setMatrix(r + residual_offset,
	    				  c + parameter_block.delta_offset(),jacobians[j][r*parameter_block_size+c]);
	    	  }
	      }
	    }
	  }

	} // class DenseJacobianWriter

	class DynamicCompressedRowJacobianWriter {
		private Program program_;

		public DynamicCompressedRowJacobianWriter(EvaluatorOptions options/* ignored */, Program program) {
			program_ = program;
		}

		// Since the dense matrix has different layout than that assumed by the cost
		// functions, use scratch space to store the jacobians temporarily then copy
		// them over to the larger jacobian later.
		public ScratchEvaluatePreparer[] CreateEvaluatePreparers(int num_threads) {
			return Create(program_, num_threads);
		}

	} // class DynamicCompressedRowJacobianWriter

	class BlockJacobianWriter {
		private Program program_;
		private Vector<int[]> jacobian_layout_;

		// The pointers in jacobian_layout_ point directly into this vector.
		private Vector<Integer> jacobian_layout_storage_;

		public BlockJacobianWriter(EvaluatorOptions options, Program program) {
			program_ = program;
			if (options.num_eliminate_blocks < 0) {
				System.err.println("num_eliminate_blocks must be >= 0 in public BlockJacobian.");
				return;
			}
			jacobian_layout_ = new Vector<int[]>();
			jacobian_layout_storage_ = new Vector<Integer>();
			BuildJacobianLayout(program, options.num_eliminate_blocks, jacobian_layout_, jacobian_layout_storage_);
		}

		public void BuildJacobianLayout(Program program, int num_eliminate_blocks, Vector<int[]> jacobian_layout,
				Vector<Integer> jacobian_layout_storage) {
			Vector<ResidualBlock> residual_blocks = program.residual_blocks();

			// Iterate over all the active residual blocks and determine how many E blocks
			// are there. This will determine where the F blocks start in the jacobian
			// matrix. Also compute the number of jacobian blocks.
			int f_block_pos = 0;
			int num_jacobian_blocks = 0;
			for (int i = 0; i < residual_blocks.size(); ++i) {
				ResidualBlock residual_block = residual_blocks.get(i);
				int num_residuals = residual_block.NumResiduals();
				int num_parameter_blocks = residual_block.NumParameterBlocks();

				// Advance f_block_pos over each E block for this residual.
				for (int j = 0; j < num_parameter_blocks; ++j) {
					ParameterBlock parameter_block = residual_block.parameter_blocks()[j];
					if (!parameter_block.IsConstant()) {
						// Only count blocks for active parameters.
						num_jacobian_blocks++;
						if (parameter_block.index() < num_eliminate_blocks) {
							f_block_pos += num_residuals * parameter_block.LocalSize();
						}
					}
				}
			}

			// We now know that the E blocks are laid out starting at zero, and the F
			// blocks are laid out starting at f_block_pos. Iterate over the residual
			// blocks again, and this time fill the jacobian_layout array with the
			// position information.

			while (jacobian_layout.size() > program.NumResidualBlocks()) {
				jacobian_layout.removeElementAt(jacobian_layout.size() - 1);
			}
			// jacobian_layout->resize(program.NumResidualBlocks());
			while (jacobian_layout_storage.size() > num_jacobian_blocks) {
				jacobian_layout_storage.removeElementAt(jacobian_layout_storage.size() - 1);
			}
			while (jacobian_layout_storage.size() < num_jacobian_blocks) {
				jacobian_layout_storage.add(0);
			}
			// jacobian_layout_storage->resize(num_jacobian_blocks);

			int e_block_pos = 0;
			for (int i = 0; i < residual_blocks.size(); ++i) {
				ResidualBlock residual_block = residual_blocks.get(i);
				int num_residuals = residual_block.NumResiduals();
				int num_parameter_blocks = residual_block.NumParameterBlocks();
				
				int k = 0;
				for (int j = 0; j < num_parameter_blocks; ++j) {
					ParameterBlock parameter_block = residual_block.parameter_blocks()[j];
					if (parameter_block.IsConstant()) {
						continue;
					}
					k++;
				}
				if (i < jacobian_layout_.size()) {
					jacobian_layout_.setElementAt(new int[k],i);
				}
				else {
					jacobian_layout_.add(new int[k]);
				}

				k = 0;
				for (int j = 0; j < num_parameter_blocks; ++j) {
					ParameterBlock parameter_block = residual_block.parameter_blocks()[j];
					int parameter_block_index = parameter_block.index();
					if (parameter_block.IsConstant()) {
						continue;
					}
					int jacobian_block_size = num_residuals * parameter_block.LocalSize();
					if (parameter_block_index < num_eliminate_blocks) {
						jacobian_layout.get(i)[k] = e_block_pos;
						e_block_pos += jacobian_block_size;
					} else {
						jacobian_layout.get(i)[k] = f_block_pos;
						f_block_pos += jacobian_block_size;
					}
					k++;
				}
			}
		}

		// Create evaluate prepareres that point directly into the final jacobian. This
		// makes the final Write() a nop.
		public BlockEvaluatePreparer[] CreateEvaluatePreparers(int num_threads) {
			int max_derivatives_per_residual_block = program_.MaxDerivativesPerResidualBlock();

			BlockEvaluatePreparer preparers[] = new BlockEvaluatePreparer[num_threads];
			for (int i = 0; i < num_threads; i++) {
				preparers[i] = new BlockEvaluatePreparer();
			    int jl[][] = new int[jacobian_layout_.size()][];
			    for (int k = 0; k < jacobian_layout_.size(); k++) {
			    	jl[k] = jacobian_layout_.get(k);
			    }
				preparers[i].Init(jl, max_derivatives_per_residual_block);
			}
			return preparers;
		}
		
		public SparseMatrix CreateJacobian() {
			  CompressedRowBlockStructure bs = new CompressedRowBlockStructure();

			  Vector<ParameterBlock> parameter_blocks =
			      program_.parameter_blocks();

			  // Construct the column blocks.
			  while (bs.cols.size() < parameter_blocks.size()) {
				  bs.cols.add(new Block());
			  }
			  while (bs.cols.size() > parameter_blocks.size()) {
				  bs.cols.removeElementAt(bs.cols.size()-1);
			  }
			  for (int i = 0, cursor = 0; i < parameter_blocks.size(); ++i) {
				if (parameter_blocks.get(i).index() == -1) {
					System.err.println("parameter_blocks.get("+i+").index() == -1 in BlockJacobianWriter CreateJacobian");
					return null;
				}
			    if (parameter_blocks.get(i).IsConstant()) {
			    	System.err.println("parameter_blocks.get("+i+").IsConstant() in BlockJacobianWrtier CreateJacobian");
			    	return null;
			    }
			    bs.cols.get(i).size = parameter_blocks.get(i).LocalSize();
			    bs.cols.get(i).position = cursor;
			    cursor += bs.cols.get(i).size;
			  }

			  // Construct the cells in each row.
			  Vector<ResidualBlock> residual_blocks = program_.residual_blocks();
			  int row_block_position = 0;
			  while (bs.rows.size() < residual_blocks.size()) {
				  bs.rows.add(new CompressedList());
			  }
			  while (bs.rows.size() > residual_blocks.size()) {
				  bs.rows.removeElementAt(bs.rows.size()-1);
			  }
			  for (int i = 0; i < residual_blocks.size(); ++i) {
			    ResidualBlock residual_block = residual_blocks.get(i);
			    CompressedList row = bs.rows.get(i);

			    if (row.block == null) {
			    	row.block = new Block();
			    }
			    row.block.size = residual_block.NumResiduals();
			    row.block.position = row_block_position;
			    row_block_position += row.block.size;

			    // Size the row by the number of active parameters in this residual.
			    int num_parameter_blocks = residual_block.NumParameterBlocks();
			    int num_active_parameter_blocks = 0;
			    for (int j = 0; j < num_parameter_blocks; ++j) {
			      if (residual_block.parameter_blocks()[j].index() != -1) {
			        num_active_parameter_blocks++;
			      }
			    }
			    while (row.cells.size() < num_active_parameter_blocks) {
			    	row.cells.add(new Cell());
			    }
			    while (row.cells.size() > num_active_parameter_blocks) {
			    	row.cells.removeElementAt(row.cells.size()-1);
			    }

			    // Add layout information for the active parameters in this row.
			    for (int j = 0, k = 0; j < num_parameter_blocks; ++j) {
			      ParameterBlock parameter_block =
			          residual_block.parameter_blocks()[j];
			      if (!parameter_block.IsConstant()) {
			        Cell cell = row.cells.get(k);
			        cell.block_id = parameter_block.index();
			        cell.position = jacobian_layout_.get(i)[k];

			        // Only increment k for active parameters, since there is only layout
			        // information for active parameters.
			        k++;
			      }
			    }
			    
			    

				CellLessThan CD = new CellLessThan();
				Collections.sort(row.cells, CD);
			  }

			  BlockSparseMatrix jacobian = new BlockSparseMatrix(bs);
			  if (jacobian == null) {
				  System.err.println("jacobian == null in BlockJacobianWriter CreateJacobian");
			  }
			  return jacobian;
			}
		
		public void Write(int residual_id,
	             int residual_offset,
	             double[][] jacobians,
	             SparseMatrix  jacobian ) {
	    // This is a noop since the blocks were written directly into their final
	    // position by the outside evaluate call, thanks to the jacobians array
	    // prepared by the BlockEvaluatePreparers.
	  }


	} // class BlockJacobianWriter
	
	class CellLessThan implements Comparator<Cell> {
		
		
		public int compare( Cell lhs, Cell rhs) {
			  if (lhs.block_id == rhs.block_id) {
			    return (lhs.position  - rhs.position);
			  }
			  return (lhs.block_id - rhs.block_id);
			}

	} // class VertexDegreesLessThan

	public ScratchEvaluatePreparer[] Create(Program program, int num_threads) {
		ScratchEvaluatePreparer preparers[] = new ScratchEvaluatePreparer[num_threads];
		int max_derivatives_per_residual_block = program.MaxDerivativesPerResidualBlock();
		for (int i = 0; i < num_threads; i++) {
			preparers[i] = new ScratchEvaluatePreparer();
			preparers[i].Init(max_derivatives_per_residual_block);
		}
		return preparers;
	}

	class CallStatistics {
		public double time;
		public int calls;

		public CallStatistics() {
			time = 0;
			calls = 0;
		}

	};

	// Struct used by various objects to report statistics about their
	// execution.
	class ExecutionSummary {
		private Lock mutex_;
		private HashMap<String, CallStatistics> statistics_;
		
		public ExecutionSummary() {
			statistics_ = new HashMap<String, CallStatistics>();
		}

		public void IncrementTimeBy(String name, double value) {
			mutex_.lock();
			CallStatistics call_stats = statistics_.get(name);
			call_stats.time += value;
			++call_stats.calls;
			mutex_.unlock();
		}

		public HashMap<String, CallStatistics> statistics() {
			return statistics_;
		}

	} // class ExecutionSummary

	class ScopedExecutionTimer {
		private double start_time_;
		private String name_;
		private ExecutionSummary summary_;

		public ScopedExecutionTimer(String name, ExecutionSummary summary) {
			start_time_ = 1.0E-3 * System.currentTimeMillis();
			name_ = name;
			summary_ = summary;
		}

		public void finalize() {
			summary_.IncrementTimeBy(name_, 1.0E-3 * System.currentTimeMillis() - start_time_);
		}

	} // class ScopedExecutionTimer


	/*
	 * template<typename EvaluatePreparer, typename JacobianWriter, typename
	 * JacobianFinalizer = NullJacobianFinalizer>
	 */
	class ProgramEvaluator<EvaluatePreparer, JacobianWriter, JacobianFinalizer> extends Evaluator {

		private EvaluatorOptions options_;
		private Program program_;
		private JacobianWriter jacobian_writer_;
		// scoped_array<EvaluatePreparer> evaluate_preparers_;
		private EvaluatePreparer evaluate_preparers_[];
		// scoped_array<EvaluateScratch> evaluate_scratch_;
		private EvaluateScratch evaluate_scratch_[];
		private Vector<Integer> residual_layout_;
		private ExecutionSummary execution_summary_;

		public ProgramEvaluator(JacobianWriter jw, EvaluatorOptions options, Program program) {
			super();
			options_ = options;
			program_ = program;
			jacobian_writer_ = jw;
			if ((options_.linear_solver_type == LinearSolverType.DENSE_QR) ||
					(options_.linear_solver_type == LinearSolverType.DENSE_NORMAL_CHOLESKY)) {
					evaluate_preparers_ = (EvaluatePreparer[])((DenseJacobianWriter)jacobian_writer_).CreateEvaluatePreparers(options.num_threads);
				}
				else if ((options_.linear_solver_type == LinearSolverType.CGNR) || 
						 (options_.linear_solver_type == LinearSolverType.ITERATIVE_SCHUR) ||
						 (options_.linear_solver_type == LinearSolverType.DENSE_SCHUR)) {
					evaluate_preparers_ = (EvaluatePreparer[])((BlockJacobianWriter)jacobian_writer_).CreateEvaluatePreparers(options.num_threads);
				}
			/*
			 * #ifdef CERES_NO_THREADS if (options_.num_threads > 1) { LOG(WARNING) <<
			 * "Neither OpenMP nor TBB support is compiled into this binary; " <<
			 * "only options.num_threads = 1 is supported. Switching " <<
			 * "to single threaded mode."; options_.num_threads = 1; } #endif //
			 * CERES_NO_THREADS
			 */

			residual_layout_ = new Vector<Integer>();
			BuildResidualLayout(program, residual_layout_);
			evaluate_scratch_ = CreateEvaluatorScratch(program, options.num_threads);
		}
		
		// Implementation of Evaluator interface.
		  public SparseMatrix CreateJacobian() {
			if ((options_.linear_solver_type == LinearSolverType.DENSE_QR) ||
				(options_.linear_solver_type == LinearSolverType.DENSE_NORMAL_CHOLESKY)) {
				return ((DenseJacobianWriter)jacobian_writer_).CreateJacobian();
			}
			else if ((options_.linear_solver_type == LinearSolverType.CGNR) ||
					 (options_.linear_solver_type == LinearSolverType.ITERATIVE_SCHUR) ||
					 (options_.linear_solver_type == LinearSolverType.DENSE_SCHUR)) {
				return ((BlockJacobianWriter)jacobian_writer_).CreateJacobian();
			}
			else {
				System.err.println("In ProgramEvaluator CreateJacobian options_.linear_solver_type = " +
			                          LinearSolverTypeToString(options_.linear_solver_type));
				return null;
			}
		  }
		  
		  public int NumParameters() {
			    return program_.NumParameters();
		  }
		  
		  public int NumEffectiveParameters() {
			    return program_.NumEffectiveParameters();
		  }

		  public int NumResiduals() {
			    return program_.NumResiduals();
		  }
		  
		  public boolean Plus(Vector<Double> state,
		                      Vector<Double> delta,
		            Vector<Double> state_plus_delta) {
		    return program_.Plus(state, delta, state_plus_delta);
		  }
		  
		  public boolean Plus(double[] state, double[] delta, double[] state_plus_delta) {
			  int i;
			  boolean cond;
			  Vector<Double>stateVec = new Vector<Double>(state.length);
			  for (i = 0; i < state.length; i++) {
				  stateVec.add(state[i]);
			  }
			  Vector<Double>deltaVec = new Vector<Double>(delta.length);
			  for (i = 0; i < delta.length; i++) {
				  deltaVec.add(delta[i]);
			  }
			  Vector<Double>state_plus_deltaVec = new Vector<Double>(state_plus_delta.length);
			  for (i = 0; i < state_plus_delta.length; i++) {
				  state_plus_deltaVec.add(state_plus_delta[i]);
			  }
			  cond = program_.Plus(stateVec, deltaVec, state_plus_deltaVec);
			  for (i = 0; i < state_plus_delta.length; i++) {
				  state_plus_delta[i] = state_plus_deltaVec.get(i);
			  }
			  return cond;
		  }
		  
		  public boolean Evaluate(EvaluateOptions evaluate_options,
				  double[] state_array, double[] cost, double residuals_array[], double[] gradient_array, SparseMatrix jacobian) {
			  int i;
			  boolean status;
			  Vector<Double>residuals = null;
			  Vector<Double>gradient = null;
			  Vector<Double>state = new Vector<Double>();
			  for (i = 0; i < state_array.length; i++) {
				  state.add(state_array[i]);
			  }
			  if (residuals_array != null) {
				  residuals = new Vector<Double>();
				  for (i = 0; i < residuals_array.length; i++) {
					  residuals.add(residuals_array[i]);
				  }
			  }
			  if (gradient_array != null) {
				  gradient = new Vector<Double>();
				  for (i = 0; i < gradient_array.length; i++) {
					  gradient.add(gradient_array[i]);
				  }
			  }
			  status = Evaluate(evaluate_options, state, cost, residuals, gradient, jacobian);
			  for (i = 0; i < state_array.length; i++) {
				  state_array[i] = state.get(i);
			  }
			  if (residuals_array != null) {
				  for (i = 0; i < residuals_array.length; i++) {
					  residuals_array[i] = residuals.get(i);
				  }
			  }
			  if (gradient_array != null) {
				  for (i = 0; i < gradient_array.length; i++) {
					  gradient_array[i] = gradient.get(i);
				  }
			  }
			  return status;
		  }
		  
		  public boolean Evaluate(EvaluateOptions evaluate_options,
	                Vector<Double> state,
	                double[] cost,
	                Vector<Double> residuals,
	                Vector<Double> gradient,
	                SparseMatrix jacobian) {
	    int i,j;
	    ScopedExecutionTimer total_timer = new ScopedExecutionTimer("Evaluator::Total", execution_summary_);
	    ScopedExecutionTimer call_type_timer;
	    if ((gradient == null) && (jacobian == null)) {
	    	call_type_timer = new ScopedExecutionTimer("Evaluator::Residual", execution_summary_);
	    }
	    else {
	    	call_type_timer = new ScopedExecutionTimer("Evaluator::Jacobian", execution_summary_);
	    }
	    // The parameters are stateful, so set the state before evaluating.
	    if (!program_.StateVectorToParameterBlocks(state)) {
	      return false;
	    }

	    // Notify the user about a new evaluation point if they are interested.
	    if (options_.evaluation_callback != null) {
	      program_.CopyParameterBlockStateToUserState();
	      options_.evaluation_callback.PrepareForEvaluation(
	          (gradient != null || jacobian != null),
	          evaluate_options.new_evaluation_point);
	    }

	    if (residuals != null) {
	      for (i = 0; i < program_.NumResiduals(); i++) {
	    	  residuals.set(i,0.0);
	      }
	    }

	    if (jacobian != null) {
	      jacobian.SetZero();
	    }

	    // Each thread gets it's own cost and evaluate scratch space.
	    for (i = 0; i < options_.num_threads; ++i) {
	      evaluate_scratch_[i].cost = 0.0;
	      if (gradient != null) {
	    	for (j = 0; j < program_.NumEffectiveParameters(); j++) {
	    		evaluate_scratch_[i].gradient[j] = 0.0;
	    	}
	      }
	    }

	    int num_residual_blocks = program_.NumResidualBlocks();

	//#if !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))
	//    ThreadTokenProvider thread_token_provider(options_.num_threads);
	//#endif // !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))

	//#ifdef CERES_USE_OPENMP
	    // This bool is used to disable the loop if an error is encountered
	    // without breaking out of it. The remaining loop iterations are still run,
	    // but with an empty body, and so will finish quickly.
	    //bool abort = false;
	//#pragma omp parallel for num_threads(options_.num_threads)
	    //for (int i = 0; i < num_residual_blocks; ++i) {
	// Disable the loop instead of breaking, as required by OpenMP.
	//#pragma omp flush(abort)
	//#endif // CERES_USE_OPENMP

	//#ifdef CERES_NO_THREADS
	    boolean abort = false;
	    for (i = 0; i < num_residual_blocks; ++i) {
	//#endif // CERES_NO_THREADS

	//#if defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
	//    std::atomic_bool abort(false);

	  //  ParallelFor(options_.context,
	  //              0,
	  //              num_residual_blocks,
	  //              options_.num_threads,
	  //              [&](int thread_id, int i) {
	//#endif // defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)

	      if (abort) {
	//#if defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
	        //return;
	//#else
	        continue;
	//#endif // defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
	      }

	//#if !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))
	      //const ScopedThreadToken scoped_thread_token(&thread_token_provider);
	      //const int thread_id = scoped_thread_token.token();
	//#endif  // !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))

//	      EvaluatePreparer preparer = &evaluate_preparers_[thread_id];
//	      EvaluateScratch* scratch = &evaluate_scratch_[thread_id];
	      EvaluatePreparer preparer = evaluate_preparers_[0];
	      EvaluateScratch scratch = evaluate_scratch_[0];

	      // Prepare block residuals if requested.
	      ResidualBlock residual_block = program_.residual_blocks().get(i);
	      double block_residuals[] = null;
	      if (residuals != null) {
	    	block_residuals = new double[residuals.size() - 1 - residual_layout_.get(i) + 1];
	    	for (j = residual_layout_.get(i); j < residuals.size(); j++) {
	    		block_residuals[j-residual_layout_.get(i)] = residuals.get(j);
	    	}
	      } else if (gradient != null) {
	    	block_residuals = new double[scratch.residual_block_residuals.length];
	    	for (j = 0; j < scratch.residual_block_residuals.length; j++) {
	    	    block_residuals[j] = scratch.residual_block_residuals[j];	
	    	}
	      }

	      // Prepare block jacobians if requested.
	      double[][] block_jacobians = null;
	      int block_jacobians_offset[] = new int[scratch.jacobian_block_ptrs.length];
	      if (jacobian != null || gradient != null) {
	    	  if ((options_.linear_solver_type == LinearSolverType.DENSE_QR) ||
	  				(options_.linear_solver_type == LinearSolverType.DENSE_NORMAL_CHOLESKY)) {
	        ((ScratchEvaluatePreparer)preparer).Prepare(residual_block,
	                          i,
	                          jacobian,
	                          scratch.jacobian_block_ptrs);
	    	  }
	    	  else if ((options_.linear_solver_type == LinearSolverType.CGNR) ||
	    			   (options_.linear_solver_type == LinearSolverType.ITERATIVE_SCHUR) ||
	    			   (options_.linear_solver_type == LinearSolverType.DENSE_SCHUR)) {
	    		  ((BlockEvaluatePreparer)preparer).Prepare(residual_block,
                          i,
                          jacobian,
                          scratch.jacobian_block_ptrs,
                          block_jacobians_offset);	  
	    	  }
	        block_jacobians = scratch.jacobian_block_ptrs;
	      }

	      // Evaluate the cost, residuals, and jacobians.
	      double block_cost[] = new double[1];
	      if (!residual_block.Evaluate(
	              evaluate_options.apply_loss_function,
	              block_cost,
	              block_residuals,
	              block_jacobians,
	              block_jacobians_offset,
	              scratch.residual_block_evaluate_scratch)) {
	        abort = true;
	//#ifdef CERES_USE_OPENMP
	// This ensures that the OpenMP threads have a consistent view of 'abort'. Do
	// the flush inside the failure case so that there is usually only one
	// synchronization point per loop iteration instead of two.
	//#pragma omp flush(abort)
	//#endif // CERES_USE_OPENMP

	//#if defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
	//        return;
	//#else
	        continue;
	//#endif // defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
	      }

	      scratch.cost += block_cost[0];

	      // Store the jacobians, if they were requested.
	      if (jacobian != null) {
	    	  if ((options_.linear_solver_type == LinearSolverType.DENSE_QR) ||
		  				(options_.linear_solver_type == LinearSolverType.DENSE_NORMAL_CHOLESKY)) {
	        ((DenseJacobianWriter)jacobian_writer_).Write(i,
	                               residual_layout_.get(i),
	                               block_jacobians,
	                               jacobian);
	    	  }
	    	  // Write is a noop for BlockJacobianWriter since the blocks were written directly into their final
	    	    // position by the outside evaluate call, thanks to the jacobians array
	    	    // prepared by the BlockEvaluatePreparers.
	    	  else if ((options_.linear_solver_type == LinearSolverType.CGNR) ||
	    			   (options_.linear_solver_type == LinearSolverType.ITERATIVE_SCHUR) ||
	    			   (options_.linear_solver_type == LinearSolverType.DENSE_SCHUR)) {
	    	  }

	      }

	      // Compute and store the gradient, if it was requested.
	      if (gradient != null) {
	        int num_residuals = residual_block.NumResiduals();
	        int num_parameter_blocks = residual_block.NumParameterBlocks();
	        for (j = 0; j < num_parameter_blocks; ++j) {
	          ParameterBlock parameter_block =
	              residual_block.parameter_blocks()[j];
	          if (parameter_block.IsConstant()) {
	            continue;
	          }

	          MatrixTransposeVectorMultiply(DYNAMIC, DYNAMIC, 1,
	              block_jacobians[j], block_jacobians_offset[j],
	              num_residuals,
	              parameter_block.LocalSize(),
	              block_residuals, 0,
	              scratch.gradient,parameter_block.delta_offset());
	        } // for (j = 0; j < num_parameter_blocks; ++j)
	      } // if (gradient != null)
	      if (residuals != null) {
		    	for (j = residual_layout_.get(i); j < residuals.size(); j++) {
		    		residuals.set(j,block_residuals[j-residual_layout_.get(i)]);
		    	}
		      } else if (gradient != null) {
		    	for (j = 0; j < scratch.residual_block_residuals.length; j++) {
		    	    scratch.residual_block_residuals[j] = block_residuals[j];	
		    	}
		      }
	    } // for (i = 0; i < num_residual_blocks; ++i)
	//#if defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
	 //   );
	//#endif // defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)

	    if (!abort) {
	      int num_parameters = program_.NumEffectiveParameters();

	      // Sum the cost and gradient (if requested) from each thread.
	      cost[0] = 0.0;
	      if (gradient != null) {
	    	for (i = 0; i < num_parameters; i++) {
	    		gradient.set(i,0.0);
	    	}
	      }
	      for (i = 0; i < options_.num_threads; ++i) {
	        cost[0] += evaluate_scratch_[i].cost;
	        if (gradient != null) {
	          for (j = 0; j < num_parameters; j++) {
	        	  gradient.set(j,gradient.get(j) + evaluate_scratch_[i].gradient[j]);
	          }
	        }
	      }

	      // Finalize the Jacobian if it is available.
	      // `num_parameters` is passed to the finalizer so that additional
	      // storage can be reserved for additional diagonal elements if
	      // necessary.
	      if (jacobian != null) {
	    	//Not presently implemented in NullJacobianFinalizer
	        //JacobianFinalizer f;
	        //f(jacobian, num_parameters);
	      }
	    }
	    return !abort;
	  }
		 

	} // class ProgramEvaluator

	class EvaluateScratch {
		private double cost;
		// scoped_array<double> residual_block_evaluate_scratch;
		private double residual_block_evaluate_scratch[];
		// The gradient in the local parameterization.
		// scoped_array<double> gradient;
		private double gradient[];
		// Enough space to store the residual for the largest residual block.
		// scoped_array<double> residual_block_residuals;
		private double residual_block_residuals[];
		// scoped_array<double*> jacobian_block_ptrs;
		private double jacobian_block_ptrs[][];

		public EvaluateScratch() {

		}

		public void Init(int max_parameters_per_residual_block, int max_scratch_doubles_needed_for_evaluate,
				int max_residuals_per_residual_block, int num_parameters) {
			residual_block_evaluate_scratch = new double[max_scratch_doubles_needed_for_evaluate];
			gradient = new double[num_parameters];
			residual_block_residuals = new double[max_residuals_per_residual_block];
			jacobian_block_ptrs = new double[max_parameters_per_residual_block][];
		}

	};

	// Create scratch space for each thread evaluating the program.
	public EvaluateScratch[] CreateEvaluatorScratch(Program program, int num_threads) {
		int max_parameters_per_residual_block = program.MaxParametersPerResidualBlock();
		int max_scratch_doubles_needed_for_evaluate = program.MaxScratchDoublesNeededForEvaluate();
		int max_residuals_per_residual_block = program.MaxResidualsPerResidualBlock();
		int num_parameters = program.NumEffectiveParameters();

		EvaluateScratch evaluate_scratch[] = new EvaluateScratch[num_threads];
		for (int i = 0; i < num_threads; i++) {
			evaluate_scratch[i] = new EvaluateScratch();
			evaluate_scratch[i].Init(max_parameters_per_residual_block, max_scratch_doubles_needed_for_evaluate,
					max_residuals_per_residual_block, num_parameters);
		}
		return evaluate_scratch;
	}

	public void BuildResidualLayout(Program program, Vector<Integer> residual_layout) {
		Vector<ResidualBlock> residual_blocks = program.residual_blocks();
		while (residual_layout.size() > program.NumResidualBlocks()) {
			residual_layout.removeElementAt(residual_layout.size() - 1);
		}
		while (residual_layout.size() < program.NumResidualBlocks()) {
			residual_layout.add(0);
		}
		int residual_pos = 0;
		for (int i = 0; i < residual_blocks.size(); ++i) {
			int num_residuals = residual_blocks.get(i).NumResiduals();
			residual_layout.set(i, residual_pos);
			residual_pos += num_residuals;
		}
	}
	
	// This struct describes the state of the optimizer after each
	// iteration of the minimization.
	 class IterationSummary {
		// Current iteration number.
		  public int iteration;

		  // Step was numerically valid, i.e., all values are finite and the
		  // step reduces the value of the linearized model.
		  //
		  // Note: step_is_valid is always true when iteration = 0.
		  public boolean step_is_valid;

		  // Step did not reduce the value of the objective function
		  // sufficiently, but it was accepted because of the relaxed
		  // acceptance criterion used by the non-monotonic trust region
		  // algorithm.
		  //
		  // Note: step_is_nonmonotonic is always false when iteration = 0;
		  public boolean step_is_nonmonotonic;

		  // Whether or not the minimizer accepted this step or not. If the
		  // ordinary trust region algorithm is used, this means that the
		  // relative reduction in the objective function value was greater
		  // than Solver::Options::min_relative_decrease. However, if the
		  // non-monotonic trust region algorithm is used
		  // (Solver::Options:use_nonmonotonic_steps = true), then even if the
		  // relative decrease is not sufficient, the algorithm may accept the
		  // step and the step is declared successful.
		  //
		  // Note: step_is_successful is always true when iteration = 0.
		  public boolean step_is_successful;

		  // Value of the objective function.
		  public double cost;

		  // Change in the value of the objective function in this
		  // iteration. This can be positive or negative.
		  public double cost_change;

		  // Infinity norm of the gradient vector.
		  public double gradient_max_norm;

		  // 2-norm of the gradient vector.
		  public double gradient_norm;

		  // 2-norm of the size of the step computed by the optimization
		  // algorithm.
		  public double step_norm;

		  // For trust region algorithms, the ratio of the actual change in
		  // cost and the change in the cost of the linearized approximation.
		  public double relative_decrease;

		  // Size of the trust region at the end of the current iteration. For
		  // the Levenberg-Marquardt algorithm, the regularization parameter
		  // mu = 1.0 / trust_region_radius.
		  public double trust_region_radius;

		  // For the inexact step Levenberg-Marquardt algorithm, this is the
		  // relative accuracy with which the Newton(LM) step is solved. This
		  // number affects only the iterative solvers capable of solving
		  // linear systems inexactly. Factorization-based exact solvers
		  // ignore it.
		  public double eta;

		  // Step sized computed by the line search algorithm.
		  public double step_size;

		  // Number of function value evaluations used by the line search algorithm.
		  public int line_search_function_evaluations;

		  // Number of function gradient evaluations used by the line search algorithm.
		  public int line_search_gradient_evaluations;

		  // Number of iterations taken by the line search algorithm.
		  public int line_search_iterations;

		  // Number of iterations taken by the linear solver to solve for the
		  // Newton step.
		  public int linear_solver_iterations;

		  // All times reported below are wall times.

		  // Time (in seconds) spent inside the minimizer loop in the current
		  // iteration.
		  public double iteration_time_in_seconds;

		  // Time (in seconds) spent inside the trust region step solver.
		  public double step_solver_time_in_seconds;

		  // Time (in seconds) since the user called Solve().
		  public double cumulative_time_in_seconds;
		  
	      public IterationSummary() {
	        iteration = 0;
	        step_is_valid = false;
	        step_is_nonmonotonic = false;
	        step_is_successful = false;
	        cost = 0.0;
	        cost_change = 0.0;
	        gradient_max_norm = 0.0;
	        gradient_norm = 0.0;
	        step_norm = 0.0;
	        eta = 0.0;
	        step_size = 0.0;
	        line_search_function_evaluations = 0;
	        line_search_gradient_evaluations = 0;
	        line_search_iterations = 0;
	        linear_solver_iterations = 0;
	        iteration_time_in_seconds = 0.0;
	        step_solver_time_in_seconds = 0.0;
	        cumulative_time_in_seconds = 0.0;  
	      }

	  
	} // class IterationSummary

	// Callback for logging the state of the minimizer to STDERR or
	// STDOUT depending on the user's preferences and logging level.
	class LoggingCallback extends IterationCallback {
		private MinimizerType minimizer_type;
		private boolean log_to_stdout_;

		public LoggingCallback(MinimizerType minimizer_type, boolean log_to_stdout) {
			this.minimizer_type = minimizer_type;
			log_to_stdout_ = log_to_stdout;
		}
		
		
		public CallbackReturnType operator(
			    IterationSummary summary) {
			  String output = null;
			  if (minimizer_type == MinimizerType.LINE_SEARCH) {
			    output =  String.format(
			        "% 4d: f:% 8e d:% 3.2e g:% 3.2e h:% 3.2e " +
			        "s:% 3.2e e:% 3d it:% 3.2e tt:% 3.2e",
			                          summary.iteration,
			                          summary.cost,
			                          summary.cost_change,
			                          summary.gradient_max_norm,
			                          summary.step_norm,
			                          summary.step_size,
			                          summary.line_search_function_evaluations,
			                          summary.iteration_time_in_seconds,
			                          summary.cumulative_time_in_seconds);
			  } else if (minimizer_type == MinimizerType.TRUST_REGION) {
			    if (summary.iteration == 0) {
			      output = "iter      cost      cost_change  |gradient|   |step|    tr_ratio  tr_radius  ls_iter  iter_time  total_time\n" +
			    		  String.format(
			  			        "% 4d % 8e   % 3.2e   % 3.2e  % 3.2e  % 3.2e % 3.2e     % 4d   % 3.2e   % 3.2e",
			  			                           summary.iteration,
			  			                           summary.cost,
			  			                           summary.cost_change,
			  			                           summary.gradient_max_norm,
			  			                           summary.step_norm,
			  			                           summary.relative_decrease,
			  			                           summary.trust_region_radius,
			  			                           summary.linear_solver_iterations,
			  			                           summary.iteration_time_in_seconds,
			  			                           summary.cumulative_time_in_seconds);;
			    }
			    else {
			      output  = String.format(
			        "% 4d % 8e   % 3.2e   % 3.2e  % 3.2e  % 3.2e % 3.2e     % 4d   % 3.2e   % 3.2e",
			                           summary.iteration,
			                           summary.cost,
			                           summary.cost_change,
			                           summary.gradient_max_norm,
			                           summary.step_norm,
			                           summary.relative_decrease,
			                           summary.trust_region_radius,
			                           summary.linear_solver_iterations,
			                           summary.iteration_time_in_seconds,
			                           summary.cumulative_time_in_seconds);
			    }
			  } else {
			    System.err.println("Unknown minimizer type.");
			  }

			  if (log_to_stdout_) {
			    Preferences.debug(output + "\n", Preferences.DEBUG_ALGORITHM);
			  } else {
				if (1 <= MAX_LOG_LEVEL) {
					Preferences.debug(output + "\n", Preferences.DEBUG_ALGORITHM);	
				}
			  }
			  return CallbackReturnType.SOLVER_CONTINUE;
			}

	};

	// Callback for updating the externally visible state of parameter
	// blocks.
	class StateUpdatingCallback extends IterationCallback {
		private Program program_;
		private Vector<Double> parameters_;

		public StateUpdatingCallback(Program program, Vector<Double> parameters) {
			program_ = program;
			parameters_ = parameters;
		}

		public CallbackReturnType operator(
			    IterationSummary summary) {
			  program_.StateVectorToParameterBlocks(parameters_);
			  program_.CopyParameterBlockStateToUserState();
			  return CallbackReturnType.SOLVER_CONTINUE;
			}

	};
	
	// Callback for updating the externally visible state of the
	// parameters vector for GradientProblemSolver.
	class GradientProblemSolverStateUpdatingCallback extends IterationCallback {
		 private int num_parameters_;
		 private double[] internal_parameters_;
		 private double[] user_parameters_;
	 public GradientProblemSolverStateUpdatingCallback(int num_parameters,
	                                             double[] internal_parameters,
	                                             double[] user_parameters) {
		  num_parameters_ = num_parameters;
	      internal_parameters_ = internal_parameters;
	      user_parameters_ = user_parameters; 	 
	 }
	 
	 public CallbackReturnType operator(
			    IterationSummary summary) {
		      int i;
			  if (summary.step_is_successful) {
				  for (i = 0; i < num_parameters_; i++) {
					  user_parameters_[i] = internal_parameters_[i];
				  }
				 
			  }
			  return CallbackReturnType.SOLVER_CONTINUE;
	 }
	 }
	
	public boolean IsOrderingValid(
		    Program program,
		    OrderedGroups<double[]> ordering,
		    String[] message) {
		  HashMap<Integer, Set<double[]> > group_to_elements =
		      ordering.group_to_elements();

		  // Verify that each group is an independent set
		    Collection<Set<double[]>> col = group_to_elements.values();
			Iterator<Set<double[]>> group_it = col.iterator();
			Set<Integer> groupNumSet = group_to_elements.keySet();
			Iterator<Integer> num_iterator = groupNumSet.iterator();
			while (group_it.hasNext()) {
				Set<double[]> group = group_it.next();
				int groupNum = num_iterator.next();
				
		    if (!program.IsParameterBlockSetIndependent(group)) {
		      message[0] =
		          String.format("The user-provided \n" +
		                       "parameter_blocks_for_inner_iterations does not \n" +
		                       "form an independent set. Group Id: %d", groupNum);
		      return false;
		    }
		  }
		  return true;
		}
	
	// Find a recursive decomposition of the Hessian matrix as a set
	// of independent sets of decreasing size and invert it. This
	// seems to work better in practice, i.e., Cameras before
	// points.
	public OrderedGroups<double[]> CreateOrdering(
	    Program program) {
		OrderedGroups<double[]> ordering = new OrderedGroups<double[]>();
	    ComputeRecursiveIndependentSetOrdering(program, ordering);
	    ordering.Reverse();
	    return ordering;
	}
	
	public void ComputeRecursiveIndependentSetOrdering(Program program,
            OrderedGroups<double[]> ordering) {
		    if (ordering == null) {
		    	System.err.println("OrderedGroups<double[]> ordering == null in ComputeRecursiveIndependentSetOrdering");
		    	return;
		    }
			ordering.Clear();
			Vector<ParameterBlock> parameter_blocks = program.parameter_blocks();
			Graph<ParameterBlock> graph = CreateHessianGraph(program);
			
			int num_covered = 0;
			int round = 0;
			while (num_covered < parameter_blocks.size()) {
			     Vector<ParameterBlock> independent_set_ordering = new Vector<ParameterBlock>();
			     int independent_set_size = IndependentSetOrdering(graph, independent_set_ordering);
			for (int i = 0; i < independent_set_size; ++i) {
			    ParameterBlock parameter_block = independent_set_ordering.get(i);
			    ordering.AddElementToGroup(parameter_block.mutable_user_state(), round);
			    graph.RemoveVertex(parameter_block);
			}
			num_covered += independent_set_size;
			++round;
			}
	}




	class CoordinateDescentMinimizer extends Minimizer {
		Vector<ParameterBlock> parameter_blocks_;
		Vector<Vector<ResidualBlock>> residual_blocks_;
		// The optimization is performed in rounds. In each round all the
		// parameter blocks that form one independent set are optimized in
		// parallel. This array, marks the boundaries of the independent
		// sets in parameter_blocks_.
		Vector<Integer> independent_set_offsets_;

		EvaluatorOptions evaluator_options_;

		Context context_;

		public CoordinateDescentMinimizer(Context context) {
			super();
			if (context == null) {
				System.err.println("Context context == null in public CoordinateDescentMinimizer");
				return;
			}
			context_ = context;
			parameter_blocks_ = new Vector<ParameterBlock>();
			residual_blocks_ = new Vector<Vector<ResidualBlock>>();
			independent_set_offsets_ = new Vector<Integer>();
			evaluator_options_ = new EvaluatorOptions();
		}
		
		public boolean Init(
			    Program program,
			    HashMap<double[], ParameterBlock> parameter_map,
			    OrderedGroups<double[]> ordering,
			    String error[]) {
			  int i,j;
			  parameter_blocks_.clear();
			  independent_set_offsets_.clear();
			  independent_set_offsets_.add(0);

			  // Serialize the OrderedGroups into a vector of parameter block
			  // offsets for parallel access.
			  HashMap<ParameterBlock, Integer> parameter_block_index = new HashMap<ParameterBlock, Integer>();
			  HashMap<Integer, Set<double[]> > group_to_elements = ordering.group_to_elements();
			  for (Set<double[]> collectionValues : group_to_elements.values()) {
			      for (double[] values : collectionValues) {
			    	  parameter_blocks_.add(parameter_map.get(values));
			    	  parameter_block_index.put(parameter_blocks_.get(parameter_blocks_.size()-1),parameter_blocks_.size()-1); 
			      }
			      independent_set_offsets_.add(independent_set_offsets_.size()-1 + collectionValues.size());
			  }

			  // The ordering does not have to contain all parameter blocks, so
			  // assign zero offsets/empty independent sets to these parameter
			  // blocks.
			  Vector<ParameterBlock> parameter_blocks = program.parameter_blocks();
			  for (i = 0; i < parameter_blocks.size(); ++i) {
			    if (!ordering.IsMember(parameter_blocks.get(i).mutable_user_state())) {
			      parameter_blocks_.add(parameter_blocks.get(i));
			      independent_set_offsets_.add(independent_set_offsets_.get(independent_set_offsets_.size()-1));
			    }
			  }

			  // Compute the set of residual blocks that depend on each parameter
			  // block.
			  while (residual_blocks_.size() > parameter_block_index.size()) {
				  residual_blocks_.removeElementAt(residual_blocks_.size()-1);
			  }
			  while (residual_blocks_.size() < parameter_block_index.size()) {
				  residual_blocks_.add(new Vector<ResidualBlock>());
			  }
			  Vector<ResidualBlock> residual_blocks = program.residual_blocks();
			  for (i = 0; i < residual_blocks.size(); ++i) {
			    ResidualBlock residual_block = residual_blocks.get(i);
			    int num_parameter_blocks = residual_block.NumParameterBlocks();
			    for (j = 0; j < num_parameter_blocks; ++j) {
			      ParameterBlock parameter_block = residual_block.parameter_blocks()[j];
			      Integer value = parameter_block_index.get(parameter_block);
			      if (value != null) {
			    	  residual_blocks_.get(value.intValue()).add(residual_block);
			      }
			    }
			  }

			  evaluator_options_.linear_solver_type = requestedLinearSolverType;
			  evaluator_options_.num_eliminate_blocks = 0;
			  evaluator_options_.num_threads = 1;
			  evaluator_options_.context = context_;

			  return true;
			}
		
		public void Minimize(
			    MinimizerOptions options,
			    Vector<Double> parameters,
			    SolverSummary summary) {
			int i;
			double parameters_array[] = new double[parameters.size()];
			for (i = 0; i < parameters.size(); i++) {
				parameters_array[i] = parameters.get(i);
			}
			Minimize(options, parameters_array, summary);
			for (i = 0; i < parameters.size(); i++) {
				parameters.set(i, parameters_array[i]);
			}
		}
		
		
		public void Minimize(
			    MinimizerOptions options,
			    double[] parameters,
			    SolverSummary summary) {
			  int i, j;
			  // Set the state and mark all parameter blocks constant.
			  for (i = 0; i < parameter_blocks_.size(); ++i) {
			    ParameterBlock parameter_block = parameter_blocks_.get(i);
			    parameter_block.SetState(parameters,parameter_block.state_offset());
			    parameter_block.SetConstant();
			  }

			  LinearSolver linear_solvers[] = new LinearSolver[options.num_threads];
			  LinearSolverOptions linear_solver_options = new LinearSolverOptions();

			  linear_solver_options.type = requestedLinearSolverType;
			  linear_solver_options.context = context_;

			  for (i = 0; i < options.num_threads; ++i) {
			    linear_solvers[i] = Create(linear_solver_options);
			  }

			  for (i = 0; i < independent_set_offsets_.size() - 1; ++i) {
			    int num_problems =
			        independent_set_offsets_.get(i + 1) - independent_set_offsets_.get(i);
			    // Avoid parallelization overhead call if the set is empty.
			    if (num_problems == 0) {
			      continue;
			    }

			    int num_inner_iteration_threads =
			        Math.min(options.num_threads, num_problems);
			    evaluator_options_.num_threads =
			        Math.max(1, options.num_threads / num_inner_iteration_threads);

			    /*ThreadTokenProvider thread_token_provider(num_inner_iteration_threads);

			#ifdef CERES_USE_OPENMP
			    // The parameter blocks in each independent set can be optimized
			    // in parallel, since they do not co-occur in any residual block.
			#pragma omp parallel for num_threads(num_inner_iteration_threads)
			#endif

			#if !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))*/
			    for (j = independent_set_offsets_.get(i);
			         j < independent_set_offsets_.get(i + 1);
			         ++j) {
			/*#else
			    ParallelFor(context_,
			                independent_set_offsets_[i],
			                independent_set_offsets_[i + 1],
			                num_inner_iteration_threads,
			                [&](int j) {
			#endif // !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))

			      const ScopedThreadToken scoped_thread_token(&thread_token_provider);
			      const int thread_id = scoped_thread_token.token();*/
			      int thread_id = 0;

			      ParameterBlock parameter_block = parameter_blocks_.get(j);
			      int old_index = parameter_block.index();
			      int old_delta_offset = parameter_block.delta_offset();
			      parameter_block.SetVarying();
			      parameter_block.set_index(0);
			      parameter_block.set_delta_offset(0);

			      Program inner_program = new Program();
			      inner_program.mutable_parameter_blocks().add(parameter_block);
			      inner_program.residual_blocks_.clear();
			      inner_program.residual_blocks_.addAll(residual_blocks_.get(j));

			      // TODO(sameeragarwal): Better error handling. Right now we
			      // assume that this is not going to lead to problems of any
			      // sort. Basically we should be checking for numerical failure
			      // of some sort.
			      //
			      // On the other hand, if the optimization is a failure, that in
			      // some ways is fine, since it won't change the parameters and
			      // we are fine.
			      SolverSummary inner_summary = new SolverSummary();
			      Solve(inner_program,
			            linear_solvers[thread_id],
			            parameters, parameter_block.state_offset(),
			            inner_summary);

			      parameter_block.set_index(old_index);
			      parameter_block.set_delta_offset(old_delta_offset);
			      parameter_block.SetState(parameters,parameter_block.state_offset());
			      parameter_block.SetConstant();
			    }
			//#if defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
			  //);
			//#endif
			  }

			  for (i =  0; i < parameter_blocks_.size(); ++i) {
			    parameter_blocks_.get(i).SetVarying();
			  }

			  for (i = 0; i < options.num_threads; ++i) {
			    linear_solvers[i] = null;
			  }
			}

		// Solve the optimization problem for one parameter block.
		private void Solve(Program program,
		                                       LinearSolver linear_solver,
		                                       double[] parameter,
		                                       int parameter_start,
		                                       SolverSummary summary) {
		  summary.initial_cost = 0.0;
		  summary.fixed_cost = 0.0;
		  summary.final_cost = 0.0;
		  String error[] = new String[1];

		  Evaluator ev = Create(evaluator_options_,program,error);
		  if (ev == null) {
			  System.err.println("Evaluator ev == null in CoordinateDescentMinimizer.Solve");
			  return;
		  }
		  Minimizer min = new Minimizer();
		  MinimizerOptions minimizer_options = min.options_;
		  minimizer_options.evaluator = ev;
		  minimizer_options.jacobian = ((ProgramEvaluator)ev).CreateJacobian();
		  if (minimizer_options.jacobian == null) {
			  System.err.println("minimizer_options.evaluator.createJacobian() create a null Jacobian in CoordinateDescentMinimizer.Solve");
			  return;
		  }

		  TrustRegionStrategyOptions trs_options = new TrustRegionStrategyOptions();
		  trs_options.linear_solver = linear_solver;
		  TrustRegionStrategy trust = Create(trs_options);
		  if (trust == null) {
			  System.err.println("TrustRegionStrategy trust = Create(trs_options) == null in Solve");
			  return;
		  }
		  minimizer_options.trust_region_strategy = trust;
		  minimizer_options.is_silent = true;

		  TrustRegionMinimizer minimizer = new TrustRegionMinimizer();
		  minimizer.Minimize(minimizer_options, parameter, summary);
		}


	} // class CoordinateDescentMinimizer
	
	class TrustRegionMinimizer extends Minimizer {
		  private MinimizerOptions options_;

		  // These pointers are shortcuts to objects passed to the
		  // TrustRegionMinimizer. The TrustRegionMinimizer does not own them.
		  private double[] parameters_;
		  private SolverSummary solver_summary_;
		  private Evaluator evaluator_;
		  private SparseMatrix jacobian_;
		  private TrustRegionStrategy strategy_;

		  private TrustRegionStepEvaluator step_evaluator_;

		  private boolean is_not_silent_;
		  private boolean inner_iterations_are_enabled_;
		  private boolean inner_iterations_were_useful_;

		  // Summary of the current iteration.
		  IterationSummary iteration_summary_;

		  // Dimensionality of the problem in the ambient space.
		  private int num_parameters_;
		  // Dimensionality of the problem in the tangent space. This is the
		  // number of columns in the Jacobian.
		  private int num_effective_parameters_;
		  // Length of the residual vector, also the number of rows in the Jacobian.
		  private int num_residuals_;

		  // Current point.
		  private Vector<Double> x_;
		  // Residuals at x_;
		  private Vector<Double> residuals_;
		  // Gradient at x_.
		  private Vector<Double> gradient_;
		  // Solution computed by the inner iterations.
		  private Vector<Double> inner_iteration_x_;
		  // model_residuals = J * trust_region_step
		  private Vector<Double> model_residuals_;
		  private Vector<Double> negative_gradient_;
		  // projected_gradient_step = Plus(x, -gradient), an intermediate
		  // quantity used to compute the projected gradient norm.
		  private Vector<Double> projected_gradient_step_;
		  // The step computed by the trust region strategy. If Jacobi scaling
		  // is enabled, this is a vector in the scaled space.
		  private Vector<Double> trust_region_step_;
		  // The current proposal for how far the trust region algorithm
		  // thinks we should move. In the most basic case, it is just the
		  // trust_region_step_ with the Jacobi scaling undone. If bounds
		  // constraints are present, then it is the result of the projected
		  // line search.
		  private Vector<Double> delta_;
		  // candidate_x  = Plus(x, delta)
		  private Vector<Double> candidate_x_;
		  // Scaling vector to scale the columns of the Jacobian.
		  private Vector<Double> jacobian_scaling_;

		  // Euclidean norm of x_.
		  private double x_norm_;
		  // Cost at x_.
		  private double x_cost_[] = new double[1];
		  // Minimum cost encountered up till now.
		  private double minimum_cost_;
		  // How much did the trust region strategy reduce the cost of the
		  // linearized Gauss-Newton model.
		  private double model_cost_change_;
		  // Cost at candidate_x_.
		  private double candidate_cost_;

		  // Time at which the minimizer was started.
		  private double start_time_in_secs_;
		  // Time at which the current iteration was started.
		  private double iteration_start_time_in_secs_;
		  // Number of consecutive steps where the minimizer loop computed a
		  // numerically invalid step.
		  private int num_consecutive_invalid_steps_;
		  
		 public TrustRegionMinimizer() {
			 super();
		 }
		 

		  // This method is not thread safe.
		  public void Minimize(MinimizerOptions options,
		                        double[] parameters,
		                        SolverSummary solver_summary) {
			  start_time_in_secs_ = 1.0E-3 * System.currentTimeMillis();
			  iteration_start_time_in_secs_ = start_time_in_secs_;
			  Init(options, parameters, solver_summary);
			  if(!IterationZero()) {
				   System.err.println("Terminating: " + solver_summary_.message[0]);  
				   return;
			  }

			  // Create the TrustRegionStepEvaluator. The construction needs to be
			  // delayed to this point because we need the cost for the starting
			  // point to initialize the step evaluator.
			  if (options_.use_nonmonotonic_steps) {
				  step_evaluator_ = new TrustRegionStepEvaluator(x_cost_[0], options_.max_consecutive_nonmonotonic_steps); 
			  }
			  else {
				  step_evaluator_ = new TrustRegionStepEvaluator(x_cost_[0],0);
			  }
			      

			  while (FinalizeIterationAndCheckIfMinimizerCanContinue()) {
			    iteration_start_time_in_secs_ = 1.0E-3 * System.currentTimeMillis();
			    iteration_summary_ = new IterationSummary();
			    iteration_summary_.iteration =
			        solver_summary.iterations.get(solver_summary.iterations.size()-1).iteration + 1;

			    if(!ComputeTrustRegionStep()) {
			    	 System.err.println("Terminating: " + solver_summary_.message[0]);  
					   return;	
			    }
			    if (!iteration_summary_.step_is_valid) {
			      if(!HandleInvalidStep()) {
			    	  System.err.println("Terminating: " + solver_summary_.message[0]);  
					  return;	  
			      }
			      continue;
			    }

			    if (options_.is_constrained) {
			      // Use a projected line search to enforce the bounds constraints
			      // and improve the quality of the step.
			      DoLineSearch(x_, gradient_, x_cost_[0], delta_);
			    }

			    ComputeCandidatePointAndEvaluateCost();
			    DoInnerIterationsIfNeeded();

			    if (ParameterToleranceReached()) {
			      return;
			    }

			    if (FunctionToleranceReached()) {
			      return;
			    }

			    if (IsStepSuccessful()) {
			      if(!HandleSuccessfulStep()) {
			    	  System.err.println("Terminating: " + solver_summary_.message[0]);  
					  return;  
			      }
			      continue;
			    }

			    HandleUnsuccessfulStep();
			  }
	  
		  }


		// Initialize the minimizer, allocate working space and set some of
		// the fields in the solver_summary.
		private void Init(MinimizerOptions options,
		                                double[] parameters,
		                                SolverSummary solver_summary) {
		  int i;
		  options_ = options;
		  Collections.sort(options_.trust_region_minimizer_iterations_to_dump);

		  parameters_ = parameters;

		  solver_summary_ = solver_summary;
		  solver_summary_.termination_type = TerminationType.NO_CONVERGENCE;
		  solver_summary_.num_successful_steps = 0;
		  solver_summary_.num_unsuccessful_steps = 0;
		  solver_summary_.is_constrained = options.is_constrained;

		  evaluator_ = options_.evaluator;
		  if (evaluator_ == null) {
			  System.err.println("options_.evaluator == null in TrustRegionMinimizer Init");
			  return;
		  }
		  jacobian_ = options_.jacobian;
		  if (jacobian_ == null) {
			  System.err.println("options_.jacobian == null in TrustRegionMinimizer Init");
			  return;
		  }
		  strategy_ = options_.trust_region_strategy;
		  if (strategy_ == null) {
			  System.err.println("options_.trust_region_strategy == null in TrustRegionMinimizer Init");
			  return;
		  }

		  is_not_silent_ = !options.is_silent;
		  inner_iterations_are_enabled_ =
		      (options.inner_iteration_minimizer != null);
		  inner_iterations_were_useful_ = false;

		  num_parameters_ = evaluator_.NumParameters();
		  num_effective_parameters_ = evaluator_.NumEffectiveParameters();
		  num_residuals_ = evaluator_.NumResiduals();
		  num_consecutive_invalid_steps_ = 0;

		  
		  x_ = new Vector<Double>(num_parameters_);
		  for (i = 0; i < num_parameters_; i++) {
			  x_.add(parameters_[i]);
		  }
		  //x_norm_ set to -1 later in routine
		  //x_norm_ = 0.0;
		  //for (i = 0; i < num_parameters_; i++) {
			//  x_norm_ += (parameters_.get(i)*parameters_get(i));
		  // }
		  //x_norm_ = Math.sqrt(x_norm_);
		  if (residuals_ == null) {
			  residuals_ = new Vector<Double>();
		  }
		  while (residuals_.size() < num_residuals_) {
			  residuals_.add(0.0);
		  }
		  while (residuals_.size() > num_residuals_) {
			  residuals_.removeElementAt(residuals_.size() - 1);
		  }
		  if (trust_region_step_ == null) {
			  trust_region_step_ = new Vector<Double>();
		  }
		  while (trust_region_step_.size() < num_effective_parameters_) {
			  trust_region_step_.add(0.0);
		  }
		  while (trust_region_step_.size() > num_effective_parameters_) {
			  trust_region_step_.removeElementAt(trust_region_step_.size()-1);
		  }
		  if (delta_ == null) {
			  delta_ = new Vector<Double>();
		  }
		  while (delta_.size() < num_effective_parameters_) {
			  delta_.add(0.0);
		  }
		  while (delta_.size() > num_effective_parameters_) {
			  delta_.removeElementAt(delta_.size()-1);
		  }
		  if (candidate_x_ == null) {
			  candidate_x_ = new Vector<Double>();
		  }
		  while (candidate_x_.size() < num_parameters_) {
			  candidate_x_.add(0.0);
		  }
		  while (candidate_x_.size() > num_parameters_) {
			  candidate_x_.removeElementAt(candidate_x_.size()-1);
		  }
		  if (gradient_ == null) {
			  gradient_ = new Vector<Double>();
		  }
		  while (gradient_.size() < num_effective_parameters_) {
			  gradient_.add(0.0);
		  }
		  while (gradient_.size() > num_effective_parameters_) {
			  gradient_.removeElementAt(gradient_.size()-1);
		  }
		  if (model_residuals_ == null) {
			  model_residuals_ = new Vector<Double>();
		  }
		  while (model_residuals_.size() < num_residuals_) {
			  model_residuals_.add(0.0);
		  }
		  while (model_residuals_.size() > num_residuals_) {
			  model_residuals_.removeElementAt(model_residuals_.size()-1);
		  }
		  if (negative_gradient_ == null) {
			  negative_gradient_ = new Vector<Double>();
		  }
		  while (negative_gradient_.size() < num_effective_parameters_) {
			  negative_gradient_.add(0.0);
		  }
		  while (negative_gradient_.size() > num_effective_parameters_) {
			  negative_gradient_.removeElementAt(negative_gradient_.size()-1);
		  }
		  if (projected_gradient_step_ == null) {
			  projected_gradient_step_ = new Vector<Double>();
		  }
		  while (projected_gradient_step_.size() < num_parameters_) {
			  projected_gradient_step_.add(0.0);
		  }
		  while (projected_gradient_step_.size() > num_parameters_) {
			  projected_gradient_step_.removeElementAt(projected_gradient_step_.size()-1);
		  }

		  // By default scaling is one, if the user requests Jacobi scaling of
		  // the Jacobian, we will compute and overwrite this vector.
		  if (jacobian_scaling_ == null) {
			  jacobian_scaling_ = new Vector<Double>();
		  }
		  jacobian_scaling_.clear();
		  for (i = 0; i < num_effective_parameters_; i++) {
			  jacobian_scaling_.add(1.0);
		  }

		  x_norm_ = -1;  // Invalid value
		  x_cost_[0] = Double.MAX_VALUE;
		  minimum_cost_ = x_cost_[0];
		  model_cost_change_ = 0.0;
		}

		// 1. Project the initial solution onto the feasible set if needed.
		// 2. Compute the initial cost, jacobian & gradient.
		//
		// Return true if all computations can be performed successfully.
		private boolean IterationZero() {
		  int i;
		  iteration_summary_ = new IterationSummary();
		  iteration_summary_.iteration = 0;
		  iteration_summary_.step_is_valid = false;
		  iteration_summary_.step_is_successful = false;
		  iteration_summary_.cost_change = 0.0;
		  iteration_summary_.gradient_max_norm = 0.0;
		  iteration_summary_.gradient_norm = 0.0;
		  iteration_summary_.step_norm = 0.0;
		  iteration_summary_.relative_decrease = 0.0;
		  iteration_summary_.eta = options_.eta;
		  iteration_summary_.linear_solver_iterations = 0;
		  iteration_summary_.step_solver_time_in_seconds = 0;

		  if (options_.is_constrained) {
			for (i = 0; i < delta_.size(); i++) {
				delta_.set(i,0.0);
			}
		    if (!evaluator_.Plus(x_, delta_, candidate_x_)) {
		      solver_summary_.message[0] =
		          "Unable to project initial point onto the feasible set.";
		      solver_summary_.termination_type = TerminationType.FAILURE;
		      return false;
		    }

		    
		    for (i = 0; i < x_.size(); i++) {
		    	x_.set(i, candidate_x_.get(i));
		    	parameters_[i] = x_.get(i);
		    }
		    x_norm_ = 0.0;
		    for (i = 0; i < x_.size(); i++) {
		    	x_norm_ += (x_.get(i)*x_.get(i));
		    }
		    x_norm_ = Math.sqrt(x_norm_);
		  }
		  
		  if (!EvaluateGradientAndJacobian(true)) {
			    return false;
		  }

			  solver_summary_.initial_cost = x_cost_[0] + solver_summary_.fixed_cost;
			  iteration_summary_.step_is_valid = true;
			  iteration_summary_.step_is_successful = true;
			  return true;
		}
		
		// For the current x_, compute
		//
		//  1. Cost
		//  2. Jacobian
		//  3. Gradient
		//  4. Scale the Jacobian if needed (and compute the scaling if we are
//		     in iteration zero).
		//  5. Compute the 2 and max norm of the gradient.
		//
		// Returns true if all computations could be performed
		// successfully. Any failures are considered fatal and the
		// Solver::Summary is updated to indicate this.
		private boolean EvaluateGradientAndJacobian(
		    boolean new_evaluation_point) {
		  int i;
		  EvaluateOptions evaluate_options = new EvaluateOptions();
		  evaluate_options.new_evaluation_point = new_evaluation_point;
		  if (!evaluator_.Evaluate(evaluate_options,
		                            x_,
		                            x_cost_,
		                            residuals_,
		                            gradient_,
		                            jacobian_)) {
		    solver_summary_.message[0] = "Residual and Jacobian evaluation failed.";
		    solver_summary_.termination_type = TerminationType.FAILURE;
		    return false;
		  }

		  iteration_summary_.cost = x_cost_[0] + solver_summary_.fixed_cost;

		  if (options_.jacobi_scaling) {
		    if (iteration_summary_.iteration == 0) {
		      // Compute a scaling vector that is used to improve the
		      // conditioning of the Jacobian.
		      //
		      // jacobian_scaling_ = diag(J'J)^{-1}
		      jacobian_.SquaredColumnNorm(jacobian_scaling_);
		      for (i = 0; i < jacobian_.num_cols(); ++i) {
		        // Add one to the denominator to prevent division by zero.
		        jacobian_scaling_.set(i, 1.0 / (1.0 + Math.sqrt(jacobian_scaling_.get(i))));
		      }
		    }

		    // jacobian = jacobian * diag(J'J) ^{-1}
		    jacobian_.ScaleColumns(jacobian_scaling_);
		  }

		  // The gradient exists in the local tangent space. To account for
		  // the bounds constraints correctly, instead of just computing the
		  // norm of the gradient vector, we compute
		  //
		  // |Plus(x, -gradient) - x|
		  //
		  // Where the Plus operator lifts the negative gradient to the
		  // ambient space, adds it to x and projects it on the hypercube
		  // defined by the bounds.
		  negative_gradient_.clear();
		  for (i = 0; i < gradient_.size(); i++) {
			  negative_gradient_.add(-gradient_.get(i));
		  }
		  if (!evaluator_.Plus(x_,
		                        negative_gradient_,
		                        projected_gradient_step_)) {
		    solver_summary_.message[0] =
		        "projected_gradient_step = Plus(x, -gradient) failed.";
		    solver_summary_.termination_type = TerminationType.FAILURE;
		    return false;
		  }

		  iteration_summary_.gradient_max_norm = 0.0;
		  double norm = 0.0;
		  for (i = 0; i < x_.size(); i++) {
			  double value = Math.abs(x_.get(i) - projected_gradient_step_.get(i));
			  norm += (value * value);
			  if (value > iteration_summary_.gradient_max_norm) {
				  iteration_summary_.gradient_max_norm = value;
			  }
		  }
		  norm = Math.sqrt(norm);
		  iteration_summary_.gradient_norm = norm;
		  return true;
		}
		
		// 1. Add the final timing information to the iteration summary.
		// 2. Run the callbacks
		// 3. Check for termination based on
//		    a. Run time
//		    b. Iteration count
//		    c. Max norm of the gradient
//		    d. Size of the trust region radius.
		//
		// Returns true if user did not terminate the solver and none of these
		// termination criterion are met.
		private boolean FinalizeIterationAndCheckIfMinimizerCanContinue() {
		  int i;
		  if (iteration_summary_.step_is_successful) {
		    ++solver_summary_.num_successful_steps;
		    if (x_cost_[0] < minimum_cost_) {
		      minimum_cost_ = x_cost_[0];
		      for (i = 0; i < num_parameters_; i++) {
		    	  parameters_[i] =  x_.get(i);
		      }
		      iteration_summary_.step_is_nonmonotonic = false;
		    } else {
		      iteration_summary_.step_is_nonmonotonic = true;
		    }
		  } else {
		    ++solver_summary_.num_unsuccessful_steps;
		  }

		  iteration_summary_.trust_region_radius = strategy_.Radius();
		  iteration_summary_.iteration_time_in_seconds =
				  1.0E-3 * System.currentTimeMillis() - iteration_start_time_in_secs_;
		  iteration_summary_.cumulative_time_in_seconds =
				  1.0E-3 * System.currentTimeMillis() - start_time_in_secs_ +
		      solver_summary_.preprocessor_time_in_seconds;

		  solver_summary_.iterations.add(iteration_summary_);

		  if (!RunCallbacks(options_, iteration_summary_, solver_summary_)) {
		    return false;
		  }

		  if (MaxSolverTimeReached()) {
		    return false;
		  }

		  if (MaxSolverIterationsReached()) {
		    return false;
		  }

		  if (GradientToleranceReached()) {
		    return false;
		  }

		  if (MinTrustRegionRadiusReached()) {
		    return false;
		  }

		  return true;
		}
		
		// Check convergence based on the max norm of the gradient (only for
		// iterations where the step was declared successful).
		private boolean GradientToleranceReached() {
		  if (!iteration_summary_.step_is_successful ||
		      iteration_summary_.gradient_max_norm > options_.gradient_tolerance) {
		    return false;
		  }

		  solver_summary_.message[0] = 
		      "Gradient tolerance reached. \n" +
		      "Gradient max norm: " + iteration_summary_.gradient_max_norm + " <= " +
		      options_.gradient_tolerance;
		  solver_summary_.termination_type = TerminationType.CONVERGENCE;
		  if ((1 <= MAX_LOG_LEVEL) && is_not_silent_) {
			  Preferences.debug("Terminating: " + solver_summary_.message[0] + "\n",
					  Preferences.DEBUG_ALGORITHM);
		  }
		  return true;
		}

		
		// Check if the maximum number of iterations allowed by the user for
		// the solver has been exceeded, and if so return false after updating
		// Solver::Summary::message.
		private boolean MaxSolverIterationsReached() {
		  if (iteration_summary_.iteration < options_.max_num_iterations) {
		    return false;
		  }

		  solver_summary_.message[0] =
		      "Maximum number of iterations reached. \n" +
		                   "Number of iterations: " +
		                   iteration_summary_.iteration + ".";

		  solver_summary_.termination_type = TerminationType.NO_CONVERGENCE;
		  if ((1 <= MAX_LOG_LEVEL) && is_not_silent_) {
			  Preferences.debug("Terminating: " + solver_summary_.message[0] + "\n",
					  Preferences.DEBUG_ALGORITHM);
		  }
		  return true;
		}


		// Check if the maximum amount of time allowed by the user for the
		// solver has been exceeded, and if so return false after updating
		// Solver::Summary::message.
		private boolean MaxSolverTimeReached() {
		  double total_solver_time =
		      1.0E-3 * System.currentTimeMillis() - start_time_in_secs_ +
		      solver_summary_.preprocessor_time_in_seconds;
		  if (total_solver_time < options_.max_solver_time_in_seconds) {
		    return false;
		  }

		  solver_summary_.message[0] = "Maximum solver time reached. \n" +
		                                          "Total solver time: " + total_solver_time +
		                                         " >= " + options_.max_solver_time_in_seconds + ".";
		  solver_summary_.termination_type = TerminationType.NO_CONVERGENCE;
		  if ((1 <= MAX_LOG_LEVEL) && is_not_silent_) {
			  Preferences.debug("Terminating: " + solver_summary_.message[0] + "\n",
					  Preferences.DEBUG_ALGORITHM);
		  }
		  return true;
		}
		
		// Check convergence based the size of the trust region radius.
		private boolean MinTrustRegionRadiusReached() {
		  if (iteration_summary_.trust_region_radius >
		      options_.min_trust_region_radius) {
		    return false;
		  }

		  solver_summary_.message[0] =
		     "Minimum trust region radius reached. \n" +
		                   "Trust region radius: " + iteration_summary_.trust_region_radius 
		                   + " <=  " + 
		                   options_.min_trust_region_radius;
		  solver_summary_.termination_type = TerminationType.CONVERGENCE;
		  if ((1 <= MAX_LOG_LEVEL) && is_not_silent_) {
			  Preferences.debug("Terminating: " + solver_summary_.message[0] + "\n",
					  Preferences.DEBUG_ALGORITHM);
		  }
		  return true;
		}

		// Compute the trust region step using the TrustRegionStrategy chosen
		// by the user.
		//
		// If the strategy returns with LINEAR_SOLVER_FATAL_ERROR, which
		// indicates an unrecoverable error, return false. This is the only
		// condition that returns false.
		//
		// If the strategy returns with LINEAR_SOLVER_FAILURE, which indicates
		// a numerical failure that could be recovered from by retrying
		// (e.g. by increasing the strength of the regularization), we set
		// iteration_summary_.step_is_valid to false and return true.
		//
		// In all other cases, we compute the decrease in the trust region
		// model problem. In exact arithmetic, this should always be
		// positive, but due to numerical problems in the TrustRegionStrategy
		// or round off error when computing the decrease it may be
		// negative. In which case again, we set
		// iteration_summary_.step_is_valid to false.
		private boolean ComputeTrustRegionStep() {
		  int i;
		  double strategy_start_time = 1.0E-3 * System.currentTimeMillis();
		  iteration_summary_.step_is_valid = false;
		  TrustRegionStrategyPerSolveOptions per_solve_options = new TrustRegionStrategyPerSolveOptions();
		  per_solve_options.eta = options_.eta;
		  if (options_.trust_region_minimizer_iterations_to_dump.indexOf(iteration_summary_.iteration) != -1) {
		    per_solve_options.dump_format_type =
		        options_.trust_region_problem_dump_format_type;
		    per_solve_options.dump_filename_base =
		        options_.trust_region_problem_dump_directory +
		                 String.format("ceres_solver_iteration_%03d",
		                              iteration_summary_.iteration);
		  }
		  
		  double residuals_array[] = new double[residuals_.size()];
		  for (i = 0; i < residuals_.size(); i++) {
			  residuals_array[i] = residuals_.get(i);
		  }
		  double trust_region_step_array[] = new double[trust_region_step_.size()];
		  for (i = 0; i < trust_region_step_.size(); i++) {
			  trust_region_step_array[i] = trust_region_step_.get(i);
		  }
		  
		  TrustRegionStrategySummary strategy_summary =
			      strategy_.ComputeStep(per_solve_options,
			                             jacobian_,
			                             residuals_array,
			                             trust_region_step_array);
		  for (i = 0; i < residuals_.size(); i++) {
			  residuals_.set(i,residuals_array[i]);
		  }
		  for (i = 0; i < trust_region_step_.size(); i++) {
			  trust_region_step_.set(i,trust_region_step_array[i]);
		  }

			  if (strategy_summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_FATAL_ERROR) {
			    solver_summary_.message[0] =
			        "Linear solver failed due to unrecoverable \n" +
			        "non-numeric causes. Please see the error log for clues. ";
			    solver_summary_.termination_type = TerminationType.FAILURE;
			    return false;
			  }

			  iteration_summary_.step_solver_time_in_seconds =
					  1.0E-3 * System.currentTimeMillis() - strategy_start_time;
			  iteration_summary_.linear_solver_iterations = strategy_summary.num_iterations;

			  if (strategy_summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_FAILURE) {
			    return true;
			  }

			  // new_model_cost
			  //  = 1/2 [f + J * step]^2
			  //  = 1/2 [ f'f + 2f'J * step + step' * J' * J * step ]
			  // model_cost_change
			  //  = cost - new_model_cost
			  //  = f'f/2  - 1/2 [ f'f + 2f'J * step + step' * J' * J * step]
			  //  = -f'J * step - step' * J' * J * step / 2
			  //  = -(J * step)'(f + J * step / 2)
			  for (i = 0; i < model_residuals_.size(); i++) {
				  model_residuals_.set(i,0.0);
			  }
			  jacobian_.RightMultiply(trust_region_step_, model_residuals_);
			  model_cost_change_ = 0.0;
			  for (i = 0; i < model_residuals_.size(); i++) {
				  model_cost_change_ += (-model_residuals_.get(i))*(residuals_.get(i) + model_residuals_.get(i)/2.0);
			  }

			  // TODO(sameeragarwal)
			  //
			  //  1. What happens if model_cost_change_ = 0
			  //  2. What happens if -epsilon <= model_cost_change_ < 0 for some
			  //     small epsilon due to round off error.
			  iteration_summary_.step_is_valid = (model_cost_change_ > 0.0);
			  if (iteration_summary_.step_is_valid) {
			    // Undo the Jacobian column scaling.
				delta_.clear();
				for (i = 0; i < trust_region_step_.size(); i++) {
					delta_.add(trust_region_step_.get(i) * jacobian_scaling_.get(i));
				}
			    num_consecutive_invalid_steps_ = 0;
			  }

			  if ((1 <= MAX_LOG_LEVEL) && is_not_silent_ && !iteration_summary_.step_is_valid) {
			      Preferences.debug("Invalid step: current_cost: " + x_cost_[0] + "\n", Preferences.DEBUG_ALGORITHM);
			      Preferences.debug(" absolute model cost change: " + model_cost_change_ + "\n", Preferences.DEBUG_ALGORITHM);
			      Preferences.debug(" relative model cost change: " + (model_cost_change_ / x_cost_[0]) + "\n",
			    		  Preferences.DEBUG_ALGORITHM);
			  }
			  return true;
			}

		// Invalid steps can happen due to a number of reasons, and we allow a
		// limited number of consecutive failures, and return false if this
		// limit is exceeded.
		private boolean HandleInvalidStep() {
		  // TODO(sameeragarwal): Should we be returning FAILURE or
		  // NO_CONVERGENCE? The solution value is still usable in many cases,
		  // it is not clear if we should declare the solver a failure
		  // entirely. For example the case where model_cost_change ~ 0.0, but
		  // just slightly negative.
		  if (++num_consecutive_invalid_steps_ >=
		      options_.max_num_consecutive_invalid_steps) {
		    solver_summary_.message[0] = String.format(
		        "Number of consecutive invalid steps more \n" +
		        "than Solver::Options::max_num_consecutive_invalid_steps: %d",
		        options_.max_num_consecutive_invalid_steps);
		    solver_summary_.termination_type = TerminationType.FAILURE;
		    return false;
		  }

		  strategy_.StepIsInvalid();

		  // We are going to try and reduce the trust region radius and
		  // solve again. To do this, we are going to treat this iteration
		  // as an unsuccessful iteration. Since the various callbacks are
		  // still executed, we are going to fill the iteration summary
		  // with data that assumes a step of length zero and no progress.
		  iteration_summary_.cost = x_cost_[0] + solver_summary_.fixed_cost;
		  iteration_summary_.cost_change = 0.0;
		  iteration_summary_.gradient_max_norm =
		      solver_summary_.iterations.get(solver_summary_.iterations.size()-1).gradient_max_norm;
		  iteration_summary_.gradient_norm =
		      solver_summary_.iterations.get(solver_summary_.iterations.size()-1).gradient_norm;
		  iteration_summary_.step_norm = 0.0;
		  iteration_summary_.relative_decrease = 0.0;
		  iteration_summary_.eta = options_.eta;
		  return true;
		}

		// Perform a projected line search to improve the objective function
		// value along delta.
		//
		// TODO(sameeragarwal): The current implementation does not do
		// anything illegal but is incorrect and not terribly effective.
		//
		// https://github.com/ceres-solver/ceres-solver/issues/187
		private void DoLineSearch(Vector<Double> x, Vector<Double> gradient, double cost, Vector<Double> delta) {
		  int i;
		  LineSearchFunction line_search_function = new LineSearchFunction(evaluator_);

		  LineSearchOptions line_search_options = new LineSearchOptions();
		  line_search_options.is_silent = true;
		  line_search_options.interpolation_type =
		      options_.line_search_interpolation_type;
		  line_search_options.min_step_size = options_.min_line_search_step_size;
		  line_search_options.sufficient_decrease =
		      options_.line_search_sufficient_function_decrease;
		  line_search_options.max_step_contraction =
		      options_.max_line_search_step_contraction;
		  line_search_options.min_step_contraction =
		      options_.min_line_search_step_contraction;
		  line_search_options.max_num_iterations =
		      options_.max_num_line_search_step_size_iterations;
		  line_search_options.sufficient_curvature_decrease =
		      options_.line_search_sufficient_curvature_decrease;
		  line_search_options.max_step_expansion =
		      options_.max_line_search_step_expansion;
		  line_search_options.function = line_search_function;

		  String message[] = new String[1];
		  LineSearch line_search = Create(LineSearchType.ARMIJO, line_search_options, message);
		  if (line_search == null) {
			  System.err.println("line_search == null in DoLineSearch");
			  return;
		  }
		  LineSearchSummary line_search_summary = new LineSearchSummary();
		  line_search_function.Init(x, delta);
		  double initial_gradient = 0.0;
		  for (i = 0; i < gradient.size(); i++) {
		      initial_gradient += (gradient.get(i)*delta.get(i)); 
		  }
		  line_search.Search(1.0, cost, initial_gradient, line_search_summary);

		  solver_summary_.num_line_search_steps += line_search_summary.num_iterations;
		  solver_summary_.line_search_cost_evaluation_time_in_seconds +=
		      line_search_summary.cost_evaluation_time_in_seconds[0];
		  solver_summary_.line_search_gradient_evaluation_time_in_seconds +=
		      line_search_summary.gradient_evaluation_time_in_seconds[0];
		  solver_summary_.line_search_polynomial_minimization_time_in_seconds +=
		      line_search_summary.polynomial_minimization_time_in_seconds;
		  solver_summary_.line_search_total_time_in_seconds +=
		      line_search_summary.total_time_in_seconds;

		  if (line_search_summary.success) {
			for (i = 0; i < delta.size(); i++) {
				delta.set(i,delta.get(i)*line_search_summary.optimal_point.x);
			}
		  }
		}
		
		// Compute candidate_x_ = Plus(x_, delta_)
		// Evaluate the cost of candidate_x_ as candidate_cost_.
		//
		// Failure to compute the step or the cost mean that candidate_cost_
		// is set to std::numeric_limits<double>::max(). Unlike
		// EvaluateGradientAndJacobian, failure in this function is not fatal
		// as we are only computing and evaluating a candidate point, and if
		// for some reason we are unable to evaluate it, we consider it to be
		// a point with very high cost. This allows the user to deal with edge
		// cases/constraints as part of the LocalParameterization and
		// CostFunction objects.
		private void ComputeCandidatePointAndEvaluateCost() {
		  int i;
		  boolean status;
		  if (!evaluator_.Plus(x_, delta_, candidate_x_)) {
			if ((WARNING <= MAX_LOG_LEVEL) && is_not_silent_) {
		        Preferences.debug("x_plus_delta = Plus(x, delta) failed. \n" +
		        "Treating it as a step with infinite cost\n", Preferences.DEBUG_ALGORITHM);
			}
		    candidate_cost_ = Double.MAX_VALUE;
		    return;
		  }
		  

		  double candidate_x_array[] = new double[candidate_x_.size()];
		  for (i = 0; i < candidate_x_.size(); i++) {
			  candidate_x_array[i] = candidate_x_.get(i);
		  }
		  double candidate_cost_array[] = new double[] {candidate_cost_};
		  status = evaluator_.Evaluate(
		          candidate_x_array, candidate_cost_array, null, null, null);
		  for (i = 0; i < candidate_x_.size(); i++) {
			  candidate_x_.set(i,candidate_x_array[i]);
		  }
		  candidate_cost_ = candidate_cost_array[0];
		  if (!status) {
			  if ((WARNING <= MAX_LOG_LEVEL) && is_not_silent_) {
		        Preferences.debug("Step failed to evaluate. \n" +
		        "Treating it as a step with infinite cost\n", Preferences.DEBUG_ALGORITHM);
			  }
		    candidate_cost_ = Double.MAX_VALUE;
		  }
		}
		
		// Use the supplied coordinate descent minimizer to perform inner
		// iterations and compute the improvement due to it. Returns the cost
		// after performing the inner iterations.
		//
		// The optimization is performed with candidate_x_ as the starting
		// point, and if the optimization is successful, candidate_x_ will be
		// updated with the optimized parameters.
		private void DoInnerIterationsIfNeeded() {
		  int i;
		  boolean status;
		  inner_iterations_were_useful_ = false;
		  if (!inner_iterations_are_enabled_ ||
		      candidate_cost_ >= Double.MAX_VALUE) {
		    return;
		  }

		  double inner_iteration_start_time = 1.0E-3 * System.currentTimeMillis();
		  ++solver_summary_.num_inner_iteration_steps;
		  inner_iteration_x_ = candidate_x_;
		  SolverSummary inner_iteration_summary = new SolverSummary();
		  options_.inner_iteration_minimizer.Minimize(
		      options_, inner_iteration_x_, inner_iteration_summary);
		  double inner_iteration_cost[] = new double[1];
		  double inner_iteration_x_array[] = new double[inner_iteration_x_.size()];
		  for (i = 0; i < inner_iteration_x_.size(); i++) {
			  inner_iteration_x_array[i] = inner_iteration_x_.get(i);
		  }
		  status = evaluator_.Evaluate(
		          inner_iteration_x_array, inner_iteration_cost, null, null, null);
		  for (i = 0; i < inner_iteration_x_.size(); i++) {
			  inner_iteration_x_.set(i, inner_iteration_x_array[i]);
		  }
		  if (!status) {
			if ((2 <= MAX_LOG_LEVEL) && is_not_silent_) {
				Preferences.debug("Inner iteration failed\n", Preferences.DEBUG_ALGORITHM);
			}
		    return;
		  }

		  if ((2 <= MAX_LOG_LEVEL) && is_not_silent_) {
		      Preferences.debug("Inner iteration succeeded; Current cost: " + x_cost_[0] + "\n" +
		      "Trust region step cost: " + candidate_cost_ + "\n" +
		      "Inner iteration cost: " + inner_iteration_cost[0] + "\n", Preferences.DEBUG_ALGORITHM);
		  }

		  candidate_x_ = inner_iteration_x_;

		  // Normally, the quality of a trust region step is measured by
		  // the ratio
		  //
		  //              cost_change
		  //    r =    -----------------
		  //           model_cost_change
		  //
		  // All the change in the nonlinear objective is due to the trust
		  // region step so this ratio is a good measure of the quality of
		  // the trust region radius. However, when inner iterations are
		  // being used, cost_change includes the contribution of the
		  // inner iterations and its not fair to credit it all to the
		  // trust region algorithm. So we change the ratio to be
		  //
		  //                              cost_change
		  //    r =    ------------------------------------------------
		  //           (model_cost_change + inner_iteration_cost_change)
		  //
		  // Practically we do this by increasing model_cost_change by
		  // inner_iteration_cost_change.

		  double inner_iteration_cost_change =
		      candidate_cost_ - inner_iteration_cost[0];
		  model_cost_change_ += inner_iteration_cost_change;
		  inner_iterations_were_useful_ = inner_iteration_cost[0] < x_cost_[0];
		  double inner_iteration_relative_progress =
		      1.0 - inner_iteration_cost[0] / candidate_cost_;

		  // Disable inner iterations once the relative improvement
		  // drops below tolerance.
		  inner_iterations_are_enabled_ =
		      (inner_iteration_relative_progress > options_.inner_iteration_tolerance);
		  if ((2 <= MAX_LOG_LEVEL) && is_not_silent_ && !inner_iterations_are_enabled_) {
		      Preferences.debug("Disabling inner iterations. Progress : " + inner_iteration_relative_progress + "\n",
		    		  Preferences.DEBUG_ALGORITHM);
		  }
		  candidate_cost_ = inner_iteration_cost[0];

		  solver_summary_.inner_iteration_time_in_seconds +=
		      1.0E-3 * System.currentTimeMillis() - inner_iteration_start_time;
		}
		
		// Solver::Options::parameter_tolerance based convergence check.
		private boolean ParameterToleranceReached() {
		  // Compute the norm of the step in the ambient space.
		  int i;
		  double norm = 0.0;
		  double diff;
		  for (i = 0; i < x_.size(); i++) {
			  diff = x_.get(i) - candidate_x_.get(i);
			  norm += diff * diff;
		  }
		  iteration_summary_.step_norm = Math.sqrt(norm);
		  double step_size_tolerance =
		      options_.parameter_tolerance * (x_norm_ + options_.parameter_tolerance);

		  if (iteration_summary_.step_norm > step_size_tolerance) {
		    return false;
		  }

		  solver_summary_.message[0] = String.format(
		      "Parameter tolerance reached. \n" +
		      "Relative step_norm: %e <= %e.",
		      (iteration_summary_.step_norm / (x_norm_ + options_.parameter_tolerance)),
		      options_.parameter_tolerance);
		  solver_summary_.termination_type = TerminationType.CONVERGENCE;
		  if ((1 <= MAX_LOG_LEVEL) && is_not_silent_) {
			  Preferences.debug("Terminating: " + solver_summary_.message[0] + "\n",
					  Preferences.DEBUG_ALGORITHM);
		  }
		  return true;
		}

		// Solver::Options::function_tolerance based convergence check.
		private boolean FunctionToleranceReached() {
		  iteration_summary_.cost_change = x_cost_[0] - candidate_cost_;
		  double absolute_function_tolerance =
		      options_.function_tolerance * x_cost_[0];

		  if (Math.abs(iteration_summary_.cost_change) > absolute_function_tolerance) {
		    return false;
		  }

		  solver_summary_.message[0] = String.format(
		      "Function tolerance reached. \n" +
		      "|cost_change|/cost: %e <= %e",
		      Math.abs(iteration_summary_.cost_change) / x_cost_[0],
		      options_.function_tolerance);
		  solver_summary_.termination_type = TerminationType.CONVERGENCE;
		  if ((1 <= MAX_LOG_LEVEL) && is_not_silent_) {
			  Preferences.debug("Terminating: " + solver_summary_.message[0] + "\n",
					  Preferences.DEBUG_ALGORITHM);
		  }
		  return true;
		}

		private boolean IsStepSuccessful() {
			  iteration_summary_.relative_decrease =
			      step_evaluator_.StepQuality(candidate_cost_, model_cost_change_);

			  // In most cases, boosting the model_cost_change by the
			  // improvement caused by the inner iterations is fine, but it can
			  // be the case that the original trust region step was so bad that
			  // the resulting improvement in the cost was negative, and the
			  // change caused by the inner iterations was large enough to
			  // improve the step, but also to make relative decrease quite
			  // small.
			  //
			  // This can cause the trust region loop to reject this step. To
			  // get around this, we expicitly check if the inner iterations
			  // led to a net decrease in the objective function value. If
			  // they did, we accept the step even if the trust region ratio
			  // is small.
			  //
			  // Notice that we do not just check that cost_change is positive
			  // which is a weaker condition and would render the
			  // min_relative_decrease threshold useless. Instead, we keep
			  // track of inner_iterations_were_useful, which is true only
			  // when inner iterations lead to a net decrease in the cost.
			  return (inner_iterations_were_useful_ ||
			          iteration_summary_.relative_decrease >
			              options_.min_relative_decrease);
			}

		// Declare the step successful, move to candidate_x, update the
		// derivatives and let the trust region strategy and the step
		// evaluator know that the step has been accepted.
		private boolean HandleSuccessfulStep() {
		  int i;
		  for (i = 0; i < x_.size(); i++) {
		    	x_.set(i, candidate_x_.get(i));
		    	parameters_[i] = x_.get(i);
		    }
		  
		  x_norm_ = 0.0;
		  for (i = 0; i < x_.size(); i++) {
			  x_norm_ += x_.get(i)*x_.get(i);
		  }
		  x_norm_ = Math.sqrt(x_norm_);

		  // Since the step was successful, this point has already had the residual
		  // evaluated (but not the jacobian). So indicate that to the evaluator.
		  if (!EvaluateGradientAndJacobian(false)) {
		    return false;
		  }

		  iteration_summary_.step_is_successful = true;
		  strategy_.StepAccepted(iteration_summary_.relative_decrease);
		  step_evaluator_.StepAccepted(candidate_cost_, model_cost_change_);
		  return true;
		}
		
		// Declare the step unsuccessful and inform the trust region strategy.
		private void HandleUnsuccessfulStep() {
		  iteration_summary_.step_is_successful = false;
		  strategy_.StepRejected(iteration_summary_.relative_decrease);
		  iteration_summary_.cost = candidate_cost_ + solver_summary_.fixed_cost;
		}

	} // TrustRegionMinimizer
	
	public LineSearch Create(LineSearchType line_search_type,
            LineSearchOptions options,
            String[] error) {
			LineSearch line_search = null;
			switch (line_search_type) {
			case ARMIJO:
			line_search = new ArmijoLineSearch(options);
			break;
			case WOLFE:
			line_search = new WolfeLineSearch(options);
			break;
			default:
			error[0] = "Invalid line search algorithm type: " +
			LineSearchTypeToString(line_search_type) +
			", unable to create line search.";
			return null;
			}
			return line_search;
	}
	
	// Bracketing / Zoom Strong Wolfe condition line search.  This implementation
	// is based on the pseudo-code algorithm presented in Nocedal & Wright [1]
	// (p60-61) with inspiration from the WolfeLineSearch which ships with the
	// minFunc package by Mark Schmidt [2].
	//
	// [1] Nocedal J., Wright S., Numerical Optimization, 2nd Ed., Springer, 1999.
	// [2] http://www.di.ens.fr/~mschmidt/Software/minFunc.html.
	class WolfeLineSearch extends LineSearch {
		  public WolfeLineSearch(LineSearchOptions options) {
			  super(options);  
		  }
		  
		  // Returns true iff either a valid point, or valid bracket are found.
		  public boolean BracketingPhase(FunctionSample initial_position,
		                       double step_size_estimate,
		                       FunctionSample bracket_low,
		                       FunctionSample bracket_high,
		                       boolean do_zoom_search[],
		                       LineSearchSummary summary) {
			  LineSearchFunction function = options().function;

			  FunctionSample previous = initial_position;
			  FunctionSample current = new FunctionSample();

			  double descent_direction_max_norm =
			      function.DirectionInfinityNorm();

			  do_zoom_search[0] = false;
			  copyFunctionSample(bracket_low,initial_position);

			  // As we require the gradient to evaluate the Wolfe condition, we always
			  // calculate it together with the value, irrespective of the interpolation
			  // type.  As opposed to only calculating the gradient after the Armijo
			  // condition is satisifed, as the computational saving from this approach
			  // would be slight (perhaps even negative due to the extra call).  Also,
			  // always calculating the value & gradient together protects against us
			  // reporting invalid solutions if the cost function returns slightly different
			  // function values when evaluated with / without gradients (due to numerical
			  // issues).
			  ++summary.num_function_evaluations;
			  ++summary.num_gradient_evaluations;
			  boolean kEvaluateGradient = true;
			  function.Evaluate(step_size_estimate, kEvaluateGradient, current);
			  while (true) {
			    ++summary.num_iterations;

			    if (current.value_is_valid &&
			        (current.value[0] > (initial_position.value[0]
			                          + options().sufficient_decrease
			                          * initial_position.gradient
			                          * current.x) ||
			         (previous.value_is_valid && current.value[0] > previous.value[0]))) {
			      // Bracket found: current step size violates Armijo sufficient decrease
			      // condition, or has stepped past an inflection point of f() relative to
			      // previous step size.
			      do_zoom_search[0] = true;
			      copyFunctionSample(bracket_low,previous);
			      copyFunctionSample(bracket_high,current);
			      if (3 <= MAX_LOG_LEVEL) {
			              Preferences.debug("Bracket found: current step (" + current.x
			              + ")\n violates Armijo sufficient condition, or has passed an \n" +
			              "inflection point of f() based on value.\n", Preferences.DEBUG_ALGORITHM);
			      }
			      break;
			    }

			    if (current.value_is_valid &&
			        Math.abs(current.gradient) <=
			        -options().sufficient_curvature_decrease * initial_position.gradient) {
			      // Current step size satisfies the strong Wolfe conditions, and is thus a
			      // valid termination point, therefore a Zoom not required.
			      copyFunctionSample(bracket_low,current);
			      copyFunctionSample(bracket_high,current);
			              if (3 <= MAX_LOG_LEVEL) {
				              Preferences.debug("Bracketing phase found step size: " + current.x +
			                  "\n satisfying strong Wolfe conditions, initial_position: "  +
			                  initial_position + "\n current: \n" + current.ToDebugString() + "\n",
			                  Preferences.DEBUG_ALGORITHM);
			              }
			      break;

			    } else if (current.value_is_valid && current.gradient >= 0) {
			      // Bracket found: current step size has stepped past an inflection point
			      // of f(), but Armijo sufficient decrease is still satisfied and
			      // f(current) is our best minimum thus far.  Remember step size
			      // monotonically increases, thus previous_step_size < current_step_size
			      // even though f(previous) > f(current).
			      do_zoom_search[0] = true;
			      // Note inverse ordering from first bracket case.
			      copyFunctionSample(bracket_low,current);
			      copyFunctionSample(bracket_high,previous);
			      if (3 <= MAX_LOG_LEVEL) {
		              Preferences.debug("Bracket found: current sep (" + current.x
			              + ")\nsatisfies Armijo, but has gradient >= 0, thus have passed \n"
			              + "an inflection point of f().",Preferences.DEBUG_ALGORITHM);
			      }
			      break;

			    } else if (current.value_is_valid &&
			               Math.abs(current.x - previous.x) * descent_direction_max_norm
			               < options().min_step_size) {
			      // We have shrunk the search bracket to a width less than our tolerance,
			      // and still not found either a point satisfying the strong Wolfe
			      // conditions, or a valid bracket containing such a point. Stop searching
			      // and set bracket_low to the size size amongst all those tested which
			      // minimizes f() and satisfies the Armijo condition.
			      if ((WARNING <= MAX_LOG_LEVEL) && !options().is_silent) {
		              Preferences.debug("Line search failed: Wolfe bracketing phase shrank " + 
			          "bracket width: " + Math.abs(current.x - previous.x) + 
			          "\n to < tolerance: " + options().min_step_size
			          + "\n with descent_direction_max_norm: "
			          + descent_direction_max_norm + "\n and failed to find "
			          + "a point satisfying the strong Wolfe conditions or a " +
			          "\nbracketing containing such a point. Accepting "
			          + "\npoint found satisfying Armijo condition only, to "
			          + "\nallow continuation.\n", Preferences.DEBUG_ALGORITHM);
			      }
			      copyFunctionSample(bracket_low,current);
			      break;

			    } else if (summary.num_iterations >= options().max_num_iterations) {
			      // Check num iterations bound here so that we always evaluate the
			      // max_num_iterations-th iteration against all conditions, and
			      // then perform no additional (unused) evaluations.
			      summary.error =
			          String.format("Line search failed: Wolfe bracketing phase failed to \n" +
			                       "find a point satisfying strong Wolfe conditions, or a \n" +
			                       "bracket containing such a point within specified \n" +
			                       "max_num_iterations: %d", options().max_num_iterations);
			      if ((WARNING <= MAX_LOG_LEVEL) && !options().is_silent) {
			    	  Preferences.debug(summary.error + "\n", Preferences.DEBUG_ALGORITHM);
			      }
			      // Ensure that bracket_low is always set to the step size amongst all
			      // those tested which minimizes f() and satisfies the Armijo condition
			      // when we terminate due to the 'artificial' max_num_iterations condition.
			      if (current.value_is_valid && current.value[0] < bracket_low.value[0]) {
			    	  copyFunctionSample(bracket_low,current);
			      }
			      break;
			    }
			    // Either: f(current) is invalid; or, f(current) is valid, but does not
			    // satisfy the strong Wolfe conditions itself, or the conditions for
			    // being a boundary of a bracket.

			    // If f(current) is valid, (but meets no criteria) expand the search by
			    // increasing the step size.
			    double max_step_size =
			        current.value_is_valid
			        ? (current.x * options().max_step_expansion) : current.x;

			    // We are performing 2-point interpolation only here, but the API of
			    // InterpolatingPolynomialMinimizingStepSize() allows for up to
			    // 3-point interpolation, so pad call with a sample with an invalid
			    // value that will therefore be ignored.
			    FunctionSample unused_previous = new FunctionSample();
			    if(unused_previous.value_is_valid) {
			    	System.err.println("unused_previous.value_is_valid = true in WolfeLineSearch BracketingPhase");
			    	return false;
			    }
			    // Contracts step size if f(current) is not valid.
			    double polynomial_minimization_start_time = 1.0E-3 * System.currentTimeMillis();
			    double step_size =
			        this.InterpolatingPolynomialMinimizingStepSize(
			            options().interpolation_type,
			            previous,
			            unused_previous,
			            current,
			            previous.x,
			            max_step_size);
			    summary.polynomial_minimization_time_in_seconds +=
			        (1.0E-3 * System.currentTimeMillis() - polynomial_minimization_start_time);
			    if (step_size * descent_direction_max_norm < options().min_step_size) {
			      summary.error =
			          String.format("Line search failed: step_size too small: %.5e \n" +
			                       "with descent_direction_max_norm: %.5e", step_size,
			                       descent_direction_max_norm);
			      if ((WARNING <= MAX_LOG_LEVEL) && !options().is_silent) {
			    	  Preferences.debug(summary.error + "\n", Preferences.DEBUG_ALGORITHM);
			      }
			      return false;
			    }

			    if (current.value_is_valid) {
			    	copyFunctionSample(previous, current);
			    }
			    ++summary.num_function_evaluations;
			    ++summary.num_gradient_evaluations;
			    function.Evaluate(step_size, kEvaluateGradient, current);
			  } // while (true)

			  // Ensure that even if a valid bracket was found, we will only mark a zoom
			  // as required if the bracket's width is greater than our minimum tolerance.
			  if (do_zoom_search[0] &&
			      Math.abs(bracket_high.x - bracket_low.x) * descent_direction_max_norm
			      < options().min_step_size) {
			    do_zoom_search[0] = false;
			  }

			  return true;
			} // public boolean BracketingPhase

			// Returns true iff solution satisfies the strong Wolfe conditions. Otherwise,
			// on return false, if we stopped searching due to the 'artificial' condition of
			// reaching max_num_iterations, solution is the step size amongst all those
			// tested, which satisfied the Armijo decrease condition and minimized f().
			public boolean ZoomPhase(FunctionSample initial_position,
			                                FunctionSample bracket_low,
			                                FunctionSample bracket_high,
			                                FunctionSample solution,
			                                LineSearchSummary summary) {
			  LineSearchFunction function = options().function;

			  if(!(bracket_low.value_is_valid && bracket_low.gradient_is_valid)) {
			      System.err.println("Ceres bug: f_low input to Wolfe Zoom invalid, please contact \n" +
			      "the developers!, initial_position: \n" +
			      initial_position.ToDebugString() + "\n" +
			      "bracket_low: \n" +
			      bracket_low.ToDebugString() +
			      "\n bracket_high: \n" +
			      bracket_high.ToDebugString());
			  }
			  // We do not require bracket_high.gradient_is_valid as the gradient condition
			  // for a valid bracket is only dependent upon bracket_low.gradient, and
			  // in order to minimize jacobian evaluations, bracket_high.gradient may
			  // not have been calculated (if bracket_high.value does not satisfy the
			  // Armijo sufficient decrease condition and interpolation method does not
			  // require it).
			  //
			  // We also do not require that: bracket_low.value < bracket_high.value,
			  // although this is typical. This is to deal with the case when
			  // bracket_low = initial_position, bracket_high is the first sample,
			  // and bracket_high does not satisfy the Armijo condition, but still has
			  // bracket_high.value < initial_position.value.
			  if(!bracket_high.value_is_valid) {
			      System.err.println("Ceres bug: f_high input to Wolfe Zoom invalid, please \n" +
			      "contact the developers!, initial_position: \n" +
			      initial_position.ToDebugString() + "\n" +
			      "bracket_low: \n" +
			      bracket_low.ToDebugString() + "\n" +
			      "bracket_high: \n" +
			      bracket_high.ToDebugString());
			  }

			  if (bracket_low.gradient * (bracket_high.x - bracket_low.x) >= 0) {
			    // The third condition for a valid initial bracket:
			    //
			    //   3. bracket_high is chosen after bracket_low, s.t.
			    //      bracket_low.gradient * (bracket_high.x - bracket_low.x) < 0.
			    //
			    // is not satisfied.  As this can happen when the users' cost function
			    // returns inconsistent gradient values relative to the function values,
			    // we do not CHECK_LT(), but we do stop processing and return an invalid
			    // value.
			    summary.error =
			        String.format("Line search failed: Wolfe zoom phase passed a bracket \n" +
			                     "which does not satisfy: bracket_low.gradient * \n" +
			                     "(bracket_high.x - bracket_low.x) < 0 [%.8e !< 0] \n" +
			                     "with initial_position: %s, bracket_low: %s, bracket_high:\n" +
			                     " %s, the most likely cause of which is the cost function \n" +
			                     "returning inconsistent gradient & function values.",
			                     bracket_low.gradient * (bracket_high.x - bracket_low.x),
			                     initial_position.ToDebugString(),
			                     bracket_low.ToDebugString(),
			                     bracket_high.ToDebugString());
			    if ((WARNING <= MAX_LOG_LEVEL) && !options().is_silent) {
			    	  Preferences.debug(summary.error + "\n", Preferences.DEBUG_ALGORITHM);
			    }
			    solution.value_is_valid = false;
			    return false;
			  }

			  int num_bracketing_iterations = summary.num_iterations;
			  double descent_direction_max_norm = function.DirectionInfinityNorm();

			  while (true) {
			    // Set solution to bracket_low, as it is our best step size (smallest f())
			    // found thus far and satisfies the Armijo condition, even though it does
			    // not satisfy the Wolfe condition.
			    copyFunctionSample(solution,bracket_low);
			    if (summary.num_iterations >= options().max_num_iterations) {
			      summary.error =
			          String.format("Line search failed: Wolfe zoom phase failed to \n" +
			                       "find a point satisfying strong Wolfe conditions \n" +
			                       "within specified max_num_iterations: %d, \n" +
			                       "(num iterations taken for bracketing: %d).",
			                       options().max_num_iterations, num_bracketing_iterations);
			      if ((WARNING <= MAX_LOG_LEVEL) && !options().is_silent) {
			    	  Preferences.debug(summary.error + "\n", Preferences.DEBUG_ALGORITHM);
			      }
			      return false;
			    }
			    if (Math.abs(bracket_high.x - bracket_low.x) * descent_direction_max_norm
			        < options().min_step_size) {
			      // Bracket width has been reduced below tolerance, and no point satisfying
			      // the strong Wolfe conditions has been found.
			      summary.error =
			          String.format("Line search failed: Wolfe zoom bracket width: %.5e \n" +
			                       "too small with descent_direction_max_norm: %.5e.",
			                       Math.abs(bracket_high.x - bracket_low.x),
			                       descent_direction_max_norm);
			      if ((WARNING <= MAX_LOG_LEVEL) && !options().is_silent) {
			    	  Preferences.debug(summary.error + "\n", Preferences.DEBUG_ALGORITHM);
			      }
			      return false;
			    }

			    ++summary.num_iterations;
			    // Polynomial interpolation requires inputs ordered according to step size,
			    // not f(step size).
			    FunctionSample lower_bound_step =
			        bracket_low.x < bracket_high.x ? bracket_low : bracket_high;
			    FunctionSample upper_bound_step =
			        bracket_low.x < bracket_high.x ? bracket_high : bracket_low;
			    // We are performing 2-point interpolation only here, but the API of
			    // InterpolatingPolynomialMinimizingStepSize() allows for up to
			    // 3-point interpolation, so pad call with a sample with an invalid
			    // value that will therefore be ignored.
			    FunctionSample unused_previous = new FunctionSample();
			    if(unused_previous.value_is_valid) {
			    	System.err.println("unused_previous.value_is_valid = true in WolfeLineSearch ZoomPhase");
			    	return false;
			    }
			    double polynomial_minimization_start_time = 1.0E-3 * System.currentTimeMillis();
			    double step_size =
			        this.InterpolatingPolynomialMinimizingStepSize(
			            options().interpolation_type,
			            lower_bound_step,
			            unused_previous,
			            upper_bound_step,
			            lower_bound_step.x,
			            upper_bound_step.x);
			    summary.polynomial_minimization_time_in_seconds +=
			        (1.0E-3 * System.currentTimeMillis() - polynomial_minimization_start_time);
			    // No check on magnitude of step size being too small here as it is
			    // lower-bounded by the initial bracket start point, which was valid.
			    //
			    // As we require the gradient to evaluate the Wolfe condition, we always
			    // calculate it together with the value, irrespective of the interpolation
			    // type.  As opposed to only calculating the gradient after the Armijo
			    // condition is satisifed, as the computational saving from this approach
			    // would be slight (perhaps even negative due to the extra call).  Also,
			    // always calculating the value & gradient together protects against us
			    // reporting invalid solutions if the cost function returns slightly
			    // different function values when evaluated with / without gradients (due
			    // to numerical issues).
			    ++summary.num_function_evaluations;
			    ++summary.num_gradient_evaluations;
			    boolean kEvaluateGradient = true;
			    function.Evaluate(step_size, kEvaluateGradient, solution);
			    if (!solution.value_is_valid || !solution.gradient_is_valid) {
			      summary.error =
			          String.format("Line search failed: Wolfe Zoom phase found \n" +
			                       "step_size: %.5e, for which function is invalid, \n" +
			                       "between low_step: %.5e and high_step: %.5e \n" +
			                       "at which function is valid.",
			                       solution.x, bracket_low.x, bracket_high.x);
			      if ((WARNING <= MAX_LOG_LEVEL) && !options().is_silent) {
			    	  Preferences.debug(summary.error + "\n", Preferences.DEBUG_ALGORITHM);
			      }
			      return false;
			    }

			    if (3 <= MAX_LOG_LEVEL) {
			               Preferences.debug("Zoom iteration: " + (summary.num_iterations - num_bracketing_iterations) + "\n" +
			               "bracket_low: \n" + 
			                bracket_low.ToDebugString() + "\n" +
			                "bracket_high: \n" +
			                bracket_high.ToDebugString() + "\n" +
			                "minimizing solution: \n" +
			                solution.ToDebugString() + "\n", Preferences.DEBUG_ALGORITHM);
			    }

			    if ((solution.value[0] > (initial_position.value[0]
			                            + options().sufficient_decrease
			                            * initial_position.gradient
			                            * solution.x)) ||
			        (solution.value[0] >= bracket_low.value[0])) {
			      // Armijo sufficient decrease not satisfied, or not better
			      // than current lowest sample, use as new upper bound.
			      copyFunctionSample(bracket_high,solution);
			      continue;
			    }

			    // Armijo sufficient decrease satisfied, check strong Wolfe condition.
			    if (Math.abs(solution.gradient) <=
			        -options().sufficient_curvature_decrease * initial_position.gradient) {
			      // Found a valid termination point satisfying strong Wolfe conditions.
			      if (3 <= MAX_LOG_LEVEL) {
			              Preferences.debug("Zoom phase found step size: " + solution.x + "\n" +
			              ", satisfying strong Wolfe conditions.\n", Preferences.DEBUG_ALGORITHM);
			      }
			      break;

			    } else if (solution.gradient * (bracket_high.x - bracket_low.x) >= 0) {
			      bracket_high = bracket_low;
			    }

			    copyFunctionSample(bracket_low,solution);
			  }
			  // Solution contains a valid point which satisfies the strong Wolfe
			  // conditions.
			  return true;

		  } // ZoomPhase
		 

		 public void DoSearch(double step_size_estimate,
		                        double initial_cost,
		                        double initial_gradient,
		                        LineSearchSummary summary) {

			  // All parameters should have been validated by the Solver, but as
			  // invalid values would produce crazy nonsense, hard check them here.
			  if (step_size_estimate < 0.0) {
				  System.err.println("step_size_estimate < 0.0 in WolfeLineSearch DoSearch");
				  return;
			  }
			  if (options().sufficient_decrease <= 0.0) {
				  System.err.println("options().sufficient_decrease <= 0.0 in WolfeLineSearch DoSearch");
				  return;
			  }
			  if (options().sufficient_curvature_decrease <= options().sufficient_decrease) {
				  System.err.println("options().sufficient_curvature_decrease <= options().sufficient_decrease in WolfeLineSearch DoSearch");
				  return;
			  }
			  if (options().sufficient_curvature_decrease >= 1.0) {
				  System.err.println("options().sufficient_curvature_decrease >= 1.0 in WolfeLineSearch DoSearch");
				  return;
			  }
			  if (options().max_step_expansion <= 1.0) {
				  System.err.println("options().max_step_expansion <= 1.0 in WolfeLineSearch DoSearch");
				  return;
			  }

			  // Note initial_cost & initial_gradient are evaluated at step_size = 0,
			  // not step_size_estimate, which is our starting guess.
			  FunctionSample initial_position = new FunctionSample(0.0, initial_cost, initial_gradient);
			  initial_position.vector_x = options().function.position();
			  initial_position.vector_x_is_valid = true;
			  boolean do_zoom_search[] = new boolean[] {false};
			  // Important: The high/low in bracket_high & bracket_low refer to their
			  // _function_ values, not their step sizes i.e. it is _not_ required that
			  // bracket_low.x < bracket_high.x.
			  FunctionSample solution = new FunctionSample();
			  FunctionSample bracket_low = new FunctionSample();
			  FunctionSample bracket_high = new FunctionSample();

			  // Wolfe bracketing phase: Increases step_size until either it finds a point
			  // that satisfies the (strong) Wolfe conditions, or an interval that brackets
			  // step sizes which satisfy the conditions.  From Nocedal & Wright [1] p61 the
			  // interval: (step_size_{k-1}, step_size_{k}) contains step lengths satisfying
			  // the strong Wolfe conditions if one of the following conditions are met:
			  //
			  //   1. step_size_{k} violates the sufficient decrease (Armijo) condition.
			  //   2. f(step_size_{k}) >= f(step_size_{k-1}).
			  //   3. f'(step_size_{k}) >= 0.
			  //
			  // Caveat: If f(step_size_{k}) is invalid, then step_size is reduced, ignoring
			  // this special case, step_size monotonically increases during bracketing.
			  if (!this.BracketingPhase(initial_position,
			                             step_size_estimate,
			                             bracket_low,
			                             bracket_high,
			                             do_zoom_search,
			                             summary)) {
			    // Failed to find either a valid point, a valid bracket satisfying the Wolfe
			    // conditions, or even a step size > minimum tolerance satisfying the Armijo
			    // condition.
			    return;
			  }

			  if (!do_zoom_search[0]) {
			    // Either: Bracketing phase already found a point satisfying the strong
			    // Wolfe conditions, thus no Zoom required.
			    //
			    // Or: Bracketing failed to find a valid bracket or a point satisfying the
			    // strong Wolfe conditions within max_num_iterations, or whilst searching
			    // shrank the bracket width until it was below our minimum tolerance.
			    // As these are 'artificial' constraints, and we would otherwise fail to
			    // produce a valid point when ArmijoLineSearch would succeed, we return the
			    // point with the lowest cost found thus far which satsifies the Armijo
			    // condition (but not the Wolfe conditions).
				if (summary.optimal_point != null) {
			        copyFunctionSample(summary.optimal_point,bracket_low);
				}
				else {
					summary.optimal_point = bracket_low;
				}
			    summary.success = true;
			    return;
			  }

			  //VLOG(3) << std::scientific << std::setprecision(kErrorMessageNumericPrecision)
			  if (3 <= MAX_LOG_LEVEL) {
			          Preferences.debug("Starting line search zoom phase with bracket_low: \n" +
			          bracket_low.ToDebugString() + "\nbracket_high: \n" + bracket_high.ToDebugString() 
			          + "\nbracket width: " + Math.abs(bracket_low.x - bracket_high.x)
			          + "\nbracket abs delta cost: " + Math.abs(bracket_low.value[0] - bracket_high.value[0]) + "\n",
			          Preferences.DEBUG_ALGORITHM);
			  }

			  // Wolfe Zoom phase: Called when the Bracketing phase finds an interval of
			  // non-zero, finite width that should bracket step sizes which satisfy the
			  // (strong) Wolfe conditions (before finding a step size that satisfies the
			  // conditions).  Zoom successively decreases the size of the interval until a
			  // step size which satisfies the Wolfe conditions is found.  The interval is
			  // defined by bracket_low & bracket_high, which satisfy:
			  //
			  //   1. The interval bounded by step sizes: bracket_low.x & bracket_high.x
			  //      contains step sizes that satsify the strong Wolfe conditions.
			  //   2. bracket_low.x is of all the step sizes evaluated *which satisifed the
			  //      Armijo sufficient decrease condition*, the one which generated the
			  //      smallest function value, i.e. bracket_low.value <
			  //      f(all other steps satisfying Armijo).
			  //        - Note that this does _not_ (necessarily) mean that initially
			  //          bracket_low.value < bracket_high.value (although this is typical)
			  //          e.g. when bracket_low = initial_position, and bracket_high is the
			  //          first sample, and which does not satisfy the Armijo condition,
			  //          but still has bracket_high.value < initial_position.value.
			  //   3. bracket_high is chosen after bracket_low, s.t.
			  //      bracket_low.gradient * (bracket_high.x - bracket_low.x) < 0.
			  if (!this.ZoomPhase(initial_position,
			                       bracket_low,
			                       bracket_high,
			                       solution,
			                       summary) && !solution.value_is_valid) {
			    // Failed to find a valid point (given the specified decrease parameters)
			    // within the specified bracket.
			    return;
			  }
			  // Ensure that if we ran out of iterations whilst zooming the bracket, or
			  // shrank the bracket width to < tolerance and failed to find a point which
			  // satisfies the strong Wolfe curvature condition, that we return the point
			  // amongst those found thus far, which minimizes f() and satisfies the Armijo
			  // condition.

			  if (!solution.value_is_valid || solution.value[0] > bracket_low.value[0]) {
				if (summary.optimal_point != null) {
			        copyFunctionSample(summary.optimal_point,bracket_low);
				}
				else {
					summary.optimal_point = bracket_low;
				}
			  } else {
				if (summary.optimal_point != null) {
			        copyFunctionSample(summary.optimal_point,solution);
				}
				else {
					summary.optimal_point = solution;
				}
			  }

			  summary.success = true;
	 
		 }
		 
	} // class WolfeLineSearch
	
	// Backtracking and interpolation based Armijo line search. This
	// implementation is based on the Armijo line search that ships in the
	// minFunc package by Mark Schmidt.
	//
	// For more details: http://www.di.ens.fr/~mschmidt/Software/minFunc.html
	class ArmijoLineSearch extends LineSearch {
	  public ArmijoLineSearch(LineSearchOptions options) {
		  super(options);  
	  }

	 public void DoSearch(double step_size_estimate,
	                        double initial_cost,
	                        double initial_gradient,
	                        LineSearchSummary summary) {
		  if (step_size_estimate < 0.0) {
			  System.err.println("step_size_estimate < 0.0 in ArmijoLineSearch DoSearch");
			  return;
		  }
		  if (options().sufficient_decrease <= 0.0) {
			  System.err.println("options().sufficient_decrease <= 0.0 in ArmijoLineSearch DoSearch");
			  return;
		  }
		  if (options().sufficient_decrease >= 1.0) {
			  System.err.println("options().sufficient_decrease >= 1.0 in ArmijoLineSearch DoSearch");
			  return;
		  }
		  if (options().max_num_iterations <= 0) {
			  System.err.println("options().max_num_iterations <= 0 in ArmijoLineSearch DoSearch");
			  return;
		  }
		  LineSearchFunction function = options().function;

		  // Note initial_cost & initial_gradient are evaluated at step_size = 0,
		  // not step_size_estimate, which is our starting guess.
		  FunctionSample initial_position = new FunctionSample(0.0, initial_cost, initial_gradient);
		  initial_position.vector_x = function.position();
		  initial_position.vector_x_is_valid = true;

		  double descent_direction_max_norm = function.DirectionInfinityNorm();
		  FunctionSample previous = new FunctionSample();
		  FunctionSample current = new FunctionSample();

		  // As the Armijo line search algorithm always uses the initial point, for
		  // which both the function value and derivative are known, when fitting a
		  // minimizing polynomial, we can fit up to a quadratic without requiring the
		  // gradient at the current query point.
		  boolean kEvaluateGradient = options().interpolation_type == LineSearchInterpolationType.CUBIC;

		  ++summary.num_function_evaluations;
		  if (kEvaluateGradient) {
		    ++summary.num_gradient_evaluations;
		  }

		  function.Evaluate(step_size_estimate, kEvaluateGradient, current);
		  while (!current.value_is_valid ||
		         current.value[0] > (initial_cost
		                          + options().sufficient_decrease
		                          * initial_gradient
		                          * current.x)) {
		    // If current.value_is_valid is false, we treat it as if the cost at that
		    // point is not large enough to satisfy the sufficient decrease condition.
		    ++summary.num_iterations;
		    if (summary.num_iterations >= options().max_num_iterations) {
		      summary.error =
		          String.format("Line search failed: Armijo failed to find a point \n" +
		                       "satisfying the sufficient decrease condition within \n" +
		                       "specified max_num_iterations: %d.",
		                       options().max_num_iterations);
		      
		      if ((WARNING <= MAX_LOG_LEVEL) && !options().is_silent) {
		          Preferences.debug(summary.error + "\n", Preferences.DEBUG_ALGORITHM);
		      }
		      return;
		    }

		    double polynomial_minimization_start_time = 1.0E-3 * System.currentTimeMillis();
		    double step_size =
		        this.InterpolatingPolynomialMinimizingStepSize(
		            options().interpolation_type,
		            initial_position,
		            previous,
		            current,
		            (options().max_step_contraction * current.x),
		            (options().min_step_contraction * current.x));
		    summary.polynomial_minimization_time_in_seconds +=
		        (1.0E-3 * System.currentTimeMillis() - polynomial_minimization_start_time);

		    if (step_size * descent_direction_max_norm < options().min_step_size) {
		      summary.error =
		          String.format("Line search failed: step_size too small: %.5e " +
		                       "with descent_direction_max_norm: %.5e.", step_size,
		                       descent_direction_max_norm);
		      if ((WARNING <= MAX_LOG_LEVEL) && !options().is_silent) {
		          Preferences.debug(summary.error + "\n", Preferences.DEBUG_ALGORITHM);
		      }
		      return;
		    }

		    previous = current;

		    ++summary.num_function_evaluations;
		    if (kEvaluateGradient) {
		      ++summary.num_gradient_evaluations;
		    }

		    function.Evaluate(step_size, kEvaluateGradient, current);
		  }

		  summary.optimal_point = current;
		  summary.success = true;
	 
	 };
	} // class ArmijoLineSearch

	
	abstract class LineSearch {
		private LineSearchOptions options_;
		
		public LineSearch(LineSearchOptions options) {
		    options_ = options;	
		}
		
		public abstract void DoSearch(double step_size_estimate,
                double initial_cost,
                double initial_gradient,
                LineSearchSummary summary);
		
		public LineSearchOptions options() { return options_; }
		
		public void Search(double step_size_estimate,
                double initial_cost,
                double initial_gradient,
                LineSearchSummary summary) {
			    double start_time = 1.0E-3 * System.currentTimeMillis();
			
			if (summary == null) {
				System.err.println("LineSearchSummary summary == null in LineSearch Search");
				return;
			}
			summary.cost_evaluation_time_in_seconds[0] = 0.0;
			summary.gradient_evaluation_time_in_seconds[0] = 0.0;
			summary.polynomial_minimization_time_in_seconds = 0.0;
			options().function.ResetTimeStatistics();
			this.DoSearch(step_size_estimate, initial_cost, initial_gradient, summary);
			options().function.
			TimeStatistics(summary.cost_evaluation_time_in_seconds,
			             summary.gradient_evaluation_time_in_seconds);
			
			summary.total_time_in_seconds = 1.0E-3 * System.currentTimeMillis() - start_time;
           }
		
		// Returns step_size \in [min_step_size, max_step_size] which minimizes the
		// polynomial of degree defined by interpolation_type which interpolates all
		// of the provided samples with valid values.
		protected double InterpolatingPolynomialMinimizingStepSize(
		    LineSearchInterpolationType interpolation_type,
		    FunctionSample lowerbound,
		    FunctionSample previous,
		    FunctionSample current,
		    double min_step_size,
		    double max_step_size) {
		  if (!current.value_is_valid ||
		      (interpolation_type == LineSearchInterpolationType.BISECTION &&
		       max_step_size <= current.x)) {
		    // Either: sample is invalid; or we are using BISECTION and contracting
		    // the step size.
		    return Math.min(Math.max(current.x * 0.5, min_step_size), max_step_size);
		  } 
		  else if (interpolation_type == LineSearchInterpolationType.BISECTION) {
			if (max_step_size <= current.x) {
				System.err.println("max_step_size <= current.x in LineSearch InterpolatingPolynomialMinimizingStepSize");
				return Double.NaN;
			}
		    // We are expanding the search (during a Wolfe bracketing phase) using
		    // BISECTION interpolation.  Using BISECTION when trying to expand is
		    // strictly speaking an oxymoron, but we define this to mean always taking
		    // the maximum step size so that the Armijo & Wolfe implementations are
		    // agnostic to the interpolation type.
		    return max_step_size;
		  }
		  // Only check if lower-bound is valid here, where it is required
		  // to avoid replicating current.value_is_valid == false
		  // behaviour in WolfeLineSearch.
		  if (!lowerbound.value_is_valid) {
		      // std::scientific << std::setprecision(kErrorMessageNumericPrecision)
		      System.err.println("Ceres bug: lower-bound sample for interpolation is invalid, ");
		      System.err.println("please contact the developers!, interpolation_type: "
		      + LineSearchInterpolationTypeToString(interpolation_type));
		      System.err.println("lowerbound: "); 
		      System.err.println(lowerbound.ToDebugString()); 
		      System.err.println(" previous: " );
		      System.err.println(previous.ToDebugString());
		      System.err.println("current: ");
		      System.err.println(current.ToDebugString());
		      return Double.NaN;
		  }

		  // Select step size by interpolating the function and gradient values
		  // and minimizing the corresponding polynomial.
		  Vector<FunctionSample> samples = new Vector<FunctionSample>();
		  samples.add(lowerbound);

		  if (interpolation_type == LineSearchInterpolationType.QUADRATIC) {
		    // Two point interpolation using function values and the
		    // gradient at the lower bound.
		    samples.add(new FunctionSample(current.x, current.value[0]));

		    if (previous.value_is_valid) {
		      // Three point interpolation, using function values and the
		      // gradient at the lower bound.
		      samples.add(new FunctionSample(previous.x, previous.value[0]));
		    }
		  } 
		  else if (interpolation_type == LineSearchInterpolationType.CUBIC) {
		    // Two point interpolation using the function values and the gradients.
		    samples.add(current);

		    if (previous.value_is_valid) {
		      // Three point interpolation using the function values and
		      // the gradients.
		      samples.add(previous);
		    }
		  } 
		  else {
		    System.err.println("Ceres bug: No handler for interpolation_type: " +
		               LineSearchInterpolationTypeToString(interpolation_type));
		    System.err.println("Please contact the developers!");
		    return Double.NaN;
		  }

		  double step_size[] = new double[] {0.0};
		  double unused_min_value[] = new double[] {0.0};
		  MinimizeInterpolatingPolynomial(samples, min_step_size, max_step_size,
		                                  step_size, unused_min_value);
		  return step_size[0];
		}


	} // abstract class LineSearch
	
	public void copyFunctionSample(FunctionSample dst, FunctionSample src) {
		dst.x = src.x;
		dst.vector_x.clear();
		dst.vector_x.addAll(src.vector_x);
		dst.vector_x_is_valid = src.vector_x_is_valid;
		dst.value[0] = src.value[0];
		dst.value_is_valid = src.value_is_valid;
		dst.vector_gradient.clear();
		dst.vector_gradient.addAll(src.vector_gradient);
		dst.vector_gradient_is_valid = src.vector_gradient_is_valid;
		dst.gradient = src.gradient;
		dst.gradient_is_valid = src.gradient_is_valid;
	}
	
	// FunctionSample is used by the line search routines to store and
	// communicate the value and (optionally) the gradient of the function
	// being minimized.
	//
	// Since line search as the name implies happens along a certain
	// line/direction. FunctionSample contains the information in two
	// ways. Information in the ambient space and information along the
	// direction of search.
	class FunctionSample {
		  // x is the location of the sample along the search direction.
		  public double x;

		  // Let p be a point and d be the search direction then
		  //
		  // vector_x = p + x * d;
		  public Vector<Double> vector_x;
		  // True if vector_x has been assigned a valid value.
		  public boolean vector_x_is_valid;

		  // value = f(vector_x)
		  public double value[] = new double[1];
		  // True of the evaluation was successful and value is a finite
		  // number.
		  public boolean value_is_valid;

		  // vector_gradient = Df(vector_position);
		  //
		  // D is the derivative operator.
		  public Vector<Double> vector_gradient;
		  // True if the vector gradient was evaluated and the evaluation was
		  // successful (the value is a finite number).
		  public boolean vector_gradient_is_valid;

		  // gradient = d.transpose() * vector_gradient
		  //
		  // where d is the search direction.
		  public double gradient;
		  // True if the evaluation of the gradient was sucessful and the
		  // value is a finite number.
		  public boolean gradient_is_valid;
	      public FunctionSample() {
	    	  x = 0.0;
	    	  vector_x = new Vector<Double>();
	          vector_x_is_valid = false;
	          value[0] = 0.0;
	          value_is_valid = false;
	          vector_gradient = new Vector<Double>();
	          vector_gradient_is_valid = false;
	          gradient = 0.0;
	          gradient_is_valid = false; 
	      }
	  
	      public FunctionSample(double x, double value) {
	    	  this.x = x;
	    	  vector_x = new Vector<Double>();
	          vector_x_is_valid = false;
	          this.value[0] = value;
	          value_is_valid = true;
	          vector_gradient = new Vector<Double>();
	          vector_gradient_is_valid = false;
	          gradient = 0.0;
	          gradient_is_valid = false; 
	      }
	      
	      public FunctionSample(double x, double value, double gradient) {
	    	  this.x = x;
	    	  vector_x = new Vector<Double>();
	          vector_x_is_valid = false;
	          this.value[0] = value;
	          value_is_valid = true;
	          vector_gradient = new Vector<Double>();
	          vector_gradient_is_valid = false;
	          this.gradient = gradient;
	          gradient_is_valid = true; 
	      }

	      public String ToDebugString() {
	    	  return String.format("[x: %.8e, value: %.8e, gradient: %.8e, " +
                      "value_is_valid: %b, gradient_is_valid: %b]",
                      x, value[0], gradient, value_is_valid, gradient_is_valid);  
	      }

	  
	};
	
	// Result of the line search.
	  class LineSearchSummary {
		    public boolean success;
		    FunctionSample optimal_point;
		    public int num_function_evaluations;
		    public int num_gradient_evaluations;
		    public int num_iterations;
		    // Cumulative time spent evaluating the value of the cost function across
		    // all iterations.
		    public double cost_evaluation_time_in_seconds[] = new double[1];
		    // Cumulative time spent evaluating the gradient of the cost function across
		    // all iterations.
		    public double gradient_evaluation_time_in_seconds[] = new double[1];
		    // Cumulative time spent minimizing the interpolating polynomial to compute
		    // the next candidate step size across all iterations.
		    public double polynomial_minimization_time_in_seconds;
		    public double total_time_in_seconds;
		    public String error;
	        public LineSearchSummary() {
	          success = false;
	          num_function_evaluations = 0;
	          num_gradient_evaluations = 0;
	          num_iterations = 0;
	          cost_evaluation_time_in_seconds[0] = -1.0;
	          gradient_evaluation_time_in_seconds[0] = -1.0;
	          polynomial_minimization_time_in_seconds = -1.0;
	          total_time_in_seconds = -1.0;
	        }

	    
	  };
	
	class LineSearchOptions {
		// Degree of the polynomial used to approximate the objective
	    // function.
	    public LineSearchInterpolationType interpolation_type;

	    // Armijo and Wolfe line search parameters.

	    // Solving the line search problem exactly is computationally
	    // prohibitive. Fortunately, line search based optimization
	    // algorithms can still guarantee convergence if instead of an
	    // exact solution, the line search algorithm returns a solution
	    // which decreases the value of the objective function
	    // sufficiently. More precisely, we are looking for a step_size
	    // s.t.
	    //
	    //  f(step_size) <= f(0) + sufficient_decrease * f'(0) * step_size
	    public double sufficient_decrease;

	    // In each iteration of the Armijo / Wolfe line search,
	    //
	    // new_step_size >= max_step_contraction * step_size
	    //
	    // Note that by definition, for contraction:
	    //
	    //  0 < max_step_contraction < min_step_contraction < 1
	    //
	    public double max_step_contraction;

	    // In each iteration of the Armijo / Wolfe line search,
	    //
	    // new_step_size <= min_step_contraction * step_size
	    // Note that by definition, for contraction:
	    //
	    //  0 < max_step_contraction < min_step_contraction < 1
	    //
	    public double min_step_contraction;

	    // If during the line search, the step_size falls below this
	    // value, it is truncated to zero.
	    public double min_step_size;

	    // Maximum number of trial step size iterations during each line search,
	    // if a step size satisfying the search conditions cannot be found within
	    // this number of trials, the line search will terminate.
	    public int max_num_iterations;

	    // Wolfe-specific line search parameters.

	    // The strong Wolfe conditions consist of the Armijo sufficient
	    // decrease condition, and an additional requirement that the
	    // step-size be chosen s.t. the _magnitude_ ('strong' Wolfe
	    // conditions) of the gradient along the search direction
	    // decreases sufficiently. Precisely, this second condition
	    // is that we seek a step_size s.t.
	    //
	    //   |f'(step_size)| <= sufficient_curvature_decrease * |f'(0)|
	    //
	    // Where f() is the line search objective and f'() is the derivative
	    // of f w.r.t step_size (d f / d step_size).
	    public double sufficient_curvature_decrease;

	    // During the bracketing phase of the Wolfe search, the step size is
	    // increased until either a point satisfying the Wolfe conditions is
	    // found, or an upper bound for a bracket containing a point satisfying
	    // the conditions is found.  Precisely, at each iteration of the
	    // expansion:
	    //
	    //   new_step_size <= max_step_expansion * step_size.
	    //
	    // By definition for expansion, max_step_expansion > 1.0.
	    public double max_step_expansion;

	    public boolean is_silent;

	    // The one dimensional function that the line search algorithm
	    // minimizes.
	    LineSearchFunction function;
	    public LineSearchOptions() {
	          interpolation_type = LineSearchInterpolationType.CUBIC;
	          sufficient_decrease = 1e-4;
	          max_step_contraction = 1e-3;
	          min_step_contraction = 0.9;
	          min_step_size = 1e-9;
	          max_num_iterations = 200;
	          sufficient_curvature_decrease = 0.9;
	          max_step_expansion = 10.0;
	          is_silent = false;
	          function = null; 
	    }

	    
	  };
	
	// An object used by the line search to access the function values
	// and gradient of the one dimensional function being optimized.
	//
	// In practice, this object provides access to the objective
	// function value and the directional derivative of the underlying
	// optimization problem along a specific search direction.
	class LineSearchFunction {
			  private Evaluator evaluator_;
			  private Vector<Double> position_;
			  private Vector<Double> direction_;

			  // scaled_direction = x * direction_;
			  private Vector<Double> scaled_direction_;

			  // We may not exclusively own the evaluator (e.g. in the Trust Region
			  // minimizer), hence we need to save the initial evaluation durations for the
			  // value & gradient to accurately determine the duration of the evaluations
			  // we invoked.  These are reset by a call to ResetTimeStatistics().
			  private double initial_evaluator_residual_time_in_seconds;
			  private double initial_evaluator_jacobian_time_in_seconds;
			  
			  public LineSearchFunction(Evaluator evaluator) {
			      evaluator_ = evaluator;
			      position_ = new Vector<Double>(evaluator.NumParameters());
			      direction_ = new Vector<Double>(evaluator.NumEffectiveParameters());
			      scaled_direction_ = new Vector<Double>(evaluator.NumEffectiveParameters());
			      initial_evaluator_residual_time_in_seconds = 0.0;
			      initial_evaluator_jacobian_time_in_seconds = 0.0;
			  }

			public void Init(Vector<Double> position, Vector<Double> direction) {
			  position_ = position;
			  direction_ = direction;
			}
			
			public void Init(double[] position, Vector<Double> direction) {
				position_ = new Vector<Double>(position.length);
				for (int i = 0; i < position.length; i++) {
					position_.add(position[i]);
				}
				direction_ = direction;
			}

	  // Evaluate the line search objective
	  //
	  //   f(x) = p(position + x * direction)
	  //
	  // Where, p is the objective function of the general optimization
	  // problem.
	  //
	  // evaluate_gradient controls whether the gradient will be evaluated
	  // or not.
	  //
	  // On return output->*_is_valid indicate indicate whether the
	  // corresponding fields have numerically valid values or not.
	  public void Evaluate(double x,
                    boolean evaluate_gradient,
                    FunctionSample output) {
		    int i;
			output.x = x;
			output.vector_x_is_valid = false;
			output.value_is_valid = false;
			output.gradient_is_valid = false;
			output.vector_gradient_is_valid = false;
			
			scaled_direction_.clear();
			for (i = 0; i < direction_.size(); i++) {
				scaled_direction_.add(output.x * direction_.get(i));
			}
			while(output.vector_x.size() < position_.size()) {
				output.vector_x.add(0.0);
			}
			while (output.vector_x.size() > position_.size()) {
				output.vector_x.removeElementAt(output.vector_x.size()-1);
			}
			if (!evaluator_.Plus(position_,
			          scaled_direction_,
			          output.vector_x)) {
			return;
			}
			output.vector_x_is_valid = true;
			
			double[] gradient = null;
			if (evaluate_gradient) {
			while (output.vector_gradient.size() < direction_.size()) {
				output.vector_gradient.add(0.0);
			}
			while (output.vector_gradient.size() > direction_.size()) {
				output.vector_gradient.removeElementAt(output.vector_gradient.size()-1);
			}
			gradient = new double[direction_.size()];
			for (i = 0; i < direction_.size(); i++) {
				gradient[i] = output.vector_gradient.get(i);
			}
			}
			double vector_x_array[] = new double[output.vector_x.size()];
			for (i = 0; i < output.vector_x.size(); i++) {
				vector_x_array[i] = output.vector_x.get(i);
			}
			boolean eval_status = evaluator_.Evaluate(
			vector_x_array, (output.value), null, gradient, null);
			for (i = 0; i < output.vector_x.size(); i++) {
				output.vector_x.set(i,vector_x_array[i]);
			}
			for (i = 0; i <output.vector_gradient.size(); i++) {
				output.vector_gradient.set(i,gradient[i]);
			}
			if (!eval_status || !Double.isFinite(output.value[0])) {
			return;
			}
			
			output.value_is_valid = true;
			if (!evaluate_gradient) {
			return;
			}
			
			output.gradient = 0.0;
			for (i = 0; i < direction_.size(); i++) {
				output.gradient += (direction_.get(i)*output.vector_gradient.get(i));
			}
			if (!Double.isFinite(output.gradient)) {
			return;
			}
			
			output.gradient_is_valid = true;
			output.vector_gradient_is_valid = true;
			}
	  
	  public double DirectionInfinityNorm() {
		  double maxVal = 0.0;
		  for (int i = 0; i < direction_.size(); i++) {
			  if (Math.abs(direction_.get(i)) > maxVal) {
				  maxVal = Math.abs(direction_.get(i));
			  }
		  }
		  return maxVal;
		}
	  
	  // Resets to now, the start point for the results from TimeStatistics().
	  public void ResetTimeStatistics() {
		  HashMap<String, CallStatistics> evaluator_statistics =
		      evaluator_.Statistics();

		  initial_evaluator_residual_time_in_seconds =
		      FindWithDefault(
		          evaluator_statistics, "Evaluator::Residual", new CallStatistics())
		          .time;
		  initial_evaluator_jacobian_time_in_seconds =
		      FindWithDefault(
		          evaluator_statistics, "Evaluator::Jacobian", new CallStatistics())
		          .time;
		}
	  
	  public void TimeStatistics(
			    double[] cost_evaluation_time_in_seconds,
			    double[] gradient_evaluation_time_in_seconds) {
			  HashMap<String, CallStatistics> evaluator_time_statistics =
			      evaluator_.Statistics();
			  cost_evaluation_time_in_seconds[0] =
			      FindWithDefault(
			          evaluator_time_statistics, "Evaluator::Residual", new CallStatistics())
			          .time -
			      initial_evaluator_residual_time_in_seconds;
			  // Strictly speaking this will slightly underestimate the time spent
			  // evaluating the gradient of the line search univariate cost function as it
			  // does not count the time spent performing the dot product with the direction
			  // vector.  However, this will typically be small by comparison, and also
			  // allows direct subtraction of the timing information from the totals for
			  // the evaluator returned in the solver summary.
			  gradient_evaluation_time_in_seconds[0] =
			      FindWithDefault(
			          evaluator_time_statistics, "Evaluator::Jacobian", new CallStatistics())
			          .time -
			      initial_evaluator_jacobian_time_in_seconds;
			}

	  public Vector<Double> position() { return position_; }
	  public Vector<Double> direction() { return direction_; }

	 
	} // class LineSearchFunction
	
	class TrustRegionStepEvaluator {
			  private int max_consecutive_nonmonotonic_steps_;
			  // The minimum cost encountered up till now.
			  private double minimum_cost_;
			  // The current cost of the trust region minimizer as informed by the
			  // last call to StepAccepted.
			  private double current_cost_;
			  private double reference_cost_;
			  private double candidate_cost_;
			  // Accumulated model cost since the last time the reference model
			  // cost was updated, i.e., when a step with cost less than the
			  // current known minimum cost is accepted.
			  private double accumulated_reference_model_cost_change_;
			  // Accumulated model cost since the last time the candidate model
			  // cost was updated, i.e., a non-monotonic step was taken with a
			  // cost that was greater than the current candidate cost.
			  private double accumulated_candidate_model_cost_change_;
			  // Number of steps taken since the last time minimum_cost was updated.
			  private int num_consecutive_nonmonotonic_steps_;
			  // initial_cost is as the name implies the cost of the starting
			  // state of the trust region minimizer.
			  //
			  // max_consecutive_nonmonotonic_steps controls the window size used
			  // by the step selection algorithm to accept non-monotonic
			  // steps. Setting this parameter to zero, recovers the classic
			  // montonic descent algorithm.
			  public TrustRegionStepEvaluator(double initial_cost,
			                           int max_consecutive_nonmonotonic_steps) {
				  max_consecutive_nonmonotonic_steps_ = max_consecutive_nonmonotonic_steps;
			      minimum_cost_ = initial_cost;
			      current_cost_ = initial_cost;
			      reference_cost_ = initial_cost;
			      candidate_cost_ = initial_cost;
			      accumulated_reference_model_cost_change_ = 0.0;
			      accumulated_candidate_model_cost_change_ = 0.0;
			      num_consecutive_nonmonotonic_steps_ = 0; 
			  }
	
			  // Return the quality of the step given its cost and the decrease in
			  // the cost of the model. model_cost_change has to be positive.
			  public double StepQuality(double cost, double model_cost_change) {
				  double relative_decrease = (current_cost_ - cost) / model_cost_change;
				  double historical_relative_decrease =
				      (reference_cost_ - cost) /
				      (accumulated_reference_model_cost_change_ + model_cost_change);
				  return Math.max(relative_decrease, historical_relative_decrease);  
			  };
	
			  // Inform the step evaluator that a step with the given cost and
			  // model_cost_change has been accepted by the trust region
			  // minimizer.
			  public void StepAccepted(double cost, double model_cost_change) {
				// Algorithm 10.1.2 from Trust Region Methods by Conn, Gould &
				  // Toint.
				  //
				  // Step 3a
				  current_cost_ = cost;
				  accumulated_candidate_model_cost_change_ += model_cost_change;
				  accumulated_reference_model_cost_change_ += model_cost_change;

				  // Step 3b.
				  if (current_cost_ < minimum_cost_) {
				    minimum_cost_ = current_cost_;
				    num_consecutive_nonmonotonic_steps_ = 0;
				    candidate_cost_ = current_cost_;
				    accumulated_candidate_model_cost_change_ = 0.0;
				  } else {
				    // Step 3c.
				    ++num_consecutive_nonmonotonic_steps_;
				    if (current_cost_ > candidate_cost_) {
				      candidate_cost_ = current_cost_;
				      accumulated_candidate_model_cost_change_ = 0.0;
				    }
				  }

				  // Step 3d.
				  //
				  // At this point we have made too many non-monotonic steps and
				  // we are going to reset the value of the reference iterate so
				  // as to force the algorithm to descend.
				  //
				  // Note: In the original algorithm by Toint, this step was only
				  // executed if the step was non-monotonic, but that would not handle
				  // the case of max_consecutive_nonmonotonic_steps = 0. The small
				  // modification of doing this always handles that corner case
				  // correctly.
				  if (num_consecutive_nonmonotonic_steps_ ==
				      max_consecutive_nonmonotonic_steps_) {
				    reference_cost_ = candidate_cost_;
				    accumulated_reference_model_cost_change_ =
				        accumulated_candidate_model_cost_change_;
				  }
  
			  }

		 
		};
		
		class TrustRegionStrategyOptions {
			  public TrustRegionStrategyType trust_region_strategy_type;
			  // Linear solver used for actually solving the trust region step.
			  public LinearSolver linear_solver;
			  public double initial_radius;
			  public double max_radius;

			  // Minimum and maximum values of the diagonal damping matrix used
			  // by LevenbergMarquardtStrategy. The DoglegStrategy also uses
			  // these bounds to construct a regularizing diagonal to ensure
			  // that the Gauss-Newton step computation is of full rank.
			  public double min_lm_diagonal;
			  public double max_lm_diagonal;

			  // Further specify which dogleg method to use
			  public DoglegType dogleg_type;
		      public TrustRegionStrategyOptions() {
		          trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		          initial_radius = 1e4;
		          max_radius = 1e32;
		          min_lm_diagonal = 1e-6;
		          max_lm_diagonal = 1e32;
		          dogleg_type = DoglegType.TRADITIONAL_DOGLEG;
		    }
		  } // class TrustStrategyRegionOptions
		
		  // Per solve options.
		  class TrustRegionStrategyPerSolveOptions {
			    // Forcing sequence for inexact solves.
			    public double eta;

			    public DumpFormatType dump_format_type;

			    // If non-empty and dump_format_type is not CONSOLE, the trust
			    // regions strategy will write the linear system to file(s) with
			    // name starting with dump_filename_base.  If dump_format_type is
			    // CONSOLE then dump_filename_base will be ignored and the linear
			    // system will be written to the standard error.
			    public String dump_filename_base;
			    public TrustRegionStrategyPerSolveOptions() {
			        eta = 0.0;
			        dump_format_type = DumpFormatType.TEXTFILE;
			    }

			    
			  } // class TrustStrategyPerSolveOptions
		  
		  class TrustRegionStrategySummary {
			    // If the trust region problem is,
			    //
			    //   1/2 x'Ax + b'x + c,
			    //
			    // then
			    //
			    //   residual_norm = |Ax -b|
			    public double residual_norm;

			    // Number of iterations used by the linear solver. If a linear
			    // solver was not called (e.g., DogLegStrategy after an
			    // unsuccessful step), then this would be zero.
			    public int num_iterations;

			    // Status of the linear solver used to solve the Newton system.
			    public LinearSolverTerminationType termination_type;
		        public TrustRegionStrategySummary() {
		          residual_norm = 0.0;
		          num_iterations = -1;
		          termination_type = LinearSolverTerminationType.LINEAR_SOLVER_FAILURE;
		    }
		    
		    
		  } // class Summary

	
	abstract class TrustRegionStrategy {
		 public TrustRegionStrategyOptions options;
		 public TrustRegionStrategyPerSolveOptions perSolveOptions;
		 public TrustRegionStrategySummary summary;
		 
		 public TrustRegionStrategy() {
			 options = new TrustRegionStrategyOptions();
			 perSolveOptions = new TrustRegionStrategyPerSolveOptions();
			 summary = new TrustRegionStrategySummary();
		 }

			  
			  
			  // Use the current radius to solve for the trust region step.
			  public abstract TrustRegionStrategySummary ComputeStep(TrustRegionStrategyPerSolveOptions per_solve_options,
			                              SparseMatrix jacobian,
			                              double residuals[],
			                              double step[]);

			  // Inform the strategy that the current step has been accepted, and
			  // that the ratio of the decrease in the non-linear objective to the
			  // decrease in the trust region model is step_quality.
			  public abstract void StepAccepted(double step_quality);

			  // Inform the strategy that the current step has been rejected, and
			  // that the ratio of the decrease in the non-linear objective to the
			  // decrease in the trust region model is step_quality.
			  public abstract void StepRejected(double step_quality);

			  // Inform the strategy that the current step has been rejected
			  // because it was found to be numerically invalid.
			  // StepRejected/StepAccepted will not be called for this step, and
			  // the strategy is free to do what it wants with this information.
			  public abstract void StepIsInvalid();

			  // Current trust region radius.
			  public abstract double Radius();

	} // class TrustRegionStrategy
	
	public TrustRegionStrategy  Create(TrustRegionStrategyOptions options) {
		  switch (options.trust_region_strategy_type) {
		    case LEVENBERG_MARQUARDT:
		      return new LevenbergMarquardtStrategy(options);
		    case DOGLEG:
		      return new DoglegStrategy(options);
		    default:
		      System.err.println("Unknown trust region strategy: " + options.trust_region_strategy_type);
		      return null;
		  }

		  
		}
	
	// Levenberg-Marquardt step computation and trust region sizing
	// strategy based on on "Methods for Nonlinear Least Squares" by
	// K. Madsen, H.B. Nielsen and O. Tingleff. Available to download from
	//
	// http://www2.imm.dtu.dk/pubdb/views/edoc_download.php/3215/pdf/imm3215.pdf
	class LevenbergMarquardtStrategy extends TrustRegionStrategy {
		  private LinearSolver linear_solver_;
		  private double radius_;
		  private double max_radius_;
		  private double min_diagonal_;
		  private double max_diagonal_;
		  private double decrease_factor_;
		  private boolean reuse_diagonal_;
		  private Vector<Double> diagonal_;   // diagonal_ =  diag(J'J)
		  // Scaled copy of diagonal_. Stored here as optimization to prevent
		  // allocations in every iteration and reuse when a step fails and
		  // ComputeStep is called again.
		  private Vector<Double> lm_diagonal_;  // lm_diagonal_ = diagonal_ / radius_;
		  
	      public LevenbergMarquardtStrategy(TrustRegionStrategyOptions options) {
	    	  super();
	    	  linear_solver_ = options.linear_solver;
	          radius_ = options.initial_radius;
	          max_radius_ = options.max_radius;
	          min_diagonal_ = options.min_lm_diagonal;
	          max_diagonal_ = options.max_lm_diagonal;
	          decrease_factor_ = 2.0;
	          reuse_diagonal_ = false;
	          diagonal_ = new Vector<Double>();
	          lm_diagonal_ = new Vector<Double>();
	          if (linear_solver_ == null) {
	        	  System.err.println("linear_solver_ == null in public LevenbergMarquardtStrategy");
	        	  return;
	          }
	          if (min_diagonal_ <= 0.0) {
	        	  System.err.println("min_diagonal_ <= 0.0 in public LevenbergMarquardtStrategy");
	        	  return;
	          }
	          if (min_diagonal_ > max_diagonal_) {
	        	  System.err.println("min_diagonal_ > max_diagonal_ in public LevenbergMarquardtStrategy");
	        	  return;
	          }
	          if (max_radius_ <= 0.0) {
	        	  System.err.println("max_radius_ <= 0.0 in public LevenbergMarquardtStrategy");
	        	  return;
	          }
	      }

	  // TrustRegionStrategy interface
	  public TrustRegionStrategySummary ComputeStep(
	      TrustRegionStrategyPerSolveOptions per_solve_options,
	      SparseMatrix jacobian,
	      double[] residuals,
	      double[] step) {
		  int i;
		  if (jacobian == null) {
			  System.err.println("jacobian == null in LevenbergMarquardtStrategy TrustRegionStrategy.Summary ComputeStep");
			  return null;
		  }
		  if (residuals == null) {
			  System.err.println("residuals == null in LevenbergMarquardtStrategy TrustRegionStrategy.Summary ComputeStep");
			  return null;
		  }
		  if (step == null) {
			  System.err.println("step == null in LevenbergMarquardtStrategy TrustRegionStrategy.Summary ComputeStep");
			  return null;
		  }

		  int num_parameters = jacobian.num_cols();
		  LinearSolverSummary linear_solver_summary = null;
		  if (!reuse_diagonal_) {
		    if (diagonal_.size() != num_parameters) {
		      while (diagonal_.size() < num_parameters) {
		    	  diagonal_.add(1.0);
		      }
		      while (diagonal_.size() > num_parameters) {
		    	  diagonal_.removeElementAt(diagonal_.size()-1);
		      }
		    }

		    double diagonal_array[] = new double[diagonal_.size()];
		    for (i = 0; i < diagonal_.size(); i++) {
		    	diagonal_array[i] = diagonal_.get(i);
		    }
		    
		    jacobian.SquaredColumnNorm(diagonal_array);
		    for (i = 0; i < num_parameters; ++i) {
		      diagonal_.set(i,Math.min(Math.max(diagonal_array[i], min_diagonal_),
		                              max_diagonal_));
		    }
		  }

		  lm_diagonal_.clear();
		  for (i = 0; i < num_parameters; ++i) {
			  lm_diagonal_.add(Math.sqrt(diagonal_.get(i)/radius_));
		  }

		  LinearSolverPerSolveOptions solve_options = new LinearSolverPerSolveOptions();
		  solve_options.D = new double[lm_diagonal_.size()];
		  for (i = 0; i < lm_diagonal_.size(); i++) {
			  solve_options.D[i] = lm_diagonal_.get(i);
		  }
		  solve_options.q_tolerance = per_solve_options.eta;
		  // Disable r_tolerance checking. Since we only care about
		  // termination via the q_tolerance. As Nash and Sofer show,
		  // r_tolerance based termination is essentially useless in
		  // Truncated Newton methods.
		  solve_options.r_tolerance = -1.0;

		  // Invalidate the output array lm_step, so that we can detect if
		  // the linear solver generated numerical garbage.  This is known
		  // to happen for the DENSE_QR and then DENSE_SCHUR solver when
		  // the Jacobian is severly rank deficient and mu is too small.
		  InvalidateArray(num_parameters, step);

		  // Instead of solving Jx = -r, solve Jy = r.
		  // Then x can be found as x = -y, but the inputs jacobian and residuals
		  // do not need to be modified.
		  
		  linear_solver_summary =
			      linear_solver_.Solve(jacobian, residuals, solve_options, step);

		  if (linear_solver_summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_FATAL_ERROR) {
		    Preferences.debug("Linear solver fatal error: " + linear_solver_summary.message[0] + "\n",
		    		Preferences.DEBUG_ALGORITHM);
		  } else if (linear_solver_summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_FAILURE)  {
		    Preferences.debug("Linear solver failure. Failed to compute a step: " + linear_solver_summary.message[0] + "\n",
		    		Preferences.DEBUG_ALGORITHM);
		  } else if (!IsArrayValid(num_parameters, step)) {
		    Preferences.debug("Linear solver failure. Failed to compute a finite step.\n", Preferences.DEBUG_ALGORITHM);
		    linear_solver_summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_FAILURE;
		  } else {
			for (i = 0; i < num_parameters; i++) {
				step[i] *= -1.0;
			}
		  }
		  reuse_diagonal_ = true;

		  if (per_solve_options.dump_format_type == DumpFormatType.CONSOLE ||
		      (per_solve_options.dump_format_type != DumpFormatType.CONSOLE &&
		       per_solve_options.dump_filename_base != null &&
		       !per_solve_options.dump_filename_base.isEmpty())) {
		    if (!DumpLinearLeastSquaresProblem(per_solve_options.dump_filename_base,
		                                       per_solve_options.dump_format_type,
		                                       jacobian,
		                                       solve_options.D,
		                                       residuals,
		                                       step,
		                                       0)) {
		      System.err.println("Unable to dump trust region problem.");
		      System.err.println(" Filename base: " + per_solve_options.dump_filename_base);
		    }
		  }


		  summary.residual_norm = linear_solver_summary.residual_norm;
		  summary.num_iterations = linear_solver_summary.num_iterations;
		  summary.termination_type = linear_solver_summary.termination_type;
		  return summary;

	  }
	  
	  public void StepAccepted(double step_quality) {
		  if (step_quality <= 0.0) {
			  System.err.println("step_quality <= 0.0 in LevenbergMarquardtStrtegy StepAccepted");
			  return;
		  }
		  radius_ = radius_ / Math.max(1.0 / 3.0,
		                               1.0 - Math.pow(2.0 * step_quality - 1.0, 3));
		  radius_ = Math.min(max_radius_, radius_);
		  decrease_factor_ = 2.0;
		  reuse_diagonal_ = false;

	  }
	  
	  public void StepRejected(double step_quality) {
		  radius_ = radius_ / decrease_factor_;
		  decrease_factor_ *= 2.0;
		  reuse_diagonal_ = true;
	  }
	  
	  public void StepIsInvalid() {
	    // Treat the current step as a rejected step with no increase in
	    // solution quality. Since rejected steps lead to decrease in the
	    // size of the trust region, the next time ComputeStep is called,
	    // this will lead to a better conditioned system.
	    StepRejected(0.0);
	  }

	  public double Radius() {
		  return radius_;
	  }

	} // class LevenbergMarquardtStrategy extends TrustRegionStrategy
	
	// Dogleg step computation and trust region sizing strategy based on
	// on "Methods for Nonlinear Least Squares" by K. Madsen, H.B. Nielsen
	// and O. Tingleff. Available to download from
	//
	// http://www2.imm.dtu.dk/pubdb/views/edoc_download.php/3215/pdf/imm3215.pdf
	//
	// One minor modification is that instead of computing the pure
	// Gauss-Newton step, we compute a regularized version of it. This is
	// because the Jacobian is often rank-deficient and in such cases
	// using a direct solver leads to numerical failure.
	//
	// If SUBSPACE is passed as the type argument to the constructor, the
	// DoglegStrategy follows the approach by Shultz, Schnabel, Byrd.
	// This finds the exact optimum over the two-dimensional subspace
	// spanned by the two Dogleg vectors.
	class DoglegStrategy extends TrustRegionStrategy {
		  private LinearSolver linear_solver_;
		  private double radius_;
		  private double max_radius_;

		  private double min_diagonal_;
		  private double max_diagonal_;

		  // mu is used to scale the diagonal matrix used to make the
		  // Gauss-Newton solve full rank. In each solve, the strategy starts
		  // out with mu = min_mu, and tries values upto max_mu. If the user
		  // reports an invalid step, the value of mu_ is increased so that
		  // the next solve starts with a stronger regularization.
		  //
		  // If a successful step is reported, then the value of mu_ is
		  // decreased with a lower bound of min_mu_.
		  private double mu_;
		  private double min_mu_;
		  private double max_mu_;
		  private double mu_increase_factor_;
		  private double increase_threshold_;
		  private double decrease_threshold_;

		  private Vector<Double> diagonal_;  // sqrt(diag(J^T J))
		  private Vector<Double> lm_diagonal_;

		  private Vector<Double> gradient_;
		  private Vector<Double> gauss_newton_step_;

		  // cauchy_step = alpha * gradient
		  private double alpha_;
		  private double dogleg_step_norm_;

		  // When, ComputeStep is called, reuse_ indicates whether the
		  // Gauss-Newton and Cauchy steps from the last call to ComputeStep
		  // can be reused or not.
		  //
		  // If the user called StepAccepted, then it is expected that the
		  // user has recomputed the Jacobian matrix and new Gauss-Newton
		  // solve is needed and reuse is set to false.
		  //
		  // If the user called StepRejected, then it is expected that the
		  // user wants to solve the trust region problem with the same matrix
		  // but a different trust region radius and the Gauss-Newton and
		  // Cauchy steps can be reused to compute the Dogleg, thus reuse is
		  // set to true.
		  //
		  // If the user called StepIsInvalid, then there was a numerical
		  // problem with the step computed in the last call to ComputeStep,
		  // and the regularization used to do the Gauss-Newton solve is
		  // increased and a new solve should be done when ComputeStep is
		  // called again, thus reuse is set to false.
		  private boolean reuse_;

		  // The dogleg type determines how the minimum of the local
		  // quadratic model is found.
		  private DoglegType dogleg_type_;

		  // If the type is SUBSPACE_DOGLEG, the two-dimensional
		  // model 1/2 x^T B x + g^T x has to be computed and stored.
		  private boolean subspace_is_one_dimensional_;
		  private Matrix subspace_basis_;
		  private Vector2d subspace_g_ = new Vector2d();
		  // Matrix2d subspace_B_;
		  private Matrix subspace_B_;
		  
		  private double kMaxMu = 1.0;
		  private double kMinMu = 1e-8;

	 
	      public DoglegStrategy(TrustRegionStrategyOptions options) {
	    	  super();
	    	  linear_solver_ = options.linear_solver;
	          radius_ = options.initial_radius;
	          max_radius_ = options.max_radius;
	          min_diagonal_ = options.min_lm_diagonal;
	          max_diagonal_ = options.max_lm_diagonal;
	          mu_ = kMinMu;
	          min_mu_ = kMinMu;
	          max_mu_ = kMaxMu;
	          mu_increase_factor_ = 10.0;
	          increase_threshold_ = 0.75;
	          decrease_threshold_ = 0.25;
	          dogleg_step_norm_ = 0.0;
	          reuse_ = false;
	          dogleg_type_ = options.dogleg_type;
	          if (linear_solver_ == null) {
	        	  System.err.println("linear_solver_ == null in public DoglegStrategy");
	        	  return;
	          }
	          if (min_diagonal_ <= 0.0) {
	        	  System.err.println("min_diagonal_ <= 0.0 in public DoglegStrategy");
	        	  return;
	          }
	          if (min_diagonal_ > max_diagonal_) {
	        	  System.err.println("min_diagonal_ > max_diagonal_ in public DoglegStrategy");
	        	  return;
	          }
	          if (max_radius_ <= 0.0) {
	        	  System.err.println("max_radius_ <= 0.0 in public DoglegStrategy");
	        	  return;
	          }
              diagonal_ = new Vector<Double>();
              lm_diagonal_ = new Vector<Double>();
              gradient_ = new Vector<Double>();
              gauss_newton_step_ = new Vector<Double>();
	      }

	  // TrustRegionStrategy interface
	  public TrustRegionStrategySummary ComputeStep(TrustRegionStrategyPerSolveOptions per_solve_options,
	                              SparseMatrix jacobian,
	                              double residuals[],
	                              double step[]) {
		  int i;
		  if (jacobian == null) {
			  System.err.println("jacobian == null in DoglegStrategy TrustRegionStrategy.Summary ComputeStep");
			  return null;
		  }
		  if (residuals == null) {
			  System.err.println("residuals == null in DoglegStrategy TrustRegionStrategy.Summary ComputeStep");
			  return null;
		  }
		  if (step == null) {
			  System.err.println("step == null in DoglegStrategy TrustRegionStrategy.Summary ComputeStep");
			  return null;
		  }

		  int n = jacobian.num_cols();
		  if (reuse_) {
		    // Gauss-Newton and gradient vectors are always available, only a
		    // new interpolant need to be computed. For the subspace case,
		    // the subspace and the two-dimensional model are also still valid.
		    switch (dogleg_type_) {
		      case TRADITIONAL_DOGLEG:
		        ComputeTraditionalDoglegStep(step);
		        break;

		      case SUBSPACE_DOGLEG:
		        ComputeSubspaceDoglegStep(step);
		        break;
		    }
		    
		    summary.num_iterations = 0;
		    summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_SUCCESS;
		    return summary;
		  }

		  reuse_ = true;
		  // Check that we have the storage needed to hold the various
		  // temporary vectors.
		  if (diagonal_.size() != n) {
			while (diagonal_.size() < n) {
				diagonal_.add(1.0);
			}
			while (diagonal_.size() > n) {
				diagonal_.removeElementAt(diagonal_.size()-1);
			}
		    while (gradient_.size() < n) {
		    	gradient_.add(1.0);
		    }
		    while (gradient_.size() > n) {
		    	gradient_.removeElementAt(gradient_.size()-1);
		    }
		    while (gauss_newton_step_.size() < n) {
		    	gauss_newton_step_.add(1.0);
		    }
		    while (gauss_newton_step_.size() > n) {
		    	gauss_newton_step_.removeElementAt(gauss_newton_step_.size()-1);
		    }
		  }

		  // Vector used to form the diagonal matrix that is used to
		  // regularize the Gauss-Newton solve and that defines the
		  // elliptical trust region
		  //
		  //   || D * step || <= radius_ .
		  //
		  
		  double diagonal_array[] = new double[diagonal_.size()];
		    for (i = 0; i < diagonal_.size(); i++) {
		    	diagonal_array[i] = diagonal_.get(i);
		    }
		    jacobian.SquaredColumnNorm(diagonal_array);
		  for (i = 0; i < n; ++i) {
		    diagonal_.set(i,Math.sqrt(Math.min(Math.max(diagonal_array[i], min_diagonal_),
		                            max_diagonal_)));
		  }

		  ComputeGradient(jacobian, residuals);
		  ComputeCauchyPoint(jacobian);

		  LinearSolverSummary linear_solver_summary =
		      ComputeGaussNewtonStep(per_solve_options, jacobian, residuals);

		  summary.residual_norm = linear_solver_summary.residual_norm;
		  summary.num_iterations = linear_solver_summary.num_iterations;
		  summary.termination_type = linear_solver_summary.termination_type;

		  if (linear_solver_summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_FATAL_ERROR) {
		    return summary;
		  }

		  if (linear_solver_summary.termination_type != LinearSolverTerminationType.LINEAR_SOLVER_FAILURE) {
		    switch (dogleg_type_) {
		      // Interpolate the Cauchy point and the Gauss-Newton step.
		      case TRADITIONAL_DOGLEG:
		        ComputeTraditionalDoglegStep(step);
		        break;

		      // Find the minimum in the subspace defined by the
		      // Cauchy point and the (Gauss-)Newton step.
		      case SUBSPACE_DOGLEG:
		        if (!ComputeSubspaceModel(jacobian)) {
		          summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_FAILURE;
		          break;
		        }
		        ComputeSubspaceDoglegStep(step);
		        break;
		    }
		  }

		  return summary;
  
	  }
	  
	  public void StepAccepted(double step_quality) {
		  if (step_quality <= 0.0) {
			  System.err.println("step_quality <= 0.0 in DoglegStrategy StepAccepted");
			  return;
		  }

		  if (step_quality < decrease_threshold_) {
		    radius_ *= 0.5;
		  }

		  if (step_quality > increase_threshold_) {
		    radius_ = Math.max(radius_, 3.0 * dogleg_step_norm_);
		  }

		  // Reduce the regularization multiplier, in the hope that whatever
		  // was causing the rank deficiency has gone away and we can return
		  // to doing a pure Gauss-Newton solve.
		  mu_ = Math.max(min_mu_, 2.0 * mu_ / mu_increase_factor_);
		  reuse_ = false;

	  }
	  
	  public void StepRejected(double step_quality) {
		  radius_ *= 0.5;
		  reuse_ = true;
	  }
	  
	  public void StepIsInvalid() {
		  mu_ *= mu_increase_factor_;
		  reuse_ = false;
	  }

	  public double Radius() {
		  return radius_;
	  }

	  // These functions are predominantly for testing.
	  public Vector<Double> gradient() { return gradient_; }
	  public Vector<Double> gauss_newton_step()  { return gauss_newton_step_; }
	  public Matrix subspace_basis() { return subspace_basis_; }
	  public Vector2d subspace_g() { return subspace_g_; }
	  public Matrix subspace_B() { return subspace_B_; }

		// The dogleg step is defined as the intersection of the trust region
		// boundary with the piecewise linear path from the origin to the Cauchy
		// point and then from there to the Gauss-Newton point (global minimizer
		// of the model function). The Gauss-Newton point is taken if it lies
		// within the trust region.
		private void ComputeTraditionalDoglegStep(double dogleg[]) {
		  int i;
		  //VectorRef dogleg_step(dogleg, gradient_.rows());

		  // Case 1. The Gauss-Newton step lies inside the trust region, and
		  // is therefore the optimal solution to the trust-region problem.
		  double gradient_norm = 0.0;
		  for (i = 0; i < gradient_.size(); i++) {
			  gradient_norm += gradient_.get(i)*gradient_.get(i);
		  }
		  gradient_norm = Math.sqrt(gradient_norm);
		  double gauss_newton_norm = 0.0;
		  for (i = 0; i < gauss_newton_step_.size(); i++) {
			  gauss_newton_norm += gauss_newton_step_.get(i)*gauss_newton_step_.get(i);
		  }
		  gauss_newton_norm = Math.sqrt(gauss_newton_norm);
		  if (gauss_newton_norm <= radius_) {
			for (i = 0; i < gradient_.size(); i++) {
				dogleg[i] = gauss_newton_step_.get(i);
			}
		    dogleg_step_norm_ = gauss_newton_norm;
		    for (i = 0; i < gradient_.size(); i++) {
		    	dogleg[i] /= diagonal_.get(i);
		    }
		    if (3 <= MAX_LOG_LEVEL) {
		        Preferences.debug("GaussNewton step size: " + dogleg_step_norm_ + "\n", Preferences.DEBUG_ALGORITHM);
		        Preferences.debug("radius: " + radius_ + "\n", Preferences.DEBUG_ALGORITHM);
		    }
		    return;
		  }

		  // Case 2. The Cauchy point and the Gauss-Newton steps lie outside
		  // the trust region. Rescale the Cauchy point to the trust region
		  // and return.
		  if  (gradient_norm * alpha_ >= radius_) {
			for (i = 0; i < gradient_.size(); i++ ) {
		        dogleg[i] = -(radius_ / gradient_norm) * gradient_.get(i);
			}
		    dogleg_step_norm_ = radius_;
		    for (i = 0; i < gradient_.size(); i++) {
		    	dogleg[i] /= diagonal_.get(i);
		    }
		    if (3 <= MAX_LOG_LEVEL) {
		        Preferences.debug("Cauchy step size: " + dogleg_step_norm_ + "\n", Preferences.DEBUG_ALGORITHM);
		        Preferences.debug("radius: " + radius_ + "\n", Preferences.DEBUG_ALGORITHM);
		    }

		    return;
		  }

		  // Case 3. The Cauchy point is inside the trust region and the
		  // Gauss-Newton step is outside. Compute the line joining the two
		  // points and the point on it which intersects the trust region
		  // boundary.

		  // a = alpha * -gradient
		  // b = gauss_newton_step
		  double dot_product = 0.0;
		  for (i = 0; i < gradient_.size(); i++) {
			  dot_product += (gradient_.get(i)*gauss_newton_step_.get(i));
		  }
		  double b_dot_a = -alpha_ * dot_product;
		  double a_squared_norm = Math.pow(alpha_ * gradient_norm, 2.0);
		  double b_minus_a_squared_norm =
		      a_squared_norm - 2 * b_dot_a + Math.pow(gauss_newton_norm, 2);

		  // c = a' (b - a)
		  //   = alpha * -gradient' gauss_newton_step - alpha^2 |gradient|^2
		  double c = b_dot_a - a_squared_norm;
		  double d = Math.sqrt(c * c + b_minus_a_squared_norm *
		                        (Math.pow(radius_, 2.0) - a_squared_norm));

		  double beta =
		      (c <= 0)
		      ? (d - c) /  b_minus_a_squared_norm
		      : (radius_ * radius_ - a_squared_norm) / (d + c);
		  for (i = 0; i < gradient_.size(); i++) {
			  dogleg[i] = (-alpha_ * (1.0 - beta)) * gradient_.get(i)
				      + beta * gauss_newton_step_.get(i);
		  }
		  dogleg_step_norm_ = 0.0;
		  for (i = 0; i < gradient_.size(); i++) {
			  dogleg_step_norm_ += dogleg[i]*dogleg[i];
		  }
		  dogleg_step_norm_ = Math.sqrt(dogleg_step_norm_);
		  for (i = 0; i < gradient_.size(); i++) {
		    	dogleg[i] /= diagonal_.get(i);
		  }
		  if (3 <= MAX_LOG_LEVEL) {
		        Preferences.debug("Dogleg step size: " + dogleg_step_norm_ + "\n", Preferences.DEBUG_ALGORITHM);
		        Preferences.debug("radius: " + radius_ + "\n", Preferences.DEBUG_ALGORITHM);
		   }
		}

		// The subspace method finds the minimum of the two-dimensional problem
		//
		//   min. 1/2 x' B' H B x + g' B x
		//   s.t. || B x ||^2 <= r^2
		//
		// where r is the trust region radius and B is the matrix with unit columns
		// spanning the subspace defined by the steepest descent and Newton direction.
		// This subspace by definition includes the Gauss-Newton point, which is
		// therefore taken if it lies within the trust region.
		private void ComputeSubspaceDoglegStep(double dogleg[]) {
	      int i;
		  //VectorRef dogleg_step(dogleg, gradient_.rows());

		  // The Gauss-Newton point is inside the trust region if |GN| <= radius_.
		  // This test is valid even though radius_ is a length in the two-dimensional
		  // subspace while gauss_newton_step_ is expressed in the (scaled)
		  // higher dimensional original space. This is because
		  //
		  //   1. gauss_newton_step_ by definition lies in the subspace, and
		  //   2. the subspace basis is orthonormal.
		  //
		  // As a consequence, the norm of the gauss_newton_step_ in the subspace is
		  // the same as its norm in the original space.
	      double gauss_newton_norm = 0.0;
		  for (i = 0; i < gauss_newton_step_.size(); i++) {
			  gauss_newton_norm += gauss_newton_step_.get(i)*gauss_newton_step_.get(i);
		  }
		  gauss_newton_norm = Math.sqrt(gauss_newton_norm);
		  if (gauss_newton_norm <= radius_) {
			for (i = 0; i < gradient_.size(); i++) {
				dogleg[i] = gauss_newton_step_.get(i);
			}
		    dogleg_step_norm_ = gauss_newton_norm;
		    dogleg_step_norm_ = 0.0;
			for (i = 0; i < gradient_.size(); i++) {
				dogleg[i] /= diagonal_.get(i);
			}
		    
			if (3 <= MAX_LOG_LEVEL) {
		        Preferences.debug("GaussNewton step size: " + dogleg_step_norm_ + "\n", Preferences.DEBUG_ALGORITHM);
		        Preferences.debug("radius: " + radius_ + "\n", Preferences.DEBUG_ALGORITHM);
		    }
		    return;
		  }
		  


		  // The optimum lies on the boundary of the trust region. The above problem
		  // therefore becomes
		  //
		  //   min. 1/2 x^T B^T H B x + g^T B x
		  //   s.t. || B x ||^2 = r^2
		  //
		  // Notice the equality in the constraint.
		  //
		  // This can be solved by forming the Lagrangian, solving for x(y), where
		  // y is the Lagrange multiplier, using the gradient of the objective, and
		  // putting x(y) back into the constraint. This results in a fourth order
		  // polynomial in y, which can be solved using e.g. the companion matrix.
		  // See the description of MakePolynomialForBoundaryConstrainedProblem for
		  // details. The result is up to four real roots y*, not all of which
		  // correspond to feasible points. The feasible points x(y*) have to be
		  // tested for optimality.

		  if (subspace_is_one_dimensional_) {
		    // The subspace is one-dimensional, so both the gradient and
		    // the Gauss-Newton step point towards the same direction.
		    // In this case, we move along the gradient until we reach the trust
		    // region boundary.
			double gradientNorm = 0.0;
			for (i = 0; i < gradient_.size(); i++) {
				gradientNorm += gradient_.get(i)*gradient_.get(i);
			}
			gradientNorm = Math.sqrt(gradientNorm);
			for (i = 0; i < gradient_.size(); i++) {
				dogleg[i] = -(radius_ / gradientNorm) * gradient_.get(i);
			}
		    dogleg_step_norm_ = radius_;
		    for (i = 0; i < gradient_.size(); i++) {
		    	dogleg[i] /= diagonal_.get(i);
		    }
		    if (3 <= MAX_LOG_LEVEL) {
		        Preferences.debug("Dogleg subspace step size (1D): " + dogleg_step_norm_ + "\n", Preferences.DEBUG_ALGORITHM);
		        Preferences.debug("radius: " + radius_ + "\n", Preferences.DEBUG_ALGORITHM);
		    }
		    return;
		  }

		  Vector2d minimum = new Vector2d(0.0, 0.0);
		  if (!FindMinimumOnTrustRegionBoundary(minimum)) {
		    // For the positive semi-definite case, a traditional dogleg step
		    // is taken in this case.
		    Preferences.debug("Failed to compute polynomial roots.\n", Preferences.DEBUG_ALGORITHM);
		    Preferences.debug("Taking traditional dogleg step instead.\n", Preferences.DEBUG_ALGORITHM);
		    ComputeTraditionalDoglegStep(dogleg);
		    return;
		  }

		  // Test first order optimality at the minimum.
		  // The first order KKT conditions state that the minimum x*
		  // has to satisfy either || x* ||^2 < r^2 (i.e. has to lie within
		  // the trust region), or
		  //
		  //   (B x* + g) + y x* = 0
		  //
		  // for some positive scalar y.
		  // Here, as it is already known that the minimum lies on the boundary, the
		  // latter condition is tested. To allow for small imprecisions, we test if
		  // the angle between (B x* + g) and -x* is smaller than acos(0.99).
		  // The exact value of the cosine is arbitrary but should be close to 1.
		  //
		  // This condition should not be violated. If it is, the minimum was not
		  // correctly determined.
		  double kCosineThreshold = 0.99;
		  double Bmin[] = new double[2];
		  Bmin[0] = subspace_B_.getArray()[0][0] * minimum.X + subspace_B_.getArray()[0][1] * minimum.Y;
		  Bmin[1] = subspace_B_.getArray()[1][0] * minimum.X + subspace_B_.getArray()[1][1] * minimum.Y;
		  Vector2d grad_minimum = new Vector2d(Bmin[0] + subspace_g_.X, Bmin[1] + subspace_g_.Y);
		  double minimum_norm = Math.sqrt(minimum.X * minimum.X + minimum.Y * minimum.Y);
		  double grad_minimum_norm = Math.sqrt(grad_minimum.X * grad_minimum.X + grad_minimum.Y * grad_minimum.Y);
		  double cosine_angle = -(minimum.X * grad_minimum.X + minimum.Y * grad_minimum.Y)/(minimum_norm * grad_minimum_norm);
		  if (cosine_angle < kCosineThreshold) {
		    Preferences.debug("First order optimality seems to be violated " +
		                 "in the subspace method!\n" + 
		                 "Cosine of angle between x and B x + g is " +
		                 cosine_angle + ".\n" +
		                 "Taking a regular dogleg step instead.\n" +
		                 "Please consider filing a bug report if this " +
		                 "happens frequently or consistently.\n", Preferences.DEBUG_ALGORITHM);
		    ComputeTraditionalDoglegStep(dogleg);
		    return;
		  }

		  // Create the full step from the optimal 2d solution.
		  for (i = 0; i < gradient_.size(); i++) {
		      dogleg[i] = subspace_basis_.getArray()[i][0] * minimum.X + subspace_basis_.getArray()[i][1] * minimum.Y;
		  }
		  dogleg_step_norm_ = radius_;
		  for (i = 0; i < gradient_.size(); i++) {
				dogleg[i] /= diagonal_.get(i);
		  }
		  if (3 <= MAX_LOG_LEVEL) {
			  Preferences.debug("Dogleg subspace step size: " + dogleg_step_norm_ + "\n", Preferences.DEBUG_ALGORITHM);
			  Preferences.debug("radius: " + radius_ + "\n", Preferences.DEBUG_ALGORITHM);
		  }
		}

	  //typedef Eigen::Matrix<double, 2, 1, Eigen::DontAlign> Vector2d;
	  //typedef Eigen::Matrix<double, 2, 2, Eigen::DontAlign> Matrix2d;

	  LinearSolverSummary ComputeGaussNewtonStep(
	      TrustRegionStrategyPerSolveOptions per_solve_options,
	      SparseMatrix jacobian,
	      double residuals[]) {
		  int i;
		  int n = jacobian.num_cols();
		  LinearSolverSummary linear_solver_summary = new LinearSolverSummary();
		  linear_solver_summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_FAILURE;

		  // The Jacobian matrix is often quite poorly conditioned. Thus it is
		  // necessary to add a diagonal matrix at the bottom to prevent the
		  // linear solver from failing.
		  //
		  // We do this by computing the same diagonal matrix as the one used
		  // by Levenberg-Marquardt (other choices are possible), and scaling
		  // it by a small constant (independent of the trust region radius).
		  //
		  // If the solve fails, the multiplier to the diagonal is increased
		  // up to max_mu_ by a factor of mu_increase_factor_ every time. If
		  // the linear solver is still not successful, the strategy returns
		  // with LINEAR_SOLVER_FAILURE.
		  //
		  // Next time when a new Gauss-Newton step is requested, the
		  // multiplier starts out from the last successful solve.
		  //
		  // When a step is declared successful, the multiplier is decreased
		  // by half of mu_increase_factor_.

		  while (mu_ < max_mu_) {
		    // Dogleg, as far as I (sameeragarwal) understand it, requires a
		    // reasonably good estimate of the Gauss-Newton step. This means
		    // that we need to solve the normal equations more or less
		    // exactly. This is reflected in the values of the tolerances set
		    // below.
		    //
		    // For now, this strategy should only be used with exact
		    // factorization based solvers, for which these tolerances are
		    // automatically satisfied.
		    //
		    // The right way to combine inexact solves with trust region
		    // methods is to use Stiehaug's method.
		    LinearSolverPerSolveOptions solve_options = new LinearSolverPerSolveOptions();
		    solve_options.q_tolerance = 0.0;
		    solve_options.r_tolerance = 0.0;

		    lm_diagonal_.clear();
		    solve_options.D = new double[diagonal_.size()];
		    for (i = 0; i < diagonal_.size(); i++) {
		    	lm_diagonal_.add(diagonal_.get(i) * Math.sqrt(mu_));
		    	solve_options.D[i] = lm_diagonal_.get(i);
		    }

		    // As in the LevenbergMarquardtStrategy, solve Jy = r instead
		    // of Jx = -r and later set x = -y to avoid having to modify
		    // either jacobian or residuals.
		    InvalidateArray(n, gauss_newton_step_);
		    double gauss_newton_step_array[] = new double[gauss_newton_step_.size()];
		    for (i = 0; i < gauss_newton_step_.size(); i++) {
		    	gauss_newton_step_array[i] = gauss_newton_step_.get(i);
		    }
		    linear_solver_summary = linear_solver_.Solve(jacobian, residuals, solve_options, gauss_newton_step_array);
		    
		    gauss_newton_step_.clear();
		    for (i = 0; i < gauss_newton_step_array.length; i++) {
		    	gauss_newton_step_.add(gauss_newton_step_array[i]);
		    }

		    if (per_solve_options.dump_format_type == DumpFormatType.CONSOLE ||
		        (per_solve_options.dump_format_type != DumpFormatType.CONSOLE &&
		         per_solve_options.dump_filename_base != null &&
		         !per_solve_options.dump_filename_base.isEmpty())) {
		      if (!DumpLinearLeastSquaresProblem(per_solve_options.dump_filename_base,
		                                         per_solve_options.dump_format_type,
		                                         jacobian,
		                                         solve_options.D,
		                                         residuals,
		                                         gauss_newton_step_array,
		                                         0)) {
		        System.err.println("Unable to dump trust region problem.");
		        System.err.println("Filename base: " +per_solve_options.dump_filename_base);
		      }
		    }

		    if (linear_solver_summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_FATAL_ERROR) {
		      return linear_solver_summary;
		    }

		    if (linear_solver_summary.termination_type == LinearSolverTerminationType.LINEAR_SOLVER_FAILURE ||
		        !IsArrayValid(n, gauss_newton_step_)) {
		      mu_ *= mu_increase_factor_;
		      if (2 <= MAX_LOG_LEVEL) {
		          Preferences.debug("Increasing mu " + mu_ + "\n", Preferences.DEBUG_ALGORITHM);
		      }
		      linear_solver_summary.termination_type = LinearSolverTerminationType.LINEAR_SOLVER_FAILURE;
		      continue;
		    }
		    break;
		  }

		  if (linear_solver_summary.termination_type != LinearSolverTerminationType.LINEAR_SOLVER_FAILURE) {
		    // The scaled Gauss-Newton step is D * GN:
		    //
		    //     - (D^-1 J^T J D^-1)^-1 (D^-1 g)
		    //   = - D (J^T J)^-1 D D^-1 g
		    //   = D -(J^T J)^-1 g
		    //
			for (i = 0; i < diagonal_.size(); i++) {
				double prod = gauss_newton_step_.get(i) * (-diagonal_.get(i));
				gauss_newton_step_.set(i, prod);
			}
		  }

		  return linear_solver_summary;
  
	  }
	  
	// The Cauchy point is the global minimizer of the quadratic model
	// along the one-dimensional subspace spanned by the gradient.
	private void ComputeCauchyPoint(SparseMatrix jacobian) {
	  int i;
	  // alpha * -gradient is the Cauchy point.
	  double Jg[] = new double[jacobian.num_rows()];
	  // The Jacobian is scaled implicitly by computing J * (D^-1 * (D^-1 * g))
	  // instead of (J * D^-1) * (D^-1 * g).
	  double scaled_gradient[] = new double[gradient_.size()];
	  for (i = 0; i < gradient_.size(); i++) {
		  scaled_gradient[i] = gradient_.get(i)/diagonal_.get(i);
	  }
	  jacobian.RightMultiply(scaled_gradient, Jg);
	  double gradient_squaredNorm = 0.0;
	  for (i = 0; i < gradient_.size(); i++) {
		  gradient_squaredNorm += (gradient_.get(i) * gradient_.get(i));
	  }
	  double Jg_squaredNorm = 0.0;
	  for (i = 0; i < Jg.length; i++) {
		  Jg_squaredNorm += (Jg[i]*Jg[i]);
	  }
	  alpha_ = gradient_squaredNorm / Jg_squaredNorm;
	}
	
	// The trust region is assumed to be elliptical with the
	// diagonal scaling matrix D defined by sqrt(diagonal_).
	// It is implemented by substituting step' = D * step.
	// The trust region for step' is spherical.
	// The gradient, the Gauss-Newton step, the Cauchy point,
	// and all calculations involving the Jacobian have to
	// be adjusted accordingly.
	private void ComputeGradient(
	    SparseMatrix jacobian,
	    double residuals[]) {
	  int i;
	  double gradient_array[] = new double[gradient_.size()];
	  jacobian.LeftMultiply(residuals, gradient_array);
	  for (i = 0; i < gradient_.size(); i++) {
		  gradient_.set(i, gradient_array[i]/diagonal_.get(i));
	  }
	}

	  
	  private boolean ComputeSubspaceModel(SparseMatrix jacobian) {
		  int i;
		  // Compute an orthogonal basis for the subspace using QR decomposition.
		  Matrix basis_vectors = new Matrix(jacobian.num_cols(), 2);
		  for (i = 0; i < jacobian.num_cols(); i++) {
			  basis_vectors.set(i, 0, gradient_.get(i));
			  basis_vectors.set(i, 1, gauss_newton_step_.get(i));
		  }
		  int rank = basis_vectors.rank();
		  double basis_vectors_array[][] = basis_vectors.getArray();
		  double tau[] = new double[Math.min(jacobian.num_cols(),2)];
		  int lwork = -1; // output optimal lwork in work[0]
	      double work[] = new double[Math.max(1,lwork)];
	      int info[] = new int[1];
	      int jpvt[] = new int[2];
	      jpvt[0] = -1;
	      jpvt[1] = -1;
		  ge2.dgeqp3(jacobian.num_cols(),2,basis_vectors_array,jacobian.num_cols(),jpvt,tau,work,lwork,info);
		  if (info[0] < 0) {
	    		System.err.println("ge2.dgeqp3 had an illegal argument " + (-i) + " for lwork = -1");
	    		return false;
	      }
	    	int optimallwork = (int)work[0];
	    	work = new double[optimallwork];
	    	ge2.dgeqp3(jacobian.num_cols(),2,basis_vectors_array,jacobian.num_cols(),jpvt,tau,work,optimallwork,info);
	    	if (info[0] < 0) {
	    		System.err.println("ge2.dgeqp3 had an illegal argument " + (-i) + " for lwork " + optimallwork);
	    		return false;
	    	}
	    	lwork = -1; // output optimal lwork in work[0]
		    work = new double[Math.max(1,lwork)];
	    	ge.dorgqr(jacobian.num_cols(),2,Math.min(jacobian.num_cols(),2),basis_vectors_array,jacobian.num_cols(),tau,work,lwork,info);
	    	if (info[0] < 0) {
	    		System.err.println("ge.dorgqr had an illegal argument " + (-i) + " for lwork = -1");
	    		return false;
	        }
	    	optimallwork = (int)work[0];
	    	work = new double[optimallwork];
	    	ge.dorgqr(jacobian.num_cols(),2,Math.min(jacobian.num_cols(),2),basis_vectors_array,jacobian.num_cols(),tau,work,optimallwork,info);
	    	if (info[0] < 0) {
	    		System.err.println("ge.dorgqr had an illegal argument " + (-i) + " for lwork " + optimallwork);
	    		return false;
	    	}
	    	// basis_vectors_array contains Q of the QR decomposition
	    	// If m >= n, A = Q *[R]
            //                   [0]
	    	// where R is an n-by-n upper triangular matrix and Q is an m-by-m orthogonal (or unitary) matrix. 
	    	// If A is of full rank n, then R is non-singular. It is sometimes convenient to write the factorization as 
	    	// A = [Q1 Q2] * [R]
	    	//               [0]
	    	// which reduces to A = Q1R
	    	// where Q1 consists of the ﬁrst n columns of Q, and Q2 the remaining m−n columns. 
		  switch (rank) {
		    case 0:
		      // This should never happen, as it implies that both the gradient
		      // and the Gauss-Newton step are zero. In this case, the minimizer should
		      // have stopped due to the gradient being too small.
		      System.err.println("Rank of subspace basis is 0. ");
		      System.err.println("This means that the gradient at the current iterate is ");
		      System.err.println("zero but the optimization has not been terminated. ");
		      System.err.println("You may have found a bug in Ceres.");
		      return false;

		    case 1:
		      // Gradient and Gauss-Newton step coincide, so we lie on one of the
		      // major axes of the quadratic problem. In this case, we simply move
		      // along the gradient until we reach the trust region boundary.
		      subspace_is_one_dimensional_ = true;
		      return true;

		    case 2:
		      subspace_is_one_dimensional_ = false;
		      break;

		    default:
		        System.err.println("Rank of the subspace basis matrix is reported to be ");
		        System.err.println("greater than 2. As the matrix contains only two ");
		        System.err.println("columns this cannot be true and is indicative of ");
		        System.err.println("a bug.");
		      return false;
		  }

		  // The subspace is two-dimensional, so compute the subspace model.
		  // Given the basis U, this is
		  //
		  //   subspace_g_ = g_scaled^T U
		  //
		  // and
		  //
		  //   subspace_B_ = U^T (J_scaled^T J_scaled) U
		  //
		  // As J_scaled = J * D^-1, the latter becomes
		  //
		  //   subspace_B_ = ((U^T D^-1) J^T) (J (D^-1 U))
		  //               = (J (D^-1 U))^T (J (D^-1 U))

		  //Takes the first 2 columns of the m by m Q to form the m by n Q1
		  // but basis_vectors_array from dorgqr is already the m by n Q1.
		  //subspace_basis_ =
		      //basis_qr.householderQ() * Matrix::Identity(jacobian->num_cols(), 2);
		  subspace_basis_ = new Matrix(basis_vectors_array);

		  subspace_g_.X = 0;
		  subspace_g_.Y = 0;
		  for (i = 0; i < jacobian.num_cols(); i++) {
			  subspace_g_.X += (basis_vectors_array[i][0] * gradient_.get(i));
			  subspace_g_.Y += (basis_vectors_array[i][1] * gradient_.get(i));
		  }
		  
		  Matrix Jb = new Matrix(2,jacobian.num_rows());

		 double tmp[] = new double[jacobian.num_cols()];
		  for (i = 0; i < jacobian.num_cols(); i++) {
			  tmp[i] = basis_vectors_array[i][0]/diagonal_.get(i);
		  }
		  double y[] = new double[jacobian.num_rows()];
		  jacobian.RightMultiply(tmp, y);
		  for (i = 0; i < jacobian.num_rows(); i++) {
			  Jb.set(0,i,y[i]);
		  }
		  for (i = 0; i < jacobian.num_cols(); i++) {
			  tmp[i] = basis_vectors_array[i][1]/diagonal_.get(i);
		  }
		  for (i = 0; i < jacobian.num_rows(); i++) {
			  y[i] = 0;
		  }
		  jacobian.RightMultiply(tmp, y);
		  for (i = 0; i < jacobian.num_rows(); i++) {
			  Jb.set(1,i,y[i]);
		  }

		  subspace_B_ = Jb.times(Jb.transpose());

		  return true;

	  }

		  // This function attempts to solve the boundary-constrained subspace problem
		  //
		  //   min. 1/2 x^T B^T H B x + g^T B x
		  //   s.t. || B x ||^2 = r^2
		  //
		  // where B is an orthonormal subspace basis and r is the trust-region radius.
		  //
		  // This is done by finding the roots of a fourth degree polynomial. If the
		  // root finding fails, the function returns false and minimum will be set
		  // to (0, 0). If it succeeds, true is returned.
		  //
		  // In the failure case, another step should be taken, such as the traditional
		  // dogleg step.
		  private boolean FindMinimumOnTrustRegionBoundary(Vector2d minimum) {
			int i;
		    if (minimum == null) {
		    	System.err.println("minimum == null in FindMinimumOnTrustRegionBoundary");
		    	return false;
		    }

		    // Return (0, 0) in all error cases.
		    minimum.X = 0;
		    minimum.Y = 0;

		    // Create the fourth-degree polynomial that is a necessary condition for
		    // optimality.
		    Vector<Double> polynomial = MakePolynomialForBoundaryConstrainedProblem();

		    // Find the real parts y_i of its roots (not only the real roots).
		    Vector<Double> roots_real = new Vector<Double>();
		    Vector<Double> roots_imag = null;
		    // FindPolynomialRoots calls Eigen::EigenSolver<Matrix> solver(companion_matrix, false);
		    if (!FindPolynomialRoots(polynomial, roots_real, roots_imag)) {
		      // Failed to find the roots of the polynomial, i.e. the candidate
		      // solutions of the constrained problem. Report this back to the caller.
		      return false;
		    }
		    
		    


		    // For each root y, compute B x(y) and check for feasibility.
		    // Notice that there should always be four roots, as the leading term of
		    // the polynomial is r^2 and therefore non-zero. However, as some roots
		    // may be complex, the real parts are not necessarily unique.
		    double minimum_value = Double.MAX_VALUE;
		    boolean valid_root_found = false;
		    for (i = 0; i < roots_real.size(); ++i) {
		      Vector2d x_i = ComputeSubspaceStepFromRoot(roots_real.get(i));

		      // Not all roots correspond to points on the trust region boundary.
		      // There are at most four candidate solutions. As we are interested
		      // in the minimum, it is safe to consider all of them after projecting
		      // them onto the trust region boundary.
		      double x_i_norm = 0.0;
		      x_i_norm = Math.sqrt(x_i.X * x_i.X + x_i.Y * x_i.Y);
		      if (x_i_norm > 0) {
		    	double scale = radius_/x_i_norm;
		    	Vector2d ev = new Vector2d(scale * x_i.X, scale * x_i.Y);
		        double f_i = EvaluateSubspaceModel(ev);
		        valid_root_found = true;
		        if (f_i < minimum_value) {
		          minimum_value = f_i;
		          minimum.X = x_i.X;
		          minimum.Y = x_i.Y;
		        }
		      }
		    }

		    return valid_root_found;

	  } 
		  
		// Build the polynomial that defines the optimal Lagrange multipliers.
		// Let the Lagrangian be
		//
		//   L(x, y) = 0.5 x^T B x + x^T g + y (0.5 x^T x - 0.5 r^2).       (1)
		//
		// Stationary points of the Lagrangian are given by
		//
		//   0 = d L(x, y) / dx = Bx + g + y x                              (2)
		//   0 = d L(x, y) / dy = 0.5 x^T x - 0.5 r^2                       (3)
		//
		// For any given y, we can solve (2) for x as
		//
		//   x(y) = -(B + y I)^-1 g .                                       (4)
		//
		// As B + y I is 2x2, we form the inverse explicitly:
		//
		//   (B + y I)^-1 = (1 / det(B + y I)) adj(B + y I)                 (5)
		//
		// where adj() denotes adjugation. This should be safe, as B is positive
		// semi-definite and y is necessarily positive, so (B + y I) is indeed
		// invertible.
		// Plugging (5) into (4) and the result into (3), then dividing by 0.5 we
		// obtain
		//
		//   0 = (1 / det(B + y I))^2 g^T adj(B + y I)^T adj(B + y I) g - r^2
//		                                                                  (6)
		//
		// or
		//
		//   det(B + y I)^2 r^2 = g^T adj(B + y I)^T adj(B + y I) g         (7a)
//		                      = g^T adj(B)^T adj(B) g
//		                           + 2 y g^T adj(B)^T g + y^2 g^T g       (7b)
		//
		// as
		//
		//   adj(B + y I) = adj(B) + y I = adj(B)^T + y I .                 (8)
		//
		// The left hand side can be expressed explicitly using
		//
		//   det(B + y I) = det(B) + y tr(B) + y^2 .                        (9)
		//
		// So (7) is a polynomial in y of degree four.
		// Bringing everything back to the left hand side, the coefficients can
		// be read off as
		//
//		     y^4  r^2
		//   + y^3  2 r^2 tr(B)
		//   + y^2 (r^2 tr(B)^2 + 2 r^2 det(B) - g^T g)
		//   + y^1 (2 r^2 det(B) tr(B) - 2 g^T adj(B)^T g)
		//   + y^0 (r^2 det(B)^2 - g^T adj(B)^T adj(B) g)
		//

	  private Vector<Double> MakePolynomialForBoundaryConstrainedProblem() {
		  double detB = subspace_B_.det();
		  double trB = subspace_B_.trace();
		  double r2 = radius_ * radius_;
		  double B_adj_array[][] = new double[2][2];
		  B_adj_array[0][0] = subspace_B_.getArray()[1][1];
		  B_adj_array[0][1] = -subspace_B_.getArray()[0][1];
		  B_adj_array[1][0] = -subspace_B_.getArray()[1][0];
		  B_adj_array[1][1] = subspace_B_.getArray()[0][0];
		  Matrix B_adj = new Matrix(B_adj_array);

		  Vector<Double> polynomial = new Vector<Double>(5);
		  polynomial.add(r2);
		  polynomial.add(2.0 * r2 * trB);
		  polynomial.add(r2 * (trB * trB + 2.0 * detB) - (subspace_g_.X*subspace_g_.X + subspace_g_.Y*subspace_g_.Y));
		  double subspace_g_array[][] = new double[2][1];
		  subspace_g_array[0][0] = subspace_g_.X;
		  subspace_g_array[1][0] = subspace_g_.Y;
		  Matrix subspace_g_mat = new Matrix(subspace_g_array);
		  double mult = (((subspace_g_mat.transpose()).times(B_adj)).times(subspace_g_mat)).getArray()[0][0];
		  polynomial.add(-2.0 * (mult
		      - r2 * detB * trB));
		  Matrix multMatrix = B_adj.times(subspace_g_mat);
		  double squaredNorm = multMatrix.getArray()[0][0]*multMatrix.getArray()[0][0]
				  + multMatrix.getArray()[1][0]*multMatrix.getArray()[1][0];
		  polynomial.add(r2 * detB * detB - squaredNorm);

		  return polynomial;
  
	  }
	  
	// Given a Lagrange multiplier y that corresponds to a stationary point
	// of the Lagrangian L(x, y), compute the corresponding x from the
	// equation
	//
	//   0 = d L(x, y) / dx
    //	     = B * x + g + y * x
    //	     = (B + y * I) * x + g
	//
	public Vector2d ComputeSubspaceStepFromRoot(double y) {
	  // x = -(B + y * I).inverse() * g;
	  double B_i_array[][] = new double[2][2];
	  B_i_array[0][0] = subspace_B_.getArray()[0][0] + y;
	  B_i_array[0][1] = subspace_B_.getArray()[0][1];
	  B_i_array[1][0] = subspace_B_.getArray()[1][0];
	  B_i_array[1][1] = subspace_B_.getArray()[1][1] + y;
	  double det = (B_i_array[0][0]*B_i_array[1][1] - B_i_array[0][1]*B_i_array[1][0]);
	  if (det == 0.0) {
		  System.out.println("det == 0.0 in ComputeSubspaceStepFromRoot");
		  return null;
	  }
	  double B_i_inverse[][] = new double[2][2];
	  B_i_inverse[0][0] = B_i_array[1][1]/det;
	  B_i_inverse[0][1] = -B_i_array[0][1]/det;
	  B_i_inverse[1][0] = -B_i_array[1][0]/det;
	  B_i_inverse[1][1] = B_i_array[0][0]/det;
	  double X = -B_i_inverse[0][0]*subspace_g_.X -B_i_inverse[0][1]*subspace_g_.Y;
	  double Y = -B_i_inverse[1][0]*subspace_g_.X -B_i_inverse[1][1]*subspace_g_.Y;
	  return new Vector2d(X,Y);
	}

	// This function evaluates the quadratic model at a point x in the
	// subspace spanned by subspace_basis_.
	double EvaluateSubspaceModel(Vector2d x) {
	  double Bx[] = new double[2];
	  Bx[0] = subspace_B_.getArray()[0][0]*x.X + subspace_B_.getArray()[0][1]*x.Y;
	  Bx[1] = subspace_B_.getArray()[1][0]*x.X + subspace_B_.getArray()[1][1]*x.Y;
	  return(0.5 * x.X*Bx[0] + 0.5*x.Y*Bx[1] + subspace_g_.X*x.X + subspace_g_.Y*x.Y);
	}
	 
	} // class DoglegStrategy
	
	// Remove leading terms with zero coefficients.
	public Vector<Double> RemoveLeadingZeros(Vector<Double> polynomial_in) {
	  int i = 0;
	  int j;
	  while (i < (polynomial_in.size() - 1) && polynomial_in.get(i) == 0.0) {
	    ++i;
	  }
	  Vector<Double>polynomial_leading_zeros_stripped = new Vector<Double>();
	  for (j = i; j < polynomial_in.size(); j++) {
		  polynomial_leading_zeros_stripped.add(polynomial_in.get(j));
	  }
	  return polynomial_leading_zeros_stripped;
	}
	
	public void FindLinearPolynomialRoots(Vector<Double> polynomial,
            Vector<Double> real,
            Vector<Double> imaginary) {
		if (polynomial.size() != 2) {
			System.err.println("polynomial.size() != 2 in FindLinearPolynomialRoots");
			return;
		}
		if (real != null) {
		    real.clear();
		    real.add(-polynomial.get(1) / polynomial.get(0));
		}
		
		if (imaginary != null) {
			imaginary.clear();
		    imaginary.add(0.0);
		}
    }

	public void FindQuadraticPolynomialRoots(Vector<Double> polynomial,
            Vector<Double> real,
            Vector<Double> imaginary) {
	        if (polynomial.size() != 3) {
	        	System.err.println("polynomial.size() != 3 in FindQuadraticPolynomialRoots");
	        	return;
	        }
		    double a = polynomial.get(0);
		    double b = polynomial.get(1);
		    double c = polynomial.get(2);
		    double D = b * b - 4 * a * c;
		    double sqrt_D = Math.sqrt(Math.abs(D));
		    if (real != null) {
		        real.clear();
		    }
		    if (imaginary != null) {
		        imaginary.clear();
		    }
		
		    // Real roots.
		    if (D >= 0) {
			    if (real != null) {
			        // Stable quadratic roots according to BKP Horn.
			        // http://people.csail.mit.edu/bkph/articles/Quadratics.pdf
			        if (b >= 0) {
			            real.add((-b - sqrt_D) / (2.0 * a));
			            real.add((2.0 * c) / (-b - sqrt_D));
			        } else {
			            real.add((2.0 * c) / (-b + sqrt_D));
			            real.add((-b + sqrt_D) / (2.0 * a));
			        }
		        }
		        return;
	       }

           // Use the normal quadratic formula for the complex case.
           if (real != null) {
               real.add(-b / (2.0 * a));
               real.add(-b / (2.0 * a));
           }
           if (imaginary != null) {
               imaginary.add(sqrt_D / (2.0 * a));
               imaginary.add(-sqrt_D / (2.0 * a));
           }
    }
	
	// Structure defining a linear least squares problem and if possible
	// ground truth solutions. To be used by various LinearSolver tests.
	class LinearLeastSquaresProblem {
		  //scoped_ptr<SparseMatrix> A;
		  public SparseMatrix A;
		  //scoped_array<double> b;
		  public double b[];
		  // scoped_array<double> D;
		  public double D[];
		  // If using the schur eliminator then how many of the variable
		  // blocks are e_type blocks.
		  public int num_eliminate_blocks;

		  // Solution to min_x |Ax - b|^2
		  //scoped_array<double> x;
		  public double x[];
		  // Solution to min_x |Ax - b|^2 + |Dx|^2
		  //scoped_array<double> x_D;
		  public double x_D[];
	  public LinearLeastSquaresProblem() {
	      A = null;
	      b = null;
	      D = null;
	      num_eliminate_blocks = 0;
	      x = null;
	      x_D = null;
	  }

	  
	};
	
	public LinearLeastSquaresProblem CreateLinearLeastSquaresProblemFromId(int id) {
		  switch (id) {
		    case 0:
		      return LinearLeastSquaresProblem0();
		    case 1:
		      return LinearLeastSquaresProblem1();
		    case 2:
		      return LinearLeastSquaresProblem2();
		    case 3:
		      return LinearLeastSquaresProblem3();
		    case 4:
		      return LinearLeastSquaresProblem4();
		    default:
		      System.err.println("Unknown problem id requested " + id);
		  }
		  return null;
		}
	
	/*
	A = [1   2]
	    [3   4]
	    [6 -10]

	b = [  8
	      18
	     -18]

	x = [2
	     3]

	D = [1
	     2]

	x_D = [1.78448275;
	       2.82327586;]
	 */
	public LinearLeastSquaresProblem LinearLeastSquaresProblem0() {
	  LinearLeastSquaresProblem problem = new LinearLeastSquaresProblem();

	  TripletSparseMatrix A = new TripletSparseMatrix(3, 2, 6);
	  problem.b = new double[3];
	  problem.D = new double[2];

	  problem.x = new double[2];
	  problem.x_D = new double[2];

	  int Ai[] = A.mutable_rows();
	  int Aj[] = A.mutable_cols();
	  double Ax[] = A.mutable_values();

	  int counter = 0;
	  for (int i = 0; i < 3; ++i) {
	    for (int j = 0; j< 2; ++j) {
	      Ai[counter] = i;
	      Aj[counter] = j;
	      ++counter;
	    }
	  }

	  Ax[0] = 1.;
	  Ax[1] = 2.;
	  Ax[2] = 3.;
	  Ax[3] = 4.;
	  Ax[4] = 6;
	  Ax[5] = -10;
	  A.set_num_nonzeros(6);
	  problem.A = A;

	  problem.b[0] = 8;
	  problem.b[1] = 18;
	  problem.b[2] = -18;

	  problem.x[0] = 2.0;
	  problem.x[1] = 3.0;

	  problem.D[0] = 1;
	  problem.D[1] = 2;

	  problem.x_D[0] = 1.78448275;
	  problem.x_D[1] = 2.82327586;
	  return problem;
	}


	/*
    A = [1 0  | 2 0 0
         3 0  | 0 4 0
         0 5  | 0 0 6
         0 7  | 8 0 0
         0 9  | 1 0 0
         0 0  | 1 1 1]

    b = [0
         1
         2
         3
         4
         5]

    c = A'* b = [ 3
                 67
                 33
                  9
                 17]

    A'A = [10    0    2   12   0
            0  155   65    0  30
            2   65   70    1   1
           12    0    1   17   1
            0   30    1    1  37]

    S = [ 42.3419  -1.4000  -11.5806
          -1.4000   2.6000    1.0000
          11.5806   1.0000   31.1935]

    r = [ 4.3032
          5.4000
          5.0323]

    S\r = [ 0.2102
            2.1367
            0.1388]

    A\b = [-2.3061
            0.3172
            0.2102
            2.1367
            0.1388]
*/
		//The following two functions create a TripletSparseMatrix and a
		//BlockSparseMatrix version of this problem.
		
		//TripletSparseMatrix version.
		public LinearLeastSquaresProblem LinearLeastSquaresProblem1() {
		int num_rows = 6;
		int num_cols = 5;
		
		LinearLeastSquaresProblem problem = new LinearLeastSquaresProblem();
		TripletSparseMatrix A = new TripletSparseMatrix(num_rows,
		                                                 num_cols,
		                                                 num_rows * num_cols);
		problem.b = new double[num_rows];
		problem.D = new double[num_cols];
		problem.num_eliminate_blocks = 2;
		
		int rows[] = A.mutable_rows();
		int cols[] = A.mutable_cols();
		double values[] = A.mutable_values();
		
		int nnz = 0;
		
		// Row 1
		{
		  rows[nnz] = 0;
		  cols[nnz] = 0;
		  values[nnz++] = 1;
		
		  rows[nnz] = 0;
		  cols[nnz] = 2;
		  values[nnz++] = 2;
		}
		
		// Row 2
		{
		  rows[nnz] = 1;
		  cols[nnz] = 0;
		  values[nnz++] = 3;
		
		  rows[nnz] = 1;
		  cols[nnz] = 3;
		  values[nnz++] = 4;
		}
		
		// Row 3
		{
		  rows[nnz] = 2;
		  cols[nnz] = 1;
		  values[nnz++] = 5;
		
		  rows[nnz] = 2;
		  cols[nnz] = 4;
		  values[nnz++] = 6;
		}
		
		// Row 4
		{
		  rows[nnz] = 3;
		  cols[nnz] = 1;
		  values[nnz++] = 7;
		
		  rows[nnz] = 3;
		  cols[nnz] = 2;
		  values[nnz++] = 8;
		}
		
		// Row 5
		{
		  rows[nnz] = 4;
		  cols[nnz] = 1;
		  values[nnz++] = 9;
		
		  rows[nnz] = 4;
		  cols[nnz] = 2;
		  values[nnz++] = 1;
		}
		
		// Row 6
		{
		  rows[nnz] = 5;
		  cols[nnz] = 2;
		  values[nnz++] = 1;
		
		  rows[nnz] = 5;
		  cols[nnz] = 3;
		  values[nnz++] = 1;
		
		  rows[nnz] = 5;
		  cols[nnz] = 4;
		  values[nnz++] = 1;
		}
		
		A.set_num_nonzeros(nnz);
		if (!A.IsValid()) {
			System.err.println("A.IsValid() == false in LinearLeastSquaresProblem1");
			return null;
		}
		
		problem.A = A;
		
		for (int i = 0; i < num_cols; ++i) {
		  problem.D[i] = 1;
		}
		
		for (int i = 0; i < num_rows; ++i) {
		  problem.b[i] = i;
		}
		
		return problem;
		}
		
		// BlockSparseMatrix version
		public LinearLeastSquaresProblem LinearLeastSquaresProblem2() {
		  int num_rows = 6;
		  int num_cols = 5;

		  LinearLeastSquaresProblem problem = new LinearLeastSquaresProblem();

		  problem.b = new double[num_rows];
		  problem.D = new double[num_cols];
		  problem.num_eliminate_blocks = 2;

		  CompressedRowBlockStructure bs = new CompressedRowBlockStructure();
		  double values[] = new double[num_rows * num_cols];

		  for (int c = 0; c < num_cols; ++c) {
		    bs.cols.add(new Block());
		    bs.cols.lastElement().size = 1;
		    bs.cols.lastElement().position = c;
		  }

		  int nnz = 0;

		  // Row 1
		  {
		    values[nnz++] = 1;
		    values[nnz++] = 2;

		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 1;
		    row.block.position = 0;
		    row.cells.add(new Cell(0, 0));
		    row.cells.add(new Cell(2, 1));
		  }

		  // Row 2
		  {
		    values[nnz++] = 3;
		    values[nnz++] = 4;

		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 1;
		    row.block.position = 1;
		    row.cells.add(new Cell(0, 2));
		    row.cells.add(new Cell(3, 3));
		  }

		  // Row 3
		  {
		    values[nnz++] = 5;
		    values[nnz++] = 6;

		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 1;
		    row.block.position = 2;
		    row.cells.add(new Cell(1, 4));
		    row.cells.add(new Cell(4, 5));
		  }

		  // Row 4
		  {
		    values[nnz++] = 7;
		    values[nnz++] = 8;

		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 1;
		    row.block.position = 3;
		    row.cells.add(new Cell(1, 6));
		    row.cells.add(new Cell(2, 7));
		  }

		  // Row 5
		  {
		    values[nnz++] = 9;
		    values[nnz++] = 1;

		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 1;
		    row.block.position = 4;
		    row.cells.add(new Cell(1, 8));
		    row.cells.add(new Cell(2, 9));
		  }

		  // Row 6
		  {
		    values[nnz++] = 1;
		    values[nnz++] = 1;
		    values[nnz++] = 1;

		    bs.rows.add(new CompressedList());
		    CompressedList row = bs.rows.lastElement();
		    row.block.size = 1;
		    row.block.position = 5;
		    row.cells.add(new Cell(2, 10));
		    row.cells.add(new Cell(3, 11));
		    row.cells.add(new Cell(4, 12));
		  }

		  BlockSparseMatrix A = new BlockSparseMatrix(bs);
		  for (int i = 0; i < nnz; i++) {
			  A.values_[i] = values[i];
		  }

		  for (int i = 0; i < num_cols; ++i) {
		    problem.D[i] = 1;
		  }

		  for (int i = 0; i < num_rows; ++i) {
		    problem.b[i] = i;
		  }

		  problem.A = A;

		  return problem;
		}


		/*
	      A = [1 0
	           3 0
	           0 5
	           0 7
	           0 9
	           0 0]

	      b = [0
	           1
	           2
	           3
	           4
	           5]
	*/
	// BlockSparseMatrix version
	public LinearLeastSquaresProblem LinearLeastSquaresProblem3() {
	  int num_rows = 5;
	  int num_cols = 2;

	  LinearLeastSquaresProblem problem = new LinearLeastSquaresProblem();

	  problem.b = new double[num_rows];
	  problem.D = new double[num_cols];
	  problem.num_eliminate_blocks = 2;

	  CompressedRowBlockStructure bs = new CompressedRowBlockStructure();
	  double values[] = new double[num_rows * num_cols];

	  for (int c = 0; c < num_cols; ++c) {
	    bs.cols.add(new Block());
	    bs.cols.get(bs.cols.size()-1).size = 1;
	    bs.cols.get(bs.cols.size()-1).position = c;
	  }

	  int nnz = 0;

	  // Row 1
	  {
	    values[nnz++] = 1;
	    bs.rows.add(new CompressedList());
	    CompressedList row = bs.rows.get(bs.rows.size()-1);
	    row.block.size = 1;
	    row.block.position = 0;
	    row.cells.add(new Cell(0, 0));
	  }

	  // Row 2
	  {
	    values[nnz++] = 3;
	    bs.rows.add(new CompressedList());
	    CompressedList row = bs.rows.get(bs.rows.size()-1);
	    row.block.size = 1;
	    row.block.position = 1;
	    row.cells.add(new Cell(0, 1));
	  }

	  // Row 3
	  {
	    values[nnz++] = 5;
	    bs.rows.add(new CompressedList());
	    CompressedList row = bs.rows.get(bs.rows.size()-1);
	    row.block.size = 1;
	    row.block.position = 2;
	    row.cells.add(new Cell(1, 2));
	  }

	  // Row 4
	  {
	    values[nnz++] = 7;
	    bs.rows.add(new CompressedList());
	    CompressedList row = bs.rows.get(bs.rows.size()-1);
	    row.block.size = 1;
	    row.block.position = 3;
	    row.cells.add(new Cell(1, 3));
	  }

	  // Row 5
	  {
	    values[nnz++] = 9;
	    bs.rows.add(new CompressedList());
	    CompressedList row = bs.rows.get(bs.rows.size()-1);
	    row.block.size = 1;
	    row.block.position = 4;
	    row.cells.add(new Cell(1, 4));
	  }

	  BlockSparseMatrix A = new BlockSparseMatrix(bs);
	  for (int i = 0; i < nnz; i++) {
		  A.values_[i] = values[i];
	  }

	  for (int i = 0; i < num_cols; ++i) {
	    problem.D[i] = 1;
	  }

	  for (int i = 0; i < num_rows; ++i) {
	    problem.b[i] = i;
	  }

	  problem.A = A;

	  return problem;
	}
	
	/*
    A = [1 2 0 0 0 1 1
         1 4 0 0 0 5 6
         0 0 9 0 0 3 1]

    b = [0
         1
         2]
		*/
		//BlockSparseMatrix version
		//
		//This problem has the unique property that it has two different
		//sized f-blocks, but only one of them occurs in the rows involving
		//the one e-block. So performing Schur elimination on this problem
		//tests the Schur Eliminator's ability to handle non-e-block rows
		//correctly when their structure does not conform to the static
		//structure determined by DetectStructure.
		//
		//NOTE: This problem is too small and rank deficient to be solved without
		//the diagonal regularization.
		public LinearLeastSquaresProblem LinearLeastSquaresProblem4() {
		int num_rows = 3;
		int num_cols = 7;
		
		LinearLeastSquaresProblem problem = new LinearLeastSquaresProblem();
		
		problem.b = new double[num_rows];
		problem.D = new double[num_cols];
		problem.num_eliminate_blocks = 1;
		
		CompressedRowBlockStructure bs = new CompressedRowBlockStructure();
		double values[] = new double[num_rows * num_cols];
		
		// Column block structure
		bs.cols.add(new Block());
		bs.cols.get(bs.cols.size()-1).size = 2;
		bs.cols.get(bs.cols.size()-1).position = 0;
		
		bs.cols.add(new Block());
		bs.cols.get(bs.cols.size()-1).size = 3;
		bs.cols.get(bs.cols.size()-1).position = 2;
		
		bs.cols.add(new Block());
		bs.cols.get(bs.cols.size()-1).size = 2;
		bs.cols.get(bs.cols.size()-1).position = 5;
		
		int nnz = 0;
		
		// Row 1 & 2
		{
		  bs.rows.add(new CompressedList());
		  CompressedList row = bs.rows.get(bs.rows.size()-1);
		  row.block.size = 2;
		  row.block.position = 0;
		
		  row.cells.add(new Cell(0, nnz));
		  values[nnz++] = 1;
		  values[nnz++] = 2;
		  values[nnz++] = 1;
		  values[nnz++] = 4;
		
		  row.cells.add(new Cell(2, nnz));
		  values[nnz++] = 1;
		  values[nnz++] = 1;
		  values[nnz++] = 5;
		  values[nnz++] = 6;
		}
		
		// Row 3
		{
		  bs.rows.add(new CompressedList());
		  CompressedList row = bs.rows.get(bs.rows.size()-1);
		  row.block.size = 1;
		  row.block.position = 2;
		
		  row.cells.add(new Cell(1, nnz));
		  values[nnz++] = 9;
		  values[nnz++] = 0;
		  values[nnz++] = 0;
		
		  row.cells.add(new Cell(2, nnz));
		  values[nnz++] = 3;
		  values[nnz++] = 1;
		}
		
		BlockSparseMatrix A = new BlockSparseMatrix(bs);
		  for (int i = 0; i < nnz; i++) {
			  A.values_[i] = values[i];
		  }
		
		for (int i = 0; i < num_cols; ++i) {
		  problem.D[i] = (i + 1) * 100;
		}
		
		for (int i = 0; i < num_rows; ++i) {
		  problem.b[i] = i;
		}
		
		problem.A = A;
		return problem;
		}

		public boolean DumpLinearLeastSquaresProblem(String filename_base,
                DumpFormatType dump_format_type,
                SparseMatrix A,
                double D[],
                double b[],
                double x[],
                int num_eliminate_blocks) {
		switch (dump_format_type) {
		case CONSOLE:
		return DumpLinearLeastSquaresProblemToConsole(A, D, b, x,
		                                 num_eliminate_blocks);
		case TEXTFILE:
		return DumpLinearLeastSquaresProblemToTextFile(filename_base,
		                                  A, D, b, x,
		                                  num_eliminate_blocks);
		default:
		System.err.println("Unknown DumpFormatType " + dump_format_type);
		}
		
		return true;
		}
		
		public boolean DumpLinearLeastSquaresProblemToConsole(SparseMatrix A,
                double D[],
                double b[],
                double x[],
                int num_eliminate_blocks) {
		if (A == null) {
		    System.err.println("A == null in DumpLinearLeastSquaresProblemToConsole");
		    return false;
		}
		Matrix AA = A.ToDenseMatrix();
		Matrix AAT = AA.transpose();
		Preferences.debug("A transpose: \n", Preferences.DEBUG_ALGORITHM);
		for (int row = 0; row < AA.getRowDimension(); row++) {
		    for (int col = 0; col < AA.getColumnDimension(); col++) {
		         Preferences.debug("AT["+row+"]["+col+"] = " + AAT.getArray()[row][col] + "\n", Preferences.DEBUG_ALGORITHM);	
		    }
		}
		
		if (D != null) {
		    Preferences.debug("A's appended diagonal:\n", Preferences.DEBUG_ALGORITHM);
		    for (int i = 0; i < A.num_cols(); i++) {
		    	Preferences.debug("D["+i+"] = " + D[i] + "\n", Preferences.DEBUG_ALGORITHM);
		    }
		}
		
		if (b != null) {
			Preferences.debug("b: \n", Preferences.DEBUG_ALGORITHM);
			for (int i = 0; i < A.num_rows(); i++) {
				Preferences.debug("b["+i+"] = " + b[i] + "\n", Preferences.DEBUG_ALGORITHM);
			}
		}
		
		if (x != null) {
			Preferences.debug("x: \n", Preferences.DEBUG_ALGORITHM);
			for (int i = 0; i < A.num_cols(); i++) {
				Preferences.debug("x["+i+"] = " + x[i] + "\n", Preferences.DEBUG_ALGORITHM);
			}
		}
		return true;
		}


		void WriteArrayToFileOrDie(String filename,
                double x[],
                int size) {
			if (x == null) {
				System.err.println("x == null in WriteArrayToFileOrDie");
				return;
			}
	
		if (2 <= MAX_LOG_LEVEL) {
			Preferences.debug("Writing array to: " + filename + "\n", Preferences.DEBUG_ALGORITHM);
		}
		File file = new File(filename);
        FileWriter fw = null;
		try {
			fw = new FileWriter(file);
		} catch (IOException e) {
			System.err.println("IOException in WriteArrayToFileOrDie on new FileWriter(file)");
			return;
		}
		
		for (int i = 0; i < size; ++i) {
			String str = String.format("% 17f\n", x[i]);
			try {
				fw.write(str, 0, str.length());
			} catch (IOException e) {
				System.err.println("IOException in WriteArrayToFileOrDie on fw.write(str,0,str.length())");
				return;
			}
		}
		try {
		    fw.close();
		}
		catch (IOException e) {
			System.err.println("IOException in WriteArrayToFileOrDie on fw.close()");
		}
		}
		
		public void WriteStringToFileOrDie(String data, String filename) {
			File file = new File(filename);
	        FileWriter fw = null;
			try {
				fw = new FileWriter(file);
			} catch (IOException e) {
				System.err.println("IOException in WriteStringToFileOrDie on new FileWriter(file)");
				return;
			}
			try {
				fw.write(data, 0, data.length());
			} catch (IOException e) {
				System.err.println("IOException in WriteStringToFileOrDie on fw.write(str,0,str.length())");
				return;
			}
		try {
		    fw.close();
		}
		catch (IOException e) {
			System.err.println("IOException in WriteStringToFileOrDie on fw.close()");
		}  
			}

		
		public boolean DumpLinearLeastSquaresProblemToTextFile(String filename_base,
                SparseMatrix A,
                double D[],
                double b[],
                double x[],
                int num_eliminate_blocks) {
		if (A == null) {
			System.err.println("A == null in DumpLinearLeastSquaresProblemToTextFile");
			return false;
		}
		Preferences.debug("Writing to: " + filename_base + "*\n", Preferences.DEBUG_ALGORITHM);
		
		String matlab_script = null;
		matlab_script = String.format(
		"function lsqp = load_trust_region_problem()\n");
		matlab_script = matlab_script + String.format(
		"lsqp.num_rows = %d;\n", A.num_rows());
		matlab_script = matlab_script + String.format(
		"lsqp.num_cols = %d;\n", A.num_cols());
		
		{
		String filename = filename_base + "_A.txt";
		File file = new File(filename);
		A.ToTextFile(file);
		/*if (A instanceof BlockSparseMatrix) {
		     ((BlockSparseMatrix)A).ToTextFile(file);
		}
		else if (A instanceof TripletSparseMatrix) {
			((TripletSparseMatrix)A).ToTextFile(file);
		}
		else if (A instanceof DenseSparseMatrix) {
			((DenseSparseMatrix)A).ToTextFile(file);
		}*/
		matlab_script = matlab_script + String.format(
		"tmp = load('%s', '-ascii');\n", filename);
		matlab_script = matlab_script + String.format(
		"lsqp.A = sparse(tmp(:, 1) + 1, tmp(:, 2) + 1, tmp(:, 3), %d, %d);\n",
		A.num_rows(),
		A.num_cols());
		}
		
		
		if (D != null) {
		String filename = filename_base + "_D.txt";
		WriteArrayToFileOrDie(filename, D, A.num_cols());
		matlab_script = matlab_script + String.format(
		"lsqp.D = load('%s', '-ascii');\n", filename);
		}
		
		if (b != null) {
		String filename = filename_base + "_b.txt";
		WriteArrayToFileOrDie(filename, b, A.num_rows());
		matlab_script = matlab_script + String.format(
		"lsqp.b = load('%s', '-ascii');\n", filename);
		}
		
		if (x != null) {
		String filename = filename_base + "_x.txt";
		WriteArrayToFileOrDie(filename, x, A.num_cols());
		matlab_script = matlab_script + String.format(
		"lsqp.x = load('%s', '-ascii');\n", filename);
		}
		
		String matlab_filename = filename_base + ".m";
		WriteStringToFileOrDie(matlab_script, matlab_filename);
		return true;
		}

		public Minimizer Create(MinimizerType minimizer_type) {
			  if (minimizer_type == MinimizerType.TRUST_REGION) {
			    return new TrustRegionMinimizer();
			  }

			  if (minimizer_type == MinimizerType.LINE_SEARCH) {
			    return new LineSearchMinimizer();
			  }

			  System.err.println("Unknown minimizer_type: " + minimizer_type);
			  return null;
	}
		
		class SteepestDescent extends LineSearchDirection {
			 public SteepestDescent() {
				 super();
			 }
			 
			 public boolean NextDirection(LineSearchMinimizer.State previous,
			                     LineSearchMinimizer.State current,
			                     Vector<Double> search_direction) {
				int i;
				search_direction.clear();
				for (i = 0; i < current.gradient.size(); i++) {
					search_direction.add(-current.gradient.get(i));
				}
			    return true;
			  }
		} // class SteepestDescent
		
		public String NonlinearConjugateGradientTypeToString(
			    NonlinearConjugateGradientType type) {
			  switch (type) {
			  case FLETCHER_REEVES:
				  return "FLETCHER_REEVES";
			  case POLAK_RIBIERE:
				  return "POLAK_RIBIERE";
			  case HESTENES_STIEFEL:
				  return "HESTENES_STIEFEL";
			    default:
			      return "UNKNOWN";
			  }
			}

		
		class NonlinearConjugateGradient extends LineSearchDirection {
			private NonlinearConjugateGradientType type_;
		    private double function_tolerance_;
			 public NonlinearConjugateGradient(NonlinearConjugateGradientType type,
			                            double function_tolerance) {
			        type_ = type;
			        function_tolerance_ = function_tolerance;
			  }

			  public boolean NextDirection(LineSearchMinimizer.State previous,
			                     LineSearchMinimizer.State current,
			                     Vector<Double> search_direction) {
				int i;
			    double beta = 0.0;
			    Vector<Double> gradient_change = new Vector<Double>();
			    double dotProduct = 0.0;
			    switch (type_) {
 			      case FLETCHER_REEVES:
			        beta = current.gradient_squared_norm / previous.gradient_squared_norm;
			        break;
			      case POLAK_RIBIERE:
			    	for (i = 0; i < current.gradient.size(); i++) {
			    		gradient_change.add(current.gradient.get(i) - previous.gradient.get(i));
			    	}
			        for (i = 0; i < current.gradient.size(); i++) {
			        	dotProduct += (current.gradient.get(i) * gradient_change.get(i));
			        }
			        beta = (dotProduct /
			                previous.gradient_squared_norm);
			        break;
			      case HESTENES_STIEFEL:
			    	for (i = 0; i < current.gradient.size(); i++) {
				    	gradient_change.add(current.gradient.get(i) - previous.gradient.get(i));
				    }
			        for (i = 0; i < current.gradient.size(); i++) {
			        	dotProduct += (current.gradient.get(i) * gradient_change.get(i));
			        }
			        double dotProduct2 = 0.0;
			        for (i = 0; i < previous.search_direction.size(); i++) {
			        	dotProduct2 += (previous.search_direction.get(i) * gradient_change.get(i));
			        }
			        beta =  (dotProduct / dotProduct2);
			        break;
			      default:
			        System.err.println("Unknown nonlinear conjugate gradient type: " + NonlinearConjugateGradientTypeToString(type_));
			    }

			    search_direction.clear();
			    for (i = 0; i < current.gradient.size(); i++) {
			    	search_direction.add(-current.gradient.get(i) + beta * previous.search_direction.get(i));
			    }
			    double directional_derivative = 0.0;
			    for (i = 0; i < current.gradient.size(); i++) {
			    	directional_derivative += (current.gradient.get(i) * search_direction.get(i));
			    }
			    if (directional_derivative > -function_tolerance_) {
			      if (WARNING <= MAX_LOG_LEVEL) {
			      Preferences.debug("Restarting non-linear conjugate gradients: \n" +
			                   "directional_derivative = " + directional_derivative + "\n", Preferences.DEBUG_ALGORITHM);
			      }
			      search_direction.clear();
					for (i = 0; i < current.gradient.size(); i++) {
						search_direction.add(-current.gradient.get(i));
					}
			    }

			    return true;
			  }

			 
			};
			
			// LowRankInverseHessian is a positive definite approximation to the
			// Hessian using the limited memory variant of the
			// Broyden-Fletcher-Goldfarb-Shanno (BFGS)secant formula for
			// approximating the Hessian.
			//
			// Other update rules like the Davidon-Fletcher-Powell (DFP) are
			// possible, but the BFGS rule is considered the best performing one.
			//
			// The limited memory variant was developed by Nocedal and further
			// enhanced with scaling rule by Byrd, Nocedal and Schanbel.
			//
			// Nocedal, J. (1980). "Updating Quasi-Newton Matrices with Limited
			// Storage". Mathematics of Computation 35 (151): 773–782.
			//
			// Byrd, R. H.; Nocedal, J.; Schnabel, R. B. (1994).
			// "Representations of Quasi-Newton Matrices and their use in
			// Limited Memory Methods". Mathematical Programming 63 (4):
			class LowRankInverseHessian extends LinearOperator {
				 private int num_parameters_;
				 private int max_num_corrections_;
			     private boolean use_approximate_eigenvalue_scaling_;
			     private double approximate_eigenvalue_scale_;
			     // ColMajorMatrix delta_x_history_;
			     private Matrix delta_x_history_;
			     // ColMajorMatrix delta_gradient_history_;
			     private Matrix delta_gradient_history_;
			     private Vector<Double> delta_x_dot_delta_gradient_;
			     private LinkedList<Integer> indices_ = new LinkedList<Integer>();
			  // num_parameters is the row/column size of the Hessian.
			  // max_num_corrections is the rank of the Hessian approximation.
			  // use_approximate_eigenvalue_scaling controls whether the initial
			  // inverse Hessian used during Right/LeftMultiply() is scaled by
			  // the approximate eigenvalue of the true inverse Hessian at the
			  // current operating point.
			  // The approximation uses:
			  // 2 * max_num_corrections * num_parameters + max_num_corrections
			  // doubles.
			  public LowRankInverseHessian(int num_parameters,
			                        int max_num_corrections,
			                        boolean use_approximate_eigenvalue_scaling) {
				  num_parameters_ = num_parameters;
			      max_num_corrections_ = max_num_corrections;
			      use_approximate_eigenvalue_scaling_ = use_approximate_eigenvalue_scaling;
			      approximate_eigenvalue_scale_ = 1.0;
			      delta_x_history_ = new Matrix(num_parameters, max_num_corrections);
			      delta_gradient_history_ = new Matrix(num_parameters, max_num_corrections);
			      delta_x_dot_delta_gradient_ = new Vector<Double>(max_num_corrections);		  
			  }

			  // Update the low rank approximation. delta_x is the change in the
			  // domain of Hessian, and delta_gradient is the change in the
			  // gradient.  The update copies the delta_x and delta_gradient
			  // vectors, and gets rid of the oldest delta_x and delta_gradient
			  // vectors if the number of corrections is already equal to
			  // max_num_corrections.
			  public boolean Update(Vector<Double> delta_x, Vector<Double> delta_gradient) {
				  int i;
				  double delta_x_dot_delta_gradient = 0.0;
				  for (i = 0; i < delta_x.size(); i++) {
					  delta_x_dot_delta_gradient += (delta_x.get(i) * delta_gradient.get(i));
				  }
				  if (delta_x_dot_delta_gradient <=
				      kLBFGSSecantConditionHessianUpdateTolerance) {
					if (2 <= MAX_LOG_LEVEL) {
				            Preferences.debug("Skipping L-BFGS Update, delta_x_dot_delta_gradient too \n" +
				                              "small: " + delta_x_dot_delta_gradient + ", tolerance: " +
				                              kLBFGSSecantConditionHessianUpdateTolerance + "\n" +
				                              " (Secant condition).\n", Preferences.DEBUG_ALGORITHM);
					}
				    return false;
				  }


				  int next = indices_.size();
				  // Once the size of the list reaches max_num_corrections_, simulate
				  // a circular buffer by removing the first element of the list and
				  // making it the next position where the LBFGS history is stored.
				  if (next == max_num_corrections_) {
				    next = indices_.getFirst();
				    indices_.removeFirst();
				  }

				  indices_.add(next);
				  for (i = 0; i < num_parameters_; i++) {
				      delta_x_history_.set(i,next,delta_x.get(i));
				      delta_gradient_history_.set(i,next,delta_gradient.get(i));
				  }
				  if (next < delta_x_dot_delta_gradient_.size()) {
				      delta_x_dot_delta_gradient_.set(next,delta_x_dot_delta_gradient);
				  }
				  else {
					  delta_x_dot_delta_gradient_.add(delta_x_dot_delta_gradient);
				  }
				  double squaredNorm = 0.0;
				  for (i = 0; i < delta_gradient.size(); i++) {
					  squaredNorm += (delta_gradient.get(i) * delta_gradient.get(i));
				  }
				  approximate_eigenvalue_scale_ =
				      delta_x_dot_delta_gradient / squaredNorm;
				  return true;

			  }
			  
			  public void RightMultiply(Vector<Double>x, Vector<Double> y) {
				  int i;
				  double x_ptr[] = new double[x.size()];
				  for (i = 0; i < x.size(); i++) {
					  x_ptr[i] = x.get(i);
				  }
				  double y_ptr[] = new double[y.size()];
				  for (i = 0; i < y.size(); i++) {
					  y_ptr[i] = y.get(i);
				  }
				  RightMultiply(x_ptr, y_ptr);
				  for (i = 0; i < y.size(); i++) {
					  y.set(i, y_ptr[i]);
				  }
			  }

			  // LinearOperator interface
			  public  void RightMultiply(double[] x_ptr, double[] y_ptr) {
				  //ConstVectorRef gradient(x_ptr, num_parameters_);
				  //VectorRef search_direction(y_ptr, num_parameters_);
                  int i,it,j;
                  for (i = 0; i < num_parameters_; i++) {
                	  y_ptr[i] = x_ptr[i];
                  }

				  int num_corrections = indices_.size();
				  Vector<Double> alpha = new Vector<Double>(num_corrections);
				  for (i = 0; i < num_corrections; i++) {
					  alpha.add(0.0);
				  }

				    for (i = indices_.size()-1; i >= 0; i--) {
				        it = indices_.get(i);
				        double num = 0.0;
				        for (j = 0; j < num_parameters_; j++) {
				        	num += delta_x_history_.get(j,it)*y_ptr[j];
				        }
				        double alpha_i = num /
				        delta_x_dot_delta_gradient_.get(it);
				        for (j = 0; j < num_parameters_; j++) {
				            y_ptr[j] -= alpha_i * delta_gradient_history_.get(j,it);
				        }
				        alpha.set(it,alpha_i);
				  }

				  if (use_approximate_eigenvalue_scaling_) {
				    // Rescale the initial inverse Hessian approximation (H_0) to be iteratively
				    // updated so that it is of similar 'size' to the true inverse Hessian along
				    // the most recent search direction.  As shown in [1]:
				    //
				    //   \gamma_k = (delta_gradient_{k-1}' * delta_x_{k-1}) /
				    //              (delta_gradient_{k-1}' * delta_gradient_{k-1})
				    //
				    // Satisfies:
				    //
				    //   (1 / \lambda_m) <= \gamma_k <= (1 / \lambda_1)
				    //
				    // Where \lambda_1 & \lambda_m are the smallest and largest eigenvalues of
				    // the true Hessian (not the inverse) along the most recent search direction
				    // respectively.  Thus \gamma is an approximate eigenvalue of the true
				    // inverse Hessian, and choosing: H_0 = I * \gamma will yield a starting
				    // point that has a similar scale to the true inverse Hessian.  This
				    // technique is widely reported to often improve convergence, however this
				    // is not universally true, particularly if there are errors in the initial
				    // jacobians, or if there are significant differences in the sensitivity
				    // of the problem to the parameters (i.e. the range of the magnitudes of
				    // the components of the gradient is large).
				    //
				    // The original origin of this rescaling trick is somewhat unclear, the
				    // earliest reference appears to be Oren [1], however it is widely discussed
				    // without specific attributation in various texts including [2] (p143/178).
				    //
				    // [1] Oren S.S., Self-scaling variable metric (SSVM) algorithms Part II:
				    //     Implementation and experiments, Management Science,
				    //     20(5), 863-874, 1974.
				    // [2] Nocedal J., Wright S., Numerical Optimization, Springer, 1999.
					  for (i = 0; i < num_parameters_; i++) {
						  y_ptr[i] *= approximate_eigenvalue_scale_;
					  }

				    if (4 <= MAX_LOG_LEVEL) {
					  Preferences.debug("Applying approximate_eigenvalue_scale: " + approximate_eigenvalue_scale_ +
							  "\nto initial inverse Hessian approximation\n.", Preferences.DEBUG_ALGORITHM);
				    }
				  }

					  for (i = 0; i < indices_.size(); i++) {
					        it = indices_.get(i);
					        double num = 0.0;
					        for (j = 0; j < num_parameters_; j++) {
					        	num += delta_gradient_history_.get(j,it)*y_ptr[j];
					        }
				            double beta = num /
				        delta_x_dot_delta_gradient_.get(it);
				        for (j = 0; j < num_parameters_; j++) {
				            y_ptr[j] += delta_x_history_.get(j,it) * (alpha.get(it) - beta);
				        }
				  }
  
			  }
			  public void LeftMultiply(double[] x, double[] y) {
			    RightMultiply(x, y);
			  }
			  public int num_rows() { return num_parameters_; }
			  public int num_cols() { return num_parameters_; }

			
			} // class LowRankInverseHessian
			
			class LBFGS extends LineSearchDirection {
				 private LowRankInverseHessian low_rank_inverse_hessian_;
			     private boolean is_positive_definite_;
				 public LBFGS(int num_parameters,
				        int max_lbfgs_rank,
				        boolean use_approximate_eigenvalue_bfgs_scaling) {
				        low_rank_inverse_hessian_ = new LowRankInverseHessian(num_parameters,
				                                  max_lbfgs_rank,
				                                  use_approximate_eigenvalue_bfgs_scaling);
				        is_positive_definite_ = true;
				 }

				  public boolean NextDirection(LineSearchMinimizer.State previous,
				                     LineSearchMinimizer.State current,
				                     Vector<Double> search_direction) {
					int i;
				    if (!is_positive_definite_) {
				        System.err.println("Ceres bug: NextDirection() called on L-BFGS after inverse Hessian \n" +
				        "approximation has become indefinite, please contact the developers!");
				        return false;
				    }

				    Vector<Double> delta_x = new Vector<Double>();
				    for (i = 0; i < previous.search_direction.size(); i++) {
				    	delta_x.add(previous.search_direction.get(i) * previous.step_size);
				    }
				    Vector<Double> delta_gradient = new Vector<Double>();
				    for (i = 0; i < current.gradient.size(); i++) {
				    	delta_gradient.add(current.gradient.get(i) - previous.gradient.get(i));
				    }
				    low_rank_inverse_hessian_.Update(
				        delta_x,
				        delta_gradient);

				    for (i = 0; i < search_direction.size(); i++) {
				    	search_direction.set(i,0.0);
				    }
				    low_rank_inverse_hessian_.RightMultiply(current.gradient,
				                                            search_direction);
				    for (i = 0; i < search_direction.size(); i++) {
				    	search_direction.set(i,-search_direction.get(i));
				    }

				    double dotProduct = 0.0;
				    for (i = 0; i < search_direction.size(); i++) {
				    	dotProduct += (search_direction.get(i) * current.gradient.get(i));
				    }
				    if (dotProduct >= 0.0) {
				      if (WARNING <= MAX_LOG_LEVEL) {
				                      Preferences.debug("Numerical failure in L-BFGS update: inverse Hessian \n" +
				                      "approximation is not positive definite, and thus \n" +
				                      "initial gradient for search direction is positive: " + dotProduct + "\n",
				                      Preferences.DEBUG_ALGORITHM);
				      }
				      is_positive_definite_ = false;
				      return false;
				    }

				    return true;
				  }

				
				} // class LBFGS
			
			class BFGS extends LineSearchDirection {
				private int num_parameters_;
			    private boolean use_approximate_eigenvalue_scaling_;
			    private Matrix inverse_hessian_;
			    private boolean initialized_;
			    private boolean is_positive_definite_;
			    
				 public BFGS(int num_parameters,
				       boolean use_approximate_eigenvalue_scaling) {
				        num_parameters_ = num_parameters;
				        use_approximate_eigenvalue_scaling_ = use_approximate_eigenvalue_scaling;
				        initialized_ = false;
				        is_positive_definite_ = true;
				        if ((WARNING <= MAX_LOG_LEVEL) && (num_parameters >= 1000)) {
				            Preferences.debug("BFGS line search being created with: " + num_parameters_ + "\n" +
				            " parameters, this will allocate a dense approximate inverse Hessian\n" +
				            " of size: " + num_parameters_ + " x "  + num_parameters_ + "\n" +
				            ", consider using the L-BFGS memory-efficient line search direction instead.\n",
				            Preferences.DEBUG_ALGORITHM);
				        }
				    // Construct inverse_hessian_ after logging warning about size s.t. if the
				    // allocation crashes us, the log will highlight what the issue likely was.
				    inverse_hessian_ = Matrix.identity(num_parameters, num_parameters);
				  }


				  public boolean NextDirection(LineSearchMinimizer.State previous,
				                     LineSearchMinimizer.State current,
				                     Vector<Double> search_direction) {
					int i,j;
				    if (!is_positive_definite_) {
				        System.err.println("Ceres bug: NextDirection() called on BFGS after inverse Hessian \n" +
				        "approximation has become indefinite, please contact the developers!");
				        return false;
				    }

				    Vector<Double>delta_x = new Vector<Double>(previous.search_direction.size()); 
				    for (i = 0; i < previous.search_direction.size(); i++) {
				    	delta_x.add(previous.search_direction.get(i) * previous.step_size);
				    }
				    Vector<Double> delta_gradient = new Vector<Double>(current.gradient.size());
				    for (i = 0; i < current.gradient.size(); i++) {
				    	delta_gradient.add(current.gradient.get(i) - previous.gradient.get(i));
				    }
				    double delta_x_dot_delta_gradient = 0.0;
				    for (i = 0; i < delta_x.size(); i++) {
				    	delta_x_dot_delta_gradient += (delta_x.get(i) * delta_gradient.get(i));
				    }

				    // The (L)BFGS algorithm explicitly requires that the secant equation:
				    //
				    //   B_{k+1} * s_k = y_k
				    //
				    // Is satisfied at each iteration, where B_{k+1} is the approximated
				    // Hessian at the k+1-th iteration, s_k = (x_{k+1} - x_{k}) and
				    // y_k = (grad_{k+1} - grad_{k}). As the approximated Hessian must be
				    // positive definite, this is equivalent to the condition:
				    //
				    //   s_k^T * y_k > 0     [s_k^T * B_{k+1} * s_k = s_k^T * y_k > 0]
				    //
				    // This condition would always be satisfied if the function was strictly
				    // convex, alternatively, it is always satisfied provided that a Wolfe line
				    // search is used (even if the function is not strictly convex).  See [1]
				    // (p138) for a proof.
				    //
				    // Although Ceres will always use a Wolfe line search when using (L)BFGS,
				    // practical implementation considerations mean that the line search
				    // may return a point that satisfies only the Armijo condition, and thus
				    // could violate the Secant equation.  As such, we will only use a step
				    // to update the Hessian approximation if:
				    //
				    //   s_k^T * y_k > tolerance
				    //
				    // It is important that tolerance is very small (and >=0), as otherwise we
				    // might skip the update too often and fail to capture important curvature
				    // information in the Hessian.  For example going from 1e-10 -> 1e-14
				    // improves the NIST benchmark score from 43/54 to 53/54.
				    //
				    // [1] Nocedal J, Wright S, Numerical Optimization, 2nd Ed. Springer, 1999.
				    //
				    // TODO(alexs.mac): Consider using Damped BFGS update instead of
				    // skipping update.
				    final double kBFGSSecantConditionHessianUpdateTolerance = 1e-14;

				    if (delta_x_dot_delta_gradient <=
				        kBFGSSecantConditionHessianUpdateTolerance) {
				      if (2 <= MAX_LOG_LEVEL) {
				              Preferences.debug("Skipping BFGS Update, delta_x_dot_delta_gradient too \n" +
				              "small: " + delta_x_dot_delta_gradient + ", tolerance: " + kBFGSSecantConditionHessianUpdateTolerance + "\n" +
				              " (Secant condition).\n", Preferences.DEBUG_ALGORITHM);
				      }
				    } else {
				      // Update dense inverse Hessian approximation.

				      if (!initialized_ && use_approximate_eigenvalue_scaling_) {
				        // Rescale the initial inverse Hessian approximation (H_0) to be
				        // iteratively updated so that it is of similar 'size' to the true
				        // inverse Hessian at the start point.  As shown in [1]:
				        //
				        //   \gamma = (delta_gradient_{0}' * delta_x_{0}) /
				        //            (delta_gradient_{0}' * delta_gradient_{0})
				        //
				        // Satisfies:
				        //
				        //   (1 / \lambda_m) <= \gamma <= (1 / \lambda_1)
				        //
				        // Where \lambda_1 & \lambda_m are the smallest and largest eigenvalues
				        // of the true initial Hessian (not the inverse) respectively. Thus,
				        // \gamma is an approximate eigenvalue of the true inverse Hessian, and
				        // choosing: H_0 = I * \gamma will yield a starting point that has a
				        // similar scale to the true inverse Hessian.  This technique is widely
				        // reported to often improve convergence, however this is not
				        // universally true, particularly if there are errors in the initial
				        // gradients, or if there are significant differences in the sensitivity
				        // of the problem to the parameters (i.e. the range of the magnitudes of
				        // the components of the gradient is large).
				        //
				        // The original origin of this rescaling trick is somewhat unclear, the
				        // earliest reference appears to be Oren [1], however it is widely
				        // discussed without specific attributation in various texts including
				        // [2] (p143).
				        //
				        // [1] Oren S.S., Self-scaling variable metric (SSVM) algorithms
				        //     Part II: Implementation and experiments, Management Science,
				        //     20(5), 863-874, 1974.
				        // [2] Nocedal J., Wright S., Numerical Optimization, Springer, 1999.
				    	double dotProduct = 0.0;
				    	for (i = 0; i < delta_gradient.size(); i++) {
				    		dotProduct += (delta_gradient.get(i) * delta_gradient.get(i));
				    	}
				        double approximate_eigenvalue_scale =
				            delta_x_dot_delta_gradient / dotProduct;
				        
				        for (i = 0; i < inverse_hessian_.getRowDimension(); i++) {
				        	for (j = 0; j < inverse_hessian_.getColumnDimension(); j++) {
				        		inverse_hessian_.set(i, j, inverse_hessian_.get(i,j) * approximate_eigenvalue_scale);
				        	}
				        	
				        }
                           if (4 <= MAX_LOG_LEVEL) {
				                Preferences.debug("Applying approximate_eigenvalue_scale: " + approximate_eigenvalue_scale +
				                "\nto initial inverse Hessian approximation.\n", Preferences.DEBUG_ALGORITHM);
                           }
				      }
				      initialized_ = true;

				      // Efficient O(num_parameters^2) BFGS update [2].
				      //
				      // Starting from dense BFGS update detailed in Nocedal [2] p140/177 and
				      // using: y_k = delta_gradient, s_k = delta_x:
				      //
				      //   \rho_k = 1.0 / (s_k' * y_k)
				      //   V_k = I - \rho_k * y_k * s_k'
				      //   H_k = (V_k' * H_{k-1} * V_k) + (\rho_k * s_k * s_k')
				      //
				      // This update involves matrix, matrix products which naively O(N^3),
				      // however we can exploit our knowledge that H_k is positive definite
				      // and thus by defn. symmetric to reduce the cost of the update:
				      //
				      // Expanding the update above yields:
				      //
				      //   H_k = H_{k-1} +
				      //         \rho_k * ( (1.0 + \rho_k * y_k' * H_k * y_k) * s_k * s_k' -
				      //                    (s_k * y_k' * H_k + H_k * y_k * s_k') )
				      //
				      // Using: A = (s_k * y_k' * H_k), and the knowledge that H_k = H_k', the
				      // last term simplifies to (A + A'). Note that although A is not symmetric
				      // (A + A') is symmetric. For ease of construction we also define
				      // B = (1 + \rho_k * y_k' * H_k * y_k) * s_k * s_k', which is by defn
				      // symmetric due to construction from: s_k * s_k'.
				      //
				      // Now we can write the BFGS update as:
				      //
				      //   H_k = H_{k-1} + \rho_k * (B - (A + A'))

				      // For efficiency, as H_k is by defn. symmetric, we will only maintain the
				      // *lower* triangle of H_k (and all intermediary terms).

				      double rho_k = 1.0 / delta_x_dot_delta_gradient;

				      // Calculate: A = s_k * y_k' * H_k
				      /*Matrix A = delta_x * (delta_gradient.transpose() *
				                            inverse_hessian_.selfadjointView<Eigen::Lower>());

				      // Calculate scalar: (1 + \rho_k * y_k' * H_k * y_k)
				      const double delta_x_times_delta_x_transpose_scale_factor =
				          (1.0 + (rho_k * delta_gradient.transpose() *
				                  inverse_hessian_.selfadjointView<Eigen::Lower>() *
				                  delta_gradient));
				      // Calculate: B = (1 + \rho_k * y_k' * H_k * y_k) * s_k * s_k'
				      Matrix B = Matrix::Zero(num_parameters_, num_parameters_);
				      B.selfadjointView<Eigen::Lower>().
				          rankUpdate(delta_x, delta_x_times_delta_x_transpose_scale_factor);

				      // Finally, update inverse Hessian approximation according to:
				      // H_k = H_{k-1} + \rho_k * (B - (A + A')).  Note that (A + A') is
				      // symmetric, even though A is not.
				      inverse_hessian_.triangularView<Eigen::Lower>() +=
				          rho_k * (B - A - A.transpose());*/
				      
				      // Just use the whole matrix rather than only the lower triangle
				      // Produce 1 by num_parameters_ array
				      double di[] = new double[num_parameters_];
				      for (i = 0; i < num_parameters_; i++) {
				    	  for (j = 0; j < num_parameters_; j++) {
				    		  di[i] += (delta_gradient.get(j) * inverse_hessian_.get(j,i));
				    	  }
				      }
				      // A is num_parameters_ by num_parameters_
				      double A[][] = new double[num_parameters_][num_parameters_];
				      for (i = 0; i < num_parameters_; i++) {
				    	  for (j = 0; j < num_parameters_; j++) {
				    		  A[i][j] = delta_x.get(i) * di[j];
				    	  }
				      }
				      
				      // Produce a num_parameters_ by 1 array
				      double id[] = new double[num_parameters_];
				      for (i = 0; i < num_parameters_; i++) {
				    	  for (j = 0; j < num_parameters_; j++) {
				    		  id[i] += (inverse_hessian_.get(i,j) * delta_gradient.get(j));
				    	  }
				      }
				      double delta_x_times_delta_x_transpose_scale_factor = 1.0;
				      for (i = 0; i < num_parameters_; i++) {
				    	  delta_x_times_delta_x_transpose_scale_factor += (rho_k * delta_gradient.get(i) * id[i]);
				      }
				      
				      double B[][] = new double[num_parameters_][num_parameters_];
				      for (i = 0; i < num_parameters_; i++) {
				    	  for (j = 0; j < num_parameters_; j++) {
				    		  B[i][j] = delta_x_times_delta_x_transpose_scale_factor * delta_x.get(i) * delta_x.get(j);
				    	  }
				      }
				      
				      for (i = 0; i < num_parameters_; i++) {
				    	  for (j = 0; j < num_parameters_; j++) {
				    		  inverse_hessian_.set(i, j, inverse_hessian_.get(i,j) + rho_k*(B[i][j] - A[i][j] - A[j][i]));
				    	  }
				      }
				    }

				    //*search_direction =
				        //inverse_hessian_.selfadjointView<Eigen::Lower>() *
				       // (-1.0 * current.gradient);
				    for (i = 0; i < num_parameters_; i++) {
				    	search_direction.set(i,0.0);
				    	for (j = 0; j < num_parameters_; j++) {
				    		search_direction.set(i, search_direction.get(i) - inverse_hessian_.get(i,j)*current.gradient.get(j));
				    	}
				    }

				    double dotProduct = 0.0;
				    for (i = 0; i < num_parameters_; i++) {
				    	dotProduct += (search_direction.get(i) * current.gradient.get(i));
				    }
				    if (dotProduct >= 0.0) {
				      if (WARNING <= MAX_LOG_LEVEL) {
				                   Preferences.debug("Numerical failure in BFGS update: inverse Hessian \n" +
				                   "approximation is not positive definite, and thus \n" +
				                   "initial gradient for search direction is positive: " + dotProduct + "\n",
				                   Preferences.DEBUG_ALGORITHM);
				      }
				      is_positive_definite_ = false;
				      return false;
				    }

				    return true;
				  }

				 
				} // class BFGS
		
				public LineSearchDirection  Create(LineSearchDirectionOptions options) {
				  if (options.type == LineSearchDirectionType.STEEPEST_DESCENT) {
				    return new SteepestDescent();
				  }

				  if (options.type == LineSearchDirectionType.NONLINEAR_CONJUGATE_GRADIENT) {
				    return new NonlinearConjugateGradient(
				        options.nonlinear_conjugate_gradient_type,
				        options.function_tolerance);
				  }

				  if (options.type == LineSearchDirectionType.LBFGS) {
				    return new LBFGS(
				        options.num_parameters,
				        options.max_lbfgs_rank,
				        options.use_approximate_eigenvalue_bfgs_scaling);
				  }

				  if (options.type == LineSearchDirectionType.BFGS) {
				    return new BFGS(
				        options.num_parameters,
				        options.use_approximate_eigenvalue_bfgs_scaling);
				  }

				  System.err.println("Unknown line search direction type: " + options.type);
				  return null;
				}


		
		class LineSearchDirectionOptions {
			public int num_parameters;
		    public LineSearchDirectionType type;
		    public NonlinearConjugateGradientType nonlinear_conjugate_gradient_type;
		    public double function_tolerance;
		    public int max_lbfgs_rank;
		    public boolean use_approximate_eigenvalue_bfgs_scaling;
		    
		    public LineSearchDirectionOptions() {
		          num_parameters = 0;
		          type = LineSearchDirectionType.LBFGS;
		          nonlinear_conjugate_gradient_type = NonlinearConjugateGradientType.FLETCHER_REEVES;
		          function_tolerance = 1e-12; 
		          max_lbfgs_rank = 20;
		          use_approximate_eigenvalue_bfgs_scaling = true;
		    }
		}
		
		abstract class LineSearchDirection {
			
			 public LineSearchDirectionOptions options;
			 
			 public LineSearchDirection() {
				 options = new LineSearchDirectionOptions();
			 }


			 public abstract boolean NextDirection(LineSearchMinimizer.State previous,
			                             LineSearchMinimizer.State current,
			                             Vector<Double> search_direction);
		} // class LineSearchDirection
		
		public String LineSearchDirectionTypeToString(LineSearchDirectionType type) {
			  switch (type) {
			  case STEEPEST_DESCENT:
				  return "STEEPEST_DESCENT";
			  case NONLINEAR_CONJUGATE_GRADIENT:
				  return "NONLINEAR_CONJUGATE_GRADIENT";
			  case LBFGS:
				  return "LBFGS";
			  case BFGS:
				  return "BFGS";
			    default:
			      return "UNKNOWN";
			  }
			}

		
		// Generic line search minimization algorithm.
		//
		// For example usage, see SolverImpl::Minimize.
		class LineSearchMinimizer extends Minimizer {
			public LineSearchMinimizer() {
				super();
			}
		
		  class State {
			  public double cost[] = new double[1];
			  public Vector<Double> gradient;
			  public double gradient_squared_norm;
			  public double gradient_max_norm;
			  public Vector<Double> search_direction;
			  public double directional_derivative;
			  public double step_size;
		      public State(int num_parameters,
		          int num_effective_parameters) {
		    	  int i;
		          cost[0] = 0.0;
		          gradient = new Vector<Double>(num_effective_parameters);
		          for (i = 0; i < num_effective_parameters; i++) {
		        	  gradient.add(0.0);
		          }
		          gradient_squared_norm = 0.0;
		          search_direction = new Vector<Double>(num_effective_parameters);
		          for (i = 0; i < num_effective_parameters; i++) {
		        	  search_direction.add(0.0);
		          }
		          directional_derivative = 0.0;
		          step_size = 0.0; 
		    }

		    
		  }
		  
		  public void copyState(State dst, State src) {
			  dst.cost[0] = src.cost[0];
			  dst.gradient.clear();
			  dst.gradient.addAll(src.gradient);
			  dst.gradient_squared_norm = src.gradient_squared_norm;
			  dst.gradient_max_norm = src.gradient_max_norm;
			  dst.search_direction.clear();
			  dst.search_direction.addAll(src.search_direction);
			  dst.directional_derivative = src.directional_derivative;
			  dst.step_size = src.step_size;
		  }
		  
		  private boolean EvaluateGradientNorms(Evaluator evaluator,
                  double[] x,
                  LineSearchMinimizer.State state,
                  String[] message) {
			  int  i;
			  boolean status;
		      Vector<Double> xvec = new Vector<Double>(x.length);  
		      for (i = 0; i < x.length; i++) {
		    	  xvec.add(x[i]);
		      }
		      status = EvaluateGradientNorms(evaluator, xvec, state, message);
		      for (i = 0; i < x.length; i++) {
		    	  x[i] = xvec.get(i);
		      }
		      return status;
		  }
		  
		  private boolean EvaluateGradientNorms(Evaluator evaluator,
                  Vector<Double> x,
                  LineSearchMinimizer.State state,
                  String[] message) {
			int i;
			Vector<Double>negative_gradient = new Vector<Double>(state.gradient.size());
			for (i = 0; i < state.gradient.size(); i++) {
				negative_gradient.add(-state.gradient.get(i));
			}
			Vector<Double> projected_gradient_step = new Vector<Double>(x.size());
			for (i = 0; i < x.size(); i++) {
				projected_gradient_step.add(0.0);
			}
			if (!evaluator.Plus(
			 x, negative_gradient, projected_gradient_step)) {
			message[0] = "projected_gradient_step = Plus(x, -gradient) failed.";
			return false;
			}
			
			double squaredNorm = 0.0;
			double lpNorm = 0.0;
			double diff;
			for (i = 0; i < x.size(); i++) {
				diff = x.get(i) - projected_gradient_step.get(i);
				squaredNorm += diff * diff;
				if (Math.abs(diff) > lpNorm) {
					lpNorm = Math.abs(diff);
				}
			}
			state.gradient_squared_norm = squaredNorm;
			state.gradient_max_norm = lpNorm;
			return true;
		}


		  public void Minimize(MinimizerOptions options,
		                        double[] parameters,
		                        SolverSummary summary) {
			  int i;
			  boolean status;
			  double diff;
			  double norm;
			  boolean is_not_silent = !options.is_silent;
			  double start_time = 1.0E-3 * System.currentTimeMillis();
			  double iteration_start_time =  start_time;

			  if (options.evaluator == null) {
				  System.err.println("options.evaluator == null in LineSearchMinimizer Minimize");
				  return;
			  }
			  Evaluator evaluator = options.evaluator;
			  int num_parameters = evaluator.NumParameters();
			  int num_effective_parameters = evaluator.NumEffectiveParameters();

			  summary.termination_type = TerminationType.NO_CONVERGENCE;
			  summary.num_successful_steps = 0;
			  summary.num_unsuccessful_steps = 0;

			  //VectorRef x(parameters, num_parameters);
			  double x[] = new double[num_parameters];
			  for (i = 0; i < num_parameters; i++) {
				  x[i] = parameters[i];
			  }

			  State current_state = new State(num_parameters, num_effective_parameters);
			  State previous_state = new State(num_parameters, num_effective_parameters);

			  IterationSummary iteration_summary = new IterationSummary();
			  iteration_summary.iteration = 0;
			  iteration_summary.step_is_valid = false;
			  iteration_summary.step_is_successful = false;
			  iteration_summary.cost_change = 0.0;
			  iteration_summary.gradient_max_norm = 0.0;
			  iteration_summary.gradient_norm = 0.0;
			  iteration_summary.step_norm = 0.0;
			  iteration_summary.linear_solver_iterations = 0;
			  iteration_summary.step_solver_time_in_seconds = 0;

			  // Do initial cost and gradient evaluation.
			  double gradient_array[] = new double[current_state.gradient.size()];
			  for (i = 0; i < current_state.gradient.size(); i++) {
				  gradient_array[i] = current_state.gradient.get(i);
			  }
			  status = evaluator.Evaluate(x,
			                           current_state.cost,
			                           null,
			                           gradient_array,
			                           null);
			  for (i = 0; i < num_parameters; i++) {
				  parameters[i] = x[i];
			  }
			  for (i = 0; i < current_state.gradient.size(); i++) {
				  current_state.gradient.set(i,gradient_array[i]);
			  }
			  if (!status) {
			    summary.termination_type = TerminationType.FAILURE;
			    summary.message[0] = "Initial cost and jacobian evaluation failed.";
			    if ((WARNING <= MAX_LOG_LEVEL) && is_not_silent) {
			        Preferences.debug("Terminating: " + summary.message[0] + "\n",
			        		Preferences.DEBUG_ALGORITHM);
			    }
			    return;
			  }
  
			    status = EvaluateGradientNorms(evaluator, x, current_state, summary.message);
			    for (i = 0; i < num_parameters; i++) {
					  parameters[i] = x[i];
				  }
			    if (!status) {
			    summary.termination_type = TerminationType.FAILURE;
			    summary.message[0] = "Initial cost and jacobian evaluation failed. \n" +
			        "More details: " + summary.message[0];
			    if ((WARNING <= MAX_LOG_LEVEL) && is_not_silent) {
			        Preferences.debug("Terminating: " + summary.message[0] + "\n",
			        		Preferences.DEBUG_ALGORITHM);
			    }
			    return;
			  }

			  summary.initial_cost = current_state.cost[0] + summary.fixed_cost;
			  iteration_summary.cost = current_state.cost[0] + summary.fixed_cost;

			  iteration_summary.gradient_norm = Math.sqrt(current_state.gradient_squared_norm);
			  iteration_summary.gradient_max_norm = current_state.gradient_max_norm;
			  if (iteration_summary.gradient_max_norm <= options.gradient_tolerance) {
			    summary.message[0] = String.format("Gradient tolerance reached. \n" +
			                                    "Gradient max norm: %e <= %e",
			                                    iteration_summary.gradient_max_norm,
			                                    options.gradient_tolerance);
			    summary.termination_type = TerminationType.CONVERGENCE;
			    if ((1 <= MAX_LOG_LEVEL) && is_not_silent) {
			        Preferences.debug("Terminating: " + summary.message[0] + "\n",
			        		Preferences.DEBUG_ALGORITHM);
			    }
			    return;
			  }

			  iteration_summary.iteration_time_in_seconds =
			      1.0E-3 * System.currentTimeMillis() - iteration_start_time;
			  iteration_summary.cumulative_time_in_seconds =
			      1.0E-3 * System.currentTimeMillis() - start_time
			      + summary.preprocessor_time_in_seconds;
			  summary.iterations.add(iteration_summary);

			  LineSearchDirectionOptions line_search_direction_options = new LineSearchDirectionOptions();
			  line_search_direction_options.num_parameters = num_effective_parameters;
			  line_search_direction_options.type = options.line_search_direction_type;
			  line_search_direction_options.nonlinear_conjugate_gradient_type =
			      options.nonlinear_conjugate_gradient_type;
			  line_search_direction_options.max_lbfgs_rank = options.max_lbfgs_rank;
			  line_search_direction_options.use_approximate_eigenvalue_bfgs_scaling =
			      options.use_approximate_eigenvalue_bfgs_scaling;
			  LineSearchDirection line_search_direction = Create(line_search_direction_options);

			  LineSearchFunction line_search_function = new LineSearchFunction(evaluator);

			  LineSearchOptions line_search_options = new LineSearchOptions();
			  line_search_options.interpolation_type =
			      options.line_search_interpolation_type;
			  line_search_options.min_step_size = options.min_line_search_step_size;
			  line_search_options.sufficient_decrease =
			      options.line_search_sufficient_function_decrease;
			  line_search_options.max_step_contraction =
			      options.max_line_search_step_contraction;
			  line_search_options.min_step_contraction =
			      options.min_line_search_step_contraction;
			  line_search_options.max_num_iterations =
			      options.max_num_line_search_step_size_iterations;
			  line_search_options.sufficient_curvature_decrease =
			      options.line_search_sufficient_curvature_decrease;
			  line_search_options.max_step_expansion =
			      options.max_line_search_step_expansion;
			  line_search_options.is_silent = options.is_silent;
			  line_search_options.function = line_search_function;

			  LineSearch line_search = Create(options.line_search_type,
			                                     line_search_options,
			                                     summary.message);
			  if (line_search == null) {
			    summary.termination_type = TerminationType.FAILURE;
			    if ((ERROR <= MAX_LOG_LEVEL) && is_not_silent) {
			        Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
			    }
			    return;
			  }

			  LineSearchSummary line_search_summary = new LineSearchSummary();
			  int num_line_search_direction_restarts = 0;

			  while (true) {
			    if (!RunCallbacks(options, iteration_summary, summary)) {
			      break;
			    }

			    iteration_start_time = 1.0E-3 * System.currentTimeMillis();
			    if (iteration_summary.iteration >= options.max_num_iterations) {
			      summary.message[0] = "Maximum number of iterations reached.";
			      summary.termination_type = TerminationType.NO_CONVERGENCE;
			      if ((1 <= MAX_LOG_LEVEL) && is_not_silent) {
				        Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
				  }
			      break;
			    }

			    double total_solver_time = iteration_start_time - start_time +
			        summary.preprocessor_time_in_seconds;
			    if (total_solver_time >= options.max_solver_time_in_seconds) {
			      summary.message[0] = "Maximum solver time reached.";
			      summary.termination_type = TerminationType.NO_CONVERGENCE;
			      if ((1 <= MAX_LOG_LEVEL) && is_not_silent) {
				        Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
				  }
			      break;
			    }

			    iteration_summary = new IterationSummary();
			    iteration_summary.iteration = summary.iterations.get(summary.iterations.size()-1).iteration + 1;
			    iteration_summary.step_is_valid = false;
			    iteration_summary.step_is_successful = false;

			    boolean line_search_status = true;
			    if (iteration_summary.iteration == 1) {
			      current_state.search_direction.clear();
			      for (i = 0; i < current_state.gradient.size(); i++) {
			    	  current_state.search_direction.add(-current_state.gradient.get(i));
			      }
			    } else {
			      line_search_status = line_search_direction.NextDirection(
			          previous_state,
			          current_state,
			          current_state.search_direction);
			    }

			    if (!line_search_status &&
			        num_line_search_direction_restarts >=
			        options.max_num_line_search_direction_restarts) {
			      // Line search direction failed to generate a new direction, and we
			      // have already reached our specified maximum number of restarts,
			      // terminate optimization.
			      summary.message[0] =
			          String.format("Line search direction failure: specified \n" +
			                       "max_num_line_search_direction_restarts: %d reached.",
			                       options.max_num_line_search_direction_restarts);
			      summary.termination_type = TerminationType.FAILURE;
			      if ((WARNING <= MAX_LOG_LEVEL) && is_not_silent) {
				        Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
				  }
			      break;
			    } else if (!line_search_status) {
			      // Restart line search direction with gradient descent on first iteration
			      // as we have not yet reached our maximum number of restarts.
			      if (num_line_search_direction_restarts >=
			               options.max_num_line_search_direction_restarts) {
			          System.err.println("num_line_search_direction_restarts >=\n" + 
			          "options.max_num_line_search_direction_restarts in LineSearchMinimizer Minimize");
			          return;
			      }

			      ++num_line_search_direction_restarts;
			      if ((WARNING <= MAX_LOG_LEVEL) && is_not_silent) {
			          Preferences.debug("Line search direction algorithm: " +
			          LineSearchDirectionTypeToString(options.line_search_direction_type) + "\n" +
			          ", failed to produce a valid new direction at " +
			          "iteration: " + iteration_summary.iteration + "\n" +
			          ". Restarting, number of restarts: \n" +
			          num_line_search_direction_restarts + " / " +
			          options.max_num_line_search_direction_restarts +
			           " [max].\n",Preferences.DEBUG_ALGORITHM);
			      }
			      line_search_direction = Create(line_search_direction_options);
			      current_state.search_direction.clear();
			      for (i = 0; i < current_state.gradient.size(); i++) {
			    	  current_state.search_direction.add(-current_state.gradient.get(i));
			      }
			    }

			    line_search_function.Init(x, current_state.search_direction);
			    current_state.directional_derivative = 0.0;
			    for (i = 0; i < current_state.gradient.size(); i++) {
			    	current_state.directional_derivative += (current_state.gradient.get(i) * current_state.search_direction.get(i));
			    }

			    // TODO(sameeragarwal): Refactor this into its own object and add
			    // explanations for the various choices.
			    //
			    // Note that we use !line_search_status to ensure that we treat cases when
			    // we restarted the line search direction equivalently to the first
			    // iteration.
			    double initial_step_size =
			        (iteration_summary.iteration == 1 || !line_search_status)
			        ? Math.min(1.0, 1.0 / current_state.gradient_max_norm)
			        : Math.min(1.0, 2.0 * (current_state.cost[0] - previous_state.cost[0]) /
			                   current_state.directional_derivative);
			    // By definition, we should only ever go forwards along the specified search
			    // direction in a line search, most likely cause for this being violated
			    // would be a numerical failure in the line search direction calculation.
			    if (initial_step_size < 0.0) {
			      summary.message[0] =
			          String.format("Numerical failure in line search, initial_step_size is \n" +
			                       "negative: %.5e, directional_derivative: %.5e, \n" +
			                       "(current_cost - previous_cost): %.5e",
			                       initial_step_size, current_state.directional_derivative,
			                       (current_state.cost[0] - previous_state.cost[0]));
			      summary.termination_type = TerminationType.FAILURE;
			      if ((WARNING <= MAX_LOG_LEVEL) && is_not_silent) {
				        Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
				  }
			      break;
			    }

			    line_search.Search(initial_step_size,
			                        current_state.cost[0],
			                        current_state.directional_derivative,
			                        line_search_summary);
			    if (!line_search_summary.success) {
			      summary.message[0] =
			          String.format("Numerical failure in line search, failed to find \n" +
			                       "a valid step size, (did not run out of iterations) \n" +
			                       "using initial_step_size: %.5e, initial_cost: %.5e, \n" +
			                       "initial_gradient: %.5e.",
			                       initial_step_size, current_state.cost[0],
			                       current_state.directional_derivative);
			      if ((WARNING <= MAX_LOG_LEVEL) && is_not_silent) {
				        Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
				  }
			      summary.termination_type = TerminationType.FAILURE;
			      break;
			    }

			    FunctionSample optimal_point = line_search_summary.optimal_point;
			    if (!optimal_point.vector_x_is_valid) {
			        System.err.println("Congratulations, you found a bug in Ceres. Please report it.");
			        System.err.println("!optimal_point.vector_x_is_valid in LineSearchMinimizer Minimize");
			        return;
			    }
			    current_state.step_size = optimal_point.x;
			    copyState(previous_state,current_state);
			    iteration_summary.step_solver_time_in_seconds =
			        1.0E-3 * System.currentTimeMillis() - iteration_start_time;

			    if (optimal_point.vector_gradient_is_valid) {
			      current_state.cost[0] = optimal_point.value[0];
			      current_state.gradient.clear();
			      for (i = 0; i < optimal_point.vector_gradient.size(); i++) {
			          current_state.gradient.add(optimal_point.vector_gradient.get(i));
			      }
			    } else {
			      EvaluateOptions evaluate_options = new EvaluateOptions();
			      evaluate_options.new_evaluation_point = false;
			      double evx[] = new double[optimal_point.vector_x.size()];
			      for (i = 0; i < evx.length; i++) {
			    	  evx[i] = optimal_point.vector_x.get(i);
			      }
			      double evg[] = new double[evx.length];
			      status = evaluator.Evaluate(evaluate_options, evx, current_state.cost, null, evg, null);
			      for (i = 0; i < evx.length; i++) {
			    	  optimal_point.vector_x.set(i,evx[i]);
			      }
			      current_state.gradient.clear();
			      for (i = 0; i < evg.length; i++) {
			    	  current_state.gradient.add(evg[i]);
			      }
			      if (!status) {
			        summary.termination_type = TerminationType.FAILURE;
			        summary.message[0] = "Cost and jacobian evaluation failed.";
			        if ((WARNING <= MAX_LOG_LEVEL) && is_not_silent) {
				        Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
				    }
			        return;
			      }
			    }

			    if (!EvaluateGradientNorms(evaluator,
			                               optimal_point.vector_x,
			                               current_state,
			                               summary.message)) {
			      summary.termination_type = TerminationType.FAILURE;
			      summary.message[0] =
			          "Step failed to evaluate. This should not happen as the step was \n" +
			          "valid when it was selected by the line search. More details: \n" +
			          summary.message[0];
			      if ((WARNING <= MAX_LOG_LEVEL) && is_not_silent) {
				        Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
				  }
			      break;
			    }

			    // Compute the norm of the step in the ambient space.
			    norm = 0.0;
			    for (i = 0; i < x.length; i++) {
			        diff = optimal_point.vector_x.get(i) - x[i];
			        norm += (diff * diff);
			    }
			    iteration_summary.step_norm = Math.sqrt(norm);
			    norm = 0.0;
			    for (i = 0; i < x.length; i++) {
			    	norm += (x[i]*x[i]);
			    }
			    double x_norm = Math.sqrt(norm);
			    for (i = 0; i < x.length; i++) {
			    	x[i] = optimal_point.vector_x.get(i);
			    	parameters[i] = x[i];
			    }

			    iteration_summary.gradient_max_norm = current_state.gradient_max_norm;
			    iteration_summary.gradient_norm = Math.sqrt(current_state.gradient_squared_norm);
			    iteration_summary.cost_change = previous_state.cost[0] - current_state.cost[0];
			    iteration_summary.cost = current_state.cost[0] + summary.fixed_cost;

			    iteration_summary.step_is_valid = true;
			    iteration_summary.step_is_successful = true;
			    iteration_summary.step_size =  current_state.step_size;
			    iteration_summary.line_search_function_evaluations =
			        line_search_summary.num_function_evaluations;
			    iteration_summary.line_search_gradient_evaluations =
			        line_search_summary.num_gradient_evaluations;
			    iteration_summary.line_search_iterations =
			        line_search_summary.num_iterations;
			    iteration_summary.iteration_time_in_seconds =
			        1.0E-3 * System.currentTimeMillis() - iteration_start_time;
			    iteration_summary.cumulative_time_in_seconds =
			    		1.0E-3 * System.currentTimeMillis() - start_time
			        + summary.preprocessor_time_in_seconds;
			    summary.iterations.add(iteration_summary);

			    // Iterations inside the line search algorithm are considered
			    // 'steps' in the broader context, to distinguish these inner
			    // iterations from from the outer iterations of the line search
			    // minimizer. The number of line search steps is the total number
			    // of inner line search iterations (or steps) across the entire
			    // minimization.
			    summary.num_line_search_steps +=  line_search_summary.num_iterations;
			    summary.line_search_cost_evaluation_time_in_seconds +=
			        line_search_summary.cost_evaluation_time_in_seconds[0];
			    summary.line_search_gradient_evaluation_time_in_seconds +=
			        line_search_summary.gradient_evaluation_time_in_seconds[0];
			    summary.line_search_polynomial_minimization_time_in_seconds +=
			        line_search_summary.polynomial_minimization_time_in_seconds;
			    summary.line_search_total_time_in_seconds +=
			        line_search_summary.total_time_in_seconds;
			    ++summary.num_successful_steps;

			    double step_size_tolerance = options.parameter_tolerance *
			                                       (x_norm + options.parameter_tolerance);
			    if (iteration_summary.step_norm <= step_size_tolerance) {
			      summary.message[0] =
			          String.format("Parameter tolerance reached. \n" +
			                       "Relative step_norm: %e <= %e.",
			                       (iteration_summary.step_norm /
			                        (x_norm + options.parameter_tolerance)),
			                       options.parameter_tolerance);
			      summary.termination_type = TerminationType.CONVERGENCE;
			      if ((1 <= MAX_LOG_LEVEL) && is_not_silent) {
				        Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
				  }
			      return;
			    }

			    if (iteration_summary.gradient_max_norm <= options.gradient_tolerance) {
			      summary.message[0] = String.format("Gradient tolerance reached. \n" +
			                                      "Gradient max norm: %e <= %e",
			                                      iteration_summary.gradient_max_norm,
			                                      options.gradient_tolerance);
			      summary.termination_type = TerminationType.CONVERGENCE;
			      if ((1 <= MAX_LOG_LEVEL) && is_not_silent) {
				        Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
				  }
			      break;
			    }

			    double absolute_function_tolerance =
			        options.function_tolerance * previous_state.cost[0];
			    if (Math.abs(iteration_summary.cost_change) <= absolute_function_tolerance) {
			      summary.message[0] =
			          String.format("Function tolerance reached. \n" +
			                       "|cost_change|/cost: %e <= %e",
			                       Math.abs(iteration_summary.cost_change) /
			                       previous_state.cost[0],
			                       options.function_tolerance);
			      summary.termination_type = TerminationType.CONVERGENCE;
			      if ((1 <= MAX_LOG_LEVEL) && is_not_silent) {
				        Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
				  }
			      break;
			    }
			  }
  
		  }
		} // class LineSearchMinimizer
		
		public boolean RunCallbacks(MinimizerOptions options,
                IterationSummary iteration_summary,
                SolverSummary summary) {
			final boolean is_not_silent = !options.is_silent;
			CallbackReturnType status = CallbackReturnType.SOLVER_CONTINUE;
			int i = 0;
			while (status == CallbackReturnType.SOLVER_CONTINUE && i < options.callbacks.size()) {
			status = (options.callbacks.get(i)).operator(iteration_summary);
			++i;
			}
			switch (status) {
			case SOLVER_CONTINUE:
			return true;
			case SOLVER_TERMINATE_SUCCESSFULLY:
			summary.termination_type = TerminationType.USER_SUCCESS;
			summary.message[0] =
			"User callback returned SOLVER_TERMINATE_SUCCESSFULLY.";
			if (is_not_silent && (1 <= MAX_LOG_LEVEL)) {
			    Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
			}
			return false;
			case SOLVER_ABORT:
			summary.termination_type = TerminationType.USER_FAILURE;
			summary.message[0] = "User callback returned SOLVER_ABORT.";
			if (is_not_silent && (1 <= MAX_LOG_LEVEL)) {
			    Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);
			}
			return false;
			default:
			System.err.println("Unknown type of user callback status");
			}
			return false;
			}

		
		// Options struct to control the behaviour of the Minimizer. Please
				// see solver.h for detailed information about the meaning and
				// default values of each of these parameters.
				class MinimizerOptions {
					public int max_num_iterations;
					public double max_solver_time_in_seconds;
					public int num_threads;

					// Number of times the linear solver should be retried in case of
					// numerical failure. The retries are done by exponentially scaling up
					// mu at each retry. This leads to stronger and stronger
					// regularization making the linear least squares problem better
					// conditioned at each retry.
					public int max_step_solver_retries;
					public double gradient_tolerance;
					public double parameter_tolerance;
					public double function_tolerance;
					public double min_relative_decrease;
					public double eta;
					public boolean jacobi_scaling;
					public boolean use_nonmonotonic_steps;
					public int max_consecutive_nonmonotonic_steps;
					public Vector<Integer> trust_region_minimizer_iterations_to_dump;
					public DumpFormatType trust_region_problem_dump_format_type;
					public String trust_region_problem_dump_directory;
					public int max_num_consecutive_invalid_steps;
					public double min_trust_region_radius;
					public LineSearchDirectionType line_search_direction_type;
					public LineSearchType line_search_type;
					public NonlinearConjugateGradientType nonlinear_conjugate_gradient_type;
					public int max_lbfgs_rank;
					public boolean use_approximate_eigenvalue_bfgs_scaling;
					public LineSearchInterpolationType line_search_interpolation_type;
					public double min_line_search_step_size;
					public double line_search_sufficient_function_decrease;
					public double max_line_search_step_contraction;
					public double min_line_search_step_contraction;
					public int max_num_line_search_step_size_iterations;
					public int max_num_line_search_direction_restarts;
					public double line_search_sufficient_curvature_decrease;
					public double max_line_search_step_expansion;
					public double inner_iteration_tolerance;

					// If true, then all logging is disabled.
					public boolean is_silent;

					// Use a bounds constrained optimization algorithm.
					public boolean is_constrained;

					// List of callbacks that are executed by the Minimizer at the end
					// of each iteration.
					//
					// The Options struct does not own these pointers.
					Vector<IterationCallback> callbacks;

					// Object responsible for evaluating the cost, residuals and
					// Jacobian matrix.
					// shared_ptr<Evaluator> evaluator;
					Evaluator evaluator;

					// Object responsible for actually computing the trust region
					// step, and sizing the trust region radius.
					// shared_ptr<TrustRegionStrategy> trust_region_strategy;
					TrustRegionStrategy trust_region_strategy;

					// Object holding the Jacobian matrix. It is assumed that the
					// sparsity structure of the matrix has already been initialized
					// and will remain constant for the life time of the
					// optimization.
					// shared_ptr<SparseMatrix> jacobian;
					SparseMatrix jacobian;

					// shared_ptr<CoordinateDescentMinimizer> inner_iteration_minimizer;
					CoordinateDescentMinimizer inner_iteration_minimizer;

					public MinimizerOptions() {
						Solver solver = new Solver();
						Init(solver.options);
					}

					public MinimizerOptions(SolverOptions options) {
						Init(options);
					}

					public void Init(SolverOptions options) {
						num_threads = options.num_threads;
						max_num_iterations = options.max_num_iterations;
						max_solver_time_in_seconds = options.max_solver_time_in_seconds;
						max_step_solver_retries = 5;
						gradient_tolerance = options.gradient_tolerance;
						parameter_tolerance = options.parameter_tolerance;
						function_tolerance = options.function_tolerance;
						min_relative_decrease = options.min_relative_decrease;
						eta = options.eta;
						jacobi_scaling = options.jacobi_scaling;
						use_nonmonotonic_steps = options.use_nonmonotonic_steps;
						max_consecutive_nonmonotonic_steps = options.max_consecutive_nonmonotonic_steps;
						trust_region_problem_dump_directory = options.trust_region_problem_dump_directory;
						trust_region_minimizer_iterations_to_dump = options.trust_region_minimizer_iterations_to_dump;
						trust_region_problem_dump_format_type = options.trust_region_problem_dump_format_type;
						max_num_consecutive_invalid_steps = options.max_num_consecutive_invalid_steps;
						min_trust_region_radius = options.min_trust_region_radius;
						line_search_direction_type = options.line_search_direction_type;
						line_search_type = options.line_search_type;
						nonlinear_conjugate_gradient_type = options.nonlinear_conjugate_gradient_type;
						max_lbfgs_rank = options.max_lbfgs_rank;
						use_approximate_eigenvalue_bfgs_scaling = options.use_approximate_eigenvalue_bfgs_scaling;
						line_search_interpolation_type = options.line_search_interpolation_type;
						min_line_search_step_size = options.min_line_search_step_size;
						line_search_sufficient_function_decrease = options.line_search_sufficient_function_decrease;
						max_line_search_step_contraction = options.max_line_search_step_contraction;
						min_line_search_step_contraction = options.min_line_search_step_contraction;
						max_num_line_search_step_size_iterations = options.max_num_line_search_step_size_iterations;
						max_num_line_search_direction_restarts = options.max_num_line_search_direction_restarts;
						line_search_sufficient_curvature_decrease = options.line_search_sufficient_curvature_decrease;
						max_line_search_step_expansion = options.max_line_search_step_expansion;
						inner_iteration_tolerance = options.inner_iteration_tolerance;
						is_silent = (options.logging_type == LoggingType.SILENT);
						is_constrained = false;
						callbacks = options.callbacks;
					}

				} // class MinimizerOptions
 

	// Interface for non-linear least squares solvers.
	class Minimizer {
		public MinimizerOptions options_;

		public Minimizer() {
			options_ = new MinimizerOptions();
		}

		public Minimizer(SolverOptions options) {
			options_ = new MinimizerOptions(options);
		}

		
		
		public boolean RunCallbacks(MinimizerOptions options,
                IterationSummary iteration_summary,
                SolverSummary summary) {
			boolean is_not_silent = !options.is_silent;
			CallbackReturnType status = CallbackReturnType.SOLVER_CONTINUE;
			int i = 0;
			while (status == CallbackReturnType.SOLVER_CONTINUE && i < options.callbacks.size()) {
			status = (options.callbacks.get(i)).operator(iteration_summary);
			++i;
			}
			switch (status) {
			case SOLVER_CONTINUE:
			return true;
			case SOLVER_TERMINATE_SUCCESSFULLY:
			summary.termination_type = TerminationType.USER_SUCCESS;
			summary.message[0] =
			"User callback returned SOLVER_TERMINATE_SUCCESSFULLY.";
			if ((1 <= MAX_LOG_LEVEL) && is_not_silent) {
			    Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);	
			}
			return false;
			case SOLVER_ABORT:
			summary.termination_type = TerminationType.USER_FAILURE;
			summary.message[0] = "User callback returned SOLVER_ABORT.";
			if ((1 <= MAX_LOG_LEVEL) && is_not_silent) {
			    Preferences.debug("Terminating: " + summary.message[0] + "\n", Preferences.DEBUG_ALGORITHM);	
			}
			return false;
			default:
			System.err.println("Unknown type of user callback status");
			}
			return false;
			}


	} // class Minimizer

	private ProblemImpl CreateGradientCheckingProblemImpl(ProblemImpl problem_impl, double relative_step_size,
			double relative_precision, GradientCheckingIterationCallback callback) {
		if (callback == null) {
			System.err.println("in CreateGradientCheckingProblemImpl callback is null");
			return null;
		}
		// We create new CostFunctions by wrapping the original CostFunction
		// in a gradient checking CostFunction. So its okay for the
		// ProblemImpl to take ownership of it and destroy it. The
		// LossFunctions and LocalParameterizations are reused and since
		// they are owned by problem_impl, gradient_checking_problem_impl
		// should not take ownership of it.
		ProblemImpl problem = new ProblemImpl();
		ProblemOptions gradient_checking_problem_options = problem.options_;
		gradient_checking_problem_options.cost_function_ownership = Ownership.TAKE_OWNERSHIP;
		gradient_checking_problem_options.loss_function_ownership = Ownership.DO_NOT_TAKE_OWNERSHIP;
		gradient_checking_problem_options.local_parameterization_ownership = Ownership.DO_NOT_TAKE_OWNERSHIP;
		gradient_checking_problem_options.context = problem_impl.context();

		NumericDiffOptions numeric_diff_options = new NumericDiffOptions();
		numeric_diff_options.relative_step_size = relative_step_size;

		ProblemImpl gradient_checking_problem_impl = new ProblemImpl(gradient_checking_problem_options);

		Program program = problem_impl.mutable_program();

		// For every ParameterBlock in problem_impl, create a new parameter
		// block with the same local parameterization and constancy.
		Vector<ParameterBlock> parameter_blocks = program.parameter_blocks();
		for (int i = 0; i < parameter_blocks.size(); ++i) {
			ParameterBlock parameter_block = parameter_blocks.get(i);
			gradient_checking_problem_impl.AddParameterBlock(parameter_block.mutable_user_state(),
					parameter_block.Size(), parameter_block.mutable_local_parameterization());

			if (parameter_block.IsConstant()) {
				gradient_checking_problem_impl.SetParameterBlockConstant(parameter_block.mutable_user_state());
			}
		}
		// For every ResidualBlock in problem_impl, create a new
		// ResidualBlock by wrapping its CostFunction inside a
		// GradientCheckingCostFunction.
		Vector<ResidualBlock> residual_blocks = program.residual_blocks();
		for (int i = 0; i < residual_blocks.size(); ++i) {
			ResidualBlock residual_block = residual_blocks.get(i);

			// Build a human readable string which identifies the
			// ResidualBlock. This is used by the GradientCheckingCostFunction
			// when logging debugging information.
			String extra_info = "Residual block id " + i; // + "; depends on parameters [";
			Vector<double[]> parameter_blocks2 = new Vector<double[]>();
			Vector<LocalParameterization> local_parameterizations = new Vector<LocalParameterization>();
			parameter_blocks2.ensureCapacity(residual_block.NumParameterBlocks());
			local_parameterizations.ensureCapacity(residual_block.NumParameterBlocks());
			for (int j = 0; j < residual_block.NumParameterBlocks(); ++j) {
				ParameterBlock parameter_block = residual_block.parameter_blocks()[j];
				parameter_blocks2.add(parameter_block.mutable_user_state());
				// StringAppendF(&extra_info, "%p", parameter_block.mutable_user_state());
				// extra_info += (j < residual_block->NumParameterBlocks() - 1) ? ", " : "]";
				local_parameterizations.add(problem_impl.GetParameterization(parameter_block.mutable_user_state()));
			}

			// Wrap the original CostFunction in a GradientCheckingCostFunction.
			CostFunction gradient_checking_cost_function = new GradientCheckingCostFunction(
					residual_block.cost_function(), local_parameterizations, numeric_diff_options, relative_precision,
					extra_info, callback);

			// The const_cast is necessary because
			// ProblemImpl::AddResidualBlock can potentially take ownership of
			// the LossFunction, but in this case we are guaranteed that this
			// will not be the case, so this const_cast is harmless.
			gradient_checking_problem_impl.AddResidualBlock(gradient_checking_cost_function,
					// const_cast<LossFunction*>(residual_block->loss_function()),
					residual_block.loss_function(), parameter_blocks2);
		}
		// Normally, when a problem is given to the solver, we guarantee
		// that the state pointers for each parameter block point to the
		// user provided data. Since we are creating this new problem from a
		// problem given to us at an arbitrary stage of the solve, we cannot
		// depend on this being the case, so we explicitly call
		// SetParameterBlockStatePtrsToUserStatePtrs to ensure that this is
		// the case.
		gradient_checking_problem_impl.mutable_program().SetParameterBlockStatePtrsToUserStatePtrs();

		return gradient_checking_problem_impl;
	}

	private void PreSolveSummarize(SolverOptions options, ProblemImpl problem, SolverSummary summary) {
		SummarizeGivenProgram(problem.program(), summary);
		OrderingToGroupSizes(options.linear_solver_ordering, summary.linear_solver_ordering_given);
		OrderingToGroupSizes(options.inner_iteration_ordering, summary.inner_iteration_ordering_given);
		summary.dense_linear_algebra_library_type = options.dense_linear_algebra_library_type;
		summary.dogleg_type = options.dogleg_type;
		summary.inner_iteration_time_in_seconds = 0.0;
		summary.num_line_search_steps = 0;
		summary.line_search_cost_evaluation_time_in_seconds = 0.0;
		summary.line_search_gradient_evaluation_time_in_seconds = 0.0;
		summary.line_search_polynomial_minimization_time_in_seconds = 0.0;
		summary.line_search_total_time_in_seconds = 0.0;
		summary.inner_iterations_given = options.use_inner_iterations;
		summary.line_search_direction_type = options.line_search_direction_type;
		summary.line_search_interpolation_type = options.line_search_interpolation_type;
		summary.line_search_type = options.line_search_type;
		summary.linear_solver_type_given = options.linear_solver_type;
		summary.max_lbfgs_rank = options.max_lbfgs_rank;
		summary.minimizer_type = options.minimizer_type;
		summary.nonlinear_conjugate_gradient_type = options.nonlinear_conjugate_gradient_type;
		summary.num_linear_solver_threads_given = options.num_threads;
		summary.num_threads_given = options.num_threads;
		summary.preconditioner_type_given = options.preconditioner_type;
		summary.sparse_linear_algebra_library_type = options.sparse_linear_algebra_library_type;
		summary.trust_region_strategy_type = options.trust_region_strategy_type;
		summary.visibility_clustering_type = options.visibility_clustering_type;
	}

	private void OrderingToGroupSizes(OrderedGroups<double[]> ordering, Vector<Integer> group_sizes) {
		if (group_sizes == null) {
			System.err.println("Vector<Integer> group_sizes is null in OrderingToGroupSizes");
			return;
		}
		group_sizes.clear();
		if (ordering == null) {
			return;
		}

		HashMap<Integer, Set<double[]>> group_to_elements = ordering.group_to_elements();
		Set<Integer> keySet = group_to_elements.keySet();
		Iterator<Integer> iter = keySet.iterator();
		while (iter.hasNext()) {
			Set<double[]> values = (Set<double[]>) group_to_elements.get(iter.next());
			if (values == null) {
				group_sizes.add(0);
			} else {
				group_sizes.add(values.size());
			}
		}
	}

	private void SummarizeGivenProgram(Program program, SolverSummary summary) {
		summary.num_parameter_blocks = program.NumParameterBlocks();
		summary.num_parameters = program.NumParameters();
		summary.num_effective_parameters = program.NumEffectiveParameters();
		summary.num_residual_blocks = program.NumResidualBlocks();
		summary.num_residuals = program.NumResiduals();
	}

	class CostFunction {
		private int num_residuals_;
		private Vector<Integer> parameter_block_sizes_;

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

		protected void set_parameter_block_sizes(Vector<Integer> parameter_block_sizes) {
			parameter_block_sizes_ = parameter_block_sizes;
		}

		protected void AddParameterBlock(int size) {
			parameter_block_sizes_.add(size);
		}

		protected void SetNumResiduals(int num_residuals) {
			num_residuals_ = num_residuals;
		}

		// Overriden by something like class QuadraticCostFunction extends SizedCostFunction
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			return true;
		}
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			return true;
		}
		
		public void finalize() {
			
		}

	}

	class SizedCostFunction extends CostFunction {

		public SizedCostFunction(int kNumResiduals, int N0, int N1, int N2, int N3, int N4, int N5, int N6, int N7,
				int N8, int N9, int num_residuals) {
			super();
			if (kNumResiduals != DYNAMIC) {
				System.err.println("Cost functions must have at least one residual block.");
				return;
			}

			if (!(((N1 == 0) && (N2 == 0) && (N3 == 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0)
					&& (N9 == 0))
					|| ((N1 > 0) && (N2 == 0) && (N3 == 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0)
							&& (N8 == 0) && (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 == 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0)
							&& (N8 == 0) && (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0)
							&& (N8 == 0) && (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0)
							&& (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 == 0) && (N7 == 0) && (N8 == 0)
							&& (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 == 0) && (N8 == 0)
							&& (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 > 0) && (N8 == 0)
							&& (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 > 0) && (N8 > 0)
							&& (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 > 0) && (N8 > 0)
							&& (N9 > 0)))) {
				System.err.println("Zero block cannot precede a non-zero block. Block sizes are ");
				System.err.println("(ignore trailing 0s): " + N0 + ", " + N1 + ", " + N2 + ", ");
				System.err.println(N3 + ", " + N4 + ", " + N5 + ", " + N6 + ", " + N7 + ", ");
				System.err.println(N8 + ", " + N9);
				return;
			}

			set_num_residuals(num_residuals);

			if (N0 > 0)
				mutable_parameter_block_sizes().add(N0);
			if (N1 > 0)
				mutable_parameter_block_sizes().add(N1);
			if (N2 > 0)
				mutable_parameter_block_sizes().add(N2);
			if (N3 > 0)
				mutable_parameter_block_sizes().add(N3);
			if (N4 > 0)
				mutable_parameter_block_sizes().add(N4);
			if (N5 > 0)
				mutable_parameter_block_sizes().add(N5);
			if (N6 > 0)
				mutable_parameter_block_sizes().add(N6);
			if (N7 > 0)
				mutable_parameter_block_sizes().add(N7);
			if (N8 > 0)
				mutable_parameter_block_sizes().add(N8);
			if (N9 > 0)
				mutable_parameter_block_sizes().add(N9);
		}

		public SizedCostFunction(int kNumResiduals, int N0, int N1, int N2, int N3, int N4, int N5, int N6, int N7,
				int N8, int N9) {
			super();
			if (kNumResiduals <= 0) {
				System.err.println("Cost functions must have at least one residual block.");
				return;
			}

			if (!(((N1 == 0) && (N2 == 0) && (N3 == 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0)
					&& (N9 == 0))
					|| ((N1 > 0) && (N2 == 0) && (N3 == 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0)
							&& (N8 == 0) && (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 == 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0)
							&& (N8 == 0) && (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 == 0) && (N5 == 0) && (N6 == 0) && (N7 == 0)
							&& (N8 == 0) && (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 == 0) && (N6 == 0) && (N7 == 0) && (N8 == 0)
							&& (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 == 0) && (N7 == 0) && (N8 == 0)
							&& (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 == 0) && (N8 == 0)
							&& (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 > 0) && (N8 == 0)
							&& (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 > 0) && (N8 > 0)
							&& (N9 == 0))
					|| ((N1 > 0) && (N2 > 0) && (N3 > 0) && (N4 > 0) && (N5 > 0) && (N6 > 0) && (N7 > 0) && (N8 > 0)
							&& (N9 > 0)))) {
				System.err.println("Zero block cannot precede a non-zero block. Block sizes are ");
				System.err.println("(ignore trailing 0s): " + N0 + ", " + N1 + ", " + N2 + ", ");
				System.err.println(N3 + ", " + N4 + ", " + N5 + ", " + N6 + ", " + N7 + ", ");
				System.err.println(N8 + ", " + N9);
			}

			set_num_residuals(kNumResiduals);

			if (N0 > 0)
				mutable_parameter_block_sizes().add(N0);
			if (N1 > 0)
				mutable_parameter_block_sizes().add(N1);
			if (N2 > 0)
				mutable_parameter_block_sizes().add(N2);
			if (N3 > 0)
				mutable_parameter_block_sizes().add(N3);
			if (N4 > 0)
				mutable_parameter_block_sizes().add(N4);
			if (N5 > 0)
				mutable_parameter_block_sizes().add(N5);
			if (N6 > 0)
				mutable_parameter_block_sizes().add(N6);
			if (N7 > 0)
				mutable_parameter_block_sizes().add(N7);
			if (N8 > 0)
				mutable_parameter_block_sizes().add(N8);
			if (N9 > 0)
				mutable_parameter_block_sizes().add(N9);
		}
		
		public void finalize() {
			
		}
	}
	
	class NumericDiffCostFunction<CostFunctor> extends SizedCostFunction {
		private CostFunctor functor_;
		private NumericDiffMethodType method;
		private Ownership ownership_;
		private NumericDiffOptions options_;
		private int kNumResiduals;
		private int num_residuals;
		private int N0;
		private int N1;
		private int N2;
		private int N3;
		private int N4;
		private int N5;
		private int N6;
		private int N7;
		private int N8;
		private int N9;
		
		// Takes ownership of functor. Uses the template-provided value for the
		// number of residuals ("kNumResiduals").
		// NumericDiffMethodType method = CENTRAL
		// Ownership ownership = TAKE_OWNERSHIP
		// NumericDiffOptions& options = NumericDiffOptions()
		public NumericDiffCostFunction(CostFunctor functor, NumericDiffMethodType method, Ownership ownership,
				NumericDiffOptions options, int kNumResiduals, 
				int N0, int N1, int N2, int N3, int N4,
				int N5, int N6, int N7, int N8, int N9) {
			super(kNumResiduals, N0, N1, N2, N3, N4, N5, N6, N7, N8, N9);
			functor_ = functor;
			this.method = method;
			ownership_ = ownership;
			options_ = options;
			this.kNumResiduals = kNumResiduals;
			if (kNumResiduals == DYNAMIC) {
				System.err
						.println("Can't run the fixed-size constructor if the number of residuals is set to DYNAMIC.");
			}
			this.N0 = N0;
			this.N1 = N1;
			this.N2 = N2;
			this.N3 = N3;
			this.N4 = N4;
			this.N5 = N5;
			this.N6 = N6;
			this.N7 = N7;
			this.N8 = N8;
			this.N9 = N9;
		}
		
		// Takes ownership of functor. Ignores the template-provided
		// kNumResiduals in favor of the "num_residuals" argument provided.
		//
		// This allows for having autodiff cost functions which return varying
		// numbers of residuals at runtime.
		public NumericDiffCostFunction(CostFunctor functor,  NumericDiffMethodType method, Ownership ownership,
				NumericDiffOptions options, int kNumResiduals, int N0, int N1, int N2, int N3, int N4,
				int N5, int N6, int N7, int N8, int N9, int num_residuals) {
			super(kNumResiduals, N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, num_residuals);
			functor_ = functor;
			this.method = method;
			ownership_ = ownership;
			options_ = options;
			this.kNumResiduals = kNumResiduals;
			this.num_residuals = num_residuals;
			if (kNumResiduals != DYNAMIC) {
				System.err.println("Can't run the dynamic-size constructor if the number of residuals is not DYNAMIC.");
				return;
			}
			this.N0 = N0;
			this.N1 = N1;
			this.N2 = N2;
			this.N3 = N3;
			this.N4 = N4;
			this.N5 = N5;
			this.N6 = N6;
			this.N7 = N7;
			this.N8 = N8;
			this.N9 = N9;

		}
		
		// There is no Java equivalent to release() because Java allocates objects from a managed heap,
		// C++ allocates them in unmanaged memory
		//public void finalize() {
			//if (ownership_ != Ownership.TAKE_OWNERSHIP) {
			    //functor_.release();
			//}	
		//}
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			int kNumParameters = N0 + N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9;	
			int kNumParameterBlocks = 0;
			if (N0 > 0) {
				kNumParameterBlocks++;
			}
			if (N1 > 0) {
				kNumParameterBlocks++;
			}
			if (N2 > 0) {
				kNumParameterBlocks++;
			}
			if (N3 > 0) {
				kNumParameterBlocks++;
			}
			if (N4 > 0) {
				kNumParameterBlocks++;
			}
			if (N5 > 0) {
				kNumParameterBlocks++;
			}
			if (N6 > 0) {
				kNumParameterBlocks++;
			}
			if (N7 > 0) {
				kNumParameterBlocks++;
			}
			if (N8 > 0) {
				kNumParameterBlocks++;
			}
			if (N9 > 0) {
				kNumParameterBlocks++;
			}
			
			// Get the function value (residuals) at the the point to evaluate.
			double x[];
			switch (testCase) {
			case COST_FUNCTOR_EXAMPLE:
				x = parameters.get(0);
			    if (!((CostFunctorExample) functor_).operator(x, residuals)) {
			    	return false;
			    }
			    break;
			case CURVE_FITTING_EXAMPLE:
				x = parameters.get(0);
			    if (!((CurveFittingFunctorExample) functor_).operator(x, residuals)) {
			    	return false;
			    }
			    break;
		    } // switch(testCase)
			
			if (jacobians == null) {
			      return true;
			}
			
			// Create a copy of the parameters which will get mutated.
		    double parameters_reference_copy[][] = new double[kNumParameterBlocks][];
		    if (N0 > 0) {
		    	parameters_reference_copy[0] = new double[N0];
		    	for (i = 0; i < N0; i++) {
		    		parameters_reference_copy[0][i] = parameters.get(0)[i]; 
		    	}
		    }
		    if (N1 > 0) {
		    	parameters_reference_copy[1] = new double[N1];
		    	for (i = 0; i < N1; i++) {
		    		parameters_reference_copy[1][i] = parameters.get(1)[i]; 
		    	}
		    }
		    if (N2 > 0) {
		    	parameters_reference_copy[2] = new double[N2];
		    	for (i = 0; i < N2; i++) {
		    		parameters_reference_copy[2][i] = parameters.get(2)[i]; 
		    	}
		    }
		    if (N3 > 0) {
		    	parameters_reference_copy[3] = new double[N3];
		    	for (i = 0; i < N3; i++) {
		    		parameters_reference_copy[3][i] = parameters.get(3)[i]; 
		    	}
		    }
		    if (N4 > 0) {
		    	parameters_reference_copy[4] = new double[N4];
		    	for (i = 0; i < N4; i++) {
		    		parameters_reference_copy[4][i] = parameters.get(4)[i]; 
		    	}
		    }
		    if (N5 > 0) {
		    	parameters_reference_copy[5] = new double[N5];
		    	for (i = 0; i < N5; i++) {
		    		parameters_reference_copy[5][i] = parameters.get(5)[i]; 
		    	}
		    }
		    if (N6 > 0) {
		    	parameters_reference_copy[6] = new double[N6];
		    	for (i = 0; i < N6; i++) {
		    		parameters_reference_copy[6][i] = parameters.get(6)[i]; 
		    	}
		    }
		    if (N7 > 0) {
		    	parameters_reference_copy[7] = new double[N7];
		    	for (i = 0; i < N7; i++) {
		    		parameters_reference_copy[7][i] = parameters.get(7)[i]; 
		    	}
		    }
		    if (N8 > 0) {
		    	parameters_reference_copy[8] = new double[N8];
		    	for (i = 0; i < N8; i++) {
		    		parameters_reference_copy[8][i] = parameters.get(8)[i]; 
		    	}
		    }
		    if (N9 > 0) {
		    	parameters_reference_copy[9] = new double[N9];
		    	for (i = 0; i < N9; i++) {
		    		parameters_reference_copy[9][i] = parameters.get(9)[i]; 
		    	}
		    }
		    int new_num_residuals = new SizedCostFunction(kNumResiduals,              
                    N0, N1, N2, N3, N4,                          
                    N5, N6, N7, N8, N9).num_residuals();  
		    if ((N0 > 0) && (jacobians[0] != null)) { 
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         0,                                           
		 		                         N0,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,        
		                             0,                                       
		                             N0,                                  
		                             parameters_reference_copy,             
		                             jacobians[0])) {                         
		          return false;                                                   
		        }
		      }
		    
		    if ((N1 > 0) && (jacobians[1] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         1,                                           
		 		                         N1,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,              
		                             1,                                       
		                             N1,                                  
		                             parameters_reference_copy,             
		                             jacobians[1])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N2 > 0) && (jacobians[2] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         2,                                           
		 		                         N2,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,              
		                             2,                                       
		                             N2,                                  
		                             parameters_reference_copy,             
		                             jacobians[2])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N3 > 0) && (jacobians[3] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         3,                                           
		 		                         N3,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,              
		                             3,                                       
		                             N3,                                  
		                             parameters_reference_copy,             
		                             jacobians[3])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N4> 0) && (jacobians[4] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         4,                                           
		 		                         N4,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,              
		                             4,                                       
		                             N4,                                  
		                             parameters_reference_copy,             
		                             jacobians[4])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N5 > 0) && (jacobians[5] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         5,                                           
		 		                         N5,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,               
		                             5,                                       
		                             N5,                                  
		                             parameters_reference_copy,             
		                             jacobians[5])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N6 > 0) && (jacobians[6] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         6,                                           
		 		                         N6,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,               
		                             6,                                       
		                             N6,                                  
		                             parameters_reference_copy,             
		                             jacobians[6])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N7 > 0) && (jacobians[7] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         7,                                           
		 		                         N7,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,               
		                             7,                                       
		                             N7,                                  
		                             parameters_reference_copy,             
		                             jacobians[7])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N8 > 0) && (jacobians[8] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         8,                                           
		 		                         N8,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,              
		                             8,                                       
		                             N8,                                  
		                             parameters_reference_copy,             
		                             jacobians[8])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N9 > 0) && (jacobians[9] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         9,                                           
		 		                         N9,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,              
		                             9,                                       
		                             N9,                                  
		                             parameters_reference_copy,             
		                             jacobians[9])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    if (N0 > 0) {
		    	for (i = 0; i < N0; i++) {
		    		parameters.get(0)[i] = parameters_reference_copy[0][i]; 
		    	}
		    }
		    if (N1 > 0) {
		    	for (i = 0; i < N1; i++) {
		    		parameters.get(1)[i] = parameters_reference_copy[1][i]; 
		    	}
		    }
		    if (N2 > 0) {
		    	for (i = 0; i < N2; i++) {
		    		parameters.get(2)[i] = parameters_reference_copy[2][i]; 
		    	}
		    }
		    if (N3 > 0) {
		    	for (i = 0; i < N3; i++) {
		    		parameters.get(3)[i] = parameters_reference_copy[3][i]; 
		    	}
		    }
		    if (N4 > 0) {
		    	for (i = 0; i < N4; i++) {
		    		parameters.get(4)[i] = parameters_reference_copy[4][i]; 
		    	}
		    }
		    if (N5 > 0) {
		    	for (i = 0; i < N5; i++) {
		    		parameters.get(5)[i] = parameters_reference_copy[5][i];  
		    	}
		    }
		    if (N6 > 0) {
		    	for (i = 0; i < N6; i++) {
		    		parameters.get(6)[i] = parameters_reference_copy[6][i]; 
		    	}
		    }
		    if (N7 > 0) {
		    	for (i = 0; i < N7; i++) {
		    		parameters.get(7)[i] = parameters_reference_copy[7][i]; 
		    	}
		    }
		    if (N8 > 0) {
		    	for (i = 0; i < N8; i++) {
		    		parameters.get(8)[i] = parameters_reference_copy[8][i]; 
		    	}
		    }
		    if (N9 > 0) {
		    	for (i = 0; i < N9; i++) {
		    		parameters.get(9)[i] = parameters_reference_copy[9][i]; 
		    	}
		    }
			return true;
		} // public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][])
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			int kNumParameters = N0 + N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9;	
			int kNumParameterBlocks = 0;
			if (N0 > 0) {
				kNumParameterBlocks++;
			}
			if (N1 > 0) {
				kNumParameterBlocks++;
			}
			if (N2 > 0) {
				kNumParameterBlocks++;
			}
			if (N3 > 0) {
				kNumParameterBlocks++;
			}
			if (N4 > 0) {
				kNumParameterBlocks++;
			}
			if (N5 > 0) {
				kNumParameterBlocks++;
			}
			if (N6 > 0) {
				kNumParameterBlocks++;
			}
			if (N7 > 0) {
				kNumParameterBlocks++;
			}
			if (N8 > 0) {
				kNumParameterBlocks++;
			}
			if (N9 > 0) {
				kNumParameterBlocks++;
			}
			
			// Get the function value (residuals) at the the point to evaluate.
			double x[];
			switch (testCase) {
			case COST_FUNCTOR_EXAMPLE:
				x = parameters.get(0);
			    if (!((CostFunctorExample) functor_).operator(x, residuals)) {
			    	return false;
			    }
			    break;
			case CURVE_FITTING_EXAMPLE:
				x = parameters.get(0);
			    if (!((CurveFittingFunctorExample) functor_).operator(x, residuals)) {
			    	return false;
			    }
			    break;
		    } // switch(testCase)
			
			if (jacobians == null) {
			      return true;
			}
			
			// Create a copy of the parameters which will get mutated.
		    double parameters_reference_copy[][] = new double[kNumParameterBlocks][];
		    if (N0 > 0) {
		    	parameters_reference_copy[0] = new double[N0];
		    	for (i = 0; i < N0; i++) {
		    		parameters_reference_copy[0][i] = parameters.get(0)[i]; 
		    	}
		    }
		    if (N1 > 0) {
		    	parameters_reference_copy[1] = new double[N1];
		    	for (i = 0; i < N1; i++) {
		    		parameters_reference_copy[1][i] = parameters.get(1)[i]; 
		    	}
		    }
		    if (N2 > 0) {
		    	parameters_reference_copy[2] = new double[N2];
		    	for (i = 0; i < N2; i++) {
		    		parameters_reference_copy[2][i] = parameters.get(2)[i]; 
		    	}
		    }
		    if (N3 > 0) {
		    	parameters_reference_copy[3] = new double[N3];
		    	for (i = 0; i < N3; i++) {
		    		parameters_reference_copy[3][i] = parameters.get(3)[i]; 
		    	}
		    }
		    if (N4 > 0) {
		    	parameters_reference_copy[4] = new double[N4];
		    	for (i = 0; i < N4; i++) {
		    		parameters_reference_copy[4][i] = parameters.get(4)[i]; 
		    	}
		    }
		    if (N5 > 0) {
		    	parameters_reference_copy[5] = new double[N5];
		    	for (i = 0; i < N5; i++) {
		    		parameters_reference_copy[5][i] = parameters.get(5)[i]; 
		    	}
		    }
		    if (N6 > 0) {
		    	parameters_reference_copy[6] = new double[N6];
		    	for (i = 0; i < N6; i++) {
		    		parameters_reference_copy[6][i] = parameters.get(6)[i]; 
		    	}
		    }
		    if (N7 > 0) {
		    	parameters_reference_copy[7] = new double[N7];
		    	for (i = 0; i < N7; i++) {
		    		parameters_reference_copy[7][i] = parameters.get(7)[i]; 
		    	}
		    }
		    if (N8 > 0) {
		    	parameters_reference_copy[8] = new double[N8];
		    	for (i = 0; i < N8; i++) {
		    		parameters_reference_copy[8][i] = parameters.get(8)[i]; 
		    	}
		    }
		    if (N9 > 0) {
		    	parameters_reference_copy[9] = new double[N9];
		    	for (i = 0; i < N9; i++) {
		    		parameters_reference_copy[9][i] = parameters.get(9)[i]; 
		    	}
		    }
		    int new_num_residuals = new SizedCostFunction(kNumResiduals,              
                    N0, N1, N2, N3, N4,                          
                    N5, N6, N7, N8, N9).num_residuals();  
		    if ((N0 > 0) && (jacobians[0] != null)) { 
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         0,                                           
		 		                         N0,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,        
		                             0,                                       
		                             N0,                                  
		                             parameters_reference_copy,             
		                             jacobians[0], jacobians_offset[0])) {                         
		          return false;                                                   
		        }
		      }
		    
		    if ((N1 > 0) && (jacobians[1] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         1,                                           
		 		                         N1,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,              
		                             1,                                       
		                             N1,                                  
		                             parameters_reference_copy,             
		                             jacobians[1], jacobians_offset[1])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N2 > 0) && (jacobians[2] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         2,                                           
		 		                         N2,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,              
		                             2,                                       
		                             N2,                                  
		                             parameters_reference_copy,             
		                             jacobians[2], jacobians_offset[2])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N3 > 0) && (jacobians[3] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         3,                                           
		 		                         N3,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,              
		                             3,                                       
		                             N3,                                  
		                             parameters_reference_copy,             
		                             jacobians[3], jacobians_offset[3])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N4> 0) && (jacobians[4] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         4,                                           
		 		                         N4,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,              
		                             4,                                       
		                             N4,                                  
		                             parameters_reference_copy,             
		                             jacobians[4], jacobians_offset[4])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N5 > 0) && (jacobians[5] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         5,                                           
		 		                         N5,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,               
		                             5,                                       
		                             N5,                                  
		                             parameters_reference_copy,             
		                             jacobians[5], jacobians_offset[5])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N6 > 0) && (jacobians[6] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         6,                                           
		 		                         N6,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,               
		                             6,                                       
		                             N6,                                  
		                             parameters_reference_copy,             
		                             jacobians[6], jacobians_offset[6])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N7 > 0) && (jacobians[7] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         7,                                           
		 		                         N7,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,               
		                             7,                                       
		                             N7,                                  
		                             parameters_reference_copy,             
		                             jacobians[7], jacobians_offset[7])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N8 > 0) && (jacobians[8] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         8,                                           
		 		                         N8,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,              
		                             8,                                       
		                             N8,                                  
		                             parameters_reference_copy,             
		                             jacobians[8], jacobians_offset[8])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    
		    if ((N9 > 0) && (jacobians[9] != null)) {                       
		        if (!EvaluateJacobianForParameterBlock(
		                        		  method,                                          
		 		                         kNumResiduals,                                   
		 		                         N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,          
		 		                         9,                                           
		 		                         N9,
		                             functor_,                              
		                             residuals,                                   
		                             options_,                                    
		                             new_num_residuals,              
		                             9,                                       
		                             N9,                                  
		                             parameters_reference_copy,             
		                             jacobians[9], jacobians_offset[9])) {                         
		          return false;                                                   
		        }                                                                 
		      }
		    if (N0 > 0) {
		    	for (i = 0; i < N0; i++) {
		    		parameters.get(0)[i] = parameters_reference_copy[0][i]; 
		    	}
		    }
		    if (N1 > 0) {
		    	for (i = 0; i < N1; i++) {
		    		parameters.get(1)[i] = parameters_reference_copy[1][i]; 
		    	}
		    }
		    if (N2 > 0) {
		    	for (i = 0; i < N2; i++) {
		    		parameters.get(2)[i] = parameters_reference_copy[2][i]; 
		    	}
		    }
		    if (N3 > 0) {
		    	for (i = 0; i < N3; i++) {
		    		parameters.get(3)[i] = parameters_reference_copy[3][i]; 
		    	}
		    }
		    if (N4 > 0) {
		    	for (i = 0; i < N4; i++) {
		    		parameters.get(4)[i] = parameters_reference_copy[4][i]; 
		    	}
		    }
		    if (N5 > 0) {
		    	for (i = 0; i < N5; i++) {
		    		parameters.get(5)[i] = parameters_reference_copy[5][i];  
		    	}
		    }
		    if (N6 > 0) {
		    	for (i = 0; i < N6; i++) {
		    		parameters.get(6)[i] = parameters_reference_copy[6][i]; 
		    	}
		    }
		    if (N7 > 0) {
		    	for (i = 0; i < N7; i++) {
		    		parameters.get(7)[i] = parameters_reference_copy[7][i]; 
		    	}
		    }
		    if (N8 > 0) {
		    	for (i = 0; i < N8; i++) {
		    		parameters.get(8)[i] = parameters_reference_copy[8][i]; 
		    	}
		    }
		    if (N9 > 0) {
		    	for (i = 0; i < N9; i++) {
		    		parameters.get(9)[i] = parameters_reference_copy[9][i]; 
		    	}
		    }
			return true;
		} // public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int[] jacobians_offset)
		
		
	} // class NumericDiffCostFunction
	
	public <CostFunctor> boolean EvaluateJacobianForParameterBlock(
			NumericDiffMethodType kMethod,
	          int kNumResiduals,
	          int N0, int N1, int N2, int N3, int N4,
	          int N5, int N6, int N7, int N8, int N9,
	          int kParameterBlock,
	          int kParameterBlockSize,
		      CostFunctor functor,
		      double[] residuals_at_eval_point,
		      NumericDiffOptions options,
		      int num_residuals,
		      int parameter_block_index,
		      int parameter_block_size,
		      double [][]parameters,
		      double []jacobian) {
		      int i;
		      int r; 
		      int c;
		   final int num_residuals_internal =
		        (kNumResiduals != DYNAMIC ? kNumResiduals : num_residuals);
		   final int parameter_block_index_internal =
		        (kParameterBlock != DYNAMIC ? kParameterBlock :
		                                             parameter_block_index);
		   final int parameter_block_size_internal =
		        (kParameterBlockSize != DYNAMIC ? kParameterBlockSize :
		                                                 parameter_block_size);
		   
		   double ResidualVector[] = new double[kNumResiduals];
		   double ParameterVector[] = new double[kParameterBlockSize];
		   //Map<JacobianMatrix> parameter_jacobian(jacobian,
                   //num_residuals_internal,
                   //parameter_block_size_internal);

			//Map<ParameterVector> x_plus_delta(
			//parameters[parameter_block_index_internal],
			//parameter_block_size_internal);
		   double parameter_jacobian[][] = new double[kNumResiduals][kParameterBlockSize];
		   for (r = 0; r < num_residuals_internal; r++) {
			   for (c = 0; c < parameter_block_size_internal; c++) {
			       parameter_jacobian[r][c] = jacobian[r*parameter_block_size_internal + c];   
			   }
		   }
		   double x_plus_delta[] = new double[kParameterBlockSize];
		   for (i = 0; i < parameter_block_size_internal; i++) {
			   x_plus_delta[i] = parameters[parameter_block_index_internal][i];   
		   }
		   double x[] = new double[kParameterBlockSize];
		   for (i = 0; i < parameter_block_size_internal; i++) {
			   x[i] = x_plus_delta[i];   
		   }
		   double step_size[] = new double[kParameterBlockSize];
		   if (kMethod == NumericDiffMethodType.RIDDERS) {
		       for (i = 0; i < parameter_block_size_internal; i++) {
		    	   step_size[i] = Math.abs(x[i]) * options.ridders_relative_initial_step_size;
		       }
		   }
		   else {
			   for (i = 0; i < parameter_block_size_internal; i++) {
		    	   step_size[i] = Math.abs(x[i]) * options.relative_step_size;
		       }   
		   }
		   
		// It is not a good idea to make the step size arbitrarily
		// small. This will lead to problems with round off and numerical
		// instability when dividing by the step size. The general
		// recommendation is to not go down below sqrt(epsilon).
		double min_step_size = Math.sqrt(epsilon);
		
		// For Ridders' method, the initial step size is required to be large,
	    // thus ridders_relative_initial_step_size is used.
	    if (kMethod == NumericDiffMethodType.RIDDERS) {
	      min_step_size = Math.max(min_step_size,
	                               options.ridders_relative_initial_step_size);
	    }
	    

	    // For each parameter in the parameter block, use finite differences to
	    // compute the derivative for that parameter.
	    double temp_residual_array[] = new double[num_residuals_internal];
	    double residual_array[] = new double[num_residuals_internal];
	    //Map<ResidualVector> residuals(residual_array.get(),
        //        num_residuals_internal);
	    double residuals[] = new double[kNumResiduals];
	    for (int j = 0; j < parameter_block_size_internal; ++j) {
	        double delta = Math.max(min_step_size, step_size[j]);

	        if (kMethod == NumericDiffMethodType.RIDDERS) {
	          if (!EvaluateRiddersJacobianColumn(kMethod, kNumResiduals,
	        		                             kParameterBlockSize,functor, j, delta,
	                                             options,
	                                             num_residuals_internal,
	                                             parameter_block_index_internal,
	                                             parameter_block_size_internal,
	                                             x,
	                                             residuals_at_eval_point,
	                                             parameters,
	                                             x_plus_delta,
	                                             temp_residual_array,
	                                             residual_array)) {
	            return false;
	          }
	        } else {
	          if (!EvaluateJacobianColumn(kMethod,kNumResiduals,
	        		                      kParameterBlockSize,
	        		                      functor, j, delta,
	                                      num_residuals_internal,
	                                      parameter_block_index_internal,
	                                      parameter_block_size_internal,
	                                      x,
	                                      residuals_at_eval_point,
	                                      parameters,
	                                      x_plus_delta,
	                                      temp_residual_array,
	                                      residual_array)) {
	            return false;
	          }
	        }

	        for (i = 0; i < num_residuals_internal; i++) {
	        	residuals[i] = residual_array[i];
	        }
	        for (i = 0; i < num_residuals_internal; i++) {
	        	parameter_jacobian[i][j] = residuals[i];
	        }
	        for (r = 0; r < num_residuals_internal; r++) {
		        jacobian[r*parameter_block_size_internal + j] = parameter_jacobian[r][j];   
		    }
	        for (i = 0; i < parameter_block_size_internal; i++) {
				   parameters[parameter_block_index_internal][i] = x_plus_delta[i];   
			   }
	      }
		return true;
	}
	
	public <CostFunctor> boolean EvaluateJacobianForParameterBlock(
			NumericDiffMethodType kMethod,
	          int kNumResiduals,
	          int N0, int N1, int N2, int N3, int N4,
	          int N5, int N6, int N7, int N8, int N9,
	          int kParameterBlock,
	          int kParameterBlockSize,
		      CostFunctor functor,
		      double[] residuals_at_eval_point,
		      NumericDiffOptions options,
		      int num_residuals,
		      int parameter_block_index,
		      int parameter_block_size,
		      double [][]parameters,
		      double []jacobian,
		      int jacobian_offset) {
		      int i;
		      int r; 
		      int c;
		   final int num_residuals_internal =
		        (kNumResiduals != DYNAMIC ? kNumResiduals : num_residuals);
		   final int parameter_block_index_internal =
		        (kParameterBlock != DYNAMIC ? kParameterBlock :
		                                             parameter_block_index);
		   final int parameter_block_size_internal =
		        (kParameterBlockSize != DYNAMIC ? kParameterBlockSize :
		                                                 parameter_block_size);
		   
		   double ResidualVector[] = new double[kNumResiduals];
		   double ParameterVector[] = new double[kParameterBlockSize];
		   //Map<JacobianMatrix> parameter_jacobian(jacobian,
                   //num_residuals_internal,
                   //parameter_block_size_internal);

			//Map<ParameterVector> x_plus_delta(
			//parameters[parameter_block_index_internal],
			//parameter_block_size_internal);
		   double parameter_jacobian[][] = new double[kNumResiduals][kParameterBlockSize];
		   for (r = 0; r < num_residuals_internal; r++) {
			   for (c = 0; c < parameter_block_size_internal; c++) {
			       parameter_jacobian[r][c] = jacobian[jacobian_offset + r*parameter_block_size_internal + c];   
			   }
		   }
		   double x_plus_delta[] = new double[kParameterBlockSize];
		   for (i = 0; i < parameter_block_size_internal; i++) {
			   x_plus_delta[i] = parameters[parameter_block_index_internal][i];   
		   }
		   double x[] = new double[kParameterBlockSize];
		   for (i = 0; i < parameter_block_size_internal; i++) {
			   x[i] = x_plus_delta[i];   
		   }
		   double step_size[] = new double[kParameterBlockSize];
		   if (kMethod == NumericDiffMethodType.RIDDERS) {
		       for (i = 0; i < parameter_block_size_internal; i++) {
		    	   step_size[i] = Math.abs(x[i]) * options.ridders_relative_initial_step_size;
		       }
		   }
		   else {
			   for (i = 0; i < parameter_block_size_internal; i++) {
		    	   step_size[i] = Math.abs(x[i]) * options.relative_step_size;
		       }   
		   }
		   
		// It is not a good idea to make the step size arbitrarily
		// small. This will lead to problems with round off and numerical
		// instability when dividing by the step size. The general
		// recommendation is to not go down below sqrt(epsilon).
		double min_step_size = Math.sqrt(epsilon);
		
		// For Ridders' method, the initial step size is required to be large,
	    // thus ridders_relative_initial_step_size is used.
	    if (kMethod == NumericDiffMethodType.RIDDERS) {
	      min_step_size = Math.max(min_step_size,
	                               options.ridders_relative_initial_step_size);
	    }
	    

	    // For each parameter in the parameter block, use finite differences to
	    // compute the derivative for that parameter.
	    double temp_residual_array[] = new double[num_residuals_internal];
	    double residual_array[] = new double[num_residuals_internal];
	    //Map<ResidualVector> residuals(residual_array.get(),
        //        num_residuals_internal);
	    double residuals[] = new double[kNumResiduals];
	    for (int j = 0; j < parameter_block_size_internal; ++j) {
	        double delta = Math.max(min_step_size, step_size[j]);

	        if (kMethod == NumericDiffMethodType.RIDDERS) {
	          if (!EvaluateRiddersJacobianColumn(kMethod, kNumResiduals,
	        		                             kParameterBlockSize,functor, j, delta,
	                                             options,
	                                             num_residuals_internal,
	                                             parameter_block_index_internal,
	                                             parameter_block_size_internal,
	                                             x,
	                                             residuals_at_eval_point,
	                                             parameters,
	                                             x_plus_delta,
	                                             temp_residual_array,
	                                             residual_array)) {
	            return false;
	          }
	        } else {
	          if (!EvaluateJacobianColumn(kMethod,kNumResiduals,
	        		                      kParameterBlockSize,
	        		                      functor, j, delta,
	                                      num_residuals_internal,
	                                      parameter_block_index_internal,
	                                      parameter_block_size_internal,
	                                      x,
	                                      residuals_at_eval_point,
	                                      parameters,
	                                      x_plus_delta,
	                                      temp_residual_array,
	                                      residual_array)) {
	            return false;
	          }
	        }

	        for (i = 0; i < num_residuals_internal; i++) {
	        	residuals[i] = residual_array[i];
	        }
	        for (i = 0; i < num_residuals_internal; i++) {
	        	parameter_jacobian[i][j] = residuals[i];
	        }
	        for (r = 0; r < num_residuals_internal; r++) {
		        jacobian[r*parameter_block_size_internal + j] = parameter_jacobian[r][j];   
		    }
	        for (i = 0; i < parameter_block_size_internal; i++) {
				   parameters[parameter_block_index_internal][i] = x_plus_delta[i];   
			   }
	      }
		return true;
	}
	
	public <CostFunctor> boolean EvaluateJacobianColumn(NumericDiffMethodType kMethod, int kNumResiduals, int kParameterBlockSize,
			CostFunctor functor,
            int parameter_index,
            double delta,
            int num_residuals,
            int parameter_block_index,
            int parameter_block_size,
            double[] x_ptr,
            double[] residuals_at_eval_point,
            double[][] parameters,
            double[] x_plus_delta_ptr,
            double[] temp_residuals_ptr,
            double[] residuals_ptr) {
		    int i;
			//using Eigen::Map;
			// using Eigen::Matrix;
			
			//typedef Matrix<double, kNumResiduals, 1> ResidualVector;
			//typedef Matrix<double, kParameterBlockSize, 1> ParameterVector;
			
			//Map<const ParameterVector> x(x_ptr, parameter_block_size);
		    double x[] = new double[kParameterBlockSize];
		    for (i = 0; i < parameter_block_size; i++) {
		    	x[i] = x_ptr[i];
		    }
			//Map<ParameterVector> x_plus_delta(x_plus_delta_ptr,
			            // parameter_block_size);
		    double x_plus_delta[] = new double[kParameterBlockSize];
		    for (i = 0; i < parameter_block_size; i++) {
		    	x_plus_delta[i] = x_plus_delta_ptr[i];
		    }
			
			//Map<ResidualVector> residuals(residuals_ptr, num_residuals);
		    double residuals[] = new double[kNumResiduals];
		    for (i = 0; i < num_residuals; i++) {
		    	residuals[i] = residuals_ptr[i];
		    }
			//Map<ResidualVector> temp_residuals(temp_residuals_ptr, num_residuals);
		    double temp_residuals[] = new double[kNumResiduals];
		    for (i = 0; i < num_residuals; i++) {
		    	temp_residuals[i] = temp_residuals_ptr[i];
		    }
			
			// Mutate 1 element at a time and then restore.
			x_plus_delta[parameter_index] = x[parameter_index] + delta;
		    parameters[parameter_block_index][parameter_index] = x_plus_delta[parameter_index];   
			
		    double xp[];
		    switch (testCase) {
			case COST_FUNCTOR_EXAMPLE:
				xp = parameters[0];
			    if (!((CostFunctorExample) functor).operator(xp, residuals)) {
			    	return false;
			    }
			    break;
			case CURVE_FITTING_EXAMPLE:
				xp = parameters[0];
			    if (!((CurveFittingFunctorExample) functor).operator(xp, residuals)) {
			    	return false;
			    }
			    break;
		    } // switch(testCase)
			
			
			// Compute this column of the jacobian in 3 steps:
			// 1. Store residuals for the forward part.
			// 2. Subtract residuals for the backward (or 0) part.
			// 3. Divide out the run.
			double one_over_delta = 1.0 / delta;
			if (kMethod == NumericDiffMethodType.CENTRAL || kMethod == NumericDiffMethodType.RIDDERS) {
			// Compute the function on the other side of x(parameter_index).
			x_plus_delta[parameter_index] = x[parameter_index] - delta;
			parameters[parameter_block_index][parameter_index] = x_plus_delta[parameter_index];
			
			
			switch (testCase) {
			case COST_FUNCTOR_EXAMPLE:
				xp = parameters[0];
			    if (!((CostFunctorExample) functor).operator(xp, temp_residuals)) {
			    	return false;
			    }
			    break;
			case CURVE_FITTING_EXAMPLE:
				xp = parameters[0];
			    if (!((CurveFittingFunctorExample) functor).operator(xp, temp_residuals)) {
			    	return false;
			    }
			    break;
		    } // switch(testCase)
			
			for (i = 0; i < residuals.length; i++) {
				residuals[i] -= temp_residuals[i];
			}
			one_over_delta /= 2;
			} else {
			// Forward difference only; reuse existing residuals evaluation.
		    for (i = 0; i < num_residuals; i++) {
		    	residuals[i] -= residuals_at_eval_point[i];
		    }
			}
			
			// Restore x_plus_delta.
			x_plus_delta[parameter_index] = x[parameter_index];
			parameters[parameter_block_index][parameter_index] = x_plus_delta[parameter_index];
			
			// Divide out the run to get slope.
			for (i = 0; i < residuals.length; i++) {
				residuals[i] *= one_over_delta;
			}
			
			for (i = 0; i < parameter_block_size; i++) {
		    	x_plus_delta_ptr[i] = x_plus_delta[i];
		    }
			
			for (i = 0; i < num_residuals; i++) {
		    	residuals_ptr[i] = residuals[i];
		    }
			
			for (i = 0; i < num_residuals; i++) {
		    	temp_residuals_ptr[i] = temp_residuals[i];
		    }
			
			return true;
		}
	
	  // This numeric difference implementation uses adaptive differentiation
	  // on the parameters to obtain the Jacobian matrix. The adaptive algorithm
	  // is based on Ridders' method for adaptive differentiation, which creates
	  // a Romberg tableau from varying step sizes and extrapolates the
	  // intermediate results to obtain the current computational error.
	  //
	  // References:
	  // C.J.F. Ridders, Accurate computation of F'(x) and F'(x) F"(x), Advances
	  // in Engineering Software (1978), Volume 4, Issue 2, April 1982,
	  // Pages 75-76, ISSN 0141-1195,
	  // http://dx.doi.org/10.1016/S0141-1195(82)80057-0.
	  public <CostFunctor> boolean EvaluateRiddersJacobianColumn(NumericDiffMethodType kMethod, int kNumResiduals, int kParameterBlockSize,
	      CostFunctor functor,
	      int parameter_index,
	      double delta,
	      NumericDiffOptions options,
	      int num_residuals,
	      int parameter_block_index,
	      int parameter_block_size,
	      double[] x_ptr,
	      double[] residuals_at_eval_point,
	      double[][] parameters,
	      double[] x_plus_delta_ptr,
	      double[] temp_residuals_ptr,
	      double[] residuals_ptr) {
		  int i, r, c;
		  double diff;
		  double temp;
	    //using Eigen::Map;
	    //using Eigen::Matrix;
	    //using Eigen::aligned_allocator;

	    //typedef Matrix<double, kNumResiduals, 1> ResidualVector;
	    //typedef Matrix<double, kNumResiduals, Eigen::Dynamic> ResidualCandidateMatrix;
	    //typedef Matrix<double, kParameterBlockSize, 1> ParameterVector;

	    //Map<const ParameterVector> x(x_ptr, parameter_block_size);
		  double x[] = new double[kParameterBlockSize];
		    for (i = 0; i < parameter_block_size; i++) {
		    	x[i] = x_ptr[i];
		    }
	    //Map<ParameterVector> x_plus_delta(x_plus_delta_ptr,
	                                      //parameter_block_size);
		    double x_plus_delta[] = new double[kParameterBlockSize];
		    for (i = 0; i < parameter_block_size; i++) {
		    	x_plus_delta[i] = x_plus_delta_ptr[i];
		    }

	    //Map<ResidualVector> residuals(residuals_ptr, num_residuals);
		    double residuals[] = new double[kNumResiduals];
		    for (i = 0; i < num_residuals; i++) {
		    	residuals[i] = residuals_ptr[i];
		    }
	    //Map<ResidualVector> temp_residuals(temp_residuals_ptr, num_residuals);
		    double temp_residuals[] = new double[kNumResiduals];
		    for (i = 0; i < num_residuals; i++) {
		    	temp_residuals[i] = temp_residuals_ptr[i];
		    }

	    // In order for the algorithm to converge, the step size should be
	    // initialized to a value that is large enough to produce a significant
	    // change in the function.
	    // As the derivative is estimated, the step size decreases.
	    // By default, the step sizes are chosen so that the middle column
	    // of the Romberg tableau uses the input delta.
	    double current_step_size = delta *
	        Math.pow(options.ridders_step_shrink_factor,
	            options.max_num_ridders_extrapolations / 2);

	    // Double-buffering temporary differential candidate vectors
	    // from previous step size.
	    //ResidualCandidateMatrix stepsize_candidates_a(
	        //num_residuals,
	        //options.max_num_ridders_extrapolations);
	    double stepsize_candidates_a[][] = new double[num_residuals][options.max_num_ridders_extrapolations];
	    //ResidualCandidateMatrix stepsize_candidates_b(
	        //num_residuals,
	        //options.max_num_ridders_extrapolations);
	    double stepsize_candidates_b[][] = new double[num_residuals][options.max_num_ridders_extrapolations];
	    double[][] current_candidates = stepsize_candidates_a;
	    double[][] current_candidates_col = new double[options.max_num_ridders_extrapolations][num_residuals];
	    double[][] previous_candidates = stepsize_candidates_b;
	    double[][] previous_candidates_col = new double[options.max_num_ridders_extrapolations][num_residuals];

	    // Represents the computational error of the derivative. This variable is
	    // initially set to a large value, and is set to the difference between
	    // current and previous finite difference extrapolations.
	    // norm_error is supposed to decrease as the finite difference tableau
	    // generation progresses, serving both as an estimate for differentiation
	    // error and as a measure of differentiation numerical stability.
	    double norm_error = Double.MAX_VALUE;

	    // Loop over decreasing step sizes until:
	    //  1. Error is smaller than a given value (ridders_epsilon),
	    //  2. Maximal order of extrapolation reached, or
	    //  3. Extrapolation becomes numerically unstable.
	    for (i = 0; i < options.max_num_ridders_extrapolations; ++i) {
	      // Compute the numerical derivative at this step size.
	      if (!EvaluateJacobianColumn(kMethod, kNumResiduals, kParameterBlockSize,
	    		                      functor, parameter_index, current_step_size,
	                                  num_residuals,
	                                  parameter_block_index,
	                                  parameter_block_size,
	                                  x,
	                                  residuals_at_eval_point,
	                                  parameters,
	                                  x_plus_delta,
	                                  temp_residuals,
	                                  current_candidates_col[0])) {
	        // Something went wrong; bail.
	        return false;
	      }
	      for (r = 0; r < num_residuals; r++) {
	    	  current_candidates[r][0] = current_candidates_col[0][r];
	      }
	      

	      // Store initial results.
	      if (i == 0) {
	        residuals = current_candidates_col[0];
	      }

	      // Shrink differentiation step size.
	      current_step_size /= options.ridders_step_shrink_factor;

	      // Extrapolation factor for Richardson acceleration method (see below).
	      double richardson_factor = options.ridders_step_shrink_factor *
	          options.ridders_step_shrink_factor;
	      for (int k = 1; k <= i; ++k) {
	        // Extrapolate the various orders of finite differences using
	        // the Richardson acceleration method.
	    	for (r = 0; r < num_residuals; r++) {
	        current_candidates_col[k][r] =
	            (richardson_factor * current_candidates_col[k - 1][r] -
	             previous_candidates_col[k - 1][r]) / (richardson_factor - 1.0);
	        current_candidates[r][k] = current_candidates_col[k][r];
	    	}

	        richardson_factor *= options.ridders_step_shrink_factor *
	            options.ridders_step_shrink_factor;

	        // Compute the difference between the previous value and the current.
	        double currentNorm = 0.0;
	        for (r = 0; r < num_residuals; r++) {
	            diff = current_candidates_col[k][r] - current_candidates_col[k-1][r];
	            currentNorm += (diff * diff);
	        }
	        currentNorm = Math.sqrt(currentNorm);
	        double previousNorm = 0.0;
	        for (r = 0; r < num_residuals; r++) {
	        	diff = current_candidates_col[k][r] - previous_candidates_col[k-1][r];
	        	previousNorm += (diff * diff);
	        }
	        previousNorm = Math.sqrt(previousNorm);
	        double candidate_error = Math.max(currentNorm, previousNorm);

	        // If the error has decreased, update results.
	        if (candidate_error <= norm_error) {
	          norm_error = candidate_error;
	          residuals = current_candidates_col[k];

	          // If the error is small enough, stop.
	          if (norm_error < options.ridders_epsilon) {
	            break;
	          }
	        }
	      }

	      // After breaking out of the inner loop, declare convergence.
	      if (norm_error < options.ridders_epsilon) {
	        break;
	      }

	      // Check to see if the current gradient estimate is numerically unstable.
	      // If so, bail out and return the last stable result.
	      if (i > 0) {
	    	double tableau_error = 0.0;
	    	for (r = 0; r < num_residuals; r++) {
	    		diff = current_candidates_col[i][r] - previous_candidates_col[i-1][r];
	    		tableau_error += (diff * diff);
	    	}
	    	tableau_error = Math.sqrt(tableau_error);

	        // Compare current error to the chosen candidate's error.
	        if (tableau_error >= 2 * norm_error) {
	          break;
	        }
	      }

	      for (r = 0; r < num_residuals; r++) {
	    	  for (c = 0; c < options.max_num_ridders_extrapolations; c++) {
	    		  temp = current_candidates[r][c];
	    		  current_candidates[r][c] = previous_candidates[r][c];
	    		  previous_candidates[r][c] = temp;
	    		  current_candidates_col[c][r] = current_candidates[r][c];
	    		  previous_candidates_col[c][r] = previous_candidates[r][c];
	    	  }
	      }
	    }
	    for (i = 0; i < parameter_block_size; i++) {
	    	x_plus_delta_ptr[i] = x_plus_delta[i];
	    }
		
		for (i = 0; i < num_residuals; i++) {
	    	residuals_ptr[i] = residuals[i];
	    }
		
		for (i = 0; i < num_residuals; i++) {
	    	temp_residuals_ptr[i] = temp_residuals[i];
	    }
	    return true;
	  }

	class AutoDiffCostFunction<CostFunctor> extends SizedCostFunction {
		private CostFunctor functor_;
		private int kNumResiduals;
		private int num_residuals;

		// Takes ownership of functor. Uses the template-provided value for the
		// number of residuals ("kNumResiduals").
		public AutoDiffCostFunction(CostFunctor functor, int kNumResiduals, int N0, int N1, int N2, int N3, int N4,
				int N5, int N6, int N7, int N8, int N9) {
			super(kNumResiduals, N0, N1, N2, N3, N4, N5, N6, N7, N8, N9);
			this.functor_ = functor;
			this.kNumResiduals = kNumResiduals;
			if (kNumResiduals == DYNAMIC) {
				System.err
						.println("Can't run the fixed-size constructor if the number of residuals is set to DYNAMIC.");
			}
		}

		// Takes ownership of functor. Ignores the template-provided
		// kNumResiduals in favor of the "num_residuals" argument provided.
		//
		// This allows for having autodiff cost functions which return varying
		// numbers of residuals at runtime.
		public AutoDiffCostFunction(CostFunctor functor, int kNumResiduals, int N0, int N1, int N2, int N3, int N4,
				int N5, int N6, int N7, int N8, int N9, int num_residuals) {
			super(kNumResiduals, N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, num_residuals);
			this.functor_ = functor;
			this.kNumResiduals = kNumResiduals;
			this.num_residuals = num_residuals;
			if (kNumResiduals != DYNAMIC) {
				System.err.println("Can't run the dynamic-size constructor if the number of residuals is not DYNAMIC.");
				return;
			}

		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			if (jacobians == null) {
				if ((kNumResiduals == 1) || ((kNumResiduals == DYNAMIC) && (num_residuals == 1))) {
					double x[] = parameters.get(0);
					return ((CostFunctorExample) functor_).operator(x, residuals);
				} else {
					return true;
				}
			} else {
				return true;
			}
		}
		/*
		 * return internal::VariadicEvaluate< CostFunctor, double, N0, N1, N2, N3, N4,
		 * N5, N6, N7, N8, N9> ::Call(*functor_, parameters, residuals); } return
		 * internal::AutoDiff<CostFunctor, double, N0, N1, N2, N3, N4, N5, N6, N7, N8,
		 * N9>::Differentiate( functor_, parameters, SizedCostFunction<kNumResiduals,
		 * N0, N1, N2, N3, N4, N5, N6, N7, N8, N9>::num_residuals(), residuals,
		 * jacobians); }
		 */

	}
	
	// Some common implementations follow below.
	//
	// Note: in the region of interest (i.e. s < 3) we have:
	//   TrivialLoss >= HuberLoss >= SoftLOneLoss >= CauchyLoss


	// This corresponds to no robustification.
	//
	//   rho(s) = s
	//
	// At s = 0: rho = [0, 1, 0].
	//
	// It is not normally necessary to use this, as passing NULL for the
	// loss function when building the problem accomplishes the same
	// thing.
	class TrivialLoss extends LossFunction {
	 public TrivialLoss() {
		 super();
	 }
	 
	 public void Evaluate(double s, double rho[]) {
		  rho[0] = s;
		  rho[1] = 1.0;
		  rho[2] = 0.0;
		}

	};
	
	// Huber.
	//
	//   rho(s) = s               for s <= 1,
	//   rho(s) = 2 sqrt(s) - 1   for s >= 1.
	//
	// At s = 0: rho = [0, 1, 0].
	//
	// The scaling parameter 'a' corresponds to 'delta' on this page:
	//   http://en.wikipedia.org/wiki/Huber_Loss_Function
	class HuberLoss extends LossFunction {
		private double a_;
			  // b = a^2.
	    private double b_;
	    public HuberLoss(double a) {
	    	super();
	    	a_ = a;
	    	b_ = (a * a);
	    }
	  
	    public void Evaluate(double s, double rho[]) {
	    	  if (s > b_) {
	    	    // Outlier region.
	    	    // 'r' is always positive.
	    	    final double r = Math.sqrt(s);
	    	    rho[0] = 2.0 * a_ * r - b_;
	    	    rho[1] = Math.max(Double.MIN_VALUE, a_ / r);
	    	    rho[2] = - rho[1] / (2.0 * s);
	    	  } else {
	    	    // Inlier region.
	    	    rho[0] = s;
	    	    rho[1] = 1.0;
	    	    rho[2] = 0.0;
	    	  }
	    	}

	};
	
	// Soft L1, similar to Huber but smooth.
	//
	//   rho(s) = 2 (sqrt(1 + s) - 1).
	//
	// At s = 0: rho = [0, 1, -1/2].
	class SoftLOneLoss extends LossFunction {
	    // b = a^2.
		private double b_;
	    // c = 1 / a^2.
	    private double c_;
	    public SoftLOneLoss(double a) {
	    	super();
	    	b_ = (a * a);
	    	c_ = (1.0/ b_); 
	
	    }
	  
	    public void Evaluate(double s, double rho[]) {
	    	  final double sum = 1.0 + s * c_;
	    	  final double tmp = Math.sqrt(sum);
	    	  // 'sum' and 'tmp' are always positive, assuming that 's' is.
	    	  rho[0] = 2.0 * b_ * (tmp - 1.0);
	    	  rho[1] = Math.max(Double.MIN_VALUE, 1.0 / tmp);
	    	  rho[2] = - (c_ * rho[1]) / (2.0 * sum);
	    }
	 
	};
	
	// Inspired by the Cauchy distribution
	//
	//   rho(s) = log(1 + s).
	//
	// At s = 0: rho = [0, 1, -1].
	class CauchyLoss extends LossFunction {
			  // b = a^2.
			  private double b_;
			  // c = 1 / a^2.
			  private double c_;
			  
	          public CauchyLoss(double a) {
	        	  super();
	        	  b_ = (a * a);
	        	  c_ = (1.0 / b_);
	          }
	  
	          public void Evaluate(double s, double rho[]) {
	        	  final double sum = 1.0 + s * c_;
	        	  final double inv = 1.0 / sum;
	        	  // 'sum' and 'inv' are always positive, assuming that 's' is.
	        	  rho[0] = b_ * Math.log(sum);
	        	  rho[1] = Math.max(Double.MIN_VALUE, inv);
	        	  rho[2] = - c_ * (inv * inv);
	        	}
	 
	};
	
	// Loss that is capped beyond a certain level using the arc-tangent function.
	// The scaling parameter 'a' determines the level where falloff occurs.
	// For costs much smaller than 'a', the loss function is linear and behaves like
	// TrivialLoss, and for values much larger than 'a' the value asymptotically
	// approaches the constant value of a * PI / 2.
	//
	//   rho(s) = a atan(s / a).
	//
	// At s = 0: rho = [0, 1, 0].
	class ArctanLoss extends LossFunction {
		private double a_;
	    // b = 1 / a^2.
	    private double b_;
	    
	    public ArctanLoss(double a) {
	    	super();
	    	a_ = a;
	    	b_ = (1.0 / (a * a));
	    }
	  
	    public void Evaluate(double s, double rho[]) {
	    	  final double sum = 1 + s * s * b_;
	    	  final double inv = 1 / sum;
	    	  // 'sum' and 'inv' are always positive.
	    	  rho[0] = a_ * Math.atan2(s, a_);
	    	  rho[1] = Math.max(Double.MIN_VALUE, inv);
	    	  rho[2] = -2.0 * s * b_ * (inv * inv);
	    }

	};
	
	// Loss function that maps to approximately zero cost in a range around the
	// origin, and reverts to linear in error (quadratic in cost) beyond this range.
	// The tolerance parameter 'a' sets the nominal point at which the
	// transition occurs, and the transition size parameter 'b' sets the nominal
	// distance over which most of the transition occurs. Both a and b must be
	// greater than zero, and typically b will be set to a fraction of a.
	// The slope rho'[s] varies smoothly from about 0 at s <= a - b to
	// about 1 at s >= a + b.
	//
	// The term is computed as:
	//
	//   rho(s) = b log(1 + exp((s - a) / b)) - c0.
	//
	// where c0 is chosen so that rho(0) == 0
	//
	//   c0 = b log(1 + exp(-a / b)
	//
	// This has the following useful properties:
	//
	//   rho(s) == 0               for s = 0
	//   rho'(s) ~= 0              for s << a - b
	//   rho'(s) ~= 1              for s >> a + b
	//   rho''(s) > 0              for all s
	//
	// In addition, all derivatives are continuous, and the curvature is
	// concentrated in the range a - b to a + b.
	//
	// At s = 0: rho = [0, ~0, ~0].
	class TolerantLoss extends LossFunction {
		private double a_, b_, c_;
		
		public TolerantLoss(double a, double b) {
		  super();
		  if (a < 0.0) {
			  System.err.println("a = " + a + " must be >= 0.0 in public TolerantLoss");
			  return;
		  }
		  if (b <= 0.0) {
			  System.err.println("b = " + b + " must be > 0.0 in public TolerantLoss");
			  return;
		  }
	      a_ = a;
	      b_ = b;
	      c_ = (b * Math.log(1.0 + Math.exp(-a / b)));
	   }

		public void Evaluate(double s, double rho[]) {
			  final double x = (s - a_) / b_;
			  // The basic equation is rho[0] = b ln(1 + e^x).  However, if e^x is too
			  // large, it will overflow.  Since numerically 1 + e^x == e^x when the
			  // x is greater than about ln(2^53) for doubles, beyond this threshold
			  // we substitute x for ln(1 + e^x) as a numerically equivalent approximation.
			  final double kLog2Pow53 = 36.7;  // ln(MathLimits<double>::kEpsilon).
			  if (x > kLog2Pow53) {
			    rho[0] = s - a_ - c_;
			    rho[1] = 1.0;
			    rho[2] = 0.0;
			  } else {
			    final double e_x = Math.exp(x);
			    rho[0] = b_ * Math.log(1.0 + e_x) - c_;
			    rho[1] = Math.max(Double.MIN_VALUE, e_x / (1.0 + e_x));
			    rho[2] = 0.5 / (b_ * (1.0 + Math.cosh(x)));
			  }
			}

	};
	
	// This is the Tukey biweight loss function which aggressively
	// attempts to suppress large errors.
	//
	// The term is computed as:
	//
	//   rho(s) = a^2 / 6 * (1 - (1 - s / a^2)^3 )   for s <= a^2,
	//   rho(s) = a^2 / 6                            for s >  a^2.
	//
	// At s = 0: rho = [0, 0.5, -1 / a^2]
	class TukeyLoss extends LossFunction {
		private double a_squared_;
		
	    public TukeyLoss(double a) {
	    	super();
	    	a_squared_ = (a * a);
	    }
	  
	    public void Evaluate(double s, double[] rho) {
	    	  if (s <= a_squared_) {
	    	    // Inlier region.
	    	    final double value = 1.0 - s / a_squared_;
	    	    final double value_sq = value * value;
	    	    rho[0] = a_squared_ / 6.0 * (1.0 - value_sq * value);
	    	    rho[1] = 0.5 * value_sq;
	    	    rho[2] = -1.0 / a_squared_ * value;
	    	  } else {
	    	    // Outlier region.
	    	    rho[0] = a_squared_ / 6.0;
	    	    rho[1] = 0.0;
	    	    rho[2] = 0.0;
	    	  }
	    	}

	};
	
	// Composition of two loss functions.  The error is the result of first
	// evaluating g followed by f to yield the composition f(g(s)).
	// The loss functions must not be NULL.
	class ComposedLoss extends LossFunction {
		private LossFunction f_, g_;
		private Ownership ownership_f_, ownership_g_;
		
		public ComposedLoss(LossFunction f, Ownership ownership_f,
                LossFunction g, Ownership ownership_g) {
			super();
			if (f == null) {
				System.err.println("LossFunction f == null in public ComposedLoss");
				return;
			}
			if (g == null) {
				System.err.println("LossFunction g == null in public ComposedLoss");
				return;
			}
			f_ = f;
			g_ = g;
			ownership_f_ = ownership_f;
			ownership_g_ = ownership_g;
        }

	    public void finalize() {
	    	if (ownership_f_ == Ownership.DO_NOT_TAKE_OWNERSHIP) {
	    	    f_ = null;
	    	  }
	    	  if (ownership_g_ == Ownership.DO_NOT_TAKE_OWNERSHIP) {
	    	    g_ = null;
	    	  }

	    }
	  
	    public void Evaluate(double s, double rho[]) {
	    	  double rho_f[] = new double[3];
	    	  double rho_g[] = new double[3];
	    	  g_.Evaluate(s, rho_g);
	    	  f_.Evaluate(rho_g[0], rho_f);
	    	  rho[0] = rho_f[0];
	    	  // f'(g(s)) * g'(s).
	    	  rho[1] = rho_f[1] * rho_g[1];
	    	  // f''(g(s)) * g'(s) * g'(s) + f'(g(s)) * g''(s).
	    	  rho[2] = rho_f[2] * rho_g[1] * rho_g[1] + rho_f[1] * rho_g[2];
	    	}
 
	};
	
	// The discussion above has to do with length scaling: it affects the space
	// in which s is measured. Sometimes you want to simply scale the output
	// value of the robustifier. For example, you might want to weight
	// different error terms differently (e.g., weight pixel reprojection
	// errors differently from terrain errors).
	//
	// If rho is the wrapped robustifier, then this simply outputs
	// s -> a * rho(s)
	//
	// The first and second derivatives are, not surprisingly
	// s -> a * rho'(s)
	// s -> a * rho''(s)
	//
	// Since we treat the a NULL Loss function as the Identity loss
	// function, rho = NULL is a valid input and will result in the input
	// being scaled by a. This provides a simple way of implementing a
	// scaled ResidualBlock.
	class ScaledLoss extends LossFunction {
		private LossFunction rho_;
	    private double a_;
	    private Ownership ownership_;
	  // Constructs a ScaledLoss wrapping another loss function. Takes
	  // ownership of the wrapped loss function or not depending on the
	  // ownership parameter.
	    public ScaledLoss(LossFunction rho, double a, Ownership ownership) {
	      super();
	      rho_ = rho;
	      a_ = a;
	      ownership_ = ownership; 
	    }

	   public void finalize() {
		    if (ownership_ == Ownership.DO_NOT_TAKE_OWNERSHIP) {
		      rho_ = null;
		    }
	    }
	   
	   public void Evaluate(double s, double rho[]) {
		   if (rho_ == null) {
		     rho[0] = a_ * s;
		     rho[1] = a_;
		     rho[2] = 0.0;
		   } else {
		     rho_.Evaluate(s, rho);
		     rho[0] *= a_;
		     rho[1] *= a_;
		     rho[2] *= a_;
		   }
		 }

	  
	};

	abstract class LossFunction {
		public LossFunction() {

		}

		// For a residual vector with squared 2-norm 'sq_norm', this method
		// is required to fill in the value and derivatives of the loss
		// function (rho in this example):
		//
		// out[0] = rho(sq_norm),
		// out[1] = rho'(sq_norm),
		// out[2] = rho''(sq_norm),
		//
		// Here the convention is that the contribution of a term to the
		// cost function is given by 1/2 rho(s), where
		//
		// s = ||residuals||^2.
		//
		// Calling the method with a negative value of 's' is an error and
		// the implementations are not required to handle that case.
		//
		// Most sane choices of rho() satisfy:
		//
		// rho(0) = 0,
		// rho'(0) = 1,
		// rho'(s) < 1 in outlier region,
		// rho''(s) < 0 in outlier region,
		//
		// so that they mimic the least squares cost for small residuals.
		public abstract void Evaluate(double sq_norm, double out[]);
		
		public void finalize() {
			
		}
	} // abstract class LossFunction

	class Program {
		// The Program does not own the ParameterBlock or ResidualBlock objects.
		private Vector<ParameterBlock> parameter_blocks_;
		private Vector<ResidualBlock> residual_blocks_;

		public Program() {
			parameter_blocks_ = new Vector<ParameterBlock>();
			residual_blocks_ = new Vector<ResidualBlock>();
		}

		public int NumResidualBlocks() {
			if (residual_blocks_ == null) {
				return 0;
			}
			return residual_blocks_.size();
		}

		public int NumParameterBlocks() {
			if (parameter_blocks_ == null) {
				return 0;
			}
			return parameter_blocks_.size();
		}

		public int NumResiduals() {
			int num_residuals = 0;
			if (residual_blocks_ == null) {
				return 0;
			}
			for (int i = 0; i < residual_blocks_.size(); ++i) {
				num_residuals += residual_blocks_.get(i).NumResiduals();
			}
			return num_residuals;
		}

		public int NumParameters() {
			if (parameter_blocks_ == null) {
				return 0;
			}
			int num_parameters = 0;
			for (int i = 0; i < parameter_blocks_.size(); ++i) {
				num_parameters += parameter_blocks_.get(i).Size();
			}
			return num_parameters;
		}

		public int NumEffectiveParameters() {
			if (parameter_blocks_ == null) {
				return 0;
			}
			int num_parameters = 0;
			for (int i = 0; i < parameter_blocks_.size(); ++i) {
				num_parameters += parameter_blocks_.get(i).LocalSize();
			}
			return num_parameters;
		}

		public boolean SetParameterBlockStatePtrsToUserStatePtrs() {
			for (int i = 0; i < parameter_blocks_.size(); ++i) {
				if (!parameter_blocks_.get(i).IsConstant()
						&& !parameter_blocks_.get(i).SetState(parameter_blocks_.get(i).user_state(),0)) {
					return false;
				}
			}
			return true;
		}

		public Vector<ParameterBlock> parameter_blocks() {
			return parameter_blocks_;
		}

		public Vector<ParameterBlock> mutable_parameter_blocks() {
			return parameter_blocks_;
		}

		public Vector<ResidualBlock> residual_blocks() {
			return residual_blocks_;
		}


		public boolean ParameterBlocksAreFinite(String message[]) {
			if (message == null) {
				System.err.println("String message[] is null in Program.ParameterBlocksAreFinite");
				return false;
			}
			for (int i = 0; i < parameter_blocks_.size(); ++i) {
				ParameterBlock parameter_block = parameter_blocks_.get(i);
				double[] array = parameter_block.user_state();
				int size = parameter_block.Size();
				int invalid_index = FindInvalidValue(size, array);
				if (invalid_index != size) {
					message[0] = "ParameterBlock with size " + size + " has at least one invalid value.\n"
							+ "First invalid value = " + array[invalid_index] + " is at index " + invalid_index
							+ "\nParameter block values: ";
					for (int k = 0; k < size; k++) {
						message[0] += "\n" + array[k];
					}
					return false;
				}
			}
			return true;
		}

		public boolean IsFeasible(String message[]) {
			if (message == null) {
				System.err.println("String message[] is null in Program.IsFeasible");
				return false;
			}
			for (int i = 0; i < parameter_blocks_.size(); ++i) {
				ParameterBlock parameter_block = parameter_blocks_.get(i);
				double[] parameters = parameter_block.user_state();
				int size = parameter_block.Size();
				if (parameter_block.IsConstant()) {
					// Constant parameter blocks must start in the feasible region
					// to ultimately produce a feasible solution, since Ceres cannot
					// change them.
					for (int j = 0; j < size; ++j) {
						double lower_bound = parameter_block.LowerBoundForParameter(j);
						double upper_bound = parameter_block.UpperBoundForParameter(j);
						if (parameters[j] < lower_bound || parameters[j] > upper_bound) {
							message[0] = "ParameterBlock with size " + size + " has at least one infeasible value."
									+ "\nFirst infeasible value is at index: " + j + "." + "\nLower bound: "
									+ lower_bound + " , value: " + parameters[j] + " , upper bound: " + upper_bound
									+ "\nParameter block values: ";
							for (int k = 0; k < size; k++) {
								message[0] += "\n" + parameters[k];
							}

							return false;
						}
					}
				} else {
					// Variable parameter blocks must have non-empty feasible
					// regions, otherwise there is no way to produce a feasible
					// solution.
					for (int j = 0; j < size; ++j) {
						double lower_bound = parameter_block.LowerBoundForParameter(j);
						double upper_bound = parameter_block.UpperBoundForParameter(j);
						if (lower_bound >= upper_bound) {
							message[0] = "ParameterBlock with size " + size
									+ " has lower_bound >= upper_bound for parameter " + j + "." + "\nLower bound: "
									+ lower_bound + " , value: " + parameters[j] + " , upper bound: " + upper_bound
									+ "\nParameter block values: ";
							for (int k = 0; k < size; k++) {
								message[0] += "\n" + parameters[k];
							}
							return false;
						}
					}
				}
			}

			return true;
		}

		public boolean IsBoundsConstrained() {
			for (int i = 0; i < parameter_blocks_.size(); ++i) {
				ParameterBlock parameter_block = parameter_blocks_.get(i);
				if (parameter_block.IsConstant()) {
					continue;
				}
				int size = parameter_block.Size();
				for (int j = 0; j < size; ++j) {
					double lower_bound = parameter_block.LowerBoundForParameter(j);
					double upper_bound = parameter_block.UpperBoundForParameter(j);
					if (lower_bound > -Double.MAX_VALUE || upper_bound < Double.MAX_VALUE) {
						return true;
					}
				}
			}
			return false;
		}

		public Program CreateReducedProgram(Vector<double[]> removed_parameter_blocks, double fixed_cost[],
				String error[]) {
			if (removed_parameter_blocks == null) {
				System.err.println("In Program.CreateReducedProgram removed_parameter_blocks == null");
				return null;
			}
			if (fixed_cost == null) {
				System.err.println("In Program.CreateReducedProgram fixed_cost == null");
				return null;
			}
			if (error == null) {
				System.err.println("In Program.CreateReducedProgram error == null");
				return null;
			}

			// scoped_ptr<Program> reduced_program(new Program(*this));
			Program reduced_program = new Program();
			reduced_program.parameter_blocks_ = parameter_blocks_;
			reduced_program.residual_blocks_ = residual_blocks_;
			if (!reduced_program.RemoveFixedBlocks(removed_parameter_blocks, fixed_cost, error)) {
				return null;
			}

			reduced_program.SetParameterOffsetsAndIndex();
			return reduced_program;
		}

		public boolean RemoveFixedBlocks(Vector<double[]> removed_parameter_blocks, double fixed_cost[],
				String error[]) {
			if (removed_parameter_blocks == null) {
				System.err.println("In Program.RemoveFixedBlocks removed_parameter_blocks == null");
				return false;
			}
			if (fixed_cost == null) {
				System.err.println("In Program.RemoveFixedBlocks fixed_cost == null");
				return false;
			}
			if (error == null) {
				System.err.println("In Program.RemoveFixedBlocks error == null");
				return false;
			}

			// scoped_array<double> residual_block_evaluate_scratch;
			double residual_block_evaluate_scratch[];
			residual_block_evaluate_scratch = new double[MaxScratchDoublesNeededForEvaluate()];
			fixed_cost[0] = 0.0;

			// Mark all the parameters as unused. Abuse the index member of the
			// parameter blocks for the marking.
			for (int i = 0; i < parameter_blocks_.size(); ++i) {
				parameter_blocks_.get(i).set_index(-1);
			}

			// Filter out residual that have all-constant parameters, and mark
			// all the parameter blocks that appear in residuals.
			int num_active_residual_blocks = 0;
			for (int i = 0; i < residual_blocks_.size(); ++i) {
				ResidualBlock residual_block = residual_blocks_.get(i);
				int num_parameter_blocks = residual_block.NumParameterBlocks();

				// Determine if the residual block is fixed, and also mark varying
				// parameters that appear in the residual block.
				boolean all_constant = true;
				for (int k = 0; k < num_parameter_blocks; k++) {
					ParameterBlock parameter_block = residual_block.parameter_blocks()[k];
					if (!parameter_block.IsConstant()) {
						all_constant = false;
						parameter_block.set_index(1);
					}
				}

				if (!all_constant) {
					residual_blocks_.set(num_active_residual_blocks++, residual_block);
					continue;
				}

				// The residual is constant and will be removed, so its cost is
				// added to the variable fixed_cost.
				double cost[] = new double[] { 0.0 };
				if (!residual_block.Evaluate(true, cost, null, null, residual_block_evaluate_scratch)) {
					error[0] = "Evaluation of the residual " + i + " failed during removal of fixed residual blocks.";
					return false;
				}
				fixed_cost[0] += cost[0];
			}
			// residual_blocks_.resize(num_active_residual_blocks);
			int oldSize = residual_blocks_.size();
			if (oldSize > num_active_residual_blocks) {
				while (residual_blocks_.size() > num_active_residual_blocks) {
					residual_blocks_.removeElementAt(residual_blocks_.size() - 1);
				}
			}

			// Filter out unused or fixed parameter blocks.
			int num_active_parameter_blocks = 0;
			removed_parameter_blocks.clear();
			for (int i = 0; i < parameter_blocks_.size(); ++i) {
				ParameterBlock parameter_block = parameter_blocks_.get(i);
				if (parameter_block.index() == -1) {
					removed_parameter_blocks.add(parameter_block.mutable_user_state());
				} else {
					parameter_blocks_.set(num_active_parameter_blocks++, parameter_block);
				}
			}
			// parameter_blocks_.resize(num_active_parameter_blocks);
			oldSize = parameter_blocks_.size();
			if (oldSize > num_active_parameter_blocks) {
				while (parameter_blocks_.size() > num_active_parameter_blocks) {
					parameter_blocks_.removeElementAt(parameter_blocks_.size() - 1);
				}
			}

			if (!(((NumResidualBlocks() == 0) && (NumParameterBlocks() == 0))
					|| ((NumResidualBlocks() != 0) && (NumParameterBlocks() != 0)))) {
				error[0] = "Congratulations, you found a bug in Ceres. Please report it.";
				return false;
			}

			return true;
		}

		public int MaxScratchDoublesNeededForEvaluate() {
			// Compute the scratch space needed for evaluate.
			int max_scratch_bytes_for_evaluate = 0;
			for (int i = 0; i < residual_blocks_.size(); ++i) {
				max_scratch_bytes_for_evaluate = Math.max(max_scratch_bytes_for_evaluate,
						residual_blocks_.get(i).NumScratchDoublesForEvaluate());
			}
			return max_scratch_bytes_for_evaluate;
		}

		public void SetParameterOffsetsAndIndex() {
			// Set positions for all parameters appearing as arguments to residuals to one
			// past the end of the parameter block array.
			for (int i = 0; i < residual_blocks_.size(); ++i) {
				ResidualBlock residual_block = residual_blocks_.get(i);
				for (int j = 0; j < residual_block.NumParameterBlocks(); ++j) {
					residual_block.parameter_blocks()[j].set_index(-1);
				}
			}
			// For parameters that appear in the program, set their position and offset.
			int state_offset = 0;
			int delta_offset = 0;
			for (int i = 0; i < parameter_blocks_.size(); ++i) {
				parameter_blocks_.get(i).set_index(i);
				parameter_blocks_.get(i).set_state_offset(state_offset);
				parameter_blocks_.get(i).set_delta_offset(delta_offset);
				state_offset += parameter_blocks_.get(i).Size();
				delta_offset += parameter_blocks_.get(i).LocalSize();
			}
		}
		
		public void ParameterBlocksToStateVector(Vector<Double> state) {
			int i;
			double state_array[] = new double[state.size()];
			for (i = 0; i < state.size(); i++) {
				state_array[i] = state.get(i);
			}
			ParameterBlocksToStateVector(state_array);
			for (i = 0; i < state.size(); i++) {
				state.set(i, state_array[i]);
			}
		}
		
		public void ParameterBlocksToStateVector(double[] state) {
			  int state_index = 0;
			  for (int i = 0; i < parameter_blocks_.size(); ++i) {
			    parameter_blocks_.get(i).GetState(state,state_index);
			    state_index += parameter_blocks_.get(i).Size();
			  }
			}


		public int MaxParametersPerResidualBlock() {
			int max_parameters = 0;
			for (int i = 0; i < residual_blocks_.size(); ++i) {
				max_parameters = Math.max(max_parameters, residual_blocks_.get(i).NumParameterBlocks());
			}
			return max_parameters;
		}

		public int MaxResidualsPerResidualBlock() {
			int max_residuals = 0;
			for (int i = 0; i < residual_blocks_.size(); ++i) {
				max_residuals = Math.max(max_residuals, residual_blocks_.get(i).NumResiduals());
			}
			return max_residuals;
		}

		public int MaxDerivativesPerResidualBlock() {
			int max_derivatives = 0;
			for (int i = 0; i < residual_blocks_.size(); ++i) {
				int derivatives = 0;
				ResidualBlock residual_block = residual_blocks_.get(i);
				int num_parameters = residual_block.NumParameterBlocks();
				for (int j = 0; j < num_parameters; ++j) {
					derivatives += residual_block.NumResiduals() * residual_block.parameter_blocks()[j].LocalSize();
				}
				max_derivatives = Math.max(max_derivatives, derivatives);
			}
			return max_derivatives;
		}

		public boolean IsParameterBlockSetIndependent(Set<double[]> independent_set) {
			// Loop over each residual block and ensure that no two parameter
			// blocks in the same residual block are part of
			// parameter_block_ptrs as that would violate the assumption that it
			// is an independent set in the Hessian matrix.
			for (ResidualBlock rb : residual_blocks_) {
				ParameterBlock parameter_blocks[] = rb.parameter_blocks();
				int num_parameter_blocks = rb.NumParameterBlocks();
				int count = 0;
				for (int i = 0; i < num_parameter_blocks; ++i) {
					if (independent_set.contains(parameter_blocks[i].mutable_user_state())) {
						count++;
					}
				}
				if (count > 1) {
					return false;
				}
			}
			return true;
		}

		public Vector<ResidualBlock> mutable_residual_blocks() {
			return residual_blocks_;
		}
		
		private boolean Plus(Vector<Double> state,
                Vector<Double> delta,
                Vector<Double> state_plus_delta) {
			int state_index = 0;
			int delta_index = 0;
			int state_plus_delta_index = 0;
			for (int i = 0; i < parameter_blocks_.size(); ++i) {
			 if (!parameter_blocks_.get(i).Plus(state, state_index, delta, delta_index,
					 state_plus_delta, state_plus_delta_index)) {
			   return false;
			 }
			 state_index += parameter_blocks_.get(i).Size();
			 delta_index += parameter_blocks_.get(i).LocalSize();
			 state_plus_delta_index += parameter_blocks_.get(i).Size();
			}
			return true;
			}

		public boolean StateVectorToParameterBlocks(Vector<Double> state) {
		    int i;
		    double state_array[] = new double[state.size()];
		    for (i = 0; i < state.size(); i++) {
		    	state_array[i] = state.get(i);
		    }
		    return StateVectorToParameterBlocks(state_array);
		}
		
		public boolean StateVectorToParameterBlocks(double state[]) {
			  int state_index = 0;
			  for (int i = 0; i < parameter_blocks_.size(); ++i) {
			    if (!parameter_blocks_.get(i).IsConstant() &&
			        !parameter_blocks_.get(i).SetState(state,state_index)) {
			      return false;
			    }
			    state_index += parameter_blocks_.get(i).Size();
			  }
			  return true;
			}

		public void CopyParameterBlockStateToUserState() {
			  for (int i = 0; i < parameter_blocks_.size(); ++i) {
			    parameter_blocks_.get(i).GetState(parameter_blocks_.get(i).mutable_user_state(),0);
			  }
			}

	} // class Program
	
	class ProblemOptions {
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

		public ProblemOptions() {
			cost_function_ownership = Ownership.TAKE_OWNERSHIP;
			loss_function_ownership = Ownership.TAKE_OWNERSHIP;
			local_parameterization_ownership = Ownership.TAKE_OWNERSHIP;
			enable_fast_removal = false;
			disable_all_safety_checks = false;
			context = new Context();
		}
	}

	class ProblemImpl {
		private Vector<double[]> residual_parameters_;
		private Program program_;
		private HashMap<double[], ParameterBlock> parameter_block_map_;
		protected ProblemOptions options_;
		// Iff enable_fast_removal is enabled, contains the current residual blocks.
		private HashSet<ResidualBlock> residual_block_set_;
		private HashMap<CostFunction, Integer> cost_function_ref_count_;
		private HashMap<LossFunction, Integer> loss_function_ref_count_;
		private boolean context_impl_owned_[] = new boolean[] {false};
		private Context context_impl_;
		private int count;
        private Vector<LocalParameterization> local_parameterizations_to_delete_;
		

		public ProblemImpl() {
			residual_parameters_ = new Vector<double[]>(10);
			options_ = new ProblemOptions();
			program_ = new Program();
			parameter_block_map_ = new HashMap<double[], ParameterBlock>();
			residual_block_set_ = new HashSet<ResidualBlock>();
			cost_function_ref_count_ = new HashMap<CostFunction, Integer>();
			loss_function_ref_count_ = new HashMap<LossFunction, Integer>();
			//InitializeContext(options_.context, context_impl_, context_impl_owned_);
			context_impl_owned_[0] = false;
			context_impl_ = options_.context;
			local_parameterizations_to_delete_ = new Vector<LocalParameterization>();
		}

		public ProblemImpl(ProblemOptions options) {
			residual_parameters_ = new Vector<double[]>(10);
			options_ = options;
			program_ = new Program();
			parameter_block_map_ = new HashMap<double[], ParameterBlock>();
			residual_block_set_ = new HashSet<ResidualBlock>();
			cost_function_ref_count_ = new HashMap<CostFunction, Integer>();
			loss_function_ref_count_ = new HashMap<LossFunction, Integer>();
			//InitializeContext(options_.context, context_impl_, context_impl_owned_);
			context_impl_owned_[0] = false;
			context_impl_ = options_.context;
			local_parameterizations_to_delete_ = new Vector<LocalParameterization>();
		}

		void InitializeContext(Context context, Context context_impl, boolean context_impl_owned[]) {
			if (context == null) {
				context_impl_owned[0] = true;
				context_impl = new Context();
			} else {
				context_impl_owned[0] = false;
				context_impl = context;
			}
		}
		
		public void AddParameterBlock(double[] values, int size) {
			  InternalAddParameterBlock(values, size);
		}


		public void AddParameterBlock(double[] values, int size, LocalParameterization local_parameterization) {
			ParameterBlock parameter_block = InternalAddParameterBlock(values, size);
			if (local_parameterization != null) {
				parameter_block.SetParameterization(local_parameterization);
			}
		}

		public void SetParameterBlockConstant(double[] values) {
			ParameterBlock parameter_block = FindWithDefault(parameter_block_map_, values, null);
			if (parameter_block == null) {
				System.err.println(
						"In SetParameterBlockConstant Parameter block not found for supplied double[] values.");
				System.err.println("You must add the parameter block to the problem before it can be set constant.");
				return;
			}

			parameter_block.SetConstant();
		}
		
		public boolean IsParameterBlockConstant(double[] values) {
			ParameterBlock parameter_block = FindWithDefault(parameter_block_map_, values, null);
			if (parameter_block == null) {
				System.err.println("In IsParameterBlockConstant Parameter block not found for supplied double[] values.");
				System.err.println("You must add the parameter block to the problem before it can be queried.");
				return false;
			}
			  
			  return parameter_block.IsConstant();
			}

		
		public void SetParameterBlockVariable(double[] values) {
			  ParameterBlock parameter_block =
			      FindWithDefault(parameter_block_map_, values, null);
			  if (parameter_block == null) {
			    System.err.println("In SetParameterBlockVariable Parameter block not found for supplied double[] values.");
			    System.err.println("You must add the parameter block to the problem before it can be set varying.");
			    return;
			  }

			  parameter_block.SetVarying();
		}
		
		public void SetParameterization(double[] values, LocalParameterization local_parameterization) {
			ParameterBlock parameter_block =
				      FindWithDefault(parameter_block_map_, values, null);
				  if (parameter_block == null) {
				    System.err.println("In SetParameterization Parameter block not found for supplied double[] values.");
				    System.err.println("You must add the parameter block to the problem before you can set its local parameterization.");
				    return;
				  }

			  parameter_block.SetParameterization(local_parameterization);
		}

		public LocalParameterization GetParameterization(double[] values) {
			ParameterBlock parameter_block = FindWithDefault(parameter_block_map_, values, null);
			if (parameter_block == null) {
				System.err.println(" In GetParameterization Parameter block not found for supplied double[] values.");
				System.err.println(
						"You must add the parameter block to the problem before you can get its local parameterization.");
				return null;
			}

			return parameter_block.local_parameterization();
		}

		public Program mutable_program() {
			return program_;
		}

		public Program program() {
			return program_;
		}

		public Context context() {
			return context_impl_;
		}

		public ResidualBlock AddResidualBlock(CostFunction cost_function, LossFunction loss_function, double[] x0) {
			residual_parameters_.clear();
			residual_parameters_.add(x0);
			return AddResidualBlock(cost_function, loss_function, residual_parameters_);
		}
		
		public ResidualBlock AddResidualBlock(CostFunction cost_function, LossFunction loss_function, double[] x0, double[] x1) {
			residual_parameters_.clear();
			residual_parameters_.add(x0);
			residual_parameters_.add(x1);
			return AddResidualBlock(cost_function, loss_function, residual_parameters_);
		}
		
		public ResidualBlock AddResidualBlock(CostFunction cost_function, LossFunction loss_function, double[] x0, double[] x1, double[] x2) {
			residual_parameters_.clear();
			residual_parameters_.add(x0);
			residual_parameters_.add(x1);
			residual_parameters_.add(x2);
			return AddResidualBlock(cost_function, loss_function, residual_parameters_);
		}

		public ResidualBlock AddResidualBlock(CostFunction cost_function, LossFunction loss_function,
				Vector<double[]> parameter_blocks) {
			int i, j;
			if (cost_function == null) {
				System.err.println("cost_function is null in AddResidualBlock");
				return null;
			}
			if (parameter_blocks.size() != cost_function.parameter_block_sizes().size()) {
				System.err.println(
						"parameter_blocks.size() != cost_function.parameter_block_sizes().size() in AddResidualBlock");
				System.err.println("parameter_blocks_size() = " + parameter_blocks.size());
				System.err.println("cost_function.parameter_block_sizes().size() = "
						+ cost_function.parameter_block_sizes().size());

				return null;
			}

			// Check the sizes match.
			Vector<Integer> parameter_block_sizes = cost_function.parameter_block_sizes();

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
				for (j = i + 1; j < parameter_blocks.size(); j++) {
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
						InternalAddParameterBlock(parameter_blocks.get(i), parameter_block_sizes.get(i)));
			}

			if (!options_.disable_all_safety_checks) {
				// Check that the block sizes match the block sizes expected by the
				// cost_function.
				for (i = 0; i < parameter_block_ptrs.size(); ++i) {
					if (cost_function.parameter_block_sizes().get(i) != parameter_block_ptrs.get(i).Size()) {
						System.err.println("The cost function expects parameter block " + i);
						System.err.println(" of size " + cost_function.parameter_block_sizes().get(i));
						System.err.println(" but was given a block of size " + parameter_block_ptrs.get(i).Size());
					}
				}
			}

			ResidualBlock new_residual_block = new ResidualBlock(cost_function, loss_function, parameter_block_ptrs,
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
				if (cost_function_ref_count_.get(cost_function) == null) {
					cost_function_ref_count_.put(cost_function, 1);
				} else {
					count = cost_function_ref_count_.get(cost_function);
					cost_function_ref_count_.put(cost_function, count + 1);
				}
			}

			if (options_.loss_function_ownership == Ownership.TAKE_OWNERSHIP && loss_function != null) {
				if (loss_function_ref_count_.get(loss_function) == null) {
					loss_function_ref_count_.put(loss_function, 1);
				} else {
					count = loss_function_ref_count_.get(loss_function);
					loss_function_ref_count_.put(loss_function, count + 1);
				}
			}

			return new_residual_block;

		}

		ParameterBlock InternalAddParameterBlock(double[] values, int size) {

			if (values == null) {
				System.err.println("Null pointer passed to AddParameterBlock for a parameter with size " + size);
				return null;
			}

			// Ignore the request if there is a block for the given pointer already.
			if (parameter_block_map_.containsKey(values)) {
				if (!options_.disable_all_safety_checks) {
					int existing_size = parameter_block_map_.get(values).Size();
					if (size != existing_size) {
						System.err.println("Tried adding a parameter block with the same double[], ");
						System.err.println(" twice, but with different block sizes. Original ");
						System.err.println("size was " + existing_size + " but new size is " + size);
						return null;
					}
				}
				return parameter_block_map_.get(values);
			}

			// Pass the index of the new parameter block as well to keep the index in
			// sync with the position of the parameter in the program's parameter vector.
			ParameterBlock new_parameter_block = new ParameterBlock(values, size, program_.parameter_blocks_.size());

			// For dynamic problems, add the list of dependent residual blocks, which is
			// empty to start.
			if (options_.enable_fast_removal) {
				new_parameter_block.EnableResidualBlockDependencies();
			}
			parameter_block_map_.put(values, new_parameter_block);
			program_.parameter_blocks_.add(new_parameter_block);
			return new_parameter_block;

		}

		public HashMap<double[], ParameterBlock> parameter_map() {
			return parameter_block_map_;
		}
		
		public void SetParameterLowerBound(double[] values,
                int index,
                double lower_bound) {
			ParameterBlock parameter_block = parameter_block_map_.get(values);
			if (parameter_block == null) {
			    System.err.println("Parameter block not found for values[] with values[0] = " + values[0]);
			    System.err.println("You must add the parameter block to the problem before ");
		        System.err.println("you can set a lower bound on one of its components.");
			}
			
			    parameter_block.SetLowerBound(index, lower_bound);
			}
			
			public void SetParameterUpperBound(double[] values,
			                int index,
			                double upper_bound) {
				ParameterBlock parameter_block = parameter_block_map_.get(values);
				if (parameter_block == null) {
				    System.err.println("Parameter block not found for values[] with values[0] = " + values[0]);
				    System.err.println("You must add the parameter block to the problem before ");
			        System.err.println("you can set an upper bound on one of its components.");
				}
			
			    parameter_block.SetUpperBound(index, upper_bound);
			}
			
			public int NumParameterBlocks() {
		        return program_.NumParameterBlocks();
			}

		    public int NumParameters() {
		        return program_.NumParameters();
			}

			public int NumResidualBlocks() {
				return program_.NumResidualBlocks();
			}

			public int NumResiduals() {
				return program_.NumResiduals();
			}
			
			public CostFunction GetCostFunctionForResidualBlock(ResidualBlock residual_block) {
				return residual_block.cost_function();
		    }

		    public LossFunction GetLossFunctionForResidualBlock(ResidualBlock residual_block) {
				  return residual_block.loss_function();
 			}

		    public void RemoveResidualBlock(ResidualBlock residual_block) {
		    	  if (residual_block == null) {
		    		  System.err.println("In ProblemImpl.removeResidualBlock residual_block == null");
		    		  return;
		    	  }

		    	  // Verify that residual_block identifies a residual in the current problem.
		    	  final String residual_not_found_message =
		    	      "Residual block to remove not found. This usually means " +
		    	                   "one of three things have happened:\n" +
		    	                   " 1) residual_block is uninitialised and points to a random " +
		    	                   "area in memory.\n" +
		    	                   " 2) residual_block represented a residual that was added to" +
		    	                   " the problem, but referred to a parameter block which has\n " +
		    	                   "since been removed, which removes all residuals which " +
		    	                   "depend on that parameter block, and was thus removed.\n" +
		    	                   " 3) residual_block referred to a residual that has already " +
		    	                   "been removed from the problem (by the user).";
		    	  if (options_.enable_fast_removal) {
		    		if (!residual_block_set_.contains(residual_block)) {
		    			System.err.println(residual_not_found_message);
		    			return;
		    		}
		    	  } else {
		    	    // Perform a full search over all current residuals.
		    		  if (!program_.residual_blocks().contains(residual_block)) {
		    			  System.err.println(residual_not_found_message);
			    			return;
		    		  }
		    	  }

		    	  InternalRemoveResidualBlock(residual_block);
		    	}
		    
		    public void InternalRemoveResidualBlock(ResidualBlock residual_block) {
		    	if (residual_block == null) {
		    		  System.err.println("In ProblemImpl.InternalRemoveResidualBlock residual_block == null");
		    		  return;
		    	  }
		    	 
		    	  // Perform no check on the validity of residual_block, that is handled in
		    	  // the public method: RemoveResidualBlock().

		    	  // If needed, remove the parameter dependencies on this residual block.
		    	  if (options_.enable_fast_removal) {
		    	    final int num_parameter_blocks_for_residual =
		    	        residual_block.NumParameterBlocks();
		    	    for (int i = 0; i < num_parameter_blocks_for_residual; ++i) {
		    	      residual_block.parameter_blocks()[i].RemoveResidualBlock(residual_block);
		    	    }

		    	    residual_block_set_.remove(residual_block);
		    	  }
		    	  DeleteBlockInVector(program_.mutable_residual_blocks(), residual_block);
		    	}

		 // Delete a block from a vector of blocks, maintaining the indexing invariant.
		 // This is done in constant time by moving an element from the end of the
		 // vector over the element to remove, then popping the last element. It
		 // destroys the ordering in the interest of speed.
		 public void DeleteBlockInVector(Vector<ResidualBlock> mutable_blocks,
		                                       ResidualBlock block_to_remove) {
		   //CHECK_EQ((*mutable_blocks)[block_to_remove->index()], block_to_remove)
		       //<< "You found a Ceres bug! \n"
		       //<< "Block requested: "
		       //<< block_to_remove->ToString() << "\n"
		       //<< "Block present: "
		       //<< (*mutable_blocks)[block_to_remove->index()]->ToString();

		   // Prepare the to-be-moved block for the new, lower-in-index position by
		   // setting the index to the blocks final location.
		   int index = mutable_blocks.indexOf(block_to_remove);
		   ResidualBlock tmp = mutable_blocks.lastElement();
		   mutable_blocks.set(index, tmp);
		   DeleteBlock(block_to_remove);
		   mutable_blocks.remove(mutable_blocks.size()-1);
		 }
		 
		// Deletes the residual block in question, assuming there are no other
		// references to it inside the problem (e.g. by another parameter). Referenced
		// cost and loss functions are tucked away for future deletion, since it is not
		// possible to know whether other parts of the problem depend on them without
		// doing a full scan.
		public void DeleteBlock(ResidualBlock residual_block) {
		  // The const casts here are legit, since ResidualBlock holds these
		  // pointers as const pointers but we have ownership of them and
		  // have the right to destroy them when the destructor is called.
		  CostFunction cost_function = residual_block.cost_function();
		  if (options_.cost_function_ownership == Ownership.TAKE_OWNERSHIP) {
			int value = cost_function_ref_count_.get(cost_function);
			if (value > 1) {
				cost_function_ref_count_.put(cost_function, value-1);

			}
			else {
				cost_function_ref_count_.remove(cost_function);
				cost_function.finalize();
				cost_function = null;
			}
		  }

		  LossFunction loss_function = residual_block.loss_function();
		  if (options_.loss_function_ownership == Ownership.TAKE_OWNERSHIP &&
		      loss_function !=null) {
			  int value = loss_function_ref_count_.get(loss_function);
				if (value > 1) {
					loss_function_ref_count_.put(loss_function, value-1);
				}
				else {
					loss_function_ref_count_.remove(loss_function);
					loss_function.finalize();
					loss_function = null;
				}
		  }
		  residual_block = null;
		}
		
		public void DeleteBlockInVector(Vector<ParameterBlock> mutable_blocks,
                ParameterBlock block_to_remove) {
			//CHECK_EQ((*mutable_blocks)[block_to_remove->index()], block_to_remove)
			//<< "You found a Ceres bug! \n"
			//<< "Block requested: "
			//<< block_to_remove->ToString() << "\n"
			//<< "Block present: "
			//<< (*mutable_blocks)[block_to_remove->index()]->ToString();
			
			// Prepare the to-be-moved block for the new, lower-in-index position by
			// setting the index to the blocks final location.
			int index = mutable_blocks.indexOf(block_to_remove);
		    ParameterBlock tmp = mutable_blocks.lastElement();
			mutable_blocks.set(index, tmp);
			DeleteBlock(block_to_remove);
			mutable_blocks.remove(mutable_blocks.size()-1);
		}
		
		// Deletes the parameter block in question, assuming there are no other
		// references to it inside the problem (e.g. by any residual blocks).
		// Referenced parameterizations are tucked away for future deletion, since it
		// is not possible to know whether other parts of the problem depend on them
		// without doing a full scan.
		public void DeleteBlock(ParameterBlock parameter_block) {
		  if (options_.local_parameterization_ownership == Ownership.TAKE_OWNERSHIP &&
		      parameter_block.local_parameterization() != null) {
		    local_parameterizations_to_delete_.add(
		        parameter_block.mutable_local_parameterization());
		  }
		  parameter_block_map_.remove(parameter_block.mutable_user_state());
		  parameter_block = null;
		}



		public HashSet<ResidualBlock> residual_block_set() {
		    if (options_.enable_fast_removal) {
		        System.err.println("Fast removal not enabled, residual_block_set is not maintained.");
		        return null;
		    }
		    return residual_block_set_;
	    }
		
		public void RemoveParameterBlock(double[] values) {
			ParameterBlock parameter_block =
				      FindWithDefault(parameter_block_map_, values, null);
				  if (parameter_block == null) {
				    System.err.println("In RemoveParameterBlock Parameter block not found for supplied double[] values.");
				    System.err.println("You must add the parameter block to the problem before it can be removed.");
				    return;
				  }

			  if (options_.enable_fast_removal) {
			    // Copy the dependent residuals from the parameter block because the set of
			    // dependents will change after each call to RemoveResidualBlock().
			    Vector<ResidualBlock> residual_blocks_to_remove = new Vector<ResidualBlock>(parameter_block.mutable_residual_blocks().size());
			    residual_blocks_to_remove.addAll(parameter_block.mutable_residual_blocks());
			    for (int i = 0; i < residual_blocks_to_remove.size(); ++i) {
			      InternalRemoveResidualBlock(residual_blocks_to_remove.get(i));
			    }
			  } else {
			    // Scan all the residual blocks to remove ones that depend on the parameter
			    // block. Do the scan backwards since the vector changes while iterating.
			    final int num_residual_blocks = NumResidualBlocks();
			    for (int i = num_residual_blocks - 1; i >= 0; --i) {
			      ResidualBlock residual_block =
			          program_.mutable_residual_blocks().get(i);
			      final int num_parameter_blocks = residual_block.NumParameterBlocks();
			      for (int j = 0; j < num_parameter_blocks; ++j) {
			        if (residual_block.parameter_blocks()[j] == parameter_block) {
			          InternalRemoveResidualBlock(residual_block);
			          // The parameter blocks are guaranteed unique.
			          break;
			        }
			      }
			    }
			  }
			  DeleteBlockInVector(program_.mutable_parameter_blocks(), parameter_block);
			}

		public int ParameterBlockSize( double[] values) {
			ParameterBlock parameter_block =
				      FindWithDefault(parameter_block_map_, values, null);
				  if (parameter_block == null) {
				    System.err.println("In ParameterBlockSize Parameter block not found for supplied double[] values.");
				    System.err.println("You must add the parameter block to the problem before you can get its size.");
				    return -1;
				  }

			  return parameter_block.Size();
		}
		
		public int ParameterBlockLocalSize( double[] values) {
			ParameterBlock parameter_block =
				      FindWithDefault(parameter_block_map_, values, null);
				  if (parameter_block == null) {
				    System.err.println("In ParameterBlockLocalSize Parameter block not found for supplied double[] values.");
				    System.err.println("You must add the parameter block to the problem before you can get its local size.");
				    return -1;
				  }

			  return parameter_block.LocalSize();
		}
		
		public void GetParameterBlocks(Vector<double[]> parameter_blocks) {
			  if (parameter_blocks == null) {
				  System.err.println("In ProblemImpl GetParameterBlocks Vector<double[]> parameter_blocks = null");
				  return;
			  }
			  parameter_blocks.clear();
			  Set<double[]> keySet = parameter_block_map_.keySet();
			  parameter_blocks.addAll(keySet);
		}
		
		public boolean HasParameterBlock(double[] parameter_block) {
			  return (parameter_block_map_.containsKey(parameter_block));
		}




	} // class ProblemImpl

	class OrderedGroups<T> {
		// Use only double[] of single length as T.
		// Use structures like double x[][] = new double[3][1]
		private HashMap<Integer, Set<T>> group_to_elements_;
		private HashMap<T, Integer> element_to_group_;

		public OrderedGroups() {
			group_to_elements_ = new HashMap<Integer, Set<T>>();
			element_to_group_ = new HashMap<T, Integer>();
		}

		public HashMap<Integer, Set<T>> group_to_elements() {
			return group_to_elements_;
		}

		// Add an element to a group. If a group with this id does not
		// exist, one is created. This method can be called any number of
		// times for the same element. Group ids should be non-negative
		// numbers.
		//
		// Return value indicates if adding the element was a success.
		public boolean AddElementToGroup(T element, int group) {
			if (group < 0) {
				return false;
			}

			Integer value_element_to_group = element_to_group_.get(element);
			if (value_element_to_group != null) {
				if (value_element_to_group.intValue() == group) {
					// Element is already in the right group, nothing to do.
					return true;
				}

				if (group_to_elements_.containsKey(value_element_to_group)) {
					group_to_elements_.get(value_element_to_group).remove(element);
					if (group_to_elements_.get(value_element_to_group).size() == 0) {
						group_to_elements_.remove(value_element_to_group);
					}
				}
			}
			element_to_group_.put(element, group);
			if (group_to_elements_.get(group) == null) {
				group_to_elements_.put(group, new HashSet<T>());
			}
			group_to_elements_.get(group).add(element);
			return true;
		}

		// Number of groups with one or more elements.
		public int NumGroups() {
			return group_to_elements_.size();
		}

		// The first group with one or more elements. Calling this when
		// there are no groups with non-zero elements will result in a
		// crash.
		public int MinNonZeroGroup() {
			if (NumGroups() == 0) {
				System.err.println("In OrderedGroups.MinNonZeroGroup() == 0");
				return -1;
			}
			return ((Integer) group_to_elements_.keySet().toArray()[0]).intValue();
		}
		
		  // This function always succeeds, i.e., implicitly there exists a
		  // group for every integer.
		  public int GroupSize(int group) {
			  if (!group_to_elements_.containsKey(group)) {
				  return 0;
			  }
			  else {
				  return group_to_elements_.get(group).size();
			  }
		  }

		// Return the group id for the element. If the element is not a
		// member of any group, return -1.
		public int GroupId(T element) {
			Integer value = element_to_group_.get(element);
			if (value == null) {
				return -1;
			}
			return value.intValue();
		}

		// Remove the element, no matter what group it is in. Return value
		// indicates if the element was actually removed.
		public boolean Remove(T element) {
			int current_group = GroupId(element);
			if (current_group < 0) {
				return false;
			}

			group_to_elements_.get(current_group).remove(element);

			if (group_to_elements_.get(current_group).size() == 0) {
				// If the group is empty, then get rid of it.
				group_to_elements_.remove(current_group);
			}

			element_to_group_.remove(element);
			return true;
		}

		public int NumElements() {
			return element_to_group_.size();
		}

		// Bulk remove elements. The return value indicates the number of
		// elements successfully removed.
		public int Remove(Vector<T> elements) {
			if (NumElements() == 0 || elements.size() == 0) {
				return 0;
			}

			int num_removed = 0;
			for (int i = 0; i < elements.size(); ++i) {
				if (Remove(elements.get(i))) {
					num_removed++;
				}
			}
			return num_removed;
		}
		
		public boolean IsMember(T element) {
			return(element_to_group_.containsKey(element));
		}
		
		public void Clear() {
		    group_to_elements_.clear();
		    element_to_group_.clear();
		}
		
		// Reverse the order of the groups in place.
		  void Reverse() {
		    if (NumGroups() == 0) {
		      return;
		    }

		    HashMap<Integer, Set<T>> new_group_to_elements = new HashMap<Integer, Set<T>>();
		    Set<Map.Entry<Integer, Set<T>>> entrySet = group_to_elements_.entrySet();
		    Iterator<Map.Entry<Integer, Set<T>>> entry_it = entrySet.iterator();
		    Vector<Map.Entry<Integer, Set<T>>> vec = new Vector<Map.Entry<Integer, Set<T>>>();
		    Map.Entry<Integer, Set<T>> firstEntry;
		    while (entry_it.hasNext()) {
		    	vec.add(entry_it.next());
		    }
		    Collections.reverse(vec);
		    firstEntry = vec.get(0);
		    new_group_to_elements.put(firstEntry.getKey(), firstEntry.getValue());
		    
		    int new_group_id = firstEntry.getKey() + 1;
		    for (int it = 1; it < vec.size(); it++) {
		    	for (T element_it: vec.get(it).getValue()) {
		    		element_to_group_.put(element_it, new_group_id);
		    	}
		    	new_group_to_elements.put(new_group_id, vec.get(it).getValue());
		    	new_group_id++;
		    }

		    group_to_elements_.clear();
		    Set<Map.Entry<Integer, Set<T>>> entrySet2 = new_group_to_elements.entrySet();
		    Iterator<Map.Entry<Integer, Set<T>>> entry_it2 = entrySet2.iterator();
		    while (entry_it2.hasNext()) {
		        Map.Entry<Integer, Set<T>> currentEntry = entry_it2.next();
		    	group_to_elements_.put(currentEntry.getKey(), currentEntry.getValue());
		    }
		  }

	} // class OrderedGroups

	class Context {
		public Context() {

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

		public ResidualBlock(CostFunction cost_function, LossFunction loss_function,
				Vector<ParameterBlock> parameter_blocks, int index) {
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

		// The size of the residual vector returned by this residual function.
		public int NumResiduals() {
			return cost_function_.num_residuals();
		}

		// Number of variable blocks that this residual term depends on.
		public int NumParameterBlocks() {
			return cost_function_.parameter_block_sizes().size();
		}

		// Access the parameter blocks for this residual. The array has size
		// NumParameterBlocks().
		public ParameterBlock[] parameter_blocks() {
			return parameter_blocks_;
		}

		public CostFunction cost_function() {
			return cost_function_;
		}

		public LossFunction loss_function() {
			return loss_function_;
		}

		public int NumScratchDoublesForEvaluate() {
			// Compute the amount of scratch space needed to store the full-sized
			// jacobians. For parameters that have no local parameterization no storage
			// is needed and the passed-in jacobian array is used directly. Also include
			// space to store the residuals, which is needed for cost-only evaluations.
			// This is slightly pessimistic, since both won't be needed all the time, but
			// the amount of excess should not cause problems for the caller.
			int num_parameters = NumParameterBlocks();
			int scratch_doubles = 1;
			for (int i = 0; i < num_parameters; ++i) {
				ParameterBlock parameter_block = parameter_blocks_[i];
				if (!parameter_block.IsConstant() && parameter_block.LocalParameterizationJacobian() != null) {
					scratch_doubles += parameter_block.Size();
				}
			}
			scratch_doubles *= NumResiduals();
			return scratch_doubles;
		}

		public boolean Evaluate(boolean apply_loss_function, double cost[], double residuals[], double jacobians[][],
				double scratch[]) {
			int state_finish;
			int blockLength;
			double block[];
			int num_parameter_blocks = NumParameterBlocks();
			int num_residuals = cost_function_.num_residuals();

			// Collect the parameters from their blocks. This will rarely allocate, since
			// residuals taking more than 8 parameter block arguments are rare.
			Vector<double[]> parameters = new Vector<double[]>();
			// parameter_blocks_[i] will often have all the same variables but with different state_start values
			for (int i = 0; i < num_parameter_blocks; ++i) {
				int state_start = parameter_blocks_[i].state_start();
				if (i < num_parameter_blocks-1) {
				    state_finish = parameter_blocks_[i+1].state_start() - 1;
				    if (state_finish < state_start) {
				    	state_finish = parameter_blocks_[i].state().length-1;	
				    }
				}
				else {
					state_finish = parameter_blocks_[i].state().length-1;
				}
				blockLength = state_finish - state_start + 1;
				block = new double[blockLength];
				for (int j = state_start; j <= state_finish; j++) {
					block[j-state_start] = parameter_blocks_[i].state()[j];
				}
				parameters.add(block);
			}

			// Put pointers into the scratch space into global_jacobians as appropriate.
			double[][] global_jacobians = new double[num_parameter_blocks][];
			int scratchNum = 0;
			Vector<Integer> scratchSize = new Vector<Integer>();
			if (jacobians != null) {
				for (int i = 0; i < num_parameter_blocks; ++i) {
					ParameterBlock parameter_block = parameter_blocks_[i];
					if (jacobians[i] != null && parameter_block.LocalParameterizationJacobian() != null) {
						scratchNum++;
						scratchSize.add(num_residuals * parameter_block.Size());
					}
				}
				double scratch2D[][] = new double[scratchNum][];
				for (int i = 0; i < scratchNum; i++) {
					scratch2D[i] = new double[scratchSize.get(i)];
				}
				for (int i = 0, j = 0; i < num_parameter_blocks; ++i) {
					ParameterBlock parameter_block = parameter_blocks_[i];
					if (jacobians[i] != null && parameter_block.LocalParameterizationJacobian() != null) {
						// global_jacobians[i] = scratch;
						// scratch += num_residuals * parameter_block.Size();
						global_jacobians[i] = scratch2D[j++];
					} else {
						global_jacobians[i] = jacobians[i];
					}
				}
			}

			// If the caller didn't request residuals, use the scratch space for them.
			boolean outputting_residuals = (residuals != null);
			if (!outputting_residuals) {
				residuals = scratch;
			}

			// Invalidate the evaluation buffers so that we can check them after
			// the CostFunction::Evaluate call, to see if all the return values
			// that were required were written to and that they are finite.
			double[][] eval_jacobians = (jacobians != null) ? global_jacobians : null;

			InvalidateEvaluation(cost, residuals, eval_jacobians);

			if (!cost_function_.Evaluate(parameters, residuals, eval_jacobians)) {
				return false;
			}

			if (!IsEvaluationValid(parameters, cost, residuals, eval_jacobians)) {
				String message = "\n\n" + "Error in evaluating the ResidualBlock.\n\n"
						+ "There are two possible reasons. Either the CostFunction did not evaluate and fill all    \n"
						+ "residual and jacobians that were requested or there was a non-finite value (nan/infinite)\n"
						+ "generated during the or jacobian computation. \n\n"
						+ EvaluationToString(parameters, cost, residuals, eval_jacobians);
				System.err.println(message);
				return false;
			}

			double squared_norm = 0.0;
			for (int i = 0; i < num_residuals; i++) {
				squared_norm += (residuals[i] * residuals[i]);
			}

			// Update the jacobians with the local parameterizations.
			if (jacobians != null) {
				for (int i = 0; i < num_parameter_blocks; ++i) {
					if (jacobians[i] != null) {
						ParameterBlock parameter_block = parameter_blocks_[i];

						// Apply local reparameterization to the jacobians.
						if (parameter_block.LocalParameterizationJacobian() != null) {
							// jacobians[i] = global_jacobians[i] * global_to_local_jacobian.
							MatrixMatrixMultiply(0, global_jacobians[i], num_residuals, parameter_block.Size(),
									parameter_block.LocalParameterizationJacobian(), parameter_block.Size(),
									parameter_block.LocalSize(), jacobians[i], 0, 0, num_residuals,
									parameter_block.LocalSize());
						}
					}
				}
			}

			if (loss_function_ == null || !apply_loss_function) {
				cost[0] = 0.5 * squared_norm;
				return true;
			}

			double rho[] = new double[3];
			loss_function_.Evaluate(squared_norm, rho);
			cost[0] = 0.5 * rho[0];

			// No jacobians and not outputting residuals? All done. Doing an early exit
			// here avoids constructing the "Corrector" object below in a common case.
			if (jacobians == null && !outputting_residuals) {
				return true;
			}

			// Correct for the effects of the loss function. The jacobians need to be
			// corrected before the residuals, since they use the uncorrected residuals.
			Corrector correct = new Corrector(squared_norm, rho);
			if (jacobians != null) {
				for (int i = 0; i < num_parameter_blocks; ++i) {
					if (jacobians[i] != null) {
						ParameterBlock parameter_block = parameter_blocks_[i];

						// Correct the jacobians for the loss function.
						correct.CorrectJacobian(num_residuals, parameter_block.LocalSize(), residuals, jacobians[i]);
					}
				}
			}

			// Correct the residuals with the loss function.
			if (outputting_residuals) {
				correct.CorrectResiduals(num_residuals, residuals);
			}
			return true;
		}
		
		public boolean Evaluate(boolean apply_loss_function, double cost[], double residuals[], double jacobians[][],
				int jacobians_offset[], double scratch[]) {
			int state_finish;
			int blockLength;
			double block[];
			int num_parameter_blocks = NumParameterBlocks();
			int num_residuals = cost_function_.num_residuals();

			// Collect the parameters from their blocks. This will rarely allocate, since
			// residuals taking more than 8 parameter block arguments are rare.
			Vector<double[]> parameters = new Vector<double[]>();
			// parameter_blocks_[i] will often have all the same variables but with different state_start values
			for (int i = 0; i < num_parameter_blocks; ++i) {
				int state_start = parameter_blocks_[i].state_start();
				if (i < num_parameter_blocks-1) {
				    state_finish = parameter_blocks_[i+1].state_start() - 1;
				    if (state_finish < state_start) {
				    	state_finish = parameter_blocks_[i].state().length-1;	
				    }
				}
				else {
					state_finish = parameter_blocks_[i].state().length-1;
				}
				blockLength = state_finish - state_start + 1;
				block = new double[blockLength];
				for (int j = state_start; j <= state_finish; j++) {
					block[j-state_start] = parameter_blocks_[i].state()[j];
				}
				parameters.add(block);
			}

			// Put pointers into the scratch space into global_jacobians as appropriate.
			double[][] global_jacobians = new double[num_parameter_blocks][];
			int global_jacobians_offset[] = new int[num_parameter_blocks];
			int scratchNum = 0;
			Vector<Integer> scratchSize = new Vector<Integer>();
			if (jacobians != null) {
				for (int i = 0; i < num_parameter_blocks; ++i) {
					ParameterBlock parameter_block = parameter_blocks_[i];
					if (jacobians[i] != null && parameter_block.LocalParameterizationJacobian() != null) {
						scratchNum++;
						scratchSize.add(num_residuals * parameter_block.Size());
					}
				}
				double scratch2D[][] = new double[scratchNum][];
				for (int i = 0; i < scratchNum; i++) {
					scratch2D[i] = new double[scratchSize.get(i)];
				}
				for (int i = 0, j = 0; i < num_parameter_blocks; ++i) {
					ParameterBlock parameter_block = parameter_blocks_[i];
					if (jacobians[i] != null && parameter_block.LocalParameterizationJacobian() != null) {
						// global_jacobians[i] = scratch;
						// scratch += num_residuals * parameter_block.Size();
						global_jacobians[i] = scratch2D[j++];
						global_jacobians_offset[i] = 0;
					} else {
						global_jacobians[i] = jacobians[i];
						global_jacobians_offset[i] = jacobians_offset[i];
					}
				}
			}

			// If the caller didn't request residuals, use the scratch space for them.
			boolean outputting_residuals = (residuals != null);
			if (!outputting_residuals) {
				residuals = scratch;
			}

			// Invalidate the evaluation buffers so that we can check them after
			// the CostFunction::Evaluate call, to see if all the return values
			// that were required were written to and that they are finite.
			double[][] eval_jacobians = (jacobians != null) ? global_jacobians : null;
			int[] eval_jacobians_offset = null;
			if (jacobians != null) {
				eval_jacobians_offset = new int[global_jacobians.length];
				for (int i = 0; i < global_jacobians.length; i++) {
					eval_jacobians_offset[i] = global_jacobians_offset[i];
				}
			}

			InvalidateEvaluation(cost, residuals, eval_jacobians, eval_jacobians_offset);

			if (!cost_function_.Evaluate(parameters, residuals, eval_jacobians, eval_jacobians_offset)) {
				return false;
			}

			if (!IsEvaluationValid(parameters, cost, residuals, eval_jacobians, eval_jacobians_offset)) {
				String message = "\n\n" + "Error in evaluating the ResidualBlock.\n\n"
						+ "There are two possible reasons. Either the CostFunction did not evaluate and fill all    \n"
						+ "residual and jacobians that were requested or there was a non-finite value (nan/infinite)\n"
						+ "generated during the or jacobian computation. \n\n"
						+ EvaluationToString(parameters, cost, residuals, eval_jacobians, eval_jacobians_offset);
				System.err.println(message);
				return false;
			}

			double squared_norm = 0.0;
			for (int i = 0; i < num_residuals; i++) {
				squared_norm += (residuals[i] * residuals[i]);
			}

			// Update the jacobians with the local parameterizations.
			if (jacobians != null) {
				for (int i = 0; i < num_parameter_blocks; ++i) {
					if (jacobians[i] != null) {
						ParameterBlock parameter_block = parameter_blocks_[i];

						// Apply local reparameterization to the jacobians.
						if (parameter_block.LocalParameterizationJacobian() != null) {
							// jacobians[i] = global_jacobians[i] * global_to_local_jacobian.
							MatrixMatrixMultiply(0, global_jacobians[i], global_jacobians_offset[i], num_residuals, parameter_block.Size(),
									parameter_block.LocalParameterizationJacobian(), parameter_block.Size(),
									parameter_block.LocalSize(), jacobians[i], jacobians_offset[i], 0, 0, num_residuals,
									parameter_block.LocalSize());
						}
					}
				}
			}

			if (loss_function_ == null || !apply_loss_function) {
				cost[0] = 0.5 * squared_norm;
				return true;
			}

			double rho[] = new double[3];
			loss_function_.Evaluate(squared_norm, rho);
			cost[0] = 0.5 * rho[0];

			// No jacobians and not outputting residuals? All done. Doing an early exit
			// here avoids constructing the "Corrector" object below in a common case.
			if (jacobians == null && !outputting_residuals) {
				return true;
			}

			// Correct for the effects of the loss function. The jacobians need to be
			// corrected before the residuals, since they use the uncorrected residuals.
			Corrector correct = new Corrector(squared_norm, rho);
			if (jacobians != null) {
				for (int i = 0; i < num_parameter_blocks; ++i) {
					if (jacobians[i] != null) {
						ParameterBlock parameter_block = parameter_blocks_[i];

						// Correct the jacobians for the loss function.
						correct.CorrectJacobian(num_residuals, parameter_block.LocalSize(), residuals, jacobians[i], jacobians_offset[i]);
					}
				}
			}

			// Correct the residuals with the loss function.
			if (outputting_residuals) {
				correct.CorrectResiduals(num_residuals, residuals);
			}
			return true;
		}

		public void InvalidateEvaluation(double cost[], double residuals[], double[][] jacobians) {
			int num_parameter_blocks = NumParameterBlocks();
			int num_residuals = NumResiduals();

			InvalidateArray(1, cost);
			InvalidateArray(num_residuals, residuals);
			if (jacobians != null) {
				for (int i = 0; i < num_parameter_blocks; ++i) {
					int parameter_block_size = parameter_blocks()[i].Size();
					InvalidateArray(num_residuals * parameter_block_size, jacobians[i]);
				}
			}
		}
		
		public void InvalidateEvaluation(double cost[], double residuals[], double[][] jacobians, int jacobians_offset[]) {
			int num_parameter_blocks = NumParameterBlocks();
			int num_residuals = NumResiduals();

			InvalidateArray(1, cost);
			InvalidateArray(num_residuals, residuals);
			if (jacobians != null) {
				for (int i = 0; i < num_parameter_blocks; ++i) {
					int parameter_block_size = parameter_blocks()[i].Size();
					InvalidateArray(num_residuals * parameter_block_size, jacobians[i], jacobians_offset[i]);
				}
			}
		}

		public boolean IsEvaluationValid(Vector<double[]> parameters, double cost[], double residuals[],
				double jacobians[][]) {
			int num_parameter_blocks = NumParameterBlocks();
			int num_residuals = NumResiduals();

			if (!IsArrayValid(num_residuals, residuals)) {
				return false;
			}

			if (jacobians != null) {
				for (int i = 0; i < num_parameter_blocks; ++i) {
					int parameter_block_size = parameter_blocks()[i].Size();
					if (!IsArrayValid(num_residuals * parameter_block_size, jacobians[i])) {
						return false;
					}
				}
			}

			return true;
		}
		
		public boolean IsEvaluationValid(Vector<double[]> parameters, double cost[], double residuals[],
				double jacobians[][], int jacobians_offset[]) {
			int num_parameter_blocks = NumParameterBlocks();
			int num_residuals = NumResiduals();

			if (!IsArrayValid(num_residuals, residuals)) {
				return false;
			}

			if (jacobians != null) {
				for (int i = 0; i < num_parameter_blocks; ++i) {
					int parameter_block_size = parameter_blocks()[i].Size();
					if (!IsArrayValid(num_residuals * parameter_block_size, jacobians[i], jacobians_offset[i])) {
						return false;
					}
				}
			}

			return true;
		}

		public String EvaluationToString(Vector<double[]> parameters, double cost[], double residuals[],
				double jacobians[][]) {
			if (cost == null) {
				return null;
			}
			if (residuals == null) {
				return null;
			}

			int num_parameter_blocks = NumParameterBlocks();
			int num_residuals = NumResiduals();
			String result[] = new String[1];

			result[0] = "Residual Block size: " + num_parameter_blocks + " parameter blocks x " + num_residuals
					+ " residuals\n\n"
					+ "For each parameter block, the value of the parameters are printed in the first column   \n"
					+ "and the value of the jacobian under the corresponding residual. If a ParameterBlock was \n"
					+ "held constant then the corresponding jacobian is printed as 'Not Computed'. If an entry \n"
					+ "of the Jacobian/residual array was requested but was not written to by user code, it is \n"
					+ "indicated by 'Uninitialized'. This is an error. Residuals or Jacobian values evaluating \n"
					+ "to Inf or NaN is also an error.  \n\n";

			String space = "Residuals:     ";
			result[0] += space;
			for (int i = 0; i < num_residuals; i++) {
				result[0] = result[0] + "\n" + residuals[i];
			}
			result[0] += "\n\n";

			for (int i = 0; i < num_parameter_blocks; ++i) {
				int parameter_block_size = parameter_blocks()[i].Size();
				result[0] = result[0] + "Parameter Block " + i + ", size: " + parameter_block_size + "\n";
				result[0] = result[0] + "\n";
				for (int j = 0; j < parameter_block_size; ++j) {
					double x[] = new double[] { parameters.get(i)[j] };
					AppendArrayToString(1, x, result);
					result[0] = result[0] + "| ";
					for (int k = 0; k < num_residuals; ++k) {
						if (jacobians != null && jacobians[i] != null) {
							double y[] = new double[] { jacobians[i][k * parameter_block_size + j] };
							AppendArrayToString(1, y, result);
						}
					}
					result[0] = result[0] + "\n";
				}
				result[0] = result[0] + "\n";
			}
			result[0] = result[0] + "\n";
			return result[0];
		}
		
		public String EvaluationToString(Vector<double[]> parameters, double cost[], double residuals[],
				double jacobians[][], int jacobians_offset[]) {
			if (cost == null) {
				return null;
			}
			if (residuals == null) {
				return null;
			}

			int num_parameter_blocks = NumParameterBlocks();
			int num_residuals = NumResiduals();
			String result[] = new String[1];

			result[0] = "Residual Block size: " + num_parameter_blocks + " parameter blocks x " + num_residuals
					+ " residuals\n\n"
					+ "For each parameter block, the value of the parameters are printed in the first column   \n"
					+ "and the value of the jacobian under the corresponding residual. If a ParameterBlock was \n"
					+ "held constant then the corresponding jacobian is printed as 'Not Computed'. If an entry \n"
					+ "of the Jacobian/residual array was requested but was not written to by user code, it is \n"
					+ "indicated by 'Uninitialized'. This is an error. Residuals or Jacobian values evaluating \n"
					+ "to Inf or NaN is also an error.  \n\n";

			String space = "Residuals:     ";
			result[0] += space;
			for (int i = 0; i < num_residuals; i++) {
				result[0] = result[0] + "\n" + residuals[i];
			}
			result[0] += "\n\n";

			for (int i = 0; i < num_parameter_blocks; ++i) {
				int parameter_block_size = parameter_blocks()[i].Size();
				result[0] = result[0] + "Parameter Block " + i + ", size: " + parameter_block_size + "\n";
				result[0] = result[0] + "\n";
				for (int j = 0; j < parameter_block_size; ++j) {
					double x[] = new double[] { parameters.get(i)[j] };
					AppendArrayToString(1, x, result);
					result[0] = result[0] + "| ";
					for (int k = 0; k < num_residuals; ++k) {
						if (jacobians != null && jacobians[i] != null) {
							double y[] = new double[] { jacobians[i][jacobians_offset[i] + k * parameter_block_size + j] };
							AppendArrayToString(1, y, result);
						}
					}
					result[0] = result[0] + "\n";
				}
				result[0] = result[0] + "\n";
			}
			result[0] = result[0] + "\n";
			return result[0];
		}

	} // class ResidualBlock

	class Corrector {
		private double sqrt_rho1_;
		private double residual_scaling_;
		private double alpha_sq_norm_;

		public Corrector(double sq_norm, double rho[]) {
			if (sq_norm < 0.0) {
				System.err.println("sq_norm < 0.0 in public Corrector(double sq_norm, double rho[])");
				return;
			}
			sqrt_rho1_ = Math.sqrt(rho[1]);

			// If sq_norm = 0.0, the correction becomes trivial, the residual
			// and the jacobian are scaled by the squareroot of the derivative
			// of rho. Handling this case explicitly avoids the divide by zero
			// error that would occur below.
			//
			// The case where rho'' < 0 also gets special handling. Technically
			// it shouldn't, and the computation of the scaling should proceed
			// as below, however we found in experiments that applying the
			// curvature correction when rho'' < 0, which is the case when we
			// are in the outlier region slows down the convergence of the
			// algorithm significantly.
			//
			// Thus, we have divided the action of the robustifier into two
			// parts. In the inliner region, we do the full second order
			// correction which re-wights the gradient of the function by the
			// square root of the derivative of rho, and the Gauss-Newton
			// Hessian gets both the scaling and the rank-1 curvature
			// correction. Normaly, alpha is upper bounded by one, but with this
			// change, alpha is bounded above by zero.
			//
			// Empirically we have observed that the full Triggs correction and
			// the clamped correction both start out as very good approximations
			// to the loss function when we are in the convex part of the
			// function, but as the function starts transitioning from convex to
			// concave, the Triggs approximation diverges more and more and
			// ultimately becomes linear. The clamped Triggs model however
			// remains quadratic.
			//
			// The reason why the Triggs approximation becomes so poor is
			// because the curvature correction that it applies to the gauss
			// newton hessian goes from being a full rank correction to a rank
			// deficient correction making the inversion of the Hessian fraught
			// with all sorts of misery and suffering.
			//
			// The clamped correction retains its quadratic nature and inverting it
			// is always well formed.
			if ((sq_norm == 0.0) || (rho[2] <= 0.0)) {
				residual_scaling_ = sqrt_rho1_;
				alpha_sq_norm_ = 0.0;
				return;
			}

			// We now require that the first derivative of the loss function be
			// positive only if the second derivative is positive. This is
			// because when the second derivative is non-positive, we do not use
			// the second order correction suggested by BANS and instead use a
			// simpler first order strategy which does not use a division by the
			// gradient of the loss function.
			if (rho[1] <= 0.0) {
				System.err.println("rho[1] <= 0.0 in public Corrector(double sq_norm, double rho[])");
				return;
			}

			// Calculate the smaller of the two solutions to the equation
			//
			// 0.5 * alpha^2 - alpha - rho'' / rho' * z'z = 0.
			//
			// Start by calculating the discriminant D.
			double D = 1.0 + 2.0 * sq_norm * rho[2] / rho[1];

			// Since both rho[1] and rho[2] are guaranteed to be positive at
			// this point, we know that D > 1.0.

			double alpha = 1.0 - Math.sqrt(D);

			// Calculate the constants needed by the correction routines.
			residual_scaling_ = sqrt_rho1_ / (1 - alpha);
			alpha_sq_norm_ = alpha / sq_norm;
		}

		public void CorrectResiduals(int num_rows, double residuals[]) {
			int i;
			if (residuals == null) {
				System.err.println("residuals == null in Corrector.CorrectResiduals");
				return;
			}
			// Equation 11 in BANS.
			for (i = 0; i < num_rows; i++) {
				residuals[i] *= residual_scaling_;
			}
		}

		public void CorrectJacobian(int num_rows, int num_cols, double residuals[], double jacobian[]) {
			int i;
			if (residuals == null) {
				System.err.println("residuals == null in Corrector.CorrectJacobian");
				return;
			}
			if (jacobian == null) {
				System.err.println("jacobian == null in Corrector.CorrectJacobian");
				return;
			}

			// The common case (rho[2] <= 0).
			if (alpha_sq_norm_ == 0.0) {
				for (i = 0; i < num_rows * num_cols; i++) {
					jacobian[i] *= sqrt_rho1_;
				}
				return;
			}

			// Equation 11 in BANS.
			//
			// J = sqrt(rho) * (J - alpha^2 r * r' J)
			//
			// In days gone by this loop used to be a single Eigen expression of
			// the form
			//
			// J = sqrt_rho1_ * (J - alpha_sq_norm_ * r* (r.transpose() * J));
			//
			// Which turns out to about 17x slower on bal problems. The reason
			// is that Eigen is unable to figure out that this expression can be
			// evaluated columnwise and ends up creating a temporary.
			for (int c = 0; c < num_cols; ++c) {
				double r_transpose_j = 0.0;
				for (int r = 0; r < num_rows; ++r) {
					r_transpose_j += jacobian[r * num_cols + c] * residuals[r];
				}

				for (int r = 0; r < num_rows; ++r) {
					jacobian[r * num_cols + c] = sqrt_rho1_
							* (jacobian[r * num_cols + c] - alpha_sq_norm_ * residuals[r] * r_transpose_j);
				}
			}
		}
		
		public void CorrectJacobian(int num_rows, int num_cols, double residuals[], double jacobian[], int jacobian_offset) {
			int i;
			if (residuals == null) {
				System.err.println("residuals == null in Corrector.CorrectJacobian");
				return;
			}
			if (jacobian == null) {
				System.err.println("jacobian == null in Corrector.CorrectJacobian");
				return;
			}

			// The common case (rho[2] <= 0).
			if (alpha_sq_norm_ == 0.0) {
				for (i = 0; i < num_rows * num_cols; i++) {
					jacobian[jacobian_offset+i] *= sqrt_rho1_;
				}
				return;
			}

			// Equation 11 in BANS.
			//
			// J = sqrt(rho) * (J - alpha^2 r * r' J)
			//
			// In days gone by this loop used to be a single Eigen expression of
			// the form
			//
			// J = sqrt_rho1_ * (J - alpha_sq_norm_ * r* (r.transpose() * J));
			//
			// Which turns out to about 17x slower on bal problems. The reason
			// is that Eigen is unable to figure out that this expression can be
			// evaluated columnwise and ends up creating a temporary.
			for (int c = 0; c < num_cols; ++c) {
				double r_transpose_j = 0.0;
				for (int r = 0; r < num_rows; ++r) {
					r_transpose_j += jacobian[jacobian_offset + r * num_cols + c] * residuals[r];
				}

				for (int r = 0; r < num_rows; ++r) {
					jacobian[jacobian_offset + r * num_cols + c] = sqrt_rho1_
							* (jacobian[jacobian_offset + r * num_cols + c] - alpha_sq_norm_ * residuals[r] * r_transpose_j);
				}
			}
		}

	} // class Corrector

	class ParameterBlock {
		int i, j;
		private double[] user_state_;
		private int size_;
		boolean is_constant_;
		LocalParameterization local_parameterization_;
		// If non-null, contains the residual blocks this parameter block is in.
		HashSet<ResidualBlock> residual_blocks_;

		// The "state" of the parameter. These fields are only needed while the
		// solver is running. While at first glance using mutable is a bad idea, this
		// ends up simplifying the internals of Ceres enough to justify the potential
		// pitfalls of using "mutable."
		double[] state_;
		int state_start = 0;
		double local_parameterization_jacobian_[][];
		// mutable scoped_array<double> local_parameterization_jacobian_;

		// The index of the parameter. This is used by various other parts of Ceres to
		// permit switching from a ParameterBlock* to an index in another array.
		int index_;

		// The offset of this parameter block inside a larger state vector.
		int state_offset_;

		// The offset of this parameter block inside a larger delta vector.
		int delta_offset_;

		// Upper and lower bounds for the parameter block. SetUpperBound
		// and SetLowerBound lazily initialize the upper_bounds_ and
		// lower_bounds_ arrays. If they are never called, then memory for
		// these arrays is never allocated. Thus for problems where there
		// are no bounds, or only one sided bounds we do not pay the cost of
		// allocating memory for the inactive bounds constraints.
		//
		// Upon initialization these arrays are initialized to
		// std::numeric_limits<double>::max() and
		// -std::numeric_limits<double>::max() respectively which correspond
		// to the parameter block being unconstrained.
		// scoped_array<double> upper_bounds_;
		// scoped_array<double> lower_bounds_;
		double upper_bounds_[];
		double lower_bounds_[];

		// Create a parameter block with the user state, size, and index specified.
		// The size is the size of the parameter block and the index is the position
		// of the parameter block inside a Program (if any).
		public ParameterBlock(double[] user_state, int size, int index) {
			Init(user_state, size, index, null);
		}

		public ParameterBlock(double[] user_state, int size, int index, LocalParameterization local_parameterization) {
			Init(user_state, size, index, local_parameterization);
		}

		// The size of the parameter block.
		public int Size() {
			return size_;
		}

		public int LocalSize() {
			return (local_parameterization_ == null) ? size_ : local_parameterization_.LocalSize();
		}

		public void SetConstant() {
			is_constant_ = true;
		}
		
		 public void SetVarying() { 
			 is_constant_ = false; 
	     }

		// Set the parameterization. The parameterization can be set exactly once;
		// multiple calls to set the parameterization to different values will crash.
		// It is an error to pass NULL for the parameterization. The parameter block
		// does not take ownership of the parameterization.
		public void SetParameterization(LocalParameterization new_parameterization) {
			if (new_parameterization == null) {
				System.err.println("null parameterization invalid.");
				return;
			}
			// Nothing to do if the new parameterization is the same as the
			// old parameterization.
			if (new_parameterization == local_parameterization_) {
				return;
			}

			if (local_parameterization_ != null) {
				System.err.println("Can't re-set the local parameterization; it leads to ");
				System.err
						.println("ambiguous ownership. Current local parameterization is: " + local_parameterization_);
				return;
			}

			if (new_parameterization.GlobalSize() != size_) {
				System.err.println("Invalid parameterization for parameter block. The parameter block ");
				System.err.println("has size " + size_ + " while the parameterization has a global ");
				System.err.println("size of " + new_parameterization.GlobalSize() + ". Did you ");
				System.err.println("accidentally use the wrong parameter block or parameterization?");
				return;
			}

			if (new_parameterization.LocalSize() <= 0) {
				System.err.println("Invalid parameterization. Parameterizations must have a positive ");
				System.err.println("dimensional tangent space.");
				return;
			}

			local_parameterization_ = new_parameterization;
			// local_parameterization_jacobian_.reset(
			local_parameterization_jacobian_ = new double[local_parameterization_.GlobalSize()][local_parameterization_.LocalSize()];
			if (!UpdateLocalParameterizationJacobian()) {
				System.err.println("Local parameterization Jacobian computation failed for x: ");
				for (i = state_start; i < state_start + size_ - 1; i++) {
					System.err.print(state_[i] + " ");
				}
				System.err.println(state_[state_start + size_ - 1]);
			}
		}

		void Init(double[] user_state, int size, int index, LocalParameterization local_parameterization) {
			user_state_ = user_state;
			size_ = size;
			index_ = index;
			is_constant_ = false;
			state_ = user_state_;
			state_start = 0;

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
			InvalidateArray(Size(), LocalSize(), local_parameterization_jacobian_);
			if (!local_parameterization_.ComputeJacobian(state_, state_start, local_parameterization_jacobian_)) {
				System.err.println("Local parameterization Jacobian computation failed for x:");
				for (i = state_start; i < state_start + size_ - 1; i++) {
					System.err.print(state_[i] + " ");
				}
				System.err.println(state_[state_start + size_ - 1]);
				return false;
			}

			if (!IsArrayValid(Size(), LocalSize(), local_parameterization_jacobian_)) {
				System.err.println("Local parameterization Jacobian computation returned");
				System.err.println("an invalid matrix for x: ");
				for (i = state_start; i < state_start + size_ - 1; i++) {
					System.err.print(state_[i] + " ");
				}
				System.err.println(state_[state_start + size_ - 1]);
				System.err.println("\n Jacobian matrix : ");
				for (i = 0; i < size_; i++) {
					for (j = 0; j < LocalSize() - 1; j++) {
						System.err.print(local_parameterization_jacobian_[i][j] + " ");
					}
					System.err.println(local_parameterization_jacobian_[i][LocalSize() - 1]);
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

		public boolean IsConstant() {
			return is_constant_;
		}

		public double[] user_state() {
			return user_state_;
		}

		// Manipulate the parameter state.
		public boolean SetState(double[] x, int state_start) {
			if (x == null) {
				System.err.println("In ParameterBlock.SetState x is null");
				return false;
			}
			if (is_constant_) {
				System.err.println("In ParameterBlock.SetState is_constant_ is true");
				return false;
			}

			state_ = x;
			this.state_start = state_start;
			return UpdateLocalParameterizationJacobian();
		}

		public double[] mutable_user_state() {
			return user_state_;
		}

		public LocalParameterization mutable_local_parameterization() {
			return local_parameterization_;
		}

		public LocalParameterization local_parameterization() {
			return local_parameterization_;
		}

		public double LowerBoundForParameter(int index) {
			if (lower_bounds_ == null) {
				return -Double.MAX_VALUE;
			} else {
				return lower_bounds_[index];
			}
		}

		public double UpperBoundForParameter(int index) {
			if (upper_bounds_ == null) {
				return Double.MAX_VALUE;
			} else {
				return upper_bounds_[index];
			}
		}

		public void SetUpperBound(int index, double upper_bound) {
			if (index >= size_) {
				System.err.println("In ParameterBlock.SetUpperBound index >= size_");
				return;
			}

			if (upper_bounds_ == null) {
				upper_bounds_ = new double[size_];
				for (int i = 0; i < size_; i++) {
					upper_bounds_[i] = Double.MAX_VALUE;
				}
			}

			upper_bounds_[index] = upper_bound;
		}

		void SetLowerBound(int index, double lower_bound) {
			if (index >= size_) {
				System.err.println("In ParameterBlock.SetLowerBound index >= size_");
				return;
			}

			if (lower_bounds_ == null) {
				lower_bounds_ = new double[size_];
				for (int i = 0; i < size_; i++) {
					lower_bounds_[i] = -Double.MAX_VALUE;
				}
			}

			lower_bounds_[index] = lower_bound;
		}

		// The local to global jacobian. Returns NULL if there is no local
		// parameterization for this parameter block. The returned matrix is row-major
		// and has Size() rows and LocalSize() columns.
		public double[][] LocalParameterizationJacobian() {
			return local_parameterization_jacobian_;
		}

		// This parameter block's index in an array.
		public int index() {
			return index_;
		}

		public void set_index(int index) {
			index_ = index;
		}

		// This parameter offset inside a larger state vector.
		public int state_offset() {
			return state_offset_;
		}

		public void set_state_offset(int state_offset) {
			state_offset_ = state_offset;
		}

		// This parameter offset inside a larger delta vector.
		public int delta_offset() {
			return delta_offset_;
		}

		public void set_delta_offset(int delta_offset) {
			delta_offset_ = delta_offset;
		}

		public double[] state() {
			return state_;
		}
		
		public int state_start() {
			return state_start;
		}

		// Copy the current parameter state out to x. This is "GetState()" rather than
		// simply "state()" since it is actively copying the data into the passed
		// pointer.
		public void GetState(double[] x, int x_start) {
			if (x != state_) {
				for (int i = state_start; i < state_start + size_; i++) {
					x[i-state_start+x_start] = state_[i];
				}
			}
		}
		
		public boolean Plus(double x[], double delta[], double x_plus_delta[]) {
			int i;
			boolean cond;
			Vector<Double>xvec = new Vector<Double>(x.length);
			for (i = 0; i < x.length; i++) {
				xvec.add(x[i]);
			}
			Vector<Double>deltavec = new Vector<Double>(delta.length);
			for (i = 0; i < delta.length; i++) {
				deltavec.add(delta[i]);
			}
			Vector<Double> x_plus_delta_vec = new Vector<Double>(x_plus_delta.length);
			for (i = 0; i < x_plus_delta.length; i++) {
				x_plus_delta_vec.add(0.0);
			}
			cond = Plus(xvec, 0, deltavec, 0, x_plus_delta_vec, 0);
			for ( i = 0; i < x_plus_delta.length; i++) {
				x_plus_delta[i] = x_plus_delta_vec.get(i);
			}
			return cond;
		}
		
		// Generalization of the addition operation. This is the same as
		  // LocalParameterization::Plus() followed by projection onto the
		  // hyper cube implied by the bounds constraints.
		  public boolean Plus(Vector<Double> x, int x_index,Vector<Double> delta, int delta_index,
				  Vector<Double> x_plus_delta, int x_plus_delta_index) {
			int i;
		    if (local_parameterization_ != null) {
		      if (!local_parameterization_.Plus(x, x_index, delta, delta_index,
		    		  x_plus_delta, x_plus_delta_index)) {
		        return false;
		      }
		    } else {
		      for (i = 0; i < size_; i++) {
		    	  x_plus_delta.set(x_plus_delta_index + i, x.get(x_index + i) + delta.get(delta_index + i));
		      }
		    }

		    // Project onto the box constraints.
		    if (lower_bounds_ != null) {
		      for (i = 0; i < size_; ++i) {
		        x_plus_delta.set(x_plus_delta_index + i, Math.max(x_plus_delta.get(x_plus_delta_index + i), lower_bounds_[i]));
		      }
		    }

		    if (upper_bounds_ != null) {
		      for (i = 0; i < size_; ++i) {
		        x_plus_delta.set(x_plus_delta_index + i, Math.min(x_plus_delta.get(x_plus_delta_index + i), upper_bounds_[i]));
		      }
		    }

		    return true;
		  }
		  
		  public void RemoveResidualBlock(ResidualBlock residual_block) {
			    if (residual_blocks_ == null) {
			    	System.err.println("In ParameterBlock.RemoveResidualBlock residual_blocks_ == null");
			    	return;
			    }
			    if (!residual_blocks_.contains(residual_block)) {
			    	System.err.println("In ParameterBlock.RemoveResidualBlock residual_blocks_.contains(residual_block) == false");
			        return;
			    }
			    residual_blocks_.remove(residual_block);
			  }
		  
		  // This is only intended for iterating; perhaps this should only expose
		  // .begin() and .end().
		  public HashSet<ResidualBlock> mutable_residual_blocks() {
		    return residual_blocks_;
		  }
	} // class ParameterBlock
	
	// Construct a local parameterization by taking the Cartesian product
	// of a number of other local parameterizations. This is useful, when
	// a parameter block is the cartesian product of two or more
	// manifolds. For example the parameters of a camera consist of a
	// rotation and a translation, i.e., SO(3) x R^3.
	//
	// Currently this class supports taking the cartesian product of up to
	// four local parameterizations.
	//
	// Example usage:
	//
	// ProductParameterization product_param(new QuaterionionParameterization(),
    //	                                       new IdentityParameterization(3));
	//
	// is the local parameterization for a rigid transformation, where the
	// rotation is represented using a quaternion.
	class ProductParameterization extends LocalParameterization {
		private Vector<LocalParameterization> local_params_ = new Vector<LocalParameterization>();
		private int local_size_;
		private int global_size_;
	    private int buffer_size_;
		  //
		  // NOTE: All the constructors take ownership of the input local
		  // parameterizations.
		  //
		  public ProductParameterization(LocalParameterization local_param1,
		                          LocalParameterization local_param2) {
			  local_params_.add(local_param1);
			  local_params_.add(local_param2);
			  Init();  
		  }

		  public ProductParameterization(LocalParameterization local_param1,
		                          LocalParameterization local_param2,
		                          LocalParameterization local_param3) {
			  local_params_.add(local_param1);
			  local_params_.add(local_param2);
			  local_params_.add(local_param3);
			  Init();
		  }

		  public ProductParameterization(LocalParameterization local_param1,
		                          LocalParameterization local_param2,
		                          LocalParameterization local_param3,
		                          LocalParameterization local_param4) {
			  local_params_.add(local_param1);
			  local_params_.add(local_param2);
			  local_params_.add(local_param3);
			  local_params_.add(local_param4);
			  Init();
		  }
		  
		  public void finalize() {
			  for (int i = 0; i < local_params_.size(); ++i) {
				  LocalParameterization local_param = local_params_.get(i);
				  local_param = null;
			  }
		  }

		  public boolean Plus(double[] x,
		                    double[] delta,
		                    double[] x_plus_delta) {
			  int i;
			  Vector<Double> xvec = new Vector<Double>();
			  for (i = 0; i < x.length; i++) {
				  xvec.add(x[i]);
			  }
			  Vector<Double> deltavec = new Vector<Double>();
			  for (i = 0; i < delta.length; i++) {
				  deltavec.add(delta[i]);
			  }
			  Vector<Double>x_plus_delta_vec = new Vector<Double>();
			  for (i = 0; i < x_plus_delta.length; i++) {
				  x_plus_delta_vec.add(x_plus_delta[i]);
			  }
			  int x_cursor = 0;
			  int delta_cursor = 0;
			  for (i = 0; i < local_params_.size(); ++i) {
			    LocalParameterization param = local_params_.get(i);
			    if (!param.Plus(xvec,x_cursor,
			                     deltavec,delta_cursor,
			                     x_plus_delta_vec,x_cursor)) {
			      return false;
			    }
			    delta_cursor += param.LocalSize();
			    x_cursor += param.GlobalSize();
			  }
			  
			  for (i = 0; i < x_plus_delta.length; i++) {
				 x_plus_delta[i] = x_plus_delta_vec.get(i);
			  }

			  return true;
		  }
		  
		  
		  
		  public boolean Plus(Vector<Double> x, int x_index,
	                Vector<Double> delta, int delta_index,
	                Vector<Double> x_plus_delta, int x_plus_delta_index) {
			  int i;
			  int x_cursor = 0;
			  int delta_cursor = 0;
			  for (i = 0; i < local_params_.size(); ++i) {
			    LocalParameterization param = local_params_.get(i);
			    if (!param.Plus(x,x_index + x_cursor,
			                     delta,delta_index + delta_cursor,
			                     x_plus_delta,x_plus_delta_index + x_cursor)) {
			      return false;
			    }
			    delta_cursor += param.LocalSize();
			    x_cursor += param.GlobalSize();
			  }
			  return true;  
		  }
		  
		  public boolean ComputeJacobian(double[] x,
		                               double[] jacobian_ptr) {
			  int r, c, index;
			  double jacobian[][] = new double[GlobalSize()][LocalSize()];
			  //MatrixRef jacobian(jacobian_ptr, GlobalSize(), LocalSize());
			  //jacobian.setZero();
			  double buffer[] = new double[buffer_size_];

			  int x_cursor = 0;
			  int delta_cursor = 0;
			  for (int i = 0; i < local_params_.size(); ++i) {
			    LocalParameterization param = local_params_.get(i);
			    int local_size = param.LocalSize();
			    int global_size = param.GlobalSize();
			    double buf2D[][] = new double[global_size][local_size];
			    for (index = 0, r = 0; r < global_size; r++) {
			    	for (c = 0; c < local_size; c++, index++) {
			    		buf2D[r][c] = buffer[index];
			    	}
			    }

			    if (!param.ComputeJacobian(x, x_cursor, buf2D)) {
			      return false;
			    }
			    for (index = 0, r = 0; r < global_size; r++) {
			    	for (c = 0; c < local_size; c++, index++) {
			    		buffer[index] = buf2D[r][c];
			    	}
			    }
			    for (index = 0, r = x_cursor; r < x_cursor + global_size; r++) {
			    	for (c = delta_cursor; c < delta_cursor + local_size; c++, index++) {
			    		jacobian[r][c] = buffer[index];
			    	}
			    }
			    
			    delta_cursor += local_size;
			    x_cursor += global_size;
			  }
			  
			  for (index = 0, r = 0; r < GlobalSize(); r++) {
				  for (c = 0; c < LocalSize(); c++, index++) {
					  jacobian_ptr[index] = jacobian[r][c];
				  }
			  }

			  return true;  
		  }
		  
		  

		  
		  public boolean ComputeJacobian(double[] x, int x_start,
                  double[][] jacobian_ptr) {
			  int r, c, index;
			  for (r = 0; r < GlobalSize(); r++) {
				  for (c = 0; c < LocalSize(); c++) {
					  jacobian_ptr[r][c] = 0;
				  }
			  }
			  //MatrixRef jacobian(jacobian_ptr, GlobalSize(), LocalSize());
			  //jacobian.setZero();
			  double buffer[] = new double[buffer_size_];

			  int x_cursor = 0;
			  int delta_cursor = 0;
			  for (int i = 0; i < local_params_.size(); ++i) {
			    LocalParameterization param = local_params_.get(i);
			    int local_size = param.LocalSize();
			    int global_size = param.GlobalSize();
			    double buf2D[][] = new double[global_size][local_size];
			    for (index = 0, r = 0; r < global_size; r++) {
			    	for (c = 0; c < local_size; c++, index++) {
			    		buf2D[r][c] = buffer[index];
			    	}
			    }

			    if (!param.ComputeJacobian(x, x_start + x_cursor, buf2D)) {
			      return false;
			    }
			    for (index = 0, r = 0; r < global_size; r++) {
			    	for (c = 0; c < local_size; c++, index++) {
			    		buffer[index] = buf2D[r][c];
			    	}
			    }
			    for (index = 0, r = x_cursor; r < x_cursor + global_size; r++) {
			    	for (c = delta_cursor; c < delta_cursor + local_size; c++, index++) {
			    		jacobian_ptr[r][c] = buffer[index];
			    	}
			    }
			    
			    delta_cursor += local_size;
			    x_cursor += global_size;
			  }

			  return true;    
          }
		  
		  public int GlobalSize() { return global_size_; }
		  public int LocalSize() { return local_size_; }

		  private void Init() {
			  global_size_ = 0;
			  local_size_ = 0;
			  buffer_size_ = 0;
			  for (int i = 0; i < local_params_.size(); ++i) {
			    LocalParameterization param = local_params_.get(i);
			    buffer_size_ = Math.max(buffer_size_,
			                            param.LocalSize() * param.GlobalSize());
			    global_size_ += param.GlobalSize();
			    local_size_ += param.LocalSize();
			  }
  
		  };

		  
		};
	

	// Hold a subset of the parameters inside a parameter block constant.
	class SubsetParameterization extends LocalParameterization {
		private int local_size_;
		private Vector<Byte> constancy_mask_;
		public SubsetParameterization(
			    int size, Vector<Integer> constant_parameters) {
			    int i;
			    boolean duplicateFound;
			    local_size_ = size - constant_parameters.size();
			    constancy_mask_ = new Vector<Byte>(size);
			    for (i = 0; i < size; i++) {
			    	constancy_mask_.add((byte)0);
			    }
			  Vector<Integer> constant = constant_parameters;
			  Collections.sort(constant);
			  if(constant.firstElement() < 0) {
			       System.err.println("In public SubsetParameterization indices indicating constant parameter must be greater than zero.");
			       return;
			  }
			  if (constant.lastElement() >= size) {
			      System.err.println("In public SubsetParameterization indices indicating constant parameter must be less than the size ");
			      System.err.println("of the parameter block.");
			      return;
			  }
			  duplicateFound = false;
			  for (i = constant.size() -1; i >= 1; i--) {
				  if (constant.get(i).intValue() == constant.get(i-1).intValue()) {
					  duplicateFound = true;  
				  }
			  }
			  if (duplicateFound) {
			      System.err.println("In public SubsetParameterization the set of constant parameters cannot contain duplicates");
			      return;
			  }
			  for (i = 0; i < constant_parameters.size(); ++i) {
			    constancy_mask_.set(constant_parameters.get(i), (byte)1);
			  }
			}
		
		public boolean Plus(Vector<Double> x, int x_index,
                Vector<Double> delta, int delta_index,
                Vector<Double> x_plus_delta, int x_plus_delta_index) {
			for (int i = 0, j = 0; i < constancy_mask_.size(); ++i) {
				if (constancy_mask_.get(i) != 0) {
					if ((x_plus_delta_index +i) < x_plus_delta.size()) {
						x_plus_delta.set(x_plus_delta_index + i, x.get(x_index + i));
					}
					else {
						x_plus_delta.add(x.get(x_index + i));
					}
				} else {
					if ((x_plus_delta_index +i) < x_plus_delta.size()) {
						x_plus_delta.set(x_plus_delta_index + i, x.get(x_index + i) + delta.get(delta_index + j++));
					}
					else {
						x_plus_delta.add(x.get(x_index + i) + delta.get(delta_index + j++));
					}
				}
				}
				return true;
		}

		public boolean Plus(double[] x,
                double[] delta,
                double[] x_plus_delta){
			for (int i = 0, j = 0; i < constancy_mask_.size(); ++i) {
			if (constancy_mask_.get(i) != 0) {
			x_plus_delta[i] = x[i];
			} else {
			x_plus_delta[i] = x[i] + delta[j++];
			}
			}
			return true;
			}
		
		public boolean ComputeJacobian(double[] x,
                double[] jacobian) {
			int r,c, index;
			if (local_size_ == 0) {
			return true;
			}
			
			for (index = 0, r = 0; r < constancy_mask_.size(); r++) {
			    for (c = 0; c < local_size_; c++, index++) {
			    	jacobian[index] = 0.0;
			    }
			}
			for (r = 0, c = 0; r < constancy_mask_.size(); ++r) {
			if (constancy_mask_.get(r) == 0) {
				index = r * local_size_ + c;
				jacobian[index] = 1.0;
				c++;
			}
			}
			return true;
			}
		
		public boolean ComputeJacobian(double[] x,
                double[][] jacobian) {
			int r,c;
			if (local_size_ == 0) {
			return true;
			}
			
			for (r = 0; r < constancy_mask_.size(); r++) {
			    for (c = 0; c < local_size_; c++) {
			    	jacobian[r][c] = 0.0;
			    }
			}
			for (r = 0, c = 0; r < constancy_mask_.size(); ++r) {
			if (constancy_mask_.get(r) == 0) {
				jacobian[r][c] = 1.0;
				c++;
			}
			}
			return true;
			}
		
		public boolean ComputeJacobian(double[] x, int x_start, 
                double[][] jacobian) {
			int r,c;
			if (local_size_ == 0) {
			return true;
			}
			
			for (r = 0; r < constancy_mask_.size(); r++) {
			    for (c = 0; c < local_size_; c++) {
			    	jacobian[r][c] = 0.0;
			    }
			}
			for (r = 0, c = 0; r < constancy_mask_.size(); ++r) {
			if (constancy_mask_.get(r) == 0) {
				jacobian[r][c] = 1.0;
				c++;
			}
			}
			return true;
			}

	 
		public boolean ComputeJacobian(double[] x, int x_start, 
                double[] jacobian) {
			int r,c, index;
			if (local_size_ == 0) {
			return true;
			}
			
			for (index = 0, r = 0; r < constancy_mask_.size(); r++) {
			    for (c = 0; c < local_size_; c++, index++) {
			    	jacobian[index] = 0.0;
			    }
			}
			for (r = 0, c = 0; r < constancy_mask_.size(); ++r) {
			if (constancy_mask_.get(r) == 0) {
				index = r * local_size_ + c;
				jacobian[index] = 1.0;
				c++;
			}
			}
			return true;
			}

		public boolean MultiplyByJacobian(double[] x,
                int num_rows,
                double[] global_matrix,
                double[] local_matrix) {
			if (local_size_ == 0) {
			return true;
			}
			
			for (int row = 0; row < num_rows; ++row) {
			for (int col = 0, j = 0; col < constancy_mask_.size(); ++col) {
			if (constancy_mask_.get(col) == 0) {
			local_matrix[row * LocalSize() + j++] =
			global_matrix[row * GlobalSize() + col];
			}
			}
			}
			return true;
			}

	  public int GlobalSize() {
	    return (constancy_mask_.size());
	  }
	  public int LocalSize() { return local_size_; }

	
	};
	
	// zw = z * w, where * is the Quaternion product between 4 vectors.
	public void QuaternionProduct(double z[], double w[], double zw[]) {
		  zw[0] = z[0] * w[0] - z[1] * w[1] - z[2] * w[2] - z[3] * w[3];
		  zw[1] = z[0] * w[1] + z[1] * w[0] + z[2] * w[3] - z[3] * w[2];
		  zw[2] = z[0] * w[2] - z[1] * w[3] + z[2] * w[0] + z[3] * w[1];
		  zw[3] = z[0] * w[3] + z[1] * w[2] - z[2] * w[1] + z[3] * w[0];
   }
	
	// Plus(x, delta) = [cos(|delta|), sin(|delta|) delta / |delta|] * x
	// with * being the quaternion multiplication operator. Here we assume
	// that the first element of the quaternion vector is the real (cos
	// theta) part.
	class QuaternionParameterization extends LocalParameterization {
	 public QuaternionParameterization() {
	     super();	 
	 }
	
	  public boolean Plus(double[] x,
	                    double[] delta,
	                    double[] x_plus_delta)  {
		  final double norm_delta =
			      Math.sqrt(delta[0] * delta[0] + delta[1] * delta[1] + delta[2] * delta[2]);
			  if (norm_delta > 0.0) {
			    final double sin_delta_by_delta = (Math.sin(norm_delta) / norm_delta);
			    double q_delta[] = new double[4];
			    q_delta[0] = Math.cos(norm_delta);
			    q_delta[1] = sin_delta_by_delta * delta[0];
			    q_delta[2] = sin_delta_by_delta * delta[1];
			    q_delta[3] = sin_delta_by_delta * delta[2];
			    QuaternionProduct(q_delta, x, x_plus_delta);
			  } else {
			    for (int i = 0; i < 4; ++i) {
			      x_plus_delta[i] = x[i];
			    }
			  }

		  return true;
	  }
	  
	  public boolean Plus(Vector<Double> x, int x_index,
              Vector<Double> delta, int delta_index,
              Vector<Double> x_plus_delta, int x_plus_delta_index) {
		  int i;
		  boolean cond;
		  double x_array[] = new double[4];
		  for (i = x_index; i < x_index+4; i++) {
			  x_array[i-x_index] = x.get(i);
		  }
		  double delta_array[] = new double[4];
		  for (i = delta_index; i < delta_index+4; i++) {
			  delta_array[i - delta_index] = delta.get(i);
		  }
		  double x_plus_delta_array[] = new double[4];
		  cond = Plus(x_array, delta_array, x_plus_delta_array);
		  for (i = x_plus_delta_index; i < x_plus_delta_index+4; i++) {
			  if (i < x_plus_delta.size()) {
				  x_plus_delta.set(i, x_plus_delta_array[i-x_plus_delta_index]);
			  }
			  else {
				  x_plus_delta.add(x_plus_delta_array[i-x_plus_delta_index]);
			  }
		  }
		  return cond;
	  }
	  public boolean ComputeJacobian(double[] x, int x_start,
	                               double[][] jacobian) {
		  // jacobian is [4][3]
		  jacobian[0][0] = -x[x_start+1]; jacobian[0][1]  = -x[x_start+2]; jacobian[0][2]  = -x[x_start+3];
		  jacobian[1][0] =  x[x_start]; jacobian[1][1]  =  x[x_start+3]; jacobian[1][2]  = -x[x_start+2];
		  jacobian[2][0] = -x[x_start+3]; jacobian[2][1]  =  x[x_start]; jacobian[2][2]  =  x[x_start+1];
		  jacobian[3][0] =  x[x_start+2]; jacobian[3][1] = -x[x_start+1]; jacobian[3][2] =  x[x_start];
		  return true;
	  }
	  public int GlobalSize() { return 4; }
	  public int LocalSize() { return 3; }
	};
	
	// Implements the quaternion local parameterization for Eigen's representation
	// of the quaternion. Eigen uses a different internal memory layout for the
	// elements of the quaternion than what is commonly used. Specifically, Eigen
	// stores the elements in memory as [x, y, z, w] where the real part is last
	// whereas it is typically stored first. Note, when creating an Eigen quaternion
	// through the constructor the elements are accepted in w, x, y, z order. Since
	// Ceres operates on parameter blocks which are raw double pointers this
	// difference is important and requires a different parameterization.
	//
	// Plus(x, delta) = [sin(|delta|) delta / |delta|, cos(|delta|)] * x
	// with * being the quaternion multiplication operator.
	// The following two typedefs are provided for convenience:
	// Quaternionf for float
	// Quaterniond for double
	/** Constructs and initializes the quaternion \f$ w+xi+yj+zk \f$ from
	    * its four coefficients \a w, \a x, \a y and \a z.
	    *
	    * \warning Note the order of the arguments: the real \a w coefficient first,
	    * while internally the coefficients are stored in the following order:
	    * [\c x, \c y, \c z, \c w]
	    */
	
	public void EigenQuaternionProduct(double a[], double b[], double ab[]) {
		// ab[3] is the real part
		// a[3] and b[3] must be the real parts
		      ab[3] = a[3] * b[3] - a[0] * b[0] - a[1] * b[1] - a[2] * b[2];
		      ab[0] = a[3] * b[0] + a[0] * b[3] + a[1] * b[2] - a[2] * b[1];
		      ab[1] = a[3] * b[1] + a[1] * b[3] + a[2] * b[0] - a[0] * b[2];
		      ab[2] = a[3] * b[2] + a[2] * b[3] + a[0] * b[1] - a[1] * b[0];
	};
	
	class EigenQuaternionParameterization extends LocalParameterization {
	 public EigenQuaternionParameterization() {
		 super();
	 }
	 
	 public boolean Plus(double[] x_ptr,
             double[] delta,
             double[] x_plus_delta_ptr)  {
		 // The inputs have the real parts first, but in the Eigen::Quaterniond the real parts are moved last
		 //Eigen::Map<Eigen::Quaterniond> x_plus_delta(x_plus_delta_ptr);
		 //Eigen::Map<const Eigen::Quaterniond> x(x_ptr);
         double x[] = new double[] {x_ptr[1], x_ptr[2], x_ptr[3], x_ptr[0]};
         double x_plus_delta[] = new double[4];
		  final double norm_delta =
		      Math.sqrt(delta[0] * delta[0] + delta[1] * delta[1] + delta[2] * delta[2]);
		  if (norm_delta > 0.0) {
		    final double sin_delta_by_delta = Math.sin(norm_delta) / norm_delta;
		 // Note, in the constructor w is first.
		    //Eigen::Quaterniond delta_q(cos(norm_delta),
		                               //sin_delta_by_delta * delta[0],
		                               //sin_delta_by_delta * delta[1],
		                               //sin_delta_by_delta * delta[2]);

		    // Here w is last
		    double delta_q[] = new double[] {
		                               sin_delta_by_delta * delta[0],
		                               sin_delta_by_delta * delta[1],
		                               sin_delta_by_delta * delta[2],
		                               Math.cos(norm_delta)};
		    EigenQuaternionProduct(delta_q, x, x_plus_delta);
		    // Real part is moved back from the last position to the first position
		    x_plus_delta_ptr[0] = x_plus_delta[3];
		    x_plus_delta_ptr[1] = x_plus_delta[0];
		    x_plus_delta_ptr[2] = x_plus_delta[1];
		    x_plus_delta_ptr[3] = x_plus_delta[2];
		  } else {
			  for (int i = 0; i < 4; ++i) {
			      x_plus_delta_ptr[i] = x_ptr[i];
			  }
		  }


			return true;
			}
			
			public boolean Plus(Vector<Double> x, int x_index,
			   Vector<Double> delta, int delta_index,
			   Vector<Double> x_plus_delta, int x_plus_delta_index) {
			int i;
			boolean cond;
			double x_array[] = new double[4];
			for (i = x_index; i < x_index+4; i++) {
				  x_array[i-x_index] = x.get(i);
			}
			double delta_array[] = new double[4];
			for (i = delta_index; i < delta_index+4; i++) {
				  delta_array[i - delta_index] = delta.get(i);
			}
			double x_plus_delta_array[] = new double[4];
			cond = Plus(x_array, delta_array, x_plus_delta_array);
			for (i = x_plus_delta_index; i < x_plus_delta_index+4; i++) {
				  if (i < x_plus_delta.size()) {
					  x_plus_delta.set(i, x_plus_delta_array[i-x_plus_delta_index]);
				  }
				  else {
					  x_plus_delta.add(x_plus_delta_array[i-x_plus_delta_index]);
				  }
			}
			return cond;
			}
			public boolean ComputeJacobian(double[] x, int x_start,
			                        double[][] jacobian) {
			// jacobian is [4][3]
			jacobian[0][0] = x[x_start+3]; jacobian[0][1]  = x[x_start+2]; jacobian[0][2]  = -x[x_start+1];
			jacobian[1][0] =  -x[x_start+2]; jacobian[1][1]  =  x[x_start+3]; jacobian[1][2]  = x[x_start];
			jacobian[2][0] = x[x_start+1]; jacobian[2][1]  =  -x[x_start]; jacobian[2][2]  =  x[x_start+3];
			jacobian[3][0] =  -x[x_start]; jacobian[3][1] = -x[x_start+1]; jacobian[3][2] =  -x[x_start+2];
			return true;
			}
			public int GlobalSize() { return 4; }
			public int LocalSize() { return 3; }
	  
	};
	
	// Algorithm 5.1.1 from 'Matrix Computations' by Golub et al. (Johns Hopkins
	// Studies in Mathematical Sciences) but using the nth element of the input
	// vector as pivot instead of first. This computes the vector v with v(n) = 1
	// and beta such that H = I - beta * v * v^T is orthogonal and
	// H * x = ||x||_2 * e_n.
	// beta is of length 1
	public void ComputeHouseholderVector(double x[],
	                              double v[],
	                              double[] beta) {
	  int i;
	  double sigma;
	  if (beta == null) {
		  System.err.println("In ComputeHouseholderVector beta == null");
		  return;
	  }
	  if (v == null) {
		  System.err.println("In ComputeHouseholderVector v == null");
		  return;	  
	  }
	  if (x.length <= 1) {
		  System.err.println("In ComputeHouseholderVector x.length <= 1");
		  return;
	  }
	  if (x.length != v.length) {
		  System.err.println("In ComputeHouseholderVector x.length != v.length");
		  return;  
	  }

	  sigma = 0;
	  for (i = 0; i < x.length - 1; i++) {
		  sigma += (x[i] * x[i]);
		  v[i] = x[i];
	  }
	  v[v.length-1] = 1.0;

	  beta[0] = 0.0;
	  final double x_pivot = x[x.length - 1];

	  if (sigma <= epsilon) {
	    if (x_pivot < 0.0) {
	      beta[0] = 2.0;
	    }
	    return;
	  }

	  final double mu = Math.sqrt(x_pivot * x_pivot + sigma);
	  double v_pivot = 1.0;

	  if (x_pivot <= 0.0) {
	    v_pivot = x_pivot - mu;
	  } else {
	    v_pivot = -sigma / (x_pivot + mu);
	  }

	  beta[0] = 2.0 * v_pivot * v_pivot / (sigma + v_pivot * v_pivot);

	  for (i = 0; i < v.length - 1; i++) {
		  v[i] /= v_pivot;
	  }
	 
	}

	
	// This provides a parameterization for homogeneous vectors which are commonly
	// used in Structure for Motion problems.  One example where they are used is
	// in representing points whose triangulation is ill-conditioned. Here
	// it is advantageous to use an over-parameterization since homogeneous vectors
	// can represent points at infinity.
	//
	// The plus operator is defined as
	// Plus(x, delta) =
//	    [sin(0.5 * |delta|) * delta / |delta|, cos(0.5 * |delta|)] * x
	// with * defined as an operator which applies the update orthogonal to x to
	// remain on the sphere. We assume that the last element of x is the scalar
	// component. The size of the homogeneous vector is required to be greater than
	// 1.
	class HomogeneousVectorParameterization extends LocalParameterization {
	 private int size_;
	 public HomogeneousVectorParameterization(int size) {
		 super();
		 if (size <= 1) {
			 System.err.println("The size of the homogeneous vector needs to be greater than 1");
			 return;
		 }

		 size_ = size;
	 }
	 
	 public boolean Plus(double[] x_ptr,
             double[] delta_ptr,
             double[] x_plus_delta_ptr)  {
		 int i;
		 double normSquared;
		 //ConstVectorRef x(x_ptr, size_);
		 // ConstVectorRef delta(delta_ptr, size_ - 1);
		  //VectorRef x_plus_delta(x_plus_delta_ptr, size_);

		 normSquared = 0.0;
		 for (i = 0; i < size_ - 1; i++) {
			 normSquared += (delta_ptr[i] * delta_ptr[i]);
		 }
		 final double norm_delta = Math.sqrt(normSquared);

		  if (norm_delta == 0.0) {
			for (i = 0; i < size_; i++) {
		        x_plus_delta_ptr[i] = x_ptr[i];
			}
		    return true;
		  }

		  // Map the delta from the minimum representation to the over parameterized
		  // homogeneous vector. See section A6.9.2 on page 624 of Hartley & Zisserman
		  // (2nd Edition) for a detailed description.  Note there is a typo on Page
		  // 625, line 4 so check the book errata.
		  final double norm_delta_div_2 = 0.5 * norm_delta;
		  final double sin_delta_by_delta = Math.sin(norm_delta_div_2) /
		      norm_delta_div_2;

		  double y[] = new double[size_];
		  for (i = 0; i < size_ - 1; i++) {
			  y[i] = 0.5 * sin_delta_by_delta * delta_ptr[i];
		  }
		  y[size_ - 1] = Math.cos(norm_delta_div_2);

		  double v[] = new double[size_];
		  double beta[] = new double[1];
		  double x[] = new double[size_];
		  normSquared = 0.0;
		  for (i = 0; i < size_; i++) {
			  x[i] = x_ptr[i];
			  normSquared += (x[i] *x[i]);
		  }
		  double xnorm = Math.sqrt(normSquared);
		  ComputeHouseholderVector(x, v, beta);

		  // Apply the delta update to remain on the unit sphere. See section A6.9.3
		  // on page 625 of Hartley & Zisserman (2nd Edition) for a detailed
		  // description.
		  //x_plus_delta = x.norm() * (y -  v * (beta * (v.transpose() * y)));
          double vy = 0;
          for (i = 0; i < size_; i++) {
        	  vy += (v[i] * y[i]);
          }
          for (i = 0; i < size_; i++) {
        	  x_plus_delta_ptr[i] = xnorm * (y[i] - v[i] * (beta[0] * vy));
          }

		 return true;
	 }
	 
	 public boolean Plus(Vector<Double> x, int x_index,
             Vector<Double> delta, int delta_index,
             Vector<Double> x_plus_delta, int x_plus_delta_index) {
		 int i;
			boolean cond;
			double x_array[] = new double[x.size()-x_index];
			for (i = x_index; i < x.size(); i++) {
				  x_array[i-x_index] = x.get(i);
			}
			double delta_array[] = new double[delta.size() - delta_index];
			for (i = delta_index; i < delta.size(); i++) {
				  delta_array[i - delta_index] = delta.get(i);
			}
			double x_plus_delta_array[] = new double[x_array.length];
			cond = Plus(x_array, delta_array, x_plus_delta_array);
			for (i = x_plus_delta_index; i < x_plus_delta_index+x_array.length; i++) {
				  if (i < x_plus_delta.size()) {
					  x_plus_delta.set(i, x_plus_delta_array[i-x_plus_delta_index]);
				  }
				  else {
					  x_plus_delta.add(x_plus_delta_array[i-x_plus_delta_index]);
				  }
			}
			return cond;
	 }
	 
	 public boolean ComputeJacobian(double x_ptr[], int x_start, double jacobian_ptr[][]) {
		  // ConstVectorRef x(x_ptr, size_);
		  // MatrixRef jacobian(jacobian_ptr, size_, size_ - 1);
		 int i,j ;
		 double x[] = new double[size_];
		 double normSquared = 0.0;
		 for (i = x_start; i < x_start + size_; i++) {
			 x[i-x_start] = x_ptr[i];
			 normSquared += (x_ptr[i] * x_ptr[i]);
		 }
		 double xnorm = Math.sqrt(normSquared);
		 
		 double v[] = new double[size_];
		 double beta[] = new double[1];
		 ComputeHouseholderVector(x, v, beta);

		  // The Jacobian is equal to J = 0.5 * H.leftCols(size_ - 1) where H is the
		  // Householder matrix (H = I - beta * v * v').
		  for (i = 0; i < size_ - 1; ++i) {
			for (j = 0; j < size_; j++) {
				jacobian_ptr[j][i] = -0.5 * beta[0] * v[i] * v[j];
			}
		   
		    jacobian_ptr[i][i] += 0.5;
		  }
		  
		  for (i = 0; i < size_ - 1; ++i) {
				for (j = 0; j < size_; j++) {
		            jacobian_ptr[j][i] *= xnorm;
				}
		  }

		 return true;
	 }
	  public int GlobalSize() { return size_; }
	  public int LocalSize() { return size_ - 1; }
	};

	// The class LocalParameterization defines the function Plus and its
	// Jacobian which is needed to compute the Jacobian of f w.r.t delta.
	abstract class LocalParameterization {
		
		public boolean MultiplyByJacobian(double x[],
                int num_rows,
                double[][] global_matrix,
                double[][] local_matrix) {
			int i,j;
			//Matrix jacobian(GlobalSize(), LocalSize());
			double jacobian[][] = new double[GlobalSize()][LocalSize()];
			if (!ComputeJacobian(x, 0, jacobian)) {
			return false;
			}
			
			//MatrixRef(local_matrix, num_rows, LocalSize()) =
			//ConstMatrixRef(global_matrix, num_rows, GlobalSize()) * jacobian;
			double local_matrix_array[][] = (new Matrix(global_matrix).times(new Matrix(jacobian))).getArray();
			for (i = 0; i < num_rows; i++) {
				for (j = 0; j < LocalSize(); j++) {
					local_matrix[i][j] = local_matrix_array[i][j];
				}
			}
			return true;
		}
		
		public boolean MultiplyByJacobian(double x[],
                int num_rows,
                double[] global_matrix,
                double[] local_matrix) {
			int r, c, index;
			//Matrix jacobian(GlobalSize(), LocalSize());
			double jacobian[][] = new double[GlobalSize()][LocalSize()];
			if (!ComputeJacobian(x, 0, jacobian)) {
			return false;
			}
			
			//MatrixRef(local_matrix, num_rows, LocalSize()) =
			//ConstMatrixRef(global_matrix, num_rows, GlobalSize()) * jacobian;
			double global_array[][] = new double[num_rows][GlobalSize()];
			for (index = 0, r = 0; r < num_rows; r++) {
				for (c = 0; c < GlobalSize(); c++, index++) {
					global_array[r][c] = global_matrix[index];
				}
			}
			double local_array[][] = (new Matrix(global_array).times(new Matrix(jacobian))).getArray();
			for (index = 0, r = 0; r < num_rows; r++) {
				for (c = 0; c < LocalSize(); c++, index++) {
					local_matrix[index] = local_array[r][c];
				}
			}
			return true;
		}


		// jacobian is a row-major GlobalSize() x LocalSize() matrix.
		public abstract boolean ComputeJacobian(double x[], int x_start, double jacobian[][]);

		// Size of x.
		public abstract int GlobalSize();

		// Size of delta.
		public abstract int LocalSize();
		
		public abstract boolean Plus(Vector<Double> x, int x_index,
                Vector<Double> delta, int delta_index,
                Vector<Double> x_plus_delta, int x_plus_delta_index);
		
		public abstract boolean Plus(double[] x, double[] delta, double[] x_plus_delta);
	}
	
	public boolean IsSolutionUsable(SolverSummary  sum) {
		  return (sum.termination_type == TerminationType.CONVERGENCE ||
		          sum.termination_type == TerminationType.NO_CONVERGENCE ||
		          sum.termination_type == TerminationType.USER_SUCCESS);
		}
	
	public String TerminationTypeToString(TerminationType type) {
		  switch (type) {
		  case CONVERGENCE:
			  return "CONVERGENCE";
		  case NO_CONVERGENCE:
			  return "NO_CONVERGENCE";
		  case FAILURE:
			  return "FAILURE";
		  case USER_SUCCESS:
			  return "USER_SUCCESS";
		  case USER_FAILURE:
			  return "USER_FAILURE";
		    default:
		      return "UNKNOWN";
		  }
		}

	
	class SolverSummary {
		public MinimizerType minimizer_type;
		public TerminationType termination_type;
		// Reason why the solver terminated.
		public String message[];
		// Cost of the problem (value of the objective function) before
		// the optimization.
		public double initial_cost;
		// Cost of the problem (value of the objective function) after the
		// optimization.
		public double final_cost;
		public double fixed_cost;
		// IterationSummary for each minimizer iteration in order.
		public Vector<IterationSummary> iterations;
		public int num_successful_steps;
		public int num_unsuccessful_steps;
		// Number of times inner iterations were performed.
		public int num_inner_iteration_steps;
		public int num_line_search_steps;
		public double preprocessor_time_in_seconds;
		// Time spent in the TrustRegionMinimizer.
		public double minimizer_time_in_seconds;
		public double postprocessor_time_in_seconds;
		// Some total of all time spent inside Ceres when Solve is called.
		public double total_time_in_seconds;
		// Time (in seconds) spent in the linear solver computing the
		// trust region step.
		public double linear_solver_time_in_seconds;
		public int num_linear_solves;
		// Time (in seconds) spent evaluating the residual vector.
		public double residual_evaluation_time_in_seconds;
		// Number of residual only evaluations.
		public int num_residual_evaluations;
		// Time (in seconds) spent evaluating the jacobian matrix.
		public double jacobian_evaluation_time_in_seconds;
		// Number of Jacobian (and residual) evaluations.
		int num_jacobian_evaluations;
		// Time (in seconds) spent doing inner iterations.
		public double inner_iteration_time_in_seconds;
		// Time (in seconds) spent evaluating the univariate cost function as part
		// of a line search.
		public double line_search_cost_evaluation_time_in_seconds;
		// Time (in seconds) spent evaluating the gradient of the univariate cost
		// function as part of a line search.
		public double line_search_gradient_evaluation_time_in_seconds;
		// Time (in seconds) spent minimizing the interpolating polynomial
		// to compute the next candidate step size as part of a line search.
		public double line_search_polynomial_minimization_time_in_seconds;
		// Total time (in seconds) spent performing line searches.
		public double line_search_total_time_in_seconds;
		// Number of parameter blocks in the problem.
		public int num_parameter_blocks;
		// Number of parameters in the probem.
		public int num_parameters;
		public int num_effective_parameters;
		// Number of residual blocks in the problem.
		public int num_residual_blocks;
		// Number of residuals in the problem.
		public int num_residuals;
		public int num_parameter_blocks_reduced;
		// Number of parameters in the reduced problem.
		public int num_parameters_reduced;
		public int num_effective_parameters_reduced;
		// Number of residual blocks in the reduced problem.
		public int num_residual_blocks_reduced;
		// Number of residuals in the reduced problem.
		public int num_residuals_reduced;
		// Is the reduced problem bounds constrained.
		public boolean is_constrained;
		// Number of threads specified by the user for Jacobian and
		// residual evaluation.
		public int num_threads_given;
		public int num_threads_used;
		// NOTE: This field is deprecated,
		// Solver::Summary::num_threads_given should be used instead.
		//
		// This field is scheduled to be removed in 1.15.0. In the interim
		// the value of this field will always be equal to
		// num_threads_given.
		//
		// Number of threads specified by the user for solving the trust
		// region problem.
		public int num_linear_solver_threads_given;
		// NOTE: This field is deprecated,
		// Solver::Summary::num_threads_used should be used instead.
		//
		// This field is scheduled to be removed in 1.15.0. In the interim
		// the value of this field will always be equal to
		// num_threads_used.
		//
		// Number of threads actually used by the solver for solving the
		// trust region problem. This number is not equal to
		// num_threads_given if OpenMP is not available.
		public int num_linear_solver_threads_used;
		// Type of the linear solver requested by the user.
		public LinearSolverType linear_solver_type_given;
		// Type of the linear solver actually used. This may be different
		// from linear_solver_type_given if Ceres determines that the
		// problem structure is not compatible with the linear solver
		// requested or if the linear solver requested by the user is not
		// available, e.g. The user requested SPARSE_NORMAL_CHOLESKY but
		// no sparse linear algebra library was available.
		public LinearSolverType linear_solver_type_used;
		// Size of the elimination groups given by the user as hints to
		// the linear solver.
		public Vector<Integer> linear_solver_ordering_given;
		public Vector<Integer> linear_solver_ordering_used;
		public String schur_structure_given;
		public String schur_structure_used;
		// True if the user asked for inner iterations to be used as part
		// of the optimization.
		public boolean inner_iterations_given;
		public boolean inner_iterations_used;
		// Size of the parameter groups given by the user for performing
		// inner iterations.
		public Vector<Integer> inner_iteration_ordering_given;
		public Vector<Integer> inner_iteration_ordering_used;
		// Type of the preconditioner requested by the user.
		public PreconditionerType preconditioner_type_given;
		public PreconditionerType preconditioner_type_used;
		public VisibilityClusteringType visibility_clustering_type;
		// Type of trust region strategy.
		public TrustRegionStrategyType trust_region_strategy_type;
		// Type of dogleg strategy used for solving the trust region
		// problem.
		public DoglegType dogleg_type;
		// Type of the dense linear algebra library used.
		public DenseLinearAlgebraLibraryType dense_linear_algebra_library_type;
		// Type of the sparse linear algebra library used.
		public SparseLinearAlgebraLibraryType sparse_linear_algebra_library_type;
		// Type of line search direction used.
		public LineSearchDirectionType line_search_direction_type;
		// Type of the line search algorithm used.
		public LineSearchType line_search_type;
		// When performing line search, the degree of the polynomial used
		// to approximate the objective function.
		public LineSearchInterpolationType line_search_interpolation_type;
		public NonlinearConjugateGradientType nonlinear_conjugate_gradient_type;
		// If the type of the line search direction is LBFGS, then this
		// indicates the rank of the Hessian approximation.
		public int max_lbfgs_rank;

		public SolverSummary() {
			// Invalid values for most fields, to ensure that we are not
			// accidentally reporting default values.
			minimizer_type = MinimizerType.TRUST_REGION;
			termination_type = TerminationType.FAILURE;
			message = new String[] { "ceres::Solve was not called." };
			initial_cost = -1.0;
			final_cost = -1.0;
			fixed_cost = -1.0;
			num_successful_steps = -1;
			num_unsuccessful_steps = -1;
			num_inner_iteration_steps = -1;
			num_line_search_steps = -1;
			preprocessor_time_in_seconds = -1.0;
			minimizer_time_in_seconds = -1.0;
			postprocessor_time_in_seconds = -1.0;
			total_time_in_seconds = -1.0;
			linear_solver_time_in_seconds = -1.0;
			num_linear_solves = -1;
			residual_evaluation_time_in_seconds = -1.0;
			num_residual_evaluations = -1;
			jacobian_evaluation_time_in_seconds = -1.0;
			num_jacobian_evaluations = -1;
			inner_iteration_time_in_seconds = -1.0;
			line_search_cost_evaluation_time_in_seconds = -1.0;
			line_search_gradient_evaluation_time_in_seconds = -1.0;
			line_search_polynomial_minimization_time_in_seconds = -1.0;
			line_search_total_time_in_seconds = -1.0;
			num_parameter_blocks = -1;
			num_parameters = -1;
			num_effective_parameters = -1;
			num_residual_blocks = -1;
			num_residuals = -1;
			num_parameter_blocks_reduced = -1;
			num_parameters_reduced = -1;
			num_effective_parameters_reduced = -1;
			num_residual_blocks_reduced = -1;
			num_residuals_reduced = -1;
			is_constrained = false;
			num_threads_given = -1;
			num_threads_used = -1;
			num_linear_solver_threads_given = -1;
			num_linear_solver_threads_used = -1;
			linear_solver_type_given = LinearSolverType.SPARSE_NORMAL_CHOLESKY;
			linear_solver_type_used = LinearSolverType.SPARSE_NORMAL_CHOLESKY;
			inner_iterations_given = false;
			inner_iterations_used = false;
			preconditioner_type_given = PreconditionerType.IDENTITY;
			preconditioner_type_used = PreconditionerType.IDENTITY;
			visibility_clustering_type = VisibilityClusteringType.CANONICAL_VIEWS;
			trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
			//dense_linear_algebra_library_type = DenseLinearAlgebraLibraryType.EIGEN;
			dense_linear_algebra_library_type = DenseLinearAlgebraLibraryType.LAPACK;
			sparse_linear_algebra_library_type = SparseLinearAlgebraLibraryType.SUITE_SPARSE;
			line_search_direction_type = LineSearchDirectionType.LBFGS;
			line_search_type = LineSearchType.ARMIJO;
			line_search_interpolation_type = LineSearchInterpolationType.BISECTION;
			nonlinear_conjugate_gradient_type = NonlinearConjugateGradientType.FLETCHER_REEVES;
			max_lbfgs_rank = -1;
			linear_solver_ordering_given = new Vector<Integer>();
			inner_iteration_ordering_given = new Vector<Integer>();
			linear_solver_ordering_used = new Vector<Integer>();
			inner_iteration_ordering_used = new Vector<Integer>();
			iterations = new Vector<IterationSummary>();
		}
		
		public String BriefReport() {
			  return String.format("Ceres Solver Report: " +
			                      "Iterations: %d, " +
			                      "Initial cost: %e, " +
			                      "Final cost: %e, " +
			                      "Termination: %s",
			                      num_successful_steps + num_unsuccessful_steps,
			                      initial_cost,
			                      final_cost,
			                      TerminationTypeToString(termination_type));
			}
	} // class SolverSummary
	
	class SolverOptions {
		public MinimizerType minimizer_type;
		public LineSearchDirectionType line_search_direction_type;
		public LineSearchType line_search_type;
		public NonlinearConjugateGradientType nonlinear_conjugate_gradient_type;
		public int max_lbfgs_rank;
		public boolean use_approximate_eigenvalue_bfgs_scaling;
		public LineSearchInterpolationType line_search_interpolation_type;
		// If during the line search, the step_size falls below this
		// value, it is truncated to zero.
		public double min_line_search_step_size;
		public double line_search_sufficient_function_decrease;
		public double max_line_search_step_contraction;
		public double min_line_search_step_contraction;
		public int max_num_line_search_step_size_iterations;
		public int max_num_line_search_direction_restarts;
		public double line_search_sufficient_curvature_decrease;
		public double max_line_search_step_expansion;
		public TrustRegionStrategyType trust_region_strategy_type;
		public DoglegType dogleg_type;
		public boolean use_nonmonotonic_steps;
		public int max_consecutive_nonmonotonic_steps;
		// Maximum number of iterations for the minimizer to run for.
		public int max_num_iterations;
		// Maximum time for which the minimizer should run for.
		public double max_solver_time_in_seconds;
		// Number of threads used by Ceres for evaluating the cost and jacobians.
		public int num_threads;
		// Trust region minimizer settings.
		public double initial_trust_region_radius;
		public double max_trust_region_radius;
		// Minimizer terminates when the trust region radius becomes
		// smaller than this value.
		public double min_trust_region_radius;
		// Lower bound for the relative decrease before a step is accepted.
		public double min_relative_decrease;
		public double min_lm_diagonal;
		public double max_lm_diagonal;
		public int max_num_consecutive_invalid_steps;
		public double function_tolerance;
		public double gradient_tolerance;
		public double parameter_tolerance;
		public LinearSolverType linear_solver_type;
		public PreconditionerType preconditioner_type;
		public VisibilityClusteringType visibility_clustering_type;
		public DenseLinearAlgebraLibraryType dense_linear_algebra_library_type;
		public SparseLinearAlgebraLibraryType sparse_linear_algebra_library_type;
		// This setting is scheduled to be removed in 1.15.0.
		public int num_linear_solver_threads;
		// NOTE: This option can only be used with the SCHUR_JACOBI preconditioner.
		public boolean use_explicit_schur_complement;
		public boolean use_postordering;
		// This settings affects the SPARSE_NORMAL_CHOLESKY solver.
		public boolean dynamic_sparsity;
		public boolean use_inner_iterations;
		public double inner_iteration_tolerance;
		// Minimum number of iterations for which the linear solver should
		// run, even if the convergence criterion is satisfied.
		public int min_linear_solver_iterations;
		public int max_linear_solver_iterations;
		public double eta;
		// Normalize the jacobian using Jacobi scaling before calling
		// the linear least squares solver.
		public boolean jacobi_scaling;
		public LoggingType logging_type;
		public boolean minimizer_progress_to_stdout;
		public String trust_region_problem_dump_directory;
		public DumpFormatType trust_region_problem_dump_format_type;
		public boolean check_gradients;
		public double gradient_check_relative_precision;
		public double gradient_check_numeric_derivative_relative_step_size;
		public boolean update_state_every_iteration;
		// List of iterations at which the minimizer should dump the trust
		// region problem. Useful for testing and benchmarking. If empty
		// (default), no problems are dumped.
		public Vector<Integer> trust_region_minimizer_iterations_to_dump;
		// The solver does NOT take ownership of the pointer.
		EvaluationCallback evaluation_callback;
		// A particular case of interest is bundle adjustment, where the user
		// has two options. The default is to not specify an ordering at all,
		// the solver will see that the user wants to use a Schur type solver
		// and figure out the right elimination ordering.
		//
		// But if the user already knows what parameter blocks are points and
		// what are cameras, they can save preprocessing time by partitioning
		// the parameter blocks into two groups, one for the points and one
		// for the cameras, where the group containing the points has an id
		// smaller than the group containing cameras.
		// shared_ptr<ParameterBlockOrdering> linear_solver_ordering;
		// Typedef for the most commonly used version of OrderedGroups.
		// typedef OrderedGroups<double*> ParameterBlockOrdering;
		public OrderedGroups<double[]> linear_solver_ordering;
		// If inner_iterations is true, then the user has two choices.
		//
		// 1. Let the solver heuristically decide which parameter blocks
		// to optimize in each inner iteration. To do this leave
		// Solver::Options::inner_iteration_ordering untouched.
		//
		// 2. Specify a collection of of ordered independent sets. Where
		// the lower numbered groups are optimized before the higher
		// number groups. Each group must be an independent set. Not
		// all parameter blocks need to be present in the ordering.
		// shared_ptr<ParameterBlockOrdering> inner_iteration_ordering;
		public OrderedGroups<double[]> inner_iteration_ordering;
		// Callbacks that are executed at the end of each iteration of the
		// Minimizer. An iteration may terminate midway, either due to
		// numerical failures or because one of the convergence tests has
		// been satisfied. In this case none of the callbacks are
		// executed.

		// Callbacks are executed in the order that they are specified in
		// this vector. By default, parameter blocks are updated only at the
		// end of the optimization, i.e when the Minimizer terminates. This
		// behaviour is controlled by update_state_every_iteration. If the
		// user wishes to have access to the updated parameter blocks when
		// his/her callbacks are executed, then set
		// update_state_every_iteration to true.
		//
		// The solver does NOT take ownership of these pointers.
		Vector<IterationCallback> callbacks;

		public SolverOptions() {
			// Default constructor that sets up a generic sparse problem.
			minimizer_type = MinimizerType.TRUST_REGION;
			line_search_direction_type = LineSearchDirectionType.LBFGS;
			line_search_type = LineSearchType.WOLFE;
			nonlinear_conjugate_gradient_type = NonlinearConjugateGradientType.FLETCHER_REEVES;
			max_lbfgs_rank = 20;
			use_approximate_eigenvalue_bfgs_scaling = false;
			line_search_interpolation_type = LineSearchInterpolationType.CUBIC;
			min_line_search_step_size = 1e-9;
			line_search_sufficient_function_decrease = 1e-4;
			max_line_search_step_contraction = 1e-3;
			min_line_search_step_contraction = 0.6;
			max_num_line_search_step_size_iterations = 2000;
			max_num_line_search_direction_restarts = 5;
			line_search_sufficient_curvature_decrease = 0.9;
			max_line_search_step_expansion = 10.0;
			trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		    //trust_region_strategy_type = TrustRegionStrategyType.DOGLEG;
			dogleg_type = DoglegType.TRADITIONAL_DOGLEG;
			//dogleg_type = DoglegType.SUBSPACE_DOGLEG;
			use_nonmonotonic_steps = false;
			max_consecutive_nonmonotonic_steps = 5;
			max_num_iterations = 50000;
			max_solver_time_in_seconds = 1e9;
			num_threads = 1;
			initial_trust_region_radius = 1e4;
			max_trust_region_radius = 1e16;
			min_trust_region_radius = 1e-32;
			min_relative_decrease = 1e-3;
			min_lm_diagonal = 1e-6;
			max_lm_diagonal = 1e32;
			//max_num_consecutive_invalid_steps = 5;
			max_num_consecutive_invalid_steps = 10;
			function_tolerance = 1e-6;
			gradient_tolerance = 1e-10;
			parameter_tolerance = 1e-8;

			// #if defined(CERES_NO_SUITESPARSE) && defined(CERES_NO_CXSPARSE) &&
			// !defined(CERES_ENABLE_LGPL_CODE) // NOLINT
			linear_solver_type = requestedLinearSolverType;
			//linear_solver_type = LinearSolverType.DENSE_SCHUR;
			// #else
			// linear_solver_type = SPARSE_NORMAL_CHOLESKY;
			// #endif

			preconditioner_type = PreconditionerType.JACOBI;
			visibility_clustering_type = VisibilityClusteringType.CANONICAL_VIEWS;
			// dense_linear_algebra_library_type = EIGEN;
			dense_linear_algebra_library_type = DenseLinearAlgebraLibraryType.LAPACK;

			// Choose a default sparse linear algebra library in the order:
			//
			// SUITE_SPARSE > CX_SPARSE > EIGEN_SPARSE > NO_SPARSE
			sparse_linear_algebra_library_type = SparseLinearAlgebraLibraryType.NO_SPARSE;
			/*
			 * #if !defined(CERES_NO_SUITESPARSE) sparse_linear_algebra_library_type =
			 * SUITE_SPARSE; #else #if !defined(CERES_NO_CXSPARSE)
			 * sparse_linear_algebra_library_type = CX_SPARSE; #else #if
			 * defined(CERES_USE_EIGEN_SPARSE) sparse_linear_algebra_library_type =
			 * EIGEN_SPARSE; #endif #endif #endif
			 */

			num_linear_solver_threads = -1;
			use_explicit_schur_complement = false;
			use_postordering = false;
			dynamic_sparsity = false;
			min_linear_solver_iterations = 0;
			max_linear_solver_iterations = 500;
			eta = 1e-1;
			jacobi_scaling = true;
			use_inner_iterations = false;
			inner_iteration_tolerance = 1e-3;
			logging_type = LoggingType.PER_MINIMIZER_ITERATION;
			minimizer_progress_to_stdout = false;
			trust_region_minimizer_iterations_to_dump = new Vector<Integer>();
			trust_region_problem_dump_directory = "/tmp";
			trust_region_problem_dump_format_type = DumpFormatType.TEXTFILE;
			check_gradients = false;
			gradient_check_relative_precision = 1e-8;
			gradient_check_numeric_derivative_relative_step_size = 1e-6;
			update_state_every_iteration = false;
			// Callbacks that are executed at the end of each iteration of the
			// Minimizer. An iteration may terminate midway, either due to
			// numerical failures or because one of the convergence tests has
			// been satisfied. In this case none of the callbacks are
			// executed.

			// Callbacks are executed in the order that they are specified in
			// this vector. By default, parameter blocks are updated only at the
			// end of the optimization, i.e when the Minimizer terminates. This
			// behaviour is controlled by update_state_every_iteration. If the
			// user wishes to have access to the updated parameter blocks when
			// his/her callbacks are executed, then set
			// update_state_every_iteration to true.
			//
			// The solver does NOT take ownership of these pointers.
			callbacks = new Vector<IterationCallback>();

			// The solver does NOT take ownership of these pointers.
			evaluation_callback = null;
		}

		public boolean IsValid(String message[]) {
			if (!CommonOptionsAreValid(message)) {
				return false;
			}

			if (minimizer_type == MinimizerType.TRUST_REGION && !TrustRegionOptionsAreValid(message)) {
				return false;
			}

			// We do not know if the problem is bounds constrained or not, if it
			// is then the trust region solver will also use the line search
			// solver to do a projection onto the box constraints, so make sure
			// that the line search options are checked independent of what
			// minimizer algorithm is being used.
			return LineSearchOptionsAreValid(message);
		}

		private boolean CommonOptionsAreValid(String message[]) {
			if (max_num_iterations < 0) {
				message[0] = "max_num_iterations incorrectly < 0";
				return false;
			}
			if (max_solver_time_in_seconds < 0.0) {
				message[0] = "max_solver_time_in_seconds incorrectly < 0.0";
				return false;
			}
			if (function_tolerance < 0.0) {
			    message[0] = "function_tolerance incorrectly < 0.0";
				return false;
			}
			if (gradient_tolerance < 0.0) {
				message[0] = "gradient_tolerance incorrectly < 0.0";
				return false;
			}
			if (parameter_tolerance < 0.0) {
				message[0] = "parameter_tolerance inocrrectly < 0.0";
				return false;
			}
			if (num_threads <= 0) {
				message[0] = "num_threads incorrectly <= 0";
				return false;
			}
			if (check_gradients) {
				if (gradient_check_relative_precision <= 0.0) {
					message[0] = "gradient_check_relative_precision incorrectly < = 0.0";
					return false;
				}
				if (gradient_check_numeric_derivative_relative_step_size <= 0.0) {
					message[0] = "gradient_check_numeric_derivative_relative_step_size incorrectly <= 0.0";
					return false;
				}
			}
			return true;
		}

		public boolean TrustRegionOptionsAreValid(String message[]) {
			if (initial_trust_region_radius <= 0.0) {
				message[0] = "initial_trust_region_radius incorrectly <= 0.0";
				return false;
			}
			if (min_trust_region_radius <= 0.0) {
				message[0] = "min_trust_region_radius incorrectly <= 0.0";
				return false;
			}
			if (max_trust_region_radius <= 0.0) {
				message[0] = "max_trust_region_radius incorrectly <= 0.0";
				return false;
			}
			if (min_trust_region_radius > max_trust_region_radius) {
				message[0] = "min_trust_region_radius incorrectly > max_trust_region_radius";
				return false;
			}
			if (min_trust_region_radius > initial_trust_region_radius) {
				message[0] = "min_trust_region_radius incorrectly > initial_trust_region_radius";
				return false;
			}
			if (initial_trust_region_radius > max_trust_region_radius) {
				message[0] = "initial_trust_region_radius incorrectly > max_trust_region_radius";
				return false;
			}
			if (min_relative_decrease < 0.0) {
				message[0] = "min_relative_decrease incorrectly < 0.0";
				return false;
			}
			if (min_lm_diagonal < 0.0) {
				message[0] = "min_lm_diagonal incorrectly < 0.0";
				return false;
			}
			if (max_lm_diagonal < 0.0) {
				message[0] = "max_lm_diagonal incorrectly < 0.0";
				return false;
			}
			if (min_lm_diagonal > max_lm_diagonal) {
			    message[0] = "min_lm_diagonal incorrectly > max_lm_diagonal";
				return false;
			}
			if (max_num_consecutive_invalid_steps < 0) {
				message[0] = "max_num_consecutive_invalid_steps incorrectly < 0";
				return false;
			}
			if (eta <= 0.0) {
				message[0] = "eta incorrectly <= 0.0";
				return false;
			}
			if (min_linear_solver_iterations < 0) {
				message[0] = "min_linear_solver_iterations incorrectly < 0";
				return false;
			}
			if (max_linear_solver_iterations < 1) {
				message[0] = "max_linear_solver_iterations incorrectly < 1";
				return false;
			}
			if (min_linear_solver_iterations > max_linear_solver_iterations) {
				message[0] = "min_linear_solver_iterations incorrectly > max_linear_solver_iterations";
				return false;
			}

			if (use_inner_iterations) {
				if (inner_iteration_tolerance < 0.0) {
					message[0] = "inner_iteration_tolerance incorrectly < 0.0";
					return false;
				}
			}

			/*
			 * if (options.use_inner_iterations && options.evaluation_callback != NULL) {
			 * error = "Inner iterations (use_inner_iterations = true) can't be "
			 * "combined with an evaluation callback "
			 * "(options.evaluation_callback != NULL)."; return false; }
			 */

			if (use_nonmonotonic_steps) {
				if (max_consecutive_nonmonotonic_steps <= 0) {
					message[0] = "max_consecutive_nonmonotonic_steps incorrectly <= 0";
					return false;
				}
			}

			if (linear_solver_type == LinearSolverType.ITERATIVE_SCHUR && use_explicit_schur_complement
					&& preconditioner_type != PreconditionerType.SCHUR_JACOBI) {
				message[0] = "use_explicit_schur_complement only supports SCHUR_JACOBI as the preconditioner.";
				return false;
			}

			if (preconditioner_type == PreconditionerType.CLUSTER_JACOBI
					&& sparse_linear_algebra_library_type != SparseLinearAlgebraLibraryType.SUITE_SPARSE) {
				message[0] = "CLUSTER_JACOBI requires Solver::Options::sparse_linear_algebra_library_type to be SUITE_SPARSE.";
				return false;
			}

			if (preconditioner_type == PreconditionerType.CLUSTER_TRIDIAGONAL
					&& sparse_linear_algebra_library_type != SparseLinearAlgebraLibraryType.SUITE_SPARSE) {
				message[0] = "CLUSTER_TRIDIAGONAL requires Solver::Options::sparse_linear_algebra_library_type to be SUITE_SPARSE.";
				return false;
			}

			/*
			 * #ifdef CERES_NO_LAPACK if (options.dense_linear_algebra_library_type ==
			 * LAPACK) { if (options.linear_solver_type == DENSE_NORMAL_CHOLESKY) { error =
			 * "Can't use DENSE_NORMAL_CHOLESKY with LAPACK because "
			 * "LAPACK was not enabled when Ceres was built."; return false; } else if
			 * (options.linear_solver_type == DENSE_QR) { error =
			 * "Can't use DENSE_QR with LAPACK because "
			 * "LAPACK was not enabled when Ceres was built."; return false; } else if
			 * (options.linear_solver_type == DENSE_SCHUR) { error =
			 * "Can't use DENSE_SCHUR with LAPACK because "
			 * "LAPACK was not enabled when Ceres was built."; return false; } } #endif
			 * 
			 * #ifdef CERES_NO_SUITESPARSE if (options.sparse_linear_algebra_library_type ==
			 * SUITE_SPARSE) { if (options.linear_solver_type == SPARSE_NORMAL_CHOLESKY) {
			 * error = "Can't use SPARSE_NORMAL_CHOLESKY with SUITESPARSE because "
			 * "SuiteSparse was not enabled when Ceres was built."; return false; } else if
			 * (options.linear_solver_type == SPARSE_SCHUR) { error =
			 * "Can't use SPARSE_SCHUR with SUITESPARSE because "
			 * "SuiteSparse was not enabled when Ceres was built."; return false; } else if
			 * (options.preconditioner_type == CLUSTER_JACOBI) { error =
			 * "CLUSTER_JACOBI preconditioner not supported. "
			 * "SuiteSparse was not enabled when Ceres was built."; return false; } else if
			 * (options.preconditioner_type == CLUSTER_TRIDIAGONAL) { error =
			 * "CLUSTER_TRIDIAGONAL preconditioner not supported. "
			 * "SuiteSparse was not enabled when Ceres was built."; return false; } } #endif
			 * 
			 * #ifdef CERES_NO_CXSPARSE if (options.sparse_linear_algebra_library_type ==
			 * CX_SPARSE) { if (options.linear_solver_type == SPARSE_NORMAL_CHOLESKY) {
			 * error = "Can't use SPARSE_NORMAL_CHOLESKY with CX_SPARSE because "
			 * "CXSparse was not enabled when Ceres was built."; return false; } else if
			 * (options.linear_solver_type == SPARSE_SCHUR) { error =
			 * "Can't use SPARSE_SCHUR with CX_SPARSE because "
			 * "CXSparse was not enabled when Ceres was built."; return false; } } #endif
			 * 
			 * #ifndef CERES_USE_EIGEN_SPARSE if (options.sparse_linear_algebra_library_type
			 * == EIGEN_SPARSE) { if (options.linear_solver_type == SPARSE_NORMAL_CHOLESKY)
			 * { error = "Can't use SPARSE_NORMAL_CHOLESKY with EIGEN_SPARSE because "
			 * "Eigen's sparse linear algebra was not enabled when Ceres was " "built.";
			 * return false; } else if (options.linear_solver_type == SPARSE_SCHUR) { error
			 * = "Can't use SPARSE_SCHUR with EIGEN_SPARSE because "
			 * "Eigen's sparse linear algebra was not enabled when Ceres was " "built.";
			 * return false; } } #endif
			 */

			if (sparse_linear_algebra_library_type == SparseLinearAlgebraLibraryType.NO_SPARSE) {
				if (linear_solver_type == LinearSolverType.SPARSE_NORMAL_CHOLESKY) {
					message[0] = "Can't use SPARSE_NORMAL_CHOLESKY as sparse_linear_algebra_library_type is NO_SPARSE.";
					return false;
				} else if (linear_solver_type == LinearSolverType.SPARSE_SCHUR) {
					message[0] = "Can't use SPARSE_SCHUR as sparse_linear_algebra_library_type is NO_SPARSE.";
					return false;
				}
			}

			if (trust_region_strategy_type == TrustRegionStrategyType.DOGLEG) {
				if (linear_solver_type == LinearSolverType.ITERATIVE_SCHUR
						|| linear_solver_type == LinearSolverType.CGNR) {
					message[0] = "DOGLEG only supports exact factorization based linear solvers.\n";
					message[0] += "If you want to use an iterative solver please \n";
					message[0] += "use LEVENBERG_MARQUARDT as the trust_region_strategy_type";
					return false;
				}
			}

			if (trust_region_minimizer_iterations_to_dump != null
					&& trust_region_minimizer_iterations_to_dump.size() > 0
					&& trust_region_problem_dump_format_type != DumpFormatType.CONSOLE
					&& ((trust_region_problem_dump_directory == null)
							|| (trust_region_problem_dump_directory.length() == 0))) {
				message[0] = "Solver::Options::trust_region_problem_dump_directory is empty.";
				return false;
			}

			if (dynamic_sparsity && linear_solver_type != LinearSolverType.SPARSE_NORMAL_CHOLESKY) {
				message[0] = "Dynamic sparsity is only supported with SPARSE_NORMAL_CHOLESKY.";
				return false;
			}

			return true;
		}

		public boolean LineSearchOptionsAreValid(String[] message) {
			if (max_lbfgs_rank <= 0) {
				message[0] = "max_lbfgs_rank incorrectly <= 0";
				return false;
			}
			if (min_line_search_step_size <= 0.0) {
				message[0] = "min_line_search_step_size incorrectly <= 0.0";
				return false;
			}
			if (max_line_search_step_contraction <= 0.0) {
				message[0] = "max_line_search_step_contraction incorrectly <= 0.0";
				return false;
			}
			if (max_line_search_step_contraction >= 1.0) {
				message[0] = "max_line_search_step_contraction incorrectly >= 1.0";
				return false;
			}
			if (max_line_search_step_contraction >= min_line_search_step_contraction) {
				message[0] = "max_line_search_step_contraction incorrectly >= min_line_search_step_contraction";
				return false;
			}
			if (min_line_search_step_contraction > 1.0) {
				message[0] = "min_line_search_step_contraction incorrectly > 1.0";
				return false;
			}
			if (max_num_line_search_step_size_iterations <= 0) {
				message[0] = "max_num_line_search_step_size_iterations incorrectly <= 0";
				return false;
			}
			if (line_search_sufficient_function_decrease <= 0.0) {
				message[0] = "line_search_sufficient_function_decrease incorrectly <= 0.0";
				return false;
			}
			if (line_search_sufficient_function_decrease >= line_search_sufficient_curvature_decrease) {
				message[0] = "line_search_sufficient_function_decrease incorrectly >= line_search_sufficient_curvature_decrease";
				return false;
			}
			if (line_search_sufficient_curvature_decrease >= 1.0) {
				message[0] = "line_search_sufficient_curvature_decrease incorrectly >= 1.0";
				return false;
			}
			if (max_line_search_step_expansion <= 1.0) {
				message[0] = "max_line_search_step_expansion incorrectly <= 1.0";
				return false;
			}

			if ((line_search_direction_type == LineSearchDirectionType.BFGS
					|| line_search_direction_type == LineSearchDirectionType.LBFGS)
					&& line_search_type != LineSearchType.WOLFE) {
				message[0] = "Invalid configuration: Solver::Options::line_search_type = "
						+ LineSearchTypeToString(line_search_type) + "\n";
				message[0] += "When using (L)BFGS, Solver::Options::line_search_type must be set to WOLFE.";
				return false;
			}

			// Warn user if they have requested BISECTION interpolation, but constraints
			// on max/min step size change during line search prevent bisection scaling
			// from occurring. Warn only, as this is likely a user mistake, but one which
			// does not prevent us from continuing.
			// LOG_IF(WARNING,
			if (line_search_interpolation_type == LineSearchInterpolationType.BISECTION
					&& (max_line_search_step_contraction > 0.5 || min_line_search_step_contraction < 0.5)) {
				message[0] = "Warning, likely a user mistake but can continue\n";
				message[0] += "Line search interpolation type is BISECTION, but specified\n";
				message[0] += "max_line_search_step_contraction: " + max_line_search_step_contraction
						+ ", and\n";
				message[0] += "min_line_search_step_contraction: " + min_line_search_step_contraction
						+ ",\n";
				message[0] += "prevent bisection (0.5) scaling, continuing with solve regardless.";
			}

			return true;
		}

	} // class SolverOptions

	class Solver {
		public SolverOptions options;
		public SolverSummary summary;

		public Solver() {
			options = new SolverOptions();
			summary = new SolverSummary();
		}

			
	} // class Solver

	class GradientCheckingIterationCallback extends IterationCallback {
		private boolean gradient_error_detected_;
		private String[] error_log_;

		public GradientCheckingIterationCallback() {
			super();
			gradient_error_detected_ = false;
		}

		// Retrieve error status (not thread safe).
		public boolean gradient_error_detected() {
			return gradient_error_detected_;
		}

		public String[] error_log() {
			return error_log_;
		}
		
		public CallbackReturnType operator(
			    IterationSummary summary) {
			  if (gradient_error_detected_) {
			    System.err.println("Gradient error detected. Terminating solver.");
			    return CallbackReturnType.SOLVER_ABORT;
			  }
			  return CallbackReturnType.SOLVER_CONTINUE;
		}

	}

	abstract class EvaluationCallback {
		public EvaluationCallback() {

		}
		
		// Called before Ceres requests residuals or jacobians for a given setting of
		  // the parameters. User parameters (the double* values provided to the cost
		  // functions) are fixed until the next call to PrepareForEvaluation(). If
		  // new_evaluation_point == true, then this is a new point that is different
		  // from the last evaluated point. Otherwise, it is the same point that was
		  // evaluated previously (either jacobian or residual) and the user can use
		  // cached results from previous evaluations.
		  public abstract void PrepareForEvaluation(boolean evaluate_jacobians,
		                                    boolean new_evaluation_point);
	}

	abstract class IterationCallback {
		public IterationCallback() {

		}
		
		public abstract CallbackReturnType operator(IterationSummary summary);
	}

	class GradientCheckingCostFunction extends CostFunction {
		private CostFunction function_;
		private GradientChecker gradient_checker_;
		private double relative_precision_;
		private String extra_info_;
		private GradientCheckingIterationCallback callback_;

		public GradientCheckingCostFunction(CostFunction function,
				Vector<LocalParameterization> local_parameterizations, NumericDiffOptions options,
				double relative_precision, String extra_info, GradientCheckingIterationCallback callback) {
			function_ = function;
			gradient_checker_ = new GradientChecker(function, local_parameterizations, options);
			relative_precision_ = relative_precision;
			extra_info_ = extra_info;
			callback_ = callback;
			if (callback == null) {
				System.err.println("callback == null in GradientCheckingCostFunction constructor");
				return;
			}
			Vector<Integer> parameter_block_sizes = function.parameter_block_sizes();
			set_parameter_block_sizes(parameter_block_sizes);
			set_num_residuals(function.num_residuals());

		}

	}

	class GradientChecker {
		private Vector<LocalParameterization> local_parameterizations_;
		private CostFunction function_;
		// internal::scoped_ptr<CostFunction> finite_diff_cost_function_
		private CostFunction finite_diff_cost_function_;

		public GradientChecker(CostFunction function, Vector<LocalParameterization> local_parameterizations,
				NumericDiffOptions options) {
			function_ = function;
			if (function == null) {
				System.err.println("CostFunction function = null in GradientChecker constructor");
				return;
			}
			if (local_parameterizations != null) {
				local_parameterizations_ = local_parameterizations;
			} else {
				// local_parameterizations == null so local_parameterizations has size == 0
				// local_parameterizations_.resize(function.parameter_block_sizes().size(),
				// null);
				int newSize = function.parameter_block_sizes().size();
				int oldSize = 0; // local_parameterizations.size();
				if (newSize > oldSize) {
					int numAdd = newSize - oldSize;
					for (int i = 0; i < numAdd; i++) {
						local_parameterizations_.add(null);
					}
				}
			}
			DynamicNumericDiffCostFunction<CostFunction> finite_diff_cost_function = new DynamicNumericDiffCostFunction<CostFunction>(
					function, Ownership.DO_NOT_TAKE_OWNERSHIP, options, NumericDiffMethodType.CENTRAL);
			finite_diff_cost_function_ = finite_diff_cost_function;

			Vector<Integer> parameter_block_sizes = function.parameter_block_sizes();
			int num_parameter_blocks = parameter_block_sizes.size();
			for (int i = 0; i < num_parameter_blocks; ++i) {
				finite_diff_cost_function.AddParameterBlock(parameter_block_sizes.get(i));
			}
			finite_diff_cost_function.SetNumResiduals(function.num_residuals());

		}
	}

	class DynamicNumericDiffCostFunction<T> extends DynamicCostFunction {
		// internal::scoped_ptr<const CostFunctor> functor_;
		T functor_;
		Ownership ownership_;
		NumericDiffOptions options_;
		NumericDiffMethodType method;

		public DynamicNumericDiffCostFunction(T functor, Ownership ownership, NumericDiffOptions options,
				NumericDiffMethodType method) {
			super();
			functor_ = functor;
			ownership_ = ownership;
			options_ = options;
			this.method = method;
		}
	}

	// A common base class for DynamicAutoDiffCostFunction and
	// DynamicNumericDiffCostFunction which depend on methods that can add
	// parameter blocks and set the number of residuals at run time.
	class DynamicCostFunction extends CostFunction {
		public DynamicCostFunction() {
			super();
		}

	}

	public void InvalidateArray(int size, double x[]) {
		if (x != null) {
			for (int i = 0; i < size; ++i) {
				x[i] = kImpossibleValue;
			}
		}
	}
	
	public void InvalidateArray(int size, double x[], int x_offset) {
		if (x != null) {
			for (int i = 0; i < size; ++i) {
				x[x_offset+ i] = kImpossibleValue;
			}
		}
	}
	
	public void InvalidateArray(int row, int col, double x[][]) {
		int r,c;
		if (x != null) {
			for (r = 0; r < row; r++) {
				for (c = 0; c < col; c++) {
					x[r][c] = kImpossibleValue;
				}
			}
		}
	}
	
	public void InvalidateArray(int size, Vector<Double>x) {
		if (x != null) {
			for (int i = 0; i < size; ++i) {
				x.set(i,kImpossibleValue);
			}
		}
	}

	public boolean IsArrayValid(int size, double x[]) {
		if (x != null) {
			for (int i = 0; i < size; ++i) {
				if (!Double.isFinite(x[i]) || (x[i] == kImpossibleValue)) {
					return false;
				}
			}
		}
		return true;
	}
	
	public boolean IsArrayValid(int size, double x[], int x_offset) {
		if (x != null) {
			for (int i = 0; i < size; ++i) {
				if (!Double.isFinite(x[x_offset+i]) || (x[x_offset+i] == kImpossibleValue)) {
					return false;
				}
			}
		}
		return true;
	}
	
	public boolean IsArrayValid(int row, int col, double x[][]) {
		int r,c;
		if (x != null) {
			for (r = 0; r < row; r++) {
				for (c = 0; c < col; c++) {
					if (!Double.isFinite(x[r][c]) || (x[r][c] == kImpossibleValue)) {
						return false;
					}
				}
			}
		}
		return true;
	}
	
	public boolean IsArrayValid(int size, Vector<Double>x) {
		if (x != null) {
			for (int i = 0; i < size; ++i) {
				if (!Double.isFinite(x.get(i)) || (x.get(i) == kImpossibleValue)) {
					return false;
				}
			}
		}
		return true;
	}

	int FindInvalidValue(int size, double[] x) {
		if (x == null) {
			return size;
		}
		for (int i = 0; i < size; ++i) {
			if (!Double.isFinite(x[i]) || (x[i] == kImpossibleValue)) {
				return i;
			}
		}
		return size;
	}

	public void AppendArrayToString(int size, double x[], String result[]) {
		for (int i = 0; i < size; ++i) {
			if (x == null) {
				result[0] = result[0] + "Not Computed  ";
				return;
			} else {
				if (x[i] == kImpossibleValue) {
					result[0] = result[0] + "Uninitialized ";
				} else {
					result[0] = result[0] + x[i] + " ";
				}
			}
		}
	}

	public void MatrixTransposeVectorMultiply(int kRowA, int kColA, int kOperation, double[] A, int Astart,
			int num_row_a, int num_col_a, double[] b, int bstart, double[] c, int cstart) {
		if (num_row_a <= 0) {
			System.err.println("In MatrixTransposeVectorMultiply num_row_a <= 0");
			return;
		}
		if (num_col_a <= 0) {
			System.err.println("In MatrixTransposeVectorMultiply num_col_a <= 0");
			return;
		}
		if ((kRowA != DYNAMIC) && (kRowA != num_row_a)) {
			System.err.println("In MatrixTransposeVectorMultiply (kRowA != DYNAMIC) && (kRowA != num_row_a)");
			return;
		}
		if ((kColA != DYNAMIC) && (kColA != num_col_a)) {
			System.err.println("In MatrixTransposeVectorMultiply (kColA != DYNAMIC) && (kColA != num_col_a)");
			return;
		}

		int NUM_ROW_A = (kRowA != DYNAMIC ? kRowA : num_row_a);
		int NUM_COL_A = (kColA != DYNAMIC ? kColA : num_col_a);

		for (int row = 0; row < NUM_COL_A; ++row) {
			double tmp = 0.0;
			for (int col = 0; col < NUM_ROW_A; ++col) {
				tmp += A[Astart + col * NUM_COL_A + row] * b[bstart + col];
			}

			if (kOperation > 0) {
				c[cstart + row] += tmp;
			} else if (kOperation < 0) {
				c[cstart + row] -= tmp;
			} else {
				c[cstart + row] = tmp;
			}
		}
	}

	public void MatrixVectorMultiply(int kRowA, int kColA, int kOperation, double[] A, int Astart, int num_row_a,
			int num_col_a, double[] b, int bstart, double[] c, int cstart) {
		if (num_row_a <= 0) { 
			System.err.println("In MatrixVectorMultiply num_row_a <= 0");
			return;
		}
		if (num_col_a <= 0) {
			System.err.println("In MatrixVectorMultiply num_col_a <= 0");
			return;
		}
		if ((kRowA != DYNAMIC) && (kRowA != num_row_a)) {
			System.err.println("In MatrixVectorMultiply (kRowA != DYNAMIC) && (kRowA != num_row_a)");
			return;
		}
		if ((kColA != DYNAMIC) && (kColA != num_col_a)) {
			System.err.println("In MatrixVectorMultiply (kColA != DYNAMIC) && (kColA != num_col_a)");
			return;
		}

		int NUM_ROW_A = (kRowA != DYNAMIC ? kRowA : num_row_a);
		int NUM_COL_A = (kColA != DYNAMIC ? kColA : num_col_a);

		for (int row = 0; row < NUM_ROW_A; ++row) {
			double tmp = 0.0;
			for (int col = 0; col < NUM_COL_A; ++col) {
				tmp += A[Astart + row * NUM_COL_A + col] * b[bstart + col];
			}

			if (kOperation > 0) {
				c[cstart + row] += tmp;
			} else if (kOperation < 0) {
				c[cstart + row] -= tmp;
			} else {
				c[cstart + row] = tmp;
			}
		}
	}
	
	public void MatrixTransposeMatrixMultiply(int kRowA, int kColA, int kRowB, int kColB, int kOperation,
	  double[] A, int A_start, int num_row_a, int num_col_a, double[] B, int B_start, int num_row_b, int num_col_b, double[] C,
	  int C_start, int start_row_c, int start_col_c, int row_stride_c, int col_stride_c) {
		  if (num_row_a <= 0) {
			  System.err.println("num_row_a <= 0 in MatrixTransposeMatrixMultiply");
			  return;
		  }
		  if (num_col_a <= 0) {
			  System.err.println("num_col_a <= 0 in MatrixTransposeMatrixMultiply");
			  return;
		  }                                          
		  if (num_row_b <= 0) {
			  System.err.println("num_row_b <= 0 in MatrixTransposeMatrixMultiply");
			  return;
		  }
		  if (num_col_b <= 0) {
			  System.err.println("num_col_b <= 0 in MatrixTransposeMatrixMultiply");
			  return;
		  } 
		  if (start_row_c < 0) {
			  System.err.println("start_row_c < 0 in MatrixTransposeMatrixMultiply");
			  return;
		  }
		  if (start_col_c < 0) {
			  System.err.println("start_col_c < 0 in MatrixTransposeMatrixMultiply");
			  return;
		  } 
		  if (row_stride_c <= 0) {
			  System.err.println("row_stride_c <= 0 in MatrixTransposeMatrixMultiply");
			  return;
		  }
		  if (col_stride_c <= 0) {
			  System.err.println("col_stride_c <= 0 in MatrixTransposeMatrixMultiply");
			  return;
		  }
		  if ((kRowA != DYNAMIC) && (kRowA != num_row_a)) {
			  System.err.println("(kRowA != DYNAMIC) && (kRowA != num_row_a) in MatrixTransposeMatrixMultiply");
			  return;
		  }
		  if ((kColA != DYNAMIC) && (kColA != num_col_a)) {
			  System.err.println("(kColA != DYNAMIC) && (kColA != num_col_a) in MatrixTransposeMatrixMultiply");
			  return;
		  }
		  if ((kRowB != DYNAMIC) && (kRowB != num_row_b)) {
			  System.err.println("(kRowB != DYNAMIC) && (kRowB != num_row_b) in MatrixTransposeMatrixMultiply");
			  return;
		  }
		  if ((kColB != DYNAMIC) && (kColB != num_col_b)) {
			  System.err.println("(kColB != DYNAMIC) && (kColB != num_col_b) in MatrixTransposeMatrixMultiply");
			  return;
		  }
		  int NUM_ROW_A = (kRowA != DYNAMIC ? kRowA : num_row_a);
		  int NUM_COL_A = (kColA != DYNAMIC ? kColA : num_col_a);
		  int NUM_ROW_B = (kRowB != DYNAMIC ? kRowB : num_row_b);
		  int NUM_COL_B = (kColB != DYNAMIC ? kColB : num_col_b);
		  if (NUM_ROW_A != NUM_ROW_B) {
			  System.err.println("NUM_ROW_A != NUM_ROW_B in MatrixTransposeMatrixMultiply");
			  return;
		  }

		  int NUM_ROW_C = NUM_COL_A;
		  int NUM_COL_C = NUM_COL_B;
		  if ((start_row_c + NUM_ROW_C) > row_stride_c) {
			  System.err.println("(start_row_c + NUM_ROW_C) > row_stride_c in MatrixTransposeMatrixMultiply");
			  return;
		  }
		  if ((start_col_c + NUM_COL_C) > col_stride_c) {
			  System.err.println("(start_col_c + NUM_COL_C) > col_stride_c in MatrixTransposeMatrixMultiply");
			  return;
		  }

		  for (int row = 0; row < NUM_ROW_C; ++row) {
		    for (int col = 0; col < NUM_COL_C; ++col) {
		      double tmp = 0.0;
		      for (int k = 0; k < NUM_ROW_A; ++k) {
		        tmp += A[A_start + k * NUM_COL_A + row] * B[B_start + k * NUM_COL_B + col];
		      }

		      int index = (row + start_row_c) * col_stride_c + start_col_c + col;
		      if (kOperation > 0) {
		        C[C_start + index]+= tmp;
		      } else if (kOperation < 0) {
		        C[C_start + index]-= tmp;
		      } else {
		        C[C_start + index]= tmp;
		      }
		    }
		  }	
	}
	
	public void MatrixMatrixMultiply(int kRowA, int kColA, int kRowB, int kColB, int kOperation,
			  double[] A, int A_start, int num_row_a, int num_col_a, double[] B, int B_start, int num_row_b, int num_col_b, double[] C,
			  int C_start, int start_row_c, int start_col_c, int row_stride_c, int col_stride_c) {
				  if (num_row_a <= 0) {
					  System.err.println("num_row_a <= 0 in MatrixMatrixMultiply");
					  return;
				  }
				  if (num_col_a <= 0) {
					  System.err.println("num_col_a <= 0 in MatrixMatrixMultiply");
					  return;
				  }                                          
				  if (num_row_b <= 0) {
					  System.err.println("num_row_b <= 0 in MatrixMatrixMultiply");
					  return;
				  }
				  if (num_col_b <= 0) {
					  System.err.println("num_col_b <= 0 in MatrixMatrixMultiply");
					  return;
				  } 
				  if (start_row_c < 0) {
					  System.err.println("start_row_c < 0 in MatrixMatrixMultiply");
					  return;
				  }
				  if (start_col_c < 0) {
					  System.err.println("start_col_c < 0 in MatrixMatrixMultiply");
					  return;
				  } 
				  if (row_stride_c <= 0) {
					  System.err.println("row_stride_c <= 0 in MatrixMatrixMultiply");
					  return;
				  }
				  if (col_stride_c <= 0) {
					  System.err.println("col_stride_c <= 0 in MatrixMatrixMultiply");
					  return;
				  }
				  if ((kRowA != DYNAMIC) && (kRowA != num_row_a)) {
					  System.err.println("(kRowA != DYNAMIC) && (kRowA != num_row_a) in MatrixMatrixMultiply");
					  return;
				  }
				  if ((kColA != DYNAMIC) && (kColA != num_col_a)) {
					  System.err.println("(kColA != DYNAMIC) && (kColA != num_col_a) in MatrixMatrixMultiply");
					  return;
				  }
				  if ((kRowB != DYNAMIC) && (kRowB != num_row_b)) {
					  System.err.println("(kRowB != DYNAMIC) && (kRowB != num_row_b) in MatrixMatrixMultiply");
					  return;
				  }
				  if ((kColB != DYNAMIC) && (kColB != num_col_b)) {
					  System.err.println("(kColB != DYNAMIC) && (kColB != num_col_b) in MatrixMatrixMultiply");
					  return;
				  }
				  int NUM_ROW_A = (kRowA != DYNAMIC ? kRowA : num_row_a);
				  int NUM_COL_A = (kColA != DYNAMIC ? kColA : num_col_a);
				  int NUM_ROW_B = (kRowB != DYNAMIC ? kRowB : num_row_b);
				  int NUM_COL_B = (kColB != DYNAMIC ? kColB : num_col_b);
				  if (NUM_COL_A != NUM_ROW_B) {
					  System.err.println("NUM_COL_A != NUM_ROW_B in MatrixMatrixMultiply");
					  return;
				  }

				  int NUM_ROW_C = NUM_ROW_A;
				  int NUM_COL_C = NUM_COL_B;
				  if ((start_row_c + NUM_ROW_C) > row_stride_c) {
					  System.err.println("(start_row_c + NUM_ROW_C) > row_stride_c in MatrixMatrixMultiply");
					  return;
				  }
				  if ((start_col_c + NUM_COL_C) > col_stride_c) {
					  System.err.println("(start_col_c + NUM_COL_C) > col_stride_c in MatrixMatrixMultiply");
					  return;
				  }

				  for (int row = 0; row < NUM_ROW_C; ++row) {
				    for (int col = 0; col < NUM_COL_C; ++col) {
				      double tmp = 0.0;
				      for (int k = 0; k < NUM_COL_A; ++k) {
				        tmp += A[A_start + row * NUM_COL_A + k] * B[B_start + k * NUM_COL_B + col];
				      }

				      int index = (row + start_row_c) * col_stride_c + start_col_c + col;
				      if (kOperation > 0) {
				        C[C_start + index]+= tmp;
				      } else if (kOperation < 0) {
				        C[C_start + index]-= tmp;
				      } else {
				        C[C_start + index]= tmp;
				      }
				    }
				  }	
			}

	public void MatrixMatrixMultiply(int kOperation, double A[], int NUM_ROW_A, int NUM_COL_A, double B[],
			int NUM_ROW_B, int NUM_COL_B, double C[], int start_row_c, int start_col_c, int row_stride_c,
			int col_stride_c) {
		if (NUM_COL_A != NUM_ROW_B) {
			System.err.println("In MatrixMatrixMultiply NUM_COL_A != NUM_ROW_B");
			return;
		}

		int NUM_ROW_C = NUM_ROW_A;
		int NUM_COL_C = NUM_COL_B;
		if (start_row_c + NUM_ROW_C > row_stride_c) {
			System.err.println("In MatrixMatrixMultiply start_row_C + NUM_ROW_C > row_stride_c");
			return;
		}
		if (start_col_c + NUM_COL_C > col_stride_c) {
			System.err.println("In MatrixMatrixMultiply start_col_C + NUM_COL_C > col_stride_c");
			return;
		}

		for (int row = 0; row < NUM_ROW_C; ++row) {
			for (int col = 0; col < NUM_COL_C; ++col) {
				double tmp = 0.0;
				for (int k = 0; k < NUM_COL_A; ++k) {
					tmp += A[row * NUM_COL_A + k] * B[k * NUM_COL_B + col];
				}

				int index = (row + start_row_c) * col_stride_c + start_col_c + col;
				if (kOperation > 0) {
					C[index] += tmp;
				} else if (kOperation < 0) {
					C[index] -= tmp;
				} else {
					C[index] = tmp;
				}
			}
		}
	}
	
	public void MatrixMatrixMultiply(int kOperation, double A[], int NUM_ROW_A, int NUM_COL_A, double B[][],
			int NUM_ROW_B, int NUM_COL_B, double C[], int start_row_c, int start_col_c, int row_stride_c,
			int col_stride_c) {
		if (NUM_COL_A != NUM_ROW_B) {
			System.err.println("In MatrixMatrixMultiply NUM_COL_A != NUM_ROW_B");
			return;
		}

		int NUM_ROW_C = NUM_ROW_A;
		int NUM_COL_C = NUM_COL_B;
		if (start_row_c + NUM_ROW_C > row_stride_c) {
			System.err.println("In MatrixMatrixMultiply start_row_C + NUM_ROW_C > row_stride_c");
			return;
		}
		if (start_col_c + NUM_COL_C > col_stride_c) {
			System.err.println("In MatrixMatrixMultiply start_col_C + NUM_COL_C > col_stride_c");
			return;
		}

		for (int row = 0; row < NUM_ROW_C; ++row) {
			for (int col = 0; col < NUM_COL_C; ++col) {
				double tmp = 0.0;
				for (int k = 0; k < NUM_COL_A; ++k) {
					tmp += A[row * NUM_COL_A + k] * B[k][col];
				}

				int index = (row + start_row_c) * col_stride_c + start_col_c + col;
				if (kOperation > 0) {
					C[index] += tmp;
				} else if (kOperation < 0) {
					C[index] -= tmp;
				} else {
					C[index] = tmp;
				}
			}
		}
	}
	
	public void MatrixMatrixMultiply(int kOperation, double A[], int A_start, int NUM_ROW_A, int NUM_COL_A, double B[][],
			int NUM_ROW_B, int NUM_COL_B, double C[], int C_start, int start_row_c, int start_col_c, int row_stride_c,
			int col_stride_c) {
		if (NUM_COL_A != NUM_ROW_B) {
			System.err.println("In MatrixMatrixMultiply NUM_COL_A != NUM_ROW_B");
			return;
		}

		int NUM_ROW_C = NUM_ROW_A;
		int NUM_COL_C = NUM_COL_B;
		if (start_row_c + NUM_ROW_C > row_stride_c) {
			System.err.println("In MatrixMatrixMultiply start_row_C + NUM_ROW_C > row_stride_c");
			return;
		}
		if (start_col_c + NUM_COL_C > col_stride_c) {
			System.err.println("In MatrixMatrixMultiply start_col_C + NUM_COL_C > col_stride_c");
			return;
		}

		for (int row = 0; row < NUM_ROW_C; ++row) {
			for (int col = 0; col < NUM_COL_C; ++col) {
				double tmp = 0.0;
				for (int k = 0; k < NUM_COL_A; ++k) {
					tmp += A[A_start + row * NUM_COL_A + k] * B[k][col];
				}

				int index = C_start + (row + start_row_c) * col_stride_c + start_col_c + col;
				if (kOperation > 0) {
					C[index] += tmp;
				} else if (kOperation < 0) {
					C[index] -= tmp;
				} else {
					C[index] = tmp;
				}
			}
		}
	}
	
	

	String LineSearchTypeToString(LineSearchType type) {
		switch (type) {
		case ARMIJO:
			return "ARMIJO";
		case WOLFE:
			return "WOLFE";
		default:
			return "UNKNOWN";
		}
	}
	
	String LineSearchInterpolationTypeToString(
		    LineSearchInterpolationType type) {
		  switch (type) {
		  case BISECTION:
			  return "BISECTION";
		  case QUADRATIC:
		      return "QUADRATIC";
		  case CUBIC:
			  return "CUBIC";
		    default:
		      return "UNKNOWN";
		  }
		}


	ParameterBlock FindWithDefault(HashMap<double[], ParameterBlock> collection, double[] key, ParameterBlock value) {
		if (!collection.containsKey(key)) {
			return value;
		} else {
			return collection.get(key);
		}

	}
	
	CallStatistics FindWithDefault(HashMap<String, CallStatistics> collection, String key, CallStatistics value) {
		if (!collection.containsKey(key)) {
			return value;
		} else {
			return collection.get(key);
		}
	}
	
	public void MinimizeInterpolatingPolynomial(Vector<FunctionSample> samples,
            double x_min,
            double x_max,
            double[] optimal_x,
            double[] optimal_value) {
			Vector<Double> polynomial = FindInterpolatingPolynomial(samples);
			MinimizePolynomial(polynomial, x_min, x_max, optimal_x, optimal_value);
			for (int i = 0; i < samples.size(); ++i) {
			    FunctionSample sample = samples.get(i);
			    if ((sample.x < x_min) || (sample.x > x_max)) {
			        continue;
			    }
			
			    double value = EvaluatePolynomial(polynomial, sample.x);
			    if (value < optimal_value[0]) {
			        optimal_x[0] = sample.x;
			        optimal_value[0] = value;
			    }
			}
	} // public void MinimizeInterpolatingPolynomial
	
	public void MinimizePolynomial(Vector<Double> polynomial,
            double x_min,
            double x_max,
            double[] optimal_x,
            double[] optimal_value) {
			// Find the minimum of the polynomial at the two ends.
			//
			// We start by inspecting the middle of the interval. Technically
			// this is not needed, but we do this to make this code as close to
			// the minFunc package as possible.
			optimal_x[0] = (x_min + x_max) / 2.0;
			optimal_value[0] = EvaluatePolynomial(polynomial, optimal_x[0]);
			
			double x_min_value = EvaluatePolynomial(polynomial, x_min);
			if (x_min_value < optimal_value[0]) {
			    optimal_value[0] = x_min_value;
			    optimal_x[0] = x_min;
			}
			
			double x_max_value = EvaluatePolynomial(polynomial, x_max);
			if (x_max_value < optimal_value[0]) {
			    optimal_value[0] = x_max_value;
			    optimal_x[0] = x_max;
			}
			
			// If the polynomial is linear or constant, we are done.
			if (polynomial.size() <= 2) {
			return;
			}
			
			Vector<Double> derivative = DifferentiatePolynomial(polynomial);
			Vector<Double> roots_real = new Vector<Double>();
			if (!FindPolynomialRoots(derivative, roots_real, null)) {
		    if (WARNING <= MAX_LOG_LEVEL) {
			    Preferences.debug("Unable to find the critical points of \n" +
			     "the interpolating polynomial.\n",Preferences.DEBUG_ALGORITHM);
		    }
			return;
			}

			// This is a bit of an overkill, as some of the roots may actually
			// have a complex part, but its simpler to just check these values.
			for (int i = 0; i < roots_real.size(); ++i) {
			    double root = roots_real.get(i);
			    if ((root < x_min) || (root > x_max)) {
			        continue;
			    }
			
			    double value = EvaluatePolynomial(polynomial, root);
			    if (value < optimal_value[0]) {
			        optimal_value[0] = value;
			        optimal_x[0] = root;
			    }
			}
	} // public void MinimizePolynomial
	
	
	Vector<Double> DifferentiatePolynomial(Vector<Double> polynomial) {
		  int degree = polynomial.size() - 1;
		  if (degree < 0) {
			  System.err.println("degree < 0 in DifferentiatePolynomial");
			  return null;
		  }
		  
		  Vector<Double> derivative = new Vector<Double>();

		  // Degree zero polynomials are constants, and their derivative does
		  // not result in a smaller degree polynomial, just a degree zero
		  // polynomial with value zero.
		  if (degree == 0) {
			derivative.add(0.0);
		    return derivative;
		  }

		  for (int i = 0; i < degree; ++i) {
		    derivative.add((degree - i) * polynomial.get(i));
		  }

		  return derivative;
		}
	
	// Evaluate the polynomial at x using the Horner scheme.
	double EvaluatePolynomial(Vector<Double> polynomial, double x) {
	  double v = 0.0;
	  for (int i = 0; i < polynomial.size(); ++i) {
	    v = v * x + polynomial.get(i);
	  }
	  return v;
	}

	
	Vector<Double> FindInterpolatingPolynomial(Vector<FunctionSample> samples) {
		  int i;
		  int num_samples = samples.size();
		  int num_constraints = 0;
		  for (i = 0; i < num_samples; ++i) {
		    if (samples.get(i).value_is_valid) {
		      ++num_constraints;
		    }
		    if (samples.get(i).gradient_is_valid) {
		      ++num_constraints;
		    }
		  }

		  int degree = num_constraints - 1;

		  Matrix lhs = new Matrix(num_constraints, num_constraints);
		  Vector<Double>rhs = new Vector<Double>(num_constraints);
		  for (i = 0; i < num_constraints; i++) {
			  rhs.add(0.0);
		  }

		  int row = 0;
		  for (i = 0; i < num_samples; ++i) {
		    FunctionSample sample = samples.get(i);
		    if (sample.value_is_valid) {
		      for (int j = 0; j <= degree; ++j) {
		        lhs.set(row, j, Math.pow(sample.x, degree - j));
		      }
		      rhs.set(row,sample.value[0]);
		      ++row;
		    }

		    if (sample.gradient_is_valid) {
		      for (int j = 0; j < degree; ++j) {
		        lhs.set(row, j, (degree - j) * Math.pow(sample.x, degree - j - 1));
		      }
		      rhs.set(row,sample.gradient);
		      ++row;
		    }
		  }

		  // TODO(sameeragarwal): This is a hack.
		  // https://github.com/ceres-solver/ceres-solver/issues/248
		// Compute the LU decomposition of A in place
		  int m = num_constraints;
		  int n = num_constraints;
		  double lhs_array[][] = lhs.getArray();
		  int lda = m;
		  int ipiv[] = new int[m];
		  int info[] = new int[1];
	      le2.dgetrf(m, n, lhs_array, lda, ipiv, info);
	      if (info[0] < 0) {
	    	  System.err.println("In FindInterpolatingPolynomial dgetrf argument number " + 
	      (-info[0]) + " is illegal");
	    	  return null;
	      }
	      if (info[0] > 0) {
	    	  System.err.println("In FindInterpolatingPolynomial dgetrf U["+(info[0]-1)+
	    			  "]["+(info[0]-1)+"] is exactly 0");
	    	  return null;
	      }
	      char trans = 'N'; // Solve A*X = B (no transpose)
	      int nrhs = 1; // number of columns of B
	      double rhs_array[][] = new double[m][1];
	      for (i = 0; i < m; i++) {
	    	  rhs_array[i][0] = rhs.get(i);
	      }
	      int ldb = n;
	      le2.dgetrs(trans, n, nrhs, lhs_array, lda, ipiv,
					rhs_array, ldb, info);
	      if (info[0] < 0) {
	    	  System.err.println("In FindInterpolatingPolynomial dgetrs argument number " + 
	      (-info[0]) + " is illegal");
	    	  return null;
	      }
	      for (i = 0; i < n; i++) {
	    	  rhs.set(i,rhs_array[i][0]);
	      }
	      return rhs;
		  //Eigen::FullPivLU<Matrix> lu(lhs);
		  //return lu.setThreshold(0.0).solve(rhs);
		}
	
	public boolean FindPolynomialRoots(Vector<Double> polynomial, Vector<Double> roots_real, Vector<Double> roots_imag) {
		int i;
	    if (polynomial.size() == 0) {
	        System.err.println("Invalid polynomial of size 0 passed to FindPolynomialRoots");
	        return false;
	      }

	    Vector<Double>polynomial_leading_zeros_stripped = RemoveLeadingZeros(polynomial);
	    int degree = polynomial_leading_zeros_stripped.size() - 1;
	    
	    if (3 <= MAX_LOG_LEVEL) {
	        Preferences.debug("Input polynomial: \n", Preferences.DEBUG_ALGORITHM);
	        for (i = 0; i < polynomial.size(); i++) {
	        	Preferences.debug(polynomial.get(i) + "\n", Preferences.DEBUG_ALGORITHM);
	        }
	        if (polynomial.size() != polynomial_leading_zeros_stripped.size()) {
	            Preferences.debug("Trimmed polynomial: \n", Preferences.DEBUG_ALGORITHM);
	            	for (i = 0; i < polynomial_leading_zeros_stripped.size(); i++) {
			        	Preferences.debug(polynomial_leading_zeros_stripped.get(i) + "\n", Preferences.DEBUG_ALGORITHM);
			        }
	        }
	    }
	    
	    // Is the polynomial constant?
	    if (degree == 0) {
	      Preferences.debug("Trying to extract roots from a constant polynomial\n",
	    		  Preferences.DEBUG_ALGORITHM);
	      // We return true with no roots, not false, as if the polynomial is constant
	      // it is correct that there are no roots. It is not the case that they were
	      // there, but that we have failed to extract them.
	    }
	    else if (degree == 1) {
	    	FindLinearPolynomialRoots(polynomial_leading_zeros_stripped, roots_real, roots_imag);
	    }
	    else if (degree == 2) {
	    	FindQuadraticPolynomialRoots(polynomial_leading_zeros_stripped, roots_real, roots_imag);
	    }
	    else {
	    	// The degree is now known to be at least 3.
	    	// Divide by leading term
	    	double leading_term = polynomial_leading_zeros_stripped.get(0);
	    	for (i = 0; i < polynomial_leading_zeros_stripped.size(); i++) {
	    	  polynomial_leading_zeros_stripped.set(i, polynomial_leading_zeros_stripped.get(i)/leading_term);
	    	}
	    	double areal, aimag, breal, bimag, creal, cimag, dreal, dimag, ereal, eimag;
	    	int num_sols[] = new int[1];
	    	double solsreal[] = null;
	    	double solsimag[] = null;
	    	if (degree == 3) {
	    		areal = 0.0;
				aimag = 0.0;
				breal = 1.0;
				bimag = 0.0;
				creal = polynomial_leading_zeros_stripped.get(1);
				cimag = 0.0;
				dreal = polynomial_leading_zeros_stripped.get(2);
				dimag = 0.0;
				ereal = polynomial_leading_zeros_stripped.get(3);
				eimag = 0.0;
				solsreal = new double[3];
				solsimag = new double[3];	
	    	}
	    	else {
		    	areal = 1.0;
				aimag = 0.0;
				breal = polynomial_leading_zeros_stripped.get(1);
				bimag = 0.0;
				creal = polynomial_leading_zeros_stripped.get(2);
				cimag = 0.0;
				dreal = polynomial_leading_zeros_stripped.get(3);
				dimag = 0.0;
				ereal = polynomial_leading_zeros_stripped.get(4);
				eimag = 0.0;
				solsreal = new double[4];
				solsimag = new double[4];
	    	}
			QuarticEquation qe = new QuarticEquation(degree, areal, aimag, breal, bimag, creal, cimag,
					dreal, dimag, ereal, eimag, num_sols, solsreal, solsimag);
			qe.run();
			if (roots_real != null) {
				for (i = 0; i < num_sols[0]; i++) {
					roots_real.add(solsreal[i]);
				}
			}
			if (roots_imag != null) {
				for (i = 0; i < num_sols[0]; i++) {
					roots_imag.add(solsimag[i]);
				}
			}
	    }
	    return true;
	    }
	
	// The options structure contains, not surprisingly, options that control how
	  // the solver operates. The defaults should be suitable for a wide range of
	  // problems; however, better performance is often obtainable with tweaking.
	  //
	  // The constants are defined inside types.h
	  class GradientProblemSolverOptions {
		// Minimizer options ----------------------------------------
		    public LineSearchDirectionType line_search_direction_type;
		    public LineSearchType line_search_type;
		    public NonlinearConjugateGradientType nonlinear_conjugate_gradient_type;

		    // The LBFGS hessian approximation is a low rank approximation to
		    // the inverse of the Hessian matrix. The rank of the
		    // approximation determines (linearly) the space and time
		    // complexity of using the approximation. Higher the rank, the
		    // better is the quality of the approximation. The increase in
		    // quality is however is bounded for a number of reasons.
		    //
		    // 1. The method only uses secant information and not actual
		    // derivatives.
		    //
		    // 2. The Hessian approximation is constrained to be positive
		    // definite.
		    //
		    // So increasing this rank to a large number will cost time and
		    // space complexity without the corresponding increase in solution
		    // quality. There are no hard and fast rules for choosing the
		    // maximum rank. The best choice usually requires some problem
		    // specific experimentation.
		    //
		    // For more theoretical and implementation details of the LBFGS
		    // method, please see:
		    //
		    // Nocedal, J. (1980). "Updating Quasi-Newton Matrices with
		    // Limited Storage". Mathematics of Computation 35 (151): 773–782.
		    public int max_lbfgs_rank;

		    // As part of the (L)BFGS update step (BFGS) / right-multiply step (L-BFGS),
		    // the initial inverse Hessian approximation is taken to be the Identity.
		    // However, Oren showed that using instead I * \gamma, where \gamma is
		    // chosen to approximate an eigenvalue of the true inverse Hessian can
		    // result in improved convergence in a wide variety of cases. Setting
		    // use_approximate_eigenvalue_bfgs_scaling to true enables this scaling.
		    //
		    // It is important to note that approximate eigenvalue scaling does not
		    // always improve convergence, and that it can in fact significantly degrade
		    // performance for certain classes of problem, which is why it is disabled
		    // by default.  In particular it can degrade performance when the
		    // sensitivity of the problem to different parameters varies significantly,
		    // as in this case a single scalar factor fails to capture this variation
		    // and detrimentally downscales parts of the jacobian approximation which
		    // correspond to low-sensitivity parameters. It can also reduce the
		    // robustness of the solution to errors in the jacobians.
		    //
		    // Oren S.S., Self-scaling variable metric (SSVM) algorithms
		    // Part II: Implementation and experiments, Management Science,
		    // 20(5), 863-874, 1974.
		    public boolean use_approximate_eigenvalue_bfgs_scaling;

		    // Degree of the polynomial used to approximate the objective
		    // function. Valid values are BISECTION, QUADRATIC and CUBIC.
		    //
		    // BISECTION corresponds to pure backtracking search with no
		    // interpolation.
		    public LineSearchInterpolationType line_search_interpolation_type;

		    // If during the line search, the step_size falls below this
		    // value, it is truncated to zero.
		    public double min_line_search_step_size;

		    // Line search parameters.

		    // Solving the line search problem exactly is computationally
		    // prohibitive. Fortunately, line search based optimization
		    // algorithms can still guarantee convergence if instead of an
		    // exact solution, the line search algorithm returns a solution
		    // which decreases the value of the objective function
		    // sufficiently. More precisely, we are looking for a step_size
		    // s.t.
		    //
		    //   f(step_size) <= f(0) + sufficient_decrease * f'(0) * step_size
		    //
		    public double line_search_sufficient_function_decrease;

		    // In each iteration of the line search,
		    //
		    //  new_step_size >= max_line_search_step_contraction * step_size
		    //
		    // Note that by definition, for contraction:
		    //
		    //  0 < max_step_contraction < min_step_contraction < 1
		    //
		    public double max_line_search_step_contraction;

		    // In each iteration of the line search,
		    //
		    //  new_step_size <= min_line_search_step_contraction * step_size
		    //
		    // Note that by definition, for contraction:
		    //
		    //  0 < max_step_contraction < min_step_contraction < 1
		    //
		    public double min_line_search_step_contraction;

		    // Maximum number of trial step size iterations during each line search,
		    // if a step size satisfying the search conditions cannot be found within
		    // this number of trials, the line search will terminate.
		    public int max_num_line_search_step_size_iterations;

		    // Maximum number of restarts of the line search direction algorithm before
		    // terminating the optimization. Restarts of the line search direction
		    // algorithm occur when the current algorithm fails to produce a new descent
		    // direction. This typically indicates a numerical failure, or a breakdown
		    // in the validity of the approximations used.
		    public int max_num_line_search_direction_restarts;

		    // The strong Wolfe conditions consist of the Armijo sufficient
		    // decrease condition, and an additional requirement that the
		    // step-size be chosen s.t. the _magnitude_ ('strong' Wolfe
		    // conditions) of the gradient along the search direction
		    // decreases sufficiently. Precisely, this second condition
		    // is that we seek a step_size s.t.
		    //
		    //   |f'(step_size)| <= sufficient_curvature_decrease * |f'(0)|
		    //
		    // Where f() is the line search objective and f'() is the derivative
		    // of f w.r.t step_size (d f / d step_size).
		    public double line_search_sufficient_curvature_decrease;

		    // During the bracketing phase of the Wolfe search, the step size is
		    // increased until either a point satisfying the Wolfe conditions is
		    // found, or an upper bound for a bracket containing a point satisfying
		    // the conditions is found.  Precisely, at each iteration of the
		    // expansion:
		    //
		    //   new_step_size <= max_step_expansion * step_size.
		    //
		    // By definition for expansion, max_step_expansion > 1.0.
		    public double max_line_search_step_expansion;

		    // Maximum number of iterations for the minimizer to run for.
		    public int max_num_iterations;

		    // Maximum time for which the minimizer should run for.
		    public double max_solver_time_in_seconds;

		    // Minimizer terminates when
		    //
		    //   (new_cost - old_cost) < function_tolerance * old_cost;
		    //
		    public double function_tolerance;

		    // Minimizer terminates when
		    //
		    //   max_i |x - Project(Plus(x, -g(x))| < gradient_tolerance
		    //
		    // This value should typically be 1e-4 * function_tolerance.
		    public double gradient_tolerance;

		    // Minimizer terminates when
		    //
		    //   |step|_2 <= parameter_tolerance * ( |x|_2 +  parameter_tolerance)
		    //
		    public double parameter_tolerance;

		    // Logging options ---------------------------------------------------------

		    public LoggingType logging_type;

		    // By default the Minimizer progress is logged to VLOG(1), which
		    // is sent to STDERR depending on the vlog level. If this flag is
		    // set to true, and logging_type is not SILENT, the logging output
		    // is sent to STDOUT.
		    public boolean minimizer_progress_to_stdout;

		    // If true, the user's parameter blocks are updated at the end of
		    // every Minimizer iteration, otherwise they are updated when the
		    // Minimizer terminates. This is useful if, for example, the user
		    // wishes to visualize the state of the optimization every
		    // iteration.
		    public boolean update_state_every_iteration;

		    // Callbacks that are executed at the end of each iteration of the
		    // Minimizer. An iteration may terminate midway, either due to
		    // numerical failures or because one of the convergence tests has
		    // been satisfied. In this case none of the callbacks are
		    // executed.

		    // Callbacks are executed in the order that they are specified in
		    // this vector. By default, parameter blocks are updated only at
		    // the end of the optimization, i.e when the Minimizer
		    // terminates. This behaviour is controlled by
		    // update_state_every_variable. If the user wishes to have access
		    // to the update parameter blocks when his/her callbacks are
		    // executed, then set update_state_every_iteration to true.
		    //
		    // The solver does NOT take ownership of these pointers.
		    Vector<IterationCallback> callbacks;
	        // Default constructor that sets up a generic sparse problem.
	      public GradientProblemSolverOptions() {
		      line_search_direction_type = LineSearchDirectionType.LBFGS;
		      //line_search_direction_type = LineSearchDirectionType.NONLINEAR_CONJUGATE_GRADIENT;
		      line_search_type = LineSearchType.WOLFE;
		      nonlinear_conjugate_gradient_type = NonlinearConjugateGradientType.FLETCHER_REEVES;
		      //nonlinear_conjugate_gradient_type = NonlinearConjugateGradientType.POLAK_RIBIERE;
		      max_lbfgs_rank = 20;
		      use_approximate_eigenvalue_bfgs_scaling = false;
		      line_search_interpolation_type = LineSearchInterpolationType.CUBIC;
		      min_line_search_step_size = 1e-9;
		      line_search_sufficient_function_decrease = 1e-4;
		      max_line_search_step_contraction = 1e-3;
		      min_line_search_step_contraction = 0.6;
		      max_num_line_search_step_size_iterations = 2000;
		      max_num_line_search_direction_restarts = 5;
		      line_search_sufficient_curvature_decrease = 0.9;
		      max_line_search_step_expansion = 10.0;
		      max_num_iterations = 50000;
		      max_solver_time_in_seconds = 1e9;
		      function_tolerance = 1e-6;
		      gradient_tolerance = 1e-10;
		      parameter_tolerance = 1e-8;
		      logging_type = LoggingType.PER_MINIMIZER_ITERATION;
		      minimizer_progress_to_stdout = false;
		      update_state_every_iteration = false;
		      callbacks = new Vector<IterationCallback>();
	    }

	    // Returns true if the options struct has a valid
	    // configuration. Returns false otherwise, and fills in *error
	    // with a message describing the problem.
	    public boolean IsValid(String[] error) {
	    	SolverOptions solver_options =
	    		      GradientProblemSolverOptionsToSolverOptions();
	    		  return solver_options.IsValid(error);
	
	    }
	    
	    public SolverOptions GradientProblemSolverOptionsToSolverOptions() {

	    	  SolverOptions solver_options = new SolverOptions();
	    	  solver_options.minimizer_type = MinimizerType.LINE_SEARCH;
	    	  solver_options.line_search_direction_type = line_search_direction_type;
	    	  solver_options.line_search_type = line_search_type;
	    	  solver_options.nonlinear_conjugate_gradient_type = nonlinear_conjugate_gradient_type;
	    	  solver_options.max_lbfgs_rank = max_lbfgs_rank;
	    	  solver_options.use_approximate_eigenvalue_bfgs_scaling = use_approximate_eigenvalue_bfgs_scaling;
	    	  solver_options.line_search_interpolation_type = line_search_interpolation_type;
	    	  solver_options.min_line_search_step_size = min_line_search_step_size;
	    	  solver_options.line_search_sufficient_function_decrease = line_search_sufficient_function_decrease;
	    	  solver_options.max_line_search_step_contraction = max_line_search_step_contraction;
	    	  solver_options.min_line_search_step_contraction = min_line_search_step_contraction;
	    	  solver_options.max_num_line_search_step_size_iterations = max_num_line_search_step_size_iterations;
	    	  solver_options.max_num_line_search_direction_restarts = max_num_line_search_direction_restarts;
	    	  solver_options.line_search_sufficient_curvature_decrease = line_search_sufficient_curvature_decrease;
	    	  solver_options.max_line_search_step_expansion = max_line_search_step_expansion;
	    	  solver_options.max_num_iterations = max_num_iterations;
	    	  solver_options.max_solver_time_in_seconds = max_solver_time_in_seconds;
	    	  solver_options.parameter_tolerance = parameter_tolerance;
	    	  solver_options.function_tolerance = function_tolerance;
	    	  solver_options.gradient_tolerance = gradient_tolerance;
	    	  solver_options.logging_type = logging_type;
	    	  solver_options.minimizer_progress_to_stdout = minimizer_progress_to_stdout;
	    	  solver_options.callbacks = callbacks;
	    	  return solver_options;
	    	}
	  } // class GradientProblemSolverOptions
	  
	  public SolverOptions GradientProblemSolverOptionsToSolverOptions(GradientProblemSolverOptions options) {

    	  SolverOptions solver_options = new SolverOptions();
    	  solver_options.minimizer_type = MinimizerType.LINE_SEARCH;
    	  solver_options.line_search_direction_type = options.line_search_direction_type;
    	  solver_options.line_search_type = options.line_search_type;
    	  solver_options.nonlinear_conjugate_gradient_type = options.nonlinear_conjugate_gradient_type;
    	  solver_options.max_lbfgs_rank = options.max_lbfgs_rank;
    	  solver_options.use_approximate_eigenvalue_bfgs_scaling = options.use_approximate_eigenvalue_bfgs_scaling;
    	  solver_options.line_search_interpolation_type = options.line_search_interpolation_type;
    	  solver_options.min_line_search_step_size = options.min_line_search_step_size;
    	  solver_options.line_search_sufficient_function_decrease = options.line_search_sufficient_function_decrease;
    	  solver_options.max_line_search_step_contraction = options.max_line_search_step_contraction;
    	  solver_options.min_line_search_step_contraction = options.min_line_search_step_contraction;
    	  solver_options.max_num_line_search_step_size_iterations = options.max_num_line_search_step_size_iterations;
    	  solver_options.max_num_line_search_direction_restarts = options.max_num_line_search_direction_restarts;
    	  solver_options.line_search_sufficient_curvature_decrease = options.line_search_sufficient_curvature_decrease;
    	  solver_options.max_line_search_step_expansion = options.max_line_search_step_expansion;
    	  solver_options.max_num_iterations = options.max_num_iterations;
    	  solver_options.max_solver_time_in_seconds = options.max_solver_time_in_seconds;
    	  solver_options.parameter_tolerance = options.parameter_tolerance;
    	  solver_options.function_tolerance = options.function_tolerance;
    	  solver_options.gradient_tolerance = options.gradient_tolerance;
    	  solver_options.logging_type = options.logging_type;
    	  solver_options.minimizer_progress_to_stdout = options.minimizer_progress_to_stdout;
    	  solver_options.callbacks = options.callbacks;
    	  return solver_options;
    	}
	  
	  class GradientProblemSolverSummary {
		    // Minimizer summary -------------------------------------------------
		    public TerminationType termination_type;

		    // Reason why the solver terminated.
		    public String message[] = new String[1];

		    // Cost of the problem (value of the objective function) before
		    // the optimization.
		    public double initial_cost;

		    // Cost of the problem (value of the objective function) after the
		    // optimization.
		    public double final_cost;

		    // IterationSummary for each minimizer iteration in order.
		    public Vector<IterationSummary> iterations;

		    // Number of times the cost (and not the gradient) was evaluated.
		    public int num_cost_evaluations;

		    // Number of times the gradient (and the cost) were evaluated.
		    public int num_gradient_evaluations;

		    // Sum total of all time spent inside Ceres when Solve is called.
		    public double total_time_in_seconds;

		    // Time (in seconds) spent evaluating the cost.
		    public double cost_evaluation_time_in_seconds;

		    // Time (in seconds) spent evaluating the gradient.
		    public double gradient_evaluation_time_in_seconds;

		    // Time (in seconds) spent minimizing the interpolating polynomial
		    // to compute the next candidate step size as part of a line search.
		    public double line_search_polynomial_minimization_time_in_seconds;

		    // Number of parameters in the probem.
		    public int num_parameters;

		    // Dimension of the tangent space of the problem.
		    public int num_local_parameters;

		    // Type of line search direction used.
		    public LineSearchDirectionType line_search_direction_type;

		    // Type of the line search algorithm used.
		    public LineSearchType line_search_type;

		    //  When performing line search, the degree of the polynomial used
		    //  to approximate the objective function.
		    public LineSearchInterpolationType line_search_interpolation_type;

		    // If the line search direction is NONLINEAR_CONJUGATE_GRADIENT,
		    // then this indicates the particular variant of non-linear
		    // conjugate gradient used.
		    public NonlinearConjugateGradientType nonlinear_conjugate_gradient_type;

		    // If the type of the line search direction is LBFGS, then this
		    // indicates the rank of the Hessian approximation.
		    public int max_lbfgs_rank;
		    
		    public GradientProblemSolverSummary() {
		        message = new String[1];
		        iterations = new Vector<IterationSummary>();
		    }
		    
		    // A brief one line description of the state of the solver after
		    // termination.
		    public String BriefReport() {
		    	  return String.format("Ceres GradientProblemSolver Report: " +
		    	                      "Iterations: %d, " +
		    	                      "Initial cost: %e, " +
		    	                      "Final cost: %e, " +
		    	                      "Termination: %s",
		    	                      iterations.size(),
		    	                      initial_cost,
		    	                      final_cost,
		    	                      TerminationTypeToString(termination_type));
		    	}


		    // A full multiline description of the state of the solver after
		    // termination.
		    //std::string FullReport() const;

		  } // GradientProblemSolverSummary
	  
	  public boolean IsSolutionUsable(GradientProblemSolverSummary  sum) {
		  return (sum.termination_type == TerminationType.CONVERGENCE ||
		          sum.termination_type == TerminationType.NO_CONVERGENCE ||
		          sum.termination_type == TerminationType.USER_SUCCESS);
		}
	  
	  class GradientProblemSolver {
		  GradientProblemSolverOptions options;
		  GradientProblemSolverSummary summary;
		  
		  public GradientProblemSolver() {
			  options = new GradientProblemSolverOptions();
			  summary = new GradientProblemSolverSummary();
		  }
		  
	  } // class GradientProblemSolver
	  
	  class GradientProblemEvaluator extends Evaluator {
		  private GradientProblem problem_;
		  private ExecutionSummary execution_summary_;  
		  
		  public GradientProblemEvaluator(GradientProblem problem) {
			  super();
			  execution_summary_ = new ExecutionSummary();
			  problem_ = problem;
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
			if (jacobian != null) {
				System.err.println("SparseMatrix jacobian must be null in GradientProblemEvaluator Evaluate");
				return false;
			}
			ScopedExecutionTimer total_timer = new ScopedExecutionTimer("Evaluator::Total", execution_summary_);
			// The reason we use Residual and Jacobian here even when we are
			// only computing the cost and gradient has to do with the fact
			// that the line search minimizer code is used by both the
			// GradientProblemSolver and the main CeresSolver coder where the
			// Evaluator evaluates the Jacobian, and these magic strings need
			// to be consistent across the code base for the time accounting
			// to work.
			ScopedExecutionTimer call_type_timer;
			if (gradient == null) {
				call_type_timer = new ScopedExecutionTimer("Evaluator::Residual", execution_summary_);
			}
			else {
				call_type_timer = new ScopedExecutionTimer("Evaluator::Jacobian", execution_summary_);
			}
			return problem_.Evaluate(state, cost, gradient);
		}
		  
		  public boolean Plus(double[] state,
                  double[] delta,
                  double[] state_plus_delta) {
              return problem_.Plus(state, delta, state_plus_delta);
          }
		  
		  public boolean Plus(Vector<Double> state, Vector<Double> delta, Vector<Double> state_plus_delta) {
			  return problem_.Plus(state, delta, state_plus_delta);
		  }
		  
		  public SparseMatrix CreateJacobian() { return null; }
		  
		  public int NumParameters() {
		      return problem_.NumParameters();
		  }

	      public int NumEffectiveParameters() {
		      return problem_.NumLocalParameters();
		  }

		 public int NumResiduals() { return 1; }

		 public HashMap<String, CallStatistics> Statistics() {
	         return execution_summary_.statistics();
		 }
	  } // class GradientProblemEvaluator

	  public void Solve(GradientProblemSolverOptions options,
              GradientProblem problem,
              double[] parameters_ptr,
              GradientProblemSolverSummary summary) {
		    int i;
			//using internal::CallStatistics;
			//using internal::GradientProblemEvaluator;
			//using internal::GradientProblemSolverStateUpdatingCallback;
			//using internal::LoggingCallback;
			//using internal::Minimizer;
			//using internal::scoped_ptr;
			//using internal::SetSummaryFinalCost;
			//using internal::WallTimeInSeconds;
			
			double start_time = 1.0E-3 * System.currentTimeMillis();
			
			if (summary == null) {
				System.err.println("GradientProblemSolverSummary summary == null in Solve");
			}
			summary.num_parameters                    = problem.NumParameters();
			summary.num_local_parameters              = problem.NumLocalParameters();
			summary.line_search_direction_type        = options.line_search_direction_type;       
			summary.line_search_interpolation_type    = options.line_search_interpolation_type;   
			summary.line_search_type                  = options.line_search_type;
			summary.max_lbfgs_rank                    = options.max_lbfgs_rank;
			summary.nonlinear_conjugate_gradient_type = options.nonlinear_conjugate_gradient_type; 
			
			// Check validity
			if (!options.IsValid(summary.message)) {
			    System.err.println("Terminating: " + summary.message[0]);
			    optionsValid = false;
			    return;
			}
			
			double solution[] = new double[problem.NumParameters()];
			for (i = 0; i < problem.NumParameters(); i++) {
				solution[i] = parameters_ptr[i];
			}
			
			// TODO(sameeragarwal): This is a bit convoluted, we should be able
			// to convert to minimizer options directly, but this will do for
			// now.
			MinimizerOptions minimizer_options = new MinimizerOptions(GradientProblemSolverOptionsToSolverOptions(options));
			minimizer_options.evaluator = new GradientProblemEvaluator(problem);
			
			IterationCallback logging_callback;
			if (options.logging_type != LoggingType.SILENT) {
			logging_callback = 
			new LoggingCallback(MinimizerType.LINE_SEARCH, options.minimizer_progress_to_stdout);
			minimizer_options.callbacks.insertElementAt(logging_callback,0);
			}
			
			IterationCallback state_updating_callback;
			if (options.update_state_every_iteration) {
			state_updating_callback =
			new GradientProblemSolverStateUpdatingCallback(
			problem.NumParameters(), solution, parameters_ptr);
			minimizer_options.callbacks.insertElementAt(state_updating_callback, 0);
			}
			
			Minimizer minimizer = Create(MinimizerType.LINE_SEARCH);
			
			SolverSummary solver_summary = new SolverSummary();
			solver_summary.fixed_cost = 0.0;
			solver_summary.preprocessor_time_in_seconds = 0.0;
			solver_summary.postprocessor_time_in_seconds = 0.0;
			solver_summary.line_search_polynomial_minimization_time_in_seconds = 0.0;
			
			((LineSearchMinimizer)minimizer).Minimize(minimizer_options, solution, solver_summary);
			
			summary.termination_type = solver_summary.termination_type;
			summary.message          = solver_summary.message;
			summary.initial_cost     = solver_summary.initial_cost;
			summary.final_cost       = solver_summary.final_cost;
			summary.iterations       = solver_summary.iterations;
			summary.line_search_polynomial_minimization_time_in_seconds =
			solver_summary.line_search_polynomial_minimization_time_in_seconds;
			
			if (IsSolutionUsable(summary)) {
				for (i = 0; i < solution.length; i++) {
					parameters_ptr[i] = solution[i];
				}
			    SetSummaryFinalCost(summary);
			}
			
			HashMap<String, CallStatistics> evaluator_statistics =
			minimizer_options.evaluator.Statistics();
			{
			CallStatistics call_stats = FindWithDefault(
			evaluator_statistics, "Evaluator::Residual", new CallStatistics());
			summary.cost_evaluation_time_in_seconds = call_stats.time;
			summary.num_cost_evaluations = call_stats.calls;
			}
			
			{
			CallStatistics call_stats = FindWithDefault(
			evaluator_statistics, "Evaluator::Jacobian", new CallStatistics());
			summary.gradient_evaluation_time_in_seconds = call_stats.time;
			summary.num_gradient_evaluations = call_stats.calls;
			}
			
			summary.total_time_in_seconds = 1.0E-3*System.currentTimeMillis() - start_time;
		}
  

	
	class GradientProblem {
	    private FirstOrderFunction function_;
	    private LocalParameterization parameterization_;
	    private double[] scratch_;
	    
	    public GradientProblem(FirstOrderFunction function) {
	        function_ = function;
	      parameterization_ = new IdentityParameterization(function_.NumParameters());
	      scratch_ = new double[function_.NumParameters()];
	    }
	    
	    public void finalize() {
	    	function_.finalize();
	    }
	    
	    public GradientProblem(FirstOrderFunction function,
                LocalParameterization parameterization) {
	    	function_ = function;
	        parameterization_ = parameterization;
            scratch_ = new double[function_.NumParameters()];
            if (function_.NumParameters() != parameterization.GlobalSize()) {
            	System.err.println("function_.NumParameters() != parameterization.GlobalSize() in public GradientProblem");
            }
        }

	    public int NumParameters() {
	        return function_.NumParameters();
	    }

	    public int NumLocalParameters() {
	        return parameterization_.LocalSize();
	    }


	    public boolean Evaluate(double[] parameters, double[] cost, double[] gradient) {
	    	  if (gradient == null) {
	    	    return function_.Evaluate(parameters, cost, null);
	    	  }

	    	  return (function_.Evaluate(parameters, cost, scratch_) &&
	    	          parameterization_.MultiplyByJacobian(parameters,
	    	                                                1,
	    	                                                scratch_,
	    	                                                gradient));
	    	}

	    public boolean Plus(double[] x, double[] delta, double[] x_plus_delta) {
	        return parameterization_.Plus(x, delta, x_plus_delta);
	    }
	    
	    public boolean Plus(Vector<Double> x, Vector<Double> delta, Vector<Double> x_plus_delta) {
	        return parameterization_.Plus(x, 0,delta, 0, x_plus_delta, 0);
	    }

	}
	
	class IdentityParameterization extends LocalParameterization {
		private int size_;
		
		public IdentityParameterization(int size) {
			super();
	        size_ = size;
	        if (size <= 0) {
	        	System.err.println("size must be > 0 in public IdentityParameterization(int size)");
	        }
	    }
		
		public boolean Plus(Vector<Double> x, int x_index,Vector<Double> delta, int delta_index,
				  Vector<Double> x_plus_delta, int x_plus_delta_index) {
			int i;
		      for (i = 0; i < size_; i++) {
		    	  x_plus_delta.set(x_plus_delta_index + i, x.get(x_index + i) + delta.get(delta_index + i));
		      }
		      return true;
		    }
		
		public boolean Plus(double[] x, double[] delta, double[] x_plus_delta) {
			int i;
			for (i = 0; i < size_; i++) {
				x_plus_delta[i] = x[i] + delta[i];
			}
            return true;
        }
		
		public boolean ComputeJacobian(double[] x, double[] jacobian) {
			int i;
			int length = size_* size_;
			for (i = 0; i < length; i++) {
				jacobian[i] = 0;
			}
			for (i = 0; i < length; i+= (size_+1)) {
				jacobian[i] = 1.0;
			}
            return true;
        }
		
		public boolean ComputeJacobian(double[] x, int x_start, double[][] jacobian) {
			int r,c;
			for (r = 0; r < size_; r++) {
				for (c = 0; c < size_; c++) {
					if (r == c) {
						jacobian[r][c] = 1.0;
					}
					else {
						jacobian[r][c] = 0.0;
					}
				}
			}
            return true;
        }


		public boolean MultiplyByJacobian(double[] x, int num_cols, double[] global_matrix,
                double[] local_matrix) {
			    int i;
			    int length = num_cols * GlobalSize();
			    for (i = 0; i < length; i++) {
			    	local_matrix[i] = global_matrix[i];
			    }
                return true;
        }

	    public int GlobalSize() { return size_; }
		public int LocalSize() { return size_; }

	}

	// A FirstOrderFunction object implements the evaluation of a function
	// and its gradient.
	abstract class FirstOrderFunction {
	  public FirstOrderFunction() {
		  
	  }
	  public void finalize() {
		  
	  }
	  // cost is never NULL. gradient may be null.
	  public abstract boolean Evaluate(double[] parameters, double[] cost, double[] gradient);
	  public abstract int NumParameters();
	};

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
		} else if (a > b) {
			return 1;
		} else {
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
