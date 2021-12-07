package gov.nih.mipav.model.algorithms;

import java.util.Vector;

import gov.nih.mipav.model.algorithms.CeresSolver.CostFunction;
import gov.nih.mipav.model.algorithms.CeresSolver.LevenbergMarquardtStrategy;
import gov.nih.mipav.model.algorithms.CeresSolver.MinimizerType;
import gov.nih.mipav.model.algorithms.CeresSolver.PreconditionerType;
import gov.nih.mipav.model.algorithms.CeresSolver.ProblemImpl;
import gov.nih.mipav.model.algorithms.CeresSolver.SizedCostFunction;
import gov.nih.mipav.model.algorithms.CeresSolver.SolverOptions;
import gov.nih.mipav.model.algorithms.CeresSolver.SolverSummary;
import gov.nih.mipav.model.algorithms.CeresSolver.TrustRegionStrategyOptions;
import gov.nih.mipav.model.algorithms.CeresSolver.TrustRegionStrategyType;
import gov.nih.mipav.model.algorithms.CeresSolverTest.CurveFittingCostFunction;

public class CeresSolverNISTTest extends CeresSolver {
	// Total score 44
	// LOW_DIFFICULTY 16
	// Misrala 2
	// Chiwrut1 2
	// Chiwrut2 2
	// Lanczos3 2
	// Gauss1 2
	// Gauss2 2
	// Danwood 2
	// Misralb 2
	
	// MEDIUM_DIFFICULTY 20
	// Kirby2 2 
	// Hahn1 2
	// Nelson 1
	// MGH17 Osborne1 1
	// Lanczos1 2
	// Lanczos2 2
	// Gauss3 2 
	// Misralc 2
	// Misrald 2
	// Roszman1 2
	// ENSO 2
	
	// HIGHER_DIFFICULTY 8
	// MGH09 KOWALIK_AND_OSBORNE 0
	// Thurber 2
	// BoxBOD 1
	// Rat42 2
	// MGH10 MEYER 1
	// Eckerle4  1
	// Rat43 1
	// Bennet5 0
	CeresSolver2 ce2 = new CeresSolver2();
	
	public CeresSolverNISTTest() {
		super();
	}
	
	public boolean fitToExternalFunction(double x[], double residuals[], double jacobian[][]) {
		return true;
	}
	
	protected final int MisralaObservations = 14;
	protected double MisralaData[] = new double[]{
			// y             x
			10.07E0,      77.6E0,
		      14.73E0,     114.9E0,
		      17.94E0,     141.1E0,
		      23.93E0,     190.8E0,
		      29.61E0,     239.9E0,
		      35.18E0,     289.0E0,
		      40.02E0,     332.8E0,
		      44.82E0,     378.4E0,
		      50.76E0,     434.8E0,
		      55.05E0,     477.3E0,
		      61.01E0,     536.8E0,
		      66.40E0,     593.1E0,
		      75.47E0,     689.1E0,
		      81.78E0,     760.0E0
	};
	
	class MisralaCostFunction extends SizedCostFunction {
		public MisralaCostFunction() {
			// number of residuals
			// size of first parameter
			super(14, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < MisralaObservations; i++) {
				double exp = Math.exp(-x[1]*MisralaData[2*i+1]);
			    residuals[i] = MisralaData[2*i] - x[0]*(1.0 - exp);
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][2*i] = exp - 1.0;
					jacobians[0][2*i+1] = -x[0]*MisralaData[2*i+1]*exp;
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < MisralaObservations; i++) {
				double exp = Math.exp(-x[1]*MisralaData[2*i+1]);
			    residuals[i] = MisralaData[2*i] - x[0]*(1.0 - exp);
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+2*i] = exp - 1.0;
					jacobians[0][jacobians_offset[0]+2*i+1] = -x[0]*MisralaData[2*i+1]*exp;
			    }
			}

			return true;
		
	  }
	} // class MisralaCostFunction
	
	public void runMisralaCostFunctionExample() {
		// Correct answer for close and distant starting points
		// Ceres Solver Report: Iterations: 4, Initial cost: 2.238564e+01, Final cost: 6.227569e-02, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 238.9421340430634 b2 = 5.501564187892499E-4
		// Actual answer b1 = 2.3894212918E+02 b2 = 5.5015643181E-04

		// Ceres Solver Report: Iterations: 28, Initial cost: 5.390095e+03, Final cost: 6.227576e-02, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 238.95154147610972 b2 = 5.50131157494199E-4
		// Actual answer b1 = 2.3894212918E+02 b2 = 5.5015643181E-04
		
		int i;
		double x[] = new double[2];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 250.0;
        	x[1] = 5.0E-4;
        }
        else {
        	x[0] = 500.0;
        	x[1] = 1.0E-4;
        }
		CostFunction cost_function = new MisralaCostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;
		solverOptions.initial_trust_region_radius = 1e12;
		solverOptions.use_nonmonotonic_steps = true;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1]);
		}
		System.out.println("Actual answer b1 = 2.3894212918E+02 b2 = 5.5015643181E-04");
		} // for (i = 0; i < 2; i++)
	}
	
	protected final int MisralbObservations = 14;
	protected double MisralbData[] = new double[]{
			10.07E0,      77.6E0,
		      14.73E0,     114.9E0,
		      17.94E0,     141.1E0,
		      23.93E0,     190.8E0,
		      29.61E0,     239.9E0,
		      35.18E0,     289.0E0,
		      40.02E0,     332.8E0,
		      44.82E0,     378.4E0,
		      50.76E0,     434.8E0,
		      55.05E0,     477.3E0,
		      61.01E0,     536.8E0,
		      66.40E0,     593.1E0,
		      75.47E0,     689.1E0,
		      81.78E0,     760.0E0
	};
	
	class MisralbCostFunction extends SizedCostFunction {
		public MisralbCostFunction() {
			// number of residuals
			// size of first parameter
			super(14, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < MisralbObservations; i++) {
				double var = 1.0 + x[1]*MisralbData[2*i+1]/2.0;
			    residuals[i] = MisralbData[2*i] - x[0]*(1.0 - 1.0/(var*var));
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][2*i] = (1.0/(var*var)) - 1.0;
					jacobians[0][2*i+1] = -x[0]*MisralbData[2*i+1]/(var*var*var);
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < MisralbObservations; i++) {
				double var = 1.0 + x[1]*MisralbData[2*i+1]/2.0;
			    residuals[i] = MisralbData[2*i] - x[0]*(1.0 - 1.0/(var*var));
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+2*i] = (1.0/(var*var)) - 1.0;
					jacobians[0][jacobians_offset[0]+2*i+1] = -x[0]*MisralbData[2*i+1]/(var*var*var);
			    }
			}
			
			
			return true;
		
	  }
	} // class MisralbCostFunction
	
	public void runMisralbCostFunctionExample() {
		// Correct answer for close and distant starting points
		// Ceres Solver Report: Iterations: 16, Initial cost: 4.327346e+03, Final cost: 3.773236e-02, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 338.00540645112267 b2 = 3.9038020203505846E-4
		// Actual answer b1 = 3.3799746163E+02  b2 = 3.9039091287E-04
		// Ceres Solver Report: Iterations: 29, Initial cost: 5.497159e+03, Final cost: 3.773240e-02, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 338.01086311846063 b2 = 3.9037288915973303E-4
		// Actual answer b1 = 3.3799746163E+02  b2 = 3.9039091287E-04
		int i;
		double x[] = new double[2];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 300.0;
        	x[1] = 2.0E-4;
        }
        else {
        	x[0] = 500.0;
        	x[1] = 1.0E-4;
        }
		CostFunction cost_function = new MisralbCostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1]);
		}
		System.out.println("Actual answer b1 = 3.3799746163E+02  b2 = 3.9039091287E-04");
		} // for (i = 0; i < 2; i++)	
	}
	
	protected final int Chiwrut1Observations = 214;
	protected double Chiwrut1Data[] = new double[]{
			92.9000E0,     0.5000E0,
		     78.7000E0,     0.6250E0,
		     64.2000E0,     0.7500E0,
		     64.9000E0,     0.8750E0,
		     57.1000E0,     1.0000E0,
		     43.3000E0,     1.2500E0,
		     31.1000E0,     1.7500E0,
		     23.6000E0,     2.2500E0,
		     31.0500E0,     1.7500E0,
		     23.7750E0,     2.2500E0,
		     17.7375E0,     2.7500E0,
		     13.8000E0,     3.2500E0,
		     11.5875E0,     3.7500E0,
		      9.4125E0,     4.2500E0,
		      7.7250E0,     4.7500E0,
		      7.3500E0,     5.2500E0,
		      8.0250E0,     5.7500E0,
		     90.6000E0,     0.5000E0,
		     76.9000E0,     0.6250E0,
		     71.6000E0,     0.7500E0,
		     63.6000E0,     0.8750E0,
		     54.0000E0,     1.0000E0,
		     39.2000E0,     1.2500E0,
		     29.3000E0,     1.7500E0,
		     21.4000E0,     2.2500E0,
		     29.1750E0,     1.7500E0,
		     22.1250E0,     2.2500E0,
		     17.5125E0,     2.7500E0,
		     14.2500E0,     3.2500E0,
		      9.4500E0,     3.7500E0,
		      9.1500E0,     4.2500E0,
		      7.9125E0,     4.7500E0,
		      8.4750E0,     5.2500E0,
		      6.1125E0,     5.7500E0,
		     80.0000E0,     0.5000E0,
		     79.0000E0,     0.6250E0,
		     63.8000E0,     0.7500E0,
		     57.2000E0,     0.8750E0,
		     53.2000E0,     1.0000E0,
		     42.5000E0,     1.2500E0,
		     26.8000E0,     1.7500E0,
		     20.4000E0,     2.2500E0,
		     26.8500E0,     1.7500E0,
		     21.0000E0,     2.2500E0,
		     16.4625E0,     2.7500E0,
		     12.5250E0,     3.2500E0,
		     10.5375E0,     3.7500E0,
		      8.5875E0,     4.2500E0,
		      7.1250E0,     4.7500E0,
		      6.1125E0,     5.2500E0,
		      5.9625E0,     5.7500E0,
		     74.1000E0,     0.5000E0,
		     67.3000E0,     0.6250E0,
		     60.8000E0,     0.7500E0,
		     55.5000E0,     0.8750E0,
		     50.3000E0,     1.0000E0,
		     41.0000E0,     1.2500E0,
		     29.4000E0,     1.7500E0,
		     20.4000E0,     2.2500E0,
		     29.3625E0,     1.7500E0,
		     21.1500E0,     2.2500E0,
		     16.7625E0,     2.7500E0,
		     13.2000E0,     3.2500E0,
		     10.8750E0,     3.7500E0,
		      8.1750E0,     4.2500E0,
		      7.3500E0,     4.7500E0,
		      5.9625E0,     5.2500E0,
		      5.6250E0,     5.7500E0,
		     81.5000E0,      .5000E0,
		     62.4000E0,      .7500E0,
		     32.5000E0,     1.5000E0,
		     12.4100E0,     3.0000E0,
		     13.1200E0,     3.0000E0,
		     15.5600E0,     3.0000E0,
		      5.6300E0,     6.0000E0,
		     78.0000E0,      .5000E0,
		     59.9000E0,      .7500E0,
		     33.2000E0,     1.5000E0,
		     13.8400E0,     3.0000E0,
		     12.7500E0,     3.0000E0,
		     14.6200E0,     3.0000E0,
		      3.9400E0,     6.0000E0,
		     76.8000E0,      .5000E0,
		     61.0000E0,      .7500E0,
		     32.9000E0,     1.5000E0,
		     13.8700E0,     3.0000E0,
		     11.8100E0,     3.0000E0,
		     13.3100E0,     3.0000E0,
		      5.4400E0,     6.0000E0,
		     78.0000E0,      .5000E0,
		     63.5000E0,      .7500E0,
		     33.8000E0,     1.5000E0,
		     12.5600E0,     3.0000E0,
		      5.6300E0,     6.0000E0,
		     12.7500E0,     3.0000E0,
		     13.1200E0,     3.0000E0,
		      5.4400E0,     6.0000E0,
		     76.8000E0,      .5000E0,
		     60.0000E0,      .7500E0,
		     47.8000E0,     1.0000E0,
		     32.0000E0,     1.5000E0,
		     22.2000E0,     2.0000E0,
		     22.5700E0,     2.0000E0,
		     18.8200E0,     2.5000E0,
		     13.9500E0,     3.0000E0,
		     11.2500E0,     4.0000E0,
		      9.0000E0,     5.0000E0,
		      6.6700E0,     6.0000E0,
		     75.8000E0,      .5000E0,
		     62.0000E0,      .7500E0,
		     48.8000E0,     1.0000E0,
		     35.2000E0,     1.5000E0,
		     20.0000E0,     2.0000E0,
		     20.3200E0,     2.0000E0,
		     19.3100E0,     2.5000E0,
		     12.7500E0,     3.0000E0,
		     10.4200E0,     4.0000E0,
		      7.3100E0,     5.0000E0,
		      7.4200E0,     6.0000E0,
		     70.5000E0,      .5000E0,
		     59.5000E0,      .7500E0,
		     48.5000E0,     1.0000E0,
		     35.8000E0,     1.5000E0,
		     21.0000E0,     2.0000E0,
		     21.6700E0,     2.0000E0,
		     21.0000E0,     2.5000E0,
		     15.6400E0,     3.0000E0,
		      8.1700E0,     4.0000E0,
		      8.5500E0,     5.0000E0,
		     10.1200E0,     6.0000E0,
		     78.0000E0,      .5000E0,
		     66.0000E0,      .6250E0,
		     62.0000E0,      .7500E0,
		     58.0000E0,      .8750E0,
		     47.7000E0,     1.0000E0,
		     37.8000E0,     1.2500E0,
		     20.2000E0,     2.2500E0,
		     21.0700E0,     2.2500E0,
		     13.8700E0,     2.7500E0,
		      9.6700E0,     3.2500E0,
		      7.7600E0,     3.7500E0,
		      5.4400E0,     4.2500E0,
		      4.8700E0,     4.7500E0,
		      4.0100E0,     5.2500E0,
		      3.7500E0,     5.7500E0,
		     24.1900E0,     3.0000E0,
		     25.7600E0,     3.0000E0,
		     18.0700E0,     3.0000E0,
		     11.8100E0,     3.0000E0,
		     12.0700E0,     3.0000E0,
		     16.1200E0,     3.0000E0,
		     70.8000E0,      .5000E0,
		     54.7000E0,      .7500E0,
		     48.0000E0,     1.0000E0,
		     39.8000E0,     1.5000E0,
		     29.8000E0,     2.0000E0,
		     23.7000E0,     2.5000E0,
		     29.6200E0,     2.0000E0,
		     23.8100E0,     2.5000E0,
		     17.7000E0,     3.0000E0,
		     11.5500E0,     4.0000E0,
		     12.0700E0,     5.0000E0,
		      8.7400E0,     6.0000E0,
		     80.7000E0,      .5000E0,
		     61.3000E0,      .7500E0,
		     47.5000E0,     1.0000E0,
		     29.0000E0,     1.5000E0,
		     24.0000E0,     2.0000E0,
		     17.7000E0,     2.5000E0,
		     24.5600E0,     2.0000E0,
		     18.6700E0,     2.5000E0,
		     16.2400E0,     3.0000E0,
		      8.7400E0,     4.0000E0,
		      7.8700E0,     5.0000E0,
		      8.5100E0,     6.0000E0,
		     66.7000E0,      .5000E0,
		     59.2000E0,      .7500E0,
		     40.8000E0,     1.0000E0,
		     30.7000E0,     1.5000E0,
		     25.7000E0,     2.0000E0,
		     16.3000E0,     2.5000E0,
		     25.9900E0,     2.0000E0,
		     16.9500E0,     2.5000E0,
		     13.3500E0,     3.0000E0,
		      8.6200E0,     4.0000E0,
		      7.2000E0,     5.0000E0,
		      6.6400E0,     6.0000E0,
		     13.6900E0,     3.0000E0,
		     81.0000E0,      .5000E0,
		     64.5000E0,      .7500E0,
		     35.5000E0,     1.5000E0,
		     13.3100E0,     3.0000E0,
		      4.8700E0,     6.0000E0,
		     12.9400E0,     3.0000E0,
		      5.0600E0,     6.0000E0,
		     15.1900E0,     3.0000E0,
		     14.6200E0,     3.0000E0,
		     15.6400E0,     3.0000E0,
		     25.5000E0,     1.7500E0,
		     25.9500E0,     1.7500E0,
		     81.7000E0,      .5000E0,
		     61.6000E0,      .7500E0,
		     29.8000E0,     1.7500E0,
		     29.8100E0,     1.7500E0,
		     17.1700E0,     2.7500E0,
		     10.3900E0,     3.7500E0,
		     28.4000E0,     1.7500E0,
		     28.6900E0,     1.7500E0,
		     81.3000E0,      .5000E0,
		     60.9000E0,      .7500E0,
		     16.6500E0,     2.7500E0,
		     10.0500E0,     3.7500E0,
		     28.9000E0,     1.7500E0,
		     28.9500E0,     1.7500E0
	};
	
	class Chiwrut1CostFunction extends SizedCostFunction {
		public Chiwrut1CostFunction() {
			// number of residuals
			// size of first parameter
			super(214, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Chiwrut1Observations; i++) {
				double exp = Math.exp(-x[0]*Chiwrut1Data[2*i+1]);
				double denom = x[1] + x[2]*Chiwrut1Data[2*i+1];
			    residuals[i] = Chiwrut1Data[2*i] - exp/denom;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][3*i] = Chiwrut1Data[2*i+1]*exp/denom;
					jacobians[0][3*i+1] = exp/(denom*denom);
					jacobians[0][3*i+2] = Chiwrut1Data[2*i+1]*exp/(denom*denom);
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Chiwrut1Observations; i++) {
				double exp = Math.exp(-x[0]*Chiwrut1Data[2*i+1]);
				double denom = x[1] + x[2]*Chiwrut1Data[2*i+1];
			    residuals[i] = Chiwrut1Data[2*i] - exp/denom;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+3*i] = Chiwrut1Data[2*i+1]*exp/denom;
					jacobians[0][jacobians_offset[0]+3*i+1] = exp/(denom*denom);
					jacobians[0][jacobians_offset[0]+3*i+2] = Chiwrut1Data[2*i+1]*exp/(denom*denom);
			    }
			}
			
			return true;
		
	  }
	} // class Chiwrut1CostFunction
	
	public void runChiwrut1CostFunctionExample() {
		// Correct answer for close and distant starting points
		// Ceres Solver Report: Iterations: 20, Initial cost: 2.287854e+03, Final cost: 1.192239e+03, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 0.19032451994884117 b2 = 0.006132380209531057 b3 = 0.010528819824805255
		// Actual answer b1 = 1.9027818370E-01  b2 = 6.1314004477E-03  b3 = 1.0530908399E-02
		// Ceres Solver Report: Iterations: 23, Initial cost: 2.503432e+04, Final cost: 1.192239e+03, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 0.1901305310148374 b2 = 0.0061308729350610615 b3 = 0.0105347776628655
		// Actual answer b1 = 1.9027818370E-01  b2 = 6.1314004477E-03  b3 = 1.0530908399E-02
		int i;
		double x[] = new double[3];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 0.15;
        	x[1] = 0.008;
        	x[2] = 0.010;
        }
        else {
        	x[0] = 0.1;
        	x[1] = 0.01;
        	x[2] = 0.02;
        }
		CostFunction cost_function = new Chiwrut1CostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		System.out.println("Actual answer b1 = 1.9027818370E-01  b2 = 6.1314004477E-03  b3 = 1.0530908399E-02");
		} // for (i = 0; i < 2; i++)		
	}
	
	protected final int Chiwrut2Observations = 54;
	protected double Chiwrut2Data[] = new double[]{
			92.9000E0,     0.500E0,
		      57.1000E0,     1.000E0,
		      31.0500E0,     1.750E0,
		      11.5875E0,     3.750E0,
		       8.0250E0,     5.750E0,
		      63.6000E0,     0.875E0,
		      21.4000E0,     2.250E0,
		      14.2500E0,     3.250E0,
		       8.4750E0,     5.250E0,
		      63.8000E0,     0.750E0,
		      26.8000E0,     1.750E0,
		      16.4625E0,     2.750E0,
		       7.1250E0,     4.750E0,
		      67.3000E0,     0.625E0,
		      41.0000E0,     1.250E0,
		      21.1500E0,     2.250E0,
		       8.1750E0,     4.250E0,
		      81.5000E0,      .500E0,
		      13.1200E0,     3.000E0,
		      59.9000E0,      .750E0,
		      14.6200E0,     3.000E0,
		      32.9000E0,     1.500E0,
		       5.4400E0,     6.000E0,
		      12.5600E0,     3.000E0,
		       5.4400E0,     6.000E0,
		      32.0000E0,     1.500E0,
		      13.9500E0,     3.000E0,
		      75.8000E0,      .500E0,
		      20.0000E0,     2.000E0,
		      10.4200E0,     4.000E0,
		      59.5000E0,      .750E0,
		      21.6700E0,     2.000E0,
		       8.5500E0,     5.000E0,
		      62.0000E0,      .750E0,
		      20.2000E0,     2.250E0,
		       7.7600E0,     3.750E0,
		       3.7500E0,     5.750E0,
		      11.8100E0,     3.000E0,
		      54.7000E0,      .750E0,
		      23.7000E0,     2.500E0,
		      11.5500E0,     4.000E0,
		      61.3000E0,      .750E0,
		      17.7000E0,     2.500E0,
		       8.7400E0,     4.000E0,
		      59.2000E0,      .750E0,
		      16.3000E0,     2.500E0,
		       8.6200E0,     4.000E0,
		      81.0000E0,      .500E0,
		       4.8700E0,     6.000E0,
		      14.6200E0,     3.000E0,
		      81.7000E0,      .500E0,
		      17.1700E0,     2.750E0,
		      81.3000E0,      .500E0,
		      28.9000E0,     1.750E0
	};
	
	class Chiwrut2CostFunction extends SizedCostFunction {
		public Chiwrut2CostFunction() {
			// number of residuals
			// size of first parameter
			super(54, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Chiwrut2Observations; i++) {
				double exp = Math.exp(-x[0]*Chiwrut2Data[2*i+1]);
				double denom = x[1] + x[2]*Chiwrut2Data[2*i+1];
			    residuals[i] = Chiwrut2Data[2*i] - exp/denom;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][3*i] = Chiwrut2Data[2*i+1]*exp/denom;
					jacobians[0][3*i+1] = exp/(denom*denom);
					jacobians[0][3*i+2] = Chiwrut2Data[2*i+1]*exp/(denom*denom);
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Chiwrut2Observations; i++) {
				double exp = Math.exp(-x[0]*Chiwrut2Data[2*i+1]);
				double denom = x[1] + x[2]*Chiwrut2Data[2*i+1];
			    residuals[i] = Chiwrut2Data[2*i] - exp/denom;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+3*i] = Chiwrut2Data[2*i+1]*exp/denom;
					jacobians[0][jacobians_offset[0]+3*i+1] = exp/(denom*denom);
					jacobians[0][jacobians_offset[0]+3*i+2] = Chiwrut2Data[2*i+1]*exp/(denom*denom);
			    }
			}
			
			return true;
		
	  }
	} // class Chiwrut2CostFunction
	
	public void runChiwrut2CostFunctionExample() {
		// Correct answer for close and distant starting points
		// Ceres Solver Report: Iterations: 10, Initial cost: 7.434794e+02, Final cost: 2.565240e+02, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 0.16659756126534608 b2 = 0.005165700845100553 b3 = 0.012149125037949078
		// Actual answer b1 = 1.6657666537E-01    b2 = 5.1653291286E-03  b3 = 1.2150007096E-02
		// Ceres Solver Report: Iterations: 27, Initial cost: 7.397395e+03, Final cost: 2.565240e+02, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 0.16655985290711292 b2 = 0.005165344897549896 b3 = 0.012150307909200847
		// Actual answer b1 = 1.6657666537E-01    b2 = 5.1653291286E-03  b3 = 1.2150007096E-02
		int i;
		double x[] = new double[3];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 0.15;
        	x[1] = 0.008;
        	x[2] = 0.010;
        }
        else {
        	x[0] = 0.1;
        	x[1] = 0.01;
        	x[2] = 0.02;
        }
		CostFunction cost_function = new Chiwrut2CostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;
		solverOptions.function_tolerance = 1.0E-8;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		System.out.println("Actual answer b1 = 1.6657666537E-01    b2 = 5.1653291286E-03  b3 = 1.2150007096E-02");
		} // for (i = 0; i < 2; i++)			
	}
	
	protected final int Lanczos3Observations = 24;
	protected double Lanczos3Data[] = new double[]{
			2.5134E+00,  0.00000E+00,
		       2.0443E+00,  5.00000E-02,
		       1.6684E+00,  1.00000E-01,
		       1.3664E+00,  1.50000E-01,
		       1.1232E+00,  2.00000E-01,
		       0.9269E+00,  2.50000E-01,
		       0.7679E+00,  3.00000E-01,
		       0.6389E+00,  3.50000E-01,
		       0.5338E+00,  4.00000E-01,
		       0.4479E+00,  4.50000E-01,
		       0.3776E+00,  5.00000E-01,
		       0.3197E+00,  5.50000E-01,
		       0.2720E+00,  6.00000E-01,
		       0.2325E+00,  6.50000E-01,
		       0.1997E+00,  7.00000E-01,
		       0.1723E+00,  7.50000E-01,
		       0.1493E+00,  8.00000E-01,
		       0.1301E+00,  8.50000E-01,
		       0.1138E+00,  9.00000E-01,
		       0.1000E+00,  9.50000E-01,
		       0.0883E+00,  1.00000E+00,
		       0.0783E+00,  1.05000E+00,
		       0.0698E+00,  1.10000E+00,
		       0.0624E+00,  1.15000E+00
	};
	
	class Lanczos3CostFunction extends SizedCostFunction {
		public Lanczos3CostFunction() {
			// number of residuals
			// size of first parameter
			super(24, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Lanczos3Observations; i++) {
			    residuals[i] = Lanczos3Data[2*i] - x[0]*Math.exp(-x[1]*Lanczos3Data[2*i+1])
			    		- x[2]*Math.exp(-x[3]*Lanczos3Data[2*i+1]) - x[4]*Math.exp(-x[5]*Lanczos3Data[2*i+1]);
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][6*i] = -Math.exp(-x[1]*Lanczos3Data[2*i+1]);
					jacobians[0][6*i+1] = Lanczos3Data[2*i+1]*x[0]*Math.exp(-x[1]*Lanczos3Data[2*i+1]);
					jacobians[0][6*i+2] = -Math.exp(-x[3]*Lanczos3Data[2*i+1]);
					jacobians[0][6*i+3] = Lanczos3Data[2*i+1]* x[2]*Math.exp(-x[3]*Lanczos3Data[2*i+1]);
					jacobians[0][6*i+4] = -Math.exp(-x[5]*Lanczos3Data[2*i+1]);
					jacobians[0][6*i+5] = Lanczos3Data[2*i+1]* x[4]*Math.exp(-x[5]*Lanczos3Data[2*i+1]);
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Lanczos3Observations; i++) {
			    residuals[i] = Lanczos3Data[2*i] - x[0]*Math.exp(-x[1]*Lanczos3Data[2*i+1])
			    		- x[2]*Math.exp(-x[3]*Lanczos3Data[2*i+1]) - x[4]*Math.exp(-x[5]*Lanczos3Data[2*i+1]);
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+6*i] = -Math.exp(-x[1]*Lanczos3Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+1] = Lanczos3Data[2*i+1]*x[0]*Math.exp(-x[1]*Lanczos3Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+2] = -Math.exp(-x[3]*Lanczos3Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+3] = Lanczos3Data[2*i+1]*x[2]*Math.exp(-x[3]*Lanczos3Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+4] = -Math.exp(-x[5]*Lanczos3Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+5] = Lanczos3Data[2*i+1]*x[4]*Math.exp(-x[5]*Lanczos3Data[2*i+1]);
			    }
			}

			
			return true;
		
	  }
	} // class Lanczos3CostFunction
	
	public void runLanczos3CostFunctionExample() {
		// Lanczos3 converges correctly from close and distant starting points
		// Ceres Solver Report: Iterations: 8, Initial cost: 3.939461e+01, Final cost: 8.058597e-09, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 0.08681648191216254 b2 = 0.9549813900048281 b3 = 0.8440079399954465
		// b4 = 2.951595609038577 b5 = 1.5825683578790957 b6 = 4.986356645642659
		// Actual answer b1 =  8.6816414977E-02  b2 = 9.5498101505E-01  b3 = 8.4400777463E-01
		// b4 = 2.9515951832E+00  b5 = 1.5825685901E+00  b6 = 4.9863565084E+00
		// Correct answer has Final cost: 8.058597e-09
	
		// Ceres Solver Report: Iterations: 12, Initial cost: 1.348757e+02, Final cost: 8.058597e-09, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 0.0868165248300966 b2 = 0.9549816314110883 b3 = 0.8440080427915962
		// b4 = 2.9515958785586496 b5 = 1.582568212183527 b6 = 4.986356731175022
		// Actual answer b1 =  8.6816414977E-02  b2 = 9.5498101505E-01  b3 = 8.4400777463E-01
		// b4 = 2.9515951832E+00  b5 = 1.5825685901E+00  b6 = 4.9863565084E+00
		// Correct answer has Final cost: 8.058597e-09
		
		int i;
		double x[] = new double[6];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 0.5;
        	x[1] = 0.7;
        	x[2] = 3.6;
        	x[3] = 4.2;
        	x[4] = 4.0;
        	x[5] = 6.3;
        }
        else {
        	x[0] = 1.2;
        	x[1] = 0.3;
        	x[2] = 5.6;
        	x[3] = 5.5;
        	x[4] = 6.5;
        	x[5] = 7.6;
        }
        
        
		CostFunction cost_function = new Lanczos3CostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;
		solverOptions.function_tolerance = 1.0E-12;
		solverOptions.min_trust_region_radius = 1.0E-50;
		solverOptions.initial_trust_region_radius = 1e12;
		solverOptions.min_relative_decrease = 1.0E-1;
		solverOptions.min_lm_diagonal = 1E-9;
		solverOptions.use_nonmonotonic_steps = true;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		System.out.println("b4 = " + x[3] + " b5 = " + x[4] + " b6 = " + x[5]);
		System.out.println("Actual answer b1 =  8.6816414977E-02  b2 = 9.5498101505E-01  b3 = 8.4400777463E-01");
		System.out.println("b4 = 2.9515951832E+00  b5 = 1.5825685901E+00  b6 = 4.9863565084E+00");
		System.out.println("Correct answer has Final cost: 8.058597e-09");
		} // for (i = 0; i < 2; i++)			
	}
	
	protected final int Gauss1Observations = 250;
	protected double Gauss1Data[] = new double[]{
			97.62227,    1.000000,
		    97.80724,    2.000000,
		    96.62247,    3.000000,
		    92.59022,    4.000000,
		    91.23869,    5.000000,
		    95.32704,    6.000000,
		    90.35040,    7.000000,
		    89.46235,    8.000000,
		    91.72520,    9.000000,
		    89.86916,   10.000000,
		    86.88076,    11.00000,
		    85.94360,   12.00000,
		    87.60686,    13.00000,
		    86.25839,    14.00000,
		    80.74976,    15.00000,
		    83.03551,    16.00000,
		    88.25837,    17.00000,
		    82.01316,    18.00000,
		    82.74098,    19.00000,
		    83.30034,    20.00000,
		    81.27850,    21.00000,
		    81.85506,    22.00000,
		    80.75195,    23.00000,
		    80.09573,    24.00000,
		    81.07633,    25.00000,
		    78.81542,    26.00000,
		    78.38596,    27.00000,
		    79.93386,    28.00000,
		    79.48474,    29.00000,
		    79.95942,    30.00000,
		    76.10691,    31.00000,
		    78.39830,    32.00000,
		    81.43060,    33.00000,
		    82.48867,    34.00000,
		    81.65462,    35.00000,
		    80.84323,    36.00000,
		    88.68663,    37.00000,
		    84.74438,    38.00000,
		    86.83934,    39.00000,
		    85.97739,    40.00000,
		    91.28509,    41.00000,
		    97.22411,    42.00000,
		    93.51733,    43.00000,
		    94.10159,    44.00000,
		   101.91760,    45.00000,
		    98.43134,    46.00000,
		    110.4214,    47.00000,
		    107.6628,    48.00000,
		    111.7288,    49.00000,
		    116.5115,    50.00000,
		    120.7609,    51.00000,
		    123.9553,    52.00000,
		    124.2437,    53.00000,
		    130.7996,    54.00000,
		    133.2960,    55.00000,
		    130.7788,    56.00000,
		    132.0565,    57.00000,
		    138.6584,    58.00000,
		    142.9252,    59.00000,
		    142.7215,    60.00000,
		    144.1249,    61.00000,
		    147.4377,    62.00000,
		    148.2647,    63.00000,
		    152.0519,    64.00000,
		    147.3863,    65.00000,
		    149.2074,    66.00000,
		    148.9537,    67.00000,
		    144.5876,    68.00000,
		    148.1226,    69.00000,
		    148.0144,    70.00000,
		    143.8893,    71.00000,
		    140.9088,    72.00000,
		    143.4434,    73.00000,
		    139.3938,    74.00000,
		    135.9878,    75.00000,
		    136.3927,    76.00000,
		    126.7262,    77.00000,
		    124.4487,    78.00000,
		    122.8647,    79.00000,
		    113.8557,    80.00000,
		    113.7037,    81.00000,
		    106.8407,    82.00000,
		    107.0034,    83.00000,
		   102.46290,    84.00000,
		    96.09296,    85.00000,
		    94.57555,    86.00000,
		    86.98824,    87.00000,
		    84.90154,    88.00000,
		    81.18023,    89.00000,
		    76.40117,    90.00000,
		    67.09200,    91.00000,
		    72.67155,    92.00000,
		    68.10848,    93.00000,
		    67.99088,    94.00000,
		    63.34094,    95.00000,
		    60.55253,    96.00000,
		    56.18687,    97.00000,
		    53.64482,    98.00000,
		    53.70307,    99.00000,
		    48.07893,   100.00000,
		    42.21258,   101.00000,
		    45.65181,   102.00000,
		    41.69728,   103.00000,
		    41.24946,   104.00000,
		    39.21349,   105.00000,
		    37.71696,    106.0000,
		    36.68395,    107.0000,
		    37.30393,    108.0000,
		    37.43277,    109.0000,
		    37.45012,    110.0000,
		    32.64648,    111.0000,
		    31.84347,    112.0000,
		    31.39951,    113.0000,
		    26.68912,    114.0000,
		    32.25323,    115.0000,
		    27.61008,    116.0000,
		    33.58649,    117.0000,
		    28.10714,    118.0000,
		    30.26428,    119.0000,
		    28.01648,    120.0000,
		    29.11021,    121.0000,
		    23.02099,    122.0000,
		    25.65091,    123.0000,
		    28.50295,    124.0000,
		    25.23701,    125.0000,
		    26.13828,    126.0000,
		    33.53260,    127.0000,
		    29.25195,    128.0000,
		    27.09847,    129.0000,
		    26.52999,    130.0000,
		    25.52401,    131.0000,
		    26.69218,    132.0000,
		    24.55269,    133.0000,
		    27.71763,    134.0000,
		    25.20297,    135.0000,
		    25.61483,    136.0000,
		    25.06893,    137.0000,
		    27.63930,    138.0000,
		    24.94851,    139.0000,
		    25.86806,    140.0000,
		    22.48183,    141.0000,
		    26.90045,    142.0000,
		    25.39919,    143.0000,
		    17.90614,    144.0000,
		    23.76039,    145.0000,
		    25.89689,    146.0000,
		    27.64231,    147.0000,
		    22.86101,    148.0000,
		    26.47003,    149.0000,
		    23.72888,    150.0000,
		    27.54334,    151.0000,
		    30.52683,    152.0000,
		    28.07261,    153.0000,
		    34.92815,    154.0000,
		    28.29194,    155.0000,
		    34.19161,    156.0000,
		    35.41207,    157.0000,
		    37.09336,    158.0000,
		    40.98330,    159.0000,
		    39.53923,    160.0000,
		    47.80123,    161.0000,
		    47.46305,    162.0000,
		    51.04166,    163.0000,
		    54.58065,    164.0000,
		    57.53001,    165.0000,
		    61.42089,    166.0000,
		    62.79032,    167.0000,
		    68.51455,    168.0000,
		    70.23053,    169.0000,
		    74.42776,    170.0000,
		    76.59911,    171.0000,
		    81.62053,    172.0000,
		    83.42208,    173.0000,
		    79.17451,    174.0000,
		    88.56985,    175.0000,
		    85.66525,    176.0000,
		    86.55502,    177.0000,
		    90.65907,    178.0000,
		    84.27290,    179.0000,
		    85.72220,    180.0000,
		    83.10702,    181.0000,
		    82.16884,    182.0000,
		    80.42568,    183.0000,
		    78.15692,    184.0000,
		    79.79691,    185.0000,
		    77.84378,    186.0000,
		    74.50327,    187.0000,
		    71.57289,    188.0000,
		    65.88031,    189.0000,
		    65.01385,    190.0000,
		    60.19582,    191.0000,
		    59.66726,    192.0000,
		    52.95478,    193.0000,
		    53.87792,    194.0000,
		    44.91274,    195.0000,
		    41.09909,    196.0000,
		    41.68018,    197.0000,
		    34.53379,   198.0000,
		    34.86419,    199.0000,
		    33.14787,    200.0000,
		    29.58864,    201.0000,
		    27.29462,    202.0000,
		    21.91439,    203.0000,
		    19.08159,    204.0000,
		    24.90290,    205.0000,
		    19.82341,    206.0000,
		    16.75551,    207.0000,
		    18.24558,    208.0000,
		    17.23549,    209.0000,
		    16.34934,    210.0000,
		    13.71285,    211.0000,
		    14.75676,    212.0000,
		    13.97169,    213.0000,
		    12.42867,    214.0000,
		    14.35519,    215.0000,
		    7.703309,    216.0000,
		   10.234410,    217.0000,
		    11.78315,    218.0000,
		    13.87768,    219.0000,
		    4.535700,    220.0000,
		   10.059280,    221.0000,
		    8.424824,    222.0000,
		   10.533120,    223.0000,
		    9.602255,    224.0000,
		    7.877514,    225.0000,
		    6.258121,    226.0000,
		    8.899865,    227.0000,
		    7.877754,    228.0000,
		    12.51191,    229.0000,
		    10.66205,    230.0000,
		    6.035400,    231.0000,
		    6.790655,    232.0000,
		    8.783535,    233.0000,
		    4.600288,    234.0000,
		    8.400915,    235.0000,
		    7.216561,    236.0000,
		   10.017410,    237.0000,
		    7.331278,    238.0000,
		    6.527863,    239.0000,
		    2.842001,    240.0000,
		   10.325070,    241.0000,
		    4.790995,    242.0000,
		    8.377101,    243.0000,
		    6.264445,    244.0000,
		    2.706213,    245.0000,
		    8.362329,    246.0000,
		    8.983658,    247.0000,
		    3.362571,    248.0000,
		    1.182746,    249.0000,
		    4.875359,    250.0000
	};
	
	class Gauss1CostFunction extends SizedCostFunction {
		public Gauss1CostFunction() {
			// number of residuals
			// size of first parameter
			super(250, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Gauss1Observations; i++) {
				double diff1 = Gauss1Data[2*i+1] - x[3];
				double diff2 = Gauss1Data[2*i+1] - x[6];
				double exp1 = Math.exp(-x[1]*Gauss1Data[2*i+1]);
				double exp2 = Math.exp(-diff1*diff1/(x[4]*x[4]));
				double exp3 = Math.exp(-diff2*diff2/(x[7]*x[7]));
			    residuals[i] = Gauss1Data[2*i] - x[0]*exp1
			    		- x[2]*exp2 - x[5]*exp2;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][8*i] = -exp1;
					jacobians[0][8*i+1] = Gauss1Data[2*i+1]*x[0]*exp1;
					jacobians[0][8*i+2] = -exp2;
					jacobians[0][8*i+3] = -2.0*x[2]*(diff1/(x[4]*x[4]))*exp2;
					jacobians[0][8*i+4] = -2.0*x[2]*(diff1*diff1/(x[4]*x[4]*x[4]))*exp2;
					jacobians[0][8*i+5] = -exp3;
					jacobians[0][8*i+6] = -2.0*x[5]*(diff2/(x[7]*x[7]))*exp3;
					jacobians[0][8*i+7] = -2.0*x[5]*(diff2*diff2/(x[7]*x[7]*x[7]))*exp3;
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Gauss1Observations; i++) {
				double diff1 = Gauss1Data[2*i+1] - x[3];
				double diff2 = Gauss1Data[2*i+1] - x[6];
				double exp1 = Math.exp(-x[1]*Gauss1Data[2*i+1]);
				double exp2 = Math.exp(-diff1*diff1/(x[4]*x[4]));
				double exp3 = Math.exp(-diff2*diff2/(x[7]*x[7]));
			    residuals[i] = Gauss1Data[2*i] - x[0]*exp1
			    		- x[2]*exp2 - x[5]*exp3;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+8*i] = -exp1;
					jacobians[0][jacobians_offset[0]+8*i+1] = Gauss1Data[2*i+1]*x[0]*exp1;
					jacobians[0][jacobians_offset[0]+8*i+2] = -exp2;
					jacobians[0][jacobians_offset[0]+8*i+3] = -2.0*x[2]*(diff1/(x[4]*x[4]))*exp2;
					jacobians[0][jacobians_offset[0]+8*i+4] = -2.0*x[2]*(diff1*diff1/(x[4]*x[4]*x[4]))*exp2;
					jacobians[0][jacobians_offset[0]+8*i+5] = -exp3;
					jacobians[0][jacobians_offset[0]+8*i+6] = -2.0*x[5]*(diff2/(x[7]*x[7]))*exp3;
					jacobians[0][jacobians_offset[0]+8*i+7] = -2.0*x[5]*(diff2*diff2/(x[7]*x[7]*x[7]))*exp3;
			    }
			}

			return true;
		
	  }
	} // class Gauss1CostFunction
	
	public void runGauss1CostFunctionExample() {
		// Correct answer for close and distant starting points
		// Ceres Solver Report: Iterations: 5, Initial cost: 6.040846e+03, Final cost: 6.579111e+02, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 98.77772012791249 b2 = 0.010497249785771002 b3 = 100.48987844503425
		// b4 = 67.48101018341671 b5 = 23.129886239814006 b6 = 71.99433726481558
		// b7 = 178.9980701307169 b8 = 18.389454077657504
		// Actual answer b1 =  9.8778210871E+01  b2 = 1.0497276517E-02  b3 = 1.0048990633E+02  
		// b4 = 6.7481111276E+01  b5 = 2.3129773360E+01  b6 = 7.1994503004E+01
		// b7 = 1.7899805021E+02 b8 = 1.8389389025E+01
		// Ceres Solver Report: Iterations: 6, Initial cost: 3.685860e+03, Final cost: 6.579111e+02, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 98.77795297914821 b2 = 0.010497154022677648 b3 = 100.48978476056163
		// b4 = 67.48105973849347 b5 = 23.129656233141585 b6 = 71.99429588656515
		// b7 = 178.99804797631572 b8 = 18.38926682193883
		// Actual answer b1 =  9.8778210871E+01  b2 = 1.0497276517E-02  b3 = 1.0048990633E+02  
		// b4 = 6.7481111276E+01  b5 = 2.3129773360E+01  b6 = 7.1994503004E+01
		// b7 = 1.7899805021E+02 b8 = 1.8389389025E+01
		int i;
		double x[] = new double[8];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 94.0;
        	x[1] = 0.0105;
        	x[2] = 99.0;
        	x[3] = 63.0;
        	x[4] = 25.0;
        	x[5] = 71.0;
        	x[6] = 180.0;
        	x[7] = 20.0;
        }
        else {
        	x[0] = 97.0;
        	x[1] = 0.009;
        	x[2] = 100.0;
        	x[3] = 65.0;
        	x[4] = 20.0;
        	x[5] = 70.0;
        	x[6] = 178.0;
        	x[7] = 16.5;
        }
        
		CostFunction cost_function = new Gauss1CostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;
		solverOptions.function_tolerance = 1.0E-8;
		solverOptions.min_trust_region_radius = 1.0E-50;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		System.out.println("b4 = " + x[3] + " b5 = " + x[4] + " b6 = " + x[5]);
		System.out.println("b7 = " + x[6] + " b8 = " + x[7]);
		System.out.println("Actual answer b1 =  9.8778210871E+01  b2 = 1.0497276517E-02  b3 = 1.0048990633E+02  ");
		System.out.println("b4 = 6.7481111276E+01  b5 = 2.3129773360E+01  b6 = 7.1994503004E+01");
		System.out.println("b7 = 1.7899805021E+02 b8 = 1.8389389025E+01");
		} // for (i = 0; i < 2; i++)			
	}
	
	protected final int Gauss2Observations = 250;
	protected double Gauss2Data[] = new double[]{
			97.58776,    1.000000,
		    97.76344,    2.000000,
		    96.56705,    3.000000,
		    92.52037,    4.000000,
		    91.15097,    5.000000,
		    95.21728,    6.000000,
		    90.21355,    7.000000,
		    89.29235,    8.000000,
		    91.51479,    9.000000,
		    89.60966,   10.000000,
		    86.56187,    11.00000,
		    85.55316,    12.00000,
		    87.13054,    13.00000,
		    85.67940,    14.00000,
		    80.04851,    15.00000,
		    82.18925,    16.00000,
		    87.24081,    17.00000,
		    80.79407,    18.00000,
		    81.28570,    19.00000,
		    81.56940,    20.00000,
		    79.22715,    21.00000,
		    79.43275,    22.00000,
		    77.90195,    23.00000,
		    76.75468,    24.00000,
		    77.17377,    25.00000,
		    74.27348,    26.00000,
		    73.11900,    27.00000,
		    73.84826,    28.00000,
		    72.47870,    29.00000,
		    71.92292,    30.00000,
		    66.92176,    31.00000,
		    67.93835,    32.00000,
		    69.56207,    33.00000,
		    69.07066,    34.00000,
		    66.53983,    35.00000,
		    63.87883,    36.00000,
		    69.71537,    37.00000,
		    63.60588,    38.00000,
		    63.37154,    39.00000,
		    60.01835,    40.00000,
		    62.67481,    41.00000,
		    65.80666,    42.00000,
		    59.14304,    43.00000,
		    56.62951,    44.00000,
		    61.21785,    45.00000,
		    54.38790,    46.00000,
		    62.93443,    47.00000,
		    56.65144,    48.00000,
		    57.13362,    49.00000,
		    58.29689,    50.00000,
		    58.91744,    51.00000,
		    58.50172,    52.00000,
		    55.22885,    53.00000,
		    58.30375,    54.00000,
		    57.43237,    55.00000,
		    51.69407,    56.00000,
		    49.93132,    57.00000,
		    53.70760,    58.00000,
		    55.39712,    59.00000,
		    52.89709,    60.00000,
		    52.31649,    61.00000,
		    53.98720,    62.00000,
		    53.54158,    63.00000,
		    56.45046,    64.00000,
		    51.32276,    65.00000,
		    53.11676,    66.00000,
		    53.28631,    67.00000,
		    49.80555,    68.00000,
		    54.69564,    69.00000,
		    56.41627,    70.00000,
		    54.59362,    71.00000,
		    54.38520,    72.00000,
		    60.15354,    73.00000,
		    59.78773,    74.00000,
		    60.49995,    75.00000,
		    65.43885,    76.00000,
		    60.70001,    77.00000,
		    63.71865,    78.00000,
		    67.77139,    79.00000,
		    64.70934,    80.00000,
		    70.78193,    81.00000,
		    70.38651,    82.00000,
		    77.22359,    83.00000,
		    79.52665,    84.00000,
		    80.13077,    85.00000,
		    85.67823,    86.00000,
		    85.20647,    87.00000,
		    90.24548,    88.00000,
		    93.61953,    89.00000,
		    95.86509,    90.00000,
		    93.46992,    91.00000,
		    105.8137,    92.00000,
		    107.8269,    93.00000,
		    114.0607,    94.00000,
		    115.5019,    95.00000,
		    118.5110,    96.00000,
		    119.6177,    97.00000,
		    122.1940,    98.00000,
		    126.9903,    99.00000,
		    125.7005,   100.00000,
		    123.7447,   101.00000,
		    130.6543,   102.00000,
		    129.7168,   103.00000,
		    131.8240,   104.00000,
		    131.8759,   105.00000,
		    131.9994,    106.0000,
		    132.1221,    107.0000,
		    133.4414,    108.0000,
		    133.8252,    109.0000,
		    133.6695,    110.0000,
		    128.2851,    111.0000,
		    126.5182,    112.0000,
		    124.7550,    113.0000,
		    118.4016,    114.0000,
		    122.0334,    115.0000,
		    115.2059,    116.0000,
		    118.7856,    117.0000,
		    110.7387,    118.0000,
		    110.2003,    119.0000,
		   105.17290,    120.0000,
		   103.44720,    121.0000,
		    94.54280,    122.0000,
		    94.40526,    123.0000,
		    94.57964,    124.0000,
		    88.76605,    125.0000,
		    87.28747,    126.0000,
		    92.50443,    127.0000,
		    86.27997,    128.0000,
		    82.44307,    129.0000,
		    80.47367,    130.0000,
		    78.36608,    131.0000,
		    78.74307,    132.0000,
		    76.12786,    133.0000,
		    79.13108,    134.0000,
		    76.76062,    135.0000,
		    77.60769,    136.0000,
		    77.76633,    137.0000,
		    81.28220,    138.0000,
		    79.74307,    139.0000,
		    81.97964,    140.0000,
		    80.02952,    141.0000,
		    85.95232,    142.0000,
		    85.96838,    143.0000,
		    79.94789,    144.0000,
		    87.17023,    145.0000,
		    90.50992,    146.0000,
		    93.23373,    147.0000,
		    89.14803,    148.0000,
		    93.11492,    149.0000,
		    90.34337,    150.0000,
		    93.69421,    151.0000,
		    95.74256,    152.0000,
		    91.85105,    153.0000,
		    96.74503,    154.0000,
		    87.60996,    155.0000,
		    90.47012,    156.0000,
		    88.11690,    157.0000,
		    85.70673,    158.0000,
		    85.01361,    159.0000,
		    78.53040,    160.0000,
		    81.34148,    161.0000,
		    75.19295,    162.0000,
		    72.66115,    163.0000,
		    69.85504,    164.0000,
		    66.29476,    165.0000,
		    63.58502,    166.0000,
		    58.33847,    167.0000,
		    57.50766,    168.0000,
		    52.80498,    169.0000,
		    50.79319,    170.0000,
		    47.03490,    171.0000,
		    46.47090,    172.0000,
		    43.09016,    173.0000,
		    34.11531,    174.0000,
		    39.28235,    175.0000,
		    32.68386,    176.0000,
		    30.44056,    177.0000,
		    31.98932,    178.0000,
		    23.63330,    179.0000,
		    23.69643,    180.0000,
		    20.26812,    181.0000,
		    19.07074,    182.0000,
		    17.59544,    183.0000,
		    16.08785,    184.0000,
		    18.94267,    185.0000,
		    18.61354,    186.0000,
		    17.25800,    187.0000,
		    16.62285,    188.0000,
		    13.48367,    189.0000,
		    15.37647,    190.0000,
		    13.47208,    191.0000,
		    15.96188,    192.0000,
		    12.32547,    193.0000,
		    16.33880,    194.0000,
		   10.438330,    195.0000,
		    9.628715,    196.0000,
		    13.12268,    197.0000,
		    8.772417,    198.0000,
		    11.76143,    199.0000,
		    12.55020,    200.0000,
		    11.33108,    201.0000,
		    11.20493,    202.0000,
		    7.816916,    203.0000,
		    6.800675,    204.0000,
		    14.26581,    205.0000,
		    10.66285,    206.0000,
		    8.911574,    207.0000,
		    11.56733,    208.0000,
		    11.58207,    209.0000,
		    11.59071,    210.0000,
		    9.730134,    211.0000,
		    11.44237,    212.0000,
		    11.22912,    213.0000,
		   10.172130,    214.0000,
		    12.50905,    215.0000,
		    6.201493,    216.0000,
		    9.019605,    217.0000,
		    10.80607,    218.0000,
		    13.09625,    219.0000,
		    3.914271,    220.0000,
		    9.567886,    221.0000,
		    8.038448,    222.0000,
		   10.231040,    223.0000,
		    9.367410,    224.0000,
		    7.695971,    225.0000,
		    6.118575,    226.0000,
		    8.793207,    227.0000,
		    7.796692,    228.0000,
		    12.45065,    229.0000,
		    10.61601,    230.0000,
		    6.001003,    231.0000,
		    6.765098,    232.0000,
		    8.764653,    233.0000,
		    4.586418,    234.0000,
		    8.390783,    235.0000,
		    7.209202,    236.0000,
		   10.012090,    237.0000,
		    7.327461,    238.0000,
		    6.525136,    239.0000,
		    2.840065,    240.0000,
		   10.323710,    241.0000,
		    4.790035,    242.0000,
		    8.376431,    243.0000,
		    6.263980,    244.0000,
		    2.705892,    245.0000,
		    8.362109,    246.0000,
		    8.983507,    247.0000,
		    3.362469,    248.0000,
		    1.182678,    249.0000,
		    4.875312,    250.0000
	};
	
	class Gauss2CostFunction extends SizedCostFunction {
		public Gauss2CostFunction() {
			// number of residuals
			// size of first parameter
			super(250, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Gauss2Observations; i++) {
				double diff1 = Gauss2Data[2*i+1] - x[3];
				double diff2 = Gauss2Data[2*i+1] - x[6];
				double exp1 = Math.exp(-x[1]*Gauss2Data[2*i+1]);
				double exp2 = Math.exp(-diff1*diff1/(x[4]*x[4]));
				double exp3 = Math.exp(-diff2*diff2/(x[7]*x[7]));
			    residuals[i] = Gauss2Data[2*i] - x[0]*exp1
			    		- x[2]*exp2 - x[5]*exp2;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][8*i] = -exp1;
					jacobians[0][8*i+1] = Gauss2Data[2*i+1]*x[0]*exp1;
					jacobians[0][8*i+2] = -exp2;
					jacobians[0][8*i+3] = -2.0*x[2]*(diff1/(x[4]*x[4]))*exp2;
					jacobians[0][8*i+4] = -2.0*x[2]*(diff1*diff1/(x[4]*x[4]*x[4]))*exp2;
					jacobians[0][8*i+5] = -exp3;
					jacobians[0][8*i+6] = -2.0*x[5]*(diff2/(x[7]*x[7]))*exp3;
					jacobians[0][8*i+7] = -2.0*x[5]*(diff2*diff2/(x[7]*x[7]*x[7]))*exp3;
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Gauss2Observations; i++) {
				double diff1 = Gauss2Data[2*i+1] - x[3];
				double diff2 = Gauss2Data[2*i+1] - x[6];
				double exp1 = Math.exp(-x[1]*Gauss2Data[2*i+1]);
				double exp2 = Math.exp(-diff1*diff1/(x[4]*x[4]));
				double exp3 = Math.exp(-diff2*diff2/(x[7]*x[7]));
			    residuals[i] = Gauss2Data[2*i] - x[0]*exp1
			    		- x[2]*exp2 - x[5]*exp3;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+8*i] = -exp1;
					jacobians[0][jacobians_offset[0]+8*i+1] = Gauss2Data[2*i+1]*x[0]*exp1;
					jacobians[0][jacobians_offset[0]+8*i+2] = -exp2;
					jacobians[0][jacobians_offset[0]+8*i+3] = -2.0*x[2]*(diff1/(x[4]*x[4]))*exp2;
					jacobians[0][jacobians_offset[0]+8*i+4] = -2.0*x[2]*(diff1*diff1/(x[4]*x[4]*x[4]))*exp2;
					jacobians[0][jacobians_offset[0]+8*i+5] = -exp3;
					jacobians[0][jacobians_offset[0]+8*i+6] = -2.0*x[5]*(diff2/(x[7]*x[7]))*exp3;
					jacobians[0][jacobians_offset[0]+8*i+7] = -2.0*x[5]*(diff2*diff2/(x[7]*x[7]*x[7]))*exp3;
			    }
			}

			return true;
		
	  }
	} // class Gauss2CostFunction
	
	public void runGauss2CostFunctionExample() {
		// Correct answer for close and distant starting points
		// Ceres Solver Report: Iterations: 13, Initial cost: 2.341565e+03, Final cost: 6.237641e+02, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 99.0179802851211 b2 = 0.01099483029051228 b3 = 101.88012890182267
		// b4 = 107.03079571368134 b5 = 23.578334810932244 b6 = 72.04542073434712
		// b7 = 153.2698595474825 b8 = 19.5262020097769
		// Actual answer b1 = 9.9018328406E+01  b2 = 1.0994945399E-02  b3 = 1.0188022528E+02
		// b4 = 1.0703095519E+02  b5 = 2.3578584029E+01  b6 = 7.2045589471E+01
		// b7 = 1.5327010194E+02 b8 = 1.9525972636E+01

		// Ceres Solver Report: Iterations: 7, Initial cost: 4.579070e+03, Final cost: 6.237641e+02, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 99.0180194542043 b2 = 0.010994824572809305 b3 = 101.8801378856425
		// b4 = 107.0309064272946 b5 = 23.57838643733977 b6 = 72.0454712696249
		// b7 = 153.2699742422434 b8 = 19.52600955176765
		// Actual answer b1 = 9.9018328406E+01  b2 = 1.0994945399E-02  b3 = 1.0188022528E+02
		// b4 = 1.0703095519E+02  b5 = 2.3578584029E+01  b6 = 7.2045589471E+01
		// b7 = 1.5327010194E+02 b8 = 1.9525972636E+01
	    int i;
		double x[] = new double[8];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 98.0;
        	x[1] = 0.0105;
        	x[2] = 103.0;
        	x[3] = 105.0;
        	x[4] = 20.0;
        	x[5] = 73.0;
        	x[6] = 150.0;
        	x[7] = 20.0;
        }
        else {
        	x[0] = 96.0;
        	x[1] = 0.009;
        	x[2] = 103.0;
        	x[3] = 106.0;
        	x[4] = 18.0;
        	x[5] = 72.0;
        	x[6] = 151.0;
        	x[7] = 18.0;
        }
        
		CostFunction cost_function = new Gauss2CostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;
		solverOptions.function_tolerance = 1.0E-8;
		solverOptions.min_trust_region_radius = 1.0E-50;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		System.out.println("b4 = " + x[3] + " b5 = " + x[4] + " b6 = " + x[5]);
		System.out.println("b7 = " + x[6] + " b8 = " + x[7]);
		System.out.println("Actual answer b1 = 9.9018328406E+01  b2 = 1.0994945399E-02  b3 = 1.0188022528E+02");
		System.out.println("b4 = 1.0703095519E+02  b5 = 2.3578584029E+01  b6 = 7.2045589471E+01");
		System.out.println("b7 = 1.5327010194E+02 b8 = 1.9525972636E+01");
		} // for (i = 0; i < 2; i++)			
	}
	
	protected final int DanwoodObservations = 6;
	protected double DanwoodData[] = new double[]{
			2.138E0,        1.309E0,
		      3.421E0,        1.471E0,
		      3.597E0,        1.490E0,
		      4.340E0,        1.565E0,
		      4.882E0,        1.611E0,
		      5.660E0,        1.680E0
	};
	
	class DanwoodCostFunction extends SizedCostFunction {
		public DanwoodCostFunction() {
			// number of residuals
			// size of first parameter
			super(6, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < DanwoodObservations; i++) {
			    residuals[i] = DanwoodData[2*i] - x[0]*Math.pow(DanwoodData[2*i+1], x[1]);
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][2*i] = -Math.pow(DanwoodData[2*i+1], x[1]);
					jacobians[0][2*i+1] = -x[0]*Math.log(DanwoodData[2*i+1])*Math.pow(DanwoodData[2*i+1],x[1]);
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < DanwoodObservations; i++) {
			    residuals[i] = DanwoodData[2*i] - x[0]*Math.pow(DanwoodData[2*i+1], x[1]);
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+2*i] = -Math.pow(DanwoodData[2*i+1], x[1]);
					jacobians[0][jacobians_offset[0]+2*i+1] = -x[0]*Math.log(DanwoodData[2*i+1])*Math.pow(DanwoodData[2*i+1],x[1]);
			    }
			}

			return true;
		
	  }
	} // class DanwoodCostFunction
	
	public void runDanwoodCostFunctionExample() {
		// Correct answer for close and distant starting points
		// Ceres Solver Report: Iterations: 6, Initial cost: 5.188235e-02, Final cost: 2.158656e-03, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 0.7688262500918367 b2 = 3.8605071208652557
		// Actual answer b1 = 7.6886226176E-01 b2 = 3.8604055871E+00
		
		// Ceres Solver Report: Iterations: 7, Initial cost: 7.485961e+01, Final cost: 2.158656e-03, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 0.7688403136201144 b2 = 3.8604770116261284
		// Actual answer b1 = 7.6886226176E-01 b2 = 3.8604055871E+00
		int i;
		double x[] = new double[2];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 0.7;
        	x[1] = 4.0;
        }
        else {
        	x[0] = 1.0;
        	x[1] = 5.0;
        }
		CostFunction cost_function = new DanwoodCostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;
		solverOptions.min_trust_region_radius = 1.0E-50;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1]);
		}
		System.out.println("Actual answer b1 = 7.6886226176E-01 b2 = 3.8604055871E+00");
		} // for (i = 0; i < 2; i++)
	}
	
	protected final int Gauss3Observations = 250;
	protected double Gauss3Data[] = new double[]{
			97.58776,    1.000000,
		    97.76344,    2.000000,
		    96.56705,    3.000000,
		    92.52037,    4.000000,
		    91.15097,    5.000000,
		    95.21728,    6.000000,
		    90.21355,    7.000000,
		    89.29235,    8.0000,
		    91.51479,    9.000000,
		    89.60965,   10.000000,
		    86.56187,    11.00000,
		    85.55315,    12.00000,
		    87.13053,    13.00000,
		    85.67938,    14.00000,
		    80.04849,    15.00000,
		    82.18922,    16.00000,
		    87.24078,    17.00000,
		    80.79401,    18.00000,
		    81.28564,    19.00000,
		    81.56932,    20.00000,
		    79.22703,    21.00000,
		    79.43259,    22.00000,
		    77.90174,    23.00000,
		    76.75438,    24.00000,
		    77.17338,    25.00000,
		    74.27296,    26.00000,
		    73.11830,    27.00000,
		    73.84732,    28.00000,
		    72.47746,    29.00000,
		    71.92128,    30.00000,
		    66.91962,    31.00000,
		    67.93554,    32.00000,
		    69.55841,    33.00000,
		    69.06592,    34.00000,
		    66.53371,    35.00000,
		    63.87094,    36.00000,
		    69.70526,    37.00000,
		    63.59295,    38.00000,
		    63.35509,    39.00000,
		    59.99747,    40.00000,
		    62.64843,    41.00000,
		    65.77345,    42.00000,
		    59.10141,    43.00000,
		    56.57750,    44.00000,
		    61.15313,    45.00000,
		    54.30767,    46.00000,
		    62.83535,    47.00000,
		    56.52957,    48.00000,
		    56.98427,    49.00000,
		    58.11459,    50.00000,
		    58.69576,    51.00000,
		    58.23322,    52.00000,
		    54.90490,    53.00000,
		    57.91442,    54.00000,
		    56.96629,    55.00000,
		    51.13831,    56.00000,
		    49.27123,    57.00000,
		    52.92668,    58.00000,
		    54.47693,    59.00000,
		    51.81710,    60.00000,
		    51.05401,    61.00000,
		    52.51731,    62.00000,
		    51.83710,    63.00000,
		    54.48196,    64.00000,
		    49.05859,    65.00000,
		    50.52315,    66.00000,
		    50.32755,    67.00000,
		    46.44419,    68.00000,
		    50.89281,    69.00000,
		    52.13203,    70.00000,
		    49.78741,    71.00000,
		    49.01637,    72.00000,
		    54.18198,    73.00000,
		    53.17456,    74.00000,
		    53.20827,    75.00000,
		    57.43459,    76.00000,
		    51.95282,    77.00000,
		    54.20282,    78.00000,
		    57.46687,    79.00000,
		    53.60268,    80.00000,
		    58.86728,    81.00000,
		    57.66652,    82.00000,
		    63.71034,    83.00000,
		    65.24244,    84.00000,
		    65.10878,    85.00000,
		    69.96313,    86.00000,
		    68.85475,    87.00000,
		    73.32574,    88.00000,
		    76.21241,    89.00000,
		    78.06311,    90.00000,
		    75.37701,    91.00000,
		    87.54449,    92.00000,
		    89.50588,    93.00000,
		    95.82098,    94.00000,
		    97.48390,    95.00000,
		   100.86070,    96.00000,
		   102.48510,    97.00000,
		    105.7311,    98.00000,
		    111.3489,    99.00000,
		    111.0305,   100.00000,
		    110.1920,   101.00000,
		    118.3581,   102.00000,
		    118.8086,   103.00000,
		    122.4249,   104.00000,
		    124.0953,   105.00000,
		    125.9337,    106.0000,
		    127.8533,    107.0000,
		    131.0361,    108.0000,
		    133.3343,    109.0000,
		    135.1278,    110.0000,
		    131.7113,    111.0000,
		    131.9151,    112.0000,
		    132.1107,    113.0000,
		    127.6898,    114.0000,
		    133.2148,    115.0000,
		    128.2296,    116.0000,
		    133.5902,    117.0000,
		    127.2539,    118.0000,
		    128.3482,    119.0000,
		    124.8694,    120.0000,
		    124.6031,    121.0000,
		    117.0648,    122.0000,
		    118.1966,    123.0000,
		    119.5408,    124.0000,
		    114.7946,    125.0000,
		    114.2780,    126.0000,
		    120.3484,    127.0000,
		    114.8647,    128.0000,
		    111.6514,    129.0000,
		    110.1826,    130.0000,
		    108.4461,    131.0000,
		    109.0571,    132.0000,
		    106.5308,    133.0000,
		    109.4691,    134.0000,
		    106.8709,    135.0000,
		    107.3192,    136.0000,
		    106.9000,    137.0000,
		    109.6526,    138.0000,
		    107.1602,    139.0000,
		    108.2509,    140.0000,
		   104.96310,    141.0000,
		    109.3601,    142.0000,
		    107.6696,    143.0000,
		    99.77286,    144.0000,
		   104.96440,    145.0000,
		    106.1376,    146.0000,
		    106.5816,    147.0000,
		   100.12860,    148.0000,
		   101.66910,    149.0000,
		    96.44254,    150.0000,
		    97.34169,    151.0000,
		    96.97412,    152.0000,
		    90.73460,    153.0000,
		    93.37949,    154.0000,
		    82.12331,    155.0000,
		    83.01657,    156.0000,
		    78.87360,    157.0000,
		    74.86971,    158.0000,
		    72.79341,    159.0000,
		    65.14744,    160.0000,
		    67.02127,    161.0000,
		    60.16136,    162.0000,
		    57.13996,    163.0000,
		    54.05769,    164.0000,
		    50.42265,    165.0000,
		    47.82430,    166.0000,
		    42.85748,    167.0000,
		    42.45495,    168.0000,
		    38.30808,    169.0000,
		    36.95794,    170.0000,
		    33.94543,    171.0000,
		    34.19017,    172.0000,
		    31.66097,    173.0000,
		    23.56172,    174.0000,
		    29.61143,    175.0000,
		    23.88765,    176.0000,
		    22.49812,    177.0000,
		    24.86901,    178.0000,
		    17.29481,    179.0000,
		    18.09291,    180.0000,
		    15.34813,    181.0000,
		    14.77997,    182.0000,
		    13.87832,    183.0000,
		    12.88891,    184.0000,
		    16.20763,    185.0000,
		    16.29024,    186.0000,
		    15.29712,    187.0000,
		    14.97839,    188.0000,
		    12.11330,    189.0000,
		    14.24168,    190.0000,
		    12.53824,    191.0000,
		    15.19818,    192.0000,
		    11.70478,    193.0000,
		    15.83745,    194.0000,
		   10.035850,    195.0000,
		    9.307574,    196.0000,
		    12.86800,    197.0000,
		    8.571671,    198.0000,
		    11.60415,    199.0000,
		    12.42772,    200.0000,
		    11.23627,    201.0000,
		    11.13198,    202.0000,
		    7.761117,    203.0000,
		    6.758250,    204.0000,
		    14.23375,    205.0000,
		    10.63876,    206.0000,
		    8.893581,    207.0000,
		    11.55398,    208.0000,
		    11.57221,    209.0000,
		    11.58347,    210.0000,
		    9.724857,    211.0000,
		    11.43854,    212.0000,
		    11.22636,    213.0000,
		   10.170150,    214.0000,
		    12.50765,    215.0000,
		    6.200494,    216.0000,
		    9.018902,    217.0000,
		    10.80557,    218.0000,
		    13.09591,    219.0000,
		    3.914033,    220.0000,
		    9.567723,    221.0000,
		    8.038338,    222.0000,
		   10.230960,    223.0000,
		    9.367358,    224.0000,
		    7.695937,    225.0000,
		    6.118552,    226.0000,
		    8.793192,    227.0000,
		    7.796682,    228.0000,
		    12.45064,    229.0000,
		    10.61601,    230.0000,
		    6.001000,    231.0000,
		    6.765096,    232.0000,
		    8.764652,    233.0000,
		    4.586417,    234.0000,
		    8.390782,    235.0000,
		    7.209201,    236.0000,
		   10.012090,    237.0000,
		    7.327461,    238.0000,
		    6.525136,    239.0000,
		    2.840065,    240.0000,
		   10.323710,    241.0000,
		    4.790035,    242.0000,
		    8.376431,    243.0000,
		    6.263980,    244.0000,
		    2.705892,    245.0000,
		    8.362109,    246.0000,
		    8.983507,    247.0000,
		    3.362469,    248.0000,
		    1.182678,    249.0000,
		    4.875312,    250.0000
	};
	
	class Gauss3CostFunction extends SizedCostFunction {
		public Gauss3CostFunction() {
			// number of residuals
			// size of first parameter
			super(250, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Gauss3Observations; i++) {
				double diff1 = Gauss3Data[2*i+1] - x[3];
				double diff2 = Gauss3Data[2*i+1] - x[6];
				double exp1 = Math.exp(-x[1]*Gauss3Data[2*i+1]);
				double exp2 = Math.exp(-diff1*diff1/(x[4]*x[4]));
				double exp3 = Math.exp(-diff2*diff2/(x[7]*x[7]));
			    residuals[i] = Gauss3Data[2*i] - x[0]*exp1
			    		- x[2]*exp2 - x[5]*exp2;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][8*i] = -exp1;
					jacobians[0][8*i+1] = Gauss3Data[2*i+1]*x[0]*exp1;
					jacobians[0][8*i+2] = -exp2;
					jacobians[0][8*i+3] = -2.0*x[2]*(diff1/(x[4]*x[4]))*exp2;
					jacobians[0][8*i+4] = -2.0*x[2]*(diff1*diff1/(x[4]*x[4]*x[4]))*exp2;
					jacobians[0][8*i+5] = -exp3;
					jacobians[0][8*i+6] = -2.0*x[5]*(diff2/(x[7]*x[7]))*exp3;
					jacobians[0][8*i+7] = -2.0*x[5]*(diff2*diff2/(x[7]*x[7]*x[7]))*exp3;
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Gauss3Observations; i++) {
				double diff1 = Gauss3Data[2*i+1] - x[3];
				double diff2 = Gauss3Data[2*i+1] - x[6];
				double exp1 = Math.exp(-x[1]*Gauss3Data[2*i+1]);
				double exp2 = Math.exp(-diff1*diff1/(x[4]*x[4]));
				double exp3 = Math.exp(-diff2*diff2/(x[7]*x[7]));
			    residuals[i] = Gauss3Data[2*i] - x[0]*exp1
			    		- x[2]*exp2 - x[5]*exp3;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+8*i] = -exp1;
					jacobians[0][jacobians_offset[0]+8*i+1] = Gauss3Data[2*i+1]*x[0]*exp1;
					jacobians[0][jacobians_offset[0]+8*i+2] = -exp2;
					jacobians[0][jacobians_offset[0]+8*i+3] = -2.0*x[2]*(diff1/(x[4]*x[4]))*exp2;
					jacobians[0][jacobians_offset[0]+8*i+4] = -2.0*x[2]*(diff1*diff1/(x[4]*x[4]*x[4]))*exp2;
					jacobians[0][jacobians_offset[0]+8*i+5] = -exp3;
					jacobians[0][jacobians_offset[0]+8*i+6] = -2.0*x[5]*(diff2/(x[7]*x[7]))*exp3;
					jacobians[0][jacobians_offset[0]+8*i+7] = -2.0*x[5]*(diff2*diff2/(x[7]*x[7]*x[7]))*exp3;
			    }
			}

			return true;
		
	  }
	} // class Gauss3CostFunction
	
	public void runGauss3CostFunctionExample() {
		// Correct answer for close and distant starting points
		// Ceres Solver Report: Iterations: 17, Initial cost: 6.999460e+03, Final cost: 6.222423e+02, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 98.9403549287578 b2 = 0.010945851512644897 b3 = 100.69438333429856
		// b4 = 111.63559912036472 b5 = 23.29994994837962 b6 = 73.7067435765268
		// b7 = 147.76094244503523 b8 = 19.66879082124004
		// Actual answer b1 = 9.8940368970E+01  b2 = 1.0945879335E-02  b3 = 1.0069553078E+02
		// b4 = 1.1163619459E+02  b5 = 2.3300500029E+01  b6 = 7.3705031418E+01
		// b7 = 1.4776164251E+02 b8 = 1.9668221230E+01
		
		// Ceres Solver Report: Iterations: 8, Initial cost: 9.452568e+03, Final cost: 6.222423e+02, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 98.94010078713973 b2 = 0.01094576508647728 b3 = 100.69444107882983
		// b4 = 111.63569295839253 b5 = 23.299936931416177 b6 = 73.70647199677175
		// b7 = 147.76097628581687 b8 = 19.66866197222553
		// Actual answer b1 = 9.8940368970E+01  b2 = 1.0945879335E-02  b3 = 1.0069553078E+02
		// b4 = 1.1163619459E+02  b5 = 2.3300500029E+01  b6 = 7.3705031418E+01
		// b7 = 1.4776164251E+02 b8 = 1.9668221230E+01
	    int i;
		double x[] = new double[8];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 96.0;
        	x[1] = 0.0096;
        	x[2] = 80.0;
        	x[3] = 110.0;
        	x[4] = 25.0;
        	x[5] = 74.0;
        	x[6] = 139.0;
        	x[7] = 25.0;
        }
        else {
        	x[0] = 94.9;
        	x[1] = 0.009;
        	x[2] = 90.1;
        	x[3] = 113.0;
        	x[4] = 20.0;
        	x[5] = 73.8;
        	x[6] = 140.0;
        	x[7] = 20.0;
        }
        
		CostFunction cost_function = new Gauss3CostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;
		solverOptions.function_tolerance = 1.0E-8;
		solverOptions.min_trust_region_radius = 1.0E-50;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		System.out.println("b4 = " + x[3] + " b5 = " + x[4] + " b6 = " + x[5]);
		System.out.println("b7 = " + x[6] + " b8 = " + x[7]);
		System.out.println("Actual answer b1 = 9.8940368970E+01  b2 = 1.0945879335E-02  b3 = 1.0069553078E+02");
		System.out.println("b4 = 1.1163619459E+02  b5 = 2.3300500029E+01  b6 = 7.3705031418E+01");
		System.out.println("b7 = 1.4776164251E+02 b8 = 1.9668221230E+01");
		} // for (i = 0; i < 2; i++)			
	}
	
	protected final int MisralcObservations = 14;
	protected double MisralcData[] = new double[]{
			// y             x
			10.07E0,      77.6E0,
		      14.73E0,     114.9E0,
		      17.94E0,     141.1E0,
		      23.93E0,     190.8E0,
		      29.61E0,     239.9E0,
		      35.18E0,     289.0E0,
		      40.02E0,     332.8E0,
		      44.82E0,     378.4E0,
		      50.76E0,     434.8E0,
		      55.05E0,     477.3E0,
		      61.01E0,     536.8E0,
		      66.40E0,     593.1E0,
		      75.47E0,     689.1E0,
		      81.78E0,     760.0E0
	};
	
	class MisralcCostFunction extends SizedCostFunction {
		public MisralcCostFunction() {
			// number of residuals
			// size of first parameter
			super(14, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < MisralcObservations; i++) {
			    residuals[i] = MisralcData[2*i] - x[0]*(1.0 - 1.0/Math.sqrt(1.0 + 2.0*MisralcData[2*i+1]*x[1]));
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][2*i] = -(1.0 - 1.0/Math.sqrt(1.0 + 2.0*MisralcData[2*i+1]*x[1]));
					jacobians[0][2*i+1] = -x[0]*MisralcData[2*i+1]*Math.pow((1.0 + 2.0*MisralcData[2*i+1]*x[1]),-1.5);
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < MisralcObservations; i++) {
			    residuals[i] = MisralcData[2*i] - x[0]*(1.0 - 1.0/Math.sqrt(1.0 + 2.0*MisralcData[2*i+1]*x[1]));
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+2*i] = -(1.0 - 1.0/Math.sqrt(1.0 + 2.0*MisralcData[2*i+1]*x[1]));
					jacobians[0][jacobians_offset[0]+2*i+1] = -x[0]*MisralcData[2*i+1]*Math.pow((1.0 + 2.0*MisralcData[2*i+1]*x[1]),-1.5);
			    }
			}

			return true;
		
	  }
	} // class MisralcCostFunction
	
	public void runMisralcCostFunctionExample() {
		// Correct answer for close and distant starting points
		// Ceres Solver Report: Iterations: 7, Initial cost: 1.312283e+02, Final cost: 2.048344e-02, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 636.4130842794502 b2 = 2.0814159183237546E-4
		// Actual answer b1 = 6.3642725809E+02 b2 = 2.0813627256E-04
		
		// Ceres Solver Report: Iterations: 17, Initial cost: 5.801508e+03, Final cost: 2.048344e-02, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 636.4429437651382 b2 = 2.0813029162534678E-4
		// Actual answer b1 = 6.3642725809E+02 b2 = 2.0813627256E-04
		
		int i;
		double x[] = new double[2];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 600.0;
        	x[1] = 2.0E-4;
        }
        else {
        	x[0] = 500.0;
        	x[1] = 1.0E-4;
        }
		CostFunction cost_function = new MisralcCostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1]);
		}
		System.out.println("Actual answer b1 = 6.3642725809E+02 b2 = 2.0813627256E-04");
		} // for (i = 0; i < 2; i++)
	}
	
	protected final int MisraldObservations = 14;
	protected double MisraldData[] = new double[]{
			// y             x
			10.07E0,      77.6E0,
		      14.73E0,     114.9E0,
		      17.94E0,     141.1E0,
		      23.93E0,     190.8E0,
		      29.61E0,     239.9E0,
		      35.18E0,     289.0E0,
		      40.02E0,     332.8E0,
		      44.82E0,     378.4E0,
		      50.76E0,     434.8E0,
		      55.05E0,     477.3E0,
		      61.01E0,     536.8E0,
		      66.40E0,     593.1E0,
		      75.47E0,     689.1E0,
		      81.78E0,     760.0E0
	};
	
	class MisraldCostFunction extends SizedCostFunction {
		public MisraldCostFunction() {
			// number of residuals
			// size of first parameter
			super(14, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < MisraldObservations; i++) {
			    residuals[i] = MisraldData[2*i] - x[0]*x[1]*MisraldData[2*i+1]/(1.0 + MisraldData[2*i+1]*x[1]);
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][2*i] = -x[1]*MisraldData[2*i+1]/(1.0 + MisraldData[2*i+1]*x[1]);;
					jacobians[0][2*i+1] = -x[0]*MisraldData[2*i+1]/Math.pow((1.0 + MisraldData[2*i+1]*x[1]),2.0);
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < MisraldObservations; i++) {
			    residuals[i] = MisraldData[2*i] - x[0]*x[1]*MisraldData[2*i+1]/(1.0 + MisraldData[2*i+1]*x[1]);
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+2*i] = -x[1]*MisraldData[2*i+1]/(1.0 + MisraldData[2*i+1]*x[1]);;
					jacobians[0][jacobians_offset[0]+2*i+1] = -x[0]*MisraldData[2*i+1]/Math.pow((1.0 + MisraldData[2*i+1]*x[1]),2.0);
			    }
			}

			return true;
		
	  }
	} // class MisraldCostFunction
	
	public void runMisraldCostFunctionExample() {
		// Correct answer for close and distant starting points
		// Ceres Solver Report: Iterations: 10, Initial cost: 8.195109e+00, Final cost: 2.820968e-02, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 437.3842048137838 b2 = 3.022615947048088E-4
		// Actual answer b1 = 4.3736970754E+02 b2 = 3.0227324449E-04
		
		// Ceres Solver Report: Iterations: 21, Initial cost: 5.601328e+03, Final cost: 2.820967e-02, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 437.3803979067243 b2 = 3.022646467847514E-4
		// Actual answer b1 = 4.3736970754E+02 b2 = 3.0227324449E-04
		
		int i;
		double x[] = new double[2];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 450.0;
        	x[1] = 3.0E-4;
        }
        else {
        	x[0] = 500.0;
        	x[1] = 1.0E-4;
        }
		CostFunction cost_function = new MisraldCostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1]);
		}
		System.out.println("Actual answer b1 = 4.3736970754E+02 b2 = 3.0227324449E-04");
		} // for (i = 0; i < 2; i++)
	}
	
	protected final int Lanczos1Observations = 24;
	protected double Lanczos1Data[] = new double[]{
			2.513400000000E+00,  0.000000000000E+00,
		       2.044333373291E+00,  5.000000000000E-02,
		       1.668404436564E+00,  1.000000000000E-01,
		       1.366418021208E+00,  1.500000000000E-01,
		       1.123232487372E+00,  2.000000000000E-01,
		       9.268897180037E-01,  2.500000000000E-01,
		       7.679338563728E-01,  3.000000000000E-01,
		       6.388775523106E-01,  3.500000000000E-01,
		       5.337835317402E-01,  4.000000000000E-01,
		       4.479363617347E-01,  4.500000000000E-01,
		       3.775847884350E-01,  5.000000000000E-01,
		       3.197393199326E-01,  5.500000000000E-01,
		       2.720130773746E-01,  6.000000000000E-01,
		       2.324965529032E-01,  6.500000000000E-01,
		       1.996589546065E-01,  7.000000000000E-01,
		       1.722704126914E-01,  7.500000000000E-01,
		       1.493405660168E-01,  8.000000000000E-01,
		       1.300700206922E-01,  8.500000000000E-01,
		       1.138119324644E-01,  9.000000000000E-01,
		       1.000415587559E-01,  9.500000000000E-01,
		       8.833209084540E-02,  1.000000000000E+00,
		       7.833544019350E-02,  1.050000000000E+00,
		       6.976693743449E-02,  1.100000000000E+00,
		       6.239312536719E-02,  1.150000000000E+00
	};
	
	class Lanczos1CostFunction extends SizedCostFunction {
		public Lanczos1CostFunction() {
			// number of residuals
			// size of first parameter
			super(24, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Lanczos1Observations; i++) {
			    residuals[i] = Lanczos1Data[2*i] - x[0]*Math.exp(-x[1]*Lanczos1Data[2*i+1])
			    		- x[2]*Math.exp(-x[3]*Lanczos1Data[2*i+1]) - x[4]*Math.exp(-x[5]*Lanczos1Data[2*i+1]);
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][6*i] = -Math.exp(-x[1]*Lanczos1Data[2*i+1]);
					jacobians[0][6*i+1] = Lanczos1Data[2*i+1]*x[0]*Math.exp(-x[1]*Lanczos1Data[2*i+1]);
					jacobians[0][6*i+2] = -Math.exp(-x[3]*Lanczos1Data[2*i+1]);
					jacobians[0][6*i+3] = Lanczos1Data[2*i+1]* x[2]*Math.exp(-x[3]*Lanczos1Data[2*i+1]);
					jacobians[0][6*i+4] = -Math.exp(-x[5]*Lanczos1Data[2*i+1]);
					jacobians[0][6*i+5] = Lanczos1Data[2*i+1]* x[4]*Math.exp(-x[5]*Lanczos1Data[2*i+1]);
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Lanczos1Observations; i++) {
			    residuals[i] = Lanczos1Data[2*i] - x[0]*Math.exp(-x[1]*Lanczos1Data[2*i+1])
			    		- x[2]*Math.exp(-x[3]*Lanczos1Data[2*i+1]) - x[4]*Math.exp(-x[5]*Lanczos1Data[2*i+1]);
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+6*i] = -Math.exp(-x[1]*Lanczos1Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+1] = Lanczos1Data[2*i+1]*x[0]*Math.exp(-x[1]*Lanczos1Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+2] = -Math.exp(-x[3]*Lanczos1Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+3] = Lanczos1Data[2*i+1]*x[2]*Math.exp(-x[3]*Lanczos1Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+4] = -Math.exp(-x[5]*Lanczos1Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+5] = Lanczos1Data[2*i+1]*x[4]*Math.exp(-x[5]*Lanczos1Data[2*i+1]);
			    }
			}

			
			return true;
		
	  }
	} // class Lanczos1CostFunction
	
	public void runLanczos1CostFunctionExample() {
		// Close and distant starting points converge correctly
		// Ceres Solver Report: Iterations: 31, Initial cost: 3.939431e+01, Final cost: 4.352743e-25, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 0.09510000051453531 b2 = 1.0000000026107674 b3 = 0.8607000014287124
		// b4 = 3.0000000033514445 b5 = 1.5575999980570767 b6 = 5.000000001164002
		// Actual answer b1 =  9.5100000027E-02 b2 = 1.0000000001E+00  b3 = 8.6070000013E-01
		// b4 = 3.0000000002E+00  b5 = 1.5575999998E+00  b6 = 5.0000000001E+00

		// Ceres Solver Report: Iterations: 31, Initial cost: 1.348752e+02, Final cost: 2.377807e-23, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 0.09510000408895412 b2 = 1.0000000209230826 b3 = 0.860700010605986
		// b4 = 3.0000000258865036 b5 = 1.5575999853070075 b6 = 5.000000008645559
		// Actual answer b1 =  9.5100000027E-02 b2 = 1.0000000001E+00  b3 = 8.6070000013E-01
		// b4 = 3.0000000002E+00  b5 = 1.5575999998E+00  b6 = 5.0000000001E+00

		int i;
		double x[] = new double[6];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 0.5;
        	x[1] = 0.7;
        	x[2] = 3.6;
        	x[3] = 4.2;
        	x[4] = 4.0;
        	x[5] = 6.3;
        }
        else {
        	x[0] = 1.2;
        	x[1] = 0.3;
        	x[2] = 5.6;
        	x[3] = 5.5;
        	x[4] = 6.5;
        	x[5] = 7.6;
        }
        
		CostFunction cost_function = new Lanczos1CostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;
		solverOptions.function_tolerance = 1.0E-8;
		solverOptions.min_trust_region_radius = 1.0E-50;
		solverOptions.initial_trust_region_radius = 1e15;
		solverOptions.min_relative_decrease = 1.0E-1;
		solverOptions.use_nonmonotonic_steps = true;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		System.out.println("b4 = " + x[3] + " b5 = " + x[4] + " b6 = " + x[5]);
		System.out.println("Actual answer b1 =  9.5100000027E-02 b2 = 1.0000000001E+00  b3 = 8.6070000013E-01");
		System.out.println("b4 = 3.0000000002E+00  b5 = 1.5575999998E+00  b6 = 5.0000000001E+00");
		} // for (i = 0; i < 2; i++)			
	}
	
	protected final int Lanczos2Observations = 24;
	protected double Lanczos2Data[] = new double[]{
			2.51340E+00,  0.00000E+00,
		       2.04433E+00,  5.00000E-02,
		       1.66840E+00,  1.00000E-01,
		       1.36642E+00,  1.50000E-01,
		       1.12323E+00,  2.00000E-01,
		       9.26890E-01,  2.50000E-01,
		       7.67934E-01,  3.00000E-01,
		       6.38878E-01,  3.50000E-01,
		       5.33784E-01,  4.00000E-01,
		       4.47936E-01,  4.50000E-01,
		       3.77585E-01,  5.00000E-01,
		       3.19739E-01,  5.50000E-01,
		       2.72013E-01,  6.00000E-01,
		       2.32497E-01,  6.50000E-01,
		       1.99659E-01,  7.00000E-01,
		       1.72270E-01,  7.50000E-01,
		       1.49341E-01,  8.00000E-01,
		       1.30070E-01,  8.50000E-01,
		       1.13812E-01,  9.00000E-01,
		       1.00042E-01,  9.50000E-01,
		       8.83321E-02,  1.00000E+00,
		       7.83354E-02,  1.05000E+00,
		       6.97669E-02,  1.10000E+00,
		       6.23931E-02,  1.15000E+00
	};
	
	class Lanczos2CostFunction extends SizedCostFunction {
		public Lanczos2CostFunction() {
			// number of residuals
			// size of first parameter
			super(24, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Lanczos2Observations; i++) {
			    residuals[i] = Lanczos2Data[2*i] - x[0]*Math.exp(-x[1]*Lanczos2Data[2*i+1])
			    		- x[2]*Math.exp(-x[3]*Lanczos2Data[2*i+1]) - x[4]*Math.exp(-x[5]*Lanczos2Data[2*i+1]);
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][6*i] = -Math.exp(-x[1]*Lanczos2Data[2*i+1]);
					jacobians[0][6*i+1] = Lanczos2Data[2*i+1]*x[0]*Math.exp(-x[1]*Lanczos2Data[2*i+1]);
					jacobians[0][6*i+2] = -Math.exp(-x[3]*Lanczos2Data[2*i+1]);
					jacobians[0][6*i+3] = Lanczos2Data[2*i+1]* x[2]*Math.exp(-x[3]*Lanczos2Data[2*i+1]);
					jacobians[0][6*i+4] = -Math.exp(-x[5]*Lanczos2Data[2*i+1]);
					jacobians[0][6*i+5] = Lanczos2Data[2*i+1]* x[4]*Math.exp(-x[5]*Lanczos2Data[2*i+1]);
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Lanczos2Observations; i++) {
			    residuals[i] = Lanczos2Data[2*i] - x[0]*Math.exp(-x[1]*Lanczos2Data[2*i+1])
			    		- x[2]*Math.exp(-x[3]*Lanczos2Data[2*i+1]) - x[4]*Math.exp(-x[5]*Lanczos2Data[2*i+1]);
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+6*i] = -Math.exp(-x[1]*Lanczos2Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+1] = Lanczos2Data[2*i+1]*x[0]*Math.exp(-x[1]*Lanczos2Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+2] = -Math.exp(-x[3]*Lanczos2Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+3] = Lanczos2Data[2*i+1]*x[2]*Math.exp(-x[3]*Lanczos2Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+4] = -Math.exp(-x[5]*Lanczos2Data[2*i+1]);
					jacobians[0][jacobians_offset[0]+6*i+5] = Lanczos2Data[2*i+1]*x[4]*Math.exp(-x[5]*Lanczos2Data[2*i+1]);
			    }
			}

			
			return true;
		
	  }
	} // class Lanczos2CostFunction
	
	public void runLanczos2CostFunctionExample() {
		// Converges correctly for the close and distant starting points
		// Ceres Solver Report: Iterations: 8, Initial cost: 3.939434e+01, Final cost: 1.114971e-11, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 0.09625107871948789 b2 = 1.0057335304684423 b3 = 0.8642470223057632
		// b4 = 3.0078287044598797 b5 = 1.5529015073681847 b6 = 5.002879916790506
		// Actual answer b1 =  9.6251029939E-02 b2 = 1.0057332849E+00  b3 = 8.6424689056E-01
		// b4 = 3.0078283915E+00  b5 = 1.5529016879E+00  b6 = 5.0028798100E+00
	
		// Ceres Solver Report: Iterations: 9, Initial cost: 1.348752e+02, Final cost: 1.114971e-11, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 0.09625129304080209 b2 = 1.005734616541688 b3 = 0.8642475764263094
		// b4 = 3.007830053627379 b5 = 1.552900739021002 b6 = 5.002880367409557
		// Actual answer b1 =  9.6251029939E-02 b2 = 1.0057332849E+00  b3 = 8.6424689056E-01
		// b4 = 3.0078283915E+00  b5 = 1.5529016879E+00  b6 = 5.0028798100E+00
		
		int i;
		double x[] = new double[6];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 0.5;
        	x[1] = 0.7;
        	x[2] = 3.6;
        	x[3] = 4.2;
        	x[4] = 4.0;
        	x[5] = 6.3;
        }
        else {
        	x[0] = 1.2;
        	x[1] = 0.3;
        	x[2] = 5.6;
        	x[3] = 5.5;
        	x[4] = 6.5;
        	x[5] = 7.6;
        }
        
		CostFunction cost_function = new Lanczos2CostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;
		solverOptions.function_tolerance = 1.0E-8;
		solverOptions.min_trust_region_radius = 1.0E-50;
		solverOptions.initial_trust_region_radius = 1e12;
		solverOptions.use_nonmonotonic_steps = true;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		System.out.println("b4 = " + x[3] + " b5 = " + x[4] + " b6 = " + x[5]);
		System.out.println("Actual answer b1 =  9.6251029939E-02 b2 = 1.0057332849E+00  b3 = 8.6424689056E-01");
		System.out.println("b4 = 3.0078283915E+00  b5 = 1.5529016879E+00  b6 = 5.0028798100E+00");
		} // for (i = 0; i < 2; i++)			
	}
	
	protected final int Kirby2Observations = 151;
	protected double Kirby2Data[] = new double[]{
			0.0082E0,      9.65E0,
		       0.0112E0,     10.74E0,
		       0.0149E0,     11.81E0,
		       0.0198E0,     12.88E0,
		       0.0248E0,     14.06E0,
		       0.0324E0,     15.28E0,
		       0.0420E0,     16.63E0,
		       0.0549E0,     18.19E0,
		       0.0719E0,     19.88E0,
		       0.0963E0,     21.84E0,
		       0.1291E0,     24.00E0,
		       0.1710E0,     26.25E0,
		       0.2314E0,     28.86E0,
		       0.3227E0,     31.85E0,
		       0.4809E0,     35.79E0,
		       0.7084E0,     40.18E0,
		       1.0220E0,     44.74E0,
		       1.4580E0,     49.53E0,
		       1.9520E0,     53.94E0,
		       2.5410E0,     58.29E0,
		       3.2230E0,     62.63E0,
		       3.9990E0,     67.03E0,
		       4.8520E0,     71.25E0,
		       5.7320E0,     75.22E0,
		       6.7270E0,     79.33E0,
		       7.8350E0,     83.56E0,
		       9.0250E0,     87.75E0,
		      10.2670E0,     91.93E0,
		      11.5780E0,     96.10E0,
		      12.9440E0,    100.28E0,
		      14.3770E0,    104.46E0,
		      15.8560E0,    108.66E0,
		      17.3310E0,    112.71E0,
		      18.8850E0,    116.88E0,
		      20.5750E0,    121.33E0,
		      22.3200E0,    125.79E0,
		      22.3030E0,    125.79E0,
		      23.4600E0,    128.74E0,
		      24.0600E0,    130.27E0,
		      25.2720E0,    133.33E0,
		      25.8530E0,    134.79E0,
		      27.1100E0,    137.93E0,
		      27.6580E0,    139.33E0,
		      28.9240E0,    142.46E0,
		      29.5110E0,    143.90E0,
		      30.7100E0,    146.91E0,
		      31.3500E0,    148.51E0,
		      32.5200E0,    151.41E0,
		      33.2300E0,    153.17E0,
		      34.3300E0,    155.97E0,
		      35.0600E0,    157.76E0,
		      36.1700E0,    160.56E0,
		      36.8400E0,    162.30E0,
		      38.0100E0,    165.21E0,
		      38.6700E0,    166.90E0,
		      39.8700E0,    169.92E0,
		      40.0300E0,    170.32E0,
		      40.5000E0,    171.54E0,
		      41.3700E0,    173.79E0,
		      41.6700E0,    174.57E0,
		      42.3100E0,    176.25E0,
		      42.7300E0,    177.34E0,
		      43.4600E0,    179.19E0,
		      44.1400E0,    181.02E0,
		      44.5500E0,    182.08E0,
		      45.2200E0,    183.88E0,
		      45.9200E0,    185.75E0,
		      46.3000E0,    186.80E0,
		      47.0000E0,    188.63E0,
		      47.6800E0,    190.45E0,
		      48.0600E0,    191.48E0,
		      48.7400E0,    193.35E0,
		      49.4100E0,    195.22E0,
		      49.7600E0,    196.23E0,
		      50.4300E0,    198.05E0,
		      51.1100E0,    199.97E0,
		      51.5000E0,    201.06E0,
		      52.1200E0,    202.83E0,
		      52.7600E0,    204.69E0,
		      53.1800E0,    205.86E0,
		      53.7800E0,    207.58E0,
		      54.4600E0,    209.50E0,
		      54.8300E0,    210.65E0,
		      55.4000E0,    212.33E0,
		      56.4300E0,    215.43E0,
		      57.0300E0,    217.16E0,
		      58.0000E0,    220.21E0,
		      58.6100E0,    221.98E0,
		      59.5800E0,    225.06E0,
		      60.1100E0,    226.79E0,
		      61.1000E0,    229.92E0,
		      61.6500E0,    231.69E0,
		      62.5900E0,    234.77E0,
		      63.1200E0,    236.60E0,
		      64.0300E0,    239.63E0,
		      64.6200E0,    241.50E0,
		      65.4900E0,    244.48E0,
		      66.0300E0,    246.40E0,
		      66.8900E0,    249.35E0,
		      67.4200E0,    251.32E0,
		      68.2300E0,    254.22E0,
		      68.7700E0,    256.24E0,
		      69.5900E0,    259.11E0,
		      70.1100E0,    261.18E0,
		      70.8600E0,    264.02E0,
		      71.4300E0,    266.13E0,
		      72.1600E0,    268.94E0,
		      72.7000E0,    271.09E0,
		      73.4000E0,    273.87E0,
		      73.9300E0,    276.08E0,
		      74.6000E0,    278.83E0,
		      75.1600E0,    281.08E0,
		      75.8200E0,    283.81E0,
		      76.3400E0,    286.11E0,
		      76.9800E0,    288.81E0,
		      77.4800E0,    291.08E0,
		      78.0800E0,    293.75E0,
		      78.6000E0,    295.99E0,
		      79.1700E0,    298.64E0,
		      79.6200E0,    300.84E0,
		      79.8800E0,    302.02E0,
		      80.1900E0,    303.48E0,
		      80.6600E0,    305.65E0,
		      81.2200E0,    308.27E0,
		      81.6600E0,    310.41E0,
		      82.1600E0,    313.01E0,
		      82.5900E0,    315.12E0,
		      83.1400E0,    317.71E0,
		      83.5000E0,    319.79E0,
		      84.0000E0,    322.36E0,
		      84.4000E0,    324.42E0,
		      84.8900E0,    326.98E0,
		      85.2600E0,    329.01E0,
		      85.7400E0,    331.56E0,
		      86.0700E0,    333.56E0,
		      86.5400E0,    336.10E0,
		      86.8900E0,    338.08E0,
		      87.3200E0,    340.60E0,
		      87.6500E0,    342.57E0,
		      88.1000E0,    345.08E0,
		      88.4300E0,    347.02E0,
		      88.8300E0,    349.52E0,
		      89.1200E0,    351.44E0,
		      89.5400E0,    353.93E0,
		      89.8500E0,    355.83E0,
		      90.2500E0,    358.32E0,
		      90.5500E0,    360.20E0,
		      90.9300E0,    362.67E0,
		      91.2000E0,    364.53E0,
		      91.5500E0,    367.00E0,
		      92.2000E0,    371.30E0
	};
	
	class Kirby2CostFunction extends SizedCostFunction {
		public Kirby2CostFunction() {
			// number of residuals
			// size of first parameter
			super(151, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Kirby2Observations; i++) {
				double KSquared = Kirby2Data[2*i+1]*Kirby2Data[2*i+1];
				double num = x[0] + x[1]*Kirby2Data[2*i+1] + x[2]*KSquared;
				double denom = 1.0 + x[3]*Kirby2Data[2*i+1] + x[4]*KSquared;
			    residuals[i] = Kirby2Data[2*i] - num/denom;
			    if (jacobians != null && jacobians[0] != null) {
			    	double denomSquared = denom*denom;
					jacobians[0][5*i] = -1.0/denom;
					jacobians[0][5*i+1] = -Kirby2Data[2*i+1]/denom;
					jacobians[0][5*i+2] = -KSquared/denom;
					jacobians[0][5*i+3] = num*Kirby2Data[2*i+1]/denomSquared;
					jacobians[0][5*i+4] = num*KSquared/denomSquared;
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Kirby2Observations; i++) {
				double KSquared = Kirby2Data[2*i+1]*Kirby2Data[2*i+1];
				double num = x[0] + x[1]*Kirby2Data[2*i+1] + x[2]*KSquared;
				double denom = 1.0 + x[3]*Kirby2Data[2*i+1] + x[4]*KSquared;
			    residuals[i] = Kirby2Data[2*i] - num/denom;
			    if (jacobians != null && jacobians[0] != null) {
			    	double denomSquared = denom*denom;
					jacobians[0][jacobians_offset[0]+5*i] = -1.0/denom;
					jacobians[0][jacobians_offset[0]+5*i+1] = -Kirby2Data[2*i+1]/denom;
					jacobians[0][jacobians_offset[0]+5*i+2] = -KSquared/denom;
					jacobians[0][jacobians_offset[0]+5*i+3] = num*Kirby2Data[2*i+1]/denomSquared;
					jacobians[0][jacobians_offset[0]+5*i+4] = num*KSquared/denomSquared;
			    }
			}
			
			return true;
		
	  }
	} // class Kirby2CostFunction
	
	public void runKirby2CostFunctionExample() {
		// Both close and distant starting points converge correctly
		// Ceres Solver Report: Iterations: 7, Initial cost: 4.938605e+02, Final cost: 1.952537e+00, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 1.674506089718086 b2 = -0.13927396597854952 b3 = 0.0025961179838818005
		// b4 = -0.0017241813655015676 b5 = 2.1664801963951228E-5
		// Actual answer b1 = 1.6745063063E+00  b2 = -1.3927397867E-01  b3 = 2.5961181191E-03
		// b4 = -1.7241811870E-03  b5 = 2.1664802578E-05
	
		// Ceres Solver Report: Iterations: 8, Initial cost: 1.866427e+05, Final cost: 1.952537e+00, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 1.6745055906931134 b2 = -0.13927393672308427 b3 = 0.0025961176722667733
		// b4 = -0.0017241817770806426 b5 = 2.1664800549879833E-5
		// Actual answer b1 = 1.6745063063E+00  b2 = -1.3927397867E-01  b3 = 2.5961181191E-03
		// b4 = -1.7241811870E-03  b5 = 2.1664802578E-05
		
		int i;
		double x[] = new double[6];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 1.5;
        	x[1] = -0.15;
        	x[2] = 0.0025;
        	x[3] = -0.0015;
        	x[4] = 2.0E-5;
        }
        else {
        	x[0] = 2.0;
        	x[1] = -0.1;
        	x[2] = 0.003;
        	x[3] = -0.001;
        	x[4] = 1.0E-5;
        }
        
		CostFunction cost_function = new Kirby2CostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;
		solverOptions.function_tolerance = 1.0E-12;
		solverOptions.min_trust_region_radius = 1.0E-50;
		solverOptions.initial_trust_region_radius = 1e12;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		System.out.println("b4 = " + x[3] + " b5 = " + x[4]);
		System.out.println("Actual answer b1 = 1.6745063063E+00  b2 = -1.3927397867E-01  b3 = 2.5961181191E-03");
		System.out.println("b4 = -1.7241811870E-03  b5 = 2.1664802578E-05");
		} // for (i = 0; i < 2; i++)			
	}
	
	protected final int Hahn1Observations = 236;
	protected double Hahn1Data[] = new double[]{
			.591E0,         24.41E0,  
		       1.547E0,         34.82E0,  
		       2.902E0,         44.09E0,  
		       2.894E0,         45.07E0,  
		       4.703E0,         54.98E0,  
		       6.307E0,         65.51E0,  
		       7.03E0,          70.53E0,  
		       7.898E0,         75.70E0,  
		       9.470E0,         89.57E0,  
		       9.484E0,         91.14E0,  
		      10.072E0,         96.40E0,  
		      10.163E0,         97.19E0,  
		      11.615E0,        114.26E0,  
		      12.005E0,        120.25E0,  
		      12.478E0,        127.08E0,  
		      12.982E0,        133.55E0,  
		      12.970E0,        133.61E0,  
		      13.926E0,        158.67E0,  
		      14.452E0,        172.74E0,  
		      14.404E0,        171.31E0,  
		      15.190E0,        202.14E0,  
		      15.550E0,        220.55E0,  
		      15.528E0,        221.05E0,  
		      15.499E0,        221.39E0,  
		      16.131E0,        250.99E0,  
		      16.438E0,        268.99E0,  
		      16.387E0,        271.80E0,  
		      16.549E0,        271.97E0,  
		      16.872E0,        321.31E0,  
		      16.830E0,        321.69E0,  
		      16.926E0,        330.14E0,  
		      16.907E0,        333.03E0,  
		      16.966E0,        333.47E0,  
		      17.060E0,        340.77E0,  
		      17.122E0,        345.65E0,  
		      17.311E0,        373.11E0,  
		      17.355E0,        373.79E0,  
		      17.668E0,        411.82E0,  
		      17.767E0,        419.51E0,  
		      17.803E0,        421.59E0,  
		      17.765E0,        422.02E0,  
		      17.768E0,        422.47E0,  
		      17.736E0,        422.61E0,  
		      17.858E0,        441.75E0,  
		      17.877E0,        447.41E0,  
		      17.912E0,        448.7E0,   
		      18.046E0,        472.89E0,  
		      18.085E0,        476.69E0,  
		      18.291E0,        522.47E0,  
		      18.357E0,        522.62E0,  
		      18.426E0,        524.43E0,  
		      18.584E0,        546.75E0,  
		      18.610E0,        549.53E0,  
		      18.870E0,        575.29E0,  
		      18.795E0,        576.00E0,  
		      19.111E0,        625.55E0,  
		        .367E0,         20.15E0,  
		        .796E0,         28.78E0,  
		       0.892E0,         29.57E0,  
		       1.903E0,         37.41E0,  
		       2.150E0,         39.12E0,  
		       3.697E0,         50.24E0,  
		       5.870E0,         61.38E0,  
		       6.421E0,         66.25E0,  
		       7.422E0,         73.42E0,  
		       9.944E0,         95.52E0,  
		      11.023E0,        107.32E0,  
		      11.87E0,         122.04E0,  
		      12.786E0,        134.03E0,  
		      14.067E0,        163.19E0,  
		      13.974E0,        163.48E0,  
		      14.462E0,        175.70E0,  
		      14.464E0,        179.86E0,  
		      15.381E0,        211.27E0,  
		      15.483E0,        217.78E0,  
		      15.59E0,         219.14E0,  
		      16.075E0,        262.52E0,  
		      16.347E0,        268.01E0,  
		      16.181E0,        268.62E0,  
		      16.915E0,        336.25E0,  
		      17.003E0,        337.23E0,  
		      16.978E0,        339.33E0,  
		      17.756E0,        427.38E0,  
		      17.808E0,        428.58E0,  
		      17.868E0,        432.68E0,  
		      18.481E0,        528.99E0,  
		      18.486E0,        531.08E0,  
		      19.090E0,        628.34E0,  
		      16.062E0,        253.24E0,  
		      16.337E0,        273.13E0,  
		      16.345E0,        273.66E0,  
		      16.388E0,        282.10E0,  
		      17.159E0,        346.62E0,  
		      17.116E0,        347.19E0,  
		      17.164E0,        348.78E0,  
		      17.123E0,        351.18E0,  
		      17.979E0,        450.10E0,  
		      17.974E0,        450.35E0,  
		      18.007E0,        451.92E0,  
		      17.993E0,        455.56E0,  
		      18.523E0,        552.22E0,  
		      18.669E0,        553.56E0,  
		      18.617E0,        555.74E0,  
		      19.371E0,        652.59E0,  
		      19.330E0,        656.20E0,  
		       0.080E0,         14.13E0,  
		       0.248E0,         20.41E0,  
		       1.089E0,         31.30E0,  
		       1.418E0,         33.84E0,  
		       2.278E0,         39.70E0,  
		       3.624E0,         48.83E0,  
		       4.574E0,         54.50E0,  
		       5.556E0,         60.41E0,  
		       7.267E0,         72.77E0,  
		       7.695E0,         75.25E0,  
		       9.136E0,         86.84E0,  
		       9.959E0,         94.88E0,  
		       9.957E0,         96.40E0,  
		      11.600E0,        117.37E0,  
		      13.138E0,        139.08E0,  
		      13.564E0,        147.73E0,  
		      13.871E0,        158.63E0,  
		      13.994E0,        161.84E0,  
		      14.947E0,        192.11E0,  
		      15.473E0,        206.76E0,  
		      15.379E0,        209.07E0,  
		      15.455E0,        213.32E0,  
		      15.908E0,        226.44E0,  
		      16.114E0,        237.12E0,  
		      17.071E0,        330.90E0,  
		      17.135E0,        358.72E0,  
		      17.282E0,        370.77E0,  
		      17.368E0,        372.72E0,  
		      17.483E0,        396.24E0,  
		      17.764E0,        416.59E0,  
		      18.185E0,        484.02E0,  
		      18.271E0,        495.47E0,  
		      18.236E0,        514.78E0,  
		      18.237E0,        515.65E0,  
		      18.523E0,        519.47E0,  
		      18.627E0,        544.47E0,  
		      18.665E0,        560.11E0,  
		      19.086E0,        620.77E0,  
		       0.214E0,         18.97E0,  
		       0.943E0,         28.93E0,  
		       1.429E0,         33.91E0,  
		       2.241E0,         40.03E0,  
		       2.951E0,         44.66E0,  
		       3.782E0,         49.87E0,  
		       4.757E0,         55.16E0,  
		       5.602E0,         60.90E0,  
		       7.169E0,         72.08E0,  
		       8.920E0,         85.15E0,  
		      10.055E0,         97.06E0,  
		      12.035E0,        119.63E0,  
		      12.861E0,        133.27E0,  
		      13.436E0,        143.84E0,  
		      14.167E0,        161.91E0,  
		      14.755E0,        180.67E0,  
		      15.168E0,        198.44E0,  
		      15.651E0,        226.86E0,  
		      15.746E0,        229.65E0,  
		      16.216E0,        258.27E0,  
		      16.445E0,        273.77E0,  
		      16.965E0,        339.15E0,  
		      17.121E0,        350.13E0,  
		      17.206E0,        362.75E0,  
		      17.250E0,        371.03E0,  
		      17.339E0,        393.32E0,  
		      17.793E0,        448.53E0,  
		      18.123E0,        473.78E0,  
		      18.49E0,         511.12E0,  
		      18.566E0,        524.70E0,  
		      18.645E0,        548.75E0,  
		      18.706E0,        551.64E0,  
		      18.924E0,        574.02E0,  
		      19.1E0,          623.86E0,  
		       0.375E0,         21.46E0,  
		       0.471E0,         24.33E0,  
		       1.504E0,         33.43E0,  
		       2.204E0,         39.22E0,  
		       2.813E0,         44.18E0,  
		       4.765E0,         55.02E0,  
		       9.835E0,         94.33E0,  
		      10.040E0,         96.44E0,  
		      11.946E0,        118.82E0,  
		      12.596E0,        128.48E0,  
		      13.303E0,        141.94E0,  
		      13.922E0,        156.92E0,  
		      14.440E0,        171.65E0,  
		      14.951E0,        190.00E0,  
		      15.627E0,        223.26E0,  
		      15.639E0,        223.88E0,  
		      15.814E0,        231.50E0,  
		      16.315E0,        265.05E0,  
		      16.334E0,        269.44E0,  
		      16.430E0,        271.78E0,  
		      16.423E0,        273.46E0,  
		      17.024E0,        334.61E0,  
		      17.009E0,        339.79E0,  
		      17.165E0,        349.52E0,  
		      17.134E0,        358.18E0,  
		      17.349E0,        377.98E0,  
		      17.576E0,        394.77E0,  
		      17.848E0,        429.66E0,  
		      18.090E0,        468.22E0,  
		      18.276E0,        487.27E0,  
		      18.404E0,        519.54E0,  
		      18.519E0,        523.03E0,  
		      19.133E0,        612.99E0,  
		      19.074E0,        638.59E0,  
		      19.239E0,        641.36E0,  
		      19.280E0,        622.05E0,  
		      19.101E0,        631.50E0,  
		      19.398E0,        663.97E0,  
		      19.252E0,        646.9E0,   
		      19.89E0,         748.29E0,  
		      20.007E0,        749.21E0,  
		      19.929E0,        750.14E0,  
		      19.268E0,        647.04E0,  
		      19.324E0,        646.89E0,  
		      20.049E0,        746.9E0,   
		      20.107E0,        748.43E0,  
		      20.062E0,        747.35E0,  
		      20.065E0,        749.27E0,  
		      19.286E0,        647.61E0,  
		      19.972E0,        747.78E0,  
		      20.088E0,        750.51E0,  
		      20.743E0,        851.37E0,  
		      20.83E0,         845.97E0,  
		      20.935E0,        847.54E0,  
		      21.035E0,        849.93E0,  
		      20.93E0,         851.61E0,  
		      21.074E0,        849.75E0,  
		      21.085E0,        850.98E0,  
		      20.935E0,        848.23E0  
	};
	
	class Hahn1CostFunction extends SizedCostFunction {
		public Hahn1CostFunction() {
			// number of residuals
			// size of first parameter
			super(236, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Hahn1Observations; i++) {
				double HSquared = Hahn1Data[2*i+1]*Hahn1Data[2*i+1];
				double HCubed = HSquared*Hahn1Data[2*i+1];
				double num = x[0] + x[1]*Hahn1Data[2*i+1] + x[2]*HSquared + x[3]*HCubed;
				double denom = 1.0 + x[4]*Hahn1Data[2*i+1] + x[5]*HSquared + x[6]*HCubed;
			    residuals[i] = Hahn1Data[2*i] - num/denom;
			    if (jacobians != null && jacobians[0] != null) {
			    	double denomSquared = denom*denom;
					jacobians[0][7*i] = -1.0/denom;
					jacobians[0][7*i+1] = -Hahn1Data[2*i+1]/denom;
					jacobians[0][7*i+2] = -HSquared/denom;
					jacobians[0][7*i+3] = -HCubed/denom;
					jacobians[0][7*i+4] = num*Hahn1Data[2*i+1]/denomSquared;
					jacobians[0][7*i+5] = num*HSquared/denomSquared;
					jacobians[0][7*i+6] = num*HCubed/denomSquared;
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < Hahn1Observations; i++) {
				double HSquared = Hahn1Data[2*i+1]*Hahn1Data[2*i+1];
				double HCubed = HSquared*Hahn1Data[2*i+1];
				double num = x[0] + x[1]*Hahn1Data[2*i+1] + x[2]*HSquared + x[3]*HCubed;
				double denom = 1.0 + x[4]*Hahn1Data[2*i+1] + x[5]*HSquared + x[6]*HCubed;
			    residuals[i] = Hahn1Data[2*i] - num/denom;
			    if (jacobians != null && jacobians[0] != null) {
			    	double denomSquared = denom*denom;
					jacobians[0][jacobians_offset[0]+7*i] = -1.0/denom;
					jacobians[0][jacobians_offset[0]+7*i+1] = -Hahn1Data[2*i+1]/denom;
					jacobians[0][jacobians_offset[0]+7*i+2] = -HSquared/denom;
					jacobians[0][jacobians_offset[0]+7*i+3] = -HCubed/denom;
					jacobians[0][jacobians_offset[0]+7*i+4] = num*Hahn1Data[2*i+1]/denomSquared;
					jacobians[0][jacobians_offset[0]+7*i+5] = num*HSquared/denomSquared;
					jacobians[0][jacobians_offset[0]+7*i+6] = num*HCubed/denomSquared;
			    }
			}
			
			return true;
		
	  }
	} // class Hahn1CostFunction
	
	public void runHahn1CostFunctionExample() {
		// Both close and distant starting points converge to the correct answer
		
		// Ceres Solver Report: Iterations: 30, Initial cost: 1.046724e+06, Final cost: 7.662191e-01, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 1.0776411396714223 b2 = -0.12269337966454558 b3 = 0.0040863818178511375
		// b4 = -1.4262672840184519E-6 b5 = -0.005760999802931795 b6 = 2.4053771652751907E-4
		// b7 = -1.2314461110115458E-7
		// Actual answer b1 = 1.0776351733E+00  b2 = -1.2269296921E-01  b3 = 4.0863750610E-03
		// b4 = -1.4262662514E-06  b5 = -5.7609940901E-03  b6 = 2.4053735503E-04
		// b7 = -1.2314450199E-07
		
		// Ceres Solver Report: Iterations: 30, Initial cost: 1.548778e+06, Final cost: 7.662191e-01, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 1.077657760149886 b2 = -0.12269467483612953 b3 = 0.0040864074618869
		// b4 = -1.4262973146878373E-6 b5 = -0.005760992441341054 b6 = 2.4053887271245495E-4
		// b7 = -1.2314606592047063E-7
		//Actual answer b1 = 1.0776351733E+00  b2 = -1.2269296921E-01  b3 = 4.0863750610E-03
		// b4 = -1.4262662514E-06  b5 = -5.7609940901E-03  b6 = 2.4053735503E-04
		// b7 = -1.2314450199E-07
		int i;
		double x[] = new double[7];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 1.0;
        	x[1] = -0.1;
        	x[2] = 0.005;
        	x[3] = -1.0E-6;
        	x[4] = -0.005;
        	x[5] = 1.0E-4;
        	x[6] = -1.0E-7;
        }
        else {
        	x[0] = 10.0;
        	x[1] = -1.0;
        	x[2] = 0.05;
        	x[3] = -1.0E-5;
        	x[4] = -0.05;
        	x[5] = 0.001;
        	x[6] = -1.0E-6;
        }
        
		CostFunction cost_function = new Hahn1CostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;
		solverOptions.function_tolerance = 1.0E-12;
		solverOptions.min_trust_region_radius = 1.0E-50;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		System.out.println("b4 = " + x[3] + " b5 = " + x[4] + " b6 = " + x[5]);
		System.out.println("b7 = " + x[6]);
		System.out.println("Actual answer b1 = 1.0776351733E+00  b2 = -1.2269296921E-01  b3 = 4.0863750610E-03");
		System.out.println("b4 = -1.4262662514E-06  b5 = -5.7609940901E-03  b6 = 2.4053735503E-04");
		System.out.println("b7 = -1.2314450199E-07");
		} // for (i = 0; i < 2; i++)			
	}
	
	protected final int NelsonObservations = 128;
	protected double NelsonData[] = new double[]{
			// y              x1            x2
		      15.00E0,         1E0,         180E0,
		      17.00E0,         1E0,         180E0,
		      15.50E0,         1E0,         180E0,
		      16.50E0,         1E0,         180E0,
		      15.50E0,         1E0,         225E0,
		      15.00E0,         1E0,         225E0,
		      16.00E0,         1E0,         225E0,
		      14.50E0,         1E0,         225E0,
		      15.00E0,         1E0,         250E0,
		      14.50E0,         1E0,         250E0,
		      12.50E0,         1E0,         250E0,
		      11.00E0,         1E0,         250E0,
		      14.00E0,         1E0,         275E0,
		      13.00E0,         1E0,         275E0,
		      14.00E0,         1E0,         275E0,
		      11.50E0,         1E0,         275E0,
		      14.00E0,         2E0,         180E0,
		      16.00E0,         2E0,         180E0,
		      13.00E0,         2E0,         180E0,
		      13.50E0,         2E0,         180E0,
		      13.00E0,         2E0,         225E0,
		      13.50E0,         2E0,         225E0,
		      12.50E0,         2E0,         225E0,
		      12.50E0,         2E0,         225E0,
		      12.50E0,         2E0,         250E0,
		      12.00E0,         2E0,         250E0,
		      11.50E0,         2E0,         250E0,
		      12.00E0,         2E0,         250E0,
		      13.00E0,         2E0,         275E0,
		      11.50E0,         2E0,         275E0,
		      13.00E0,         2E0,         275E0,
		      12.50E0,         2E0,         275E0,
		      13.50E0,         4E0,         180E0,
		      17.50E0,         4E0,         180E0,
		      17.50E0,         4E0,         180E0,
		      13.50E0,         4E0,         180E0,
		      12.50E0,         4E0,         225E0,
		      12.50E0,         4E0,         225E0,
		      15.00E0,         4E0,         225E0,
		      13.00E0,         4E0,         225E0,
		      12.00E0,         4E0,         250E0,
		      13.00E0,         4E0,         250E0,
		      12.00E0,         4E0,         250E0,
		      13.50E0,         4E0,         250E0,
		      10.00E0,         4E0,         275E0,
		      11.50E0,         4E0,         275E0,
		      11.00E0,         4E0,         275E0,
		       9.50E0,         4E0,         275E0,
		      15.00E0,         8E0,         180E0,
		      15.00E0,         8E0,         180E0,
		      15.50E0,         8E0,         180E0,
		      16.00E0,         8E0,         180E0,
		      13.00E0,         8E0,         225E0,
		      10.50E0,         8E0,         225E0,
		      13.50E0,         8E0,         225E0,
		      14.00E0,         8E0,         225E0,
		      12.50E0,         8E0,         250E0,
		      12.00E0,         8E0,         250E0,
		      11.50E0,         8E0,         250E0,
		      11.50E0,         8E0,         250E0,
		       6.50E0,         8E0,         275E0,
		       5.50E0,         8E0,         275E0,
		       6.00E0,         8E0,         275E0,
		       6.00E0,         8E0,         275E0,
		      18.50E0,        16E0,         180E0,
		      17.00E0,        16E0,         180E0,
		      15.30E0,        16E0,         180E0,
		      16.00E0,        16E0,         180E0,
		      13.00E0,        16E0,         225E0,
		      14.00E0,        16E0,         225E0,
		      12.50E0,        16E0,         225E0,
		      11.00E0,        16E0,         225E0,
		      12.00E0,        16E0,         250E0,
		      12.00E0,        16E0,         250E0,
		      11.50E0,        16E0,         250E0,
		      12.00E0,        16E0,         250E0,
		       6.00E0,        16E0,         275E0,
		       6.00E0,        16E0,         275E0,
		       5.00E0,        16E0,         275E0,
		       5.50E0,        16E0,         275E0,
		      12.50E0,        32E0,         180E0,
		      13.00E0,        32E0,         180E0,
		      16.00E0,        32E0,         180E0,
		      12.00E0,        32E0,         180E0,
		      11.00E0,        32E0,         225E0,
		       9.50E0,        32E0,         225E0,
		      11.00E0,        32E0,         225E0,
		      11.00E0,        32E0,         225E0,
		      11.00E0,        32E0,         250E0,
		      10.00E0,        32E0,         250E0,
		      10.50E0,        32E0,         250E0,
		      10.50E0,        32E0,         250E0,
		       2.70E0,        32E0,         275E0,
		       2.70E0,        32E0,         275E0,
		       2.50E0,        32E0,         275E0,
		       2.40E0,        32E0,         275E0,
		      13.00E0,        48E0,         180E0,
		      13.50E0,        48E0,         180E0,
		      16.50E0,        48E0,         180E0,
		      13.60E0,        48E0,         180E0,
		      11.50E0,        48E0,         225E0,
		      10.50E0,        48E0,         225E0,
		      13.50E0,        48E0,         225E0,
		      12.00E0,        48E0,         225E0,
		       7.00E0,        48E0,         250E0,
		       6.90E0,        48E0,         250E0,
		       8.80E0,        48E0,         250E0,
		       7.90E0,        48E0,         250E0,
		       1.20E0,        48E0,         275E0,
		       1.50E0,        48E0,         275E0,
		       1.00E0,        48E0,         275E0,
		       1.50E0,        48E0,         275E0,
		      13.00E0,        64E0,         180E0,
		      12.50E0,        64E0,         180E0,
		      16.50E0,        64E0,         180E0,
		      16.00E0,        64E0,         180E0,
		      11.00E0,        64E0,         225E0,
		      11.50E0,        64E0,         225E0,
		      10.50E0,        64E0,         225E0,
		      10.00E0,        64E0,         225E0,
		       7.27E0,        64E0,         250E0,
		       7.50E0,        64E0,         250E0,
		       6.70E0,        64E0,         250E0,
		       7.60E0,        64E0,         250E0,
		       1.50E0,        64E0,         275E0,
		       1.00E0,        64E0,         275E0,
		       1.20E0,        64E0,         275E0,
		       1.20E0,        64E0,         275E0

	};
 
	class NelsonCostFunction extends SizedCostFunction {
		public NelsonCostFunction() {
			// number of residuals
			// size of first parameter
			super(128, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < NelsonObservations; i++) {
				double exp = Math.exp(-x[2]*NelsonData[3*i+2]);
			    residuals[i] = Math.log(NelsonData[3*i]) - x[0] + x[1]*NelsonData[3*i+1]*exp;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][3*i] = -1.0;
					jacobians[0][3*i+1] = NelsonData[3*i+1]*exp;
					jacobians[0][3*i+2] = -x[1]*NelsonData[3*i+1]*NelsonData[3*i+2]*exp;
			    }
			}

			return true;
		
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < NelsonObservations; i++) {
				double exp = Math.exp(-x[2]*NelsonData[3*i+2]);
			    residuals[i] = Math.log(NelsonData[3*i]) - x[0] + x[1]*NelsonData[3*i+1]*exp;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0]+3*i] = -1.0;
					jacobians[0][jacobians_offset[0]+3*i+1] = NelsonData[3*i+1]*exp;
					jacobians[0][jacobians_offset[0]+3*i+2] = -x[1]*NelsonData[3*i+1]*NelsonData[3*i+2]*exp;
			    }
			}
			
			
			return true;
		
	  }
	} // class NelsonCostFunction
	
	public void runNelsonCostFunctionExample() {
		// Close starting point converges to correct answer but distant starting point does not
		
		// Ceres Solver Report: Iterations: 39, Initial cost: 2.424496e+01, Final cost: 1.898842e+00, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 2.5906835195506814 b2 = 5.617720758983594E-9 b3 = -0.05770104605336713
		// Actual answer b1 = 2.5906836021E+00  b2 = 5.6177717026E-09  b3 = -5.7701013174E-02
		// Correct answer has Final cost: 1.898842e+00
	
		// Ceres Solver Report: Iterations: 119, Initial cost: 3.154177e+01, Final cost: 2.376316e+00, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 2.622039123598816 b2 = 2.1202258375182664E-7 b3 = -0.03483834335150183
		// Actual answer b1 = 2.5906836021E+00  b2 = 5.6177717026E-09  b3 = -5.7701013174E-02
		// Correct answer has Final cost: 1.898842e+00
		
		int i;
		double x[] = new double[3];
		for (i = 0; i < 2; i++) {
        if (i == 0) {
        	x[0] = 2.5;
        	x[1] = 5E-9;
        	x[2] = -0.05;
        }
        else {
        	x[0] = 2.0;
        	x[1] = 1.0E-4;
        	x[2] = -0.01;
        }
        
		CostFunction cost_function = new NelsonCostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, x);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
		solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
		solverOptions.max_num_consecutive_invalid_steps = 200;
		solverOptions.gradient_tolerance = epsilon;
		solverOptions.parameter_tolerance = epsilon;
		solverOptions.function_tolerance = 1.0E-12;
		solverOptions.min_trust_region_radius = 1.0E-50;
		solverOptions.use_nonmonotonic_steps = true;

		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		System.out.println(solverSummary.BriefReport());
		if (i == 0) {
		    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		else {
			System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
		}
		System.out.println("Actual answer b1 = 2.5906836021E+00  b2 = 5.6177717026E-09  b3 = -5.7701013174E-02");
		System.out.println("Correct answer has Final cost: 1.898842e+00");
		} // for (i = 0; i < 2; i++)
	}
		
		final int Roszman1Observations = 25;
		double Roszman1Data[] = new double[]{
				0.252429,    -4868.68,
			       0.252141,    -4868.09,
			       0.251809,    -4867.41,
			       0.297989,    -3375.19,
			       0.296257,    -3373.14,
			       0.295319,    -3372.03,
			       0.339603,    -2473.74,
			       0.337731,    -2472.35,
			       0.333820,    -2469.45,
			       0.389510,    -1894.65,
			       0.386998,    -1893.40,
			       0.438864,    -1497.24,
			       0.434887,    -1495.85,
			       0.427893,    -1493.41,
			       0.471568,    -1208.68,
			       0.461699,    -1206.18,
			       0.461144,    -1206.04,
			       0.513532,     -997.92,
			       0.506641,     -996.61,
			       0.505062,     -996.31,
			       0.535648,     -834.94,
			       0.533726,     -834.66,
			       0.568064,     -710.03,
			       0.612886,     -530.16,
			       0.624169,     -464.17
		};
		
		class Roszman1CostFunction extends SizedCostFunction {
			public Roszman1CostFunction() {
				// number of residuals
				// size of first parameter
				super(25, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0);
			}

			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < Roszman1Observations; i++) {
				    residuals[i] = Roszman1Data[2*i] - x[0] + x[1]*Roszman1Data[2*i+1] 
				    		+ Math.atan2(x[2],Roszman1Data[2*i+1] - x[3])/Math.PI;
				    if (jacobians != null && jacobians[0] != null) {
				    	double denom = Roszman1Data[2*i+1] - x[3];
				    	double ratio = x[2]/(Roszman1Data[2*i+1] - x[3]);
				    	double ratioSquared = ratio * ratio;
						jacobians[0][4*i] = -1.0;
						jacobians[0][4*i+1] = Roszman1Data[2*i+1];
						jacobians[0][4*i+2] = 1.0/(denom * (ratioSquared + 1.0) * Math.PI);
						jacobians[0][4*i+3] = x[2]/((x[3]*x[3] - 2.0*Roszman1Data[2*i+1]*x[3] 
								+ Roszman1Data[2*i+1]*Roszman1Data[2*i+1] + x[2]*x[2])*Math.PI);
				    }
				}

				return true;
			
		  }
			
			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < Roszman1Observations; i++) {
				    residuals[i] = Roszman1Data[2*i] - x[0] + x[1]*Roszman1Data[2*i+1] 
				    		+ Math.atan2(x[2],Roszman1Data[2*i+1] - x[3])/Math.PI;
				    if (jacobians != null && jacobians[0] != null) {
				    	double denom = Roszman1Data[2*i+1] - x[3];
				    	double ratio = x[2]/(Roszman1Data[2*i+1] - x[3]);
				    	double ratioSquared = ratio * ratio;
						jacobians[0][jacobians_offset[0]+4*i] = -1.0;
						jacobians[0][jacobians_offset[0]+4*i+1] = Roszman1Data[2*i+1];
						jacobians[0][jacobians_offset[0]+4*i+2] = 1.0/(denom * (ratioSquared + 1.0) * Math.PI);
						jacobians[0][jacobians_offset[0]+4*i+3] = x[2]/((x[3]*x[3] - 2.0*Roszman1Data[2*i+1]*x[3] 
								+ Roszman1Data[2*i+1]*Roszman1Data[2*i+1] + x[2]*x[2])*Math.PI);
				    }
				}
				
				return true;
			
		  }
		} // class Roszman1CostFunction
		
		public void runRoszman1CostFunctionExample() {
			// Roszman1 converges correctly for both close and distant starting points
			// Ceres Solver Report: Iterations: 50, Initial cost: 1.259393e+01, Final cost: 2.474242e-04, Termination: CONVERGENCE
			// Solved answer for close starting point b1 = 1.2019686238107419 b2 = -6.19534509350161E-6 b3 = 1204.4558151244883
			// b4 = -181.34279331958928
			// Actual answer b1 = 1.20196866396E00  b2 = -6.1953516256E-06  b3 = 1.2044556708E+03
			// b4 = -1.8134269537E+02
			
			// Ceres Solver Report: Iterations: 28, Initial cost: 1.632464e+01, Final cost: 2.474242e-04, Termination: CONVERGENCE
			// Solved answer for distant starting point b1 = 1.2019686211909493 b2 = -6.195344657727014E-6 b3 = 1204.455824049894
			// b4 = -181.34279995474526
			// Actual answer b1 = 1.20196866396E00  b2 = -6.1953516256E-06  b3 = 1.2044556708E+03
			// b4 = -1.8134269537E+02
			int i;
			double x[] = new double[4];
			for (i = 0; i < 2; i++) {
	        if (i == 0) {
	        	x[0] = 0.2;
	        	x[1] = -5E-6;
	        	x[2] = 1200;
	        	x[3] = -150;
	        }
	        else {
	        	x[0] = 0.1;
	        	x[1] = -1.0E-5;
	        	x[2] = 1000;
	        	x[3] = -100;
	        }
	        
			CostFunction cost_function = new Roszman1CostFunction();
			ProblemImpl problem = new ProblemImpl();
			problem.AddResidualBlock(cost_function, null, x);

			// Run the solver!
			SolverOptions solverOptions = new SolverOptions();
			solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
			solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
			solverOptions.max_num_consecutive_invalid_steps = 200;
			solverOptions.gradient_tolerance = epsilon;
			solverOptions.parameter_tolerance = epsilon;
			solverOptions.function_tolerance = 1.0E-14;
			solverOptions.min_trust_region_radius = 1.0E-50;

			solverOptions.minimizer_progress_to_stdout = true;
			SolverSummary solverSummary = new SolverSummary();
			Solve(solverOptions, problem, solverSummary);
			System.out.println(solverSummary.BriefReport());
			if (i == 0) {
			    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
			}
			else {
				System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
			}
			System.out.println("b4 = " + x[3]);
			System.out.println("Actual answer b1 = 1.20196866396E00  b2 = -6.1953516256E-06  b3 = 1.2044556708E+03");
			System.out.println("b4 = -1.8134269537E+02");
			} // for (i = 0; i < 2; i++)
		}
		
		final int ENSOObservations = 168;
		double ENSOData[] = new double[]{
				12.90000,    1.000000,
			    11.30000,    2.000000,
			    10.60000,    3.000000,
			    11.20000,    4.000000,
			    10.90000,    5.000000,
			    7.500000,    6.000000,
			    7.700000,    7.000000,
			    11.70000,    8.000000,
			    12.90000,    9.000000,
			    14.30000,   10.000000,
			    10.90000,    11.00000,
			    13.70000,    12.00000,
			    17.10000,    13.00000,
			    14.00000,    14.00000,
			    15.30000,    15.00000,
			    8.500000,    16.00000,
			    5.700000,    17.00000,
			    5.500000,    18.00000,
			    7.600000,    19.00000,
			    8.600000,    20.00000,
			    7.300000,    21.00000,
			    7.600000,    22.00000,
			    12.70000,    23.00000,
			    11.00000,    24.00000,
			    12.70000,    25.00000,
			    12.90000,    26.00000,
			    13.00000,    27.00000,
			    10.90000,    28.00000,
			   10.400000,    29.00000,
			   10.200000,    30.00000,
			    8.000000,    31.00000,
			    10.90000,    32.00000,
			    13.60000,    33.00000,
			   10.500000,    34.00000,
			    9.200000,    35.00000,
			    12.40000,    36.00000,
			    12.70000,    37.00000,
			    13.30000,    38.00000,
			   10.100000,    39.00000,
			    7.800000,    40.00000,
			    4.800000,    41.00000,
			    3.000000,    42.00000,
			    2.500000,    43.00000,
			    6.300000,    44.00000,
			    9.700000,    45.00000,
			    11.60000,    46.00000,
			    8.600000,    47.00000,
			    12.40000,    48.00000,
			   10.500000,    49.00000,
			    13.30000,    50.00000,
			   10.400000,    51.00000,
			    8.100000,    52.00000,
			    3.700000,    53.00000,
			    10.70000,    54.00000,
			    5.100000,    55.00000,
			   10.400000,    56.00000,
			    10.90000,    57.00000,
			    11.70000,    58.00000,
			    11.40000,    59.00000,
			    13.70000,    60.00000,
			    14.10000,    61.00000,
			    14.00000,    62.00000,
			    12.50000,    63.00000,
			    6.300000,    64.00000,
			    9.600000,    65.00000,
			    11.70000,    66.00000,
			    5.000000,    67.00000,
			    10.80000,    68.00000,
			    12.70000,    69.00000,
			    10.80000,    70.00000,
			    11.80000,    71.00000,
			    12.60000,    72.00000,
			    15.70000,    73.00000,
			    12.60000,    74.00000,
			    14.80000,    75.00000,
			    7.800000,    76.00000,
			    7.100000,    77.00000,
			    11.20000,    78.00000,
			    8.100000,    79.00000,
			    6.400000,    80.00000,
			    5.200000,    81.00000,
			    12.00000,    82.00000,
			   10.200000,    83.00000,
			    12.70000,    84.00000,
			   10.200000,    85.00000,
			    14.70000,    86.00000,
			    12.20000,    87.00000,
			    7.100000,    88.00000,
			    5.700000,    89.00000,
			    6.700000,    90.00000,
			    3.900000,    91.00000,
			    8.500000,    92.00000,
			    8.300000,    93.00000,
			    10.80000,    94.00000,
			    16.70000,    95.00000,
			    12.60000,    96.00000,
			    12.50000,    97.00000,
			    12.50000,    98.00000,
			    9.800000,    99.00000,
			    7.200000,   100.00000,
			    4.100000,   101.00000,
			    10.60000,   102.00000,
			   10.100000,   103.00000,
			   10.100000,   104.00000,
			    11.90000,   105.00000,
			    13.60000,    106.0000,
			    16.30000,    107.0000,
			    17.60000,    108.0000,
			    15.50000,    109.0000,
			    16.00000,    110.0000,
			    15.20000,    111.0000,
			    11.20000,    112.0000,
			    14.30000,    113.0000,
			    14.50000,    114.0000,
			    8.500000,    115.0000,
			    12.00000,    116.0000,
			    12.70000,    117.0000,
			    11.30000,    118.0000,
			    14.50000,    119.0000,
			    15.10000,    120.0000,
			   10.400000,    121.0000,
			    11.50000,    122.0000,
			    13.40000,    123.0000,
			    7.500000,    124.0000,
			   0.6000000,    125.0000,
			   0.3000000,    126.0000,
			    5.500000,    127.0000,
			    5.000000,    128.0000,
			    4.600000,    129.0000,
			    8.200000,    130.0000,
			    9.900000,    131.0000,
			    9.200000,    132.0000,
			    12.50000,    133.0000,
			    10.90000,    134.0000,
			    9.900000,    135.0000,
			    8.900000,    136.0000,
			    7.600000,    137.0000,
			    9.500000,    138.0000,
			    8.400000,    139.0000,
			    10.70000,    140.0000,
			    13.60000,    141.0000,
			    13.70000,    142.0000,
			    13.70000,    143.0000,
			    16.50000,    144.0000,
			    16.80000,    145.0000,
			    17.10000,    146.0000,
			    15.40000,    147.0000,
			    9.500000,    148.0000,
			    6.100000,    149.0000,
			   10.100000,    150.0000,
			    9.300000,    151.0000,
			    5.300000,    152.0000,
			    11.20000,    153.0000,
			    16.60000,    154.0000,
			    15.60000,    155.0000,
			    12.00000,    156.0000,
			    11.50000,    157.0000,
			    8.600000,    158.0000,
			    13.80000,    159.0000,
			    8.700000,    160.0000,
			    8.600000,    161.0000,
			    8.600000,    162.0000,
			    8.700000,    163.0000,
			    12.80000,    164.0000,
			    13.20000,    165.0000,
			    14.00000,    166.0000,
			    13.40000,    167.0000,
			    14.80000,    168.0000
		};
		
		class ENSOCostFunction extends SizedCostFunction {
			public ENSOCostFunction() {
				// number of residuals
				// size of first parameter
				super(168, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0);
			}

			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < ENSOObservations; i++) {
					double cos2 = Math.cos(Math.PI*ENSOData[2*i+1]/6.0);
					double sin2 = Math.sin(Math.PI*ENSOData[2*i+1]/6.0);
					double cos5 = Math.cos(2.0 * Math.PI * ENSOData[2*i+1]/x[3]);
					double sin5 = Math.sin(2.0 * Math.PI * ENSOData[2*i+1]/x[3]);
					double cos8 = Math.cos(2.0 * Math.PI * ENSOData[2*i+1]/x[6]);
					double sin8 = Math.sin(2.0 * Math.PI * ENSOData[2*i+1]/x[6]);
				    residuals[i] = ENSOData[2*i] - x[0] - x[1]*cos2 - x[2]*sin2 
				    		-x[4]*cos5 - x[5]*sin5 - x[7]*cos8 - x[8]*sin8;
				    if (jacobians != null && jacobians[0] != null) {
				    	double x3Squared = x[3]*x[3];
				    	double x6Squared = x[6]*x[6];
						jacobians[0][9*i] = -1.0;
						jacobians[0][9*i+1] = -cos2;
						jacobians[0][9*i+2] = -sin2;
						jacobians[0][9*i+3] = -x[4]*sin5*2.0*Math.PI*ENSOData[2*i+1]/x3Squared
					                          +x[5]*cos5*2.0*Math.PI*ENSOData[2*i+1]/x3Squared;
					    jacobians[0][9*i+4] = -cos5;
					    jacobians[0][9*i+5] = -sin5;
					    jacobians[0][9*i+6] = -x[7]*sin8*2.0*Math.PI*ENSOData[2*i+1]/x6Squared
					    		              +x[8]*cos8*2.0*Math.PI*ENSOData[2*i+1]/x6Squared;
					    jacobians[0][9*i+7] = -cos8;
					    jacobians[0][9*i+8] = -sin8;
				    }
				}

				return true;
			
		  }
			
			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < ENSOObservations; i++) {
					double cos2 = Math.cos(Math.PI*ENSOData[2*i+1]/6.0);
					double sin2 = Math.sin(Math.PI*ENSOData[2*i+1]/6.0);
					double cos5 = Math.cos(2.0 * Math.PI * ENSOData[2*i+1]/x[3]);
					double sin5 = Math.sin(2.0 * Math.PI * ENSOData[2*i+1]/x[3]);
					double cos8 = Math.cos(2.0 * Math.PI * ENSOData[2*i+1]/x[6]);
					double sin8 = Math.sin(2.0 * Math.PI * ENSOData[2*i+1]/x[6]);
				    residuals[i] = ENSOData[2*i] - x[0] - x[1]*cos2 - x[2]*sin2 
				    		-x[4]*cos5 - x[5]*sin5 - x[7]*cos8 - x[8]*sin8;
				    if (jacobians != null && jacobians[0] != null) {
				    	double x3Squared = x[3]*x[3];
				    	double x6Squared = x[6]*x[6];
						jacobians[0][jacobians_offset[0]+9*i] = -1.0;
						jacobians[0][jacobians_offset[0]+9*i+1] = -cos2;
						jacobians[0][jacobians_offset[0]+9*i+2] = -sin2;
						jacobians[0][jacobians_offset[0]+9*i+3] = -x[4]*sin5*2.0*Math.PI*ENSOData[2*i+1]/x3Squared
					                          +x[5]*cos5*2.0*Math.PI*ENSOData[2*i+1]/x3Squared;
					    jacobians[0][jacobians_offset[0]+9*i+4] = -cos5;
					    jacobians[0][jacobians_offset[0]+9*i+5] = -sin5;
					    jacobians[0][jacobians_offset[0]+9*i+6] = -x[7]*sin8*2.0*Math.PI*ENSOData[2*i+1]/x6Squared
					    		              +x[8]*cos8*2.0*Math.PI*ENSOData[2*i+1]/x6Squared;
					    jacobians[0][jacobians_offset[0]+9*i+7] = -cos8;
					    jacobians[0][jacobians_offset[0]+9*i+8] = -sin8;
				    }
				}
				
				return true;
			
		  }
		} // class ENSOCostFunction
		
		public void runENSOCostFunctionExample() {
			// Converges correctly for close and distant starting points
			// Ceres Solver Report: Iterations: 32, Initial cost: 4.574878e+02, Final cost: 3.942699e+02, Termination: CONVERGENCE
			// Solved answer for close starting point b1 = 10.510749198084643 b2 = 3.0762128023118374 b3 = 0.532801377069108
			// b4 = 44.311089093267604 b5 = -1.6231428094328249 b6 = 0.5255450407129744
			// b7 = 26.88761487511951 b8 = 0.2123232746982427 b9 = 1.4966869843233406
			// Actual answer b1 = 1.0510749193E+01  b2 = 3.0762128085E+00  b3 = 5.3280138227E-01
			// b4 = 4.4311088700E+01 b5 = -1.6231428586E+00 b6 = 5.2554493756E-01
			// b7 = 2.6887614440E+01 b8 = 2.1232288488E-01 b9 = 1.4966870418E+00
		
			// Ceres Solver Report: Iterations: 36, Initial cost: 5.769720e+02, Final cost: 3.942699e+02, Termination: CONVERGENCE
			// Solved answer for distant starting point b1 = 10.51074918830191 b2 = 3.0762128137817477 b3 = 0.5328013866431732
			// b4 = 44.311088369472195 b5 = -1.6231429000310378 b6 = 0.5255448507405884
			// b7 = 26.88761407390571 b8 = 0.212322556828547 b9 = 1.4966870902177205
			// Actual answer b1 = 1.0510749193E+01  b2 = 3.0762128085E+00  b3 = 5.3280138227E-01
			// b4 = 4.4311088700E+01 b5 = -1.6231428586E+00 b6 = 5.2554493756E-01
			// b7 = 2.6887614440E+01 b8 = 2.1232288488E-01 b9 = 1.4966870418E+00
			
			int i;
			double x[] = new double[9];
			for (i = 0; i < 2; i++) {
	        if (i == 0) {
	        	x[0] = 10.0;
	        	x[1] = 3.0;
	        	x[2] = 0.5;
	        	x[3] = 44.0;
	        	x[4] = -1.5;
	        	x[5] = 0.5;
	        	x[6] = 26.0;
	        	x[7] = -0.1;
	        	x[8] = 1.5;
	        }
	        else {
	        	x[0] = 11.0;
	        	x[1] = 3.0;
	        	x[2] = 0.5;
	        	x[3] = 40.0;
	        	x[4] = -0.7;
	        	x[5] = -1.3;
	        	x[6] = 25.0;
	        	x[7] = -0.3;
	        	x[8] = 1.4;
	        }
	        
			CostFunction cost_function = new ENSOCostFunction();
			ProblemImpl problem = new ProblemImpl();
			problem.AddResidualBlock(cost_function, null, x);

			// Run the solver!
			SolverOptions solverOptions = new SolverOptions();
			solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
			solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
			solverOptions.max_num_consecutive_invalid_steps = 200;
			solverOptions.gradient_tolerance = epsilon;
			solverOptions.parameter_tolerance = epsilon;
			solverOptions.function_tolerance = 1.0E-14;
			solverOptions.min_trust_region_radius = 1.0E-60;
			solverOptions.initial_trust_region_radius = 1e12;
			solverOptions.use_nonmonotonic_steps = true;

			solverOptions.minimizer_progress_to_stdout = true;
			SolverSummary solverSummary = new SolverSummary();
			Solve(solverOptions, problem, solverSummary);
			System.out.println(solverSummary.BriefReport());
			if (i == 0) {
			    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
			}
			else {
				System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
			}
			System.out.println("b4 = " + x[3] + " b5 = " + x[4] + " b6 = " + x[5]);
			System.out.println("b7 = " + x[6] + " b8 = " + x[7] + " b9 = " + x[8]);
			System.out.println("Actual answer b1 = 1.0510749193E+01  b2 = 3.0762128085E+00  b3 = 5.3280138227E-01");
			System.out.println("b4 = 4.4311088700E+01 b5 = -1.6231428586E+00 b6 = 5.2554493756E-01");
			System.out.println("b7 = 2.6887614440E+01 b8 = 2.1232288488E-01 b9 = 1.4966870418E+00");
			} // for (i = 0; i < 2; i++)
		}
		
		final int ThurberObservations = 37;
		double ThurberData[] = new double[]{
				80.574E0,      -3.067E0,
			      84.248E0,      -2.981E0,
			      87.264E0,      -2.921E0,
			      87.195E0,      -2.912E0,
			      89.076E0,      -2.840E0,
			      89.608E0,      -2.797E0,
			      89.868E0,      -2.702E0,
			      90.101E0,      -2.699E0,
			      92.405E0,      -2.633E0,
			      95.854E0,      -2.481E0,
			     100.696E0,      -2.363E0,
			     101.060E0,      -2.322E0,
			     401.672E0,      -1.501E0,
			     390.724E0,      -1.460E0,
			     567.534E0,      -1.274E0,
			     635.316E0,      -1.212E0,
			     733.054E0,      -1.100E0,
			     759.087E0,      -1.046E0,
			     894.206E0,      -0.915E0,
			     990.785E0,      -0.714E0,
			    1090.109E0,      -0.566E0,
			    1080.914E0,      -0.545E0,
			    1122.643E0,      -0.400E0,
			    1178.351E0,      -0.309E0,
			    1260.531E0,      -0.109E0,
			    1273.514E0,      -0.103E0,
			    1288.339E0,       0.010E0,
			    1327.543E0,       0.119E0,
			    1353.863E0,       0.377E0,
			    1414.509E0,       0.790E0,
			    1425.208E0,       0.963E0,
			    1421.384E0,       1.006E0,
			    1442.962E0,       1.115E0,
			    1464.350E0,       1.572E0,
			    1468.705E0,       1.841E0,
			    1447.894E0,       2.047E0,
			    1457.628E0,       2.200E0
		};
		
		class ThurberCostFunction extends SizedCostFunction {
			public ThurberCostFunction() {
				// number of residuals
				// size of first parameter
				super(37, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0);
			}

			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < ThurberObservations; i++) {
					double TSquared = ThurberData[2*i+1]*ThurberData[2*i+1];
					double TCubed = TSquared*ThurberData[2*i+1];
					double num = x[0] + x[1]*ThurberData[2*i+1] + x[2]*TSquared + x[3]*TCubed;
					double denom = 1.0 + x[4]*ThurberData[2*i+1] + x[5]*TSquared + x[6]*TCubed;
				    residuals[i] = ThurberData[2*i] - num/denom;
				    if (jacobians != null && jacobians[0] != null) {
				    	double denomSquared = denom*denom;
						jacobians[0][7*i] = -1.0/denom;
						jacobians[0][7*i+1] = -ThurberData[2*i+1]/denom;
						jacobians[0][7*i+2] = -TSquared/denom;
						jacobians[0][7*i+3] = -TCubed/denom;
						jacobians[0][7*i+4] = num*ThurberData[2*i+1]/denomSquared;
						jacobians[0][7*i+5] = num*TSquared/denomSquared;
						jacobians[0][7*i+6] = num*TCubed/denomSquared;
				    }
				}

				return true;
			
		  }
			
			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < ThurberObservations; i++) {
					double TSquared = ThurberData[2*i+1]*ThurberData[2*i+1];
					double TCubed = TSquared*ThurberData[2*i+1];
					double num = x[0] + x[1]*ThurberData[2*i+1] + x[2]*TSquared + x[3]*TCubed;
					double denom = 1.0 + x[4]*ThurberData[2*i+1] + x[5]*TSquared + x[6]*TCubed;
				    residuals[i] = ThurberData[2*i] - num/denom;
				    if (jacobians != null && jacobians[0] != null) {
				    	double denomSquared = denom*denom;
						jacobians[0][jacobians_offset[0]+7*i] = -1.0/denom;
						jacobians[0][jacobians_offset[0]+7*i+1] = -ThurberData[2*i+1]/denom;
						jacobians[0][jacobians_offset[0]+7*i+2] = -TSquared/denom;
						jacobians[0][jacobians_offset[0]+7*i+3] = -TCubed/denom;
						jacobians[0][jacobians_offset[0]+7*i+4] = num*ThurberData[2*i+1]/denomSquared;
						jacobians[0][jacobians_offset[0]+7*i+5] = num*TSquared/denomSquared;
						jacobians[0][jacobians_offset[0]+7*i+6] = num*TCubed/denomSquared;
				    }
				}
				
				return true;
			
		  }
		} // class ThurberCostFunction
		
		public void runThurberCostFunctionExample() {
			// Converges correctly for close and distant starting points
			// Ceres Solver Report: Iterations: 28, Initial cost: 4.293687e+07, Final cost: 2.821354e+03, Termination: CONVERGENCE
			// Solved answer for close starting point b1 = 1288.1396791829395 b2 = 1491.0793947094453 b3 = 583.2384672770386
			// b4 = 75.41666354796357 b5 = 0.9662951237848489 b6 = 0.39797290134914665
			// b7 = 0.049727325710259486
			// Actual answer b1 = 1.2881396800E+03  b2 = 1.4910792535E+03  b3 = 5.8323836877E+02
			// b4 = 7.5416644291E+01  b5 = 9.6629502864E-01  b6 = 3.9797285797E-01
			// b7 = 4.9727297349E-02
		
			// Ceres Solver Report: Iterations: 32, Initial cost: 2.264062e+06, Final cost: 2.821354e+03, Termination: CONVERGENCE
			// Solved answer for distant starting point b1 = 1288.139682949544 b2 = 1491.0789699357167 b3 = 583.2381603137167
			// b4 = 75.41660438249802 b5 = 0.9662947953772522 b6 = 0.39797274902159935
			// b7 = 0.04972726589555047
			// Actual answer b1 = 1.2881396800E+03  b2 = 1.4910792535E+03  b3 = 5.8323836877E+02
			// b4 = 7.5416644291E+01  b5 = 9.6629502864E-01  b6 = 3.9797285797E-01
			// b7 = 4.9727297349E-02
			
			int i;
			double x[] = new double[7];
			for (i = 0; i < 2; i++) {
	        if (i == 0) {
	        	x[0] = 1300;
	        	x[1] = 1500;
	        	x[2] = 500;
	        	x[3] = 75;
	        	x[4] = 1;
	        	x[5] = 0.4;
	        	x[6] = 0.05;
	        }
	        else {
	        	x[0] = 1000;
	        	x[1] = 1000;
	        	x[2] = 400;
	        	x[3] = 40;
	        	x[4] = 0.7;
	        	x[5] = 0.3;
	        	x[6] = 0.03;
	        }
	        
			CostFunction cost_function = new ThurberCostFunction();
			ProblemImpl problem = new ProblemImpl();
			problem.AddResidualBlock(cost_function, null, x);

			// Run the solver!
			SolverOptions solverOptions = new SolverOptions();
			solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
			solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
			solverOptions.max_num_consecutive_invalid_steps = 200;
			solverOptions.gradient_tolerance = epsilon;
			solverOptions.parameter_tolerance = epsilon;
			solverOptions.function_tolerance = 1.0E-12;
			solverOptions.min_trust_region_radius = 1.0E-50;
			if (i == 0) {
			    solverOptions.initial_trust_region_radius = 1e12;
			}
			else {
				solverOptions.initial_trust_region_radius = 1e4;
			}

			solverOptions.minimizer_progress_to_stdout = true;
			SolverSummary solverSummary = new SolverSummary();
			Solve(solverOptions, problem, solverSummary);
			System.out.println(solverSummary.BriefReport());
			if (i == 0) {
			    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
			}
			else {
				System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
			}
			System.out.println("b4 = " + x[3] + " b5 = " + x[4] + " b6 = " + x[5]);
			System.out.println("b7 = " + x[6]);
			System.out.println("Actual answer b1 = 1.2881396800E+03  b2 = 1.4910792535E+03  b3 = 5.8323836877E+02");
			System.out.println("b4 = 7.5416644291E+01  b5 = 9.6629502864E-01  b6 = 3.9797285797E-01");
			System.out.println("b7 = 4.9727297349E-02");
			} // for (i = 0; i < 2; i++)			
		}
		
		final int BoxBODObservations = 6;
		double BoxBODData[] = new double[]{
				109,             1,
			      149,             2,
			      149,             3,
			      191,             5,
			      213,             7,
			      224,            10
		};
		
		class BoxBODCostFunction extends SizedCostFunction {
			public BoxBODCostFunction() {
				// number of residuals
				// size of first parameter
				super(6, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0);
			}

			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < BoxBODObservations; i++) {
					double exp = Math.exp(-x[1]*BoxBODData[2*i+1]);
				    residuals[i] = BoxBODData[2*i] - x[0]*(1.0 - exp);
				    if (jacobians != null && jacobians[0] != null) {
						jacobians[0][2*i] = exp - 1.0;
						jacobians[0][2*i+1] = -x[0]*exp*BoxBODData[2*i+1];
				    }
				}

				return true;
			
		  }
			
			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < BoxBODObservations; i++) {
					double exp = Math.exp(-x[1]*BoxBODData[2*i+1]);
				    residuals[i] = BoxBODData[2*i] - x[0]*(1.0 - exp);
				    if (jacobians != null && jacobians[0] != null) {
						jacobians[0][jacobians_offset[0]+2*i] = exp - 1.0;
						jacobians[0][jacobians_offset[0]+2*i+1] = -x[0]*exp*BoxBODData[2*i+1];
				    }
				}
				
				return true;
			
		  }
		} // class BoxBODCostFunction
		
		public void runBoxBODCostFunctionExample() {
			// Close starting point converges to correct answer but distant starting point does not
			// Ceres Solver Report: Iterations: 15, Initial cost: 2.439263e+04, Final cost: 5.840044e+02, Termination: CONVERGENCE
			// Solved answer for close starting point b1 = 213.80937962004887 b2 = 0.5472377008108861
			// Actual answer b1 = 2.1380940889E+02  b2 = 5.4723748542E-01
			
			// Ceres Solver Report: Iterations: 36, Initial cost: 9.319119e+04, Final cost: 4.887770e+03, Termination: CONVERGENCE
			// Solved answer for distant starting point b1 = 173.32052313996235 b2 = 4767.713101821059
			// Actual answer b1 = 2.1380940889E+02  b2 = 5.4723748542E-01
			
			int i;
			double x[] = new double[2];
			for (i = 0; i < 2; i++) {
	        if (i == 0) {
	        	x[0] = 100;
	        	x[1] = 0.75;
	        }
	        else {
	        	x[0] = 1;
	        	x[1] = 1;
	        }
	        
			CostFunction cost_function = new BoxBODCostFunction();
			ProblemImpl problem = new ProblemImpl();
			problem.AddResidualBlock(cost_function, null, x);

			// Run the solver!
			SolverOptions solverOptions = new SolverOptions();
			solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
			solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
			solverOptions.max_num_consecutive_invalid_steps = 200;
			solverOptions.gradient_tolerance = epsilon;
			solverOptions.parameter_tolerance = epsilon;
			solverOptions.function_tolerance = 1.0E-12;
			solverOptions.min_trust_region_radius = 1.0E-50;

			solverOptions.minimizer_progress_to_stdout = true;
			SolverSummary solverSummary = new SolverSummary();
			Solve(solverOptions, problem, solverSummary);
			System.out.println(solverSummary.BriefReport());
			if (i == 0) {
			    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1]);
			}
			else {
				System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1]);
			}
			System.out.println("Actual answer b1 = 2.1380940889E+02  b2 = 5.4723748542E-01");
			} // for (i = 0; i < 2; i++)			
		}
		
		final int Rat42Observations = 9;
		double Rat42Data[] = new double[]{
				 8.930E0,        9.000E0,
			      10.800E0,       14.000E0,
			      18.590E0,       21.000E0,
			      22.330E0,       28.000E0,
			      39.350E0,       42.000E0,
			      56.110E0,       57.000E0,
			      61.730E0,       63.000E0,
			      64.620E0,       70.000E0,
			      67.080E0,       79.000E0
		};
		
		class Rat42CostFunction extends SizedCostFunction {
			public Rat42CostFunction() {
				// number of residuals
				// size of first parameter
				super(9, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0);
			}

			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < Rat42Observations; i++) {
					double exp = Math.exp(x[1]-x[2]*Rat42Data[2*i+1]);
					double denom = 1.0 + exp;
				    residuals[i] = Rat42Data[2*i] - x[0]/denom;
				    if (jacobians != null && jacobians[0] != null) {
						jacobians[0][3*i] = -1.0/(1.0 + exp);
						jacobians[0][3*i+1] = x[0]*exp/(denom*denom);
						jacobians[0][3*i+2] = -x[0]*exp*Rat42Data[2*i+1]/(denom*denom);
				    }
				}

				return true;
			
		  }
			
			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < Rat42Observations; i++) {
					double exp = Math.exp(x[1]-x[2]*Rat42Data[2*i+1]);
					double denom = 1.0 + exp;
				    residuals[i] = Rat42Data[2*i] - x[0]/denom;
				    if (jacobians != null && jacobians[0] != null) {
						jacobians[0][jacobians_offset[0]+3*i] = -1.0/(1.0 + exp);
						jacobians[0][jacobians_offset[0]+3*i+1] = x[0]*exp/(denom*denom);
						jacobians[0][jacobians_offset[0]+3*i+2] = -x[0]*exp*Rat42Data[2*i+1]/(denom*denom);
				    }
				}
				
				return true;
			
		  }
		} // class Rat42CostFunction
		
		public void runRat42CostFunctionExample() {
			// Correct answers for close and distant starting points
			// Ceres Solver Report: Iterations: 18, Initial cost: 7.638101e+01, Final cost: 4.028261e+00, Termination: CONVERGENCE
			// Solved answer for close starting point b1 = 72.46224038410811 b2 = 2.618076685249202 b3 = 0.06735919327795407
			// Actual answer b1 = 7.2462237576E+01 b2 = 2.6180768402E+00 b3 = 6.7359200066E-02
	
			// Ceres Solver Report: Iterations: 25, Initial cost: 9.957926e+03, Final cost: 4.028261e+00, Termination: CONVERGENCE
			// Solved answer for distant starting point b1 = 72.46224028332767 b2 = 2.618076713458897b3 = 0.06735919398686918
			// Actual answer b1 = 7.2462237576E+01  b2 = 2.6180768402E+00 b3 = 6.7359200066E-02
			int i;
			double x[] = new double[3];
			for (i = 0; i < 2; i++) {
	        if (i == 0) {
	        	x[0] = 75;
	        	x[1] = 2.5;
	        	x[2] = 0.07;
	        }
	        else {
	        	x[0] = 100;
	        	x[1] = 1;
	        	x[2] = 0.1;
	        }
	        
			CostFunction cost_function = new Rat42CostFunction();
			ProblemImpl problem = new ProblemImpl();
			problem.AddResidualBlock(cost_function, null, x);

			// Run the solver!
			SolverOptions solverOptions = new SolverOptions();
			solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
			solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
			solverOptions.max_num_consecutive_invalid_steps = 200;
			solverOptions.gradient_tolerance = epsilon;
			solverOptions.parameter_tolerance = epsilon;
			solverOptions.function_tolerance = 1.0E-12;
			solverOptions.min_trust_region_radius = 1.0E-50;

			solverOptions.minimizer_progress_to_stdout = true;
			SolverSummary solverSummary = new SolverSummary();
			Solve(solverOptions, problem, solverSummary);
			System.out.println(solverSummary.BriefReport());
			if (i == 0) {
			    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
			}
			else {
				System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + "b3 = " + x[2]);
			}
			System.out.println("Actual answer b1 = 7.2462237576E+01  b2 = 2.6180768402E+00 b3 = 6.7359200066E-02");
			} // for (i = 0; i < 2; i++)			
		}
		
		final int Rat43Observations = 15;
		double Rat43Data[] = new double[]{
		16.08E0,     1.0E0,
	      33.83E0,     2.0E0,
	      65.80E0,     3.0E0,
	      97.20E0,     4.0E0,
	     191.55E0,     5.0E0,
	     326.20E0,     6.0E0,
	     386.87E0,     7.0E0,
	     520.53E0,     8.0E0,
	     590.03E0,     9.0E0,
	     651.92E0,    10.0E0,
	     724.93E0,    11.0E0,
	     699.56E0,    12.0E0,
	     689.96E0,    13.0E0,
	     637.56E0,    14.0E0,
	     717.41E0,    15.0E0
		};
		
		class Rat43CostFunction extends SizedCostFunction {
			public Rat43CostFunction() {
				// number of residuals
				// size of first parameter
				super(15, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0);
			}

			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < Rat43Observations; i++) {
					double exp = Math.exp(x[1]-x[2]*Rat43Data[2*i+1]);
					double denom = Math.pow(1.0 + exp,1.0/x[3]);
				    residuals[i] = Rat43Data[2*i] - x[0]/denom;
				    if (jacobians != null && jacobians[0] != null) {
						jacobians[0][4*i] = -1.0/denom;
						jacobians[0][4*i+1] = x[0]*Math.pow(1.0 + exp, -1.0/x[3] - 1.0)*exp/x[3];
						jacobians[0][4*i+2] = -x[0]*Math.pow(1.0 + exp, -1.0/x[3] - 1.0)*exp*Rat43Data[2*i+1]/x[3];
						jacobians[0][4*i+3] = -x[0]*Math.log(1.0 + exp)/(Math.pow(1.0 + exp, 1.0/x[3])*x[3]*x[3]);
				    }
				}

				return true;
			
		  }
			
			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < Rat43Observations; i++) {
					double exp = Math.exp(x[1]-x[2]*Rat43Data[2*i+1]);
					double denom = Math.pow(1.0 + exp,1.0/x[3]);
				    residuals[i] = Rat43Data[2*i] - x[0]/denom;
				    if (jacobians != null && jacobians[0] != null) {
						jacobians[0][jacobians_offset[0]+4*i] = -1.0/denom;
						jacobians[0][jacobians_offset[0]+4*i+1] = x[0]*Math.pow(1.0 + exp, -1.0/x[3] - 1.0)*exp/x[3];
						jacobians[0][jacobians_offset[0]+4*i+2] = -x[0]*Math.pow(1.0 + exp, -1.0/x[3] - 1.0)*exp*Rat43Data[2*i+1]/x[3];
						jacobians[0][jacobians_offset[0]+4*i+3] = -x[0]*Math.log(1.0 + exp)/(Math.pow(1.0 + exp, 1.0/x[3])*x[3]*x[3]);
				    }
				}
				
				return true;
			
		  }
		} // class Rat43CostFunction
		
		public void runRat43CostFunctionExample() {
			// The close starting point converges correctly but the distant starting point fails because the exponential reaches infinity
			// Ceres Solver Report: Iterations: 21, Initial cost: 7.327607e+03, Final cost: 4.393202e+03, Termination: CONVERGENCE
			// Solved answer for close starting point b1 = 699.6414928325237 b2 = 5.277131126064428 b3 = 0.7596298998436753
			// b4 = 1.2792503465001268
			// Actual answer b1 = 6.9964151270E+02  b2 =  5.2771253025E+00 b3 = 7.5962938329E-01
			// b4 = 1.2792483859E+00
			
			// Terminating: Residual and Jacobian evaluation failed.
			// Ceres Solver Report: Iterations: 3, Initial cost: 1.533154e+06, Final cost: 5.881820e+05, Termination: FAILURE
			// Solved answer for distant starting point b1 = 100.0 b2 = 10.0b3 = 1.0
			// b4 = 1.0
			// Actual answer b1 = 6.9964151270E+02  b2 =  5.2771253025E+00 b3 = 7.5962938329E-01
			// b4 = 1.2792483859E+00
			int i;
			double x[] = new double[4];
			for (i = 0; i < 2; i++) {
	        if (i == 0) {
	        	x[0] = 700;
	        	x[1] = 5;
	        	x[2] = 0.75;
	        	x[3] = 1.3;
	        }
	        else {
	        	x[0] = 100;
	        	x[1] = 10;
	        	x[2] = 1;
	        	x[3] = 1;
	        }
	        
			CostFunction cost_function = new Rat43CostFunction();
			ProblemImpl problem = new ProblemImpl();
			problem.AddResidualBlock(cost_function, null, x);

			// Run the solver!
			SolverOptions solverOptions = new SolverOptions();
			solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
			solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
			solverOptions.max_num_consecutive_invalid_steps = 200;
			solverOptions.gradient_tolerance = epsilon;
			solverOptions.parameter_tolerance = epsilon;
			solverOptions.function_tolerance = 1.0E-12;
			solverOptions.min_trust_region_radius = 1.0E-50;

			solverOptions.minimizer_progress_to_stdout = true;
			SolverSummary solverSummary = new SolverSummary();
			Solve(solverOptions, problem, solverSummary);
			System.out.println(solverSummary.BriefReport());
			if (i == 0) {
			    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
			}
			else {
				System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + "b3 = " + x[2]);
			}
			System.out.println("b4 = " + x[3]);
			System.out.println("Actual answer b1 = 6.9964151270E+02  b2 =  5.2771253025E+00 b3 = 7.5962938329E-01");
			System.out.println("b4 = 1.2792483859E+00");
			} // for (i = 0; i < 2; i++)			
		}
		
		final int Eckerle4Observations = 35;
		double Eckerle4Data[] = new double[]{
				0.0001575E0,    400.000000E0,
			      0.0001699E0,    405.000000E0,
			      0.0002350E0,    410.000000E0,
			      0.0003102E0,    415.000000E0,
			      0.0004917E0,    420.000000E0,
			      0.0008710E0,    425.000000E0,
			      0.0017418E0,    430.000000E0,
			      0.0046400E0,    435.000000E0,
			      0.0065895E0,    436.500000E0,
			      0.0097302E0,    438.000000E0,
			      0.0149002E0,    439.500000E0,
			      0.0237310E0,    441.000000E0,
			      0.0401683E0,    442.500000E0,
			      0.0712559E0,    444.000000E0,
			      0.1264458E0,    445.500000E0,
			      0.2073413E0,    447.000000E0,
			      0.2902366E0,    448.500000E0,
			      0.3445623E0,    450.000000E0,
			      0.3698049E0,    451.500000E0,
			      0.3668534E0,    453.000000E0,
			      0.3106727E0,    454.500000E0,
			      0.2078154E0,    456.000000E0,
			      0.1164354E0,    457.500000E0,
			      0.0616764E0,    459.000000E0,
			      0.0337200E0,    460.500000E0,
			      0.0194023E0,    462.000000E0,
			      0.0117831E0,    463.500000E0,
			      0.0074357E0,    465.000000E0,
			      0.0022732E0,    470.000000E0,
			      0.0008800E0,    475.000000E0,
			      0.0004579E0,    480.000000E0,
			      0.0002345E0,    485.000000E0,
			      0.0001586E0,    490.000000E0,
			      0.0001143E0,    495.000000E0,
			      0.0000710E0,    500.000000E0
		};
		
		class Eckerle4CostFunction extends SizedCostFunction {
			public Eckerle4CostFunction() {
				// number of residuals
				// size of first parameter
				super(35, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0);
			}

			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < Eckerle4Observations; i++) {
					double var = (Eckerle4Data[2*i+1] - x[2])/x[1];
					double exp = Math.exp(-0.5*var*var);
				    residuals[i] = Eckerle4Data[2*i] - (x[0]/x[1]) * exp;
				    if (jacobians != null && jacobians[0] != null) {
				    	double x1Squared = x[1]*x[1];
						jacobians[0][3*i] = -exp/x[1];
						jacobians[0][3*i+1] = x[0]*(x[1]*x[1] - x[2]*x[2] + 2.0*Eckerle4Data[2*i+1]*x[2]
								- Eckerle4Data[2*i+1]*Eckerle4Data[2*i+1])*exp/(x1Squared * x1Squared);
						jacobians[0][3*i+2] = x[0]*(x[2] - Eckerle4Data[2*i+1])*exp/x1Squared;
				    }
				}

				return true;
			
		  }
			
			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < Eckerle4Observations; i++) {
					double var = (Eckerle4Data[2*i+1] - x[2])/x[1];
					double exp = Math.exp(-0.5*var*var);
				    residuals[i] = Eckerle4Data[2*i] - (x[0]/x[1]) * exp;
				    if (jacobians != null && jacobians[0] != null) {
				    	double x1Squared = x[1]*x[1];
						jacobians[0][jacobians_offset[0]+3*i] = -exp/x[1];
						jacobians[0][jacobians_offset[0]+3*i+1] = x[0]*(x[1]*x[1] - x[2]*x[2] + 2.0*Eckerle4Data[2*i+1]*x[2]
								- Eckerle4Data[2*i+1]*Eckerle4Data[2*i+1])*exp/(x1Squared * x1Squared);
						jacobians[0][jacobians_offset[0]+3*i+2] = x[0]*(x[2] - Eckerle4Data[2*i+1])*exp/x1Squared;
				    }
				}
				
				return true;
			
		  }
		} // class Eckerle4CostFunction
		
		public void runEckerle4CostFunctionExample() {
			// Close starting points converges to correct answer but distant starting point fails
			
			// Ceres Solver Report: Iterations: 54, Initial cost: 2.834145e-02, Final cost: 7.318029e-04, Termination: CONVERGENCE
			// Solved answer for close starting point b1 = 1.5544040007788293 b2 = 4.088994316140087 b3 = 451.5403232095928
			// Actual answer b1 = 1.5543827178E+00  b2 = 4.0888321754E+00 b3 = 4.5154121844E+02
		
			// Ceres Solver Report: Iterations: 36, Initial cost: 3.611513e-01, Final cost: 1.626664e-01, Termination: CONVERGENCE
			// Solved answer for distant starting point b1 = 2.4183817920383435 b2 = 15.204733398279517b3 = 457.92753900649905
			// Actual answer b1 = 1.5543827178E+00  b2 = 4.0888321754E+00 b3 = 4.5154121844E+02

			int i;
			double x[] = new double[3];
			for (i = 0; i < 2; i++) {
	        if (i == 0) {
	        	x[0] = 1.5;
	        	x[1] = 5;
	        	x[2] = 450;
	        }
	        else {
	        	x[0] = 1;
	        	x[1] = 10;
	        	x[2] = 500;
	        }
	        
			CostFunction cost_function = new Eckerle4CostFunction();
			ProblemImpl problem = new ProblemImpl();
			problem.AddResidualBlock(cost_function, null, x);

			// Run the solver!
			SolverOptions solverOptions = new SolverOptions();
			solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
			solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
			solverOptions.max_num_consecutive_invalid_steps = 200;
			solverOptions.gradient_tolerance = epsilon;
			solverOptions.parameter_tolerance = epsilon;
			solverOptions.function_tolerance = 1.0E-12;
			solverOptions.min_trust_region_radius = 1.0E-50;

			solverOptions.minimizer_progress_to_stdout = true;
			SolverSummary solverSummary = new SolverSummary();
			Solve(solverOptions, problem, solverSummary);
			System.out.println(solverSummary.BriefReport());
			if (i == 0) {
			    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
			}
			else {
				System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
			}
			System.out.println("Actual answer b1 = 1.5543827178E+00  b2 = 4.0888321754E+00 b3 = 4.5154121844E+02");
			} // for (i = 0; i < 2; i++)			
		}
		
		final int Bennett5Observations = 154;
		double Bennett5Data[] = new double[]{
				-34.834702E0,      7.447168E0,
			     -34.393200E0,      8.102586E0,
			     -34.152901E0,      8.452547E0,
			     -33.979099E0,      8.711278E0,
			     -33.845901E0,      8.916774E0,
			     -33.732899E0,      9.087155E0,
			     -33.640301E0,      9.232590E0,
			     -33.559200E0,      9.359535E0,
			     -33.486801E0,      9.472166E0,
			     -33.423100E0,      9.573384E0,
			     -33.365101E0,      9.665293E0,
			     -33.313000E0,      9.749461E0,
			     -33.260899E0,      9.827092E0,
			     -33.217400E0,      9.899128E0,
			     -33.176899E0,      9.966321E0,
			     -33.139198E0,     10.029280E0,
			     -33.101601E0,     10.088510E0,
			     -33.066799E0,     10.144430E0,
			     -33.035000E0,     10.197380E0,
			     -33.003101E0,     10.247670E0,
			     -32.971298E0,     10.295560E0,
			     -32.942299E0,     10.341250E0,
			     -32.916302E0,     10.384950E0,
			     -32.890202E0,     10.426820E0,
			     -32.864101E0,     10.467000E0,
			     -32.841000E0,     10.505640E0,
			     -32.817799E0,     10.542830E0,
			     -32.797501E0,     10.578690E0,
			     -32.774300E0,     10.613310E0,
			     -32.757000E0,     10.646780E0,
			     -32.733799E0,     10.679150E0,
			     -32.716400E0,     10.710520E0,
			     -32.699100E0,     10.740920E0,
			     -32.678799E0,     10.770440E0,
			     -32.661400E0,     10.799100E0,
			     -32.644001E0,     10.826970E0,
			     -32.626701E0,     10.854080E0,
			     -32.612202E0,     10.880470E0,
			     -32.597698E0,     10.906190E0,
			     -32.583199E0,     10.931260E0,
			     -32.568699E0,     10.955720E0,
			     -32.554298E0,     10.979590E0,
			     -32.539799E0,     11.002910E0,
			     -32.525299E0,     11.025700E0,
			     -32.510799E0,     11.047980E0,
			     -32.499199E0,     11.069770E0,
			     -32.487598E0,     11.091100E0,
			     -32.473202E0,     11.111980E0,
			     -32.461601E0,     11.132440E0,
			     -32.435501E0,     11.152480E0,
			     -32.435501E0,     11.172130E0,
			     -32.426800E0,     11.191410E0,
			     -32.412300E0,     11.210310E0,
			     -32.400799E0,     11.228870E0,
			     -32.392101E0,     11.247090E0,
			     -32.380501E0,     11.264980E0,
			     -32.366001E0,     11.282560E0,
			     -32.357300E0,     11.299840E0,
			     -32.348598E0,     11.316820E0,
			     -32.339901E0,     11.333520E0,
			     -32.328400E0,     11.349940E0,
			     -32.319698E0,     11.366100E0,
			     -32.311001E0,     11.382000E0,
			     -32.299400E0,     11.397660E0,
			     -32.290699E0,     11.413070E0,
			     -32.282001E0,     11.428240E0,
			     -32.273300E0,     11.443200E0,
			     -32.264599E0,     11.457930E0,
			     -32.256001E0,     11.472440E0,
			     -32.247299E0,     11.486750E0,
			     -32.238602E0,     11.500860E0,
			     -32.229900E0,     11.514770E0,
			     -32.224098E0,     11.528490E0,
			     -32.215401E0,     11.542020E0,
			     -32.203800E0,     11.555380E0,
			     -32.198002E0,     11.568550E0,
			     -32.189400E0,     11.581560E0,
			     -32.183601E0,     11.594420E0,
			     -32.174900E0,     11.607121E0,
			     -32.169102E0,     11.619640E0,
			     -32.163300E0,     11.632000E0,
			     -32.154598E0,     11.644210E0,
			     -32.145901E0,     11.656280E0,
			     -32.140099E0,     11.668200E0,
			     -32.131401E0,     11.679980E0,
			     -32.125599E0,     11.691620E0,
			     -32.119801E0,     11.703130E0,
			     -32.111198E0,     11.714510E0,
			     -32.105400E0,     11.725760E0,
			     -32.096699E0,     11.736880E0,
			     -32.090900E0,     11.747890E0,
			     -32.088001E0,     11.758780E0,
			     -32.079300E0,     11.769550E0,
			     -32.073502E0,     11.780200E0,
			     -32.067699E0,     11.790730E0,
			     -32.061901E0,     11.801160E0,
			     -32.056099E0,     11.811480E0,
			     -32.050301E0,     11.821700E0,
			     -32.044498E0,     11.831810E0,
			     -32.038799E0,     11.841820E0,
			     -32.033001E0,     11.851730E0,
			     -32.027199E0,     11.861550E0,
			     -32.024300E0,     11.871270E0,
			     -32.018501E0,     11.880890E0,
			     -32.012699E0,     11.890420E0,
			     -32.004002E0,     11.899870E0,
			     -32.001099E0,     11.909220E0,
			     -31.995300E0,     11.918490E0,
			     -31.989500E0,     11.927680E0,
			     -31.983700E0,     11.936780E0,
			     -31.977900E0,     11.945790E0,
			     -31.972099E0,     11.954730E0,
			     -31.969299E0,     11.963590E0,
			     -31.963501E0,     11.972370E0,
			     -31.957701E0,     11.981070E0,
			     -31.951900E0,     11.989700E0,
			     -31.946100E0,     11.998260E0,
			     -31.940300E0,     12.006740E0,
			     -31.937401E0,     12.015150E0,
			     -31.931601E0,     12.023490E0,
			     -31.925800E0,     12.031760E0,
			     -31.922899E0,     12.039970E0,
			     -31.917101E0,     12.048100E0,
			     -31.911301E0,     12.056170E0,
			     -31.908400E0,     12.064180E0,
			     -31.902599E0,     12.072120E0,
			     -31.896900E0,     12.080010E0,
			     -31.893999E0,     12.087820E0,
			     -31.888201E0,     12.095580E0,
			     -31.885300E0,     12.103280E0,
			     -31.882401E0,     12.110920E0,
			     -31.876600E0,     12.118500E0,
			     -31.873699E0,     12.126030E0,
			     -31.867901E0,     12.133500E0,
			     -31.862101E0,     12.140910E0,
			     -31.859200E0,     12.148270E0,
			     -31.856300E0,     12.155570E0,
			     -31.850500E0,     12.162830E0,
			     -31.844700E0,     12.170030E0,
			     -31.841801E0,     12.177170E0,
			     -31.838900E0,     12.184270E0,
			     -31.833099E0,     12.191320E0,
			     -31.830200E0,     12.198320E0,
			     -31.827299E0,     12.205270E0,
			     -31.821600E0,     12.212170E0,
			     -31.818701E0,     12.219030E0,
			     -31.812901E0,     12.225840E0,
			     -31.809999E0,     12.232600E0,
			     -31.807100E0,     12.239320E0,
			     -31.801300E0,     12.245990E0,
			     -31.798401E0,     12.252620E0,
			     -31.795500E0,     12.259200E0,
			     -31.789700E0,     12.265750E0,
			     -31.786800E0,     12.272240E0
		};
		
		class Bennett5CostFunction extends SizedCostFunction {
			public Bennett5CostFunction() {
				// number of residuals
				// size of first parameter
				super(154, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0);
			}

			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < Bennett5Observations; i++) {
				    residuals[i] = Bennett5Data[2*i] - x[0]*Math.pow(x[1] + Bennett5Data[2*i+1],-1.0/x[2]);
				    if (jacobians != null && jacobians[0] != null) {
						jacobians[0][3*i] = -Math.pow(x[1] + Bennett5Data[2*i+1],-1.0/x[2]);
						jacobians[0][3*i+1] = x[0]*Math.pow(x[1] + Bennett5Data[2*i+1], -1.0/x[2] - 1.0)/x[2];
						jacobians[0][3*i+2] = -x[0]*Math.log(x[1] + Bennett5Data[2*i+1])/(Math.pow(x[1] + Bennett5Data[2*i+1],1.0/x[2])*x[2]*x[2]);
				    }
				}

				return true;
			
		  }
			
			public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
				int i;
				// Called by ResidualBlock.Evaluate
				double x[] = parameters.get(0);
				
				for (i = 0; i < Bennett5Observations; i++) {
				    residuals[i] = Bennett5Data[2*i] - x[0]*Math.pow(x[1] + Bennett5Data[2*i+1],-1.0/x[2]);
				    if (jacobians != null && jacobians[0] != null) {
						jacobians[0][jacobians_offset[0]+3*i] = -Math.pow(x[1] + Bennett5Data[2*i+1],-1.0/x[2]);
						jacobians[0][jacobians_offset[0]+3*i+1] = x[0]*Math.pow(x[1] + Bennett5Data[2*i+1], -1.0/x[2] - 1.0)/x[2];
						jacobians[0][jacobians_offset[0]+3*i+2] = -x[0]*Math.log(x[1] + Bennett5Data[2*i+1])/(Math.pow(x[1] +
								Bennett5Data[2*i+1],1.0/x[2])*x[2]*x[2]);
				    }
				}
				
				return true;
			
		  }
		} // class Bennett5CostFunction
		
		public void runBennett5CostFunctionExample() {
			// Neither starting point converges to the correct answer
			// Ceres Solver Report: Iterations: 61, Initial cost: 2.863055e+04, Final cost: 2.996963e-03, Termination: CONVERGENCE
			// Solved answer for close starting point b1 = -2305.955839229158 b2 = 45.31675341090308 b3 = 0.9460747487752403
			// Actual answer b1 = -2.5235058043E+03  b2 = 4.6736564644E+01 b3 = 9.3218483193E-01
		
			// Ceres Solver Report: Iterations: 66, Initial cost: 3.301122e+04, Final cost: 2.266918e-03, Termination: CONVERGENCE
			// Solved answer for distant starting point b1 = -3183.4680455028883 b2 = 48.75891005264904b3 = 0.8924540859923037
			// Actual answer b1 = -2.5235058043E+03  b2 = 4.6736564644E+01 b3 = 9.3218483193E-01

			int i;
			double x[] = new double[3];
			for (i = 0; i < 2; i++) {
	        if (i == 0) {
	        	x[0] = -1500;
	        	x[1] = 45;
	        	x[2] = 0.85;
	        }
	        else {
	        	x[0] = -2000;
	        	x[1] = 50;
	        	x[2] = 0.8;
	        }
	        
			CostFunction cost_function = new Bennett5CostFunction();
			ProblemImpl problem = new ProblemImpl();
			problem.AddResidualBlock(cost_function, null, x);

			// Run the solver!
			SolverOptions solverOptions = new SolverOptions();
			solverOptions.minimizer_type = MinimizerType.TRUST_REGION;
			solverOptions.trust_region_strategy_type = TrustRegionStrategyType.LEVENBERG_MARQUARDT;
			solverOptions.max_num_consecutive_invalid_steps = 200;
			solverOptions.gradient_tolerance = epsilon;
			solverOptions.parameter_tolerance = epsilon;
			solverOptions.function_tolerance = 1.0E-12;
			solverOptions.min_trust_region_radius = 1.0E-100;

			solverOptions.minimizer_progress_to_stdout = true;
			SolverSummary solverSummary = new SolverSummary();
			Solve(solverOptions, problem, solverSummary);
			System.out.println(solverSummary.BriefReport());
			if (i == 0) {
			    System.out.println("Solved answer for close starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
			}
			else {
				System.out.println("Solved answer for distant starting point b1 = " + x[0] + " b2 = " + x[1] + " b3 = " + x[2]);
			}
			System.out.println("Actual answer b1 = -2.5235058043E+03  b2 = 4.6736564644E+01 b3 = 9.3218483193E-01");
			System.out.println("Correct answer has Final cost: 2.620237e-04");
			} // for (i = 0; i < 2; i++)			
		}
}