package gov.nih.mipav.model.algorithms;

import java.util.Vector;

import gov.nih.mipav.model.algorithms.CeresSolver.CostFunction;
import gov.nih.mipav.model.algorithms.CeresSolver.MinimizerType;
import gov.nih.mipav.model.algorithms.CeresSolver.ProblemImpl;
import gov.nih.mipav.model.algorithms.CeresSolver.SizedCostFunction;
import gov.nih.mipav.model.algorithms.CeresSolver.SolverOptions;
import gov.nih.mipav.model.algorithms.CeresSolver.SolverSummary;
import gov.nih.mipav.model.algorithms.CeresSolver.TrustRegionStrategyType;
import gov.nih.mipav.model.algorithms.CeresSolverTest.CurveFittingCostFunction;

public class CeresSolverNISTTest extends CeresSolver {
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
		// Correct answer for near starting point but not for distant starting point
		// Ceres Solver Report: Iterations: 11, Initial cost: 2.238564e+01, Final cost: 6.227579e-02, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 238.95359260828437 b2 = 5.501256500210992E-4
		// Actual answer b1 = 2.3894212918E+02 b2 = 5.5015643181E-04
		// Ceres Solver Report: Iterations: 29, Initial cost: 5.390095e+03, Final cost: 2.329783e+01, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 1459.50369618817 b2 = 7.90953729676105E-5
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
}