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
		// Both near and far starting points converge to incorrect answers
		// Ceres Solver Report: Iterations: 53, Initial cost: 3.939461e+01, Final cost: 2.924861e-06, Termination: CONVERGENCE
		// Solved answer for close starting point b1 = 0.07395951695875314 b2 = 0.7029289338395481 b3 = 1.5047932073070354
		// b4 = 3.4330341994293527 b5 = 0.9355133905699761 b6 = 5.659476614670378
		// Actual answer b1 =  8.6816414977E-02  b2 = 9.5498101505E-01  b3 = 8.4400777463E-01
		// b4 = 2.9515951832E+00  b5 = 1.5825685901E+00    b6 = 4.9863565084E+00
		// Ceres Solver Report: Iterations: 39, Initial cost: 1.348757e+02, Final cost: 2.189192e-07, Termination: CONVERGENCE
		// Solved answer for distant starting point b1 = 0.3156011255976063 b2 = 1.6410130068543172 b3 = 1.948137899383847
		// b4 = 4.287347747370616 b5 = 0.24975840812820052 b6 = 6.392080193393909
		// Actual answer b1 =  8.6816414977E-02  b2 = 9.5498101505E-01  b3 = 8.4400777463E-01
		// b4 = 2.9515951832E+00  b5 = 1.5825685901E+00    b6 = 4.9863565084E+00

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
		System.out.println("Actual answer b1 =  8.6816414977E-02  b2 = 9.5498101505E-01  b3 = 8.4400777463E-01");
		System.out.println("b4 = 2.9515951832E+00  b5 = 1.5825685901E+00  b6 = 4.9863565084E+00");
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

}