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

}