package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.CeresSolver.FirstOrderFunction;
import gov.nih.mipav.model.algorithms.CeresSolver.GradientProblem;
import gov.nih.mipav.model.algorithms.CeresSolver.GradientProblemSolverOptions;
import gov.nih.mipav.model.algorithms.CeresSolver.GradientProblemSolverSummary;
import gov.nih.mipav.model.algorithms.CeresSolver.ProblemImpl;
import gov.nih.mipav.model.algorithms.CeresSolver.Solver;
import gov.nih.mipav.model.algorithms.CeresSolverTest.Rosenbrock;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.ViewUserInterface;

import java.io.*;

import java.util.*;

/*
MIT License

Copyright (c) 2018 marcocastellaro

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/


public class DSC_MRI_toolbox extends CeresSolver {
	private ViewUserInterface UI;
	// 4D matrix with raw GRE-DSC acquisition
	private double volumes[][][][];
	private int nR;
	private int nC;
	private int nS;
	private int nT;
	private double time[];
	// echo time in seconds
	private double te = 0.025;
	// repetition time in seconds
	private double tr = 1.55;
	
	// DISPLAY OPTIONS
	private int display = 2; // 0:off, 1:notify (text), 2:notify (images), 3:debug
	private int waitbar = 1; // 0:off, 1:on
	
	// DATA PREPARATION OPTIONS
	
	// Represents the number of minimum pixels of a connected component that is used
	// as a threshold to exclude the scalp and adjacent areas outside the brain
	// from the image
	private int mask_npixel = 300;
	// 0: The data provided is a signal, 1: The data provided are concentrations
	private int conc = 0;
	
	// Minimum number of initial scans on which to calculate S0
	private int S0_nSamplesMin = 3;
	// Maximum number of initial scans on whcih to caclulate s0
	private int S0_nSamplesMax = 3;
	
	// Threshold used to choose the instant of appearance of the tracer
	private double S0_thresh = 0.05;
	
	// OPTIONS FOR THE IDENTIFICATION PHASE OF THE AIF
	// 0: It does not calculate the AIF, 1: It does calculate the AIF
	private int aif_enable = 1;
	
	// 0: It does not take recirculation into account, 1: Fits the recirculation
	private int aif_recirculation = 1;
	
	// Slice on which to search for the AIF (-1 : Makes the operator select the slice)
	private int aif_nSlice = -1;
	
	// Dimension of the semimajor axis for the search area
	private double aif_semiMajorAixs = 0.35;
	
	// Dimension of the semiminor axis for the search area
	private double aif_semiMinorAxis = 0.15;
	
	// Fraction of voxels discarded due to area under curve
	private double aif_pArea = 0.4;
	
	// Fraction of voxels discarded due to TTP
	private double aif_pTTP = 0.4;
	
	// Fraction of voxels discarded due to the regularity of the trend
	private double aif_pReg = 0.05;
	
	// Threshold to decide whether to select cluster based on peak or TTP
	private double aif_diffPeak = 0.04;
	
	// Maximum voxels chosen for AIF
	private int aif_nVoxelMax = 6;
	
	// Minimum voxels chosen for AIF
	private int aif_nVoxelMin = 4;
	
	// Correction of the formula for calculating the concentration from the 
	// signal in the case of calculating the AIF
	// 0: Does not apply the correction, 1: Applies the correction
	private int qr_enable = 0;
	private double qr_b = 5.74E-4;
	private double qr_a = 0.0076;
	private double qr_r = 0.044;
	
	// OPTIONS FOR DECONVOLUTION METHODS
	// SVD threshold
	private double deconv_SVD_threshold = 0.2;
	// 0: Does not save residuals, 1: Save residuals
	private int deconv_SVD_residual = 1;
	
	// CSVD threshold (0.1 applies to data obtained at 1.5T)
	private double deconv_cSVD_threshold = 0.1;
	// 0: Does not save residuals, 1: Saves residuals
	private int deconv_csVD_residual = 1;
	
	// Threshold of 10% with Ostergaard and Calamante
	private double deconv_oSVD_OIthres = 0.035;
	private int deconv_oSVD_OIcounter = 1;
	// 0: Does not save residuals, 1: Saves residuals
	private int deconv_oSVD_residual = 1;
	
	// TO ADD PARAMETERS FOR STABLE SPLINE
	private int deconv_SS_residual = 1;
	
	// Methods to be applied for perfusion calculation
	// "SVD", "cSVD", "oSVD"
	private String deconv_method = "cSVD";
	
	// Constants ofproportionality
	private double par_kh = 1.0;
	private double par_rho = 1.0;
	private double par_kvoi = 1.0;
	
	// Mask optimized for masking the entire brain
	private byte mask_data[][][];
	// Mask optimized for finding the arterial input function
	private byte mask_aif[][][];
	
	private int gauss2FittingObservations;
    private double gauss2FittingData[];
	
	public DSC_MRI_toolbox(double volumes[][][][], double te, double tr) {
		this.volumes = volumes;
		this.te = te;
		this.tr = tr;
	}
	
	public void runAlgorithm() {
		DSC_mri_core();
	}
	
	public void DSC_mri_core() {
		int i;
		UI = ViewUserInterface.getReference();
		if (display > 0) {
			UI.setDataText("Checking data...\n");
		}
		nR = volumes.length;
		nC = volumes[0].length;
		nS = volumes[0][0].length;
		nT = volumes[0][0][0].length;
		
		time = new double[nT];
		for (i = 0; i < nT; i++) {
			time[i] = i*tr;
		}
		
		if (display > 0) {
			UI.setDataText("DATA SIZE\n");
			UI.setDataText("Rows = " + nR + "\n");
			UI.setDataText("Columns = " + nC + "\n");
			UI.setDataText("Slices = " + nS + "\n");
			UI.setDataText("Samples = " + nT + "\n");
			UI.setDataText("Echo time = " + te + "\n");
			UI.setDataText("Repetition time = " + tr + "\n");
		}
	}
	
	public void DSC_mri_mask() {
		int i, x, y, z, t;
	    if (display > 0) {
	    	UI.setDataText("Masking data...");
	    }
	    
	    int nbin = 100;
	    double volume_sum[][][] = new double[nR][nC][nS];
	    double maxSum = -Double.MAX_VALUE;
	    double minSum = Double.MAX_VALUE;
	    for (x = 0; x < nR; x++) {
	    	for (y = 0; y < nC; y++) {
	    		for (z = 0; z < nS; z++) {
	    			for (t = 0; t < nT; t++) {
	    				volume_sum[x][y][z] += volumes[x][y][z][t];
	    			}
	    			if (volume_sum[x][y][z] <  minSum) {
	    				minSum = volume_sum[x][y][z];
	    			}
	    			if (volume_sum[x][y][z] > maxSum) {
	    				maxSum = volume_sum[x][y][z];
	    			}
	    		}
	    	}
	    }
	    mask_data = new byte[nR][nC][nS];
	    mask_data = new byte[nR][nC][nS];
	    double intensity[] = new double[nbin];
	    int prob[] = new int[nbin];
	    for (i = 0; i < nbin; i++) {
	    	intensity[i] = minSum + ((2.0*i + 1.0)/2.0)*((maxSum - minSum)/nbin);
	    }
	    int binNum = 0;
	    for (x = 0; x < nR; x++) {
	    	for (y = 0; y < nC; y++) {
	    		for (z = 0; z < nS; z++) {
	    		    binNum = (int)(((volume_sum[x][y][z] - minSum)/(maxSum - minSum))*nbin);
	    		    if (binNum == nbin) {
	    		    	binNum =  nbin -1;
	    		    }
	    		    prob[binNum]++;
	    		}
	    	}
	    }
	    
	    int ind_max = -1;
	    int maxProb = -1;
	    for (i = 0; i < binNum; i++) {
	    	if (prob[i] > maxProb) {
	    		ind_max = i;
	    		maxProb = prob[i];
	    	}
	    }
	    if (((double)prob[ind_max + 1])/((double)maxProb) > 0.3) {
	        ind_max = ind_max-1;	
	    }
	    int tempInt[] = new int[nbin-1-ind_max];
	    for (i = ind_max+1; i <= nbin-1; i++) {
	    	tempInt[i-ind_max-1] = prob[i];
	    }
	    prob = null;
	    prob = new int[nbin-1-ind_max];
	    for (i = 0; i < nbin-1-ind_max; i++) {
	    	prob[i] = tempInt[i];
	    }
	    
	    double tempDouble[] = new double[nbin-1-ind_max];
	    for (i = ind_max+1; i <=nbin-1; i++) {
	    	tempDouble[i-ind_max-1] = intensity[i];
	    }
	    intensity = null;
	    intensity = new double[nbin-1-ind_max];
	    for (i = 0; i < nbin-1-ind_max; i++) {
	    	intensity[i] = tempDouble[i];
	    }
	    
	    gauss2FittingObservations = nbin-1-ind_max;
	    gauss2FittingData = new double[2*gauss2FittingObservations];
	    for (i = 0; i < gauss2FittingObservations; i++) {
	    	gauss2FittingData[2*i] = intensity[i];
	    	gauss2FittingData[2*i+1] = (double)prob[i];
	    }
	    
	    double xp[] = new double[] {(double)prob[gauss2FittingObservations/3], intensity[gauss2FittingObservations/3], 1.0, 
	    		(double)prob[2*gauss2FittingObservations/3], intensity[2*gauss2FittingObservations/3], 1.0};
	    CostFunction cost_function = new gauss2FittingFunctorExample();
	    ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, xp);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.linear_solver_type = LinearSolverType.DENSE_QR;
		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		if (display > 0) {
		    UI.setDataText(solverSummary.BriefReport());
		    UI.setDataText("Solved answer for a1*exp(-((x-b1)/c1)^2) + a2*exp(-((x-b2)/c2)^2)\n");
		    UI.setDataText("a1 = " + xp[0] + "\n");
		    UI.setDataText("b1 = " + xp[1] + "\n");
		    UI.setDataText("c1 = " + xp[2] + "\n");
		    UI.setDataText("a2 = " + xp[3] + "\n");
		    UI.setDataText("b2 = " + xp[4] + "\n");
		    UI.setDataText("c2 = " + xp[5] + "\n");
		}
		
		  double parameters[] = new double[]{(xp[1] + xp[4])/2.0};

		  GradientProblemSolverOptions options = new GradientProblemSolverOptions();
		  options.minimizer_progress_to_stdout = true;

		  GradientProblemSolverSummary summary = new GradientProblemSolverSummary();
		  GradientProblem gradientProblem = new GradientProblem(new diffGaussians(xp));
		  Solve(options, gradientProblem, parameters, summary);

	      System.out.println(summary.BriefReport());
	      UI.setDataText("Initial guess for intensity at which 2 Gaussians intersect = " + ((xp[1] + xp[4])/2.0) + "\n");
	      UI.setDataText("Final calculation for intensity at which 2 Gaussians intersect = " + parameters[0] + "\n");
	}
	
	    // f(x) = a1*exp(-((x-b1)/c1)^2) - a2*exp(-((x-b2)/c2)^2)
		class diffGaussians extends FirstOrderFunction {
		  double xp[];
		  public diffGaussians(double xp[]) {
			  super();
			  this.xp = xp;
		  }

		  public boolean Evaluate(double[] parameters,
		                        double[] cost,
		                        double[] gradient) {
		    final double x = parameters[0];
            double val1 = (x - xp[1])/xp[2];
            double val2 = (x - xp[4])/xp[5];
		    cost[0] = xp[0]*Math.exp(-val1*val1) - xp[3]*Math.exp(-val2*val2);
		    if (gradient != null) {
		      gradient[0] = -2.0*xp[0]*(val1/xp[2])*Math.exp(-val1*val1) + 2.0*xp[3]*(val2/xp[5])*Math.exp(-val2*val2);
		    }
		    return true;
		  }

		  public int NumParameters() { return 1; }
		} // class diffGaussians
	
	public boolean fitToExternalFunction(double x[], double residuals[], double jacobian[][]) {
		return true;
	}
	
	class gauss2FittingFunctorExample extends SizedCostFunction {
		
		public gauss2FittingFunctorExample() {
			// number of resdiuals
			// size of first parameter
			super(gauss2FittingObservations, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean fitToExternalFunction(double x[], double residual[], double jacobian[][]) {
			int i;
			for (i = 0; i < gauss2FittingObservations; i++) {
				double val1 = (gauss2FittingData[2*i] - x[1])/x[2];
				double val2 = (gauss2FittingData[2*i] - x[4])/x[5];
				double value = x[0]*Math.exp(-val1*val1) + x[3]*Math.exp(-val2*val2);
			    residual[i] = gauss2FittingData[2*i+1] - value;
			}

			return true;
		}
	};
	
} 