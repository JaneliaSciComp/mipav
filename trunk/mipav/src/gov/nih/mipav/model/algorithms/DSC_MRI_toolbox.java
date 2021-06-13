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
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentGraph;
import gov.nih.mipav.view.ViewJFrameGraph;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
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
    // For 2 ideal Gaussians with doCurveIntersect = true;
    // Final calculation for intensity at which 2 Gaussians intersect = 4.154614002693739
    // For 2 ideal Gaussians with doCurveIntersect = false solving a one parameter search problem
    // Final calculation for intensity at which 2 Gaussians intersect = 4.154017548758297
    boolean doCurveIntersect = true;
    double x_out[];
    double y_out[];
    double x_loc[];
    double y_loc[];
    
    boolean readTestImage = true;
    boolean test2PerfectGaussians = true;
    private double[] GxData;
    private double[] GxxData;
    private double sigmas[] = new double[] {1.0};
    
    public DSC_MRI_toolbox() {
    	
    }
	
	public DSC_MRI_toolbox(double volumes[][][][], double te, double tr) {
		this.volumes = volumes;
		this.te = te;
		this.tr = tr;
	}
	
	public void runAlgorithm() {
		if (readTestImage) {
			int x, y, z, t;
			final FileIO io = new FileIO();
		    io.setQuiet(true);
	        io.setSuppressProgressBar(true);
	        ModelImage img = io.readImage("C:" + File.separator + "TSP datasets" + File.separator +
	        		"dsc-mri-toolbox-master" + File.separator + "demo-data" + File.separator + "GRE_DSC.nii.gz");
	        if (img.getNDims() != 4) {
	        	System.err.println("img.getNDims() = " + img.getNDims());
	        	return;
	        }
	        nR = img.getExtents()[0];
	        nC = img.getExtents()[1];
	        nS = img.getExtents()[2];
	        nT = img.getExtents()[3];
	        int length = nR * nC;
	        int vol = length * nS;
	        int buffer_size = vol*nT;
	        short buffer[] = new short[buffer_size];
	        try {
	        	img.exportData(0, buffer_size, buffer);
	        }
	        catch (IOException e) {
	        	System.err.println("IOException " + e);
	        	return;
	        }
	        volumes = new double[nR][nC][nS][nT];
	        for (x = 0; x < nR; x++) {
	        	for (y = 0; y < nC; y++) {
	        		for (z = 0; z < nS; z++) {
	        			for (t = 0; t < nT; t++) {
	        				volumes[x][y][z][t] = buffer[x + y*nR + z*length + t*vol];
	        			}
	        		}
	        	}
	        }
		}
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
		
		if (conc == 0) {
			DSC_mri_mask();
		}
	}
	
	public void DSC_mri_mask() {
		int i, j, k, x, y, z, t;
		double mask_threshold;
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
	    double probDouble[] = new double[nbin-1-ind_max];
	    for (i = 0; i < nbin-1-ind_max; i++) {
	    	probDouble[i] = (double)tempInt[i];
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
	    //double ySum = 0.0;
	    for (i = 0; i < gauss2FittingObservations; i++) {
	    	gauss2FittingData[2*i] = intensity[i];
	    	gauss2FittingData[2*i+1] = probDouble[i];
	    	//ySum += probDouble[i];
	    }
	    double xp[] = null;
	    if (test2PerfectGaussians) {
	    	double a1 = 1000.0;
	    	double b1 = 3.0;
	    	double c1 = 1.0;
	    	double a2 = 2000.0;
	    	double b2 = 7.0;
	    	double c2 = 2.0;
	    	gauss2FittingObservations = 100;
	    	gauss2FittingData = new double[200];
	    	for (i = 0; i < 100; i++) {
	    		gauss2FittingData[2*i] = 0.1*i;
	    		intensity[i] = 0.1*i;
	    		double val1 = (0.1*i - b1)/c1;
	    		double val2 = (0.1*i - b2)/c2;
	    		gauss2FittingData[2*i+1] = a1*Math.exp(-val1*val1) + a2*Math.exp(-val2*val2);
	    		probDouble[i] = gauss2FittingData[2*i+1];
	    		//Initial estimates for a1*exp(-((x-b1)/c1)^2) + a2*exp(-((x-b2)/c2)^2)
	    		//a1 = 2014.0515679050666
	    		//b1 = 7.0
	    		//c1 = 1.9798989873223332
	    		//a2 = 1004.2121919422965
	    		//b2 = 2.9000000000000004
	    		//c2 = 0.9899494936611666
	    		//Ceres Solver Report: Iterations: 10, Initial cost: 7.261461e+04, Final cost: 1.129515e-07, Termination: CONVERGENCE
	    		//Solved answer for a1*exp(-((x-b1)/c1)^2) + a2*exp(-((x-b2)/c2)^2)
	    		//a1 = 2000.0000318008936
	    		//b1 = 6.9999999825169725
	    		//c1 = 1.9999999839278295
	    		//a2 = 1000.0000306581111
	    		//b2 = 2.99999987472384
	    		//c2 = 0.9999999590528206
	    	}
	    }
	    
	    
	    // Simple intialization scheme does not work
	    //double xp[] = new double[] {ySum/gauss2FittingObservations, intensity[gauss2FittingObservations/3], (intensity[intensity.length -1] - intensity[0])/3.0, 
	    		//ySum/gauss2FittingObservations, intensity[2*gauss2FittingObservations/3], (intensity[intensity.length -1] - intensity[0])/3.0};
	    byte zeroCrossing[][] = new byte[3*gauss2FittingObservations][];
	    int numberZeroCrossings[] = new int[3*gauss2FittingObservations];
	    int firstFiveIndex = -1;
	    int firstFourIndex = -1;
	    int firstThreeIndex = -1;
	    int firstTwoIndex = -1;
	    int firstOneIndex = -1;
	    for (i = 1; i <= 3*gauss2FittingObservations; i++) {
	    	sigmas[0] = 1 + (i-1)*0.1;
	    	zeroCrossing[i-1] = calcZeroX(probDouble);
	    	for (j = 0; j < zeroCrossing[i-1].length; j++) {
	    		if (zeroCrossing[i-1][j] == 1) {
	    			numberZeroCrossings[i-1]++;
	    		}
	    		
	    	}
	    	if ((firstFiveIndex == -1) && (numberZeroCrossings[i-1] == 5)) {
	    		firstFiveIndex = i-1;
	    	}
	    	else if ((firstFourIndex == -1) && (numberZeroCrossings[i-1] == 4)) {
	    		firstFourIndex = i-1;
	    	}
	    	else if ((firstThreeIndex == -1) && (numberZeroCrossings[i-1] == 3)) {
	    		firstThreeIndex = i-1;
	    	}
	    	else if ((firstTwoIndex == -1) && (numberZeroCrossings[i-1] == 2)) {
	    		firstTwoIndex = i-1;
	    	}
	    	else if ((firstOneIndex == -1) && (numberZeroCrossings[i-1] == 1)) {
	    		firstOneIndex = i-1;
	    	}
	    }
	    for (i = 0; i < 3*gauss2FittingObservations; i++) {
	    	Preferences.debug("i = " + (i+1) + " number zero crossings = " + numberZeroCrossings[i] + "\n",
	    			Preferences.DEBUG_ALGORITHM);
	    	int numberDisplayed = 0;
	    	for (j = 0; j < zeroCrossing[i].length; j++) {
	    		if (zeroCrossing[i][j] == 1) {
		    		if (numberDisplayed == numberZeroCrossings[i]-1) {
		    			Preferences.debug(j + "\n", Preferences.DEBUG_ALGORITHM);
		    		}
		    		else {
		    			Preferences.debug(j + " ", Preferences.DEBUG_ALGORITHM);
		    		}
		    		numberDisplayed++;
	    		}		
	    	}
	    }
	    if ((firstFourIndex == -1) && (firstTwoIndex == -1) && (firstOneIndex == -1)) {
	    	System.err.println("No scale space with 4 zero crossings found in initializing sum of Gaussians");
	    	System.err.println("No scale space with 2 zero crossings found in initializing sum of Gaussians");
	    	return;
	    }
	    if ((firstFourIndex == -1) && (firstOneIndex == -1)) {
	    	System.err.println("No scale space with 4 zero crossings found in initializing sum of Gaussians");
	    	return;
	    }
	    if ((firstTwoIndex == -1) && (firstOneIndex == -1)) {
	    	System.err.println("No scale space with 2 zero crossings found in initializing sum of Gaussians");
	    	return;
	    }
	    if (((firstFourIndex == -1) || (firstTwoIndex == -1)) && (firstOneIndex > 0)) {
	    	Preferences.debug("Removing unpaired branch in Gaussian initialization\n", Preferences.DEBUG_ALGORITHM);
	        int zeroCrossingsToEliminate[] = new int[firstOneIndex+1];
	        for (j = 0; j < zeroCrossing[firstOneIndex].length; j++) {
	        	if (zeroCrossing[firstOneIndex][j] == 1) {
	        		zeroCrossingsToEliminate[firstOneIndex] = j;
	        	}
	        }
	        for (j = firstOneIndex-1; j >= 0; j--) {
	        	int distance = Integer.MAX_VALUE;
	        	for (i = 0; i < zeroCrossing[j].length; i++) {
	        		if ((zeroCrossing[j][i] == 1) && (Math.abs(i - zeroCrossingsToEliminate[j+1]) < distance)) {
	        			zeroCrossingsToEliminate[j] = i;
	        			distance = Math.abs(i - zeroCrossingsToEliminate[j+1]);
	        		}
	        	}
	        }
	        for (j = 3*gauss2FittingObservations-1; j > firstOneIndex; j--) {
	        	for (i = 0; i < zeroCrossing[j].length; i++) {
	        		zeroCrossing[j][i] = 0;
	        	}
	        	numberZeroCrossings[j] = 0;
	        }
	        for (j = 0; j <= firstOneIndex; j++) {
	        	zeroCrossing[j][zeroCrossingsToEliminate[j]] = 0;
	        	numberZeroCrossings[j]--;
	        }
	        firstFourIndex = firstFiveIndex;
	        firstTwoIndex = firstThreeIndex;
	        for (i = 0; i < 3*gauss2FittingObservations; i++) {
		    	Preferences.debug("i = " + (i+1) + " number zero crossings = " + numberZeroCrossings[i] + "\n",
		    			Preferences.DEBUG_ALGORITHM);
		    	int numberDisplayed = 0;
		    	for (j = 0; j < zeroCrossing[i].length; j++) {
		    		if (zeroCrossing[i][j] == 1) {
			    		if (numberDisplayed == numberZeroCrossings[i]-1) {
			    			Preferences.debug(j + "\n", Preferences.DEBUG_ALGORITHM);
			    		}
			    		else {
			    			Preferences.debug(j + " ", Preferences.DEBUG_ALGORITHM);
			    		}
			    		numberDisplayed++;
		    		}		
		    	}
		    }
		    if ((firstFourIndex == -1) && (firstTwoIndex == -1)) {
		    	System.err.println("No scale space with 4 zero crossings found in initializing sum of Gaussians");
		    	System.err.println("No scale space with 2 zero crossings found in initializing sum of Gaussians");
		    	return;
		    }
		    if (firstFourIndex == -1) {
		    	System.err.println("No scale space with 4 zero crossings found in initializing sum of Gaussians");
		    	return;
		    }
		    if (firstTwoIndex == -1) {
		    	System.err.println("No scale space with 2 zero crossings found in initializing sum of Gaussians");
		    	return;
		    }
	    } // if (((firstFourIndex == -1) || (firstTwoIndex == -1)) && (firstOneIndex > 0))
	    int twoIndexLowLocation[] = new int[firstTwoIndex+1];
	    int twoIndexHighLocation[] = new int[firstTwoIndex+1];
	    for (j = 0; j < firstTwoIndex+1; j++) {
	    	twoIndexLowLocation[j] = -1;
	    	twoIndexHighLocation[j] = -1;
	    }
	    for (j = 0; j < zeroCrossing[firstTwoIndex].length; j++) {
	    	if ((zeroCrossing[firstTwoIndex][j] == 1) && (twoIndexLowLocation[firstTwoIndex] == -1)) {
	    	    twoIndexLowLocation[firstTwoIndex] = j;	
	    	}
	    	else if (zeroCrossing[firstTwoIndex][j] == 1) {
	    		twoIndexHighLocation[firstTwoIndex] = j;
	    	}
	    }
	  
	    for (j = firstTwoIndex-1; j >= 0; j--) {
		    int firstGaussianLowDistance = Integer.MAX_VALUE;
		    int firstGaussianHighDistance = Integer.MAX_VALUE;
		    for (i = 0; i < zeroCrossing[j].length; i++) {
		    	if ((zeroCrossing[j][i] == 1) && (Math.abs(i - twoIndexLowLocation[j+1]) < firstGaussianLowDistance)) {
		    		twoIndexLowLocation[j] = i;
		    		firstGaussianLowDistance = Math.abs(i - twoIndexLowLocation[j+1]);
		    	}
		    	else if ((zeroCrossing[j][i] == 1) && (Math.abs(i - twoIndexHighLocation[j+1]) < firstGaussianHighDistance)) {
		    		twoIndexHighLocation[j] = i;
		    		firstGaussianHighDistance = Math.abs(i - twoIndexHighLocation[j+1]);
		    	}
		    }
	    }
	    
	    sigmas[0] = 1.0;
	    makeGxKernels1D();
        double[] firstDerivBuffer = new double[probDouble.length];
        convolve(probDouble, GxData, firstDerivBuffer);
	    
	    int twoIndexLowSlope = 0;
        if (firstDerivBuffer[twoIndexLowLocation[0]] > 0) {
	         twoIndexLowSlope = 1;	
	    }
        else if (firstDerivBuffer[twoIndexLowLocation[0]] < 0) {
        	twoIndexLowSlope = -1;
        }
        int twoIndexHighSlope = 0;
        if (firstDerivBuffer[twoIndexHighLocation[0]] > 0) {
	         twoIndexHighSlope = 1;	
	    }
        else if (firstDerivBuffer[twoIndexHighLocation[0]] < 0) {
        	twoIndexHighSlope = -1;
        }
        if ((twoIndexLowSlope > 0) && (twoIndexHighSlope > 0)) {
        	System.err.println("In scale space with 2 zero crossings both crossings are positive");
        	return;
        }
        if ((twoIndexLowSlope < 0) && (twoIndexHighSlope < 0)) {
        	System.err.println("In scale space with 2 zero crossings both crossings are negative");
        	return;
        }
	    int fourIndexLowLocation[] = new int[firstFourIndex+1];
	    int fourIndexHighLocation[] = new int[firstFourIndex+1];
	    for (j = 0; j < firstFourIndex+1; j++) {
	    	fourIndexLowLocation[j] = -1;
	    	fourIndexHighLocation[j] = -1;
	    }
	    for (j = 0; j < zeroCrossing[firstFourIndex].length; j++) {
	    	if ((zeroCrossing[firstFourIndex][j] == 1) && (j != twoIndexLowLocation[firstFourIndex]) && (j != twoIndexHighLocation[firstFourIndex]) &&
	    		(fourIndexLowLocation[firstFourIndex] == -1)) {
	    	    fourIndexLowLocation[firstFourIndex] = j;	
	    	}
	    	else if ((zeroCrossing[firstFourIndex][j] == 1) && (j != twoIndexLowLocation[firstFourIndex]) && (j != twoIndexHighLocation[firstFourIndex])) {
	    		fourIndexHighLocation[firstFourIndex] = j;
	    	}
	    }
	    
	    for (j = firstFourIndex-1; j >= 0; j--) {
		    int secondGaussianLowDistance = Integer.MAX_VALUE;
		    int secondGaussianHighDistance = Integer.MAX_VALUE;
		    for (i = 0; i < zeroCrossing[j].length; i++) {
		    	if ((zeroCrossing[j][i] == 1) && (j != twoIndexLowLocation[j]) && (j != twoIndexHighLocation[j]) &&
		    			(Math.abs(i - fourIndexLowLocation[j+1]) < secondGaussianLowDistance)) {
		    		fourIndexLowLocation[j] = i;
		    		secondGaussianLowDistance = Math.abs(i - fourIndexLowLocation[j+1]);
		    	}
		    	else if ((zeroCrossing[j][i] == 1) && (j != twoIndexLowLocation[j]) && (j != twoIndexHighLocation[j]) &&
		    			(Math.abs(i - fourIndexHighLocation[j+1]) < secondGaussianHighDistance)) {
		    		fourIndexHighLocation[j] = i;
		    		secondGaussianHighDistance = Math.abs(i - fourIndexHighLocation[j+1]);
		    	}
		    }
	    }
	    
	    int fourIndexLowSlope = 0;
        if (firstDerivBuffer[fourIndexLowLocation[0]] > 0) {
	         fourIndexLowSlope = 1;	
	    }
        else if (firstDerivBuffer[fourIndexLowLocation[0]] < 0) {
        	fourIndexLowSlope = -1;
        }
        int fourIndexHighSlope = 0;
        if (firstDerivBuffer[fourIndexHighLocation[0]] > 0) {
	         fourIndexHighSlope = 1;	
	    }
        else if (firstDerivBuffer[fourIndexHighLocation[0]] < 0) {
        	fourIndexHighSlope = -1;
        }
        if ((fourIndexLowSlope > 0) && (fourIndexHighSlope > 0)) {
        	System.err.println("In scale space with 4 zero crossings 3 crossings are positive");
        	return;
        }
        if ((fourIndexLowSlope < 0) && (fourIndexHighSlope < 0)) {
        	System.err.println("In scale space with 4 zero crossings 3 crossings are negative");
        	return;
        }
	    
	    double firstGaussianMean = ((double)(intensity[twoIndexLowLocation[0]] + intensity[twoIndexHighLocation[0]]))/2.0;
	    double firstGaussianStandardDeviation = ((double)(intensity[twoIndexHighLocation[0]] - intensity[twoIndexLowLocation[0]]))/2.0;
	    double secondGaussianMean = ((double)(intensity[fourIndexLowLocation[0]] + intensity[fourIndexHighLocation[0]]))/2.0;
	    double secondGaussianStandardDeviation = ((double)(intensity[fourIndexHighLocation[0]] - intensity[fourIndexLowLocation[0]]))/2.0;
	    // a11*firstGaussianAmplitude + a12*secondGaussianAmplitude = b1
	    // a21*firstGaussianAmplitude + a22*secondGaussianAmplitude = b2;
	    double a11 = 0.0;
	    for (k = 0; k < gauss2FittingObservations; k++) {
	    	double val = (intensity[k] - firstGaussianMean)/firstGaussianStandardDeviation;
	    	a11 += Math.exp(-val * val);
	    }
	    double a12 = 0;
	    for (k = 0; k < gauss2FittingObservations; k++) {
	    	double diff1 = (intensity[k] - firstGaussianMean);
	    	double diff2 = (intensity[k] - secondGaussianMean);
	    	a12 += Math.exp(-diff1*diff1/(2.0*firstGaussianStandardDeviation*firstGaussianStandardDeviation) 
	    			        -diff2*diff2/(2.0*secondGaussianStandardDeviation*secondGaussianStandardDeviation));
	    }
	    double a21 = a12;
	    double a22 = 0.0;
	    for (k = 0; k < gauss2FittingObservations; k++) {
	    	double val = (intensity[k] - secondGaussianMean)/secondGaussianStandardDeviation;
	    	a22 += Math.exp(-val * val);
	    }
	    double b1 = 0.0;
	    for (k = 0; k < gauss2FittingObservations; k++) {
	    	double diff = (intensity[k] - firstGaussianMean);
	    	b1 += probDouble[k]*Math.exp(-diff*diff/(2.0*firstGaussianStandardDeviation*firstGaussianStandardDeviation));
	    }
	    double b2 = 0;
	    for (k = 0; k < gauss2FittingObservations; k++) {
	    	double diff = (intensity[k] - secondGaussianMean);
	    	b2 += probDouble[k]*Math.exp(-diff*diff/(2.0*secondGaussianStandardDeviation*secondGaussianStandardDeviation));
	    }
	    double det = a11*a22 - a21*a12;
	    if (det == 0.0) {
	    	System.err.println("Cannot solve linear equations for Gaussian amplitudes because determinant is zero");
	    	return;
	    }
	    double det1 = b1*a22 - b2*a12;
	    double det2 = a11*b2 - a21*b1;
	    double firstGaussianAmplitude = det1/det;
	    double secondGaussianAmplitude = det2/det;
	    xp = new double[] {firstGaussianAmplitude, firstGaussianMean, Math.sqrt(2.0)*firstGaussianStandardDeviation,
	    		secondGaussianAmplitude, secondGaussianMean, Math.sqrt(2.0)*secondGaussianStandardDeviation};
	    if (display > 0) {
	    	UI.setDataText("Initial estimates for a1*exp(-((x-b1)/c1)^2) + a2*exp(-((x-b2)/c2)^2)\n");
		    UI.setDataText("a1 = " + xp[0] + "\n");
		    UI.setDataText("b1 = " + xp[1] + "\n");
		    UI.setDataText("c1 = " + xp[2] + "\n");
		    UI.setDataText("a2 = " + xp[3] + "\n");
		    UI.setDataText("b2 = " + xp[4] + "\n");
		    UI.setDataText("c2 = " + xp[5] + "\n");
	    }
	   
	    CostFunction cost_function = new gauss2FittingCostFunction();
	    ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, xp);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.linear_solver_type = LinearSolverType.DENSE_QR;
		solverOptions.max_num_consecutive_invalid_steps = 100;
		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		if (display > 0) {
		    UI.setDataText(solverSummary.BriefReport() + "\n");
		    UI.setDataText("Solved answer for a1*exp(-((x-b1)/c1)^2) + a2*exp(-((x-b2)/c2)^2)\n");
		    UI.setDataText("a1 = " + xp[0] + "\n");
		    UI.setDataText("b1 = " + xp[1] + "\n");
		    UI.setDataText("c1 = " + xp[2] + "\n");
		    UI.setDataText("a2 = " + xp[3] + "\n");
		    UI.setDataText("b2 = " + xp[4] + "\n");
		    UI.setDataText("c2 = " + xp[5] + "\n");
		}
		
		if (doCurveIntersect) {
		    curveIntersect(intensity, xp);
		    mask_threshold = x_out[0];
		}
		else {
		  double parameters[] = new double[]{(xp[1] + xp[4])/2.0};

		  GradientProblemSolverOptions options = new GradientProblemSolverOptions();
		  options.minimizer_progress_to_stdout = true;

		  GradientProblemSolverSummary summary = new GradientProblemSolverSummary();
		  GradientProblem gradientProblem = new GradientProblem(new diffGaussians(xp));
		  Solve(options, gradientProblem, parameters, summary);
		  mask_threshold = parameters[0];
          if (display > 0) {
		      System.out.println(summary.BriefReport());
		      UI.setDataText("Initial guess for intensity at which 2 Gaussians intersect = " + ((xp[1] + xp[4])/2.0) + "\n");
          }
		}
		if (display > 0) {
		    UI.setDataText("Final calculation for intensity at which 2 Gaussians intersect = " + mask_threshold + "\n");
		}
		
		if (display > 1) {
			float intensityf[] = new float[intensity.length];
			for (i = 0; i < intensity.length; i++) {
				intensityf[i] = (float)intensity[i];
			}
			double y1[] = new double[intensity.length];
		    for (i = 0; i < intensity.length; i++) {
		    	double val1 = (intensity[i] - xp[1])/xp[2];
		    	y1[i] = xp[0]*Math.exp(-val1*val1);
		    }
		    double y2[] = new double[intensity.length];
		    for (i = 0; i < intensity.length; i++) {
		    	double val2 = (intensity[i] - xp[4])/xp[5];
		    	y2[i] = xp[3]*Math.exp(-val2*val2);
		    }
		    float fitf[] = new float[intensity.length];
		    for (i = 0; i < intensity.length; i++) {
		    	fitf[i] = (float)(y1[i] + y2[i]);
		    }
		    ViewJFrameGraph fittedGaussiansGraph = new ViewJFrameGraph(intensityf, fitf, "2 Gaussians fit", "Intensity", "Fitted Gaussians");
		    fittedGaussiansGraph.setVisible(true);
		    ViewJComponentGraph graph = fittedGaussiansGraph.getGraph();
		    Graphics g = graph.getGraphics();
		    graph.paintComponent(g);
		    Rectangle graphBounds = graph.getGraphBounds();
		    double axlim[] = new double[4];
		    axlim[0] = Double.MAX_VALUE;
			axlim[1] = -Double.MAX_VALUE;
			axlim[2] = Double.MAX_VALUE;
			axlim[3] = -Double.MAX_VALUE;
			for (i = 0; i < gauss2FittingObservations; i++) {
				if (intensityf[i] < axlim[0]) {
	    	    	axlim[0] = intensityf[i];
	    	    }
	    	    if (intensityf[i] > axlim[1]) {
	    	    	axlim[1] = intensityf[i];
	    	    }
	    	    if (fitf[i] < axlim[2]) {
	    	    	axlim[2] = fitf[i];
	    	    }
	    	    if (fitf[i] > axlim[3]) {
	    	    	axlim[3] = fitf[i];
	    	    }
			}
		    double xScale = graphBounds.width / (axlim[1] - axlim[0]);
	        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
		    int xthresh =  (int)Math.round(graphBounds.x + xScale*(mask_threshold - axlim[0]));
		    int y1t =  (int)Math.round(graphBounds.y + yScale*(0 - axlim[2]));
		    y1t = -y1t + 2*graphBounds.y + graphBounds.height;
		    double maxprob = -Double.MAX_VALUE;
		    for (i = 0; i < gauss2FittingObservations; i++) {
		    	if (probDouble[i] > maxprob) {
		    		maxprob = probDouble[i];
		    	}
		    }
		    int y2t =  (int)Math.round(graphBounds.y + yScale*(Math.min(1.05*maxprob,axlim[3]) - axlim[2]));
		    y2t = -y2t + 2*graphBounds.y + graphBounds.height;
		    g = graph.getGraphics();
		    g.setColor(Color.BLUE);
		    graph.drawLine(g,xthresh, y1t, xthresh, y2t);
		    graph.plotGraph(g);
		}
	}
	
	// Output zero edge crossings of second order derivative of 1D Gaussian of buffer
	public byte[] calcZeroX(double[] buffer) {
        makeGxxKernels1D();
        double[] secondDerivBuffer = new double[buffer.length];
        convolve(buffer, GxxData, secondDerivBuffer);
        return edgeDetect(secondDerivBuffer);
    }
	
	public byte[] edgeDetect(double secondDerivBuffer[]) {
		int i,j;
		double x0;
		double x1;
		int xDim = secondDerivBuffer.length;
		int xxDim = xDim - 1;
		byte edgeDetectBuffer[] = new byte[xDim];
		for (i = 0; i < xxDim; i++) {
			 x0 = secondDerivBuffer[i];
             x1 = secondDerivBuffer[i+1];
             if ((x0 > 0) && (x1 > 0)) {
            	 edgeDetectBuffer[i] = 0;
             }
             else if ((x0 < 0) && (x1 < 0)) {
            	 edgeDetectBuffer[i] = 0;
             }
             else if (i == 0) {
            	 // A false contour is easy to detect because it always appears
            	 // in either the first or the last column of the space-scale
            	 // image in high resolution.
            	 edgeDetectBuffer[i] = 0;
             }
             else {
            	 edgeDetectBuffer[i] = 1;
             }
             
		}
		return edgeDetectBuffer;
	}
	
	/**
     * Creates Gaussian derivative kernels.
     */
    private void makeGxxKernels1D() {
        int xkDim;
        int[] derivOrder = new int[1];

        int kExtents[] = new int[1];
        derivOrder[0] = 2;

        // For buffer all 1.0 values:
        // Sum of 9 GxxData coefficients = -7.2e-5 for sigmas[0] = 1.0;
        // Sum of 17 GxxData coefficients = -2.11E-7 for sigmas[0] = 1.0;
        xkDim = (int)Math.round(16 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        GxxData = new double[xkDim];

        GenerateGaussian Gxx = new GenerateGaussian(GxxData, kExtents, sigmas, derivOrder);

        Gxx.dcalc(false);
        Gxx.finalize();
        Gxx = null;  
    }
    
    private void makeGxKernels1D() {
        int xkDim;
        int[] derivOrder = new int[1];

        int kExtents[] = new int[1];
        derivOrder[0] = 1;

        xkDim = (int)Math.round(16 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;
        
        GxData = new double[xkDim];
        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);
        Gx.dcalc(false);
        Gx.finalize();
        Gx = null;
    }
	
	/**
     * Perform one-dimension convolution.
     * 
     * @param imageBuffer
     * @param kernelBuffer
     * @param resultBuffer
     * Problem is huge border effects.  Convolution uses kernels of length going from 9 to 801 on buffer of length 100.
     */
    private void convolve(final double[] imageBuffer, final double[] kernelBuffer, final double[] resultBuffer) {
        final int kernelDim = kernelBuffer.length;
        final int halfKernelDim = kernelDim / 2;
        for (int i = 0; i < imageBuffer.length; i++) {
                int count = 0;
                double sum = 0;
                double norm = 0;
                int start = i - halfKernelDim;
                int end = start + kernelDim;
                if (start < 0) {
                    count = count - start;
                    start = 0;
                }
                if (end > imageBuffer.length) {
                    end = imageBuffer.length;
                }
                for (int j = start; j < end; j++) {
                    sum += kernelBuffer[count] * imageBuffer[j];
                    if (kernelBuffer[count] > 0) {
                        norm += kernelBuffer[count];
                    } else {
                        norm -= kernelBuffer[count];
                    }
                    count++;
                }
                resultBuffer[i] = sum / norm;
        }
    }
	
	public void curveIntersect(double x[], double param[]) {
	    int i;
	    x_out = null;
	    y_out = null;
	    double y1[] = new double[x.length];
	    for (i = 0; i < x.length; i++) {
	    	double val1 = (x[i] - param[1])/param[2];
	    	y1[i] = param[0]*Math.exp(-val1*val1);
	    }
	    double y2[] = new double[x.length];
	    for (i = 0; i < x.length; i++) {
	    	double val2 = (x[i] - param[4])/param[5];
	    	y2[i] = param[3]*Math.exp(-val2*val2);
	    }
	    double x1in[];
	    double y1in[];
	    double x2in[];
	    double y2in[];
	    double ymin;
	    double ymax;
	    double diff_x[] = new double[x.length-1];
	    for (i = 0; i < x.length-1; i++) {
	    	diff_x[i] = x[i+1] - x[i];
	    }
	    int ind_x[] = new int[x.length-1];
	    for (i = 0; i < x.length-1; i++) {
	    	if (diff_x[i] > 0) {
	    		ind_x[i] = 1;
	    	}
	    	else if (diff_x[i] == 0.0) {
	    		ind_x[i] = 0;
	    	}
	    	else {
	    		ind_x[i] = -1;
	    	}
	    }
	    
	    int ind1 = 0;
	    while (ind1 < x.length-1) {
	    	boolean found = false;
	        int ind_max = ind1 - 1;
	        for (i = ind1 + 1; (i < ind_x.length) && (!found); i++) {
	        	if (ind_x[i] != ind_x[ind1]) {
	        		found = true;
	        		ind_max = ind1 + i -1;
	        	}
	        }
	        if (ind_max <= ind1) {
	        	ind_max = x.length-1;
	        }
	        int[] ind1_array = new int[ind_max - ind1 + 1];
	        for (i = 0; i < ind1_array.length; i++) {
	        	ind1_array[i] = ind1 + i;
	        }
	        
	        int ind2 = 0;
	        while (ind2 < x.length-1) {
	            found = false;
	            ind_max = ind2 - 1;
	            for (i = ind2 + 1; (i < ind_x.length) && (!found); i++) {
	            	if (ind_x[i] != ind_x[ind2]) {
	            		found = true;
	            		ind_max = ind2 + i - 1;
	            	}
	            }
	            if (ind_max <= ind2) {
	            	ind_max = x.length-1;
	            }
	            int[] ind2_array = new int[ind_max - ind2 + 1];
		        for (i = 0; i < ind2_array.length; i++) {
		        	ind2_array[i] = ind2 + i;
		        }
		        
		        if ((ind_x[ind1_array[0]] == 0) && (ind_x[ind2_array[0]] != 0)) {
		        	x_loc = new double[1];
		            x_loc[0] = x[ind1_array[0]];
		            x2in = new double[ind2_array.length];
		            for (i = 0; i < ind2_array.length; i++) {
		            	x2in[i] = x[ind2_array[i]];
		            }
		            y2in = new double[ind2_array.length];
		            for (i = 0; i < ind2_array.length; i++) {
		            	y2in[i] = y2[ind2_array[i]];
		            }
		            y_loc = interp1(x2in, y2in, x_loc);
		            ymin = Double.MAX_VALUE;
		            ymax = -Double.MAX_VALUE;
		            for (i = 0; i < ind1_array.length; i++) {
		            	if (y1[ind1_array[i]] < ymin) {
		            		ymin = y1[ind1_array[i]];
		            	}
		            	if (y1[ind1_array[i]] > ymax) {
		            		ymax = y1[ind1_array[i]];
		            	}
		            }
		            if (!((y_loc[0] >= ymin) && (y_loc[0] <= ymax))) {
		            	y_loc = null;
		            	x_loc = null;
		            }
		        }
		        else if ((ind_x[ind2_array[0]] == 0) && (ind_x[ind1_array[0]] != 0)) {
		        	x_loc = new double[1];
		        	x_loc[0] = x[ind2_array[0]];
		        	x1in = new double[ind1_array.length];
		            for (i = 0; i < ind1_array.length; i++) {
		            	x1in[i] = x[ind1_array[i]];
		            }
		            y1in = new double[ind1_array.length];
		            for (i = 0; i < ind1_array.length; i++) {
		            	y1in[i] = y1[ind1_array[i]];
		            }
		            y_loc = interp1(x1in, y1in, x_loc);
		            ymin = Double.MAX_VALUE;
		            ymax = -Double.MAX_VALUE;
		            for (i = 0; i < ind2_array.length; i++) {
		            	if (y2[ind2_array[i]] < ymin) {
		            		ymin = y2[ind2_array[i]];
		            	}
		            	if (y2[ind2_array[i]] > ymax) {
		            		ymax = y2[ind2_array[i]];
		            	}
		            }
		            if (!((y_loc[0] >= ymin) && (y_loc[0] <= ymax))) {
		            	y_loc = null;
		            	x_loc = null;
		            }
		        }
                else if ((ind_x[ind2_array[0]] != 0) && (ind_x[ind1_array[0]] != 0)) {
                	x1in = new double[ind1_array.length];
		            for (i = 0; i < ind1_array.length; i++) {
		            	x1in[i] = x[ind1_array[i]];
		            }
		            y1in = new double[ind1_array.length];
		            for (i = 0; i < ind1_array.length; i++) {
		            	y1in[i] = y1[ind1_array[i]];
		            }
		            x2in = new double[ind2_array.length];
		            for (i = 0; i < ind2_array.length; i++) {
		            	x2in[i] = x[ind2_array[i]];
		            }
		            y2in = new double[ind2_array.length];
		            for (i = 0; i < ind2_array.length; i++) {
		            	y2in[i] = y2[ind2_array[i]];
		            }
		            curveintersect_local(x1in, y1in, x2in, y2in);
		        }
                else if ((ind_x[ind2_array[0]] == 0) && (ind_x[ind1_array[0]] == 0)) {
		        	x_loc = null;
		        	y_loc = null;
		        }
		        if ((x_out == null) && (x_loc != null)) {
		        	x_out = new double[x_loc.length];
		        	for (i = 0; i < x_loc.length; i++) {
		        		x_out[i] = x_loc[i];
		        	}
		        }
		        else if ((x_out != null) && (x_loc != null)) {
		        	double temp[] = new double[x_out.length];
		        	for (i = 0; i < x_out.length; i++) {
		        		temp[i] = x_out[i];
		        	}
		        	x_out = null;
		        	x_out = new double[temp.length + x_loc.length];
		        	for (i = 0; i < temp.length; i++) {
		        		x_out[i] = temp[i];
		        	}
		        	for (i = 0; i < x_loc.length; i++) {
		        		x_out[temp.length + i] = x_loc[i];
		        	}
		        	temp = null;
		        }
		        if ((y_out == null) && (y_loc != null)) {
		        	y_out = new double[y_loc.length];
		        	for (i = 0; i < y_loc.length; i++) {
		        		y_out[i] = y_loc[i];
		        	}
		        }
		        else if ((y_out != null) && (y_loc != null)) {
		        	double temp[] = new double[y_out.length];
		        	for (i = 0; i < y_out.length; i++) {
		        		temp[i] = y_out[i];
		        	}
		        	y_out = null;
		        	y_out = new double[temp.length + y_loc.length];
		        	for (i = 0; i < temp.length; i++) {
		        		y_out[i] = temp[i];
		        	}
		        	for (i = 0; i < y_loc.length; i++) {
		        		y_out[temp.length + i] = y_loc[i];
		        	}
		        	temp = null;
		        }
		        ind2 = ind2_array[ind2_array.length-1];
	        }
	        ind1 = ind1_array[ind1_array.length-1];
	    }
	}
	
	public void curveintersect_local(double x1[], double y1[], double x2[], double y2[]) {
		int i, j;
	    boolean equalX = true;
	    if (x1.length != x2.length) {
	    	equalX = false;
	    }
	    else {
	    	for (i = 0; i < x1.length; i++) {
	    		if (x1[i] != x2[i]) {
	    			equalX = false;
	    		}
	    	}
	    }
	    double xx[];
	    double yy[];
	    if (!equalX) {
	    	double minx1 = Double.MAX_VALUE;
	    	double maxx1 = -Double.MAX_VALUE;
	    	double minx2 = Double.MAX_VALUE;
	    	double maxx2 = -Double.MAX_VALUE;
	    	for (i = 0; i < x1.length; i++) {
	    		if (x1[i] < minx1) {
	    			minx1 = x1[i];
	    		}
	    		if (x1[i] > maxx1) {
	    			maxx1 = x1[i];
	    		}
	    	}
	    	for (i = 0; i < x2.length; i++) {
	    		if (x2[i] < minx2) {
	    			minx2 = x2[i];
	    		}
	    		if (x2[i] > maxx2) {
	    			maxx2 = x2[i];
	    		}
	    	}
	    	double maxmin = Math.max(minx1, minx2);
	    	double minmax = Math.min(maxx1, maxx2);
	    	Vector<Double>xxVec = new Vector<Double>();
	    	for (i = 0; i < x1.length; i++)  {
	    		xxVec.add(x1[i]);
	    	}
	    	boolean unique;
	    	for (i = 0; i < x2.length; i++) {
	    		unique = true;
	    		for (j = 0; j < x1.length; j++) {
	    			if (x2[i] == x1[j]) {
	    				unique = false;
	    			}
	    		}
	    		if (unique && (x2[i] >= maxmin) && (x2[i] <= minmax)) {
	    			xxVec.add(x2[i]);
	    		}
	    	}
	    	if (xxVec.size() < 2) {
	    		x_loc = null;
	    		y_loc = null;
	    		return;
	    	}
	    	Collections.sort(xxVec);
	    	xx = new double[xxVec.size()];
	    	for (i = 0; i < xx.length; i++) {
	    		xx[i] = xxVec.get(i);
	    	}
	    	double yy1[] = interp1(x1,y1,xx);
	    	double yy2[] = interp1(x2,y2,xx);
	    	yy = new double[yy1.length];
	    	for (i = 0; i < yy.length; i++) {
	    		yy[i] = yy1[i] - yy2[i];
	    	}
	    }
	    else {
	        xx = x1;
	        yy = new double[y1.length];
	        for (i = 0; i < yy.length; i++) {
	        	yy[i] = y1[i] - y2[i];
	        }
	    }
	    mminvinterp(xx,yy,0.0); // find zero crossings of difference
	    if ((x_loc != null) && (x_loc.length > 0)) {
	        y_loc = interp1(x1, y1, x_loc);	
	    }
	    else {
	    	x_loc = null;
	    	y_loc = null;
	    }
	}
	
	public void mminvinterp(double x[], double y[], double yo) { 
		int i;
		if (x.length != y.length) {
			System.err.println("x.length != y.length in mminvinterp");
			return;
		}
		int n = y.length;
		
		// Quick exit if no values exist
	    double miny = Double.MAX_VALUE;
	    double maxy = -Double.MAX_VALUE;
	    for (i = 0; i < n; i++) {
	    	if (y[i] < miny) {
	    		miny = y[i];
	    	}
	    	if (y[i] > maxy) {
	    		maxy = y[i];
	    	}
	    }
	    if ((yo < miny) || (yo > maxy)) {
	    	x_loc = null;
	    	y_loc = null;
	    	return;
	    }
	    
	    // Find the desired points
	    boolean below[] = new boolean[n];
	    boolean above[] = new boolean[n];
	    boolean on[] = new boolean[n];
	    for (i = 0; i < n; i++) {
	    	if (y[i] < yo) {
	    		below[i] = true;
	    	}
	    	else if (y[i] > yo) {
	    		above[i] = true;
	    	}
	    	else {
	    		on[i] = true;
	    	}
	    }
	    
	    boolean kth[] = new boolean[n-1]; // point k
	    for (i = 0; i < n-1; i++) {
	    	kth[i] = ((below[i] & above[i+1]) | (above[i] & below[i+1]));
	    }
	    boolean kp1[] = new boolean[n]; // point k+1
	    kp1[0] = false;
	    for (i = 1; i < n; i++) {
	    	kp1[i] = kth[i-1];
	    }
	    
	    Vector<Double> xo = new Vector<Double>(); // distance between x[k+1] and x[k]
	    for (i = 0; i < n-1; i++) {
	    	if (kth[i]) {
	    		double alpha = (yo - y[i])/(y[i+1] - y[i]);
	    		xo.add(alpha*(x[i+1] - x[i]) + x[i]);
	    	}
	    }
	    for (i = 0; i < n; i++) {
	    	if (on[i]) {
	    		xo.add(x[i]);
	    	}
	    }
	    Collections.sort(xo);
	    // Add points, which are directly on the line
	    x_loc = new double[xo.size()];
	    for (i = 0; i < x_loc.length; i++) {
	    	x_loc[i] = xo.get(i);
	    }
	    // Duplicate yo, to match xo points found
	    y_loc = new double[x_loc.length];
	    for (i = 0; i < y_loc.length; i++) {
	    	y_loc[i] = yo;
	    }
	}
	
	public double[] interp1(double x[], double y[], double x_loc[]) {
		int i,j;
		double y_interp[] = new double[x_loc.length];
		int len = x.length;
		for (i = 0; i < x_loc.length; i++) {
			y_interp[i] = -Double.MAX_VALUE;
			for (j = 0; j < len; j++) {
				if (x[j] == x_loc[i]) {
				    y_interp[i] = y[j];
				}
				else if ((j < x.length-1) && (x_loc[i] > x[j])) {
					y_interp[i] = (y[j] + (y[j+1] - y[j])*(x_loc[i] - x[j])/(x[j+1] - x[j]));
				}
			}
		}
		return y_interp;
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
            double part1 = xp[0]*Math.exp(-val1*val1);
            double part2 = xp[3]*Math.exp(-val2*val2);
		    cost[0] = Math.abs(xp[0]*Math.exp(-val1*val1) - xp[3]*Math.exp(-val2*val2));
		    if (gradient != null) {
		    	if (part1 > part2) {
		            gradient[0] = -2.0*part1*(val1/xp[2]) + 2.0*part2*(val2/xp[5]);
		    	}
		    	else {
		    		gradient[0] = 2.0*part1*(val1/xp[2]) - 2.0*part2*(val2/xp[5]);
		    	}
		    }
		    return true;
		  }

		  public int NumParameters() { return 1; }
		} // class diffGaussians
	
	public boolean fitToExternalFunction(double x[], double residuals[], double jacobian[][]) {
		int i;
		for (i = 0; i < x.length; i++) {
			UI.setDataText("unexpected fit x["+i+"] = " + x[i] + "\n");
		}
		return true;
	}
	
	class gauss2FittingCostFunction extends SizedCostFunction {
		
		public gauss2FittingCostFunction() {
			// number of residuals
			// size of first parameter
			super(gauss2FittingObservations, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < gauss2FittingObservations; i++) {
				double val1 = (gauss2FittingData[2*i] - x[1])/x[2];
				double val2 = (gauss2FittingData[2*i] - x[4])/x[5];
				double value = x[0]*Math.exp(-val1*val1) + x[3]*Math.exp(-val2*val2);
			    residuals[i] = gauss2FittingData[2*i+1] - value;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][6*i] = -Math.exp(-val1*val1);
					jacobians[0][6*i+1] = -2.0*x[0]*val1*Math.exp(-val1*val1)/x[2];
					jacobians[0][6*i+2] = -2.0*x[0]*val1*Math.exp(-val1*val1)*(gauss2FittingData[2*i] - x[1])/(x[2]*x[2]);
					jacobians[0][6*i+3] = -Math.exp(-val2*val2);
					jacobians[0][6*i+4] = -2.0*x[3]*val2*Math.exp(-val2*val2)/x[5];
					jacobians[0][6*i+5] = -2.0*x[3]*val2*Math.exp(-val2*val2)*(gauss2FittingData[2*i] - x[4])/(x[5]*x[5]);
			    }
			}

			return true;
	  }
		
		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][], int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			
			for (i = 0; i < gauss2FittingObservations; i++) {
				double val1 = (gauss2FittingData[2*i] - x[1])/x[2];
				double val2 = (gauss2FittingData[2*i] - x[4])/x[5];
				double value = x[0]*Math.exp(-val1*val1) + x[3]*Math.exp(-val2*val2);
			    residuals[i] = gauss2FittingData[2*i+1] - value;
			    if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0] + 6*i] = -Math.exp(-val1*val1);
					jacobians[0][jacobians_offset[0] + 6*i+1] = -2.0*x[0]*val1*Math.exp(-val1*val1)/x[2];
					jacobians[0][jacobians_offset[0] + 6*i+2] = -2.0*x[0]*val1*Math.exp(-val1*val1)*(gauss2FittingData[2*i] - x[1])/(x[2]*x[2]);
					jacobians[0][jacobians_offset[0] + 6*i+3] = -Math.exp(-val2*val2);
					jacobians[0][jacobians_offset[0] + 6*i+4] = -2.0*x[3]*val2*Math.exp(-val2*val2)/x[5];
					jacobians[0][jacobians_offset[0] + 6*i+5] = -2.0*x[3]*val2*Math.exp(-val2*val2)*(gauss2FittingData[2*i] - x[4])/(x[5]*x[5]);
			    }
			}

			return true;
	  }
	};
	
} 