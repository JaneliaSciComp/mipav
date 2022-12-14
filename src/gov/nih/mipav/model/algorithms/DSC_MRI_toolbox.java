package gov.nih.mipav.model.algorithms;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Vector;

import javax.imageio.ImageIO;

import Jama.Matrix;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileTypeTable;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.model.structures.jama.GeneralizedInverse2;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.model.structures.jama.SVD;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentGraph;
import gov.nih.mipav.view.ViewJFrameGraph;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

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
References:
Curve Fitting by a Sum of Gaussians, Ardeshir Goshtasby and William D. O'Neill,
CVGIP: Graphical Models and Image Processing, Vol. 56, No. 4, July, pp. 281-288, 1994.
## Features

### Semi-automatic AIF selection

The method is based on dicotomic hierarchical clustering method, it only need to select
 the slice where it will look for the best AIF. Please cite [1] if you use the AIF extraction tool:

[1] **Peruzzo Denis**,  Bertoldo Alessandra, Zanderigo Francesca and Cobelli Claudio, "[Automatic selection
 of arterial input function on dynamic contrast-enhanced MR images][paper1]", *Computer methods and programs
  in biomedicine, 104:e148-e157 (2011)*.
*/

public class DSC_MRI_toolbox extends CeresSolver {
	private ViewUserInterface UI;
	// 4D matrix with raw GRE-DSC acquisition
	private double volumes[][][][];
	private double conc[][][][];
	private double AIFslice[][][];
	private ModelImage srcImage = null;
	private int nC;
	private int nR;
	private int nS;
	private int nT;
	private int extents2D[] = new int[2];
	private int extents3D[] = new int[3];
	private int extents4D[] = new int[4];
	float resolutions3D[] = new float[3];
	int units3D[] = new int[3];
	float resolutions4D[] = new float[4];
	int units4D[] = new int[4];
	private int length;
	private int volume;
	private double time[];
	// echo time in seconds
	private double te = 0.025;
	// repetition time in seconds
	private double tr = 1.55;

	// DISPLAY OPTIONS
	private int display = 0; // 0:off, 1:notify (text), 2:notify (images), 3:debug
	private int waitbar = 1; // 0:off, 1:on

	// DATA PREPARATION OPTIONS

	// Represents the number of minimum pixels of a connected component that is used
	// as a threshold to exclude the scalp and adjacent areas outside the brain
	// from the image
	private int mask_npixel = 300;
	// 0: The data provided is a signal, 1: The data provided are concentrations
	private int options_conc = 0;

	// Minimum number of initial scans on which to calculate S0
	private int S0_nSamplesMin = 3;
	// Maximum number of initial scans on whcih to caclulate s0
	private int S0_nSamplesMax = 12;

	// Threshold used to choose the instant of appearance of the tracer
	private double S0_thresh = 0.05;

	// OPTIONS FOR THE IDENTIFICATION PHASE OF THE AIF
	// 0: It does not calculate the AIF, 1: It does calculate the AIF
	private int aif_enable = 1;

	// 0: It does not take recirculation into account, 1: Fits the recirculation
	private int aif_recirculation = 1;

	// Slice on which to search for the AIF (-1 : Makes the operator select the
	// slice)
	private int aif_nSlice = -1;

	// Dimension of the semimajor axis for the search area
	private double aif_semiMajorAxis = 0.40;

	// Dimension of the semiminor axis for the search area
	private double aif_semiMinorAxis = 0.20;

	// Fraction of voxels discarded due to area under curve
	private double aif_pArea = 0.4;

	// Fraction of voxels discarded due to TTP
	private double aif_pTTP = 0.4;

	// Fraction of voxels discarded due to the regularity of the trend
	private double aif_pReg = 0.05;

	// Threshold to decide whether to select cluster based on peak or TTP
	private double aif_diffPeak = 0.04;

	// Maximum voxels chosen for AIF
	//private int aif_nVoxelMax = 6;
	private int aif_nVoxelMax = 15;

	// Minimum voxels chosen for AIF
	//private int aif_nVoxelMin = 4;
	private int aif_nVoxelMin = 10;

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
	private int deconv_cSVD_residual = 1;

	// Threshold of 10% with Ostergaard and Calamante
	private double deconv_oSVD_OIthres = 0.035;
	//private int deconv_oSVD_OIcounter = 1;
	// 0: Does not save residuals, 1: Saves residuals
	private int deconv_oSVD_residual = 1;

	// TO ADD PARAMETERS FOR STABLE SPLINE
	private int deconv_SS_residual = 1;

	// Methods to be applied for perfusion calculation
	// "SVD", "cSVD", "oSVD"
	private String[] deconv_method = new String[] {"cSVD"};
	// On GRE_DSC.nii.gz without file saves:
	// For SVD DSC_mri_core() execution time in seconds = 20.403
	// For cSVD DSC_mri_core() execution time in seconds = 20.141
	// For oSVD DSC_mri_core() execution time in seconds = 552.111

	// Constants ofproportionality
	private double par_kh = 1.0;
	private double par_rho = 1.0;
	private double par_kvoi = 1.0;

	// Mask optimized for masking the entire brain
	private byte mask_data[][][];
	// Mask optimized for finding the arterial input function
	private byte mask_aif[][][];
	private byte mask_aif_slice[][];

	private int gauss2FittingObservations;
	private double gauss2FittingData[];
	// For 2 ideal Gaussians with doCurveIntersect = true;
	// Final calculation for intensity at which 2 Gaussians intersect =
	// 4.154614002693739
	// For 2 ideal Gaussians with doCurveIntersect = false solving a one parameter
	// search problem
	// Final calculation for intensity at which 2 Gaussians intersect =
	// 4.154017548758297
	boolean doCurveIntersect = false;
	double x_out[];
	double y_out[];
	double x_loc[];
	double y_loc[];

	boolean readTestImage = true;
	boolean test2PerfectGaussians = false;
	private double[] GxData;
	private double[] GxxData;
	private double sigmas[] = new double[] { 1.0 };
	double firstGaussianMean;
	int firstGaussianMeanBin;
	double firstGaussianAmplitude;
	double firstGaussianStandardDeviation;
	double c1;
	int secondGaussianMeanBin;
	double secondGaussianAmplitude;
	double secondGaussianStandardDeviation;
	double secondGaussianMean;
	double c2;
	double minSum;
	double maxSum;
	private String outputFilePath = "C:" + File.separator + "TSP datasets" + File.separator + "dsc-mri-toolbox-master"
			+ File.separator + "demo-data" + File.separator;
	private String inputFileName = null;
	private String outputPrefix = "";
	private FileIO fileIO = null;

	private int saveFileFormat = FileUtility.NIFTI;
	// used by CoreTool to only save AIF/sliceAIF pngs, if chosen by the user
	private boolean doSaveAllOutputs = true;
	double S0map[][][];
	int bolus[];
	boolean equalTimeSpacing;
	byte AIF_ROI[][];
	double AIF_ROI_x[];
	double AIF_ROI_y[];
	double AIF_conc[];
	int AIF_voxels[][];
	double data_peak1[];
	double weights_peak1[];
	double fitParameters_peak1[];
	double cv_est_parGV_peak1[];
	double TR;
	double Tmax;
	double Tmin;
	double TRfine;
	int nTfine;
	double tGrid[];
	double dati_peak2[];
	double weights_peak2[];
	double fitParameters_peak2[];
	double cv_est_parGV_peak2[];
	double AIF_fit_weights[];
	double AIF_fit_parameters[];
	double AIF_fit_cv_est_parGV[];
	double AIF_fit_gv[];
	double t0_init;
	double alpha_init;
	double beta_init;
	double A_init;
	double td_init;
	double K_init;
	double tao_init;
	boolean gaussStandardDeviationCheck = false;
	boolean gauss1FittingCheck = false;
	boolean gauss2FittingCheck = false;
	boolean GVFittingCheck = false;
	boolean GVRecirculationCheck = false;
	boolean doAIFTransfer = false;
	boolean errorInMaskRoutine = false;
	double intensity[] = null;
	double probDouble[] = null;
	double cbv[][][] = null;
	double cbv_lc[][][] = null;
	double cbf_svd[][][] = null;
	double cbf_svd_residual[][][][] = null;
	double cbf_csvd[][][] = null;
	double cbf_csvd_residual[][][][] = null;
	double cbf_osvd[][][] = null;
	double cbf_osvd_OI[][][] = null;
	double cbf_osvd_residual[][][][] = null;
	double mtt_svd[][][] = null;
	double mtt_csvd[][][] = null;
	double mtt_osvd[][][] = null;
	short ttp[][][] = null;
	double fwhm[][][] = null;

	public DSC_MRI_toolbox() {

	}
	
	public DSC_MRI_toolbox(ModelImage srcImage, String outputFilePath, double te, double tr, int aif_nSlice, 
			String[] deconv_method) {
		this.srcImage = srcImage;
		this.outputFilePath = outputFilePath;
		this.te = te;
		this.tr = tr;
		this.aif_nSlice = aif_nSlice;
		this.deconv_method = deconv_method;
		doAIFTransfer = false;
		readTestImage = false;
		doSaveAllOutputs = true;
	}

	public DSC_MRI_toolbox(double volumes[][][][], double te, double tr, int aif_nSlice, String outputFilePath) {
		this.volumes = volumes;
		this.te = te;
		this.tr = tr;
		this.aif_nSlice = aif_nSlice;
		this.outputFilePath = outputFilePath;
		doAIFTransfer = true;
		readTestImage = false;
		doSaveAllOutputs = false;
	}

	public void runAlgorithm() {
		final long startTime = System.currentTimeMillis();
		int x, y, z, t;
		
		if (!doAIFTransfer) {
			if (readTestImage) {
				 final FileIO io = new FileIO();
				 io.setQuiet(true);
				 io.setSuppressProgressBar(true);
				 srcImage = io.readImage("C:" + File.separator + "TSP datasets" +
				 File.separator
				 + "dsc-mri-toolbox-master" + File.separator + "demo-data" + File.separator +
				 "GRE_DSC.nii.gz");
				//srcImage = io.readImage(
				//		"C:" + File.separator + "TSP datasets" + File.separator + "EVTcase#1-baseline PWI" + File.separator
				//				+ "baseline PWI" + File.separator + "ST000001" + File.separator + "SE000001Original.nii");
				 aif_nSlice = 7;
			}
			if (srcImage.getNDims() != 4) {
				System.err.println("srcImage.getNDims() = " + srcImage.getNDims());
				return;
			}
			nC = srcImage.getExtents()[0];
			nR = srcImage.getExtents()[1];
			nS = srcImage.getExtents()[2];
			nT = srcImage.getExtents()[3];
			float resolutions[] = srcImage.getFileInfo()[0].getResolutions();
			resolutions3D[0] = resolutions[0];
			resolutions3D[1] = resolutions[1];
			resolutions3D[2] = resolutions[2];
			resolutions4D[0] = resolutions[0];
			resolutions4D[1] = resolutions[1];
			resolutions4D[2] = resolutions[2];
			resolutions4D[3] = resolutions[3];
			int units[] = srcImage.getFileInfo()[0].getUnitsOfMeasure();
			units3D[0] = units[0];
			units3D[1] = units[1];
			units3D[2] = units[2];
			units4D[0] = units[0];
			units4D[1] = units[1];
			units4D[2] = units[2];
			units4D[3] = units[3];
			length = nC * nR;
			volume = length * nS;
			int buffer_size = volume * nT;
			extents2D[0] = nC;
			extents2D[1] = nR;
			extents3D[0] = nC;
			extents3D[1] = nR;
			extents3D[2] = nS;
			extents4D[0] = nC;
			extents4D[1] = nR;
			extents4D[2] = nS;
			extents4D[3] = nT;
			short buffer[] = new short[buffer_size];
			try {
				srcImage.exportData(0, buffer_size, buffer);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			if (readTestImage) {
			    srcImage.disposeLocal();
			    srcImage = null;
			}
			volumes = new double[nC][nR][nS][nT];
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					for (z = 0; z < nS; z++) {
						for (t = 0; t < nT; t++) {
							volumes[x][y][z][t] = buffer[x + y * nC + z * length + t * volume];
						}
					}
				}
			}

			DSC_mri_core();

		} else  { // doAIFTransfer
			nC = volumes.length;
			nR = volumes[0].length;
			nS = volumes[0][0].length;
			nT = volumes[0][0][0].length;
			aif_nSlice = nS / 2; // aif_nSlice = 0 for nS = 1;
			length = nC * nR;
			volume = length * nS;
			extents2D[0] = nC;
			extents2D[1] = nR;
			extents3D[0] = nC;
			extents3D[1] = nR;
			extents3D[2] = nS;
			DSC_mri_core();
			if (errorInMaskRoutine) {
				AIF_voxels = null;
				return;
			}
		}
		long totalTime = System.currentTimeMillis() - startTime;
		if (doAIFTransfer) {
		    System.out.println("AIF extraction execution time in seconds = " + (totalTime / 1000.0));
		}
		else {
			System.out.println("DSC_mri_core() execution time in seconds = " + (totalTime / 1000.0));
		}

	}

	public int[][] getAIF_voxels() {
		return AIF_voxels;
	}

	public void DSC_mri_core() {
		int i;
		UI = ViewUserInterface.getReference();
		if (display > 0) {
			UI.setDataText("Checking data...\n");
		}
		nC = volumes.length;
		nR = volumes[0].length;
		nS = volumes[0][0].length;
		nT = volumes[0][0][0].length;

		time = new double[nT];
		for (i = 0; i < nT; i++) {
			time[i] = i * tr;
		}
		// 1.) Definition of the virtual grid necessary for convolution
		// Prevent repetition in GVfunction_recirculation
		TR = time[1] - time[0];
		Tmax = -Double.MAX_VALUE;
		Tmin = Double.MAX_VALUE;
		int t;
		for (t = 0; t < nT; t++) {
			if (time[t] < Tmin) {
				Tmin = time[t];
			}
			if (time[t] > Tmax) {
				Tmax = time[t];
			}
		}

		TRfine = TR / 10.0;
		nTfine = 1 + (int) ((2 * Tmax - Tmin) / TRfine);
		tGrid = new double[nTfine];
		for (t = 0; t < nTfine; t++) {
			tGrid[t] = Tmin + t * TRfine;
		}
		equalTimeSpacing = true;

		if (display > 0) {
			UI.setDataText("DATA SIZE\n");
			UI.setDataText("Columns = " + nC + "\n");
			UI.setDataText("Rows = " + nR + "\n");
			UI.setDataText("Slices = " + nS + "\n");
			UI.setDataText("Samples = " + nT + "\n");
			UI.setDataText("Echo time = " + te + "\n");
			UI.setDataText("Repetition time = " + tr + "\n");
		}

		if (options_conc == 0) {
			DSC_mri_mask();
			if (errorInMaskRoutine) {
				return;
			}

			// Calculations of Concentrations and S0
			DSC_mri_conc();
		}

		// AIF extraction
		if (aif_enable == 1) {
			DSC_mri_aif();
		}
		
		if (doAIFTransfer) {
			return;
		}
		
		if (display > 0) {
			UI.setDataText("Calculating maps\n");
		}
		
		// CBV calculation
		DSC_mri_cbv();
		
		// CBV leakage correction
		DSC_mri_cbv_lc();
		
		// CBF calculation
		DSC_mri_cbf();
		
		// MTT calculation
		DSC_mri_mtt();
		
		// TTP calculation
		DSC_mri_ttp();
		
		// fwhm calculation
		DSC_mri_fwhm();
	}

	public void DSC_mri_mask() {
		// Original MATLAB code was last modified by Marco Castallaro 08/07/2010
		// Author of original MATLAB code: Marco Castallaro - Universita di Padova - DEI
		//
		// Calculate DSC-MRI exam masks
		//
		// Input parameters: volumes (4D matrix) which contains the trends of the DSC
		// signals
		// of all voxels.
		// And the structure that contains the method options, the significant ones are:
		//
		// mask_npixel: represents the number of minimum pixels of a connected component
		// that
		// is used as a threshold to exclude the scalp and adjacent areas outside the
		// brain
		// from the image.
		//
		// display: Level 1 shows the processing progress
		// Level 2 shows the masks and information on the threshold and intensity of
		// the images to be masked.
		//
		// Output parameters: Structure mask, which contains:
		// mask_aif: Mask optimized for the arterial input function
		// mask_data: Mask optimized for masking the entire brain
		// mask_threshold: Threshold calculated and supplied at the output
		//
		// For the supplied sample image only the right half of the first Gaussian is
		// present, so find the
		// mean and amplitude of the first Gaussian with a simple search for the highest
		// peak.
		// Use the ratio of the amplitude 3 channels to right of the mean to the
		// amplitude of the
		// mean to obtain an initial estimate of the standard deviation. Use a 1
		// parameter search over the
		// 4 channels from the mean to 3 channels right of the mean to obtain a refined
		// value of the first
		// Gaussian standard deviation. Since a half Gaussian will not produce 2 paired
		// in scaled space,
		// eliminate all zero crossings to 3 channels right of the
		// first Gaussian mean over all scales. Use scale space to find the amplitude,
		// mean, and standard deviation
		// of the second Gaussian. Find the smallest sigma for which only 2 branches are
		// present and
		// follow them scale by scale to the down to the zero crossings at the smallest
		// sigma. If there is
		// no sigma for which only 2 branches are present, find the lowest sigma for
		// which only one branch
		// is present, and follow it down and eliminate all zero crossings for the
		// unpaired lone branch. Then
		// look for the new smallest sigma at which only 2 branches are present.
		int i, j, k, x, y, z, t, s;
		boolean doneBackupSecondGaussian = false;
		double[] firstDerivBuffer = null;
		int twoIndexLowLocation[] = null;
		int twoIndexHighLocation[] = null;
		double mask_threshold;
		if (display > 0) {
			UI.setDataText("Masking data...");
		}

		int nbin = 100;
		double volume_sum[][][] = new double[nC][nR][nS];
		maxSum = -Double.MAX_VALUE;
		minSum = Double.MAX_VALUE;
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				for (z = 0; z < nS; z++) {
					for (t = 0; t < nT; t++) {
						volume_sum[x][y][z] += volumes[x][y][z][t];
					}
					if (volume_sum[x][y][z] < minSum) {
						minSum = volume_sum[x][y][z];
					}
					if (volume_sum[x][y][z] > maxSum) {
						maxSum = volume_sum[x][y][z];
					}
				}
			}
		}
		mask_data = new byte[nC][nR][nS];
		intensity = new double[nbin];
		int prob[] = new int[nbin];
		for (i = 0; i < nbin; i++) {
			intensity[i] = minSum + ((2.0 * i + 1.0) / 2.0) * ((maxSum - minSum) / nbin);
		}
		int binNum = 0;
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				for (z = 0; z < nS; z++) {
					binNum = (int) (((volume_sum[x][y][z] - minSum) / (maxSum - minSum)) * nbin);
					if (binNum == nbin) {
						binNum = nbin - 1;
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
		if (((double) prob[ind_max + 1]) / ((double) maxProb) > 0.3) {
			ind_max = ind_max - 1;
		}
		int tempInt[] = new int[nbin - 1 - ind_max];
		for (i = ind_max + 1; i <= nbin - 1; i++) {
			tempInt[i - ind_max - 1] = prob[i];
		}
		prob = null;
		probDouble = new double[nbin - 1 - ind_max];
		for (i = 0; i < nbin - 1 - ind_max; i++) {
			probDouble[i] = (double) tempInt[i];
		}

		double tempDouble[] = new double[nbin - 1 - ind_max];
		for (i = ind_max + 1; i <= nbin - 1; i++) {
			tempDouble[i - ind_max - 1] = intensity[i];
		}
		intensity = null;
		intensity = new double[nbin - 1 - ind_max];
		for (i = 0; i < nbin - 1 - ind_max; i++) {
			intensity[i] = tempDouble[i];
		}

		gauss2FittingObservations = nbin - 1 - ind_max;
		gauss2FittingData = new double[2 * gauss2FittingObservations];
		// double ySum = 0.0;
		for (i = 0; i < gauss2FittingObservations; i++) {
			gauss2FittingData[2 * i] = intensity[i];
			gauss2FittingData[2 * i + 1] = probDouble[i];
			Preferences.debug("probDouble[" + i + "] = " + probDouble[i] + "\n", Preferences.DEBUG_ALGORITHM);
			// ySum += probDouble[i];
		}
		double xp[] = null;

		if (!test2PerfectGaussians) {
			// The first Gaussian at the extreme left is found by highest amplitude
			firstGaussianMeanBin = -1;
			firstGaussianAmplitude = -Double.MAX_VALUE;
			for (i = 0; i < intensity.length; i++) {
				if (probDouble[i] > firstGaussianAmplitude) {
					firstGaussianAmplitude = probDouble[i];
					firstGaussianMeanBin = i;
				}
			}
			firstGaussianMean = intensity[firstGaussianMeanBin];
			UI.setDataText("First Gaussian amplitude = " + firstGaussianAmplitude + "\n");
			UI.setDataText("First Gaussian mean = " + firstGaussianMean + "\n");
			// Calculate c1 from amplitude of best fit of highest channel and next 3
			// channels to the right

			// Initial estimate of c1 = sqrt(2) * standard deviation
			double xp1[] = new double[1];
			double ratio = probDouble[firstGaussianMeanBin + 3] / probDouble[firstGaussianMeanBin];
			double diff = (intensity[firstGaussianMeanBin + 3] - firstGaussianMean);
			// exp(-((x-mean)/c1)^2) = ratio
			// -(intensity[firstGaussianMeanBin+3] - firstGaussianMean)^2/(c1*c1) =
			// ln(ratio)
			xp1[0] = Math.sqrt(-(diff * diff) / Math.log(ratio));
			CostFunction cost_function1 = new gaussStandardDeviationFittingCostFunction();
			ProblemImpl problem = new ProblemImpl();
			problem.AddResidualBlock(cost_function1, null, xp1);

			// Run the solver!
			SolverOptions solverOptions = new SolverOptions();
			solverOptions.linear_solver_type = LinearSolverType.DENSE_QR;
			solverOptions.max_num_consecutive_invalid_steps = 100;
			solverOptions.minimizer_progress_to_stdout = true;
			SolverSummary solverSummary = new SolverSummary();
			Solve(solverOptions, problem, solverSummary);
			c1 = xp1[0];
			firstGaussianStandardDeviation = Math.sqrt(2.0) * c1;
			if (display > 0) {
				UI.setDataText(solverSummary.BriefReport() + "\n");
				UI.setDataText("Solved answer for firstGaussianAmplitude*exp(-((x-firstGaussianMean)/c1)^2)\n");
				UI.setDataText("c1 = " + xp1[0] + "\n");
			}

			if (gaussStandardDeviationCheck) {
				// Solved answer for firstGaussianAmplitude*exp(-((x-firstGaussianMean)/c1)^2)
				// Ceres Solver Report: Iterations: 4, Initial cost: 7.957407e+09, Final cost:
				// 6.387359e+07, Termination: CONVERGENCE
				// c1 = 755.8417495326521

				// ******* Elsunc Fit Gauss Standard Deviation Fitting *********
				// analyticalJacobian = true
				// Number of iterations: 5
				// Chi-squared: 1.2774713914732091E8
				// a0 755.7982689862721
				// ******* Elsunc Fit Gauss Standard Deviation Fitting *********
				// analyticalJacobian = false
				// Number of iterations: 5
				// Chi-squared: 1.2774713914732087E8
				// a0 755.7982690260098
				System.out.println("Solved answer for firstGaussianAmplitude*exp(-((x-firstGaussianMean)/c1)^2)\n");
				System.out.println(solverSummary.BriefReport());
				System.out.println("c1 = " + xp1[0] + "\n");
				boolean doAnalytical = true;
				xp1[0] = Math.sqrt(-(diff * diff) / Math.log(ratio));
				gaussStandardDeviationFitting gsd = new gaussStandardDeviationFitting(xp1, doAnalytical);
				gsd.driver();
				gsd.dumpResults();
				doAnalytical = false;
				xp1[0] = Math.sqrt(-(diff * diff) / Math.log(ratio));
				gsd = new gaussStandardDeviationFitting(xp1, doAnalytical);
				gsd.driver();
				gsd.dumpResults();
				System.exit(0);
			} // if (gaussStandardDeviationCheck)

			secondGaussianMeanBin = -1;
			secondGaussianAmplitude = -Double.MAX_VALUE;
			for (i = firstGaussianMeanBin + 4; i < intensity.length; i++) {
				if ((probDouble[i] > secondGaussianAmplitude) && (probDouble[i] > probDouble[i - 3])) {
					secondGaussianAmplitude = probDouble[i];
					secondGaussianMeanBin = i;
				}
			}
			secondGaussianMean = intensity[secondGaussianMeanBin];
			UI.setDataText("Second Gaussian amplitude = " + secondGaussianAmplitude + "\n");
			UI.setDataText("Second Gaussian mean = " + secondGaussianMean + "\n");
			// Calculate c2 from amplitude of best fit of highest channel and next 3
			// channels to the right

			// Initial estimate of c1 = sqrt(2) * standard deviation
			ratio = probDouble[secondGaussianMeanBin + 3] / probDouble[secondGaussianMeanBin];
			diff = (intensity[secondGaussianMeanBin + 3] - secondGaussianMean);
			// exp(-((x-mean)/c1)^2) = ratio
			// -(intensity[secondGaussianMeanBin+3] - secondGaussianMean)^2/(c2*c2) =
			// ln(ratio)
			xp1[0] = Math.sqrt(-(diff * diff) / Math.log(ratio));
			cost_function1 = new gaussSecondStandardDeviationFittingCostFunction();
			problem = new ProblemImpl();
			problem.AddResidualBlock(cost_function1, null, xp1);

			// Run the solver!
			solverOptions = new SolverOptions();
			solverOptions.linear_solver_type = LinearSolverType.DENSE_QR;
			solverOptions.max_num_consecutive_invalid_steps = 100;
			solverOptions.minimizer_progress_to_stdout = true;
			solverSummary = new SolverSummary();
			Solve(solverOptions, problem, solverSummary);
			c2 = xp1[0];
			secondGaussianStandardDeviation = Math.sqrt(2.0) * c2;
			if (display > 0) {
				UI.setDataText(solverSummary.BriefReport() + "\n");
				UI.setDataText("Solved answer for secondGaussianAmplitude*exp(-((x-secondGaussianMean)/c2)^2)\n");
			}
			UI.setDataText("c2 = " + xp1[0] + "\n");
			xp = new double[] { firstGaussianAmplitude, firstGaussianMean, c1, secondGaussianAmplitude,
					secondGaussianMean, c2 };
		} // if (!test2PerfectGaussians)

		if (test2PerfectGaussians) {
			double a1 = 1000.0;
			double b1 = 3.0;
			c1 = 1.0;
			double a2 = 2000.0;
			double b2 = 7.0;
			c2 = 2.0;
			gauss2FittingObservations = 100;
			gauss2FittingData = new double[200];
			for (i = 0; i < 100; i++) {
				gauss2FittingData[2 * i] = 0.1 * i;
				intensity[i] = 0.1 * i;
				double val1 = (0.1 * i - b1) / c1;
				double val2 = (0.1 * i - b2) / c2;
				gauss2FittingData[2 * i + 1] = a1 * Math.exp(-val1 * val1) + a2 * Math.exp(-val2 * val2);
				probDouble[i] = gauss2FittingData[2 * i + 1];
				// Initial estimates for a1*exp(-((x-b1)/c1)^2) + a2*exp(-((x-b2)/c2)^2)
				// a1 = 2014.0515679050666
				// b1 = 7.0
				// c1 = 1.9798989873223332
				// a2 = 1004.2121919422965
				// b2 = 2.9000000000000004
				// c2 = 0.9899494936611666
				// Ceres Solver Report: Iterations: 10, Initial cost: 7.261461e+04, Final cost:
				// 1.129515e-07, Termination: CONVERGENCE
				// Solved answer for a1*exp(-((x-b1)/c1)^2) + a2*exp(-((x-b2)/c2)^2)
				// a1 = 2000.0000318008936
				// b1 = 6.9999999825169725
				// c1 = 1.9999999839278295
				// a2 = 1000.0000306581111
				// b2 = 2.99999987472384
				// c2 = 0.9999999590528206
			}
		} // if (test2PerfectGaussians)

		// Simple intialization scheme does not work
		// double xp[] = new double[] {ySum/gauss2FittingObservations,
		// intensity[gauss2FittingObservations/3], (intensity[intensity.length -1] -
		// intensity[0])/3.0,
		// ySum/gauss2FittingObservations, intensity[2*gauss2FittingObservations/3],
		// (intensity[intensity.length -1] - intensity[0])/3.0};
		/*
		 * byte zeroCrossing[][] = new byte[3*gauss2FittingObservations][]; int
		 * numberZeroCrossings[] = new int[3*gauss2FittingObservations]; int
		 * firstFiveIndex = -1; int firstFourIndex = -1; int firstThreeIndex = -1; int
		 * firstTwoIndex = -1; int firstOneIndex = -1; for (i = 1; i <=
		 * 3*gauss2FittingObservations; i++) { sigmas[0] = 1 + (i-1)*0.1;
		 * zeroCrossing[i-1] = calcZeroX(probDouble); for (j = 0; j <
		 * zeroCrossing[i-1].length; j++) { if (zeroCrossing[i-1][j] == 1) {
		 * numberZeroCrossings[i-1]++; }
		 * 
		 * } if ((firstFiveIndex == -1) && (numberZeroCrossings[i-1] == 5)) {
		 * firstFiveIndex = i-1; } else if ((firstFourIndex == -1) &&
		 * (numberZeroCrossings[i-1] == 4)) { firstFourIndex = i-1; } else if
		 * ((firstThreeIndex == -1) && (numberZeroCrossings[i-1] == 3)) {
		 * firstThreeIndex = i-1; } else if ((firstTwoIndex == -1) &&
		 * (numberZeroCrossings[i-1] == 2)) { firstTwoIndex = i-1; } else if
		 * ((firstOneIndex == -1) && (numberZeroCrossings[i-1] == 1)) { firstOneIndex =
		 * i-1; } } for (i = 0; i < 3*gauss2FittingObservations; i++) {
		 * Preferences.debug("i = " + (i+1) + " number zero crossings = " +
		 * numberZeroCrossings[i] + "\n", Preferences.DEBUG_ALGORITHM); int
		 * numberDisplayed = 0; for (j = 0; j < zeroCrossing[i].length; j++) { if
		 * (zeroCrossing[i][j] == 1) { if (numberDisplayed == numberZeroCrossings[i]-1)
		 * { Preferences.debug(j + "\n", Preferences.DEBUG_ALGORITHM); } else {
		 * Preferences.debug(j + " ", Preferences.DEBUG_ALGORITHM); } numberDisplayed++;
		 * } } } if (test2PerfectGaussians && (firstFourIndex == -1) && (firstTwoIndex
		 * == -1) && (firstOneIndex == -1)) { System.err.
		 * println("No scale space with 4 zero crossings found in initializing sum of Gaussians"
		 * ); System.err.
		 * println("No scale space with 2 zero crossings found in initializing sum of Gaussians"
		 * ); errorInMaskRoutine = true; return; } if (test2PerfectGaussians &&
		 * (firstFourIndex == -1) && (firstOneIndex == -1)) { System.err.
		 * println("No scale space with 4 zero crossings found in initializing sum of Gaussians"
		 * ); errorInMaskRoutine = true; return; } if (test2PerfectGaussians &&
		 * (firstTwoIndex == -1) && (firstOneIndex == -1)) { System.err.
		 * println("No scale space with 2 zero crossings found in initializing sum of Gaussians"
		 * ); errorInMaskRoutine = true; return; } if ((!test2PerfectGaussians) &&
		 * (firstTwoIndex == -1) && (firstOneIndex == -1)) { System.err.
		 * println("No scale space with 2 zero crossings found in initializing sum of Gaussians"
		 * ); backupSecondGaussianRoutine(); doneBackupSecondGaussian = true; } if
		 * (test2PerfectGaussians && ((firstFourIndex == -1) || (firstTwoIndex == -1))
		 * && (firstOneIndex > 0)) {
		 * Preferences.debug("Removing unpaired branch in Gaussian initialization\n",
		 * Preferences.DEBUG_ALGORITHM); int zeroCrossingsToEliminate[] = new
		 * int[firstOneIndex+1]; for (j = 0; j < zeroCrossing[firstOneIndex].length;
		 * j++) { if (zeroCrossing[firstOneIndex][j] == 1) {
		 * zeroCrossingsToEliminate[firstOneIndex] = j; } } for (j = firstOneIndex-1; j
		 * >= 0; j--) { int distance = Integer.MAX_VALUE; for (i = 0; i <
		 * zeroCrossing[j].length; i++) { if ((zeroCrossing[j][i] == 1) && (Math.abs(i -
		 * zeroCrossingsToEliminate[j+1]) < distance)) { zeroCrossingsToEliminate[j] =
		 * i; distance = Math.abs(i - zeroCrossingsToEliminate[j+1]); } } } for (j =
		 * 3*gauss2FittingObservations-1; j > firstOneIndex; j--) { for (i = 0; i <
		 * zeroCrossing[j].length; i++) { zeroCrossing[j][i] = 0; }
		 * numberZeroCrossings[j] = 0; } for (j = 0; j <= firstOneIndex; j++) {
		 * zeroCrossing[j][zeroCrossingsToEliminate[j]] = 0; numberZeroCrossings[j]--; }
		 * firstFourIndex = firstFiveIndex; firstTwoIndex = firstThreeIndex; for (i = 0;
		 * i < 3*gauss2FittingObservations; i++) { Preferences.debug("i = " + (i+1) +
		 * " number zero crossings = " + numberZeroCrossings[i] + "\n",
		 * Preferences.DEBUG_ALGORITHM); int numberDisplayed = 0; for (j = 0; j <
		 * zeroCrossing[i].length; j++) { if (zeroCrossing[i][j] == 1) { if
		 * (numberDisplayed == numberZeroCrossings[i]-1) { Preferences.debug(j + "\n",
		 * Preferences.DEBUG_ALGORITHM); } else { Preferences.debug(j + " ",
		 * Preferences.DEBUG_ALGORITHM); } numberDisplayed++; } } } if ((firstFourIndex
		 * == -1) && (firstTwoIndex == -1)) { System.err.
		 * println("No scale space with 4 zero crossings found in initializing sum of Gaussians"
		 * ); System.err.
		 * println("No scale space with 2 zero crossings found in initializing sum of Gaussians"
		 * ); errorInMaskRoutine = true; return; } if (firstFourIndex == -1) {
		 * System.err.
		 * println("No scale space with 4 zero crossings found in initializing sum of Gaussians"
		 * ); errorInMaskRoutine = true; return; } if (firstTwoIndex == -1) {
		 * System.err.
		 * println("No scale space with 2 zero crossings found in initializing sum of Gaussians"
		 * ); errorInMaskRoutine = true; return; } } // if (test2PerfectGaussians &&
		 * ((firstFourIndex == -1) || (firstTwoIndex == -1)) && (firstOneIndex > 0)) if
		 * ((!test2PerfectGaussians) && (!doneBackupSecondGaussian) && (firstTwoIndex ==
		 * -1) && (firstOneIndex > 0)) {
		 * Preferences.debug("Removing unpaired branch in Gaussian initialization\n",
		 * Preferences.DEBUG_ALGORITHM); int zeroCrossingsToEliminate[] = new
		 * int[firstOneIndex+1]; for (j = 0; j < zeroCrossing[firstOneIndex].length;
		 * j++) { if (zeroCrossing[firstOneIndex][j] == 1) {
		 * zeroCrossingsToEliminate[firstOneIndex] = j; } } for (j = firstOneIndex-1; j
		 * >= 0; j--) { int distance = Integer.MAX_VALUE; for (i = 0; i <
		 * zeroCrossing[j].length; i++) { if ((zeroCrossing[j][i] == 1) && (Math.abs(i -
		 * zeroCrossingsToEliminate[j+1]) < distance)) { zeroCrossingsToEliminate[j] =
		 * i; distance = Math.abs(i - zeroCrossingsToEliminate[j+1]); } } } for (j =
		 * 3*gauss2FittingObservations-1; j > firstOneIndex; j--) { for (i = 0; i <
		 * zeroCrossing[j].length; i++) { zeroCrossing[j][i] = 0; }
		 * numberZeroCrossings[j] = 0; } for (j = 0; j <= firstOneIndex; j++) {
		 * zeroCrossing[j][zeroCrossingsToEliminate[j]] = 0; numberZeroCrossings[j]--; }
		 * firstTwoIndex = firstThreeIndex; for (i = 0; i < 3*gauss2FittingObservations;
		 * i++) { Preferences.debug("i = " + (i+1) + " number zero crossings = " +
		 * numberZeroCrossings[i] + "\n", Preferences.DEBUG_ALGORITHM); int
		 * numberDisplayed = 0; for (j = 0; j < zeroCrossing[i].length; j++) { if
		 * (zeroCrossing[i][j] == 1) { if (numberDisplayed == numberZeroCrossings[i]-1)
		 * { Preferences.debug(j + "\n", Preferences.DEBUG_ALGORITHM); } else {
		 * Preferences.debug(j + " ", Preferences.DEBUG_ALGORITHM); } numberDisplayed++;
		 * } } } if (firstTwoIndex == -1) { System.err.
		 * println("No scale space with 2 zero crossings found in initializing sum of Gaussians"
		 * ); backupSecondGaussianRoutine(); doneBackupSecondGaussian = true; } } // if
		 * ((!test2PerfectGaussians) && (!doneBackupSecondGaussian) && (firstTwoIndex ==
		 * -1) && (firstOneIndex > 0)) if (!doneBackupSecondGaussian) {
		 * twoIndexLowLocation = new int[firstTwoIndex+1]; twoIndexHighLocation = new
		 * int[firstTwoIndex+1]; for (j = 0; j < firstTwoIndex+1; j++) {
		 * twoIndexLowLocation[j] = -1; twoIndexHighLocation[j] = -1; } for (j = 0; j <
		 * zeroCrossing[firstTwoIndex].length; j++) { if
		 * ((zeroCrossing[firstTwoIndex][j] == 1) && (twoIndexLowLocation[firstTwoIndex]
		 * == -1)) { twoIndexLowLocation[firstTwoIndex] = j; } else if
		 * (zeroCrossing[firstTwoIndex][j] == 1) { twoIndexHighLocation[firstTwoIndex] =
		 * j; } }
		 * 
		 * for (j = firstTwoIndex-1; j >= 0; j--) { int GaussianLowDistance =
		 * Integer.MAX_VALUE; int GaussianHighDistance = Integer.MAX_VALUE; for (i = 0;
		 * i < zeroCrossing[j].length; i++) { if ((zeroCrossing[j][i] == 1) &&
		 * (Math.abs(i - twoIndexLowLocation[j+1]) < GaussianLowDistance)) {
		 * twoIndexLowLocation[j] = i; GaussianLowDistance = Math.abs(i -
		 * twoIndexLowLocation[j+1]); } else if ((zeroCrossing[j][i] == 1) &&
		 * (Math.abs(i - twoIndexHighLocation[j+1]) < GaussianHighDistance)) {
		 * twoIndexHighLocation[j] = i; GaussianHighDistance = Math.abs(i -
		 * twoIndexHighLocation[j+1]); } } }
		 * 
		 * sigmas[0] = 1.0; makeGxKernels1D(); firstDerivBuffer = new
		 * double[probDouble.length]; convolve(probDouble, GxData, firstDerivBuffer);
		 * 
		 * int twoIndexLowSlope = 0; if (firstDerivBuffer[twoIndexLowLocation[0]] > 0) {
		 * twoIndexLowSlope = 1; } else if (firstDerivBuffer[twoIndexLowLocation[0]] <
		 * 0) { twoIndexLowSlope = -1; } int twoIndexHighSlope = 0; if
		 * (firstDerivBuffer[twoIndexHighLocation[0]] > 0) { twoIndexHighSlope = 1; }
		 * else if (firstDerivBuffer[twoIndexHighLocation[0]] < 0) { twoIndexHighSlope =
		 * -1; } if ((twoIndexLowSlope > 0) && (twoIndexHighSlope > 0)) { System.err.
		 * println("In scale space with 2 zero crossings both crossings are positive");
		 * backupSecondGaussianRoutine(); doneBackupSecondGaussian = true; } else if
		 * ((twoIndexLowSlope < 0) && (twoIndexHighSlope < 0)) { System.err.
		 * println("In scale space with 2 zero crossings both crossings are negative");
		 * backupSecondGaussianRoutine(); doneBackupSecondGaussian = true; } else if
		 * ((twoIndexLowSlope < 0) && (twoIndexHighSlope > 0)) { System.err.
		 * println("In scale space with 2 zero crossings low crossing is negative and high crossing is positive"
		 * ); backupSecondGaussianRoutine(); doneBackupSecondGaussian = true; } } // if
		 * (!doneBackupSecondGaussian)
		 * 
		 * if (test2PerfectGaussians) { int fourIndexLowLocation[] = new
		 * int[firstFourIndex+1]; int fourIndexHighLocation[] = new
		 * int[firstFourIndex+1]; for (j = 0; j < firstFourIndex+1; j++) {
		 * fourIndexLowLocation[j] = -1; fourIndexHighLocation[j] = -1; } for (j = 0; j
		 * < zeroCrossing[firstFourIndex].length; j++) { if
		 * ((zeroCrossing[firstFourIndex][j] == 1) && (j !=
		 * twoIndexLowLocation[firstFourIndex]) && (j !=
		 * twoIndexHighLocation[firstFourIndex]) &&
		 * (fourIndexLowLocation[firstFourIndex] == -1)) {
		 * fourIndexLowLocation[firstFourIndex] = j; } else if
		 * ((zeroCrossing[firstFourIndex][j] == 1) && (j !=
		 * twoIndexLowLocation[firstFourIndex]) && (j !=
		 * twoIndexHighLocation[firstFourIndex])) {
		 * fourIndexHighLocation[firstFourIndex] = j; } }
		 * 
		 * for (j = firstFourIndex-1; j >= 0; j--) { int secondGaussianLowDistance =
		 * Integer.MAX_VALUE; int secondGaussianHighDistance = Integer.MAX_VALUE; for (i
		 * = 0; i < zeroCrossing[j].length; i++) { if ((zeroCrossing[j][i] == 1) && (j
		 * != twoIndexLowLocation[j]) && (j != twoIndexHighLocation[j]) && (Math.abs(i -
		 * fourIndexLowLocation[j+1]) < secondGaussianLowDistance)) {
		 * fourIndexLowLocation[j] = i; secondGaussianLowDistance = Math.abs(i -
		 * fourIndexLowLocation[j+1]); } else if ((zeroCrossing[j][i] == 1) && (j !=
		 * twoIndexLowLocation[j]) && (j != twoIndexHighLocation[j]) && (Math.abs(i -
		 * fourIndexHighLocation[j+1]) < secondGaussianHighDistance)) {
		 * fourIndexHighLocation[j] = i; secondGaussianHighDistance = Math.abs(i -
		 * fourIndexHighLocation[j+1]); } } }
		 * 
		 * int fourIndexLowSlope = 0; if (firstDerivBuffer[fourIndexLowLocation[0]] > 0)
		 * { fourIndexLowSlope = 1; } else if (firstDerivBuffer[fourIndexLowLocation[0]]
		 * < 0) { fourIndexLowSlope = -1; } int fourIndexHighSlope = 0; if
		 * (firstDerivBuffer[fourIndexHighLocation[0]] > 0) { fourIndexHighSlope = 1; }
		 * else if (firstDerivBuffer[fourIndexHighLocation[0]] < 0) { fourIndexHighSlope
		 * = -1; } if ((fourIndexLowSlope > 0) && (fourIndexHighSlope > 0)) {
		 * System.err.
		 * println("In scale space with 4 zero crossings 3 crossings are positive");
		 * errorInMaskRoutine = true; return; } if ((fourIndexLowSlope < 0) &&
		 * (fourIndexHighSlope < 0)) { System.err.
		 * println("In scale space with 4 zero crossings 3 crossings are negative");
		 * errorInMaskRoutine = true; return; } if ((fourIndexLowSlope < 0) &&
		 * (fourIndexHighSlope > 0)) { System.err.
		 * println("In scale space with 4 zero crossings second Gaussian low crossing is negative and second Gaussian high crossing is positive"
		 * ); errorInMaskRoutine = true; return; }
		 * 
		 * firstGaussianMean = ((double)(intensity[twoIndexLowLocation[0]] +
		 * intensity[twoIndexHighLocation[0]]))/2.0; firstGaussianStandardDeviation =
		 * ((double)(intensity[twoIndexHighLocation[0]] -
		 * intensity[twoIndexLowLocation[0]]))/2.0; secondGaussianMean =
		 * ((double)(intensity[fourIndexLowLocation[0]] +
		 * intensity[fourIndexHighLocation[0]]))/2.0; secondGaussianStandardDeviation =
		 * ((double)(intensity[fourIndexHighLocation[0]] -
		 * intensity[fourIndexLowLocation[0]]))/2.0; // a11*firstGaussianAmplitude +
		 * a12*secondGaussianAmplitude = b1 // a21*firstGaussianAmplitude +
		 * a22*secondGaussianAmplitude = b2; double a11 = 0.0; for (k = 0; k <
		 * gauss2FittingObservations; k++) { double val = (intensity[k] -
		 * firstGaussianMean)/firstGaussianStandardDeviation; a11 += Math.exp(-val *
		 * val); } double a12 = 0; for (k = 0; k < gauss2FittingObservations; k++) {
		 * double diff1 = (intensity[k] - firstGaussianMean); double diff2 =
		 * (intensity[k] - secondGaussianMean); a12 +=
		 * Math.exp(-diff1*diff1/(2.0*firstGaussianStandardDeviation*
		 * firstGaussianStandardDeviation)
		 * -diff2*diff2/(2.0*secondGaussianStandardDeviation*
		 * secondGaussianStandardDeviation)); } double a21 = a12; double a22 = 0.0; for
		 * (k = 0; k < gauss2FittingObservations; k++) { double val = (intensity[k] -
		 * secondGaussianMean)/secondGaussianStandardDeviation; a22 += Math.exp(-val *
		 * val); } double b1 = 0.0; for (k = 0; k < gauss2FittingObservations; k++) {
		 * double diff = (intensity[k] - firstGaussianMean); b1 +=
		 * probDouble[k]*Math.exp(-diff*diff/(2.0*firstGaussianStandardDeviation*
		 * firstGaussianStandardDeviation)); } double b2 = 0; for (k = 0; k <
		 * gauss2FittingObservations; k++) { double diff = (intensity[k] -
		 * secondGaussianMean); b2 +=
		 * probDouble[k]*Math.exp(-diff*diff/(2.0*secondGaussianStandardDeviation*
		 * secondGaussianStandardDeviation)); } double det = a11*a22 - a21*a12; if (det
		 * == 0.0) { System.err.
		 * println("Cannot solve linear equations for Gaussian amplitudes because determinant is zero"
		 * ); return; } double det1 = b1*a22 - b2*a12; double det2 = a11*b2 - a21*b1;
		 * firstGaussianAmplitude = det1/det; secondGaussianAmplitude = det2/det; xp =
		 * new double[] {firstGaussianAmplitude, firstGaussianMean,
		 * Math.sqrt(2.0)*firstGaussianStandardDeviation, secondGaussianAmplitude,
		 * secondGaussianMean, Math.sqrt(2.0)*secondGaussianStandardDeviation}; if
		 * (display > 0) { UI.
		 * setDataText("Initial estimates for a1*exp(-((x-b1)/c1)^2) + a2*exp(-((x-b2)/c2)^2)\n"
		 * ); UI.setDataText("a1 = " + xp[0] + "\n"); UI.setDataText("b1 = " + xp[1] +
		 * "\n"); UI.setDataText("c1 = " + xp[2] + "\n"); UI.setDataText("a2 = " + xp[3]
		 * + "\n"); UI.setDataText("b2 = " + xp[4] + "\n"); UI.setDataText("c2 = " +
		 * xp[5] + "\n"); }
		 * 
		 * CostFunction cost_function = new gauss2FittingCostFunction(); ProblemImpl
		 * problem = new ProblemImpl(); problem.AddResidualBlock(cost_function, null,
		 * xp);
		 * 
		 * // Run the solver! SolverOptions solverOptions = new SolverOptions();
		 * solverOptions.linear_solver_type = LinearSolverType.DENSE_QR;
		 * solverOptions.max_num_consecutive_invalid_steps = 100;
		 * solverOptions.minimizer_progress_to_stdout = true; SolverSummary
		 * solverSummary = new SolverSummary(); Solve(solverOptions, problem,
		 * solverSummary); if (display > 0) { UI.setDataText(solverSummary.BriefReport()
		 * + "\n"); UI.
		 * setDataText("Solved answer for a1*exp(-((x-b1)/c1)^2) + a2*exp(-((x-b2)/c2)^2)\n"
		 * ); UI.setDataText("a1 = " + xp[0] + "\n"); UI.setDataText("b1 = " + xp[1] +
		 * "\n"); UI.setDataText("c1 = " + xp[2] + "\n"); UI.setDataText("a2 = " + xp[3]
		 * + "\n"); UI.setDataText("b2 = " + xp[4] + "\n"); UI.setDataText("c2 = " +
		 * xp[5] + "\n"); } if (gauss2FittingCheck) { // Ceres Solver Report:
		 * Iterations: 10, Initial cost: 7.261461e+04, Final cost: 1.129515e-07,
		 * Termination: CONVERGENCE // Solved answer for a1*exp(-((x-b1)/c1)^2) +
		 * a2*exp(-((x-b2)/c2)^2) // a1 = 2000.0000318008936 // b1 = 6.9999999825169725
		 * // c1 = 1.9999999839278295 // a2 = 1000.0000306581111 // b2 =
		 * 2.99999987472384 // c2 = 0.9999999590528206 // ******* Elsunc 2 Gaussian
		 * Curve Fitting ********* // analyticalJacobian = true // Number of iterations:
		 * 4 // Chi-squared: 0.0 // a0 2000.0 // a1 7.0 // a2 2.0 // a3 1000.0 // a4 3.0
		 * // a5 1.0 // ******* Elsunc 2 Gaussian Curve Fitting ********* //
		 * analyticalJacobian = false // Number of iterations: 4 // Chi-squared: 0.0 //
		 * a0 2000.0 // a1 7.0 // a2 2.0 // a3 1000.0 // a4 3.0 // a5 1.0
		 * System.out.println(solverSummary.BriefReport()); System.out.
		 * println("Solved answer for a1*exp(-((x-b1)/c1)^2) + a2*exp(-((x-b2)/c2)^2)");
		 * System.out.println("a1 = " + xp[0]); System.out.println("b1 = " + xp[1]);
		 * System.out.println("c1 = " + xp[2]); System.out.println("a2 = " + xp[3]);
		 * System.out.println("b2 = " + xp[4]); System.out.println("c2 = " + xp[5]);
		 * 
		 * boolean doAnalytical = true; xp = new double[] {firstGaussianAmplitude,
		 * firstGaussianMean, Math.sqrt(2.0)*firstGaussianStandardDeviation,
		 * secondGaussianAmplitude, secondGaussianMean,
		 * Math.sqrt(2.0)*secondGaussianStandardDeviation}; gauss2Fitting g2 = new
		 * gauss2Fitting(xp, doAnalytical); g2.driver(); g2.dumpResults(); doAnalytical
		 * = false; xp = new double[] {firstGaussianAmplitude, firstGaussianMean,
		 * Math.sqrt(2.0)*firstGaussianStandardDeviation, secondGaussianAmplitude,
		 * secondGaussianMean, Math.sqrt(2.0)*secondGaussianStandardDeviation}; g2 = new
		 * gauss2Fitting(xp, doAnalytical); g2.driver(); g2.dumpResults();
		 * System.exit(0); } } // if (test2PerfectGaussians) else if
		 * (!doneBackupSecondGaussian){ secondGaussianMean =
		 * ((double)(intensity[twoIndexLowLocation[0]] +
		 * intensity[twoIndexHighLocation[0]]))/2.0; secondGaussianStandardDeviation =
		 * ((double)(intensity[twoIndexHighLocation[0]] -
		 * intensity[twoIndexLowLocation[0]]))/2.0; c2 = Math.sqrt(2.0) *
		 * secondGaussianStandardDeviation; // secondGaussianAmplitude = (b2 -
		 * a21*firstGaussianAmplitude)/a22 double a21 = 0; for (k = 0; k <
		 * gauss2FittingObservations; k++) { double diff1 = (intensity[k] -
		 * firstGaussianMean); double diff2 = (intensity[k] - secondGaussianMean); a21
		 * += Math.exp(-diff1*diff1/(2.0*firstGaussianStandardDeviation*
		 * firstGaussianStandardDeviation)
		 * -diff2*diff2/(2.0*secondGaussianStandardDeviation*
		 * secondGaussianStandardDeviation)); } double a22 = 0.0; for (k = 0; k <
		 * gauss2FittingObservations; k++) { double val = (intensity[k] -
		 * secondGaussianMean)/secondGaussianStandardDeviation; a22 += Math.exp(-val *
		 * val); } double b2 = 0; for (k = 0; k < gauss2FittingObservations; k++) {
		 * double diff = (intensity[k] - secondGaussianMean); b2 +=
		 * probDouble[k]*Math.exp(-diff*diff/(2.0*secondGaussianStandardDeviation*
		 * secondGaussianStandardDeviation)); } secondGaussianAmplitude = (b2 -
		 * a21*firstGaussianAmplitude)/a22; double xp1[] = new double[]
		 * {secondGaussianAmplitude, secondGaussianMean, c2}; if (display > 0) {
		 * UI.setDataText("Initial estimates for a2*exp(-((x-b2)/c2)^2)\n");
		 * UI.setDataText("a2 = " + xp1[0] + "\n"); UI.setDataText("b2 = " + xp1[1] +
		 * "\n"); UI.setDataText("c2 = " + xp1[2] + "\n"); }
		 * 
		 * CostFunction cost_function = new gauss1FittingCostFunction(); ProblemImpl
		 * problem = new ProblemImpl(); problem.AddResidualBlock(cost_function, null,
		 * xp1); problem.AddParameterBlock(xp1,3); problem.SetParameterLowerBound(xp1,
		 * 0, 0.1*secondGaussianAmplitude); problem.SetParameterUpperBound(xp1, 0,
		 * 10.0*secondGaussianAmplitude); problem.SetParameterLowerBound(xp1, 1,
		 * Math.max(0.1*secondGaussianMean, firstGaussianMean + 0.5*c1));
		 * problem.SetParameterUpperBound(xp1, 1, Math.min(10.0*secondGaussianMean,
		 * maxSum)); problem.SetParameterLowerBound(xp1, 2, 0.1*c2);
		 * problem.SetParameterUpperBound(xp1, 2, 10.0*c2);
		 * 
		 * // Run the solver! SolverOptions solverOptions = new SolverOptions();
		 * solverOptions.linear_solver_type = LinearSolverType.DENSE_QR;
		 * solverOptions.max_num_consecutive_invalid_steps = 100;
		 * solverOptions.minimizer_progress_to_stdout = true; SolverSummary
		 * solverSummary = new SolverSummary(); Solve(solverOptions, problem,
		 * solverSummary); xp = new double[] {firstGaussianAmplitude, firstGaussianMean,
		 * c1, xp1[0], xp1[1], xp1[2]}; if (xp1[1] < minSum) {
		 * System.err.println("Second gaussian mean = " + xp1[1] +
		 * " < minimum volume_sum value = " + minSum);
		 * System.err.println("Second gaussian amplitude = " + xp1[0]);
		 * System.err.println("Second gaussian c2 = " + xp1[2]);
		 * System.err.println("First gaussian mean was at " + firstGaussianMean);
		 * System.err.println("First gaussian amplitude was at " +
		 * firstGaussianAmplitude); System.err.println("First gaussian c1 was at " +
		 * c1); System.err.println("Initial guess for secondGaussianMean was " +
		 * secondGaussianMean);
		 * System.err.println("Initial guess for secondGaussianAmplitude was " +
		 * secondGaussianAmplitude); System.err.println("Initial guess for c2 was " +
		 * (Math.sqrt(2.0)*secondGaussianStandardDeviation)); System.exit(0); } if
		 * (display > 0) { UI.setDataText(solverSummary.BriefReport() + "\n");
		 * UI.setDataText("Solved answer for a2*exp(-((x-b2)/c2)^2)\n");
		 * UI.setDataText("a2 = " + xp1[0] + "\n"); UI.setDataText("b2 = " + xp1[1] +
		 * "\n"); UI.setDataText("c2 = " + xp1[2] + "\n"); }
		 * 
		 * if (gauss1FittingCheck) { // Ceres Solver Report: Iterations: 7, Initial
		 * cost: 3.846540e+08, Final cost: 3.557932e+08, Termination: CONVERGENCE //
		 * Solved answer for a2*exp(-((x-b2)/c2)^2) // a2 = 3476.2601728215 // b2 =
		 * 54719.31365217361 // c2 = 12820.182141802445 // ******* Elsunc Gauss 1 Curve
		 * Fitting ********* // analyticalJacobian = true // Number of iterations: 7 //
		 * Chi-squared: 7.115862477050933E8 // a0 3473.0758151809737 // a1
		 * 54725.28268304309 // a2 12843.401190193323 // ******* Elsunc Gauss 1 Curve
		 * Fitting ********* // analyticalJacobian = false // Number of iterations: 7 //
		 * Chi-squared: 7.115862477050937E8 // a0 3473.075759678535 // a1
		 * 54725.282764237934 // a2 12843.401729462
		 * 
		 * System.out.println(solverSummary.BriefReport());
		 * System.out.println("Solved answer for a2*exp(-((x-b2)/c2)^2)");
		 * System.out.println("a2 = " + xp1[0]); System.out.println("b2 = " + xp1[1]);
		 * System.out.println("c2 = " + xp1[2]);
		 * 
		 * boolean doAnalytical = true; xp1 = new double[] {secondGaussianAmplitude,
		 * secondGaussianMean, Math.sqrt(2.0)*secondGaussianStandardDeviation};
		 * gauss1Fitting g1 = new gauss1Fitting(xp1, doAnalytical); g1.driver();
		 * g1.dumpResults(); doAnalytical = false; xp1 = new double[]
		 * {secondGaussianAmplitude, secondGaussianMean,
		 * Math.sqrt(2.0)*secondGaussianStandardDeviation}; g1 = new gauss1Fitting(xp1,
		 * doAnalytical); g1.driver(); g1.dumpResults(); System.exit(0); } } // else if
		 * (!doneBackupSecondGaussian) else if (doneBackupSecondGaussian) { xp = new
		 * double[] {firstGaussianAmplitude, firstGaussianMean, c1,
		 * secondGaussianAmplitude, secondGaussianMean, c2}; if (display > 0) {
		 * UI.setDataText("Solved answer for a2*exp(-((x-b2)/c2)^2)\n");
		 * UI.setDataText("a2 = " + xp[3] + "\n"); UI.setDataText("b2 = " + xp[4] +
		 * "\n"); UI.setDataText("c2 = " + xp[5] + "\n"); } }
		 */

		if (doCurveIntersect) {
			curveIntersect(intensity, xp);
			if (x_out == null) {
				System.err.println("x_out == null because curveIntersect found no intersection");
				return;
			}
			mask_threshold = x_out[0];
		} else {
			double parameters[] = new double[] { (xp[1] + xp[4]) / 2.0 };

			GradientProblemSolverOptions options = new GradientProblemSolverOptions();
			options.minimizer_progress_to_stdout = true;

			GradientProblemSolverSummary summary = new GradientProblemSolverSummary();
			GradientProblem gradientProblem = new GradientProblem(new diffGaussians(xp));
			Solve(options, gradientProblem, parameters, summary);
			mask_threshold = parameters[0];
			if ((mask_threshold < minSum) || (mask_threshold <= xp[1]) || (mask_threshold >= xp[4])) {
				//System.err.println("mask_threshold = " + mask_threshold + " < minimum volume_sum value = " + minSum);
				//System.err.println("First gaussian mean was at " + xp[1]);
				//System.err.println("Second gaussian mean was at " + xp[4]);
				//System.exit(0);
				curveIntersect(intensity, xp);
				if (x_out == null) {
					System.err.println("x_out == null because curveIntersect found no intersection");
					return;
				}
				mask_threshold = x_out[0];
			}
			if ((display > 0) && (mask_threshold >= minSum)) {
				System.out.println(summary.BriefReport());
				UI.setDataText("Initial guess for intensity at which 2 Gaussians intersect = " + ((xp[1] + xp[4]) / 2.0)
						+ "\n");
			}
		}
		if (display > 0) {
			UI.setDataText("Final calculation for intensity at which 2 Gaussians intersect = " + mask_threshold + "\n");
		}

		if (display > 1) {
			float probFloat[] = new float[probDouble.length];
			for (i = 0; i < probDouble.length; i++) {
				probFloat[i] = (float) probDouble[i];
			}
			float intensityf[] = new float[intensity.length];
			for (i = 0; i < intensity.length; i++) {
				intensityf[i] = (float) intensity[i];
			}
			double y1[] = new double[intensity.length];
			for (i = 0; i < intensity.length; i++) {
				double val1 = (intensity[i] - xp[1]) / xp[2];
				y1[i] = xp[0] * Math.exp(-val1 * val1);
			}
			double y2[] = new double[intensity.length];
			for (i = 0; i < intensity.length; i++) {
				double val2 = (intensity[i] - xp[4]) / xp[5];
				y2[i] = xp[3] * Math.exp(-val2 * val2);
			}
			float fitf[] = new float[intensity.length];
			for (i = 0; i < intensity.length; i++) {
				fitf[i] = (float) (y1[i] + y2[i]);
			}
			ViewJFrameGraph actualDataGraph = new ViewJFrameGraph(intensityf, probFloat, "Raw data", "Intensity",
					"Amplitudes");
			actualDataGraph.setVisible(true);
			try {
				actualDataGraph.save(outputFilePath + outputPrefix + "actualDataGraph.plt");
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			Component component = actualDataGraph.getComponent(0);
			Rectangle rect = component.getBounds();
			String format = "png";
			BufferedImage captureImage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_ARGB);
			component.paint(captureImage.getGraphics());

			File actualDataGraphFile = new File(outputFilePath + outputPrefix + "actualDataGraph.png");
			try {
				ImageIO.write(captureImage, format, actualDataGraphFile);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			actualDataGraph.removeComponentListener();
			actualDataGraph.dispose();
			captureImage.flush();
			ViewJFrameGraph fittedGaussiansGraph = new ViewJFrameGraph(intensityf, fitf, "2 Gaussians fit", "Intensity",
					"Fitted Gaussians");
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
			int xthresh = (int) Math.round(graphBounds.x + xScale * (mask_threshold - axlim[0]));
			int y1t = (int) Math.round(graphBounds.y + yScale * (0 - axlim[2]));
			y1t = -y1t + 2 * graphBounds.y + graphBounds.height;
			double maxprob = -Double.MAX_VALUE;
			for (i = 0; i < gauss2FittingObservations; i++) {
				if (probDouble[i] > maxprob) {
					maxprob = probDouble[i];
				}
			}
			int y2t = (int) Math.round(graphBounds.y + yScale * (Math.min(1.05 * maxprob, axlim[3]) - axlim[2]));
			y2t = -y2t + 2 * graphBounds.y + graphBounds.height;
			g = graph.getGraphics();
			g.setColor(Color.BLUE);
			graph.drawLine(g, xthresh, y1t, xthresh, y2t);
			graph.plotGraph(g);
			try {
				fittedGaussiansGraph.save(outputFilePath + outputPrefix + "fittedGaussiansGraph.plt");
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			component = actualDataGraph.getComponent(0);
			rect = component.getBounds();
			format = "png";
			captureImage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_ARGB);
			component.paint(captureImage.getGraphics());

			File fittedGaussiansGraphFile = new File(outputFilePath + outputPrefix + "fittedGaussiansGraph.png");
			try {
				ImageIO.write(captureImage, format, fittedGaussiansGraphFile);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			fittedGaussiansGraph.removeComponentListener();
			fittedGaussiansGraph.dispose();
			captureImage.flush();
		} // if (display > 1)
		mask_aif = new byte[nC][nR][nS];
		mask_data = new byte[nC][nR][nS];
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				for (z = 0; z < nS; z++) {
					if (volume_sum[x][y][z] > mask_threshold) {
						mask_aif[x][y][z] = 1;
					}
				}
			}
		}
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				for (z = 0; z < nS; z++) {
					if (volume_sum[x][y][z] > mask_threshold) {
						mask_aif[x][y][z] = 1;
					}
				}
			}
		}

		byte temp[] = new byte[length];
		ModelImage idImage = new ModelImage(ModelStorageBase.BYTE, extents2D, "idImage");
		ModelImage tempImage = null;
		if (display > 1) {
			tempImage = new ModelImage(ModelStorageBase.BYTE, extents2D, "tempImage");
		}
		System.out.println("nS = " + nS);
		for (s = 0; s < nS; s++) {
			System.out.println("s = " + s);
			// I cover any "holes" created by thresholding
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					temp[x + y * nC] = mask_aif[x][y][s];
				}
			}
			// I delete the minor connected components and leave the major ones of
			// options.mask.pixel intact
			try {
				idImage.importData(0, temp, true);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}

			int numPruningPixels = 0;
			int edgingType = 0;
			int kernel = AlgorithmMorphology2D.CONNECTED8;
			float circleDiameter = 0.0f;
			int method = AlgorithmMorphology2D.ID_OBJECTS;
			int itersDilation = 0;
			int itersErosion = 0;
			boolean wholeImage = true;
			AlgorithmMorphology2D idObjectsAlgo2D = new AlgorithmMorphology2D(idImage, kernel, circleDiameter, method,
					itersDilation, itersErosion, numPruningPixels, edgingType, wholeImage);
			idObjectsAlgo2D.setMinMax(1, Integer.MAX_VALUE);
			idObjectsAlgo2D.run();
			idObjectsAlgo2D.finalize();
			idObjectsAlgo2D = null;

			idImage.calcMinMax();
			int numObjects = (int) idImage.getMax();

			byte IDArray[] = new byte[length];

			try {
				idImage.exportData(0, length, IDArray);
			} catch (IOException error) {
				System.err.println("IOException " + error);
				return;
			}

			int objectSize[] = new int[numObjects + 1];
			for (i = 0; i < length; i++) {
				objectSize[IDArray[i]]++;
			}
			int maxObjectSize = 0;
			for (i = 1; i <= numObjects; i++) {
				if (objectSize[i] > maxObjectSize) {
					maxObjectSize = objectSize[i];
				}
			}

			for (i = 1; i <= numObjects; i++) {
				if ((maxObjectSize > objectSize[i]) && (objectSize[i] < mask_npixel)) {
					for (j = 0; j < length; j++) {
						if (IDArray[j] == i) {
							temp[j] = 0;
						}
					}
				}
			} // for (i = 1; i <= numObjects; i++)

			try {
				idImage.importData(0, temp, true);
			} catch (IOException error) {
				System.err.println("IOException " + error);
				return;
			}

			AlgorithmMorphology2D fillHolesAlgo2D = new AlgorithmMorphology2D(idImage, 0, 0,
					AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0, 0, wholeImage);
			fillHolesAlgo2D.run();
			fillHolesAlgo2D.finalize();
			fillHolesAlgo2D = null;

			try {
				idImage.exportData(0, length, IDArray);
			} catch (IOException error) {
				System.err.println("IOException " + error);
				return;
			}

			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					mask_data[x][y][s] = IDArray[x + y * nC];
				}
			}

			if ((display > 2) || ((display > 1) && (s == (int) Math.round(0.5 * nS) - 1))) {
				try {
					tempImage.importData(0, temp, true);
				} catch (IOException error) {
					System.err.println("IOException " + error);
					return;
				}
				AlgorithmVOIExtraction algoVOIExtraction = new AlgorithmVOIExtraction(tempImage);
				algoVOIExtraction.run();
				algoVOIExtraction.finalize();
				algoVOIExtraction = null;

				ModelImage volume_sum_sliceImage = new ModelImage(ModelStorageBase.DOUBLE, extents2D,
						"volume_sum_sliceImage_" + s);
				volume_sum_sliceImage.setVOIs(tempImage.getVOIs());
				tempDouble = new double[length];
				for (x = 0; x < nC; x++) {
					for (y = 0; y < nR; y++) {
						tempDouble[x + y * nC] = volume_sum[x][y][s];
					}
				}
				try {
					volume_sum_sliceImage.importData(0, tempDouble, true);
				} catch (IOException e) {
					System.err.println("IOException " + e);
					return;
				}
				ViewJFrameImage vFrame = new ViewJFrameImage(volume_sum_sliceImage);
				Component component = vFrame.getComponent(0);
				Rectangle rect = component.getBounds();
				String format = "png";
				BufferedImage captureImage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_ARGB);
				component.paint(captureImage.getGraphics());

				File volume_sum_sliceFile = new File(
						outputFilePath + outputPrefix + "maskedDataForAIFSelection_" + s + ".png");
				boolean foundWriter;
				try {
					foundWriter = ImageIO.write(captureImage, format, volume_sum_sliceFile);
				} catch (IOException e) {
					System.err.println("IOException " + e);
					return;
				}
				if (!foundWriter) {
					System.err.println("No appropriate writer for maskedDataForAIFSelection_" + s + ".png");
					return;
				}
				captureImage.flush();
				component.setEnabled(false);
				component.setVisible(false);
				component.setIgnoreRepaint(true);
				vFrame.removeComponentListener();
				vFrame.removeWindowListener();
				vFrame.removeMouseMotionListener();
				vFrame.removeMouseListener();
				vFrame.removeKeyListener();
				vFrame.close(false);
				volume_sum_sliceImage.disposeLocal();

				algoVOIExtraction = new AlgorithmVOIExtraction(idImage);
				algoVOIExtraction.run();
				algoVOIExtraction.finalize();
				algoVOIExtraction = null;
				volume_sum_sliceImage = new ModelImage(ModelStorageBase.DOUBLE, extents2D,
						"volume_sum_sliceImage_" + s);
				try {
					volume_sum_sliceImage.importData(0, tempDouble, true);
				} catch (IOException e) {
					System.err.println("IOException " + e);
					return;
				}
				volume_sum_sliceImage.setVOIs(idImage.getVOIs());
				vFrame = new ViewJFrameImage(volume_sum_sliceImage);
				component = vFrame.getComponent(0);
				rect = component.getBounds();
				format = "png";
				captureImage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_ARGB);
				component.paint(captureImage.getGraphics());

				volume_sum_sliceFile = new File(outputFilePath + outputPrefix + "maskedData_" + s + ".png");
				try {
					foundWriter = ImageIO.write(captureImage, format, volume_sum_sliceFile);
				} catch (IOException e) {
					System.err.println("IOException " + e);
					return;
				}
				if (!foundWriter) {
					System.err.println("No appropriate writer for maskedData_" + s + ".png");
					return;
				}
				captureImage.flush();
				component.setEnabled(false);
				component.setVisible(false);
				component.setIgnoreRepaint(true);
				vFrame.removeComponentListener();
				vFrame.removeWindowListener();
				vFrame.removeMouseMotionListener();
				vFrame.removeMouseListener();
				vFrame.removeKeyListener();
				vFrame.close(false);
				volume_sum_sliceImage.disposeLocal();
			} // if ((display > 2) || ((display > 1) && (s == (int)Math.round(0.5 * nS)-1)))
		} // for (s = 0; s < nS; s++)
		System.out.println("Finished s loop");
		idImage.disposeLocal();
		idImage = null;
		if (tempImage != null) {
			tempImage.disposeLocal();
			tempImage = null;
		}

		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				for (z = 0; z < nS; z++) {
					mask_aif[x][y][z] = (byte) (mask_aif[x][y][z] * mask_data[x][y][z]);
				}
			}
		}
		
		if (doSaveAllOutputs) {
			byte maskBuffer[] = new byte[volume];
			ModelImage mask_aifImage = new ModelImage(ModelStorageBase.BYTE, extents3D, "mask_aif");
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					for (z = 0; z < nS; z++) {
						maskBuffer[x + y*nC + z*length] = mask_aif[x][y][z];
					}
				}
			}
			try {
	    		mask_aifImage.importData(0, maskBuffer, true);
	    	}
	    	catch (IOException e) {
	    		MipavUtil.displayError("IOException on mask_aifImage");
	    		//setCompleted(false);
	    		return;
	    	}
			FileInfoBase fileInfo[] = mask_aifImage.getFileInfo();
	    	for (i = 0; i < nS; i++) {
	    		fileInfo[i].setResolutions(resolutions3D);
	    		fileInfo[i].setUnitsOfMeasure(units3D);
	    		fileInfo[i].setDataType(ModelStorageBase.BYTE);
	    	}
	    	
	    	saveImageFile(mask_aifImage, outputFilePath, outputPrefix + "mask_aif", saveFileFormat);
	    	mask_aifImage.disposeLocal();
	    	mask_aifImage = null;
	    	
	    	ModelImage mask_dataImage = new ModelImage(ModelStorageBase.BYTE, extents3D, "mask_data");
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					for (z = 0; z < nS; z++) {
						maskBuffer[x + y*nC + z*length] = mask_data[x][y][z];
					}
				}
			}
			try {
	    		mask_dataImage.importData(0, maskBuffer, true);
	    	}
	    	catch (IOException e) {
	    		MipavUtil.displayError("IOException on mask_dataImage");
	    		//setCompleted(false);
	    		return;
	    	}
			fileInfo = mask_dataImage.getFileInfo();
	    	for (i = 0; i < nS; i++) {
	    		fileInfo[i].setResolutions(resolutions3D);
	    		fileInfo[i].setUnitsOfMeasure(units3D);
	    		fileInfo[i].setDataType(ModelStorageBase.BYTE);
	    	}
	    	
	    	saveImageFile(mask_dataImage, outputFilePath, outputPrefix + "mask_data", saveFileFormat);
	    	mask_dataImage.disposeLocal();
	    	mask_dataImage = null;
		} // if (doSaveAllOutputs)
	} // public void DSC_mri_mask()

	private void backupSecondGaussianRoutine() {
		int i;
		secondGaussianMeanBin = -1;
		secondGaussianAmplitude = -Double.MAX_VALUE;
		for (i = firstGaussianMeanBin + 4; i < intensity.length; i++) {
			if ((probDouble[i] > secondGaussianAmplitude) && (probDouble[i] > probDouble[i - 3])) {
				secondGaussianAmplitude = probDouble[i];
				secondGaussianMeanBin = i;
			}
		}
		secondGaussianMean = intensity[secondGaussianMeanBin];
		UI.setDataText("Second Gaussian amplitude = " + secondGaussianAmplitude + "\n");
		UI.setDataText("Second Gaussian mean = " + secondGaussianMean + "\n");
		// Calculate c2 from amplitude of best fit of highest channel and next 3
		// channels to the right

		// Initial estimate of c1 = sqrt(2) * standard deviation
		double xp1[] = new double[1];
		double ratio = probDouble[secondGaussianMeanBin + 3] / probDouble[secondGaussianMeanBin];
		double diff = (intensity[secondGaussianMeanBin + 3] - secondGaussianMean);
		// exp(-((x-mean)/c1)^2) = ratio
		// -(intensity[secondGaussianMeanBin+3] - secondGaussianMean)^2/(c2*c2) =
		// ln(ratio)
		xp1[0] = Math.sqrt(-(diff * diff) / Math.log(ratio));
		CostFunction cost_function1 = new gaussSecondStandardDeviationFittingCostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function1, null, xp1);

		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.linear_solver_type = LinearSolverType.DENSE_QR;
		solverOptions.max_num_consecutive_invalid_steps = 100;
		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		c2 = xp1[0];
		secondGaussianStandardDeviation = Math.sqrt(2.0) * c2;
		if (display > 0) {
			UI.setDataText(solverSummary.BriefReport() + "\n");
			UI.setDataText("Solved answer for secondGaussianAmplitude*exp(-((x-secondGaussianMean)/c2)^2)\n");
			UI.setDataText("c2 = " + xp1[0] + "\n");
		}
	}

	public void DSC_mri_conc() {
		// Original MATLAB version last modified by Denis Peruzzo 07/06/2010
		// Author for original MATLAB version: Marco Castellaro - Universit di Padova -
		// DEI
		//
		// Calculate the map of concentrations of S0 in DSC-MRI exams.
		//
		// Input parameters:
		// volumes (4D matrix) which contains the trends of the DSC signal of all the
		// voxels
		// mask (3D matrix) contains the matrix for masking the voxels not of interest
		// for the study
		//
		// Options The structure that contains the method options, the signficiant ones
		// are:
		//
		// par_kvoi - Proportionality constant for the calculation of the tracer
		// concentration in the VOI,
		// by default considered unknown and set to 1.
		//
		// S0 Series of parameters to identify the calculation threshold of S0.
		// S0_nSamplesMin - n of the samples that I consider definitely acquired before
		// injection
		// S0_nSamplesMax - n of the samples after which I stop anyway.
		// S0_thresh - I add a sample if its diefference from the mean is less than
		// threshold
		//
		// display - Level 1 shows the processing progress
		// Level 2 shows the maps of S0 and the average signal on which it is estimated.
		//
		// Output parameters:
		// conc: 4D matrix of concentrations
		// S0map 3D S0 matrix
		int x, y, z, t, i;

		if (display > 0) {
			UI.setDataText("Calculating concentration...\n");
		}

		DSC_mri_S0();

		conc = new double[nC][nR][nS][nT];
		// Note that if volumes[x][y][z][t] = 0 that step1 = 0 and conc[x][y][z][t] =
		// infinity
		// The MATLAB DSC_mri_aif.m code has:
		// [aif_old]=estraiAIF(reshape(conc(:,:,options.aif.nSlice,:),options.nR,options.nC,options.nT),mask(:,:,options.aif.nSlice),options);
		// function [AIF]=estraiAIF(AIFslice,mask,options)
		// AUC=sum(AIFslice,3); % calcolo l'AUC di ogni voxel.
		// AUC=AUC.*ROI;
		// where ROI is always 0 or 1
		// AUC(isinf(AUC))=0;
		// This would only be the case if an infinity in conc passes an infinity to AUC.
		// So the original MATLAB code expects conc[x][y][z][t] to be set to infinity
		// for volumes[x][y][z][t] = 0
		double step1;
		for (t = 0; t < nT; t++) {
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					for (z = 0; z < nS; z++) {
						if (mask_data[x][y][z] == 1) {
							step1 = volumes[x][y][z][t] / S0map[x][y][z];
							conc[x][y][z][t] = -(par_kvoi / te) * Math.log(step1);
						}
					}
				}
			}
		} // for (t = 0; t < nT; t++)
		
		if (doSaveAllOutputs) {
			double concBuffer[] = new double[volume*nT];
			ModelImage concImage = new ModelImage(ModelStorageBase.DOUBLE, extents4D, "conc");
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					for (z = 0; z < nS; z++) {
						for (t = 0; t < nT; t++) {
						    concBuffer[x + y*nC + z*length + t*volume] = conc[x][y][z][t];
						}
					}
				}
			}
			try {
	    		concImage.importData(0, concBuffer, true);
	    	}
	    	catch (IOException e) {
	    		MipavUtil.displayError("IOException on mask_concImage");
	    		//setCompleted(false);
	    		return;
	    	}
			FileInfoBase fileInfo[] = concImage.getFileInfo();
	    	for (i = 0; i < nS*nT; i++) {
	    		fileInfo[i].setResolutions(resolutions4D);
	    		fileInfo[i].setUnitsOfMeasure(units4D);
	    		fileInfo[i].setDataType(ModelStorageBase.DOUBLE);
	    	}
	    	
	    	saveImageFile(concImage, outputFilePath, outputPrefix + "conc", saveFileFormat);
	    	concImage.disposeLocal();
	    	concImage = null;
		} // if (doSaveAllOutputs)
	} // public void DSC_mri_conc()

	public void DSC_mri_S0() {
		// Author of original MATLAB version Marco Castellaro - Universit di Padova -
		// DEI
		//
		// The function calculates the bolus instant and the S0 from the data.
		// 1.) On the average trend I calculate the inection of the bolus. I calculate
		// the
		// average of the first n samples and add the n+1 if its percentage difference
		// from
		// the average is less than a given threshold.
		// 2.) Calculate S0 as the mean of the first n samples for all voxels.
		//
		// Definition of parameters
		// n of the samples that I consider definitely acquired before injection
		int nSamplesMin = S0_nSamplesMin;
		// n of samples after which I stop anyway
		int nSamplesMax = S0_nSamplesMax;
		// I add a sample if its difference from the mean is less than threshold
		double thresh = S0_thresh;
		//
		// 1.) Calculation of the moment of injection of the bolus.
		// 1.1) Calculation of the average trend
		int i, x, y, s, t;
		boolean cycle;
		int pos;
		double mean_val;

		double mean_signal[][] = new double[nS][nT];
		double signalSum;
		int signalNumber;
		double valSum;
		for (s = 0; s < nS; s++) {
			for (t = 0; t < nT; t++) {
				signalSum = 0.0;
				signalNumber = 0;
				for (x = 0; x < nC; x++) {
					for (y = 0; y < nR; y++) {
						if (mask_data[x][y][s] == 1) {
							signalSum += volumes[x][y][s][t];
							signalNumber++;
						}
					}
				}
				if (signalNumber > 0) {
					mean_signal[s][t] = signalSum / signalNumber;
				}
			}
		} // for (s = 0; s < nS; s++)

		S0map = new double[nC][nR][nS];
		bolus = new int[nS];
		double S0mapSum;
		for (s = 0; s < nS; s++) {
			// 1.2) calculation of the moment of injection of the bolus
			cycle = true;
			pos = nSamplesMin;
			while (cycle) {
				valSum = 0.0;
				for (t = 0; t < pos; t++) {
					valSum += mean_signal[s][t];
				}
				mean_val = valSum / pos;
				if (Math.abs((mean_val - mean_signal[s][pos]) / mean_val) < thresh) {
					pos = pos + 1;
				} else {
					cycle = false;
					// Conservative choice, I do not consider the last sample before injection
					pos = pos - 1;
				}
				if (pos >= nSamplesMax) {
					cycle = false;
					pos = pos - 1;
				}
			} // while (cycle)

			if ((display > 2) || ((s == (int) Math.round(0.5 * nS) - 1) && (display > 1))) {
				float timef[] = new float[nT];
				for (i = 0; i < nT; i++) {
					timef[i] = (float) time[i];
				}
				float mean_signalf[] = new float[nT];
				for (i = 0; i < nT; i++) {
					mean_signalf[i] = (float) mean_signal[s][i];
				}
				ViewJFrameGraph meanSignalGraph = new ViewJFrameGraph(timef, mean_signalf,
						"S0 computed from first " + pos + " samples", "Time", "Mean Signal slice " + s);
				meanSignalGraph.setVisible(true);
				try {
					meanSignalGraph.save(outputFilePath + outputPrefix + "meanSignal_" + s + "Graph.plt");
				} catch (IOException e) {
					System.err.println("IOException " + e);
					return;
				}
				Component component = meanSignalGraph.getComponent(0);
				Rectangle rect = component.getBounds();
				String format = "png";
				BufferedImage captureImage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_ARGB);
				component.paint(captureImage.getGraphics());

				File meanSignalGraphFile = new File(outputFilePath + outputPrefix + "meanSignal_" + s + "Graph.png");
				try {
					ImageIO.write(captureImage, format, meanSignalGraphFile);
				} catch (IOException e) {
					System.err.println("IOException " + e);
					return;
				}
				meanSignalGraph.removeComponentListener();
				meanSignalGraph.dispose();
				captureImage.flush();
			} // if ((display > 2) || ((s == (int)Math.round(0.5*nS)-1) && (display > 1)))

			// 2.) Calculation of S0
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					if (mask_data[x][y][s] == 1) {
						S0mapSum = 0.0;
						for (t = 0; t < pos; t++) {
							S0mapSum += volumes[x][y][s][t];
						}
						S0map[x][y][s] = S0mapSum / pos;
					}
				}
			}

			if ((display > 2) || ((s == (int) Math.round(0.5 * nS) - 1) && (display > 1))) {
				ModelImage S0mapImage = new ModelImage(ModelStorageBase.DOUBLE, extents2D, "S0mapImage");
				double tempDouble[] = new double[length];
				for (x = 0; x < nC; x++) {
					for (y = 0; y < nR; y++) {
						tempDouble[x + y * nC] = S0map[x][y][s];
					}
				}
				try {
					S0mapImage.importData(0, tempDouble, true);
				} catch (IOException e) {
					System.err.println("IOException " + e);
					return;
				}
				saveImageFile(S0mapImage, outputFilePath, outputPrefix + "S0map_" + s, saveFileFormat);
				S0mapImage.disposeLocal();
			} // if ((display > 2) || ((s == (int)Math.round(0.5*nS)-1) && (display > 1)))
			bolus[s] = pos;
		} // for (s = 0; s < nS; s++)
	} // public void DSC_mri_S0()

	public void DSC_mri_aif() {
		// Last modification of original MATLAB code: Denis Peruzzo 08/06/2010
		// Author of original MATLAB code: Denis Peruzzo - Universit di Padova - DEI
		//
		// Locate the AIF for the DSC-MRI exam. The method is designed to identify the
		// middle
		// cerebral artery (MCA) in the slices immediately superior to the corpus
		// callosum.
		//
		// Input parameters:
		// conc (4D matrix) which contains the concentration trends of all the voxels
		// mask_aif (3D matrix) contains the mask of each slice. The mask is not the one
		// used
		// for the calculation of concentrations, but a restricted version of it.
		// (The fill function was not used.)
		// options The structure that contains the options of the method, the
		// significant
		// ones are:
		// aif_enable: flag that enables the AIF search (default = 1).
		//
		// aif_semiMajorAxis: Allows you to identify the AIF search area. The elliptical
		// search zone
		// has the semi major axis greater than or equal to twice the portion
		// indicated by this option of the size of the brain (default = 0.35)
		//
		// aif_semiMinorAxis: Like the semi major axis, but realtive to the other
		// semiaxis of the
		// search region (default = 0.15)
		//
		// aif_pArea: Percentage of candidate voxels based on area under the curve
		//
		// aif_pTTP: Percentage of candidate voxels excluded on basis of time to peak
		// (default = 0.4)
		//
		// aif_pReg: Percentage of candidate voxels excluded based on irregularity of
		// the curve (default = 0.05)
		//
		// aif_diffPeak: 0.0400
		//
		// aif_nVoxelMax: Maximum number of arterial voxels accepted (default = 6).
		//
		// aif_nVoxelMin: Minimum number of arterial voxels accepted (default = 4)
		//
		// Output parameters: AIF structure, which contains
		// - ROI
		int i, x, y, t;

		if (display > 0) {
			UI.setDataText("AIF extraction...\n");
		}

		// AIF slice selection
		if ((aif_nSlice < 0) || (aif_nSlice >= nS)) {
			aif_nSlice = DSC_mri_slice_selection_figure();
		}

		AIFslice = new double[nC][nR][nT];
		mask_aif_slice = new byte[nC][nR];
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				mask_aif_slice[x][y] = mask_aif[x][y][aif_nSlice];
				for (t = 0; t < nT; t++) {
					AIFslice[x][y][t] = conc[x][y][aif_nSlice][t];
				}
			}
		}
		extractAIF();
		
		if (qr_enable == 1) {
	        for (t = 0; t < nT; t++) {
	            AIF_conc[t] =(qr_a*AIF_conc[t]+qr_b*AIF_conc[t]*AIF_conc[t])/qr_r;
	        }
		} // if (qr_enable == 1)
	} // public DSC_mri_aif()

	public int DSC_mri_slice_selection_figure() {
		return -1;
	} // public int DSC_mri_slice_selection_figure()

	public void extractAIF() {
		// Extract the AIF from the supplied slice
		// 1.) Find the region containing the AIF
		// 2.) Decimation of the candidate voxel
		// 3.) Apply the hierarchical cluster algorithm to locate arterial voxels
		// 4.) Prepare the output
		double semiMajorAxis;
		double semiMinorAxis;
		double pArea;
		double pTTP;
		double pReg;
		int nVoxelMax;
		int nVoxelMin;
		double diffPeak;
		byte mask[][] = new byte[nC][nR];
		double immagine_img[][] = new double[nC][nR];
		int x, y, t;
		double vettImmagine[];
		double immagine_bound[] = new double[2];
		boolean cycle;
		int r;
		int minR = 0;
		int maxR = nR - 1;
		int c;
		int minC = 0;
		int maxC = nC - 1;
		int maskSum;
		double center[] = new double[2];
		double semiAxisA;
		double semiAxisB;
		byte ROI[][] = new byte[nC][nR];
		double rdiff;
		double cdiff;
		byte ROIInitialized[][] = new byte[nC][nR];
		double xROI[];
		int nL;
		int i;
		double val;
		double yROI[];
		int k;
		double diffX;
		int totalCandidates;
		int totalCandidatesToKeep;
		double AUC[][] = new double[nC][nR];
		int nCycles;
		double AUCdown;
		double AUCup;
		double threshold = 0.0;
		int nCandidates;
		byte ROIauc[][] = new byte[nC][nR];
		int ROISum;
		short TTP[][] = new short[nC][nR];
		double AIFsliceMaxValue;
		int AIFsliceTIndex;
		int numTTPEqualMinus1;
		int numTTPLessThreshold;
		byte ROIttp[][] = new byte[nC][nR];
		int survivedVoxels;
		double REG[][];
		double REGdown;
		double REGup;
		byte ROIreg[][] = new byte[nC][nR];
		double dati2D[][];
		byte maskAIF[][] = new byte[nC][nR];
		int nCluster;
		double centroidi[][];
		int selectedCluster = 0;
		int otherCluster = 1;
		int j;

		// Preparation of accessory variables and parameters
		semiMajorAxis = aif_semiMajorAxis;
		semiMinorAxis = aif_semiMinorAxis;
		pArea = aif_pArea;
		pTTP = aif_pTTP;
		pReg = aif_pReg;
		nVoxelMax = aif_nVoxelMax;
		nVoxelMin = aif_nVoxelMin;
		diffPeak = aif_diffPeak;
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				mask[x][y] = mask_aif_slice[x][y];
				for (t = 0; t < nT; t++) {
					immagine_img[x][y] += AIFslice[x][y][t];
				}
			}
		}

		// I prepare the image for any visualizations
		vettImmagine = new double[length];
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				vettImmagine[x + y * nC] = immagine_img[x][y];
			}
		}
		Arrays.sort(vettImmagine);
		immagine_bound[0] = 0.0;
		immagine_bound[1] = vettImmagine[(int) Math.round(0.95 * length) - 1];
		vettImmagine = null;

		// 1.) Identification of the ROI containing the AIF
		// 1.1) Identification of extremes of the mask
		if (display > 0) {
			UI.setDataText(" Brain bound detection\n");
		}
		cycle = true;
		r = 0;
		while (cycle) {
			maskSum = 0;
			for (x = 0; x < nC; x++) {
				maskSum += mask[x][r];
			}
			if (maskSum != 0) {
				minR = r;
				cycle = false;
			} else {
				r = r + 1;
			}
		} // while(cycle)
		cycle = true;
		r = nR - 1;
		while (cycle) {
			maskSum = 0;
			for (x = 0; x < nC; x++) {
				maskSum += mask[x][r];
			}
			if (maskSum != 0) {
				maxR = r;
				cycle = false;
			} else {
				r = r - 1;
			}
		} // while (cycle)

		cycle = true;
		c = 0;
		while (cycle) {
			maskSum = 0;
			for (y = 0; y < nR; y++) {
				maskSum += mask[c][y];
			}
			if (maskSum != 0) {
				minC = c;
				cycle = false;
			} else {
				c = c + 1;
			}
		} // while (cycle)
		cycle = true;
		c = nC - 1;
		while (cycle) {
			maskSum = 0;
			for (y = 0; y < nR; y++) {
				maskSum += mask[c][y];
			}
			if (maskSum != 0) {
				maxC = c;
				cycle = false;
			} else {
				c = c - 1;
			}
		} // while (cycle)

		if (display > 2) {
			ModelImage mask_aif_sliceImage = new ModelImage(ModelStorageBase.BYTE, extents2D, "mask_aif_sliceImage");
			byte tempByte[] = new byte[length];
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					tempByte[x + y * nC] = mask[x][y];
				}
			}
			try {
				mask_aif_sliceImage.importData(0, tempByte, true);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			float xArr[] = new float[2];
			float yArr[] = new float[2];
			float zArr[] = new float[2];
			xArr[0] = 0;
			xArr[1] = nC - 1;
			yArr[0] = minR;
			yArr[1] = minR;
			VOI boundLowYVOI = new VOI((short) 0, "boundLowYLine", VOI.LINE, -1.0f);
			boundLowYVOI.importCurve(xArr, yArr, zArr);
			mask_aif_sliceImage.registerVOI(boundLowYVOI);
			boundLowYVOI.setFixed(true);
			boundLowYVOI.setColor(Color.green);
			yArr[0] = maxR;
			yArr[1] = maxR;
			VOI boundHighYVOI = new VOI((short) 0, "boundHighYLine", VOI.LINE, -1.0f);
			boundHighYVOI.importCurve(xArr, yArr, zArr);
			mask_aif_sliceImage.registerVOI(boundHighYVOI);
			boundHighYVOI.setFixed(true);
			boundHighYVOI.setColor(Color.green);
			xArr[0] = minC;
			xArr[1] = minC;
			yArr[0] = 0;
			yArr[1] = nR - 1;
			VOI boundLowXVOI = new VOI((short) 0, "boundLowXLine", VOI.LINE, -1.0f);
			boundLowXVOI.importCurve(xArr, yArr, zArr);
			mask_aif_sliceImage.registerVOI(boundLowXVOI);
			boundLowXVOI.setFixed(true);
			boundLowXVOI.setColor(Color.green);
			xArr[0] = maxC;
			xArr[1] = maxC;
			VOI boundHighXVOI = new VOI((short) 0, "boundHighXLine", VOI.LINE, -1.0f);
			boundHighXVOI.importCurve(xArr, yArr, zArr);
			mask_aif_sliceImage.registerVOI(boundHighXVOI);
			boundHighXVOI.setFixed(true);
			boundHighXVOI.setColor(Color.green);
			ViewJFrameImage vFrame = new ViewJFrameImage(mask_aif_sliceImage);
			Component component = vFrame.getComponent(0);
			Rectangle rect = component.getBounds();
			String format = "png";
			BufferedImage captureImage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_ARGB);
			component.paint(captureImage.getGraphics());

			File mask_aif_sliceFile = new File(outputFilePath + outputPrefix + "mask_aif_slice.png");
			boolean foundWriter;
			try {
				foundWriter = ImageIO.write(captureImage, format, mask_aif_sliceFile);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			if (!foundWriter) {
				System.err.println("No appropriate writer for mask_aif_slice.png");
				return;
			}
			captureImage.flush();
			component.setEnabled(false);
			component.setVisible(false);
			component.setIgnoreRepaint(true);
			vFrame.removeComponentListener();
			vFrame.removeWindowListener();
			vFrame.removeMouseMotionListener();
			vFrame.removeMouseListener();
			vFrame.removeKeyListener();
			vFrame.close(false);
			mask_aif_sliceImage.disposeLocal();
		} // if (display > 2)

		// 1.2) ROI design
		if (display > 0) {
			UI.setDataText("Definition of the AIF extraction searching area\n");
		}
		
		// The semimajor axis. Along the anterior-posterior direction and then from
		// left to right on the image.
		semiAxisB = semiMajorAxis * (maxC - minC);
		semiAxisA = semiMinorAxis * (maxR - minR);
		// Y coordinate of the center (calculated on the rows)
		center[1] = 0.5 * (minR + maxR) - semiAxisA;
		// X coordinate of the center (calculated on the columns)
		center[0] = 0.5 * (minC + maxC);

		
		for (r = 0; r < nR; r++) {
			for (c = 0; c < nC; c++) {
				rdiff = r - center[1];
				cdiff = c - center[0];
				if (((rdiff * rdiff) / (semiAxisA * semiAxisA) + (cdiff * cdiff) / (semiAxisB * semiAxisB)) <= 1.0) {
					ROI[c][r] = 1;
				}
				
			}
		}
		// Of the values of the ROI I keep only those also present in the mask
		for (r = 0; r < nR; r++) {
			for (c = 0; c < nC; c++) {
				ROI[c][r] = (byte) (ROI[c][r] * mask[c][r]);
				ROIInitialized[c][r] = ROI[c][r];
			}
		}

		nL = (int) (2 * semiAxisB / 0.01) + 1;
		xROI = new double[2 * nL];
		for (i = 0; i < nL; i++) {
			xROI[i] = center[0] - semiAxisB + 0.01*i;
		}
		xROI[2*nL-1] = 0.0;

		yROI = new double[2 * nL];
		for (k = 0; k < nL; k++) {
			diffX = xROI[k] - center[0];
			yROI[k] = semiAxisA * (Math.sqrt(Math.max(0.0, (1 - (diffX * diffX) / (semiAxisB * semiAxisB)))))
					+ center[1];
		}
		for (k = 1; k <= nL; k++) {
			xROI[nL + k - 1] = xROI[nL - k];

		    diffX = xROI[nL + k -1] - center[0];
		    yROI[nL + k - 1] = -semiAxisA * (Math.sqrt(Math.max(0.0, (1 - (diffX * diffX) / (semiAxisB * semiAxisB)))))
				+ center[1];
		}

		if (display >= 2) {
			vettImmagine = new double[length];
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					vettImmagine[x + y * nC] = immagine_img[x][y];
				}
			}
			ModelImage immagineImage = new ModelImage(ModelStorageBase.DOUBLE, extents2D, "immagineImage");
			VOIVector voiVector = immagineImage.getVOIs();
			try {
				immagineImage.importData(0, vettImmagine, true);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			float xArr[] = new float[1];
			float yArr[] = new float[1];
			float zArr[] = new float[1];
			VOI ellipseVOI = new VOI((short)0, "ellipseVOI", VOI.CONTOUR, 0.0f);
		    Vector3f ellipsePt[] = new Vector3f[2*nL];
			for (i = 0; i < 2*nL; i++) {
				 ellipsePt[i] = new Vector3f((float)xROI[i], (float)yROI[i], 0.0f);
			}
			ellipseVOI.importCurve(ellipsePt);
			voiVector.add(ellipseVOI);
			VOI centerPtVOI = new VOI((short) (2 * nL), "center", VOI.POINT, -1.0f);
			centerPtVOI.setColor(Color.green);
			xArr[0] = (float) center[0];
			yArr[0] = (float) center[1];
			centerPtVOI.importCurve(xArr, yArr, zArr);
			((VOIPoint) (centerPtVOI.getCurves().elementAt(0))).setFixed(true);
			voiVector.add(centerPtVOI);
			ViewJFrameImage vFrame = new ViewJFrameImage(immagineImage);
			Component component = vFrame.getComponent(0);
			Rectangle rect = component.getBounds();
			String format = "png";
			BufferedImage captureImage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_ARGB);
			component.paint(captureImage.getGraphics());

			File immagineFile = new File(outputFilePath + outputPrefix + "ellipse.png");
			boolean foundWriter;
			try {
				foundWriter = ImageIO.write(captureImage, format, immagineFile);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			if (!foundWriter) {
				System.err.println("No appropriate writer for ellipse.png");
				return;
			}
			captureImage.flush();
			component.setEnabled(false);
			component.setVisible(false);
			component.setIgnoreRepaint(true);
			vFrame.removeComponentListener();
			vFrame.removeWindowListener();
			vFrame.removeMouseMotionListener();
			vFrame.removeMouseListener();
			vFrame.removeKeyListener();
			vFrame.close(false);
			immagineImage.disposeLocal();
		} // if (display >= 2)

		// 2.) Decimation of candidate voxels
		if (display > 0) {
			UI.setDataText("Candidate voxel analysis\n");
		}
		// 2.1) Selection due to area under the curve
		totalCandidates = 0;
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				totalCandidates += ROI[x][y];
			}
		}
		totalCandidatesToKeep = (int) Math.ceil(totalCandidates * (1.0 - pArea));
		// I calculate the area under the curve of each voxel.
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				for (t = 0; t < nT; t++) {
					AUC[x][y] += AIFslice[x][y][t];
				}
				AUC[x][y] = AUC[x][y] * ROI[x][y];
				if (Double.isInfinite(AUC[x][y])) {
					AUC[x][y] = 0.0;
				}
			}
		}

		cycle = true;
		nCycles = 0;
		AUCdown = Double.MAX_VALUE;
		AUCup = -Double.MAX_VALUE;
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				if (AUC[x][y] < AUCdown) {
					AUCdown = AUC[x][y];
				}
				if (AUC[x][y] > AUCup) {
					AUCup = AUC[x][y];
				}
			}
		}
		while (cycle) {
			nCycles = nCycles + 1;
			threshold = 0.5 * (AUCup + AUCdown);
			nCandidates = 0;
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					if (AUC[x][y] > threshold) {
						nCandidates++;
					}
				}
			}

			if (nCandidates == totalCandidatesToKeep) {
				cycle = false;
			} else if (nCandidates > totalCandidatesToKeep) {
				AUCdown = threshold;
			} else {
				AUCup = threshold;
			}
			if (((AUCup - AUCdown) < 0.01) || (nCycles > 100)) {
				cycle = false;
			}
		} // while (cycle)

		// Values 2 for discarded voxels, 1 for kept voxels
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				if (AUC[x][y] > threshold) {
					ROIauc[x][y] = ROI[x][y];
				} else {
					ROIauc[x][y] = (byte) (2 * ROI[x][y]);
				}
			}
		}
		if (display > 2) {
			UI.setDataText("Candidate voxel selection via AUC criteria\n");
			UI.setDataText("Voxel initial amounmt: " + totalCandidates + "\n");
			ROISum = 0;
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					ROISum += ROI[x][y];
				}
			}
			UI.setDataText("Survived voxels: " + ROISum + "\n");
			byte temp[] = new byte[length];
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					temp[x + y * nC] = ROIauc[x][y];
				}
			}
			ModelImage ROIaucImage = new ModelImage(ModelStorageBase.BYTE, extents2D, "ROIaucImage");
			try {
				ROIaucImage.importData(0, temp, true);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			saveImageFile(ROIaucImage, outputFilePath, outputPrefix + "ROIauc", saveFileFormat);
			ROIaucImage.disposeLocal();
		} // if (display > 2)

		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				if (AUC[x][y] <= threshold) {
					ROI[x][y] = 0;
				}
			}
		}

		// 2.2) Selection due to the TTP
		totalCandidates = 0;
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				totalCandidates += ROI[x][y];
			}
		}
		totalCandidatesToKeep = (int) Math.ceil(totalCandidates * (1 - pTTP));
		numTTPEqualMinus1 = length;
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				AIFsliceMaxValue = -Double.MAX_VALUE;
				AIFsliceTIndex = -1;
				TTP[x][y] = -1;
				for (t = 0; t < nT; t++) {
					if (AIFslice[x][y][t] > AIFsliceMaxValue) {
						AIFsliceMaxValue = AIFslice[x][y][t];
						AIFsliceTIndex = t;
					}
				}
				if (ROI[x][y] == 1) {
					TTP[x][y] = (short) AIFsliceTIndex;
					numTTPEqualMinus1--;
				}
			}
		}

		cycle = true;
		threshold = 0.0;
		while (cycle) {
			numTTPLessThreshold = 0;
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					if (TTP[x][y] < threshold) {
						numTTPLessThreshold++;
					}
				}
			}
			if ((numTTPLessThreshold - numTTPEqualMinus1) >= totalCandidatesToKeep) {
				cycle = false;
			} else {
				threshold = threshold + 1.0;
			}
		}

		// Values 2 for discarded voxels, 1 for kept voxels
		survivedVoxels = 0;
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				if (TTP[x][y] < threshold) {
					ROIttp[x][y] = ROI[x][y];
					if (ROI[x][y] == 1) {
						survivedVoxels++;
					}
				} else {
					ROIttp[x][y] = (byte) (2 * ROI[x][y]);
				}
			}
		}

		if (display > 2) {
			UI.setDataText("Candidate voxel selection via TTP criteria\n");
			UI.setDataText("Voxel initial amount = " + totalCandidates + "\n");
			UI.setDataText("Survived voxels = " + survivedVoxels + "\n");
			byte temp[] = new byte[length];
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					temp[x + y * nC] = ROIauc[x][y];
				}
			}
			ModelImage ROIttpImage = new ModelImage(ModelStorageBase.BYTE, extents2D, "ROIttpImage");
			try {
				ROIttpImage.importData(0, temp, true);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			saveImageFile(ROIttpImage, outputFilePath, outputPrefix + "ROIttp", saveFileFormat);
			ROIttpImage.disposeLocal();
		}

		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				if (TTP[x][y] >= threshold) {
					ROI[x][y] = 0;
				}
			}
		}

		// Select on the basis of the irregualrity index
		totalCandidates = 0;
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				if (ROI[x][y] == 1) {
					totalCandidates++;
				}
			}
		}
		totalCandidatesToKeep = (int) Math.ceil(totalCandidates * (1.0 - pReg));
		REG = calculateREG(ROI);

		cycle = true;
		nCycles = 0;
		REGdown = Double.MAX_VALUE;
		REGup = -Double.MAX_VALUE;
		for (c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				if (REG[c][r] < REGdown) {
					REGdown = REG[c][r];
				}
				if (REG[c][r] > REGup) {
					REGup = REG[c][r];
				}
			}
		} // for (c = 0; c < nC; c++)
		while (cycle) {
			nCycles = nCycles + 1;
			threshold = 0.5 * (REGup + REGdown);
			nCandidates = 0;
			for (c = 0; c < nC; c++) {
				for (r = 0; r < nR; r++) {
					if (REG[c][r] > threshold) {
						nCandidates++;
					}
				}
			}

			if (nCandidates == totalCandidatesToKeep) {
				cycle = false;
			} else if (nCandidates < totalCandidatesToKeep) {
				REGup = threshold;
			} else {
				REGdown = threshold;
			}
			if (((REGup - REGdown) < 0.001) || (nCycles >= 100)) {
				cycle = false;
			}
		} // while (cycle)

		// Values 2 for discarded voxels, 1 for kept voxels
		survivedVoxels = 0;
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				if (REG[x][y] > threshold) {
					ROIreg[x][y] = ROI[x][y];
					if (ROI[x][y] == 1) {
						survivedVoxels++;
					}
				} else {
					ROIreg[x][y] = (byte) (2 * ROI[x][y]);
				}
			}
		}

		if (display > 2) {
			UI.setDataText("Candidate voxel selection via regularity criteria\n");
			UI.setDataText("Voxel initial amount = " + totalCandidates + "\n");
			UI.setDataText("Survived voxels = " + survivedVoxels + "\n");
			byte temp[] = new byte[length];
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					temp[x + y * nC] = ROIreg[x][y];
				}
			}
			ModelImage ROIregImage = new ModelImage(ModelStorageBase.BYTE, extents2D, "ROIregImage");
			try {
				ROIregImage.importData(0, temp, true);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			saveImageFile(ROIregImage, outputFilePath, outputPrefix + "ROIreg", saveFileFormat);
			ROIregImage.disposeLocal();
		}

		ROISum = 0;
		int ROISumNoInfinity = 0;
		boolean infinityFound;
		for (x = 0; x < nC; x++) {
			for (y = 0; y < nR; y++) {
				if (REG[x][y] <= threshold) {
					ROI[x][y] = 0;
				}
				if (ROI[x][y] == 1) {
					ROISum++;
				}
				
			}
		}
		
		for (c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				infinityFound = false;
				for (t = 0; t < nT; t++) {
					if (Double.isInfinite(AIFslice[c][r][t])) {
						infinityFound = true;
					}
				}
				if ((ROI[c][r] == 1) && (!infinityFound)) {
					ROISumNoInfinity++;
					maskAIF[c][r] = ROI[c][r];
				}
				
			}
		}

		// 3.) Application of the cluster algorithm to search for the arterial
		if (display > 0) {
			UI.setDataText("Arterial voxels extraction\n");
		}
		// 3.1) Preparation of the matrix containing the data
		dati2D = new double[ROISumNoInfinity][nT];
		for (i = 0, c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				if (maskAIF[c][r] == 1) {
					for (t = 0; t < nT; t++) {
						dati2D[i][t] = AIFslice[c][r][t];
					}
					i++;
				}
			}
		}
		

		// 3.2) I apply the hierarchical cluster algorithm recursively

		cycle = true;
		nCycles = 0;

		nCluster = 2;
		centroidi = new double[nCluster][nT];
		AlgorithmKMeans kMeansAlgo;
		ModelImage kMeansImage = null;
		int algoSelection = AlgorithmKMeans.K_MEANS;
		int distanceMeasure = AlgorithmKMeans.EUCLIDEAN_SQUARED;
		double scale[] = new double[nT];
		for (t = 0; t < nT; t++) {
			scale[t] = 1.0;
		}
		// String resultsFileName = outputFilePath + outputPrefix + "kmeans.txt";
		String resultsFileName = null;
		int initSelection;
		float redBuffer[] = null;
		float greenBuffer[] = null;
		float blueBuffer[] = null;
		double scaleMax = 255.0;
		boolean useColorHistogram = false;
		boolean scaleVariablesToUnitVariance = false;
		double axesRatio[] = null;
		boolean bwSegmentedImage = false;
		double doubleBuffer[] = null;
		boolean showKMeansSegmentedImage = false;
		boolean followBatchWithIncremental = false;
		// If true, three dimensional color segmenting in RGB. If false, two dimensional
		// color segmenting in CIELAB
		boolean colorSegmentInRGB = false;
		double centroidPos[][] = new double[nT][nCluster];
		while (cycle) {
			nCycles = nCycles + 1;
			if (display > 2) {
				UI.setDataText("Cycle number = " + nCycles + '\n');
			}

			// I apply the hierarchical cluster
			// Use AlgorithmKMeans instead of MATLAB hierarchical clustering

			// Reverse order of indices for pos;
			double pos[][] = new double[nT][dati2D.length];
			for (t = 0; t < nT; t++) {
				for (i = 0; i < dati2D.length; i++) {
					pos[t][i] = dati2D[i][t];
				}
			}

			int vettCluster[] = new int[dati2D.length];
			// vettCluster in extract correponds to groupNum in AlgorithmKMeans
			double weight[] = new double[dati2D.length];
			for (i = 0; i < dati2D.length; i++) {
				weight[i] = 1.0;
			}
			System.out.println("groupNum = " + dati2D.length);
			// 588 points originally
			// First BRADLEY_FAYYAD_INIT takes 588 points to 97 or 98 points
			// AIF extraction times in seconds:
			// BRADLEY_FAYYAD_INIT for >= 10: 75, 67.655, 62.992
			// BRADLEY_FAYYAD_INIT for >= 20: 52.262, 53.049, 52.637
			// BRADLAY_FAYYAD_INIT for >= 40: 52.002, 51.892, 52.385
			// BRADLEY_FAYYAD_INIT for >= 80: 51.801, 55.007, 51.387
			// BRADLEY_FAYYAD_INIT for >= 100: 56.133, 57.772, 57.067
			// BRADLEY_FAYYAD_INIT for >= 600: Still working on first
			// HIERARCHICAL_GROUPING_INIT after 10 minutes
			if (dati2D.length >= 100) {
				initSelection = AlgorithmKMeans.BRADLEY_FAYYAD_INIT;
				System.out.println("Running BRADLEY_FAYYAD_INIT");
			} else {
				initSelection = AlgorithmKMeans.HIERARCHICAL_GROUPING_INIT;
				System.out.println("Running HIERARCHICAL_GROUPING_INIT");
			}
			long startKMeansTime = System.currentTimeMillis();
			kMeansAlgo = new AlgorithmKMeans(kMeansImage, algoSelection, distanceMeasure, pos, scale, vettCluster,
					weight, centroidPos, resultsFileName, initSelection, redBuffer, greenBuffer, blueBuffer, scaleMax,
					useColorHistogram, scaleVariablesToUnitVariance, axesRatio, bwSegmentedImage, doubleBuffer,
					showKMeansSegmentedImage, followBatchWithIncremental, colorSegmentInRGB);
			kMeansAlgo.run();
			// groupMean[nDims][numberClusters]
			double[][] groupMean = kMeansAlgo.getGroupMean();
			kMeansAlgo.finalize();
			kMeansAlgo = null;
			long KMeanRunTime = System.currentTimeMillis() - startKMeansTime;
			System.out.println("KMeans run time in milliseconds = " + KMeanRunTime);
			for (i = 0; i < nCluster; i++) {
				for (t = 0; t < nT; t++) {
					centroidi[i][t] = groupMean[t][i];
				}
			}

			// I compare the clusters and choose which one to keep
			double MC1 = -Double.MAX_VALUE;
			int TTP1 = -1;
			double MC2 = -Double.MAX_VALUE;
			int TTP2 = -1;
			for (t = 0; t < nT; t++) {
				if (centroidi[0][t] > MC1) {
					TTP1 = t;
					MC1 = centroidi[0][t];
				}
				if (centroidi[1][t] > MC2) {
					TTP2 = t;
					MC2 = centroidi[1][t];
				}
			}
			double maxMC = Math.max(MC1, MC2);
			double minMC = Math.min(MC1, MC2);
			if ((((maxMC - minMC) / maxMC) < diffPeak) && (TTP1 != TTP2)) {
				// When the difference between the peaks is less than the threshold,
				// I choose based on the TTP
				// The result is 1 if TTP1 > TTP2 and 0 if TTP2 < TTP1
				if (TTP1 > TTP2) {
					selectedCluster = 1;
					otherCluster = 0;
				} else {
					selectedCluster = 0;
					otherCluster = 1;
				}
				if (display > 2) {
					UI.setDataText("Cluster selected via TTP criteria\n");
					UI.setDataText("Selected cluster: " + selectedCluster + "\n");
				}
			} // if ((((maxMC - minMC)/maxMC) < diffPeak) && (TTP1 != TTP2))
			else {
				// I choose based on the difference between the peaks
				// The result is 0 if MC1 > MC2 and 1 if MC2 > MC1
				if (MC2 > MC1) {
					selectedCluster = 1;
					otherCluster = 0;
				} else {
					selectedCluster = 0;
					otherCluster = 1;
				}
				if (display > 2) {
					UI.setDataText("Cluster selected via MC criteria\n");
					UI.setDataText("Selected cluster: " + selectedCluster + "\n");
				}
			}
			int numberSelectedCluster = 0;
			int numberOtherCluster = 0;
			for (i = 0; i < dati2D.length; i++) {
				if (vettCluster[i] == selectedCluster) {
					numberSelectedCluster++;
				} else {
					numberOtherCluster++;
				}
			}
			if ((numberSelectedCluster < nVoxelMin) && (numberOtherCluster >= nVoxelMin)) {
				// I choose the other cluster
				i = selectedCluster;
				selectedCluster = otherCluster;
				otherCluster = i;
				i = numberSelectedCluster;
				numberSelectedCluster = numberOtherCluster;
				numberOtherCluster = i;
				if (display > 2) {
					UI.setDataText("Cluster selected switched because of minimum voxel bound\n");
					UI.setDataText("Selected cluster: " + selectedCluster + "\n");
				}
			} // if ((numberSelectedCluster < nVoxelMin) && (numberOtherCluster >= nVoxelMin))

			// I only keep the data relating to the chosen cluster
			for (i = 0, c = 0; c < nC; c++) {
				for (r = 0; r < nR; r++) {
					if (maskAIF[c][r] == 1) {
						if (vettCluster[i++] == otherCluster) {
							maskAIF[c][r] = 0;
						}
					}
				}
			}

			double dati2Dold[][] = new double[dati2D.length][nT];
			for (i = 0; i < dati2D.length; i++) {
				for (t = 0; t < nT; t++) {
					dati2Dold[i][t] = dati2D[i][t];
				}
			}
			dati2D = new double[numberSelectedCluster][nT];
			for (j = 0, i = 0; i < dati2Dold.length; i++) {
				if (vettCluster[i] == selectedCluster) {
					for (t = 0; t < nT; t++) {
						dati2D[j][t] = dati2Dold[i][t];
					}
					j++;
				}
			}

			if (display > 2) {
				UI.setDataText("Resume cycle # " + nCycles + "\n");
				UI.setDataText("Voxel initial amount: " + dati2D.length + "\n");
				UI.setDataText("Survived voxels: " + numberSelectedCluster + "\n");
				UI.setDataText("Cluster 0: MC = " + MC1 + "\n");
				UI.setDataText("Cluster 0 TTP = " + TTP1 + "\n");
				if (selectedCluster == 0) {
					UI.setDataText("Cluster 0 voxel number = " + numberSelectedCluster + "\n");
				} else {
					UI.setDataText("Cluster 0 voxel number = " + numberOtherCluster + "\n");
				}
				UI.setDataText("Cluster 1: MC = " + MC2 + "\n");
				UI.setDataText("Cluster 1 TTP = " + TTP2 + "\n");
				if (selectedCluster == 1) {
					UI.setDataText("Cluster 1 voxel number = " + numberSelectedCluster + "\n");
				} else {
					UI.setDataText("Cluster 1 voxel number = " + numberOtherCluster + "\n");
				}
				UI.setDataText("Selected cluster: " + selectedCluster + "\n");
			} // if (display > 2)

			// Check the exit criterial
			if ((numberSelectedCluster <= nVoxelMax) || (nCycles >= 100)) {
				cycle = false;
			}
		} // while (cycle)

		// 4.) Output preparation

		// 4.1) Save the search ROI
		AIF_ROI = new byte[nC][nR];
		for (c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				AIF_ROI[c][r] = ROI[c][r];
			}
		}
		AIF_ROI_x = new double[xROI.length];
		for (i = 0; i < xROI.length; i++) {
			AIF_ROI_x[i] = xROI[i];
		}
		AIF_ROI_y = new double[yROI.length];
		for (i = 0; i < yROI.length; i++) {
			AIF_ROI_y[i] = yROI[i];
		}

		// 4.2) Save the position of the chosen voxels and the average concentration
		// Concentration standards for AIF
		AIF_conc = new double[nT];
		for (t = 0; t < nT; t++) {
			AIF_conc[t] = centroidi[selectedCluster][t];
		}
		int numMaskAIF = 0;
		for (c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				if (maskAIF[c][r] == 1) {
					numMaskAIF++;
				}
			}
		}

		AIF_voxels = new int[numMaskAIF][2];
		int pos = 0;
		for (c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				if (maskAIF[c][r] == 1) {
					AIF_voxels[pos][0] = c;
					AIF_voxels[pos][1] = r;
					pos++;
				}
			}
		}

		// 4.3) Calculate the fit of the arterial with the gamma-variate (with
		// recirculation)
		if (display > 0) {
			UI.setDataText("Gamma variate fit computation\n");
		}

		// Weights for the calculation of the fit
		AIF_fit_weights = new double[nT];
		for (t = 0; t < nT; t++) {
			AIF_fit_weights[t] = 0.01 + Math.exp(-AIF_conc[t]);
		}

		double MC = -Double.MAX_VALUE;
		int WTTP = -1;
		for (t = 0; t < nT; t++) {
			if (AIF_conc[t] > MC) {
				WTTP = t;
				MC = AIF_conc[t];
			}
		}
		AIF_fit_weights[WTTP] = AIF_fit_weights[WTTP] / 10.0;
		if (WTTP >= 1) {
			AIF_fit_weights[WTTP - 1] = AIF_fit_weights[WTTP - 1] / 5.0;
		}
		if (WTTP < nT-1) {
		   AIF_fit_weights[WTTP + 1] = AIF_fit_weights[WTTP + 1] / 2.0;
		}

		fitGV_peak1(AIF_conc, AIF_fit_weights);
		if (aif_recirculation == 1) {
			fitGV_peak2(AIF_conc, AIF_fit_weights);
			AIF_fit_parameters = new double[7];
			AIF_fit_cv_est_parGV = new double[7];
			for (i = 0; i < 4; i++) {
				AIF_fit_parameters[i] = fitParameters_peak1[i];
				AIF_fit_cv_est_parGV[i] = cv_est_parGV_peak1[i];
			}
			for (i = 0; i < 3; i++) {
				AIF_fit_parameters[i + 4] = fitParameters_peak2[i];
				AIF_fit_cv_est_parGV[i + 4] = cv_est_parGV_peak2[i];
			}
			AIF_fit_gv = GVfunction(AIF_fit_parameters);
		} // if (aif_recirculation == 1)
		else {
			AIF_fit_parameters = new double[4];
			AIF_fit_cv_est_parGV = new double[4];
			for (i = 0; i < 4; i++) {
				AIF_fit_parameters[i] = fitParameters_peak1[i];
				AIF_fit_cv_est_parGV[i] = cv_est_parGV_peak1[i];
			}
			AIF_fit_gv = GVfunction_peak1(AIF_fit_parameters);
		}

		//if (display > 1) {
			vettImmagine = new double[length];
			for (x = 0; x < nC; x++) {
				for (y = 0; y < nR; y++) {
					vettImmagine[x + y * nC] = immagine_img[x][y];
				}
			}
			ModelImage immagineImage = new ModelImage(ModelStorageBase.DOUBLE, extents2D, "immagineImage");
			try {
				immagineImage.importData(0, vettImmagine, true);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			float xArr[] = new float[1];
			float yArr[] = new float[1];
			float zArr[] = new float[1];
			for (i = 0; i < numMaskAIF; i++) {
				VOI AIFPtVOI = new VOI((short) (i), "", VOI.POINT, -1.0f);
				AIFPtVOI.setColor(Color.red);
				xArr[0] = (float) AIF_voxels[i][0];
				yArr[0] = (float) AIF_voxels[i][1];
				AIFPtVOI.importCurve(xArr, yArr, zArr);
				((VOIPoint) (AIFPtVOI.getCurves().elementAt(0))).setFixed(true);
				immagineImage.registerVOI(AIFPtVOI);
			}
			ViewJFrameImage vFrame = new ViewJFrameImage(immagineImage);
			Component component = vFrame.getComponent(0);
			Rectangle rect = component.getBounds();
			String format = "png";
			BufferedImage captureImage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_ARGB);
			component.paint(captureImage.getGraphics());

			File immagineFile = new File(outputFilePath + outputPrefix + "AIF_slice"+String.valueOf(aif_nSlice)+".png");
			boolean foundWriter;
			try {
				foundWriter = ImageIO.write(captureImage, format, immagineFile);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			if (!foundWriter) {
				System.err.println("No appropriate writer for AIF_slice"+String.valueOf(aif_nSlice)+".png");
				return;
			}
			captureImage.flush();
			component.setEnabled(false);
			component.setVisible(false);
			component.setIgnoreRepaint(true);
			vFrame.removeComponentListener();
			vFrame.removeWindowListener();
			vFrame.removeMouseMotionListener();
			vFrame.removeMouseListener();
			vFrame.removeKeyListener();
			vFrame.close(false);
			immagineImage.disposeLocal();
		//} // if (display > 1)
	} // public void extractAIF()
	
	public void DSC_mri_cbv() {
		// Original author Marco Castellaro - Universita di Padova - DEI
		
		// Calculate Cerebral Blood Volume parametric maps for a subject
		
		// Input parameters:
		// conc (4D matrix) which contains the trends of the DSC concentrations of all the slices
		// Options and the struct that contains the options of the method, the significant ones are:
		
		// options.time - It represents the vector of the times of the DSC exam (each sample represents
		//                the acquisition of an entire brain volume.
		
		// options.par.kh - Parameter representing the hematocrit dependence of the Cerebral Blood
		//                  Volume (CBV), by default set to 1, in this case relative estimates of
		//                  the CBV parameter are obtained
		
		// options.par.rho - Parameter that represents the dependence on the blood density of the
		//                   Cerebral Blood Volume (CBV), by default set to 1, in this case relative
		//                   estimates of the CBV parameter are obtained.
		
		// Output parameters:
		// cbv - (3D matrix) which contains the calculated parametric map
		int c, r, s, t;
		
		if (display > 0) {
			UI.setDataText("Calculating CBV\n");
		}
		
		cbv = new double[nC][nR][nS];
		
		double coeff = par_kh/par_rho;
		double conc_slice[][][] = new double[nC][nR][nT];
		double trapz_aif = trapz(AIF_fit_gv);
		double trapz_conc[][];
		for (s = 0; s < nS; s++) {
		    for (c = 0; c < nC; c++) {
		    	for (r = 0; r < nR; r++) {
		    		for (t = 0; t < nT; t++) {
		    		    conc_slice[c][r][t] = conc[c][r][s][t];
		    		} // for (t = 0; t < nT; t++)
		    	} // for (r = 0; r < nR; r++)
		    } // for (c = 0; c < nC; c++)
		    trapz_conc = trapz(conc_slice);
		    for (c = 0; c < nC; c++) {
		    	for (r = 0; r < nR; r++) {
		    		cbv[c][r][s] = coeff*mask_data[c][r][s]*trapz_conc[c][r]/trapz_aif;
		    	}
		    }
		} // for (s = 0; s < nS; s++)
		
		if (doSaveAllOutputs) {
			double cbvBuffer[] = new double[volume];
			ModelImage cbvImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "cbv");
			for (c = 0; c < nC; c++) {
				for (r = 0; r < nR; r++) {
					for (s = 0; s < nS; s++) {
						cbvBuffer[c + r*nC + s*length] = cbv[c][r][s];
					}
				}
			}
			try {
	    		cbvImage.importData(0, cbvBuffer, true);
	    	}
	    	catch (IOException e) {
	    		MipavUtil.displayError("IOException on cbvImage");
	    		//setCompleted(false);
	    		return;
	    	}
			FileInfoBase fileInfo[] = cbvImage.getFileInfo();
	    	for (s = 0; s < nS; s++) {
	    		fileInfo[s].setResolutions(resolutions3D);
	    		fileInfo[s].setUnitsOfMeasure(units3D);
	    		fileInfo[s].setDataType(ModelStorageBase.DOUBLE);
	    	}
	    	
	    	saveImageFile(cbvImage, outputFilePath, outputPrefix + "cbv", saveFileFormat);
	    	cbvImage.disposeLocal();
	    	cbvImage = null;
		} // if (doSaveAllOutputs)
	} // public void DSC_mri_cbv()
	
	public void DSC_mri_cbv_lc() {
		// Original author Marco Castellaro - Universita di Padova - DEI
		
		// Calculate leakage corrected Cerebral Blood Volume parametric maps for a subject
		
		// Input parameters:
		// conc (4D matrix) which contains the trends of the DSC concentrations of all the slices
		// Options and the struct that contains the options of the method, the significant ones are:
		
		// options.time - It represents the vector of the times of the DSC exam (each sample represents
		//                the acquisition of an entire brain volume.
		
		// options.par.kh - Parameter representing the hematocrit dependence of the Cerebral Blood
		//                  Volume (CBV), by default set to 1, in this case relative estimates of
		//                  the CBV parameter are obtained
		
		// options.par.rho - Parameter that represents the dependence on the blood density of the
		//                   Cerebral Blood Volume (CBV), by default set to 1, in this case relative
		//                   estimates of the CBV parameter are obtained.
		
		// Output parameters:
		// cbv_lc - (3D matrix) which contains the calculated parametric map
		int c, r, s, t, i, j, numUsed, v;
		double diff;
		LinearEquations2 le2 = new LinearEquations2();
		GeneralizedInverse2 gi2 = new GeneralizedInverse2();
		
		if (display > 0) {
			UI.setDataText("Calculating leakage corrected CBV\n");
		}	
		
	    double cbv_prelc[][][] = new double[nC][nR][nS];
		
		// We next estimated R2*(t) by averaging R2*(t) for all pixels within the mask that
		// did not demonstrate signal intensity enhancement (averaged over the final 10 time
		// points) greater than 1 SD above that pixel's
		// average baseline and estimated by using trapezoidal integration over the 120
		// acquired time points
	    
	    double SD[][] = vol2mat(conc, mask_data);
	    double SDstd[] = new double[SD.length];
	    double sumBolus = 0.0;
	    for (s = 0; s < nS; s++) {
	    	sumBolus += bolus[s];
	    }
	    double meanBolus = sumBolus/nS;
	    int floorMeanBolus = (int)Math.floor(meanBolus);
	    for (i = 0; i < SD.length; i++) {
	    	numUsed = 0;
	    	double firstDimensionTotal = 0.0;
	        for (j = 0; j <= floorMeanBolus; j++) {
	        	if (!Double.isNaN(SD[i][j])) {
	        		firstDimensionTotal += SD[i][j];
	        		numUsed++;
	        	}
	        } // for (j = 0; j <= floorMeanBolus; j++)
	        if (numUsed == 0) {
	        	SDstd[i] = Double.NaN;
	        }
	        else if (numUsed == 1) {
	        	SDstd[i] = 0.0;
	        }
	        else {
	        	double firstDimensionMean = firstDimensionTotal/numUsed;
	        	double sumSquared = 0.0;
	        	for (j = 0; j <= floorMeanBolus; j++) {
	        		if (!Double.isNaN(SD[i][j])) {
	        			diff = SD[i][j] - firstDimensionMean;
	        			sumSquared += diff * diff;
	        		}
	        	} // for (j = 0; j <= floorMeanBolus; j++) 
	        	SDstd[i] = Math.sqrt(sumSquared/(numUsed - 1.0));
	        }
	    } // for (i = 0; i < SD.length; i++)
	    double SD_map[][][] = new double[nC][nR][nS];
	    for (i = 0, c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				for (s = 0; s < nS; s++) {
					if (mask_data[c][r][s] == 1) {
						SD_map[c][r][s] = SDstd[i++];
					}
				}
			}
		}
	    
	    // AVG is the same as SD
	    Vector<Double>firstDimensionSamples = new Vector<Double>();
	    double AVGmedian[] = new double[SD.length];
	    for (i = 0; i < SD.length; i++) {
	    	numUsed = 0;
	    	firstDimensionSamples.clear();
	    	for (t = nT - 10; t < nT; t++) {
	    		if (!Double.isNaN(SD[i][t])) {
	        		firstDimensionSamples.add(SD[i][t]);
	        		numUsed++;
	        	}	
	    	} // for (t = nT - 10; t < nT; t++)
	    	if (numUsed == 0) {
	            AVGmedian[i] = Double.NaN;
	        }
	        else if (numUsed == 1) {
	        	AVGmedian[i] = firstDimensionSamples.get(0);
	        }
	        else {
	        	Collections.sort(firstDimensionSamples);
	        	if ((numUsed % 2) == 1) {
	        		AVGmedian[i] = firstDimensionSamples.get((numUsed-1)/2);
	        	}
	        	else {
	        		AVGmedian[i] = (firstDimensionSamples.get(numUsed/2) + firstDimensionSamples.get((numUsed/2)-1))/2.0;
	        	}
	        }
	    } // for (i = 0; i < SD.length; i++)
	    double AVG_map[][][] = new double[nC][nR][nS];
	    for (i = 0, c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				for (s = 0; s < nS; s++) {
					if (mask_data[c][r][s] == 1) {
						AVG_map[c][r][s] = AVGmedian[i++];
					}
				}
			}
		}
	    
	    byte mask_not_enhancing[][][] = new byte[nC][nR][nS];
	    for (c = 0; c < nC; c++) {
	    	for (r = 0; r < nR; r++) {
	    		for (s = 0; s < nS; s++) {
	    			if (mask_data[c][r][s] == 1) {
	    				if (!(Math.abs(AVG_map[c][r][s]) > 2.0 * SD_map[c][r][s])) {
	    					mask_not_enhancing[c][r][s] = 1;
	    				}
	    			}
	    		}
	    	}
	    }
	    
	    double conc_infinities_removed[][][][] = new double[nC][nR][nS][nT];
	    for (c = 0; c < nC; c++) {
	    	for (r = 0; r < nR; r++) {
	    		for (s = 0; s < nS; s++) {
	    			for (t = 0; t < nT; t++) {
	    				if (Double.isInfinite(conc[c][r][s][t])) {
	    					conc_infinities_removed[c][r][s][t] = 0.0;
	    				}
	    				else {
	    					conc_infinities_removed[c][r][s][t] = conc[c][r][s][t];
	    				}
	    			}
	    		}
	    	}
	    }
	    
	    double concmat[][] = vol2mat(conc_infinities_removed, mask_not_enhancing);
	    double R2star_AVG_not_enhancing[] = new double[nT];
	    for (t = 0; t < nT; t++) {
	    	double sum = 0.0;
	    	numUsed = 0;
	    	for (i = 0; i < concmat.length; i++) {
	    	    if (!Double.isNaN(concmat[i][t])) {
	    	    	sum += concmat[i][t];
	    	    	numUsed++;
	    	    }
	    	}
	    	if (numUsed == 0) {
	    		R2star_AVG_not_enhancing[t] = Double.NaN;
	    	}
	    	else {
	    		R2star_AVG_not_enhancing[t] = sum/numUsed;
	    	}
	    } // for (t = 0; t < nT; t++)
	    
	    double Delta_R2star[][] = vol2mat(conc_infinities_removed, mask_data);
	    
	    double phat[][] = new double[Delta_R2star.length][2];
	    double CVp[][] = new double[Delta_R2star.length][2];
	    double cumtrapz_R2star[] = cumtrapz(R2star_AVG_not_enhancing);
	    Matrix A = new Matrix(nT,2);
	    for (t = 0; t < nT; t++) {
	    	A.set(t, 0, -cumtrapz_R2star[t]);
	    	A.set(t, 1, R2star_AVG_not_enhancing[t]);
	    }
	    double sigmaphat[][] = ((A.transpose()).times(A)).inverse().getArray();
	    int min_bolus = Integer.MAX_VALUE;
	    for (s = 0; s < nS; s++) {
	    	if (bolus[s] < min_bolus) {
	    		min_bolus = bolus[s];
	    	}
	    }
	    
	    double Delta_R2star_vett[] = new double[nT];
	    double temp[][] = new double[2][2];
	    for (v = 0; v < Delta_R2star.length; v++) {
	    	double sumVett = 0.0;
	        for (t = 0; t < nT; t++) {
	        	Delta_R2star_vett[t] = Delta_R2star[v][t];
	        	if (t <= min_bolus) {
	        		sumVett += Delta_R2star_vett[t];
	        	}
	        }
	        double meanVett = sumVett/(min_bolus+1);
	        double sumSquaredVett = 0.0;
	        for (t = 0; t <= min_bolus; t++) {
	            diff = Delta_R2star_vett[t] - meanVett;
	            sumSquaredVett += diff * diff;
	        }
	        double sigma2 = sumSquaredVett/min_bolus;
	        double Acopy[][] = new double[nT][2];
	        for (t = 0; t < nT; t++) {
	        	Acopy[t][0] = A.get(t,0);
	        	Acopy[t][1] = A.get(t,1);
	        }
	        int ipiv[] = new int[Math.min(nT,2)];
	        int info[] = new int[1];
	        boolean rankDeficient = false;
	        int nrhs = 1; // number of columns of B
	        le2.dgetrf(nT,2,Acopy,nT,ipiv,info);
	        if (info[0] < 0) {
		    	  System.err.println("In DSC_mri_cbv_lc dgetrf argument number " + 
		      (-info[0]) + " is illegal");
		    	  return;
		      }
		      if (info[0] > 0) {
		    	  //System.err.println("In DSC_mri_cbv_lc dgetrf U["+(info[0]-1)+
		    			  //"]["+(info[0]-1)+"] is exactly 0");
		    	  rankDeficient = true;
		      }
		      if (!rankDeficient) {
		    	  char trans = 'N'; // Solve A*X = B (no transpose)
		    	  double rhs_array[][] = new double[nT][1];
			      for (t = 0; t < nT; t++) {
			    	  rhs_array[t][0] = Delta_R2star_vett[t];
			      }
			      le2.dgetrs(trans,2,nrhs,Acopy,nT,ipiv,rhs_array,nT,info);
			      if (info[0] < 0) {
			    	  System.err.println("In DSC_mri_cbv_lc dgetrs argument number " + 
			      (-info[0]) + " is illegal");
			    	  return;
			      }
			      for (i = 0; i < 2; i++) {
			    	  phat[v][i] = rhs_array[i][0];
			      }
		      } // if (!rankDeficient)
		      else {
		    	  double B[][] = new double[nT][nrhs];
				  for (t = 0; t < nT; t++) {
					  B[t][0] = Delta_R2star_vett[t];
				  }
				  double sing[] = new double[Math.min(nT,2)];
				  double rcond = -1.0; // Singular values s(i) <= RCOND*s[0] are treated as zero.
					                   // If  rcond < 0, machine precision is used instead.
				  int rank[] = new int[1];
				  double work[] = new double[1];
				  int lwork_query = -1;
				  int lwork;
				  Acopy = new double[nT][2];
			        for (t = 0; t < nT; t++) {
			        	Acopy[t][0] = A.get(t,0);
			        	Acopy[t][1] = A.get(t,1);
			        }
				  gi2.dgelss(nT,2,nrhs,Acopy,nT,B,nT,sing,rcond,rank,work,lwork_query,info);
				  if (info[0] != 0) {
				        System.err.println("DSC_mri_cbv_lc: LAPACK dgelss work query error code = " + info[0]);
				        return;
					}
					lwork = (int)work[0];
		
					// Allocate the workspace
					try {
						work = new double[lwork];
					}
					catch (OutOfMemoryError e) {
						return;	
					}
					// This routine must handle rank deficient matrices
					// since dgetrf does not handle rank deficient matrices.
					gi2.dgelss(nT,2,nrhs,Acopy,nT,B,nT,sing,rcond,rank,work,lwork,info);
					if (info[0] < 0) {
						System.err.println("In DSC_mri_cbv_lc for dgelss the (-info[0]) argument had an illegal value");
						return;
					}
					if (info[0] > 0) {
						System.err.println("In DSC_mri_cbv_lc the algorithm for computing the SVD failed to converge");
						System.err.println(info[0] + " off-diagonal elements of an intermediate bidiagonal form did not converge to zero.");
						return;
					}
				    for (i = 0; i < 2; i++) {
					   phat[v][i] = B[i][0];
				    }
		      }
		      temp[0][0] = sigma2 * sigmaphat[0][0];
		      temp[1][1] = sigma2 * sigmaphat[1][1];
		      CVp[v][0] = 100.0 * Math.sqrt(temp[0][0])/Math.abs(phat[v][0]);
		      CVp[v][1] = 100.0 * Math.sqrt(temp[1][1])/Math.abs(phat[v][1]);
	    } // for (v = 0; v < Delta_R2star.length; v++)
	    
	    double K2_vett[] = new double[Delta_R2star.length];
	    double K1_vett[] = new double[Delta_R2star.length];
	    double K2_CV_vett[] = new double[Delta_R2star.length];
	    double K1_CV_vett[] = new double[Delta_R2star.length];
	    for (i = 0; i < Delta_R2star.length; i++) {
	    	K2_vett[i] = phat[i][0];
	    	K1_vett[i] = phat[i][1];
	    	K2_CV_vett[i] = CVp[i][0];
	    	K1_CV_vett[i] = CVp[i][1];
	    }
	    
	    double K2_map[][][] = new double[nC][nR][nS];
	    double K1_map[][][] = new double[nC][nR][nS];
	    
	    for (i = 0, c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				for (s = 0; s < nS; s++) {
					if (mask_data[c][r][s] == 1) {
						K2_map[c][r][s] = K2_vett[i];
						K1_map[c][r][s] = K1_vett[i++];
					}
				}
			}
		}
	    
	    double K2_CV_map[][][] = new double[nC][nR][nS];
	    double K1_CV_map[][][] = new double[nC][nR][nS];
	    for (i = 0, c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				for (s = 0; s < nS; s++) {
					if (mask_data[c][r][s] == 1) {
						K2_CV_map[c][r][s] = K2_CV_vett[i];
						K1_CV_map[c][r][s] = K1_CV_vett[i++];
					}
				}
			}
		}
	    
	    double conc_time[] = new double[nT];
	    for (s = 0; s < nS; s++) {
	    	for (c = 0; c < nC; c++) {
	    		for (r = 0; r < nR; r++) {
	    			for (t = 0; t < nT; t++) {
	    				conc_time[t] = conc_infinities_removed[c][r][s][t];
	    			}
	    			cbv_prelc[c][r][s] = mask_data[c][r][s]*trapz(conc_time);
	    		}
	    	}
	    }
	    
	    cbv_lc = new double[nC][nR][nS];
	    double R2trapz = trapz(cumtrapz_R2star);
	    double trapz_aif = trapz(AIF_fit_gv);
	    double coeff = R2trapz/trapz_aif;
	    for (c = 0; c < nC; c++) {
	    	for (r = 0; r < nR; r++) {
	    		for (s = 0; s < nS; s++) {
	    		    cbv_lc[c][r][s] = cbv_prelc[c][r][s];
	    		    if (K2_CV_map[c][r][s] < 100.0) {
	    		    	cbv_lc[c][r][s] += K2_map[c][r][s] * coeff;
	    		    }
	    		}
	    	}
	    }
	    
	    if (doSaveAllOutputs) {
			double cbv_lcBuffer[] = new double[volume];
			ModelImage cbv_lcImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "cbv_lc");
			for (c = 0; c < nC; c++) {
				for (r = 0; r < nR; r++) {
					for (s = 0; s < nS; s++) {
						cbv_lcBuffer[c + r*nC + s*length] = cbv_lc[c][r][s];
					}
				}
			}
			try {
	    		cbv_lcImage.importData(0, cbv_lcBuffer, true);
	    	}
	    	catch (IOException e) {
	    		MipavUtil.displayError("IOException on cbv_lcImage");
	    		//setCompleted(false);
	    		return;
	    	}
			FileInfoBase fileInfo[] = cbv_lcImage.getFileInfo();
	    	for (s = 0; s < nS; s++) {
	    		fileInfo[s].setResolutions(resolutions3D);
	    		fileInfo[s].setUnitsOfMeasure(units3D);
	    		fileInfo[s].setDataType(ModelStorageBase.DOUBLE);
	    	}
	    	
	    	saveImageFile(cbv_lcImage, outputFilePath, outputPrefix + "cbv_lc", saveFileFormat);
	    	cbv_lcImage.disposeLocal();
	    	cbv_lcImage = null;
		} // if (doSaveAllOutputs)
	} // public void DSC_mri_cbv_lc()
	
	public void DSC_mri_cbf() {
		// Original author Marco Castellaro - Universita di Padova - DEI
		
		// Calculate parametric Cerebral Blood Flow (CBF) maps for a subject
		
		// Input parameters:
		// conc (4D matrix), which contains the trends of the DSC concentrations
		//                   of all the voxels
		// aif, trend of concentrations in the site chosen as arterial
		// mask (3D matrix), contains the matrix used to mask the brain volume
		//                   to be analyzed
		
		// Options and the struct that contains the options of the method, the
		// significant ones are:
		
		// The calculation of the CBF parameter requires a deconvolution operation,
		// in the toolbox there are some suitable methodologies for this purpose:
		// SVD, cSVD, oSVD (cSVD is block-circulant)
		
		// options.deconv.method - It must be a vector of cells that contains the names
		//                         of the algorithms with which the analysis is to be
		//                         carried out, for example, SVD, cSVD, or oSVD.
		//
		//                         For each method a struct with the characteristic parameters
		//                         of the method itself must be inserted in the options, as in
		//                         this example:
		
		//                         options.deconv.<method name>.<par_1>
		//                         options.deconv.<method name>.<...>
		//                         options.deconv.<method name>.<par_n>
		
		// options.time - It represents the vector of the times of the DSC exam (each sample
		//                represents the acquistion of a cerebral internal volume).
		
		// options.display level 1 shows the processing progress
		//                 level 2 It also gives information on the parameters set for the
		//                         algorithms used
		
		// Output parameters:
		// cbf - Different sub-structs, one for each method that you have chosen to use.
		//       each sub-struct contains a map field that distinguishes the computed
		//       cbf map, for example:
		//       cbf.<method name>.map
		
		// residual, 4D matrix that contains the residuals (and an optional field that 
		//           must be requested in the options parameter:
		//           parameter options.deconv.<method name>.residual)
		//           for example:
		//           cbf.<method name>,residual
		
		// It is possible to add a method of deconvolution, in order to do this it is
		// necessary to update the method variable, adding the string identifier and a 
		// case for the call of the new method, in addition a function must be written
		// that realizes the method and called DSC_mri_<method name>
		
		// Update if you intend to add a new method
		
		//String method[] = new String[] {"SVD","cSVD","oSVD"};
		int i;
		
		if (display > 0) {
			UI.setDataText("CBF\n");
		}
		
		for (i = 0; i < deconv_method.length; i++) {
			if (deconv_method[i].equalsIgnoreCase("SVD")) {
				DSC_mri_SVD();
			}
			else if (deconv_method[i].equalsIgnoreCase("cSVD")) {
				DSC_mri_cSVD();
			}
			else if (deconv_method[i].equalsIgnoreCase("oSVD")) {
				DSC_mri_oSVD();
			}
			// Update if you intend to add a new method
			// otherwise unrecognized method
			else {
				System.err.println("In DSC_mri_cbf deconv_method["+i+"] = " + deconv_method[i] + " is unrecognized");
				return;
			}
		}
 	} // public void DSC_mri_cbf()
	
	public void DSC_mri_SVD() {
	    // Original author: Marco Castellaro
		
		// Calculate the parametric Cerebral Blood Flow (CBF) and for deconvolution using the
		// method of singular value decomposition with truncation
		
		// Input parameters:
		// conc (4D matrix), which contains the trends of the DSC concentrations
		//                   of all the voxels
		// aif, trend of concentrations in the site chosen as arterial
		// mask (3D matrix), contains the matrix used to mask the brain volume
		//                   to be analyzed
		
		// Options and the struct that contains the options of the method, the
		// significant ones are:
		
		// options.deconv.svd.threshold - Percentage truncation threshold, referred to the
		// maximum eigenvalue, in Ostergaard and Calamante et al.  Set at 20%.
		
		// options.deconv.svd.residual - if at 1 it also produces the 4D matrix of residuals,
		//                               otherwise they are not calculated
		
		int k, r, c, s, t;
		SVD svd = new SVD();
		
		if (display > 1) {
			UI.setDataText("Method: SVD\n");
			UI.setDataText("deconv_SVD_threshold = " + deconv_SVD_threshold);
		}
		
		// I create the matrix G
		double aifVett[] = new double[nT];
		aifVett[0] = AIF_fit_gv[0];
		aifVett[nT-1] = AIF_fit_gv[nT-1];
		
		for (k = 1; k <= nT-2; k++) {
			aifVett[k] = (AIF_fit_gv[k-1] + 4.0*AIF_fit_gv[k] + AIF_fit_gv[k+1])/6.0;
		}
		
		// G is a toeplitz array
		double G[][] = new double[nT][nT];
		for (r = 0; r < nT; r++) {
			G[0][r] = aifVett[r];
	    }
		for (r = 1; r < nT; r++) {
			for (c = 1; c < nT; c++) {
				G[c][r] = G[c-1][r-1];
			}
		}
		
		// 2.) I apply the SVD to calculate the inverse G
		double eigenV[] = new double[nT];
		double U[][] = new double[nT][nT];
		double VT[][] = new double[nT][nT];
		double work[] = new double[1];
   	    int lwork = -1;
   	    int info[] = new int[1];
		svd.dgesvd('A','A',nT,nT,G,nT,eigenV,U,nT,VT,nT,work,lwork,info);
		lwork = (int)work[0];
        work = new double[lwork];
        svd.dgesvd('A','A',nT,nT,G,nT,eigenV,U,nT,VT,nT,work,lwork,info);
        if (info[0] < 0) {
        	System.err.println("In DSC_mri_SVD() svd.dgesvd argument " + (-info[0]) + " had an illegal value");
        	Preferences.debug("In DSC_mri_SVD() svd.dgesvd argument " + (-info[0]) + " had an illegal value\n", Preferences.DEBUG_ALGORITHM);
        	return;
	     }
	     if (info[0] > 0) {
        	System.err.println("In DSC_mri_SVD() svd.dgesvd dbdsqr did not converge.");
        	Preferences.debug("In DSC_mri_SVD() svd.dgesvd dbdsqr did not converge.\n",
        			Preferences.DEBUG_ALGORITHM);
        	return;
         }
	     
	     // threshold of 10% in Ostergaard and Calamante
	     // Note that the earlier comment gave 20%
	     double threshold = deconv_SVD_threshold * eigenV[0];
	     double newEigen[] = new double[nT];
	     for (k = 0; k < nT; k++) {
	         if (eigenV[k] >= threshold) {
	        	 newEigen[k] = 1.0/eigenV[k];
	         }
	     } // for (k = 0; k < nT; k++)
	     
	     // G = U S VT
	     // Ginv = V * Sinv * UT
	     Matrix matVT = new Matrix(VT);
	     Matrix matV = matVT.transpose();
	     Matrix matU = new Matrix(U);
	     Matrix matUT = matU.transpose();
	     Matrix matSinv = new Matrix(nT,nT);
	     for (k = 0; k < nT; k++) {
	    	 matSinv.set(k, k, newEigen[k]);
	     }
	     double Ginv[][] = ((matV.times(matSinv)).times(matUT)).getArray();
	     
	     // 3.) I apply the Ginv to calculate the residual function and the
	     //     CBF of each voxel
	     cbf_svd = new double[nC][nR][nS];
	     if (deconv_SVD_residual == 1) {
	    	 cbf_svd_residual = new double[nC][nR][nS][nT];
	     }
	     
	     double vettRes[] = new double[nT];
	     for (c = 0; c < nC; c++) {
	    	 for (r = 0; r < nR; r++) {
	    		 for (s = 0; s < nS; s++) {
	    			 if (mask_data[c][r][s] == 1) {
	    				 // Compute the residual function
	    				 for (t = 0; t < nT; t++) {
	    					 vettRes[t] = 0.0;
	    					 for (k = 0; k < nT; k++) {
	    						 vettRes[t] += Ginv[t][k]*conc[c][r][s][k];
	    					 }
	    					 vettRes[t] = vettRes[t]/tr;
	    				 } // for (t = 0; t < nT; t++)
	    				 double maxAbsVettRes = 0.0;
	    				 for (t = 0; t < nT; t++) {
	    					 if (Math.abs(vettRes[t]) > maxAbsVettRes) {
	    						 maxAbsVettRes = Math.abs(vettRes[t]);
	    					 }
	    				 } // for (t = 0; t < nT; t++)
	    				 cbf_svd[c][r][s] = maxAbsVettRes;
	    				 if (deconv_SVD_residual == 1) {
	    					 for (t = 0; t < nT; t++) {
	    						 cbf_svd_residual[c][r][s][t] = vettRes[t];
	    					 }
	    				 }
	    			 }
	    		 }
	    	 }
	     }
	     
	     if (doSaveAllOutputs) {
			double cbf_svdBuffer[] = new double[volume];
			ModelImage cbf_svdImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "cbf_svd");
			for (c = 0; c < nC; c++) {
				for (r = 0; r < nR; r++) {
					for (s = 0; s < nS; s++) {
						cbf_svdBuffer[c + r*nC + s*length] = cbf_svd[c][r][s];
					}
				}
			}
			try {
	    		cbf_svdImage.importData(0, cbf_svdBuffer, true);
	    	}
	    	catch (IOException e) {
	    		MipavUtil.displayError("IOException on cbf_svdImage");
	    		//setCompleted(false);
	    		return;
	    	}
			FileInfoBase fileInfo[] = cbf_svdImage.getFileInfo();
	    	for (s = 0; s < nS; s++) {
	    		fileInfo[s].setResolutions(resolutions3D);
	    		fileInfo[s].setUnitsOfMeasure(units3D);
	    		fileInfo[s].setDataType(ModelStorageBase.DOUBLE);
	    	}
	    	
	    	saveImageFile(cbf_svdImage, outputFilePath, outputPrefix + "cbf_svd", saveFileFormat);
	    	cbf_svdImage.disposeLocal();
	    	cbf_svdImage = null;
		} // if (doSaveAllOutputs)
	} // public void DSC_mri_SVD()
	
	public void DSC_mri_cSVD() {
        // Last modfication to original code Denis Peruzzo 07/06/2010
		
		// Original author: Marco Castellaro
		
		// Calculate the parametric Cerebral Blood Flow (CBF) and for deconvolution using the
		// method of singular value decomposition block-circulant version with truncation
		
		// Input parameters:
		// conc (4D matrix), which contains the trends of the DSC concentrations
		//                   of all the voxels
		// aif, trend of concentrations in the site chosen as arterial
		// mask (3D matrix), contains the matrix used to mask the brain volume
		//                   to be analyzed
		
		// Options and the struct that contains the options of the method, the
		// significant ones are:
		
		// options.deconv.svd.threshold - Percentage truncation threshold, referred to the
		// maximum eigenvalue, in Ostergaard and Calamante et al.  Set at 10%.
		
		// options.deconv.svd.residual - if at 1 it also produces the 4D matrix of residuals,
		//                               otherwise they are not calculated
		int k, r, c, s, t, nTpad;
		
		if (display > 1) {
			UI.setDataText("Method: cSVD\n");
			UI.setDataText("deconv_cSVD_threshold = " + deconv_cSVD_threshold);
		}
		
		// I create the matrix G
		nTpad = 2 * nT;
		double columnG[] = new double[nTpad];
		columnG[0] = AIF_fit_gv[0];
		columnG[nT-1] = (AIF_fit_gv[nT-2] + 4*AIF_fit_gv[nT-1])/6;
		columnG[nT] = AIF_fit_gv[nT-1]/6;
		for (k = 1; k <= nT-2; k++) {
			columnG[k] = (AIF_fit_gv[k-1] + 4*AIF_fit_gv[k] + AIF_fit_gv[k+1])/6;
		}
		double rowG[] = new double[nTpad];
		rowG[0] = columnG[0];
		for (k = 2; k <= nTpad; k++) {
			rowG[k-1] = columnG[nTpad + 1 - k];
		}
		
		// G is a toeplitz array
		double G[][] = new double[nTpad][nTpad];
		for (r = 0; r < nTpad; r++) {
			G[0][r] = columnG[r];
		}
		for (c = 1; c < nTpad; c++) {
			G[c][0] = rowG[c];
		}
		for (r = 1; r < nTpad; r++) {
			for (c = 1; c < nTpad; c++) {
				G[c][r] = G[c-1][r-1];
			}
		}
		
		// 2.) I apply the SVD to calculate the inverse G
		double eigenV[] = new double[nTpad];
		double U[][] = new double[nTpad][nTpad];
		double VT[][] = new double[nTpad][nTpad];
		double work[] = new double[1];
   	    int lwork = -1;
   	    int info[] = new int[1];
		svd.dgesvd('A','A',nTpad,nTpad,G,nTpad,eigenV,U,nTpad,VT,nTpad,work,lwork,info);
		lwork = (int)work[0];
        work = new double[lwork];
        svd.dgesvd('A','A',nTpad,nTpad,G,nTpad,eigenV,U,nTpad,VT,nTpad,work,lwork,info);
        if (info[0] < 0) {
        	System.err.println("In DSC_mri_cSVD() svd.dgesvd argument " + (-info[0]) + " had an illegal value");
        	Preferences.debug("In DSC_mri_cSVD() svd.dgesvd argument " + (-info[0]) + " had an illegal value\n", Preferences.DEBUG_ALGORITHM);
        	return;
	     }
	     if (info[0] > 0) {
        	System.err.println("In DSC_mri_cSVD() svd.dgesvd dbdsqr did not converge.");
        	Preferences.debug("In DSC_mri_cSVD() svd.dgesvd dbdsqr did not converge.\n",
        			Preferences.DEBUG_ALGORITHM);
        	return;
         }
	     
	     // threshold of 10% in Ostergaard and Calamante
	     double threshold = deconv_cSVD_threshold * eigenV[0];
	     double newEigen[] = new double[nTpad];
	     for (k = 0; k < nTpad; k++) {
	         if (eigenV[k] >= threshold) {
	        	 newEigen[k] = 1.0/eigenV[k];
	         }
	     } // for (k = 0; k < nTpad; k++)
	     
	     // G = U S VT
	     // Ginv = V * Sinv * UT
	     Matrix matVT = new Matrix(VT);
	     Matrix matV = matVT.transpose();
	     Matrix matU = new Matrix(U);
	     Matrix matUT = matU.transpose();
	     Matrix matSinv = new Matrix(nTpad,nTpad);
	     for (k = 0; k < nTpad; k++) {
	    	 matSinv.set(k, k, newEigen[k]);
	     }
	     double Ginv[][] = ((matV.times(matSinv)).times(matUT)).getArray();
	     
	     // 3.) I apply the Ginv to calculate the residual function and the
	     //     CBF of each voxel
	     cbf_csvd = new double[nC][nR][nS];
	     if (deconv_cSVD_residual == 1) {
	    	 cbf_csvd_residual = new double[nC][nR][nS][nTpad];
	     }
	     
	     double vettRes[] = new double[nTpad];
	     for (c = 0; c < nC; c++) {
	    	 for (r = 0; r < nR; r++) {
	    		 for (s = 0; s < nS; s++) {
	    			 if (mask_data[c][r][s] == 1) {
	    				 // Compute the residual function
	    				 for (t = 0; t < nTpad; t++) {
	    					 vettRes[t] = 0.0;
	    					 for (k = 0; k < nT; k++) {
	    						 vettRes[t] += Ginv[t][k]*conc[c][r][s][k];
	    					 }
	    					 vettRes[t] = vettRes[t]/tr;
	    				 } // for (t = 0; t < nTpad; t++)
	    				 double maxAbsVettRes = 0.0;
	    				 for (t = 0; t < nTpad; t++) {
	    					 if (Math.abs(vettRes[t]) > maxAbsVettRes) {
	    						 maxAbsVettRes = Math.abs(vettRes[t]);
	    					 }
	    				 } // for (t = 0; t < nTpad; t++)
	    				 cbf_csvd[c][r][s] = maxAbsVettRes;
	    				 if (deconv_cSVD_residual == 1) {
	    					 for (t = 0; t < nTpad; t++) {
	    						 cbf_csvd_residual[c][r][s][t] = vettRes[t];
	    					 }
	    				 }
	    			 }
	    		 }
	    	 }
	     }
	     
	     if (doSaveAllOutputs) {
			double cbf_csvdBuffer[] = new double[volume];
			ModelImage cbf_csvdImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "cbf_csvd");
			for (c = 0; c < nC; c++) {
				for (r = 0; r < nR; r++) {
					for (s = 0; s < nS; s++) {
						cbf_csvdBuffer[c + r*nC + s*length] = cbf_csvd[c][r][s];
					}
				}
			}
			try {
	    		cbf_csvdImage.importData(0, cbf_csvdBuffer, true);
	    	}
	    	catch (IOException e) {
	    		MipavUtil.displayError("IOException on cbf_csvdImage");
	    		//setCompleted(false);
	    		return;
	    	}
			FileInfoBase fileInfo[] = cbf_csvdImage.getFileInfo();
	    	for (s = 0; s < nS; s++) {
	    		fileInfo[s].setResolutions(resolutions3D);
	    		fileInfo[s].setUnitsOfMeasure(units3D);
	    		fileInfo[s].setDataType(ModelStorageBase.DOUBLE);
	    	}
	    	
	    	saveImageFile(cbf_csvdImage, outputFilePath, outputPrefix + "cbf_csvd", saveFileFormat);
	    	cbf_csvdImage.disposeLocal();
	    	cbf_csvdImage = null;
		} // if (doSaveAllOutputs)
	} // public void DSC_mri_cSVD()
	
	public void DSC_mri_oSVD() {
		// Last modfication to original code Marco Castellaro 10/03/2013
		
		// Original author: Marco Castellaro
		
		// Calculate the parametric Cerebral Blood Flow (CBF) and for deconvolution using the
		// method of singular value decomposition block-circulant version with oscillation index
		
		// Input parameters:
		// conc (4D matrix), which contains the trends of the DSC concentrations
		//                   of all the voxels
		// aif, trend of concentrations in the site chosen as arterial
		// mask (3D matrix), contains the matrix used to mask the brain volume
		//                   to be analyzed
		
		// Options and the struct that contains the options of the method, the
		// significant ones are:
		
		// options.deconv.osvd.oi - Oscillation index - default 0.035
		
		// options.deconv.svd.residual - if at 1 it also produces the 4D matrix of residuals,
		//                               otherwise they are not calculated
		int k, r, c, s, t, threshold, nTpad, j1;
		
		if (display > 1) {
			UI.setDataText("Method: oSVD\n");
			UI.setDataText("deconv_oSVD_OIthres = " + deconv_oSVD_OIthres);
		}
		
		// I create the matrix G
		nTpad = 2 * nT;
		double columnG[] = new double[nTpad];
		columnG[0] = AIF_fit_gv[0];
		columnG[nT-1] = (AIF_fit_gv[nT-2] + 4*AIF_fit_gv[nT-1])/6;
		columnG[nT] = AIF_fit_gv[nT-1]/6;
		for (k = 1; k <= nT-2; k++) {
			columnG[k] = (AIF_fit_gv[k-1] + 4*AIF_fit_gv[k] + AIF_fit_gv[k+1])/6;
		}
		double rowG[] = new double[nTpad];
		rowG[0] = columnG[0];
		for (k = 2; k <= nTpad; k++) {
			rowG[k-1] = columnG[nTpad + 1 - k];
		}
		
		// G is a toeplitz array
		double G[][] = new double[nTpad][nTpad];
		for (r = 0; r < nTpad; r++) {
			G[0][r] = columnG[r];
		}
		for (c = 1; c < nTpad; c++) {
			G[c][0] = rowG[c];
		}
		for (r = 1; r < nTpad; r++) {
			for (c = 1; c < nTpad; c++) {
				G[c][r] = G[c-1][r-1];
			}
		}
		
		double OIthres = deconv_oSVD_OIthres;
		// OIcounter = deconv_oSVD_OIcounter;
		
		// 3.) I apply the Ginv to calculate the residual function and the
	     //     CBF of each voxel
	     cbf_osvd = new double[nC][nR][nS];
	     cbf_osvd_OI = new double[nC][nR][nS];
	     if (deconv_oSVD_residual == 1) {
	    	 cbf_osvd_residual = new double[nC][nR][nS][nTpad];
	     }
	     
	     double eigenV[] = new double[nTpad];
		double U[][] = new double[nTpad][nTpad];
		double VT[][] = new double[nTpad][nTpad];
		double work[] = new double[1];
   	    int lwork = -1;
   	    int info[] = new int[1];
		svd.dgesvd('A','A',nTpad,nTpad,G,nTpad,eigenV,U,nTpad,VT,nTpad,work,lwork,info);
		lwork = (int)work[0];
        work = new double[lwork];
        svd.dgesvd('A','A',nTpad,nTpad,G,nTpad,eigenV,U,nTpad,VT,nTpad,work,lwork,info);
        if (info[0] < 0) {
        	System.err.println("In DSC_mri_oSVD() svd.dgesvd argument " + (-info[0]) + " had an illegal value");
        	Preferences.debug("In DSC_mri_oSVD() svd.dgesvd argument " + (-info[0]) + " had an illegal value\n", Preferences.DEBUG_ALGORITHM);
        	return;
	     }
	     if (info[0] > 0) {
        	System.err.println("In DSC_mri_oSVD() svd.dgesvd dbdsqr did not converge.");
        	Preferences.debug("In DSC_mri_oSVD() svd.dgesvd dbdsqr did not converge.\n",
        			Preferences.DEBUG_ALGORITHM);
        	return;
         }
	     double W[][] = new double[nTpad][nTpad];
	     for (k = 0; k < nTpad; k++) {
	    	 W[k][k] = 1.0/eigenV[k];
	     }
	     
	     Matrix vettConcMat = new Matrix(nTpad, 1);
	     Matrix W1Mat = new Matrix(nTpad, nTpad);
	     Matrix matVT = new Matrix(VT);
	     Matrix matV = matVT.transpose();
	     Matrix matU = new Matrix(U);
	     Matrix matUT = matU.transpose();
	     double vettRes[][] = null;
	     double vettResMax = -Double.MAX_VALUE;
	     double O;
	     double OI = 0.0;
	     for (c = 0; c < nC; c++) {
	    	 for (r = 0; r < nR; r++) {
	    		 for (s = 0; s < nS; s++) {
	    			 if (mask_data[c][r][s] == 1) {
	    				 // Compute the residual function
	    				 for (t = 0; t < nT; t++) {
	    					 vettConcMat.set(t, 0, conc[c][r][s][t]);
	    				 }
	    				 for (k = 0; k < nTpad; k++) {
	    					 W1Mat.set(k,k,W[k][k]);
	    				 }
	    				 for (threshold = 5; threshold <= 95; threshold += 5) {
	    				     for (k = 0; k < nTpad; k++) {
	    				    	 if (eigenV[k] < ((double)threshold)/100*eigenV[0]) {
	    				    		 W1Mat.set(k,k,0.0);
	    				    	 }
	    				     }
	    				     vettRes = ((matV.times(W1Mat)).times(matUT.times(vettConcMat))).getArray();
	    				     O = 0.0;
	    				     for (j1 = 2; j1 < nTpad; j1++) {
	    				    	 O = O + Math.abs(vettRes[j1][0] -2*vettRes[j1-1][0] + vettRes[j1-2][0]);
	    				     }
	    				     vettResMax = -Double.MAX_VALUE;
	    				     for (k = 0; k < nTpad; k++) {
	    				    	 if (vettRes[k][0] > vettResMax) {
	    				    		 vettResMax = vettRes[k][0];
	    				    	 }
	    				     }
	    				     OI = (1.0/(double)nTpad)*(1.0/vettResMax)*O;
	    				     
	    				     if (OI < OIthres) {
	    				    	 break;
	    				     }
	    				 } // for (threshold = 5; threshold <= 95; threshold += 5)
	    				 
	    				 cbf_osvd[c][r][s] = vettResMax;
	    				 cbf_osvd_OI[c][r][s] = OI;
	    				 if (deconv_oSVD_residual == 1) {
	    				     for (t = 0; t < nTpad; t++) {
	    				    	 cbf_osvd_residual[c][r][s][t] = vettRes[t][0];
	    				     }
	    				 }
	    			 } // if (mask_data[c][r][s] == 1)
	    		 }
	    	 }
	     }
	     
	     if (doSaveAllOutputs) {
			double cbf_osvdBuffer[] = new double[volume];
			ModelImage cbf_osvdImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "cbf_osvd");
			for (c = 0; c < nC; c++) {
				for (r = 0; r < nR; r++) {
					for (s = 0; s < nS; s++) {
						cbf_osvdBuffer[c + r*nC + s*length] = cbf_osvd[c][r][s];
					}
				}
			}
			try {
	    		cbf_osvdImage.importData(0, cbf_osvdBuffer, true);
	    	}
	    	catch (IOException e) {
	    		MipavUtil.displayError("IOException on cbf_osvdImage");
	    		//setCompleted(false);
	    		return;
	    	}
			FileInfoBase fileInfo[] = cbf_osvdImage.getFileInfo();
	    	for (s = 0; s < nS; s++) {
	    		fileInfo[s].setResolutions(resolutions3D);
	    		fileInfo[s].setUnitsOfMeasure(units3D);
	    		fileInfo[s].setDataType(ModelStorageBase.DOUBLE);
	    	}
	    	
	    	saveImageFile(cbf_osvdImage, outputFilePath, outputPrefix + "cbf_osvd", saveFileFormat);
	    	cbf_osvdImage.disposeLocal();
	    	cbf_osvdImage = null;
		} // if (doSaveAllOutputs)
	} // public void DSC_mri_oSVD()
	
	public void DSC_mri_mtt() {
        int i, c, r, s;
		
		if (display > 0) {
			UI.setDataText("MTT\n");
		}
		
		for (i = 0; i < deconv_method.length; i++) {
			if (deconv_method[i].equalsIgnoreCase("SVD")) {
				mtt_svd = new double[nC][nR][nS];
				for (c = 0; c < nC; c++) {
					for (r = 0; r < nR; r++) {
						for (s = 0; s < nS; s++) {
							mtt_svd[c][r][s] = cbv_lc[c][r][s]/cbf_svd[c][r][s];
						}
					}
				}
				if (doSaveAllOutputs) {
					double mtt_svdBuffer[] = new double[volume];
					ModelImage mtt_svdImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "mtt_svd");
					for (c = 0; c < nC; c++) {
						for (r = 0; r < nR; r++) {
							for (s = 0; s < nS; s++) {
								mtt_svdBuffer[c + r*nC + s*length] = mtt_svd[c][r][s];
							}
						}
					}
					try {
			    		mtt_svdImage.importData(0, mtt_svdBuffer, true);
			    	}
			    	catch (IOException e) {
			    		MipavUtil.displayError("IOException on mtt_svdImage");
			    		//setCompleted(false);
			    		return;
			    	}
					FileInfoBase fileInfo[] = mtt_svdImage.getFileInfo();
			    	for (s = 0; s < nS; s++) {
			    		fileInfo[s].setResolutions(resolutions3D);
			    		fileInfo[s].setUnitsOfMeasure(units3D);
			    		fileInfo[s].setDataType(ModelStorageBase.DOUBLE);
			    	}
			    	
			    	saveImageFile(mtt_svdImage, outputFilePath, outputPrefix + "mtt_svd", saveFileFormat);
			    	mtt_svdImage.disposeLocal();
			    	mtt_svdImage = null;
			  } // if (doSaveAllOutputs)
			}
			else if (deconv_method[i].equalsIgnoreCase("cSVD")) {
				mtt_csvd = new double[nC][nR][nS];
				for (c = 0; c < nC; c++) {
					for (r = 0; r < nR; r++) {
						for (s = 0; s < nS; s++) {
							mtt_csvd[c][r][s] = cbv_lc[c][r][s]/cbf_csvd[c][r][s];
						}
					}
				}
				if (doSaveAllOutputs) {
					double mtt_csvdBuffer[] = new double[volume];
					ModelImage mtt_csvdImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "mtt_csvd");
					for (c = 0; c < nC; c++) {
						for (r = 0; r < nR; r++) {
							for (s = 0; s < nS; s++) {
								mtt_csvdBuffer[c + r*nC + s*length] = mtt_csvd[c][r][s];
							}
						}
					}
					try {
			    		mtt_csvdImage.importData(0, mtt_csvdBuffer, true);
			    	}
			    	catch (IOException e) {
			    		MipavUtil.displayError("IOException on mtt_csvdImage");
			    		//setCompleted(false);
			    		return;
			    	}
					FileInfoBase fileInfo[] = mtt_csvdImage.getFileInfo();
			    	for (s = 0; s < nS; s++) {
			    		fileInfo[s].setResolutions(resolutions3D);
			    		fileInfo[s].setUnitsOfMeasure(units3D);
			    		fileInfo[s].setDataType(ModelStorageBase.DOUBLE);
			    	}
			    	
			    	saveImageFile(mtt_csvdImage, outputFilePath, outputPrefix + "mtt_csvd", saveFileFormat);
			    	mtt_csvdImage.disposeLocal();
			    	mtt_csvdImage = null;
			  } // if (doSaveAllOutputs)
			}
			else if (deconv_method[i].equalsIgnoreCase("oSVD")) {
				mtt_osvd = new double[nC][nR][nS];
				for (c = 0; c < nC; c++) {
					for (r = 0; r < nR; r++) {
						for (s = 0; s < nS; s++) {
							mtt_osvd[c][r][s] = cbv_lc[c][r][s]/cbf_osvd[c][r][s];
						}
					}
				}
				if (doSaveAllOutputs) {
					double mtt_osvdBuffer[] = new double[volume];
					ModelImage mtt_osvdImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "mtt_osvd");
					for (c = 0; c < nC; c++) {
						for (r = 0; r < nR; r++) {
							for (s = 0; s < nS; s++) {
								mtt_osvdBuffer[c + r*nC + s*length] = mtt_osvd[c][r][s];
							}
						}
					}
					try {
			    		mtt_osvdImage.importData(0, mtt_osvdBuffer, true);
			    	}
			    	catch (IOException e) {
			    		MipavUtil.displayError("IOException on mtt_osvdImage");
			    		//setCompleted(false);
			    		return;
			    	}
					FileInfoBase fileInfo[] = mtt_osvdImage.getFileInfo();
			    	for (s = 0; s < nS; s++) {
			    		fileInfo[s].setResolutions(resolutions3D);
			    		fileInfo[s].setUnitsOfMeasure(units3D);
			    		fileInfo[s].setDataType(ModelStorageBase.DOUBLE);
			    	}
			    	
			    	saveImageFile(mtt_osvdImage, outputFilePath, outputPrefix + "mtt_osvd", saveFileFormat);
			    	mtt_osvdImage.disposeLocal();
			    	mtt_osvdImage = null;
			  } // if (doSaveAllOutputs)
			}
			// Update if you intend to add a new method
			// otherwise unrecognized method
			else {
				System.err.println("In DSC_mri_mtt deconv_method["+i+"] = " + deconv_method[i] + " is unrecognized");
				return;
			}
		}	
	} // public void DSC_mri_mtt()
	
	public void DSC_mri_ttp() {
		int c, r, s, t;
		double maxConc;
		short maxt;
	    ttp = new short[nC][nR][nS];
	    for (c = 0; c < nC; c++) {
	    	for (r = 0; r < nR; r++) {
	    		for (s = 0; s < nS; s++) {
	    			if (mask_data[c][r][s] == 0) {
	    				ttp[c][r][s] = -1;
	    			}
	    			else {
	    				maxConc = -Double.MAX_VALUE;
	    				maxt = -1;
	    				for (t = 0; t < nT; t++) {
	    				    if (conc[c][r][s][t] > maxConc) {
	    				    	maxConc = conc[c][r][s][t];
	    				    	maxt = (short)t;
	    				    }
	    				}
	    				ttp[c][r][s] = maxt;
	    			}
	    		}
	    	}
	    }
	    
	    if (doSaveAllOutputs) {
			short ttpBuffer[] = new short[volume];
			ModelImage ttpImage = new ModelImage(ModelStorageBase.SHORT, extents3D, "ttp");
			for (c = 0; c < nC; c++) {
				for (r = 0; r < nR; r++) {
					for (s = 0; s < nS; s++) {
						ttpBuffer[c + r*nC + s*length] = ttp[c][r][s];
					}
				}
			}
			try {
	    		ttpImage.importData(0, ttpBuffer, true);
	    	}
	    	catch (IOException e) {
	    		MipavUtil.displayError("IOException on ttpImage");
	    		//setCompleted(false);
	    		return;
	    	}
			FileInfoBase fileInfo[] = ttpImage.getFileInfo();
	    	for (s = 0; s < nS; s++) {
	    		fileInfo[s].setResolutions(resolutions3D);
	    		fileInfo[s].setUnitsOfMeasure(units3D);
	    		fileInfo[s].setDataType(ModelStorageBase.SHORT);
	    	}
	    	
	    	saveImageFile(ttpImage, outputFilePath, outputPrefix + "ttp", saveFileFormat);
	    	ttpImage.disposeLocal();
	    	ttpImage = null;
	  } // if (doSaveAllOutputs)
	} // public void DSC_mri_ttp;
	
	public void DSC_mri_fwhm() {
		int i, c, r, s;
	    double conc_vect[][] = vol2mat(conc,mask_data);	
	    double fwhm_vect[] = new double[conc_vect.length];
	    
	    for (r = 0; r < conc_vect.length; r++) {
	        fwhm_vect[r] = fwhm(time, conc_vect[r]);
	        if (Double.isNaN(fwhm_vect[r])) {
	        	fwhm_vect[r] = 0.0;
	        }
	    } // for (r = 0; r < conc_vect.length; r++)
	    
	    fwhm = new double[nC][nR][nS];
	    for (i = 0, c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				for (s = 0; s < nS; s++) {
					if (mask_data[c][r][s] == 1) {
						fwhm[c][r][s] = fwhm_vect[i++];
					}
				}
			}
		}
	    
	    if (doSaveAllOutputs) {
			double fwhmBuffer[] = new double[volume];
			ModelImage fwhmImage = new ModelImage(ModelStorageBase.DOUBLE, extents3D, "fwhm");
			for (c = 0; c < nC; c++) {
				for (r = 0; r < nR; r++) {
					for (s = 0; s < nS; s++) {
						fwhmBuffer[c + r*nC + s*length] = fwhm[c][r][s];
					}
				}
			}
			try {
	    		fwhmImage.importData(0, fwhmBuffer, true);
	    	}
	    	catch (IOException e) {
	    		MipavUtil.displayError("IOException on fwhmImage");
	    		//setCompleted(false);
	    		return;
	    	}
			FileInfoBase fileInfo[] = fwhmImage.getFileInfo();
	    	for (s = 0; s < nS; s++) {
	    		fileInfo[s].setResolutions(resolutions3D);
	    		fileInfo[s].setUnitsOfMeasure(units3D);
	    		fileInfo[s].setDataType(ModelStorageBase.DOUBLE);
	    	}
	    	
	    	saveImageFile(fwhmImage, outputFilePath, outputPrefix + "fwhm", saveFileFormat);
	        fwhmImage.disposeLocal();
	    	fwhmImage = null;
	  } // if (doSaveAllOutputs)
	} // public void DSC_mri_fwhm
	
	double fwhm(double x[], double yorg[]) {
		// function width = fwhm(x,y)
		
		// Full-Width at Half-Maximum (FWHM) of the waveform y(x)
		// and its polarity.
		// The FWHM result in 'width' will be in units of 'x'
		
		
		// Rev 1.2, April 2006 (Patrick Egan)
		int i;
		int N;
		double miny;
		double maxy;
		int centerindex;
		double interp;
		double tlead;
		double ttrail;
		double width;

        maxy = -Double.MAX_VALUE;
        N = yorg.length;
        double y[] = new double[N];
        for (i = 0; i < N; i++) {
        	y[i] = yorg[i];
        	if (y[i] > maxy) {
        		maxy = y[i];
        	}
        }
        for (i = 0; i < N; i++) {
		    y[i] = y[i] / maxy;
        }
	
		double lev50 = 0.5;
		// find index of center (max or min) of pulse
		if (y[0] < lev50) {
			maxy = -Double.MAX_VALUE;
			centerindex = -1;
			for (i = 0; i < N; i++) {
	        	if (y[i] > maxy) {
	        		maxy = y[i];
	        		centerindex = i;
	        	}
	        }
		    //Pol = +1;
		    //UI.setDataText("Pulse Polarity = Positive\n");
		}
		else {
			miny = Double.MAX_VALUE;
			centerindex = -1;
			for (i = 0; i < N; i++) {
				if (y[i] < miny) {
					miny = y[i];
					centerindex = i;
				}
			}
		    //Pol = -1;
		    //UI.setDataText("Pulse Polarity = Negative\n");
		}
		i = 1;
		while (Math.signum(y[i]-lev50) == Math.signum(y[i-1]-lev50)) {
		    i = i+1;
		}                                  
		// first crossing is between v(i-1) & v(i)
		interp = (lev50-y[i-1]) / (y[i]-y[i-1]);
		tlead = x[i-1] + interp*(x[i]-x[i-1]);
		// start search for next crossing at center
		i = centerindex+1;                    
		while ((i < N-1) && (Math.signum(y[i]-lev50) == Math.signum(y[i-1]-lev50))) {
		    i = i+1;
		}
		if (i < N-1) {
		    //Ptype = 1;  
		    //UI.setDataText("Pulse is Impulse or Rectangular with 2 edges\n");
		    interp = (lev50-y[i-1]) / (y[i]-y[i-1]);
		    ttrail = x[i-1] + interp*(x[i]-x[i-1]);
		    width = ttrail - tlead;
		}
		else {
		    //Ptype = 2; 
		    //UI.setDataText("Step-Like Pulse, no second edge\n");
		    //ttrail = Double.NaN;
		    width = Double.NaN;
		}
		return width;

	}
	
	private double[][] vol2mat(double data[][][][], byte selected[][][]) {
		int c,r,s,t,i;
		int numSelected = 0;
		for (c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				for (s = 0; s < nS; s++) {
					if (selected[c][r][s] == 1) {
						numSelected++;
					}
				}
			}
		}
		int N_samples = data[0][0][0].length;
		double mat[][] = new double[numSelected][N_samples];
		double vol_temp[][][] = new double[nC][nR][nS];
		
		for (t = 0; t < N_samples; t++) {
		    for (c = 0; c < nC; c++) { 
		    	for (r = 0; r < nR; r++) {
		    		for (s = 0; s < nS; s++) {
		    			vol_temp[c][r][s] = data[c][r][s][t];
		    		}
		    	}
		    } // for (c = 0; c < nC; c++)
		    for (i = 0, c = 0; c < nC; c++) {
		    	for (r = 0; r < nR; r++) {
		    		for (s = 0; s < nS; s++) {
		    			if (selected[c][r][s] == 1) {
		    				mat[i++][t] = vol_temp[c][r][s];
		    			}
		    		}
		    	}
		    }
		} // for (t = 0; t < N_samples; t++)
		return mat;
	}

	private double[] GVfunction(double p[]) {
		// Compute the gamma-variate function defined by the parameters contained in p.
		// The gamma-variate function defined by the formula:

		// FP(t) = A*((t-t0)^alpha)*exp(-(t-t0)/beta)

		// c(t) = FP(t) + FP(t-td) conv K*exp(-t/tao)

		// The parameters are passed in the following order:
		// p = [t0 alpha beta A td K tao]

		// Since the formula predicts a convolution, the time grid
		// along which the much denser gamma variate than the final grid is calculated
		double t0 = p[0];
		double alpha = p[1];
		double beta = p[2];
		double A = p[3];
		double td = p[4];
		double K = p[5];
		double tao = p[6];
		int i, j, t;

		// 1.) Definition of the virtual grid necessary for convolution
		// Performed in DSC_mri_core to prevent unnecessary repetitions
		// double TR = time[1] - time[0];
		// double Tmax = -Double.MAX_VALUE;
		// double Tmin = Double.MAX_VALUE;

		// for (t = 0; t < nT; t++) {
		// if (time[t] < Tmin) {
		// Tmin = time[t];
		// }
		// if (time[t] > Tmax) {
		// Tmax = time[t];
		// }
		// }

		// double TRfine = TR/10.0;
		nTfine = 1 + (int) ((2 * Tmax) / TRfine);
		tGrid = new double[nTfine];
		for (t = 0; t < nTfine; t++) {
			tGrid[t] = t * TRfine;
		}

		// Calculation of the components of GV
		// I divide the GV into its main components
		double peak1[] = new double[nTfine]; // Main peak
		double peak2[] = new double[nTfine]; // Peak of recirculation
		double disp[] = new double[nTfine]; // Dispersion of recirculation

		double tg;
		for (i = 0; i < nTfine; i++) {
			tg = tGrid[i];

			if (tg > t0) {
				peak1[i] = A * Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta);
			}

			if (tg > (t0 + td)) {
				// Calculation of FP(t-td)
				peak2[i] = K * Math.pow((tg - t0 - td), alpha) * Math.exp(-(tg - t0 - td) / beta);
			}

			// Calculation of disp(t)
			disp[i] = Math.exp(-tg / tao);
		}

		// 3.) I assemble the components to obtain the GV calculated on the fine grid
		double recirculation_fine[] = new double[nTfine];
		double conc[] = new double[nTfine];
		for (i = 0; i < nTfine; i++) {
			for (j = 0; j <= i; j++) {
				recirculation_fine[i] += peak2[j] * disp[i - j];
			}
			recirculation_fine[i] *= TRfine;
			conc[i] = peak1[i] + recirculation_fine[i];
		}

		// 4.) I'm going to sample GV on the time instants requeted in time
		double GV[] = new double[nT];
		for (t = 0; t < nT; t++) {
			int pos = -1;
			double minValue = Double.MAX_VALUE;
			for (i = 0; i < nTfine; i++) {
				double absValue = Math.abs(tGrid[i] - time[t]);
				if (absValue < minValue) {
					minValue = absValue;
					pos = i;
				}
			}
			GV[t] = conc[pos];
		}
		return GV;
	}

	private void fitGV_peak1(double dati[], double orgWeights[]) {
		// Calculate the fit of the first peak with a gamma variate function
		// The function used is described by the formula:

		// FP(t) = A*((t-t0)^alpha)*exp(-(t-t0)/beta)

		// c(t) = FP(t)

		// Parameters: p = [t0 alpha beta A]

		// The last parameter returned represents the exit flag,
		int i,j;
		LinearEquations2 le2 = new LinearEquations2();
		GeneralizedInverse2 gi2 = new GeneralizedInverse2();
		double weights[] = new double[orgWeights.length];
		for (i = 0; i < orgWeights.length; i++) {
			weights[i] = orgWeights[i];
		}

		// Initial parameter estimates (modification of Denis)
		// Alpha is set to 5
		alpha_init = 5.0;
		// t0 is estimated on the initial data. It is calculated as the last instant at
		// which the data remains less than 5% of the peak.
		double MCdati = -Double.MAX_VALUE;
		int TTPpos = -1;
		int t;
		for (t = 0; t < nT; t++) {
			if (dati[t] > MCdati) {
				MCdati = dati[t];
				TTPpos = t;
			}
		}
		double TTPdati = time[TTPpos];
		int lastIndex = -1;
		for (t = 0; t < TTPpos; t++) {
			if (dati[t] <= 0.05 * MCdati) {
				lastIndex = t;
			}
		}
		if (lastIndex == -1) {
			double mindati = Double.MAX_VALUE;
			for (t = 0; t <= TTPpos; t++) {
				if (dati[t] < mindati) {
					mindati = dati[t];
					lastIndex = t;
				}
			}
		}
		t0_init = time[lastIndex];

		// beta was estimated using the relation that TTP = t0 + alpha*beta
		beta_init = (TTPdati - t0_init) / alpha_init;

		// Initialize the parameters [t0 alpha beta] and choose A so that the initial
		// estimate
		// and the data have the same maximum
		double p[] = new double[] { t0_init, alpha_init, beta_init, 1.0 };
		double GV[] = GVfunction_peak1(p);
		double maxGV = -Double.MAX_VALUE;
		for (t = 0; t < nT; t++) {
			if (GV[t] > maxGV) {
				maxGV = GV[t];
			}
		}
		A_init = MCdati / maxGV;

		// Initial values of parameters in the estimate
		fitParameters_peak1 = new double[] { t0_init, alpha_init, beta_init, A_init };

		// In extractAIF already have:
		// weights[WTTP] = weights[WTTP]/10.0;
		// weights[WTTP-1] = weights[WTTP-1]/5.0;
		// weights[WTTP+1] = weights[WTTP+1]/2.0;
		// so don't implement duplicative:
		// Marco
		// I increase the precision of the weight
		// weight[TTPpos] = weight[TTPpos]/10.0;
		// weight[TTPpos-1] = weight[TTPPos-1]/2.0;

		// I find the end of the first peak (20% maximum value)
		i = TTPpos;
		while ((i < dati.length-1) && (dati[i] > 0.2 * dati[TTPpos])) {
			i++;
		}

		// Suitable data for "first peak only"
		data_peak1 = new double[nT];
		for (t = 0; t <= i; t++) {
			data_peak1[t] = dati[t];
		}

		weights_peak1 = new double[nT];
		for (t = 0; t <= i; t++) {
			weights_peak1[t] = weights[t];
		}
		for (t = i + 1; t < nT; t++) {
			weights_peak1[t] = 0.01;
		}
		if (display > 0) {
			UI.setDataText("Initial estimates for A*((t-t0)^alpha)*exp(-(t-t0)/beta)\n");
			UI.setDataText("t0 = " + fitParameters_peak1[0] + "\n");
			UI.setDataText("alpha = " + fitParameters_peak1[1] + "\n");
			UI.setDataText("beta = " + fitParameters_peak1[2] + "\n");
			UI.setDataText("A = " + fitParameters_peak1[3] + "\n");
		}

		CostFunction cost_function = new GVFittingCostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, fitParameters_peak1);
		problem.AddParameterBlock(fitParameters_peak1, 4);
		problem.SetParameterLowerBound(fitParameters_peak1, 0, 0.1 * t0_init);
		problem.SetParameterUpperBound(fitParameters_peak1, 0, 10.0 * t0_init);
		problem.SetParameterLowerBound(fitParameters_peak1, 1, 0.1 * alpha_init);
		problem.SetParameterUpperBound(fitParameters_peak1, 1, 10.0 * alpha_init);
		problem.SetParameterLowerBound(fitParameters_peak1, 2, 0.1 * beta_init);
		problem.SetParameterUpperBound(fitParameters_peak1, 2, 10.0 * beta_init);
		problem.SetParameterLowerBound(fitParameters_peak1, 3, 0.1 * A_init);
		problem.SetParameterUpperBound(fitParameters_peak1, 3, 10.0 * A_init);
		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.linear_solver_type = LinearSolverType.DENSE_QR;
		solverOptions.max_num_consecutive_invalid_steps = 1000;
		solverOptions.function_tolerance = 1.0E-8;
		solverOptions.parameter_tolerance = 1.0E-10;
		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		if (display > 0) {
			UI.setDataText(solverSummary.BriefReport() + "\n");
			UI.setDataText("Solved answer for A*((t-t0)^alpha)*exp(-(t-t0)/beta)\n");
			UI.setDataText("t0 = " + fitParameters_peak1[0] + "\n");
			UI.setDataText("alpha = " + fitParameters_peak1[1] + "\n");
			UI.setDataText("beta = " + fitParameters_peak1[2] + "\n");
			UI.setDataText("A = " + fitParameters_peak1[3] + "\n");
		}
		if (GVFittingCheck) {
			// Initial estimates for A*((t-t0)^alpha)*exp(-(t-t0)/beta)
			// t0_init = 23.25
			// alpha_init = 5.0
			// beta_init = 0.9300000000000004
			// A_init = 3.597861389964149
			// Ceres Solver Report: Iterations: 351, Initial cost: 2.477412e+06, Final cost:
			// 8.551629e+04, Termination: CONVERGENCE
			// Function tolerance reached.
			// |cost_change|/cost: 2.189536e-09 <= 1.000000e-08
			// Solved answer for A*((t-t0)^alpha)*exp(-(t-t0)/beta)
			// t0 = 21.90196100375891
			// alpha = 6.211813046079057
			// beta = 0.9739564489051871
			// A = 0.36594936256478594
			// ******* Elsunc gamma-variate Curve Fitting *********
			// analyticalJacobian = true
			// Number of iterations: 210
			// Chi-squared: 170852.8625953145
			// t0 21.89645002369929
			// // alpha 6.2260791592529925
			// beta 0.9725966077201533
			// A 0.35978613899641493
			// ******* Elsunc gamma-variate Curve Fitting *********
			// analyticalJacobian = false
			// Number of iterations: 210
			// Chi-squared: 170852.8625953156
			// t0 21.89645002617124
			// alpha 6.226079163642488
			// beta 0.9725966064905736
			// A 0.35978613899641493
			System.out.println("Initial estimates for A*((t-t0)^alpha)*exp(-(t-t0)/beta)");
			System.out.println("t0_init = " + t0_init);
			System.out.println("alpha_init = " + alpha_init);
			System.out.println("beta_init = " + beta_init);
			System.out.println("A_init = " + A_init);
			System.out.println(solverSummary.BriefReport());
			System.out.println(solverSummary.message[0]);
			System.out.println("Solved answer for A*((t-t0)^alpha)*exp(-(t-t0)/beta)");
			System.out.println("t0 = " + fitParameters_peak1[0]);
			System.out.println("alpha = " + fitParameters_peak1[1]);
			System.out.println("beta = " + fitParameters_peak1[2]);
			System.out.println("A = " + fitParameters_peak1[3]);

			boolean doAnalytical = true;
			fitParameters_peak1 = new double[] { t0_init, alpha_init, beta_init, A_init };
			GVFitting gv = new GVFitting(fitParameters_peak1, doAnalytical);
			gv.driver();
			gv.dumpResults();
			doAnalytical = false;
			fitParameters_peak1 = new double[] { t0_init, alpha_init, beta_init, A_init };
			gv = new GVFitting(fitParameters_peak1, doAnalytical);
			gv.driver();
			gv.dumpResults();
			System.exit(0);
		} // if (GVFittingCheck)
		double t0 = fitParameters_peak1[0];
		double alpha = fitParameters_peak1[1];
		double beta = fitParameters_peak1[2];
		double A = fitParameters_peak1[3];
		double tm;
		double J[][] = new double[nT][4];
		for (i = 0; i < nT; i++) {
			tm = time[i];
			if (tm > t0) {
				J[i][0] = (A / weights_peak1[i])
						* ((alpha * Math.pow((tm - t0), (alpha - 1.0)) * Math.exp(-(tm - t0) / beta))
								- (Math.pow((tm - t0), alpha) * Math.exp(-(tm - t0) / beta) / beta));
				J[i][1] = -(A / weights_peak1[i]) * Math.log(tm - t0) * Math.pow((tm - t0), alpha)
						* Math.exp(-(tm - t0) / beta);
				J[i][2] = -(A / weights_peak1[i]) * ((tm - t0) / (beta * beta)) * Math.pow((tm - t0), alpha)
						* Math.exp(-(tm - t0) / beta);
				J[i][3] = -(1.0 / weights_peak1[i]) * Math.pow((tm - t0), alpha) * Math.exp(-(tm - t0) / beta);
			} else {
				J[i][0] = 0.0;
				J[i][1] = 0.0;
				J[i][2] = 0.0;
				J[i][3] = 0.0;
			}
		}
		Matrix matJ = new Matrix(J);
		double JtJ[][] = ((matJ.transpose()).times(matJ)).getArray();
		double JtJcopy[][] = new double[4][4];
		for (i = 0; i < 4; i++) {
			for (j = 0; j < 4; j++) {
				JtJcopy[i][j] = JtJ[i][j];
			}
		}
		double covp[][] = new double[4][4];
		for (i = 0; i < 4; i++) {
	    	  covp[i][i] = 1.0;
	    }
		int ipiv[] = new int[Math.min(4,4)];
        int info[] = new int[1];
        boolean rankDeficient = false;
        int nrhs = 4; // number of columns of B
        le2.dgetrf(4,4,JtJcopy,4,ipiv,info);
        if (info[0] < 0) {
	    	  System.err.println("In fitGV_peak1 dgetrf argument number " + 
	      (-info[0]) + " is illegal");
	    	  return;
	      }
	      if (info[0] > 0) {
	    	  //System.err.println("In fitGV_peak2 dgetrf U["+(info[0]-1)+
	    			  //"]["+(info[0]-1)+"] is exactly 0");
	    	  rankDeficient = true;
	      }
	      if (!rankDeficient) {
	    	  char trans = 'N'; // Solve JtJ*X = Identity matrix (no transpose)
		      le2.dgetrs(trans,4,nrhs,JtJcopy,4,ipiv,covp,4,info);
		      if (info[0] < 0) {
		    	  System.err.println("In fitGV_peak1 dgetrs argument number " + 
		      (-info[0]) + " is illegal");
		    	  return;
		      }
	      } // if (!rankDeficient)
	      else {
			  double sing[] = new double[Math.min(4,4)];
			  double rcond = -1.0; // Singular values s(i) <= RCOND*s[0] are treated as zero.
				                   // If  rcond < 0, machine precision is used instead.
			  int rank[] = new int[1];
			  double work[] = new double[1];
			  int lwork_query = -1;
			  int lwork;
		      for (i = 0; i < 4; i++) {
		          for (j = 0; j < 4; j++) {
		        	  JtJcopy[i][j] = JtJ[i][j];
		          }
		      }
			  gi2.dgelss(4,4,nrhs,JtJcopy,4,covp,4,sing,rcond,rank,work,lwork_query,info);
			  if (info[0] != 0) {
			        System.err.println("fitGV_peak1: LAPACK dgelss work query error code = " + info[0]);
			        return;
				}
				lwork = (int)work[0];
	
				// Allocate the workspace
				try {
					work = new double[lwork];
				}
				catch (OutOfMemoryError e) {
					return;	
				}
				// This routine must handle rank deficient matrices
				// since dgetrf does not handle rank deficient matrices.
				gi2.dgelss(4,4,nrhs,JtJcopy,4,covp,4,sing,rcond,rank,work,lwork,info);
				if (info[0] < 0) {
					System.err.println("In fitGV_peak1 for dgelss the (-info[0]) argument had an illegal value");
					return;
				}
				if (info[0] > 0) {
					System.err.println("In fitGV_peak1 the algorithm for computing the SVD failed to converge");
					System.err.println(info[0] + " off-diagonal elements of an intermediate bidiagonal form did not converge to zero.");
					return;
				}
			    
	      }
		double var[] = new double[] { covp[0][0], covp[1][1], covp[2][2], covp[3][3] };
		double sd[] = new double[] { Math.sqrt(var[0]), Math.sqrt(var[1]), Math.sqrt(var[2]), Math.sqrt(var[3]) };
		cv_est_parGV_peak1 = new double[4];
		for (i = 0; i < 4; i++) {
			cv_est_parGV_peak1[i] = sd[i] / fitParameters_peak1[i] * 100.0;
		}
		if (display > 2) {
			GV = GVfunction_peak1(fitParameters_peak1);
			float timef[] = new float[nT];
			for (i = 0; i < nT; i++) {
				timef[i] = (float) time[i];
			}
			float GVf[] = new float[nT];
			for (i = 0; i < nT; i++) {
				GVf[i] = (float) GV[i];
			}
			ViewJFrameGraph firstPeakGraph = new ViewJFrameGraph(timef, GVf, "First peak final fit", "Time", "GV fit");
			firstPeakGraph.setVisible(true);
			try {
				firstPeakGraph.save(outputFilePath + outputPrefix + "firstPeakGraph.plt");
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			Component component = firstPeakGraph.getComponent(0);
			Rectangle rect = component.getBounds();
			String format = "png";
			BufferedImage captureImage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_ARGB);
			component.paint(captureImage.getGraphics());

			File firstPeakGraphFile = new File(outputFilePath + outputPrefix + "firstPeakGraph.png");
			try {
				ImageIO.write(captureImage, format, firstPeakGraphFile);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			firstPeakGraph.removeComponentListener();
			firstPeakGraph.dispose();
			captureImage.flush();
		} // if (display > 2)
	} // fitGV_peak1

	private void fitGV_peak2(double dati[], double weights[]) {
		// Calculate the fit with a gamma variate function
		// The function used described by the formula:

		// FP(t) = A*((t-t0)^alpha)*exp(-(t-t0)/beta)

		// c(t) = FP(t) + FP(t-td) conv K*exp(-t/tao)

		// Parameters: p = [t0 alpha beta A td K tao]

		// I fill in the data for the fit
		LinearEquations2 le2 = new LinearEquations2();
		GeneralizedInverse2 gi2 = new GeneralizedInverse2();
		double peak1[] = GVfunction_peak1(fitParameters_peak1);
		// The data to fit are the residues of the first fit
		int i, j, t;
		dati_peak2 = new double[nT];
		for (t = 0; t < nT; t++) {
			dati_peak2[t] = dati[t] - peak1[t];
		}

		// Uniform weights for the calculation of the fit
		weights_peak2 = new double[nT];
		for (t = 0; t < nT; t++) {
			weights_peak2[t] = 1.0;
		}

		double maxDati = -Double.MAX_VALUE;
		int maxT = -1;
		for (t = 0; t < nT; t++) {
			if (dati[t] > maxDati) {
				maxDati = dati[t];
				maxT = t;
			}
		}
		int lastT = -1;
		for (t = 0; t < nT; t++) {
			if (dati[t] > 0.4 * maxDati) {
				lastT = t;
			}
		}
		int posTaglioWeights = Math.min(lastT, maxT + 3);
		// I reduce the weight of the data before the main peak. I arrive
		// until the concentration has dropped below 40% of the peak to avoid cases
		// where the main peak does not fit well the post-peak data abd the
		// residuals may show a bogus peak.
		// for (t = 0; t <= posTaglioWeights; t++) {
		// weights_peak2[t] = 1.0;
		// }

		// Parameter initialization
		// Data based search for initial points. I only consider data from the instant
		// in
		// which the concentrations fall below 40% of the peak to avoid too much noisy
		// residues
		// not related to the recirculation. The fit is done with all the residues.
		double dati_x_stime_init[] = new double[nT];
		for (t = 0; t < nT; t++) {
			dati_x_stime_init[t] = dati_peak2[t];
		}
		for (t = 0; t <= posTaglioWeights; t++) {
			dati_x_stime_init[t] = 0;
		}
		for (t = posTaglioWeights + 1; t < nT; t++) {
			if (dati_x_stime_init[t] < 0) {
				dati_x_stime_init[t] = 0;
			}
		}
		// td_init is calculated as the distance between the instant of the main peak
		// and the
		// distance of the recirculation peak. The recirculation peak was identified as
		// the data
		// peak minus the prediction of the main peak.
		double maxPeak2 = -Double.MAX_VALUE;
		int TTPpeak2 = -1;
		for (t = 0; t < nT; t++) {
			if (dati_x_stime_init[t] > maxPeak2) {
				maxPeak2 = dati_x_stime_init[t];
				TTPpeak2 = t;
			}
		}
		int t0peak2 = -1;
		for (t = 0; t <= TTPpeak2; t++) {
			if (dati_x_stime_init[t] < 0.1 * maxPeak2) {
				t0peak2 = t;
			}
		}
		// If not time is less than 0.1 * maxPeak2, use the time with the minimum value in this range
		if (t0peak2 == -1) {
		    double minPeak2 = Double.MAX_VALUE;
		    for (t = 0; t <= TTPpeak2; t++) {
		    	if (dati_x_stime_init[t] < minPeak2) {
		    		minPeak2 = dati_x_stime_init[t];
		    		t0peak2 = t;
		    	}
		    }
		}
		td_init = time[t0peak2] - fitParameters_peak1[0];

		// The initial estimate of tao fixed. 100 mamnages to give a well spread
		// recirculation and that with the bounds can become both a zero dispersion
		// and lead to an almost complete dispersion.
		tao_init = 40.0;
		// The estimate of K is made so that the maxima of the predicted
		// recirculation and of the fitted data are equal.
		double xp[] = new double[] { fitParameters_peak1[0], fitParameters_peak1[1], fitParameters_peak1[2],
				fitParameters_peak1[3], td_init, 1.0, tao_init };
		double recirculation[] = GVfunction_recirculation(xp);
		double maxRecirculation = -Double.MAX_VALUE;
		for (t = 0; t < nT; t++) {
			if (recirculation[t] > maxRecirculation) {
				maxRecirculation = recirculation[t];
			}
		}
		K_init = maxPeak2 / maxRecirculation;

		fitParameters_peak2 = new double[] { td_init, K_init, tao_init }; // Initial values
		if (display > 0) {
			UI.setDataText("Initial estimates for FP(t-td) convolution K*exp(-t/tao)\n");
			UI.setDataText("FP(t) = A*((t-t0)^alpha)*exp(-(t-t0)/beta)\n");
			UI.setDataText("td = " + fitParameters_peak2[0] + "\n");
			UI.setDataText("K = " + fitParameters_peak2[1] + "\n");
			UI.setDataText("tao = " + fitParameters_peak2[2] + "\n");
		}

		CostFunction cost_function = new GVRecirculationCostFunction();
		ProblemImpl problem = new ProblemImpl();
		problem.AddResidualBlock(cost_function, null, fitParameters_peak2);
		problem.AddParameterBlock(fitParameters_peak2, 3);
		problem.SetParameterLowerBound(fitParameters_peak2, 0, 0.1 * td_init);
		problem.SetParameterUpperBound(fitParameters_peak2, 0, 10.0 * td_init);
		problem.SetParameterLowerBound(fitParameters_peak2, 1, 0.1 * K_init);
		problem.SetParameterUpperBound(fitParameters_peak2, 1, 10.0 * K_init);
		problem.SetParameterLowerBound(fitParameters_peak2, 2, 0.1 * tao_init);
		problem.SetParameterUpperBound(fitParameters_peak2, 2, 10.0 * tao_init);
		// Run the solver!
		SolverOptions solverOptions = new SolverOptions();
		solverOptions.linear_solver_type = LinearSolverType.DENSE_QR;
		solverOptions.max_num_consecutive_invalid_steps = 1000;
		solverOptions.function_tolerance = 1.0E-8;
		solverOptions.parameter_tolerance = 1.0E-10;
		solverOptions.minimizer_progress_to_stdout = true;
		SolverSummary solverSummary = new SolverSummary();
		Solve(solverOptions, problem, solverSummary);
		if (display > 0) {
			UI.setDataText(solverSummary.BriefReport() + "\n");
			UI.setDataText("Solved answer for FP(t-td) convolution K*exp(-t/tao)\n");
			UI.setDataText("FP(t) = A*((t-t0)^alpha)*exp(-(t-t0)/beta)\n");
			UI.setDataText("td = " + fitParameters_peak2[0] + "\n");
			UI.setDataText("K = " + fitParameters_peak2[1] + "\n");
			UI.setDataText("tao = " + fitParameters_peak2[2] + "\n");
		}
		if (GVRecirculationCheck) {
			// Initial estimates for FP(t-td) convolution K*exp(-t/tao)
			// FP(t) = A*((t-t0)^alpha)*exp(-(t-t0)/beta)
			// td_init = 19.76651061960303
			// K_init = 0.018402223739224195
			// tao_init = 40.0
			// Ceres Solver Report: Iterations: 31, Initial cost: 5.944661e+02, Final cost:
			// 2.525571e+02, Termination: CONVERGENCE
			// Minimum trust region radius reached.
			// Trust region radius: 3.895532391309105E-35 <= 1.0E-32
			// Solved answer for FP(t-td) convolution K*exp(-t/tao)
			// td = 15.18426191214277
			// K = 0.012837854971044015
			// tao = 38.10774224717836
			// ******* Elsunc GV recirculation Curve Fitting *********
			// analyticalJacobian = true
			// Number of iterations: 8
			// Chi-squared: 505.0804959002924
			// td 15.191393614623278
			// K 0.012944380197663731
			// tao 37.61800971361649
			// ******* Elsunc GV recirculation Curve Fitting *********
			// analyticalJacobian = false
			// Number of iterations: 8
			// Chi-squared: 505.08049590029225
			// td 15.191393550184474
			// K 0.012944380101420678
			// tao 37.61801026358771

			System.out.println("Initial estimates for FP(t-td) convolution K*exp(-t/tao)");
			System.out.println("FP(t) = A*((t-t0)^alpha)*exp(-(t-t0)/beta)");
			System.out.println("td_init = " + td_init);
			System.out.println("K_init = " + K_init);
			System.out.println("tao_init = " + tao_init);
			System.out.println(solverSummary.BriefReport());
			System.out.println(solverSummary.message[0]);
			System.out.println("Solved answer for FP(t-td) convolution K*exp(-t/tao)");
			System.out.println("td = " + fitParameters_peak2[0]);
			System.out.println("K = " + fitParameters_peak2[1]);
			System.out.println("tao = " + fitParameters_peak2[2]);

			boolean doAnalytical = true;
			fitParameters_peak2 = new double[] { td_init, K_init, tao_init };
			GVRecirculation gv = new GVRecirculation(fitParameters_peak2, doAnalytical);
			gv.driver();
			gv.dumpResults();
			doAnalytical = false;
			fitParameters_peak2 = new double[] { td_init, K_init, tao_init };
			gv = new GVRecirculation(fitParameters_peak2, doAnalytical);
			gv.driver();
			gv.dumpResults();
			System.exit(0);
		} // if (GVRecirculationCheck)

		double t0 = fitParameters_peak1[0];
		double alpha = fitParameters_peak1[1];
		double beta = fitParameters_peak1[2];
		double td = fitParameters_peak2[0];
		double K = fitParameters_peak2[1];
		double tao = fitParameters_peak2[2];
		double J[][] = new double[nT][3];

		// Vector initialization
		double peak2[] = new double[nTfine]; // Peak of recirculation
		double disp[] = new double[nTfine]; // Dispersion of recirculation

		double tg;
		for (i = 0; i < nTfine; i++) {
			tg = tGrid[i];

			if (tg > (t0 + td)) {
				// Calculation of FP(t-td)
				peak2[i] = K * Math.pow((tg - t0 - td), alpha) * Math.exp(-(tg - t0 - td) / beta);
			}

			// Calculation of disp(t)
			disp[i] = Math.exp(-tg / tao);
		}

		double dpeak2dtd[] = new double[nTfine];
		double dpeak2dK[] = new double[nTfine];
		double ddispdtao[] = new double[nTfine];
		for (i = 0; i < nTfine; i++) {
			tg = tGrid[i];

			if (tg > (t0 + td)) {
				// Calculation of FP(t-td)
				dpeak2dtd[i] = K * alpha * Math.pow((tg - t0 - td), (alpha - 1.0)) * Math.exp(-(tg - t0 - td) / beta)
						- (K / beta) * Math.pow((tg - t0 - td), alpha) * Math.exp(-(tg - t0 - td) / beta);
				dpeak2dK[i] = (-1.0) * Math.pow((tg - t0 - td), alpha) * Math.exp(-(tg - t0 - td) / beta);
			}

			// Calculation of disp(t)
			ddispdtao[i] = (-tg / (tao * tao)) * Math.exp(-tg / tao);
		}

		// 3.) I assemble the components to obtainthe GV calculated on the fine grid
		double recirculation_fine_td[] = new double[nTfine];
		double recirculation_fine_K[] = new double[nTfine];
		double recirculation_fine_tao[] = new double[nTfine];
		for (i = 0; i < nTfine; i++) {
			for (j = 0; j <= i; j++) {
				recirculation_fine_td[i] += dpeak2dtd[j] * disp[i - j];
				recirculation_fine_K[i] += dpeak2dK[j] * disp[i - j];
				recirculation_fine_tao[i] += peak2[j] * ddispdtao[i - j];
			}
			recirculation_fine_td[i] *= TRfine;
			recirculation_fine_K[i] *= TRfine;
			recirculation_fine_tao[i] *= TRfine;
		}

		for (t = 0; t < nT; t++) {
			double pos = 2.0 * (time[t] - Tmin) / TRfine;
			int lowIndex = (int) Math.floor(pos);
			int highIndex = Math.min((int) Math.ceil(pos), nTfine - 1);
			if (lowIndex == highIndex) {
				J[t][0] = recirculation_fine_td[lowIndex] / weights_peak2[t];
				J[t][1] = recirculation_fine_K[lowIndex] / weights_peak2[t];
				J[t][2] = recirculation_fine_tao[lowIndex] / weights_peak2[t];
			} else {
				double lowFraction = pos - lowIndex;
				double highFraction = highIndex - pos;
				J[t][0] = (lowFraction * recirculation_fine_td[highIndex]
						+ highFraction * recirculation_fine_td[lowIndex]) / weights_peak2[t];
				J[t][1] = (lowFraction * recirculation_fine_K[highIndex]
						+ highFraction * recirculation_fine_K[lowIndex]) / weights_peak2[t];
				J[t][2] = (lowFraction * recirculation_fine_tao[highIndex]
						+ highFraction * recirculation_fine_tao[lowIndex]) / weights_peak2[t];
			}
		}

		Matrix matJ = new Matrix(J);
		double JtJ[][] = ((matJ.transpose()).times(matJ)).getArray();
		double JtJcopy[][] = new double[3][3];
		for (i = 0; i < 3; i++) {
			for (j = 0; j < 3; j++) {
				JtJcopy[i][j] = JtJ[i][j];
			}
		}
		double covp[][] = new double[3][3];
		for (i = 0; i < 3; i++) {
	    	  covp[i][i] = 1.0;
	    }
		int ipiv[] = new int[Math.min(3,3)];
        int info[] = new int[1];
        boolean rankDeficient = false;
        int nrhs = 3; // number of columns of B
        le2.dgetrf(3,3,JtJcopy,3,ipiv,info);
        if (info[0] < 0) {
	    	  System.err.println("In fitGV_peak2 dgetrf argument number " + 
	      (-info[0]) + " is illegal");
	    	  return;
	      }
	      if (info[0] > 0) {
	    	  //System.err.println("In fitGV_peak2 dgetrf U["+(info[0]-1)+
	    			  //"]["+(info[0]-1)+"] is exactly 0");
	    	  rankDeficient = true;
	      }
	      if (!rankDeficient) {
	    	  char trans = 'N'; // Solve JtJ*X = Identity matrix (no transpose)
		      le2.dgetrs(trans,3,nrhs,JtJcopy,3,ipiv,covp,3,info);
		      if (info[0] < 0) {
		    	  System.err.println("In fitGV_peak2 dgetrs argument number " + 
		      (-info[0]) + " is illegal");
		    	  return;
		      }
	      } // if (!rankDeficient)
	      else {
			  double sing[] = new double[Math.min(3,3)];
			  double rcond = -1.0; // Singular values s(i) <= RCOND*s[0] are treated as zero.
				                   // If  rcond < 0, machine precision is used instead.
			  int rank[] = new int[1];
			  double work[] = new double[1];
			  int lwork_query = -1;
			  int lwork;
		      for (i = 0; i < 3; i++) {
		          for (j = 0; j < 3; j++) {
		        	  JtJcopy[i][j] = JtJ[i][j];
		          }
		      }
			  gi2.dgelss(3,3,nrhs,JtJcopy,3,covp,3,sing,rcond,rank,work,lwork_query,info);
			  if (info[0] != 0) {
			        System.err.println("fitGV_peak2: LAPACK dgelss work query error code = " + info[0]);
			        return;
				}
				lwork = (int)work[0];
	
				// Allocate the workspace
				try {
					work = new double[lwork];
				}
				catch (OutOfMemoryError e) {
					return;	
				}
				// This routine must handle rank deficient matrices
				// since dgetrf does not handle rank deficient matrices.
				gi2.dgelss(3,3,nrhs,JtJcopy,3,covp,3,sing,rcond,rank,work,lwork,info);
				if (info[0] < 0) {
					System.err.println("In fitGV_peak2 for dgelss the (-info[0]) argument had an illegal value");
					return;
				}
				if (info[0] > 0) {
					System.err.println("In fitGV_peak2 the algorithm for computing the SVD failed to converge");
					System.err.println(info[0] + " off-diagonal elements of an intermediate bidiagonal form did not converge to zero.");
					return;
				}
			    
	      }
		double var[] = new double[] { covp[0][0], covp[1][1], covp[2][2] };
		double sd[] = new double[] { Math.sqrt(var[0]), Math.sqrt(var[1]), Math.sqrt(var[2]) };
		cv_est_parGV_peak2 = new double[3];
		for (i = 0; i < 3; i++) {
			cv_est_parGV_peak2[i] = sd[i] / fitParameters_peak2[i] * 100.0;
		}
		if (display > 2) {
			double p[] = new double[7];
			for (i = 0; i < 4; i++) {
				p[i] = fitParameters_peak1[i];
			}
			for (i = 0; i < 3; i++) {
				p[i + 4] = fitParameters_peak2[i];
			}
			double[] GV = GVfunction_recirculation(p);
			float timef[] = new float[nT];
			for (i = 0; i < nT; i++) {
				timef[i] = (float) time[i];
			}
			float GVf[] = new float[nT];
			for (i = 0; i < nT; i++) {
				GVf[i] = (float) GV[i];
			}
			ViewJFrameGraph recirculationGraph = new ViewJFrameGraph(timef, GVf, "Recirculation final fit", "Time",
					"GVrecirculation fit");
			recirculationGraph.setVisible(true);
			try {
				recirculationGraph.save(outputFilePath + outputPrefix + "recirculationGraph.plt");
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			Component component = recirculationGraph.getComponent(0);
			Rectangle rect = component.getBounds();
			String format = "png";
			BufferedImage captureImage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_ARGB);
			component.paint(captureImage.getGraphics());

			File recirculationGraphFile = new File(outputFilePath + outputPrefix + "recirculationGraph.png");
			try {
				ImageIO.write(captureImage, format, recirculationGraphFile);
			} catch (IOException e) {
				System.err.println("IOException " + e);
				return;
			}
			recirculationGraph.removeComponentListener();
			recirculationGraph.dispose();
			captureImage.flush();
		} // if (display > 2)
	} // fitGV_peak2

	private double[] GVfunction_recirculation(double p[]) {
		// Compute the gamma-variate function that describes the recirculation of
		// concentrations and defined by the parameters contained in p.
		// The gamma-variate function defined by the formula:
		
		// FP(t) = A*((t-t0)^alpha)*exp(-(t-t0)/beta)
		
		// recirculation(t) = FP(t-td) conv K*exp(-t/tao)
		
		// The parameters are passed in the following order:
		// p = [t0 alpha beta A td K tao]
		
		// Since the formula predicts a convolution, the time grid
		// along which the much denser gamma variate than the final grid is calculated
		double t0 = p[0];
		double alpha = p[1];
		double beta = p[2];
		double A = p[3];
		double td = p[4];
		double K = p[5];
		double tao = p[6];
		int i, j, t;
		
		// 1.) Definition of the virtual grid necessary for convolution
		// Performed in DSC_mri_core to prevent unnecessary repetitions
		//double TR = time[1] - time[0];
		//double Tmax = -Double.MAX_VALUE;
		//double Tmin = Double.MAX_VALUE;
		
		//for (t = 0; t < nT; t++) {
		//	if (time[t] < Tmin) {
		//		Tmin = time[t];
		//	}
		//	if (time[t] > Tmax) {
		//		Tmax = time[t];
		//	}
		//}
		
		//double TRfine = TR/10.0;
		//int nTfine = 1 + (int)((2*Tmax - Tmin)/TRfine);
		//double tGrid[] = new double[nTfine];
		//for (t = 0; t < nTfine; t++) {
		//	tGrid[t] = Tmin + t*TRfine;
		//}
		
		// Calculate the funcions necessary for the recirculation calculation
		// FP(t) = A*((t-t0)^alpha)*exp(-(t-t0)/beta)
		// disp(t) = exp(-t/tao)
		// recirculation(t) = K * [FP(t-td) convolution disp(t)]
		
		// Vector initialization
		double peak2[] = new double[nTfine]; // Peak of recirculation
		double disp[] = new double[nTfine]; // Dispersion of recirculation
		
		double tg;
		for (i = 0; i < nTfine; i++) {
			tg = tGrid[i];
			
			if (tg > (t0+td)) {
				// Calculation of FP(t-td)
				peak2[i] = K*Math.pow((tg-t0-td),alpha)*Math.exp(-(tg-t0-td)/beta);
			}
			
			// Calculation of disp(t)
			disp[i] = Math.exp(-tg/tao);
		}
		
		// 3.) I assemble the components to obtainthe GV calculated on the fine grid
		double recirculation_fine[] = new double[nTfine];
		for (i = 0; i < nTfine; i++) {
			for (j = 0; j <= i; j++) {
				recirculation_fine[i] += peak2[j] * disp[i - j];
			}
			recirculation_fine[i] *= TRfine;
		}
		
		// 4.) I'm going to sample GV on the time instants requeted in time
		double recirculation[] = new double[nT];
		for (t = 0; t < nT; t++) {
			double pos = 2.0*(time[t] - Tmin)/TRfine;
			int lowIndex = (int)Math.floor(pos);
			int highIndex = Math.min((int)Math.ceil(pos),nTfine-1);
			if (lowIndex == highIndex) {
				recirculation[t] = recirculation_fine[lowIndex];
			}
			else {
				double lowFraction = pos - lowIndex;
				double highFraction = highIndex - pos;
				recirculation[t] = lowFraction*recirculation_fine[highIndex] + highFraction*recirculation_fine[lowIndex];
			}
		}
		return recirculation;
	}

	private double[] GVfunction_peak1(double p[]) {
		// Compute the gamma-variate function defined by the parameters contained in p.
		// The gamma-variate function defined by the formula:

		// GV[t] = A*((t_t0)^alpha)*exp(-(t-t0)/beta)

		// parameters : p = [t0 alpha beta A]
		double t0 = p[0];
		double alpha = p[1];
		double beta = p[2];
		double A = p[3];

		double GV[] = new double[nT];
		int cont;
		for (cont = 0; cont < nT; cont++) {
			double t = time[cont];
			if (t > t0) {
				GV[cont] = A * Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta);
			}
		}
		return GV;
	}

	private void clusterHierarchical(double dati[][], int nCluster, double centroidi[][]) {
		// Apply the hierarchical cluster algorithm to the data and divide it into
		// nCluster.
		// Returns a vector containing the number of the cluster to which each voxel has
		// been assigned and the centroid of that cluster.
		int distanceNumber = dati.length * (dati.length - 1) / 2;
		double distance[] = new double[distanceNumber];
		int i, j, t, index;
		double distanceSquared;
		double diff;
		/*
		 * for (index = 0, i = 0; i < dati.length-1; i++) { for (j = i+1; j <
		 * dati.length; j++) { distanceSquared = 0.0; for (t = 0; t < nT; t++) { diff =
		 * dati[i][t] - dati[j][t]; distanceSquared += diff*diff; } distance[index++] =
		 * Math.sqrt(distanceSquared); } }
		 */
	}

	private double[][] calculateREG(byte mask[][]) {
		// Caclulates the irregularity index of the concentration curve for each voxel.
		// The index is calculated by normalizing the area so as not to penalize areas
		// with elevated voxels.
		// The formula used to calculate the index:
		// CTC = Integral((C"(t)^2 dt)
		double y[][][] = new double[nC][nR][nT];
		double derivative2[][][] = new double[nC][nR][nT];
		double AUC;
		int c, r, t;
		double timeDiff;
		double REG[][];

		for (c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				AUC = 0.0;
				for (t = 0; t < nT; t++) {
					y[c][r][t] = AIFslice[c][r][t];
					AUC += y[c][r][t];
				}
				if (AUC == 0.0) {
					AUC = 1.0;
				}
				for (t = 0; t < nT; t++) {
					y[c][r][t] = y[c][r][t] / AUC;
				}
			}
		}

		// Calculation of the second derivative
		for (c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				if (mask[c][r] == 1) {
					for (t = 0; t < nT; t++) {
						if ((t > 0) && (t < nT - 1)) {
							// Standard case
							derivative2[c][r][t] = (y[c][r][t + 1] - y[c][r][t]) / (time[t + 1] - time[t])
									- (y[c][r][t] - y[c][r][t - 1]) / (time[t] - time[t - 1]);
						} else if (t == 0) {
							// The previous sample is missing
							timeDiff = time[t + 1] - time[t];
							derivative2[c][r][t] = (y[c][r][t + 1] - y[c][r][t]) / (timeDiff * timeDiff);
						} else {
							// The next sample is missing
							timeDiff = time[t] - time[t - 1];
							derivative2[c][r][t] = (y[c][r][t] - y[c][r][t - 1]) / (timeDiff * timeDiff);
						}
					}
				}
			}
		} // for (c = 0; c < nC; c++)

		// Calculation of the irregularity index
		for (c = 0; c < nC; c++) {
			for (r = 0; r < nR; r++) {
				for (t = 0; t < nT; t++) {
					derivative2[c][r][t] = derivative2[c][r][t] * derivative2[c][r][t];
				}
			}
		} // for (c = 0; c < nC; c++)

		REG = trapz(derivative2);
		return REG;
	}
	
	private double[] cumtrapz(double f[]) {
		double REG[] = new double[nT];
		double scale;
		int t;
		double sum = 0.0;
		if (equalTimeSpacing) {
			scale = (time[nT-1] - time[0]) / (2.0* (nT-1));
			for (t = 0; t < nT - 1; t++) {
				REG[t+1] = REG[t] + scale * (f[t] + f[t+1]);
			}
		}
		else {
			for (t = 0; t < nT - 1; t++) {
				scale = (time[t+1] - time[t])/2.0;
				REG[t+1] = REG[t] + scale * (f[t] + f[t+1]);
			}
		}
		return REG;
	}
	
	private double trapz(double f[]) {
		double REG;
		int t;
		double sum;
		double scale;
		if (equalTimeSpacing) {
			scale = (time[nT - 1] - time[0]) / (2.0 * (nT-1));
			sum = f[0] + f[nT - 1];
			for (t = 1; t < nT - 1; t++) {
				sum += 2.0 * f[t];
			}
			REG = scale * sum;
		} // if (equalTimeSpacing)
		else {
		    REG = 0.0;
			for (t = 0; t < nT - 1; t++) {
				REG += (time[t + 1] - time[t]) * (f[t] + f[t + 1]);
			}
			REG = 0.5 * REG;
		}
		return REG;
	}

	private double[][] trapz(double f[][][]) {
		double REG[][] = new double[nC][nR];
		int c, r, t;
		double sum;
		double scale;
		if (equalTimeSpacing) {
			scale = (time[nT - 1] - time[0]) / (2.0 * (nT-1));
			for (c = 0; c < nC; c++) {
				for (r = 0; r < nR; r++) {
					sum = f[c][r][0] + f[c][r][nT - 1];
					for (t = 1; t < nT - 1; t++) {
						sum += 2.0 * f[c][r][t];
					}
					REG[c][r] = scale * sum;
				}
			}
		} // if (equalTimeSpacing)
		else {
			for (c = 0; c < nC; c++) {
				for (r = 0; r < nR; r++) {
					for (t = 0; t < nT - 1; t++) {
						REG[c][r] += (time[t + 1] - time[t]) * (f[c][r][t] + f[c][r][t + 1]);
					}
					REG[r][r] = 0.5 * REG[c][r];
				}
			}
		}
		return REG;
	}

	private File saveImageFile(final ModelImage img, final String dir, final String fileBasename, int fileType) {
		return saveImageFile(img, dir, fileBasename, fileType, false);
	}

	private File saveImageFile(final ModelImage img, final String dir, final String fileBasename, int fileType,
			boolean alwaysSave) {
		if (fileIO == null) {
			fileIO = new FileIO();
			fileIO.setQuiet(true);
		}

		// if no directory specified, skip writing out images
		// or if option is set and this is a file that is optionally written out
		if (dir == null || (!alwaysSave && !doSaveAllOutputs)) {
			return null;
		}

		FileWriteOptions opts = new FileWriteOptions(true);
		opts.setFileDirectory(dir);

		if (img.getNDims() == 3) {
			opts.setBeginSlice(0);
			opts.setEndSlice(img.getExtents()[2] - 1);
		} else if (img.getNDims() == 4) {
			opts.setBeginSlice(0);
			opts.setEndSlice(img.getExtents()[2] - 1);
			opts.setBeginTime(0);
			opts.setEndTime(img.getExtents()[3] - 1);
		}

		opts.setFileType(fileType);
		final String ext = FileTypeTable.getFileTypeInfo(fileType).getDefaultExtension();
		opts.setFileName(fileBasename + ext);

		opts.setOptionsSet(true);
		opts.setMultiFile(false);

		fileIO.writeImage(img, opts, false, false);

		return new File(dir + File.separator + fileBasename + ext);
	}

	// Output zero edge crossings of second order derivative of 1D Gaussian of
	// buffer
	public byte[] calcZeroX(double[] buffer) {
		makeGxxKernels1D();
		double[] secondDerivBuffer = new double[buffer.length];
		convolve(buffer, GxxData, secondDerivBuffer);
		return edgeDetect(secondDerivBuffer);
	}

	public byte[] edgeDetect(double secondDerivBuffer[]) {
		int i, j;
		double x0;
		double x1;
		int xDim = secondDerivBuffer.length;
		int xxDim = xDim - 1;
		byte edgeDetectBuffer[] = new byte[xDim];
		for (i = 0; i < xxDim; i++) {
			x0 = secondDerivBuffer[i];
			x1 = secondDerivBuffer[i + 1];
			if ((x0 > 0) && (x1 > 0)) {
				edgeDetectBuffer[i] = 0;
			} else if ((x0 < 0) && (x1 < 0)) {
				edgeDetectBuffer[i] = 0;
			}
			// else if (i == 0) {
			// A false contour is easy to detect because it always appears
			// in either the first or the last column of the space-scale
			// image in high resolution.
			// edgeDetectBuffer[i] = 0;
			// }
			else {
				edgeDetectBuffer[i] = 1;
			}

		}
		if (!test2PerfectGaussians) {
			for (i = 0; i <= firstGaussianMeanBin + 3; i++) {
				edgeDetectBuffer[i] = 0;
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
		xkDim = (int) Math.round(16 * sigmas[0]);

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

		xkDim = (int) Math.round(16 * sigmas[0]);

		if ((xkDim % 2) == 0) {
			xkDim++;
		}

		if (xkDim < 3) {
			xkDim = 3;
		}

		kExtents[0] = xkDim;

		GxData = new double[xkDim];
		GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);
		Gx.dcalc(true);
		Gx.finalize();
		Gx = null;
	}

	/**
	 * Perform one-dimension convolution.
	 * 
	 * @param imageBuffer
	 * @param kernelBuffer
	 * @param resultBuffer Problem is huge border effects. Convolution uses kernels
	 *                     of length going from 9 to 801 on buffer of length 100.
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
			double val1 = (x[i] - param[1]) / param[2];
			y1[i] = param[0] * Math.exp(-val1 * val1);
		}
		double y2[] = new double[x.length];
		for (i = 0; i < x.length; i++) {
			double val2 = (x[i] - param[4]) / param[5];
			y2[i] = param[3] * Math.exp(-val2 * val2);
		}
		double x1in[];
		double y1in[];
		double x2in[];
		double y2in[];
		double ymin;
		double ymax;
		double diff_x[] = new double[x.length - 1];
		for (i = 0; i < x.length - 1; i++) {
			diff_x[i] = x[i + 1] - x[i];
		}
		int ind_x[] = new int[x.length - 1];
		for (i = 0; i < x.length - 1; i++) {
			if (diff_x[i] > 0) {
				ind_x[i] = 1;
			} else if (diff_x[i] == 0.0) {
				ind_x[i] = 0;
			} else {
				ind_x[i] = -1;
			}
		}

		int ind1 = 0;
		while (ind1 < x.length - 1) {
			boolean found = false;
			int ind_max = ind1 - 1;
			for (i = ind1 + 1; (i < ind_x.length) && (!found); i++) {
				if (ind_x[i] != ind_x[ind1]) {
					found = true;
					ind_max = ind1 + i - 1;
				}
			}
			if (ind_max <= ind1) {
				ind_max = x.length - 1;
			}
			int[] ind1_array = new int[ind_max - ind1 + 1];
			for (i = 0; i < ind1_array.length; i++) {
				ind1_array[i] = ind1 + i;
			}

			int ind2 = 0;
			while (ind2 < x.length - 1) {
				found = false;
				ind_max = ind2 - 1;
				for (i = ind2 + 1; (i < ind_x.length) && (!found); i++) {
					if (ind_x[i] != ind_x[ind2]) {
						found = true;
						ind_max = ind2 + i - 1;
					}
				}
				if (ind_max <= ind2) {
					ind_max = x.length - 1;
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
				} else if ((ind_x[ind2_array[0]] == 0) && (ind_x[ind1_array[0]] != 0)) {
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
				} else if ((ind_x[ind2_array[0]] != 0) && (ind_x[ind1_array[0]] != 0)) {
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
				} else if ((ind_x[ind2_array[0]] == 0) && (ind_x[ind1_array[0]] == 0)) {
					x_loc = null;
					y_loc = null;
				}
				if ((x_out == null) && (x_loc != null)) {
					x_out = new double[x_loc.length];
					for (i = 0; i < x_loc.length; i++) {
						x_out[i] = x_loc[i];
					}
				} else if ((x_out != null) && (x_loc != null)) {
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
				} else if ((y_out != null) && (y_loc != null)) {
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
				ind2 = ind2_array[ind2_array.length - 1];
			}
			ind1 = ind1_array[ind1_array.length - 1];
		}
	}

	public void curveintersect_local(double x1[], double y1[], double x2[], double y2[]) {
		int i, j;
		boolean equalX = true;
		if (x1.length != x2.length) {
			equalX = false;
		} else {
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
			Vector<Double> xxVec = new Vector<Double>();
			for (i = 0; i < x1.length; i++) {
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
			double yy1[] = interp1(x1, y1, xx);
			double yy2[] = interp1(x2, y2, xx);
			yy = new double[yy1.length];
			for (i = 0; i < yy.length; i++) {
				yy[i] = yy1[i] - yy2[i];
			}
		} else {
			xx = x1;
			yy = new double[y1.length];
			for (i = 0; i < yy.length; i++) {
				yy[i] = y1[i] - y2[i];
			}
		}
		mminvinterp(xx, yy, 0.0); // find zero crossings of difference
		if ((x_loc != null) && (x_loc.length > 0)) {
			y_loc = interp1(x1, y1, x_loc);
		} else {
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
			} else if (y[i] > yo) {
				above[i] = true;
			} else {
				on[i] = true;
			}
		}

		boolean kth[] = new boolean[n - 1]; // point k
		for (i = 0; i < n - 1; i++) {
			kth[i] = ((below[i] & above[i + 1]) | (above[i] & below[i + 1]));
		}
		boolean kp1[] = new boolean[n]; // point k+1
		kp1[0] = false;
		for (i = 1; i < n; i++) {
			kp1[i] = kth[i - 1];
		}

		Vector<Double> xo = new Vector<Double>(); // distance between x[k+1] and x[k]
		for (i = 0; i < n - 1; i++) {
			if (kth[i]) {
				double alpha = (yo - y[i]) / (y[i + 1] - y[i]);
				xo.add(alpha * (x[i + 1] - x[i]) + x[i]);
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
		int i, j;
		double y_interp[] = new double[x_loc.length];
		int len = x.length;
		for (i = 0; i < x_loc.length; i++) {
			y_interp[i] = -Double.MAX_VALUE;
			for (j = 0; j < len; j++) {
				if (x[j] == x_loc[i]) {
					y_interp[i] = y[j];
				} else if ((j < x.length - 1) && (x_loc[i] > x[j])) {
					y_interp[i] = (y[j] + (y[j + 1] - y[j]) * (x_loc[i] - x[j]) / (x[j + 1] - x[j]));
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

		public boolean Evaluate(double[] parameters, double[] cost, double[] gradient) {
			final double x = parameters[0];
			double val1 = (x - xp[1]) / xp[2];
			double val2 = (x - xp[4]) / xp[5];
			double part1 = xp[0] * Math.exp(-val1 * val1);
			double part2 = xp[3] * Math.exp(-val2 * val2);
			cost[0] = Math.abs(xp[0] * Math.exp(-val1 * val1) - xp[3] * Math.exp(-val2 * val2));
			if (gradient != null) {
				if (part1 > part2) {
					gradient[0] = -2.0 * part1 * (val1 / xp[2]) + 2.0 * part2 * (val2 / xp[5]);
				} else {
					gradient[0] = 2.0 * part1 * (val1 / xp[2]) - 2.0 * part2 * (val2 / xp[5]);
				}
			}
			return true;
		}

		public int NumParameters() {
			return 1;
		}
	} // class diffGaussians

	public boolean fitToExternalFunction(double x[], double residuals[], double jacobian[][]) {
		int i;
		for (i = 0; i < x.length; i++) {
			UI.setDataText("unexpected fit x[" + i + "] = " + x[i] + "\n");
		}
		return true;
	}

	class gaussStandardDeviationFitting extends NLConstrainedEngine {

		public gaussStandardDeviationFitting(double x0[], boolean doAnalytical) {
			// nPoints, params
			super(4, 1);

			bounds = 0; // bounds = 0 means unconstrained
			analyticalJacobian = doAnalytical;
			// bl[0] = -Double.MAX_VALUE;
			// bu[0] = Double.MAX_VALUE;
			// bl[1] = 1.0E-10;
			// bu[1] = 74.999999;

			// bounds = 1 means same lower and upper bounds for
			// all parameters
			// bounds = 2 means different lower and upper bounds
			// for all parameters

			// The default is internalScaling = false
			// To make internalScaling = true and have the columns of the
			// Jacobian scaled to have unit length include the following line.
			// internalScaling = true;
			// Suppress diagnostic messages
			outputMes = false;
			for (int i = 0; i < x0.length; i++) {
				gues[i] = x0[i];
			}
		}

		/**
		 * Fit to function.
		 * 
		 * @param a         The x value of the data point.
		 * @param residuals The best guess parameter values.
		 * @param covarMat  The derivative values of y with respect to fitting
		 *                  parameters.
		 */
		public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
			int ctrl;
			int i;

			try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
					for (i = firstGaussianMeanBin; i <= firstGaussianMeanBin + 3; i++) {
						double val1 = (gauss2FittingData[2 * i] - firstGaussianMean) / a[0];
						double value = firstGaussianAmplitude * Math.exp(-val1 * val1);
						residuals[i - firstGaussianMeanBin] = gauss2FittingData[2 * i + 1] - value;
					}
				} else if (ctrl == 2) {
					if (analyticalJacobian) {
						for (i = firstGaussianMeanBin; i <= firstGaussianMeanBin + 3; i++) {
							double val1 = (gauss2FittingData[2 * i] - firstGaussianMean) / a[0];
							covarMat[i - firstGaussianMeanBin][0] = -2.0 * firstGaussianAmplitude * val1
									* Math.exp(-val1 * val1) * (gauss2FittingData[2 * i] - firstGaussianMean)
									/ (a[0] * a[0]);
						}
					} else {
						ctrlMat[0] = 0;
					}
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
			}

			return;
		}

		/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}

		/**
		 * Display results of gaussStandardDeviationFitting.
		 */
		public void dumpResults() {
			System.out.println(" ******* Elsunc Gauss Standard Deviation Fitting ********* ");
			System.out.println("analyticalJacobian = " + analyticalJacobian);
			System.out.println("Number of iterations: " + String.valueOf(iters));
			System.out.println("Chi-squared: " + String.valueOf(getChiSquared()));
			System.out.println("a0 " + String.valueOf(a[0]));
		}
	}

	class gaussSecondStandardDeviationFittingCostFunction extends SizedCostFunction {

		public gaussSecondStandardDeviationFittingCostFunction() {
			// number of resdiuals
			// size of first parameter
			super(4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);

			for (i = secondGaussianMeanBin; i <= secondGaussianMeanBin + 3; i++) {
				double val1 = (gauss2FittingData[2 * i] - secondGaussianMean) / x[0];
				double value = secondGaussianAmplitude * Math.exp(-val1 * val1);
				residuals[i - secondGaussianMeanBin] = gauss2FittingData[2 * i + 1] - value;
				if (jacobians != null && jacobians[0] != null) {
					jacobians[0][i - secondGaussianMeanBin] = -2.0 * secondGaussianAmplitude * val1
							* Math.exp(-val1 * val1) * (gauss2FittingData[2 * i] - secondGaussianMean) / (x[0] * x[0]);
				}
			}
			return true;
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][],
				int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);

			for (i = secondGaussianMeanBin; i <= secondGaussianMeanBin + 3; i++) {
				double val1 = (gauss2FittingData[2 * i] - secondGaussianMean) / x[0];
				double value = secondGaussianAmplitude * Math.exp(-val1 * val1);
				residuals[i - secondGaussianMeanBin] = gauss2FittingData[2 * i + 1] - value;
				if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0] + i - secondGaussianMeanBin] = -2.0 * secondGaussianAmplitude
							* val1 * Math.exp(-val1 * val1) * (gauss2FittingData[2 * i] - secondGaussianMean)
							/ (x[0] * x[0]);
				}
			}
			return true;
		}
	}

	class gaussStandardDeviationFittingCostFunction extends SizedCostFunction {

		public gaussStandardDeviationFittingCostFunction() {
			// number of resdiuals
			// size of first parameter
			super(4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);

			for (i = firstGaussianMeanBin; i <= firstGaussianMeanBin + 3; i++) {
				double val1 = (gauss2FittingData[2 * i] - firstGaussianMean) / x[0];
				double value = firstGaussianAmplitude * Math.exp(-val1 * val1);
				residuals[i - firstGaussianMeanBin] = gauss2FittingData[2 * i + 1] - value;
				if (jacobians != null && jacobians[0] != null) {
					jacobians[0][i - firstGaussianMeanBin] = -2.0 * firstGaussianAmplitude * val1
							* Math.exp(-val1 * val1) * (gauss2FittingData[2 * i] - firstGaussianMean) / (x[0] * x[0]);
				}
			}
			return true;
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][],
				int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);

			for (i = firstGaussianMeanBin; i <= firstGaussianMeanBin + 3; i++) {
				double val1 = (gauss2FittingData[2 * i] - firstGaussianMean) / x[0];
				double value = firstGaussianAmplitude * Math.exp(-val1 * val1);
				residuals[i - firstGaussianMeanBin] = gauss2FittingData[2 * i + 1] - value;
				if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0] + i - firstGaussianMeanBin] = -2.0 * firstGaussianAmplitude * val1
							* Math.exp(-val1 * val1) * (gauss2FittingData[2 * i] - firstGaussianMean) / (x[0] * x[0]);
				}
			}
			return true;
		}
	}

	class gauss1Fitting extends NLConstrainedEngine {

		public gauss1Fitting(double x0[], boolean doAnalytical) {
			// nPoints, params
			super(gauss2FittingObservations, 3);

			bounds = 2; // bounds = 0 means unconstrained
			analyticalJacobian = doAnalytical;
			bl[0] = 0.1 * secondGaussianAmplitude;
			bu[0] = 10.0 * secondGaussianAmplitude;
			bl[1] = Math.max(0.1 * secondGaussianMean, firstGaussianMean + 0.5 * c1);
			bu[1] = Math.min(10.0 * secondGaussianMean, maxSum);
			bl[2] = 0.1 * c2;
			bu[2] = 10.0 * c2;

			// bounds = 1 means same lower and upper bounds for
			// all parameters
			// bounds = 2 means different lower and upper bounds
			// for all parameters

			// The default is internalScaling = false
			// To make internalScaling = true and have the columns of the
			// Jacobian scaled to have unit length include the following line.
			// internalScaling = true;
			// Suppress diagnostic messages
			outputMes = false;
			for (int i = 0; i < x0.length; i++) {
				gues[i] = x0[i];
			}
		}

		/**
		 * Fit to function.
		 * 
		 * @param a         The x value of the data point.
		 * @param residuals The best guess parameter values.
		 * @param covarMat  The derivative values of y with respect to fitting
		 *                  parameters.
		 */
		public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
			int ctrl;
			int i;

			try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
					for (i = 0; i < gauss2FittingObservations; i++) {
						double val1 = (gauss2FittingData[2 * i] - firstGaussianMean) / c1;
						double val2 = (gauss2FittingData[2 * i] - a[1]) / a[2];
						double value = firstGaussianAmplitude * Math.exp(-val1 * val1) + a[0] * Math.exp(-val2 * val2);
						residuals[i] = gauss2FittingData[2 * i + 1] - value;
					}
				} else if (ctrl == 2) {
					if (analyticalJacobian) {
						for (i = 0; i < gauss2FittingObservations; i++) {
							double val2 = (gauss2FittingData[2 * i] - a[1]) / a[2];
							covarMat[i][0] = -Math.exp(-val2 * val2);
							covarMat[i][1] = -2.0 * a[0] * val2 * Math.exp(-val2 * val2) / a[2];
							covarMat[i][2] = -2.0 * a[0] * val2 * Math.exp(-val2 * val2)
									* (gauss2FittingData[2 * i] - a[1]) / (a[2] * a[2]);
						}
					} else {
						ctrlMat[0] = 0;
					}
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
			}

			return;
		}

		/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}

		/**
		 * Display results of gauss 1 curve Fitting.
		 */
		public void dumpResults() {
			System.out.println(" ******* Elsunc Gauss 1 Curve Fitting ********* ");
			System.out.println("analyticalJacobian = " + analyticalJacobian);
			System.out.println("Number of iterations: " + String.valueOf(iters));
			System.out.println("Chi-squared: " + String.valueOf(getChiSquared()));
			System.out.println("a0 " + String.valueOf(a[0]));
			System.out.println("a1 " + String.valueOf(a[1]));
			System.out.println("a2 " + String.valueOf(a[2]));
		}
	}

	class gauss1FittingCostFunction extends SizedCostFunction {

		public gauss1FittingCostFunction() {
			// number of residuals
			// size of first parameter
			super(gauss2FittingObservations, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);

			for (i = 0; i < gauss2FittingObservations; i++) {
				double val1 = (gauss2FittingData[2 * i] - firstGaussianMean) / c1;
				double val2 = (gauss2FittingData[2 * i] - x[1]) / x[2];
				double value = firstGaussianAmplitude * Math.exp(-val1 * val1) + x[0] * Math.exp(-val2 * val2);
				residuals[i] = gauss2FittingData[2 * i + 1] - value;
				if (jacobians != null && jacobians[0] != null) {
					jacobians[0][3 * i] = -Math.exp(-val2 * val2);
					jacobians[0][3 * i + 1] = -2.0 * x[0] * val2 * Math.exp(-val2 * val2) / x[2];
					jacobians[0][3 * i + 2] = -2.0 * x[0] * val2 * Math.exp(-val2 * val2)
							* (gauss2FittingData[2 * i] - x[1]) / (x[2] * x[2]);
				}
			}

			return true;
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][],
				int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);

			for (i = 0; i < gauss2FittingObservations; i++) {
				double val1 = (gauss2FittingData[2 * i] - firstGaussianMean) / c1;
				double val2 = (gauss2FittingData[2 * i] - x[1]) / x[2];
				double value = firstGaussianAmplitude * Math.exp(-val1 * val1) + x[0] * Math.exp(-val2 * val2);
				residuals[i] = gauss2FittingData[2 * i + 1] - value;
				if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0] + 3 * i] = -Math.exp(-val2 * val2);
					jacobians[0][jacobians_offset[0] + 3 * i + 1] = -2.0 * x[0] * val2 * Math.exp(-val2 * val2) / x[2];
					jacobians[0][jacobians_offset[0] + 3 * i + 2] = -2.0 * x[0] * val2 * Math.exp(-val2 * val2)
							* (gauss2FittingData[2 * i] - x[1]) / (x[2] * x[2]);
				}
			}

			return true;
		}
	};

	class gauss2Fitting extends NLConstrainedEngine {

		public gauss2Fitting(double x0[], boolean doAnalytical) {
			// nPoints, params
			super(gauss2FittingObservations, 6);

			bounds = 0; // bounds = 0 means unconstrained
			analyticalJacobian = doAnalytical;
			// bl[0] = -Double.MAX_VALUE;
			// bu[0] = Double.MAX_VALUE;
			// bl[1] = 1.0E-10;
			// bu[1] = 74.999999;

			// bounds = 1 means same lower and upper bounds for
			// all parameters
			// bounds = 2 means different lower and upper bounds
			// for all parameters

			// The default is internalScaling = false
			// To make internalScaling = true and have the columns of the
			// Jacobian scaled to have unit length include the following line.
			// internalScaling = true;
			// Suppress diagnostic messages
			outputMes = false;
			for (int i = 0; i < x0.length; i++) {
				gues[i] = x0[i];
			}
		}

		/**
		 * Fit to function.
		 * 
		 * @param a         The x value of the data point.
		 * @param residuals The best guess parameter values.
		 * @param covarMat  The derivative values of y with respect to fitting
		 *                  parameters.
		 */
		public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
			int ctrl;
			int i;

			try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
					for (i = 0; i < gauss2FittingObservations; i++) {
						double val1 = (gauss2FittingData[2 * i] - a[1]) / a[2];
						double val2 = (gauss2FittingData[2 * i] - a[4]) / a[5];
						double value = a[0] * Math.exp(-val1 * val1) + a[3] * Math.exp(-val2 * val2);
						residuals[i] = gauss2FittingData[2 * i + 1] - value;
					}
				} else if (ctrl == 2) {
					if (analyticalJacobian) {
						for (i = 0; i < gauss2FittingObservations; i++) {
							double val1 = (gauss2FittingData[2 * i] - a[1]) / a[2];
							double val2 = (gauss2FittingData[2 * i] - a[4]) / a[5];
							covarMat[i][0] = -Math.exp(-val1 * val1);
							covarMat[i][1] = -2.0 * a[0] * val1 * Math.exp(-val1 * val1) / a[2];
							covarMat[i][2] = -2.0 * a[0] * val1 * Math.exp(-val1 * val1)
									* (gauss2FittingData[2 * i] - a[1]) / (a[2] * a[2]);
							covarMat[i][3] = -Math.exp(-val2 * val2);
							covarMat[i][4] = -2.0 * a[3] * val2 * Math.exp(-val2 * val2) / a[5];
							covarMat[i][5] = -2.0 * a[3] * val2 * Math.exp(-val2 * val2)
									* (gauss2FittingData[2 * i] - a[4]) / (a[5] * a[5]);
						}
					} else {
						ctrlMat[0] = 0;
					}
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
			}

			return;
		}

		/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}

		/**
		 * Display results of gauss 2 curve Fitting.
		 */
		public void dumpResults() {
			System.out.println(" ******* Elsunc 2 Gaussian Curve Fitting ********* ");
			System.out.println("analyticalJacobian = " + analyticalJacobian);
			System.out.println("Number of iterations: " + String.valueOf(iters));
			System.out.println("Chi-squared: " + String.valueOf(getChiSquared()));
			System.out.println("a0 " + String.valueOf(a[0]));
			System.out.println("a1 " + String.valueOf(a[1]));
			System.out.println("a2 " + String.valueOf(a[2]));
			System.out.println("a3 " + String.valueOf(a[3]));
			System.out.println("a4 " + String.valueOf(a[4]));
			System.out.println("a5 " + String.valueOf(a[5]));
		}
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
				double val1 = (gauss2FittingData[2 * i] - x[1]) / x[2];
				double val2 = (gauss2FittingData[2 * i] - x[4]) / x[5];
				double value = x[0] * Math.exp(-val1 * val1) + x[3] * Math.exp(-val2 * val2);
				residuals[i] = gauss2FittingData[2 * i + 1] - value;
				if (jacobians != null && jacobians[0] != null) {
					jacobians[0][6 * i] = -Math.exp(-val1 * val1);
					jacobians[0][6 * i + 1] = -2.0 * x[0] * val1 * Math.exp(-val1 * val1) / x[2];
					jacobians[0][6 * i + 2] = -2.0 * x[0] * val1 * Math.exp(-val1 * val1)
							* (gauss2FittingData[2 * i] - x[1]) / (x[2] * x[2]);
					jacobians[0][6 * i + 3] = -Math.exp(-val2 * val2);
					jacobians[0][6 * i + 4] = -2.0 * x[3] * val2 * Math.exp(-val2 * val2) / x[5];
					jacobians[0][6 * i + 5] = -2.0 * x[3] * val2 * Math.exp(-val2 * val2)
							* (gauss2FittingData[2 * i] - x[4]) / (x[5] * x[5]);
				}
			}

			return true;
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][],
				int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);

			for (i = 0; i < gauss2FittingObservations; i++) {
				double val1 = (gauss2FittingData[2 * i] - x[1]) / x[2];
				double val2 = (gauss2FittingData[2 * i] - x[4]) / x[5];
				double value = x[0] * Math.exp(-val1 * val1) + x[3] * Math.exp(-val2 * val2);
				residuals[i] = gauss2FittingData[2 * i + 1] - value;
				if (jacobians != null && jacobians[0] != null) {
					jacobians[0][jacobians_offset[0] + 6 * i] = -Math.exp(-val1 * val1);
					jacobians[0][jacobians_offset[0] + 6 * i + 1] = -2.0 * x[0] * val1 * Math.exp(-val1 * val1) / x[2];
					jacobians[0][jacobians_offset[0] + 6 * i + 2] = -2.0 * x[0] * val1 * Math.exp(-val1 * val1)
							* (gauss2FittingData[2 * i] - x[1]) / (x[2] * x[2]);
					jacobians[0][jacobians_offset[0] + 6 * i + 3] = -Math.exp(-val2 * val2);
					jacobians[0][jacobians_offset[0] + 6 * i + 4] = -2.0 * x[3] * val2 * Math.exp(-val2 * val2) / x[5];
					jacobians[0][jacobians_offset[0] + 6 * i + 5] = -2.0 * x[3] * val2 * Math.exp(-val2 * val2)
							* (gauss2FittingData[2 * i] - x[4]) / (x[5] * x[5]);
				}
			}

			return true;
		}
	};

	class GVRecirculation extends NLConstrainedEngine {

		public GVRecirculation(double x0[], boolean doAnalytical) {
			// nPoints, params
			super(nT, 3);

			bounds = 2; // bounds = 0 means unconstrained
			analyticalJacobian = doAnalytical;
			bl[0] = 0.1 * td_init;
			bu[0] = 10.0 * td_init;
			bl[1] = 0.1 * K_init;
			bu[1] = 10.0 * K_init;
			bl[2] = 0.1 * tao_init;
			bu[2] = 10.0 * tao_init;

			// bounds = 1 means same lower and upper bounds for
			// all parameters
			// bounds = 2 means different lower and upper bounds
			// for all parameters

			// The default is internalScaling = false
			// To make internalScaling = true and have the columns of the
			// Jacobian scaled to have unit length include the following line.
			// internalScaling = true;
			// Suppress diagnostic messages
			outputMes = false;
			for (int i = 0; i < x0.length; i++) {
				gues[i] = x0[i];
			}
		}

		/**
		 * Fit to function.
		 * 
		 * @param a         The x value of the data point.
		 * @param residuals The best guess parameter values.
		 * @param covarMat  The derivative values of y with respect to fitting
		 *                  parameters.
		 */
		public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
			int ctrl;
			int i, j, t;

			try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
					double t0 = fitParameters_peak1[0];
					double alpha = fitParameters_peak1[1];
					double beta = fitParameters_peak1[2];
					double td = a[0];
					double K = a[1];
					double tao = a[2];

					// Vector initialization
					double peak2[] = new double[nTfine]; // Peak of recirculation
					double disp[] = new double[nTfine]; // Dispersion of recirculation

					double tg;
					for (i = 0; i < nTfine; i++) {
						tg = tGrid[i];

						if (tg > (t0 + td)) {
							// Calculation of FP(t-td)
							peak2[i] = K * Math.pow((tg - t0 - td), alpha) * Math.exp(-(tg - t0 - td) / beta);
						}

						// Calculation of disp(t)
						disp[i] = Math.exp(-tg / tao);
					}

					// 3.) I assemble the components to obtain the GV calculated on the fine grid
					double recirculation_fine[] = new double[nTfine];
					for (i = 0; i < nTfine; i++) {
						for (j = 0; j <= i; j++) {
							recirculation_fine[i] += peak2[j] * disp[i - j];
						}
						recirculation_fine[i] *= TRfine;
					}

					// 4.) I'm going to sample GV on the time instants requeted in time
					double recirculation;
					for (t = 0; t < nT; t++) {
						double pos = 2.0*(time[t] - Tmin) / TRfine;
						int lowIndex = (int) Math.floor(pos);
						int highIndex = Math.min((int) Math.ceil(pos),nTfine-1);
						if (lowIndex == highIndex) {
							recirculation = recirculation_fine[lowIndex];
						} else {
							double lowFraction = pos - lowIndex;
							double highFraction = highIndex - pos;
							recirculation = lowFraction * recirculation_fine[highIndex]
									+ highFraction * recirculation_fine[lowIndex];
						}
						residuals[t] = (dati_peak2[t] - recirculation) / weights_peak2[t];
					}
				} else if (ctrl == 2) {
					if (analyticalJacobian) {
						double t0 = fitParameters_peak1[0];
						double alpha = fitParameters_peak1[1];
						double beta = fitParameters_peak1[2];
						double td = a[0];
						double K = a[1];
						double tao = a[2];
						double tg;

						// Vector initialization
						double peak2[] = new double[nTfine]; // Peak of recirculation
						double disp[] = new double[nTfine]; // Dispersion of recirculation

						for (i = 0; i < nTfine; i++) {
							tg = tGrid[i];

							if (tg > (t0 + td)) {
								// Calculation of FP(t-td)
								peak2[i] = K * Math.pow((tg - t0 - td), alpha) * Math.exp(-(tg - t0 - td) / beta);
							}

							// Calculation of disp(t)
							disp[i] = Math.exp(-tg / tao);
						}

						double dpeak2dtd[] = new double[nTfine];
						double dpeak2dK[] = new double[nTfine];
						double ddispdtao[] = new double[nTfine];
						for (i = 0; i < nTfine; i++) {
							tg = tGrid[i];

							if (tg > (t0 + td)) {
								// Calculation of FP(t-td)
								dpeak2dtd[i] = K * alpha * Math.pow((tg - t0 - td), (alpha - 1.0))
										* Math.exp(-(tg - t0 - td) / beta)
										- (K / beta) * Math.pow((tg - t0 - td), alpha)
												* Math.exp(-(tg - t0 - td) / beta);
								dpeak2dK[i] = (-1.0) * Math.pow((tg - t0 - td), alpha)
										* Math.exp(-(tg - t0 - td) / beta);
							}

							// Calculation of disp(t)
							ddispdtao[i] = (-tg / (tao * tao)) * Math.exp(-tg / tao);
						}

						// 3.) I assemble the components to obtainthe GV calculated on the fine grid
						double recirculation_fine_td[] = new double[nTfine];
						double recirculation_fine_K[] = new double[nTfine];
						double recirculation_fine_tao[] = new double[nTfine];
						for (i = 0; i < nTfine; i++) {
							for (j = 0; j <= i; j++) {
								recirculation_fine_td[i] += dpeak2dtd[j] * disp[i - j];
								recirculation_fine_K[i] += dpeak2dK[j] * disp[i - j];
								recirculation_fine_tao[i] += peak2[j] * ddispdtao[i - j];
							}
							recirculation_fine_td[i] *= TRfine;
							recirculation_fine_K[i] *= TRfine;
							recirculation_fine_tao[i] *= TRfine;
						}

						for (t = 0; t < nT; t++) {
							double pos = 2.0*(time[t] - Tmin) / TRfine;
							int lowIndex = (int) Math.floor(pos);
							int highIndex = Math.min((int) Math.ceil(pos),nTfine-1);
							if (lowIndex == highIndex) {
								covarMat[t][0] = recirculation_fine_td[lowIndex] / weights_peak2[t];
								covarMat[t][1] = recirculation_fine_K[lowIndex] / weights_peak2[t];
								covarMat[t][2] = recirculation_fine_tao[lowIndex] / weights_peak2[t];
							} else {
								double lowFraction = pos - lowIndex;
								double highFraction = highIndex - pos;
								covarMat[t][0] = (lowFraction * recirculation_fine_td[highIndex]
										+ highFraction * recirculation_fine_td[lowIndex]) / weights_peak2[t];
								covarMat[t][1] = (lowFraction * recirculation_fine_K[highIndex]
										+ highFraction * recirculation_fine_K[lowIndex]) / weights_peak2[t];
								covarMat[t][2] = (lowFraction * recirculation_fine_tao[highIndex]
										+ highFraction * recirculation_fine_tao[lowIndex]) / weights_peak2[t];
							}
						}
					} else {
						ctrlMat[0] = 0;
					}
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
			}

			return;
		}

		/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}

		/**
		 * Display results of GV recirculation curve Fitting.
		 */
		public void dumpResults() {
			System.out.println(" ******* Elsunc GV recirculation Curve Fitting ********* ");
			System.out.println("analyticalJacobian = " + analyticalJacobian);
			System.out.println("Number of iterations: " + String.valueOf(iters));
			System.out.println("Chi-squared: " + String.valueOf(getChiSquared()));
			System.out.println("td " + String.valueOf(a[0]));
			System.out.println("K " + String.valueOf(a[1]));
			System.out.println("tao " + String.valueOf(a[2]));
		}
	}

	class GVRecirculationCostFunction extends SizedCostFunction {
		public GVRecirculationCostFunction() {
			// number of residuals
			// size of first parameter
			super(nT, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i, j, t;
			double t0 = fitParameters_peak1[0];
			double alpha = fitParameters_peak1[1];
			double beta = fitParameters_peak1[2];
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			double td = x[0];
			double K = x[1];
			double tao = x[2];

			// Vector initialization
			double peak2[] = new double[nTfine]; // Peak of recirculation
			double disp[] = new double[nTfine]; // Dispersion of recirculation

			double tg;
			for (i = 0; i < nTfine; i++) {
				tg = tGrid[i];

				if (tg > (t0 + td)) {
					// Calculation of FP(t-td)
					peak2[i] = K * Math.pow((tg - t0 - td), alpha) * Math.exp(-(tg - t0 - td) / beta);
				}

				// Calculation of disp(t)
				disp[i] = Math.exp(-tg / tao);
			}

			// 3.) I assemble the components to obtain the GV calculated on the fine grid
			double recirculation_fine[] = new double[nTfine];
			for (i = 0; i < nTfine; i++) {
				for (j = 0; j <= i; j++) {
					recirculation_fine[i] += peak2[j] * disp[i - j];
				}
				recirculation_fine[i] *= TRfine;
			}

			// 4.) I'm going to sample GV on the time instants requeted in time
			double recirculation;
			for (t = 0; t < nT; t++) {
				double pos = 2.0*(time[t] - Tmin) / TRfine;
				int lowIndex = (int) Math.floor(pos);
				int highIndex = Math.min((int) Math.ceil(pos),nTfine-1);
				if (lowIndex == highIndex) {
					recirculation = recirculation_fine[lowIndex];
				} else {
					double lowFraction = pos - lowIndex;
					double highFraction = highIndex - pos;
					recirculation = lowFraction * recirculation_fine[highIndex]
							+ highFraction * recirculation_fine[lowIndex];
				}
				residuals[t] = (dati_peak2[t] - recirculation) / weights_peak2[t];
			}
			if (jacobians != null && jacobians[0] != null) {
				double dpeak2dtd[] = new double[nTfine];
				double dpeak2dK[] = new double[nTfine];
				double ddispdtao[] = new double[nTfine];
				for (i = 0; i < nTfine; i++) {
					tg = tGrid[i];

					if (tg > (t0 + td)) {
						// Calculation of FP(t-td)
						dpeak2dtd[i] = K * alpha * Math.pow((tg - t0 - td), (alpha - 1.0))
								* Math.exp(-(tg - t0 - td) / beta)
								- (K / beta) * Math.pow((tg - t0 - td), alpha) * Math.exp(-(tg - t0 - td) / beta);
						dpeak2dK[i] = (-1.0) * Math.pow((tg - t0 - td), alpha) * Math.exp(-(tg - t0 - td) / beta);
					}

					// Calculation of disp(t)
					ddispdtao[i] = (-tg / (tao * tao)) * Math.exp(-tg / tao);
				}

				// 3.) I assemble the components to obtainthe GV calculated on the fine grid
				double recirculation_fine_td[] = new double[nTfine];
				double recirculation_fine_K[] = new double[nTfine];
				double recirculation_fine_tao[] = new double[nTfine];
				for (i = 0; i < nTfine; i++) {
					for (j = 0; j <= i; j++) {
						recirculation_fine_td[i] += dpeak2dtd[j] * disp[i - j];
						recirculation_fine_K[i] += dpeak2dK[j] * disp[i - j];
						recirculation_fine_tao[i] += peak2[j] * ddispdtao[i - j];
					}
					recirculation_fine_td[i] *= TRfine;
					recirculation_fine_K[i] *= TRfine;
					recirculation_fine_tao[i] *= TRfine;
				}

				for (t = 0; t < nT; t++) {
					double pos = 2.0*(time[t] - Tmin) / TRfine;
					int lowIndex = (int) Math.floor(pos);
					int highIndex = Math.min((int) Math.ceil(pos),nTfine-1);
					if (lowIndex == highIndex) {
						jacobians[0][3 * t] = recirculation_fine_td[lowIndex] / weights_peak2[t];
						jacobians[0][3 * t + 1] = recirculation_fine_K[lowIndex] / weights_peak2[t];
						jacobians[0][3 * t + 2] = recirculation_fine_tao[lowIndex] / weights_peak2[t];
					} else {
						double lowFraction = pos - lowIndex;
						double highFraction = highIndex - pos;
						jacobians[0][3 * t] = (lowFraction * recirculation_fine_td[highIndex]
								+ highFraction * recirculation_fine_td[lowIndex]) / weights_peak2[t];
						jacobians[0][3 * t + 1] = (lowFraction * recirculation_fine_K[highIndex]
								+ highFraction * recirculation_fine_K[lowIndex]) / weights_peak2[t];
						jacobians[0][3 * t + 2] = (lowFraction * recirculation_fine_tao[highIndex]
								+ highFraction * recirculation_fine_tao[lowIndex]) / weights_peak2[t];
					}
				}
			}
			return true;
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][],
				int jacobians_offset[]) {
			int i, j, t;
			double t0 = fitParameters_peak1[0];
			double alpha = fitParameters_peak1[1];
			double beta = fitParameters_peak1[2];
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			double td = x[0];
			double K = x[1];
			double tao = x[2];

			// Vector initialization
			double peak2[] = new double[nTfine]; // Peak of recirculation
			double disp[] = new double[nTfine]; // Dispersion of recirculation

			double tg;
			for (i = 0; i < nTfine; i++) {
				tg = tGrid[i];

				if (tg > (t0 + td)) {
					// Calculation of FP(t-td)
					peak2[i] = K * Math.pow((tg - t0 - td), alpha) * Math.exp(-(tg - t0 - td) / beta);
				}

				// Calculation of disp(t)
				disp[i] = Math.exp(-tg / tao);
			}

			// 3.) I assemble the components to obtain the GV calculated on the fine grid
			double recirculation_fine[] = new double[nTfine];
			for (i = 0; i < nTfine; i++) {
				for (j = 0; j <= i; j++) {
					recirculation_fine[i] += peak2[j] * disp[i - j];
				}
				recirculation_fine[i] *= TRfine;
			}

			// 4.) I'm going to sample GV on the time instants requeted in time
			double recirculation;
			for (t = 0; t < nT; t++) {
				double pos = 2.0*(time[t] - Tmin) / TRfine;
				int lowIndex = (int) Math.floor(pos);
				int highIndex = Math.min((int) Math.ceil(pos), nTfine-1);
				if (lowIndex == highIndex) {
					recirculation = recirculation_fine[lowIndex];
				} else {
					double lowFraction = pos - lowIndex;
					double highFraction = highIndex - pos;
					recirculation = lowFraction * recirculation_fine[highIndex]
							+ highFraction * recirculation_fine[lowIndex];
				}
				residuals[t] = (dati_peak2[t] - recirculation) / weights_peak2[t];
			}
			if (jacobians != null && jacobians[0] != null) {
				double dpeak2dtd[] = new double[nTfine];
				double dpeak2dK[] = new double[nTfine];
				double ddispdtao[] = new double[nTfine];
				for (i = 0; i < nTfine; i++) {
					tg = tGrid[i];

					if (tg > (t0 + td)) {
						// Calculation of FP(t-td)
						dpeak2dtd[i] = K * alpha * Math.pow((tg - t0 - td), (alpha - 1.0))
								* Math.exp(-(tg - t0 - td) / beta)
								- (K / beta) * Math.pow((tg - t0 - td), alpha) * Math.exp(-(tg - t0 - td) / beta);
						dpeak2dK[i] = (-1.0) * Math.pow((tg - t0 - td), alpha) * Math.exp(-(tg - t0 - td) / beta);
					}

					// Calculation of disp(t)
					ddispdtao[i] = (-tg / (tao * tao)) * Math.exp(-tg / tao);
				}

				// 3.) I assemble the components to obtainthe GV calculated on the fine grid
				double recirculation_fine_td[] = new double[nTfine];
				double recirculation_fine_K[] = new double[nTfine];
				double recirculation_fine_tao[] = new double[nTfine];
				for (i = 0; i < nTfine; i++) {
					for (j = 0; j <= i; j++) {
						recirculation_fine_td[i] += dpeak2dtd[j] * disp[i - j];
						recirculation_fine_K[i] += dpeak2dK[j] * disp[i - j];
						recirculation_fine_tao[i] += peak2[j] * ddispdtao[i - j];
					}
					recirculation_fine_td[i] *= TRfine;
					recirculation_fine_K[i] *= TRfine;
					recirculation_fine_tao[i] *= TRfine;
				}

				for (t = 0; t < nT; t++) {
					double pos = 2.0*(time[t] - Tmin) / TRfine;
					int lowIndex = (int) Math.floor(pos);
					int highIndex = Math.min((int) Math.ceil(pos),nTfine-1);
					if (lowIndex == highIndex) {
						jacobians[0][jacobians_offset[0] + 3 * t] = recirculation_fine_td[lowIndex] / weights_peak2[t];
						jacobians[0][jacobians_offset[0] + 3 * t + 1] = recirculation_fine_K[lowIndex]
								/ weights_peak2[t];
						jacobians[0][jacobians_offset[0] + 3 * t + 2] = recirculation_fine_tao[lowIndex]
								/ weights_peak2[t];
					} else {
						double lowFraction = pos - lowIndex;
						double highFraction = highIndex - pos;
						jacobians[0][jacobians_offset[0] + 3 * t] = (lowFraction * recirculation_fine_td[highIndex]
								+ highFraction * recirculation_fine_td[lowIndex]) / weights_peak2[t];
						jacobians[0][jacobians_offset[0] + 3 * t + 1] = (lowFraction * recirculation_fine_K[highIndex]
								+ highFraction * recirculation_fine_K[lowIndex]) / weights_peak2[t];
						jacobians[0][jacobians_offset[0] + 3 * t + 2] = (lowFraction * recirculation_fine_tao[highIndex]
								+ highFraction * recirculation_fine_tao[lowIndex]) / weights_peak2[t];
					}
				}
			}
			return true;
		}
	}

	class GVFitting extends NLConstrainedEngine {

		public GVFitting(double x0[], boolean doAnalytical) {
			// nPoints, params
			super(nT, 4);

			bounds = 2; // bounds = 0 means unconstrained
			analyticalJacobian = doAnalytical;
			bl[0] = 0.1 * t0_init;
			bu[0] = 10.0 * t0_init;
			bl[1] = 0.1 * alpha_init;
			bu[1] = 10.0 * alpha_init;
			bl[2] = 0.1 * beta_init;
			bu[2] = 10.0 * beta_init;
			bl[3] = 0.1 * A_init;
			bu[3] = 10.0 * A_init;

			// bounds = 1 means same lower and upper bounds for
			// all parameters
			// bounds = 2 means different lower and upper bounds
			// for all parameters

			// The default is internalScaling = false
			// To make internalScaling = true and have the columns of the
			// Jacobian scaled to have unit length include the following line.
			// internalScaling = true;
			// Suppress diagnostic messages
			outputMes = false;
			for (int i = 0; i < x0.length; i++) {
				gues[i] = x0[i];
			}
		}

		/**
		 * Fit to function.
		 * 
		 * @param a         The x value of the data point.
		 * @param residuals The best guess parameter values.
		 * @param covarMat  The derivative values of y with respect to fitting
		 *                  parameters.
		 */
		public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
			int ctrl;
			int i;

			try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
					double t0 = a[0];
					double alpha = a[1];
					double beta = a[2];
					double A = a[3];
					double GV;
					for (i = 0; i < nT; i++) {
						double t = time[i];
						if (t > t0) {
							GV = A * Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta);
						} else {
							GV = 0.0;
						}
						residuals[i] = (data_peak1[i] - GV) / weights_peak1[i];
					}
				} else if (ctrl == 2) {
					if (analyticalJacobian) {
						double t0 = a[0];
						double alpha = a[1];
						double beta = a[2];
						double A = a[3];
						double GV;
						for (i = 0; i < nT; i++) {
							double t = time[i];
							if (t > t0) {
								covarMat[i][0] = (A / weights_peak1[i])
										* ((alpha * Math.pow((t - t0), (alpha - 1.0)) * Math.exp(-(t - t0) / beta))
												- (Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta) / beta));
								covarMat[i][1] = -(A / weights_peak1[i]) * Math.log(t - t0) * Math.pow((t - t0), alpha)
										* Math.exp(-(t - t0) / beta);
								covarMat[i][2] = -(A / weights_peak1[i]) * ((t - t0) / (beta * beta))
										* Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta);
								covarMat[i][3] = -(1.0 / weights_peak1[i]) * Math.pow((t - t0), alpha)
										* Math.exp(-(t - t0) / beta);
							} else {
								covarMat[i][0] = 0.0;
								covarMat[i][1] = 0.0;
								covarMat[i][2] = 0.0;
								covarMat[i][3] = 0.0;
							}
						}
					} else {
						ctrlMat[0] = 0;
					}
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
			}

			return;
		}

		/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}

		/**
		 * Display results of gammma-variate curve Fitting.
		 */
		public void dumpResults() {
			System.out.println(" ******* Elsunc gamma-variate Curve Fitting ********* ");
			System.out.println("analyticalJacobian = " + analyticalJacobian);
			System.out.println("Number of iterations: " + String.valueOf(iters));
			System.out.println("Chi-squared: " + String.valueOf(getChiSquared()));
			System.out.println("t0 " + String.valueOf(a[0]));
			System.out.println("alpha " + String.valueOf(a[1]));
			System.out.println("beta " + String.valueOf(a[2]));
			System.out.println("A " + String.valueOf(a[3]));
		}
	}

	class GVFittingCostFunction extends SizedCostFunction {

		public GVFittingCostFunction() {
			// number of residuals
			// size of first parameter
			super(nT, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			double t0 = x[0];
			double alpha = x[1];
			double beta = x[2];
			double A = x[3];
			double GV;
			for (i = 0; i < nT; i++) {
				double t = time[i];
				if (t > t0) {
					GV = A * Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta);
				} else {
					GV = 0.0;
				}
				residuals[i] = (data_peak1[i] - GV) / weights_peak1[i];
				if (jacobians != null && jacobians[0] != null) {
					if (t > t0) {
						jacobians[0][4 * i] = (A / weights_peak1[i])
								* ((alpha * Math.pow((t - t0), (alpha - 1.0)) * Math.exp(-(t - t0) / beta))
										- (Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta) / beta));
						jacobians[0][4 * i + 1] = -(A / weights_peak1[i]) * Math.log(t - t0) * Math.pow((t - t0), alpha)
								* Math.exp(-(t - t0) / beta);
						jacobians[0][4 * i + 2] = -(A / weights_peak1[i]) * ((t - t0) / (beta * beta))
								* Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta);
						jacobians[0][4 * i + 3] = -(1.0 / weights_peak1[i]) * Math.pow((t - t0), alpha)
								* Math.exp(-(t - t0) / beta);
					} else {
						jacobians[0][4 * i] = 0.0;
						jacobians[0][4 * i + 1] = 0.0;
						jacobians[0][4 * i + 2] = 0.0;
						jacobians[0][4 * i + 3] = 0.0;
					}
				}
			}
			return true;
		}

		public boolean Evaluate(Vector<double[]> parameters, double residuals[], double jacobians[][],
				int jacobians_offset[]) {
			int i;
			// Called by ResidualBlock.Evaluate
			double x[] = parameters.get(0);
			double t0 = x[0];
			double alpha = x[1];
			double beta = x[2];
			double A = x[3];
			double GV;
			for (i = 0; i < nT; i++) {
				double t = time[i];
				if (t > t0) {
					GV = A * Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta);
				} else {
					GV = 0.0;
				}
				residuals[i] = (data_peak1[i] - GV) / weights_peak1[i];
				if (jacobians != null && jacobians[0] != null) {
					if (t > t0) {
						jacobians[0][jacobians_offset[0] + 4 * i] = (A / weights_peak1[i])
								* ((alpha * Math.pow((t - t0), (alpha - 1.0)) * Math.exp(-(t - t0) / beta))
										- (Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta) / beta));
						jacobians[0][jacobians_offset[0] + 4 * i + 1] = -(A / weights_peak1[i]) * Math.log(t - t0)
								* Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta);
						jacobians[0][jacobians_offset[0] + 4 * i + 2] = -(A / weights_peak1[i])
								* ((t - t0) / (beta * beta)) * Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta);
						jacobians[0][jacobians_offset[0] + 4 * i + 3] = -(1.0 / weights_peak1[i])
								* Math.pow((t - t0), alpha) * Math.exp(-(t - t0) / beta);
					} else {
						jacobians[0][jacobians_offset[0] + 4 * i] = 0.0;
						jacobians[0][jacobians_offset[0] + 4 * i + 1] = 0.0;
						jacobians[0][jacobians_offset[0] + 4 * i + 2] = 0.0;
						jacobians[0][jacobians_offset[0] + 4 * i + 3] = 0.0;
					}
				}
			}
			return true;
		}
	}

}