package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

/** 
 * This code is ported from the MATLAB source code of the Rice Wavelet tools, version 2.4.
 * Below is the license for the original source code.
 */

/**This "rice-wlet-tools", version 2.4
Released - <Dec 1 2002>

CONDITIONS FOR USE:
Copyright (c) 2000 RICE UNIVERSITY. All rights reserved.

This software is distributed and licensed to you on a non-exclusive 
basis, free-of-charge. Redistribution and use in source and binary forms, 
with or without modification, are permitted provided that the following 
conditions are met:

1. Redistribution of source code must retain the above copyright notice, 
   this list of conditions and the following disclaimer.
2. Redistribution in binary form must reproduce the above copyright notice, 
   this list of conditions and the following disclaimer in the 
   documentation and/or other materials provided with the distribution.
3. All advertising materials mentioning features or use of this software 
   must display the following acknowledgment: This product includes 
   software developed by Rice University, Houston, Texas and its contributors.
4. Neither the name of the University nor the names of its contributors 
   may be used to endorse or promote products derived from this software 
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY WILLIAM MARSH RICE UNIVERSITY, HOUSTON, TEXAS, 
AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, 
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL RICE UNIVERSITY 
OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
OR BUSINESS INTERRUPTIONS) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
OTHERWISE), PRODUCT LIABILITY, OR OTHERWISE ARISING IN ANY WAY OUT OF THE 
USE OF THIS SOFTWARE,  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

For information on commercial licenses, contact Rice University's Office of 
Technology Transfer at techtran@rice.edu or (713) 348-6173*/



public class AlgorithmRiceWaveletTools extends AlgorithmBase {
    
    public static final int MINIMUM_PHASE = 1;
    
    public static final int MID_PHASE = 2;
    
    public static final int MAXIMUM_PHASE = 3;
    
    public static final int MAD = 0; // Mean absolute deviation
    
    public static final int STD = 1; // Classical numerical std estimate
    
    public static final int SOFT_THRESHOLDING = 0;
    
    public static final int HARD_THRESHOLDING = 1;
    
    private int filterLength;
    
    private int filterType = MINIMUM_PHASE;
    
    private double[] scalingFilter;
    
    private double[] waveletFilter;
    
    private int nDims;
    
    private int extents[];
    
    private int xDim;
    
    private int yDim;
    
    private int sliceSize;
    
    private int zDim;
    
    private double aArray[];
    
    private int error = 0;
    
    private int numberOfLevels;
    
    private boolean redundant = false;
    
    private boolean doWaveletImages;
    
    private ModelImage[] waveletImage = null;
    
    // Low pass wavelet component
    private double yl[] = null;
    // Save low pass wavelet component for each level
    private double llA[][] = null;
    // Create 3 high pass wavelet components for each level
    private double lhA[][] = null;
    private double hlA[][] = null;
    private double hhA[][] = null;
    // mdwt output
    private double y[] = null;
    private int minimumLevel = 1;
    private int maximumLevel;
    private int z;
    private boolean selfTest = false;
    private boolean test_mrdwt_1 = false;
    private boolean test_mrdwt_2 = false;
    private boolean test_mrdwt_2L2 = false;
    private boolean test_mirdwt_1 = false;
    private boolean test_mirdwt_2D = false;
    private boolean test_mdwt_1D = false;
    private boolean test_mdwt_2D = false;
    private boolean test_midwt_1D = false;
    private boolean test_midwt_2D = false;
    private boolean test_denoise_default = false;
    private boolean test_denoise_2D = false;
    private boolean test_denoise_udwt = false;
    private boolean test_denoise_udwt_2d = false;
    private boolean test_denoise_threshold_low = false;
    private boolean test_denoise_thresh_multiplier = false;
    private boolean test_denoise_std = false;
    private boolean test_denoise_hard = false;
    private boolean test_denoise_levels = false;
    private boolean test_denoise_actual_thresh = false;
    private boolean test_denoise_udwt_threshold_low = false;
    private boolean test_denoise_udwt_thresh_multiplier = false;
    private boolean test_denoise_udwt_std = false;
    private boolean test_denoise_udwt_soft = false;
    private boolean test_denoise_udwt_levels = false;
    private boolean test_denoise_udwt_actual_thresh = false;
    private boolean doDenoise = true;
    // actual_threshold used if value is other than zero
    private double actualThreshold = 0.0;
    private int varianceEstimator = MAD;
    // The threshold thld is computed as thld = c*MAD(noise_estimate);
    // Defaults are 3.6 for undecimated or redundant based denoising
    // and 3.0 if not decimated or redundant
    private double thresholdMultiplier = 3.6;
    // Defaults are SOFT_THRESHOLDING if not redundant and HARD_THRESHOLDING if redundant
    private int thresholdingType = HARD_THRESHOLDING;
    // Default is don't threshold low pass components
    private boolean thresholdLowPass = false;
    private double noiseStandardDeviation[] = null;
    private boolean useNoiseStdConstructor = false;
    
    
    
    public AlgorithmRiceWaveletTools(ModelImage destImg, ModelImage srcImg, int filterLength, boolean redundant,
            int numberOfLevels, boolean doWaveletImages, int minimumLevel, int maximumLevel,
            int filterType, boolean doDenoise, double actualThreshold, int varianceEstimator, double thresholdMultiplier,
            int thresholdingType, boolean thresholdLowPass) {
        super(destImg, srcImg);
        this.filterLength = filterLength;
        this.redundant = redundant;
        this.numberOfLevels = numberOfLevels;
        this.doWaveletImages = doWaveletImages;
        this.minimumLevel = minimumLevel;
        this.maximumLevel = maximumLevel;
        this.filterType = filterType;
        this.doDenoise = doDenoise;
        this.actualThreshold = actualThreshold;
        this.varianceEstimator = varianceEstimator;
        this.thresholdMultiplier = thresholdMultiplier;
        this.thresholdingType = thresholdingType;
        this.thresholdLowPass = thresholdLowPass;
    }
    
    public AlgorithmRiceWaveletTools(ModelImage srcImg, int filterLength, boolean redundant, int numberOfLevels,
            boolean doWaveletImages, int minimumLevel, int maximumLevel, int filterType,boolean doDenoise, double actualThreshold, 
            int varianceEstimator, double thresholdMultiplier, int thresholdingType, boolean thresholdLowPass) {
        super(null, srcImg);
        this.filterLength = filterLength;
        this.redundant = redundant;
        this.numberOfLevels = numberOfLevels;
        this.doWaveletImages = doWaveletImages;
        this.minimumLevel = minimumLevel;
        this.maximumLevel = maximumLevel;
        this.filterType = filterType;
        this.doDenoise = doDenoise;
        this.actualThreshold = actualThreshold;
        this.varianceEstimator = varianceEstimator;
        this.thresholdMultiplier = thresholdMultiplier;
        this.thresholdingType = thresholdingType;
        this.thresholdLowPass = thresholdLowPass;
    }
    
    public AlgorithmRiceWaveletTools(ModelImage srcImg, double noiseStandardDeviation[]) {
    	super(null, srcImg);
    	this.noiseStandardDeviation = noiseStandardDeviation;
    	numberOfLevels = Integer.MAX_VALUE;
    	filterLength = 4;
    	redundant = false;
    	doDenoise = true;
    	actualThreshold = 0.0;
    	varianceEstimator = MAD;
    	useNoiseStdConstructor = true;
    }
    
    
    public void runAlgorithm() {
        int i, j, k;
        int divisor;
        int numberValues;
        double tmp[];
        double median;
        double thld;
        double sum;
        double mean;
        double diff;
        double variance;
        double std;
        double srchA[][] = null;
        double absVal;
        int numX;
        int numY;
        int twoL;
        int ix;
        int jx;
        int ixLow;
        int jxLow;
        int ix2;
        int jx2;
        double ykeep[];
        
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        if (useNoiseStdConstructor && (noiseStandardDeviation == null)) {
        	displayError("noiseStandardDeviation is null in AlgorithmRiceWaveletTools construtor");
        	return;
        }
        
        if (selfTest) {
            nDims = 1;
            xDim = 8;
            yDim = 1;
            sliceSize = 8;
            filterLength = 4;
            numberOfLevels = 1;
            maximumLevel = 1;
            //makeSig("Leopold",8);
            //makeSig("HeaviSine",8);
            //makeSig("Bumps", 8);
            //makeSig("Blocks",8);
            //xDim = 12;
            //sliceSize = 12;
            //makeSig("Doppler",12);
            //makeSig("Ramp",8);
            //makeSig("Cusp",8);
            //makeSig("Sing",8);
            //makeSig("HiSine",8);
            //makeSig("LoSine",8);
            //makeSig("LinChirp",8);
            //makeSig("TwoChirp",8);
            //makeSig("QuadChirp",8);
            //makeSig("MishMash",8);
            makeSig("WernerSorrows",8);
        }
        else if (test_mrdwt_1) {
	        //x = makesig("Leopold",8); // sets aArray
        	makeSig("Leopold",8);
	        //h = daubcqf(4,'min'); // sets scalingFilter and waveletFilter
        	filterType = MINIMUM_PHASE;
        	filterLength = 4;
        	scalingFilter = new double[filterLength];
            
            waveletFilter = new double[filterLength];
        	daubcqf();
	        //L = 1;
        	numberOfLevels = 1;
        	nDims = 1;
            xDim = 8;
            yDim = 1;
            sliceSize = 8;
            maximumLevel = 1;
	        // [yl, yh, L] = mrdwt(x, h, L);
            // Create low pass wavelet component
            yl = new double[sliceSize];
            lhA = new double[numberOfLevels][sliceSize];
            mrdwt();
            for (i = 0; i < sliceSize; i++) {
            	Preferences.debug("yl["+i+"] = " + yl[i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
            for (i = 0; i < sliceSize; i++) {
            	Preferences.debug("lhA[0]["+i+"] = " + lhA[0][i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
            setCompleted(true);
            return;
            // Correct values obtained:
            // yl[0] = 0.8365163037378078
    		// yl[1] = 0.48296291314453416
    		// yl[2] = 0.0
    		// yl[3] = 0.0
    		// yl[4] = 0.0
    		// yl[5] = 0.0
    		// yl[6] = -0.12940952255126037
    		// yl[7] = 0.2241438680420134
    		// lhA[0][0] = -0.2241438680420134
    		// lhA[0][1] = -0.12940952255126037
    		// lhA[0][2] = 0.0
    		// lhA[0][3] = 0.0
    		// lhA[0][4] = 0.0
    		// lhA[0][5] = 0.0
    		// lhA[0][6] = -0.48296291314453416
    		// lhA[0][7] = 0.8365163037378078
            
	        //yl_corr = [0.8365  0.4830 0 0 0 0 -0.1294 0.2241];
	        //yh_corr = [-0.2241 -0.1294 0 0 0 0 -0.4830 0.8365];
	        //L_corr = 1;
	        //assertVectorsAlmostEqual(yl, yl_corr, 'relative', 0.001);
	        //assertVectorsAlmostEqual(yh, yh_corr, 'relative', 0.001);
	        // assertEqual(L, L_corr);
        }
        else if (test_mrdwt_2) {
        	  //x = [1 3 5 2; 3 4 8 1; 3 9 2 0; 1 2 3 0];
        	  aArray = new double[] {1, 3, 5, 2, 3, 4, 8, 1, 3, 9, 2, 0, 1, 2, 3, 0};
        	  //h = daubcqf(4, 'min'); // sets scalingFilter and waveletFilter
        	  filterType = MINIMUM_PHASE;
          	  filterLength = 4;
          	  scalingFilter = new double[filterLength];
              waveletFilter = new double[filterLength];
          	  daubcqf();
  	          //L = 1;
          	  numberOfLevels = 1;
          	  nDims = 2;
              xDim = 4;
              yDim = 4;
              sliceSize = 16;
              maximumLevel = 1;
              
              // Create low pass wavelet component
              yl = new double[sliceSize];
              // Save the low pass component for each level
              llA = new double[numberOfLevels-1][sliceSize];
              
              // Create 3 high pass components for each level
              lhA = new double[numberOfLevels][sliceSize];
              hlA = new double[numberOfLevels][sliceSize];
              hhA = new double[numberOfLevels][sliceSize];
              mrdwt();
              for (i = 0; i < sliceSize; i++) {
              	Preferences.debug("yl["+i+"] = " + yl[i] + "\n", Preferences.DEBUG_ALGORITHM); 
              }
              
              for (i = 0; i < sliceSize; i++) {
              	  Preferences.debug("lhA[0]["+i+"] = " + lhA[0][i] + "\n", Preferences.DEBUG_ALGORITHM); 
              }
              for (i = 0; i < sliceSize; i++) {
                  Preferences.debug("hlA[0]["+i+"] = " + hlA[0][i] + "\n", Preferences.DEBUG_ALGORITHM); 
              }
              for (i = 0; i < sliceSize; i++) {
                  Preferences.debug("hhA[0]["+i+"] = " + hhA[0][i] + "\n", Preferences.DEBUG_ALGORITHM); 
              }
              setCompleted(true);
              return;
              // yl and yl_corr match
              // lhA and first 4 columns of yh_corr match
              // hlA and middle 4 columns of yh_corr match
              // hhA and last 4 columns of yh_corr match
              // yl[0] = 9.011057158514985
              // yl[1] = 10.779886545341357
    		  // yl[2] = 5.879487298107779
    		  // yl[3] = 4.110657911281409
    		  // yl[4] = 11.139342088718587
    		  // yl[5] = 8.776601615137755
    		  // yl[6] = 2.550240473580836
    		  // yl[7] = 4.91298094716167
    		  // yl[8] = 6.946474596215561
    		  // yl[9] = 5.757772228311383
    		  // yl[10] = 1.6629809471616712
    		  // yl[11] = 2.851683315065848
    		  // yl[12] = 4.818189666011959
    		  // yl[13] = 7.761057158514986
    		  // yl[14] = 4.992227771688615
    		  // yl[15] = 2.0493602791855876
    		  // lhA[0][0] = 4.572354790610808
    		  // lhA[0][1] = 0.42852540378443815
    		  // lhA[0][2] = -1.8827722283113837
    		  // lhA[0][3] = 2.261057158514986
    		  // lhA[0][4] = -2.444069860407206
    		  // lhA[0][5] = -2.431810333988042
    		  // lhA[0][6] = -1.446474596215561
    		  // lhA[0][7] = -1.4587341226347261
    		  // lhA[0][8] = -1.7487976320958225
    		  // lhA[0][9] = -0.5870190528383299
    		  // lhA[0][10] = 0.5592150697963958
    		  // lhA[0][11] = -0.6025635094610965
    		  // lhA[0][12] = -0.37948729810778037
    		  // lhA[0][13] = 2.5903039830419314
    		  // lhA[0][14] = 2.7700317547305473
    		  // lhA[0][15] = -0.1997595264191645
    		  // hlA[0][0] = 4.871392896287467
    		  // hlA[0][1] = -3.102563509461097
    		  // hlA[0][2] = -1.7978357377724807
    		  // hlA[0][3] = 0.029006350946109177
    		  // hlA[0][4] = 1.886057158514987
    		  // hlA[0][5] = -4.2487976320958225
    		  // hlA[0][6] = -1.977563509461097
    		  // hlA[0][7] = 4.340303983041931
    		  // hlA[0][8] = 1.1662658773652737
    		  // hlA[0][9] = -2.3549682452694523
    		  // hlA[0][10] = -1.7398230358802615
    		  // hlA[0][11] = 2.928525403784438
    		  // hlA[0][12] = 4.151601615137754
    		  // hlA[0][13] = -1.2087341226347261
    		  // hlA[0][14] = -1.5600952641916448
    		  // hlA[0][15] = -1.3827722283113837
    		  // hhA[0][0] = -2.962019052838329
    		  // hhA[0][1] = -1.1818103339880413
    		  // hhA[0][2] = -1.1294872981077808
    		  // hhA[0][3] = 5.273316684934151
    		  // hhA[0][4] = -0.02331668493415151
    		  // hhA[0][5] = 0.035576211353315984
    		  // hhA[0][6] = 0.9497595264191645
    		  // hhA[0][7] = -0.962019052838329
    		  // hhA[0][8] = -0.6964745962155621
    		  // hhA[0][9] = 1.8582531754730547
    		  // hhA[0][10] = -0.7120190528383288
    		  // hhA[0][11] = -0.44975952641916406
    		  // hhA[0][12] = 3.6818103339880417
    		  // hhA[0][13] = -0.7120190528383288
    		  // hhA[0][14] = 0.8917468245269453
    		  // hhA[0][15] = -3.861538105676658
        	  /*[yl, yh, L] = mrdwt(x, h, 1);
        	  //yl_corr = [
        	      9.0111   10.7799    5.8795    4.1107;
        	     11.1393    8.7766    2.5502    4.9130;
        	      6.9465    5.7578    1.6630    2.8517;
        	      4.8182    7.7611    4.9922    2.0494];
        	  yh_corr = [
        	      4.5724    0.4285   -1.8828    2.2611    4.8714   -3.1026   -1.7978    0.0290   -2.9620   -1.1818   -1.1295    5.2733;
        	     -2.4441   -2.4318   -1.4465   -1.4587    1.8861   -4.2488   -1.9776    4.3403   -0.0233    0.0356    0.9498   -0.9620;
        	     -1.7488   -0.5870    0.5592   -0.6026    1.1663   -2.3550   -1.7398    2.9285   -0.6965    1.8583   -0.7120   -0.4498;
        	     -0.3795    2.5903    2.7700   -0.1998    4.1516   -1.2087   -1.5601   -1.3828    3.6818   -0.7120    0.8917   -3.8615];
        	assertVectorsAlmostEqual(yl, yl_corr, 'relative', 0.001);
        	assertVectorsAlmostEqual(yh, yh_corr, 'relative', 0.001);*/
        }
        else if (test_mrdwt_2L2) {
      	  //x = [1 3 5 2; 3 4 8 1; 3 9 2 0; 1 2 3 0];
      	  aArray = new double[] {1, 3, 5, 2, 3, 4, 8, 1, 3, 9, 2, 0, 1, 2, 3, 0};
      	  //h = daubcqf(4, 'min'); // sets scalingFilter and waveletFilter
      	  filterType = MINIMUM_PHASE;
          filterLength = 4;
          scalingFilter = new double[filterLength];
          waveletFilter = new double[filterLength];
          daubcqf();
	      //L = 2;
          numberOfLevels = 2;
          nDims = 2;
            xDim = 4;
            yDim = 4;
            sliceSize = 16;
            maximumLevel = 2;
            
            // Create low pass wavelet component
            yl = new double[sliceSize];
            // Save the low pass component for each level
            llA = new double[numberOfLevels-1][sliceSize];
            
            // Create 3 high pass components for each level
            lhA = new double[numberOfLevels][sliceSize];
            hlA = new double[numberOfLevels][sliceSize];
            hhA = new double[numberOfLevels][sliceSize];
            extents = new int[] {4,4};
            waveletImage = new ModelImage[4];
            mrdwt();
            for (i = 0; i < sliceSize; i++) {
            	Preferences.debug("yl["+i+"] = " + yl[i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
            
            for (i = 0; i < sliceSize; i++) {
            	  Preferences.debug("lhA[0]["+i+"] = " + lhA[0][i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
            for (i = 0; i < sliceSize; i++) {
                Preferences.debug("hlA[0]["+i+"] = " + hlA[0][i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
            for (i = 0; i < sliceSize; i++) {
                Preferences.debug("hhA[0]["+i+"] = " + hhA[0][i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
            
            for (i = 0; i < sliceSize; i++) {
          	  Preferences.debug("llA[0]["+i+"] = " + llA[0][i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
            for (i = 0; i < sliceSize; i++) {
          	  Preferences.debug("lhA[1]["+i+"] = " + lhA[1][i] + "\n", Preferences.DEBUG_ALGORITHM); 
	        }
	        for (i = 0; i < sliceSize; i++) {
	           Preferences.debug("hlA[1]["+i+"] = " + hlA[1][i] + "\n", Preferences.DEBUG_ALGORITHM); 
	        }
	        for (i = 0; i < sliceSize; i++) {
	            Preferences.debug("hhA[1]["+i+"] = " + hhA[1][i] + "\n", Preferences.DEBUG_ALGORITHM); 
	        }
            setCompleted(true);
            return;
            // yl and yl_corr match
            // lhA[0] and yh_corr rows 1, 3, 5, and 7 and columns 1 to 4 match
            // hlA[0] and yh_corr rows 1, 3, 5, and 7 and columns 5 to 8 match
            // hhA[0] and yh_corr rows 1, 3, 5, and 7 and column 9 to 12 match
            // lhA[1] and yh_corr rows 2, 4, 6, and 8 and columns 1 to 4 match
            // hlA[1] and yh_corr rows 2, 4, 6, and 8 and columns 5 to 8 match
            // hhA[1] and yh_corr rows 2, 4, 6, and 8 and columns 9 to 12 match
            /*yl[0] = 11.749999999999998
    		yl[1] = 11.750000000000002
    		yl[2] = 11.749999999999996
    		yl[3] = 11.749999999999998
    		yl[4] = 11.749999999999998
    		yl[5] = 11.75
    		yl[6] = 11.749999999999998
    		yl[7] = 11.749999999999998
    		yl[8] = 11.749999999999996
    		yl[9] = 11.749999999999998
    		yl[10] = 11.749999999999995
    		yl[11] = 11.749999999999998
    		yl[12] = 11.749999999999996
    		yl[13] = 11.75
    		yl[14] = 11.749999999999996
    		yl[15] = 11.749999999999996
    		lhA[0][0] = 4.572354790610808
    		lhA[0][1] = 0.42852540378443815
    		lhA[0][2] = -1.8827722283113837
    		lhA[0][3] = 2.261057158514986
    		lhA[0][4] = -2.444069860407206
    		lhA[0][5] = -2.431810333988042
    		lhA[0][6] = -1.446474596215561
    		lhA[0][7] = -1.4587341226347261
    		lhA[0][8] = -1.7487976320958225
    		lhA[0][9] = -0.5870190528383299
    		lhA[0][10] = 0.5592150697963958
    		lhA[0][11] = -0.6025635094610965
    		lhA[0][12] = -0.37948729810778037
    		lhA[0][13] = 2.5903039830419314
    		lhA[0][14] = 2.7700317547305473
    		lhA[0][15] = -0.1997595264191645
    		hlA[0][0] = 4.871392896287467
    		hlA[0][1] = -3.102563509461097
    		hlA[0][2] = -1.7978357377724807
    		hlA[0][3] = 0.029006350946109177
    		hlA[0][4] = 1.886057158514987
    		hlA[0][5] = -4.2487976320958225
    		hlA[0][6] = -1.977563509461097
    		hlA[0][7] = 4.340303983041931
    		hlA[0][8] = 1.1662658773652737
    		hlA[0][9] = -2.3549682452694523
    		hlA[0][10] = -1.7398230358802615
    		hlA[0][11] = 2.928525403784438
    		hlA[0][12] = 4.151601615137754
    		hlA[0][13] = -1.2087341226347261
    		hlA[0][14] = -1.5600952641916448
    		hlA[0][15] = -1.3827722283113837
    		hhA[0][0] = -2.962019052838329
    		hhA[0][1] = -1.1818103339880413
    		hhA[0][2] = -1.1294872981077808
    		hhA[0][3] = 5.273316684934151
    		hhA[0][4] = -0.02331668493415151
    		hhA[0][5] = 0.035576211353315984
    		hhA[0][6] = 0.9497595264191645
    		hhA[0][7] = -0.962019052838329
    		hhA[0][8] = -0.6964745962155621
    		hhA[0][9] = 1.8582531754730547
    		hhA[0][10] = -0.7120190528383288
    		hhA[0][11] = -0.44975952641916406
    		hhA[0][12] = 3.6818103339880417
    		hhA[0][13] = -0.7120190528383288
    		hhA[0][14] = 0.8917468245269453
    		hhA[0][15] = -3.861538105676658
    		llA[0][0] = 9.011057158514985
    		llA[0][1] = 10.779886545341357
    		llA[0][2] = 5.879487298107779
    		llA[0][3] = 4.110657911281409
    		llA[0][4] = 11.139342088718587
    		llA[0][5] = 8.776601615137755
    		llA[0][6] = 2.550240473580836
    		llA[0][7] = 4.91298094716167
    		llA[0][8] = 6.946474596215561
    		llA[0][9] = 5.757772228311383
    		llA[0][10] = 1.6629809471616712
    		llA[0][11] = 2.851683315065848
    		llA[0][12] = 4.818189666011959
    		llA[0][13] = 7.761057158514986
    		llA[0][14] = 4.992227771688615
    		llA[0][15] = 2.0493602791855876
    		lhA[1][0] = 3.1405444566227634
    		lhA[1][1] = 3.1405444566227674
    		lhA[1][2] = 3.140544456622764
    		lhA[1][3] = 3.1405444566227656
    		lhA[1][4] = 1.9395825622994223
    		lhA[1][5] = 1.939582562299428
    		lhA[1][6] = 1.9395825622994245
    		lhA[1][7] = 1.9395825622994245
    		lhA[1][8] = -3.140544456622765
    		lhA[1][9] = -3.1405444566227687
    		lhA[1][10] = -3.140544456622766
    		lhA[1][11] = -3.1405444566227674
    		lhA[1][12] = -1.939582562299424
    		lhA[1][13] = -1.939582562299429
    		lhA[1][14] = -1.9395825622994263
    		lhA[1][15] = -1.9395825622994263
    		hlA[1][0] = 4.207531754730547
    		hlA[1][1] = 4.7876587736527405
    		hlA[1][2] = -4.207531754730549
    		hlA[1][3] = -4.787658773652742
    		hlA[1][4] = 4.207531754730548
    		hlA[1][5] = 4.787658773652742
    		hlA[1][6] = -4.207531754730548
    		hlA[1][7] = -4.787658773652742
    		hlA[1][8] = 4.207531754730547
    		hlA[1][9] = 4.787658773652739
    		hlA[1][10] = -4.207531754730549
    		hlA[1][11] = -4.787658773652742
    		hlA[1][12] = 4.207531754730547
    		hlA[1][13] = 4.787658773652742
    		hlA[1][14] = -4.207531754730548
    		hlA[1][15] = -4.787658773652742
    		hhA[1][0] = -1.0759618943233418
    		hhA[1][1] = 1.8815698604072053
    		hhA[1][2] = 1.075961894323341
    		hhA[1][3] = -1.8815698604072058
    		hhA[1][4] = 4.381569860407205
    		hhA[1][5] = -0.9240381056766571
    		hhA[1][6] = -4.381569860407203
    		hhA[1][7] = 0.9240381056766567
    		hhA[1][8] = 1.0759618943233416
    		hhA[1][9] = -1.8815698604072062
    		hhA[1][10] = -1.075961894323341
    		hhA[1][11] = 1.8815698604072066
    		hhA[1][12] = -4.381569860407205
    		hhA[1][13] = 0.9240381056766567
    		hhA[1][14] = 4.381569860407204
    		hhA[1][15] = -0.9240381056766565*/
            // yl_corr = [
           // 11.7500   11.7500   11.7500   11.7500;
           // 11.7500   11.7500   11.7500   11.7500;
           // 11.7500   11.7500   11.7500   11.7500;
           // 11.7500   11.7500   11.7500   11.7500];
          // yh_corr = [
            // 4.5724    0.4285   -1.8828    2.2611    4.8714   -3.1026   -1.7978    0.0290   -2.9620   -1.1818   -1.1295    5.2733 ...
            // 3.1405    3.1405    3.1405    3.1405    4.2075    4.7877   -4.2075   -4.7877   -1.0760    1.8816    1.0760   -1.8816;
           // -2.4441   -2.4318   -1.4465   -1.4587    1.8861   -4.2488   -1.9776    4.3403   -0.0233    0.0356    0.9498   -0.9620 ...
            // 1.9396    1.9396    1.9396    1.9396    4.2075    4.7877   -4.2075   -4.7877    4.3816   -0.9240   -4.3816    0.9240;
           // -1.7488   -0.5870    0.5592   -0.6026    1.1663   -2.3550   -1.7398    2.9285   -0.6965    1.8583   -0.7120   -0.4498 ...
           // -3.1405   -3.1405   -3.1405   -3.1405    4.2075    4.7877   -4.2075   -4.7877    1.0760   -1.8816   -1.0760    1.8816;
           // -0.3795    2.5903    2.7700   -0.1998    4.1516   -1.2087   -1.5601   -1.3828    3.6818   -0.7120    0.8917   -3.8615 ...
           // -1.9396   -1.9396   -1.9396   -1.9396    4.2075    4.7877   -4.2075   -4.7877   -4.3816    0.9240    4.3816   -0.9240];
        // assertVectorsAlmostEqual(yl, yl_corr, 'relative', 0.001);
        // assertVectorsAlmostEqual(yh, yh_corr, 'relative', 0.001);
        }
        else if (test_mirdwt_1) {
        	//x = makesig("Leopold",8); // sets aArray
        	makeSig("Leopold",8);
	        //h = daubcqf(4,'min'); // sets scalingFilter and waveletFilter
        	filterType = MINIMUM_PHASE;
        	filterLength = 4;
        	scalingFilter = new double[filterLength];
            
            waveletFilter = new double[filterLength];
        	daubcqf();
	        //L = 1;
        	numberOfLevels = 1;
        	nDims = 1;
            xDim = 8;
            yDim = 1;
            sliceSize = 8;
            maximumLevel = 1;
	        // [yl, yh, L] = mrdwt(x, h, L);
            // Create low pass wavelet component
            yl = new double[sliceSize];
            lhA = new double[numberOfLevels][sliceSize];
            mrdwt();
            mirdwt();
            for (i = 0; i < 8; i++) {
                Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM);	
            }
            // Input and output aArray match
            // aArray[0] = 5.204170427930421E-18
            // aArray[1] = 0.9999999999999999
    		// aArray[2] = 5.204170427930421E-18
    		// aArray[3] = 2.7755575615628914E-17
    		// aArray[4] = 0.0
    		// aArray[5] = 0.0
    		// aArray[6] = 0.0
    		// aArray[7] = 2.7755575615628914E-17
            // Correct aArray = [0     1     0     0     0     0     0     0];
            setCompleted(true);
            return;	
        }
        else if (test_mirdwt_2D) {
        	// load lena512; 
            // x = lena512;
            // h = daubcqf(6);
            // [yl,yh,L] = mrdwt(x,h);
            // assertEqual(L,9);
            // [x_new,L] = mirdwt(yl,yh,h);
            // assertEqual(L,9);
            // assertVectorsAlmostEqual(x, x_new,'relative',0.0001);
        	final FileIO io = new FileIO();
			 io.setQuiet(true);
			 io.setSuppressProgressBar(true);
			 ModelImage image = io.readImage("C:" + File.separator + "Rice Wavelet Toolbox" +
			 File.separator
			 + "rwt-master" + File.separator + "tests" + File.separator +
			 "lena512.mat");
			 nDims = 2;
			 xDim = 512;
			 yDim = 512;
			 sliceSize = xDim * yDim;
			 double aArrayOriginal[] = new double[sliceSize];
			 try {
				 image.exportData(0, sliceSize, aArrayOriginal);
			 }
			 catch (IOException e) {
				 MipavUtil.displayError("IOException on srcImage.exportData(0, sliceSize, aArrayOriginal");
				 setCompleted(false);
				 return;
			 }
			 image.disposeLocal();
			 image = null;
			 aArray = new double[sliceSize];
			 for (i = 0; i < sliceSize; i++ ) {
				 aArray[i] = aArrayOriginal[i];
			 }
			 i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            k = yDim;
	            i = 0;
	            while((k % 2) == 0) {
	                k = (k >> 1);
	                i++;
	            }
	            
	            numberOfLevels = Math.min(i, j);
	            maximumLevel = numberOfLevels;
	            Preferences.debug("The maximum possible number of levels = " + numberOfLevels + "\n", Preferences.DEBUG_FILEIO);
	            filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
	            // Create low pass wavelet component
	            yl = new double[sliceSize];
	            // Save the low pass component for each level
	            llA = new double[numberOfLevels-1][sliceSize];
	            
	            // Create 3 high pass components for each level
	            lhA = new double[numberOfLevels][sliceSize];
	            hlA = new double[numberOfLevels][sliceSize];
	            hhA = new double[numberOfLevels][sliceSize];
	            extents = new int[] {512,512};
	            waveletImage = new ModelImage[4];
	            mrdwt();
	            mirdwt();
	            double maxDiff = 0;
	            for (i = 0; i < sliceSize; i++) {
	                double absDiff = Math.abs(aArrayOriginal[i] - aArray[i]);
	                if (absDiff > maxDiff) {
	                	maxDiff = absDiff;
	                }
	            }
	            Preferences.debug("maxDiff = " + maxDiff + "\n", Preferences.DEBUG_ALGORITHM);
	            setCompleted(true);
	            return;
	            // Test passes
	            // The maximum possible number of levels = 9
	            // maxDiff = 1.7337242752546445E-12
        }
        else if (test_mdwt_1D) {
        	  // x = makesig('LinChirp', 8); // sets aArray
        	  makeSig("LinChirp",8);
        	  //h = daubcqf(4,'min'); // sets scalingFilter and waveletFilter
          	  filterType = MINIMUM_PHASE;
          	  filterLength = 4;
          	  scalingFilter = new double[filterLength];
              
              waveletFilter = new double[filterLength];
          	  daubcqf();
        	  //L = 2;  % For 8 values in x we would normally be L=2 
          	  numberOfLevels = 2;
          	  nDims = 1;
              xDim = 8;
              yDim = 1;
              sliceSize = 8;
              y = new double[sliceSize];
              waveletImage = null;
        	  //[y, L] = mdwt(x, h, L);
              mdwt();
              for (i = 0; i < sliceSize; i++) {
              	Preferences.debug("y["+i+"] = " + y[i] + "\n", Preferences.DEBUG_ALGORITHM); 
              }
              // y and y_corr match
              // y[0] = 1.1096922627375003
    		  // y[1] = 0.8766618229593225
    		  // y[2] = 0.8203918521066691
    		  // y[3] = -0.5200740936425833
    		  // y[4] = -0.03392766824720583
    		  // y[5] = 0.10011069546128454
    		  // y[6] = 0.22008824024609502
    		  // y[7] = -0.14008160439760842
        	  //y_corr = [1.1097 0.8767 0.8204 -0.5201 -0.0339 0.1001 0.2201 -0.1401];
        	  //L_corr = 2;
        	  //assertVectorsAlmostEqual(y, y_corr, 'relative', 0.001);
        	  // assertEqual(L, L_corr);
            setCompleted(true);
            return;
        }
        else if (test_mdwt_2D) {
        	aArray = new double[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};
        	//h = daubcqf(4,'min'); // sets scalingFilter and waveletFilter
      	  filterType = MINIMUM_PHASE;
      	  filterLength = 4;
      	  scalingFilter = new double[filterLength];
          
          waveletFilter = new double[filterLength];
      	  daubcqf();
          //y = mdwt(x, h);
      	numberOfLevels = 2;
  	    nDims = 2;
        xDim = 4;
        yDim = 4;
        sliceSize = 16;
        y = new double[sliceSize];
        extents = new int[] {4,4};
        waveletImage = new ModelImage[1];
        mdwt();
        for (i = 0; i < sliceSize; i++) {
      	  Preferences.debug("y["+i+"] = " + y[i] + "\n", Preferences.DEBUG_ALGORITHM); 
        }
        // y and y_corr match
        // y[0] = 33.999999999999986
		// y[1] = -3.464101615137755
		// y[2] = -7.6194792136857E-16
		// y[3] = -2.000000000000001
		// y[4] = -13.856406460551018
		// y[5] = -1.5543122344752192E-15
		// y[6] = -1.5931907666570925E-15
		// y[7] = -2.000000000000001
		// y[8] = 1.7763568394002505E-15
		// y[9] = -1.7763568394002505E-15
		// y[10] = -9.287196608840452E-16
		// y[11] = 1.1102230246251565E-16
		// y[12] = -7.999999999999998
		// y[13] = -8.0
		// y[14] = 4.576919232789127E-16
		// y[15] = 7.771561172376096E-16
        // y_corr = [34.0000 -3.4641 0.0000 -2.0000; -13.8564 0.0000 0.0000 -2.0000; -0.0000 0.0000 -0.0000 -0.0000; -8.0000 -8.0000 0.0000 -0.0000];
         //assertVectorsAlmostEqual(y, y_corr, 'relative', 0.001);
      	    
          setCompleted(true);
          return;
      }
        else if (test_midwt_1D) {
      	  // x = makesig('LinChirp', 8); // sets aArray
      	  makeSig("LinChirp",8);
      	  double aArrayOriginal[] = new double[8];
      	  for (i = 0; i < 8; i++) {
      		  aArrayOriginal[i] = aArray[i];
      	  }
      	  //h = daubcqf(4,'min'); // sets scalingFilter and waveletFilter
        	  filterType = MINIMUM_PHASE;
        	  filterLength = 4;
        	  scalingFilter = new double[filterLength];
            
            waveletFilter = new double[filterLength];
        	  daubcqf();
      	  //L = 2;  % For 8 values in x we would normally be L=2 
        	  numberOfLevels = 2;
        	  nDims = 1;
            xDim = 8;
            yDim = 1;
            sliceSize = 8;
            y = new double[sliceSize];
            waveletImage = null;
      	  //[y, L] = mdwt(x, h, L);
            mdwt();
            //[x_new,L] = midwt(y,h,L);
            midwt();
            for (i = 0; i < sliceSize; i++) {
            	Preferences.debug("aArrayOriginal["+i+"] = " + aArrayOriginal[i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
            for (i = 0; i < sliceSize; i++) {
            	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
            // aArrayOriginal and aArray match
            // aArrayOriginal[0] = 0.049067674327418015
    		// aArrayOriginal[1] = 0.19509032201612825
    		// aArrayOriginal[2] = 0.4275550934302821
    		// aArrayOriginal[3] = 0.7071067811865475
    		// aArrayOriginal[4] = 0.9415440651830208
    		// aArrayOriginal[5] = 0.9807852804032304
    		// aArrayOriginal[6] = 0.6715589548470186
    		// aArrayOriginal[7] = 1.2246467991473532E-16
    		// aArray[0] = 0.04906767432741811
    		// aArray[1] = 0.19509032201612841
    		// aArray[2] = 0.42755509343028203
    		// aArray[3] = 0.7071067811865474
    		// aArray[4] = 0.9415440651830206
    		// aArray[5] = 0.9807852804032302
    		// aArray[6] = 0.6715589548470184
    		// aArray[7] = 1.1102230246251565E-16
          setCompleted(true);
          return;
      }
        else if (test_midwt_2D) {
        	// load lena512; 
            // x = lena512;
            // h = daubcqf(6);
            // [yl,yh,L] = mrdwt(x,h);
            // assertEqual(L,9);
            // [x_new,L] = mirdwt(yl,yh,h);
            // assertEqual(L,9);
            // assertVectorsAlmostEqual(x, x_new,'relative',0.0001);
        	final FileIO io = new FileIO();
			 io.setQuiet(true);
			 io.setSuppressProgressBar(true);
			 ModelImage image = io.readImage("C:" + File.separator + "Rice Wavelet Toolbox" +
			 File.separator
			 + "rwt-master" + File.separator + "tests" + File.separator +
			 "lena512.mat");
			 nDims = 2;
			 xDim = 512;
			 yDim = 512;
			 sliceSize = xDim * yDim;
			 double aArrayOriginal[] = new double[sliceSize];
			 try {
				 image.exportData(0, sliceSize, aArrayOriginal);
			 }
			 catch (IOException e) {
				 MipavUtil.displayError("IOException on srcImage.exportData(0, sliceSize, aArrayOriginal");
				 setCompleted(false);
				 return;
			 }
			 image.disposeLocal();
			 image = null;
			 aArray = new double[sliceSize];
			 for (i = 0; i < sliceSize; i++ ) {
				 aArray[i] = aArrayOriginal[i];
			 }
			 i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            k = yDim;
	            i = 0;
	            while((k % 2) == 0) {
	                k = (k >> 1);
	                i++;
	            }
	            
	            numberOfLevels = Math.min(i, j);
	            maximumLevel = numberOfLevels;
	            Preferences.debug("The maximum possible number of levels = " + numberOfLevels + "\n", Preferences.DEBUG_FILEIO);
	            filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
	            y = new double[sliceSize];
	            extents = new int[] {512,512};
	            waveletImage = new ModelImage[1];
	            mdwt();
	            midwt();
	            double maxDiff = 0;
	            for (i = 0; i < sliceSize; i++) {
	                double absDiff = Math.abs(aArrayOriginal[i] - aArray[i]);
	                if (absDiff > maxDiff) {
	                	maxDiff = absDiff;
	                }
	            }
	            Preferences.debug("maxDiff = " + maxDiff + "\n", Preferences.DEBUG_ALGORITHM);
	            setCompleted(true);
	            return;
	            // Test passes
	            // The maximum possible number of levels = 9
	            // maxDiff = 2.0747847884194925E-12
        }
        else if (test_denoise_default) {
        	  //signal = makesig('Doppler', 32); // sets aArray
        	  makeSig("Doppler",32);
        	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
        		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
        		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
        		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
        		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
        	  //with_noise = signal + noise / 10;
        	  for (i = 0; i < 32; i++) {
        		  aArray[i] += noise[i]/10.0;
        	  }
        	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
        	    i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            numberOfLevels = j;
	            maximumLevel = numberOfLevels;
        	    filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
        	  //h = daubcqf(6)
        	  //[signal_denoised, subtracted_noise, actual_options] = denoise(with_noise, h);
	            mdwt();
	            numX = xDim - (int)Math.floor(xDim/2);
	            numberValues = numX;
	            tmp = new double[numberValues];
        		for (i = numX; i < xDim; i++) {
        	        tmp[i-numX] = y[i];
        		}
        		for (i = 0; i < numberValues; i++) {
					tmp[i] = Math.abs(tmp[i]);
				}
				Arrays.sort(tmp);
				if ((numberValues %2) == 0) {
				    median = (tmp[numberValues/2] + tmp[numberValues/2 - 1])/2.0; 
				}
				else {
					median = tmp[(numberValues - 1)/2];
				}
				thresholdMultiplier = 3.0;
				thld = thresholdMultiplier*median/.67;
				twoL = 1;
            	for (i = 0; i < numberOfLevels; i++) {
            		twoL = 2 * twoL;
            	}
        	    ix = xDim/twoL;
        	    ykeep = new double[xDim];
        	    for (i = 0; i < ix; i++) {
        	    	ykeep[i] = y[i];
        	    }
        	    for (i = 0; i < sliceSize; i++) {
			    	absVal = Math.abs(y[i]);
			    	if (absVal <= thld) {
			    		y[i] = 0.0;
			    	}
			    	else {
			    		y[i] = Math.signum(y[i])*(absVal - thld);
			    	}
			    }
        	    for (i = 0; i < ix; i++) {
        	    	y[i] = ykeep[i];
        	    }
        	    midwt();
        	    for (i = 0; i < sliceSize; i++) {
                	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
                }
        	    // aAArray and signal_denoised_corr match
        	    /*aArray[0] = 0.0741827688375061
	    		aArray[1] = 0.07917019025262682
	    		aArray[2] = 0.07608426152723406
	    		aArray[3] = 0.07504768317741797
	    		aArray[4] = 0.1112797747795676
	    		aArray[5] = 0.1634750532835444
	    		aArray[6] = -0.04982638153505337
	    		aArray[7] = 0.09460730882373157
	    		aArray[8] = 0.1351265624869108
	    		aArray[9] = -0.018609062095819452
	    		aArray[10] = -0.07488124799912885
	    		aArray[11] = -0.10347020605942651
	    		aArray[12] = 0.023425484325177562
	    		aArray[13] = 0.23977254083625654
	    		aArray[14] = 0.09205833989623083
	    		aArray[15] = -0.15218064036689205
	    		aArray[16] = -0.11668207330615624
	    		aArray[17] = -0.045938985076278145
	    		aArray[18] = -0.002452400397783411
	    		aArray[19] = 0.07557391641048371
	    		aArray[20] = 0.10254833351221398
	    		aArray[21] = 0.12109991174418425
	    		aArray[22] = 0.17739050792162042
	    		aArray[23] = 0.24038604155309365
	    		aArray[24] = 0.23110593331715737
	    		aArray[25] = 0.19821092449327385
	    		aArray[26] = 0.1756728129907254
	    		aArray[27] = 0.13882204961303488
	    		aArray[28] = 0.1274916153878268
	    		aArray[29] = 0.12140959718632514
	    		aArray[30] = 0.09949353201307842
	    		aArray[31] = 0.07600193408654268*/
        	  //signal_denoised_corr = [0.0741827688375062 0.0791701902526268 0.0760842615272340 0.0750476831774179 0.111279774779568
        	  //0.163475053283544 -0.0498263815350539 0.0946073088237311 0.135126562486911 -0.0186090620958193 -0.0748812479991294 -0.103470206059426
        	  //0.0234254843251780 0.239772540836257 0.0920583398962312 -0.152180640366891 -0.116682073306156 -0.0459389850762785 -0.00245240039778375
        	  // 0.0755739164104836 0.102548333512214 0.121099911744184 0.177390507921620 0.240386041553093 0.231105933317157 0.198210924493273
        	  //0.175672812990725 0.138822049613034 0.127491615387826 0.121409597186325 0.0994935320130783 0.0760019340865427];
        	  //assertVectorsAlmostEqual(signal_denoised, signal_denoised_corr, 'relative', 0.0001);
        	  setCompleted(true);
        	  return;
        } // else if (test_denoise_default)
        else if (test_denoise_2D) {
          //[signal_denoised, subtracted_noise, actual_options] = denoise(x, h);
        	  
        	aArray = new double[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10.09, 11, 12, 13, 13.91, 15, 16};
        	//h = daubcqf(4,'min'); // sets scalingFilter and waveletFilter
      	  filterType = MINIMUM_PHASE;
      	  filterLength = 4;
      	  scalingFilter = new double[filterLength];
          
          waveletFilter = new double[filterLength];
      	  daubcqf();
      	numberOfLevels = 2;
      	maximumLevel = 2;
  	    nDims = 2;
        xDim = 4;
        yDim = 4;
        sliceSize = 16;
        y = new double[sliceSize];
        extents = new int[] {4,4};
        waveletImage = null;
        destImage = null;
        mdwt();
        numX = xDim - (int)Math.floor(xDim/2);
		numY = yDim - (int)Math.floor(yDim/2);
		numberValues = numX*numY;
		tmp = new double[numberValues];
		for (i = numY; i < yDim; i++) {
			for (j = numX; j < xDim; j++) {
			    tmp[(i-numY)*numX + j-numX] = y[i*xDim + j];
			}
		}
		for (i = 0; i < numberValues; i++) {
			tmp[i] = Math.abs(tmp[i]);
		}
		Arrays.sort(tmp);
		if ((numberValues %2) == 0) {
		    median = (tmp[numberValues/2] + tmp[numberValues/2 - 1])/2.0; 
		}
		else {
			median = tmp[(numberValues - 1)/2];
		}
		thresholdMultiplier = 3.0;
		thld = thresholdMultiplier*median/.67;
		twoL = 1;
    	for (i = 0; i < numberOfLevels; i++) {
    		twoL = 2 * twoL;
    	}
	    ix = yDim/twoL;
	    jx = xDim/twoL;
	    ykeep = new double[sliceSize];
	    for (i = 0; i < ix; i++) {
	    	for (j = 0; j < jx; j++) {
	    		ykeep[i*xDim + j] = y[i*xDim + j];
	    	}
	    }
	    for (i = 0; i < sliceSize; i++) {
	    	absVal = Math.abs(y[i]);
	    	if (absVal <= thld) {
	    		y[i] = 0.0;
	    	}
	    	else {
	    		y[i] = Math.signum(y[i])*(absVal - thld);
	    	}
	    }
	    for (i = 0; i < ix; i++) {
	    	for (j = 0; j < jx; j++) {
	    		y[i*xDim + j] = ykeep[i*xDim + j];
	    	}
	    }
	    midwt();
	    for (i = 0; i < sliceSize; i++) {
        	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
        }
	    // aArray and signal_denoised_corr match
	    /*aArray[0] = 1.0934958015873275
		aArray[1] = 2.052784169768515
		aArray[2] = 3.036985129109064
		aArray[3] = 4.014510779767095
		aArray[4] = 5.037416383975939
		aArray[5] = 6.006178652683392
		aArray[6] = 6.994963120759169
		aArray[7] = 7.978382656683507
		aArray[8] = 9.04759354668492
		aArray[9] = 10.003998510025584
		aArray[10] = 10.977825887256135
		aArray[11] = 11.946984942754687
		aArray[12] = 13.009489364401716
		aArray[13] = 13.93703866752249
		aArray[14] = 14.93985272854726
		aArray[15] = 15.922499658473127*/
        //signal_denoised_corr = [1.093495801587334   2.052784169768518   3.036985129109070   4.014510779767102;  
        //5.037416383975946   6.006178652683398   6.994963120759174   7.978382656683513; 
        //9.047593546684929  10.003998510025589  10.977825887256145  11.94698494275469; 
        //13.009489364401729  13.937038667522501  14.939852728547271  15.9224996584731398];
	    setCompleted(true);
	    return;
        } // else if (test_denoise_2D)
        else if (test_denoise_udwt) {
        	//signal = makesig('Doppler', 32); // sets aArray
      	  makeSig("Doppler",32);
      	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
      		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
      		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
      		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
      		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
      	  //with_noise = signal + noise / 10;
      	  for (i = 0; i < 32; i++) {
      		  aArray[i] += noise[i]/10.0;
      	  }
      	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
      	    i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            numberOfLevels = j;
	            maximumLevel = numberOfLevels;
      	        filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
      	        //h = daubcqf(6)
	            // Create low pass wavelet component
	            yl = new double[sliceSize];
	            lhA = new double[numberOfLevels][sliceSize];
	            
	            waveletImage = null;
	            destImage = null;
	            mrdwt();
            	srchA = lhA;
            	tmp = new double[sliceSize];
    		    for (i = 0; i < sliceSize; i++) {
    				tmp[i] = srchA[0][i];
    			}
    			for (i = 0; i < sliceSize; i++) {
					tmp[i] = Math.abs(tmp[i]);
				}
				Arrays.sort(tmp);
				if((sliceSize %2) == 0) {
				    median = (tmp[sliceSize/2] + tmp[sliceSize/2 - 1])/2.0; 
				}
				else {
					median = tmp[(sliceSize - 1)/2];
				}
				thresholdMultiplier = 3.6;
				thld = thresholdMultiplier*median/.67;
				for (i = minimumLevel-1; i < maximumLevel; i++) {
    				for (j = 0; j < sliceSize; j++) {
    					absVal = Math.abs(srchA[i][j]);
    					if (absVal <= thld) {
    						srchA[i][j] = 0.0;
    					}
    				}
    			}
	            mirdwt();
	            for (i = 0; i < sliceSize; i++) {
	            	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
	            }
	            // aArray and signal_denoised_corr match
	            /*aArray[0] = 0.1262446153851522
        		aArray[1] = 0.09523197124253038
        		aArray[2] = 0.06713436071525052
        		aArray[3] = 0.051390297972258595
        		aArray[4] = 0.04304027326826328
        		aArray[5] = 0.05869325751317936
        		aArray[6] = 0.08610697519026987
        		aArray[7] = 0.0989949047763018
        		aArray[8] = 0.09084186581286383
        		aArray[9] = -0.014145467011905916
        		aArray[10] = -0.1447915274370268
        		aArray[11] = -0.0185533166035904
        		aArray[12] = 0.27835161378213097
        		aArray[13] = 0.2790337063766599
        		aArray[14] = -0.020501203205426165
        		aArray[15] = -0.21236765840797703
        		aArray[16] = -0.24148434369799646
        		aArray[17] = -0.24858229883105987
        		aArray[18] = -0.2133742147817445
        		aArray[19] = -0.10196371214110948
        		aArray[20] = 0.045424885131056435
        		aArray[21] = 0.18110433394974934
        		aArray[22] = 0.27529440729325916
        		aArray[23] = 0.30907625988205994
        		aArray[24] = 0.2986004503850738
        		aArray[25] = 0.25908073779660795
        		aArray[26] = 0.2111235358017181
        		aArray[27] = 0.18302178352573945
        		aArray[28] = 0.17196634086657622
        		aArray[29] = 0.17161681258609737
        		aArray[30] = 0.16872000630019302
        		aArray[31] = 0.1510664281840722*/
	            //[signal_denoised, subtracted_noise, actual_options] = denoise(with_noise, h, 1);
	            //signal_denoised_corr = [0.126244615385152 0.0952319712425300 0.0671343607152503 0.0513902979722585 0.0430402732682634
	            //0.0586932575131794 0.0861069751902698 0.0989949047763016 0.0908418658128637 -0.0141454670119059 -0.144791527437026 
	            //-0.0185533166035902 0.278351613782131 0.279033706376659 -0.0205012032054263 -0.212367658407976 -0.241484343697995 
	            //-0.248582298831059 -0.213374214781743 -0.101963712141109 0.0454248851310567 0.181104333949749 0.275294407293259 
	            //0.309076259882059 0.298600450385073 0.259080737796607 0.211123535801718 0.183021783525739 0.171966340866576 
	            //0.171616812586097 0.168720006300193 0.151066428184072];
	            setCompleted(true);
	            return;
        } // else if (test_denoise_udwt)
        else if (test_denoise_udwt_2d) {
        	aArray = new double[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10.09, 11, 12, 13, 13.91, 15, 16};
        	//h = daubcqf(4,'min'); // sets scalingFilter and waveletFilter
      	  filterType = MINIMUM_PHASE;
      	  filterLength = 4;
      	  scalingFilter = new double[filterLength];
          
          waveletFilter = new double[filterLength];
      	  daubcqf();
	      	numberOfLevels = 2;
	      	maximumLevel = 2;
	  	    nDims = 2;
	        xDim = 4;
	        yDim = 4;
	        sliceSize = 16;	
	        // Create low pass wavelet component
	        yl = new double[sliceSize];
	        // Savethe low pass component for each level
	        llA = new double[numberOfLevels-1][sliceSize];
	        
	        // Create 3 high pass components for each level
	        lhA = new double[numberOfLevels][sliceSize];
	        hlA = new double[numberOfLevels][sliceSize];
	        hhA = new double[numberOfLevels][sliceSize];
	        extents = new int[] {4,4};
	        waveletImage = null;
	        destImage = null;
	        mrdwt();
	        srchA = hhA;
	        tmp = new double[sliceSize];
			for (i = 0; i < sliceSize; i++) {
				tmp[i] = srchA[0][i];
			}
			for (i = 0; i < sliceSize; i++) {
				tmp[i] = Math.abs(tmp[i]);
			}
			Arrays.sort(tmp);
			if ((sliceSize % 2) == 0) {
			    median = (tmp[sliceSize/2] + tmp[sliceSize/2 - 1])/2.0; 
			}
			else {
				median = tmp[(sliceSize - 1)/2];
			}
			thresholdMultiplier = 3.6;
			thld = thresholdMultiplier*median/.67;
			for (i = minimumLevel-1; i < maximumLevel; i++) {
				for (j = 0; j < sliceSize; j++) {
					absVal = Math.abs(srchA[i][j]);
					if (absVal <= thld) {
						srchA[i][j] = 0.0;
					}
				}
			}
			for (i = minimumLevel-1; i < maximumLevel; i++) {
				for (j = 0; j < sliceSize; j++) {
					absVal = Math.abs(lhA[i][j]);
					if (absVal <= thld) {
						lhA[i][j] = 0.0;
					}
				}
			}	
			
			for (i = minimumLevel-1; i < maximumLevel; i++) {
				for (j = 0; j < sliceSize; j++) {
					absVal = Math.abs(hlA[i][j]);
					if (absVal <= thld) {
						hlA[i][j] = 0.0;
					}
				}
			}
			mirdwt();
			for (i = 0; i < sliceSize; i++) {
            	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
			// aArray and signal_denoised_corr match
			/*aArray[0] = 1.0070404888661963
			aArray[1] = 1.9934052745217659
			aArray[2] = 3.0062684040300893
			aArray[3] = 3.996424654030088
			aArray[4] = 4.995935171857873
			aArray[5] = 6.002401216530088
			aArray[6] = 7.001252328142124
			aArray[7] = 8.005847881693981
			aArray[8] = 9.009508189685654
			aArray[9] = 10.059981743374518
			aArray[10] = 11.001190131625476
			aArray[11] = 11.999030274521763
			aArray[12] = 12.987516149590267
			aArray[13] = 13.944211765573618
			aArray[14] = 14.991289136202303
			aArray[15] = 15.998697189754157*/
	        // signal_denoised_corr = [
            // 1.007040488866197   1.993405274521765   3.006268404030089   3.996424654030090;
            // 4.995935171857875   6.002401216530091   7.001252328142127   8.005847881693983;
            // 9.009508189685661  10.059981743374523  11.001190131625481  11.999030274521770;
           // 12.987516149590270  13.944211765573623  14.991289136202310  15.998697189754166];
		   setCompleted(true);
		   return;
        } // else if (test_denoise_udwt_2d)
        else if (test_denoise_threshold_low) {
      	  //signal = makesig('Doppler', 32); // sets aArray
      	  makeSig("Doppler",32);
      	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
      		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
      		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
      		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
      		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
      	  //with_noise = signal + noise / 10;
      	  for (i = 0; i < 32; i++) {
      		  aArray[i] += noise[i]/10.0;
      	  }
      	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
      	    i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            numberOfLevels = j;
	            maximumLevel = numberOfLevels;
      	    filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
      	  //h = daubcqf(6)
      	  //[signal_denoised, subtracted_noise, actual_options] = denoise(with_noise, h);
	            thresholdLowPass = true;
	            mdwt();
	            numX = xDim - (int)Math.floor(xDim/2);
	            numberValues = numX;
	            tmp = new double[numberValues];
      		for (i = numX; i < xDim; i++) {
      	        tmp[i-numX] = y[i];
      		}
      		for (i = 0; i < numberValues; i++) {
					tmp[i] = Math.abs(tmp[i]);
				}
				Arrays.sort(tmp);
				if ((numberValues %2) == 0) {
				    median = (tmp[numberValues/2] + tmp[numberValues/2 - 1])/2.0; 
				}
				else {
					median = tmp[(numberValues - 1)/2];
				}
				thresholdMultiplier = 3.0;
				thld = thresholdMultiplier*median/.67;
				twoL = 1;
          	for (i = 0; i < numberOfLevels; i++) {
          		twoL = 2 * twoL;
          	}
      	    ix = xDim/twoL;
      	    ykeep = new double[xDim];
      	    for (i = 0; i < ix; i++) {
      	    	ykeep[i] = y[i];
      	    }
      	    for (i = 0; i < sliceSize; i++) {
			    	absVal = Math.abs(y[i]);
			    	if (absVal <= thld) {
			    		y[i] = 0.0;
			    	}
			    	else {
			    		y[i] = Math.signum(y[i])*(absVal - thld);
			    	}
			}
      	    midwt();
      	    for (i = 0; i < sliceSize; i++) {
              	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
      	    // aArray and signal_denoised_corr match
      	    /*aArray[0] = 0.018774235427834875
			aArray[1] = 0.023761656842955597
			aArray[2] = 0.020675728117562825
			aArray[3] = 0.019639149767746722
			aArray[4] = 0.05587124136989634
			aArray[5] = 0.10806651987387313
			aArray[6] = -0.10523491494472464
			aArray[7] = 0.03919877541406031
			aArray[8] = 0.07971802907723954
			aArray[9] = -0.0740175955054907
			aArray[10] = -0.1302897814088001
			aArray[11] = -0.15887873946909772
			aArray[12] = -0.03198304908449369
			aArray[13] = 0.18436400742658526
			aArray[14] = 0.03664980648655958
			aArray[15] = -0.20758917377656327
			aArray[16] = -0.17209060671582743
			aArray[17] = -0.10134751848594939
			aArray[18] = -0.05786093380745464
			aArray[19] = 0.020165383000812456
			aArray[20] = 0.047139800102542734
			aArray[21] = 0.06569137833451298
			aArray[22] = 0.12198197451194918
			aArray[23] = 0.18497750814342243
			aArray[24] = 0.17569739990748612
			aArray[25] = 0.1428023910836026
			aArray[26] = 0.12026427958105415
			aArray[27] = 0.08341351620336362
			aArray[28] = 0.07208308197815555
			aArray[29] = 0.0660010637766539
			aArray[30] = 0.04408499860340718
			aArray[31] = 0.020593400676871456*/
      	    //signal_denoised_corr = [0.0187742354278351 0.0237616568429558 0.0206757281175629 0.0196391497677469 0.0558712413698966
      	    //0.108066519873873 -0.105234914944725 0.0391987754140600 0.0797180290772401 -0.0740175955054904 -0.130289781408801
      	    //-0.158878739469097 -0.0319830490844931 0.184364007426586 0.0366498064865601 -0.207589173776562 -0.172090606715827
      	    //-0.101347518485950 -0.0578609338074549 0.0201653830008125 0.0471398001025425 0.0656913783345127 0.121981974511949
      	    //0.184977508143422 0.175697399907486 0.142802391083602 0.120264279581054 0.0834135162033633 0.0720830819781554
      	    //0.0660010637766539 0.0440849986034073 0.0205934006768717];
      	    setCompleted(true);
      	    return;
        } // else if (test_denoise_threshold_low)
        else if (test_denoise_thresh_multiplier) {
        	  //signal = makesig('Doppler', 32); // sets aArray
        	  makeSig("Doppler",32);
        	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
        		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
        		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
        		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
        		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
        	  //with_noise = signal + noise / 10;
        	  for (i = 0; i < 32; i++) {
        		  aArray[i] += noise[i]/10.0;
        	  }
        	  nDims = 1;
  	            xDim = 32;
  	            yDim = 1;
  	            sliceSize = 32;
  	            y = new double[sliceSize];
  	            waveletImage = null;
        	    i = xDim;
  	            j = 0;
  	            while ((i % 2) == 0) {
  	                i = (i >> 1);
  	                j++;
  	            }
  	            numberOfLevels = j;
  	            maximumLevel = numberOfLevels;
        	    filterType = MINIMUM_PHASE;
  	            filterLength = 6;
  	            scalingFilter = new double[filterLength];
  	            waveletFilter = new double[filterLength];
  	            daubcqf();
        	  //h = daubcqf(6)
        	  //[signal_denoised, subtracted_noise, actual_options] = denoise(with_noise, h);
  	            thresholdLowPass = true;
  	            mdwt();
  	            numX = xDim - (int)Math.floor(xDim/2);
  	            numberValues = numX;
  	            tmp = new double[numberValues];
        		for (i = numX; i < xDim; i++) {
        	        tmp[i-numX] = y[i];
        		}
        		for (i = 0; i < numberValues; i++) {
  					tmp[i] = Math.abs(tmp[i]);
  				}
  				Arrays.sort(tmp);
  				if ((numberValues %2) == 0) {
  				    median = (tmp[numberValues/2] + tmp[numberValues/2 - 1])/2.0; 
  				}
  				else {
  					median = tmp[(numberValues - 1)/2];
  				}
  				thresholdMultiplier = 3.5;
  				thld = thresholdMultiplier*median/.67;
  				twoL = 1;
            	for (i = 0; i < numberOfLevels; i++) {
            		twoL = 2 * twoL;
            	}
        	    ix = xDim/twoL;
        	    ykeep = new double[xDim];
        	    for (i = 0; i < ix; i++) {
        	    	ykeep[i] = y[i];
        	    }
        	    for (i = 0; i < sliceSize; i++) {
  			    	absVal = Math.abs(y[i]);
  			    	if (absVal <= thld) {
  			    		y[i] = 0.0;
  			    	}
  			    	else {
  			    		y[i] = Math.signum(y[i])*(absVal - thld);
  			    	}
  			    }
        	    midwt();
        	    for (i = 0; i < sliceSize; i++) {
                	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
                }
        	    // aArray and signal_denoised_corr match
        	    /*aArray[0] = 0.005635270748034314
	    		aArray[1] = 0.011085305240404526
	    		aArray[2] = 0.01015901934719145
	    		aArray[3] = 0.011678951854607196
	    		aArray[4] = 0.03546256584432051
	    		aArray[5] = 0.06919046064269771
	    		aArray[6] = -0.06470102521879662
	    		aArray[7] = 0.03934850970120357
	    		aArray[8] = 0.030229774647826318
	    		aArray[9] = -0.06582302964018759
	    		aArray[10] = -0.0947938063374138
	    		aArray[11] = -0.14794315185100945
	    		aArray[12] = -0.0355607514547518
	    		aArray[13] = 0.14302782780048917
	    		aArray[14] = 0.012675297797007476
	    		aArray[15] = -0.20057766382158412
	    		aArray[16] = -0.1490592590076559
	    		aArray[17] = -0.05644321019402142
	    		aArray[18] = -0.028136507066194926
	    		aArray[19] = 0.020102137187145888
	    		aArray[20] = 0.04384127727873706
	    		aArray[21] = 0.0596866399869514
	    		aArray[22] = 0.09671019379894606
	    		aArray[23] = 0.13645164191756548
	    		aArray[24] = 0.13071630710708848
	    		aArray[25] = 0.10914691438813172
	    		aArray[26] = 0.09252008496534367
	    		aArray[27] = 0.06576074173634136
	    		aArray[28] = 0.05505849108988604
	    		aArray[29] = 0.04696362314481811
	    		aArray[30] = 0.027726848617731125
	    		aArray[31] = 0.006671354073980545*/
        	    //signal_denoised_corr = [0.00563527074803461 0.0110853052404048 0.0101590193471916 0.0116789518546074 0.0354625658443208
        	    //0.0691904606426981 -0.0647010252187970 0.0393485097012034 0.0302297746478269 -0.0658230296401878 -0.0947938063374137
        	    //-0.147943151851009 -0.0355607514547514 0.143027827800490 0.0126752977970079 -0.200577663821584 -0.149059259007655
        	    //-0.0564432101940217 -0.0281365070661950 0.0201021371871464 0.0438412772787373 0.0596866399869512 0.0967101937989458
        	    //0.136451641917565 0.130716307107088 0.109146914388131 0.0925200849653435 0.0657607417363412 0.0550584910898860
        	    //0.0469636231448182 0.0277268486177313 0.00667135407398081];
        	    setCompleted(true);
        	    return;
        } // else if (test_denoise_thresh_multiplier)
        else if (test_denoise_std) {
      	  //signal = makesig('Doppler', 32); // sets aArray
      	  makeSig("Doppler",32);
      	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
      		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
      		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
      		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
      		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
      	  //with_noise = signal + noise / 10;
      	  for (i = 0; i < 32; i++) {
      		  aArray[i] += noise[i]/10.0;
      	  }
      	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
      	    i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            numberOfLevels = j;
	            maximumLevel = numberOfLevels;
      	    filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
      	  //h = daubcqf(6)
      	  //[signal_denoised, subtracted_noise, actual_options] = denoise(with_noise, h);
	            mdwt();
	            numX = xDim - (int)Math.floor(xDim/2);
	            numberValues = numX;
	            tmp = new double[numberValues];
      		for (i = numX; i < xDim; i++) {
      	        tmp[i-numX] = y[i];
      		}
      		sum = 0.0;
			for (i = 0; i < numberValues; i++) {
				sum += tmp[i];
			}
			mean = sum/numberValues;
			sum = 0.0;
			for (i = 0; i < numberValues; i++) {
				diff = tmp[i] - mean;
				sum += diff*diff;
			}
			variance = sum/(numberValues - 1);
			std = Math.sqrt(variance);
			thresholdMultiplier = 3.0;
			thld = thresholdMultiplier*std;
				twoL = 1;
          	for (i = 0; i < numberOfLevels; i++) {
          		twoL = 2 * twoL;
          	}
      	    ix = xDim/twoL;
      	    ykeep = new double[xDim];
      	    for (i = 0; i < ix; i++) {
      	    	ykeep[i] = y[i];
      	    }
      	    for (i = 0; i < sliceSize; i++) {
			    	absVal = Math.abs(y[i]);
			    	if (absVal <= thld) {
			    		y[i] = 0.0;
			    	}
			    	else {
			    		y[i] = Math.signum(y[i])*(absVal - thld);
			    	}
			}
      	    for (i = 0; i < ix; i++) {
      	    	y[i] = ykeep[i];
      	    }
      	    midwt();
      	    for (i = 0; i < sliceSize; i++) {
              	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
      	    // aArray and signal_denoised_corr match
      	        /*aArray[0] = 0.06869260696580579
      			aArray[1] = 0.07062160451964727
      			aArray[2] = 0.07197690325297562
      			aArray[3] = 0.07435683051310576
      			aArray[4] = 0.07542519965346917
      			aArray[5] = 0.07635491038556112
      			aArray[6] = 0.07839727507444466
      			aArray[7] = 0.08070921364755644
      			aArray[8] = 0.07631099549980479
      			aArray[9] = 0.06930176836042048
      			aArray[10] = 0.06286975371913808
      			aArray[11] = 0.05474925316775597
      			aArray[12] = 0.07555194784015586
      			aArray[13] = 0.10793125604665628
      			aArray[14] = 0.08599597914648853
      			aArray[15] = 0.04943761183392213
      			aArray[16] = 0.06020593645954456
      			aArray[17] = 0.07850772297383835
      			aArray[18] = 0.07919996068422662
      			aArray[19] = 0.08094106057775195
      			aArray[20] = 0.08446521845489209
      			aArray[21] = 0.08737490848819247
      			aArray[22] = 0.09115352780857333
      			aArray[23] = 0.09520273329512771
      			aArray[24] = 0.09363160164684284
      			aArray[25] = 0.0898878427420566
      			aArray[26] = 0.08667341859170458
      			aArray[27] = 0.08207096857449239
      			aArray[28] = 0.07934814323230782
      			aArray[29] = 0.07683069652692404
      			aArray[30] = 0.07279957277923926
      			aArray[31] = 0.06841965915660457*/
      	    //signal_denoised_corr = [0.0686926069658060 0.0706216045196474 0.0719769032529757 0.0743568305131058 0.0754251996534692
      	    //0.0763549103855611 0.0783972750744446 0.0807092136475563 0.0763109954998047 0.0693017683604205 0.0628697537191382
      	    //0.0547492531677562 0.0755519478401559 0.107931256046656 0.0859959791464885 0.0494376118339224 0.0602059364595448
      	    //0.0785077229738383 0.0791999606842265 0.0809410605777517 0.0844652184548917 0.0873749084881920 0.0911535278085727
      	    //0.0952027332951270 0.0936316016468421 0.0898878427420561 0.0866734185917041 0.0820709685744921 0.0793481432323076
      	    //0.0768306965269240 0.0727995727792393 0.0684196591566048];
      	    setCompleted(true);
      	    return;
        } // else if (test_denoise_std)
        else if (test_denoise_hard) {
      	  //signal = makesig('Doppler', 32); // sets aArray
      	  makeSig("Doppler",32);
      	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
      		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
      		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
      		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
      		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
      	  //with_noise = signal + noise / 10;
      	  for (i = 0; i < 32; i++) {
      		  aArray[i] += noise[i]/10.0;
      	  }
      	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
      	    i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            numberOfLevels = j;
	            maximumLevel = numberOfLevels;
      	    filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
      	  //h = daubcqf(6)
      	  //[signal_denoised, subtracted_noise, actual_options] = denoise(with_noise, h);
	            mdwt();
	            numX = xDim - (int)Math.floor(xDim/2);
	            numberValues = numX;
	            tmp = new double[numberValues];
      		for (i = numX; i < xDim; i++) {
      	        tmp[i-numX] = y[i];
      		}
      		for (i = 0; i < numberValues; i++) {
					tmp[i] = Math.abs(tmp[i]);
				}
				Arrays.sort(tmp);
				if ((numberValues %2) == 0) {
				    median = (tmp[numberValues/2] + tmp[numberValues/2 - 1])/2.0; 
				}
				else {
					median = tmp[(numberValues - 1)/2];
				}
				thresholdMultiplier = 3.0;
				thld = thresholdMultiplier*median/.67;
				twoL = 1;
          	for (i = 0; i < numberOfLevels; i++) {
          		twoL = 2 * twoL;
          	}
      	    ix = xDim/twoL;
      	    ykeep = new double[xDim];
      	    for (i = 0; i < ix; i++) {
      	    	ykeep[i] = y[i];
      	    }
      	    for (i = 0; i < sliceSize; i++) {
			    	absVal = Math.abs(y[i]);
			    	if (absVal <= thld) {
			    		y[i] = 0.0;
			    	}
			}
      	    for (i = 0; i < ix; i++) {
      	    	y[i] = ykeep[i];
      	    }
      	    midwt();
      	    for (i = 0; i < sliceSize; i++) {
              	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
      	    // aArray and signal_denoised_corr match
      	        /*aArray[0] = 0.09773941601037227
      			aArray[1] = 0.09941615609833866
      			aArray[2] = 0.08324474078073844
      			aArray[3] = 0.06669833116971875
      			aArray[4] = 0.1774209715954132
      			aArray[5] = 0.34023058389711025
      			aArray[6] = -0.35459706967129556
      			aArray[7] = 0.025001787227501595
      			aArray[8] = 0.39441848534323853
      			aArray[9] = -0.059574530437451215
      			aArray[10] = -0.45240157079339977
      			aArray[11] = -0.17570756085210207
      			aArray[12] = -0.006223203251308512
      			aArray[13] = 0.4378670654118161
      			aArray[14] = 0.18748534658430582
      			aArray[15] = -0.24106066468704962
      			aArray[16] = -0.30628589612077417
      			aArray[17] = -0.37394653646637077
      			aArray[18] = -0.2461659244756577
      			aArray[19] = 0.00210496326791014
      			aArray[20] = 0.052862996606481714
      			aArray[21] = 0.09673836569533485
      			aArray[22] = 0.2754106936174399
      			aArray[23] = 0.4872989261699707
      			aArray[24] = 0.45498525371869025
      			aArray[25] = 0.34860333139363237
      			aArray[26] = 0.28820574394224924
      			aArray[27] = 0.1868065964962613
      			aArray[28] = 0.17214726040566036
      			aArray[29] = 0.18005085171468163
      			aArray[30] = 0.14213644582628845
      			aArray[31] = 0.10448472540148152*/
      	    //signal_denoised_corr = [0.0977394160103721 0.0994161560983385 0.0832447407807381 0.0666983311697188 0.177420971595413
      	    //0.340230583897110 -0.354597069671295 0.0250017872275015 0.394418485343238 -0.0595745304374512 -0.452401570793399
      	    //-0.175707560852101 -0.00622320325130765 0.437867065411816 0.187485346584306 -0.241060664687049 -0.306285896120773
      	    //-0.373946536466370 -0.246165924475657 0.00210496326791051 0.0528629966064817 0.0967383656953347 0.275410693617439
      	    //0.487298926169970 0.454985253718689 0.348603331393631 0.288205743942248 0.186806596496260 0.172147260405660 0.180050851714681
      	    //0.142136445826288 0.104484725401481];
      	    setCompleted(true);
      	    return;
        } // else if (test_denoise_hard)
        else if (test_denoise_levels) {
      	  //signal = makesig('Doppler', 32); // sets aArray
      	  makeSig("Doppler",32);
      	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
      		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
      		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
      		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
      		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
      	  //with_noise = signal + noise / 10;
      	  for (i = 0; i < 32; i++) {
      		  aArray[i] += noise[i]/10.0;
      	  }
      	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
	            numberOfLevels = 4;
	            maximumLevel = numberOfLevels;
      	    filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
      	  //h = daubcqf(6)
      	  //[signal_denoised, subtracted_noise, actual_options] = denoise(with_noise, h);
	            mdwt();
	            numX = xDim - (int)Math.floor(xDim/2);
	            numberValues = numX;
	            tmp = new double[numberValues];
      		for (i = numX; i < xDim; i++) {
      	        tmp[i-numX] = y[i];
      		}
      		for (i = 0; i < numberValues; i++) {
					tmp[i] = Math.abs(tmp[i]);
				}
				Arrays.sort(tmp);
				if ((numberValues %2) == 0) {
				    median = (tmp[numberValues/2] + tmp[numberValues/2 - 1])/2.0; 
				}
				else {
					median = tmp[(numberValues - 1)/2];
				}
				thresholdMultiplier = 3.0;
				thld = thresholdMultiplier*median/.67;
				twoL = 1;
          	for (i = 0; i < numberOfLevels; i++) {
          		twoL = 2 * twoL;
          	}
      	    ix = xDim/twoL;
      	    ykeep = new double[xDim];
      	    for (i = 0; i < ix; i++) {
      	    	ykeep[i] = y[i];
      	    }
      	    for (i = 0; i < sliceSize; i++) {
			    	absVal = Math.abs(y[i]);
			    	if (absVal <= thld) {
			    		y[i] = 0.0;
			    	}
			    	else {
			    		y[i] = Math.signum(y[i])*(absVal - thld);
			    	}
			}
      	    for (i = 0; i < ix; i++) {
      	    	y[i] = ykeep[i];
      	    }
      	    midwt();
      	    for (i = 0; i < sliceSize; i++) {
              	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
      	    // aArray and signal_dnoise_corr match
      	        /*aArray[0] = 0.1642599928172618
      			aArray[1] = 0.1563790712187119
      			aArray[2] = 0.14221268567170295
      			aArray[3] = 0.12503896357376165
      			aArray[4] = 0.15029781525207322
      			aArray[5] = 0.19153676797863603
      			aArray[6] = -0.03816395807657292
      			aArray[7] = 0.08810920321920991
      			aArray[8] = 0.11962928445848599
      			aArray[9] = -0.040609072536549225
      			aArray[10] = -0.1056454267314928
      			aArray[11] = -0.14182083199460246
      			aArray[12] = -0.028031897720270954
      			aArray[13] = 0.17317196012983155
      			aArray[14] = 0.011753743728243699
      			aArray[15] = -0.24711572995729425
      			aArray[16] = -0.2067592972859119
      			aArray[17] = -0.12314786604236322
      			aArray[18] = -0.06858082454225228
      			aArray[19] = 0.025582636014139976
      			aArray[20] = 0.06353029303970834
      			aArray[21] = 0.0930381970490926
      			aArray[22] = 0.16572808446313997
      			aArray[23] = 0.24688414715761528
      			aArray[24] = 0.24660321134558214
      			aArray[25] = 0.22021093493400362
      			aArray[26] = 0.20643699172308932
      			aArray[27] = 0.17717267554821084
      			aArray[28] = 0.1789489974332753
      			aArray[29] = 0.18801017789275004
      			aArray[30] = 0.1797981281810655
      			aArray[31] = 0.17093702367694485*/
      	    //signal_denoised_corr = [0.164259992817262 0.156379071218712 0.142212685671703 0.125038963573761 0.150297815252073
      	    //0.191536767978636 -0.0381639580765735 0.0881092032192094 0.119629284458486 -0.0406090725365491 -0.105645426731493
      	    //-0.141820831994602 -0.0280318977202704 0.173171960129832 0.0117537437282443 -0.247115729957293 -0.206759297285911 
      	    //-0.123147866042363 -0.0685808245422524 0.0255826360141400 0.0635302930397082 0.0930381970490923 0.165728084463140
      	    //0.246884147157615 0.246603211345582 0.220210934934003 0.206436991723089 0.177172675548210 0.178948997433275 0.188010177892750
      	    //0.179798128181065 0.170937023676945];
      	    setCompleted(true);
      	    return;
        } // else if (test_denoise_levels)
        else if (test_denoise_actual_thresh) {
      	  //signal = makesig('Doppler', 32); // sets aArray
      	  makeSig("Doppler",32);
      	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
      		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
      		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
      		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
      		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
      	  //with_noise = signal + noise / 10;
      	  for (i = 0; i < 32; i++) {
      		  aArray[i] += noise[i]/10.0;
      	  }
      	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
      	    i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            numberOfLevels = j;
	            maximumLevel = numberOfLevels;
      	    filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
      	  //h = daubcqf(6)
      	  //[signal_denoised, subtracted_noise, actual_options] = denoise(with_noise, h);
	            mdwt();
	            actualThreshold = 0.5;
				thld = actualThreshold;
				twoL = 1;
          	for (i = 0; i < numberOfLevels; i++) {
          		twoL = 2 * twoL;
          	}
      	    ix = xDim/twoL;
      	    ykeep = new double[xDim];
      	    for (i = 0; i < ix; i++) {
      	    	ykeep[i] = y[i];
      	    }
      	    for (i = 0; i < sliceSize; i++) {
			    	absVal = Math.abs(y[i]);
			    	if (absVal <= thld) {
			    		y[i] = 0.0;
			    	}
			    	else {
			    		y[i] = Math.signum(y[i])*(absVal - thld);
			    	}
			}
      	    for (i = 0; i < ix; i++) {
      	    	y[i] = ykeep[i];
      	    }
      	    midwt();
      	    for (i = 0; i < sliceSize; i++) {
              	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
            }
      	    // aArray and signal_denoised_corr match
      	        /*aArray[0] = 0.06070991839422943
      			aArray[1] = 0.06543515211935236
      			aArray[2] = 0.06841547598006095
      			aArray[3] = 0.07420189341484545
      			aArray[4] = 0.07588450053900132
      			aArray[5] = 0.07695115306431106
      			aArray[6] = 0.08108566067302526
      			aArray[7] = 0.08580233753160371
      			aArray[8] = 0.07047064433505186
      			aArray[9] = 0.047206090604758597
      			aArray[10] = 0.025432967951844247
      			aArray[11] = -0.0015459094040531866
      			aArray[12] = 0.05984551825793486
      			aArray[13] = 0.15655670784187767
      			aArray[14] = 0.0864272987162391
      			aArray[15] = -0.028783533528049386
      			aArray[16] = 0.006060171201546553
      			aArray[17] = 0.06595925754329313
      			aArray[18] = 0.07139580804955854
      			aArray[19] = 0.08128917350764933
      			aArray[20] = 0.09537019813471814
      			aArray[21] = 0.10755457679123927
      			aArray[22] = 0.12373914689559246
      			aArray[23] = 0.14118042264072728
      			aArray[24] = 0.13708504462260152
      			aArray[25] = 0.12483836676008718
      			aArray[26] = 0.11485295743723353
      			aArray[27] = 0.09972940005717923
      			aArray[28] = 0.0922174665178413
      			aArray[29] = 0.08577589765576875
      			aArray[30] = 0.07370526310313429
      			aArray[31] = 0.06054705420902285*/
      	    //signal_denoised_corr = [0.0607099183942295 0.0654351521193524 0.0684154759800610 0.0742018934148454 0.0758845005390013
      	    //0.0769511530643110 0.0810856606730252 0.0858023375316036 0.0704706443350518 0.0472060906047587 0.0254329679518446
      	    //-0.00154590940405266 0.0598455182579352 0.156556707841878 0.0864272987162393 -0.0287835335280487 0.00606017120154721
      	    //0.0659592575432934 0.0713958080495586 0.0812891735076492 0.0953701981347179 0.107554576791239 0.123739146895592 0.141180422640726
      	    //0.137085044622601 0.124838366760086 0.114852957437233 0.0997294000571788 0.0922174665178409 0.0857758976557685 0.0737052631031342
      	    //0.0605470542090229];
      	    setCompleted(true);
      	    return;
        } // else if (test_denoise_actual_thresh)
        else if (test_denoise_udwt_threshold_low) {
        	//signal = makesig('Doppler', 32); // sets aArray
      	  makeSig("Doppler",32);
      	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
      		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
      		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
      		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
      		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
      	  //with_noise = signal + noise / 10;
      	  for (i = 0; i < 32; i++) {
      		  aArray[i] += noise[i]/10.0;
      	  }
      	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
      	    i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            numberOfLevels = j;
	            maximumLevel = numberOfLevels;
      	        filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
      	        //h = daubcqf(6)
	            // Create low pass wavelet component
	            yl = new double[sliceSize];
	            lhA = new double[numberOfLevels][sliceSize];
	            
	            waveletImage = null;
	            destImage = null;
	            mrdwt();
            	srchA = lhA;
            	tmp = new double[sliceSize];
    		    for (i = 0; i < sliceSize; i++) {
    				tmp[i] = srchA[0][i];
    			}
    			for (i = 0; i < sliceSize; i++) {
					tmp[i] = Math.abs(tmp[i]);
				}
				Arrays.sort(tmp);
				if((sliceSize %2) == 0) {
				    median = (tmp[sliceSize/2] + tmp[sliceSize/2 - 1])/2.0; 
				}
				else {
					median = tmp[(sliceSize - 1)/2];
				}
				thresholdMultiplier = 3.0;
				thld = thresholdMultiplier*median/.67;
				for (i = minimumLevel-1; i < maximumLevel; i++) {
    				for (j = 0; j < sliceSize; j++) {
    					absVal = Math.abs(srchA[i][j]);
    					if (absVal <= thld) {
    						srchA[i][j] = 0.0;
    					}
    				}
    			}
				thresholdLowPass = true;
				for (i = 0; i < sliceSize; i++) {
			    	absVal = Math.abs(yl[i]);
			    	if (absVal <= thld) {
			    		yl[i] = 0.0;
			    	}
			    }
	            mirdwt();
	            for (i = 0; i < sliceSize; i++) {
	            	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
	            }
	            // aArray and signal_denoised_corr match
	            /*aArray[0] = 0.1350394004837414
        		aArray[1] = 0.11780517560460974
        		aArray[2] = 0.09677095841770364
        		aArray[3] = 0.014206029256730736
        		aArray[4] = -0.02398402946038142
        		aArray[5] = 0.32342586133169693
        		aArray[6] = -0.21228520012564356
        		aArray[7] = 0.16606665768573103
        		aArray[8] = 0.1366537398217854
        		aArray[9] = -0.03617082856552898
        		aArray[10] = -0.2446222173193135
        		aArray[11] = -0.07514861123448237
        		aArray[12] = 0.2791289971966281
        		aArray[13] = 0.29991529467282135
        		aArray[14] = 0.008223890772394035
        		aArray[15] = -0.23218077049924488
        		aArray[16] = -0.3301372633352007
        		aArray[17] = -0.29395531820617316
        		aArray[18] = -0.17553892638083599
        		aArray[19] = -0.07335686775435407
        		aArray[20] = 0.049241196655250785
        		aArray[21] = 0.20016589949069416
        		aArray[22] = 0.30461565061026347
        		aArray[23] = 0.33732537637811716
        		aArray[24] = 0.3255939843108077
        		aArray[25] = 0.2820489561509328
        		aArray[26] = 0.2288610818705466
        		aArray[27] = 0.19665688084214947
        		aArray[28] = 0.18095936648614186
        		aArray[29] = 0.17521041002240617
        		aArray[30] = 0.16982805022973618
        		aArray[31] = 0.1550332562094973*/
	            //signal_denoised_corr = [0.135039400483741 0.117805175604609 0.0967709584177031 0.0142060292567307 -0.0239840294603812
	            //0.323425861331697 -0.212285200125643 0.166066657685731 0.136653739821785 -0.0361708285655289 -0.244622217319313
	            //-0.0751486112344819 0.279128997196628 0.299915294672821 0.00822389077239383 -0.232180770499244 -0.330137263335199
	            //-0.293955318206172 -0.175538926380835 -0.0733568677543535 0.049241196655251 0.200165899490694 0.304615650610263
	            //0.337325376378116 0.325593984310807 0.282048956150932 0.228861081870546 0.196656880842149 0.180959366486141 0.175210410022406
	            //0.169828050229736 0.155033256209497];
	            setCompleted(true);
	            return;
        } // else if (test_denoise_udwt_threshold_low)
        else if (test_denoise_udwt_thresh_multiplier) {
        	//signal = makesig('Doppler', 32); // sets aArray
      	  makeSig("Doppler",32);
      	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
      		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
      		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
      		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
      		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
      	  //with_noise = signal + noise / 10;
      	  for (i = 0; i < 32; i++) {
      		  aArray[i] += noise[i]/10.0;
      	  }
      	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
      	    i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            numberOfLevels = j;
	            maximumLevel = numberOfLevels;
      	        filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
      	        //h = daubcqf(6)
	            // Create low pass wavelet component
	            yl = new double[sliceSize];
	            lhA = new double[numberOfLevels][sliceSize];
	            
	            waveletImage = null;
	            destImage = null;
	            mrdwt();
            	srchA = lhA;
            	tmp = new double[sliceSize];
    		    for (i = 0; i < sliceSize; i++) {
    				tmp[i] = srchA[0][i];
    			}
    			for (i = 0; i < sliceSize; i++) {
					tmp[i] = Math.abs(tmp[i]);
				}
				Arrays.sort(tmp);
				if((sliceSize %2) == 0) {
				    median = (tmp[sliceSize/2] + tmp[sliceSize/2 - 1])/2.0; 
				}
				else {
					median = tmp[(sliceSize - 1)/2];
				}
				thresholdMultiplier = 3.5;
				thld = thresholdMultiplier*median/.67;
				for (i = minimumLevel-1; i < maximumLevel; i++) {
    				for (j = 0; j < sliceSize; j++) {
    					absVal = Math.abs(srchA[i][j]);
    					if (absVal <= thld) {
    						srchA[i][j] = 0.0;
    					}
    				}
    			}
				thresholdLowPass = true;
				for (i = 0; i < sliceSize; i++) {
			    	absVal = Math.abs(yl[i]);
			    	if (absVal <= thld) {
			    		yl[i] = 0.0;
			    	}
			    }
	            mirdwt();
	            for (i = 0; i < sliceSize; i++) {
	            	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
	            }
	            // aArray and signal_denoised_corr match
	            /*aArray[0] = 0.04794785068666102
        		aArray[1] = 0.01606530463050457
        		aArray[2] = -0.012660890293451885
        		aArray[3] = -0.0292521383561941
        		aArray[4] = -0.038335504375122634
        		aArray[5] = -0.023949480210921713
        		aArray[6] = 0.0020004253652661647
        		aArray[7] = 0.013563661000390208
        		aArray[8] = 0.003996370411957276
        		aArray[9] = -0.10052137850094398
        		aArray[10] = -0.22992352496550128
        		aArray[11] = -0.10261422557659282
        		aArray[12] = 0.19585059627072474
        		aArray[13] = 0.19759341333610245
        		aArray[14] = -0.10088240677529345
        		aArray[15] = -0.2911636301192516
        		aArray[16] = -0.318524834100707
        		aArray[17] = -0.3247528873202357
        		aArray[18] = -0.28891621887424374
        		aArray[19] = -0.17665853091385847
        		aArray[20] = -0.02853659232675936
        		aArray[21] = 0.1084098165726487
        		aArray[22] = 0.20406370201706114
        		aArray[23] = 0.23917024855676983
        		aArray[24] = 0.23010869068477868
        		aArray[25] = 0.1901193941844443
        		aArray[26] = 0.14091827822899086
        		aArray[27] = 0.11174543739754021
        		aArray[28] = 0.09913010327678085
        		aArray[29] = 0.0977198505254532
        		aArray[30] = 0.09376395476885865
        		aArray[31] = 0.0745251447941451*/
	            //signal_denoised_corr = [0.0479478506866607 0.0160653046305043 -0.012660890293452 -0.0292521383561941 -0.0383355043751224
	            //-0.0239494802109215 0.00200042536526626 0.0135636610003902 0.00399637041195728 -0.100521378500944 -0.229923524965501
	            //-0.102614225576592 0.195850596270724 0.197593413336102 -0.100882406775293 -0.291163630119251 -0.318524834100706 -0.324752887320235
	            //-0.288916218874243 -0.176658530913858 -0.028536592326759 0.108409816572649 0.204063702017061 0.239170248556769 0.230108690684778
	            //0.190119394184444 0.14091827822899 0.11174543739754 0.0991301032767805 0.0977198505254529 0.0937639547688583 0.0745251447941448];
	            setCompleted(true);
	            return;
        } // else if (test_denoise_udwt_thresh_multiplier)
        else if (test_denoise_udwt_std) {
        	//signal = makesig('Doppler', 32); // sets aArray
      	  makeSig("Doppler",32);
      	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
      		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
      		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
      		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
      		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
      	  //with_noise = signal + noise / 10;
      	  for (i = 0; i < 32; i++) {
      		  aArray[i] += noise[i]/10.0;
      	  }
      	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
      	    i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            numberOfLevels = j;
	            maximumLevel = numberOfLevels;
      	        filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
      	        //h = daubcqf(6)
	            // Create low pass wavelet component
	            yl = new double[sliceSize];
	            lhA = new double[numberOfLevels][sliceSize];
	            
	            waveletImage = null;
	            destImage = null;
	            mrdwt();
            	srchA = lhA;
            	tmp = new double[sliceSize];
    		    for (i = 0; i < sliceSize; i++) {
    				tmp[i] = srchA[0][i];
    			}
				sum = 0.0;
				for (i = 0; i < sliceSize; i++) {
					sum += tmp[i];
				}
				mean = sum/sliceSize;
				sum = 0.0;
				for (i = 0; i < sliceSize; i++) {
					diff = tmp[i] - mean;
					sum += diff*diff;
				}
				variance = sum/(sliceSize - 1);
				std = Math.sqrt(variance);
				thresholdMultiplier = 3.0;
				thld = thresholdMultiplier*std;
				for (i = minimumLevel-1; i < maximumLevel; i++) {
    				for (j = 0; j < sliceSize; j++) {
    					absVal = Math.abs(srchA[i][j]);
    					if (absVal <= thld) {
    						srchA[i][j] = 0.0;
    					}
    				}
    			}
	            mirdwt();
	            for (i = 0; i < sliceSize; i++) {
	            	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
	            }
	            // aArray and signal_denoised_corr match
	            /*aArray[0] = 0.0847626939447049
        		aArray[1] = 0.06486693754888792
        		aArray[2] = 0.050512704899884256
        		aArray[3] = 0.04314776906689656
        		aArray[4] = 0.04434589950916612
        		aArray[5] = 0.0638361516754723
        		aArray[6] = 0.09266982000654436
        		aArray[7] = 0.12271635749675139
        		aArray[8] = 0.13559168386401943
        		aArray[9] = 0.037746675302719086
        		aArray[10] = -0.08891665868972289
        		aArray[11] = -0.031070001694325937
        		aArray[12] = 0.16530654803758976
        		aArray[13] = 0.23734985816958526
        		aArray[14] = 0.05776920514974439
        		aArray[15] = -0.137751577705709
        		aArray[16] = -0.18354744395111114
        		aArray[17] = -0.18820542754033556
        		aArray[18] = -0.15790285748042185
        		aArray[19] = -0.05539132357693756
        		aArray[20] = 0.07918923984603016
        		aArray[21] = 0.19806818599737266
        		aArray[22] = 0.27147142283611253
        		aArray[23] = 0.28227588681522875
        		aArray[24] = 0.24668929363091704
        		aArray[25] = 0.2055467054965888
        		aArray[26] = 0.16546007731141044
        		aArray[27] = 0.1451308983829681
        		aArray[28] = 0.14713296360380007
        		aArray[29] = 0.14247274982306563
        		aArray[30] = 0.1321634482909461
        		aArray[31] = 0.11195819555138528*/
	            //signal_denoised_corr = [0.0847626939447046 0.0648669375488877 0.0505127048998841 0.0431477690668965 0.0443458995091662
	            //0.0638361516754724 0.0926698200065443 0.122716357496751 0.135591683864019 0.0377466753027189 -0.0889166586897228 -0.0310700016943258
	            //0.16530654803759 0.237349858169585 0.0577692051497442 -0.137751577705709 -0.18354744395111 -0.188205427540335 -0.157902857480421
	            //-0.055391323576937 0.0791892398460303 0.198068185997372 0.271471422836112 0.282275886815228 0.246689293630916 0.205546705496588
	            //0.16546007731141 0.145130898382968 0.1471329636038 0.142472749823065 0.132163448290946 0.111958195551385];
	            setCompleted(true);
	            return;
        } // else if (test_denoise_udwt_std)
        else if (test_denoise_udwt_soft) {
        	//signal = makesig('Doppler', 32); // sets aArray
      	  makeSig("Doppler",32);
      	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
      		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
      		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
      		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
      		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
      	  //with_noise = signal + noise / 10;
      	  for (i = 0; i < 32; i++) {
      		  aArray[i] += noise[i]/10.0;
      	  }
      	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
      	    i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            numberOfLevels = j;
	            maximumLevel = numberOfLevels;
      	        filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
      	        //h = daubcqf(6)
	            // Create low pass wavelet component
	            yl = new double[sliceSize];
	            lhA = new double[numberOfLevels][sliceSize];
	            
	            waveletImage = null;
	            destImage = null;
	            mrdwt();
            	srchA = lhA;
            	tmp = new double[sliceSize];
    		    for (i = 0; i < sliceSize; i++) {
    				tmp[i] = srchA[0][i];
    			}
    			for (i = 0; i < sliceSize; i++) {
					tmp[i] = Math.abs(tmp[i]);
				}
				Arrays.sort(tmp);
				if((sliceSize %2) == 0) {
				    median = (tmp[sliceSize/2] + tmp[sliceSize/2 - 1])/2.0; 
				}
				else {
					median = tmp[(sliceSize - 1)/2];
				}
				thresholdMultiplier = 3.0;
				thld = thresholdMultiplier*median/.67;
				for (i = minimumLevel-1; i < maximumLevel; i++) {
    				for (j = 0; j < sliceSize; j++) {
    					absVal = Math.abs(srchA[i][j]);
    					if (absVal <= thld) {
    						srchA[i][j] = 0.0;
    					}
    					else {
    						srchA[i][j] = Math.signum(srchA[i][j])*(absVal - thld);
    					}
    				}
    			}
	            mirdwt();
	            for (i = 0; i < sliceSize; i++) {
	            	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
	            }
	            // aArray and signal_denoised_corr match
	            /*aArray[0] = 0.08666801674942808
        		aArray[1] = 0.07809065263227778
        		aArray[2] = 0.07045584274954446
        		aArray[3] = 0.06282468420568438
        		aArray[4] = 0.06424979553464172
        		aArray[5] = 0.0868999243186416
        		aArray[6] = 0.053549539548214106
        		aArray[7] = 0.10064417536630808
        		aArray[8] = 0.10072656003745818
        		aArray[9] = 0.05147940604621438
        		aArray[10] = -0.01129994521110453
        		aArray[11] = 0.03611539471096058
        		aArray[12] = 0.14762499854761213
        		aArray[13] = 0.15951630876696005
        		aArray[14] = 0.0591190626825689
        		aArray[15] = -0.020817294484415855
        		aArray[16] = -0.04217091241303858
        		aArray[17] = -0.04682516829882306
        		aArray[18] = -0.02717928582782492
        		aArray[19] = 0.017379645805456843
        		aArray[20] = 0.07122512601147638
        		aArray[21] = 0.12353278023847036
        		aArray[22] = 0.1539260342412197
        		aArray[23] = 0.16013875504969993
        		aArray[24] = 0.15356216865833675
        		aArray[25] = 0.13874801944060014
        		aArray[26] = 0.12370780535236198
        		aArray[27] = 0.11522342561260708
        		aArray[28] = 0.11089087735538132
        		aArray[29] = 0.10790964897344332
        		aArray[30] = 0.10363095423818101
        		aArray[31] = 0.09584908498068499*/
	            //signal_denoised_corr = [0.086668016749428   0.078090652632278   0.070455842749544   0.062824684205684  0.064249795534642 
	            //0.086899924318641   0.053549539548214   0.100644175366308  0.100726560037458   0.051479406046214  -0.011299945211104 
	            //0.036115394710961  0.147624998547612   0.159516308766960   0.059119062682569  -0.020817294484415 -0.042170912413038 
	            //-0.046825168298822  -0.027179285827824   0.017379645805457  0.071225126011476   0.123532780238470   0.153926034241219 
	            //0.160138755049699  0.153562168658336   0.138748019440599   0.123707805352361   0.115223425612607  0.110890877355381
	            //0.107909648973443   0.103630954238181   0.095849084980685];
	            setCompleted(true);
	            return;
        } // else if (test_denoise_udwt_soft)
        else if (test_denoise_udwt_levels) {
        	//signal = makesig('Doppler', 32); // sets aArray
      	  makeSig("Doppler",32);
      	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
      		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
      		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
      		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
      		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
      	  //with_noise = signal + noise / 10;
      	  for (i = 0; i < 32; i++) {
      		  aArray[i] += noise[i]/10.0;
      	  }
      	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
	            numberOfLevels = 4;
	            maximumLevel = numberOfLevels;
      	        filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
      	        //h = daubcqf(6)
	            // Create low pass wavelet component
	            yl = new double[sliceSize];
	            lhA = new double[numberOfLevels][sliceSize];
	            
	            waveletImage = null;
	            destImage = null;
	            mrdwt();
            	srchA = lhA;
            	tmp = new double[sliceSize];
    		    for (i = 0; i < sliceSize; i++) {
    				tmp[i] = srchA[0][i];
    			}
    			for (i = 0; i < sliceSize; i++) {
					tmp[i] = Math.abs(tmp[i]);
				}
				Arrays.sort(tmp);
				if((sliceSize %2) == 0) {
				    median = (tmp[sliceSize/2] + tmp[sliceSize/2 - 1])/2.0; 
				}
				else {
					median = tmp[(sliceSize - 1)/2];
				}
				thresholdMultiplier = 3.0;
				thld = thresholdMultiplier*median/.67;
				for (i = minimumLevel-1; i < maximumLevel; i++) {
    				for (j = 0; j < sliceSize; j++) {
    					absVal = Math.abs(srchA[i][j]);
    					if (absVal <= thld) {
    						srchA[i][j] = 0.0;
    					}
    				}
    			}
	            mirdwt();
	            for (i = 0; i < sliceSize; i++) {
	            	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
	            }
	            // aArray and signal_denoised_corr match
	            /*aArray[0] = 0.137633389000662
        		aArray[1] = 0.12067680414732689
        		aArray[2] = 0.09978275821514367
        		aArray[3] = 0.01569857402026693
        		aArray[4] = -0.025118098815378794
        		aArray[5] = 0.3197883319915217
        		aArray[6] = -0.21791921767008973
        		aArray[7] = 0.16023820177375642
        		aArray[8] = 0.13127034042953442
        		aArray[9] = -0.041415802797292414
        		aArray[10] = -0.2498536103806945
        		aArray[11] = -0.08012674088377889
        		aArray[12] = 0.2750343359853379
        		aArray[13] = 0.2969828314002656
        		aArray[14] = 0.006200146572810622
        		aArray[15] = -0.23430964793484604
        		aArray[16] = -0.3327312518521213
        		aArray[17] = -0.2968269467488904
        		aArray[18] = -0.17855072617827605
        		aArray[19] = -0.07484941251789026
        		aArray[20] = 0.05037526601024815
        		aArray[21] = 0.2038034288308693
        		aArray[22] = 0.31024966815470967
        		aArray[23] = 0.3431538322900918
        		aArray[24] = 0.33097738370305874
        		aArray[25] = 0.28729393038269624
        		aArray[26] = 0.23409247493192759
        		aArray[27] = 0.20163501049144594
        		aArray[28] = 0.18505402769743196
        		aArray[29] = 0.17814287329496187
        		aArray[30] = 0.17185179442931958
        		aArray[31] = 0.15716213364509837*/
	            //signal_denoised_corr = [0.137633389000662   0.120676804147327   0.099782758215143   0.015698574020267 -0.025118098815379
	            //0.319788331991522  -0.217919217670089   0.160238201773756  0.131270340429534  -0.041415802797292  -0.249853610380694
	            //-0.080126740883778  0.275034335985338   0.296982831400265   0.006200146572810  -0.234309647934845 -0.332731251852120
	            //-0.296826946748889  -0.178550726178275  -0.074849412517890  0.050375266010248   0.203803428830869   0.310249668154709
	            //0.343153832290091  0.330977383703058   0.287293930382695   0.234092474931927   0.201635010491445  0.185054027697432 
	            //0.178142873294961   0.171851794429319   0.157162133645098];
	            setCompleted(true);
	            return;
        } // else if (test_denoise_udwt_levels)
        else if (test_denoise_udwt_actual_thresh) {
        	//signal = makesig('Doppler', 32); // sets aArray
      	  makeSig("Doppler",32);
      	  double noise[] = new double [] {1.54421189550395, 0.0859311331754255, -1.49159031063761, -0.742301837259857, -1.06158173331999, 2.35045722400204,
      		  -0.615601881466894, 0.748076783703985, -0.192418510588264, 0.888610425420721, -0.764849236567874, -1.40226896933876, -1.42237592509150,
      		  0.488193909859941, -0.177375156618825, -0.196053487807333, 1.41931015064255, 0.291584373984183, 0.197811053464361, 1.58769908997406,
      		  -0.804465956349547, 0.696624415849607, 0.835088165072682, -0.243715140377952, 0.215670086403744, -1.16584393148205, -1.14795277889859,
      		  0.104874716016494, 0.722254032225002, 2.58549125261624, -0.666890670701386, 0.187331024578940};
      	  //with_noise = signal + noise / 10;
      	  for (i = 0; i < 32; i++) {
      		  aArray[i] += noise[i]/10.0;
      	  }
      	  nDims = 1;
	            xDim = 32;
	            yDim = 1;
	            sliceSize = 32;
	            y = new double[sliceSize];
	            waveletImage = null;
      	    i = xDim;
	            j = 0;
	            while ((i % 2) == 0) {
	                i = (i >> 1);
	                j++;
	            }
	            numberOfLevels = j;
	            maximumLevel = numberOfLevels;
      	        filterType = MINIMUM_PHASE;
	            filterLength = 6;
	            scalingFilter = new double[filterLength];
	            waveletFilter = new double[filterLength];
	            daubcqf();
      	        //h = daubcqf(6)
	            // Create low pass wavelet component
	            yl = new double[sliceSize];
	            lhA = new double[numberOfLevels][sliceSize];
	            
	            waveletImage = null;
	            destImage = null;
	            mrdwt();
            	srchA = lhA;
				thresholdMultiplier = 3.0;
				actualThreshold = 0.5;
				thld = actualThreshold;
				for (i = minimumLevel-1; i < maximumLevel; i++) {
    				for (j = 0; j < sliceSize; j++) {
    					absVal = Math.abs(srchA[i][j]);
    					if (absVal <= thld) {
    						srchA[i][j] = 0.0;
    					}
    				}
    			}
	            mirdwt();
	            for (i = 0; i < sliceSize; i++) {
	            	Preferences.debug("aArray["+i+"] = " + aArray[i] + "\n", Preferences.DEBUG_ALGORITHM); 
	            }
	            // aArray and signal_denoised_corr match
	            /*aArray[0] = 0.1262446153851522
        		aArray[1] = 0.09523197124253038
        		aArray[2] = 0.06713436071525052
        		aArray[3] = 0.051390297972258595
        		aArray[4] = 0.04304027326826328
        		aArray[5] = 0.05869325751317936
        		aArray[6] = 0.08610697519026987
        		aArray[7] = 0.0989949047763018
        		aArray[8] = 0.09084186581286383
        		aArray[9] = -0.014145467011905916
        		aArray[10] = -0.1447915274370268
        		aArray[11] = -0.0185533166035904
        		aArray[12] = 0.27835161378213097
        		aArray[13] = 0.2790337063766599
        		aArray[14] = -0.020501203205426165
        		aArray[15] = -0.21236765840797703
        		aArray[16] = -0.24148434369799646
        		aArray[17] = -0.24858229883105987
        		aArray[18] = -0.2133742147817445
        		aArray[19] = -0.10196371214110948
        		aArray[20] = 0.045424885131056435
        		aArray[21] = 0.18110433394974934
        		aArray[22] = 0.27529440729325916
        		aArray[23] = 0.30907625988205994
        		aArray[24] = 0.2986004503850738
        		aArray[25] = 0.25908073779660795
        		aArray[26] = 0.2111235358017181
        		aArray[27] = 0.18302178352573945
        		aArray[28] = 0.17196634086657622
        		aArray[29] = 0.17161681258609737
        		aArray[30] = 0.16872000630019302
        		aArray[31] = 0.1510664281840722*/
	            //signal_denoised_corr = [0.126244615385152 0.09523197124253 0.0671343607152503 0.0513902979722585 0.0430402732682634 
	            //0.0586932575131794 0.0861069751902698 0.0989949047763016 0.0908418658128637 -0.0141454670119059 -0.144791527437026
	            //-0.0185533166035902 0.278351613782131 0.279033706376659 -0.0205012032054263 -0.212367658407976 -0.241484343697995
	            //-0.248582298831059 -0.213374214781743 -0.101963712141109 0.0454248851310567 0.181104333949749 0.275294407293258
	            //0.309076259882059 0.298600450385073 0.259080737796607 0.211123535801717 0.183021783525739 0.171966340866576 0.171616812586097 
	            //0.168720006300193 0.151066428184072];
	            setCompleted(true);
	            return;
        } // else if (test_denoise_udwt_actual_thresh)
        else {
            nDims = srcImage.getNDims();
            extents = srcImage.getExtents();
            xDim = extents[0];
            if ((xDim % 2) == 1) {
                MipavUtil.displayError("1 level of decomposition requires an even xDim");
                setCompleted(false);
                return;
            }
            yDim = extents[1];
            if ((yDim % 2) == 1) {
                MipavUtil.displayError("1 level of decomposition requires an even yDim");
                setCompleted(false);
                return;
            }
            sliceSize = xDim * yDim;
            
            if ((filterLength % 2) == 1) {
                displayError("No Daubechies filter exists for odd length");
                setCompleted(false);
                return;
            }
            
            if (numberOfLevels <= 0) {
                MipavUtil.displayError("A wavelet decomposition requires number of levels >= 1");
                setCompleted(false);
            }
            
            if (numberOfLevels < Integer.MAX_VALUE) {
                divisor = 1;
                for (i = 1; i <= numberOfLevels; i++) {
                    divisor *= 2;
                }
                if ((xDim % divisor) != 0) {
                    MipavUtil.displayError("Error!  xDim mod " + divisor + " does not equal 0");
                    setCompleted(false);
                    return;
                }
                
                if ((yDim % divisor) != 0) {
                    MipavUtil.displayError("Error!  yDim mod " + divisor + " does not equal 0");
                    setCompleted(false);
                    return;
                }
            } // if (numberOfLevels < Integer.MAX_VALUE);
            else { // Calculate maximum possible number of levels
                i = xDim;
                j = 0;
                while ((i % 2) == 0) {
                    i = (i >> 1);
                    j++;
                }
                k = yDim;
                i = 0;
                while((k % 2) == 0) {
                    k = (k >> 1);
                    i++;
                }
                
                numberOfLevels = Math.min(i, j);
                Preferences.debug("The maximum possible number of levels = " + numberOfLevels + "\n", Preferences.DEBUG_FILEIO);
            } // else calculate maximum possible number of levels
    
            if (nDims > 2) {
                zDim = extents[2];
            }
            else {
                zDim = 1;
            }
    
            try {
                aArray = new double[sliceSize];
            } catch (final OutOfMemoryError e) {
                aArray = null;
                System.gc();
                displayError("AlgorithmRiceWaveletTools: Out of memory creating a");
    
                setCompleted(false);
    
                return;
            }
    
        } // else
        
        scalingFilter = new double[filterLength];
        
        waveletFilter = new double[filterLength];
        
        daubcqf();
        Preferences.debug("Scaling filter:\n", Preferences.DEBUG_ALGORITHM);
        for (j = 0; j < filterLength; j++) {
            Preferences.debug(scalingFilter[j] + "\n", Preferences.DEBUG_ALGORITHM);
        }
        Preferences.debug("Wavelet filter:\n", Preferences.DEBUG_ALGORITHM);
        for (j = 0; j < filterLength; j++) {
            Preferences.debug(waveletFilter[j] + "\n", Preferences.DEBUG_ALGORITHM);
        }
        if (error == -1) {
            setCompleted(false);
            return;
        }
        
        if (redundant) {
	        // Create low pass wavelet component
	        yl = new double[sliceSize];
	        // Savethe low pass component for each level
	        llA = new double[numberOfLevels-1][sliceSize];
	        
	        // Create 3 high pass components for each level
	        lhA = new double[numberOfLevels][sliceSize];
	        hlA = new double[numberOfLevels][sliceSize];
	        hhA = new double[numberOfLevels][sliceSize];
	        if (doWaveletImages) {
	        	if (maximumLevel > minimumLevel) {
	                waveletImage = new ModelImage[4*numberOfLevels+4];
	        	}
	        	else {
	        		waveletImage = new ModelImage[4*numberOfLevels];
	        	}
	        }
	        else if (maximumLevel > minimumLevel) {
	            waveletImage = new ModelImage[4];
	        }
	        
	        for (z = 0; z < zDim; z++) {
	            if (zDim > 1) {
	                fireProgressStateChanged((z * 100)/(zDim - 1));
	            }
	            
	            try {
	                srcImage.exportData(z*sliceSize, sliceSize, aArray);
	            } catch (final IOException error) {
	                displayError("AlgorithmRiceWaveletTools: Source image is locked");
	    
	                setCompleted(false);
	    
	                return;
	            }
	        
	            mrdwt();
	            
	            if (doDenoise) {
	            	if (nDims == 1) {
	            		srchA = lhA;
	            	}
	            	else {
	            		srchA = hhA;
	            	}
            		if (actualThreshold == 0.0) {
            			tmp = new double[sliceSize];
            			for (i = 0; i < sliceSize; i++) {
            				tmp[i] = srchA[0][i];
            			}
            			if (varianceEstimator == MAD) {
            				for (i = 0; i < sliceSize; i++) {
            					tmp[i] = Math.abs(tmp[i]);
            				}
            				Arrays.sort(tmp);
            				if ((sliceSize % 2) == 0) {
            				    median = (tmp[sliceSize/2] + tmp[sliceSize/2 - 1])/2.0; 
            				}
            				else {
            					median = tmp[(sliceSize - 1)/2];
            				}
            				thld = thresholdMultiplier*median/.67;
            			}
            			else { // varianceEstimator == STD
            				sum = 0.0;
            				for (i = 0; i < sliceSize; i++) {
            					sum += tmp[i];
            				}
            				mean = sum/sliceSize;
            				sum = 0.0;
            				for (i = 0; i < sliceSize; i++) {
            					diff = tmp[i] - mean;
            					sum += diff*diff;
            				}
            				variance = sum/(sliceSize - 1);
            				std = Math.sqrt(variance);
            				thld = thresholdMultiplier*std;
            			}
            		} // if (actualThreshold == 0.0)
            		else {
            			thld = actualThreshold;
            		}
            		if (thresholdingType == SOFT_THRESHOLDING) {
            			for (i = minimumLevel-1; i < maximumLevel; i++) {
            				for (j = 0; j < sliceSize; j++) {
            					absVal = Math.abs(srchA[i][j]);
            					if (absVal <= thld) {
            						srchA[i][j] = 0.0;
            					}
            					else {
            						srchA[i][j] = Math.signum(srchA[i][j])*(absVal - thld);
            					}
            				}
            			}
            			if (nDims == 2) {
            				for (i = minimumLevel-1; i < maximumLevel; i++) {
                				for (j = 0; j < sliceSize; j++) {
                					absVal = Math.abs(lhA[i][j]);
                					if (absVal <= thld) {
                						lhA[i][j] = 0.0;
                					}
                					else {
                						lhA[i][j] = Math.signum(lhA[i][j])*(absVal - thld);
                					}
                				}
                			}	
            				
            				for (i = minimumLevel-1; i < maximumLevel; i++) {
                				for (j = 0; j < sliceSize; j++) {
                					absVal = Math.abs(hlA[i][j]);
                					if (absVal <= thld) {
                						hlA[i][j] = 0.0;
                					}
                					else {
                						hlA[i][j] = Math.signum(hlA[i][j])*(absVal - thld);
                					}
                				}
                			}
            			} // if (nDims == 2)
            			if (thresholdLowPass) {
            			    for (i = 0; i < sliceSize; i++) {
            			    	absVal = Math.abs(yl[i]);
            			    	if (absVal <= thld) {
            			    		yl[i] = 0.0;
            			    	}
            			    	else {
            			    		yl[i] = Math.signum(yl[i])*(absVal - thld);
            			    	}
            			    }
            			} // if (thresholdLowPass)
            		} // if (thresholdingType == SOFT_THRESHOLDING)
            		else { // thresholdingType == HARD_THRESHOLDING) {
            			for (i = minimumLevel-1; i < maximumLevel; i++) {
            				for (j = 0; j < sliceSize; j++) {
            					absVal = Math.abs(srchA[i][j]);
            					if (absVal <= thld) {
            						srchA[i][j] = 0.0;
            					}
            				}
            			}
            			if (nDims == 2) {
            				for (i = minimumLevel-1; i < maximumLevel; i++) {
                				for (j = 0; j < sliceSize; j++) {
                					absVal = Math.abs(lhA[i][j]);
                					if (absVal <= thld) {
                						lhA[i][j] = 0.0;
                					}
                				}
                			}	
            				
            				for (i = minimumLevel-1; i < maximumLevel; i++) {
                				for (j = 0; j < sliceSize; j++) {
                					absVal = Math.abs(hlA[i][j]);
                					if (absVal <= thld) {
                						hlA[i][j] = 0.0;
                					}
                				}
                			}
            			} // if (nDims == 2)
            			if (thresholdLowPass) {
            			    for (i = 0; i < sliceSize; i++) {
            			    	absVal = Math.abs(yl[i]);
            			    	if (absVal <= thld) {
            			    		yl[i] = 0.0;
            			    	}
            			    }
            			} // if (thresholdLowPass)
            		}
            		mirdwt();
            		
            		if (destImage != null) {
	            		try {
	                        destImage.importData(0, aArray, true);
	                    }
	                    catch(IOException e) {
	                        MipavUtil.displayError("IOException on destImage.importData(0, aArray, true)");
	                        setCompleted(false);
	                        return;
	                    }
            		} // if (destImage != null)
	            } // if (doDenoise)
	        }
        } // if (redundant)
        else { // !redundant
        	y = new double[sliceSize];
        	if (noiseStandardDeviation == null) {
        	    waveletImage = new ModelImage[1];
        	}
        	for (z = 0; z < zDim; z++) {
                if (zDim > 1) {
                    fireProgressStateChanged((z * 100)/(zDim - 1));
                }
                
                try {
                    srcImage.exportData(z*sliceSize, sliceSize, aArray);
                } catch (final IOException error) {
                    displayError("AlgorithmRiceWaveletTools: Source image is locked");
        
                    setCompleted(false);
        
                    return;
                }
            
                mdwt();
                
                if (doDenoise) {
                	if (actualThreshold == 0.0) {
                		numX = xDim - (int)Math.floor(xDim/2);
                		numY = yDim - (int)Math.floor(yDim/2);
                		numberValues = numX*numY;
                		tmp = new double[numberValues];
                		for (i = numY; i < yDim; i++) {
                			for (j = numX; j < xDim; j++) {
                			    tmp[(i-numY)*numX + j-numX] = y[i*xDim + j];
                			}
                		}
                		if (varianceEstimator == MAD) {
            				for (i = 0; i < numberValues; i++) {
            					tmp[i] = Math.abs(tmp[i]);
            				}
            				Arrays.sort(tmp);
            				if ((numberValues %2) == 0) {
            				    median = (tmp[numberValues/2] + tmp[numberValues/2 - 1])/2.0; 
            				}
            				else {
            					median = tmp[(numberValues - 1)/2];
            				}
            				if (noiseStandardDeviation != null) {
            				    noiseStandardDeviation[0] = median/.6745;
            				    setCompleted(true);
            				    return;
            				} // if (noiseStandardDeviation != null)
            				thld = thresholdMultiplier*median/.67;
            			}
            			else { // varianceEstimator == STD
            				sum = 0.0;
            				for (i = 0; i < numberValues; i++) {
            					sum += tmp[i];
            				}
            				mean = sum/numberValues;
            				sum = 0.0;
            				for (i = 0; i < numberValues; i++) {
            					diff = tmp[i] - mean;
            					sum += diff*diff;
            				}
            				variance = sum/(numberValues - 1);
            				std = Math.sqrt(variance);
            				thld = thresholdMultiplier*std;
            			}
                	} // if (actualThreshold == 0.0)
                	else {
            			thld = actualThreshold;
            		}
                	twoL = 1;
                	for (i = 0; i < numberOfLevels; i++) {
                		twoL = 2 * twoL;
                	}
                	jx = 0;
                	jxLow = 0;
                	jx2 = yDim;
                	if (nDims == 1) {
                		ixLow = xDim/twoL;
                	    ix = xDim/twoL;
                	    ix2 = xDim;
                	    if (minimumLevel > 1) {
	                	    for (i = 0; i < (minimumLevel -1); i++) {
	                	    	ix = ix * 2;
	                	    }
                	    } // if (minimumLevel > 1)
                	    ykeep = new double[xDim];
                	    for (i = 0; i < ix; i++) {
                	    	ykeep[i] = y[i];
                	    }
                	    if (maximumLevel < numberOfLevels) {
                	    	for (i = 0; i < (numberOfLevels - maximumLevel); i++) {
                	    		ix2 = ix2/2;
                	    	}
                	    }
                	    for (i = ix2; i < xDim; i++) {
                	    	ykeep[i] = y[i];
                	    }
                	} // if (nDims == 1)
                	else {
                		ixLow = yDim/twoL;
                	    jxLow = xDim/twoL;
                		ix = yDim/twoL;
                	    jx = xDim/twoL;
                	    ix2 = yDim;
                	    jx2 = xDim;
                	    ykeep = new double[sliceSize];
                	    if (minimumLevel > 1) {
	                	    for (i = 0; i < (minimumLevel -1); i++) {
	                	    	ix = ix * 2;
	                	    	jx = jx * 2;
	                	    }
                	    } // if (minimumLevel > 1)
                	    for (i = 0; i < ix; i++) {
                	    	for (j = 0; j < jx; j++) {
                	    		ykeep[i*xDim + j] = y[i*xDim + j];
                	    	}
                	    }
                	    if (maximumLevel < numberOfLevels) {
                	    	for (i = 0; i < (numberOfLevels - maximumLevel); i++) {
                	    		ix2 = ix2/2;
                	    		jx2 = jx2/2;
                	    	}
                	    }
                	    for (i = ix2; i < yDim; i++) {
                	    	for (j = jx2; j < xDim; j++) {
                	    		ykeep[i*xDim+j] = y[i*xDim + j];
                	    	}
                	    }
                	}
                	if (thresholdingType == SOFT_THRESHOLDING) {
                		for (i = 0; i < sliceSize; i++) {
        			    	absVal = Math.abs(y[i]);
        			    	if (absVal <= thld) {
        			    		y[i] = 0.0;
        			    	}
        			    	else {
        			    		y[i] = Math.signum(y[i])*(absVal - thld);
        			    	}
        			    }	
                	}
                	else { // thresholdingType == HARD_THRESHOLDING
                		for (i = 0; i < sliceSize; i++) {
        			    	absVal = Math.abs(y[i]);
        			    	if (absVal <= thld) {
        			    		y[i] = 0.0;
        			    	}
        			    }		
                	} // else threhsoldingType == HARD_THRESHOLDING
                	if (!thresholdLowPass) {
                	    if (nDims == 1) {
                	    	for (i = 0; i < ixLow; i++) {
                    	    	y[i] = ykeep[i];
                    	    }
                	    }
                	    else {
                	    	for (i = 0; i < ixLow; i++) {
                    	    	for (j = 0; j < jxLow; j++) {
                    	    		y[i*xDim + j] = ykeep[i*xDim + j];
                    	    	}
                    	    }	
                	    }
                	} // if (!thresholdLowPass)
                	if (nDims == 1) {
            	    	for (i = ixLow; i < ix; i++) {
                	    	y[i] = ykeep[i];
                	    }
            	    	for (i = ix2; i < xDim; i++) {
                	    	y[i] = ykeep[i];
                	    }
            	    }
            	    else {
            	    	for (i = ixLow; i < ix; i++) {
                	    	for (j = jxLow; j < jx; j++) {
                	    		y[i*xDim + j] = ykeep[i*xDim + j];
                	    	}
                	    }
            	    	for (i = ix2; i < yDim; i++) {
                	    	for (j = jx2; j < xDim; j++) {
                	    		y[i*xDim+j] = ykeep[i*xDim + j];
                	    	}
                	    }
            	    }
                	midwt();
                	
                	if (destImage != null) {
	            		try {
	                        destImage.importData(0, aArray, true);
	                    }
	                    catch(IOException e) {
	                        MipavUtil.displayError("IOException on destImage.importData(0, aArray, true)");
	                        setCompleted(false);
	                        return;
	                    }
            		} // if (destImage != null)
                } // if (doDenoise)
            }
        } // else !redundant
        
        
        if (selfTest) {
            for (i = 0; i < xDim; i++)  {
                Preferences.debug("aArray[" + i + "] = " + aArray[i] + "\n", Preferences.DEBUG_FILEIO);
                //Preferences.debug("Calculated yl["+i+"] = " + yl[i] + "\n", Preferences.DEBUG_FILEIO);
                //Preferences.debug("Calculated yh["+i+"] = " + lhA[0][i] + "\n", Preferences.DEBUG_FILEIO);
            }
        }
        
       /* mirdwt();
        if (selfTest) {
            for (i = 0; i < xDim; i++)  {
                Preferences.debug("aArray[" + i + "] = " + aArray[i] + "\n", Preferences.DEBUG_FILEIO);
            }
            setCompleted(false);
            return;
        }
        
        try {
            destImage.importData(0, aArray, true);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException on destImage.importData(0, aArray, true)");
            setCompleted(false);
            return;
        } */
        
        setCompleted(true);
        return;
        
    }
    
    private void makeSig(String sigName, int signalLength) {
    	// Leopold calculated correctly:
        // aArray[0] = 0.0
		// aArray[1] = 1.0
		// aArray[2] = 0.0
		// aArray[3] = 0.0
		// aArray[4] = 0.0
		// aArray[5] = 0.0
		// aArray[6] = 0.0
		// aArray[7] = 0.0
        // Correct aArray = [0     1     0     0     0     0     0     0];
    	
    	// Heavisine calculated correctly:
    	// aArray[0] = 4.0
		// aArray[1] = 4.440892098500626E-16
		// aArray[2] = -6.0
		// aArray[3] = -2.000000000000001
		// aArray[4] = 2.0
		// aArray[5] = 1.4432899320127035E-15
		// aArray[6] = -4.0
		// aArray[7] = -1.9984014443252818E-15
	    // Correct aArray = [4.0000    0.0000   -6.0000   -2.0000    2.0000    0.0000   -4.0000   -0.0000];
    	
    	// Bumps calculated correctly:
    	// aArray[0] = 0.32057129709958176
		// aArray[1] = 5.052686334003055
		// aArray[2] = 0.3726713834396781
		// aArray[3] = 0.012873234114248008
		// aArray[4] = 0.029514406018810786
		// aArray[5] = 0.04889716017951243
		// aArray[6] = 3.7177246433324436E-4
		// aArray[7] = 3.4712645702268925E-5
    	// Correct aArray = y = [0.3206    5.0527    0.3727    0.0129    0.0295    0.0489    0.0004    0.0000];
    	
    	// Blocks calculated correctly:
    	// aArray[0] = 4.0
		// aArray[1] = 0.5
		// aArray[2] = 3.0
		// aArray[3] = 0.8999999999999999
		// aArray[4] = 0.8999999999999999
		// aArray[5] = 5.199999999999999
		// aArray[6] = -8.881784197001252E-16
		// aArray[7] = -8.881784197001252E-16
    	// Correct aArray = [4.0000    0.5000    3.0000    0.9000    0.9000    5.2000   -0.0000   -0.0000];
    	
    	// Doppler calculated correctly:
    	// aArray[0] = -0.19543398999264275
		// aArray[1] = -0.30670797808729855
		// aArray[2] = 3.7120133355371736E-16
		// aArray[3] = -0.4703055680484446
		// aArray[4] = 0.4930066485916346
		// aArray[5] = -0.2703204087277996
		// aArray[6] = -0.4127286397614705
		// aArray[7] = 0.10249756772553398
		// aArray[8] = 0.400051572595633
		// aArray[9] = 0.3453568260433621
		// aArray[10] = 0.14249155862305468
		// aArray[11] = -0.0
    	// Correct aArray = [-0.1954 -0.3067 0.0000 -0.4703 0.4930 -0.2703 -0.4127 0.1025 0.4001 0.3454 0.1425 0];
    	
    	// Ramp calculated correctly:
    	// aArray[0] = 0.125
		// aArray[1] = 0.25
		// aArray[2] = -0.625
		// aArray[3] = -0.5
		// aArray[4] = -0.375
		// aArray[5] = -0.25
		// aArray[6] = -0.125
		// aArray[7] = 0.0
    	// Correct aArray = = [0.1250    0.2500   -0.6250   -0.5000   -0.3750   -0.2500   -0.1250         0];
    	
    	// Cusp calculated correctly:
    	// aArray[0] = 0.4949747468305833
		// aArray[1] = 0.34641016151377546
		// aArray[2] = 0.07071067811865478
		// aArray[3] = 0.36055512754639896
		// aArray[4] = 0.5049752469181039
		// aArray[5] = 0.6164414002968976
		// aArray[6] = 0.7106335201775947
		// aArray[7] = 0.7937253933193772
    	// Correct aArray = [0.4950    0.3464    0.0707    0.3606    0.5050    0.6164    0.7106    0.7937];
    	
    	// Sing calculated correctly:
    	// aArray[0] = 5.333333333333333
		// aArray[1] = 16.0
		// aArray[2] = 16.0
		// aArray[3] = 5.333333333333333
		// aArray[4] = 3.2
		// aArray[5] = 2.2857142857142856
		// aArray[6] = 1.7777777777777777
		// aArray[7] = 1.4545454545454546
    	// Correct aArray = [5.3333   16.0000   16.0000    5.3333    3.2000    2.2857    1.7778    1.4545];
    	
    	// HiSine calculated correctly:
    	// aArray[0] = 0.8267272436365299
		// aArray[1] = -0.9302383506006373
		// aArray[2] = 0.21998241252745854
		// aArray[3] = 0.6827128478839758
		// aArray[4] = -0.9881749191102808
		// aArray[5] = 0.42918737547700503
		// aArray[6] = 0.5052507632470037
		// aArray[7] = -0.997698502043002
    	// Correct aArray = [0.8267   -0.9302    0.2200    0.6827   -0.9882    0.4292    0.5053   -0.9977];
    	
    	// LoSine calculated correctly:
    	// aArray[0] = 0.8659730391584588
		// aArray[1] = 0.8661301045447303
		// aArray[2] = 3.141592601914583E-4
		// aArray[3] = -0.8658158883040749
		// aArray[4] = -0.8662870844473873
		// aArray[5] = -6.283184893766407E-4
		// aArray[6] = 0.8656586519970885
		// aArray[7] = 0.8664439788509374
    	// Correct aArray = [0.865973039158459   0.866130104544730   0.000314159260191  -0.865815888304075 
    	//                   -0.866287084447387  -0.000628318489377   0.865658651997088   0.866443978850937];
    	
    	// LinChirp calculated correctly:
    	// aArray[0] = 0.049067674327418015
		// aArray[1] = 0.19509032201612825
		// aArray[2] = 0.4275550934302821
		// aArray[3] = 0.7071067811865475
		// aArray[4] = 0.9415440651830208
		// aArray[5] = 0.9807852804032304
		// aArray[6] = 0.6715589548470186
		// aArray[7] = 1.2246467991473532E-16
    	// Correct aArray = [0.0491    0.1951    0.4276    0.7071    0.9415    0.9808    0.6716    0.0000];
    	
    	// TwoChirp calculated correctly:
    	// aArray[0] = 0.5132096245851414
		// aArray[1] = 1.5
		// aArray[2] = 0.5411961001461971
		// aArray[3] = 0.8660254037844385
		// aArray[4] = -0.5132096245851412
		// aArray[5] = 0.0
		// aArray[6] = 0.513209624585139
		// aArray[7] = 0.8660254037844382
    	// Correct aArray = [0.5132    1.5000    0.5412    0.8660   -0.5132         0    0.5132    0.8660];
    	
    	// QuadChirp calculated correctly:
    	// aArray[0] = 0.01636173162648678
		// aArray[1] = 0.13052619222005157
		// aArray[2] = 0.4275550934302821
		// aArray[3] = 0.8660254037844386
		// aArray[4] = 0.8895160754218563
		// aArray[5] = -0.38268343236508967
		// aArray[6] = -0.6216605733700774
		// aArray[7] = 0.8660254037844392
    	// Correct aArray = [0.0164    0.1305    0.4276    0.8660    0.8895   -0.3827   -0.6217    0.8660];
    	
    	// MishMash calculated correctly:
    	// aArray[0] = 0.8921566495904347
		// aArray[1] = -0.6046218363644575
		// aArray[2] = 1.0750925993880227
		// aArray[3] = 2.2558450328549617
		// aArray[4] = 0.8428852214945963
		// aArray[5] = 1.0272892235151458
		// aArray[6] = 0.5551491447239448
		// aArray[7] = -0.13167309825856277
    	// Correct aArray = [0.8922   -0.6046    1.0751    2.2558    0.8429    1.0273    0.5551   -0.1317];
    	
    	// WernerSorrows calculated correctly:
    	// aArray[0] = 1.5545232016241137
		// aArray[1] = 5.317538305418545
		// aArray[2] = 0.8252019541826737
		// aArray[3] = 1.6955860819982236
		// aArray[4] = -1.2677793818568923
		// aArray[5] = 0.6466149233539719
		// aArray[6] = 1.7331595333261316
		// aArray[7] = -0.9976637893973013
    	// Correct aArray = [1.5545    5.3175    0.8252    1.6956   -1.2678    0.6466    1.7332   -0.9977];
    	
        int i, j;
        double t[] = new double[signalLength];
        aArray = new double[signalLength];
        for ( i = 1; i <= signalLength; i++) {
            t[i-1] = (double)i/(double)signalLength;    
        }
        if (sigName.equalsIgnoreCase("Leopold")) { // Kronecker
            for (i = 0; i < signalLength; i++) {
                if (t[i] == Math.floor(0.37 * signalLength)/signalLength) {
                    aArray[i] = 1.0;
                }
                else {
                    aArray[i] = 0.0;
                }
            }
        }
        else if (sigName.equalsIgnoreCase("HeaviSine")) {
        	for (i = 0; i < signalLength; i++) {
        		aArray[i] = 4.0*Math.sin(4.0*Math.PI*t[i]);
        		aArray[i] = aArray[i] - Math.signum(t[i] - 0.3) - Math.signum(0.72 - t[i]);
        	}
        }
        else if (sigName.equalsIgnoreCase("Bumps")) {
        	double pos[] = new double[] { .1, .13, .15, .23, .25, .40, .44, .65, .76, .78, .81};
        	double hgt[] = new double[] { 4,  5,   3,   4,  5,  4.2, 2.1, 4.3,  3.1, 5.1, 4.2};
        	double wth[] = new double[] {.005, .005, .006, .01, .01, .03, .01, .01,  .005, .008, .005};
        	for (i = 0; i < signalLength; i++) {
	        	for (j = 0; j < pos.length; j++) {
	        	    aArray[i] = aArray[i] + hgt[j]/Math.pow( 1 + Math.abs((t[i] - pos[j])/wth[j]),4.0);
	        	}
        	}
        }
        else if (sigName.equalsIgnoreCase("Blocks")) {
        	double pos[] = new double[] { .1, .13, .15, .23, .25, .40, .44, .65,  .76, .78, .81};
        	double hgt[] = new double[] {4, (-5), 3, (-4), 5, (-4.2), 2.1, 4.3,  (-3.1), 2.1, (-4.2)};
        	
        	for (i = 0; i < signalLength; i++) {
	        	for (j = 0; j < pos.length; j++) {
        	        aArray[i] = aArray[i] + (1 + Math.signum(t[i]-pos[j]))*(hgt[j]/2) ;
	        	}
        	}
        }
        else if (sigName.equalsIgnoreCase("Doppler")) {
        	for (i = 0; i < signalLength; i++) {
        	    aArray[i] = Math.sqrt(t[i]*(1-t[i]))*Math.sin((2*Math.PI*1.05) /(t[i]+.05));	
        	}
        }
        else if (sigName.equalsIgnoreCase("Ramp")) {
        	for (i = 0; i < signalLength; i++) {
        		if (t[i] < 0.37) {
        	        aArray[i] = t[i];
        		}
        		else {
        			aArray[i] = t[i] - 1.0;
        		}
        	}
        }
        else if (sigName.equalsIgnoreCase("Cusp")) {
        	for (i = 0; i < signalLength; i++) {
        	    aArray[i] = Math.sqrt(Math.abs(t[i] - .37));	
        	}
        }
        else if (sigName.equalsIgnoreCase("Sing")) {
        	int k = (int)Math.floor(signalLength * .37);
        	for (i = 0; i < signalLength; i++) {
        	    aArray[i] = 1 /Math.abs(t[i] - (k+.5)/signalLength);	
        	}
        }
        else if (sigName.equalsIgnoreCase("HiSine")) {
        	for (i = 0; i < signalLength; i++) {
        	    aArray[i] = Math.sin( Math.PI * (signalLength * .6902) * t[i]);	
        	}
        }
        else if (sigName.equalsIgnoreCase("LoSine")) {
        	for (i = 0; i < signalLength; i++) {
        	    aArray[i] = Math.sin( Math.PI * (signalLength * .3333) * t[i]);	
        	}
        }
        else if (sigName.equalsIgnoreCase("LinChirp")) {
        	for (i = 0; i < signalLength; i++) {
        	    aArray[i] = Math.sin(Math.PI * t[i] * ((signalLength * .125) * t[i]));	
        	}
        }
        else if (sigName.equalsIgnoreCase("TwoChirp")) {
        	for (i = 0; i < signalLength; i++) {
        	    aArray[i] = Math.sin(Math.PI * t[i] * (signalLength * t[i])) + Math.sin((Math.PI/3) * t[i] * (signalLength * t[i]));	
        	}
        }
        else if (sigName.equalsIgnoreCase("QuadChirp")) {
        	for (i = 0; i < signalLength; i++) {
        	    aArray[i] = Math.sin( (Math.PI/3) * t[i] * (signalLength * t[i] * t[i]));	
        	}
        }
        else if (sigName.equalsIgnoreCase("MishMash")) {
        	// QuadChirp + LinChirp + HiSine
        	for (i = 0; i < signalLength; i++) {
        	    aArray[i] = Math.sin( (Math.PI/3) * t[i] * (signalLength * t[i] * t[i]));
        	    aArray[i] += Math.sin( Math.PI * (signalLength * .6902) * t[i]);
        	    aArray[i] += Math.sin(Math.PI * t[i] * ((signalLength * .125) * t[i]));	
        	}
        }
        else if (sigName.equalsIgnoreCase("WernerSorrows")) {
        	double pos[] = new double[] { .1, .13, .15, .23, .25, .40, .44, .65, .76, .78, .81};
        	double hgt[] = new double[] { 4,  5,   3,   4,  5,  4.2, 2.1, 4.3,  3.1, 5.1, 4.2};
        	double wth[] = new double[] {.005, .005, .006, .01, .01, .03, .01, .01,  .005, .008, .005};
        	for (i = 0; i < signalLength; i++) {
        		aArray[i] = Math.sin(Math.PI * t[i] * (signalLength/2.0 * t[i]*t[i])) ;
        		aArray[i] = aArray[i] +  Math.sin(Math.PI * (signalLength * .6902) * t[i]);
        		aArray[i] = aArray[i] +  Math.sin(Math.PI * t[i] * (signalLength * t[i]));
	        	for (j = 0; j < pos.length; j++) {
	        	    aArray[i] = aArray[i] + hgt[j]/Math.pow( 1 + Math.abs((t[i] - pos[j])/wth[j]),4.0);
	        	}
        	}
        }
    }
    
    private void mdwt() {
    	// function computes the discrete wavelet transform y for a 1D or 2D input
    	// signal x.
    	
    	//    Input:
    	//	x    : finite length 1D or 2D signal (implicitely periodized)
        //       h    : scaling filter
    	//       L    : number of levels. in case of a 1D signal length(x) must be
        //              divisible by 2^L; in case of a 2D signal the row and the
    	//              column dimension must be divisible by 2^L.
    	
    	// output y of length sliceSize
    	
    	int i;
        int lh;
        double h0[];
        double h1[];
        int actual_yDim;
        int actual_xDim;
        int lhm1;
        int actual_L;
        int r_o_a = 1;
        int c_o_a;
        double xdummy[];
        int maxmn;
        int ir;
        double ydummyl[];
        double ydummyh[];
        int ic;
        boolean calcMinMax = true;
        double x[] = new double[sliceSize];
        lh = filterLength;
        
        maxmn = Math.max(xDim,yDim);
        xdummy = new double[maxmn+lh-1];
        ydummyl = new double[maxmn];
        ydummyh = new double[maxmn];
        
        h0 = new double[lh];
        h1 = new double[lh];
        // analysis lowpass and highpass
        for (i = 0; i < filterLength; i++) {
            h0[i] = scalingFilter[filterLength - i - 1];
            h1[i] = scalingFilter[i];
        }  
        
        for (i = 0; i < lh; i += 2) {
            h1[i] = -h1[i];
        }
        
        lhm1 = lh - 1;
        actual_yDim = 2*yDim;
        actual_xDim = 2*xDim;
        for (i = 0; i < sliceSize; i++) {
            x[i] = aArray[i];
        }
        
        /* main loop */
        for (actual_L=1; actual_L <= numberOfLevels; actual_L++){
          if (yDim==1)
            actual_yDim = 1;
          else{
            actual_yDim = actual_yDim/2;
            r_o_a = actual_yDim/2;     
          }
          actual_xDim = actual_xDim/2;
          c_o_a = actual_xDim/2;
          

          /* go by rows */
          for (ir=0; ir<actual_yDim; ir++) {            /* loop over rows */
            /* store in dummy variable */
            for (i=0; i<actual_xDim; i++) {
		      	if (actual_L==1) {  
		      	  xdummy[i] = x[ir*xDim + i];  
		      	}
		      	else  {
		      	  xdummy[i] = y[ir*xDim + i];  
		      	} 
            } // for (i=0; i<actual_xDim; i++) 
	        /* perform filtering lowpass and highpass*/
	        fpsconv(xdummy, actual_xDim, h0, h1, lhm1, ydummyl, ydummyh); 
	        /* restore dummy variables in matrices */
	        ic = c_o_a;
	        for  (i=0; i<c_o_a; i++){    
		      	y[ir*xDim + i] = ydummyl[i];  
		      	y[ir*xDim + ic++] = ydummyh[i];  
            } 
          } // for (ir=0; ir<actual_yDim; ir++) 
          
          /* go by columns in case of a 2D signal*/
          if (yDim>1){
            for (ic=0; ic<actual_xDim; ic++){            /* loop over column */
		      	/* store in dummy variables */
		      	for (i=0; i<actual_yDim; i++) {
		      	  xdummy[i] = y[i*xDim + ic];
		      	}
		      	/* perform filtering lowpass and highpass*/
		      	fpsconv(xdummy, actual_yDim, h0, h1, lhm1, ydummyl, ydummyh); 
		      	/* restore dummy variables in matrix */
		      	ir = r_o_a;
		      	for (i=0; i<r_o_a; i++){    
		      	  y[i*xDim + ic] = ydummyl[i];  
		      	  y[ir++ * xDim + ic] = ydummyh[i];  
		      	}
            }
          }
        } // for (actual_L=1; actual_L <= numberOfLevels; actual_L++)
        if (waveletImage != null) {
	        if (z == 0) {
	            waveletImage[0] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_wavelet");
	        }
	        try {
	            waveletImage[0].importData(z*sliceSize, y, calcMinMax); 
	        }
	        catch(IOException  e) {
	            MipavUtil.displayError("IOException on waveletImage[0].importData(z*sliceSize, y, calcMinMax)");
	            setCompleted(false);
	            return;
	        } 
        } // if (waveletImage != null)
    }
    
    private void mrdwt() {
    	//    Function computes the redundant discrete wavelet transform y
    	//   for a 1D  or 2D input signal. (Redundant means here that the
    	//    sub-sampling after each stage is omitted.) yl contains the
    	//    lowpass and yh the highpass components. In the case of a 2D
    	//    signal, the ordering in yh is 
    	//    [lh hl hh lh hl ... ] (first letter refers to row, second to
    	//    column filtering). 
    	
    	//    Input:
    	//	     x : finite length 1D or 2D signal (implicitly periodized)
    	//       h : scaling filter
    	//       L : number of levels. In the case of a 1D 
        //           length(x) must be  divisible by 2^L;
        //           in the case of a 2D signal, the row and the
    	//           column dimension must be divisible by 2^L.
    	
        int i;
        int lh;
        double h0[];
        double h1[];
        int actual_yDim;
        int actual_xDim;
        int sample_f;
        int actual_L;
        double xdummyl[];
        double xdummyh[];
        double ydummyll[];
        double ydummylh[];
        double ydummyhl[];
        double ydummyhh[];
        int maxmn;
        int n_cb;
        int ir;
        int n_c;
        int ic;
        int n_rb;
        int n_r;
        int waveletImageIndex = 0;
        boolean calcMinMax = true;
        
        lh = filterLength;
        h0 = new double[lh];
        h1 = new double[lh];
        // analysis lowpass and highpass
        for (i = 0; i < filterLength; i++) {
            h0[i] = scalingFilter[filterLength - i - 1];
            h1[i] = scalingFilter[i];
        }
        
        
        for (i = 0; i < lh; i += 2) {
            h1[i] = -h1[i];
        }
        
        maxmn = Math.max(xDim, yDim);
        xdummyl = new double[maxmn + lh - 1];
        xdummyh = new double[maxmn + lh - 1];
        ydummyll = new double[maxmn];
        ydummylh = new double[maxmn];
        ydummyhl = new double[maxmn];
        ydummyhh = new double[maxmn];
        
        actual_yDim = 2*yDim;
        actual_xDim = 2*xDim;
        for (i = 0; i < sliceSize; i++) {
            yl[i] = aArray[i];
        }
        
        /* main loop */
        sample_f = 1;
        for (actual_L = 1; actual_L <= numberOfLevels; actual_L++) {
            actual_yDim = actual_yDim/2;
            actual_xDim = actual_xDim/2;
            
            /* go by rows */
            n_cb = xDim/actual_xDim;   /* # of column blocks per row */
            for (ir = 0; ir < yDim; ir++) {  /* loop over rows */
                for (n_c = 0; n_c < n_cb; n_c++) {  /* loop within one row */
                    /* store in dummy variable */ 
                    ic = -sample_f + n_c;
                    for (i = 0; i < actual_xDim; i++) {
                        ic = ic + sample_f;
                        xdummyl[i] = yl[ic + ir * xDim];
                    } // for (i = 0; i < actual_xDim; i++)
                    /* perform the filtering lowpass/highpass */
                    fpconv(xdummyl, actual_xDim, h0, h1, lh, ydummyll, ydummyhh);
                    /* restore dummy variables in matrices */
                    ic = -sample_f + n_c;
                    for (i = 0; i < actual_xDim; i++) {
                        ic = ic + sample_f;
                        yl[ic + ir * xDim] = ydummyll[i];
                        lhA[actual_L-1][ic + ir * xDim] = ydummyhh[i];
                    } // for (i = 0; i < actual_xDim; i++)
                } // for (n_c = 0; n_c < n_cb; n_c++)
            } // for (ir = 0; ir < yDim; ir++)
            
            /* go by columns in case of a 2D signal */
            if (yDim > 1) {
                n_rb = yDim/actual_yDim;  /* # of row blocks per column */
                for (ic = 0; ic < xDim; ic++) { /* loop over column */
                    for (n_r = 0; n_r < n_rb; n_r++) { /* loop within one column */
                        /* store in dummy variables */
                        ir = -sample_f + n_r;
                        for (i = 0; i < actual_yDim; i++) {
                            ir = ir + sample_f;
                            xdummyl[i] = yl[ic + ir * xDim];
                            xdummyh[i] = lhA[actual_L-1][ic + ir * xDim];
                        } // for (i = 0; i < actual_yDim; i++)
                        /* perform filtering, first LL/LH, then HL/HH */
                        fpconv(xdummyl, actual_yDim, h0, h1, lh, ydummyll, ydummylh);
                        fpconv(xdummyh, actual_yDim, h0, h1, lh, ydummyhl, ydummyhh);
                        /* restore dummy variables in arrays */
                        ir = -sample_f + n_r;
                        for (i = 0; i < actual_yDim; i++) {
                            ir = ir + sample_f; 
                            yl[ic + ir * xDim] = ydummyll[i];
                            lhA[actual_L-1][ic + ir * xDim] = ydummylh[i];
                            hlA[actual_L-1][ic + ir * xDim] = ydummyhl[i];
                            hhA[actual_L-1][ic + ir * xDim] = ydummyhh[i];
                        } // for (i = 0; i < actual_yDim; i++)
                    } // for (n_r = 0; n_r < n_rb; n_r++)
                } // for (ic = 0; ic < xDim; ic++)
                
                if (actual_L < numberOfLevels) {
                    for (i = 0; i < sliceSize; i++) {
                        llA[actual_L-1][i] = yl[i];
                    }
                }
                
                if (waveletImage != null) {
	                if (z == (zDim - 1)) {
	                    calcMinMax = true;
	                }
	                else {
	                    calcMinMax = false;
	                }
	                
	                if ((actual_L == maximumLevel) && (maximumLevel > minimumLevel)) {
	                    if (z == 0) {
	                        waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_Mll");
	                    }
	                    try {
	                        waveletImage[waveletImageIndex++].importData(z*sliceSize, llA[minimumLevel-1], calcMinMax);  
	                    }
	                    catch(IOException  e) {
	                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(z*sliceSize," +
	                        		" llA[minimumLevel-1], calcMinMax)");
	                        setCompleted(false);
	                        return;
	                    }  
	                    if (z == 0) {
	                        waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_Mlh");
	                    }
	                    try {
	                        waveletImage[waveletImageIndex++].importData(z*sliceSize, lhA[minimumLevel-1], calcMinMax);  
	                    }
	                    catch(IOException  e) {
	                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(z*sliceSize, " +
	                        		"lhA[minimumLevel-1], calcMinMax)");
	                        setCompleted(false);
	                        return;
	                    }
	                    
	                    if (z == 0) {
	                        waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_Mhl"); 
	                    }
	                    try {
	                        waveletImage[waveletImageIndex++].importData(z*sliceSize, hlA[minimumLevel-1], calcMinMax);  
	                    }
	                    catch(IOException  e) {
	                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(z*sliceSize," +
	                        		" hlA[minimumLevel-1], calcMinMax)");
	                        setCompleted(false);
	                        return;
	                    }
	                    
	                    if (z == 0) {
	                        waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_Mhh"); 
	                    }
	                    try {
	                        waveletImage[waveletImageIndex++].importData(z*sliceSize, hhA[minimumLevel-1], calcMinMax);  
	                    }
	                    catch(IOException  e) {
	                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(z*sliceSize," +
	                        		" hhA[minimumLevel-1], calcMinMax)");
	                        setCompleted(false);
	                        return;
	                    }    
	                }
	                
	                if (doWaveletImages) {
	                    
	                    if (z == 0) {
	                        waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_L" +
	                            actual_L + "_ll");  
	                    }
	                    try {
	                        waveletImage[waveletImageIndex++].importData(z*sliceSize, yl, calcMinMax);  
	                    }
	                    catch(IOException  e) {
	                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(z*sliceSize, yl, calcMinMax)");
	                        setCompleted(false);
	                        return;
	                    }
	                    if (z == 0) {
	                        waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_L" +
	                                                                       actual_L + "_lh");
	                    }
	                    try {
	                        waveletImage[waveletImageIndex++].importData(z*sliceSize, lhA[actual_L-1], calcMinMax);  
	                    }
	                    catch(IOException  e) {
	                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(z*sliceSize," +
	                        		" lhA[actual_L-1], calcMinMax)");
	                        setCompleted(false);
	                        return;
	                    }
	                    
	                    if (z == 0) {
	                        waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_L" +
	                            actual_L + "_hl");  
	                    }
	                    try {
	                        waveletImage[waveletImageIndex++].importData(z*sliceSize, hlA[actual_L-1], calcMinMax);  
	                    }
	                    catch(IOException  e) {
	                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(z*sliceSize, " +
	                        		"hlA[actual_L-1], calcMinMax)");
	                        setCompleted(false);
	                        return;
	                    }
	                    
	                    if (z == 0) {
	                        waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_L" +
	                            actual_L + "_hh");
	                    }
	                    try {
	                        waveletImage[waveletImageIndex++].importData(z*sliceSize, hhA[actual_L-1], calcMinMax);  
	                    }
	                    catch(IOException  e) {
	                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(z*sliceSize, " +
	                        		"hhA[actual_L-1], calcMinMax)");
	                        setCompleted(false);
	                        return;
	                    }
	                    
	                } // if (doWaveletImages)
                } // if (waveletImage != null)
            } // if (yDim > 1)
            sample_f = sample_f*2;
        } // for (actual_L = 1; actual_L <= numberOfLevels; actual_L++)
        
    } // mrdwt
    
    private void fpconv(double x_in[], int lx, double h0[], double h1[], int lh,
                        double x_outl[], double x_outh[]) {
        int i,j;
        double x0, x1;
        
        for (i = lx; i < lx + lh - 1; i++) {
            x_in[i] = x_in[i-lx];
        }
        
        for (i = 0; i < lx; i++) {
            x0 = 0;
            x1 = 0;
            for (j = 0; j < lh; j++) {
                x0 = x0 + x_in[j+i]*h0[lh-1-j];
                x1 = x1 + x_in[j+i]*h1[lh-1-j];
            } // for (j = 0; j < lh; j++)
            x_outl[i] = x0;
            x_outh[i] = x1;
        } // for (i = 0; i < lx; i++)
    } // fpconv
    
    private void fpsconv(double x_in[], int lx, double h0[], double h1[], int lhm1,
    		double x_outl[], double x_outh[]) {
      int i, j, ind;
      double x0, x1;

      for (i=lx; i < lx+lhm1; i++) {
        x_in[i] = x_in[i-lx];
      }
      ind = 0;
      for (i=0; i < lx; i+=2) {
        x0 = 0;
        x1 = 0;
        for (j=0; j<=lhm1; j++) {
          x0 = x0 + x_in[i+j]*h0[lhm1-j];
          x1 = x1 + x_in[i+j]*h1[lhm1-j];
        }
        x_outl[ind] = x0;
        x_outh[ind++] = x1;
      }
    }
    
    public void midwt() {
        int i;
        int lh = filterLength;
        double g0[] = new double[lh];
        double g1[] = new double[lh];
        int lhm1, lhhm1, sample_f, actual_xDim, actual_yDim;
        int actual_L;
        int r_o_a;
        int c_o_a;
        int ic, ir;
        int maxmn = Math.max(xDim,yDim);
        double xdummy[] = new double[maxmn];
        double ydummyl[] = new double[maxmn+lh/2-1];
        double ydummyh[] = new double[maxmn+lh/2-1];
        
        /* synthesis lowpass and highpass */
        for (i = 0; i < filterLength; i++) {
            g0[i] = scalingFilter[i];
            g1[i] = scalingFilter[filterLength - i - 1];
        }
        
        for (i = 1; i < lh; i += 2) {
            g1[i] = -g1[i];
        }
        
        lhm1 = lh - 1;
        lhhm1 = lh/2 - 1;
        /* 2^L */
        sample_f = 1;
        for (i = 1; i < numberOfLevels; i++) {
            sample_f = sample_f * 2;
        }
        
        if (yDim>1) {
            actual_yDim = yDim/sample_f;
        }
        else { 
            actual_yDim = 1;
        }
        actual_xDim = xDim/sample_f;
        
        for (i=0; i< sliceSize; i++) {
            aArray[i] = y[i];
        }
          
        /* main loop */
        for (actual_L=numberOfLevels; actual_L >= 1; actual_L--) {
          r_o_a = actual_yDim/2;
          c_o_a = actual_xDim/2;
          
          /* go by columns in case of a 2D signal*/
          if (yDim>1){
            for (ic=0; ic<actual_xDim; ic++){            /* loop over column */
		      	/* store in dummy variables */
		      	ir = r_o_a;
		      	for (i=0; i<r_o_a; i++){    
		      	  ydummyl[i+lhhm1] = aArray[i*xDim + ic];  
		      	  ydummyh[i+lhhm1] = aArray[ir++ * xDim + ic];  
		      	}
		      	/* perform filtering lowpass and highpass*/
		      	bpsconv(xdummy, r_o_a, g0, g1, lhm1, lhhm1, ydummyl, ydummyh); 
		      	/* restore dummy variables in matrix */
		      	for (i=0; i<actual_yDim; i++) {
		      	  aArray[i*xDim + ic] = xdummy[i]; 
		      	}
		     }
          } // if (yDim > 1)
          /* go by rows */
          for (ir=0; ir<actual_yDim; ir++){            /* loop over rows */
            /* store in dummy variable */
            ic = c_o_a;
            for  (i=0; i<c_o_a; i++){    
      	        ydummyl[i+lhhm1] = aArray[ir*xDim + i];  
      	        ydummyh[i+lhhm1] = aArray[ir*xDim + ic++];  
            } 
            /* perform filtering lowpass and highpass*/
            bpsconv(xdummy, c_o_a, g0, g1, lhm1, lhhm1, ydummyl, ydummyh); 
            /* restore dummy variables in matrices */
            for (i=0; i<actual_xDim; i++) {
              aArray[ir*xDim + i] = xdummy[i]; 
            }
          } // for (ir=0; ir<actual_yDim; ir++)  
          if (yDim==1) {
            actual_yDim = 1;
          }
          else {
            actual_yDim = actual_yDim*2;
          }
          actual_xDim = actual_xDim*2;
        } // for (actual_L=numberOfLevels; actual_L >= 1; actual_L--)  
    }
    
    public void mirdwt() {
        int i;
        int lh;
        double g0[];
        double g1[];
        double xh[];
        double xdummyl[];
        double xdummyh[];
        double ydummyll[];
        double ydummylh[];
        double ydummyhl[];
        double ydummyhh[];
        int maxmn;
        int lhm1;
        int sample_f;
        int actual_xDim;
        int actual_yDim;
        int actual_L;
        int n_rb;
        int ic;
        int n_r;
        int ir;
        int n_cb;
        int n_c;
        
        lh = filterLength;
        g0 = new double[lh];
        g1 = new double[lh];
        
        /* synthesis lowpass and highpass */
        for (i = 0; i < filterLength; i++) {
            g0[i] = scalingFilter[i]/2;
            g1[i] = scalingFilter[filterLength - i - 1]/2;
        }
        
        for (i = 1; i < lh; i += 2) {
            g1[i] = -g1[i];
        }
        
        maxmn = Math.max(xDim, yDim);
        xh = new double[sliceSize];
        xdummyl = new double[maxmn];
        xdummyh = new double[maxmn];
        ydummyll = new double[maxmn+lh-1];
        ydummylh = new double[maxmn+lh-1];
        ydummyhl = new double[maxmn+lh-1];
        ydummyhh = new double[maxmn+lh-1];
        
        lhm1 = lh - 1;
        /* 2^L */
        sample_f = 1;
        for (i = 1; i < numberOfLevels; i++) {
            sample_f = sample_f * 2;
        }
        actual_yDim = yDim/sample_f;
        actual_xDim = xDim/sample_f;
        /* Restore yl in aArray */
        for (i = 0; i < sliceSize; i++) {
            aArray[i] = yl[i];
        }
        
        /* main loop */
        for (actual_L = numberOfLevels; actual_L >= 1; actual_L--) {
            /* Go by columns in case of a 2D signal*/
            if (yDim > 1) {
                n_rb = yDim/actual_yDim;   /* # of row blocks per column */
                for (ic = 0; ic < xDim; ic++) {    /* loop over column */
                    for (n_r = 0; n_r < n_rb; n_r++) {    /* loop within one column */
                        /* store in dummy variables */
                        ir = -sample_f + n_r;
                        for (i = 0; i < actual_yDim; i++) {
                            ir = ir + sample_f; 
                            ydummyll[i+lhm1] = aArray[ic + ir * xDim];
                            ydummylh[i+lhm1] = lhA[actual_L-1][ic + ir * xDim];
                            ydummyhl[i+lhm1] = hlA[actual_L-1][ic + ir * xDim];
                            ydummyhh[i+lhm1] = hhA[actual_L-1][ic + ir * xDim];
                        } // for (i = 0; i < actual_yDim; i++)
                        /* perform filtering and adding: first LL/LH, then HL/HH */
                        bpconv(xdummyl, actual_yDim, g0, g1, lh, ydummyll, ydummylh);
                        bpconv(xdummyh, actual_yDim, g0, g1, lh, ydummyhl, ydummyhh);
                        /* Store dummy variables in matrices */
                        ir = -sample_f + n_r;
                        for (i = 0; i < actual_yDim; i++) {
                            ir = ir + sample_f;
                            aArray[ic + ir * xDim] = xdummyl[i];
                            xh[ic + ir * xDim] = xdummyh[i];
                        } // for (i = 0; i < actual_yDim; i++)
                    } // for (n_r = 0; n_r < n_rb; n_r++)
                } // for (ic = 0; ic < xDim; ic++)
            } // if (yDim > 1)
            
            /* go by rows */
            n_cb = xDim/actual_xDim;       /* number of column blocks per row */
            for (ir = 0; ir < yDim; ir++) {    /* loop over rows */
                for (n_c = 0; n_c < n_cb; n_c++) { /* loop within one row */
                    /* store in dummy variable */
                    ic = -sample_f + n_c;
                    for (i = 0; i < actual_xDim; i++) {
                        ic = ic + sample_f;
                        ydummyll[i+lhm1] = aArray[ic + ir * xDim];
                        if (yDim > 1) {
                            ydummyhh[i+lhm1] = xh[ic + ir * xDim];
                        }
                        else {
                            ydummyhh[i+lhm1] = lhA[actual_L-1][ic + ir * xDim];
                        }
                    } // for (i = 0; i < actual_xDim; i++)
                    /* perform filtering lowpass/highpass */
                    bpconv(xdummyl, actual_xDim, g0, g1, lh, ydummyll, ydummyhh);
                    /* restore dummy variables in matrices */
                    ic = -sample_f + n_c;
                    for (i = 0; i < actual_xDim; i++) {
                        ic = ic + sample_f;
                        aArray[ic + ir * xDim] = xdummyl[i];
                    } // for (i = 0; i < actual_xDim; i++)
                } // for (n_c = 0; n_c < n_cb; n_c++)
            } // for (ir = 0; ir < yDim; ir++)
            sample_f = sample_f/2;
            actual_yDim = actual_yDim * 2;
            actual_xDim = actual_xDim * 2;
        } // for (actual_L = numberOfLevels; actual_L >= 1; actual_L--)
    } // mirdwt()
    
    private void bpsconv(double x_out[], int lx, double g0[], double g1[], int lhm1, int lhhm1,
    		double x_inl[], double x_inh[]) {
      int i, j, ind, tj;
      double x0, x1;

      for (i=lhhm1-1; i > -1; i--){
        x_inl[i] = x_inl[lx+i];
        x_inh[i] = x_inh[lx+i];
      }
      ind = 0;
      for (i=0; i<(lx); i++){
        x0 = 0;
        x1 = 0;
        tj = -2;
        for (j=0; j<=lhhm1; j++){
          tj+=2;
          x0 = x0 + x_inl[i+j]*g0[lhm1-1-tj] + x_inh[i+j]*g1[lhm1-1-tj] ;
          x1 = x1 + x_inl[i+j]*g0[lhm1-tj] + x_inh[i+j]*g1[lhm1-tj] ;
        }
        x_out[ind++] = x0;
        x_out[ind++] = x1;
      }
    }

    
    private void bpconv(double x_out[], int lx, double g0[], double g1[], int lh, double x_inl[], double x_inh[]) {
        int i, j;
        double x0;
        
        for (i = lh-2; i > -1; i--) {
            x_inl[i] = x_inl[lx+i];
            x_inh[i] = x_inh[lx+i];
        }
        for (i = 0; i < lx; i++) {
            x0 = 0;
            for (j = 0; j < lh; j++) {
                x0 = x0 + x_inl[j+i]*g0[lh-1-j] + x_inh[j+i]*g1[lh-1-j];
            }
            x_out[i] = x0;
        }
    } // bpconv
    
    /**
     * Function computes the Daubechies' scaling and wavelet filters (normalized to sqrt(2)).
     * The user specifies an even number filter length and a minimum phase, mid phase, or 
     * maximum phase solution.
     * Reference: "Orthonormal Bases of Compactly Supported Wavelets", CPAM, October, 1989.
     * 
     * From the original source file:
     * %File Name: daubcqf.m
       %Last Modification Date: 01/02/96   15:12:57
       %Current Version: daubcqf.m 2.4
       %File Creation Date: 10/10/88
       %Author: Ramesh Gopinath  <ramesh@dsp.rice.edu>
       %
       %Copyright (c) 2000 RICE UNIVERSITY. All rights reserved.
       %Created by Ramesh Gopinath, Department of ECE, Rice University. 
       
        Correctly gives scaling and wavelet filters for filter length = 4 minimum phase:
        Calculated scaling filter:
        0.48296291314453416
        0.8365163037378078
        0.2241438680420134
        -0.12940952255126037
        Calculated Wavelet filter:
        0.12940952255126037
        0.2241438680420134
        -0.8365163037378078
        0.48296291314453416
        Correct scaling filter = [0.482962913144534   0.836516303737808   0.224143868042013  -0.129409522551260];
        Correct wavelet filter = [0.129409522551260   0.224143868042013  -0.836516303737808   0.482962913144534];
        
        Correctly gives scaling and wavelet filters for filter length = 4 mid phase
        Calculated scaling filter:
		0.48296291314453416
		0.8365163037378078
		0.2241438680420134
		-0.12940952255126037
		Calculated wavelet filter:
		0.12940952255126037
		0.2241438680420134
		-0.8365163037378078
		0.48296291314453416
		Correct scaling filter = [0.482962913144534   0.836516303737808   0.224143868042013  -0.129409522551260];
        Corrct wavelet filter = [0.129409522551260   0.224143868042013  -0.836516303737808   0.482962913144534];
        
        Correctly gives scaling and wavelet filters for filter length = 4 maximum phase
        Calculated scaling filter:
		-0.12940952255126037
		0.2241438680420134
		0.8365163037378078
		0.48296291314453416
		Calculated wavelet filter:
		-0.48296291314453416
		0.8365163037378078
		-0.2241438680420134
		-0.12940952255126037
		Correct scaling filter = [-0.129409522551260   0.224143868042013   0.836516303737808   0.482962913144534];
        Correct wavelet filter = [-0.482962913144534   0.836516303737808  -0.224143868042013  -0.129409522551260];
        
        Correctly gives scaling and wavelet filters for filter length = 6 minimum phase
        Scaling filter:
        0.3326705529500828
        0.8068915093110931
        0.459877502118492
        -0.1350110200102549
        -0.08544127388202716
        0.03522629188570937
        Wavelet filter:
        -0.03522629188570937
        -0.08544127388202716
        0.1350110200102549
        0.459877502118492
        -0.8068915093110931
        0.3326705529500828
        
        Correctly gives scaling and wavelet filters for filter length = 6 mid phase
        Scaling filter:
		0.3326705529500828
		0.8068915093110931
		0.459877502118492
		-0.1350110200102549
		-0.08544127388202716
		0.03522629188570937
		Wavelet filter:
		-0.03522629188570937
		-0.08544127388202716
		0.1350110200102549
		0.459877502118492
		-0.8068915093110931
		0.3326705529500828
        Correct scaling filter = [0.332670552950083   0.806891509311093   0.459877502118491  -0.135011020010255  -0.085441273882027   0.035226291885710];
        Correct wavelet filter = [-0.035226291885710  -0.085441273882027   0.135011020010255   0.459877502118491 -0.806891509311093   0.332670552950083];
     */
    private void daubcqf() {
        int j, m;
        int k = filterLength/2;
        double a = 1.0;
        double p[] = new double[]{1.0};
        double q[] = new double[]{1.0};
        double h_0[] = new double[]{1.0, 1.0};
        double oldp[];
        double oldq[];
        double oldh_0[];
        double A[][];
        double[] eigenvalueR;
        double[][] V;
        double[] eI;
        double[] qtR;
        double[] qtI;
        double[] polyqtR;
        double[] polyqtI;
        double[] w;
        int minj;
        int maxj;
        double sumw;
        double scale;
        double temp[];
        
        if (filterLength == 2) {
            if (filterType == MINIMUM_PHASE) {
                scalingFilter[0] = 1.0/Math.sqrt(2.0);
                scalingFilter[1] = 1.0/Math.sqrt(2.0);
                waveletFilter[0] = 1.0/Math.sqrt(2.0);
                waveletFilter[1] = -1.0/Math.sqrt(2.0);
            }
            return;
        } // if (filterLength == 2)
        
        
        for (j = 1; j <= k-1; j++) {
            a = -a * 0.25 * (j + k - 1.0)/(double)j;
            oldh_0 = new double[h_0.length];
            for (m = 0; m < h_0.length; m++) {
                oldh_0[m] = h_0[m];
            }
            h_0 = new double[h_0.length + 1];
            for (m = 0; m < h_0.length - 1; m++) {
                h_0[m+1] += oldh_0[m];
                h_0[m] += oldh_0[m];
            }
            oldp = new double[p.length];
            for (m = 0; m < p.length; m++) {
                oldp[m] = p[m];
            }
            p = new double[p.length+1];
            for (m = 0; m < p.length-1; m++) {
                p[m+1] += -oldp[m];
                p[m] += oldp[m];
            }
            oldp = new double[p.length];
            for (m = 0; m < p.length; m++) {
                oldp[m] = p[m];
            }
            p = new double[p.length+1];
            for (m = 0; m < p.length-1; m++) {
                p[m+1] += -oldp[m];
                p[m] += oldp[m];
            }
            oldq = new double[q.length];
            for (m = 0; m < q.length; m++) {
                oldq[m] = q[m];
            }
            q = new double[q.length+2];
            for (m = 0; m < q.length-2; m++) {
                q[m+1] = oldq[m];    
            }
            for (m = 0; m < p.length; m++) {
                q[m] += a*p[m];
            }
        } // for (j = 1; j <= k-1; j++) 
        
        A = new double[q.length-1][q.length-1];
        for (m = 1; m < q.length-1; m++) {
            A[m][m-1] = 1.0;
        }
        for (m = 0; m < q.length -1; m++) {
            A[0][m] = -q[m+1]/q[0];
        }
        eigenvalueR = new double[q.length-1];
        eI = new double[q.length-1];
        V = new double[q.length-1][q.length-1];
        // A = V * (diagonal eigenvalues) * V'
        // In EigevalueDecomposition the columns of V represent the eigenvectors
        // Whitening matrix = v * 1/sqrt(diagonal eigenvalues) * V'
        Eigenvalue.decompose( A, V, eigenvalueR, eI );
        // Sort into ascending order
        List<EigenvalueComplex> list = new ArrayList<EigenvalueComplex>();  
        for (m = 0; m < q.length-1; m++) {
            list.add(new EigenvalueComplex(eigenvalueR[m], eI[m]));
        }
        Collections.sort(list, new EigenvalueComplexComparator());
        qtR = new double[k-1];
        qtI = new double[k-1];
        for (m = 0; m < k-1; m++) {
            qtR[m] = list.get(m).getReal();
            qtI[m] = list.get(m).getImaginary();
        }
        if (filterType == MID_PHASE) {
            if ((k % 2) == 1) {
                j = 0;
                for (m = 0; m < filterLength-2; m += 4) {
                    qtR[j] = list.get(m).getReal();
                    qtI[j++] = list.get(m).getImaginary();
                }
                for (m = 1; m < filterLength-2; m += 4) {
                    qtR[j] = list.get(m).getReal();
                    qtI[j++] = list.get(m).getImaginary();
                }
            } // if ((k % 2) == 1)
            else {
                qtR[0]= list.get(0).getReal();
                qtI[0] = list.get(0).getImaginary();
                j = 1;
                for (m = 3; m <= k-2; m += 4) {
                    qtR[j] = list.get(m).getReal();
                    qtI[j++] = list.get(m).getImaginary();
                }
                for (m = 4; m <= k-2; m += 4) {
                    qtR[j] = list.get(m).getReal();
                    qtI[j++] = list.get(m).getImaginary();
                }
                for (m = filterLength-4; m >= k-1; m -= 4) {
                    qtR[j] = list.get(m).getReal();
                    qtI[j++] = list.get(m).getImaginary();
                }
                for (m = filterLength-5; m >= k-1; m -= 4) {
                    qtR[j] = list.get(m).getReal();
                    qtI[j++] = list.get(m).getImaginary();
                }
            } // else
        } // if (filterType == MID_PHASE)
        polyqtR = new double[k];
        polyqtI = new double[k];
        polyqtR[0] = 1.0;
        polyqtI[0] = 0.0;
        for (j = 0; j <= k-2; j++) {
            for (m = j+1; m >= 1; m--) {
                polyqtR[m] = polyqtR[m] - qtR[j]*polyqtR[m-1] + qtI[j]*polyqtI[m-1];
                polyqtI[m] = polyqtI[m] - qtR[j]*polyqtI[m-1] - qtI[j]*polyqtR[m-1];
            }
        }
        // Convolve h_0 with polyqtR
        // Length of h_0 = 2 + k - 1 = k + 1
        // Length of polyqtR = k
        // Length of (h_0 * polyqtR) = k + 1 + k - 1 = 2*k = filterLength
        w = new double[filterLength];
        for (m = 0; m < filterLength; m++) {
            minj = Math.max(0, m + 1 - k);
            maxj = Math.min(m, k);
            for (j = minj; j <= maxj; j++) {
                w[m] += h_0[j]*polyqtR[m - j];
            }
        }
        sumw = 0.0;
        for (j = 0; j < filterLength; j++) {
            sumw += w[j];
        }
        // Normalize to sqrt(2)
        scale = Math.sqrt(2.0)/sumw;
        for (j = 0; j < filterLength; j++) {
            scalingFilter[j] = scale*w[j];
        }
        temp = new double[filterLength];
        if (filterType == MAXIMUM_PHASE) {
            for (j = 0; j < filterLength; j++) {
                temp[filterLength-1-j] = scalingFilter[j];
            }
            for (j = 0; j < filterLength; j++) {
                scalingFilter[j] = temp[j];
            }
        } // if (filterType == MAXIMUM_PHASE)
        sumw = 0.0;
        for (j = 0; j < filterLength ; j++) {
            sumw += scalingFilter[j]*scalingFilter[j];
        }
        if ((sumw - 1.0) > 1.0e-4) {
            displayError("Numerically unstable for this value of filterLength");
            error = -1;
        }
        for (j = 0; j < filterLength; j++) {
            waveletFilter[filterLength-1-j] = scalingFilter[j];
        }
        for (j = 0; j < filterLength; j+= 2) {
            waveletFilter[j] = -waveletFilter[j];
        }
        
        return;
    } // daubcqf()
    
    /**
     * Accessor that returns the image.
     * 
     * @return the wavelet image
     */
    public ModelImage[] getWaveletImages() {
        return waveletImage;
    }
    
    class EigenvalueComplex {
        private double er;
        private double ei;
        
        public EigenvalueComplex(double er, double ei) {
            this.er = er;
            this.ei = ei;
        }
        
        double getReal() {
            return er;
        }
        
        double getImaginary() {
            return ei;
        }
    }
    
    class EigenvalueComplexComparator implements Comparator<EigenvalueComplex> {
        public int compare(EigenvalueComplex e1, EigenvalueComplex e2) {
            if (e1.getReal() > e2.getReal()) {
                return 1;
            }
            else if (e1.getReal() < e2.getReal()) {
                return -1;
            }
            else if (e1.getImaginary() > e2.getImaginary()) {
                return 1;
            }
            else if (e1.getImaginary() < e2.getImaginary()) {
                return -1;
            }
            else {
                return 0;
            }
        }
    }
    
}