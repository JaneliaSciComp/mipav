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
    private boolean doDenoise = false;
    // actual_threshold used if value is other than zero
    private double actualThreshold = 0.0;
    private int varianceEstimator = MAD;
    // The threshold thld is computed as thld = c*MAD(noise_estimate);
    // Defaults are 3.6 for undecimated or redundant based denoising
    // and 3.0 if not decimated or redundant
    private double thresholdMultiplier = 3.6;
    // Defaults are SOFT_THRESHOLDING if not redundant and HARD_THRESHOLDING if redundant
    private int thresholdingType = HARD_THRESHOLDING;
    
    
    
    public AlgorithmRiceWaveletTools(ModelImage destImg, ModelImage srcImg, int filterLength, boolean redundant,
            int numberOfLevels, boolean doWaveletImages, int minimumLevel, int maximumLevel,
            int filterType) {
        super(destImg, srcImg);
        this.filterLength = filterLength;
        this.redundant = redundant;
        this.numberOfLevels = numberOfLevels;
        this.doWaveletImages = doWaveletImages;
        this.minimumLevel = minimumLevel;
        this.maximumLevel = maximumLevel;
        this.filterType = filterType;
    }
    
    public AlgorithmRiceWaveletTools(ModelImage srcImg, int filterLength, boolean redundant, int numberOfLevels,
            boolean doWaveletImages, int minimumLevel, int maximumLevel, int filterType) {
        super(null, srcImg);
        this.filterLength = filterLength;
        this.redundant = redundant;
        this.numberOfLevels = numberOfLevels;
        this.doWaveletImages = doWaveletImages;
        this.minimumLevel = minimumLevel;
        this.maximumLevel = maximumLevel;
        this.filterType = filterType;
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
        
        if (srcImage == null) {
            displayError("Source Image is null");

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
	            		if (actualThreshold == 0.0) {
	            			numberValues = numberOfLevels*sliceSize;
	            			tmp = new double[numberValues];
	            			for (i = 0; i < numberOfLevels; i++) {
	            				for (j = 0; j < sliceSize; j++) {
	            					tmp[i*sliceSize + j] = Math.abs(lhA[i][j]);
	            				}
	            			}
	            			if (varianceEstimator == MAD) {
	            				Arrays.sort(tmp);
	            				median = (tmp[numberValues/2] + tmp[numberValues/2 - 1])/2.0; 
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
	            		if (thresholdingType == SOFT_THRESHOLDING) {
	            			
	            		}
	            	}
	            	else {
                        if (actualThreshold == 0.0) {
                        	numberValues = numberOfLevels*sliceSize;
	            			tmp = new double[numberValues];
	            			for (i = 0; i < numberOfLevels; i++) {
	            				for (j = 0; j < sliceSize; j++) {
	            					tmp[i*sliceSize + j] = Math.abs(hhA[i][j]);
	            				}
	            			}
	            		}
	            	}
	            } // if (doDenoise)
	        }
        } // if (redundant)
        else { // !redundant
        	y = new double[sliceSize];
        	waveletImage = new ModelImage[1];
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