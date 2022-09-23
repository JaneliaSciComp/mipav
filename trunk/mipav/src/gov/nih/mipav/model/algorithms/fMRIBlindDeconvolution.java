package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.algorithms.filters.PyWavelets;
import gov.nih.mipav.model.algorithms.filters.PyWavelets.DiscreteWavelet;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameGraph;

import java.awt.Color;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.*;


public class fMRIBlindDeconvolution extends AlgorithmBase {
	// Ported from the Python pyBOLD package
	/*Copyright (c) 2018, pyBOLD
	All rights reserved.

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:

	* Redistributions of source code must retain the above copyright notice, this
	  list of conditions and the following disclaimer.

	* Redistributions in binary form must reproduce the above copyright notice,
	  this list of conditions and the following disclaimer in the documentation
	  and/or other materials provided with the distribution.

	* Neither the name of totalactivation nor the names of its
	  contributors may be used to endorse or promote products derived from
	  this software without specific prior written permission.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
	FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
	DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
	SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
	CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
	OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
	*/
	
	// Gradient code ported from _numdiff.py in SCIPY
	 /*Copyright (c) 2001-2002 Enthought, Inc. 2003-2022, SciPy Developers.
	All rights reserved.

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions
	are met:

	1. Redistributions of source code must retain the above copyright
	   notice, this list of conditions and the following disclaimer.

	2. Redistributions in binary form must reproduce the above
	   copyright notice, this list of conditions and the following
	   disclaimer in the documentation and/or other materials provided
	   with the distribution.

	3. Neither the name of the copyright holder nor the names of its
	   contributors may be used to endorse or promote products derived
	   from this software without specific prior written permission.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
	"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
	LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
	A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
	OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
	SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
	LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
	DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
	THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
	*/
	
	/**
	 * Reference:
	 * Hamza Cherkaoui, Thomas Moreau, Abderrahim Halimi, Philippe Ciuciu. Sparsity-based blind 
	 * deconvolution of neural activation signal in fMRI. IEEE-ICASSP 2019 - International Conference on
       Acoustics, Speech and Signal Processing, May 2019, Brighton, United Kingdom. ffhal-02085810v2
	 */
	
	private final double MIN_DELTA = 0.5;
    private final double MAX_DELTA = 2.0;
    
    public fMRIBlindDeconvolution() {
    	
    }


	public void runAlgorithm() {
		
	}
	
	// This module gathers useful data generation.
	// SPM canonical HRF with a time scaling parameter.
	// HRF is hemodynamic response function
	public void spm_hrf(double hrf[], double t_hrf[], double delta, double t_r,
			    double dur, boolean normalized_hrf, double dt, double p_delay,
	            double undershoot, double p_disp, double u_disp, double p_u_ratio, double onset) {
		// Default values:
		// t_r = 1.0
		// dur = 60.0
		// normalized_hrf = true
		// dt = 0.001
		// p_delay = 6.0
		// undershoot = 16.0
		// p_disp = 1.0
		// u_disp = 1.0
		// p_u_ratio = 0.167
		// onset = 0.0
		int i, j;
		double t[];
		double scaled_time_stamps[];
		double loc;
		double a;
		double am1;
		double y;
		double gama[];
		Gamma gam;
		double peak[];
		double under;
		double fhrf[];
		double maxhrf;
		int numsamples = (int)(dur/ dt);
		int step = (int)(t_r/dt);
		//int returnedsamples = ((numsamples-1)/step) + 1; length of hrf[] and t_hrf[]
		
		if ((delta < MIN_DELTA) || (delta > MAX_DELTA)) {
	        System.err.println("In spm_hrf delta should belong in range["+ MIN_DELTA+", " + MAX_DELTA+"]"); 
	        System.err.println("which correspond to a max FWHM of 10.52s and a min FWHM of 2.80s");
	        System.err.println("Supplied delta = " + delta);
	        System.exit(-1);
		}
		
		// dur: the (continuous) time segment on which we represent all
	    // the HRF. Can cut the HRF too early. The time scale is second.
		t = new double[numsamples];
		scaled_time_stamps = new double[numsamples];
		for (i = 0; i < numsamples; i++) {
			t[i] = (double)i/(numsamples - 1.0) - onset/dt;
			scaled_time_stamps[i] = delta * t[i];
		}
		a = p_delay/p_disp;
		am1 = a - 1.0;
		gama = new double[1];
		gam = new Gamma(a, gama);
		gam.run();
		loc = dt/p_disp;
		peak = new double[numsamples];
		for (i = 0; i < numsamples; i++) {
			y = scaled_time_stamps[i] - loc;
			peak[i] = Math.pow(y,am1)*Math.exp(-y)/gama[0];
		}
		a = undershoot/u_disp;
		am1 = a - 1.0;
		gam = new Gamma(a, gama);
		gam.run();
		loc = dt/u_disp;
		fhrf = new double[numsamples];
		maxhrf = -Double.MAX_VALUE;
		for (i = 0; i < numsamples; i++) {
			y = scaled_time_stamps[i] - loc;
			under = Math.pow(y,am1)*Math.exp(-y)/gama[0];
			fhrf[i] = peak[i] - p_u_ratio * under;
			maxhrf = Math.max(maxhrf, fhrf[i]);
		}
		
		if (normalized_hrf) {
			for (i = 0; i < numsamples; i++) {
				fhrf[i] = fhrf[i]/(maxhrf + 1.0e-30);
			}
		}
		
		for (i = 0, j = 0; i < numsamples; i += step) {
		    hrf[j] = fhrf[i];
		    t_hrf[j++] = t[i];
		}
		return;
	}
	
	public void test_power_2_padding_length() {
		// In test_power_2_padding_length() no errors were detected
        // Test if the output of next_power_of_2_padding is a power of two.
		int i,j;
		int N;
		int arr[] = new int[]{100, 128, 200, 256, 250, 300, 500, 600, 1000};
		int min_power_of_2 = 1024;
		int min_zero_padd = 50;
		double zero_padd_ratio = 0.5;
		int ans[] = new int[]{1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024};
		Random rand = new Random();
		double signal[];
		int numErrors = 0;
        
        for (i = 0; i < arr.length; i++) {
        	N = arr[i];
        	signal = new double[N];
        	for (j = 0; j < N; j++) {
        		signal[j] = rand.nextGaussian();
        	}
        	int p_total[] = new int[2];
            double padded_signal[] = _custom_padd(p_total, signal, min_power_of_2, min_zero_padd, zero_padd_ratio);
            if (padded_signal.length != ans[i]) {
                numErrors++;
                System.err.println("In test_power_2_padding_length() padded_signal.length = " + padded_signal.length + " instead of the correct " + ans[i]);
            }
        }
        if (numErrors > 0) {
        	System.err.println("In test_power_2_padding_length() " + numErrors + " errors were detected");
        }
        else {
        	System.out.println("In test_power_2_padding_length() no errors were detected");
        }
	}
	
	public void test_zero_padd_unpadd() {
		// In test_zero_padd_unpadd() no errors were detected
        // Test if the padded-unpadded array is equal to the orig.
		int i,j,k,m,p;
		int N;
		int arr[] = new int[]{100, 128, 200, 256, 250, 300, 500, 600, 1000};
		Random rand = new Random();
		double s[];
		int numErrors = 0;
		int padlr[] = new int[] {10,20,214};
		String paddarr[] = new String[] {"left", "right"};
		String paddtype;
		double padded_s[];
		double test_s[];
		int differences;
		int padcenter[][] = new int[][] {{5,10}, {20,20}, {214,145}};
		int parr[];
		for (i = 0; i < arr.length; i++) {
        	N = arr[i];
            s = new double[N];
            for (j = 0; j < N; j++) {
        		s[j] = rand.nextGaussian();
        	}
            for (j = 0; j < padlr.length; j++) {
                p = padlr[j];
                for (k = 0; k < paddarr.length; k++) {
                	paddtype = paddarr[k];
                    padded_s = _padd_symetric(s, p, 0.0, paddtype);
                    test_s = _unpadd_symetric(padded_s, p, paddtype);
                    differences = 0;
                    for (m = 0; m < N; m++) {
                        if (s[m] != test_s[m]) {
                            differences++;	
                        }
                    }
                    if (differences != 0) {
                    	numErrors++;
                    	System.err.println("For N = " + N + " pad = " + p + " pad type = " +paddtype + "  " + differences + " differences were found");
                    }
                } // for (k = 0; k < paddarr.length; k++) 
            } // for (j = 0; j < padlr.length; j++) 

            paddtype = "center";
            for (j = 0; j < padcenter.length; j++) {
                parr = padcenter[j];
                padded_s = _padd_assymetric(s, parr, 0.0, paddtype);
                test_s = _unpadd_assymetric(padded_s, parr, paddtype);
                differences = 0;
                for (m = 0; m < N; m++) {
                    if (s[m] != test_s[m]) {
                        differences++;	
                    }
                }
                if (differences != 0) {
                	numErrors++;
                	System.err.println("For N = " + N + " pad left = " + parr[0] + " pad right = " + parr[1] + " pad type = " +paddtype + "  " + differences + " differences were found");
                }
            } // for (j = 0; j < padcenter.length; j++)
		} // for (i = 0; i < arr.length; i++)
		if (numErrors > 0) {
        	System.err.println("In test_zero_padd_unpadd() " + numErrors + " errors were detected");
        }
        else {
        	System.out.println("In test_zero_padd_unpadd() no errors were detected");
        }
	}
	
	public void test_zero_mirror_zero_padd_unpadd() {
		// In test_zero_mirror_zero_padd_unpadd() no errors were detected
        // Test if the padded-unpadded array is equal to the orig.
		int i,j;
		int N;
		int arr[] = new int[]{100, 128, 200, 256, 250, 300, 500, 600, 900, 1000};
		int min_power_of_2 = 1024;
		int min_zero_padd = 50;
		double zero_padd_ratio = 0.5;
		Random rand = new Random();
		double signal[];
		int numErrors = 0;
		String paddtype = "center";
		double test_signal[];
		int differences;
		int m;
        
        for (i = 0; i < arr.length; i++) {
        	N = arr[i];
        	signal = new double[N];
        	for (j = 0; j < N; j++) {
        		signal[j] = rand.nextGaussian();
        	}
        	int p_total[] = new int[2];
            double padded_signal[] = _custom_padd(p_total, signal, min_power_of_2, min_zero_padd, zero_padd_ratio);
            test_signal = _unpadd_assymetric(padded_signal, p_total, paddtype);
            differences = 0;
            for (m = 0; m < N; m++) {
                if (signal[m] != test_signal[m]) {
                    differences++;	
                }
            }
            if (differences != 0) {
            	numErrors++;
            	System.err.println("For N = " + N + " pad left = " + p_total[0] + " pad right = " + p_total[1] + " pad type = " + paddtype + "  " + differences + " differences were found");
            }
        } // for (i = 0; i < arr.length; i++)
        if (numErrors > 0) {
        	System.err.println("In test_zero_mirror_zero_padd_unpadd() " + numErrors + " errors were detected");
        }
        else {
        	System.out.println("In test_zero_mirror_zero_padd_unpadd() no errors were detected");
        }
	}



	
	// This module provide the padding functions used with the Fourier
	// implementation for the HRF operator.
	public double[] _padd_symetric(double arrays[], int p, double c, String paddtype) {
	   // Helper function to c-padd in a symetric way arrays.

	    // Parameters
	    // ----------
	    // arrays : np.ndarray
	    // array to padd.

	    // p :  int,
	    //    length of padding.

	    // c : double,
	    //    value to padd with.
		// Default 0

	    // paddtype : "left", "right", "center"
	    //    where to place the padding.
		// Default "center"

	    // Results
	    // -------
	    // arrays : np.ndarray
	    // the padded array
		int p_left;
		//int p_right;
		double paddedArray[] = new double[arrays.length + p];
		int i;
		
		if (paddtype.equalsIgnoreCase("center")) {
            p_left = (int)(p / 2);
            //p_right = (int)(p / 2) + (p % 2);
            for (i = 0; i < p_left; i++) {
            	paddedArray[i] = c;
            }
            for (i = p_left; i < p_left + arrays.length; i++) {
            	paddedArray[i] = arrays[i-p_left];
            }
            for (i = p_left + arrays.length; i < arrays.length + p; i++) {
            	paddedArray[i] = c;
            }
		}
		else if (paddtype.equalsIgnoreCase("left")) {
			for (i = 0; i < p; i++) {
            	paddedArray[i] = c;
            }
            for (i = p; i < p + arrays.length; i++) {
            	paddedArray[i] = arrays[i-p];
            }
		}
		else if (paddtype.equalsIgnoreCase("right")) {
			for (i = 0; i < arrays.length; i++) {
            	paddedArray[i] = arrays[i];
            }
            for (i = arrays.length; i < arrays.length + p; i++) {
            	paddedArray[i] = c;
            }
		}
        else {
        	System.err.println("In _padd_symmetric paddtype = " + paddtype);
        	System.err.println("paddtype should be left, center, or right");
        	System.exit(-1);
        }
		return paddedArray;
	}
	
	public double[][] _padd_symetric(double arrays[][], int p, double c, String paddtype) {
		   // Helper function to c-padd in a symetric way arrays.

		    // Parameters
		    // ----------
		    // arrays : list of np.ndarray
            // list of arrays to padd.


		    // p :  int,
		    //    length of padding.

		    // c : double,
		    //    value to padd with.
		    // Default 0

		    // paddtype : "left", "right", "center"
		    //    where to place the padding.
		    // Default "center"

		    // Results
		    // -------
	        // arrays : list of np.ndarray
            // list of arrays

			int p_left;
			//int p_right;
			double paddedArray[][] = new double[arrays.length][];
			int i,j;
			for (i = 0; i < paddedArray.length; i++) {
			    paddedArray[i] = new double[arrays[i].length + p];
				if (paddtype.equalsIgnoreCase("center")) {
		            p_left = (int)(p / 2);
		            //p_right = (int)(p / 2) + (p % 2);
		            for (j = 0; j < p_left; j++) {
		            	paddedArray[i][j] = c;
		            }
		            for (j = p_left; j < p_left + arrays[i].length; j++) {
		            	paddedArray[i][j] = arrays[i][j-p_left];
		            }
		            for (j = p_left + arrays[i].length; j < arrays[i].length + p; j++) {
		            	paddedArray[i][j] = c;
		            }
				}
				else if (paddtype.equalsIgnoreCase("left")) {
					for (j = 0; j < p; j++) {
		            	paddedArray[i][j] = c;
		            }
		            for (j = p; j < p + arrays[i].length; j++) {
		            	paddedArray[i][j] = arrays[i][j-p];
		            }
				}
				else if (paddtype.equalsIgnoreCase("right")) {
					for (j = 0; j < arrays[i].length; j++) {
		            	paddedArray[i][j] = arrays[i][j];
		            }
		            for (j = arrays[i].length; j < arrays[i].length + p; j++) {
		            	paddedArray[i][j] = c;
		            }
				}
		        else {
		        	System.err.println("In _padd_symmetric paddtype = " + paddtype);
		        	System.err.println("paddtype should be left, center, or right");
		        	System.exit(-1);
		        }
			}
			return paddedArray;
		}
	
	public double[] _padd_assymetric(double arrays[], int p[], double c, String paddtype) {
	   // Helper function to c-padd in an assymetric way arrays.

	   // Parameters
	   // ----------
	   // arrays : np.ndarray
	   // array to padd.

	    // p :  int[2],
	    // length of padding.

	    // c : double,
	    //    value to padd with.
		// Default 0

	    // paddtype : ['center'],
	    //    where to place the padding.

	   // Note:
	   // -----
	   // Will raise a ValueError if paddtype is not 'center'.

	    // Results
	    // -------
	    // arrays : np.ndarray
	    // the padded array
		int i;
	   
		if (!paddtype.equalsIgnoreCase("center")) {
			System.err.println("In _padd_asymmetric paddtype = " + paddtype);
        	System.err.println("paddtype should center");
        	System.exit(-1);	
		}

        double paddedArray[] = new double[arrays.length + p[0] + p[1]];
        for (i = 0; i < p[0]; i++) {
        	paddedArray[i] = c;
        }
        for (i = p[0]; i < p[0] + arrays.length; i++) {
        	paddedArray[i] = arrays[i-p[0]];
        }
        for (i = p[0] + arrays.length; i < arrays.length + p[0] + p[1]; i++) {
        	paddedArray[i] = c;
        }
        return paddedArray;
	}
	
	public double[][] _padd_assymetric(double arrays[][], int p[], double c, String paddtype) {
		   // Helper function to c-padd in an assymetric way arrays.

		   // Parameters
		   // ----------
		   // arrays : np.ndarray or list of np.ndarray
           // list of arrays to padd.


		    // p :  int[2],
		    // length of padding.

		    // c : double,
		    //    value to padd with.
		    // Default 0

		    // paddtype : ['center'],
		    //    where to place the padding.

		   // Note:
		   // -----
		   // Will raise a ValueError if paddtype is not 'center'.

		    // Results
		    // -------
		    // arrays : list of np.ndarray
	        // list of arrays

			int i,j;
		   
			if (!paddtype.equalsIgnoreCase("center")) {
				System.err.println("In _padd_asymmetric paddtype = " + paddtype);
	        	System.err.println("paddtype should center");
	        	System.exit(-1);	
			}

	        double paddedArray[][] = new double[arrays.length][];
	        for (i = 0; i < paddedArray.length; i++) {
			    paddedArray[i] = new double[arrays[i].length + p[0] + p[1]];
		        for (j = 0; j < p[0]; j++) {
		        	paddedArray[i][j] = c;
		        }
		        for (j = p[0]; j < p[0] + arrays[i].length; j++) {
		        	paddedArray[i][j] = arrays[i][j-p[0]];
		        }
		        for (j = p[0] + arrays[i].length; j < arrays[i].length + p[0] + p[1]; j++) {
		        	paddedArray[i][j] = c;
		        }
	        }
	        return paddedArray;
		}
	
	public double[] _unpadd_symetric(double arrays[], int p, String paddtype) {
	    // Helper function to unpadd in an assymetric way arrays.

	    // Parameters
	    // ----------
	    // arrays : np.ndarray
	    // array to padd.

	    // p :  int,
	    //    length of padding.

	    // paddtype : left, right, or center,
	    //    where to place the padding.
		// Default center

	    // Results
	    // -------
	    // arrays : np.ndarray
	    // the unpadded array.
		
		int p_left;
		int p_right;
		int i;
		double unpaddedArray[] = new double[arrays.length-p];

		if (paddtype.equalsIgnoreCase("center")) {
            p_left = (int)(p / 2);
            p_right = (int)(p / 2) + (p % 2);
            for (i = p_left; i < arrays.length - p_right; i++) {
            	unpaddedArray[i-p_left] = arrays[i];
            }
		}
        else if (paddtype.equalsIgnoreCase("left")) {
        	for (i = p; i < arrays.length; i++) {
        		unpaddedArray[i-p] = arrays[i];
        	}
        }
        else if (paddtype.equalsIgnoreCase("right")) {
        	for (i = 0; i < arrays.length-p; i++) {
        		unpaddedArray[i] = arrays[i];
        	}
        }
        
        else {
        	System.err.println("In _unpadd_symmetric paddtype = " + paddtype);
        	System.err.println("paddtype should be left, center, or right");
        	System.exit(-1);
        }
        return unpaddedArray;
	}
	
	public double[][] _unpadd_symetric(double arrays[][], int p, String paddtype) {
	    // Helper function to unpadd in an assymetric way arrays.

	    // Parameters
	    // ----------
		// arrays : list of np.ndarray
        // list of arrays to padd.


	    // p :  int,
	    //    length of padding.

	    // paddtype : left, right, or center,
	    //    where to place the padding.
		// Default center

	    // Results
	    // -------
		// arrays : list of np.ndarray
        // the list of arrays.

		
		int p_left;
		int p_right;
		int i,j;
		double unpaddedArray[][] = new double[arrays.length][];
		for (i = 0; i < arrays.length; i++) {
            unpaddedArray[i] = new double[arrays[i].length-p];
			if (paddtype.equalsIgnoreCase("center")) {
	            p_left = (int)(p / 2);
	            p_right = (int)(p / 2) + (p % 2);
	            for (j = p_left; j < arrays[i].length - p_right; j++) {
	            	unpaddedArray[i][j-p_left] = arrays[i][j];
	            }
			}
	        else if (paddtype.equalsIgnoreCase("left")) {
	        	for (j = p; j < arrays[i].length; j++) {
	        		unpaddedArray[i][j-p] = arrays[i][j];
	        	}
	        }
	        else if (paddtype.equalsIgnoreCase("right")) {
	        	for (j = 0; j < arrays[i].length-p; j++) {
	        		unpaddedArray[i][j] = arrays[i][j];
	        	}
	        }
	        
	        else {
	        	System.err.println("In _unpadd_symmetric paddtype = " + paddtype);
	        	System.err.println("paddtype should be left, center, or right");
	        	System.exit(-1);
	        }
		}
        return unpaddedArray;
	}
	
	public double[] _unpadd_assymetric(double arrays[], int p[], String paddtype) {
	    // Helper function to unpadd in assymetric way arrays.

	    // Parameters
	    // ----------
	    // arrays : np.ndarray
	    // array to padd.

	    // p :  int[2]
	    //    length of padding.

	    // paddtype : ['center'],
	    //    where to place the padding.

	    // Note:
	    // -----
	    // Will raise a ValueError if paddtype is not 'center'.

	    // Results
	    // -------
	    // arrays : np.ndarray
	    //    the unpadded array.
		int i;
		   
		if (!paddtype.equalsIgnoreCase("center")) {
			System.err.println("In _unpadd_asymmetric paddtype = " + paddtype);
        	System.err.println("paddtype should center");
        	System.exit(-1);	
		}
		int unpaddedLength = arrays.length-p[0]-p[1];
		double unpaddedArray[] = new double[unpaddedLength];
		for (i = p[0]; i < p[0] + unpaddedLength; i++) {
			unpaddedArray[i-p[0]] = arrays[i];
		}
		return unpaddedArray;
	}
	
	public double[][] _unpadd_assymetric(double arrays[][], int p[], String paddtype) {
	    // Helper function to unpadd in assymetric way arrays.

	    // Parameters
	    // ----------
		// arrays : list of np.ndarray
        // list of arrays to padd.


	    // p :  int[2]
	    //    length of padding.

	    // paddtype : ['center'],
	    //    where to place the padding.

	    // Note:
	    // -----
	    // Will raise a ValueError if paddtype is not 'center'.

	    // Results
	    // -------
		// arrays : list of np.ndarray
        // list of arrays.

		int i,j;
		   
		if (!paddtype.equalsIgnoreCase("center")) {
			System.err.println("In _unpadd_asymmetric paddtype = " + paddtype);
        	System.err.println("paddtype should center");
        	System.exit(-1);	
		}

		double unpaddedArray[][] = new double[arrays.length][];
		for (i = 0; i < arrays.length; i++) {
			int unpaddedLength = arrays[i].length-p[0]-p[1];
			unpaddedArray[i] = new double[unpaddedLength];
			for (j = p[0]; j < p[0] + unpaddedLength; j++) {
				unpaddedArray[i][j-p[0]] = arrays[i][j];
			}
		}
		return unpaddedArray;
	}
	
	public double log2(double x) {
		return (Math.log(x)/Math.log(2.0));
	}
	
	public double[] _custom_padd(int p_total[], double a[], int min_power_of_2, int min_zero_padd,
		            double zero_padd_ratio) {
		// Private helper to make a zeros-mirror-zeros padd to the next power of
		// two of a.
		
		// Parameters
		// ----------
		// arrays : np.ndarray,
		//   array to padd.
		
		// min_power_of_2 : int (default=1024),
		//   min length (power of two) for the padded array.
		   
		// min_zero_padd : int (default=50)
		//   min zero padd, either for the first or the second zero-padd.
		
		// zero_padd_ratio : float (default=0.5),
		//   determine the ratio of the length of zero padds (either for the first
		//   or the second zero-padd) w.r.t the array length.
		
		
		
		// Note:
		// -----
		// Having a signal close to ~200 can make trouble.
		
		// Results
		// -------
		// arrays : np.ndarray
		//   the padded array.
		
		// p_total int[2]
		//   the applied padd.
		double divby2;
		int nextpow2;
		int diff;
		int zero_padd_len;
		boolean too_short;
		int p_zeros[];
		int len_padd_left;
		int len_padd_right;
		double paddeda[];
		int len_reflect_padd_left;
		int len_reflect_padd_right;
		int p_reflect[];
		double reflecta[];
		int i,j;
		
	
	    divby2 = min_power_of_2;
	    while (divby2 > 1.0) {
	    	divby2 = divby2/2.0;
	    }
	    if (divby2 != 1.0) {
	    	System.err.println("In _custom_padd min_power_of_2 should be a power of two");
	    	System.err.println("min_power_of_2 = " + min_power_of_2);
	    	System.exit(-1);
	    }
		
		nextpow2 = (int)(Math.pow(2, Math.ceil(log2(a.length))));
		if (nextpow2 < min_power_of_2) {
			nextpow2 = min_power_of_2;
		}
		
		diff = nextpow2 - a.length;
		
		// define the three possible padding
		zero_padd_len = (int)(zero_padd_ratio * a.length);
		
		too_short = zero_padd_len < min_zero_padd;
		if (too_short) {
			zero_padd_len = min_zero_padd;
		}
		p_zeros = new int[] {zero_padd_len, zero_padd_len};
		
		len_padd_left = (diff / 2);
		len_padd_right = (diff / 2) + (a.length % 2);
		p_total[0] = len_padd_left;
		p_total[1] = len_padd_right;
		
		if (diff == 0) {
		   // [ s ]
		
		   p_total[0] = 0;
		   p_total[1] = 0;
		
		   return a;
		}
		
		else if ((0 < diff) && (diff < 2 * zero_padd_len)) {
		   // [ /zeros | s | zeros/ ]
		   if (p_total[0] == p_total[1]) {
			   paddeda = _padd_symetric(a, p_total[0] + p_total[1], 0.0, "center");
		   }
		   else {
			   paddeda = _padd_assymetric(a, p_total, 0.0, "center");
		   }
		
		   return paddeda;
		}
		
		else if ((2 * zero_padd_len < diff) && (diff < 4 * zero_padd_len)) {
			System.out.println("Section 3");
		   // [ zeros | mirror-signal | s | mirror-signal | zeros ]
		
		   len_reflect_padd_left = len_padd_left - zero_padd_len;
		   len_reflect_padd_right = len_padd_right - zero_padd_len;
		   p_reflect = new int[] {len_reflect_padd_left, len_reflect_padd_right};
		
		   // padding
		   reflecta = new double[p_reflect[0] + a.length + p_reflect[1]];
		   for (i = p_reflect[0]; i < p_reflect[0] + a.length; i++) {
			   reflecta[i] = a[i - p_reflect[0]];
		   }
		   for (i = p_reflect[0] - 1, j = 0; i >= 0; i--, j++) {
			   reflecta[i] = reflecta[p_reflect[0]+1+j];
		   }
		   for (i = p_reflect[0] + a.length, j = 0; i < p_reflect[0] + a.length + p_reflect[1]; i++, j++) {
			   reflecta[i] = reflecta[p_reflect[0] + a.length - 2 - j];
		   }
		   paddeda = _padd_symetric(reflecta, p_zeros[0] + p_zeros[1], 0.0, "center");
		
		   return paddeda;
		}
		
		else {
		   // [ zeros | mirror-signal | zeros | s | zeros | mirror-signal | zeros ]
		
		   len_reflect_padd_left = len_padd_left - 2 * zero_padd_len;
		   len_reflect_padd_right = len_padd_right - 2 * zero_padd_len;
		   p_reflect = new int[] {len_reflect_padd_left, len_reflect_padd_right};
		
		   // padding
		   paddeda = _padd_symetric(a, p_zeros[0] + p_zeros[1], 0.0, "center");
		   reflecta = new double[p_reflect[0] + paddeda.length + p_reflect[1]];
		   for (i = p_reflect[0]; i < p_reflect[0] + paddeda.length; i++) {
			   reflecta[i] = paddeda[i - p_reflect[0]];
		   }
		   for (i = p_reflect[0] - 1, j = 0; i >= 0; i--, j++) {
			   reflecta[i] = reflecta[p_reflect[0]+1+j];
		   }
		   for (i = p_reflect[0] + paddeda.length, j = 0; i < p_reflect[0] + paddeda.length + p_reflect[1]; i++, j++) {
			   reflecta[i] = reflecta[p_reflect[0] + paddeda.length - 2 - j];
		   }
		   paddeda = _padd_symetric(reflecta, p_zeros[0] + p_zeros[1], 0.0, "center");
		
		   return paddeda;
		}
    }
	

    public double[][] custom_padd(int padd[], double arrays[][], int min_power_of_2, int min_zero_padd,
                double zero_padd_ratio) {
        // Zeros-mirror-zeros padding function to the next power of two of arrays.

	    // Parameters
	    // ----------
	    // arrays :list of np.ndarray,
	       // list of arrays to padd.
	
	    // min_power_of_2 : int (default=1024),
	    //     min length (power of two) for the padded array.
	        
	    // min_zero_padd : int (default=50)
	    //    min zero padd, either for the first or the second zero-padd.
	
	
	    // zero_padd_ratio : float (default=0.5),
	    //    determine the ratio of the length of zero padds (either for the first
	    //    or the second zero-padd) w.r.t the array length.
	
	    // Note:
	    // -----
	    // Having a signal close to ~200 can make trouble.
	
	    // Results
	    // ------
	    // padd_arrays : list of np.ndarray
	       // list of arrays.
	
	    // padd : int[2]
	    //    the applied padd (might not be the same for all the arrays).
	    int i;
        _custom_padd(padd, arrays[0],
                               min_power_of_2,
                               min_zero_padd,
                               zero_padd_ratio);
        double padd_arrays[][] = new double[arrays.length][];
        for (i = 0; i < arrays.length; i++) {
            padd_arrays[i] = _custom_padd(padd, arrays[i],
                                    min_power_of_2,
                                    min_zero_padd,
                                    zero_padd_ratio);
        }
	    return padd_arrays;
    }
    
    
    public double[][] rfft(double x[]) {
    	// reals in FFTUtility requires an even number of input points
    	// so if x has an odd number of points add on an extra zero point
    	int i,j;
    	int xlen = x.length;
    	int outlen;
    	if ((xlen % 2) == 0) {
    		outlen = (xlen/2) + 1;
    	}
    	else {
    		outlen = (xlen + 1)/2 + 1;
    	}
    	double output[][] = new double[2][outlen];
    	for (i = 0, j = 0; i < outlen -1; i++) {
    		output[0][i] = x[j++];
    		if (j < xlen) {
    		    output[1][i] = x[j++];
    		}
    	}
    	FFTUtility fft = new FFTUtility(output[0],output[1],1,outlen-1,1,-1,FFTUtility.FFT);
    	fft.setShowProgress(false);
    	fft.run();
    	fft = new FFTUtility(output[0],output[1],1,outlen-1,1,-1,FFTUtility.REALS);
    	fft.setShowProgress(false);
        fft.run();
        return output;
    }
    
    public double[] irfft(double x[][], boolean isOdd) {
    	int i, j = 0;
    	int n = x[0].length-1;
    	FFTUtility fft = new FFTUtility(x[0],x[1],1,n,1,1,FFTUtility.REALS);
    	fft.setShowProgress(false);
        fft.run();	
        fft = new FFTUtility(x[0],x[1],1,n,1,1,FFTUtility.FFT);
        fft.setShowProgress(false);
        fft.run();
        double output[];
        if (isOdd) {
        	output = new double[2*n-1];
        }
        else {
        	output = new double[2*n];
        }
        for (i = 0, j = 0; i < n; i++) {
        	output[j++] = x[0][i];
        	if (j < output.length) {
        		output[j++] = x[1][i];
        	}
        }
        return output;
    }
	    
    public double[] spectral_convolve(double k[], double xin[]) {
        // Return k.conv(x).

        // Parameters:
        // -----------
        // k : 1d np.ndarray,
        //    kernel.
        // x : 1d np.ndarray,
        //    signal.

        // Results:
        // --------
        // k_conv_x : 1d np.ndarray,
        //    the convolved signal.
    	int i;
    	double x[] = new double[xin.length];
    	for (i = 0; i < x.length; i++) {
    		x[i] = xin[i];
    	}
        int p[] = new int[2];
        // Smallest power of 2 >= k.length + xin.length -1 for padding on one side
        // So use that 2 times that amount to cover both sides for center padding
        int minpowerof2 = 2;
        int minrequiredlength = 2*(k.length + xin.length - 1);
        while (minpowerof2 < minrequiredlength) {
        	minpowerof2 *= 2;
        }
        x = _custom_padd(p, x, minpowerof2, 50, 0.5);
        int N = x.length;
        boolean isOdd = ((N % 2) == 1);
        double kAdj[] = new double[N];
        for (i = 0; i < Math.min(N, k.length); i++) {
        	kAdj[i] = k[i];
        }
        double fft_k[][] = rfft(kAdj);
        double fft_x[][] = rfft(x);
        double fft_xk[][] = new double[2][fft_k[0].length];
        for (i = 0; i < fft_k[0].length; i++) {
        	fft_xk[0][i] = fft_k[0][i]*fft_x[0][i] - fft_k[1][i]*fft_x[1][i];
        	fft_xk[1][i] = fft_k[0][i]*fft_x[1][i] + fft_k[1][i]*fft_x[0][i];
        }
        double padded_h_conv_x[] = irfft(fft_xk, isOdd);
        double k_conv_x[];
        int ptotal = p[0] + p[1];
        if ((ptotal) == 0) {
            k_conv_x = padded_h_conv_x;	
        }
        else if ((p[0] == ptotal/2) && (p[1] == (ptotal/2 + (ptotal % 2)))) {
            k_conv_x = _unpadd_symetric(padded_h_conv_x, ptotal, "center");	
        }
        else {
        	k_conv_x = _unpadd_assymetric(padded_h_conv_x, p, "center");
        }

        return k_conv_x;
    }
    
    public double[] spectral_retro_convolve(double k[], double xin[]) {
        // Return k_t.conv(x).

        // Parameters:
        // -----------
        // k : 1d np.ndarray,
        //    kernel.
        // x : 1d np.ndarray,
        //    signal.

        // Results:
        // --------
        // k_conv_x : 1d np.ndarray,
        //     the convolved signal.
    	int i;
    	double x[] = new double[xin.length];
    	for (i = 0; i < x.length; i++) {
    		x[i] = xin[i];
    	}
        int p[] = new int[2];
        // Smallest power of 2 >= k.length + xin.length -1 for padding on one side
        // So use that 2 times that amount to cover both sides for center padding
        int minpowerof2 = 2;
        int minrequiredlength = 2*(k.length + xin.length - 1);
        while (minpowerof2 < minrequiredlength) {
        	minpowerof2 *= 2;
        }
        x = _custom_padd(p, x, minpowerof2, 50, 0.5);
        int N = x.length;
        double kAdj[] = new double[N];
        for (i = 0; i < Math.min(N, k.length); i++) {
        	kAdj[i] = k[i];
        }
        double fft_k[][] = rfft(kAdj);
        for (i = 0; i < fft_k[1].length; i++) {
        	fft_k[1][i] = -fft_k[1][i];
        }
        double fft_x[][] = rfft(x);
        double fft_xk[][] = new double[2][fft_k[0].length];
        for (i = 0; i < fft_k[0].length; i++) {
        	fft_xk[0][i] = fft_k[0][i]*fft_x[0][i] - fft_k[1][i]*fft_x[1][i];
        	fft_xk[1][i] = fft_k[0][i]*fft_x[1][i] + fft_k[1][i]*fft_x[0][i];
        }
        boolean isOdd = ((N % 2) == 1);
        double padded_h_conv_x[] = irfft(fft_xk, isOdd);
        double k_conv_x[];
        int ptotal = p[0] + p[1];
        if ((ptotal) == 0) {
            k_conv_x = padded_h_conv_x;	
        }
        else if ((p[0] == ptotal/2) && (p[1] == (ptotal/2 + (ptotal % 2)))) {
            k_conv_x = _unpadd_symetric(padded_h_conv_x, ptotal, "center");	
        }
        else {
        	k_conv_x = _unpadd_assymetric(padded_h_conv_x, p, "center");
        }

        return k_conv_x;
    }
    
    /**
     * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
     * 
     * @param zr double
     * @param zi double
     * 
     * @return double
     */
    private double zabs(final double zr, final double zi) {
        double u, v, q, s;
        u = Math.abs(zr);
        v = Math.abs(zi);
        s = u + v;

        // s * 1.0 makes an unnormalized underflow on CDC machines into a true
        // floating zero
        s = s * 1.0;

        if (s == 0.0) {
            return 0.0;
        } else if (u > v) {
            q = v / u;

            return (u * Math.sqrt(1.0 + (q * q)));
        } else {
            q = u / v;

            return (v * Math.sqrt(1.0 + (q * q)));
        }
    }
    
    /**
     * complex divide c = a/b.
     * 
     * @param ar double
     * @param ai double
     * @param br double
     * @param bi double
     * @param cr double[]
     * @param ci double[]
     */
    private void zdiv(final double ar, final double ai, final double br, final double bi, final double[] cr,
            final double[] ci) {
        double bm, cc, cd, ca, cb;

        bm = 1.0 / zabs(br, bi);
        cc = br * bm;
        cd = bi * bm;
        ca = ( (ar * cc) + (ai * cd)) * bm;
        cb = ( (ai * cc) - (ar * cd)) * bm;
        cr[0] = ca;
        ci[0] = cb;

        return;
    }

    public double[] spectral_deconvolve(double k[], double x[]) {
        // Return k.conv(x).

        // Parameters:
        // -----------
        // k : 1d np.ndarray,
        //    kernel.
        // x : 1d np.ndarray,
        //    signal.

        // Results:
        // --------
        // k_conv_x : 1d np.ndarray,
        //     the convolved signal.
    	int i;
        int p[] = new int[2];
        x = _custom_padd(p, x, 1024, 50, 0.5);
        int N = x.length;
        double kAdj[] = new double[N];
        for (i = 0; i < Math.min(N, k.length); i++) {
        	kAdj[i] = k[i];
        }
        double fft_kinv[][] = rfft(kAdj);
        double fft_k[][] = new double[2][fft_kinv[0].length];
        double cr[] = new double[1];
        double ci[] = new double[1];
        for (i = 0; i < fft_k[0].length; i++) {
            zdiv(1.0, 0.0, fft_kinv[0][i], fft_kinv[1][i], cr, ci);
            fft_k[0][i] = cr[0];
            fft_k[1][i] = ci[0];
        }
        double fft_x[][] = rfft(x);
        double fft_xk[][] = new double[2][fft_k[0].length];
        for (i = 0; i < fft_k[0].length; i++) {
        	fft_xk[0][i] = fft_k[0][i]*fft_x[0][i] - fft_k[1][i]*fft_x[1][i];
        	fft_xk[1][i] = fft_k[0][i]*fft_x[1][i] + fft_k[1][i]*fft_x[0][i];
        }
        boolean isOdd = ((N % 2) == 1);
        double padded_h_conv_x[] = irfft(fft_xk, isOdd);
        double k_conv_x[];
        int ptotal = p[0] + p[1];
        if ((ptotal) == 0) {
            k_conv_x = padded_h_conv_x;	
        }
        else if ((p[0] == ptotal/2) && (p[1] == (ptotal/2 + (ptotal % 2)))) {
            k_conv_x = _unpadd_symetric(padded_h_conv_x, ptotal, "center");	
        }
        else {
        	k_conv_x = _unpadd_assymetric(padded_h_conv_x, p, "center");
        }

        return k_conv_x;
    }
    
    public double[] spectral_retro_deconvolve(double k[], double x[]) {
        // Return k.conv(x).

        // Parameters:
        // -----------
        // k : 1d np.ndarray,
        //     kernel.
        // x : 1d np.ndarray,
        //    signal.

        // Results:
        // --------
        // k_conv_x : 1d np.ndarray,
        //    the convolved signal.
    	int i;
        int p[] = new int[2];
        x = _custom_padd(p, x, 1024, 50, 0.5);
        int N = x.length;
        double kAdj[] = new double[N];
        for (i = 0; i < Math.min(N, k.length); i++) {
        	kAdj[i] = k[i];
        }
        double fft_kinv[][] = rfft(kAdj);
        double fft_k[][] = new double[2][fft_kinv[0].length];
        double cr[] = new double[1];
        double ci[] = new double[1];
        for (i = 0; i < fft_k[0].length; i++) {
            zdiv(1.0, 0.0, fft_kinv[0][i], fft_kinv[1][i], cr, ci);
            fft_k[0][i] = cr[0];
            fft_k[1][i] = ci[0];
        }
        for (i = 0; i < fft_k[1].length; i++) {
        	fft_k[1][i] = -fft_k[1][i];
        }
        double fft_x[][] = rfft(x);
        double fft_xk[][] = new double[2][fft_k[0].length];
        for (i = 0; i < fft_k[0].length; i++) {
        	fft_xk[0][i] = fft_k[0][i]*fft_x[0][i] - fft_k[1][i]*fft_x[1][i];
        	fft_xk[1][i] = fft_k[0][i]*fft_x[1][i] + fft_k[1][i]*fft_x[0][i];
        }
        boolean isOdd = ((N % 2) == 1);
        double padded_h_conv_x[] = irfft(fft_xk, isOdd);
        double k_conv_x[];
        int ptotal = p[0] + p[1];
        if ((ptotal) == 0) {
            k_conv_x = padded_h_conv_x;	
        }
        else if ((p[0] == ptotal/2) && (p[1] == (ptotal/2 + (ptotal % 2)))) {
            k_conv_x = _unpadd_symetric(padded_h_conv_x, ptotal, "center");	
        }
        else {
        	k_conv_x = _unpadd_assymetric(padded_h_conv_x, p, "center");
        }

        return k_conv_x;
    }

    public double[][] toeplitz_from_kernel(double k[], int dim_in, int dim_out) {
        // Return the Toeplitz matrix that correspond to k.conv(.).

        // Parameters:
        // -----------
        // k : 1d np.ndarray,
        //     kernel.
        // dim_in : int,
        //     dimension of the input vector (dim of x in y = k.conv(x)).
        // dim_out : int (default None),
        //    dimension of the ouput vector (dim of y in y = in k.conv(x)). If None
        //    dim_out = dim_in.

        // Results:
        // --------
        // K : 2d np.ndarray,
        //    the Toeplitz matrix corresponding to the convolution specified.
       
        // if dim_out is None:
        //    dim_out = dim_in
        int i,j;
        int start_idx;
        double padded_k[] = new double[2*dim_in + k.length];
        for (i = dim_in; i < dim_in + k.length; i++) {
        	padded_k[i] = k[k.length - 1 - (i - dim_in)];
        }
        double K[][] = new double[dim_out][dim_in];
        for (i = 0; i < dim_out; i++) {
            start_idx = (dim_in + k.length - 1) - i;
            for (j= 0; j < dim_in; j++) {
                K[i][j] = padded_k[start_idx + j];
            }
        }

        return K;
    }
    
    public double[] simple_convolve(double k[], double x[], int dim_out) {
        // Return k.conv(x).

        // Parameters:
        // -----------
        // k : 1d np.ndarray,
        //    kernel.
        // x : 1d np.ndarray,
        //    signal.
        // dim_out : int (default None),
        //    dimension of the ouput vector (dim of y in y = in k.conv(x)). If None
        //    d = len(x).

        // Results:
        // --------
        // k_conv_x : 1d np.ndarray,
        //    the convolved signal.
       
        // if dim_out is None:
        //    dim_out = len(x)
    	int i,j;
    	int dim_in;
    	double k_conv_x[];
    	int start_idx;
        dim_in = x.length;
 
        double padded_k[] = new double[2*dim_in + k.length];
        for (i = dim_in; i < dim_in + k.length; i++) {
        	padded_k[i] = k[k.length - 1 - (i - dim_in)];
        }
        k_conv_x = new double[dim_out];

        for (i = 0; i < dim_out; i++) {
            start_idx = (dim_in + k.length - 1) - i;
            for (j = 0; j < dim_in; j++) {
                k_conv_x[i] += (padded_k[start_idx + j] * x[j]);
            }
        }

        return k_conv_x;
    }

    public double[] simple_retro_convolve(double k[], double x[], int dim_out) {
        // Return k_t.conv(x).

        // Parameters:
        // -----------
        // k : 1d np.ndarray,
        //    kernel
        // x : 1d np.ndarray,
        //    signal.
        // dim_out : int (default None),
        //    dimension of the ouput vector (dim of y in y = in k.conv(x)). If None
        //    d = len(x).

        // Results:
        // --------
        // k_conv_x : 1d np.ndarray,
        //    the convolved signal.
       
        // if dim_out is None:
        //    dim_out = len(x)
    	int i,j;
    	int dim_in;
    	double k_conv_x[];
    	int start_idx;
        dim_in = x.length;
 
        double padded_k[] = new double[2*dim_out - 2 + k.length];
        for (i = dim_out-1; i < dim_out -1 + k.length; i++) {
        	padded_k[i] = k[i - (dim_out-1)];
        }
        k_conv_x = new double[dim_out];
        
        for (i = 0; i < dim_out; i++) {
        	start_idx = dim_out - 1 - i;
            for (j = 0; j < dim_in; j++) {
                k_conv_x[i] += (padded_k[start_idx + j] * x[j]);
            }
        }

        return k_conv_x;
    }
    
    public double median(double x[]) {
    	int i;
    	double med;
    	double xsort[] = new double[x.length];
    	for (i = 0; i < x.length; i++) {
    		xsort[i] = x[i];
    	}
    	Arrays.sort(xsort);
    	if ((x.length % 2) == 1) {
    	    med = xsort[(x.length-1)/2];	
    	}
    	else {
    		med = (xsort[x.length/2] + xsort[(x.length/2)-1])/2.0;
    	}
    	return med;
    }
    
    public double mad(double x[], double c) {
        // Median absolute deviation.
        // Default c = 0.6744
    	int i;
    	double absxm[] = new double[x.length];
    	double medx = median(x);
    	for (i = 0; i < x.length; i++) {
    		absxm[i] = Math.abs(x[i] - medx);
    	}
        return median(absxm) / c;
    }


    public double mad_daub_noise_est(double x[], double c) {
        // Estimate the statistical dispersion of the noise with Median Absolute
        // Deviation on the first order detail coefficients of the 1d-Daubechies
        // wavelets transform.
    	// Default c = 0.6744
    	PyWavelets py = new PyWavelets();
    	PyWavelets.WAVELET_NAME wavelet_name = PyWavelets.WAVELET_NAME.DB;
    	int wavelet_order = 3;
		DiscreteWavelet w  = py.discrete_wavelet(wavelet_name, wavelet_order);
		short max_level = py.dwt_max_level(x.length, w.dec_len);
		PyWavelets.MODE mode = PyWavelets.MODE.MODE_SYMMETRIC;
		double coeffs[][] = py.wavedec(x, w, mode, Math.min(1, max_level));
		double cD[] = coeffs[coeffs.length-1];
        return mad(cD, c);
    }

    public double fwhm(double t_hrf[], double hrf[], int k) {
        // Return the full width at half maximum.

        // Parameters:
        // -----------
        // t_hrf : 1d np.ndarray,
        //    the sampling od time.

        // hrf : 1d np.ndarray,
        //    the HRF.

        // k : int (default=3),
        //    the degree of spline to fit the HRF.

        // Return:
        // -------
        // fwhm : float,
        //     the FWHM
    	int i;
        double maxhrf = -Double.MAX_VALUE;
        for (i = 0; i < hrf.length; i++) {
        	if (hrf[i] > maxhrf) {
        		maxhrf = hrf[i];
        	}
        }
        
        double half_max = maxhrf / 2.0;
        int iopt = 0;
        int m = t_hrf.length;
        double x[] = t_hrf;
        double y[] = new double[hrf.length];
        for (i = 0; i  < y.length; i++) {
        	y[i] = hrf[i] - half_max;
        }
        double w[] = new double[hrf.length];
        for (i = 0; i < hrf.length; i++) {
        	w[i] = 1.0;
        }
        double xb = x[0];
        double xe = x[x.length-1];
        double s = 0.0;
        int nest = m + k + 1;
        int n[] = new int[1];
        double t[] = new double[nest];
        double c[] = new double[nest];
        double fp[] = new double[1];
        int lwrk = nest*(3*k + 7) + m*(k + 1);
        int iwrk[] = new int[nest];
        int ier[] = new int[1];
        curfit cur = new curfit(iopt, m, x, y, w, xb, xe, k, s, nest, n,
        		t, c, fp, lwrk, iwrk, ier);
        cur.run();
        if (ier[0] == -1) {
        	Preferences.debug("Normal return from curfit\n", Preferences.DEBUG_ALGORITHM);
        	Preferences.debug("The spline returned is an interpolating spline (fp[0] = 0)\n",
        			Preferences.DEBUG_ALGORITHM);
        }
        else if (ier[0] == 1) {
        	System.err.println("In curfit the required storage space exceeds the available storage space");
        	System.exit(-1);
        }
        else if (ier[0] == 10) {
        	System.err.println("In curfit the input data are invalid");
        	System.exit(-1);
        }
        int mest = 10;
        double zero[] = new double[mest];
        int marr[] = new int[1];
        sproot spr = new sproot(t,n[0],c,zero,mest,marr,ier);
        spr.run();
        if (ier[0] == 0) {
        	Preferences.debug("Normal return from curfit\n", Preferences.DEBUG_ALGORITHM);	
        }
        else if (ier[0] == 1) {
        	System.err.println("In sproot the number of zeros exceeds mest");
        	System.exit(-1);
        }
        else if (ier[0] == 10) {
        	System.err.println("In sproot the input data is invalid");
        	System.exit(-1);
        }
        if (marr[0] == 0) {
        	System.err.println("sproot did not return any zeros");
        	return -1.0;
        }
        else if (marr[0] == 1) {
        	System.err.println("sproot only returned 1 zero at " + zero[0]);
        	return -1.0;
        }
        else {
        	return Math.abs(zero[1] - zero[0]);
        }
    }
    
    public double tp(double t_hrf[], double hrf[]) {
        // Return time to peak oh the signal.
        
    	int i;
    	double maxhrf = -Double.MAX_VALUE;
    	int maxind = -1;
    	for (i = 0; i < hrf.length; i++) {
    		if (hrf[i] > maxhrf) {
    			maxhrf = hrf[i];
    			maxind = i;
    		}
    	}
    	return t_hrf[maxind];
    }

    public Random random_generator(long random_state) {
        // Return a random instance with a fix seed if random_state is a int.
        Random ran = new Random(random_state);
        return ran;
    }
    
    
    public double spectral_radius_est(ConvAndLinear L, int x_shape, int nb_iter,
    		double tol, boolean verbose) {
        // EStimation of the spectral radius of the operator L.
        // int nb_iter default = 30
    	// double tol default = 1.0E-6
        // boolean verbose default = false;
    	int i,j;
    	Random r = new Random();
    	double x_old[] = new double[x_shape];
    	for (i = 0; i < x_shape; i++) {
    		x_old[i] = r.nextGaussian();
    	}

        boolean stopped = false;
        double x_new_num[];
        double x_new_denom;
        double x_new[] = null;
        for (i = 0; i < nb_iter; i++) {
        	x_new_num = L.adj(L.op(x_old));
        	x_new_denom = norm(x_old);
        	x_new = new double[x_new_num.length];
        	for (j = 0; j < x_new.length; j++) {
        		x_new[j] = x_new_num[j]/x_new_denom;
        	}
            if(Math.abs(norm(x_new) - x_new_denom) < tol) {
                stopped = true;
                break;
            }
            for (j = 0; j < x_shape; j++) {
                x_old[j] = x_new[j];
            }
        } // for (i = 0; i < nb_iter; i++)
        if ((!stopped) && verbose) {
            System.out.println("Spectral radius estimation did not converge");
        }

        return norm(x_new);
    }
    
    public void test_inf_norm() {
    	// In test_inf_norm() no errors were detected
        // Test (max - min) of the min-max-normalized array is 1.0.
    	int i,j;
		int N;
		int arr[] = new int[]{100, 128, 200, 256, 250, 300, 500, 600, 1000};
		Random rand = new Random();
		double signal[];
		int numErrors = 0;
		double maxabssignal;
        
        for (i = 0; i < arr.length; i++) {
        	N = arr[i];
        	signal = new double[N];
        	for (j = 0; j < N; j++) {
        		signal[j] = rand.nextGaussian();
        	}
        	signal = __inf_norm(signal);
        	 maxabssignal = 0.0;
             for (j = 0; j < N; j++) {
            	 if (Math.abs(signal[j]) > maxabssignal) {
            		 maxabssignal = Math.abs(signal[j]);
            	 }
             }
             if (Math.abs(maxabssignal - 1.0) >= 1.5E-7) {
            	 numErrors++;
            	 System.err.println("In test_inf_norm() for N = " + N + " maxabssignal = " + maxabssignal);
             }
        }
        if (numErrors > 0) {
        	System.err.println("In test_inf_norm() " + numErrors + " errors were detected");
        }
        else {
        	System.out.println("In test_inf_norm() no errors were detected");
        }
    }


   
    public double[] __inf_norm(double x[]) {
        // Private helper for inf-norm normalization a list of arrays.
        
    	int i;
    	double maxabsx = 0.0;
    	for (i = 0; i < x.length; i++) {
    		if (Math.abs(x[i]) > maxabsx) {
    			maxabsx = Math.abs(x[i]);
    		}
    	}
    	double xnorm[] = new double[x.length];
    	for (i = 0; i < x.length; i++) {
    		xnorm[i] = x[i]/(maxabsx + 1.0E-12);
    	}
    	return xnorm;
    }

    public double[][] _inf_norm(double arr[][], int axis) {
    	// default axis = 1;
    	int rownum = arr.length;
    	int colnum = arr[0].length;
    	double normarr[][] = new double[arr.length][arr[0].length];
    	int i,j;
    	if (axis == 0) {
    		double colarr[] = new double[rownum];
    	    for (j = 0; j < colnum; j++) {
    	        for (i = 0; i < rownum; i++) {
    	        	colarr[i] = arr[i][j];
    	        }
    	        double infarr[] = __inf_norm(colarr);
    	        for (i = 0; i < rownum; i++) {
    	        	normarr[i][j] = infarr[i];
    	        }
    	    }
    	}
    	else {
    		for (i = 0; i < rownum; i++) {
    			double infarr[] = __inf_norm(arr[i]);
    			for (j = 0; j < colnum; j++) {
    				normarr[i][j] = infarr[j];
    			}
    		}
    	}
    	return normarr;
    }
    
    public double[][] _inf_norm(double arr[][][], int axis) {
    	// default axis = 1;
    	int rownum = arr[0].length;
    	int colnum = arr[0][0].length;
    	double normarr[][] = new double[arr.length*arr[0].length][arr[0][0].length];
    	int i,j,k;
    	for (k = 0; k < arr.length; k++) {
	    	if (axis == 0) {
	    		double colarr[] = new double[rownum];
	    	    for (j = 0; j < colnum; j++) {
	    	        for (i = 0; i < rownum; i++) {
	    	        	colarr[i] = arr[k][i][j];
	    	        }
	    	        double infarr[] = __inf_norm(colarr);
	    	        for (i = 0; i < rownum; i++) {
	    	        	normarr[k*rownum+i][j] = infarr[i];
	    	        }
	    	    }
	    	}
	    	else {
	    		for (i = 0; i < rownum; i++) {
	    			double infarr[] = __inf_norm(arr[k][i]);
	    			for (j = 0; j < colnum; j++) {
	    				normarr[k*rownum+i][j] = infarr[j];
	    			}
	    		}
	    	}
    	} // for (k = 0; k < arr.length; k++)
    	return normarr;
    }
    
    public double[][][] __inf_norm(double x[][][]) {
        // Private helper for inf-norm normalization a list of arrays.
        
    	int i,j,k;
    	double maxabsx = 0.0;
    	for (i = 0; i < x.length; i++) {
    		for (j = 0; j < x[0].length; j++) {
    			for (k = 0; k < x[0][0].length; k++) {
		    		if (Math.abs(x[i][j][k]) > maxabsx) {
		    			maxabsx = Math.abs(x[i][j][k]);
		    		}
    			}
    		}
    	}
    	double xnorm[][][] = new double[x.length][x[0].length][x[0][0].length];
    	for (i = 0; i < x.length; i++) {
    		for (j = 0; j < x[0].length; j++) {
    			for (k = 0; k < x[0][0].length; k++) {
    		        xnorm[i][j][k] = x[i][j][k]/(maxabsx + 1.0E-12);
    			}
    		}
    	}
    	return xnorm;
    }
    
    public double norm(double x[]) {
    	int i;
    	double sum = 0.0;
    	for (i = 0; i < x.length; i++) {
    	    sum += (x[i]*x[i]);	
    	}
    	return Math.sqrt(sum);
    }
    
    public void add_gaussian_noise(double noisy_signal[], double noise[], double signal[],
    		double snr, boolean haveSeed, long random_state) {
        // Add a Gaussian noise to signal to ouput a signal with the targeted snr.

        // Parameters
        // ----------
        // signal : np.ndarray,
        //    The given signal on which add a Guassian noise.

        // snr : float,
        //    The expected SNR for the output signal.
    	
    	// boolean haveSeed default false

        // random_state :  int or None (default=None),
        //    Whether to impose a seed on the random generation or not (for
        //    reproductability).

        // Return
        // ------
        // noisy_signal: np.ndarray,
        //    the noisy produced signal.

        // noise:  np.ndarray,
        //    the additif produced noise.
    	int i;
        Random r;
     // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon is called the largest relative spacing
        double epsilon = 2.2204460e-16;
    	if (haveSeed) {
			r = random_generator(random_state);
		}
		else {
			r = new Random();
		}
        
        int s_shape = signal.length;
        for (i = 0; i < s_shape; i++) {
        	noise[i] = r.nextGaussian();
        }

        double true_snr_num = norm(signal);
        double true_snr_deno = norm(noise);
        double true_snr = true_snr_num / (true_snr_deno + epsilon);
        double std_dev = (1.0 / Math.sqrt(Math.pow(10,(snr/10.0)))) * true_snr;
        for (i = 0; i < s_shape; i++) {
            noise[i] = std_dev * noise[i];
            noisy_signal[i] = signal[i] + noise[i];
        }

        return; // noisy_signal, noise
    }

    
    public void gen_rnd_ai_s(double ai_s[], double i_s[], double t[], double dur, 
    		double tr, int nb_events, int avg_dur, double std_dur,
            boolean middle_spike, boolean overlapping, boolean unitary_block,
            boolean haveSeed, long random_state, int nb_try, int nb_try_duration,
            boolean centered) {
		// Generate a Activity inducing signal.
		// dur : int (default=3),
		//   The length of the BOLD signal (in minutes).
		
		// tr : float (default=1.0),
		//   Repetition time
		
		// nb_events : int (default=4),
		//   Number of neural activity on-sets.
		
		// avg_dur : int (default=5),
		//   The average duration (in second) of neural activity on-sets.
		
		// std_dur : int (default=1),
		//   The standard deviation (in second) on the duration of neural
		//   activity on-sets.
		
		// middle_spike : bool (default=False),
		//   Whether to force a dirac on the middle on the BOLD signal.
		
		// overlapping : bool (default=False),
		//   Whether to authorize overlapping between on-sets.
		
		// unitary_block : bool (default=False),
		//   force the block to have unitary amplitude.
    	
    	// boolean haveSeed = false if random_state = None
    	// else haveSeed = true if random_state is a long
		   
		// random_state : int or None (default=None),
		//   Whether to impose a seed on the random generation or not (for
		//   reproductability).
		
		// nb_try : int (default=1000),
		//   Number of try to generate the BOLD signal.
		
		// nb_try_duration : int (default=1000),
		//   Number of try to generate the each neural activity on-set.
		
		// centered : boolean default false
		
		// Return
    	// All 3 outputs have the same length
    	// length = 1 + (int)((60000*dur-1)/(int)(1000tr))
		// ------
		
		// ai_s : np.ndarray
		//   Activity inducing signal.
		
		// i_s : np.ndarray,
		//   Innovation signal.
		
		// t : np.ndarray,
		//   time scale signal.
		double dt;
		int N;
		double var_dur;
		int center_neighbors[];
		Random r;
		int i,j,k;
		double durs[] = new double[nb_events];
		int durations[] = new int[nb_events];
		boolean lessthanone;
		boolean greaterthanone;
		int offset;
		int duration;
		double ai_s_unsampled[];
		int step;
		double inc;
		int current_nb_events;
		double ai_s_mean;
		double i_s_mean;
	
		dt = 0.001;  // to similate continious signal generation
		N = (int)((dur * 60) / dt);
		avg_dur /= dt;
		var_dur = std_dur * std_dur;
		var_dur /= dt;
		center_neighbors = new int[] {(N/2)-1, (N/2)+1};
		
		// for reproductibility
		
		if (haveSeed) {
			nb_try = 1;
			r = random_generator(random_state);
		}
		else {
			r = new Random();
		}
		
		// generate the block
		for (i = 0; i < nb_try; i++) {
		   int offsets[] = r.ints(nb_events, 0, N).toArray();
		   for (j = 0; j < nb_try_duration; j++) {
			   lessthanone = false;
			   for (k = 0; k < nb_events; k++) {
			       durs[k] =  avg_dur + var_dur*r.nextGaussian(); 
			       if (durs[k] < 1.0) {
			    	   lessthanone = true;
			       }
			   } // for (k = 0; k < nb_events; k++)
			   if (lessthanone) {
				   continue;
			   }
			   else {
				   break;
			   }
		   } // for (j = 0; j < nb_try_duration; j++)
		      
		
		   // place the block
		   for (j = 0; j < nb_events; j++) {
			   durations[j] = (int)durs[j];
		   }
		   ai_s_unsampled = new double[N];
		   loop1: for (j = 0; j < nb_events; j++) {
		       offset = offsets[j];
		       duration = durations[j];
		       for (k = offset; k < offset + duration; k++) {
		    	   if (k >= N) {
		    	       break loop1;   
		    	   }
		    	   else {
		    		   ai_s_unsampled[k] += 1;
		    	   }
		       }
		   } // loop1: for (j = 0; j < nb_events; j++)
		
		   // check optional conditions
		   if (middle_spike) {
		       ai_s_unsampled[N/2] += 1;  // put a dirac une the center
		   }
		   greaterthanone = false;
		   for (j = 0; j < N; j++) {
			   if (ai_s_unsampled[j] > 1) {
				   greaterthanone = true;
			   }
		   }
		   if ((!overlapping) && greaterthanone) {
		       continue;  // overlapping events: retry
		   }
		   
		   if (overlapping) {
		       if (unitary_block) {
		    	   for (j = 0; j < N; j++) {
		    		   if (ai_s_unsampled[j] > 1.0) {
		    			   ai_s_unsampled[j] = 1.0; // normalized overlapping ai_s_unsampled
		    		   }
		    	   }
		       } // if (unitary_block)
		   } if (overlapping)
		   if (middle_spike && ((ai_s_unsampled[center_neighbors[0]] > 0) || (ai_s_unsampled[center_neighbors[1]] > 0))) {
		       continue;  // middle-spike not isolated: retry
		   }
		
		   // subsample signal at 1/TR
		   step = (int)(tr/dt);
		   for (j = 0, k = 0; k < N; j++, k += step) {
			   ai_s[j] = ai_s_unsampled[k];
		   }
		
		   // generate innovation signal and the time scale on 'dur' duration with
		   // '1/TR' sample rate
		   i_s[0] = 0;
		   for (j = 1; j < ai_s.length; j++) {
			   i_s[j] = ai_s[j] - ai_s[j-1];
		   }
		   inc = dur*60.0/(ai_s.length-1.0);
		   t[0] = 0;
		   t[ai_s.length-1] = dur*60;
		   for (j = 1; j < ai_s.length-1; j++) {
			   t[j] = j*inc;
		   }
		
		   current_nb_events = 0;
		   for (j = 0; j < i_s.length; j++) {
			   if (i_s[j] > 0.5) {
				   current_nb_events++;
			   }
		   }
		  
		   if( (!overlapping) && (current_nb_events != nb_events)) {
		       continue;  // decimation step erase an event
		   }
		
		   if (centered) {
			   ai_s_mean = 0.0;
			   i_s_mean = 0.0;
			   for (j = 0; j < ai_s.length; j++) {
				   ai_s_mean += ai_s[j];
				   i_s_mean += i_s[j];
			   }
			   ai_s_mean /= ai_s.length;
			   i_s_mean /= i_s.length;
			   for (j = 0; j < ai_s.length; j++) {
		           ai_s[j] -= ai_s_mean;
		           i_s[j] -= i_s_mean;
			   }
		   }
		   
		   return; //  ai_s, i_s, t
		} // for (i = 0; i < nb_try; i++)
		
		System.err.println("[Failure] Failed to produce an activity-inducing signal");
		System.err.println("Please re-run gen_ai_s function with possibly new arguments.");
		System.exit(-1);
    }
    
    public void gen_rnd_bloc_bold(double noisy_ar_s[],double ar_s[], double ai_s[],
    		double i_s[],  double t[], double noise[],
    		double dur, double tr, boolean sourcehrf, double hrf[], int nb_events, int avg_dur,
            double std_dur, boolean middle_spike, boolean overlapping,
            boolean unitary_block, double snr, int nb_try,
            int nb_try_duration, boolean centered, boolean haveSeed, long random_state) {
		// Generate synthetic BOLD signal.
		
		// Parameters
		// ----------
		// dur : int (default=5),
		// The length of the BOLD signal (in minutes).
		
		// tr : float (default=1.0),
		// Repetition time
    	
    	// sourcehrf: boolean default: false
		
		// hrf : np.ndarray (default=None),
		// Specified HRF, if None a SPM like HRF is used based on
		// hrf_time_length arg.
		
		// nb_events : int (default=4),
		// Number of neural activity on-sets.
		
		// avg_dur : int (default=5),
		// The average duration (in second) of neural activity on-sets.
		
		// std_dur : int (default=1),
		// The standard deviation (in second) on the duration of neural
		// activity on-sets.
		
		// middle_spike : bool (default=False),
		// Whether to force a dirac on the middle on the BOLD signal.
		
		// overlapping : bool (default=False),
		// Whether to authorize overlapping between on-sets.
		
		// unitary_block : bool (default=False),
		// force the block to have unitary amplitude.
		
		// snr: float (default=1.0),
		// SNR of the noisy BOLD signal.
		
		// nb_try: int (default = 1000);
    
        // nb_try_duration: int (default = 1000)
    	
    	// // centered : boolean default false
        
        // haveSeed: boolean (default = false)
		
		// random_state : int or None (default=None),
		// Whether to impose a seed on the random generation or not (for
		// reproductability).
		
		// Return
		// ------
    	// noisy_ar_s, ar_s, ai_s, i_s, t, noise all have the same length;
    	// length = 1 + (int)((60000*dur-1)/(int)(1000tr))
    	// if sourcehrf = false length of hrf is given by:
    	// int numsamples = (int)(dur/ dt);
		// int step = (int)(t_r/dt);
		// int returnedsamples = ((numsamples-1)/step) + 1; length of hrf[]
		
		// noisy_ar_s : np.ndarray,
		// Noisy activity related signal.
		
		// ar_s : np.ndarray,
		// Activity related signal.
		
		// ai_s : np.ndarray,
		// Activity inducing signal.
		
		// i_s : np.ndarray,
		// Innovation signal.
		
		// t : np.ndarray,
		// time scale signal.
		
		// hrf : np.ndarray,
		// HRF.
		
		// t_hrf : np.ndarray,
		// time scale HRF, if an HRF is specified then t_hrf is None and the user
		// should be aware of the corresponding time scale HRF.
		
		// noise : np.ndarray,
		// Noise.
		
	
		int i;
    	gen_rnd_ai_s(ai_s, i_s, t, dur, tr, nb_events,
		                      avg_dur, std_dur,
		                      middle_spike,
		                      overlapping,
		                      unitary_block,
		                      haveSeed, random_state,
		                      nb_try, nb_try_duration, centered);
		
		if (!sourcehrf) {
		     double dt = 0.001;
			 int numsamples = (int)(dur/ dt);
		     int step = (int)(tr/dt);
		     int returnedsamples = ((numsamples-1)/step) + 1;
		     double t_hrf[] = new double[returnedsamples];
		     double delta = 1.0;
		     double durs = 60.0;
		     boolean normalized_hrf = true;
		     double p_delay = 6.0;
		     double undershoot = 16.0;
		     double p_disp = 1.0;
		     double u_disp = 1.0;
		     double p_u_ratio = 0.167;
		     double onset = 0.0;
		     spm_hrf(hrf, t_hrf, delta, tr,
		            durs, normalized_hrf, dt, p_delay,
                    undershoot, p_disp, u_disp, p_u_ratio, onset);
		} // if (!sourcehrf)
		double ar_s_copy[] = simple_convolve(hrf, ai_s, ai_s.length);
		for (i = 0; i < ar_s.length; i++) {
			ar_s[i] = ar_s_copy[i];
		}
		
		add_gaussian_noise(noisy_ar_s, noise, ar_s, snr,
		                   haveSeed, random_state);
		
		return; // noisy_ar_s, ar_s, ai_s, i_s, t, hrf, noise
    }
    
    public void _yield_data(double noisy_ar_s[][], double ar_s[][], double ai_s[][],
    		double i_s[][], double hrf[][], double noise[][],
    		double snr[], int nb_events[]) {
        // Yield signals for unittests.
        
        // caution random_state should be fixed but it could also systematically
        // lead to failing signal generation, so this arg is carefully set
    	int i;
        long random_state = 99;
        double snr_s[] = new double[]{1.0, 10.0/*, 20.0*/};
        double tr_s[] = new double[] {0.1, 2.0};
        int dur_orig_s[] = new int[] {3, 5/*, 10*/};  // minutes
        double delta_s[] = new double[] {0.5, 1.0};
        double tr;
        int dur_orig;
        double delta;
        double t[];
        double dt = 0.001;
        double dur = 60.0;
		int numsamples = (int)(dur/ dt);
	    boolean normalized_hrf = true;
	    double p_delay = 6.0;
	    double undershoot = 16.0;
	    double p_disp = 1.0;
	    double u_disp = 1.0;
	    double p_u_ratio = 0.167;
	    double onset = 0.0;
	    boolean sourcehrf = true;
	    int avg_dur = 1;
	    double std_dur = 5.0;
	    boolean middle_spike = false;
	    boolean overlapping = false;
	    boolean unitary_block = false;
	    int nb_try = 1000;
	    int nb_try_duration = 1000;
	    boolean centered = false;
	    boolean haveSeed = true;
        for (i = 0; i < 2; i++) {
            snr[i] = snr_s[i];
            tr = tr_s[i];
            dur_orig = dur_orig_s[i];
            delta = delta_s[i];
            nb_events[i] = (int)(dur_orig/2);
            int step = (int)(tr/dt);
    	    int returnedsamples = ((numsamples-1)/step) + 1;
    	    hrf[i] = new double[returnedsamples];
    	    double t_hrf[] = new double[returnedsamples];
		    spm_hrf(hrf[i], t_hrf, delta, tr,
		            dur, normalized_hrf, dt, p_delay,
                    undershoot, p_disp, u_disp, p_u_ratio, onset);
		    int genlength = 1 + (int)((60000*dur_orig-1)/(int)(1000*tr));
		    noisy_ar_s[i] = new double[genlength];
		    ar_s[i] = new double[genlength];
		    ai_s[i] = new double[genlength];
		    i_s[i] = new double[genlength];
		    t = new double[genlength];
		    noise[i] = new double[genlength];
            gen_rnd_bloc_bold(noisy_ar_s[i], ar_s[i], ai_s[i],
            		i_s[i],  t, noise[i],
            		dur_orig, tr, sourcehrf, hrf[i], nb_events[i], avg_dur,
                    std_dur, middle_spike, overlapping,
                    unitary_block, snr[i], nb_try,
                    nb_try_duration, centered, haveSeed, random_state);
        } // for (i = 0; i < 2; i++)
    }
    
    public void _test_data(double noisy_ar_s[], double ar_s[], double ai_s[], 
    		double i_s[], double hrf[], double noise[], 
    		double snr, int nb_events, String testnum) {
        // Helper to test:
        // - Manually reconvolve the block signal
        //  and compare it to the BOLD signal
        // - test the produce signal SNR
        // - test if noise == (noisy_ar_s - ar_s)

        // test if the 'ar_s = (hrf * ai_s)'
    	int i;
    	System.out.println("Testing with snr = " + snr + " nb_events = " + nb_events + " for test number " + testnum);
        double ar_s_test[] = simple_convolve(hrf, ai_s, ai_s.length);
        int numDifferences = 0;
        for (i = 0; i < ar_s.length; i++) {
            if (Math.abs(ar_s[i] - ar_s_test[i]) >= 1.5E-7) {
            	numDifferences++;
            }
        }
        if (numDifferences > 0) {
        	System.err.println(numDifferences + " differences were detected for allclose(ar_s, ar_s_test)");
        }
        else {
        	System.out.println("No differences were detected for allclose(ar_s, ar_s_test)");
        }

        // test if expect snr == data snr
        double true_snr = 20.0 * Math.log10(norm(ar_s) / norm(noise));
        if (Math.abs(snr - true_snr) > 1.5E-7) {
        	System.err.println("snr = " + snr + " true_snr = " + true_snr);
        }
        else {
        	System.out.println("snr is close to true_snr");
        }

        // test if expect noise == (noisy_ar_s - ar_s)
        double residual[] = new double[ar_s.length];
        for (i = 0; i < ar_s.length; i++) {
        	residual[i] = noisy_ar_s[i] - ar_s[i];
        }
        numDifferences = 0;
        for (i = 0; i < residual.length; i++) {
            if (Math.abs(noise[i] - residual[i]) >= 1.5E-7) {
            	numDifferences++;
            }
        }
        if (numDifferences > 0) {
        	System.err.println(numDifferences + " differences were detected for allclose(residual, noise)");
        }
        else {
        	System.out.println("No differences were detected for allclose(resiudal, noise)");
        }

        // test if nb_events is respected
        int true_nb_events = nb_events;
        nb_events = 0;
        for (i = 0; i < i_s.length; i++) {
        	if (i_s[i] > 0.5) {
        		nb_events++;
        	}
        }
        if (nb_events != true_nb_events) {
            System.err.println("nb_events = " + nb_events + " true_nb_events = " + true_nb_events);	
        }
        else {
        	System.out.println("nb_events = true_nb_events");
        }
    }


    public void test_data_gen() {
    	// Testing with snr = 1.0 nb_events = 1 for test number 0
		// No differences were detected for allclose(ar_s, ar_s_test)
		// snr is close to true_snr
		// No differences were detected for allclose(resiudal, noise)
		// nb_events = true_nb_events
		// Testing with snr = 10.0 nb_events = 2 for test number 1
		// No differences were detected for allclose(ar_s, ar_s_test)
		// snr is close to true_snr
		// No differences were detected for allclose(resiudal, noise)
		// nb_events = true_nb_events
        // Test the data generation function:
           // - Manually reconvolve the block signal
           //   and compare it to the BOLD signal
           // - test the produce signal SNR
           // - test if noise == (noisy_ar_s - ar_s)
    	int i;
    	double noisy_ar_s[][] = new double[2][];
    	double ar_s[][] = new double[2][];
    	double ai_s[][] = new double[2][];
    	double i_s[][] = new double[2][];
    	double hrf[][] = new double[2][];
    	double noise[][] = new double[2][];
    	double snr[] = new double[2];
    	int nb_events[] = new int[2];
    	_yield_data(noisy_ar_s, ar_s, ai_s,
        		i_s, hrf, noise,
        		snr, nb_events);
    	for (i = 0; i < 2; i++) {
    		_test_data(noisy_ar_s[i], ar_s[i], ai_s[i], 
    	    		i_s[i], hrf[i], noise[i], 
    	    		snr[i], nb_events[i], String.valueOf(i));	
    	}
    }
    
    public void gen_regular_ai_s(double ai_s[], double i_s[], double t[], 
    		int dur, double tr, double dur_bloc, boolean centered) {
        // Generate a Activity inducing signal.
        // Defaults:
    	// dur = 10
    	// tr = 1.0
    	// dur_bloc = 30.0
    	// centered = true
    	int i;
    	double ai_s_mean;
    	double i_s_mean;
        int N = (int)(dur * 60 / tr);
        //i_s = np.zeros(N)
        for (i = 0; i < N; i++) {
        	i_s[i] = 0.0;
        }
        int step = (int)(dur_bloc/tr);
        int sign = 1;
        for (i = 0; i < N; i += step) {
        	i_s[i] = sign;
        	sign *= -1;
        }
        ai_s[0] = i_s[0];
        for (i = 1; i < N; i++) {
        	ai_s[i] = ai_s[i-1] + i_s[i];
        }
        double inc = dur*60.0/(ai_s.length-1.0);
		t[0] = 0;
		t[ai_s.length-1] = dur*60;
		for (i = 1; i < ai_s.length-1; i++) {
			   t[i] = i*inc;
		}
		
		if (centered) {
		   ai_s_mean = 0.0;
		   i_s_mean = 0.0;
		   for (i = 0; i < ai_s.length; i++) {
			   ai_s_mean += ai_s[i];
			   i_s_mean += i_s[i];
		   }
		   ai_s_mean /= ai_s.length;
		   i_s_mean /= i_s.length;
		   for (i = 0; i < ai_s.length; i++) {
	           ai_s[i] -= ai_s_mean;
	           i_s[i] -= i_s_mean;
		   }
	   }
		   
	   return; //  ai_s, i_s, t
    }

    public void gen_regular_bloc_bold(double noisy_ar_s[], double ar_s[], double ai_s[],
    		double i_s[], double t[], double noise[], int dur, double tr, double dur_bloc,
    		boolean sourcehrf, double hrf[], double snr, boolean haveSeed, long random_state) {
		// Generate a Activity inducing signal.
		// Defaults:
    	// int dur = 10
    	// double tr = 1.0
    	// double dur_bloc = 30.0
    	// boolean sourcehrf = false
    	// double snr = 1.0
    	// boolean haveSeed = false
    	int i;
		gen_regular_ai_s(ai_s, i_s, t, dur, tr, dur_bloc, true);
		
		if (!sourcehrf) {
		     double dt = 0.001;
			 int numsamples = (int)(dur/ dt);
		     int step = (int)(tr/dt);
		     int returnedsamples = ((numsamples-1)/step) + 1;
		     double t_hrf[] = new double[returnedsamples];
		     double delta = 1.0;
		     double durs = 60.0;
		     boolean normalized_hrf = true;
		     double p_delay = 6.0;
		     double undershoot = 16.0;
		     double p_disp = 1.0;
		     double u_disp = 1.0;
		     double p_u_ratio = 0.167;
		     double onset = 0.0;
		     spm_hrf(hrf, t_hrf, delta, tr,
		            durs, normalized_hrf, dt, p_delay,
                   undershoot, p_disp, u_disp, p_u_ratio, onset);
		} // if (!sourcehrf)
		double ar_s_copy[] = simple_convolve(hrf, ai_s, ai_s.length);
		for (i = 0; i < ar_s.length; i++) {
			ar_s[i] = ar_s_copy[i];
		}
		
		add_gaussian_noise(noisy_ar_s, noise, ar_s, snr,
		                   haveSeed, random_state);
		
		return; // noisy_ar_s, ar_s, ai_s, i_s, t, hrf, noise
    }
    
    public void gen_rnd_i_s(double i_s[], double t[], int dur, double tr, int nb_events,
    		double avg_ampl, double std_ampl, boolean haveSeed, 
            long random_state, int nb_try, int nb_try_duration,
		    boolean centered) {
		// Generate a innovation signal.
		//dur : int (default=3),
		//    The length of the BOLD signal (in minutes).
		
		// tr : float (default=1.0),
		//    Repetition time
		
		// nb_events : int (default=4),
		//    Number of neural activity on-sets.
		
		// avg_ampl : double (default=1),
		//    The average of the amplitude of the events.
		
		// std_ampl : double (default=0.5),
		//    The standard deviation of the amplitude of the events.
    	
    	// boolean haveSeed = false
    	
    	// random_state : int or None (default=None),
	    // Whether to impose a seed on the random generation or not (for
	    // reproductability).
		
		// nb_try : int (default=1000),
		//    Number of try to generate the BOLD signal.
		
		// nb_try_duration : int (default=1000),
		//    Number of try to generate the each neural activity on-set.
		
		// boolean centered = false
		
		// Return
		// ------
		
		// i_s : np.ndarray,
		//    Innovation signal.
		
		// t : np.ndarray,
		//    time scale signal.
		
	    int N;
	    int i,j,k;
	    boolean lessthanzero;
	    double var_ampl;
	    double ampls[] = new double[nb_events];
	    Random r;
	    int offset;
	    double ampl;
	    int current_nb_events;
	    double i_s_mean;
		N = (int)((dur * 60) / tr);
		var_ampl = std_ampl * std_ampl;
		
		// for reproductibility
		if (haveSeed) {
			nb_try = 1;
			r = random_generator(random_state);
		}
		else {
			r = new Random();
		}
		
		// generate the block
		for (i = 0; i < nb_try; i++) {
		   int offsets[] = r.ints(nb_events, 0, N).toArray();
		   for (j = 0; j < nb_try_duration; j++) {
			   lessthanzero = false;
			   for (k = 0; k < nb_events; k++) {
			       ampls[k] =  avg_ampl + var_ampl*r.nextGaussian(); 
			       if (ampls[k] < 0.0) {
			    	   lessthanzero = true;
			       }
			   } // for (k = 0; k < nb_events; k++)
			   if (lessthanzero) {
				   continue; // null or negative duration events: retry
			   }
			   else {
				   break;
			   }
		   } // for (j = 0; j < nb_try_duration; j++)
		
		   // place the block
		   // i_s = np.zeros(N)
		   for (j = 0; j < N; j++) {
			   i_s[j] = 0.0;
		   }
		   for (j = 0; j < N; j++) {
		       offset = offsets[j];
		       ampl = ampls[j];
		       if (offset >= N) {
		    	   break;
		       }
		       else {
		    	   i_s[offset] += ampl;
		       }
		   }
		
		    // generate innovation signal and the time scale on 'dur' duration with
		    // '1/TR' sample rate
		    double inc = dur*60.0/(i_s.length-1.0);
			t[0] = 0;
			t[i_s.length-1] = dur*60;
			for (j = 1; j < i_s.length-1; j++) {
				   t[i] = i*inc;
			}
		
		    current_nb_events = 0;
		    for (j = 0; j < N; j++) {
		    	if (i_s[j] > 0.0) {
		    		current_nb_events++;
		    	}
		    }
		    
		    if (current_nb_events != nb_events) {
		        continue;  // decimation step erase an event
		    }
		
		    if (centered) {
		    	i_s_mean = 0.0;
		    	for (j = 0; j < N; j++) {
		    		i_s_mean += i_s[j];
		    	}
		    	i_s_mean = i_s_mean/N;
		    	for (j = 0; j < N; j++) {
		            i_s[j] -= i_s_mean;
		    	}
		    }
		
		    return; // i_s, t
		} // for for (i = 0; i < nb_try; i++)
		
		System.err.println("[Failure] Failed to produce an activity-inducing signal");
		System.err.println("Please re-run gen_ai_s function with possibly new arguments.");
		System.exit(-1);
    }

    public void gen_rnd_event_bold(double noisy_ar_s[], double ar_s[], double i_s[], double t[], double noise[],
    		int dur, double tr, boolean sourcehrf, double hrf[], int nb_events, double avg_ampl,
            double std_ampl, double snr, boolean haveSeed, long random_state) {
		// Generate synthetic BOLD signal.
		
		// Parameters
		// ----------
		// dur : int (default=5),
		// The length of the BOLD signal (in minutes).
		
		// tr : float (default=1.0),
		// Repetition time
    	
    	// boolean sourcehrf: default = false;
		
		// hrf : np.ndarray (default=None),
		// Specified HRF, if None a SPM like HRF is used based on
		// hrf_time_length arg.
		
		// nb_events : int (default=4),
		// Number of neural activity on-sets.
		
		// avg_ampl : double (default=5),
		// The average of the amplitude of the events.
		
		// std_ampl : double (default=1),
		// The standard deviation of the amplitude of the events.
		
		// snr: float (default=1.0),
		// SNR of the noisy BOLD signal.
    	
    	// boolean haveSeed (default = false)
		
		// random_state : int or None (default=None),
		// Whether to impose a seed on the random generation or not (for
		// reproductability).
		
		// Return
		// ------
		
		// noisy_ar_s : np.ndarray,
		// Noisy activity related signal.
		
		// ar_s : np.ndarray,
		// Activity related signal.
		
		// i_s : np.ndarray,
		// Innovation signal.
		
		// t : np.ndarray,
		// time scale signal.
		
		// hrf : np.ndarray,
		// HRF.
		
		// t_hrf : np.ndarray,
		// time scale HRF, if an HRF is specified then t_hrf is None and the user
		// should be aware of the corresponding time scale HRF.
		
		// noise : np.ndarray,
		// Noise.
		int i;
	    int nb_try = 1000;
	    int nb_try_duration = 1000;
	    boolean centered = false;
		gen_rnd_i_s(i_s, t, dur, tr, nb_events,
		              avg_ampl, std_ampl,
		              haveSeed, random_state, 
		              nb_try, nb_try_duration, centered);
		
		if (!sourcehrf) {
		     double dt = 0.001;
			 int numsamples = (int)(dur/ dt);
		     int step = (int)(tr/dt);
		     int returnedsamples = ((numsamples-1)/step) + 1;
		     double t_hrf[] = new double[returnedsamples];
		     double delta = 1.0;
		     double durs = 60.0;
		     boolean normalized_hrf = true;
		     double p_delay = 6.0;
		     double undershoot = 16.0;
		     double p_disp = 1.0;
		     double u_disp = 1.0;
		     double p_u_ratio = 0.167;
		     double onset = 0.0;
		     spm_hrf(hrf, t_hrf, delta, tr,
		            durs, normalized_hrf, dt, p_delay,
                  undershoot, p_disp, u_disp, p_u_ratio, onset);
		} // if (!sourcehrf)
		double ar_s_copy[] = simple_convolve(hrf, i_s, i_s.length);
		for (i = 0; i < ar_s.length; i++) {
			ar_s[i] = ar_s_copy[i];
		}
		
		add_gaussian_noise(noisy_ar_s, noise, ar_s, snr,
		                   haveSeed, random_state);
		
		return; // noisy_ar_s, ar_s, i_s, t, hrf, noise
    }

    
    public void yield_diracs_signal(double i_s[][], double hrf[][], double alpha[][], double tr[]) {
        // Yield diracs signals on which to test the convolution functions.
    	int i,j;
        int durm;
        double tr_s[];
        double delta_s[];
        double delta;
        double t[];
        int tlength;
        double tstep;
        int onset_event;
        double dur = 60.0;
        boolean normalized_hrf = true;
        double dt = 0.001;
        double p_delay = 6.0;
        double undershoot = 16.0;
        double p_disp = 1.0;
        double u_disp = 1.0;
        double p_u_ratio = 0.167;
        double onset = 0.0;
        int numsamples;
        int step;
        int returnedsamples;
        double t_hrf[];
        int nb_atoms = 20;
        
        durm = 1;  // minutes
        tr_s = new double[] {0.1, 2.0};
        delta_s = new double[] {0.5, 1.0};
        for (i = 0; i < 2; i++) {
            tr[i] = tr_s[i];
            delta = delta_s[i];
            tlength = (int)(durm*60/tr[i]);
            t = new double[tlength];
            t[0] = 0;
            t[tlength-1] = durm*60;
            tstep = durm*60/(tlength-1);
            for (j = 1; j < tlength-1; j++) {
                t[j] = j*tstep;	
            }
            // place Dirac at 20% of dur
            onset_event = (int)(0.20 * durm * 60 / tr[i]);
            i_s[i] = new double[tlength];
            i_s[i][onset_event] = 1;
            numsamples = (int)(dur/ dt);
    		step = (int)(tr[i]/dt);
    		returnedsamples = ((numsamples-1)/step) + 1; // length of hrf[] and t_hrf[]
    		hrf[i] = new double[returnedsamples];
    		t_hrf = new double[returnedsamples];
            spm_hrf(hrf[i], t_hrf, delta, tr[i],
            	    dur, normalized_hrf, dt, p_delay,
            	    undershoot, p_disp, u_disp, p_u_ratio, onset);
            alpha[i] = new double[nb_atoms];
            alpha[i][10] = 1.0;
            //yield i_s, hrf, alpha, tr
        } // for (i = 0; i < 2; i++)
    }

    public void yield_blocks_signal(double ai_s[][], double hrf[][], double alpha[][], double tr[]) {
        // Yield blocks signals on which to test the convolution functions.
        int i;
        long random_state = 0;
        double tr_s[] = new double[] {0.1, 2.0};
        int dur_orig_s[] = new int[] {3, 10};  // minutes
        double delta_s[] = new double[] {0.5, 1.0};
        int dur_orig;
        double delta;
        int ailength;
        double i_s[];
        double t[];
        int nb_events;
        int avg_dur = 1;
        int std_dur = 5;
        boolean middle_spike = false;
        boolean overlapping = true;
        boolean unitary_block = false;
        boolean haveSeed = true;
        int nb_try = 1000;
        int nb_try_duration = 1000;
        boolean centered = false;
        double dur = 60.0;
        boolean normalized_hrf = true;
        double dt = 0.001;
        double p_delay = 6.0;
        double undershoot = 16.0;
        double p_disp = 1.0;
        double u_disp = 1.0;
        double p_u_ratio = 0.167;
        double onset = 0.0;
        int numsamples;
        int step;
        int returnedsamples;
        double t_hrf[];
        int nb_atoms = 20;
        for (i = 0; i < 2; i++) {
            tr[i] = tr_s[i];
            dur_orig = dur_orig_s[i];
            delta = delta_s[i];
            ailength = 1 + (int)((60000*dur_orig-1)/(int)(1000*tr[i]));
            ai_s[i] = new double[ailength];
            i_s = new double[ailength];
            t = new double[ailength];
            // nb_events should be adapted to
            // the length of the signal
            nb_events = dur_orig/2;
            gen_rnd_ai_s(ai_s[i], i_s, t, dur_orig, 
            	         tr[i], nb_events, avg_dur, std_dur,
            	            middle_spike, overlapping, unitary_block,
            	            haveSeed, random_state, nb_try, nb_try_duration,
            	            centered);
            numsamples = (int)(dur/ dt);
    		step = (int)(tr[i]/dt);
    		returnedsamples = ((numsamples-1)/step) + 1; // length of hrf[] and t_hrf[]
    		hrf[i] = new double[returnedsamples];
    		t_hrf = new double[returnedsamples];
            spm_hrf(hrf[i], t_hrf, delta, tr[i],
            	    dur, normalized_hrf, dt, p_delay,
            	    undershoot, p_disp, u_disp, p_u_ratio, onset);
            alpha[i] = new double[nb_atoms];
            alpha[i][10] = 1.0;
            //yield ai_s, hrf, alpha, tr
        } // for (i = 0; i < 2; i++)
    }
    
    // Here we test three implementations for the convolution:
	// - the mathematical definition (two for loops)
	// - the Toeplitz matrix product
	// - the Fourier (spectral multiplication)
	
	// We test the direct convolution as long as the adjoint operator in the case
	// where kernel = HRF and kernel = block signal (for different HRF and block
	// signal)


	// case kernel = HRF

	// direct op

	// Toeplitz


	public void _test_conv_toeplitz_kernel_hrf(double ai_s[], double hrf[], String testname) {
	    // Helper to test the conv with a Toeplitz implementation,
	    // the kernel being the HRF.
	    int i,j;
	    int numDifferences = 0;
	    double H[][] = toeplitz_from_kernel(hrf, ai_s.length, ai_s.length);
	    double ar_s_ref[] = simple_convolve(hrf, ai_s, ai_s.length);
	    double ar_s_test[] = new double[H.length];
	    for (i = 0; i < H.length; i++) {
	    	for (j = 0; j < H[0].length; j++) {
	    		ar_s_test[i] += (H[i][j]*ai_s[j]);
	    	}
	    }
	    for (i = 0; i < ar_s_ref.length; i++) {
	    	if (Math.abs(ar_s_ref[i] - ar_s_test[i]) > (1.0E-7 + 1.0E-5*Math.abs(ar_s_test[i]))) {
	    	    numDifferences++;	
	    	}
	    }
	    if (numDifferences > 0) {
	    	System.err.println(numDifferences + " differences were found in " + testname);
	    }
	    else {
	    	System.out.println("No differences were found in " + testname);
	    }
	}
	
	public void test_toeplitz_convolution_kernel_hrf_on_dirac() {
		// No differences were found in test_toeplitz_convolution_kernel_hrf_on_dirac 0
		// No differences were found in test_toeplitz_convolution_kernel_hrf_on_dirac 1
        // Test Toeplitz implementation of the convolution on a single dirac,
        // the kernel being the HRF.
		int i;
        double ai_s[][] = new double[2][];
        double hrf[][] = new double[2][];
        double alpha[][] = new double[2][];
        double tr[] = new double[2];
		yield_diracs_signal(ai_s, hrf, alpha, tr);
		for (i = 0; i < 2; i++) {
			_test_conv_toeplitz_kernel_hrf(ai_s[i], hrf[i], "test_toeplitz_convolution_kernel_hrf_on_dirac " + i);	
		}
	}

	public void test_toeplitz_convolution_kernel_hrf_on_ai_s() {
		// No differences were found in test_toeplitz_convolution_kernel_hrf_on_ai_s 0
		// No differences were found in test_toeplitz_convolution_kernel_hrf_on_ai_s 1
        // Test Toeplitz implementation of the convolution on a block signal,
        // the kernel being the HRF.
		int i;
        double ai_s[][] = new double[2][];
        double hrf[][] = new double[2][];
        double alpha[][] = new double[2][];
        double tr[] = new double[2];
		yield_blocks_signal(ai_s, hrf, alpha, tr);
		for (i = 0; i < 2; i++) {
			_test_conv_toeplitz_kernel_hrf(ai_s[i], hrf[i], "test_toeplitz_convolution_kernel_hrf_on_ai_s " + i);	
		}
	}

	// Fourier


	public void _test_conv_fourier_kernel_hrf(double ai_s[], double hrf[], String testname) {
	    // Helper to test the conv with a Fourier implementation,
	    // the kernel being the HRF.
		int i;
	    int numDifferences = 0;
	    double ar_s_ref[] = simple_convolve(hrf, ai_s, ai_s.length);
	    double ar_s_test[] = spectral_convolve(hrf, ai_s);
	    for (i = 0; i < ar_s_ref.length; i++) {
	    	if (Math.abs(ar_s_ref[i] - ar_s_test[i]) > (1.0E-7 + 1.0E-5*Math.abs(ar_s_test[i]))) {
	    	    numDifferences++;	
	    	}
	    }
	    if (numDifferences > 0) {
	    	System.err.println(numDifferences + " differences were found in " + testname);
	    }
	    else {
	    	System.out.println("No differences were found in " + testname);
	    }
	}

	public void test_fourier_convolution_kernel_hrf_on_dirac() {
		// No differences were found in test_fourier_convolution_kernel_hrf_on_dirac 0
		// No differences were found in test_fourier_convolution_kernel_hrf_on_dirac 1
        // Test Fourier implementation of the convolution on a single dirac,
        // the kernel being the HRF.
		int i;
        double ai_s[][] = new double[2][];
        double hrf[][] = new double[2][];
        double alpha[][] = new double[2][];
        double tr[] = new double[2];
		yield_diracs_signal(ai_s, hrf, alpha, tr);
		for (i = 0; i < 2; i++) {
			_test_conv_fourier_kernel_hrf(ai_s[i], hrf[i], "test_fourier_convolution_kernel_hrf_on_dirac " + i);	
		}
	}
	
	public void test_fourier_convolution_kernel_hrf_on_ai_s() {
        // Test Fourier implementation of the convolution on a block signal,
        // the kernel being the HRF.
		// No differences were found in test_fourier_convolution_kernel_hrf_on_ai_s 0
		// No differences were found in test_fourier_convolution_kernel_hrf_on_ai_s 1
		int i;
        double ai_s[][] = new double[2][];
        double hrf[][] = new double[2][];
        double alpha[][] = new double[2][];
        double tr[] = new double[2];
		yield_blocks_signal(ai_s, hrf, alpha, tr);
		for (i = 0; i < 2; i++) {
			_test_conv_fourier_kernel_hrf(ai_s[i], hrf[i], "test_fourier_convolution_kernel_hrf_on_ai_s " + i);	
		}
	}

	// adj op


	// Toeplitz


	public void _test_conv_adj_toeplitz_kernel_hrf(double ai_s[], double hrf[], String testname) {
	    // Helper to test the adj conv with a Toeplitz implementation,
	    // the kernel being the HRF.
	    int i,j;
	    int numDifferences = 0;
	    double H[][] = toeplitz_from_kernel(hrf, ai_s.length, ai_s.length);
	    double HT[][] = new double[H[0].length][H.length];
	    for (i = 0; i < H.length; i++) {
	    	for (j = 0; j < H[0].length; j++) {
	    		HT[j][i] = H[i][j];
	    	}
	    }
	    double ar_s_ref[] = simple_retro_convolve(hrf, ai_s, ai_s.length);
	    double ar_s_test[] = new double[HT.length];
	    for (i = 0; i < HT.length; i++) {
	    	for (j = 0; j < HT[0].length; j++) {
	    		ar_s_test[i] += (HT[i][j]*ai_s[j]);
	    	}
	    }
	    for (i = 0; i < ar_s_ref.length; i++) {
	    	if (Math.abs(ar_s_ref[i] - ar_s_test[i]) > (1.0E-7 + 1.0E-5*Math.abs(ar_s_test[i]))) {
	    	    numDifferences++;	
	    	}
	    }
	    if (numDifferences > 0) {
	    	System.err.println(numDifferences + " differences were found in " + testname);
	    }
	    else {
	    	System.out.println("No differences were found in " + testname);
	    }
	}
	
	public void test_toeplitz_adj_convolution_kernel_hrf_on_dirac() {
		// No differences were found in test_conv_adj_toeplitz_kernel_hrf_on_dirac 0
		// No differences were found in test_conv_adj_toeplitz_kernel_hrf_on_dirac 1
        // Test Toeplitz implementation of the adj convolution on a single
        // dirac, the kernel being the HRF.
		int i;
        double ai_s[][] = new double[2][];
        double hrf[][] = new double[2][];
        double alpha[][] = new double[2][];
        double tr[] = new double[2];
		yield_diracs_signal(ai_s, hrf, alpha, tr);
		for (i = 0; i < 2; i++) {
			_test_conv_adj_toeplitz_kernel_hrf(ai_s[i], hrf[i], "test_conv_adj_toeplitz_kernel_hrf_on_dirac " + i);	
		}
	}

	public void test_toeplitz_adj_convolution_kernel_hrf_on_ai_s() {
		// No differences were found in test_conv_adj_toeplitz_kernel_hrf_on_ai_s 0
		// No differences were found in test_conv_adj_toeplitz_kernel_hrf_on_ai_s 1
        // Test Toeplitz implementation of the adj convolution on a block
        // signal, the kernel being the HRF.
		int i;
        double ai_s[][] = new double[2][];
        double hrf[][] = new double[2][];
        double alpha[][] = new double[2][];
        double tr[] = new double[2];
		yield_blocks_signal(ai_s, hrf, alpha, tr);
		for (i = 0; i < 2; i++) {
			_test_conv_adj_toeplitz_kernel_hrf(ai_s[i], hrf[i], "test_conv_adj_toeplitz_kernel_hrf_on_ai_s " + i);	
		}
	}
	
	// Fourier


	public void _test_conv_adj_fourier_hrf(double ai_s[], double hrf[], String testname) {
	    // Helper to test the adj conv with a Fourier implementation,
	    // the kernel being the HRF.
	    int i;
	    int numDifferences = 0;
	    double adj_ar_s_ref[] = simple_retro_convolve(hrf, ai_s, ai_s.length);
	    double adj_ar_s_test[] = spectral_retro_convolve(hrf, ai_s);
	    for (i = 0; i < adj_ar_s_ref.length; i++) {
	    	if (Math.abs(adj_ar_s_ref[i] - adj_ar_s_test[i]) > (1.0E-7 + 1.0E-5*Math.abs(adj_ar_s_test[i]))) {
	    	    numDifferences++;	
	    	}
	    }
	    if (numDifferences > 0) {
	    	System.err.println(numDifferences + " differences were found in " + testname);
	    }
	    else {
	    	System.out.println("No differences were found in " + testname);
	    }
	}

	public void test_fourier_adj_convolution_kernel_hrf_on_dirac() {
		// No differences were found in test_fourier_adj_convolution_kernel_hrf_on_dirac 0
		// No differences were found in test_fourier_adj_convolution_kernel_hrf_on_dirac 1
        // Test Fourier implementation of the adj convolution on a single
        // dirac, the kernel being the HRF.
		int i;
        double ai_s[][] = new double[2][];
        double hrf[][] = new double[2][];
        double alpha[][] = new double[2][];
        double tr[] = new double[2];
		yield_diracs_signal(ai_s, hrf, alpha, tr);
		for (i = 0; i < 2; i++) {
			_test_conv_adj_fourier_hrf(ai_s[i], hrf[i], "test_fourier_adj_convolution_kernel_hrf_on_dirac " + i);	
		}
	}
	
	public void test_fourier_adj_convolution_kernel_hrf_on_ai_s() {
		// No differences were found in test_fourier_adj_convolution_kernel_hrf_on_ai_s 0
		// No differences were found in test_fourier_adj_convolution_kernel_hrf_on_ai_s 1
        // Test Fourier implementation of the adj convolution on a block
        // signal, the kernel being the HRF.
		int i;
        double ai_s[][] = new double[2][];
        double hrf[][] = new double[2][];
        double alpha[][] = new double[2][];
        double tr[] = new double[2];
		yield_blocks_signal(ai_s, hrf, alpha, tr);
		for (i = 0; i < 2; i++) {
			_test_conv_adj_fourier_hrf(ai_s[i], hrf[i], "test_fourier_adj_convolution_kernel_hrf_on_ai_s " + i);	
		}
	}

	// case kernel = block signal

	// direct op

	// Toeplitz

    public void _test_conv_toeplitz_kernel_signal(double ai_s[], double hrf[], String testname) {
	    // Helper to test the conv with a Toeplitz implementation,
	    // the kernel being the block signal.
    	int i,j;
	    int numDifferences = 0;
	    double H[][] = toeplitz_from_kernel(ai_s, hrf.length, ai_s.length);
	    double ar_s_ref[] = simple_convolve(hrf, ai_s, ai_s.length);
	    double ar_s_test[] = new double[H.length];
	    for (i = 0; i < H.length; i++) {
	    	for (j = 0; j < H[0].length; j++) {
	    		ar_s_test[i] += (H[i][j]*hrf[j]);
	    	}
	    }
	    for (i = 0; i < ar_s_ref.length; i++) {
	    	if (Math.abs(ar_s_ref[i] - ar_s_test[i]) > (1.0E-7 + 1.0E-5*Math.abs(ar_s_test[i]))) {
	    	    numDifferences++;	
	    	}
	    }
	    if (numDifferences > 0) {
	    	System.err.println(numDifferences + " differences were found in " + testname);
	    }
	    else {
	    	System.out.println("No differences were found in " + testname);
	    }
    }
    
    public void test_toeplitz_convolution_kernel_signal_on_dirac() {
    	// No differences were found in test_toeplitz_convolution_kernel_signal_on_dirac 0
    	// No differences were found in test_toeplitz_convolution_kernel_signal_on_dirac 1
        // Test Toeplitz implementation of the convolution on a single dirac,
        // the kernel being the block signal.
    	int i;
        double ai_s[][] = new double[2][];
        double hrf[][] = new double[2][];
        double alpha[][] = new double[2][];
        double tr[] = new double[2];
		yield_diracs_signal(ai_s, hrf, alpha, tr);
		for (i = 0; i < 2; i++) {
			_test_conv_toeplitz_kernel_signal(ai_s[i], hrf[i], "test_toeplitz_convolution_kernel_signal_on_dirac " + i);	
		}
    }

    public void test_toeplitz_convolution_kernel_signal_on_ai_s() {
    	// No differences were found in test_toeplitz_convolution_kernel_signal_on_ai_s 0
    	// No differences were found in test_toeplitz_convolution_kernel_signal_on_ai_s 1
        // Test Toeplitz implementation of the convolution on a block signal,
        // the kernel being the block signal.
    	int i;
        double ai_s[][] = new double[2][];
        double hrf[][] = new double[2][];
        double alpha[][] = new double[2][];
        double tr[] = new double[2];
		yield_blocks_signal(ai_s, hrf, alpha, tr);
		for (i = 0; i < 2; i++) {
			_test_conv_toeplitz_kernel_signal(ai_s[i], hrf[i], "test_toeplitz_convolution_kernel_signal_on_ai_s " + i);	
		}
    }
    
    // adj op

    // Toeplitz


    public void _test_conv_adj_toeplitz_kernel_signal(double ai_s[], double hrf[], String testname) {
        // Helper to test the adj conv with a Toeplitz implementation,
        // the kernel being the block signal.
    	int i,j;
	    int numDifferences = 0;
        double H[][] = toeplitz_from_kernel(ai_s, hrf.length, ai_s.length);
        double Hdothrf[] = new double[H.length];
        for (i = 0; i < H.length; i++) {
        	for (j = 0; j < hrf.length; j++) {
        		Hdothrf[i] += (H[i][j] * hrf[j]);
        	}
        }
        double adj_ar_s_ref[] = simple_retro_convolve(ai_s, Hdothrf, hrf.length);
        double HT[][] = new double[H[0].length][H.length];
        for (i = 0; i < H.length; i++) {
        	for (j = 0; j < H[0].length; j++) {
        		HT[j][i] = H[i][j];
        	}
        }
        double adj_ar_test[] = new double[HT.length];
        for (i = 0; i < HT.length; i++) {
        	for (j = 0; j < HT[0].length; j++) {
        		adj_ar_test[i] += (HT[i][j] * Hdothrf[j]);
        	}
        }
        for (i = 0; i < adj_ar_s_ref.length; i++) {
	    	if (Math.abs(adj_ar_s_ref[i] - adj_ar_test[i]) > (1.0E-7 + 1.0E-5*Math.abs(adj_ar_test[i]))) {
	    	    numDifferences++;	
	    	}
	    }
	    if (numDifferences > 0) {
	    	System.err.println(numDifferences + " differences were found in " + testname);
	    }
	    else {
	    	System.out.println("No differences were found in " + testname);
	    }
    }

    public void test_toeplitz_adj_convolution_kernel_signal_on_dirac() {
    	// No differences were found in test_toeplitz_adj_convolution_kernel_signal_on_dirac 0
    	// No differences were found in test_toeplitz_adj_convolution_kernel_signal_on_dirac 1
        // Test Toeplitz implementation of the adj convolution on a single
        // dirac, the kernel being the block signal.
    	int i;
        double ai_s[][] = new double[2][];
        double hrf[][] = new double[2][];
        double alpha[][] = new double[2][];
        double tr[] = new double[2];
		yield_diracs_signal(ai_s, hrf, alpha, tr);
		for (i = 0; i < 2; i++) {
			_test_conv_adj_toeplitz_kernel_signal(ai_s[i], hrf[i], "test_toeplitz_adj_convolution_kernel_signal_on_dirac " + i);	
		}
    }

    public void test_toeplitz_adj_convolution_kernel_signal_on_ai_s() {
    	// No differences were found in test_toeplitz_adj_convolution_kernel_signal_on_ai_s 0
    	// No differences were found in test_toeplitz_adj_convolution_kernel_signal_on_ai_s 1
        // Test Toeplitz implementation of the adj convolution on a block
        // signal, the kernel being the block signal.
    	int i;
        double ai_s[][] = new double[2][];
        double hrf[][] = new double[2][];
        double alpha[][] = new double[2][];
        double tr[] = new double[2];
		yield_blocks_signal(ai_s, hrf, alpha, tr);
		for (i = 0; i < 2; i++) {
			_test_conv_adj_toeplitz_kernel_signal(ai_s[i], hrf[i], "test_toeplitz_adj_convolution_kernel_signal_on_ai_s " + i);	
		}
    }
    
    class DiscretInteg {
        // Intergrator operator.
        
        public DiscretInteg() {
        	
        }

        public double[] op(double x[]) {
            // Return time integration of x.

            // Parameters:
            // -----------
            // x : 1d np.ndarray,
            //    signal.

            // Results:
            // --------
            // integ_x : np.ndarray,
            //    the integrated 1d vector.
            int i;
            double integ_x[] = new double[x.length];
            integ_x[0] = x[0];
            for (i =1; i < integ_x.length; i++) {
            	integ_x[i] = integ_x[i-1] + x[i];
            }
            return integ_x;
        }

        public double[] adj(double x[]) {
            // Return adj time integrated of x.

            // Parameters:
            // -----------
            // x : 1d np.ndarray,
            //    signal.

            // Results:
            // --------
            // adj_integ_x : np.ndarray,
            //    the adj integretated 1d vector.
            int i;
            double flipx[] = new double[x.length];
            for (i = 0; i < x.length; i++) {
            	flipx[i] = x[x.length - 1 - i];
            }
            double cumx[] = new double[x.length];
            cumx[0] = flipx[0];
            for (i = 1; i < x.length; i++) {
            	cumx[i] = cumx[i-1] + flipx[i];
            }
            double adj_integ_x[] = new double[x.length];
            for (i = 0; i < x.length; i++) {
            	adj_integ_x[i] = cumx[x.length-i-1];
            }
            return adj_integ_x;
        }
    }
    
    public void test_integ_op() {
    	// No differences were found in test_integ_op
        // Test the  operator.
        
        DiscretInteg integ_op = new DiscretInteg();
        Random r = new Random();
        int i;
        double signal[] = new double[500];
        for (i = 0; i < 500; i++) {
        	signal[i] = r.nextGaussian();
        }
        double test_integ_signal[] = integ_op.op(signal);
        double ref_integ_signal[] = new double[500];
        ref_integ_signal[0] = signal[0];
        for (i = 1; i < 500; i++) {
        	ref_integ_signal[i] = ref_integ_signal[i-1] + signal[i];
        }
        int numDifferences = 0;
        for (i = 0; i < 500; i++) {
        	if (test_integ_signal[i] != ref_integ_signal[i]) {
        		numDifferences++;
        	}
        }

        if (numDifferences > 0) {
        	System.err.println(numDifferences + " differences were found in test_integ_op");
        }
        else {
        	System.out.println("No differences were found in test_integ_op");
        }
    }

    public void test_integ_adj() {
    	// No differences were found in test_integ_adj
        // Test the integ adj operator.
    	DiscretInteg integ_op = new DiscretInteg();
        Random r = new Random();
        int i;
        double signal[] = new double[500];
        for (i = 0; i < 500; i++) {
        	signal[i] = r.nextGaussian();
        }
        double test_integ_signal[] = integ_op.adj(signal);
        double flip_signal[] = new double[500];
        for (i = 0; i < 500; i++) {
        	flip_signal[i] = signal[500 - i - 1];
        }
        double cum_signal[] = new double[500];
        cum_signal[0] = flip_signal[0];
        for (i = 1; i < 500; i++) {
        	cum_signal[i] = cum_signal[i-1] + flip_signal[i];
        }
        double ref_integ_signal[] = new double[500];
        for (i = 0; i < 500; i++) {
        	ref_integ_signal[i] = cum_signal[500 - i - 1];
        }
        int numDifferences = 0;
        for (i = 0; i < 500; i++) {
        	if (test_integ_signal[i] != ref_integ_signal[i]) {
        		numDifferences++;
        	}
        }

        if (numDifferences > 0) {
        	System.err.println(numDifferences + " differences were found in test_integ_adj");
        }
        else {
        	System.out.println("No differences were found in test_integ_adj");
        }
    }
    
    class ConvAndLinear {
        // Linear (Matrix) operator followed by a convolution.
        double k[];
        double K[][];
        double K_T[][];
        boolean spectral_conv = false;
        DiscretInteg di;
        public ConvAndLinear(double kernel[], int dim_in, int dim_out, boolean spectral_conv) {
            // ConvAndLinear linear operator class.
            // Parameters:
            // -----------

            // kernel : 1d np.ndarray,
            //    kernel.

            // dim_in : int,
            //    chosen convolution input dimension.

            // dim_out : int (default None),
            //    chosen convolution ouput dimension.
        	
        	// boolean spectral_conv default = false;
            
            di = new DiscretInteg();
            k = kernel;
            this.spectral_conv = spectral_conv;
            if (!spectral_conv) {
            	int i,j;
                K = toeplitz_from_kernel(k, dim_in, dim_out);
                K_T = new double[K[0].length][K.length];
                for (i = 0; i < K.length; i++) {
                	for (j = 0; j < K[0].length; j++) {
                		K_T[j][i] = K[i][j];
                	}
                }
            }
        }
                

        public double[] op(double x[]) {
            // Return k.convolve(D_hrf.dot(x)).

            // Parameters:
            // -----------
            // x : 1d np.ndarray,
            //    signal.

            // Results:
            // --------
            // img_x : np.ndarray,
            //    the resulting 1d vector.
            
            double bloc_signal[] = di.op(x);
        	int i,j;
        	double convolved_signal[];

            if (spectral_conv) {
                convolved_signal = spectral_convolve(k, bloc_signal);
            }
            else {
            	convolved_signal = new double[K.length];
            	for (i = 0; i < K.length; i++) {
            		for (j = 0; j < K[0].length; j++) {
            			convolved_signal[i] += (K[i][j]*bloc_signal[j]);
            		}
            	}
            	
            }

            return convolved_signal;
        }

        public double[] adj(double x[]) {
            // Return k.T.convolve(D_hrf.T.dot(x)).

            // Parameters:
            // -----------
            // x : 1d np.ndarray,
            //    signal.

            // Results:
            // --------
            // img_x : np.ndarray,
            //    the resulting 1d vector.
        	int i,j;
            double retro_convolved_signal[];
            if (spectral_conv) {
                retro_convolved_signal = spectral_retro_convolve(k, x);
            }
            else {
            	retro_convolved_signal = new double[K_T.length];
            	for (i = 0; i < K_T.length; i++) {
            		for (j = 0; j < K_T[0].length; j++) {
            			retro_convolved_signal[i] += (K_T[i][j]*x[j]);
            		}
            	}
            }

            return di.adj(retro_convolved_signal);
        }
    }
    
    public double sign(double x) {
    	if (Double.isNaN(x)) {
    		return Double.NaN;
    	}
    	else if (x > 0.0) {
    		return 1.0;
    	}
    	else if (x < 0.0) {
    		return -1.0;
    	}
    	else {
    		return 0.0;
    	}
    }

    public void deconv(double x[], double z[], double diff_z[], Vector<Double> J, Vector<Double> R, Vector<Double> G,
    		double y[], double t_r, double hrf[], double lbda, boolean early_stopping, double tol,  // noqa
            int wind, int nb_iter, int nb_sub_iter, int verbose) {
	     // Deconvolve the given BOLD signal given an HRF convolution kernel.
	     // The source signal is supposed to be a bloc signal.
	
	     // Parameters:
	     // ----------
	     // y : 1d np.ndarray,
	     //    the observed bold signal.
	
	     // t_r : float,
	     //    the TR.
	
	     // hrf : 1d np.ndarray,
	     //    the HRF.
	
	     // lbda : float (default=Double.NaN), otherwise 1.0 is the second default
	     //    the regularization parameter.
    	
    	// boolean early_stopping default = true;
    	
    	// double tol default = 1.0E_6
    	
    	// int wind default = 6
    	
    	// int nb_iter default = 1000
    	
    	// int nb_sub_iter default = 1000
	
	     // verbose : int (default=0),
	     //    the verbosity level.
	
	     // Return:
	     // ------
	     // x : 1d np.ndarray, same length as y
	     //    the estimated convolved signal.
	
	     // z : 1d np.ndarray, same length as y
	     //    the estimated convolved signal.
	
	     // diff_z : 1d np.ndarray, same length as y
	     //    the estimated convolved signal.
	
	     // J : Vector<Double>,
	     //    the evolution of the cost-function.
    	
    	// R: Vector<Double>
    	
    	// G: Vector<Double>
	     
    	 int i,j;
    	 for (i = 0; i < diff_z.length; i++) {
    		 diff_z[i] = 0;
    	 }
         //diff_z = np.zeros_like(y)
    	 double diff_z_old[] = new double[y.length];
    	 double t;
         double t_old;
         Vector<double[]> xx = new Vector<double[]>();
	     ConvAndLinear H = new ConvAndLinear(hrf, y.length, y.length, false);
	     double H_adj_y[] = H.adj(y);
	     double grad_lipschitz_cst = 0.9 * spectral_radius_est(H, diff_z.length, 30, 1.0E-6, false);
	     double step = 1.0 / grad_lipschitz_cst;
	     double xcopy[] = new double[x.length];
	     
	     if (!Double.isNaN(lbda)) {

	         double th = lbda / grad_lipschitz_cst;
	         ;
	         J.clear();
	        
	         t = t_old = 1;
	         int idx;
	         double grad1[];
	         double grad[] = new double[y.length];
	         double diff;
	         double sum1;
	         double sum2;
	         int sub_wind_len;
	         double old_iter[] = new double[y.length];
	         double new_iter[] = new double[y.length];
	         double diff_iter[] = new double[y.length];
	         int old_iter_size;
	         int new_iter_size;
	         double crit_num;
	         double crit_deno;
	         double Jdenom;

	         for (idx = 0; idx < nb_iter; idx++) {
                 grad1 = H.adj(H.op(diff_z));
                 for (i = 0; i < y.length; i++) {
                	 grad[i] = grad1[i] - H_adj_y[i];
                	 diff_z[i] -= step * grad[i];
                	 diff_z[i] = sign(diff_z[i]) * Math.max(Math.abs(diff_z[i]) - th, 0);
                 }

	             t = 0.5 * (1.0 + Math.sqrt(1 + 4*t_old*t_old));
	             for (i = 0; i < y.length; i++) {
	                 diff_z[i] = diff_z[i] + (t_old-1)/t * (diff_z[i] - diff_z_old[i]);
	             }

	             t_old = t;
	             for (i = 0; i < y.length; i++) {
	                 diff_z_old[i] = diff_z[i];
	             }

	             z[0] = diff_z[0];
	             for (i = 1; i < y.length; i++) {
	            	 z[i] = z[i-1] + diff_z[i];
	             }
	             xcopy = spectral_convolve(hrf, z);
	             for (i = 0; i < y.length; i++) {
	            	 x[i] = xcopy[i];
	             }
	             sum1 = 0.0;
	             sum2 = 0.0;
	             for (i = 0; i < y.length; i++) {
	                 diff = x[i] - y[i];
	                 sum1 += (diff * diff);
	                 sum2 += Math.abs(diff_z[i]);
	             }
	             J.add(0.5*sum1 + lbda*sum2);

	             System.out.println("Main loop: iteration " + idx + " |grad| = " + norm(grad)
	                   + " j = " + J.get(idx));

	             xx.add(diff_z_old);
	             if (xx.size() > wind) {
	                 xx.remove(0);
	             }

	             if (early_stopping) {
	                 if (idx > wind) {
	                     sub_wind_len = (int)(wind/2);
	                     sum1 = 0.0;
	                     for (i = 0; i < y.length; i++) {
	                    	 old_iter[i] = 0;
	                    	 new_iter[i] = 0;
	                     }
	                     old_iter_size = Math.max(xx.size()-sub_wind_len, 0);
	                     for (i = 0; i < old_iter_size; i++) {
	                         for (j = 0; j < y.length; j++) {
	                        	 old_iter[j] += xx.get(i)[j];
	                         }
	                     }
	                     if (old_iter_size > 1) {
	                    	 for (i = 0; i < y.length; i++) {
	                    		 old_iter[i] = old_iter[i]/old_iter_size;
	                    	 }
	                     }
	                     new_iter_size = sub_wind_len;
	                     for (i = xx.size()-sub_wind_len; i < xx.size(); i++) {
	                    	 for (j = 0; j < y.length; j++) {
	                    		 new_iter[j] += xx.get(i)[j];
	                    	 }
	                     }
	                     if (new_iter_size > 1) {
	                    	 for (i = 0; i < y.length; i++) {
	                    		 new_iter[i] = new_iter[i]/new_iter_size;
	                    	 }
	                     }
	                     for (i = 0; i < y.length; i++) {
	                    	 diff_iter[i] = new_iter[i] - old_iter[i];
	                     }
	                     crit_num = norm(diff_iter);
	                     crit_deno = norm(new_iter);
	                     diff = crit_num / (crit_deno + 1.0e-10);
	                     if (diff < tol) {
	                         break;
	                     }
	                 } // if (idx > wind)
	             } // if (early_stopping)
	         } // for (idx = 0; idx < nb_iter; idx++)
	         Jdenom = J.get(0) + 1.0E-30;
             for (i = 0; i < J.size(); i++) {
                 J.set(i, J.get(i)/Jdenom);	 
             }
	         return; // x, z, diff_z, np.array(J) / (J[0] + 1.0e-30), None, None
	     } // if (!Double.isNaN(lbda))
	     else {
	    	 double sigma;
	    	 double alpha;
	    	 double mu;
	    	 double th;
	    	 double diff_z1[];
	    	 int k,m;
	         Vector<Double>l_alpha = new Vector<Double>();
	         J.clear();
	         R.clear();
	         G.clear();
	         double diff;
	         int sub_wind_len;
	         double old_iter[] = new double[y.length];
	         double new_iter[] = new double[y.length];
	         double diff_iter[] = new double[y.length];
	         int old_iter_size;
	         int new_iter_size;
	         double crit_num;
	         double crit_deno;
	         double grad;
	         double r;
	         double g;
	         double old_iter_scalar;
	         double new_iter_scalar;
	         sigma = mad_daub_noise_est(y, 0.6744);
	         alpha = 1.0;
	         lbda = 1.0 / (2.0 * alpha);
	         mu = 1.0e-4;
	         
	         for (i = 0; i < nb_iter; i++) {

	             // deconvolution step
	             th = lbda / grad_lipschitz_cst;
	             xx.clear();
	             t = t_old = 1;

	             for (j = 0; j < nb_sub_iter; j++) {

	            	 diff_z1 = H.adj(H.op(diff_z));
	            	 for (k = 0; k < y.length; k++) {
	            		 diff_z[k] -= step * (diff_z1[k] - H_adj_y[k]);
	            		 diff_z[k] = sign(diff_z[k]) * Math.max(Math.abs(diff_z[k]) - th, 0.0);
	            	 }

	                 t = 0.5 * (1.0 + Math.sqrt(1 + 4*t_old*t_old));
	                 for (k = 0; k < y.length; k++) {
	                     diff_z[k] = diff_z[k] + (t_old-1)/t * (diff_z[k] - diff_z_old[k]);
	                 }

	                 t_old = t;
	                 for (k = 0; k < y.length; k++) {
	                     diff_z_old[k] = diff_z[k];
	                 }

	                 xx.add(diff_z_old);
	                 if (xx.size() > wind) {
	                	 xx.remove(0);
	                 }

	                 if (early_stopping) {
	                     if (j > wind) {
	                    	 sub_wind_len = (int)(wind/2);
		                     for (k = 0; k < y.length; k++) {
		                    	 old_iter[k] = 0;
		                    	 new_iter[k] = 0;
		                     }
		                     old_iter_size = Math.max(xx.size()-sub_wind_len, 0);
		                     for (k = 0; k < old_iter_size; k++) {
		                         for (m = 0; m < y.length; m++) {
		                        	 old_iter[m] += xx.get(k)[m];
		                         }
		                     }
		                     if (old_iter_size > 1) {
		                    	 for (k = 0; k < y.length; k++) {
		                    		 old_iter[k] = old_iter[k]/old_iter_size;
		                    	 }
		                     }
		                     new_iter_size = sub_wind_len;
		                     for (k = xx.size()-sub_wind_len; k < xx.size(); k++) {
		                    	 for (m = 0; m < y.length; m++) {
		                    		 new_iter[m] += xx.get(k)[m];
		                    	 }
		                     }
		                     if (new_iter_size > 1) {
		                    	 for (k = 0; k < y.length; k++) {
		                    		 new_iter[k] = new_iter[k]/new_iter_size;
		                    	 }
		                     }
		                     for (k = 0; k < y.length; k++) {
		                    	 diff_iter[k] = new_iter[k] - old_iter[k];
		                     }
		                     crit_num = norm(diff_iter);
		                     crit_deno = norm(new_iter);
		                     diff = crit_num / (crit_deno + 1.0e-10);
		                     if (diff < tol) {
		                         break;
		                     }
	                     } // if (j > wind)
	                 } // if (early_stopping)
	             } // for (j = 0; j < nb_sub_iter; j++)

	             // lambda optimization
	             z[0] = diff_z[0];
	             for (k = 1; k < y.length; k++) {
	            	 z[k] = z[k-1] + diff_z[k];
	             }
	             xcopy = spectral_convolve(hrf, z);
	             for (k = 0; k < y.length; k++) {
	            	 x[i] = xcopy[i];
	             }
	             
	             grad = 0.0;
	             for (k = 0; k < y.length; k++) {
	            	 diff = x[k] - y[k];
	            	 grad += (diff * diff);
	             }
	             grad = grad - y.length * sigma*sigma;
	             alpha += mu * grad;
	             lbda = 1.0 / (2.0 * alpha);

	             // iterate update and saving
	             l_alpha.add(alpha);
	             if (l_alpha.size() > wind) {  // # only hold the 'wind' last iterates
	                 l_alpha.remove(0);
	             }

	             // metrics evolution
	             r = 0.0;
	             g = 0.0;
	             for (k = 0; k < y.length; k++) {
	            	 diff = x[k] - y[k];
	            	 r += (diff * diff);
	            	 g += Math.abs(diff_z[k]);
	             }
	             R.add(r);
	             G.add(g);
	             J.add(0.5 * r + lbda * g);
	             if (verbose > 0) {
	                 System.out.println("Main loop: iteration " + (i+1) + " |grad| = " + Math.abs(grad) + " lbda = " + lbda);
	             } // if (verbose > 0)

	             // early stopping
	             if (early_stopping) {
	                 if (i > wind) {
	                     sub_wind_len = (int)(wind/2);
	                     old_iter_scalar = 0.0;
	                     old_iter_size = Math.max(l_alpha.size()-sub_wind_len, 0);
	                     for (k = 0; k < old_iter_size; k++) {
	                         old_iter_scalar += l_alpha.get(k);	 
	                     }
	                     if (old_iter_size > 1) {
	                    	 old_iter_scalar = old_iter_scalar/old_iter_size;
	                     }
	                     new_iter_scalar = 0.0;
	                     new_iter_size = sub_wind_len;
	                     for (k = l_alpha.size() - sub_wind_len; k < l_alpha.size(); k++) {
	                    	 new_iter_scalar += l_alpha.get(k);
	                     }
	                     if (new_iter_size > 1) {
	                    	 new_iter_scalar = new_iter_scalar/new_iter_size;
	                     }
	                     crit_num = Math.abs(new_iter_scalar - old_iter_scalar);
	                     crit_deno = Math.abs(new_iter_scalar);
	                     diff = crit_num / crit_deno;
	                     if (diff < tol) {
	                         if (verbose > 1) {
	                             System.out.println("\n-----> early-stopping done at " + i + "/" + nb_iter);
	                             System.out.println("Cost function = " + J.get(i));
	                         break;
	                         } // if (verbose > 1)
	                     } // if (dif < tol)
	                 } // if (i > wind)
	             } // if (early_stopping)
	         } // for (i = 0; i < nb_iter; i++)
	         
	         // last deconvolution with larger number of iterations
	         th = lbda / grad_lipschitz_cst;
	         xx.clear();
	         t = t_old = 1;

	         for (j = 0; j < nb_sub_iter; j++) {

	        	 diff_z1 = H.adj(H.op(diff_z));
            	 for (k = 0; k < y.length; k++) {
            		 diff_z[k] -= step * (diff_z1[k] - H_adj_y[k]);
            		 diff_z[k] = sign(diff_z[k]) * Math.max(Math.abs(diff_z[k]) - th, 0.0);
            	 }

	             t = 0.5 * (1.0 + Math.sqrt(1 + 4*t_old*t_old));
	             for (k = 0; k < y.length; k++) {
	                 diff_z[k] = diff_z[k] + (t_old-1)/t * (diff_z[k] - diff_z_old[k]);
	             }

	             t_old = t;
	             for (k = 0; k < y.length; k++) {
	                 diff_z_old[k] = diff_z[k];
	             }

	             xx.add(diff_z_old);
	             if (xx.size() > wind) {
	                 xx.remove(0);
	             }

	                 if (early_stopping) {
	                     if (j > wind) {
	                    	 sub_wind_len = (int)(wind/2);
		                     for (k = 0; k < y.length; k++) {
		                    	 old_iter[k] = 0;
		                    	 new_iter[k] = 0;
		                     }
		                     old_iter_size = Math.max(xx.size()-sub_wind_len, 0);
		                     for (k = 0; k < old_iter_size; k++) {
		                         for (m = 0; m < y.length; m++) {
		                        	 old_iter[m] += xx.get(k)[m];
		                         }
		                     }
		                     if (old_iter_size > 1) {
		                    	 for (k = 0; k < y.length; k++) {
		                    		 old_iter[k] = old_iter[k]/old_iter_size;
		                    	 }
		                     }
		                     new_iter_size = sub_wind_len;
		                     for (k = xx.size()-sub_wind_len; k < xx.size(); k++) {
		                    	 for (m = 0; m < y.length; m++) {
		                    		 new_iter[m] += xx.get(k)[m];
		                    	 }
		                     }
		                     if (new_iter_size > 1) {
		                    	 for (k = 0; k < y.length; k++) {
		                    		 new_iter[k] = new_iter[k]/new_iter_size;
		                    	 }
		                     }
		                     for (k = 0; k < y.length; k++) {
		                    	 diff_iter[k] = new_iter[k] - old_iter[k];
		                     }
		                     crit_num = norm(diff_iter);
		                     crit_deno = norm(new_iter);
		                     diff = crit_num / (crit_deno + 1.0e-10);
		                     if (diff < tol) {
		                         break;
		                     }
	                     } // if (j > wind)
	                 } // if (early_stopping)
	         } // for (j = 0; j < nb_sub_iter; j++)

	         z[0] = diff_z[0];
	         for (k = 1; k < y.length; k++) {
	        	 z[k] = z[k-1] + diff_z[k];
	         }
	         xcopy = spectral_convolve(hrf, z);
	         for (i = 0; i < y.length; i++) {
	        	 x[i] = xcopy[i];
	         }

	         return;  // x, z, diff_z, J, R, G


	     } // else 

    } // deconv
    
    public double std(double x[]) {
    	int i;
    	double mean = 0.0;
    	for (i = 0; i < x.length; i++) {
    		mean += x[i];
    	}
    	mean = mean/x.length;
    	double stdev = 0.0;
    	double diff;
    	for (i = 0; i < x.length; i++) {
    		diff = x[i] - mean;
    		stdev += (diff * diff);
    	}
    	stdev = Math.sqrt(stdev/x.length);
    	return stdev;
    }
    
    public void deconv_example() {
    	// deconv duration in seconds = 1.162
    	// Standard deviation of noise = 2.131560152061249
    	// Standard deviation of est_noise = 2.429113187614236
    	// Euclidean norm of noise = 52.220970328069214
    	// Euclidean norm of est_noise = 59.51527182299158
    	
    	int i;
    	// generate data
    	double hrf_dur = 30.0;
    	int dur = 10;  // minutes
    	double TR = 1.0;
    	double snr = 1.0;

    	// True HRF
    	double true_hrf_delta = 1.5;
    	double dt = 0.001;
    	int numsamples = (int)(hrf_dur/ dt);
    	int step = (int)(TR/dt);
    	int returnedsamples = ((numsamples-1)/step) + 1; // length of orig_hrf[] and t_hrf[]
    	double orig_hrf[] = new double[returnedsamples];
    	double t_hrf[] = new double[returnedsamples];
    	boolean normalized_hrf = true;
    	double p_delay = 6.0;
    	double undershoot = 16.0;
    	double p_disp = 1.0;
    	double u_disp = 1.0;
    	double p_u_ratio = 0.167;
    	double onset = 0.0;
    	spm_hrf(orig_hrf, t_hrf, true_hrf_delta, TR,
    				    hrf_dur, normalized_hrf, dt, p_delay,
    		            undershoot, p_disp, u_disp, p_u_ratio, onset);
    			
    	boolean sourcehrf = true;
    	boolean haveSeed = true;
    	long random_state = 0L;
    	// For noisy_ar_s, ar_s, ai_s, i_s, t, noise length is int N = (int)(dur * 60 / TR);
    	int N = (int)(dur * 60 / TR);
    	double noisy_ar_s[] = new double[N];
    	double ar_s[] = new double[N];
    	double ai_s[] = new double[N];
    	double i_s[] = new double[N];
    	double t[] = new double[N];
    	double noise[] = new double[N];
    	gen_regular_bloc_bold(noisy_ar_s, ar_s, ai_s,
    		    		i_s, t, noise, dur, TR, hrf_dur,
    		    		sourcehrf, orig_hrf, snr, haveSeed, random_state);
    				
    	// deconvolve the signal
    	int nb_iter = 100;

    	long t0 = System.currentTimeMillis();
    	double est_ar_s[] = new double[N];
    	double est_ai_s[] = new double[N];
    	double est_i_s[] = new double[N];
    	Vector<Double> J = new Vector<Double>();
    	Vector<Double> R = new Vector<Double>();
    	Vector<Double> G = new Vector<Double>();
    	double lbda = Double.NaN;
    	boolean early_stopping = true;
    	double tol = 1.0E-6;
    	int wind = 6;
    	int nb_sub_iter = 1000;
    	int verbose = 1;
    	deconv(est_ar_s, est_ai_s, est_i_s, J, R, G,
    		    		noisy_ar_s, TR, orig_hrf, lbda, early_stopping, tol, 
    		            wind, nb_iter, nb_sub_iter, verbose);
    			     
    	double delta_t = (System.currentTimeMillis() - t0)/1000.0;
    	double runtimes[] = new double[J.size()];
    	runtimes[0] = 0;
    	runtimes[J.size()-1] = delta_t;
    	double inc = delta_t/(J.size() - 1.0);
    	for (i = 1; i < J.size() -1 ; i++) {
    		runtimes[i] = i*inc;
    	}

    	System.out.println("deconv duration in seconds = " + delta_t);

    	double est_noise[] = new double[N];
    	for (i = 0; i < N; i++) {
    		est_noise[i] = noisy_ar_s[i] - est_ar_s[i];
    	}
    	System.out.println("Standard deviation of noise = " + std(noise));
    	System.out.println("Standard deviation of est_noise = " + std(est_noise));
    	System.out.println("Euclidean norm of noise = " + norm(noise));
    	System.out.println("Euclidean norm of est_noise = " + norm(est_noise));
    	
        float xInit[][] = new float[2][N];
        float yInit[][] = new float[2][N];
        for (i = 0; i < N; i++) {
        	xInit[0][i] = (float)t[i];
        	xInit[1][i] = (float)t[i];
        	yInit[0][i] = (float)noisy_ar_s[i]; // "Noisy activation related signal, snr= " + snr + " dB";
        	yInit[1][i] = (float)est_ar_s[i]; // label="Est. activation related signal"
        }
        String title = "Input noisy BOLD signals, TR = " + TR + " s";
        String labelX = "time (s)";
        String labelY = "ampl.";
        Color colorArray[] = new Color[] {Color.YELLOW, Color.GREEN};
        new ViewJFrameGraph(xInit, yInit, title, labelX, labelY, colorArray);
        
        float xInit2[][] = new float[2][N];
        float yInit2[][] = new float[2][N];
        for (i = 0; i < N; i++) {
        	xInit2[0][i] = (float)t[i];
        	xInit2[1][i] = (float)t[i];
        	yInit2[0][i] = (float)ar_s[i]; // "Orig. activation related signal"
        	yInit2[1][i] = (float)est_ar_s[i]; // label="Est. activation related signal"
        }
        String title2 = "Estimated convolved signals, TR = " + TR + " s";
        String labelX2 = "time (s)";
        String labelY2 = "ampl.";
        Color colorArray2[] = new Color[] {Color.BLUE, Color.GREEN};
        new ViewJFrameGraph(xInit2, yInit2, title2, labelX2, labelY2, colorArray2);
        
        float xInit3[][] = new float[2][N];
        float yInit3[][] = new float[2][N];
        for (i = 0; i < N; i++) {
        	xInit3[0][i] = (float)t[i];
        	xInit3[1][i] = (float)t[i];
        	yInit3[0][i] = (float)ai_s[i]; // "Orig. activation inducing signal, snr = " + snr + " dB".;
        	yInit3[1][i] = (float)est_ai_s[i]; // label="Est. activation inducing signal"
        }
        String title3 = "Estimated signals, TR = " + TR + " s";
        String labelX3 = "time (s)";
        String labelY3 = "ampl.";
        Color colorArray3[] = new Color[] {Color.BLUE, Color.GREEN};
        new ViewJFrameGraph(xInit3, yInit3, title3, labelX3, labelY3, colorArray3);
    }
    
    public double hrf_fit_err(double theta, double z[], double y[], double t_r, double hrf_dur) {
        // Cost function for the scaled-gamma HRF model.
        // e.g. 0.5 * || h*x - y ||_2^2 with h an HRF model.
    	int i;
    	double diff;
    	double sum;
    	double dt = 0.001;
        boolean normalized_hrf = false;
        int numsamples = (int)(hrf_dur/ dt);
        int step = (int)(t_r/dt);
        int returnedsamples = ((numsamples-1)/step) + 1; // h[] and t_hrf[]
        double h[] = new double[returnedsamples];
        double t_hrf[] = new double[returnedsamples];
        double p_delay = 6.0;
    	double undershoot = 16.0;
    	double p_disp = 1.0;
    	double u_disp = 1.0;
    	double p_u_ratio = 0.167;
    	double onset = 0.0;
        spm_hrf(h, t_hrf, theta, t_r, hrf_dur, normalized_hrf, dt,
        		p_delay, undershoot, p_disp, u_disp, p_u_ratio, onset);
        double x[] = spectral_convolve(h, z);
        sum = 0.0;
        for (i = 0; i < y.length; i++) {
        	diff = y[i] - x[i];
        	sum += (diff * diff);
        }
        return (0.5 * sum);
    }
    
    class Tracker {
        // Callback class to be used with optimization function from Scipy.
        public Vector<Double> J;
        // f = hrf_fit_err
        double z[];
        double y[];
        double t_r;
        double dur;
        int verbose;
        int idx;
        public Tracker(Vector<Double>J, double z[], double y[], double t_r, double dur, int verbose) {
        	// default verbose = 0;
            this.J = J;
            this.z = z;
            this.y = y;
            this.t_r = t_r;
            this.dur = dur;
            this.verbose = verbose;
            idx = 0;
        }

        public void __call__(double x) {
            idx += 1;
            double j = hrf_fit_err(x, z, y, t_r, dur);
            if (verbose > 2) {
                System.out.println("At iterate " + idx + ", tracked function = " + j);
            }
            J.add(j);
        }
    }


    public void hrf_estim(double h[], Vector<Double>J, double z[], double y[],
    		double t_r, double dur, int verbose) {
    	// Default verbose = 0
        // Private function HRF estimation.
       
        double bounds[] = new double[] {MIN_DELTA + 1.0e-1, MAX_DELTA - 1.0e-1};
        Tracker f_cost = new Tracker(J, z, y, t_r, dur, verbose);
        
        //      We specify the dimension n of the sample problem and the number
     	//        m of limited memory corrections stored.  (n and m should not
     	//        exceed the limits nmax and mmax respectively.)
     	 
     	int n=1;
     	int m=10;
        //      We suppress the default output.  (The user could also elect to 
     	//       use the default output by choosing iprint >= 0.)

     	int iprint = 100;
     	String task[] = new String[1];
	    String csave[] = new String[1];
	    boolean lsave[] = new boolean[4];
	    int nbd[] = new int[n];
	    int isave[] = new int[23];
	    double f[] = new double[1];
	    double factr = 0.0; 
	    double pgtol = 1.0E-12; 
	    
	    // mainlb arguments
	    double x[] = new double[n];
	    double l[] = new double[n];
	    double u[] = new double[n];
	    double g[] = new double[n]; 
	    double dsave[] = new double[29];
	    double ws[][] = new double[n][m];
	    double wy[][] = new double[n][m];
	    double sy[][] = new double[m][m];
	    double ss[][] = new double[m][m];
	    double wt[][] = new double[m][m];
	    double wn[][] = new double[2*m][2*m];
	    double snd[][] = new double[2*m][2*m];
	    double zbuf[] = new double[n];
	    double r[] = new double[n];
	    double d[] = new double[n];
	    double t[] = new double[n];
	    double xp[] = new double[n];
	    double wa1[] = new double[2*m];
	    double wa2[] = new double[2*m];
	    double wa3[] = new double[2*m];
	    double wa4[] = new double[2*m];
	    int index[] = new int[n];
	    int iwhere[] = new int[n];
	    int indx2[] = new int[n];
	    
        //	     We now specify nbd which defines the bounds on the variables:
	    //                    l   specifies the lower bounds,
	    //                    u   specifies the upper bounds.
	    nbd[0] = 2; // both a lower and upper bound
	    l[0] = bounds[0];
	    u[0] = bounds[1];
        //	     We now define the starting point.
	    x[0] = MAX_DELTA;
	    System.out.println("Initial x[0] = " + x[0]);
	    
        //	     We start the iteration by initializing task.
	     
        task[0] = "START";
        double lower_dist;
        double upper_dist;
        double originalStep = 1.0E-8;
        boolean central;
        boolean forward;
        boolean backward;
        double adjustedForwardStep;
        double adjustedBackwardStep;
        double min_dist;
        boolean adjusted_central;
        RandomAccessFile raFile = null;
        String fileDir = "C:/L-BFGS-B/";
	           if (iprint >= 1) {
	       		//                                open a summary file 'iterate.dat'
	            File file = new File(fileDir + "iterate.data");
	            try {
	                raFile = new RandomAccessFile(file, "rw");
	            }
	            catch (FileNotFoundException e) {
	            	System.err.println("At raFile = new Random AccessFile(file, rw) FileNotException " + e);
	            	System.exit(-1);
	            }
	            catch (SecurityException e2) {
	            	System.err.println("At raFile = new Random AccessFile(file, rw) SecurityException " + e2);
	            	System.exit(-1);	
	            }
	            // Necessary so that if this is an overwritten file there isn't any
	            // junk at the end
	            try {
	                raFile.setLength(0);
	            }
	            catch (IOException e) {
	            	System.err.println("At raFile.setLength(0) IOException " + e);
	            	System.exit(-1);	
	            }
	         } // if (iprint >= 1)   
        L_BFGS_B LBB = new L_BFGS_B();
        
        //      ------- the beginning of the loop ----------
		 
		 do {
		      
		//     This is the call to the L-BFGS-B code.
	         LBB.mainlb(n,m,x,l,u,nbd,f,g,factr,pgtol,
		             ws,wy,sy,ss,wt,
		             wn,snd,zbuf,r,d,t,xp,wa1,wa2,wa3,wa4,
		             index, iwhere,indx2,task,iprint, 
		             csave,lsave,isave,dsave,raFile);
	         System.out.println("x[0] = " + x[0]);
	         
	         if ((task[0].length() >= 2) && (task[0].substring(0,2).equalsIgnoreCase("FG"))) {
	     		//        the minimization routine has returned to request the
	     		//        function f and gradient g values at the current x.

	     		//        Compute function value f for the sample problem.

	     	        	  f_cost.__call__(x[0]);
	     	        	  f[0] = J.get(J.size()-1);
                //	     	           Compute gradient g for the sample problem
	     	        	  
	     	        	  lower_dist = x[0] - l[0];
	     	              upper_dist = u[0] - x[0];
	     	              central = (lower_dist >= originalStep) && (upper_dist >= originalStep);
	     	              if (central) {
	     	            	  g[0] = (hrf_fit_err(x[0]+originalStep, z, y, t_r, dur) 
	     	            			  - hrf_fit_err(x[0]-originalStep, z, y, t_r, dur))/(2.0*originalStep);
	     	            	  continue;
	     	              }
	     	              forward = upper_dist >= lower_dist;
	     	              adjustedForwardStep = Math.min(originalStep, 0.5 *upper_dist);
	     	              if (forward) {
	     	                  g[0] = (hrf_fit_err(x[0] + adjustedForwardStep, z, y, t_r, dur) - f[0])/adjustedForwardStep;
	     	                  continue;
	     	              }
	     	              backward = upper_dist < lower_dist;
	     	              adjustedBackwardStep = Math.min(originalStep, 0.5*lower_dist);
                          if (backward) {
                              g[0] = (f[0] - hrf_fit_err(x[0] - adjustedBackwardStep, z, y, t_r, dur))/adjustedBackwardStep;
	     	                  continue;
                          }
                          min_dist = Math.min(upper_dist, lower_dist);
                          adjusted_central = (Math.abs(adjustedForwardStep) <= min_dist) && (Math.abs(adjustedBackwardStep) <= min_dist);
	     		          if (adjusted_central) {
	     		        	 g[0] = (hrf_fit_err(x[0]+min_dist, z, y, t_r, dur) - hrf_fit_err(x[0]-min_dist, z, y, t_r, dur))/(2.0*min_dist);
	     	            	 continue;
	     		          }

	     	              System.err.println("Could not calculate g[0]");
	     	              System.exit(-1);
	     	         } // if ((task[0].length() >= 2) && (task[0].substring(0,2).equalsIgnoreCase("FG")))
	     		
	     	         if ((task[0].length() >= 5) && (task[0].substring(0,5).equalsIgnoreCase("NEW_X"))) {   
	     		     
	     		//        the minimization routine has returned with a new iterate.
	     		//        At this point have the opportunity of stopping the iteration 
	     		//        or observing the values of certain parameters
	     		

	     		//        Note: task(1:4) must be assigned the value 'STOP' to terminate  
	     		//          the iteration and ensure that the final results are
	     		//          printed in the default format. The rest of the character
	     		//          string TASK may be used to store other information.

	     		//        1) Terminate if the total number of f and g evaluations
	     		//             exceeds 99999.

	     		         if (isave[12] >= 99999) {
	     		             task[0] = "STOP: TOTAL NO. of f AND g EVALUATIONS EXCEEDS LIMIT";
	     		         }


	     		//        We now wish to print the following information at each
	     		//        iteration:
	     		        
	     		//          1) the current iteration number, isave(30),
	     		//          2) the total number of f and g evaluations, isave(34),
	     		//          3) the value of the objective function f,
	     		//          4) the norm of the projected gradient,  dsve(13)
	     		
	     		//        See the comments at the end of driver1 for a description
	     		//        of the variables isave and dsave.
	     		         
	     		         //System.out.println("Current iteration number = " + isave[8]);
	     		         //System.out.println("Total number of f and g evaluations = " + isave[12]);
	     		         //System.out.println("Value of the objective function f = " + f[0]);
	     		         //System.out.println("Norm of the projected gradient = " + dsave[12]);

	     		//        If the run is to be terminated, we print also the information
	     		//        contained in task as well as the final value of x.

	     		         if (task[0].substring(0,4).equals("STOP")) {
	     		            System.out.println("task[0] = " + task[0]);
	     		            System.out.print("Final X = " + x[0]);
	     		         } // if (task[0].substring(0,4).equals("STOP"))

	     		//          go back to the minimization routine.
	     		         continue;

	     	         } // if ((task[0].length() >= 5) && (task[0].substring(0,5).equalsIgnoreCase("NEW_X")))

	     		//           ---------- the end of the loop -------------
	     		 
	     		//     If task is neither FG nor NEW_X we terminate execution.

	     		      break;
	     		 } while (true);
        

        /*theta, _, _ = fmin_l_bfgs_b(
                            func=hrf_fit_err, x0=MAX_DELTA, args=args,
                            bounds=bounds, approx_grad=True, callback=f_cost,
                            maxiter=99999, pgtol=1.0e-12)*/
        double theta = x[0];
        System.out.println("Final x[0] = " + x[0]);
        double dt = 0.001;
        int numsamples = (int)(dur/ dt);
        int step = (int)(t_r/dt);
        int returnedsamples = ((numsamples-1)/step) + 1; // h[] and t_hrf[]
        double t_hrf[] = new double[returnedsamples];
        boolean normalized_hrf = false;
        double p_delay = 6.0;
    	double undershoot = 16.0;
    	double p_disp = 1.0;
    	double u_disp = 1.0;
    	double p_u_ratio = 0.167;
    	double onset = 0.0;
        spm_hrf(h, t_hrf, theta, t_r, dur, normalized_hrf, dt,
        		p_delay, undershoot, p_disp, u_disp, p_u_ratio, onset);

        return; // h, J
    }
    
    public void simple_hrf_estimation() {
    	int i;
    	// experimentation parameters
    	double dur = 3.5;
    	double hrf_dur = 20.0;
    	double TR = 0.75;
    	double snr = 5.0;
    	long random_state = 99;
    	// True HRF
    	double true_hrf_delta = 1.0;
    	double dt = 0.001;
    	int numsamples = (int)(hrf_dur/ dt);
    	int step = (int)(TR/dt);
    	int returnedsamples = ((numsamples-1)/step) + 1; // length of orig_hrf[] and t_hrf[]
    	double orig_hrf[] = new double[returnedsamples];
    	double t_hrf[] = new double[returnedsamples];
    	boolean normalized_hrf = false;
    	double p_delay = 6.0;
    	double undershoot = 16.0;
    	double p_disp = 1.0;
    	double u_disp = 1.0;
    	double p_u_ratio = 0.167;
    	double onset = 0.0;
    	spm_hrf(orig_hrf, t_hrf, true_hrf_delta, TR,
    				    hrf_dur, normalized_hrf, dt, p_delay,
    		            undershoot, p_disp, u_disp, p_u_ratio, onset);
    	boolean sourcehrf = true;
    	int nb_events = 2;
	    int avg_dur = 12;
	    double std_dur = 0.0;
	    boolean middle_spike = false;
	    boolean overlapping = false;
	    boolean unitary_block = false;
	    int nb_try = 1000;
	    int nb_try_duration = 1000;
	    boolean centered = false;
	    boolean haveSeed = true;
	    int genlength = 1 + (int)((60000*dur-1)/(int)(1000*TR));
	    double y_tilde[] = new double[genlength];
	    double y[] = new double[genlength];
	    double z[] = new double[genlength];
	    double i_s[] = new double[genlength];
	    double t[] = new double[genlength];
	    double noise[] = new double[genlength];
        gen_rnd_bloc_bold(y_tilde, y, z,
        		i_s,  t, noise,
        		dur, TR, sourcehrf, orig_hrf, nb_events, avg_dur,
                std_dur, middle_spike, overlapping,
                unitary_block, snr, nb_try,
                nb_try_duration, centered, haveSeed, random_state);
        // ###############################################################################
        // HRF estimation
        double est_hrf[] = new double[returnedsamples];
        Vector<Double>J = new Vector<Double>();
        int verbose = 3;
        long t0 = System.currentTimeMillis();
        hrf_estim(est_hrf, J, z, y_tilde,
        		  TR, hrf_dur, verbose);
        double delta_t = (System.currentTimeMillis() - t0)/1000.0;
        double runtimes[] = new double[J.size()];
        runtimes[0] = 0.0;
        runtimes[runtimes.length-1] = delta_t;
        double runStep = delta_t/(runtimes.length-1);
        for (i = 1; i < runtimes.length-1; i++) {
        	runtimes[i] = i*runStep;
        }
        System.out.println("hrf_estim duration = " + delta_t + " seconds");

        // Plot shows perfect match between orig_hrf and est_hrf
        float xInit[][] = new float[2][returnedsamples];
        float yInit[][] = new float[2][returnedsamples];
        for (i = 0; i < returnedsamples; i++) {
        	xInit[0][i] = (float)t_hrf[i];
        	xInit[1][i] = (float)t_hrf[i];
        	yInit[0][i] = (float)orig_hrf[i]; // Orig. HRF
        	yInit[1][i] = (float)est_hrf[i]; // Est. HRF
        }
        String title = "Est. SPM HRF";
        String labelX = "time (s)";
        String labelY = "ampl.";
        Color colorArray[] = new Color[] {Color.BLUE, Color.RED};
        new ViewJFrameGraph(xInit, yInit, title, labelX, labelY, colorArray);
        
        double inf_norm_y_tilde = 0.0;
        for (i = 0; i < genlength; i++) {
        	inf_norm_y_tilde = Math.max(Math.abs(y_tilde[i]), inf_norm_y_tilde);
        }
        float xInit2[][] = new float[2][genlength];
        float yInit2[][] = new float[2][genlength];
        for (i = 0; i < genlength; i++) {
        	xInit2[0][i] = (float)t[i];
        	xInit2[1][i] = (float)t[i];
        	yInit2[0][i] = (float)(y_tilde[i]/inf_norm_y_tilde); // Normalized noisy BOLD signal
        	yInit2[1][i] = (float)z[i]; // Block signal
        }
        String title2 = "Normalized noisy BOLD signal";
        String labelX2 = "time (s)";
        String labelY2 = "ampl.";
        Color colorArray2[] = new Color[] {Color.YELLOW, Color.BLACK};
        new ViewJFrameGraph(xInit2, yInit2, title2, labelX2, labelY2, colorArray2);
        
        float xInit3[] = new float[J.size()];
        float yInit3[] = new float[J.size()];
        for (i = 0; i < J.size(); i++) {
        	xInit3[i] = (float)runtimes[i];
        	yInit3[i] = (float)J.get(i).doubleValue();
        }
        String title3 = "Evolution of the cost function";
        String labelX3 = "time (s)";
        String labelY3 = "Cost function";
        new ViewJFrameGraph(xInit3, yInit3, title3, labelX3, labelY3);

    }
    
    
    public double[] _loops_deconv(double y[], double diff_z[], double H[][], 
    		double lbda, long nb_iter, boolean early_stopping, long wind, double tol) {
	     // Main loop for deconvolution.
    	int i,j,k;
    	// Create a lower triangle of ones
	    double L[][] = new double[y.length][y.length];
	    for (i = 0; i < y.length; i++) {
	    	for (j = 0; j <= i; j++) {
	    		L[i][j] = 1.0;
	    	}
	    }
	    double A[][] = new double[H.length][y.length];
	    for (i = 0; i < H.length; i++) {
	    	for (j = 0; j < y.length; j++) {
	    		for (k = 0; k < y.length; k++) {
	    			A[i][j] += H[i][k]*L[k][j];
	    		}
	    	}
	    }
	    double AT[][] = new double[y.length][H.length];
	    for (i = 0; i < y.length; i++) {
	    	for (j = 0; j < H.length; j++) {
	    		AT[i][j] = A[j][i];
	    	}
	    }
	    double A_t_A[][] = new double[y.length][y.length];
	    for (i = 0; i < y.length; i++) {
	    	for (j = 0; j < y.length; j++) {
	    		for (k = 0; k < H.length; k++) {
	    			A_t_A[i][j] += AT[i][k]*A[k][j];
	    		}
	    	}
	    }
	    double A_t_y[] = new double[y.length];
	    for (i = 0; i < y.length; i++) {
	    	for (j = 0; j < y.length; j++) {
	    		A_t_y[i] += AT[i][j]*y[j];
	    	}
	    }
	    double grad_lipschitz_cst = 0.0;
	    for (i = 0; i < y.length; i++) {
	    	for (j = 0; j < y.length; j++) {
	    		grad_lipschitz_cst += A_t_A[i][j]*A_t_A[i][j];
	    	}
	    }
	    grad_lipschitz_cst = Math.sqrt(grad_lipschitz_cst);
	    double step = 1.0 / grad_lipschitz_cst;
	    double th = lbda / grad_lipschitz_cst;
	    double diff_z_old[] = new double[y.length];
	    double t = 1.0;
	    double t_old = 1.0;
	    double A_t_A_z[] = new double[y.length];
	    double diff_z_diff[] = new double[y.length];
	
	    for (j = 0; j < nb_iter; j++) {
	        for (i = 0; i < y.length; i++) {
	        	for (k = 0; k < y.length; k++) {
	        		A_t_A_z[i] += A_t_A[i][k]*diff_z[k];
	        	}
	        }
	        for (i = 0; i < y.length; i++) {
	        	diff_z[i] -= step*(A_t_A_z[i] - A_t_y[i]);
	        }
	        
	        for (i = 0; i < y.length; i++) {
	            diff_z[i] = sign(diff_z[i]) * Math.max(Math.abs(diff_z[i]) - th, 0);
	        }
	
	        t = 0.5 * (1.0 + Math.sqrt(1 + 4*t_old*t_old));
	        for (i = 0; i < y.length; i++) {
	            diff_z[i] = diff_z[i] + (t_old-1)/t * (diff_z[i] - diff_z_old[i]);
	        }
	
	        if (early_stopping) {
	            if (j > 2) {
	            	for (i = 0; i < y.length; i++) {
	            		diff_z_diff[i] = diff_z[i] - diff_z_old[i];
	            	}
	                double crit_num = norm(diff_z_diff);
	                double crit_deno = norm(diff_z);
	                double diff = crit_num / (crit_deno + 1.0e-10);
	                if (diff < tol) {
	                    break;
	                }
	            } // if A(j > 2)
	        } // if (early_stopping)
	
	        t_old = t;
	        for (i = 0; i < y.length; i++) {
	            diff_z_old[i] = diff_z[i];
	        }
	    } // for (j = 0; j < nb_iter; j++)
	
	    return diff_z;
    }

    
    public void bd(Vector<Double> xout,  Vector<Double>zout, Vector<Double> diff_zout,
    		Vector<Double> hout, Vector<Double> dr, Vector<Double> dg, Vector<Double> dJ,
    		double y[], double t_r, double lbda, double theta, double z_0[], double hrf_dur,
    	       double bounds[], int nb_iter, int nb_sub_iter, int nb_last_iter,
    	       int print_period, boolean early_stopping, int wind, double tol, int verbose) {
    	    // BOLD blind deconvolution function based on a scaled HRF model and an
    	    // blocs BOLD model.
    	    // Defaults:
    	    // lbda 1.0
    	    // theta MAX_DELTA
    	    // double z_0[] default null if present same length as y
    	    // hrf_dur 20.0
    	    // double bounds[] default  [(MIN_DELTA + 1.0e-1, MAX_DELTA - 1.0e-1)] 
    	    // nb_iter 100
    	    // nb_sub_iter 1000
    	    // nb_last_iter 10000
    	    // print_period 50
    	    // early_stopping false
    	    // wind 4
    	    // tol 1.0E-12
    	    // verbose 0

    	    // initialization
    	    int i;
    	    int idx;
    	    double H[][];
    	    double r;
    	    double g;
    	    int sub_wind_len;
    	    double old_j;
    	    double new_j;
    	    double dt = 0.001;
        	int numsamples = (int)(hrf_dur/ dt);
        	int step = (int)(t_r/dt);
        	int returnedsamples = ((numsamples-1)/step) + 1; // length of orig_hrf[] and t_hrf[]
        	double h[] = new double[returnedsamples];
        	double t_hrf[] = new double[returnedsamples];
        	boolean normalized_hrf = false;
        	double p_delay = 6.0;
        	double undershoot = 16.0;
        	double p_disp = 1.0;
        	double u_disp = 1.0;
        	double p_u_ratio = 0.167;
        	double onset = 0.0;
        	spm_hrf(h, t_hrf, theta, t_r,
        				    hrf_dur, normalized_hrf, dt, p_delay,
        		            undershoot, p_disp, u_disp, p_u_ratio, onset);


            double diff_z[];
            double z[];
            double x[];
    	    if (z_0 == null) {
    	        diff_z = new double[y.length];
    	        z = new double[y.length];
    	        x = new double[y.length];
    	    }
    	    else {
    	    	diff_z = new double[z_0.length];
    	    	for (i = 1; i < diff_z.length; i++) {
    	    		diff_z[i] = z_0[i] - z_0[i-1];
    	    	}
    	        z = new double[z_0.length];
    	        for (i = 0; i < z.length; i++) {
    	        	z[i] = z_0[i];
    	        }
    	        x = spectral_convolve(h, z);
    	    }
    	    
    	    double r_0 = 0.0;
    	    double diff;
    	    for (i = 0; i < y.length; i++) {
    	    	diff = x[i] - y[i];
    	    	r_0 += diff*diff;
    	    }
    	    dr.add(1.0);
    	    double g_0 = 0.0;
    	    for (i = 0; i < diff_z.length; i++) {
    	    	g_0 += Math.abs(diff_z[i]);
    	    }
    	    dg.add(g_0);
    	    double j_0 = r_0 + lbda * g_0;
    	    dJ.add(1.0);

    	    if (verbose > 0) {
    	        System.out.println("Normalized global cost-function initialized to 1.0");
    	    }
    	    
    	    // main loop
    	    for (idx = 0; idx < nb_iter; idx++) {
    	        // deconvolution
    	        H = toeplitz_from_kernel(h, y.length, y.length);
    	        diff_z = _loops_deconv(y, diff_z, H, lbda, nb_iter, early_stopping,
                        wind, tol);
                z[0] = diff_z[0];
                for (i = 1; i < y.length; i++) {
                	z[i] = z[i-1] + diff_z[i];
                }
                
                // hrf estimation
                
                //      We specify the dimension n of the sample problem and the number
             	//        m of limited memory corrections stored.  (n and m should not
             	//        exceed the limits nmax and mmax respectively.)
             	 
             	int n=1;
             	int m=10;
                //      We suppress the default output.  (The user could also elect to 
             	//       use the default output by choosing iprint >= 0.)

             	int iprint = 100;
             	String task[] = new String[1];
        	    String csave[] = new String[1];
        	    boolean lsave[] = new boolean[4];
        	    int nbd[] = new int[n];
        	    int isave[] = new int[23];
        	    double f[] = new double[1];
        	    double factr = 0.0; 
        	    double pgtol = 1.0E-12; 
        	    
        	    // mainlb arguments
        	    double xarg[] = new double[1];
        	    double l[] = new double[n];
        	    double u[] = new double[n];
        	    double gbuf[] = new double[n]; 
        	    double dsave[] = new double[29];
        	    double ws[][] = new double[n][m];
        	    double wy[][] = new double[n][m];
        	    double sy[][] = new double[m][m];
        	    double ss[][] = new double[m][m];
        	    double wt[][] = new double[m][m];
        	    double wn[][] = new double[2*m][2*m];
        	    double snd[][] = new double[2*m][2*m];
        	    double zbuf[] = new double[n];
        	    double rbuf[] = new double[n];
        	    double d[] = new double[n];
        	    double t[] = new double[n];
        	    double xp[] = new double[n];
        	    double wa1[] = new double[2*m];
        	    double wa2[] = new double[2*m];
        	    double wa3[] = new double[2*m];
        	    double wa4[] = new double[2*m];
        	    int index[] = new int[n];
        	    int iwhere[] = new int[n];
        	    int indx2[] = new int[n];
        	    
                //	     We now specify nbd which defines the bounds on the variables:
        	    //                    l   specifies the lower bounds,
        	    //                    u   specifies the upper bounds.
        	    nbd[0] = 2; // both a lower and upper bound
        	    l[0] = bounds[0];
        	    u[0] = bounds[1];
                //	     We now define the starting point.
        	    xarg[0] = theta;
        	    System.out.println("Initial xarg[0] = " + xarg[0]);
        	    
                //	     We start the iteration by initializing task.
        	     
                task[0] = "START";
                double lower_dist;
                double upper_dist;
                double originalStep = 1.0E-8;
                boolean central;
                boolean forward;
                boolean backward;
                double adjustedForwardStep;
                double adjustedBackwardStep;
                double min_dist;
                boolean adjusted_central;
                RandomAccessFile raFile = null;
                String fileDir = "C:/L-BFGS-B/";
        	           if (iprint >= 1) {
        	       		//                                open a summary file 'iterate.dat'
        	            File file = new File(fileDir + "iterate.data");
        	            try {
        	                raFile = new RandomAccessFile(file, "rw");
        	            }
        	            catch (FileNotFoundException e) {
        	            	System.err.println("At raFile = new Random AccessFile(file, rw) FileNotException " + e);
        	            	System.exit(-1);
        	            }
        	            catch (SecurityException e2) {
        	            	System.err.println("At raFile = new Random AccessFile(file, rw) SecurityException " + e2);
        	            	System.exit(-1);	
        	            }
        	            // Necessary so that if this is an overwritten file there isn't any
        	            // junk at the end
        	            try {
        	                raFile.setLength(0);
        	            }
        	            catch (IOException e) {
        	            	System.err.println("At raFile.setLength(0) IOException " + e);
        	            	System.exit(-1);	
        	            }
        	         } // if (iprint >= 1)   
                L_BFGS_B LBB = new L_BFGS_B();
                
                /*
                Verifies that arg[0] = 1.9 is the correct answer for minimiaing f
                for (double del = l[0]; del <= u[0]+0.01; del+= 0.05) {
                	f[0] = hrf_fit_err(del, z, y, t_r, hrf_dur);	
                	System.out.println("del = " + del + " f[0] = " + f[0]);
                	del = 0.6 f[0] = 840.0567047083654
        			del = 0.65 f[0] = 840.0271655137155
        			del = 0.7000000000000001 f[0] = 839.9890383664151
        			del = 0.7500000000000001 f[0] = 839.9409176227871
        			del = 0.8000000000000002 f[0] = 839.8813597735948
        			del = 0.8500000000000002 f[0] = 839.8089060698674
        			del = 0.9000000000000002 f[0] = 839.7221061260544
        			del = 0.9500000000000003 f[0] = 839.6195422026619
        			del = 1.0000000000000002 f[0] = 839.4998538695758
        			del = 1.0500000000000003 f[0] = 839.361762748399
        			del = 1.1000000000000003 f[0] = 839.2040970291404
        			del = 1.1500000000000004 f[0] = 839.0258154554954
        			del = 1.2000000000000004 f[0] = 838.8260304749393
        			del = 1.2500000000000004 f[0] = 838.6040302558562
        			del = 1.3000000000000005 f[0] = 838.3592992845844
        			del = 1.3500000000000005 f[0] = 838.0915372706678
        			del = 1.4000000000000006 f[0] = 837.8006761088217
        			del = 1.4500000000000006 f[0] = 837.4868946707275
        			del = 1.5000000000000007 f[0] = 837.1506312284679
        			del = 1.5500000000000007 f[0] = 836.7925933432631
        			del = 1.6000000000000008 f[0] = 836.4137650878166
        			del = 1.6500000000000008 f[0] = 836.0154115069281
        			del = 1.7000000000000008 f[0] = 835.5990802584737
        			del = 1.7500000000000009 f[0] = 835.166600414537
        			del = 1.800000000000001 f[0] = 834.7200784396598
        			del = 1.850000000000001 f[0] = 834.2618913991536
        			del = 1.900000000000001 f[0] = 833.7946774845946
                }
                */
                
                //      ------- the beginning of the loop ----------
        		 
        		 do {
        		      
        		//     This is the call to the L-BFGS-B code.
        	         LBB.mainlb(n,m,xarg,l,u,nbd,f,gbuf,factr,pgtol,
        		             ws,wy,sy,ss,wt,
        		             wn,snd,zbuf,rbuf,d,t,xp,wa1,wa2,wa3,wa4,
        		             index, iwhere,indx2,task,iprint, 
        		             csave,lsave,isave,dsave,raFile);
        	         System.out.println("xarg[0] = " + xarg[0]);
        	         
        	         if ((task[0].length() >= 2) && (task[0].substring(0,2).equalsIgnoreCase("FG"))) {
        	     		//        the minimization routine has returned to request the
        	     		//        function f and gradient g values at the current x.

        	     		//        Compute function value f for the sample problem.

        	     	        	  f[0] = hrf_fit_err(xarg[0], z, y, t_r, hrf_dur);
                        //	     	           Compute gradient g for the sample problem
        	     	        	  
        	     	        	  lower_dist = xarg[0] - l[0];
        	     	              upper_dist = u[0] - xarg[0];
        	     	              central = (lower_dist >= originalStep) && (upper_dist >= originalStep);
        	     	              if (central) {
        	     	            	  gbuf[0] = (hrf_fit_err(xarg[0]+originalStep, z, y, t_r, hrf_dur) 
        	     	            			  - hrf_fit_err(xarg[0]-originalStep, z, y, t_r, hrf_dur))/(2.0*originalStep);
        	     	            	  System.out.println("gbuf[0] central = " + gbuf[0]);
        	     	            	  continue;
        	     	              }
        	     	              forward = upper_dist >= lower_dist;
        	     	              adjustedForwardStep = Math.min(originalStep, 0.5 *upper_dist);
        	     	              if (forward) {
        	     	                  gbuf[0] = (hrf_fit_err(xarg[0] + adjustedForwardStep, z, y, t_r, hrf_dur) - f[0])/adjustedForwardStep;
        	     	                  System.out.println("gbuf[0] forward = " + gbuf[0]);
        	     	                  continue;
        	     	              }
        	     	              backward = upper_dist < lower_dist;
        	     	              adjustedBackwardStep = Math.min(originalStep, 0.5*lower_dist);
                                  if (backward) {
                                      gbuf[0] = (f[0] - hrf_fit_err(xarg[0] - adjustedBackwardStep, z, y, t_r, hrf_dur))/adjustedBackwardStep;
                                      System.out.println("gbuf[0] backward = " + gbuf[0]);
        	     	                  continue;
                                  }
                                  min_dist = Math.min(upper_dist, lower_dist);
                                  adjusted_central = (Math.abs(adjustedForwardStep) <= min_dist) && (Math.abs(adjustedBackwardStep) <= min_dist);
        	     		          if (adjusted_central) {
        	     		        	 gbuf[0] = (hrf_fit_err(xarg[0]+min_dist, z, y, t_r, hrf_dur) 
        	     		        			 - hrf_fit_err(xarg[0]-min_dist, z, y, t_r, hrf_dur))/(2.0*min_dist);
        	     		        	 System.out.println("gbuf[0] adjusted = " + gbuf[0]);
        	     	            	 continue;
        	     		          }

        	     	              System.err.println("Could not calculate gbuf[0]");
        	     	              System.exit(-1);
        	     	         } // if ((task[0].length() >= 2) && (task[0].substring(0,2).equalsIgnoreCase("FG")))
        	     		
        	     	         if ((task[0].length() >= 5) && (task[0].substring(0,5).equalsIgnoreCase("NEW_X"))) {   
        	     		     
        	     		//        the minimization routine has returned with a new iterate.
        	     		//        At this point have the opportunity of stopping the iteration 
        	     		//        or observing the values of certain parameters
        	     		

        	     		//        Note: task(1:4) must be assigned the value 'STOP' to terminate  
        	     		//          the iteration and ensure that the final results are
        	     		//          printed in the default format. The rest of the character
        	     		//          string TASK may be used to store other information.

        	     		//        1) Terminate if the total number of f and g evaluations
        	     		//             exceeds 999.

        	     		         if (isave[12] >= 999) {
        	     		             task[0] = "STOP: TOTAL NO. of f AND gbuf EVALUATIONS EXCEEDS LIMIT";
        	     		         }


        	     		//        We now wish to print the following information at each
        	     		//        iteration:
        	     		        
        	     		//          1) the current iteration number, isave(30),
        	     		//          2) the total number of f and g evaluations, isave(34),
        	     		//          3) the value of the objective function f,
        	     		//          4) the norm of the projected gradient,  dsve(13)
        	     		
        	     		//        See the comments at the end of driver1 for a description
        	     		//        of the variables isave and dsave.
        	     		         
        	     		         //System.out.println("Current iteration number = " + isave[8]);
        	     		         //System.out.println("Total number of f and g evaluations = " + isave[12]);
        	     		         //System.out.println("Value of the objective function f = " + f[0]);
        	     		         //System.out.println("Norm of the projected gradient = " + dsave[12]);

        	     		//        If the run is to be terminated, we print also the information
        	     		//        contained in task as well as the final value of x.

        	     		         if (task[0].substring(0,4).equals("STOP")) {
        	     		            System.out.println("task[0] = " + task[0]);
        	     		            System.out.print("Final xarg = " + xarg[0]);
        	     		         } // if (task[0].substring(0,4).equals("STOP"))

        	     		//          go back to the minimization routine.
        	     		         continue;

        	     	         } // if ((task[0].length() >= 5) && (task[0].substring(0,5).equalsIgnoreCase("NEW_X")))

        	     		//           ---------- the end of the loop -------------
        	     		 
        	     		//     If task is neither FG nor NEW_X we terminate execution.

        	     		      break;
        	     		 } while (true);
        		 theta = xarg[0];
                
                //args = (z, y, t_r, hrf_dur)
                        // theta, _, _ = fmin_l_bfgs_b(
                                            // func=hrf_fit_err, x0=theta, args=args,
                                           // bounds=bounds, approx_grad=True, maxiter=999,
                                           // pgtol=1.0e-12)
        		 spm_hrf(h, t_hrf, theta, t_r,
     				    hrf_dur, normalized_hrf, dt, p_delay,
     		            undershoot, p_disp, u_disp, p_u_ratio, onset);
        		 x = spectral_convolve(h, z);
        		 
        		 // cost function
        		 r = 0.0;
        		 for (i = 0; i < y.length; i++) {
        			 diff = x[i] - y[i];
        			 r += diff*diff;
        		 }
                 g = 0.0;
                 for (i = 0; i < y.length; i++) {
                	 g += Math.abs(diff_z[i]);
                 }
        	     dJ.add((r + lbda * g) / j_0 + 1.0e-30);
        	     dr.add(r / r_0 + 1.0e-30);
        	     dg.add(g);

        	     if ((verbose > 0) && ((idx+1) % print_period == 0)) {
        	         System.out.println("Normalized global cost-function at idx = " + idx + " is " + dJ.get(dJ.size()-1));
        	     }

        	     // early stopping
        	     if (early_stopping) {
    	            if (idx > wind) {
    	                sub_wind_len = (int)(wind/2);
    	                old_j = 0.0;
    	                for (i = 0; i < dJ.size() - sub_wind_len; i++) {
    	                	old_j += dJ.get(i);
    	                }
    	                old_j = old_j/(dJ.size() - sub_wind_len);
    	                new_j = 0.0;
    	                for (i = dJ.size() - sub_wind_len; i < dJ.size(); i++) {
    	                	new_j += dJ.get(i);
    	                }
    	                new_j = new_j/sub_wind_len;
    	                diff = (new_j - old_j) / new_j;
    	                if (diff < tol) {
    	                    if (verbose > 0) {
    	                    	System.out.println("Early stopping done at idx = " + idx + " with normalized cost function = " + dJ.get(idx));
    	                    } // if (verbose > 0)
    	                    break;
    	                } // if (diff < tol)
    	            } // if (idx > wind)
        	     } // if (early_stopping)


    	    } // for (idx = 0; idx < nb_iter; idx++)
    	    
    	    // last (long) deconvolution
    	    H = toeplitz_from_kernel(h, y.length, y.length);
    	    diff_z = _loops_deconv(y, diff_z, H, lbda, nb_iter, early_stopping, wind,
    	                           tol);
    	    z[0] = diff_z[0];
    	    for (i = 1; i < y.length; i++) {
    	    	z[i] = z[i-1] + diff_z[i];
    	    }
    	    x = spectral_convolve(h, z);

    	    // cost function
    	    r = 0.0;
	   		for (i = 0; i < y.length; i++) {
	   			diff = x[i] - y[i];
	   			r += diff*diff;
	   		}
            g = 0.0;
            for (i = 0; i < y.length; i++) {
           	     g += Math.abs(diff_z[i]);
            }
    	    dJ.add((r + lbda * g) / j_0);
    	    dr.add(r / r_0);
    	    dg.add(g);

    	    for (i = 0; i < x.length; i++) {
    	    	xout.add(x[i]);
    	    }
    	    for (i = 0; i < z.length; i++) {
    	    	zout.add(z[i]);
    	    }
    	    for (i = 0; i < diff_z.length; i++) {
    	    	diff_zout.add(diff_z[i]);
    	    }
    	    for (i = 0; i < h.length; i++) {
    	    	hout.add(h[i]);
    	    }

    	    return;
    }

    public void blind_deconvolution_example() {
    	// generate data
    	double hrf_dur = 20.0;
    	int dur = 3;  // minutes
    	double TR = 0.75;
    	double snr = 1.0;
 
    	double dt = 0.001;
    	int numsamples = (int)(hrf_dur/ dt);
    	int step = (int)(TR/dt);
    	// returnedsamples = 27
    	int returnedsamples = ((numsamples-1)/step) + 1; // length of orig_hrf[] and t_hrf[]
    	double orig_hrf[] = new double[returnedsamples];
    	// t_hrf are .037501875 apart
    	double t_hrf[] = new double[returnedsamples];
    	boolean normalized_hrf = true;
    	double p_delay = 6.0;
    	double undershoot = 16.0;
    	double p_disp = 1.0;
    	double u_disp = 1.0;
    	double p_u_ratio = 0.167;
    	double onset = 0.0;
    	spm_hrf(orig_hrf, t_hrf, 1.0, TR,
    				    hrf_dur, normalized_hrf, dt, p_delay,
    		            undershoot, p_disp, u_disp, p_u_ratio, onset);
    	double dur_bloc = 30.0;
    	boolean sourcehrf = true;
    	boolean haveSeed = true;
    	long random_state = 0L;
    	// For noisy_ar_s, ar_s, ai_s, i_s, t, noise length is int N = (int)(dur * 60 / TR);
    	// N = 240
    	int N = (int)(dur * 60 / TR);
    	System.out.println("N = " + N);
    	double noisy_ar_s[] = new double[N];
    	double ar_s[] = new double[N];
    	double ai_s[] = new double[N];
    	double i_s[] = new double[N];
    	// t are spaced at dur*60/(N-1) = .753138
    	double t[] = new double[N];
    	double noise[] = new double[N];
    	gen_regular_bloc_bold(noisy_ar_s, ar_s, ai_s,
    		    		i_s, t, noise, dur, TR, dur_bloc,
    		    		sourcehrf, orig_hrf, snr, haveSeed, random_state);
    	
    	
    	// ###############################################################################
    	// blind deconvolution
    	int nb_iter = 1;
    	double init_hrf[] = new double[returnedsamples];
    	spm_hrf(init_hrf, t_hrf, MAX_DELTA, TR,
			    hrf_dur, normalized_hrf, dt, p_delay,
	            undershoot, p_disp, u_disp, p_u_ratio, onset);

    	Vector<Double> est_ar_s = new Vector<Double>();
    	Vector<Double> est_ai_s = new Vector<Double>();
    	Vector<Double> est_i_s = new Vector<Double>();
    	Vector<Double> est_hrf = new Vector<Double>();
    	Vector<Double> dr = new Vector<Double>();
    	Vector<Double> dg = new Vector<Double>();
    	Vector<Double> dJ = new Vector<Double>();
    	double lbda = 1.0;
    	double theta = MAX_DELTA;
    	double z_0[] = null;
    	double bounds[] = new double[] {MIN_DELTA + 1.0e-1, MAX_DELTA - 1.0e-1};
    	int nb_sub_iter = 1000;
    	int nb_last_iter = 10000;
    	int print_period = 50;
    	boolean early_stopping = false;
    	int wind = 4;
    	double tol = 1.0E-2;
    	int verbose = 1;
    	
    	long t0 = System.currentTimeMillis();
    	bd(est_ar_s, est_ai_s, est_i_s, est_hrf, dr, dg, dJ, 
           noisy_ar_s, TR, lbda, theta, z_0, hrf_dur,
           bounds, nb_iter, nb_sub_iter, nb_last_iter,
           print_period, early_stopping, wind, tol, verbose);
        	    
    	double delta_t = (System.currentTimeMillis() - t0)/1000.0;

    	System.out.println("bd duration = " + delta_t + " seconds");
    	System.out.println("returnedsamples = " + returnedsamples);
    	System.out.println("est_hrf.size() = " + est_hrf.size());
    	int i;
    	
    	double inf_norm_noisy_ar_s = inf_norm(noisy_ar_s);
		for (i = 0; i < noisy_ar_s.length; i++) {
			noisy_ar_s[i] /= inf_norm_noisy_ar_s;
		}
		
		double inf_norm_ar_s = inf_norm(ar_s);
		for (i = 0; i < ar_s.length; i++) {
            ar_s[i] /= inf_norm_ar_s;
		}
		
		double inf_norm_ai_s = inf_norm(ai_s);
		for (i = 0; i < ai_s.length; i++) {
			ai_s[i] /= inf_norm_ai_s;
		}
		
		double inf_norm_i_s = inf_norm(i_s);
		for (i = 0; i < i_s.length; i++) {
			i_s[i] /= inf_norm_i_s;
		}

		double inf_norm_est_ar_s = inf_norm(est_ar_s);
		for (i = 0; i < est_ar_s.size(); i++) {
			est_ar_s.set(i, est_ar_s.get(i)/inf_norm_est_ar_s);
		}
		
		double inf_norm_est_ai_s = inf_norm(est_ai_s);
		for (i = 0; i < est_ai_s.size(); i++) {
			est_ai_s.set(i, est_ai_s.get(i)/inf_norm_est_ai_s);
		}
		
		double inf_norm_est_i_s = inf_norm(est_i_s);
		for (i = 0; i < est_i_s.size(); i++) {
			est_i_s.set(i, est_i_s.get(i)/inf_norm_est_i_s);
		}
		
		double inf_norm_est_hrf = inf_norm(est_hrf);
		for (i = 0; i < est_hrf.size(); i++) {
			est_hrf.set(i, est_hrf.get(i)/inf_norm_est_hrf);
		}
		
		double inf_norm_orig_hrf = inf_norm(orig_hrf);
		for (i = 0; i < orig_hrf.length; i++) {
			orig_hrf[i] /= inf_norm_orig_hrf;
		}
		
		float xInit[][] = new float[2][N];
        float yInit[][] = new float[2][N];
        for (i = 0; i < N; i++) {
        	xInit[0][i] = (float)t[i];
        	xInit[1][i] = (float)t[i];
        	yInit[0][i] = (float)noisy_ar_s[i]; // "Noisy activation related signal, snr = " + snr + " dB";
        	yInit[1][i] = (float)est_ar_s.get(i).doubleValue(); // ="Est. activation related signal"
        }
        String title = "Noisy activation related signal, snr = " + snr + " dB";
        String labelX = "time (s)";
        String labelY = "ampl.";
        Color colorArray[] = new Color[] {Color.YELLOW, Color.GREEN};
        new ViewJFrameGraph(xInit, yInit, title, labelX, labelY, colorArray);
        
        float xInit2[][] = new float[2][N];
        float yInit2[][] = new float[2][N];
        for (i = 0; i < N; i++) {
        	xInit2[0][i] = (float)t[i];
        	xInit2[1][i] = (float)t[i];
        	yInit2[0][i] = (float)ar_s[i]; // "Orig. activation related signal"
        	yInit2[1][i] = (float)est_ar_s.get(i).doubleValue(); // ="Est. activation related signal"
        }
        String title2 = "Orig. activation related signal";
        String labelX2 = "time (s)";
        String labelY2 = "ampl.";
        Color colorArray2[] = new Color[] {Color.BLUE, Color.GREEN};
        new ViewJFrameGraph(xInit2, yInit2, title2, labelX2, labelY2, colorArray2);
        
        float xInit3[][] = new float[3][N];
        float yInit3[][] = new float[3][N];
        for (i = 0; i < N; i++) {
        	xInit3[0][i] = (float)t[i];
        	xInit3[1][i] = (float)t[i];
        	xInit3[2][i] = (float)t[i];
        	yInit3[0][i] = (float)ar_s[i]; // "Orig. activation inducing signal, snr = " + snr + " dB"
        	yInit3[1][i] = (float)est_ai_s.get(i).doubleValue(); // "Est. activation inducing signal"
        	yInit3[2][i] = (float)est_i_s.get(i).doubleValue(); //  "Est. innovation signal")
        }
        String title3 = "Orig. activation inducing signal, snr = " + snr + " dB";
        String labelX3 = "time (s)";
        String labelY3 = "ampl.";
        Color colorArray3[] = new Color[] {Color.BLUE, Color.GREEN, Color.RED};
        new ViewJFrameGraph(xInit3, yInit3, title3, labelX3, labelY3, colorArray3);
        
        float xInit4[][] = new float[3][returnedsamples];
        float yInit4[][] = new float[3][returnedsamples];
        for (i = 0; i < returnedsamples; i++) {
        	xInit4[0][i] = (float)t_hrf[i];
        	xInit4[1][i] = (float)t_hrf[i];
        	xInit4[2][i] = (float)t_hrf[i];
        	yInit4[0][i] = (float)orig_hrf[i]; // "Orig. HRF"
        	yInit4[1][i] = (float)est_hrf.get(i).doubleValue(); // "Est. HRF"
        	yInit4[2][i] = (float)init_hrf[i]; //  "Init. HRF"
        }
        String title4 = "Orig. HRF";
        String labelX4 = "time (s)";
        String labelY4 = "ampl.";
        Color colorArray4[] = new Color[] {Color.BLUE, Color.GREEN, Color.BLACK};
        new ViewJFrameGraph(xInit4, yInit4, title4, labelX4, labelY4, colorArray4);
        
        float xInit5[] = new float[dJ.size()];
        float yInit5[] = new float[dJ.size()];
        for (i = 0; i < dJ.size(); i++) {
        	xInit5[i] = (float)i;
        	yInit5[i] = (float)dJ.get(i).doubleValue();
        }
        String title5 = "Evolution of the cost function";
        String labelX5 = "n iters";
        String labelY5 = "Cost function";
        new ViewJFrameGraph(xInit5, yInit5, title5, labelX5, labelY5);
		
    }
    
    private double inf_norm(double x[]) {
    	int i;
    	double ans = 0.0;
    	for (i = 0; i < x.length; i++) {
    	    ans = Math.max(ans, Math.abs(x[i]));	
    	}
    	return ans;
    }
    
    private double inf_norm(Vector<Double>x) {
    	int i;
    	double ans = 0.0;
    	for (i = 0; i < x.size(); i++) {
    	    ans = Math.max(ans, Math.abs(x.get(i)));	
    	}
    	return ans;
    }


}
