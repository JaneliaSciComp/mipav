package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.algorithms.filters.PyWavelets;
import gov.nih.mipav.model.algorithms.filters.PyWavelets.DiscreteWavelet;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.awt.Graphics;
import java.io.*;
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
		int p_right;
		double paddedArray[] = new double[arrays.length + p];
		int i;
		
		if (paddtype.equalsIgnoreCase("center")) {
            p_left = (int)(p / 2);
            p_right = (int)(p / 2) + (p % 2);
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
    	fft.run();
    	fft = new FFTUtility(output[0],output[1],1,outlen-1,1,-1,FFTUtility.REALS);
        fft.run();
        return output;
    }
    
    public double[] irfft(double x[][], boolean isOdd) {
    	int i, j = 0;
    	int n = x[0].length-1;
    	FFTUtility fft = new FFTUtility(x[0],x[1],1,n,1,1,FFTUtility.REALS);
        fft.run();	
        fft = new FFTUtility(x[0],x[1],1,n,1,1,FFTUtility.FFT);
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
	    
    public double[] spectral_convolve(double k[], double x[]) {
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
        int p[] = new int[2];
        x = _custom_padd(p, x, 1024, 50, 0.5);
        int N = x.length;
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
    
    public double[] spectral_retro_convolve(double k[], double x[]) {
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
        int p[] = new int[2];
        x = _custom_padd(p, x, 1024, 50, 0.5);
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
        for (i = dim_out-1; i < dim_out - 1 + k.length; i++) {
        	padded_k[i] = k[k.length - 1 - (i - (dim_out - 1))];
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
        double w[] = null;
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
    
    /*
    def spectral_radius_est(L, x_shape, nb_iter=30, tol=1.0e-6, verbose=False):
        """ EStimation of the spectral radius of the operator L.
        """
        x_old = np.random.randn(*x_shape)

        stopped = False
        for i in range(nb_iter):
            x_new = L.adj(L.op(x_old)) / norm_2(x_old)
            if(np.abs(norm_2(x_new) - norm_2(x_old)) < tol):
                stopped = True
                break
            x_old = x_new
        if not stopped and verbose:
            print("Spectral radius estimation did not converge")

        return norm_2(x_new)
        */
   
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

}
