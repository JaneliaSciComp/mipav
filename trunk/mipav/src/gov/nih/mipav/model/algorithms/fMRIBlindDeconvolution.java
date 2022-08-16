package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;

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

	    // paddtype : "left", "right", "center"
	    //    where to place the padding.

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

		    // paddtype : "left", "right", "center"
		    //    where to place the padding.

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

		double unpaddedArray[] = new double[arrays.length-p[0]-p[1]];
		for (i = p[0]; i < p[0] + arrays.length; i++) {
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
			unpaddedArray[i] = new double[arrays[i].length-p[0]-p[1]];
			for (j = p[0]; j < p[0] + arrays[i].length; j++) {
				unpaddedArray[i][j-p[0]] = arrays[i][j];
			}
		}
		return unpaddedArray;
	}
}
