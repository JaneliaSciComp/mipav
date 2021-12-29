package gov.nih.mipav.model.algorithms.filters;


import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;
import java.util.Date;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.RandomNumberGen;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

/**
 * 
 * @author ilb
 * This is a port of code in the file czt.py by John Garrett.
 * MIT License

Copyright (c) 2020 John Garrett

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

The chirp Z-transform (CZT) is a generalization of the discrete Fourier transform (DFT). While the
DFT samples the Z plane at uniformly-spaced points along the unit circle, the chirp Z-transform samples
along spiral arcs in the Z-plane, corresponding to straight lines in the S plane. The DFT, real DFT, 
and zoom DFT can be calculated as special cases of the CZT.

References
----------

- [L. Rabiner, R. Schafer and C. Rader, "The chirp z-transform algorithm, 
"*IEEE Transactions on Audio and Electroacoustics*, vol. 17, no. 2, pp. 86-92, Jun. 1969, 
*doi: 10.1109/TAU.1969.1162034.](https://web.ece.ucsb.edu/Faculty/Rabiner/ece259/Reprints/015_czt.pdf)

- [V. Sukhoy and A. Stoytchev, "Generalizing the inverse FFT off the unit circle," *Scientific Reports*,
 vol. 9, no. 14443, Oct. 2019, doi: 10.1038/s41598-019-50234-9.](https://doi.org/10.1038/s41598-019-50234-9)

- [Chirp Z-Transform (Wikipedia)](https://en.wikipedia.org/wiki/Chirp_Z-transform)

- [Discrete Fourier Transform (Wikipedia)](https://en.wikipedia.org/wiki/Discrete_Fourier_transform)

*/

public class ChirpZTransform extends AlgorithmBase {
	
	private ViewUserInterface UI;
	
	public ChirpZTransform() {
		UI = ViewUserInterface.getReference();	
	}
	
	// complex input array
	private double xX[][];
	
	private double x[][];
	private double X[][];
	
	// Length of output array
	// For forward M = MN
	// For inverse N = MN
	// Default = x[0].length
	private int MN;
	
	private int M;
	
	private int N;
	
	// Complex ratio between points
	// Default = exp(-2i * Math.pi / M) W[0] = Math.cos(2.0*Math.PI/M) W[1] = -Math.sin(2.0 * Math.PI/M)
	private double W[];
	
	// Complex starting point  Default = 1.0.
	private double A[];
	
	// For forward: Use simple algorithm? (very slow) Default = false
	// For inverse: calculate ICZT using simple method (using CZT and
    // conjugate)Default = true
	private boolean simple;
	
	// Toeplitz matrix multiplication method. "ce" for
    // circulant embedding, "pd" for Pustylnikov's decomposition, "mm"
    // for simple matrix multiplication, "scipy" for matmul_toeplitz
    // from scipy.linalg.  In inverse ignored if you are not using the simple ICZT
    // method.
	// For forward default = "ce";
	// For inverse default = "scipy"
	private String t_method;
	
	// FFT method. 'FFTUtility" for FFT from FFTUtility,
    // "recursive" for recursive method.
	// Default = "FFTUtility"
	
	private String f_method;
	
	// double[2][MN] with Chirp Z-transform
	private double output[][];
	
	// True for forward transform, false for inverse transform
	private boolean forward;
	public ChirpZTransform(double output[][], double xX[][], int MN, double W[], double A[],
			boolean simple,  String t_method, String f_method, boolean forward) {
		this.output = output;
	    this.xX	= xX;
	    this.MN = MN;
	    this.W = W;
	    this.A = A;
	    this.simple = simple;
	    this.t_method = t_method;
	    this.f_method = f_method;
	    this.forward = forward;
	}
	
	/**
     * Starts the program.
     */
	
    public void runAlgorithm() {
    	int i, n;
        if (forward || simple) {
        	// Calculate the Chirp Z-transform (CZT).
            // Solves in O(n log n) time.
            // See algorithm 1 in Sukhoy & Stoytchev 2019.
        	x = new double[2][xX[0].length];
        	if (!forward) {
        		for (i = 0; i < xX[0].length; i++) {
        			x[0][i] = xX[0][i];
        			x[1][i] = -xX[1][i];
        		}
        	}
        	else {
        		for (i = 0; i < xX[0].length; i++) {
        			x[0][i] = xX[0][i];
        			x[1][i] = xX[1][i];
        		}
        	}
        	M = MN;
        	N = x[0].length;
        	// Simple algorithm very slow
        	if (forward && simple) {
        		double z[] = new double[2];
        		double br[] = new double[1];
        		double bi[] = new double[1];
        		int ierr[] = new int[1];
        	    for (i = 0; i < M; i++) {
        	        zpow(W[0], W[1], -i, br, bi, ierr);	
        	        z[0] = A[0]*br[0] - A[1]*bi[0];
        	        z[1] = A[0]*bi[0] + A[1]*br[0];
        	        output[0][i] = 0.0;
        	        output[1][i] = 0.0;
        	        for (n = 0; n < N; n++) {
        	             zpow(z[0],z[1], -n, br, bi, ierr); 
        	             output[0][i] += x[0][n]*br[0] - x[1][n]*bi[0];
        	             output[1][i] += x[0][n]*bi[0] + x[1][n]*br[0];
        	        }
        	    }
        	} // if (simple)
        	else {
        		// Algorithm 1 from Sukhoy & Stoytchev 2019	
        		int MNmax = Math.max(M,N);
        	} // else not simple
        	if (!forward) {
        		for (i = 0; i < M; i++) {
        			output[1][i] = -output[1][i];
        		}
        	}
        } // if (forward || simple))
        else {
            // Calculate inverse Chirp Z-transform (ICZT).
            // Solves in O(n log n) time.
            // See algorithm 2 in Sukhoy & Stoytchev 2019.
        	X = new double[2][xX[0].length];
        	for (i = 0; i < xX[0].length; i++) {
    			X[0][i] = xX[0][i];
    			X[1][i] = xX[1][i];
    		}
        	N = MN;
        	M = X[0].length;
        }
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
     * b = z**a = exp(a*log(z))
     * 
     * @param zr
     * @param zi
     * @param a
     * @param br
     * @param bi
     * @param ierr
     */
    private void zpow(final double zr, final double zi, final double a, final double br[], final double bi[],
            final int ierr[]) {
        zlog(zr, zi, br, bi, ierr);
        if (ierr[0] == 1) {
            MipavUtil.displayError("PseudoPolarFourierTransform has error in zlog in zpow");
            return;
        }
        br[0] = a * br[0];
        bi[0] = a * bi[0];
        zexp(br[0], bi[0], br, bi);
        return;
    }
    
    /**
     * complex logarithm b = clog(a).
     * 
     * @param ar double
     * @param ai double
     * @param br double[]
     * @param bi double[]
     * @param ierr int[] ierr = 0, normal return ierr = 1, z = cmplx(0.0, 0.0)
     */
    private void zlog(final double ar, final double ai, final double[] br, final double[] bi, final int[] ierr) {
        double theta;
        double zm;
        ierr[0] = 0;

        if (ar == 0.0) {

            if (ai == 0.0) {
                ierr[0] = 1;

                return;
            } // if (ai == 0.0)
            else {

                if (ai > 0.0) {
                    bi[0] = Math.PI / 2.0;
                } else {
                    bi[0] = -Math.PI / 2.0;
                }

                br[0] = Math.log(Math.abs(ai));

                return;
            }
        } // if (ar == 0.0)
        else if (ai == 0.0) {

            if (ar > 0.0) {
                br[0] = Math.log(ar);
                bi[0] = 0.0;

                return;
            } else {
                br[0] = Math.log(Math.abs(ar));
                bi[0] = Math.PI;

                return;
            }
        } // else if (ai == 0.0)

        theta = Math.atan(ai / ar);

        if ( (theta <= 0.0) && (ar < 0.0)) {
            theta = theta + Math.PI;
        } else if (ar < 0.0) {
            theta = theta - Math.PI;
        }

        zm = zabs(ar, ai);
        br[0] = Math.log(zm);
        bi[0] = theta;

        return;
    }
    
    /**
     * complex exponential function b = exp(a).
     * 
     * @param ar double
     * @param ai double
     * @param br double[]
     * @param bi double[]
     */
    private void zexp(final double ar, final double ai, final double[] br, final double[] bi) {
        double zm, ca, cb;
        zm = Math.exp(ar);
        ca = zm * Math.cos(ai);
        cb = zm * Math.sin(ai);
        br[0] = ca;
        bi[0] = cb;

        return;
    }
}