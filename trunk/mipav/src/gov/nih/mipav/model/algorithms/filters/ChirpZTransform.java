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
    	int i, j, n;
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
        		double r[][] = new double[2][N];
        		double X[][] = new double[2][N];
        		double Xout[][] = new double[2][N];
        		double c[][] = new double[2][M];
        		double br[] = new double[1];
        		double bi[] = new double[1];
        		double cr[] = new double[1];
        		double ci[] = new double[1];
        		int ierr[] = new int[1];
        		for (i = 0; i < MNmax; i++) {
        		    zpow(W[0], W[1], -(i*i)/2.0, br, bi, ierr);	
        		    if (i < M) {
        		    	c[0][i] = br[0];
        		    	c[1][i] = bi[0];
        		    }
        		    if (i < N) {
        		    	r[0][i] = br[0];
        		    	r[1][i] = bi[0];
        		    	zpow(A[0], A[1], -i, br, bi, ierr);
        		    	zmlt(br[0], bi[0], x[0][i], x[1][i], cr, ci);
        		    	zdiv(cr[0], ci[0], r[0][i], r[1][i], br, bi);
        		    	X[0][i] = br[0];
        		    	X[1][i] = bi[0];
        		    }
        		} // for (i = 0; i < MNmax; i++)
        	    if (t_method.equalsIgnoreCase("ce")) {
        	        Xout = _toeplitz_mult_ce(r, c, X, f_method);	
        	    }
        	    else if (t_method.equalsIgnoreCase("pd")) {
        	    	//Xout = _toeplitz_mult_pd(r, c, X, f_method);
        	    }
        	    else if (t_method.equalsIgnoreCase("mm")) {
        	    	double toep[][][] = toeplitz(c, r);
        	    	for (i = 0; i < M; i++) {
        	    	    for (j = 0; j < N; j++) {
        	    	    	Xout[0][i] += (toep[0][i][j]*X[0][j] - toep[1][i][j]*X[1][j]);
        	    	    	Xout[1][i] += (toep[0][i][j]*X[1][j] + toep[1][i][j]*X[0][j]);
        	    	    }
        	    	}
        	    }
        	    else if (t_method.equalsIgnoreCase("scipy")) {
        	    	//Efficient Toeplitz Matrix-Matrix Multiplication using FFT
        	    	double embedded_col[][] = new double[2][M+N-1];
        	    	for (i = 0; i < M; i++) {
        	    		embedded_col[0][i] = c[0][i];
        	    		embedded_col[1][i] = c[1][i];
        	    	}
        	    	for (i = 0; i < N-1; i++) {
        	    		embedded_col[0][i+M] = r[0][N-1-i];
        	    		embedded_col[1][i+M] = r[1][N-i-i];
        	    	}
        	    	FFTUtility fft = new FFTUtility(embedded_col[0], embedded_col[1], 1, M+N-1, 1, -1, FFTUtility.FFT);
        	    	fft.setShowProgress(false);
        	    	fft.run();
        	    	fft.finalize();
        	    	fft = null;
        	    	double Xpad[][] = new double[2][N+M-1];
        	    	for (i = 0; i < N; i++) {
        	    		Xpad[0][i] = X[0][i];
        	    		Xpad[1][i] = X[1][i];
        	    	}
        	    	fft = new FFTUtility(Xpad[0], Xpad[1], 1, M+N-1, 1, -1, FFTUtility.FFT);
        	    	fft.setShowProgress(false);
        	    	fft.run();
        	    	fft.finalize();
        	    	fft = null;
        	    	double colX[][] = new double[2][N+M-1];
        	    	for (i = 0; i < N+M-1; i++) {
        	    		colX[0][i] = embedded_col[0][i]*Xpad[0][i] - embedded_col[1][i]*Xpad[1][i];
        	    		colX[1][i] = embedded_col[0][i]*Xpad[1][i] + embedded_col[1][i]*Xpad[0][i];
        	    	}
        	    	FFTUtility ifft = new FFTUtility(colX[0], colX[1], 1, M+N-1, 1, 1, FFTUtility.FFT);
        	    	ifft.setShowProgress(false);
        	    	ifft.run();
        	        ifft.finalize();
        	        ifft = null;
        	        for (i = 0; i < N; i++) {
        	        	Xout[0][i] = colX[0][i];
        	        	Xout[1][i] = colX[1][i];
        	        }
        	    }
        	    else {
        	    	System.err.println("t_method " + t_method + " not recognized");
        	    	System.exit(0);
        	    }
        		for (i = 0; i < M; i++) {
        			zdiv(Xout[0][i], Xout[1][i], c[0][i], c[1][i], br, bi);
        			output[0][i] = br[0];
        			output[1][i] = bi[0];
        		}
        	} // else not simple
        	if (!forward) {
        		for (i = 0; i < M; i++) {
        			output[0][i] = output[0][i]/M;
        			output[1][i] = -output[1][i]/M;
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
    
    private double[][] _toeplitz_mult_ce(double r[][], double c[][], double x[][], String f_method) {
        // Multiply Toeplitz matrix by vector using circulant embedding.

        // See algorithm S1 in Sukhoy & Stoytchev 2019:

           // Compute the product y = Tx of a Toeplitz matrix T and a vector x, where
           // T is specified by its first row r = (r[0], r[1], r[2],...,r[N-1]) and
           // its first column c = (c[0], c[1], c[2],...,c[M-1]), where r[0] = c[0].

        // Args:
            // r (np.ndarray): first row of Toeplitz matrix
           //  c (np.ndarray): first column of Toeplitz matrix
            // x (np.ndarray): vector to multiply the Toeplitz matrix
            // f_method (str): FFT method. "FFTUtility", 'recursive'
                // for recursive method.

        // Returns:
            // np.ndarray: product of Toeplitz matrix and vector x

        int i;
        int N = r[0].length;
        int M = c[0].length;
        if  ((r[0][0] != c[0][0]) || (r[1][0] != c[1][0])) {
        	System.err.println("In _toeplitz_mult_ce r[0] = " + r[0][0] + " + " + r[1][0] + "i");
        	System.err.println("In _toeplitz_mult_ce c[0] = " + c[0][0] + " + " + c[1][0] + "i");
        	System.err.println("toeplitz_mult_ce must have r[0] = c[0]");
        	System.exit(0);
        }
        if (x[0].length != N) {
        	System.err.println("In _toeplitz_mult_ce x[0].length != N");
        	System.exit(0);
        }
        int n = (int)Math.pow(2,Math.ceil(log2(M + N - 1)));
        if (n < M) {
        	System.err.println("IN _toeplitz_mult_ce n < M");
        	System.exit(0);
        }
        if (n < N) {
        	System.err.println("IN _toeplitz_mult_ce n < N");
        	System.exit(0);
        }
        double chat[][] = new double[2][n];
        for (i = 0; i < M; i++) {
        	chat[0][i] = c[0][i];
        	chat[1][i] = c[1][i];
        }
        for (i = 0; i < N-1; i++) {
        	chat[0][i+n-(N-1)] = r[0][N-1-i];
        	chat[1][i+n-(N-1)] = r[1][N-1-i];
        }
        double xhat[][] = new double[2][n];
        for (i = 0; i < N; i++) {
        	xhat[0][i] = x[0][i];
        	xhat[1][i] = x[1][i];
        }
        double yhat[][] = _circulant_multiply(chat, xhat, f_method);
        double y[][] = new double[2][M];
        for (i = 0; i < M; i++) {
        	y[0][i] = yhat[0][i];
        	y[1][i] = yhat[1][i];
        }
        return y;
    }
    
    private double[][] _circulant_multiply(double c[][], double x[][], String f_method) {
        // Multiply a circulant matrix by a vector.

        // Runs in O(n log n) time.

        // See algorithm S4 in Sukhoy & Stoytchev 2019:

           // Compute the product y = Gx of a circulant matrix G and a vector x,
           // where G is generated by its first column c=(c[0], c[1],...,c[n-1]).

        // Args:
            // c (np.ndarray): first column of circulant matrix G
            // x (np.ndarray): vector x
            // f_method (str): FFT method. "FFTUtility",
            // 'recursive' for recursive method.

        // Returns:
            // np.ndarray: product Gx
        int i;
        int n = c[0].length;
        if (x[0].length != n) {
            System.err.println("In _circulant_multiply x[0].length != n");
            System.exit(0);
        }
        if (f_method.equalsIgnoreCase("FFTUtility")) {
            FFTUtility fft = new FFTUtility(c[0], c[1], 1, n, 1, -1, FFTUtility.FFT);
            fft.setShowProgress(false);
	    	fft.run();
	    	fft.finalize();
	    	fft = null;
	    	fft = new FFTUtility(x[0], x[1], 1, n, 1, -1, FFTUtility.FFT);
            fft.setShowProgress(false);
	    	fft.run();
	    	fft.finalize();
	    	fft = null;
	    	double Y[][] = new double[2][n];
	    	for (i = 0; i < n; i++) {
	    	    Y[0][i] = c[0][i]*x[0][i] - c[1][i]*x[1][i];
	    	    Y[1][i] = c[0][i]*x[1][i] + c[1][i]*x[0][i];
	    	}
	    	FFTUtility ifft = new FFTUtility(Y[0], Y[1], 1, n, 1, 1, FFTUtility.FFT);
            ifft.setShowProgress(false);
	    	ifft.run();
	    	ifft.finalize();
	    	ifft = null;
            return Y;
        }
        else if (f_method.equalsIgnoreCase("recursive")) {
            double C[][] = _fft(c);
            double X[][] = _fft(x);
            double Y[][] = new double[2][n];
            for (i = 0; i < n; i++) {
            	Y[0][i] = C[0][i]*X[0][i] - C[1][i]*X[1][i];
            	Y[1][i] = C[0][i]*X[1][i] + C[1][i]*X[0][i];
            }
            return _ifft(Y);
        }
        else {
            System.err.println("f_method not recognized in _circulant_multiply");
            return null;
        }
    }
    
    private double[][] _fft(double x[][]) {
        // Recursive FFT algorithm. Runs in O(n log n) time.

        // Args:
            // x (np.ndarray): input

        // Returns:
            // np.ndarray: FFT of x

        int i,j,k;
    	int n = x[0].length;
        if (n == 1) {
            return x;
        }
        double xe[][];
        double xo[][];
        if ((n % 2) == 1) {
        	xe = new double[2][(n+1)/2];
        	xo = new double[2][(n-1)/2];
        }
        else {
        	xe = new double[2][n/2];
        	xo = new double[2][n/2];
        }
        for (i = 0, j = 0; i < n; i += 2) {
        	xe[0][j] = x[0][i];
        	xe[1][j++] = x[1][i];
        }
        for (i = 1, j = 0; i < n; i += 2) {
        	xo[0][j] = x[0][i];
        	xo[1][j++] = x[1][i];
        }
        double y1[][] = _fft(xe);
        double y2[][] = _fft(xo);
        int nd2 = n/2;
        double w[][] = new double[2][nd2];
        for (k = 0; k < nd2; k++) {
        	w[0][k] = Math.cos(2.0 * Math.PI * k/n);
        	w[1][k] = -Math.sin(2.0 * Math.PI * k/n);
        }
        double y[][] = new double[2][n];
        for (i = 0; i < nd2; i++) {
        	y[0][i] = y1[0][i] + w[0][i]*y2[0][i] - w[1][i]*y2[1][i];
        	y[1][i] = y1[1][i] + w[0][i]*y2[1][i] + w[1][i]*y2[0][i];
        }
        for (i = nd2; i < n; i++) {
        	y[0][i] = y1[0][i-nd2] - w[0][i-nd2]*y2[0][i-nd2] + w[1][i-nd2]*y2[1][i-nd2];
        	y[1][i] = y1[1][i-nd2] - w[0][i-nd2]*y2[1][i-nd2] - w[1][i-nd2]*y2[0][i-nd2];

        }
        return y;
    }
    
    private double[][] _ifft(double y[][]) {
        // Recursive IFFT algorithm. Runs in O(n log n) time.

        // Args:
            // y (np.ndarray): input

        // Returns:
            // np.ndarray: IFFT of y

        int i,j,k;
    	int n = y[0].length;
        if (n == 1) {
            return y;
        }
        double ye[][];
        double yo[][];
        if ((n % 2) == 1) {
        	ye = new double[2][(n+1)/2];
        	yo = new double[2][(n-1)/2];
        }
        else {
        	ye = new double[2][n/2];
        	yo = new double[2][n/2];
        }
        for (i = 0, j = 0; i < n; i += 2) {
        	ye[0][j] = y[0][i];
        	ye[1][j++] = y[1][i];
        }
        for (i = 1, j = 0; i < n; i += 2) {
        	yo[0][j] = y[0][i];
        	yo[1][j++] = y[1][i];
        }
        double x1[][] = _ifft(ye);
        double x2[][] = _ifft(yo);
        int nd2 = n/2;
        double w[][] = new double[2][nd2];
        for (k = 0; k < nd2; k++) {
        	w[0][k] = Math.cos(2.0 * Math.PI * k/n);
        	w[1][k] = Math.sin(2.0 * Math.PI * k/n);
        }
        double x[][] = new double[2][n];
        for (i = 0; i < nd2; i++) {
        	x[0][i] = (x1[0][i] + w[0][i]*x2[0][i] - w[1][i]*x2[1][i])/2.0;
        	x[1][i] = (x1[1][i] + w[0][i]*x2[1][i] + w[1][i]*x2[0][i])/2.0;
        }
        for (i = nd2; i < n; i++) {
        	x[0][i] = (x1[0][i-nd2] - w[0][i-nd2]*x2[0][i-nd2] + w[1][i-nd2]*x2[1][i-nd2])/2.0;
        	x[1][i] = (x1[1][i-nd2] - w[0][i-nd2]*x2[1][i-nd2] - w[1][i-nd2]*x2[0][i-nd2])/2.0;

        }
        return x;
    }

    
    private double log2(double input) {
        return (Math.log10(input) / Math.log10(2.0));
	 }

    
    private double[][][] toeplitz(double c[][], double r[][]) {
    	int i,row,col;
    	double ans[][][] = new double[2][c[0].length][r[0].length];
    	for (i = 0; i < c[0].length; i++) {
    		ans[0][i][0] = c[0][i];
    		ans[1][i][0] = c[1][i];
    	}
    	for (i = 1; i < r[0].length; i++) {
    		ans[0][0][i] = r[0][i];
    		ans[1][0][i] = r[1][i];
    	}
    	for (row = 1; row < c[0].length; row++) {
    		for (col = 1; col < r[0].length; col++) {
    			ans[0][row][col] = ans[0][row-1][col-1];
    			ans[1][row][col] = ans[1][row-1][col-1];
    		}
    	}
    	return ans;
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
    
    /**
     * complex multiply c = a * b.
     * 
     * @param ar double
     * @param ai double
     * @param br double
     * @param bi double
     * @param cr double[]
     * @param ci double[]
     */
    private void zmlt(final double ar, final double ai, final double br, final double bi, final double[] cr,
            final double[] ci) {
        double ca, cb;

        ca = (ar * br) - (ai * bi);
        cb = (ar * bi) + (ai * br);
        cr[0] = ca;
        ci[0] = cb;

        return;
    }
}