package gov.nih.mipav.model.algorithms.filters;


import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.RandomNumberGen;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

/**
 * 
 * @author aailb
 * This is a port of MATLAB code in the ppft2 download package by Yoel Shkolnisky.
 * 
 * References:
 * 1.) Pseudo-Polar and Discrete Radon Transforms Software Package Documentation by Yoel Shkolnisky
       February 19, 2003.
   2.) DIRECT INVERSION OF THE THREE-DIMENSIONAL PSEUDO-POLAR FOURIER TRANSFORM
      AMIR AVERBUCH, GIL SHABAT, AND YOEL SHKOLNISKY SIAM J. SCI. COMPUT. 
      2016 Society for Industrial and Applied Mathematics
      Vol. 38, No. 2, pp. A1100–A1120
   3.) A FRAMEWORK FOR DISCRETE INTEGRAL TRANSFORMATIONS I – THE PSEUDO-POLAR FOURIER TRANSFORM 
       A. AVERBUCH, R.R. COIFMAN, D.L. DONOHO, M. ISRAELI, AND Y. SHKOLNISKY
   4.) Y. Keller, Y. Shkolnisky, and A. Averbuch. Volume registration using the 3-D pseudo-polar
       Fourier transform. IEEE Transactions on Signal Processing, 54(11):4323-4331, 2006. 
 *
 */

public class PseudoPolarFourierTransform extends AlgorithmBase {
	
	public PseudoPolarFourierTransform() {
		
	}
	
	/**
     * Starts the program.
     */
    public void runAlgorithm() {
    	
    }
    
    public void testCfft() {
    // All tests pass.
    
    // Tests the functions cfft, cfrft, icfft.
    // See also cfft, cfrft, icfft.
    
    // Yoel Shkolnisky 9/2/02

    double eps = 0.00000001;
    int j[] = new int[] {100,101,256,317};
    int i,m;
    RandomNumberGen randomGen = new RandomNumberGen();
    for (int k = 0; k < j.length; k++) {
    	i = j[k];
    	System.out.println("Testing with i = " + i);   
    	double x[][] = new double[2][i];
    	for (m = 0; m < i; m++) {
    		x[0][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    		x[1][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    	}

    	double cdftx[][]  = cdft(x);
    	double cfftx[][]  = cfft(x);
    	double frftx[][]  = frft(x,1);
    	double cfrftx[][] = cfrft(x,1);

    	//check frft all this vectors should be equal.
    	int failed = 0;
    	boolean realFailed;
    	boolean imagFailed;
    	for (m = 0; m < i; m++) {
    		realFailed = false;
    		imagFailed = false;
    		if ((cdftx[0][m] - cfftx[0][m]) > eps) {
    			System.err.println("cdftx[0]["+m+"] = " + cdftx[0][m]);
    			System.err.println("cfftx[0]["+m+"] = " + cfftx[0][m]);
    			realFailed = true;
    		}
    		if ((cdftx[1][m] - cfftx[1][m]) > eps) {
    			System.err.println("cdftx[1]["+m+"] = " + cdftx[1][m]);
    			System.err.println("cfftx[1]["+m+"] = " + cfftx[1][m]);
    			imagFailed = true;
    		}
    		if (realFailed || imagFailed) {
    			failed++;
    		}
    	}
    	
    	if (failed == 0) {
    		System.out.println("cfft OK");
    	}
    	else {
    		System.err.println("cfft failed " + failed + " times");
    	}

    	failed = 0;
    	for (m = 0; m < i; m++) {
    		realFailed = false;
    		imagFailed = false;
    		if ((cdftx[0][m] - frftx[0][m]) > eps) {
    			System.err.println("cdftx[0]["+m+"] = " + cdftx[0][m]);
    			System.err.println("frftx[0]["+m+"] = " + frftx[0][m]);
    			realFailed = true;
    		}
    		if ((cdftx[1][m] - frftx[1][m]) > eps) {
    			System.err.println("cdftx[1]["+m+"] = " + cdftx[1][m]);
    			System.err.println("frftx[1]["+m+"] = " + frftx[1][m]);
    			imagFailed = true;
    		}
    		if (realFailed || imagFailed) {
    			failed++;
    		}
    	}

    	if (failed == 0) {
    		System.out.println("frftx OK");
    	}
    	else {
    		System.err.println("frftx failed " + failed + " times");
    	}
    	
    	failed = 0;
    	for (m = 0; m < i; m++) {
    		realFailed = false;
    		imagFailed = false;
    		if ((frftx[0][m] - cfrftx[0][m]) > eps) {
    			System.err.println("frftx[0]["+m+"] = " + frftx[0][m]);
    			System.err.println("cfrftx[0]["+m+"] = " + cfrftx[0][m]);
    			realFailed = true;
    		}
    		if ((frftx[1][m] - cfrftx[1][m]) > eps) {
    			System.err.println("frftx[1]["+m+"] = " + frftx[1][m]);
    			System.err.println("cfrftx[1]["+m+"] = " + cfrftx[1][m]);
    			imagFailed = true;
    		}
    		if (realFailed || imagFailed) {
    			failed++;
    		}
    	}
    	
    	if (failed == 0) {
    		System.out.println("cfrftx OK");
    	}
    	else {
    		System.err.println("cfrftx failed " + failed + " times");
    	}

    	double icfftx[][] = icfft(x);
    	double icdftx[][] = icdft(x);
    	
    	for (m = 0; m < i; m++) {
    		realFailed = false;
    		imagFailed = false;
    		if ((icdftx[0][m] - icfftx[0][m]) > eps) {
    			System.err.println("icdftx[0]["+m+"] = " + icdftx[0][m]);
    			System.err.println("icfftx[0]["+m+"] = " + icfftx[0][m]);
    			realFailed = true;
    		}
    		if ((icdftx[1][m] - icfftx[1][m]) > eps) {
    			System.err.println("icdftx[1]["+m+"] = " + icdftx[1][m]);
    			System.err.println("icfftx[1]["+m+"] = " + icfftx[1][m]);
    			imagFailed = true;
    		}
    		if (realFailed || imagFailed) {
    			failed++;
    		}
    	}
    	
    	if (failed == 0) {
    		System.out.println("icfft OK");
    	}
    	else {
    		System.err.println("icfft failed " + failed + " times");
    	}

    }
  } // public void testCfft()
    
    public void testChirp() {
    	// All tests passed.
	    
	    // Tests the functions slowChirp and ChirpZ
	    
	    // Yoel Shkolnisky 13/1/03
	
	    // Compare slowChirp to odd fft.
	    // Compute the aliased Fourier transform for an odd sequence using slowChirp and verify the result.
    	RandomNumberGen randomGen = new RandomNumberGen();
    	int m;
	    System.out.println("Test 1: SlowChirp odd");
	    double x[][] = new double[2][7];
    	for (m = 0; m < x[0].length; m++) {
    		x[0][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    		x[1][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    	}
    	double A[] = new double[] {1.0,0.0};
    	double arg = 2*Math.PI/x[0].length;
    	double W[] = new double[2];
    	W[0] = Math.cos(arg);
    	W[1] = -Math.sin(arg);
	    double c1[][] = cfft(x);
	    double c2[][] = slowChirp(x,A,W,x[0].length);
	    compare(c1,c2);
	
	    // Compare slowChirp to even fft
	    // Compute the aliased Fourier transform for an even sequence using slowChirp and verify the result.
	    System.out.println("Test 2: SlowChirp even");
	    x = new double[2][8];
    	for (m = 0; m < x[0].length; m++) {
    		x[0][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    		x[1][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    	}
    	A = new double[] {1.0,0.0};
    	arg = 2*Math.PI/x[0].length;
    	W = new double[2];
    	W[0] = Math.cos(arg);
    	W[1] = -Math.sin(arg);
	    c1 = cfft(x);
	    c2 = slowChirp(x,A,W,x[0].length);
	    compare(c1,c2);
	
	    // After this point we assume that slowChirp work correctly.
	    // In the following tests we use slowChirp as a reference.
	
	    // Compare slowChirp and ChirpZ for odd random vector.
	    // Input length is equal to output length
	    System.out.println("Test 3: ChirpZ odd");
	    x = new double[2][11];
    	for (m = 0; m < x[0].length; m++) {
    		x[0][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    		x[1][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    	}
	    A[0] = Math.cos(6.5);
	    A[1] = Math.sin(6.5);
	    W[0] = Math.cos(2.31*Math.PI);
	    W[1] = -Math.sin(2.31*Math.PI);
	    c1 = slowChirp(x,A,W,x[0].length);
	    c2 = ChirpZ(x,A,W,x[0].length);
	    compare(c1,c2);
	
	    // Compare slowChirp and ChirpZ for even radon vector
	    // Input length is equal to output length
	    System.out.println("Test 4: ChirpZ even");
	    x = new double[2][16];
    	for (m = 0; m < x[0].length; m++) {
    		x[0][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    		x[1][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    	}
	    A[0] = 2;
	    A[1] = 0;
	    W[0] = Math.cos(2.31*Math.PI);
	    W[1] = -Math.sin(2.31*Math.PI);
	    c1 = slowChirp(x,A,W,x[0].length);
	    c2 = ChirpZ(x,A,W,x[0].length);
	    compare(c1,c2);
	
	    // Compare slowChirp and ChirpZ for odd random vector.
	    // Input length is different from output length
	    System.out.println("Test 5: ChirpZ odd (n<>M)");
	    x = new double[2][21];
    	for (m = 0; m < x[0].length; m++) {
    		x[0][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    		x[1][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    	}
	    A[0] = Math.cos(0.1);
	    A[1] = Math.sin(0.1);
	    W[0] = Math.cos(2.31);
	    W[1] = -Math.sin(2.31);
	    c1 = slowChirp(x,A,W,51);
	    c2 = ChirpZ(x,A,W,51);
	    compare(c1,c2);
	
	    // Compare slowChirp and ChirpZ for even random vector.
	    // Input length is different from output length
	    System.out.println("Test 6: ChirpZ even (n<>M)");
	    x = new double[2][32];
    	for (m = 0; m < x[0].length; m++) {
    		x[0][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    		x[1][m] = randomGen.genUniformRandomNum(0.0, 1.0);
    	}
    	A[0] = Math.cos(0.1);
	    A[1] = Math.sin(0.1);
	    W[0] = Math.cos(2.31);
	    W[1] = -Math.sin(2.31);
	    c1 = slowChirp(x,A,W,64);
	    c2 = ChirpZ(x,A,W,64);
	    compare(c1,c2);
    } // public void testChirp()
       
    private void compare(double c1[][], double c2[][]) {
    	int n = c1[0].length;
    	int i;
    	int failed = 0;
    	
    	for (i = 0; i < n; i++) {
    		boolean realFailed = false;
    		boolean imagFailed = false;
    		if (Math.abs(c1[0][i] - c2[0][i]) > 1.0E-11) {
    			System.err.println("c1[0]["+i+"] = " + c1[0][i]);
    			System.err.println("c2[0]["+i+"] = " + c2[0][i]);
    			realFailed = true;
    		}
    		if (Math.abs(c1[1][i] - c2[1][i]) > 1.0E-11) {
    			System.err.println("c1[1]["+i+"] = " + c1[1][i]);
    			System.err.println("c2[1]["+i+"] = " + c2[1][i]);
    			imagFailed = true;
    		}
    		if (realFailed || imagFailed) {
    			failed++;
    		}
    	}
    	if (failed == 0) {
    		System.out.println("OK");
    	}
    	else {
    		System.err.println(failed + " tests failed");
    	}
    }
     
    
    private int lowIdx(int n) {
    		
    		// Return the minimal index for an aliased sequence of length n.
    		
    		// 	n	 the length of the indexed sequence.
    		
    		// For example:
    		//	For n=5, the indices of the aliased sequence are -2 -1 0 1 2.
    		//	Hence, lowIdx(n) = -2.
    		//	For n=4, the indices of the aliased sequence are -2 -1 0 1.
    		//	Hence, lowIdx(n) = -2.
    		//
    		// Yoel Shkolnisky 22/10/01

    	    //int idx = -fix(n/2);
    	    int idx = -n/2;
    	    return idx;
    }
    
    private int hiIdx(int n) {
    	// Returns the maximal index for an aliased sequence of length n.
    	//
    	// n     the length of the indexed sequence.
    	
    	// For example:
    	//   For n=5, the indices of the aliased sequence are -2 -1 0 1 2.
    	//   Hence, hiIdx(n) = 2.
    	//   For n=4, the indices of the aliased sequence are -2 -1 0 1.
    	//   Hence, hiIdx(n) = 1.
    	
    	// Yoel Shkolnisky 22/10/01

        // idx = fix((n-0.5)/2);

    	// The above code actually performs:
    	int idx;
    	if ((n % 2)==0) {
           idx = n/2-1;
    	}
    	else    {
    		idx = (n-1)/2;
    	}
    	return idx;
    }
    
    private int toUnaliasedIdx(int idx,int n) {
    	// Converts an index from the range -n/2...n/2-1 to an index in the range 0...n-1.
    	// Both odd and even values of n are handled. 
    	
    	// idx    An index from the range -n/2...n/2-1.
    	// n      The range of indices.
    	
    	// Returns the index "idx" scaled to the range 0...n-1.
    	 
    	// For example:
    	//       n   = 5;
    	//       idx = -1;
    	//       toUnaliasedIdx(idx,n) will return 1:
    	//           -2 -1 0 1 2
    	//            ^
    	//        the index of -1 is 1 if scaled to 0...n-1.
    	
    	// Yoel Shkolnisky 22/10/01

    	int y = idx+ n/2;
    	return y;
    }
    
    private double[][] cdft(double x[]) {
    	// Aliased DFT of the real sequence x.
    	// The DFT is computed directly using O(n^2) operations.
    	
    	// x    The sequence whose DFT should be computed. Can be of odd or even length.
    	
    	// Returns the aliased DFT of the sequence x.
    	 
    	// Yoel Shkolnisky 22/10/01
    	int m= x.length;
    	double y[][] = new double[2][m];

    	for (int k=lowIdx(m); k <= hiIdx(m); k++) {
    	   double acc = 0.0;
    	   double accImag = 0.0;
    	   for (int j=lowIdx(m); j <= hiIdx(m); j++) {
    		  // acc = acc + x(toUnaliasedIdx(j,m))* exp(-2*pi*i*j*k/m);
    		  int idx = toUnaliasedIdx(j,m);
    		  double arg = 2*Math.PI*j*k/m;
    		  double cos = Math.cos(arg);
    		  double sin = Math.sin(arg);
    	      acc = acc + x[idx]* cos;
    	      accImag = accImag -x[idx]*sin;
    	   }
    	   int idxOut = toUnaliasedIdx(k,m);
    	   y[0][idxOut] = acc;
    	   y[1][idxOut] = accImag;
        }
    	return y;
    }
    
    private double[][] cdft(double x[][]) {
    	// Aliased DFT of the complex sequence x.
    	// The DFT is computed directly using O(n^2) operations.
    	
    	// x    The sequence whose DFT should be computed. Can be of odd or even length.
    	
    	// Returns the aliased DFT of the sequence x.
    	 
    	// Yoel Shkolnisky 22/10/01
    	int m= x[0].length;
    	double y[][] = new double[2][m];

    	for (int k=lowIdx(m); k <= hiIdx(m); k++) {
    	   double acc = 0.0;
    	   double accImag = 0.0;
    	   for (int j=lowIdx(m); j <= hiIdx(m); j++) {
    		  // acc = acc + x(toUnaliasedIdx(j,m))* exp(-2*pi*i*j*k/m);
    		  int idx = toUnaliasedIdx(j,m);
    		  double arg = 2*Math.PI*j*k/m;
    		  double cos = Math.cos(arg);
    		  double sin = Math.sin(arg);
    	      acc = acc + x[0][idx]* cos + x[1][idx]*sin;
    	      accImag = accImag -x[0][idx]*sin + x[1][idx]*cos;
    	   }
    	   int idxOut = toUnaliasedIdx(k,m);
    	   y[0][idxOut] = acc;
    	   y[1][idxOut] = accImag;
        }
    	return y;
    }
    
    private double[][][] cdft2(double x[][]) {
    	// Returns the aliased 2D DFT of the real image x.
    	 
    	// Yoel Shkolnisky 22/10/01

        int my = x.length;
        int mx = x[0].length;
    	double y[][][] = new double[2][my][mx];

    	for (int xi1=lowIdx(mx); xi1 <= hiIdx(mx); xi1++) {    // omegaX direction
    	   for (int xi2=lowIdx(my); xi2 <= hiIdx(my); xi2++) { // omegaY direction
    		  double acc = 0.0;
    		  double accImag = 0.0;
    	      for (int u=lowIdx(mx); u <= hiIdx(mx); u++) {  // x direction
    	         for (int v=lowIdx(my); v <= hiIdx(my); v++) { // y direction
    	        	int idy = toUnaliasedIdx(v,my);
    	        	int idx = toUnaliasedIdx(u,mx);
    	        	double arg = 2.0*Math.PI*(u*xi1/mx + v*xi2/my);
    	        	double cos = Math.cos(arg);
    	    		double sin = Math.sin(arg);
    	    	    acc = acc + x[idy][idx]* cos;
    	    	    accImag = accImag -x[idy][idx]*sin;
    	         }      
    	      }
    	      int idyOut = toUnaliasedIdx(xi2,my);
    	      int idxOut = toUnaliasedIdx(xi1,mx);
    	      y[0][idyOut][idxOut] = acc;
    	      y[1][idyOut][idxOut] = accImag;
    	   }
    	}
    	return y;
    }
    
    private double[][][] cdft2(double x[][][]) {
    	// Returns the aliased 2D DFT of the complex image x.
    	 
    	// Yoel Shkolnisky 22/10/01

        int my = x[0].length;
        int mx = x[0][0].length;
    	double y[][][] = new double[2][my][mx];

    	for (int xi1=lowIdx(mx); xi1 <= hiIdx(mx); xi1++) {    // omegaX direction
    	   for (int xi2=lowIdx(my); xi2 <= hiIdx(my); xi2++) { // omegaY direction
    		  double acc = 0.0;
    		  double accImag = 0.0;
    	      for (int u=lowIdx(mx); u <= hiIdx(mx); u++) {  // x direction
    	         for (int v=lowIdx(my); v <= hiIdx(my); v++) { // y direction
    	        	int idy = toUnaliasedIdx(v,my);
    	        	int idx = toUnaliasedIdx(u,mx);
    	        	double arg = 2.0*Math.PI*(u*xi1/mx + v*xi2/my);
    	        	double cos = Math.cos(arg);
    	    		double sin = Math.sin(arg);
    	    	    acc = acc + x[0][idy][idx]* cos + x[1][idy][idx]*sin;
    	    	    accImag = accImag -x[0][idy][idx]*sin + x[1][idy][idx]*cos;
    	         }      
    	      }
    	      int idyOut = toUnaliasedIdx(xi2,my);
    	      int idxOut = toUnaliasedIdx(xi1,mx);
    	      y[0][idyOut][idxOut] = acc;
    	      y[1][idyOut][idxOut] = accImag;
    	   }
    	}
    	return y;
    }
    
    private double[][][][] cdft3(double x[][][]) {
    	// Aliased 3D DFT of the real image x.
    	// The DFT is computed directly using O(n^6) operations.
    	// The function serves as a reference function to test the correctness of the 3D
    	// Fourier routines.
    	
    	// x   The sequence whose DFT should be computed. Can be of odd or even length.
    	
    	// Returns the aliased 3D DFT of the image x.
     
    	// Yoel Shkolnisky 11/1/03
    	
    	int mz = x.length;
        int my = x[0].length;
        int mx = x[0][0].length;
    	double y[][][][] = new double[2][mz][my][mx];

    	for (int xi1=lowIdx(mx); xi1 <= hiIdx(mx); xi1++) {    // omegaX direction
    	   for (int xi2=lowIdx(my); xi2 <= hiIdx(my); xi2++) { // omegaY direction
    		  for (int xi3 = lowIdx(mz); xi3 <= hiIdx(mz); xi3++) { // omegaZ direction
	    		  double acc = 0.0;
	    		  double accImag = 0.0;
	    	      for (int u=lowIdx(mx); u <= hiIdx(mx); u++) {  // x direction
	    	         for (int v=lowIdx(my); v <= hiIdx(my); v++) { // y direction
	    	        	for (int w = lowIdx(mz); w <= hiIdx(mz); w++) { // z direction
	    	        		int idz = toUnaliasedIdx(w,mz);
		    	        	int idy = toUnaliasedIdx(v,my);
		    	        	int idx = toUnaliasedIdx(u,mx);
		    	        	double arg = 2.0*Math.PI*(u*xi1/mx + v*xi2/my + w*xi3/mz);
		    	        	double cos = Math.cos(arg);
		    	    		double sin = Math.sin(arg);
		    	    	    acc = acc + x[idz][idy][idx]* cos;
		    	    	    accImag = accImag -x[idz][idy][idx]*sin;
	    	        	}
	    	         }      
	    	      }
	    	      int idzOut = toUnaliasedIdx(xi3,mz);
	    	      int idyOut = toUnaliasedIdx(xi2,my);
	    	      int idxOut = toUnaliasedIdx(xi1,mx);
    	          y[0][idzOut][idyOut][idxOut] = acc;
    	          y[1][idzOut][idyOut][idxOut] = accImag;
    		  }
    	   }
    	}
    	return y;
    }
    
    private double[][][][] cdft3(double x[][][][]) {
    	// Aliased 3D DFT of the complex image x.
    	// The DFT is computed directly using O(n^6) operations.
    	// The function serves as a reference function to test the correctness of the 3D
    	// Fourier routines.
    	
    	// x   The sequence whose DFT should be computed. Can be of odd or even length.
    	
    	// Returns the aliased 3D DFT of the image x.
     
    	// Yoel Shkolnisky 11/1/03
    	
    	int mz = x[0].length;
        int my = x[0][0].length;
        int mx = x[0][0][0].length;
    	double y[][][][] = new double[2][mz][my][mx];

    	for (int xi1=lowIdx(mx); xi1 <= hiIdx(mx); xi1++) {    // omegaX direction
    	   for (int xi2=lowIdx(my); xi2 <= hiIdx(my); xi2++) { // omegaY direction
    		  for (int xi3 = lowIdx(mz); xi3 <= hiIdx(mz); xi3++) { // omegaZ direction
	    		  double acc = 0.0;
	    		  double accImag = 0.0;
	    	      for (int u=lowIdx(mx); u <= hiIdx(mx); u++) {  // x direction
	    	         for (int v=lowIdx(my); v <= hiIdx(my); v++) { // y direction
	    	        	for (int w = lowIdx(mz); w <= hiIdx(mz); w++) { // z direction
	    	        		int idz = toUnaliasedIdx(w,mz);
		    	        	int idy = toUnaliasedIdx(v,my);
		    	        	int idx = toUnaliasedIdx(u,mx);
		    	        	double arg = 2.0*Math.PI*(u*xi1/mx + v*xi2/my + w*xi3/mz);
		    	        	double cos = Math.cos(arg);
		    	    		double sin = Math.sin(arg);
		    	    	    acc = acc + x[0][idz][idy][idx]* cos + x[1][idz][idy][idx]*sin;
		    	    	    accImag = accImag -x[0][idz][idy][idx]*sin + x[1][idz][idy][idx]*cos;
	    	        	}
	    	         }      
	    	      }
	    	      int idzOut = toUnaliasedIdx(xi3,mz);
	    	      int idyOut = toUnaliasedIdx(xi2,my);
	    	      int idxOut = toUnaliasedIdx(xi1,mx);
    	          y[0][idzOut][idyOut][idxOut] = acc;
    	          y[1][idzOut][idyOut][idxOut] = accImag;
    		  }
    	   }
    	}
    	return y;
    }
    
    private double[][] icdft(double x[][]) {
		//
		// Aliased inverse discrete Fourier transform (IDFT) of the complex sequence x.
		// The DFT is computed directly using O(n^2) operations.
		
		// x    The sequence whose IDFT should be computed. Can be of odd or even length.
		
		// Returns the aliased IDFT of the sequence x.
		 
		// Yoel Shkolnisky 22/10/01
    	
    	int m= x[0].length;
    	double y[][] = new double[2][m];

    	for (int k=lowIdx(m); k <= hiIdx(m); k++) {
    	   double acc = 0.0;
    	   double accImag = 0.0;
    	   for (int j=lowIdx(m); j <= hiIdx(m); j++) {
    		  // acc = acc + x(toUnaliasedIdx(j,m))* exp(2*pi*i*j*k/m);
    		  int idx = toUnaliasedIdx(j,m);
    		  double arg = 2*Math.PI*j*k/m;
    		  double cos = Math.cos(arg);
    		  double sin = Math.sin(arg);
    	      acc = acc + x[0][idx]* cos - x[1][idx]*sin;
    	      accImag = accImag +x[0][idx]*sin + x[1][idx]*cos;
    	   }
    	   int idxOut = toUnaliasedIdx(k,m);
    	   y[0][idxOut] = acc/m;
    	   y[1][idxOut] = accImag/m;
        }
    	return y;
    }
    
    private double[][] fftshift1d(double x[][]) {
    	// Performs 1D fftshift.
    	// A fast implementation of Matlab's fftshift for 1D vectors.
    	
    	// x   The vector to fftshift. Must be a 1D vector.
    	
    	// Yoel Shkolnisky 14/1/03

    	int m= x[0].length;
    	int p=(int)Math.ceil(m/2.0);
    	double y[][] = new double[2][m];
    	int i;
    	for (i = p; i < m; i++) {
    		y[0][i-p] = x[0][i];
    		y[1][i-p] = x[1][i];
    	}
    	for (i = 0; i < p; i++) {
    		y[0][m-p+i] = x[0][i];
    		y[1][m-p+i] = x[1][i];
    	}
        return y;	
    }
    
    private double[][] ifftshift1d(double x[][]) {
    	// Performs 1D ifftshift.
    	// A fast implementation of Matlab's ifftshift for 1D vectors.
    	
    	// x   The vector to ifftshift. Must be a 1D vector.
    	
    	// Yoel Shkolnisky 14/1/03

    	int m= x[0].length;
    	int p=m/2;
    	double y[][] = new double[2][m];
    	int i;
    	for (i = p; i < m; i++) {
    		y[0][i-p] = x[0][i];
    		y[1][i-p] = x[1][i];
    	}
    	for (i = 0; i < p; i++) {
    		y[0][m-p+i] = x[0][i];
    		y[1][m-p+i] = x[1][i];
    	}
        return y;		
    }
    
    private double[][] cfft(double x[][]) {
    	// Aliased FFT of the sequence x.
    	// The FFT is computed using O(nlogn) operations.
    	
    	// x   The sequence whose FFT should be computed. 
    	//     Can be of odd or even length. Must be a 1-D vector.
    	
    	// Returns the aliased FFT of the sequence x.
    	 
    	// Yoel Shkolnisky 22/10/01
    	double ix[][] = ifftshift1d(x);
    	FFTUtility fft = new FFTUtility(ix[0],ix[1],1,x[0].length,1,-1,FFTUtility.FFT);
    	fft.run();
    	double y[][] = fftshift1d(ix);
        return y;	
    }
    
    private double[][] icfft(double x[][]) {
    	// Aliased inverse Fourier transform (IFFT) of the sequence x.
    	// The FFT is computed using O(nlogn) operations.
    	
    	// x    The sequence whose IFFT should be computed. 
    	//      Can be of odd or even length. Must be a 1D vector.
    	
    	// Returns the aliased IFFT of the sequence x.
    
    	// Yoel Shkolnisky 22/10/01
    	double ix[][] = ifftshift1d(x);
    	// Inverse FFT
    	FFTUtility fft = new FFTUtility(ix[0],ix[1],1,x[0].length,1,1,FFTUtility.FFT);
    	fft.run();
    	double y[][] = fftshift1d(ix);
    	return y;
    }
    
    private double[][][] fftshift(double in[][][]) {
    	int yDim = in[0].length;
    	int xDim = in[0][0].length;
		double out[][][] = new double[2][yDim][xDim];
		int highestxquad1Index;
		int highestyquad1Index;
		int quad1height;
		int quad1width;
		int quad2height;
		int quad2width;
		int quad3height;
		int quad3width;
		int quad4height;
		int quad4width;
		int y,x;
		if ((yDim %2) == 1) {
			//yodd = true;
			highestyquad1Index = (yDim - 1)/2;
		}
		else {
			//yodd = false;
			highestyquad1Index = yDim/2 - 1;
		}
		if ((xDim % 2) == 1) {
			//xodd = true;
			highestxquad1Index = (xDim - 1)/2;
		}
		else {
			//xodd = false;
			highestxquad1Index = xDim/2 - 1;
		}
		quad1width = highestxquad1Index + 1;
		quad1height = highestyquad1Index + 1;
		quad2width = xDim - quad1width;
		quad2height = quad1height;
		quad3width = quad2width;
		quad3height = yDim - quad1height;
		quad4width = quad1width;
		quad4height = quad3height;
		double quad1[][][] = new double[2][quad1height][quad1width];
		double quad2[][][] = new double[2][quad2height][quad2width];
		double quad3[][][] = new double[2][quad3height][quad3width];
		double quad4[][][] = new double[2][quad4height][quad4width];
		for (y = 0; y <= highestyquad1Index; y++) {
			for (x = 0; x <= highestxquad1Index; x++) {
				quad1[0][y][x] = in[0][y][x];
				quad1[1][y][x] = in[1][y][x];
			}
		}
		for (y = 0; y <= highestyquad1Index; y++) {
			for (x = highestxquad1Index+1; x < xDim; x++) {
				quad2[0][y][x - highestxquad1Index-1] = in[0][y][x];
				quad2[1][y][x - highestxquad1Index-1] = in[1][y][x];
			}
		}
		for (y = highestyquad1Index+1; y < yDim; y++) {
			for (x = highestxquad1Index+1; x < xDim; x++) {
				quad3[0][y - highestyquad1Index - 1][x - highestxquad1Index - 1] = in[0][y][x];
				quad3[1][y - highestyquad1Index - 1][x - highestxquad1Index - 1] = in[1][y][x];
			}
		}
		for (y = highestyquad1Index+1; y < yDim; y++) {
			for (x = 0; x <= highestxquad1Index; x++) {
				quad4[0][y - highestyquad1Index - 1][x] = in[0][y][x];
				quad4[1][y - highestyquad1Index - 1][x] = in[1][y][x];
			}
		}
		
		// Move third quadrant to first
		for (y = 0; y < quad3height; y++) {
			for (x = 0; x < quad3width; x++) {
				out[0][y][x] = quad3[0][y][x];
				out[1][y][x] = quad3[1][y][x];
			}
		}
		// Move fourth quadrant to second
		for (y = 0; y < quad4height; y++) {
			for (x = 0; x < quad4width; x++) {
				out[0][y][x + quad3width] = quad4[0][y][x];
				out[1][y][x + quad3width] = quad4[1][y][x];
			}
		}
		// Move first quadrant to third
		for (y = 0; y < quad1height; y++) {
			for (x = 0; x < quad1width; x++) {
				out[0][y+quad3height][x + quad3width] = quad1[0][y][x];
				out[1][y+quad3height][x + quad3width] = quad1[1][y][x];
			}
		}
		// Move second quadrant to fourth
		for (y = 0; y < quad2height; y++) {
			for (x = 0; x < quad2width; x++) {
				out[0][y+quad3height][x] = quad2[0][y][x];
				out[1][y+quad3height][x] = quad2[1][y][x];
			}
		}
		return out;
	}
    
    private double[][][] ifftshift(double in[][][]) {
    	int yDim = in[0].length;
    	int xDim = in[0][0].length;
		double out[][][] = new double[2][yDim][xDim];
		int highestxquad1Index;
		int highestyquad1Index;
		int quad1height;
		int quad1width;
		int quad2height;
		int quad2width;
		int quad3height;
		int quad3width;
		int quad4height;
		int quad4width;
		int y,x;
		if ((yDim %2) == 1) {
			//yodd = true;
			highestyquad1Index = (yDim - 1)/2 - 1;
		}
		else {
			//yodd = false;
			highestyquad1Index = yDim/2 - 1;
		}
		if ((xDim % 2) == 1) {
			//xodd = true;
			highestxquad1Index = (xDim - 1)/2 - 1;
		}
		else {
			//xodd = false;
			highestxquad1Index = xDim/2 - 1;
		}
		quad1width = highestxquad1Index + 1;
		quad1height = highestyquad1Index + 1;
		quad2width = xDim - quad1width;
		quad2height = quad1height;
		quad3width = quad2width;
		quad3height = yDim - quad1height;
		quad4width = quad1width;
		quad4height = quad3height;
		double quad1[][][] = new double[2][quad1height][quad1width];
		double quad2[][][] = new double[2][quad2height][quad2width];
		double quad3[][][] = new double[2][quad3height][quad3width];
		double quad4[][][] = new double[2][quad4height][quad4width];
		for (y = 0; y <= highestyquad1Index; y++) {
			for (x = 0; x <= highestxquad1Index; x++) {
				quad1[0][y][x] = in[0][y][x];
				quad1[1][y][x] = in[1][y][x];
			}
		}
		for (y = 0; y <= highestyquad1Index; y++) {
			for (x = highestxquad1Index+1; x < xDim; x++) {
				quad2[0][y][x - highestxquad1Index-1] = in[0][y][x];
				quad2[1][y][x - highestxquad1Index-1] = in[1][y][x];
			}
		}
		for (y = highestyquad1Index+1; y < yDim; y++) {
			for (x = highestxquad1Index+1; x < xDim; x++) {
				quad3[0][y - highestyquad1Index - 1][x - highestxquad1Index - 1] = in[0][y][x];
				quad3[1][y - highestyquad1Index - 1][x - highestxquad1Index - 1] = in[1][y][x];
			}
		}
		for (y = highestyquad1Index+1; y < yDim; y++) {
			for (x = 0; x <= highestxquad1Index; x++) {
				quad4[0][y - highestyquad1Index - 1][x] = in[0][y][x];
				quad4[1][y - highestyquad1Index - 1][x] = in[1][y][x];
			}
		}
		
		// Move third quadrant to first
		for (y = 0; y < quad3height; y++) {
			for (x = 0; x < quad3width; x++) {
				out[0][y][x] = quad3[0][y][x];
				out[1][y][x] = quad3[1][y][x];
			}
		}
		// Move fourth quadrant to second
		for (y = 0; y < quad4height; y++) {
			for (x = 0; x < quad4width; x++) {
				out[0][y][x + quad3width] = quad4[0][y][x];
				out[1][y][x + quad3width] = quad4[1][y][x];
			}
		}
		// Move first quadrant to third
		for (y = 0; y < quad1height; y++) {
			for (x = 0; x < quad1width; x++) {
				out[0][y+quad3height][x + quad3width] = quad1[0][y][x];
				out[1][y+quad3height][x + quad3width] = quad1[1][y][x];
			}
		}
		// Move second quadrant to fourth
		for (y = 0; y < quad2height; y++) {
			for (x = 0; x < quad2width; x++) {
				out[0][y+quad3height][x] = quad2[0][y][x];
				out[1][y+quad3height][x] = quad2[1][y][x];
			}
		}
		return out;
	}
    
    private double[][][][] fftshift(double in[][][][]) {
    	int zDim = in[0].length;
    	int yDim = in[0][0].length;
    	int xDim = in[0][0][0].length;
		double out[][][][] = new double[2][zDim][yDim][xDim];
		int highestxoct1Index;
		int highestyoct1Index;
		int highestzoct1Index;
		int oct1depth;
		int oct1height;
		int oct1width;
		int oct2depth;
		int oct2height;
		int oct2width;
		int oct3depth;
		int oct3height;
		int oct3width;
		int oct4depth;
		int oct4height;
		int oct4width;
		int oct5depth;
		int oct5height;
		int oct5width;
		int oct6depth;
		int oct6height;
		int oct6width;
		int oct7depth;
		int oct7height;
		int oct7width;
		int oct8depth;
		int oct8height;
		int oct8width;
		int z,y,x;
		if ((zDim % 2) == 1) {
			highestzoct1Index = (zDim - 1)/2;
		}
		else {
			highestzoct1Index = zDim/2 - 1;
		}
		if ((yDim %2) == 1) {
			//yodd = true;
			highestyoct1Index = (yDim - 1)/2;
		}
		else {
			//yodd = false;
			highestyoct1Index = yDim/2 - 1;
		}
		if ((xDim % 2) == 1) {
			//xodd = true;
			highestxoct1Index = (xDim - 1)/2;
		}
		else {
			//xodd = false;
			highestxoct1Index = xDim/2 - 1;
		}
		oct1width = highestxoct1Index + 1;
		oct1height = highestyoct1Index + 1;
		oct1depth = highestzoct1Index + 1;
		oct2width = xDim - oct1width;
		oct2height = oct1height;
		oct2depth = oct1depth;
		oct3width = oct2width;
		oct3height = yDim - oct1height;
		oct3depth = oct1depth;
		oct4width = oct1width;
		oct4height = oct3height;
		oct4depth = oct1depth;
		oct5width = oct1width;
		oct5height = oct1height;
		oct5depth = zDim - oct1depth;
		oct6width = oct2width;
		oct6height = oct2height;
		oct6depth = zDim - oct2depth;
		oct7width = oct3width;
		oct7height = oct3height;
		oct7depth = zDim - oct3depth;
		oct8width = oct4width;
		oct8height = oct4height;
		oct8depth = zDim - oct4depth;
		double oct1[][][][] = new double[2][oct1depth][oct1height][oct1width];
		double oct2[][][][] = new double[2][oct2depth][oct2height][oct2width];
		double oct3[][][][] = new double[2][oct3depth][oct3height][oct3width];
		double oct4[][][][] = new double[2][oct4depth][oct4height][oct4width];
		double oct5[][][][] = new double[2][oct5depth][oct5height][oct5width];
		double oct6[][][][] = new double[2][oct6depth][oct6height][oct6width];
		double oct7[][][][] = new double[2][oct7depth][oct7height][oct7width];
		double oct8[][][][] = new double[2][oct8depth][oct8height][oct8width];
		for (z = 0; z <= highestzoct1Index; z++) {
			for (y = 0; y <= highestyoct1Index; y++) {
				for (x = 0; x <= highestxoct1Index; x++) {
					oct1[0][z][y][x] = in[0][z][y][x];
					oct1[1][z][y][x] = in[1][z][y][x];
				}
			}
		}
		for (z = 0; z <= highestzoct1Index; z++) {
			for (y = 0; y <= highestyoct1Index; y++) {
				for (x = highestxoct1Index+1; x < xDim; x++) {
					oct2[0][z][y][x - highestxoct1Index-1] = in[0][z][y][x];
					oct2[1][z][y][x - highestxoct1Index-1] = in[1][z][y][x];
				}
			}
		}
		for (z = 0; z <= highestzoct1Index; z++) {
			for (y = highestyoct1Index+1; y < yDim; y++) {
				for (x = highestxoct1Index+1; x < xDim; x++) {
					oct3[0][z][y - highestyoct1Index - 1][x - highestxoct1Index - 1] = in[0][z][y][x];
					oct3[1][z][y - highestyoct1Index - 1][x - highestxoct1Index - 1] = in[1][z][y][x];
				}
			}
		}
		for (z = 0; z <= highestzoct1Index; z++) {
			for (y = highestyoct1Index+1; y < yDim; y++) {
				for (x = 0; x <= highestxoct1Index; x++) {
					oct4[0][z][y - highestyoct1Index - 1][x] = in[0][z][y][x];
					oct4[1][z][y - highestyoct1Index - 1][x] = in[1][z][y][x];
				}
			}
        }
		for (z = highestzoct1Index+1; z < zDim; z++) {
			for (y = 0; y <= highestyoct1Index; y++) {
				for (x = 0; x <= highestxoct1Index; x++) {
					oct5[0][z - highestzoct1Index - 1][y][x] = in[0][z][y][x];
					oct5[1][z - highestzoct1Index - 1][y][x] = in[1][z][y][x];
				}
			}
		}
		for (z = highestzoct1Index+1; z < zDim; z++) {
			for (y = 0; y <= highestyoct1Index; y++) {
				for (x = highestxoct1Index+1; x < xDim; x++) {
					oct6[0][z - highestzoct1Index - 1][y][x - highestxoct1Index-1] = in[0][z][y][x];
					oct6[1][z - highestzoct1Index - 1][y][x - highestxoct1Index-1] = in[1][z][y][x];
				}
			}
		}
		for (z = highestzoct1Index+1; z < zDim; z++) {
			for (y = highestyoct1Index+1; y < yDim; y++) {
				for (x = highestxoct1Index+1; x < xDim; x++) {
					oct7[0][z - highestzoct1Index - 1][y - highestyoct1Index - 1][x - highestxoct1Index - 1] = in[0][z][y][x];
					oct7[1][z - highestzoct1Index - 1][y - highestyoct1Index - 1][x - highestxoct1Index - 1] = in[1][z][y][x];
				}
			}
		}
		for (z = highestzoct1Index+1; z < zDim; z++) {
			for (y = highestyoct1Index+1; y < yDim; y++) {
				for (x = 0; x <= highestxoct1Index; x++) {
					oct8[0][z - highestzoct1Index - 1][y - highestyoct1Index - 1][x] = in[0][z][y][x];
					oct8[1][z - highestzoct1Index - 1][y - highestyoct1Index - 1][x] = in[1][z][y][x];
				}
			}
        }
		
		// Move seventh octant to first
		for (z = 0; z < oct7depth; z++) {
			for (y = 0; y < oct7height; y++) {
				for (x = 0; x < oct7width; x++) {
					out[0][z][y][x] = oct7[0][z][y][x];
					out[1][z][y][x] = oct7[1][z][y][x];
				}
			}
		}
		// Move eighth octant to second
		for (z = 0; z < oct8depth; z++) {
			for (y = 0; y < oct8height; y++) {
				for (x = 0; x < oct8width; x++) {
					out[0][z][y][x + oct7width] = oct8[0][z][y][x];
					out[1][z][y][x + oct7width] = oct8[1][z][y][x];
				}
			}
		}
		// Move fifth octant to third
		for (z = 0; z < oct5depth; z++) {
			for (y = 0; y < oct5height; y++) {
				for (x = 0; x < oct5width; x++) {
					out[0][z][y+oct7height][x + oct7width] = oct5[0][z][y][x];
					out[1][z][y+oct7height][x + oct7width] = oct5[1][z][y][x];
				}
			}
		}
		// Move six octant to fourth
		for (z = 0; z < oct6depth; z++) {
			for (y = 0; y < oct6height; y++) {
				for (x = 0; x < oct6width; x++) {
					out[0][z][y+oct7height][x] = oct6[0][z][y][x];
					out[1][z][y+oct7height][x] = oct6[1][z][y][x];
				}
			}
		}
		// Move first octant to seventh
		for (z = 0; z < oct1depth; z++) {
			for (y = 0; y < oct1height; y++) {
				for (x = 0; x < oct1width; x++) {
					out[0][z + oct7depth][y + oct7height][x + oct7width] = oct1[0][z][y][x];
					out[1][z + oct7depth][y + oct7height][x + oct7width] = oct1[1][z][y][x];
				}
			}
		}
		// Move second octant to eighth
		for (z = 0; z < oct2depth; z++) {
			for (y = 0; y < oct2height; y++) {
				for (x = 0; x < oct2width; x++) {
					out[0][z + oct7depth][y + oct7height][x] = oct2[0][z][y][x];
					out[1][z + oct7depth][y + oct7height][x] = oct2[1][z][y][x];
				}
			}
		}
		// Move third octant to fifth
		for (z = 0; z < oct3depth; z++) {
			for (y = 0; y < oct3height; y++) {
				for (x = 0; x < oct3width; x++) {
					out[0][z + oct7depth][y][x] = oct3[0][z][y][x];
					out[1][z + oct7depth][y][x] = oct3[1][z][y][x];
				}
			}
		}
		// Move fourth octant to sixth
		for (z = 0; z < oct4depth; z++) {
			for (y = 0; y < oct4height; y++) {
				for (x = 0; x < oct4width; x++) {
					out[0][z + oct7depth][y][x + oct7width] = oct4[0][z][y][x];
					out[1][z + oct7depth][y][x + oct7width] = oct4[1][z][y][x];
				}
			}
		}
		return out;
	}
    
    private double[][][][] ifftshift(double in[][][][]) {
    	int zDim = in[0].length;
    	int yDim = in[0][0].length;
    	int xDim = in[0][0][0].length;
		double out[][][][] = new double[2][zDim][yDim][xDim];
		int highestxoct1Index;
		int highestyoct1Index;
		int highestzoct1Index;
		int oct1depth;
		int oct1height;
		int oct1width;
		int oct2depth;
		int oct2height;
		int oct2width;
		int oct3depth;
		int oct3height;
		int oct3width;
		int oct4depth;
		int oct4height;
		int oct4width;
		int oct5depth;
		int oct5height;
		int oct5width;
		int oct6depth;
		int oct6height;
		int oct6width;
		int oct7depth;
		int oct7height;
		int oct7width;
		int oct8depth;
		int oct8height;
		int oct8width;
		int z,y,x;
		if ((zDim % 2) == 1) {
			highestzoct1Index = (zDim - 1)/2 - 1;
		}
		else {
			highestzoct1Index = zDim/2 - 1;
		}
		if ((yDim %2) == 1) {
			//yodd = true;
			highestyoct1Index = (yDim - 1)/2 - 1;
		}
		else {
			//yodd = false;
			highestyoct1Index = yDim/2 - 1;
		}
		if ((xDim % 2) == 1) {
			//xodd = true;
			highestxoct1Index = (xDim - 1)/2 - 1;
		}
		else {
			//xodd = false;
			highestxoct1Index = xDim/2 - 1;
		}
		oct1width = highestxoct1Index + 1;
		oct1height = highestyoct1Index + 1;
		oct1depth = highestzoct1Index + 1;
		oct2width = xDim - oct1width;
		oct2height = oct1height;
		oct2depth = oct1depth;
		oct3width = oct2width;
		oct3height = yDim - oct1height;
		oct3depth = oct1depth;
		oct4width = oct1width;
		oct4height = oct3height;
		oct4depth = oct1depth;
		oct5width = oct1width;
		oct5height = oct1height;
		oct5depth = zDim - oct1depth;
		oct6width = oct2width;
		oct6height = oct2height;
		oct6depth = zDim - oct2depth;
		oct7width = oct3width;
		oct7height = oct3height;
		oct7depth = zDim - oct3depth;
		oct8width = oct4width;
		oct8height = oct4height;
		oct8depth = zDim - oct4depth;
		double oct1[][][][] = new double[2][oct1depth][oct1height][oct1width];
		double oct2[][][][] = new double[2][oct2depth][oct2height][oct2width];
		double oct3[][][][] = new double[2][oct3depth][oct3height][oct3width];
		double oct4[][][][] = new double[2][oct4depth][oct4height][oct4width];
		double oct5[][][][] = new double[2][oct5depth][oct5height][oct5width];
		double oct6[][][][] = new double[2][oct6depth][oct6height][oct6width];
		double oct7[][][][] = new double[2][oct7depth][oct7height][oct7width];
		double oct8[][][][] = new double[2][oct8depth][oct8height][oct8width];
		for (z = 0; z <= highestzoct1Index; z++) {
			for (y = 0; y <= highestyoct1Index; y++) {
				for (x = 0; x <= highestxoct1Index; x++) {
					oct1[0][z][y][x] = in[0][z][y][x];
					oct1[1][z][y][x] = in[1][z][y][x];
				}
			}
		}
		for (z = 0; z <= highestzoct1Index; z++) {
			for (y = 0; y <= highestyoct1Index; y++) {
				for (x = highestxoct1Index+1; x < xDim; x++) {
					oct2[0][z][y][x - highestxoct1Index-1] = in[0][z][y][x];
					oct2[1][z][y][x - highestxoct1Index-1] = in[1][z][y][x];
				}
			}
		}
		for (z = 0; z <= highestzoct1Index; z++) {
			for (y = highestyoct1Index+1; y < yDim; y++) {
				for (x = highestxoct1Index+1; x < xDim; x++) {
					oct3[0][z][y - highestyoct1Index - 1][x - highestxoct1Index - 1] = in[0][z][y][x];
					oct3[1][z][y - highestyoct1Index - 1][x - highestxoct1Index - 1] = in[1][z][y][x];
				}
			}
		}
		for (z = 0; z <= highestzoct1Index; z++) {
			for (y = highestyoct1Index+1; y < yDim; y++) {
				for (x = 0; x <= highestxoct1Index; x++) {
					oct4[0][z][y - highestyoct1Index - 1][x] = in[0][z][y][x];
					oct4[1][z][y - highestyoct1Index - 1][x] = in[1][z][y][x];
				}
			}
        }
		for (z = highestzoct1Index+1; z < zDim; z++) {
			for (y = 0; y <= highestyoct1Index; y++) {
				for (x = 0; x <= highestxoct1Index; x++) {
					oct5[0][z - highestzoct1Index - 1][y][x] = in[0][z][y][x];
					oct5[1][z - highestzoct1Index - 1][y][x] = in[1][z][y][x];
				}
			}
		}
		for (z = highestzoct1Index+1; z < zDim; z++) {
			for (y = 0; y <= highestyoct1Index; y++) {
				for (x = highestxoct1Index+1; x < xDim; x++) {
					oct6[0][z - highestzoct1Index - 1][y][x - highestxoct1Index-1] = in[0][z][y][x];
					oct6[1][z - highestzoct1Index - 1][y][x - highestxoct1Index-1] = in[1][z][y][x];
				}
			}
		}
		for (z = highestzoct1Index+1; z < zDim; z++) {
			for (y = highestyoct1Index+1; y < yDim; y++) {
				for (x = highestxoct1Index+1; x < xDim; x++) {
					oct7[0][z - highestzoct1Index - 1][y - highestyoct1Index - 1][x - highestxoct1Index - 1] = in[0][z][y][x];
					oct7[1][z - highestzoct1Index - 1][y - highestyoct1Index - 1][x - highestxoct1Index - 1] = in[1][z][y][x];
				}
			}
		}
		for (z = highestzoct1Index+1; z < zDim; z++) {
			for (y = highestyoct1Index+1; y < yDim; y++) {
				for (x = 0; x <= highestxoct1Index; x++) {
					oct8[0][z - highestzoct1Index - 1][y - highestyoct1Index - 1][x] = in[0][z][y][x];
					oct8[1][z - highestzoct1Index - 1][y - highestyoct1Index - 1][x] = in[1][z][y][x];
				}
			}
        }
		
		// Move seventh octant to first
		for (z = 0; z < oct7depth; z++) {
			for (y = 0; y < oct7height; y++) {
				for (x = 0; x < oct7width; x++) {
					out[0][z][y][x] = oct7[0][z][y][x];
					out[1][z][y][x] = oct7[1][z][y][x];
				}
			}
		}
		// Move eighth octant to second
		for (z = 0; z < oct8depth; z++) {
			for (y = 0; y < oct8height; y++) {
				for (x = 0; x < oct8width; x++) {
					out[0][z][y][x + oct7width] = oct8[0][z][y][x];
					out[1][z][y][x + oct7width] = oct8[1][z][y][x];
				}
			}
		}
		// Move fifth octant to third
		for (z = 0; z < oct5depth; z++) {
			for (y = 0; y < oct5height; y++) {
				for (x = 0; x < oct5width; x++) {
					out[0][z][y+oct7height][x + oct7width] = oct5[0][z][y][x];
					out[1][z][y+oct7height][x + oct7width] = oct5[1][z][y][x];
				}
			}
		}
		// Move six octant to fourth
		for (z = 0; z < oct6depth; z++) {
			for (y = 0; y < oct6height; y++) {
				for (x = 0; x < oct6width; x++) {
					out[0][z][y+oct7height][x] = oct6[0][z][y][x];
					out[1][z][y+oct7height][x] = oct6[1][z][y][x];
				}
			}
		}
		// Move first octant to seventh
		for (z = 0; z < oct1depth; z++) {
			for (y = 0; y < oct1height; y++) {
				for (x = 0; x < oct1width; x++) {
					out[0][z + oct7depth][y + oct7height][x + oct7width] = oct1[0][z][y][x];
					out[1][z + oct7depth][y + oct7height][x + oct7width] = oct1[1][z][y][x];
				}
			}
		}
		// Move second octant to eighth
		for (z = 0; z < oct2depth; z++) {
			for (y = 0; y < oct2height; y++) {
				for (x = 0; x < oct2width; x++) {
					out[0][z + oct7depth][y + oct7height][x] = oct2[0][z][y][x];
					out[1][z + oct7depth][y + oct7height][x] = oct2[1][z][y][x];
				}
			}
		}
		// Move third octant to fifth
		for (z = 0; z < oct3depth; z++) {
			for (y = 0; y < oct3height; y++) {
				for (x = 0; x < oct3width; x++) {
					out[0][z + oct7depth][y][x] = oct3[0][z][y][x];
					out[1][z + oct7depth][y][x] = oct3[1][z][y][x];
				}
			}
		}
		// Move fourth octant to sixth
		for (z = 0; z < oct4depth; z++) {
			for (y = 0; y < oct4height; y++) {
				for (x = 0; x < oct4width; x++) {
					out[0][z + oct7depth][y][x + oct7width] = oct4[0][z][y][x];
					out[1][z + oct7depth][y][x + oct7width] = oct4[1][z][y][x];
				}
			}
		}
		return out;
	}
    
    private double[][][] cfft2(double x[][][]) {
    	// Aliased 2D FFT of the image x.
    	// The FFT is computed using O(n^2logn) operations.
    	
    	// x   The image whose 2D FFT should be computed. Can be of odd or even length.
    	//
    	// Returns the aliased 2D FFT of the image x.
    	 
    	// Yoel Shkolnisky 22/10/01
    	int h, w;
    	int yDim = x[0].length;
    	int xDim = x[0][0].length;
    	double ix[][][] = ifftshift(x);
    	double ixarr[][] = new double[2][yDim*xDim];
    	for (h = 0; h < yDim; h++) {
    		for (w = 0; w < xDim; w++) {
    			ixarr[0][h*xDim + w] = ix[0][h][w];
    			ixarr[1][h*xDim + w] = ix[1][h][w];
    		}
    	}
    	FFTUtility fftx = new FFTUtility(ixarr[0], ixarr[1], yDim, xDim, 1, -1, FFTUtility.FFT);
    	fftx.run();
    	FFTUtility ffty = new FFTUtility(ixarr[0], ixarr[1], 1, yDim, xDim, -1, FFTUtility.FFT);
    	ffty.run();
    	double y[][][] = new double[2][yDim][xDim];
    	for (h = 0; h < yDim; h++) {
    		for (w = 0; w < xDim; w++) {
    			y[0][h][w] = ixarr[0][h*xDim + w];
    			y[1][h][w] = ixarr[1][h*xDim + w];
    		}
    	}
    	return (fftshift(y));
    }
    
    private double[][][] icfft2(double x[][][]) {
    	// Aliased 2D Inverse FFT of the image x.
    	// The FFT is computed using O(n^2logn) operations.
    	
    	// x   The image whose 2D inverse FFT should be computed. Can be of odd or even length.
    	//
    	// Returns the aliased 2D inverse FFT of the image x.
    	 
    	// Yoel Shkolnisky 06/02/02
    	int h, w;
    	int yDim = x[0].length;
    	int xDim = x[0][0].length;
    	double ix[][][] = ifftshift(x);
    	double ixarr[][] = new double[2][yDim*xDim];
    	for (h = 0; h < yDim; h++) {
    		for (w = 0; w < xDim; w++) {
    			ixarr[0][h*xDim + w] = ix[0][h][w];
    			ixarr[1][h*xDim + w] = ix[1][h][w];
    		}
    	}
    	// Inverse FFT
    	FFTUtility fftx = new FFTUtility(ixarr[0], ixarr[1], yDim, xDim, 1, 1, FFTUtility.FFT);
    	fftx.run();
    	FFTUtility ffty = new FFTUtility(ixarr[0], ixarr[1], 1, yDim, xDim, 1, FFTUtility.FFT);
    	ffty.run();
    	double y[][][] = new double[2][yDim][xDim];
    	for (h = 0; h < yDim; h++) {
    		for (w = 0; w < xDim; w++) {
    			y[0][h][w] = ixarr[0][h*xDim + w];
    			y[1][h][w] = ixarr[1][h*xDim + w];
    		}
    	}
    	return (fftshift(y));
    }
    
    private double[][][][] cfft3(double x[][][][]) {
    	// Aliased 3D FFT of the image x.
    	// The FFT is computed using O(n^2logn) operations.
    	
    	// x   The image whose 3D FFT should be computed. Can be of odd or even length.
    	//
    	// Returns the aliased 3D FFT of the image x.
    	 
    	int d, h, w;
    	int zDim = x[0].length;
    	int yDim = x[0][0].length;
    	int xDim = x[0][0][0].length;
    	int length = xDim * yDim;
    	double ix[][][][] = ifftshift(x);
    	double ixarr[][] = new double[2][zDim*length];
    	for (d = 0; d < zDim; d++) {
	    	for (h = 0; h < yDim; h++) {
	    		for (w = 0; w < xDim; w++) {
	    			ixarr[0][d*length + h*xDim + w] = ix[0][d][h][w];
	    			ixarr[1][d*length + h*xDim + w] = ix[1][d][h][w];
	    		}
	    	}
    	}
    	FFTUtility fftx = new FFTUtility(ixarr[0], ixarr[1], yDim*zDim, xDim, 1, -1, FFTUtility.FFT);
    	fftx.run();
    	FFTUtility ffty = new FFTUtility(ixarr[0], ixarr[1], zDim, yDim, xDim, -1, FFTUtility.FFT);
    	ffty.run();
    	FFTUtility fftz = new FFTUtility(ixarr[0], ixarr[1], 1, zDim, length, -1, FFTUtility.FFT);
    	fftz.run();
    	double y[][][][] = new double[2][zDim][yDim][xDim];
    	for (d = 0; d < zDim; d++) {
	    	for (h = 0; h < yDim; h++) {
	    		for (w = 0; w < xDim; w++) {
	    			y[0][d][h][w] = ixarr[0][d*length + h*xDim + w];
	    			y[1][d][h][w] = ixarr[1][d*length + h*xDim + w];
	    		}
	    	}
    	}
    	return (fftshift(y));
    }
    
    private double[][][][] icfft3(double x[][][][]) {
    	// Aliased 3D Inverse FFT of the image x.
    	// The FFT is computed using O(n^2logn) operations.
    	
    	// x   The image whose 3D inverse FFT should be computed. Can be of odd or even length.
    	//
    	// Returns the aliased 3D inverse FFT of the image x.
    	int d, h, w;
    	int zDim = x[0].length;
    	int yDim = x[0][0].length;
    	int xDim = x[0][0][0].length;
    	int length = xDim * yDim;
    	double ix[][][][] = ifftshift(x);
    	double ixarr[][] = new double[2][zDim*length];
    	for (d = 0; d < zDim; d++) {
	    	for (h = 0; h < yDim; h++) {
	    		for (w = 0; w < xDim; w++) {
	    			ixarr[0][d*length + h*xDim + w] = ix[0][d][h][w];
	    			ixarr[1][d*length + h*xDim + w] = ix[1][d][h][w];
	    		}
	    	}
    	}
    	// Inverse FFT
    	FFTUtility fftx = new FFTUtility(ixarr[0], ixarr[1], yDim*zDim, xDim, 1, 1, FFTUtility.FFT);
    	fftx.run();
    	FFTUtility ffty = new FFTUtility(ixarr[0], ixarr[1], zDim, yDim, xDim, 1, FFTUtility.FFT);
    	ffty.run();
    	FFTUtility fftz = new FFTUtility(ixarr[0], ixarr[1], 1, zDim, length, 1, FFTUtility.FFT);
    	fftz.run();
    	double y[][][][] = new double[2][zDim][yDim][xDim];
    	for (d = 0; d < zDim; d++) {
	    	for (h = 0; h < yDim; h++) {
	    		for (w = 0; w < xDim; w++) {
	    			y[0][d][h][w] = ixarr[0][d*length + h*xDim + w];
	    			y[1][d][h][w] = ixarr[1][d*length + h*xDim + w];
	    		}
	    	}
    	}
    	return (fftshift(y));
    }
    
    private double[][] frft(double x[][], double alpha) {
    		
    		// Aliased fractional Fourier transform (FRFT) of the sequence x.
    		// The FRFT is computed directly using O(n^2) operations.
    		
    		// x       The sequence whose FRFT should be computed. Can be of odd or even length.
    		// alpha	  The alpha parameter of the fractional Fourier transform
    		
    		// Returns the aliased FRFT with parameter alpha of the sequence x.
    		// The result is always a row vector.
    		
    		// The fractional Fourier transform y of the sequence x (with parameter alpha) is defined by
    		//                   n/2-1
    		//       y(k) =       sum  x(u)*exp(-2*pi*i*k*u*alpha/N),  -n/2 <= k <= n/2-1, N=length(x).
    		//                   u=-n/2
    		// The value of the fractional Fourier transform (y) for index k (-n/2 <= k <= n/2-1) is stored in 
    		// the array y in index toUnalisedIdx(k,N) (which is between 1 and N).
    		 
    		// Yoel Shkolnisky 22/10/01

    		int m= x[0].length;
    		double y[][] = new double[2][m];

    		for (int k=lowIdx(m); k <= hiIdx(m); k++) {
    		   double acc = 0.0;
    		   double accImag = 0.0;
    		   for (int j=lowIdx(m); j <= hiIdx(m); j++) {
    			  double arg = 2.0*Math.PI*j*k*alpha/m;
    			  double cos = Math.cos(arg);
    			  double sin = Math.sin(arg);
    			  int idx = toUnaliasedIdx(j,m);
    			  acc = acc + x[0][idx]*cos + x[1][idx]*sin;
    			  accImag = accImag -x[0][idx]*sin + x[1][idx]*cos;	  
    		   }
    		   int outputIdx = toUnaliasedIdx(k,m);
    		   y[0][outputIdx] = acc;
    		   y[1][outputIdx] = accImag;
    		}
    		return y;
    }
    
    private double[][] cfrft(double x[][], double alpha) {
    	// Aliased fractional Fourier transform of the sequence x.
    	// The FRFT is computed using O(nlogn) operations.
    	
    	// x       The sequence whose FRFT should be computed. Can be of odd or even
    	//         length. Must be a 1-D row vector.
    	// alpha   The parameter alpha of the fractional Fourier transform.
    	
    	// Returns the aliased FRFT with parameter alpha of the sequence x.
    	// The fractional Fourier transform w of the sequence x (with parameter alpha) is defined by
    	//                   n/2-1
    	//       w(k) =       sum  x(u)*exp(-2*pi*i*k*u*alpha/N),  -n/2 <= k <= n/2-1, N=length(x).
    	//                   u=-n/2
    	
    	 
    	// This function is the same as cfrftV2. It uses the less padding (3m as in the paper)
    	// and therefore it is more memory efficient. This may cause the lengths of the sequences to be non-optimal 
    	// for Matlab's FFT function. The function cfrftV2 uses more memory (for padding) but uses FFTs
    	// of dyadic length.
    	
    	// Yoel Shkolnisky 22/10/01

        int k;
        double arg;
        double cos;
        double sin;
    	int m= x[0].length;
    	int j[] = new int[hiIdx(m) - lowIdx(m) + 1];
    	for (k = lowIdx(m); k <= hiIdx(m); k++) {
    		j[k-lowIdx(m)] = k;
    	}
    	int j2[] = new int[hiIdx(2*m) - lowIdx(2*m) + 1];
    	for (k = lowIdx(2*m); k <= hiIdx(2*m); k++) {
    		j2[k-lowIdx(2*m)] = k;
    	}
    	//E=i*pi*alpha;
    	double y[][] = new double[2][m];
    	for (k = 0; k < m; k++) {
    		arg = Math.PI*alpha*j[k]*j[k]/m;
    		cos = Math.cos(arg);
    		sin = Math.sin(arg);
    		y[0][k] = x[0][k] * cos + x[1][k] * sin;
    		y[1][k] = -x[0][k] * sin + x[1][k] * cos;
    	}

    	double ypad[][] = new double[2][3*m];
    	for (k = 0; k < m; k++) {
    		ypad[0][k+m] = y[0][k];
    		ypad[1][k+m] = y[1][k];
    	}

    	double z[][] = new double[2][3*m];
    	int l=toUnaliasedIdx(-m,3*m);
    	for (k = l; k <= l + j2.length -1; k++) {
    	   arg = Math.PI*alpha*j2[k-l]*j2[k-l]/m;
    	   z[0][k] = Math.cos(arg);
    	   z[1][k] = Math.sin(arg);
    	}

    	double Y[][] =cfft(ypad);
    	double Z[][] =cfft(z);
    	double W[][] = new double[2][3*m];
    	for (k = 0; k < 3*m; k++) {
    		W[0][k] = Y[0][k]*Z[0][k] - Y[1][k]*Z[1][k];
    		W[1][k] = Y[0][k]*Z[1][k] + Y[1][k]*Z[0][k];
    	}
    	double w[][] =icfft(W);
        int lowIndex = toUnaliasedIdx(lowIdx(m),3*m);
        int highIndex = toUnaliasedIdx(hiIdx(m),3*m);
        double wtrunc[][] = new double[2][highIndex - lowIndex + 1];
        for (k = lowIndex; k <= highIndex; k++) {
        	wtrunc[0][k-lowIndex] = w[0][k];
        	wtrunc[1][k-lowIndex] = w[1][k];
        }
        double wprod[][] = new double[2][wtrunc[0].length];
    	for (k = 0; k < j.length; k++) {
    		arg = Math.PI*alpha*j[k]*j[k]/m;
    		cos = Math.cos(arg);
    		sin = Math.sin(arg);
    		wprod[0][k] = wtrunc[0][k]*cos + wtrunc[1][k]*sin;
    		wprod[1][k] = -wtrunc[0][k]*sin + wtrunc[1][k]*cos;
    	}
    	return wprod;
    }
    
    private double log2(double input) {
        return (Math.log10(input) / Math.log10(2.0));
	 }
    
    private double[][] cfrftV2(double x[][], double alpha) {
    	// Aliased fractional Fourier transform of the sequence x.
    	// The FRFT is computed using O(nlogn) operations.
    	
    	// x       The sequence whose FRFT should be computed. Can be of odd or even
    	//         length. Must be a 1-D row vector.
    	// alpha	  The parameter alpha of the fractional Fourier transform
    	
    	// Returns the aliased FRFT with parameter alpha of the sequence x.
    	// The fractional Fourier transform w of the sequence x (with parameter alpha) is defined by
    	//                   n/2-1
    	//       w(k) =       sum  x(u)*exp(-2*pi*i*k*u*alpha/N),  -n/2 <= k <= n/2-1, N=length(x).
    	//                   u=-n/2
    
    	
    	// Yoel Shkolnisky 18/12/02

    	int k;
    	double arg;
    	double cos;
    	double sin;
    	int m= x[0].length;
    	//disp (strcat('FRFT LEN=',int2str(m)));
    	int j[] = new int[hiIdx(m) - lowIdx(m) + 1];
    	for (k = lowIdx(m); k <= hiIdx(m); k++) {
    		j[k-lowIdx(m)] = k;
    	}
    	
    	int j2[] = new int[hiIdx(2*m) - lowIdx(2*m) + 1];
    	for (k = lowIdx(2*m); k <= hiIdx(2*m); k++) {
    		j2[k-lowIdx(2*m)] = k;
    	}
    	//E=i*pi*alpha;

    	int nextpow2 = (int)Math.ceil(log2(3*m));
    	int paddedsize = (int)Math.round(Math.pow(2.0, nextpow2));
    	int leftpad = (paddedsize-m+1)/2;
    	int rightpad = paddedsize-m-leftpad; 
    	double y[][] = new double[2][m];
    	for (k = 0; k < m; k++) {
    		arg = Math.PI*alpha*j[k]*j[k]/m;
    		cos = Math.cos(arg);
    		sin = Math.sin(arg);
    		y[0][k] = x[0][k] * cos + x[1][k] * sin;
    		y[1][k] = -x[0][k] * sin + x[1][k] * cos;
    	}
    	double ypad[][] = new double[2][leftpad + m + rightpad];
    	for (k = 0; k < m; k++) {
    		ypad[0][k+leftpad] = y[0][k];
    		ypad[1][k+leftpad] = y[1][k];
    	}

    	// compute the fractional Fourier transform not by padding to 3*m (as in the paper) but by
    	// padding to the next power of 2. This uses more memory, but since all FFTs use dyadic length
    	// it is much faster

    	double z[][] = new double[2][paddedsize];
    	int l=toUnaliasedIdx(-m,paddedsize);
    	for (k = l; k <= l + j2.length -1; k++) {
     	   arg = Math.PI*alpha*j2[k-l]*j2[k-l]/m;
     	   z[0][k] = Math.cos(arg);
     	   z[1][k] = Math.sin(arg);
     	}

    	double Y[][] =cfft(ypad);
    	double Z[][] =cfft(z);
    	
        double W[][] = new double[2][Y[0].length];
    	for (k = 0; k < Y[0].length; k++) {
    		W[0][k] = Y[0][k]*Z[0][k] - Y[1][k]*Z[1][k];
    		W[1][k] = Y[0][k]*Z[1][k] + Y[1][k]*Z[0][k];
    	}
    	double w[][] =icfft(W);
    	int lowIndex = toUnaliasedIdx(lowIdx(m),paddedsize);
        int highIndex = toUnaliasedIdx(hiIdx(m),paddedsize);
        double wtrunc[][] = new double[2][highIndex - lowIndex + 1];
        for (k = lowIndex; k <= highIndex; k++) {
        	wtrunc[0][k-lowIndex] = w[0][k];
        	wtrunc[1][k-lowIndex] = w[1][k];
        }
        double wprod[][] = new double[2][wtrunc[0].length];
    	for (k = 0; k < j.length; k++) {
    		arg = Math.PI*alpha*j[k]*j[k]/m;
    		cos = Math.cos(arg);
    		sin = Math.sin(arg);
    		wprod[0][k] = wtrunc[0][k]*cos + wtrunc[1][k]*sin;
    		wprod[1][k] = -wtrunc[0][k]*sin + wtrunc[1][k]*cos;
    	}
    	return wprod;
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
    
    private double[][] ChirpZ(double x[][], double A[] ,double W[], int M) {
    		
    		// Chrip Z-transform of the sequence x on the contour defined by
    		// A*W^(-k), where k is a sequence of M indices centered around the origin.
    		// For example, for M=5, the sequence k is -2,-1,0,1,2. For M=4, the sequence 
    		// k is -2,-1,0,1.
    		
    		// The chirp Z-transform is computed using O(nlogn) operations.
    		
    		// x    The sequence whose chirp Z-transform should be computed. Can be of odd or even length.
    		// A    Arbitrary complex number.
    		// W    The complex ratio between two consecutive points on the contour.
    		// M    The length of the output sequence. If not specified, the default value
    		//      is M=length(x);
    		
    		// Returns the chirp Z-transform of the sequence x define by
    		//                n/2-1
    		//          X(Z) = sum x(j)Z^(-j)
    		//                j=-n/2
    		// along the contour Z_k = AW^(-k)     k=-M/2...M/2-1.
    		
    		// Yoel Shkolnisky 6/1/03

            int i;
    		int n= x[0].length;
    		int j[] = new int[hiIdx(n) - lowIdx(n) + 1];
    		for (i = lowIdx(n); i <= hiIdx(n); i++) {
    			j[i - lowIdx(n)] = i;
    		}     
    		int pl = M+n; // the required total length for the convolution.
    		int j2[] = new int[hiIdx(pl) - lowIdx(pl) + 1];
    		for (i = lowIdx(pl); i <= hiIdx(pl); i++) {
    			j2[i-lowIdx(pl)] = i;
    		}

    		// Create the array y of length pl and place the terms of the sequence, defined 
    		// in the paper, in the middle (centered about zero).
    		//x = x(:).'; % ensure that x is a row vector
    		double y[][] = new double[2][pl];
    		int low = toUnaliasedIdx(lowIdx(n),pl);
    		int hi = toUnaliasedIdx(hiIdx(n),pl);
    		double Apow[] = new double[1];
    		double ApowImag[] = new double[1];
    		double Wpow[] = new double[1];
    		double WpowImag[] = new double[1];
    		int ierr[] = new int[1];
    		double xA;
    		double xAImag;
    		for (i = low; i <= hi; i++) {
    			zpow(A[0], A[1], -j[i-low], Apow, ApowImag, ierr);
    			zpow(W[0], W[1], j[i-low]*j[i-low]/2.0, Wpow, WpowImag, ierr);
    			xA = x[0][i-low]*Apow[0] - x[1][i-low]*ApowImag[0];
    			xAImag = x[0][i-low]*ApowImag[0] + x[1][i-low]*Apow[0];
    			y[0][i] = xA*Wpow[0] - xAImag*WpowImag[0];
    			y[1][i] = xA*WpowImag[0] + xAImag*Wpow[0];
    		}

    		// Create the array v
    		double v[][] = new double[2][j2.length];
    		for (i = 0; i < j2.length; i++) {
    			zpow(W[0],W[1],-j2[i]*j2[i]/2.0, Wpow, WpowImag, ierr);
    			v[0][i] = Wpow[0];
    			v[1][i] = WpowImag[0];
    		}

    		// Convolve the arrays y and v
    		double Y[][] =cfft(y);
    		double V[][] =cfft(v);
    		double G[][] = new double[2][Y[0].length];
    		for (i = 0; i < Y[0].length; i++) {
    			G[0][i] = Y[0][i]*V[0][i] - Y[1][i]*V[1][i];
    			G[1][i] = Y[0][i]*V[1][i] + Y[1][i]*V[0][i];
    		}
    		double g[][] =icfft(G);

    		// Extract relevant portion of the array - the portion the corresponds to -n/2<=k<=n/2
    		low = toUnaliasedIdx(lowIdx(M),pl);
    		hi = toUnaliasedIdx(hiIdx(M),pl);
    		double gtrunc[][] = new double[2][hi-low+1];
    		for (i = low; i <= hi; i++) {
    			gtrunc[0][i-low] = g[0][i];
    			gtrunc[1][i-low] = g[1][i];
    		}

    		// Postmultiplication
    		int outIdx[] = new int[hiIdx(M) - lowIdx(M) + 1];
    		for (i = lowIdx(M); i <= hiIdx(M); i++) {
    			outIdx[i-lowIdx(M)] = i;
    		}
    		double gmult[][] = new double[2][gtrunc[0].length];
    		for (i = 0; i < gtrunc[0].length; i++) {
    			zpow(W[0], W[1], outIdx[i]*outIdx[i]/2.0, Wpow, WpowImag, ierr);
    			gmult[0][i] = gtrunc[0][i]*Wpow[0] - gtrunc[1][i]*WpowImag[0];
    			gmult[1][i] = gtrunc[0][i]*WpowImag[0] + gtrunc[1][i]*Wpow[0];
    		}
    		return gmult;
    }
    
    private double[][] slowChirp(double x[][], double A[], double W[], int M) {
    		
    		// Chirp Z-transform of the sequence x on the contour defined by
    		// A*W^(-k), where k is a sequence of M indices centered around the origin.
    		// For example, for M=5, the sequence k is -2,-1,0,1,2. For M=4, the sequence 
    		// k is -2,-1,0,1.
    		// The chirp Z-transform is computed directly using O(n^2) operations.
    		
    		// x    The sequence whose chirp Z-transform should be computed. Can be of odd or even length.
    		// A    Arbitrary complex number.
    		// W    The complex ratio between two consecutive points on the contour.
    		// M    The length of the output sequence. If not specified, the default value 
    		//      is M=length(x);
    		
    		// Returns the chirp Z-transform of the sequence x define by
    		//                n/2-1
    		//          X(Z) = sum  x(j)Z^(-j)
    		//                j=-n/2
    		// along the contour Z_k = AW^(-k)     k=-M/2...M/2-1.
    		//
    		// For example, for x = [1 2 3 4 5], the call
    		//     slowChirp(x,1,exp(-2*pi*i/5),5)
    		// computes the aliased DFT of x.
    		 
    		// Yoel Shkolnisky 6/1/03

    		int n= x[0].length;
    		double g[][] = new double[2][M];
    		double Wpow[] = new double[1];
    		double WpowImag[] = new double[1];
    		int ierr[] = new int[1];
            double Z;
            double ZImag;
            double acc;
            double accImag;
            int idx;
            double Zpow[] = new double[1];
            double ZpowImag[] = new double[1];
    		for (int k=lowIdx(M); k <= hiIdx(M); k++) {
    		   zpow(W[0], W[1], -k, Wpow, WpowImag, ierr);
    		   Z = A[0]*Wpow[0] - A[1]*WpowImag[0];
    		   ZImag = A[0]*WpowImag[0] + A[1]*Wpow[0];
    		   acc = 0.0;
    		   accImag = 0.0;
    		   for (int j=lowIdx(n); j <= hiIdx(n); j++) {
    			  idx = toUnaliasedIdx(j,n);
    			  zpow(Z, ZImag, -j, Zpow, ZpowImag, ierr);
    			  acc = acc + x[0][idx]*Zpow[0] - x[1][idx]*ZpowImag[0];
    			  accImag = accImag + x[0][idx]*ZpowImag[0] + x[1][idx]*Zpow[0];
    		   }
    		   int outIdx = toUnaliasedIdx(k,M);
    		   g[0][outIdx] = acc;
    		   g[1][outIdx] = accImag;
    		}
    		return g;
    }


    
    /*private void CG(double [][][]Y, int flag[], double relres[], int iter[] ,double absres[],
    		// function PtP
    		double[][][]X, double[] params, double ErrTol,int MaxIts,
    		double [][][]guess, boolean verbose, double [][][]RefY) {
    		
    		// Solve the system X=PtP(Y,params) using the conjugate gradient method.
    		// The operator PtP must be hermitian and positive defined.
    		// The firat parameter to PtP must be the variable Y. params can be any list
    		// of comma separated arguments.
    		
    		//  Input parameters:
    		//    PtP       Name of the operator to invert
    		//    X         The transformed matrix. The matrix at the range space of the operator PtP, 
    		//              whose source needs to be found.
    		//    params    Additional parameters to the operator PtP.
    		//    ErrTol    Error tolerance of the CG method. Default 1.e-9.
    		//    MaxIts    Maximum number of iterations. Default 10.
    		//    guess     Initial guess of the solution. Default is X.
    		//    verbose    By default, if more than one output argument is specified, then all output 
    		//              messages are suppressed. Set this flag to any value other than 0 to
    		//              always display output messages.
    		//    RefY      The untransformed matrix Y. Used only for checking absolute error.
    		//              If not specified, absolute error is not computed.
    		
    		//  Output parameters:
    		//    Y         The result matrix of the CG method. This is the estimate of the CG
    		//              method to the solution of the system X=PtP(Y).
    		//    flag      A flag that describes the convergence of the CG method.
    		//              0 CG converged to the desired tolerance ErrTol within MaxIts iterations.
    		//              1 CG did not converge to ErrTol within MaxIts iterations.
    		//    relres    Residual error at the end of the CG method. Computed using max norm.
    		//    iter      The iteration number at which ErrTol was achieved. Relevant only if
    		//              flag=0.
    		//    absres    The absolute error at the end of the CG method. Relevant only if RefY
    		//              was given as parameter. Computed using max norm.
    		
    		// Yoel Shkolnisky 15/12/02

    		// Check the input and initialize flags and default parameters
    	    boolean ref_given;
    	    boolean suppress_output;
    	    double xk[][][];
    		ref_given=true;         // The flags is 1 if the reference untransformed matrix RefY is given and 0 otherwise

    		if (refY == null) {    // RefY not given
    		   ref_given=false;   
    		}

    		// Initialize convergence flag. If the routine will detect that the CG method converged, this flag 
    		// will be set to 0 to represent convergence. By default it assumes that the CG did not converge.
    		flag[0]=1;

    		// Set flag to suppress output if "flag" output is specified.
    		suppress_output=false;
    		if (!verbose) {
    		   suppress_output=true;
    		}

    		// iter holds the iteration in which CG converged to ErrTol.
    		iter[0]=-1;

    		// Initialization
    		xk = guess;
    		// Evaluates the function PtP using xk and params
    		//temp = feval(PtP,xk,params{:});
    		gk = temp-X;
    		pk = -gk;
    		dk = sum(abs(gk(:)).^2);

    		% Conjugate gradient iteration
    		j=2;
    		done = 0;
    		while (j<=MaxIts) & (~done)
    		    perr=sum((abs(pk(:))).^2);
    		    if ref_given % If reference matrix is given compute absolute error
    		        xerr=max(max(abs(RefY-xk)));
    		    end
    		    if (~suppress_output) & (flag)
    		%        fprintf('Iteration %2d:  Residual error=%-2.7e',j-1,perr);
    		        fprintf('Iteration %2d:  Gradient norm=%-2.7e',j-1,perr);
    		        fprintf('\t Residual error=%-2.7e',dk);
    		        if ref_given            
    		            fprintf('\t Absolute error=%-2.7e',xerr);
    		        end
    		        fprintf('\n');
    		    end

    		    if perr<=ErrTol
    		        iter=j-1;  %CG converged at previous iteration
    		        flag=0;
    		        done=1;
    		    end
    		    if perr>ErrTol
    		        hk = feval(PtP,pk,params{:});
    		        tk = dk/dot(pk(:),hk(:));  %line search parameter
    		        xk = xk+tk*pk;       % update approximate solution
    		        gk = gk+tk*hk;       % update gradient
    		        temp = sum(abs(gk(:)).^2);
    		        bk = temp/dk;
    		        dk = temp;
    		        pk = -gk+bk*pk;       %update search direction
    		    end
    		    j=j+1;
    		end

    		relres = perr;
    		if ref_given
    		    absres = xerr;
    		end      

    		Y = xk;
    } // private void CG */
    
    private double[] dirichlet(double t[], int m) {
    		
		// compute the value of the dirichlet kernel of length m at point t
		
		// Yoel Shkolnisky 22/10/01
    	
    	// epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        double epsilon = 1.0;
        double neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)


    		double y[] = new double[t.length];
    		for (int k=0; k < y.length; k++) {
    		    if (Math.abs(t[k])<epsilon) {
    		        y[k]=1;
    		    }
    		    else {
    		        y[k]= Math.sin(Math.PI*t[k])/(m*Math.sin(Math.PI*t[k]/m));
    		    }
    		}
    		return y;
    }
    
    private int[] toUnaliasedCoord(int aCoord[], int N[]) {
    		
    		// Converts indices from the range -n/2...n/2-1 to indices in the range 0...n-1.
    		// Both odd and even values of n are handled. 
    		
    		// The functions accepts a vector of aliased indices (aCoord) and a vector of ranges
    		// (N) from which the indices are taken. It converts each aliased index aCoord(k) into an
    		// unaliased index uCoord(k) in the range 0...N(k)-1. If the vector of ranges
    		// (N) is a scalar, then the function assumes that all indices are taken
    		// from the same range N. This allows calling the function on a vector of
    		// indices:
    		//       toUnaliasedCoord([-1 1 2],5)
    		// instead of
    		//       toUnaliasedCoord([-1 1 2],[5 5 5])
    		
    		// Input:
    		//   aCoord    Vector of aliased indices. Must be 1-D row vector.
    		//   N         Vector that contains the range of each index. Must be 1-D row
    		//             vector or a scalar. If N is scalar it is used for all
    		//             coordinates.
    		// Output:
    		//   uCoord    Vector of unaliased indices.
    		
    		// If N is not a scaler, the vectors aCoord and N must have the same length.
    		
    		// Yoel Shkolnisky 8/1/03

    		
    		if ((N.length != 1) && (aCoord.length != N.length)) {
    		   MipavUtil.displayError ("In toUnaliasedCoord N must be scalar or length of coordinates vector and ranges vector must be the same");
    		   System.exit(0);
    		}

    		// The boolean flag scalar is 1 if the ranges vector N is a scalar.
    		boolean scalar= false;
    		if (N.length==1) {
    		    scalar=true;
    		}


    		int uCoord[] = new int[aCoord.length];
    		for (int k=0; k< aCoord.length; k++) {
    		    if (scalar) {
    		        uCoord[k] = toUnaliasedIdx(aCoord[k],N[0]);
    		    }
    		    else {
    		        uCoord[k] = toUnaliasedIdx(aCoord[k],N[k]);
    		    }
    		}
    		return uCoord;
    }
    
    public boolean verifyImage(ModelImage im) {
	    
	    // Verify input image.
	    
	    // The function verifies that the input image is a 3D image of size nxnxn
	    // with n even. Otherwise the function aborts with an error message.
	    
	    // Yoel Shkolnisky
	
	    // Verify that the input is a 3D image of size nxnxn
	    int extents[] = im.getExtents();
	    if (extents.length != 3) {
	       System.err.println("Input must be a 3D image");
	       return false;
	    }
	
	    if ((extents[0] != extents[1]) || (extents[1] != extents[2])) {
	       System.err.println("Input image must be cube");
	       return false;
	    }
	
	    if ((extents[0] % 2) != 0) {
	       System.err.println("Input image must have even sides");
	       return false;
	    }
	    
	    return true;
    }

    private double[][] adjF(double y[][]) {
    		
    		// Adjoint of the operator F
    		
    		// Parameters:
    		//      y        vector of length n+1 (n even)
    		//      x        vector of length n
    		
    		// Yoel Shkolnisky 30/03/03

    	    int i;	
    	    // Check that the vector y is of length n+1 with n even.
    		int n = y[0].length-1;
    		if ((n % 2) !=0) {
    		    MipavUtil.displayError("In adjF y must be of length n+1 (n even)");
    		    return null;
    		}
    		int m=2*n+1;

    		double y2[][] = new double[2][m];
    		for (i = 0; i < y.length; i++) {
    			y2[0][2*i] = y[0][i];
    			y2[1][2*i] = y[1][i];
    		}
    		double iy2[][] = icfft(y2);
    		double x[][] = new double[2][m];
    		for (i = 0; i < m; i++) {
    			x[0][i] = m*y2[0][i];
    			x[1][i] = m*y2[1][i];
    		}
    	    double xout[][] = new double[2][n];
    	    for (i = n/2; i < 3*n/2; i++) {
    	    	xout[0][i-n/2] = x[0][i];
    	    	xout[1][i-n/2] = x[1][i];
    	    }
    		return xout;
    }

    private double[] chebzeros(int n) {
    		
    		// Compute all zero of the Chebyshev polynomial of order n
    		
    		// Parameters:
    		//   n   Order of the polynomial. n is an integer. n>=0.
    		
    		// Returned value:
    		//   t   Array with the n zeros.
    		
    		// Yoel Shkolnisky 22/09/04

    		if (n<0) {
    		    MipavUtil.displayError(" in chebzeros n should be a positive integer");
    		    return null;
    		}
    		
    		double t[] = new double[n];
    		for (int i = 1; i <= n; i++) {
    		    t[i-1] = -Math.cos((2.0*i-1.0)/n*(Math.PI/2.0));	
    		}
            return t;
    }
    
    private class indexValueComparator implements Comparator<indexValueItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(indexValueItem o1, indexValueItem o2) {
            double a = o1.getValue();
            double b = o2.getValue();
            int i = o1.getIndex();
            int j = o2.getIndex();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else if (i < j) {
            	return -1;
            } else if (i > j) {
            	return 1;
            } else {
                return 0;
            }
        }

    }
	
	private class indexValueItem {
		private int index;
		private double value;
		
		public indexValueItem(int index, double value) {
			this.index = index;
			this.value = value;
		}
		
		public int getIndex() {
			return index;
		}
		
		public double getValue() {
			return value;
		}
		
		
	}
    
    private double[][] fastfmmresample(double f[][], double yin[], double xin[],
    		double dj[] ,double cl[], double EPS) {
    		
    		// Resample a trigonometric polynomial from the points y to the points x.
    		
    		// Let f(w) be the trigonometric polynomial given by
    		//
    		//       n/2-1
    		// T(w) = sum a(k)*exp(i*k*w).
    		//       k=-n/2
    		// The array f is the sample of T(w) at the points y. The function computes
    		// the values of T(w) at the points x.
    		
    		// Input parameters:
    		//   f      Values of T(x) at the points y.
    		//   y      Points where f is given.
    		//   x      Points where to resample T(w).
    		//   cl,dj  Precomputed interpolation constants.
    	    //   EPS has has a default of 1.0E-8
    		
    		// Output parameters:
    		//   g     Values of T(w) at the points x.
    		
    		// The arrays f,x,y should be of the same even length.
    		
    		// Yoel Shkolnisky  03/10/2004

    		int i;
    	    int n=f[0].length;
    		
    		if ((dj == null) || (dj.length == 0)) {
    			System.err.println("In fastmmresample interpolation constants dj must be supplied");
    			return null;
    		}
    		
    		if ((cl == null) || (cl.length == 0)) {
    			System.err.println("In fastmmresample interpolation constants cl must be supplied");
    			return null;
    		}

    		if (xin.length != n) {
    			System.err.println("In fastmmresample f.length = " + n + ", but xin.length = " + xin.length);
    			return null;
    		}
    		
    		if (yin.length != n) {
    			System.err.println("In fastmmresample f.length = " + n + ", but yin.length = " + yin.length);
    			return null;
    		}

    		if ((n % 2)==1) {
    		    System.err.println("In fastmmresample the length n must be even");
    		    return null;
    		}
    		
    		double x[] = new double[xin.length];
    		for (i = 0; i < xin.length; i++) {
    			x[i] = xin[i];
    		}
    		double y[] = new double[yin.length];
    		for (i = 0; i < yin.length; i++) {
    			y[i] = yin[i];
    		}

    		// sort x and then sort everything to the same order as x.
    		ArrayList <indexValueItem> indexValueList = new ArrayList<indexValueItem>();
    		for (i = 0; i < n; i++) {
    			indexValueList.add(new indexValueItem(i, x[i]));
    		}
    		Collections.sort(indexValueList, new indexValueComparator());
    		int srtXidx[] = new int[n];
    		for (i = 0; i < n; i++) {
    	    	indexValueItem item = indexValueList.get(i);
    	    	x[i] = item.getValue();
    	    	srtXidx[i] = item.getIndex();
    		}
    		indexValueList.clear();
    		for (i = 0; i < n; i++) {
    			indexValueList.add(new indexValueItem(i, y[i]));
    		}
    		Collections.sort(indexValueList, new indexValueComparator());
    		int srtYidx[] = new int[n];
    		for (i = 0; i < n; i++) {
    	    	indexValueItem item = indexValueList.get(i);
    	    	y[i] = item.getValue();
    	    	srtYidx[i] = item.getIndex();
    		}
    		double fsrt[][] = new double[2][n];
    		for (i = 0; i < n; i++) {
    			fsrt[0][i] = f[0][srtYidx[i]];
    			fsrt[1][i] = f[1][srtYidx[i]];
    		}

    		// remove x points that are equal to some y point
    		int idxX=0;
    		int idxY=0;
    		int idxDroppedElems[] = new int[n]; // the indices of the x that appear in y
    		int idxEqualTo[] = new int[n]; // the index in the y array of the dropped elements
    		for (i = 0; i < n; i++) {
    			idxDroppedElems[i] = -1;
    			idxEqualTo[i] = -1;
    		}

    		int j=0;
    		while ((idxX<n) && (idxY<n)) {
    		    if (Math.abs(x[idxX]-y[idxY])<1.0d-15) {
    		        idxDroppedElems[j]=idxX;
    		        idxEqualTo[j]=idxY;
    		        j=j+1;
    		        idxX=idxX+1;
    		    }
    		    else if (x[idxX]<y[idxY]) {
    		        idxX=idxX+1;
    		    }
    		    else {
    		        idxY=idxY+1;
    		    }
    		 }

    		// idxUsed is the set of indices from x which we need to
    		// compute. In other words, these are the elements that were not dropped in
    		// the previous iteration.
    		int m = 0;
    		int k = 0;
    		boolean found;
    		int idxUsed[] = new int[n];
    		for (i = 0; i < n; i++) {
    			found = false;
    			for (k = 0; k < n; k++) {
    				if (i == idxDroppedElems[k]) {
    					found = true;
    				}
    			}
    			if (!found) {
    				idxUsed[m++] = i;
    			}
    		}
    		double xused[] = new double[m];
    		for (i = 0; i < m; i++) {
    			xused[i] = x[idxUsed[i]];
    		}
    		int lendropped=j; // number of dropped elements from x
    		double clused[] = new double[m];
    		for (i = 0; i < m; i++) {
    			clused[i] = cl[idxUsed[i]];
    		}

    		// Now we can resample the polynomial using the modified vector x

    		double LARGE=1.0E15;
    		double g[][] = new double[2][n];

    		// Resample the polynomial
    		double gg[][] = new double[2][m];
    		if (xused.length>0) {
    			double fd[][] = new double[2][n];
    			double sfd = 0.0;
    			double sfdImag = 0.0;
    			for (i = 0; i < n; i++) {
    				fd[0][i] = fsrt[0][i]*dj[i];
    				fd[1][i] = fsrt[1][i]*dj[i];
    				sfd += fd[0][i];
    				sfdImag += fd[1][i];
    			}
    			double ydiv2[] = new double[n];
    			for (i = 0; i < n; i++) {
    				ydiv2[i] = y[i]/2.0;
    			}
    			double xdiv2[] = new double[m];
    			for (i = 0; i < m; i++) {
    				xdiv2[i] = xused[i]/2.0;
    			}
    			double b[][] =optimizedtansum(fd,ydiv2,xdiv2,EPS);
    			for (k = 0; k < m; k++) {
    				b[0][k]=b[0][k]+sfdImag;
    		        b[1][k]=b[1][k]-sfd;
    			}
    			for (i = 0; i < m; i++) {
    			    gg[0][i] = clused[i]*b[0][i];
    			    gg[1][i] = clused[i]*b[1][i];
    			}
    		}

    		// For each dropped x, the value of the polynomial is simply the
    		// corresponding value from f. 
    		// Create the output array by appending the computed values to the relevant
    		// values from f.
    		for (i = 0; i < m; i++) {
    			g[0][idxUsed[i]] = gg[0][i];
    			g[1][idxUsed[i]] = gg[1][i];
    		}
    		for (k=0; k < lendropped; k++) {
    		    g[0][idxDroppedElems[k]] = fsrt[0][idxEqualTo[k]];
    		    g[1][idxDroppedElems[k]] = fsrt[1][idxEqualTo[k]];
    		}

    		// reorder the output array to the original order.
    		double greorder[][] = new double[2][n];
    		for (i = 0; i < n; i++) {
    		    greorder[0][i]=g[0][srtXidx[i]];
    		    greorder[1][i]=g[1][srtXidx[i]];
    		}
    		return greorder;
    }
    
    private double[][] optimizedtansum(double alphain[][], double xin[], double yin[], double eps) {
    		
    		// An optimized version of the function tansum. See tansum for more details.
    		
    		// Input parameters:
    		//    alphain   Weights in the sum above
    		//    xin       Points at which the weights are given.
    		//    yin       Points where to evaluate the sum v.
    		//    eps     Required accuracy. Default value 1.0e-8.
    		//
    		// Returned value:
    		//    v       An array of the same length as y where v(k) is the value of
    		//            the sum above at the point y(k).
    		//
    		// Yoel Shkolnisky 21/09/04

    		int i;
    	    if (alphain[0].length != xin.length) {
    		    System.err.println("In optimizedtansum arrays alphain and xin must have the same length");
    		    System.exit(0);
    		}
    	    
    	    double alpha[][] = new double[2][alphain[0].length];
    	    for (i = 0; i < alphain[0].length; i++) {
    	    	alpha[0][i] = alphain[0][i];
    	    	alpha[1][i] = alphain[1][i];
    	    }
    	    double x[] = new double[xin.length];
    	    for (i = 0; i < xin.length; i++) {
    	    	x[i] = xin[i];
    	    }
    	    double y[] = new double[yin.length];
    	    for (i = 0; i < yin.length; i++) {
    	    	y[i] = yin[i];
    	    }

    		// Find the smallest interval the contains all points x and y
    		double lowpoint = Double.MAX_VALUE;
    		double highpoint = -Double.MAX_VALUE;
    		for (i = 0; i < x.length; i++) {
    			if (x[i] < lowpoint) {
    				lowpoint = x[i];
    			}
    			if (x[i] > highpoint) {
    				highpoint = x[i];
    			}
    		}
    		for (i = 0; i < y.length; i++) {
    			if (y[i] < lowpoint) {
    				lowpoint = y[i];
    			}
    			if (y[i] > highpoint) {
    				highpoint = y[i];
    			}
    		}

    		// Sort the arrays alpha and x
    		ArrayList <indexValueItem> indexValueList = new ArrayList<indexValueItem>();
    		for (i = 0; i < x.length; i++) {
    			indexValueList.add(new indexValueItem(i, x[i]));
    		}
    		Collections.sort(indexValueList, new indexValueComparator());
    		int sortidx[] = new int[x.length];
    		for (i = 0; i < x.length; i++) {
    	    	indexValueItem item = indexValueList.get(i);
    	    	x[i] = item.getValue();
    	    	sortidx[i] = item.getIndex();
    		}
    		for (i = 0; i < x.length; i++) {
    		    alpha[0][i]=alpha[0][sortidx[i]];
    		    alpha[1][i]=alpha[1][sortidx[i]];
    		}

    		// Set sizes and legnths
    		int K=y.length;
    		int N= x.length;
    		int depth=(int)Math.floor(log2(N)/512);                        // Number of levels in the algorithm
    		depth=Math.max(depth,1);
    		double v[][] = new double[2][K];
    		double rmin=(highpoint-lowpoint)/Math.pow(2,(depth+1)); // The smallest neigborhood processsed in the algorithm
    		double sin = Math.sin(rmin);
    		int nmax=(int)Math.ceil(Math.log(6/(eps*sin*sin))/Math.log(5)); // Largest number of coeffcients used in any level of the algorithm
    		int n=nmax; // all levels use the same value of n since n does not varies very much
    		double t[] =chebzeros(n);
    		double tmp[][] = new double[2][n];

    		// Precompute once the coefficients used by the polynomial u
    		double ucoefs[][]= new double[n-1][n];
    		/*for k=1:n-1
    		    ucoefs(k,:)=(cos(k*acos(t(:)))).';
    		end

    		for l=2:depth
    		    % Compute the interpolation coefficients f

    		    r=(highpoint-lowpoint)/2^(l+1);   %Radius of the interval processed in the current level.
    		    coefs=zeros(2^l,nmax);
    		    
    		    xpointidx=1;
    		    for i=1:2^l 
    		        % Center of the current interval. The processed interval is
    		        % [c-r,c+r].
    		        c=2*r*(i-1)+lowpoint+r;
    		           
    		        % Create the far-field expansion coefficients for the current
    		        % inteval.
    		        intervalleft=lowpoint+2*r*(i-1);           %left point of the current interval
    		        intervalright=intervalleft+2*r;  %right point of the current interval
    		        
    		        while (xpointidx<=N) & (x(xpointidx)<=intervalright),
    		            trueidx=sortidx(xpointidx);
    		            alphak=alpha(trueidx);
    		            xk=x(trueidx);            
    		            coefs(i,1:n)=coefs(i,1:n)+alphak.*(t+3*tan(r)*tan(xk-c))./(3*tan(r)-t*tan(xk-c));           
    		            xpointidx=xpointidx+1;
    		        end
    		    end
    		    
    		    % For each point y, sum its iteraction with far intervals that were
    		    % not processed in previous iterations. For each interval i,these
    		    % intervals are  exactly intervals i-2 and i+2 at the current level
    		    % (referred to as interaction list at the original paper)
    		    
    		    for k=1:K
    		        % Find the interval of yk
    		        interval=floor((y(k)-lowpoint)/(2*r))+1;
    		        if ((interval-2)>=1)            
    		            c=2*r*(interval-3)+lowpoint+r; % center of interval i-2
    		            p=3*tan(r)/(tan(y(k)-c));
    		            % Compute the polynomial u inline (no procedure call) for optimization
    		            b=cos([1:n-1]*acos(p));
    		            tmp=ucoefs(1,:).*b(1);
    		            for m=2:n-1
    		                tmp=tmp+ucoefs(m,:).*b(m);
    		            end
    		            tmp=2.*tmp/n;
    		            tmp=tmp+1/n;
    		            
    		            v(k)=v(k)+sum(coefs(interval-2,1:n).*tmp);
    		        end
    		        if ((interval+2)<=2^l)
    		            c=2*r*(interval+1)+lowpoint+r; % center of interval i+2
    		            p=3*tan(r)/(tan(y(k)-c));
    		            % Compute the polynomial u inline (no procedure call) for optimization
    		            b=cos([1:n-1]*acos(p));
    		            tmp=ucoefs(1,:).*b(1);
    		            for m=2:n-1
    		                tmp=tmp+ucoefs(m,:).*b(m);
    		            end
    		            tmp=2.*tmp/n;
    		            tmp=tmp+1/n;
    		            
    		            v(k)=v(k)+sum(coefs(interval+2,1:n).*tmp);
    		        end
    		        
    		        % Check if we should process also intervals i-3 and i+3
    		        if ((interval-3)>=1) & (floor((interval-1)/2)-floor((interval-4)/2)==1)
    		            c=2*r*(interval-4)+lowpoint+r; % center of interval i-3
    		            p=3*tan(r)/(tan(y(k)-c));
    		            % Compute the polynomial u inline (no procedure call) for optimization
    		            b=cos([1:n-1]*acos(p));
    		            tmp=ucoefs(1,:).*b(1);
    		            for m=2:n-1
    		                tmp=tmp+ucoefs(m,:).*b(m);
    		            end
    		            tmp=2.*tmp/n;
    		            tmp=tmp+1/n;       
    		            
    		            v(k)=v(k)+sum(coefs(interval-3,1:n).*tmp);
    		        end

    		        if ((interval+3)<=2^l) & (floor((interval+2)/2)-floor((interval-1)/2)==1)
    		            c=2*r*(interval+2)+lowpoint+r; % center of interval i+3
    		            p=3*tan(r)/(tan(y(k)-c));            
    		            % Compute the polynomial u inline (no procedure call) for optimization
    		            b=cos([1:n-1]*acos(p));
    		            tmp=ucoefs(1,:).*b(1);
    		            for m=2:n-1
    		                tmp=tmp+ucoefs(m,:).*b(m);
    		            end
    		            tmp=2.*tmp/n;
    		            tmp=tmp+1/n;   

    		            v(k)=v(k)+sum(coefs(interval+3,1:n).*tmp);
    		        end      
    		    end
    		end

    		% Compute which points belong to which interval at the finest level.
    		% The array finest partition contains for for each of the 2^depth intervals at the finest 
    		% level the indices of all the x points in that interval.
    		%
    		% finestpartition is a 2D array that contains for each interval i
    		% (i=1..2^depth) the indices of all points x that are contained in this
    		% interval.

    		finestpartition = zeros(2^depth,N);
    		r=(highpoint-lowpoint)/2^(depth+1);
    		xpointidx=1;

    		for i=1:2^depth
    		    % Center of the current interval. The processed interval is
    		    % [c-r,c+r].
    		    c=2*r*(i-1)+lowpoint+r;
    		    listidx=1; %current position in the array the corresponds to the current interval.
    		           
    		    intervalleft=lowpoint+2*r*(i-1);           %left point of the current interval
    		    intervalright=intervalleft+2*r;  %right point of the current interval
    		        
    		    % The 1.0e-15 compansates for tiny numerical errors
    		    % XXX: remove the +1.0e-15, run test13 for n=4 (it won't work) and find
    		    % another fix for the bug. 
    		%  while (xpointidx<=N) & (x(xpointidx)<=intervalright+1.0e-15),

    		   while (xpointidx<=N) & (x(xpointidx)<=intervalright),
    		        finestpartition(i,listidx)=sortidx(xpointidx);
    		        listidx=listidx+1;
    		        xpointidx=xpointidx+1;
    		    end
    		end

    		% Process the last point, which may be unprocessed in the above loop due to
    		% tiny numerical errors
    		if (xpointidx==N)
    		    finestpartition(i,listidx)=sortidx(xpointidx);
    		end
    		    

    		% At the finsest level, compute for each point the interaction with the
    		% neasrest intervals directly.

    		for k=1:K
    		    % Find the interval of yk
    			interval=floor((y(k)-lowpoint)/(2*r))+1;
    		    if interval>2^depth %should happen only due to tiny numerical errors
    		        interval=2^depth;
    		    end
    		    
    		    % Compute the interaction with the previous interval
    			if ((interval-1)>=1)
    		        j=1;
    		        while (j<=N) & (finestpartition(interval-1,j)~=0)
    		            idx=finestpartition(interval-1,j);
    		            v(k)=v(k)+alpha(idx)/tan(y(k)-x(idx));
    		            j=j+1;
    		        end
    			end

    		    %Compute the interaction with the current interval
    		    j=1;
    		    while (j<=N) & (finestpartition(interval,j)~=0)
    		        idx=finestpartition(interval,j);
    		        v(k)=v(k)+alpha(idx)/tan(y(k)-x(idx));
    		        j=j+1;
    		    end
    		    
    		    % Compute the interaction with the next interval
    		    if ((interval+1)<=2^depth)
    		        j=1;
    		        while (j<=N) & (finestpartition(interval+1,j)~=0)
    		            idx=finestpartition(interval+1,j);
    		            v(k)=v(k)+alpha(idx)/tan(y(k)-x(idx));
    		            j=j+1;
    		        end
    		    end
    		end*/
    		return v;
    }

    		/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    		% THIS FUNCTION IS NOT USED - all calls were embeded inline.
    		function cousins=checkcousins(interval1,interval2,l)
    		%
    		% Check if the parents of interval1 and interval 2 are neighbors in level 
    		% l-1. Both interval1 and interval2 are from level l. Hence interval1 and
    		% interval2 must be in the range 1..2^l.

    		% This calls are safer but cost a lot time
    		%if (interval1<1) | (interval1>2^l) 
    		%    error('interval1 must be in the range 1...2^l');
    		%end

    		% This calls are safer but cost a lot time
    		%if (interval2<1) | (interval2>2^l) 
    		%    error('interval2 must be in the range 1...2^l');
    		%end

    		% The function assumes that l>0 since l==0 should never be called by the
    		% calling function.
    		%

    		cousins=0;
    		parentint1=floor((interval1-1)/2)+1;
    		parentint2=floor((interval2-1)/2)+1;
    		if (abs(parentint1-parentint2)==1) 
    		    cousins=1;
    		end

    		% THIS FUNCTION IS NOT USED - all calls were embeded inline.
    		function y=u(n,x,ucoefs)
    		%
    		% Compute the polynomial u_{j,n}(x) defined in "Fast Fourier Transform for
    		% Non-equispaced data II" (Dutt and Rokhlin), ACHA 2, 85-100,1995. The
    		% function is defined by Eq. 52. u_{j,n}(x) is the j'th Lagrange interpolation
    		% polynomial at Chebyshev zeros t_{1}...t_{n}. The function computes
    		% u_{j,n}(x) for j=1:n. ucoefs are the precomputed coefficients use by all
    		% calls to this function
    		%
    		% Input parameters:
    		%   n       Number of interpolation points.
    		%   x       Point to evaluate the polynomials.
    		%   ucoefs  Precomputed coefficients used to compute u.
    		%
    		% Output:
    		%   y     Value of u_{j,n}(x) for j=1:n.
    		%

    		y=zeros(1,n);
    		b=cos([1:n-1]*acos(x));
    		for k=1:n-1
    		    y=y+ucoefs(k,:).*b(k);
    		end
    		y=2.*y/n;
    		y=y+1/n;*/


    
}