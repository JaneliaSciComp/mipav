package gov.nih.mipav.model.algorithms.filters;


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



    
}