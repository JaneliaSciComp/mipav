package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.RandomNumberGen;
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

    
}