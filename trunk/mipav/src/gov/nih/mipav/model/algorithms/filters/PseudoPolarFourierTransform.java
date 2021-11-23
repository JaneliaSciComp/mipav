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
	
	/**
     * Starts the program.
     */
    public void runAlgorithm() {
    	
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
    
    /*private double[][][] cfft2(double x[][][]) {
    	// Aliased 2D FFT of the image x.
    	// The FFT is computed using O(n^2logn) operations.
    	
    	// x   The image whose 2D FFT should be computed. Can be of odd or even length.
    	//
    	// Returns the aliased 2D FFT of the image x.
    	 
    	// Yoel Shkolnisky 22/10/01

    		y = fftshift(fft2(ifftshift(x)));	
    }*/
}