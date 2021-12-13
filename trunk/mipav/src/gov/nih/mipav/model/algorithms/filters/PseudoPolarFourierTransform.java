package gov.nih.mipav.model.algorithms.filters;


import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

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
 * This is a port of MATLAB code in the ppft2 download package by Yoel Shkolnisky.
 * The Magic Square code used in testing is derived from the code in the article 
 * Magic Square by A. Riazi.  The article states that the article has no explicit
 * license attached to it.
 * 
 * References:
 * 1.) Pseudo-Polar and Discrete Radon Transforms Software Package Documentation by Yoel Shkolnisky
       February 19, 2003.
   2.) DIRECT INVERSION OF THE THREE-DIMENSIONAL PSEUDO-POLAR FOURIER TRANSFORM
      AMIR AVERBUCH, GIL SHABAT, AND YOEL SHKOLNISKY SIAM J. SCI. COMPUT. 
      2016 Society for Industrial and Applied Mathematics
      Vol. 38, No. 2, pp. A1100-A1120
   3.) A FRAMEWORK FOR DISCRETE INTEGRAL TRANSFORMATIONS I - THE PSEUDO-POLAR FOURIER TRANSFORM 
       A. AVERBUCH, R.R. COIFMAN, D.L. DONOHO, M. ISRAELI, AND Y. SHKOLNISKY
   4.) Y. Keller, Y. Shkolnisky, and A. Averbuch. Volume registration using the 3-D pseudo-polar
       Fourier transform. IEEE Transactions on Signal Processing, 54(11):4323-4331, 2006. 
 *
 */

public class PseudoPolarFourierTransform extends AlgorithmBase {
	
	private ViewUserInterface UI;
	
	public PseudoPolarFourierTransform() {
		UI = ViewUserInterface.getReference();	
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
    
    public void testippft() {
    	// Test passes Maximum error = 6.550393932985149E-11
    
	    // Test the function ippt.
	    // The function generates a matrix, computes its pseud-polar Fourier
	    // transform, inverts it, and compares the results.
	    
	    // The output of the function should be very close to zero.
	    
	    // Yoel Shkolnisky 11/10/04
	
	    double EPS=1.0e-10;
	    int n=128;
	    int imint[][] = MagicSquare(128);
	    double im[][][] = new double[2][128][128];
	    int i,j;
	    for (i = 0; i < 128; i++) {
	    	for (j = 0; j < 128; j++) {
	    		im[0][i][j] = (double)imint[i][j];
	    	}
	    }
	    double pp1[][][] = new double[2][257][129];
	    double pp2[][][] = new double[2][257][129];
	    PPFT(pp1,pp2,im);
	    double Y[][][] = new double[2][128][128];
	    int flag[] = new int[1];
	    double residual[] = new double[1];
	    int iter[] = new int[1];
	    int maxIts = 100;
	    boolean verbose = false;
	    ippft(Y, flag, residual, iter,pp1,pp2,EPS, maxIts, verbose);
	    double maxDiff = 0.0;
	    double diff;
	    int ival = 0;
	    int jval = 0;
	    for (i = 0; i < 128; i++) {
	    	for (j = 0; j < 128; j++) {
	    		diff = Math.abs(Y[0][i][j] - im[0][i][j]);
	    		if (diff > maxDiff) {
	    			maxDiff = diff;
	    			ival = i;
	    			jval = j;
	    		}
	    	}
	    }
	    double error = maxDiff/Math.abs(im[0][ival][jval]);
	    System.out.println("Maximum error = " + error);
    }
    
    private int[][] MagicSquare(int n) {
        if (n < 3) {
        	System.err.println("In MagicSquare n must be >= 3");
        	return null;
        }
        int matrix[][] = new int[n][n];
        if ((n % 2) == 1) { // n is odd
        	OddMagicSquare(matrix, n);
        }
        else if ((n % 4) == 0) { // Doubly even order
        	DoublyEvenMagicSquare(matrix, n);
        }
        else { // Singly even order
        	SinglyEvenMagicSquare(matrix, n);
        }
        return matrix;
    }
    
    private void OddMagicSquare(int matrix[][], int n)
    {
      int nsqr = n * n;
      int i=0, j=n/2;     // start position

      for (int k=1; k<=nsqr; ++k) 
      {
        matrix[i][j] = k;

        i--;
        j++;

        if (k%n == 0) 
        { 
            i += 2; 
            --j; 
          }
          else 
          {
            if (j==n) 
              j -= n;
            else if (i<0) 
              i += n;
          }
        }
      }
    
    private void DoublyEvenMagicSquare(int matrix[][], int n)
    {
      int I[][] = new int[n][n];
      int J[][] = new int[n][n];

      int i, j;

      //prepare I, J
      int index=1;
      for (i=0; i<n; i++)
        for (j=0; j<n; j++)
        {
          I[i][j]=((i+1)%4)/2;
          J[j][i]=((i+1)%4)/2;
          matrix[i][j]=index;
          index++;
        }

      for (i=0; i<n; i++)
        for (j=0; j<n; j++)
        {
          if (I[i][j]==J[i][j])
            matrix[i][j]=n*n+1-matrix[i][j];
        }
    }


    private void SinglyEvenMagicSquare(int matrix[][], int n)
    {
      int p=n/2;
      
      int M[][] = MagicSquare(p);
      
      int i, j, k;

      for (i=0; i<p; i++)
        for (j=0; j<p; j++)
        {
          matrix[i][j]=M[i][j];
          matrix[i+p][j]=M[i][j]+3*p*p;
          matrix[i][j+p]=M[i][j]+2*p*p;
          matrix[i+p][j+p]=M[i][j]+p*p;
        }

      if (n==2)
        return;  

      int I[] = new int[p];
      Vector<Integer> J = new Vector<Integer>();

      for (i=0; i<p; i++)
        I[i]=i+1;

      k=(n-2)/4;
      
      for (i=1; i<=k; i++)
        J.add(i);

      for (i=n-k+2; i<=n; i++)
        J.add(i);

      int temp;
      for (i=1; i<=p; i++)
        for (j=1; j<=J.size(); j++)
        {
          temp=matrix[i-1][J.get(j-1)-1];
          matrix[i-1][J.get(j-1)-1]=matrix[i+p-1][J.get(j-1)-1];
          matrix[i+p-1][J.get(j-1)-1]=temp;
        }

      //j=1, i
      //i=k+1, k+1+p
      i=k; 
      j=0;
      temp=matrix[i][j]; matrix[i][j]=matrix[i+p][j]; matrix[i+p][j]=temp;

      j=i;
      temp=matrix[i+p][j]; matrix[i+p][j]=matrix[i][j]; matrix[i][j]=temp;
    }



    
    public void testOptimizedAdjPPFT() {
	    // Test passes
	    // Tests the function OptimizedAdjPPFT.
	    // Check if <optimizedPPFT(A),B> = <A,optimizedAdjPPFT(B)> for various A and B. 
	    // <A,B> stands for the inner-product of A and B.
	    
	    // Legend for results format:
	    //   n      - matrix size
	    //   a      - <optimizedPPFT(A),B>
	    //   b      - <A,optimizedAdjPPFT(B)>
	    //   error  - absolute error a-b
	    //   Rel a  - Relative error (a-b)/a
	    //   Rel b  - Relative error (a-b)/b
	    //
	    // Yoel Shkolnisky 22/10/01
    	int i,k,m,n;
    	double A[][][];
    	double B1[][][];
    	double B2[][][];
    	double r1[][][];
    	double r2[][][];
    	double r[][][];
    	double B[][][];
    	double a[];
    	double b[];
    	int arr[] = new int[] {4,8,16,32,64}; // 128,256,512
    	RandomNumberGen randomGen = new RandomNumberGen();
    	for (i = 0; i < arr.length; i++) {
	        k = arr[i];
	        A = new double[2][k][k];
	        for (m = 0; m < k; m++) {
	        	for (n = 0; n < k; n++) {
	        		A[0][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
	        		A[1][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
	        	}
	        }
	        B1 = new double[2][2*k+1][k+1];
	        B2 = new double[2][2*k+1][k+1];
	        for (m = 0; m < 2*k+1; m++) {
	        	for (n = 0; n < k+1; n++) {
	        		B1[0][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
	        		B1[1][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
	        		B2[0][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
	        		B2[1][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
	        	}
	        }
	        r1 = new double[2][2*k+1][k+1];
	        r2 = new double[2][2*k+1][k+1];
	        OptimizedPPFT(r1,r2,A);
	        //PPFT(r1,r2,A[0]);
	        r = new double[2][2*k+1][2*(k+1)];
	        B = new double[2][2*k+1][2*(k+1)];
	        for (m = 0; m < 2*k+1; m++) {
	        	for (n = 0; n < k+1; n++) {
	        		r[0][m][n] = r1[0][m][n];
	        		r[1][m][n] = r1[1][m][n];
	        		r[0][m][n+k+1] = r2[0][m][n];
	        		r[1][m][n+k+1] = r2[1][m][n];
	        		B[0][m][n] = B1[0][m][n];
	        		B[1][m][n] = B1[1][m][n];
	        		B[0][m][n+k+1] = B2[0][m][n];
	        		B[1][m][n+k+1] = B2[1][m][n];
	        	}
	        }
	        a = ip(r,B);
	        b = ip(A,OptimizedAdjPPFT(B1,B2));
	        //b = ip(A,adjPPFT(B1,B2));
	
	        System.out.println("k = " + k);
	        System.out.println("a = " + a[0] + " " + a[1]+"i");
	        System.out.println("b = " + b[0] + " " + b[1]+"i");
	        double error[] = new double[] {a[0]-b[0], a[1]-b[1]};
	        System.out.println("Error = " + error[0] + " "+error[1]+"i");
	        double abserr = zabs(error[0],error[1]);
	        double rela = abserr/zabs(a[0],a[1]);
	        System.out.println("Rel a = " + rela);
	        double relb = abserr/zabs(b[0],b[1]);
	        System.out.println("Rel b = " + relb);
	        System.out.println("*********************\n");
    	}
    }
    
    public void testAdj() {
	    // Test passes
	    // Tests the functions adjPPFT and adjRadon.
	    // Check if <PPFT(A),B> = <A,adjPPFT(B)> for various A and B. 
	    // <A,B> stands for the inner-product of A and B.
	    
	    // Legend for results format:
	    //   n      - matrix size
	    //   a      - <PPFT(A),B>
	    //   b      - <A,adjPPFT(B)>
	    //   error  - absolute error a-b
	    //   Rel a  - Relative error (a-b)/a
	    //   Rel b  - Relative error (a-b)/b
	    
	    // See also PPFT, adjPPFT, PsuedoRadon, adjRadon.
	    
	    // Yoel Shkolnisky 9/2/02
	
	    int l;
	    int i,k,m,n;
    	double A[][][];
    	double B1[][][];
    	double B2[][][];
    	double r1[][][];
    	double r2[][][];
    	double r[][][];
    	double B[][][];
    	double a[];
    	double b[];
    	int arr[] = new int[] {4,8,16,32,64}; // 128,256,512
    	RandomNumberGen randomGen = new RandomNumberGen();
    	for (l = 1; l <= 2; l++) {
	       if (l==1) {
	          System.out.println("******** START Test of adjPPFT **********");
	       }
	       else {     
	          System.out.println("******** START Test of adjRadon **********");
	       }
	       
	       for (i = 0; i < arr.length; i++) {
		        k = arr[i];
		        A = new double[2][k][k];
		        for (m = 0; m < k; m++) {
		        	for (n = 0; n < k; n++) {
		        		A[0][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
		        		A[1][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
		        	}
		        }
		        B1 = new double[2][2*k+1][k+1];
		        B2 = new double[2][2*k+1][k+1];
		        for (m = 0; m < 2*k+1; m++) {
		        	for (n = 0; n < k+1; n++) {
		        		B1[0][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
		        		B1[1][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
		        		B2[0][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
		        		B2[1][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
		        	}
		        }
	            
		        B1 = new double[2][2*k+1][k+1];
		        B2 = new double[2][2*k+1][k+1];
		        for (m = 0; m < 2*k+1; m++) {
		        	for (n = 0; n < k+1; n++) {
		        		B1[0][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
		        		B1[1][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
		        		B2[0][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
		        		B2[1][m][n] = randomGen.genUniformRandomNum(0.0, 1.0);
		        	}
		        }
		        r1 = new double[2][2*k+1][k+1];
			    r2 = new double[2][2*k+1][k+1];
			    r = new double[2][2*k+1][2*(k+1)];
		        B = new double[2][2*k+1][2*(k+1)];
		        for (m = 0; m < 2*k+1; m++) {
		        	for (n = 0; n < k+1; n++) {
		        		B[0][m][n] = B1[0][m][n];
		        		B[1][m][n] = B1[1][m][n];
		        		B[0][m][n+k+1] = B2[0][m][n];
		        		B[1][m][n+k+1] = B2[1][m][n];
		        	}
		        }
		        if (l == 1) {
		        	PPFT(r1,r2,A);
			        for (m = 0; m < 2*k+1; m++) {
			        	for (n = 0; n < k+1; n++) {
			        		r[0][m][n] = r1[0][m][n];
			        		r[1][m][n] = r1[1][m][n];
			        		r[0][m][n+k+1] = r2[0][m][n];
			        		r[1][m][n+k+1] = r2[1][m][n];
			        	}
			        }
		            a = ip(r,B);
		            b = ip(A,adjPPFT(B1,B2));
		        }
		        else {
		        	Radon(r1[0],r2[0],A[0]);
		        	for (m = 0; m < 2*k+1; m++) {
			        	for (n = 0; n < k+1; n++) {
			        		r[0][m][n] = r1[0][m][n];
			        		r[0][m][n+k+1] = r2[0][m][n];
			        	}
			        }
		        	a = new double[2];
		        	a[0] = ip(r[0],B[0]);
		        	a[1] = 0.0;
		        	b = new double[2];
		        	b[0] = ip(A[0], adjRadon(B1[0],B2[0]));
		        	b[1] = 0.0;
		        }
		
		        System.out.println("k = " + k);
		        System.out.println("a = " + a[0] + " " + a[1]+"i");
		        System.out.println("b = " + b[0] + " " + b[1]+"i");
		        double error[] = new double[] {a[0]-b[0], a[1]-b[1]};
		        System.out.println("Error = " + error[0] + " "+error[1]+"i");
		        double abserr = zabs(error[0],error[1]);
		        double rela = abserr/zabs(a[0],a[1]);
		        System.out.println("Rel a = " + rela);
		        double relb = abserr/zabs(b[0],b[1]);
		        System.out.println("Rel b = " + relb);
		        System.out.println("*********************\n");
	       } // for (i = 0; i < arr.length; i++)
    	} // for (l = 1; l <= 2; l++)
    }
    
    public void testRadon() {
    	// PPFT OK
    	// OptimizedPPFT OK
    	// Radon OK
	    
	    // Tests the functions PPFT,Optimized PPFT, and Radon.
	    // See also PPFT, OptimizedPPPT, Radon.
	    
	    //  Yoel Shkolnisky 9/2/02
	
	    double eps = 0.00000001;
	    int imint[][] = MagicSquare(8);
	    double im[][][] = new double[2][8][8];
	    int i,j;
	    for (i = 0; i < 8; i++) {
	    	for (j = 0; j < 8; j++) {
	    		im[0][i][j] = (double)imint[i][j];
	    		im[1][i][j] = (double)imint[i][j] + 1.0;
	    	}
	    }
	    
	    double spp1[][][] = new double[2][17][9];
	    double spp2[][][] = new double[2][17][9];
	    double pp1[][][] = new double[2][17][9];
	    double pp2[][][] = new double[2][17][9];
	    double opp1[][][] = new double[2][17][9];
	    double opp2[][][] = new double[2][17][9];
	    slowPPFT(spp1, spp2, im);
	    PPFT(pp1, pp2, im);
	    OptimizedPPFT(opp1, opp2,im);
	    double maxpp1Error = 0.0;
	    double maxpp2Error = 0.0;
	    double maxopp1Error = 0.0;
	    double maxopp2Error = 0.0;
	    double error;
	    for (i = 0; i < 17; i++) {
	    	for (j = 0; j < 9; j++) {
	    	    error = zabs(spp1[0][i][j]-pp1[0][i][j],spp1[1][i][j]-pp1[1][i][j]);
	    	    if (error > maxpp1Error) {
	    	    	maxpp1Error = error;
	    	    }
	    	    error = zabs(spp2[0][i][j]-pp2[0][i][j],spp2[1][i][j]-pp2[1][i][j]);
	    	    if (error > maxpp2Error) {
	    	    	maxpp2Error = error;
	    	    }
	    	    error = zabs(spp1[0][i][j]-opp1[0][i][j],spp1[1][i][j]-opp1[1][i][j]);
	    	    if (error > maxopp1Error) {
	    	    	maxopp1Error = error;
	    	    }
	    	    error = zabs(spp2[0][i][j]-opp2[0][i][j],spp2[1][i][j]-opp2[1][i][j]);
	    	    if (error > maxopp2Error) {
	    	    	maxopp2Error = error;
	    	    }
	    	}
	    }
	    
	    if ((maxpp1Error > eps) || (maxpp2Error > eps)) {
	    	System.err.println("PPFT not OK");
	    	System.err.println("maxpp1Error = " + maxpp1Error);
	    	System.err.println("maxpp2Error = " + maxpp2Error);
	    }
	    else {
	    	System.out.println("PPFT OK");
	    }
	    
	    if ((maxopp1Error > eps) || (maxopp2Error > eps)) {
	    	System.err.println("OptimizedPPFT not OK");
	    	System.err.println("maxopp1Error = " + maxopp1Error);
	    	System.err.println("maxopp2Error = " + maxopp2Error);
	    }
	    else {
	    	System.out.println("OptimizedPPFT OK");
	    }
	
	    double sr1[][] = new double[17][9];
	    double sr2[][] = new double[17][9];
	    double r1[][] = new double[17][9];
	    double r2[][] = new double[17][9];
	
	    slowRadon(sr1,sr2,im[0]);
	    Radon(r1,r2,im[0]);
	    double maxr1Error = 0.0;
	    double maxr2Error = 0.0;
	    
	    for (i = 0; i < 17; i++) {
	    	for (j = 0; j < 9; j++) {
	    	    error = Math.abs(sr1[i][j] - r1[i][j]);
	    	    if (error > maxr1Error) {
	    	    	maxr1Error = error;
	    	    }
	    	    error = Math.abs(sr2[i][j] - r2[i][j]);
	    	    if (error > maxr2Error) {
	    	    	maxr2Error = error;
	    	    }
	    	}
	    }
	    
	    if ((maxr1Error > eps) || (maxr2Error > eps)) {
	    	System.err.println("Radon not OK");
	    	System.err.println("maxr1Error = " + maxr1Error);
	    	System.err.println("maxr2Error = " + maxr2Error);
	    }
	    else {
	    	System.out.println("Radon OK");
	    }
    }
    
    public void testIRadon() {
        // All 8 tests passed.
	    // Tests the functions iRadon and ippft.
	    //
	    // Tests the correctness and performance of the inversion algorithm of the discrete Radon transform.
	    // The function tests also the inversion of the pseudo-polar Fourier transform since 
	    // inverting the discrete Radon transform requirers inverting the pseudo-polar Fourier transform.
	    
	    // Yoel Shkolnisky 21/12/02
	
	    // test No.1: magic square of size 64x64
	    // Inversion should converge
	    double pp1[][][];
	    double pp2[][][];
		int imint[][] = MagicSquare(64);
	    double a[][] = new double[64][64];
	    int i,j;
	    for (i = 0; i < 64; i++) {
	    	for (j = 0; j < 64; j++) {
	    		a[i][j] = (double)imint[i][j];
	    	}
	    }
	    pp1 = new double[2][2*64+1][64+1];
	    pp2 = new double[2][2*64+1][64+1];
	    Radon(pp1[0],pp2[0],a);
	    runTest(pp1,pp2,1.e-2,20,a,"1 - Magic square 64x64",true,true);
	
	    // test No.2: magic square of size 64x64
	    // Inversion should NOT converge (required error too small)
	    imint = MagicSquare(64);
	    a = new double[64][64];
	    for (i = 0; i < 64; i++) {
	    	for (j = 0; j < 64; j++) {
	    		a[i][j] = (double)imint[i][j];
	    	}
	    }
	    pp1 = new double[2][2*64+1][64+1];
	    pp2 = new double[2][2*64+1][64+1];
	    Radon(pp1[0],pp2[0],a);
	    runTest(pp1,pp2,1.e-7,10,a,"2 - Magic square 64x64 (error too small)",true,true);
	
	    // test No.3: magic square of size 64x64
	    // Inversion should NOT converge (not enough iterations)
	    imint = MagicSquare(64);
	    a = new double[64][64];
	    for (i = 0; i < 64; i++) {
	    	for (j = 0; j < 64; j++) {
	    		a[i][j] = (double)imint[i][j];
	    	}
	    }
	    pp1 = new double[2][2*64+1][64+1];
	    pp2 = new double[2][2*64+1][64+1];
	    Radon(pp1[0],pp2[0],a);
	    runTest(pp1,pp2,1.e-4,5,a,"3 - Magic square 64x64 (not enough iterations)",true,true);
	
	
	    // test No.4: magic square of size 128x128
	    imint = MagicSquare(128);
	    a = new double[128][128];
	    for (i = 0; i < 128; i++) {
	    	for (j = 0; j < 128; j++) {
	    		a[i][j] = (double)imint[i][j];
	    	}
	    }
	    pp1 = new double[2][2*128+1][128+1];
	    pp2 = new double[2][2*128+1][128+1];
	    Radon(pp1[0],pp2[0],a);
	    runTest(pp1,pp2,1.e-1,10,a,"4 - Magic square 128x128",true,true);
	
	    //test No.5: random 64x64 image with real values from [0,1]
	    RandomNumberGen randomGen = new RandomNumberGen();
	    a = new double[64][64];
	    for (i = 0; i < 64; i++) {
	    	for (j = 0; j < 64; j++) {
	    		a[i][j] = randomGen.genUniformRandomNum(0.0, 1.0);
	    	}
	    }
	    pp1 = new double[2][2*64+1][64+1];
	    pp2 = new double[2][2*64+1][64+1];
	    Radon(pp1[0],pp2[0],a);
	    runTest(pp1,pp2,1.e-2,10,a,"5 - Real random matrix 64x64 from [0,1]",true,false);
	
	    //test No.6: random 64x64 image with integer values from [0,255]
	    a = new double[64][64];
	    for (i = 0; i < 64; i++) {
	    	for (j = 0; j < 64; j++) {
	    		a[i][j] = Math.floor(256.0*randomGen.genUniformRandomNum(0.0, 1.0));
	    	}
	    }
	    pp1 = new double[2][2*64+1][64+1];
	    pp2 = new double[2][2*64+1][64+1];
	    Radon(pp1[0],pp2[0],a);
	    runTest(pp1,pp2,1.e-2,10,a,"6 - Integer random matrix 64x64 from [0,255]",true,true);
	
	    // test No.7: Lena 256
	    String lenaFile = "C:" + File.separator + "Polar Fourier Transform" + File.separator+ "ppft2" + File.separator + "lena.bmp";
	    FileIO fileIO = new FileIO(); 
    	fileIO.setQuiet(true);
    	fileIO.setSuppressProgressBar(true);
    	ModelImage lenaImage = fileIO.readImage(lenaFile);
    	int length = 256*256;
    	int buffer[] = new int[length];
    	try {
    		lenaImage.exportData(0, length, buffer);
    	}
    	catch (IOException e) {
    		System.err.println("IOException on lenaImage.exportData");
    		System.exit(0);
    	}
    	lenaImage.disposeLocal();
    	lenaImage = null;
    	a = new double[256][256];
	    for (i = 0; i < 256; i++) {
	    	for (j = 0; j < 256; j++) {
	    		a[i][j] =  (double)buffer[256*i+j];
	    	}
	    }
	    pp1 = new double[2][2*256+1][256+1];
	    pp2 = new double[2][2*256+1][256+1];
	    Radon(pp1[0],pp2[0],a);
	    runTest(pp1,pp2,1.e-2,10,a,"7 - Lena 256",true,true);
	
	    //test No.8: magic square 32x32. Run quietly
	    imint = MagicSquare(32);
	    a = new double[32][32];
	    for (i = 0; i < 32; i++) {
	    	for (j = 0; j < 32; j++) {
	    		a[i][j] = (double)imint[i][j];
	    	}
	    }
	    pp1 = new double[2][2*32+1][32+1];
	    pp2 = new double[2][2*32+1][32+1];
	    Radon(pp1[0],pp2[0],a);
	    runTest(pp1,pp2,1.e-2,10,a,"8 - Magic square 32x32 (quiet)",false,true);
    }

     
    //%%%%%%%%%%%%%%%
    // Sub functions
    //%%%%%%%%%%%%%%%

    // Execute a single inversion test.
    // pp1,pp2      The Radon sectors.
    // ErrTol       Residual error required from the inversion algorithm.
    // MaxIts       Number of iterations of the inversion algorithm.
    // ref          The original image. Used as a reference to check the absolute error.
    // description  Test description for printing purposes.
    // verbose      If true, print the inversion log.
    // trueimage    True if the input represents the discrete Radon transform of an integer image.
    //              In this case it is possible to exactly inverting the transform
    //              by truncating any floating parts of the result.
    private void runTest(double pp1[][][], double pp2[][][], double ErrTol,
    		int MaxIts, double ref[][], String description,
    		boolean verbose, boolean trueimage) {

	    int i,j;
    	System.out.println("Test name : " + description);
	    int n = (pp1[0].length-1)/2;
	    System.out.println("Input size = " + n);
	    System.out.println("Requested error = " + ErrTol);
	    System.out.println("Max number of iterations = " + MaxIts);
	    if (verbose) {
	       System.out.println("Inversion log:");
	       System.out.println("--------------");
	    }
	    long startTime = System.currentTimeMillis();
	    double Y[][][] = new double[2][n][n];
	    int flag[] = new int[1];
	    double res[] = new double[1];
	    int iter[] = new int[1];
	    iRadon(Y,flag,res,iter,pp1,pp2,ErrTol,MaxIts,verbose);
	    double t = (System.currentTimeMillis() - startTime)/1000.0;
	    String str;
	    if (flag[0] == 0) {
	       str = "CONVERGED";
	    }
	    else {
	       str = "DID NOT CONVERGE";
	    }
	
	    System.out.println("\nResults:");
	    System.out.println("---------");
	    System.out.println("Inversion " + str);
	    System.out.println("Residual error " + res[0] + " at iteration no. " + iter[0]);
	    double error;
	    double maxError = 0.0;
	    for (i = 0; i < n; i++) {
	    	for (j = 0; j < n; j++) {
	    		error = Math.abs(Y[0][i][j] - ref[i][j]);
	    		if (error > maxError) {
	    			maxError = error;
	    		}
	    	}
	    }
	    System.out.println("Maxiumum absolute error = " + maxError);
	
	    if (trueimage) {
	    	maxError = 0.0;
	    	for (i = 0; i < n; i++) {
		    	for (j = 0; j < n; j++) {
		    		error = Math.abs(Math.round(Y[0][i][j]) - ref[i][j]);
		    		if (error > maxError) {
		    			maxError = error;
		    		}
		    	}
		    }
	       System.out.println("Maximum absolute error of reconstructed image = " + maxError);
	    }
	       
	    System.out.println("Computation time = " + t + " seconds");
	    System.out.println("--------------------------------------------------------\n\n");
    }
    
    public void testppft3() {
	    
	    // Test the functions ppft3_ref, ppft3, and radon3.
	    
	    // Yoel Shkolnisky 20/05/2013
	    int i,j,n;
	    int volume;
	    double eps = 1.e-14;
	    ModelImage im;
	    int extents[] = new int[3];
	
	    int sz[]=new int[] {4, 8, 16, 20, 32, 40, 64, 100, 128, 200, 256};
	    RandomNumberGen randomGen = new RandomNumberGen();
	
	    for (i = 0; i < sz.length; i++) {
	    	n = sz[i];
	        // Test the function ppft3 by comparing it to ppft3_ref.
	    	volume = n*n*n;
	    	double buffer[] = new double[volume];
	    	for (j = 0; j < volume; j++) {
	    		buffer[i] = randomGen.genUniformRandomNum(0.0, 1.0);
	    	}
	    	extents[0] = n;
	    	extents[1] = n;
	    	extents[2] = n;
	    	im = new ModelImage(ModelImage.DOUBLE, extents, "im");
	    	try {
	    		im.importData(0, buffer, true);
	    	}
	    	catch (IOException e) {
	    		System.err.println("In testppft3 IOException on im.importData(0,buffer,true)");
	    		System.exit(0);
	    	}
	    	long startTime = System.currentTimeMillis();
	        double pp[][][][][] = ppft3_ref(im);
	        double t1=(System.currentTimeMillis() - startTime)/1000.0;
	        startTime = System.currentTimeMillis();
	        double pp2[][][][][] = ppft3(im);
	        double t2=(System.currentTimeMillis() - startTime)/1000.0;
	        boolean ok[] = new boolean[1];
	        double err[] = new double[1];
	        equals(pp,pp2,eps,ok,err);
	        reportResult("Test ppft3",n,ok[0],err[0],t1,t2);
	    } // for (i = 0; i < sz.length; i++)
	    
	    // Test the function Radon3 by comparing it to the reference function
	    // slowRadon3.
	    volume = 4*4*4;
    	double buffer[] = new double[volume];
    	for (j = 0; j < volume; j++) {
    		buffer[i] = randomGen.genUniformRandomNum(0.0, 1.0);
    	}
    	extents[0] = 4;
    	extents[1] = 4;
    	extents[2] = 4;
    	im = new ModelImage(ModelImage.DOUBLE, extents, "im");
    	try {
    		im.importData(0, buffer, true);
    	}
    	catch (IOException e) {
    		System.err.println("In testppft3 IOException on im.importData(0,buffer,true)");
    		System.exit(0);
    	}
    	long startTime = System.currentTimeMillis();
	    double rr[][][][] = slowradon3(im);
	    double t1=(System.currentTimeMillis() - startTime)/1000.0;
        startTime = System.currentTimeMillis();
	    double rr2[][][][] = Radon3(im);
	    double t2=(System.currentTimeMillis() - startTime)/1000.0;
        boolean ok[] = new boolean[1];
        double err[] = new double[1];
        equals(rr,rr2,eps,ok,err);
	    reportResult("Test radon3",4,ok[0],err[0],t1,t2);
    }



    private void reportResult(String testMsg,int n,boolean res, double err, double t1, double t2) {
	    String str;
	    if (res) {
	        str = "OK";
	    }
	    else {
	        str = "FAIL";
	    }
	    System.out.println(testMsg + " n = " + n + " " + str + " err = " + err);
	    System.out.println("t1 = " + t1 + " seconds t2 = " + t2 + " seconds speedup = " + (t1/t2));
    }


    private void equals(double v1[][][][][], double v2[][][][][], double eps, boolean ok[], double error[]) {
    		
    		// Compare two multi-dimensional arrays.
    		
    		// v1,v2    multi-dimensional arrays to compare
    		// eps      Threshold. Default 1.e-12
    		
    		// The comparison is performed element by element. The arrays are considered
    		// equal if no element in the difference v1-v2 exceeds the threshold.
    		
    		// Returns true if the arrays are equal and false otherwise.
    		
    		// Yoel Shkolnisky 03/02/03
            int i,j,k,m;
            double diff;
            double diffImag;
    		ok[0] = true;

    		// Verify that v1 and v2 have the same dimensions
    		int s11 = v1.length;
    		int s21 = v2.length;

    		if (s11 != s21) {
    		    MipavUtil.displayError("v1.length = " + s11 + " != v2.length = " + s21);
    		    System.exit(0);
    		}
    		
    		int s12 = v1[0].length;
    		int s22 = v2[0].length;

    		if (s12 != s22) {
    		    MipavUtil.displayError("v1[0].length = " + s12 + " != v2[0].length = " + s22);
    		    System.exit(0);
    		}
    		
    		int s13 = v1[0][0].length;
    		int s23 = v2[0][0].length;

    		if (s13 != s23) {
    		    MipavUtil.displayError("v1[0][0].length = " + s13 + " != v2[0][0].length = " + s23);
    		    System.exit(0);
    		}
    		
    		int s14 = v1[0][0][0].length;
    		int s24 = v2[0][0][0].length;

    		if (s14 != s24) {
    		    MipavUtil.displayError("v1[0][0][0].length = " + s14 + " != v2[0][0][0].length = " + s24);
    		    System.exit(0);
    		}
    		
    		int s15 = v1[0][0][0][0].length;
    		int s25 = v2[0][0][0][0].length;

    		if (s15 != s25) {
    		    MipavUtil.displayError("v1[0][0][0][0].length = " + s15 + " != v2[0][0][0][0].length = " + s25);
    		    System.exit(0);
    		}
    		
    		double sumDiffSquare = 0;
    		double sumV1Square = 0;
    		for (i = 0; i < s12; i++) {
    			for (j = 0; j < s13; j++) {
    				for (k = 0; k < s14; k++) {
    					for (m = 0; m < s15; m++) {
    						diff = v1[0][i][j][k][m] - v2[0][i][j][k][m];
    						diffImag = v1[1][i][j][k][m] - v2[1][i][j][k][m];
    						sumDiffSquare += (diff*diff + diffImag*diffImag);
    						sumV1Square += (v1[0][i][j][k][m]*v1[0][i][j][k][m] + v1[1][i][j][k][m]*v1[1][i][j][k][m]);
    					}
    				}
    			}
    		}
    		double ratio = sumDiffSquare/sumV1Square;
    		error[0] = Math.sqrt(ratio);

    		if (error[0] > eps) {
    		        ok[0] = false;
    		}
    }
    
    private void equals(double v1[][][][], double v2[][][][], double eps, boolean ok[], double error[]) {
		
		// Compare two multi-dimensional arrays.
		
		// v1,v2    multi-dimensional arrays to compare
		// eps      Threshold. Default 1.e-12
		
		// The comparison is performed element by element. The arrays are considered
		// equal if no element in the difference v1-v2 exceeds the threshold.
		
		// Returns true if the arrays are equal and false otherwise.
		
		// Yoel Shkolnisky 03/02/03
        int i,j,k,m;
        double diff;
		ok[0] = true;

		// Verify that v1 and v2 have the same dimensions
		int s11 = v1.length;
		int s21 = v2.length;

		if (s11 != s21) {
		    MipavUtil.displayError("v1.length = " + s11 + " != v2.length = " + s21);
		    System.exit(0);
		}
		
		int s12 = v1[0].length;
		int s22 = v2[0].length;

		if (s12 != s22) {
		    MipavUtil.displayError("v1[0].length = " + s12 + " != v2[0].length = " + s22);
		    System.exit(0);
		}
		
		int s13 = v1[0][0].length;
		int s23 = v2[0][0].length;

		if (s13 != s23) {
		    MipavUtil.displayError("v1[0][0].length = " + s13 + " != v2[0][0].length = " + s23);
		    System.exit(0);
		}
		
		int s14 = v1[0][0][0].length;
		int s24 = v2[0][0][0].length;

		if (s14 != s24) {
		    MipavUtil.displayError("v1[0][0][0].length = " + s14 + " != v2[0][0][0].length = " + s24);
		    System.exit(0);
		}
		
		double sumDiffSquare = 0;
		double sumV1Square = 0;
		for (i = 0; i < s11; i++) {
			for (j = 0; j < s12; j++) {
				for (k = 0; k < s13; k++) {
					for (m = 0; m < s14; m++) {
						diff = v1[i][j][k][m] - v2[i][j][k][m];
						sumDiffSquare += (diff*diff);
						sumV1Square += (v1[i][j][k][m]*v1[i][j][k][m]);
					}
				}
			}
		}
		double ratio = sumDiffSquare/sumV1Square;
		error[0] = Math.sqrt(ratio);

		if (error[0] > eps) {
		        ok[0] = false;
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
    	fft.setShowProgress(false);
    	fft.run();
    	fft.finalize();
    	fft = null;
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
    	fft.setShowProgress(false);
    	fft.run();
        fft.finalize();
        fft = null;
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
    	fftx.setShowProgress(false);
    	fftx.run();
    	fftx.finalize();
    	fftx = null;
    	FFTUtility ffty = new FFTUtility(ixarr[0], ixarr[1], 1, yDim, xDim, -1, FFTUtility.FFT);
    	ffty.setShowProgress(false);
    	ffty.run();
    	ffty.finalize();
    	ffty = null;
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
    	fftx.setShowProgress(false);
    	fftx.run();
    	fftx.finalize();
    	fftx = null;
    	FFTUtility ffty = new FFTUtility(ixarr[0], ixarr[1], 1, yDim, xDim, 1, FFTUtility.FFT);
    	ffty.setShowProgress(false);
    	ffty.run();
    	ffty.finalize();
    	ffty = null;
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
    	fftx.setShowProgress(false);
    	fftx.run();
    	fftx.finalize();
    	fftx = null;
    	FFTUtility ffty = new FFTUtility(ixarr[0], ixarr[1], zDim, yDim, xDim, -1, FFTUtility.FFT);
    	ffty.setShowProgress(false);
    	ffty.run();
    	ffty.finalize();
    	ffty = null;
    	FFTUtility fftz = new FFTUtility(ixarr[0], ixarr[1], 1, zDim, length, -1, FFTUtility.FFT);
    	fftz.setShowProgress(false);
    	fftz.run();
    	fftz.finalize();
    	fftz = null;
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
    	fftx.setShowProgress(false);
    	fftx.run();
    	fftx.finalize();
    	fftx = null;
    	FFTUtility ffty = new FFTUtility(ixarr[0], ixarr[1], zDim, yDim, xDim, 1, FFTUtility.FFT);
    	ffty.setShowProgress(false);
    	ffty.run();
    	ffty.finalize();
    	ffty = null;
    	FFTUtility fftz = new FFTUtility(ixarr[0], ixarr[1], 1, zDim, length, 1, FFTUtility.FFT);
    	fftz.setShowProgress(false);
    	fftz.run();
    	fftz.finalize();
    	fftz = null;
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
    
    private void iRadon(double Y[][][], int flag[], double residual[], int iter[],
    		double res1[][][], double res2[][][], double ErrTol, int MaxIts,
    		boolean verbose) {
    		
    		// 2-D inverse discrete Radon transform.
    		// The inverse transform is computed using the conjugate gradient method.
    		
    		//  Input parameters:
    		//    res1,res22   Discrete Radon sectors as returned from the function Radon.
    		//    ErrTol       Optional parameter of error tolerance used by the conjugate
    		//                 gradient method. If not specified, tolerance of 1.e-2 is used.
    		//    MaxIts       Maximum number of iterations. Default 10.
    		//    verbose      Display verbose CG information. 0 will suppress verbose information.
    		//                 Any non-zero value will display verbose CG information. Default false.
    		//
    		//  Output arguments:
    		//    Y            The inverted matrix.
    		//    flag         Convergence flag. See CG for more information.
    		//    residual     Residual error at the end of the inversion.
    		//    iter         The iteration number at which ErrTol was achieved. Relevant only if
    		//                 flag=0
    		//
    		// Yoel Shkolnisky 17/12/02


    		//temp1 = cfftd(res1,1);
    		//temp2 = cfftd(res2,1);
    	    int i,j;
    		int t1 = res1[0].length;
    		int t2 = res1[0][0].length;
    		double temp1[][][] = new double[2][t1][t2];
    		double temp2[][][] = new double[2][t1][t2];
    		// Forward FFT on first dimension
            double tmpC[][];
   	         for (j = 0; j < t2; j++) {
	   	    	 tmpC = new double[2][t1];
	   	    	 for (i = 0; i < t1; i++) {
	   	    		 tmpC[0][i] = res1[0][i][j];
	   	    		 tmpC[1][i] = res1[1][i][j];
	   	    	 }
	   	    	 tmpC = ifftshift1d(tmpC);
	   	    	 FFTUtility fft = new FFTUtility(tmpC[0], tmpC[1], 1, t1, 1, -1, FFTUtility.FFT);
	   		     fft.setShowProgress(false);
	   		     fft.run();
	   		     fft.finalize();
	   		     fft = null;
	   		     tmpC = fftshift1d(tmpC);
	   	    	 for (i = 0; i < t1; i++) {
	   	    		 temp1[0][i][j] = tmpC[0][i];
	   	    		 temp1[1][i][j] = tmpC[1][i];
	   	    	 }
   	         }
   	    	 
   	    	for (j = 0; j < t2; j++) {
      	    	 tmpC = new double[2][t1];
      	    	 for (i = 0; i < t1; i++) {
      	    		 tmpC[0][i] = res2[0][i][j];
      	    		 tmpC[1][i] = res2[1][i][j];
      	    	 }
      	    	 tmpC = ifftshift1d(tmpC);
      	    	 FFTUtility fft2 = new FFTUtility(tmpC[0], tmpC[1], 1, t1, 1, -1, FFTUtility.FFT);
      		     fft2.setShowProgress(false);
      		     fft2.run();
      		     fft2.finalize();
      		     fft2 = null;
      		     tmpC = fftshift1d(tmpC);
      	    	 for (i = 0; i < t1; i++) {
      	    		 temp2[0][i][j] = tmpC[0][i];
      	    		 temp2[1][i][j] = tmpC[1][i];
      	    	 }
   	        }

    		ippft(Y,flag,residual,iter,temp1,temp2,ErrTol,MaxIts,verbose);

    		// Revision record
    		// 15/1/03	Yoel Shkolnisky		Used cfftd instead of column-wise cfft
    }

    private void ippft(double Y[][][], int flag[], double residual[], int iter[],
    		double pp1[][][], double pp2[][][], double ErrTol, int MaxIts,
    		boolean verbose) {
	    
	    // Inverse pseudo-polar Fourier transform.
	    // The inverse transform is computed using conjugate gradient method.
	    
	    // Input arguments:
	    // pp1,pp2      Pseudo-polar sectors as returned from the function ppft.
	    // ErrTol       Optional parameter of error tolerance used by the conjugate
	    //              gradient method. If not specified, tolerance of 1.e-2 is used.
	    // MaxIts       Maximum number of iterations. Default 10.
	    // verbose      Display verbose CG information. 0 will suppress verbose information.
	    //              Any non-zero value will display verbose CG information.  Default is false
	    
	    // Output arguments:
	    // Y            The inverted matrix.
	    // flag         Convergence flag. See CG for more information.
	    // residual     Residual error at the end of the inversion.
	    // iter         The iteration number at which ErrTol was achieved. Relevant only if
	    //              flag=0
	    
	    // Yoel Shkolnisky 18/12/02
	
	    double temp[][][] = PrecondAdjPPFT(pp1,pp2);
	    double guess[][][] = new double[2][temp[0].length][temp[0][0].length];
	    double absres[] = new double[1];
	    CG(Y, flag, residual, iter, absres,/*'PtP',*/temp,ErrTol,MaxIts,guess,verbose,null);
	    if (flag[0] == 1) {
	       System.out.println("ippft warning! CG inversion did not converge. Residual error = " + residual[0]);
	    }
    }

    
    private void CG(double [][][]Y, int flag[], double relres[], int iter[] ,double absres[],
    		// function PtP
    		double[][][]X, 
    		//double[] params, 
    		double ErrTol,int MaxIts,
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
    	    double temp[][][];
    	    double temp2;
    	    int row = X[0].length;
    	    int col = X[0][0].length;
    	    int j,r,c;
    	    double gk[][][];
    	    double pk[][][];
    	    double dk;
    	    boolean done;
    	    double perr = 0.0;
    	    double xerr = 0.0;
    	    double err;
    	    double hk[][][];
    	    double tk[] = new double[1];
    	    double tkImag[] = new double[1];
    	    double dotProd[];
    	    double bk;
    		ref_given=true;         // The flags is 1 if the reference untransformed matrix RefY is given and 0 otherwise

    		if (RefY == null) {    // RefY not given
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
    		xk = new double[2][row][col];
    		for (r = 0; r < row; r++) {
    			for (c = 0; c < col; c++) {
    				xk[0][r][c] = guess[0][r][c];
    				xk[1][r][c] = guess[1][r][c];
    			}
    		}
    		// Evaluates the function PtP using xk and params
    		temp = PtP(xk);
    		gk = new double[2][row][col];
    		pk = new double[2][row][col];
    		dk = 0.0;
    		for (r = 0; r < row; r++) {
    			for (c = 0; c < col; c++) {
		    		gk[0][r][c] = temp[0][r][c]-X[0][r][c];
		    		gk[1][r][c] = temp[1][r][c]-X[1][r][c];
		    		pk[0][r][c] = -gk[0][r][c];
		    		pk[1][r][c] = -gk[1][r][c];
		    		dk += (gk[0][r][c]*gk[0][r][c] + gk[1][r][c]*gk[1][r][c]);
    			}
    		}

    		// Conjugate gradient iteration
    		j=2;
    		done = false;
    		while ((j<=MaxIts) && (!done)) {
    			perr = 0.0;
    			for (r = 0; r < row; r++) {
    				for (c = 0; c < col; c++) {
    					perr += (pk[0][r][c]*pk[0][r][c] + pk[1][r][c]*pk[1][r][c]);
    				}
    			}
    		    if (ref_given) { // If reference matrix is given compute absolute error
    		    	xerr = 0;
    		    	for (r = 0; r < row; r++) {
    		    		for (c = 0; c < col; c++) {
    		    			err = zabs(RefY[0][r][c]-xk[0][r][c],RefY[1][r][c]-xk[1][r][c]);
    		    			if (err > xerr) {
    		    				xerr = err;
    		    			}
    		    		}
    		    	}
    		    }
    		    if ((!suppress_output) && (flag[0] == 1)) {
    		    	System.out.println("Iteration = " + (j-1));
    		    	System.out.println("Gradient norm = " + perr);
    		    	System.out.println("Residual error = " + dk);
    		        if (ref_given) {
    		        	System.out.println("Absolute error = " + xerr);
    		        }
    		        System.out.println();
    		    } // if ((!suppress_output) && (flag[0] == 1))

    		    if (perr<=ErrTol) {
    		        iter[0]=j-1;  // CG converged at previous iteration
    		        flag[0]=0;
    		        done=true;
    		    }
    		    if (perr>ErrTol) {
    		        hk = PtP(pk);
    		        // line search parameter
    		        dotProd = new double[2];
    		        for (r = 0; r < row; r++) {
    		        	for (c = 0; c < col; c++) {
    		        		dotProd[0] += (pk[0][r][c]*hk[0][r][c] - pk[1][r][c]*hk[1][r][c]);
    		        		dotProd[1] += (pk[0][r][c]*hk[1][r][c] + pk[1][r][c]*hk[0][r][c]);
    		        	}
    		        }
    		        zdiv(dk, 0.0, dotProd[0], dotProd[1], tk, tkImag);
    		        temp2 = 0.0;
    		        for (r = 0; r < row; r++) {
    		        	for (c = 0; c < col; c++) {
    		        		// update approximate solution
		    		        xk[0][r][c] = xk[0][r][c]+tk[0]*pk[0][r][c] - tkImag[0]*pk[1][r][c]; 
		    		        xk[1][r][c] = xk[1][r][c]+tk[0]*pk[1][r][c] + tkImag[0]*pk[0][r][c]; 
		    		        // update gradient
		    		        gk[0][r][c] = gk[0][r][c]+tk[0]*hk[0][r][c] - tkImag[0]*hk[1][r][c]; 
		    		        gk[1][r][c] = gk[1][r][c]+tk[0]*hk[1][r][c] + tkImag[0]*hk[0][r][c]; 
		    		        temp2 += (gk[0][r][c]*gk[0][r][c] + gk[1][r][c]*gk[1][r][c]);
    		        	}
    		        }
    		        bk = temp2/dk;
    		        dk = temp2;
    		        // update search direction
    		        for (r = 0; r < row; r++) {
    		        	for (c = 0; c < col; c++) {
    		                pk[0][r][c] = -gk[0][r][c]+bk*pk[0][r][c];
    		                pk[1][r][c] = -gk[1][r][c]+bk*pk[1][r][c];
    		        	}
    		        }
    		    } // if (perr>ErrTol)
    		    j=j+1;
    		} // while ((j<=MaxIts) && (!done))

    		relres[0] = perr;
    		if (ref_given) {
    		    absres[0] = xerr;
    		}      

    		for (r = 0; r < row; r++) {
    			for (c = 0; c < col; c++) {
    				Y[0][r][c] = xk[0][r][c];
    				Y[1][r][c] = xk[1][r][c];
    			}
    		}
    } // private void CG
    
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

    		int i,k,l;
    		double r;
    		int twopowl;
    		double coefs[][][];
    		int xpointidx;
    		double c;
    		double intervalleft;
    		double intervalright;
    		int trueidx;
    		double alphak[] = new double[2];
    		double xk;
    		int j;
    		double var;
    		int interval;
    		double p;
    		double b[];
    		int m;
    		int twopowdepth;
    		int finestpartition[][];
    		int listidx;
    		int idx;
    		double tan;
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
    		double tmp[] = new double[n];
    		b = new double[n-1];

    		// Precompute once the coefficients used by the polynomial u
    		double ucoefs[][]= new double[n-1][n];
    		for (k=1; k <= n-1;k++) {
    			for (i = 0; i < n; i++) {
    		        ucoefs[k-1][i] = Math.cos(k*Math.acos(t[i]));
    			}
    		}

    		for (l=2; l <= depth; l++) {
    		    // Compute the interpolation coefficients f

    		    r=(highpoint-lowpoint)/Math.pow(2,(l+1));   // Radius of the interval processed in the current level.
    		    twopowl = (int)Math.round(Math.pow(2,l));
    		    coefs= new double[2][twopowl][nmax];
    		    
    		    xpointidx=0;
    		    for (i=0; i < twopowl; i++) { 
    		        // Center of the current interval. The processed interval is
    		        // [c-r,c+r].
    		        c=2*r*i+lowpoint+r;
    		           
    		        // Create the far-field expansion coefficients for the current
    		        // inteval.
    		        intervalleft=lowpoint+2*r*i;           // left point of the current interval
    		        intervalright=intervalleft+2*r;  // right point of the current interval
    		        
    		        while ((xpointidx< N) && (x[xpointidx]<=intervalright)) {
    		            trueidx=sortidx[xpointidx];
    		            alphak[0]=alpha[0][trueidx];
    		            alphak[1]=alpha[1][trueidx];
    		            xk=x[trueidx];
                        for (j = 0; j < n; j++) { 
                        	var = (t[j]+3*Math.tan(r)*Math.tan(xk-c))/(3*Math.tan(r)-t[j]*Math.tan(xk-c));
                        	coefs[0][i][j] = coefs[0][i][j]+alphak[0]*var;
                        	coefs[1][i][j] = coefs[1][i][j]+alphak[1]*var;
                        }
    		            xpointidx=xpointidx+1;
    		        } // while ((xpointidx< N) && (x[xpointidx]<=intervalright))
    		    } // for (i=0; i < twopowl; i++)
    		    
    		    // For each point y, sum its iteraction with far intervals that were
    		    // not processed in previous iterations. For each interval i,these
    		    // intervals are  exactly intervals i-2 and i+2 at the current level
    		    // (referred to as interaction list at the original paper)
    		    
    		    for (k=0; k < K; k++) {
    		        // Find the interval of yk
    		        interval=(int)Math.floor((y[k]-lowpoint)/(2*r))+1;
    		        if ((interval-2)>=1) {            
    		            c=2*r*(interval-3)+lowpoint+r; // center of interval i-2
    		            p=3*Math.tan(r)/(Math.tan(y[k]-c));
    		            // Compute the polynomial u inline (no procedure call) for optimization
    		            for (j = 1; j <= n-1; j++) {
    		                b[j-1]=Math.cos(j*Math.acos(p));
    		            }
    		            for (j = 0; j < n; j++) {
    		                tmp[j]=ucoefs[0][j]*b[0];
    		            }
    		            for (m=1; m < n-1; m++) {
    		            	for (j = 0; j < n; j++) {
    		                    tmp[j]=tmp[j]+ucoefs[m][j]*b[m];
    		            	}
    		            }
    		            for (j = 0; j < n; j++) {
    		               tmp[j]=2.0*tmp[j]/n;
    		               tmp[j]=tmp[j]+1.0/n;
    		            }
    		            for (j = 0; j < n; j++) {
    		            	v[0][k] = v[0][k] + coefs[0][interval-3][j]*tmp[j];
    		            	v[1][k] = v[1][k] + coefs[1][interval-3][j]*tmp[j];
    		            }
    		        } // if ((interval-2)>=1)
    		        if ((interval+2)<=twopowl) {
    		            c=2*r*(interval+1)+lowpoint+r; // center of interval i+2
    		            p=3*Math.tan(r)/(Math.tan(y[k]-c));
    		            // Compute the polynomial u inline (no procedure call) for optimization
    		            for (j = 1; j <= n-1; j++) {
    		                b[j-1]=Math.cos(j*Math.acos(p));
    		            }
    		            for (j = 0; j < n; j++) {
    		                tmp[j]=ucoefs[0][j]*b[0];
    		            }
    		            for (m=1; m < n-1; m++) {
    		            	for (j = 0; j < n; j++) {
    		                    tmp[j]=tmp[j]+ucoefs[m][j]*b[m];
    		            	}
    		            }
    		            for (j = 0; j < n; j++) {
    		               tmp[j]=2.0*tmp[j]/n;
    		               tmp[j]=tmp[j]+1.0/n;
    		            }
    		            for (j = 0; j < n; j++) {
    		            	v[0][k] = v[0][k] + coefs[0][interval+1][j]*tmp[j];
    		            	v[1][k] = v[1][k] + coefs[1][interval+1][j]*tmp[j];
    		            }
    		        } // if ((interval+2)<=twopowl)
    		        
    		        // Check if we should process also intervals i-3 and i+3
    		        if (((interval-3)>=1) && (Math.floor((interval-1)/2)-Math.floor((interval-4)/2)==1)) {
    		            c=2*r*(interval-4)+lowpoint+r; // center of interval i-3
    		            p=3*Math.tan(r)/(Math.tan(y[k]-c));
    		            // Compute the polynomial u inline (no procedure call) for optimization
    		            for (j = 1; j <= n-1; j++) {
    		                b[j-1]=Math.cos(j*Math.acos(p));
    		            }
    		            for (j = 0; j < n; j++) {
    		                tmp[j]=ucoefs[0][j]*b[0];
    		            }
    		            for (m=1; m < n-1; m++) {
    		            	for (j = 0; j < n; j++) {
    		                    tmp[j]=tmp[j]+ucoefs[m][j]*b[m];
    		            	}
    		            }
    		            for (j = 0; j < n; j++) {
    		               tmp[j]=2.0*tmp[j]/n;
    		               tmp[j]=tmp[j]+1.0/n;
    		            }
    		            for (j = 0; j < n; j++) {
    		            	v[0][k] = v[0][k] + coefs[0][interval-4][j]*tmp[j];
    		            	v[1][k] = v[1][k] + coefs[1][interval-4][j]*tmp[j];
    		            }
    		        } // if (((interval-3)>=1) && (Math.floor((interval-1)/2)-Math.floor((interval-4)/2)==1))

    		        if (((interval+3)<=twopowl) && (Math.floor((interval+2)/2)-Math.floor((interval-1)/2)==1)) {
    		            c=2*r*(interval+2)+lowpoint+r; // center of interval i+3
    		            p=3*Math.tan(r)/(Math.tan(y[k]-c));
    		            // Compute the polynomial u inline (no procedure call) for optimization
    		            for (j = 1; j <= n-1; j++) {
    		                b[j-1]=Math.cos(j*Math.acos(p));
    		            }
    		            for (j = 0; j < n; j++) {
    		                tmp[j]=ucoefs[0][j]*b[0];
    		            }
    		            for (m=1; m < n-1; m++) {
    		            	for (j = 0; j < n; j++) {
    		                    tmp[j]=tmp[j]+ucoefs[m][j]*b[m];
    		            	}
    		            }
    		            for (j = 0; j < n; j++) {
    		               tmp[j]=2.0*tmp[j]/n;
    		               tmp[j]=tmp[j]+1.0/n;
    		            }
    		            for (j = 0; j < n; j++) {
    		            	v[0][k] = v[0][k] + coefs[0][interval+2][j]*tmp[j];
    		            	v[1][k] = v[1][k] + coefs[1][interval+2][j]*tmp[j];
    		            }
    		        } // if (((interval+3)<=twopowl) && (Math.floor((interval+2)/2)-Math.floor((interval-1)/2)==1))      
    		    } // for (k=0; k < K; k++)
    		} // for (l=2; l <= depth; l++)

    		// Compute which points belong to which interval at the finest level.
    		// The array finest partition contains for for each of the 2^depth intervals at the finest 
    		// level the indices of all the x points in that interval.
    		
    		// finestpartition is a 2D array that contains for each interval i
    		// (i=1..2^depth) the indices of all points x that are contained in this
    		// interval.

    		twopowdepth = (int)Math.round(Math.pow(2,depth));
    		finestpartition = new int[twopowdepth][N];
    		for (i = 0; i < twopowdepth; i++) {
    			for (j = 0; j < N; j++) {
    				finestpartition[i][j] = -1;
    			}
    		}
    		r=(highpoint-lowpoint)/Math.pow(2,(depth+1));
    		xpointidx=0;
    		listidx = 0;

    		for (i=0; i < twopowdepth; i++) {
    		    // Center of the current interval. The processed interval is
    		    // [c-r,c+r].
    		    c=2*r*i+lowpoint+r;
    		    listidx=0; //current position in the array the corresponds to the current interval.
    		           
    		    intervalleft=lowpoint+2*r*i;           // left point of the current interval
    		    intervalright=intervalleft+2*r;  // right point of the current interval
    		        
    		    // The 1.0e-15 compansates for tiny numerical errors
    		    // XXX: remove the +1.0e-15, run test13 for n=4 (it won't work) and find
    		    // another fix for the bug. 
    		//  while (xpointidx<=N) & (x(xpointidx)<=intervalright+1.0e-15),

    		   while ((xpointidx<N) && (x[xpointidx]<=intervalright)) {
    		        finestpartition[i][listidx]=sortidx[xpointidx];
    		        listidx=listidx+1;
    		        xpointidx=xpointidx+1;
    		   } // while ((xpointidx<N) && (x[xpointidx]<=intervalright))
    		} // for (i=0; i < twopowdepth; i++) 

    		// Process the last point, which may be unprocessed in the above loop due to
    		// tiny numerical errors
    		if (xpointidx==N-1) {
    		    finestpartition[i][listidx]=sortidx[xpointidx];
    		}
    		    

    		// At the finsest level, compute for each point the interaction with the
    		// neasrest intervals directly.

    		for (k=0; k < K; k++) {
    		    // Find the interval of yk
    			interval=(int)Math.floor((y[k]-lowpoint)/(2*r))+1;
    		    if (interval>twopowdepth) { // should happen only due to tiny numerical errors
    		        interval=twopowdepth;
    		    }
    		    
    		    // Compute the interaction with the previous interval
    			if ((interval-1)>=1) {
    		        j=0;
    		        while ((j<N) && (finestpartition[interval-2][j] != -1)) {
    		            idx=finestpartition[interval-2][j];
    		            tan = Math.tan(y[k] - x[idx]);
    		            v[0][k]=v[0][k]+alpha[0][idx]/tan;
    		            v[1][k]=v[1][k]+alpha[1][idx]/tan;
    		            j=j+1;
    		        } // while ((j<N) && (finestpartition[interval-2][j] != -1))
    		    } // if ((interval-1)>=1)

    		    // Compute the interaction with the current interval
    		    j=0;
    		    while ((j<N) && (finestpartition[interval-1][j] != -1)) {
    		        idx=finestpartition[interval-1][j];
    		        tan = Math.tan(y[k] - x[idx]);
		            v[0][k]=v[0][k]+alpha[0][idx]/tan;
		            v[1][k]=v[1][k]+alpha[1][idx]/tan;
		            j=j+1;
    		    } //   while ((j<N) && (finestpartition[interval-1][j] != -1))
    		    
    		    // Compute the interaction with the next interval
    		    if ((interval+1)<=twopowdepth) {
    		        j=0;
    		        while ((j<N) && (finestpartition[interval][j] != -1)) {
    		            idx=finestpartition[interval][j];
    		            tan = Math.tan(y[k] - x[idx]);
    		            v[0][k]=v[0][k]+alpha[0][idx]/tan;
    		            v[1][k]=v[1][k]+alpha[1][idx]/tan;
    		            j=j+1;
    		        } // while ((j<N) && (finestpartition[interval][j] != -1))
    		    } // if ((interval+1)<=twopowdepth)
    		} // for (k=0; k < K; k++)
    		return v;
    }
    
    private boolean floateq(double x, double y) {
		
		// Check if the real numbers x and y are close up to epsilon.
		// Returns true if equal and false otherwise.
		
		// Yoel Shkolnisky 05/10/04

		double EPS=1.0-15;
		boolean eq= (Math.abs(x-y)<EPS);
		return eq;
    }
    
    private boolean floateq(double x[], double y[]) {
    		
    		// Check if the complex numbers x and y are close up to epsilon.
    		// Returns true if equal and false otherwise.
    		
    		// Yoel Shkolnisky 05/10/04

    		double EPS=1.0-15;
    		boolean eq= ((Math.abs(x[0]-y[0])<EPS)  && (Math.abs(x[1] - y[1]) < EPS));
    		return eq;
    }
    
    private double[][] topsol(double c[][], double rin[][], double y[][]) {
    		
    		// Solve a toeplitz system.
    		// The toeplitz matrix is given by the column c and the row r.
    		// The function solves the system 
    		//     Toeplitz(c,r)*x=y
    		// in O(n^2) operations using the Levinson iterations.
    		
    		// Input parameters
    		//    c  First column of the toepliz matrix.
    		//    r  First row of the toepliz matrix.
    		
    		// Output parameters
    		//    x  The solution of the system Toeplitz(c,r)*x=y
    		
    		// Yoel Shkolnisky 04/10/04
    
            int i,j,k,m,m1,m2,n;
            double sxn[] = new double[2];
            double sd[] = new double[2];
            double sgd[] = new double[2];
            double sgn[] = new double[2];
            double shn[] = new double[2];
            double pp[] = new double[2];
            double qq[] = new double[2];
            double pt1[] = new double[2];
            double pt2[] = new double[2];
            double qt1[] = new double[2];
            double qt2[] = new double[2];
            if ((rin == null) || (rin.length == 0) || (c == null) || (c.length == 0)) {
            	return null;
            }

    		if ((!floateq(c[0][0],rin[0][0])) || (!floateq(c[1][0], rin[1][0]))) {
    		    System.err.println("In topsol first element of column does not match first element of row");
    		    System.exit(0);
    		}

    		if (c[0].length != rin[0].length) {
    		    System.err.println("In topsol c and r must have the same length");
    		    System.exit(0);
    		}

    		// store c and r in a single array R such that R(n+j) is equal to element
    		// R(j) where
    		//    +-                                            -+
    		//    |R(0)   R(-1)   R(-2) ... R(-(N-2))  R(-(N-1)) |
    		// R= |R(1)   R(0)    R(-1)     R(-(N-3))  R(-(N-2)) |
    		//    |...                ....                  .... |
    		//    |R(N-1) R(N-2) R(N-3) ... R(1)       R(0)      |  
    		//    +-                                            -+
    		n=c[0].length;
    		double r[][] = new double[2][2*n-1];
    		for (i = 0; i < n; i++) {
    			r[0][i] = rin[0][n-1-i];
    			r[1][i] = rin[1][n-1-i];
    		}
    		for (i = 1; i < n; i++) {
    			r[0][i+n-1] = c[0][i];
    			r[1][i+n-1] = c[1][i];
    		}

    		// Initialize iterations
    		double x[][] = new double[2][n];
    		double g[][] = new double[2][n];
    		double h[][] = new double[2][n];
    		double divout[] = new double[1];
    		double divoutImag[] = new double[1];
    		zdiv(y[0][0], y[1][0], r[0][n-1], r[1][n-1], divout, divoutImag);
    		x[0][0] = divout[0];
    		x[1][0] = divoutImag[0];
    		if (n == 1) {
    			return x;
    		}
    		
    		zdiv(r[0][n-2], r[1][n-2], r[0][n-1], r[1][n-1], divout, divoutImag);
    		g[0][0] = divout[0];
    		g[1][0] = divoutImag[0];
    		zdiv(r[0][n], r[1][n], r[0][n-1], r[1][n-1], divout, divoutImag);
    		h[0][0] = divout[0];
    		h[1][0] = divoutImag[0];

    		// Iterations
    		for (m=1; m <= n; m++) {
    		    m1=m+1;
    		    sxn[0]=-y[0][m1-1];
    		    sxn[1]=-y[1][m1-1];
    		    sd[0]=-r[0][n-1];
    		    sd[1]=-r[1][n-1];
    		    for (j=1; j <= m; j++) {
    		        sxn[0]=sxn[0]+r[0][n+m1-j-1]*x[0][j-1] - r[1][n+m1-j-1]*x[1][j-1];
    		        sxn[1]=sxn[1]+r[0][n+m1-j-1]*x[1][j-1] + r[1][n+m1-j-1]*x[0][j-1];
    		        sd[0]=sd[0]+r[0][n+m1-j-1]*g[0][m-j] - r[1][n+m1-j-1]*g[1][m-j];
    		        sd[1]=sd[1]+r[0][n+m1-j-1]*g[1][m-j] + r[1][n+m1-j-1]*g[0][m-j];
    		    } // vfor (j=1; j <= m; j++)
    		    if (zabs(sd[0],sd[1])<1.0e-15) {
    		        System.err.println("In topsol singular principal minor");
    		        System.exit(0);
    		    }
    		    
    		    zdiv(sxn[0], sxn[1], sd[0], sd[1], divout, divoutImag);
    		    x[0][m1-1] = divout[0];
    		    x[1][m1-1] = divoutImag[0];
    		    for (j=1; j <= m; j++) {
    		        x[0][j-1]=x[0][j-1]-x[0][m1-1]*g[0][m-j] + x[1][m1-1]*g[1][m-j];
    		        x[1][j-1]=x[1][j-1]-x[0][m1-1]*g[1][m-j] - x[1][m1-1]*g[0][m-j];
    		    }
    		    
    		    if (m1==n) {
    		        return x;
    		    }
    		    
    		    sgn[0]=-r[0][n-m1-1];
    		    sgn[1]=-r[1][n-m1-1];
    		    shn[0]=-r[0][n+m1-1];
    		    shn[1]=-r[1][n+m1-1];
    		    sgd[0]=-r[0][n-1];
    		    sgd[1]=-r[1][n-1];
    		    for (j=1; j <=m; j++) {
    		        sgn[0]=sgn[0]+r[0][n+j-m1-1]*g[0][j-1] - r[1][n+j-m1-1]*g[1][j-1];
    		        sgn[1]=sgn[1]+r[0][n+j-m1-1]*g[1][j-1] + r[1][n+j-m1-1]*g[0][j-1];
    		        shn[0]=shn[0]+r[0][n+m1-j-1]*h[0][j-1] - r[1][n+m1-j-1]*h[1][j-1];
    		        shn[1]=shn[1]+r[0][n+m1-j-1]*h[1][j-1] + r[1][n+m1-j-1]*h[0][j-1];
    		        sgd[0]=sgd[0]+r[0][n+j-m1-1]*h[0][m-j] - r[1][n+j-m1-1]*h[1][m-j];
    		        sgd[1]=sgd[1]+r[0][n+j-m1-1]*h[1][m-j] + r[1][n+j-m1-1]*h[0][m-j];
    		    } // for (j=1; j <=m; j++)
    		    
    		    if (zabs(sgd[0],sgd[1])<1.0e-15) {
    		    	 System.err.println("In topsol singular principal minor");
		             System.exit(0);
    		    }
    		    
    		    zdiv(sgn[0], sgn[1], sgd[0], sgd[1], divout, divoutImag);
    		    g[0][m1-1] = divout[0];
    		    g[1][m1-1] = divoutImag[0];
    		    zdiv(shn[0], shn[1], sd[0], sd[1], divout, divoutImag);
    		    h[0][m1-1] = divout[0];
    		    h[1][m1-1] = divoutImag[0];
    		    k=m-1;
    		    m2=(m+1)/2;
    		    pp[0]=g[0][m1-1];
    		    pp[1]=g[1][m1-1];
    		    qq[0]=h[0][m1-1];
    		    qq[1]=h[1][m1-1];
    		    for (j=0; j < m2; j++) {
    		        pt1[0]=g[0][j];
    		        pt1[1]=g[1][j];
    		        pt2[0]=g[0][k];
    		        pt2[1]=g[1][k];
    		        qt1[0]=h[0][j];
    		        qt1[1]=h[1][j];
    		        qt2[0]=h[0][k];
    		        qt2[1]=h[1][k];
    		        g[0][j]=pt1[0]-pp[0]*qt2[0] + pp[1]*qt2[1];
    		        g[1][j]=pt1[1]-pp[0]*qt2[1] - pp[1]*qt2[0];
    		        g[0][k]=pt2[0]-pp[0]*qt1[0] + pp[1]*qt1[1];
    		        g[1][k]=pt2[1]-pp[0]*qt1[1] - pp[1]*qt1[0];
    		        h[0][j]=qt1[0]-qq[0]*pt2[0] + qq[1]*pt2[1];
    		        h[1][j]=qt1[1]-qq[0]*pt2[1] - qq[1]*pt2[0];
    		        h[0][k]=qt2[0]-qq[0]*pt1[0] + qq[1]*pt1[1];
    		        h[1][k]=qt2[1]-qq[0]*pt1[1] - qq[1]*pt1[0];
    		        k=k-1;
    		    } // for (j=0; j < m2; j++)
    		} // for (m=1; m <= n; m++)
    		return x;
    }
    
    private void topinv(double m1[][], double m2[][], double m3[][], double m4[][],
    		                          double c[][], double r[][]) {
    		
    		// factorize the inverse toeplitz matrix using the Gohberg-Semencul
    		// factorization. This requires O(n^2) operations. 
    		
    		// The function returns four toeplitz matrices which are the
    		// Gohberg-Semencul factorization.
    		
    		// Input parameters
    		//    c  First column of the toeplitz matrix.
    		//    r  First row of the toeplitz matrix.
    		
    		// Output parameters
    		//    m1 m2 m3 m4     Four toeplitz matrics, which correpond to the
    		//                    Gohberg-Semencul factorization of inv(Toeplitz(c,r)).
    		//                    There are cell arrays of size 2 where m_i{1} is the first column 
    		//                    of the toeplitz matrix m_i and m_i{2} is the first row of
    		//                    the toeplitz matrix m_i.
    	    //                    Input m1, m2, m3, m4 as arrays of double[2][2*n];
    		
    		// Yoel Shkolnisky 04/10/04

	    	int i;
	    	double divout[] = new double[1];
	    	double divoutImag[] = new double[1];
    	    if ((!floateq(c[0][0],r[0][0])) || (!floateq(c[1][0], r[1][0]))) {
			    System.err.println("In topinv first element of column does not match first element of row");
			    System.exit(0);
			}	
    	
    		int n = c[0].length;
    		int m = r[0].length;

    		if (m !=n) {
    		    System.err.println("In topinv c and r must have the same length");
    		    System.exit(0);
    		}

    		double e0[][] = new double[2][m];
    	    double en[][] = new double[2][m];
    		e0[0][0]=1;
    		en[0][n-1]=1;

    		double x[][]=topsol(c,r,e0);
    		double y[][] =topsol(c,r,en);
    		
    		for (i = 0; i < n; i++) {
    			zdiv(x[0][i], x[1][i], x[0][0], x[1][0], divout, divoutImag);
    			m1[0][i] = divout[0];
    			m1[1][i] = divoutImag[0];
    		}
    		m1[0][n] = 1.0;
    		m1[1][n] = 0.0;
    		for (i = n+1; i < 2*n; i++) {
    			m1[0][i] = 0.0;
    			m1[1][i] = 0.0;
    		}

    		m2[0][0] = y[0][n-1];
    		m2[1][0] = y[1][n-1];
    		for (i = 1; i < n; i++) {
    			m2[0][i] = 0.0;
    			m2[1][i] = 0.0;
    		}
    		for (i = n; i < 2*n; i++) {
    			m2[0][i] = y[0][2*n-i-1];
    			m2[1][i] = y[1][2*n-i-1];
    		}
    		
    		m3[0][0] = 0.0;
    		m3[1][0] = 0.0;
    		for (i = 0; i < n-1; i++) {
    			zdiv(y[0][i], y[1][i], x[0][0], x[1][0], divout, divoutImag);
    			m3[0][i+1] = divout[0];
    			m3[1][i+1] = divoutImag[0];
    		}
    		for (i = n; i < 2*n; i++) {
    			m3[0][i] = 0.0;
    			m3[1][i] = 0.0;
    		}
    
    		for (i = 0; i < n; i++) {
    			m4[0][i] = 0.0;
    			m4[1][i] = 0.0;
    		}
    		m4[0][n] = 0.0;
    		m4[1][n] = 0.0;
    		for (i = n+1; i < 2*n; i++) {
    			m4[0][i] = x[0][2*n-i];
    			m4[1][i] = x[1][2*n-i];
    		}
    }

    private double[][] topmul(double D[][],double v[][]) {
	    
	    // Mulitply a toeplitz matrix, whose diagonal form is D, with the vector v,
	    // in  O(n log n) operations. 
	    
	    // Input parameters
	    //   D   Diagonal form of the toeplitz matrix to multiply.
	    //   v   The vector to multiply.
	    
	    // Output parameters
	    //   u   The product of the toeplitz matrix that corresponds to D with the
	    //       vector v. 
	    
	    // Yoel Shkolnisky 04/10/04
	
	    int i;
    	int n=v[0].length;
	
	    if (D[0].length !=2*n) {
	        System.err.println("In topmul D must be 2*length(v)");
	        System.exit(0);
	    }
	
	    double fv[][] = new double[2][2*n];
	    for (i = 0; i < n; i++) {
	    	fv[0][i] = v[0][i];
	    	fv[1][i] = v[1][i];
	    }
	    FFTUtility fft = new FFTUtility(fv[0], fv[1], 1, 2*n, 1, -1, FFTUtility.FFT);
	    fft.setShowProgress(false);
	    fft.run();
	    fft.finalize();
	    fft = null;
	    double Dfv[][] = new double[2][2*n];
	    for (i = 0; i < 2*n; i++) {
	    	Dfv[0][i] = D[0][i]*fv[0][i] - D[1][i]*fv[1][i];
	    	Dfv[1][i] = D[0][i]*fv[1][i] + D[1][i]*fv[0][i];
	    }
	    FFTUtility ifft = new FFTUtility(Dfv[0], Dfv[1], 1, 2*n, 1, 1, FFTUtility.FFT);
	    ifft.setShowProgress(false);
	    ifft.run();
	    ifft.finalize();
	    ifft = null;
	    double u[][] = new double[2][n];
	    for (i = 0; i < n; i++) {
	    	u[0][i] = Dfv[0][i];
	    	u[1][i] = Dfv[1][i];
	    }
	    return u;
    }
    
    private double[][] topinvmul(double D1[][], double D2[][], double D3[][], double D4[][], double v[][]) {
    		
    		// Mulitply the inverse toeplitz matrix, where the diagonal form of its
    		// Gohberg-Semencul factors are given by D1,D2,D3,and D4, with the
    		// vector v, in O(n log n) operations. 
    		// The factors D1,D2,D3,D4 are returned by the function topprepinv.
    		
    		// Input parameters
    		//  D1 D2 D3 D4     Diagonal form of the Gohberg-Semencul factors of the
    		//                  inverse topelitz matrix.
    		// v                Vector to multiply.
    		
    		// Output parameters
    		// u                The result of inv(T)*v where T is inv(T) is the inverse
    		//                  toeplitz matrix.
    		
    		// Yoel Shkolnisky 04/10/04

    		int i;
    	    double u1[][]=topmul(D2,v);
    		u1=topmul(D1,u1);

    		double u2[][]=topmul(D4,v);
    		u2=topmul(D3,u2);

    		double u[][] = new double[2][u1[0].length];
    		for (i = 0; i < u1[0].length; i++) {
    			u[0][i] = u1[0][i] - u2[0][i];
    			u[1][i] = u1[1][i] - u2[1][i];
    		}
    		return u;
    }
    
    private double[][] topprep(double c[][], double r[][]) {
	    
	    // Prepare the Toeplitz matrix, whose first column is c and first row is r, 
	    // for fast multiplication by a vector. The function diagonalizes the
	    // toeplitz matrix given by c and r
	    
	    // Input parameters
	    //    c  First column of the toeplitz matrix.
	    //    r  First row of the toeplitz matrix.
	    
	    // Output parameters
	    //    D  Diagonal form of the toeplitz matrix. This data used for fast 
	    //       muliplication of the matrix Toeplitz(c,r) by an arbitrary vector. 
	    
	    // Yoel Shkolnisky 04/10/04
	
	
	    // Algorithm description:
	    
	    // The function embeds the toeplitz matrix, given by c and r, in a circulat
	    // matrix, diagonalizes it, and returns the eigenvalus of the circulat
	    // matrix.
	
    	int i;
    	if ((!floateq(c[0][0],r[0][0])) || (!floateq(c[1][0], r[1][0]))) {
		    System.err.println("In topprep first element of column does not match first element of row");
		    System.exit(0);
		}	
    	
	    int n=c[0].length;
	    int m=r[0].length;
	    
	    double D[][] = new double[2][2*n];
	    for (i = 0; i < n; i++) {
	    	D[0][i] = c[0][i];
	    	D[1][i] = c[1][i];
	    }
	    
	    for (i = n+1; i < 2*n; i++) {
	    	D[0][i] = r[0][2*n-i];
	    	D[1][i] = r[1][2*n-i];
	    }
	
	    FFTUtility fft = new FFTUtility(D[0], D[1], 1, 2*n, 1, -1, FFTUtility.FFT);
	    fft.setShowProgress(false);
	    fft.run();
	    fft.finalize();
	    fft = null;
	    return D;
    }

    private void topprepinv(double D1[][], double D2[][], double D3[][], double D4[][],
    		double m1[][], double m2[][], double m3[][], double m4[][]) {
    		
    		// Like topprep but processes 4 toeplitz matrices at once.
    		
    		// Input parameters
    		//   m1 m2 m3 m4     Cell arrays of size 2 where m_i{1} is the first column
    		//                   of the toeplitz matrix m_i and m_i{2} is the first row of
    		//                   the toeplitz matrix m_i.
    		
    		// Output parameters
    		//  D1 D2 D3 D4      D_i is the diagonal form of the matrix m_i.
    	    //  D1 same length as m1, D2 same length as m2, D3 same length as m3,
    	    //  D4 same length as m4
    		
    		// Yoel Shkolnisky 04/10/04

    		int i;
    	    int len1 = m1[0].length/2;
    		double m1c[][] = new double[2][len1];
    		double m1r[][] = new double[2][len1];
    		for (i = 0; i < len1; i++) {
    			m1c[0][i] = m1[0][i];
    			m1c[1][i] = m1[1][i];
    			m1r[0][i] = m1[0][i+len1];
    			m1r[0][i] = m1[1][i+len1];
    		}
    		
    		int len2 = m2[0].length/2;
    		double m2c[][] = new double[2][len2];
    		double m2r[][] = new double[2][len2];
    		for (i = 0; i < len2; i++) {
    			m2c[0][i] = m2[0][i];
    			m2c[1][i] = m2[1][i];
    			m2r[0][i] = m2[0][i+len2];
    			m2r[0][i] = m2[1][i+len2];
    		}
    		
    		int len3 = m3[0].length/2;
    		double m3c[][] = new double[2][len3];
    		double m3r[][] = new double[2][len3];
    		for (i = 0; i < len3; i++) {
    			m3c[0][i] = m3[0][i];
    			m3c[1][i] = m3[1][i];
    			m3r[0][i] = m3[0][i+len3];
    			m3r[0][i] = m3[1][i+len3];
    		}
    		
    		int len4 = m4[0].length/2;
    		double m4c[][] = new double[2][len4];
    		double m4r[][] = new double[2][len4];
    		for (i = 0; i < len4; i++) {
    			m4c[0][i] = m4[0][i];
    			m4c[1][i] = m4[1][i];
    			m4r[0][i] = m4[0][i+len4];
    			m4r[0][i] = m4[1][i+len4];
    		}

    		D1=topprep(m1c,m1r);
    		D2=topprep(m2c,m2r);
    		D3=topprep(m3c,m3r);
    		D4=topprep(m4c,m4r);
    }

    private double[][] invDecimatedFreqs(double[][][] fim) {
    		
    		// Recover the image whose decimated Fourier transform is fim.
    		// The input is of size (n+1)x(n+1). 
    		// The function computed the toeplitz matrix that corresponds to the
    		// transform that map rim to fim, computes its inverse (O(n^2) operations
    		// for rim of size nxn), and applies this matrix to all rows and columns of
    		// fim. Each application requires O(nlogn) operations and hence the total
    		// complexity for inverting fim is O(n^2logn).
    		
    		// Input parameters
    		//   fim    Frequnecy image to recover.
    		
    		// Output parameters
    		//   rim    The recovered image.
    		
    		// Yoel Shkolnisky 04/10/04
            
            int i,l,k,n;
            double c[][];
            double r[][];
            double m1[][];
            double m2[][];
            double m3[][];
            double m4[][];
            double D1[][];
            double D2[][];
            double D3[][];
            double D4[][];
            double tmpres[][][];
            double rim[][];
            double v[][] = new double[2][];
            double u[][];
    		int n1 = fim[0].length;
    		int n2 = fim[0][0].length;

    		if (n1 != n2) {
    		    System.err.println("In invDecimatedFreqs input matrix must be square");
    		    System.exit(0);
    		}

    	    n=n1-1;
    		c= new double[2][n];
    		r= new double[2][n];
    		for (k=-n/2; k <= n/2-1; k++) {
    		    for (l=-n/2; l <= n/2; l++) {
    		        c[0][k+n/2]=c[0][k+n/2]+Math.cos(4.0*Math.PI*l*(-n/2.0-k)/(2.0*n+1.0));
    		        c[1][k+n/2]=c[1][k+n/2]+Math.sin(4.0*Math.PI*l*(-n/2.0-k)/(2.0*n+1.0));
    		        r[0][k+n/2]=r[0][k+n/2]+Math.cos(4.0*Math.PI*l*(k+n/2.0)/(2.0*n+1.0));
    		        r[1][k+n/2]=r[1][k+n/2]+Math.sin(4.0*Math.PI*l*(k+n/2.0)/(2.0*n+1.0));
    		    } // for (l=-n/2; l <= n/2; l++)
    		} // for (k=-n/2; k <= n/2-1; k++)
    		
    		m1 = new double[2][2*n];
    		m2 = new double[2][2*n];
    		m3 = new double[2][2*n];
    		m4 = new double[2][2*n];
    		topinv(m1,m2,m3,m4,c,r);
    		D1 = new double[2][2*n];
    		D2 = new double[2][2*n];
    		D3 = new double[2][2*n];
    		D4 = new double[2][2*n];
		    topprepinv(D1,D2,D3,D4,m1,m2,m3,m4);

		    tmpres= new double[2][n+1][n];
		    rim=new double[n][n];

		    // apply inverse to rows
		    for (k=0; k < n+1; k++) {
		        v[0]=fim[0][k];
		        v[1]=fim[1][k];
		        v=adjF(v);
		        u=topinvmul(D1,D2,D3,D4,v);
		        tmpres[0][k] = u[0];
		        tmpres[1][k] = u[1];
		    } // for (k=0; k < n+1; k++)

		    // apply inverse to columns
		    for (k=0; k < n; k++) {
		    	for (i = 0; i < n+1; i++) {
		           v[0][i]=tmpres[0][i][k];
		           v[1][i]=tmpres[1][i][k];
		    	}
		        v=adjF(v);
		        u=topinvmul(D1,D2,D3,D4,v);
		        for (i = 0; i < n; i++) {
		            rim[i][k]=u[0][i];
		        }
		    } // for (k=0; k < n; k++)
		    return rim;
    }
    
    private void ippftconsts(double c[][][], double d[][][], int n) {
    		// Prepare the tables with the constants used by the interpolation.
    		// If the tables exists as file then the functions simply reads them.
    		// Otherwise, it generates the using a slow algorithm. In such a case a
    		// warning is issued.
    		
    		// Input parameters
    		//   n    The size of the the pseudo-polar grid is 2x(2n+1)x(n+1)
    		
    		// Output parameters
    		//   c    Constants cl. [4][n/2][n] 
    		//   d    Constants dj. [4][n/2][n]
    		// Both arrays c and d are 3D arrays of size 4xn2xn . The vectors d(type,:,:) and
    		// c(type,:,:), type=1,2,3,4, contain constants which correspond to a
    		// row/column of the given type, where Type 1 refers to rows from -n/2 to -1; type
    		// 2 refers to columns from -n/2 to -1; type 3 refers to rows from 1 to n/2;
    		// and type 4 refers to columns from 1 to n/2. 
    		
    		// Yoel Shkolnisky 11/10/04

    		prepConstD(d, n);
    		prepConstC(c, n);
    }

    		
    private void prepConstD(double d[][][], int n) {
		//d=zeros(4,n/2,n);
        int j;
		for (j=n/2; j >= 1; j--) {
		    d[0][n/2-j] = compD(n,-2*j,1);
		    d[2][n/2-j] = compD(n,2*j,3);
		    d[1][n/2-j] = compD(n,-2*j,2);
		    d[3][n/2-j] = compD(n,2*j,4);       
		} // for (j=n/2; j >= 1; j--)
    }
    
    private int N(int k, int s) {
    		// s is the length of the vector x. This means that s=n+1 where n is even
    		// and the vector x is indexed by
    		//   [x(-n/2),...,x(0),...,x(n/2)].
    	    // The parameter k should in the range -n/2<=k<=n/2, or when using the
    		// argument s: -(s-1)/2 <= k <= (s-1)/2.
    		return(int)Math.round(2.0*((s-1.0)/2.0-Math.abs(k)));
    }
    
    private int[] createArray(int lower, int upper) {
    	int i, len;
    	int result[];
    	if (upper >= lower) {
    	    len = upper - lower + 1;
    	    result = new int[len];
    	    for (i = lower; i <= upper; i++) {
    	    	result[i-lower] = i;
    	    }
    	}
    	else {
    		len = lower - upper + 1;
    		result = new int[len];
    		for (i = lower; i >= upper; i--) {
    			result[lower-i] = i;
    		}
    	}
    	return result;
    }

    		
    private double[] compD(int n, int j, int type) {
    		
    		// Compute the constants required for the interpolation of row/column j in
    		// the the pseudo-polar grid.
    		int i, k, m;
    		double alpha;
    		int idx1[];
    		int idx2[];
    		int idx3[];
    		double y[];
    		double LARGE;
    		double EPS;
    		double dj[];
    		
    		if ((n % 2)==1) {
    		    System.err.println("In compD the length n must be even");
    		    System.exit(0);
    		}
    		m=2*n+1;
    		alpha=-2.0*j/n;

    		// Compute the frequencies in the vector of length N(k,n+1)+n+1. These
    		// frequencies are denoted as the vector y
    		if ((type==1) || (type==4)) {
    		    idx1= createArray(-n/2,-n/2+N(j/2,n+1)/2-1);
    		    idx3=createArray(n/2-N(j/2,n+1)/2+1,n/2-1);
    		}
    		else {
    		    idx1=createArray(-n/2+1,-n/2+N(j/2,n+1)/2-1);
    		    idx3=createArray(n/2-N(j/2,n+1)/2+1,n/2);
    		}

    		if (alpha==-2) {
    			idx2 = createArray(-n/2+N(j/2,n+1)/2+1,n/2-N(j/2,n+1)/2);
    			for (i = 0; i < idx2.length; i++) {
    				idx2[i] = (int)Math.round(idx2[i]/(alpha/2.0));
    			}
    		}
    		else if (alpha==2) {
    			    idx2 = createArray(-n/2+N(j/2,n+1)/2,n/2-N(j/2,n+1)/2-1);
    			    for (i = 0; i < idx2.length; i++) {
        				idx2[i] = (int)Math.round(idx2[i]/(alpha/2.0));
        			}
    		}
    		else if (alpha!=0) {
    			idx2 = createArray(-n/2+N(j/2,n+1)/2,n/2-N(j/2,n+1)/2);
    			for (i = 0; i < idx2.length; i++) {
    				idx2[i] = (int)Math.round(idx2[i]/(alpha/2.0));
    			}
    		}
    		else {
    		    idx2=new int[] {0};
    		}
    		
    		y = new double[idx1.length + idx2.length + idx3.length];
    		for (i = 0; i < idx1.length; i++) {
    			y[i] = -4.0*Math.PI*idx1[i]/m;
    		}
    		for (i = 0; i < idx2.length; i++) {
    			y[i + idx1.length] = -2.0*Math.PI*alpha*idx2[i]/m;
    		}
    		for (i = 0; i < idx3.length; i++) {
    			y[i + idx1.length + idx2.length] = -4.0*Math.PI*idx3[i]/m;
    		}

    		Arrays.sort(y);

    		

    		LARGE=1.0E15;
    		EPS=1.0E-15;
    		dj= new double[n];

    		// Compute the factors dj
    		for (j=0; j < n; j++) {
    		    dj[j]=0.0;
    		    for (k=0; k < n; k++) {
    		        if (j != k) {
    		            if (Math.abs(y[j]-y[k]) > EPS) {
    		                dj[j]=dj[j]-Math.log(Math.sin((y[j]-y[k])/2.0));
    		            }
    		            else {
    		                System.err.println("In compD two values of y are too close to each other");
    		                System.exit(0);
    		            }
    		        } // if (j != k)
    		    } // for (k=0; k < n; k++)
    		} // for (j=0; j < n; j++) 

    		for (i = 0; i < n; i++) {
    		    dj[i]=Math.exp(dj[i]);
    		}
    		return dj;
    }

    		
    private void prepConstC(double c[][][], int n) {
    		//c=zeros(4,n/2,n);

    		int j;
    	    for (j=n/2; j >= 1; j--) {
    		    c[0][n/2-j] = compC(n,-2*j,1);
    		    c[2][n/2-j] = compC(n,2*j,3);
    		    c[1][n/2-j] = compC(n,-2*j,2);
    		    c[3][n/2-j] = compC(n,2*j,4);   
    	    } // for (j=n/2; j >= 1; j--)
    }

    		
    private double[] compC(int n, int j, int type) {
    		
    		// Compute the constant required for the interpolation of row/column j in
    		// the the pseudo-polar grid.
    		int i, k, l, m;
    		double alpha;
    		int idx1[];
    		int idx2[];
    		int idx3[];
    		double y[];
    		int array1[];
    		double x[];
    		int idxX;
    		int idxY;
    		int idxDroppedElems[];
    		boolean found;
    		double xused[];
    		double LARGE;
    		double EPS;
    		double clt[];
    		int lendropped;
    		double cl[];
    		
    		if ((n % 2)==1) {
    		    System.err.println("In compC the length n must be even");
    		    System.exit(0);
    		}

    		m=2*n+1;
    		alpha=-2.0*j/n;

    		// Compute the frequencies in the vector of length N(k,n+1)+n+1. These
    		// frequencies are denoted as the vector y
    		if ((type==1) || (type==4)) {
    		    idx1=createArray(-n/2,-n/2+N(j/2,n+1)/2-1);
    		    idx3=createArray(n/2-N(j/2,n+1)/2+1,n/2-1);
    		}
    		else {
    		    idx1=createArray(-n/2+1,-n/2+N(j/2,n+1)/2-1);
    		    idx3=createArray(n/2-N(j/2,n+1)/2+1,n/2);
    		}

    		if (alpha==-2) {
    			idx2 = createArray(-n/2+N(j/2,n+1)/2+1,n/2-N(j/2,n+1)/2);
    			for (i = 0; i < idx2.length; i++) {
    				idx2[i] = (int)Math.round(idx2[i]/(alpha/2.0));
    			}
    		}
    		else if (alpha==2) {
    	        idx2 = createArray(-n/2+N(j/2,n+1)/2,n/2-N(j/2,n+1)/2-1);
    	        for (i = 0; i < idx2.length; i++) {
    				idx2[i] = (int)Math.round(idx2[i]/(alpha/2.0));
    			}
    		}
    		else if (alpha!=0) {
    			idx2 = createArray(-n/2+N(j/2,n+1)/2,n/2-N(j/2,n+1)/2);
    			for (i = 0; i < idx2.length; i++) {
    				idx2[i] = (int)Math.round(idx2[i]/(alpha/2.0));
    			}
    		}
    		else {
    		    idx2=new int[] {0};
    		}

    		y = new double[idx1.length + idx2.length + idx3.length];
    		if (y.length != n) {
    			System.err.println("In compC y.length != n");
    			System.exit(0);
    		}
    		for (i = 0; i < idx1.length; i++) {
    			y[i] = -4.0*Math.PI*idx1[i]/m;
    		}
    		for (i = 0; i < idx2.length; i++) {
    			y[i + idx1.length] = -2.0*Math.PI*alpha*idx2[i]/m;
    		}
    		for (i = 0; i < idx3.length; i++) {
    			y[i + idx1.length + idx2.length] = -4.0*Math.PI*idx3[i]/m;
    		}

    		// Compute the frequencies on the Cartesian grid.
    		if ((type==1) || (type==4)) {
    			array1 = createArray(-n/2,n/2-1);
    			x = new double[array1.length];
    			for (i = 0; i < x.length; i++) {
    				x[i] = -4.0*Math.PI*array1[i]/m;
    			}
    		}
    		else {
    			array1 = createArray(-n/2+1,n/2);
    			x = new double[array1.length];
    			for (i = 0; i < x.length; i++) {
    				x[i] = -4.0*Math.PI*array1[i]/m;
    			}
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

    		// remove x points that are equal to some y point
    		idxX=0;
    		idxY=0;
    		idxDroppedElems=new int[n]; // the indices of the x that appear in y
    		for (i = 0; i < n; i++) {
    			idxDroppedElems[i] = -1;
    		}

    		j=0;
    		while ((idxX<n) && (idxY<n)) {
    		    if (Math.abs(x[idxX]-y[idxY])<1.0d-15) {
    		        idxDroppedElems[j]=idxX;
    		        j=j+1;
    		        idxX=idxX+1;
    		    }
    		    else if (x[idxX]<y[idxY]) {
    		        idxX=idxX+1;
    		    }
    		    else {
    		        idxY=idxY+1;
    		    }
    		} // while ((idxX<n) && (idxY<n))

    		// idxUsed is the set of indices from x which we need to
    		// compute. In other words, these are the elements that were not dropped in
    		// the previous iteration.
    		m = 0;
    		k = 0;
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
    		xused = new double[m];
    		for (i = 0; i < m; i++) {
    			xused[i] = x[idxUsed[i]];
    		}
    		lendropped=j; // number of dropped elements from x

    		// Now we can resample the polynomial using the modified vector x

    		LARGE=1.0E15;
    		EPS=1.0E-15;
    		clt=new double[m];

    		// Compute the factors cl
    		for (l = 0; l < m; l++) {
    		    // compute cl
    		    clt[l]=0.0;
    		    for (k=0; k < n; k++) {
    		        clt[l]=clt[l]+Math.log(Math.sin((xused[l]-y[k])/2));
    		    }
    		    
    		    if (Math.abs(clt[l])>LARGE) {
    		        System.out.println("Warning in compC value of cl too large...");
    		    }
    		} // for (l = 0; l < m; l++)

    		for (i = 0; i < m; i++) {
    		    clt[i]=Math.exp(clt[i]);
    		}

    		// Put zeros in the elements that were not computed by this function.
    		// These zeros will be dropped when the invserion function gets the table of
    		// cl. We use zero padding so that all arrays will have the same size,
    		// although this wastes storage space. 
    		cl=new double[n];
    		for (i = 0; i < m; i++) {
    		    cl[idxUsed[i]]=clt[i];
    		}
    		return cl;
    }

    private void ippftprecomp(int n) {
	    
	    // Compute the tables used in the inversion of the pseudo-polar of an image
	    // of size nxn.
	    // The function generates two mat files named c_[n] and d_[n] (for example
	    // c_256 and d_256.
	    
	    // Yoel Shkolnisky 11/10/04
    	int i,j,k;
    	double c[][][] = new double[4][n/2][n];
    	double d[][][] = new double[4][n/2][n];
    	RandomAccessFile raFile = null;
	    ippftconsts(c,d,n);
	    String outputFilePath = "C:" + File.separator + "PseudoPolarFourierTranformTables" + File.separator;
	    File file = new File(outputFilePath);
		if (!file.exists()) {
			file.mkdir();
		}
	    String fname = outputFilePath + "d_" + String.valueOf(n);
	    File filed = new File(fname);
		if (!filed.exists()) {
			System.out.println("About to filed.createNewFile()");
			try {
				filed.createNewFile();
			} catch (IOException e) {
				MipavUtil.displayError("In ippftprecomp filed.createNewFile() IOException " + e);
				System.exit(-1);
			}
			try {
				raFile = new RandomAccessFile(filed, "rw");
			} catch (IOException e) {
				MipavUtil.displayError(
						"In ippftprecomp raFile = new RandomAccessFile(filed, \"rw\") IOException " + e);
				System.exit(-1);
			}
			for (i = 0; i < 4; i++) {
				for (j = 0; j < n/2; j++) {
					for (k = 0; k < n; k++) {
					    try {
					    	raFile.writeDouble(d[i][j][k]);
					    }
					    catch (IOException e) {
							MipavUtil.displayError(
									"In ippftprecomp raFile.writeDouble IOException " + e);
							System.exit(-1);
					    }
					}
				}
			}
			try {
				raFile.close();
			}
			catch (IOException e) {
				MipavUtil.displayError(
						"In ippftprecomp raFile.close IOException " + e);
				System.exit(-1);
		    }
		} // if (!filed.exists())
		else {
			System.out.println(fname + " already exists");
		}
	    fname=outputFilePath + "c_" + String.valueOf(n);
	    File filec = new File(fname);
		if (!filec.exists()) {
			System.out.println("About to filec.createNewFile()");
			try {
				filec.createNewFile();
			} catch (IOException e) {
				MipavUtil.displayError("In ippftprecomp filec.createNewFile() IOException " + e);
				System.exit(-1);
			}
			try {
				raFile = new RandomAccessFile(filec, "rw");
			} catch (IOException e) {
				MipavUtil.displayError(
						"In ippftprecomp raFile = new RandomAccessFile(filec, \"rw\") IOException " + e);
				System.exit(-1);
			}
			for (i = 0; i < 4; i++) {
				for (j = 0; j < n/2; j++) {
					for (k = 0; k < n; k++) {
					    try {
					    	raFile.writeDouble(c[i][j][k]);
					    }
					    catch (IOException e) {
							MipavUtil.displayError(
									"In ippftprecomp raFile.writeDouble IOException " + e);
							System.exit(-1);
					    }
					}
				}
			}
			try {
				raFile.close();
			}
			catch (IOException e) {
				MipavUtil.displayError(
						"In ippftprecomp raFile.close IOException " + e);
				System.exit(-1);
		    }
		} // if (!filec.exists())
		else {
			System.out.println(fname + " already exists");
		}  
    }

    private double[][] ippftd(double pp1[][][], double pp2[][][], double EPS) {
	    
	    // Inverse pseudo-polar Fourier transform.
	    
	    // Direct algorithm that computes the inverse pseudo-polar Fourier
	    // transform.
	    
	    // Input parameters:
	    //   pp1,pp2   Pseudo-polar sectors.
	    //   EPS       Required accuracy of the computation.  Default is 1.0E-7.
	    
	    // Output parameters:
	    //   rim       recovered image
	    
	    // Yoel Shkolnisky 24/01/05
	    int n;
	    double fim[][][];
	    int i,j;
	    double r1[][];
	    double r2[][];
	    double c1[][];
	    double c2[][];
	    int idx;
	
	    n=checkInput(pp1,pp2);
	    double c[][][] = new double[4][n/2][n];
	    double d[][][] = new double[4][n/2][n];
	    double fu[][] = new double[2][n+1];
	    double u[][] = new double[2][n+1];
	    double fv[][] = new double[2][n+1];
	    double v[][] = new double[2][n+1];
	    double dj[] = new double[n];
	    double cl[] = new double[n];
	    loadConsts(c,d,n);
	
	    // Prepare output array
	    fim = new double[2][n+1][n+1];
	
	    // Scan the pseudo-polar arrays in an "onion-peel" order and recover the
	    // Fourier samples of each row/column
	    for (j=n/2; j >= 1; j--) {
	        // recover rows j and -j from pp1
	    	idx = toUnaliasedIdx(-2*j,2*n+1);
	    	for (i = 0; i < n+1; i++) {
	    		fu[0][i] = pp1[0][idx][i];
	    		fu[1][i] = pp1[1][idx][i];
	    	}
	        idx = toUnaliasedIdx(-j,n+1);
	        for (i = 0; i < n+1; i++) {
	        	u[0][i] = fim[0][idx][i];
	        	u[1][i] = fim[1][idx][i];
	        }
	        for (i = 0; i < n; i++) {
	            dj[i]=d[0][n/2-j][i];
	            cl[i]=c[0][n/2-j][i];
	        }
	        r1 = recover(fu,u,-2*j,1,dj,cl,EPS);
	    
	        idx = toUnaliasedIdx(2*j,2*n+1);
	        for (i = 0; i < n+1; i++) {
	    		fu[0][i] = pp1[0][idx][i];
	    		fu[1][i] = pp1[1][idx][i];
	    	}
	        idx = toUnaliasedIdx(j,n+1);
	        for (i = 0; i < n+1; i++) {
	        	u[0][i] = fim[0][idx][i];
	        	u[1][i] = fim[1][idx][i];
	        }
	        for (i = 0; i < n; i++) {
		        dj[i]=d[2][n/2-j][i];
		        cl[i]=c[2][n/2-j][i];
	        }
	        r2 = recover(fu,u,2*j,3,dj,cl,EPS);
	    
	        // recover columns j and -j from pp2
	        idx = toUnaliasedIdx(-2*j,2*n+1);
	        for (i = 0; i < n+1; i++) {
	        	fv[0][i] = pp2[0][idx][i];
	        	fv[1][i] = pp2[1][idx][i];
	        }
	        idx = toUnaliasedIdx(-j,n+1);
	        for (i = 0; i < n+1; i++) {
	            v[0][i] = fim[0][i][idx];
	            v[1][i] = fim[1][i][idx];
	        }
	        for (i = 0; i < n; i++) {
	            dj[i]=d[1][n/2-j][i];
	            cl[i]=c[1][n/2-j][i];
	        }
	        c1 = recover(fv,v,-2*j,2,dj,cl,EPS);
	    
	        idx = toUnaliasedIdx(2*j,2*n+1);
	        for (i = 0; i < n+1; i++) {
	        	fv[0][i] = pp2[0][idx][i];
	        	fv[1][i] = pp2[1][idx][i];
	        }
	        idx = toUnaliasedIdx(j,n+1);
	        for (i = 0; i < n+1; i++) {
	            v[0][i] = fim[0][i][idx];
	            v[1][i] = fim[1][i][idx];
	        }
	        for (i = 0; i < n; i++) {
	            dj[i]=d[3][n/2-j][i];
	            cl[i]=c[3][n/2-j][i];
	        }
	        c2 = recover(fv,v,2*j,4,dj,cl,EPS);    
	    
	        // store the recovered rows and columns in the output array
	        idx = toUnaliasedIdx(-j,n+1);
	        for (i = 0; i < n; i++) {
	        	fim[0][idx][i] = r1[0][i];
	        	fim[1][idx][i] = r1[1][i];
	        }
	        idx = toUnaliasedIdx(j,n+1);
	        for (i = 0; i < n; i++) {
	        	fim[0][idx][i+1] = r2[0][i];
	        	fim[1][idx][i+1] = r2[1][i];
	        }
	        idx = toUnaliasedIdx(-j,n+1);
	        for (i = 0; i < n; i++) {
	        	fim[0][i+1][idx] = c1[0][i];
	        	fim[1][i+1][idx] = c1[1][i];
	        }
	        idx = toUnaliasedIdx(j,n+1);
	        for (i = 0; i < n; i++) {
	        	fim[0][i][idx] = c2[0][i];
	        	fim[1][i][idx] = c2[1][i];
	        }
	    } // for (j=n/2; j >= 1; j--)
	    
	    // recover row 0 (no need to recover column 0).
	    fim[0][toUnaliasedIdx(0,n+1)][toUnaliasedIdx(0,n+1)]=pp1[0][toUnaliasedIdx(0,2*n+1)][toUnaliasedIdx(0,n+1)];
	    fim[1][toUnaliasedIdx(0,n+1)][toUnaliasedIdx(0,n+1)]=pp1[1][toUnaliasedIdx(0,2*n+1)][toUnaliasedIdx(0,n+1)];
	
	    // recover the orignal image by inverting fim
	    double rim[][] =invDecimatedFreqs(fim);
	    double invertedrim[][] = new double[n][n];
	    for (i = 0; i < n; i++) {
	    	for (j = 0; j < n; j++) {
	    		invertedrim[i][j] = rim[n-1-i][j];
	    	}
	    }
	    return invertedrim;
    }


    private int checkInput(double pp1[][][], double pp2[][][]) {
	    // Check that the arrays pp1 and pp2 are properly structured. pp1 and pp2
	    // should be of size 2x(2n+1)x(n+1). If everything is ok with pp1 and pp2
	    // the function returns n.
	    
	    int s11 = pp1[0].length;
	    int s12 = pp1[0][0].length;
	    int s21 = pp2[0].length;
	    int s22 = pp2[0][0].length;
	    
	    if (s11 != s21) {
	    	System.err.println("In checkInput pp1[0].length != pp2[0].length");
	    	System.exit(0);
	    }
	    
	    if (s12 != s22) {
	    	System.err.println("In checkInput pp1[0][0].length != pp2[0][0].length");
	    	System.exit(0);
	    }
	    
	    if ((s11 % 2) != 1) {
	    	System.err.println("In checkInput pp1[0].length is not odd");
	    	System.exit(0);
	    }
	    
	    if ((s12 % 2) != 1) {
	    	System.err.println("In checkInput pp1[0][0].length is not odd");
	    	System.exit(0);
	    }
	
	    if (((s11 - 1)/2) != (s12 - 1)) {
	    	System.err.println("In checkInput input parameter is not of form (2n+1)x(n+1)");
	    	System.exit(0);
	    }
	    
	    int n = s12 - 1;
	    
	    return n;
    }

    private void loadConsts(double c[][][], double d[][][], int n) {
	    
	    // Try to load the constants from the disk. If the constants do not exist on
	    // the disk, compute them using a slow algorithm and print a warning.
    	int i,j,k;
    	RandomAccessFile raFile = null;
	    boolean load = true;
    	String outputFilePath = "C:" + File.separator + "PseudoPolarFourierTranformTables" + File.separator;
    	File file = new File(outputFilePath);
    	if (!file.exists()) {
			System.out.println("Warning outputFilePath directory for tables not found on disk");
			load = false;
		}
    	else {
    		String cname=outputFilePath + "c_" + String.valueOf(n);
    	    File filec = new File(cname);
    		if (!filec.exists()) {
    			System.out.println("Warning c_" + String.valueOf(n) + " does not exist on the disk");
    			load = false;
    		}
    		else {
    			try {
    				raFile = new RandomAccessFile(filec, "r");
    			} catch (IOException e) {
    				System.err.println(
    						"In loadConsts raFile = new RandomAccessFile(filec, \"r\") IOException " + e);
    				load = false;
    			}
    			for (i = 0; i < 4; i++) {
    				for (j = 0; j < n/2; j++) {
    					for (k = 0; k < n; k++) {
    					    try {
    					    	c[i][j][k] = raFile.readDouble();
    					    }
    					    catch (IOException e) {
    							System.err.println(
    									"In loadConsts raFile.readDouble IOException " + e);
    							load = false;
    					    }
    					}
    				}
    			}
    			try {
    				raFile.close();
    			}
    			catch (IOException e) {
    				System.err.println(
    						"In loadConsts raFile.close IOException " + e);
    				load = false;
    		    }	
    		}
    		
    		String dname=outputFilePath + "d_" + String.valueOf(n);
    	    File filed = new File(dname);
    		if (!filed.exists()) {
    			System.out.println("Warning d_" + String.valueOf(n) + " does not exist on the disk");
    			load = false;
    		}
    		else {
    			try {
    				raFile = new RandomAccessFile(filed, "r");
    			} catch (IOException e) {
    				System.err.println(
    						"In loadConsts raFile = new RandomAccessFile(filed, \"r\") IOException " + e);
    				load = false;
    			}
    			for (i = 0; i < 4; i++) {
    				for (j = 0; j < n/2; j++) {
    					for (k = 0; k < n; k++) {
    					    try {
    					    	d[i][j][k] = raFile.readDouble();
    					    }
    					    catch (IOException e) {
    							System.err.println(
    									"In loadConsts raFile.readDouble IOException " + e);
    							load = false;
    					    }
    					}
    				}
    			}
    			try {
    				raFile.close();
    			}
    			catch (IOException e) {
    				System.err.println(
    						"In loadConsts raFile.close IOException " + e);
    				load = false;
    		    }	
    		}
    	}
	
	    if (!load) {
	        System.out.println("Warning tables not found on disk. Generating inversion tables...");
	        ippftconsts(c,d,n);
	    }
    }

    private double[][] recover(double fu[][], double u[][], int j,int type, double dj[], double cl[], double EPS) {
	    // Respacing of the frequency samples to the cartesian grid.
	    // The vector fx contains samples fractional samples given by
	    //        n/2-1
	    // fu(l) =  sum x(u)exp(-2*pi*i*alpha*u*l/m) 
	    //        u=-n/2
	    // where alpha=-2j/n and l=-n/2...n/2.
	    
	    // u contains the samples of the Cartesian Fourier grid that was recovered 
	    // in previous iterations.
	    
	    // The function returns the vector y given by
	    //         n/2-1
	    // w(l) =  sum x(j)exp(-2*pi*i*(2*l)u/m).
	    //        u=-n/2
    	int n;
    	int m;
    	double alpha;
    	int i;
    	int idx1[];
    	int idx2[];
    	int idx3[];
    	double y[];
    	int array1[];
    	double x[];
    	double xs[];
    	double ys[];
    	int lx;
    	int ly;
    	double f[][];
	
	    n=fu[0].length-1;
	    m=2*n+1;
	    alpha=-2.0*j/n;
	
	    // Compute the frequencies in the vector of length N(k,n+1)+n+1. These
	    // frequencies are denoted as the vector y
	
	    if ((type==1) || (type==4)) {
	        idx1=createArray(-n/2,-n/2+N(j/2,n+1)/2-1);
	        idx3=createArray(n/2-N(j/2,n+1)/2+1,n/2-1);
	    }
	    else {
	        idx1=createArray(-n/2+1,-n/2+N(j/2,n+1)/2-1);
	        idx3=createArray(n/2-N(j/2,n+1)/2+1,n/2);
	    }
	
	    if (alpha==-2) {
	        if ((type==1) || (type==3)) {
	        	idx2 = createArray(-n/2+N(j/2,n+1)/2+1,n/2-N(j/2,n+1)/2);
	        	for (i = 0; i < idx2.length; i++) {
	        		idx2[i] = (int)Math.round(idx2[i]/(alpha/2.0));
	        	}
	        }
	        else {
	        	idx2 = createArray(-n/2+N(j/2,n+1)/2,n/2-N(j/2,n+1)/2-1);
	        	for (i = 0; i < idx2.length; i++) {
	        		idx2[i] = (int)Math.round(idx2[i]/(alpha/2.0));
	        	}
	        }
	    }
	    else if (alpha==2) {
	        // Handling types 2 and 4 is different than type 1 and 3 because of the
	        // flip (reverse angle order) in pp2.
	        if ((type==1) || (type==3)) {
	        	idx2 = createArray(-n/2+N(j/2,n+1)/2,n/2-N(j/2,n+1)/2-1);
	        	for (i = 0; i < idx2.length; i++) {
	        		idx2[i] = (int)Math.round(idx2[i]/(alpha/2.0));
	        	}
	        }
	        else {
	        	idx2 = createArray(-n/2+N(j/2,n+1)/2+1,n/2-N(j/2,n+1)/2);
	        	for (i = 0; i < idx2.length; i++) {
	        		idx2[i] = (int)Math.round(idx2[i]/(alpha/2.0));
	        	}
	        }
	    }
	    else if (alpha !=0) {
	    	idx2 = createArray(-n/2+N(j/2,n+1)/2,n/2-N(j/2,n+1)/2);
	    	for (i = 0; i < idx2.length; i++) {
        		idx2[i] = (int)Math.round(idx2[i]/(alpha/2.0));
        	}
	    }
	    else {
	        idx2= new int[] {0};
	    }
	    
	    y = new double[idx1.length + idx2.length + idx3.length];
		
		for (i = 0; i < idx1.length; i++) {
			y[i] = -4.0*Math.PI*idx1[i]/m;
		}
		for (i = 0; i < idx2.length; i++) {
			y[i + idx1.length] = -2.0*Math.PI*alpha*idx2[i]/m;
		}
		for (i = 0; i < idx3.length; i++) {
			y[i + idx1.length + idx2.length] = -4.0*Math.PI*idx3[i]/m;
		}
	
	    // Compute the frequencies on the Cartesian grid.
		x = new double[n];
	    if ((type==1) || (type==4)) {
	    	array1 = createArray(-n/2,n/2-1);
	    	for (i = 0; i < n; i++) {
	    		x[i] = -4.0*Math.PI*array1[i]/m;
	    	}
	    }
	    else {
	    	array1 = createArray(-n/2+1,n/2);
	    	for (i = 0; i < n; i++) {
	    		x[i] = -4.0*Math.PI*array1[i]/m;
	    	}
	    }
	    
	    lx=x.length;
	    ly=y.length;
	    
	    if (lx!=ly) {
	        System.out.println("Warning in recover x and y are not of the same length");
	    }
	
	    // Correspondence test of x and y. 
	    // Used for validation of the code.
	    // Remove in the final version.
	    xs = new double[x.length];
	    for (i = 0; i < x.length; i++) {
	    	xs[i] = x[i];
	    }
	    Arrays.sort(xs);
	    ys = new double[y.length];
	    for (i = 0; i < y.length; i++) {
	    	ys[i] = y[i];
	    }
	    Arrays.sort(ys);
	    
	
	    if (xs[0] !=ys[0]) {
	        System.out.println("Warning in recover first element in x and y are different");
	    }
	    if (xs[lx-1] != ys[lx-1]) {
	        System.out.println("Warning in recover last element in x and y are different");
	    }
	    // END of validation code
	    f = new double[2][idx1.length + idx2.length + idx3.length];
	    for (i = 0; i < idx1.length; i++) {
	    	f[0][i] = u[0][idx1[i]+n/2];
	    	f[1][i] = u[1][idx1[i]+n/2];
	    }
	    for (i = 0; i < idx2.length; i++) {
	    	f[0][i+idx1.length] = fu[0][idx2[i]+n/2];
	    	f[1][i+idx1.length] = fu[1][idx2[i]+n/2];
	    }
	    for (i = 0; i < idx3.length; i++) {
	    	f[0][i+idx1.length+idx2.length] = u[0][idx3[i]+n/2];
	    	f[1][i+idx1.length+idx2.length] = u[1][idx3[i]+n/2];
	    }
	    double w[][]=fastfmmresample(f,y,x,dj,cl,EPS);
	    return w;
    }
    
    private double[][] adjGKN(double w[][], int k) {
    		
    		// Computes the adjoint of the operator GKN for the vector w and the row k.
    		// For a vector w of length n:
    		// 		GKN(w) = U_{2n+1,n+1}(F^{2k/n}_{2n+1}(E_{n,2n+1}(F^{-1}(w)))) 
    		// Hence,
    		// 		adjGKN = adj F^{-1} \circ adj E_{n,2n+1} \circ adj F^{2k/n}_{2n+1} \circ adj U_{2n+1,n+1}
    		// (where n is the length of the vector w and not of the input to adjGKN)
    		
    		// w    The sequence to resample. Can be of odd or even length.
    		// k    The row whose transform is computed.
    		
    		// Returns the adjoint of GKN for the sequence x and the row k.
    		// See thesis' final version for more information.
    		
    		// See Also GKN.
    		
    		// Yoel Shkolnisky 22/10/01

    		int n = w[0].length-1;
    		double v[][] = adjInvF(adjE(adjCfrft(adjU(w),2.0*k/n)));
    		return v;
    }

    
    private double[][] adjU(double v[][]) {
            int i;
    		int n = v[0].length-1;
    		double z[][] = new double[2][2*n+1];
    		for (i = 0; i < n+1; i++) {
    			z[0][i+n/2] = v[0][i];
    			z[1][i+n/2] = v[1][i];
    		}
    		return z;
    }

    private double[][] adjE(double v[][]) {
    	    int i;
    		int n = (v[0].length-1)/2;
    		double z[][] = new double[2][n];
    		for (i = 0; i < n; i++) {
    			z[0][i] = v[0][i+n/2];
    			z[1][i] = v[1][i+n/2];
    		}
    		return z;
    }

    private double[][] adjCfrft(double x[][], double alpha) {
    		double z[][] = cfrft(x,-alpha);
    		return z;
    }

     private double[][] adjInvF(double v[][]) {
    	    int i;
    		int n= v[0].length;
    		double z[][] = cfft(v);
    		for (i = 0; i < z[0].length; i++) {
    			z[0][i] = z[0][i]/n;
    			z[1][i] = z[1][i]/n;
    		}
    		return z;
     }
     
     private double[][][] adjPPFT(double pp1[][][], double pp2[][][]) { 
    		 
    		 // Computes the adjoint of the pseudo-polar Fourier transform.
    		 
    		 // pp1,pp2   The pseudo-polar sections resulted from the PPFT (pseudo-polar Fourier transform).
    		 //           pp1 and pp2 must be of size (2n+1)x(n+1) as results from PPFT.
    		 
    		 // See also PPFT.
    		 
    		 // Yoel Shkolnisky 9/2/02

             int i, j, k;
             double u[][];
             double v[][];
             int idx;
	    	 int s11 = pp1[0].length;
	 	     int s12 = pp1[0][0].length;
	 	     int s21 = pp2[0].length;
	 	     int s22 = pp2[0][0].length;
	 	    
	 	     if (s11 != s21) {
	 	    	 System.err.println("In adjPPFT pp1[0].length != pp2[0].length");
	 	    	 System.exit(0);
	 	     }
	 	    
	 	     if (s12 != s22) {
	 	    	 System.err.println("In adjPPFT pp1[0][0].length != pp2[0][0].length");
	 	    	 System.exit(0);
	 	     }
	 	    
	 	     if ((s11 % 2) != 1) {
	 	    	 System.err.println("In adjPPFT pp1[0].length is not odd");
	 	    	 System.exit(0);
	 	     }
	 	    
	 	     if ((s12 % 2) != 1) {
	 	    	 System.err.println("In adjPPFT pp1[0][0].length is not odd");
	 	    	 System.exit(0);
	 	     }
	 	
	 	     if (((s11 - 1)/2) != (s12 - 1)) {
	 	    	 System.err.println("In adjPPFT input parameter is not of form (2n+1)x(n+1)");
	 	    	 System.exit(0);
	 	     }
	 	    
	 	     int n = s12 - 1;

    		 double adjpp1[][][] = new double[2][n][n];
    		 double adjpp2[][][] = new double[2][n][n];

    		 // Compute the adjoint of PP1
    		 double tmp[][][] = new double[2][2*n+1][n];
    		 u = new double[2][n+1];
    		 for (k = -n; k <= n; k++) {
    			idx = toUnaliasedIdx(k,2*n+1);
    			for (i = 0; i < n+1; i++) {
    				u[0][i] = pp1[0][idx][n-i];
    				u[1][i] = pp1[1][idx][n-i];
    			}
    		    v = adjGKN(u,k);
    		    for (i = 0; i < n; i++) {
    		    	tmp[0][idx][i] = v[0][i];
    		    	tmp[1][idx][i] = v[1][i];
    		    }
    		 }
    		 tmp = icfft2(tmp);
    		 for (i = 0; i < 2*n+1; i++) {
    		     for (j = 0; j < n; j++) {
    		    	 tmp[0][i][j] = n*(2*n+1)*tmp[0][i][j];
    		    	 tmp[1][i][j] = n*(2*n+1)*tmp[1][i][j];
    		     }
    		 }
    		 for (i = 0; i < n; i++) {
    			 for (j = 0; j < n; j++) {
    				 adjpp1[0][i][j] = tmp[0][3*n/2 - 1 - i][j];
    				 adjpp1[1][i][j] = tmp[1][3*n/2 - 1 - i][j];
    			 }
    		 }

    		 // Compute the adjoint of PP2
    		 tmp = new double[2][n][2*n+1];
    		 for (k=-n; k <= n; k++) {
    			idx = toUnaliasedIdx(k,2*n+1);
    			for (i = 0; i < n+1; i++) {
    				u[0][i] = pp2[0][idx][n-i];
    				u[1][i] = pp2[1][idx][n-i];
    			}
    		    v = adjGKN(u,k);
    		    for (i = 0; i < n; i++) {
    		    	tmp[0][i][idx] = v[0][i];
    		    	tmp[1][i][idx] = v[1][i];
    		    }
    		 }
    		 tmp = icfft2(tmp);
    		 for (i = 0; i < n; i++) {
    		     for (j = 0; j < 2*n+1; j++) {
    		    	 tmp[0][i][j] = n*(2*n+1)*tmp[0][i][j];
    		    	 tmp[1][i][j] = n*(2*n+1)*tmp[1][i][j];
    		     }
    		 }
    		 for (i = 0; i < n; i++) {
    			 for (j = 0; j < n; j++) {
    				 adjpp2[0][i][j] = tmp[0][n - 1 - i][j + n/2];
    				 adjpp2[1][i][j] = tmp[1][n - 1 - i][j + n/2];
    			 }
    		 }

    		 // Combine both adjoints
    		 double im[][][] = new double[2][n][n];
    		 for (i = 0; i < n; i++) {
    			 for (j = 0; j < n; j++) {
    				 im[0][i][j] = adjpp1[0][i][j] + adjpp2[0][i][j];
    				 im[1][i][j] = adjpp1[1][i][j] + adjpp2[1][i][j];
    			 }
    		 }
    		 return im;
     }
     
     private double[][][] PrecondAdjPPFT(double pp1[][][], double pp2[][][]) { 
    		 
    		 // Computes the preconditioned adjoint of the pseudo-polar Fourier transform.
    		 
    		 // pp1,pp2  The pseudo-polar sections.
    		 //          pp1 and pp2 must be of size (2n+1)x(n+1) as results from PPFT.
    		 
    		 // Returns the image (im) that is the preconditioned adjoint of the PPFT.
    		 // See differences between this function and the function optimizedadjppft.m
    		 // to see how the preconditioner is defined.
    		 // See precond.m for an explicit form of the preconditioner.
    		 
    		 // Yoel Shkolnisky 17/12/02
    	 
	    	 int i, j, k, m;
	    	 double mSquared;
	    	 double alpha;
	    	 double mult;
	         double u[][];
	         double v[][];
	         int idx;
	         
	         // Check if the input is of valid size 2x(2n+1)x(n+1)
	    	 int s11 = pp1[0].length;
	 	     int s12 = pp1[0][0].length;
	 	     int s21 = pp2[0].length;
	 	     int s22 = pp2[0][0].length;
	 	    
	 	     if (s11 != s21) {
	 	    	 System.err.println("In PrecondAdjPPFT pp1[0].length != pp2[0].length");
	 	    	 System.exit(0);
	 	     }
	 	    
	 	     if (s12 != s22) {
	 	    	 System.err.println("In PrecondAdjPPFT pp1[0][0].length != pp2[0][0].length");
	 	    	 System.exit(0);
	 	     }
	 	    
	 	     if ((s11 % 2) != 1) {
	 	    	 System.err.println("In PrecondAdjPPFT pp1[0].length is not odd");
	 	    	 System.exit(0);
	 	     }	
	 	    
	 	     if ((s12 % 2) != 1) {
	 	    	 System.err.println("In PrecondAdjPPFT pp1[0][0].length is not odd");
	 	    	 System.exit(0);
	 	     }
	 	
	 	     if (((s11 - 1)/2) != (s12 - 1)) {
	 	    	 System.err.println("In PrecondAdjPPFT input parameter is not of form (2n+1)x(n+1)");
	 	    	 System.exit(0);
	 	     }
	 	    
	 	     int n = (s11 - 1)/2;

    		 m=2*n+1;
    		 alpha = 2.0*(n+1.0)/(n*m);
    		 mSquared = (double)(m*m);

    		 // Compute the adjoint of PP1
    		 double tmp[][][] = new double[2][2*n+1][n];
    		 u = new double[2][n+1];
    		 for (k=-n; k <= n; k++) {
    		    if (k==0) {
    		       mult = 1.0/mSquared;
    		    }
    		    else {
    		       mult = Math.abs(k*alpha);
    		    }
    		    idx = toUnaliasedIdx(k,2*n+1);
    		    
    		    for (i = 0; i < n+1; i++) {
    		    	u[0][i] = pp1[0][idx][n-i];
    		    	u[1][i] = pp1[1][idx][n-i];
    		    }
    		    v = cfrft(u,-k*alpha);
    		    for (i = 0; i < n; i++) {
    		    	v[0][i] = mult*v[0][i];
    		    	v[1][i] = mult*v[1][i];
    		    	tmp[0][idx][i] = v[0][i];
    		    	tmp[1][idx][i] = v[1][i];
    		    }
    		 } // for (k=-n; k <= n; k++)

    		 // Inverse FFT along columns
    		 double tmpC[][];
    	     for (j = 0; j < n; j++) {
    	    	 tmpC = new double[2][2*n+1];
    	    	 for (i = 0; i < 2*n+1; i++) {
    	    		 tmpC[0][i] = tmp[0][i][j];
    	    		 tmpC[1][i] = tmp[1][i][j];
    	    	 }
    	    	 tmpC = ifftshift1d(tmpC);
    	    	 FFTUtility fft = new FFTUtility(tmpC[0], tmpC[1], 1, 2*n+1, 1, 1, FFTUtility.FFT);
    		     fft.setShowProgress(false);
    		     fft.run();
    		     fft.finalize();
    		     fft = null;
    		     tmpC = fftshift1d(tmpC);
    	    	 for (i = 0; i < 2*n+1; i++) {
    	    		 tmp[0][i][j] = m*tmpC[0][i];
    	    		 tmp[1][i][j] = m*tmpC[1][i];
    	    	 }
    	     }
            
    	     double adjpp1[][][] = new double[2][n][n];
             for (i = 0; i < n; i++) {
    			 for (j = 0; j < n; j++) {
    				 adjpp1[0][i][j] = tmp[0][3*n/2 - 1 - i][j];
    				 adjpp1[1][i][j] = tmp[1][3*n/2 - 1 - i][j];
    			 }
    		 }

    		 // Compute the adjoint of PP2

             tmp = new double[2][2*n+1][n];
             u = new double[2][n+1];
             for (k=-n; k <= n; k++) {
     		    if (k==0) {
     		       mult = 1.0/mSquared;
     		    }
     		    else {
     		       mult = Math.abs(k*alpha);
     		    }
     		    idx = toUnaliasedIdx(k,2*n+1);
     		    
     		    for (i = 0; i < n+1; i++) {
     		    	u[0][i] = pp2[0][idx][n-i];
     		    	u[1][i] = pp2[1][idx][n-i];
     		    }
     		    v = cfrft(u,-k*alpha);
     		    for (i = 0; i < n; i++) {
     		    	v[0][i] = mult*v[0][i];
     		    	v[1][i] = mult*v[1][i];
     		    	tmp[0][idx][i] = v[0][i];
     		    	tmp[1][idx][i] = v[1][i];
     		    }
     		 } // for (k=-n; k <= n; k++)
    		 
    		 // To follow the code in adjPPFT we should have transposed each row before we assign it to tmp 
    		 // (and creating an array of size nx(2n+1)). Then we had to apply cfft along the rows.
    		 // To save operations, we assign the vector v to tmp without transpose, apply cfft along columns
    		 // and then transpose the entire matrix at once.

             // Inverse FFT along columns
    	     for (j = 0; j < n; j++) {
    	    	 tmpC = new double[2][2*n+1];
    	    	 for (i = 0; i < 2*n+1; i++) {
    	    		 tmpC[0][i] = tmp[0][i][j];
    	    		 tmpC[1][i] = tmp[1][i][j];
    	    	 }
    	    	 tmpC = ifftshift1d(tmpC);
    	    	 FFTUtility fft = new FFTUtility(tmpC[0], tmpC[1], 1, 2*n+1, 1, 1, FFTUtility.FFT);
    		     fft.setShowProgress(false);
    		     fft.run();
    		     fft.finalize();
    		     fft = null;
    		     tmpC = fftshift1d(tmpC);
    	    	 for (i = 0; i < 2*n+1; i++) {
    	    		 tmp[0][i][j] = m*tmpC[0][i];
    	    		 tmp[1][i][j] = m*tmpC[1][i];
    	    	 }
    	     }
    	     
    	     double tmpTranspose[][][] = new double[2][n][2*n+1];
             for (i = 0; i < 2*n+1; i++) {
            	 for (j = 0; j < n; j++) {
            		 tmpTranspose[0][j][i] = tmp[0][i][j];
            		 tmpTranspose[1][j][i] = tmp[1][i][j];
            	 }
             }

             double adjpp2[][][] = new double[2][n][n];
             for (i = 0; i < n; i++) {
            	 for (j = 0; j < n; j++) {
            		 adjpp2[0][i][j] = tmpTranspose[0][n-1-i][j+n/2]; 
            		 adjpp2[1][i][j] = tmpTranspose[1][n-1-i][j+n/2];
            	 }
             }
            

    		 // Combine both adjoints
             double im[][][] = new double[2][n][n];
             for (i = 0; i < n; i++) {
            	 for (j = 0; j < n; j++) {
            		 im[0][i][j] = (adjpp1[0][i][j] + adjpp2[0][i][j])/mSquared;
            		 im[1][i][j] = (adjpp1[1][i][j] + adjpp2[1][i][j])/mSquared;
            	 }
             }
             return im;

    		 // Revision record
    		 // 15/1/03   Yoel Shkolnisky     Use cfftd instead of column-wise cfft
     }

     
     private double[][][] OptimizedAdjPPFT(double pp1[][][], double pp2[][][]) {
    		 
		 // Optimized version of adjppft, the adjoint operator of the
		 // pseudo-polar Fourier transform operator.
		 
		 // See adjppft.m for more information.
		 
		 // Yoel Shkolnisky 22/10/01
    	 
    	 int i, j, k;
    	 double v[][];
    	 int idx;
    	 int s11 = pp1[0].length;
 	     int s12 = pp1[0][0].length;
 	     int s21 = pp2[0].length;
 	     int s22 = pp2[0][0].length;
 	    
 	     if (s11 != s21) {
 	    	 System.err.println("In OptimizedAdjPPFT pp1[0].length != pp2[0].length");
 	    	 System.exit(0);
 	     }
 	    
 	     if (s12 != s22) {
 	    	 System.err.println("In OptimizedAdjPPFT pp1[0][0].length != pp2[0][0].length");
 	    	 System.exit(0);
 	     }
 	    
 	     if ((s11 % 2) != 1) {
 	    	 System.err.println("In OptimizedAdjPPFT pp1[0].length is not odd");
 	    	 System.exit(0);
 	     }
 	    
 	     if ((s12 % 2) != 1) {
 	    	 System.err.println("In OptimizedAdjPPFT pp1[0][0].length is not odd");
 	    	 System.exit(0);
 	     }
 	
 	     if (((s11 - 1)/2) != (s12 - 1)) {
 	    	 System.err.println("In OptimizedAdjPPFT input parameter is not of form (2n+1)x(n+1)");
 	    	 System.exit(0);
 	     }
 	    
 	     int n = (s11 - 1)/2;
         int m = 2*n+1;
         double alpha = 2.0*(n+1.0)/(n*m);

         // Compute the adjoint of PP1
         double tmp[][] = new double[2][(2*n+1)*n];
         double u[][] = new double[2][n+1];
         for (k=-n; k <= n; k++) {
        	    idx = toUnaliasedIdx(k,2*n+1);
        	    for (i = 0; i < n+1; i++) {
    				u[0][i] = pp1[0][idx][n-i];
    				u[1][i] = pp1[1][idx][n-i];
    			}
    		    v = cfrft(u,-k*alpha);
    		    for (i = 0; i < n; i++) {
    		    	tmp[0][idx*n+i] = v[0][i];
    		    	tmp[1][idx*n+i] = v[1][i];
    		    }
         }
         
         // Inverse FFT on first dimension
         double tmpC[][];
	     for (j = 0; j < n; j++) {
	    	 tmpC = new double[2][2*n+1];
	    	 for (i = 0; i < 2*n+1; i++) {
	    		 tmpC[0][i] = tmp[0][i*n+j];
	    		 tmpC[1][i] = tmp[1][i*n+j];
	    	 }
	    	 tmpC = ifftshift1d(tmpC);
	    	 FFTUtility fft = new FFTUtility(tmpC[0], tmpC[1], 1, 2*n+1, 1, 1, FFTUtility.FFT);
		     fft.setShowProgress(false);
		     fft.run();
		     fft.finalize();
		     fft = null;
		     tmpC = fftshift1d(tmpC);
	    	 for (i = 0; i < 2*n+1; i++) {
	    		 tmp[0][i*n + j] = tmpC[0][i];
	    		 tmp[1][i*n + j] = tmpC[1][i];
	    	 }
	     }
        
         for (i = 0; i < 2*n+1; i++) {
        	 for (j = 0; j < n; j++) {
        		 tmp[0][i*n + j] = m*tmp[0][i*n + j];
        		 tmp[1][i*n + j] = m*tmp[1][i*n + j];
        	 }
         }
         double adjpp1[][][] = new double[2][n][n];
         for (i = 0; i < n; i++) {
			 for (j = 0; j < n; j++) {
				 adjpp1[0][i][j] = tmp[0][(3*n/2 - 1 - i)*n + j];
				 adjpp1[1][i][j] = tmp[1][(3*n/2 - 1 - i)*n + j];
			 }
		 }

         // Compute the adjoint of PP2
         tmp = new double[2][(2*n+1)*n];
    	 for (k=-n; k <= n; k++) {
    		 idx = toUnaliasedIdx(k,2*n+1);
    		 for (i = 0; i < n+1; i++) {
 				u[0][i] = pp2[0][idx][n-i];
 				u[1][i] = pp2[1][idx][n-i];
 			 }
    		 v = cfrft(u,-k*alpha);
    		 for (i = 0; i < n; i++) {
    			 tmp[0][idx*n + i] = v[0][i];
    			 tmp[1][idx*n + i] = v[1][i];
    		 }
    	 }
         // To follow the code in adjPPFT we should have transposed each row before we assign it to tmp 
         // (and creating an array of size nx(2n+1)). Then we had to apply cfft along the rows.
    	 // To save operations, we assign the vector v to tmp without transpose, apply cfft along columns
    	 // and then transpose the entire matrix at once.
    	 
    	 // Inverse FFT on first dimension
    	 for (j = 0; j < n; j++) {
	    	 tmpC = new double[2][2*n+1];
	    	 for (i = 0; i < 2*n+1; i++) {
	    		 tmpC[0][i] = tmp[0][i*n+j];
	    		 tmpC[1][i] = tmp[1][i*n+j];
	    	 }
	    	 tmpC = ifftshift1d(tmpC);
	    	 FFTUtility fft = new FFTUtility(tmpC[0], tmpC[1], 1, 2*n+1, 1, 1, FFTUtility.FFT);
		     fft.setShowProgress(false);
		     fft.run();
		     fft.finalize();
		     fft = null;
		     tmpC = fftshift1d(tmpC);
	    	 for (i = 0; i < 2*n+1; i++) {
	    		 tmp[0][i*n + j] = tmpC[0][i];
	    		 tmp[1][i*n + j] = tmpC[1][i];
	    	 }
	     }
         
         for (i = 0; i < 2*n+1; i++) {
        	 for (j = 0; j < n; j++) {
        		 tmp[0][i*n + j] = m*tmp[0][i*n + j];
        		 tmp[1][i*n + j] = m*tmp[1][i*n + j];
        	 }
         }
         
         double tmpTranspose[][][] = new double[2][n][2*n+1];
         for (i = 0; i < 2*n+1; i++) {
        	 for (j = 0; j < n; j++) {
        		 tmpTranspose[0][j][i] = tmp[0][i*n + j];
        		 tmpTranspose[1][j][i] = tmp[1][i*n + j];
        	 }
         }

         double adjpp2[][][] = new double[2][n][n];
         for (i = 0; i < n; i++) {
        	 for (j = 0; j < n; j++) {
        		 adjpp2[0][i][j] = tmpTranspose[0][n-1-i][j+n/2]; 
        		 adjpp2[1][i][j] = tmpTranspose[1][n-1-i][j+n/2];
        	 }
         }

    	 // Combine both adjoints
         double im[][][] = new double[2][n][n];
         for (i = 0; i < n; i++) {
        	 for (j = 0; j < n; j++) {
        		 im[0][i][j] = adjpp1[0][i][j] + adjpp2[0][i][j];
        		 im[1][i][j] = adjpp1[1][i][j] + adjpp2[1][i][j];
        	 }
         }
         return im;
     }
     
     private double[][] adjRadon(double r1[][], double r2[][]) { 
    		 
    		 // Computes the adjoint 2-D discrete Radon transform.
    		 
    		 // r1,r2    The sections of the discrete Radon transform whose adjoint should be computed.
    		 //          r1 and r2 must be of size (2n+1)x(n+1) as results from the function Radon.
    		 
    		 // See also Radon, adjPPFT.
    		 
    		 // Yoel Shkolnisky 9/2/02

    		 int i,j;
    	     // Check if the input is of valid size 2x(2n+1)x(n+1)
	    	 int s11 = r1.length;
	 	     int s12 = r1[0].length;
	 	     int s21 = r2.length;
	 	     int s22 = r2[0].length;
	 	    
	 	     if (s11 != s21) {
	 	    	 System.err.println("In adjRadon r1.length != r2.length");
	 	    	 System.exit(0);
	 	     }
	 	    
	 	     if (s12 != s22) {
	 	    	 System.err.println("In adjRadon r1[0].length != r2[0].length");
	 	    	 System.exit(0);
	 	     }
	 	    
	 	     if ((s11 % 2) != 1) {
	 	    	 System.err.println("In adjRadon r1.length is not odd");
	 	    	 System.exit(0);
	 	     }
	 	    
	 	     if ((s12 % 2) != 1) {
	 	    	 System.err.println("In adjRadon r1[0].length is not odd");
	 	    	 System.exit(0);
	 	     }
	 	
	 	     if (((s11 - 1)/2) != (s12 - 1)) {
	 	    	 System.err.println("In adjRadon input parameter is not of form (2n+1)x(n+1)");
	 	    	 System.exit(0);
	 	     }
	 	    
	 	     int n = s11; // n - number of rows
	 	     double ppp1[][][] = new double[2][s11][s12];
             double ppp2[][][] = new double[2][s11][s12];
             double pp1[][];
             double pp2[][];
             for (j = 0; j < s12; j++) {
    	    	 pp1 = new double[2][s11];
    	    	 pp2 = new double[2][s11];
    	    	 for (i = 0; i < s11; i++) {
    	    		 pp1[0][i] = r1[i][j];
    	    		 pp1[1][i] = 0.0;
    	    		 pp2[0][i] = r2[i][j];
    	    		 pp2[1][i] = 0.0;
    	    	 }
    	    	 pp1 = ifftshift1d(pp1);
    	    	 pp2 = ifftshift1d(pp2);
    	    	 FFTUtility fft = new FFTUtility(pp1[0], pp1[1], 1, s11, 1, -1, FFTUtility.FFT);
    		     fft.setShowProgress(false);
    		     fft.run();
    		     fft.finalize();
    		     fft = null;
    		     fft = new FFTUtility(pp2[0], pp2[1], 1, s11, 1, -1, FFTUtility.FFT);
    		     fft.setShowProgress(false);
    		     fft.run();
    		     fft.finalize();
    		     fft = null;
    		     pp1 = fftshift1d(pp1);
    		     pp2 = fftshift1d(pp2);
    	    	 for (i = 0; i < s11; i++) {
    	    		 ppp1[0][i][j] = pp1[0][i]/n;
    	    		 ppp1[1][i][j] = pp1[1][i]/n;
    	    		 ppp2[0][i][j] = pp2[0][i]/n;
    	    		 ppp2[1][i][j] = pp2[1][i]/n;
    	    	 }
    	     }
             
    		 double imComplex[][][] = OptimizedAdjPPFT(ppp1,ppp2);
    		 return imComplex[0];

    		 // Revision record
    		 // 15/1/03	Yoel Shkolnisky		Used cfftd instead of column-wise cfft
     }
     
    private double ip(double a[][], double b[][]) {
	     
	     // Inner product of 2 multi-dimensional arrays.
	     // For two matrices the inner product is given by
	     
	     //         N1   N2
	     //    c = sum (sum a(k,l)*conj(b(k,l))
	     //        k=1  l=1 
	     
	     // Yoel Shkolnisky 9/2/02
	
	     // c = sum(conj(a(:)).*b(:));
    	 // Note comment had conj(b) but code has conj(a).
    	 int N1 = a.length;
    	 int N2 = a[0].length;
    	 int B1 = b.length;
    	 int B2 = b[0].length;
    	 if (N1 != B1) {
    		 System.err.println("In ip a.length != b.length");
    		 return Double.NaN;
    	 }
    	 if (N2 != B2) {
    		 System.err.println("In ip a[0].length != b[0].length");
    		 return Double.NaN;
    	 }
    	 int i,j;
    	 double sum = 0.0;
    	 for (i = 0; i < N1; i++) {
    		 for (j = 0; j < N2; j++) {
    			 sum += a[i][j]*b[i][j];
    		 }
    	 }
    	 return sum;
     }
     
     private double[] ip(double a[][][], double b[][][]) {
	     
	     // Inner product of 2 multi-dimensional arrays.
	     // For two matrices the inner product is given by
	     
	     //         N1   N2
	     //    c = sum (sum a(k,l)*conj(b(k,l))
	     //        k=1  l=1 
	     
	     // Yoel Shkolnisky 9/2/02
	
	     // c = sum(conj(a(:)).*b(:));
    	 // Note comment had conj(b) but code has conj(a).
    	 int N1 = a[0].length;
    	 int N2 = a[0][0].length;
    	 int B1 = b[0].length;
    	 int B2 = b[0][0].length;
    	 if (N1 != B1) {
    		 System.err.println("In ip a[0].length != b[0].length");
    		 return null;
    	 }
    	 if (N2 != B2) {
    		 System.err.println("In ip a[0][0].length != b[0][0].length");
    		 return null;
    	 }
    	 int i,j;
    	 double sum[] = new double[2];
    	 for (i = 0; i < N1; i++) {
    		 for (j = 0; j < N2; j++) {
    			 sum[0] += (a[0][i][j]*b[0][i][j] + a[1][i][j]*b[1][i][j]);
    			 sum[1] += (a[0][i][j]*b[1][i][j] - a[1][i][j]*b[0][i][j]);
    		 }
    	 }
    	 return sum;
     }

     
     private double[][] GKN(double x[][], int k) {
    		 
    		 // Application of the operator G(K,N) to the sequence x (of length n).
    		 // The operator GKN is defined as follows:
    		 //       1. Apply the inverse Fourier transform to the sequence x.
    		 //       2. Pad the resulting sequence to length 2n+1.
    		 //       3. Apply the fractional Fourier transform with alpha=2k/n.
    		 //       4. Return the n+1 central elements.
    		 
    		 // x   The sequence to resample using the operator GKN. Can be of odd or even
    		 //     length. Must be a 1-D row vector.
    		 // k   The row whose transform is being computed.
    		 
    		 // Returns the result of the application of GKN to the sequence x.
    		 // See thesis' final version for more information.
    		 
    		 // See Also adjGKN.
    		 
    		 // Yoel Shkolnisky 22/10/01
             int i;

    		 int n= x[0].length;
    		 if ((n % 2)==1) {
    		    System.err.println("In GKN input sequence must of even length");
    		    System.exit(0);
    		 }
    		    
    		 double w[][] = icfft(x);
    		 double wpad[][] = new double[2][w[0].length+n+1];
    		 for (i = 0; i < w[0].length; i++) {
    			 wpad[0][i+n/2] = w[0][i];
    			 wpad[1][i+n/2] = w[1][i];
    		 }
    		 wpad = cfrft(wpad,2.0*k/n);  // optimization - use alpha=2k/m instead of padding
    		 double y[][] = new double[2][n+1];
    		 for (i = 0; i < n+1; i++) {
    			 y[0][i] = wpad[0][i+n/2];
    			 y[1][i] = wpad[1][i+n/2];
    		 }
    		 return y;
     }
     
     private double[][] GKN3(double x[][], int k) {
    		 
    		 // Application of the operator G(K,N) to the sequence x (of length n).
    		 // The operator GKN in 3-D is defined as follows:
    		 //       1. Apply the inverse Fourier transform to the sequence x.
    		 //       2. Pad the resulting sequence to length 3n+1.
    		 //       3. Apply the fractional Fourier transform with alpha=2k/n.
    		 //       4. Return the n+1 central elements.
    		 
    		 // x  The sequence to resample using the operator GKN. Can be of odd or even length.
    		 // k  The row whose transform is being computed.
    		 
    		 // Yoel Shkolnisky 30/01/03

	    	 int i;
	
			 int n= x[0].length;
			 if ((n % 2)==1) {
			    System.err.println("In GKN3 input sequence must of even length");
			    System.exit(0);
			 }
			 
			 double w[][] = icfft(x);
    		 double wpad[][] = new double[2][w[0].length+2*n+1];
    		 for (i = 0; i < w[0].length; i++) {
    			 wpad[0][i+n] = w[0][i];
    			 wpad[1][i+n] = w[1][i];
    		 }
    		 wpad = cfrft(wpad,2.0*k/n);  // optimization - use alpha=2k/m instead of padding
    		 // return n+1 central elements
    		 double y[][] = new double[2][n+1];
    		 for (i = 0; i < n+1; i++) {
    			 y[0][i] = wpad[0][i+n];
    			 y[1][i] = wpad[1][i+n];
    		 }
    		 return y;
     }

     
     private void PPFT(double res1[][][], double res2[][][], double im[][][]) {
     
	     // Fast algorithm for computing the pseudo-polar Fourier transform.
	     // The computation requires O(n^2logn) operations.
	     
	     // im    The image whose pseudo-polar Fourier transform should be computed.
	     //       Must of a dyadic square size (2^k x 2^k).  im is size n x n.
	     
	     // Returns res1 and res2 (of size 2n+1xn+1) that contain the pseudo-polar Fourier 
	     // transform of the input image im.  res1 and res2 are 2x2n+1xn+1
	     // res1 contains the values of PPI1(k,l). res2 contains the values of PPI2(k,l). 
	     // The first argument of Res1 and Res2 corresponds to pseudo-radius and the second argument 
	     // corresponds to pseudo-angle.
	     // Angle ordering:
	     //       res1(k,l) - pseudo-polar fourier transform which corresponds to radius "k" and angle
	     //                   arctan(2l/n). "l" goes from -n/2 to n/2, and therefore, for a constant "k",
	     //                   res1 corresponds to angles from -pi/4 to pi/4.
	     //       res2(k,l) - pseudo-polar fourier transform which corresponds to radius "k" and angle
	     //                   pi/2-arctan(2l/n). "l" goes from -n/2 to n/2, and therefore, for a constant "k",
	     //                   res1 corresponds to angles from 3pi/4 to pi/4.
	     // To obtains a continuous change in angle from res1 to res2, one must flip res2 such that it
	     // corresponds to angles from pi/4 to 3pi/4 (and therefore continuous the angles in res1).
	     
	     //     3pi/4   pi/4
	     //        \    /
	     //         \  /  ^
	     //          \/   |
	     //          /\   |  angle change in res1 
	     //         /  \  |
	     //        /    \
	     //             -pi/4
	     
	     
	     
	     //       -------> angle change in res2
	     //     3pi/4   pi/4
	     //        \    /
	     //         \  /  
	     //          \/   
	     //          /\    
	     //         /  \  
	     //        /    \
	     //             -pi/4
	     //
	     // See thesis' final PDF for more information.
	     
	     // Yoel Shkolnisky 22/10/01
	
	     // Matlab origin if top-left corner. y-axes goes up-down (values increase as we move down). 
	     // Mathematical axes goes bottom-up (negative values are at the bottom). We must flip the matrix
	     // to convert Matlab axes into the mathematical axes so the matrix will match the mathematical equations.
    	 int i,j,k;
    	 int idx;
    	 double u[][];
    	 double w[][];
    	 double v[][];
    	 double imflip[][][] = new double[2][im[0].length][im[0][0].length];
	     for (i = 0; i < im[0].length; i++) {
	    	 for (j = 0; j < im[0][0].length; j++) {
	    		 imflip[0][i][j] = im[0][im[0].length-1-i][j];
	    		 imflip[1][i][j] = im[1][im[0].length-1-i][j];
	    	 }
	     }
	
	     int s1 = im[0].length;
	     int s2 = im[0][0].length;
	     
	     if (s1 != s2) {
	        System.err.println("In PPFT input image must be square");
	        System.exit(0);
	     }
	
	     if ((s1 % 2) !=0) {
	        System.err.println("In PPFT input image must have even side");
	        System.exit(0);
	     }
	
	     // initialize constants and data structures
	     int n= s1;  // At this point, all validity test passed and therefore the input image is square. Therefore we can
	                 // choose n to be the size of either of the dimensions.
	     int m=2*n+1;
	     //res1 = zeros(m,n+1);
	     //res2 = zeros(m,n+1);
	
	     // Computation of Res1
	     double EI[][][] = new double[2][2*n+1][n];
	     for (i = 0; i < n; i++) {
	    	 for (j = 0; j < n; j++) {
	    		 EI[0][i+n/2][j] = imflip[0][i][j];
	    		 EI[1][i+n/2][j] = imflip[1][i][j];
	    	 }
	     }
	     double FEI[][][] = cfft2(EI);
	           
	     u = new double[2][n];
	     for (k=-n; k <= n; k++) {
	    	 idx = toUnaliasedIdx(k,m);
	    	 for (i = 0; i < FEI[0][0].length; i++) {
	    		 u[0][i] = FEI[0][idx][i];
	    		 u[1][i] = FEI[1][idx][i];
	    	 }
	         w = GKN(u,k);
	         // See thesis paper for explanation of the flip
	         for (i = 0; i < w[0].length; i++) {
	        	 res1[0][idx][i] = w[0][w[0].length-1-i];
	        	 res1[1][idx][i] = w[1][w[0].length-1-i];
	         }
	     }   
	
	
	     // Computation of Res2
	     EI = new double[2][n][2*n+1];
	     for (i = 0; i < n; i++) {
	    	 for (j = 0; j < n; j++) {
	    		 EI[0][i][j+n/2] = imflip[0][i][j];
	    		 EI[1][i][j+n/2] = imflip[1][i][j];
	    	 }
	     }
	     FEI = cfft2(EI);
	
	     v = new double[2][n];
	     for (k=-n; k <= n; k++) {
	    	 idx = toUnaliasedIdx(k,m);
	    	 for (i = 0; i < FEI[0].length; i++) {
	    	     v[0][i] = FEI[0][i][idx];
	    	     v[1][i] = FEI[1][i][idx];
	    	 }
	         w = GKN(v,k);
	         // See thesis paper for explanation of the flip
	         for (i = 0; i < w[0].length; i++) {
	        	 res2[0][idx][i] = w[0][w[0].length-1-i];
	        	 res2[1][idx][i] = w[1][w[0].length-1-i];
	         } 
	     }
	
	     //Revision record
	     // 9/12/02   Yoel Shkolnisky     Added comments
     }

     private void OptimizedPPFT(double res1[][][], double res2[][][], double im[][][]) {
	     
	     // Optimized algorithm for computing the pseudo-polar Fourier transform.
	     // The computation requires O(n^2logn) operations and uses further 
	     // optimizations to reduce the operation count (compared to PPFT.m).
	     
	     // im    The image whose pseudo-polar Fourier transform should be computed.
	     //       Must of a dyadic square size (2^k x 2^k) = nxn.
	     
	     // Returns res1 and res2 (of size 2n+1xn+1) that contain the pseudo-polar Fourier 
	     // transform of the input image im.
	     // res1 contains the values of PPI1(k,l). res2 contains the values of PPI2(k,l). 
	     
	     // See PPFT.m for more documentation.
	     
	     // Yoel Shkolnisky 22/10/01
    	 
    	 int i,j,k,m,n;
    	 int idx;
    	 double alpha;
    	 double u[][];
    	 double w[][];
    	 double v[][];
    	 double imflip[][][] = new double[2][im[0].length][im[0][0].length];
	     for (i = 0; i < im[0].length; i++) {
	    	 for (j = 0; j < im[0][0].length; j++) {
	    		 imflip[0][i][j] = im[0][im[0].length-1-i][j];
	    		 imflip[1][i][j] = im[1][im[0].length-1-i][j];
	    	 }
	     }
	
	     int s1 = im[0].length;
	     int s2 = im[0][0].length;
	     
	     if (s1 != s2) {
	        System.err.println("In OptimizedPPFT input image must be square");
	        System.exit(0);
	     }
	
	     if ((s1 % 2) !=0) {
	        System.err.println("In OPtimizedPPFT input image must have even side");
	        System.exit(0);
	     }
	
	     // initialize constants and data structures
	     n=s1;
	     m=2*n+1;
	     alpha = 2.0*(n+1.0)/(n*m);
	     //res1 = zeros(m,n+1);
	     //res2 = zeros(m,n+1);
	
	     // Part I: Computation of Res1
	     // padding the y-direction and applying column-wise fft
	     double FEI[][] = new double[2][(2*n+1)*n];
	     double FEIC[][];
	     for (j = 0; j < n; j++) {
	    	 FEIC = new double[2][2*n+1];
	    	 for (i = 0; i < n; i++) {
	    		 FEIC[0][n/2+i] = imflip[0][i][j];
	    		 FEIC[1][n/2+i] = imflip[1][i][j];
	    	 }
	    	 FEIC = ifftshift1d(FEIC);
	    	 FFTUtility fft = new FFTUtility(FEIC[0], FEIC[1], 1, 2*n+1, 1, -1, FFTUtility.FFT);
		     fft.setShowProgress(false);
		     fft.run();
		     fft.finalize();
		     fft = null;
		     FEIC = fftshift1d(FEIC);
	    	 for (i = 0; i < 2*n+1; i++) {
	    		 FEI[0][i*n + j] = FEIC[0][i];
	    		 FEI[1][i*n + j] = FEIC[1][i];
	    	 }
	     }
	     
	
	     // fractional along rows with alpha=2*k*(n+1)/(n*m). This is equivalent
	     // to padding u to length 2n+1, applying FRFT with alpha=2*k/n and extracting 
	     // the n+1 central elements. However, this requires less memory and operations.
	     // The padding with the single zero is used to generate output of length
	     // n+1.
	     u = new double[2][n+1];
	     for (k=-n; k <= n; k++) {
	    	 idx = toUnaliasedIdx(k,m);
	    	 for (i = 0; i < n; i++) {
	    		 u[0][i] = FEI[0][idx*n + i];
	    		 u[1][i] = FEI[1][idx*n + i];
	    	 }
	         w = cfrft(u,k*alpha);
	         for (i = 0; i < n+1; i++) {
	        	 res1[0][idx][i] = w[0][n-i];
	        	 res1[1][idx][i] = w[1][n-i];
	         }
	     }   
	
	     // Part II: Computation of Res2
	     // padding the x-direction and applying row-wise fft
	     //EI  = [zeros(n,n/2) im zeros(n,n/2+1)];
	     FEI = new double[2][n*(2*n+1)];
	     double FEIR[][];
	     for (i = 0; i < n; i++) {
	    	 FEIR = new double[2][2*n+1];
	    	 for (j = 0; j < n; j++) {
	    		 FEIR[0][j+n/2] = imflip[0][i][j];
	    		 FEIR[1][j+n/2] = imflip[1][i][j];
	    	 }
	    	 FEIR = ifftshift1d(FEIR);
	    	 FFTUtility fft2 = new FFTUtility(FEIR[0], FEIR[1], 1, 2*n+1, 1, -1, FFTUtility.FFT);
		     fft2.setShowProgress(false);
		     fft2.run();
		     fft2.finalize();
		     fft2 = null;
		     FEIR = fftshift1d(FEIR);
	    	 for (j = 0; j < 2*n+1; j++) {
	    		 FEI[0][i*(2*n+1) + j] = FEIR[0][j];
	    		 FEI[1][i*(2*n+1) + j] = FEIR[1][j];
	    	 }
	     }
	
	     v = new double[2][n+1];
	     for (k=-n; k <= n; k++) {
	    	 idx = toUnaliasedIdx(k,m);
	    	 for (i = 0; i < n; i++) {
	    		 v[0][i] = FEI[0][i*(2*n+1) + idx];
	    		 v[1][i] = FEI[1][i*(2*n+1) + idx];
	    	 }
	         w = cfrft(v,k*alpha);
	         for (i = 0; i < n+1; i++) {
	        	 res2[0][idx][i] = w[0][n-i];
	        	 res2[1][idx][i] = w[1][n-i];
	         }
	     }
	
	     // Revision record
	     // 15/1/03   Yoel Shkolnisky     Used cfftd instead of column-wise cfft
     }
     
     private void slowPPFT(double res1[][][], double res2[][][], double im[][][]) {
	     
	     // Compute the pseudo-polar Fourier transform directly.
	     // The computation requires O(n^4) operations.
	     
	     // im    The image whose pseudo-polar Fourier transform should be computed.
	     //       Must be of a dyadic square size (2^k x 2^k).
	     
	     // Returns res1 and res2 (of size 2n+1xn+1) that contain the pseudo-polar Fourier 
	     // transform of the input image im.
	     // res1 contains the values of PPI1(k,l). res2 contains the values of PPI2(k,l). 
	     // The first argument of Res1 and Res2 corresponds to pseudo-radius and the second argument 
	     // corresponds to pseudo-angle.
	     
	     // See PPFT.m for more documentation.
	     
	     // See thesis' final PDF p.42 for more information.
	     
	     // Yoel Shkolnisky 22/10/0
    	 
    	 int i,j,k,l,n,ppRows,ppCols;
    	 double trig[];
    	 double imflip[][][] = new double[2][im[0].length][im[0][0].length];
	     for (i = 0; i < im[0].length; i++) {
	    	 for (j = 0; j < im[0][0].length; j++) {
	    		 imflip[0][i][j] = im[0][im[0].length-1-i][j];
	    		 imflip[1][i][j] = im[1][im[0].length-1-i][j];
	    	 }
	     }
	
	     int s1 = im[0].length;
	     int s2 = im[0][0].length;
	     
	     if (s1 != s2) {
	        System.err.println("In slowPPFT input image must be square");
	        System.exit(0);
	     }
	
	     if ((s1 % 2) !=0) {
	        System.err.println("In slowPPFT input image must have even side");
	        System.exit(0);
	     }
	
	     n=s1;
	
	     ppRows = 2*n+1;
	     ppCols = n+1;
	     //res1 = zeros(ppRows,ppCols);
	     //res2 = zeros(ppRows,ppCols);
	
	     // computation of Res1
	     for (k=-n; k <= n; k++) {
	        for (l=-n/2; l <= n/2; l++) {
	           trig = trigPoly(imflip, -2.0*l*k/n,k);
	           res1[0][toUnaliasedIdx(k,ppRows)][toUnaliasedIdx(l,ppCols)]=trig[0];
	           res1[1][toUnaliasedIdx(k,ppRows)][toUnaliasedIdx(l,ppCols)]=trig[1];
	        }
	     } 
	
	     // computation of Res2
	     for (k=-n; k <= n; k++) {
	        for (l=-n/2; l <= n/2; l++) {
	           trig = trigPoly(imflip,k,-2.0*l*k/n);
	           res2[0][toUnaliasedIdx(k,ppRows)][toUnaliasedIdx(l,ppCols)]=trig[0];
	           res2[1][toUnaliasedIdx(k,ppRows)][toUnaliasedIdx(l,ppCols)]=trig[1];
	        }
	     }
     }


     private double[] trigPoly(double im[][][], double xi1, double xi2) {
	     // The function evaluates the trigonometric polynomial
	     //                          n/2    n/2
	     //       TrigPolyI(xi1,xi2) = sum    sum I(u,v)*exp(-2*pi*i*(xi1*u+xi2*v)/m)       m=2n+1
	     //                         u=-n/2 v=-n/2
	     // where:
	     //       xi1,xi2 -   The points in the frequency domain at which we evaluate the trigonometric polynomial
	     //       im          -  The 2-D signal (image) for which the trigonometric polynomial is evaluated
	     
	     // This function is used as an auxiliary function for computing the pseudo-polar Fourier transform
	     // directly (according to the definition of the pseudo-polar Fourier transform).
	     // The calling function (slowPPFT) uses this auxiliary function to compute the trigonometric 
	     // polynomial on the pseudo-polar grid points.
	
	     int n = im[0].length;
	     int m=2*n+1;
	     double acc[] = new double[2];
	     int u,v;
	     for (u=lowIdx(n); u <= hiIdx(n); u++) {     // x direction
	        for (v=lowIdx(n); v <= hiIdx(n); v++) {  // y direction
	           double imcom = im[0][toUnaliasedIdx(v,n)][toUnaliasedIdx(u,n)];
	           double imcomImag = im[1][toUnaliasedIdx(v,n)][toUnaliasedIdx(u,n)];
	           double arg = 2.0*Math.PI*(xi1*u + xi2*v)/m;
	           double cos = Math.cos(arg);
	           double sin = Math.sin(arg);
	           acc[0] = acc[0] + imcom*cos + imcomImag*sin;
	           acc[1] = acc[1] - imcom*sin + imcomImag*cos;
	        }
	     }
	     return acc;
	
	
	     // Revision record
	     // 9/12/02   Yoel Shkolnisky     Added comments to the function "trigPoly"
     }

     
     private void Radon(double res1[][], double res2[][], double im[][]) {
    	 // res1 and res2 are real
	     // Fast algorithm for computing the discrete Radon transform.
	     // The computation requires O(n^2logn) operations.
	     
	     // im    The image whose discrete Radon transform should be computed.
	     //       Must be real (no imaginary components) and of a dyadic square size (2^k x 2^k).
	     
	     // Returns res1 and res2 (of size 2n+1xn+1) that contain the discrete Radon
	     // transform of the input image im.
	     // res1 contains the values which correspond to rays of radius k=-n...n and angles
	     // theta=arctan(2l/n) l=-n/2...n/2 (from -pi/4 to pi/4). 
	     // res2 contains the values which correspond to rays of radius k=-n...n and angles
	     // theta=pi/2-arctan(2l/n) l=-n/2...n/2 (from 3pi/4 to pi/4). 
	     
	     // Due to small round-off errors, the output may contain small imaginary
	     // components. If the input image is real, the function truncates any
	     // imaginary components from the output array (since the discrete Radon
	     // transform of a real image is real)
	     //
	     // See thesis' final PDF for more information.
	     
	     // See also PPFT, OptimizedPPFT, adjRadon.
	     
	     // Yoel Shkolnisky 22/10/01
    	 int i,j;
    	 int n = im[0].length;
    	 double res1Complex[][][] = new double[2][2*n+1][n+1];
    	 double res2Complex[][][] = new double[2][2*n+1][n+1];
    	 double imComplex[][][] = new double[2][n][n];
    	 for (i = 0; i < n; i++) {
    		 for (j = 0; j < n; j++) {
    			 imComplex[0][i][j] = im[i][j];
    		 }
    	 }
	
	     OptimizedPPFT(res1Complex, res2Complex, imComplex);
	     double res1C[][];
	     double res2C[][];
	     
	     // Inverse FFT along columns
	     for (j = 0; j < n+1; j++) {
	    	 res1C = new double[2][2*n+1];
	    	 res2C = new double[2][2*n+1];
	    	 for (i = 0; i < 2*n+1; i++) {
	    		 res1C[0][i] = res1Complex[0][i][j];
	    		 res1C[1][i] = res1Complex[1][i][j];
	    		 res2C[0][i] = res2Complex[0][i][j];
	    		 res2C[1][i] = res2Complex[1][i][j];
	    	 }
	    	 res1C = ifftshift1d(res1C);
	    	 res2C = ifftshift1d(res2C);
	    	 FFTUtility fft = new FFTUtility(res1C[0], res1C[1], 1, 2*n+1, 1, 1, FFTUtility.FFT);
		     fft.setShowProgress(false);
		     fft.run();
		     fft.finalize();
		     fft = null;
		     fft = new FFTUtility(res2C[0], res2C[1], 1, 2*n+1, 1, 1, FFTUtility.FFT);
		     fft.setShowProgress(false);
		     fft.run();
		     fft.finalize();
		     fft = null;
		     res1C = fftshift1d(res1C);
		     res2C = fftshift1d(res2C);
	    	 for (i = 0; i < 2*n+1; i++) {
	    		 res1Complex[0][i][j] = res1C[0][i];
	    		 res1Complex[1][i][j] = res1C[1][i];
	    		 res2Complex[0][i][j] = res2C[0][i];
	    		 res2Complex[1][i][j] = res2C[1][i];
	    	 }
	     }
	
	     
	     
	
	     // For safety - should never happen.
	     // No complex entries are expected in rim (the result array).
	     // This condition should catch programming bugs that result in complex entries in rim.
	     double maxAbsVal1 = 0.0;
	     double maxAbsVal2 = 0.0;
	     for (i = 0; i < 2*n+1; i++) {
	    	 for (j = 0; j < n+1; j++) {
	    		 if (Math.abs(res1Complex[1][i][j]) > maxAbsVal1) {
	    			 maxAbsVal1 = Math.abs(res1Complex[1][i][j]);
	    		 }
	    		 if (Math.abs(res2Complex[1][i][j]) > maxAbsVal2) {
	    			 maxAbsVal2 = Math.abs(res2Complex[1][i][j]);
	    		 }
	    	 }
	     }
	     if (maxAbsVal1 > 1.0E-5) {
	    	 System.out.println("Warning! In Radon res1 has an imaginary component of maximum absolute value = " + maxAbsVal1);
	     }
	     if (maxAbsVal2 > 1.0E-5) {
	    	 System.out.println("Warning! In Radon res2 has an imaginary component of maximum absolute value = " + maxAbsVal2);
	     }
	
	     // Remove the imaginary component 00000000i from the entries of the result
	     // matrix. This component causes the entries of rim to be considered imaginary, 
	     // although the imaginary part is very close to zero.
	     for (i = 0; i < 2*n+1; i++) {
	    	 for (j = 0; j < n+1; j++) {
	    		 res1[i][j] = res1Complex[0][i][j];
	    		 res2[i][j] = res2Complex[0][i][j];
	    	 }
	     }
	        
	     // Revision record
	     // 15/1/03   Yoel Shkolnisky     Use cfftd instead of column-wise cfft
     }
     
     private double[][][][] Radon3(ModelImage im) {
    		 
    		 // Fast algorithm for computing the 3-D discrete Radon transform.
    		 // The computation requires O(n^3logn) operations for a nxnxn image.
    		 
    		 // The algorithm is based on the Fourier slice theorem for the 30D discrete
    		 // Radon transform. 
    		 // The function computes the 3-D pseudo-polar Fourier transform of im and
    		 // then perform 1-D inverse FFT along the parameters k (the pseudo-radius
    		 // parameter).
    		 
    		 // Due to small round-off errors, the output may contain small imaginary
    		 // components. If the input image is real, the function truncates any
    		 // imaginary components from the output array (since the discrete Radon
    		 // transform of a real image is real)
    		 
    		 // Input:
    		 //   im - 3D array of size nxnxn.
    		 //        first  index - x direction
    		 //        second index - y direction
    		 //        third  index - z direction
    		 //     In each of the directions, the range 1...n is treated as -n/2...n/2-1.
    		 //     The input image should be of size nxnxn with even n.
    		 
    		 // Output:
    		 //   xr - 4D array 3x(3n+1)x(n+1)x(n+1) containing the 3-D discrete Radon transform of the input image.
    		 //     The output array has the following format:
    		 //        first  index  - 1 for x-planes, 2 for y-planes, 3 for z-planes
    		 //        second index  - plane intercept
    		 //        third  index  - slope p
    		 //        fourth index  - slope q
    		 
    		 // Yoel Shkolnisky 03/02/03

    		 int i,j,k,m;
    	     // verify that the input is a 3D image of size nxnxn
    		 verifyImage(im);
    		 int n = im.getExtents()[0];
    		 
    		 double pp[][][][][] = ppft3(im);
    		 double rr[][][][][] = new double[2][3][3*n+1][n+1][n+1];
    		 double res[][];
    		 // Inverse FFT on second dimension
    		 for (i = 0; i < 3; i++) {
    			 for (j = 0; j < n+1; j++) {
    				 for (k = 0; k < n+1; k++) {
    					 res = new double[2][3*n+1];
    					 for (m = 0; m < 3*n+1; m++) {
    						 res[0][m] = pp[0][i][m][j][k];
    						 res[1][m] = pp[1][i][m][j][k];
    					 }
    					 res = ifftshift1d(res);
    			    	 FFTUtility fft = new FFTUtility(res[0], res[1], 1, 3*n+1, 1, 1, FFTUtility.FFT);
    				     fft.setShowProgress(false);
    				     fft.run();
    				     fft.finalize();
    				     fft = null;
    				     res = fftshift1d(res);
    				     for (m = 0; m < 3*n+1; m++) {
    				    	 rr[0][i][m][j][k] = res[0][m];
    				    	 rr[1][i][m][j][k] = res[1][m];
    				     }
    				 }
    			 }
    		 }
    		 return rr[0];
     }

     
     private void slowRadon(double res1[][], double res2[][], double im[][]) {
	     
	     // Compute the pseudo-Radon transform directly.
	     // The computation requires O(n^3) operations.
	     
	     // im    The image whose discrete Radon transform should be computed.
	     //       Must be real (no imaginary components) and of a dyadic square size (2^k x 2^k).
	     
	     // Returns res1 and res2 (of size 2n+1xn+1) that contain the discrete Radon
	     // transform of the input image im.
	     // res1 contains the radon values of basically horizontal lines. res2 contains the Radon
	     // values of basically vertical lines. 
	     // The first argument of res1 and res2 corresponds to pseudo-radius and the second argument 
	     // corresponds to pseudo-angle.
	     
	     // See Radon for more information.
	     
	     // See thesis' final PDF for more information.
	     
	     // Yoel Shkolnisky 22/10/01
    	 
    	 int i,j,l,n,ppRows,ppCols,t,u,v;
    	 double slope, acc;
    	 double imflip[][] = new double[im.length][im[0].length];
	     for (i = 0; i < im.length; i++) {
	    	 for (j = 0; j < im[0].length; j++) {
	    		 imflip[i][j] = im[im.length-1-i][j];
	    	 }
	     }
	
	     int s1 = im.length;
	     int s2 = im[0].length;
	     
	     if (s1 != s2) {
	        System.err.println("In slowRadon input image must be square");
	        System.exit(0);
	     }
	
	     if ((s1 % 2) !=0) {
	        System.err.println("In slowRadon input image must have even side");
	        System.exit(0);
	     }
	
	     n=s1;
	
	     ppRows = 2*n+1;
	     ppCols = n+1;
	     //res1 = zeros(ppRows,ppCols);
	     //res2 = zeros(ppRows,ppCols);
	
	     // computation of res1
	     for (t=-n; t <= n; t++) {
	        for (l=-n/2; l <= n/2; l++) {
	           slope = 2.0*l/n;
	           acc   = 0;
	           
	           for (u=-n/2; u <= n/2-1; u++) {
	              acc = acc + I1(imflip,n,u,slope*u+t);
	           }
	           res1[toUnaliasedIdx(t,ppRows)][toUnaliasedIdx(l,ppCols)]=acc;
	        }
	     }     
	         
	     // computation of res2
	
	     for (t=-n; t <= n; t++) {
	        for (l=-n/2; l <= n/2; l++) {
	           slope = 2.0*l/n;
	           acc   = 0;
	           
	           for (v=-n/2; v <= n/2-1; v++) {
	              acc = acc + I2(imflip,n,slope*v+t,v);
	           }
	           res2[toUnaliasedIdx(t,ppRows)][toUnaliasedIdx(l,ppCols)]=acc;
	        }
	     }
     }

     private double[][][][] slowradon3(ModelImage im) {
	     
	     // Compute the 3D pseudo-Radon transform directly, according to its definition.
	     // The computation requires O(n^6) operations for a nxnxn image.
	     
	     // Input:
	     //   im - 3D array of size nxnxn.
	     //        first  index - x direction
	     //        second index - y direction
	     //        third  index - z direction
	     //     In each of the directions, the range 1...n is treated as -n/2...n/2-1.
	     //     The input image should be of size nxnxn with even n.
	     
	     // Output:
	     //   xr - 4D array 3x(3n+1)x(n+1)x(n+1) containing the 3-D discrete Radon transform of the input image.
	     //     The output array has the following format:
	     //        first  index  - 1 for x-planes, 2 for y-planes, 3 for z-planes
	     //        second index  - plane intercept
	     //        third  index  - slope p
	     //        fourth index  - slope q
	     
	     // Yoel Shkolnisky 29/01/03
	
	     int i,j,k,p,q,t,u,v,w;
	     double s1,s2;
	     double acc;
	     int aCoord[];
	     int N[];
	     int coord[];
    	 // verify that the input is a 3D image of size nxnxn
	     verifyImage(im);
	
	     // Initialize output data structure
	     int n= im.getExtents()[0]; // at this point n is even
	     double buffer[] = new double[n*n*n];
	     try {
	    	 im.exportData(0, n*n*n, buffer);
	     }
	     catch (IOException e) {
	    	 System.err.println("IOException on im.exportData(0, n*n*n. buffer)");
	    	 System.exit(0);
	     }
	     double ima[][][] = new double[n][n][n];
	     for (i = 0; i < n; i++) {
	    	 for (j = 0; j < n; j++) {
	    		 for (k = 0; k < n; k++) {
	    			 ima[i][j][k] = buffer[i*n*n + j*n + k];
	    		 }
	    	 }
	     }
	     int m = 3*n+1;
	     double rr[][][][] = new double[3][3*n+1][n+1][n+1];
	
	     // Compute the 3-D discrete Radon transform for x-planes
	     for (p=-n/2; p <= n/2; p++) {  // first slope
	        s1=2.0*p/n;
	        
	         for (q=-n/2; q <= n/2; q++) { // second slope
	           s2=2.0*q/n;
	           
	           for (t=-3*n/2; t <= 3*n/2; t++) {    // intercept
	             acc=0;
	             for (v=-n/2; v <= n/2-1; v++) {
	                 for (w=-n/2; w <= n/2-1; w++) {
	                    acc = acc+I1(ima,n,s1*v+s2*w+t,v,w);
	                 } // for (w=-n/2; w <= n/2-1; w++)
	             } // for (v=-n/2; v <= n/2-1; v++)
	             aCoord = new int[] {t,p,q};
	             N = new int[] {m,n+1,n+1};
	             coord = toUnaliasedCoord(aCoord, N);
	             rr[0][coord[0]][coord[1]][coord[2]] = acc;
	           } // for (t=-3*n/2; t <= 3*n/2; t++)
	         } // for (q=-n/2; q <= n/2; q++)
	     } // for (p=-n/2; p <= n/2; p++)
	
	     // Compute the 3-D discrete Radon transform for y-planes
	     for (p=-n/2; p <= n/2; p++) {  // first slope
	        s1=2.0*p/n;
	        
	         for (q=-n/2; q <= n/2; q++) { // second slope
	           s2=2.0*q/n;
	           
	           for (t=-3*n/2; t <= 3*n/2; t++) {  // intercept
	             acc=0;
	             for (u=-n/2; u <= n/2-1; u++) {
	                 for (w=-n/2; w <= n/2-1; w++) {
	                    acc = acc+I2(ima,n,u,s1*u+s2*w+t,w);
	                 } // for (w=-n/2; w <= n/2-1; w++)
	             } // for (u=-n/2; u <= n/2-1; u++)
	             aCoord = new int[] {t,p,q};
	             N = new int[] {m,n+1,n+1};
	             coord = toUnaliasedCoord(aCoord, N);
	             rr[1][coord[0]][coord[1]][coord[2]] = acc;
	           } // for (t=-3*n/2; t <= 3*n/2; t++)
	         } // for (q=-n/2; q <= n/2; q++)
	     } // for (p=-n/2; p <= n/2; p++)
	
	     // the 3-D discrete Radon transform for z-planes
	     for (p=-n/2; p <= n/2; p++) {  // first slope
	        s1=2.0*p/n;
	        
	         for (q=-n/2; q <= n/2; q++) { // second slope
	           s2=2.0*q/n;
	           
	           for (t=-3*n/2; t <= 3*n/2; t++) { // intercept
	             acc=0;
	             for (u=-n/2; u <= n/2-1; u++) {
	                 for (v=-n/2; v <= n/2-1; v++) {
	                    acc = acc+I3(ima,n,u,v,s1*u+s2*v+t);
	                 } // for (v=-n/2; v <= n/2-1; v++)
	             } // for (u=-n/2; u <= n/2-1; u++)
	             aCoord = new int[] {t,p,q};
	             N = new int[] {m,n+1,n+1};
	             coord = toUnaliasedCoord(aCoord, N);
	             rr[2][coord[0]][coord[1]][coord[2]] = acc;
	           } // for (t=-3*n/2; t <= 3*n/2; t++)
	         } // for (q=-n/2; q <= n/2; q++)
	     } // for (p=-n/2; p <= n/2; p++)
	     return rr;
     }

     // computation of I1 - trigonometric interpolation of I along the columns (along the y-axis)
     private double I1(double im[][], int n, int u, double y) {
	     int m=2*n+1;
	     double acc = 0;
	     int v;
	     double yin[] = new double[1];
	
	     for (v=-n/2; v <= n/2-1; v++) {
	    	yin[0] = y-v;
	        acc = acc + im[toUnaliasedIdx(v,n)][toUnaliasedIdx(u,n)]*dirichlet(yin,m)[0];
	     }
	     return acc; 
     }
     
     // computation of I1 - continuous extension of I along the x direction
     private double I1(double im[][][], int n, double x,int v,int w) {
		 int m=3*n+1;
		 double acc = 0;
		 int u;
		 double xin[] = new double[1];
		 int aCoord[];
		 int N[];
		 int coord[];
		
		 for (u=-n/2; u <= n/2-1; u++) {
			aCoord = new int[] {u,v,w};
			N = new int[] {n,n,n}; 
		    coord = toUnaliasedCoord(aCoord,N);
		    xin[0] = x-u;
		    acc = acc + im[coord[0]][coord[1]][coord[1]]*dirichlet(xin,m)[0];
		 }
		 return acc;
     }


     // computation of I2 - trigonometric interpolation of I along the columns (along the y-axis)
     private double I2(double im[][], int n, double x,int v) {
	     int m=2*n+1;
	     double acc = 0;
	     int u;
	     double xin[] = new double[1];
	
	     for (u=-n/2; u <= n/2-1; u++) {
	    	xin[0] = x-u;
	        acc = acc + im[toUnaliasedIdx(v,n)][toUnaliasedIdx(u,n)]*dirichlet(xin,m)[0];
	     }
	     return acc;
     }
     
     // computation of I2 - continuous extension of I along the y direction
     private double I2(double im[][][],int n,int u,double y, int w) {
	     int m=3*n+1;
	     double acc = 0;
	     int v;
	     double yin[] = new double[1];
	     int aCoord[];
		 int N[];
		 int coord[];
	     
	     for (v=-n/2; v <= n/2-1; v++) {
	    	    aCoord = new int[] {u,v,w};
				N = new int[] {n,n,n}; 
			    coord = toUnaliasedCoord(aCoord,N);
			    yin[0] = y-v;
	            acc = acc + im[coord[0]][coord[1]][coord[2]]*dirichlet(yin,m)[0];
	     }
	     return acc; 
     }
     
     // computation of I3 - continuous extension of I along the z direction
     private double I3(double im[][][], int n,int u,int v, double z) {
	     int m=3*n+1;
	     double acc = 0;
	     int w;
	     double zin[] = new double[1];
	     int aCoord[];
		 int N[];
		 int coord[];
	
	     for (w=-n/2; w <= n/2-1; w++) {
	    	 aCoord = new int[] {u,v,w};
			 N = new int[] {n,n,n}; 
			 coord = toUnaliasedCoord(aCoord,N);
			 zin[0] = z-w;
	         acc = acc + im[coord[0]][coord[1]][coord[2]]*dirichlet(zin,m)[0];
	     }
	     return acc; 
     }

     private double[][][] PtP(double X[][][]) {
    		 
    		 // Gram Operator of the pseudo-polar Fourier transform.
    		 // Performs adjP(D(P)) where
    		 //    P     The pseudo-polar Fourier transform
    		 //    D     Preconditioner
    		 //    adjP  Adjoint pseudo-polar Fourier transform
    		 
    		 //  Input parameters:
    		 //    X      n*n matrix (x,y)
    		 //  Outputs parameters:
    		 //    Y      n*n matrix (x,y)
    		 //
    		 // Yoel Shkolnisky 17/12/02

    		 int n = X[0].length;
    		 double pp1[][][] = new double[2][2*n+1][n+1];
    		 double pp2[][][] = new double[2][2*n+1][n+1];
    		 OptimizedPPFT(pp1, pp2, X);
    		 double Y[][][] = PrecondAdjPPFT(pp1,pp2);
    		 return Y;
     }
     
     private void cfrftV3_precomp(double PQ[][][], double PZ[][][], int m, double alpha[]) {
    		 //
    		 // Generate tables to be used by cfrftV3. 
    		 // The tables contain precomputed factors that are for computing the
    		 // fractional Fourier transform. Avoiding recomputing these factors speeds
    		 // up the computation.
    		 
    		 // Input
    		 //   alpha    A vector with the spacings with which the fractional Fourier
    		 //            transform will be computed in subsequent calls to cfrftV3.
    		 //            After calling the current function, instead of calling
    		 //            w=cfrftV3(x,alpha), where alpha is the required spacing, call
    		 //            w=cfrftV3(x,0,PQ,PZ,k), where PQ and PZ are the tables
    		 //            computed by the current function. The fractional Fourier 
    		 //            transform will be then computed with spacing alpha(k).
    		 //   m        The length of the signal that will be tramsformed by
    		 //            subsequent calls to cfrftV3.
    		 
    		 // Returns the precomputed tables PQ and PZ.
    		 //
    		 // Revised:
    		 // Yoel Shkolnisky  May 17, 2010.

    		 int i,k;
    	     int n=alpha.length;

    		 int lm= -(int)Math.floor(m/2.0);
    		 int hm= (int)Math.floor((m-0.5)/2.0);
    		 int ofs=(int)Math.floor(3.0*m/2.0)+1;

    		 int jlen = hm-lm+1;
    		 int j[] = new int[jlen];
    		 for (i = 0; i < jlen; i++) {
    			 j[i] = lm+i;
    		 }
    		 int j2len = 2*m+1;
    		 int j2[] = new int[j2len];
    		 for (i = 0; i < j2len; i++) {
    			 j2[i] = -m + i;
    		 }

    		 //PQ=zeros(m,n);
    		 //PZ=zeros(3*m,n);

    		 for (k=0; k < n; k++) {    
    		     for (i = 0; i < m; i++) {
    		         double arg = Math.PI*alpha[k]*j[i]*j[i]/m;
    		         PQ[0][i][k] = Math.cos(arg);
    		         PQ[1][i][k] = -Math.sin(arg);
    		     }
    		     double z[][] = new double[2][3*m];

    		     for (i = 0; i < 2*m+1; i++) {
    		    	 double arg2 = Math.PI*alpha[k]*j2[i]*j2[i]/m;
    		    	 z[0][-m+ofs+i] = Math.cos(arg2);
    		    	 z[1][-m+ofs+i] = Math.sin(arg2);
    		     }
    		     FFTUtility fft = new FFTUtility(z[0], z[1], 1, 3*m, 1, -1, FFTUtility.FFT);
    		     fft.setShowProgress(false);
    		     fft.run();
    		     fft.finalize();
    		     fft = null;
    		     for (i = 0; i < 3*m; i++) {
    		    	 PZ[0][i][k] = z[0][i];
    		    	 PZ[1][i][k] = z[1][i];
    		     }   
    		 } // for (k=0; k < n; k++)
     }
     
     private double[][][] cfrftV3(double x[][][], double alpha,double PQ[][][], double PZ[][][], int k) {
    		 
    		 // Aliased fractional Fourier transform of the sequence x.
    		 // The FRFT is computed using O(nlogn) operations.
    		 
    		 // Input:
    		 // x       The sequence whose FRFT should be computed. Can be of odd or even
    		 //         length. Must be a 1-D row vector.
    		 // alpha   The parameter alpha of the fractional Fourier transform.
    		 // PQ,PZ   Precomputed tables for avoiding unnecessary computations in
    		 //         repeated call to the function. These tables are generated using
    		 //         cfrftV3_precomp.
    		 // k       Determines the value of the fractional spacing, as well as the
    		 //         appropriate column to use in the precomputed tables. See ppft3V3.
    		 
    		 // Returns the aliased FRFT with parameter alpha of the sequence x.
    		 // The fractional Fourier transform w of the sequence x (with parameter alpha) is defined by
    		 //                   n/2-1
    		 //       w(j) =       sum  x(u)*exp(-2*pi*i*j*u*alpha/N),  -n/2 <= j <= n/2-1, N=length(x).
    		 //                   u=-n/2
    		 
    		  
    		 // This function is the same as cfrftV2. It uses the less padding (3m as in
    		 // the paper) and therefore it is more memory efficient. This may cause the
    		 // lengths of the sequences to be non-optimal for Matlab's FFT function. The
    		 // function cfrftV2 uses more memory (for padding) but uses FFTs of dyadic
    		 // length. 
    		 
    		 // If five parameters are provided, then alpha is ignored. Just pass,
    		 // for example, zero.
    		 
    		 // Yoel Shkolnisky 22/10/01
    		 
    		 // Revisions:
    		 // Yoel Shkolnisky 21/05/2013 Chage the code to work with column vectors.
    		 //          Allow x to be a matrix, in which case the transform is applied
    		 //          on all columns.

    		 int i,n;
    	     int m=x[0].length;
    	     double q[][] = new double[2][m];
    		 double Z[][] = new double[2][3*m];
    		 int lm= -(int)Math.floor(m/2.0);
    		 int hm= (int)Math.floor((m-0.5)/2.0);
    		 int ofs=(int)Math.floor(3.0*m/2.0)+1;

    		 if ((PQ != null) && (PZ != null)) {
    		     // load weights from the precomputed structure
    			 for (i = 0; i < m; i++) {
    				 q[0][i] = PQ[0][i][k-1];
    				 q[1][i] = PQ[1][i][k-1];
    			 }
    		     for (i = 0; i < 3*m; i++) {
    		    	 Z[0][i] = PZ[0][i][k-1];
    		    	 Z[1][i] = PZ[1][i][k-1];
    		     }
    		 }
    		 else {
    		     // computed required weights
    			 int jlen = hm-lm+1;
        		 int j[] = new int[jlen];
        		 for (i = 0; i < jlen; i++) {
        			 j[i] = lm+i;
        		 }
        		 int j2len = 2*m+1;
        		 int j2[] = new int[j2len];
        		 for (i = 0; i < j2len; i++) {
        			 j2[i] = -m + i;
        		 }
    		     //E=1i*pi*alpha;
        		 for (i = 0; i < m; i++) {
    		         double arg = Math.PI*alpha*j[i]*j[i]/m;
    		         q[0][i] = Math.cos(arg);
    		         q[1][i] = -Math.sin(arg);
    		     }
    		     //q=q(:);
        		 for (i = 0; i < 2*m+1; i++) {
    		    	 double arg2 = Math.PI*alpha*j2[i]*j2[i]/m;
    		    	 Z[0][-m+ofs+i] = Math.cos(arg2);
    		    	 Z[1][-m+ofs+i] = Math.sin(arg2);
    		     }
    		     FFTUtility fft = new FFTUtility(Z[0], Z[1], 1, 3*m, 1, -1, FFTUtility.FFT);
    		     fft.setShowProgress(false);
    		     fft.run();
    		     fft.finalize();
    		     fft = null;
    		 }

    		 int sz2=x[0][0].length;
    		 double y[][][] = new double[2][m][sz2];
    		 for (i = 0; i < m; i++) {
    		     for (n = 0; n < sz2; n++) {
    		    	 y[0][i][n] = x[0][i][n]*q[0][i] - x[1][i][n]*q[1][i];
    		    	 y[1][i][n] = x[0][i][n]*q[1][i] + x[1][i][n]*q[0][i];
    		     }
    		 }
    		 double Y[][] = new double[2][3*m*sz2];
    		 for (i = 0; i < m; i++) {
    			 for (n = 0; n < sz2; n++) {
    			 	 Y[0][(i + m)*sz2 + n] = y[0][i][n];
    			 	 Y[1][(i + m)*sz2 + n] = y[1][i][n];
    			 }
    		 }
    		 FFTUtility fft = new FFTUtility(Y[0], Y[1], 1, 3*m, sz2, -1, FFTUtility.FFT);
    		 fft.setShowProgress(false);
    		 fft.run();
    		 fft.finalize();
    		 fft = null;
    		 if (sz2 > 1) {
    			 FFTUtility fft2 = new FFTUtility(Y[0],Y[1], 3*m, sz2, 1, -1, FFTUtility.FFT);
    			 fft2.setShowProgress(false);
    			 fft2.run();
    			 fft2.finalize();
    			 fft2 = null;
    		 }
    		 double W[][] = new double[2][3*m*sz2];
    		 for (i = 0; i < 3*m; i++) {
    			 for (n = 0; n < sz2; n++) {
    				 W[0][i*sz2 + n] = Y[0][i*sz2 + n]*Z[0][i] - Y[1][i*sz2 + n]*Z[1][i]; 
    				 W[1][i*sz2 + n] = Y[0][i*sz2 + n]*Z[1][i] + Y[1][i*sz2 + n]*Z[0][i]; 
    			 }
    		 }
    		 FFTUtility ifft = new FFTUtility(W[0], W[1], 1, 3*m, sz2, 1, FFTUtility.FFT);
    		 ifft.setShowProgress(false);
    		 ifft.run();
    		 ifft.finalize();
    		 ifft = null;
    		 if (sz2 > 1) {
    			 FFTUtility ifft2 = new FFTUtility(W[0],W[1], 3*m, sz2, 1, 1, FFTUtility.FFT);
    			 ifft2.setShowProgress(false);
    			 ifft2.run();
    			 ifft2.finalize();
    			 ifft2 = null;
    		 }
    		 double w[][][] = new double[2][3*m][sz2];
    		 for (i = ofs-1; i < 3*m; i++) {
    			 for (n = 0; n < sz2; n++) {
    				 w[0][i-(ofs-1)][n] = W[0][i*sz2+n];
    				 w[1][i-(ofs-1)][n] = W[1][i*sz2+n];
    			 }
    		 }
    		 for (i = 0; i < ofs-1; i++) {
    			for (n = 0; n < sz2; n++) {
    				w[0][3*m-ofs+1+i][n] = W[0][i*sz2+n];
    				w[1][3*m-ofs+1+i][n] = W[1][i*sz2+n];
    			}
    		 }
    		 double wtrunc[][][] = new double[2][hm-lm+1][sz2];
    		 for (i = lm+ofs-1; i < hm+ofs; i++) {
    			 for (n = 0; n < sz2; n++) {
    				 wtrunc[0][i-(lm+ofs-1)][n] = w[0][i][n];
    				 wtrunc[1][i-(lm+ofs-1)][n] = w[1][i][n];
    			 }
    		 }
    		 double wout[][][] = new double[2][m][sz2];
    		 for (i = 0; i < m; i++) {
    			 for (n = 0; n < sz2; n++) {
    				 wout[0][i][n] = wtrunc[0][i][n]*q[0][i] - wtrunc[1][i][n]*q[1][i];
    				 wout[1][i][n] = wtrunc[0][i][n]*q[1][i] + wtrunc[1][i][n]*q[0][i];
    			 }
    		 }
    		 return wout;
     }

     private double[][][][][] ppft3(ModelImage im) {
	     
	     // Fast algorithm for computing the 3-D pseudo-polar Fourier transform.
	     // The computation requires O(n^3logn) operations.
	     
	     // The function computes the 3-D pseudo-polar Fourier transform according to
	     // the algorithm given in
	     // "A. Averbuch and Y. Shkolnisky. 3D Fourier based discrete Radon
	     // transform. Applied and Computational Harmonic Analysis, 15(1):33-69,
	     // 2003."
	     
	     // This implementation follows exactly the pseudo-code and notations given
	     // in the paper. 
	     
	     // Input:
	     //     im    3-D image of size nxnxn (n even).
	     //           First  index - x direction
	     //           Second index - y direction
	     //           Third  index - z direction
	     //     The indices in each direction are assumed to be from -n/2 to n/2-1 and
	     //     not from 1 to n.
	     
	     // Output:
	     //     pp - 4-D array of size 3x(3n+1)x(n+1)x(n+1) containing the 3-D
	     //     pseudo-polar Fourier transform.
	     //     The array pp contains the following Fourier samples:
	     //        pp(1,k,l,j) = FI(k,-2lk/n,-2jk/n)
	     //        pp(2,k,l,j) = FI(-2lk/n,k,-2jk/n)
	     //        pp(3,k,l,j) = FI(-2lk/n,-2jk/n,k)
	     //        where
	     //             l,j = -n/2,...,n/2   k=-3n/2,...3n/2
	     //        and
	     //                           n/2-1   n/2-1   n/2-1
	     //             FI(ox,oy,oz)=  sum     sum     sum  I(u,v,w)exp(-2*pi*i(u*ox+v*oy+w*oz)/m)   m=3n+1
	     //                           u=-n/2  v=-n/2  w=-n/2
	     
	     // See also ppft3_ref.
	     
	     // Yoel Shkolnisky 30/01/03
	     
	     // Revisions:
	     // Yoel Shkolnisky   19/05/2013    Renamed from OptimizedPPFT3 to PPFT3.
	     // Yoel Shkolnisky   21/05/2013    Vectorize for speed (about factor 4).
	
	     int i,k,p,q;
    	 // verify that the input is a 3D image of size nxnxn
	     verifyImage(im);
	
	     // Initialize output data structure
	     int n= im.getExtents()[0]; // at this point n is even
	     double buffer[] = new double[n*n*n];
	     try {
	    	 im.exportData(0, n*n*n, buffer);
	     }
	     catch (IOException e) {
	    	 System.err.println("IOException on im.exportData(0, n*n*n. buffer)");
	    	 System.exit(0);
	     }
	     int m = 3*n+1;
	     double pp[][][][][]  = new double[2][3][3*n+1][n+1][n+1];
	     double tmp[][][][] = new double[2][3*n+1][n+1][n+1];
	     double alpha = 2.0*(n+1.0)/(n*m);
	     double alphaArray[] = new double[3*n+1];
	     for (i = -3*n/2; i <= 3*n/2; i++) {
	    	 alphaArray[i+3*n/2] = alpha*i;
	     }
	     double PQ[][][] = new double[2][n+1][3*n+1];
	     double PZ[][][] = new double[2][3*(n+1)][3*n+1];
	
	     cfrftV3_precomp(PQ, PZ, n+1,alphaArray);
	
	     // Compute the pseudo-polar Fourier transform PP1
	     // pad the image to size m along the x direction
	     double pim[][][][] = new double[2][3*n+1][n][n];
	     for (i = 0; i < n; i++) {
	    	 for (p = 0; p < n; p++) {
	    		 for (q = 0; q < n; q++) {
	    			 pim[0][i+n][p][q] = buffer[i*n*n + p*n + q];
	    		 }
	    	 }
	     }
	     double cff1[][] = new double[2][3*n+1];
	     double fim[][][][] = new double[2][3*n+1][n][n];
	     for (p = 0; p < n; p++) {
	    	 for (q = 0; q < n; q++) {
	    		 for (i = 0; i < 3*n+1; i++) {
	    			 cff1[0][i] = pim[0][i][p][q];
	    			 cff1[1][i] = 0.0;
	    		 }
	    		 cff1 = ifftshift1d(cff1);
	    		 FFTUtility fft = new FFTUtility(cff1[0], cff1[1], 1, 3*n+1, 1, -1, FFTUtility.FFT);
	    		 fft.setShowProgress(false);
	    		 fft.run();
	    		 fft.finalize();
	    		 fft = null;
	    		 cff1 = fftshift1d(cff1);
	    		 for (i = 0; i < 3*n+1; i++) {
	    			 fim[0][i][p][q] = cff1[0][i];
	    			 fim[1][i][p][q] = cff1[1][i];
	    		 }
	    	 }
	     }
	     double tmp1[][][][] = new double[2][m][n][n+1]; // intermediate result after the first resampling. Referred as T1 in the paper.
	
	     int ofs_m=(int)Math.floor(m/2.0)+1;
	     //ofs_n=floor(n/2)+1;
	
	     double U[][][] = new double[2][n+1][n];
	     for (k=-3*n/2; k <= 3*n/2; k++) {
	     //     for l=-n/2:n/2-1
	     //         U = fim(k+ofs_m,l+ofs_n,:);
	     //         tmp1(k+ofs_m,l+ofs_n,:) = cfrftV3([U(:); 0],0,PQ,PZ,k+3*n/2+1);
	     //     end
	    	 for (p = 0; p < n; p++) {
	    		 for (q = 0; q < n; q++) {
	    			 U[0][q][p] = fim[0][k + ofs_m - 1][p][q];
	    			 U[1][q][p] = fim[1][k + ofs_m - 1][p][q];
	    		 }
	    	 }
	         double F[][][]=cfrftV3(U,0,PQ,PZ,k+3*n/2+1);
	         for (p = 0; p < n; p++) {
	        	 for (q = 0; q < n+1; q++) {
	        		 tmp1[0][k+ofs_m-1][p][q] = F[0][q][p];
	        		 tmp1[1][k+ofs_m-1][p][q] = F[1][q][p];
	        	 }
	         }
	     } // for (k=-3*n/2; k <= 3*n/2; k++)
	
	     double V[][][] = new double[2][n+1][n+1];
	     for (k=-3*n/2; k <= 3*n/2; k++) {
	     //     for j=-n/2:n/2
	     //         V = tmp1(k+ofs_m,:,j+ofs_n);
	     //         tmp(k+ofs_m,:,j+ofs_n) = cfrftV3([V(:); 0],0,PQ,PZ,k+3*n/2+1);
	     //     end 
	    	 for (p = 0; p < n; p++) {
	    		 for (q = 0; q < n+1; q++) {
	    			 V[0][p][q] = tmp1[0][k+ofs_m-1][p][q];
	    			 V[1][p][q] = tmp1[1][k+ofs_m-1][p][q];
	    		 }
	    	 }
	         double F[][][]=cfrftV3(V,0,PQ,PZ,k+3*n/2+1);
	         for (p = 0; p < n+1; p++) {
	        	 for (q = 0; q < n+1; q++) {
	                 tmp[0][k+ofs_m-1][p][q]=F[0][p][q];
	                 tmp[1][k+ofs_m-1][p][q]=F[1][p][q];
	        	 }
	         }
	     } // for (k=-3*n/2; k <= 3*n/2; k++)
	     
	     for (i = 0; i < 3*n+1; i++) {
		     for (p = 0; p < n+1; p++) {
		    	 for (q = 0; q < n+1; q++) {
		    		 pp[0][0][i][p][q] = tmp[0][i][n-p][n-q];
		    		 pp[1][0][i][p][q] = tmp[1][i][n-p][n-q];
		    	 }
		     }
	     }
	
	     // Compute the pseudo-polar Fourier transform PP2
	     // % pad the image to size m along the y direction
	     pim = new double[2][n][3*n+1][n];
	     for (i = 0; i < n; i++) {
	    	 for (p = 0; p < n; p++) {
	    		 for (q = 0; q < n; q++) {
	    			 pim[0][i][p+n][q] = buffer[i*n*n + p*n + q];
	    		 }
	    	 }
	     }
	     double cff2[][] = new double[2][3*n+1];
	     fim = new double[2][n][3*n+1][n];
	     for (i = 0; i < n; i++) {
	    	 for (q = 0; q < n; q++) {
	    		 for (p = 0; p < 3*n+1; p++) {
	    			 cff2[0][p] = pim[0][i][p][q];
	    			 cff2[1][p] = 0.0;
	    		 }
	    		 cff2 = ifftshift1d(cff2);
	    		 FFTUtility fft = new FFTUtility(cff2[0], cff2[1], 1, 3*n+1, 1, -1, FFTUtility.FFT);
	    		 fft.setShowProgress(false);
	    		 fft.run();
	    		 fft.finalize();
	    		 fft = null;
	    		 cff2 = fftshift1d(cff2);
	    		 for (p = 0; p < 3*n+1; p++) {
	    			 fim[0][i][p][q] = cff2[0][p];
	    			 fim[1][i][p][q] = cff2[1][p];
	    		 }
	    	 }
	     }
	     tmp1 = new double[2][n+1][m][n]; // intermediate result after the first resampling. Referred as T2 in the paper.
	
	     // The loop order (k,l,j) differs from PP1 and PP3 to keep consistency with
	     // the paper.
	     U = new double[2][n+1][n];
	     for (k=-3*n/2; k <= 3*n/2; k++) {
	     //     for j=-n/2:n/2-1
	     //         U = fim(:,k+ofs_m,j+ofs_n);
	     //         tmp1(:,k+ofs_m,j+ofs_n) = cfrftV3([U(:); 0],0,PQ,PZ,k+3*n/2+1);
	     //     end
	    	 for (i = 0; i < n; i++) {
	    		 for (q = 0; q < n; q++) {
	    			 U[0][i][q] = fim[0][i][k+ofs_m-1][q];
	    			 U[1][i][q] = fim[1][i][k+ofs_m-1][q];
	    		 }
	    	 }
	         double F[][][] =cfrftV3(U,0,PQ,PZ,k+3*n/2+1);
	         for (i = 0; i < n+1; i++) {
	        	 for (q = 0; q < n; q++) {
	        		 tmp1[0][i][k+ofs_m-1][q] = F[0][i][q];
	        		 tmp1[1][i][k+ofs_m-1][q] = F[1][i][q];
	        	 }
	         }
	     } // for (k=-3*n/2; k <= 3*n/2; k++)
	
	     V = new double[2][n+1][n+1];
	     for (k=-3*n/2; k <= 3*n/2; k++) {
	     //     for l=-n/2:n/2
	     //         V = tmp1(l+ofs_n,k+ofs_m,:);
	     //         tmp(k+ofs_m,l+ofs_n,:) = cfrftV3([V(:); 0],0,PQ,PZ,k+3*n/2+1);
	     //     end
	    	 for (i = 0; i < n+1; i++) {
	    		 for (q = 0; q < n; q++) {
	    			 V[0][q][i] = tmp1[0][i][k+ofs_m-1][q];
	    			 V[1][q][i] = tmp1[1][i][k+ofs_m-1][q];
	    		 }
	    	 }
	         double F[][][]=cfrftV3(V,0,PQ,PZ,k+3*n/2+1);
	         for (i = 0; i < n+1; i++) {
	        	 for (q = 0; q < n+1; q++) {
	                 tmp[0][k+ofs_m-1][i][q]=F[0][q][i];
	                 tmp[1][k+ofs_m-1][i][q]=F[1][q][i];
	             }
	         }
	
	     } // for (k=-3*n/2; k <= 3*n/2; k++)
	     
	     for (i = 0; i < 3*n+1; i++) {
	    	 for (p = 0; p < n+1; p++) {
	    		 for (q = 0; q < n+1; q++) {
	    			 pp[0][1][i][p][q] = tmp[0][i][n-p][n-q];
	    			 pp[1][1][i][p][q] = tmp[1][i][n-p][n-q];
	    		 }
	    	 }
	     }
	
	     
	
	     // Compute the pseudo-polar Fourier transform PP3
	     // pad the image to size m along the z direction
	     pim = new double[2][n][n][3*n+1];
	     for (i = 0; i < n; i++) {
	    	 for (p = 0; p < n; p++) {
	    		 for (q = 0; q < n; q++) {
	    			 pim[0][i][p][q+n] = buffer[i*n*n + p*n + q];
	    		 }
	    	 }
	     }
	     double cff3[][] = new double[2][3*n+1];
	     fim = new double[2][n][n][3*n+1];
	     for (i = 0; i < n; i++) {
	    	 for (p = 0; p < n; p++) {
	    		 for (q = 0; q < 3*n+1; q++) {
	    			 cff3[0][q] = pim[0][i][p][q];
	    			 cff3[1][q] = 0.0;
	    		 }
	    		 cff3 = ifftshift1d(cff3);
	    		 FFTUtility fft = new FFTUtility(cff3[0], cff3[1], 1, 3*n+1, 1, -1, FFTUtility.FFT);
	    		 fft.setShowProgress(false);
	    		 fft.run();
	    		 fft.finalize();
	    		 fft = null;
	    		 cff3 = fftshift1d(cff3);
	    		 for (q = 0; q < 3*n+1; q++) {
	    			 fim[0][i][p][q] = cff3[0][q];
	    			 fim[1][i][p][q] = cff3[1][q];
	    		 }
	    	 }
	     }
	     tmp1 = new double[2][n][n+1][m]; // intermediate result after the first resampling. Referred as T3 in the paper.
	
	     U = new double[2][n+1][n];
	     for (k=-3*n/2; k <= 3*n/2; k++) {
	     //     for l=-n/2:n/2-1
	     //         U = fim(l+ofs_n,:,k+ofs_m);
	     //         tmp1(l+ofs_n,:,k+ofs_m) = cfrftV3([U(:); 0],0,PQ,PZ,k+3*n/2+1);
	     //     end
	    	 for (i = 0; i < n; i++) {
	    		 for (q = 0; q < n; q++) {
	    			 U[0][q][i] = fim[0][i][q][k+ofs_m-1];
	    			 U[1][q][i] = fim[1][i][q][k+ofs_m-1];
	    		 }
	    	 }
	         double F[][][]=cfrftV3(U,0,PQ,PZ,k+3*n/2+1);
	         for (i = 0; i < n; i++) {
	        	 for (q = 0; q < n+1; q++) {
	        		tmp1[0][i][q][k+ofs_m-1] = F[0][q][i]; 
	        		tmp1[1][i][q][k+ofs_m-1] = F[1][q][i]; 
	        	 }
	         }  
	     } // for (k=-3*n/2; k <= 3*n/2; k++)
	
	     V = new double[2][n+1][n+1];
	     for (k=-3*n/2; k <= 3*n/2; k++) {
	     //     for j=-n/2:n/2
	     //         V = tmp1(:,j+ofs_n,k+ofs_m);
	     //         tmp(k+ofs_m,:,j+ofs_n) = cfrftV3([V(:); 0],0,PQ,PZ,k+3*n/2+1);
	     //     end
	    	 for (i = 0; i < n; i++) {
	    		 for (p = 0; p < n+1; p++) {
	    			 V[0][i][p] = tmp1[0][i][p][k+ofs_m-1];
	    			 V[1][i][p] = tmp1[1][i][p][k+ofs_m-1];
	    		 }
	    	 }   
	         double F[][][]=cfrftV3(V,0,PQ,PZ,k+3*n/2+1);
	         for (p = 0; p < n+1; p++) {
	        	 for (q = 0; q < n+1; q++) {
	        		 tmp[0][k+ofs_m-1][p][q] = F[0][p][q];
	        		 tmp[1][k+ofs_m-1][p][q] = F[1][p][q];
	        	 }
	         }   
	     } // for (k=-3*n/2; k <= 3*n/2; k++)
	
	     for (i = 0; i < 3*n+1; i++) {
	    	 for (p = 0; p < n+1; p++) {
	    		 for (q = 0; q < n+1; q++) {
	    			 pp[0][2][i][p][q] = tmp[0][i][n-p][n-q];
	    			 pp[1][2][i][p][q] = tmp[1][i][n-p][n-q];
	    		 }
	    	 }
	     }
	     return pp;
     }
     
     private double[][][][][] ppft3_ref(ModelImage im) {
    		 
    		 // Fast algorithm for computing the 3-D pseudo-polar Fourier transform.
    		 // The computation requires O(n^3logn) operations.
    		 
    		 // The function computes the 3-D pseudo-polar Fourier transform according to
    		 // the algorithm given in
    		 // "A. Averbuch and Y. Shkolnisky. 3D Fourier based discrete Radon
    		 // transform. Applied and Computational Harmonic Analysis, 15(1):33-69,
    		 // 2003."
    		 // This implementation follows exactly the pseudo-code and notations given
    		 // in the paper. See ppft3 for an optimized version of the algorithm.
    		 
    		 // Input:
    		 //     im    3-D image of size nxnxn (n even).
    		 //           First  index - x direction
    		 //           Second index - y direction
    		 //           Third  index - z direction
    		 //     The indices in each direction are assumed to be from -n/2 to n/2-1 and
    		 //     not from 1 to n.
    		 
    		 // Output:
    		 //     pp - 4-D array of size 3x(3n+1)x(n+1)x(n+1) containing the 3-D
    		 //     pseudo-polar Fourier transform.
    		 //     The array pp contains the following Fourier samples:
    		 //        pp(1,k,l,j) = FI(k,-2lk/n,-2jk/n)
    		 //        pp(2,k,l,j) = FI(-2lk/n,k,-2jk/n)
    		 //        pp(3,k,l,j) = FI(-2lk/n,-2jk/n,k)
    		 //        where
    		 //             l,j = -n/2,...,n/2   k=-3n/2,...3n/2
    		 //        and
    		 //                           n/2-1   n/2-1   n/2-1
    		 //             FI(ox,oy,oz)=  sum     sum     sum  I(u,v,w)exp(-2*pi*i(u*ox+v*oy+w*oz)/m)   m=3n+1
    		 //                           u=-n/2  v=-n/2  w=-n/2
    		 
    		 
    		 // Yoel Shkolnisky 30/01/03
    		 
    		 // Revisions:
    		 // Yoel Shkolnisky 19/5/2013  Renamed from ppft3 to ppft3_ref.

    		 int i,j,k,l,p,q;
    		 int coord[];
    		 int aCoord[];
    		 int N[];
    		 double GKN3Out[][];
    	     // verify that the input is a 3D image of size nxnxn
    		 verifyImage(im);
    		 // Initialize output data structure
    		 int n= im.getExtents()[0]; // at this point n is even
    	     double buffer[] = new double[n*n*n];
    	     try {
    	    	 im.exportData(0, n*n*n, buffer);
    	     }
    	     catch (IOException e) {
    	    	 System.err.println("IOException on im.exportData(0, n*n*n. buffer)");
    	    	 System.exit(0);
    	     }
    	     int m = 3*n+1;
    	     double pp[][][][][]  = new double[2][3][3*n+1][n+1][n+1];
    	     double tmp[][][][] = new double[2][3*n+1][n+1][n+1];

    		 // Compute the pseudo-polar Fourier transform PP1
    	     // pad the image to size m along the x direction
    	     double pim[][][][] = new double[2][3*n+1][n][n];
    	     for (i = 0; i < n; i++) {
    	    	 for (p = 0; p < n; p++) {
    	    		 for (q = 0; q < n; q++) {
    	    			 pim[0][i+n][p][q] = buffer[i*n*n + p*n + q];
    	    		 }
    	    	 }
    	     }
    	     double fim[][][][] = cfft3(pim);
    	     double tmp1[][][][] = new double[2][m][n][n+1]; // intermediate result after the first resampling. Referred as T1 in the paper

    	     double U[][] = new double[2][n];
    		 for (k=-3*n/2; k <= 3*n/2; k++) {
    		     for (l=-n/2; l <= n/2-1; l++) {
    		    	 aCoord = new int[] {k,l};
    		    	 N = new int[] {m,n};
    		         coord = toUnaliasedCoord(aCoord, N);
    		         for (q = 0; q < n; q++) {
    		        	 U[0][q] = fim[0][coord[0]][coord[1]][q];
    		        	 U[1][q] = fim[1][coord[0]][coord[1]][q];
    		         }
    		         GKN3Out = GKN3(U,k);
    		         for (q = 0; q < n+1; q++) {
    		        	 tmp1[0][coord[0]][coord[1]][q] = GKN3Out[0][q];
    		        	 tmp1[1][coord[0]][coord[1]][q] = GKN3Out[1][q];
    		         }
    		     } // for (l=-n/2; l <= n/2-1; l++)
    		 } // for (k=-3*n/2; k <= 3*n/2; k++ 

    		 double V[][] = new double[2][n];
    		 for (k=-3*n/2; k <= 3*n/2; k++) {
    		     for (j=-n/2; j <= n/2; j++) {
    		    	 aCoord = new int[] {k,j};
    		    	 N = new int[] {m,n};
    		         coord = toUnaliasedCoord(aCoord, N);
    		         for (p = 0; p < n; p++) {
    		        	 V[0][p] = tmp1[0][coord[0]][p][coord[1]];
    		        	 V[1][p] = tmp1[1][coord[0]][p][coord[1]];
    		         }
    		         GKN3Out = GKN3(V,k);
    		         for (p = 0; p < n+1; p++) {
    		        	 tmp[0][coord[0]][p][coord[1]] = GKN3Out[0][p];
    		        	 tmp[1][coord[0]][p][coord[1]] = GKN3Out[1][p];
    		         }
    		         
    		     } // for (j=-n/2; j <= n/2; j++)
    		 } // for (k=-3*n/2; k <= 3*n/2; k++)
    		 
    		 for (i = 0; i < 3*n+1; i++) {
    		     for (p = 0; p < n+1; p++) {
    		    	 for (q = 0; q < n+1; q++) {
    		    		 pp[0][0][i][p][q] = tmp[0][i][n-p][n-q];
    		    		 pp[1][0][i][p][q] = tmp[1][i][n-p][n-q];
    		    	 }
    		     }
    	     }

    		 // Compute the pseudo-polar Fourier transform PP2
    		 // pad the image to size m along the y direction
    		 pim = new double[2][n][3*n+1][n];
    	     for (i = 0; i < n; i++) {
    	    	 for (p = 0; p < n; p++) {
    	    		 for (q = 0; q < n; q++) {
    	    			 pim[0][i][p+n][q] = buffer[i*n*n + p*n + q];
    	    		 }
    	    	 }
    	     }
    		 
    		 fim  = cfft3(pim);
    		 tmp1 = new double[2][n+1][m][n]; // intermediate result after the first resampling. Referred as T2 in the paper.

    		 // The loop order (k,l,j) differs from PP1 and PP3 to keep consistency with
    		 // the paper.
    		 for (k=-3*n/2; k <= 3*n/2; k++) {
    		     for (j=-n/2; j <= n/2-1; j++) {
    		    	 aCoord = new int[] {k,j};
    		    	 N = new int[] {m,n};
    		         coord = toUnaliasedCoord(aCoord, N);
    		         for (i = 0; i < n; i++) {
    		        	 U[0][i] = fim[0][i][coord[0]][coord[1]];
    		        	 U[1][i] = fim[1][i][coord[0]][coord[1]];
    		         }
    		         GKN3Out = GKN3(U,k);
    		         for (i = 0; i < n+1; i++) {
    		        	 tmp1[0][i][coord[0]][coord[1]] = GKN3Out[0][i];
    		        	 tmp1[1][i][coord[0]][coord[1]] = GKN3Out[1][i];
    		         }
    		     } // for (j=-n/2; j <= n/2-1; j++)
    		 } // for (k=-3*n/2; k <= 3*n/2; k++)

    		 for (k=-3*n/2; k <= 3*n/2; k++) {
    		     for (l=-n/2; l <= n/2; l++) {
    		    	 aCoord = new int[] {k,l};
    		    	 N = new int[] {m,n};
    		         coord = toUnaliasedCoord(aCoord, N);
    		         for (q = 0; q < n; q++) {
    		              V[0][q] = tmp1[0][coord[1]][coord[0]][q];
    		              V[1][q] = tmp1[1][coord[1]][coord[0]][q];
    		         }
    		         GKN3Out = GKN3(V,k);
    		         for (q = 0; q < n+1; q++) {
    		        	 tmp[0][coord[0]][coord[1]][q] = GKN3Out[0][q];
    		        	 tmp[1][coord[0]][coord[1]][q] = GKN3Out[1][q];
    		         }
    		     } // for (l=-n/2; l <= n/2; l++)
    		 } // for (k=-3*n/2; k <= 3*n/2; k++)
    		 
    		 for (i = 0; i < 3*n+1; i++) {
    	    	 for (p = 0; p < n+1; p++) {
    	    		 for (q = 0; q < n+1; q++) {
    	    			 pp[0][1][i][p][q] = tmp[0][i][n-p][n-q];
    	    			 pp[1][1][i][p][q] = tmp[1][i][n-p][n-q];
    	    		 }
    	    	 }
    	     }

    		 // Compute the pseudo-polar Fourier transform PP3
             // pad the image to size m along the z direction
    		 pim = new double[2][n][n][3*n+1];
    	     for (i = 0; i < n; i++) {
    	    	 for (p = 0; p < n; p++) {
    	    		 for (q = 0; q < n; q++) {
    	    			 pim[0][i][p][q+n] = buffer[i*n*n + p*n + q];
    	    		 }
    	    	 }
    	     }
    		 fim  = cfft3(pim);
    		 tmp1 = new double[2][n][n+1][m]; // intermediate result after the first resampling. Referred as T3 in the paper.

    		 for (k=-3*n/2; k <= 3*n/2; k++) {
    		     for (l=-n/2; l <= n/2-1; l++) {
    		    	 aCoord = new int[] {k,l};
    		    	 N = new int[] {m,n};
    		         coord = toUnaliasedCoord(aCoord, N);
    		         for (p = 0; p < n; p++) {
    		        	 U[0][p] = fim[0][coord[1]][p][coord[0]];
    		        	 U[1][p] = fim[1][coord[1]][p][coord[0]];
    		         }
    		         GKN3Out = GKN3(U,k);
    		         for (p = 0; p < n+1; p++) {
    		        	 tmp1[0][coord[1]][p][coord[0]] = GKN3Out[0][p];
    		        	 tmp1[1][coord[1]][p][coord[0]] = GKN3Out[1][p];
    		         }
    		     } // for (l=-n/2; l <= n/2-1; l++)
    		 } // for (k=-3*n/2; k <= 3*n/2; k++)

    		 for (k=-3*n/2; k <= 3*n/2; k++) {
    		     for (j=-n/2; j <= n/2; j++) {
    		    	 aCoord = new int[] {k,j};
    		    	 N = new int[] {m,n};
    		         coord = toUnaliasedCoord(aCoord, N);
    		         for (i = 0; i < n; i++) {
    		        	 V[0][i] = tmp1[0][i][coord[1]][coord[0]];
    		        	 V[1][i] = tmp1[1][i][coord[1]][coord[0]];
    		         }
    		         GKN3Out = GKN3(V,k);
    		         for (p = 0; p < n+1; p++) {
    		        	 tmp[0][coord[0]][p][coord[1]] = GKN3Out[0][p];
    		        	 tmp[1][coord[0]][p][coord[1]] = GKN3Out[1][p];
    		         }
    		     } // for (j=-n/2; j <= n/2; j++)
    		 } // for (k=-3*n/2; k <= 3*n/2; k++)
    		 
    		 for (i = 0; i < 3*n+1; i++) {
    	    	 for (p = 0; p < n+1; p++) {
    	    		 for (q = 0; q < n+1; q++) {
    	    			 pp[0][2][i][p][q] = tmp[0][i][n-p][n-q];
    	    			 pp[1][2][i][p][q] = tmp[1][i][n-p][n-q];
    	    		 }
    	    	 }
    	     }
    	     return pp;

     }
     
    		 

}