package gov.nih.mipav.model.algorithms;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;

import Jama.Matrix;

public class Fastfit {

/**
    Original C and MATLAB code from fastfit and lightspeed packages by Tom Minka.
    Ported to Java by William Gandler 
	Fastfit
	=======

	This toolbox implements efficient maximum-likelihood estimation of various
	distributions.  It utilizes the technique of generalized Newton iteration,
	described in the following papers:

	[Beyond Newton's method](http://tminka.github.io/papers/newton.html)
	[Estimating a Dirichlet distribution](http://tminka.github.io/papers/dirichlet/)
	[Estimating a Gamma distribution](http://tminka.github.io/papers/minka-gamma.pdf)
	This toolbox requires the [Lightspeed toolbox](https://github.com/tminka/lightspeed/), so you will need to install both.

    See Contents.m for more details.

    A python port is available at https://github.com/ericsuh/dirichlet

    Tom Minka
    
    Lightspeed matlab toolbox
	=========================
	
	This library provides:
	
	* highly optimized versions of mathematical functions such as `normcdf`, set intersection, and `gammaln`
	* efficient random number generators
	* evaluation of common probability densities
	* routines for counting floating-point
	operations (FLOPS), useful for benchmarking algorithms.
	* utilities such as filename globbing and parsing of variable-length argument lists.
	* graphics functions such as `axis_pct` and `mobile_text` (in the graphics subdirectory).
	
	See Contents.m for a table of contents.

    Tom Minka

    
    MIT License

	Copyright (c) 2017 Tom Minka
	
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

	*/
	public double epsilon = 2.220446049250313E-16;
	
	public Fastfit() {
		
	}
	
	public void test_digamma() {
		// test_digamma() had no errors
		int i;
        double actual;
        double e;
        int numErrors = 0;
	    double x[] = new double[] {9, 2.5, 0.1, 7E-4, 7E-5, 7E-6, 2E-6, 1E-6, 7E-7, -0.5, -1.1};
	    double expected[] = new double[] {2.140641477955609996536345, 0.7031566406452431872257,
		    -10.42375494041107679516822, -1429.147493371120205005198,
		    -14286.29138623969227538398, -142857.7200612932791081972,
		    -500000.5772123750382073831, -1000000.577214019968668068,
		    -1428572.005785942019703646, .03648997397857652055902367,
		    10.15416395914385769902271}; 
	    for (i = 0; i < x.length; i++) {
	        actual = digamma(x[i]);	
	        e = Math.abs(actual - expected[i])/expected[i];
	    	if (e > 1.5e-12) {
	    		System.err.println("error = " + e);
	    		System.err.println("For digmma x = " + x[i] + " actual = " + actual + " expected = " + expected[i]);
	    		numErrors++;
	    	}
	    }
	    if (digamma(-1.0) != Double.NEGATIVE_INFINITY) {
	    		System.err.println("digamma(-1.0) is " + digamma(-1.0) + " instead of the expected -Infinity");
	    		numErrors++;
	    }
	    if (digamma(0) != Double.NEGATIVE_INFINITY) {
    		System.err.println("digamma(0) is " + digamma(0) + " instead of the expected -Infinity");
    		numErrors++;
        }
	    if (!Double.isNaN(digamma(Double.NEGATIVE_INFINITY))) {
	    	System.err.println("digamma(Double.NEGATIVE_INFINITY) is " + digamma(Double.NEGATIVE_INFINITY) + " instead of the expected NaN");
	        
	    }
	    if (numErrors > 0) {
	    	System.err.println("test_digamma() had " + numErrors + " errors");
	    }
	    else {
	    	System.out.println("test_digamma() had no errors");
	    }
	}
	
	public double digamma(double x) {
	    // DIGAMMA   Digamma function.
	    // DIGAMMA(X) returns digamma(x) = d log(gamma(x)) / dx
	    // If X is a matrix, returns the digamma function evaluated at each element.

		// Reference:
		
		//    J Bernardo,
	    //    Psi ( Digamma ) Function,
		//    Algorithm AS 103,
		//    Applied Statistics,
	    //    Volume 25, Number 3, pages 315-317, 1976.
			
	    // From http://www.psc.edu/~burkardt/src/dirichlet/dirichlet.f
		double large = 9.5;
		double d1 = -0.5772156649015328606065121;  // digamma(1)
		double d2 = (Math.PI*Math.PI)/6.0;
		double small = 1e-6;
		double s3 = 1.0/12.0;
		double s4 = 1.0/120.0;
		double s5 = 1.0/252.0;
		double s6 = 1.0/240.0;
		double s7 = 1.0/132.0;
		double s8 = 691.0/32760.0;
		double s9 = 1.0/12.0;
		double s10 = 3617.0/8160.0;

        double r;
		double y = 0.0;

		// illegal arguments
		if (x == Double.NEGATIVE_INFINITY || Double.isNaN(x)) {
			return Double.NaN;
		}
		

		// Negative values
		if (x < 0.0) {
		  // Use the reflection formula (Jeffrey 11.1.6):
		  // digamma(-x) = digamma(x+1) + pi*cot(pi*x)
		  if (x == Math.round(x)) {
			  y = Double.NEGATIVE_INFINITY;
		  }
		  else {
		      y = digamma(-x+1) + Math.PI/Math.tan(-Math.PI*x);
		  }
		  return y;
		  // This is related to the identity
		  // digamma(-x) = digamma(x+1) - digamma(z) + digamma(1-z)
		  // where z is the fractional part of x
		  // For example:
		  // digamma(-3.1) = 1/3.1 + 1/2.1 + 1/1.1 + 1/0.1 + digamma(1-0.1)
		  //               = digamma(4.1) - digamma(0.1) + digamma(1-0.1)
		  // Then we use
		  // digamma(1-z) - digamma(z) = pi*cot(pi*z)
		}
		
		if (x == 0.0) {
			return Double.NEGATIVE_INFINITY;
		}

		//  Use approximation if argument <= small.
		if (x > 0 & x <= small) {
		  y = d1 - 1.0/ x + d2*x;
		  return y;
		}

		//  Reduce to digamma(X + N) where (X + N) >= large.
		while( true) {
		  if (x <= small) {
			  return y;
		  }
		  if (x >= large) {
			  break;
		  }
		  y = y - 1.0/ x;
		  x = x + 1.0;
		}

		//  Use de Moivre's expansion if argument >= large.
		// In maple: asympt(Psi(x), x);
		// only x >= large can reach here
		//if (x >= large) {
		  r = 1.0/ x;
		  y = y + Math.log(x) - 0.5 * r;
		  r = r * r;
		  y = y - r * ( s3 - r * ( s4 - r * (s5 - r * (s6 - r * s7))));
		  return y;
		//} // if (x >= large)

	} // public double digamma(double x)
	
	public void test_trigamma() {
		// test_trigamma() had no errors
		int i;
        double actual;
        double e;
        int numErrors = 0;
	    double x[] = new double[] {9, 2.5, 0.1, 7E-4, 7E-5, 7E-6, 7E-7, -0.5, -1.1};
	    double expected[] = new double[] {.117512014694031425134547, .4903577561002348649728011,
		                                  101.4332991507927588172155, 2040817.969783389022409943,
		                                  204081634.2978270192803090, 20408163266.95103968719027,
		                                  2040816326532.257177281929, 8.934802200544679309417246,
		                                  102.7489862404689390536693}; 
	    for (i = 0; i < x.length; i++) {
	        actual = trigamma(x[i]);	
	        e = Math.abs(actual - expected[i])/expected[i];
	    	if (e > 1e-12) {
	    		System.err.println("error = " + e);
	    		System.err.println("For trigmma x = " + x[i] + " actual = " + actual + " expected = " + expected[i]);
	    		numErrors++;
	    	}
	    }
	    // Wolfram graph shows POSITIVE_INFINITY
	    if (trigamma(-1.0) != Double.POSITIVE_INFINITY) {
	    		System.err.println("trigamma(-1.0) is " + trigamma(-1.0) + " instead of the expected Infinity");
	    		numErrors++;
	    }
	    if (trigamma(0) != Double.POSITIVE_INFINITY) {
    		System.err.println("trigamma(0) is " + trigamma(0) + " instead of the expected Infinity");
    		numErrors++;
        }
	    if (!Double.isNaN(trigamma(Double.NEGATIVE_INFINITY))) {
	    	System.err.println("trigamma(Double.NEGATIVE_INFINITY) is " + trigamma(Double.NEGATIVE_INFINITY) + " instead of the expected NaN");
	        
	    }
	    if (numErrors > 0) {
	    	System.err.println("test_trigamma() had " + numErrors + " errors");
	    }
	    else {
	    	System.out.println("test_trigamma() had no errors");
	    }
	}
	
	public double trigamma(double x) {
		// TRIGAMMA   Trigamma function.
		// TRIGAMMA(X) returns trigamma(x) = d**2 log(gamma(x)) / dx**2
		// If X is a matrix, returns the trigamma function evaluated at each element.

		// Reference:
		
		//    B Schneider,
		//    Trigamma Function,
		//    Algorithm AS 121,
		//    Applied Statistics, 
		//    Volume 27, Number 1, page 97-99, 1978.
		
		// From http://www.psc.edu/~burkardt/src/dirichlet/dirichlet.f

		double small = 1e-4;
		double large = 8.0;
		double c = (Math.PI*Math.PI)/6.0;
		double c1 = -2.404113806319188570799476;
		double b2 =  1.0/6.0;
		double b4 = -1.0/30.0;
		double b6 =  1.0/42.0;
		double b8 = -1.0/30.0;
		double b10 = 5.0/66.0;

		// Initialize
		double var;
		double z;
		double y = 0.0;

		// illegal values
		if (x == Double.NEGATIVE_INFINITY || Double.isNaN(x)) {
			return Double.NaN;
		}

		// zero or negative integer
		if ((x <= 0) && (Math.floor(x) == x)) {
			return Double.POSITIVE_INFINITY;
		}
		

		// Negative non-integer
		if ((x < 0) & (Math.floor(x) != x)) { 
		  // Use the derivative of the digamma reflection formula:
		  // -trigamma(-x) = trigamma(x+1) - (pi*csc(pi*x))^2
		  var = Math.PI/Math.sin(-Math.PI*x);
		  y = -trigamma(-x+1) + var*var;
		  return y;
		}
		  
		// Small value approximation
		if ((x > 0) && (x <= small)) {
		  y = 1.0/(x*x) + c + c1*x;
		}

		// Reduce to trigamma(x+n) where ( X + N ) >= large.
		while(true) {
		  if (x <= small) {
			  return y;
		  }
		  if (x >= large) {
			  break;
		  }
		  y = y + 1.0/(x*x);
		  x = x + 1.0;
		} // while (true)

		// Apply asymptotic formula when X >= large
		// Onlhy x >= large reach this point
		//if (x >= large) {
		  z = 1.0/(x*x);
		  y = y + 0.5*z + (1.0 + z*(b2 + z*(b4 + z*(b6 + z*(b8 + z*b10))))) / x;
		  return y;
	    //} if (x >= large)
	} // public double trigamma(double x)
	

    // From the file random.c	
	static long ix = 101;
	static long iy = 1001;
	static long iz = 10001;
	static double RandN_previous = 0;
	static int RandN_usePrevious = 0;
	
	/*
	*  Generates a uniformly distributed r.v. between 0 and 1.
	*  Kris Popat  6/1985
	*  Ref: B. A. Wichmann and I. D. Hill, 
	*  "Algorithm AS 183: An efficient and portable pseudo-random number generator"
	*       Applied Statistics 31(2): 188-190, 1982
	*  Based on FORTRAN routine by H. Malvar.
	*  For Python code, see http://www.python.org/doc/2.3/lib/module-whrandom.html
	*/
	public double Rand()
	{
	  double u;
	  
	  ix = 171*(ix % 177)-2*(ix/177);
	  iy = 172*(iy % 176)-2*(iy/176);
	  iz = 170*(iz % 178)-2*(iz/178);
	  
	  if (ix<0) ix = ix + 30269;
	  if (iy<0) iy = iy + 30307;
	  if (iz<0) iz = iz + 30323;
	  
	  u = ((double) ix)/30269 +
	                ((double) iy)/30307 + ((double) iz)/30323;
	  u -= (double)(int)u;
	  return(u);
	}
	
	/* Resets Rand() to generate the same numbers again. */
	public void ResetSeed()
	{
	  SetSeed(101,1001,10001);
	}
	/* Sets the seed for Rand(). 
	 * The seed determines the sequence of numbers it generates.
	 * There is no constraint on the values provided, but zero is not recommended.
	 */
	public void SetSeed(long new_ix, long new_iy, long new_iz)
	{
	  ix = new_ix;
	  iy = new_iy;
	  iz = new_iz;
	  RandN_usePrevious = 0;
	}
	
	/* Gets the seed for Rand().
	 */
	public void GetSeed(long ix_out[], long iy_out[], long iz_out[])
	{
	  ix_out[0] = ix;
	  iy_out[0] = iy;
	  iz_out[0] = iz;
	  RandN_usePrevious = 0;
	}
	
	/* Returns a sample from Normal(0,1)
	 */
	public double RandN()
	{
	  double x,y,radius;
	  if(RandN_usePrevious != 0) {
	    RandN_usePrevious = 0;
	    return RandN_previous;
	  }
	  /* Generate a random point inside the unit circle */
	  do {
	    x = 2*Rand()-1;
	    y = 2*Rand()-1;
	    radius = (x*x)+(y*y);
	  } while((radius >= 1.0) || (radius == 0.0));
	  /* Box-Muller formula */
	  radius = Math.sqrt(-2*Math.log(radius)/radius);
	  x *= radius;
	  y *= radius;
	  RandN_previous = y;
	  RandN_usePrevious = 1;
	  return x;
	}
	
	public void test_randgamma() {
		// Expected meanx = a = 3.75 Actual meanx = 3.7533513786580834
		// Expected meanlogx = digamma(a) = 1.182537388611769 Actual meanlogx = 1.1819293270655182
		double a = 3.75;
		int i;
		double x;
		double xsum = 0.0;
		double logxsum = 0.0;
		for (i = 0; i < 10000; i++) {
		    x = GammaRand(a);	
		    xsum += x;
		    logxsum += Math.log(x);
		}
		
	    double meanx = xsum/10000.0;
	    double meanlogx = logxsum/10000.0;
	    System.out.println("Expected meanx = a = " + 3.75 + " Actual meanx = " + meanx);
	    System.out.println("Expected meanlogx = digamma(a) = " + digamma(a) + " Actual meanlogx = " + meanlogx);
	}
	
	/* Returns a sample from Gamma(a, 1).
	 * For Gamma(a,b), scale the result by b.
	 */
	public double GammaRand(double a)
	{
	  /* Algorithm:
	   * G. Marsaglia and W.W. Tsang, A simple method for generating gamma
	   * variables, ACM Transactions on Mathematical Software, Vol. 26, No. 3,
	   * Pages 363-372, September, 2000.
	   * http://portal.acm.org/citation.cfm?id=358414
	   */
	  double boost, d, c, v;
	  if(a < 1) {
	    /* boost using Marsaglia's (1961) method: gam(a) = gam(a+1)*U^(1/a) */
	    boost = Math.exp(Math.log(Rand())/a);
	    a++;
	  } 
	  else boost = 1;
	  d = a-1.0/3; c = 1.0/Math.sqrt(9*d);
	  while(true) {
	    double x,u;
	    do {
	      x = RandN();
	      v = 1+c*x;
	    } while(v <= 0);
	    v = v*v*v;
	    x = x*x;
	    u = Rand();
	    if((u < 1-.0331*x*x) || 
	       (Math.log(u) < 0.5*x + d*(1-v+Math.log(v)))) break;
	  }
	  return( boost*d*v );
	}

	/* Returns a sample from Beta(a,b) */
	public double BetaRand(double a, double b)
	{
	  double g = GammaRand(a);
	  return g/(g + GammaRand(b));
	}
	
	/* Very fast binomial sampler. 
	 * Returns the number of successes out of n trials, with success probability p.
	 */
	public long BinoRand(double p, long n)
	{
	  long r = 0;
	  if(Double.isNaN(p)) return 0;
	  if(p < epsilon) return 0;
	  if(p >= 1-epsilon) return n;
	  if((p > 0.5) && (n < 15)) {
	    /* Coin flip method. This takes O(n) time. */
	    int i;
	    for(i=0;i<n;i++) {
	      if(Rand() < p) r++;
	    }
	    return r;
	  }
	  if(n*p < 10) {
	    /* Waiting time method.  This takes O(np) time. */
	    double q = -Math.log(1-p), e = -Math.log(Rand()), s;
	    r = n;
	    for(s = e/r; s <= q; s += e/r) {
	      r--;
	      if(r == 0) break;
	      e = -Math.log(Rand());
	    }
	    r = n-r;
	    return r;
	  }
    /* Recursive method.  This makes O(log(log(n))) recursive calls. */
	  long i = (long)(p*(n+1));
    double b = BetaRand(i, n+1-i);
    if(b <= p) r = i + BinoRand((p-b)/(1-b), n-i);
    else r = i - 1 - BinoRand((b-p)/b, i-1);
    return r;
	}
	
	public double logdet(double A[][]) {
		// LOGDET  Logarithm of determinant for positive-definite matrix
		// logdet(A) returns log(det(A)) where A is positive-definite.
		// This is faster and more stable than using log(det(A)).
		// Note that logdet does not check if A is positive-definite.
		// If A is not positive-definite, the result will not be the same as log(det(A)).

		// Written by Tom Minka
		// (c) Microsoft Corporation. All rights reserved.

		int i,j;
		GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
		double U[][] = new double[A.length][A[0].length];
		for (i = 0; i < A.length; i++) {
			for (j = 0; j < A[0].length; j++) {
				U[i][j] = A[i][j];
			}
		}
   	    int info[] = new int[1];
		ge.dpotrf('U', A.length, U, A.length, info);
		 if (info[0] < 0) {
			System.err.println("In dpotrf argument " + (-info[0]) + " had an illegal value");
			System.exit(-1);
		 }
		 else if (info[0] > 0) {
			System.err.println("In dpotrf the leading minor of order " + info[0] + " is not positive definite,"); 
			System.err.println("and the factorization could not be completed.");
			System.exit(-1);
		 }
		 double y = 0.0;
		 for (i = 0; i < A.length; i++) {
			 y += Math.log(U[i][i]);
		 }
		 y = 2*y;
		 return y;
	}
	
	public void test_randwishart() {
		// Wishart(2.7) mean should be:
		// 2.7 0.0 0.0
		// 0.0 2.7 0.0
		// 0.0 0.0 2.7
		// Calculated Wishart mean: 
		// 2.6821589381350974 -8.111620486689514E-4 0.006292473681388858
		// -8.111620486689514E-4 2.719486591726699 0.009120196698769917
		// 0.006292473681388858 0.009120196698769917 2.7029981746555776
		// Calcuated E[logdet] = 1.5414578001447885
		// True E[logdet] = 1.5496244806057047
		// Calculated variance: 
		// 2.702851902491278 1.364881469129864 1.3577236947249294
		// 1.364881469129864 2.7427333771781077 1.3584130433904802
		// 1.3577236947249294 1.3584130433904802 2.6908994269574134
		// True variance: 
		// 2.7 1.35 1.35
		// 1.35 2.7 1.35
		// 1.35 1.35 2.7

		int N = 10000;
		double a = 2.7;
		int d = 3;
		double m[][] = new double[d][d];
		double m2[][] = new double[d][d];
		double s = 0;
		int i,j,k;
		double L[][];
		double X[][];
		for (i = 1; i <= N; i++) {
		  L = randwishart(a,d);
		  X = (((new Matrix(L)).transpose()).times(new Matrix(L))).getArray();
		  for (j = 0; j < m.length; j++) {
			  for (k = 0; k < m[0].length; k++) {
				  m[j][k] = m[j][k] + X[j][k];
			  }
		  }
		  s = s + logdet(X);
		  for (j = 0; j < m2.length; j++) {
			  for (k = 0; k < m2[0].length; k++) {
				  m2[j][k] = m2[j][k] + X[j][k]*X[j][k];
			  }
		  }
		} // for (i = 1; i <= N; i++)
		double sTrue = 0.0;
		for (i = 0; i <= d-1; i++) {
			sTrue += digamma(a - i*0.5);
		}
		
		for (j = 0; j < m.length; j++) {
			  for (k = 0; k < m[0].length; k++) {
				  m[j][k] = m[j][k]/N;
			  }
		  }
		s = s/N;
		double v[][] = new double[m2.length][m2[0].length];
		for (j = 0; j < m2.length; j++) {
			for (k = 0; k < m2[0].length; k++) {
				v[j][k] = m2[j][k]/N - m[j][k]*m[j][k];
			}
		}
		System.out.println("Wishart(2.7) mean should be:");
		System.out.println("2.7 0.0 0.0");
		System.out.println("0.0 2.7 0.0");
		System.out.println("0.0 0.0 2.7");
		System.out.println("Calculated Wishart mean: ");
		for (i = 0; i < 3; i++) {
			System.out.println(m[i][0] + " " + m[i][1] + " " + m[i][2]);
		}
		System.out.println("Calcuated E[logdet] = " + s);
		System.out.println("True E[logdet] = " + sTrue);
		double vTrue[][] = new double[d][d];
		for (i = 0; i < d; i++) {
			for (j = 0; j < d; j++) {
				if (i == j) {
					vTrue[i][j] = a;
				}
				else {
					vTrue[i][j] = a/2.0;
				}
			}
		}
		System.out.println("Calculated variance: ");
		for (i = 0; i < 3; i++) {
			System.out.println(v[i][0] + " " + v[i][1] + " " + v[i][2]);
		}
		System.out.println("True variance: ");
		for (i = 0; i < 3; i++) {
			System.out.println(vTrue[i][0] + " " + vTrue[i][1] + " " + vTrue[i][2]);
		}
	}
	
	public double[][] randwishart(double a, int d) {
		// RANDWISHART    Sample from Wishart distribution.
		
		// cholX = RANDWISHART(A,D) returns a DxD upper triangular matrix such that
		// cholX'*cholX is a sample from a Wishart distribution with shape parameter
		// A and unit scale.
		
		// The probability density function has the form:
		// p(X) = |X|^(a-(d+1)/2)*exp(-tr(X))/Gamma_d(a)
		// where Gamma_d is the multivariate Gamma function.

		// Written by Tom Minka
		// (c) Microsoft Corporation. All rights reserved.

		int i,j;
		double sqrth = Math.sqrt(0.5);
		double cholX[][] = new double[d][d];
		for (i = 0; i < d; i++) {
			for (j = i; j < d; j++) {
				cholX[i][j] = RandN()*sqrth;
			}
		}
		for (i = 0; i <= (d-1); i++) {
			cholX[i][i] = Math.sqrt(GammaRand(a - i*0.5));
		}
		return cholX;
	}
	
	public double[] logsumexp(double ain[][], int dim) {
		// Returns log(sum(exp(a),dim)) while avoiding numerical underflow.
		// Default is dim = 1 (columns).
		// logsumexp(a, 2) will sum across rows instead of columns.
		// Unlike matlab's "sum", it will not switch the summing direction
		// if you provide a row vector.

		// Written by Tom Minka
		// (c) Microsoft Corporation. All rights reserved.

		//if nargin < 2
		//  dim = 1;
		// end

		// subtract the largest in each column
		double y[];
		int i[];
		int j,k;
		double a[][] = new double[ain.length][ain[0].length];
		double s[] = null;
		double expsum;
		double logsum;
		for (j = 0; j < a.length; j++) {
			for (k = 0; k < a[0].length; k++) {
				a[j][k] = ain[j][k];
			}
		}
		double maxval;
		if (dim == 1) {
		    y = new double[a[0].length];
		    s = new double[a[0].length];
		    i = new int[a[0].length];
		    for (k = 0; k < a[0].length; k++) {
		        maxval = -Double.MAX_VALUE;
		        for (j = 0; j < a.length; j++) {
		            if (a[j][k] > maxval) {
		            	y[k] = maxval;
		            }
		        }
		    }
		    for (j = 0; j < a.length; j++) {
		    	for (k = 0; k < a[0].length; k++) {
		    		a[j][k] = a[j][k] - y[k];
		    	}
		    }
		    for (k = 0; k < a[0].length; k++) {
		    	expsum = 0.0;
		        for (j = 0; j < a.length; j++) {
		        	expsum += Math.exp(a[j][k]);
		        }
		        logsum = Math.log(expsum);
		        if (Double.isInfinite(y[k])) {
		        	s[k] = y[k];
		        }
		        else {
		            s[k] = y[k] + logsum;
		        }
		    }
		}
		else if (dim == 2) {
			y = new double[a.length];
			s = new double[a.length];
			i = new int[a.length];
			for (j = 0; j < a.length; j++) {
				maxval = -Double.MAX_VALUE;
				for (k = 0; k < a[0].length; k++) {
					if (a[j][k] > maxval) {
						y[j] = maxval;
					}
				}
			}
			for (j = 0; j < a.length; j++) {
				for (k = 0; k < a[0].length; k++) {
					a[j][k] = a[j][k] - y[j];
				}
			}
			for (j = 0; j < a.length; j++) {
				expsum = 0.0;
				for (k = 0; k < a[0].length; k++) {
					expsum += Math.exp(a[j][k]);
				}
				logsum = Math.log(expsum);
				if (Double.isInfinite(y[j])) {
					s[j]= y[j];
				}
				else {
				  s[j] = y[j] + logsum;
				}
			}
		}
		return s;
	}

}