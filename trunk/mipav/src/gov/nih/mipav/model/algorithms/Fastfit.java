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
	
	/* Evaluate the tetragamma function (the derivative of the trigamma function)
	 */
	public double tetragamma(double x)
	{
	  double
	    small = 1e-4,
	    large = 8,
	    tetragamma1 = -2.404113806319188570799476,  /* -2 Zeta(3) */
			pentagamma1 = 6.49393940226682914909602217, /* 6 Zeta(4) */
	    b2 =  1./6,
	    b4 = -1./30,
	    b6 =  1./42,
	    b8 = -1./30,
	    b10 = 5./66;
	  double result;
	  /* Illegal arguments */
	  if((x == Double.NEGATIVE_INFINITY) || Double.isNaN(x)) {
	    return Double.NaN;
	  }
	  /* Singularities */
	  if((x <= 0) && (Math.floor(x) == x)) {
	    return Double.NEGATIVE_INFINITY;
	  }
	  /* Negative values */
	  /* Use the derivative of the trigamma reflection formula:
	   * -trigamma(-x) = trigamma(x+1) - (pi*csc(pi*x))^2
	   * tetragamma(-x) = tetragamma(x+1) + 2*pi^3*cos(pi*x)/sin(pi*x)^3
	   */
	  if(x < 0) {
			double pix = Math.PI*x;
			double cospix = Math.cos(pix);
	    double cscpix = Math.PI/Math.sin(pix);
			double cscpix3 = cscpix*cscpix*cscpix;
	    return tetragamma(1-x) + 2*cscpix3*cospix;
	  }
	  /* Use Taylor series if argument <= small */
	  if(x <= small) {
	    return -2/(x*x*x) + tetragamma1 + pentagamma1*x;
	  }
	  result = 0;
	  /* Reduce to tetragamma(x+n) where ( X + N ) >= B */
	  while(x < large) {
	    result -= 2/(x*x*x);
	    x++;
	  }
	  /* Apply asymptotic formula when X >= B */
	  /* This expansion can be computed in Maple via asympt(Psi(2,x),x) */
	  if(x >= large) {
	    double r = 1/(x*x), t;
	    t = (5*b4 + r*(7*b6 + r*(9*b8 + r*11*b10)));
	    result -= r/x + r*(1 + r*(3*b2 + r*t));
	  }
	  return result;
	}
	

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
	
	public void test_logsumexp() {
		// logsumexp handled [-Inf; -Inf] correctly
		double x[][] = new double[2][1];
		x[0][0] = Double.NEGATIVE_INFINITY;
		x[1][0] = Double.NEGATIVE_INFINITY;
		double ans[] = logsumexp(x,0);
		if ((ans.length != 1) || (ans[0] != Double.NEGATIVE_INFINITY)) {
			System.err.println("logsumexp[-Inf; -Inf] should be -Inf");
		}
		else {
			System.out.println("logsumexp handled [-Inf; -Inf] correctly");
		}
		
	}
	
	public double[] logsumexp(double ain[][], int dim) {
		// Returns log(sum(exp(a),dim)) while avoiding numerical underflow.
		// Default is dim = 0 (columns).
		// logsumexp(a, 1) will sum across rows instead of columns.
		// Unlike matlab's "sum", it will not switch the summing direction
		// if you provide a row vector.

		// Written by Tom Minka
		// (c) Microsoft Corporation. All rights reserved.

		//if nargin < 2
		//  dim = 0;
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
		if (dim == 0) {
		    y = new double[a[0].length];
		    s = new double[a[0].length];
		    i = new int[a[0].length];
		    for (k = 0; k < a[0].length; k++) {
		        y[k] = -Double.MAX_VALUE;
		        for (j = 0; j < a.length; j++) {
		            if (a[j][k] > y[k]) {
		            	y[k] = a[j][k];
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
		else if (dim == 1) {
			y = new double[a.length];
			s = new double[a.length];
			i = new int[a.length];
			for (j = 0; j < a.length; j++) {
				y[j] = -Double.MAX_VALUE;
				for (k = 0; k < a[0].length; k++) {
					if (a[j][k] >y[j]) {
						y[j] = a[j][k];
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
	
	public void test_logmulexp() {
		// i = 0 j = 0 logmulexp = 0.563803136450568 true answer = 0.5638031364505682
		// i = 0 j = 1 logmulexp = 0.47737565601251886 true answer = 0.47737565601251886
		// i = 0 j = 2 logmulexp = 0.8948147556170905 true answer = 0.8948147556170905
		// i = 0 j = 3 logmulexp = 0.7411210877603489 true answer = 0.741121087760349
		// i = 1 j = 0 logmulexp = 0.7643419913938545 true answer = 0.7643419913938545
		// i = 1 j = 1 logmulexp = 0.6779145109558053 true answer = 0.6779145109558052
		// i = 1 j = 2 logmulexp = 1.095353610560377 true answer = 1.0953536105603767
		// i = 1 j = 3 logmulexp = 0.9416599427036354 true answer = 0.9416599427036354
		// i = 2 j = 0 logmulexp = 0.9994491204781111 true answer = 0.9994491204781112
		// i = 2 j = 1 logmulexp = 0.9130216400400619 true answer = 0.9130216400400619
		// i = 2 j = 2 logmulexp = 1.3304607396446335 true answer = 1.3304607396446335
		// i = 2 j = 3 logmulexp = 1.176767071787892 true answer = 1.176767071787892
		int i,j;
	    double a[][] = new double[3][1];
	    double b[][] = new double[1][4];
	    double abtrue[][] = new double[3][4];
	    for (i = 0; i < 3; i++) {
	    	a[i][0] = Rand();
	    }
	    for (i = 0; i < 4; i++) {
	    	b[0][i] = Rand();
	    }
	    for (i = 0; i < 3; i++) {
	    	for (j = 0; j < 4; j++) {
	    	    abtrue[i][j] = Math.log(Math.exp(a[i][0])*Math.exp(b[0][j]));	
	    	}
	    }
	    double abcalc[][] = logmulexp(a,b);
	    for (i = 0; i < 3; i++) {
	    	for (j = 0; j <4; j++) {
	    		System.out.println("i = " + i + " j = " + j + " logmulexp = " + abcalc[i][j] + " true answer = " + abtrue[i][j]);
	    	}
	    }
	}
	
	public double[][] logmulexp(double a[][], double b[][]) {
		// LOGMULEXP        Matrix multiply in the log domain.
		// logmulexp(a,b) returns log(exp(a)*exp(b)) while avoiding numerical underflow.
		// The * is matrix multiplication.

		// Written by Tom Minka
		// (c) Microsoft Corporation. All rights reserved.
        int i,j,k;
        // Must have a[0].length = b.length;
		// s = repmat(a,cols(b),1) + kron(b',ones(rows(a),1));
		// s = reshape(logsumexp(s,2),rows(a),cols(b));
		double repa[][] = new double[a.length*b[0].length][a[0].length];
		for (i = 0; i < b[0].length; i++) {
			for (j = 0; j < a.length; j++) {
				for (k = 0; k < a[0].length; k++) {
					repa[i*a.length + j][k] = a[j][k];
				}
			}
		}
		double bT[][] = new double[b[0].length][b.length];
		for (i = 0; i < b.length; i++) {
			for (j = 0; j < b[0].length; j++) {
				bT[j][i] = b[i][j];
			}
		}
		double kb[][] = new double[a.length*b[0].length][b.length];
		for (i = 0; i < a.length; i++) {
			for (j = 0; j < b[0].length; j++) {
				for (k = 0; k < b.length; k++) {
				    kb[i + j*a.length][k] = bT[j][k];
				}
			}
		}
        double s[][] = new double[a.length*b[0].length][a[0].length];
        for (i = 0; i < a.length*b[0].length; i++) {
        	for (j = 0; j < a[0].length; j++) {
        		s[i][j] = repa[i][j] + kb[i][j];
        	}
        }
        double ans[] = logsumexp(s,1);
        s = new double[a.length][b[0].length];
        for (j = 0; j < b[0].length; j++) {
            for (i = 0; i < a.length; i++) {
        		s[i][j] = ans[i + j*a.length];
        	}
        }
        return s;
		// s = kron(a',ones(1,cols(b))) + repmat(b,1,rows(a));
		// s = reshape(logsumexp(s),cols(b),rows(a))';
	}
	
	public void test_normcdf() {
		// normcdf gave 0.6179114221889526
		// Statistics.GAUSSIAN_PROBABILITY_INTEGRAL gave 0.6179113580156126
		double x = 0.3;
		double m = 0.0;
		double s = 1.0;
		double ans = normcdf(x, m, s);
		double ans2[] = new double[1];
		Statistics stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, x, 0.0, ans2);
		stat.run();
		System.out.println("normcdf gave " + ans);
		System.out.println("Statistics.GAUSSIAN_PROBABILITY_INTEGRAL gave " + ans2[0]);
	}
	
	public double normcdf(double x, double m, double s) {
		// Default m = 0.0, s = 1.0
		// NORMCDF   Normal Cumulative Density Function.
		// P = NORMCDF(X) returns the probability that a standard normal variate will
		// be less than X.
		
		// P = NORMCDF(X,M,S) returns the probability that a normal variate with
		// mean M and standard deviation S will be less than x.
		double p;
		x = (x-m)/s;
		double var = x/Math.sqrt(2.0);
		double result[] = new double[1];

		Cephes ce = new Cephes(var, Cephes.ERF, result);
		ce.run();
		p = 0.5*result[0] + 0.5;
		return p;
	}
	
	public double normcdfln(double x) {
		// NORMCDFLN   log of normal cumulative density function.
		// More accurate than log(normcdf(x)) when x is small.
		// The following is a quick and dirty approximation to normcdfln:
		// normcdfln(x) =approx -(log(1+exp(0.88-x))/1.5)^2

		// Written by Tom Minka
		// (c) Microsoft Corporation. All rights reserved.
		double e;
		double t = -6.5;
		double y, z;
		if (x >= t) {
			e = Math.log(normcdf(x, 0.0, 1.0));
			return e;
		}
		else {
		    z = 1.0/(x*x);
		    // asymptotic series for logcdf
		    // subs(x=-x,asympt(log(gauss_cdf(-x)),x));
		    double c[] = new double[] {-1.0, 5.0/2.0, -37.0/3.0, 353.0/4.0, -4081.0/5.0, 55205.0/6.0, -854197.0/7.0};
		    y = z*(c[0]+z*(c[1]+z*(c[2]+z*(c[3]+z*(c[4]+z*(c[5]+z*c[6]))))));
		    e = y -0.5*Math.log(2.0*Math.PI) -0.5*x*x - Math.log(-x);
		    return e;
		}
	}
	
	public double normcdflogit(double x) {
		// NORMCDFLOGIT   log(normcdf/(1-normcdf))
		// More accurate than explicitly evaluating log(normcdf/(1-normcdf)), and
		// retains more precision than normcdfln when x is large.

		// Written by Tom Minka
		// (c) Microsoft Corporation. All rights reserved.

		double e;
		double small = -7.0;
		double large = 7.0;
		if ((x >= small) && (x <= large)) {
		  e = normcdf(x, 0.0, 1.0);
		  e = Math.log(e/(1.0-e));
		}
		else if (x < small) {
		  e = normcdfln(x);
		}
		else { // x > large
		  e = -normcdfln(-x);
		}
		return e;
	}
	
	public void test_cholproj() {
		// i = 0 j = 0 A = 10.0 Arec = 10.000000000000002
		// i = 0 j = 1 A = 5.0 Arec = 5.0
		// i = 0 j = 2 A = 2.0 Arec = 2.0
		// i = 1 j = 0 A = 5.0 Arec = 5.0
		// i = 1 j = 1 A = 3.0 Arec = 3.0
		// i = 1 j = 2 A = 2.0 Arec = 2.0
		// i = 2 j = 0 A = 2.0 Arec = 2.0
		// i = 2 j = 1 A = 2.0 Arec = 2.0
		// i = 2 j = 2 A = 3.0 Arec = 3.0
		int i,j;
    	double A[][] = new double[][] {{10.0, 5.0, 2.0},{5.0, 3.0, 2.0},{2.0, 2.0, 3.0}};
    	boolean isPosDef[] = new boolean[1];
    	double U[][] = cholproj(A, isPosDef);
    	double Arec[][] = (((new Matrix(U)).transpose()).times(new Matrix(U))).getArray();
    	for (i = 0; i < 3; i++) {
    		for (j = 0; j < 3; j++) {
    			System.out.println("i = " + i + " j = " + j + " A = " + A[i][j] + " Arec = " + Arec[i][j]);
    		}
    	}
	}

	public double[][] cholproj(double A[][], boolean isPosDef[]) {
		// CHOLPROJ  Projected Cholesky factorization.
		// cholproj(A) returns an upper triangular matrix U so that U'*U = A,
		// provided A is symmetric positive semidefinite (sps).
		
		// If A is not sps, then U will approximately satisfy U'*U = A.   
		// This is useful when dealing with matrices that are affected
		// by roundoff errors.  By multiplying U'*U you effectively round A to the 
		// nearest sps matrix.
		//
		// U = cholproj(A, isPOSdEF) also returns whether A is positive definite.

		double U[][] = new double[A.length][A[0].length];
		isPosDef[0] = true;
		int i,j,k;
		double s;
		double usum;
		for (i = 0; i < A.length; i++) {
		  for (j = i; j < A[0].length; j++) {
			usum = 0.0;
		    for (k = 0; k <= (i-1); k++) {
		    	 usum += U[k][i]*U[k][j];
		    }
		    s = A[i][j] - usum;
		    if (i == j) {
		      if (s <= 0) {
			      isPosDef[0] = false;
			      U[i][i] = 0.0;
		      }
		      else { // s > 0
			      U[i][i] = Math.sqrt(s);
		      }
		    }
		    else {// i != j
		      if (U[i][i] > 0) {
			      U[i][j] = s / U[i][i];
		      }
		      else {
			      U[i][j] = 0;
		      }
		    } // else i != j
		} // for (j = i; j < A.length; j++)
	  } // for (i = 0; i < A[0].length; i++)
	  return U;
	}
	
	/* Logarithm of multivariate Gamma function, defined as
	 * Gamma_d(x) = pi^(d*(d-1)/4)*prod_(i=1..d) Gamma(x + (1-i)/2)
	 * http://en.wikipedia.org/wiki/Multivariate_gamma_function
	 */
	double gammaln2(double x, double d)
	{
	  double M_lnPI = 1.14472988584940;
	  double r = d*(d-1)/4*M_lnPI;
	  int i;
	  for(i=0; i<d; i++) r += gammaln(x - 0.5*i);
	  return r;
	}
	
	public void test_gammaln() {
		// test_gammaln() passed all tests
		int i;
		int numFailed = 0;
		double pairs[][] = new double[][] {{0.1, 2.2527126517342059598697},
		{0.6 ,.39823385806923489961685},
		{0.7, .26086724653166651438573},
		{1.0, 0},
		{2.0, 0},
		{3.4, 1.0923280598027415674947},
		{4.0, 1.791759469228055000812477},
		{8.0, 8.525161361065414300165531},
		{64.0, 201.00931639928152667928},
		{256.0, 1161.71210111840065079}};
	    double err;
		for (i = 0; i < pairs.length; i++) {
		    err = Math.abs(gammaln(pairs[i][0]) - pairs[i][1])/pairs[i][0];	
		    if (err > 1.0E-10) {
		    	System.err.println("For gammaln("+pairs[i][0]+") error = " + err);
		    	numFailed++;
		    }
		}
		
		err = Math.abs(gammaln2(1.1,2) - 0.920726359734123);
		if (err > 1.0E-10) {
		    System.out.println("Error for gammaln2 = " + err);
		    numFailed++;
		}
		if (gammaln(0) != Double.POSITIVE_INFINITY) {
		    System.err.println("gammaln(0) = " + gammaln(0) + " instead of the correct Infinity");
		    numFailed++;
		}
		if (!Double.isNaN(gammaln(-1))) {
			System.err.println("gammaln(-1) = " + gammaln(-1) + " instead of the correct NaN");
		    numFailed++;
		}
		if (gammaln(Double.POSITIVE_INFINITY) != Double.POSITIVE_INFINITY) {
			System.err.println("gammaln(Double.POSITIVE_INFINITY) = " + gammaln(Double.POSITIVE_INFINITY) + " instead of the correct Infinity");
		    numFailed++;
		}
		
		// should be NaN?
		//gammaln(-Inf)
		if (!Double.isNaN(gammaln(Double.NaN))) {
			System.err.println("gammaln(Double.NaN) = " + gammaln(Double.NaN) + " instead of the correct NaN");
		    numFailed++;
		}
		
        if (numFailed == 0) {
        	System.out.println("test_gammaln() passed all tests");
        }

	}

	/* Logarithm of the gamma function.
	   Returns NaN for negative arguments, even though log(gamma(x)) may
	   actually be defined.
	*/
	double gammaln(double x)
	{
	  double M_lnSqrt2PI = 0.91893853320467274178;
	  double gamma_series[] = new double[]{
	    76.18009172947146,
	    -86.50532032941677,
	    24.01409824083091,
	    -1.231739572450155,
	    0.1208650973866179e-2,
	    -0.5395239384953e-5
	  };
	  int i;
	  double denom, x1, series;
	  if (Double.isNaN(x)) return Double.NaN;
	  if(x < 0) return Double.NaN;
	  if(x == 0) return Double.POSITIVE_INFINITY;
	  if(Double.isInfinite(x)) return x;
	  /* Lanczos method */
	  denom = x+1;
	  x1 = x + 5.5;
	  series = 1.000000000190015;
	  for(i = 0; i < 6; i++) {
	    series += gamma_series[i] / denom;
	    denom += 1.0;
	  }
	  return( M_lnSqrt2PI + (x+0.5)*Math.log(x1) - x1 + Math.log(series/x) );
	}
	
	public double wishpdfln(double X, double a, double B, boolean inverse) {
		// WISHPDFLN    Logarithm of Wishart probability density function.
		//  See WISHPDF for argument description.

		// Written by Tom Minka
		// (c) Microsoft Corporation. All rights reserved.

		//if nargin < 3
		//  B = null;
		//end
		//if nargin < 4
		//  inverse = false;
		//end
		int d;
		double d2;
		double XB;
		double logDetB;
		double logDetXB;
		double lp;
        
		if (inverse) {
		  X = 1.0/X;
		}
		if (B == Double.NaN) {
		  XB = X;
		  logDetB = 0;
		}
		else {
		  XB = X*B;
		  logDetB = Math.log(B);
		}
		d = 1;
		d2 = (d+1.0)/2.0;
		if (inverse) {
		  d2 = -d2;
		}
		logDetXB = (a-d2)*Math.log(XB);
		lp = logDetXB - XB + d2*logDetB - gammaln2(a,d);
		return lp;
	}

	
	public double wishpdfln(double Xin[][], double a, double B[][], boolean inverse) {
		// WISHPDFLN    Logarithm of Wishart probability density function.
		//  See WISHPDF for argument description.

		// Written by Tom Minka
		// (c) Microsoft Corporation. All rights reserved.

		//if nargin < 3
		//  B = null;
		//end
		//if nargin < 4
		//  inverse = false;
		//end
		int d,i,j;
		double d2;
		double XB[][];
		double logDetB;
		double logDetXB;
		double lp;
		double traceXB;
        double X[][] = new double[Xin.length][Xin[0].length];
        for (i = 0; i < X.length; i++) {
        	for (j = 0; j < X[0].length; j++) {
        		X[i][j] = Xin[i][j];
        	}
        }
		if (inverse) {
		  X = ((new Matrix(X)).inverse()).getArray();
		}
		if (B == null) {
		  XB = X;
		  logDetB = 0;
		}
		else {
		  XB = ((new Matrix(X)).times(new Matrix(B))).getArray();
		  logDetB = logdet(B);
		}
		d = X.length;
		d2 = (d+1.0)/2.0;
		if (inverse) {
		  d2 = -d2;
		}
		logDetXB = (a-d2)*logdet(XB);
		traceXB = 0.0;
		for (i = 0; i < XB.length; i++) {
			traceXB += XB[i][i];
		}
		lp = logDetXB - traceXB + d2*logDetB - gammaln2(a,d);
		return lp;
	}
	
	public void test_wishpdf() {
		// wishpdf passed 2 tests
		// gampdf(2,3,4)
		int numFailed = 0;
		double p = 0.018954;
		double q = wishpdf(2,3,1.0/4.0,false);
		if (Math.abs(q - p) > 1e-6) {
			System.err.println("wishpdf failed on scalars");
			numFailed++;
		}

		double x[][] = new double[][]{{2, 1}, {1, 3}};
		double a = 5;
		p = 0.0038062;
		double B[][] = null;
		q = wishpdf(x,a, B, false);
		if (Math.abs(q - p) > 1e-6) {
			System.err.println("wishpdf failed on matrix");
			numFailed++;
		}
		
		if (numFailed == 0) {
			System.out.println("wishpdf passed 2 tests");
		}

	}
	
	public double wishpdf(double X, double a, double B, boolean inverse) {
		// WISHPDF     Wishart probability density function.
		// WISHPDF(X,A) returns the density at X under a Wishart distribution with 
		// shape parameter A and unit scale parameter.  
		// X is a positive definite matrix and A is scalar.
		// WISHPDF(X,A,B) specifies the scale parameter of the distribution (a 
		// positive definite matrix with the same size as X).
		
		// The probability density function has the form:
		// p(X) = |X|^(a-(d+1)/2)*exp(-tr(X*B))*|B|^a/Gamma_d(a)
		// where Gamma_d is the multivariate Gamma function.
		
		// WISHPDF(X,A,B,'inv') returns the density at X under an inverse Wishart
		// distribution.  The probability density function for an inverse Wishart is:
		// p(X) = |X|^(-a-(d+1)/2)*exp(-tr(inv(X)*B))*|B|^a/Gamma_d(a)

		// Written by Tom Minka
		// (c) Microsoft Corporation. All rights reserved.
        double p;
		p = Math.exp(wishpdfln(X, a, B, inverse));
		return p;
    }
	
	public double wishpdf(double X[][], double a, double B[][], boolean inverse) {
		// WISHPDF     Wishart probability density function.
		// WISHPDF(X,A) returns the density at X under a Wishart distribution with 
		// shape parameter A and unit scale parameter.  
		// X is a positive definite matrix and A is scalar.
		// WISHPDF(X,A,B) specifies the scale parameter of the distribution (a 
		// positive definite matrix with the same size as X).
		
		// The probability density function has the form:
		// p(X) = |X|^(a-(d+1)/2)*exp(-tr(X*B))*|B|^a/Gamma_d(a)
		// where Gamma_d is the multivariate Gamma function.
		
		// WISHPDF(X,A,B,'inv') returns the density at X under an inverse Wishart
		// distribution.  The probability density function for an inverse Wishart is:
		// p(X) = |X|^(-a-(d+1)/2)*exp(-tr(inv(X)*B))*|B|^a/Gamma_d(a)

		// Written by Tom Minka
		// (c) Microsoft Corporation. All rights reserved.
        double p;
		p = Math.exp(wishpdfln(X, a, B, inverse));
		return p;
    }

    int CACHE_SIZE = 200;
    
    public void test_pochhammer() {
    	// test_pochhammer() passes
    	// For pochhammer(5.6, 0) actual result = 0.0 correct result = 0.0
		// For pochhammer(1.0E4, 3) actual result = 27.63132109093155 correct result = 27.63132109093154778328
		// For pochhammer(1.0E4, 1000) actual result = 9258.704693976295 correct result = 9258.70469397627827724045
		// For pochhammer(1.0E5, 3) actual result = 34.53880639466069 correct result = 34.538806394660688260
		// For pochhammer(1.0E6, 3) actual result = 41.44653467389032 correct result = 41.44653467389032232
		// For pochhammer(0.1, 100) actual result = 357.3415591371228 correct result = 357.3415591371226056543663
		// For slow_pochhammer(5.6, 0) actual result = 0.0 correct result = 0.0
		// For slow_pochhammer(1.0E4, 3) actual result = 27.631321090931547 correct result = 27.63132109093154778328
		// For slow_pochhammer(1.0E4, 1000) actual result = 9258.704693976295 correct result = 9258.70469397627827724045
		// For slow_pochhammer(1.0E5, 3) actual result = 34.53880639466069 correct result = 34.538806394660688260
		// For slow_pochhammer(1.0E6, 3) actual result = 41.44653467389032 correct result = 41.44653467389032232
		// For slow_pochhammer(0.1, 100) actual result = 357.3415591372267 correct result = 357.3415591371226056543663
    	double poch;
    	poch = pochhammer(5.6, 0);
    	System.out.println("For pochhammer(5.6, 0) actual result = " + poch + " correct result = 0.0");
    	poch = pochhammer(1.0E4, 3);
    	System.out.println("For pochhammer(1.0E4, 3) actual result = " + poch + " correct result = 27.63132109093154778328");
    	poch = pochhammer(1.0E4, 1000);
    	System.out.println("For pochhammer(1.0E4, 1000) actual result = " + poch + " correct result = 9258.70469397627827724045");
    	poch = pochhammer(1.0E5, 3);
    	System.out.println("For pochhammer(1.0E5, 3) actual result = " + poch + " correct result = 34.538806394660688260");
    	poch = pochhammer(1.0E6, 3);
    	System.out.println("For pochhammer(1.0E6, 3) actual result = " + poch + " correct result = 41.44653467389032232");
    	poch = pochhammer(0.1, 100);
    	System.out.println("For pochhammer(0.1, 100) actual result = " + poch + " correct result = 357.3415591371226056543663");
    	
    	poch = slow_pochhammer(5.6, 0);
    	System.out.println("For slow_pochhammer(5.6, 0) actual result = " + poch + " correct result = 0.0");
    	poch = slow_pochhammer(1.0E4, 3);
    	System.out.println("For slow_pochhammer(1.0E4, 3) actual result = " + poch + " correct result = 27.63132109093154778328");
    	poch = slow_pochhammer(1.0E4, 1000);
    	System.out.println("For slow_pochhammer(1.0E4, 1000) actual result = " + poch + " correct result = 9258.70469397627827724045");
    	poch = slow_pochhammer(1.0E5, 3);
    	System.out.println("For slow_pochhammer(1.0E5, 3) actual result = " + poch + " correct result = 34.538806394660688260");
    	poch = slow_pochhammer(1.0E6, 3);
    	System.out.println("For slow_pochhammer(1.0E6, 3) actual result = " + poch + " correct result = 41.44653467389032232");
    	poch = slow_pochhammer(0.1, 100);
    	System.out.println("For slow_pochhammer(0.1, 100) actual result = " + poch + " correct result = 357.3415591371226056543663");
    }
    
    /* Requires: n >= 0 */
    public double pochhammer(double x, int n)
    {
      double cache_x = -1;
      double cache_v[] = new double[CACHE_SIZE];
      int max_cached = 1;
      double result;
      int i;
      /* the maximum n for which we have a cached value */
      if(n == 0) return 0;
      if(n > CACHE_SIZE) {
        if(x >= 1.e4*n) {
          return Math.log(x) + (n-1)*Math.log(x+n/2);
        }
        return gammaln(x+n) - gammaln(x);
      }
      if(x != cache_x) {
        max_cached = 1;
        cache_v[0] = Math.log(x);
        cache_x = x;
      }
      if(n <= max_cached) return cache_v[n-1];
      result = cache_v[max_cached-1];
      x = x + max_cached-1;
      for(i=max_cached;i<n;i++) {
        x = x + 1;
        result += Math.log(x);
        cache_v[i] = result;
      }
      max_cached = n;
      return result;
    }
    
    /* Requires: n >= 0 */
    public double slow_pochhammer(double x, int n)
    {
      double result;
      if(n == 0) return 0;
      if(n <= 20) {
        int i;
        double xi = x;
        /* this assumes x is not too large */
        result = xi;
        for(i=n-1; i > 0; i--) {
          xi = xi + 1;
          result *= xi;
        }
        result = Math.log(result);
      }
      else if(x >= 1.e4*n) {
        result = Math.log(x) + (n-1)*Math.log(x+n/2);
      }
      else result = gammaln(x+n) - gammaln(x);
      return result;
    }
    
    public double di_pochhammer(double x, int n)
    {
      double cache_x = -1;
      double cache_v[] = new double[CACHE_SIZE];
      int max_cached = 1;
      double result;
      int i;
      /* the maximum n for which we have a cached value */
      if(n == 0) return 0;
      if(n > CACHE_SIZE) {
        return digamma(x+n) - digamma(x);
      }
      if(x != cache_x) {
        max_cached = 1;
        cache_v[0] = 1/x;
        cache_x = x;
      }
      if(n <= max_cached) return cache_v[n-1];
      result = cache_v[max_cached-1];
      x = x + max_cached-1;
      for(i=max_cached;i<n;i++) {
        x = x + 1;
        result += 1/x;
        cache_v[i] = result;
      }
      max_cached = n;
      return result;
    }
    
    public double slow_di_pochhammer(double x, int n)
    {
      double result;
      if(n == 0) return 0;
      if(n <= 20) {
        int i;
        double xi = x;
        result = 1/xi;
        for(i=n-1; i > 0; i--) {
          xi = xi + 1;
          result += 1/xi;
        }
      }
      else result = digamma(x+n) - digamma(x);
      return result;
    }
    
    public double tri_pochhammer(double x, int n)
    {
      double cache_x = -1;
      double cache_v[] = new double[CACHE_SIZE];
      int max_cached = 1;
      double result;
      int i;
      /* the maximum n for which we have a cached value */
      if(n == 0) return 0;
      if(n > CACHE_SIZE) {
        return trigamma(x+n) - trigamma(x);
      }
      if(x != cache_x) {
        max_cached = 1;
        cache_v[0] = -1/(x*x);
        cache_x = x;
      }
      if(n <= max_cached) return cache_v[n-1];
      result = cache_v[max_cached-1];
      x = x + max_cached-1;
      for(i=max_cached;i<n;i++) {
        x = x + 1;
        result -= 1/(x*x);
        cache_v[i] = result;
      }
      max_cached = n;
      return result;
    }
    
    public double slow_tri_pochhammer(double x, int n)
    {
      double result;
      if(n == 0) return 0;
      if(n <= 20) {
        result = -1/(x*x);
        n--;
        while(n > 0) {
          x = x + 1;
          result -= 1/(x*x);
          n--;
        }
        return result;
      }
      return trigamma(x+n) - trigamma(x);
    }
    
    public double[][] solve_tril(double T[][], double b[][]) {
    	// SOLVE_TRIL      Left division by lower triangular matrix.
    	// SOLVE_TRIL(T,b) is the same as T\b but requires T to be lower triangular 
    	// and runs faster.
    	GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    	char side='L',uplo='L',trans='N',diag='N';
    	int m = T.length;
    	int n = T[0].length;
    	/* n = cols(b) */
    	if (m != n) {
    		System.err.println("T must be square in solve_tril");
    		System.exit(-1);
    	}
    	double alpha = 1.0;
    	double x[][] = new double[b.length][b[0].length];
    	int i,j;
    	for (i = 0; i < x.length; i++) {
    		for (j = 0; j < x[0].length; j++) {
    			x[i][j] = b[i][j];
    		}
    	}
    	ge.dtrsm(side, uplo, trans, diag, m, n, alpha, T, m, x, m);
    	return x;
    }
    
    public double[][] solve_tril2(double T[][], double b[][]) {
    	int m = T.length;
    	int n = T[0].length;
    	/* n = cols(b) */
    	if (m != n) {
    		System.err.println("T must be square in solve_tril");
    		System.exit(-1);
    	}
    	double x[][] = new double[b.length][b[0].length];
    	int i,j,k;
    	/* Lower triangular */
    	  for(j=0;j<n;j++) x[0][j] = b[0][j]/T[0][0];
    	  for(i=1;i<m;i++) {
    	    for(j=0;j<n;j++) {
    	      double s = 0;
    	      for(k=0;k<i;k++) {
    		s += T[i][k]*x[k][j];
    	      }
    	      x[i][j] = (b[i][j] - s)/T[i][i];
    	    }
    	  }
    	  return x;
    }
    
    public void test_solve_tri() {
    	// ansU[0][0] = 1.0 ansU2[0][0] = 1.0
		// ansU[0][1] = 2.0 ansU2[0][1] = 2.0
		// ansU[0][2] = 3.0 ansU2[0][2] = 3.0
		// ansU[1][0] = 4.0 ansU2[1][0] = 4.0
		// ansU[1][1] = 5.0 ansU2[1][1] = 5.0
		// ansU[1][2] = 6.0 ansU2[1][2] = 6.0
		// ansU[2][0] = 7.0 ansU2[2][0] = 7.0
		// ansU[2][1] = 8.0 ansU2[2][1] = 8.0
		// ansU[2][2] = 9.0 ansU2[2][2] = 9.0
		// ansL[0][0] = 0.1 ansL2[0][0] = 0.1
		// ansL[0][1] = 0.2 ansL2[0][1] = 0.2
		// ansL[0][2] = 0.3 ansL2[0][2] = 0.3
		// ansL[1][0] = 1.6 ansL2[1][0] = 1.6
		// ansL[1][1] = 2.0 ansL2[1][1] = 2.0
		// ansL[1][2] = 2.4 ansL2[1][2] = 2.4
		// ansL[2][0] = 1.129032258064516 ansL2[2][0] = 1.129032258064516
		// ansL[2][1] = 1.2903225806451613 ansL2[2][1] = 1.2903225806451613
		// ansL[2][2] = 1.4516129032258065 ansL2[2][2] = 1.4516129032258065
    	int i,j;
    	double U[][] = new double[][] {{1.0, 0.0, 0.0},{0.5, 1.0, 0.0},{-0.3, -0.04, 1.0}};
    	double L[][] = new double[][] {{10.0, -7.0, 0.0},{0.0, 2.5, 5.0}, {0.0, 0.0, 6.2}};
    	double b[][] = new double[][] {{1.0, 2.0, 3.0},{4.0,5.0,6.0},{7.0,8.0,9.0}};
    	double ansU[][] = solve_triu(U, b);
    	double ansU2[][] = solve_triu2(U,b);
    	for (i = 0; i < 3; i++) {
    		for (j = 0; j < 3; j++) {
    		    System.out.println("ansU["+i+"]["+j+"] = " + ansU[i][j] + " ansU2["+i+"]["+j+"] = " + ansU2[i][j]);
    		}
    	}
    	double ansL[][] = solve_tril(L, b);
    	double ansL2[][] = solve_tril2(L,b);
    	for (i = 0; i < 3; i++) {
    		for (j = 0; j < 3; j++) {
    		    System.out.println("ansL["+i+"]["+j+"] = " + ansL[i][j] + " ansL2["+i+"]["+j+"] = " + ansL2[i][j]);
    		}
    	}
    }
    
    public double[][] solve_triu(double T[][], double b[][]) {
    	// SOLVE_TRIU      Left division by upper triangular matrix.
    	// SOLVE_TRIU(T,b) is the same as T\b but requires T to be upper triangular 
    	// and runs faster.
    	GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    	char side='L',uplo='U',trans='N',diag='N';
    	int m = T.length;
    	int n = T[0].length;
    	/* n = cols(b) */
    	if (m != n) {
    		System.err.println("T must be square in solve_triu");
    		System.exit(-1);
    	}
    	double alpha = 1.0;
    	double x[][] = new double[b.length][b[0].length];
    	int i,j;
    	for (i = 0; i < x.length; i++) {
    		for (j = 0; j < x[0].length; j++) {
    			x[i][j] = b[i][j];
    		}
    	}
    	ge.dtrsm(side, uplo, trans, diag, m, n, alpha, T, m, x, m);
    	return x;
    }
    
    public double[][] solve_triu2(double T[][], double b[][]) {
    	int m = T.length;
    	int n = T[0].length;
    	/* n = cols(b) */
    	if (m != n) {
    		System.err.println("T must be square in solve_triu");
    		System.exit(-1);
    	}
    	double x[][] = new double[b.length][b[0].length];
    	int i,j,k;
    	/* Upper triangular */
    	  for(j=0;j<n;j++) x[m-1][j] = b[m-1][j]/T[m-1][m - 1];
    	  for(i=m-2;i>=0;i--) {
    	    for(j=0;j<n;j++) {
    	      double s = 0;
    	      for(k=i+1;k<m;k++) {
    		s += T[i][k]*x[k][j];
    	      }
    	      x[i][j] = (b[i][j] - s)/T[i][i];
    	    }
    	  }   
    	return x;
    }
    
    public double[][] inv_triu(double U[][]) {
		// INV_TRIU     Invert upper triangular matrix.

		// Singularity test: 
		// inv_triu([1 1; 0 0])
    	double B[][] = new double[U.length][U.length];
    	for (int i = 0; i < U.length; i++) {
    		B[i][i] = 1.0;
    	}

		double x[][] = solve_triu(U,B);
		return x;
    }
    
    public void test_inv_posdef() {
    	// AIdent[0][0] = 0.999999999999996
        // AIdent[0][1] = 5.329070518200751E-15
    	// AIdent[0][2] = -1.7763568394002505E-15
    	// AIdent[1][0] = -4.440892098500626E-16
    	// AIdent[1][1] = 1.0000000000000018
        // AIdent[1][2] = -8.881784197001252E-16
    	// AIdent[2][0] = -8.881784197001252E-16
    	// AIdent[2][1] = 0.0
    	// AIdent[2][2] = 0.9999999999999996
    	int i,j;
    	double A[][] = new double[][] {{10.0, 5.0, 2.0},{5.0, 3.0, 2.0},{2.0, 2.0, 3.0}};
        //double A[][] = new double[][] {{2.0, -1.0, 0.0}, {-1.0, 2.0, -1.0},{0.0, -1.0, 2.0}};
        double Ainv[][] = inv_posdef(A);
        double AIdent[][] = ((new Matrix(A)).times(new Matrix(Ainv))).getArray();
        for (i = 0; i < 3; i++) {
        	for (j = 0; j < 3; j++) {
        		System.out.println("AIdent["+i+"]["+j+"] = " + AIdent[i][j]);
        	}
        }
    }
    
    public double[][] inv_posdef(double A[][]) {
		// INV_POSDEF        Invert positive definite matrix.
		// INV_POSDEF(A) is like INV(A) but faster and more numerically stable.
		// See test_inv_posdef for a timing test.

		// Written by Tom Minka
        boolean isPosDef[] = new boolean[1];
		double U[][] = cholproj(A,isPosDef);
		int i,j;
		double iU[][] = inv_triu(U);
		double x[][] = ((new Matrix(iU)).times((new Matrix(iU)).transpose())).getArray();
		return x;
    }
    
    public double invnormcdf(double p) {
		// INVNORMCDF(P)  Normal quantile function
		// X = INVNORMCDF(P) returns the P-th quantile of the standard normal distribution.
		// In other words, it returns X such that P = NORMCDF(X).

		Erfinv erf = new Erfinv();
    	double x = erf.erfinv(2*p-1)*Math.sqrt(2);
		return x;
    }
    
    public double[] col_sum(double x[][]) {
		// COL_SUM   Sum for each column.
		// A more readable alternative to sum(x,0).
		// s = sum(x,0);
    	int i,j;
    	double sum[] = new double[x[0].length];
    	for (j = 0; j < x[0].length; j++) {
    		for (i = 0; i < x.length; i++) {
    			sum[j] += x[i][j];
    		}
    	}
    	return sum;
    }
    
    public double[] row_sum(double x[][]) {
		// ROW_SUM   Sum for each row.
		// A faster and more readable alternative to sum(x,1).

		// unfortunately, this removes any sparseness of x.
		// = x*ones(cols(x),1);
    	int i,j;
    	double sum[] = new double[x.length];
    	for (i = 0; i < x.length; i++) {
    		for (j = 0; j < x[0].length; j++) {
    			sum[i] += x[i][j];
    		}
    	}
    	return sum;
    }

    
    public double[] mvnormpdf(double x[][], double m[][], double S[][], double iS[][], double V[][], double iV[][]) {
    	// MVNORMPDF    Multivariate normal probability density function.
    	// MVNORMPDF(x) returns a row vector giving the density at each column of x 
    	//   under a standard multivariate normal.
    	// MVNORMPDF(x,m) subtracts m from x first.  
    	//   If cols(m) == 1, subtracts m from each column of x.
    	//   If cols(m) == cols(x), subtracts corresponding columns.
    	//   If cols(x) == 1, x is repeated to match cols(m).
    	// MVNORMPDF(x,m,S) specifies the standard deviation, or more generally
    	//   an upper triangular Cholesky factor of the covariance matrix.
    	//   In the univariate case, multiple standard deviations can be specified.
    	//   If m is empty, no subtraction is done (zero mean).
    	// MVNORMPDF(x,m,[],V) specifies the variance or covariance matrix.
    	// MVNORMPDF(x,m,'inv',iV) specifies the inverse of the covariance matrix, i.e.
    	//   the precision matrix.
    	// MVNORMPDF(x,m,iS,'inv') specifies the reciprocal of the standard deviation,
    	//   or more generally the upper triangular Cholesky factor of the
    	//   inverse covariance matrix.
    	//   This is the most efficient option.
    	// See test_normpdf for a timing test.

    	// this may look strange, but computing normpdf directly is no faster or
    	// more stable than exp(mvnormpdfln).
    	int i;
    	double p[] = mvnormpdfln(x, m, S, iS, V, iV);
        for (i = 0; i < p.length; i++) {
        	p[i] = Math.exp(p[i]);
        }
        return p;
    }
    
    public double[] mvnormpdfln(double x[][], double m[][], double S[][], double iS[][], double V[][], double iV[][]) {
    	// MVNORMPDFLN    log of multivariate normal density.
    	//   See MVNORMPDF for argument description.
    	double log2pi = 1.83787706640935;
    	int d = x.length;
    	int n = x[0].length;
    	double dx[][] = null;
    	int sz[] = new int[2];
    	int nm;
    	int i,j;
    	double p[] = null;
    	double dxdx[][] = null;
    	double cs[] = null;
    	boolean have_inv = false;
    	double logdetiS[] = null;
    	if (m == null) {
    	    dx = x;	
    	}
    	else {
    		// m specified
    		sz[0] = m.length;
    		sz[1] = m[0].length;
    		if (sz[0] != d) {
    		    System.err.println("rows(m) != rows(x) in mvnormpdfln");
    		    System.exit(-1);
    		}
    		nm = sz[1];
    		if (nm == 1) {
    		    dx = new double[d][n];
    		    for (i = 0; i < d; i++) {
    		        for (j = 0; j < n; j++) {
    		            dx[i][j] = x[i][j] - m[i][0];	
    		        }
    		    }
    		} // if (nm == 1)
    		else if (n == 1) {
    		    dx = new double[d][nm];	
    		    for (i = 0; i < d; i++) {
    		    	for (j = 0; j < nm; j++) {
    		    		dx[i][j] = x[i][0] - m[i][j];
    		    	}
    		    }
    		} // else if (n == 1)
    		else if (nm == n) {
    		    dx = new double[d][n];
    		    for (i = 0; i < d; i++) {
    		    	for (j = 0; j < n; j++) {
    		    		dx[i][j] = x[i][j] - m[i][j];
    		    	}
    		    }
    		} // else if (nm == n)
    		else {
    			System.err.println("Incompatible number of columns in x and m in mvnormpdfln");
    			System.exit(-1);
    		}
    	} // else m specified
    	if ((S == null) && (iS == null) && (V == null) && (iV == null)) {
    		// unit variance
    		dxdx = new double[dx.length][dx[0].length];
    		for (i = 0; i < dx.length; i++) {
    			for (j = 0; j < dx[0].length; j++) {
    				dxdx[i][j] = dx[i][j]*dx[i][j];
    			}
    		}
    		cs = col_sum(dxdx);
    		p = new double[cs.length];
    		for (i = 0; i < p.length; i++) {
    			p[i] = -0.5*(d*log2pi + cs[i]);
    		}
    		return p;
    	} // if ((S == null) && (iS == null) && (V == null) && (iV == null))
    	if ((S != null) && (iS == null) && (V == null) && (iV == null)) {
    	    // standard deviation given	
    		if (d == 1) {
    			// if (d == 1) dx.length == 1
    		    if ((S.length == 1) && (S[0].length == 1)) {
		        	for (j = 0; j < dx[0].length; j++) {
		        		dx[0][j] = dx[0][j]/S[0][0];
		        	}
    		        dxdx = new double[1][dx[0].length];
	    			for (j = 0; j < dx[0].length; j++) {
	    				dxdx[0][j] = dx[0][j]*dx[0][j];
	    			}
	    			p = new double[dx[0].length];
	    			for (i = 0; i < dx[0].length; i++) {
	    				p[i] = (-Math.log(S[0][0]) -0.5*log2pi) - 0.5*(dxdx[0][i]);	
	    			}
	    			return p;
    		    } // if ((S.length == 1) && (S[0].length == 1))
    		    else if ((S.length == dx[0].length) && (S[0].length == 1)) {
    		    	for (j = 0; j < dx[0].length; j++) {
		        		dx[0][j] = dx[0][j]/S[j][0];
		        	}
    		        dxdx = new double[1][dx[0].length];
	    			for (j = 0; j < dx[0].length; j++) {
	    				dxdx[0][j] = dx[0][j]*dx[0][j];
	    			}
	    			p = new double[dx[0].length];
	    			for (i = 0; i < dx[0].length; i++) {
	    				p[i] = (-Math.log(S[i][0]) -0.5*log2pi) - 0.5*(dxdx[0][i]);	
	    			}
	    			return p;	
    		    } // else if ((S.length == dx[0].length) && (S[0].length == 1))
    		    else if ((S.length == 1) && (S[0].length == dx[0].length)) {
    		    	for (j = 0; j < dx[0].length; j++) {
		        		dx[0][j] = dx[0][j]/S[0][j];
		        	}
    		        dxdx = new double[1][dx[0].length];
	    			for (j = 0; j < dx[0].length; j++) {
	    				dxdx[0][j] = dx[0][j]*dx[0][j];
	    			}
	    			p = new double[dx[0].length];
	    			for (i = 0; i < dx[0].length; i++) {
	    				p[i] = (-Math.log(S[0][i]) -0.5*log2pi) - 0.5*(dxdx[0][i]);	
	    			}
	    			return p;	
    		    } // else if ((S.length == 1) && (S[0].length == dx[0].length))
    		    else {
    		    	System.err.println("For d == 1 S lengths and dx[0].length do not match in mvnormpdfln");
    		    	System.exit(-1);
    		    }
    		} // if (d == 1)
    		if (S[1][0] != 0) {
    	        System.err.println("S is not upper triangular in mvnorfmpdfln");
    	        System.exit(-1);
    		}
    		if ((S.length != d) || (S[0].length != d)) {
    		    System.err.println("S is not the right size in mvnormpdfln");
    		    System.exit(-1);
    		}
    	} // if ((S != null) && (iS == null) && (V == null) && (iV == null))
    	else {
    		if (iS != null) {
    		    // inverse stddev given	
    			have_inv = true;
    		}
    		else if (iV != null) {
    		    // inverse variance given
    			if (d == 1) {
    			    iS = new double[iV.length][iV[0].length];	
    			    for (i = 0; i < iS.length; i++) {
    			    	for (j = 0; j < iS[0].length; j++) {
    			    		iS[i][j] = Math.sqrt(iV[i][j]);
    			    	}
    			    }
    			} // if (d == 1)
    			else { // d != 1
    				iS = new double[iV.length][iV[0].length];	
    			    for (i = 0; i < iS.length; i++) {
    			    	for (j = 0; j < iS[0].length; j++) {
    			    		iS[i][j] = iV[i][j];
    			    	}
    			    }
    			    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    			    int info[] = new int[1];
    			    ge.dpotrf('U',iS.length,iS,iS.length,info);
    			} // else d != 1
    			have_inv = true;
    		} // else if (iV != null)
    		else { // variance given
    			if (d == 1) {
    			    S = new double[V.length][V[0].length];	
    			    for (i = 0; i < S.length; i++) {
    			    	for (j = 0; j < S[0].length; j++) {
    			    		S[i][j] = Math.sqrt(V[i][j]);
    			    	}
    			    }
    			} // if (d == 1)
    			else { // d != 1
    				S = new double[V.length][V[0].length];	
    			    for (i = 0; i < S.length; i++) {
    			    	for (j = 0; j < S[0].length; j++) {
    			    		S[i][j] = V[i][j];
    			    	}
    			    }
    			    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    			    int info[] = new int[1];
    			    ge.dpotrf('U',S.length,S,S.length,info);
    			} // else d != 1	
    		} // else variance given
    	} // else
    	if (have_inv) {
    	    if (d == 1) {
    	    	// if (d == 1) dx.length == 1
    	    	for (j = 0; j < dx[0].length; j++) {
    	    	    dx[0][j] = iS[0][j] * dx[0][j];	
    	    	}
    	    	logdetiS = new double[iS[0].length];
    	    	for (i = 0; i < logdetiS.length; i++) {
    	    		logdetiS[i] = Math.log(iS[0][i]);
    	    	}
    	    } // if (d == 1)
    	    else { // d != 1
    	        dx = ((new Matrix(iS)).times(new Matrix(dx))).getArray();	
    	        logdetiS = new double[1];
    	        for (i = 0; i < iS.length; i++) {
    	        	logdetiS[0] += Math.log(iS[i][i]);
    	        }
    	    } // else d != 1
    	} // if (have_inv)
    	else { // !have_inv
    	    if (d == 1) {
    	    	if ((S.length == 1) && (S[0].length == 1)) {
		        	for (j = 0; j < dx[0].length; j++) {
		        		dx[0][j] = dx[0][j]/S[0][0];
		        	}
		        	logdetiS = new double[1];
	    	    	logdetiS[0] = -Math.log(S[0][0]);
    	    	}
    	    	 else if ((S.length == dx[0].length) && (S[0].length == 1)) {
     		    	for (j = 0; j < dx[0].length; j++) {
 		        		dx[0][j] = dx[0][j]/S[j][0];
 		        	}
     		    	logdetiS = new double[S.length];
     		    	for (i = 0; i < S.length; i++) {
     		    		logdetiS[i] = -Math.log(S[i][0]);
     		    	}
    	    	 }
    	    	 else if ((S.length == 1) && (S[0].length == dx[0].length)) {
     		    	for (j = 0; j < dx[0].length; j++) {
 		        		dx[0][j] = dx[0][j]/S[0][j];
 		        	}
     		    	logdetiS = new double[S[0].length];
     		    	for (i = 0; i < S[0].length; i++) {
     		    		logdetiS[i] = -Math.log(S[0][i]);
     		    	}
    	    	 }
    	    } // if (d == 1)
    	    else { // d != 1
    	    	double ST[][] = new double[S[0].length][S.length];
    	    	for (i = 0; i < S.length; i++) {
    	    		for (j = 0; j < S[0].length; j++) {
    	    			ST[j][i] = S[i][j];
    	    		}
    	    	}
    	    	dx = solve_tril(ST,dx);	
    	    	logdetiS = new double[1];
    	    	for (i = 0; i < iS.length; i++) {
    	        	logdetiS[0] -= Math.log(iS[i][i]);
    	        }
    	    } // else d != 1
    	} // else !have_inv
    	dxdx = new double[dx.length][dx[0].length];
		for (i = 0; i < dx.length; i++) {
			for (j = 0; j < dx[0].length; j++) {
				dxdx[i][j] = dx[i][j]*dx[i][j];
			}
		}
		cs = col_sum(dxdx);
		p = new double[cs.length];
		for (i = 0; i < p.length; i++) {
			p[i] = (logdetiS[Math.min(logdetiS.length-1,i)] -0.5*d*log2pi) - 0.5*cs[i];
		}
		return p;
    }
    
    public double[][] sqdist(double p[][], double q[][], double A[][]) {
		// SQDIST      Squared Euclidean or Mahalanobis distance.
		// SQDIST(p,q)   returns m(i,j) = (p(:,i) - q(:,j))'*(p(:,i) - q(:,j)).
		// SQDIST(p,q,A) returns m(i,j) = (p(:,i) - q(:,j))'*A*(p(:,i) - q(:,j)).

		// Written by Tom Minka

    	int i,j;    	
    	//[d, pn] = size(p);
		//[d, qn] = size(q);
    	int d = p.length;
    	int pn = p[0].length;
    	int qn = q[0].length;
    	double m[][] = null;
    	double pmag[] = null;
    	double qmag[] = null;
		if ((pn == 0) || (qn == 0)) {
		  m = new double[pn][qn];
		  return m;
		}

		if (A == null) {
		  double pp[][] = new double[d][pn];
		  for (i = 0; i < d; i++) {
			  for (j = 0; j < pn; j++) {
			      pp[i][j] = p[i][j]*p[i][j];	  
			  }
		  }
		  pmag = col_sum(pp);
		  double qq[][] = new double[d][qn];
		  for (i = 0; i < d; i++) {
			  for (j = 0; j < qn; j++) {
				  qq[i][j] = q[i][j]*q[i][j];
			  }
		  }
		  qmag = col_sum(qq);
		  double qp[][] = new double[pn][qn];
		  for (i = 0; i < pn; i++) {
			  for (j = 0; j < qn; j++) {
				  qp[i][j] = qmag[j];
			  }
		  }
		  double pq[][] = new double[pn][qn];
		  for (i = 0; i < pn; i++) {
			  for (j = 0; j < qn; j++) {
				  pq[i][j] = pmag[i];
			  }
		  }
		  double pTq[][] = (((new Matrix(p)).transpose()).times(new Matrix(q))).getArray();
		  m = new double[pn][qn];
		  for (i = 0; i < pn; i++) {
			  for (j = 0; j < qn; j++) {
				  m[i][j] = qp[i][j] + pq[i][j] - 2.0*pTq[i][j];
			  }
		  }
		}
		else {

		  double Ap[][] = ((new Matrix(A)).times(new Matrix(p))).getArray();
		  double Aq[][] = ((new Matrix(A)).times(new Matrix(q))).getArray();
		  double pAp[][] = new double[d][pn];
		  for (i = 0; i < d; i++) {
			  for (j = 0; j < pn; j++) {
			      pAp[i][j] = p[i][j]*Ap[i][j];	  
			  }
		  }
		  pmag = col_sum(pAp);
		  double qAq[][] = new double[d][qn];
		  for (i = 0; i < d; i++) {
			  for (j = 0; j < qn; j++) {
				  qAq[i][j] = Aq[i][j]*q[i][j];
			  }
		  }
		  qmag = col_sum(qAq);
		  double qp[][] = new double[pn][qn];
		  for (i = 0; i < pn; i++) {
			  for (j = 0; j < qn; j++) {
				  qp[i][j] = qmag[j];
			  }
		  }
		  double pq[][] = new double[pn][qn];
		  for (i = 0; i < pn; i++) {
			  for (j = 0; j < qn; j++) {
				  pq[i][j] = pmag[i];
			  }
		  }
		  double pTAq[][] = (((new Matrix(p)).transpose()).times(new Matrix(Aq))).getArray();
		  m = new double[pn][qn];
		  for (i = 0; i < pn; i++) {
			  for (j = 0; j < qn; j++) {
				  m[i][j] = qp[i][j] + pq[i][j] - 2.0*pTAq[i][j];
			  }
		  }
		  
		}
		return m;
    }
    
    public void test_dirichlet_sample() {
    	// test_dirichlet_sample() passes
    	// exactlogp1 = -1.1154640891413887
		// empiricallogp1 = -1.1134617132837537
		// exactp1logp1 = -0.36071025193601863
		// empiricalp1logp1 = -0.3606366289107984
		// exactonedivp1 = 3.1052631578947367
		// empiricalonedivp1 = 3.0997973803343424
		// exactp1divp1plusq1 = 0.4
		// empiricalp1divp1plusq1 = 0.4091696162839972
		// exactloga1plusb1 = -0.2204021757054968
		// empiricaloga1plusb1 = -0.19635320762479835

    	int i;
        double a[][] = new double[3][1];
        a[0][0] = 20.0;
        a[1][0] = 20.0;
        a[2][0] = 20.0;
        double x[][] = dirichlet_sample(a, 1000);
        // E[log p1]
        double suma = 60.0;
        double exactlogp1 = digamma(a[0][0]) - digamma(suma);
        System.out.println("exactlogp1 = " + exactlogp1);
        double sumlogx0 = 0.0;
        for (i = 0; i < x[0].length; i++) {
            sumlogx0 += Math.log(x[0][i]);	
        }
        double empiricallogp1 = sumlogx0/x[0].length;
        System.out.println("empiricallogp1 = " + empiricallogp1);
        
        // E[p1logp1]
        double exactp1logp1 = a[0][0]/suma * (digamma(a[0][0]+1.0) - digamma(suma + 1));
        System.out.println("exactp1logp1 = " + exactp1logp1);
        double sum = 0.0;
        for (i = 0; i < x[0].length; i++) {
        	sum += (x[0][i]*Math.log(x[0][i]));
        }
        double empiricalp1logp1 = sum/x[0].length;
        System.out.println("empiricalp1logp1 = " + empiricalp1logp1);
        
        // E[1/p1]
        double exactonedivp1 = (suma - 1.0)/(a[0][0] - 1.0);
        System.out.println("exactonedivp1 = " + exactonedivp1);
        double sumonedivx0 = 0.0;
        for (i = 0; i < x[0].length; i++) {
        	sumonedivx0 += 1.0/x[0][i];
        }
        double empiricalonedivp1 = sumonedivx0/x[0].length;
        System.out.println("empiricalonedivp1 = " + empiricalonedivp1);
        
        double b[][] = new double[3][1];
        b[0][0] = 5.0;
        b[1][0] = 2.0;
        b[2][0] = 3.0;
        double y[][] = dirichlet_sample(b, 1000);
        
        // E[ p1/(p1 + q1) ]
        double sumb = 10.0;
        double a1 = a[0][0]/suma;
        double b1 = b[0][0]/sumb;
        double exactp1divp1plusq1 = a1/(a1+b1);
        System.out.println("exactp1divp1plusq1 = " + exactp1divp1plusq1);
        sum = 0.0;
        for (i = 0; i < x[0].length; i++) {
            sum += (x[0][i]/(x[0][i] + y[0][i]));	
        }
        double empiricalp1divp1plusq1 = sum/x[0].length;
        System.out.println("empiricalp1divp1plusq1 = " + empiricalp1divp1plusq1);
        
        // E[ log (p1 + q1) ]
        a1 = (a[0][0] - 0.1)/(suma - 0.1);
        a1 = Math.exp(digamma(a[0][0]) - digamma(suma));
        b1 = (b[0][0] - 0.1)/(sumb - 0.1);
        b1 = Math.exp(digamma(b[0][0]) - digamma(sumb));
        double exactloga1plusb1 = Math.log(a1 + b1);
        System.out.println("exactloga1plusb1 = " + exactloga1plusb1);
        sum = 0.0;
        for (i = 0; i < x[0].length; i++) {
        	sum += Math.log(x[0][i] + y[0][i]);
        }
        double empiricalloga1plusb1 = sum/x[0].length;
        System.out.println("empiricaloga1plusb1 = " + empiricalloga1plusb1);
    }

    public double[][] dirichlet_sample(double a[][], int n) {
    	// a is either a row vector or a column vector
    	// a.length = 1 or a[0].length == 1
		// DIRICHLET_SAMPLE   Sample from Dirichlet distribution.
		
		// DIRICHLET_SAMPLE(a) returns a probability vector sampled from a 
		// Dirichlet distribution with parameter vector A.
		// DIRICHLET_SAMPLE(a,n) returns N samples, collected into a matrix, each 
		// vector having the same orientation as A.
		
		//   References:
		//      [1]  L. Devroye, "Non-Uniform Random Variate Generation", 
		//      Springer-Verlag, 1986

		// This is essentially a generalization of the method for Beta rv's.
		// Theorem 4.1, p.594

		//if nargin < 2
		//  n = 1;
		//end
    	int i,j;
    	boolean row;
    	if (a.length == 1) {
    		row = true; // row vector
    	}
    	else {
    		row = false; // column vector
    	}

		double acol[] = new double[a.length*a[0].length];
        for (j = 0; j < a[0].length; j++) {
        	for(i = 0; i < a.length; i++) {
        		acol[i + j*a.length] = a[i][j];
        	}
        }
		double y[][] = new double[acol.length][n];
		for (i = 0; i < acol.length; i++) {
			for (j = 0; j < n; j++) {
				y[i][j] = GammaRand(acol[i]);
			}
		}
		double r[] = col_sum(y); // r is a row vector of length n
		for (i = 0; i < n; i++) {
			if (r[i] == 0) {
				r[i] = 1;
			}
		}
		double rarr[][];
		if (row) {
		    rarr = new double[n][acol.length];	
		}
		else {
		    rarr = new double[acol.length][n];
		}
		for (i = 0; i < acol.length; i++) {
			for (j = 0; j < n; j++) {
				if (row) {
					rarr[j][i] = y[i][j]/r[j];
				}
				else {
				    rarr[i][j] = y[i][j]/r[j];
				}
			}
		}
		return rarr;
    }
    
    public double[] dirichlet_moment_match(double p[][]) {
		// Each row of p is a multivariate observation on the probability simplex.
        int i,j;
        double sum;
		// a = mean(p); // row vector containing mean of each column
    	double a[] = new double[p[0].length];
    	for (j = 0; j < p[0].length; j++) {
    	    sum = 0.0;
    	    for (i = 0; i < p.length; i++) {
    	    	sum += p[i][j];
    	    }
    	    a[j] = sum/p.length;
    	}
    	double pp[][] = new double[p.length][p[0].length];
    	for (i = 0; i < p.length; i++) {
    		for (j = 0; j < p[0].length; j++) {
    			pp[i][j] = p[i][j]*p[i][j];
    		}
    	}
		//m2 = mean(p.*p);
    	double m2[] = new double[p[0].length];
    	for (j = 0; j < p[0].length; j++) {
    	    sum = 0.0;
    	    for (i = 0; i < p.length; i++) {
    	    	sum += pp[i][j];
    	    }
    	    m2[j] = sum/p.length;
    	}
		// ok = (a > 0);
    	int numok = 0;
    	for (i = 0; i < a.length; i++) {
    		if (a[i] > 0.0) {
    			numok++;
    		}
    	}
    	double s[] = new double[numok];
    	for (i = 0, j = 0; i < a.length; i++) {
    		if (a[i] > 0.0) {
    		    s[j++] = (a[i] - m2[i])/( m2[i] - a[i]*a[i]);	
    		}
    	}
		// each dimension of p gives an independent estimate of s, so take the median.
		Arrays.sort(s);
		double meds;
		if ((s.length%2) == 1) {
		    meds = s[(s.length-1)/2];	
		}
		else {
			meds = (s[s.length/2] + s[(s.length/2)-1])/2.0;
		}
		if (meds == 0) {
		  meds = 1;
		}
		for (i = 0; i < a.length; i++) {
			a[i] *= meds;
		}
		return a;
    }
    
    public double[] dirichlet_fit_s(double data[][], double ain[], double bar_p[], int niter) {
    	// default niter = 100;
		// DIRICHLET_FIT_S   Maximum-likelihood Dirichlet precision.
		
		// DIRICHLET_FIT_S(data,a) returns the MLE (a) for the matrix DATA,
		// subject to a constraint on A/sum(A).
		// Each row of DATA is a probability vector.
		// A is a row vector providing the initial guess for the parameters.
		
		// A is decomposed into S*M, where M is a vector such that sum(M)=1,
		// and only S is changed by this function.  In other words, A/sum(A)
		// is unchanged by this function.
		
		// The algorithm is a generalized Newton iteration, described in
		// "Estimating a Dirichlet distribution" by T. Minka.

		// Written by Tom Minka
        int i,j;
        double s;
        double a[] = new double[ain.length];
        for (i = 0; i < a.length; i++) {
        	a[i] = ain[i];
        }
        double m[] = new double[a.length];
		boolean show_progress = false;
		double bar_p_sum;
		int iter;
		double old_s;
		double sumd;
		double g;
		double h;
		double summ;
		boolean success;
		double e[] = null;
        
		s = 0.0;
		for (i = 0; i < a.length; i++) {
		    s += a[i];	
		}
		for (i = 0; i < a.length; i++) {
			m[i] = a[i]/s;
		}

		// sufficient statistics
		if (bar_p == null) {
			bar_p = new double[data[0].length];
			for (j = 0; j < data[0].length; j++) {
			    for (i = 0; i < data.length; i++) {
			    	bar_p[j] += Math.log(data[i][j]);
			    }
			    bar_p[j] = bar_p[j]/data.length;
			}
		}
		
		bar_p_sum = 0.0;
		for (i = 0; i < m.length; i++) {
			bar_p_sum += m[i]*bar_p[i];
		}

		if (show_progress) {
			e = new double[niter];
		}
		for (iter = 1; iter <= niter; iter++) {
		  old_s = s;
		  sumd = 0.0;
		  for (i = 0; i < m.length; i++) {
			  sumd += (m[i] * digamma(s * m[i]));
		  }
		  g = digamma(s) - sumd + bar_p_sum;
		  summ = 0.0;
		  for (i = 0; i < m.length; i++) {
			  summ += (m[i]*m[i]*trigamma(s*m[i]));
		  }
		  h = trigamma(s) - summ;
		  success = false;
		  if (g + s*h < 0) {
		    s = 1/(1/s + g/h/(s*s));
		    if (s > 0) {
		      success = true;
		    }
		    else {
		      s = old_s;
		    }
		  } // if (g + s*h < 0)
		  if (!success) {
		    // Newton on log(s)
		    s = s*Math.exp(-g/(s*h + g));
		    if (s > 0) {
		      success = true;
		    }
		    else {
		      s = old_s;
		    }
		  } // if (!success)
		  if (!success) {
		    // Newton on 1/s
		    s = 1/(1/s + g/(s*s*h + 2*s*g));
		    if (s > 0) {
		      success = true;
		    }
		    else {
		      s = old_s;
		    }
		  } // if (!success)
		  if (!success) {
		    // Newton
		    s = s - g/h;
		    if (s > 0) {
		      success = true;
		    }
		    else {
		      s = old_s;
		    }
		  } // if (!success)
		  if (!success) {
		    System.err.println("All updates failed for iter = " + iter + " in dirichlet_fit_s");
		    System.exit(-1);
		  }
		  for (i = 0; i < a.length; i++) {
			  a[i] = s * m[i];
		  }
		  if (show_progress) {
			 double sumdlog[] = dirichlet_logProb(a, data);
			 for (i = 0; i < sumdlog.length; i++) {
				 e[iter-1] += sumdlog[i];
			 }
		  }
		  if (Math.abs(s - old_s) < 1e-6) {
		    break;
		  }
		} // for (iter = 1; iter <= niter; iter++)
		if (show_progress) {
			float xInit[][] = new float[1][iter];
			float eshowInit[][] = new float[1][iter];
			for (i = 0; i < iter; i++) {
				xInit[0][i] = i+1;
				eshowInit[0][i] = (float)e[i];
			}
		    new ViewJFrameGraph(xInit, eshowInit, "dirichlet_fit_s e");
		}
		return a;
    }
    
    public double[] dirichlet_logProb(double a[], double data[][]) {
		// DIRICHLET_LOGPROB  Evaluate a Dirichlet distribution.
		
		// DIRICHLET_LOGPROB(a,data) returns a vector containing the log-probability
		// of each vector in DATA, under the Dirichlet distribution with parameter A.
		// DATA is a matrix of probability vectors.
		// If A is a row vector, then the vectors are the rows, otherwise columns.
    	// a from dirichlet_fit_s is a row vector
    	int i,j;
    	double expandedData[][] = null;
    	if (data[0].length == (a.length-1)) {
    		double rdata[] = row_sum(data);
    		expandedData = new double[data.length][data[0].length+1];
    		for (i = 0; i < data.length; i++) {
    			for (j = 0; j < data[0].length; j++) {
    				expandedData[i][j] = data[i][j];
    			}
    			expandedData[i][data[0].length] = 1 - rdata[i];
    		}
    	}
    	else {
    		expandedData = data;
    	}
    	
    	double am1[] = new double[a.length];
    	for (i = 0; i < a.length; i++) {
    	   am1[i] = a[i] - 1.0;	
    	}
    	double p[] = new double[expandedData.length];
    	for (i = 0; i < expandedData.length; i++) {
    		for (j = 0; j < expandedData[0].length; j++) {
    			p[i] += (Math.log(expandedData[i][j]) * am1[j]);
    		}
    	}
    	double suma = 0.0;
    	for (i = 0; i < a.length; i++) {
    		suma += a[i];
    	}
    	double gammalnsuma = gammaln(suma);
    	double sumgammalna = 0.0;
    	for (i = 0; i < a.length; i++) {
    		sumgammalna += (gammaln(a[i]));
    	}
		for (i = 0; i < p.length; i++) {
		  p[i] = p[i] + gammalnsuma - sumgammalna;
		}

		return p;
    }

    public double[] dirichlet_fit_m(double data[][], double ain[], double bar_p[], int niter) {
		// DIRICHLET_FIT_M   Maximum-likelihood Dirichlet mean.
		
		// DIRICHLET_FIT_M(data,a) returns the MLE (a) for the matrix DATA,
		// subject to a constraint on sum(A).
		// Each row of DATA is a probability vector.
		// A is a row vector providing the initial guess for the parameters.
		// A is decomposed into S*M, where M is a vector such that sum(M)=1,
		// and only M is changed by this function.  In other words, sum(A)
		// is unchanged by this function.
		
		// The algorithm is a generalized Newton iteration, described in
		// "Estimating a Dirichlet distribution" by T. Minka.

		// Written by Tom Minka
        int i,j;
        double a[] = new double[ain.length];
        for (i = 0; i < a.length; i++) {
        	a[i] = ain[i];
        }
		boolean show_progress = false;
		int diter = 4;
		double e[] = null;
		int iter;
		double sa;
		double old_a[] = new double[a.length];
		double w[] = new double[a.length];
		double sumw;
		double g[];
		double suma;
		double maxDiff;

		// sufficient statistics
		if (bar_p == null) {
			bar_p = new double[data[0].length];
			for (j = 0; j < data[0].length; j++) {
			    for (i = 0; i < data.length; i++) {
			    	bar_p[j] += Math.log(data[i][j]);
			    }
			    bar_p[j] = bar_p[j]/data.length;
			}
		}
		g = new double[bar_p.length];
		

		if (show_progress) {
			e = new double[niter];
		}
		for (iter = 1; iter <= niter; iter++) {
		  sa = 0.0;
		  for (i = 0; i < a.length; i++) {
			  sa += a[i];
			  old_a[i] = a[i];
		  }
		  // convergence is guaranteed for any w, but this one is fastest
		    for (i = 0; i < a.length; i++) {
		    	w[i] = a[i]/sa;
		    }
		    sumw = 0.0;
		    for (i = 0; i < w.length; i++) {
		    	sumw += (w[i]*(digamma(a[i]) - bar_p[i]));
		    }
		    for (i = 0; i < bar_p.length; i++) {
		    	g[i] = sumw + bar_p[i];
		    }
		    for (i = 0; i < g.length; i++) {
		        a[i] = inv_digamma(g[i],diter);
		    }
		    // project back onto the constraint
		    suma = 0.0;
		    for (i = 0; i < a.length; i++) {
		    	suma += a[i];
		    }
		    for (i = 0; i < a.length; i++) {
		    	a[i] = a[i]/suma*sa;
		    }
		    if (show_progress) {
				 double sumdlog[] = dirichlet_logProb(a, data);
				 for (i = 0; i < sumdlog.length; i++) {
					 e[iter-1] += sumdlog[i];
				 }
			}
			maxDiff = 0.0;
			for (i = 0; i < a.length; i++) {
				maxDiff = Math.max(maxDiff,Math.abs(a[i] - old_a[i]));
			}
		    if (maxDiff < 1.0E-6) {
		    	break;
		    }
		} // for (iter = 1; iter <= niter; iter++)
		if (show_progress) {
			float xInit[][] = new float[1][iter];
			float eshowInit[][] = new float[1][iter];
			for (i = 0; i < iter; i++) {
				xInit[0][i] = i+1;
				eshowInit[0][i] = (float)e[i];
			}
		    new ViewJFrameGraph(xInit, eshowInit, "dirichlet_fit_m e");
		}
		return a;
    }
    
    public double inv_digamma(double y,int niter) {
    	// Never need more than 5 iterations
    	// Default niter = 5
		// INV_DIGAMMA    Inverse of the digamma function.
		
		// inv_digamma(y) returns x such that digamma(x) = y.

		// a different algorithm is provided by Paul Fackler:
		// http://www.american.edu/academic.depts/cas/econ/gaussres/pdf/loggamma.src

		// Newton iteration to solve digamma(x)-y = 0
    	int iter;
		double x = Math.exp(y)+1/2;
		if (y <= -2.22) {
			x = -1.0/(y - digamma(1.0));
		}
		
		for (iter = 1; iter <= niter; iter++) {
		  x = x - (digamma(x)-y)/trigamma(x);
		}
		return x;

		// test
		// = -3:0.01:0.1;
		// = digamma(inv_digamma(y));
		// max(abs(x-y))
		// max(abs(x-y)./inv_digamma(y))
    }

    public double dirichlet_logProb_fast(double a[], double meanlog[]) {
        int i;
        double p;
        double suma = 0.0;
        for (i = 0; i < a.length; i++) {
        	suma += a[i];
        }
        double gammalnsuma = gammaln(suma);
        double sumgammalna = 0.0;
    	for (i = 0; i < a.length; i++) {
    		sumgammalna += (gammaln(a[i]));
    	}
		double sumam = 0.0;
		for (i = 0; i < a.length; i++) {
			sumam += ((a[i]-1.0)*meanlog[i]);
		}
		p = gammalnsuma + sumgammalna + sumam;
        return p;
    }
    
    public void test_dirichlet_fit() {
    	// dirichlet_fit_simple ok err = 3.262224557154525E-8
		// dirichlet_fit_simple had ebest = 925.4592744344884
		// dirichlet_fit ok err = 7.123190925995004E-12
		// dirichlet_fit had ebest = 248.88807229877784
		// dirichlet_fit_newton ok err = 8.881784197001252E-16
		// dirichlet_fit_newton had ebest = 248.6273106945127
    	int i,j;
        double ain[][] = new double[1][3];
        ain[0][0] = 30.0;
        ain[0][1] = 10.0;
        ain[0][2] = 20.0;
        double data[][] = dirichlet_sample(ain,100*ain[0].length);
        double e[] = new double[1000];
        double ebest;
        for (i = 0; i < e.length; i++) {
        	e[i] = Double.NaN;
        }
        double a[] = dirichlet_fit_simple(e, data, null);
        // check it is a maximum
        double bar_p[] = new double[data[0].length];
		for (j = 0; j < data[0].length; j++) {
		    for (i = 0; i < data.length; i++) {
		    	bar_p[j] += Math.log(data[i][j]);
		    }
		    bar_p[j] = bar_p[j]/data.length;
		}
		double suma = 0.0;
		for (i = 0; i < a.length; i++) {
			suma += a[i];
		}
		double digammasuma = digamma(suma);
		double err = 0.0;
		for (i = 0; i < a.length; i++) {
			err = Math.max(err,Math.abs(digammasuma - digamma(a[i]) + bar_p[i]));
		}
        if (err < 1e-6) {
          System.out.println("dirichlet_fit_simple ok err = " + err);
        }
        else {
          System.err.println("dirichlet_fit_simple normal equations not satisfied err = " + err);
        }
        
        int lastValideIndex = -2;
        for (i = 0; i < e.length && (lastValideIndex == -2); i++) {
        	if (Double.isNaN(e[i])) {
        	    lastValideIndex = i-1;	
        	}
        }
        if (lastValideIndex == -2) {
        	lastValideIndex = e.length-1;
        }
        if (lastValideIndex == -1) {
        	System.err.println("No valid e found in dirichlet_fit_simple");
        }
        else {
        	ebest = Double.NEGATIVE_INFINITY;
        	for (i = 0; i <= lastValideIndex; i++) {
        		ebest = Math.max(ebest, e[i]);
        	}
        	System.out.println("dirichlet_fit_simple had ebest = " + ebest);
        }
        
        e = new double[100];
        for (i = 0; i < e.length; i++) {
        	e[i] = Double.NaN;
        }
        a = dirichlet_fit(e, data, null, null);
        // check it is a maximum
        bar_p = new double[data[0].length];
		for (j = 0; j < data[0].length; j++) {
		    for (i = 0; i < data.length; i++) {
		    	bar_p[j] += Math.log(data[i][j]);
		    }
		    bar_p[j] = bar_p[j]/data.length;
		}
		suma = 0.0;
		for (i = 0; i < a.length; i++) {
			suma += a[i];
		}
		digammasuma = digamma(suma);
		err = 0.0;
		for (i = 0; i < a.length; i++) {
			err = Math.max(err,Math.abs(digammasuma - digamma(a[i]) + bar_p[i]));
		}
        if (err < 1e-6) {
          System.out.println("dirichlet_fit ok err = " + err);
        }
        else {
          System.err.println("dirichlet_fit normal equations not satisfied err = " + err);
        }
        lastValideIndex = -2;
        for (i = 0; i < e.length && (lastValideIndex == -2); i++) {
        	if (Double.isNaN(e[i])) {
        	    lastValideIndex = i-1;	
        	}
        }
        if (lastValideIndex == -2) {
        	lastValideIndex = e.length-1;
        }
        if (lastValideIndex == -1) {
        	System.err.println("No valid e found in dirichlet_fit");
        }
        else {
        	ebest = Double.NEGATIVE_INFINITY;
        	for (i = 0; i <= lastValideIndex; i++) {
        		ebest = Math.max(ebest, e[i]);
        	}
        	System.out.println("dirichlet_fit had ebest = " + ebest);
        }
        
        e = new double[100];
        for (i = 0; i < e.length; i++) {
        	e[i] = Double.NaN;
        }
        a = dirichlet_fit_newton(e, data, null, null);
        // check it is a maximum
        bar_p = new double[data[0].length];
		for (j = 0; j < data[0].length; j++) {
		    for (i = 0; i < data.length; i++) {
		    	bar_p[j] += Math.log(data[i][j]);
		    }
		    bar_p[j] = bar_p[j]/data.length;
		}
		suma = 0.0;
		for (i = 0; i < a.length; i++) {
			suma += a[i];
		}
		digammasuma = digamma(suma);
		err = 0.0;
		for (i = 0; i < a.length; i++) {
			err = Math.max(err,Math.abs(digammasuma - digamma(a[i]) + bar_p[i]));
		}
        if (err < 1e-6) {
          System.out.println("dirichlet_fit_newton ok err = " + err);
        }
        else {
          System.err.println("dirichlet_fit_newton normal equations not satisfied err = " + err);
        }
        lastValideIndex = -2;
        for (i = 0; i < e.length && (lastValideIndex == -2); i++) {
        	if (Double.isNaN(e[i])) {
        	    lastValideIndex = i-1;	
        	}
        }
        if (lastValideIndex == -2) {
        	lastValideIndex = e.length-1;
        }
        if (lastValideIndex == -1) {
        	System.err.println("No valid e found in dirichlet_fit_newton");
        }
        else {
        	ebest = Double.NEGATIVE_INFINITY;
        	for (i = 0; i <= lastValideIndex; i++) {
        		ebest = Math.max(ebest, e[i]);
        	}
        	System.out.println("dirichlet_fit_newton had ebest = " + ebest);
        }
    }
    
    public double[] dirichlet_fit(double e[], double data[][], double ain[], double bar_p[]) {
    	// Initialize all e[i] to Double.NaN
		// DIRICHLET_FIT   Maximum-likelihood Dirichlet distribution.
		
		// DIRICHLET_FIT(data) returns the MLE (a) for the matrix DATA.
		// Each row of DATA is a probability vector.
		// DIRICHLET_FIT(data,a) provides an initial guess A to speed up the search.
		
		// The Dirichlet distribution is parameterized as
		//   p(p) = (Gamma(sum_k a_k)/prod_k Gamma(a_k)) prod_k p_k^(a_k-1)
		
		// The algorithm is an alternating optimization for m and for s, described in
		// "Estimating a Dirichlet distribution" by T. Minka.

		// Written by Tom Minka
        int i,j;
        double a[] = null;
        double s;
        int iter;
        double maxDiff;
        if (ain != null) {
	        a = new double[ain.length];
	        for (i = 0; i < a.length; i++) {
	        	a[i] = ain[i];
	        }
        }
        else {
        	a = dirichlet_moment_match(data);	
        }
        
        if (bar_p == null) {
			bar_p = new double[data[0].length];
			for (j = 0; j < data[0].length; j++) {
			    for (i = 0; i < data.length; i++) {
			    	bar_p[j] += Math.log(data[i][j]);
			    }
			    bar_p[j] = bar_p[j]/data.length;
			}
		}
		
        double old_a[] = new double[a.length];
        double m[] = new double[a.length];
		
        s = 0.0;
        for (i = 0; i < a.length; i++) {
        	s += a[i];
        }
		if (s <= 0) {
		  // bad initial guess; fix it
		  System.out.println("Fixing initial guess in dirichlet_fit");
		  if (s == 0) {
			  for (i = 0; i < a.length; i++) {
				  a[i] = 1.0/a.length;
			  }
		  }
		  else {
			  for (i = 0; i < a.length; i++) {
				  a[i] = a[i]/s;
			  }
		  }
		  s = 1.0;
		} // if (s <= 0)
		for (iter = 1; iter <= 100; iter++) {
		  for (i = 0; i < a.length; i++) {
			  old_a[i] = a[i];
		  }
		  // time for fit_s is negligible compared to fit_m
		  a = dirichlet_fit_s(data, a, bar_p, 100);
		  s = 0.0;
          for (i = 0; i < a.length; i++) {
        	  s += a[i];
          }
		  a = dirichlet_fit_m(data, a, bar_p, 1);
		  for (i = 0; i < a.length; i++) {
			  m[i] = a[i]/s;
		  }
		  if (e != null) {
			  e[iter-1] = dirichlet_logProb_fast(a, bar_p);
		  }
		  maxDiff = 0.0;
			for (i = 0; i < a.length; i++) {
				maxDiff = Math.max(maxDiff,Math.abs(a[i] - old_a[i]));
			}
		    if (maxDiff < 1.0E-4) {
		    	break;
		    }
		} // for (iter = 1; iter <= 100; iter++) 
		return a;
    }

    public double[] dirichlet_fit_simple(double e[], double data[][], double ain[]) {
		// DIRICHLET_FIT_SIMPLE   Maximum-likelihood Dirichlet distribution.
		
		// Same as DIRICHLET_FIT but uses the simple fixed-point iteration described in
		// "Estimating a Dirichlet distribution" by T. Minka. 
        int i,j;
		boolean show_progress = false;
		double a[] = null;
		int iter;
		double sa;
		double maxDiff;
		if (ain != null) {
		   a = new double[ain.length];	
		   for (i = 0; i < a.length; i++) {
			   a[i] = ain[i];
		   }
		}
		else {
		    a = dirichlet_moment_match(data);
		}
		double old_a[] = new double[a.length];
		double bar_p[] = new double[data[0].length];
		for (j = 0; j < data[0].length; j++) {
		    for (i = 0; i < data.length; i++) {
		    	bar_p[j] += Math.log(data[i][j]);
		    }
		    bar_p[j] = bar_p[j]/data.length;
		}
		double g;

		// fixed-pt iteration
		for (iter = 1; iter <= 1000; iter++) {
		  sa = 0.0;
		  for (i = 0; i < a.length; i++) {
		    old_a[i] = a[i];
		    sa += a[i];
		  }
		  for (i = 0; i < bar_p.length; i++) {
		      g = digamma(sa) + bar_p[i];
		      a[i] = inv_digamma(g,5);
		  }
		  if (e != null) {
			  double sumdlog[] = dirichlet_logProb(a, data);
			  e[iter-1] = 0.0;
			  for (i = 0; i < sumdlog.length; i++) {
			      e[iter-1] += sumdlog[i];
			  }  
		  }
		  maxDiff = 0.0;
		  for (i = 0; i < a.length; i++) {
		      maxDiff = Math.max(maxDiff,Math.abs(a[i] - old_a[i]));
		  }
		  if (maxDiff < 1.0E-6) {
		      break;
		  }
		} // for (iter = 1; iter <= 1000; iter++)
		if (show_progress) {
			float xInit[][] = new float[1][iter];
			float eshowInit[][] = new float[1][iter];
			for (i = 0; i < iter; i++) {
				xInit[0][i] = i+1;
				eshowInit[0][i] = (float)e[i];
			}
		    new ViewJFrameGraph(xInit, eshowInit, "dirichlet_fit_simple e");
		}
		return a;		
    }
    
    public double[] dirichlet_fit_newton(double e[], double data[][], double ain[],double bar_p[]) {
		// DIRICHLET_FIT_NEWTON   Maximum-likelihood Dirichlet distribution.
		
		// Same as DIRICHLET_FIT but uses the Newton iteration described in
		// "Estimating a Dirichlet distribution" by T. Minka. 

		// Written by Tom Minka
        int i,j;
		boolean show_progress = false;
		double a[] = null;
		double old_e;
		double lambda;
		int iter;
		double suma;
		double digammasuma;
		boolean abort;
		double hg[];
		boolean allhglessthana;
		double maxDiff;
		if (ain != null) {
		   a = new double[ain.length];	
		   for (i = 0; i < a.length; i++) {
			   a[i] = ain[i];
		   }
		}
		else {
		    a = dirichlet_moment_match(data);
		}
		double old_a[] = new double[a.length];
		double g[] = new double[a.length];
		double aminushg[] = new double[a.length];
		
		if (bar_p == null) {
			bar_p = new double[data[0].length];
			for (j = 0; j < data[0].length; j++) {
			    for (i = 0; i < data.length; i++) {
			    	bar_p[j] += Math.log(data[i][j]);
			    }
			    bar_p[j] = bar_p[j]/data.length;
			}
		}
		

		old_e = dirichlet_logProb_fast(a, bar_p);
		lambda = 0.1;
		for (iter = 1; iter <= 100; iter++) {
		  suma = 0.0;
		  for (i = 0; i < a.length; i++) {
			  old_a[i] = a[i];
			  suma += a[i];
		  }
		  if (suma == 0) {
		    break;
		  }
		  digammasuma = digamma(suma);
		  for (i = 0; i < a.length; i++) {
			  g[i] = digammasuma - digamma(a[i]) + bar_p[i];
		  }
		  abort = false;
		  // Newton iteration
		  // loop until we get a nonsingular hessian matrix
		  while(true) {
		    hg = hessian_times_gradient(a, g, lambda);
		    allhglessthana = true;
		    for (i = 0; i < a.length; i++) {
		    	if (hg[i] >= a[i]) {
		    		allhglessthana = false;
		    	}
		    }
		    if (allhglessthana) {
		      for (i = 0; i < a.length; i++) {
		    	  aminushg[i] = a[i] - hg[i];
		      }
		      e[iter-1] = dirichlet_logProb_fast(aminushg, bar_p);
		      if(e[iter-1] > old_e) {
			      old_e = e[iter-1];
			      for (i = 0; i < a.length; i++) {
			    	  a[i] = a[i] - hg[i];
			      }
				  lambda = lambda/10;
				  break;
		      } // if(e[iter-1] > old_e)
		    } // if (allhglessthana)
		    lambda = lambda*10;
		    if (lambda > 1e+6) {
		      abort = true;
		      break;
		    } // if (lambda > 1e+6)
		  } // while(true)
		  if (abort) {
		    Preferences.debug("Search aborted in dirichlet_fit_newton\n", Preferences.DEBUG_ALGORITHM);
		    e[iter-1] = old_e;
		    break;
		  } // if (abort)
		  for (i = 0; i < a.length; i++) {
			  if (a[i] < epsilon) {
				  a[i] = epsilon;
			  }
		  }
		  maxDiff = 0.0;
		  for (i = 0; i < a.length; i++) {
		      maxDiff = Math.max(maxDiff,Math.abs(a[i] - old_a[i]));
		  }
		  if (maxDiff < 1.0E-10) {
		      break;
		  }
		} // for (iter = 1; iter <= 100; iter++)
		if (show_progress) {
			float xInit[][] = new float[1][iter];
			float eshowInit[][] = new float[1][iter];
			for (i = 0; i < iter; i++) {
				xInit[0][i] = i+1;
				eshowInit[0][i] = (float)e[i];
			}
		    new ViewJFrameGraph(xInit, eshowInit, "dirichlet_fit_newton e");
		}
		return a;	
    }

    public double[] hessian_times_gradient(double a[], double g[], double lambda) {
            int i;
            double sa = 0.0;
            double q[] = new double[a.length];
            for (i = 0; i < a.length; i++) {
            	sa += a[i];
            	q[i] = -trigamma(a[i]);
            }
    		double z = trigamma(sa);
    		for (i = 0; i < a.length; i++) {
    		    q[i] = q[i] - lambda;
    		    q[i] = 1./q[i];
    		}
    		double sumgq = 0.0;
    		double sumq = 0.0;
    		for (i = 0; i < a.length; i++) {
    			sumgq += (g[i]*q[i]);
    			sumq += q[i];
    		}
    		double b = sumgq/(1/z + sumq);
    		double hg[] = new double[a.length];
    		for (i = 0; i < a.length; i++) {
    			hg[i] = (g[i] - b) * q[i];
    		}
    		return hg;
    }
    
    public void test_gamma_fit() {
    	// Test passes
    	// a = 7.3 acalc[0] = 7.243850921724396
    	// b = 4.5 bcalc[0] = 4.537971005083331
    	int i;
    	int n = 10000;
    	double a = 7.3;
    	double b = 4.5;
    	double x[] = new double[n];
    	for (i = 0; i < n; i++) {
    	    x[i] = GammaRand(a)*b;
    	}
    	double acalc[] = new double[1];
    	double bcalc[] = new double[1];
    	gamma_fit(acalc, bcalc, x, Double.NaN);
    	System.out.println("a = " + a + " acalc[0] = " + acalc[0]);
    	System.out.println("b = " + b + " bcalc[0] = " + bcalc[0]);
    }

    public void gamma_fit(double a[], double b[], double x[], double s) {
		// GAMMA_FIT     Maximum-likelihood gamma distribution.
		
		// GAMMA_FIT(x) returns the MLE (a,b) for the data in vector x.
		
		// GAMMA_FIT(m,s) returns the MLE (a,b) for data with sufficient statistics
		// given by 
		//   m = mean(x)
		//   s = log(m) - mean(log(x))
		
		// The gamma distribution is parameterized as
		//   p(x) = x^(a-1)/(Gamma(a) b^a) exp(-x/b)
		//   E[x] = ab
		
		// The algorithm is a generalized Newton iteration, described in
		// "Estimating a Gamma distribution" by T. Minka.

		// Written by Tom Minka

		int i;
		int iter;
		double old_a;
		double g;
		double h;
    	double m = 0.0;
    	for (i = 0; i < x.length; i++) {
    		m += x[i];
    	}
    	m = m/x.length;
    	if (Double.isNaN(s)) {
    		double meanlogx = 0.0;
    		for (i = 0; i < x.length; i++) {
    			meanlogx += Math.log(x[i]);
    		}
    		meanlogx = meanlogx/x.length;
    		s = Math.log(m) - meanlogx;
    	} // if (s == Double.NaN)
    	
		a[0] = 0.5/s;

		// gen Newton
		for (iter = 1; iter <= 100; iter++) {
		  old_a = a[0];
		  g = Math.log(a[0])-s-digamma(a[0]);
		  h = 1/a[0] - trigamma(a[0]);
		  a[0] = 1/(1/a[0] + g/(a[0]*a[0]*h));
		  if(Math.abs(a[0] - old_a) < 1e-8) {
			  break;
		  }
		} // for (iter = 1; iter <= 100; iter++)  
		b[0] = m/a[0];
    }
   
}
