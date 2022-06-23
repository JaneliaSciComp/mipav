package gov.nih.mipav.model.algorithms;
import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;

public class Fastfit {

/**
    Original MATLAB code from fastfit and lightspeed packages by Tom Minka.
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


}