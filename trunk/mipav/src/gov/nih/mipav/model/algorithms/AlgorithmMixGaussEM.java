package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;

/**
 * 
 * @author aailb
 * Copyright (c) 2016, Mo Chen
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution
* Neither the name of  nor the names of its
  contributors may be used to endorse or promote products derived from this
  software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 *
 */

public class AlgorithmMixGaussEM extends AlgorithmBase {
	
	public void runAlgorithm() {
		
	}
	
	//public function [X, z, model] = mixGaussRnd(d, k, n)
	public void mixGaussRnd(int d, int k, int n) {
		// Genarate samples form a Gaussian mixture model.
		// Input:
		//   d: dimension of data
		//   k: number of components
		//   n: number of data
		// Output:
		//   X: d x n data matrix
		//   z: 1 x n response variable
		//   model: model structure
		// Written by Mo Chen (sth4nth@gmail.com).
		double alpha0;
		double W0[][];
		int i,j,y,x;
		double v0;
		double mu0[];
		double beta0;
		double w[];
		double mv[];
		int z[];
		double mu[][];
		double Sigma[][][];
		double X[][];
		
		alpha0 = 1;  // hyperparameter of Dirichlet prior
		W0 = new double[d][d];  // hyperparameter of inverse Wishart prior of covariances
		for (i = 0; i < d; i++) {
			W0[i][i] = 1.0;
		}
		v0 = d+1;  // hyperparameter of inverse Wishart prior of covariances
		mu0 = new double[d];  // hyperparameter of Gaussian prior of means
		beta0 = Math.pow(k,1.0/d); // hyperparameter of Gaussian prior of means % in volume x^d there is k points: x^d=k
		
		mv = new double[k];
		for (i = 0; i < k; i++) {
			mv[i] = 1.0/(double)k;
		}
		w = dirichletRnd(alpha0,mv);
		z = discreteRnd(w,n);
		
		mu = new double[d][k];
		Sigma = new double[d][d][k];
		X = new double[d][n];
		Vector<Integer> idxVec = new Vector<Integer>();
		for (i = 1; i <= k; i++) {
			idxVec.clear();
		    for (j = 0; j < z.length; j++) {
		    	
		    }
		} // for (i = 1; i <= k; i++)
		
	}
	
	public int[] discreteRnd(double p[], int n) {
		// Generate samples from a discrete distribution (multinomial).
		// Input:
		//   p: k dimensional probability vector
		//   n: number of samples
		// Ouput:
		//   x: k x n generated samples x~Mul(p)
		// Written by Mo Chen (sth4nth@gmail.com).
		//if nargin == 1
		//    n = 1;
		//end
		int i,j;
		boolean found;
		RandomNumberGen randomGen = new RandomNumberGen();
		double r[] = new double[n];
		for (i = 0; i < n; i++) {
			r[i] = randomGen.genUniformRandomNum(0.0,1.0);
		}
		for (i = 1; i < p.length; i++) {
			p[i] = p[i] + p[i-1];
		}
		double binranges[] = new double[p.length+1];
		for (i = 1; i < binranges.length; i++) {
			binranges[i] = p[i-1]/p[p.length-1];
		}
		int x[] = new int[binranges.length];
		for (i = 0; i < n; i++) {
			found = false;
		    for (j = 0; j < binranges.length-1 && (!found); j++) {
		        if ((r[i] >= binranges[j]) && (r[i] < binranges[j+1])) {
		            found = true;
		            x[i] = j+1;
		        }
		    }
		    if (!found) {
		    	x[i] = binranges.length;
		    }
		}
	    return x;
	}
	
	public double[] dirichletRnd(double ain, double m[]) {
			// Generate samples from a Dirichlet distribution.
			// Input:
			//   a: k dimensional vector
			//   m: k dimensional mean vector
			// Output:
			//   x: generated sample x~Dir(a,m)
			// Written by Mo Chen (sth4nth@gmail.com).
	        int i;
	        double a[] = new double[m.length];
			for (i = 0; i < a.length; i++) {
				a[i] = ain*m[i];
			}
			double x[] = new double[m.length];
			double sumx = 0.0;
			for (i = 0; i < x.length; i++) {
				x[i] = gamrnd(a[i],1.0);
				sumx += x[i];
			}
			for (i = 0; i < x.length; i++) {
				x[i] = x[i]/sumx;
			}
			return x;
    }

     public double gamrnd(double a, double b) {
    	 // From A Simple Method for Generating Gamma Variables
    	 // by George Marsaglia and Wai Wan Tsang
    	 double d;
    	 double c;
    	 boolean cycle = true;
    	 RandomNumberGen randomGen = new RandomNumberGen();
    	 double x;
    	 double v = 0.0;
    	 double cuberootv;
    	 double uniform;
    	 double var;
         if (a > 1) {
             d = a - 1.0/3.0;
             c = 1.0/Math.sqrt(9.0*d);
             while (cycle) {
            	 x = randomGen.genStandardGaussian();
            	 if (x > -1.0/c) {
            	     cuberootv = 1.0 + c*x;
            	     v = cuberootv*cuberootv*cuberootv;
            	     uniform = randomGen.genUniformRandomNum(0.0,1.0);
            	     cycle= (0.5*x*x+d-d*v+d*Math.log(v)) < Math.log(uniform);
            	 } // if (x > -1.0/c)	 
             } // while (cycle) 
             var = d*v/b;
         } // if (a > 1)
         else {
        	 var =gamrnd(a+1,b);
        	 uniform = randomGen.genUniformRandomNum(0.0,1.0);
        	 var=var*Math.pow(uniform,(1.0/a));
         }
         return var;
     }
	
}