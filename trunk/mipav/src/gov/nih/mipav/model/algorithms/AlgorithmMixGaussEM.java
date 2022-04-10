package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.view.*;

import java.awt.Color;
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
	private double X[][];
	private int label[];
	private mixGaussOut model;
	
	public AlgorithmMixGaussEM() {
		
	}
	
	public void runAlgorithm() {
		
	}
	
	public void mixGaussEm_demo() {
		int d = 2;
		int k = 3;
		int n = 500;
		mixGaussRnd(d,k,n);
		plotClass(X,label);	
	}
	
	class mixGaussOut {
		double mu[][];
		double Sigma[][][];
		double w[];
		
		public mixGaussOut(double mu[][], double Sigma[][][], double w[]) {
			this.mu = mu;
			this.Sigma = Sigma;
			this.w = w;
		}
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
		//   label: 1 x n response variable
		//   model: model structure
		// Written by Mo Chen (sth4nth@gmail.com).
		double alpha0;
		double W0[][];
		int i,j,y,x,index;
		int v0;
		double mu0[];
		double mui[] = new double[d];
		double beta0;
		double w[];
		double mv[];
		double mu[][];
		double Sigma[][][];
		
		double Xout[][];
		WishartVariateGenerator wvg;
		double SA[][] = new double[d][d];
		double SB[][] = new double[d][d];
		double bSigma[][] = new double[d][d];
		double gOut[] = new double[d];
		LinearEquations2 le2 = new LinearEquations2();
		int ipiv[] = new int[d];
		int info[] = new int[1];
		double work[];
		int lwork;
		
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
		label = discreteRnd(w,n);
		
		mu = new double[d][k];
		Sigma = new double[d][d][k];
		X = new double[d][n];
		boolean idx[] = new boolean[label.length];
		int idxSum;
		for (i = 1; i <= k; i++) {
			idxSum = 0;
		    for (j = 0; j < label.length; j++) {
		        if (i == label[j]) {
		            idx[j] = true;
		            idxSum ++;
		        }
		        else {
		        	idx[j] = false;
		        }
		    }
		    wvg = new WishartVariateGenerator(W0, v0, SA, SB);
		    le2.dgetrf(d,d,SA,d,ipiv,info);
		    boolean rankDeficient = true;
		    if (info[0] < 0) {
		    	  System.err.println("In le2.dgetrf argument number " + 
		      (-info[0]) + " is illegal");
		    	  return;
		      }
		      if (info[0] > 0) {
		    	  System.err.println("In le2.dgetrf U["+(info[0]-1)+"]["+(info[0]-1)+"] is exactly 0");
		    	  rankDeficient = true;
		    	  return;
		      }
		      work = new double[1];
		      lwork = -1;
		      le2.dgetri(d,SA,d,ipiv,work,lwork,info);
		      if (info[0] < 0) {
		    	  System.err.println("In le2.dgetri argument number " + 
		      (-info[0]) + " is illegal");
		    	  return;
		      }
		      lwork = (int)work[0];
		      work = new double[lwork];
		      le2.dgetri(d,SA,d,ipiv,work,lwork,info);
		      if (info[0] < 0) {
		    	  System.err.println("In le2.dgetri argument number " + 
		      (-info[0]) + " is illegal");
		    	  return;
		      }
		      if (info[0] > 0) {
		    	  System.err.println("In le2.dgetri U["+(info[0]-1)+"]["+(info[0]-1)+"] is exactly 0");
		    	  rankDeficient = true;
		    	  return;
		      }
		      //Sigma(:,:,i) = iwishrnd(W0,v0); % invpd(wishrnd(W0,v0));
		      for (y = 0; y < d; y++) {
		    	  for (x = 0; x < d; x++) {
		    		  Sigma[y][x][i] = SA[y][x];
		    		  bSigma[y][x] = beta0*Sigma[y][x][i];
		    	  }
		      }
		      gOut = gaussRnd(mu0, bSigma);
		      for (y = 0; y < d; y++) {
		    	  mu[y][i] = gOut[y];
		      }
		      for (y = 0; y < d; y++) {
		    	  mui[y] = mu[y][i];
		      }
		      for (y = 0; y < d; y++) {
		    	  for (x = 0; x < d; x++) {
		    	      bSigma[y][x] = Sigma[y][x][i];  
		    	  }
		      }
		      Xout = gaussRnd(mui, bSigma, idxSum);
		      for (y = 0; y < Xout.length; y++) {
		    	  for (x = 0, index = 0; x < Xout[0].length; x++) {
		    		  if (idx[x]) {
		    		      X[y][index++] = Xout[y][x];
		    		  }
		    	  }
		      }
		} // for (i = 1; i <= k; i++)
		model = new mixGaussOut(mu, Sigma, w);
		return;
	}
	
	public int[] discreteRnd(double pin[], int n) {
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
		double p[] = new double[pin.length];
		p[0] = pin[0];
		for (i = 1; i < p.length; i++) {
			p[i] = pin[i] + p[i-1];
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
		            x[j] = x[j]+1;
		        }
		    }
		    if (!found) {
		    	x[binranges.length-1] = x[binranges.length-1] + 1;
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
     
     private double[] gaussRnd(double mu[], double Sigma[][]) {
		 // Generate samples from a Gaussian distribution.
		 // Input:
		 //   mu: d x 1 mean vector
		 //   Sigma: d x d covariance matrix
		 //   n: number of samples
		 // Output:
		 //   x: d x n generated sample x~Gauss(mu,Sigma)
		 // Written by Mo Chen (sth4nth@gmail.com).
		 // if nargin == 2
		 //    n = 1;
		 // end
    	 // Cholesky factorization
    	 int i,j,k;
    	 int d = Sigma.length;
    	 double V[][] = new double[d][d];
    	 for (i = 0; i < d; i++) {
    		 for (j = 0; j < d; j++) {
    			 V[i][j] = Sigma[i][j];
    		 }
    	 }
    	 RandomNumberGen randomGen = new RandomNumberGen();
    	 GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    	 int info[] = new int[1];
 		 ge.dpotrf('U', Sigma[0].length, V, Sigma.length, info);
 		 if (info[0] < 0) {
 			System.err.println("In dpotrf argument " + (-info[0]) + " had an illegal value");
 			return null;
 		 }
 		 else if (info[0] > 0) {
 			System.err.println("In dpotrf the leading minor of order " + info[0] + " is not positive definite,"); 
 			System.err.println("and the factorization could not be completed.");
 			return null;
 		 }
 		 double ran[] = new double[d];
		 for (i = 0; i < d; i++) {
		     ran[i] = randomGen.genStandardGaussian();
		 }
		 double VT[][] = new double[d][d];
		 for (j = 0; j < d; j++) {
			 for (i = j; i < d; i++) {
			     VT[i][j] = V[j][i];	 
			 }
		 }
		 double VTran[] = new double[d];
		 for (i = 0; i < d; i++) {
		     for (k = 0; k < d; k++) {
				 VTran[i] += VT[i][k] * ran[k];	 
		     }
		 }
		 double x[] = new double[d];
		 for (i = 0; i < d; i++) {
		     x[i] = VTran[i] + mu[i];
		 }
		 return x;
     }
     
     //function x = gaussRnd(mu, Sigma, n)
     private double[][] gaussRnd(double mu[], double Sigma[][], int n) {
		 // Generate samples from a Gaussian distribution.
		 // Input:
		 //   mu: d x 1 mean vector
		 //   Sigma: d x d covariance matrix
		 //   n: number of samples
		 // Output:
		 //   x: d x n generated sample x~Gauss(mu,Sigma)
		 // Written by Mo Chen (sth4nth@gmail.com).
		 // if nargin == 2
		 //    n = 1;
		 // end
    	 // Cholesky factorization
    	 int i,j,k;
    	 int d = Sigma.length;
    	 double V[][] = new double[d][d];
    	 for (i = 0; i < d; i++) {
    		 for (j = 0; j < d; j++) {
    			 V[i][j] = Sigma[i][j];
    		 }
    	 }
    	 RandomNumberGen randomGen = new RandomNumberGen();
    	 GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    	 int info[] = new int[1];
 		 ge.dpotrf('U', Sigma[0].length, V, Sigma.length, info);
 		 if (info[0] < 0) {
 			System.err.println("In dpotrf argument " + (-info[0]) + " had an illegal value");
 			return null;
 		 }
 		 else if (info[0] > 0) {
 			System.err.println("In dpotrf the leading minor of order " + info[0] + " is not positive definite,"); 
 			System.err.println("and the factorization could not be completed.");
 			return null;
 		 }
 		 double ran[][] = new double[d][n];
		 for (i = 0; i < d; i++) {
			 for (j = 0; j < n; j++) {
				 ran[i][j] = randomGen.genStandardGaussian();
			 }
		 }
		 double VT[][] = new double[d][d];
		 for (j = 0; j < d; j++) {
			 for (i = j; i < d; i++) {
			     VT[i][j] = V[j][i];	 
			 }
		 }
		 double VTran[][] = new double[d][n];
		 for (i = 0; i < d; i++) {
			 for (j = 0; j < n; j++) {
				 for (k = 0; k < d; k++) {
				     VTran[i][j] += VT[i][k] * ran[k][j];	 
				 }
			 }
		 }
		 double x[][] = new double[d][n];
		 for (i = 0; i < d; i++) {
			 for (j = 0; j < n; j++) {
				 x[i][j] = VTran[i][j] + mu[i];
			 }
		 }
		 return x;
     }
     
     private void plotClass(double X[][], int label[]) {
	     // Plot 2d/3d samples of different classes with different colors.
	     // Written by Mo Chen (sth4nth@gmail.com).
    	 int i;
    	 int d = X.length;
    	 int n = X[0].length;
	     //if nargin == 1
	     //    label = ones(n,1);
	     //end
	     if (n != label.length) {
	    	 System.err.println("In plotClass n = " + n + ", but label.length = " + label.length);
	    	 return;
	     }
	
	     int m = 7; // number of colors
	     Color color[] = new Color[7];
	     color[0] = Color.BLUE;
	     color[1] = Color.RED;
	     color[2] = Color.GREEN;
	     color[3] = Color.MAGENTA;
	     color[4] = Color.CYAN;
	     color[5] = Color.YELLOW;
	     color[6] = Color.BLACK;
	     //c = max(label)
	     int c = Integer.MIN_VALUE;
	     for (i = 0; i < label.length; i++) {
	    	 if (label[i] > c) {
	    		 c = label[i];
	    	 }
	     }
	     System.out.println("max label = " + c);
	     
	     switch(d) {
	     case 2:
	    	 for (i = 1; i <= c; i++) {
	    		 
	    	 } // for (i = 1; i <= c; i++)
	    	 break;
	     }
	
	     /*figure(gcf);
	     clf;
	     hold on;
	     switch d
	         case 2
	             view(2);
	             for i = 1:c
	                 idc = label==i;
	     %             plot(X(1,label==i),X(2,label==i),['.' color(i)],'MarkerSize',15);
	                 scatter(X(1,idc),X(2,idc),36,color(mod(i-1,m)+1));
	             end
	         case 3
	             view(3);
	             for i = 1:c
	                 idc = label==i;
	     %             plot3(X(1,idc),X(2,idci),X(3,idc),['.' idc],'MarkerSize',15);
	                 scatter3(X(1,idc),X(2,idc),X(3,idc),36,color(mod(i-1,m)+1));
	             end
	         otherwise
	             error('ERROR: only support data of 2D or 3D.');
	     end
	     axis equal
	     grid on
	     hold off*/
     }
	
}