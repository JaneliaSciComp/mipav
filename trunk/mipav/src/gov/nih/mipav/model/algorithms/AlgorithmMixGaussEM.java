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
	// Seeiing split classes.  One class on 2 opposite sides of another class
	private double X[][];
	private int label[];
	private model mixGaussOut;
	private double llh[];
	private int iter;
	RandomNumberGen randomGen = new RandomNumberGen();
	
	public AlgorithmMixGaussEM() {
		
	}
	
	public void runAlgorithm() {
		
	}
	
	public void mixGaussEm_demo() {
		int i,j;
		int d = 2;
		int k = 3;
		int n = 500;
		mixGaussRnd(d,k,n);
		plotClass(X,label,"Initial Classes");
		
		int m = n/2;
		// X is d by n
		double X1[][] = new double[d][m];
		double X2[][] = new double[d][n-m];
		for (i = 0; i < d; i++) {
			for (j = 0; j < m; j++) {
				X1[i][j] = X[i][j];
			}
			for (j = m; j < n; j++) {
				X2[i][j-m] = X[i][j];
			}
		}
		// train
		mixGaussEm(X1,k);
		plot(llh);
		plotClass(X1,label,"Trained Classes");
	}
	
	class model {
		double mu[][];
		double Sigma[][][];
		double w[];
		
		public model(double mu[][], double Sigma[][][], double w[]) {
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
		    wvg.run();
		    le2.dgetrf(d,d,SA,d,ipiv,info);
		    boolean rankDeficient = false;
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
		    		  Sigma[y][x][i-1] = SA[y][x];
		    		  bSigma[y][x] = beta0*Sigma[y][x][i-1];
		    	  }
		      }
		      gOut = gaussRnd(mu0, bSigma);
		      for (y = 0; y < d; y++) {
		    	  mu[y][i-1] = gOut[y];
		      }
		      for (y = 0; y < d; y++) {
		    	  mui[y] = mu[y][i-1];
		      }
		      for (y = 0; y < d; y++) {
		    	  for (x = 0; x < d; x++) {
		    	      bSigma[y][x] = Sigma[y][x][i-1];  
		    	  }
		      }
		      Xout = gaussRnd(mui, bSigma, idxSum);
		      for (y = 0; y < d; y++) {
		    	  for (x = 0, index = 0; x < n; x++) {
		    		  if (idx[x]) {
		    		      X[y][x] = Xout[y][index++];
		    		  }
		    	  }
		      }
		} // for (i = 1; i <= k; i++)
		mixGaussOut = new model(mu, Sigma, w);
		return;
	}
	
	public void test_discreteRnd() {
		// test_discreteRnd passes
		// Requested probability = 0.1
		// Requested probability = 0.3
		// Requested probability = 0.6
		// Calculated probability = 0.10037
		// Calculated probability = 0.29944
		// Calculated probability = 0.60019
		int i,j;
		double pin[] = new double[] {0.1,0.3,0.6};
		int numOut[] = new int[pin.length];
		double pout[] = new double[pin.length];
		for (i = 0; i < pin.length; i++) {
			System.out.println("Requested probability = " + pin[i]);
		}
		int n = 100000;
		int samples[] = discreteRnd(pin, n);
		for (j = 1; j <= pin.length; j++) {
			for (i = 0; i < n; i++) {
				if (samples[i] == j) {
					numOut[j-1]++;
				}
			}
		}
		for (i = 0; i < pout.length; i++) {
			pout[i] = (double)numOut[i]/(double)n;
			System.out.println("Calculated probability = " + pout[i]);
		}
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
		// bin indices
		int x[] = new int[n];
		for (i = 0; i < n; i++) {
			found = false;
		    for (j = 0; j < binranges.length-1 && (!found); j++) {
		        if ((r[i] >= binranges[j]) && (r[i] < binranges[j+1])) {
		            found = true;
		            x[i] = j+1;
		        }
		    }
		    if (!found) {
		    	// Only has the value for r[i] = 1.0;
		    	x[i] = binranges.length;
		    }
		}
	    return x;
	}
	
	public void test_dirichletRnd() {
		// test_dirichletRnd passes
		// expectedAverage[0] = 0.0
		// expectedAverage[1] = 0.022222222222222223
		// expectedAverage[2] = 0.044444444444444446
		// expectedAverage[3] = 0.06666666666666667
		// expectedAverage[4] = 0.08888888888888889
		// expectedAverage[5] = 0.1111111111111111
		// expectedAverage[6] = 0.13333333333333333
		// expectedAverage[7] = 0.15555555555555556
		// expectedAverage[8] = 0.17777777777777778
		// expectedAverage[9] = 0.2
		// actualAverage[0] = 0.0
		// actualAverage[1] = 0.022128327702693545
		// actualAverage[2] = 0.04469011010870423
		// actualAverage[3] = 0.06674813589550753
		// actualAverage[4] = 0.08856580860958682
		// actualAverage[5] = 0.11148441317509951
		// actualAverage[6] = 0.13346869394877817
		// actualAverage[7] = 0.1556538683773657
		// actualAverage[8] = 0.17774747770886784
		// actualAverage[9] = 0.19951316447339668

		int i,j;
		double m[] = new double[10];
		double msum = 0.0;
		for (i = 0; i < m.length; i++) {
			m[i] = i;
			msum += i;
		}
		double expectedAverage[] = new double[m.length];
		for (i = 0; i < m.length; i++) {
			expectedAverage[i] = m[i]/msum;
			System.out.println("expectedAverage["+i+"] = " + expectedAverage[i]);
		}
		double ain = 20.0;
		double actualAverage[] = new double[m.length];
		for (i = 0; i < 1000; i++) {
		    double x[] = dirichletRnd(ain, m);
		    for (j = 0; j < m.length; j++) {
		    	actualAverage[j] += x[j];
		    }
		}
		for (i = 0; i < m.length; i++) {
			actualAverage[i] = actualAverage[i]/1000;
			System.out.println("actualAverage["+i+"] = " + actualAverage[i]);
		}
		
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
	
	public void test_gamrnd() {
		// test_gamrnd passes
		// Expected meanx = (a/b) = 1.875 Actual meanx = 1.8758717848708735
		double a = 3.75;
		double b = 2.0;
		int i;
		double x;
		double xsum = 0.0;
		for (i = 0; i < 100000; i++) {
		    x = gamrnd(a,b);	
		    xsum += x;
		}
		
	    double meanx = xsum/100000.0;
	    System.out.println("Expected meanx = (a/b) = " + (a/b) + " Actual meanx = " + meanx);
	}

     public double gamrnd(double a, double b) {
    	 // From A Simple Method for Generating Gamma Variables
    	 // by George Marsaglia and Wai Wan Tsang
    	 double d;
    	 double c;
    	 boolean cycle = true;
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
     
     public void test_gaussRnd2() {
    	 // test_gaussRnd2 passes
    	 // Requested mu[0] = 1.5 calculated mu[0] = 1.502759682658677
		 // Requested mu[1] = 3.4 calculated mu[1] = 3.400619707092323
		 // Requested mu[2] = 6.5 calculated mu[2] = 6.507958541033659
		 // Requested covariance[0][0] = 3.0 calculated covariance[0][0] = 3.0104961516943454
		 // Requested covariance[0][1] = 2.0 calculated covariance[0][1] = 2.0164865293532612
		 // Requested covariance[0][2] = 1.0 calculated covariance[0][2] = 1.0008600259521765
		 // Requested covariance[1][0] = 2.0 calculated covariance[1][0] = 2.0164865293532612
		 // Requested covariance[1][1] = 4.0 calculated covariance[1][1] = 4.013134531525033
		 // Requested covariance[1][2] = 1.5 calculated covariance[1][2] = 1.5255898932111716
		 // Requested covariance[2][0] = 1.0 calculated covariance[2][0] = 1.0008600259521765
		 // Requested covariance[2][1] = 1.5 calculated covariance[2][1] = 1.5255898932111716
		 // Requested covariance[2][2] = 6.0 calculated covariance[2][2] = 6.007168895800442
    	 int i,j,k;
    	 int d = 3;
    	 int n = 100000;
    	 double mu[] = new double[] {1.5, 3.4, 6.5};
    	 double Sigma[][] = new double[][] {{3.0, 2.0, 1.0}, {2.0, 4.0, 1.5}, {1.0, 1.5, 6.0}};
    	 double x[][] = new double[n][];
    	 for (i = 0; i < n; i++) {
    	     x[i] = gaussRnd(mu, Sigma);
    	 }
    	 double mucalc[] = new double[3];
    	 for (i = 0; i < d; i++) {
    		 for (j = 0; j < n; j++) {
    		     mucalc[i] += x[j][i];	 
    		 }
    	 }
    	 for (i = 0; i < d; i++) {
    		 mucalc[i] = mucalc[i]/n;
    		 System.out.println("Requested mu["+i+"] = " + mu[i] + " calculated mu["+i+"] = " + mucalc[i]);
    	 }
    	 double calcCov[][] = new double[3][3];
    	 for (i = 0; i < d; i++) {
    		 for (j = 0; j < d; j++) {
    		     for (k = 0; k < n; k++) {
    		    	 calcCov[i][j] += (x[k][i] - mucalc[i])*(x[k][j] - mucalc[j]);
    		     }
    		 }
    	 }
    	 for (i = 0; i < d; i++) {
    		 for (j = 0; j < d; j++) {
    			 calcCov[i][j] = calcCov[i][j]/(n-1);
    			 System.out.println("Requested covariance["+i+"]["+j+"] = " + Sigma[i][j] + 
    					 " calculated covariance["+i+"]["+j+"] = " + calcCov[i][j]);
    		 }
    	 }
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
     
     public void test_gaussRnd() {
    	 // test_gaussRnd passes.
    	 // Requested mu[0] = 1.5 calculated mu[0] = 1.4842322099613756
		 // Requested mu[1] = 3.4 calculated mu[1] = 3.396309971060101
		 // Requested mu[2] = 6.5 calculated mu[2] = 6.499841239593269
		 // Requested covariance[0][0] = 3.0 calculated covariance[0][0] = 2.986776038743687
		 // Requested covariance[0][1] = 2.0 calculated covariance[0][1] = 1.9865031922704346
		 // Requested covariance[0][2] = 1.0 calculated covariance[0][2] = 1.0099657551698031
		 // Requested covariance[1][0] = 2.0 calculated covariance[1][0] = 1.9865031922704346
		 // Requested covariance[1][1] = 4.0 calculated covariance[1][1] = 3.9715560645648176
		 // Requested covariance[1][2] = 1.5 calculated covariance[1][2] = 1.4952752061897359
		 // Requested covariance[2][0] = 1.0 calculated covariance[2][0] = 1.0099657551698031
		 // Requested covariance[2][1] = 1.5 calculated covariance[2][1] = 1.4952752061897359
		 // Requested covariance[2][2] = 6.0 calculated covariance[2][2] = 5.992249921374945
    	 int i,j,k;
    	 int d = 3;
    	 double mu[] = new double[] {1.5, 3.4, 6.5};
    	 double Sigma[][] = new double[][] {{3.0, 2.0, 1.0}, {2.0, 4.0, 1.5}, {1.0, 1.5, 6.0}};
    	 int n = 100000;
    	 double x[][] = gaussRnd(mu, Sigma, n);
    	 double mucalc[] = new double[3];
    	 for (i = 0; i < d; i++) {
    		 for (j = 0; j < n; j++) {
    		     mucalc[i] += x[i][j];	 
    		 }
    	 }
    	 for (i = 0; i < d; i++) {
    		 mucalc[i] = mucalc[i]/n;
    		 System.out.println("Requested mu["+i+"] = " + mu[i] + " calculated mu["+i+"] = " + mucalc[i]);
    	 }
    	 double calcCov[][] = new double[3][3];
    	 for (i = 0; i < d; i++) {
    		 for (j = 0; j < d; j++) {
    		     for (k = 0; k < n; k++) {
    		    	 calcCov[i][j] += (x[i][k] - mucalc[i])*(x[j][k] - mucalc[j]);
    		     }
    		 }
    	 }
    	 for (i = 0; i < d; i++) {
    		 for (j = 0; j < d; j++) {
    			 calcCov[i][j] = calcCov[i][j]/(n-1);
    			 System.out.println("Requested covariance["+i+"]["+j+"] = " + Sigma[i][j] + 
    					 " calculated covariance["+i+"]["+j+"] = " + calcCov[i][j]);
    		 }
    	 }
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
     
     private void plotClass(double X[][], int label[], String title) {
	     // Plot 2d/3d samples of different classes with different colors.
	     // Written by Mo Chen (sth4nth@gmail.com).
    	 int i,j,index,maxIndex;
    	 int d = X.length;
    	 int n = X[0].length;
    	 int numCount[];
    	 boolean haveEmptyClass = false;
	     int numEmptyClasses = 0;
	     int newC;
	     int newLabel[];
	     int presentLabelNum;
	     //if nargin == 1
	     //    label = ones(n,1);
	     //end
	     if (n != label.length) {
	    	 System.err.println("In plotClass n = " + n + ", but label.length = " + label.length);
	    	 return;
	     }
	
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
	    	 numCount = new int[c];
	    	 for (i = 1; i <= c; i++) {
	    		 numCount[c-1] = 0;
	    		 for (j = 0; j < n; j++) {
	    			 if (label[j] == i) {
	    			     numCount[c-1]++;	 
	    			 }
	    		 }
	    		 System.out.println("Number in class " + i + " is " + numCount[c-1]);
	    		 if (numCount[c-1] == 0) {
	    			 haveEmptyClass = true;
	    			 numEmptyClasses++;
	    		 }
	    	 }
	    	 
	    	 if (haveEmptyClass) {
	    		 newC = c - numEmptyClasses;
	    		 System.out.println("Reducing to " + newC + " classes");
	    		 newLabel = new int[label.length];
	    		 presentLabelNum = 1;
	    		 for (i = 1; i <= c; i++) {
	    			 if (numCount[c-1] != 0) {
	    				 for (j = 0; j < n; j++) {
	    					 if (label[j] == i) { 
	    						 newLabel[j] = presentLabelNum;
	    					 }
	    				 }
	    				 presentLabelNum++;
	    			 }
	    		 }
	    		 c = newC;
	    		 label = newLabel;
	    	 }
	    	 
	    	 float xInit[][] = new float[c][];
		     float yInit[][] = new float[c][];
		     maxIndex = 0;
	    	 for (i = 1; i <= c; i++) {
	    		 index = 0;
	    	     for (j = 0; j < n; j++) {
	    	    	 if (label[j]  == i) {
	    	    		 index++;
	    	    	 }
	    	     }
	    	     if (index > maxIndex) {
	    	    	 maxIndex = index;
	    	     }
	    	 }
	    	 for (i = 0; i < c; i++) {
	    		 xInit[i] = new float[maxIndex];
	    		 yInit[i] = new float[maxIndex];
	    	 }
	    	 for (i = 1; i <= c; i++) {
	    	     index = 0;
	    	     for (j = 0; j < n; j++) {
	    	    	 if (label[j] == i) {
	    	    	     xInit[i-1][index] = (float)X[0][j];
	    	    	     yInit[i-1][index++] = (float)X[1][j];
	    	    	 }
	    	     }
	    	     if (index > 0) {
		    	     while (index < maxIndex) {
	    	    		 xInit[i-1][index] = xInit[i-1][index-1];
	    	    		 yInit[i-1][index] = yInit[i-1][index-1];
	    	    		 index++;
	    	    	 }
	    	     }
	    	 } // for (i = 1; i <= c; i++)
    	     String labelX = "X coordinate";
    	     String labelY = "Y coordinate";
    	     // Colors only for lines
    	     ViewJFrameGraph vFrameGraph = new ViewJFrameGraph(xInit, yInit, title, labelX, labelY);
    	     ViewJComponentGraph vcGraph = vFrameGraph.getGraph();
    	     vcGraph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_POINTS_ONLY);
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
     
     //function [label, model, llh] = mixGaussEm(X, init)
     private void mixGaussEm(double X[][], int init) {
		 // Perform EM algorithm for fitting the Gaussian mixture model.
		 // Input: 
		 //   X: d x n data matrix
		 //   init: k (1 x 1) number of components or label (1 x n, 1<=label(i)<=k) or model structure
		 // Output:
		 //   label: 1 x n cluster label
		 //   model: trained model structure
		 //   llh: loglikelihood
		 // Written by Mo Chen (sth4nth@gmail.com).
		 // init
    	 int i,j,index;
		 System.out.println("EM for Gaussian mixture: running ...");
		 double tol = 1e-6;
		 int maxiter = 500;
		 llh = new double[maxiter];
		 for (i = 0; i < maxiter; i++) {
			 llh[i] = Double.NEGATIVE_INFINITY;
		 }
		 iter = 0;
		 double R[][] = initialization(X,init);
		 int n = R.length;
		 int k = R[0].length;
		 double maxVal;
		 int maxIndex;
		 label = new int[n];
		 for (iter = 1; iter < maxiter; iter++) {
			 for (i = 0; i < n; i++) {
				 maxVal = -Double.MAX_VALUE;
				 maxIndex = -1;
				 for (j = 0; j < k; j++) {
					 if (R[i][j] > maxVal) {
						 maxVal = R[i][j];
						 maxIndex = j+1;
					 }
				 }
				 label[i] = maxIndex;
			 }
		     boolean haveK[] = new boolean[k];
		     for (i = 0; i < n; i++) {
		    	 if (label[i] >= 1) {
		    	     haveK[label[i]-1] = true;
		    	 }
		     }
		     int numLabel = 0;
		     int newLabel[] = new int[k];
		     for (i = 0, index = 0; i < k; i++) {
		    	 if (haveK[i]) {
		    		 newLabel[i] = index++;
		    		 numLabel++;
		    	 }
		     }
		     if (numLabel < k) {
		    	 System.out.println("iter = " + iter);
		    	 System.out.println("numLabel = " + numLabel);
		    	 // Remove empty clusters
		         double tempR[][] = new double[n][numLabel];
		         for (i = 0; i < n; i++) {
		        	 for (j = 0; j < k; j++) {
		        		 if (haveK[j]) {
		        			 tempR[i][newLabel[j]] = R[i][j];
		        		 }
		        	 }
		         }
		         k = numLabel;
		         R = new double[n][k];
		         for (i = 0; i < n; i++) {
		        	 for (j = 0; j < k; j++) {
		        		 R[i][j] = tempR[i][j];
		        	 }
		         }
		         tempR = null;
		     } // if (numLabel < k)
		     
		     model mod = maximization(X,R);
		     R = expectation(X,mod);
		     if (Math.abs(llh[iter]-llh[iter-1]) < tol*Math.abs(llh[iter])) {
		    	 break;
		     }
		 } // for (iter = 1; iter < maxiter; iter++)
		 double llhtemp[] = new double[iter];
		 for (i = 1; i <= iter; i++) {
			 llhtemp[i-1] = llh[i];
		 }
		 llh = new double[iter];
		 for (i = 0; i < iter; i++) {
			 llh[i] = llhtemp[i];
		 }
		 llhtemp = null;
     }
     
     private double[][] initialization(double X[][], int init) {
    	 // Init with random init k
    	 int i;
    	 int n = X[0].length;
    	 int k = init;
 		 double r[] = new double[n];
 		 for (i = 0; i < n; i++) {
 			r[i] = randomGen.genUniformRandomNum(0.0,1.0);
 		 }
 		 int label[] = new int[n];
 		 for (i = 0; i < n; i++) {
 			label[i] = (int)Math.ceil(k*r[i]);
 			if (label[i] == 0) {
 				label[i] = 1;
 			}
 		 }
 		 double R[][] = new double[n][k];
 		 for (i = 0; i < n; i++) {
 			R[i][label[i]-1] = 1;
 		 }
 		 return R;
     }
     
     private double[][] initialization(double X[][], int init[]) {
    	 // Init with labels
    	 int i;
    	 int n = X[0].length;
    	 int label[] = init;
    	 int k = 0;
 		 for (i = 0; i < n; i++) {
	 		 if (label[i] > k) {
	 			 k = label[i];
	 		 }
 		 }
 		 double R[][] = new double[n][k];
 		 for (i = 0; i < n; i++) {
 			R[i][label[i]-1] = 1;
 		 }
 		 return R;
     }
     
     private double[][] initialization(double X[][], model init) {
    	 // init with a model
    	 double R[][] = expectation(X,init);
    	 return R;
     }
     
     //function [R, llh] = expectation(X, model)
     private double[][] expectation(double X[][], model mod) {
    	 int i,j,m;
		 double mu[][] = mod.mu;
		 double Sigma[][][] = mod.Sigma;
		 double w[] = mod.w;

		 int n = X[0].length;
		 int k = mu[0].length;
		 double R[][] = new double[n][k];
		 int d = Sigma.length;
		 double mui[] = new double[d];
		 double Sigmai[][] = new double[d][d];
		 double out[];
		 for (i = 0; i < k; i++) {
			 for (j = 0; j < d; j++) {
				 mui[j] = mu[j][i];
				 for (m = 0; m < d; m++) {
					 Sigmai[j][m] = Sigma[j][m][i];
				 }
			 }
		     out = loggausspdf(X,mui, Sigmai);
		     for (j = 0; j < n; j++) {
		    	 R[j][i] = out[j];
		     }
		 }
		 for (j = 0; j < k; j++) {
			 for (i = 0; i < n; i++) {
				 R[i][j] = R[i][j] + Math.log(w[j]);
			 }
		 }
		 // Form a column vector with the maximum number in each row
		 double T[] = logsumexp(R,1);
		 double sumT = 0.0;
		 for (i = 0; i < n; i++) {
			 sumT += T[i];
		 }
		 if (iter > 0) {
		     llh[iter] = sumT/n; // loglikelihood
		 }
		 for (j = 0; j < k; j++) {
			 for (i = 0; i < n; i++) {
				 R[i][j] = R[i][j] - T[i];
				 R[i][j] = Math.exp(R[i][j]);
			 }
		 }
		 return R;
     }

     private double[] loggausspdf(double Xin[][], double mu[], double Sigma[][]) {
    	 int i,j;
		 int d = Xin.length;
		 int n = Xin[0].length;
		 double X[][] = new double[d][n];
		 for (i = 0; i < d; i++) {
			 for (j = 0; j < n; j++) {
				 X[i][j] = Xin[i][j] - mu[i];
			 }
         }
		 // dpotrf computes the Cholesky factorization of a real symmetric
		 // positive definite matrix A
		 // D is an upper triangular array, such that sigma = DPRIME * D
		 // On output from dpotrf the leading n-by-n upper triangular part of D
		 // contains the upper triangular part of the matrix D.
		 GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
		 int info[] = new int[1];
		 ge.dpotrf('U', d, Sigma, Sigma.length, info);
		 if (info[0] < 0) {
		     System.err.println("In loggausspdf dpotrf argument " + (-info[0]) + " had an illegal value");
			 return null;
		 }
		 else if (info[0] > 0) {
			 System.err.println("In loggausspdf dpotrf the leading minor of order " + info[0] + " is not positive definite,"); 
			 System.err.println("and the factorization could not be completed.");
			 return null;
		 }
		 double U[][] = new double[d][d];
		 double UT[][] = new double[d][d];
		 for (j = 0; j < d; j++) {
			 for (i = 0; i <= j; i++) {
				 U[i][j] = Sigma[i][j];
				 UT[j][i] = Sigma[i][j];
			 }
		 }
		 LinearEquations2 le2 = new LinearEquations2();
		 int ipiv[] = new int[d];
		 le2.dgesv(d,n,UT,d,ipiv,X,d,info);
		 if (info[0] < 0) {
		     System.err.println("In loggausspdf dgesv argument " + (-info[0]) + " had an illegal value");
			 return null;
		 }
		 else if (info[0] > 0) {
			 System.err.println("In loggausspdf dgesv U["+(info[0]-1)+"]["+(info[0]-1)+"] is exactly 0"); 
			 System.err.println("The factorization has been completed, but the factor U is exactly"); 
	         System.err.println("singular, so the solution could not be computed");
			 return null;
		 }
		 // X is d by n
		 // q is quadratic term (M distance)
		 double q[] = new double[n];
		 for (j = 0; j < n; j++) {
			 for (i = 0; i < d; i++) {
			     q[j] += X[i][j] * X[i][j];	 
			 }
		 }
		 double sum = 0.0;
		 for (i = 0; i < d; i++) {
			 sum += Math.log(U[i][i]);
		 }
		 // normalization constant c
		 double c = d*Math.log(2*Math.PI)+2*sum;  
		 double y[] = new double[n];
		 for (i = 0; i < n; i++) {
			 y[i] = -(c + q[i])/2.0;
		 }
		 return y;
     }
     
     private model maximization(double X[][], double R[][]) {
    	 int i,j,m,p;
		 int d = X.length;
		 int n = X[0].length;
		 int k = R[0].length;
		 double nk[] = new double[k];
		 for (j = 0; j < k; j++) {
			 for (i = 0; i < R.length; i++) {
				 nk[j] += R[i][j];
			 }
		 }
		 double w[] = new double[k];
		 for (i = 0; i < k; i++) {
			 w[i] = nk[i]/n;
		 }
		 double XR[][] = new double[d][k];
		 for (i = 0; i < d; i++) {
			 for (j = 0; j < k; j++) {
				 for (m = 0; m < n; m++) {
					 XR[i][j] += (X[i][m] * R[m][j]);
				 }
			 }
		 }
		 double mu[][] = new double[d][k];
		 for (i = 0; i < d; i++) {
			 for (j = 0; j < k; j++) {
				 mu[i][j] = XR[i][j] * (1.0/nk[j]);
			 }
		 }

		 double Sigma[][][] = new double[d][d][k];
		 double r[][] = new double[n][k];
		 for (i = 0; i < n; i++) {
			 for (j = 0; j < k; j++) {
				 r[i][j] = Math.sqrt(R[i][j]);
			 }
		 }
		 double Xo[][] = new double[d][n];
		 double XoT[][] = new double[n][d];
		 double XoXo[][] = new double[d][d];
		 for (i = 0; i < k; i++) {
			 for (j = 0; j < d; j++) {
				 for (m = 0; m < n; m++) {
					 Xo[j][m] = X[j][m] - mu[j][i];
					 Xo[j][m] = Xo[j][m] * r[m][i];
					 XoT[m][j] = Xo[j][m];
				 }
			 }
			 for (j = 0; j < d; j++) {
				 for (m = 0; m < d; m++) {
					 XoXo[j][m] = 0.0;
					 for (p = 0; p < n; p++) {
					     XoXo[j][m] += Xo[j][p] * XoT[p][m]; 
					 }
				 }
			 }
			 for (j = 0; j < d; j++) {
				 for (m = 0; m < d; m++) {
					 if (j == m) {
						 Sigma[j][m][i] = XoXo[j][m]/nk[i] + 1.0E-6;
					 }
					 else {
						 Sigma[j][m][i] = XoXo[j][m]/nk[i];
					 }
				 }
			 }
		 } // for (i = 0; i < k; i++)
		 
		 model mod = new model(mu, Sigma, w);
		 return mod;
     }
     
     private void plot(double llh[]) {
    	 int i;
	     float xInit[] = new float[llh.length];
	     for (i = 0; i < llh.length; i++) {
	    	 xInit[i] = i+1;
	     }
	     float yInit[] = new float[llh.length];
	     for (i = 0; i < llh.length; i++) {
	    	 yInit[i] = (float)llh[i];
	     }
	     String title = "llh";
	     String labelX = "Iteration";
	     String labelY = "Log likelihood";
	     ViewJFrameGraph vFrameGraph = new ViewJFrameGraph(xInit, yInit, title, labelX, labelY);	 
     }
     
     public double[] logsumexp(double ain[][], int dim) {
 		// Returns log(sum(exp(a),dim)) while avoiding numerical underflow.
 		// Default is dim = 0 (columns).
 		// logsumexp(a, 1) will sum across rows instead of columns.

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

     
}