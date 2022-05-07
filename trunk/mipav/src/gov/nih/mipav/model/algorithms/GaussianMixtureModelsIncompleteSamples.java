package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;

import Jama.Matrix;
import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

/**
 * 
 * @author aailb
 * Ported from pygmmis.py
 *MIT License

Copyright (c) 2017 Peter Melchior

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

[![PyPI](https://img.shields.io/pypi/v/pygmmis.svg)](https://pypi.python.org/pypi/pygmmis/)
[![License](https://img.shields.io/github/license/pmelchior/pygmmis.svg)](https://github.com/pmelchior/pygmmis/blob/master/LICENSE.md)
[![DOI](https://img.shields.io/badge/DOI-10.1016%2Fj.ascom.2018.09.013-blue.svg)](https://doi.org/10.1016/j.ascom.2018.09.013)
[![arXiv](https://img.shields.io/badge/arxiv-1611.05806-red.svg)](http://arxiv.org/abs/1611.05806)

# pyGMMis

Need a simple and powerful Gaussian-mixture code in pure python? It can be as easy as this:

```python
import pygmmis
gmm = pygmmis.GMM(K=K, D=D)      # K components, D dimensions
logL, U = pygmmis.fit(gmm, data) # logL = log-likelihood, U = association of data to components
```
However, **pyGMMis** has a few extra tricks up its sleeve.

* It can account for independent multivariate normal measurement errors for each of the observed samples, and then recovers
*  an estimate of the error-free distribution. This technique is known as "Extreme Deconvolution" by Bovy, Hogg & Roweis (2011).
* It works with missing data (features) by setting the respective elements of the covariance matrix to a vary large value, 
* thus effectively setting the weights of the missing feature to 0.
* It can deal with gaps (aka "truncated data") and variable sample completeness as long as
  * you know the incompleteness over the entire feature space,
  * and the incompleteness does not depend on the sample density (missing at random).
* It can incorporate a "background" distribution (implemented is a uniform one) and separate signal from background,
*  with the former being fit by the GMM.
* It keeps track of which components need to be evaluated in which regions of the feature space, thereby substantially
*  increasing the performance for fragmented data.

If you want more context and details on those capabilities, have a look at this [blog post](http://pmelchior.net/blog/gaussian-mixture-models-for-astronomy.html).

Under the hood, **pyGMMis** uses the Expectation-Maximization procedure. When dealing with sample incompleteness
 it generates its best guess of the unobserved samples on the fly given the current model fit to the observed samples.

![Example of pyGMMis](https://raw.githubusercontent.com/pmelchior/pygmmis/master/tests/pygmmis.png)

In the example above, the true distribution is shown as contours in the left panel. We then draw 400 samples from it (red),
 add Gaussian noise to them (1,2,3 sigma contours shown in blue), and select only samples within the box but outside of the circle (blue).

The code is written in pure python (developed and tested in 2.7), parallelized with `multiprocessing`, 
and is capable of performing density estimation with millions of samples and thousands of model components on machines with sufficient memory.

More details are in the paper listed below. Please cite it if you make use of this code:


ARTICLE{pygmmis,
   author = {{Melchior}, P. and {Goulding}, A.~D.},
    title = "{Filling the gaps: Gaussian mixture models from noisy, truncated or incomplete samples}",
  journal = {Astronomy and Computing},
   volume = "25",
    pages = {183 - 194},
     year = "2018",
    month = oct,
      doi = {10.1016/j.ascom.2018.09.013},
      url = {https://www.sciencedirect.com/science/article/pii/S2213133718300489},
archivePrefix = "arXiv",
   eprint = {1611.05806},
 primaryClass = "astro-ph.IM"
 
 ## How to run the code

1. Create a GMM object with the desired component number K and data dimensionality D:
   ```gmm = pygmmis.GMM(K=K, D=D) ```

3. Define a callback for the completeness function. When called with with `data` with shape `(N,D)`
 and returns the probability of each sample getting observed. Two simple examples:

   ```python
   def cutAtSix(coords):
   	"""Selects all samples whose first coordinate is < 6"""
       return (coords[:,0] < 6)

   def selSlope(coords, rng=np.random):
       """Selects probabilistically according to first coordinate x:
       Omega = 1    for x < 0
             = 1-x  for x = 0 .. 1
             = 0    for x > 1
       """
       return np.max(0, np.min(1, 1 - coords[:,0]))
   ```

4. If the samples are noisy (i.e. they have positional uncertainties), you need to provide the covariance matrix of each data sample,
 or one for all in case of i.i.d. noise.

4. If the samples are noisy *and* there completeness function isn't constant, you need to provide a callback function that returns
 an estimate of the covariance at arbitrary locations:

   ```python
   # example 1: simply using the same covariance for all samples
   dispersion = 1
   default_covar = np.eye(D) * dispersion**2
   covar_cb = lambda coords: default_covar
   
   # example: use the covariance of the nearest neighbor.
   def covar_tree_cb(coords, tree, covar):
       """Return the covariance of the nearest neighbor of coords in data."""
       dist, ind = tree.query(coords, k=1)
       return covar[ind.flatten()]
   
   from sklearn.neighbors import KDTree
   tree = KDTree(data, leaf_size=100)
   
   from functools import partial
   covar_cb = partial(covar_tree_cb, tree=tree, covar=covar)
   ```

5. If there is a uniform background signal, you need to define it. Because a uniform distribution is normalizable only if its support is finite, you need to decide on the footprint over which the background model is present, e.g.:

   ```python
   footprint = data.min(axis=0), data.max(axis=0)
   amp = 0.3
   bg = pygmmis.Background(footprint, amp=amp)
   
   # fine tuning, if desired
   bg.amp_min = 0.1
   bg.amp_max = 0.5
   bg.adjust_amp = False # freezes bg.amp at current value
   ```

6. Select an initialization method. This tells the GMM what initial parameters is should assume. The options are `'minmax','random','kmeans','none'`. See the respective functions for details:

   * `pygmmis.initFromDataMinMax()`
   * `pygmmis.initFromDataAtRandom()`
   * `pygmmis.initFromKMeans()`

   For difficult situations, or if you are not happy with the convergence, you may want to experiment with your own initialization. All you have to do is set `gmm.amp`, `gmm.mean`, and `gmm.covar` to desired values and use `init_method='none'`.

7. Decide to freeze out any components. This makes sense if you *know* some of the parameters of the components. You can freeze amplitude, mean, or covariance of any component by listing them in a dictionary, e.g:

   ```python
   frozen={"amp": [1,2], "mean": [], "covar": [1]}
   ```

   This freezes the amplitudes of component 1 and 2 (NOTE: Counting starts at 0), and the covariance of 1.

8. Run the fitter:

   ```python
   w = 0.1    # minimum covariance regularization, same units as data
   cutoff = 5 # segment the data set into neighborhood within 5 sigma around components
   tol = 1e-3 # tolerance on logL to terminate EM
   
   # define RNG for deterministic behavior
   from numpy.random import RandomState
   seed = 42
   rng = RandomState(seed)
   
   # run EM
   logL, U = pygmmis.fit(gmm, data, init_method='random',\
                         sel_callback=cb, covar_callback=covar_cb, w=w, cutoff=cutoff,\
                         background=bg, tol=tol, frozen=frozen, rng=rng)
   ```

   This runs the EM procedure until tolerance is reached and returns the final mean log-likelihood of all samples, and the neighborhood of each component (indices of data samples that are within cutoff of a GMM component).

9. Evaluate the model:

   ```python
   # log of p(x)
   p = gmm(test_coords, as_log=False)
   N_s = 1000
   # draw samples from GMM
   samples = gmm.draw(N_s)
   
   # draw sample from the model with noise, background, and selection:
   # if you want to get the missing sample, set invert_sel=True.
   # N_orig is the estimated number of samples prior to selection
   obs_size = len(data)
   samples, covar_samples, N_orig = pygmmis.draw(gmm, obs_size, sel_callback=cb,\
                                                 invert_sel=False, orig_size=None,\
                                                 covar_callback=covar_cb,background=bg)
 */

public class GaussianMixtureModelsIncompleteSamples extends AlgorithmBase {
	private int K; // K components
	private int D; // D dimensions
	
    public GaussianMixtureModelsIncompleteSamples() {
		
	}
	public GaussianMixtureModelsIncompleteSamples(int K, int D) {
		this.K = K;
		this.D = D;
	}
	
	public void test() {
		int i,j,m,r;
		// set up test
	    int N = 400;         // number of samples
	    int K = 3;           // number of components
	    int T = 1;           // number of runs
	    String sel_type = "boxWithHole";    // type of selection
	    double disp = 0.5;   // additive noise dispersion
	    double bg_amp = 0.0; // fraction of background samples
	    double w = 0.1;      // minimum covariance regularization [data units]
	    double cutoff = 5;   // cutoff distance between components [sigma]
	    long seed = 8365;    // seed value
	    int oversampling = 10;   // for missing data: imputation samples per observed sample
	    // show EM iteration results
	    //logging.basicConfig(format='%(message)s',level=logging.INFO)
	    
	    // define RNG for run
	    Random rng = new Random(seed);
	    
	    // draw N points from 3-component GMM
	    int D = 2;
	    GMM gmm = new GMM(K, D);
	    gmm.amp = new double[] { 0.36060026,  0.27986906,  0.206774};
	    double sum = 0.0;
	    for (i = 0; i < K; i++) {
	    	sum += gmm.amp[i];
	    }
	    for (i = 0; i < K; i++) {
	    	gmm.amp[i] /= sum;
	    }
	    gmm.mean = new double[][] {{ 0.08016886,  0.21300697},
	    	{ 0.70306351,  0.6709532 },
	    	{ 0.01087670,  0.852077}};
	    for (i = 0; i < K; i++) {
	    	for (j = 0; j < D; j++) {
	    		gmm.mean[i][j] *= 10.0;
	    	}
	    }
	    gmm.covar = new double[][][] {{{ 0.08530014, -0.00314178},
	                                  {-0.00314178,  0.00541106}},
	    	                          {{ 0.03053402, 0.0125736},
	                                  {0.0125736,  0.01075791}},
	                                  {{0.00258605,  0.00409287},
	                                  { 0.00409287,  0.01065186}}};
	    for (i = 0; i < K; i++) {
	        for (j = 0; j < D; j++) { 
	        	for (m = 0; m < D; m++) {
	        	    gmm.covar[i][j][m] *= 100.0;	
	        	}
	        }
	    }
	    
	    // data come from pure GMM model or one with background?
	    double orig[][] = gmm.draw(N, rng);
	    double orig_bg[][];
	    Background bg;
	    if (bg_amp == 0.0) {
	        orig_bg = orig;	
	        bg = null;
	    }
	    else {
	        double footprint[][] = new double[][] {{-10,-10},{20,20}};
	        bg = new Background(footprint,0.0);
	        bg.amp = bg_amp;
	        bg.adjust_amp = true;

	        int bg_size = (int)(bg_amp/(1-bg_amp) * N);
	        double orig2[][] = bg.draw(bg_size, rng);
	        orig_bg = new double[orig.length + orig2.length][orig[0].length];
	        for (i = 0; i < orig.length; i++) {
	        	for (j = 0; j < orig[0].length; j++) {
	        	    orig_bg[i][j] = orig[i][j];	
	        	}
	        }
	        for (i = 0; i < orig2.length; i++) {
	        	for (j = 0; j < orig[0].length; j++) {
	        		orig_bg[orig.length + i][j] = orig2[i][j];
	        	}
	        }
	    } // else
	    
	    // add isotropic errors on data
	    double noisy[][] = new double[orig_bg.length][D];
	    for (i = 0; i < orig_bg.length; i++) {
	    	for (j = 0; j < D; j++) {
	    		noisy[i][j] = orig_bg[i][j] + rng.nextGaussian()*disp; 
	    	}
	    }
	    
	    // get observational selection function
	    //omega, ps = getSelection(sel_type, rng);
	    
	    // apply selection
	    // sel = rng.rand(N) < omega(noisy)
	    double ranarr[] = new double[N];
	    for (i = 0; i < N; i++) {
	    	ranarr[i] = rng.nextDouble();
	    }
	    double omega[] = null;
	    if (sel_type.equalsIgnoreCase("hole")) {
	       omega = getHole(noisy);	
	    }
	    else if (sel_type.equalsIgnoreCase("box")) {
	        omega = getBox(noisy);
	    }
	    else if (sel_type.equalsIgnoreCase("boxWithHole")) {
	        omega = getBoxWithHole(noisy);
	    }
	    else if (sel_type.equalsIgnoreCase("cut")) {
	    	omega = getCut(noisy);
	    }
	    else if (sel_type.equalsIgnoreCase("all")) {
	    	omega = getAll(noisy);
	    }
	    else if (sel_type.equalsIgnoreCase("half")) {
	    	omega = getHalf(noisy);
	    }
	    boolean sel[] = new boolean[N];
	    int numSel = 0;
	    for (i = 0; i < N; i++) {
	    	if (omega[i] > ranarr[i]) {
	    		sel[i] = true;
	    		numSel++;
	    	}
	    }
	    double data[][] = new double[numSel][D];
	    for (i = 0, j = 0; i < N; i++) {
	    	if (sel[i]) {
	    		for (m = 0; m < D; m++) {
	    	        data[j][m] = noisy[i][m];
	    	        j++;
	    		}
	    	}
	    }
	    // single covariance for all samples
	    double covar[][] = new double[D][D];
	    for (i = 0; i < D; i++) {
	    	covar[i][i] = disp*disp;
	    }
	    
	    // plot data vs true model
	    plotResults(orig, data, gmm, /*patch=ps,*/ "Truth", disp);
	    
	    // repeated runs: store results and logL
	    double l[] = new double[T];
	    GMM gmms[] = new GMM[T];
	    for (r = 0; r < T; r++) {
	    	gmms[r] = new GMM(K,D);
	    }
	    
	    // # 1) EM without imputation, ignoring errors
	    long start = System.currentTimeMillis();
	    rng = new Random(seed);
	    for (r = 0; r < T; r++) {
	        if (bg != null) {
	            bg.amp = bg_amp;	
	        }
	        l[r] = fit(gmms[r], data, null, "random", w, cutoff, null, bg, rng);
	    } // for (r = 0; r < T; r++)
	} // public void test()
	
    
	
	public void runAlgorithm() {
	    GMM gmm = new GMM(K,D);	
	}
	
    class GMM {
    	// Gaussian mixture model with K components in D dimensions.
    	int K;
    	int D;
    	double amp[]; // component amplitudes
    	double mean[][]; // component means
    	double covar[][][]; // component variances
    	public GMM(int K, int D) {
    	    this.K = K;
    	    this.D = D;
    	    amp = new double[K];
    	    mean = new double[K][D];
    	    covar = new double[K][D][D];
    	}
    	
    	public double[][] draw(int size, Random rng) {
    		// Draw samples from the GMM.

            // Args:
                // size (int): number of samples to draw
                // rng: numpy.random.RandomState for deterministic draw

            // Returns:
                // numpy array (size,D)	
    		// draw indices for components given amplitudes, need to make sure: sum=1
    		// ind = rng.choice(self.K, size=size, p=self.amp/self.amp.sum())
    		// N = np.bincount(ind, minlength=self.K)
    		int i,j,m,p;
    		boolean found;
    		double value;
    		int lower;
    		int upper;
    		int N[];
    		GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    		
    		double sum = 0.0;
    		for (i = 0; i < K; i++) {
    			sum += amp[i];
    		}
    		double prob[] = new double[K];
    		for (i = 0; i < K; i++) {
    			prob[i] = amp[i]/sum;
    		}
    		double upperLimit[] = new double[K];
    		upperLimit[K-1] = 1.0;
    		upperLimit[0] = prob[0];
    		for (i = 1; i < K-1; i++) {
    			upperLimit[i] = upperLimit[i-1] + prob[i];
    		}
    		int ind[] = new int[size];
    		for (i = 0; i < size; i++) {
    		    value = rng.nextDouble();
    		    for (j = 0; j < K; j++) {
    		    	if (value < upperLimit[j]) {
    		    		ind[i] = j;
    		    		break;
    		    	}
    		    }
    		}
    		N = new int[K];
    		
    		for (i = 0; i < K; i++) {
    			for (j = 0; j < size; j++) {
    				if (ind[j] == i) {
    					N[i]++;
    				}
    				
    			}
    		}

    		// for each component: draw as many points as in ind from a normal
    		double samples[][] = new double[size][D];
    		double L[][] = new double[D][D];
    		int info[] = new int[1];
    		double u[] = new double[D];
    		lower = 0;
    		for (i = 0; i < K; i++) {
    			if (N[i] != 0) {
    			    upper = lower + N[i];
    			    for (j = 0; j < D; j++) {
    			    	for (m = 0; m < D; m++) {
    			    		L[j][m] = covar[i][j][m];
    			    	}
    			    }
    			    // dpotrf computes the Cholesky factorization of a real symmetric
    			    // positive definite matrix A.  The factorization has the form A = U'*U, 
    			    // if uplo = 'U', or A = L * L', if uplo = 'L', where U is an upper
    			    // triangular matrix and L is lower triangular.  If uplo == 'L', the leading n-by-n lower
    			    // triangular part of A contains the lower triangular part of the matrix A, 
    			    // and the strictly upper triangular part of A is not referenced.
    			    ge.dpotrf('L',D,L,D,info);
    			    if (info[0] < 0) {
    			    	System.err.println("In GMM.draw dpotrf had an illegal value for argument " + (-info[0]));
    			    	System.exit(-1);
    			    }
    			    if (info[0] > 0) {
    			    	System.err.println("In GMM.draw for dpotrf the leading minor of order " + info[0] + " is not positive definite");
    			    	System.err.println("and the factorization could not be completed.");
    			    	System.exit(-1);
    			    }
    			    for (j = 0; j < D; j++) {
    			    	for (m = 0; m < j; m++) {
    			    		L[j][m] = 0.0;
    			    	}
    			    }
    			    for (j = lower; j < upper; j++) {
    			        for (m = 0; m < D; m++) {
    			        	u[m] = rng.nextGaussian();
    			        }
    			        for (m = 0; m < D; m++) {
    			        	for (p = 0; p < D; p++) {
    			        	    samples[j][m] += L[m][p]*u[p];	
    			        	}
    			        	samples[j][m] += mean[j][m];
    			        }
    			    } // for (j = lower; j < upper; j++)
    			    lower = upper;
    			} // if (N[i] != 0)
    		} // for (i = 0; i < K; i++)
    		return samples;
    	}
    	
    	public double[] _call_(double coords[][], double cov[][][], boolean as_log) {
    		// default cov = null, as_log = false 
    		// Evaluate model PDF at given coordinates.

            // see logL() for details.

            // Args:
            //    coords: numpy array (N, D) of test coordinates
            //    cov:  numpy array (N, D, D) covariance matrix of coords
            //    as_log (bool): return log(p) instead p

            // Returns:
            //     numpy array (N, 1) of PDF (or its log)
    		int i;
    		double result[] = logL(coords, cov);
    		if (!as_log) {
    			for (i = 0; i < result.length; i++) {
    				result[i] = Math.exp(result[i]);
    			}
    		}
    		return result;
    	}
    	
    	public double[] logL(double coords[][], double cov[][][]) {
    		// Log-likelihood of coords given all (i.e. the sum of) GMM components

            // Distributes computation over all threads on the machine.

            // If covar is None, this method returns
                // log(sum_k(p(x | k)))
            // of the data values x. If covar is set, the method returns
                // log(sum_k(p(y | k))),
            // where y = x + noise and noise ~ N(0, covar).

            // Args:
                // coords: numpy array (N, D) of test coordinates
                // cov:  numpy array (N, D, D) covariance matrix of coords

            // Returns:
                // numpy array (N, 1) log(L)
    		int i;
    		double log_p_y_k[][] = new double[K][coords.length];
    		for (i = 0; i < K; i++) {
    			log_p_y_k[i] = logL_k(i, coords, cov, false);
    		}
    		return logsum(log_p_y_k);
    	}
    	
    	public double[] logL_k(int k, double coords[][], double cov[][][], boolean chi2_only) {
    		// defaults cov = null, chi2-only = false
            // Log-likelihood of coords given only component k.

            // Args:
            //    k (int): component index
            //    coords: numpy array (N, D) of test coordinates
            //    cov:  numpy array (N, D, D) covariance matrix of coords
            //    chi2_only (bool): only compute deltaX^T Sigma_k^-1 deltaX

            // Returns:
            //    numpy array (1,) or (N, 1) log(L), depending on shape of data
    		
    		// compute p(x | k)
    		int i,j,m;
    		LinearEquations2 le2 = new LinearEquations2();
    		int N = coords.length;
    		double dx[][] = new double[N][D];
    		for (i = 0; i < N; i++) {
    			for (j = 0; j < D; j++) {
    				dx[i][j] = coords[i][j] - mean[k][j];
    			}
    		}
    		double T_k[][];
    		if (cov == null) {
                T_k = covar[k];
    		}
            else {
            	T_k = new double[D][D];
            	for (i = 0; i < D; i++) {
            		for (j = 0; j < D; j++) {
                    T_k[i][j] = covar[k][i][j] + cov[0][i][j];
            		}
            	}
            }
    		// chi2 = np.einsum('...i,...ij,...j', dx, np.linalg.inv(T_k), dx)
    		// Equivalent to diagonal of (dx * TK.inverse * dx.transpose())
    		int ipiv[] = new int[D];
    		int info[] = new int[1];
    		double work[];
    		int lwork;
    		double T_kinv[][] = new double[D][D];
    		for (i = 0; i < D; i++) {
    			for (j = 0; j < D; j++) {
    				T_kinv[i][j] = T_k[i][j];
    			}
    		}
    		le2.dgetrf(D,D,T_kinv,D,ipiv,info);
		    boolean rankDeficient = false;
		    if (info[0] < 0) {
		    	  System.err.println("In le2.dgetrf argument number " + 
		      (-info[0]) + " is illegal");
		    	  System.exit(-1);
		      }
		      if (info[0] > 0) {
		    	  System.err.println("In le2.dgetrf U["+(info[0]-1)+"]["+(info[0]-1)+"] is exactly 0");
		    	  rankDeficient = true;
		    	  System.exit(-1);
		      }
		      work = new double[1];
		      lwork = -1;
		      le2.dgetri(D,T_kinv,D,ipiv,work,lwork,info);
		      if (info[0] < 0) {
		    	  System.err.println("In le2.dgetri argument number " + 
		      (-info[0]) + " is illegal");
		    	  System.exit(-1);
		      }
		      lwork = (int)work[0];
		      work = new double[lwork];
		      le2.dgetri(D,T_kinv,D,ipiv,work,lwork,info);
		      if (info[0] < 0) {
		    	  System.err.println("In le2.dgetri argument number " + 
		      (-info[0]) + " is illegal");
		    	  System.exit(-1);
		      }
		      if (info[0] > 0) {
		    	  System.err.println("In le2.dgetri U["+(info[0]-1)+"]["+(info[0]-1)+"] is exactly 0");
		    	  rankDeficient = true;
		    	  System.exit(-1);
		      }
		      double dxT[][] = new double[N][D];
		      for (i = 0; i < N; i++) {
		    	  for (m = 0; m < D; m++) {
		    	      for (j = 0; j < D; j++) {
		    	          dxT[i][m] += dx[i][j] * T_kinv[j][m]; 
		    	      }
		    	  }
		      }
		      double dxTdx[][] = new double[N][N];
		      for (i = 0; i < N; i++) {
		    	  for (m = 0; m < N; m++) {
		    		  for (j = 0; j < D; j++) {
		    			  dxTdx[i][m] += dxT[i][j] * dx[m][j];
		    		  }
		    	  }
		      }
		      double chi2[] = new double[N];
		      for (i = 0; i < N; i++) {
		    	  chi2[i] = dxTdx[i][i];
		      }
		      
		      if (chi2_only) {
		    	  return chi2;
		      }
		      
		      // prevent tiny negative determinants to mess up
		      // (sign, logdet) = np.linalg.slogdet(T_k)
		      Matrix TkMat = new Matrix(T_k);
		      double det = TkMat.det();
		      double sign;
		      double logdet;
		      if (det < 0) {
		          sign = -1.0;
		          logdet = Math.log(-det);
		      }
		      else if (det == 0.0) {
		    	  sign = 0.0;
		    	  logdet = Double.NEGATIVE_INFINITY;
		      }
		      else {
		    	  sign = 1.0;
		    	  logdet = Math.log(det);
		      }
		      double log2piD2 = Math.log(2*Math.PI)*(0.5*D);
		      double result[] = new double[N];
		      for (i = 0; i < N; i++) {
		    	  result[i] = Math.log(amp[k]) - log2piD2 - sign*logdet/2.0 - chi2[i]/2.0;
		      }
		      return result;
    	}
    	
    } // class GMM
    
    public double[] logsum(double input[][]) {
    	int i, j;
    	double c;
    	double result[] = new double[input[0].length];
    	for (j = 0; j < input[0].length; j++) {
    		/*for (i = 0; i < input.length; i++) {
    			result[j] += input[i][j];
    		}
    		result[j] = Math.log(result[j]);*/
    		double minValue = Double.MAX_VALUE;
    		double maxValue = -Double.MAX_VALUE;
    		for (i = 0; i < input.length; i++) {
    			if (input[i][j] < minValue) {
    				minValue = input[i][j];
    			}
    			if (input[i][j] > maxValue) {
    				maxValue = input[i][j];
    			}
    		}
    		double underflow = Math.log(Double.MIN_NORMAL) - minValue;
    	    double overflow = Math.log(Double.MAX_VALUE) - maxValue - Math.log(input.length);
    	    if (underflow < overflow) {
    	    	c = underflow;
    	    }
    	    else {
    	    	c = overflow;
    	    }
    	    double sum = 0.0;
    	    for (i = 0; i < input.length; i++) {
    	        double ex = Math.exp(input[i][j] + c);
    	        sum += ex;
    	    }
    	    result[j] = Math.log(sum) - c;
    	}
    	return result;
    }
    
    class Background {
    	// Background object to be used in conjuction with GMM.

        // For a normalizable uniform distribution, a support footprint must be set.
        // It should be sufficiently large to explain all non-clusters samples.

        // Attributes:
            // amp (float): mixing amplitude default = 0.0;
            // footprint: numpy array, (2, D) of rectangular volume
            // adjust_amp (bool): whether amp will be adjusted as part of the fit
            // amp_max (float): maximum value of amp allowed if adjust_amp=True
    	
    	double footprint[][];
    	double amp = 0.0;
    	boolean adjust_amp;
    	double amp_max;
    	double amp_min;
    	
    	public Background(double footprint[][], double amp) {
    	    this.amp = amp;
    	    this.footprint = footprint;
    	    adjust_amp = true;
    	    amp_max = 1.0;
    	    amp_min = 0.0;
    	}
    	
    	public double p() {
    		// Probability of the background model.

            // Returns:
                // float, equal to 1/volume, where volume is given by footprint.
    		int i;
    		double volume = footprint[1][0] - footprint[0][0];
    		for (i = 1; i < D; i++) {
    			volume *= (footprint[1][i] - footprint[0][i]); 
    		}
    		return (1.0/volume);
    	}
    	
    	public double[][] draw(int size, Random rng) {
    		// Draw samples from uniform background.

            // Args:
            //    size (int): number of samples to draw
            //    rng: numpy.random.RandomState for deterministic draw

            // Returns:
            //    numpy array (size, D)
    		int i,j;
            double dx[] = new double[D];
            for (i = 0; i < D; i++) {
            	dx[i] = footprint[1][i] - footprint[0][i];
            }
            double result[][] = new double[size][D];
            double ranarr[][] = new double[size][D];
            for (i = 0; i < size; i++) {
            	for (j = 0; j < D; j++) {
            		ranarr[i][j] = rng.nextDouble();
            	}
            }
            for (i = 0; i < size; i++) {
            	for (j = 0; j < D; j++) {
            	result[i][j] = footprint[0][j]	+ dx[j]*ranarr[i][j];
            	}
            }
            return result;
    	}
    }
    
    /*getSelection(type="hole", rng=np.random):
        if type == "hole":
            cb = getHole
            ps = patches.Circle([6.5, 6.], radius=2, fc="none", ec='k', lw=1, ls='dashed')
        if type == "box":
            cb = getBox
            ps = patches.Rectangle([0,0], 10, 10, fc="none", ec='k', lw=1, ls='dashed')
        if type == "boxWithHole":
            cb = getBoxWithHole
            ps = [patches.Circle([6.5, 6.], radius=2, fc="none", ec='k', lw=1, ls='dashed'),
                patches.Rectangle([0,0], 10, 10, fc="none", ec='k', lw=1, ls='dashed')]
        if type == "cut":
            cb = getCut
            ps = lines.Line2D([6, 6],[-5, 15], ls='dotted', lw=1, color='k')
        if type == "all":
            cb = getAll
            ps = None
        return cb, ps*/
    
    public double[] getBox(double coords[][]) {
    	int i;
        double result[] = new double[coords.length];
        double box_limits[][] = new double[][] {{0.0,0.0},{10.0,10.0}};
        for (i = 0; i < coords.length; i++) {
        	if ((coords[i][0] > box_limits[0][0]) && (coords[i][0] < box_limits[1][0]) &&
        			(coords[i][1] > box_limits[0][1]) && (coords[i][1] < box_limits[1][1])) {
        		result[i] = 1.0;
        	}
        }
        return result;
    }
    
    public double[] getHole(double coords[][]) {
    	int i;
    	double result[] = new double[coords.length];
    	double x = 6.5;
    	double y = 6.0;
    	double r = 2.0;
    	double xdiff;
    	double ydiff;
    	for (i = 0; i < coords.length; i++) {
    	    xdiff = coords[i][0] - x;
    	    ydiff = coords[i][1] - y;
    	    if ((xdiff*xdiff + ydiff*ydiff) > r*r) {
    	    	result[i] = 1.0;
    	    }
    	}
    	return result;
    }
    
    public double[] getBoxWithHole(double coords[][]) {
    	int i;
        double result[] = new double[coords.length];
        double box_limits[][] = new double[][] {{0.0,0.0},{10.0,10.0}};
        double x = 6.5;
    	double y = 6.0;
    	double r = 2.0;
    	double xdiff;
    	double ydiff;
        for (i = 0; i < coords.length; i++) {
        	xdiff = coords[i][0] - x;
    	    ydiff = coords[i][1] - y;
        	if ((coords[i][0] > box_limits[0][0]) && (coords[i][0] < box_limits[1][0]) &&
        			(coords[i][1] > box_limits[0][1]) && (coords[i][1] < box_limits[1][1]) && 
        		((xdiff*xdiff + ydiff*ydiff) > r*r)) {
        		result[i] = 1.0;
        	}
        }
        return result;
    }
    
    public double[] getCut(double coords[][]) {
    	int i;
        double result[] = new double[coords.length];
        for (i = 0; i < coords.length; i++) {
        	if (coords[i][0] < 6) {
        		result[i] = 1.0;
        	}
        }
        return result;
    }
    
    public double[] getAll(double coords[][]) {
    	int i;
    	double result[] = new double[coords.length];
        for (i = 0; i < coords.length; i++) {
            result[i] = 1.0;
        }
        return result;
    }
    
    public double[] getHalf(double coords[][]) {
    	int i;
    	double result[] = new double[coords.length];
        for (i = 0; i < coords.length; i++) {
            result[i] = 0.5;
        }
        return result;
    }
    
    public void plotResults(double orig[][], double data[][], GMM gmm, /* patches ps, */
    		String description, double disp) {
        // orig is [N][D], D = 2
    	// data is [numSel][D]
    	int i,j;
    	float xInit[][] = new float[2][orig.length];
    	float yInit[][] = new float[2][orig.length];
    	for (i = 0; i < orig.length; i++) {
    		xInit[0][i] = (float)orig[i][0];
    		yInit[0][i] = (float)orig[i][1];
    	}
    	double data_[][];
    	boolean missing = false;
    	for (i = 0; i < data.length; i++) {
    		if ((Double.isNaN(data[i][0])) || (Double.isNaN(data[i][1]))) {
    		    missing = true;	
    		}
    	}
    	if (missing) {
    		data_ = new double[data.length][D];
    		for (i = 0; i < data.length; i++) {
    			for (j = 0; j < D; j++) {
	        		if (Double.isNaN(data[i][j])) {
	        		    data_[i][j] = -5; // put at limits of plotting range
	        		}
	        		else {
	        			data_[i][j] = data[i][j];
	        		}
    			}
        	}
    	} // if (missing)
    	else {
    		data_ = data;
    	}
    	for (i = 0; i < data_.length-1; i++) {
    		xInit[1][i] = (float)data_[i][0];
    		yInit[1][i] = (float)data_[i][1];
    	}
    	for (i = data_.length-1; i < orig.length; i++) {
    		xInit[1][i] = (float)data_[data_.length-1][0];
    		yInit[1][i] = (float)data_[data_.length-1][1];
    	}
    	ViewJFrameGraph vFrameGraph = new ViewJFrameGraph(xInit, yInit, description, "X coordinate", "Y coordinate");
    	ViewJComponentGraph vcGraph = vFrameGraph.getGraph();
	    vcGraph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_POINTS_ONLY);
	    
	    // prediction
	    int B = 100;
	    double step = (15.0 + 5.0)/(B - 1.0);
        double x[][] = new double[B][B];
        for (i = 0; i < B; i++) {
        	for (j = 0; j < B; j++) {
        		x[i][j] = -5 + j*step;
        	}
        }
        double y[][] = new double[B][B];
        for (i = 0; i < B; i++) {
        	for (j = 0; j < B; j++) {
        		y[i][j] = -5 + i*step;
        	}
        }
        
        double coords[][] = new double[B*B][2];
        for (i = 0; i < B; i++) {
        	for (j = 0; j < B; j++) {
        	    coords[i*B +j][0] = x[i][j];
        	    coords[i*B +j][1] = y[i][j];
        	}
        }
        
        // compute sum_k(p_k(x)) for all x
        // p = gmm(coords).reshape((B,B))
        double p1D[] = gmm._call_(coords,null,false);
        double p[][] = new double[B][B];
        for (i = 0; i < B; i++) {
        	for (j = 0; j < B; j++) {
        	    p[i][j] = p1D[i*B + j];	
        	}
        }
        // for better visibility use arcshinh stretch
        for (i = 0; i < B; i++) {
        	for (j = 0; j < B; j++) {
                p[i][j] = asinh(p[i][j]/1e-4);
        	}
        }
    }
    
    double asinh(double x)
    {
    return Math.log(x + Math.sqrt(x*x + 1.0));
    }
    
    public double fit(GMM gmm, double data[][], double covar[][][], /*R=None*/ String init_method, double w, double cutoff, String sel_callback,
    		 /*oversampling=10, covar_callback=None, */ Background background, 
    		 /*tol=1e-3, miniter=1, maxiter=1000, frozen=None, split_n_merge=False,*/ Random rng) {
    	        // Default covar = null;
    	        // Default init_method = "random"
    	        // Default w = 0.0
    	        // Default cutoff = None
    	        // default sel_callback = null
    	        // Backround = None 
    		    // Fit GMM to data.

    		    // If given, init_callback is called to set up the GMM components. Then, the
    		    // EM sequence is repeated until the mean log-likelihood converges within tol.

    		    // Args:
    		        // gmm: an instance if GMM
    		        // data: numpy array (N,D)
    		        // covar: sample noise covariance; numpy array (N,D,D) or (D,D) if i.i.d.
    		        // R: sample projection matrix; numpy array (N,D,D)
    		        // init_method (string): one of ['random', 'minmax', 'kmeans', 'none']
    		            // defines the method to initialize the GMM components
    		        // w (float): minimum covariance regularization
    		        // cutoff (float): size of component neighborhood [in 1D equivalent sigmas]
    		        // sel_callback: completeness callback to generate imputation samples.
    		        // oversampling (int): number of imputation samples per data sample.
    		            // only used if sel_callback is set.
    		            // value of 1 is fine but results are noisy. Set as high as feasible.
    		        // covar_callback: covariance callback for imputation samples.
    		            // needs to be present if sel_callback and covar are set.
    		        // background: an instance of Background if simultaneous fitting is desired
    		        // tol (float): tolerance for covergence of mean log-likelihood
    		        // maxiter (int): maximum number of iterations of EM
    		        // frozen (iterable or dict): index list of components that are not updated
    		        // split_n_merge (int): number of split & merge attempts
    		        // rng: numpy.random.RandomState for deterministic behavior
    		        
    		        // Notes:
    		            // If frozen is a simple list, it will be assumed that is applies to mean
    		            // and covariance of the specified components. It can also be a dictionary
    		            // with the keys "mean" and "covar" to specify them separately.
    		            // In either case, amplitudes will be updated to reflect any changes made.
    		            // If frozen["amp"] is set, it will use this list instead.

    		        // Returns:
    		            // mean log-likelihood (float), component neighborhoods (list of ints)

    		        // Throws:
    		            // RuntimeError for inconsistent argument combinations
    	int i,j,m;
    	int N = data.length;
    	// if there are data (features) missing, i.e. masked as np.nan, set them to zeros
        // and create/set covariance elements to very large value to reduce its weight
        // to effectively zero
    	double data_[][] = new double[N][data[0].length];
    	boolean anymissing = false;
    	boolean missing[][] = new boolean[N][data[0].length];
    	double covar_[][][] = null;
    	for (i = 0; i < N; i++) {
    		for (j = 0; j < data[0].length; j++) {
    			data_[i][j] = data[i][j];
    			if (Double.isNaN(data[i][j])) {
    				anymissing = true;
    				data_[i][j] = 0; // value does not matter as long as it's not nan
    				missing[i][j] = true;
    			}
    		}
    	}
    	if (anymissing) {
    	    if (covar == null) {
    	        covar = new double[1][gmm.D][gmm.D];
    	        // need to create covar_callback if imputation is requested
    	        if (sel_callback != null) {
    	        	// covar_callback = partial(covar_callback_default, default=np.zeros((gmm.D, gmm.D)))	
    	        } // if (sel_callback != null)
    	    } // if (covar == null) 
    	    if ((covar.length == 1) && (covar[0].length == gmm.D) && (covar[0][0].length == gmm.D)) {
    	        covar_ = new double[N][gmm.D][gmm.D];	
    	        for (i = 0; i < N; i++) {
    	        	for (j = 0; j < gmm.D; j++) {
    	        		for (m = 0; m < gmm.D; m++) {
    	        			covar_[i][j][m] = covar[0][j][m];
    	        		}
    	        	}
    	        }
    	    }
    	    else {
    	    	covar_ = new double[covar.length][covar[0].length][covar[0][0].length];
    	    	for (i = 0; i < N; i++) {
    	        	for (j = 0; j < gmm.D; j++) {
    	        		for (m = 0; m < gmm.D; m++) {
    	        			covar_[i][j][m] = covar[i][j][m];
    	        		}
    	        	}
    	        }
    	    }
    	    
    	    double large = 1.0E10;
    	    for (i = 0; i < N; i++) {
        		for (j = 0; j < gmm.D; j++) {
        		    if (missing[i][j]) {	
        		    	covar_[i][j][j] += large;
        		    }
        		}
    	    }
    	} // if (anymissing)
    	else {
    		if ((covar == null) || ((covar.length == 1) && (covar[0].length == gmm.D) && (covar[0][0].length == gmm.D))) {
    			covar_ = covar;
    		}
    		else {
    			covar_ = new double[covar.length][covar[0].length][covar[0][0].length];
    	    	for (i = 0; i < N; i++) {
    	        	for (j = 0; j < gmm.D; j++) {
    	        		for (m = 0; m < gmm.D; m++) {
    	        			covar_[i][j][m] = covar[i][j][m];
    	        		}
    	        	}
    	        }	
    		}
    	}
    	if ((!init_method.toLowerCase().equals("random")) && (!init_method.toLowerCase().equals("minmax")) &&
    			(!init_method.toLowerCase().equals("kmeans")) && (!init_method.toLowerCase().equals("none"))) {
    		System.err.println("Illegal value of " + init_method + " for init_method in fit");
    		System.exit(-1);
    	}
    	if (init_method.toLowerCase().equals("random")) {
            initFromDataAtRandom(gmm, data_, covar_, -1.0, null, rng);
    	}
    	return 0.0;
    }
    
    public double[][] covar_callback_default(double coords[][], double Default[][]) {
    	// default: Default = null;
    	int N = coords.length;
    	int D = coords[0].length;
        if ((Default.length != D) && (Default[0].length != D)) {
            System.err.println("covar_callback received improper default covariance length = " + 
            Default.length+","+Default[0].length);
            System.exit(-1);
        }
        // no need to copy since a single covariance matrix is sufficient
        // return np.tile(default, (N,1,1))
        return Default;
    }
    
    public void initFromDataAtRandom(GMM gmm, double data[][], double covar[][][], double s, int k[], Random rng) {
    	// default covar = null, s = -1, k = null
        // Initialization callback for component means to follow data on scales > s.

        // Component amplitudes are set to 1/gmm.K, covariances are set to
        // s**2*np.eye(D). For each mean, a data sample is selected at random, and a
        // multivariant Gaussian offset is added, whose variance is given by s**2.

        // If s is not given, it will be set such that the volume of all components
        // completely fills the space covered by data.

        // Args:
        //    gmm: A GMM to be initialized
        //    data: numpy array (N,D) to define the range of the component means
        //    covar: ignored in this callback
        //    s (double): if >= 0, sets component variances
        //    k (iterable): list of components to set, is None sets all components
        //    rng: numpy.random.RandomState for deterministic behavior

        // Returns:
        //    None
       
        int i,j,m;
        int k_len;
    	if (k == null) {
            k = new int[gmm.K];
            for (i = 0; i < gmm.K; i++) {
            	k[i] = i;
            }
            k_len = gmm.K;
        }
        else {
            k_len = k.length;
        }
        // initialize components around data points with uncertainty s
    	int refs[] = new int[k_len];
    	for (i = 0; i < k_len; i++) {
    		refs[i] = rng.nextInt(data.length);
    	}
        D = data[0].length;
		if (s < 0.0) {
			double min_pos[] = new double[D];
			double max_pos[] = new double[D];
			double vol_data = 1.0;
			for (j = 0; j < D; j++) {
				min_pos[D] = Double.MAX_VALUE;
				max_pos[D] = -Double.MAX_VALUE;
				for (i = 0; i < data.length; i++) {
					if (data[i][j] < min_pos[D]) {
						min_pos[D] = data[i][j];
					}
					if (data[i][j] > max_pos[D]) {
						max_pos[D] = data[i][j];
					}
				}
				vol_data *= (max_pos[D] - min_pos[D]);
			}
			double result[] = new double[1];
			Gamma gam = new Gamma((gmm.D*0.5 + 1),result);
			gam.run();
	        s = Math.pow((vol_data / gmm.K * result[0]),(1/gmm.D)) / Math.sqrt(Math.PI);
	        System.out.println("initializing spheres with s = " + s + " near data points");
		} // if (s < 0.0)

		
		for (i = 0; i < gmm.mean.length; i++) {
			for (j = 0; j < gmm.mean[0].length; j++) {
				gmm.mean[i][j] = 0.0;
			}
		}
		for (i = 0; i < k_len; i++) {
			for (j = 0; j < D; j++) {
			    gmm.mean[k[i]][j] = data[refs[i]][j] + s*rng.nextGaussian();	
			}
		}
		for (i = 0; i < gmm.covar.length; i++) {
			for (j = 0; j < gmm.covar[0].length; j++) {
				for (m = 0; m < gmm.covar[0][0].length; m++) {
					gmm.covar[i][j][m] = 0.0;
				}
			}
		}
		for (i = 0; i < k_len; i++) {
			for (j = 0; j < data[0].length; j++) {
				gmm.covar[k[i]][j][j] = s*s;
			}
		}
    }

}

    