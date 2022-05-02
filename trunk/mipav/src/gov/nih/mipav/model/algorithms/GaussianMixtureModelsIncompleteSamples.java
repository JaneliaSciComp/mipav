package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;
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
		int i,j,m;
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
	    }
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
    	
    } // class GMM
    
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
            	dx[i] = footprint[i][1] = footprint[i][0];
            }
            double result[][] = new double[size][D];
            double ranarr[][] = new double[size][D];
            for (i = 0; i < size; i++) {
            	for (j = 0; j < D; j++) {
            		ranarr[i][j] = rng.nextDouble();
            	}
            }
            return result;
    	}
    }
}

    