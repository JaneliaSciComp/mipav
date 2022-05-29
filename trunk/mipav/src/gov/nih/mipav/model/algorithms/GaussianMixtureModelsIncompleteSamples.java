package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.model.structures.jama.SVD;
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
	private double covar_callback_default_arg[][][] = null;
	
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
	    double omega[] = getSelection(sel_type,noisy);
	    
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
	    		}
	    		j++;
	    	}
	    }
	    // single covariance for all samples
	    double covar[][] = new double[D][D];
	    double covartest2[][][] = new double[1][D][D];
	    double covartest4[][][] = new double[1][D][D];
	    double covarstack[][][] = new double[1][D][D];
	    for (i = 0; i < D; i++) {
	    	covar[i][i] = disp*disp;
	    	covartest2[0][i][i] = disp*disp;
	    	covartest4[0][i][i] = disp*disp;
	    	covarstack[0][i][i] = disp*disp;
	    }
	    
	    // plot data vs true model
	    plotResults(orig, data, gmm, /*patch=ps,*/ "Truth", Double.NaN);
	    
	    // repeated runs: store results and logL
	    double l[] = new double[T];
	    GMM gmms[] = new GMM[T];
	    for (r = 0; r < T; r++) {
	    	gmms[r] = new GMM(K,D);
	    }
	    
	    // # 1) EM without imputation, ignoring errors
	    long start = System.currentTimeMillis();
	    rng = new Random(seed);
	    double cov[][][] = null;
	    double R[][][] = null;
	    String sel_callback = null;
	    String covar_callback = null;
	    double tol = 1.0E-3;
	    int miniter = 1;
	    int maxiter = 1000;
	    int frozen_amp[] = null;
	    int frozen_mean[] = null;
	    int frozen_covar[] = null;
	    int split_n_merge = 0;
	    for (r = 0; r < T; r++) {
	        if (bg != null) {
	            bg.amp = bg_amp;	
	        }
	        int U[][] = new int[gmm.K][]; 
	        l[r] = fit(U, gmms[r], data, cov, R, "random", w, cutoff, sel_callback, 
	        		oversampling, covar_callback, bg,
	        		tol, miniter, maxiter,
	        		frozen_amp, frozen_mean, frozen_covar, split_n_merge, rng);
	    } // for (r = 0; r < T; r++)
	    GMM avg = stack(gmms, l);
	    double test1time = (System.currentTimeMillis() - start)/1000.0;
	    System.out.println("Test 1 execution time = " + test1time + " seconds");
	    plotResults(orig, data, avg, /*ps,*/ "Standard EM", Double.NaN);
	    
	    // 2) EM without imputation, deconvolving via Extreme Deconvolution
	    start = System.currentTimeMillis();
	    rng = new Random(seed);
	    for (r = 0; r < T; r++) {
	        if (bg != null) {
	            bg.amp = bg_amp;	
	        }
	        int U[][] = new int[gmm.K][]; 
	        l[r] = fit(U, gmms[r], data, covartest2, R, "random", w, cutoff, sel_callback, 
	        		oversampling, covar_callback, bg,
	        		tol, miniter, maxiter,
	        		frozen_amp, frozen_mean, frozen_covar, split_n_merge, rng);
	    } // for (r = 0; r < T; r++)
	    avg = stack(gmms, l);
	    double test2time = (System.currentTimeMillis() - start)/1000.0;
	    System.out.println("Test 2 execution time = " + test2time + " seconds");
	    plotResults(orig, data, avg, /*ps,*/ "Standard EM & noise deconvolution", disp);
	    
	    // 3) pygmmis with imputation, igoring errors
	    // We need a good initial location to explore the
	    // volume that is spanned by the missing part of the data
	    // We therefore run a standard GMM without imputation first
	    start = System.currentTimeMillis();
	    rng = new Random(seed);
	    for (r = 0; r < T; r++) {
	        if (bg != null) {
	            bg.amp = bg_amp;	
	        }
	        int U[][] = new int[gmm.K][]; 
	        sel_callback = null;
	        fit(U, gmms[r], data, cov, R, "random", w, cutoff, sel_callback, 
	        		oversampling, covar_callback, bg,
	        		tol, miniter, maxiter,
	        		frozen_amp, frozen_mean, frozen_covar, split_n_merge, rng);
	        
	        sel_callback = "boxWithHole";
	        l[r] = fit(U, gmms[r], data, cov, R, "none", w, cutoff, sel_callback, 
	        		oversampling, covar_callback, bg,
	        		tol, miniter, maxiter,
	        		frozen_amp, frozen_mean, frozen_covar, split_n_merge, rng);
	    } // for (r = 0; r < T; r++)
	    avg = stack(gmms, l);
	    double test3time = (System.currentTimeMillis() - start)/1000.0;
	    System.out.println("Test 3 execution time = " + test3time + " seconds");
	    plotResults(orig, data, avg, /*ps,*/ "mathtt{GMMis}",Double.NaN);
	    
	    // 4) pygmmis with imputation, incorporating errors
	    //covar_cb = partial(pygmmis.covar_callback_default, default=np.eye(D)*disp**2)
	    start = System.currentTimeMillis();
	    rng = new Random(seed);
	    
	    for (r = 0; r < T; r++) {
	        if (bg != null) {
	            bg.amp = bg_amp;	
	        }
	        int U[][] = new int[gmm.K][]; 
	        sel_callback = null;
	        covar_callback = null;
	        fit(U, gmms[r], data, cov, R, "random", w, cutoff, sel_callback, 
	        		oversampling, covar_callback, bg,
	        		tol, miniter, maxiter,
	        		frozen_amp, frozen_mean, frozen_covar, split_n_merge, rng);
	        
	        sel_callback = "boxWithHole";
	        covar_callback = "covar_cb";
	        l[r] = fit(U, gmms[r], data, covartest4, R, "none", w, cutoff, sel_callback, 
	        		oversampling, covar_callback, bg,
	        		tol, miniter, maxiter,
	        		frozen_amp, frozen_mean, frozen_covar, split_n_merge, rng);
	    } // for (r = 0; r < T; r++)
	    avg = stack(gmms, l);
	    double test4time = (System.currentTimeMillis() - start)/1000.0;
	    System.out.println("Test 4 execution time = " + test4time + " seconds");
	    plotResults(orig, data, avg, /*ps,*/ "mathtt{GMMis} & noise deconvolution",disp);
	    
	    if (T > 1) {
	        plotDifferences(orig, data, gmms, avg, l/*, patch=ps*/);
	    }
	    // In test.py all code after here is commented out with triple quotes.
	    
	    // stacked estimator: needs to do init by hand to keep it fixed
	    //start = System.currentTimeMillis();
	    //rng = new Random(seed);
	    //for (r = 0; r < gmms.length; r++) {
	    	// init_cb not present
	        //init_cb(gmms[r], data, covar, rng);
	    //}
	    
	    //int L = 10;
	    //tol = 1.0E-5;
	    //String init_callback = null;
	    //sel_callback = "boxWithHole";
        //covar_callback = "covar_cb";
	    //GMM stacked = stack_fit(gmms, data, covarstack, init_callback, w, cutoff, 
	    		//sel_callback, covar_callback, bg, L, tol, rng);
	} // public void test()
	
	public GMM stack_fit(GMM gmms[], double data[][], double covar[][][], String init_callback, double w, double cutoff, 
			String sel_callback, String covar_callack, Background bg,
			int L, double tol, Random rng) {
		return null;
	}
	
	public GMM stack(GMM gmms[] , double weights[]) {
	    // build stacked model by combining all gmms and applying weights to amps
		int m;
		int i,j,p;
		double sum = 0.0;
		int K = gmms[0].K*gmms.length;
		int D = gmms[0].D;
	    GMM stacked = new GMM(K,D);
	    for (m = 0; m < gmms.length; m++) {
	    	for (i = 0; i < gmms[0].K; i++) {
	            stacked.amp[m*gmms[0].K + i] = weights[m]*gmms[m].amp[i];
	            sum += stacked.amp[m*gmms[0].K + i];
	            for (j = 0; j < D; j++) {
	                stacked.mean[m*gmms[0].K + i][j] = gmms[m].mean[i][j];
	                for (p = 0; p < D; p++) {
	                    stacked.covar[m*gmms[0].K + i][j][p] = gmms[m].covar[i][j][p];
	                }
	            }
	    	} // for (i = 0; i < gmms[0].K; i++)
	    } // for (m = 0; m < gmms.length; m++)
	    for (i = 0; i < gmms.length*gmms[0].K; i++) {
	        stacked.amp[i] /= sum;
	    }
	    return stacked;
	}
	
	public double[] getSelection(String sel_type, double noisy[][]) {
		double omega[];
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
	    else {
	    	omega = null;
	    }
		return omega;
	}
	
    
	
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
    			        	samples[j][m] += mean[i][m];
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
    
    public double logsum(double input[]) {
    	int i;
    	double c;
    	double result;
    	double minValue = Double.MAX_VALUE;
		double maxValue = -Double.MAX_VALUE;
		for (i = 0; i < input.length; i++) {
			if (input[i] < minValue) {
				minValue = input[i];
			}
			if (input[i] > maxValue) {
				maxValue = input[i];
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
	        double ex = Math.exp(input[i] + c);
	        sum += ex;
	    }
	    result = Math.log(sum) - c;
	    return result;
    }
    
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
    
    public void plotDifferences(double orig[][], double data[][], GMM gmms[], GMM avg, double l[] /*, patch=None*/) {
    	int i,j,r;
    	double diff;
    	float xInit[] = new float[data.length];
    	float yInit[] = new float[data.length];
    	for (i = 0; i < data.length; i++) {
    		xInit[i] = (float)data[i][0];
    		yInit[i] = (float)data[i][1];
    	}	
    	
    	ViewJFrameGraph vFrameGraph = new ViewJFrameGraph(xInit, yInit, "data", "X coordinate", "Y coordinate");
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
        //pw = avg(coords).reshape((B,B))
        double pwD[] = avg._call_(coords,null,false);
        double pw[][] = new double[B][B];
        for (i = 0; i < B; i++) {
        	for (j = 0; j < B; j++) {
        	    pw[i][j] = pwD[i*B + j];	
        	}
        }
        
        // use each run and compute weighted std
        double p[][][] = new double[gmms.length][B][B];
        for (r = 0; r < gmms.length; r++) {
        	// compute sum_k(p_k(x)) for all x
            //p[r,:,:] = gmms[r](coords).reshape((B,B))
        	double pD[] = gmms[r]._call_(coords,null,false);
        	for (i = 0; i < B; i++) {
            	for (j = 0; j < B; j++) {
            	    p[r][i][j] = pD[i*B + j];	
            	}
            }
        }
        
        double psum[][] = new double[B][B];
        for (r = 0; r < gmms.length; r++) {
        	for (i = 0; i < B; i++) {
        		for (j = 0; j < B; j++) {
        		    diff = p[r][i][j] - pw[i][j];
        		    psum[i][j] += diff*diff + l[r];
        		}
        	}
        }
        double V1 = 0;
        double V2 = 0;
        for (i = 0; i < r; i++) {
        	V1 += l[r];
        	V2 += l[r]*l[r];
        }
        for (i = 0; i < B; i++) {
        	for (j = 0; j < B; j++) {
        		psum[i][j] /= (V1 - V2/V1);
        	}
        }
        
        for (i = 0; i < B; i++) {
        	for (j = 0; j < B; j++) {
                psum[i][j] = asinh(Math.sqrt(psum[i][j])/1e-4);
        	}
        }
        
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
    
    public double fit(int U[][], GMM gmm, double data[][], double covar[][][],double R[][][], String init_method, double w, double cutoff, String sel_callback,
    		 int oversampling, String covar_callback, Background background, 
    		 double tol, int miniter, int maxiter,
    		 int frozen_amp[], int frozen_mean[], int frozen_covar[], int split_n_merge, Random rng) {
    	        // Default covar = null;
    	        // Default R = null;
    	        // Default init_method = "random"
    	        // Default w = 0.0
    	        // Default cutoff = Double.NaN
    	        // Default sel_callback = null
    	        // Default oversampling = 10
    	        // Default Backround = None 
    	        // Default tol = 1.0E-3
    	        // Default miniter = 1
    	        // Default maxiter = 1000
    	        // Default frozen_amp, frozen_mean, frozen_covar = null
    	        // Default split_n_merge = 0
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
    	int ND = data.length;
    	// if there are data (features) missing, i.e. masked as np.nan, set them to zeros
        // and create/set covariance elements to very large value to reduce its weight
        // to effectively zero
    	double data_[][] = new double[ND][data[0].length];
    	boolean anymissing = false;
    	boolean missing[][] = new boolean[ND][data[0].length];
    	double covar_[][][] = null;
    	boolean nondiag[][] = null;
    	String mess = null;
    	boolean changeable_amp[] = null;
    	boolean changeable_mean[] = null;
    	boolean changeable_covar[] = null;
    	for (i = 0; i < ND; i++) {
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
    	        	covar_callback_default_arg = new double[1][gmm.D][gmm.D];	
    	        } // if (sel_callback != null)
    	    } // if (covar == null) 
    	    if ((covar.length == 1) && (covar[0].length == gmm.D) && (covar[0][0].length == gmm.D)) {
    	        covar_ = new double[ND][gmm.D][gmm.D];	
    	        for (i = 0; i < ND; i++) {
    	        	for (j = 0; j < gmm.D; j++) {
    	        		for (m = 0; m < gmm.D; m++) {
    	        			covar_[i][j][m] = covar[0][j][m];
    	        		}
    	        	}
    	        }
    	    }
    	    else {
    	    	covar_ = new double[covar.length][covar[0].length][covar[0][0].length];
    	    	for (i = 0; i < ND; i++) {
    	        	for (j = 0; j < gmm.D; j++) {
    	        		for (m = 0; m < gmm.D; m++) {
    	        			covar_[i][j][m] = covar[i][j][m];
    	        		}
    	        	}
    	        }
    	    }
    	    
    	    double large = 1.0E10;
    	    for (i = 0; i < ND; i++) {
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
    	    	for (i = 0; i < ND; i++) {
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
    	if (init_method.toLowerCase().equals("minmax")) {
            initFromDataMinMax(gmm, data_, covar_, -1.0, null, rng);
    	}
    	else if (init_method.toLowerCase().equals("kmeans")) {
            initFromKMeans(gmm, data_, rng);
    	}
    	
    	// test if callbacks are consistent
        if ((sel_callback != null) && (covar != null) && (covar_callback == null)) {
            System.err.println("covar is set, but covar_callback is None: imputation samples inconsistent");
            System.exit(-1);
        }
        
        // containers
        // precautions for cases when some points are treated as outliers
        // and not considered as belonging to any component
        double log_S[] = new double[ND];          // S = sum_k p(x|k)
        double log_p[][] = new double[gmm.K][];        // P = p(x|k) for x in U[k]
        double T_inv[][][][] = new double[gmm.K][][][];      // T = covar(x) + gmm.covar[k]
        // U = {x close to k}
        double p_bg[][] = null;
        if (background != null) {
        	for (i = 0; i < gmm.amp.length; i++) {
        	    gmm.amp[i] *= 1 - background.amp;          // GMM amp + BG amp = 1
        	}
        	p_bg = new double[][] {{Double.NaN}};                   // p_bg = p(x|BG), no log because values are larger	
        	if (covar != null) {
        		// check if covar is diagonal and issue warning if not
                mess = "background model will only consider diagonal elements of covar";	
                boolean havenondiag = false;
                if ((covar[0].length == gmm.D) && (covar[0][0].length == gmm.D)) {
                	for (i = 0; i < covar.length; i++) {
                    	for (j = 0; j < gmm.D; j++) {
                    		for (m = 0; m < gmm.D; m++) {
	                    		if (j != m) {
	                    		    if (covar[i][j][m] != 0.0) {
	                    		    	havenondiag = true;
	                    		    }
	                    		}
                    		}
                    	}
                	}
                }
                if (havenondiag) {
                	System.out.println("Warning! " + mess);
                }
        	} // if (covar != null)
        } // if (background != null)
        
        changeable_amp = new boolean[gmm.K];
        for (i = 0; i < gmm.K; i++) {
        	changeable_amp[i] = true;
        }
        changeable_mean = new boolean[gmm.K];
        for (i = 0; i < gmm.K; i++) {
        	changeable_mean[i] = true;
        }
        changeable_covar = new boolean[gmm.K];
        for (i = 0; i < gmm.K; i++) {
        	changeable_covar[i] = true;
        }
        if (frozen_amp != null) {
            for (i = 0; i < frozen_amp.length; i++) {
                if ((frozen_amp[i] >= 0) && (frozen_amp[i] < gmm.K)) {
                	changeable_amp[i] = false;
                }
            }
        }
        if (frozen_mean != null) {
            for (i = 0; i < frozen_mean.length; i++) {
                if ((frozen_mean[i] >= 0) && (frozen_mean[i] < gmm.K)) {
                	changeable_mean[i] = false;
                }
            }
        }
        if (frozen_covar != null) {
            for (i = 0; i < frozen_covar.length; i++) {
                if ((frozen_covar[i] >= 0) && (frozen_covar[i] < gmm.K)) {
                	changeable_covar[i] = false;
                }
            }
        }
        double log_L[] = new double[1];
        int N[] = new int[1];
        double N2[] = new double[1];
        String prefix = "";
        _EM(log_L, N, N2, gmm, log_p, U, T_inv, log_S, data_, covar_, R,
        		 sel_callback, oversampling, covar_callback, background, p_bg, w,
        		 //pool=pool, chunksize=chunksize, 
        		 cutoff, miniter, maxiter, tol, prefix,
        		 changeable_amp, changeable_mean, changeable_covar,
        		  rng);
        
        // should we try to improve by split'n'merge of components?
	    // if so, keep backup copy
	    GMM gmm_ = null;
	    if (((frozen_amp != null) || (frozen_mean != null) || (frozen_covar != null)) && (split_n_merge > 0)) {
	        System.out.println("Warning! Forgoing split'n'merge because some components are frozen");
	    }
	    else {
	    	while ((split_n_merge > 0) && (gmm.K >= 3)) {
	    	
	    		if (gmm_ == null) {
	                gmm_ = new GMM(gmm.K, gmm.D);
	    		}
	    		
	    		for (i = 0; i < gmm.K; i++) {
	    			gmm_.amp[i] = gmm.amp[i];
	    			for (j = 0; j < gmm.D; j++) {
	    				gmm_.mean[i][j] = gmm.mean[i][j];
	    				for (m = 0; m < gmm.D; m++) {
	    				    gmm_.covar[i][j][m] = gmm.covar[i][j][m];	
	    				}
	    			}
	    		}
	    		
	    		int U_[][] = new int[gmm.K][];
	    		for (i = 0; i < gmm.K; i++) {
	    			U_[i] = new int[U[i].length];
	    			for (j = 0; j < U[i].length; j++) {
	    				U_[i][j] = U[i][j];
	    			}
	    		}

	    		boolean cleanup[] = new boolean[1];
	    		int changing[] = new int[3];
	    		_findSNMComponents(changing, cleanup, gmm, U, log_p, log_S, N[0]+N2[0]
	    				/*, pool=pool, chunksize=chunksize*/);
	    		Preferences.debug("merging " + changing[0] + " and " + changing[1] + " , splitting " + changing[2] + "\n",
	    				Preferences.DEBUG_ALGORITHM);

	            // modify components
	            _update_snm(gmm, changing, U, N[0]+N2[0], cleanup[0]);
	            
	            // run partial EM on changeable components
	            // NOTE: for a partial run, we'd only need the change to Log_S from the
	            // changeable components. However, the neighborhoods can change from _update_snm
	            // or because they move, so that operation is ill-defined.
	            // Thus, we'll always run a full E-step, which is pretty cheap for
	            // converged neighborhood.
	            // The M-step could in principle be run on the changeable components only,
	            // but there seem to be side effects in what I've tried.
	            // Similar to the E-step, the imputation step needs to be run on all
	            // components, otherwise the contribution of the changeable ones to the mixture
	            // would be over-estimated.
	            // Effectively, partial runs are as expensive as full runs.
	            
	            for (i = 0; i < gmm.K; i++) {
	            	changeable_amp[i] = false;
	            	changeable_mean[i] = false;
	            	changeable_covar[i] = false;
	            }
	            for (i = 0; i < 3; i++) {
	            	changeable_amp[changing[i]] = true;
	            	changeable_mean[changing[i]] = true;
	            	changeable_covar[changing[i]] = true;
	            }
	            double log_L_[] = new double[1];
	            int N_[] = new int[1];
	            double N2_[] = new double[1];
	            prefix = "SNM_P";
	            _EM(log_L_, N_, N2_, gmm, log_p, U, T_inv, log_S, data_, covar_, R, 
	            		sel_callback, oversampling, covar_callback, background, p_bg, w,
	           		 //pool=pool, chunksize=chunksize, 
	           		 cutoff, miniter, maxiter, tol, prefix,
	           		 changeable_amp, changeable_mean, changeable_covar,
	           		 rng);
	            
	            for (i = 0; i < gmm.K; i++) {
	            	changeable_amp[i] = true;
	            	changeable_mean[i] = true;
	            	changeable_covar[i] = true;
	            }
	            
	            prefix = "SNM_F";
	            _EM(log_L_, N_, N2_, gmm, log_p, U, T_inv, log_S, data_, covar_, R, 
	            		sel_callback, oversampling, covar_callback, background, p_bg, w,
	           		 //pool=pool, chunksize=chunksize, 
	           		 cutoff, miniter, maxiter, tol, prefix,
	           		 changeable_amp, changeable_mean, changeable_covar,
	           		 rng);
	            
	            if (log_L[0] >= log_L_[0]) {
	                // revert to backup
            		for (i = 0; i < gmm.K; i++) {
    	    			gmm.amp[i] = gmm_.amp[i];
    	    			for (j = 0; j < gmm.D; j++) {
    	    				gmm.mean[i][j] = gmm_.mean[i][j];
    	    				for (m = 0; m < gmm.D; m++) {
    	    				    gmm.covar[i][j][m] = gmm_.covar[i][j][m];	
    	    				}
    	    			}
    	    		}
            		for (i = 0; i < gmm.K; i++) {
    	    			U[i] = new int[U_[i].length];
    	    			for (j = 0; j < U_[i].length; j++) {
    	    				U[i][j] = U_[i][j];
    	    			}	
            		}
	                Preferences.debug("Split'n'merge likelihood decreased: reverting to previous model\n",
	                		Preferences.DEBUG_ALGORITHM);
	                break;
	            } // if (log_L[0] >= log_L_[0])
	            
	            log_L[0] = log_L_[0];
	            split_n_merge -= 1;
	            		
	    	} // while ((split_n_merge > 0) && (gmm.K >= 3))
	    } // else
	    for (i = 0; i < data_.length; i++) {
	    	data_[i] = null;
	    }
	    data_ = null;
	    if (covar_ != null) {
		    for (i = 0; i < covar_.length; i++) {
		    	for (j = 0; j < covar_[0].length; j++) {
		    		covar_[i][j] = null;
		    	}
		    }
		    for (i = 0; i < covar_.length; i++) {
		    	covar_[i] = null;
		    }
		    covar_ = null;
	    } // if (covar_ != null)
	    log_S = null;
    	return log_L[0];
    }
    
    public void _update_snm( GMM gmm, int changeable[], int U[][], double N, boolean cleanup) {
    	int i,j,k;
    	double A[] = new double[gmm.K];
    	double gsum[] = new double[gmm.D];
    	double Asum;
        // reconstruct A from gmm.amp
    	for (i = 0; i < gmm.K; i++) {
            A[i] = gmm.amp[i] * N;
    	}
    	
    	// update parameters and U
        // merge 0 and 1, store in 0, Bovy eq. 39
    	gmm.amp[changeable[0]] = gmm.amp[changeable[0]] + gmm.amp[changeable[1]] + gmm.amp[changeable[2]];
        if (!cleanup) {
        	Asum = A[changeable[0]] + A[changeable[1]] + A[changeable[2]];
        	for (i = 0; i < gmm.D; i++) {
        	    for (j = 0; j < 3; j++) {
        	    	gmm.mean[changeable[0]][i] += (gmm.mean[changeable[j]][i] * A[changeable[j]]);
        	    	gmm.mean[changeable[0]][i] /= Asum;
        	    }
        	}
        	for (i = 0; i < gmm.D; i++) {
        		for (j = 0; j < gmm.D; j++) {
	        	    for (k = 0; k < 3; k++) {
	        	    	gmm.covar[changeable[0]][i][j] += (gmm.covar[changeable[k]][i][j] * A[changeable[k]]);
	        	    	gmm.covar[changeable[0]][i][j] /= Asum;
	        	    }
        		}
        	}
        	Vector<Integer>Utemp = new Vector<Integer>();
        	for (i = 0; i < U[changeable[0]].length; i++) {
        		if (!Utemp.contains(U[changeable[0]][i])) {
        			Utemp.add(U[changeable[0]][i]);
        		}
        	}
        	for (i = 0; i < U[changeable[1]].length; i++) {
        		if (!Utemp.contains(U[changeable[1]][i])) {
        			Utemp.add(U[changeable[1]][i]);
        		}
        	}
        	U[changeable[0]] = new int[Utemp.size()];
        	for (i = 0; i < Utemp.size(); i++) {
        		U[changeable[0]][i] = Utemp.get(i);
        	}
        	Arrays.sort(U[changeable[0]]);
        	Utemp.clear();
        } // if (!cleanup)
        else {
            // if we're cleaning up the weakest components:
            // merging does not lead to valid component parameters as the original
            // ones can be anywhere. Simply adopt second one.
        	for (i = 0; i < gmm.D; i++) {
        		gmm.mean[changeable[0]][i] = gmm.mean[changeable[1]][i];
        		for (j = 0; j < gmm.D; j++) {
        			gmm.covar[changeable[0]][i][j] = gmm.covar[changeable[1]][i][j];
        		}
        	}
            U[changeable[0]] = new int[U[changeable[1]].length];
            for (i = 0; i < U[changeable[1]].length; i++) {
                U[changeable[0]][i] = U[changeable[1]][i];	
            }
        } // else
        
        // split 2, store in 1 and 2
        // following SVD method in Zhang 2003, with alpha=1/2, u = 1/4
        gmm.amp[changeable[1]] = gmm.amp[changeable[2]] = gmm.amp[changeable[2]] / 2;
        // TODO: replace with linalg.eigvalsh, but eigenvalues are not always ordered
        //_, radius2, rotation = np.linalg.svd(gmm.covar[changeable[2]])
        SVD svd = new SVD();
        double radius2[] = new double[gmm.D];
	    int ldu = 1;
	    double UU[][] = new double[1][1];
	    int ldvt = gmm.D;
	    double rotation[][] = new double[gmm.D][gmm.D];
	    int info[] = new int[1];
    	double cov[][] = new double[gmm.D][gmm.D];
    	for (j = 0; j < gmm.D; j++) {
    		for (k = 0; k < gmm.D; k++) {
    			cov[j][k] = gmm.covar[changeable[2]][j][k];
    		}
    	}
    	 double work[] = new double[1];
    	 int lwork = -1;
         svd.dgesvd('N','A',gmm.D, gmm.D, cov, gmm.D, radius2, UU, ldu, rotation, ldvt, work, lwork, info);
         lwork = (int)work[0];
         work = new double[lwork];
         svd.dgesvd('N','A',gmm.D, gmm.D, cov, gmm.D, radius2, UU, ldu, rotation, ldvt, work, lwork, info);
         if (info[0] < 0) {
           	System.err.println("In svd.dgesvd argument " + (-info[0]) + " had an illegal value");
           	Preferences.debug("In svd.dgesvd argument " + (-info[0]) + " had an illegal value\n", Preferences.DEBUG_ALGORITHM);
           	System.exit(-1);
        }
         if (info[0] > 0) {
           	System.err.println("In svd.dgesvd dbdsqr did not converge.");
           	Preferences.debug("In svd.dgesvd dbdsqr did not converge.\n",
           			Preferences.DEBUG_ALGORITHM);
           	System.exit(-1);
        }
        double dl;
        for (i = 0; i < gmm.D; i++) {
        	dl = Math.sqrt(radius2[0]) * rotation[0][i]/4.0;
        	gmm.mean[changeable[1]][i] = gmm.mean[changeable[2]][i] - dl;
        	gmm.mean[changeable[2]][i] = gmm.mean[changeable[2]][i] + dl;
        }
        double det = (new Matrix(gmm.covar[changeable[2]])).det();
        double var = Math.pow(det,1.0/gmm.D);
        for (i = 0; i < gmm.D; i++) {
        	for (j = 0; j < gmm.D; j++) {
        		if (i == j) {
        			gmm.covar[changeable[1]][i][j] = var;
        		}
        		else {
        			gmm.covar[changeable[1]][i][j] = 0.0;
        		}
        	}
        }
        // Now 1 and 2 have the same U
        U[changeable[1]] = new int[U[changeable[2]].length];
        for (i = 0; i < U[changeable[2]].length; i++) {
            U[changeable[1]][i] = U[changeable[2]][i];	
        }
        
        return;
    }
    
    public void _findSNMComponents(int changing[], boolean cleanup[], GMM gmm, int U[][], double log_p[][], double log_S[], double N
    		/*, pool=None, chunksize=1*/) {
    	int i,j,k;
    	boolean presorted = true;
    	Vector<Integer> i_kVec = new Vector<Integer>();
    	Vector<Integer> i_jVec = new Vector<Integer>();
    	int merge_jk[] = new int[2];
    	double maxVal;
    	double JS[];
    	double A[];
    	// find those components that are most similar
        double JM[][] = new double[gmm.K][gmm.K];
        // compute log_q (posterior for k given i), but use normalized probabilities
        // to allow for merging of empty components
        double log_q[][] = new double[gmm.K][];	
        double EV[][];
        double cov[][];
        SVD svd = new SVD();
        int split_l3[] = new int[3];
        for (i = 0; i < gmm.K; i++) {
        	log_q[i] = new double[log_p[i].length];
        	for (j = 0; j < log_p[i].length; j++) {
        		log_q[i][j] = log_p[i][j] - log_S[U[i][j]] - Math.log(gmm.amp[i]);
        	}
        }
        for (k = 0; k < gmm.K; k++) {
        	// don't need diagonal (can merge), and JM is symmetric
            for (j = k+1; j < gmm.K; j++) {
            	// get index list for intersection of U of k and l
                // FIXME: match1d fails if either U is empty
                // SOLUTION: merge empty U, split another
            	
            	i_kVec.clear();
            	i_jVec.clear();
                match1d(i_kVec, i_jVec, U[k], U[j], presorted);
                for (i = 0; i < i_kVec.size(); i++) {
                	JM[k][j] += Math.exp(log_q[k][i_kVec.get(i)]) * Math.exp(log_q[j][i_jVec.get(i)]);
                }	
            } // for (j = k+1; j < gmm.K; j++)
        } // for (k = 0; k < gmm.K; k++)
        // merge_jk = np.unravel_index(JM.argmax(), JM.shape)
        // Index of first occurrence of maximum value
        maxVal = -Double.MAX_VALUE;
        for (i = 0; i < gmm.K; i++) {
        	for (j = 0; j < gmm.K; j++) {
        		if (JM[i][j] > maxVal) {
        			maxVal = JM[i][j];
        			merge_jk[0] = i;
        			merge_jk[1] = j;
        		}
        	}
        }
        // if all Us are disjunct, JM is blank and merge_jk = [0,0]
	    // merge two smallest components and clean up from the bottom
	    cleanup[0] = false;
	    if ((merge_jk[0] == 0) && (merge_jk[1] == 0)) {
	        Preferences.debug("Neighborhoods disjunct. merging components " + merge_jk[0] + " and " + merge_jk[1] + "\n",
	        		Preferences.DEBUG_ALGORITHM);
	        if (gmm.amp[1] >= gmm.amp[0]) {
	            merge_jk[0] = 0;
	            merge_jk[1] = 1;
	        }
	        else {
	        	merge_jk[0] = 1;
	        	merge_jk[1] = 0;
	        }
	        cleanup[0] = true;
	    } // if ((merge_jk[0] == 0) && (merge_jk[1] == 0))
	    
	    // split the one whose p(x|k) deviate most from current Gaussian
	    // ask for the three worst components to avoid split being in merge_jk
	    
	    JS = new double[gmm.K];
	    A = new double[gmm.K];
	    for (i = 0; i < gmm.K; i++) {
	        A[i] = gmm.amp[i] * N;
	    }
	    for (k = 0; k < gmm.K; k++) {
	       JS[k] = _JS(k, gmm, log_p, log_S, U, A);	
	    }
	    
	    // get largest Eigenvalue, weighed by amplitude
	    // Large EV implies extended object, which often is caused by coverving
	    // multiple clusters. This happes also for almost empty components, which
	    // should rather be merged than split, hence amplitude weights.
	    // TODO: replace with linalg.eigvalsh, but eigenvalues are not always ordered
	    EV = new double[gmm.K][gmm.D];
	    int ldu = 1;
	    double UU[][] = new double[1][1];
	    int ldvt = 1;
	    double VT[][] = new double[1][1];
	    int info[] = new int[1];
	    for (i = 0; i < gmm.K; i++) {
	    	cov = new double[gmm.D][gmm.D];
	    	for (j = 0; j < gmm.D; j++) {
	    		for (k = 0; k < gmm.D; k++) {
	    			cov[j][k] = gmm.covar[i][j][k];
	    		}
	    	}
	    	 double work[] = new double[1];
	    	 int lwork = -1;
	         svd.dgesvd('N','N',gmm.D, gmm.D, cov, gmm.D, EV[i], UU, ldu, VT, ldvt, work, lwork, info);
	         lwork = (int)work[0];
	         work = new double[lwork];
	         svd.dgesvd('N','N',gmm.D, gmm.D, cov, gmm.D, EV[i], UU, ldu, VT, ldvt, work, lwork, info);
	         if (info[0] < 0) {
               	System.err.println("In svd.dgesvd argument " + (-info[0]) + " had an illegal value");
               	Preferences.debug("In svd.dgesvd argument " + (-info[0]) + " had an illegal value\n", Preferences.DEBUG_ALGORITHM);
               	System.exit(-1);
            }
	         if (info[0] > 0) {
               	System.err.println("In svd.dgesvd dbdsqr did not converge.");
               	Preferences.debug("In svd.dgesvd dbdsqr did not converge.\n",
               			Preferences.DEBUG_ALGORITHM);
               	System.exit(-1);
            }
	    }
	    for (i = 0; i < gmm.K; i++) {
	    	JS[i] = EV[i][0] * gmm.amp[i];
	    }
	    double minVal = Double.MAX_VALUE;
	    maxVal = -Double.MAX_VALUE;
	    for (i = gmm.K-3; i <= gmm.K-1; i++) {
	        if (JS[i] < minVal) {
	            minVal = JS[i];
	            split_l3[2] = i;
	        }
	        if (JS[i] > maxVal) {
	        	maxVal = JS[i];
	        	split_l3[0] = i;
	        }
	    }
	    for (i = gmm.K-3; i <= gmm.K-1; i++) {
	    	if ((i != split_l3[0]) && (i != split_l3[2])) {
	    		split_l3[1] = i;
	    	}
	    }
	    
	    // check that the three indices are unique
	    changing[0] = merge_jk[0];
	    changing[1] = merge_jk[1];
	    changing[2] = split_l3[0];
	    if ((split_l3[0] == merge_jk[0]) || (split_l3[0] == merge_jk[1])) {
	        if ((split_l3[1] != merge_jk[0]) && (split_l3[1] != merge_jk[1])) {
	            changing[2] = split_l3[1];
	        }
	        else {
	            changing[2] = split_l3[2];
	        }
	    }
	    return;
	    
    }
    
    public double _JS(int k, GMM gmm, double log_p[][], double log_S[], int U[][], double A[]) {
    	int i;
    	// compute Kullback-Leiber divergence
    	double log_q_k[] = new double[log_p[k].length];
    	for (i = 0; i < log_p[k].length; i++) {
    		log_q_k[i] = log_p[k][i] - log_S[U[k][i]];
    	}
        double prod = 0.0;
        for (i = 0; i < log_q_k.length; i++) {
        	prod += Math.exp(log_q_k[i]) * (log_q_k[i] - Math.log(A[k]) - log_p[k][i] + Math.log(gmm.amp[k]));
        }
        return (prod/A[k]);
    }
    
    // Blantant copy from Erin Sheldon's esutil
    // https://github.com/esheldon/esutil/blob/master/esutil/numpy_util.py
    public void match1d(Vector<Integer>sub1Vec, Vector<Integer>sub2Vec, int arr1Input[], int arr2[], boolean presorted) {
    	// default presorted = false
    	// NAME:
        //     match
        // CALLING SEQUENCE:
        //    ind1,ind2 = match(arr1, arr2, presorted=False)
        // PURPOSE:
        //    Match two numpy arrays.  Return the indices of the matches or empty
        //    arrays if no matches are found.  This means arr1[ind1] == arr2[ind2] is
        //    true for all corresponding pairs.  arr1 must contain only unique
        //    inputs, but arr2 may be non-unique.
        //    If you know arr1 is sorted, set presorted=True and it will run
        //    even faster
        // METHOD:
        //    uses searchsorted with some sugar.  Much faster than old version
        //    based on IDL code.
        // REVISION HISTORY:
        //    Created 2015, Eli Rykoff, SLAC.
    	int i,j;
    	boolean unique;
    	boolean found;
    	int arr1[] = null;
    	if ((arr1Input.length == 0) || (arr2.length == 0)) {
            System.err.println("Error in match1d: arr1Input and arr2 must each be non-zero length");
            System.exit(-1);
    	}
    	
    	// make sure that arr1 has unique values...
    	unique = true;
    	for (i = 0; i < arr1Input.length && unique; i++) {
    		for (j = i+1; j < arr1Input.length && unique; j++) {
    			if (arr1Input[i] == arr1Input[j]) {
    				unique = false;
    			}
    		}
    	}
        if (!unique) {
            System.err.println("Error in match1d: arr1Input must be unique");
            System.exit(-1);
        }
        
        // sort arr1 if not presorted
        if (!presorted) {
        	arr1 = new int[arr1Input.length];
        	for (i = 0; i < arr1.length; i++) {
        		arr1[i] = arr1Input[i];
        	}
            Arrays.sort(arr1);
        }
        else {
        	arr1 = arr1Input;
        }
        
        // search the sorted array
        // sub1=np.searchsorted(arr1,arr2,sorter=st1)
        int sub1[] = new int[arr2.length];
        for (i = 0; i < arr2.length; i++) {
        	if (arr2[i] < arr1[0]) {
        		sub1[i] = 0;
        	}
        	else if (arr2[i] > arr1[arr1.length-1]) {
        		sub1[i] = arr1.length;
        	}
        	else {
        		found = false;
        		for (j = 1; j <= arr1.length-1 && (!found); j++) {
        		    if ((arr2[i] > arr1[j-1] && (arr2[i] <= arr1[j]))) {
        		    	found = true;
        		    	sub1[i] = j;
        		    }
        		}
        	}
        } // for (i = 0; i < arr2.length; i++)
        
        // check for out-of-bounds at the high end if necessary
        int arr2max = Integer.MIN_VALUE;
        for (i = 0; i < arr2.length; i++) {
        	if (arr2[i] > arr2max) {
        		arr2max = arr2[i];
        	}
        }
        int arr1max = arr1[arr1.length-1];
        if (arr2max > arr1max) {
        	for (i = 0; i < sub1.length; i++) {
        		if (sub1[i] == arr1.length) {
        			sub1[i] = arr1.length-1;
        		}
        	}
        }
        
        for (i = 0; i < sub1.length; i++) {
            for (j = 0; j < arr2.length; j++) {
            	if (arr1[sub1[i]] == arr2[j]) {
            		sub2Vec.add(j);
            	}
            }
        }
        for (i = 0; i < sub2Vec.size(); i++) {
        	sub1Vec.add(sub1[sub2Vec.get(i)]);
        }
        return;

    }
    
    // run EM sequence
    public void _EM(double log_L[], int N[], double N2[], GMM gmm, double log_p[][], int U[][], double T_inv[][][][],
    		double log_S[], double data[][], double covar[][][], double R[][][], String sel_callback, int oversampling,
    		String covar_callback, Background background, double p_bg[][], double w,
    	    //pool=pool, chunksize=chunksize, 
    		double cutoff, int miniter, int maxiter, double tol, String prefix,
    		boolean changeable_amp[], boolean changeable_mean[], boolean changeable_covar[],
    		Random rng) {
        // Defaults: covar=null, R=null, sel_callback=null, oversampling=10, covar_callback=null,
    	// background=null, p_bg=null, w=0, pool=null, chunksize=1, cutoff=Double.NaN, miniter=1, maxiter=1000, tol=1e-3, prefix="",
    	// changeable_amp = null, changeable_mean = null, changeable_covar = null
    	int i,j,k,m,p;
    	double cutoff_nd;
    	double shift_cutoff;
    	double omega[];
    	boolean anyomegazero;
    	int it;
    	String header;
    	GMM gmm_;
    	double N0;
    	double bg_amp_ = 0.0;
    	double log_L_[] = new double[1];
    	double N2_[] = new double[1];
    	double N0_[] = new double[1];
    	LinearEquations2 le2 = new LinearEquations2();
    	double shift2[];
    	int moved[];
    	int movedsize;
    	String status_mess = null;
    	boolean found;
    	
    	// compute effective cutoff for chi2 in D dimensions
    	if (!Double.isNaN(cutoff)) {
    		// note: subsequently the cutoff parameter, e.g. in _E(), refers to this:
    	    // chi2 < cutoff,
    	    // while in fit() it means e.g. "cut at 3 sigma".
    	    // These differing conventions need to be documented well.
    		cutoff_nd = chi2_cutoff(gmm.D, cutoff);

    		// store chi2 cutoff for component shifts, use 0.5 sigma
    		shift_cutoff = chi2_cutoff(gmm.D, Math.min(0.1, cutoff/2.0));
    	} // if (!Double.isNaN(cutoff))
    	else {
            cutoff_nd = Double.NaN;
            shift_cutoff = chi2_cutoff(gmm.D, 0.1);
    	}
    	
    	if (sel_callback != null) {
    	    omega = getSelection(sel_callback, data);
    	    anyomegazero = false;
    	    for (i = 0; i < omega.length; i++) {
    	    	if (omega[i] == 0.0) {
    	    		anyomegazero = true;
    	    	}
    	    }
    	    if (anyomegazero) {
    	    	System.out.println("Warning! Selection probability Omega = 0 for an observed sample.");
                System.out.println("Selection callback likely incorrect! Bad things will happen!");
    	    }
    	} // if (sel_callback != null)
    	else {
    		omega = null;
    	}
    	
    	it = 0;
	    header = "ITER\tSAMPLES";
	    if (sel_callback != null) {
	        header += "\tIMPUTED\tORIG";
	    }
	    if (background != null) {
	        header += "\tBG_AMP";
	    }
	    header += "\tLOG_L\tSTABLE";
	    //logger.info(header)
	    
	    // save backup
	    gmm_ = new GMM(gmm.K, gmm.D);
	    for (i = 0; i < gmm.K; i++) {
	    	gmm_.amp[i] = gmm.amp[i];
	    	for (j = 0; j < gmm.D; j++) {
	    		gmm_.mean[i][j] = gmm.mean[i][j];
	    		for (m = 0; m < gmm.D; m++) {
	    			gmm_.covar[i][j][m] = gmm.covar[i][j][m];
	    		}
	    	}
	    }
	    N0 = data.length; // size of original (unobscured) data set (signal and background)
	    N2[0] = 0;        // size of imputed signal sample
	    if (background != null) {
	        bg_amp_ = background.amp;
	    }
	    
	    while (it < maxiter) { // limit loop in case of slow convergence
	    	_EMstep(log_L_, N, N2_, N0_,gmm, log_p, U, T_inv, log_S, N0, data, covar, R, sel_callback,
	    			 omega, oversampling, covar_callback, background, p_bg, w,
	    			 //pool=pool, chunksize=chunksize, 
	    			 cutoff_nd, tol, changeable_amp, changeable_mean, changeable_covar, it, rng);
	    	
	    	// check if component has moved by more than sigma/2
	        // shift2 = np.einsum('...i,...ij,...j', gmm.mean - gmm_.mean, np.linalg.inv(gmm_.covar), gmm.mean - gmm_.mean)
	    	double dm[][] = new double[gmm.K][gmm.D];
	    	for (i = 0; i < gmm.K; i++) {
	    		for (j = 0; j < gmm.D; j++) {
	    			dm[i][j] = gmm.mean[i][j] - gmm_.mean[i][j];
	    		}
	    	}
	    	double invcovar[][][] = new double[gmm_.covar.length][gmm.D][gmm.D];
	    	for (i = 0; i < gmm_.covar.length; i++) {
	    	    for (j = 0; j < gmm.D; j++) {
	    	    	for (m = 0; m < gmm.D; m++) {
	    	    		invcovar[i][j][m] = gmm_.covar[i][j][m];
	    	    	}
	    	    }
	    	}
	    	int ipiv[] = new int[gmm.D];
	    	int info[] = new int[1];
	    	for (i = 0; i < invcovar.length; i++) {
        	    le2.dgetrf(gmm.D,gmm.D,invcovar[i],gmm.D,ipiv,info);
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
    		      double work[] = new double[1];
    		      int lwork = -1;
    		      le2.dgetri(gmm.D,invcovar[i],gmm.D,ipiv,work,lwork,info);
    		      if (info[0] < 0) {
    		    	  System.err.println("In le2.dgetri argument number " + 
    		      (-info[0]) + " is illegal");
    		    	  System.exit(-1);
    		      }
    		      lwork = (int)work[0];
    		      work = new double[lwork];
    		      le2.dgetri(gmm.D,invcovar[i],gmm.D,ipiv,work,lwork,info);
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
    	    } // for (i = 0; i < invcovar.length; i++)
	    	double dmC[][][] = new double[gmm.K][gmm.K][gmm.D];
        	for (i = 0; i < gmm.K; i++) {
        		for (j = 0; j < gmm.K; j++) {
        			for (m = 0; m < gmm.D; m++) {
        				for (p = 0; p < gmm.D; p++) {
        					dmC[i][j][m] += dm[i][p] * invcovar[j][p][m];
        				}
        			}
        		}
        	}
        	double dmCdm[][][] = new double[gmm.K][gmm.K][gmm.K];
        	for (i = 0; i < gmm.K; i++) {
        		for (j = 0; j < gmm.K; j++) {
        			for (m = 0; m < gmm.K; m++) {
        				for (p = 0; p < gmm.D; p++) {
        					dmCdm[i][j][m] += dmC[i][j][p] * dm[m][p];
        				}
        			}
        		}
        	}
        	shift2 = new double[gmm.K];
        	for (i = 0; i < gmm.K; i++) {
        		shift2[i] = dmCdm[i][i][i];
        	}
	    	movedsize = 0;
	    	for (i = 0; i < gmm.K; i++) {
	    	    if (shift2[i] > shift_cutoff) {
	    	        movedsize++;	
	    	    }
	    	}
	    	if (movedsize == 0) {
	    		moved = null;
	    	}
	    	else {
	    		moved = new int[movedsize];
	    		for (i = 0, j = 0; i < gmm.K; i++) {
		    	    if (shift2[i] > shift_cutoff) {
		    	        moved[j++] = i;	
		    	    }
		    	}
	    	}
	    	status_mess = prefix + " it = " + String.valueOf(it) + "  N[0] = " + String.valueOf(N[0]) + "\n";
	    	if (sel_callback != null) {
	    		status_mess += "N2_[0] = " + String.valueOf(N2_[0]) + "  N0_[0] = " + String.valueOf(N0_[0]) + "\n";
	    	}
	    	if (background != null) {
	    		status_mess += "bg_amp_ = " + String.valueOf(bg_amp_) + "\n";
	    	}
	    	status_mess += "log_L_[0] = " + String.valueOf(log_L_[0]) + "  gmm.K - moved.size = " + (gmm.K - movedsize) + "\n";
	    	Preferences.debug(status_mess, Preferences.DEBUG_ALGORITHM);
	    	
	    	// convergence tests
	        if (it > miniter) {
	            if (sel_callback == null) {
	                if ((Math.abs(log_L_[0] - log_L[0]) < tol * Math.abs(log_L[0])) && (movedsize == 0)) {
	                    log_L[0] = log_L_[0];
	                    Preferences.debug("Likelihood converged within relative tolerance " + tol + "  Stopping here.\n",
	                    		Preferences.DEBUG_ALGORITHM);
	                    break;
	                }
	            } // if (sel_callback == null)
	            else {
	                if ((Math.abs(N0_[0] - N0) < tol * N0) && (Math.abs(N2_[0] - N2[0]) < tol * N2[0]) && (movedsize == 0)) {
	                    log_L[0] = log_L_[0];
	                    Preferences.debug("Imputation sample size converged within relative tolerance " + tol + " Stopping here.\n",
	                    		Preferences.DEBUG_ALGORITHM);
	                    break;
	                }
	            } // else 
	        } // if (it > miniter)
	        
	        // force update to U for all moved components
	        if ((!Double.isNaN(cutoff)) && (moved != null)) {
	        	int Utemp[][] = new int[U.length][U[0].length];
	        	for (i = 0; i < U.length; i++) {
	        		for (j = 0; j < U[0].length; j++) {
	        		    Utemp[i][j] = U[i][j];
	        		}
	        	}
	        	for (i = 0; i < U.length; i++) {
	        		int uisize = U[i].length;
	        		for (j = 0; j < U[i].length; j++) {
	        			found = false;
	        			for (k = 0; k < movedsize  && (!found); k++) {
	        				if (U[i][j] == moved[k]) {
	        					found = true;
	        					uisize--;
	        				}
	        			}
	        		}
	        		U[i] = new int[uisize];
	        		for (j = 0, p = 0; j < Utemp[i].length; j++) {
	        		    found = false;
	        		    for (k = 0; k < movedsize && (!found); k++) {
	        		    	if (U[i][j] == moved[k]) {
	        		    		found = true;
	        		    	}
	        		    }
	        		    if (!found) {
	        		        U[i][p++] = Utemp[i][j];	
	        		    }
	        		}
	        	}
	        } // if ((!Double.isNaN(cutoff)) && (moved != null)) 
	        
	        if (movedsize > 0) {
	            Preferences.debug("Resetting neighborhoods of moving components:\n", Preferences.DEBUG_ALGORITHM);
	            for (i = 0; i < movedsize-1; i++) {
	            	Preferences.debug(String.valueOf(moved[i]) + " ",Preferences.DEBUG_ALGORITHM);
	            }
	            Preferences.debug(String.valueOf(moved[movedsize-1]) + "\n", Preferences.DEBUG_ALGORITHM);
	        }
	        
	        // update all important _ quantities for convergence test(s)
	        log_L[0] = log_L_[0];
	        N0 = N0_[0];
	        N2[0] = N2_[0];

	        // backup to see if components move or if next step gets worse
	        // note: not gmm = gmm_ !
	        for (i = 0; i < gmm.K; i++ ) {
	        	gmm_.amp[i] = gmm.amp[i];
	        	for (j = 0; j < gmm.D; j++) {
	        		gmm_.mean[i][j] = gmm.mean[i][j];
	        		for (k = 0; k < gmm.D; k++) {
	        			gmm_.covar[i][j][k] = gmm.covar[i][j][k];
	        		}
	        	}
	        }
	        if (background != null) {
	            bg_amp_ = background.amp;
	        }

	        it += 1;
	    } // while (it < maxiter)
	    
	    return;
    }
    
    // run one EM step
    public void _EMstep(double log_L[], int N[], double N2[], double N0update[],
    		GMM gmm, double log_p[][], int U[][], double T_inv[][][][], double log_S[], double N0, 
    		double data[][], double covar[][][], double R[][][], String sel_callback,
	    			 double omega[], int oversampling, String covar_callback, Background background, 
	    			 double p_bg[][], double w,
	    			 //pool=pool, chunksize=chunksize, 
	    			 double cutoff, double tol, boolean changeable_amp[], 
	    			 boolean changeable_mean[], boolean changeable_covar[], int it, Random rng) {
    	// Defaults: covar=null, R=null, sel_callback=null, omega=null, oversampling=10, covar_callback=null,
    	// background=null, p_bg=null, w=0, pool=null, chunksize=1, cutoff=Double.NaN, tol=1e-3, 
    	// changeable_amp = null, changeable_mean = null, changeable_covar = null, it=0
    	// NOTE: T_inv (in fact (T_ik)^-1 for all samples i and components k)
        // is very large and is unfortunately duplicated in the parallelized _Mstep.
        // If memory is too limited, one can recompute T_inv in _Msums() instead.
    	double data2[][] = null;
    	int i,j,p;
        log_L[0] = _Estep(gmm, log_p, U, T_inv, log_S, data, covar, R, omega, background, p_bg, 
        		          // pool=pool, chunksize=chunksize, 
        		          cutoff, it, rng);
        // save the M sums from the observed data
        double A[] = new double[gmm.K]; // sum for amplitudes
        double M[][] = new double[gmm.K][gmm.D]; // sum for means
        double C[][][] = new double[gmm.K][gmm.D][gmm.D]; // sum for covariances
        double B[] = new double[1];
        N[0] = data.length;
        _Mstep(A,M,C,B,gmm, U, log_p, T_inv, log_S, data, covar, R, p_bg 
        		/*,pool=pool,chunksize=chunksize*/);
        double A2[] = new double[gmm.K]; // sum for amplitudes
        double M2[][] = new double[gmm.K][gmm.D]; // sum for means
        double C2[][][] = new double[gmm.K][gmm.D][gmm.D]; // sum for covariances
        double B2[] = new double[1];
        N2[0] = 0;
        
        // here the magic happens: imputation from the current model
        if (sel_callback != null) {
        	// if there are projections / missing data, we don't know how to
            // generate those for the imputation samples
            // NOTE: in principle, if there are only missing data, i.e. R is 1_D,
            // we could ignore missingness for data2 because we'll do an analytic
            // marginalization. This doesn't work if R is a non-trivial matrix.
            if (R != null) {
                System.err.println("Not implemente error.  R is not null: imputation samples likely inconsistent");
                System.exit(-1);
            } // if (R != null)
            
            // create fake data with same mechanism as the original data,
            // but invert selection to get the missing part
            Vector<double[][]>data2Vec = new Vector<double[][]>();
            Vector<double[][][]>covar2Vec = new Vector<double[][][]>();
            Vector<double[]>omega2Vec = new Vector<double[]>();
            boolean invert_sel = true;
            int orig_size = (int)Math.round(N0*oversampling);
            draw(data2Vec, covar2Vec, N0update, omega2Vec, gmm, data.length*oversampling, sel_callback,
            		invert_sel,orig_size,  covar_callback,
            		background, rng);
            data2 = data2Vec.get(0);
            double covar2[][][] = covar2Vec.get(0);
            double omega2[] = omega2Vec.get(0);
            //data2 = createShared(data2)
            //if not(covar2 is None or covar2.shape == (gmm.D, gmm.D)):
               // covar2 = createShared(covar2)
            
            N0update[0] = N0/oversampling;
        	int U2[][] = new int[gmm.K][];
        	
        	if (data2.length > 0) {
        		double log_S2[] = new double[data2.length];
        		double log_p2[][] = new double[gmm.K][];
        		double T2_inv[][][][] = new double[gmm.K][][][];
        		double R2[][][] = null;
        		double p_bg2[][] = null;
        		if (background != null) {
        			p_bg2 = new double[][] {{Double.NaN}};
        		}
        		
        		double log_L2 = _Estep(gmm, log_p2, U2, T2_inv, log_S2, data2, covar2, R2, null, background, p_bg2, 
        				/*pool=pool, chunksize=chunksize, */
        				cutoff, it, rng);
        		N2[0] = data2.length;
        		_Mstep(A2,M2,C2,B2,gmm, U2, log_p2, T2_inv, log_S2, data2, covar2, R2, p_bg2 
                		/*,pool=pool,chunksize=chunksize*/);
        		
        		// normalize for oversampling
        		for (i = 0; i < gmm.K; i++) {
        			A2[i] /= oversampling;
        			for (j = 0; j < gmm.D; j++) {
        				M2[i][j] /= oversampling;
        				for (p = 0; p < gmm.D; p++) {
        					C2[i][j][p] /= oversampling;
        				}
        			}
        		}
                B2[0] /= oversampling;
                N2[0] = N2[0]/oversampling; // need floating point precision in update
                
                // check if components have outside selection
                boolean sel_outside  = false;
                for (i = 0; i < gmm.K && (!sel_outside); i++) {
                	if (A2[i] > tol * A[i]) {
                		sel_outside = true;
                	}
                }
                if (sel_outside) {
                	Preferences.debug("component inside fractions A/(A+A2)\n", Preferences.DEBUG_ALGORITHM);
                	for (i = 0; i < gmm.K; i++) {
                		Preferences.debug("Fraction " + i + " = " + (A[i]/(A[i] + A2[i]))+"\n", Preferences.DEBUG_ALGORITHM);
                	}
                }
        	} // if (data2.length > 0)
        	// correct the observed likelihood for the overall normalization constant of
            // of the data process with selection:
            // logL(x | gmm) = sum_k p_k(x) / Z(gmm), with Z(gmm) = int dx sum_k p_k(x) = 1
            // becomes
            // logL(x | gmm) = sum_k Omega(x) p_k(x) / Z'(gmm),
            // with Z'(gmm) = int dx Omega(x) sum_k p_k(x), which we can gt by MC integration
        	double omegaSum = 0.0;
        	for (i = 0; i < omega.length; i++) {
        		omegaSum += omega[i];
        	}
        	double omega2Sum = 0.0;
        	for (i = 0; i < omega2.length; i++) {
        		omega2Sum += omega2[i];
        	}
            log_L[0] -= N[0] * Math.log((omegaSum + omega2Sum / oversampling) / (N[0] + N2[0]));
        } // if (sel_callback != null)
        
        _update(gmm, A, M, C, N, B, A2, M2, C2, N2, B2, w, changeable_amp, changeable_mean, changeable_covar, background);
        return;
    }
    
    // update component with the moment matrices.
    // If changeable is set, update only those components and renormalize the amplitudes
    public void _update(GMM gmm, double A[], double M[][], double C[][][], int N[], double B[],
    		double A2[], double M2[][], double C2[][][], double N2[], double B2[], 
    		double w, boolean changeable_amp[], boolean changeable_mean[], 
    		boolean changeable_covar[], Background background) {
    	int i,j,p;
    	boolean allampchangeable = true;
    	double total;
    	double Asum;
    	double w_eff;
    	
    	// recompute background amplitude
        if ((background != null) && background.adjust_amp) {
            background.amp = Math.max(Math.min((B[0] + B2[0]) / (N[0] + N2[0]), background.amp_max), background.amp_min);
        }
        
        // amp update:
        // for partial update: need to update amp for any component that is changeable
        for (i = 0; i < changeable_amp.length && allampchangeable; i++) {
        	if (!changeable_amp[i]) {
        		allampchangeable = false;
        	}
        }
        if (allampchangeable) { // it's a slice(None), not a bool array
        	for (i = 0; i < gmm.amp.length; i++) {
        		gmm.amp[i] = (A[i] + A2[i])/(N[0] + N2[0]);
        	}
        }
        else {
        	// Bovy eq. 31, with correction for bg.amp if needed
            if (background == null) {
                total = 1.0;
            }
            else {
                total = 1 - background.amp;
            }
            Asum = 0.0;
            double gsum = 0.0;
            for (i = 0; i < A.length; i++) {
            	if (changeable_amp[i]) {
            		Asum += (A[i] + A2[i]);
            	}
            	else {
            		gsum += gmm.amp[i];
            	}
            }
            for (i = 0; i < gmm.amp.length; i++) {
            	if (changeable_amp[i]) {
            		gmm.amp[i] = (A[i] + A2[i])/Asum * (total - gsum);
            	}
            }
        }
        
        // mean updateL
        for (i = 0; i < M.length; i++) {
        	if (changeable_mean[i]) {
        		Asum = A[i] + A2[i];
	        	for (j = 0; j < M[0].length; j++) {
	        		gmm.mean[i][j] = (M[i][j] + M2[i][j])/Asum;
	        	}
        	}
        }
       
        // covar updateL
        // minimum covariance term?
        if (w > 0) {
            // we assume w to be a lower bound of the isotropic dispersion,
            // C_k = w^2 I + ...
            // then eq. 38 in Bovy et al. only ~works for N = 0 because of the
            // prefactor 1 / (q_j + 1) = 1 / (A + 1) in our terminology
            // On average, q_j = N/K, so we'll adopt that to correct.
            w_eff = w*w * ((N[0]+N2[0])/gmm.K + 1);
            for (i = 0; i < C.length; i++) {
            	if (changeable_covar[i]) {
            	    Asum = A[i] + A2[i] + 1.0;
            	    for (j = 0; j < gmm.D; j++) {
            	    	for (p = 0; p < gmm.D; p++) {
            	    		if (j == p) {
            	    			gmm.covar[i][j][p] = (C[i][j][p] + C2[i][j][p] + w_eff)/Asum;
            	    		}
            	    		else {
            	    			gmm.covar[i][j][p] = (C[i][j][p] + C2[i][j][p])/Asum;
            	    		}
            	    	}
            	    }
            	}
            }
        }     		
        else {
        	for (i = 0; i < C.length; i++) {
            	if (changeable_covar[i]) {
            	    Asum = A[i] + A2[i];
            	    for (j = 0; j < gmm.D; j++) {
            	    	for (p = 0; p < gmm.D; p++) {
            	    	    gmm.covar[i][j][p] = (C[i][j][p] + C2[i][j][p])/Asum;
            	    	}
            	    }
            	}
            }
        }
        
        return;
    }
    
    public void draw(Vector<double[][]> dataVec, Vector<double[][][]> covarVec, double updated_orig_size[], Vector<double[]> omegaVec,
    		GMM gmm, int obs_size, String sel_callback, boolean invert_sel, int orig_size, String covar_callback, Background background,
    		Random rng) {
        // Draw from the GMM (and the Background) with noise and selection.

        // Draws orig_size samples from the GMM and the Background, if set; calls
        // covar_callback if set and applies resulting covariances; the calls
        // sel_callback on the (noisy) samples and returns those matching ones.

        // If the number is resulting samples is inconsistent with obs_size, i.e.
        // outside of the 68 percent confidence limit of a Poisson draw, it will
        // update its estimate for the original sample size orig_size.
        // An estimate can be provided with orig_size, otherwise it will use obs_size.

        // Note:
        //    If sel_callback is set, the number of returned samples is not
        //    necessarily given by obs_size.

        //Args:
        //    gmm: an instance if GMM
        //    obs_size (int): number of observed samples
        //    sel_callback: completeness callback to generate imputation samples.
        //    invert_sel (bool): whether to invert the result of sel_callback
        //    orig_size (int): an estimate of the original size of the sample.
        //    background: an instance of Background
        //    covar_callback: covariance callback for imputation samples.
        //    rng: numpy.random.RandomState for deterministic behavior

        // Returns:
        //    sample: nunmpy array (N_orig, D)
        //    covar_sample: numpy array (N_orig, D, D) or None of covar_callback=None
        //    N_orig (int): updated estimate of orig_size if sel_callback is set

        // Throws:
        //    RuntimeError for inconsistent argument combinations
    	double omega[] = null;
    	double data[][] = null;
    	double covar[][][] = null;
    	int i,j,p;
    	Vector<Integer> selVec = null;
    	double alpha;
    	int obs_size_;
    	int sel[] = null;
    	
    	if (orig_size <= 0) {
            updated_orig_size[0] = obs_size;
    	}
    	else {
    		updated_orig_size[0] = orig_size;
    	}
    	
    	// draw from model (with background) and add noise.
        // TODO: may want to decide whether to add noise before selection or after
        // Here we do noise, then selection, but this is not fundamental
        _drawGMM_BG(dataVec, covarVec, gmm, updated_orig_size[0], covar_callback, background, rng);
        
        // apply selection
        if (sel_callback != null) {
        	data = dataVec.get(0);
            omega = getSelection(sel_callback,data);
            selVec = new Vector<Integer>();
            for (i = 0; i < data.length; i++) {
            	if (rng.nextDouble() < omega[i]) {
            		selVec.add(i);
            	}
            }

            // check if predicted observed size is consistent with observed data
            // 68% confidence interval for Poisson variate: observed size
            alpha = 0.32;
            // lower = 0.5*scipy.stats.chi2.ppf(alpha/2, 2*obs_size)
            // Percent point function Inverse of cdf-percentiles
            //cutoff_nd = scipy.stats.chi2.ppf(confidence_1d, D)
            // Regularized incomplete gamma function Q(D/2,1/(2*confidence_1d)) =
            // 1 - regularized incomplete gamma function P(D/2,1/(2*confidence_1d))
            double lowerIncompleteGamma[] = new double[1];
            double upperIncompleteGamma[] = new double[1];
            double regularizedGammaP[] = new double[1];
            Gamma gam = new Gamma(obs_size, 1.0/alpha, lowerIncompleteGamma,
            		upperIncompleteGamma, regularizedGammaP);
            gam.run();
            double lower = 0.5*(1.0 - regularizedGammaP[0]);
            //upper = 0.5*scipy.stats.chi2.ppf(1 - alpha/2, 2*obs_size + 2)
            gam = new Gamma(obs_size + 1, 1.0/(2.0 - alpha), lowerIncompleteGamma,
            		upperIncompleteGamma, regularizedGammaP);
            gam.run();
            double upper = 0.5*(1.0 - regularizedGammaP[0]);
            obs_size_ = selVec.size();
            while ((obs_size_ > upper) || (obs_size_ < lower)) {
                updated_orig_size[0] = (int)(updated_orig_size[0] / obs_size_ * obs_size);
                dataVec.clear();
                covarVec.clear();
                _drawGMM_BG(dataVec, covarVec, gmm, updated_orig_size[0], covar_callback, background, rng);
                data = dataVec.get(0);
                omega = getSelection(sel_callback,data);
                selVec.clear();
                for (i = 0; i < data.length; i++) {
                	if (rng.nextDouble() < omega[i]) {
                		selVec.add(i);
                	}
                }
                obs_size_ = selVec.size();
            } // while ((obs_size_ > upper) || (obs_size_ < lower))

            sel = new int[obs_size_];
            for (i = 0; i < obs_size_; i++) {
            	sel[i] = selVec.get(i);
            }
            if (invert_sel) {
            	sel = new int[data.length - obs_size_];
            	for (i = 0, j = 0; i < data.length; i++) {
            	    if (!selVec.contains(i)) {
            	        sel[j++] = i;	
            	    }
            	}
            }
            
            double tempData[][] = new double[data.length][data[0].length];
            for (i = 0; i < data.length; i++) {
            	for (j = 0; j < data[0].length; j++) {
            		tempData[i][j] = data[i][j];
            	}
            }
            data = new double[sel.length][tempData[0].length];
            for (i = 0; i < sel.length; i++) {
            	for (j = 0; j < data[0].length; j++) {
            		data[i][j] = tempData[sel[i]][j];
            	}
            }
            
            double tempOmega[] = new double[omega.length];
            for (i = 0; i < omega.length; i++) {
            	tempOmega[i] = omega[i];
            }
            omega = new double[sel.length];
            for (i = 0; i < sel.length; i++) {
            	omega[i] = tempOmega[sel[i]];
            }
            omegaVec.add(omega);
            covar = covarVec.get(0);
            if ((covar_callback != null) && ((covar.length != 1) || (covar[0].length != gmm.D) || (covar[0][0].length != gmm.D))) {
            	double tempCovar[][][] = new double[covar.length][covar[0].length][covar[0][0].length];
            	for (i = 0; i < covar.length; i++) {
            		for (j = 0; j < covar[0].length; j++) {
            			for (p = 0; p < covar[0][0].length; p++) {
            				tempCovar[i][j][p] = covar[i][j][p];
            			}
            		}
            	}
            	covar = new double[sel.length][tempCovar[0].length][tempCovar[0][0].length];
            	for (i = 0; i < sel.length; i++) {
            		for (j = 0; j < covar[0].length; j++) {
            			for (p = 0; p < covar[0][0].length; p++) {
            				covar[i][j][p] = tempCovar[sel[i]][j][p];
            			}
            		}
            	}
            }
        } // if (sel_callback != null)

        dataVec.add(data);
        covarVec.add(covar);
        return;
    }
    
    // draw from the model (+ background) and apply appropriate covariances
    public void _drawGMM_BG(Vector<double[][]> data2Vec, Vector<double[][][]> covar2Vec,
    		GMM gmm , double size, String covar_callback, Background background, Random rng) {
    	int i,j,p;
    	// draw sample from model, or from background+model
    	double data2[][] = null;
    	double covar2[][][] = null;
        if (background == null) {
            data2 = gmm.draw((int)Math.round(size), rng);
        }
        else {
            // model is GMM + Background
            int bg_size = (int)(background.amp * size);
            double data2a[][] = gmm.draw((int)Math.round(size-bg_size), rng);
            double data2b[][] = background.draw(bg_size, rng);
            data2 = new double[data2a.length+data2b.length][data2a[0].length];
            for (i = 0; i < data2a.length; i++) {
            	for (j = 0; j < data2[0].length; j++) {
            		data2[i][j] = data2a[i][j];
            	}
            }
            for (i = 0; i < data2b.length; i++) {
            	for (j = 0; j < data2b[0].length; j++) {
            		data2[i+data2a.length][j] = data2b[i][j];
            	}
            }	
        }
        
        // add noise
        // NOTE: When background is set, adding noise is problematic if
        // scattering them out is more likely than in.
        // This can be avoided when the background footprint is large compared to
        // selection region
        if (covar_callback != null) {
            covar2 = covar_callback_default(data2);
            covar2Vec.add(covar2);
            double L[][] = new double[gmm.D][gmm.D];
            double u[] = new double[gmm.D];
            GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    		int info[] = new int[1];
    		double noise[][] = new double[data2.length][gmm.D];
            if ((covar2.length == 1) && (covar2[0].length == gmm.D) && (covar2[0][0].length == gmm.D)) { // one-for-all
            	for (i = 0; i < gmm.D; i++) {
			    	for (j = 0; j < gmm.D; j++) {
			    		L[i][j] = covar2[0][i][j];
			    	}
			    }
			    // dpotrf computes the Cholesky factorization of a real symmetric
			    // positive definite matrix A.  The factorization has the form A = U'*U, 
			    // if uplo = 'U', or A = L * L', if uplo = 'L', where U is an upper
			    // triangular matrix and L is lower triangular.  If uplo == 'L', the leading n-by-n lower
			    // triangular part of A contains the lower triangular part of the matrix A, 
			    // and the strictly upper triangular part of A is not referenced.
			    ge.dpotrf('L',gmm.D,L,gmm.D,info);
			    if (info[0] < 0) {
			    	System.err.println("In _drawGMM_BG dpotrf had an illegal value for argument " + (-info[0]));
			    	System.exit(-1);
			    }
			    if (info[0] > 0) {
			    	System.err.println("In _drawGMM_BG for dpotrf the leading minor of order " + info[0] + " is not positive definite");
			    	System.err.println("and the factorization could not be completed.");
			    	System.exit(-1);
			    }
			    for (i = 0; i < gmm.D; i++) {
			    	for (j = 0; j < i; j++) {
			    		L[i][j] = 0.0;
			    	}
			    }
			    for (i = 0; i < data2.length; i++) {
			        for (j = 0; j < gmm.D; j++) {
			        	u[j] = rng.nextGaussian();
			        }
			        for (j = 0; j < gmm.D; j++) {
			        	for (p = 0; p < gmm.D; p++) {
			        	    noise[i][j] += L[j][p]*u[p];	
			        	}
			        	// mean is zero
			        	//noise[i][j] += mean[i][j];
			        }
			    } // for (i = 0; i < data2.length; i++)
            }
            else {
                // create noise from unit covariance and then dot with eigenvalue
                // decomposition of covar2 to get a the right noise distribution:
                // n' = R V^1/2 n, where covar = R V R^-1
                // faster than drawing one sample per each covariance
            	for (i = 0; i < data2.length; i++) {
            		for (j = 0; j < gmm.D; j++) {
            			noise[i][j] = rng.nextGaussian();
            		}
            	}
            	
            	double val[][] = new double[covar2.length][gmm.D];
            	double rot[][][] = new double[covar2.length][gmm.D][gmm.D];
            	for (i = 0; i < covar2.length; i++) {
                	// In EigenvalueDecomposition the columns represent the
                    // eigenvectors
                	// Eigenvalue.decompose has values in increasing order
            	    Eigenvalue.decompose(covar2[i], rot[i], val[i]);
            	    for (j = 0; j < gmm.D; j++) {
            	    	val[i][j] = Math.max(val[i][j], 0.0); // to prevent univariate errors to underflow
            	    }
            	}
		        
                //noise = np.einsum('...ij,...j', rot, np.sqrt(val)*noise)
            	double valnoise[][] = new double[covar2.length][gmm.D];
            	for (i = 0; i < covar2.length; i++) {
	        		for (j = 0; j < gmm.D; j++) {
	        			valnoise[i][j] = Math.sqrt(val[i][j])*noise[i][j];
	        		}
            	}
            	for (i = 0; i < covar2.length; i++) {
            		for (j = 0; j < gmm.D; j++) {
            			for (p = 0; p < gmm.D; p++) {
            				noise[i][j] += rot[i][j][p] * valnoise[i][p];
            			}
            		}
            	}
            }
            for (i = 0; i < data2.length; i++) {
            	for (j = 0; j < gmm.D; j++) {
            		data2[i][j] += noise[i][j];
            	}
            }
        } // if (covar_callback != null)
        data2Vec.add(data2);
        return;
    }
    
    // get zeroth, first, second moments of the data weighted with p_k(x) avgd over x
    public void _Mstep(double A[], double M[][], double C[][][], double B[], GMM gmm, int U[][],
    		double log_p[][], double T_inv[][][][], double log_S[], double data[][], double covar[][][],
    		double R[][][], double p_bg[][]/*, pool=None, chunksize=1*/) {
        // Defaults covar = null, R = null, p_bg = null
    	
    	// perform sums for M step in the pool
        // NOTE: in a partial run, could work on changeable components only;
        // however, there seem to be side effects or race conditions
        int k;
        for (k = 0; k < gmm.K; k++) {
            _Msums(A, M, C, k, U, log_p, T_inv, gmm, data, R, log_S);	
        }
        
        if (p_bg != null) {
        	double q_bg[] = new double[log_S.length];
        	B[0] = 0.0;
        	for (k = 0; k < log_S.length; k++) {
        		// # equivalent to A_k in _Msums, but done without logs
        		B[0] += p_bg[0][k]/Math.exp(log_S[k]);
        	} 
        }
        else {
            B[0] = 0;
        }
        
        return;
        
    }
    
    public void _Msums(double A[], double M[][], double C[][][], int k, int U[][], double log_p[][], 
    		double T_inv[][][][], GMM gmm, double data[][], double R[][][], double log_S[]) {
    	int i,j,p,q;
    	double d[][];
    	double R_[][][] = null;
    	double d_m[][];
    	double q_k[];
        int U_k[] = U[k];
        double log_p_k[] = log_p[k];
        double T_inv_k[][][] = T_inv[k];
        if ((log_p_k == null) || (log_p_k.length == 0)) {
        	A[k] = 0;
        	for (i = 0; i < gmm.D; i++) {
        	    M[k][i] = 0;
        	    for (j = 0; j < gmm.D; j++) {
        	    	C[k][i][j] = 0;
        	    }
        	}
        	return;
        }
        
        // get log_q_ik by dividing with S = sum_k p_ik
        // NOTE:  this modifies log_p_k in place, but is only relevant
        // within this method since the call is parallel and its arguments
        // therefore don't get updated across components.

        // NOTE: reshape needed when U_k is None because of its
        // implicit meaning as np.newaxis
        if ((U_k == null) || (U_k.length == 0)) {
        	for (i = 0; i < log_p_k.length; i++) {
        		log_p_k[i] -= log_S[i];	
        	}
        }
        else {
        	for (i = 0, j = 0; i < log_p_k.length; i++) {
        		log_p_k[i] -= log_S[U_k[i]];		
        	}
        }
        d = new double[log_p_k.length][gmm.D];
        if ((U_k == null) || (U_k.length == 0)) {
        	for (i = 0; i < log_p_k.length; i++) {
        		for (j = 0; j < gmm.D; j++) {
        		    d[i][j] = data[i][j];	
        		}
        	}
        }
        else {
        	for (i = 0; i < log_p_k.length; i++) {
        		for (j = 0; j < gmm.D; j++) {
        		    d[i][j] = data[U_k[i]][j];	
        		}
        	}	
        }
        if (R != null) {
        	R_ = new double[log_p_k.length][gmm.D][gmm.D];
        	if ((U_k == null) || (U_k.length == 0)) {
        		for (i = 0; i < log_p_k.length; i++) {
            		for (j = 0; j < gmm.D; j++) {
            			for (p = 0; p < gmm.D; p++) {
            			    R_[i][j][p]	= R[i][j][p];
            			}
            		}
        		}
        	}
        	else {
        		for (i = 0; i < log_p_k.length; i++) {
            		for (j = 0; j < gmm.D; j++) {
            			for (p = 0; p < gmm.D; p++) {
            			    R_[i][j][p]	= R[U_k[i]][j][p];
            			}
            		}
        		}	
        	}
        } // if (R != null)
        
        // amplitude: A_k = sum_i q_ik
        A[k] = Math.exp(logsum(log_p_k));
        
        // in fact: q_ik, but we treat sample index i silently everywhere
        q_k = new double[log_p_k.length];
        for (i = 0; i < log_p_k.length; i++) {
        	q_k[i] = Math.exp(log_p_k[i]);
        }

        
        d_m = new double[log_p_k.length][gmm.D];
        if (R == null) {
        	for (i = 0; i < log_p_k.length; i++) {
        		for (j = 0; j < gmm.D; j++) {
                    d_m[i][j] = d[i][j] - gmm.mean[k][j];
        		}
        	}
        }
        else {
        	double Rg[][] = new double[log_p_k.length][gmm.D];
        	for (i = 0; i < log_p_k.length; i++) {
        		for (j = 0; j < gmm.D; j++) {
        			for (p = 0; p < gmm.D; p++) {
        				Rg[i][j] += R_[i][j][p] * gmm.mean[k][p];
        			}
        		}
        	}
        	for (i = 0; i < log_p_k.length; i++) {
        		for (j = 0; j < gmm.D; j++) {
                    d_m[i][j] = d[i][j] - Rg[i][j];
        		}
        	}
        }
        
        // data with errors?
	    if ((T_inv_k == null) && (R == null)) {
	        // mean: M_k = sum_i x_i q_ik
	    	for (j = 0; j < gmm.D; j++) {
	    		for (i = 0; i < q_k.length; i++) {
	    			M[k][j] += d[i][j] * q_k[i];
	    		}
	    	}

	        // covariance: C_k = sum_i (x_i - mu_k)^T(x_i - mu_k) q_ik
	        // funny way of saying: for each point i, do the outer product
	        // of d_m with its transpose, multiply with pi[i], and sum over i
	    		for (j = 0; j < gmm.D; j++) {
	    			for (p = 0; p < gmm.D; p++) {
	    			    for (i = 0; i < q_k.length; i++) {
	    			    	C[k][j][p] += (q_k[i] * d_m[i][j] * d_m[i][p]);
	    			}
	    		}
	    	}
	    } // if ((T_inv_k == null) && (R == null))
	    else {
	    	double b_k[][] = new double[d_m.length][gmm.D];
	    	double B_k[][][] = new double[T_inv_k.length][gmm.D][gmm.D];
	    	if (R == null) { // that means T_ik is not None	
	    		// b_ik = mu_k + C_k T_ik^-1 (x_i - mu_k)
	    	    // B_ik = C_k - C_k T_ik^-1 C_k
	    	    // b_k = gmm.mean[k] + np.einsum('ij,...jk,...k', gmm.covar[k], T_inv_k, d_m)
	    		// B_k = gmm.covar[k] - np.einsum('ij,...jk,...kl', gmm.covar[k], T_inv_k, gmm.covar[k])
	    		if (T_inv_k.length == 1) {
	    		    double cT[][] = new double[gmm.D][gmm.D];
	    		    for (i = 0; i < gmm.D; i++) {
	    		    	for (j = 0; j < gmm.D; j++) {
	    		    		for (p = 0; p < gmm.D; p++) {
	    		    			cT[i][j] += gmm.covar[k][i][p] * T_inv_k[0][p][j]; 
	    		    		}
	    		    	}
	    		    }
		    		double cTd[][] = new double[d_m.length][gmm.D];
		    		for (i = 0; i < d_m.length; i++) {
		    			for (j = 0; j < gmm.D; j++) {
		    				for (p = 0; p < gmm.D; p++) {
		    					cTd[i][j] += d_m[i][p] * cT[j][p];
		    				}
		    			}
		    		}
		    		for (i = 0; i < d_m.length; i++) {
		    			for (j = 0; j < gmm.D; j++) {
		    				b_k[i][j] = gmm.mean[k][j] + cTd[i][j];
		    			}
		    		}
		    		double cTc[][] = new double[gmm.D][gmm.D];
		    		for (i = 0; i < gmm.D; i++) {
		    			for (j = 0; j < gmm.D; j++) {
		    				for (p = 0; p < gmm.D; p++) {
		    				    cTc[i][j] += cT[i][p] * gmm.covar[k][p][j];
		    				}
		    			}
		    		}
		    	    for (i = 0; i < gmm.D; i++) {
		    	    	for (j = 0; j < gmm.D; j++) {
		    	    		B_k[0][i][j] = gmm.covar[k][i][j] - cTc[i][j];
		    	    	}
		    	    }
	    		} // if (T_inv_k.length == 1)
	    		else { // T_inv_k.length > 1
	    		    for (i = 0; i < T_inv_k.length; i++) {
	    		    	double cT[][] = new double[gmm.D][gmm.D];
	    		    	for (j = 0; j < gmm.D; j++) {
	    		    		for (p = 0; p < gmm.D; p++) {
	    		    		    for (q = 0; q < gmm.D; q++) {
	    		    		    	cT[j][p] += gmm.covar[k][j][q] * T_inv_k[i][q][p];
	    		    		    }
	    		    		}
	    		    	}
	    		    	double cTd[] = new double[gmm.D];
			    		for (j = 0; j < gmm.D; j++) {
			    			for (p = 0; p < gmm.D; p++) {
			    				cTd[j] += cT[j][p] * d_m[i][p];
			    			}
			    		}
			    		for (j = 0; j < gmm.D; j++) {
			    			b_k[i][j] = gmm.mean[k][j] + cTd[j];
			    		}
			    		double cTc[][] = new double[gmm.D][gmm.D];
			    		for (j = 0; j < gmm.D; j++) {
			    			for (p = 0; p < gmm.D; p++) {
			    				for (q = 0; q < gmm.D; q++) {
			    					cTc[j][p] += cT[j][q] * gmm.covar[k][q][p]; 
			    				}
			    			}
			    		}
			    		for (j = 0; j < gmm.D; j++) {
			    	    	for (p = 0; p < gmm.D; p++) {
			    	    		B_k[i][j][p] = gmm.covar[k][j][p] - cTc[j][p];
			    	    	}
			    	    }
	    		    } // for (i = 0; i < T_inv_k.length; i++)
	    		} // T_inv_k.length > 1
	    	} // if (R == null)
	    	else {
	           // F_ik = C_k R_i^T T_ik^-1
	           // F_k = np.einsum('ij,...kj,...kl', gmm.covar[k], R_, T_inv_k)
	           // b_k = gmm.mean[k] + np.einsum('...ij,...j', F_k, d_m)
	           // B_k = gmm.covar[k] - np.einsum('...ij,...jk,kl', F_k, R_, gmm.covar[k])
	    	   double cR[][][] = new double[R_.length][gmm.D][gmm.D];
	    	   double F_k[][][] = new double[R_.length][gmm.D][gmm.D];
	    	   for (i = 0; i < R_.length; i++) {
	    		   for (j = 0; j < gmm.D; j++) {
	    			   for (p = 0; p < gmm.D; p++) {
	    				   for (q = 0; q < gmm.D; q++) {
	    					   cR[i][j][p] += gmm.covar[k][j][q] * R_[i][p][q];
	    				   }
	    			   }
	    		   }
	    	   }
	    	   if (T_inv_k.length == 1) {
	    		   for (i = 0; i < R_.length; i++) {
		    		   for (j = 0; j < gmm.D; j++) {
		    			   for (p = 0; p < gmm.D; p++) {
		    				   for (q = 0; q < gmm.D; q++) {
		    					   F_k[i][j][p] += cR[i][j][q] * T_inv_k[0][q][p];
		    				   }
		    			   }
		    		   }
		    	   } 
	    	   }
	    	   else {
	    		   for (i = 0; i < R_.length; i++) {
		    		   for (j = 0; j < gmm.D; j++) {
		    			   for (p = 0; p < gmm.D; p++) {
		    				   for (q = 0; q < gmm.D; q++) {
		    					   F_k[i][j][p] += cR[i][j][q] * T_inv_k[i][q][p];
		    				   }
		    			   }
		    		   }
		    	   }    
	    	   }
	    	   for (i = 0; i < F_k.length; i++) {
	    		   double Fd[] = new double[gmm.D];
	    		   for (j = 0; j < gmm.D; j++) {
	    		       for (p = 0; p < gmm.D; p++) {
	    		    	   Fd[j] += F_k[i][j][p] * d_m[i][p];
	    		       }
	    		   }
	    		   for (j = 0; j < gmm.D; j++) {
		    			b_k[i][j] = gmm.mean[k][j] + Fd[j];
		    		}
	    	   }
	    	   for (i = 0; i < F_k.length; i++) {
	    	       double FR[][] = new double[gmm.D][gmm.D];
	    	       for (j = 0; j < gmm.D; j++) {
	    			   for (p = 0; p < gmm.D; p++) {
	    				   for (q = 0; q < gmm.D; q++) {
	    					   FR[j][p] += F_k[i][j][q] * R_[i][q][p];
	    				   }
	    			   }
	    		   }
	    	       double FRc[][] = new double[gmm.D][gmm.D];
	    	       for (j = 0; j < gmm.D; j++) {
	    			   for (p = 0; p < gmm.D; p++) {
	    				   for (q = 0; q < gmm.D; q++) {
	    					   FRc[j][p] += FR[j][q] * gmm.covar[k][q][p];
	    				   }
	    			   }
	    		   }
	    	       for (j = 0; j < gmm.D; j++) {
		    	    	for (p = 0; p < gmm.D; p++) {
		    	    		B_k[i][j][p] = gmm.covar[k][j][p] - FRc[j][p];
		    	    	}
		    	    }
	    	   } // for (i = 0; i < F_k.length; i++)
	    	} // else for (R == null)
	    	for (i = 0; i < gmm.D; i++) {
	    		M[k][i] = 0;
	    		for (j = 0; j < b_k.length; j++) {
	    			M[k][i] += b_k[j][i] * q_k[j];
	    		}
	    	}
	        for (i = 0; i < b_k.length; i++) {
	        	for (j = 0; j < gmm.D; j++) {
	        		b_k[i][j] -= gmm.mean[k][j];
	        	}
	        }
	        double prod[][][] = new double[q_k.length][gmm.D][gmm.D];
	        for (i = 0; i < q_k.length; i++) {
	        	for (j = 0; j < gmm.D; j++) {
	        		for (p = 0; p < gmm.D; p++) {
	        			prod[i][j][p] = q_k[i]*b_k[i][j]*b_k[i][p];
	        		}
	        	}
	        }
	        double prodPlus[][][] = new double[q_k.length][gmm.D][gmm.D];
	        for (i = 0; i < q_k.length; i++) {
	        	for (j = 0; j < gmm.D; j++) {
	        		for (p = 0; p < gmm.D; p++) {
	        			prodPlus[i][j][p] = prod[i][j][p] + B_k[Math.min(i,B_k.length-1)][j][p];
	        		}
	        	}
	        }
	        for (j = 0; j < gmm.D; j++) {
	        	for (p = 0; p < gmm.D; p++) {
	        		C[k][j][p] = 0;
	        		for (i = 0; i < q_k.length; i++) {
	        			C[k][j][p] += prodPlus[i][j][p];
	        		}
	        	}
	        }
	    } // else for ((T_inv_k == null) && (R == null))
	    return;
    }
    
    // perform E step calculations.
    // If cutoff is set, this will also set the neighborhoods U
    public double _Estep(GMM gmm, double log_p[][], int U[][], double T_inv[][][][], double log_S[],
    		double data[][], double covar[][][], double R[][][], double omega[], Background background,
    		double p_bg[][], 
    		// pool=None, chunksize=1,
    		double cutoff, int it, Random rng) {
    	// Defaults covar=null, R=null, omega=null, background=null, p_bg=null,
    	// pool=null, chunksize=1, cutoff=null, it=0
    	
    	int i,d;
    	boolean H[];
    	int k;
    	double log_L;
    	double denom[] = null;
    	if (covar != null) {
    		denom = new double[covar.length];
    	}
    	// compute p(i | k) for each k independently in the pool
        // need S = sum_k p(i | k) for further calculation
    	for (i = 0; i < log_S.length; i++) {
            log_S[i] = 0.0;
    	}
    	
    	// H = {i | i in neighborhood[k]} for any k, needed for outliers below
        // TODO: Use only when cutoff is set
        H = new boolean[data.length];
        
        
        for (k = 0; k < gmm.K; k++) {
        	_Esum(log_p, T_inv, k, U, gmm, data, covar, R, cutoff /*, pool chunksize*/);
        	for (i = 0; i < U[k].length; i++) {
        	    log_S[U[k][i]] += Math.exp(log_p[k][i]); // actually S, not logS
        	    H[U[k][i]] = true;
        	}
        }
        
        if (background != null) {
        	p_bg[0] = new double[data.length];
        	for (i = 0; i < data.length; i++) {
                p_bg[0][i] = background.amp * background.p();
        	}
            if (covar != null) {
                // This is the zeroth moment of a truncated Normal error distribution
                // Its calculation is simple only of the covariance is diagonal!
                // See e.g. Manjunath & Wilhem (2012) if not
                double error[] = new double[data.length];
                for (i = 0; i < error.length; i++) {
                	error[i] = 1.0;
                }
                double x0[] = background.footprint[0];
                double x1[] = background.footprint[1];
                for (d = 0; d < gmm.D; d++) { 
                    if ((covar.length == 1) && (covar[0].length == gmm.D) && (covar[0][0].length == gmm.D)) { // one-for-all
                        denom[0] = Math.sqrt(2 * covar[1][d][d]);
                    }
                    else {
                    	for (i = 0; i < covar.length; i++ ) {
                            denom[i] = Math.sqrt(2 * covar[i][d][d]);
                    	}
                    }
                    // CAUTION: The erf is approximate and returns 0
                    // Thus, we don't add the logs but multiple the value itself
                    // underrun is not a big problem here
                    for (i = 0; i < data.length; i++) {
                        error[i] *= (erf((data[i][d] - x0[d])/denom[Math.min(i,denom.length-1)])  
                        		- erf((data[i][d] - x1[d])/denom[Math.min(i,denom.length-1)])) / 2;
                    }
                } // for (d = 0; d < gmm.D; d++)
                for (i = 0; i < error.length; i++) {
                    p_bg[0][i] *= error[i];
                }
            } // if (covar != null)
            for (i = 0; i < log_S.length; i++) {
                log_S[i] = Math.log(log_S[i] + p_bg[0][i]);
            }
            if (omega != null) {
                log_S[i] += Math.log(omega[i]);
            }
            log_L = 0.0;
            for (i = 0; i < log_S.length; i++) {
                log_L += log_S[i];
            }
        } // if (background != null)
        else {
            // need log(S), but since log(0) isn't a good idea, need to restrict to H
        	for (i = 0; i < log_S.length; i++) {
        		if (H[i]) {
        			log_S[i] = Math.log(log_S[i]);
        		}
        	}
            if (omega != null) {
            	for (i = 0; i < log_S.length; i++) {
                    log_S[i] += Math.log(omega[i]);
            	}
            }
            log_L = 0.0;
            for (i = 0; i < log_S.length; i++) {
            	if (H[i]) {
                    log_L += log_S[i];
            	}
            }
        } // else

    	return log_L;
    }
    
    // Error function erf(x) from Computation of Special Functions by 
 	// Shanjie Zhang and Jianming Jin. pp. 622-623.
 	private double erf(double x) {
 		double eps = 1.0E-15;
 		double x2, er, r, c0, err;
 		int k;
 		x2 = x * x;
 		if (Math.abs(x) < 3.5) {
 			er = 1.0;
 			r = 1.0;
 			for (k = 1; k <= 50; k++) {
 				r = r*x2/(k + 0.5);
 				er = er + r;
 				if (Math.abs(r) <= Math.abs(er)*eps) {
 					break;
 				}
 			} // for (k = 1; k <= 50; k++)
 			c0 = 2.0/Math.sqrt(Math.PI) * x * Math.exp(-x2);
 			err = c0 * er;
 		} // if (Math.abs(x) < 3.5)
 		else {
 			er = 1.0;
 			r = 1.0;
 			for (k = 1; k <= 12; k++) {
 				r = -r*(k - 0.5)/x2;
 				er = er + r;
 			}
 			c0 = Math.exp(-x2)/(Math.abs(x) * Math.sqrt(Math.PI));
 			err = 1.0 - c0 * er;
 			if (x < 0.0) err = -err;
 		} // else
 		return err;
 	}
    
    // compute chi^2, and apply selections on component neighborhood based in chi^2
    public void _Esum(double log_p[][], double T_inv[][][][], int k, int U[][], GMM gmm, double data[][], double covar[][][], double R[][][], double cutoff
    		/*, pool, chunksize*/) {
    	 // since U_k could be Integer.MIN_VALUE, need explicit reshape
    	double logpk[] = log_p[k];
    	double T_inv_k[][][] = T_inv[k];
    	int U_k[] = U[k];
    	int i,j,m,p;
    	double d_[][];
    	double covar_[][][] = null;
    	double R_[][][] = null;
    	double dx[][] = null;
    	LinearEquations2 le2 = new LinearEquations2();
    	double chi2[] = null;
    	int ipiv[] = new int[gmm.D];
		int info[] = new int[1];
		double work[];
		int lwork;
		double sign[];
		double logdet[];
    	if ((U_k == null) || (U_k.length == 0)) {
    		d_ = new double[data.length][gmm.D];
    		for (i = 0; i < data.length; i++) {
    			for (j = 0; j < gmm.D; j++) {
    				d_[i][j] = data[i][j];
    			}
    		}
    	}
    	else {
    		d_ = new double[U_k.length][gmm.D];
    		for (i = 0; i < U_k.length; i++) {
    			for (j = 0; j < gmm.D; j++) {
    				d_[i][j] = data[U_k[i]][j];
    			}
    		}
    	}
    	
    	if (covar != null) {
            if ((covar.length == 1) && (covar[0].length == gmm.D) && (covar[0][0].length == gmm.D)) {  // one-for-all
            	covar_ = new double[1][gmm.D][gmm.D];
            	for (i = 0; i < gmm.D; i++) {
            	    for (j = 0; j < gmm.D; j++) {
            		    covar_[0][i][j] = covar[0][i][j];	
            		}
            	}
            }
            else if ((U_k == null) || (U_k.length == 0)) { // each datum has covariance
            	covar_ = new double[covar.length][gmm.D][gmm.D];
            	for (i = 0; i < covar.length; i++) {
            	    for (j = 0; j < gmm.D; j++) {
            		    for (m = 0; m < gmm.D; m++) {
            		        covar_[i][j][m] = covar[i][j][m];	
            		    }
            		}
            	}
            }
            else {
            	covar_ = new double[U_k.length][gmm.D][gmm.D];
            	for (i = 0; i < U_k.length; i++) {
            	    for (j = 0; j < gmm.D; j++) {
            		    for (m = 0; m < gmm.D; m++) {
            		        covar_[i][j][m] = covar[U_k[i]][j][m];	
            		    }
            		}
            	}	
            }
        } // if (covar != null)
    	    
        if (R != null) {
        	if ((U_k == null) || (U_k.length == 0)) {
	        	R_ = new double[R.length][gmm.D][gmm.D];
	        	for (i = 0; i < R.length; i++) {
	        		for (j = 0; j < gmm.D; j++) {
	        		    for (m = 0; m < gmm.D; m++) {
	        		    	R_[i][j][m] = R[i][j][m];
	        		    }
	        		}
	        	}
        	}
        	else {
        		R_ = new double[U_k.length][gmm.D][gmm.D];
	        	for (i = 0; i < U_k.length; i++) {
	        		for (j = 0; j < gmm.D; j++) {
	        		    for (m = 0; m < gmm.D; m++) {
	        		    	R_[i][j][m] = R[U_k[i]][j][m];
	        		    }
	        		}
	        	}	
        	}
        } // if (R != null)
        
        // p(x | k) for all x in the vicinity of k
        // determine all points within cutoff sigma from mean[k]
        dx = new double[d_.length][gmm.D];
        if (R == null) {
        	for (i = 0; i < d_.length; i++) {
        		for (j = 0; j < gmm.D; j++) {
                    dx[i][j] = d_[i][j] - gmm.mean[k][j];
        		}
        	}
        }
        else {
        	double rg[][] = new double[R_.length][gmm.D];
        	for (i = 0; i < R_.length; i++) {
        		for (j = 0; j < gmm.D; j++) {
        			for (m = 0; m < gmm.D; m++) {
        			    rg[i][j] += R_[i][j][m] * gmm.mean[k][m];
        			}
        		}
        	}
        	for (i = 0; i < d_.length; i++) {
        		for (j = 0; j < gmm.D; j++) {
        		    dx[i][j] = d_[i][j] - rg[i][j];
        		}
        	}
        }
        
        if ((covar == null) && (R == null)) {
            T_inv_k = null;
            // chi2 = np.einsum('...i,...ij,...j', dx, np.linalg.inv(gmm.covar[k]), dx)
            // Equivalent to diagonal of (dx * covar[k].inverse * dx.transpose())
    		double covarinv[][] = new double[gmm.D][gmm.D];
    		for (i = 0; i < gmm.D; i++) {
    			for (j = 0; j < gmm.D; j++) {
    				covarinv[i][j] = gmm.covar[k][i][j];
    			}
    		}
    		le2.dgetrf(gmm.D,gmm.D,covarinv,gmm.D,ipiv,info);
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
		      le2.dgetri(gmm.D,covarinv,gmm.D,ipiv,work,lwork,info);
		      if (info[0] < 0) {
		    	  System.err.println("In le2.dgetri argument number " + 
		      (-info[0]) + " is illegal");
		    	  System.exit(-1);
		      }
		      lwork = (int)work[0];
		      work = new double[lwork];
		      le2.dgetri(gmm.D,covarinv,gmm.D,ipiv,work,lwork,info);
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
		      double dxC[][] = new double[dx.length][gmm.D];
		      for (i = 0; i < dx.length; i++) {
		    	  for (m = 0; m < gmm.D; m++) {
		    	      for (j = 0; j < gmm.D; j++) {
		    	          dxC[i][m] += dx[i][j] * covarinv[j][m]; 
		    	      }
		    	  }
		      }
		      double dxCdx[][] = new double[dx.length][dx.length];
		      for (i = 0; i < dx.length; i++) {
		    	  for (m = 0; m < dx.length; m++) {
		    		  for (j = 0; j < gmm.D; j++) {
		    			  dxCdx[i][m] += dxC[i][j] * dx[m][j];
		    		  }
		    	  }
		      }
		      chi2 = new double[dx.length];
		      for (i = 0; i < dx.length; i++) {
		    	  chi2[i] = dxCdx[i][i];
		      }
        } // if ((covar == null) && (R == null))
        else {
        	// with data errors: need to create and return T_ik = covar_i + C_k
        	// and weight each datum appropriately	
        	if (R == null) {
        	    T_inv_k = new double[covar_.length][gmm.D][gmm.D];	
        	    for (i = 0; i < covar_.length; i++) {
        	    	for (j = 0; j < gmm.D; j++) {
        	    		for (m = 0; m < gmm.D; m++) {
        	    		    T_inv_k[i][j][m] = gmm.covar[k][j][m] + covar_[i][j][m];
        	    		}
        	    	}
        	    }
        	} // if (R == null)
        	else { // need to project out missing elements: T_ik = R_i C_k R_i^R + covar_i
        	    // T_inv_k = np.linalg.inv(np.einsum('...ij,jk,...lk', R_, gmm.covar[k], R_) + covar_)	
        		// Code here is never executed in the test.py file
        		// The einsum does a matrix multiplication of R_ and gmm.covar[k] resulting
        		// in a matrix RC of length[R_.length][gmm.D][gmm.D].  The we multiply 2D
        		// stacks of RC with 2D stacks of R_ with axes 1 and 2 transposed.  This gives
        		// a final result with a length[R_.length][gmm.D][gmm.D].  A covar_ of the same
        		// length is added. A np.linalg.inv will perform inverses on the stack of 2D 
        		// matrices yielding a T_inv_k of length[R_.length][gmm.D][gmm.D]
        		double RC[][][] = new double[R_.length][gmm.D][gmm.D];
        		for (i = 0; i < R_.length; i++) {
        			for (j = 0; j < gmm.D; j++) {
        				for (m = 0; m < gmm.D; m++) {
        					for (p = 0; p < gmm.D; p++) {
        					    RC[i][j][m] += R_[i][j][p] * gmm.covar[k][p][m];
        					}
        				}
        			}
        		}
        		double RCR[][][] = new double[R_.length][gmm.D][gmm.D];
        		for (i = 0; i < R_.length; i++) {
        			for (j = 0; j < gmm.D; j++) {
        				for (m = 0; m < gmm.D; m++) {
        					for (p = 0; p < gmm.D; p++) {
        					    RCR[i][j][m] += R_[i][j][p] * R_[i][m][p];
        					}
        				}
        			}
        		}
        		T_inv_k = new double[R_.length][gmm.D][gmm.D];
        		for (i = 0; i < R_.length; i++) {
        			for (j = 0; j < gmm.D; j++) {
        				for (m = 0; m < gmm.D; m++) {
        				    T_inv_k[i][j][m] = RCR[i][j][m] + covar_[i][j][m];
        				}	
        			}
        		}
        	} // else
        	for (i = 0; i < T_inv_k.length; i++) {
        	    le2.dgetrf(gmm.D,gmm.D,T_inv_k[i],gmm.D,ipiv,info);
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
    		      le2.dgetri(gmm.D,T_inv_k[i],gmm.D,ipiv,work,lwork,info);
    		      if (info[0] < 0) {
    		    	  System.err.println("In le2.dgetri argument number " + 
    		      (-info[0]) + " is illegal");
    		    	  System.exit(-1);
    		      }
    		      lwork = (int)work[0];
    		      work = new double[lwork];
    		      le2.dgetri(gmm.D,T_inv_k[i],gmm.D,ipiv,work,lwork,info);
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
    	    } // for (i = 0; i < T_inv_k.length; i++)
        	// chi2 = np.einsum('...i,...ij,...j', dx, T_inv_k, dx)
        	//System.out.println("dx.length = " + dx.length);
        	//System.out.println("dx[0].length = " + dx[0].length);
        	//System.out.println("T_inv_K.length = " + T_inv_k.length);
        	if (T_inv_k.length == dx.length) {
	        	double dxT[][][] = new double[dx.length][dx.length][gmm.D];
	        	for (i = 0; i < dx.length; i++) {
	        		for (j = 0; j < dx.length; j++) {
	        			for (m = 0; m < gmm.D; m++) {
	        				for (p = 0; p < gmm.D; p++) {
	        					dxT[i][j][m] += dx[i][p] * T_inv_k[j][p][m];
	        				}
	        			}
	        		}
	        	}
	        	double dxTdx[][][] = new double[dx.length][dx.length][dx.length];
	        	for (i = 0; i < dx.length; i++) {
	        		for (j = 0; j < dx.length; j++) {
	        			for (m = 0; m < dx.length; m++) {
	        				for (p = 0; p < gmm.D; p++) {
	        					dxTdx[i][j][m] += dxT[i][j][p] * dx[m][p];
	        				}
	        			}
	        		}
	        	}
	        	chi2 = new double[dx.length];
	        	for (i = 0; i < dx.length; i++) {
	        		chi2[i] = dxTdx[i][i][i];
	        	}
        	}
        	else if (T_inv_k.length == 1) {
        		double dxT[][] = new double[dx.length][gmm.D];
        		for (i = 0; i < dx.length; i++) {
        			for (m = 0; m < gmm.D; m++) {
        				for (p = 0; p < gmm.D; p++) {
        					dxT[i][m] += dx[i][p] * T_inv_k[0][p][m];
        				}
        			}
	        	}
        		double dxTdx[][] = new double[dx.length][dx.length];
	        	for (i = 0; i < dx.length; i++) {
        			for (m = 0; m < dx.length; m++) {
        				for (p = 0; p < gmm.D; p++) {
        					dxTdx[i][m] += dxT[i][p] * dx[m][p];
        				}
	        		}
	        	}
	        	chi2 = new double[dx.length];
	        	for (i = 0; i < dx.length; i++) {
	        		chi2[i] = dxTdx[i][i];
	        	}
        	}
        } // else
        
        // NOTE: close to convergence, we could stop applying the cutoff because
        // changes to U will be minimal
        if (!Double.isNaN(cutoff)) {
        	int numindices = 0;
        	for (i = 0; i < chi2.length; i++) {
        		if (chi2[i] < cutoff) {
        			numindices++;
        		}
        	}
        	int indices[] = new int[numindices];
        	for (i = 0, j = 0; i< chi2.length; i++) {
        		if (chi2[i] < cutoff) {
        		    indices[j++] = i;
        		}
        	}
            double chi2temp[] = new double[chi2.length];
            for (i = 0; i < chi2.length; i++) {
            	chi2temp[i] = chi2[i];
            }
            chi2 = new double[numindices];
            for (i = 0; i < numindices; i++) {
            	chi2[i] = chi2temp[indices[i]];
            }
            chi2temp = null;
            if (((covar != null) && ((covar.length != 1) || (covar[0].length != gmm.D) || (covar[0][0].length !=  gmm.D))) || (R != null)) {
            	double T_inv_ktemp[][][] = new double[T_inv_k.length][gmm.D][gmm.D];
            	for (i = 0; i < T_inv_k.length; i++) {
            		for (j = 0; j < gmm.D; j++) {
            			for (m = 0; m < gmm.D; m++) {
            				T_inv_ktemp[i][j][m] = T_inv_k[i][j][m];
            			}
            		}
            	}
            	T_inv_k = new double[numindices][gmm.D][gmm.D];
            	for (i = 0; i < numindices; i++) {
            		for (j = 0; j < gmm.D; j++) {
            			for (m = 0; m < gmm.D; m++) {
            				T_inv_k[i][j][m] = T_inv_ktemp[indices[i]][j][m];
            			}
            		}
            	}
                T_inv_ktemp = null;
            }
            if ((U_k == null) || (U_k.length == 0)) {
            	int numnonzero = 0;
            	for (i = 0; i < indices.length; i++) {
            		if (indices[i] != 0) {
            			numnonzero++;
            		}
            	}
            	U_k = new int[numnonzero];
            	for (i = 0, j = 0; i < indices.length; i++) {
            		if (indices[i] != 0) {
            			U_k[j++] = i;
            		}
            	}
            }
            else {
            	int U_ktemp[] = new int[U_k.length];
            	for (i = 0; i < U_k.length; i++) {
            		U_ktemp[i] = U_k[i];
            	}
            	U_k = new int[indices.length];
            	for (i = 0, j = 0; i < indices.length; i++) {
            		U_k[j++] = U_k[indices[i]];
            	}
            	U_ktemp = null;
            }
        } // if (!Double.isNaN(cutoff))
        
        // prevent tiny negative determinants to mess up
        if (covar == null) {
        	Matrix detMat = new Matrix(gmm.covar[k]);
		    double det = detMat.det();
		    sign = new double[1];
		    logdet = new double[1];
		    if (det < 0) {
		        sign[0] = -1.0;
		        logdet[0] = Math.log(-det);
		    }
		    else if (det == 0.0) {
		    	sign[0] = 0.0;
		    	logdet[0] = Double.NEGATIVE_INFINITY;
		    }
		    else {
		    	sign[0] = 1.0;
		    	logdet[0] = Math.log(det);
		    }
        }
        else {
            //(sign, logdet) = np.linalg.slogdet(T_inv_k)
            //sign *= -1 # since det(T^-1) = 1/det(T)
        	sign = new double[T_inv_k.length];
        	logdet = new double[T_inv_k.length];
        	for (i = 0; i < T_inv_k.length; i++) {
	            Matrix detMat = new Matrix(T_inv_k[i]);
			    double det = detMat.det();
			    if (det < 0) {
			        sign[i] = -1.0;
			        logdet[i] = Math.log(-det);
			    }
			    else if (det == 0.0) {
			    	sign[i] = 0.0;
			    	logdet[i] = Double.NEGATIVE_INFINITY;
			    }
			    else {
			    	sign[i] = 1.0;
			    	logdet[i] = Math.log(det);
			    }
			    sign[i] *= -1; // since det(T^-1) = 1/det(T)
        	} // for (i = 0; i < T_inv_k.length; i++)
        }

        double log2piD2 = Math.log(2.0*Math.PI)*(0.5*gmm.D);
        log_p[k] = new double[Math.max(chi2.length,logdet.length)];
        for (i = 0; i < log_p[k].length; i++) {
        	if (chi2.length > 0) {
        	    log_p[k][i] = Math.log(gmm.amp[k]) - log2piD2 -sign[Math.min(i,sign.length-1)]*logdet[Math.min(i,logdet.length-1)]
        			- chi2[Math.min(i,chi2.length-1)]/2;
        	}
        	else {
        		log_p[k][i] = Math.log(gmm.amp[k]) - log2piD2 -sign[Math.min(i,sign.length-1)]*logdet[Math.min(i,logdet.length-1)];	
        	}
        }
        U[k] = U_k;
        T_inv[k] = T_inv_k;
        return;
    }
    
    public double chi2_cutoff(int D, double cutoff) {
    	// Default: cutoff = 3.0.
        // D-dimensional eqiuvalent of "n sigma" cut.

        // Evaluates the quantile function of the chi-squared distribution to determine
        // the limit for the chi^2 of samples wrt to GMM so that they satisfy the
        // 68-95-99.7 percent rule of the 1D Normal distribution.

        // Args:
        //    D (int): dimensions of the feature space
        //    cutoff (float): 1D equivalent cut [in units of sigma]

        // Returns:
        //    float: upper limit for chi-squared in D dimensions
       
        double ansR[] = new double[1];
    	Statistics stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, cutoff, 0, ansR);
    	stat.run();
    	double cdf_1d = ansR[0];
        double confidence_1d = 1-(1-cdf_1d)*2;
        // Percent point function Inverse of cdf-percentiles
        //cutoff_nd = scipy.stats.chi2.ppf(confidence_1d, D)
        // Regularized incomplete gamma function Q(D/2,1/(2*confidence_1d)) =
        // 1 - regularized incomplete gamma function P(D/2,1/(2*confidence_1d))
        double lowerIncompleteGamma[] = new double[1];
        double upperIncompleteGamma[] = new double[1];
        double regularizedGammaP[] = new double[1];
        Gamma gam = new Gamma(D/2.0, 1.0/(2.0 * confidence_1d), lowerIncompleteGamma,
        		upperIncompleteGamma, regularizedGammaP);
        gam.run();
        double cutoff_nd = 1.0 - regularizedGammaP[0];
        return cutoff_nd;
    }
    
    public double[][][] covar_callback_default(double coords[][]) {
    	// default: Default = null;
    	int N = coords.length;
    	int D = coords[0].length;
        if ((covar_callback_default_arg.length != 1) && (covar_callback_default_arg[0].length != D) && (covar_callback_default_arg[0][0].length != D)) {
            System.err.println("covar_callback received improper default covariance length = " + 
            covar_callback_default_arg.length+","+covar_callback_default_arg[0].length+","+covar_callback_default_arg[0][0].length);
            System.exit(-1);
        }
        // no need to copy since a single covariance matrix is sufficient
        // return np.tile(default, (N,1,1))
        return covar_callback_default_arg;
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
        int D = data[0].length;
		if (s < 0.0) {
			double min_pos;
			double max_pos;
			double vol_data = 1.0;
			for (j = 0; j < D; j++) {
				min_pos = Double.MAX_VALUE;
				max_pos = -Double.MAX_VALUE;
				for (i = 0; i < data.length; i++) {
					if (data[i][j] < min_pos) {
						min_pos = data[i][j];
					}
					if (data[i][j] > max_pos) {
						max_pos = data[i][j];
					}
				}
				vol_data *= (max_pos - min_pos);
			}
			double result[] = new double[1];
			Gamma gam = new Gamma((gmm.D*0.5 + 1),result);
			gam.run();
	        s = Math.pow((vol_data / gmm.K * result[0]),(1/gmm.D)) / Math.sqrt(Math.PI);
	        System.out.println("initializing spheres with s = " + s + " near data points");
		} // if (s < 0.0)

		
		
		for (i = 0; i < k_len; i++) {
			for (j = 0; j < D; j++) {
			    gmm.mean[k[i]][j] = data[refs[i]][j] + s*rng.nextGaussian();	
			}
		}
		
		for (i = 0; i < k_len; i++) {
			for (j = 0; j < data[0].length; j++) {
				for (m = 0; m < data[0].length; m++) {
					if (j == m) {
	                    gmm.covar[k[i]][j][m] = s*s;
					}
					else {
						gmm.covar[k[i]][j][m] = 0.0;
					}
				}
			}
		}
    }
    
    public void initFromKMeans(GMM gmm, double data[][], Random rng) {
        // Initialization callback from a k-means clustering run.

        // See Algorithm 1 from Bloemer & Bujna (arXiv:1312.5946)
        // NOTE: The result of this call are not deterministic even if rng is set
        // because scipy.cluster.vq.kmeans2 uses its own initialization.

        // Args:
        //    gmm: A GMM to be initialized
        //    data: numpy array (N,D) to define the range of the component means
        //    rng: numpy.random.RandomState for deterministic behavior

        //Returns:
        //    None
    	int i,j,m,p,index;
    	AlgorithmKMeans kMeansAlgo;
		ModelImage kMeansImage = null;
		int algoSelection = AlgorithmKMeans.K_MEANS;
		int distanceMeasure = AlgorithmKMeans.EUCLIDEAN_SQUARED;
		// First subscript x = 0, y = 1, z = 2, t = 3
	    // Second subscript 0 to nPoints-1 for each point
	    // Value is the point position
		double[][] pos = new double[data[0].length][data.length];
		
		for (i = 0; i < data.length; i++) {
			for (j = 0; j < data[0].length; j++) {
				pos[j][i] = data[i][j];
			}
		}
		// Use 1.0 in every dimension if not scaled
		double scale[] = new double[data[0].length];
		for (i = 0; i < data[0].length; i++) {
			scale[i] = 1.0;
		}
		// subscript goes from 0 to nPoints-1 for each point
	    // Value is the cluster number from 0 to numberClusters-1.
		int[] groupNum = new int[data.length];
		// subscript goes form 0 to nPoints-1 for each point
		// Value is the weight or number of occurrences of each point
		double[] weight = new double[data.length];
		for (i = 0; i < data.length; i++) {
			weight[i] = 1.0;
		}
		// First subscript x = 0, y = 1, z = 2, t = 3
	    // Second subscript 0 to numberClusters-1 for each cluster
	    // Value is the cluster position
	    double[][] centroidPos = new double[data[0].length][gmm.K];
	    String resultsFileName = null;
	    int initSelection = AlgorithmKMeans.RANDOM_INIT;
	    float redBuffer[] = null;
		float greenBuffer[] = null;
		float blueBuffer[] = null;
		// Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
		double scaleMax = 255.0;
		// If true, each color is weighed proportional to its frequency
		boolean useColorHistogram = false;
		boolean scaleVariablesToUnitVariance = false;
		double axesRatio[] = null;
		// If true, segment a black and white image using one dimensional kmeans
		boolean bwSegmentedImage = false;
		// Buffer containing original values of black and white image
		double doubleBuffer[] = null;
		boolean showKMeansSegmentedImage = false;
		boolean followBatchWithIncremental = false;
		// If true, three dimensional color segmenting in RGB. If false, two dimensional
		// color segmenting in CIELAB
		boolean colorSegmentInRGB = false;
		kMeansAlgo = new AlgorithmKMeans(kMeansImage, algoSelection, distanceMeasure, pos, scale, groupNum,
				weight, centroidPos, resultsFileName, initSelection, redBuffer, greenBuffer, blueBuffer, scaleMax,
				useColorHistogram, scaleVariablesToUnitVariance, axesRatio, bwSegmentedImage, doubleBuffer,
				showKMeansSegmentedImage, followBatchWithIncremental, colorSegmentInRGB);
		kMeansAlgo.run();
		boolean mask[] = new boolean[data.length];
		double d_m[][];
		double prod[][] = new double[data[0].length][data[0].length];
        for (i = 0; i < gmm.K; i++) {
        	int maskSum = 0;
        	for (j = 0; j < data.length; j++) {
        		if (groupNum[j] == i) {
        			mask[j] = true;
        			maskSum++;
        		}
        		else {
        			mask[j] = false;
        		}
        	}
            gmm.amp[i] = (double)maskSum / (double)data.length;
            d_m = new double[maskSum][data[0].length];
            for (j = 0; j < data[0].length; j++) {
            	double dataSum = 0.0;
                for (m = 0; m < data.length; m++) {
                	if (mask[m]) {
                	    dataSum += data[m][j];	
                	}
                }
                gmm.mean[i][j] = dataSum/(double)maskSum;
            }
            for (j = 0, index = 0; j < data.length; j++) {
            	if (mask[j]) {
            	    for (m = 0; m < data[0].length; m++) {
            	    	d_m[index][m] = data[j][m] - gmm.mean[i][m];
            	    }
            	    index++;
            	}
            }
            // funny way of saying: for each point i, do the outer product
            // of d_m with its transpose and sum over i
            for (j = 0; j < data[0].length; j++) {
            	for (m = 0; m < maskSum; m++) {
            	    for (p = 0; p < data[0].length; p++) {
            	    	prod[j][p] += (d_m[m][j] * d_m[m][p]);
            	    }
            	}
            }
            for (j = 0; j < data[0].length; j++) {
            	for (p = 0; p < data[0].length; p++) {
            		gmm.covar[i][j][p] = prod[j][p]/data.length;
            	}
            }
        } //  for (i = 0; i < gmm.K; i++)
    }
    
    public void initFromDataMinMax(GMM gmm, double data[][], double covar[][][], double s, int k[], Random rng) {
    	// default covar = null, s = -1, k = null
        // Initialization callback for uniform random component means.

        // Component amplitudes are set at 1/gmm.K, covariances are set to
        // s**2*np.eye(D), and means are distributed randomly over the range that is
        // covered by data.

        // If s is not given, it will be set such that the volume of all components
        // completely fills the space covered by data.

        // Args:
        //    gmm: A GMM to be initialized
        //    data: numpy array (N,D) to define the range of the component means
        //    covar: ignored in this callback
        //    s (float): if set, sets component variances
        //    k (iterable): list of components to set, is None sets all components
        //    rng: numpy.random.RandomState for deterministic behavior

        // Returns:
        //    None
      
    	int i,j,m,index;
    	if (k == null) {
            k = new int[gmm.K];
            for (i = 0; i < gmm.K; i++) {
            	k[i] = i;
            }
        }
    	for (i = 0; i < k.length; i++) {
            gmm.amp[k[i]] = 1/gmm.K;
    	}
        // set model to random positions with equally sized spheres within
        // volumne spanned by data
    	int D = data[0].length;
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
		for (i = 0, index = 0; i < k.length; i++) {
			for (j = 0; j < gmm.D; j++) {
				gmm.mean[k[index]][j] = min_pos[j] + (max_pos[j] - min_pos[j])*rng.nextDouble();
			}
			index++;
		}
        // if s is not set: use volume filling argument:
    	// K spheres of radius s [having volume s^D * pi^D/2 / gamma(D/2+1)]
	    // should completely fill the volume spanned by data.
	    if (s < 0.0) {
	    	double result[] = new double[1];
			Gamma gam = new Gamma((gmm.D*0.5 + 1),result);
			gam.run();
	        s = Math.pow((vol_data / gmm.K * result[0]),(1/gmm.D)) / Math.sqrt(Math.PI);
	        System.out.println("initializing spheres with s = " + s + " in data domain");
	    } // if (s < 0.0)
    
       for (i = 0; i < k.length; i++) {
	       for (j = 0; j < data[0].length; j++) {
	    	   for (m = 0; m < data[0].length; m++) {
					if (j == m) {
	                    gmm.covar[k[i]][j][m] = s*s;
					}
					else {
						gmm.covar[k[i]][j][m] = 0.0;
					}
				}
			}
		}

    }
}

    