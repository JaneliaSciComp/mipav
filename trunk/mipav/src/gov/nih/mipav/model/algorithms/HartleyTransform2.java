package gov.nih.mipav.model.algorithms;


import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;

/***********************************************************
* Smithsonian Astrophysical Observatory
* Submillimeter Receiver Laboratory
* am
*
* transform.c                  S. Paine rev. 2018 February 5
*
*
* Discrete Fourier, Hartley, and Hilbert transforms.
************************************************************/

/*
 * Note that this port to Java only ports the Hartley functions
 * This file contains functions for fast Fourier, Hartley, and Hilbert
 * transforms, written with the aid of Joerg Arndt's excellent text,
 * "Matters Computational," online at http://www.jjj.de/fxt.  The FFT's
 * and FHT's here omit the usual reverse-binary permutations, since the
 * transform-domain operations in am (convolution and Hilbert transformation)
 * can be done on the permuted data.
 *
 * For transforms, the top level external functions are:
 *  fft_dif(z, n) - forward complex fft, bit reversed output
 *  ifft_dit(z, n) - inverse complex fft, bit reversed input
 *  fht_dif(x, n) - real fht, bit reversed output
 *  fht_dit(x, n) - real fht, bit reversed input
 *  hilbert(z, n) - computes Im(z) as the Hilbert transform of Re(z)
 * 
 * For testing, there are two reverse binary permutation functions here,
 * for complex and real arrays:
 *  bitrev_permute(z, n) - reverse binary permutation of complex array z
 *  bitrev_permute_real(x, n) - reverse binary permutation of real array x
 *
 * All of these functions assume n is a power of 2, without checking.
 *
 * The FFT and FHT functions use a combined recursive-iterative strategy,
 * described in (Singleton, R. C. 1967. "On Computing the Fast Fourier
 * Transform," Communications of the ACM 10:647.  See also M. Frigo
 * S. G. Johnson 2005. "The Design and Implementation of FFTW3," Proc.
 * IEEE 93:216.)
 *
 * Taking fft_dif() as an example, fft_dif() calls the recursive FFT
 * fft_dif_rec(), setting an initial branch count of 1 for the recursion.
 * fft_dif_rec() calls itself recursively (twice) to perform two half-
 * sized transforms, until the transform is small enough to be done entirely
 * in L1 cache, at which point it calls an iterative FFT, fft_dif_iter(),
 * instead.  The branch count is doubled at each recursion stage.  Under
 * OpenMP, if the environment variable OMP_NESTED=true, each recursive
 * call starts two new threads until the branch count exceeds the maximum
 * number of threads for a parallel region.
 *
 * There are two versions of each of the iterative FFT's and FHT's: a
 * conventional one (e.g. fft_dif_iter()) which minimizes trigonometric
 * computations at the expense of non-local memory access, and another
 * (e.g. fft_dif_iter_seq()) which accesses memory with unit stride.  As
 * Arndt points out in the text referenced above, on modern hardware the
 * unit-stride version can be significantly faster, despite the larger
 * number of arithmetic operations.  Tests on AMD Opteron and Intel
 * Pentium 4 showed this to be true down to transforms fitting in L2
 * cache, but for transforms fitting in L1 cache the non-local transforms
 * were faster.  Overall, the fastest strategy is to do recursive computation
 * down to the L1 cache size, then switch over to the non-local iterative
 * transform.
 */



public class HartleyTransform2 extends AlgorithmBase {
    private ModelImage transformImage;
    private ModelImage inverseImage;
    private static final int FHT_UNIT_STRIDE = 0;
    // L1 cache is closest to the core (each core has its own cache) and typically 
    // there are one for data and one for instructions, sizes are 8-64KB.
    private static final int L1_CACHE_BYTES = 8192;
    
    
    public HartleyTransform2() {
		
	}
    
    public void testHartley() {
    	double x[] = new double[32];
    	int i;
    	for (i = 0; i < 32; i++) {
    		x[i] = i;
    	}
    	fht_dif(x,32);
    	fht_dit(x,32);
    	for (i = 0; i < 32; i++) {
    		x[i] = x[i]/32;
    	}
    	for (i = 0; i < 32; i++) {
    		System.out.println("x["+i+"] = " + x[i]);
    	}
    	
    	for (i = 0; i < 32; i++) {
    		x[i] = i;
    	}
    	fht_dif(x, 32);
    	x = bitrevorder(x);
    	fht_dif(x, 32);
    	x = bitrevorder(x);
    	for (i = 0; i < 32; i++) {
    		x[i] = x[i]/32;
    	}
    	for (i = 0; i < 32; i++) {
    		System.out.println("x["+i+"] = " + x[i]);
    	}
    }
    
    public HartleyTransform2(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg) {
		super(null, srcImg);
		this.transformImage = transformImage;
		this.inverseImage = inverseImage;
	}
    
    public void fht2D(int yDim, int xDim, double src[][], double dst[][], boolean forwardTransform) {
    	int i, j;
    	double transT[][] = new double[xDim][yDim];
    	for (i = 0; i < yDim; i++) {
		    fht_dif(src[i], xDim);
		    src[i] = bitrevorder(src[i]);
		    if (!forwardTransform) {
		    	for (j = 0; j < xDim; j++) {
		    		src[i][j] = src[i][j]/xDim;
		    	}
		    }
		}
    	
    	for (i = 0; i < xDim; i++) {
			for (j = 0; j < yDim; j++) {
				transT[i][j] = src[j][i];
			}
		}
    	
    	for (i = 0; i < xDim; i++) {
    		fht_dif(transT[i], yDim);
    		transT[i] = bitrevorder(transT[i]);
    		if (!forwardTransform) {
    			for (j = 0; j < yDim; j++) {
    				transT[i][j] = transT[i][j]/yDim;
    			}
    		}
    	}
    	
    	for (i  = 0; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				dst[i][j] = transT[j][i];
			}
		}
    }
    
    /***********************************************************
    * void fht_dif(double *x, unsigned long n)
    *
    * Purpose:
    *   Computes the discrete Hartley transform of a real sequence
    *   x[0..n-1], using a radix 2 decimation-in-frequency FHT.
    *   n must be a power of 2.  Entering this function, x[] is
    *   in normal order.  On return, x[] contains the Hartley
    *   transform, stored in bit-reversed order.
    *
    * Arguments:
    *   double *x - array of n doubles, representing n real numbers
    *   unsigned long n - dimension of x, must be a power of 2
    *
    * Return:
    *   none
    ************************************************************/

    public void fht_dif(double x[], int n)
    {
        fht_dif_rec(x, n, 1);
        return;
    }   /* fht_dif() */

    /***********************************************************
    * static void fht_dif_iter(double *x, unsigned long n)
    *
    * Purpose:
    *   Computes the discrete Hartley transform of a real sequence
    *   x[0..n-1], using an iterative radix 2 decimation-in-frequency
    *   FHT.  n must be a power of 2.  Entering this function, x[] is
    *   in normal order.  On return, x[] contains the Hartley
    *   transform, stored in bit-reversed order.
    *
    * Arguments:
    *   double *x - array of n doubles, representing n real numbers
    *   unsigned long n - dimension of x, must be a power of 2
    *
    * Return:
    *   none
    ************************************************************/

    static void fht_dif_iter(double x[], int n)
    {
        int m;

        for (m = n; m > 1; m >>= 1) {
            double a, b, c, s, t;
            int i, j, k, mh, mq;
            mh = m >> 1;
            mq = mh >> 1;
            t = Math.PI / (double)mh;
            a = Math.sin(0.5 * t);
            a *= 2.0 * a;
            b = Math.sin(t);
            for (i = 0; i < n; i += m) {
                for (j = 0, k = mh; j < mh; ++j, ++k) {
                    double u, v;
                    u = x[i + j];
                    v = x[i + k];
                    x[i + j] = u + v;
                    x[i + k] = u - v;
                }
            }
            c = 1.0;
            s = 0.0;
            for (j = 1, k = mh - 1; j < mq; ++j, --k) {
                double tmp;
                tmp = c;
                c -= a * c + b * s;
                s -= a * s - b * tmp;
                for (i = 0; i < n; i += m) {
                    double u, v;
                    u = x[j + mh + i];
                    v = x[k + mh + i];
                    x[j + mh + i] = u * c + v * s;
                    x[k + mh + i] = u * s - v * c;
                }
            }
        }
        return;
    }   /* fht_dif_iter() */
    
    /***********************************************************
    * static void fht_dif_iter_seq(double *x, unsigned long n)
    *
    * Purpose:
    *   Computes the discrete Hartley transform of a real sequence
    *   x[0..n-1], using an iterative radix 2 decimation-in-frequency
    *   FHT.  n must be a power of 2.  Entering this function, x[] is
    *   in normal order.  On return, x[] contains the Hartley
    *   transform, stored in bit-reversed order.
    *
    *   The two inner loops of the FHT computation are ordered
    *   to favor sequential memory access at the expense of
    *   redundant trig computations.  See J. Arndt, "Algorithms
    *   for Programmers," online at http://www.jjj.de/fxt/.
    *
    * Arguments:
    *   double *x - array of n doubles, representing n real numbers
    *   unsigned long n - dimension of x, must be a power of 2
    *
    * Return:
    *   none
    ************************************************************/

    static void fht_dif_iter_seq(double x[], int n)
    {
        int m;

        for (m = n; m > 1; m >>= 1) {
            double a, b, t;
            int i, mh, mq;
            mh = m >> 1;
            mq = mh >> 1;
            t = Math.PI / (double)mh;
            a = Math.sin(0.5 * t);
            a *= 2.0 * a;
            b = Math.sin(t);
            for (i = 0; i < n; i += m) {
                double c, s;
                int xp;
                int j, k;
                xp = i;
                for (j = 0, k = mh; j < mh; ++j, ++k) {
                    double u, v;
                    u = x[xp + j];
                    v = x[xp + k];
                    x[xp + j] = u + v;
                    x[xp + k] = u - v;
                }
                xp += mh;
                c = 1.0;
                s = 0.0;
                for (j = 1, k = mh - 1; j < mq; ++j, --k) {
                    double u, v, tmp;
                    tmp = c;
                    c -= a * c + b * s;
                    s -= a * s - b * tmp;
                    u = x[xp + j];
                    v = x[xp + k];
                    x[xp + j] = u * c + v * s;
                    x[xp + k] = u * s - v * c;
                }
            }
        }
        return;
    }   /* fht_dif_iter_seq() */

    /***********************************************************
    * static void fht_dif_rec(double *x, unsigned long n, int nbranch)
    *
    * Purpose:
    *   Computes the discrete Hartley transform of a real sequence
    *   x[0..n-1], using a radix 2 decimation-in-frequency FHT.  If
    *   the computation is small enough to fit in cache, it is done
    *   iteratively.  Otherwise, it is done recursively until the
    *   recursion descends to cache-sized chunks.
    *
    *   n must be a power of 2.  Entering this function, x[] must
    *   be in normal order.  On return, x[] contains the Hartley
    *   transform, in bit-reversed order.
    *
    *   To support OpenMP parallelism, nbranch keeps track of the
    *   number of active transforms at a given recursion level. On
    *   the first call to this function, nbranch should be 1.  It
    *   is then doubled for each recursion.
    *
    * Arguments:
    *   double *x - array of n doubles, representing n real numbers
    *   unsigned long n - dimension of x, must be a power of 2
    *   int nbranch - number of transforms at this recursion level
    *
    * Return:
    *   none
    ************************************************************/

    static void fht_dif_rec(double x[], int n, int nbranch)
    {
        double a, b, c, s, t;
        int j, jmax, k, nh, nq;

        if (n == 1)
            return;
        if (n <= L1_CACHE_BYTES/8) {
            if (FHT_UNIT_STRIDE > 0)
                fht_dif_iter_seq(x, n);
            else
                fht_dif_iter(x, n);
            return;
        }
        nh = n >> 1;
        nq = nh >> 1;
        t = Math.PI / (double)nh;
        a = Math.sin(0.5 * t);
        a *= 2.0 * a;
        b = Math.sin(t);
        for (j = 0, k = nh; j < nh; ++j, ++k) {
            double u, v;
            u = x[j];
            v = x[k];
            x[j] = u + v;
            x[k] = u - v;
        }
        c = 1.0;
        s = 0.0;
        jmax = nq + nh;
        for (j = nh + 1, k = n - 1; j < jmax; ++j, --k) {
            double u, v, tmp;
            tmp = c;
            c -= a * c + b * s;
            s -= a * s - b * tmp;
            u = x[j];
            v = x[k];
            x[j] = u * c + v * s;
            x[k] = u * s - v * c;
        }
        nbranch <<= 1;
        //#if (_OPENMP >= 200203)
        //#pragma omp parallel sections if (nbranch <= omp_get_max_threads()) num_threads(2)
        //#endif
        //{
           // #if (_OPENMP >= 200203)
            //#pragma omp section
            //#endif
            fht_dif_rec(x, nh, nbranch);
            //#if (_OPENMP >= 200203)
           // #pragma omp section
            //#endif
            double x2[] = new double[nh];
            for (j = 0; j < nh; j++) {
            	x2[j] = x[j + nh];
            }
            fht_dif_rec(x2, nh, nbranch);
            for (j = 0; j < nh; j++) {
            	x[j + nh] = x2[j];
            }
        //}
        return;
    }   /* fht_dif_rec() */
    
    /***********************************************************
    * void fht_dit(double *x, unsigned long n)
    *
    * Purpose:
    *   Computes the discrete Hartley transform of a real sequence
    *   x[0..n-1], using a radix 2 decimation-in-time FHT.
    *   n must be a power of 2.  Entering this function, x[]
    *   must be in bit-reversed order.  On return, x[] contains
    *   the Hartley transform, returned to normal order.
    *
    * Arguments:
    *   double *x - array of n doubles, representing n real numbers
    *   unsigned long n - dimension of x, must be a power of 2
    *
    * Return:
    *   none
    ************************************************************/

    public void fht_dit(double x[], int n)
    {
        fht_dit_rec(x, n, 1);
        return;
    }   /* fht_dit() */

    /***********************************************************
    * static void fht_dit_iter(double *x, unsigned long n)
    *
    * Purpose:
    *   Computes the discrete Hartley transform of a real sequence
    *   x[0..n-1], using an iterative radix 2 decimation-in-time
    *   FHT.  n must be a power of 2.  Entering this function, x[]
    *   must be in bit-reversed order.  On return, x[] contains the
    *   Hartley transform, returned to normal order.
    *
    * Arguments:
    *   double *x - array of n doubles, representing n real numbers
    *   unsigned long n - dimension of x, must be a power of 2
    *
    * Return:
    *   none
    ************************************************************/

    static void fht_dit_iter(double x[], int n)
    {
        int m;

        for (m = 2; m <= n; m <<= 1) {
            double a, b, c, s, t;
            int i, j, k, mh, mq;
            mh = m >> 1;
            mq = mh >> 1;
            t = Math.PI / (double)mh;
            a = Math.sin(0.5 * t);
            a *= 2.0 * a;
            b = Math.sin(t);
            c = 1.0;
            s = 0.0;
            for (j = 1, k = mh - 1; j < mq; ++j, --k) {
                double tmp;
                tmp = c;
                c -= a * c + b * s;
                s -= a * s - b * tmp;
                for (i = 0; i < n; i += m) {
                    double u, v;
                    u = x[j + mh + i];
                    v = x[k + mh + i];
                    x[j + mh + i] = u * c + v * s;
                    x[k + mh  + i] = u * s - v * c;
                }
            }
            for (i = 0; i < n; i += m) {
                for (j = 0, k = mh; j < mh; ++j, ++k) {
                    double u, v;
                    u = x[i + j];
                    v = x[i + k];
                    x[i + j] = u + v;
                    x[i + k] = u - v;
                }
            }
        }
        return;
    }   /* fht_dit_iter() */
    
    /***********************************************************
    * static void fht_dit_iter_seq(double *x, unsigned long n)
    *
    * Purpose:
    *   Computes the discrete Hartley transform of a real sequence
    *   x[0..n-1], using an iterative radix 2 decimation-in-time
    *   FHT.  n must be a power of 2.  Entering this function, x[]
    *   must be in bit-reversed order.  On return, x[] contains the
    *   Hartley transform, returned to normal order.
    *
    *   The two inner loops of the FHT computation are ordered
    *   to favor sequential memory access at the expense of
    *   redundant trig computations.  See J. Arndt, "Algorithms
    *   for Programmers," online at http://www.jjj.de/fxt/.
    *
    * Arguments:
    *   double *x - array of n doubles, representing n real numbers
    *   unsigned long n - dimension of x, must be a power of 2
    *
    * Return:
    *   none
    ************************************************************/

    static void fht_dit_iter_seq(double x[], int n)
    {
        int m;

        for (m = 2; m <= n; m <<= 1) {
            double a, b, t;
            int i, mh, mq;
            mh = m >> 1;
            mq = mh >> 1;
            t = Math.PI / (double)mh;
            a = Math.sin(0.5 * t);
            a *= 2.0 * a;
            b = Math.sin(t);
            for (i = 0; i < n; i += m) {
                double c, s;
                int xp;
                int j, k;
                xp = i + mh;
                c = 1.0;
                s = 0.0;
                for (j = 1, k = mh - 1; j < mq; ++j, --k) {
                    double tmp, u, v;
                    tmp = c;
                    c -= a * c + b * s;
                    s -= a * s - b * tmp;
                    u = x[xp + j];
                    v = x[xp + k];
                    x[xp + j] = u * c + v * s;
                    x[xp + k] = u * s - v * c;
                }
                xp -= mh;
                for (j = 0, k = mh; j < mh; ++j, ++k) {
                    double u, v;
                    u = x[xp + j];
                    v = x[xp + k];
                    x[xp + j] = u + v;
                    x[xp + k] = u - v;
                }
            }
        }
        return;
    }   /* fht_dit_iter_seq() */
    
    /***********************************************************
    * static void fht_dit_rec(double *x, unsigned long n, int nbranch)
    *
    * Purpose:
    *   Computes the discrete Hartley transform of a real sequence
    *   x[0..n-1], using a radix 2 decimation-in-time FHT.  If
    *   the computation is small enough to fit in cache, it is done
    *   iteratively.  Otherwise, it is done recursively until the
    *   recursion descends to cache-sized chunks.
    *
    *   n must be a power of 2.  Entering this function, x[] must
    *   be in bit-reversed order.  On return, x[] contains the
    *   Hartley transform, returned to normal order.
    *
    *   To support OpenMP parallelism, nbranch keeps track of the
    *   number of active transforms at a given recursion level. On
    *   the first call to this function, nbranch should be 1.  It
    *   is then doubled for each recursion.
    *
    * Arguments:
    *   double *x - array of n doubles, representing n real numbers
    *   unsigned long n - dimension of x, must be a power of 2
    *   int nbranch - number of transforms at this recursion level
    *
    * Return:
    *   none
    ************************************************************/

    static void fht_dit_rec(double x[], int n, int nbranch)
    {
        double a, b, c, s, t;
        int j, jmax, k, nh, nq;

        if (n == 1)
            return;
        if (n <= L1_CACHE_BYTES/8) {
            if (FHT_UNIT_STRIDE > 0)
                fht_dit_iter_seq(x, n);
            else
                fht_dit_iter(x, n);
            return;
        }
        nh = n >> 1;
        nq = nh >> 1;
        nbranch <<= 1;
        //#if (_OPENMP >= 200203)
        //#pragma omp parallel sections if (nbranch <= omp_get_max_threads()) num_threads(2)
        //#endif
        //{
            //#if (_OPENMP >= 200203)
            //#pragma omp section
            //#endif
            fht_dit_rec(x, nh, nbranch);
            //#if (_OPENMP >= 200203)
            //#pragma omp section
            //#endif
            double x2[] = new double[nh];
            for (j = 0; j < nh; j++) {
            	x2[j] = x[j + nh];
            }
            fht_dit_rec(x2, nh, nbranch);
            for (j = 0; j < nh; j++) {
            	x[j + nh] = x2[j];
            }
        //}
        t = Math.PI / (double)nh;
        a = Math.sin(0.5 * t);
        a *= 2.0 * a;
        b = Math.sin(t);
        jmax = nq + nh;
        c = 1.0;
        s = 0.0;
        for (j = nh + 1, k = n - 1; j < jmax; ++j, --k) {
            double tmp, u, v;
            tmp = c;
            c -= a * c + b * s;
            s -= a * s - b * tmp;
            u = x[j];
            v = x[k];
            x[j] = u * c + v * s;
            x[k] = u * s - v * c;
        }
        for (j = 0, k = nh; j < nh; ++j, ++k) {
            double u, v;
            u = x[j];
            v = x[k];
            x[j] = u + v;
            x[k] = u - v;
        }
        return;
    }   /* fht_dit_rec() */

    // ------------------------------------------------------
 	//  Function for bit reversal
 	// ------------------------------------------------------
 	private double[] bitrevorder(double X[]) {
 	    // Rearrange vector X to reverse bit order,upto max 2^k size <= length(X)
 		int N;
 		double R[];
 		int NTest;
 		int log2;
 		int i;
 		int j;
 		N = X.length;
 		R = new double[N];
 		NTest = N;
 		log2 = 0;
 		int biti;
 		int bitReversed = 0;
 		while ((NTest % 2) == 0) {
 	    	NTest = NTest/2;
 	    	log2++;
 	    }
 		for (j = 0; j < N; j++) {
 			bitReversed = 0;
 			for (i = 0; i <= log2-1; i++) {
 				biti = (j >>> i) & 0x1;
 				bitReversed = bitReversed | (biti << log2 - 1 - i);
 			}
 		    R[j] = X[bitReversed];
 		}
 		return R;
 	}

	
	public void runAlgorithm() {
		int xDim;
		int yDim;
		int zDim;
		int length;
		double doubleBuffer[];
		int xTest;
		int yTest;
		int z;
		double src[][];
        double dst[][];
        int x;
        int y;
		xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        doubleBuffer = new double[length];
        zDim = 1;
        if (srcImage.getNDims() > 2) {
        	zDim = srcImage.getExtents()[2];
        }
         
        xTest = xDim;
        while ((xTest % 2) == 0) {
        	xTest = xTest/2;
        }
        if (xTest != 1) {
        	MipavUtil.displayError("X dimension not a power of 2");
        	setCompleted(false);
        	return;	
        }
        yTest = yDim;
        while ((yTest % 2) == 0) {
        	yTest = yTest/2;
        }
        if (yTest != 1) {
        	MipavUtil.displayError("Y dimension not a power of 2");
        	setCompleted(false);
        	return;	
        }
        src = new double[yDim][xDim];
        dst = new double[yDim][xDim];
        for (z = 0; z < zDim; z++) {
        	try {
                srcImage.exportData(z * length, length, doubleBuffer); // locks and releases lock
            } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Hartley Transform: Image(s) locked", true);

                return;
            }
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			src[y][x] = doubleBuffer[x + y * xDim];
        			dst[y][x] = 0;
        		}
        	}
        	fht2D(yDim, xDim, src, dst, true);
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			doubleBuffer[x + y * xDim] = dst[y][x];
        			src[y][x] = 0;
        		}
        	}
        	try {
                transformImage.importData(z*length, doubleBuffer, false);
             } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Discrete Sine Transform: Image(s) locked", true);

                return;
             }
        	// Inverse transform
        	fht2D(yDim, xDim, dst, src, false);
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			doubleBuffer[x + y * xDim] = src[y][x];
        		}
        	}
        	try {
                inverseImage.importData(z*length, doubleBuffer, false);
             } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Hartley Transform: Image(s) locked", true);

                return;
             }
        }
        transformImage.calcMinMax();
        inverseImage.calcMinMax();
        setCompleted(true);
        return;
	}
	
	

}

