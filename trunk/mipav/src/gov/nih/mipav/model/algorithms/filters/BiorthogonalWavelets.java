package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.filters.AlgorithmRiceWaveletTools.EigenvalueComplex;
import gov.nih.mipav.model.algorithms.filters.AlgorithmRiceWaveletTools.EigenvalueComplexComparator;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

/** 
 * This code is ported from the MATLAB source code of BiorthogonalWavelets by Brian Moore
 * Below is the license for the original source code.
 */

/**
 * Copyright (c) 2012, Brian Moore
Copyright (c) 2004, Ben Barrowes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the distribution

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.\
*/

public  class BiorthogonalWavelets {
	
	/** D1MACH(4). */
    private double epsilon = Double.NaN;
    
    private double h[];
    
    private double ht[];
    
    private int inds_h[];
    
    private int inds_ht[];
    
    private double g[];
    
    private double gt[];
    
    private int inds_g[];
    
    private int inds_gt[];
    
    private void computeEpsilon() {
    	// epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        double neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)	
    }
	
	public void CDFWavelets(int l, int lt, boolean display) {
		int N;
		double h1[];
		int i;
		int j;
		double sqrt2;
		double val;
		int kk;
		int Nt;
		double ht1[];
		double val1;
		int K;
		double P[];
		int jj;
		double r[][];
		double realRoots[];
		double complexRoots[][];
		double C;
		double Ct;
		double prod;
		double A;
		double h2[];
		double a;
		double b;
		double u[];
		double ht2[];
		int lower;
		int upper;
		int res;
		double w[];
		double spacing;
		double HwReal[];
		double HwImag[];
		double HtwReal[];
		double HtwImag[];
		double GwReal[];
		double GwImag[];
		double GtwReal[];
		double GtwImag[];
		int ind;
		if (Double.isNaN(epsilon)) {
			computeEpsilon();
		}
	    // Compute left half of h
		N = 2 * l;
		h1 = new double[N+1];
		sqrt2 = Math.sqrt(2.0);
		val = sqrt2/Math.pow(2.0, N);
		for (i = 0; i < h1.length; i++) {
		    h1[i] = val;
		}
		for (kk = 0; kk <= N; kk++) {
			h1[kk] = h1[kk] * nchoosek(N, kk);
		}
		
		// Compute left half of ht
		Nt = 2 * lt;
		ht1 = new double[Nt+1];
		val1 = sqrt2/Math.pow(2.0, Nt);
		for (i = 0; i < ht1.length; i++) {
			ht1[i] = val1;
		}
		for (kk = 0; kk <= Nt; kk++) {
			ht1[kk] = ht1[kk] * nchoosek(Nt, kk);
		}
		
		// Compute right halves of h and ht
		K = l + lt;
		P = new double[K];
		for (jj = 0; jj <= K-1; jj++) {
			P[K-jj-1] = nchoosek(K-1+jj,jj);
		}
		r = cplxpair(roots(P));
		jj = 0;
		while (jj < r.length) {
			if (r[jj][1] == 0.0) {
				break;
			}
			jj = jj + 1;
		} // while (jj < r.length)
		
		if ((jj % 2) == 1) {
			MipavUtil.displayError("All complex roots were not conjugates");
		    return;
		}
		else if (jj == 0) {
			// All real roots
			realRoots = new double[r.length];
			for (i = 0; i < r.length; i++) {
				realRoots[i] = r[i][0];
			}
			complexRoots = null;
			C= 1;
			Ct = P[0];
		}
		else if (jj == r.length) {
			// All complex roots
			realRoots = null;
			complexRoots = new double[r.length][2];
			for (i = 0; i < r.length; i++) {
				complexRoots[i][0] = r[i][0];
				complexRoots[i][1] = r[i][1];
			}
			C = P[0];
			Ct = 1;
		}
		else {
			// Mix of real and complex roots
			realRoots = new double[r.length - jj];
			for (i = 0; i < r.length - jj; i++) {
				realRoots[i] = r[i + jj][0];
			}
			complexRoots = new double[jj][2];
			for (i = 0; i < jj; i++) {
				complexRoots[i][0] = r[i][0];
				complexRoots[i][1] = r[i][1];
			}
			prod = -r[jj][0];
			for (i = jj+1; i < r.length; i++) {
				prod = prod * (-r[i][0]);
			}
			A = 1.0/prod;
			C = P[0]/A;
			Ct = A;
		}
		
		h2 = new double[]{C};
		for (jj = 0; jj < complexRoots.length; jj += 2) {
		    a = complexRoots[jj][0];
		    b = -complexRoots[jj][1];
		    u = new double[]{0.0625,a/2-0.25,a*a+b*b-a+0.375,a/2-0.25,0.0625};
		    h2 = conv(u,h2);
		}
		
		ht2 = new double[]{Ct};
		for (jj = 0; jj < realRoots.length; jj++) {
			u = new double[]{-0.25,0.5-realRoots[jj],-0.25};
			ht2 = conv(u,ht2);
		}
		
		// Compute h and ht via convolution
		h = conv(h1,h2);
		ht = conv(ht1,ht2);
		
		lower = -(h.length - 1)/2;
		upper = (h.length - 1)/2;
		inds_h = new int[upper - lower + 1];
		for (i = 0; i < upper - lower + 1; i++) {
			inds_h[i] = lower + i;
		}
		
		lower = -(ht.length - 1)/2;
		upper = (ht.length - 1)/2;
		inds_ht = new int[upper - lower + 1];
		for (i = 0; i < upper - lower + 1; i++) {
			inds_ht[i] = lower + i;
		}
		
		// Compute g and gt
		inds_g = new int[inds_ht.length];
		for (i = 0; i < inds_ht.length; i++) {
			inds_g[i] = 1 - inds_ht[inds_ht.length-1-i];
		}
		
		inds_gt = new int[inds_h.length];
		for (i = 0; i < inds_h.length; i++) {
			inds_gt[i] = 1 - inds_h[inds_h.length-1-i];
		}
		
		g = new double[ht.length];
		for (i = 0; i < ht.length; i++) {
			g[i] = ht[ht.length-1-i] * (2 * (inds_ht[inds_ht.length-1-i] % 2) - 1);
		}
		
		gt = new double[h.length];
		for (i = 0; i < h.length; i++) {
			gt[i] = h[h.length-1-i] * (2 * (inds_h[inds_h.length-1-i] % 2) - 1);
		}
		
		// Plot the frequency responses if desired
		if (display) {
			Preferences.debug(realRoots.length + " real zeros\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug(complexRoots.length + " complex zeros\n", Preferences.DEBUG_ALGORITHM);
			
			res = 1000;
			w = new double[res];
			spacing = Math.PI/(res - 1.0);
			for (i = 0; i < res; i++) {
			    w[i] = i * spacing;	
			}
			
			HwReal = new double[res];
			HwImag = new double[res];
			for (i = 0; i < inds_h.length; i++) {
			    for (j = 0; j < res; j++) {
			        HwReal[j] = HwReal[j] + h[i]*Math.cos(inds_h[i] * w[j]);
			        HwImag[j] = HwImag[j] + h[i]*Math.sin(inds_h[i] * w[j]);
			    }
			}
			
			HtwReal = new double[res];
			HtwImag = new double[res];
			for (i = 0; i < inds_ht.length; i++) {
			    for (j = 0; j < res; j++) {
			        HtwReal[j] = HtwReal[j] + ht[i]*Math.cos(inds_ht[i] * w[j]);
			        HtwImag[j] = HtwImag[j] + ht[i]*Math.sin(inds_ht[i] * w[j]);
			    }
			}
		} // if (display)
	}
	
	private double[] conv(double u[], double v[]) {
	    int m = u.length;
	    int n = v.length;
	    double w[] = new double[m+n-1];
	    int j,k;
	    int lower;
	    int upper;
	    for (k = 0; k < m+n-1; k++) {
	        lower = Math.max(0,k-n+1);
	        upper = Math.min(k, m-1);
	        for (j = lower; j <= upper; j++) {
	        	w[k] += (u[j] * v[k-j]);
	        }
	    }
	    return w;
	}
	
	private double[][] roots(double p[]) {
		int i;
		int n = p.length- 1;
		double A[][] = new double[n][n];
		for (i = 0; i < n-1; i++) {
			A[i+1][i] = 1.0;
		}
		for (i = 0; i < n; i++) {
			A[0][i] = -p[i+1]/p[0];
		}
		// A = V * (diagonal eigenvalues) * V'
        // In EigevalueDecomposition the columns of V represent the eigenvectors
		 double eigenvalueR[] = new double[n];
	     double eI[] = new double[n];
	     double V[][] = new double[n][n];
	     Eigenvalue.decompose( A, V, eigenvalueR, eI );
	     double e[][] = new double[n][2];
	     for (i = 0; i < n; i++) {
	    	 e[i][0] = eigenvalueR[i];
	    	 e[i][1] = eI[i];
	     }
	     return e;
	}
	
	private double[][] cplxpair(double p[][]) {
		int i, j;
		double tolerance = 100.0 * epsilon;
		double realPart;
		double imagPart;
		double ratio;
		boolean isReal;
		double result[][] = new double[p.length][2];
		double minDist;
		double distance;
		double realPart2;
		double imagPart2;
		int conjugateIndex = -1;
		int resultIndex = 0;
		// Sort by increasing real parts
		// If real parts are equal, sort by increasing imaginary parts
		List<realImaginaryItem> list = new ArrayList<realImaginaryItem>();
		for (i = 0; i < p.length; i++) {
            list.add(new realImaginaryItem(p[i][0], p[i][1]));
        }
        Collections.sort(list, new realImaginaryComparator());
        List<Double>realList = new ArrayList<Double>();
        List<realImaginaryItem>complexList = new ArrayList<realImaginaryItem>();
        for (i = 0; i < list.size(); i++) {
            realPart = list.get(i).getReal();
            imagPart = list.get(i).getImaginary();
            if ((realPart == 0.0) && (imagPart == 0.0)) {
            	isReal = true;
            }
            else {
                ratio = Math.abs(imagPart)/(zabs(realPart,imagPart));
                if (ratio < tolerance) {
                	isReal = true;
                }
                else {
                	isReal = false;
                }
            }
            if (isReal) {
            	realList.add(realPart);
            }
            else {
            	complexList.add(new realImaginaryItem(realPart, imagPart));
            }
        } // for (i = 0; i < list.size(); i++)
        int unpaired = 0;
        boolean complexPaired[] = new boolean[complexList.size()];
        for (i = 0; i < complexList.size(); i++) {
        	if (!complexPaired[i]) {
        	    minDist = Double.MAX_VALUE;
        	    realPart = complexList.get(i).getReal();
    	        imagPart = complexList.get(i).getImaginary();
        	    for (j = i+1; j < complexList.size(); j++) {
        	    	if (!complexPaired[j]) {
	        	        realPart2 = complexList.get(j).getReal();
	        	        imagPart2 = complexList.get(j).getImaginary();
	        	        distance = zabs(realPart-realPart2,imagPart+imagPart2);
	        	        if (distance < minDist) {
	        	        	minDist = distance;
	        	        	conjugateIndex = j;
	        	        }
        	    	} // if (!complexPaired[j])
        	    } // for (j = i+1; j < complexList.size(); j++)
        	    if (minDist < tolerance) {
        	    	complexPaired[i] = true;
        	    	complexPaired[j] = true;
        	    	realPart2 = complexList.get(conjugateIndex).getReal();
        	        imagPart2 = complexList.get(conjugateIndex).getImaginary();
        	    	if (imagPart < 0.0) {
        	    	    result[resultIndex][0] = realPart;
        	    	    result[resultIndex++][1] = imagPart;
        	    	    result[resultIndex][0] = realPart;
        	    	    result[resultIndex++][1] = -imagPart;
        	    	}
        	    	else {
        	    		result[resultIndex][0] = realPart;
        	    	    result[resultIndex++][1] = -imagPart;
        	    	    result[resultIndex][0] = realPart;
        	    	    result[resultIndex++][1] = imagPart;	
        	    	}
        	    } // if (minDist < tolerance)
        	    else {
        	    	unpaired++;
        	    }
        	} // if (!complexPaired[i])
        } // for (i = 0; i < complexList.size(); i++)
        if (unpaired > 0) {
        	MipavUtil.displayError("cplxpair found " + unpaired + " complex numbers");
        }
        for (i = 0; i < realList.size(); i++) {
            result[resultIndex][0] = realList.get(i);
            result[resultIndex++][1] = 0.0;
        }
        return result;
	}
	
	/**
     * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
     * 
     * @param zr double
     * @param zi double
     * 
     * @return double
     */
    private double zabs(final double zr, final double zi) {
        double u, v, q, s;
        u = Math.abs(zr);
        v = Math.abs(zi);
        s = u + v;

        // s * 1.0 makes an unnormalized underflow on CDC machines into a true
        // floating zero
        s = s * 1.0;

        if (s == 0.0) {
            return 0.0;
        } else if (u > v) {
            q = v / u;

            return (u * Math.sqrt(1.0 + (q * q)));
        } else {
            q = u / v;

            return (v * Math.sqrt(1.0 + (q * q)));
        }
    }
	
	private class realImaginaryComparator implements Comparator<realImaginaryItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final realImaginaryItem o1, final realImaginaryItem o2) {
            final double a = o1.getReal();
            final double b = o2.getReal();
            final double c = o1.getImaginary();
            final double d = o2.getImaginary();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else if (c < d) {
            	return -1;
            } else if (c > d) {
            	return 1;
            } else {
                return 0;
            }
        }

    }
	
	private class realImaginaryItem {

        /** DOCUMENT ME! */
        private final double real;

        /** DOCUMENT ME! */
        private final double imaginary;

        /**
         * Creates a new realImaginaryItem object.
         * 
         * @param real
         * @param imaginary
         */
        public realImaginaryItem(final double real, final double imaginary) {
            this.real = real;
            this.imaginary = imaginary;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getReal() {
            return real;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getImaginary() {
            return imaginary;
        }

    }
	
	private long nchoosek(int n, int k) {
	    long ans = factorial(n)/(factorial(k) * factorial(n-k));
	    return ans;
	}
	
	/**
     * Returns the factorial of a nonnegative integer.
     *
     * @param   number  integer whose factorial is being returned
     *
     * @return  number!
     */
    public long factorial(int number) {
        long i, j;

        if (number < 0) {
            MipavUtil.displayError("A factorial cannot be performed on a negative number");

            return -1L;
        }

        if (number == 0) {
            return 1L;
        } else {

            for (i = 1, j = 1; i <= number; i++) {
                j = j * i;
            }

            return j;
        }
    }
}
