package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;

public class AlgorithmHilbertTransform extends AlgorithmBase {
	/**
	 * The Hilbert tramsform and the other needed supporting routines are ported from the file transform.c written by
	 * Scott Paine of the Smithsonian Astrophysical Observatory, Submillimeter Receiver Laboratory on February 2, 2010.
	 * Scott Paine wrote these routines with the add of Joerg Arndt's excellent text, "Matters Computational," online
	 * at http://www.jjj.de./fxt
	 * 
	 *  All of the functions assume n is a power of 2, without checking
	 *  
	 *  Purpose:
     *   Given a real sequence, initially stored in the complex array z,
     *   this function computes the corresponding analytic sequence.  The
     *   imaginary part is the Hilbert transform of the real part.

	 */
	
	// Array of 2n doubles, representing complex numbers
	// Real elements are stored in z[0, 2, ..., 2n-2] and
	// imaginary elements are stored in z[1,3,...,2n-1]
	// Note that there are 2 different definitions of Hilbert transform in which the results differ by a 
	// multiplicative factor of -1.
	private double z[];
	
	// Dimension of z, must be a power of 2
	private int n;
	
	public AlgorithmHilbertTransform(double z[], int n) {
	     this.z = z;
	     this.n = n;
	}
	
	public void runAlgorithm() {
		double x;
		int i;
	    long n2;

	    n2 = n << 1;
	    /*
	     * Compute the (bit-reversed) Fourier transform of z.
	     */
	    fft_dif(z, n);
	    /*
	     * Form the transform of the analytic sequence by zeroing
	     * the transform for negative time, except for the (N/2)th.
	     * element.  Since z is now in bit-reversed order, this means
	     * zeroing every other complex element.  The array indices of
	     * the elements to be zeroed are 6,7,10,11...etc. (The real
	     * and imaginary parts of the (N/2)th element are in z[2] and
	     * z[3], respectively.)
	     */
	    for (i = 6; i < n2; i += 4) {
	        z[i] = 0.;
	        z[i+1] = 0.;
	    }
	    /*
	     * The 0th and (N/2)th elements get multiplied by 0.5.  Test
	     * for the trivial 1-point transform, just in case.
	     */
	    z[0] *= 0.5;
	    z[1] *= 0.5;
	    if (n > 1) {
	        z[2] *= 0.5;
	        z[3] *= 0.5;
	    }
	    /*
	     * Compute the inverse transform.
	     */
	    ifft_dit(z, n);
	    /*
	     * Normalize the array.  The factor of 2 is left over from
	     * forming the transform in the time domain.
	     */
	    x = 2. / (double)n;
	    for (i = 0; i < n2; ++i)
	        z[i] *= x;
	    return;
	
	}
	
	/***********************************************************
	* void fft_dif(double z[], int n)
	*
	* Purpose:
	*   Computes the forward discrete Fourier transform of a complex
	*   sequence z, using a radix 2 decimation-in-frequency FFT.
	*   z[0..2n-1] is an array of n complex numbers, stored in the
	*   usual way with real elements in z[0,2,..2n-2] and imaginary
	*   elements in z[1,3,..2n-1].
	*
	*   Entering this function, z[] should be in normal order.  On
	*   return, the FT is stored in bit-reversed order.
	*
	*   n must be a power of 2.
	*
	* Arguments:
	*   double z[] - array of 2n doubles, representing n complex numbers
	*   int n - dimension of z, must be a power of 2
	*
	* Return:
	*   none
	************************************************************/

	private void fft_dif(double z[], int n)
	{
	    fft_dif_rec(z, n, 1);
	    return;
	}   /* fft_dif() */

	
	/***********************************************************
	* static void fft_dif_rec(double z[], int n, int nbranch)
	*
	* Purpose:
	*   Computes the forward discrete Fourier transform of a complex
	*   sequence z, using a radix 2 decimation-in-frequency FFT.
	*   If the computation is small enough to fit in cache, it is
	*   done iteratively.  Otherwise, it is done recursively until
	*   the recursion descends to cache-sized chunks.
	*
	*   z[0..2n-1] is an array of n complex numbers, stored in the
	*   usual way with real elements in z[0,2,..2n-2] and imaginary
	*   elements in z[1,3,..2n-1].
	*
	*   Entering this function, z[] should be in normal order.  On
	*   return, the FT is stored in bit-reversed order.
	*
	*   n must be a power of 2.

	*   To support OpenMP parallelism, nbranch keeps track of the
	*   number of active transforms at a given recursion level. On
	*   the first call to this function, nbranch should be 1.  It
	*   is then doubled for each recursion.
	*
	* Arguments:
	*   double z[] - array of 2n doubles, representing n complex numbers
	*   int n - dimension of z, must be a power of 2
	*   int nbranch - number of transforms at this recursion level
	*
	* Return:
	*   none
	************************************************************/

	private void fft_dif_rec(double z[], int n, int nbranch)
	{
	    double a, b, c, s, t;
	    int nh;
	    int kr;
	    int i;

	    if (n == 1)
	        return;
	    if (n < 4) {
	    	fft_dif_iter(z,n);
	    	return;
	    }
	    t = (2.0 * Math.PI) / (double)n;
	    a = Math.sin(0.5 * t);
	    a *= 2.0 * a;
	    b = Math.sin(t);
	    c = 1.0;
	    s = 0.0;
	    for (kr = 0; kr < n; kr += 2) {
	        double ur, ui, tmp;
	        int ki, mr, mi;
	        ki = kr + 1;
	        mr = kr + n;
	        mi = mr + 1;
	        ur = z[kr];
	        ui = z[ki];
	        z[kr] = ur + z[mr];
	        z[ki] = ui + z[mi];
	        ur -= z[mr];
	        ui -= z[mi];
	        z[mr] = ur * c - ui * s;
	        z[mi] = ur * s + ui * c;
	        tmp = c;
	        c -= a * c + b * s;
	        s -= a * s - b * tmp;
	    }
	    nh = n >> 1;
	    nbranch <<= 1;
	    double z2[] = new double[2*nh];
	    for (i = 0; i < 2*nh; i++) {
	    	z2[i] = z[i + n];
	    }
	    {
	        fft_dif_rec(z, nh, nbranch);
	        fft_dif_rec(z2, nh, nbranch);
	        for (i = 0; i < 2*nh; i++) {
		    	z[i + n] = z2[i];
	        }
	    }
	    return;
	}   /* fft_dif_rec() */
	
	/***********************************************************
	* static void fft_dif_iter(double *z, unsigned long n)
	*
	* Purpose:
	*   Computes the forward discrete Fourier transform of a complex
	*   sequence z, using an iterative radix 2 decimation-in-frequency
	*   FFT.  z[0..2n-1] is an array of n complex numbers, stored in the
	*   usual way with real elements in z[0,2,..2n-2] and imaginary
	*   elements in z[1,3,..2n-1].
	*
	*   Entering this function, z[] should be in normal order.  On
	*   return, the FT is stored in bit-reversed order.
	*
	*   n must be a power of 2.
	*
	* Arguments:
	*   double *z - array of 2n doubles, representing n complex numbers
	*   unsigned long n - dimension of z, must be a power of 2
	*
	* Return:
	*   none
	************************************************************/

	private void fft_dif_iter(double z[], int n)
	{
	    int i, n2;

	    n2 = n << 1;
	    for (i = n; i > 1; i >>= 1) {
	        double a, b, c, s, t;
	        int i2, j;
	        i2 = i << 1;
	        t = 2.0 * Math.PI / (double)i;
	        a = Math.sin(0.5 * t);
	        a *= 2.0 * a;
	        b = Math.sin(t);
	        c = 1.0;
	        s = 0.0;
	        for (j = 0; j < i; j += 2) {
	            double tmp;
	            int kr, kmax;
	            kmax = n2 + j;
	            for (kr = j; kr < kmax; kr += i2) {
	                double ur, ui;
	                int ki, mr, mi;
	                ki = kr + 1;
	                mr = kr + i;
	                mi = mr + 1;
	                ur = z[kr];
	                ui = z[ki];
	                z[kr] = ur + z[mr];
	                z[ki] = ui + z[mi];
	                ur -= z[mr];
	                ui -= z[mi];
	                z[mr] = ur * c - ui * s;
	                z[mi] = ur * s + ui * c;
	            }
	            tmp = c;
	            c -= a * c + b * s;
	            s -= a * s - b * tmp;
	        }
	    }
	    return;
	}   /* fft_dif_iter() */


	
	/***********************************************************
	* void ifft_dit(double z[], int n)
	*
	* Purpose:
	*   Computes the inverse discrete Fourier transform of a complex
	*   sequence z, using a radix 2 decimation-in-time FFT.
	*   z[0..2n-1] is an array of n complex numbers, stored in the
	*   usual way with real elements in z[0,2,..2n-2] and imaginary
	*   elements in z[1,3,..2n-1].
	*
	*   Entering this function, z[] should be in bit-reversed order.
	*   The returned inverse FT is restored to normal order.
	*
	*   n must be a power of 2.
	*
	* Arguments:
	*   double z[] - array of 2n doubles, representing n complex numbers
	*   int n - dimension of z, must be a power of 2
	*
	* Return:
	*   none
	************************************************************/

	private void ifft_dit(double z[], int n)
	{
	    ifft_dit_rec(z, n, 1);
	    return;
	}   /* ifft_dit() */


	/***********************************************************
	* static void ifft_dit_rec(double z[], int n, int nbranch)
	*
	* Purpose:
	*   Computes the inverse discrete Fourier transform of a complex
	*   sequence z, using a radix 2 decimation-in-time FFT. If the
	*   computation is small enough to fit in cache, it is done
	*   iteratively.  Otherwise, it is done recursively until the
	*   recursion descends to cache-sized chunks.
	*
	*   z[0..2n-1] is an array of n complex numbers, stored in the
	*   usual way with real elements in z[0,2,..2n-2] and imaginary
	*   elements in z[1,3,..2n-1].
	*
	*   Entering this function, z[] should be in normal order.  On
	*   return, the FT is stored in bit-reversed order.
	*
	*   n must be a power of 2.
	*
	*   To support OpenMP parallelism, nbranch keeps track of the
	*   number of active transforms at a given recursion level. On
	*   the first call to this function, nbranch should be 1.  It
	*   is then doubled for each recursion.
	*
	* Arguments:
	*   double z[] - array of 2n doubles, representing n complex numbers
	*   int n - dimension of z, must be a power of 2
	*   int nbranch - number of transforms at this recursion level
	*
	* Return:
	*   none
	************************************************************/

	private void ifft_dit_rec(double z[], int n, int nbranch)
	{
	    double a, b, c, s, t;
	    int  nh, kr;
	    int i;

	    if (n == 1)
	        return;
	    if (n < 4) {
	    	ifft_dit_iter(z, n);
	    	return;
	    }
	    nh = n >> 1;
	    nbranch <<= 1;
	    double z2[] = new double[2*nh];
	    for (i = 0; i < 2*nh; i++) {
	    	z2[i] = z[i + n];
	    }
	    {
	        ifft_dit_rec(z, nh, nbranch);
	        ifft_dit_rec(z2, nh, nbranch);
	        for (i = 0; i < 2*nh; i++) {
		    	z[i+n] = z2[i];
	        }
	    }
	    t = -(2.0 * Math.PI) / (double)n;
	    a = Math.sin(0.5 * t);
	    a *= 2.0 * a;
	    b = Math.sin(t);
	    c = 1.0;
	    s = 0.0;
	    for (kr = 0; kr < n; kr += 2) {
	        double vr, vi, tmp;
	        int ki, mr, mi;
	        ki = kr + 1;
	        mr = kr + n;
	        mi = mr + 1;
	        vr = z[mr] * c - z[mi] * s;
	        vi = z[mr] * s + z[mi] * c;
	        z[mr] = z[kr] - vr;
	        z[mi] = z[ki] - vi;
	        z[kr] += vr;
	        z[ki] += vi;
	        tmp = c;
	        c -= a * c + b * s;
	        s -= a * s - b * tmp;
	    }
	    return;
	}   /* ifft_dit_rec() */
	
	/***********************************************************
	* static void ifft_dit_iter(double *z, unsigned long n)
	*
	* Purpose:
	*   Computes the inverse discrete Fourier transform of a complex
	*   sequence z, using an iterative radix 2 decimation-in-time FFT.
	*   z[0..2n-1] is an array of n complex numbers, stored in the
	*   usual way with real elements in z[0,2,..2n-2] and imaginary
	*   elements in z[1,3,..2n-1].
	*
	*   Entering this function, z[] should be in bit-reversed order.
	*   The returned inverse FT is restored to normal order.
	*
	*   n must be a power of 2.
	*
	* Arguments:
	*   double *z - array of 2n doubles, representing n complex numbers
	*   unsigned long n - dimension of z, must be a power of 2
	*
	* Return:
	*   none
	************************************************************/

	private void ifft_dit_iter(double z[], int n)
	{
	    int i, n2;

	    n2 = n << 1;
	    for (i = 2; i <= n; i <<= 1) {
	        double a, b, c, s, t;
	        int i2, j;
	        i2 = i << 1;
	        t = -2.0*Math.PI / (double)i;
	        a = Math.sin(0.5 * t);
	        a *= 2.0 * a;
	        b = Math.sin(t);
	        c = 1.0;
	        s = 0.0;
	        for (j = 0; j < i; j += 2) {
	            double tmp;
	            int kr, kmax;
	            kmax = n2 + j;
	            for (kr = j; kr < kmax; kr += i2) {
	                double vr, vi;
	                int ki, mr, mi;
	                ki = kr + 1;
	                mr = kr + i;
	                mi = mr + 1;
	                vr = z[mr] * c - z[mi] * s;
	                vi = z[mr] * s + z[mi] * c;
	                z[mr] = z[kr] - vr;
	                z[mi] = z[ki] - vi;
	                z[kr] += vr;
	                z[ki] += vi;
	            }
	            tmp = c;
	            c -= a * c + b * s;
	            s -= a * s - b * tmp;
	        }
	    }
	    return;
	}   /* ifft_dit_iter() */


	
}