package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import java.io.*;

/** Noise estimator class for MCVolumes, Volumes and slices...
 *
 */
public class NoiseEstimate
{
	/** NoiseEstimate constructor.
	 *
	 */
	public NoiseEstimate() {
	}

	/** Calculate the noise estimate via the histogram method.
	 */
	public float histogramMethod(MCVolume volume) 
	{
		Histogram hh = new Histogram();
		int ii = 0;

		//
		//  Compute the thresholded histogram.
		//
		float lower_threshold = (float)0.01;
		float upper_threshold = (float)0.3 * (float)Statistics.max(volume);
		hh.histogram( volume, lower_threshold, upper_threshold );

		//
		//  Get the counts and bins.
		//
		float[] bins = hh.getBins();
		float[] counts = hh.getCounts();

		//
		//  Find the maximum count.
		//
		float max = (float)0.0; int max_index = 0;
		for(ii=0; ii<counts.length; ii++) {
			if( counts[ii] > max ) {
				max_index = ii;
				max = counts[ii];
			}
		}

		//
		//  Find the first minimum after the maximum
		//
		ii = max_index + 1;
		do {
			ii++;
		} while( counts[ii] < counts[ii-1] && ( ii < bins.length ) );

		float sigma = (float)0.0;
		if( ii >= bins.length-1 ) {
			System.out.println("Gack...could not find the first minimum.");
		}
		else {
			sigma = bins[ii];
		}

		return sigma;
	}

	/** Calculate the noise estimate via the histogram method.
	 */
	public float histogramMethod(Volume volume) 
	{
		Histogram hh = new Histogram();
		int ii = 0;

		//
		//  Compute the thresholded histogram.
		//
		float lower_threshold = (float)0.01;
		float upper_threshold = (float)0.3 * (float)Statistics.max(volume);
		hh.histogram( volume, lower_threshold, upper_threshold );

		//
		//  Get the counts and bins.
		//
		float[] bins = hh.getBins();
		float[] counts = hh.getCounts();

		//
		//  Find the maximum count.
		//
		float max = (float)0.0; int max_index = 0;
		for(ii=0; ii<counts.length; ii++) {
			if( counts[ii] > max ) {
				max_index = ii;
				max = counts[ii];
			}
		}

		//
		//  Find the first minimum after the maximum
		//
		ii = max_index + 1;
		do {
			ii++;
		} while( counts[ii] < counts[ii-1] && ( ii < bins.length ) );

		float sigma = (float)0.0;
		if( ii >= bins.length-1 ) {
			System.out.println("Gack...could not find the first minimum.");
		}
		else {
			sigma = bins[ii];
		}

		return sigma;
	}

	/** Noise estimation via the histogram method on a single slice.
	 */
	public float histogramMethod(Slice slice) 
	{
		Histogram hh = new Histogram();
		int ii = 0;

		//
		//  Compute the thresholded histogram.
		//
		float lower_threshold = (float)0.01;
		float upper_threshold = (float)0.3 * (float)Statistics.max(slice);
		hh.histogram( slice, lower_threshold, upper_threshold );

		//
		//  Get the counts and bins.
		//
		float[] bins = hh.getBins();
		float[] counts = hh.getCounts();

		//
		//  Find the maximum count.
		//
		float max = (float)0.0; int max_index = 0;
		for(ii=0; ii<counts.length; ii++) {
			if( counts[ii] > max ) {
				max_index = ii;
				max = counts[ii];
			}
		}

		//
		//  Find the first minimum after the maximum
		//
		ii = max_index + 1;
		do {
			ii++;
		} while( counts[ii] < counts[ii-1] && ( ii < bins.length ) );

		float sigma = (float)0.0;
		if( ii >= bins.length-1 ) {
			System.out.println("Gack...could not find the first minimum.");
		}
		else {
			sigma = bins[ii];
		}

		return sigma;
	}

	/** Calculate the noise estimate via the gradient method given 
	 *  a specific slice.
	 *
	 */
	public float gradientMethod(Slice slice) 
	{
		//
		//  Intialize variables.
		//
		int nrows = slice.getNRows();
		int ncols = slice.getNCols();

		float[] v = new float[nrows*ncols]; int v_numset = 0;
		float t = (float)0.0; 
		float sig_y = (float)0.0;
		float sig_x = (float)0.0;

		//
		//  Compute the column gradient
		//
		for(int row=0; row<nrows; row++) {
			for(int col=1; col<ncols-1; col++) {
				t = slice.getData(row,col+1) - (float)2.0 * slice.getData(row,col) + slice.getData(row,col-1);

				if( t != (float)0.0 || slice.getData(row,col) != (float)0.0 ) {
					v[v_numset] = t; v_numset++;
				}
			}
		}

		if( v_numset <= 10 ) {
			sig_y = (float)0.0;
		}
		else {
			sig_y = vec_stdDevMod(v, v_numset);
		}

		v_numset = 0;

		//
		//  Compute the row gradient
		//
		for(int col=0; col<ncols; col++) {
			for(int row=1; row<nrows-1; row++) {
				t = slice.getData(row+1,col) - (float)2.0 * slice.getData(row,col) + slice.getData(row-1,col);

				if( t != (float)0.0 || slice.getData(row,col) != (float)0.0 )  {
					v[v_numset] = t; v_numset++;
				}
			}
		}

		if( v_numset <= 10 ) {
			sig_x = (float)0.0;
		}
		else {
			sig_x = vec_stdDevMod(v, v_numset);
		}

		return (float)Math.sqrt( (double)(sig_x * sig_x + sig_y * sig_y) ) / (float)2.0;
	}

	/** Calcualte the noise estimate via the gradient method given 
	 *  a volume and slice/channel.
	 *
	 */
	public float gradientMethod(Volume vol, int slice_num) 
	{
		//
		//  Get the right slice.
		//
		Slice slice = vol.getSlice(slice_num);

		return gradientMethod(slice);
	}

	/** Calcualte the noise estimate via the gradient method given 
	 *  a volume and channel.
	 *
	 */
	public float gradientMethod(Volume vol) 
	{
		float nes = (float)0.0;

		//
		//  Get the right slice.
		//
		for(int ii=0; ii<vol.getNSlices(); ii++) {
			nes += gradientMethod( vol.getSlice(ii) );
		}

		return (nes / (float)vol.getNSlices());
	}

	 
	/** Calcualte the noise estimate via the gradient method given 
	 *  a volume and slice/channel.
	 *
	 */
	public float gradientMethod(MCVolume vol, int slice_num, int channel_num) 
	{
		//
		//  Get the right slice.
		//
		Slice slice = vol.getSlice(slice_num, channel_num);

		return gradientMethod(slice);
	}

	/** Calcualte the noise estimate via the gradient method given 
	 *  a volume and channel.
	 *
	 */
	public float gradientMethod(MCVolume vol, int channel_num) 
	{
		float nes = (float)0.0;

		//
		//  Get the right slice.
		//
		for(int ii=0; ii<vol.getNSlices(); ii++) {
			nes += gradientMethod( vol.getSlice(ii, channel_num) );
		}

		return (nes / (float)vol.getNSlices());
	}

	 
	/** Calcualte the noise estimate via the gradient method.
	 *
	 */
	public float gradientMethod(MCVolume vol) 
	{
		float nes = (float)0.0;

		//
		//  Get the right slice.
		//
		for(int ii=0; ii<vol.getNSlices(); ii++) {
			for(int jj=0; jj<vol.getNChannels(); jj++) {
				nes += gradientMethod( vol.getSlice(ii, jj) );
			}
		}

		return (nes / ((float)vol.getNSlices()*(float)vol.getNChannels()) );
	}

	 
	/** One of the internal routines.
	 *
	 */
	private float vec_stdDevBounded(float[] v, int n, float lower, float upper){
		float t=(float)0.0, t2=(float)0.0;
		long m=0;
	 
		for(int i=0;i<n;i++)
		{
			if (v[i] >= lower && v[i] <= upper)
			{
				m++;
				t+=v[i];
				t2+=v[i]*v[i];
			}
		}

		if (m <= 1) return 0;
	 
		return (float)Math.sqrt((double)(t2/m - (t/m)*(t/m)));
	}
	 
	/** The other internal routine.
	 *
	 */
	private float vec_stdDevMod(float[] v, int n)
	{
		float iqr, med;
		float lb, ub;
		long i;
	 
		Arrays.sort(v);
		med = v[(int)(n/2)]; 
		iqr = v[(int)(3*n/4)] - v[(int)(n/4)];
		lb = med - (float)2.0*iqr; ub = med + (float)2.0*iqr;
	 
		return vec_stdDevBounded(v,n,lb,ub);
	}
}
