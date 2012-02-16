package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.relax.t2roi;

import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2fit.T2FitNNLS;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.*;

/** 
 *  Responsible for compute the mean and standard deviation of a decay curve
 *  which will then be used in T2ROI.solve() to compute the decay curve.
 */
public class T2ROIMean extends T2ROI
{
	/**
	 *  Only thing we need to do is to make sure it knows that 
	 *  the number of iterations is 1.
	 */
	public T2ROIMean()
	{
		iterations = 1;
	}

	/**
	 *  Set the name of this method.
	 */
	public String getMethodName() { return "mean"; }

	/**
	 *  Number of iterations does not make sense for the mean.
	 */
	public void setIterations(final int iterations)
	{
	}

	/**
	 *  Given a pixel file and the volume, compute the mean 
	 *  and standard deviation over the pixels.
	 */
	public void calculateMeanStd(final MCVolume vol, final int slice, 
		final PixFile pix, double[] means, double[] stds)
	{
		int[] indices = pix.getIndices();
		double[] total_squared = new double[vol.getNChannels()];

		int N = indices.length;
		int row=0, col=0;

		Arrays.fill(means, 0.0);
		Arrays.fill(total_squared, 0.0);
		Arrays.fill(stds, 0.0);

		// Loop over the number of pixels (N) in the pix file.
		for(int jj=0; jj<N; jj++)
		{
			int rowcol[] = T2ROI.index2sub( indices[jj], 
				vol.getNRows(), vol.getNCols());

			for(int ch=0; ch<vol.getNChannels(); ch++)
			{
				double tt = (double)vol.getData(rowcol[0],rowcol[1],slice,ch);
				means[ch] += tt;
				total_squared[ch] += (tt*tt);
			}
		}

		// Compute the standard deviation.	
		for(int ch=0; ch<vol.getNChannels(); ch++)
		{
			stds[ch] = Math.sqrt((total_squared[ch]-(means[ch]*means[ch])/(double)N)/(double)(N-1) );	
			means[ch] /= (double)N;
		}
	}
}
