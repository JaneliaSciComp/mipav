package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.relax.t2roi;

import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2fit.T2FitNNLS;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;

/** T2MapClass is responsible to calculate the T2 spectrum through a volume
 *  of points, to contain the results and to save the results.
 */
public class T2ROIBootstrap extends T2ROI
{
	/** Random number generator (uniform distribution). */
	private Random rand_gen = null;

	/**
	 *  Setup the random number generator.
	 */
	public T2ROIBootstrap()
	{
		rand_gen = new Random();
	}

	public String getMethodName() { return "bootstrap"; }

	/**
	 *  Compute a mean and standard deviation decay curve from 
	 *  "N" randomly chosen (with replacement) indices.
	 */
	public void calculateMeanStd(final MCVolume vol, final int slice, 
		final PixFile pix, double[] means, double[] stds)
	{
		// Used for calculating the standard deviation.
		double[] total_squared = new double[ means.length ];
		Arrays.fill(total_squared, 0.0);
		Arrays.fill(means, 0.0);

		// Get the indices from the Pixel File.
		int[] indices = pix.getIndices();
		int N = pix.getIndices().length;

		// Create a pseudo-roi with N pixels chosen
		// with replacement from the indices.
		for(int jj=0; jj<N; jj++)
		{
			// Get a uniformly distributed random integer
			// in the range of [0, N).
			int rindex = rand_gen.nextInt( N );

			// Compute the row and column of the random index
			int rowcol[] = T2ROI.index2sub( indices[rindex], 
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
