package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.curvefit.GaussianFit;

public class CSFMask 
{
	/** How verbose do you want it?
	 */
	private int verbose = 0;

	/** Minimum threshold on the angles that will be included
	 */
	private double minimum_arctan = 0.30;

	/** Factor to multiply by the estimated noise based on the
	 *  gradient method.
	 */
	private double short_noise_gain = 4;

	/** Factor on the standard deviation for computing the
	 *  lower threshold on the angles.
	 */
	private double angle_std_factor = 2;

	/** Factor that thresholds which angles to include in the 
	 *  Gaussian fitting.  ie Calculate based on
	 *  angles > gaussian_factor * max.
	 */
	private double gaussian_factor = 0.4;

	public CSFMask()
	{
	}

	/** 
	 *  Set the verbose'ness.
	 */
	public void setVerbose( int verbose )
	{
		this.verbose = verbose;
	}

	public Slice csfmask(Slice imshort, Slice imlong)
	{
		int nrows = imshort.getNRows();
		int ncols = imshort.getNCols();

		/*
		 *  Create the output slice.
		 */
		Slice mask = new Slice( nrows, ncols );

		/*
		 *  Calculate the arctangent slice and set it to 0 
		 *  whenever the data value from the short image is 
		 *  less than the noise estimate.
		 */
		NoiseEstimate ne = new NoiseEstimate();
		double noise_level = short_noise_gain * (double)ne.gradientMethod(imshort);

		for(int row=0; row<imshort.getNRows(); row++)
		{
			for(int col=0; col<imshort.getNCols(); col++)
			{
				if( imshort.getData(row,col) > noise_level && 
				    imlong.getData(row,col) > 0.0 )
				{
					mask.setData((float)Math.atan(imlong.getData(row,col)/
											  imshort.getData(row,col)), 
							  row, col);
				}
				else
				{
					mask.setData((float)0.0, row,col);
				}
			}
		}

		/*
		 *  Compute the histogram of the arctan image.
		 */
		Histogram hh = new Histogram();

		hh.histogram(mask, (float)minimum_arctan, (float)10.0);
		float[] bins = hh.getBins();
		float[] counts = hh.getCounts();

		/* 
		 *  Now create the list of elements that are within
		 *  X% of the peak.
		 */
		double max = 0.0;
		int max_index = 0;
		for(int ii=0; ii<counts.length; ii++)
		{
			if( (double)counts[ii] > max )
			{
				max = (double)counts[ii];
				max_index = ii;
			}
		}

		// Find the lower index for the number of elements that are > X% of max.
		int lower_index = max_index;
		while( counts[lower_index] > gaussian_factor*max )
		{
			lower_index--;
		}

		// Find the upper index for the number of elements that are > X% of max.
		int upper_index = max_index;
		while( counts[upper_index] > gaussian_factor*max )
		{
			upper_index++;
		}


		if( verbose > 0 )
			System.out.println("Lower/Upper indices are " + lower_index + " " + upper_index);

		double[] main_bins = new double[upper_index-lower_index+1];
		double[] main_counts = new double[upper_index-lower_index+1];
		int jj=0;
		for(int ii=lower_index; ii<=upper_index; ii++)
		{
				main_bins[jj] = bins[ii];
				main_counts[jj] = counts[ii];
				jj++;
		}

		/*
		 *  Now do a gaussian fit to the data.
		 */
		GaussianFit gf = new GaussianFit();
		gf.doFit(main_bins,main_counts);
		double[] params = gf.getParams();

		/*
		 *  Now do the thresholding.
		 */
		double csf_cutoff = params[1] + angle_std_factor * params[2];
		System.out.println("csf_cutoff = " + csf_cutoff);

		for(int row=0; row<imshort.getNRows(); row++)
		{
			for(int col=0; col<imshort.getNCols(); col++)
			{
				if( mask.getData(row,col)>csf_cutoff )
				{
					mask.setData(1.0, row,col);
				}
				else
				{
					mask.setData(0.0, row,col);
				}
			}
		}

		return mask;
	}

	public Volume csfmask(final Volume imshort, final Volume imlong)
	{
		int nrows = imshort.getNRows();
		int ncols = imshort.getNCols();
		int nslices = imshort.getNSlices();

		/*
		 *  Create the output slice.
		 */
		Volume mask = new Volume( nrows, ncols, nslices );

		/*
		 *  Calculate the arctangent slice and set it to 0 
		 *  whenever the data value from the short image is 
		 *  less than the noise estimate.
		 */
		NoiseEstimate ne = new NoiseEstimate();
		double noise_level = short_noise_gain * (double)ne.gradientMethod(imshort);

		if( verbose > 0 )
			System.out.println("Noise estimate was: " + noise_level );

		for(int slice=0; slice<imshort.getNSlices(); slice++)
		{
			for(int row=0; row<imshort.getNRows(); row++)
			{
				for(int col=0; col<imshort.getNCols(); col++)
				{
					if( imshort.getData(row,col,slice) > noise_level )
					{
						mask.setData((float)Math.atan(imlong.getData(row,col,slice)/
												  imshort.getData(row,col,slice)), 
								  row, col, slice);
					}
					else
					{
						mask.setData((float)0.0, row, col, slice);
					}
				}
			}
		}

		/*
		 *  Compute the histogram of the arctan image.
		 */
		Histogram hh = new Histogram();

		hh.histogram(mask, (float)minimum_arctan, (float)10.0, 20);
		float[] bins = hh.getBins();
		float[] counts = hh.getCounts();

		/* 
		 *  Now create the list of elements that are within
		 *  X% of the peak.
		 */
		double max = 0.0;
		int max_index = 0;
		for(int ii=0; ii<counts.length; ii++)
		{
			if( (double)counts[ii] > max )
			{
				max = (double)counts[ii];
				max_index = ii;
			}
		}

		if( verbose > 0 )
			System.out.println("Max was " + max + " and was at bin " + bins[max_index]);

		// Find the lower index for the number of elements that are > X% of max.
		int lower_index = max_index;
		while( counts[lower_index] > gaussian_factor*max )
		{
			lower_index--;
		}

		// Find the upper index for the number of elements that are > X% of max.
		int upper_index = max_index;
		while( counts[upper_index] > gaussian_factor*max )
		{
			upper_index++;
		}


		if( verbose > 0 )
			System.out.println("Lower/Upper indices are " + lower_index + " " + upper_index);

		double[] main_bins = new double[upper_index-lower_index+1];
		double[] main_counts = new double[upper_index-lower_index+1];
		int jj=0;
		for(int ii=lower_index; ii<=upper_index; ii++)
		{
				main_bins[jj] = bins[ii];
				main_counts[jj] = counts[ii];
				System.out.println( main_bins[jj] + "   " + main_counts[jj]);

				jj++;
		}

		/*
		 *  Now do a gaussian fit to the data.
		 */
		GaussianFit gf = new GaussianFit();
		gf.doFit(main_bins,main_counts);
		double[] params = gf.getParams();

		if( verbose > 0 )
		{
			System.out.println("Gaussian fit resulted in ");

			for(int ii=0; ii<params.length; ii++)
			{
				System.out.print("  " + params[ii]);
			}
			System.out.println("");
		}

		/*
		 *  Now do the thresholding.
		 */
		double csf_cutoff = params[1] + angle_std_factor * params[2];
		System.out.println("csf_cutoff = " + csf_cutoff);

		for(int slice=0; slice<imshort.getNSlices(); slice++)
		{
			for(int row=0; row<imshort.getNRows(); row++)
			{
				for(int col=0; col<imshort.getNCols(); col++)
				{
					if( mask.getData(row,col,slice)>csf_cutoff )
					{
						mask.setData(1.0, row,col,slice);
					}
					else
					{
						mask.setData(0.0, row,col,slice);
					}
				}
			}
		}

		return mask;
	}

	public MCVolume csfmask(final MCVolume vol)
	{
		MCVolume out = new MCVolume(vol.getNRows(), vol.getNCols(), vol.getNSlices(), 1);

		if( vol.getNChannels() < 2 ) 
		{
			System.out.println("csfmask: At this point, you must pass a dual echo MIF into the program.");
			System.exit(-1);
		}

		Volume vv = csfmask(vol.getVolume(0), vol.getVolume(1));

		out.setVolume(vv, 0);

		return out;
	}
}
