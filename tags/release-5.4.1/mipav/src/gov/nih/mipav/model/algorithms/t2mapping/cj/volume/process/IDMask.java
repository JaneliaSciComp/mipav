package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.curvefit.GaussianFit;

public class IDMask 
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

	public IDMask()
	{
	}

	/** 
	 *  Set the verbose'ness.
	 */
	public void setVerbose( int verbose )
	{
		this.verbose = verbose;
	}

	public void setGaussianFactor(final double gf)
	{
		gaussian_factor = gf;
	}

	public Slice idmask(Slice imshort)
	{
		Slice mask = null;
		return mask;
	}

	public Volume idmask(final Volume imshort)
	{
		int middle_slice = (int)imshort.getNSlices() / 2;

		int nrows = imshort.getNRows();
		int ncols = imshort.getNCols();
		int nslices = imshort.getNSlices();

		/*
		 *  Create the output slice.
		 */
		Volume mask = (Volume)imshort.clone();

		/*
		 *  Calculate the arctangent slice and set it to 0 
		 *  whenever the data value from the short image is 
		 *  less than the noise estimate.
		 */
		NoiseEstimate ne = new NoiseEstimate();
		double noise_level = short_noise_gain * (double)ne.gradientMethod(imshort);

		if( verbose > 0 )
		{
			System.out.println("Noise estimate was: " + noise_level );
		}

		/*
		 *  Compute the histogram of the arctan image.
		 */
		Histogram hh = new Histogram();

		hh.histogram(imshort, (float)noise_level, (float)Statistics.max(imshort), 50);
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
		{
			System.out.println("Max was " + max + " and was at bin " + 
			                   bins[max_index]);
		}

		// Find the lower index for the number of elements that are > X% of max.
		int lower_index = max_index;
		while( (lower_index > 0) && (counts[lower_index] > gaussian_factor*max) )
		{
			lower_index--;
		}

		// Find the upper index for the number of elements that are > X% of max.
		int upper_index = max_index;
		while( (upper_index < counts.length) && counts[upper_index] > gaussian_factor*max )
		{
			upper_index++;
		}


		if( verbose > 0 )
		{
			System.out.println("Lower/Upper indices are " + lower_index + " " + 
			                   upper_index);
		}

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
		double lower = params[1] - 3.0 * params[2];

		// There seem to be some pathological cases where the
		// lower bound was negative, so for these, let's just
		// keep bumping up the lower bound until it is positive.
		double f = 2.0;
		while( lower < 0.0 )
		{
			f -= 1.0;
			lower = params[1] - f * params[2];
		}

		double upper = Statistics.prctile( imshort, 99.99 );
		if( verbose > 0 ) 
		{
			System.out.println("Lower and upper thresholds are " + lower + " " + upper);
		}

		BinaryVolume bv = imshort.threshold(0.6*lower, upper);
		bv.erodeSlices(1, true);

		mask.applyMask( bv );

		//-----------------------------------------
		//
		// Label the mask.
		//
		Slice L = bv.getSlice(middle_slice).label();

		// Find the largest component.
		hh.histogram(L, (float)0.0, (float)Statistics.max(L), (int)Statistics.max(L));
		bins = hh.getBins();
		counts = hh.getCounts();

		int maxC = -1;
		int maxI = 0;
		for(int ii=1; ii<bins.length; ii++)
		{
			if( (int)counts[ii] > maxC ) 
			{
				maxC = (int)counts[ii];
				maxI = ii;
			}
		}
		
		// Create middle slice mask which is the largest connected
		// component.
		BinarySlice middle_slice_mask = L.threshold(maxI-0.1, maxI+0.1);

		// Create previous slice mask which is the largest connected
		// component.
		BinarySlice prev_slice_mask = L.threshold(maxI-0.1, maxI+0.1);

		// Fill the holes on the previous slice mask.
		prev_slice_mask.fillHoles(true, true);

		// Set the masked slice data back into "mask".
		mask.setSlice(prev_slice_mask, middle_slice);

		//---------------------------------------------------------
		//
		//  Now go down.
		//
		for(int slice=middle_slice-1; slice >= 0; slice--)
		{
			if( verbose > 1 )
			{
				System.out.println("-----------------------------------");
				System.out.println("Doing slice " + slice );
			}

			BinarySlice slice_mask = internalMask( bv.getSlice(slice), 
			                                       prev_slice_mask);

			slice_mask.fillHoles(true, true);
			mask.setSlice(slice_mask, slice);
			prev_slice_mask = (BinarySlice)slice_mask.clone();
		}

		//---------------------------------------------------------
		//
		//  Now go up.
		//
		prev_slice_mask = middle_slice_mask;
		for(int slice=middle_slice+1; slice < mask.getNSlices(); slice++)
		{
			if( verbose > 1 )
			{
				System.out.println("-----------------------------------");
				System.out.println("Doing slice " + slice );
			}

			BinarySlice slice_mask = internalMask( bv.getSlice(slice), 
			                                       prev_slice_mask);

			slice_mask.fillHoles(true, true);
			mask.setSlice(slice_mask, slice);
			prev_slice_mask = (BinarySlice)slice_mask.clone();
		}

		return mask;
	}

	private BinarySlice internalMask( final BinarySlice bs, 
									  final BinarySlice prev_slice_mask )
	{
		/*
		 *  Label the slice based on connected components.
		 */
		Slice L = bs.label();

		/*
		 *  Find the largest component.
		 */
		Histogram hh = new Histogram();
		hh.histogram(L, (float)0.0, (float)Statistics.max(L), 
					 (int)Statistics.max(L));

		float[] bins = hh.getBins();
		float[] counts = hh.getCounts();

		int maxC = -1;
		int maxI = 0;
		for(int ii=1; ii<bins.length; ii++)
		{
			if( (int)counts[ii] > maxC ) 
			{
				maxC = (int)counts[ii];
				maxI = ii;
			}
		}

		if( verbose > 1 )
		{
			System.out.println("Max was for label " + maxI );
		}
		
		/*
		 *  Now run through each labelled component
		 *  and see if it is greater than 100 and 
		 *  it overlaps at least 20% with the component above.
		 */
		BinarySlice slice_mask = new BinarySlice(L.getNRows(), L.getNCols());
		for(int label = 1; label <= bins.length; label++)
		{
			BinarySlice temp = L.threshold(label-0.1, label+0.1);

			int num = temp.numberTrue();
			
			if( verbose > 1 )
			{
				System.out.println("There were " + num + " pixels for label " + label);
			}

			if( num < 100 )
				continue;

			BinarySlice temp2 = (BinarySlice)temp.clone();
			temp2.and(prev_slice_mask);

			if( verbose > 1 )
			{
				System.out.println("There are " + temp2.numberTrue() + " in the temp slice");
			}

			if( (double)temp2.numberTrue() / (double)num < 0.2 )
				continue;

			slice_mask.or( temp );	
		}

		return slice_mask;
	}

	public MCVolume idmask(final MCVolume vol) 
	{ 
		MCVolume out = (MCVolume)vol.clone();

		//for(int ii=0; ii<vol.getNChannels(); ii++)
		for(int ii=0; ii<1; ii++)
		{
			out.setVolume( idmask( vol.getVolume(ii) ), ii);	
		}

		return out;
	} 
}
