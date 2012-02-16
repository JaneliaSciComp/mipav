package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class ConvolveFilter 
{

	private int verbose = 0;

	public ConvolveFilter()
	{
	}

	/** 
	 *  Set the verbose'ness.
	 */
	public void setVerbose( int verbose )
	{
		this.verbose = verbose;
	}

	public void uniform(Slice slice, int window)
	{
		int rad = (window-1)/2;

		if (rad == 0) return;

		int nrows = slice.getNRows();
		int ncols = slice.getNCols();
		float[] t, t2;

		// Sum along each col
		t = new float[nrows]; t2 = new float[nrows];

		for(int ii=0; ii<ncols; ii++) 
		{
			slice.getCol(t, ii);
			java.util.Arrays.fill(t2, 0);

			for(int jj=0; jj<nrows; jj++)
			{
				int end = Math.min(jj+1+rad,nrows);

				for(int kk=Math.max(jj-rad,0); kk < end; kk++)
				{
					t2[jj] += t[kk];
				}
			}

			slice.setCol(t2, ii);
		}

		// Sum along each row
		t = new float[ncols]; t2 = new float[ncols];

		for(int ii=0; ii<nrows; ii++) 
		{
			slice.getRow(t, ii);
			java.util.Arrays.fill(t2, 0);

			int s1 = Math.min(ii+1+rad,nrows) - Math.max(0,ii-rad);

			for(int jj=0; jj<ncols; jj++)
			{
				int end = Math.min(jj+1+rad,ncols);

				for(int kk=Math.max(jj-rad,0); kk < end; kk++)
				{
					t2[jj] += t[kk];
				}

				int s2 = Math.min(jj+1+rad,ncols) - Math.max(0,jj-rad);

				t2[jj] /= (s1*s2);
			}

			slice.setRow(t2, ii);
		}
	}

	public void uniform(Volume vol, int window)
	{
		//  Make window be an odd number.
		if( (window%2) == 0 ) 
		{
			window++;
		}

		//  Loop over the slices and channels.
		for(int slice=0; slice<vol.getNSlices(); slice++) {
			if( verbose > 0 )
			{
				System.out.println("Filtering slice " + slice);
			}

			// Get the slice of interest.
			Slice s = vol.getSlice(slice);
			uniform(s,window);
			vol.setSlice(s, slice);
		}
	}

	public void uniform(MCVolume vol, int window)
	{
		//  Make window be an odd number.
		if( (window%2) == 0 ) 
		{
			window++;
		}

		//  Loop over the slices and channels.
		for(int slice=0; slice<vol.getNSlices(); slice++) {
			for(int channel=0; channel<vol.getNChannels(); channel++) {

				if( verbose > 0 )
				{
					System.out.println("Filtering slice " + slice + 
						" and channel " + channel);
				}

				// Get the slice of interest.
				Slice s = vol.getSlice(slice, channel);
				uniform(s,window);
				vol.setSlice(s, slice, channel);
			}
		}
	}

	public void gaussian(Slice slice, double sigma)
	{
		int rad = (int)(1+sigma*3);
		int width = 2*rad + 1;

		if (sigma == 0) return;

		int nrows = slice.getNRows();
		int ncols = slice.getNCols();

		double k1[] = new double[width];

		// Initialise 1D Gaussian kernel
		{
			double tot=0;
			for (int i=-rad; i<=rad; i++)
			{
				k1[i+rad] = Math.exp(-(double)i*i/(2*sigma*sigma));
				tot += k1[i+rad];
			}
			for (int i=0; i<width; i++) { k1[i] /= tot; }
		}

		float[] t, t2;

		// Sum along each col
		t = new float[nrows]; t2 = new float[nrows];

		for(int ii=0; ii<ncols; ii++) 
		{
			slice.getCol(t, ii);
			java.util.Arrays.fill(t2, 0);

			for(int jj=0; jj<nrows; jj++)
			{
				int end = Math.min(jj+1+rad,nrows);

				for(int kk=Math.max(jj-rad,0); kk < end; kk++)
				{
					t2[jj] += t[kk] * k1[kk-jj+rad];
				}
			}

			slice.setCol(t2, ii);
		}

		// Sum along each row
		t = new float[ncols]; t2 = new float[ncols];

		for(int ii=0; ii<nrows; ii++) 
		{
			slice.getRow(t, ii);
			java.util.Arrays.fill(t2, 0);

			for(int jj=0; jj<ncols; jj++)
			{
				int end = Math.min(jj+1+rad,ncols);

				for(int kk=Math.max(jj-rad,0); kk < end; kk++)
				{
					t2[jj] += t[kk] * k1[kk-jj+rad];
				}
			}

			slice.setRow(t2, ii);
		}
	}

	public void gaussian(Volume vol, double sigma)
	{
		//  Loop over the slices and channels.
		for(int slice=0; slice<vol.getNSlices(); slice++) {
			if( verbose > 0 )
			{
				System.out.println("Filtering slice " + slice);
			}

			// Get the slice of interest.
			Slice s = vol.getSlice(slice);
			gaussian(s,sigma);
			vol.setSlice(s, slice);
		}
	}

	public void gaussian(MCVolume vol, double sigma)
	{
		//  Loop over the slices and channels.
		for(int slice=0; slice<vol.getNSlices(); slice++) {
			for(int channel=0; channel<vol.getNChannels(); channel++) {

				if( verbose > 0 )
				{
					System.out.println("Filtering slice " + slice + 
						" and channel " + channel);
				}

				// Get the slice of interest.
				Slice s = vol.getSlice(slice, channel);
				gaussian(s,sigma);
				vol.setSlice(s, slice, channel);
			}
		}
	}
}
