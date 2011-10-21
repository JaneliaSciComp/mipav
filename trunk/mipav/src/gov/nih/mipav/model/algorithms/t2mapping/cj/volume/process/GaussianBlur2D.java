package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class GaussianBlur2D extends ImageFilter2D
{
	private double sigma;
	private int rad, width;
	private double k1[];

	public GaussianBlur2D(double sigma)
	{
	// Initialise Gaussian table
		this.sigma = sigma;
		this.rad = (int)(1+sigma*3);
		this.width = 2*rad + 1;

		this.k1 = new double[width];

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

	}

	public void filter(Slice slice)
	{
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
}
