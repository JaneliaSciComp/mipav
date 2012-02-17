package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class UniformBlur2D extends ImageFilter2D
{
	private int window, rad;

	public UniformBlur2D(int window)
	{
		this.window = window;

		if( (window%2) == 0 ) 
			window++;

		this.rad = (window-1)/2;
	}

	public void filter(Slice slice)
	{
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
}
