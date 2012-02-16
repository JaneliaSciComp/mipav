package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

/**
 *  Basic arithmetic on Slices, Volumes and MCVolumes.
 */
public class Arithmetic 
{
	public Arithmetic()
	{
	}

	static public void add(Slice slice, double aa)
	{
		int nrows = slice.getNRows();
		int ncols = slice.getNCols();

		for(int ii=0; ii<nrows; ii++)
		{
			float[] tt = slice.getRow(ii);
			for(int jj=0; jj<ncols; jj++) tt[jj]+=aa;
			slice.setRow(tt, ii);
		}
	}

	static public void subtract(Slice slice, double aa)
	{
		int nrows = slice.getNRows();
		int ncols = slice.getNCols();

		for(int ii=0; ii<nrows; ii++)
		{
			float[] tt = slice.getRow(ii);
			for(int jj=0; jj<ncols; jj++) tt[jj]+=aa;
			slice.setRow(tt, ii);
		}
	}

	static public void multiply(Slice slice, double aa)
	{
		int nrows = slice.getNRows();
		int ncols = slice.getNCols();

		for(int ii=0; ii<nrows; ii++)
		{
			float[] tt = slice.getRow(ii);
			for(int jj=0; jj<ncols; jj++) tt[jj]*=aa;
			slice.setRow(tt, ii);
		}
	}

	static public void divide(Slice slice, double aa)
	{
		int nrows = slice.getNRows();
		int ncols = slice.getNCols();

		for(int ii=0; ii<nrows; ii++)
		{
			float[] tt = slice.getRow(ii);
			for(int jj=0; jj<ncols; jj++) tt[jj]/=aa;
			slice.setRow(tt, ii);
		}
	}


	static public void add(Volume vol, double aa)
	{
		for(int ii=0; ii<vol.getNSlices(); ii++)
		{
			add(vol.getSlice(ii), aa);
		}
	}

	static public void subtract(Volume vol, double aa)
	{
		for(int ii=0; ii<vol.getNSlices(); ii++)
		{
			subtract(vol.getSlice(ii), aa);
		}
	}

	static public void multiply(Volume vol, double aa)
	{
		for(int ii=0; ii<vol.getNSlices(); ii++)
		{
			multiply(vol.getSlice(ii), aa);
		}
	}
	static public void divide(Volume vol, double aa)
	{
		for(int ii=0; ii<vol.getNSlices(); ii++)
		{
			divide(vol.getSlice(ii), aa);
		}
	}

	static public void add(MCVolume vol, double aa)
	{
		for(int ii=0; ii<vol.getNChannels(); ii++)
		{
			add(vol.getVolume(ii), aa);
		}
	}

	static public void subtract(MCVolume vol, double aa)
	{
		for(int ii=0; ii<vol.getNChannels(); ii++)
		{
			subtract(vol.getVolume(ii), aa);
		}
	}

	static public void multiply(MCVolume vol, double aa)
	{
		for(int ii=0; ii<vol.getNChannels(); ii++)
		{
			multiply(vol.getVolume(ii), aa);
		}
	}
	static public void divide(MCVolume vol, double aa)
	{
		for(int ii=0; ii<vol.getNChannels(); ii++)
		{
			divide(vol.getVolume(ii), aa);
		}
	}
}
