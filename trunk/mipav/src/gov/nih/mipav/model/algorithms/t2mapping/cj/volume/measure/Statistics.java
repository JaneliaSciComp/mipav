package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

/**
 *  This class is used to calculate different statistics on 
 *  Slices, Volumes and MCVolumes.  Most if not all methods 
 *  are static therefore no instance of this class needs to be
 *  constructed.
 */
public class Statistics 
{

	public Statistics()
	{
	}

	/**
	 *  Find the maximum value over the slice.
	 */
	static public double max(Slice slice)
	{
		double biggest = -Double.MAX_VALUE;
		for(int ii=0; ii<slice.getNRows();ii++) 
		{
			for(int jj=0; jj<slice.getNCols();jj++) 
			{
				biggest = Math.max(biggest, slice.getData(ii,jj) );
			}
		}
		return biggest;
	}

	/**
	 *  Find the maximum value over the volume.
	 */
	static public double max(Volume vol)
	{
		double biggest = -Double.MAX_VALUE;
		for(int kk=0; kk<vol.getNSlices();kk++) 
		{
			biggest = Math.max(biggest, max(vol.getSlice(kk)) );
		}

		return biggest;
	}

	/**
	 *  Find the maximum value over the multi-channel volume.
	 */
	static public double max(MCVolume vol)
	{
		double biggest = -Double.MAX_VALUE;
		for(int kk=0; kk<vol.getNChannels();kk++) 
		{
			biggest = Math.max(biggest, max(vol.getVolume(kk)) );
		}

		return biggest;
	}



	static public double min(Slice slice)
	{
		double smallest = Double.MAX_VALUE;
		for(int ii=0; ii<slice.getNRows();ii++) 
		{
			for(int jj=0; jj<slice.getNCols();jj++) 
			{
				smallest = Math.min(smallest, slice.getData(ii,jj) );
			}
		}
		return smallest;
	}

	static public double min(Volume vol)
	{
		double smallest = Double.MAX_VALUE;
		for(int kk=0; kk<vol.getNSlices();kk++) 
		{
			smallest = Math.min(smallest, min(vol.getSlice(kk)) );
		}

		return smallest;
	}

	static public double min(MCVolume vol)
	{
		double smallest = Double.MAX_VALUE;
		for(int kk=0; kk<vol.getNChannels();kk++) 
		{
			smallest = Math.min(smallest, min(vol.getVolume(kk)) );
		}

		return smallest;
	}


	static public double sum(Slice slice)
	{
		double total = 0.0;
		for(int ii=0; ii<slice.getNRows();ii++) 
		{
			for(int jj=0; jj<slice.getNCols();jj++) 
			{
				total += slice.getData(ii,jj);
			}
		}
		return total;
	}

	static public double sum(Volume vol)
	{
		double total = 0.0;
		for(int kk=0; kk<vol.getNSlices();kk++) 
		{
			total += sum( vol.getSlice(kk) );
		}

		return total;
	}

	static public double sum(MCVolume vol)
	{
		double total = 0.0;
		for(int kk=0; kk<vol.getNChannels();kk++) 
		{
			total += sum(vol.getVolume(kk));
		}

		return total;
	}


	static public double mean(final Slice slice)
	{
		return sum(slice) / (double)(slice.getNRows()*slice.getNCols());
	}

	static public double mean(final Volume vol)
	{
		return sum(vol) / (double)(vol.getNRows()*vol.getNCols()*vol.getNSlices());
	}

	static public double mean(final MCVolume vol)
	{
		return sum(vol) / (double)(vol.getNRows()*vol.getNCols()*
		                             vol.getNSlices()*vol.getNChannels());
	}

	/*-------------------------------------------------------------------
	 *
	 *                         STANDARD DEVIATIONS
	 *
	 *-------------------------------------------------------------------
	 */

	static public void stdRows(final Slice slice, double[] stds)
	{
		stdRows(slice, Double.POSITIVE_INFINITY, stds);
	}

	/** stdRows - Calculate the standard deviation for each row.
	 *  So the return vector will contain a single double which is the
	 *  standard deviation for that row in the slice.
	 *
	 */
	static public void stdRows(final Slice slice, final double threshold, 
							   double[] stds)
	{
		int n = 0;
		double x = 0.0;
		double x2 = 0.0;

		for(int row=0; row<slice.getNRows(); row++) 
		{
			n = 0;
			x = x2 = 0.0;
			for(int col=0; col<slice.getNCols(); col++) 
			{
				if( slice.getData(row,col) < threshold )
				{
					x += slice.getData(row,col);
					x2 += (slice.getData(row,col)*slice.getData(row,col));
					n++;
				}
			}

			if( n > 0 ) 
			{
				stds[row] = Math.sqrt((double)(x2 - (x*x)/(double)n)/(double)(n-1));
			}
			else
			{
				stds[row] = 0.0;
			}
		}
	}

	/** stdCols - Calculate the standard deviation for each column.
	 */
	static public double[] stdCols(final Slice slice)
	{
		return stdCols(slice, Double.NEGATIVE_INFINITY);
	}

	/** stdCols - Calculate the standard deviation for each col.
	 */
	static public double[] stdCols(final Slice slice, final double threshold)
	{
		double[] stds = new double[slice.getNCols()];
		int n = 0;
		double x = 0.0;
		double x2 = 0.0;

		for(int col=0; col<slice.getNCols(); col++) 
		{
			n = 0;
			x = x2 = 0.0;
			for(int row=0; row<slice.getNRows(); row++) 
			{
				if( slice.getData(row,col) > threshold )
				{
					x += slice.getData(row,col);
					x2 += (slice.getData(row,col)*slice.getData(row,col));
					n++;
				}
			}

			if( n > 0 ) 
			{
				stds[col] = Math.sqrt((double)(x2 - (x*x)/(double)n)/(double)(n-1));
			}
			else
			{
				stds[col] = 0.0;
			}
		}

		return stds;
	}

	/*-------------------------------------------------------------------
	 *
	 *                         PERCENTILES
	 *
	 *-------------------------------------------------------------------
	 */

	static public double[] prctile(final Slice slice, double[] pc)
	{
		double[] prc_out = new double[ pc.length ];

		for(int ii=0; ii<pc.length; ii++) 
		{
			prc_out[ii] = prctile(slice, pc[ii]);
		}

		return prc_out;
	}

	static public double prctile(final Slice slice, final double pc)
	{
		double result = 0.0;
		int p_up, p_down;
		int len = slice.getNRows()*slice.getNCols();
		float[] thedata = new float[ len ];
	 
		for(int row=0; row<slice.getNRows(); row++)
		{
			for(int col=0; col<slice.getNCols(); col++)
			{
				thedata[row*slice.getNCols()+col] = slice.getData(row,col);
			}
		}

		/*
		 *  Sort the things.
		 */
		Arrays.sort(thedata);
		double p = (double)thedata.length * pc / 100.0 + 0.5;
		p_up = (int)Math.ceil( p );
		p_down = (int)Math.floor( p );

		if( p_down <= 0 ) {
			result = (double)thedata[0];
		}
		else if( p_up >= thedata.length ) {
			result = (double)thedata[ thedata.length-1];
		}
		else {
			result = (thedata[p_up-1]-thedata[p_down-1]) *
				(p-p_down)/(p_up-p_down) + thedata[p_down-1];
		}
	 
		return result;
	}

	static public double[] prctile(final Volume vol, double[] pc)
	{
		double[] prc_out = new double[ pc.length ];

		for(int ii=0; ii<pc.length; ii++) 
		{
			prc_out[ii] = prctile(vol, pc[ii]);
		}

		return prc_out;
	}

	static public double prctile(final Volume vol, double pc)
	{
		double result = 0.0;
		int p_up, p_down;
		Slice s = null;
		float[] thedata = new float[ vol.getNRows()* vol.getNCols()* vol.getNSlices()];
	 
		for(int sl=0; sl<vol.getNSlices(); sl++)
		{
			for(int row=0; row<vol.getNRows(); row++)
			{
				for(int col=0; col<vol.getNCols(); col++)
				{
					thedata[sl*vol.getNRows()*vol.getNCols()+row*vol.getNCols()+col] = vol.getData(row,col,sl);
				}
			}
		}


		/*
		 *  Sort the things.
		 */
		Arrays.sort(thedata);
		double p = (double)thedata.length * pc / 100.0 + 0.5;
		p_up = (int)Math.ceil( p );
		p_down = (int)Math.floor( p );

		if( p_down <= 0 ) {
			result = (double)thedata[0];
		}
		else if( p_up >= thedata.length ) {
			result = (double)thedata[ thedata.length-1];
		}
		else {
			result = (thedata[p_up-1]-thedata[p_down-1]) *
				(p-p_down)/(p_up-p_down) + thedata[p_down-1];
		}
	 
		return result;
	}

	static public double[] prctile(final MCVolume vol, double[] pc)
	{
		double[] prc_out = new double[ pc.length ];

		for(int ii=0; ii<pc.length; ii++) 
		{
			prc_out[ii] = prctile(vol, pc[ii]);
		}

		return prc_out;
	}

	static public double prctile(final MCVolume vol, double pc)
	{
		double result = 0.0;
		int p_up, p_down;
		Slice s = null;
		float[] thedata = new float[ 
						vol.getNRows()* vol.getNCols()* 
						vol.getNSlices()* vol.getNChannels()];
	 
		for(int ch=0; ch<vol.getNChannels(); ch++)
		{
			for(int sl=0; sl<vol.getNSlices(); sl++)
			{
				for(int row=0; row<vol.getNRows(); row++)
				{
					for(int col=0; col<vol.getNCols(); col++)
					{
						thedata[ch*vol.getNSlices()*vol.getNRows()*vol.getNCols() + sl*vol.getNRows()*vol.getNCols()+row*vol.getNCols()+col] = vol.getData(row,col,sl,ch);
					}
				}
			}
		}


		/*
		 *  Sort the things.
		 */
		Arrays.sort(thedata);
		double p = (double)thedata.length * pc / 100.0 + 0.5;
		p_up = (int)Math.ceil( p );
		p_down = (int)Math.floor( p );

		if( p_down <= 0 ) {
			result = (double)thedata[0];
		}
		else if( p_up >= thedata.length ) {
			result = (double)thedata[ thedata.length-1];
		}
		else {
			result = (thedata[p_up-1]-thedata[p_down-1]) *
				(p-p_down)/(p_up-p_down) + thedata[p_down-1];
		}
	 
		return result;
	}
}
