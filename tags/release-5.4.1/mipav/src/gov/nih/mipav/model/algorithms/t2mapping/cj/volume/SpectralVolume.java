package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

import java.io.*;
import java.lang.*;
import java.util.*;
import javax.imageio.stream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.*;

/**
 * SpectralVolume is a class that defines a volume of amplitudes based
 * on a set of T2 times as calculated by NNLS.
 * 
 * @author Craig Jones
 * @version 1.1
 */
public class SpectralVolume implements Cloneable 
{
	private int verbose = 0;

	/** 
	 *  Empty constructor probably should not be used.
	 */
	public SpectralVolume() 
	{
	}

	/**
	 *  Constructor that defines the number of rows, columns, slices and T2 times.
	 *
	 *  @param nrows_in - Number of rows.
	 *  @param ncols_in - Number of columns.
	 *  @param nslices_in - Number of slices.
	 *  @param nt2_in - Number of T2 values.
	 */
	public SpectralVolume(int nrows_in, int ncols_in, int nslices_in, int nt2_in) 
	{
		volume = new MCVolumeFile(nrows_in, ncols_in, nslices_in, nt2_in);
	}

	/** I want to be cloned.
	 *
	 */
	public Object clone()
	{
		Object o = null;
		try {
			o = super.clone();
		}
		catch( CloneNotSupportedException e) {
			System.out.println("SpectralVolume can not be cloned.");
		}

		return o;
	}

	/**
	 *  Read function to read in the set of data.
	 *
	 *  @param ds - Little endian data input stream.
	 */
	public void read( ImageInputStream ds ) 
	{
		if( volume == null ) {
			throw (new NullPointerException("SpectralVolume::read: The volume has not been defined"));
		}

		for(int jj=0; jj<volume.getNChannels(); jj++) {
			for(int ii=0; ii<volume.getNSlices(); ii++) {
				volume.read(ds, ii, jj, MRIFile.DATATYPE_FLOAT );
			}
		}
	}

	/** 
	 *  Set up a bin.
	 */
	public void defineBin(double start, double stop, String name)
	{
		if( binmap == null ) {
			binmap = new T2BinMaps(volume.getNRows(), volume.getNCols(), volume.getNSlices());
		}

		binmap.define(start, stop, name);
	}

	/** 
	 *  Compute the bins.
	 */
	public void computeBins( double[] t2s )
	{
		double[] amplitudes = null;
	 
		for(int slice=0; slice < volume.getNSlices(); slice++) 
		{
			for(int row=0; row < volume.getNRows(); row++) 
			{
				for(int col=0; col < volume.getNCols(); col++) 
				{
					amplitudes = volume.getChannelDouble(row,col,slice);						
					binmap.add(row,col,slice,amplitudes, t2s);
				}
			}	
		}
	}

	/** Save the bins.
	 *
	 */
	public void saveBins(String basename, Vector header_in)
	{
		binmap.save(basename, header_in);
	}

	/**
	 *  Write function to write the set of data.
	 *
	 *  @param ds - Little endian data input stream.
	 *  @param type - String representation of the underlying data type (ex. "ushort")
	 */
	public void write( ImageOutputStream ds, int datatype ) 
	{
		if( volume == null ) {
			throw (new NullPointerException("SpectralVolume::write: The volume has not been defined"));
		}

		for(int ii=0; ii<volume.getNSlices(); ii++) {
			for(int jj=0; jj<volume.getNChannels(); jj++) {
				volume.write(ds, ii, jj, datatype);
			}
		}
	}

	/**
	 *  Write function to write the set of data.  The default output type is 
	 *  the "ushort".
	 *
	 *  @param ds - Little endian data input stream.
	 */
	public void write( ImageOutputStream ds ) 
	{
		write(ds, MRIFile.DATATYPE_FLOAT );
	}

	/**
	 *  setCoefs is used to set the amplitudes for a row, column and slice.
	 *
	 *  @param x - Double array of the amplitudes.
	 *  @param row - The row.
	 *  @param col - The column.
	 *  @param slice - The slice.
	 *
	 *  @deprecated - Should use setData.
	 */
	public void setCoefs(double[] x, int row, int column, int slice) 
	{
		volume.setChannel(x, row, column, slice);
	}

	/**
	 *  setData is used to set the amplitudes for a row, column and slice.
	 *
	 *  @param x - Double array of the amplitudes.
	 *  @param row - The row.
	 *  @param col - The column.
	 *  @param slice - The slice.
	 *
	 */
	public void setData(double x, int row, int column, int slice, int channel) 
	{
		volume.setData(x, row, column, slice, channel);
	}

	/**
	 *  setData is used to set the amplitudes for a row, column and slice.
	 *
	 *  @param x - Double array of the amplitudes.
	 *  @param row - The row.
	 *  @param col - The column.
	 *  @param slice - The slice.
	 *
	 */
	public void setData(double[] x, int row, int column, int slice) 
	{
		volume.setChannel(x, row, column, slice);
	}

	/**
	 *  getNRows will return the number of rows.
	 *
	 *  @return The number of rows.
	 */
    public int getNRows() 
	{
		return volume.getNRows();
	}

	/**
	 *  getNCols will return the number of columns.
	 *
	 *  @return The number of columns.
	 */
    public int getNCols() 
	{
		return volume.getNCols();
	}

	/**
	 *  getNSlices will return the number of slices.
	 *
	 *  @return The number of slices.
	 */
    public int getNSlices() 
	{
		return volume.getNSlices();
	}

	/**
	 *  getNT2 will return the number of T2 values.
	 *
	 *  @return The number of T2.
	 */
    public int getNT2() 
	{
		return volume.getNChannels();
	}

	/**
	 *  Used to display the size of the volume.
	 */
	public String toString() 
	{
		return "The volume is " + getNRows() + "x" + 
			getNCols() + "x" + getNSlices();
	}

	/**
	 *  Used to display the size of the volume.
	 */
	public void show()
	{
		System.out.println(toString());
	}

	public MCVolume getMCVolume()
	{
		return volume;
	}

	/** volume - The volume of amplitudes */
	private MCVolume volume = null;

	/** Vector of bins.. */
	private T2BinMaps binmap = null;
}
