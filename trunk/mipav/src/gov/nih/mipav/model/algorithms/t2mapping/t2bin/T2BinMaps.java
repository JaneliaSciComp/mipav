package gov.nih.mipav.model.algorithms.t2mapping.t2bin;

import java.io.*;
import java.lang.*;
import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2BinMap;

/** The T2BinMaps class holds a set of T2BinMap.
 *
 */
public class T2BinMaps
{
	int nrows, ncols, nslices;
	private Vector bins = null;

	/** Constructor to set the number of rows,  columsn and slices.
	 *
	 */
	public T2BinMaps(int nrows, int ncols, int nslices) 
	{
		this.nrows = nrows;
		this.ncols = ncols;
		this.nslices = nslices;
		bins = new Vector();
	}

	/**  Define the start and stopping point of the bin as well as the name.
	 *
	 */
	public void define(double start, double stop, String name)
	{
		T2BinMap temp = new T2BinMap(nrows, ncols, nslices);
		temp.define(start, stop, name);
		bins.add( temp );
	}

	/**  Add a info for a row/col/slice.
	 *
	 */
	public void add(int row, int col, int slice, double[] spec_amp, double[] spec_time)
	{
		for(int ii=0; ii<bins.size(); ii++)
		{
			((T2BinMap)bins.get(ii)).add(row,col,slice,spec_amp, spec_time);
		}
	}

	/** Requisite toString to show what the bin is...
	 *
	 */
	public String toString()
	{
		return "";
	}

	/** Save the bins.
	 *
	 */
	public void save(String basename, Vector more_header)
	{
		for(int ii=0; ii<bins.size(); ii++)
		{
			((T2BinMap)bins.get(ii)).save(basename, more_header);
		}
	}

}
