package gov.nih.mipav.model.algorithms.t2mapping.t2bin;

import java.io.*;
import java.lang.*;
import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Bin;

/** 
 *  Represents a map of a t2bin (start time to end time) and stores
 *  fr, dn, am, gm, hm, l2 and va.
 */
public class T2BinMap 
{

	/** Constructor to set the number of rows,  columsn and slices.
	 *
	 */
	public T2BinMap(int nrows, int ncols, int nslices) 
	{
		t2bin = new T2Bin();

		fr = new MCVolumeFile(nrows, ncols, nslices,  1);
		dn = new MCVolumeFile(nrows, ncols, nslices,  1);
		am = new MCVolumeFile(nrows, ncols, nslices,  1);
		gm = new MCVolumeFile(nrows, ncols, nslices,  1);
		hm = new MCVolumeFile(nrows, ncols, nslices,  1);
		l2 = new MCVolumeFile(nrows, ncols, nslices,  1);
		va = new MCVolumeFile(nrows, ncols, nslices,  1);
	}

	/**  Define the start and stopping point of the bin as well as the name.
	 *
	 */
	public void define(double start, double stop, String name)
	{
		this.start = start;
		this.stop = stop;
		this.name = name;

		t2bin.define(start, stop, name);
	}

	/**  Add a info for a row/col/slice.
	 *
	 */
	public void add(int row, int col, int slice, double[] spec_amp, double[] spec_time)
	{
		t2bin.calculate(spec_amp, spec_time);

		fr.setData( t2bin.getFR(), row, col, slice, 0);
		dn.setData( t2bin.getDN(), row, col, slice, 0);
		am.setData( t2bin.getAM(), row, col, slice, 0);
		gm.setData( t2bin.getGM(), row, col, slice, 0);
		hm.setData( t2bin.getHM(), row, col, slice, 0);
		l2.setData( t2bin.getL2(), row, col, slice, 0);
		va.setData( t2bin.getVA(), row, col, slice, 0);
	}

	/** Requisite toString to show what the bin is...
	 *
	 */
	public String toString()
	{
		return ( name + " from " + start + " to " + stop );
	}

	/**
	 *  Retrieve the fraction of the total distribution in this bin.
	 */
	public MCVolume getFR()
	{
		return (MCVolume)fr.clone();
	}

	/**
	 *  Retrieve the total distribution in this bin.
	 */
	public MCVolume getDN()
	{
		return (MCVolume)dn.clone();
	}

	/**
	 *  Retrieve the arithimetic mean T2 of this bin.
	 */
	public MCVolume getAM()
	{
		return (MCVolume)am.clone();
	}

	/**
	 *  Retrieve the geometric mean T2 of this bin.
	 */
	public MCVolume getGM()
	{
		return (MCVolume)gm.clone();
	}

	/**
	 *  Retrieve the harmonic mean T2 of this bin.
	 */
	public MCVolume getHM()
	{
		return (MCVolume)hm.clone();
	}

	/**
	 *  Retrieve the L2 of this bin.
	 */
	public MCVolume getL2()
	{
		return (MCVolume)l2.clone();
	}

	/**
	 *  Retrieve the variance of this bin.
	 */
	public MCVolume getVA()
	{
		return (MCVolume)va.clone();
	}

	/** 
	 *  Show the bin.
	 */
	public void show()
	{
		System.out.println( toString() );
	}

	/** 
	 *  Save the bins based on the specified basename 
	 *  passed in.
	 */
	public void save(String basename, Vector more_header)
	{
		saveMCVolume( fr, basename, "fr", more_header );
		saveMCVolume( dn, basename, "dn", more_header );
		saveMCVolume( am, basename, "am", more_header );
		saveMCVolume( gm, basename, "gm", more_header );
		saveMCVolume( hm, basename, "hm", more_header );
		saveMCVolume( l2, basename, "l2", more_header );
		saveMCVolume( va, basename, "va", more_header );
	}

	/** Save a bin
	 *
	 */
	private void saveMCVolume(MCVolume vol, String basename, String append_name, Vector more_header)
	{
		// Remove extraneous.
		for(int ii=0; ii<more_header.size(); ii++) 
		{
			if( ((String)more_header.elementAt(ii)).startsWith("nimages") ) {	
				more_header.removeElementAt(ii);
			}

			if( ((String)more_header.elementAt(ii)).startsWith("nrows") ) {	
				more_header.removeElementAt(ii);
			}

			if( ((String)more_header.elementAt(ii)).startsWith("ncols") ) {	
				more_header.removeElementAt(ii);
			}

			if( ((String)more_header.elementAt(ii)).startsWith("nslices") ) {	
				more_header.removeElementAt(ii);
			}

			if( ((String)more_header.elementAt(ii)).startsWith("nechoes") ) {	
				more_header.removeElementAt(ii);
			}

			if( ((String)more_header.elementAt(ii)).startsWith("type") ) {	
				more_header.removeElementAt(ii);
			}

			if( ((String)more_header.elementAt(ii)).startsWith("endian") ) {	
				more_header.removeElementAt(ii);
			}
		}

		BFF bff = new BFF(more_header);

		bff.addComment("t2part: Bin name " + name +
				" T2 range [" + start + ", " + stop + ")");
		bff.setMCVolume( vol );
		bff.write( basename + "_" + name + append_name + ".bff.gz", 
			MRIFile.DATATYPE_FLOAT );
	}

	private double start = 0.0;
	private double stop = 0.0;
	private String name = null;

	private MCVolume fr = null;
	private MCVolume dn = null;
	private MCVolume am = null;
	private MCVolume gm = null;
	private MCVolume hm = null;
	private MCVolume l2 = null;
	private MCVolume va = null;

	private T2Bin t2bin = null;
}
