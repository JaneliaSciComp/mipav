package gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats;

import java.io.*;
import java.lang.*;
import java.lang.Float;
import java.util.*;
import java.net.*;
import java.util.zip.*;
import java.nio.ByteOrder;
import javax.imageio.stream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;

/** BFF class.
 *
 *  The BFF class is made up of two sections.   The first section is the 
 *  header and the second section is the data.  The data and header
 *  are separated by a ^L^N
 *
 *  The primary keywords are:
 *       BFF, nrows, ncols, nslices, nchannels, nimages, endian and {data}type
 *
 *  The primary keywords are stripped out of the header variable after they
 *  are read in and then written to the file directly from the internal variables.
 *
 */

public class BFF extends BFFBase {

	protected Vector te;
	protected Vector tr;
	protected Vector ti;
	protected Vector sloc;
	protected Vector cmplx;
	int nchannels = 0;

	/** BFF default constructor.
	 *
	 */
	public BFF() 
	{
		initialize();
	}

	/** BFF constructor that reads in the file.
	 *
	 */
	public BFF(String filename) 
	{
		initialize();

		read( filename );
	}

	/** BFF constructor that takes a vector of strings that is the header.
	 *
	 */
	public BFF(Vector header) 
	{
		initialize();
		this.header = (Vector)header.clone();
		nimages = parseForInt("nslices") * parseForInt("nchannels");
		parseForString("type", true);
		parseForString("endian", true);
	}

	/** Initializer to make sure everything is normal.
	 *
	 */
	public void initialize() 
	{
		data = null;
		header = null;

		endian = ByteOrder.LITTLE_ENDIAN;
		datatype = MRIFile.DATATYPE_USHORT;
	}

	/* 
	 *  Remove the specified channel.
	 */
	public void removeVolume(final int index)
	{
		data.removeVolume(index);

		if( te != null )
			te.remove(index);
		if( tr != null )
			tr.remove(index);
		if( ti != null )
			ti.remove(index);
		if( sloc != null ) 
			sloc.remove(index);
		if( cmplx != null )
			cmplx.remove(index);

		if( te == null )
		{
			System.out.println("Te is null!!");
		}

		int flags = 0;
		flags |= UPDATE_CHANNELS;
		updateHeader(flags);
	}

	/** Read header from the input stream.
	 *
	 */
	public void readHeader( ImageInputStream ldis ) 
	{
		String line;

		header = new Vector(20);

		try {
			//
			//  Read in each line of the header.
			//
			do {
				line = ldis.readLine();

				if( line.charAt(0) != '\014' ) {
					header.add( line );
				}

			} while( line.charAt(0) != '\014' );

			parseHeader();
		}
		catch( IOException e ) {
			System.out.println("BFF: Could not read in the header");
			System.out.println(e);
			System.exit(0);
		}
	}

	/** Read the data from the input stream.
	 *
	 */
	public void readData( ImageInputStream ldis ) 
	{
		ldis.setByteOrder(endian);

		//
		//  Now read in the data.
		//
		for(int slice=0; slice<data.getNSlices();slice++) {
			for(int echo=0; echo<data.getNChannels();echo++) {
				data.read( ldis, slice, echo, datatype );
			}
		}
	}

	/** Write the header to the output data stream.
	 *
	 */
	public void writeHeader( ImageOutputStream ldos ) 
	{
		String thestring = "";

		try 
		{
			header.insertElementAt("nrows: " + data.getNRows(), 2);
			header.insertElementAt("ncols: " + data.getNCols(), 3);
			header.insertElementAt("nslices: " + data.getNSlices(), 4);
			header.insertElementAt("nchannels: " + data.getNChannels(), 5);
			header.insertElementAt("nimages: " + nimages, 5);
			header.insertElementAt("type: " + MRIFile.dataTypeToString(datatype), 5);
			header.insertElementAt("endian: " + MRIFile.dataEndianToString(endian), 5);

			// TE
			if( te != null ) 
			{
				thestring = "";
				for(int ii=0; ii<te.size(); ii++) 
				{
					thestring += ((Float)te.get(ii)).toString();
					if( ii != te.size()-1) thestring += ", ";
				}
				header.insertElementAt("te: " + thestring, 5);
			}

			// TR
			if( tr != null ) 
			{
				thestring = "";
				for(int ii=0; ii<tr.size(); ii++) 
				{
					thestring += ((Float)tr.get(ii)).toString();
					if( ii != tr.size()-1) thestring += ", ";
				}
				header.insertElementAt("tr: " + thestring, 5);
			}

			// TI
			if( ti != null  && ti.size() > 0 ) 
			{
				thestring = "";
				for(int ii=0; ii<ti.size(); ii++) 
				{
					thestring += ((Float)ti.get(ii)).toString();
					if( ii != ti.size()-1) thestring += ", ";
				}
				header.insertElementAt("ti: " + thestring, 5);
			}

			// Slice Location
			if( sloc != null && sloc.size() > 0 ) 
			{
				thestring = "";
				for(int ii=0; ii<sloc.size(); ii++) 
				{
					thestring += (String)sloc.get(ii);
					if( ii != sloc.size()-1) thestring += ", ";
				}
				header.insertElementAt("sloc: " + thestring, 5);
			}


			// Complex
			if( cmplx != null ) 
			{
				thestring = "";
				for(int ii=0; ii<cmplx.size(); ii++) 
				{
					thestring += (String)cmplx.get(ii);
					if( ii != cmplx.size()-1) thestring += ", ";
				}
				header.insertElementAt("complex: " + thestring, 5);
			}


			for(int ii=0; ii<header.size(); ii++) {
				ldos.writeBytes((String)header.elementAt(ii) + "\n");
			}
			ldos.writeBytes((String)"\014\n");
		}
		catch( IOException e ) {
			System.out.println("BFF: Could not write header");
			System.exit(0);
		}
	}

	/** Write the data to the output data stream with the type "type".
	 *
	 */
	public void writeData( ImageOutputStream ldos ) 
	{
		ldos.setByteOrder(endian);

		for(int slice=0; slice<data.getNSlices();slice++) {
			for(int echo=0; echo<data.getNChannels();echo++) {
				data.write( ldos, slice, echo, datatype );
			}
		}
	}

	/** Get a copy of the TE times.
	 *
	 *  NOTE:  This will create a copy of the first
	 *         _nechoes_ TE times and not ALL the TE
	 *         times in the vector.
	 *
	 *         There was a problem with some code that
	 *         was using the length of the vector as the 
	 *         number of TE times even though the TE times
	 *         are listed for each slice as well.
	 *
	 */
    public double[] getTE() 
	{
		Object[] temp = te.toArray();
		double[] teout = new double[nchannels];
		for(int ii=0; ii<nchannels; ii++)
		{
			teout[ii] = ((Float)temp[ii]).doubleValue();
		}

		return teout;
	}

	public void updateHeader(int flags) 
	{ 
		if ((flags&UPDATE_CHANNELS) != 0 || ( (flags&UPDATE_SLICES) != 0 ) )
		{
			nimages = data.getNSlices() * data.getNChannels();
		}
	}

	/** Parse the header.
	 *
	 */
	protected void parseHeader() 
	{
		nchannels = -1;
		int nechoes = -1;

		int nrows = parseForInt("nrows:", true);
		int ncols = parseForInt("ncols:", true);
		int nslices = parseForInt("nslices:", true);
		nimages = parseForInt("nimages:", true);


		nchannels = parseForInt("nchannels:", true);
		nechoes = parseForInt("nechoes:", true);

		if( nchannels == -1 ) 
		{
			System.out.println("Old style BFF, going to set nchannels = nechoes");
			nchannels = nechoes;
		}

		datatype = MRIFile.dataTypeFromString( parseForString("type:", true) );

		if( parseForString("endian:", true).trim().compareTo("big") == 0 ) 
		{
			endian = ByteOrder.BIG_ENDIAN;
		}
		else 
		{
			endian = ByteOrder.LITTLE_ENDIAN;
		}

		te = parseForDoubleVec("te:", true);
		tr = parseForDoubleVec("tr:", true);
		ti = parseForDoubleVec("ti:", true);
		sloc = parseForStringVec("sloc:", true);
		cmplx = parseForStringVec("complex:", true);

		data = new MCVolumeFile(nrows, ncols, nslices, nchannels);
	}
	
}