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

public class BFFSpectral extends BFFBase {

	private double t2[];
	private SpectralVolume sv;

	/** Constructor that takes a vector header.
	 *
	 */
	public BFFSpectral(Vector header) {
		initialize();

		this.header = (Vector)header.clone();
	}

	/** Most likely constructor -- read in a file.
	 *
	 */
	public BFFSpectral(String filename) {
		initialize();

		read( filename );
	}

	/** Initializer
	 *
	 */
	public void initialize() {
		endian = ByteOrder.LITTLE_ENDIAN;
		datatype = MRIFile.DATATYPE_FLOAT;

		data = null;
		sv = null;
		header = null;
	}

	/** Return the list of TEs, but, they are not defined in the 
	 *  Analyze format so they will have to figure out some otherway
	 *  of getting that info.
	 *
	 *  @return null - Nothing to return (though null is probably 
	 *                 a stupid way of doing this).
	 */
	public double[] getTE()
	{
		System.out.println("BFFSpectral: TE not defined in this file format.");
		return null;
	}

	/** Retrieve the spectral volume (or at least a copy of it).
	 *
	 */
	public SpectralVolume getSpectralVolume()
	{
		return (SpectralVolume)sv.clone();
	}

	/** Read in the header. 
	 */
	public void readHeader( ImageInputStream ldis ) 
	{
		String line;
		header = new Vector(0);

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
			System.out.println("BFFSpectral: Could not read in the header");
			System.exit(0);
		}

		ldis.setByteOrder(endian);
	}

	/** Read in the data.
	 *
	 */
	public void readData( ImageInputStream ldis ) 
	{
		sv.read( ldis );
	}

	/** Write the data with the default type -- ahhh always float.
	 *
	 */
	public void writeData( ImageOutputStream ldos ) 
	{
		sv.write( ldos, datatype );
	}

	/** Write the header out.
	 *
	 */
	public void writeHeader( ImageOutputStream ldos ) 
	{
		System.out.println("going to write spectral header.");

		//
		//  Set the number of images.
		//
		parseForInt("nimages", true);

		try {
			header.insertElementAt("nimages: " + (sv.getNSlices()*sv.getNT2()), 2);
		}
		catch( NullPointerException e ) {
			System.out.println("Spectral Volume is a null pointer....");
			System.exit(-1);
		}
		
		//
		//  Set the t2s.
		//
		String temp = "t2s: ";
		for(int ii=0; ii<t2.length; ii++) {
			temp = temp + t2[ii];

			if( ii < t2.length-1 ) {
				temp += ", ";
			}
		}
		header.insertElementAt(temp, 3);

		try {
			header.insertElementAt("nrows: " + sv.getNRows(), 2);
			header.insertElementAt("ncols: " + sv.getNCols(), 3);
			header.insertElementAt("nslices: " + sv.getNSlices(), 4);
			header.insertElementAt("nchannels: " + sv.getNT2(), 5);
			header.insertElementAt("nimages: " + sv.getNSlices()*sv.getNT2(), 5);
			header.insertElementAt("type: " + MRIFile.dataTypeToString(datatype), 5);
			header.insertElementAt("endian: " + MRIFile.dataEndianToString(endian), 5);
			for(int ii=0; ii<header.size(); ii++) {
				ldos.writeBytes((String)header.elementAt(ii) + "\n");
			}
			ldos.writeBytes((String)"\014\n");
		}
		catch( IOException e ) {
			System.out.println("BFFSpectral: Could not write header");
			System.out.println(e);
			System.exit(0);
		}
	}

	/** Set the T2 values.
	 *
	 */
    public void setT2(double[] t2_in) 
	{
		t2 = new double[ t2_in.length ];
		System.arraycopy(t2_in, 0, t2, 0, t2_in.length);
	}

	/** Retrieve a copy of the T2 values.
	 *
	 */
    public double[] getT2() 
	{
		double[] t2out = new double[ t2.length ];

		System.arraycopy(t2, 0, t2out, 0, t2.length);

		return t2out;
	}

	/** Show the header.
	 *
	 */
	public void show() 
	{
		for(int ii=0; ii<header.size(); ii++) {
			System.out.println(header.elementAt(ii));
		}
	}

	/** Set the spectral volume.
	 *
	 */
	public void setSpectralVolume(SpectralVolume sv) 
	{
		this.sv = (SpectralVolume)sv.clone();
	}

	/** Parse the header.
	 *
	 */
	protected void parseHeader() 
	{
		int nrows = parseForInt("nrows:", true);
		int ncols = parseForInt("ncols:", true);
		int nslices = parseForInt("nslices:", true);
		parseForInt("nchannels:", true);
		parseForInt("nimages:", true);


		Vector temp_t2 = parseForDoubleVec("t2s:");
		int nchannels = temp_t2.size();

		t2 = new double[nchannels];
		for(int ii=0; ii<nchannels; ii++)
		{
			t2[ii] = ((Float)temp_t2.get(ii)).doubleValue();
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


		sv = new SpectralVolume(nrows, ncols, nslices, nchannels);
	}
}
