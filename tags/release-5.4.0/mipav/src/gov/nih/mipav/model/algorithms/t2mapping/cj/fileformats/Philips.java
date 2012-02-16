package gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats;

import java.io.*;
import java.lang.*;
import java.lang.Float;
import java.util.*;
import java.net.*;
import java.nio.ByteOrder;
import java.util.zip.*;
import javax.imageio.stream.*;
import java.nio.ByteOrder;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;

public class Philips extends MRIFile 
{
	int x_dim, y_dim, z_dim, t_dim;	

	//-----------------------------------------------------
	//
	//  Constructor
	//
	public Philips() {
		initialize();
	}

	public Philips(String filename) {
		initialize();

		read( filename );
	}

	public void initialize() {
	}

	public void readHeader( final String filename ) {

		ImageInputStream iis = null;
		String line;
		
		//---------------------------------------------------
		//
		//  Read header first.
		//
		String temp_filename = filename;
		if( !filename.endsWith(".PAR") && !filename.endsWith(".par") ) 
		{
			temp_filename += ".PAR";
		}

		iis = AppKit.openRead( temp_filename, true );
		if( iis == null ) {
			System.out.println("Philips: Could not open " +
			                    temp_filename+ " to read.");
			System.exit(0);
		}

		readHeader( iis );
	}

	public void readHeader( ImageInputStream iis ) {

		endian = ByteOrder.LITTLE_ENDIAN;
		boolean count = false;
		int num_ims = 0;
		boolean EOF = false;

		//  Read in each line and see if it is one we need.
		datatype = MRIFile.DATATYPE_SHORT;

		while( !EOF )
		{
			try 
			{
				String line = iis.readLine();

				if( line == null ) { EOF = true; } 

				if( count ) 
				{ 
					num_ims++; 
					System.out.print("\r " + num_ims);
				}

				if( !count && !EOF )
				{
					// x_dim and y_dim
					if( line.length() > 12 && line.indexOf("Recon resolu") != -1 )
					{
						int ind = line.indexOf(":");	
						String ss = line.substring(ind+1).trim();
						ind = ss.indexOf("  ");
						x_dim = Integer.parseInt(ss.substring(0, ind).trim());
						y_dim = Integer.parseInt(ss.substring(ind+1).trim());
						System.out.println("File is " + x_dim + " by " + y_dim);
					}

					// z_dim 
					if( line.length() > 30 && line.indexOf("Max. number of slices/locations") != -1 )
					{
						int ind = line.indexOf(":");	
						String ss = line.substring(ind+1).trim();
						z_dim = Integer.parseInt(ss);
						System.out.println("File is " + z_dim);
					}

					if( line.length() > 4 && line.startsWith("#sl ") )
						{ count = true; }
				}
			}
			catch( EOFException e) {
				EOF = true;
			}
			catch( IOException e ) {
				System.out.println("Philips: Could not read in the header");
				System.exit(0);
			}
		}

		try { iis.close(); } catch (IOException e2) {System.exit(0);};

		t_dim = (num_ims-4) / z_dim;
		System.out.println("t_dim is " + t_dim);
	}

	public void read( String filename ) {

		String line;
		
		if( filename.endsWith(".REC") )
		{
			readHeader( filename.replaceAll(".REC", ".PAR") );
		}
		else if( filename.endsWith(".rec") )
		{
			readHeader( filename.replaceAll(".rec", ".par") );
		}

		//---------------------------------------------------
		//
		//  Read in the data
		//
		ImageInputStream iis = AppKit.openRead(filename);
		if( iis == null ) {
			System.out.println("Philips: Could not open " + 
				 	            filename + " to read.");
			System.exit(0);
		}

		//
		//  Now read in the data.
		//
		data = new MCVolumeFile(x_dim, y_dim, z_dim, t_dim);
		for(int echo=0; echo<t_dim;echo++) {
			for(int slice=0; slice<z_dim;slice++) {
				data.read( iis, slice, echo, datatype );
			}
		}

		try {
			iis.close();
		}
		catch( IOException e ) {
			System.out.println("Philips: Could not close " + filename );
			System.exit(0);
		}
	}

	/** 
	 *  Write out the Philips file.
	 */
	public void write( String filename ) {
//		String line;
//		
//		//---------------------------------------------------
//		//
//		//  Read header first.
//		//
//		ImageOutputStream ios = AppKit.openWrite(filename + ".hdr");
//		if( ios == null ) {
//			System.out.println("Philips: Could not open " + 
//			                    filename + " to read.");
//			System.exit(0);
//		}
//
//		/* Set the endian for writing out the header. */
//		ios.setByteOrder(endian);
//
//		try {
//			ios.writeInt(sizeof_hdr ); /* For ANALYZE compatibility only */
//			ios.write(pad1, 0, 28);
//			ios.writeInt(extents );    /* For ANALYZE compatibility only */
//			ios.write(pad2, 0, 2);
//			ios.write(regular, 0, 1);;    /* For ANALYZE compatibility only */
//			ios.write(pad3, 0, 1);
//			ios.writeShort(dims );       /* For ANALYZE compatibility only */
//			ios.writeShort(x_dim );      /* AIR */
//			ios.writeShort(y_dim );      /* AIR */
//			ios.writeShort(z_dim );      /* AIR */
//			ios.writeShort(t_dim );      /* For ANALYZE compatibility only */
//			ios.write(pad4, 0, 20);
//			ios.writeShort(air_datatype );   /* For ANALYZE compatibility only */
//			ios.writeShort(bits );       /* AIR */
//			ios.write(pad5, 0, 6);
//			ios.writeFloat(x_size );     /* AIR */
//			ios.writeFloat(y_size );     /* AIR */
//			ios.writeFloat(z_size );     /* AIR */
//			ios.write(pad6, 0, 48);
//			ios.writeInt(glmax );      /* AIR */
//			ios.writeInt(glmin );      /* AIR */
//			ios.write(descrip, 0, 80);    /* AIR (non-essential) */
//			ios.write(pad7, 0, 120);
//
//			ios.close();
//		}
//		catch( IOException e ) {
//			System.out.println("Philips: Could not read in the header");
//			System.exit(0);
//		}
//
//		//---------------------------------------------------
//		//
//		//  Write in the data
//		//
//		ImageOutputStream ldos = AppKit.openWrite(filename + ".img");
//		if( ldos == null ) {
//			System.out.println("Philips: Could not open " + 
//				               filename + " to write.");
//			System.exit(0);
//		}
//
//		/* Set the endian for writing out the data. */
//		ldos.setByteOrder(endian);
//
//		//
//		//  Now write out the data.
//		//
//		for(int slice=0; slice<z_dim;slice++) {
//			for(int echo=0; echo<t_dim;echo++) {
//				data.write( ldos, slice, echo, datatype );
//			}
//		}
//
//		try {
//			ldos.close();
//		}
//		catch( IOException e ) {
//			System.out.println("Philips: Could not close " + filename );
//			System.exit(0);
//		}
	}

	/** Return the list of TEs, but, they are not defined in the 
	 *  Philips format so they will have to figure out some otherway
	 *  of getting that info.
	 *
	 *  @return null - Nothing to return (though null is probably 
	 *                 a stupid way of doing this).
	 */
	public double[] getTE()
	{
		System.out.println("Philips: TE not defined in this file format.");
		return null;
	}

	public void show() {
	}

	public Slice getSlice( int slice_num, int channel ) {
		return data.getSlice( slice_num, channel );
	}

	public float[] getChannels( int row, int col, int slice ) {
		return data.getChannel(row,col,slice); 
	}

	public void addComment(String comment) {
	}

}
