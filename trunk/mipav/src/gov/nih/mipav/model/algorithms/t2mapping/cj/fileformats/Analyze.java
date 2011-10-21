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

public class Analyze extends MRIFile 
{

	int sizeof_hdr;	/* For ANALYZE compatibility only */
	byte[] pad1 = null;
	int extents;	/* For ANALYZE compatibility only */
	byte[] pad2 = null;
	byte[] regular = null;	/* For ANALYZE compatibility only */
	byte[] pad3 = null;
	short dims;		/* For ANALYZE compatibility only */
	short x_dim;		/* AIR */
	short y_dim;		/* AIR */
	short z_dim;		/* AIR */
	short t_dim;		/* For ANALYZE compatibility only */
	byte[] pad4 = null;
	short air_datatype;	/* For ANALYZE compatibility only */
	short bits;		/* AIR */
	byte[] pad5 = null;
	float x_size;		/* AIR */
	float y_size;		/* AIR */
	float z_size;		/* AIR */
	byte[] pad6 = null;
	int glmax;		/* AIR */
	int glmin;		/* AIR */
	byte[] descrip = null;	/* AIR (non-essential) */
	byte[] pad7 = null;
	//}

	double[] te = null;
	
	//-----------------------------------------------------
	//
	//  Constructor
	//
	public Analyze() {
		initialize();
	}

	public Analyze(String filename) {
		initialize();

		read( filename );
	}

	public void initialize() {
		pad1 = new byte[28];
		pad2 = new byte[2];
		pad3 = new byte[1];
		regular = new byte[1];
		pad4 = new byte[20];
		pad5 = new byte[6];
		pad6 = new byte[48];
		descrip = new byte[80];
		pad7 = new byte[120];
		data = null;
	}

	public void readHeader( final String filename ) {

		ImageInputStream iis = null;
		String line;

		endian = ByteOrder.LITTLE_ENDIAN;
		
		//---------------------------------------------------
		//
		//  Read header first.
		//
		String temp_filename = filename;
		if( !filename.endsWith(".hdr") ) 
		{
			temp_filename += ".hdr";
		}

		iis = AppKit.openRead( temp_filename, endian == ByteOrder.LITTLE_ENDIAN );
		if( iis == null ) {
			System.out.println("Analyze: Could not open " +
			                    temp_filename+ " to read.");
			System.exit(0);
		}

		//  A test of the endianness.	
		int sizeof_hdr = 0;
		try
		{
			sizeof_hdr = iis.readInt(); /* For ANALYZE compatibility only */
		}
		catch( IOException e ) {
			System.out.println("Analyze: Could not read in the header");
			System.exit(0);
		}

		if( sizeof_hdr < 0 || sizeof_hdr > 500 )
		{
			endian = ByteOrder.BIG_ENDIAN;
			iis = AppKit.openRead( temp_filename, (endian == ByteOrder.LITTLE_ENDIAN ) );
		}

		readHeader( iis );
	}

	public void readHeader( ImageInputStream iis ) {

		try {
			sizeof_hdr = iis.readInt(); /* For ANALYZE compatibility only */
			iis.read(pad1, 0, 28);
			extents = iis.readInt();    /* For ANALYZE compatibility only */
			iis.read(pad2, 0, 2);
			iis.read(regular, 0, 1);;    /* For ANALYZE compatibility only */
			iis.read(pad3, 0, 1);
			dims = iis.readShort();       /* For ANALYZE compatibility only */
			x_dim = iis.readShort();      /* AIR */
			y_dim = iis.readShort();      /* AIR */
			z_dim = iis.readShort();      /* AIR */
			t_dim = iis.readShort();      /* For ANALYZE compatibility only */
			iis.read(pad4, 0, 20);
			air_datatype = iis.readShort();   /* For ANALYZE compatibility only */

			switch( air_datatype ) {
				case 2:
					datatype = MRIFile.DATATYPE_BYTE;
				break;
				case 4:
					datatype = MRIFile.DATATYPE_SHORT;
				break;
				case 8:
					datatype = MRIFile.DATATYPE_INT;
				break;
				case 16:
					datatype = MRIFile.DATATYPE_FLOAT;
				break;
				case 64:
					datatype = MRIFile.DATATYPE_DOUBLE;
				break;
				default:
					datatype = MRIFile.DATATYPE_SHORT;
				break;
			}

			bits = iis.readShort();       /* AIR */
			iis.read(pad5, 0, 6);
			x_size = iis.readFloat();     /* AIR */
			y_size = iis.readFloat();     /* AIR */
			z_size = iis.readFloat();     /* AIR */
			iis.read(pad6, 0, 48);
			glmax = iis.readInt();      /* AIR */
			glmin = iis.readInt();      /* AIR */
			iis.read(descrip, 0, 80);    /* AIR (non-essential) */
			iis.read(pad7, 0, 120);

			iis.close();
		}
		catch( IOException e ) {
			System.out.println("Analyze: Could not read in the header");
			System.exit(0);
		}
	}

	public void read( String filename ) {

		String line;
		
		readHeader( filename + ".hdr" );

		//---------------------------------------------------
		//
		//  Read in the data
		//
		ImageInputStream iis = AppKit.openRead(filename + ".img", endian==ByteOrder.LITTLE_ENDIAN);
		if( iis == null ) {
			System.out.println("Analyze: Could not open " + 
				 	            filename + " to read.");
			System.exit(0);
		}

		//
		//  Now read in the data.
		//
		data = new MCVolumeFile(x_dim, y_dim, z_dim, t_dim);
//		data = new MCVolume(x_dim, y_dim, z_dim, t_dim);
		for(int slice=0; slice<z_dim;slice++) {
			for(int echo=0; echo<t_dim;echo++) {
				System.out.println("reading " + echo + " " + slice );
				data.read( iis, slice, echo, datatype );
			}
		}

		try {
			iis.close();
		}
		catch( IOException e ) {
			System.out.println("Analyze: Could not close " + filename );
			System.exit(0);
		}

		readTE( filename );
	}
	
	/*
	 *  See if we can open a file called <filname>.te
	 */
	private double[] readTE( String filename )
	{
		ArrayList<Double> teal = new ArrayList<Double>(10);
		String thisLine = null;

		try {
			FileInputStream fin =  new FileInputStream(filename + ".te");
			// JDK1.1+
			BufferedReader myInput = new BufferedReader
					(new InputStreamReader(fin));
			while ((thisLine = myInput.readLine()) != null) {  
				System.out.println(thisLine);
				teal.add( new Double( thisLine.trim() ) );
			}
		}
		catch (NumberFormatException e) {
			System.out.println(e);
			System.out.println("in file " + filename + ".te");
			System.exit(-1);
		}
		catch (FileNotFoundException e) {
			System.out.println("File " + filename + ".te does not exist");
		}
		catch (IOException e) {
			System.out.println("File " + filename + ".te could not be read");
			System.exit(-1);
		}

		//  Copy it into the array.
		te = new double[ teal.size() ];
		for(int ii=0; ii<teal.size(); ii++) { te[ii] = ((Double)teal.get(ii)).doubleValue(); }
   		
		return te;
   }		

	/** 
	 *  Write out the Analyze file.
	 */
	public void write( String filename ) {
		String line;
		
		//---------------------------------------------------
		//
		//  Read header first.
		//
		ImageOutputStream ios = AppKit.openWrite(filename + ".hdr");
		if( ios == null ) {
			System.out.println("Analyze: Could not open " + 
			                    filename + " to read.");
			System.exit(0);
		}

		/* Set the endian for writing out the header. */
		ios.setByteOrder(endian);

		try {
			ios.writeInt(sizeof_hdr ); /* For ANALYZE compatibility only */
			ios.write(pad1, 0, 28);
			ios.writeInt(extents );    /* For ANALYZE compatibility only */
			ios.write(pad2, 0, 2);
			ios.write(regular, 0, 1);;    /* For ANALYZE compatibility only */
			ios.write(pad3, 0, 1);
			ios.writeShort(dims );       /* For ANALYZE compatibility only */
			ios.writeShort(x_dim );      /* AIR */
			ios.writeShort(y_dim );      /* AIR */
			ios.writeShort(z_dim );      /* AIR */
			ios.writeShort(t_dim );      /* For ANALYZE compatibility only */
			ios.write(pad4, 0, 20);
			ios.writeShort(air_datatype );   /* For ANALYZE compatibility only */
			ios.writeShort(bits );       /* AIR */
			ios.write(pad5, 0, 6);
			ios.writeFloat(x_size );     /* AIR */
			ios.writeFloat(y_size );     /* AIR */
			ios.writeFloat(z_size );     /* AIR */
			ios.write(pad6, 0, 48);
			ios.writeInt(glmax );      /* AIR */
			ios.writeInt(glmin );      /* AIR */
			ios.write(descrip, 0, 80);    /* AIR (non-essential) */
			ios.write(pad7, 0, 120);

			ios.close();
		}
		catch( IOException e ) {
			System.out.println("Analyze: Could not read in the header");
			System.exit(0);
		}

		//---------------------------------------------------
		//
		//  Write in the data
		//
		ImageOutputStream ldos = AppKit.openWrite(filename + ".img");
		if( ldos == null ) {
			System.out.println("Analyze: Could not open " + 
				               filename + " to write.");
			System.exit(0);
		}

		/* Set the endian for writing out the data. */
		ldos.setByteOrder(endian);

		//
		//  Now write out the data.
		//
		System.out.println("Writing out " + z_dim + " slices and " + t_dim + " echoes ");
		for(int slice=0; slice<z_dim;slice++) {
			for(int echo=0; echo<t_dim;echo++) {
				data.write( ldos, slice, echo, datatype );
			}
		}

		try {
			ldos.close();
		}
		catch( IOException e ) {
			System.out.println("Analyze: Could not close " + filename );
			System.exit(0);
		}
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
		System.out.println("here");
		if( te == null )
			System.out.println("te is null");

		return te;
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

	public void SetPixelX(int x) { x_size = (float)x/(float)1000.0; }
	public void SetPixelY(int x) { y_size = (float)x/(float)1000.0; }
	public void SetSliceThick(int x) { z_size = (float)x/(float)1000.0; }

	public int GetPixelX() { return (int)(x_size * 1000.0); }
	public int GetPixelY() { return (int)(y_size * 1000.0); }
	public int GetSliceThick() { return (int)(z_size * 1000.0); }
}
