package gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats;

import java.io.*;
import java.lang.*;
import java.lang.Float;
import java.util.*;
import java.text.*;
import java.util.zip.*;
import javax.imageio.stream.*;
import java.nio.ByteOrder;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.MRIFile;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;

public class TBF extends MRIFile {

	public TBF(String filename) {
		initialize();

		read(filename);
	}

	public void initialize()
	{
		endian = ByteOrder.BIG_ENDIAN;
		datatype = MRIFile.DATATYPE_USHORT;

		buffer = new byte[ 512 ];
	}

	public void readHeader( String filename ) {
		readHeader( AppKit.openRead(filename) ); 
	}

	public void readHeader( ImageInputStream ldis )
	{
		try {
			 ldis.readFully(buffer, 0, 512);
			 dim_x = 256;
			 dim_y = 256;
		}
		catch( EOFException e ) {
			System.out.println(e.getMessage());
			System.out.println("End of file for some reason");
			System.exit(0);
		}
		catch( IOException e ) {
			System.out.println(e.getMessage());
			System.out.println("Could not read header");
			System.exit(0);
		}
	}

	public int getNEchoes()
	{
		return echo_count;
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
		System.out.println("TBF: TE not defined in this file format.");
		return null;
	}

	public void read( String filename ) {
		ImageInputStream ldis = null;
		long file_size = 0;

		echo_count = (int)(file_size / ( 256L * 256L * 2L ));

		ldis = AppKit.openRead(filename);

		ldis.setByteOrder(endian);


		readHeader(ldis);

		//
		//  Now read in the data.
		//
		data = new MCVolume(dim_x, dim_y, 1, getNEchoes());
		for(int echo=0; echo<getNEchoes(); echo++) {
			data.read(ldis, 0, echo, datatype);
		}

		try {
			ldis.close();
		}
		catch( IOException e ) {
			System.out.println("TBF: Weird, could not close file.");
			System.exit(-1);
		}
	}

	public void write(String filename) {
		ImageOutputStream ldos = null;

		//
		//  Open the file for output.
		//
		ldos = AppKit.openWrite(filename);
		
		if( ldos == null ) {
			System.out.println("TBF: Could not open " + filename + " for writing.");
			System.exit(0);
		}

		ldos.setByteOrder(ByteOrder.BIG_ENDIAN);

		//
		//  Write the header.
		//
		writeHeader(ldos);

		//
		//  Write the data.
		//
		for(int echo=0; echo<getNEchoes(); echo++) {
			data.write( ldos, 0, echo, datatype );
		}

		//
		//  Close the file.
		//
		try {
			ldos.close();
		}
		catch( IOException e ) {
			System.out.println("Could not close file.");
			System.exit(-1);
		}
	}

	public void writeHeader( ImageOutputStream ldos )
	{
		try {
			ldos.write(buffer, 0, 512);
		}
		catch( EOFException e ) {
			System.out.println(e.getMessage());
			System.out.println("End of file for some reason");
			System.exit(0);
		}
		catch( IOException e ) {
			System.out.println(e.getMessage());
			System.out.println("Could not read header");
			System.exit(0);
		}
	}

	public void show() {
		gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " + getNEchoes() + "\n", "echo_count");
		gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " + dim_x + "\n", "dim_x");
		gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " + dim_y + "\n", "dim_y");
	}

	//
	//  private variables
	//
    private byte[] buffer = null;
    private int echo_count;
    private int dim_x;
    private int dim_y;
}
