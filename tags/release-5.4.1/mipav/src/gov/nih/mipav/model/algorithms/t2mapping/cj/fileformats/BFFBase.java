package gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats;

import java.io.*;
import java.lang.*;
import java.lang.Float;
import java.util.*;
import java.nio.ByteOrder;
import java.net.*;
import java.util.zip.*;
import javax.imageio.stream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;

/** BFFBase class is used as the super class of all BFF file types.  
 */
public abstract class BFFBase extends MRIFile {

	protected Vector header;
	protected boolean bigEndian_f;
	protected int nimages;

	/** Default constructor.
	 */
	public BFFBase()
	{
		initialize();
	}

	/** Constructor that takes a file.
	 */
	public BFFBase(String filename)
	{
		initialize();
	}

	/** Initializer 
	 */
	public void initialize() 
	{
		data = null;
		header = null;

		datatype = MRIFile.DATATYPE_SHORT;
		endian = ByteOrder.LITTLE_ENDIAN;
	}

	/** Abstract readHeader method that must be defined elsewhere.
	 */
	public abstract void readHeader( ImageInputStream ldis );

	/** Abstract readData method that must be defined elsewhere.
	 */
	public abstract void readData( ImageInputStream ldis );

	/** Abstract write Header that must be defined in the subclass.
	 */
	public abstract void writeHeader( ImageOutputStream ldos );

	/** Abstract write Data that must be defined in the subclass.
	 */
	public abstract void writeData( ImageOutputStream ldos );
	
	/** Abstract parseheader that must be defined in the subclass.
	 */
	abstract void parseHeader();
	

	/** Read header from a file.
	 *
	 */
	public void readHeader( String filename ) 
	{
		ImageInputStream ldis = null;
		String line;
		
		//
		//  Read header first.
		//
		header = new Vector(0);

		ldis = AppKit.openRead(filename);
		if(ldis == null) {
			System.out.println("BFF: Could not open " + filename + " to read.");
			System.exit(0);
		}

		readHeader( ldis );
	}

	/** Read method that reads from a file.
	 */
	public void read( String filename ) 
	{

		ImageInputStream ldis = null;
		String line;
		
		//
		//  Read header first.
		//
		header = new Vector(0);

		ldis = AppKit.openRead(filename);
		if( ldis == null ) {
			System.out.println("BFF: Could not open " + filename + " to read.");
			System.exit(0);
		}

		readHeader( ldis );
		readData( ldis );

		try 
		{
			ldis.close();
		}
		catch( IOException e )
		{
			System.out.println("BFF: Could not close " + filename );
			System.exit(0);
		}
	}

	/** Get a copy of the header.
	 */
	public Vector getHeader() 
	{
		return (Vector)header.clone();
	}

	/** Write method that writes to the specified file and writes
	 *  the data in the specified type.
	 */
	public void write( String filename, int datatype ) 
	{
		System.out.println("Entering BFFBase::write with " + filename);
		
		ImageOutputStream ldos = null;
		GZIPOutputStream gzos = null;
		String line;

		OutputStream fos = null;

		try 
		{
			fos = new FileOutputStream(new File(filename));
			if( filename.endsWith(".gz") )
			{
				gzos = new GZIPOutputStream( fos ); 
			}
			
			fos = new BufferedOutputStream( gzos );
		} 
		catch (IOException e)
		{
			System.out.println(e); 
			System.exit(-1);
		}

		ldos = new MemoryCacheImageOutputStream(fos);

		setDataType( datatype );

		parseForString("type:", true);
		header.insertElementAt("type: " + MRIFile.dataTypeToString(datatype), 3);
		
//		ldos = AppKit.openWrite(filename);
//		if( ldos == null ) {
//			System.out.println("BFF: Could not open " + filename + " to write.");
//			System.exit(0);
//		}

		writeHeader( ldos );

		writeData( ldos );

		try {
			ldos.close();
			gzos.finish();
		}
		catch( IOException e ) {
			System.out.println("BFF: Could not close " + filename );
			System.exit(0);
		}

		System.out.println("Leaving BFFBase::write");
	}

	/** Write method that writes the data in an unsigned short format.
	 */
	public void write( String filename )
	{
		write( filename, datatype ); 
	}

	/** Show the header.
	 */
	public void show() 
	{
		for(int ii=0; ii<header.size(); ii++) {
			System.out.println(header.elementAt(ii));
		}
	}

	/** Retrieve the specified slice.
	 */
	public Slice getSlice( int slice_num, int channel ) 
	{
		return data.getSlice( slice_num, channel );
	}

	/** Get the channels in a float format. 
	 *
	 */
	public float[] getChannels( int row, int col, int slice ) 
	{
		return data.getChannel(row,col,slice); 
	}

	/** Get the channels in a double format. 
	 *
	 */
	public double[] getChannelsDouble( int row, int col, int slice ) 
	{
		return data.getChannelDouble(row,col,slice); 
	}

	/** Add a comment to the header.
	 */
	public void addComment(String comment) 
	{
		String computer = AppKit.getHostName();

		if (computer != null)
		{
			computer = " on " + computer;
		}
		else
		{
			System.out.println("ge2bff: Could not determine the name of the host, continuing...");
			computer = "";
		}

		header.add("comment: " + comment  + 
			" (Processed on " + (new Date()).toString() + 
			computer + " by " + System.getProperty("user.name") + ")");	
	}

	protected int parseForInt( String keyword, boolean remove ) 
	{
		int toreturn = -1;
		String temp;
		Integer tempInt;

		for(int ii=0; ii<header.size(); ii++) {
			temp = (String)header.elementAt(ii);
	
			if( temp.indexOf(keyword) != -1 ) {
				tempInt = new Integer(temp.substring( temp.indexOf(":")+1 , 
				                                      temp.length() ).trim());
				toreturn = tempInt.intValue();

				if( remove ) {
					header.removeElementAt(ii);
					ii++;
				}
			}
		}

		return toreturn;
	}

	protected int parseForInt( String keyword )
	{
		return parseForInt( keyword, false );
	}

	protected String parseForString( String keyword ) 
	{
		return parseForString( keyword, false );
	}

	protected String parseForString( String keyword, boolean remove ) 
	{
		String toreturn = "";
		String temp;

		for(int ii=0; ii<header.size(); ii++) {
			temp = (String)header.elementAt(ii);

			if( temp.indexOf(keyword) != -1 ) {
				toreturn = temp.substring( temp.indexOf(":")+1 , 
										  temp.length() ).trim();

				if( remove ) {
					header.removeElementAt(ii);
					ii++;
				}
			}
		}
		return toreturn;
	}

	protected Vector parseForDoubleVec( String keyword ) 
	{
		return parseForDoubleVec(keyword, false);
	}

	protected Vector parseForStringVec( String keyword, boolean remove ) 
	{
		int toreturn = -1;
		String temp;
		int index;
		boolean found = false;
		Vector tempvector = new Vector();

		for(int ii=0; ii<header.size(); ii++) 
		{
			temp = (String)header.elementAt(ii);

			if( temp.indexOf(keyword) == 0 ) 
			{
				//
				//  Found the right line, now we need to parse it.
				//
				index = temp.indexOf(":")+1;
				boolean done = false;
				while( !done ) {
					//
					//  Look for the commas.
					//
					if( temp.indexOf(",", index+1 ) > 0 ) {
						tempvector.add( temp.substring(index+1, temp.indexOf(",", index+1) ).trim() );	
						index = temp.indexOf(",", index+1);
						found = true;
					}
					else {
						tempvector.add( temp.substring(temp.lastIndexOf(",")+1).trim() );	
						done = true;
					}
				}

				if( remove ) 
				{
					header.removeElementAt(ii);
					ii++;
				}
			}
		}

		return tempvector;
	}

	protected Vector parseForDoubleVec( String keyword, boolean remove ) 
	{
		int toreturn = -1;
		String temp;
		int index;
		boolean found = false;
		Vector tempvector = new Vector();

		for(int ii=0; ii<header.size(); ii++) 
		{
			temp = (String)header.elementAt(ii);

			if( temp.indexOf(keyword) == 0 ) 
			{
				//
				//  Found the right line, now we need to parse it.
				//
				index = temp.indexOf(":")+1;
				boolean done = false;
				while( !done ) {
					//
					//  Look for the commas.
					//
					if( temp.indexOf(",", index+1 ) > 0 ) {
						tempvector.add( new Float( temp.substring(index+1, temp.indexOf(",", index+1) ).trim() ));	
						index = temp.indexOf(",", index+1);
						found = true;
					}
					else {
						tempvector.add( new Float( temp.substring(temp.lastIndexOf(",")+1).trim() ) );	
						done = true;
					}
				}

				if( remove ) 
				{
					header.removeElementAt(ii);
					ii++;
				}
			}
		}

		return tempvector;
	}


//	protected double[] parseForDoubleVec( String keyword ) 
//	{
//		int toreturn = -1;
//		String temp;
//		int index;
//		boolean found = false;
//		Vector tempvector = new Vector();
//
//		for(int ii=0; ii<header.size(); ii++) {
//			temp = (String)header.elementAt(ii);
//
//			if( temp.indexOf(keyword) == 0 ) {
//				//
//				//  Found the right line, now we need to parse it.
//				//
//				index = temp.indexOf(":")+1;
//				boolean done = false;
//				while( !done ) {
//					//
//					//  Look for the commas.
//					//
//					if( temp.indexOf(",", index+1 ) > 0 ) {
//						tempvector.add( new Float( temp.substring(index+1, temp.indexOf(",", index+1) ).trim() ));	
//						index = temp.indexOf(",", index+1);
//						found = true;
//					}
//					else {
//						tempvector.add( new Float( temp.substring(temp.lastIndexOf(",")+1).trim() ) );	
//						done = true;
//					}
//				}
//			}
//		}
//
//		//
//		//  Copy vector info to a double array and return that.
//		//
//		double[] doubleReturn = new double[tempvector.size()];
//		for(int ii=0; ii<tempvector.size(); ii++) {
//			doubleReturn[ii] = ((Float)tempvector.elementAt(ii)).doubleValue();
//		}
//
//		return doubleReturn;
//	}
//
}
