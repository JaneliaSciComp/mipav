package gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats;

import java.io.*;
import java.lang.*;
import java.lang.Float;
import java.nio.ByteOrder;
import java.util.*;
import java.text.*;
import java.util.zip.*;
import javax.imageio.stream.*;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;

public abstract class MRIFile {

	static final public int FILETYPE_UNKNOWN = 0;
	static final public int FILETYPE_BFF = 1;
	static final public int FILETYPE_TBF = 2;
	static final public int FILETYPE_MIF3 = 3;
	static final public int FILETYPE_MIF4 = 4;
	static final public int FILETYPE_GENESIS = 5;
	static final public int FILETYPE_ANALYZE = 6;
	static final public int FILETYPE_GEPFILE = 7;
	static final public int FILETYPE_PHILIPS = 8;

	static final public int UPDATE_CHANNELS = 1<<0;
	static final public int UPDATE_SLICES   = 1<<1;
	static final public int UPDATE_ROWCOLS  = 1<<2;

	/*------------------------------------------------------------------
	 *
	 *   DATA TYPE STUFF
	 *
	 *------------------------------------------------------------------
	 */

	/** The allowable data types.  */
	static final public int DATATYPE_BYTE = 0;
	static final public int DATATYPE_SHORT = 1;
	static final public int DATATYPE_USHORT = 2;
	static final public int DATATYPE_INT = 3;
	static final public int DATATYPE_UINT = 4;
	static final public int DATATYPE_FLOAT = 5;
	static final public int DATATYPE_DOUBLE = 6;

	protected int fileType=0;

	final public int getFileType()
	{
		return fileType;
	}

	protected int verbose = 0;

	/** The data type holder. */
	
	protected int datatype;
	
	public void updateHeader(int flags) 
	{
		
	}

	final public void setDataType( int datatype )
	{
		this.datatype = datatype;
	}

	final public int getDataType( )
	{
		return datatype;
	}

	/**
	 *  Retrieve the constant int describing the string type.
	 */
	static public int dataTypeFromString( String type ) 
	{
		int datatype = 0;

		if( type.compareTo("byte") == 0 ) 
		{
			datatype = MRIFile.DATATYPE_BYTE;
		} 
		else if( type.compareTo("short") == 0 ) 
		{
			datatype = MRIFile.DATATYPE_SHORT;
		}
		else if( type.compareTo("ushort") == 0 ) 
		{
			datatype = MRIFile.DATATYPE_USHORT;
		}
		else if( type.compareTo("int") == 0 ) 
		{
			datatype = MRIFile.DATATYPE_INT;
		}
		else if( type.compareTo("uint") == 0 ) 
		{
			datatype = MRIFile.DATATYPE_UINT;
		}
		else if( type.compareTo("float") == 0 ) 
		{
			datatype = MRIFile.DATATYPE_FLOAT;
		}
		else if( type.compareTo("double") == 0 ) 
		{
			datatype = MRIFile.DATATYPE_DOUBLE;
		}
		else 
		{
			System.out.println("MRIFile:: Unknown data type " + type );
			System.exit(-1);
		}

		return datatype;
	}

	/**
	 *  Retrieve the string describing the int datatype.
	 */
	static public String dataTypeToString( int datatype ) 
	{
		String type = "";

	 	if( datatype == MRIFile.DATATYPE_BYTE ) 
		{
			type = "byte";
		} 
		else if( datatype == MRIFile.DATATYPE_USHORT ) 
		{
			type = "ushort";
		}
		else if( datatype == MRIFile.DATATYPE_SHORT ) 
		{
			type = "short";
		}
		else if( datatype == MRIFile.DATATYPE_UINT ) 
		{
			type = "uint";
		}
		else if( datatype == MRIFile.DATATYPE_INT ) 
		{
			type = "int";
		}
		else if( datatype == MRIFile.DATATYPE_FLOAT ) 
		{
			type = "float";
		}
		else if( datatype == MRIFile.DATATYPE_DOUBLE ) 
		{
			type = "double";
		}
		else 
		{
			System.out.println("MRIFile:: Unknown data type.");
			System.exit(-1);
		}

		return type;
	}

	/*------------------------------------------------------------------
	 *
	 *   ENDIAN STUFF
	 *
	 *------------------------------------------------------------------
	 */

	/** The endian holder. */
	protected ByteOrder endian;

	/** Return the string representation of the endian. */
	final static public String dataEndianToString( ByteOrder endian )
	{
		String str_endian = "little";

		if( endian == ByteOrder.BIG_ENDIAN ) 
		{
			str_endian = "big";
		}

		return str_endian;
	}

	/** Return the int representation of the String endian. */
	final static public ByteOrder dataEndianFromString( String str_endian )
	{
		ByteOrder temp_endian = ByteOrder.BIG_ENDIAN;

		if( str_endian.compareTo("big") == 0 )
		{
			temp_endian = ByteOrder.BIG_ENDIAN;
		}
		else if( str_endian.compareTo("little") == 0 ) 
		{
			temp_endian = ByteOrder.LITTLE_ENDIAN;
		}
		else 
		{
			System.out.println("dataEndianFromString: Unknown endian " + str_endian);
			System.exit(-1);
		}

		return temp_endian;
	}

	/** Set the endian. */
	final public void setEndian( ByteOrder endian )
	{
		this.endian = endian;
	}

	/** Get the endian. */
	final public ByteOrder getEndian( )
	{
		return endian;
	}

	/*------------------------------------------------------------------
	 *
	 *   DATA STUFF
	 *
	 *------------------------------------------------------------------
	 */

	/** The data. */
	
	protected MCVolume data = null;

	abstract public double[] getTE();

	abstract public void read( String filename ); 
	abstract public void readHeader( String filename ); 
	abstract public void write( String filename ); 

	abstract public void readHeader( ImageInputStream iis );

    final public int nRows() 
    {
		return data.getNRows();
	}

    final public int nCols() {
		return data.getNCols();
	}

	public void show() {}

	/** 
	 *  
	 */ 
	
	public MCVolume getMCVolume() 
	{
		return data;
	}

	/* 
	 *  Remove the specified channel.
	 */
	public void removeVolume(final int index)
	{
		data.removeVolume(index);

		int flags = 0;
		flags |= UPDATE_CHANNELS;
		updateHeader(flags);
	}

	/** 
	 *
	 */
	public  void setMCVolume(MCVolume data_in) 
	{
		int flags = 0;

		if( data_in == null ) 
		{
			System.out.println("MRIFile::setMCVolume: Passed in a null pointer");
			System.exit(-1);
		}

		/* If the internal data is not defined yet, just clone the passed
		 * data.
		 */
		if( data == null ) 
		{
			flags |= UPDATE_ROWCOLS;
			flags |= UPDATE_ROWCOLS;
			flags |= UPDATE_SLICES;
			flags |= UPDATE_CHANNELS;

			data = (MCVolume)data_in.clone();
		}
		else {

			if (data.getNRows() != data_in.getNRows())
				flags |= UPDATE_ROWCOLS;
			if (data.getNCols() != data_in.getNCols())
				flags |= UPDATE_ROWCOLS;
			if (data.getNSlices() != data_in.getNSlices())
				flags |= UPDATE_SLICES;
			if (data.getNChannels() != data_in.getNChannels())
				flags |= UPDATE_CHANNELS;

			data = (MCVolume)data_in.clone();
		}

		updateHeader(flags);
	}

	/** 
	 *
	 */
	public  Volume getVolume(int num) 
	{
		return data.getVolume(num);
	}

	/** 
	 *
	 *  @deprecated - Should use setMCVolume instead.
	 */
	public void setVolume(MCVolume data_in) 
	{
		int flags = 0;

		if (data.getNRows() != data_in.getNRows())
			flags |= UPDATE_ROWCOLS;
		if (data.getNCols() != data_in.getNCols())
			flags |= UPDATE_ROWCOLS;
		if (data.getNSlices() != data_in.getNSlices())
			flags |= UPDATE_SLICES;
		if (data.getNChannels() != data_in.getNChannels())
			flags |= UPDATE_CHANNELS;

		data = (MCVolume)data_in.clone();
		updateHeader(flags);
	}

		
	public static MRIFile readMRIFile(String filename)
	{
		MRIFile m;
		int typeOfFile = fileType( filename ) ;

		if( typeOfFile == FILETYPE_BFF )
			{ m = new BFF(filename); m.fileType = typeOfFile; return m; }

		if( typeOfFile == FILETYPE_TBF )
			{ m = new TBF(filename); m.fileType = typeOfFile; return m; }

		if( typeOfFile == FILETYPE_MIF3 )
			{ m = new MIF3(filename); m.fileType = typeOfFile; return m; }

		if( typeOfFile == FILETYPE_MIF4 )
			{ m = new MIF4(filename); m.fileType = typeOfFile; return m; }

		if( typeOfFile == FILETYPE_GENESIS )
			{ m = new Genesis(filename); m.fileType = typeOfFile; return m; }

		if( typeOfFile == FILETYPE_ANALYZE )
			{ m = new Analyze(filename); m.fileType = typeOfFile; return m; }

		if( typeOfFile == FILETYPE_PHILIPS )
			{ m = new Philips(filename); m.fileType = typeOfFile; return m; }

		if( typeOfFile == FILETYPE_GEPFILE )
			{ m = new GEPFile(filename); m.fileType = typeOfFile; return m; }

		System.out.println("MRIFile::readMRIFile: Could not determine type of file.");
		return null;
	}

 	/**
	 *  Used to determine the type of file that 
	 *  we are wanting to read in.
	 *
	 */
	static public int fileType(String filename) 
	{
		int filetype = FILETYPE_UNKNOWN;
		int version = 0;

        if( filename.indexOf(".bff") != -1 ) {
            filetype = FILETYPE_BFF;
        }
        else if( filename.indexOf(".tbf") != -1 ) {           
            filetype = FILETYPE_TBF;
        }                                                     
        else if( filename.startsWith("I.") || 
		         filename.indexOf("/I.") != -1 ) {
            filetype = FILETYPE_GENESIS;
        }                                                     
        else if( filename.startsWith("P") || 
		         filename.indexOf("/P") != -1 ) {
            filetype = FILETYPE_GEPFILE;
        }                                                     
        else if( filename.startsWith("H.") || 
		         filename.indexOf("/H.") != -1 ) {
            filetype = FILETYPE_GENESIS;
        }                                                     
		else if( ((new File(filename + ".hdr")).exists() && (new File(filename + ".img")).exists()) ||
		         ( filename.endsWith(".hdr") || filename.endsWith(".img") ) )
		 {
			filetype = FILETYPE_ANALYZE;
		}
//		else if( (new File(filename + ".PAR")).exists() && (new File(filename + ".REC")).exists()  ){
//			filetype = FILETYPE_PHILIPS;
//		}
		else if( filename.endsWith("PAR") || filename.endsWith("REC") || filename.endsWith("rec") )
		{
			filetype = FILETYPE_PHILIPS;
		}
        else {                                                
			//  Hopefully a MIF File, so we need to determine the version
			InputStream fis = null;
			ImageInputStream d = null;

			d = AppKit.openRead(filename);

			if (d == null) {
				System.out.println("MRIFile: Could not open file " + filename);
				return FILETYPE_UNKNOWN;
			}

			try {
				d.skipBytes(44);

				byte[] tmp = new byte[2];
				d.read(tmp, 0, 2);
				version = (int)((tmp[1]&0xff)<<8) | (tmp[0]&0xff);
			}

			catch( IOException e ) {
				System.out.println("MRIFile: Could not open file " + filename);
				return FILETYPE_UNKNOWN;
			}
	
			if( version == 3 ) {
				filetype = FILETYPE_MIF3;
			}
			else if( version == 4 ) {
				filetype = FILETYPE_MIF4;
			}
        }                                                     

		return filetype;
	}
}
