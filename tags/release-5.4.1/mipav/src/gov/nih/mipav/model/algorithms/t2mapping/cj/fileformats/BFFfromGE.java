package gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats;

import java.io.*;
import java.lang.*;
import java.lang.Float;
import java.util.*;
import java.net.*;
import java.util.zip.*;
import java.nio.ByteOrder;
import javax.imageio.stream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;

/** BFFfromGE class.
 *
 *  This class is strictly a class that allows the conversion from 
 *  the GE file format to BFF.
 *
 */
public class BFFfromGE extends BFF {

	int nslices;
	int nchannels;
	int nimages;

	boolean convert_magnitude = false;
	boolean convert_phase = false;
	boolean convert_real = false;
	boolean convert_imag = false;


	/** Set the convert types
	 *
	 */
	public void setConvertType(String type)
	{
		if( type.toUpperCase().startsWith("M") )
		{
			convert_magnitude = true;
		}
		else if( type.toUpperCase().startsWith("P") )
		{
			convert_phase = true;
		}
		else if( type.toUpperCase().startsWith("R") )
		{
			convert_real = true;
		}
		else if( type.toUpperCase().startsWith("I") )
		{
			convert_imag = true;
		}
		else
		{
			System.out.print("ge2bff: Could not figure out what " + type + " is.");
			System.out.println("It should be one of magn, phase, real or imag.");
			System.exit(0);
		}

	}

	/**  Get the sorted list of GE files.
	 *  
	 */
	public Genesis fromGenesis( String directory ) 
	{
		/*
		 *  Get all of the files in the "directory" that start
		 *  with the letter 'I'.
		 */
		File ge_files = new File( directory );
		String[] ge_filenames = ge_files.list(new GENameFilter());
		Arrays.sort(ge_filenames);

		return fromGenesis(directory, ge_filenames);
	}

	/** Convert the ge files into a BFF file.
	 *
	 */
	public Genesis fromGenesis( String directory, String[] ge_filenames ) 
	{
		InputStream fis = null;
		ImageInputStream ldis = null;
		
		header = new Vector(10);
		/*
		 *  First read in the I.001 file from the
		 *  specified directory.
		 */
//		System.out.println("Reading in " +  directory + "/" + ge_filenames[0] );
		Genesis ge = new Genesis();
		ge.read( directory + ge_filenames[0] );

		/*
		 *  Set the internal variables.
		 */
		int nrows = ge.get_IH_img_height();
		int ncols = ge.get_IH_img_width();

		nslices = ge.get_MR_slquant();

		/*
		 *  Set a couple of other header values.
		 *     The time going in to Date must be in **milliseconds**.
		 */

		header.add("BFF");
		header.add("ptname: " + ge.get_EX_patname() );

		GregorianCalendar cal = null;
		cal = new GregorianCalendar();
		Date d = new Date((long)ge.get_EX_ex_datetime()*(long)1000);
		cal.setTime(d);
	
		String scandate = new String("scandate: ");
		scandate += (new Format("%04d")).form(cal.get(Calendar.YEAR));
		scandate += "/";
		scandate += (new Format("%02d")).form(cal.get(Calendar.MONTH)+1);
		scandate += "/";
		scandate += (new Format("%02d")).form(cal.get(Calendar.DATE));
		scandate += " ";
		scandate += (new Format("%02d")).form(cal.get(Calendar.HOUR_OF_DAY));
		scandate += ":";
		scandate += (new Format("%02d")).form(cal.get(Calendar.MINUTE));
		scandate += ":";
		scandate += (new Format("%02d")).form(cal.get(Calendar.SECOND));

		header.add(scandate);
		header.add("nex: " + ge.get_MR_nex());

		header.add("desc: " + ge.get_EX_ex_desc() );
		header.add("exno: " + ge.get_EX_ex_no() );
		header.add("serno: " + ge.get_SE_se_no() );
		header.add("nfreq: " + ge.get_MR_dim_X() );
		header.add("nphase: " + ge.get_MR_dim_Y() );
		header.add("fov: " + ge.get_MR_dfov() );
		header.add("pixheight: " + ge.get_MR_pixsize_Y() );
		header.add("pixwidth: " + ge.get_MR_pixsize_X() );
		header.add("pixthick: " + ge.get_MR_slthick() );
		header.add("trigger: " + ge.get_MR_tdel() );
		header.add("seq: " + ge.get_MR_psdname() );
	 
		if( ge.get_EX_patsex() == 1 ) {
			header.add( "gender: male" );
		}
		else if( ge.get_EX_patsex() == 2 ) {
			header.add( "gender: female" );
		}
		else {
			header.add( "gender: undefined" );
		}
	 
		header.add( "ptage: " + ge.get_EX_patage() );

		/*
		 *  Setup the data volume.
		 */
		data = new MCVolumeFile(nrows, ncols, 1, ge_filenames.length);

		/*
		 *  Now, for each file....
		 */
		tr = new Vector(ge_filenames.length);
		te = new Vector(ge_filenames.length);
		ti = new Vector(ge_filenames.length);
		sloc = new Vector(ge_filenames.length);

		String complex = "complex: ";

		int num_converted = 0;
		for( int ii=0; ii<ge_filenames.length; ii++) 
		{

			/*
			 *  Read the file in.
			 */
			if( verbose > 0 ) 
			{
				System.out.println("Reading in " +  directory + "/" + 
								   ge_filenames[ii] );
			}

			ge.readHeader( directory + ge_filenames[ii] );
	
			/*
			 *  Let's make sure it is one we are to convert.
			 */
			if( ( ge.get_MR_image_type() == 0 && convert_magnitude) ||
			    ( ge.get_MR_image_type() == 1 && convert_phase ) ||
			    ( ge.get_MR_image_type() == 2 && convert_real ) ||
			    ( ge.get_MR_image_type() == 3 && convert_imag ) )
			{

				tr.add(new Double((double)ge.get_MR_tr() / (double)1000.0));
				te.add(new Double((double)ge.get_MR_te() / (double)1000.0));
				ti.add(new Double((double)ge.get_MR_ti() / (double)1000.0));
				sloc.add("" + ge.get_MR_loc_ras() + "" + ge.get_MR_loc());

				switch( ge.get_MR_image_type() )
				{
					case 0:
						complex += "magn";
					break;
					case 1:
						complex += "phase";
					break;
					case 2:
						complex += "real";
					break;
					case 3:
						complex += "imag";
					break;
				}
				if( ii != ge_filenames.length-1 ) complex += ", ";


				/*
				 *  Open the file and get to the data.
				 */
				ldis = AppKit.openRead( directory + 
				          File.separator + ge_filenames[ii]);

				if( ldis == null )
				{
					System.out.println("ge2bff: Could not open the GE file " + 
								directory + File.separator + ge_filenames[ii]);
					System.exit(-1);
				}

				ldis.setByteOrder(ByteOrder.BIG_ENDIAN);

				try {
					/*
					 *  Poor man's seek.
					 */
					byte[] dummy = new byte[7904];
					ldis.read(dummy, 0, 7904);
					data.read( ldis, 0, num_converted );
				}
				catch( Exception e ) {
					System.out.println("BFF: Could not read in GE data.");
					System.exit(0);
				}

				num_converted++;
			}
		}

		String temp;
		temp = "tr: ";
		for(int ii=0; ii<num_converted; ii++) {
			temp += ((Double)tr.get(ii)).toString();
			if( ii != (num_converted-1) ) {
				temp += ", ";
			}
		}
		header.add(temp);

//		System.out.println("There are " + numUnique(te) + " unique TEs");
		temp = "te: ";
		for(int ii=0; ii<num_converted; ii++) {
			temp += ((Double)te.get(ii)).toString();
			if( ii != (num_converted-1) ) {
				temp += ", ";
			}
		}
		header.add(temp);

		temp = "ti: ";
		for(int ii=0; ii<num_converted; ii++) {
			temp += ((Double)ti.get(ii)).toString();
			if( ii != (num_converted-1) ) {
				temp += ", ";
			}
		}
		header.add(temp);

//		System.out.println("There are " + numUnique(sloc) + " unique slices");
		nslices = numUnique(sloc);
		nchannels = (int)Math.floor( (double)nimages /  (double)nslices );
		temp = "sloc: ";
		for(int ii=0; ii<num_converted; ii++) {
			temp += (String)sloc.get(ii);
			if( ii != (num_converted-1) ) {
				temp += ", ";
			}
		}
		header.add(temp);

		header.add(complex);

		nimages = num_converted;
		nchannels = (int)Math.floor( (double)nimages /  (double)nslices );

		datatype = MRIFile.DATATYPE_SHORT;
		header.add("type: " + MRIFile.dataTypeToString( datatype ) );
		header.add("endian: " + MRIFile.dataEndianToString( endian ) );

		addComment("ge2bff: Converted from " + (new File( directory ) ).getAbsolutePath());

		return ge;
	}

	public void writeFromGenesis(String filename)
	{
		ImageOutputStream ios = null;
		String line;
		
		ios = AppKit.openWrite(filename);
		if( ios == null ) {
			System.out.println("BFF: Could not open " + filename + " to write.");
			System.exit(0);
		}

		// Fininsh filling the header.
		header.insertElementAt("nrows: " + data.getNRows(), 2);
		header.insertElementAt("ncols: " + data.getNCols(), 3);
		header.insertElementAt("nslices: " + nslices, 4);
		header.insertElementAt("nchannels: " + nchannels, 5);
		header.insertElementAt("nimages: " + nimages, 5);

		// Write out the header.
		try {
			for(int ii=0; ii<header.size(); ii++) {
				ios.writeBytes((String)header.elementAt(ii) + "\n");
			}
			ios.writeBytes((String)"\014\n");
		}
		catch( IOException e ) {
			System.out.println("BFF: Could not write header");
			System.exit(0);
		}

		int nimages = parseForInt("nimages");

		ios.setByteOrder(endian);

		// Write the data.
		for(int channel=0; channel<nimages;channel++) {
			data.write( ios, 0, channel, datatype );
		}

		try {
			ios.close();
		}
		catch( IOException e ) {
			System.out.println("BFF: Could not close " + filename );
			System.exit(0);
		}

	}

	private int numUnique(final Vector vector)
	{
		Vector unique = new Vector();

		for(int ii=0; ii<vector.size(); ii++)
		{
			if( vector.get(ii) != null && !unique.contains( vector.get(ii) ) )
			{
				unique.add( vector.get(ii) );
			}
		}

		return unique.size();
	}

}

class GENameFilter implements FilenameFilter {
	public boolean accept(File dir, String name) {
		return name.startsWith("I");
	}
}
