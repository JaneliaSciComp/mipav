package gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats;

import java.io.*;
import java.lang.*;
import java.lang.Float;
import java.util.*;
import java.text.*;
import java.util.zip.*;
import java.nio.ByteOrder;
import javax.imageio.stream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.MRIFile;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format;

public class MIF3 extends MRIFile {

	public MIF3(String filename) {
		initialize();

		read(filename);
	}

	private void initialize()
	{
		endian = ByteOrder.LITTLE_ENDIAN;
		datatype = MRIFile.DATATYPE_USHORT;

		mif_id = new byte[ 20 ];
		buffer1 = new byte[ 4 ];
		scan_date = new byte[ 10 ];
		mif_file_date = new byte[ 10 ];
		file_id = new byte[ 13 ];
		scan_time = new byte[ 9 ];
		echo_count = new byte[ 1 ];
		buffer2 = new byte[ 2 ];
		patient_name = new byte[ 25 ];
		patient_number = new byte[ 25 ];
		mif_time = new byte[ 6 ];
		image_offsets = new int[ 32 ];
		mif_name = new byte[ 26 ];
		pixel_min = new int[ 32 ];
		pixel_max = new int[ 32 ];
		pixel_divisor = new int[ 32 ];
		patient_birth_date = new byte[ 12 ];
		tape_name = new byte[ 8 ];
		patient_initials = new byte[ 3 ];
		filler = new byte[ 3 ];
	}

	public void readHeader( String filename ) {
		ImageInputStream ldis = null;

		ldis = AppKit.openRead(filename);

		if( ldis == null ) {
			System.out.println("Could not open file");
			System.exit(0);
		}

		readHeader(ldis);
	}

	public void readHeader( ImageInputStream ldis )
	{
		try {
			 ldis.readFully(mif_id, 0, 20);
			 ldis.readFully(buffer1, 0, 4);
			 ldis.readFully(scan_date, 0, 10);
			 ldis.readFully(mif_file_date, 0, 10);
			 mif_version = ldis.readShort();
			 mif_subversion = ldis.readShort();
			 scanner_id = ldis.readShort();
			 ldis.readFully(file_id, 0, 13);
			 ldis.readFully(scan_time, 0, 8);
			 ldis.readFully(echo_count, 0, 1);
			 ldis.readFully(buffer2, 0, 2);
			 scan_id = ldis.readInt();
			 ldis.readFully(patient_name, 0, 25);
			 ldis.readFully(patient_number, 0, 25);
			 conversion_exe_time = ldis.readInt();
			 conversion_version = ldis.readShort();
			 te = ldis.readInt();
			 tr = ldis.readInt();
			 slice_gap = ldis.readInt();
			 ldis.readFully(mif_time, 0, 6);
			 for(int ii=0; ii<32; ii++) {
				 image_offsets[ii] = ldis.readShort();
			}
			 compression = ldis.readShort();
			 window = ldis.readShort();
			 bpp = ldis.readShort();
			 pixel_size_x = ldis.readInt();
			 pixel_size_y = ldis.readInt();
			 slice_thickness = ldis.readInt();
			 dim_x = ldis.readShort();
			 dim_y = ldis.readShort();
			 slice_count = ldis.readShort();
			 ldis.readFully(mif_name, 0, 26);
			 for(int ii=0; ii<32; ii++) {
				 pixel_min[ii] = ldis.readShort();
			 }
			 for(int ii=0; ii<32; ii++) {
				 pixel_max[ii] = ldis.readShort();
			 }
			 for(int ii=0; ii<32; ii++) {
				 pixel_divisor[ii] = ldis.readShort();
			 }
			 ldis.readFully(patient_birth_date, 0, 12);
			 trans_x = ldis.readInt();
			 trans_y = ldis.readInt();
			 trans_z = ldis.readInt();
			 rot_x = ldis.readInt();
			 rot_y = ldis.readInt();
			 rot_z = ldis.readInt();
			 ldis.readFully(tape_name, 0, 8);
			 ldis.readFully(patient_initials, 0, 3);
			 file_count = ldis.readInt();
			 ldis.readFully(filler, 0, 3);

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

	public double[] getTE()
	{
		double[] telist = new double[1];
		telist[0] = (double)te/1000.0;
		return telist;
	}

	public int getNEchoes()
	{
		return (new Integer( echo_count[0] )).intValue();
	}

	public void read( String filename ) {
		ImageInputStream ldis = null;

		ldis = AppKit.openRead(filename);
		
		if( ldis == null ) {
			System.out.println("Could not open file");
			System.exit(0);
		}

		readHeader(ldis);

		//
		//  Now read in the data.
		//
		data = new MCVolume(dim_y, dim_x, slice_count, getNEchoes());
		for(int slice=0; slice<slice_count; slice++) {
			for(int echo=0; echo<getNEchoes(); echo++) {
				data.read(ldis, slice, echo, datatype);
			}
		}

		try {
			ldis.close();
		}
		catch( IOException e ) {
			System.out.println("MIF3: Weird, could not close file.");
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
			System.out.println("MIF3: Could not open " + filename + " for writing.");
			System.exit(0);
		}

		//
		//  Write the header.
		//
		writeHeader(ldos);

		//
		//  Write the data.
		//
		for(int slice=0; slice<slice_count; slice++) {
			for(int echo=0; echo<getNEchoes(); echo++) {
				data.write( ldos, slice, echo, datatype );
			}
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
			ldos.write(mif_id, 0, 20);
			ldos.write(buffer1, 0, 4);
			ldos.write(scan_date, 0, 10);
			ldos.write(mif_file_date, 0, 10);
			ldos.writeShort(mif_version );
			ldos.writeShort(mif_subversion );
			ldos.writeShort(scanner_id );
			ldos.write(file_id, 0, 13);
			ldos.write(scan_time, 0, 8);
			ldos.write(echo_count, 0, 1);
			ldos.write(buffer2, 0, 2);
			ldos.writeInt(scan_id );
			ldos.write(patient_name, 0, 25);
			ldos.write(patient_number, 0, 25);
			ldos.writeInt(conversion_exe_time );
			ldos.writeShort(conversion_version );
			ldos.writeInt(te );
			ldos.writeInt(tr );
			ldos.writeInt(slice_gap );
			ldos.write(mif_time, 0, 6);
			for(int ii=0; ii<32; ii++) {
				ldos.writeShort(image_offsets[ii]);
			}
			ldos.writeShort(compression );
			ldos.writeShort(window );
			ldos.writeShort(bpp );
			ldos.writeInt(pixel_size_x );
			ldos.writeInt(pixel_size_y );
			ldos.writeInt(slice_thickness );
			ldos.writeShort(dim_x );
			ldos.writeShort(dim_y );
			ldos.writeShort(slice_count );
			ldos.write(mif_name, 0, 26);
			for(int ii=0; ii<32; ii++) {
				ldos.writeShort(pixel_min[ii]);
			}
			for(int ii=0; ii<32; ii++) {
				ldos.writeShort(pixel_max[ii]);
			}
			for(int ii=0; ii<32; ii++) {
				ldos.writeShort(pixel_divisor[ii]);
			}
			ldos.write(patient_birth_date, 0, 12);
			ldos.writeInt(trans_x );
			ldos.writeInt(trans_y );
			ldos.writeInt(trans_z );
			ldos.writeInt(rot_x );
			ldos.writeInt(rot_y );
			ldos.writeInt(rot_z );
			ldos.write(tape_name, 0, 8);
			ldos.write(patient_initials, 0, 3);
			ldos.writeInt(file_count);
			ldos.write(filler, 0, 3);
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
		Format.print(System.out, "%+20s: " + new String(mif_id) + "\n", "mif_id");
		Format.print(System.out, "%+20s: " + new String(buffer1) + "\n", "buffer1");
		Format.print(System.out, "%+20s: " + new String(scan_date) + "\n", "scan_date");
		Format.print(System.out, "%+20s: " + new String(mif_file_date) + "\n", "mif_file_date");
		Format.print(System.out, "%+20s: " + mif_version + "\n", "mif_version");
		Format.print(System.out, "%+20s: " + mif_subversion + "\n", "mif_subversion");
		Format.print(System.out, "%+20s: " + scanner_id + "\n", "scanner_id");
		Format.print(System.out, "%+20s: " + new String(file_id) + "\n", "file_id");
		Format.print(System.out, "%+20s: " + new String(scan_time) + "\n", "scan_time");
		Format.print(System.out, "%+20s: " + getNEchoes() + "\n", "echo_count");
		Format.print(System.out, "%+20s: " + new String(buffer2) + "\n", "buffer2");
		Format.print(System.out, "%+20s: " + scan_id + "\n", "scan_id");
		Format.print(System.out, "%+20s: " + new String(patient_name) + "\n", "patient_name");
		Format.print(System.out, "%+20s: " + new String(patient_number) + "\n", "patient_number");
		Format.print(System.out, "%+20s: " + conversion_exe_time + "\n", "conversion_exe_time");
		Format.print(System.out, "%+20s: " + conversion_version + "\n", "conversion_version");
		Format.print(System.out, "%+20s: " + te + "\n", "te");
		Format.print(System.out, "%+20s: " + tr + "\n", "tr");
		Format.print(System.out, "%+20s: " + slice_gap + "\n", "slice_gap");
		Format.print(System.out, "%+20s: " + new String(mif_time) + "\n", "mif_time");
		Format.print(System.out, "%+20s: ", "image_offsets");
		for(int ii=0; ii<32; ii++) {
			System.out.print(image_offsets[ii] + ", ");
		}
		System.out.println("");
		Format.print(System.out, "%+20s: " + compression + "\n", "compression");
		Format.print(System.out, "%+20s: " + window + "\n", "window");
		Format.print(System.out, "%+20s: " + bpp + "\n", "bpp");
		Format.print(System.out, "%+20s: " + pixel_size_x + "\n", "pixel_size_x");
		Format.print(System.out, "%+20s: " + pixel_size_y + "\n", "pixel_size_y");
		Format.print(System.out, "%+20s: " + slice_thickness + "\n", "slice_thickness");
		Format.print(System.out, "%+20s: " + dim_x + "\n", "dim_x");
		Format.print(System.out, "%+20s: " + dim_y + "\n", "dim_y");
		Format.print(System.out, "%+20s: " + slice_count + "\n", "slice_count");
		Format.print(System.out, "%+20s: " + new String(mif_name) + "\n", "mif_name");
		Format.print(System.out, "%+20s: ", "pixel_min");
		for(int ii=0; ii<32; ii++) {
			System.out.print(pixel_min[ii] + ", ");
		}
		System.out.println("");
		Format.print(System.out, "%+20s: ", "pixel_max");
		for(int ii=0; ii<32; ii++) {
			System.out.print(pixel_max[ii] + ", ");
		}
		System.out.println("");
		Format.print(System.out, "%+20s: ", "pixel_divisor");
		for(int ii=0; ii<32; ii++) {
			System.out.print(pixel_divisor[ii] + ", ");
		}
		System.out.println("");
		Format.print(System.out, "%+20s: " + new String(patient_birth_date) + "\n", "patient_birth_date");
		Format.print(System.out, "%+20s: " + trans_x + "\n", "trans_x");
		Format.print(System.out, "%+20s: " + trans_y + "\n", "trans_y");
		Format.print(System.out, "%+20s: " + trans_z + "\n", "trans_z");
		Format.print(System.out, "%+20s: " + rot_x + "\n", "rot_x");
		Format.print(System.out, "%+20s: " + rot_y + "\n", "rot_y");
		Format.print(System.out, "%+20s: " + rot_z + "\n", "rot_z");
		Format.print(System.out, "%+20s: " + new String(tape_name) + "\n", "tape_name");
		Format.print(System.out, "%+20s: " + new String(patient_initials) + "\n", "patient_initials");
		Format.print(System.out, "%+20s: " + file_count + "\n", "file_count");
		Format.print(System.out, "%+20s: " + new String(filler) + "\n", "filler");
	}

	//
	//  private variables
	//
    private byte[] mif_id = null;
    private byte[] buffer1 = null;
    private byte[] scan_date = null;
    private byte[] mif_file_date = null;
    private int mif_version;
    private int mif_subversion;
    private int scanner_id;
    private byte[] file_id = null;
    private byte[] scan_time = null;
    private byte[] echo_count = null;
    private byte[] buffer2 = null;
    private int scan_id;
    private byte[] patient_name = null;
    private byte[] patient_number = null;
    private int conversion_exe_time;
    private int conversion_version;
    private int te;
    private int tr;
    private int slice_gap;
    private byte[] mif_time = null;
    private int[] image_offsets = null;
    private int compression;
    private int window;
    private int bpp;
    private int pixel_size_x;
    private int pixel_size_y;
    private int slice_thickness;
    private int dim_x;
    private int dim_y;
    private int slice_count;
    private byte[] mif_name = null;
    private int[] pixel_min = null;
    private int[] pixel_max = null;
    private int[] pixel_divisor = null;
    private byte[] patient_birth_date = null;
    private int trans_x;
    private int trans_y;
    private int trans_z;
    private int rot_x;
    private int rot_y;
    private int rot_z;
    private byte[] tape_name = null;
    private byte[] patient_initials = null;
    private int file_count;
    private byte[] filler = null;
}
