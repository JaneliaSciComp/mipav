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

public class GEPFile extends MRIFile {

	public GEPFile(String filename) {
		initialize();

		read(filename);
	}

	private void initialize()
	{
		endian = ByteOrder.LITTLE_ENDIAN;
		datatype = MRIFile.DATATYPE_USHORT;

		rdb_hdr_run_char = new byte[6+1]; //[6+1];
		rdb_hdr_scan_date = new byte[10+1]; //[10+1];
		rdb_hdr_scan_time = new byte[8+1]; //[8+1];
		rdb_hdr_logo = new byte[10+1]; //[10+1];
		rdb_hdr_ps_powerspec = new byte[256+1]; //[256+1];
		rdb_hdr_rec_noise_mean = new float[16]; //[16];
		rdb_hdr_rec_noise_std = new float[16]; //[16];
		rdb_hdr_excess = new short[516]; // [516];
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
			rdb_hdr_rdbm_rev = ldis.readFloat();
			rdb_hdr_run_int = ldis.readInt();
			rdb_hdr_scan_seq = ldis.readShort();
			ldis.readFully(rdb_hdr_run_char, 0, 6); //[6+1];
			ldis.readFully(rdb_hdr_scan_date, 0, 10); //[10+1];
			ldis.readFully(rdb_hdr_scan_time, 0, 8); //[8+1];
			ldis.readFully(rdb_hdr_logo, 0, 10); //[10+1];
			rdb_hdr_file_contents = ldis.readShort();
			rdb_hdr_lock_mode = ldis.readShort();
			rdb_hdr_dacq_ctrl = ldis.readShort();
			rdb_hdr_recon_ctrl = ldis.readShort();
			rdb_hdr_exec_ctrl = ldis.readShort();
			rdb_hdr_scan_type = ldis.readShort();
			rdb_hdr_data_collect_type = ldis.readShort();
			rdb_hdr_data_format = ldis.readShort();
			rdb_hdr_recon = ldis.readShort();
			rdb_hdr_datacq = ldis.readShort();
			rdb_hdr_npasses = ldis.readShort();
			rdb_hdr_npomp = ldis.readShort();
			rdb_hdr_nslices = ldis.readShort();
			rdb_hdr_nechoes = ldis.readShort();
			rdb_hdr_navs = ldis.readShort();
			rdb_hdr_nframes = ldis.readShort();
			rdb_hdr_baseline_views = ldis.readShort();
			rdb_hdr_hnover = ldis.readShort();
			rdb_hdr_frame_size = ldis.readShort();
			rdb_hdr_point_size = ldis.readShort();
			rdb_hdr_vquant = ldis.readShort();
			rdb_hdr_cheart = ldis.readShort();
			rdb_hdr_ctr = ldis.readFloat();
			rdb_hdr_ctrr = ldis.readFloat();
			rdb_hdr_initpass = ldis.readShort();
			rdb_hdr_incrpass = ldis.readShort();
			rdb_hdr_method_ctrl = ldis.readShort();
			rdb_hdr_da_xres = ldis.readShort();
			rdb_hdr_da_yres = ldis.readShort();
			rdb_hdr_rc_xres = ldis.readShort();
			rdb_hdr_rc_yres = ldis.readShort();
			rdb_hdr_im_size = ldis.readShort();
			rdb_hdr_rc_zres = ldis.readInt();
			rdb_hdr_raw_pass_size = ldis.readInt();
			rdb_hdr_sspsave = ldis.readInt();
			rdb_hdr_udasave = ldis.readInt();
			rdb_hdr_fermi_radius = ldis.readFloat();
			rdb_hdr_fermi_width = ldis.readFloat();
			rdb_hdr_fermi_ecc = ldis.readFloat();
			rdb_hdr_clip_min = ldis.readFloat();
			rdb_hdr_clip_max = ldis.readFloat();
			rdb_hdr_default_offset = ldis.readFloat();
			rdb_hdr_xoff = ldis.readFloat();
			rdb_hdr_yoff = ldis.readFloat();
			rdb_hdr_nwin = ldis.readFloat();
			rdb_hdr_ntran = ldis.readFloat();
			rdb_hdr_scalei = ldis.readFloat();
			rdb_hdr_scaleq = ldis.readFloat();
			rdb_hdr_rotation = ldis.readShort();
			rdb_hdr_transpose = ldis.readShort();
			rdb_hdr_kissoff_views = ldis.readShort();
			rdb_hdr_slblank = ldis.readShort();
			rdb_hdr_gradcoil = ldis.readShort();
			rdb_hdr_ddaover = ldis.readShort();
			rdb_hdr_sarr = ldis.readShort();
			rdb_hdr_fd_tr = ldis.readShort();
			rdb_hdr_fd_te = ldis.readShort();
			rdb_hdr_fd_ctrl = ldis.readShort();
			rdb_hdr_algor_num = ldis.readShort();
			rdb_hdr_fd_df_dec = ldis.readShort();

			ldis.skipBytes(16);

			rdb_hdr_user0 = ldis.readFloat();
			rdb_hdr_user1 = ldis.readFloat();
			rdb_hdr_user2 = ldis.readFloat();
			rdb_hdr_user3 = ldis.readFloat();
			rdb_hdr_user4 = ldis.readFloat();
			rdb_hdr_user5 = ldis.readFloat();
			rdb_hdr_user6 = ldis.readFloat();
			rdb_hdr_user7 = ldis.readFloat();
			rdb_hdr_user8 = ldis.readFloat();
			rdb_hdr_user9 = ldis.readFloat();
			rdb_hdr_user10 = ldis.readFloat();
			rdb_hdr_user11 = ldis.readFloat();
			rdb_hdr_user12 = ldis.readFloat();
			rdb_hdr_user13 = ldis.readFloat();
			rdb_hdr_user14 = ldis.readFloat();
			rdb_hdr_user15 = ldis.readFloat();
			rdb_hdr_user16 = ldis.readFloat();
			rdb_hdr_user17 = ldis.readFloat();
			rdb_hdr_user18 = ldis.readFloat();
			rdb_hdr_user19 = ldis.readFloat();
			rdb_hdr_v_type = ldis.readInt();
			rdb_hdr_v_coefxa = ldis.readFloat();
			rdb_hdr_v_coefxb = ldis.readFloat();
			rdb_hdr_v_coefxc = ldis.readFloat();
			rdb_hdr_v_coefxd = ldis.readFloat();
			rdb_hdr_v_coefya = ldis.readFloat();
			rdb_hdr_v_coefyb = ldis.readFloat();
			rdb_hdr_v_coefyc = ldis.readFloat();
			rdb_hdr_v_coefyd = ldis.readFloat();
			rdb_hdr_v_coefza = ldis.readFloat();
			rdb_hdr_v_coefzb = ldis.readFloat();
			rdb_hdr_v_coefzc = ldis.readFloat();
			rdb_hdr_v_coefzd = ldis.readFloat();
			rdb_hdr_vm_coef1 = ldis.readFloat();
			rdb_hdr_vm_coef2 = ldis.readFloat();
			rdb_hdr_vm_coef3 = ldis.readFloat();
			rdb_hdr_vm_coef4 = ldis.readFloat();
			rdb_hdr_v_venc = ldis.readFloat();

			ldis.skipBytes(40);

			rdb_hdr_ps_command = ldis.readInt();
			rdb_hdr_ps_mps_r1 = ldis.readInt();
			rdb_hdr_ps_mps_r2 = ldis.readInt();
			rdb_hdr_ps_mps_tg = ldis.readInt();
			rdb_hdr_ps_mps_freq = ldis.readInt();
			rdb_hdr_ps_aps_r1 = ldis.readInt();
			rdb_hdr_ps_aps_r2 = ldis.readInt();
			rdb_hdr_ps_aps_tg = ldis.readInt();
			rdb_hdr_ps_aps_freq = ldis.readInt();
			rdb_hdr_ps_scalei = ldis.readFloat();
			rdb_hdr_ps_scaleq = ldis.readFloat();
			rdb_hdr_ps_snr_warning = ldis.readInt();
			rdb_hdr_ps_aps_or_mps = ldis.readInt();
			rdb_hdr_ps_mps_bitmap = ldis.readInt();
			ldis.readFully(rdb_hdr_ps_powerspec, 0, 256); //[256+1] = ldis.readShort();
			rdb_hdr_ps_filler1 = ldis.readInt();
			rdb_hdr_ps_filler2 = ldis.readInt();
			for(int ii=0;ii<16; ii++) rdb_hdr_rec_noise_mean[ii]=ldis.readFloat();
			for(int ii=0;ii<16; ii++) rdb_hdr_rec_noise_std[ii]=ldis.readFloat();

			ldis.skipBytes(2);

			rdb_hdr_im_size_y = ldis.readShort();
			rdb_hdr_data_collect_type1 = ldis.readInt();
			rdb_hdr_freq_scale = ldis.readFloat();
			rdb_hdr_phase_scale = ldis.readFloat();
			rdb_hdr_ovl = ldis.readShort();
			rdb_hdr_pclin = ldis.readShort();
			rdb_hdr_pclinnpts = ldis.readShort();
			rdb_hdr_pclinorder = ldis.readShort();
			rdb_hdr_pclinavg = ldis.readShort();
			rdb_hdr_pccon = ldis.readShort();
			rdb_hdr_pcconnpts = ldis.readShort();
			rdb_hdr_pcconorder = ldis.readShort();
			rdb_hdr_pcextcorr = ldis.readShort();
			rdb_hdr_pcgraph = ldis.readShort();
			rdb_hdr_pcileave = ldis.readShort();
			rdb_hdr_hdbestky = ldis.readShort();
			rdb_hdr_pcctrl = ldis.readShort();
			rdb_hdr_pcthrespts = ldis.readShort();
			rdb_hdr_pcdiscbeg = ldis.readShort();
			rdb_hdr_pcdiscmid = ldis.readShort();
			rdb_hdr_pcdiscend = ldis.readShort();
			rdb_hdr_pcthrespct = ldis.readShort();
			rdb_hdr_pcspacial = ldis.readShort();
			rdb_hdr_pctemporal = ldis.readShort();
			rdb_hdr_pcspare = ldis.readShort();
			rdb_hdr_ileaves = ldis.readShort();
			rdb_hdr_kydir = ldis.readShort();
			rdb_hdr_alt = ldis.readShort();
			rdb_hdr_reps = ldis.readShort();
			rdb_hdr_ref = ldis.readShort();
			rdb_hdr_pcconnorm = ldis.readFloat();
			rdb_hdr_pcconfitwt = ldis.readFloat();
			rdb_hdr_pclinnorm = ldis.readFloat();
			rdb_hdr_pclinfitwt = ldis.readFloat();
			rdb_hdr_pcbestky = ldis.readFloat();
			rdb_hdr_vrgf = ldis.readInt();
			rdb_hdr_vrgfxres = ldis.readInt();
			rdb_hdr_bp_corr = ldis.readInt();
			rdb_hdr_recv_freq_s = ldis.readFloat();
			rdb_hdr_recv_freq_e = ldis.readFloat();
			rdb_hdr_hniter = ldis.readInt();
			rdb_hdr_fast_rec = ldis.readInt();
			rdb_hdr_refframes = ldis.readInt();
			rdb_hdr_refframep = ldis.readInt();
			rdb_hdr_scnframe = ldis.readInt();
			rdb_hdr_pasframe = ldis.readInt();
			rdb_hdr_pcfitorig = ldis.readShort();
			rdb_hdr_pcshotfirst = ldis.readShort();
			rdb_hdr_pcshotlast = ldis.readShort();
			rdb_hdr_pcmultegrp = ldis.readShort();
			rdb_hdr_pclinfix = ldis.readShort();
			rdb_hdr_pclinslope = ldis.readFloat();
			rdb_hdr_pcconfix = ldis.readShort();
			rdb_hdr_pcconslope = ldis.readFloat();
			rdb_hdr_pccoil = ldis.readShort();
			rdb_hdr_dp_type = ldis.readShort();
			rdb_hdr_ut_ctrl = ldis.readInt();
			for(int ii=0;ii<516; ii++) rdb_hdr_excess[ii]=ldis.readShort();
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
//		double[] telist = new double[1];
//		telist[0] = (double)te/1000.0;
//		return telist;

		return null;
	}

	public int getNEchoes()
	{
		//return (new Integer( echo_count[0] )).intValue();
		return 0;
	}

	public void read( String filename ) {
		ImageInputStream ldis = null;

		ldis = AppKit.openRead(filename);
		ldis.setByteOrder(ByteOrder.BIG_ENDIAN);
		
		if( ldis == null ) {
			System.out.println("Could not open file");
			System.exit(0);
		}

		readHeader(ldis);

		//
		//  Now read in the data.
		//
//		data = new MCVolume(dim_y, dim_x, slice_count, getNEchoes());
//		for(int slice=0; slice<slice_count; slice++) {
//			for(int echo=0; echo<getNEchoes(); echo++) {
//				data.read(ldis, slice, echo, datatype);
//			}
//		}
//
//		try {
//			ldis.close();
//		}
//		catch( IOException e ) {
//			System.out.println("GEPFile: Weird, could not close file.");
//			System.exit(-1);
//		}
	}

	public void write(String filename) {
//		LEDataOutputStream ldos = null;
//
//		//
//		//  Open the file for output.
//		//
//		ldos = AppKit.openOutput(filename);
//		if( ldos == null ) {
//			System.out.println("GEPFile: Could not open " + filename + " for writing.");
//			System.exit(0);
//		}
//
//		//
//		//  Write the header.
//		//
//		writeHeader(ldos);
//
//		//
//		//  Write the data.
//		//
//		for(int slice=0; slice<slice_count; slice++) {
//			for(int echo=0; echo<getNEchoes(); echo++) {
//				data.write( ldos, slice, echo, datatype );
//			}
//		}
//
//		//
//		//  Close the file.
//		//
//		try {
//			ldos.close();
//		}
//		catch( IOException e ) {
//			System.out.println("Could not close file.");
//			System.exit(-1);
//		}
	}

	public void writeHeader( ImageOutputStream ldos )
	{
//		try {
//			ldos.write(mif_id, 0, 20);
//			ldos.write(buffer1, 0, 4);
//			ldos.write(scan_date, 0, 10);
//			ldos.write(mif_file_date, 0, 10);
//			ldos.writeShort(mif_version );
//			ldos.writeShort(mif_subversion );
//			ldos.writeShort(scanner_id );
//			ldos.write(file_id, 0, 13);
//			ldos.write(scan_time, 0, 8);
//			ldos.write(echo_count, 0, 1);
//			ldos.write(buffer2, 0, 2);
//			ldos.writeInt(scan_id );
//			ldos.write(patient_name, 0, 25);
//			ldos.write(patient_number, 0, 25);
//			ldos.writeInt(conversion_exe_time );
//			ldos.writeShort(conversion_version );
//			ldos.writeInt(te );
//			ldos.writeInt(tr );
//			ldos.writeInt(slice_gap );
//			ldos.write(mif_time, 0, 6);
//			for(int ii=0; ii<32; ii++) {
//				ldos.writeShort(image_offsets[ii]);
//			}
//			ldos.writeShort(compression );
//			ldos.writeShort(window );
//			ldos.writeShort(bpp );
//			ldos.writeInt(pixel_size_x );
//			ldos.writeInt(pixel_size_y );
//			ldos.writeInt(slice_thickness );
//			ldos.writeShort(dim_x );
//			ldos.writeShort(dim_y );
//			ldos.writeShort(slice_count );
//			ldos.write(mif_name, 0, 26);
//			for(int ii=0; ii<32; ii++) {
//				ldos.writeShort(pixel_min[ii]);
//			}
//			for(int ii=0; ii<32; ii++) {
//				ldos.writeShort(pixel_max[ii]);
//			}
//			for(int ii=0; ii<32; ii++) {
//				ldos.writeShort(pixel_divisor[ii]);
//			}
//			ldos.write(patient_birth_date, 0, 12);
//			ldos.writeInt(trans_x );
//			ldos.writeInt(trans_y );
//			ldos.writeInt(trans_z );
//			ldos.writeInt(rot_x );
//			ldos.writeInt(rot_y );
//			ldos.writeInt(rot_z );
//			ldos.write(tape_name, 0, 8);
//			ldos.write(patient_initials, 0, 3);
//			ldos.writeInt(file_count);
//			ldos.write(filler, 0, 3);
//		}
//		catch( EOFException e ) {
//			System.out.println(e.getMessage());
//			System.out.println("End of file for some reason");
//			System.exit(0);
//		}
//		catch( IOException e ) {
//			System.out.println(e.getMessage());
//			System.out.println("Could not read header");
//			System.exit(0);
//		}
	}

	public void show() {
		System.out.println("GEPfile");

			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_rdbm_rev + "\n", "rdb_hdr_rdbm_rev");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_run_int + "\n", "rdb_hdr_run_int");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_scan_seq + "\n", "rdb_hdr_scan_seq");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_run_char+ "\n", "rdb_hdr_run_char");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " + (new String(rdb_hdr_scan_date))+ "\n", "rdb_hdr_scan_date");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +(new String(rdb_hdr_scan_time))+ "\n", "rdb_hdr_scan_time");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +(new String(rdb_hdr_logo))+ "\n", "rdb_hdr_logo");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_file_contents + "\n", "rdb_hdr_file_contents");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_lock_mode + "\n", "rdb_hdr_lock_mode");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_dacq_ctrl + "\n", "rdb_hdr_dacq_ctrl");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_recon_ctrl + "\n", "rdb_hdr_recon_ctrl");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_exec_ctrl + "\n", "rdb_hdr_exec_ctrl");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_scan_type + "\n", "rdb_hdr_scan_type");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_data_collect_type + "\n", "rdb_hdr_data_collect_type");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_data_format + "\n", "rdb_hdr_data_gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_recon + "\n", "rdb_hdr_recon");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_datacq + "\n", "rdb_hdr_datacq");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_npasses + "\n", "rdb_hdr_npasses");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_npomp + "\n", "rdb_hdr_npomp");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_nslices + "\n", "rdb_hdr_nslices");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_nechoes + "\n", "rdb_hdr_nechoes");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_navs + "\n", "rdb_hdr_navs");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_nframes + "\n", "rdb_hdr_nframes");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_baseline_views + "\n", "rdb_hdr_baseline_views");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_hnover + "\n", "rdb_hdr_hnover");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_frame_size + "\n", "rdb_hdr_frame_size");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_point_size + "\n", "rdb_hdr_point_size");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_vquant + "\n", "rdb_hdr_vquant");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_cheart + "\n", "rdb_hdr_cheart");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ctr + "\n", "rdb_hdr_ctr");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ctrr + "\n", "rdb_hdr_ctrr");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_initpass + "\n", "rdb_hdr_initpass");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_incrpass + "\n", "rdb_hdr_incrpass");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_method_ctrl + "\n", "rdb_hdr_method_ctrl");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_da_xres + "\n", "rdb_hdr_da_xres");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_da_yres + "\n", "rdb_hdr_da_yres");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_rc_xres + "\n", "rdb_hdr_rc_xres");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_rc_yres + "\n", "rdb_hdr_rc_yres");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_im_size + "\n", "rdb_hdr_im_size");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_rc_zres + "\n", "rdb_hdr_rc_zres");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_raw_pass_size + "\n", "rdb_hdr_raw_pass_size");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_sspsave + "\n", "rdb_hdr_sspsave");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_udasave + "\n", "rdb_hdr_udasave");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_fermi_radius + "\n", "rdb_hdr_fermi_radius");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_fermi_width + "\n", "rdb_hdr_fermi_width");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_fermi_ecc + "\n", "rdb_hdr_fermi_ecc");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_clip_min + "\n", "rdb_hdr_clip_min");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_clip_max + "\n", "rdb_hdr_clip_max");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_default_offset + "\n", "rdb_hdr_default_offset");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_xoff + "\n", "rdb_hdr_xoff");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_yoff + "\n", "rdb_hdr_yoff");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_nwin + "\n", "rdb_hdr_nwin");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ntran + "\n", "rdb_hdr_ntran");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_scalei + "\n", "rdb_hdr_scalei");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_scaleq + "\n", "rdb_hdr_scaleq");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_rotation + "\n", "rdb_hdr_rotation");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_transpose + "\n", "rdb_hdr_transpose");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_kissoff_views + "\n", "rdb_hdr_kissoff_views");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_slblank + "\n", "rdb_hdr_slblank");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_gradcoil + "\n", "rdb_hdr_gradcoil");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ddaover + "\n", "rdb_hdr_ddaover");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_sarr + "\n", "rdb_hdr_sarr");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_fd_tr + "\n", "rdb_hdr_fd_tr");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_fd_te + "\n", "rdb_hdr_fd_te");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_fd_ctrl + "\n", "rdb_hdr_fd_ctrl");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_algor_num + "\n", "rdb_hdr_algor_num");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_fd_df_dec + "\n", "rdb_hdr_fd_df_dec");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user0 + "\n", "rdb_hdr_user0");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user1 + "\n", "rdb_hdr_user1");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user2 + "\n", "rdb_hdr_user2");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user3 + "\n", "rdb_hdr_user3");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user4 + "\n", "rdb_hdr_user4");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user5 + "\n", "rdb_hdr_user5");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user6 + "\n", "rdb_hdr_user6");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user7 + "\n", "rdb_hdr_user7");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user8 + "\n", "rdb_hdr_user8");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user9 + "\n", "rdb_hdr_user9");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user10 + "\n", "rdb_hdr_user10");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user11 + "\n", "rdb_hdr_user11");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user12 + "\n", "rdb_hdr_user12");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user13 + "\n", "rdb_hdr_user13");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user14 + "\n", "rdb_hdr_user14");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user15 + "\n", "rdb_hdr_user15");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user16 + "\n", "rdb_hdr_user16");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user17 + "\n", "rdb_hdr_user17");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user18 + "\n", "rdb_hdr_user18");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_user19 + "\n", "rdb_hdr_user19");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_type + "\n", "rdb_hdr_v_type");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_coefxa + "\n", "rdb_hdr_v_coefxa");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_coefxb + "\n", "rdb_hdr_v_coefxb");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_coefxc + "\n", "rdb_hdr_v_coefxc");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_coefxd + "\n", "rdb_hdr_v_coefxd");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_coefya + "\n", "rdb_hdr_v_coefya");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_coefyb + "\n", "rdb_hdr_v_coefyb");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_coefyc + "\n", "rdb_hdr_v_coefyc");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_coefyd + "\n", "rdb_hdr_v_coefyd");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_coefza + "\n", "rdb_hdr_v_coefza");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_coefzb + "\n", "rdb_hdr_v_coefzb");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_coefzc + "\n", "rdb_hdr_v_coefzc");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_coefzd + "\n", "rdb_hdr_v_coefzd");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_vm_coef1 + "\n", "rdb_hdr_vm_coef1");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_vm_coef2 + "\n", "rdb_hdr_vm_coef2");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_vm_coef3 + "\n", "rdb_hdr_vm_coef3");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_vm_coef4 + "\n", "rdb_hdr_vm_coef4");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_v_venc + "\n", "rdb_hdr_v_venc");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_command + "\n", "rdb_hdr_ps_command");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_mps_r1 + "\n", "rdb_hdr_ps_mps_r1");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_mps_r2 + "\n", "rdb_hdr_ps_mps_r2");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_mps_tg + "\n", "rdb_hdr_ps_mps_tg");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_mps_freq + "\n", "rdb_hdr_ps_mps_freq");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_aps_r1 + "\n", "rdb_hdr_ps_aps_r1");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_aps_r2 + "\n", "rdb_hdr_ps_aps_r2");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_aps_tg + "\n", "rdb_hdr_ps_aps_tg");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_aps_freq + "\n", "rdb_hdr_ps_aps_freq");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_scalei + "\n", "rdb_hdr_ps_scalei");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_scaleq + "\n", "rdb_hdr_ps_scaleq");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_snr_warning + "\n", "rdb_hdr_ps_snr_warning");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_aps_or_mps + "\n", "rdb_hdr_ps_aps_or_mps");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_mps_bitmap + "\n", "rdb_hdr_ps_mps_bitmap");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +(new String(rdb_hdr_ps_powerspec))+ "\n", "rdb_hdr_ps_powerspec");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_filler1 + "\n", "rdb_hdr_ps_filler1");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ps_filler2 + "\n", "rdb_hdr_ps_filler2");
			System.out.println("Noise mean:");
			for(int ii=0;ii<15; ii++) gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "\t%+20s: " +rdb_hdr_rec_noise_mean[ii]+ "\n", "rdb_hdr_rec_noise_mean["+ii+"]");
			System.out.println("Noise std:");
			for(int ii=0;ii<15; ii++) gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "\t%+20s: " +rdb_hdr_rec_noise_std[ii]+ "\n", "rdb_hdr_rec_noise_std["+ii+"]");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_im_size_y + "\n", "rdb_hdr_im_size_y");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_data_collect_type1 + "\n", "rdb_hdr_data_collect_type1");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_freq_scale + "\n", "rdb_hdr_freq_scale");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_phase_scale + "\n", "rdb_hdr_phase_scale");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ovl + "\n", "rdb_hdr_ovl");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pclin + "\n", "rdb_hdr_pclin");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pclinnpts + "\n", "rdb_hdr_pclinnpts");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pclinorder + "\n", "rdb_hdr_pclinorder");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pclinavg + "\n", "rdb_hdr_pclinavg");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pccon + "\n", "rdb_hdr_pccon");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcconnpts + "\n", "rdb_hdr_pcconnpts");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcconorder + "\n", "rdb_hdr_pcconorder");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcextcorr + "\n", "rdb_hdr_pcextcorr");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcgraph + "\n", "rdb_hdr_pcgraph");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcileave + "\n", "rdb_hdr_pcileave");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_hdbestky + "\n", "rdb_hdr_hdbestky");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcctrl + "\n", "rdb_hdr_pcctrl");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcthrespts + "\n", "rdb_hdr_pcthrespts");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcdiscbeg + "\n", "rdb_hdr_pcdiscbeg");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcdiscmid + "\n", "rdb_hdr_pcdiscmid");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcdiscend + "\n", "rdb_hdr_pcdiscend");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcthrespct + "\n", "rdb_hdr_pcthrespct");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcspacial + "\n", "rdb_hdr_pcspacial");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pctemporal + "\n", "rdb_hdr_pctemporal");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcspare + "\n", "rdb_hdr_pcspare");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ileaves + "\n", "rdb_hdr_ileaves");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_kydir + "\n", "rdb_hdr_kydir");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_alt + "\n", "rdb_hdr_alt");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_reps + "\n", "rdb_hdr_reps");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ref + "\n", "rdb_hdr_ref");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcconnorm + "\n", "rdb_hdr_pcconnorm");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcconfitwt + "\n", "rdb_hdr_pcconfitwt");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pclinnorm + "\n", "rdb_hdr_pclinnorm");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pclinfitwt + "\n", "rdb_hdr_pclinfitwt");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcbestky + "\n", "rdb_hdr_pcbestky");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_vrgf + "\n", "rdb_hdr_vrgf");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_vrgfxres + "\n", "rdb_hdr_vrgfxres");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_bp_corr + "\n", "rdb_hdr_bp_corr");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_recv_freq_s + "\n", "rdb_hdr_recv_freq_s");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_recv_freq_e + "\n", "rdb_hdr_recv_freq_e");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_hniter + "\n", "rdb_hdr_hniter");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_fast_rec + "\n", "rdb_hdr_fast_rec");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_refframes + "\n", "rdb_hdr_refframes");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_refframep + "\n", "rdb_hdr_refframep");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_scnframe + "\n", "rdb_hdr_scnframe");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pasframe + "\n", "rdb_hdr_pasframe");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcfitorig + "\n", "rdb_hdr_pcfitorig");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcshotfirst + "\n", "rdb_hdr_pcshotfirst");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcshotlast + "\n", "rdb_hdr_pcshotlast");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcmultegrp + "\n", "rdb_hdr_pcmultegrp");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pclinfix + "\n", "rdb_hdr_pclinfix");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pclinslope + "\n", "rdb_hdr_pclinslope");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcconfix + "\n", "rdb_hdr_pcconfix");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pcconslope + "\n", "rdb_hdr_pcconslope");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_pccoil + "\n", "rdb_hdr_pccoil");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_dp_type + "\n", "rdb_hdr_dp_type");
			gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " +rdb_hdr_ut_ctrl + "\n", "rdb_hdr_ut_ctrl");

//			for(int ii=0;ii<516; ii++) rdb_hdr_excess[ii]=ldis.readShort();
//		gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format.print(System.out, "%+20s: " + new String(filler) + "\n", "filler");
	}

	//
	//  private variables
	//
	private float rdb_hdr_rdbm_rev;
	private long rdb_hdr_run_int;
	private short rdb_hdr_scan_seq;
	private byte[] rdb_hdr_run_char = null; //[6+1];
	private byte[] rdb_hdr_scan_date = null; //[10+1];
	private byte[] rdb_hdr_scan_time = null; //[8+1];
	private byte[] rdb_hdr_logo = null; //[10+1];
	private short rdb_hdr_file_contents;
	private short rdb_hdr_lock_mode;
	private short rdb_hdr_dacq_ctrl;
	private short rdb_hdr_recon_ctrl;
	private short rdb_hdr_exec_ctrl;
	private short rdb_hdr_scan_type;
	private short rdb_hdr_data_collect_type;
	private short rdb_hdr_data_format;
	private short rdb_hdr_recon;
	private short rdb_hdr_datacq;
	private short rdb_hdr_npasses;
	private short rdb_hdr_npomp;
	private short rdb_hdr_nslices;
	private short rdb_hdr_nechoes;
	private short rdb_hdr_navs;
	private short rdb_hdr_nframes;
	private short rdb_hdr_baseline_views;
	private short rdb_hdr_hnover;
	private short rdb_hdr_frame_size;
	private short rdb_hdr_point_size;
	private short rdb_hdr_vquant;
	private short rdb_hdr_cheart;
	private float rdb_hdr_ctr;
	private float rdb_hdr_ctrr;
	private short rdb_hdr_initpass;
	private short rdb_hdr_incrpass;
	private short rdb_hdr_method_ctrl;
	private short rdb_hdr_da_xres;
	private short rdb_hdr_da_yres;
	private short rdb_hdr_rc_xres;
	private short rdb_hdr_rc_yres;
	private short rdb_hdr_im_size;
	private long rdb_hdr_rc_zres;
	private long rdb_hdr_raw_pass_size;
	private long rdb_hdr_sspsave;
	private long rdb_hdr_udasave;
	private float rdb_hdr_fermi_radius;
	private float rdb_hdr_fermi_width;
	private float rdb_hdr_fermi_ecc;
	private float rdb_hdr_clip_min;
	private float rdb_hdr_clip_max;
	private float rdb_hdr_default_offset;
	private float rdb_hdr_xoff;
	private float rdb_hdr_yoff;
	private float rdb_hdr_nwin;
	private float rdb_hdr_ntran;
	private float rdb_hdr_scalei;
	private float rdb_hdr_scaleq;
	private short rdb_hdr_rotation;
	private short rdb_hdr_transpose;
	private short rdb_hdr_kissoff_views;
	private short rdb_hdr_slblank;
	private short rdb_hdr_gradcoil;
	private short rdb_hdr_ddaover;
	private short rdb_hdr_sarr;
	private short rdb_hdr_fd_tr;
	private short rdb_hdr_fd_te;
	private short rdb_hdr_fd_ctrl;
	private short rdb_hdr_algor_num;
	private short rdb_hdr_fd_df_dec;
	private float rdb_hdr_user0;
	private float rdb_hdr_user1;
	private float rdb_hdr_user2;
	private float rdb_hdr_user3;
	private float rdb_hdr_user4;
	private float rdb_hdr_user5;
	private float rdb_hdr_user6;
	private float rdb_hdr_user7;
	private float rdb_hdr_user8;
	private float rdb_hdr_user9;
	private float rdb_hdr_user10;
	private float rdb_hdr_user11;
	private float rdb_hdr_user12;
	private float rdb_hdr_user13;
	private float rdb_hdr_user14;
	private float rdb_hdr_user15;
	private float rdb_hdr_user16;
	private float rdb_hdr_user17;
	private float rdb_hdr_user18;
	private float rdb_hdr_user19;
	private long rdb_hdr_v_type;
	private float rdb_hdr_v_coefxa;
	private float rdb_hdr_v_coefxb;
	private float rdb_hdr_v_coefxc;
	private float rdb_hdr_v_coefxd;
	private float rdb_hdr_v_coefya;
	private float rdb_hdr_v_coefyb;
	private float rdb_hdr_v_coefyc;
	private float rdb_hdr_v_coefyd;
	private float rdb_hdr_v_coefza;
	private float rdb_hdr_v_coefzb;
	private float rdb_hdr_v_coefzc;
	private float rdb_hdr_v_coefzd;
	private float rdb_hdr_vm_coef1;
	private float rdb_hdr_vm_coef2;
	private float rdb_hdr_vm_coef3;
	private float rdb_hdr_vm_coef4;
	private float rdb_hdr_v_venc;
	private long rdb_hdr_ps_command;
	private long rdb_hdr_ps_mps_r1;
	private long rdb_hdr_ps_mps_r2;
	private long rdb_hdr_ps_mps_tg;
	private long rdb_hdr_ps_mps_freq;
	private long rdb_hdr_ps_aps_r1;
	private long rdb_hdr_ps_aps_r2;
	private long rdb_hdr_ps_aps_tg;
	private long rdb_hdr_ps_aps_freq;
	private float rdb_hdr_ps_scalei;
	private float rdb_hdr_ps_scaleq;
	private long rdb_hdr_ps_snr_warning;
	private long rdb_hdr_ps_aps_or_mps;
	private long rdb_hdr_ps_mps_bitmap;
	private byte[] rdb_hdr_ps_powerspec = null; //[256+1];
	private long rdb_hdr_ps_filler1;
	private long rdb_hdr_ps_filler2;
	private float[] rdb_hdr_rec_noise_mean = null; //[16];
	private float[] rdb_hdr_rec_noise_std = null; //[16];
	private short rdb_hdr_im_size_y;
	private long rdb_hdr_data_collect_type1;
	private float rdb_hdr_freq_scale;
	private float rdb_hdr_phase_scale;
	private short rdb_hdr_ovl;
	private short rdb_hdr_pclin;
	private short rdb_hdr_pclinnpts;
	private short rdb_hdr_pclinorder;
	private short rdb_hdr_pclinavg;
	private short rdb_hdr_pccon;
	private short rdb_hdr_pcconnpts;
	private short rdb_hdr_pcconorder;
	private short rdb_hdr_pcextcorr;
	private short rdb_hdr_pcgraph;
	private short rdb_hdr_pcileave;
	private short rdb_hdr_hdbestky;
	private short rdb_hdr_pcctrl;
	private short rdb_hdr_pcthrespts;
	private short rdb_hdr_pcdiscbeg;
	private short rdb_hdr_pcdiscmid;
	private short rdb_hdr_pcdiscend;
	private short rdb_hdr_pcthrespct;
	private short rdb_hdr_pcspacial;
	private short rdb_hdr_pctemporal;
	private short rdb_hdr_pcspare;
	private short rdb_hdr_ileaves;
	private short rdb_hdr_kydir;
	private short rdb_hdr_alt;
	private short rdb_hdr_reps;
	private short rdb_hdr_ref;
	private float rdb_hdr_pcconnorm;
	private float rdb_hdr_pcconfitwt;
	private float rdb_hdr_pclinnorm;
	private float rdb_hdr_pclinfitwt;
	private float rdb_hdr_pcbestky;
	private long rdb_hdr_vrgf;
	private long rdb_hdr_vrgfxres;
	private long rdb_hdr_bp_corr;
	private float rdb_hdr_recv_freq_s;
	private float rdb_hdr_recv_freq_e;
	private long rdb_hdr_hniter;
	private long rdb_hdr_fast_rec;
	private long rdb_hdr_refframes;
	private long rdb_hdr_refframep;
	private long rdb_hdr_scnframe;
	private long rdb_hdr_pasframe;
	private short rdb_hdr_pcfitorig;
	private short rdb_hdr_pcshotfirst;
	private short rdb_hdr_pcshotlast;
	private short rdb_hdr_pcmultegrp;
	private short rdb_hdr_pclinfix;
	private float rdb_hdr_pclinslope;
	private short rdb_hdr_pcconfix;
	private float rdb_hdr_pcconslope;
	private short rdb_hdr_pccoil;
	private short rdb_hdr_dp_type;
	private long rdb_hdr_ut_ctrl;
	private short[] rdb_hdr_excess = null; // [516];
}
