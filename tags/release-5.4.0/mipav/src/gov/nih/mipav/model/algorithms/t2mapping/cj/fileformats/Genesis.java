package gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats;

import java.lang.*;
import java.util.Date;
import java.util.Arrays;
import java.io.*;
import java.util.zip.*;
import java.nio.ByteOrder;
import javax.imageio.stream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;

public class Genesis extends MRIFile {

	/** 
	 *  Default constructor.
	 *
	 */
	public Genesis() 
	{
		initialize();
	}

	/** 
	 *  Constructor that will read in the specified file, if it exists.
	 *
	 */
	public Genesis(String filename) 
	{
		initialize();
		read(filename);
	}

	/**
	 *  Initializes all of the byte arrays.
	 *
	 */
	public void initialize() 
	{

		endian = ByteOrder.BIG_ENDIAN;
		datatype = MRIFile.DATATYPE_SHORT;

		_SU_su_id = new byte[4]; Arrays.fill(_SU_su_id , (byte)0);
		_SU_su_diskid = new byte[1]; Arrays.fill(_SU_su_diskid , (byte)0);
		_SU_prodid = new StringBuffer();
		_SU_su_verscre = new byte[2]; Arrays.fill(_SU_su_verscre , (byte)0);
		_SU_su_verscur = new byte[2]; Arrays.fill(_SU_su_verscur , (byte)0);
		_SU_su_padding = new byte[85]; Arrays.fill(_SU_su_padding , (byte)0);
		_EX_ex_suid = new StringBuffer();
		_EX_hospname = new StringBuffer();
		_EX_patid = new StringBuffer();
		_EX_patname = new StringBuffer();
		_EX_hist = new StringBuffer();
		_EX_reqnum = new StringBuffer();
		_EX_refphy = new StringBuffer();
		_EX_diagrad = new StringBuffer();
		_EX_op = new StringBuffer();
		_EX_ex_desc = new StringBuffer();
		_EX_ex_typ = new StringBuffer();
		_EX_ex_sysid = new StringBuffer();
		_EX_ex_alloc_key = new StringBuffer();
		_EX_ex_verscre = new StringBuffer();
		_EX_ex_verscur = new StringBuffer();
		_EX_uniq_sys_id = new StringBuffer();
		_EX_service_id = new StringBuffer();
		_EX_mobile_loc = new StringBuffer();
		_EX_ex_padding = new StringBuffer();
		_SE_se_suid = new StringBuffer();
		_SE_se_diskid = new StringBuffer();
		_SE_se_desc = new StringBuffer();
		_SE_pr_sysid = new StringBuffer();
		_SE_pansysid = new StringBuffer();
		_SE_anref = new StringBuffer();
		_SE_prtcl = new StringBuffer();
		_SE_start_ras = new StringBuffer();
		_SE_end_ras = new StringBuffer();
		_SE_se_alloc_key = new StringBuffer();
		_SE_se_verscre = new StringBuffer();
		_SE_se_verscur = new StringBuffer();
		_SE_se_padding = new StringBuffer();
		_MR_im_suid = new StringBuffer();
		_MR_im_diskid = new StringBuffer();
		_MR_pdid = new StringBuffer();
		_MR_contrastIV = new StringBuffer();
		_MR_contrastOral = new StringBuffer();
		_MR_forimgrev = new StringBuffer();
		_MR_psdname = new StringBuffer();
		_MR_psd_iname = new StringBuffer();
		_MR_cname = new StringBuffer();
		_MR_im_alloc_key = new StringBuffer();
		_MR_im_verscre = new StringBuffer();
		_MR_im_verscur = new StringBuffer();
		_MR_proj_name = new StringBuffer();
		_MR_ref_img = new StringBuffer();
		_MR_sum_img = new StringBuffer();
		_MR_slop_str_1 = new StringBuffer();
		_MR_slop_str_2 = new StringBuffer();
		_MR_mr_padding = new StringBuffer();
	}

	/**
	 *  Empty write method as there is really no use for writing 
	 *  the Genesis file format (yet).
	 *
	 */
	public void write(String filename ) 
	{
	}

	public double[] getTE()
	{
		double[] telist = new double[1];
		telist[0] = _MR_te;

		return telist;
	}

	/** 
	 *  Ths show function.
	 *
	 */
	public void show() 
	{
		Date date = new Date();

		System.out.println("\t_IH_img_magic = " + _IH_img_magic);
		System.out.println("\t_IH_img_hdr_length = " + _IH_img_hdr_length);
		System.out.println("\t_IH_img_width = " + _IH_img_width);
		System.out.println("\t_IH_img_height = " + _IH_img_height);
		System.out.println("\t_IH_img_depth = " + _IH_img_depth);
		System.out.println("\t_IH_img_compress = " + _IH_img_compress);
		System.out.println("\t_IH_img_dwindow = " + _IH_img_dwindow);
		System.out.println("\t_IH_img_dlevel = " + _IH_img_dlevel);
		System.out.println("\t_IH_img_bgshade = " + _IH_img_bgshade);
		System.out.println("\t_IH_img_ovrflow = " + _IH_img_ovrflow);
		System.out.println("\t_IH_img_undflow = " + _IH_img_undflow);
		System.out.println("\t_IH_img_top_offset = " + _IH_img_top_offset);
		System.out.println("\t_IH_img_bot_offset = " + _IH_img_bot_offset);
		System.out.println("\t_IH_img_version = " + _IH_img_version);
		System.out.println("\t_IH_img_checksum = " + _IH_img_checksum);
		System.out.println("\t_IH_img_p_id = " + _IH_img_p_id);
		System.out.println("\t_IH_img_l_id = " + _IH_img_l_id);
		System.out.println("\t_IH_img_p_unpack = " + _IH_img_p_unpack);
		System.out.println("\t_IH_img_l_unpack = " + _IH_img_l_unpack);
		System.out.println("\t_IH_img_p_compress = " + _IH_img_p_compress);
		System.out.println("\t_IH_img_l_compress = " + _IH_img_l_compress);
		System.out.println("\t_IH_img_p_histo = " + _IH_img_p_histo);
		System.out.println("\t_IH_img_l_histo = " + _IH_img_l_histo);
		System.out.println("\t_IH_img_p_text = " + _IH_img_p_text);
		System.out.println("\t_IH_img_l_text = " + _IH_img_l_text);
		System.out.println("\t_IH_img_p_graphics = " + _IH_img_p_graphics);
		System.out.println("\t_IH_img_l_graphics = " + _IH_img_l_graphics);
		System.out.println("\t_IH_img_p_dbHdr = " + _IH_img_p_dbHdr);
		System.out.println("\t_IH_img_l_dbHdr = " + _IH_img_l_dbHdr);
		System.out.println("\t_IH_img_levelOffset = " + _IH_img_levelOffset);
		System.out.println("\t_IH_img_p_user = " + _IH_img_p_user);
		System.out.println("\t_IH_img_l_user = " + _IH_img_l_user);
		System.out.println("\t_IH_img_p_suite = " + _IH_img_p_suite);
		System.out.println("\t_IH_img_l_suite = " + _IH_img_l_suite);
		System.out.println("\t_IH_img_p_exam = " + _IH_img_p_exam);
		System.out.println("\t_IH_img_l_exam = " + _IH_img_l_exam);
		System.out.println("\t_IH_img_p_series = " + _IH_img_p_series);
		System.out.println("\t_IH_img_l_series = " + _IH_img_l_series);
		System.out.println("\t_IH_img_p_image = " + _IH_img_p_image);
		System.out.println("\t_IH_img_l_image = " + _IH_img_l_image);
		System.out.println("\t_HS_hs_version = " + _HS_hs_version);
		System.out.println("\t_HS_hs_sd = " + _HS_hs_sd);
		System.out.println("\t_HS_hs_mean = " + _HS_hs_mean);
		System.out.println("\t_HS_hs_min = " + _HS_hs_min);
		System.out.println("\t_HS_hs_max = " + _HS_hs_max);
		System.out.println("\t_HS_hs_first = " + _HS_hs_first);
		System.out.println("\t_HS_hs_region = " + _HS_hs_region);
		System.out.println("\t_HS_hs_length = " + _HS_hs_length);
		System.out.println("\t_SU_su_id = " + new String(_SU_su_id));
		System.out.println("\t_SU_su_uniq = " + _SU_su_uniq);
		System.out.println("\t_SU_su_diskid = " + new String(_SU_su_diskid));
		System.out.println("\t_SU_prodid = " + new String(_SU_prodid));
		System.out.println("\t_SU_su_verscre = " + new String(_SU_su_verscre));
		System.out.println("\t_SU_su_verscur = " + new String(_SU_su_verscur));
		System.out.println("\t_SU_su_checksum = " + _SU_su_checksum);
		System.out.println("\t_SU_su_padding = " + new String(_SU_su_padding));
		System.out.println("\t_EX_ex_suid = " + new String(_EX_ex_suid));
		System.out.println("\t_EX_ex_uniq = " + _EX_ex_uniq);
		System.out.println("\t_EX_ex_diskid = " + _EX_ex_diskid);
		System.out.println("\t_EX_ex_no = " + _EX_ex_no);
		System.out.println("\t_EX_hospname = " + new String(_EX_hospname));
		System.out.println("\t_EX_detect = " + _EX_detect);
		System.out.println("\t_EX_numcells = " + _EX_numcells);
		System.out.println("\t_EX_zerocell = " + _EX_zerocell);
		System.out.println("\t_EX_cellspace = " + _EX_cellspace);
		System.out.println("\t_EX_srctodet = " + _EX_srctodet);
		System.out.println("\t_EX_srctoiso = " + _EX_srctoiso);
		System.out.println("\t_EX_tubetyp = " + _EX_tubetyp);
		System.out.println("\t_EX_dastyp = " + _EX_dastyp);
		System.out.println("\t_EX_num_dcnk = " + _EX_num_dcnk);
		System.out.println("\t_EX_dcn_len = " + _EX_dcn_len);
		System.out.println("\t_EX_dcn_density = " + _EX_dcn_density);
		System.out.println("\t_EX_dcn_stepsize = " + _EX_dcn_stepsize);
		System.out.println("\t_EX_dcn_shiftcnt = " + _EX_dcn_shiftcnt);
		System.out.println("\t_EX_magstrength = " + _EX_magstrength);
		System.out.println("\t_EX_patid = " + new String(_EX_patid));
		System.out.println("\t_EX_patname = " + new String(_EX_patname));
		System.out.println("\t_EX_patage = " + _EX_patage);
		System.out.println("\t_EX_patian = " + _EX_patian);
		System.out.println("\t_EX_patsex = " + _EX_patsex);
		System.out.println("\t_EX_patweight = " + _EX_patweight);
		System.out.println("\t_EX_trauma = " + _EX_trauma);
		System.out.println("\t_EX_hist = " + new String(_EX_hist));
		System.out.println("\t_EX_reqnum = " + new String(_EX_reqnum));
		date.setTime(_EX_ex_datetime*1000L);
		System.out.println("\t_EX_ex_datetime = " + _EX_ex_datetime 
			+ " ( " + date.toString() + ")" );
		System.out.println("\t_EX_refphy = " + new String(_EX_refphy));
		System.out.println("\t_EX_diagrad = " + new String(_EX_diagrad));
		System.out.println("\t_EX_op = " + new String(_EX_op));
		System.out.println("\t_EX_ex_desc = " + new String(_EX_ex_desc));
		System.out.println("\t_EX_ex_typ = " + new String(_EX_ex_typ));
		System.out.println("\t_EX_ex_format = " + _EX_ex_format);
		System.out.println("\t_EX_ex_sysid = " + new String(_EX_ex_sysid));
		System.out.println("\t_EX_ex_lastmod = " + _EX_ex_lastmod);
		System.out.println("\t_EX_protocolflag = " + _EX_protocolflag);
		System.out.println("\t_EX_ex_alloc_key = " + new String(_EX_ex_alloc_key));
		System.out.println("\t_EX_ex_delta_cnt = " + _EX_ex_delta_cnt);
		System.out.println("\t_EX_ex_verscre = " + new String(_EX_ex_verscre));
		System.out.println("\t_EX_ex_verscur = " + new String(_EX_ex_verscur));
		System.out.println("\t_EX_ex_checksum = " + _EX_ex_checksum);
		System.out.println("\t_EX_ex_complete = " + _EX_ex_complete);
		System.out.println("\t_EX_ex_seriesct = " + _EX_ex_seriesct);
		System.out.println("\t_EX_ex_numarch = " + _EX_ex_numarch);
		System.out.println("\t_EX_ex_numseries = " + _EX_ex_numseries);
		System.out.println("\t_EX_ex_numunser = " + _EX_ex_numunser);
		System.out.println("\t_EX_ex_toarchcnt = " + _EX_ex_toarchcnt);
		System.out.println("\t_EX_ex_prospcnt = " + _EX_ex_prospcnt);
		System.out.println("\t_EX_ex_modelnum = " + _EX_ex_modelnum);
		System.out.println("\t_EX_ex_modelcnt = " + _EX_ex_modelcnt);
		System.out.println("\t_EX_ex_stat = " + _EX_ex_stat);
		System.out.println("\t_EX_uniq_sys_id = " + new String(_EX_uniq_sys_id));
		System.out.println("\t_EX_service_id = " + new String(_EX_service_id));
		System.out.println("\t_EX_mobile_loc = " + new String(_EX_mobile_loc));
		System.out.println("\t_EX_ex_padding = " + new String(_EX_ex_padding));
		System.out.println("\t_SE_se_suid = " + new String(_SE_se_suid));
		System.out.println("\t_SE_se_uniq = " + _SE_se_uniq);
		System.out.println("\t_SE_se_diskid = " + new String(_SE_se_diskid));
		System.out.println("\t_SE_se_exno = " + _SE_se_exno);
		System.out.println("\t_SE_se_no = " + _SE_se_no);
		System.out.println("\t_SE_se_datetime = " + _SE_se_datetime);
		System.out.println("\t_SE_se_actual_dt = " + _SE_se_actual_dt);
		System.out.println("\t_SE_se_desc = " + new String(_SE_se_desc));
		System.out.println("\t_SE_pr_sysid = " + new String(_SE_pr_sysid));
		System.out.println("\t_SE_pansysid = " + new String(_SE_pansysid));
		System.out.println("\t_SE_se_typ = " + _SE_se_typ);
		System.out.println("\t_SE_se_source = " + _SE_se_source);
		System.out.println("\t_SE_se_plane = " + _SE_se_plane);
		System.out.println("\t_SE_position = " + _SE_position);
		System.out.println("\t_SE_entry = " + _SE_entry);
		System.out.println("\t_SE_anref = " + new String(_SE_anref));
		System.out.println("\t_SE_lmhor = " + _SE_lmhor);
		System.out.println("\t_SE_prtcl = " + new String(_SE_prtcl));
		System.out.println("\t_SE_se_contrast = " + _SE_se_contrast);
		System.out.println("\t_SE_start_ras = " + new String(_SE_start_ras));
		System.out.println("\t_SE_start_loc = " + _SE_start_loc);
		System.out.println("\t_SE_end_ras = " + new String(_SE_end_ras));
		System.out.println("\t_SE_end_loc = " + _SE_end_loc);
		System.out.println("\t_SE_se_pseq = " + _SE_se_pseq);
		System.out.println("\t_SE_se_sortorder = " + _SE_se_sortorder);
		System.out.println("\t_SE_se_lndmrkcnt = " + _SE_se_lndmrkcnt);
		System.out.println("\t_SE_se_nacq = " + _SE_se_nacq);
		System.out.println("\t_SE_xbasest = " + _SE_xbasest);
		System.out.println("\t_SE_xbaseend = " + _SE_xbaseend);
		System.out.println("\t_SE_xenhst = " + _SE_xenhst);
		System.out.println("\t_SE_xenhend = " + _SE_xenhend);
		System.out.println("\t_SE_se_lastmod = " + _SE_se_lastmod);
		System.out.println("\t_SE_se_alloc_key = " + new String(_SE_se_alloc_key));
		System.out.println("\t_SE_se_delta_cnt = " + _SE_se_delta_cnt);
		System.out.println("\t_SE_se_verscre = " + new String(_SE_se_verscre));
		System.out.println("\t_SE_se_verscur = " + new String(_SE_se_verscur));
		System.out.println("\t_SE_se_pds_a = " + _SE_se_pds_a);
		System.out.println("\t_SE_se_pds_c = " + _SE_se_pds_c);
		System.out.println("\t_SE_se_pds_u = " + _SE_se_pds_u);
		System.out.println("\t_SE_se_checksum = " + _SE_se_checksum);
		System.out.println("\t_SE_se_complete = " + _SE_se_complete);
		System.out.println("\t_SE_se_numarch = " + _SE_se_numarch);
		System.out.println("\t_SE_se_imagect = " + _SE_se_imagect);
		System.out.println("\t_SE_se_numimages = " + _SE_se_numimages);
		System.out.println("\t_SE_se_numunimg = " + _SE_se_numunimg);
		System.out.println("\t_SE_se_toarchcnt = " + _SE_se_toarchcnt);
		System.out.println("\t_SE_echo1_alpha = " + _SE_echo1_alpha);
		System.out.println("\t_SE_echo1_beta = " + _SE_echo1_beta);
		System.out.println("\t_SE_echo1_window = " + _SE_echo1_window);
		System.out.println("\t_SE_echo1_level = " + _SE_echo1_level);
		System.out.println("\t_SE_echo2_alpha = " + _SE_echo2_alpha);
		System.out.println("\t_SE_echo2_beta = " + _SE_echo2_beta);
		System.out.println("\t_SE_echo2_window = " + _SE_echo2_window);
		System.out.println("\t_SE_echo2_level = " + _SE_echo2_level);
		System.out.println("\t_SE_echo3_alpha = " + _SE_echo3_alpha);
		System.out.println("\t_SE_echo3_beta = " + _SE_echo3_beta);
		System.out.println("\t_SE_echo3_window = " + _SE_echo3_window);
		System.out.println("\t_SE_echo3_level = " + _SE_echo3_level);
		System.out.println("\t_SE_echo4_alpha = " + _SE_echo4_alpha);
		System.out.println("\t_SE_echo4_beta = " + _SE_echo4_beta);
		System.out.println("\t_SE_echo4_window = " + _SE_echo4_window);
		System.out.println("\t_SE_echo4_level = " + _SE_echo4_level);
		System.out.println("\t_SE_echo5_alpha = " + _SE_echo5_alpha);
		System.out.println("\t_SE_echo5_beta = " + _SE_echo5_beta);
		System.out.println("\t_SE_echo5_window = " + _SE_echo5_window);
		System.out.println("\t_SE_echo5_level = " + _SE_echo5_level);
		System.out.println("\t_SE_echo6_alpha = " + _SE_echo6_alpha);
		System.out.println("\t_SE_echo6_beta = " + _SE_echo6_beta);
		System.out.println("\t_SE_echo6_window = " + _SE_echo6_window);
		System.out.println("\t_SE_echo6_level = " + _SE_echo6_level);
		System.out.println("\t_SE_echo7_alpha = " + _SE_echo7_alpha);
		System.out.println("\t_SE_echo7_beta = " + _SE_echo7_beta);
		System.out.println("\t_SE_echo7_window = " + _SE_echo7_window);
		System.out.println("\t_SE_echo7_level = " + _SE_echo7_level);
		System.out.println("\t_SE_echo8_alpha = " + _SE_echo8_alpha);
		System.out.println("\t_SE_echo8_beta = " + _SE_echo8_beta);
		System.out.println("\t_SE_echo8_window = " + _SE_echo8_window);
		System.out.println("\t_SE_echo8_level = " + _SE_echo8_level);
		System.out.println("\t_SE_se_padding = " + new String(_SE_se_padding));
		System.out.println("\t_MR_im_suid = " + new String(_MR_im_suid));
		System.out.println("\t_MR_im_uniq = " + _MR_im_uniq);
		System.out.println("\t_MR_im_diskid = " + new String(_MR_im_diskid));
		System.out.println("\t_MR_im_exno = " + _MR_im_exno);
		System.out.println("\t_MR_im_seno = " + _MR_im_seno);
		System.out.println("\t_MR_im_no = " + _MR_im_no);
		System.out.println("\t_MR_im_datetime = " + _MR_im_datetime);
		System.out.println("\t_MR_im_actual_dt = " + _MR_im_actual_dt);
		System.out.println("\t_MR_sctime = " + _MR_sctime);
		System.out.println("\t_MR_slthick = " + _MR_slthick);
		System.out.println("\t_MR_imatrix_X = " + _MR_imatrix_X);
		System.out.println("\t_MR_imatrix_Y = " + _MR_imatrix_Y);
		System.out.println("\t_MR_dfov = " + _MR_dfov);
		System.out.println("\t_MR_dfov_rect = " + _MR_dfov_rect);
		System.out.println("\t_MR_dim_X = " + _MR_dim_X);
		System.out.println("\t_MR_dim_Y = " + _MR_dim_Y);
		System.out.println("\t_MR_pixsize_X = " + _MR_pixsize_X);
		System.out.println("\t_MR_pixsize_Y = " + _MR_pixsize_Y);
		System.out.println("\t_MR_pdid = " + new String(_MR_pdid));
		System.out.println("\t_MR_contrastIV = " + new String(_MR_contrastIV));
		System.out.println("\t_MR_contrastOral = " + new String(_MR_contrastOral));
		System.out.println("\t_MR_contmode = " + _MR_contmode);
		System.out.println("\t_MR_serrx = " + _MR_serrx);
		System.out.println("\t_MR_imgrx = " + _MR_imgrx);
		System.out.println("\t_MR_screenformat = " + _MR_screenformat);
		System.out.println("\t_MR_plane = " + _MR_plane);
		System.out.println("\t_MR_scanspacing = " + _MR_scanspacing);
		System.out.println("\t_MR_im_compress = " + _MR_im_compress);
		System.out.println("\t_MR_im_scouttype = " + _MR_im_scouttype);
		System.out.println("\t_MR_loc_ras = " + _MR_loc_ras);
		System.out.println("\t_MR_loc = " + _MR_loc);
		System.out.println("\t_MR_ctr_R = " + _MR_ctr_R);
		System.out.println("\t_MR_ctr_A = " + _MR_ctr_A);
		System.out.println("\t_MR_ctr_S = " + _MR_ctr_S);
		System.out.println("\t_MR_norm_R = " + _MR_norm_R);
		System.out.println("\t_MR_norm_A = " + _MR_norm_A);
		System.out.println("\t_MR_norm_S = " + _MR_norm_S);
		System.out.println("\t_MR_tlhc_R = " + _MR_tlhc_R);
		System.out.println("\t_MR_tlhc_A = " + _MR_tlhc_A);
		System.out.println("\t_MR_tlhc_S = " + _MR_tlhc_S);
		System.out.println("\t_MR_trhc_R = " + _MR_trhc_R);
		System.out.println("\t_MR_trhc_A = " + _MR_trhc_A);
		System.out.println("\t_MR_trhc_S = " + _MR_trhc_S);
		System.out.println("\t_MR_brhc_R = " + _MR_brhc_R);
		System.out.println("\t_MR_brhc_A = " + _MR_brhc_A);
		System.out.println("\t_MR_brhc_S = " + _MR_brhc_S);
		System.out.println("\t_MR_forimgrev = " + new String(_MR_forimgrev));
		System.out.println("\t_MR_tr = " + _MR_tr);
		System.out.println("\t_MR_ti = " + _MR_ti);
		System.out.println("\t_MR_te = " + _MR_te);
		System.out.println("\t_MR_te2 = " + _MR_te2);
		System.out.println("\t_MR_numecho = " + _MR_numecho);
		System.out.println("\t_MR_echonum = " + _MR_echonum);
		System.out.println("\t_MR_tbldlta = " + _MR_tbldlta);
		System.out.println("\t_MR_nex = " + _MR_nex);
		System.out.println("\t_MR_contig = " + _MR_contig);
		System.out.println("\t_MR_hrtrate = " + _MR_hrtrate);
		System.out.println("\t_MR_tdel = " + _MR_tdel);
		System.out.println("\t_MR_saravg = " + _MR_saravg);
		System.out.println("\t_MR_sarpeak = " + _MR_sarpeak);
		System.out.println("\t_MR_monsar = " + _MR_monsar);
		System.out.println("\t_MR_trgwindow = " + _MR_trgwindow);
		System.out.println("\t_MR_reptime = " + _MR_reptime);
		System.out.println("\t_MR_imgpcyc = " + _MR_imgpcyc);
		System.out.println("\t_MR_xmtgain = " + _MR_xmtgain);
		System.out.println("\t_MR_rcvgain1 = " + _MR_rcvgain1);
		System.out.println("\t_MR_rcvgain2 = " + _MR_rcvgain2);
		System.out.println("\t_MR_mr_flip = " + _MR_mr_flip);
		System.out.println("\t_MR_mindat = " + _MR_mindat);
		System.out.println("\t_MR_cphase = " + _MR_cphase);
		System.out.println("\t_MR_swappf = " + _MR_swappf);
		System.out.println("\t_MR_pauseint = " + _MR_pauseint);
		System.out.println("\t_MR_pausetime = " + _MR_pausetime);
		System.out.println("\t_MR_obplane = " + _MR_obplane);
		System.out.println("\t_MR_slocfov = " + _MR_slocfov);
		System.out.println("\t_MR_xmtfreq = " + _MR_xmtfreq);
		System.out.println("\t_MR_autoxmtfreq = " + _MR_autoxmtfreq);
		System.out.println("\t_MR_autoxmtgain = " + _MR_autoxmtgain);
		System.out.println("\t_MR_prescan_r1 = " + _MR_prescan_r1);
		System.out.println("\t_MR_prescan_r2 = " + _MR_prescan_r2);
		System.out.println("\t_MR_user_bitmap = " + _MR_user_bitmap);
		System.out.println("\t_MR_cenfreq = " + _MR_cenfreq);
		System.out.println("\t_MR_imode = " + _MR_imode);
		System.out.println("\t_MR_iopt = " + _MR_iopt);
		System.out.println("\t_MR_pseq = " + _MR_pseq);
		System.out.println("\t_MR_pseqmode = " + _MR_pseqmode);
		System.out.println("\t_MR_psdname = " + new String(_MR_psdname));
		System.out.println("\t_MR_psd_datetime = " + _MR_psd_datetime);
		System.out.println("\t_MR_psd_iname = " + new String(_MR_psd_iname));
		System.out.println("\t_MR_ctyp = " + _MR_ctyp);
		System.out.println("\t_MR_cname = " + new String(_MR_cname));
		System.out.println("\t_MR_surfctyp = " + _MR_surfctyp);
		System.out.println("\t_MR_surfcext = " + _MR_surfcext);
		System.out.println("\t_MR_rawrunnum = " + _MR_rawrunnum);
		System.out.println("\t_MR_cal_fldstr = " + _MR_cal_fldstr);
		System.out.println("\t_MR_supp_tech = " + _MR_supp_tech);
		System.out.println("\t_MR_vbw = " + _MR_vbw);
		System.out.println("\t_MR_slquant = " + _MR_slquant);
		System.out.println("\t_MR_gpre = " + _MR_gpre);
		System.out.println("\t_MR_intr_del = " + _MR_intr_del);
		System.out.println("\t_MR_user0 = " + _MR_user0);
		System.out.println("\t_MR_user1 = " + _MR_user1);
		System.out.println("\t_MR_user2 = " + _MR_user2);
		System.out.println("\t_MR_user3 = " + _MR_user3);
		System.out.println("\t_MR_user4 = " + _MR_user4);
		System.out.println("\t_MR_user5 = " + _MR_user5);
		System.out.println("\t_MR_user6 = " + _MR_user6);
		System.out.println("\t_MR_user7 = " + _MR_user7);
		System.out.println("\t_MR_user8 = " + _MR_user8);
		System.out.println("\t_MR_user9 = " + _MR_user9);
		System.out.println("\t_MR_user10 = " + _MR_user10);
		System.out.println("\t_MR_user11 = " + _MR_user11);
		System.out.println("\t_MR_user12 = " + _MR_user12);
		System.out.println("\t_MR_user13 = " + _MR_user13);
		System.out.println("\t_MR_user14 = " + _MR_user14);
		System.out.println("\t_MR_user15 = " + _MR_user15);
		System.out.println("\t_MR_user16 = " + _MR_user16);
		System.out.println("\t_MR_user17 = " + _MR_user17);
		System.out.println("\t_MR_user18 = " + _MR_user18);
		System.out.println("\t_MR_user19 = " + _MR_user19);
		System.out.println("\t_MR_user20 = " + _MR_user20);
		System.out.println("\t_MR_user21 = " + _MR_user21);
		System.out.println("\t_MR_user22 = " + _MR_user22);
		System.out.println("\t_MR_user23 = " + _MR_user23);
		System.out.println("\t_MR_user24 = " + _MR_user24);
		System.out.println("\t_MR_im_alloc_key = " + new String(_MR_im_alloc_key));
		System.out.println("\t_MR_im_lastmod = " + _MR_im_lastmod);
		System.out.println("\t_MR_im_verscre = " + new String(_MR_im_verscre));
		System.out.println("\t_MR_im_verscur = " + new String(_MR_im_verscur));
		System.out.println("\t_MR_im_pds_a = " + _MR_im_pds_a);
		System.out.println("\t_MR_im_pds_c = " + _MR_im_pds_c);
		System.out.println("\t_MR_im_pds_u = " + _MR_im_pds_u);
		System.out.println("\t_MR_im_checksum = " + _MR_im_checksum);
		System.out.println("\t_MR_im_archived = " + _MR_im_archived);
		System.out.println("\t_MR_im_complete = " + _MR_im_complete);
		System.out.println("\t_MR_satbits = " + _MR_satbits);
		System.out.println("\t_MR_scic = " + _MR_scic);
		System.out.println("\t_MR_satxloc1 = " + _MR_satxloc1);
		System.out.println("\t_MR_satxloc2 = " + _MR_satxloc2);
		System.out.println("\t_MR_satyloc1 = " + _MR_satyloc1);
		System.out.println("\t_MR_satyloc2 = " + _MR_satyloc2);
		System.out.println("\t_MR_satzloc1 = " + _MR_satzloc1);
		System.out.println("\t_MR_satzloc2 = " + _MR_satzloc2);
		System.out.println("\t_MR_satxthick = " + _MR_satxthick);
		System.out.println("\t_MR_satythick = " + _MR_satythick);
		System.out.println("\t_MR_satzthick = " + _MR_satzthick);
		System.out.println("\t_MR_flax = " + _MR_flax);
		System.out.println("\t_MR_venc = " + _MR_venc);
		System.out.println("\t_MR_thk_disclmr = " + _MR_thk_disclmr);
		System.out.println("\t_MR_ps_flag = " + _MR_ps_flag);
		System.out.println("\t_MR_ps_status = " + _MR_ps_status);
		System.out.println("\t_MR_image_type = " + _MR_image_type);
		System.out.println("\t_MR_vas_collapse = " + _MR_vas_collapse);
		System.out.println("\t_MR_user23n = " + _MR_user23n);
		System.out.println("\t_MR_user24n = " + _MR_user24n);
		System.out.println("\t_MR_proj_alg = " + _MR_proj_alg);
		System.out.println("\t_MR_proj_name = " + new String(_MR_proj_name));
		System.out.println("\t_MR_x_axis_rot = " + _MR_x_axis_rot);
		System.out.println("\t_MR_y_axis_rot = " + _MR_y_axis_rot);
		System.out.println("\t_MR_z_axis_rot = " + _MR_z_axis_rot);
		System.out.println("\t_MR_thresh_min1 = " + _MR_thresh_min1);
		System.out.println("\t_MR_thresh_max1 = " + _MR_thresh_max1);
		System.out.println("\t_MR_thresh_min2 = " + _MR_thresh_min2);
		System.out.println("\t_MR_thresh_max2 = " + _MR_thresh_max2);
		System.out.println("\t_MR_echo_trn_len = " + _MR_echo_trn_len);
		System.out.println("\t_MR_frac_echo = " + _MR_frac_echo);
		System.out.println("\t_MR_prep_pulse = " + _MR_prep_pulse);
		System.out.println("\t_MR_cphasenum = " + _MR_cphasenum);
		System.out.println("\t_MR_var_echo = " + _MR_var_echo);
		System.out.println("\t_MR_ref_img = " + new String(_MR_ref_img));
		System.out.println("\t_MR_sum_img = " + new String(_MR_sum_img));
		System.out.println("\t_MR_img_window = " + _MR_img_window);
		System.out.println("\t_MR_img_level = " + _MR_img_level);
		System.out.println("\t_MR_slop_int_1 = " + _MR_slop_int_1);
		System.out.println("\t_MR_slop_int_2 = " + _MR_slop_int_2);
		System.out.println("\t_MR_slop_int_3 = " + _MR_slop_int_3);
		System.out.println("\t_MR_slop_int_4 = " + _MR_slop_int_4);
		System.out.println("\t_MR_slop_int_5 = " + _MR_slop_int_5);
		System.out.println("\t_MR_slop_float_1 = " + _MR_slop_float_1);
		System.out.println("\t_MR_slop_float_2 = " + _MR_slop_float_2);
		System.out.println("\t_MR_slop_float_3 = " + _MR_slop_float_3);
		System.out.println("\t_MR_slop_float_4 = " + _MR_slop_float_4);
		System.out.println("\t_MR_slop_float_5 = " + _MR_slop_float_5);
		System.out.println("\t_MR_slop_str_1 = " + new String(_MR_slop_str_1));
		System.out.println("\t_MR_slop_str_2 = " + new String(_MR_slop_str_2));
		//System.out.println("\t_MR_mr_padding = " + new String(_MR_mr_padding));
	}

	/**
	 *  Reads in the header from the given input stream.
	 *
	 */
	public void readHeader(ImageInputStream ldis) 
	{
		int bytes_read = 0;

		try {
			_IH_img_magic = ldis.readInt();
			_IH_img_hdr_length = ldis.readInt();
			_IH_img_width = ldis.readInt();
			_IH_img_height = ldis.readInt();
			_IH_img_depth = ldis.readInt();
			_IH_img_compress = ldis.readInt();
			_IH_img_dwindow = ldis.readInt();
			_IH_img_dlevel = ldis.readInt();
			_IH_img_bgshade = ldis.readInt();
			_IH_img_ovrflow = ldis.readInt();
			_IH_img_undflow = ldis.readInt();
			_IH_img_top_offset = ldis.readInt();
			_IH_img_bot_offset = ldis.readInt();
			_IH_img_version = ldis.readShort();
			_IH_img_checksum = ldis.readUnsignedShort();
			_IH_img_p_id = ldis.readInt();
			_IH_img_l_id = ldis.readInt();
			_IH_img_p_unpack = ldis.readInt();
			_IH_img_l_unpack = ldis.readInt();
			_IH_img_p_compress = ldis.readInt();
			_IH_img_l_compress = ldis.readInt();
			_IH_img_p_histo = ldis.readInt();
			_IH_img_l_histo = ldis.readInt();
			_IH_img_p_text = ldis.readInt();
			_IH_img_l_text = ldis.readInt();
			_IH_img_p_graphics = ldis.readInt();
			_IH_img_l_graphics = ldis.readInt();
			_IH_img_p_dbHdr = ldis.readInt();
			_IH_img_l_dbHdr = ldis.readInt();
			_IH_img_levelOffset = ldis.readInt();
			_IH_img_p_user = ldis.readInt();
			_IH_img_l_user = ldis.readInt();
			_IH_img_p_suite = ldis.readInt();
			_IH_img_l_suite = ldis.readInt();
			_IH_img_p_exam = ldis.readInt();
			_IH_img_l_exam = ldis.readInt();
			_IH_img_p_series = ldis.readInt();
			_IH_img_l_series = ldis.readInt();
			_IH_img_p_image = ldis.readInt();

			//ldis.seek(0 + 152);
			_IH_img_l_image = ldis.readInt();

			bytes_read = 156;
			ldis.skipBytes( _IH_img_p_histo - bytes_read );
			bytes_read = _IH_img_p_histo;

			//-----------------------------------------------------------
			//
			//                HS SECTION
			//
			_HS_hs_version = ldis.readInt();
			_HS_hs_sd = ldis.readFloat();
			_HS_hs_mean = ldis.readShort();
			_HS_hs_min = ldis.readShort();
			_HS_hs_max = ldis.readShort();
			_HS_hs_first = ldis.readShort();
			_HS_hs_region = ldis.readShort();
			_HS_hs_length = ldis.readShort();

			bytes_read += 20;

			ldis.skipBytes( _IH_img_p_suite - bytes_read );

			bytes_read = _IH_img_p_suite;

			//-----------------------------------------------------------
			//
			//                SU SECTION
			//

			for(int ii=0; ii<4; ii++) {
				_SU_su_id[ii] = (byte)ldis.readUnsignedByte();
				if( _SU_su_id[ii] == '\014') _SU_su_id[ii] = '\000';
			}

			_SU_su_uniq = ldis.readShort();

			for(int ii=0; ii<1; ii++) {
				_SU_su_diskid[ii] = (byte)ldis.readUnsignedByte();
				if( _SU_su_diskid[ii] == '\014') _SU_su_diskid[ii] = '\000';
			}

			readString(ldis, _SU_prodid, 13);

			for(int ii=0; ii<2; ii++) {
				_SU_su_verscre[ii] = (byte)ldis.readUnsignedByte();
				if( _SU_su_verscre[ii] == '\014') _SU_su_verscre[ii] = '\000';
			}

			for(int ii=0; ii<2; ii++) {
				_SU_su_verscur[ii] = (byte)ldis.readUnsignedByte();
				if( _SU_su_verscur[ii] == '\014') _SU_su_verscur[ii] = '\000';
			}

			_SU_su_checksum = ldis.readInt();

			for(int ii=0; ii<85; ii++) {
				_SU_su_padding[ii] = (byte)ldis.readUnsignedByte();
				if( _SU_su_padding[ii] == '\014') _SU_su_padding[ii] = '\000';
			}

			bytes_read += (28 + 85);
			ldis.skipBytes( _IH_img_p_exam - bytes_read );
			bytes_read  = _IH_img_p_exam;

			//-----------------------------------------------------------
			//
			//                EXAM SECTION
			//

//			for(int ii=0; ii<4; ii++) {
//				_EX_ex_suid[ii] = (byte)ldis.readUnsignedByte();
//				if( _EX_ex_suid[ii] == '\014') _EX_ex_suid[ii] = '\000';
//			}
			readString(ldis, _EX_ex_suid, 4);

			_EX_ex_uniq = ldis.readShort();
			_EX_ex_diskid = ldis.readChar();
			_EX_ex_no = ldis.readUnsignedShort();
//			for(int ii=0; ii<32; ii++) {
//				_EX_hospname[ii] = (byte)ldis.readUnsignedByte();
//				if( _EX_hospname[ii] == '\014') _EX_hospname[ii] = '\000';
//			}
			readString(ldis, _EX_hospname, 32);

			_EX_detect = ldis.readShort();
			_EX_numcells = ldis.readInt();
			_EX_zerocell = ldis.readFloat();
			_EX_cellspace = ldis.readFloat();
			_EX_srctodet = ldis.readFloat();
			_EX_srctoiso = ldis.readFloat();
			_EX_tubetyp = ldis.readShort();
			_EX_dastyp = ldis.readInt();
			_EX_num_dcnk = ldis.readShort();
			_EX_dcn_len = ldis.readShort();
			_EX_dcn_density = ldis.readShort();
			_EX_dcn_stepsize = ldis.readShort();
			_EX_dcn_shiftcnt = ldis.readShort();
			_EX_magstrength = ldis.readInt();
			readString(ldis, _EX_patid, 13);
			readString(ldis, _EX_patname, 25);
			_EX_patage = ldis.readShort();
			_EX_patian = ldis.readShort();
			_EX_patsex = ldis.readShort();
			_EX_patweight = ldis.readInt();
			_EX_trauma = ldis.readShort();
//			for(int ii=0; ii<61; ii++) {
//				_EX_hist[ii] = (byte)ldis.readUnsignedByte();
//				if( _EX_hist[ii] == '\014') _EX_hist[ii] = '\000';
//			}
			readString(ldis, _EX_hist, 61);
			readString(ldis, _EX_reqnum, 13);
			_EX_ex_datetime = ldis.readInt();
			readString(ldis, _EX_refphy, 33);
			readString(ldis, _EX_diagrad, 33);
			readString(ldis, _EX_op, 4);
			readString(ldis, _EX_ex_desc, 23);

			readString(ldis, _EX_ex_typ, 3);

			_EX_ex_format = ldis.readShort();

			ldis.readDouble();
			readString(ldis, _EX_ex_sysid, 10);
			_EX_ex_lastmod = ldis.readInt();
			_EX_protocolflag = ldis.readShort();
			readString(ldis, _EX_ex_alloc_key, 14);
			_EX_ex_delta_cnt = ldis.readInt();
			readString(ldis, _EX_ex_verscre, 2);
			readString(ldis, _EX_ex_verscur, 2);
			_EX_ex_checksum = ldis.readInt();
			_EX_ex_complete = ldis.readInt();
			_EX_ex_seriesct = ldis.readInt();
			_EX_ex_numarch = ldis.readInt();
			_EX_ex_numseries = ldis.readInt();

			ldis.skipBytes( 8 );

			//ldis.seek(_IH_img_p_exam + 384);
			_EX_ex_numunser = ldis.readInt();

			ldis.skipBytes( 8 );

			//ldis.seek(_IH_img_p_exam + 396);
			_EX_ex_toarchcnt = ldis.readInt();

			ldis.skipBytes( 8 );

			//ldis.seek(_IH_img_p_exam + 408);
			_EX_ex_prospcnt = ldis.readInt();

			ldis.skipBytes( 8 );

			//ldis.seek(_IH_img_p_exam + 420);
			_EX_ex_modelnum = ldis.readInt();

			//ldis.seek(_IH_img_p_exam + 424);
			_EX_ex_modelcnt = ldis.readInt();

			ldis.skipBytes( 8 );

			//ldis.seek(_IH_img_p_exam + 436);
			_EX_ex_stat = ldis.readShort();

			//ldis.seek(_IH_img_p_exam + 438);
			readString(ldis, _EX_uniq_sys_id, 16);

			//ldis.seek(_IH_img_p_exam + 454);
			readString(ldis, _EX_service_id, 16);

			//ldis.seek(_IH_img_p_exam + 470);
			readString(ldis, _EX_mobile_loc, 4);

			//ldis.seek(_IH_img_p_exam + 474);
			readString(ldis, _EX_ex_padding, 550);

			bytes_read += (474+550);
			ldis.skipBytes( _IH_img_p_series - bytes_read );
			bytes_read  = _IH_img_p_series;

			//-----------------------------------------------------------
			//
			//                SERIES SECTION
			//

			//ldis.seek(_IH_img_p_series + 0);
			readString(ldis, _SE_se_suid, 4);

			//ldis.seek(_IH_img_p_series + 4);
			_SE_se_uniq = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 6);
			readString(ldis, _SE_se_diskid, 1);

			ldis.skipBytes(1); 

			//ldis.seek(_IH_img_p_series + 8);
			_SE_se_exno = ldis.readUnsignedShort();

			//ldis.seek(_IH_img_p_series + 10);
			_SE_se_no = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 12);
			_SE_se_datetime = ldis.readInt();

			//ldis.seek(_IH_img_p_series + 16);
			_SE_se_actual_dt = ldis.readInt();

			//ldis.seek(_IH_img_p_series + 20);
			readString(ldis, _SE_se_desc, 30);

			//ldis.seek(_IH_img_p_series + 50);
			readString(ldis, _SE_pr_sysid, 9);

			//ldis.seek(_IH_img_p_series + 59);
			readString(ldis, _SE_pansysid, 9);

			//ldis.seek(_IH_img_p_series + 68);
			_SE_se_typ = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 70);
			_SE_se_source = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 72);
			_SE_se_plane = ldis.readShort();

			ldis.skipBytes(2); 

			//ldis.seek(_IH_img_p_series + 76);
			_SE_position = ldis.readInt();

			//ldis.seek(_IH_img_p_series + 80);
			_SE_entry = ldis.readInt();

			//ldis.seek(_IH_img_p_series + 84);
			readString(ldis, _SE_anref, 4);

			//ldis.seek(_IH_img_p_series + 88);
			_SE_lmhor = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 92);
			readString(ldis, _SE_prtcl, 26);

			//ldis.seek(_IH_img_p_series + 118);
			_SE_se_contrast = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 120);
			readString(ldis, _SE_start_ras, 1);

			ldis.skipBytes(1); 

			//ldis.seek(_IH_img_p_series + 122);
			_SE_start_loc = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 126);
			readString(ldis, _SE_end_ras, 1);

			ldis.skipBytes(1); 

			//ldis.seek(_IH_img_p_series + 128);
			_SE_end_loc = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 132);
			_SE_se_pseq = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 134);
			_SE_se_sortorder = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 136);
			_SE_se_lndmrkcnt = ldis.readInt();

			//ldis.seek(_IH_img_p_series + 140);
			_SE_se_nacq = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 142);
			_SE_xbasest = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 144);
			_SE_xbaseend = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 146);
			_SE_xenhst = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 148);
			_SE_xenhend = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 150);
			_SE_se_lastmod = ldis.readInt();

			//ldis.seek(_IH_img_p_series + 154);
			readString(ldis, _SE_se_alloc_key, 14);

			//ldis.seek(_IH_img_p_series + 168);
			_SE_se_delta_cnt = ldis.readInt();

			//ldis.seek(_IH_img_p_series + 172);
			readString(ldis, _SE_se_verscre, 2);

			//ldis.seek(_IH_img_p_series + 174);
			readString(ldis, _SE_se_verscur, 2);

			//ldis.seek(_IH_img_p_series + 176);
			_SE_se_pds_a = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 180);
			_SE_se_pds_c = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 184);
			_SE_se_pds_u = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 188);
			_SE_se_checksum = ldis.readInt();

			//ldis.seek(_IH_img_p_series + 192);
			_SE_se_complete = ldis.readInt();

			//ldis.seek(_IH_img_p_series + 196);
			_SE_se_numarch = ldis.readInt();

			//ldis.seek(_IH_img_p_series + 200);
			_SE_se_imagect = ldis.readInt();

			//ldis.seek(_IH_img_p_series + 204);
			_SE_se_numimages = ldis.readInt();

			ldis.skipBytes(8); 

			//ldis.seek(_IH_img_p_series + 216);
			_SE_se_numunimg = ldis.readInt();

			ldis.skipBytes(8); 

			//ldis.seek(_IH_img_p_series + 228);
			_SE_se_toarchcnt = ldis.readInt();

			ldis.skipBytes(8); 

			//ldis.seek(_IH_img_p_series + 240);
			_SE_echo1_alpha = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 244);
			_SE_echo1_beta = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 248);
			_SE_echo1_window = ldis.readUnsignedShort();

			//ldis.seek(_IH_img_p_series + 250);
			_SE_echo1_level = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 252);
			_SE_echo2_alpha = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 256);
			_SE_echo2_beta = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 260);
			_SE_echo2_window = ldis.readUnsignedShort();

			//ldis.seek(_IH_img_p_series + 262);
			_SE_echo2_level = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 264);
			_SE_echo3_alpha = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 268);
			_SE_echo3_beta = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 272);
			_SE_echo3_window = ldis.readUnsignedShort();

			//ldis.seek(_IH_img_p_series + 274);
			_SE_echo3_level = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 276);
			_SE_echo4_alpha = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 280);
			_SE_echo4_beta = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 284);
			_SE_echo4_window = ldis.readUnsignedShort();

			//ldis.seek(_IH_img_p_series + 286);
			_SE_echo4_level = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 288);
			_SE_echo5_alpha = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 292);
			_SE_echo5_beta = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 296);
			_SE_echo5_window = ldis.readUnsignedShort();

			//ldis.seek(_IH_img_p_series + 298);
			_SE_echo5_level = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 300);
			_SE_echo6_alpha = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 304);
			_SE_echo6_beta = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 308);
			_SE_echo6_window = ldis.readUnsignedShort();

			//ldis.seek(_IH_img_p_series + 310);
			_SE_echo6_level = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 312);
			_SE_echo7_alpha = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 316);
			_SE_echo7_beta = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 320);
			_SE_echo7_window = ldis.readUnsignedShort();

			//ldis.seek(_IH_img_p_series + 322);
			_SE_echo7_level = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 324);
			_SE_echo8_alpha = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 328);
			_SE_echo8_beta = ldis.readFloat();

			//ldis.seek(_IH_img_p_series + 332);
			_SE_echo8_window = ldis.readUnsignedShort();

			//ldis.seek(_IH_img_p_series + 334);
			_SE_echo8_level = ldis.readShort();

			//ldis.seek(_IH_img_p_series + 336);
			readString(ldis, _SE_se_padding, 684);

			//ldis.seek(_IH_img_p_image + 0);
			readString(ldis, _MR_im_suid, 4);

			//ldis.seek(_IH_img_p_image + 4);
			_MR_im_uniq = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 6);
			readString(ldis, _MR_im_diskid, 2);

			//ldis.seek(_IH_img_p_image + 8);
			_MR_im_exno = ldis.readUnsignedShort();

			//ldis.seek(_IH_img_p_image + 10);
			_MR_im_seno = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 12);
			_MR_im_no = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 14);
			_MR_im_datetime = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 18);
			_MR_im_actual_dt = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 22);
			_MR_sctime = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 26);
			_MR_slthick = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 30);
			_MR_imatrix_X = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 32);
			_MR_imatrix_Y = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 34);
			_MR_dfov = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 38);
			_MR_dfov_rect = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 42);
			_MR_dim_X = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 46);
			_MR_dim_Y = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 50);
			_MR_pixsize_X = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 54);
			_MR_pixsize_Y = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 58);
			readString(ldis, _MR_pdid, 14);

			//ldis.seek(_IH_img_p_image + 72);
			readString(ldis, _MR_contrastIV, 17);

			//ldis.seek(_IH_img_p_image + 89);
			readString(ldis, _MR_contrastOral, 17);

			//ldis.seek(_IH_img_p_image + 106);
			_MR_contmode = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 108);
			_MR_serrx = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 110);
			_MR_imgrx = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 112);
			_MR_screenformat = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 114);
			_MR_plane = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 116);
			_MR_scanspacing = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 120);
			_MR_im_compress = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 122);
			_MR_im_scouttype = ldis.readShort();


			//ldis.seek(_IH_img_p_image + 124);
			_MR_loc_ras = (char)ldis.readByte();

			ldis.skipBytes(1); 

			//ldis.seek(_IH_img_p_image + 126);
			_MR_loc = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 130);
			_MR_ctr_R = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 134);
			_MR_ctr_A = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 138);
			_MR_ctr_S = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 142);
			_MR_norm_R = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 146);
			_MR_norm_A = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 150);
			_MR_norm_S = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 154);
			_MR_tlhc_R = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 158);
			_MR_tlhc_A = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 162);
			_MR_tlhc_S = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 166);
			_MR_trhc_R = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 170);
			_MR_trhc_A = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 174);
			_MR_trhc_S = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 178);
			_MR_brhc_R = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 182);
			_MR_brhc_A = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 186);
			_MR_brhc_S = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 190);
			readString(ldis, _MR_forimgrev, 4);

			//ldis.seek(_IH_img_p_image + 194);
			_MR_tr = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 198);
			_MR_ti = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 202);
			_MR_te = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 206);
			_MR_te2 = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 210);
			_MR_numecho = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 212);
			_MR_echonum = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 214);
			_MR_tbldlta = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 218);
			_MR_nex = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 222);
			_MR_contig = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 224);
			_MR_hrtrate = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 226);
			_MR_tdel = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 230);
			_MR_saravg = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 234);
			_MR_sarpeak = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 238);
			_MR_monsar = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 240);
			_MR_trgwindow = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 242);
			_MR_reptime = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 246);
			_MR_imgpcyc = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 248);
			_MR_xmtgain = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 250);
			_MR_rcvgain1 = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 252);
			_MR_rcvgain2 = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 254);
			_MR_mr_flip = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 256);
			_MR_mindat = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 260);
			_MR_cphase = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 262);
			_MR_swappf = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 264);
			_MR_pauseint = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 266);
			_MR_pausetime = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 270);
			_MR_obplane = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 274);
			_MR_slocfov = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 278);
			_MR_xmtfreq = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 282);
			_MR_autoxmtfreq = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 286);
			_MR_autoxmtgain = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 288);
			_MR_prescan_r1 = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 290);
			_MR_prescan_r2 = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 292);
			_MR_user_bitmap = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 296);
			_MR_cenfreq = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 298);
			_MR_imode = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 300);
			_MR_iopt = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 304);
			_MR_pseq = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 306);
			_MR_pseqmode = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 308);
			readString(ldis, _MR_psdname, 34);

			//ldis.seek(_IH_img_p_image + 342);
			_MR_psd_datetime = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 346);
			readString(ldis, _MR_psd_iname, 14);

			//ldis.seek(_IH_img_p_image + 360);
			_MR_ctyp = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 362);
			readString(ldis, _MR_cname, 18);

			//ldis.seek(_IH_img_p_image + 380);
			_MR_surfctyp = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 382);
			_MR_surfcext = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 384);
			_MR_rawrunnum = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 388);
			_MR_cal_fldstr = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 392);
			_MR_supp_tech = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 394);
			_MR_vbw = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 398);
			_MR_slquant = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 400);
			_MR_gpre = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 402);
			_MR_intr_del = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 406);
			_MR_user0 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 410);
			_MR_user1 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 414);
			_MR_user2 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 418);
			_MR_user3 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 422);
			_MR_user4 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 426);
			_MR_user5 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 430);
			_MR_user6 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 434);
			_MR_user7 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 438);
			_MR_user8 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 442);
			_MR_user9 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 446);
			_MR_user10 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 450);
			_MR_user11 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 454);
			_MR_user12 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 458);
			_MR_user13 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 462);
			_MR_user14 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 466);
			_MR_user15 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 470);
			_MR_user16 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 474);
			_MR_user17 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 478);
			_MR_user18 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 482);
			_MR_user19 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 486);
			_MR_user20 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 490);
			_MR_user21 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 494);
			_MR_user22 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 498);
			_MR_user23 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 502);
			_MR_user24 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 506);
			readString(ldis, _MR_im_alloc_key, 14);

			//ldis.seek(_IH_img_p_image + 520);
			_MR_im_lastmod = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 524);
			readString(ldis, _MR_im_verscre, 2);

			//ldis.seek(_IH_img_p_image + 526);
			readString(ldis, _MR_im_verscur, 2);

			//ldis.seek(_IH_img_p_image + 528);
			_MR_im_pds_a = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 532);
			_MR_im_pds_c = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 536);
			_MR_im_pds_u = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 540);
			_MR_im_checksum = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 544);
			_MR_im_archived = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 548);
			_MR_im_complete = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 552);
			_MR_satbits = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 554);
			_MR_scic = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 556);
			_MR_satxloc1 = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 558);
			_MR_satxloc2 = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 560);
			_MR_satyloc1 = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 562);
			_MR_satyloc2 = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 564);
			_MR_satzloc1 = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 566);
			_MR_satzloc2 = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 568);
			_MR_satxthick = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 570);
			_MR_satythick = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 572);
			_MR_satzthick = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 574);
			_MR_flax = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 576);
			_MR_venc = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 578);
			_MR_thk_disclmr = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 580);
			_MR_ps_flag = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 582);
			_MR_ps_status = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 584);
			_MR_image_type = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 586);
			_MR_vas_collapse = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 588);
			_MR_user23n = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 592);
			_MR_user24n = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 596);
			_MR_proj_alg = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 598);
			readString(ldis, _MR_proj_name, 14);

			//ldis.seek(_IH_img_p_image + 612);
			_MR_x_axis_rot = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 616);
			_MR_y_axis_rot = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 620);
			_MR_z_axis_rot = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 624);
			_MR_thresh_min1 = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 628);
			_MR_thresh_max1 = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 632);
			_MR_thresh_min2 = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 636);
			_MR_thresh_max2 = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 640);
			_MR_echo_trn_len = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 642);
			_MR_frac_echo = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 644);
			_MR_prep_pulse = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 646);
			_MR_cphasenum = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 648);
			_MR_var_echo = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 650);
			readString(ldis, _MR_ref_img, 1);

			//ldis.seek(_IH_img_p_image + 651);
			readString(ldis, _MR_sum_img, 1);

			//ldis.seek(_IH_img_p_image + 652);
			_MR_img_window = ldis.readUnsignedShort();

			//ldis.seek(_IH_img_p_image + 654);
			_MR_img_level = ldis.readShort();

			//ldis.seek(_IH_img_p_image + 656);
			_MR_slop_int_1 = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 660);
			_MR_slop_int_2 = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 664);
			_MR_slop_int_3 = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 668);
			_MR_slop_int_4 = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 672);
			_MR_slop_int_5 = ldis.readInt();

			//ldis.seek(_IH_img_p_image + 676);
			_MR_slop_float_1 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 680);
			_MR_slop_float_2 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 684);
			_MR_slop_float_3 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 688);
			_MR_slop_float_4 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 692);
			_MR_slop_float_5 = ldis.readFloat();

			//ldis.seek(_IH_img_p_image + 696);
			readString(ldis, _MR_slop_str_1, 16);

			//ldis.seek(_IH_img_p_image + 712);
			readString(ldis, _MR_slop_str_2, 16);

			//ldis.seek(_IH_img_p_image + 728);
			readString(ldis, _MR_mr_padding, 294);
			
			ldis.close();

		} 
		catch( IOException e ) {
			System.out.println("Genesis: Could not read in the header.");
            System.exit(0);
		}

	}

	/** 
	 *   Read the header from the specified file. 
	 *
	 */
	public void readHeader(String filename) 
	{
		ImageInputStream ldis = null;
		int bytes_read = 0;

		ldis = AppKit.openRead(filename);
        if( ldis == null ) {
            System.out.println("BFF: Could not open " + filename + " to read.");
            System.exit(0);
        }
		ldis.setByteOrder(ByteOrder.BIG_ENDIAN);

		readHeader(ldis);
	}

	/**
	 *  Reads the whole file speicifed by the filename.
	 *
	 */
	public void read(String filename) 
	{
		ImageInputStream ldis = null;
		int bytes_read = 0;

		ldis = AppKit.openRead(filename);

        if( ldis == null ) {
            System.out.println("BFF: Could not open " + filename + " to read.");
            System.exit(0);
        }

		/* It should really always be this. */
		ldis.setByteOrder(endian);

		try {
			readHeader(ldis);

			ldis = AppKit.openRead(filename);

			if (ldis == null) {
				System.out.println("Could not find the file.");
            	System.exit(0);
			}

			/* It should really always be this. */
			ldis.setByteOrder(ByteOrder.BIG_ENDIAN);
		
			//
			//  poor man's seek.
			//
			ldis.skipBytes(7904);

			data = new MCVolume((int)_IH_img_width, (int)_IH_img_height, 1, 1);
			data.read(ldis, 0, 0, datatype );

			ldis.close();
		} 
		catch( IOException e ) {
			System.out.println("Could not read the file.");
            System.exit(0);
		}
	}

	public int get_IH_img_magic() { return _IH_img_magic; };

	public int get_IH_img_hdr_length() { return _IH_img_hdr_length; };

	public int get_IH_img_width() { return _IH_img_width; };

	public int get_IH_img_height() { return _IH_img_height; };

	public int get_IH_img_depth() { return _IH_img_depth; };

	public int get_IH_img_compress() { return _IH_img_compress; };

	public int get_IH_img_dwindow() { return _IH_img_dwindow; };

	public int get_IH_img_dlevel() { return _IH_img_dlevel; };

	public int get_IH_img_bgshade() { return _IH_img_bgshade; };

	public int get_IH_img_ovrflow() { return _IH_img_ovrflow; };

	public int get_IH_img_undflow() { return _IH_img_undflow; };

	public int get_IH_img_top_offset() { return _IH_img_top_offset; };

	public int get_IH_img_bot_offset() { return _IH_img_bot_offset; };

	public int get_IH_img_version() { return _IH_img_version; };

	public int get_IH_img_checksum() { return _IH_img_checksum; };

	public int get_IH_img_p_id() { return _IH_img_p_id; };

	public int get_IH_img_l_id() { return _IH_img_l_id; };

	public int get_IH_img_p_unpack() { return _IH_img_p_unpack; };

	public int get_IH_img_l_unpack() { return _IH_img_l_unpack; };

	public int get_IH_img_p_compress() { return _IH_img_p_compress; };

	public int get_IH_img_l_compress() { return _IH_img_l_compress; };

	public int get_IH_img_p_histo() { return _IH_img_p_histo; };

	public int get_IH_img_l_histo() { return _IH_img_l_histo; };

	public int get_IH_img_p_text() { return _IH_img_p_text; };

	public int get_IH_img_l_text() { return _IH_img_l_text; };

	public int get_IH_img_p_graphics() { return _IH_img_p_graphics; };

	public int get_IH_img_l_graphics() { return _IH_img_l_graphics; };

	public int get_IH_img_p_dbHdr() { return _IH_img_p_dbHdr; };

	public int get_IH_img_l_dbHdr() { return _IH_img_l_dbHdr; };

	public int get_IH_img_levelOffset() { return _IH_img_levelOffset; };

	public int get_IH_img_p_user() { return _IH_img_p_user; };

	public int get_IH_img_l_user() { return _IH_img_l_user; };

	public int get_IH_img_p_suite() { return _IH_img_p_suite; };

	public int get_IH_img_l_suite() { return _IH_img_l_suite; };

	public int get_IH_img_p_exam() { return _IH_img_p_exam; };

	public int get_IH_img_l_exam() { return _IH_img_l_exam; };

	public int get_IH_img_p_series() { return _IH_img_p_series; };

	public int get_IH_img_l_series() { return _IH_img_l_series; };

	public int get_IH_img_p_image() { return _IH_img_p_image; };

	public int get_IH_img_l_image() { return _IH_img_l_image; };

	public int get_HS_hs_version() { return _HS_hs_version; };

	public float get_HS_hs_sd() { return _HS_hs_sd; };

	public int get_HS_hs_mean() { return _HS_hs_mean; };

	public int get_HS_hs_min() { return _HS_hs_min; };

	public int get_HS_hs_max() { return _HS_hs_max; };

	public int get_HS_hs_first() { return _HS_hs_first; };

	public int get_HS_hs_region() { return _HS_hs_region; };

	public int get_HS_hs_length() { return _HS_hs_length; };

	public String get_SU_su_id() { return new String(_SU_su_id); };

	public int get_SU_su_uniq() { return _SU_su_uniq; };

	public String get_SU_su_diskid() { return new String(_SU_su_diskid); };

	public String get_SU_prodid() { return new String(_SU_prodid); };

	public String get_SU_su_verscre() { return new String(_SU_su_verscre); };

	public String get_SU_su_verscur() { return new String(_SU_su_verscur); };

	public int get_SU_su_checksum() { return _SU_su_checksum; };

	public String get_SU_su_padding() { return new String(_SU_su_padding); };

	public String get_EX_ex_suid() { return new String(_EX_ex_suid); };

	public int get_EX_ex_uniq() { return _EX_ex_uniq; };

	public char get_EX_ex_diskid() { return _EX_ex_diskid; };

	public int get_EX_ex_no() { return _EX_ex_no; };

	public String get_EX_hospname() { return new String(_EX_hospname); };

	public int get_EX_detect() { return _EX_detect; };

	public int get_EX_numcells() { return _EX_numcells; };

	public float get_EX_zerocell() { return _EX_zerocell; };

	public float get_EX_cellspace() { return _EX_cellspace; };

	public float get_EX_srctodet() { return _EX_srctodet; };

	public float get_EX_srctoiso() { return _EX_srctoiso; };

	public int get_EX_tubetyp() { return _EX_tubetyp; };

	public int get_EX_dastyp() { return _EX_dastyp; };

	public int get_EX_num_dcnk() { return _EX_num_dcnk; };

	public int get_EX_dcn_len() { return _EX_dcn_len; };

	public int get_EX_dcn_density() { return _EX_dcn_density; };

	public int get_EX_dcn_stepsize() { return _EX_dcn_stepsize; };

	public int get_EX_dcn_shiftcnt() { return _EX_dcn_shiftcnt; };

	public int get_EX_magstrength() { return _EX_magstrength; };

	public String get_EX_patid() { return new String(_EX_patid); };

	public String get_EX_patname() { return new String(_EX_patname); };

	public int get_EX_patage() { return _EX_patage; };

	public int get_EX_patian() { return _EX_patian; };

	public int get_EX_patsex() { return _EX_patsex; };

	public int get_EX_patweight() { return _EX_patweight; };

	public int get_EX_trauma() { return _EX_trauma; };

	public String get_EX_hist() { return new String(_EX_hist); };

	public String get_EX_reqnum() { return new String(_EX_reqnum); };

	public int get_EX_ex_datetime() { return _EX_ex_datetime; };

	public String get_EX_refphy() { return new String(_EX_refphy); };

	public String get_EX_diagrad() { return new String(_EX_diagrad); };

	public String get_EX_op() { return new String(_EX_op); };

	public String get_EX_ex_desc() { return new String(_EX_ex_desc); };

	public String get_EX_ex_typ() { return new String(_EX_ex_typ); };

	public int get_EX_ex_format() { return _EX_ex_format; };

	public String get_EX_ex_sysid() { return new String(_EX_ex_sysid); };

	public int get_EX_ex_lastmod() { return _EX_ex_lastmod; };

	public int get_EX_protocolflag() { return _EX_protocolflag; };

	public String get_EX_ex_alloc_key() { return new String(_EX_ex_alloc_key); };

	public int get_EX_ex_delta_cnt() { return _EX_ex_delta_cnt; };

	public String get_EX_ex_verscre() { return new String(_EX_ex_verscre); };

	public String get_EX_ex_verscur() { return new String(_EX_ex_verscur); };

	public int get_EX_ex_checksum() { return _EX_ex_checksum; };

	public int get_EX_ex_complete() { return _EX_ex_complete; };

	public int get_EX_ex_seriesct() { return _EX_ex_seriesct; };

	public int get_EX_ex_numarch() { return _EX_ex_numarch; };

	public int get_EX_ex_numseries() { return _EX_ex_numseries; };

	public int get_EX_ex_numunser() { return _EX_ex_numunser; };

	public int get_EX_ex_toarchcnt() { return _EX_ex_toarchcnt; };

	public int get_EX_ex_prospcnt() { return _EX_ex_prospcnt; };

	public int get_EX_ex_modelnum() { return _EX_ex_modelnum; };

	public int get_EX_ex_modelcnt() { return _EX_ex_modelcnt; };

	public int get_EX_ex_stat() { return _EX_ex_stat; };

	public String get_EX_uniq_sys_id() { return new String(_EX_uniq_sys_id); };

	public String get_EX_service_id() { return new String(_EX_service_id); };

	public String get_EX_mobile_loc() { return new String(_EX_mobile_loc); };

	public String get_EX_ex_padding() { return new String(_EX_ex_padding); };

	public String get_SE_se_suid() { return new String(_SE_se_suid); };

	public int get_SE_se_uniq() { return _SE_se_uniq; };

	public String get_SE_se_diskid() { return new String(_SE_se_diskid); };

	public int get_SE_se_exno() { return _SE_se_exno; };

	public int get_SE_se_no() { return _SE_se_no; };

	public int get_SE_se_datetime() { return _SE_se_datetime; };

	public int get_SE_se_actual_dt() { return _SE_se_actual_dt; };

	public String get_SE_se_desc() { return new String(_SE_se_desc); };

	public String get_SE_pr_sysid() { return new String(_SE_pr_sysid); };

	public String get_SE_pansysid() { return new String(_SE_pansysid); };

	public int get_SE_se_typ() { return _SE_se_typ; };

	public int get_SE_se_source() { return _SE_se_source; };

	public int get_SE_se_plane() { return _SE_se_plane; };

	public int get_SE_position() { return _SE_position; };

	public int get_SE_entry() { return _SE_entry; };

	public String get_SE_anref() { return new String(_SE_anref); };

	public float get_SE_lmhor() { return _SE_lmhor; };

	public String get_SE_prtcl() { return new String(_SE_prtcl); };

	public int get_SE_se_contrast() { return _SE_se_contrast; };

	public String get_SE_start_ras() { return new String(_SE_start_ras); };

	public float get_SE_start_loc() { return _SE_start_loc; };

	public String get_SE_end_ras() { return new String(_SE_end_ras); };

	public float get_SE_end_loc() { return _SE_end_loc; };

	public int get_SE_se_pseq() { return _SE_se_pseq; };

	public int get_SE_se_sortorder() { return _SE_se_sortorder; };

	public int get_SE_se_lndmrkcnt() { return _SE_se_lndmrkcnt; };

	public int get_SE_se_nacq() { return _SE_se_nacq; };

	public int get_SE_xbasest() { return _SE_xbasest; };

	public int get_SE_xbaseend() { return _SE_xbaseend; };

	public int get_SE_xenhst() { return _SE_xenhst; };

	public int get_SE_xenhend() { return _SE_xenhend; };

	public int get_SE_se_lastmod() { return _SE_se_lastmod; };

	public String get_SE_se_alloc_key() { return new String(_SE_se_alloc_key); };

	public int get_SE_se_delta_cnt() { return _SE_se_delta_cnt; };

	public String get_SE_se_verscre() { return new String(_SE_se_verscre); };

	public String get_SE_se_verscur() { return new String(_SE_se_verscur); };

	public float get_SE_se_pds_a() { return _SE_se_pds_a; };

	public float get_SE_se_pds_c() { return _SE_se_pds_c; };

	public float get_SE_se_pds_u() { return _SE_se_pds_u; };

	public int get_SE_se_checksum() { return _SE_se_checksum; };

	public int get_SE_se_complete() { return _SE_se_complete; };

	public int get_SE_se_numarch() { return _SE_se_numarch; };

	public int get_SE_se_imagect() { return _SE_se_imagect; };

	public int get_SE_se_numimages() { return _SE_se_numimages; };

	public int get_SE_se_numunimg() { return _SE_se_numunimg; };

	public int get_SE_se_toarchcnt() { return _SE_se_toarchcnt; };

	public float get_SE_echo1_alpha() { return _SE_echo1_alpha; };

	public float get_SE_echo1_beta() { return _SE_echo1_beta; };

	public int get_SE_echo1_window() { return _SE_echo1_window; };

	public int get_SE_echo1_level() { return _SE_echo1_level; };

	public float get_SE_echo2_alpha() { return _SE_echo2_alpha; };

	public float get_SE_echo2_beta() { return _SE_echo2_beta; };

	public int get_SE_echo2_window() { return _SE_echo2_window; };

	public int get_SE_echo2_level() { return _SE_echo2_level; };

	public float get_SE_echo3_alpha() { return _SE_echo3_alpha; };

	public float get_SE_echo3_beta() { return _SE_echo3_beta; };

	public int get_SE_echo3_window() { return _SE_echo3_window; };

	public int get_SE_echo3_level() { return _SE_echo3_level; };

	public float get_SE_echo4_alpha() { return _SE_echo4_alpha; };

	public float get_SE_echo4_beta() { return _SE_echo4_beta; };

	public int get_SE_echo4_window() { return _SE_echo4_window; };

	public int get_SE_echo4_level() { return _SE_echo4_level; };

	public float get_SE_echo5_alpha() { return _SE_echo5_alpha; };

	public float get_SE_echo5_beta() { return _SE_echo5_beta; };

	public int get_SE_echo5_window() { return _SE_echo5_window; };

	public int get_SE_echo5_level() { return _SE_echo5_level; };

	public float get_SE_echo6_alpha() { return _SE_echo6_alpha; };

	public float get_SE_echo6_beta() { return _SE_echo6_beta; };

	public int get_SE_echo6_window() { return _SE_echo6_window; };

	public int get_SE_echo6_level() { return _SE_echo6_level; };

	public float get_SE_echo7_alpha() { return _SE_echo7_alpha; };

	public float get_SE_echo7_beta() { return _SE_echo7_beta; };

	public int get_SE_echo7_window() { return _SE_echo7_window; };

	public int get_SE_echo7_level() { return _SE_echo7_level; };

	public float get_SE_echo8_alpha() { return _SE_echo8_alpha; };

	public float get_SE_echo8_beta() { return _SE_echo8_beta; };

	public int get_SE_echo8_window() { return _SE_echo8_window; };

	public int get_SE_echo8_level() { return _SE_echo8_level; };

	public String get_SE_se_padding() { return new String(_SE_se_padding); };

	public String get_MR_im_suid() { return new String(_MR_im_suid); };

	public int get_MR_im_uniq() { return _MR_im_uniq; };

	public String get_MR_im_diskid() { return new String(_MR_im_diskid); };

	public int get_MR_im_exno() { return _MR_im_exno; };

	public int get_MR_im_seno() { return _MR_im_seno; };

	public int get_MR_im_no() { return _MR_im_no; };

	public int get_MR_im_datetime() { return _MR_im_datetime; };

	public int get_MR_im_actual_dt() { return _MR_im_actual_dt; };

	public float get_MR_sctime() { return _MR_sctime; };

	public float get_MR_slthick() { return _MR_slthick; };

	public int get_MR_imatrix_X() { return _MR_imatrix_X; };

	public int get_MR_imatrix_Y() { return _MR_imatrix_Y; };

	public float get_MR_dfov() { return _MR_dfov; };

	public float get_MR_dfov_rect() { return _MR_dfov_rect; };

	public float get_MR_dim_X() { return _MR_dim_X; };

	public float get_MR_dim_Y() { return _MR_dim_Y; };

	public float get_MR_pixsize_X() { return _MR_pixsize_X; };

	public float get_MR_pixsize_Y() { return _MR_pixsize_Y; };

	public String get_MR_pdid() { return new String(_MR_pdid); };

	public String get_MR_contrastIV() { return new String(_MR_contrastIV); };

	public String get_MR_contrastOral() { return new String(_MR_contrastOral); };

	public int get_MR_contmode() { return _MR_contmode; };

	public int get_MR_serrx() { return _MR_serrx; };

	public int get_MR_imgrx() { return _MR_imgrx; };

	public int get_MR_screenformat() { return _MR_screenformat; };

	public int get_MR_plane() { return _MR_plane; };

	public float get_MR_scanspacing() { return _MR_scanspacing; };

	public int get_MR_im_compress() { return _MR_im_compress; };

	public int get_MR_im_scouttype() { return _MR_im_scouttype; };

	public char get_MR_loc_ras() { return _MR_loc_ras; };

	public float get_MR_loc() { return _MR_loc; };

	public float get_MR_ctr_R() { return _MR_ctr_R; };

	public float get_MR_ctr_A() { return _MR_ctr_A; };

	public float get_MR_ctr_S() { return _MR_ctr_S; };

	public float get_MR_norm_R() { return _MR_norm_R; };

	public float get_MR_norm_A() { return _MR_norm_A; };

	public float get_MR_norm_S() { return _MR_norm_S; };

	public float get_MR_tlhc_R() { return _MR_tlhc_R; };

	public float get_MR_tlhc_A() { return _MR_tlhc_A; };

	public float get_MR_tlhc_S() { return _MR_tlhc_S; };

	public float get_MR_trhc_R() { return _MR_trhc_R; };

	public float get_MR_trhc_A() { return _MR_trhc_A; };

	public float get_MR_trhc_S() { return _MR_trhc_S; };

	public float get_MR_brhc_R() { return _MR_brhc_R; };

	public float get_MR_brhc_A() { return _MR_brhc_A; };

	public float get_MR_brhc_S() { return _MR_brhc_S; };

	public String get_MR_forimgrev() { return new String(_MR_forimgrev); };

	public int get_MR_tr() { return _MR_tr; };

	public int get_MR_ti() { return _MR_ti; };

	public int get_MR_te() { return _MR_te; };

	public int get_MR_te2() { return _MR_te2; };

	public int get_MR_numecho() { return _MR_numecho; };

	public int get_MR_echonum() { return _MR_echonum; };

	public float get_MR_tbldlta() { return _MR_tbldlta; };

	public float get_MR_nex() { return _MR_nex; };

	public int get_MR_contig() { return _MR_contig; };

	public int get_MR_hrtrate() { return _MR_hrtrate; };

	public int get_MR_tdel() { return _MR_tdel; };

	public float get_MR_saravg() { return _MR_saravg; };

	public float get_MR_sarpeak() { return _MR_sarpeak; };

	public int get_MR_monsar() { return _MR_monsar; };

	public int get_MR_trgwindow() { return _MR_trgwindow; };

	public float get_MR_reptime() { return _MR_reptime; };

	public int get_MR_imgpcyc() { return _MR_imgpcyc; };

	public int get_MR_xmtgain() { return _MR_xmtgain; };

	public int get_MR_rcvgain1() { return _MR_rcvgain1; };

	public int get_MR_rcvgain2() { return _MR_rcvgain2; };

	public int get_MR_mr_flip() { return _MR_mr_flip; };

	public int get_MR_mindat() { return _MR_mindat; };

	public int get_MR_cphase() { return _MR_cphase; };

	public int get_MR_swappf() { return _MR_swappf; };

	public int get_MR_pauseint() { return _MR_pauseint; };

	public float get_MR_pausetime() { return _MR_pausetime; };

	public int get_MR_obplane() { return _MR_obplane; };

	public int get_MR_slocfov() { return _MR_slocfov; };

	public int get_MR_xmtfreq() { return _MR_xmtfreq; };

	public int get_MR_autoxmtfreq() { return _MR_autoxmtfreq; };

	public int get_MR_autoxmtgain() { return _MR_autoxmtgain; };

	public int get_MR_prescan_r1() { return _MR_prescan_r1; };

	public int get_MR_prescan_r2() { return _MR_prescan_r2; };

	public int get_MR_user_bitmap() { return _MR_user_bitmap; };

	public int get_MR_cenfreq() { return _MR_cenfreq; };

	public int get_MR_imode() { return _MR_imode; };

	public int get_MR_iopt() { return _MR_iopt; };

	public int get_MR_pseq() { return _MR_pseq; };

	public int get_MR_pseqmode() { return _MR_pseqmode; };

	public String get_MR_psdname() { return new String(_MR_psdname); };

	public int get_MR_psd_datetime() { return _MR_psd_datetime; };

	public String get_MR_psd_iname() { return new String(_MR_psd_iname); };

	public int get_MR_ctyp() { return _MR_ctyp; };

	public String get_MR_cname() { return new String(_MR_cname); };

	public int get_MR_surfctyp() { return _MR_surfctyp; };

	public int get_MR_surfcext() { return _MR_surfcext; };

	public int get_MR_rawrunnum() { return _MR_rawrunnum; };

	public int get_MR_cal_fldstr() { return _MR_cal_fldstr; };

	public int get_MR_supp_tech() { return _MR_supp_tech; };

	public float get_MR_vbw() { return _MR_vbw; };

	public int get_MR_slquant() { return _MR_slquant; };

	public int get_MR_gpre() { return _MR_gpre; };

	public int get_MR_intr_del() { return _MR_intr_del; };

	public float get_MR_user0() { return _MR_user0; };

	public float get_MR_user1() { return _MR_user1; };

	public float get_MR_user2() { return _MR_user2; };

	public float get_MR_user3() { return _MR_user3; };

	public float get_MR_user4() { return _MR_user4; };

	public float get_MR_user5() { return _MR_user5; };

	public float get_MR_user6() { return _MR_user6; };

	public float get_MR_user7() { return _MR_user7; };

	public float get_MR_user8() { return _MR_user8; };

	public float get_MR_user9() { return _MR_user9; };

	public float get_MR_user10() { return _MR_user10; };

	public float get_MR_user11() { return _MR_user11; };

	public float get_MR_user12() { return _MR_user12; };

	public float get_MR_user13() { return _MR_user13; };

	public float get_MR_user14() { return _MR_user14; };

	public float get_MR_user15() { return _MR_user15; };

	public float get_MR_user16() { return _MR_user16; };

	public float get_MR_user17() { return _MR_user17; };

	public float get_MR_user18() { return _MR_user18; };

	public float get_MR_user19() { return _MR_user19; };

	public float get_MR_user20() { return _MR_user20; };

	public float get_MR_user21() { return _MR_user21; };

	public float get_MR_user22() { return _MR_user22; };

	public float get_MR_user23() { return _MR_user23; };

	public float get_MR_user24() { return _MR_user24; };

	public String get_MR_im_alloc_key() { return new String(_MR_im_alloc_key); };

	public int get_MR_im_lastmod() { return _MR_im_lastmod; };

	public String get_MR_im_verscre() { return new String(_MR_im_verscre); };

	public String get_MR_im_verscur() { return new String(_MR_im_verscur); };

	public int get_MR_im_pds_a() { return _MR_im_pds_a; };

	public int get_MR_im_pds_c() { return _MR_im_pds_c; };

	public int get_MR_im_pds_u() { return _MR_im_pds_u; };

	public int get_MR_im_checksum() { return _MR_im_checksum; };

	public int get_MR_im_archived() { return _MR_im_archived; };

	public int get_MR_im_complete() { return _MR_im_complete; };

	public int get_MR_satbits() { return _MR_satbits; };

	public int get_MR_scic() { return _MR_scic; };

	public int get_MR_satxloc1() { return _MR_satxloc1; };

	public int get_MR_satxloc2() { return _MR_satxloc2; };

	public int get_MR_satyloc1() { return _MR_satyloc1; };

	public int get_MR_satyloc2() { return _MR_satyloc2; };

	public int get_MR_satzloc1() { return _MR_satzloc1; };

	public int get_MR_satzloc2() { return _MR_satzloc2; };

	public int get_MR_satxthick() { return _MR_satxthick; };

	public int get_MR_satythick() { return _MR_satythick; };

	public int get_MR_satzthick() { return _MR_satzthick; };

	public int get_MR_flax() { return _MR_flax; };

	public int get_MR_venc() { return _MR_venc; };

	public int get_MR_thk_disclmr() { return _MR_thk_disclmr; };

	public int get_MR_ps_flag() { return _MR_ps_flag; };

	public int get_MR_ps_status() { return _MR_ps_status; };

	public int get_MR_image_type() { return _MR_image_type; };

	public int get_MR_vas_collapse() { return _MR_vas_collapse; };

	public float get_MR_user23n() { return _MR_user23n; };

	public float get_MR_user24n() { return _MR_user24n; };

	public int get_MR_proj_alg() { return _MR_proj_alg; };

	public String get_MR_proj_name() { return new String(_MR_proj_name); };

	public float get_MR_x_axis_rot() { return _MR_x_axis_rot; };

	public float get_MR_y_axis_rot() { return _MR_y_axis_rot; };

	public float get_MR_z_axis_rot() { return _MR_z_axis_rot; };

	public int get_MR_thresh_min1() { return _MR_thresh_min1; };

	public int get_MR_thresh_max1() { return _MR_thresh_max1; };

	public int get_MR_thresh_min2() { return _MR_thresh_min2; };

	public int get_MR_thresh_max2() { return _MR_thresh_max2; };

	public int get_MR_echo_trn_len() { return _MR_echo_trn_len; };

	public int get_MR_frac_echo() { return _MR_frac_echo; };

	public int get_MR_prep_pulse() { return _MR_prep_pulse; };

	public int get_MR_cphasenum() { return _MR_cphasenum; };

	public int get_MR_var_echo() { return _MR_var_echo; };

	public String get_MR_ref_img() { return new String(_MR_ref_img); };

	public String get_MR_sum_img() { return new String(_MR_sum_img); };

	public int get_MR_img_window() { return _MR_img_window; };

	public int get_MR_img_level() { return _MR_img_level; };

	public int get_MR_slop_int_1() { return _MR_slop_int_1; };

	public int get_MR_slop_int_2() { return _MR_slop_int_2; };

	public int get_MR_slop_int_3() { return _MR_slop_int_3; };

	public int get_MR_slop_int_4() { return _MR_slop_int_4; };

	public int get_MR_slop_int_5() { return _MR_slop_int_5; };

	public float get_MR_slop_float_1() { return _MR_slop_float_1; };

	public float get_MR_slop_float_2() { return _MR_slop_float_2; };

	public float get_MR_slop_float_3() { return _MR_slop_float_3; };

	public float get_MR_slop_float_4() { return _MR_slop_float_4; };

	public float get_MR_slop_float_5() { return _MR_slop_float_5; };

	public String get_MR_slop_str_1() { return new String(_MR_slop_str_1); };

	public String get_MR_slop_str_2() { return new String(_MR_slop_str_2); };

	public String get_MR_mr_padding() { return new String(_MR_mr_padding); };


	private void readString(ImageInputStream ldis, StringBuffer b, final int length) 
	{
		byte bb;

		try{
		for(int ii=0; ii< length; ii++)
		{
			bb = (byte)ldis.readUnsignedByte();
			if( bb != '\014' && bb != '\000' ) b.append((char)bb);
		}
		}
		catch(IOException e)
		{
			System.out.println("genesis: Could not read in file.");
			System.exit(-1);
		}
	}

	//  magic number 
	private int _IH_img_magic;

	//  a byte displacement to the <pixel data area> 
	private int _IH_img_hdr_length;

	//  width (pixels) of image 
	private int _IH_img_width;

	//  height (pixels) of image 
	private int _IH_img_height;

	//  depth (1, 8, 16, or 24 bits) of 
	private int _IH_img_depth;

	//  type of compression; see IC_* below 
	private int _IH_img_compress;

	//  default window setting 
	private int _IH_img_dwindow;

	//  default level setting 
	private int _IH_img_dlevel;

	//  background shade to use for non-image 
	private int _IH_img_bgshade;

	//  overflow value 
	private int _IH_img_ovrflow;

	//  underflow value 
	private int _IH_img_undflow;

	//  number of blank lines at image top 
	private int _IH_img_top_offset;

	//  number of blank lines at image bottom 
	private int _IH_img_bot_offset;

	// (SHORT) version of the header structure 
	private int _IH_img_version;

	//  (UNSIGNED SHORT) 16 bit end_around_carry sum of pixels 
	private int _IH_img_checksum;

	//  a byte disp to unique image identifier 
	private int _IH_img_p_id;

	//  byte length of unique image identifier 
	private int _IH_img_l_id;

	//  a byte disp to <unpack control> 
	private int _IH_img_p_unpack;

	//  byte length of <unpack control> 
	private int _IH_img_l_unpack;

	//  a byte disp to <compression control> 
	private int _IH_img_p_compress;

	//  byte length of <compression control> 
	private int _IH_img_l_compress;

	//  a byte disp to <histogram control> 
	private int _IH_img_p_histo;

	//  byte length of <histogram control> 
	private int _IH_img_l_histo;

	//  a byte disp to <text plane data> 
	private int _IH_img_p_text;

	//  byte length of <text plane data> 
	private int _IH_img_l_text;

	//  a byte disp to <graphics plane data> 
	private int _IH_img_p_graphics;

	//  byte length of <graphics plane data> 
	private int _IH_img_l_graphics;

	//  a byte disp to <data base header data> 
	private int _IH_img_p_dbHdr;

	//  byte length of <data base header data> 
	private int _IH_img_l_dbHdr;

	//  value to add to stored Pixel Data values 
	private int _IH_img_levelOffset;

	//  byte displacement to user defined data 
	private int _IH_img_p_user;

	//  byte length of user defined data 
	private int _IH_img_l_user;

	//  byte displacement to <suite> header data 
	private int _IH_img_p_suite;

	//  byte length of <suite> header data 
	private int _IH_img_l_suite;

	//  byte displacement to <exam> header data 
	private int _IH_img_p_exam;

	//  byte length of <exam> header data 
	private int _IH_img_l_exam;

	//  byte displacement to <series> header data 
	private int _IH_img_p_series;

	//  byte length of <series> header data 
	private int _IH_img_l_series;

	//  byte displacement to <image> header data 
	private int _IH_img_p_image;

	//  byte length of <image> header data 
	private int _IH_img_l_image;

	//  version of the histogram structure 
	private int _HS_hs_version;

	//  standard deviation of pixel data value 
	private float _HS_hs_sd;

	// (SHORT) rounded mean pixel data value 
	private int _HS_hs_mean;

	// (SHORT) minimum pixel data value 
	private int _HS_hs_min;

	// (SHORT) maximum pixel data value 
	private int _HS_hs_max;

	// (SHORT) first histogram entry used for <hs_sd> 
	private int _HS_hs_first;

	// (SHORT) region entries used for <hs_sd> 
	private int _HS_hs_region;

	// (SHORT) number of bins in the histogram area 
	private int _HS_hs_length;

	//  Suite ID 
	private byte[] _SU_su_id;

	// (SHORT) Make Unique Flag 
	private int _SU_su_uniq;

	//  Disk ID 
	private byte[] _SU_su_diskid;

	//  Product ID 
	private StringBuffer _SU_prodid;

	//  Original Version of Record 
	private byte[] _SU_su_verscre;

	//  Current Version of Record 
	private byte[] _SU_su_verscur;

	//  (UINT) Suite Record Checksum 
	private int _SU_su_checksum;

	//  Spare Space 
	private byte[] _SU_su_padding;

	//  Suite ID for this Exam 
	private StringBuffer _EX_ex_suid;

	// (SHORT) Make-Unique Flag 
	private int _EX_ex_uniq;

	//  Disk ID for this Exam 
	private char _EX_ex_diskid;

	//  (UNSIGNED SHORT) Exam Number 
	private int _EX_ex_no;

	//  Hospital Name 
	private StringBuffer _EX_hospname;

	// (SHORT) Detector Type 
	private int _EX_detect;

	//  Number of cells in det 
	private int _EX_numcells;

	//  Cell number at theta 
	private float _EX_zerocell;

	//  Cell spacing 
	private float _EX_cellspace;

	//  Distance from source to detector 
	private float _EX_srctodet;

	//  Distance from source to iso 
	private float _EX_srctoiso;

	// (SHORT) Tube type 
	private int _EX_tubetyp;

	//  DAS type 
	private int _EX_dastyp;

	// (SHORT) Number of Decon Kernals 
	private int _EX_num_dcnk;

	// (SHORT) Number of elements in a Decon Kernal 
	private int _EX_dcn_len;

	// (SHORT) Decon Kernal density 
	private int _EX_dcn_density;

	// (SHORT) Decon Kernal stepsize 
	private int _EX_dcn_stepsize;

	// (SHORT) Decon Kernal Shift Count 
	private int _EX_dcn_shiftcnt;

	//  Magnet strength (in gauss) 
	private int _EX_magstrength;

	//  Patient ID for this Exam 
	private StringBuffer _EX_patid;

	//  Patient Name 
	private StringBuffer _EX_patname;

	// (SHORT) Patient Age (years, months or days) 
	private int _EX_patage;

	// (SHORT) Patient Age Notation 
	private int _EX_patian;

	// (SHORT) Patient Sex 
	private int _EX_patsex;

	//  Patient Weight 
	private int _EX_patweight;

	// (SHORT) Trauma Flag 
	private int _EX_trauma;

	//  Patient History 
//	private byte[] _EX_hist;
	private StringBuffer _EX_hist;

	//  Requisition Number 
//	private byte[] _EX_reqnum;
	private StringBuffer _EX_reqnum;

	//  Exam date/time stamp 
	private int _EX_ex_datetime;

	//  Referring Physician 
//	private byte[] _EX_refphy;
	private StringBuffer _EX_refphy;

	//  Diagnostician/Radiologist 
//	private byte[] _EX_diagrad;
	private StringBuffer _EX_diagrad;

	//  Operator 
//	private byte[] _EX_op;
	private StringBuffer _EX_op;

	//  Exam Description 
//	private byte[] _EX_ex_desc;
	private StringBuffer _EX_ex_desc;

	//  Exam Type 
	private StringBuffer _EX_ex_typ;

	// (SHORT) Exam Format 
	private int _EX_ex_format;

	//  Creator Suite and Host 
	private StringBuffer _EX_ex_sysid;

	//  Date/Time of Last Change 
	private int _EX_ex_lastmod;

	// (SHORT) Non-Zero indicates Protocol Exam 
	private int _EX_protocolflag;

	//  Process that allocated this record 
//	private byte[] _EX_ex_alloc_key;
	private StringBuffer _EX_ex_alloc_key;

	//  Indicates number of updates to header 
	private int _EX_ex_delta_cnt;

	//  Version - Created 
//	private byte[] _EX_ex_verscre;
	private StringBuffer _EX_ex_verscre;

	//  Version - Now 
//	private byte[] _EX_ex_verscur;
	private StringBuffer _EX_ex_verscur;

	//  (UINT) Exam Record Checksum 
	private int _EX_ex_checksum;

	//  Exam Complete Flag 
	private int _EX_ex_complete;

	//  Last Series Number Used 
	private int _EX_ex_seriesct;

	//  Number of Series Archived 
	private int _EX_ex_numarch;

	//  Number of Series Existing 
	private int _EX_ex_numseries;

	//  Number of Unstored Series 
	private int _EX_ex_numunser;

	//  Number of Unarchived Series 
	private int _EX_ex_toarchcnt;

	//  Number of Prospective/Scout Series 
	private int _EX_ex_prospcnt;

	//  Last Model Number used 
	private int _EX_ex_modelnum;

	//  Number of ThreeD Models 
	private int _EX_ex_modelcnt;

	// (SHORT) Patient Status 
	private int _EX_ex_stat;

	//  Unique System ID 
//	private byte[] _EX_uniq_sys_id;
	private StringBuffer _EX_uniq_sys_id;

	//  Unique Service ID 
//	private byte[] _EX_service_id;
	private StringBuffer _EX_service_id;

	//  Mobile Location Number 
//	private byte[] _EX_mobile_loc;
	private StringBuffer _EX_mobile_loc;

	//  Spare Space 
//	private byte[] _EX_ex_padding;
	private StringBuffer _EX_ex_padding;

	//  Suite ID for this Series 
	private StringBuffer _SE_se_suid;

	// (SHORT) The Make-Unique Flag 
	private int _SE_se_uniq;

	//  Disk ID for this Series 
	private StringBuffer _SE_se_diskid;

	//  (UNSIGNED SHORT) Exam Number 
	private int _SE_se_exno;

	// (SHORT) Series Number 
	private int _SE_se_no;

	//  Allocation Series Data/Time stamp 
	private int _SE_se_datetime;

	//  Actual Series Data/Time stamp 
	private int _SE_se_actual_dt;

	//  Series Description 
	private StringBuffer _SE_se_desc;

	//  Primary Receiver Suite and Host 
	private StringBuffer _SE_pr_sysid;

	//  Archiver Suite and Host 
	private StringBuffer _SE_pansysid;

	// (SHORT) Series Type 
	private int _SE_se_typ;

	// (SHORT) Series from which prescribed 
	private int _SE_se_source;

	// (SHORT) Most-like Plane (for L/S) 
	private int _SE_se_plane;

	//  Patient Position 
	private int _SE_position;

	//  Patient Entry 
	private int _SE_entry;

	//  Anatomical reference 
	private StringBuffer _SE_anref;

	//  Horizontal Landmark 
	private float _SE_lmhor;

	//  Scan Protocol Name 
	private StringBuffer _SE_prtcl;

	// (SHORT) Non-zero if > 0 image used contrast(L/S) 
	private int _SE_se_contrast;

	//  RAS letter for first scan location (L/S) 
	private StringBuffer _SE_start_ras;

	//  First scan location (L/S) 
	private float _SE_start_loc;

	//  RAS letter for last scan location (L/S) 
	private StringBuffer _SE_end_ras;

	//  Last scan location (L/S) 
	private float _SE_end_loc;

	// (SHORT) Last Pulse Sequence Used (L/S) 
	private int _SE_se_pseq;

	// (SHORT) Image Sort Order (L/S) 
	private int _SE_se_sortorder;

	//  Landmark Counter 
	private int _SE_se_lndmrkcnt;

	// (SHORT) Number of Acquisitions 
	private int _SE_se_nacq;

	// (SHORT) Starting number for baselines 
	private int _SE_xbasest;

	// (SHORT) Ending number for baselines 
	private int _SE_xbaseend;

	// (SHORT) Starting number for enhanced scans 
	private int _SE_xenhst;

	// (SHORT) Ending number for enhanced scans 
	private int _SE_xenhend;

	//  Date/Time of Last Change 
	private int _SE_se_lastmod;

	//  Process that allocated this record 
	private StringBuffer _SE_se_alloc_key;

	//  Indicates number of updates to header 
	private int _SE_se_delta_cnt;

	//  Genesis Version - Created 
	private StringBuffer _SE_se_verscre;

	//  Genesis Version - Now 
	private StringBuffer _SE_se_verscur;

	//  PixelData size - as stored 
	private float _SE_se_pds_a;

	//  PixelData size - Compressed 
	private float _SE_se_pds_c;

	//  PixelData size - UnCompressed 
	private float _SE_se_pds_u;

	//  (UINT) Series Record checksum 
	private int _SE_se_checksum;

	//  Series Complete Flag 
	private int _SE_se_complete;

	//  Number of Images Archived 
	private int _SE_se_numarch;

	//  Last Image Number Used 
	private int _SE_se_imagect;

	//  Number of Images Existing 
	private int _SE_se_numimages;

	//  Number of Unstored Images 
	private int _SE_se_numunimg;

	//  Number of Unarchived Images 
	private int _SE_se_toarchcnt;

	//  Echo 1 Alpha Value 
	private float _SE_echo1_alpha;

	//  Echo 1 Beta Value 
	private float _SE_echo1_beta;

	//  (UNSIGNED SHORT) Echo 1 Window Value 
	private int _SE_echo1_window;

	// (SHORT) Echo 1 Level Value 
	private int _SE_echo1_level;

	//  Echo 2 Alpha Value 
	private float _SE_echo2_alpha;

	//  Echo 2 Beta Value 
	private float _SE_echo2_beta;

	//  (UNSIGNED SHORT) Echo 2 Window Value 
	private int _SE_echo2_window;

	// (SHORT) Echo 2 Level Value 
	private int _SE_echo2_level;

	//  Echo 3 Alpha Value 
	private float _SE_echo3_alpha;

	//  Echo 3 Beta Value 
	private float _SE_echo3_beta;

	//  (UNSIGNED SHORT) Echo 3 Window Value 
	private int _SE_echo3_window;

	// (SHORT) Echo 3 Level Value 
	private int _SE_echo3_level;

	//  Echo 4 Alpha Value 
	private float _SE_echo4_alpha;

	//  Echo 4 Beta Value 
	private float _SE_echo4_beta;

	//  (UNSIGNED SHORT) Echo 4 Window Value 
	private int _SE_echo4_window;

	// (SHORT) Echo 4 Level Value 
	private int _SE_echo4_level;

	//  Echo 5 Alpha Value 
	private float _SE_echo5_alpha;

	//  Echo 5 Beta Value 
	private float _SE_echo5_beta;

	//  (UNSIGNED SHORT) Echo 5 Window Value 
	private int _SE_echo5_window;

	// (SHORT) Echo 5 Level Value 
	private int _SE_echo5_level;

	//  Echo 6 Alpha Value 
	private float _SE_echo6_alpha;

	//  Echo 6 Beta Value 
	private float _SE_echo6_beta;

	//  (UNSIGNED SHORT) Echo 6 Window Value 
	private int _SE_echo6_window;

	// (SHORT) Echo 6 Level Value 
	private int _SE_echo6_level;

	//  Echo 7 Alpha Value 
	private float _SE_echo7_alpha;

	//  Echo 7 Beta Value 
	private float _SE_echo7_beta;

	//  (UNSIGNED SHORT) Echo 7 Window Value 
	private int _SE_echo7_window;

	// (SHORT) Echo 7 Level Value 
	private int _SE_echo7_level;

	//  Echo 8 Alpha Value 
	private float _SE_echo8_alpha;

	//  Echo 8 Beta Value 
	private float _SE_echo8_beta;

	//  (UNSIGNED SHORT) Echo 8 Window Value 
	private int _SE_echo8_window;

	// (SHORT) Echo 8 Level Value 
	private int _SE_echo8_level;

	//  Spare Space 
	private StringBuffer _SE_se_padding;

	//  Suite id for this image 
	private StringBuffer _MR_im_suid;

	// (SHORT) The Make-Unique Flag 
	private int _MR_im_uniq;

	//  Disk ID for this Image 
	private StringBuffer _MR_im_diskid;

	//  (UNSIGNED SHORT) Exam number for this image 
	private int _MR_im_exno;

	// (SHORT) Series Number for this image 
	private int _MR_im_seno;

	// (SHORT) Image Number 
	private int _MR_im_no;

	//  Allocation Image date/time stamp 
	private int _MR_im_datetime;

	//  Actual Image date/time stamp 
	private int _MR_im_actual_dt;

	//  Duration of scan (secs) 
	private float _MR_sctime;

	//  Slice Thickness (mm) 
	private float _MR_slthick;

	// (SHORT) Image matrix size - X 
	private int _MR_imatrix_X;

	// (SHORT) Image matrix size - Y 
	private int _MR_imatrix_Y;

	//  Display field of view - X (mm) 
	private float _MR_dfov;

	//  Display field of view - Y (if different) 
	private float _MR_dfov_rect;

	//  Image dimension - X 
	private float _MR_dim_X;

	//  Image dimension - Y 
	private float _MR_dim_Y;

	//  Image pixel size - X 
	private float _MR_pixsize_X;

	//  Image pixel size - Y 
	private float _MR_pixsize_Y;

	//  Pixel Data ID 
	private StringBuffer _MR_pdid;

	//  IV Contrast Agent 
	private StringBuffer _MR_contrastIV;

	//  Oral Contrast Agent 
	private StringBuffer _MR_contrastOral;

	// (SHORT) Image Contrast Mode 
	private int _MR_contmode;

	// (SHORT) Series from which prescribed 
	private int _MR_serrx;

	// (SHORT) Image from which prescribed 
	private int _MR_imgrx;

	// (SHORT) Screen Format(8/16 bit) 
	private int _MR_screenformat;

	// (SHORT) Plane Type 
	private int _MR_plane;

	//  Spacing between scans (mm?) 
	private float _MR_scanspacing;

	// (SHORT) Image compression type for allocation 
	private int _MR_im_compress;

	// (SHORT) Scout Type (AP or lateral) 
	private int _MR_im_scouttype;

	//  RAS letter of image location 
	private char _MR_loc_ras;

	//  Image location 
	private float _MR_loc;

	//  Center R coord of plane image 
	private float _MR_ctr_R;

	//  Center A coord of plane image 
	private float _MR_ctr_A;

	//  Center S coord of plane image 
	private float _MR_ctr_S;

	//  Normal R coord 
	private float _MR_norm_R;

	//  Normal A coord 
	private float _MR_norm_A;

	//  Normal S coord 
	private float _MR_norm_S;

	//  R Coord of Top Left Hand Corner 
	private float _MR_tlhc_R;

	//  A Coord of Top Left Hand Corner 
	private float _MR_tlhc_A;

	//  S Coord of Top Left Hand Corner 
	private float _MR_tlhc_S;

	//  R Coord of Top Right Hand Corner 
	private float _MR_trhc_R;

	//  A Coord of Top Right Hand Corner 
	private float _MR_trhc_A;

	//  S Coord of Top Right Hand Corner 
	private float _MR_trhc_S;

	//  R Coord of Bottom Right Hand Corner 
	private float _MR_brhc_R;

	//  A Coord of Bottom Right Hand Corner 
	private float _MR_brhc_A;

	//  S Coord of Bottom Right Hand Corner 
	private float _MR_brhc_S;

	//  Foreign Image Revision 
	private StringBuffer _MR_forimgrev;

	//  Pulse repetition time(usec) 
	private int _MR_tr;

	//  Pulse inversion time(usec) 
	private int _MR_ti;

	//  Pulse echo time(usec) 
	private int _MR_te;

	//  Second echo echo (usec) 
	private int _MR_te2;

	// (SHORT) Number of echoes 
	private int _MR_numecho;

	// (SHORT) Echo Number 
	private int _MR_echonum;

	//  Table Delta 
	private float _MR_tbldlta;

	//  Number of Excitations 
	private float _MR_nex;

	// (SHORT) Continuous Slices Flag 
	private int _MR_contig;

	// (SHORT) Cardiac Heart Rate (bpm) 
	private int _MR_hrtrate;

	//  Delay time after trigger (msec) 
	private int _MR_tdel;

	//  Average SAR 
	private float _MR_saravg;

	//  Peak SAR 
	private float _MR_sarpeak;

	// (SHORT) Monitor SAR flag 
	private int _MR_monsar;

	// (SHORT) Trigger window (% of R-R interval) 
	private int _MR_trgwindow;

	//  Cardiac repetition time 
	private float _MR_reptime;

	// (SHORT) Images per cardiac cycle 
	private int _MR_imgpcyc;

	// (SHORT) Actual Transmit Gain (.1 db) 
	private int _MR_xmtgain;

	// (SHORT) Actual Receive Gain Analog (.1 db) 
	private int _MR_rcvgain1;

	// (SHORT) Actual Receive Gain Digital (.1 db) 
	private int _MR_rcvgain2;

	// (SHORT) Flip Angle for GRASS scans (deg.) 
	private int _MR_mr_flip;

	//  Minimum Delay after Trigger (uSec) 
	private int _MR_mindat;

	// (SHORT) Total Cardiac Phase prescribed 
	private int _MR_cphase;

	// (SHORT) Swap Phase/Frequency Axis 
	private int _MR_swappf;

	// (SHORT) Pause Interval (slices) 
	private int _MR_pauseint;

	//  Pause Time 
	private float _MR_pausetime;

	//  Oblique Plane 
	private int _MR_obplane;

	//  Slice Offsets on Freq axis 
	private int _MR_slocfov;

	//  Center Frequency (0.1 Hz) 
	private int _MR_xmtfreq;

	//  Auto Center Frequency (0.1 Hz) 
	private int _MR_autoxmtfreq;

	// (SHORT) Auto Transmit Gain (0.1 dB) 
	private int _MR_autoxmtgain;

	// (SHORT) PreScan R1 - Analog 
	private int _MR_prescan_r1;

	// (SHORT) PreScan R2 - Digital 
	private int _MR_prescan_r2;

	//  Bitmap defining user CVs 
	private int _MR_user_bitmap;

	// (SHORT) Center Frequency Method 
	private int _MR_cenfreq;

	// (SHORT) Imaging Mode 
	private int _MR_imode;

	//  Imaging Options 
	private int _MR_iopt;

	// (SHORT) Pulse Sequence 
	private int _MR_pseq;

	// (SHORT) Pulse Sequence Mode 
	private int _MR_pseqmode;

	//  Pulse Sequence Name 
	private StringBuffer _MR_psdname;

	//  PSD Creation Date and Time 
	private int _MR_psd_datetime;

	//  PSD name from inside PSD 
	private StringBuffer _MR_psd_iname;

	// (SHORT) Coil Type 
	private int _MR_ctyp;

	//  Coil Name 
	private StringBuffer _MR_cname;

	// (SHORT) Surface Coil Type 
	private int _MR_surfctyp;

	// (SHORT) Extremity Coil Flag 
	private int _MR_surfcext;

	//  RawData Run Number 
	private int _MR_rawrunnum;

	//  (UINT) Calibrated Field Strength (x10 uGauss) 
	private int _MR_cal_fldstr;

	// (SHORT) SAT fat/water/none 
	private int _MR_supp_tech;

	//  Variable Bandwidth (Hz) 
	private float _MR_vbw;

	// (SHORT) Number of slices in this scan group 
	private int _MR_slquant;

	// (SHORT) Graphically prescribed 
	private int _MR_gpre;

	//  Interimage/interloc delay (uSec) 
	private int _MR_intr_del;

	//  User Variable 0 
	private float _MR_user0;

	//  User Variable 1 
	private float _MR_user1;

	//  User Variable 2 
	private float _MR_user2;

	//  User Variable 3 
	private float _MR_user3;

	//  User Variable 4 
	private float _MR_user4;

	//  User Variable 5 
	private float _MR_user5;

	//  User Variable 6 
	private float _MR_user6;

	//  User Variable 7 
	private float _MR_user7;

	//  User Variable 8 
	private float _MR_user8;

	//  User Variable 9 
	private float _MR_user9;

	//  User Variable 10 
	private float _MR_user10;

	//  User Variable 11 
	private float _MR_user11;

	//  User Variable 12 
	private float _MR_user12;

	//  User Variable 13 
	private float _MR_user13;

	//  User Variable 14 
	private float _MR_user14;

	//  User Variable 15 
	private float _MR_user15;

	//  User Variable 16 
	private float _MR_user16;

	//  User Variable 17 
	private float _MR_user17;

	//  User Variable 18 
	private float _MR_user18;

	//  User Variable 19 
	private float _MR_user19;

	//  User Variable 20 
	private float _MR_user20;

	//  User Variable 21 
	private float _MR_user21;

	//  User Variable 22 
	private float _MR_user22;

	//  Projection Angle 
	private float _MR_user23;

	//  Concat Sat Type Flag 
	private float _MR_user24;

	//  
	private StringBuffer _MR_im_alloc_key;

	//  Date/Time of Last Change 
	private int _MR_im_lastmod;

	//  Genesis Version - Created 
	private StringBuffer _MR_im_verscre;

	//  Genesis Version - Now 
	private StringBuffer _MR_im_verscur;

	//  PixelData size - as stored 
	private int _MR_im_pds_a;

	//  PixelData size - Compressed 
	private int _MR_im_pds_c;

	//  PixelData size - UnCompressed 
	private int _MR_im_pds_u;

	//  (UINT) AcqRecon record checksum 
	private int _MR_im_checksum;

	//  Image Archive Flag 
	private int _MR_im_archived;

	//  Image Complete Flag 
	private int _MR_im_complete;

	// (SHORT) Bitmap of SAT selections 
	private int _MR_satbits;

	// (SHORT) Surface Coil Intensity Correction Flag 
	private int _MR_scic;

	// (SHORT) R-side SAT pulse loc rel to lndmrk 
	private int _MR_satxloc1;

	// (SHORT) L-side SAT pulse loc rel to lndmrk 
	private int _MR_satxloc2;

	// (SHORT) A-side SAT pulse loc rel to lndmrk 
	private int _MR_satyloc1;

	// (SHORT) P-side SAT pulse loc rel to lndmrk 
	private int _MR_satyloc2;

	// (SHORT) S-side SAT pulse loc rel to lndmrk 
	private int _MR_satzloc1;

	// (SHORT) I-side SAT pulse loc rel to lndmrk 
	private int _MR_satzloc2;

	// (SHORT) Thickness of X-axis SAT pulse 
	private int _MR_satxthick;

	// (SHORT) Thickness of Y-axis SAT pulse 
	private int _MR_satythick;

	// (SHORT) Thickness of Z-axis SAT pulse 
	private int _MR_satzthick;

	// (SHORT) Phase contrast flow axis 
	private int _MR_flax;

	// (SHORT) Phase contrast velocity encoding 
	private int _MR_venc;

	// (SHORT) Slice Thickness 
	private int _MR_thk_disclmr;

	// (SHORT) Auto/Manual Prescan flag 
	private int _MR_ps_flag;

	// (SHORT) Bitmap of changed values 
	private int _MR_ps_status;

	// (SHORT) Magnitude, Phase, Imaginary, or Real 
	private int _MR_image_type;

	// (SHORT) Collapse Image 
	private int _MR_vas_collapse;

	//  User Variable 23 
	private float _MR_user23n;

	//  User Variable 24 
	private float _MR_user24n;

	// (SHORT) Projection Algorithm 
	private int _MR_proj_alg;

	//  Projection Algorithm Name 
	private StringBuffer _MR_proj_name;

	//  X Axis Rotation 
	private float _MR_x_axis_rot;

	//  Y Axis Rotation 
	private float _MR_y_axis_rot;

	//  Z Axis Rotation 
	private float _MR_z_axis_rot;

	//  Lower Range of Pixels 1 
	private int _MR_thresh_min1;

	//  Upper Range of Pixels 1 
	private int _MR_thresh_max1;

	//  Lower Range of Pixels 2 
	private int _MR_thresh_min2;

	//  Upper Range of Pixels 2 
	private int _MR_thresh_max2;

	// (SHORT) Echo Train Length for Fast Spin Echo 
	private int _MR_echo_trn_len;

	// (SHORT) Fractional Echo - Effective TE Flag 
	private int _MR_frac_echo;

	// (SHORT) Preporatory Pulse Option 
	private int _MR_prep_pulse;

	// (SHORT) Cardiac Phase Number 
	private int _MR_cphasenum;

	// (SHORT) Variable Echo Flag 
	private int _MR_var_echo;

	//  Reference Image Field 
	private StringBuffer _MR_ref_img;

	//  Summary Image Field 
	private StringBuffer _MR_sum_img;

	//  (UNSIGNED SHORT) Window Value 
	private int _MR_img_window;

	// (SHORT) Level Value 
	private int _MR_img_level;

	//  Integer Slop Field 1 
	private int _MR_slop_int_1;

	//  Integer Slop Field 2 
	private int _MR_slop_int_2;

	//  Integer Slop Field 3 
	private int _MR_slop_int_3;

	//  Integer Slop Field 4 
	private int _MR_slop_int_4;

	//  Integer Slop Field 5 
	private int _MR_slop_int_5;

	//  Float Slop Field 1 
	private float _MR_slop_float_1;

	//  Float Slop Field 2 
	private float _MR_slop_float_2;

	//  Float Slop Field 3 
	private float _MR_slop_float_3;

	//  Float Slop Field 4 
	private float _MR_slop_float_4;

	//  Float Slop Field 5 
	private float _MR_slop_float_5;

	//  String Slop Field 1 
	private StringBuffer _MR_slop_str_1;

	//  String Slop Field 2 
	private StringBuffer _MR_slop_str_2;

	//  Spare Space 
	private StringBuffer _MR_mr_padding;

}
