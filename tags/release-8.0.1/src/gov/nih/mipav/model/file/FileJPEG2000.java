package gov.nih.mipav.model.file;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.Vector;

public class FileJPEG2000 extends FileBase {
	// The following license goes with all the JPEG 2000 decoding software
	// ported
	// from the C language open source OpenJPEG codec.
	/*
	 * The copyright in this software is being made available under the
	 * 2-clauses BSD License, included below. This software may be subject to
	 * other third party and contributor rights, including patent rights, and no
	 * such rights are granted under this license.
	 * 
	 * Copyright (c) 2002-2014, Universite catholique de Louvain (UCL), Belgium
	 * Copyright (c) 2002-2014, Professor Benoit Macq Copyright (c) 2003-2014,
	 * Antonin Descampe Copyright (c) 2003-2009, Francois-Olivier Devaux
	 * Copyright (c) 2005, Herve Drolon, FreeImage Team Copyright (c) 2002-2003,
	 * Yannick Verschueren Copyright (c) 2001-2003, David Janssens Copyright (c)
	 * 2011-2012, Centre National d'Etudes Spatiales (CNES), France Copyright
	 * (c) 2012, CS Systemes d'Information, France
	 * 
	 * All rights reserved.
	 * 
	 * Redistribution and use in source and binary forms, with or without
	 * modification, are permitted provided that the following conditions are
	 * met: 1. Redistributions of source code must retain the above copyright
	 * notice, this list of conditions and the following disclaimer. 2.
	 * Redistributions in binary form must reproduce the above copyright notice,
	 * this list of conditions and the following disclaimer in the documentation
	 * and/or other materials provided with the distribution.
	 * 
	 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS
	 * IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
	 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
	 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
	 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
	 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
	 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
	 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
	 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
	 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
	 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
	 */

	private static final int J2K_CFMT = 0;
	private static final int JP2_CFMT = 1;
	private static final int JPT_CFMT = 2;

	private static final int PXM_DFMT = 10;
	private static final int PGX_DFMT = 11;
	private static final int BMP_DFMT = 12;
	//private static final int YUV_DFMT = 13;
	private static final int TIF_DFMT = 14;
	private static final int RAW_DFMT = 15; /* MSB / Big Endian */
	private static final int TGA_DFMT = 16;
	private static final int PNG_DFMT = 17;
	private static final int RAWL_DFMT = 18; /* LSB / Little Endian */

	private static final int OPJ_PREC_MODE_CLIP = 0;

	private static final int OPJ_PREC_MODE_SCALE = 1;

	//private static final int OPJ_PREC_MODE_ORIGINAL_BIT_DEPTH = 2;
	private static final int OPJ_PATH_LEN = 4096;
	/** < Maximum allowed size for filenames */
	private static final int OPJ_J2K_DEFAULT_NB_SEGS = 10;

	// private static final int OPJ_J2K_STREAM_CHUNK_SIZE = 0x100000; /** 1 mega
	// by default */
	private static final int OPJ_J2K_MCC_DEFAULT_NB_RECORDS = 10;
	private static final int OPJ_J2K_MCT_DEFAULT_NB_RECORDS = 10;

	private static final int JPWL_EXPECTED_COMPONENTS = 3;
	/** < Expect this number of components, so you'll find better the first EPB */
	private static final int JPWL_MAXIMUM_TILES = 8192;
	/** < Expect this maximum number of tiles, to avoid some crashes */
	private static final int JPWL_MAX_NO_TILESPECS = 16;
	/** < Maximum number of tile parts expected by JPWL: increase at your will */
	private static final int JPWL_MAX_NO_PACKSPECS = 16;
	/** < Maximum number of packet parts expected by JPWL: increase at your will */
	private static final int OPJ_J2K_MAXRLVLS = 33;
	/** < Number of maximum resolution level authorized */
	private static final int OPJ_J2K_MAXBANDS = (3 * OPJ_J2K_MAXRLVLS - 2);
	/**
	 * < Number of maximum sub-band linked to number of resolution level
	 */
	private static final int OPJ_J2K_DEFAULT_HEADER_SIZE = 1000;
	private static final int OPJ_VALIDATION_SIZE = 10;

	private static final int OPJ_J2K_DEFAULT_CBLK_DATA_SIZE = 8192;

	//private static final int OPJ_J2K_BUILD_DECODER = 1;
	//private static final int OPJ_J2K_DECODING_VALIDATION = 2;

	private static final int J2K_CP_CSTY_PRT = 0x01;
	private static final int J2K_CP_CSTY_SOP = 0x02;
	private static final int J2K_CP_CSTY_EPH = 0x04;
	private static final int J2K_CCP_CSTY_PRT = 0x01;
	private static final int J2K_CCP_CBLKSTY_LAZY = 0x01;
	/** < Selective arithmetic coding bypass */
	private static final int J2K_CCP_CBLKSTY_RESET = 0x02;
	/** < Reset context probabilities on coding pass boundaries */
	private static final int J2K_CCP_CBLKSTY_TERMALL = 0x04;
	/** < Termination on each coding pass */
	private static final int J2K_CCP_CBLKSTY_VSC = 0x08;
	/** < Vertically stripe causal context */
	//private static final int J2K_CCP_CBLKSTY_PTERM = 0x10;
	/** < Predictable termination */
	private static final int J2K_CCP_CBLKSTY_SEGSYM = 0x20;
	/** < Segmentation symbols are used */
	private static final int J2K_CCP_QNTSTY_NOQNT = 0;
	private static final int J2K_CCP_QNTSTY_SIQNT = 1;
	//private static final int J2K_CCP_QNTSTY_SEQNT = 2;

	//private static final int J2K_STATE_NONE = 0x0000;
	/** < a SOC marker is expected */
	private static final int J2K_STATE_MHSOC = 0x0001;
	/** < a SOC marker is expected */
	private static final int J2K_STATE_MHSIZ = 0x0002;
	/** < a SIZ marker is expected */
	private static final int J2K_STATE_MH = 0x0004;
	/** < the decoding process is in the main header */
	private static final int J2K_STATE_TPHSOT = 0x0008;
	/** < the decoding process is in a tile part header and expects a SOT marker */
	private static final int J2K_STATE_TPH = 0x0010;
	/** < the decoding process is in a tile part header */
	//private static final int J2K_STATE_MT = 0x0020;
	/** < the EOC marker has just been read */
	private static final int J2K_STATE_NEOC = 0x0040;
	/**
	 * < the decoding process must not expect a EOC marker because the
	 * codestream is truncated
	 */

	private static final int J2K_STATE_EOC = 0x0100;
	/** < the decoding process has encountered the EOC marker */
	//private static final int J2K_STATE_ERR = 0x8000;
	/**
	 * < the decoding process has encountered an error (FIXME warning V1 =
	 * 0x0080)
	 */

	private static final int J2K_MS_SOC = 0xff4f;
	/** < SOC marker value */
	private static final int J2K_MS_SOT = 0xff90;
	/** < SOT marker value */
	private static final int J2K_MS_SOD = 0xff93;
	/** < SOD marker value */
	private static final int J2K_MS_EOC = 0xffd9;
	/** < EOC marker value */
	private static final int J2K_MS_SIZ = 0xff51;
	/** < SIZ marker value */
	private static final int J2K_MS_COD = 0xff52;
	/** < COD marker value */
	private static final int J2K_MS_COC = 0xff53;
	/** < COC marker value */
	private static final int J2K_MS_RGN = 0xff5e;
	/** < RGN marker value */
	private static final int J2K_MS_QCD = 0xff5c;
	/** < QCD marker value */
	private static final int J2K_MS_QCC = 0xff5d;
	/** < QCC marker value */
	private static final int J2K_MS_POC = 0xff5f;
	/** < POC marker value */
	private static final int J2K_MS_TLM = 0xff55;
	/** < TLM marker value */
	private static final int J2K_MS_PLM = 0xff57;
	/** < PLM marker value */
	private static final int J2K_MS_PLT = 0xff58;
	/** < PLT marker value */
	private static final int J2K_MS_PPM = 0xff60;
	/** < PPM marker value */
	private static final int J2K_MS_PPT = 0xff61;
	/** < PPT marker value */
	private static final int J2K_MS_SOP = 0xff91;
	/** < SOP marker value */
	//private static final int J2K_MS_EPH = 0xff92;
	/** < EPH marker value */
	private static final int J2K_MS_CRG = 0xff63;
	/** < CRG marker value */
	private static final int J2K_MS_COM = 0xff64;
	/** < COM marker value */
	private static final int J2K_MS_CBD = 0xff78;
	/** < CBD marker value */
	private static final int J2K_MS_MCC = 0xff75;
	/** < MCC marker value */
	private static final int J2K_MS_MCT = 0xff74;
	/** < MCT marker value */
	private static final int J2K_MS_MCO = 0xff77;
	/** < MCO marker value */

	private static final int J2K_MS_UNK = 0;
	/** < UNKNOWN marker value */

	// ifdef USE_JPWL
	private static final int J2K_MS_EPC = 0xff68;
	/** < EPC marker value (Part 11: JPEG 2000 for Wireless) */
	private static final int J2K_MS_EPB = 0xff66;
	/** < EPB marker value (Part 11: JPEG 2000 for Wireless) */
	private static final int J2K_MS_ESD = 0xff67;
	/** < ESD marker value (Part 11: JPEG 2000 for Wireless) */
	private static final int J2K_MS_RED = 0xff69;
	/** < RED marker value (Part 11: JPEG 2000 for Wireless) */
	// #endif /* USE_JPWL */
	// #ifdef USE_JPSEC
	private static final int J2K_MS_SEC = 0xff65;
	/** < SEC marker value (Part 8: Secure JPEG 2000) */
	private static final int J2K_MS_INSEC = 0xff94;
	/** < INSEC marker value (Part 8: Secure JPEG 2000) */
	// #endif /* USE_JPSEC */

	private static final int OPJ_J2K_READ_SOT = 1;
	private static final int OPJ_J2K_READ_COD = 2;
	private static final int OPJ_J2K_READ_COC = 3;
	private static final int OPJ_J2K_READ_RGN = 4;
	private static final int OPJ_J2K_READ_QCD = 5;
	private static final int OPJ_J2K_READ_QCC = 6;
	private static final int OPJ_J2K_READ_POC = 7;
	private static final int OPJ_J2K_READ_SIZ = 8;
	private static final int OPJ_J2K_READ_TLM = 9;
	private static final int OPJ_J2K_READ_PLM = 10;
	private static final int OPJ_J2K_READ_PLT = 11;
	private static final int OPJ_J2K_READ_PPM = 12;
	private static final int OPJ_J2K_READ_PPT = 13;
	private static final int OPJ_J2K_READ_CRG = 14;
	private static final int OPJ_J2K_READ_COM = 15;
	private static final int OPJ_J2K_READ_MCT = 16;
	private static final int OPJ_J2K_READ_CBD = 17;
	private static final int OPJ_J2K_READ_MCC = 18;
	private static final int OPJ_J2K_READ_MCO = 19;
	private static final int J2K_READ_EPC = 20;
	private static final int J2K_READ_EPB = 21;
	private static final int J2K_READ_ESD = 22;
	private static final int J2K_READ_RED = 23;
	private static final int J2K_READ_SEC = 24;
	private static final int J2K_READ_INSEC = 25;

	private static final int OPJ_DWT_GETGAIN_REAL = 0;;
	private static final int OPJ_DWT_GETGAIN = 1;

	private static final int MQC_NUMCTXS = 19;
	private static final int T1_TYPE_MQ = 0;
	/** < Normal coding using entropy coder */
	private static final int T1_TYPE_RAW = 1;
	/**
	 * < No encoding the information is store under raw format in codestream
	 * (mode switch RAW)
	 */

	private static final int T1_NUMCTXS_ZC = 9;
	private static final int T1_NUMCTXS_SC = 5;
	private static final int T1_NUMCTXS_MAG = 3;
	private static final int T1_NUMCTXS_AGG = 1;
	//private static final int T1_NUMCTXS_UNI = 1;

	private static final int T1_CTXNO_ZC = 0;
	private static final int T1_CTXNO_SC = (T1_CTXNO_ZC + T1_NUMCTXS_ZC);
	private static final int T1_CTXNO_MAG = (T1_CTXNO_SC + T1_NUMCTXS_SC);
	private static final int T1_CTXNO_AGG = (T1_CTXNO_MAG + T1_NUMCTXS_MAG);
	private static final int T1_CTXNO_UNI = (T1_CTXNO_AGG + T1_NUMCTXS_AGG);
	//private static final int T1_NUMCTXS = (T1_CTXNO_UNI + T1_NUMCTXS_UNI);

	private static final int T1_SIG_NE = 0x0001;
	/** < Context orientation : North-East direction */
	private static final int T1_SIG_SE = 0x0002;
	/** < Context orientation : South-East direction */
	private static final int T1_SIG_SW = 0x0004;
	/** < Context orientation : South-West direction */
	private static final int T1_SIG_NW = 0x0008;
	/** < Context orientation : North-West direction */
	private static final int T1_SIG_N = 0x0010;
	/** < Context orientation : North direction */
	private static final int T1_SIG_E = 0x0020;
	/** < Context orientation : East direction */
	private static final int T1_SIG_S = 0x0040;
	/** < Context orientation : South direction */
	private static final int T1_SIG_W = 0x0080;
	/** < Context orientation : West direction */
	private static final int T1_SIG_OTH = (T1_SIG_N | T1_SIG_NE | T1_SIG_E
			| T1_SIG_SE | T1_SIG_S | T1_SIG_SW | T1_SIG_W | T1_SIG_NW);
	private static final int T1_SIG_PRIM = (T1_SIG_N | T1_SIG_E | T1_SIG_S | T1_SIG_W);

	private static final int T1_SGN_N = 0x0100;
	private static final int T1_SGN_E = 0x0200;
	private static final int T1_SGN_S = 0x0400;
	private static final int T1_SGN_W = 0x0800;
	private static final int T1_SGN = (T1_SGN_N | T1_SGN_E | T1_SGN_S | T1_SGN_W);

	private static final int T1_SIG = 0x1000;
	private static final int T1_REFINE = 0x2000;
	private static final int T1_VISIT = 0x4000;

	private static final int OPJ_DWT_DECODE_1 = 1;

	private static final float opj_dwt_alpha = 1.586134342f; /* 12994 */
	private static final float opj_dwt_beta = 0.052980118f; /* 434 */
	private static final float opj_dwt_gamma = -0.882911075f; /* -7233 */
	private static final float opj_dwt_delta = -0.443506852f; /* -3633 */

	private static final float opj_K = 1.230174105f; /* 10078 */
	private static final float opj_c13318 = 1.625732422f;

	// Either inputImageDirectory or compressedFile must be specified.
	private String inputImageDirectory;

	// Enter 1 of these 3 letter
	// groups:<PBM|PGM|PPM|PNM|PAM|PGX|PNG|BMP|TIF|RAW|RAWL|TGA>
	// Required only if inputImageDirectory is used
	// Output format for decompressed images
	private String outputFormatExtension;

	// Required only if an inputImageDirectory is not specified.
	// Currently accepts J2K-files, JP2-Files, and JPT-files.
	// The file type is identified based on its suffix.
	private String compressedFile;

	// Required only if compressedFile is used. Currently accepts formats:
	// <PBM|PGM|PPM|PNM|PAM|PGX|PNG|BMP|TIF|RAW|RAWL|TGA>
	// Binary data is written to the file, not ascii.
	// If a PGX filename is given, there will be as many output
	// files as there are components: an indice starting from
	// 0 will then be appended to the output filename, just
	// before the pgx extension. If a PGM filename is given and
	// there is more than one component, only the first component
	// will be written to the file.
	private String decompressedFile;

	// The number of highest resolution levels to be discarded.
	// The image resolution is effectively divided by 2 to the
	// power of the number of discarded levels. The reduceFactor
	// is limited by the smallest total number of decomposition
	// levels among tiles
	// Optional field. Set to zero if not used.
	private int reduceFactor = 0;

	// Number of quality layers to decode
	// Set the maximum number of quality layers to decode
	// If there are less quality layers than the specified
	// number, all the quality layers are decoded.
	// Optional field. Set to Integer.MAX_VALUE if not used.
	private int qualityLayers = Integer.MAX_VALUE;

	// Create an index file indexName.Idx
	// Optional field
	private String indexName;

	// By default all the image is decoding
	private boolean decodingArea = false;

	// Decoding area coordinates
	private int x0;

	private int y0;

	private int x1;

	private int y1;

	// Optional. Set the tile index of the decoded tile.
	// Follow the JPEG2000 convention from left-up to right-bottom.
	// By default all tiles are decoded.
	private int tileIndex = -1;

	// Force the precision (bit depth) of components
	// There shall be at least 1 value. There is no
	// limit on the number of values. The last values are
	// ignored if there are too many values. If there are
	// less values than components, the last value is used
	// for the remaining components.
	// Optional
	private int component0Precision = -1;

	private int component1Precision = -1;

	private int component2Precision = -1;

	// OPJ_PREC_MODE_CLIP or OPJ_PREC_MODE_SCALE
	// OPJ_PREC_MODE_CLIP is the default
	// Values are clipped or scaled
	// Optional
	private int mode0 = OPJ_PREC_MODE_CLIP;
	private int mode1 = OPJ_PREC_MODE_CLIP;
	private int mode2 = OPJ_PREC_MODE_CLIP;

	// Optional. Force the output image colorspace to RGB
	private boolean forceRGB = false;

	// Optional. Downsampled components will be upsampled to image size
	private boolean upsample = false;

	// Optional Split output components to different files when writing to PNM.
	private boolean splitPNM = false;

	/**
	 * @file jpwl.h
	 * @brief The JPEG-2000 Part11 (JPWL) marker segments manager
	 * 
	 *        The functions in JPWL.C have for goal to read/write the markers
	 *        added by JPWL.
	 */

	/** @defgroup JPWL JPWL - JPEG-2000 Part11 (JPWL) codestream manager */
	/* @{ */

	// Optional
	private boolean useJPWL = false;

	// If true, activates the JPWL correction capability, fi the codestream
	// complies.
	// Optional
	private boolean JPWLCorrection = false;

	// The number of expected components in the codestream.
	// Search of the frist EPB rely upon this.
	// Default is JPWL_EXPECTED_COMPONENTS
	// Optional.
	private int JPWLNumComponents = JPWL_EXPECTED_COMPONENTS;

	// Maximum number of tiles
	// Optional
	private int JPWLMaximumTiles = JPWL_MAXIMUM_TILES;

	/**
	 * Assume a basic codestream structure, so you can resort better from
	 * uncorrected errors
	 */
	private boolean JPWLAssume = true;

	private boolean OPJ_DISABLE_TPSOT_FIX = false;

	private boolean MQC_PERF_OPT = false;

	private boolean useMJ2 = false;

	private boolean OPJ_HAVE_LIBLCMS1 = false;

	private boolean OPJ_HAVE_LIBLCMS2 = false;

	// Set Operations (SSE)
	// SSE 2.0 up to the currently latest version 4.2 can process four single
	// precision (32-bit) floating point numbers
	// or two double precision (64-bit) floating point numbers in vectorized
	// manner.
	// private boolean SSE = false;

	// A JPEG200 file is always big endian, storing and transferring the most
	// significant bytes first.
	private boolean endianess = FileBase.BIG_ENDIAN;

	// Original source requires either an inputImageDirectory or a
	// compressedFile.
	public FileJPEG2000(String inputImageDirectory,
			String outputFormatExtension, String compressedFile,
			String decompressedFile, int reduceFactor, int qualityLayers,
			String indexName, boolean decodingArea, int x0, int y0, int x1,
			int y1, int tileIndex, int component0Precision, int mode0,
			int component1Precision, int mode1, int component2Precision,
			int mode2, boolean forceRGB, boolean upsample, boolean splitPNM,
			boolean useJPWL, boolean JPWLCorrection, int JPWLNumComponents,
			int JPWLMaximumTiles, boolean JPWLAssume,
			boolean OPJ_DISABLE_TPSOT_FIX, boolean MQC_PERF_OPT,
			boolean useMJ2, boolean OPJ_HAVE_LIBLCMS1, boolean OPJ_HAVE_LIBLCMS2) {
		this.inputImageDirectory = inputImageDirectory;
		this.outputFormatExtension = outputFormatExtension;
		this.compressedFile = compressedFile;
		this.decompressedFile = decompressedFile;
		this.reduceFactor = reduceFactor;
		this.qualityLayers = qualityLayers;
		this.indexName = indexName;
		this.decodingArea = decodingArea;
		this.x0 = x0;
		this.y0 = y0;
		this.x1 = x1;
		this.y1 = y1;
		this.tileIndex = tileIndex;
		this.component0Precision = component0Precision;
		this.mode0 = mode0;
		this.component1Precision = component1Precision;
		this.mode1 = mode1;
		this.component2Precision = component2Precision;
		this.mode2 = mode2;
		this.forceRGB = forceRGB;
		this.upsample = upsample;
		this.splitPNM = splitPNM;
		this.useJPWL = useJPWL;
		this.JPWLCorrection = JPWLCorrection;
		this.JPWLNumComponents = JPWLNumComponents;
		this.JPWLMaximumTiles = JPWLMaximumTiles;
		this.JPWLAssume = JPWLAssume;
		this.OPJ_DISABLE_TPSOT_FIX = OPJ_DISABLE_TPSOT_FIX;
		this.MQC_PERF_OPT = MQC_PERF_OPT;
		this.useMJ2 = useMJ2;
		this.OPJ_HAVE_LIBLCMS1 = OPJ_HAVE_LIBLCMS1;
		this.OPJ_HAVE_LIBLCMS2 = OPJ_HAVE_LIBLCMS2;
	}
	
	public FileJPEG2000() {
		
	}
	
	public void selfTest() {
		inputImageDirectory = null;
		outputFormatExtension = null;
	    //compressedFile = "C:" + File.separator + "images" + File.separator+ "J2K" + File.separator + "Bretagne1_0.j2k";
		compressedFile = "C:" + File.separator + "images" + File.separator+ "J2K" + File.separator + "lenaj2k.j2k";
		decompressedFile = "C:" + File.separator + "images" + File.separator+ "J2K" + File.separator + "lena.pgm";
		//decompressedFile = "C:" + File.separator + "images" + File.separator+ "J2K" + File.separator + "Bretagne1_0.pgm";
		opj_decompress_main();
	}

	private byte lut_ctxno_zc[] = new byte[] { 0, 1, 1, 2, 1, 2, 2, 2, 1, 2, 2,
			2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5,
			6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7,
			7, 7, 7, 7, 7, 7, 7, 7, 7, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
			3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 7, 7, 7,
			7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
			7, 7, 7, 7, 7, 7, 7, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
			6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8,
			8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
			8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
			7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8,
			8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
			8, 8, 8, 0, 1, 1, 2, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 5, 6, 6,
			6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, 3, 3, 3,
			3, 3, 3, 3, 3, 3, 3, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
			7, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 8, 8, 8, 8, 8,
			8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
			7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 3,
			3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 7, 7, 7, 7, 7, 7, 7,
			7, 7, 7, 7, 7, 7, 7, 7, 7, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
			4, 4, 4, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
			7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8,
			8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
			7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 0, 1, 1, 2, 1,
			2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
			3, 3, 3, 3, 3, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7,
			7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 3, 3, 3, 3, 3, 3, 3,
			3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
			4, 4, 4, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
			7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 5, 6, 6, 6, 6, 6, 6, 6, 6,
			6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
			7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
			8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
			7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8,
			8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
			8, 8, 8, 8, 8, 8, 8, 8, 8, 0, 3, 3, 6, 3, 6, 6, 8, 3, 6, 6, 8, 6,
			8, 8, 8, 1, 4, 4, 7, 4, 7, 7, 8, 4, 7, 7, 8, 7, 8, 8, 8, 1, 4, 4,
			7, 4, 7, 7, 8, 4, 7, 7, 8, 7, 8, 8, 8, 2, 5, 5, 7, 5, 7, 7, 8, 5,
			7, 7, 8, 7, 8, 8, 8, 1, 4, 4, 7, 4, 7, 7, 8, 4, 7, 7, 8, 7, 8, 8,
			8, 2, 5, 5, 7, 5, 7, 7, 8, 5, 7, 7, 8, 7, 8, 8, 8, 2, 5, 5, 7, 5,
			7, 7, 8, 5, 7, 7, 8, 7, 8, 8, 8, 2, 5, 5, 7, 5, 7, 7, 8, 5, 7, 7,
			8, 7, 8, 8, 8, 1, 4, 4, 7, 4, 7, 7, 8, 4, 7, 7, 8, 7, 8, 8, 8, 2,
			5, 5, 7, 5, 7, 7, 8, 5, 7, 7, 8, 7, 8, 8, 8, 2, 5, 5, 7, 5, 7, 7,
			8, 5, 7, 7, 8, 7, 8, 8, 8, 2, 5, 5, 7, 5, 7, 7, 8, 5, 7, 7, 8, 7,
			8, 8, 8, 2, 5, 5, 7, 5, 7, 7, 8, 5, 7, 7, 8, 7, 8, 8, 8, 2, 5, 5,
			7, 5, 7, 7, 8, 5, 7, 7, 8, 7, 8, 8, 8, 2, 5, 5, 7, 5, 7, 7, 8, 5,
			7, 7, 8, 7, 8, 8, 8, 2, 5, 5, 7, 5, 7, 7, 8, 5, 7, 7, 8, 7, 8, 8, 8 };

	private byte lut_ctxno_sc[] = new byte[] { 0x9, 0xa, 0xc, 0xd, 0xa, 0xa,
			0xd, 0xd, 0xc, 0xd, 0xc, 0xd, 0xd, 0xd, 0xd, 0xd, 0x9, 0xa, 0xc,
			0xb, 0xa, 0x9, 0xd, 0xc, 0xc, 0xb, 0xc, 0xb, 0xd, 0xc, 0xd, 0xc,
			0x9, 0xa, 0xc, 0xb, 0xa, 0xa, 0xb, 0xb, 0xc, 0xd, 0x9, 0xa, 0xd,
			0xd, 0xa, 0xa, 0x9, 0xa, 0xc, 0xd, 0xa, 0x9, 0xb, 0xc, 0xc, 0xb,
			0x9, 0xa, 0xd, 0xc, 0xa, 0x9, 0x9, 0xa, 0xc, 0xd, 0xa, 0x9, 0xb,
			0xc, 0xc, 0xd, 0xc, 0xd, 0xb, 0xc, 0xb, 0xc, 0x9, 0xa, 0xc, 0xb,
			0xa, 0xa, 0xb, 0xb, 0xc, 0xb, 0xc, 0xb, 0xb, 0xb, 0xb, 0xb, 0x9,
			0xa, 0xc, 0xb, 0xa, 0x9, 0xd, 0xc, 0xc, 0xd, 0x9, 0xa, 0xb, 0xc,
			0xa, 0x9, 0x9, 0xa, 0xc, 0xd, 0xa, 0xa, 0xd, 0xd, 0xc, 0xb, 0x9,
			0xa, 0xb, 0xb, 0xa, 0xa, 0x9, 0xa, 0xc, 0xd, 0xa, 0xa, 0xd, 0xd,
			0xc, 0xb, 0x9, 0xa, 0xb, 0xb, 0xa, 0xa, 0x9, 0xa, 0xc, 0xb, 0xa,
			0x9, 0xd, 0xc, 0xc, 0xd, 0x9, 0xa, 0xb, 0xc, 0xa, 0x9, 0x9, 0xa,
			0xc, 0xb, 0xa, 0xa, 0xb, 0xb, 0xc, 0xb, 0xc, 0xb, 0xb, 0xb, 0xb,
			0xb, 0x9, 0xa, 0xc, 0xd, 0xa, 0x9, 0xb, 0xc, 0xc, 0xd, 0xc, 0xd,
			0xb, 0xc, 0xb, 0xc, 0x9, 0xa, 0xc, 0xd, 0xa, 0x9, 0xb, 0xc, 0xc,
			0xb, 0x9, 0xa, 0xd, 0xc, 0xa, 0x9, 0x9, 0xa, 0xc, 0xb, 0xa, 0xa,
			0xb, 0xb, 0xc, 0xd, 0x9, 0xa, 0xd, 0xd, 0xa, 0xa, 0x9, 0xa, 0xc,
			0xb, 0xa, 0x9, 0xd, 0xc, 0xc, 0xb, 0xc, 0xb, 0xd, 0xc, 0xd, 0xc,
			0x9, 0xa, 0xc, 0xd, 0xa, 0xa, 0xd, 0xd, 0xc, 0xd, 0xc, 0xd, 0xd,
			0xd, 0xd, 0xd };

	private byte lut_spb[] = new byte[] { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1,
			0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
			1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0,
			0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
			0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0,
			1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1,
			1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1,
			0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1,
			1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
			1, 1 };
	
	//private static final int OPJ_CLRSPC_UNKNOWN = -1; // not supported by the library
	//private static final int OPJ_CLRSPC_UNSPECIFIED = 0; // not specified in the codestream
	private static final int OPJ_CLRSPC_SRGB = 1; // sRGB 
	private static final int OPJ_CLRSPC_GRAY = 2; // grayscale
	private static final int OPJ_CLRSPC_SYCC = 3; // YUV
	private static final int OPJ_CLRSPC_EYCC = 4; // e-YCC
	private static final int OPJ_CLRSPC_CMYK = 5; // CMYK

	/**
	 * Supported codec
	 */
//	private enum OPJ_CODEC_FORMAT {
//		OPJ_CODEC_UNKNOWN, // < place-holder
//		OPJ_CODEC_J2K, // < JPEG-2000 codestream : read/write
//		OPJ_CODEC_JPT, // < JPT-stream (JPEG 2000, JPIP) : read only 
//		OPJ_CODEC_JP2, // < JP2 file format : read/write
//		OPJ_CODEC_JPP, // < JPP-stream (JPEG 2000, JPIP) : to be coded
//		OPJ_CODEC_JPX
		// < JPX file format (JPEG 2000 Part-2) : to be coded 
//	};
	
	// Progression order
	private static final int OPJ_PROG_UNKNOWN = -1; // < place-holder
	private static final int OPJ_LRCP = 0; // < layer-resolution-component-precinct order
	private static final int OPJ_RLCP = 1; // < resolution-layer-component-precinct order 
	private static final int OPJ_RPCL = 2; // < resolution-precinct-component-layer order
	private static final int OPJ_PCRL = 3; // < precinct-component-resolution-layer order 
	private static final int OPJ_CPRL = 4; // < component-precinct-resolution-layer order
	
	private static final int MCT_TYPE_INT16 = 0; // MCT data is stored as signed shorts
	private static final int MCT_TYPE_INT32 = 1; // MCT data is stored as signed integers
	private static final int MCT_TYPE_FLOAT = 2; // MCT data is stored as floats
	private static final int MCT_TYPE_DOUBLE = 3; // MCT data is stored as doubles

	// Type of MCT array
	//private static final int MCT_TYPE_DEPENDENCY = 0;
	//private static final int MCT_TYPE_DECORRELATION = 1;
	//private static final int MCT_TYPE_OFFSET = 2;

	private static final int MCT_ELEMENT_SIZE[] = new int[] { 2, 4, 4, 8 };

	class opj_dec_memory_marker_handler_t {
		/** marker value */
		int id;
		/** value of the state when the marker can appear */
		int states;
		/** action linked to the marker */
		int handler;

		opj_dec_memory_marker_handler_t(int id, int states, int handler) {
			this.id = id;
			this.states = states;
			this.handler = handler;
		}
	};

	opj_dec_memory_marker_handler_t j2k_memory_marker_handler_tab[] = new opj_dec_memory_marker_handler_t[] {
			new opj_dec_memory_marker_handler_t(J2K_MS_SOT, J2K_STATE_MH
					| J2K_STATE_TPHSOT, OPJ_J2K_READ_SOT),
			new opj_dec_memory_marker_handler_t(J2K_MS_COD, J2K_STATE_MH
					| J2K_STATE_TPH, OPJ_J2K_READ_COD),
			new opj_dec_memory_marker_handler_t(J2K_MS_COC, J2K_STATE_MH
					| J2K_STATE_TPH, OPJ_J2K_READ_COC),
			new opj_dec_memory_marker_handler_t(J2K_MS_RGN, J2K_STATE_MH
					| J2K_STATE_TPH, OPJ_J2K_READ_RGN),
			new opj_dec_memory_marker_handler_t(J2K_MS_QCD, J2K_STATE_MH
					| J2K_STATE_TPH, OPJ_J2K_READ_QCD),
			new opj_dec_memory_marker_handler_t(J2K_MS_QCC, J2K_STATE_MH
					| J2K_STATE_TPH, OPJ_J2K_READ_QCC),
			new opj_dec_memory_marker_handler_t(J2K_MS_POC, J2K_STATE_MH
					| J2K_STATE_TPH, OPJ_J2K_READ_POC),
			new opj_dec_memory_marker_handler_t(J2K_MS_SIZ, J2K_STATE_MHSIZ,
					OPJ_J2K_READ_SIZ),
			new opj_dec_memory_marker_handler_t(J2K_MS_TLM, J2K_STATE_MH,
					OPJ_J2K_READ_TLM),
			new opj_dec_memory_marker_handler_t(J2K_MS_PLM, J2K_STATE_MH,
					OPJ_J2K_READ_PLM),
			new opj_dec_memory_marker_handler_t(J2K_MS_PLT, J2K_STATE_TPH,
					OPJ_J2K_READ_PLT),
			new opj_dec_memory_marker_handler_t(J2K_MS_PPM, J2K_STATE_MH,
					OPJ_J2K_READ_PPM),
			new opj_dec_memory_marker_handler_t(J2K_MS_PPT, J2K_STATE_TPH,
					OPJ_J2K_READ_PPT),
			new opj_dec_memory_marker_handler_t(J2K_MS_SOP, 0, 0),
			new opj_dec_memory_marker_handler_t(J2K_MS_CRG, J2K_STATE_MH,
					OPJ_J2K_READ_CRG),
			new opj_dec_memory_marker_handler_t(J2K_MS_COM, J2K_STATE_MH
					| J2K_STATE_TPH, OPJ_J2K_READ_COM),
			new opj_dec_memory_marker_handler_t(J2K_MS_MCT, J2K_STATE_MH
					| J2K_STATE_TPH, OPJ_J2K_READ_MCT),
			new opj_dec_memory_marker_handler_t(J2K_MS_CBD, J2K_STATE_MH,
					OPJ_J2K_READ_CBD),
			new opj_dec_memory_marker_handler_t(J2K_MS_MCC, J2K_STATE_MH
					| J2K_STATE_TPH, OPJ_J2K_READ_MCC),
			new opj_dec_memory_marker_handler_t(J2K_MS_MCO, J2K_STATE_MH
					| J2K_STATE_TPH, OPJ_J2K_READ_MCO),
			// ifdef USE_JPWL
			// ifdef TODO_MS /* remove these functions which are not commpatible
			// with the v2 API */
			new opj_dec_memory_marker_handler_t(J2K_MS_EPC, J2K_STATE_MH
					| J2K_STATE_TPH, J2K_READ_EPC),
			new opj_dec_memory_marker_handler_t(J2K_MS_EPB, J2K_STATE_MH
					| J2K_STATE_TPH, J2K_READ_EPB),
			new opj_dec_memory_marker_handler_t(J2K_MS_ESD, J2K_STATE_MH
					| J2K_STATE_TPH, J2K_READ_ESD),
			new opj_dec_memory_marker_handler_t(J2K_MS_RED, J2K_STATE_MH
					| J2K_STATE_TPH, J2K_READ_RED),
			// endif
			// endif /* USE_JPWL */
			// ifdef USE_JPSEC
			new opj_dec_memory_marker_handler_t(J2K_MS_SEC, J2K_STATE_MH,
					J2K_READ_SEC),
			new opj_dec_memory_marker_handler_t(J2K_MS_INSEC, 0, J2K_READ_INSEC),
			// endif /* USE_JPSEC */
			new opj_dec_memory_marker_handler_t(J2K_MS_UNK, J2K_STATE_MH
					| J2K_STATE_TPH, 0) /* opj_j2k_read_unk is directly used */
	};

	class opj_precision {
		int prec;
		// OPJ_PREC_MODE_CLIP or OPJ_PREC_MODE_SCALE
		int mode;
	};

	/**
	 * Decompression parameters
	 * */
	class opj_dparameters_t {
		/**
		 * Set the number of highest resolution levels to be discarded. The
		 * image resolution is effectively divided by 2 to the power of the
		 * number of discarded levels. The reduce factor is limited by the
		 * smallest total number of decomposition levels among tiles. if != 0,
		 * then original dimension divided by 2^(reduce); if == 0 or not used,
		 * image is decoded to the full resolution
		 */
		int cp_reduce;
		/**
		 * Set the maximum number of quality layers to decode. If there are less
		 * quality layers than the specified number, all the quality layers are
		 * decoded. if != 0, then only the first "layer" layers are decoded; if
		 * == 0 or not used, all the quality layers are decoded
		 */
		int cp_layer;

		/** @name command line decoder parameters (not used inside the library) */
		/** input file name */
		char infile[] = new char[OPJ_PATH_LEN];
		/** output file name */
		char outfile[] = new char[OPJ_PATH_LEN];
		/** input file format 0: J2K, 1: JP2, 2: JPT */
		int decod_format;
		/** output file format 0: PGX, 1: PxM, 2: BMP */
		int cod_format;

		/** Decoding area left boundary */
		int DA_x0;
		/** Decoding area right boundary */
		int DA_x1;
		/** Decoding area up boundary */
		int DA_y0;
		/** Decoding area bottom boundary */
		int DA_y1;
		/** Verbose mode */
		boolean m_verbose;

		/** tile number ot the decoded tile */
		int tile_index;
		/** Nb of tile to decode */
		int nb_tile_to_decode;

		/* NOT YET USED IN THE V2 VERSION OF OPENJPEG */
		/** @name JPWL decoding parameters */
		/** activates the JPWL correction capabilities */
		boolean jpwl_correct;
		/** expected number of components */
		int jpwl_exp_comps;
		/** maximum number of tiles */
		int jpwl_max_tiles;

		int flags;

	};

	class opj_decompress_parameters {
		/** core library parameters */
		opj_dparameters_t core = new opj_dparameters_t();

		/** input file name */
		String infile;
		/** output file name */
		String outfile;
		/** input file format 0: J2K, 1: JP2, 2: JPT */
		int decod_format;
		/** output file format 0: PGX, 1: PxM, 2: BMP */
		int cod_format;
		/** index file name */
		String indexfilename;

		/** Decoding area left boundary */
		int DA_x0;
		/** Decoding area right boundary */
		int DA_x1;
		/** Decoding area up boundary */
		int DA_y0;
		/** Decoding area bottom boundary */
		int DA_y1;
		/** Verbose mode */
		boolean m_verbose;

		/** tile number ot the decoded tile */
		int tile_index;
		/** Nb of tile to decode */
		int nb_tile_to_decode;

		opj_precision[] precision;
		int nb_precision;

		/* force output colorspace to RGB */
		int force_rgb;
		/* upsample components according to their dx/dy values */
		int upsample;
		/* split output components to different files */
		int split_pnm;

		/* NOT YET USED IN THE V2 VERSION OF OPENJPEG */
		/** @name JPWL decoding parameters */
		/** activates the JPWL correction capabilities */
		boolean jpwl_correct;
		/** expected number of components */
		int jpwl_exp_comps;
		/** maximum number of tiles */
		int jpwl_max_tiles;
	};

	/**
	 * Defines a single image component
	 * */
	class opj_image_comp_t {
		/**
		 * XRsiz: horizontal separation of a sample of ith component with
		 * respect to the reference grid
		 */
		int dx;
		/**
		 * YRsiz: vertical separation of a sample of ith component with respect
		 * to the reference grid
		 */
		int dy;
		/** data width */
		int w;
		/** data height */
		int h;
		/** x component offset compared to the whole image */
		int x0;
		/** y component offset compared to the whole image */
		int y0;
		/** precision */
		int prec;
		/** image depth in bits */
		int bpp;
		/** signed (1) / unsigned (0) */
		int sgnd;
		/** number of decoded resolution */
		int resno_decoded;
		/**
		 * number of division by 2 of the out image compared to the original
		 * size of image
		 */
		int factor;
		/** image component data */
		int data[];
		/** alpha channel */
		short alpha;
	};

	/**
	 * Defines image data and characteristics
	 * */
	class opj_image_t {
		/**
		 * XOsiz: horizontal offset from the origin of the reference grid to the
		 * left side of the image area
		 */
		int x0;
		/**
		 * YOsiz: vertical offset from the origin of the reference grid to the
		 * top side of the image area
		 */
		int y0;
		/** Xsiz: width of the reference grid */
		int x1;
		/** Ysiz: height of the reference grid */
		int y1;
		/** number of components in the image */
		int numcomps;
		/** color space: sRGB, Greyscale or YUV */
		int color_space;
		/** image components */
		opj_image_comp_t comps[];
		/** 'restricted' ICC profile */
		char icc_profile_buf[];
		/** size of ICC profile */
		int icc_profile_len;
	};

	/**
	 * Component parameters structure used by the opj_image_create function
	 * */
	class opj_image_cmptparm_t {
		/**
		 * XRsiz: horizontal separation of a sample of ith component with
		 * respect to the reference grid
		 */
		int dx;
		/**
		 * YRsiz: vertical separation of a sample of ith component with respect
		 * to the reference grid
		 */
		int dy;
		/** data width */
		int w;
		/** data height */
		int h;
		/** x component offset compared to the whole image */
		int x0;
		/** y component offset compared to the whole image */
		int y0;
		/** precision */
		int prec;
		/** image depth in bits */
		int bpp;
		/** signed (1) / unsigned (0) */
		int sgnd;
	};

	/**
	 * Marker structure
	 * */
	class opj_marker_info_t {
		/** marker type */
		short type;
		/** position in codestream */
		long pos;
		/** length, marker val included */
		int len;
	};

	/**
	 * Index structure about a tile part
	 */
	class opj_tp_index_t {
		/** start position */
		long start_pos;
		/** end position of the header */
		long end_header;
		/** end position */
		long end_pos;

	};

	/**
	 * Index structure : Information concerning a packet inside tile
	 * */
	class opj_packet_info_t {
		/** packet start position (including SOP marker if it exists) */
		long start_pos;
		/** end of packet header position (including EPH marker if it exists) */
		long end_ph_pos;
		/** packet end position */
		long end_pos;
		/** packet distorsion */
		double disto;
	};

	/**
	 * Index structure about a tile
	 */
	class opj_tile_index_t {
		/** tile index */
		int tileno;

		/** number of tile parts */
		int nb_tps;
		/** current nb of tile part (allocated) */
		int current_nb_tps;
		/** current tile-part index */
		int current_tpsno;
		/** information concerning tile parts */
		opj_tp_index_t tp_index[];

		/* NOT USED FOR THE MOMENT IN THE V2 VERSION */
		/** number of markers */
		int marknum;
		/** list of markers */
		opj_marker_info_t marker[];
		/** actual size of markers array */
		int maxmarknum;

		/** packet number */
		int nb_packet;
		/** information concerning packets inside tile */
		opj_packet_info_t packet_index[];

	};

	/**
	 * Index structure of the codestream (FIXME should be expand and enhance)
	 */
	class opj_codestream_index_t {
		/** main header start position (SOC position) */
		long main_head_start;
		/** main header end position (first SOT position) */
		long main_head_end;

		/** codestream's size */
		long codestream_size;

		/* NOT USED FOR THE MOMENT IN THE V2 VERSION */
		/** number of markers */
		int marknum;
		/** list of markers */
		opj_marker_info_t marker[];
		/** actual size of markers array */
		int maxmarknum;

		/** */
		int nb_of_tiles;
		/** */
		opj_tile_index_t tile_index[]; /* FIXME not used for the moment */

	};

	class img_fol_t {
		/** The directory path of the folder containing input images */
		String imgdirpath;
		/** Output format */
		String out_format;
		/** Enable option */
		char set_imgdir;
		/** Enable Cod Format for output */
		char set_out_format;

	};

	class dircnt_t {
		String filename[];
	};

	/**
	 * Progression order changes
	 * 
	 */
	class opj_poc_t {
		/** Resolution num start, Component num start, given by POC */
		int resno0, compno0;
		/** Layer num end,Resolution num end, Component num end, given by POC */
		int layno1, resno1, compno1;
		/** Layer num start,Precinct num start, Precinct num end */
		int precno0, precno1;
		/** Layer num start,Precinct num start, Precinct num end */
		int layno0;
		/** Progression order enum */
		int prg1, prg;
		/** Progression order string - 5 characters */
		String progorder;
		/** Tile number */
		int tile;
		/** Start and end values for Tile width and height */
		int tx0, tx1, ty0, ty1;
		/** Start value, initialised in pi_initialise_encode */
		int layS, resS, compS, prcS;
		/** End value, initialised in pi_initialise_encode */
		int layE, resE, compE, prcE;
		/**
		 * Start and end values of Tile width and height, initialised in
		 * pi_initialise_encode
		 */
		int txS, txE, tyS, tyE, dx, dy;
		/** Temporary values for Tile parts, initialised in pi_create_encode */
		int lay_t, res_t, comp_t, prc_t, tx0_t, ty0_t;
	};

	/**
	 * A list of procedures.
	 */
	class opj_procedure_list_t {
		/**
		 * The number of validation procedures.
		 */
		int m_nb_procedures;
		/**
		 * The number of the array of validation procedures.
		 */
		int m_nb_max_procedures;
		/**
		 * The array of procedures.
		 */
		Vector<Integer> m_procedures;

	};

	/**
	 * Quantization stepsize
	 */
	class opj_stepsize_t {
		/** exponent */
		int expn;
		/** mantissa */
		int mant;
	};

	/**
	 * Tile-component coding parameters
	 */
	class opj_tccp_t {
		/** coding style */
		int csty;
		/** number of resolutions */
		int numresolutions;
		/** code-blocks width */
		int cblkw;
		/** code-blocks height */
		int cblkh;
		/** code-block coding style */
		int cblksty;
		/** discrete wavelet transform identifier */
		int qmfbid;
		/** quantisation style */
		int qntsty;
		/** stepsizes used for quantization */
		opj_stepsize_t stepsizes[] = new opj_stepsize_t[OPJ_J2K_MAXBANDS];
		/** number of guard bits */
		int numgbits;
		/** Region Of Interest shift */
		int roishift;
		/** precinct width */
		int prcw[] = new int[OPJ_J2K_MAXRLVLS];
		/** precinct height */
		int prch[] = new int[OPJ_J2K_MAXRLVLS];
		/** the dc_level_shift **/
		int m_dc_level_shift;
	};

	class opj_encoding_param_t {
		/**
		 * Maximum rate for each component. If == 0, component size limitation
		 * is not considered
		 */
		int m_max_comp_size;
		/** Position of tile part flag in progression order */
		int m_tp_pos;
		/** fixed layer */
		int m_matrice[];
		/** Flag determining tile part generation */
		byte m_tp_flag;
		/** allocation by rate/distortion */
		int m_disto_alloc = 1;
		/** allocation by fixed layer */
		int m_fixed_alloc = 1;
		/** add fixed_quality */
		int m_fixed_quality = 1;
		/** Enabling Tile part generation */
		int m_tp_on = 1;
	};

	class opj_decoding_param_t {
		/**
		 * if != 0, then original dimension divided by 2^(reduce); if == 0 or
		 * not used, image is decoded to the full resolution
		 */
		int m_reduce;
		/**
		 * if != 0, then only the first "layer" layers are decoded; if == 0 or
		 * not used, all the quality layers are decoded
		 */
		int m_layer;
	};

	/**
	 * FIXME DOC
	 */
	class opj_mct_data_t {
		int m_element_type;
		int m_array_type;
		int m_index;
		byte m_data[];
		int m_data_size;
	};

	/**
	 * FIXME DOC
	 */
	class opj_simple_mcc_decorrelation_data_t {
		int m_index;
		int m_nb_comps;
		opj_mct_data_t m_decorrelation_array;
		opj_mct_data_t m_offset_array;
		boolean m_is_irreversible = true;
	};

	class opj_ppx {
		byte m_data[]; /* m_data == NULL => Zppx not read yet */
		int m_data_size;
	};

	/**
	 * Tile coding parameters : this structure is used to store coding/decoding
	 * parameters common to all tiles (information like COD, COC in main header)
	 */
	class opj_tcp_t {
		/** coding style */
		int csty;
		/** progression order */
		int prg;
		/** number of layers */
		int numlayers;
		int num_layers_to_decode;
		/** multi-component transform identifier */
		int mct;
		/** rates of layers */
		float rates[] = new float[100];
		/** number of progression order changes */
		int numpocs;
		/** progression order changes */
		opj_poc_t pocs[] = new opj_poc_t[32];

		/** number of ppt markers (reserved size) */
		int ppt_markers_count;
		/** ppt markers data (table indexed by Zppt) */
		opj_ppx ppt_markers[];

		/** packet header store there for future use in t2_decode_packet */
		byte ppt_data[];
		/** used to keep a track of the allocated memory */
		byte ppt_buffer[];
		/** Number of bytes stored inside ppt_data */
		int ppt_data_size;
		/** size of ppt_data */
		int ppt_len;
		/** add fixed_quality */
		float distoratio[] = new float[100];
		/** tile-component coding parameters */
		opj_tccp_t tccps[];
		/** number of tile parts for the tile. */
		int m_nb_tile_parts;
		/** data for the tile */
		byte m_data[];
		/** size of data */
		int m_data_size;
		/** encoding norms */
		double mct_norms[];
		/** the mct decoding matrix */
		float m_mct_decoding_matrix[];
		/** the mct coding matrix */
		float m_mct_coding_matrix[];
		/** mct records */
		opj_mct_data_t m_mct_records[];
		/** the number of mct records. */
		int m_nb_mct_records;
		/** the max number of mct records. */
		int m_nb_max_mct_records;
		/** mcc records */
		opj_simple_mcc_decorrelation_data_t m_mcc_records[];
		/** the number of mct records. */
		int m_nb_mcc_records;
		/** the max number of mct records. */
		int m_nb_max_mcc_records;

		/***** FLAGS *******/
		/** If cod == 1 --> there was a COD marker for the present tile */
		//int cod = 1;
		int cod = 0;
		/** If ppt == 1 --> there was a PPT marker for the present tile */
		// int ppt = 1;
		int ppt = 0;
		/** indicates if a POC marker has been used O:NO, 1:YES */
		// boolean POC = true;
		boolean POC = false;
	};

	/**
	 * Coding parameters
	 */
	class opj_cp_t {
		/** Size of the image in bits */
		/* int img_size; */
		/** Rsiz */
		short rsiz;
		/** XTOsiz */
		int tx0; /* MSD see norm */
		/** YTOsiz */
		int ty0; /* MSD see norm */
		/** XTsiz */
		int tdx;
		/** YTsiz */
		int tdy;
		/** comment */
		String comment;
		/** number of tiles in width */
		int tw;
		/** number of tiles in heigth */
		int th;

		/** number of ppm markers (reserved size) */
		int ppm_markers_count;
		/** ppm markers data (table indexed by Zppm) */
		opj_ppx ppm_markers[];

		/** packet header store there for future use in t2_decode_packet */
		byte ppm_data[];
		/** size of the ppm_data */
		int ppm_len;
		/** size of the ppm_data */
		int ppm_data_read;

		byte ppm_data_current[];

		/** packet header storage original buffer */
		byte ppm_buffer[];
		/**
		 * pointer remaining on the first byte of the first header if ppm is
		 * used
		 */
		long ppm_data_first;
		/** Number of bytes actually stored inside the ppm_data */
		int ppm_data_size;
		/** use in case of multiple marker PPM (number of info already store) */
		int ppm_store;
		/**
		 * use in case of multiple marker PPM (case on non-finished previous
		 * info)
		 */
		int ppm_previous;

		/** tile coding parameters */
		opj_tcp_t tcps[];

		opj_decoding_param_t m_dec;

		opj_encoding_param_t m_enc;

		// m_specific_param;

		// #ifdef USE_JPWL
		/** enables writing of EPC in MH, thus activating JPWL */
		boolean epc_on;
		/** enables writing of EPB, in case of activated JPWL */
		boolean epb_on;
		/** enables writing of ESD, in case of activated JPWL */
		boolean esd_on;
		/**
		 * enables writing of informative techniques of ESD, in case of
		 * activated JPWL
		 */
		boolean info_on;
		/** enables writing of RED, in case of activated JPWL */
		boolean red_on;
		/** error protection method for MH (0,1,16,32,37-128) */
		int hprot_MH;
		/** tile number of header protection specification (>=0) */
		int hprot_TPH_tileno[] = new int[JPWL_MAX_NO_TILESPECS];
		/** error protection methods for TPHs (0,1,16,32,37-128) */
		int hprot_TPH[] = new int[JPWL_MAX_NO_TILESPECS];
		/** tile number of packet protection specification (>=0) */
		int pprot_tileno[] = new int[JPWL_MAX_NO_PACKSPECS];
		/** packet number of packet protection specification (>=0) */
		int pprot_packno[] = new int[JPWL_MAX_NO_PACKSPECS];
		/** error protection methods for packets (0,1,16,32,37-128) */
		int pprot[] = new int[JPWL_MAX_NO_PACKSPECS];
		/** enables writing of ESD, (0/2/4 bytes) */
		int sens_size;
		/** sensitivity addressing size (0=auto/2/4 bytes) */
		int sens_addr;
		/** sensitivity range (0-3) */
		int sens_range;
		/** sensitivity method for MH (-1,0-7) */
		int sens_MH;
		/** tile number of sensitivity specification (>=0) */
		int sens_TPH_tileno[] = new int[JPWL_MAX_NO_TILESPECS];
		/** sensitivity methods for TPHs (-1,0-7) */
		int sens_TPH[] = new int[JPWL_MAX_NO_TILESPECS];
		/** enables JPWL correction at the decoder */
		boolean correct;
		/** expected number of components at the decoder */
		int exp_comps;
		/** maximum number of tiles at the decoder */
		int max_tiles;
		// #endif /* USE_JPWL */

		/******** FLAGS *********/
		/** if ppm == 1 --> there was a PPM marker */
		int ppm = 1;
		/** tells if the parameter is a coding or decoding one */
		boolean m_is_decoder = true;
	};

	class opj_j2k_dec_t {
		/**
		 * locate in which part of the codestream the decoder is (main header,
		 * tile header, end)
		 */
		int m_state;
		/**
		 * store decoding parameters common to all tiles (information like COD,
		 * COC in main header)
		 */
		opj_tcp_t m_default_tcp;
		byte m_header_data[];
		int m_header_offset = 0;
		int m_header_data_size;
		/** to tell the tile part length */
		int m_sot_length;
		/** Only tiles index in the correct range will be decoded. */
		int m_start_tile_x;
		int m_start_tile_y;
		int m_end_tile_x;
		int m_end_tile_y;
		/**
		 * Decoded area set by the user
		 */
		int m_DA_x0;
		int m_DA_y0;
		int m_DA_x1;
		int m_DA_y1;

		/** Index of the tile to decode (used in get_tile) */
		int m_tile_ind_to_dec;
		/** Position of the last SOT marker read */
		long m_last_sot_read_pos;

		/**
		 * Indicate that the current tile-part is assume as the last tile part
		 * of the codestream. It is useful in the case of PSot is equal to zero.
		 * The sot length will be compute in the SOD reader function. FIXME NOT
		 * USED for the moment
		 */
		int m_last_tile_part;
		/** to tell that a tile can be decoded. */
		// Used by opj_j2k_decode_tile(), opj_j2k_read_sot(), and opj_j2k_read_tile_header().
		boolean m_can_decode = true;
		// Used by opj_j2k_read_siz() and opj_j2k_read_deccode_area().
		boolean m_discard_tiles = true;
		// Used by opj_j2k_read_sot() and opj_j2k_read_tile_header()
		boolean m_skip_data = true;
		/** TNsot correction : see issue 254 **/
		int m_nb_tile_parts_correction_checked = 1;
		int m_nb_tile_parts_correction = 1;

	};

	// struct opj_tcd;

	/**
	 * JPEG-2000 codestream reader/writer
	 */
	class opj_j2k_t {
		/* J2K codestream is decoded */
		boolean m_is_decoder;

		opj_j2k_dec_t m_decoder;
		// opj_j2k_enc_t m_encoder;

		// m_specific_param;

		/** pointer to the internal/private encoded / decoded image */
		opj_image_t m_private_image;

		/* pointer to the output image (decoded) */
		opj_image_t m_output_image;

		/** Coding parameters */
		opj_cp_t m_cp;

		/** the list of procedures to exec **/
		opj_procedure_list_t m_procedure_list;

		/**
		 * the list of validation procedures to follow to make sure the code is
		 * valid
		 **/
		opj_procedure_list_t m_validation_list;

		/** helper used to write the index file */
		opj_codestream_index_t cstr_index;

		/** number of the tile curently concern by coding/decoding */
		int m_current_tile_number[] = new int[1];

		/** the current tile coder/decoder **/
		opj_tcd_t m_tcd;
	};

	/**
	 * Tag node
	 */
	class opj_tgt_node_t {
		opj_tgt_node_t parent;
		int value;
		int low;
		int known;
	};

	/**
	 * Tag tree
	 */
	class opj_tgt_tree_t {
		int numleafsh;
		int numleafsv;
		int numnodes;
		opj_tgt_node_t nodes[];
		int nodes_size; /* maximum size taken by nodes */
	};

	class opj_tcd_seg_t {
		byte data[];
		int dataindex;
		int numpasses;
		int real_num_passes;
		int len;
		int maxpasses;
		int numnewpasses;
		int newlen;
	};

	class opj_tcd_cblk_dec_t {
		byte data[]; /* Data */
		opj_tcd_seg_t segs[]; /* segments information */
		int x0, y0, x1, y1; /*
							 * position of the code-blocks : left upper corner
							 * (x0, y0) right low corner (x1,y1)
							 */
		int numbps;
		int numlenbits;
		int data_max_size; /* Size of allocated data buffer */
		int data_current_size; /* Size of used data buffer */
		int numnewpasses; /* number of pass added to the code-blocks */
		int numsegs; /* number of segments */
		int real_num_segs;
		int m_current_max_segs;
	};

	class opj_tcd_precinct_t {
		int x0, y0, x1, y1; /*
							 * dimension of the precinct : left upper corner
							 * (x0, y0) right low corner (x1,y1)
							 */
		int cw, ch; /* number of precinct in width and height */
		// union{ /* code-blocks information */
		// opj_tcd_cblk_enc_t* enc;
		opj_tcd_cblk_dec_t dec[];
		// void* blocks;
		// } cblks;
		int block_size; /* size taken by cblks (in bytes) */
		opj_tgt_tree_t incltree; /* inclusion tree */
		opj_tgt_tree_t imsbtree; /* IMSB tree */
	};

	class opj_tcd_band_t {
		int x0, y0, x1, y1; /*
							 * dimension of the subband : left upper corner (x0,
							 * y0) right low corner (x1,y1)
							 */
		int bandno;
		opj_tcd_precinct_t precincts[]; /* precinct information */
		int precincts_data_size; /* size of data taken by precincts */
		int numbps;
		float stepsize;
	};

	class opj_tcd_resolution_t {
		int x0, y0, x1, y1; /*
							 * dimension of the resolution level : left upper
							 * corner (x0, y0) right low corner (x1,y1)
							 */
		int pw, ph;
		int numbands; /* number sub-band for the resolution level */
		opj_tcd_band_t bands[] = new opj_tcd_band_t[3]; /* subband information */
	};

	class opj_tcd_tilecomp_t {
		int x0, y0, x1, y1; /*
							 * dimension of component : left upper corner (x0,
							 * y0) right low corner (x1,y1)
							 */
		int numresolutions; /* number of resolutions level */
		int minimum_num_resolutions; /*
									 * number of resolutions level to decode (at
									 * max)
									 */
		opj_tcd_resolution_t resolutions[]; /* resolutions information */
		int resolutions_size; /* size of data for resolutions (in bytes) */
		int data[]; /* data of the component */
		boolean ownsData; /*
						 * if true, then need to free after usage, otherwise do
						 * not free
						 */
		int data_size_needed; /*
							 * we may either need to allocate this amount of
							 * data, or re-use image data and ignore this value
							 */
		int data_size; /* size of the data of the component */
		int numpix; /* add fixed_quality */
	};

	class opj_tcd_tile_t {
		int x0, y0, x1, y1; /*
							 * dimension of the tile : left upper corner (x0,
							 * y0) right low corner (x1,y1)
							 */
		int numcomps; /* number of components in tile */
		opj_tcd_tilecomp_t comps[]; /* Components information */
		int numpix; /* add fixed_quality */
		double distotile; /* add fixed_quality */
		double distolayer[] = new double[100]; /* add fixed_quality */
		int packno; /* packet number */
	};

	class opj_tcd_image_t {
		opj_tcd_tile_t tiles; /* Tiles information */
	};

	/**
	 * Tile coder/decoder
	 */
	class opj_tcd_t {
		/** Position of the tilepart flag in Progression order */
		int tp_pos;
		/** Tile part number */
		int tp_num;
		/** Current tile part number */
		int cur_tp_num;
		/** Total number of tileparts of the current tile */
		int cur_totnum_tp;
		/** Current Packet iterator number */
		int cur_pino;
		/** info on each image tile */
		opj_tcd_image_t tcd_image;
		/** image header */
		opj_image_t image;
		/** coding parameters */
		opj_cp_t cp;
		/** coding/decoding parameters common to all tiles */
		opj_tcp_t tcp;
		/** current encoded/decoded tile */
		int tcd_tileno;
		/** tell if the tcd is a decoder. */
		int m_is_decoder = 1;
	};

	/**
	 * Main codec handler used for compression or decompression.
	 */
	class opj_codec_private_t {
		// /** FIXME DOC */
		// union
		// {
		// /**
		// * Decompression handler.
		// */
		// struct opj_decompression
		// {
		// /** Main header reading function handler */
		// OPJ_BOOL (*opj_read_header) ( struct opj_stream_private * cio,
		// void * p_codec,
		// opj_image_t **p_image,
		// struct opj_event_mgr * p_manager);
		//
		// /** Decoding function */
		// OPJ_BOOL (*opj_decode) ( void * p_codec,
		// struct opj_stream_private * p_cio,
		// opj_image_t * p_image,
		// struct opj_event_mgr * p_manager);
		//
		// /** FIXME DOC */
		// OPJ_BOOL (*opj_read_tile_header)( void * p_codec,
		// OPJ_UINT32 * p_tile_index,
		// OPJ_UINT32 * p_data_size,
		// OPJ_INT32 * p_tile_x0,
		// OPJ_INT32 * p_tile_y0,
		// OPJ_INT32 * p_tile_x1,
		// OPJ_INT32 * p_tile_y1,
		// OPJ_UINT32 * p_nb_comps,
		// OPJ_BOOL * p_should_go_on,
		// struct opj_stream_private * p_cio,
		// struct opj_event_mgr * p_manager);
		//
		// /** FIXME DOC */
		// OPJ_BOOL (*opj_decode_tile_data)( void * p_codec,
		// OPJ_UINT32 p_tile_index,
		// OPJ_BYTE * p_data,
		// OPJ_UINT32 p_data_size,
		// struct opj_stream_private * p_cio,
		// struct opj_event_mgr * p_manager);
		//
		// /** Reading function used after codestream if necessary */
		// OPJ_BOOL (* opj_end_decompress) ( void *p_codec,
		// struct opj_stream_private * cio,
		// struct opj_event_mgr * p_manager);
		//
		// /** Codec destroy function handler */
		// void (*opj_destroy) (void * p_codec);
		//
		// /** Setup decoder function handler */
		// void (*opj_setup_decoder) ( void * p_codec, opj_dparameters_t *
		// p_param);
		//
		// /** Set decode area function handler */
		// OPJ_BOOL (*opj_set_decode_area) ( void * p_codec,
		// opj_image_t * p_image,
		// OPJ_INT32 p_start_x,
		// OPJ_INT32 p_end_x,
		// OPJ_INT32 p_start_y,
		// OPJ_INT32 p_end_y,
		// struct opj_event_mgr * p_manager);
		//
		// /** Get tile function */
		// OPJ_BOOL (*opj_get_decoded_tile) ( void *p_codec,
		// opj_stream_private_t * p_cio,
		// opj_image_t *p_image,
		// struct opj_event_mgr * p_manager,
		// OPJ_UINT32 tile_index);
		//
		// /** Set the decoded resolution factor */
		// OPJ_BOOL (*opj_set_decoded_resolution_factor) ( void * p_codec,
		// OPJ_UINT32 res_factor,
		// opj_event_mgr_t * p_manager);
		// } m_decompression;
		//
		// /**
		// * Compression handler. FIXME DOC
		// */
		// struct opj_compression
		// {
		// OPJ_BOOL (* opj_start_compress) ( void *p_codec,
		// struct opj_stream_private * cio,
		// struct opj_image * p_image,
		// struct opj_event_mgr * p_manager);
		//
		// OPJ_BOOL (* opj_encode) ( void * p_codec,
		// struct opj_stream_private *p_cio,
		// struct opj_event_mgr * p_manager);
		//
		// OPJ_BOOL (* opj_write_tile) ( void * p_codec,
		// OPJ_UINT32 p_tile_index,
		// OPJ_BYTE * p_data,
		// OPJ_UINT32 p_data_size,
		// struct opj_stream_private * p_cio,
		// struct opj_event_mgr * p_manager);
		//
		// OPJ_BOOL (* opj_end_compress) ( void * p_codec,
		// struct opj_stream_private * p_cio,
		// struct opj_event_mgr * p_manager);
		//
		// void (* opj_destroy) (void * p_codec);
		//
		// OPJ_BOOL (* opj_setup_encoder) ( void * p_codec,
		// opj_cparameters_t * p_param,
		// struct opj_image * p_image,
		// struct opj_event_mgr * p_manager);
		// } m_compression;
		// } m_codec_data;
		//
		opj_j2k_t m_codec;
		// /** Event handler */
		// opj_event_mgr_t m_event_mgr;
		// /** Flag to indicate if the codec is used to decode or encode*/
		boolean is_decompressor;
		// void (*opj_dump_codec) (void * p_codec, OPJ_INT32 info_flag, FILE*
		// output_stream);
		// opj_codestream_info_v2_t* (*opj_get_codec_info)(void* p_codec);
		// opj_codestream_index_t* (*opj_get_codec_index)(void* p_codec);
	};

	/**
	 * Tier-2 coding
	 */
	class opj_t2_t {

		/**
		 * Encoding: pointer to the src image. Decoding: pointer to the dst
		 * image.
		 */
		opj_image_t image;
		/** pointer to the image coding parameters */
		opj_cp_t cp;
	};

	class opj_pi_resolution_t {
		int pdx, pdy;
		int pw, ph;
	};

	class opj_pi_comp_t {
		int dx, dy;
		/** number of resolution levels */
		int numresolutions;
		opj_pi_resolution_t resolutions[];
	};

	/**
	 * Packet iterator
	 */
	class opj_pi_iterator_t {
		/** Enabling Tile part generation */
		byte tp_on;
		/**
		 * precise if the packet has been already used (useful for progression
		 * order change)
		 */
		short include[];
		/** layer step used to localize the packet in the include vector */
		int step_l;
		/** resolution step used to localize the packet in the include vector */
		int step_r;
		/** component step used to localize the packet in the include vector */
		int step_c;
		/** precinct step used to localize the packet in the include vector */
		int step_p;
		/** component that identify the packet */
		int compno;
		/** resolution that identify the packet */
		int resno;
		/** precinct that identify the packet */
		int precno;
		/** layer that identify the packet */
		int layno;
		/** 0 if the first packet */
		boolean first;
		/** progression order change information */
		opj_poc_t poc = new opj_poc_t();
		/** number of components in the image */
		int numcomps;
		/** Components */
		opj_pi_comp_t comps[];
		/** FIXME DOC */
		int tx0, ty0, tx1, ty1;
		/** FIXME DOC */
		int x, y;
		/** FIXME DOC */
		int dx, dy;
	};

	/**
	 * Tile-component coding parameters information
	 */
	class opj_tccp_info_t {
		/** component index */
		int compno;
		/** coding style */
		int csty;
		/** number of resolutions */
		int numresolutions;
		/** code-blocks width */
		int cblkw;
		/** code-blocks height */
		int cblkh;
		/** code-block coding style */
		int cblksty;
		/** discrete wavelet transform identifier */
		int qmfbid;
		/** quantisation style */
		int qntsty;
		/** stepsizes used for quantization */
		int stepsizes_mant[] = new int[OPJ_J2K_MAXBANDS];
		/** stepsizes used for quantization */
		int stepsizes_expn[] = new int[OPJ_J2K_MAXBANDS];
		/** number of guard bits */
		int numgbits;
		/** Region Of Interest shift */
		int roishift;
		/** precinct width */
		int prcw[] = new int[OPJ_J2K_MAXRLVLS];
		/** precinct height */
		int prch[] = new int[OPJ_J2K_MAXRLVLS];
	};

	/**
	 * Tile coding parameters information
	 */
	class opj_tile_info_v2_t {

		/** number (index) of tile */
		int tileno;
		/** coding style */
		int csty;
		/** progression order */
		int prg;
		/** number of layers */
		int numlayers;
		/** multi-component transform identifier */
		int mct;

		/** information concerning tile component parameters */
		opj_tccp_info_t tccp_info;

	};

	/**
	 * Individual bit input-output stream (BIO)
	 */
	class opj_bio_t {
		byte data[];
		/** pointer to the start of the buffer */
		int start;
		/** pointer to the end of the buffer */
		int end;
		/** pointer to the present position in the buffer */
		int bp;
		/** temporary place where each byte is read or written */
		int buf;
		/** coder : number of bits free to write. decoder : number of bits read */
		int ct;
	};

	/**
	 * RAW encoding operations
	 */
	class opj_raw_t {
		/** temporary buffer where bits are coded or decoded */
		byte c;
		/** number of bits already read or free to write */
		int ct;
		/** maximum length to decode */
		int lenmax;
		/** length decoded */
		int len;
		/** buffer - not included in original class */
		byte buffer[];
		/** pointer to the current position in the buffer */
		// byte bp[];
		int bp;
		/** pointer to the start of the buffer */
		int start;
		// byte start[];
		/** pointer to the end of the buffer */
		int end;
		// byte end[];
	};

	/**
	 * This struct defines the state of a context.
	 */
	class opj_mqc_state_t {
		/**
		 * the probability of the Least Probable Symbol (0.75->0x8000,
		 * 1.5->0xffff)
		 */
		int qeval;
		/** the Most Probable Symbol (0 or 1) */
		int mps;
		/** next state if the next encoded symbol is the MPS */
		opj_mqc_state_t nmps;
		/** next state if the next encoded symbol is the LPS */
		opj_mqc_state_t nlps;
	};

	/**
	 * MQ coder
	 */
	class opj_mqc_t {
		int c;
		int a;
		int ct;
		// Not included in original class
		byte buff[];
		// current location in buffer
		// byte bp[];
		int bp;
		// start of buffer
		// byte start[];
		int start;
		// end of buffer
		// byte end[];
		int end;
		opj_mqc_state_t ctxs[] = new opj_mqc_state_t[MQC_NUMCTXS];
		// The current ctxs index
		opj_mqc_state_t curctx;
		// #ifdef MQC_PERF_OPT
		// char buffer[];
		byte buffer[];
		// #endif
	};

	/**
	 * Tier-1 coding (coding of code-block coefficients)
	 */
	class opj_t1_t {

		/** MQC component */
		opj_mqc_t mqc;
		/** RAW component */
		opj_raw_t raw;

		int data[];
		short flags[];
		int w;
		int h;
		int datasize;
		int flagssize;
		int flags_stride;
		int data_stride;
		boolean encoder;
	};

	class opj_dwt_t {
		int mem[];
		int dn;
		int sn;
		int cas;
	};

	class opj_v4_t {
		float f[] = new float[4];
	};

	class opj_v4dwt_t {
		opj_v4_t wavelet[];
		int dn;
		int sn;
		int cas;
	};

	opj_mqc_state_t mqc_states[] = new opj_mqc_state_t[47 * 2];

	/**
	 * ------------------------------------------------------------------------
	 * --
	 */
	/**
	 * OPJ_DECOMPRESS MAIN
	 */
	/*
	 * --------------------------------------------------------------------------
	 */
	public boolean opj_decompress_main()
    {
    	opj_decompress_parameters parameters = new opj_decompress_parameters();			/* decompression parameters */
    	opj_image_t image = null;
    	RandomAccessFile l_stream = null;				/* Stream */
    	//opj_codec_t* l_codec = NULL;				/* Handle to a decompressor */
    	opj_codestream_index_t cstr_index = null;

    	int num_images, imageno;
    	img_fol_t img_fol;
    	dircnt_t dirptr = null;
      int failed = 0;
      int numDecompressedImages = 0;
      int i;
      opj_codec_private_t l_codec = null;
      
      final long startTime = System.currentTimeMillis();
      
      for (i = 0; i < 94; i++) {
    	  mqc_states[i] = new opj_mqc_state_t();
      }
      
      mqc_states[0].qeval = 0x5601;
      mqc_states[0].mps = 0;
      mqc_states[0].nmps = mqc_states[2];
      mqc_states[0].nlps = mqc_states[3];
      mqc_states[1].qeval = 0x5601;
      mqc_states[1].mps = 1;
      mqc_states[1].nmps = mqc_states[3];
      mqc_states[1].nlps = mqc_states[2];
  	  mqc_states[2].qeval = 0x3401;
      mqc_states[2].mps = 0;
      mqc_states[2].nmps = mqc_states[4];
      mqc_states[2].nlps = mqc_states[12];
      mqc_states[3].qeval = 0x3401;
      mqc_states[3].mps = 1;
      mqc_states[3].nmps = mqc_states[5];
      mqc_states[3].nlps = mqc_states[13];
      mqc_states[4].qeval = 0x1801;
      mqc_states[4].mps = 0;
      mqc_states[4].nmps = mqc_states[6];
      mqc_states[4].nlps = mqc_states[18];
      mqc_states[5].qeval = 0x1801;
      mqc_states[5].mps = 1;
      mqc_states[5].nmps = mqc_states[7];
      mqc_states[5].nlps = mqc_states[19];
      mqc_states[6].qeval = 0x0ac1;
      mqc_states[6].mps = 0;
      mqc_states[6].nmps = mqc_states[8];
      mqc_states[6].nlps = mqc_states[24];
      mqc_states[7].qeval = 0x0ac1;
      mqc_states[7].mps = 1;
      mqc_states[7].nmps = mqc_states[9];
      mqc_states[7].nlps = mqc_states[25];
      mqc_states[8].qeval = 0x0521;
      mqc_states[8].mps = 0;
      mqc_states[8].nmps = mqc_states[10];
      mqc_states[8].nlps = mqc_states[58];
      mqc_states[9].qeval = 0x0521;
      mqc_states[9].mps = 1;
      mqc_states[9].nmps = mqc_states[11];
      mqc_states[9].nlps = mqc_states[59];
      mqc_states[10].qeval = 0x0221;
      mqc_states[10].mps = 0;
      mqc_states[10].nmps = mqc_states[76];
      mqc_states[10].nlps = mqc_states[66];
      mqc_states[11].qeval = 0x0221;
      mqc_states[11].mps = 1;
      mqc_states[11].nmps = mqc_states[77];
      mqc_states[11].nlps = mqc_states[67];
      mqc_states[12].qeval = 0x5601;
      mqc_states[12].mps = 0;
      mqc_states[12].nmps = mqc_states[14];
      mqc_states[12].nlps = mqc_states[13];
      mqc_states[13].qeval = 0x5601;
      mqc_states[13].mps = 1;
      mqc_states[13].nmps = mqc_states[15];
      mqc_states[13].nlps = mqc_states[12];
      mqc_states[14].qeval = 0x5401;
      mqc_states[14].mps = 0;
      mqc_states[14].nmps = mqc_states[16];
      mqc_states[14].nlps = mqc_states[28];
      mqc_states[15].qeval = 0x5401;
      mqc_states[15].mps = 1;
      mqc_states[15].nmps = mqc_states[17];
      mqc_states[15].nlps = mqc_states[29];
      mqc_states[16].qeval = 0x4801;
      mqc_states[16].mps = 0;
      mqc_states[16].nmps = mqc_states[18];
      mqc_states[16].nlps = mqc_states[28];
      mqc_states[17].qeval = 0x4801;
      mqc_states[17].mps = 1;
      mqc_states[17].nmps = mqc_states[19];
      mqc_states[17].nlps = mqc_states[29];
      mqc_states[18].qeval = 0x3801;
      mqc_states[18].mps = 0;
      mqc_states[18].nmps = mqc_states[20];
      mqc_states[18].nlps = mqc_states[28];
      mqc_states[19].qeval = 0x3801;
      mqc_states[19].mps = 1;
      mqc_states[19].nmps = mqc_states[21];
      mqc_states[19].nlps = mqc_states[29];
      mqc_states[20].qeval = 0x3001;
      mqc_states[20].mps = 0;
      mqc_states[20].nmps = mqc_states[22];
      mqc_states[20].nlps = mqc_states[34];
      mqc_states[21].qeval = 0x3001;
      mqc_states[21].mps = 1;
      mqc_states[21].nmps = mqc_states[23];
      mqc_states[21].nlps = mqc_states[35];
      mqc_states[22].qeval = 0x2401;
      mqc_states[22].mps = 0;
      mqc_states[22].nmps = mqc_states[24];
      mqc_states[22].nlps = mqc_states[36];
      mqc_states[23].qeval = 0x2401;
      mqc_states[23].mps = 1;
      mqc_states[23].nmps = mqc_states[25];
      mqc_states[23].nlps = mqc_states[37];
      mqc_states[24].qeval = 0x1c01;
      mqc_states[24].mps = 0;
      mqc_states[24].nmps = mqc_states[26];
      mqc_states[24].nlps = mqc_states[40];
      mqc_states[25].qeval = 0x1c01;
      mqc_states[25].mps = 1;
      mqc_states[25].nmps = mqc_states[27];
      mqc_states[25].nlps = mqc_states[41];
      mqc_states[26].qeval = 0x1601;
      mqc_states[26].mps = 0;
      mqc_states[26].nmps = mqc_states[58];
      mqc_states[26].nlps = mqc_states[42];
      mqc_states[27].qeval = 0x1601;
      mqc_states[27].mps = 1;
      mqc_states[27].nmps = mqc_states[59];
      mqc_states[27].nlps = mqc_states[43];
      mqc_states[28].qeval = 0x5601;
      mqc_states[28].mps = 0;
      mqc_states[28].nmps = mqc_states[30];
      mqc_states[28].nlps = mqc_states[29];
      mqc_states[29].qeval = 0x5601;
      mqc_states[29].mps = 1;
      mqc_states[29].nmps = mqc_states[31];
      mqc_states[29].nlps = mqc_states[28];
      mqc_states[30].qeval = 0x5401;
      mqc_states[30].mps = 0;
      mqc_states[30].nmps = mqc_states[32];
      mqc_states[30].nlps = mqc_states[28];
      mqc_states[31].qeval = 0x5401;
      mqc_states[31].mps = 1;
      mqc_states[31].nmps = mqc_states[33];
      mqc_states[31].nlps = mqc_states[29];
      mqc_states[32].qeval = 0x5101;
      mqc_states[32].mps = 0;
      mqc_states[32].nmps = mqc_states[34];
      mqc_states[32].nlps = mqc_states[30];
      mqc_states[33].qeval = 0x5101;
      mqc_states[33].mps = 1;
      mqc_states[33].nmps = mqc_states[35];
      mqc_states[33].nlps = mqc_states[31];
      mqc_states[34].qeval = 0x4801;
      mqc_states[34].mps = 0;
      mqc_states[34].nmps = mqc_states[36];
      mqc_states[34].nlps = mqc_states[32];
      mqc_states[35].qeval = 0x4801;
      mqc_states[35].mps = 1;
      mqc_states[35].nmps = mqc_states[37];
      mqc_states[35].nlps = mqc_states[33];
      mqc_states[36].qeval = 0x3801;
      mqc_states[36].mps = 0;
      mqc_states[36].nmps = mqc_states[38];
      mqc_states[36].nlps = mqc_states[34];
      mqc_states[37].qeval = 0x3801;
      mqc_states[37].mps = 1;
      mqc_states[37].nmps = mqc_states[39];
      mqc_states[37].nlps = mqc_states[35];
      mqc_states[38].qeval = 0x3401;
      mqc_states[38].mps = 0;
      mqc_states[38].nmps = mqc_states[40];
      mqc_states[38].nlps = mqc_states[36];
      mqc_states[39].qeval = 0x3401;
      mqc_states[39].mps = 1;
      mqc_states[39].nmps = mqc_states[41];
      mqc_states[39].nlps = mqc_states[37];
      mqc_states[40].qeval = 0x3001;
      mqc_states[40].mps = 0;
      mqc_states[40].nmps = mqc_states[42];
      mqc_states[40].nlps = mqc_states[38];
      mqc_states[41].qeval = 0x3001;
      mqc_states[41].mps = 1;
      mqc_states[41].nmps = mqc_states[43];
      mqc_states[41].nlps = mqc_states[39];
      mqc_states[42].qeval = 0x2801;
      mqc_states[42].mps = 0;
      mqc_states[42].nmps = mqc_states[44];
      mqc_states[42].nlps = mqc_states[38];
      mqc_states[43].qeval = 0x2801;
      mqc_states[43].mps = 1;
      mqc_states[43].nmps = mqc_states[45];
      mqc_states[43].nlps = mqc_states[39];
      mqc_states[44].qeval = 0x2401;
      mqc_states[44].mps = 0;
      mqc_states[44].nmps = mqc_states[46];
      mqc_states[44].nlps = mqc_states[40];
      mqc_states[45].qeval = 0x2401;
      mqc_states[45].mps = 1;
      mqc_states[45].nmps = mqc_states[47];
      mqc_states[45].nlps = mqc_states[41];
      mqc_states[46].qeval = 0x2201;
      mqc_states[46].mps = 0;
      mqc_states[46].nmps = mqc_states[48];
      mqc_states[46].nlps = mqc_states[42];
      mqc_states[47].qeval = 0x2201;
      mqc_states[47].mps = 1;
      mqc_states[47].nmps = mqc_states[49];
      mqc_states[47].nlps = mqc_states[43];
      mqc_states[48].qeval = 0x1c01;
      mqc_states[48].mps = 0;
      mqc_states[48].nmps = mqc_states[50];
      mqc_states[48].nlps = mqc_states[44];
      mqc_states[49].qeval = 0x1c01;
      mqc_states[49].mps = 1;
      mqc_states[49].nmps = mqc_states[51];
      mqc_states[49].nlps = mqc_states[45];
      mqc_states[50].qeval = 0x1801;
      mqc_states[50].mps = 0;
      mqc_states[50].nmps = mqc_states[52];
      mqc_states[50].nlps = mqc_states[46];
      mqc_states[51].qeval = 0x1801;
      mqc_states[51].mps = 1;
      mqc_states[51].nmps = mqc_states[53];
      mqc_states[51].nlps = mqc_states[47];
      mqc_states[52].qeval = 0x1601;
      mqc_states[52].mps = 0;
      mqc_states[52].nmps = mqc_states[54];
      mqc_states[52].nlps = mqc_states[48];
      mqc_states[53].qeval = 0x1601;
      mqc_states[53].mps = 1;
      mqc_states[53].nmps = mqc_states[55];
      mqc_states[53].nlps = mqc_states[49];
      mqc_states[54].qeval = 0x1401;
      mqc_states[54].mps = 0;
      mqc_states[54].nmps = mqc_states[56];
      mqc_states[54].nlps = mqc_states[50];
      mqc_states[55].qeval = 0x1401;
      mqc_states[55].mps = 1;
      mqc_states[55].nmps = mqc_states[57];
      mqc_states[55].nlps = mqc_states[51];
      mqc_states[56].qeval = 0x1201;
      mqc_states[56].mps = 0;
      mqc_states[56].nmps = mqc_states[58];
      mqc_states[56].nlps = mqc_states[52];
      mqc_states[57].qeval = 0x1201;
      mqc_states[57].mps = 1;
      mqc_states[57].nmps = mqc_states[59];
      mqc_states[57].nlps = mqc_states[53];
      mqc_states[58].qeval = 0x1101;
      mqc_states[58].mps = 0;
      mqc_states[58].nmps = mqc_states[60];
      mqc_states[58].nlps = mqc_states[54];
      mqc_states[59].qeval = 0x1101;
      mqc_states[59].mps = 1;
      mqc_states[59].nmps = mqc_states[61];
      mqc_states[59].nlps = mqc_states[55];
      mqc_states[60].qeval = 0x0ac1;
      mqc_states[60].mps = 0;
      mqc_states[60].nmps = mqc_states[62];
      mqc_states[60].nlps = mqc_states[56];
      mqc_states[61].qeval = 0x0ac1;
      mqc_states[61].mps = 1;
      mqc_states[61].nmps = mqc_states[63];
      mqc_states[61].nlps = mqc_states[57];
      mqc_states[62].qeval = 0x09c1;
      mqc_states[62].mps = 0;
      mqc_states[62].nmps = mqc_states[64];
      mqc_states[62].nlps = mqc_states[58];
      mqc_states[63].qeval = 0x09c1;
      mqc_states[63].mps = 1;
      mqc_states[63].nmps = mqc_states[65];
      mqc_states[63].nlps = mqc_states[59];
      mqc_states[64].qeval = 0x08a1;
      mqc_states[64].mps = 0;
      mqc_states[64].nmps = mqc_states[66];
      mqc_states[64].nlps = mqc_states[60];
      mqc_states[65].qeval = 0x08a1;
      mqc_states[65].mps = 1;
      mqc_states[65].nmps = mqc_states[67];
      mqc_states[65].nlps = mqc_states[61];
      mqc_states[66].qeval = 0x0521;
      mqc_states[66].mps = 0;
      mqc_states[66].nmps = mqc_states[68];
      mqc_states[66].nlps = mqc_states[62];
      mqc_states[67].qeval = 0x0521;
      mqc_states[67].mps = 1;
      mqc_states[67].nmps = mqc_states[69];
      mqc_states[67].nlps = mqc_states[63];
      mqc_states[68].qeval = 0x0441;
      mqc_states[68].mps = 0;
      mqc_states[68].nmps = mqc_states[70];
      mqc_states[68].nlps = mqc_states[64];
      mqc_states[69].qeval = 0x0441;
      mqc_states[69].mps = 1;
      mqc_states[69].nmps = mqc_states[71];
      mqc_states[69].nlps = mqc_states[65];
      mqc_states[70].qeval = 0x02a1;
      mqc_states[70].mps = 0;
      mqc_states[70].nmps = mqc_states[72];
      mqc_states[70].nlps = mqc_states[66];
      mqc_states[71].qeval = 0x02a1;
      mqc_states[71].mps = 1;
      mqc_states[71].nmps = mqc_states[73];
      mqc_states[71].nlps = mqc_states[67];
      mqc_states[72].qeval = 0x0221;
      mqc_states[72].mps = 0;
      mqc_states[72].nmps = mqc_states[74];
      mqc_states[72].nlps = mqc_states[68];
      mqc_states[73].qeval = 0x0221;
      mqc_states[73].mps = 1;
      mqc_states[73].nmps = mqc_states[75];
      mqc_states[73].nlps = mqc_states[69];
      mqc_states[74].qeval = 0x0141;
      mqc_states[74].mps = 0;
      mqc_states[74].nmps = mqc_states[76];
      mqc_states[74].nlps = mqc_states[70];
      mqc_states[75].qeval = 0x0141;
      mqc_states[75].mps = 1;
      mqc_states[75].nmps = mqc_states[77];
      mqc_states[75].nlps = mqc_states[71];
      mqc_states[76].qeval = 0x0111;
      mqc_states[76].mps = 0;
      mqc_states[76].nmps = mqc_states[78];
      mqc_states[76].nlps = mqc_states[72];
      mqc_states[77].qeval = 0x0111;
      mqc_states[77].mps = 1;
      mqc_states[77].nmps = mqc_states[79];
      mqc_states[77].nlps = mqc_states[73];
      mqc_states[78].qeval = 0x0085;
      mqc_states[78].mps = 0;
      mqc_states[78].nmps = mqc_states[80];
      mqc_states[78].nlps = mqc_states[74];
      mqc_states[79].qeval = 0x0085;
      mqc_states[79].mps = 1;
      mqc_states[79].nmps = mqc_states[81];
      mqc_states[79].nlps = mqc_states[75];
      mqc_states[80].qeval = 0x0049;
      mqc_states[80].mps = 0;
      mqc_states[80].nmps = mqc_states[82];
      mqc_states[80].nlps = mqc_states[76];
      mqc_states[81].qeval = 0x0049;
      mqc_states[81].mps = 1;
      mqc_states[81].nmps = mqc_states[83];
      mqc_states[81].nlps = mqc_states[77];
      mqc_states[82].qeval = 0x0025;
      mqc_states[82].mps = 0;
      mqc_states[82].nmps = mqc_states[84];
      mqc_states[82].nlps = mqc_states[78];
      mqc_states[83].qeval = 0x0025;
      mqc_states[83].mps = 1;
      mqc_states[83].nmps = mqc_states[85];
      mqc_states[83].nlps = mqc_states[79];
      mqc_states[84].qeval = 0x0015;
      mqc_states[84].mps = 0;
      mqc_states[84].nmps = mqc_states[86];
      mqc_states[84].nlps = mqc_states[80];
      mqc_states[85].qeval = 0x0015;
      mqc_states[85].mps = 1;
      mqc_states[85].nmps = mqc_states[87];
      mqc_states[85].nlps = mqc_states[81];
      mqc_states[86].qeval = 0x0009;
      mqc_states[86].mps = 0;
      mqc_states[86].nmps = mqc_states[88];
      mqc_states[86].nlps = mqc_states[82];
      mqc_states[87].qeval = 0x0009;
      mqc_states[87].mps = 1;
      mqc_states[87].nmps = mqc_states[89];
      mqc_states[87].nlps = mqc_states[83];
      mqc_states[88].qeval = 0x0005;
      mqc_states[88].mps = 0;
      mqc_states[88].nmps = mqc_states[90];
      mqc_states[88].nlps = mqc_states[84];
      mqc_states[89].qeval = 0x0005;
      mqc_states[89].mps = 1;
      mqc_states[89].nmps = mqc_states[91];
      mqc_states[89].nlps = mqc_states[85];
      mqc_states[90].qeval = 0x0001;
      mqc_states[90].mps = 0;
      mqc_states[90].nmps = mqc_states[90];
      mqc_states[90].nlps = mqc_states[86];
      mqc_states[91].qeval = 0x0001;
      mqc_states[91].mps = 1;
      mqc_states[91].nmps = mqc_states[91];
      mqc_states[91].nlps = mqc_states[87];
      mqc_states[92].qeval = 0x5601;
      mqc_states[92].mps = 0;
      mqc_states[92].nmps = mqc_states[92];
      mqc_states[92].nlps = mqc_states[92];
      mqc_states[93].qeval = 0x5601;
      mqc_states[93].mps = 1;
      mqc_states[93].nmps = mqc_states[93];
      mqc_states[93].nlps = mqc_states[93];
    	/* set decoding parameters to default values */
    	set_default_parameters(parameters);

    	/* Initialize img_fol */
    	img_fol = new img_fol_t();
        img_fol.set_out_format = 0;
        if ((inputImageDirectory != null)  && (compressedFile != null)) {
        	MipavUtil.displayError("inputImageDirectory and compressedFile cannot be used together");
        	destroy_parameters(parameters);
	    	return false;
        }
        if ((inputImageDirectory != null) && (outputFormatExtension == null)) {
        	MipavUtil.displayError("When inputImageDirectory is used outputFormatExtension must be used");
        	destroy_parameters(parameters);
        	return false;
        }
        if ((inputImageDirectory != null) && (decompressedFile != null)) {
        	MipavUtil.displayError("decompressedFile cannot be used with inputImageDirectory");
        	destroy_parameters(parameters);
        	return false;
        }
        if ((compressedFile != null) && (decompressedFile == null)) {
        	MipavUtil.displayError("decompressedFile must be present along with compressedFile");
        	destroy_parameters(parameters);
        	return false;
        }
        if ((inputImageDirectory == null) && (compressedFile == null)) {
        	MipavUtil.displayError("Either a inputImageDirectory or a compressedFile must be present");
        	destroy_parameters(parameters);
        	return false;
        }
        if ((compressedFile != null) && (outputFormatExtension != null)) {
        	MipavUtil.displayError("outputFormatExtension is not used with compressedFile");
        	destroy_parameters(parameters);
        	return false;
        } 
    	// parse input and get user encoding parameters
    	if (compressedFile != null) {
    	    parameters.decod_format  = infile_format(compressedFile);
    	    switch(parameters.decod_format) {
    	    case J2K_CFMT:
    	    	break;
    	    case JP2_CFMT:
    	    	break;
    	    case JPT_CFMT:
    	    	break;
    	    case -2:
    	    	MipavUtil.displayError("Input file " + compressedFile + " cannot be read");
    	    	destroy_parameters(parameters);
    	    	return false;
    	    default:
    	    	MipavUtil.displayError("Input file " + compressedFile + " has an illegal file format");
    	    	MipavUtil.displayError("Legal file formats are .j2k, .jp2, .jpc, .jpt");
    	    	destroy_parameters(parameters);
    	    	return false;
    	    } // switch(parameters.decod_format)
    	    parameters.infile = new String(compressedFile);
    	} // if (compressedFile != null)
    	if (decompressedFile != null) {
	    	parameters.cod_format = get_file_format(decompressedFile);
	    	switch(parameters.cod_format) {
	    	case PGX_DFMT:
				break;
			case PXM_DFMT:
				break;
			case BMP_DFMT:
				break;
			case TIF_DFMT:
				break;
			case RAW_DFMT:
				break;
			case RAWL_DFMT:
				break;
			case TGA_DFMT:
				break;
			case PNG_DFMT:
				break;
			default:
				MipavUtil.displayError("Illegal decompressed image output format for " + decompressedFile);
				MipavUtil.displayError("only *.png, *.pnm, *.pgm, *.ppm, *.pgx, *.bmp, *.tif, *.raw or *.tga");
				destroy_parameters(parameters);
		    	return false;
	    	} // switch(parameters.code_format)
	    	parameters.outfile = new String(decompressedFile);
    	} // if (decompressedFile != null)
    	if (outputFormatExtension != null) {
    	    img_fol.set_out_format = 1;
    	    parameters.cod_format = get_file_format("." + outputFormatExtension);
    	    switch(parameters.cod_format) {
			case PGX_DFMT:
				img_fol.out_format = "pgx";
				break;
			case PXM_DFMT:
				img_fol.out_format = "ppm";
				break;
			case BMP_DFMT:
				img_fol.out_format = "bmp";
				break;
			case TIF_DFMT:
				img_fol.out_format = "tif";
				break;
			case RAW_DFMT:
				img_fol.out_format = "raw";
				break;
			case RAWL_DFMT:
				img_fol.out_format = "rawl";
				break;
			case TGA_DFMT:
				img_fol.out_format = "tga";
				break;
			case PNG_DFMT:
				img_fol.out_format = "png";
				break;
			default:
		        MipavUtil.displayError("Illegal output format file extension " + outputFormatExtension);
		        MipavUtil.displayError("Only *.png, *.pnm, *.pgm, *.ppm, *.pgx, *.bmp, *.tif, *.raw or *.tga");
				destroy_parameters(parameters);
				return false;
    	    }
    	} // if (outputFormatExtension != null)
    	parameters.core.cp_reduce = reduceFactor;
    	parameters.core.cp_layer = qualityLayers;
    	if (inputImageDirectory != null) {
    	    img_fol.imgdirpath = new String(inputImageDirectory);
    	    img_fol.set_imgdir = 1;	
    	} // if (inputImageDirectory != null)
    	if (decodingArea) {
    		parameters.DA_x0 = x0;
    		parameters.DA_y0 = y0;
    		parameters.DA_x1 = x1;
    		parameters.DA_y1 = y1;
    	} // if (decodingArea)
    	if (tileIndex > -1) {
    		parameters.tile_index = tileIndex;
    		parameters.nb_tile_to_decode = 1;
    	} // if (tileIndex > -1)
    	if (indexName != null) {
    		parameters.indexfilename = indexName;
    	} // if (indexName != null)
    	parameters.nb_precision = 0;
    	if (component0Precision > -1) {
    	    if (component2Precision > -1) {
    	        parameters.precision = new opj_precision[3];
    	        for (i = 0; i < 3; i++) {
    	            parameters.precision[i] = new opj_precision();	
    	        }
    	        parameters.nb_precision = 3;
    	    }
    	    else if (component1Precision > -1) {
    	    	parameters.precision = new opj_precision[2];
    	    	for (i = 0; i < 2; i++) {
    	    		parameters.precision[i] = new opj_precision();
    	    	}
    	    	parameters.nb_precision = 2;
    	    }
    	    else {
    	    	parameters.precision = new opj_precision[1];
    	    	parameters.precision[0] = new opj_precision();
    	    	parameters.nb_precision = 1;
    	    }
    	    if((component0Precision < 1) || (component0Precision > 32)) {
    	    	MipavUtil.displayError("component0Precision must be between 1 and 32");
    	    	destroy_parameters(parameters);
    	    	return false;
    	    }
    	    parameters.precision[0].prec = component0Precision;
    	    if ((mode0 != OPJ_PREC_MODE_CLIP) && (mode0 != OPJ_PREC_MODE_SCALE)) {
    	    	MipavUtil.displayError("mode0 must be OPJ_PREC_MODE_CLIP or OPJ_PREC_MODE_SCALE");
    	    	destroy_parameters(parameters);
    	    	return false;
    	    }
    	    parameters.precision[0].mode = mode0;
    	    if (component1Precision > -1) {
    	    	if((component1Precision < 1) || (component1Precision > 32)) {
        	    	MipavUtil.displayError("component1Precision must be between 1 and 32");
        	    	destroy_parameters(parameters);
        	    	return false;
        	    }
        	    parameters.precision[1].prec = component1Precision;
        	    if ((mode1 != OPJ_PREC_MODE_CLIP) && (mode1 != OPJ_PREC_MODE_SCALE)) {
        	    	MipavUtil.displayError("mode1 must be OPJ_PREC_MODE_CLIP or OPJ_PREC_MODE_SCALE");
        	    	destroy_parameters(parameters);
        	    	return false;
        	    }
        	    parameters.precision[1].mode = mode1;
        	    if (component2Precision > -1) {
        	    	if((component2Precision < 1) || (component2Precision > 32)) {
            	    	MipavUtil.displayError("component2Precision must be between 1 and 32");
            	    	destroy_parameters(parameters);
            	    	return false;
            	    }
            	    parameters.precision[2].prec = component2Precision;
            	    if ((mode2 != OPJ_PREC_MODE_CLIP) && (mode2 != OPJ_PREC_MODE_SCALE)) {
            	    	MipavUtil.displayError("mode2 must be OPJ_PREC_MODE_CLIP or OPJ_PREC_MODE_SCALE");
            	    	destroy_parameters(parameters);
            	    	return false;
            	    }
            	    parameters.precision[2].mode = mode2;
        	    } // if (component2Precision > -1)
    	    } // if (component1Precision > -1)
    	} // if (component0Precision > -1)
    	if (useJPWL) {
    	  if (JPWLCorrection) {
              if ((JPWLNumComponents < 1) || (JPWLNumComponents > 256)) {
            	  MipavUtil.displayError("JPWLNumComponents must be between 1 and 256");
            	  destroy_parameters(parameters);
            	  return false;
              }
              parameters.jpwl_exp_comps = JPWLNumComponents;
              if ((JPWLMaximumTiles < 1) || (JPWLMaximumTiles > JPWL_MAXIMUM_TILES)) {
            	  MipavUtil.displayError("JPWLMaximumTiles must be between 1 and " + JPWL_MAXIMUM_TILES);
            	  destroy_parameters(parameters);
            	  return false;
              }
              parameters.jpwl_max_tiles = JPWLMaximumTiles;
              parameters.jpwl_correct = true;
    	  } // if (JPWLCorrection)
    	} // if (useJPWL)
    	
    	// Initialize reading of directory
    	if(img_fol.set_imgdir==1){	
    		num_images=get_num_images(img_fol.imgdirpath);

    		dirptr = new dircnt_t();
    		dirptr.filename = new String[num_images];
    			
    		if(load_images(dirptr,img_fol.imgdirpath)==1){
    			destroy_parameters(parameters);
    			return false;
    		}
    		if (num_images==0){
    			MipavUtil.displayError("Folder is empty");
    			destroy_parameters(parameters);
    			return false;
    		}
    	}else{
    		num_images=1;
    	} 

    	/*Decoding image one by one*/
    	for(imageno = 0; imageno < num_images ; imageno++)	{

    		if(img_fol.set_imgdir==1){
    			if (get_next_file(imageno, dirptr,img_fol, parameters) == 1) {
    				Preferences.debug("skipping file...\n", Preferences.DEBUG_FILEIO);
    				destroy_parameters(parameters);
    				continue;
    			}
    		}

    		/* read the input file and put it in memory */
    		/* ---------------------------------------- */

    		l_stream = opj_stream_create_default_file_stream(parameters.infile,true);
    		if (l_stream == null){
    			MipavUtil.displayError("ERROR -> failed to create the RandomAccessFile from the file " + parameters.infile);
    			destroy_parameters(parameters);
    			return false;
    		}

    		/* decode the JPEG2000 stream */
    		/* ---------------------- */
    		if ((parameters.decod_format != J2K_CFMT) && (parameters.decod_format != JP2_CFMT)) {
    			Preferences.debug("Skipping file\n", Preferences.DEBUG_FILEIO);
    			destroy_parameters(parameters);
    			try {
    			    l_stream.close();
    			}
    			catch (IOException e) {
    				MipavUtil.displayError("IOException on l_stream.close()");
    				return false;
    			}
    			continue;
    		}

//    		switch(parameters.decod_format) {
//    			case J2K_CFMT:	/* JPEG-2000 codestream */
//    			{
//    				/* Get a decoder handle */
//    				l_codec = opj_create_decompress(OPJ_CODEC_J2K);
//    				break;
//    			}
//    			case JP2_CFMT:	/* JPEG 2000 compressed image data */
//    			{
//    				/* Get a decoder handle */
//    				l_codec = opj_create_decompress(OPJ_CODEC_JP2);
//    				break;
//    			}
//    			case JPT_CFMT:	/* JPEG 2000, JPIP */
//    			{
//    				/* Get a decoder handle */
//    				l_codec = opj_create_decompress(OPJ_CODEC_JPT);
//    				break;
//    			}
//    			default:
//    				fprintf(stderr, "skipping file..\n");
//    				destroy_parameters(&parameters);
//    				opj_stream_destroy(l_stream);
//    				continue;
//    		}

    		/* catch events using our callbacks and give a local context */		
//    		opj_set_info_handler(l_codec, info_callback,00);
//    		opj_set_warning_handler(l_codec, warning_callback,00);
//    		opj_set_error_handler(l_codec, error_callback,00);

//    		t = opj_clock();
    		
    		// opj_create_decompress
    		l_codec = new opj_codec_private_t();
		    l_codec.is_decompressor = true;
    		if (parameters.decod_format == J2K_CFMT) {
    		    // opj_j2k_t* opj_j2k_create_decompress(void)
    		    opj_j2k_t l_j2k = new opj_j2k_t();
    		    l_codec.m_codec = l_j2k;
    		    l_j2k.m_is_decoder = true;
    		    l_j2k.m_cp = new opj_cp_t();
    		    l_j2k.m_cp.m_is_decoder = true;
    		    l_j2k.m_decoder = new opj_j2k_dec_t();
    		    if (OPJ_DISABLE_TPSOT_FIX) {
    		    	l_j2k.m_decoder.m_nb_tile_parts_correction_checked = 1;	
    		    }
    		    l_j2k.m_decoder.m_default_tcp = new opj_tcp_t();
    		    for (i = 0; i < 32; i++) {
    		    	l_j2k.m_decoder.m_default_tcp.pocs[i] = new opj_poc_t();
    		    }
    		    l_j2k.m_decoder.m_header_data = new byte[OPJ_J2K_DEFAULT_HEADER_SIZE];
    		    l_j2k.m_decoder.m_header_data_size = OPJ_J2K_DEFAULT_HEADER_SIZE;
    		    l_j2k.m_decoder.m_state = 0x0000;

	            l_j2k.m_decoder.m_tile_ind_to_dec = -1 ;

	            l_j2k.m_decoder.m_last_sot_read_pos = 0;
    		    
	            /* codestream index creation */
	            // opj_j2k_create_cstr_index();
	            l_j2k.cstr_index = new opj_codestream_index_t();
    		    l_j2k.cstr_index.maxmarknum = 100;
    		    l_j2k.cstr_index.marknum = 0;
    		    l_j2k.cstr_index.marker = new opj_marker_info_t[l_j2k.cstr_index.maxmarknum];
    		    for (i = 0; i < l_j2k.cstr_index.maxmarknum; i++) {
    		    	l_j2k.cstr_index.marker[i] = new opj_marker_info_t();
    		    }
                l_j2k.cstr_index.tile_index = null;
                
                /* validation list creation */
                // opj_procedure_list_create();
                l_j2k.m_validation_list = new opj_procedure_list_t();
                l_j2k.m_validation_list.m_nb_procedures = 0;
                l_j2k.m_validation_list.m_nb_max_procedures = OPJ_VALIDATION_SIZE;
                l_j2k.m_validation_list.m_procedures = new Vector<Integer>(OPJ_VALIDATION_SIZE);
                
                /* execution list creation */
                // opj_procedure_list_create();
                l_j2k.m_procedure_list = new opj_procedure_list_t();
                l_j2k.m_procedure_list.m_nb_procedures = 0;
                l_j2k.m_procedure_list.m_nb_max_procedures = OPJ_VALIDATION_SIZE;
                l_j2k.m_procedure_list.m_procedures = new Vector<Integer>(OPJ_VALIDATION_SIZE);
    		            
                // opj_set_default_event_handler(&(l_codec->m_event_mgr));
                // Setup the decoder decoding parameters using user parameters 
        		// if ( !opj_setup_decoder(l_codec, &(parameters.core)) ){
                // opj_j2k_setup_decoder

                l_j2k.m_cp.m_dec = new opj_decoding_param_t();
                l_j2k.m_cp.m_dec.m_layer = parameters.core.cp_layer;
                l_j2k.m_cp.m_dec.m_reduce = parameters.core.cp_reduce;
                
                if (useJPWL) {
                    l_j2k.m_cp.correct = parameters.core.jpwl_correct;
                    l_j2k.m_cp.exp_comps = parameters.core.jpwl_exp_comps;
                    l_j2k.m_cp.max_tiles = parameters.core.jpwl_max_tiles;
                } // if (useJPWL)
                
                // Read the main header of the codestream and if necessary the JP2 boxes
        		// if(! opj_read_header(l_stream, l_codec, &image)){
                // opj_j2k_read_header
                
                /* create an empty image header */
                l_j2k.m_private_image = new opj_image_t();
                
                // customization of the validation 
                // if (! opj_j2k_setup_decoding_validation(p_j2k, p_manager)) {
                //if (! opj_procedure_list_add_procedure(p_j2k->m_validation_list,(opj_procedure)opj_j2k_build_decoder, p_manager)) {
                    //return OPJ_FALSE;
                //}
                //if (! opj_procedure_list_add_procedure(p_j2k->m_validation_list,(opj_procedure)opj_j2k_decoding_validation, p_manager)) {
                    //return OPJ_FALSE;
                //}


                
//                if (l_j2k.m_validation_list.m_nb_max_procedures ==l_j2k.m_validation_list.m_nb_procedures) {
//                	l_j2k.m_validation_list.m_nb_max_procedures += OPJ_VALIDATION_SIZE;
//                	l_j2k.m_validation_list.m_procedures.ensureCapacity(l_j2k.m_validation_list.m_nb_max_procedures);
//                }
//                l_j2k.m_validation_list.m_procedures.add(l_j2k.m_validation_list.m_nb_procedures, OPJ_J2K_BUILD_DECODER);
//                l_j2k.m_validation_list.m_nb_procedures++;
//                
//                if (l_j2k.m_validation_list.m_nb_max_procedures ==l_j2k.m_validation_list.m_nb_procedures) {
//                	l_j2k.m_validation_list.m_nb_max_procedures += OPJ_VALIDATION_SIZE;
//                	l_j2k.m_validation_list.m_procedures.ensureCapacity(l_j2k.m_validation_list.m_nb_max_procedures);
//                }
//                l_j2k.m_validation_list.m_procedures.add(l_j2k.m_validation_list.m_nb_procedures, OPJ_J2K_DECODING_VALIDATION);
//                l_j2k.m_validation_list.m_nb_procedures++;
                
                // validation of the parameters codec
                //if (! opj_j2k_exec(p_j2k, p_j2k->m_validation_list, p_stream,p_manager)) {
                        //opj_image_destroy(p_j2k->m_private_image);
                        //p_j2k->m_private_image = NULL;
                        //return OPJ_FALSE;
                //}

                
//                for (i = 0; i < l_j2k.m_validation_list.m_nb_procedures; i++) {
//                	if (l_j2k.m_validation_list.m_procedures.get(i) == OPJ_J2K_BUILD_DECODER) {
                	  // execute opj_j2k_build_decoder	
//                	}
//                	else if (l_j2k.m_validation_list.m_procedures.get(i) == OPJ_J2K_DECODING_VALIDATION) {
                		// execute opj_j2k_decoding_validation
//                	}
//                }
                
//                l_j2k.m_validation_list.m_procedures.clear();
//                l_j2k.m_validation_list.m_nb_procedures = 0;
                
// opj_j2k_build_decoder says 
//                add here initialization of cp
 //               copy paste of setup_decoder 
 // opj_j2k_setup_decoder has already been done
                
 // opj_j2k_setup_header_reading
 // From opj_j2k_decoding_validation just derived l_j2k.m_decoder.m_state = 0x0000;
                
//              if (l_j2k.m_procedure_list.m_nb_max_procedures ==l_j2k.m_procedure_list.m_nb_procedures) {
//            	l_j2k.m_procedure_list.m_nb_max_procedures += OPJ_VALIDATION_SIZE;
//            	l_j2k.m_procedure_list.m_procedures.ensureCapacity(l_j2k.m_procedure_list.m_nb_max_procedures);
//            }
//            l_j2k.m_procedure_list.m_procedures.add(l_j2k.m_procedure_list.m_nb_procedures, OPJ_J2K_READ_HEADER_PROCEDURE);
//            l_j2k.m_procedure_list.m_nb_procedures++;
//            
//            if (l_j2k.m_procedure_list.m_nb_max_procedures ==l_j2k.m_procedure_list.m_nb_procedures) {
//            	l_j2k.m_procedure_list.m_nb_max_procedures += OPJ_VALIDATION_SIZE;
//            	l_j2k.m_procedure_list.m_procedures.ensureCapacity(l_j2k.m_procedure_list.m_nb_max_procedures);
//            }
//            l_j2k.m_procedure_list.m_procedures.add(l_j2k.m_procedure_list.m_nb_procedures,
//                OPJ_J2K_COPY_DEFAULT_TCP_AND_CREATE_TCD);
//            l_j2k.m_procedure_list.m_nb_procedures++;
            
//            for (i = 0; i < l_j2k.m_procedure_list.m_nb_procedures; i++) {
//            	if (l_j2k.m_procedure_list.m_procedures.get(i) == OPJ_J2K_READ_HEADER_PROCEDURE) {
            	  // execute opj_j2k_read_header_procedure	
//            	}
//            	else if (l_j2k.m_procedure_list.m_procedures.get(i) == OPJ_J2K_COPY_DEFAULT_TCP_AND_CREATE_TCD) {
            		// execute opj_j2k_copy_default_tcp_and_create_tcd
//            	}
//            }
                
                opj_j2k_read_header_procedure(l_j2k, l_stream); 
                
                opj_j2k_copy_default_tcp_and_create_tcd(l_j2k, l_stream);
                
                

            
//            l_j2k.m_procedure_list.m_procedures.clear();
//            l_j2k.m_procedure_list.m_nb_procedures = 0;
                
                image = new opj_image_t();
        		
        		/* Copy codestream image information to the output image */
        		opj_copy_image_header(l_j2k.m_private_image, image);
        		
        		/*Allocate and initialize some elements of codestream index*/
        		if (!opj_j2k_allocate_tile_element_cstr_index(l_j2k)){
        		return false;
        		}
        		
        		if (parameters.nb_tile_to_decode == 0) {
        			/* Optional if you want decode the entire image */
        			if (!opj_j2k_set_decode_area(l_j2k, image, parameters.DA_x0,
        					parameters.DA_y0, parameters.DA_x1, parameters.DA_y1)){
        				MipavUtil.displayError("ERROR -> opj_decompress: failed to set the decoded area");
        				destroy_parameters(parameters);
        				try {
            			    l_stream.close();
            			}
            			catch (IOException e) {
            				MipavUtil.displayError("IOException on l_stream.close()");
            			}
        				opj_image_destroy(image);
        				return false;
        			}

        			/* Get the decoded image */
        			// opj_decode
        			// opj_j2k_end_decompress just does a return true;
        			if (!(opj_j2k_decode(l_j2k, l_stream, image)/* && opj_j2k_end_decompress(l_codec,	l_stream)*/)) {
        				MipavUtil.displayError("ERROR -> opj_decompress: failed to decode image!");
        				destroy_parameters(parameters);
        				try {
            			    l_stream.close();
            			}
            			catch (IOException e) {
            				MipavUtil.displayError("IOException on l_stream.close()");
            			}
        				opj_image_destroy(image);
        				return false;
        			}
        		}
        		else {

        			/* It is just here to illustrate how to use the resolution after set parameters */
        			/*if (!opj_set_decoded_resolution_factor(l_codec, 5)) {
        				fprintf(stderr, "ERROR -> opj_decompress: failed to set the resolution factor tile!\n");
        				opj_destroy_codec(l_codec);
        				opj_stream_destroy(l_stream);
        				opj_image_destroy(image);
        				return EXIT_FAILURE;
        			}*/

        			if (!opj_j2k_get_tile(l_j2k, l_stream, image, parameters.tile_index)) {
        				MipavUtil.displayError("ERROR -> opj_decompress: failed to decode tile!\n");
        				destroy_parameters(parameters);
        				try {
            			    l_stream.close();
            			}
            			catch (IOException e) {
            				MipavUtil.displayError("IOException on l_stream.close()");
            			}
        				opj_image_destroy(image);
        				return false;
        			}
        			Preferences.debug("tile " + parameters.tile_index + " is decoded!\n\n", Preferences.DEBUG_FILEIO);
        		}
               
                
               
                
    		} // if (parameters.decod_format == J2K_CFMT)

    		

    		

    		//tCumulative += opj_clock() - t;
    		numDecompressedImages++;

    		/* Close the byte stream */
    		try {
			    l_stream.close();
			}
			catch (IOException e) {
				MipavUtil.displayError("IOException on l_stream.close()");
				opj_image_destroy(image);
				return false;
			}
			

    		if( image.color_space != OPJ_CLRSPC_SYCC 
    			&& image.numcomps == 3 && image.comps[0].dx == image.comps[0].dy
    			&& image.comps[1].dx != 1 )
    			image.color_space = OPJ_CLRSPC_SYCC;
    		else if (image.numcomps <= 2)
    			image.color_space = OPJ_CLRSPC_GRAY;

    		if(image.color_space == OPJ_CLRSPC_SYCC){
    			color_sycc_to_rgb(image);
    		}
    		else if((image.color_space == OPJ_CLRSPC_CMYK) && (parameters.cod_format != TIF_DFMT)){
    			color_cmyk_to_rgb(image);
    		}
    		else if(image.color_space == OPJ_CLRSPC_EYCC){
    			color_esycc_to_rgb(image);
    		}
    		
    		if (image.icc_profile_buf != null) {
    if (OPJ_HAVE_LIBLCMS1 || OPJ_HAVE_LIBLCMS2) {
    			//if(image.icc_profile_len != 0)
    			 //color_apply_icc_profile(image);
    			//else
    			 //color_cielab_to_rgb(image);
    } // if (OPJ_HAVE_LIBLCMS1 || OPJ_HAVE_LIBLCMS2)
    			image.icc_profile_buf = null;
    			image.icc_profile_len = 0;
    		} // if (image.icc_profile_buf != null)
    		
    		/* Force output precision */
    		/* ---------------------- */
    		if (parameters.precision != null)
    		{
    			int compno;
    			for (compno = 0; compno < image.numcomps; ++compno)
    			{
    				int precno = compno;
    				int prec;
    				
    				if (precno >= parameters.nb_precision) {
    					precno = parameters.nb_precision - 1;
    				}
    				
    				prec = parameters.precision[precno].prec;
    				if (prec == 0) {
    					prec = image.comps[compno].prec;
    				}
    				
    				switch (parameters.precision[precno].mode) {
    					case OPJ_PREC_MODE_CLIP:
    						clip_component(image.comps[compno], prec);
    						break;
    					case OPJ_PREC_MODE_SCALE:
    						scale_component(image.comps[compno], prec);
    						break;
    					default:
    						break;
    				} // switch (parameters.precision[precno].mode)
    				
    			} // for (compno = 0; compno < image.numcomps; ++compno)
    		} // if (parameters.precision != null)
    		
    		/* Upsample components */
    		/* ------------------- */
    		if (upsample)
    		{
    			image = upsample_image_components(image);
    			if (image == null) {
    			    MipavUtil.displayError("opj_decompress: failed to upsample image components!");
    				destroy_parameters(parameters);
    				return false;
    			}
    		} // if (upsample)
    		
    		/* Force RGB output */
    		/* ---------------- */
    		if (forceRGB)
    		{
    			switch (image.color_space) {
    				case OPJ_CLRSPC_SRGB:
    					break;
    				case OPJ_CLRSPC_GRAY:
    					image = convert_gray_to_rgb(image);
    					break;
    				default:
    					MipavUtil.displayError("opj_decompress: don't know how to convert image to RGB colorspace!");
    					opj_image_destroy(image);
    					image = null;
    					break;
    			}
    			if (image == null) {
    				MipavUtil.displayError("opj_decompress: failed to convert to RGB image!");
    				destroy_parameters(parameters);
    				return false;
    			}
    		} // if (forceRGB)

    		/* create output image */
    		/* ------------------- */
    		switch (parameters.cod_format) {
    		case PXM_DFMT:			// PNM PGM PPM 
    			if (!imagetopnm(image, parameters.outfile, splitPNM)) {
                    MipavUtil.displayError("Outfile " + parameters.outfile + " not generated");
            failed = 1;
    			}
    			else {
                    Preferences.debug("Generated Outfile " + parameters.outfile + "\n", Preferences.DEBUG_FILEIO);
    			}
    			break;

    		case PGX_DFMT:			// PGX 
    			if(!imagetopgx(image, parameters.outfile)){
    				MipavUtil.displayError("Outfile " + parameters.outfile + " not generated");
            failed = 1;
    			}
    			else {
    				Preferences.debug("Generated Outfile " + parameters.outfile + "\n", Preferences.DEBUG_FILEIO);
    			}
    			break;

    		case BMP_DFMT:			// BMP
    			if(!imagetobmp(image, parameters.outfile)){
                    MipavUtil.displayError("Outfile " + parameters.outfile + " not generated");
            failed = 1;
    			}
    			else {
                    Preferences.debug("Generated Outfile " + parameters.outfile + "\n", Preferences.DEBUG_FILEIO);
    			}
    			break;
    //#ifdef OPJ_HAVE_LIBTIFF
            // convert.c requires libtiff library functions
    		//case TIF_DFMT:			// TIFF
    			//if(imagetotif(image, parameters.outfile)){
                    //fprintf(stderr,"[ERROR] Outfile %s not generated\n",parameters.outfile);
            //failed = 1;
    			//}
    			//else {
                    //fprintf(stdout,"[INFO] Generated Outfile %s\n",parameters.outfile);
    			//}
    			//break;
    //#endif  OPJ_HAVE_LIBTIFF 
    		case RAW_DFMT:			// RAW big-endian
    			if(!imagetoraw_common(image, parameters.outfile, true)){
                     MipavUtil.displayError("Error generating raw file. Outfile " + parameters.outfile + " not generated");
            failed = 1;
    			}
    			else {
                    Preferences.debug("Generated Outfile " + parameters.outfile + "\n", Preferences.DEBUG_FILEIO);
    			}
    			break;

    		case RAWL_DFMT:			// RAWL little-endian
            	 if(!imagetoraw_common(image, parameters.outfile, false)){
                     MipavUtil.displayError("Error generating rawl file. Outfile " + parameters.outfile + " not generated");
            failed = 1;
    			}
    			else {
                    Preferences.debug("Generated Outfile " + parameters.outfile + "\n", Preferences.DEBUG_FILEIO);
    			}
    			break;
    			

    		case TGA_DFMT:			// TGA
    			if(!imagetotga(image, parameters.outfile)){
    				MipavUtil.displayError("Error generating tga file. Outfile " + parameters.outfile + " not generated");
            failed = 1;
    			}
    			else {
    				Preferences.debug("Generated Outfile " + parameters.outfile + "\n", Preferences.DEBUG_FILEIO);
    			}
    			break;
    //#ifdef OPJ_HAVE_LIBPNG
    	    // convertpng.c file must be linked to png library functions
    		//case PNG_DFMT:			// PNG 
    			//if(imagetopng(image, parameters.outfile)){
                    //fprintf(stderr,"[ERROR] Error generating png file. Outfile %s not generated\n",parameters.outfile);
            //failed = 1;
    			//}
    			//else {
                    //fprintf(stdout,"[INFO] Generated Outfile %s\n",parameters.outfile);
    			//}
    			//break;
    //#endif // OPJ_HAVE_LIBPNG 
    // Can happen if output file is TIFF or PNG
    // and OPJ_HAVE_LIBTIF or OPJ_HAVE_LIBPNG is undefined
    			default:
                    MipavUtil.displayError("Outfile " + parameters.outfile + " not generated");
            failed = 1;
    		} // switch (parameters.cod_format)

    		/* free remaining structures */
    		if (l_codec != null) {
    			opj_destroy_codec(l_codec);
    		}


    		/* free image data structure */
    		opj_image_destroy(image);

    		/* destroy the codestream index */
    		if (parameters.decod_format == J2K_CFMT) {
    		    j2k_destroy_cstr_index(cstr_index);
    		}

    		//if(failed != 0) (void)remove(parameters.outfile); /* ignore return value */
    	}
    	destroy_parameters(parameters);
    	long consumedTime = System.currentTimeMillis() - startTime;
    	Preferences.debug("Time Consumed in milliseconds: " + consumedTime + "\n", Preferences.DEBUG_FILEIO);
    	if (numDecompressedImages != 0) {
    		Preferences.debug("Milliseconds per image in milliseconds = " +  (consumedTime/numDecompressedImages) + "\n",
    				Preferences.DEBUG_FILEIO);
    	}
    	return (failed != 0) ? false: true;
    }
	
	private void opj_destroy_codec(opj_codec_private_t p_codec)
	{
		int i, j, k, m, n, p;
		if (p_codec != null) {
			
            if (p_codec.m_codec != null) {
            	if (p_codec.m_codec.m_decoder != null) {
            		if (p_codec.m_codec.m_decoder.m_default_tcp != null) {
            			p_codec.m_codec.m_decoder.m_default_tcp.rates = null;
            			if (p_codec.m_codec.m_decoder.m_default_tcp.pocs != null) {
            				for (i = 0; i < p_codec.m_codec.m_decoder.m_default_tcp.pocs.length; i++) {
            					p_codec.m_codec.m_decoder.m_default_tcp.pocs[i].progorder = null;	
            					p_codec.m_codec.m_decoder.m_default_tcp.pocs[i] = null;
            				}
            				p_codec.m_codec.m_decoder.m_default_tcp.pocs = null;	
            			}
            			if (p_codec.m_codec.m_decoder.m_default_tcp.ppt_markers != null) {
            				for (i = 0; i < p_codec.m_codec.m_decoder.m_default_tcp.ppt_markers.length; i++) {
            					p_codec.m_codec.m_decoder.m_default_tcp.ppt_markers[i].m_data = null;
            					p_codec.m_codec.m_decoder.m_default_tcp.ppt_markers[i] = null;
                			}
            				p_codec.m_codec.m_decoder.m_default_tcp.ppt_markers = null;	
            			}
            			p_codec.m_codec.m_decoder.m_default_tcp.ppt_data = null;
            			p_codec.m_codec.m_decoder.m_default_tcp.ppt_buffer = null;
            			p_codec.m_codec.m_decoder.m_default_tcp.distoratio = null;
            			if (p_codec.m_codec.m_decoder.m_default_tcp.tccps != null) {
            				for (i = 0; i < p_codec.m_codec.m_decoder.m_default_tcp.tccps.length; i++) {
            					if (p_codec.m_codec.m_decoder.m_default_tcp.tccps[i].stepsizes != null) {
            						for (j = 0; j < p_codec.m_codec.m_decoder.m_default_tcp.tccps[i].stepsizes.length; j++) {
            							 p_codec.m_codec.m_decoder.m_default_tcp.tccps[i].stepsizes[j] = null;	
            						}
            						p_codec.m_codec.m_decoder.m_default_tcp.tccps[i].stepsizes = null;	
            					}
            					p_codec.m_codec.m_decoder.m_default_tcp.tccps[i].prcw = null;
            					p_codec.m_codec.m_decoder.m_default_tcp.tccps[i].prch = null;	
            					p_codec.m_codec.m_decoder.m_default_tcp.tccps[i] = null;	
            				}
            				p_codec.m_codec.m_decoder.m_default_tcp.tccps = null;	
            			}
            			p_codec.m_codec.m_decoder.m_default_tcp.m_data = null;
            			p_codec.m_codec.m_decoder.m_default_tcp.m_mct_decoding_matrix = null;
            			p_codec.m_codec.m_decoder.m_default_tcp.m_mct_coding_matrix = null;
            			if (p_codec.m_codec.m_decoder.m_default_tcp.m_mct_records != null) {
            				for (i = 0; i < p_codec.m_codec.m_decoder.m_default_tcp.m_mct_records.length; i++) {
            					p_codec.m_codec.m_decoder.m_default_tcp.m_mct_records[i].m_data = null;	
            					p_codec.m_codec.m_decoder.m_default_tcp.m_mct_records[i] = null;	
            				}
            				p_codec.m_codec.m_decoder.m_default_tcp.m_mct_records = null;	
            			}
            			if (p_codec.m_codec.m_decoder.m_default_tcp.m_mcc_records != null) {
            				for (i = 0; i < p_codec.m_codec.m_decoder.m_default_tcp.m_mcc_records.length; i++) {
            					if (p_codec.m_codec.m_decoder.m_default_tcp.m_mcc_records[i].m_decorrelation_array != null) {
            						p_codec.m_codec.m_decoder.m_default_tcp.m_mcc_records[i].m_decorrelation_array.m_data = null;	
            						p_codec.m_codec.m_decoder.m_default_tcp.m_mcc_records[i].m_decorrelation_array = null;	
            					}
            					if (p_codec.m_codec.m_decoder.m_default_tcp.m_mcc_records[i].m_offset_array != null) {
            						p_codec.m_codec.m_decoder.m_default_tcp.m_mcc_records[i].m_offset_array.m_data = null;	
            						p_codec.m_codec.m_decoder.m_default_tcp.m_mcc_records[i].m_offset_array = null;	
            					}
            					p_codec.m_codec.m_decoder.m_default_tcp.m_mcc_records[i] = null;	
            				}
            				p_codec.m_codec.m_decoder.m_default_tcp.m_mcc_records = null;	
            			}
            			p_codec.m_codec.m_decoder.m_default_tcp = null;
            		}
            		p_codec.m_codec.m_decoder.m_header_data = null;
            		p_codec.m_codec.m_decoder = null;
            	}
            	if (p_codec.m_codec.m_private_image != null) {
            		if (p_codec.m_codec.m_private_image.comps != null) {
            			for (i = 0; i < p_codec.m_codec.m_private_image.comps.length; i++) {
            				p_codec.m_codec.m_private_image.comps[i].data = null;
            				p_codec.m_codec.m_private_image.comps[i] = null;
            			}
            			p_codec.m_codec.m_private_image.comps = null;
            		}
            		p_codec.m_codec.m_private_image.icc_profile_buf = null;
            		p_codec.m_codec.m_private_image = null;
            	}
            	if (p_codec.m_codec.m_output_image != null) {
            		if (p_codec.m_codec.m_output_image.comps != null) {
            			for (i = 0; i < p_codec.m_codec.m_output_image.comps.length; i++) {
            				p_codec.m_codec.m_output_image.comps[i].data = null;
            				p_codec.m_codec.m_output_image.comps[i] = null;
            			}
            			p_codec.m_codec.m_output_image.comps = null;
            		}
            		p_codec.m_codec.m_output_image.icc_profile_buf = null;
            		p_codec.m_codec.m_output_image = null;
            	}
            	if (p_codec.m_codec.m_cp != null) {
            		if (p_codec.m_codec.m_cp.ppm_markers != null) {
            			for (i = 0; i < p_codec.m_codec.m_cp.ppm_markers.length; i++) {
            				p_codec.m_codec.m_cp.ppm_markers[i].m_data = null;
            				p_codec.m_codec.m_cp.ppm_markers[i] = null;
            			}
            			p_codec.m_codec.m_cp.ppm_markers = null;
            		}
            		p_codec.m_codec.m_cp.ppm_data = null;
            		p_codec.m_codec.m_cp.ppm_data_current = null;
            		p_codec.m_codec.m_cp.ppm_buffer = null;
            		if (p_codec.m_codec.m_cp.tcps != null) {
            			for (i = 0; i < p_codec.m_codec.m_cp.tcps.length; i++) {
            				p_codec.m_codec.m_cp.tcps[i].rates = null;
            				if (p_codec.m_codec.m_cp.tcps[i].pocs != null) {
            					for (j = 0; j < p_codec.m_codec.m_cp.tcps[i].pocs.length; j++) {
            					    p_codec.m_codec.m_cp.tcps[i].pocs[j].progorder = null;
            					    p_codec.m_codec.m_cp.tcps[i].pocs[j] = null;
            					}
            					p_codec.m_codec.m_cp.tcps[i].pocs = null;	
            				}
            				if (p_codec.m_codec.m_cp.tcps[i].ppt_markers != null) {
            					for (j = 0; j < p_codec.m_codec.m_cp.tcps[i].ppt_markers.length; j++) {
            						p_codec.m_codec.m_cp.tcps[i].ppt_markers[j].m_data = null;	
            						p_codec.m_codec.m_cp.tcps[i].ppt_markers[j] = null;	
            					}
            					p_codec.m_codec.m_cp.tcps[i].ppt_markers = null;	
            				}
            				p_codec.m_codec.m_cp.tcps[i].ppt_data = null;
            				p_codec.m_codec.m_cp.tcps[i].ppt_buffer = null;	
            				p_codec.m_codec.m_cp.tcps[i].distoratio = null;	
            				if (p_codec.m_codec.m_cp.tcps[i].tccps != null) {
            					for (j = 0; j < p_codec.m_codec.m_cp.tcps[i].tccps.length; j++) {
            						if (p_codec.m_codec.m_cp.tcps[i].tccps[j].stepsizes != null) {
            							for (k = 0; k < p_codec.m_codec.m_cp.tcps[i].tccps[j].stepsizes.length; k++) {
            								p_codec.m_codec.m_cp.tcps[i].tccps[j].stepsizes[k] = null;	
            							}
            							p_codec.m_codec.m_cp.tcps[i].tccps[j].stepsizes = null;	
            						}
            						p_codec.m_codec.m_cp.tcps[i].tccps[j].prcw = null;	
            						p_codec.m_codec.m_cp.tcps[i].tccps[j].prch = null;	
            						p_codec.m_codec.m_cp.tcps[i].tccps[j] = null;		
            					}
            					p_codec.m_codec.m_cp.tcps[i].tccps = null;	
            				}
            				p_codec.m_codec.m_cp.tcps[i].m_data = null;
            				p_codec.m_codec.m_cp.tcps[i].mct_norms = null;
            				p_codec.m_codec.m_cp.tcps[i].m_mct_decoding_matrix = null;
            				p_codec.m_codec.m_cp.tcps[i].m_mct_coding_matrix = null;
            				if (p_codec.m_codec.m_cp.tcps[i].m_mct_records != null) {
            					for (j = 0; j < p_codec.m_codec.m_cp.tcps[i].m_mct_records.length; j++) {
            						p_codec.m_codec.m_cp.tcps[i].m_mct_records[j].m_data = null;
            						p_codec.m_codec.m_cp.tcps[i].m_mct_records[j] = null;
            					}
            					p_codec.m_codec.m_cp.tcps[i].m_mct_records = null;	
            				}
            				if (p_codec.m_codec.m_cp.tcps[i].m_mcc_records != null) {
            					for (j = 0; j < p_codec.m_codec.m_cp.tcps[i].m_mcc_records.length; j++) {
            						if (p_codec.m_codec.m_cp.tcps[i].m_mcc_records[j].m_decorrelation_array != null) {
            							p_codec.m_codec.m_cp.tcps[i].m_mcc_records[j].m_decorrelation_array.m_data = null;	
            							p_codec.m_codec.m_cp.tcps[i].m_mcc_records[j].m_decorrelation_array = null;	
            						}
            						if (p_codec.m_codec.m_cp.tcps[i].m_mcc_records[j].m_offset_array != null) {
            							p_codec.m_codec.m_cp.tcps[i].m_mcc_records[j].m_offset_array.m_data = null;
            							p_codec.m_codec.m_cp.tcps[i].m_mcc_records[j].m_offset_array = null;	
            						}
            						p_codec.m_codec.m_cp.tcps[i].m_mcc_records[j] = null;	
            					}
            					p_codec.m_codec.m_cp.tcps[i].m_mcc_records = null;	
            				}
            				p_codec.m_codec.m_cp.tcps[i] = null;	
            			}
            			p_codec.m_codec.m_cp.tcps = null;	
            		}
            		if (p_codec.m_codec.m_cp.m_dec != null) {
            			p_codec.m_codec.m_cp.m_dec = null;	
            		}
            		if (p_codec.m_codec.m_cp.m_enc != null) {
            			p_codec.m_codec.m_cp.m_enc = null;	
            		}
            		p_codec.m_codec.m_cp.hprot_TPH_tileno = null;
            		p_codec.m_codec.m_cp.hprot_TPH = null;
            		p_codec.m_codec.m_cp.pprot_tileno = null;
            		p_codec.m_codec.m_cp.pprot_packno = null;
            		p_codec.m_codec.m_cp.pprot = null;
            		p_codec.m_codec.m_cp.sens_TPH_tileno = null;
            		p_codec.m_codec.m_cp.sens_TPH = null;
            		p_codec.m_codec.m_cp = null;
            	}
            	if (p_codec.m_codec.m_procedure_list != null) {
            		p_codec.m_codec.m_procedure_list = null;
            	}
            	if (p_codec.m_codec.m_validation_list != null) {
            		p_codec.m_codec.m_validation_list = null;
            	}
            	if (p_codec.m_codec.cstr_index != null) {
            		j2k_destroy_cstr_index(p_codec.m_codec.cstr_index);
            	}
            	p_codec.m_codec.m_current_tile_number = null;
            	if (p_codec.m_codec.m_tcd != null) {
            		if (p_codec.m_codec.m_tcd.tcd_image != null) {
            			if (p_codec.m_codec.m_tcd.tcd_image.tiles != null) {
            				if (p_codec.m_codec.m_tcd.tcd_image.tiles.comps != null) {
            					for (i = 0; i < p_codec.m_codec.m_tcd.tcd_image.tiles.comps.length; i++) {
            						if (p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions != null) {
            							for (j = 0; j < p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions.length; j++) {
            								if (p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands != null) {
            									for (k = 0; k < p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands.length; k++) {
            										if (p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts != null) {
            											for (m = 0; m < p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts.length; m++) {
            												if (p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].dec != null) {
            													for (n = 0; n < p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].dec.length; n++) {
            														p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].dec[n].data = null;
            														if (p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].dec[n].segs != null) {
            															for (p = 0; p < p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].dec[n].segs.length; p++) {
            																p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].dec[n].segs[p].data = null;
            																p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].dec[n].segs[p] = null;	
            															}
            															p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].dec[n].segs = null;	
            														}
            														p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].dec[n] = null;	
            													}
            													p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].dec = null;	
            												}
            												if (p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].incltree != null) {
            													opj_tgt_destroy(p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].incltree);	
            												}
            												if (p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].imsbtree != null) {
            													opj_tgt_destroy(p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m].imsbtree);	
            												}
            												p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts[m] = null;	
            											}
            											p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k].precincts = null;
            										}
            										p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands[k] = null;	
            									}
            									p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j].bands = null;	
            								}
            								p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions[j] = null;		
            							}
            							p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].resolutions = null;	
            						}
            						p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i].data = null;
            						p_codec.m_codec.m_tcd.tcd_image.tiles.comps[i] = null;	
            					}
            					p_codec.m_codec.m_tcd.tcd_image.tiles.comps = null;	
            				}
            				p_codec.m_codec.m_tcd.tcd_image.tiles.distolayer = null;
            				p_codec.m_codec.m_tcd.tcd_image.tiles = null;	
            			}
            			p_codec.m_codec.m_tcd.tcd_image = null;	
            		}
            		if (p_codec.m_codec.m_tcd.image != null) {
            			if (p_codec.m_codec.m_tcd.image.comps != null) {
                			for (i = 0; i < p_codec.m_codec.m_tcd.image.comps.length; i++) {
                				p_codec.m_codec.m_tcd.image.comps[i].data = null;
                				p_codec.m_codec.m_tcd.image.comps[i] = null;
                			}
                			p_codec.m_codec.m_tcd.image.comps = null;
                		}
                		p_codec.m_codec.m_tcd.image.icc_profile_buf = null;
            			p_codec.m_codec.m_tcd.image = null;	
            		}
            		if (p_codec.m_codec.m_tcd.cp != null) {
            			if (p_codec.m_codec.m_tcd.cp.ppm_markers != null) {
            				for (i = 0; i < p_codec.m_codec.m_tcd.cp.ppm_markers.length; i++) {
            					p_codec.m_codec.m_tcd.cp.ppm_markers[i].m_data = null;	
            					p_codec.m_codec.m_tcd.cp.ppm_markers[i] = null;		
            				}
            				p_codec.m_codec.m_tcd.cp.ppm_markers = null;
            			}
        				p_codec.m_codec.m_tcd.cp.ppm_data = null;
        				p_codec.m_codec.m_tcd.cp.ppm_data_current = null;
        				p_codec.m_codec.m_tcd.cp.ppm_buffer = null;
        				if (p_codec.m_codec.m_tcd.cp.tcps != null) {
        					for (i = 0; i < p_codec.m_codec.m_tcd.cp.tcps.length; i++) {
        						p_codec.m_codec.m_tcd.cp.tcps[i].rates = null;
        						if (p_codec.m_codec.m_tcd.cp.tcps[i].pocs != null) {
        							for (j = 0; j < p_codec.m_codec.m_tcd.cp.tcps[i].pocs.length; j++) {
        								p_codec.m_codec.m_tcd.cp.tcps[i].pocs[j].progorder = null;
        								p_codec.m_codec.m_tcd.cp.tcps[i].pocs[j] = null;	
        							}
        							p_codec.m_codec.m_tcd.cp.tcps[i].pocs = null;	
        						}
        						if (p_codec.m_codec.m_tcd.cp.tcps[i].ppt_markers != null) {
        							for (j = 0; j < p_codec.m_codec.m_tcd.cp.tcps[i].ppt_markers.length; j++) {
        								p_codec.m_codec.m_tcd.cp.tcps[i].ppt_markers[j].m_data = null;	
        								p_codec.m_codec.m_tcd.cp.tcps[i].ppt_markers[j] = null;	
        							}
        							p_codec.m_codec.m_tcd.cp.tcps[i].ppt_markers = null;	
        						}
        						p_codec.m_codec.m_tcd.cp.tcps[i].ppt_data = null;
        						p_codec.m_codec.m_tcd.cp.tcps[i].ppt_buffer = null;
        						p_codec.m_codec.m_tcd.cp.tcps[i].distoratio = null;	
        						if (p_codec.m_codec.m_tcd.cp.tcps[i].tccps != null) {
        							for (j = 0; j < p_codec.m_codec.m_tcd.cp.tcps[i].tccps.length; j++) {
        								if (p_codec.m_codec.m_tcd.cp.tcps[i].tccps[j].stepsizes != null) {
        									for (k = 0; k < p_codec.m_codec.m_tcd.cp.tcps[i].tccps[j].stepsizes.length; k++) {
        										p_codec.m_codec.m_tcd.cp.tcps[i].tccps[j].stepsizes[k] = null;		
        									}
        									p_codec.m_codec.m_tcd.cp.tcps[i].tccps[j].stepsizes = null;	
        								}
        								p_codec.m_codec.m_tcd.cp.tcps[i].tccps[j].prcw = null;
        								p_codec.m_codec.m_tcd.cp.tcps[i].tccps[j].prch = null;
        								p_codec.m_codec.m_tcd.cp.tcps[i].tccps[j] = null;	
        							}
        							p_codec.m_codec.m_tcd.cp.tcps[i].tccps = null;	
        						}
        						p_codec.m_codec.m_tcd.cp.tcps[i].m_data = null;
        						p_codec.m_codec.m_tcd.cp.tcps[i].mct_norms = null;
        						p_codec.m_codec.m_tcd.cp.tcps[i].m_mct_decoding_matrix = null;	
        						p_codec.m_codec.m_tcd.cp.tcps[i].m_mct_coding_matrix = null;
        						if (p_codec.m_codec.m_tcd.cp.tcps[i].m_mct_records != null) {
        							for (j = 0; j < p_codec.m_codec.m_tcd.cp.tcps[i].m_mct_records.length; j++) {
        								p_codec.m_codec.m_tcd.cp.tcps[i].m_mct_records[j].m_data = null;
        								p_codec.m_codec.m_tcd.cp.tcps[i].m_mct_records[j] = null;	
        							}
        							p_codec.m_codec.m_tcd.cp.tcps[i].m_mct_records = null;
        						}
        						if (p_codec.m_codec.m_tcd.cp.tcps[i].m_mcc_records != null) {
        							for (j = 0; j < p_codec.m_codec.m_tcd.cp.tcps[i].m_mcc_records.length; j++) {
        								if (p_codec.m_codec.m_tcd.cp.tcps[i].m_mcc_records[j].m_decorrelation_array != null) {
        									p_codec.m_codec.m_tcd.cp.tcps[i].m_mcc_records[j].m_decorrelation_array.m_data = null;	
        									p_codec.m_codec.m_tcd.cp.tcps[i].m_mcc_records[j].m_decorrelation_array = null;	
        								}
        								if (p_codec.m_codec.m_tcd.cp.tcps[i].m_mcc_records[j].m_offset_array != null) {
        									p_codec.m_codec.m_tcd.cp.tcps[i].m_mcc_records[j].m_offset_array.m_data = null;	
        									p_codec.m_codec.m_tcd.cp.tcps[i].m_mcc_records[j].m_offset_array = null;	
        								}
        								p_codec.m_codec.m_tcd.cp.tcps[i].m_mcc_records[j] = null;	
        							}
        							p_codec.m_codec.m_tcd.cp.tcps[i].m_mcc_records = null;
        						}
        						p_codec.m_codec.m_tcd.cp.tcps[i] = null;		
        					}
        					p_codec.m_codec.m_tcd.cp.tcps = null;	
        				}
        				if (p_codec.m_codec.m_tcd.cp.m_dec != null) {
        					p_codec.m_codec.m_tcd.cp.m_dec = null;
        				}
        				if (p_codec.m_codec.m_tcd.cp.m_enc != null) {
        					p_codec.m_codec.m_tcd.cp.m_enc = null;
        				}
        				p_codec.m_codec.m_tcd.cp.hprot_TPH_tileno = null;
        				p_codec.m_codec.m_tcd.cp.hprot_TPH = null;
        				p_codec.m_codec.m_tcd.cp.pprot_tileno = null;
        				p_codec.m_codec.m_tcd.cp.pprot_packno = null;
        				p_codec.m_codec.m_tcd.cp.pprot = null;
        				p_codec.m_codec.m_tcd.cp.sens_TPH_tileno = null;
        				p_codec.m_codec.m_tcd.cp.sens_TPH = null;
            			p_codec.m_codec.m_tcd.cp = null;	
            		}
            		if (p_codec.m_codec.m_tcd.tcp != null) {
            			p_codec.m_codec.m_tcd.tcp.rates = null;
            			if (p_codec.m_codec.m_tcd.tcp.pocs != null) {
            				for (i = 0; i < p_codec.m_codec.m_tcd.tcp.pocs.length; i++) {
            					p_codec.m_codec.m_tcd.tcp.pocs[i].progorder = null;
            					p_codec.m_codec.m_tcd.tcp.pocs[i] = null;	
            				}
            				p_codec.m_codec.m_tcd.tcp.pocs = null;
            			}
            			if (p_codec.m_codec.m_tcd.tcp.ppt_markers != null) {
            				for (i = 0; i < p_codec.m_codec.m_tcd.tcp.ppt_markers.length; i++) {
            					p_codec.m_codec.m_tcd.tcp.ppt_markers[i].m_data = null; 
            					p_codec.m_codec.m_tcd.tcp.ppt_markers[i] = null;    	
            				}
            				p_codec.m_codec.m_tcd.tcp.ppt_markers = null; 
            			}
            			p_codec.m_codec.m_tcd.tcp.ppt_data = null;
            			p_codec.m_codec.m_tcd.tcp.ppt_buffer = null;
            			p_codec.m_codec.m_tcd.tcp.distoratio = null;
            			if (p_codec.m_codec.m_tcd.tcp.tccps != null) {
            				for (i = 0; i < p_codec.m_codec.m_tcd.tcp.tccps.length; i++) {
            					if (p_codec.m_codec.m_tcd.tcp.tccps[i].stepsizes != null) {
            						for (j = 0; j < p_codec.m_codec.m_tcd.tcp.tccps[i].stepsizes.length; j++) {
            							p_codec.m_codec.m_tcd.tcp.tccps[i].stepsizes[j] = null;	
            						}
            						p_codec.m_codec.m_tcd.tcp.tccps[i].stepsizes = null;	
            					}
            					p_codec.m_codec.m_tcd.tcp.tccps[i].prcw = null;
            					p_codec.m_codec.m_tcd.tcp.tccps[i].prch = null;
            					p_codec.m_codec.m_tcd.tcp.tccps[i] = null;	
            				}
            				p_codec.m_codec.m_tcd.tcp.tccps = null;	
            			}
            			p_codec.m_codec.m_tcd.tcp.m_data = null;
            			p_codec.m_codec.m_tcd.tcp.mct_norms = null;
            			p_codec.m_codec.m_tcd.tcp.m_mct_decoding_matrix = null;
            			p_codec.m_codec.m_tcd.tcp.m_mct_coding_matrix = null;
            			if (p_codec.m_codec.m_tcd.tcp.m_mct_records != null) {
            				for (i = 0; i < p_codec.m_codec.m_tcd.tcp.m_mct_records.length; i++) {
            					p_codec.m_codec.m_tcd.tcp.m_mct_records[i].m_data = null;	
            					p_codec.m_codec.m_tcd.tcp.m_mct_records[i] = null;		
            				}
            				p_codec.m_codec.m_tcd.tcp.m_mct_records = null;	
            			}
            			if (p_codec.m_codec.m_tcd.tcp.m_mcc_records != null) {
            				for (i = 0; i < p_codec.m_codec.m_tcd.tcp.m_mcc_records.length; i++) {
            					if (p_codec.m_codec.m_tcd.tcp.m_mcc_records[i].m_decorrelation_array != null) {
            						p_codec.m_codec.m_tcd.tcp.m_mcc_records[i].m_decorrelation_array.m_data = null;
            						p_codec.m_codec.m_tcd.tcp.m_mcc_records[i].m_decorrelation_array = null;	
            					}
            					if (p_codec.m_codec.m_tcd.tcp.m_mcc_records[i].m_offset_array != null) {
            						p_codec.m_codec.m_tcd.tcp.m_mcc_records[i].m_offset_array.m_data = null;
            						p_codec.m_codec.m_tcd.tcp.m_mcc_records[i].m_offset_array = null;	
            					}
            					p_codec.m_codec.m_tcd.tcp.m_mcc_records[i] = null;	
            				}
            				p_codec.m_codec.m_tcd.tcp.m_mcc_records = null;		
            			}
            			p_codec.m_codec.m_tcd.tcp = null;	
            		}
            		p_codec.m_codec.m_tcd = null;
            	}
			    p_codec.m_codec = null;
            }
			p_codec = null;
		}
	}


	/* end opj_decompress_main */

	private void j2k_destroy_cstr_index(opj_codestream_index_t p_cstr_ind) {
		if (p_cstr_ind != null) {

			if (p_cstr_ind.marker != null) {
				p_cstr_ind.marker = null;
			}

			if (p_cstr_ind.tile_index != null) {
				int it_tile = 0;

				for (it_tile = 0; it_tile < p_cstr_ind.nb_of_tiles; it_tile++) {

					if (p_cstr_ind.tile_index[it_tile].packet_index != null) {
						p_cstr_ind.tile_index[it_tile].packet_index = null;
					}

					if (p_cstr_ind.tile_index[it_tile].tp_index != null) {
						p_cstr_ind.tile_index[it_tile].tp_index = null;
					}

					if (p_cstr_ind.tile_index[it_tile].marker != null) {
						p_cstr_ind.tile_index[it_tile].marker = null;

					}
				}

				p_cstr_ind.tile_index = null;
			}

			p_cstr_ind = null;
		}
	}

	private boolean opj_j2k_decode(opj_j2k_t p_j2k, RandomAccessFile p_stream,
			opj_image_t p_image) {
		int compno;
		boolean dodebug = false;
		int i;
		int imageData;

		if (p_image == null)
			return false;

		p_j2k.m_output_image = new opj_image_t();

		opj_copy_image_header(p_image, p_j2k.m_output_image);

		/* Decode the codestream */
		if (!opj_j2k_decode_tiles(p_j2k, p_stream)) {
			opj_image_destroy(p_j2k.m_private_image);
			p_j2k.m_private_image = null;
			return false;
		}

		/* Move data and copy one information from codec to output image */
		for (compno = 0; compno < p_image.numcomps; compno++) {
			p_image.comps[compno].resno_decoded = p_j2k.m_output_image.comps[compno].resno_decoded;
			p_image.comps[compno].data = p_j2k.m_output_image.comps[compno].data;
			if (dodebug) {
				File file = new File("C:/tmp/" + String.valueOf(compno)
						+ ".raw");
				try {
					raFile = new RandomAccessFile(file, "rw");
				} catch (FileNotFoundException e) {
					MipavUtil.displayError("FileNotFoundException " + e
							+ " raFile = new RandomAccessFile");
					return false;
				}
				try {
					raFile.setLength(0);
				} catch (IOException e) {
					MipavUtil.displayError("IOException " + e
							+ " on raFile.setLength(0)");
					return false;
				}
				for (i = 0; i < p_image.comps[compno].w
						* p_image.comps[compno].h; i++) {
					imageData = p_image.comps[compno].data[i];
					try {
						raFile.write(imageData);
					} catch (IOException e) {
						MipavUtil.displayError("IOException " + e
								+ " on raFile.write(imageData)");
						return false;
					}
				}
				try {
					raFile.close();
				} catch (IOException e) {
					MipavUtil.displayError("IOException " + e
							+ " on raFile.close()");
					return false;
				}
			} // if (dodebug)
			p_j2k.m_output_image.comps[compno].data = null;
		}

		return true;
	}

	private boolean opj_j2k_decode_tiles(opj_j2k_t p_j2k,
			RandomAccessFile p_stream) {
		boolean l_go_on[] = new boolean[] { true };
		int l_current_tile_no[] = new int[1];
		int l_data_size[] = new int[1];
		int l_max_data_size;
		int l_tile_x0[] = new int[1];
		int l_tile_y0[] = new int[1];
		int l_tile_x1[] = new int[1];
		int l_tile_y1[] = new int[1];
		int l_nb_comps[] = new int[1];
		byte l_current_data[];
		int l_current_data_ptr[] = new int[1];
		int nr_tiles = 0;
		int i;

		l_current_data = new byte[1000];

		l_max_data_size = 1000;

		for (;;) {
			if (!opj_j2k_read_tile_header(p_j2k, l_current_tile_no,
					l_data_size, l_tile_x0, l_tile_y0, l_tile_x1, l_tile_y1,
					l_nb_comps, l_go_on, p_stream)) {
				l_current_data = null;
				return false;
			}

			if (!l_go_on[0]) {
				break;
			}

			if (l_data_size[0] > l_max_data_size) {
				byte l_new_current_data[] = new byte[l_data_size[0]];
				for (i = 0; i < l_max_data_size; i++) {
					l_new_current_data[i] = l_current_data[i];
				}

				l_current_data = l_new_current_data;
				l_max_data_size = l_data_size[0];
			}

			if (!opj_j2k_decode_tile(p_j2k, l_current_tile_no[0],
					l_current_data, l_current_data_ptr, l_data_size[0],
					p_stream)) {
				l_current_data = null;
				MipavUtil.displayError("Failed to decode tile "
						+ (l_current_tile_no[0] + 1) + "/"
						+ (p_j2k.m_cp.th * p_j2k.m_cp.tw));
				return false;
			}
			Preferences.debug("Tile " + (l_current_tile_no[0] + 1) + "/"
					+ (p_j2k.m_cp.th * p_j2k.m_cp.tw) + " has been decoded.\n",
					Preferences.DEBUG_FILEIO);

			l_current_data_ptr[0] = 0;
			if (!opj_j2k_update_image_data(p_j2k.m_tcd, l_current_data,
					l_current_data_ptr, p_j2k.m_output_image)) {
				l_current_data = null;
				return false;
			}
			Preferences.debug("Image data has been updated with tile "
					+ (l_current_tile_no[0] + 1) + ".\n\n",
					Preferences.DEBUG_FILEIO);

			if (opj_stream_get_number_byte_left(p_stream) == 0
					&& p_j2k.m_decoder.m_state == J2K_STATE_NEOC)
				break;
			if (++nr_tiles == p_j2k.m_cp.th * p_j2k.m_cp.tw)
				break;
		}

		l_current_data = null;

		return true;
	}

	private long opj_stream_get_number_byte_left(RandomAccessFile p_stream) {
		long filePointer;
		long fileLength;

		try {
			filePointer = p_stream.getFilePointer();
		} catch (IOException e) {
			MipavUtil.displayError("IOException " + e
					+ " on filePointer = p_stream.getFilePointer()");
			return -1;
		}
		try {
			fileLength = p_stream.length();
		} catch (IOException e) {
			MipavUtil.displayError("IOException " + e
					+ " on fileLength = p_stream.length()");
			return -1;
		}
		return (fileLength - filePointer);
	}

	private boolean opj_j2k_read_tile_header(opj_j2k_t p_j2k,
			int p_tile_index[], int p_data_size[], int p_tile_x0[],
			int p_tile_y0[], int p_tile_x1[], int p_tile_y1[],
			int p_nb_comps[], boolean p_go_on[], RandomAccessFile p_stream) {
		int l_current_marker = J2K_MS_SOT;
		int l_marker_size[] = new int[1];
		opj_dec_memory_marker_handler_t l_marker_handler = null;
		opj_tcp_t l_tcp = null;
		int bytesRead = 0;
		int bytesSkipped = 0;
		int i;
		long filePointer;

		/* preconditions */
		if (p_stream == null) {
			MipavUtil
					.displayError("p_stream == null at opj_j2k_read_tile_header entry");
			return false;
		}
		if (p_j2k == null) {
			MipavUtil
					.displayError("p_j2k == null at opj_j2k_read_tile_header entry");
			return false;
		}

		/* Reach the End Of Codestream ? */
		if (p_j2k.m_decoder.m_state == J2K_STATE_EOC) {
			l_current_marker = J2K_MS_EOC;
		}
		/* We need to encounter a SOT marker (a new tile-part header) */
		else if (p_j2k.m_decoder.m_state != J2K_STATE_TPHSOT) {
			return false;
		}

		/*
		 * Read into the codestream until reach the EOC or ! can_decode ???
		 * FIXME
		 */
		while ((!p_j2k.m_decoder.m_can_decode)
				&& (l_current_marker != J2K_MS_EOC)) {

			/* Try to read until the Start Of Data is detected */
			while (l_current_marker != J2K_MS_SOD) {

				if (opj_stream_get_number_byte_left(p_stream) == 0) {

					p_j2k.m_decoder.m_state = J2K_STATE_NEOC;
					break;
				}

				/*
				 * Try to read 2 bytes (the marker size) from stream and copy
				 * them into the buffer
				 */
				try {
					bytesRead = p_stream.read(p_j2k.m_decoder.m_header_data, 0,
							2);
				} catch (IOException e) {
					MipavUtil.displayError("IOException " + e
							+ " on p_stream.read");
					return false;
				}
				if (bytesRead != 2) {
					MipavUtil.displayError("RandomAccessFile too short");
					return false;
				}

				/* Read 2 bytes from the buffer as the marker size */
				l_marker_size[0] = getBufferUShort(
						p_j2k.m_decoder.m_header_data, 0, endianess);

				/*
				 * Check marker size (does not include marker ID but includes
				 * marker size)
				 */
				if (l_marker_size[0] < 2) {
					MipavUtil.displayError("Inconsistent marker size");
					return false;
				}

				/* cf. https://code.google.com/p/openjpeg/issues/detail?id=226 */
				if (l_current_marker == 0x8080
						&& opj_stream_get_number_byte_left(p_stream) == 0) {
					p_j2k.m_decoder.m_state = J2K_STATE_NEOC;
					break;
				}

				/* Why this condition? FIXME */
				if ((p_j2k.m_decoder.m_state & J2K_STATE_TPH) != 0) {
					p_j2k.m_decoder.m_sot_length -= (l_marker_size[0] + 2);
				}
				l_marker_size[0] -= 2; /*
										 * Subtract the size of the marker ID
										 * already read
										 */

				/* Get the marker handler from the marker ID */
				l_marker_handler = opj_j2k_get_marker_handler(l_current_marker);

				/*
				 * Check if the marker is known and if it is the right place to
				 * find it
				 */
				if ((p_j2k.m_decoder.m_state & l_marker_handler.states) == 0) {
					MipavUtil
							.displayError("Marker is not compliant with its position");
					return false;
				}
				/* FIXME manage case of unknown marker as in the main header ? */

				/*
				 * Check if the marker size is compatible with the header data
				 * size
				 */
				if (l_marker_size[0] > p_j2k.m_decoder.m_header_data_size) {
					byte new_header_data[] = null;
					/*
					 * If we are here, this means we consider this marker as
					 * known & we will read it
					 */
					/* Check enough bytes left in stream before allocation */
					if (l_marker_size[0] > opj_stream_get_number_byte_left(p_stream)) {
						MipavUtil
								.displayError("Marker size inconsistent with stream length");
						return false;
					}
					new_header_data = new byte[l_marker_size[0]];
					for (i = 0; i < p_j2k.m_decoder.m_header_data_size; i++) {
						new_header_data[i] = p_j2k.m_decoder.m_header_data[i];
					}

					p_j2k.m_decoder.m_header_data = new_header_data;
					p_j2k.m_decoder.m_header_data_size = l_marker_size[0];
				}

				/*
				 * Try to read the rest of the marker segment from stream and
				 * copy them into the buffer
				 */
				try {
					bytesRead = p_stream.read(p_j2k.m_decoder.m_header_data, 0,
							l_marker_size[0]);
				} catch (IOException e) {
					MipavUtil.displayError("IOException " + e
							+ " on p_stream.read");
					return false;
				}
				if (bytesRead != l_marker_size[0]) {
					MipavUtil.displayError("RandomAccessFile too short");
					return false;
				}

				if (l_marker_handler.handler == 0) {
					/* See issue #175 */
					MipavUtil.displayError("l_marker_handler.handler == 0");
					return false;
				}
				/* Read the marker segment with the correct marker handler */
				if (!readMarkerHandler(l_marker_handler.handler, p_j2k,
						p_j2k.m_decoder.m_header_data, l_marker_size[0])) {
					MipavUtil
							.displayError("Failed to read the current marker segment = "
									+ l_current_marker);
					return false;
				}

				/* Add the marker to the codestream index */
				try {
					filePointer = p_stream.getFilePointer();
				} catch (IOException e) {
					MipavUtil.displayError("IOException " + e
							+ " on p_stream.getFilePointer()");
					return false;
				}
				if (false == opj_j2k_add_tlmarker(
						p_j2k.m_current_tile_number[0], p_j2k.cstr_index,
						l_marker_handler.id,
						filePointer - l_marker_size[0] - 4,
						l_marker_size[0] + 4)) {
					MipavUtil
							.displayError("Not enough memory to add tl marker");
					return false;
				}

				/* Keep the position of the last SOT marker read */
				try {
					filePointer = p_stream.getFilePointer();
				} catch (IOException e) {
					MipavUtil.displayError("IOException " + e
							+ " on p_stream.getFilePointer()");
					return false;
				}
				if (l_marker_handler.id == J2K_MS_SOT) {
					int sot_pos = (int) filePointer - l_marker_size[0] - 4;
					if (sot_pos > p_j2k.m_decoder.m_last_sot_read_pos) {
						p_j2k.m_decoder.m_last_sot_read_pos = sot_pos;
					}
				}

				if (p_j2k.m_decoder.m_skip_data) {
					/* Skip the rest of the tile part header */
					try {
						bytesSkipped = p_stream
								.skipBytes(p_j2k.m_decoder.m_sot_length);
					} catch (IOException e) {
						MipavUtil.displayError("IOException " + e
								+ " on p_stream.skipBytes");
						return false;
					}
					if (bytesSkipped != p_j2k.m_decoder.m_sot_length) {
						MipavUtil.displayError("RandomAccessFile too short");
						return false;
					}
					l_current_marker = J2K_MS_SOD; /* Normally we reached a SOD */
				} else {
					/*
					 * Try to read 2 bytes (the next marker ID) from stream and
					 * copy them into the buffer
					 */
					try {
						bytesRead = p_stream.read(
								p_j2k.m_decoder.m_header_data, 0, 2);
					} catch (IOException e) {
						MipavUtil.displayError("IOException " + e
								+ " on p_stream.read");
						return false;
					}
					if (bytesRead != 2) {
						MipavUtil.displayError("RandomAccessFile too short");
						return false;
					}
					/* Read 2 bytes from the buffer as the new marker ID */
					l_current_marker = getBufferUShort(
							p_j2k.m_decoder.m_header_data, 0, endianess);
				}
			}
			if (opj_stream_get_number_byte_left(p_stream) == 0
					&& p_j2k.m_decoder.m_state == J2K_STATE_NEOC)
				break;

			/* If we didn't skip data before, we need to read the SOD marker */
			if (!p_j2k.m_decoder.m_skip_data) {
				/* Try to read the SOD marker and skip data ? FIXME */
				if (!opj_j2k_read_sod(p_j2k, p_stream)) {
					return false;
				}
				if (p_j2k.m_decoder.m_can_decode
						&& (p_j2k.m_decoder.m_nb_tile_parts_correction_checked == 0)) {
					/* Issue 254 */
					boolean l_correction_needed[] = new boolean[1];

					p_j2k.m_decoder.m_nb_tile_parts_correction_checked = 1;
					if (!opj_j2k_need_nb_tile_parts_correction(p_stream,
							p_j2k.m_current_tile_number[0], l_correction_needed)) {
						MipavUtil
								.displayError("opj_j2k_apply_nb_tile_parts_correction error");
						return false;
					}
					if (l_correction_needed[0]) {
						int l_nb_tiles = p_j2k.m_cp.tw * p_j2k.m_cp.th;
						int l_tile_no;

						p_j2k.m_decoder.m_can_decode = false;
						p_j2k.m_decoder.m_nb_tile_parts_correction = 1;
						/* correct tiles */
						for (l_tile_no = 0; l_tile_no < l_nb_tiles; ++l_tile_no) {
							if (p_j2k.m_cp.tcps[l_tile_no].m_nb_tile_parts != 0) {
								p_j2k.m_cp.tcps[l_tile_no].m_nb_tile_parts += 1;
							}
						}
						Preferences.debug(
								"Non conformant codestream TPsot==TNsot.\n",
								Preferences.DEBUG_FILEIO);
					}
				}
				if (!p_j2k.m_decoder.m_can_decode) {
					/*
					 * Try to read 2 bytes (the next marker ID) from stream and
					 * copy them into the buffer
					 */
					try {
						bytesRead = p_stream.read(
								p_j2k.m_decoder.m_header_data, 0, 2);
					} catch (IOException e) {
						MipavUtil.displayError("IOException " + e
								+ " on p_stream.read");
						return false;
					}
					if (bytesRead != 2) {
						MipavUtil.displayError("RandomAccessFile too short");
						return false;
					}

					/* Read 2 bytes from buffer as the new marker ID */
					l_current_marker = getBufferUShort(
							p_j2k.m_decoder.m_header_data, 0, endianess);
				}
			} else {
				/* Indicate we will try to read a new tile-part header */
				p_j2k.m_decoder.m_skip_data = false;
				p_j2k.m_decoder.m_can_decode = false;
				p_j2k.m_decoder.m_state = J2K_STATE_TPHSOT;

				/*
				 * Try to read 2 bytes (the next marker ID) from stream and copy
				 * them into the buffer
				 */
				try {
					bytesRead = p_stream.read(p_j2k.m_decoder.m_header_data, 0,
							2);
				} catch (IOException e) {
					MipavUtil.displayError("IOException " + e
							+ " on p_stream.read");
					return false;
				}
				if (bytesRead != 2) {
					MipavUtil.displayError("RandomAccessFile too short");
					return false;
				}

				/* Read 2 bytes from buffer as the new marker ID */
				l_current_marker = getBufferUShort(
						p_j2k.m_decoder.m_header_data, 0, endianess);
			}
		}

		/* Current marker is the EOC marker ? */
		if (l_current_marker == J2K_MS_EOC) {
			if (p_j2k.m_decoder.m_state != J2K_STATE_EOC) {
				p_j2k.m_current_tile_number[0] = 0;
				p_j2k.m_decoder.m_state = J2K_STATE_EOC;
			}
		}

		/* FIXME DOC ??? */
		if (!p_j2k.m_decoder.m_can_decode) {
			int l_nb_tiles = p_j2k.m_cp.th * p_j2k.m_cp.tw;
			l_tcp = p_j2k.m_cp.tcps[p_j2k.m_current_tile_number[0]];

			while (((p_j2k.m_current_tile_number[0] < l_nb_tiles) && (p_j2k.m_current_tile_number[0] < p_j2k.m_cp.tcps.length-1))
					&& ((l_tcp.m_data == null) || (l_tcp.m_data.length == 0))) {
				++p_j2k.m_current_tile_number[0];
				l_tcp = p_j2k.m_cp.tcps[p_j2k.m_current_tile_number[0]];
			}

			if (p_j2k.m_current_tile_number[0] == l_nb_tiles) {
				p_go_on[0] = false;
				return true;
			}
		}

		if (!opj_j2k_merge_ppt(p_j2k.m_cp.tcps[p_j2k.m_current_tile_number[0]])) {
			MipavUtil.displayError("Failed to merge PPT data");
			return false;
		}
		/* FIXME ??? */
		if (!opj_tcd_init_decode_tile(p_j2k.m_tcd,
				p_j2k.m_current_tile_number[0])) {
			MipavUtil.displayError("Cannot decode tile, memory error");
			return false;
		}

		Preferences.debug("Header of tile "
				+ (p_j2k.m_current_tile_number[0] + 1) + " / "
				+ (p_j2k.m_cp.th * p_j2k.m_cp.tw) + " has been read.\n",
				Preferences.DEBUG_FILEIO);

		p_tile_index[0] = p_j2k.m_current_tile_number[0];
		p_go_on[0] = true;
		p_data_size[0] = opj_tcd_get_decoded_tile_size(p_j2k.m_tcd);
		p_tile_x0[0] = p_j2k.m_tcd.tcd_image.tiles.x0;
		p_tile_y0[0] = p_j2k.m_tcd.tcd_image.tiles.y0;
		p_tile_x1[0] = p_j2k.m_tcd.tcd_image.tiles.x1;
		p_tile_y1[0] = p_j2k.m_tcd.tcd_image.tiles.y1;
		p_nb_comps[0] = p_j2k.m_tcd.tcd_image.tiles.numcomps;

		p_j2k.m_decoder.m_state |= 0x0080;/* FIXME J2K_DEC_STATE_DATA; */

		return true;
	}

	private boolean opj_j2k_decode_tile(opj_j2k_t p_j2k, int p_tile_index,
			byte p_data[], int p_data_ptr[], int p_data_size,
			RandomAccessFile p_stream) {
		int l_current_marker;
		byte l_data[] = new byte[2];
		opj_tcp_t l_tcp;
		int bytesRead = 0;

		/* preconditions */
		if (p_stream == null) {
			MipavUtil
					.displayError("p_stream == null at opj_j2k_decode_tile entry");
			return false;
		}
		if (p_j2k == null) {
			MipavUtil
					.displayError("p_j2k == null at opj_j2k_decode_tile entry");
			return false;
		}

		if (((p_j2k.m_decoder.m_state & 0x0080/* FIXME J2K_DEC_STATE_DATA */) == 0)
				|| (p_tile_index != p_j2k.m_current_tile_number[0])) {
			return false;
		}

		l_tcp = p_j2k.m_cp.tcps[p_tile_index];
		if ((l_tcp.m_data == null) || (l_tcp.m_data.length == 0)) {
			if (l_tcp.m_data == null) {
				MipavUtil.displayError("l_tcp.m_data == null at opj_j2k_decode_tile entry");
			}
			else {
				MipavUtil.displayError("l_tcp.m_data.length == 0 at opj_j2k_decode_tile_entry");
			}
			opj_j2k_tcp_destroy(l_tcp);
			return false;
		}
		
		if (!opj_tcd_decode_tile(p_j2k.m_tcd, l_tcp.m_data, l_tcp.m_data_size,
				p_tile_index, p_j2k.cstr_index)) {
			opj_j2k_tcp_destroy(l_tcp);
			p_j2k.m_decoder.m_state |= 0x8000;/* FIXME J2K_DEC_STATE_ERR; */
			MipavUtil.displayError("Failed to decode.");
			return false;
		}

		if (!opj_tcd_update_tile_data(p_j2k.m_tcd, p_data, p_data_ptr,
				p_data_size)) {
			return false;
		}

		/*
		 * To avoid to destroy the tcp which can be useful when we try to decode
		 * a tile decoded before (cf j2k_random_tile_access) we destroy just the
		 * data which will be re-read in read_tile_header
		 */
		/*
		 * opj_j2k_tcp_destroy(l_tcp); p_j2k->m_tcd->tcp = 0;
		 */
		opj_j2k_tcp_data_destroy(l_tcp);

		p_j2k.m_decoder.m_can_decode = false;
		p_j2k.m_decoder.m_state &= (~(0x0080));/* FIXME J2K_DEC_STATE_DATA); */

		if (opj_stream_get_number_byte_left(p_stream) == 0
				&& p_j2k.m_decoder.m_state == J2K_STATE_NEOC) {
			return true;
		}

		if (p_j2k.m_decoder.m_state != 0x0100) { /* FIXME J2K_DEC_STATE_EOC) */
			try {
				bytesRead = p_stream.read(l_data, 0, 2);
			} catch (IOException e) {
				MipavUtil
						.displayError("IOException " + e + " on p_stream.read");
			}
			if (bytesRead != 2) {
				MipavUtil.displayError("RandomAccessFile too short");
				return false;
			}

			l_current_marker = getBufferUShort(l_data, 0, endianess);

			if (l_current_marker == J2K_MS_EOC) {
				p_j2k.m_current_tile_number[0] = 0;
				p_j2k.m_decoder.m_state = 0x0100;/* FIXME J2K_DEC_STATE_EOC; */
			} else if (l_current_marker != J2K_MS_SOT) {
				if (opj_stream_get_number_byte_left(p_stream) == 0) {
					p_j2k.m_decoder.m_state = J2K_STATE_NEOC;
					Preferences.debug(
							"Warning! Stream does not end with EOC\n",
							Preferences.DEBUG_FILEIO);
					return true;
				}
				MipavUtil
						.displayError("RandomAccessFile too short, expected SOT");
				return false;
			}
		}

		return true;
	}

	private boolean opj_j2k_update_image_data(opj_tcd_t p_tcd, byte p_data[],
			int p_data_ptr[], opj_image_t p_output_image) {
		int i, j, k = 0;
		int l_width_src, l_height_src;
		int l_width_dest, l_height_dest;
		int l_offset_x0_src, l_offset_y0_src, l_offset_x1_src, l_offset_y1_src;
		long l_start_offset_src, l_line_offset_src, l_end_offset_src;
		int l_start_x_dest, l_start_y_dest;
		int l_x0_dest, l_y0_dest, l_x1_dest, l_y1_dest;
		long l_start_offset_dest, l_line_offset_dest;

		opj_image_comp_t l_img_comp_src = null;
		opj_image_comp_t l_img_comp_dest = null;

		opj_tcd_tilecomp_t l_tilec = null;
		opj_image_t l_image_src = null;
		int l_size_comp, l_remaining;
		int l_dest_ptr;
		opj_tcd_resolution_t l_res = null;

		l_tilec = p_tcd.tcd_image.tiles.comps[0];
		l_image_src = p_tcd.image;
		l_img_comp_src = l_image_src.comps[0];

		l_img_comp_dest = p_output_image.comps[0];

		for (i = 0; i < l_image_src.numcomps; i++) {

			/* Allocate output component buffer if necessary */
			if (l_img_comp_dest.data == null) {

				l_img_comp_dest.data = new int[l_img_comp_dest.w
						* l_img_comp_dest.h];

			}

			/* Copy info from decoded comp image to output image */
			l_img_comp_dest.resno_decoded = l_img_comp_src.resno_decoded;

			/*-----*/
			/* Compute the precision of the output buffer */
			l_size_comp = l_img_comp_src.prec >>> 3; /* (/ 8) */
			l_remaining = l_img_comp_src.prec & 7; /* (%8) */
			l_res = l_tilec.resolutions[l_img_comp_src.resno_decoded];

			if (l_remaining != 0) {
				++l_size_comp;
			}

			if (l_size_comp == 3) {
				l_size_comp = 4;
			}
			/*-----*/

			/* Current tile component size */
			/*
			 * if (i == 0) { fprintf(stdout,
			 * "SRC: l_res_x0=%d, l_res_x1=%d, l_res_y0=%d, l_res_y1=%d\n",
			 * l_res->x0, l_res->x1, l_res->y0, l_res->y1); }
			 */

			l_width_src = l_res.x1 - l_res.x0;
			l_height_src = l_res.y1 - l_res.y0;

			/* Border of the current output component */
			l_x0_dest = (int) ((l_img_comp_dest.x0
					+ (1L << l_img_comp_dest.factor) - 1) >>> l_img_comp_dest.factor);
			l_y0_dest = (int) ((l_img_comp_dest.y0
					+ (1L << l_img_comp_dest.factor) - 1) >>> l_img_comp_dest.factor);
			l_x1_dest = l_x0_dest + l_img_comp_dest.w; /*
														 * can't overflow given
														 * that image->x1 is
														 * uint32
														 */
			l_y1_dest = l_y0_dest + l_img_comp_dest.h;

			/*
			 * if (i == 0) { fprintf(stdout,
			 * "DEST: l_x0_dest=%d, l_x1_dest=%d, l_y0_dest=%d, l_y1_dest=%d (%d)\n"
			 * , l_x0_dest, l_x1_dest, l_y0_dest, l_y1_dest,
			 * l_img_comp_dest->factor ); }
			 */

			/*-----*/
			/*
			 * Compute the area (l_offset_x0_src, l_offset_y0_src,
			 * l_offset_x1_src, l_offset_y1_src) of the input buffer (decoded
			 * tile component) which will be move in the output buffer. Compute
			 * the area of the output buffer (l_start_x_dest, l_start_y_dest,
			 * l_width_dest, l_height_dest) which will be modified by this input
			 * area.
			 */
			if (l_res.x0 < 0) {
				MipavUtil
						.displayError("l_res.x0 < 0 in opj_j2k_update_image_data");
				return false;
			}
			if (l_res.x1 < 0) {
				MipavUtil
						.displayError("l_res.x1 < 0 in opj_j2k_update_image_data");
				return false;
			}

			if (l_x0_dest < l_res.x0) {
				l_start_x_dest = l_res.x0 - l_x0_dest;
				l_offset_x0_src = 0;

				if (l_x1_dest >= l_res.x1) {
					l_width_dest = l_width_src;
					l_offset_x1_src = 0;
				} else {
					l_width_dest = l_x1_dest - l_res.x0;
					l_offset_x1_src = (l_width_src - l_width_dest);
				}
			} else {
				l_start_x_dest = 0;
				l_offset_x0_src = l_x0_dest - l_res.x0;

				if (l_x1_dest >= l_res.x1) {
					l_width_dest = l_width_src - l_offset_x0_src;
					l_offset_x1_src = 0;
				} else {
					l_width_dest = l_img_comp_dest.w;
					l_offset_x1_src = l_res.x1 - l_x1_dest;
				}
			}

			if (l_y0_dest < l_res.y0) {
				l_start_y_dest = l_res.y0 - l_y0_dest;
				l_offset_y0_src = 0;

				if (l_y1_dest >= l_res.y1) {
					l_height_dest = l_height_src;
					l_offset_y1_src = 0;
				} else {
					l_height_dest = l_y1_dest - l_res.y0;
					l_offset_y1_src = (l_height_src - l_height_dest);
				}
			} else {
				l_start_y_dest = 0;
				l_offset_y0_src = l_y0_dest - l_res.y0;

				if (l_y1_dest >= l_res.y1) {
					l_height_dest = l_height_src - l_offset_y0_src;
					l_offset_y1_src = 0;
				} else {
					l_height_dest = l_img_comp_dest.h;
					l_offset_y1_src = l_res.y1 - l_y1_dest;
				}
			}

			if ((l_offset_x0_src < 0) || (l_offset_y0_src < 0)
					|| (l_offset_x1_src < 0) || (l_offset_y1_src < 0)) {
				return false;
			}
			/* testcase 2977.pdf.asan.67.2198 */
			if (l_width_dest < 0 || l_height_dest < 0) {
				return false;
			}
			/*-----*/

			/* Compute the input buffer offset */
			l_start_offset_src = (long) l_offset_x0_src
					+ (long) l_offset_y0_src * (long) l_width_src;
			l_line_offset_src = (long) l_offset_x1_src + (long) l_offset_x0_src;
			l_end_offset_src = (long) l_offset_y1_src * (long) l_width_src
					- (long) l_offset_x0_src;

			/* Compute the output buffer offset */
			l_start_offset_dest = (long) l_start_x_dest + (long) l_start_y_dest
					* (long) l_img_comp_dest.w;
			l_line_offset_dest = (long) l_img_comp_dest.w - (long) l_width_dest;

			/* Move the output buffer to the first place where we will write */
			// l_dest_ptr = l_img_comp_dest->data + l_start_offset_dest;
			l_dest_ptr = (int) l_start_offset_dest;

			/*
			 * if (i == 0) { fprintf(stdout, "COMPO[%d]:\n",i); fprintf(stdout,
			 * "SRC: l_start_x_src=%d, l_start_y_src=%d, l_width_src=%d, l_height_src=%d\n"
			 * "\t tile offset:%d, %d, %d, %d\n"
			 * "\t buffer offset: %d; %d, %d\n", l_res->x0, l_res->y0,
			 * l_width_src, l_height_src, l_offset_x0_src, l_offset_y0_src,
			 * l_offset_x1_src, l_offset_y1_src, l_start_offset_src,
			 * l_line_offset_src, l_end_offset_src);
			 * 
			 * fprintf(stdout,
			 * "DEST: l_start_x_dest=%d, l_start_y_dest=%d, l_width_dest=%d, l_height_dest=%d\n"
			 * "\t start offset: %d, line offset= %d\n", l_start_x_dest,
			 * l_start_y_dest, l_width_dest, l_height_dest, l_start_offset_dest,
			 * l_line_offset_dest); }
			 */

			switch (l_size_comp) {
			case 1: {
				// OPJ_CHAR * l_src_ptr = (OPJ_CHAR*) p_data;
				int l_src_ptr = p_data_ptr[0];
				l_src_ptr += l_start_offset_src; /*
												 * Move to the first place where
												 * we will read
												 */

				if (l_img_comp_src.sgnd != 0) {
					for (j = 0; j < l_height_dest; ++j) {
						for (k = 0; k < l_width_dest; ++k) {
							/* Copy only the data needed for the output image */
							l_img_comp_dest.data[l_dest_ptr++] = p_data[l_src_ptr++];
						}

						l_dest_ptr += l_line_offset_dest; /*
														 * Move to the next
														 * place where we will
														 * write
														 */
						l_src_ptr += l_line_offset_src; /*
														 * Move to the next
														 * place where we will
														 * read
														 */
					}
				} else {
					for (j = 0; j < l_height_dest; ++j) {
						for (k = 0; k < l_width_dest; ++k) {
							l_img_comp_dest.data[l_dest_ptr++] = getUnsignedByte(
									p_data, l_src_ptr++);
						}

						l_dest_ptr += l_line_offset_dest;
						l_src_ptr += l_line_offset_src;
					}
				}

				l_src_ptr += l_end_offset_src; /*
												 * Move to the end of this
												 * component-part of the input
												 * buffer
												 */
				p_data_ptr[0] = l_src_ptr; /*
											 * Keep the current position for the
											 * next component-part
											 */
			}
				break;
			case 2: {
				// OPJ_INT16 * l_src_ptr = (OPJ_INT16 *) p_data;
				int l_src_ptr = p_data_ptr[0];
				l_src_ptr += 2 * l_start_offset_src;

				if (l_img_comp_src.sgnd != 0) {
					for (j = 0; j < l_height_dest; ++j) {
						for (k = 0; k < l_width_dest; ++k) {
							l_img_comp_dest.data[l_dest_ptr++] = getBufferShort(
									p_data, l_src_ptr, endianess);
							l_src_ptr += 2;
						}

						l_dest_ptr += l_line_offset_dest;
						l_src_ptr += 2 * l_line_offset_src;
					}
				} else {
					for (j = 0; j < l_height_dest; ++j) {
						for (k = 0; k < l_width_dest; ++k) {
							l_img_comp_dest.data[l_dest_ptr++] = getBufferUShort(
									p_data, l_src_ptr, endianess);
							l_src_ptr += 2;
						}

						l_dest_ptr += l_line_offset_dest;
						l_src_ptr += 2 * l_line_offset_src;
					}
				}

				l_src_ptr += 2 * l_end_offset_src;
				p_data_ptr[0] = l_src_ptr;
			}
				break;
			case 4: {
				// OPJ_INT32 * l_src_ptr = (OPJ_INT32 *) p_data;
				int l_src_ptr = p_data_ptr[0];
				l_src_ptr += 4 * l_start_offset_src;

				for (j = 0; j < l_height_dest; ++j) {
					for (k = 0; k < l_width_dest; ++k) {
						l_img_comp_dest.data[l_dest_ptr++] = getBufferInt(
								p_data, l_src_ptr, endianess);
						l_src_ptr += 4;
					}

					l_dest_ptr += l_line_offset_dest;
					l_src_ptr += 4 * l_line_offset_src;
				}

				l_src_ptr += 4 * l_end_offset_src;
				p_data_ptr[0] = l_src_ptr;
			}
				break;
			}

			if (i < l_image_src.numcomps - 1) {
				l_img_comp_dest = p_output_image.comps[i + 1];
				l_img_comp_src = l_image_src.comps[i + 1];
				l_tilec = p_tcd.tcd_image.tiles.comps[i + 1];
			}
		}

		return true;
	}

	private boolean opj_j2k_add_tlmarker(int tileno,
			opj_codestream_index_t cstr_index, int type, long pos, int len) {

		int i;
		if (cstr_index == null) {
			MipavUtil
					.displayError("cstr_index == null at opj_j2k_add_tlmarker entry");
			return false;
		}
		if (cstr_index.tile_index == null) {
			MipavUtil
					.displayError("cstr_index.tile_index == null at opj_j2k_add_tlmarker entry");
			return false;
		}

		/* expand the list? */
		if ((cstr_index.tile_index[tileno].marknum + 1) > cstr_index.tile_index[tileno].maxmarknum) {
			opj_marker_info_t new_marker[];
			cstr_index.tile_index[tileno].maxmarknum = (100 + cstr_index.tile_index[tileno].maxmarknum);
			new_marker = new opj_marker_info_t[cstr_index.tile_index[tileno].maxmarknum];
			for (i = 0; i < cstr_index.tile_index[tileno].marker.length; i++) {
				new_marker[i] = cstr_index.tile_index[tileno].marker[i];
			}
			for (i = cstr_index.tile_index[tileno].marker.length; i < cstr_index.tile_index[tileno].maxmarknum; i++) {
				new_marker[i] = new opj_marker_info_t();
			}
			cstr_index.tile_index[tileno].marker = new_marker;
		}

		/* add the marker */
		cstr_index.tile_index[tileno].marker[cstr_index.tile_index[tileno].marknum].type = (short) type;
		cstr_index.tile_index[tileno].marker[cstr_index.tile_index[tileno].marknum].pos = pos;
		cstr_index.tile_index[tileno].marker[cstr_index.tile_index[tileno].marknum].len = len;
		cstr_index.tile_index[tileno].marknum++;

		if (type == J2K_MS_SOT) {
			int l_current_tile_part = cstr_index.tile_index[tileno].current_tpsno;

			if (cstr_index.tile_index[tileno].tp_index != null)
				cstr_index.tile_index[tileno].tp_index[l_current_tile_part].start_pos = pos;

		}
		return true;
	}

	private boolean opj_j2k_read_sod(opj_j2k_t p_j2k, RandomAccessFile p_stream) {
		long l_current_read_size = 0;
		opj_codestream_index_t l_cstr_index = null;
		byte l_current_data[] = null;
		opj_tcp_t l_tcp = null;
		int l_tile_len;
		boolean l_sot_length_pb_detected = false;
		int i;
		long l_current_pos = 0;

		/* preconditions */
		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null at opj_j2k_read_sod_entry");
			return false;
		}
		if (p_stream == null) {
			MipavUtil
					.displayError("p_stream == null at opj_j2k_read_sod_entry");
			return false;
		}

		l_tcp = p_j2k.m_cp.tcps[p_j2k.m_current_tile_number[0]];

		if (p_j2k.m_decoder.m_last_tile_part != 0) {
			/*
			 * opj_stream_get_number_byte_left returns OPJ_OFF_T // but we are
			 * in the last tile part, // so its result will fit on OPJ_UINT32
			 * unless we find // a file with a single tile part of more than 4
			 * GB...
			 */
			p_j2k.m_decoder.m_sot_length = (int) (opj_stream_get_number_byte_left(p_stream) - 2);
		} else {
			/* Check to avoid pass the limit of OPJ_UINT32 */
			if (p_j2k.m_decoder.m_sot_length >= 2)
				p_j2k.m_decoder.m_sot_length -= 2;
			else {
				/* MSD: case commented to support empty SOT marker (PHR data) */
			}
		}

		l_current_data = l_tcp.m_data;
		l_tile_len = l_tcp.m_data_size;

		/* Patch to support new PHR data */
		if (p_j2k.m_decoder.m_sot_length != 0) {
			/* If we are here, we'll try to read the data after allocation */
			/* Check enough bytes left in stream before allocation */
			if (p_j2k.m_decoder.m_sot_length > opj_stream_get_number_byte_left(p_stream)) {
				MipavUtil
						.displayError("Tile part length size inconsistent with stream length");
				return false;
			}
			if (l_current_data == null) {
				/*
				 * LH: oddly enough, in this path, l_tile_len!=0. TODO: If this
				 * was consistent, we could simplify the code to only use
				 * realloc(), as realloc(0,...) default to malloc(0,...).
				 */
				l_current_data = new byte[p_j2k.m_decoder.m_sot_length];
			} else {
				byte l_new_current_data[] = new byte[l_tile_len
						+ p_j2k.m_decoder.m_sot_length];
				for (i = 0; i < l_current_data.length; i++) {
					l_new_current_data[i] = l_current_data[i];
				}
				l_current_data = l_new_current_data;
			}

		} else {
			l_sot_length_pb_detected = true;
		}

		/* Index */
		l_cstr_index = p_j2k.cstr_index;
		if (l_cstr_index != null) {
			try {
				l_current_pos = p_stream.getFilePointer() - 2;
			} catch (IOException e) {
				MipavUtil.displayError("IOException " + e
						+ " on p_stream.getFilePointer()");
				return false;
			}

			int l_current_tile_part = l_cstr_index.tile_index[p_j2k.m_current_tile_number[0]].current_tpsno;
			l_cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index[l_current_tile_part].end_header = l_current_pos;
			l_cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index[l_current_tile_part].end_pos = l_current_pos
					+ p_j2k.m_decoder.m_sot_length + 2;

			if (false == opj_j2k_add_tlmarker(p_j2k.m_current_tile_number[0],
					l_cstr_index, J2K_MS_SOD, l_current_pos,
					p_j2k.m_decoder.m_sot_length + 2)) {
				MipavUtil.displayError("Not enough memory to add tl marker");
				return false;
			}

			/* l_cstr_index->packno = 0; */
		}

		/* Patch to support new PHR data */
		if (!l_sot_length_pb_detected) {
			try {
				l_current_read_size = p_stream.read(l_current_data, l_tile_len,
						p_j2k.m_decoder.m_sot_length);
			} catch (IOException e) {
				MipavUtil
						.displayError("IOException " + e + " on p_stream.read");
				return false;
			}
		} else {
			l_current_read_size = 0;
		}

		if (l_current_read_size != p_j2k.m_decoder.m_sot_length) {
			p_j2k.m_decoder.m_state = J2K_STATE_NEOC;
		} else {
			p_j2k.m_decoder.m_state = J2K_STATE_TPHSOT;
		}

		l_tile_len += l_current_read_size;

		return true;
	}

	private boolean opj_j2k_need_nb_tile_parts_correction(
			RandomAccessFile p_stream, int tile_no,
			boolean p_correction_needed[]) {
		byte l_header_data[] = new byte[10];
		long l_stream_pos_backup;
		int l_current_marker;
		int l_marker_size;
		int l_tile_no[] = new int[1];
		int l_tot_len[] = new int[1];
		int l_current_part[] = new int[1];
		int l_num_parts[] = new int[1];
		int bytesRead = 0;
		int skipBytes = 0;

		/* initialize to no correction needed */
		p_correction_needed[0] = false;

		try {
			l_stream_pos_backup = p_stream.getFilePointer();
		} catch (IOException e) {
			MipavUtil.displayError("IOException " + e
					+ " on p_stream.getFilePointer()");
			return false;
		}

		for (;;) {
			/*
			 * Try to read 2 bytes (the next marker ID) from stream and copy
			 * them into the buffer
			 */
			try {
				bytesRead = p_stream.read(l_header_data, 0, 2);
			} catch (IOException e) {
				MipavUtil
						.displayError("IOException " + e + " on p_stream.read");
				return false;
			}
			if (bytesRead != 2) {
				/* assume all is OK */
				try {
					p_stream.seek(l_stream_pos_backup);
				} catch (IOException e) {
					MipavUtil.displayError("IOException " + e
							+ " p_stream.seek");
					return false;
				}

				return true;
			}

			/* Read 2 bytes from buffer as the new marker ID */
			l_current_marker = getBufferUShort(l_header_data, 0, endianess);

			if (l_current_marker != J2K_MS_SOT) {
				/* assume all is OK */
				try {
					p_stream.seek(l_stream_pos_backup);
				} catch (IOException e) {
					MipavUtil.displayError("IOException " + e
							+ " p_stream.seek");
					return false;
				}
				return true;
			}

			/*
			 * Try to read 2 bytes (the marker size) from stream and copy them
			 * into the buffer
			 */
			try {
				bytesRead = p_stream.read(l_header_data, 0, 2);
			} catch (IOException e) {
				MipavUtil
						.displayError("IOException " + e + " on p_stream.read");
				return false;
			}
			if (bytesRead != 2) {
				MipavUtil.displayError("RandomAccessFile too short");
				return false;
			}

			/* Read 2 bytes from the buffer as the marker size */
			l_marker_size = getBufferUShort(l_header_data, 0, endianess);

			/* Check marker size for SOT Marker */
			if (l_marker_size != 10) {
				MipavUtil.displayError("Inconsistent marker size");
				return false;
			}
			l_marker_size -= 2;

			try {
				bytesRead = p_stream.read(l_header_data, 0, l_marker_size);
			} catch (IOException e) {
				MipavUtil
						.displayError("IOException " + e + " on p_stream.read");
				return false;
			}
			if (bytesRead != l_marker_size) {
				MipavUtil.displayError("RandomAdessFile too short\n");
				return false;
			}

			if (!opj_j2k_get_sot_values(l_header_data, l_marker_size,
					l_tile_no, l_tot_len, l_current_part, l_num_parts)) {
				return false;
			}

			if (l_tile_no[0] == tile_no) {
				/* we found what we were looking for */
				break;
			}

			if ((l_tot_len[0] == 0) || (l_tot_len[0] < 14)) {
				/* last SOT until EOC or invalid Psot value */
				/* assume all is OK */
				try {
					p_stream.seek(l_stream_pos_backup);
				} catch (IOException e) {
					return false;
				}

				return true;
			}
			l_tot_len[0] -= 12;
			/* look for next SOT marker */
			try {
				skipBytes = p_stream.skipBytes(l_tot_len[0]);
			} catch (IOException e) {
				MipavUtil.displayError("IOException " + e
						+ " p_stream_skipBytes");
				return false;
			}
			if (skipBytes != l_tot_len[0]) {
				/* assume all is OK */
				try {
					p_stream.seek(l_stream_pos_backup);
				} catch (IOException e) {
					return false;
				}
				return true;
			}
		}

		/* check for correction */
		if (l_current_part[0] == l_num_parts[0]) {
			p_correction_needed[0] = true;
		}

		try {
			p_stream.seek(l_stream_pos_backup);
		} catch (IOException e) {
			return false;
		}

		return true;
	}

	/**
	 * Merges all PPT markers read (Packed packet headers, tile-part header)
	 * 
	 * @param p_tcp
	 *            the tile.
	 */
	private boolean opj_j2k_merge_ppt(opj_tcp_t p_tcp) {
		int i, l_ppt_data_size, j;
		/* preconditions */
		if (p_tcp == null) {
			MipavUtil.displayError("p_tcp = null at opj_j2k_merge_ppt entry");
			return false;
		}
		if (p_tcp.ppt_buffer != null) {
			MipavUtil
					.displayError("p_tcp.ppt_buffer != null at opj_j2k_merge_ppt entry");
			return false;
		}

		if (p_tcp.ppt == 0) {
			return true;
		}

		l_ppt_data_size = 0;
		for (i = 0; i < p_tcp.ppt_markers_count; ++i) {
			l_ppt_data_size += p_tcp.ppt_markers[i].m_data_size; /*
																 * can't
																 * overflow, max
																 * 256 markers
																 * of max 65536
																 * bytes
																 */
		}

		p_tcp.ppt_buffer = new byte[l_ppt_data_size];

		p_tcp.ppt_len = l_ppt_data_size;
		l_ppt_data_size = 0;
		for (i = 0; i < p_tcp.ppt_markers_count; ++i) {
			if (p_tcp.ppt_markers[i].m_data != null) { /*
														 * standard doesn't seem
														 * to require contiguous
														 * Zppt
														 */
				for (j = 0; j < p_tcp.ppt_markers[i].m_data_size; j++) {
					p_tcp.ppt_buffer[l_ppt_data_size + j] = p_tcp.ppt_markers[i].m_data[j];
				}
				l_ppt_data_size += p_tcp.ppt_markers[i].m_data_size; /*
																	 * can't
																	 * overflow,
																	 * max 256
																	 * markers
																	 * of max
																	 * 65536
																	 * bytes
																	 */

				p_tcp.ppt_markers[i].m_data = null;
				p_tcp.ppt_markers[i].m_data_size = 0;
			}
		}

		p_tcp.ppt_markers_count = 0;
		for (i = 0; i < p_tcp.ppt_markers.length; i++) {
			p_tcp.ppt_markers[i].m_data = null;
			p_tcp.ppt_markers[i] = null;
		}
		p_tcp.ppt_markers = null;

		p_tcp.ppt_data = p_tcp.ppt_buffer;
		p_tcp.ppt_data_size = p_tcp.ppt_len;
		return true;
	}

	private boolean opj_tcd_init_decode_tile(opj_tcd_t p_tcd, int p_tile_no) {
		return opj_tcd_init_tile(p_tcd, p_tile_no, false, 0.5f);
	}

	private boolean opj_tcd_init_tile(opj_tcd_t p_tcd, int p_tile_no,
			boolean isEncoder, float fraction) {
		int l_gain_ptr = 0;
		int compno, resno, bandno, precno, cblkno;
		opj_tcp_t l_tcp = null;
		opj_cp_t l_cp = null;
		opj_tcd_tile_t l_tile = null;
		opj_tccp_t l_tccp = null;
		opj_tcd_tilecomp_t l_tilec = null;
		opj_image_comp_t l_image_comp = null;
		opj_tcd_resolution_t l_res = null;
		opj_tcd_band_t l_band = null;
		opj_stepsize_t l_step_size = null;
		opj_tcd_precinct_t l_current_precinct = null;
		opj_image_t l_image = null;
		int p, q;
		int l_level_no;
		int l_pdx, l_pdy;
		int l_gain;
		int l_x0b, l_y0b;
		int l_tx0, l_ty0;
		/* extent of precincts , top left, bottom right* */
		int l_tl_prc_x_start, l_tl_prc_y_start, l_br_prc_x_end, l_br_prc_y_end;
		/* number of precinct for a resolution */
		int l_nb_precincts;
		/* room needed to store l_nb_precinct precinct for a resolution */
		int l_nb_precinct_size;
		/* number of code blocks for a precinct */
		int l_nb_code_blocks;
		/* room needed to store l_nb_code_blocks code blocks for a precinct */
		int l_nb_code_blocks_size;
		/* size of data for a tile */
		int l_data_size;
		int i;
		int j;
		int l_step_size_index;

		l_cp = p_tcd.cp;
		l_tcp = l_cp.tcps[p_tile_no];
		l_tile = p_tcd.tcd_image.tiles;
		l_tccp = l_tcp.tccps[0];
		l_tilec = l_tile.comps[0];
		l_image = p_tcd.image;
		l_image_comp = p_tcd.image.comps[0];

		p = p_tile_no % l_cp.tw; /* tile coordinates */
		q = p_tile_no / l_cp.tw;
		/* fprintf(stderr, "Tile coordinate = %d,%d\n", p, q); */

		/* 4 borders of the tile rescale on the image if necessary */
		l_tx0 = l_cp.tx0 + p * l_cp.tdx; /*
										 * can't be greater than l_image->x1 so
										 * won't overflow
										 */
		l_tile.x0 = Math.max(l_tx0, l_image.x0);
		l_tile.x1 = Math.min(l_tx0 + l_cp.tdx, l_image.x1);
		l_ty0 = l_cp.ty0 + q * l_cp.tdy; /*
										 * can't be greater than l_image->y1 so
										 * won't overflow
										 */
		l_tile.y0 = Math.max(l_ty0, l_image.y0);
		l_tile.y1 = Math.min(l_ty0 + l_cp.tdy, l_image.y1);

		/* testcase 1888.pdf.asan.35.988 */
		if (l_tccp.numresolutions == 0) {
			MipavUtil.displayError("tiles require at least one resolution");
			return false;
		}
		/*
		 * fprintf(stderr, "Tile border = %d,%d,%d,%d\n", l_tile->x0,
		 * l_tile->y0,l_tile->x1,l_tile->y1);
		 */

		/* tile->numcomps = image->numcomps; */
		for (compno = 0; compno < l_tile.numcomps; ++compno) {
			/* fprintf(stderr, "compno = %d/%d\n", compno, l_tile->numcomps); */
			l_image_comp.resno_decoded = 0;
			/* border of each l_tile component (global) */
			l_tilec.x0 = (l_tile.x0 + l_image_comp.dx - 1) / l_image_comp.dx;
			l_tilec.y0 = (l_tile.y0 + l_image_comp.dy - 1) / l_image_comp.dy;
			l_tilec.x1 = (l_tile.x1 + l_image_comp.dx - 1) / l_image_comp.dx;
			l_tilec.y1 = (l_tile.y1 + l_image_comp.dy - 1) / l_image_comp.dy;
			/*
			 * fprintf(stderr, "\tTile compo border = %d,%d,%d,%d\n",
			 * l_tilec->x0, l_tilec->y0,l_tilec->x1,l_tilec->y1);
			 */

			/* compute l_data_size with overflow check */
			l_data_size = (l_tilec.x1 - l_tilec.x0);
			if ((2147483647 / l_data_size) < (l_tilec.y1 - l_tilec.y0)) {
				MipavUtil.displayError("Not enough memory for tile data");
				return false;
			}
			l_data_size = l_data_size * (l_tilec.y1 - l_tilec.y0);

			if ((2147483647 / 4) < l_data_size) {
				MipavUtil.displayError("Not enough memory for tile data");
				return false;
			}
			// l_data_size = l_data_size * (OPJ_UINT32)sizeof(OPJ_UINT32);
			l_tilec.numresolutions = l_tccp.numresolutions;
			if (l_tccp.numresolutions < l_cp.m_dec.m_reduce) {
				l_tilec.minimum_num_resolutions = 1;
			} else {
				l_tilec.minimum_num_resolutions = l_tccp.numresolutions
						- l_cp.m_dec.m_reduce;
			}

			l_tilec.data_size_needed = l_data_size;
			if ((p_tcd.m_is_decoder != 0)
					&& !opj_alloc_tile_component_data(l_tilec)) {
				MipavUtil.displayError("Not enough memory for tile data");
				return false;
			}

			l_data_size = l_tilec.numresolutions;

			if (l_tilec.resolutions == null) {
				l_tilec.resolutions = new opj_tcd_resolution_t[l_data_size];
				for (i = 0; i < l_data_size; i++) {
					l_tilec.resolutions[i] = new opj_tcd_resolution_t();
					for (j = 0; j < 3; j++) {
						l_tilec.resolutions[i].bands[j] = new opj_tcd_band_t();
					}
				}

				/*
				 * fprintf(stderr,
				 * "\tAllocate resolutions of tilec (opj_tcd_resolution_t): %d\n"
				 * ,l_data_size);
				 */
				l_tilec.resolutions_size = l_data_size;
				for (i = 0; i < l_data_size; i++) {
					l_tilec.resolutions[i].x0 = 0;
					l_tilec.resolutions[i].y0 = 0;
					l_tilec.resolutions[i].x1 = 0;
					l_tilec.resolutions[i].y1 = 0;
					l_tilec.resolutions[i].pw = 0;
					l_tilec.resolutions[i].ph = 0;
					l_tilec.resolutions[i].numbands = 0;
				}
			} else if (l_data_size > l_tilec.resolutions_size) {
				opj_tcd_resolution_t new_resolutions[] = new opj_tcd_resolution_t[l_data_size];
				for (i = 0; i < l_tilec.resolutions_size; i++) {
					new_resolutions[i] = l_tilec.resolutions[i];
					
				}
				for (i = l_tilec.resolutions_size; i < l_data_size; i++) {
					new_resolutions[i] = new opj_tcd_resolution_t();
				} 

				l_tilec.resolutions = new_resolutions;
				for (i = l_tilec.resolutions_size; i < l_data_size; i++) {
					l_tilec.resolutions[i].x0 = 0;
					l_tilec.resolutions[i].y0 = 0;
					l_tilec.resolutions[i].x1 = 0;
					l_tilec.resolutions[i].y1 = 0;
					l_tilec.resolutions[i].pw = 0;
					l_tilec.resolutions[i].ph = 0;
					l_tilec.resolutions[i].numbands = 0;
					l_tilec.resolutions[i].bands = new opj_tcd_band_t[3];
					for (j = 0; j < 3; j++) {
						l_tilec.resolutions[i].bands[j] = new opj_tcd_band_t();
					}
				}
				/*
				 * fprintf(stderr,
				 * "\tReallocate data of tilec (int): from %d to %d x OPJ_UINT32\n"
				 * , l_tilec->resolutions_size, l_data_size);
				 */
				l_tilec.resolutions_size = l_data_size;
			}

			l_level_no = l_tilec.numresolutions - 1;
			l_res = l_tilec.resolutions[0];
			l_step_size = l_tccp.stepsizes[0];
			l_step_size_index = 0;
			if (l_tccp.qmfbid == 0) {
				l_gain_ptr = OPJ_DWT_GETGAIN_REAL;
			} else {
				l_gain_ptr = OPJ_DWT_GETGAIN;
			}
			/* fprintf(stderr, "\tlevel_no=%d\n",l_level_no); */

			for (resno = 0; resno < l_tilec.numresolutions; ++resno) {
				/*
				 * fprintf(stderr, "\t\tresno = %d/%d\n", resno,
				 * l_tilec->numresolutions);
				 */
				int tlcbgxstart, tlcbgystart /* , brcbgxend, brcbgyend */;
				int cbgwidthexpn, cbgheightexpn;
				int cblkwidthexpn, cblkheightexpn;

				/* border for each resolution level (global) */
				l_res.x0 = (int) ((l_tilec.x0 + (1L << l_level_no) - 1) >>> l_level_no);
				l_res.y0 = (int) ((l_tilec.y0 + (1L << l_level_no) - 1) >>> l_level_no);
				l_res.x1 = (int) ((l_tilec.x1 + (1L << l_level_no) - 1) >>> l_level_no);
				l_res.y1 = (int) ((l_tilec.y1 + (1L << l_level_no) - 1) >>> l_level_no);
				/*
				 * fprintf(stderr,
				 * "\t\t\tres_x0= %d, res_y0 =%d, res_x1=%d, res_y1=%d\n",
				 * l_res->x0, l_res->y0, l_res->x1, l_res->y1);
				 */
				/*
				 * p. 35, table A-23, ISO/IEC FDIS154444-1 : 2000 (18 august
				 * 2000)
				 */
				l_pdx = l_tccp.prcw[resno];
				l_pdy = l_tccp.prch[resno];
				/* fprintf(stderr, "\t\t\tpdx=%d, pdy=%d\n", l_pdx, l_pdy); */
				/* p. 64, B.6, ISO/IEC FDIS15444-1 : 2000 (18 august 2000) */
				l_tl_prc_x_start = (l_res.x0 >> l_pdx) << l_pdx;
				l_tl_prc_y_start = (l_res.y0 >> l_pdy) << l_pdy;
				l_br_prc_x_end = (int) (((l_res.x1 + (1L << l_pdx) - 1) >>> l_pdx) << l_pdx);
				l_br_prc_y_end = (int) (((l_res.y1 + (1L << l_pdy) - 1) >>> l_pdy) << l_pdy);
				/*
				 * fprintf(stderr,
				 * "\t\t\tprc_x_start=%d, prc_y_start=%d, br_prc_x_end=%d, br_prc_y_end=%d \n"
				 * , l_tl_prc_x_start, l_tl_prc_y_start, l_br_prc_x_end
				 * ,l_br_prc_y_end );
				 */

				l_res.pw = (l_res.x0 == l_res.x1) ? 0
						: ((l_br_prc_x_end - l_tl_prc_x_start) >>> l_pdx);
				l_res.ph = (l_res.y0 == l_res.y1) ? 0
						: ((l_br_prc_y_end - l_tl_prc_y_start) >>> l_pdy);
				/*
				 * fprintf(stderr, "\t\t\tres_pw=%d, res_ph=%d\n", l_res->pw,
				 * l_res->ph );
				 */

				l_nb_precincts = l_res.pw * l_res.ph;
				// l_nb_precinct_size = l_nb_precincts *
				// (OPJ_UINT32)sizeof(opj_tcd_precinct_t);
				l_nb_precinct_size = l_nb_precincts;
				if (resno == 0) {
					tlcbgxstart = l_tl_prc_x_start;
					tlcbgystart = l_tl_prc_y_start;
					/* brcbgxend = l_br_prc_x_end; */
					/* brcbgyend = l_br_prc_y_end; */
					cbgwidthexpn = l_pdx;
					cbgheightexpn = l_pdy;
					l_res.numbands = 1;
				} else {
					tlcbgxstart = (int) ((l_tl_prc_x_start + (1L << 1) - 1) >>> 1);
					tlcbgystart = (int) ((l_tl_prc_y_start + (1L << 1) - 1) >>> 1);
					/* brcbgxend = opj_int_ceildivpow2(l_br_prc_x_end, 1); */
					/* brcbgyend = opj_int_ceildivpow2(l_br_prc_y_end, 1); */
					cbgwidthexpn = l_pdx - 1;
					cbgheightexpn = l_pdy - 1;
					l_res.numbands = 3;
				}

				cblkwidthexpn = Math.min(l_tccp.cblkw, cbgwidthexpn);
				cblkheightexpn = Math.min(l_tccp.cblkh, cbgheightexpn);
				l_band = l_res.bands[0];

				for (bandno = 0; bandno < l_res.numbands; ++bandno) {
					int numbps;
					/*
					 * fprintf(stderr, "\t\t\tband_no=%d/%d\n", bandno,
					 * l_res->numbands );
					 */

					if (resno == 0) {
						l_band.bandno = 0;
						l_band.x0 = (int) ((l_tilec.x0 + (1L << l_level_no) - 1) >>> l_level_no);
						l_band.y0 = (int) ((l_tilec.y0 + (1L << l_level_no) - 1) >>> l_level_no);
						l_band.x1 = (int) ((l_tilec.x1 + (1L << l_level_no) - 1) >>> l_level_no);
						l_band.y1 = (int) ((l_tilec.y1 + (1L << l_level_no) - 1) >>> l_level_no);
					} else {
						l_band.bandno = bandno + 1;
						/* x0b = 1 if bandno = 1 or 3 */
						l_x0b = l_band.bandno & 1;
						/* y0b = 1 if bandno = 2 or 3 */
						l_y0b = ((l_band.bandno) >>> 1);
						/* l_band border (global) */
						l_band.x0 = (int) ((l_tilec.x0
								- ((long) l_x0b << l_level_no)
								+ (1L << (l_level_no + 1)) - 1) >>> (l_level_no + 1));
						l_band.y0 = (int) ((l_tilec.y0
								- ((long) l_y0b << l_level_no)
								+ (1L << (l_level_no + 1)) - 1) >>> (l_level_no + 1));
						l_band.x1 = (int) ((l_tilec.x1
								- ((long) l_x0b << l_level_no)
								+ (1L << (l_level_no + 1)) - 1) >>> (l_level_no + 1));
						l_band.y1 = (int) ((l_tilec.y1
								- ((long) l_y0b << l_level_no)
								+ (1L << (l_level_no + 1)) - 1) >>> (l_level_no + 1));
					}

					/** avoid an if with storing function pointer */
					if (l_gain_ptr == OPJ_DWT_GETGAIN_REAL) {
						l_gain = opj_dwt_getgain_real(l_band.bandno);
					} else {
						l_gain = opj_dwt_getgain(l_band.bandno);
					}
					numbps = l_image_comp.prec + l_gain;
					l_band.stepsize = (float) (((1.0 + l_step_size.mant / 2048.0) * Math
							.pow(2.0, (int) (numbps - l_step_size.expn))))
							* fraction;
					l_band.numbps = l_step_size.expn + l_tccp.numgbits - 1; /*
																			 * WHY
																			 * -
																			 * 1
																			 * ?
																			 */

					if ((l_band.precincts == null) && (l_nb_precincts > 0)) {
						l_band.precincts = new opj_tcd_precinct_t[l_nb_precinct_size];
						for (j = 0; j < l_nb_precinct_size; j++) {
							l_band.precincts[j] = new opj_tcd_precinct_t();
							l_band.precincts[j].x0 = 0;
							l_band.precincts[j].y0 = 0;
							l_band.precincts[j].x1 = 0;
							l_band.precincts[j].y1 = 0;
							l_band.precincts[j].cw = 0;
							l_band.precincts[j].ch = 0;
							l_band.precincts[j].dec = null;
							l_band.precincts[j].block_size = 0;
							l_band.precincts[j].incltree = null;
							l_band.precincts[j].imsbtree = null;
						}

						/*
						 * fprintf(stderr,
						 * "\t\t\t\tAllocate precincts of a band (opj_tcd_precinct_t): %d\n"
						 * ,l_nb_precinct_size);
						 */
						l_band.precincts_data_size = l_nb_precinct_size;
					} else if (l_band.precincts_data_size < l_nb_precinct_size) {

						opj_tcd_precinct_t new_precincts[] = new opj_tcd_precinct_t[l_nb_precinct_size];
						for (i = 0; i < l_band.precincts_data_size; i++) {
							new_precincts[i] = l_band.precincts[i];
						}
						l_band.precincts = new_precincts;
						for (j = l_band.precincts_data_size; j < l_nb_precinct_size; j++) {
							l_band.precincts[j] = new opj_tcd_precinct_t();
							l_band.precincts[j].x0 = 0;
							l_band.precincts[j].y0 = 0;
							l_band.precincts[j].x1 = 0;
							l_band.precincts[j].y1 = 0;
							l_band.precincts[j].cw = 0;
							l_band.precincts[j].ch = 0;
							l_band.precincts[j].dec = null;
							l_band.precincts[j].block_size = 0;
							l_band.precincts[j].incltree = null;
							l_band.precincts[j].imsbtree = null;
						}
						/*
						 * fprintf(stderr,
						 * "\t\t\t\tReallocate precincts of a band (opj_tcd_precinct_t): from %d to %d\n"
						 * ,l_band->precincts_data_size, l_nb_precinct_size);
						 */
						l_band.precincts_data_size = l_nb_precinct_size;
					}

					l_current_precinct = l_band.precincts[0];
					for (precno = 0; precno < l_nb_precincts; ++precno) {
						int tlcblkxstart, tlcblkystart, brcblkxend, brcblkyend;
						int cbgxstart = tlcbgxstart + (precno % l_res.pw)
								* (1 << cbgwidthexpn);
						int cbgystart = tlcbgystart + (precno / l_res.pw)
								* (1 << cbgheightexpn);
						int cbgxend = cbgxstart + (1 << cbgwidthexpn);
						int cbgyend = cbgystart + (1 << cbgheightexpn);
						/*
						 * fprintf(stderr,
						 * "\t precno=%d; bandno=%d, resno=%d; compno=%d\n",
						 * precno, bandno , resno, compno);
						 */
						/*
						 * fprintf(stderr,
						 * "\t tlcbgxstart(=%d) + (precno(=%d) percent res->pw(=%d)) * (1 << cbgwidthexpn(=%d)) \n"
						 * ,tlcbgxstart,precno,l_res->pw,cbgwidthexpn);
						 */

						/* precinct size (global) */
						/*
						 * fprintf(stderr,
						 * "\t cbgxstart=%d, l_band->x0 = %d \n",cbgxstart,
						 * l_band->x0);
						 */

						l_current_precinct.x0 = Math.max(cbgxstart, l_band.x0);
						l_current_precinct.y0 = Math.max(cbgystart, l_band.y0);
						l_current_precinct.x1 = Math.min(cbgxend, l_band.x1);
						l_current_precinct.y1 = Math.min(cbgyend, l_band.y1);
						/*
						 * fprintf(stderr,
						 * "\t prc_x0=%d; prc_y0=%d, prc_x1=%d; prc_y1=%d\n"
						 * ,l_current_precinct->x0, l_current_precinct->y0
						 * ,l_current_precinct->x1, l_current_precinct->y1);
						 */

						tlcblkxstart = (l_current_precinct.x0 >>> cblkwidthexpn) << cblkwidthexpn;
						/*
						 * fprintf(stderr, "\t tlcblkxstart =%d\n",tlcblkxstart
						 * );
						 */
						tlcblkystart = (l_current_precinct.y0 >>> cblkheightexpn) << cblkheightexpn;
						/*
						 * fprintf(stderr, "\t tlcblkystart =%d\n",tlcblkystart
						 * );
						 */
						brcblkxend = (int) (((l_current_precinct.x1
								+ (1L << cblkwidthexpn) - 1) >>> cblkwidthexpn) << cblkwidthexpn);
						/* fprintf(stderr, "\t brcblkxend =%d\n",brcblkxend ); */
						brcblkyend = (int) (((l_current_precinct.y1
								+ (1L << cblkheightexpn) - 1) >>> cblkheightexpn) << cblkheightexpn);
						/* fprintf(stderr, "\t brcblkyend =%d\n",brcblkyend ); */
						l_current_precinct.cw = ((brcblkxend - tlcblkxstart) >>> cblkwidthexpn);
						l_current_precinct.ch = ((brcblkyend - tlcblkystart) >>> cblkheightexpn);

						l_nb_code_blocks = l_current_precinct.cw
								* l_current_precinct.ch;
						/*
						 * fprintf(stderr,
						 * "\t\t\t\t precinct_cw = %d x recinct_ch = %d\n"
						 * ,l_current_precinct->cw, l_current_precinct->ch);
						 */
						// l_nb_code_blocks_size = l_nb_code_blocks *
						// (OPJ_UINT32)sizeof_block;

						l_nb_code_blocks_size = l_nb_code_blocks;

						if ((l_current_precinct.dec == null)
								&& (l_nb_code_blocks > 0)) {
							l_current_precinct.dec = new opj_tcd_cblk_dec_t[l_nb_code_blocks_size];
                            for (i = 0; i < l_nb_code_blocks; i++) {
                            	l_current_precinct.dec[i] = new opj_tcd_cblk_dec_t();
                            	l_current_precinct.dec[i].data = null;
                            	l_current_precinct.dec[i].data_current_size = 0;
                            	l_current_precinct.dec[i].data_max_size = 0;
                            	l_current_precinct.dec[i].m_current_max_segs = 0;
                            	l_current_precinct.dec[i].numbps = 0;
                            	l_current_precinct.dec[i].numlenbits = 0;
                            	l_current_precinct.dec[i].numnewpasses = 0;
                            	l_current_precinct.dec[i].numsegs = 0;
                            	l_current_precinct.dec[i].real_num_segs = 0;
                            	l_current_precinct.dec[i].segs = null;
                            }
							/*
							 * fprintf(stderr,
							 * "\t\t\t\tAllocate cblks of a precinct (opj_tcd_cblk_dec_t): %d\n"
							 * ,l_nb_code_blocks_size);
							 */

							l_current_precinct.block_size = l_nb_code_blocks_size;
						} else if (l_nb_code_blocks_size > l_current_precinct.block_size) {
							opj_tcd_cblk_dec_t new_dec[] = new opj_tcd_cblk_dec_t[l_nb_code_blocks_size];
							for (i = 0; i < l_current_precinct.block_size; i++) {
								new_dec[i] = l_current_precinct.dec[i];
							}
							for (i = l_current_precinct.block_size; i < l_nb_code_blocks_size; i++) {
							    new_dec[i] = new opj_tcd_cblk_dec_t();
                            	new_dec[i].data = null;
                            	new_dec[i].data_current_size = 0;
                            	new_dec[i].data_max_size = 0;
                            	new_dec[i].m_current_max_segs = 0;
                            	new_dec[i].numbps = 0;
                            	new_dec[i].numlenbits = 0;
                            	new_dec[i].numnewpasses = 0;
                            	new_dec[i].numsegs = 0;
                            	new_dec[i].real_num_segs = 0;
                            	new_dec[i].segs = null;	
							}

							l_current_precinct.dec = new_dec;
							/*
							 * fprintf(stderr,
							 * "\t\t\t\tReallocate cblks of a precinct (opj_tcd_cblk_dec_t): from %d to %d\n"
							 * ,l_current_precinct->block_size,
							 * l_nb_code_blocks_size);
							 */

							l_current_precinct.block_size = l_nb_code_blocks_size;
						}

						if (l_current_precinct.incltree == null) {
							l_current_precinct.incltree = opj_tgt_create(
									l_current_precinct.cw,
									l_current_precinct.ch);
						} else {
							l_current_precinct.incltree = opj_tgt_init(
									l_current_precinct.incltree,
									l_current_precinct.cw,
									l_current_precinct.ch);
						}

						if (l_current_precinct.incltree == null) {
							Preferences.debug("No incltree created.\n",
									Preferences.DEBUG_FILEIO);
							/* return OPJ_FALSE; */
						}

						if (l_current_precinct.imsbtree == null) {
							l_current_precinct.imsbtree = opj_tgt_create(
									l_current_precinct.cw,
									l_current_precinct.ch);
						} else {
							l_current_precinct.imsbtree = opj_tgt_init(
									l_current_precinct.imsbtree,
									l_current_precinct.cw,
									l_current_precinct.ch);
						}

						if (l_current_precinct.imsbtree == null) {
							Preferences.debug("No imsbtree created.\n",
									Preferences.DEBUG_FILEIO);
							/* return OPJ_FALSE; */
						}

						for (cblkno = 0; cblkno < l_nb_code_blocks; ++cblkno) {
							int cblkxstart = tlcblkxstart
									+ (cblkno % l_current_precinct.cw)
									* (1 << cblkwidthexpn);
							int cblkystart = tlcblkystart
									+ (cblkno / l_current_precinct.cw)
									* (1 << cblkheightexpn);
							int cblkxend = cblkxstart + (1 << cblkwidthexpn);
							int cblkyend = cblkystart + (1 << cblkheightexpn);

							// if (isEncoder) {
							// opj_tcd_cblk_enc_t* l_code_block =
							// l_current_precinct->cblks.enc + cblkno;

							// if (!
							// opj_tcd_code_block_enc_allocate(l_code_block)) {
							// return OPJ_FALSE;
							// }
							/* code-block size (global) */
							// l_code_block->x0 = opj_int_max(cblkxstart,
							// l_current_precinct->x0);
							// l_code_block->y0 = opj_int_max(cblkystart,
							// l_current_precinct->y0);
							// l_code_block->x1 = opj_int_min(cblkxend,
							// l_current_precinct->x1);
							// l_code_block->y1 = opj_int_min(cblkyend,
							// l_current_precinct->y1);

							// if (!
							// opj_tcd_code_block_enc_allocate_data(l_code_block))
							// {
							// return OPJ_FALSE;
							// }
							// } else {
							opj_tcd_cblk_dec_t l_code_block = l_current_precinct.dec[cblkno];

							if (!opj_tcd_code_block_dec_allocate(l_code_block)) {
								return false;
							}
							/* code-block size (global) */
							l_code_block.x0 = Math.max(cblkxstart,
									l_current_precinct.x0);
							l_code_block.y0 = Math.max(cblkystart,
									l_current_precinct.y0);
							l_code_block.x1 = Math.min(cblkxend,
									l_current_precinct.x1);
							l_code_block.y1 = Math.min(cblkyend,
									l_current_precinct.y1);
							// }
						}
						if (precno < l_nb_precincts - 1) {
							l_current_precinct = l_band.precincts[precno + 1];
						}
					} /* precno */
					if (bandno < l_res.numbands - 1) {
						l_band = l_res.bands[bandno + 1];
					}
					if (l_tccp.stepsizes.length - 1 > l_step_size_index) {
						l_step_size = l_tccp.stepsizes[++l_step_size_index];
					}
				} /* bandno */
				if (resno < l_tilec.numresolutions - 1) {
					l_res = l_tilec.resolutions[resno + 1];
					--l_level_no;
				}
			} /* resno */
			if (compno < l_tile.numcomps - 1) {
				l_tccp = l_tcp.tccps[compno + 1];
				l_tilec = l_tile.comps[compno + 1];
				l_image_comp = p_tcd.image.comps[compno + 1];
			}
		} /* compno */
		return true;
	}

	private int opj_tcd_get_decoded_tile_size(opj_tcd_t p_tcd) {
		int i;
		int l_data_size = 0;
		opj_image_comp_t l_img_comp = null;
		opj_tcd_tilecomp_t l_tile_comp = null;
		opj_tcd_resolution_t l_res = null;
		int l_size_comp, l_remaining;

		l_tile_comp = p_tcd.tcd_image.tiles.comps[0];
		l_img_comp = p_tcd.image.comps[0];

		for (i = 0; i < p_tcd.image.numcomps; ++i) {
			l_size_comp = l_img_comp.prec >>> 3; /* (/ 8) */
			l_remaining = l_img_comp.prec & 7; /* (%8) */

			if (l_remaining != 0) {
				++l_size_comp;
			}

			if (l_size_comp == 3) {
				l_size_comp = 4;
			}

			l_res = l_tile_comp.resolutions[l_tile_comp.minimum_num_resolutions - 1];
			l_data_size += l_size_comp
					* ((l_res.x1 - l_res.x0) * (l_res.y1 - l_res.y0));
			if (i < p_tcd.image.numcomps - 1) {
				l_img_comp = p_tcd.image.comps[i + 1];
				l_tile_comp = p_tcd.tcd_image.tiles.comps[i + 1];
			}
		}

		return l_data_size;
	}

	private boolean opj_alloc_tile_component_data(opj_tcd_tilecomp_t l_tilec) {
		if ((l_tilec.data == null)
				|| ((l_tilec.data_size_needed > l_tilec.data_size) && (l_tilec.ownsData == false))) {
			l_tilec.data = new int[l_tilec.data_size_needed];

			/*
			 * fprintf(stderr,
			 * "tAllocate data of tilec (int): %d x OPJ_UINT32n",l_data_size);
			 */
			l_tilec.data_size = l_tilec.data_size_needed;
			l_tilec.ownsData = true;
		} else if (l_tilec.data_size_needed > l_tilec.data_size) {
			/* We don't need to keep old data */
			l_tilec.data = null;
			l_tilec.data = new int[l_tilec.data_size_needed];
			/*
			 * fprintf(stderr,
			 * "tReallocate data of tilec (int): from %d to %d x OPJ_UINT32n",
			 * l_tilec->data_size, l_data_size);
			 */
			l_tilec.data_size = l_tilec.data_size_needed;
			l_tilec.ownsData = true;
		}
		return true;
	}

	private void opj_j2k_tcp_destroy(opj_tcp_t p_tcp) {
		if (p_tcp == null) {
			return;
		}

		if (p_tcp.ppt_markers != null) {
			int i;
			for (i = 0; i < p_tcp.ppt_markers_count; ++i) {
				if (p_tcp.ppt_markers[i].m_data != null) {
					p_tcp.ppt_markers[i].m_data = null;
				}
			}
			p_tcp.ppt_markers_count = 0;
			p_tcp.ppt_markers = null;
		}

		if (p_tcp.ppt_buffer != null) {
			p_tcp.ppt_buffer = null;
		}

		if (p_tcp.tccps != null) {
			p_tcp.tccps = null;
		}

		if (p_tcp.m_mct_coding_matrix != null) {
			p_tcp.m_mct_coding_matrix = null;
		}

		if (p_tcp.m_mct_decoding_matrix != null) {
			p_tcp.m_mct_decoding_matrix = null;
		}

		if (p_tcp.m_mcc_records != null) {
			p_tcp.m_mcc_records = null;
			p_tcp.m_nb_max_mcc_records = 0;
			p_tcp.m_nb_mcc_records = 0;
		}

		if (p_tcp.m_mct_records != null) {
			opj_mct_data_t l_mct_data = p_tcp.m_mct_records[0];
			int i;

			for (i = 0; i < p_tcp.m_nb_mct_records; ++i) {
				if (l_mct_data.m_data != null) {
					l_mct_data.m_data = null;
				}

				if (i < p_tcp.m_nb_mct_records - 1) {
					l_mct_data = p_tcp.m_mct_records[i + 1];
				}
			}

			p_tcp.m_mct_records = null;
		}

		if (p_tcp.mct_norms != null) {
			p_tcp.mct_norms = null;
		}

		opj_j2k_tcp_data_destroy(p_tcp);

	}

	private void opj_j2k_tcp_data_destroy(opj_tcp_t p_tcp) {
		if (p_tcp.m_data != null) {
			p_tcp.m_data = null;
			p_tcp.m_data_size = 0;
		}
	}

	private boolean opj_tcd_decode_tile(opj_tcd_t p_tcd, byte p_src[],
			int p_max_length, int p_tile_no, opj_codestream_index_t p_cstr_index) {
		int l_data_read[] = new int[1];
		p_tcd.tcd_tileno = p_tile_no;
		p_tcd.tcp = p_tcd.cp.tcps[p_tile_no];
		

		// if (TODO_MSD) { /* No p_cstr_info structure */
		// if(p_cstr_info) {
		// OPJ_UINT32 resno, compno, numprec = 0;
		// for (compno = 0; compno < (OPJ_UINT32) p_cstr_info->numcomps;
		// compno++) {
		// opj_tcp_t *tcp = &p_tcd->cp->tcps[0];
		// opj_tccp_t *tccp = &tcp->tccps[compno];
		// opj_tcd_tilecomp_t *tilec_idx =
		// &p_tcd->tcd_image->tiles->comps[compno];
		// for (resno = 0; resno < tilec_idx->numresolutions; resno++) {
		// opj_tcd_resolution_t *res_idx = &tilec_idx->resolutions[resno];
		// p_cstr_info->tile[p_tile_no].pw[resno] = res_idx->pw;
		// p_cstr_info->tile[p_tile_no].ph[resno] = res_idx->ph;
		// numprec += res_idx->pw * res_idx->ph;
		// p_cstr_info->tile[p_tile_no].pdx[resno] = tccp->prcw[resno];
		// p_cstr_info->tile[p_tile_no].pdy[resno] = tccp->prch[resno];
		// }
		// }
		// p_cstr_info->tile[p_tile_no].packet = (opj_packet_info_t *)
		// opj_malloc(p_cstr_info->numlayers * numprec *
		// sizeof(opj_packet_info_t));
		// p_cstr_info->packno = 0;
		// }
		// } // if (TODO_MSD)

		/*--------------TIER2------------------*/
		/* FIXME _ProfStart(PGROUP_T2); */
		if (!opj_tcd_t2_decode(p_tcd, p_src, l_data_read, p_max_length,
				p_cstr_index)) {
			return false;
		}
		/* FIXME _ProfStop(PGROUP_T2); */

		/*------------------TIER1-----------------*/

		/* FIXME _ProfStart(PGROUP_T1); */
		if (!opj_tcd_t1_decode(p_tcd)) {
			return false;
		}
		/* FIXME _ProfStop(PGROUP_T1); */

		/*----------------DWT---------------------*/

		/* FIXME _ProfStart(PGROUP_DWT); */
		if (!opj_tcd_dwt_decode(p_tcd)) {
			return false;
		}
		/* FIXME _ProfStop(PGROUP_DWT); */

		/*----------------MCT-------------------*/
		/* FIXME _ProfStart(PGROUP_MCT); */
		if (!opj_tcd_mct_decode(p_tcd)) {
			return false;
		}
		/* FIXME _ProfStop(PGROUP_MCT); */

		/* FIXME _ProfStart(PGROUP_DC_SHIFT); */
		if (!opj_tcd_dc_level_shift_decode(p_tcd)) {
			return false;
		}
		/* FIXME _ProfStop(PGROUP_DC_SHIFT); */

		/*---------------TILE-------------------*/
		return true;
	}

	private boolean opj_tcd_dwt_decode(opj_tcd_t p_tcd) {
		int compno;
		opj_tcd_tile_t l_tile = p_tcd.tcd_image.tiles;
		opj_tcd_tilecomp_t l_tile_comp = l_tile.comps[0];
		opj_tccp_t l_tccp = p_tcd.tcp.tccps[0];
		opj_image_comp_t l_img_comp = p_tcd.image.comps[0];

		for (compno = 0; compno < l_tile.numcomps; compno++) {
			/*
			 * if (tcd->cp->reduce != 0) {
			 * tcd->image->comps[compno].resno_decoded =
			 * tile->comps[compno].numresolutions - tcd->cp->reduce - 1; if
			 * (tcd->image->comps[compno].resno_decoded < 0) { return false; } }
			 * numres2decode = tcd->image->comps[compno].resno_decoded + 1;
			 * if(numres2decode > 0){
			 */

			if (l_tccp.qmfbid == 1) {
				if (!opj_dwt_decode(l_tile_comp, l_img_comp.resno_decoded + 1)) {
					return false;
				}
			} else {
				if (!opj_dwt_decode_real(l_tile_comp,
						l_img_comp.resno_decoded + 1)) {
					return false;
				}
			}
			if (compno < l_tile.numcomps - 1) {
				l_tile_comp = l_tile.comps[compno + 1];
				l_img_comp = p_tcd.image.comps[compno + 1];
				l_tccp = p_tcd.tcp.tccps[compno + 1];
			}
		}

		return true;
	}

	/* <summary> */
	/* Inverse 5-3 wavelet transform in 2-D. */
	/* </summary> */
	private boolean opj_dwt_decode(opj_tcd_tilecomp_t tilec, int numres) {
		return opj_dwt_decode_tile(tilec, numres, OPJ_DWT_DECODE_1);
	}

	/* <summary> */
	/* Inverse wavelet transform in 2-D. */
	/* </summary> */
	private boolean opj_dwt_decode_tile(opj_tcd_tilecomp_t tilec, int numres,
			int dwt_1D) {
		opj_dwt_t h = new opj_dwt_t();
		opj_dwt_t v = new opj_dwt_t();

		opj_tcd_resolution_t tr = tilec.resolutions[0];

		int rw = (tr.x1 - tr.x0); /* width of the resolution level computed */
		int rh = (tr.y1 - tr.y0); /* height of the resolution level computed */

		int w = (tilec.x1 - tilec.x0);

		if (numres == 1) {
			return true;
		}

		int mr = 0;
		int i = numres;
		int j = 0;
		opj_tcd_resolution_t r = tilec.resolutions[j];
		while (--i != 0) {
			r = tilec.resolutions[++j];
			if (mr < (w = (r.x1 - r.x0)))
				mr = w;
			if (mr < (w = (r.y1 - r.y0)))
				mr = w;
		}

		h.mem = new int[mr];
		if (h.mem == null) {
			/* FIXME event manager error callback */
			return false;
		}

		v.mem = h.mem;

		int p = 0;
		while (--numres != 0) {
			int tiledp[] = tilec.data;

			tr = tilec.resolutions[++p];
			h.sn = rw;
			v.sn = rh;
			rw = (tr.x1 - tr.x0);
			rh = (tr.y1 - tr.y0);
			h.dn = (rw - h.sn);
			h.cas = tr.x0 % 2;

			for (j = 0; j < rh; ++j) {
				opj_dwt_interleave_h(h, tiledp, j * w);
				switch (dwt_1D) {
				case OPJ_DWT_DECODE_1:
					opj_dwt_decode_1(h);
					break;
				}
				for (i = 0; i < rw; i++) {
					tiledp[j * w + i] = h.mem[i];
				}
			}

			v.dn = (rh - v.sn);
			v.cas = tr.y0 % 2;

			for (j = 0; j < rw; ++j) {
				int k;
				opj_dwt_interleave_v(v, tiledp, j, w);
				switch (dwt_1D) {
				case OPJ_DWT_DECODE_1:
					opj_dwt_decode_1(v);
					break;
				}
				for (k = 0; k < rh; ++k) {
					tiledp[k * w + j] = v.mem[k];
				}
			}
		}
		h.mem = null;
		return true;
	}

	/* <summary> */
	/* Inverse 5-3 wavelet transform in 1-D. */
	/* </summary> */
	private void opj_dwt_decode_1_(int a[], int dn, int sn, int cas) {
		int i;

		if (cas == 0) {
			if ((dn > 0) || (sn > 1)) { /* NEW : CASE ONE ELEMENT */
				for (i = 0; i < sn; i++)
					a[2 * i] -= (OPJ_D_(a, i - 1, dn) + OPJ_D_(a, i, dn) + 2) >> 2;
				for (i = 0; i < dn; i++)
					a[2 * i + 1] += (OPJ_S_(a, i, sn) + OPJ_S_(a, i + 1, sn)) >> 1;
			}
		} else {
			if (sn == 0 && dn == 1) /* NEW : CASE ONE ELEMENT */
				a[0] /= 2;
			else {
				for (i = 0; i < sn; i++)
					a[2 * i + 1] -= (OPJ_SS_(a, i, dn) + OPJ_SS_(a, i + 1, dn) + 2) >> 2;
				for (i = 0; i < dn; i++)
					a[2 * i] += (OPJ_DD_(a, i, sn) + OPJ_DD_(a, i - 1, sn)) >> 1;
			}
		}
	}

	private int OPJ_S_(int a[], int i, int sn) {
		if (i < 0) {
			return a[0];
		} else if (i >= sn) {
			return a[2 * (sn - 1)];
		} else {
			return a[2 * i];
		}
	}

	private int OPJ_D_(int a[], int i, int dn) {
		if (i < 0) {
			return a[1];
		} else if (i >= dn) {
			return a[2 * (dn - 1) + 1];
		} else {
			return a[2 * i + 1];
		}

	}

	private int OPJ_SS_(int a[], int i, int dn) {
		if (i < 0) {
			return a[0];
		} else if (i >= dn) {
			return a[2 * (dn - 1)];
		} else {
			return a[2 * i];
		}
	}

	private int OPJ_DD_(int a[], int i, int sn) {
		if (i < 0) {
			return a[1];
		} else if (i >= sn) {
			return a[2 * (sn - 1) + 1];
		} else {
			return a[2 * i + 1];
		}
	}

	/* <summary> */
	/* Inverse 5-3 wavelet transform in 1-D. */
	/* </summary> */
	private void opj_dwt_decode_1(opj_dwt_t v) {
		opj_dwt_decode_1_(v.mem, v.dn, v.sn, v.cas);
	}

	/* <summary> */
	/* Inverse lazy transform (horizontal). */
	/* </summary> */
	private void opj_dwt_interleave_h(opj_dwt_t h, int a[], int a_index) {
		int ai = a_index;
		// OPJ_INT32 *bi = h->mem + h->cas;
		int bi = h.cas;
		int i = h.sn;
		
		while (i-- != 0) {
			h.mem[bi] = a[ai++];
			bi += 2;
		}
		ai = h.sn;
		// bi = h->mem + 1 - h->cas;
		bi = 1 - h.cas;
		i = h.dn;
		while (i-- != 0) {
			h.mem[bi] = a[ai++];
			bi += 2;
		}
	}

	/* <summary> */
	/* Inverse lazy transform (vertical). */
	/* </summary> */
	private void opj_dwt_interleave_v(opj_dwt_t v, int a[], int a_index, int x) {
		int ai = a_index;
		// OPJ_INT32 *bi = v->mem + v->cas;
		int bi = v.cas;
		int i = v.sn;
		while (i-- != 0) {
			v.mem[bi] = a[ai];
			bi += 2;
			ai += x;
		}
		// ai = a + (v->sn * x);
		ai = v.sn * x;
		// bi = v->mem + 1 - v->cas;
		bi = 1 - v.cas;
		i = v.dn;
		while (i-- != 0) {
			v.mem[bi] = a[ai];
			bi += 2;
			ai += x;
		}
	}

	/* <summary> */
	/* Inverse 9-7 wavelet transform in 2-D. */
	/* </summary> */
	private boolean opj_dwt_decode_real(opj_tcd_tilecomp_t tilec, int numres) {
		opj_v4dwt_t h = new opj_v4dwt_t();
		opj_v4dwt_t v = new opj_v4dwt_t();
		;

		opj_tcd_resolution_t res = tilec.resolutions[0];

		int rw = (res.x1 - res.x0); /* width of the resolution level computed */
		int rh = (res.y1 - res.y0); /* height of the resolution level computed */

		int w = (tilec.x1 - tilec.x0);

		int mr = 0;
		int i = numres;
		int j = 0;
		opj_tcd_resolution_t r = tilec.resolutions[j];
		while (--i != 0) {
			r = tilec.resolutions[j++];
			if (mr < (w = (r.x1 - r.x0)))
				mr = w;
			if (mr < (w = (r.y1 - r.y0)))
				mr = w;
		}
		h.wavelet = new opj_v4_t[mr + 5];
		for (i = 0; i < mr + 5; i++) {
			h.wavelet[i] = new opj_v4_t();
		}

		v.wavelet = h.wavelet;

		int p = 0;
		res = tilec.resolutions[p];
		float aj[] = new float[tilec.data.length];
		for (i = 0; i < tilec.data.length; i++) {
			aj[i] = (float) tilec.data[i];
		}
		int aj_index = 0;
		while (--numres != 0) {
			// OPJ_FLOAT32 * restrict aj = (OPJ_FLOAT32*) tilec.data;
			aj_index = 0;
			int bufsize = ((tilec.x1 - tilec.x0) * (tilec.y1 - tilec.y0));

			h.sn = rw;
			v.sn = rh;

			res = tilec.resolutions[p++];

			rw = (res.x1 - res.x0); /* width of the resolution level computed */
			rh = (res.y1 - res.y0); /* height of the resolution level computed */

			h.dn = (rw - h.sn);
			h.cas = res.x0 % 2;

			for (j = rh; j > 3; j -= 4) {
				int k;
				opj_v4dwt_interleave_h(h, aj, aj_index, w, bufsize);
				opj_v4dwt_decode(h);

				for (k = rw; --k >= 0;) {
					aj[aj_index + k] = h.wavelet[k].f[0];
					aj[aj_index + k + w] = h.wavelet[k].f[1];
					aj[aj_index + k + w * 2] = h.wavelet[k].f[2];
					aj[aj_index + k + w * 3] = h.wavelet[k].f[3];
				}

				aj_index += w * 4;
				bufsize -= w * 4;
			}

			if ((rh & 0x03) != 0) {
				int k;
				j = rh & 0x03;
				opj_v4dwt_interleave_h(h, aj, aj_index, w, bufsize);
				opj_v4dwt_decode(h);
				for (k = rw; --k >= 0;) {
					switch (j) {
					case 3:
						aj[aj_index + k + w * 2] = h.wavelet[k].f[2];
					case 2:
						aj[aj_index + k + w] = h.wavelet[k].f[1];
					case 1:
						aj[aj_index + k] = h.wavelet[k].f[0];
					}
				}
			}

			v.dn = (rh - v.sn);
			v.cas = res.y0 % 2;

			// aj = (OPJ_FLOAT32*) tilec->data;
			aj_index = 0;
			for (j = rw; j > 3; j -= 4) {
				int k;

				opj_v4dwt_interleave_v(v, aj, aj_index, w, 4);
				opj_v4dwt_decode(v);

				for (k = 0; k < rh; ++k) {
					for (i = 0; i < 4; i++) {
						aj[aj_index + k * w + i] = v.wavelet[k].f[i];
					}
				}
				aj_index += 4;
			}

			if ((rw & 0x03) != 0) {
				int k;

				j = rw & 0x03;

				opj_v4dwt_interleave_v(v, aj, aj_index, w, j);
				opj_v4dwt_decode(v);

				for (k = 0; k < rh; ++k) {
					for (i = 0; i < j; i++) {
						aj[aj_index + k * w + i] = v.wavelet[k + i / 4].f[i % 4];
					}
				}
			}
		}
		for (i = 0; i < tilec.data.length; i++) {
			tilec.data[i] = (int) aj[i];
		}
		h.wavelet = null;
		return true;
	}

	private void opj_v4dwt_interleave_v(opj_v4dwt_t v, float a[], int a_index,
			int x, int nb_elts_read) {
		// opj_v4_t* restrict bi = v->wavelet + v->cas;
		int bi = v.cas;
		int i;
		int j;

		for (i = 0; i < v.sn; ++i) {
			for (j = 0; j < nb_elts_read; j++) {
				v.wavelet[bi + (2 * i + j) / 4].f[(2 * i + j) % 4] = a[a_index
						+ i * x + j];
			}
		}

		a_index += v.sn * x;
		// bi = v->wavelet + 1 - v->cas;
		bi = 1 - v.cas;

		for (i = 0; i < v.dn; ++i) {
			for (j = 0; j < nb_elts_read; j++) {
				v.wavelet[bi + (2 * i + j) / 4].f[(2 * i + j) % 4] = a[a_index
						+ i * x + j];
			}
		}
	}

	/* <summary> */
	/* Inverse 9-7 wavelet transform in 1-D. */
	/* </summary> */
	private void opj_v4dwt_decode(opj_v4dwt_t dwt) {
		int a, b;
		if (dwt.cas == 0) {
			if (!((dwt.dn > 0) || (dwt.sn > 1))) {
				return;
			}
			a = 0;
			b = 1;
		} else {
			if (!((dwt.sn > 0) || (dwt.dn > 1))) {
				return;
			}
			a = 1;
			b = 0;
		}
		// if (SSE) {
		// opj_v4dwt_decode_step1_sse(dwt->wavelet+a, dwt->sn,
		// _mm_set1_ps(opj_K));
		// opj_v4dwt_decode_step1_sse(dwt->wavelet+b, dwt->dn,
		// _mm_set1_ps(opj_c13318));
		// opj_v4dwt_decode_step2_sse(dwt->wavelet+b, dwt->wavelet+a+1, dwt->sn,
		// opj_int_min(dwt->sn, dwt->dn-a), _mm_set1_ps(opj_dwt_delta));
		// opj_v4dwt_decode_step2_sse(dwt->wavelet+a, dwt->wavelet+b+1, dwt->dn,
		// opj_int_min(dwt->dn, dwt->sn-b), _mm_set1_ps(opj_dwt_gamma));
		// opj_v4dwt_decode_step2_sse(dwt->wavelet+b, dwt->wavelet+a+1, dwt->sn,
		// opj_int_min(dwt->sn, dwt->dn-a), _mm_set1_ps(opj_dwt_beta));
		// opj_v4dwt_decode_step2_sse(dwt->wavelet+a, dwt->wavelet+b+1, dwt->dn,
		// opj_int_min(dwt->dn, dwt->sn-b), _mm_set1_ps(opj_dwt_alpha));
		// }
		// else {
		opj_v4dwt_decode_step1(dwt.wavelet, a, dwt.sn, opj_K);
		opj_v4dwt_decode_step1(dwt.wavelet, b, dwt.dn, opj_c13318);
		opj_v4dwt_decode_step2(dwt.wavelet, b, dwt.wavelet, a + 1, dwt.sn,
				Math.min(dwt.sn, dwt.dn - a), opj_dwt_delta);
		opj_v4dwt_decode_step2(dwt.wavelet, a, dwt.wavelet, b + 1, dwt.dn,
				Math.min(dwt.dn, dwt.sn - b), opj_dwt_gamma);
		opj_v4dwt_decode_step2(dwt.wavelet, b, dwt.wavelet, a + 1, dwt.sn,
				Math.min(dwt.sn, dwt.dn - a), opj_dwt_beta);
		opj_v4dwt_decode_step2(dwt.wavelet, a, dwt.wavelet, b + 1, dwt.dn,
				Math.min(dwt.dn, dwt.sn - b), opj_dwt_alpha);
		// }
	}

	private void opj_v4dwt_decode_step1(opj_v4_t w[], int w_index, int count,
			final float c) {
		// OPJ_FLOAT32* restrict fw = (OPJ_FLOAT32*) w;
		int i;
		for (i = 0; i < count; ++i) {
			float tmp1 = w[w_index + i * 2].f[0];
			float tmp2 = w[w_index + i * 2].f[1];
			float tmp3 = w[w_index + i * 2].f[2];
			float tmp4 = w[w_index + i * 2].f[3];
			w[w_index + i * 2].f[0] = tmp1 * c;
			w[w_index + i * 2].f[1] = tmp2 * c;
			w[w_index + i * 2].f[2] = tmp3 * c;
			w[w_index + i * 2].f[3] = tmp4 * c;
		}
	}

	static void opj_v4dwt_decode_step2(opj_v4_t l[], int l_index, opj_v4_t w[],
			int w_index, int k, int m, float c) {
		// OPJ_FLOAT32* fl = (OPJ_FLOAT32*) l;
		// OPJ_FLOAT32* fw = (OPJ_FLOAT32*) w;
		int i;
		for (i = 0; i < m; ++i) {
			float tmp1_1 = l[l_index].f[0];
			float tmp1_2 = l[l_index].f[1];
			float tmp1_3 = l[l_index].f[2];
			float tmp1_4 = l[l_index].f[3];
			float tmp2_1 = w[w_index - 1].f[0];
			float tmp2_2 = w[w_index - 1].f[1];
			float tmp2_3 = w[w_index - 1].f[2];
			float tmp2_4 = w[w_index - 1].f[3];
			float tmp3_1 = w[w_index].f[0];
			float tmp3_2 = w[w_index].f[1];
			float tmp3_3 = w[w_index].f[2];
			float tmp3_4 = w[w_index].f[3];
			w[w_index - 1].f[0] = tmp2_1 + ((tmp1_1 + tmp3_1) * c);
			w[w_index - 1].f[1] = tmp2_2 + ((tmp1_2 + tmp3_2) * c);
			w[w_index - 1].f[2] = tmp2_3 + ((tmp1_3 + tmp3_3) * c);
			w[w_index - 1].f[3] = tmp2_4 + ((tmp1_4 + tmp3_4) * c);
			l = w;
			l_index = w_index;
			w_index += 2;
		}
		if (m < k) {
			float c1;
			float c2;
			float c3;
			float c4;
			c += c;
			c1 = l[l_index].f[0] * c;
			c2 = l[l_index].f[1] * c;
			c3 = l[l_index].f[2] * c;
			c4 = l[l_index].f[3] * c;
			for (; m < k; ++m) {
				float tmp1 = w[w_index - 1].f[0];
				float tmp2 = w[w_index - 1].f[1];
				float tmp3 = w[w_index - 1].f[2];
				float tmp4 = w[w_index - 1].f[3];
				w[w_index - 1].f[0] = tmp1 + c1;
				w[w_index - 1].f[1] = tmp2 + c2;
				w[w_index - 1].f[2] = tmp3 + c3;
				w[w_index - 1].f[3] = tmp4 + c4;
				w_index += 2;
			}
		}
	}

	private void opj_v4dwt_interleave_h(opj_v4dwt_t w, float a[], int a_index,
			int x, int size) {
		// OPJ_FLOAT32* restrict bi = (OPJ_FLOAT32*) (w->wavelet + w->cas);
		// 8*i in float goes to 2*i in opj_v4_t
		int bi = w.cas;
		int count = w.sn;
		int i, k;

		for (k = 0; k < 2; ++k) {
			if (count + 3 * x < size && (4 * a.length & 0x0f) == 0
					&& (4 * w.wavelet[w.cas].f.length & 0x0f) == 0
					&& (x & 0x0f) == 0) {
				/* Fast code path */
				for (i = 0; i < count; ++i) {
					int j = i;
					w.wavelet[bi + 2 * i].f[0] = a[a_index + j];
					// bi[i*8 ] = a[j];
					j += x;
					w.wavelet[bi + 2 * i].f[1] = a[a_index + j];
					// bi[i*8 + 1] = a[j];
					j += x;
					w.wavelet[bi + 2 * i].f[2] = a[a_index + j];
					// bi[i*8 + 2] = a[j];
					j += x;
					w.wavelet[bi + 2 * i].f[3] = a[a_index + j];
					// bi[i*8 + 3] = a[j];
				}
			} else {
				/* Slow code path */
				for (i = 0; i < count; ++i) {
					int j = i;
					w.wavelet[bi + 2 * i].f[0] = a[a_index + j];
					// bi[i*8 ] = a[j];
					j += x;
					if (j >= size)
						continue;
					w.wavelet[bi + 2 * i].f[1] = a[a_index + j];
					// bi[i*8 + 1] = a[j];
					j += x;
					if (j >= size)
						continue;
					w.wavelet[bi + 2 * i].f[2] = a[a_index + j];
					// bi[i*8 + 2] = a[j];
					j += x;
					if (j >= size)
						continue;
					w.wavelet[bi + 2 * i].f[3] = a[a_index + j];
					// bi[i*8 + 3] = a[+j]; /* This one*/
				}
			}

			// bi = (OPJ_FLOAT32*) (w->wavelet + 1 - w->cas);
			bi = 1 - w.cas;
			a_index += w.sn;
			size -= w.sn;
			count = w.dn;
		}
	}

	private boolean opj_tcd_mct_decode(opj_tcd_t p_tcd) {
		opj_tcd_tile_t l_tile = p_tcd.tcd_image.tiles;
		opj_tcp_t l_tcp = p_tcd.tcp;
		opj_tcd_tilecomp_t l_tile_comp[] = l_tile.comps;
		int l_samples, i, j;

		if (l_tcp.mct == 0) {
			return true;
		}

		l_samples = ((l_tile_comp[0].x1 - l_tile_comp[0].x0) * (l_tile_comp[0].y1 - l_tile_comp[0].y0));

		if (l_tile.numcomps >= 3) {
			/* testcase 1336.pdf.asan.47.376 */
			if ((l_tile.comps[0].x1 - l_tile.comps[0].x0)
					* (l_tile.comps[0].y1 - l_tile.comps[0].y0) < l_samples
					|| (l_tile.comps[1].x1 - l_tile.comps[1].x0)
							* (l_tile.comps[1].y1 - l_tile.comps[1].y0) < l_samples
					|| (l_tile.comps[2].x1 - l_tile.comps[2].x0)
							* (l_tile.comps[2].y1 - l_tile.comps[2].y0) < l_samples) {
				MipavUtil
						.displayError("Tiles don't all have the same dimension. Skip the MCT step.");
				return false;
			} else if (l_tcp.mct == 2) {
				byte l_data[][];

				if (l_tcp.m_mct_decoding_matrix == null) {
					return true;
				}

				l_data = new byte[l_tile.numcomps][];

				for (i = 0; i < l_tile.numcomps; ++i) {
					l_data[i] = new byte[4 * l_tile_comp[i].data.length];
					for (j = 0; j < l_tile_comp[i].data.length; j++) {
						l_data[i][4 * j] = (byte) (l_tile_comp[i].data[j] >>> 24);
						l_data[i][4 * j + 1] = (byte) (l_tile_comp[i].data[j] >>> 16);
						l_data[i][4 * j + 2] = (byte) (l_tile_comp[i].data[j] >>> 8);
						l_data[i][4 * j + 3] = (byte) (l_tile_comp[i].data[j] & 0xff);
					}
				}

				if (!opj_mct_decode_custom(/* MCT data */
				l_tcp.m_mct_decoding_matrix,
				/* size of components */
				l_samples,
				/* components */
				l_data,
				/* nb of components (i.e. size of pData) */
				l_tile.numcomps,
				/* tells if the data is signed */
				p_tcd.image.comps[0].sgnd)) {
					for (i = 0; i < l_tile.numcomps; i++) {
						l_data[i] = null;
					}
					l_data = null;
					return false;
				}

				for (i = 0; i < l_tile.numcomps; i++) {
					l_data[i] = null;
				}
				l_data = null;
			} else {
				if (l_tcp.tccps[0].qmfbid == 1) {
					opj_mct_decode(l_tile.comps[0].data, l_tile.comps[1].data,
							l_tile.comps[2].data, l_samples);
				} else {
					opj_mct_decode_real(l_tile.comps[0].data,
							l_tile.comps[1].data, l_tile.comps[2].data,
							l_samples);
				}
			}
		} else {
			MipavUtil.displayError("Number of components (" + l_tile.numcomps
					+ ") is inconsistent with a MCT. Skip the MCT step.");
		}

		return true;
	}

	/* <summary> */
	/* Inverse irreversible MCT. */
	/* </summary> */
	void opj_mct_decode_real(int c0[], int c1[], int c2[], int n) {
		int i;
		for (i = 0; i < n; ++i) {
			float y = Float.intBitsToFloat(c0[i]);
			float u = Float.intBitsToFloat(c1[i]);
			float v = Float.intBitsToFloat(c2[i]);
			float r = y + (v * 1.402f);
			float g = y - (u * 0.34413f) - (v * (0.71414f));
			float b = y + (u * 1.772f);
			c0[i] = Float.floatToIntBits(r);
			c1[i] = Float.floatToIntBits(g);
			c2[i] = Float.floatToIntBits(b);
		}
	}

	private void opj_mct_decode(int c0[], int c1[], int c2[], int n) {
		int i;
		for (i = 0; i < n; ++i) {
			int y = c0[i];
			int u = c1[i];
			int v = c2[i];
			int g = y - ((u + v) >> 2);
			int r = v + g;
			int b = u + g;
			c0[i] = r;
			c1[i] = g;
			c2[i] = b;
		}
	}

	private boolean opj_mct_decode_custom(float pDecodingData[], int n,
			byte pData[][], int pNbComp, int isSigned) {
		int i;
		int j;
		int k;
		int tmpInt;

		float lCurrentData[] = null;

		// OPJ_ARG_NOT_USED(isSigned);

		lCurrentData = new float[2 * pNbComp];

		int pDecodingDataIndex;
		for (i = 0; i < n; ++i) {
			pDecodingDataIndex = 0;
			for (j = 0; j < pNbComp; ++j) {
				lCurrentData[j] = FileBase.bytesToFloat(true, 0, pData[j]);
			}
			for (j = 0; j < pNbComp; ++j) {
				lCurrentData[pNbComp + j] = 0;
				for (k = 0; k < pNbComp; ++k) {
					lCurrentData[pNbComp + j] += pDecodingData[pDecodingDataIndex++]
							* lCurrentData[k];
				}
				tmpInt = Float.floatToIntBits(lCurrentData[pNbComp + j]);
				pData[j][4 * i] = (byte) (tmpInt >>> 24);
				pData[j][4 * i + 1] = (byte) (tmpInt >>> 16);
				pData[j][4 * i + 2] = (byte) (tmpInt >>> 8);
				pData[j][4 * i + 3] = (byte) (tmpInt & 0xff);
			}
		}
		lCurrentData = null;
		return true;
	}

	private boolean opj_tcd_dc_level_shift_decode(opj_tcd_t p_tcd) {
		int compno;
		opj_tcd_tilecomp_t l_tile_comp[] = null;
		opj_tccp_t l_tccp[] = null;
		opj_image_comp_t l_img_comp[] = null;
		opj_tcd_resolution_t l_res = null;
		opj_tcd_tile_t l_tile;
		int l_width, l_height, i, j;
		int l_current_ptr;
		int l_min, l_max;
		int l_stride;

		l_tile = p_tcd.tcd_image.tiles;
		l_tile_comp = l_tile.comps;
		l_tccp = p_tcd.tcp.tccps;
		l_img_comp = p_tcd.image.comps;

		for (compno = 0; compno < l_tile.numcomps; compno++) {
			l_res = l_tile_comp[compno].resolutions[l_img_comp[compno].resno_decoded];
			l_width = (l_res.x1 - l_res.x0);
			l_height = (l_res.y1 - l_res.y0);
			l_stride = (l_tile_comp[compno].x1 - l_tile_comp[compno].x0)
					- l_width;

			if (l_height != 0
					&& l_width + l_stride > l_tile_comp[compno].data_size
							/ l_height) { /* MUPDF */
				MipavUtil
						.displayError("l_width + l_stride > l_tile_comp[compno].data_size/l_height on opj_tcd_dc_level_shift_decode");
			}

			if (l_img_comp[compno].sgnd != 0) {
				l_min = -(1 << (l_img_comp[compno].prec - 1));
				l_max = (1 << (l_img_comp[compno].prec - 1)) - 1;
			} else {
				l_min = 0;
				l_max = (1 << l_img_comp[compno].prec) - 1;
			}

			// l_current_ptr = l_tile_comp[compno].data;
			l_current_ptr = 0;

			if (l_tccp[compno].qmfbid == 1) {
				for (j = 0; j < l_height; ++j) {
					for (i = 0; i < l_width; ++i) {
						l_tile_comp[compno].data[l_current_ptr] = opj_int_clamp(
								l_tile_comp[compno].data[l_current_ptr]
										+ l_tccp[compno].m_dc_level_shift,
								l_min, l_max);
						++l_current_ptr;
					}
					l_current_ptr += l_stride;
				}
			} else {
				for (j = 0; j < l_height; ++j) {
					for (i = 0; i < l_width; ++i) {
						float l_value = (float) l_tile_comp[compno].data[l_current_ptr];
						l_current_ptr = opj_int_clamp((int) opj_lrintf(l_value)
								+ l_tccp[compno].m_dc_level_shift, l_min, l_max);
						;
						++l_current_ptr;
					}
					l_current_ptr += l_stride;
				}
			}
		}

		return true;
	}

	private long opj_lrintf(float f) {
		return (long) ((f > 0.0f) ? (f + 0.5f) : (f - 0.5f));
	}

	/**
	 * Clamp an integer inside an interval
	 * 
	 * @return <ul>
	 *         <li>Returns a if (min < a < max)
	 *         <li>Returns max if (a > max)
	 *         <li>Returns min if (a < min)
	 *         </ul>
	 */
	private int opj_int_clamp(int a, int min, int max) {
		if (a < min)
			return min;
		if (a > max)
			return max;
		return a;
	}

	private boolean opj_tcd_update_tile_data(opj_tcd_t p_tcd, byte p_dest[],
			int l_dest_ptr[], int p_dest_length) {
		int i, j, k, l_data_size = 0;
		opj_image_comp_t l_img_comp[] = null;
		opj_tcd_tilecomp_t l_tilec[] = null;
		opj_tcd_resolution_t l_res;
		int l_size_comp, l_remaining;
		int l_stride, l_width, l_height;

		l_data_size = opj_tcd_get_decoded_tile_size(p_tcd);
		if (l_data_size > p_dest_length) {
			return false;
		}

		l_tilec = p_tcd.tcd_image.tiles.comps;
		l_img_comp = p_tcd.image.comps;

		for (i = 0; i < p_tcd.image.numcomps; ++i) {
			l_size_comp = l_img_comp[i].prec >>> 3; /* (/ 8) */
			l_remaining = l_img_comp[i].prec & 7; /* (%8) */
			l_res = l_tilec[i].resolutions[l_img_comp[i].resno_decoded];
			l_width = (l_res.x1 - l_res.x0);
			l_height = (l_res.y1 - l_res.y0);
			l_stride = (l_tilec[i].x1 - l_tilec[i].x0) - l_width;

			if (l_remaining != 0) {
				++l_size_comp;
			}

			if (l_size_comp == 3) {
				l_size_comp = 4;
			}

			switch (l_size_comp) {
			case 1: {
				// OPJ_CHAR * l_dest_ptr = (OPJ_CHAR *) p_dest;
				// const OPJ_INT32 * l_src_ptr = l_tilec->data;
				int l_src_ptr = 0;

				if (l_img_comp[i].sgnd != 0) {
					for (j = 0; j < l_height; ++j) {
						for (k = 0; k < l_width; ++k) {
							p_dest[l_dest_ptr[0]++] = (byte) l_tilec[i].data[l_src_ptr++];
						}
						l_src_ptr += l_stride;
					}
				} else {
					for (j = 0; j < l_height; ++j) {
						for (k = 0; k < l_width; ++k) {
							p_dest[l_dest_ptr[0]++] = (byte) (l_tilec[i].data[l_src_ptr++] & 0xff);
						}
						l_src_ptr += l_stride;
					}
				}

			}
				break;
			case 2: {
				// const OPJ_INT32 * l_src_ptr = l_tilec->data;
				// OPJ_INT16 * l_dest_ptr = (OPJ_INT16 *) p_dest;
				int l_src_ptr = 0;
				short tmpShort;

				if (l_img_comp[i].sgnd != 0) {
					for (j = 0; j < l_height; ++j) {
						for (k = 0; k < l_width; ++k) {
							tmpShort = (short) l_tilec[i].data[l_src_ptr++];
							p_dest[l_dest_ptr[0]++] = (byte) (tmpShort >>> 8);
							p_dest[l_dest_ptr[0]++] = (byte) (tmpShort & 0xff);
						}
						l_src_ptr += l_stride;
					}
				} else {
					for (j = 0; j < l_height; ++j) {
						for (k = 0; k < l_width; ++k) {
							tmpShort = (short) (l_tilec[i].data[l_src_ptr++] & 0xffff);
							p_dest[l_dest_ptr[0]++] = (byte) (tmpShort >>> 8);
							p_dest[l_dest_ptr[0]++] = (byte) (tmpShort & 0xff);
						}
						l_src_ptr += l_stride;
					}
				}

			}
				break;
			case 4: {
				// OPJ_INT32 * l_dest_ptr = (OPJ_INT32 *) p_dest;
				// OPJ_INT32 * l_src_ptr = l_tilec->data;
				int l_src_ptr = 0;
				int tmpInt;

				for (j = 0; j < l_height; ++j) {
					for (k = 0; k < l_width; ++k) {
						tmpInt = l_tilec[i].data[l_src_ptr++];
						p_dest[l_dest_ptr[0]++] = (byte) (tmpInt >>> 24);
						p_dest[l_dest_ptr[0]++] = (byte) (tmpInt >>> 16);
						p_dest[l_dest_ptr[0]++] = (byte) (tmpInt >>> 8);
						p_dest[l_dest_ptr[0]++] = (byte) (tmpInt & 0xff);
					}
					l_src_ptr += l_stride;
				}

			}
				break;
			}

		}

		return true;
	}

	private opj_tgt_tree_t opj_tgt_create(int numleafsh, int numleafsv) {
		int nplh[] = new int[32];
		int nplv[] = new int[32];
		opj_tgt_node_t node = null;
		opj_tgt_node_t l_parent_node = null;
		opj_tgt_node_t l_parent_node0 = null;
		opj_tgt_tree_t tree = null;
		int i;
		int j, k;
		int numlvls;
		int n;

		tree = new opj_tgt_tree_t();

		tree.numleafsh = numleafsh;
		tree.numleafsv = numleafsv;

		numlvls = 0;
		nplh[0] = numleafsh;
		nplv[0] = numleafsv;
		tree.numnodes = 0;
		do {
			n = (nplh[numlvls] * nplv[numlvls]);
			nplh[numlvls + 1] = (nplh[numlvls] + 1) / 2;
			nplv[numlvls + 1] = (nplv[numlvls] + 1) / 2;
			tree.numnodes += n;
			++numlvls;
		} while (n > 1);

		/* ADD */
		if (tree.numnodes == 0) {
			tree = null;
			Preferences
					.debug("Warning! tgt_create tree->numnodes == 0, no tree created.\n",
							Preferences.DEBUG_FILEIO);
			return null;
		}

		tree.nodes = new opj_tgt_node_t[tree.numnodes];
		for (i = 0; i < tree.numnodes; i++) {
			tree.nodes[i] = new opj_tgt_node_t();
		}

		// tree.nodes_size = tree.numnodes * (OPJ_UINT32)sizeof(opj_tgt_node_t);
		tree.nodes_size = tree.numnodes * 16;

		node = tree.nodes[0];
		int node_index = 0;
		if (tree.numleafsh * tree.numleafsv <= tree.numnodes-1) {
			l_parent_node = tree.nodes[tree.numleafsh * tree.numleafsv];
			int parent_node_index = tree.numleafsh * tree.numleafsv;
			l_parent_node0 = l_parent_node;
			int parent_node0_index = parent_node_index;
	
			for (i = 0; i < numlvls - 1; ++i) {
				for (j = 0; j < nplv[i]; ++j) {
					k = nplh[i];
					while (--k >= 0) {
						node.parent = l_parent_node;
						if (node_index < tree.nodes.length - 1) {
							node = tree.nodes[++node_index];
						}
						if (--k >= 0) {
							node.parent = l_parent_node;
							if (node_index < tree.nodes.length - 1) {
								node = tree.nodes[++node_index];
							}
						}
						if (parent_node_index < tree.nodes.length - 1) {
							l_parent_node = tree.nodes[++parent_node_index];
						}
					}
					if (((j & 1) != 0) || j == nplv[i] - 1) {
						l_parent_node0 = l_parent_node;
						parent_node0_index = parent_node_index;
					} else {
						l_parent_node = l_parent_node0;
						parent_node_index = parent_node0_index;
						if (parent_node0_index + nplh[i] <= tree.nodes.length - 1) {
							parent_node0_index += nplh[i];
							l_parent_node0 = tree.nodes[parent_node0_index];
						}
					}
				}
			}
		} // if (tree.numleafsh * tree.numleafsv <= tree.numnodes-1)
		node.parent = null;
		opj_tgt_reset(tree);
		return tree;
	}

	/**
	 * Reinitialises a tag-tree from an existing one.
	 * 
	 * @param p_tree
	 *            the tree to reinitialize.
	 * @param p_num_leafs_h
	 *            the width of the array of leafs of the tree
	 * @param p_num_leafs_v
	 *            the height of the array of leafs of the tree
	 * @return a new tag-tree if successful, NULL otherwise
	 */
	private opj_tgt_tree_t opj_tgt_init(opj_tgt_tree_t p_tree,
			int p_num_leafs_h, int p_num_leafs_v) {
		int l_nplh[] = new int[32];
		int l_nplv[] = new int[32];
		opj_tgt_node_t l_node = null;
		opj_tgt_node_t l_parent_node = null;
		opj_tgt_node_t l_parent_node0 = null;
		int i;
		int j, k;
		int l_num_levels;
		int n;
		int l_node_size;

		if (p_tree == null) {
			return null;
		}

		if ((p_tree.numleafsh != p_num_leafs_h)
				|| (p_tree.numleafsv != p_num_leafs_v)) {
			p_tree.numleafsh = p_num_leafs_h;
			p_tree.numleafsv = p_num_leafs_v;

			l_num_levels = 0;
			l_nplh[0] = p_num_leafs_h;
			l_nplv[0] = p_num_leafs_v;
			p_tree.numnodes = 0;
			do {
				n = (l_nplh[l_num_levels] * l_nplv[l_num_levels]);
				l_nplh[l_num_levels + 1] = (l_nplh[l_num_levels] + 1) / 2;
				l_nplv[l_num_levels + 1] = (l_nplv[l_num_levels] + 1) / 2;
				p_tree.numnodes += n;
				++l_num_levels;
			} while (n > 1);

			/* ADD */
			if (p_tree.numnodes == 0) {
				opj_tgt_destroy(p_tree);
				return null;
			}
			// l_node_size = p_tree->numnodes *
			// (OPJ_UINT32)sizeof(opj_tgt_node_t);
			l_node_size = p_tree.numnodes;

			if (l_node_size > p_tree.nodes_size) {
				opj_tgt_node_t new_nodes[] = new opj_tgt_node_t[l_node_size];
				for (i = 0; i < p_tree.nodes.length; i++) {
					new_nodes[i] = p_tree.nodes[i];
				}
				for (i = p_tree.nodes.length; i < l_node_size; i++) {
					new_nodes[i] = new opj_tgt_node_t();
				}
				p_tree.nodes = new_nodes;
				p_tree.nodes_size = l_node_size;
			}
			l_node = p_tree.nodes[0];
			int node_index = 0;
			l_parent_node = p_tree.nodes[p_tree.numleafsh * p_tree.numleafsv];
			int parent_node_index = p_tree.numleafsh * p_tree.numleafsv;
			l_parent_node0 = l_parent_node;
			int parent_node0_index = parent_node_index;

			for (i = 0; i < l_num_levels - 1; ++i) {
				for (j = 0; j < l_nplv[i]; ++j) {
					k = l_nplh[i];
					while (--k >= 0) {
						l_node.parent = l_parent_node;
						if (node_index < p_tree.nodes.length - 1) {
							l_node = p_tree.nodes[++node_index];
						}
						if (--k >= 0) {
							l_node.parent = l_parent_node;
							if (node_index < p_tree.nodes.length - 1) {
								l_node = p_tree.nodes[++node_index];
							}
						}
						if (parent_node_index < p_tree.nodes.length - 1) {
							l_parent_node = p_tree.nodes[++parent_node_index];
						}
					}
					if (((j & 1) != 0) || j == l_nplv[i] - 1) {
						l_parent_node0 = l_parent_node;
						parent_node0_index = parent_node_index;
					} else {
						l_parent_node = l_parent_node0;
						parent_node_index = parent_node0_index;
						if (parent_node0_index + l_nplh[i] <= p_tree.nodes.length - 1) {
							parent_node0_index += l_nplh[i];
							l_parent_node0 = p_tree.nodes[parent_node0_index];
						}
					}
				}
			}
			l_node.parent = null;
		}
		opj_tgt_reset(p_tree);

		return p_tree;
	}

	private void opj_tgt_destroy(opj_tgt_tree_t p_tree) {
		if (p_tree == null) {
			return;
		}

		if (p_tree.nodes != null) {
			p_tree.nodes = null;
		}
		p_tree = null;
	}

	private void opj_tgt_reset(opj_tgt_tree_t p_tree) {
		int i;
		opj_tgt_node_t l_current_node = null;

		if (p_tree == null) {
			return;
		}

		l_current_node = p_tree.nodes[0];
		for (i = 0; i < p_tree.numnodes; ++i) {
			l_current_node.value = 999;
			l_current_node.low = 0;
			l_current_node.known = 0;
			if (i < p_tree.numnodes - 1) {
				l_current_node = p_tree.nodes[i + 1];
			}
		}
	}

	/**
	 * Allocates memory for a decoding code block.
	 */
	private boolean opj_tcd_code_block_dec_allocate(
			opj_tcd_cblk_dec_t p_code_block) {
		int i;
		if (p_code_block.data == null) {

			p_code_block.data = new byte[OPJ_J2K_DEFAULT_CBLK_DATA_SIZE];
			if (p_code_block.data == null) {
				return false;
			}
			p_code_block.data_max_size = OPJ_J2K_DEFAULT_CBLK_DATA_SIZE;
			/* fprintf(stderr, "Allocate 8192 elements of code_block->data\n"); */

			p_code_block.segs = new opj_tcd_seg_t[OPJ_J2K_DEFAULT_NB_SEGS];
			for (i = 0; i < OPJ_J2K_DEFAULT_NB_SEGS; i++) {
				p_code_block.segs[i] = new opj_tcd_seg_t();
			}
			/*
			 * fprintf(stderr, "Allocate %d elements of code_block->data\n",
			 * OPJ_J2K_DEFAULT_NB_SEGS * sizeof(opj_tcd_seg_t));
			 */

			p_code_block.m_current_max_segs = OPJ_J2K_DEFAULT_NB_SEGS;
			/*
			 * fprintf(stderr, "m_current_max_segs of code_block->data = %d\n",
			 * p_code_block->m_current_max_segs);
			 */
		} else {
			/* sanitize */
			byte l_data[] = p_code_block.data;
			int l_data_max_size = p_code_block.data_max_size;
			opj_tcd_seg_t l_segs[] = p_code_block.segs;
			int l_current_max_segs = p_code_block.m_current_max_segs;

			// memset(p_code_block, 0, sizeof(opj_tcd_cblk_dec_t));
			p_code_block.data = l_data;
			p_code_block.data_max_size = l_data_max_size;
			p_code_block.segs = l_segs;
			p_code_block.m_current_max_segs = l_current_max_segs;
		}

		return true;
	}

	private boolean opj_tcd_t2_decode(opj_tcd_t p_tcd, byte p_src_data[],
			int p_data_read[], int p_max_src_size,
			opj_codestream_index_t p_cstr_index) {
		opj_t2_t l_t2;

		l_t2 = opj_t2_create(p_tcd.image, p_tcd.cp);
		if (l_t2 == null) {
			return false;
		}

		if (!opj_t2_decode_packets(l_t2, p_tcd.tcd_tileno,
				p_tcd.tcd_image.tiles, p_src_data, p_data_read, p_max_src_size,
				p_cstr_index)) {
			opj_t2_destroy(l_t2);
			return false;
		}
		
		opj_t2_destroy(l_t2);

		/*---------------CLEAN-------------------*/
		return true;
	}

	private boolean opj_t2_decode_packets(opj_t2_t p_t2, int p_tile_no,
			opj_tcd_tile_t p_tile, byte p_src[], int p_data_read[],
			int p_max_len, opj_codestream_index_t p_cstr_index) {
		int p_src_ptr[] = new int[1];
		opj_pi_iterator_t l_pi[] = null;
		int pino;
		opj_image_t l_image = p_t2.image;
		opj_cp_t l_cp = p_t2.cp;
		opj_tcp_t l_tcp = p_t2.cp.tcps[p_tile_no];
		int l_nb_bytes_read[] = new int[1];
		int l_nb_pocs = l_tcp.numpocs + 1;
		opj_pi_iterator_t l_current_pi = null;
		// #ifdef TODO_MSD
		// int curtp = 0;
		// int tp_start_packno;
		// #endif
		opj_packet_info_t l_pack_info = null;
		opj_image_comp_t l_img_comp = null;
		int i;

		// OPJ_ARG_NOT_USED(p_cstr_index);

		// if (TODO_MSD) {
		// if (p_cstr_index != null) {
		// l_pack_info = p_cstr_index.tile_index[p_tile_no].packet_index[0];
		// }
		// } // if (TODO_MSD)

		/* create a packet iterator */
		l_pi = opj_pi_create_decode(l_image, l_cp, p_tile_no);
		if (l_pi == null) {
			return false;
		}

		l_current_pi = l_pi[0];
		for (pino = 0; pino <= l_tcp.numpocs; ++pino) {
			/*
			 * if the resolution needed is too low, one dim of the tilec could
			 * be equal to zero and no packets are used to decode this
			 * resolution and l_current_pi->resno is always >=
			 * p_tile->comps[l_current_pi->compno].minimum_num_resolutions and
			 * no l_img_comp->resno_decoded are computed
			 */
			boolean first_pass_failed[] = null;

			if (l_current_pi.poc.prg == OPJ_PROG_UNKNOWN) {
				/* TODO ADE : add an error */
				opj_pi_destroy(l_pi, l_nb_pocs);
				return false;
			}

			first_pass_failed = new boolean[l_image.numcomps];
			for (i = 0; i < l_image.numcomps; i++) {
				first_pass_failed[i] = true;
			}

			while (opj_pi_next(l_current_pi)) {
				Preferences
						.debug("packet offset = 00000166 prg = "
								+ l_current_pi.poc.prg1 + " cmptno = "
								+ l_current_pi.compno + "\n" + "rlvlno = "
								+ l_current_pi.resno + " prcno = "
								+ l_current_pi.precno + " lyrno = "
								+ l_current_pi.layno + "\n\n",
								Preferences.DEBUG_FILEIO);

				if (l_tcp.num_layers_to_decode > l_current_pi.layno
						&& l_current_pi.resno < p_tile.comps[l_current_pi.compno].minimum_num_resolutions) {
					l_nb_bytes_read[0] = 0;

					first_pass_failed[l_current_pi.compno] = false;

					if (!opj_t2_decode_packet(p_t2, p_tile, l_tcp,
							l_current_pi, p_src, p_src_ptr, l_nb_bytes_read,
							p_max_len, l_pack_info)) {
						opj_pi_destroy(l_pi, l_nb_pocs);
						first_pass_failed = null;
						return false;
					}

					l_img_comp = l_image.comps[l_current_pi.compno];
					l_img_comp.resno_decoded = Math.max(l_current_pi.resno,
							l_img_comp.resno_decoded);
				} else {
					l_nb_bytes_read[0] = 0;
					if (!opj_t2_skip_packet(p_t2, p_tile, l_tcp, l_current_pi,
							p_src, p_src_ptr, l_nb_bytes_read, p_max_len,
							l_pack_info)) {
						opj_pi_destroy(l_pi, l_nb_pocs);
						first_pass_failed = null;
						return false;
					}
				}

				if (first_pass_failed[l_current_pi.compno]) {
					l_img_comp = l_image.comps[l_current_pi.compno];
					if (l_img_comp.resno_decoded == 0)
						l_img_comp.resno_decoded = p_tile.comps[l_current_pi.compno].minimum_num_resolutions - 1;
				}

				p_max_len -= l_nb_bytes_read[0];

				// p_cstr_info undefined
				// if (TODO_MSD) {
				// if(p_cstr_info != null) {
				// opj_tile_info_v2_t info_TL = p_cstr_info.tile[p_tile_no];
				// opj_packet_info_t info_PK =
				// info_TL.packet[p_cstr_info.packno];
				// tp_start_packno = 0;
				// if (p_cstr_info.packno == 0) {
				// info_PK.start_pos = info_TL.end_header + 1;
				// } else if (info_TL.packet[p_cstr_info.packno-1].end_pos >=
				// p_cstr_info.tile[p_tile_no].tp[curtp].tp_end_pos){ /* New
				// tile part */
				// info_TL.tp[curtp].tp_numpacks = p_cstr_info.packno -
				// tp_start_packno; /* Number of packets in previous tile-part
				// */
				// tp_start_packno = p_cstr_info.packno;
				// curtp++;
				// info_PK.start_pos =
				// p_cstr_info.tile[p_tile_no].tp[curtp].tp_end_header+1;
				// } else {
				// info_PK.start_pos = (l_cp.m_enc.m_tp_on && info_PK.start_pos)
				// ?
				// info_PK.start_pos : info_TL.packet[p_cstr_info.packno -
				// 1].end_pos + 1;
				// }
				// info_PK.end_pos = info_PK.start_pos + l_nb_bytes_read[0] - 1;
				// info_PK.end_ph_pos += info_PK.start_pos - 1; /* End of packet
				// header which now only represents the distance */
				// ++p_cstr_info.packno;
				// }
				// } // if (TODO_MSD)
			}
			if (pino <= l_tcp.numpocs - 1) {
				l_current_pi = l_pi[pino + 1];
			}

			first_pass_failed = null;
		}

		// if (TODO_MSD) {
		// if
		// (p_cstr_info != null) {
		// p_cstr_info.tile[p_tile_no].tp[curtp].tp_numpacks =
		// p_cstr_info.packno - tp_start_packno; /* Number of packets in last
		// tile-part */
		// }
		// } // if (TODO_MSD)

		/* don't forget to release pi */
		opj_pi_destroy(l_pi, l_nb_pocs);
		p_data_read[0] = p_src_ptr[0];
		return true;
	}

	private boolean opj_t2_decode_packet(opj_t2_t p_t2, opj_tcd_tile_t p_tile,
			opj_tcp_t p_tcp, opj_pi_iterator_t p_pi, byte p_src[],
			int p_src_ptr[], int p_data_read[], int p_max_length,
			opj_packet_info_t p_pack_info) {
		boolean l_read_data[] = new boolean[1];
		int l_nb_bytes_read[] = new int[1];
		int l_nb_total_bytes_read = 0;

		p_data_read[0] = 0;

		if (!opj_t2_read_packet_header(p_t2, p_tile, p_tcp, p_pi, l_read_data,
				p_src, p_src_ptr, l_nb_bytes_read, p_max_length, p_pack_info)) {
			return false;
		}

		// p_src += l_nb_bytes_read;
		l_nb_total_bytes_read += l_nb_bytes_read[0];
		p_max_length -= l_nb_bytes_read[0];

		/* we should read data for the packet */
	
		if (l_read_data[0]) {
			l_nb_bytes_read[0] = 0;

			if (!opj_t2_read_packet_data(p_t2, p_tile, p_pi, p_src, p_src_ptr,
					l_nb_bytes_read, p_max_length, p_pack_info)) {
				return false;
			}

			l_nb_total_bytes_read += l_nb_bytes_read[0];
		}

		p_data_read[0] = l_nb_total_bytes_read;

		return true;
	}

	private boolean opj_t2_skip_packet(opj_t2_t p_t2, opj_tcd_tile_t p_tile,
			opj_tcp_t p_tcp, opj_pi_iterator_t p_pi, byte p_src[],
			int p_src_ptr[], int p_data_read[], int p_max_length,
			opj_packet_info_t p_pack_info) {
		boolean l_read_data[] = new boolean[1];
		int l_nb_bytes_read[] = new int[1];
		int l_nb_total_bytes_read = 0;

		p_data_read[0] = 0;

		if (!opj_t2_read_packet_header(p_t2, p_tile, p_tcp, p_pi, l_read_data,
				p_src, p_src_ptr, l_nb_bytes_read, p_max_length, p_pack_info)) {
			return false;
		}

		// p_src += l_nb_bytes_read;
		l_nb_total_bytes_read += l_nb_bytes_read[0];
		p_max_length -= l_nb_bytes_read[0];

		/* we should read data for the packet */
		if (l_read_data[0]) {
			l_nb_bytes_read[0] = 0;

			if (!opj_t2_skip_packet_data(p_t2, p_tile, p_pi, l_nb_bytes_read,
					p_max_length, p_pack_info)) {
				return false;
			}

			l_nb_total_bytes_read += l_nb_bytes_read[0];
		}
		p_data_read[0] = l_nb_total_bytes_read;

		return true;
	}

	private boolean opj_t2_read_packet_header(opj_t2_t p_t2,
			opj_tcd_tile_t p_tile, opj_tcp_t p_tcp, opj_pi_iterator_t p_pi,
			boolean p_is_data_present[], byte p_src_data[], int p_src_ptr[],
			int p_data_read[], int p_max_length, opj_packet_info_t p_pack_info)

	{
		/* loop */
		int bandno, cblkno;
		int l_nb_code_blocks;
		int l_remaining_length;
		int l_header_length;
		int l_modified_length_ptr;
		int p_src_ptr_original = p_src_ptr[0];
		opj_cp_t l_cp = p_t2.cp;
		opj_bio_t l_bio = null; /* BIO component */
		opj_tcd_band_t l_band = null;
		opj_tcd_cblk_dec_t l_cblk = null;
		opj_tcd_resolution_t l_res = p_tile.comps[p_pi.compno].resolutions[p_pi.resno];

		byte l_header_data[] = null;
		int l_header_data_start;
		int l_header_data_ptr;

		int l_present;

		if (p_pi.layno == 0) {
			l_band = l_res.bands[0];

			/* reset tagtrees */
			for (bandno = 0; bandno < l_res.numbands; ++bandno) {
				opj_tcd_precinct_t l_prc = l_band.precincts[p_pi.precno];

				if (!((l_band.x1 - l_band.x0 == 0) || (l_band.y1 - l_band.y0 == 0))) {
					opj_tgt_reset(l_prc.incltree);
					opj_tgt_reset(l_prc.imsbtree);
					l_cblk = l_prc.dec[0];

					l_nb_code_blocks = l_prc.cw * l_prc.ch;
					for (cblkno = 0; cblkno < l_nb_code_blocks; ++cblkno) {
						l_cblk.numsegs = 0;
						l_cblk.real_num_segs = 0;
						if (cblkno < l_nb_code_blocks - 1) {
							l_cblk = l_prc.dec[cblkno + 1];
						}
					} // for (cblkno = 0; cblkno < l_nb_code_blocks; ++cblkno)
				} // if (!((l_band.x1 - l_band.x0 == 0) || (l_band.y1 - l_band.y0 == 0)))
				if (bandno < l_res.numbands - 1) {
					l_band = l_res.bands[bandno + 1];
				}
			} // for (bandno = 0; bandno < l_res.numbands; ++bandno)
		} // if (p_pi.layno == 0)

		/* SOP markers */

		if ((p_tcp.csty & J2K_CP_CSTY_SOP) != 0) {
			if (p_max_length < 6) {
				Preferences.debug("Not enough space for expected SOP marker\n",
						Preferences.DEBUG_FILEIO);
			} else if (p_src_data[p_src_ptr[0]] != 0xff
					|| p_src_data[p_src_ptr[0] + 1] != 0x91) {
				Preferences.debug("Warning! Expected SOP marker\n",
						Preferences.DEBUG_FILEIO);
			} else {
				p_src_ptr[0] += 6;
			}

			/** TODO : check the Nsop value */
		}

		/*
		 * When the marker PPT/PPM is used the packet header are store in
		 * PPT/PPM marker This part deal with this caracteristic step 1: Read
		 * packet header in the saved structure step 2: Return to codestream for
		 * decoding
		 */

		l_bio = new opj_bio_t();

		if (l_cp.ppm == 1) { /* PPM */
			// l_header_data_start = l_cp.ppm_data;
			l_header_data_start = 0;
			l_header_data_ptr = 0;
			l_header_data = l_cp.ppm_data;
			l_modified_length_ptr = l_cp.ppm_len;

		} else if (p_tcp.ppt == 1) { /* PPT */
			// l_header_data_start = &(p_tcp->ppt_data);
			l_header_data_start = 0;
			l_header_data_ptr = 0;
			l_header_data = p_tcp.ppt_data;
			l_modified_length_ptr = p_tcp.ppt_len;
		} else { /* Normal Case */
			l_header_data_start = p_src_ptr[0];
			l_header_data_ptr = p_src_ptr[0];
			l_header_data = p_src_data;
			l_remaining_length = p_src_ptr_original + p_max_length
					- p_src_ptr[0];
			l_modified_length_ptr = l_remaining_length;
		}

		opj_bio_init_dec(l_bio, l_header_data, l_header_data_start,
				l_modified_length_ptr);

		l_present = opj_bio_read(l_bio, 1);
		Preferences.debug("l_present = " + l_present + "\n",
				Preferences.DEBUG_FILEIO);
		if (l_present == 0) {
			/* TODO MSD: no test to control the output of this function */
			opj_bio_inalign(l_bio);
			l_header_data_ptr += opj_bio_numbytes(l_bio);
			l_bio = null;

			/* EPH markers */
			if ((p_tcp.csty & J2K_CP_CSTY_EPH) != 0) {
				if ((l_modified_length_ptr - (l_header_data_ptr - l_header_data_start)) < 2) {
					Preferences.debug(
							"Not enough space for expected EPH marker\n",
							Preferences.DEBUG_FILEIO);
				} else if (l_header_data[l_header_data_ptr] != 0xff
						|| l_header_data[l_header_data_ptr + 1] != 0x92) {
					Preferences.debug("Warning! Expected EPH marker\n",
							Preferences.DEBUG_FILEIO);
				} else {
					l_header_data_ptr += 2;
				}
			}

			l_header_length = (l_header_data_ptr - l_header_data_start);
			l_modified_length_ptr -= l_header_length;
			l_header_data_start += l_header_length;
			//l_header_data_ptr = l_header_data_start;

			/* << INDEX */
			/*
			 * End of packet header position. Currently only represents the
			 * distance to start of packet Will be updated later by incrementing
			 * with packet start value
			 */
			if (p_pack_info != null) {
				p_pack_info.end_ph_pos = (p_src_ptr[0] - p_src_ptr_original);
			}
			/* INDEX >> */

			p_is_data_present[0] = false;
			p_data_read[0] = p_src_ptr[0] - p_src_ptr_original;
			return true;
		}

		l_band = l_res.bands[0];
		for (bandno = 0; bandno < l_res.numbands; ++bandno) {
			opj_tcd_precinct_t l_prc = l_band.precincts[p_pi.precno];

			if ((l_band.x1 - l_band.x0 == 0) || (l_band.y1 - l_band.y0 == 0)) {
				if (bandno < l_res.numbands - 1) {
					l_band = l_res.bands[bandno + 1];
				}
				continue;
			}

			l_nb_code_blocks = l_prc.cw * l_prc.ch;
			l_cblk = l_prc.dec[0];
			for (cblkno = 0; cblkno < l_nb_code_blocks; cblkno++) {
				int l_included, l_increment, l_segno;
				int n;

				/* if cblk not yet included before --> inclusion tagtree */
				if (l_cblk.numsegs == 0) {
					l_included = opj_tgt_decode(l_bio, l_prc.incltree, cblkno,
							(p_pi.layno + 1));
					/* else one bit */
				} else {
					l_included = opj_bio_read(l_bio, 1);
				}

				/* if cblk not included */
				if (l_included == 0) {
					l_cblk.numnewpasses = 0;
					if (cblkno < l_nb_code_blocks - 1) {
						l_cblk = l_prc.dec[cblkno + 1];
					}
					Preferences.debug("l_included = " + l_included + "\n",
							Preferences.DEBUG_FILEIO);
					continue;
				}

				/* if cblk not yet included --> zero-bitplane tagtree */
				if (l_cblk.numsegs == 0) {
					int i = 0;

					while (opj_tgt_decode(l_bio, l_prc.imsbtree, cblkno, i) == 0) {
						++i;
					}

					l_cblk.numbps = l_band.numbps + 1 - i;
					l_cblk.numlenbits = 3;
				}

				/* number of coding passes */
				l_cblk.numnewpasses = opj_t2_getnumpasses(l_bio);
				l_increment = opj_t2_getcommacode(l_bio);

				/* length indicator increment */
				l_cblk.numlenbits += l_increment;
				l_segno = 0;

				if (l_cblk.numsegs == 0) {
					if (!opj_t2_init_seg(l_cblk, l_segno,
							p_tcp.tccps[p_pi.compno].cblksty, 1)) {
						l_bio = null;
						return false;
					}
				} else {
					l_segno = l_cblk.numsegs - 1;
					if (l_cblk.segs[l_segno].numpasses == l_cblk.segs[l_segno].maxpasses) {
						++l_segno;
						if (!opj_t2_init_seg(l_cblk, l_segno,
								p_tcp.tccps[p_pi.compno].cblksty, 0)) {
							l_bio = null;
							return false;
						}
					}
				}
				n = l_cblk.numnewpasses;

				do {
					l_cblk.segs[l_segno].numnewpasses = Math
							.min((l_cblk.segs[l_segno].maxpasses - l_cblk.segs[l_segno].numpasses),
									n);
					l_cblk.segs[l_segno].newlen = opj_bio_read(
							l_bio,
							l_cblk.numlenbits
									+ opj_uint_floorlog2(l_cblk.segs[l_segno].numnewpasses));
					Preferences.debug("l_included =  " + l_included
							+ " numnewpasses = "
							+ l_cblk.segs[l_segno].numnewpasses + "\n"
							+ "l_increment = " + l_increment + " len = "
							+ l_cblk.segs[l_segno].newlen + "\n",
							Preferences.DEBUG_FILEIO);

					n -= l_cblk.segs[l_segno].numnewpasses;
					if (n > 0) {
						++l_segno;

						if (!opj_t2_init_seg(l_cblk, l_segno,
								p_tcp.tccps[p_pi.compno].cblksty, 0)) {
							l_bio = null;
							return false;
						}
					}
				} while (n > 0);

				if (cblkno < l_nb_code_blocks - 1) {
					l_cblk = l_prc.dec[cblkno + 1];
				}
			}

			if (bandno < l_res.numbands - 1) {
				l_band = l_res.bands[bandno + 1];
			}
		}

		if (!opj_bio_inalign(l_bio)) {
			l_bio = null;
			return false;
		}

		l_header_data_ptr += opj_bio_numbytes(l_bio);
		l_bio = null;

		/* EPH markers */
		if ((p_tcp.csty & J2K_CP_CSTY_EPH) != 0) {
			if ((l_modified_length_ptr - (l_header_data_ptr - l_header_data_start)) < 2) {
				Preferences.debug(
						"Warning! Not enough space for expected EPH marker\n",
						Preferences.DEBUG_FILEIO);
			} else if (l_header_data[l_header_data_ptr] != 0xff
					|| l_header_data[l_header_data_ptr + 1] != 0x92) {
				Preferences.debug("Warning! Expected EPH marker\n",
						Preferences.DEBUG_FILEIO);
			} else {
				l_header_data_ptr += 2;
			}
		}

		l_header_length = (l_header_data_ptr - l_header_data_start);
		Preferences.debug("hdrlen = " + l_header_length + "\n",
				Preferences.DEBUG_FILEIO);
		Preferences.debug("packet body\n", Preferences.DEBUG_FILEIO);
		l_modified_length_ptr -= l_header_length;
		l_header_data_start += l_header_length;
		//l_header_data_ptr = l_header_data_start;

		/* << INDEX */
		/*
		 * End of packet header position. Currently only represents the distance
		 * to start of packet Will be updated later by incrementing with packet
		 * start value
		 */
		if (p_pack_info != null) {
			p_pack_info.end_ph_pos = (p_src_ptr[0] - p_src_ptr_original);
		}
		/* INDEX >> */

		p_is_data_present[0] = true;
		p_data_read[0] = (p_src_ptr[0] - p_src_ptr_original);

		return true;
	}

	/**
	 * Get logarithm of an integer and round downwards
	 * 
	 * @return Returns log2(a)
	 */
	private int opj_uint_floorlog2(int a) {
		int l;
		for (l = 0; a > 1; ++l) {
			a >>= 1;
		}
		return l;
	}

	private boolean opj_t2_init_seg(opj_tcd_cblk_dec_t cblk, int index,
			int cblksty, int first) {
		int i;
		opj_tcd_seg_t seg = null;
		int l_nb_segs = index + 1;

		if (l_nb_segs > cblk.m_current_max_segs) {
			opj_tcd_seg_t new_segs[];
			cblk.m_current_max_segs += OPJ_J2K_DEFAULT_NB_SEGS;

			new_segs = new opj_tcd_seg_t[cblk.m_current_max_segs];
			for (i = 0; i < cblk.segs.length; i++) {
				new_segs[i] = cblk.segs[i];
			}
			for (i = cblk.segs.length; i < new_segs.length; i++) {
				new_segs[i] = new opj_tcd_seg_t();
			}
			cblk.segs = new_segs;
		}

		seg = cblk.segs[index];
		seg.data = null;
		seg.dataindex = 0;
		seg.len = 0;
		seg.maxpasses = 0;
		seg.newlen = 0;
		seg.numnewpasses = 0;
		seg.numpasses = 0;
		seg.real_num_passes = 0;

		if ((cblksty & J2K_CCP_CBLKSTY_TERMALL) != 0) {
			seg.maxpasses = 1;
		} else if ((cblksty & J2K_CCP_CBLKSTY_LAZY) != 0) {
			if (first != 0) {
				seg.maxpasses = 10;
			} else {
				opj_tcd_seg_t segm1 = cblk.segs[index - 1];
				seg.maxpasses = ((segm1.maxpasses == 1) || (segm1.maxpasses == 10)) ? 2
						: 1;
			}
		} else {
			seg.maxpasses = 109;
		}

		return true;
	}

	private int opj_t2_getcommacode(opj_bio_t bio) {
		int n = 0;
		while (opj_bio_read(bio, 1) != 0) {
			++n;
		}
		return n;
	}

	private int opj_t2_getnumpasses(opj_bio_t bio) {
		int n;
		if (opj_bio_read(bio, 1) == 0)
			return 1;
		if (opj_bio_read(bio, 1) == 0)
			return 2;
		if ((n = opj_bio_read(bio, 2)) != 3)
			return (3 + n);
		if ((n = opj_bio_read(bio, 5)) != 31)
			return (6 + n);
		return (37 + opj_bio_read(bio, 7));
	}

	private int opj_tgt_decode(opj_bio_t bio, opj_tgt_tree_t tree, int leafno,
			int threshold) {
		opj_tgt_node_t stk[] = new opj_tgt_node_t[31];
		opj_tgt_node_t node;
		int low;
		int index;

		index = 0;
		node = tree.nodes[leafno];
		while (node.parent != null) {
			stk[index++] = node;
			node = node.parent;
		}

		low = 0;
		for (;;) {
			if (low > node.low) {
				node.low = low;
			} else {
				low = node.low;
			}
			while (low < threshold && low < node.value) {
				if (opj_bio_read(bio, 1) != 0) {
					node.value = low;
				} else {
					++low;
				}
			}
			node.low = low;
			if (index == 0) {
				break;
			}
			node = stk[--index];
		}

		return (node.value < threshold) ? 1 : 0;
	}

	private int opj_bio_numbytes(opj_bio_t bio) {
		return (bio.bp - bio.start);
	}

	private boolean opj_bio_inalign(opj_bio_t bio) {
		if ((bio.buf & 0xff) == 0xff) {
			if (!opj_bio_bytein(bio)) {
				return false;
			}
		}
		bio.ct = 0;
		return true;
	}

	private int opj_bio_read(opj_bio_t bio, int n) {
		int i;
		int v;
		v = 0;
		for (i = n - 1; i >= 0; i--) {
			v += opj_bio_getbit(bio) << i;
		}
		return v;
	}

	private int opj_bio_getbit(opj_bio_t bio) {
		if (bio.ct == 0) {
			opj_bio_bytein(bio); /*
								 * MSD: why not check the return value of this
								 * function ?
								 */
		}
		bio.ct--;
		return (bio.buf >> bio.ct) & 1;
	}

	private boolean opj_bio_bytein(opj_bio_t bio) {
		bio.buf = (bio.buf << 8) & 0xffff;
		bio.ct = bio.buf == 0xff00 ? 7 : 8;
		if (bio.bp >= bio.end) {
			return false;
		}
		bio.buf |= bio.data[bio.bp++];
		return true;
	}

	private void opj_bio_init_dec(opj_bio_t bio, byte bp[], int start, int len) {
		bio.data = bp;
		bio.start = start;
		bio.end = start + len;
		bio.bp = start;
		bio.buf = 0;
		bio.ct = 0;
	}

	private boolean opj_t2_read_packet_data(opj_t2_t p_t2,
			opj_tcd_tile_t p_tile, opj_pi_iterator_t p_pi, byte p_src_data[],
			int p_src_ptr[], int p_data_read[], int p_max_length,
			opj_packet_info_t pack_info) {
		int bandno, cblkno;
		int l_nb_code_blocks;
		// OPJ_BYTE *l_current_data = p_src_data;
		int p_src_ptr_original = p_src_ptr[0];
		opj_tcd_band_t l_band = null;
		opj_tcd_cblk_dec_t l_cblk = null;
		opj_tcd_resolution_t l_res = p_tile.comps[p_pi.compno].resolutions[p_pi.resno];
		int i;

		// OPJ_ARG_NOT_USED(p_t2);
		// OPJ_ARG_NOT_USED(pack_info);

		l_band = l_res.bands[0];
		for (bandno = 0; bandno < l_res.numbands; ++bandno) {
			opj_tcd_precinct_t l_prc = l_band.precincts[p_pi.precno];

			if ((l_band.x1 - l_band.x0 == 0) || (l_band.y1 - l_band.y0 == 0)) {
				if (bandno < l_res.numbands - 1) {
					l_band = l_res.bands[bandno + 1];
				}
				continue;
			}

			l_nb_code_blocks = l_prc.cw * l_prc.ch;
			l_cblk = l_prc.dec[0];

			for (cblkno = 0; cblkno < l_nb_code_blocks; ++cblkno) {
				opj_tcd_seg_t l_seg = null;

				if (l_cblk.numnewpasses == 0) {
					/* nothing to do */
					if (cblkno < l_nb_code_blocks - 1) {
						l_cblk = l_prc.dec[cblkno + 1];
					}
					continue;
				}

				if (l_cblk.numsegs == 0) {
					l_seg = l_cblk.segs[0];
					++l_cblk.numsegs;
					l_cblk.data_current_size = 0;
				} else {
					l_seg = l_cblk.segs[l_cblk.numsegs - 1];

					if (l_seg.numpasses == l_seg.maxpasses) {
						l_seg = l_cblk.segs[l_cblk.numsegs];
						++l_cblk.numsegs;
					}
				}

				do {
					/*
					 * Check possible overflow (on l_current_data only, assumes
					 * input args already checked) then size
					 */
					if (((p_src_ptr[0] + l_seg.newlen) < p_src_ptr[0])
							|| (p_src_ptr[0] + l_seg.newlen > p_src_ptr_original
									+ p_max_length)) {
						MipavUtil.displayError("read: segment too long ("
								+ l_seg.newlen + ") with max (" + p_max_length
								+ ") for codeblock " + cblkno);
						MipavUtil.displayError("(p = " + p_pi.precno
								+ ",  b = " + bandno + ", r = " + p_pi.resno
								+ ", c = " + p_pi.compno + ")");
						return false;
					}

					if (useJPWL) {
						/*
						 * we need here a j2k handle to verify if making a check
						 * to the validity of cblocks parameters is selected
						 * from user (-W)
						 */
						int l_cblklen = 0;
						if (l_cblk.data != null) {
							l_cblklen += l_cblk.data.length;
						}
						l_cblklen += 48;
						/* let's check that we are not exceeding */
						if ((l_cblklen + l_seg.newlen) > 8192) {
							Preferences.debug("JPWL: segment too long ("
									+ l_seg.newlen + ") for codeblock = "
									+ cblkno + "\n" + "(p = " + p_pi.precno
									+ ",  b = " + bandno + ", r = "
									+ p_pi.resno + ", c = " + p_pi.compno
									+ ")\n", Preferences.DEBUG_FILEIO);
							if (!JPWLAssume) {
								MipavUtil.displayError("JPWL: giving up");
								return false;
							}
							l_seg.newlen = 8192 - l_cblklen;
							Preferences.debug("      - truncating segment to "
									+ l_seg.newlen + "\n",
									Preferences.DEBUG_FILEIO);
							break;
						}
						;

					} // if (useJPWL)
					/* Check possible overflow on size */
					if ((l_cblk.data_current_size + l_seg.newlen) < l_cblk.data_current_size) {
						MipavUtil.displayError("read: segment too long ("
								+ l_seg.newlen + ") with current size ("
								+ l_cblk.data_current_size + " > "
								+ (0xFFFFFFFF - l_seg.newlen) + ")");
						MipavUtil.displayError("for codeblock " + cblkno
								+ " (p= " + p_pi.precno + ", b= " + bandno
								+ ", r= " + p_pi.resno + ", c= " + p_pi.compno
								+ ")");
						return false;
					}
					/* Check if the cblk->data have allocated enough memory */
					if ((l_cblk.data_current_size + l_seg.newlen) > l_cblk.data_max_size) {
						byte new_cblk_data[] = new byte[l_cblk.data_current_size
								+ l_seg.newlen];
						for (i = 0; i < l_cblk.data.length; i++) {
							new_cblk_data[i] = l_cblk.data[i];
						}
						l_cblk.data_max_size = l_cblk.data_current_size
								+ l_seg.newlen;
						l_cblk.data = new_cblk_data;
					}

					for (i = 0; i < l_seg.newlen; i++) {
						l_cblk.data[l_cblk.data_current_size + i] = p_src_data[p_src_ptr[0]
								+ i];
					}

					if (l_seg.numpasses == 0) {
						l_seg.data = l_cblk.data;
						l_seg.dataindex = l_cblk.data_current_size;
					}

					p_src_ptr[0] += l_seg.newlen;
					l_seg.numpasses += l_seg.numnewpasses;
					l_cblk.numnewpasses -= l_seg.numnewpasses;

					l_seg.real_num_passes = l_seg.numpasses;
					l_cblk.data_current_size += l_seg.newlen;
					l_seg.len += l_seg.newlen;

					if (l_cblk.numnewpasses > 0) {
						l_seg = l_cblk.segs[l_cblk.numsegs];
						++l_cblk.numsegs;
					}
				} while (l_cblk.numnewpasses > 0);

				l_cblk.real_num_segs = l_cblk.numsegs;
				if (cblkno < l_nb_code_blocks - 1) {
					l_cblk = l_prc.dec[cblkno + 1];
				}
			} /* next code_block */

			if (bandno < l_res.numbands - 1) {
				l_band = l_res.bands[bandno + 1];
			}
		}

		p_data_read[0] = p_src_ptr[0] - p_src_ptr_original;

		return true;
	}

	private boolean opj_t2_skip_packet_data(opj_t2_t p_t2,
			opj_tcd_tile_t p_tile, opj_pi_iterator_t p_pi, int p_data_read[],
			int p_max_length, opj_packet_info_t pack_info) {
		int bandno, cblkno;
		int l_nb_code_blocks;
		opj_tcd_band_t l_band = null;
		opj_tcd_cblk_dec_t l_cblk = null;
		opj_tcd_resolution_t l_res = p_tile.comps[p_pi.compno].resolutions[p_pi.resno];

		// OPJ_ARG_NOT_USED(p_t2);
		// OPJ_ARG_NOT_USED(pack_info);

		p_data_read[0] = 0;
		l_band = l_res.bands[0];

		for (bandno = 0; bandno < l_res.numbands; ++bandno) {
			opj_tcd_precinct_t l_prc = l_band.precincts[p_pi.precno];

			if ((l_band.x1 - l_band.x0 == 0) || (l_band.y1 - l_band.y0 == 0)) {
				if (bandno < l_res.numbands - 1) {
					l_band = l_res.bands[bandno + 1];
				}
				continue;
			}

			l_nb_code_blocks = l_prc.cw * l_prc.ch;
			l_cblk = l_prc.dec[0];

			for (cblkno = 0; cblkno < l_nb_code_blocks; ++cblkno) {
				opj_tcd_seg_t l_seg = null;

				if (l_cblk.numnewpasses == 0) {
					/* nothing to do */
					if (cblkno < l_nb_code_blocks - 1) {
						l_cblk = l_prc.dec[cblkno + 1];
					}
					continue;
				}

				if (l_cblk.numsegs == 0) {
					l_seg = l_cblk.segs[0];
					++l_cblk.numsegs;
					l_cblk.data_current_size = 0;
				} else {
					l_seg = l_cblk.segs[l_cblk.numsegs - 1];

					if (l_seg.numpasses == l_seg.maxpasses) {
						l_seg = l_cblk.segs[l_cblk.numsegs];
						++l_cblk.numsegs;
					}
				}

				do {
					/* Check possible overflow then size */
					if (((p_data_read[0] + l_seg.newlen) < (p_data_read[0]))
							|| ((p_data_read[0] + l_seg.newlen) > p_max_length)) {
						MipavUtil.displayError("skip: segment too long ("
								+ l_seg.newlen + ") with max (" + p_max_length
								+ ") for codeblock " + cblkno);
						MipavUtil.displayError("(p = " + p_pi.precno + ", b = "
								+ bandno + ", r = " + p_pi.resno + ", c= "
								+ p_pi.compno + ")");
						return false;
					}

					if (useJPWL) {
						/*
						 * we need here a j2k handle to verify if making a check
						 * to the validity of cblocks parameters is selected
						 * from user (-W)
						 */
						int l_cblklen = 0;
						if (l_cblk.data != null) {
							l_cblklen += l_cblk.data.length;
						}
						l_cblklen += 48;
						/* let's check that we are not exceeding */
						if ((l_cblklen + l_seg.newlen) > 8192) {
							Preferences.debug("JPWL: segment too long ("
									+ l_seg.newlen + ") for codeblock "
									+ cblkno + "\n" + "(p = " + p_pi.precno
									+ ", b = " + bandno + ", r = " + p_pi.resno
									+ ", c = " + p_pi.compno + ")\n",
									Preferences.DEBUG_FILEIO);
							if (!JPWLAssume) {
								MipavUtil.displayError("JPWL: giving up");
								return false;
							}
							l_seg.newlen = 8192 - l_cblklen;
							Preferences.debug("      - truncating segment to "
									+ l_seg.newlen + "\n",
									Preferences.DEBUG_FILEIO);
							break;
						}
						;

					} // if (useJPWL)
					Preferences.debug("p_data_read[0] (" + p_data_read[0]
							+ ") l_seg.newlen (" + l_seg.newlen + ") \n",
							Preferences.DEBUG_FILEIO);
					p_data_read[0] += l_seg.newlen;

					l_seg.numpasses += l_seg.numnewpasses;
					l_cblk.numnewpasses -= l_seg.numnewpasses;
					if (l_cblk.numnewpasses > 0) {
						l_seg = l_cblk.segs[l_cblk.numsegs];
						++l_cblk.numsegs;
					}
				} while (l_cblk.numnewpasses > 0);

				if (cblkno < l_nb_code_blocks - 1) {
					l_cblk = l_prc.dec[cblkno + 1];
				}
			}

			if (bandno < l_res.numbands - 1) {
				l_band = l_res.bands[bandno + 1];
			}
		}

		return true;
	}

	private boolean opj_pi_next(opj_pi_iterator_t pi) {
		switch (pi.poc.prg) {
		case OPJ_LRCP:
			return opj_pi_next_lrcp(pi);
		case OPJ_RLCP:
			return opj_pi_next_rlcp(pi);
		case OPJ_RPCL:
			return opj_pi_next_rpcl(pi);
		case OPJ_PCRL:
			return opj_pi_next_pcrl(pi);
		case OPJ_CPRL:
			return opj_pi_next_cprl(pi);
		case OPJ_PROG_UNKNOWN:
			return false;
		}

		return false;
	}

	private boolean opj_pi_next_lrcp(opj_pi_iterator_t pi) {
		opj_pi_comp_t comp = null;
		opj_pi_resolution_t res = null;
		int index = 0;
		boolean noskip1 = true;
		boolean noskip2 = true;

		if (!pi.first) {
			comp = pi.comps[pi.compno];
			res = comp.resolutions[pi.resno];
			noskip1 = false;
			noskip2 = false;
		} else {
			pi.first = false;
		}

		for (pi.layno = pi.poc.layno0; pi.layno < pi.poc.layno1; pi.layno++) {
			for (pi.resno = pi.poc.resno0; pi.resno < pi.poc.resno1; pi.resno++) {
				for (pi.compno = pi.poc.compno0; pi.compno < pi.poc.compno1; pi.compno++) {
					if (noskip1) {
						comp = pi.comps[pi.compno];
						if (pi.resno >= comp.numresolutions) {
							continue;
						}
						res = comp.resolutions[pi.resno];
						if (pi.tp_on == 0) {
							pi.poc.precno1 = res.pw * res.ph;
						}
					} else {
						noskip1 = true;
					}
					for (pi.precno = pi.poc.precno0; pi.precno < pi.poc.precno1; pi.precno++) {
						if (noskip2) {
							index = pi.layno * pi.step_l + pi.resno * pi.step_r
									+ pi.compno * pi.step_c + pi.precno
									* pi.step_p;
							if (pi.include[index] == 0) {
								pi.include[index] = 1;
								return true;
							}
						} else {
							noskip2 = true;
						}
					}
				}
			}
		}

		return false;
	}

	private boolean opj_pi_next_rlcp(opj_pi_iterator_t pi) {
		opj_pi_comp_t comp = null;
		opj_pi_resolution_t res = null;
		int index = 0;
		boolean noskip1 = true;
		boolean noskip2 = true;

		if (!pi.first) {
			comp = pi.comps[pi.compno];
			res = comp.resolutions[pi.resno];
			noskip1 = false;
			noskip2 = false;
		} else {
			pi.first = false;
		}

		for (pi.resno = pi.poc.resno0; pi.resno < pi.poc.resno1; pi.resno++) {
			for (pi.layno = pi.poc.layno0; pi.layno < pi.poc.layno1; pi.layno++) {
				for (pi.compno = pi.poc.compno0; pi.compno < pi.poc.compno1; pi.compno++) {
					if (noskip1) {
						comp = pi.comps[pi.compno];
						if (pi.resno >= comp.numresolutions) {
							continue;
						}
						res = comp.resolutions[pi.resno];
						if (pi.tp_on == 0) {
							pi.poc.precno1 = res.pw * res.ph;
						}
					} else {
						noskip1 = true;
					}
					for (pi.precno = pi.poc.precno0; pi.precno < pi.poc.precno1; pi.precno++) {
						if (noskip2) {
							index = pi.layno * pi.step_l + pi.resno * pi.step_r
									+ pi.compno * pi.step_c + pi.precno
									* pi.step_p;
							if (pi.include[index] == 0) {
								pi.include[index] = 1;
								return true;
							}
						} else {
							noskip2 = true;
						}
					}
				}
			}
		}

		return false;
	}

	private boolean opj_pi_next_rpcl(opj_pi_iterator_t pi) {
		opj_pi_comp_t comp = null;
		opj_pi_resolution_t res = null;
		int index = 0;
		boolean noskip1 = true;
		boolean noskip2 = true;

		if (!pi.first) {
			noskip1 = false;
			noskip2 = false;
		} else {
			int compno, resno;
			pi.first = false;
			pi.dx = 0;
			pi.dy = 0;
			for (compno = 0; compno < pi.numcomps; compno++) {
				comp = pi.comps[compno];
				for (resno = 0; resno < comp.numresolutions; resno++) {
					int dx, dy;
					res = comp.resolutions[resno];
					dx = comp.dx
							* (1 << (res.pdx + comp.numresolutions - 1 - resno));
					dy = comp.dy
							* (1 << (res.pdy + comp.numresolutions - 1 - resno));
					pi.dx = (pi.dx == 0) ? dx : Math.min(pi.dx, dx);
					pi.dy = (pi.dy == 0) ? dy : Math.min(pi.dy, dy);
				}
			}
		}
		if ((pi.tp_on == 0) && noskip1) {
			pi.poc.ty0 = pi.ty0;
			pi.poc.tx0 = pi.tx0;
			pi.poc.ty1 = pi.ty1;
			pi.poc.tx1 = pi.tx1;
		}
		for (pi.resno = pi.poc.resno0; pi.resno < pi.poc.resno1; pi.resno++) {
			for (pi.y = pi.poc.ty0; pi.y < pi.poc.ty1; pi.y += (pi.dy - (pi.y % pi.dy))) {
				for (pi.x = pi.poc.tx0; pi.x < pi.poc.tx1; pi.x += (pi.dx - (pi.x % pi.dx))) {
					for (pi.compno = pi.poc.compno0; pi.compno < pi.poc.compno1; pi.compno++) {
						if (noskip1) {
							int levelno;
							int trx0, try0;
							int trx1, try1;
							int rpx, rpy;
							int prci, prcj;
							comp = pi.comps[pi.compno];
							if (pi.resno >= comp.numresolutions) {
								continue;
							}
							res = comp.resolutions[pi.resno];
							levelno = comp.numresolutions - 1 - pi.resno;
							trx0 = (pi.tx0 + (comp.dx << levelno) - 1)
									/ (comp.dx << levelno);
							try0 = (pi.ty0 + (comp.dy << levelno) - 1)
									/ (comp.dy << levelno);
							trx1 = (pi.tx1 + (comp.dx << levelno) - 1)
									/ (comp.dx << levelno);
							try1 = (pi.ty1 + (comp.dy << levelno) - 1)
									/ (comp.dy << levelno);
							rpx = res.pdx + levelno;
							rpy = res.pdy + levelno;
							if (!((pi.y % (comp.dy << rpy) == 0) || ((pi.y == pi.ty0) && (((try0 << levelno) % (1 << rpy)) != 0)))) {
								continue;
							}
							if (!((pi.x % (comp.dx << rpx) == 0) || ((pi.x == pi.tx0) && (((trx0 << levelno) % (1 << rpx)) != 0)))) {
								continue;
							}

							if ((res.pw == 0) || (res.ph == 0))
								continue;

							if ((trx0 == trx1) || (try0 == try1))
								continue;

							prci = (((pi.x + (comp.dx << levelno) - 1) / (comp.dx << levelno)) >>> res.pdx)
									- (trx0 >>> res.pdx);
							prcj = (((pi.y + (comp.dy << levelno) - 1) / (comp.dy << levelno)) >>> res.pdy)
									- (try0 >>> res.pdy);
							pi.precno = (prci + prcj * res.pw);
						} else {
							noskip1 = true;
						}
						for (pi.layno = pi.poc.layno0; pi.layno < pi.poc.layno1; pi.layno++) {
							if (noskip2) {
								index = pi.layno * pi.step_l + pi.resno
										* pi.step_r + pi.compno * pi.step_c
										+ pi.precno * pi.step_p;
								if (pi.include[index] == 0) {
									pi.include[index] = 1;
									return true;
								}
							} else {
								noskip2 = true;
							}
						}
					}
				}
			}
		}

		return false;
	}

	private boolean opj_pi_next_pcrl(opj_pi_iterator_t pi) {
		opj_pi_comp_t comp = null;
		opj_pi_resolution_t res = null;
		int index = 0;
		boolean noskip1 = true;
		boolean noskip2 = true;

		if (!pi.first) {
			comp = pi.comps[pi.compno];
			noskip1 = false;
			noskip2 = false;
		} else {
			int compno, resno;
			pi.first = false;
			pi.dx = 0;
			pi.dy = 0;
			for (compno = 0; compno < pi.numcomps; compno++) {
				comp = pi.comps[compno];
				for (resno = 0; resno < comp.numresolutions; resno++) {
					int dx, dy;
					res = comp.resolutions[resno];
					dx = comp.dx
							* (1 << (res.pdx + comp.numresolutions - 1 - resno));
					dy = comp.dy
							* (1 << (res.pdy + comp.numresolutions - 1 - resno));
					pi.dx = (pi.dx == 0) ? dx : Math.min(pi.dx, dx);
					pi.dy = (pi.dy == 0) ? dy : Math.min(pi.dy, dy);
				}
			}
		}
		if ((pi.tp_on == 0) && noskip1) {
			pi.poc.ty0 = pi.ty0;
			pi.poc.tx0 = pi.tx0;
			pi.poc.ty1 = pi.ty1;
			pi.poc.tx1 = pi.tx1;
		}
		for (pi.y = pi.poc.ty0; pi.y < pi.poc.ty1; pi.y += (pi.dy - (pi.y % pi.dy))) {
			for (pi.x = pi.poc.tx0; pi.x < pi.poc.tx1; pi.x += (pi.dx - (pi.x % pi.dx))) {
				for (pi.compno = pi.poc.compno0; pi.compno < pi.poc.compno1; pi.compno++) {
					comp = pi.comps[pi.compno];
					for (pi.resno = pi.poc.resno0; pi.resno < Math.min(
							pi.poc.resno1, comp.numresolutions); pi.resno++) {
						if (noskip1) {
							int levelno;
							int trx0, try0;
							int trx1, try1;
							int rpx, rpy;
							int prci, prcj;
							res = comp.resolutions[pi.resno];
							levelno = comp.numresolutions - 1 - pi.resno;
							trx0 = (pi.tx0 + (comp.dx << levelno) - 1)
									/ (comp.dx << levelno);
							try0 = (pi.ty0 + (comp.dy << levelno) - 1)
									/ (comp.dy << levelno);
							trx1 = (pi.tx1 + (comp.dx << levelno) - 1)
									/ (comp.dx << levelno);
							try1 = (pi.ty1 + (comp.dy << levelno) - 1)
									/ (comp.dy << levelno);
							rpx = res.pdx + levelno;
							rpy = res.pdy + levelno;
							if (!((pi.y % (comp.dy << rpy) == 0) || ((pi.y == pi.ty0) && (((try0 << levelno) % (1 << rpy)) != 0)))) {
								continue;
							}
							if (!((pi.x % (comp.dx << rpx) == 0) || ((pi.x == pi.tx0) && (((trx0 << levelno) % (1 << rpx)) != 0)))) {
								continue;
							}

							if ((res.pw == 0) || (res.ph == 0))
								continue;

							if ((trx0 == trx1) || (try0 == try1))
								continue;

							prci = (((pi.x + (comp.dx << levelno) - 1) / (comp.dx << levelno)) >>> res.pdx)
									- (trx0 >>> res.pdx);
							prcj = (((pi.y + (comp.dy << levelno) - 1) / (comp.dy << levelno)) >>> res.pdy)
									- (try0 >>> res.pdy);
							pi.precno = (prci + prcj * res.pw);
						} else {
							noskip1 = true;
						}
						for (pi.layno = pi.poc.layno0; pi.layno < pi.poc.layno1; pi.layno++) {
							if (noskip2) {
								index = pi.layno * pi.step_l + pi.resno
										* pi.step_r + pi.compno * pi.step_c
										+ pi.precno * pi.step_p;
								if (pi.include[index] == 0) {
									pi.include[index] = 1;
									return true;
								}
							} else {
								noskip2 = true;
							}
						}
					}
				}
			}
		}

		return false;
	}

	private boolean opj_pi_next_cprl(opj_pi_iterator_t pi) {
		opj_pi_comp_t comp = null;
		opj_pi_resolution_t res = null;
		int index = 0;
		boolean noskip1 = true;
		boolean noskip2 = true;

		if (!pi.first) {
			comp = pi.comps[pi.compno];
			noskip1 = false;
			noskip2 = false;
		} else {
			pi.first = false;
		}

		for (pi.compno = pi.poc.compno0; pi.compno < pi.poc.compno1; pi.compno++) {
			int resno;
			comp = pi.comps[pi.compno];
			pi.dx = 0;
			pi.dy = 0;
			for (resno = 0; resno < comp.numresolutions; resno++) {
				int dx, dy;
				res = comp.resolutions[resno];
				dx = comp.dx
						* (1 << (res.pdx + comp.numresolutions - 1 - resno));
				dy = comp.dy
						* (1 << (res.pdy + comp.numresolutions - 1 - resno));
				pi.dx = (pi.dx == 0) ? dx : Math.min(pi.dx, dx);
				pi.dy = (pi.dy == 0) ? dy : Math.min(pi.dy, dy);
			}
			if ((pi.tp_on == 0) && noskip1) {
				pi.poc.ty0 = pi.ty0;
				pi.poc.tx0 = pi.tx0;
				pi.poc.ty1 = pi.ty1;
				pi.poc.tx1 = pi.tx1;
			}
			for (pi.y = pi.poc.ty0; pi.y < pi.poc.ty1; pi.y += (pi.dy - (pi.y % pi.dy))) {
				for (pi.x = pi.poc.tx0; pi.x < pi.poc.tx1; pi.x += (pi.dx - (pi.x % pi.dx))) {
					for (pi.resno = pi.poc.resno0; pi.resno < Math.min(
							pi.poc.resno1, comp.numresolutions); pi.resno++) {
						if (noskip1) {
							int levelno;
							int trx0, try0;
							int trx1, try1;
							int rpx, rpy;
							int prci, prcj;
							res = comp.resolutions[pi.resno];
							levelno = comp.numresolutions - 1 - pi.resno;
							trx0 = (pi.tx0 + (comp.dx << levelno) - 1)
									/ (comp.dx << levelno);
							try0 = (pi.ty0 + (comp.dy << levelno) - 1)
									/ (comp.dy << levelno);
							trx1 = (pi.tx1 + (comp.dx << levelno) - 1)
									/ (comp.dx << levelno);
							try1 = (pi.ty1 + (comp.dy << levelno) - 1)
									/ (comp.dy << levelno);
							rpx = res.pdx + levelno;
							rpy = res.pdy + levelno;
							if (!((pi.y % (comp.dy << rpy) == 0) || ((pi.y == pi.ty0) && (((try0 << levelno) % (1 << rpy)) != 0)))) {
								continue;
							}
							if (!((pi.x % (comp.dx << rpx) == 0) || ((pi.x == pi.tx0) && (((trx0 << levelno) % (1 << rpx)) != 0)))) {
								continue;
							}

							if ((res.pw == 0) || (res.ph == 0))
								continue;

							if ((trx0 == trx1) || (try0 == try1))
								continue;

							prci = (((pi.x + (comp.dx << levelno) - 1) / (comp.dx << levelno)) >>> res.pdx)
									- (trx0 >>> res.pdx);
							prcj = (((pi.y + (comp.dy << levelno) - 1) / (comp.dy << levelno)) >>> res.pdy)
									- (try0 >>> res.pdy);
							pi.precno = (prci + prcj * res.pw);
						} else {
							noskip1 = true;
						}
						for (pi.layno = pi.poc.layno0; pi.layno < pi.poc.layno1; pi.layno++) {
							if (noskip2) {
								index = pi.layno * pi.step_l + pi.resno
										* pi.step_r + pi.compno * pi.step_c
										+ pi.precno * pi.step_p;
								if (pi.include[index] == 0) {
									pi.include[index] = 1;
									return true;
								}
							} else {
								noskip2 = true;
							}
						}
					}
				}
			}
		}

		return false;
	}

	/*
	 * ========================================================== Packet
	 * iterator interface
	 * ==========================================================
	 */
	private opj_pi_iterator_t[] opj_pi_create_decode(opj_image_t p_image,
			opj_cp_t p_cp, int p_tile_no) {
		/* loop */
		int pino;
		int compno, resno;

		/* to store w, h, dx and dy fro all components and resolutions */
		// int l_tmp_data[];
		int l_tmp_ptr[][];

		/* encoding prameters to set */
		int l_max_res[] = new int[1];
		int l_max_prec[] = new int[1];
		int l_tx0[] = new int[1];
		int l_tx1[] = new int[1];
		int l_ty0[] = new int[1];
		int l_ty1[] = new int[1];
		int l_dx_min[] = new int[1];
		int l_dy_min[] = new int[1];
		int l_bound;
		int l_step_p, l_step_c, l_step_r, l_step_l;
		int l_data_stride;
		int i;

		/* pointers */
		opj_pi_iterator_t l_pi[] = null;
		opj_tcp_t l_tcp = null;
		opj_pi_comp_t l_current_comp = null;
		opj_image_comp_t l_img_comp = null;
		opj_pi_iterator_t l_current_pi = null;
		opj_pi_iterator_t last_pi;
		int l_encoding_value_ptr;

		/* preconditions in debug */
		if (p_cp == null) {
			MipavUtil
					.displayError("p_cp == null at opj_pi_create_decode entry");
			return null;
		}
		if (p_image == null) {
			MipavUtil
					.displayError("p_image == null at opj_pi_create_decode entry");
			return null;
		}
		if (p_tile_no >= p_cp.tw * p_cp.th) {
			MipavUtil
					.displayError("p_tile_no >= p_cp.tw * p_cp.th at opj_pi_create_decode entry");
			return null;
		}

		/* initializations */
		l_tcp = p_cp.tcps[p_tile_no];
		l_bound = l_tcp.numpocs + 1;

		l_data_stride = 4 * OPJ_J2K_MAXRLVLS;
		// l_tmp_data = new int[l_data_stride * p_image.numcomps];
		l_tmp_ptr = new int[p_image.numcomps][l_data_stride];

		/* memory allocation for pi */
		l_pi = opj_pi_create(p_image, p_cp, p_tile_no);
		if (l_pi == null) {
			// l_tmp_data = null;
			l_tmp_ptr = null;
			return null;
		}

		// l_encoding_value_ptr = l_tmp_data;
		// l_encoding_value_ptr = 0;
		/* update pointer array */
		// for
		// (compno = 0; compno < p_image.numcomps; ++compno)
		// {
		// l_tmp_ptr[compno] = l_encoding_value_ptr;
		// for (i = 0; i < l_data_stride; i++) {
		// l_tmp_ptr[compno][i] = l_tmp_data[compno*l_data_stride + i];
		// }
		// l_encoding_value_ptr += l_data_stride;
		// }
		/* get encoding parameters */
		opj_get_all_encoding_parameters(p_image, p_cp, p_tile_no, l_tx0, l_tx1,
				l_ty0, l_ty1, l_dx_min, l_dy_min, l_max_prec, l_max_res,
				l_tmp_ptr);

		/* step calculations */
		l_step_p = 1;
		l_step_c = l_max_prec[0] * l_step_p;
		l_step_r = p_image.numcomps * l_step_c;
		l_step_l = l_max_res[0] * l_step_r;

		/* set values for first packet iterator */
		l_current_pi = l_pi[0];

		/* memory allocation for include */
		l_current_pi.include = new short[(l_tcp.numlayers + 1) * l_step_l];

		/* special treatment for the first packet iterator */
		l_current_comp = l_current_pi.comps[0];
		l_img_comp = p_image.comps[0];

		l_current_pi.tx0 = l_tx0[0];
		l_current_pi.ty0 = l_ty0[0];
		l_current_pi.tx1 = l_tx1[0];
		l_current_pi.ty1 = l_ty1[0];

		/* l_current_pi->dx = l_img_comp->dx; */
		/* l_current_pi->dy = l_img_comp->dy; */

		l_current_pi.step_p = l_step_p;
		l_current_pi.step_c = l_step_c;
		l_current_pi.step_r = l_step_r;
		l_current_pi.step_l = l_step_l;

		/*
		 * allocation for components and number of components has already been
		 * calculated by opj_pi_create
		 */
		for (compno = 0; compno < l_current_pi.numcomps; ++compno) {
			opj_pi_resolution_t l_res = l_current_comp.resolutions[0];
			// l_encoding_value_ptr = l_tmp_ptr[compno];
			l_encoding_value_ptr = 0;
			l_current_comp.dx = l_img_comp.dx;
			l_current_comp.dy = l_img_comp.dy;
			/* resolutions have already been initialized */
			for (resno = 0; resno < l_current_comp.numresolutions; resno++) {
				l_res.pdx = l_tmp_ptr[compno][l_encoding_value_ptr++];
				l_res.pdy = l_tmp_ptr[compno][l_encoding_value_ptr++];
				l_res.pw = l_tmp_ptr[compno][l_encoding_value_ptr++];
				l_res.ph = l_tmp_ptr[compno][l_encoding_value_ptr++];
				if (resno < l_current_comp.numresolutions - 1) {
					l_res = l_current_comp.resolutions[resno + 1];
				}
			}
			if (compno < l_current_pi.numcomps - 1) {
				l_current_comp = l_current_pi.comps[compno + 1];
				l_img_comp = p_image.comps[compno + 1];
			}
		}
		if (l_pi.length > 1) {
		    l_current_pi = l_pi[1];
		}
		last_pi = l_pi[0];

		for (pino = 1; pino < l_bound; ++pino) {
			l_current_comp = l_current_pi.comps[0];
			l_img_comp = p_image.comps[0];

			l_current_pi.tx0 = l_tx0[0];
			l_current_pi.ty0 = l_ty0[0];
			l_current_pi.tx1 = l_tx1[0];
			l_current_pi.ty1 = l_ty1[0];
			/* l_current_pi->dx = l_dx_min; */
			/* l_current_pi->dy = l_dy_min; */
			l_current_pi.step_p = l_step_p;
			l_current_pi.step_c = l_step_c;
			l_current_pi.step_r = l_step_r;
			l_current_pi.step_l = l_step_l;

			/*
			 * allocation for components and number of components has already
			 * been calculated by opj_pi_create
			 */
			for (compno = 0; compno < l_current_pi.numcomps; ++compno) {
				opj_pi_resolution_t l_res = l_current_comp.resolutions[0];
				// l_encoding_value_ptr = l_tmp_ptr[compno];
				l_encoding_value_ptr = 0;

				l_current_comp.dx = l_img_comp.dx;
				l_current_comp.dy = l_img_comp.dy;
				/* resolutions have already been initialized */
				for (resno = 0; resno < l_current_comp.numresolutions; resno++) {
					l_res.pdx = l_tmp_ptr[compno][l_encoding_value_ptr++];
					l_res.pdy = l_tmp_ptr[compno][l_encoding_value_ptr++];
					l_res.pw = l_tmp_ptr[compno][l_encoding_value_ptr++];
					l_res.ph = l_tmp_ptr[compno][l_encoding_value_ptr++];
					if (resno < l_current_comp.numresolutions - 1) {
						l_res = l_current_comp.resolutions[resno + 1];
					}
				}
				if (compno < l_current_pi.numcomps - 1) {
					l_current_comp = l_current_pi.comps[compno + 1];
					l_img_comp = p_image.comps[compno + 1];
				}
			}
			/* special treatment */
			l_current_pi.include = last_pi.include;
			if (pino < l_bound - 1) {
				last_pi = l_current_pi;
				l_current_pi = l_pi[pino + 1];
			}
		}
		// opj_free(l_tmp_data);
		// l_tmp_data = 00;
		for (i = 0; i < l_tmp_ptr.length; i++) {
			l_tmp_ptr[i] = null;
		}
		l_tmp_ptr = null;
		if (l_tcp.POC) {
			opj_pi_update_decode_poc(l_pi, l_tcp, l_max_prec[0], l_max_res[0]);
		} else {
			opj_pi_update_decode_not_poc(l_pi, l_tcp, l_max_prec[0],
					l_max_res[0]);
		}
		return l_pi;
	}

	private void opj_pi_update_decode_poc(opj_pi_iterator_t p_pi[],
			opj_tcp_t p_tcp, int p_max_precision, int p_max_res) {
		/* loop */
		int pino;

		/* encoding prameters to set */
		int l_bound;

		opj_pi_iterator_t l_current_pi = null;
		opj_poc_t l_current_poc = null;

		// OPJ_ARG_NOT_USED(p_max_res);

		/* preconditions in debug */
		if (p_pi == null) {
			MipavUtil
					.displayError("p_pi == null at opj_pi_update_decode_poc entry");
			return;
		}
		if (p_tcp == null) {
			MipavUtil
					.displayError("p_tcp == null at opj_pi_update_decode_poc entry");
			return;
		}

		/* initializations */
		l_bound = p_tcp.numpocs + 1;
		l_current_pi = p_pi[0];
		l_current_poc = p_tcp.pocs[0];

		for (pino = 0; pino < l_bound; ++pino) {
			l_current_pi.poc.prg = l_current_poc.prg; /* Progression Order #0 */
			l_current_pi.first = true;

			l_current_pi.poc.resno0 = l_current_poc.resno0; /*
															 * Resolution Level
															 * Index #0 (Start)
															 */
			l_current_pi.poc.compno0 = l_current_poc.compno0; /*
															 * Component Index
															 * #0 (Start)
															 */
			l_current_pi.poc.layno0 = 0;
			l_current_pi.poc.precno0 = 0;
			l_current_pi.poc.resno1 = l_current_poc.resno1; /*
															 * Resolution Level
															 * Index #0 (End)
															 */
			l_current_pi.poc.compno1 = l_current_poc.compno1; /*
															 * Component Index
															 * #0 (End)
															 */
			l_current_pi.poc.layno1 = l_current_poc.layno1; /*
															 * Layer Index #0
															 * (End)
															 */
			l_current_pi.poc.precno1 = p_max_precision;
			if (pino < l_bound - 1) {
				l_current_pi = p_pi[pino + 1];
				l_current_poc = p_tcp.pocs[pino + 1];
			}
		}
	}

	private void opj_pi_update_decode_not_poc(opj_pi_iterator_t p_pi[],
			opj_tcp_t p_tcp, int p_max_precision, int p_max_res) {
		/* loop */
		int pino;

		/* encoding prameters to set */
		int l_bound;

		opj_pi_iterator_t l_current_pi = null;
		/* preconditions in debug */
		if (p_pi == null) {
			MipavUtil
					.displayError("p_pi == null at opj_pi_update_decode_not_poc entry");
			return;
		}
		if (p_tcp == null) {
			MipavUtil
					.displayError("p_tcp == null at opj_pi_update_decode_not_poc entry");
			return;
		}

		/* initializations */
		l_bound = p_tcp.numpocs + 1;
		l_current_pi = p_pi[0];

		for (pino = 0; pino < l_bound; ++pino) {
			l_current_pi.poc.prg = p_tcp.prg;
			l_current_pi.first = true;
			l_current_pi.poc.resno0 = 0;
			l_current_pi.poc.compno0 = 0;
			l_current_pi.poc.layno0 = 0;
			l_current_pi.poc.precno0 = 0;
			l_current_pi.poc.resno1 = p_max_res;
			l_current_pi.poc.compno1 = l_current_pi.numcomps;
			l_current_pi.poc.layno1 = p_tcp.numlayers;
			l_current_pi.poc.precno1 = p_max_precision;
			if (pino < l_bound - 1) {
				l_current_pi = p_pi[pino + 1];
			}
		}
	}

	/**
	 * Gets the encoding parameters needed to update the coding parameters and
	 * all the pocs. The precinct widths, heights, dx and dy for each component
	 * at each resolution will be stored as well. the last parameter of the
	 * function should be an array of pointers of size nb components, each
	 * pointer leading to an area of size 4 * max_res. The data is stored inside
	 * this area with the following pattern : dx_compi_res0 , dy_compi_res0 ,
	 * w_compi_res0, h_compi_res0 , dx_compi_res1 , dy_compi_res1 ,
	 * w_compi_res1, h_compi_res1 , ...
	 * 
	 * @param p_image
	 *            the image being encoded.
	 * @param p_cp
	 *            the coding parameters.
	 * @param tileno
	 *            the tile index of the tile being encoded.
	 * @param p_tx0
	 *            pointer that will hold the X0 parameter for the tile
	 * @param p_tx1
	 *            pointer that will hold the X1 parameter for the tile
	 * @param p_ty0
	 *            pointer that will hold the Y0 parameter for the tile
	 * @param p_ty1
	 *            pointer that will hold the Y1 parameter for the tile
	 * @param p_max_prec
	 *            pointer that will hold the the maximum precision for all the
	 *            bands of the tile
	 * @param p_max_res
	 *            pointer that will hold the the maximum number of resolutions
	 *            for all the poc inside the tile.
	 * @param p_dx_min
	 *            pointer that will hold the the minimum dx of all the
	 *            components of all the resolutions for the tile.
	 * @param p_dy_min
	 *            pointer that will hold the the minimum dy of all the
	 *            components of all the resolutions for the tile.
	 * @param p_resolutions
	 *            pointer to an area corresponding to the one described above.
	 */
	private void opj_get_all_encoding_parameters(final opj_image_t p_image,
			final opj_cp_t p_cp, int tileno, int p_tx0[], int p_tx1[],
			int p_ty0[], int p_ty1[], int p_dx_min[], int p_dy_min[],
			int p_max_prec[], int p_max_res[], int p_resolutions[][]) {
		/* loop */
		int compno, resno;

		/* pointers */
		opj_tcp_t tcp = null;
		opj_tccp_t l_tccp = null;
		opj_image_comp_t l_img_comp = null;

		/* to store l_dx, l_dy, w and h for each resolution and component. */
		int lResolutionPtr = 0;

		/* position in x and y of tile */
		int p, q;

		/* non-corrected (in regard to image offset) tile offset */
		int l_tx0, l_ty0;

		/* preconditions in debug */
		if (p_cp == null) {
			MipavUtil
					.displayError("p_cp == null at opj_get_all_encoding_parameters entry");
			return;
		}
		if (p_image == null) {
			MipavUtil
					.displayError("p_image == null at opj_get_all_encoding_parameters entry");
			return;
		}
		if (tileno >= p_cp.tw * p_cp.th) {
			MipavUtil
					.displayError("tileno >= p_cp.tw * p_cp.th at opj_get_all_encoding_parameters entry");
		}

		/* initializations */
		tcp = p_cp.tcps[tileno];
		l_tccp = tcp.tccps[0];
		l_img_comp = p_image.comps[0];

		/* position in x and y of tile */
		p = tileno % p_cp.tw;
		q = tileno / p_cp.tw;

		/* here calculation of tx0, tx1, ty0, ty1, maxprec, l_dx and l_dy */
		l_tx0 = p_cp.tx0 + p * p_cp.tdx; /*
										 * can't be greater than p_image->x1 so
										 * won't overflow
										 */
		p_tx0[0] = Math.max(l_tx0, p_image.x0);
		p_tx1[0] = Math.min(l_tx0 + p_cp.tdx, p_image.x1);
		l_ty0 = p_cp.ty0 + q * p_cp.tdy; /*
										 * can't be greater than p_image->y1 so
										 * won't overflow
										 */
		p_ty0[0] = Math.max(l_ty0, p_image.y0);
		p_ty1[0] = Math.min(l_ty0 + p_cp.tdy, p_image.y1);

		/* max precision and resolution is 0 (can only grow) */
		p_max_prec[0] = 0;
		p_max_res[0] = 0;

		/* take the largest value for dx_min and dy_min */
		p_dx_min[0] = 0x7fffffff;
		p_dy_min[0] = 0x7fffffff;

		for (compno = 0; compno < p_image.numcomps; ++compno) {
			/* aritmetic variables to calculate */
			int l_level_no;
			int l_rx0, l_ry0, l_rx1, l_ry1;
			int l_px0, l_py0, l_px1, py1;
			int l_product;
			int l_tcx0, l_tcy0, l_tcx1, l_tcy1;
			int l_pdx, l_pdy, l_pw, l_ph;

			// lResolutionPtr = p_resolutions[compno];
			lResolutionPtr = 0;

			l_tcx0 = (p_tx0[0] + l_img_comp.dx - 1) / l_img_comp.dx;
			l_tcy0 = (p_ty0[0] + l_img_comp.dy - 1) / l_img_comp.dy;
			l_tcx1 = (p_tx1[0] + l_img_comp.dx - 1) / l_img_comp.dx;
			l_tcy1 = (p_ty1[0] + l_img_comp.dy - 1) / l_img_comp.dy;

			if (l_tccp.numresolutions > p_max_res[0]) {
				p_max_res[0] = l_tccp.numresolutions;
			}

			/* use custom size for precincts */
			l_level_no = l_tccp.numresolutions - 1;
			for (resno = 0; resno < l_tccp.numresolutions; ++resno) {
				int l_dx, l_dy;

				/* precinct width and height */
				l_pdx = l_tccp.prcw[resno];
				l_pdy = l_tccp.prch[resno];
				p_resolutions[compno][lResolutionPtr++] = l_pdx;
				p_resolutions[compno][lResolutionPtr++] = l_pdy;
				l_dx = l_img_comp.dx * (1 << (l_pdx + l_level_no));
				l_dy = l_img_comp.dy * (1 << (l_pdy + l_level_no));
				/* take the minimum size for l_dx for each comp and resolution */
				p_dx_min[0] = Math.min(p_dx_min[0], l_dx);
				p_dy_min[0] = Math.min(p_dy_min[0], l_dy);

				/* various calculations of extents */
				l_rx0 = (int) ((l_tcx0 + (1L << l_level_no) - 1) >>> l_level_no);
				l_ry0 = (int) ((l_tcy0 + (1L << l_level_no) - 1) >>> l_level_no);
				l_rx1 = (int) ((l_tcx1 + (1L << l_level_no) - 1) >>> l_level_no);
				l_ry1 = (int) ((l_tcy1 + (1L << l_level_no) - 1) >>> l_level_no);
				l_px0 = (l_rx0 >> l_pdx) << l_pdx;
				l_py0 = (l_ry0 >> l_pdy) << l_pdy;
				l_px1 = (int) ((l_rx1 + (1L << l_pdx) - 1) >>> l_pdx) << l_pdx;
				py1 = (int) ((l_ry1 + (1L << l_pdy) - 1) >>> l_pdy) << l_pdy;
				l_pw = (l_rx0 == l_rx1) ? 0 : ((l_px1 - l_px0) >>> l_pdx);
				l_ph = (l_ry0 == l_ry1) ? 0 : ((py1 - l_py0) >>> l_pdy);
				p_resolutions[compno][lResolutionPtr++] = l_pw;
				p_resolutions[compno][lResolutionPtr++] = l_ph;
				l_product = l_pw * l_ph;

				/* update precision */
				if (l_product > p_max_prec[0]) {
					p_max_prec[0] = l_product;
				}

				--l_level_no;
			}
			if (compno < p_image.numcomps - 1) {
				l_tccp = tcp.tccps[compno + 1];
				l_img_comp = p_image.comps[compno + 1];
			}
		}
	}

	private opj_pi_iterator_t[] opj_pi_create(final opj_image_t image,
			final opj_cp_t cp, int tileno) {
		/* loop */
		int pino, compno;
		/* number of poc in the p_pi */
		int l_poc_bound;

		/* pointers to tile coding parameters and components. */
		opj_pi_iterator_t l_pi[] = null;
		opj_tcp_t tcp = null;
		opj_tccp_t tccp = null;
		int i;

		/* current packet iterator being allocated */
		opj_pi_iterator_t l_current_pi = null;

		/* preconditions in debug */
		if (cp == null) {
			MipavUtil.displayError("cp == null at opj_pi_create");
			return null;
		}
		if (image == null) {
			MipavUtil.displayError("image == null at opj_pi_create");
			return null;
		}
		if (tileno >= cp.tw * cp.th) {
			MipavUtil.displayError("tileno >= cp.tw * cp.th at opj_pi_create");
		}

		/* initializations */
		tcp = cp.tcps[tileno];
		l_poc_bound = tcp.numpocs + 1;

		/* memory allocations */
		l_pi = new opj_pi_iterator_t[l_poc_bound];
		for (i = 0; i < l_poc_bound; i++) {
			l_pi[i] = new opj_pi_iterator_t();
		}

		l_current_pi = l_pi[0];
		for (pino = 0; pino < l_poc_bound; ++pino) {

			l_current_pi.comps = new opj_pi_comp_t[image.numcomps];
			for (i = 0; i < image.numcomps; i++) {
				l_current_pi.comps[i] = new opj_pi_comp_t();
			}

			l_current_pi.numcomps = image.numcomps;

			for (compno = 0; compno < image.numcomps; ++compno) {
				opj_pi_comp_t comp = l_current_pi.comps[compno];

				tccp = tcp.tccps[compno];

				comp.resolutions = new opj_pi_resolution_t[tccp.numresolutions];
				for (i = 0; i < tccp.numresolutions; i++) {
					comp.resolutions[i] = new opj_pi_resolution_t();
				}

				comp.numresolutions = tccp.numresolutions;
			}
			if (pino < l_poc_bound - 1) {
				l_current_pi = l_pi[pino + 1];
			}
		}
		return l_pi;
	}

	private void opj_pi_destroy(opj_pi_iterator_t p_pi[], int p_nb_elements) {
		int compno, pino;
		opj_pi_iterator_t l_current_pi = p_pi[0];

		for (pino = 0; pino < p_nb_elements; ++pino) {
			l_current_pi.include = null;
			if (l_current_pi.comps != null) {
				opj_pi_comp_t l_current_component = l_current_pi.comps[0];
				for (compno = 0; compno < l_current_pi.numcomps; compno++) {
					if (l_current_component.resolutions != null) {
						l_current_component.resolutions = null;
					}
					if (compno < l_current_pi.numcomps - 1) {
						l_current_component = l_current_pi.comps[compno + 1];
					}
				}
				l_current_pi.comps = null;
			}
			if (pino < p_nb_elements - 1) {
				l_current_pi = p_pi[pino + 1];
			}
		}
		p_pi = null;
	}

	/**
	 * Creates a Tier 2 handle
	 * 
	 * @param p_image
	 *            Source or destination image
	 * @param p_cp
	 *            Image coding parameters.
	 * @return a new T2 handle if successful, NULL otherwise.
	 */
	private opj_t2_t opj_t2_create(opj_image_t p_image, opj_cp_t p_cp) {
		/* create the t2 structure */
		opj_t2_t l_t2 = new opj_t2_t();

		l_t2.image = p_image;
		l_t2.cp = p_cp;

		return l_t2;
	}

	void opj_t2_destroy(opj_t2_t t2) {
		if (t2 != null) {	
			t2 = null;
		}
	}

	private boolean opj_tcd_t1_decode(opj_tcd_t p_tcd) {
		int compno;
		opj_t1_t l_t1;
		opj_tcd_tile_t l_tile = p_tcd.tcd_image.tiles;
		opj_tcd_tilecomp_t l_tile_comp = l_tile.comps[0];
		opj_tccp_t l_tccp = p_tcd.tcp.tccps[0];

		l_t1 = opj_t1_create(false);
		if (l_t1 == null) {
			return false;
		}

		for (compno = 0; compno < l_tile.numcomps; ++compno) {
			/* The +3 is headroom required by the vectorized DWT */
			if (false == opj_t1_decode_cblks(l_t1, l_tile_comp, l_tccp)) {
				opj_t1_destroy(l_t1);
				return false;
			}
			if (compno < l_tile.numcomps - 1) {
				l_tile_comp = l_tile.comps[compno + 1];
				l_tccp = p_tcd.tcp.tccps[compno + 1];
			}
		}

		opj_t1_destroy(l_t1);

		return true;
	}

	/**
	 * Creates a new Tier 1 handle and initializes the look-up tables of the
	 * Tier-1 coder/decoder
	 * 
	 * @return a new T1 handle if successful, returns NULL otherwise
	 */
	private opj_t1_t opj_t1_create(boolean isEncoder) {
		opj_t1_t l_t1 = null;

		l_t1 = new opj_t1_t();

		/* create MQC and RAW handles */
		l_t1.mqc = opj_mqc_create();
		if (l_t1.mqc == null) {
			opj_t1_destroy(l_t1);
			return null;
		}

		l_t1.raw = new opj_raw_t();
		if (l_t1.raw == null) {
			opj_t1_destroy(l_t1);
			return null;
		}
		l_t1.encoder = isEncoder;

		return l_t1;
	}

	/*
	 * ========================================================== MQ-Coder
	 * interface ==========================================================
	 */

	private opj_mqc_t opj_mqc_create() {
		opj_mqc_t mqc = new opj_mqc_t();
		if (MQC_PERF_OPT) {
			if (mqc != null) {
				mqc.buffer = null;
			}
		}
		return mqc;
	}

	private boolean opj_t1_decode_cblks(opj_t1_t t1, opj_tcd_tilecomp_t tilec,
			opj_tccp_t tccp) {
		int resno, bandno, precno, cblkno;
		int tile_w = (tilec.x1 - tilec.x0);

		for (resno = 0; resno < tilec.minimum_num_resolutions; ++resno) {
			opj_tcd_resolution_t res = tilec.resolutions[resno];

			for (bandno = 0; bandno < res.numbands; ++bandno) {
				opj_tcd_band_t band = res.bands[bandno];

				for (precno = 0; precno < res.pw * res.ph; ++precno) {
					opj_tcd_precinct_t precinct = band.precincts[precno];

					for (cblkno = 0; cblkno < precinct.cw * precinct.ch; ++cblkno) {
						opj_tcd_cblk_dec_t cblk = precinct.dec[cblkno];
						int datap[];
						int cblk_w, cblk_h;
						int x, y;
						int i, j;
						int datap_index;
						int tiledp_index;
						int tiledp2_index;

						if (false == opj_t1_decode_cblk(t1, cblk, band.bandno,
								tccp.roishift, tccp.cblksty)) {
							return false;
						}

						x = cblk.x0 - band.x0;
						y = cblk.y0 - band.y0;
						if ((band.bandno & 1) != 0) {
							opj_tcd_resolution_t pres = tilec.resolutions[resno - 1];
							x += pres.x1 - pres.x0;
						}
						if ((band.bandno & 2) != 0) {
							opj_tcd_resolution_t pres = tilec.resolutions[resno - 1];
							y += pres.y1 - pres.y0;
						}

						datap = t1.data;
						datap_index = 0;
						cblk_w = t1.w;
						cblk_h = t1.h;

						if (tccp.roishift != 0) {
							int thresh = 1 << tccp.roishift;
							for (j = 0; j < cblk_h; ++j) {
								for (i = 0; i < cblk_w; ++i) {
									int val = datap[(j * cblk_w) + i];
									int mag = Math.abs(val);
									if (mag >= thresh) {
										mag >>>= tccp.roishift;
										datap[(j * cblk_w) + i] = val < 0 ? -mag
												: mag;
									}
								}
							}
						}
						if (tccp.qmfbid == 1) {
							for (j = 0; j < cblk_h; ++j) {
								for (i = 0; i < cblk_w; ++i) {
									int tmp = datap[(j * cblk_w) + i];
									tilec.data[y * tile_w + x + (j * tile_w)
											+ i] = tmp / 2;
								}
							}
						} else { /* if (tccp->qmfbid == 0) */
							tiledp_index = y * tile_w + x;
							// OPJ_FLOAT32* restrict tiledp = (OPJ_FLOAT32*)
							// &tilec->data[(OPJ_UINT32)y * tile_w +
							// (OPJ_UINT32)x];
							for (j = 0; j < cblk_h; ++j) {
								tiledp2_index = tiledp_index;
								// OPJ_FLOAT32* restrict tiledp2 = tiledp;
								for (i = 0; i < cblk_w; ++i) {
									// OPJ_FLOAT32 tmp = (OPJ_FLOAT32)*datap *
									// band->stepsize;
									float tmp = (float) datap[datap_index]
											* band.stepsize;
									tilec.data[tiledp2_index] = (int) tmp;
									datap_index++;
									tiledp2_index++;
								}
								tiledp_index += tile_w;
							}
						}
					} /* cblkno */
				} /* precno */
			} /* bandno */
		} /* resno */
		return true;
	}

	private boolean opj_t1_decode_cblk(opj_t1_t t1, opj_tcd_cblk_dec_t cblk,
			int orient, int roishift, int cblksty) {
		opj_raw_t raw = t1.raw; /* RAW component */
		opj_mqc_t mqc = t1.mqc; /* MQC component */

		int bpno_plus_one;
		int passtype;
		int segno, passno;
		byte type = T1_TYPE_MQ; /* BYPASS mode */

		if (!opj_t1_allocate_buffers(t1, (cblk.x1 - cblk.x0),
				(cblk.y1 - cblk.y0))) {
			return false;
		}

		bpno_plus_one = (roishift + cblk.numbps);
		passtype = 2;

		opj_mqc_resetstates(mqc);
		opj_mqc_setstate(mqc, T1_CTXNO_UNI, 0, 46);
		opj_mqc_setstate(mqc, T1_CTXNO_AGG, 0, 3);
		opj_mqc_setstate(mqc, T1_CTXNO_ZC, 0, 4);

		for (segno = 0; segno < cblk.real_num_segs; ++segno) {
			opj_tcd_seg_t seg = cblk.segs[segno];

			/* BYPASS mode */
			type = ((bpno_plus_one <= ((cblk.numbps)) - 4) && (passtype < 2) && ((cblksty & J2K_CCP_CBLKSTY_LAZY) != 0)) ? (byte) T1_TYPE_RAW
					: (byte) T1_TYPE_MQ;
			/*
			 * FIXME: slviewer gets here with a null pointer. Why? Partially
			 * downloaded and/or corrupt textures?
			 */
			if (seg.data == null) {
				continue;
			}
			if (type == T1_TYPE_RAW) {
				opj_raw_init_dec(raw, seg.data, seg.dataindex, seg.len);
			} else {
				if (false == opj_mqc_init_dec(mqc, seg.data, seg.dataindex,
						seg.len)) {
					return false;
				}
			}

			for (passno = 0; passno < seg.real_num_passes; ++passno) {
				switch (passtype) {
				case 0:
					if (type == T1_TYPE_RAW) {
						opj_t1_dec_sigpass_raw(t1, bpno_plus_one, orient,
								cblksty);
					} else {
						if ((cblksty & J2K_CCP_CBLKSTY_VSC) != 0) {
							opj_t1_dec_sigpass_mqc_vsc(t1, bpno_plus_one,
									orient);
						} else {
							opj_t1_dec_sigpass_mqc(t1, bpno_plus_one, orient);
						}
					}
					break;
				case 1:
					if (type == T1_TYPE_RAW) {
						opj_t1_dec_refpass_raw(t1, bpno_plus_one, cblksty);
					} else {
						if ((cblksty & J2K_CCP_CBLKSTY_VSC) != 0) {
							opj_t1_dec_refpass_mqc_vsc(t1, bpno_plus_one);
						} else {
							opj_t1_dec_refpass_mqc(t1, bpno_plus_one);
						}
					}
					break;
				case 2:
					opj_t1_dec_clnpass(t1, bpno_plus_one, orient, cblksty);
					break;
				}

				if (((cblksty & J2K_CCP_CBLKSTY_RESET) != 0)
						&& type == T1_TYPE_MQ) {
					opj_mqc_resetstates(mqc);
					opj_mqc_setstate(mqc, T1_CTXNO_UNI, 0, 46);
					opj_mqc_setstate(mqc, T1_CTXNO_AGG, 0, 3);
					opj_mqc_setstate(mqc, T1_CTXNO_ZC, 0, 4);
				}
				if (++passtype == 3) {
					passtype = 0;
					bpno_plus_one--;
				}
			}
		}
		return true;
	}

	private void opj_t1_dec_clnpass(opj_t1_t t1, int bpno, int orient,
			int cblksty) {
		int one, half, oneplushalf, runlen, vsc;
		boolean agg;
		int i, j, k;
		int segsym = cblksty & J2K_CCP_CBLKSTY_SEGSYM;

		opj_mqc_t mqc = t1.mqc; /* MQC component */

		one = 1 << bpno;
		half = one >> 1;
		oneplushalf = one | half;
		if ((cblksty & J2K_CCP_CBLKSTY_VSC) != 0) {
			for (k = 0; k < t1.h; k += 4) {
				for (i = 0; i < t1.w; ++i) {
					if (k + 3 < t1.h) {
						agg = ((MACRO_t1_flags(t1, 1 + k, 1 + i) & (T1_SIG
								| T1_VISIT | T1_SIG_OTH)) == 0)
								&& ((MACRO_t1_flags(t1, 1 + k + 1, 1 + i) & (T1_SIG
										| T1_VISIT | T1_SIG_OTH)) == 0)
								&& ((MACRO_t1_flags(t1, 1 + k + 2, 1 + i) & (T1_SIG
										| T1_VISIT | T1_SIG_OTH)) == 0)
								&& (((MACRO_t1_flags(t1, 1 + k + 3, 1 + i) & (~(T1_SIG_S
										| T1_SIG_SE | T1_SIG_SW | T1_SGN_S))) & (T1_SIG
										| T1_VISIT | T1_SIG_OTH)) == 0);
					} else {
						agg = false;
					}
					if (agg) {
						opj_mqc_setcurctx(mqc, T1_CTXNO_AGG);
						if (opj_mqc_decode(mqc) == 0) {
							continue;
						}
						opj_mqc_setcurctx(mqc, T1_CTXNO_UNI);
						runlen = opj_mqc_decode(mqc);
						runlen = (runlen << 1) | opj_mqc_decode(mqc);
					} else {
						runlen = 0;
					}
					for (j = k + runlen; j < k + 4 && j < t1.h; ++j) {
						vsc = (j == k + 3 || j == t1.h - 1) ? 1 : 0;
						opj_t1_dec_clnpass_step_vsc(t1, t1.flags,
								((j + 1) * t1.flags_stride) + i + 1, t1.data,
								(j * t1.w) + i, orient, oneplushalf, agg
										&& (j == k + runlen), vsc);
					}
				}
			}
		} else {
			int data1_index = 0;
			int flags1_index = 1;
			for (k = 0; k < (t1.h & ~3); k += 4) {
				for (i = 0; i < t1.w; ++i) {
					// OPJ_INT32 *data2 = data1 + i;
					int data2_index = data1_index + i;
					// opj_flag_t *flags2 = flags1 + i;
					int flags2_index = flags1_index + i;
					agg = (((MACRO_t1_flags(t1, 1 + k, 1 + i)
							| MACRO_t1_flags(t1, 1 + k + 1, 1 + i)
							| MACRO_t1_flags(t1, 1 + k + 2, 1 + i) | MACRO_t1_flags(
							t1, 1 + k + 3, 1 + i)) & (T1_SIG | T1_VISIT | T1_SIG_OTH)) == 0);
					if (agg) {
						opj_mqc_setcurctx(mqc, T1_CTXNO_AGG);
						if (opj_mqc_decode(mqc) == 0) {
							continue;
						}
						opj_mqc_setcurctx(mqc, T1_CTXNO_UNI);
						runlen = opj_mqc_decode(mqc);
						runlen = (runlen << 1) | opj_mqc_decode(mqc);
						flags2_index += runlen * t1.flags_stride;
						data2_index += runlen * t1.w;
						for (j = runlen; j < 4 && j < t1.h; ++j) {
							flags2_index += t1.flags_stride;
							if (agg && (j == runlen)) {
								opj_t1_dec_clnpass_step_partial(t1, t1.flags,
										flags2_index, t1.data, data2_index,
										orient, oneplushalf);
							} else {
								opj_t1_dec_clnpass_step(t1, t1.flags,
										flags2_index, t1.data, data2_index,
										orient, oneplushalf);
							}
							data2_index += t1.w;
						}
					} else {
						flags2_index += t1.flags_stride;
						opj_t1_dec_clnpass_step(t1, t1.flags, flags2_index,
								t1.data, data2_index, orient, oneplushalf);
						data2_index += t1.w;
						flags2_index += t1.flags_stride;
						opj_t1_dec_clnpass_step(t1, t1.flags, flags2_index,
								t1.data, data2_index, orient, oneplushalf);
						data2_index += t1.w;
						flags2_index += t1.flags_stride;
						opj_t1_dec_clnpass_step(t1, t1.flags, flags2_index,
								t1.data, data2_index, orient, oneplushalf);
						data2_index += t1.w;
						flags2_index += t1.flags_stride;
						opj_t1_dec_clnpass_step(t1, t1.flags, flags2_index,
								t1.data, data2_index, orient, oneplushalf);
						data2_index += t1.w;
					}
				}
				data1_index += t1.w << 2;
				flags1_index += t1.flags_stride << 2;
			}
			for (i = 0; i < t1.w; ++i) {
				int data2_index = data1_index + i;
				int flags2_index = flags1_index + i;
				for (j = k; j < t1.h; ++j) {
					flags2_index += t1.flags_stride;
					opj_t1_dec_clnpass_step(t1, t1.flags, flags2_index,
							t1.data, data2_index, orient, oneplushalf);
					data2_index += t1.w;
				}
			}
		}

		if (segsym != 0) {
			int v = 0;
			opj_mqc_setcurctx(mqc, T1_CTXNO_UNI);
			v = opj_mqc_decode(mqc);
			v = (v << 1) | opj_mqc_decode(mqc);
			v = (v << 1) | opj_mqc_decode(mqc);
			v = (v << 1) | opj_mqc_decode(mqc);
			/*
			 * if (v!=0xa) { opj_event_msg(t1->cinfo, EVT_WARNING,
			 * "Bad segmentation symbol %x\n", v); }
			 */
		}
	} /* VSC and BYPASS by Antonin */

	private void opj_t1_dec_clnpass_step(opj_t1_t t1, short flagsp[],
			int flagsp_index, int datap[], int datap_index, int orient,
			int oneplushalf) {
		int v, flag;

		opj_mqc_t mqc = t1.mqc; /* MQC component */

		flag = flagsp[flagsp_index];
		if ((flag & (T1_SIG | T1_VISIT)) == 0) {
			opj_mqc_setcurctx(mqc, opj_t1_getctxno_zc(flag, orient));
			if (opj_mqc_decode(mqc) != 0) {
				opj_mqc_setcurctx(mqc, opj_t1_getctxno_sc(flag));
				v = opj_mqc_decode(mqc) ^ opj_t1_getspb(flag);
				datap[datap_index] = (v != 0) ? -oneplushalf : oneplushalf;
				opj_t1_updateflags(flagsp, flagsp_index, v, t1.flags_stride);
			}
		}
		flagsp[flagsp_index] &= ~T1_VISIT;
	} /* VSC and BYPASS by Antonin */

	private void opj_t1_updateflags(short flagsp[], int flagsp_index, int s,
			int stride) {
		// opj_flag_t *np = flagsp - stride;
		// opj_flag_t *sp = flagsp + stride;

		short mod[] = new short[] { T1_SIG_S, T1_SIG_S | T1_SGN_S, T1_SIG_E,
				T1_SIG_E | T1_SGN_E, T1_SIG_W, T1_SIG_W | T1_SGN_W, T1_SIG_N,
				T1_SIG_N | T1_SGN_N };

		flagsp[flagsp_index - stride - 1] |= T1_SIG_SE;
		flagsp[flagsp_index - stride] |= mod[s];
		flagsp[flagsp_index - stride + 1] |= T1_SIG_SW;

		flagsp[flagsp_index - 1] |= mod[s + 2];
		flagsp[flagsp_index] |= T1_SIG;
		flagsp[flagsp_index + 1] |= mod[s + 4];

		flagsp[flagsp_index + stride - 1] |= T1_SIG_NE;
		flagsp[flagsp_index + stride] |= mod[s + 6];
		flagsp[flagsp_index + stride + 1] |= T1_SIG_NW;
	}

	private byte opj_t1_getspb(int f) {
		return lut_spb[(f & (T1_SIG_PRIM | T1_SGN)) >> 4];
	}

	private byte opj_t1_getctxno_sc(int f) {
		return lut_ctxno_sc[(f & (T1_SIG_PRIM | T1_SGN)) >> 4];
	}

	private byte opj_t1_getctxno_zc(int f, int orient) {
		return lut_ctxno_zc[(orient << 8) | (f & T1_SIG_OTH)];
	}

	private void opj_t1_dec_clnpass_step_partial(opj_t1_t t1, short flagsp[],
			int flagsp_index, int datap[], int datap_index, int orient,
			int oneplushalf) {
		int v, flag;
		opj_mqc_t mqc = t1.mqc; /* MQC component */

		// OPJ_ARG_NOT_USED(orient);

		flag = flagsp[flagsp_index];
		opj_mqc_setcurctx(mqc, opj_t1_getctxno_sc(flag));
		v = opj_mqc_decode(mqc) ^ opj_t1_getspb(flag);
		datap[datap_index] = (v != 0) ? -oneplushalf : oneplushalf;
		opj_t1_updateflags(flagsp, flagsp_index, v, t1.flags_stride);
		flagsp[flagsp_index] &= ~T1_VISIT;
	} /* VSC and BYPASS by Antonin */

	private void opj_t1_dec_clnpass_step_vsc(opj_t1_t t1, short flagsp[],
			int flagsp_index, int datap[], int datap_index, int orient,
			int oneplushalf, boolean partial, int vsc) {
		int v, flag;

		opj_mqc_t mqc = t1.mqc; /* MQC component */

		flag = (vsc != 0) ? ((flagsp[flagsp_index]) & (~(T1_SIG_S | T1_SIG_SE
				| T1_SIG_SW | T1_SGN_S))) : (flagsp[flagsp_index]);
		if (partial) {
			opj_mqc_setcurctx(mqc, opj_t1_getctxno_sc(flag));
			v = opj_mqc_decode(mqc) ^ opj_t1_getspb(flag);
			datap[datap_index] = (v != 0) ? -oneplushalf : oneplushalf;
			opj_t1_updateflags(flagsp, flagsp_index, v, t1.flags_stride);
		} else if ((flag & (T1_SIG | T1_VISIT)) == 0) {
			opj_mqc_setcurctx(mqc, opj_t1_getctxno_zc(flag, orient));
			if (opj_mqc_decode(mqc) != 0) {
				opj_mqc_setcurctx(mqc, opj_t1_getctxno_sc(flag));
				v = opj_mqc_decode(mqc) ^ opj_t1_getspb(flag);
				datap[datap_index] = (v != 0) ? -oneplushalf : oneplushalf;
				opj_t1_updateflags(flagsp, flagsp_index, v, t1.flags_stride);
			}
		}
		flagsp[flagsp_index] &= ~T1_VISIT;
	}

	private int opj_mqc_decode(opj_mqc_t mqc) {
		int d;
		mqc.a -= (mqc.curctx).qeval;
		if ((mqc.c >> 16) < (mqc.curctx).qeval) {
			d = opj_mqc_lpsexchange(mqc);
			opj_mqc_renormd(mqc);
		} else {
			mqc.c -= (mqc.curctx).qeval << 16;
			if ((mqc.a & 0x8000) == 0) {
				d = opj_mqc_mpsexchange(mqc);
				opj_mqc_renormd(mqc);
			} else {
				d = (mqc.curctx).mps;
			}
		}

		return d;
	}

	private int opj_mqc_mpsexchange(opj_mqc_t mqc) {
		int d;
		if (mqc.a < (mqc.curctx).qeval) {
			d = (1 - (mqc.curctx).mps);
			mqc.curctx = (mqc.curctx).nlps;
		} else {
			d = (mqc.curctx).mps;
			mqc.curctx = (mqc.curctx).nmps;
		}

		return d;
	}

	private void opj_mqc_renormd(opj_mqc_t mqc) {
		do {
			if (mqc.ct == 0) {
				opj_mqc_bytein(mqc);
			}
			mqc.a <<= 1;
			mqc.c <<= 1;
			mqc.ct--;
		} while (mqc.a < 0x8000);
	}

	private int opj_mqc_lpsexchange(opj_mqc_t mqc) {
		int d;
		if (mqc.a < (mqc.curctx).qeval) {
			mqc.a = (mqc.curctx).qeval;
			d = (mqc.curctx).mps;
			mqc.curctx = (mqc.curctx).nmps;
		} else {
			mqc.a = (mqc.curctx).qeval;
			d = (1 - (mqc.curctx).mps);
			mqc.curctx = (mqc.curctx).nlps;
		}

		return d;
	}

	/**
	 * Set the current context used for coding/decoding
	 * 
	 * @param mqc
	 *            MQC handle
	 * @param ctxno
	 *            Number that identifies the context
	 */
	private void opj_mqc_setcurctx(opj_mqc_t mqc, int ctxno) {
		mqc.curctx = mqc.ctxs[ctxno];
	}

	private int MACRO_t1_flags(opj_t1_t t1, int x, int y) {
		return t1.flags[((x) * (t1.flags_stride)) + (y)];
	}

	private void opj_t1_dec_refpass_mqc(opj_t1_t t1, int bpno) {
		int one, poshalf, neghalf;
		int i, j, k;
		// OPJ_INT32 *data1 = t1->data;
		int data1_index = 0;
		// opj_flag_t *flags1 = &t1->flags[1];
		int flags1_index = 1;
		one = 1 << bpno;
		poshalf = one >> 1;
		neghalf = bpno > 0 ? -poshalf : -1;
		for (k = 0; k < (t1.h & ~3); k += 4) {
			for (i = 0; i < t1.w; ++i) {
				int data2_index = data1_index + i;
				int flags2_index = flags1_index + i;
				flags2_index += t1.flags_stride;
				opj_t1_dec_refpass_step_mqc(t1, t1.flags, flags2_index,
						t1.data, data2_index, poshalf, neghalf);
				data2_index += t1.w;
				flags2_index += t1.flags_stride;
				opj_t1_dec_refpass_step_mqc(t1, t1.flags, flags2_index,
						t1.data, data2_index, poshalf, neghalf);
				data2_index += t1.w;
				flags2_index += t1.flags_stride;
				opj_t1_dec_refpass_step_mqc(t1, t1.flags, flags2_index,
						t1.data, data2_index, poshalf, neghalf);
				data2_index += t1.w;
				flags2_index += t1.flags_stride;
				opj_t1_dec_refpass_step_mqc(t1, t1.flags, flags2_index,
						t1.data, data2_index, poshalf, neghalf);
				data2_index += t1.w;
			}
			data1_index += t1.w << 2;
			flags1_index += t1.flags_stride << 2;
		}
		for (i = 0; i < t1.w; ++i) {
			int data2_index = data1_index + i;
			int flags2_index = flags1_index + i;
			for (j = k; j < t1.h; ++j) {
				flags2_index += t1.flags_stride;
				opj_t1_dec_refpass_step_mqc(t1, t1.flags, flags2_index,
						t1.data, data2_index, poshalf, neghalf);
				data2_index += t1.w;
			}
		}
	} /* VSC and BYPASS by Antonin */

	private void opj_t1_dec_refpass_step_mqc(opj_t1_t t1, short flagsp[],
			int flagsp_index, int datap[], int datap_index, int poshalf,
			int neghalf) {
		int v, t, flag;

		opj_mqc_t mqc = t1.mqc; /* MQC component */

		flag = flagsp[flagsp_index];
		if ((flag & (T1_SIG | T1_VISIT)) == T1_SIG) {
			opj_mqc_setcurctx(mqc, opj_t1_getctxno_mag(flag)); /* ESSAI */
			v = opj_mqc_decode(mqc);
			t = (v != 0) ? poshalf : neghalf;
			datap[datap_index] += datap[datap_index] < 0 ? -t : t;
			flagsp[flagsp_index] |= T1_REFINE;
		}
	} /* VSC and BYPASS by Antonin */

	private int opj_t1_getctxno_mag(int f) {
		int tmp1 = ((f & T1_SIG_OTH) != 0) ? T1_CTXNO_MAG + 1 : T1_CTXNO_MAG;
		int tmp2 = ((f & T1_REFINE) != 0) ? T1_CTXNO_MAG + 2 : tmp1;
		return (tmp2);
	}

	private void opj_t1_dec_refpass_mqc_vsc(opj_t1_t t1, int bpno) {
		int one, poshalf, neghalf;
		int i, j, k;
		int vsc;
		one = 1 << bpno;
		poshalf = one >> 1;
		neghalf = bpno > 0 ? -poshalf : -1;
		for (k = 0; k < t1.h; k += 4) {
			for (i = 0; i < t1.w; ++i) {
				for (j = k; j < k + 4 && j < t1.h; ++j) {
					vsc = ((j == k + 3 || j == t1.h - 1)) ? 1 : 0;
					opj_t1_dec_refpass_step_mqc_vsc(t1, t1.flags,
							((j + 1) * t1.flags_stride) + i + 1, t1.data,
							(j * t1.w) + i, poshalf, neghalf, vsc);
				}
			}
		}
	} /* VSC and BYPASS by Antonin */

	private void opj_t1_dec_refpass_step_mqc_vsc(opj_t1_t t1, short flagsp[],
			int flagsp_index, int datap[], int datap_index, int poshalf,
			int neghalf, int vsc) {
		int v, t, flag;

		opj_mqc_t mqc = t1.mqc; /* MQC component */

		flag = (vsc != 0) ? ((flagsp[flagsp_index]) & (~(T1_SIG_S | T1_SIG_SE
				| T1_SIG_SW | T1_SGN_S))) : (flagsp[flagsp_index]);
		if ((flag & (T1_SIG | T1_VISIT)) == T1_SIG) {
			opj_mqc_setcurctx(mqc, opj_t1_getctxno_mag(flag)); /* ESSAI */
			v = opj_mqc_decode(mqc);
			t = (v != 0) ? poshalf : neghalf;
			datap[datap_index] += datap[datap_index] < 0 ? -t : t;
			flagsp[flagsp_index] |= T1_REFINE;
		}
	} /* VSC and BYPASS by Antonin */

	private void opj_t1_dec_refpass_raw(opj_t1_t t1, int bpno, int cblksty) {
		int one, poshalf, neghalf;
		int i, j, k;
		int vsc;
		one = 1 << bpno;
		poshalf = one >> 1;
		neghalf = bpno > 0 ? -poshalf : -1;
		for (k = 0; k < t1.h; k += 4) {
			for (i = 0; i < t1.w; ++i) {
				for (j = k; j < k + 4 && j < t1.h; ++j) {
					vsc = (((cblksty & J2K_CCP_CBLKSTY_VSC) != 0) && (j == k + 3 || j == t1.h - 1)) ? 1
							: 0;
					opj_t1_dec_refpass_step_raw(t1, t1.flags,
							((j + 1) * t1.flags_stride) + i + 1, t1.data,
							(j * t1.w) + i, poshalf, neghalf, vsc);
				}
			}
		}
	} /* VSC and BYPASS by Antonin */

	private void opj_t1_dec_refpass_step_raw(opj_t1_t t1, short flagsp[],
			int flagsp_index, int datap[], int datap_index, int poshalf,
			int neghalf, int vsc) {
		int v, t, flag;

		opj_raw_t raw = t1.raw; /* RAW component */

		flag = (vsc != 0) ? ((flagsp[flagsp_index]) & (~(T1_SIG_S | T1_SIG_SE
				| T1_SIG_SW | T1_SGN_S))) : (flagsp[flagsp_index]);
		if ((flag & (T1_SIG | T1_VISIT)) == T1_SIG) {
			v = opj_raw_decode(raw);
			t = (v != 0) ? poshalf : neghalf;
			datap[datap_index] += datap[datap_index] < 0 ? -t : t;
			flagsp[flagsp_index] |= T1_REFINE;
		}
	} /* VSC and BYPASS by Antonin */

	int opj_raw_decode(opj_raw_t raw) {
		int d;
		if (raw.ct == 0) {
			raw.ct = 8;
			if (raw.len == raw.lenmax) {
				raw.c = (byte) 0xff;
			} else {
				if (raw.c == 0xff) {
					raw.ct = 7;
				}
				raw.c = raw.buffer[raw.len];
				raw.len++;
			}
		}
		raw.ct--;
		d = (raw.c >> raw.ct) & 0x01;

		return d;
	}

	private void opj_t1_dec_sigpass_mqc(opj_t1_t t1, int bpno, int orient) {
		int one, half, oneplushalf;
		int i, j, k;
		// OPJ_INT32 *data1 = t1->data;
		int data1_index = 0;
		// opj_flag_t *flags1 = &t1->flags[1];
		int flags1_index = 1;
		one = 1 << bpno;
		half = one >> 1;
		oneplushalf = one | half;
		for (k = 0; k < (t1.h & ~3); k += 4) {
			for (i = 0; i < t1.w; ++i) {
				int data2_index = data1_index + i;
				int flags2_index = flags1_index + i;
				flags2_index += t1.flags_stride;
				opj_t1_dec_sigpass_step_mqc(t1, t1.flags, flags2_index,
						t1.data, data2_index, orient, oneplushalf);
				data2_index += t1.w;
				flags2_index += t1.flags_stride;
				opj_t1_dec_sigpass_step_mqc(t1, t1.flags, flags2_index,
						t1.data, data2_index, orient, oneplushalf);
				data2_index += t1.w;
				flags2_index += t1.flags_stride;
				opj_t1_dec_sigpass_step_mqc(t1, t1.flags, flags2_index,
						t1.data, data2_index, orient, oneplushalf);
				data2_index += t1.w;
				flags2_index += t1.flags_stride;
				opj_t1_dec_sigpass_step_mqc(t1, t1.flags, flags2_index,
						t1.data, data2_index, orient, oneplushalf);
				data2_index += t1.w;
			}
			data1_index += t1.w << 2;
			flags1_index += t1.flags_stride << 2;
		}
		for (i = 0; i < t1.w; ++i) {
			int data2_index = data1_index + i;
			int flags2_index = flags1_index + i;
			for (j = k; j < t1.h; ++j) {
				flags2_index += t1.flags_stride;
				opj_t1_dec_sigpass_step_mqc(t1, t1.flags, flags2_index,
						t1.data, data2_index, orient, oneplushalf);
				data2_index += t1.w;
			}
		}
	} /* VSC and BYPASS by Antonin */

	private void opj_t1_dec_sigpass_step_mqc(opj_t1_t t1, short flagsp[],
			int flagsp_index, int datap[], int datap_index, int orient,
			int oneplushalf) {
		int v, flag;

		opj_mqc_t mqc = t1.mqc; /* MQC component */

		flag = flagsp[flagsp_index];
		if (((flag & T1_SIG_OTH) != 0) && ((flag & (T1_SIG | T1_VISIT)) == 0)) {
			opj_mqc_setcurctx(mqc, opj_t1_getctxno_zc(flag, orient));
			if (opj_mqc_decode(mqc) != 0) {
				opj_mqc_setcurctx(mqc, opj_t1_getctxno_sc(flag));
				v = opj_mqc_decode(mqc) ^ opj_t1_getspb(flag);
				datap[datap_index] = (v != 0) ? -oneplushalf : oneplushalf;
				opj_t1_updateflags(flagsp, flagsp_index, v, t1.flags_stride);
			}
			flagsp[flagsp_index] |= T1_VISIT;
		}
	} /* VSC and BYPASS by Antonin */

	private void opj_t1_dec_sigpass_mqc_vsc(opj_t1_t t1, int bpno, int orient) {
		int one, half, oneplushalf, vsc;
		int i, j, k;
		one = 1 << bpno;
		half = one >> 1;
		oneplushalf = one | half;
		for (k = 0; k < t1.h; k += 4) {
			for (i = 0; i < t1.w; ++i) {
				for (j = k; j < k + 4 && j < t1.h; ++j) {
					vsc = (j == k + 3 || j == t1.h - 1) ? 1 : 0;
					opj_t1_dec_sigpass_step_mqc_vsc(t1, t1.flags,
							((j + 1) * t1.flags_stride) + i + 1, t1.data,
							(j * t1.w) + i, orient, oneplushalf, vsc);
				}
			}
		}
	} /* VSC and BYPASS by Antonin */

	private void opj_t1_dec_sigpass_step_mqc_vsc(opj_t1_t t1, short flagsp[],
			int flagsp_index, int datap[], int datap_index, int orient,
			int oneplushalf, int vsc) {
		int v, flag;

		opj_mqc_t mqc = t1.mqc; /* MQC component */

		flag = (vsc != 0) ? ((flagsp[flagsp_index]) & (~(T1_SIG_S | T1_SIG_SE
				| T1_SIG_SW | T1_SGN_S))) : (flagsp[flagsp_index]);
		if (((flag & T1_SIG_OTH) != 0) && ((flag & (T1_SIG | T1_VISIT)) == 0)) {
			opj_mqc_setcurctx(mqc, opj_t1_getctxno_zc(flag, orient));
			if (opj_mqc_decode(mqc) != 0) {
				opj_mqc_setcurctx(mqc, opj_t1_getctxno_sc(flag));
				v = opj_mqc_decode(mqc) ^ opj_t1_getspb(flag);
				datap[datap_index] = (v != 0) ? -oneplushalf : oneplushalf;
				opj_t1_updateflags(flagsp, flagsp_index, v, t1.flags_stride);
			}
			flagsp[flagsp_index] |= T1_VISIT;
		}
	} /* VSC and BYPASS by Antonin */

	private void opj_t1_dec_sigpass_raw(opj_t1_t t1, int bpno, int orient,
			int cblksty) {
		int one, half, oneplushalf, vsc;
		int i, j, k;
		one = 1 << bpno;
		half = one >> 1;
		oneplushalf = one | half;
		for (k = 0; k < t1.h; k += 4) {
			for (i = 0; i < t1.w; ++i) {
				for (j = k; j < k + 4 && j < t1.h; ++j) {
					vsc = (((cblksty & J2K_CCP_CBLKSTY_VSC) != 0) && (j == k + 3 || j == t1.h - 1)) ? 1
							: 0;
					opj_t1_dec_sigpass_step_raw(t1, t1.flags,
							((j + 1) * t1.flags_stride) + i + 1, t1.data,
							(j * t1.w) + i, orient, oneplushalf, vsc);
				}
			}
		}
	}

	void opj_t1_dec_sigpass_step_raw(opj_t1_t t1, short flagsp[],
			int flagsp_index, int datap[], int datap_index, int orient,
			int oneplushalf, int vsc) {
		int v, flag;
		opj_raw_t raw = t1.raw; /* RAW component */
		// OPJ_ARG_NOT_USED(orient);

		flag = (vsc != 0) ? ((flagsp[flagsp_index]) & (~(T1_SIG_S | T1_SIG_SE
				| T1_SIG_SW | T1_SGN_S))) : (flagsp[flagsp_index]);
		if (((flag & T1_SIG_OTH) != 0) && ((flag & (T1_SIG | T1_VISIT)) == 0)) {
			if (opj_raw_decode(raw) != 0) {
				v = opj_raw_decode(raw); /* ESSAI */
				datap[datap_index] = (v != 0) ? -oneplushalf : oneplushalf;
				opj_t1_updateflags(flagsp, flagsp_index, v, t1.flags_stride);
			}
			flagsp[flagsp_index] |= T1_VISIT;
		}
	}

	private boolean opj_t1_allocate_buffers(opj_t1_t t1, int w, int h) {
		int datasize = w * h;
		int flagssize;

		/* encoder uses tile buffer, so no need to allocate */
		if (!t1.encoder) {
			if (datasize > t1.datasize) {
				t1.data = null;
				t1.data = new int[datasize];
				if (t1.data == null) {
					/* FIXME event manager error callback */
					return false;
				}
				t1.datasize = datasize;
			}
		}
		t1.flags_stride = w + 2;
		flagssize = t1.flags_stride * (h + 2);

		if (flagssize > t1.flagssize) {
			t1.flags = null;
			t1.flags = new short[flagssize];
			if (t1.flags == null) {
				/* FIXME event manager error callback */
				return false;
			}
			t1.flagssize = flagssize;
		}

		t1.w = w;
		t1.h = h;

		return true;
	}

	private void opj_mqc_resetstates(opj_mqc_t mqc) {
		int i;
		for (i = 0; i < MQC_NUMCTXS; i++) {
			mqc.ctxs[i] = mqc_states[0];
		}
	}

	private void opj_mqc_setstate(opj_mqc_t mqc, int ctxno, int msb, int prob) {
		mqc.ctxs[ctxno] = mqc_states[msb + (prob << 1)];
	}

	private boolean opj_mqc_init_dec(opj_mqc_t mqc, byte buff[], int start,
			int len) {
		opj_mqc_setcurctx(mqc, 0);
		mqc.buff = buff;
		mqc.start = start;
		mqc.end = start + len;
		mqc.bp = start;
		int bp = start;
		int i;
		if (len == 0)
			mqc.c = 0xff << 16;
		else
			mqc.c = (mqc.buff[mqc.bp] << 16);

		if (MQC_PERF_OPT) /* TODO_MSD: check this option and put in experimental */
		{
			int c;
			int ip[];
			// OPJ_BYTE *end = mqc->end - 1;
			int end = mqc.end - 1;
			// void* new_buffer = opj_realloc(mqc->buffer, (len + 1) *
			// sizeof(OPJ_UINT32));
			byte new_buffer[] = new byte[4 * (len + 1)];
			for (i = 0; i < mqc.buffer.length; i++) {
				new_buffer[i] = mqc.buffer[i];
			}

			mqc.buffer = new_buffer;

			// ip = (OPJ_UINT32 *) mqc->buffer;
			ip = new int[len + 1];
			int ip_index = 0;

			while (bp < end) {
				c = buff[bp + 1];
				if (buff[bp] == 0xff) {
					if (c > 0x8f) {
						break;
					} else {
						ip[ip_index] = 0x00000017 | (c << 9);
					}
				} else {
					ip[ip_index] = 0x00000018 | (c << 8);
				}
				bp++;
				ip_index++;
			}

			/* Handle last byte of data */
			c = 0xff;
			if (buff[bp] == 0xff) {
				ip[ip_index] = 0x0000ff18;
			} else {
				bp++;
				ip[ip_index] = 0x00000018 | (c << 8);
			}
			ip_index++;

			ip[ip_index] = 0x0000ff08;
			for (i = 0; i <= ip_index; i++) {
				mqc.buffer[4 * i] = (byte) (ip[i] & 0xff);
				mqc.buffer[4 * i + 1] = (byte) (ip[i] >>> 8);
				mqc.buffer[4 * i + 2] = (byte) (ip[i] >>> 16);
				mqc.buffer[4 * i + 3] = (byte) (ip[i] >>> 24);
			}
			mqc.buff = mqc.buffer;
			mqc.bp = 0;

		} // if (MQC_PERF_OPT)
		opj_mqc_bytein(mqc);
		mqc.c <<= 7;
		mqc.ct -= 7;
		mqc.a = 0x8000;
		return true;
	}

	private void opj_mqc_bytein(opj_mqc_t mqc) {
		if (MQC_PERF_OPT) {
			int i = mqc.bp;
			mqc.c += i & 0xffff00;
			mqc.ct = i & 0x0f;
			mqc.buff[mqc.bp] += (i >> 2) & 0x04;
		} else {
			if (mqc.bp != mqc.end) {
				int c;
				if (mqc.bp + 1 != mqc.end) {
					c = mqc.buff[mqc.bp + 1];
				} else {
					c = 0xff;
				}
				if (mqc.buff[mqc.bp] == 0xff) {
					if (c > 0x8f) {
						mqc.c += 0xff00;
						mqc.ct = 8;
					} else {
						mqc.bp++;
						mqc.c += c << 9;
						mqc.ct = 7;
					}
				} else {
					mqc.bp++;
					mqc.c += c << 8;
					mqc.ct = 8;
				}
			} else {
				mqc.c += 0xff00;
				mqc.ct = 8;
			}
		}
	}

	private void opj_raw_init_dec(opj_raw_t raw, byte buffer[], int start,
			int len) {
		raw.buffer = buffer;
		raw.start = start;
		raw.lenmax = len;
		raw.len = 0;
		raw.c = 0;
		raw.ct = 0;
	}

	/**
	 * Destroys a previously created T1 handle
	 * 
	 * @param p_t1
	 *            Tier 1 handle to destroy
	 */
	private void opj_t1_destroy(opj_t1_t p_t1) {
		if (p_t1 == null) {
			return;
		}

		/* destroy MQC and RAW handles */
		opj_mqc_destroy(p_t1.mqc);
		p_t1.mqc = null;
		p_t1.raw = null;

		/* encoder uses tile buffer, so no need to free */
		if (!p_t1.encoder && (p_t1.data != null)) {
			p_t1.data = null;
		}

		if (p_t1.flags != null) {
			p_t1.flags = null;
		}

		p_t1 = null;
	}

	private void opj_mqc_destroy(opj_mqc_t mqc) {
		if (mqc != null) {
			if (MQC_PERF_OPT) {
				if (mqc.buffer != null) {
					mqc.buffer = null;
				}
			}
			mqc = null;
		}
	}

	/* <summary> */
	/* Get gain of 5-3 wavelet transform. */
	/* </summary> */
	private int opj_dwt_getgain(int orient) {
		if (orient == 0)
			return 0;
		if (orient == 1 || orient == 2)
			return 1;
		return 2;
	}

	/* <summary> */
	/* Get gain of 9-7 wavelet transform. */
	/* </summary> */
	private int opj_dwt_getgain_real(int orient) {
		// (void)orient;
		return 0;
	}

	private boolean opj_j2k_set_decode_area(opj_j2k_t p_j2k,
			opj_image_t p_image, int p_start_x, int p_start_y, int p_end_x,
			int p_end_y) {
		opj_cp_t l_cp = p_j2k.m_cp;
		opj_image_t l_image = p_j2k.m_private_image;

		int it_comp;
		int l_comp_x1, l_comp_y1;
		opj_image_comp_t l_img_comp = null;

		/* Check if we are read the main header */
		if (p_j2k.m_decoder.m_state != J2K_STATE_TPHSOT) { /*
															 * FIXME
															 * J2K_DEC_STATE_TPHSOT
															 * )
															 */
			MipavUtil
					.displayError("Need to decode the main header before begin to decode the remaining codestream");
			return false;
		}

		if ((p_start_x == 0) && (p_start_y == 0) && (p_end_x == 0)
				&& (p_end_y == 0)) {
			Preferences
					.debug("No decoded area parameters, set the decoded area to the whole image\n",
							Preferences.DEBUG_FILEIO);

			p_j2k.m_decoder.m_start_tile_x = 0;
			p_j2k.m_decoder.m_start_tile_y = 0;
			p_j2k.m_decoder.m_end_tile_x = l_cp.tw;
			p_j2k.m_decoder.m_end_tile_y = l_cp.th;

			return true;
		}

		/* ----- */
		/* Check if the positions provided by the user are correct */

		/* Left */
		if (p_start_x < 0) {
			MipavUtil.displayError("p_start_x = " + p_start_x
					+ " < 0 in opj_j2k_set_decode_area");
			return false;
		}
		if (p_start_y < 0) {
			MipavUtil.displayError("p_start_y = " + p_start_y
					+ " < 0 in opj_j2k_set_decode_area");
			return false;
		}

		if (p_start_x > l_image.x1) {
			MipavUtil
					.displayError("Left position of the decoded area (region_x0= "
							+ p_start_x
							+ " ) is outside the image area (Xsiz= "
							+ l_image.x1);
			return false;
		} else if (p_start_x < l_image.x0) {
			Preferences.debug("Left position of the decoded area (region_x0="
					+ p_start_x + ")\n", Preferences.DEBUG_FILEIO);
			Preferences.debug("is outside the image area (XOsiz=" + l_image.x0
					+ ").\n", Preferences.DEBUG_FILEIO);
			p_j2k.m_decoder.m_start_tile_x = 0;
			p_image.x0 = l_image.x0;
		} else {
			p_j2k.m_decoder.m_start_tile_x = (p_start_x - l_cp.tx0) / l_cp.tdx;
			p_image.x0 = p_start_x;
		}

		/* Up */
		if (p_start_y > l_image.y1) {
			MipavUtil
					.displayError("Up position of the decoded area (region_y0="
							+ p_start_y + ") is outside the image area (Ysiz="
							+ l_image.y1 + ").");
			return false;
		} else if (p_start_y < l_image.y0) {
			Preferences.debug("Up position of the decoded area (region_y0= "
					+ p_start_y + ")\n", Preferences.DEBUG_FILEIO);
			Preferences.debug("is outside the image area (YOsiz=" + l_image.y0
					+ ").\n", Preferences.DEBUG_FILEIO);
			p_j2k.m_decoder.m_start_tile_y = 0;
			p_image.y0 = l_image.y0;
		} else {
			p_j2k.m_decoder.m_start_tile_y = (p_start_y - l_cp.ty0) / l_cp.tdy;
			p_image.y0 = p_start_y;
		}

		/* Right */
		if (p_end_x <= 0) {
			MipavUtil.displayError("p_end_x = " + p_end_x
					+ " <= 0 in opj_j2k_set_decode_area");
			return false;
		}
		if (p_end_y <= 0) {
			MipavUtil.displayError("p_end_y = " + p_end_y
					+ " <= 0 in opj_j2k_set_decode_area");
			return false;
		}

		if (p_end_x < l_image.x0) {
			MipavUtil
					.displayError("Right position of the decoded area (region_x1= "
							+ p_end_x
							+ ") is outside the image area (XOsiz="
							+ l_image.x0 + ").");
			return false;
		} else if (p_end_x > l_image.x1) {
			Preferences.debug("Right position of the decoded area (region_x1="
					+ p_end_x + ")\n", Preferences.DEBUG_FILEIO);
			Preferences.debug("is outside the image area (Xsiz=" + l_image.x1
					+ ").\n", Preferences.DEBUG_FILEIO);
			p_j2k.m_decoder.m_end_tile_x = l_cp.tw;
			p_image.x1 = l_image.x1;
		} else {
			p_j2k.m_decoder.m_end_tile_x = (p_end_x - l_cp.tx0 + l_cp.tdx - 1)
					/ l_cp.tdx;
			p_image.x1 = p_end_x;
		}

		/* Bottom */
		if (p_end_y < l_image.y0) {
			MipavUtil
					.displayError("Bottom position of the decoded area (region_y1="
							+ p_end_y
							+ ") is outside the image area (YOsiz="
							+ l_image.y0 + ").");
			return false;
		}
		if (p_end_y > l_image.y1) {
			Preferences.debug("Bottom position of the decoded area (region_y1="
					+ p_end_y + ")\n", Preferences.DEBUG_FILEIO);
			Preferences.debug("is outside the image area (Ysiz=" + l_image.y1
					+ ").\n", Preferences.DEBUG_FILEIO);
			p_j2k.m_decoder.m_end_tile_y = l_cp.th;
			p_image.y1 = l_image.y1;
		} else {
			p_j2k.m_decoder.m_end_tile_y = (p_end_y - l_cp.ty0 + l_cp.tdy - 1)
					/ l_cp.tdy;
			p_image.y1 = p_end_y;
		}
		/* ----- */

		p_j2k.m_decoder.m_discard_tiles = true;

		l_img_comp = p_image.comps[0];
		for (it_comp = 0; it_comp < p_image.numcomps; ++it_comp) {
			int l_h, l_w;

			l_img_comp.x0 = (p_image.x0 + l_img_comp.dx - 1) / l_img_comp.dx;
			l_img_comp.y0 = (p_image.y0 + l_img_comp.dy - 1) / l_img_comp.dy;
			l_comp_x1 = (p_image.x1 + l_img_comp.dx - 1) / l_img_comp.dx;
			l_comp_y1 = (p_image.y1 + l_img_comp.dy - 1) / l_img_comp.dy;

			l_w = (int) ((l_comp_x1 + (1L << l_img_comp.factor) - 1) >>> l_img_comp.factor)
					- (int) ((l_img_comp.x0 + (1L << l_img_comp.factor) - 1) >>> l_img_comp.factor);
			if (l_w < 0) {
				MipavUtil
						.displayError("Size x of the decoded component image is incorrect (comp["
								+ it_comp + "].w=" + l_w);
				return false;
			}
			l_img_comp.w = l_w;

			l_h = (int) ((l_comp_y1 + (1L << l_img_comp.factor) - 1) >>> l_img_comp.factor)
					- (int) ((l_img_comp.y0 + (1L << l_img_comp.factor) - 1) >>> l_img_comp.factor);
			if (l_h < 0) {
				MipavUtil
						.displayError("Size y of the decoded component image is incorrect (comp["
								+ it_comp + "].h=" + l_h);
				return false;
			}
			l_img_comp.h = l_h;

			if (it_comp < p_image.numcomps - 1) {
				l_img_comp = p_image.comps[it_comp + 1];
			}
		}

		Preferences.debug("Setting decoding area to " + p_image.x0 + ","
				+ p_image.y0 + "," + p_image.x1 + "," + p_image.y1 + "\n",
				Preferences.DEBUG_FILEIO);

		return true;
	}

	private boolean opj_j2k_allocate_tile_element_cstr_index(opj_j2k_t p_j2k) {
		int it_tile = 0;
		int i;

		p_j2k.cstr_index.nb_of_tiles = p_j2k.m_cp.tw * p_j2k.m_cp.th;
		p_j2k.cstr_index.tile_index = new opj_tile_index_t[p_j2k.cstr_index.nb_of_tiles];
		for (i = 0; i < p_j2k.cstr_index.nb_of_tiles; i++) {
			p_j2k.cstr_index.tile_index[i] = new opj_tile_index_t();
		}

		for (it_tile = 0; it_tile < p_j2k.cstr_index.nb_of_tiles; it_tile++) {
			p_j2k.cstr_index.tile_index[it_tile].maxmarknum = 100;
			p_j2k.cstr_index.tile_index[it_tile].marknum = 0;
			p_j2k.cstr_index.tile_index[it_tile].marker = new opj_marker_info_t[p_j2k.cstr_index.tile_index[it_tile].maxmarknum];
			for (i = 0; i < p_j2k.cstr_index.tile_index[it_tile].maxmarknum; i++) {
				p_j2k.cstr_index.tile_index[it_tile].marker[i] = new opj_marker_info_t();
			}
		}

		return true;
	}

	/**
	 * Copy only header of image and its component header (no data are copied)
	 * if dest image have data, they will be freed
	 * 
	 * @param p_image_src
	 *            the src image
	 * @param p_image_dest
	 *            the dest image
	 * 
	 */
	private void opj_copy_image_header(final opj_image_t p_image_src,
			opj_image_t p_image_dest) {
		int compno;
		int i;

		/* preconditions */
		if (p_image_src == null) {
			MipavUtil
					.displayError("p_image_src == null at opj_copy_image_header entry");
			return;
		}
		if (p_image_dest == null) {
			MipavUtil
					.displayError("p_image_dest == null at opj_copy_image_header entry");
			return;
		}

		p_image_dest.x0 = p_image_src.x0;
		p_image_dest.y0 = p_image_src.y0;
		p_image_dest.x1 = p_image_src.x1;
		p_image_dest.y1 = p_image_src.y1;

		if (p_image_dest.comps != null) {
			for (compno = 0; compno < p_image_dest.numcomps; compno++) {
				opj_image_comp_t image_comp = p_image_dest.comps[compno];
				if (image_comp.data != null) {
					image_comp.data = null;
				}
			}
			p_image_dest.comps = null;
		}

		p_image_dest.numcomps = p_image_src.numcomps;

		p_image_dest.comps = new opj_image_comp_t[p_image_dest.numcomps];
		for (i = 0; i < p_image_dest.numcomps; i++) {
			p_image_dest.comps[i] = new opj_image_comp_t();
		}

		for (compno = 0; compno < p_image_dest.numcomps; compno++) {
			p_image_dest.comps[compno].dx = p_image_src.comps[compno].dx;
			p_image_dest.comps[compno].dy = p_image_src.comps[compno].dy;
			p_image_dest.comps[compno].w = p_image_src.comps[compno].w;
			p_image_dest.comps[compno].h = p_image_src.comps[compno].h;
			p_image_dest.comps[compno].x0 = p_image_src.comps[compno].x0;
			p_image_dest.comps[compno].y0 = p_image_src.comps[compno].y0;
			p_image_dest.comps[compno].prec = p_image_src.comps[compno].prec;
			p_image_dest.comps[compno].bpp = p_image_src.comps[compno].bpp;
			p_image_dest.comps[compno].sgnd = p_image_src.comps[compno].sgnd;
			p_image_dest.comps[compno].resno_decoded = p_image_src.comps[compno].resno_decoded;
			p_image_dest.comps[compno].factor = p_image_src.comps[compno].factor;
			p_image_dest.comps[compno].alpha = p_image_src.comps[compno].alpha;
			p_image_dest.comps[compno].data = null;
		}

		p_image_dest.color_space = p_image_src.color_space;
		p_image_dest.icc_profile_len = p_image_src.icc_profile_len;

		if (p_image_dest.icc_profile_len != 0) {
			p_image_dest.icc_profile_buf = new char[p_image_dest.icc_profile_len];
			for (i = 0; i < p_image_src.icc_profile_len; i++) {
				p_image_dest.icc_profile_buf[i] = p_image_src.icc_profile_buf[i];
			}
		} else {
			p_image_dest.icc_profile_buf = null;
		}

		return;
	}

	private void opj_image_destroy(opj_image_t image) {
		if (image != null) {
			if (image.comps != null) {
				int compno;

				/* image components */
				for (compno = 0; compno < image.numcomps; compno++) {
					opj_image_comp_t image_comp = image.comps[compno];
					if (image_comp.data != null) {
						image_comp.data = null;
					}
				}
				image.comps = null;
			}

			if (image.icc_profile_buf != null) {
				image.icc_profile_buf = null;
			}

			image = null;
		}
	}

	private boolean opj_j2k_copy_default_tcp_and_create_tcd(opj_j2k_t p_j2k,
			RandomAccessFile p_stream) {
		opj_tcp_t l_tcp = null;
		opj_tcp_t l_default_tcp = null;
		int l_nb_tiles;
		int i, j, k;
		opj_tccp_t l_current_tccp[] = null;
		int l_tccp_size;
		int l_mct_size;
		opj_image_t l_image;
		int l_mcc_records_size, l_mct_records_size;

		/* preconditions */
		if (p_j2k == null) {
			MipavUtil
					.displayError("p_j2k null at entry to opj_j2k_copy_default_tcp_and_create_tcd");
			return false;
		}
		if (p_stream == null) {
			MipavUtil
					.displayError("p_stream null at entry to opj_j2k_copy_default_tcp_and_create_tcd");
			return false;
		}

		l_image = p_j2k.m_private_image;
		l_nb_tiles = p_j2k.m_cp.th * p_j2k.m_cp.tw;
		l_tcp = p_j2k.m_cp.tcps[0];
		l_tccp_size = l_image.numcomps;
		l_default_tcp = p_j2k.m_decoder.m_default_tcp;
		l_mct_size = l_image.numcomps * l_image.numcomps;

		/* For each tile */
		for (i = 0; i < l_nb_tiles; ++i) {
			/*
			 * keep the tile-compo coding parameters pointer of the current tile
			 * coding parameters
			 */
			l_current_tccp = l_tcp.tccps;
			/*
			 * Copy default coding parameters into the current tile coding
			 * parameters
			 */
			l_tcp.csty = l_default_tcp.csty;
			l_tcp.prg = l_default_tcp.prg;
			l_tcp.numlayers = l_default_tcp.numlayers;
			l_tcp.num_layers_to_decode = l_default_tcp.num_layers_to_decode;
			l_tcp.mct = l_default_tcp.mct;
			l_tcp.rates = new float[l_default_tcp.rates.length];
			for (j = 0; j < l_default_tcp.rates.length; j++) {
				l_tcp.rates[j] = l_default_tcp.rates[j];
			}
			l_tcp.numpocs = l_default_tcp.numpocs;
			l_tcp.pocs = new opj_poc_t[l_default_tcp.pocs.length];
			for (j = 0; j < l_default_tcp.pocs.length; j++) {
				l_tcp.pocs[j] = new opj_poc_t();
				l_tcp.pocs[j].resno0 = l_default_tcp.pocs[j].resno0;
				l_tcp.pocs[j].compno0 = l_default_tcp.pocs[j].compno0;
				l_tcp.pocs[j].layno1 = l_default_tcp.pocs[j].layno1;
				l_tcp.pocs[j].resno1 = l_default_tcp.pocs[j].resno1;
				l_tcp.pocs[j].compno1 = l_default_tcp.pocs[j].compno1;
				l_tcp.pocs[j].layno0 = l_default_tcp.pocs[j].layno0;
				l_tcp.pocs[j].precno0 = l_default_tcp.pocs[j].precno0;
				l_tcp.pocs[j].precno1 = l_default_tcp.pocs[j].precno1;
				l_tcp.pocs[j].prg1 = l_default_tcp.pocs[j].prg1;
				l_tcp.pocs[j].prg = l_default_tcp.pocs[j].prg;
				if (l_default_tcp.pocs[j].progorder != null) {
				    l_tcp.pocs[j].progorder = new String(l_default_tcp.pocs[j].progorder);
				}
				l_tcp.pocs[j].tile = l_default_tcp.pocs[j].tile;
				l_tcp.pocs[j].tx0 = l_default_tcp.pocs[j].tx0;
				l_tcp.pocs[j].tx1 = l_default_tcp.pocs[j].tx1;
				l_tcp.pocs[j].ty0 = l_default_tcp.pocs[j].ty0;
				l_tcp.pocs[j].ty1 = l_default_tcp.pocs[j].ty1;
				l_tcp.pocs[j].layS = l_default_tcp.pocs[j].layS;
				l_tcp.pocs[j].resS = l_default_tcp.pocs[j].resS;
				l_tcp.pocs[j].compS = l_default_tcp.pocs[j].compS;
				l_tcp.pocs[j].prcS = l_default_tcp.pocs[j].prcS;
				l_tcp.pocs[j].layE = l_default_tcp.pocs[j].layE;
				l_tcp.pocs[j].resE = l_default_tcp.pocs[j].resE;
				l_tcp.pocs[j].compE = l_default_tcp.pocs[j].compE;
				l_tcp.pocs[j].prcE = l_default_tcp.pocs[j].prcE;
				l_tcp.pocs[j].txS = l_default_tcp.pocs[j].txS;
				l_tcp.pocs[j].txE = l_default_tcp.pocs[j].txE;
				l_tcp.pocs[j].tyS = l_default_tcp.pocs[j].tyS;
				l_tcp.pocs[j].tyE = l_default_tcp.pocs[j].tyE;
				l_tcp.pocs[j].dx = l_default_tcp.pocs[j].dx;
				l_tcp.pocs[j].dy = l_default_tcp.pocs[j].dy;
				l_tcp.pocs[j].lay_t = l_default_tcp.pocs[j].lay_t;
				l_tcp.pocs[j].res_t = l_default_tcp.pocs[j].res_t;
				l_tcp.pocs[j].comp_t = l_default_tcp.pocs[j].comp_t;
				l_tcp.pocs[j].prc_t = l_default_tcp.pocs[j].prc_t;
				l_tcp.pocs[j].tx0_t = l_default_tcp.pocs[j].tx0_t;
				l_tcp.pocs[j].ty0_t = l_default_tcp.pocs[j].ty0_t;
			}
			l_tcp.ppt_markers_count = l_default_tcp.ppt_markers_count;
			l_tcp.ppt_markers = new opj_ppx[l_default_tcp.ppt_markers_count];
			for (j = 0; j < l_default_tcp.ppt_markers_count; j++) {
				l_tcp.ppt_markers[j] = new opj_ppx();
				l_tcp.ppt_markers[j].m_data = new byte[l_default_tcp.ppt_markers[j].m_data.length];
				for (k = 0; k < l_default_tcp.ppt_markers[j].m_data.length; k++) {
					l_tcp.ppt_markers[j].m_data[k] = l_default_tcp.ppt_markers[j].m_data[k];
				}
				l_tcp.ppt_markers[j].m_data_size = l_default_tcp.ppt_markers[j].m_data_size;
			}
			if (l_default_tcp.ppt_buffer != null) {
				l_tcp.ppt_buffer = new byte[l_default_tcp.ppt_buffer.length];
				for (j = 0; j < l_default_tcp.ppt_buffer.length; j++) {
					l_tcp.ppt_buffer[j] = l_default_tcp.ppt_buffer[j];
				}
			}
			l_tcp.ppt_len = l_tcp.ppt_len;
			l_tcp.distoratio = new float[l_default_tcp.distoratio.length];
			for (j = 0; j < l_default_tcp.distoratio.length; j++) {
				l_tcp.distoratio[j] = l_default_tcp.distoratio[j];
			}
			l_tcp.m_nb_tile_parts = l_default_tcp.m_nb_tile_parts;
			l_tcp.m_data = new byte[l_default_tcp.m_data_size];
			for (j = 0; j < l_default_tcp.m_data_size; j++) {
				l_tcp.m_data[j] = l_default_tcp.m_data[j];
			}
			l_tcp.m_data_size = l_default_tcp.m_data_size;
			if (l_default_tcp.mct_norms != null) {
				l_tcp.mct_norms = new double[l_default_tcp.mct_norms.length];
				for (j = 0; j < l_tcp.mct_norms.length; j++) {
					l_tcp.mct_norms[j] = l_default_tcp.mct_norms[j];
				}
			}
			if (l_default_tcp.m_mct_coding_matrix != null) {
				l_tcp.m_mct_coding_matrix = new float[l_default_tcp.m_mct_coding_matrix.length];
				for (j = 0; j < l_default_tcp.m_mct_coding_matrix.length; j++) {
					l_tcp.m_mct_coding_matrix[j] = l_default_tcp.m_mct_coding_matrix[j];
				}
			}
			l_tcp.POC = l_default_tcp.POC;
			/* Initialize some values of the current tile coding parameters */
			l_tcp.cod = 0;
			l_tcp.ppt = 0;
			l_tcp.ppt_data = null;
			l_tcp.ppt_data_size = 0;
			/*
			 * Remove memory not owned by this tile in case of early error
			 * return.
			 */
			l_tcp.m_mct_decoding_matrix = null;
			l_tcp.m_nb_mct_records = 0;
			l_tcp.m_nb_max_mct_records = 0;
			l_tcp.m_mct_records = null;
			l_tcp.m_nb_mcc_records = 0;
			l_tcp.m_nb_max_mcc_records = 0;
			l_tcp.m_mcc_records = null;
			/*
			 * Reconnect the tile-compo coding parameters pointer to the current
			 * tile coding parameters
			 */
			l_tcp.tccps = l_current_tccp;

			/*
			 * Get the mct_decoding_matrix of the dflt_tile_cp and copy them
			 * into the current tile cp
			 */
			if (l_default_tcp.m_mct_decoding_matrix != null) {
				l_tcp.m_mct_decoding_matrix = new float[l_mct_size];
				for (j = 0; j < l_mct_size; j++) {
					l_tcp.m_mct_decoding_matrix[j] = l_default_tcp.m_mct_decoding_matrix[j];
				}
			}

			/*
			 * Get the mct_record of the dflt_tile_cp and copy them into the
			 * current tile cp
			 */
			l_mct_records_size = l_default_tcp.m_nb_max_mct_records;
			l_tcp.m_mct_records = new opj_mct_data_t[l_mct_records_size];

			for (j = 0; j < l_mct_records_size; j++) {
				l_tcp.m_mct_records[j] = new opj_mct_data_t();
				if (l_default_tcp.m_mct_records[j] != null) {
					l_tcp.m_mct_records[j].m_element_type = l_default_tcp.m_mct_records[j].m_element_type;
					l_tcp.m_mct_records[j].m_array_type = l_default_tcp.m_mct_records[j].m_array_type;
					l_tcp.m_mct_records[j].m_index = l_default_tcp.m_mct_records[j].m_index;
					l_tcp.m_mct_records[j].m_data = new byte[l_default_tcp.m_mct_records[j].m_data_size];
					for (k = 0; k < l_default_tcp.m_mct_records[j].m_data_size; k++) {
						l_tcp.m_mct_records[j].m_data[k] = l_default_tcp.m_mct_records[j].m_data[k];
					}
					l_tcp.m_mct_records[j].m_data_size = l_default_tcp.m_mct_records[j].m_data_size;
				}
			}
			l_tcp.m_nb_max_mct_records = l_mct_records_size;

			/*
			 * Get the mcc_record of the dflt_tile_cp and copy them into the
			 * current tile cp
			 */
			l_mcc_records_size = l_default_tcp.m_nb_max_mcc_records;
			l_tcp.m_mcc_records = new opj_simple_mcc_decorrelation_data_t[l_mcc_records_size];
			for (j = 0; j < l_mcc_records_size; j++) {
				l_tcp.m_mcc_records[j] = new opj_simple_mcc_decorrelation_data_t();
				l_tcp.m_mcc_records[j].m_index = l_default_tcp.m_mcc_records[j].m_index;
				l_tcp.m_mcc_records[j].m_nb_comps = l_default_tcp.m_mcc_records[j].m_nb_comps;
				l_tcp.m_mcc_records[j].m_decorrelation_array = new opj_mct_data_t();
				if (l_default_tcp.m_mcc_records[j].m_decorrelation_array != null) {
					l_tcp.m_mcc_records[j].m_decorrelation_array.m_element_type = l_default_tcp.m_mcc_records[j].m_decorrelation_array.m_element_type;
					l_tcp.m_mcc_records[j].m_decorrelation_array.m_array_type = l_default_tcp.m_mcc_records[j].m_decorrelation_array.m_array_type;
					l_tcp.m_mcc_records[j].m_decorrelation_array.m_index = l_default_tcp.m_mcc_records[j].m_decorrelation_array.m_index;
					l_tcp.m_mcc_records[j].m_decorrelation_array.m_data = new byte[l_default_tcp.m_mcc_records[j].m_decorrelation_array.m_data.length];
					for (k = 0; k < l_default_tcp.m_mcc_records[j].m_decorrelation_array.m_data.length; k++) {
						l_tcp.m_mcc_records[j].m_decorrelation_array.m_data[k] = l_default_tcp.m_mcc_records[j].m_decorrelation_array.m_data[k];
					}
					l_tcp.m_mcc_records[j].m_decorrelation_array.m_data_size = l_default_tcp.m_mcc_records[j].m_decorrelation_array.m_data_size;
				} // if (l_default_tcp.m_mcc_records[j].m_decorrelation_array !=
					// null)

				l_tcp.m_mcc_records[j].m_offset_array = new opj_mct_data_t();
				if (l_default_tcp.m_mcc_records[j].m_offset_array != null) {
					l_tcp.m_mcc_records[j].m_offset_array.m_element_type = l_default_tcp.m_mcc_records[j].m_offset_array.m_element_type;
					l_tcp.m_mcc_records[j].m_offset_array.m_array_type = l_default_tcp.m_mcc_records[j].m_offset_array.m_array_type;
					l_tcp.m_mcc_records[j].m_offset_array.m_index = l_default_tcp.m_mcc_records[j].m_offset_array.m_index;
					l_tcp.m_mcc_records[j].m_offset_array.m_data = new byte[l_default_tcp.m_mcc_records[j].m_offset_array.m_data.length];
					for (k = 0; k < l_default_tcp.m_mcc_records[j].m_offset_array.m_data.length; k++) {
						l_tcp.m_mcc_records[j].m_offset_array.m_data[k] = l_default_tcp.m_mcc_records[j].m_offset_array.m_data[k];
					}
					l_tcp.m_mcc_records[j].m_offset_array.m_data_size = l_default_tcp.m_mcc_records[j].m_offset_array.m_data_size;
				} // if (l_default_tcp.m_mcc_records[j].m_offset_array != null)
				l_tcp.m_mcc_records[j].m_is_irreversible = l_default_tcp.m_mcc_records[j].m_is_irreversible;
			}

			l_tcp.m_nb_max_mcc_records = l_default_tcp.m_nb_max_mcc_records;

			/* Copy all the dflt_tile_compo_cp to the current tile cp */
			for (j = 0; j < l_tccp_size; j++) {
				l_current_tccp[j].csty = l_default_tcp.tccps[j].csty;
				l_current_tccp[j].numresolutions = l_default_tcp.tccps[j].numresolutions;
				l_current_tccp[j].cblkw = l_default_tcp.tccps[j].cblkw;
				l_current_tccp[j].cblkh = l_default_tcp.tccps[j].cblkh;
				l_current_tccp[j].cblksty = l_default_tcp.tccps[j].cblksty;
				l_current_tccp[j].qmfbid = l_default_tcp.tccps[j].qmfbid;
				l_current_tccp[j].qntsty = l_default_tcp.tccps[j].qntsty;
				l_current_tccp[j].stepsizes = new opj_stepsize_t[l_default_tcp.tccps[j].stepsizes.length];
				for (k = 0; k < l_default_tcp.tccps[j].stepsizes.length; k++) {
					l_current_tccp[j].stepsizes[k] = new opj_stepsize_t();
					l_current_tccp[j].stepsizes[k].expn = l_default_tcp.tccps[j].stepsizes[k].expn;
					l_current_tccp[j].stepsizes[k].mant = l_default_tcp.tccps[j].stepsizes[k].mant;
				}
				l_current_tccp[j].numgbits = l_default_tcp.tccps[j].numgbits;
				l_current_tccp[j].roishift = l_default_tcp.tccps[j].roishift;
				l_current_tccp[j].prcw = new int[l_default_tcp.tccps[j].prcw.length];
				for (k = 0; k < l_default_tcp.tccps[j].prcw.length; k++) {
					l_current_tccp[j].prcw[k] = l_default_tcp.tccps[j].prcw[k];
				}
				l_current_tccp[j].prch = new int[l_default_tcp.tccps[j].prch.length];
				for (k = 0; k < l_default_tcp.tccps[j].prch.length; k++) {
					l_current_tccp[j].prch[k] = l_default_tcp.tccps[j].prch[k];
				}
				l_current_tccp[j].m_dc_level_shift = l_default_tcp.tccps[j].m_dc_level_shift;
			}

			/* Move to next tile cp */
			if (i < l_nb_tiles - 1) {
				l_tcp = p_j2k.m_cp.tcps[i + 1];
			}
		} // for (i=0; i<l_nb_tiles; ++i)

		/* Create the current tile decoder */
		p_j2k.m_tcd = opj_tcd_create(true); /* FIXME why a cast ? */
		if (p_j2k.m_tcd == null) {
			return false;
		}

		if (!opj_tcd_init(p_j2k.m_tcd, l_image, p_j2k.m_cp)) {
			p_j2k.m_tcd = null;
			MipavUtil.displayError("Cannot init decode tile, memory error");
			return false;
		}

		return true;
	}

	private boolean opj_tcd_init(opj_tcd_t p_tcd, opj_image_t p_image,
			opj_cp_t p_cp) {
		int i;
		p_tcd.image = p_image;
		p_tcd.cp = p_cp;

		p_tcd.tcd_image.tiles = new opj_tcd_tile_t();
		if (p_tcd.tcd_image.tiles == null) {
			return false;
		}

		p_tcd.tcd_image.tiles.comps = new opj_tcd_tilecomp_t[p_image.numcomps];
		for (i = 0; i < p_image.numcomps; i++) {
			p_tcd.tcd_image.tiles.comps[i] = new opj_tcd_tilecomp_t();
		}

		p_tcd.tcd_image.tiles.numcomps = p_image.numcomps;
		if (p_cp.m_enc != null) {
		    p_tcd.tp_pos = p_cp.m_enc.m_tp_pos;
		}

		return true;
	}

	/**
	 * Create a new TCD handle
	 */
	private opj_tcd_t opj_tcd_create(boolean p_is_decoder) {
		opj_tcd_t l_tcd = null;

		/* create the tcd structure */
		l_tcd = new opj_tcd_t();

		l_tcd.m_is_decoder = p_is_decoder ? 1 : 0;

		l_tcd.tcd_image = new opj_tcd_image_t();

		return l_tcd;
	}

	private boolean opj_j2k_read_header_procedure(opj_j2k_t l_j2k,
			RandomAccessFile l_stream) {
		int i;
		int l_current_marker[] = new int[1];
		int l_marker_size[] = new int[1];
		opj_dec_memory_marker_handler_t l_marker_handler = null;
		boolean l_has_siz = false;
		boolean l_has_cod = false;
		boolean l_has_qcd = false;
		long filePointer = 0;

		/* We enter in the main header */
		l_j2k.m_decoder.m_state = J2K_STATE_MHSOC;

		/* Try to read the SOC marker, the codestream must begin with SOC marker */
		
		if (!opj_j2k_read_soc(l_j2k, l_stream)) {
			MipavUtil.displayError("Expected a SOC marker");
			return false;
		}

		/*
		 * Try to read 2 bytes (the next marker ID) from stream and copy them
		 * into the buffer
		 */
		int l_read_nb_bytes = 0;
		try {
			l_read_nb_bytes = l_stream
					.read(l_j2k.m_decoder.m_header_data, 0, 2);
		} catch (IOException e) {
			MipavUtil.displayError("IOException " + e + " on l_stream.read");
			return false;
		}
		if (l_read_nb_bytes != 2) {
			MipavUtil
					.displayError("Could not read 2 bytes from RandomAccessFile");
			return false;
		}

		/* Read 2 bytes as the new marker ID */
		l_current_marker[0] = getBufferUShort(l_j2k.m_decoder.m_header_data, 0,
				endianess);
			

		/* Try to read until the SOT is detected */
		while (l_current_marker[0] != J2K_MS_SOT) {

			/* Check if the current marker ID is valid */
			if (l_current_marker[0] < 0xff00) {
				MipavUtil
						.displayError("A marker ID was expected (0xff--) instead of "
								+ hex(l_current_marker[0]));
				return false;
			}

			/* Get the marker handler from the marker ID */
			l_marker_handler = opj_j2k_get_marker_handler(l_current_marker[0]);

			/* Manage case where marker is unknown */
			if (l_marker_handler.id == J2K_MS_UNK) {
				
				if (!opj_j2k_read_unk(l_j2k, l_stream, l_current_marker)) {
					MipavUtil
							.displayError("Unknow marker has been detected and generated error.");
					return false;
				}

				if (l_current_marker[0] == J2K_MS_SOT)
					break; /*
							 * SOT marker is detected main header is completely
							 * read
							 */
				else
					/* Get the marker handler from the marker ID */
					l_marker_handler = opj_j2k_get_marker_handler(l_current_marker[0]);
			} // if (l_marker_handler.id == J2K_MS_UNK)

			if (l_marker_handler.id == J2K_MS_SIZ) {
				/* Mark required SIZ marker as found */
				l_has_siz = true;
			}
			if (l_marker_handler.id == J2K_MS_COD) {
				/* Mark required COD marker as found */
				l_has_cod = true;
			}
			if (l_marker_handler.id == J2K_MS_QCD) {
				/* Mark required QCD marker as found */
				l_has_qcd = true;
			}

			/*
			 * Check if the marker is known and if it is the right place to find
			 * it
			 */
			if ((l_j2k.m_decoder.m_state & l_marker_handler.states) == 0) {
				MipavUtil
						.displayError("Marker is not compliant with its position");
				return false;
			}

			/*
			 * Try to read 2 bytes (the marker size) from stream and copy them
			 * into the buffer
			 */
			try {
				l_read_nb_bytes = l_stream.read(l_j2k.m_decoder.m_header_data,
						0, 2);
			} catch (IOException e) {
				MipavUtil
						.displayError("IOException " + e + " on l_stream.read");
				return false;
			}
			if (l_read_nb_bytes != 2) {
				MipavUtil.displayError("RandomAccessFile too short");
				return false;
			}

			/* read 2 bytes as the marker size */
			l_marker_size[0] = getBufferUShort(l_j2k.m_decoder.m_header_data,
					0, endianess);
			l_marker_size[0] -= 2; /*
									 * Subtract the size of the marker ID
									 * already read
									 */

			/* Check if the marker size is compatible with the header data size */
			if (l_marker_size[0] > l_j2k.m_decoder.m_header_data_size) {
				byte new_header_data[] = new byte[l_marker_size[0]];
				for (i = 0; i < l_j2k.m_decoder.m_header_data_size; i++) {
					new_header_data[i] = l_j2k.m_decoder.m_header_data[i];
				}
				l_j2k.m_decoder.m_header_data = new_header_data;
				l_j2k.m_decoder.m_header_data_size = l_marker_size[0];
			} // if (l_marker_size[0] > l_j2k.m_decoder.m_header_data_size) 

			/*
			 * Try to read the rest of the marker segment from stream and copy
			 * them into the buffer
			 */
			try {
				l_read_nb_bytes = l_stream.read(l_j2k.m_decoder.m_header_data,
						0, l_marker_size[0]);
			} catch (IOException e) {
				MipavUtil
						.displayError("IOException " + e + " on l_stream.read");
				return false;
			}
			if (l_read_nb_bytes != l_marker_size[0]) {
				MipavUtil.displayError("RandomAccessFile too short");
				return false;
			}

			/* Read the marker segment with the correct marker handler */
			if (!readMarkerHandler(l_marker_handler.handler, l_j2k,
					l_j2k.m_decoder.m_header_data, l_marker_size[0])) {
				MipavUtil
						.displayError("Marker handler function failed to read the marker segment");
				return false;
			}

			/* Add the marker to the codestream index */
			try {
				filePointer = l_stream.getFilePointer();
			} catch (IOException e) {
				MipavUtil.displayError("IOException " + e
						+ " on l_stream.getFilePointer()");
				return false;
			}
			if (false == opj_j2k_add_mhmarker(l_j2k.cstr_index,
					l_marker_handler.id, filePointer - l_marker_size[0] - 4,
					l_marker_size[0] + 4)) {

				MipavUtil.displayError("Not enough memory to add mh marker");
				return false;
			}

			/*
			 * Try to read 2 bytes (the next marker ID) from stream and copy
			 * them into the buffer
			 */
			try {
				l_read_nb_bytes = l_stream.read(l_j2k.m_decoder.m_header_data,
						0, 2);
			} catch (IOException e) {
				MipavUtil
						.displayError("IOException " + e + " on l_stream.read");
				return false;
			}
			if (l_read_nb_bytes != 2) {
				MipavUtil.displayError("RandomAccessFile too short");
				return false;
			}

			/* read 2 bytes as the new marker ID */
			l_current_marker[0] = getBufferUShort(
					l_j2k.m_decoder.m_header_data, 0, endianess);
		} // while (l_current_marker[0] != J2K_MS_SOT)

		if (!l_has_siz) {
			MipavUtil
					.displayError("Required SIZ marker not found in main header");
			return false;
		}
		if (!l_has_cod) {
			MipavUtil
					.displayError("Required COD marker not found in main header");
			return false;
		}
		if (!l_has_qcd) {
			MipavUtil
					.displayError("Required QCD marker not found in main header");
			return false;
		}

		if (!opj_j2k_merge_ppm(l_j2k.m_cp)) {
			MipavUtil.displayError("Failed to merge PPM data");
			return false;
		}

		Preferences.debug("Main header has been correctly decoded.\n",
				Preferences.DEBUG_FILEIO);

		/* Position of the last element if the main header */
		try {
			filePointer = l_stream.getFilePointer();
		} catch (IOException e) {
			MipavUtil.displayError("IOException " + e
					+ " on l_stream.getFilePointer()");
			return false;
		}
		l_j2k.cstr_index.main_head_end = filePointer - 2;

		/* Next step: read a tile-part header */
		l_j2k.m_decoder.m_state = J2K_STATE_TPHSOT;

		return true;
	}

	/**
	 * Merges all PPM markers read (Packed headers, main header)
	 * 
	 * @param p_cp
	 *            main coding parameters.
	 */
	private boolean opj_j2k_merge_ppm(opj_cp_t p_cp) {
		int i, l_ppm_data_size, l_N_ppm_remaining;

		/* preconditions */
		if (p_cp == null) {
			MipavUtil.displayError("p_cp == null at opj_j2k_merge_ppm entry");
			return false;
		}
		if (p_cp.ppm_buffer != null) {
			MipavUtil
					.displayError("p_cp.ppm_buffer != null at opj_j2k_merge_ppm entry");
			return false;
		}

		if (p_cp.ppm == 0) {
			return true;
		}

		l_ppm_data_size = 0;
		l_N_ppm_remaining = 0;
		for (i = 0; i < p_cp.ppm_markers_count; ++i) {
			if (p_cp.ppm_markers[i].m_data != null) { /*
													 * standard doesn't seem to
													 * require contiguous Zppm
													 */
				int l_N_ppm;
				int l_data_size = p_cp.ppm_markers[i].m_data_size;
				final byte l_data[] = p_cp.ppm_markers[i].m_data;
				int l_data_offset = 0;

				if (l_N_ppm_remaining >= l_data_size) {
					l_N_ppm_remaining -= l_data_size;
					l_data_size = 0;
				} else {
					l_data_offset += l_N_ppm_remaining;
					l_data_size -= l_N_ppm_remaining;
					l_N_ppm_remaining = 0;
				}

				if (l_data_size > 0) {
					do {
						/* read Nppm */
						if (l_data_size < 4) {
							/* clean up to be done on l_cp destruction */
							MipavUtil
									.displayError("Not enough bytes to read Nppm");
							return false;
						}
						l_N_ppm = getBufferInt(l_data, l_data_offset, endianess);
						l_data_offset += 4;
						l_data_size -= 4;
						l_ppm_data_size += l_N_ppm; /*
													 * can't overflow, max 256
													 * markers of max 65536
													 * bytes, that is when PPM
													 * markers are not corrupted
													 * which is checked
													 * elsewhere
													 */

						if (l_data_size >= l_N_ppm) {
							l_data_size -= l_N_ppm;
							l_data_offset += l_N_ppm;
						} else {
							l_N_ppm_remaining = l_N_ppm - l_data_size;
							l_data_size = 0;
						}
					} while (l_data_size > 0);
				}
			}
		}

		if (l_N_ppm_remaining != 0) {
			/* clean up to be done on l_cp destruction */
			MipavUtil.displayError("Corrupted PPM markers");
			return false;
		}

		p_cp.ppm_buffer = new byte[l_ppm_data_size];

		p_cp.ppm_len = l_ppm_data_size;
		l_ppm_data_size = 0;
		l_N_ppm_remaining = 0;
		for (i = 0; i < p_cp.ppm_markers_count; ++i) {
			if (p_cp.ppm_markers[i].m_data != null) { /*
													 * standard doesn't seem to
													 * require contiguous Zppm
													 */
				int l_N_ppm;
				int l_data_size = p_cp.ppm_markers[i].m_data_size;
				final byte l_data[] = p_cp.ppm_markers[i].m_data;
				int l_data_offset = 0;

				if (l_N_ppm_remaining >= l_data_size) {
					for (i = 0; i < l_data_size; i++) {
						p_cp.ppm_buffer[l_ppm_data_size + i] = l_data[i];
					}
					l_ppm_data_size += l_data_size;
					l_N_ppm_remaining -= l_data_size;
					l_data_size = 0;
				} else {
					for (i = 0; i < l_N_ppm_remaining; i++) {
						p_cp.ppm_buffer[l_ppm_data_size + i] = l_data[i];
					}
					l_ppm_data_size += l_N_ppm_remaining;
					l_data_offset += l_N_ppm_remaining;
					l_data_size -= l_N_ppm_remaining;
					l_N_ppm_remaining = 0;
				}

				if (l_data_size > 0) {
					do {
						/* read Nppm */
						if (l_data_size < 4) {
							/* clean up to be done on l_cp destruction */
							MipavUtil
									.displayError("Not enough bytes to read Nppm");
							return false;
						}
						l_N_ppm = getBufferInt(l_data, l_data_offset, endianess);
						l_data_offset += 4;
						l_data_size -= 4;

						if (l_data_size >= l_N_ppm) {
							for (i = 0; i < l_N_ppm; i++) {
								p_cp.ppm_buffer[l_ppm_data_size + i] = l_data[l_data_offset
										+ i];
							}
							l_ppm_data_size += l_N_ppm;
							l_data_size -= l_N_ppm;
							l_data_offset += l_N_ppm;
						} else {
							for (i = 0; i < l_data_size; i++) {
								p_cp.ppm_buffer[l_ppm_data_size + i] = l_data[l_data_offset
										+ i];
							}
							l_ppm_data_size += l_data_size;
							l_N_ppm_remaining = l_N_ppm - l_data_size;
							l_data_size = 0;
						}
					} while (l_data_size > 0);
				}
				p_cp.ppm_markers[i].m_data = null;
				p_cp.ppm_markers[i].m_data_size = 0;
			}
		}

		p_cp.ppm_data = p_cp.ppm_buffer;
		p_cp.ppm_data_size = p_cp.ppm_len;

		p_cp.ppm_markers_count = 0;
		p_cp.ppm_markers = null;

		return true;
	}

	private boolean readMarkerHandler(int handler, opj_j2k_t p_j2k,
			byte p_header_data[], int p_header_size) {
		boolean success;
		switch (handler) {
		case OPJ_J2K_READ_SOT:
			success = opj_j2k_read_sot(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_COD:
			success = opj_j2k_read_cod(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_COC:
			success = opj_j2k_read_coc(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_RGN:
			success = opj_j2k_read_rgn(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_QCD:
			success = opj_j2k_read_qcd(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_QCC:
			success = opj_j2k_read_qcc(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_POC:
			success = opj_j2k_read_poc(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_SIZ:
			success = opj_j2k_read_siz(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_TLM:
			success = opj_j2k_read_tlm(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_PLM:
			success = opj_j2k_read_plm(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_PLT:
			success = opj_j2k_read_plt(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_PPM:
			success = opj_j2k_read_ppm(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_PPT:
			success = opj_j2k_read_ppt(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_CRG:
			success = opj_j2k_read_crg(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_COM:
			success = opj_j2k_read_com(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_MCT:
			success = opj_j2k_read_mct(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_CBD:
			success = opj_j2k_read_cbd(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_MCC:
			success = opj_j2k_read_mcc(p_j2k, p_header_data, p_header_size);
			break;
		case OPJ_J2K_READ_MCO:
			success = opj_j2k_read_mco(p_j2k, p_header_data, p_header_size);
			break;
		default:
			success = false;
		}

		return success;
	}

	/**
	 * Reads a MCO marker (Multiple Component Transform Ordering)
	 * 
	 * @param p_header_data
	 *            the data contained in the MCO box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the MCO marker.
	 */
	private boolean opj_j2k_read_mco(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		int l_tmp, i;
		int l_nb_stages;
		opj_tcp_t l_tcp;
		opj_tccp_t l_tccp;
		opj_image_t l_image;
		int p_header_offset = 0;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		l_image = p_j2k.m_private_image;
		l_tcp = p_j2k.m_decoder.m_state == J2K_STATE_TPH ? p_j2k.m_cp.tcps[p_j2k.m_current_tile_number[0]]
				: p_j2k.m_decoder.m_default_tcp;

		if (p_header_size < 1) {
			MipavUtil
					.displayError("Error reading MCO marker p_header_size < 1");
			return false;
		}

		l_nb_stages = getUnsignedByte(p_header_data, p_header_offset); /*
																		 * Nmco
																		 * :
																		 * only
																		 * one
																		 * transform
																		 * stage
																		 */
		++p_header_offset;

		if (l_nb_stages > 1) {
			Preferences.debug(
					"Cannot take in charge multiple transformation stages.\n",
					Preferences.DEBUG_FILEIO);
			return true;
		}

		if (p_header_size != l_nb_stages + 1) {
			MipavUtil.displayError("Error reading MCO marker");
			return false;
		}

		l_tccp = l_tcp.tccps[0];

		for (i = 0; i < l_image.numcomps; ++i) {
			l_tccp.m_dc_level_shift = 0;
			if (i < l_image.numcomps - 1) {
				l_tccp = l_tcp.tccps[i + 1];
			}
		}

		if (l_tcp.m_mct_decoding_matrix != null) {
			l_tcp.m_mct_decoding_matrix = null;
		}

		for (i = 0; i < l_nb_stages; ++i) {
			l_tmp = getUnsignedByte(p_header_data, p_header_offset);
			++p_header_offset;

			if (!opj_j2k_add_mct(l_tcp, p_j2k.m_private_image, l_tmp)) {
				return false;
			}
		}

		return true;
	}

	private boolean opj_j2k_add_mct(opj_tcp_t p_tcp, opj_image_t p_image,
			int p_index) {
		int i;
		opj_simple_mcc_decorrelation_data_t l_mcc_record;
		opj_mct_data_t l_deco_array, l_offset_array;
		int l_data_size;
		int l_nb_elem;
		int l_offset_data[];
		opj_tccp_t l_tccp;

		/* preconditions */
		if (p_tcp == null) {
			return false;
		}

		l_mcc_record = p_tcp.m_mcc_records[0];

		for (i = 0; i < p_tcp.m_nb_mcc_records; ++i) {
			if (l_mcc_record.m_index == p_index) {
				break;
			}
			if (i < p_tcp.m_nb_mcc_records - 1) {
				l_mcc_record = p_tcp.m_mcc_records[i + 1];
			}
		}

		if (i == p_tcp.m_nb_mcc_records) {
			/** element discarded **/
			return true;
		}

		if (l_mcc_record.m_nb_comps != p_image.numcomps) {
			/** do not support number of comps != image */
			return true;
		}

		l_deco_array = l_mcc_record.m_decorrelation_array;

		if (l_deco_array != null) {
			int index = l_deco_array.m_element_type;
			l_data_size = MCT_ELEMENT_SIZE[index] * p_image.numcomps
					* p_image.numcomps;
			if (l_deco_array.m_data_size != l_data_size) {
				return false;
			}

			l_nb_elem = p_image.numcomps * p_image.numcomps;
			p_tcp.m_mct_decoding_matrix = new float[l_nb_elem];

			if (l_deco_array.m_element_type == MCT_TYPE_INT16) {
				opj_j2k_read_int16_to_float(l_deco_array.m_data,
						p_tcp.m_mct_decoding_matrix, l_nb_elem);
			} else if (l_deco_array.m_element_type == MCT_TYPE_INT32) {
				opj_j2k_read_int32_to_float(l_deco_array.m_data,
						p_tcp.m_mct_decoding_matrix, l_nb_elem);
			} else if (l_deco_array.m_element_type == MCT_TYPE_FLOAT) {
				opj_j2k_read_float32_to_float(l_deco_array.m_data,
						p_tcp.m_mct_decoding_matrix, l_nb_elem);
			} else if (l_deco_array.m_element_type == MCT_TYPE_DOUBLE) {
				opj_j2k_read_float64_to_float(l_deco_array.m_data,
						p_tcp.m_mct_decoding_matrix, l_nb_elem);
			}
		}

		l_offset_array = l_mcc_record.m_offset_array;

		if (l_offset_array != null) {
			int index = l_offset_array.m_element_type;
			l_data_size = MCT_ELEMENT_SIZE[index] * p_image.numcomps;
			if (l_offset_array.m_data_size != l_data_size) {
				return false;
			}

			l_nb_elem = p_image.numcomps;
			l_offset_data = new int[l_nb_elem];

			if (l_offset_array.m_element_type == MCT_TYPE_INT16) {
				opj_j2k_read_int16_to_int32(l_offset_array.m_data,
						l_offset_data, l_nb_elem);
			} else if (l_offset_array.m_element_type == MCT_TYPE_INT32) {
				opj_j2k_read_int32_to_int32(l_offset_array.m_data,
						l_offset_data, l_nb_elem);
			} else if (l_offset_array.m_element_type == MCT_TYPE_FLOAT) {
				opj_j2k_read_float32_to_int32(l_offset_array.m_data,
						l_offset_data, l_nb_elem);
			} else if (l_offset_array.m_element_type == MCT_TYPE_DOUBLE) {
				opj_j2k_read_float64_to_int32(l_offset_array.m_data,
						l_offset_data, l_nb_elem);
			}

			l_tccp = p_tcp.tccps[0];

			for (i = 0; i < p_image.numcomps; ++i) {
				l_tccp.m_dc_level_shift = l_offset_data[i];
				if (i < p_image.numcomps - 1) {
					l_tccp = p_tcp.tccps[i + 1];
				}
			}

			l_offset_data = null;
		}

		return true;
	}

	private void opj_j2k_read_int16_to_float(byte p_src_data[],
			float p_dest_data[], int p_nb_elem) {
		int i;

		for (i = 0; i < p_nb_elem; ++i) {
			p_dest_data[i] = (float) getBufferUShort(p_src_data, 2 * i,
					endianess);
		}
	}

	private void opj_j2k_read_int32_to_float(byte p_src_data[],
			float p_dest_data[], int p_nb_elem) {
		int i;

		for (i = 0; i < p_nb_elem; ++i) {
			p_dest_data[i] = (float) getBufferInt(p_src_data, 4 * i, endianess);
		}
	}

	private void opj_j2k_read_float32_to_float(byte p_src_data[],
			float p_dest_data[], int p_nb_elem) {
		int i;

		for (i = 0; i < p_nb_elem; ++i) {
			p_dest_data[i] = getBufferFloat(p_src_data, 4 * i, endianess);
		}
	}

	private void opj_j2k_read_float64_to_float(byte p_src_data[],
			float p_dest_data[], int p_nb_elem) {
		int i;

		for (i = 0; i < p_nb_elem; ++i) {
			p_dest_data[i] = (float) getBufferDouble(p_src_data, 8 * i,
					endianess);
		}
	}

	private void opj_j2k_read_int16_to_int32(byte p_src_data[],
			int p_dest_data[], int p_nb_elem) {
		int i;

		for (i = 0; i < p_nb_elem; ++i) {
			p_dest_data[i] = getBufferUShort(p_src_data, 2 * i, endianess);
		}
	}

	private void opj_j2k_read_int32_to_int32(byte p_src_data[],
			int p_dest_data[], int p_nb_elem) {
		int i;

		for (i = 0; i < p_nb_elem; ++i) {
			p_dest_data[i] = getBufferInt(p_src_data, 4 * i, endianess);
		}
	}

	private void opj_j2k_read_float32_to_int32(byte p_src_data[],
			int p_dest_data[], int p_nb_elem) {
		int i;

		for (i = 0; i < p_nb_elem; ++i) {
			p_dest_data[i] = (int) getBufferFloat(p_src_data, 4 * i, endianess);
		}
	}

	private void opj_j2k_read_float64_to_int32(byte p_src_data[],
			int p_dest_data[], int p_nb_elem) {
		int i;

		for (i = 0; i < p_nb_elem; ++i) {
			p_dest_data[i] = (int) getBufferDouble(p_src_data, 8 * i, endianess);
		}
	}

	/**
	 * Reads a MCC marker (Multiple Component Collection)
	 * 
	 * @param p_header_data
	 *            the data contained in the MCC box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the MCC marker.
	 */
	private boolean opj_j2k_read_mcc(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		int i, j;
		int l_tmp;
		int l_indix;
		opj_tcp_t l_tcp;
		opj_simple_mcc_decorrelation_data_t l_mcc_record;
		opj_mct_data_t l_mct_data;
		int l_nb_collections;
		int l_nb_comps;
		int l_nb_bytes_by_comp;
		int p_header_offset = 0;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		l_tcp = p_j2k.m_decoder.m_state == J2K_STATE_TPH ? p_j2k.m_cp.tcps[p_j2k.m_current_tile_number[0]]
				: p_j2k.m_decoder.m_default_tcp;

		if (p_header_size < 2) {
			MipavUtil
					.displayError("Error reading MCC marker p_header_size < 2");
			return false;
		}

		/* first marker */
		l_tmp = getBufferUShort(p_header_data, p_header_offset, endianess); /* Zmcc */
		p_header_offset += 2;
		if (l_tmp != 0) {
			Preferences.debug("Cannot take in charge multiple data spanning\n",
					Preferences.DEBUG_FILEIO);
			return true;
		}

		if (p_header_size < 7) {
			MipavUtil
					.displayError("Error reading MCC marker p_header_size < 7");
			return false;
		}

		l_indix = getUnsignedByte(p_header_data, p_header_offset); /*
																	 * Imcc ->
																	 * no need
																	 * for other
																	 * values,
																	 * take the
																	 * first
																	 */
		++p_header_offset;

		l_mcc_record = l_tcp.m_mcc_records[0];

		for (i = 0; i < l_tcp.m_nb_mcc_records; ++i) {
			if (l_mcc_record.m_index == l_indix) {
				break;
			}
			if (i < l_tcp.m_nb_mcc_records - 1) {
				l_mcc_record = l_tcp.m_mcc_records[i + 1];
			}
		}

		/** NOT FOUND */
		if (i == l_tcp.m_nb_mcc_records) {
			if (l_tcp.m_nb_mcc_records == l_tcp.m_nb_max_mcc_records) {
				opj_simple_mcc_decorrelation_data_t new_mcc_records[];
				l_tcp.m_nb_max_mcc_records += OPJ_J2K_MCC_DEFAULT_NB_RECORDS;

				new_mcc_records = new opj_simple_mcc_decorrelation_data_t[l_tcp.m_nb_max_mcc_records];
				for (i = 0; i < l_tcp.m_nb_mcc_records; i++) {
					new_mcc_records[i] = l_tcp.m_mcc_records[i];
				}
				for (i = l_tcp.m_nb_mcc_records; i < l_tcp.m_nb_max_mcc_records; i++) {
					new_mcc_records[i] = new opj_simple_mcc_decorrelation_data_t();
				}

				l_tcp.m_mcc_records = new_mcc_records;
				l_mcc_record = l_tcp.m_mcc_records[l_tcp.m_nb_mcc_records];
			}
			l_mcc_record = l_tcp.m_mcc_records[l_tcp.m_nb_mcc_records];
		}
		l_mcc_record.m_index = l_indix;

		/* only one marker atm */
		l_tmp = getBufferUShort(p_header_data, p_header_offset, endianess); /* Ymcc */
		p_header_offset += 2;
		if (l_tmp != 0) {
			Preferences.debug("Cannot take in charge multiple data spanning\n",
					Preferences.DEBUG_FILEIO);
			return true;
		}

		l_nb_collections = getBufferUShort(p_header_data, p_header_offset,
				endianess); /* Qmcc -> number of collections -> 1 */
		p_header_offset += 2;

		if (l_nb_collections > 1) {
			Preferences.debug("Cannot take in charge multiple collections\n",
					Preferences.DEBUG_FILEIO);
			return true;
		}

		p_header_size -= 7;

		for (i = 0; i < l_nb_collections; ++i) {
			if (p_header_size < 3) {
				MipavUtil
						.displayError("Error reading MCC marker p_header_size < 3");
				return false;
			}

			l_tmp = getUnsignedByte(p_header_data, p_header_offset); /*
																	 * Xmcci
																	 * type of
																	 * component
																	 * transformation
																	 * -> array
																	 * based
																	 * decorrelation
																	 */
			++p_header_offset;

			if (l_tmp != 1) {
				Preferences
						.debug("Cannot take in charge collections other than array decorrelation\n",
								Preferences.DEBUG_FILEIO);
				return true;
			}

			l_nb_comps = getBufferUShort(p_header_data, p_header_offset,
					endianess);

			p_header_offset += 2;
			p_header_size -= 3;

			l_nb_bytes_by_comp = 1 + (l_nb_comps >>> 15);
			l_mcc_record.m_nb_comps = l_nb_comps & 0x7fff;

			if (p_header_size < (l_nb_bytes_by_comp
					* l_mcc_record.m_nb_comps + 2)) {
				MipavUtil.displayError("Error reading MCC marker");
				return false;
			}

			p_header_size -= (l_nb_bytes_by_comp * l_mcc_record.m_nb_comps + 2);

			for (j = 0; j < l_mcc_record.m_nb_comps; ++j) {
				if (l_nb_bytes_by_comp == 1) {
					l_tmp = getUnsignedByte(p_header_data, p_header_offset); /*
																			 * Cmccij
																			 * Component
																			 * offset
																			 */
				} else if (l_nb_bytes_by_comp == 2) {
					l_tmp = getBufferUShort(p_header_data, p_header_offset,
							endianess); /* Cmccij Component offset */
				} else if (l_nb_bytes_by_comp == 3) {
					l_tmp = getBuffer3Bytes(p_header_data, p_header_offset,
							endianess); /* Cmccij Component offset */
				} else if (l_nb_bytes_by_comp == 4) {
					l_tmp = getBufferInt(p_header_data, p_header_offset,
							endianess); /* Cmccij Component offset */
				}
				p_header_offset += l_nb_bytes_by_comp;

				if (l_tmp != j) {
					Preferences
							.debug("Cannot take in charge collections with indix shuffle\n",
									Preferences.DEBUG_FILEIO);
					return true;
				}
			}

			l_nb_comps = getBufferUShort(p_header_data, p_header_offset,
					endianess);
			p_header_offset += 2;

			l_nb_bytes_by_comp = 1 + (l_nb_comps >>> 15);
			l_nb_comps &= 0x7fff;

			if (l_nb_comps != l_mcc_record.m_nb_comps) {
				Preferences
						.debug("Cannot take in charge collections without same number of indixes\n",
								Preferences.DEBUG_FILEIO);
				return true;
			}

			if (p_header_size < (l_nb_bytes_by_comp
					* l_mcc_record.m_nb_comps + 3)) {
				MipavUtil.displayError("Error reading MCC marker");
				return false;
			}

			p_header_size -= (l_nb_bytes_by_comp * l_mcc_record.m_nb_comps + 3);

			for (j = 0; j < l_mcc_record.m_nb_comps; ++j) {
				if (l_nb_bytes_by_comp == 1) {
					l_tmp = getUnsignedByte(p_header_data, p_header_offset); /*
																			 * Wmccij
																			 * Component
																			 * offset
																			 */
				} else if (l_nb_bytes_by_comp == 2) {
					l_tmp = getBufferUShort(p_header_data, p_header_offset,
							endianess); /* Wmccij Component offset */
				} else if (l_nb_bytes_by_comp == 3) {
					l_tmp = getBuffer3Bytes(p_header_data, p_header_offset,
							endianess); /* Wmccij Component offset */
				} else if (l_nb_bytes_by_comp == 4) {
					l_tmp = getBufferInt(p_header_data, p_header_offset,
							endianess); /* Wmccij Component offset */
				}
				p_header_offset += l_nb_bytes_by_comp;

				if (l_tmp != j) {
					Preferences
							.debug("Cannot take in charge collections with indix shuffle\n",
									Preferences.DEBUG_FILEIO);
					return true;
				}
			}

			l_tmp = getBuffer3Bytes(p_header_data, p_header_offset, endianess); /*
																				 * Wmccij
																				 * Component
																				 * offset
																				 */
			p_header_offset += 3;

			l_mcc_record.m_is_irreversible = (((l_tmp >> 16) & 1) == 0);
			l_mcc_record.m_decorrelation_array = null;
			l_mcc_record.m_offset_array = null;

			l_indix = l_tmp & 0xff;
			if (l_indix != 0) {
				l_mct_data = l_tcp.m_mct_records[0];
				for (j = 0; j < l_tcp.m_nb_mct_records; ++j) {
					if (l_mct_data.m_index == l_indix) {
						l_mcc_record.m_decorrelation_array = l_mct_data;
						break;
					}
					if (j < l_tcp.m_nb_mct_records - 1) {
						l_mct_data = l_tcp.m_mct_records[j + 1];
					}
				}

				if (l_mcc_record.m_decorrelation_array == null) {
					MipavUtil.displayError("Error reading MCC marker");
					return false;
				}
			}

			l_indix = (l_tmp >> 8) & 0xff;
			if (l_indix != 0) {
				l_mct_data = l_tcp.m_mct_records[0];
				for (j = 0; j < l_tcp.m_nb_mct_records; ++j) {
					if (l_mct_data.m_index == l_indix) {
						l_mcc_record.m_offset_array = l_mct_data;
						break;
					}
					if (j < l_tcp.m_nb_mct_records - 1) {
						l_mct_data = l_tcp.m_mct_records[j + 1];
					}
				}

				if (l_mcc_record.m_offset_array == null) {
					MipavUtil.displayError("Error reading MCC marker");
					return false;
				}
			}
		}

		if (p_header_size != 0) {
			MipavUtil.displayError("Error reading MCC marker");
			return false;
		}

		++l_tcp.m_nb_mcc_records;

		return true;
	}

	/**
	 * Converts byte data to int data.
	 * 
	 * @param buffer
	 *            Array of byte data.
	 * @param index
	 *            Index into array data.
	 * @param bigEndian
	 *            <code>true</code> indicates big endian byte order,
	 *            <code>false</code> indicates little endian.
	 * 
	 * @return Integer value extracted from byte array.
	 */
	public final int getBuffer3Bytes(final byte[] buffer, final int index,
			final boolean bigEndian) {

		if (bigEndian) {
			return (((buffer[index] & 0xff) << 16)
					| ((buffer[index + 1] & 0xff) << 8) | (buffer[index + 2] & 0xff));
		} else {
			return (((buffer[index + 2] & 0xff) << 16)
					| ((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
		}
	}

	/**
	 * Reads a CBD marker (Component bit depth definition)
	 * 
	 * @param p_header_data
	 *            the data contained in the CBD box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the CBD marker.
	 */
	private boolean opj_j2k_read_cbd(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		int l_nb_comp, l_num_comp;
		int l_comp_def;
		int i;
		opj_image_comp_t l_comp = null;
		int p_header_offset = 0;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		l_num_comp = p_j2k.m_private_image.numcomps;

		if (p_header_size != p_j2k.m_private_image.numcomps + 2) {
			MipavUtil.displayError("Error reading CBD marker");
			return false;
		}

		l_nb_comp = getBufferUShort(p_header_data, p_header_offset, endianess); /* Ncbd */
		p_header_offset += 2;

		if (l_nb_comp != l_num_comp) {
			MipavUtil.displayError("Error reading CBD marker");
			return false;
		}

		l_comp = p_j2k.m_private_image.comps[0];
		for (i = 0; i < l_num_comp; ++i) {
			l_comp_def = getUnsignedByte(p_header_data, p_header_offset); /*
																		 * Component
																		 * bit
																		 * depth
																		 */
			++p_header_offset;
			l_comp.sgnd = (l_comp_def >> 7) & 1;
			l_comp.prec = (l_comp_def & 0x7f) + 1;
			if (i < l_num_comp - 1) {
				l_comp = p_j2k.m_private_image.comps[i + 1];
			}
		}

		return true;
	}

	/**
	 * Reads a MCT marker (Multiple Component Transform)
	 * 
	 * @param p_header_data
	 *            the data contained in the MCT box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the MCT marker.
	 */
	private boolean opj_j2k_read_mct(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		int i, j;
		opj_tcp_t l_tcp = null;
		int l_tmp;
		int l_indix;
		opj_mct_data_t l_mct_data;
		int p_header_offset = 0;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		l_tcp = p_j2k.m_decoder.m_state == J2K_STATE_TPH ? p_j2k.m_cp.tcps[p_j2k.m_current_tile_number[0]]
				: p_j2k.m_decoder.m_default_tcp;

		if (p_header_size < 2) {
			MipavUtil
					.displayError("Error reading MCT marker p_header_size < 2");
			return false;
		}

		/* first marker */
		l_tmp = getBufferUShort(p_header_data, p_header_offset, endianess); /* Zmct */
		p_header_offset += 2;
		if (l_tmp != 0) {
			Preferences
					.debug("Cannot take in charge mct data within multiple MCT records\n",
							Preferences.DEBUG_FILEIO);
			return true;
		}

		if (p_header_size <= 6) {
			MipavUtil
					.displayError("Error reading MCT marker p_header_size <= 6");
			return false;
		}

		/*
		 * Imct -> no need for other values, take the first, type is double with
		 * decorrelation x0000 1101 0000 0000
		 */
		l_tmp = getBufferUShort(p_header_data, p_header_offset, endianess); /* Imct */
		p_header_offset += 2;

		l_indix = l_tmp & 0xff;
		l_mct_data = l_tcp.m_mct_records[0];

		for (i = 0; i < l_tcp.m_nb_mct_records; ++i) {
			if (l_mct_data.m_index == l_indix) {
				break;
			}
			if (i < l_tcp.m_nb_mct_records - 1) {
				l_mct_data = l_tcp.m_mct_records[i + 1];
			}
		}

		/* NOT FOUND */
		if (i == l_tcp.m_nb_mct_records) {
			if (l_tcp.m_nb_mct_records == l_tcp.m_nb_max_mct_records) {
				opj_mct_data_t new_mct_records[];
				l_tcp.m_nb_max_mct_records += OPJ_J2K_MCT_DEFAULT_NB_RECORDS;

				new_mct_records = new opj_mct_data_t[l_tcp.m_nb_max_mct_records];
				for (j = 0; j < l_tcp.m_nb_mct_records; j++) {
					new_mct_records[j] = l_tcp.m_mct_records[j];
				}
				for (j = l_tcp.m_nb_mct_records; j < l_tcp.m_nb_max_mct_records; j++) {
					new_mct_records[j] = new opj_mct_data_t();
				}

				l_tcp.m_mct_records = new_mct_records;
			}

			l_mct_data = l_tcp.m_mct_records[l_tcp.m_nb_mct_records];
			++l_tcp.m_nb_mct_records;
		}

		if (l_mct_data.m_data != null) {
			l_mct_data.m_data = null;
		}

		l_mct_data.m_index = l_indix;
		l_mct_data.m_array_type = ((l_tmp >> 8) & 3);
		l_mct_data.m_element_type = ((l_tmp >> 10) & 3);

		l_tmp = getBufferUShort(p_header_data, p_header_offset, endianess); /* Ymct */
		p_header_offset += 2;
		if (l_tmp != 0) {
			Preferences.debug("Cannot take in charge multiple MCT markers\n",
					Preferences.DEBUG_FILEIO);
			return true;
		}

		p_header_size -= 6;

		l_mct_data.m_data = new byte[p_header_size];
		for (i = 0; i < p_header_size; i++) {
			l_mct_data.m_data[i] = p_header_data[i];
		}

		l_mct_data.m_data_size = p_header_size;

		return true;
	}

	/**
	 * Reads a COM marker (comments)
	 * 
	 * @param p_j2k
	 *            the jpeg2000 file codec.
	 * @param p_header_data
	 *            the data contained in the COM box.
	 * @param p_header_size
	 *            the size of the data contained in the COM marker.
	 */
	private boolean opj_j2k_read_com(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		return true;
	}

	/**
	 * Reads a CRG marker (Component registration)
	 * 
	 * @param p_header_data
	 *            the data contained in the CRG box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the CRG marker.
	 */
	private boolean opj_j2k_read_crg(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		int l_nb_comp;
		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		l_nb_comp = p_j2k.m_private_image.numcomps;

		if (p_header_size != l_nb_comp * 4) {
			MipavUtil.displayError("Error reading CRG marker");
			return false;
		}
		/*
		 * Do not care of this at the moment since only local variables are set
		 * here
		 */
		/*
		 * for (i = 0; i < l_nb_comp; ++i) {
		 * opj_read_bytes(p_header_data,&l_Xcrg_i,2); // Xcrg_i
		 * p_header_data+=2; opj_read_bytes(p_header_data,&l_Ycrg_i,2); //
		 * Xcrg_i p_header_data+=2; }
		 */
		return true;
	}

	/**
	 * Reads a PPT marker (Packed packet headers, tile-part header)
	 * 
	 * @param p_header_data
	 *            the data contained in the PPT box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the PPT marker.
	 */
	private boolean opj_j2k_read_ppt(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		opj_cp_t l_cp = null;
		opj_tcp_t l_tcp = null;
		int l_Z_ppt;
		int p_header_offset = 0;
		int i;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		/* We need to have the Z_ppt element + 1 byte of Ippt at minimum */
		if (p_header_size < 2) {
			MipavUtil
					.displayError("Error reading PPT marker p_header_size < 2");
			return false;
		}

		l_cp = p_j2k.m_cp;
		if (l_cp.ppm == 1) {
			MipavUtil
					.displayError("Error reading PPT marker: packet header have been previously found in the main header (PPM marker).");
			return false;
		}

		l_tcp = l_cp.tcps[p_j2k.m_current_tile_number[0]];
		l_tcp.ppt = 1;

		l_Z_ppt = getUnsignedByte(p_header_data, p_header_offset); /* Z_ppt */
		++p_header_offset;
		--p_header_size;

		/* check allocation needed */
		if (l_tcp.ppt_markers == null) { /* first PPT marker */
			int l_newCount = l_Z_ppt + 1; /* can't overflow, l_Z_ppt is UINT8 */
			if (l_tcp.ppt_markers_count != 0) {
				MipavUtil.displayError("l_tcp.ppt_markers_count != 0");
				return false;
			}

			l_tcp.ppt_markers = new opj_ppx[l_newCount];
			for (i = 0; i < l_newCount; i++) {
				l_tcp.ppt_markers[i] = new opj_ppx();
			}

			l_tcp.ppt_markers_count = l_newCount;
		} else if (l_tcp.ppt_markers_count <= l_Z_ppt) {
			int l_newCount = l_Z_ppt + 1; /* can't overflow, l_Z_ppt is UINT8 */
			opj_ppx new_ppt_markers[] = new opj_ppx[l_newCount];
			for (i = 0; i < l_tcp.ppt_markers_count; i++) {
				new_ppt_markers[i] = l_tcp.ppt_markers[i];
			}
			for (i = l_tcp.ppt_markers_count; i < l_newCount; i++) {
				new_ppt_markers[i] = new opj_ppx();
			}

			l_tcp.ppt_markers = new_ppt_markers;
			l_tcp.ppt_markers_count = l_newCount;
		}

		if (l_tcp.ppt_markers[l_Z_ppt].m_data != null) {
			/* clean up to be done on l_tcp destruction */
			MipavUtil.displayError("Zppt " + l_Z_ppt + " already read");
			return false;
		}

		l_tcp.ppt_markers[l_Z_ppt].m_data = new byte[p_header_size];
		l_tcp.ppt_markers[l_Z_ppt].m_data_size = p_header_size;
		for (i = 0; i < p_header_size; i++) {
			l_tcp.ppt_markers[l_Z_ppt].m_data[i] = p_header_data[i];
		}
		return true;
	}

	/**
	 * Reads a PPM marker (Packed packet headers, main header)
	 * 
	 * @param p_header_data
	 *            the data contained in the PPM box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the PPM marker.
	 */

	private boolean opj_j2k_read_ppm(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		opj_cp_t l_cp = null;
		int l_Z_ppm;
		int p_header_offset = 0;
		int i;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		/* We need to have the Z_ppm element + 1 byte of Nppm/Ippm at minimum */
		if (p_header_size < 2) {
			MipavUtil
					.displayError("Error reading PPM marker p_header_size < 2");
			return false;
		}

		l_cp = p_j2k.m_cp;
		l_cp.ppm = 1;

		l_Z_ppm = getUnsignedByte(p_header_data, p_header_offset); /* Z_ppm */
		++p_header_offset;
		--p_header_size;

		/* check allocation needed */
		if (l_cp.ppm_markers == null) { /* first PPM marker */
			int l_newCount = l_Z_ppm + 1; /* can't overflow, l_Z_ppm is UINT8 */
			if (l_cp.ppm_markers_count != 0) {
				MipavUtil.displayError("l_cp.ppm_markers_count != 0");
				return false;
			}

			l_cp.ppm_markers = new opj_ppx[l_newCount];
			for (i = 0; i < l_newCount; i++) {
				l_cp.ppm_markers[i] = new opj_ppx();
			}

			l_cp.ppm_markers_count = l_newCount;
		} else if (l_cp.ppm_markers_count <= l_Z_ppm) {
			int l_newCount = l_Z_ppm + 1; /* can't overflow, l_Z_ppm is UINT8 */
			opj_ppx new_ppm_markers[] = new opj_ppx[l_newCount];
			for (i = 0; i < l_cp.ppm_markers_count; i++) {
				new_ppm_markers[i] = l_cp.ppm_markers[i];
			}
			for (i = l_cp.ppm_markers_count; i < l_newCount; i++) {
				new_ppm_markers[i] = new opj_ppx();
			}
			l_cp.ppm_markers = new_ppm_markers;
			l_cp.ppm_markers_count = l_newCount;
		}

		if (l_cp.ppm_markers[l_Z_ppm].m_data != null) {
			/* clean up to be done on l_cp destruction */
			MipavUtil.displayError("Zppm " + l_Z_ppm + " already read");
			return false;
		}

		l_cp.ppm_markers[l_Z_ppm].m_data = new byte[p_header_size];
		l_cp.ppm_markers[l_Z_ppm].m_data_size = p_header_size;
		for (i = 0; i < p_header_size; i++) {
			l_cp.ppm_markers[l_Z_ppm].m_data[i] = p_header_data[i];
		}

		return true;
	}

	/**
	 * Reads a PLT marker (Packet length, tile-part header)
	 * 
	 * @param p_header_data
	 *            the data contained in the PLT box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the PLT marker.
	 */
	private boolean opj_j2k_read_plt(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		int l_tmp, l_packet_len = 0, i;
		int p_header_offset = 0;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		if (p_header_size < 1) {
			MipavUtil
					.displayError("Error reading PLT marker p_header_size < 1");
			return false;
		}

		getUnsignedByte(p_header_data, p_header_offset); /* Zplt */
		p_header_offset++;
		--p_header_size;

		for (i = 0; i < p_header_size; ++i) {
			l_tmp = getUnsignedByte(p_header_data, p_header_offset); /* Iplt_ij */
			p_header_offset++;
			/* take only the last seven bytes */
			l_packet_len |= (l_tmp & 0x7f);
			if ((l_tmp & 0x80) != 0) {
				l_packet_len <<= 7;
			} else {
				/* store packet length and proceed to next packet */
				l_packet_len = 0;
			}
		}

		if (l_packet_len != 0) {
			MipavUtil
					.displayError("Error reading PLT marker l_packet_len != 0");
			return false;
		}

		return true;
	}

	/**
	 * Reads a PLM marker (Packet length, main header marker)
	 * 
	 * @param p_header_data
	 *            the data contained in the PLM box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the PLM marker.
	 */
	private boolean opj_j2k_read_plm(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		if (p_header_size < 1) {
			MipavUtil
					.displayError("Error reading PLM marker p_header_size < 1");
			return false;
		}
		/*
		 * Do not care of this at the moment since only local variables are set
		 * here
		 */
		/*
		 * opj_read_bytes(p_header_data,&l_Zplm,1); // Zplm ++p_header_data;
		 * --p_header_size;
		 * 
		 * while (p_header_size > 0) { opj_read_bytes(p_header_data,&l_Nplm,1);
		 * // Nplm ++p_header_data; p_header_size -= (1+l_Nplm); if
		 * (p_header_size < 0) { opj_event_msg(p_manager, EVT_ERROR,
		 * "Error reading PLM marker\n"); return false; } for (i = 0; i <
		 * l_Nplm; ++i) { opj_read_bytes(p_header_data,&l_tmp,1); // Iplm_ij
		 * ++p_header_data; // take only the last seven bytes l_packet_len |=
		 * (l_tmp & 0x7f); if (l_tmp & 0x80) { l_packet_len <<= 7; } else { //
		 * store packet length and proceed to next packet l_packet_len = 0; } }
		 * if (l_packet_len != 0) { opj_event_msg(p_manager, EVT_ERROR,
		 * "Error reading PLM marker\n"); return false; } }
		 */
		return true;
	}

	/**
	 * Reads a TLM marker (Tile Length Marker)
	 * 
	 * @param p_header_data
	 *            the data contained in the TLM box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the TLM marker.
	 */
	private boolean opj_j2k_read_tlm(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		int l_Stlm, l_ST, l_SP, l_tot_num_tp_remaining, l_quotient, l_Ptlm_size;
		int p_header_offset = 0;
		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		if (p_header_size < 2) {
			MipavUtil
					.displayError("Error reading TLM marker p_header_size < 2");
			return false;
		}
		p_header_size -= 2;
		getUnsignedByte(p_header_data, p_header_offset); /* Ztlm */
		p_header_offset++;
		l_Stlm = getUnsignedByte(p_header_data, p_header_offset); /* Stlm */
		p_header_offset++;

		l_ST = ((l_Stlm >> 4) & 0x3);
		l_SP = (l_Stlm >> 6) & 0x1;

		l_Ptlm_size = (l_SP + 1) * 2;
		l_quotient = l_Ptlm_size + l_ST;

		l_tot_num_tp_remaining = p_header_size % l_quotient;

		if (l_tot_num_tp_remaining != 0) {
			MipavUtil.displayError("Error reading TLM marker");
			return false;
		}
		/*
		 * FIXME Do not care of this at the moment since only local variables
		 * are set here
		 */
		/*
		 * for (i = 0; i < l_tot_num_tp; ++i) {
		 * opj_read_bytes(p_header_data,&l_Ttlm_i,l_ST); // Ttlm_i p_header_data
		 * += l_ST; opj_read_bytes(p_header_data,&l_Ptlm_i,l_Ptlm_size); //
		 * Ptlm_i p_header_data += l_Ptlm_size; }
		 */
		return true;
	}

	/**
	 * Reads a SIZ marker (image and tile size)
	 * 
	 * @param p_j2k
	 *            the jpeg2000 file codec.
	 * @param p_header_data
	 *            the data contained in the SIZ box.
	 * @param p_header_size
	 *            the size of the data contained in the SIZ marker.
	 */
	private boolean opj_j2k_read_siz(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		int i, j, k;
		int l_nb_comp;
		int l_nb_comp_remain;
		int l_remaining_size;
		int l_nb_tiles;
		int l_tmp, l_tx1, l_ty1;
		opj_image_t l_image = null;
		opj_cp_t l_cp = null;
		opj_image_comp_t l_img_comp = null;
		opj_tcp_t l_current_tile_param = null;
		int p_header_offset = 0;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		l_image = p_j2k.m_private_image;
		l_cp = p_j2k.m_cp;

		/* minimum size == 39 - 3 (= minimum component parameter) */
		if (p_header_size < 36) {
			MipavUtil
					.displayError("Error with SIZ marker size p_header_size < 36");
			return false;
		}

		l_remaining_size = p_header_size - 36;
		l_nb_comp = l_remaining_size / 3;
		l_nb_comp_remain = l_remaining_size % 3;
		if (l_nb_comp_remain != 0) {
			MipavUtil.displayError("Error with SIZ marker size");
			return false;
		}

		l_tmp = getBufferUShort(p_header_data, p_header_offset, endianess); /*
																			 * Rsiz
																			 * (
																			 * capabilities
																			 * )
																			 */
		p_header_offset += 2;
		l_cp.rsiz = (short) l_tmp;
		l_image.x1 = getBufferInt(p_header_data, p_header_offset, endianess); /* Xsiz */
		p_header_offset += 4;
		l_image.y1 = getBufferInt(p_header_data, p_header_offset, endianess); /* Ysiz */
		p_header_offset += 4;
		l_image.x0 = getBufferInt(p_header_data, p_header_offset, endianess); /* X0siz */
		p_header_offset += 4;
		l_image.y0 = getBufferInt(p_header_data, p_header_offset, endianess); /* Y0siz */
		p_header_offset += 4;
		l_cp.tdx = getBufferInt(p_header_data, p_header_offset, endianess); /* XTsiz */
		p_header_offset += 4;
		l_cp.tdy = getBufferInt(p_header_data, p_header_offset, endianess); /* YTsiz */
		p_header_offset += 4;
		l_cp.tx0 = getBufferInt(p_header_data, p_header_offset, endianess); /* XT0siz */
		p_header_offset += 4;
		l_cp.ty0 = getBufferInt(p_header_data, p_header_offset, endianess); /* YT0siz */
		p_header_offset += 4;
		l_tmp = getBufferUShort(p_header_data, p_header_offset, endianess); /* Csiz */
		p_header_offset += 2;
		if (l_tmp < 16385)
			l_image.numcomps = l_tmp;
		else {
			MipavUtil
					.displayError("Error with SIZ marker: number of component is illegal -> "
							+ l_tmp);
			return false;
		}

		if (l_image.numcomps != l_nb_comp) {
			MipavUtil
					.displayError("Error with SIZ marker: number of component is not compatible"
							+ " with the remaining number of parameters");
			MipavUtil.displayError(l_image.numcomps + " versus " + l_nb_comp);
			return false;
		}

		/* testcase 4035.pdf.SIGSEGV.d8b.3375 */
		/* testcase issue427-null-image-size.jp2 */
		if ((l_image.x0 >= l_image.x1) || (l_image.y0 >= l_image.y1)) {
			MipavUtil
					.displayError("Error with SIZ marker: negative or zero image size"
							+ (l_image.x1 - l_image.x0)
							+ " by  "
							+ (l_image.y1 - l_image.y0));
			return false;
		}
		/*
		 * testcase 2539.pdf.SIGFPE.706.1712 (also 3622.pdf.SIGFPE.706.2916 and
		 * 4008.pdf.SIGFPE.706.3345 and maybe more)
		 */
		if ((l_cp.tdx * l_cp.tdy) == 0) {
			MipavUtil
					.displayError("Error with SIZ marker: invalid tile size (tdx: "
							+ l_cp.tdx + " tdy: " + l_cp.tdy);
			return false;
		}

		/* testcase 1610.pdf.SIGSEGV.59c.681 */
		if (((long) l_image.x1) * ((long) l_image.y1) != (l_image.x1 * l_image.y1)) {
			MipavUtil.displayError("Prevent buffer overflow (x1: " + l_image.x1
					+ ", y1: " + l_image.y1);
			return false;
		}

		/* testcase issue427-illegal-tile-offset.jp2 */
		// l_tx1 = opj_uint_adds(l_cp->tx0, l_cp->tdx); /* manage overflow */
		// l_ty1 = opj_uint_adds(l_cp->ty0, l_cp->tdy); /* manage overflow */
		// Get the saturated sum of two unsigned integers
		// @return Returns saturated sum of a+b
		// static INLINE OPJ_UINT32 opj_uint_adds(OPJ_UINT32 a, OPJ_UINT32 b) {
		// OPJ_UINT64 sum = (OPJ_UINT64)a + (OPJ_UINT64)b;
		// return (OPJ_UINT32)(-(OPJ_INT32)(sum >> 32)) | (OPJ_UINT32)sum;
		// }
		l_tx1 = l_cp.tx0 + l_cp.tdx;
		l_ty1 = l_cp.ty0 + l_cp.tdy;
		if ((l_cp.tx0 > l_image.x0) || (l_cp.ty0 > l_image.y0)
				|| (l_tx1 <= l_image.x0) || (l_ty1 <= l_image.y0)) {
			MipavUtil
					.displayError("Error with SIZ marker: illegal tile offset");
			return false;
		}

		if (useJPWL) {
			if (l_cp.correct) {
				/*
				 * if JPWL is on, we check whether TX errors have damaged too
				 * much the SIZ parameters
				 */
				if ((l_image.x1 * l_image.y1) == 0) {
					MipavUtil.displayError("JPWL: bad image size ("
							+ l_image.x1 + " by " + l_image.y1);
					MipavUtil.displayError("JPWL: giving up");
					return false;
				}

				/*
				 * FIXME check previously in the function so why keep this piece
				 * of code ? Need by the norm ? if (l_image->numcomps != ((len -
				 * 38) / 3)) { opj_event_msg(p_manager, JPWL_ASSUME ?
				 * EVT_WARNING : EVT_ERROR,
				 * "JPWL: Csiz is %d => space in SIZ only for %d comps.!!!\n",
				 * l_image->numcomps, ((len - 38) / 3)); if (!JPWL_ASSUME) {
				 * opj_event_msg(p_manager, EVT_ERROR, "JPWL: giving up\n");
				 * return OPJ_FALSE; }
				 *//* we try to correct */
				/*
				 * opj_event_msg(p_manager, EVT_WARNING,
				 * "- trying to adjust this\n"); if (l_image->numcomps < ((len -
				 * 38) / 3)) { len = 38 + 3 * l_image->numcomps;
				 * opj_event_msg(p_manager, EVT_WARNING,
				 * "- setting Lsiz to %d => HYPOTHESIS!!!\n", len); } else {
				 * l_image->numcomps = ((len - 38) / 3);
				 * opj_event_msg(p_manager, EVT_WARNING,
				 * "- setting Csiz to %d => HYPOTHESIS!!!\n",
				 * l_image->numcomps); } }
				 */

				/* update components number in the jpwl_exp_comps filed */
				l_cp.exp_comps = l_image.numcomps;
			} // if (l_cp.correct)
		} // if (useJPWL)

		/* Allocate the resulting image components */
		l_image.comps = new opj_image_comp_t[l_image.numcomps];
		for (i = 0; i < l_image.numcomps; i++) {
			l_image.comps[i] = new opj_image_comp_t();
		}

		l_img_comp = l_image.comps[0];

		/* Read the component information */
		for (i = 0; i < l_image.numcomps; ++i) {
			int tmp;
			tmp = getUnsignedByte(p_header_data, p_header_offset); /* Ssiz_i */
			p_header_offset++;
			l_img_comp.prec = (tmp & 0x7f) + 1;
			l_img_comp.sgnd = tmp >> 7;
			tmp = getUnsignedByte(p_header_data, p_header_offset); /* XRsiz_i */
			p_header_offset++;
			l_img_comp.dx = tmp; /* should be between 1 and 255 */
			tmp = getUnsignedByte(p_header_data, p_header_offset); /* YRsiz_i */
			p_header_offset++;
			l_img_comp.dy = tmp; /* should be between 1 and 255 */
			if (l_img_comp.dx < 1 || l_img_comp.dx > 255 || l_img_comp.dy < 1
					|| l_img_comp.dy > 255) {
				MipavUtil.displayError("Invalid values for comp = " + i
						+ ": dx=" + l_img_comp.dx + " dy=" + l_img_comp.dy);
				MipavUtil
						.displayError("should be between 1 and 255 according the JPEG2000 norm");
				return false;
			}

			if (useJPWL) {
				if (l_cp.correct) {
					/*
					 * if JPWL is on, we check whether TX errors have damaged
					 * too much the SIZ parameters, again
					 */
					if ((l_image.comps[i].dx * l_image.comps[i].dy) == 0) {
						if (JPWLAssume) {
							Preferences.debug("JPWL: bad XRsize = "
									+ l_image.comps[i].dx + " by YRsiz = "
									+ l_image.comps[i].dy + "\n",
									Preferences.DEBUG_FILEIO);
						} else {
							MipavUtil.displayError("JPWL: bad XRsize = "
									+ l_image.comps[i].dx + " by YRsiz = "
									+ l_image.comps[i].dy);
							MipavUtil.displayError("JPWL: giving up");
							return false;
						}

						/* we try to correct */
						Preferences.debug("- trying to adjust them\n",
								Preferences.DEBUG_FILEIO);
						if (l_image.comps[i].dx == 0) {
							l_image.comps[i].dx = 1;
							Preferences.debug("- setting XRsiz_" + i
									+ " to 1 => HYPOTHESIS!!!\n",
									Preferences.DEBUG_FILEIO);
						}
						if (l_image.comps[i].dy == 0) {
							l_image.comps[i].dy = 1;
							Preferences.debug("- setting YRsiz_" + i
									+ " to 1 => HYPOTHESIS!!!\n",
									Preferences.DEBUG_FILEIO);
						}
					}
				} // if (l_cp.correct)
			} // if (useJPWL)
			l_img_comp.resno_decoded = 0; /* number of resolution decoded */
			l_img_comp.factor = l_cp.m_dec.m_reduce; /*
													 * reducing factor per
													 * component
													 */
			if (i < l_image.numcomps - 1) {
				l_img_comp = l_image.comps[i + 1];
			}
		}

		/* Compute the number of tiles */
		if (l_cp.tdx <= 0) {
			MipavUtil.displayError("l_cp.tdx <= 0");
			return false;
		}
		l_cp.tw = ((l_image.x1 - l_cp.tx0) + l_cp.tdx - 1) / l_cp.tdx;
		if (l_cp.tdy <= 0) {
			MipavUtil.displayError("l_cp.tdy <= 0");
			return false;
		}
		l_cp.th = ((l_image.y1 - l_cp.ty0) + l_cp.tdy - 1) / l_cp.tdy;

		/* Check that the number of tiles is valid */
		if (l_cp.tw == 0 || l_cp.th == 0 || l_cp.tw > 65535 / l_cp.th) {
			MipavUtil.displayError("Invalid number of tiles : " + l_cp.tw
					+ " by " + l_cp.th);
			MipavUtil
					.displayError("Maximum fixed by jpeg2000 norm is 65535 tiles");
			return false;
		}
		l_nb_tiles = l_cp.tw * l_cp.th;

		/* Define the tiles which will be decoded */
		if (p_j2k.m_decoder.m_discard_tiles) {
			p_j2k.m_decoder.m_start_tile_x = (p_j2k.m_decoder.m_start_tile_x - l_cp.tx0)
					/ l_cp.tdx;
			p_j2k.m_decoder.m_start_tile_y = (p_j2k.m_decoder.m_start_tile_y - l_cp.ty0)
					/ l_cp.tdy;
			p_j2k.m_decoder.m_end_tile_x = ((p_j2k.m_decoder.m_end_tile_x - l_cp.tx0)
					+ l_cp.tdx - 1)
					/ l_cp.tdx;
			p_j2k.m_decoder.m_end_tile_y = ((p_j2k.m_decoder.m_end_tile_y - l_cp.ty0)
					+ l_cp.tdy - 1)
					/ l_cp.tdy;
		} else {
			p_j2k.m_decoder.m_start_tile_x = 0;
			p_j2k.m_decoder.m_start_tile_y = 0;
			p_j2k.m_decoder.m_end_tile_x = l_cp.tw;
			p_j2k.m_decoder.m_end_tile_y = l_cp.th;
		}

		if (useJPWL) {
			if (l_cp.correct) {
				/*
				 * if JPWL is on, we check whether TX errors have damaged too
				 * much the SIZ parameters
				 */
				if ((l_cp.tw < 1) || (l_cp.th < 1)
						|| (l_cp.tw > l_cp.max_tiles)
						|| (l_cp.th > l_cp.max_tiles)) {
					if (JPWLAssume) {
						Preferences.debug("JPWL: bad number of tiles ("
								+ l_cp.tw + " by " + l_cp.th + ")\n",
								Preferences.DEBUG_FILEIO);
					} else {
						MipavUtil.displayError("JPWL: bad number of tiles ("
								+ l_cp.tw + " by " + l_cp.th + ")");
						MipavUtil.displayError("JPWL: giving up");
						return false;
					}

					/* we try to correct */
					Preferences.debug("- trying to adjust them\n",
							Preferences.DEBUG_FILEIO);
					if (l_cp.tw < 1) {
						l_cp.tw = 1;
						Preferences.debug(
								"- setting 1 tile in x => HYPOTHESIS!!!\n",
								Preferences.DEBUG_FILEIO);
					}
					if (l_cp.tw > l_cp.max_tiles) {
						l_cp.tw = 1;
						Preferences.debug(
								"- too large x, increase expectance of "
										+ l_cp.max_tiles + "\n",
								+Preferences.DEBUG_FILEIO);
						Preferences.debug(
								"- setting 1 tile in x => HYPOTHESIS!!!\n",
								Preferences.DEBUG_FILEIO);
					}
					if (l_cp.th < 1) {
						l_cp.th = 1;
						Preferences.debug(
								"- setting 1 tile in y => HYPOTHESIS!!!\n",
								Preferences.DEBUG_FILEIO);
					}
					if (l_cp.th > l_cp.max_tiles) {
						l_cp.th = 1;
						Preferences.debug(
								"- too large y, increase expectance of "
										+ l_cp.max_tiles + " to continue\n",
								Preferences.DEBUG_FILEIO);
						Preferences.debug(
								"- setting 1 tile in y => HYPOTHESIS!!!\n",
								Preferences.DEBUG_FILEIO);
					}
				}
			} // if (l_cp.correct)
		} // if (useJPWL)

		/* memory allocations */
		l_cp.tcps = new opj_tcp_t[l_nb_tiles];
		for (i = 0; i < l_nb_tiles; i++) {
			l_cp.tcps[i] = new opj_tcp_t();
			for (j = 0; j < 32; j++) {
				l_cp.tcps[i].pocs[j] = new opj_poc_t();
			}
		}

		if (useJPWL) {
			if (l_cp.correct) {
				if (l_cp.tcps == null) {
					MipavUtil
							.displayError("JPWL: could not alloc tcps field of cp");
					MipavUtil.displayError("JPWL: giving up");
					return false;
				} // if (l_cp.tcps == null)
			} // if (l_cp.correct)
		} // if (useJPWL)

		p_j2k.m_decoder.m_default_tcp.tccps = new opj_tccp_t[l_image.numcomps];
		for (i = 0; i < l_image.numcomps; i++) {
			p_j2k.m_decoder.m_default_tcp.tccps[i] = new opj_tccp_t();
			for (j = 0; j < OPJ_J2K_MAXBANDS; j++) {
				p_j2k.m_decoder.m_default_tcp.tccps[i].stepsizes[j] = new opj_stepsize_t();
			}
		}

		p_j2k.m_decoder.m_default_tcp.m_mct_records = new opj_mct_data_t[OPJ_J2K_MCT_DEFAULT_NB_RECORDS];
		for (i = 0; i < OPJ_J2K_MCT_DEFAULT_NB_RECORDS; i++) {
			p_j2k.m_decoder.m_default_tcp.m_mct_records[i] = new opj_mct_data_t();
		}

		p_j2k.m_decoder.m_default_tcp.m_nb_max_mct_records = OPJ_J2K_MCT_DEFAULT_NB_RECORDS;

		p_j2k.m_decoder.m_default_tcp.m_mcc_records = new opj_simple_mcc_decorrelation_data_t[OPJ_J2K_MCC_DEFAULT_NB_RECORDS];
		for (i = 0; i < OPJ_J2K_MCC_DEFAULT_NB_RECORDS; i++) {
			p_j2k.m_decoder.m_default_tcp.m_mcc_records[i] = new opj_simple_mcc_decorrelation_data_t();
		}

		p_j2k.m_decoder.m_default_tcp.m_nb_max_mcc_records = OPJ_J2K_MCC_DEFAULT_NB_RECORDS;

		/* set up default dc level shift */
		for (i = 0; i < l_image.numcomps; ++i) {
			if (l_image.comps[i].sgnd == 0) {
				p_j2k.m_decoder.m_default_tcp.tccps[i].m_dc_level_shift = 1 << (l_image.comps[i].prec - 1);
			}
		}

		l_current_tile_param = l_cp.tcps[0];
		for (i = 0; i < l_nb_tiles; ++i) {
			l_current_tile_param.tccps = new opj_tccp_t[l_image.numcomps];
			for (j = 0; j < l_image.numcomps; j++) {
				l_current_tile_param.tccps[j] = new opj_tccp_t();
				for (k = 0; k < OPJ_J2K_MAXBANDS; k++) {
					l_current_tile_param.tccps[j].stepsizes[k] = new opj_stepsize_t();	
				}
			}

			if (i < l_nb_tiles - 1) {
				l_current_tile_param = l_cp.tcps[i + 1];
			}
		}

		p_j2k.m_decoder.m_state = J2K_STATE_MH; /* FIXME J2K_DEC_STATE_MH; */
		opj_image_comp_header_update(l_image, l_cp);

		return true;
	}

	/**
	 * Updates the components characteristics of the image from the coding
	 * parameters.
	 * 
	 * @param p_image_header
	 *            the image header to update.
	 * @param p_cp
	 *            the coding parameters from which to update the image.
	 */
	private void opj_image_comp_header_update(opj_image_t p_image_header,
			opj_cp_t p_cp) {
		int i, l_width, l_height;
		int l_x0, l_y0, l_x1, l_y1;
		int l_comp_x0, l_comp_y0, l_comp_x1, l_comp_y1;
		opj_image_comp_t l_img_comp = null;

		l_x0 = Math.max(p_cp.tx0, p_image_header.x0);
		l_y0 = Math.max(p_cp.ty0, p_image_header.y0);
		l_x1 = p_cp.tx0 + (p_cp.tw - 1) * p_cp.tdx; /*
													 * validity of p_cp members
													 * used here checked in
													 * opj_j2k_read_siz. Can't
													 * overflow.
													 */
		l_y1 = p_cp.ty0 + (p_cp.th - 1) * p_cp.tdy; /* can't overflow */
		l_x1 = Math.min((l_x1 + p_cp.tdx), p_image_header.x1); /*
																 * use add
																 * saturated to
																 * prevent
																 * overflow
																 */
		l_y1 = Math.min((l_y1 + p_cp.tdy), p_image_header.y1); /*
																 * use add
																 * saturated to
																 * prevent
																 * overflow
																 */

		l_img_comp = p_image_header.comps[0];
		for (i = 0; i < p_image_header.numcomps; ++i) {
			l_comp_x0 = (l_x0 + l_img_comp.dx - 1) / l_img_comp.dx;
			l_comp_y0 = (l_y0 + l_img_comp.dy - 1) / l_img_comp.dy;
			l_comp_x1 = (l_x1 + l_img_comp.dx - 1) / l_img_comp.dx;
			l_comp_y1 = (l_y1 + l_img_comp.dy - 1) / l_img_comp.dy;
			l_width = (int) (((l_comp_x1 - l_comp_x0)
					+ (1L << l_img_comp.factor) - 1) >> l_img_comp.factor);
			l_height = (int) (((l_comp_y1 - l_comp_y0)
					+ (1L << l_img_comp.factor) - 1) >> l_img_comp.factor);
			l_img_comp.w = l_width;
			l_img_comp.h = l_height;
			l_img_comp.x0 = l_comp_x0;
			l_img_comp.y0 = l_comp_y0;
			if (i < p_image_header.numcomps - 1) {
				l_img_comp = p_image_header.comps[i + 1];
			}
		}
	}

	/**
	 * Reads a POC marker (Progression Order Change)
	 * 
	 * @param p_header_data
	 *            the data contained in the POC box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the POC marker.
	 */
	private boolean opj_j2k_read_poc(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		int i, l_nb_comp, l_tmp;
		opj_image_t l_image = null;
		int l_old_poc_nb, l_current_poc_nb, l_current_poc_remaining;
		int l_chunk_size, l_comp_room;

		opj_cp_t l_cp = null;
		opj_tcp_t l_tcp = null;
		opj_poc_t l_current_poc = null;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}
		int p_header_offset = 0;

		l_image = p_j2k.m_private_image;
		l_nb_comp = l_image.numcomps;
		if (l_nb_comp <= 256) {
			l_comp_room = 1;
		} else {
			l_comp_room = 2;
		}
		l_chunk_size = 5 + 2 * l_comp_room;
		l_current_poc_nb = p_header_size / l_chunk_size;
		l_current_poc_remaining = p_header_size % l_chunk_size;

		if ((l_current_poc_nb <= 0) || (l_current_poc_remaining != 0)) {
			MipavUtil.displayError("Error reading POC marker");
			return false;
		}

		l_cp = p_j2k.m_cp;
		l_tcp = (p_j2k.m_decoder.m_state == J2K_STATE_TPH) ? l_cp.tcps[p_j2k.m_current_tile_number[0]]
				: p_j2k.m_decoder.m_default_tcp;
		l_old_poc_nb = l_tcp.POC ? l_tcp.numpocs + 1 : 0;
		l_current_poc_nb += l_old_poc_nb;

		if (l_current_poc_nb >= 32) {
			MipavUtil.displayError("Too many POCs have " + l_current_poc_nb);
			return false;
		}

		/* now poc is in use. */
		l_tcp.POC = true;

		l_current_poc = l_tcp.pocs[l_old_poc_nb];
		for (i = l_old_poc_nb; i < l_current_poc_nb; ++i) {
			l_current_poc.resno0 = getUnsignedByte(p_header_data,
					p_header_offset); /* RSpoc_i */
			p_header_offset++;
			if (l_comp_room == 1) {
				l_current_poc.compno0 = getUnsignedByte(p_header_data,
						p_header_offset); /* CSpoc_i */
				p_header_offset++;
			} else if (l_comp_room == 2) {
				l_current_poc.compno0 = getBufferUShort(p_header_data,
						p_header_offset, endianess); /* CSpoc_i */
				p_header_offset += 2;
			}
			l_current_poc.layno1 = getBufferUShort(p_header_data,
					p_header_offset, endianess); /* LYEpoc_i */
			p_header_offset += 2;
			/* make sure layer end is in acceptable bounds */
			l_current_poc.layno1 = Math.min(l_current_poc.layno1,
					l_tcp.numlayers);
			l_current_poc.resno1 = getUnsignedByte(p_header_data,
					p_header_offset); /* REpoc_i */
			p_header_offset++;
			if (l_comp_room == 1) {
				l_current_poc.compno1 = getUnsignedByte(p_header_data,
						p_header_offset); /* CEpoc_i */
				p_header_offset++;
			} else if (l_comp_room == 2) {
				l_current_poc.compno1 = getBufferUShort(p_header_data,
						p_header_offset, endianess); /* CEpoc_i */
				p_header_offset += 2;
			}
			l_tmp = getUnsignedByte(p_header_data, p_header_offset); /* Ppoc_i */
			p_header_offset++;
			l_current_poc.prg = l_tmp;
			/* make sure comp is in acceptable bounds */
			l_current_poc.compno1 = Math.min(l_current_poc.compno1, l_nb_comp);
			if (i < l_current_poc_nb - 1) {
				l_current_poc = l_tcp.pocs[i + 1];
			}
		}

		l_tcp.numpocs = l_current_poc_nb - 1;
		return true;
	}

	/**
	 * Reads a QCC marker (Quantization component)
	 * 
	 * @param p_header_data
	 *            the data contained in the QCC box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the QCC marker.
	 */
	private boolean opj_j2k_read_qcc(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		int l_num_comp, l_comp_no;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}
		int p_header_offset = 0;

		l_num_comp = p_j2k.m_private_image.numcomps;

		if (l_num_comp <= 256) {
			if (p_header_size < 1) {
				MipavUtil
						.displayError("Error reading QCC marker p_header_size < 1");
				return false;
			}
			l_comp_no = getUnsignedByte(p_header_data, 0);
			p_header_offset = 1;
			p_header_size = p_header_size - 1;
		} else {
			if (p_header_size < 2) {
				MipavUtil
						.displayError("Error reading QCC marker p_header_size < 2");
				return false;
			}
			l_comp_no = getBufferUShort(p_header_data, 0, endianess);
			p_header_offset = 2;
			p_header_size -= 2;
		}

		if (useJPWL) {
			if (p_j2k.m_cp.correct) {

				int backup_compno = 0;

				/* compno is negative or larger than the number of components!!! */
				if (/* (l_comp_no < 0) || */(l_comp_no >= l_num_comp)) {
					    if (JPWLAssume) {
						Preferences.debug("JPWL: bad component number in QCC ("
								+ l_comp_no + " out of a maximum of "
								+ l_num_comp + ")\n", Preferences.DEBUG_FILEIO);
					    }
						else {
						MipavUtil
								.displayError("JPWL: bad component number in QCC ("
										+ l_comp_no
										+ " out of a maximum of "
										+ l_num_comp + ")");
						MipavUtil.displayError("JPWL: giving up");
						return false;
					}

					/* we try to correct */
					l_comp_no = backup_compno % l_num_comp;
					Preferences.debug("- trying to adjust this\n",
							Preferences.DEBUG_FILEIO);
					Preferences.debug("- setting component number to "
							+ l_comp_no + "\n", Preferences.DEBUG_FILEIO);
				}

				/* keep your private count of tiles */
				backup_compno++;
			} // if (p_j2k.m_cp.correct)
		} // if (useJPWL)

		if (l_comp_no >= p_j2k.m_private_image.numcomps) {
			MipavUtil.displayError("Invalid component number: " + l_comp_no
					+ ", regarding the number of components "
					+ p_j2k.m_private_image.numcomps);
			return false;
		}

		int header_size[] = new int[]{p_header_size};
		if (!opj_j2k_read_SQcd_SQcc(p_j2k, l_comp_no, p_header_data,
				p_header_offset, header_size)) {
			MipavUtil.displayError("Error reading QCC marker");
			return false;
		}

		if (header_size[0] != 0) {
			MipavUtil.displayError("Error reading QCC marker header_size[0] != 0");
			return false;
		}

		return true;
	}

	/**
	 * Reads a QCD marker (Quantization defaults)
	 * 
	 * @param p_header_data
	 *            the data contained in the QCD box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the QCD marker.
	 */
	private boolean opj_j2k_read_qcd(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		/* preconditions */

		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}
		int p_header_offset = 0;

		int header_size[] = new int[]{p_header_size};
		if (!opj_j2k_read_SQcd_SQcc(p_j2k, 0, p_header_data, p_header_offset,
				header_size)) {
			MipavUtil.displayError("Error reading QCD marker");
			return false;
		}

		if (header_size[0] != 0) {
			MipavUtil.displayError("Error reading QCD marker");
			return false;
		}

		/*
		 * Apply the quantization parameters to other components of the current
		 * tile or the m_default_tcp
		 */
		opj_j2k_copy_tile_quantization_parameters(p_j2k);

		return true;
	}

	private void opj_j2k_copy_tile_quantization_parameters(opj_j2k_t p_j2k) {
		int i, j;
		opj_cp_t l_cp = null;
		opj_tcp_t l_tcp = null;
		opj_tccp_t l_ref_tccp = null;
		opj_tccp_t l_copied_tccp = null;

		/* preconditions */
		if (p_j2k == null) {
			MipavUtil
					.displayError("p_j2k == null at opj_j2k_copy_tile_quantization_parameters");
			return;
		}

		l_cp = p_j2k.m_cp;
		l_tcp = p_j2k.m_decoder.m_state == J2K_STATE_TPH ? l_cp.tcps[p_j2k.m_current_tile_number[0]]
				: p_j2k.m_decoder.m_default_tcp;

		l_ref_tccp = l_tcp.tccps[0];
		if (l_tcp.tccps.length > 1) {
		    l_copied_tccp = l_tcp.tccps[1];
		}

		for (i = 1; i < p_j2k.m_private_image.numcomps; ++i) {
			l_copied_tccp.qntsty = l_ref_tccp.qntsty;
			l_copied_tccp.numgbits = l_ref_tccp.numgbits;
			for (j = 0; j < OPJ_J2K_MAXBANDS; j++) {
				l_copied_tccp.stepsizes[j].expn = l_ref_tccp.stepsizes[j].expn;
				l_copied_tccp.stepsizes[j].mant = l_ref_tccp.stepsizes[j].mant;
			}
			if (i < p_j2k.m_private_image.numcomps - 1) {
				l_copied_tccp = l_tcp.tccps[i + 1];
			}
		}
	}

	/**
	 * Reads a SQcd or SQcc element, i.e. the quantization values of a band in
	 * the QCD or QCC.
	 * 
	 * @param p_j2k
	 *            J2K codec.
	 * @param compno
	 *            the component number to output.
	 * @param p_header_data
	 *            the data buffer.
	 * @param p_header_offset
	 * @param p_header_size
	 *            pointer to the size of the data buffer, it is changed by the
	 *            function.
	 * 
	 */

	private boolean opj_j2k_read_SQcd_SQcc(opj_j2k_t p_j2k, int p_comp_no,
			byte p_header_data[], int p_header_offset, int p_header_size[]) {
		/* loop */
		int l_band_no;
		opj_cp_t l_cp = null;
		opj_tcp_t l_tcp = null;
		opj_tccp_t l_tccp = null;
		int l_tmp, l_num_band;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		l_cp = p_j2k.m_cp;
		/* come from tile part header or main header ? */
		l_tcp = (p_j2k.m_decoder.m_state == J2K_STATE_TPH) ? /*
															 * FIXME
															 * J2K_DEC_STATE_TPH
															 */
		l_cp.tcps[p_j2k.m_current_tile_number[0]]
				: p_j2k.m_decoder.m_default_tcp;

		/* precondition again */
		if (p_comp_no >= p_j2k.m_private_image.numcomps) {
			MipavUtil
					.displayError("Error (p_comp_no >=  p_j2k.m_private_image.numcomps)");
			return false;
		}

		l_tccp = l_tcp.tccps[p_comp_no];

		if (p_header_size[0] < 1) {
			MipavUtil.displayError("Error reading SQcd or SQcc element");
			return false;
		}
		p_header_size[0] -= 1;

		l_tmp = getUnsignedByte(p_header_data, 0); /* Sqcx */
		p_header_offset += 1;

		l_tccp.qntsty = l_tmp & 0x1f;
		l_tccp.numgbits = l_tmp >> 5;
		if (l_tccp.qntsty == J2K_CCP_QNTSTY_SIQNT) {
			l_num_band = 1;
		} else {
			l_num_band = (l_tccp.qntsty == J2K_CCP_QNTSTY_NOQNT) ? (p_header_size[0])
					: (p_header_size[0]) / 2;

			if (l_num_band > OPJ_J2K_MAXBANDS) {
				Preferences
						.debug("While reading CCP_QNTSTY element inside QCD or QCC marker segment, \n",
								Preferences.DEBUG_FILEIO);
				Preferences.debug("number of subbands " + l_num_band
						+ " is greater than OPJ_J2K_MAXBANDS "
						+ OPJ_J2K_MAXBANDS + ".\n", Preferences.DEBUG_FILEIO);
				Preferences.debug(
						"So we limit the number of elements stored to OPJ_J2K_MAXBANDS ("
								+ OPJ_J2K_MAXBANDS + ") and skip the rest.\n",
						Preferences.DEBUG_FILEIO);
				/* return OPJ_FALSE; */
			}
		}

		if (useJPWL) {
			if (l_cp.correct) {

				/* if JPWL is on, we check whether there are too many subbands */
				if (/* (l_num_band < 0) || */(l_num_band >= OPJ_J2K_MAXBANDS)) {
					if (JPWLAssume) {
						Preferences.debug(
								"JPWL: bad number of subbands in Sqcx = "
										+ l_num_band + "\n",
								Preferences.DEBUG_FILEIO);
					} else {
						MipavUtil
								.displayError("JPWL: bad number of subbands in Sqcx = "
										+ l_num_band);
						MipavUtil.displayError("JPWL: giving up");
						return false;
					}

					/* we try to correct */
					l_num_band = 1;
					Preferences.debug("- trying to adjust them\n",
							Preferences.DEBUG_FILEIO);
					Preferences.debug("- setting number of bands to "
							+ l_num_band + " => HYPOTHESIS!!!\n",
							Preferences.DEBUG_FILEIO);
				}

			} // if (l_cp.correct)
		} // if (useJPWL)

		if (l_tccp.qntsty == J2K_CCP_QNTSTY_NOQNT) {
			for (l_band_no = 0; l_band_no < l_num_band; l_band_no++) {
				l_tmp = getUnsignedByte(p_header_data, p_header_offset); /* SPqcx_i */
				p_header_offset += 1;

				if (l_band_no < OPJ_J2K_MAXBANDS) {
					l_tccp.stepsizes[l_band_no].expn = (l_tmp >> 3);
					l_tccp.stepsizes[l_band_no].mant = 0;
				}
			}
			p_header_size[0] = p_header_size[0] - l_num_band;
		} else {
			for (l_band_no = 0; l_band_no < l_num_band; l_band_no++) {
				l_tmp = getBufferUShort(p_header_data, p_header_offset,
						endianess); /* SPqcx_i */
				p_header_offset += 2;
				if (l_band_no < OPJ_J2K_MAXBANDS) {
					l_tccp.stepsizes[l_band_no].expn = (l_tmp >> 11);
					l_tccp.stepsizes[l_band_no].mant = l_tmp & 0x7ff;
				}
			}
			p_header_size[0] = p_header_size[0] - 2 * l_num_band;
		}

		/* Add Antonin : if scalar_derived -> compute other stepsizes */
		if (l_tccp.qntsty == J2K_CCP_QNTSTY_SIQNT) {
			for (l_band_no = 1; l_band_no < OPJ_J2K_MAXBANDS; l_band_no++) {
				l_tccp.stepsizes[l_band_no].expn = ((l_tccp.stepsizes[0].expn)
						- ((l_band_no - 1) / 3) > 0) ? (l_tccp.stepsizes[0].expn)
						- ((l_band_no - 1) / 3)
						: 0;
				l_tccp.stepsizes[l_band_no].mant = l_tccp.stepsizes[0].mant;
			}
		}

		return true;
	}

	/**
	 * Reads a RGN marker (Region Of Interest)
	 * 
	 * @param p_header_data
	 *            the data contained in the POC box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the POC marker.
	 */
	private boolean opj_j2k_read_rgn(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		int l_nb_comp;
		opj_image_t l_image = null;

		opj_cp_t l_cp = null;
		opj_tcp_t l_tcp = null;
		int l_comp_room, l_comp_no;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		l_image = p_j2k.m_private_image;
		l_nb_comp = l_image.numcomps;

		if (l_nb_comp <= 256) {
			l_comp_room = 1;
		} else {
			l_comp_room = 2;
		}

		if (p_header_size != 2 + l_comp_room) {
			MipavUtil
					.displayError("Error reading RGN marker p_header_size != 2 + l_comp_room");
			return false;
		}

		l_cp = p_j2k.m_cp;
		l_tcp = (p_j2k.m_decoder.m_state == J2K_STATE_TPH) ? l_cp.tcps[p_j2k.m_current_tile_number[0]]
				: p_j2k.m_decoder.m_default_tcp;

		if (l_comp_room == 1) {
			l_comp_no = getUnsignedByte(p_header_data, 0); /* Crgn */
		} else {
			l_comp_no = getBufferUShort(p_header_data, 0, endianess); /* Crgn */
		}
		getUnsignedByte(p_header_data, l_comp_room); /* Srgn */

		if (useJPWL) {
			if (l_cp.correct) {
				/* totlen is negative or larger than the bytes left!!! */
				if (l_comp_room >= l_nb_comp) {
					MipavUtil
							.displayError("JPWL: bad component number in RGN ("
									+ l_comp_room + " when there are only "
									+ l_nb_comp);
					MipavUtil.displayError("JPWL: giving up");
					return false;
				} // if (l_comp_room >= l_nb_comp)
			} // if (l_cp.correct)
		} // if (useJPWL)

		/* testcase 3635.pdf.asan.77.2930 */
		if (l_comp_no >= l_nb_comp) {
			MipavUtil.displayError("bad component number in RGN (" + l_comp_no
					+ " when there are only " + l_nb_comp);
			return false;
		}

		l_tcp.tccps[l_comp_no].roishift = getUnsignedByte(p_header_data,
				l_comp_room + 1); /* SPrgn */

		return true;

	}

	/**
	 * Reads a COC marker (Coding Style Component)
	 * 
	 * @param p_header_data
	 *            the data contained in the COC box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the COC marker.
	 */
	private boolean opj_j2k_read_coc(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		opj_cp_t l_cp = null;
		opj_tcp_t l_tcp = null;
		opj_image_t l_image = null;
		int l_comp_room;
		int l_comp_no;
		int p_header_offset;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		l_cp = p_j2k.m_cp;
		l_tcp = (p_j2k.m_decoder.m_state == J2K_STATE_TPH) ? /*
															 * FIXME
															 * J2K_DEC_STATE_TPH
															 */
		l_cp.tcps[p_j2k.m_current_tile_number[0]]
				: p_j2k.m_decoder.m_default_tcp;
		l_image = p_j2k.m_private_image;

		l_comp_room = l_image.numcomps <= 256 ? 1 : 2;

		/* make sure room is sufficient */
		if (p_header_size < l_comp_room + 1) {
			MipavUtil
					.displayError("Error reading COC marker p_header_size[0] < l_comp_room+1");
			return false;
		}
		p_header_size -= l_comp_room + 1;

		if (l_comp_room == 1) {
			l_comp_no = getUnsignedByte(p_header_data, 0); /* Ccoc */
		} else {
			l_comp_no = getBufferUShort(p_header_data, 0, endianess); /* Ccoc */
		}
		if (l_comp_no >= l_image.numcomps) {
			MipavUtil
					.displayError("Error reading COC marker (bad number of components)");
			return false;
		}

		l_tcp.tccps[l_comp_no].csty = getUnsignedByte(p_header_data,
				l_comp_room); /* Scoc */
		p_header_offset = l_comp_room + 1;

		int header_size[] = new int[]{p_header_size};
		if (!opj_j2k_read_SPCod_SPCoc(p_j2k, l_comp_no, p_header_data,
				p_header_offset, header_size)) {
			MipavUtil.displayError("Error reading COC marker");
			return false;
		}

		if (header_size[0] != 0) {
			MipavUtil.displayError("Error reading COC marker");
			return false;
		}
		return true;
	}

	/**
	 * Reads a COD marker (Coding Styke defaults)
	 * 
	 * @param p_header_data
	 *            the data contained in the COD box.
	 * @param p_j2k
	 *            the jpeg2000 codec.
	 * @param p_header_size
	 *            the size of the data contained in the COD marker..
	 */
	private boolean opj_j2k_read_cod(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		/* loop */
		int i;
		int l_tmp;
		opj_cp_t l_cp = null;
		opj_tcp_t l_tcp = null;
		opj_image_t l_image = null;
		int p_header_offset;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		l_image = p_j2k.m_private_image;
		l_cp = p_j2k.m_cp;

		/* If we are in the first tile-part header of the current tile */
		l_tcp = (p_j2k.m_decoder.m_state == J2K_STATE_TPH) ? l_cp.tcps[p_j2k.m_current_tile_number[0]]
				: p_j2k.m_decoder.m_default_tcp;

		/* Only one COD per tile */
		if (l_tcp.cod == 1) {
			MipavUtil
					.displayError("COD marker already read. No more than one COD marker per tile.");
			return false;
		}
		l_tcp.cod = 1;

		/* Make sure room is sufficient */
		if (p_header_size < 5) {
			MipavUtil.displayError("Error p_header_size < 5");
			return false;
		}

		l_tcp.csty = getUnsignedByte(p_header_data, 0); /* Scod */
		/* Make sure we know how to decode this */
		if ((l_tcp.csty & ~(J2K_CP_CSTY_PRT | J2K_CP_CSTY_SOP | J2K_CP_CSTY_EPH)) != 0) {
			MipavUtil.displayError("Unknown Scod value in COD marker");
			return false;
		}
		l_tmp = getUnsignedByte(p_header_data, 1); /* SGcod (A) */

		if ((l_tmp >= 0) && (l_tmp <= 4)) {
			l_tcp.prg = l_tmp;
		}
		/* Make sure progression order is valid */
		else {
			Preferences.debug(
					"Error Unknown progression order in COD marker\n",
					Preferences.DEBUG_FILEIO);
			l_tcp.prg = OPJ_PROG_UNKNOWN;
		}
		l_tcp.numlayers = getBufferUShort(p_header_data, 2, endianess);

		if ((l_tcp.numlayers < 1) || (l_tcp.numlayers > 65535)) {
			MipavUtil.displayError("Invalid number of layers in COD marker : "
					+ l_tcp.numlayers + " not in range [1-65535]");
			return false;
		}

		/*
		 * If user didn't set a number layer to decode take the max specify in
		 * the codestream.
		 */
		if (l_cp.m_dec.m_layer != 0) {
			l_tcp.num_layers_to_decode = l_cp.m_dec.m_layer;
		} else {
			l_tcp.num_layers_to_decode = l_tcp.numlayers;
		}

		l_tcp.mct = getUnsignedByte(p_header_data, 4); /* SGcod (C) */

		p_header_size -= 5;
		p_header_offset = 5;
		for (i = 0; i < l_image.numcomps; ++i) {
			l_tcp.tccps[i].csty = l_tcp.csty & J2K_CCP_CSTY_PRT;
		}

		int header_size[] = new int[]{p_header_size};
		if (!opj_j2k_read_SPCod_SPCoc(p_j2k, 0, p_header_data, p_header_offset,
				header_size)) {
			MipavUtil.displayError("Error reading COD marker");
			return false;
		}

		if (header_size[0] != 0) {
			MipavUtil.displayError("Error reading COD marker");
			return false;
		}

		/*
		 * Apply the coding style to other components of the current tile or the
		 * m_default_tcp
		 */
		opj_j2k_copy_tile_component_parameters(p_j2k);

		/* Index */
		// p_j2k.cstr_info does not exist
		// if (WIP_REMOVE_MSD) {
		// if (p_j2k.cstr_info != null) {
		// /*opj_codestream_info_t *l_cstr_info = p_j2k->cstr_info;*/
		// p_j2k.cstr_info.prog = l_tcp.prg;
		// p_j2k.cstr_info.numlayers = l_tcp.numlayers;
		// p_j2k.cstr_info.numdecompos = new int[l_image.numcomps];
		// for (i = 0; i < l_image.numcomps; ++i) {
		// p_j2k.cstr_info.numdecompos[i] = l_tcp.tccps[i].numresolutions - 1;
		// }
		// }
		// } // if (WIP_REMOVE_MSD)

		return true;
	}

	private void opj_j2k_copy_tile_component_parameters(opj_j2k_t p_j2k) {
		/* loop */
		int i, j;
		opj_cp_t l_cp = null;
		opj_tcp_t l_tcp = null;
		opj_tccp_t l_ref_tccp = null, l_copied_tccp = null;
		int l_prc_size;

		/* preconditions */
		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return;
		}

		l_cp = p_j2k.m_cp;
		l_tcp = (p_j2k.m_decoder.m_state == J2K_STATE_TPH) ? /*
															 * FIXME
															 * J2K_DEC_STATE_TPH
															 */
		l_cp.tcps[p_j2k.m_current_tile_number[0]]
				: p_j2k.m_decoder.m_default_tcp;

		l_ref_tccp = l_tcp.tccps[0];
		if (l_tcp.tccps.length > 1) {
		    l_copied_tccp = l_tcp.tccps[1];
		}
		l_prc_size = l_ref_tccp.numresolutions;

		for (i = 1; i < p_j2k.m_private_image.numcomps; ++i) {
			l_copied_tccp.numresolutions = l_ref_tccp.numresolutions;
			l_copied_tccp.cblkw = l_ref_tccp.cblkw;
			l_copied_tccp.cblkh = l_ref_tccp.cblkh;
			l_copied_tccp.cblksty = l_ref_tccp.cblksty;
			l_copied_tccp.qmfbid = l_ref_tccp.qmfbid;
			for (j = 0; j < l_prc_size; j++) {
				l_copied_tccp.prcw[j] = l_ref_tccp.prcw[j];
				l_copied_tccp.prch[j] = l_ref_tccp.prch[j];
			}
			if (i < p_j2k.m_private_image.numcomps-1) {
			    l_copied_tccp = l_tcp.tccps[i + 1];
			}
		}
	}

	private boolean opj_j2k_read_SPCod_SPCoc(opj_j2k_t p_j2k, int compno,
			byte p_header_data[], int p_header_offset, int p_header_size[]) {
		int i, l_tmp;
		opj_cp_t l_cp = null;
		opj_tcp_t l_tcp = null;
		opj_tccp_t l_tccp = null;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		l_cp = p_j2k.m_cp;
		l_tcp = (p_j2k.m_decoder.m_state == J2K_STATE_TPH) ? l_cp.tcps[p_j2k.m_current_tile_number[0]]
				: p_j2k.m_decoder.m_default_tcp;

		/* precondition again */
		if (compno >= p_j2k.m_private_image.numcomps) {
			MipavUtil.displayError("compno >= p_j2k.m_private_image.numcomps");
			return false;
		}

		l_tccp = l_tcp.tccps[compno];

		/* make sure room is sufficient */
		if (p_header_size[0] < 5) {
			MipavUtil
					.displayError("Error reading SPCod SPCoc element p_header_size[0] < 5");
			return false;
		}

		l_tccp.numresolutions = getUnsignedByte(p_header_data, p_header_offset); /*
																				 * SPcox
																				 * (
																				 * D
																				 * )
																				 */
		p_header_offset++;
		++l_tccp.numresolutions; /* tccp->numresolutions = read() + 1 */
		if (l_tccp.numresolutions > OPJ_J2K_MAXRLVLS) {
			MipavUtil
					.displayError("Invalid value for numresolutions : "
							+ l_tccp.numresolutions
							+ ", max value is set in openjpeg.h at "
							+ OPJ_J2K_MAXRLVLS);
			return false;
		}

		/*
		 * If user wants to remove more resolutions than the codestream
		 * contains, return error
		 */
		if (l_cp.m_dec.m_reduce >= l_tccp.numresolutions) {
			MipavUtil.displayError("Error decoding component " + compno);
			MipavUtil
					.displayError("The number of resolutions to remove is higher than the number of resolutions of this component");
			MipavUtil.displayError("Modify the cp_reduce parameter");
			p_j2k.m_decoder.m_state |= 0x8000;/* FIXME J2K_DEC_STATE_ERR; */
			return false;
		}

		l_tccp.cblkw = getUnsignedByte(p_header_data, p_header_offset); /*
																		 * SPcoc
																		 * (E)
																		 */
		p_header_offset++;
		l_tccp.cblkw += 2;

		l_tccp.cblkh = getUnsignedByte(p_header_data, p_header_offset); /*
																		 * SPcoc
																		 * (F)
																		 */
		p_header_offset++;
		l_tccp.cblkh += 2;

		if ((l_tccp.cblkw > 10) || (l_tccp.cblkh > 10)
				|| ((l_tccp.cblkw + l_tccp.cblkh) > 12)) {
			MipavUtil
					.displayError("Error reading SPCod SPCoc element, Invalid cblkw/cblkh combination");
			return false;
		}

		l_tccp.cblksty = getUnsignedByte(p_header_data, p_header_offset); /*
																		 * SPcoc
																		 * (G)
																		 */
		p_header_offset++;

		l_tccp.qmfbid = getUnsignedByte(p_header_data, p_header_offset); /*
																		 * SPcoc
																		 * (H)
																		 */
		p_header_offset++;

		p_header_size[0] = p_header_size[0] - 5;

		/* use custom precinct size ? */
		if ((l_tccp.csty & J2K_CCP_CSTY_PRT) != 0) {
			if (p_header_size[0] < l_tccp.numresolutions) {
				MipavUtil.displayError("Error reading SPCod SPCoc element");
				return false;
			}

			for (i = 0; i < l_tccp.numresolutions; ++i) {
				l_tmp = getUnsignedByte(p_header_data, p_header_offset); /*
																		 * SPcoc
																		 * (I_i)
																		 */
				p_header_offset++;
				/*
				 * Precinct exponent 0 is only allowed for lowest resolution
				 * level (Table A.21)
				 */
				if ((i != 0) && (((l_tmp & 0xf) == 0) || ((l_tmp >> 4) == 0))) {
					MipavUtil.displayError("Invalid precinct size");
					return false;
				}
				l_tccp.prcw[i] = l_tmp & 0xf;
				l_tccp.prch[i] = l_tmp >> 4;
			}

			p_header_size[0] = p_header_size[0] - l_tccp.numresolutions;
		} else {
			/* set default size for the precinct width and height */
			for (i = 0; i < l_tccp.numresolutions; ++i) {
				l_tccp.prcw[i] = 15;
				l_tccp.prch[i] = 15;
			}
		}

		// p_j2k.cstr_info does not exist
		// if (WIP_REMOVE_MSD) {
		//
		// if (p_j2k.cstr_info != null && compno == 0) {
		// int l_data_size = l_tccp.numresolutions;

		// p_j2k.cstr_info.tile[p_j2k.m_current_tile_number[0]].tccp_info[compno].cblkh
		// = l_tccp.cblkh;
		// p_j2k.cstr_info.tile[p_j2k.m_current_tile_number[0]].tccp_info[compno].cblkw
		// = l_tccp.cblkw;
		// p_j2k.cstr_info.tile[p_j2k.m_current_tile_number[0]].tccp_info[compno].numresolutions
		// = l_tccp.numresolutions;
		// p_j2k.cstr_info.tile[p_j2k.m_current_tile_number[0]].tccp_info[compno].cblksty
		// = l_tccp.cblksty;
		// p_j2k.cstr_info.tile[p_j2k.m_current_tile_number[0]].tccp_info[compno].qmfbid
		// = l_tccp.qmfbid;

		// for (i = 0; i < l_data_size; i++) {
		// p_j2k.cstr_info.tile[p_j2k.m_current_tile_number[0]].pdx[i] =
		// l_tccp.prcw[i];
		// p_j2k.cstr_info.tile[p_j2k.m_current_tile_number[0]].pdy[i] =
		// l_tccp.prch[i];
		// }

		// }

		// } // if (WIP_REMOVE_MSD)

		return true;
	}

	private boolean opj_j2k_read_sot(opj_j2k_t p_j2k, byte p_header_data[],
			int p_header_size) {
		int i;
		opj_cp_t l_cp = null;
		opj_tcp_t l_tcp = null;
		int l_tot_len[] = new int[1];
		int l_num_parts[] = new int[1];
		int l_current_part[] = new int[1];
		;
		int l_tile_x, l_tile_y;

		/* preconditions */
		if (p_header_data == null) {
			MipavUtil.displayError("p_header_data == null");
			return false;
		}

		if (p_j2k == null) {
			MipavUtil.displayError("p_j2k == null");
			return false;
		}

		if (!opj_j2k_get_sot_values(p_header_data, p_header_size,
				p_j2k.m_current_tile_number, l_tot_len, l_current_part,
				l_num_parts)) {
			MipavUtil.displayError("Error reading SOT marker");
			return false;
		}

		l_cp = p_j2k.m_cp;

		/* testcase 2.pdf.SIGFPE.706.1112 */
		if (p_j2k.m_current_tile_number[0] >= l_cp.tw * l_cp.th) {
			MipavUtil.displayError("Invalid tile number = "
					+ p_j2k.m_current_tile_number[0]);
			return false;
		}

		l_tcp = l_cp.tcps[p_j2k.m_current_tile_number[0]];
		l_tile_x = p_j2k.m_current_tile_number[0] % l_cp.tw;
		l_tile_y = p_j2k.m_current_tile_number[0] / l_cp.tw;

		if (useJPWL) {
			if (l_cp.correct) {

				int tileno = p_j2k.m_current_tile_number[0];
				int backup_tileno = 0;

				/* tileno is negative or larger than the number of tiles!!! */
				if (tileno > (l_cp.tw * l_cp.th)) {
					MipavUtil.displayError("JPWL: bad tile number (" + tileno
							+ " out of a maximum of " + (l_cp.tw * l_cp.th)
							+ ")");
					if (!JPWLAssume) {
						MipavUtil.displayError("JPWL: giving up");
						return false;
					}

					/* we try to correct */
					tileno = backup_tileno;
					Preferences.debug("- trying to adjust this\n",
							Preferences.DEBUG_FILEIO);
					Preferences.debug("- setting tile number to " + tileno
							+ "\n", Preferences.DEBUG_FILEIO);
				} // if (tileno > (l_cp.tw * l_cp.th))

				/* keep your private count of tiles */
				backup_tileno++;
			} // if (l_cp.correct)
		} // if (useJPWL)

		/* look for the tile in the list of already processed tile (in parts). */
		/*
		 * Optimization possible here with a more complex data structure and
		 * with the removing of tiles
		 */
		/* since the time taken by this function can only grow at the time */

		/* PSot should be equal to zero or >=14 or <= 2^32-1 */
		if ((l_tot_len[0] != 0) && (l_tot_len[0] < 14)) {
			if (l_tot_len[0] == 12) /*
									 * MSD: Special case for the PHR data which
									 * are read by kakadu
									 */
			{
				Preferences.debug("Empty SOT marker detected: Psot = "
						+ l_tot_len[0] + ".\n", Preferences.DEBUG_FILEIO);
			} else {
				MipavUtil
						.displayError("Psot value is not correct regards to the JPEG2000 norm: "
								+ l_tot_len[0] + ".");
				return false;
			}
		}

		if (useJPWL) {
			if (l_cp.correct) {

				/* totlen is negative or larger than the bytes left!!! */
				if (/* (l_tot_len < 0) || */(l_tot_len[0] > p_header_size)) { /*
																			 * FIXME
																			 * it
																			 * seems
																			 * correct
																			 * ;
																			 * for
																			 * info
																			 * in
																			 * V1
																			 * -
																			 * >
																			 * (
																			 * p_stream_numbytesleft
																			 * (
																			 * p_stream
																			 * )
																			 * +
																			 * 8
																			 * )
																			 * )
																			 * )
																			 * {
																			 */
					MipavUtil.displayError("JPWL: bad tile byte size ("
							+ l_tot_len[0] + " bytes against " + p_header_size
							+ " bytes left)");
					/*
					 * FIXME it seems correct; for info in V1 ->
					 * p_stream_numbytesleft(p_stream) + 8);
					 */
					if (!JPWLAssume) {
						MipavUtil.displayError("JPWL: giving up");
						return false;
					}

					/* we try to correct */
					l_tot_len[0] = 0;
					Preferences.debug("- trying to adjust this\n",
							Preferences.DEBUG_FILEIO);
					Preferences.debug("- setting Psot to " + l_tot_len[0]
							+ " => assuming it is the last tile\n",
							Preferences.DEBUG_FILEIO);
				}
			} // if (l_cp.correct)
		} // if (useJPWL)

		/*
		 * Ref A.4.2: Psot could be equal zero if it is the last tile-part of
		 * the codestream.
		 */
		if (l_tot_len[0] == 0) {
			Preferences.debug(
					"Psot value of the current tile-part is equal to zero,\n",
					Preferences.DEBUG_FILEIO);
			Preferences
					.debug("we assuming it is the last tile-part of the codestream.\n",
							Preferences.DEBUG_FILEIO);
			p_j2k.m_decoder.m_last_tile_part = 1;
		}

		if (l_num_parts[0] != 0) { /*
									 * Number of tile-part header is provided by
									 * this tile-part header
									 */
			l_num_parts[0] += p_j2k.m_decoder.m_nb_tile_parts_correction;
			/*
			 * Useful to manage the case of textGBR.jp2 file because two values
			 * of TNSot are allowed: the correct numbers of tile-parts for that
			 * tile and zero (A.4.2 of 15444-1 : 2002).
			 */
			if (l_tcp.m_nb_tile_parts != 0) {
				if (l_current_part[0] >= l_tcp.m_nb_tile_parts) {
					MipavUtil.displayError("In SOT marker, TPSot ("
							+ l_current_part[0]
							+ ") is not valid regards to the current ");
					MipavUtil.displayError("number of tile-part ("
							+ l_tcp.m_nb_tile_parts + "), giving up");
					p_j2k.m_decoder.m_last_tile_part = 1;
					return false;
				}
			}
			if (l_current_part[0] >= l_num_parts[0]) {
				/* testcase 451.pdf.SIGSEGV.ce9.3723 */
				MipavUtil.displayError("In SOT marker, TPSot ("
						+ l_current_part[0]
						+ ") is not valid regards to the current");
				MipavUtil.displayError("number of tile-part (header) ("
						+ l_num_parts[0] + "), giving up\n");
				p_j2k.m_decoder.m_last_tile_part = 1;
				return false;
			}
			l_tcp.m_nb_tile_parts = l_num_parts[0];
		}

		/*
		 * If know the number of tile part header we will check if we didn't
		 * read the last
		 */
		if (l_tcp.m_nb_tile_parts != 0) {
			if (l_tcp.m_nb_tile_parts == (l_current_part[0] + 1)) {
				p_j2k.m_decoder.m_can_decode = true; /*
													 * Process the last
													 * tile-part header
													 */
			}
		}

		if (p_j2k.m_decoder.m_last_tile_part == 0) {
			/* Keep the size of data to skip after this marker */
			p_j2k.m_decoder.m_sot_length = l_tot_len[0] - 12; /*
															 * SOT_marker_size =
															 * 12
															 */
		} else {
			/*
			 * FIXME: need to be computed from the number of bytes remaining in
			 * the codestream
			 */
			p_j2k.m_decoder.m_sot_length = 0;
		}

		p_j2k.m_decoder.m_state = J2K_STATE_TPH;

		/*
		 * Check if the current tile is outside the area we want decode or not
		 * corresponding to the tile index
		 */
		if (p_j2k.m_decoder.m_tile_ind_to_dec == -1) {
			p_j2k.m_decoder.m_skip_data = (l_tile_x < p_j2k.m_decoder.m_start_tile_x)
					|| (l_tile_x >= p_j2k.m_decoder.m_end_tile_x)
					|| (l_tile_y < p_j2k.m_decoder.m_start_tile_y)
					|| (l_tile_y >= p_j2k.m_decoder.m_end_tile_y);
		} else {
			if (p_j2k.m_decoder.m_tile_ind_to_dec < 0) {
				MipavUtil.displayError("p_j2k.m_decoder.m_tile_ind_to_dec < 0");
				return false;
			}
			p_j2k.m_decoder.m_skip_data = (p_j2k.m_current_tile_number[0] != p_j2k.m_decoder.m_tile_ind_to_dec);
		}

		/* Index */
		if (p_j2k.cstr_index != null) {
			if (p_j2k.cstr_index.tile_index == null) {
				MipavUtil.displayError("p_j2k.cstr_index.tile_index == null");
				return false;
			}
			p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tileno = p_j2k.m_current_tile_number[0];
			p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].current_tpsno = l_current_part[0];

			if (l_num_parts[0] != 0) {
				p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].nb_tps = l_num_parts[0];
				p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].current_nb_tps = l_num_parts[0];

				if (p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index == null) {
					p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index = new opj_tp_index_t[l_num_parts[0]];
					for (i = 0; i < l_num_parts[0]; i++) {
						p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index[i] = new opj_tp_index_t();	
					}
				} else {
					opj_tp_index_t new_tp_index[] = new opj_tp_index_t[l_num_parts[0]];
					int len = p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index.length;
					for (i = 0; i < len; i++) {
						new_tp_index[i] = p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index[i];
					}
					for (i = len; i < l_num_parts[0]; i++) {
						new_tp_index[i] = new opj_tp_index_t();
					}
					p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index = new_tp_index;
				}
			} else {
				/*
				 * if
				 * (!p_j2k->cstr_index->tile_index[p_j2k->m_current_tile_number
				 * ].tp_index)
				 */{

					if (p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index == null) {
						p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].current_nb_tps = 10;
						p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index = new opj_tp_index_t[p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].current_nb_tps];
                        for (i = 0; i < p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].current_nb_tps; i++) {
                        	p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index[i] = new opj_tp_index_t();	
                        }
					}

					if (l_current_part[0] >= p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].current_nb_tps) {
						opj_tp_index_t new_tp_index[];
						p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].current_nb_tps = l_current_part[0] + 1;
						new_tp_index = new opj_tp_index_t[p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].current_nb_tps];
						int len = p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index.length;
						for (i = 0; i < len; i++) {
							new_tp_index[i] = p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index[i];
						}
						for (i = len; i < p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].current_nb_tps; i++) {
							new_tp_index[i] = new opj_tp_index_t();
						}
						p_j2k.cstr_index.tile_index[p_j2k.m_current_tile_number[0]].tp_index = new_tp_index;
					}
				}

			}

		}

		/*
		 * FIXME move this onto a separate method to call before reading any
		 * SOT, remove part about main_end header, use a index struct inside
		 * p_j2k
		 */
		/*
		 * if (p_j2k->cstr_info) { if (l_tcp->first) { if (tileno == 0) {
		 * p_j2k->cstr_info->main_head_end = p_stream_tell(p_stream) - 13; }
		 * 
		 * p_j2k->cstr_info->tile[tileno].tileno = tileno;
		 * p_j2k->cstr_info->tile[tileno].start_pos = p_stream_tell(p_stream) -
		 * 12; p_j2k->cstr_info->tile[tileno].end_pos =
		 * p_j2k->cstr_info->tile[tileno].start_pos + totlen - 1;
		 * p_j2k->cstr_info->tile[tileno].num_tps = numparts;
		 * 
		 * if (numparts) { p_j2k->cstr_info->tile[tileno].tp = (opj_tp_info_t *)
		 * opj_malloc(numparts * sizeof(opj_tp_info_t)); } else {
		 * p_j2k->cstr_info->tile[tileno].tp = (opj_tp_info_t *) opj_malloc(10 *
		 * sizeof(opj_tp_info_t)); // Fixme (10) } } else {
		 * p_j2k->cstr_info->tile[tileno].end_pos += totlen; }
		 * 
		 * p_j2k->cstr_info->tile[tileno].tp[partno].tp_start_pos =
		 * p_stream_tell(p_stream) - 12;
		 * p_j2k->cstr_info->tile[tileno].tp[partno].tp_end_pos =
		 * p_j2k->cstr_info->tile[tileno].tp[partno].tp_start_pos + totlen - 1;
		 * }
		 */
		return true;
	}

	private boolean opj_j2k_get_sot_values(byte p_header_data[],
			int p_header_size, int p_tile_no[], int p_tot_len[],
			int p_current_part[], int p_num_parts[]) {

		/*
		 * Size of this marker is fixed = 12 (we have already read marker and
		 * its size)
		 */
		if (p_header_size != 8) {
			MipavUtil.displayError("Error reading SOT marker");
			return false;
		}

		p_tile_no[0] = getBufferUShort(p_header_data, 0, endianess);
		p_tot_len[0] = getBufferInt(p_header_data, 2, endianess);
		p_current_part[0] = getUnsignedByte(p_header_data, 6);
		p_num_parts[0] = getUnsignedByte(p_header_data, 7);
		return true;
	}

	private boolean opj_j2k_read_unk(opj_j2k_t p_j2k,
			RandomAccessFile p_stream, int output_marker[]) {
		int l_unknown_marker;
		opj_dec_memory_marker_handler_t l_marker_handler;
		int l_size_unk = 2;
		int bytesRead;
		long filePointer = 0;

		Preferences.debug("Unknown marker\n", Preferences.DEBUG_FILEIO);

		for (;;) {
			/*
			 * Try to read 2 bytes (the next marker ID) from stream and copy
			 * them into the buffer
			 */
			try {
				bytesRead = p_stream.read(p_j2k.m_decoder.m_header_data, 0, 2);
			} catch (IOException e) {
				MipavUtil
						.displayError("IOException " + e + " on p_stream.read");
				return false;
			}
			if (bytesRead != 2) {
				MipavUtil.displayError("RandomAccessFile too short");
				return false;
			}

			/* read 2 bytes as the new marker ID */
			l_unknown_marker = getBufferUShort(p_j2k.m_decoder.m_header_data,
					0, endianess);

			if (!(l_unknown_marker < 0xff00)) {

				/* Get the marker handler from the marker ID */
				l_marker_handler = opj_j2k_get_marker_handler(l_unknown_marker);

				if ((p_j2k.m_decoder.m_state & l_marker_handler.states) == 0) {
					MipavUtil
							.displayError("Marker is not compliant with its position");
					return false;
				} else {
					if (l_marker_handler.id != J2K_MS_UNK) {
						/* Add the marker to the codestream index */
						if (l_marker_handler.id != J2K_MS_SOT) {
							try {
								filePointer = p_stream.getFilePointer();
							} catch (IOException e) {
								MipavUtil.displayError("IOException " + e
										+ " on p_stream.getFilePointer()");
								return false;
							}
							boolean res = opj_j2k_add_mhmarker(
									p_j2k.cstr_index, J2K_MS_UNK, filePointer
											- l_size_unk, l_size_unk);
							if (res == false) {
								MipavUtil
										.displayError("Not enough memory to add mh marker");
								return false;
							}
						}
						break; /* next marker is known and well located */
					} else
						l_size_unk += 2;
				}
			}
		}

		output_marker[0] = l_marker_handler.id;

		return true;
	}

	private opj_dec_memory_marker_handler_t opj_j2k_get_marker_handler(int p_id) {
		int i;
		for (i = 0; j2k_memory_marker_handler_tab[i].id != 0; i++) {
			if (j2k_memory_marker_handler_tab[i].id == p_id) {
				break;
			}
		}
		return j2k_memory_marker_handler_tab[i];
	}

	/**
	 * Reads a SOC marker (Start of Codestream)
	 * 
	 * @param p_j2k
	 *            the jpeg2000 file codec.
	 * @param p_stream
	 *            FIXME DOC
	 */
	private boolean opj_j2k_read_soc(opj_j2k_t p_j2k, RandomAccessFile p_stream) {
		byte l_data[] = new byte[2];
		int l_marker;
		int bytesRead;
		long filePointer;

		try {
			bytesRead = p_stream.read(l_data);
		} catch (IOException e) {
			MipavUtil.displayError("IOException " + e + " on p_stream.read");
			return false;
		}
		if (bytesRead != 2) {
			return false;
		}

		l_marker = getBufferUShort(l_data, 0, endianess);
		if (l_marker != J2K_MS_SOC) {
			return false;
		}

		/* Next marker should be a SIZ marker in the main header */
		p_j2k.m_decoder.m_state = J2K_STATE_MHSIZ;

		try {
			filePointer = p_stream.getFilePointer();
		} catch (IOException e) {
			MipavUtil.displayError("IOException " + e
					+ " on p_stream.getFilePointer()");
			return false;
		}

		/* FIXME move it in a index structure included in p_j2k */
		p_j2k.cstr_index.main_head_start = filePointer - 2;

		Preferences.debug("Start to read j2k main header at "
				+ p_j2k.cstr_index.main_head_start + "\n",
				Preferences.DEBUG_FILEIO);

		/* Add the marker to the codestream index */
		if (false == opj_j2k_add_mhmarker(p_j2k.cstr_index, J2K_MS_SOC,
				p_j2k.cstr_index.main_head_start, 2)) {
			MipavUtil.displayError("Not enough memory to add mh marker");
			return false;
		}
		return true;
	}

	private boolean opj_j2k_add_mhmarker(opj_codestream_index_t cstr_index,
			int type, long pos, int len) {
        int i;
		/* expand the list? */
		if ((cstr_index.marknum + 1) > cstr_index.maxmarknum) {
			opj_marker_info_t new_marker[];
			cstr_index.maxmarknum = (100 + cstr_index.maxmarknum);
			new_marker = new opj_marker_info_t[cstr_index.maxmarknum];
			for (i = 0; i < cstr_index.marker.length; i++) {
				new_marker[i] = cstr_index.marker[i];
			}
			for (i = cstr_index.marker.length; i < cstr_index.maxmarknum; i++) {
				new_marker[i] = new opj_marker_info_t();
			}
			cstr_index.marker = new_marker;
		}

		/* add the marker */
		cstr_index.marker[cstr_index.marknum].type = (short) type;
		cstr_index.marker[cstr_index.marknum].pos = pos;
		cstr_index.marker[cstr_index.marknum].len = len;
		cstr_index.marknum++;
		return true;
	}

	private int get_next_file(int imageno, dircnt_t dirptr, img_fol_t img_fol,
			opj_decompress_parameters parameters) {
		String image_filename;
		String infilename;
		String outfilename;
		String temp_ofname;

		image_filename = new String(dirptr.filename[imageno]);
		Preferences.debug("File number = " + imageno + " image_filename = "
				+ image_filename + "\n", Preferences.DEBUG_FILEIO);
		infilename = new String(img_fol.imgdirpath + File.separator
				+ image_filename);
		parameters.decod_format = infile_format(infilename);
		if (parameters.decod_format == -1)
			return 1;
		parameters.infile = new String(infilename);

		/* Set output file */
		int index = image_filename.lastIndexOf(".");
		// Include period
		temp_ofname = image_filename.substring(0, index + 1);
		if (img_fol.set_out_format == 1) {
			outfilename = new String(img_fol.imgdirpath + File.separator
					+ temp_ofname + img_fol.out_format);
			parameters.outfile = new String(outfilename);
		}

		return 0;
	}

	private int load_images(dircnt_t dirptr, String imgdirpath) {
		int i = 0;

		/* Reading the input images from given input directory */

		File folder = new File(imgdirpath);

		if (folder.isFile()) {
			MipavUtil.displayError(imgdirpath + " is a nondirectory file");
			return 1;
		}

		File[] listofFiles = folder.listFiles();
		for (File file : listofFiles) {
			if (file.isFile()) {
				dirptr.filename[i++] = new String(file.getName());
			}
		}
		return 0;
	}

	private int get_num_images(String imgdirpath) {
		int num_images = 0;
		File folder = new File(imgdirpath);

		if (folder.isFile()) {
			MipavUtil.displayError(imgdirpath + " is a nondirectory file");
			return 0;
		}

		File[] listofFiles = folder.listFiles();
		for (File file : listofFiles) {
			if (file.isFile()) {
				num_images++;
			}
		}

		return num_images;
	}

	private void destroy_parameters(opj_decompress_parameters parameters) {
		if (parameters != null) {
			if (parameters.precision != null) {
				parameters.precision = null;
			}
		}
	}

	int get_file_format(String filename) {
		int i;
		String extension[] = { "pgx", "pnm", "pgm", "ppm", "bmp", "tif", "raw",
				"rawl", "tga", "png", "j2k", "jp2", "jpt", "j2c", "jpc" };
		int format[] = { PGX_DFMT, PXM_DFMT, PXM_DFMT, PXM_DFMT, BMP_DFMT,
				TIF_DFMT, RAW_DFMT, RAWL_DFMT, TGA_DFMT, PNG_DFMT, J2K_CFMT,
				JP2_CFMT, JPT_CFMT, J2K_CFMT, J2K_CFMT };
		int ext = filename.lastIndexOf('.');
		if (ext == -1 || ext == filename.length() - 1)
			return -1;

		String suffix = filename.substring(ext + 1);
		for (i = 0; i < format.length; i++) {
			if (suffix.equalsIgnoreCase(extension[i])) {
				return format[i];
			}
		}

		return -1;
	}

	private int infile_format(String fname) {
		File reader;
		RandomAccessFile raFile;
		String s, magic_s;
		int ext_format, magic_format;
		byte buf[] = new byte[12];
		byte buf4[] = new byte[4];
		int l_nb_read;
		int i;
		byte JP2_RFC3745_MAGIC[] = new byte[] { (byte) 0x00, (byte) 0x00,
				(byte) 0x00, (byte) 0x0c, (byte) 0x6a, (byte) 0x50,
				(byte) 0x20, (byte) 0x20, (byte) 0x0d, (byte) 0x0a,
				(byte) 0x87, (byte) 0x0a };
		byte JP2_MAGIC[] = new byte[] { (byte) 0x0d, (byte) 0x0a, (byte) 0x87,
				(byte) 0x0a };
		/* position 45: "\xff\x52" */
		byte J2K_CODESTREAM_MAGIC[] = new byte[] { (byte) 0xff, (byte) 0x4f,
				(byte) 0xff, (byte) 0x51 };

		reader = new File(fname);

		try {
			raFile = new RandomAccessFile(reader, "r");
		} catch (FileNotFoundException e) {
			MipavUtil.displayError("File not found exception " + e);
			return -2;
		}

		try {
			l_nb_read = raFile.read(buf, 0, 12);
		} catch (IOException e) {
			MipavUtil.displayError("IOException " + e + " on raFile.read");
			try {
				raFile.close();
			} catch (IOException e2) {
				MipavUtil.displayError("IOException " + e
						+ " on raFile.close()");
			}
			return -1;
		}
		try {
			raFile.close();
		} catch (IOException e) {
			MipavUtil.displayError("IOException " + e + " on raFile.close()");
			return -1;
		}
		if (l_nb_read != 12)
			return -1;

		for (i = 0; i < 4; i++) {
			buf4[i] = buf[i];
		}

		ext_format = get_file_format(fname);

		if (ext_format == JPT_CFMT)
			return JPT_CFMT;

		if (Arrays.equals(buf, JP2_RFC3745_MAGIC)
				|| Arrays.equals(buf4, JP2_MAGIC)) {
			magic_format = JP2_CFMT;
			magic_s = ".jp2";
		} else if (Arrays.equals(buf4, J2K_CODESTREAM_MAGIC)) {
			magic_format = J2K_CFMT;
			magic_s = ".j2k or .jpc or .j2c";
		} else
			return -1;

		if (magic_format == ext_format)
			return ext_format;

		s = fname.substring(fname.length() - 4);

		System.err.println("\n===========================================");

		System.err.println("The extension of this file is incorrect.");
		System.err.println("FOUND " + s + ". SHOULD BE " + magic_s);
		System.err.println("===========================================");

		return magic_format;
	}

	private RandomAccessFile opj_stream_create_default_file_stream(
			String fname, boolean p_is_read_stream) {

		RandomAccessFile l_stream;
		File p_file;
		String mode;

		if (fname == null) {
			return null;
		}

		if (p_is_read_stream)
			mode = "r";
		else
			mode = "rw";

		p_file = new File(fname);

		try {
			l_stream = new RandomAccessFile(p_file, mode);
		} catch (FileNotFoundException e) {
			MipavUtil.displayError("File not found exception " + e);
			return null;
		}

		return l_stream;
	}

	private void set_default_parameters(opj_decompress_parameters parameters) {
		if (parameters != null) {

			/* default decoding parameters (command line specific) */
			parameters.decod_format = -1;
			parameters.cod_format = -1;

			/* default decoding parameters (core) */
			opj_set_default_decoder_parameters(parameters.core);
		}
	}

	private void opj_set_default_decoder_parameters(opj_dparameters_t parameters) {
		if (parameters != null) {
			/* default decoding parameters */
			parameters.cp_layer = 0;
			parameters.cp_reduce = 0;

			parameters.decod_format = -1;
			parameters.cod_format = -1;
			parameters.flags = 0;
			if (useJPWL) {
				parameters.jpwl_correct = false;
				parameters.jpwl_exp_comps = JPWL_EXPECTED_COMPONENTS;
				parameters.jpwl_max_tiles = JPWL_MAXIMUM_TILES;
			}
		}
	}

	// opj_codec_t* opj_create_decompress(OPJ_CODEC_FORMAT p_format)
	// {
	// opj_codec_private_t *l_codec = 00;

	// l_codec = (opj_codec_private_t*) opj_calloc(1,
	// sizeof(opj_codec_private_t));
	// if (!l_codec){
	// return 00;
	// }

	// l_codec->is_decompressor = 1;

	// switch (p_format) {
	// case OPJ_CODEC_J2K:
	// l_codec->opj_dump_codec = (void (*) (void*, OPJ_INT32, FILE*)) j2k_dump;

	// l_codec->opj_get_codec_info = (opj_codestream_info_v2_t* (*) (void*) )
	// j2k_get_cstr_info;

	// l_codec->opj_get_codec_index = (opj_codestream_index_t* (*) (void*) )
	// j2k_get_cstr_index;

	// l_codec->m_codec_data.m_decompression.opj_decode =
	// (OPJ_BOOL (*) ( void *,
	// struct opj_stream_private *,
	// opj_image_t*, struct opj_event_mgr * )) opj_j2k_decode;

	// l_codec->m_codec_data.m_decompression.opj_end_decompress =
	// (OPJ_BOOL (*) ( void *,
	// struct opj_stream_private *,
	// struct opj_event_mgr *)) opj_j2k_end_decompress;

	// l_codec->m_codec_data.m_decompression.opj_read_header =
	// (OPJ_BOOL (*) ( struct opj_stream_private *,
	// void *,
	// opj_image_t **,
	// struct opj_event_mgr * )) opj_j2k_read_header;

	// l_codec->m_codec_data.m_decompression.opj_destroy =
	// (void (*) (void *))opj_j2k_destroy;

	// l_codec->m_codec_data.m_decompression.opj_setup_decoder =
	// (void (*) (void * , opj_dparameters_t * )) opj_j2k_setup_decoder;

	// l_codec->m_codec_data.m_decompression.opj_read_tile_header =
	// (OPJ_BOOL (*) ( void *,
	// OPJ_UINT32*,
	// OPJ_UINT32*,
	// OPJ_INT32*, OPJ_INT32*,
	// OPJ_INT32*, OPJ_INT32*,
	// OPJ_UINT32*,
	// OPJ_BOOL*,
	// struct opj_stream_private *,
	// struct opj_event_mgr * )) opj_j2k_read_tile_header;

	// l_codec->m_codec_data.m_decompression.opj_decode_tile_data =
	// (OPJ_BOOL (*) ( void *,
	// OPJ_UINT32,
	// OPJ_BYTE*,
	// OPJ_UINT32,
	// struct opj_stream_private *,
	// struct opj_event_mgr *)) opj_j2k_decode_tile;

	// l_codec->m_codec_data.m_decompression.opj_set_decode_area =
	// (OPJ_BOOL (*) ( void *,
	// opj_image_t*,
	// OPJ_INT32, OPJ_INT32, OPJ_INT32, OPJ_INT32,
	// struct opj_event_mgr *)) opj_j2k_set_decode_area;

	// l_codec->m_codec_data.m_decompression.opj_get_decoded_tile =
	// (OPJ_BOOL (*) ( void *p_codec,
	// opj_stream_private_t *p_cio,
	// opj_image_t *p_image,
	// struct opj_event_mgr * p_manager,
	// OPJ_UINT32 tile_index)) opj_j2k_get_tile;

	// l_codec->m_codec_data.m_decompression.opj_set_decoded_resolution_factor =
	// (OPJ_BOOL (*) ( void * p_codec,
	// OPJ_UINT32 res_factor,
	// struct opj_event_mgr * p_manager)) opj_j2k_set_decoded_resolution_factor;

	// l_codec->m_codec = opj_j2k_create_decompress();

	// if (! l_codec->m_codec) {
	// opj_free(l_codec);
	// return NULL;
	// }

	// break;

	// case OPJ_CODEC_JP2:
	// /* get a JP2 decoder handle */
	// l_codec->opj_dump_codec = (void (*) (void*, OPJ_INT32, FILE*)) jp2_dump;

	// l_codec->opj_get_codec_info = (opj_codestream_info_v2_t* (*) (void*) )
	// jp2_get_cstr_info;

	// l_codec->opj_get_codec_index = (opj_codestream_index_t* (*) (void*) )
	// jp2_get_cstr_index;

	// l_codec->m_codec_data.m_decompression.opj_decode =
	// (OPJ_BOOL (*) ( void *,
	// struct opj_stream_private *,
	// opj_image_t*,
	// struct opj_event_mgr * )) opj_jp2_decode;

	// l_codec->m_codec_data.m_decompression.opj_end_decompress =
	// (OPJ_BOOL (*) ( void *,
	// struct opj_stream_private *,
	// struct opj_event_mgr *)) opj_jp2_end_decompress;

	// l_codec->m_codec_data.m_decompression.opj_read_header =
	// (OPJ_BOOL (*) ( struct opj_stream_private *,
	// void *,
	// opj_image_t **,
	// struct opj_event_mgr * )) opj_jp2_read_header;

	// l_codec->m_codec_data.m_decompression.opj_read_tile_header =
	// (OPJ_BOOL (*) ( void *,
	// OPJ_UINT32*,
	// OPJ_UINT32*,
	// OPJ_INT32*,
	// OPJ_INT32*,
	// OPJ_INT32 * ,
	// OPJ_INT32 * ,
	// OPJ_UINT32 * ,
	// OPJ_BOOL *,
	// struct opj_stream_private *,
	// struct opj_event_mgr * )) opj_jp2_read_tile_header;

	// l_codec->m_codec_data.m_decompression.opj_decode_tile_data =
	// (OPJ_BOOL (*) ( void *,
	// OPJ_UINT32,OPJ_BYTE*,OPJ_UINT32,
	// struct opj_stream_private *,
	// struct opj_event_mgr * )) opj_jp2_decode_tile;

	// l_codec->m_codec_data.m_decompression.opj_destroy = (void (*) (void
	// *))opj_jp2_destroy;

	// l_codec->m_codec_data.m_decompression.opj_setup_decoder =
	// (void (*) (void * ,opj_dparameters_t * )) opj_jp2_setup_decoder;

	// l_codec->m_codec_data.m_decompression.opj_set_decode_area =
	// (OPJ_BOOL (*) ( void *,
	// opj_image_t*,
	// OPJ_INT32,OPJ_INT32,OPJ_INT32,OPJ_INT32,
	// struct opj_event_mgr * )) opj_jp2_set_decode_area;

	// l_codec->m_codec_data.m_decompression.opj_get_decoded_tile =
	// (OPJ_BOOL (*) ( void *p_codec,
	// opj_stream_private_t *p_cio,
	// opj_image_t *p_image,
	// struct opj_event_mgr * p_manager,
	// OPJ_UINT32 tile_index)) opj_jp2_get_tile;

	// l_codec->m_codec_data.m_decompression.opj_set_decoded_resolution_factor =
	// (OPJ_BOOL (*) ( void * p_codec,
	// OPJ_UINT32 res_factor,
	// opj_event_mgr_t * p_manager)) opj_jp2_set_decoded_resolution_factor;

	// l_codec->m_codec = opj_jp2_create(OPJ_TRUE);

	// if (! l_codec->m_codec) {
	// opj_free(l_codec);
	// return 00;
	// }

	// break;
	// case OPJ_CODEC_UNKNOWN:
	// case OPJ_CODEC_JPT:
	// default:
	// opj_free(l_codec);
	// return 00;
	// }

	// opj_set_default_event_handler(&(l_codec->m_event_mgr));
	// return (opj_codec_t*) l_codec;
	// }

	public String hex(int n) {
		// call toUpperCase() if that's required
		return String.format("0x%8s", Integer.toHexString(n)).replace(' ', '0');
	}

	private boolean opj_j2k_get_tile(opj_j2k_t p_j2k,
			RandomAccessFile p_stream, opj_image_t p_image, int tile_index) {
		int compno;
		int l_tile_x, l_tile_y;
		opj_image_comp_t l_img_comp;

		if (p_image == null) {
			MipavUtil.displayError("We need an image previously created.");
			return false;
		}

		if ( /* (tile_index < 0) && */(tile_index >= p_j2k.m_cp.tw
				* p_j2k.m_cp.th)) {
			MipavUtil
					.displayError("Tile index provided by the user is incorrect "
							+ tile_index
							+ " (max = "
							+ ((p_j2k.m_cp.tw * p_j2k.m_cp.th) - 1) + ")");
			return false;
		}

		/* Compute the dimension of the desired tile */
		l_tile_x = tile_index % p_j2k.m_cp.tw;
		l_tile_y = tile_index / p_j2k.m_cp.tw;

		p_image.x0 = l_tile_x * p_j2k.m_cp.tdx + p_j2k.m_cp.tx0;
		if (p_image.x0 < p_j2k.m_private_image.x0)
			p_image.x0 = p_j2k.m_private_image.x0;
		p_image.x1 = (l_tile_x + 1) * p_j2k.m_cp.tdx + p_j2k.m_cp.tx0;
		if (p_image.x1 > p_j2k.m_private_image.x1)
			p_image.x1 = p_j2k.m_private_image.x1;

		p_image.y0 = l_tile_y * p_j2k.m_cp.tdy + p_j2k.m_cp.ty0;
		if (p_image.y0 < p_j2k.m_private_image.y0)
			p_image.y0 = p_j2k.m_private_image.y0;
		p_image.y1 = (l_tile_y + 1) * p_j2k.m_cp.tdy + p_j2k.m_cp.ty0;
		if (p_image.y1 > p_j2k.m_private_image.y1)
			p_image.y1 = p_j2k.m_private_image.y1;

		l_img_comp = p_image.comps[0];
		for (compno = 0; compno < p_image.numcomps; ++compno) {
			int l_comp_x1, l_comp_y1;

			l_img_comp.factor = p_j2k.m_private_image.comps[compno].factor;

			l_img_comp.x0 = (p_image.x0 + l_img_comp.dx - 1) / l_img_comp.dx;
			l_img_comp.y0 = (p_image.y0 + l_img_comp.dy - 1) / l_img_comp.dy;
			l_comp_x1 = (p_image.x1 + l_img_comp.dx - 1) / l_img_comp.dx;
			l_comp_y1 = (p_image.y1 + l_img_comp.dy - 1) / l_img_comp.dy;

			l_img_comp.w = (int) (((l_comp_x1 + (1L << l_img_comp.factor) - 1) >> l_img_comp.factor) - ((l_img_comp.x0
					+ (1L << l_img_comp.factor) - 1) >> l_img_comp.factor));
			l_img_comp.h = (int) (((l_comp_y1 + (1L << l_img_comp.factor) - 1) >> l_img_comp.factor) - ((l_img_comp.y0
					+ (1L << l_img_comp.factor) - 1) >> l_img_comp.factor));

			if (compno < p_image.numcomps - 1) {
				l_img_comp = p_image.comps[compno + 1];
			}
		}

		/* Destroy the previous output image */
		if (p_j2k.m_output_image != null)
			opj_image_destroy(p_j2k.m_output_image);

		/* Create the ouput image from the information previously computed */
		p_j2k.m_output_image = new opj_image_t();
		if (p_j2k.m_output_image == null) {
			return false;
		}
		opj_copy_image_header(p_image, p_j2k.m_output_image);

		p_j2k.m_decoder.m_tile_ind_to_dec = tile_index;

		/* customization of the decoding */
		// opj_j2k_setup_decoding_tile(p_j2k);

		/* Decode the codestream */
		if (!opj_j2k_decode_one_tile(p_j2k, p_stream)) {
			opj_image_destroy(p_j2k.m_private_image);
			p_j2k.m_private_image = null;
			return false;
		}

		/* Move data and copy one information from codec to output image */
		for (compno = 0; compno < p_image.numcomps; compno++) {
			p_image.comps[compno].resno_decoded = p_j2k.m_output_image.comps[compno].resno_decoded;

			if (p_image.comps[compno].data != null)
				p_image.comps[compno].data = null;

			p_image.comps[compno].data = p_j2k.m_output_image.comps[compno].data;

			p_j2k.m_output_image.comps[compno].data = null;
		}

		return true;
	}

	/*
	 * Read and decode one tile.
	 */
	private boolean opj_j2k_decode_one_tile(opj_j2k_t p_j2k,
			RandomAccessFile p_stream) {
		boolean l_go_on[] = new boolean[] { true };
		int l_current_tile_no[] = new int[1];
		int l_tile_no_to_dec;
		int l_data_size[] = new int[1];
		int l_max_data_size;
		int l_tile_x0[] = new int[1];
		int l_tile_y0[] = new int[1];
		int l_tile_x1[] = new int[1];
		int l_tile_y1[] = new int[1];
		int l_nb_comps[] = new int[1];
		byte l_current_data[];
		int l_current_data_ptr[] = new int[1];
		int i;

		l_current_data = new byte[1000];

		l_max_data_size = 1000;

		/*
		 * Allocate and initialize some elements of codestrem index if not
		 * already done
		 */
		if (p_j2k.cstr_index.tile_index == null) {
			if (!opj_j2k_allocate_tile_element_cstr_index(p_j2k)) {
				l_current_data = null;
				return false;
			}
		}
		/*
		 * Move into the codestream to the first SOT used to decode the desired
		 * tile
		 */
		l_tile_no_to_dec = p_j2k.m_decoder.m_tile_ind_to_dec;
		if (p_j2k.cstr_index.tile_index != null)
			if (p_j2k.cstr_index.tile_index[0].tp_index != null) {
				if (p_j2k.cstr_index.tile_index[l_tile_no_to_dec].nb_tps == 0) {
					/*
					 * the index for this tile has not been built, so move to
					 * the last SOT read
					 */
					try {
						p_stream.seek(p_j2k.m_decoder.m_last_sot_read_pos + 2);
					} catch (IOException e) {
						MipavUtil.displayError("Problem with seek function");
						l_current_data = null;
						return false;
					}
				} else {
					try {
						p_stream.seek(p_j2k.cstr_index.tile_index[l_tile_no_to_dec].tp_index[0].start_pos + 2);
					} catch (IOException e) {
						MipavUtil.displayError("Problem with seek function");
						l_current_data = null;
						return false;
					}
				}
				/*
				 * Special case if we have previously read the EOC marker (if
				 * the previous tile getted is the last )
				 */
				if (p_j2k.m_decoder.m_state == J2K_STATE_EOC)
					p_j2k.m_decoder.m_state = J2K_STATE_TPHSOT;
			}

		for (;;) {
			if (!opj_j2k_read_tile_header(p_j2k, l_current_tile_no,
					l_data_size, l_tile_x0, l_tile_y0, l_tile_x1, l_tile_y1,
					l_nb_comps, l_go_on, p_stream)) {
				l_current_data = null;
				return false;
			}

			if (!l_go_on[0]) {
				break;
			}

			if (l_data_size[0] > l_max_data_size) {
				byte l_new_current_data[] = new byte[l_data_size[0]];
				for (i = 0; i < l_current_data.length; i++) {
					l_new_current_data[i] = l_current_data[i];
				}

				l_current_data = l_new_current_data;
				l_max_data_size = l_data_size[0];
			}

			if (!opj_j2k_decode_tile(p_j2k, l_current_tile_no[0],
					l_current_data, l_current_data_ptr, l_data_size[0],
					p_stream)) {
				l_current_data = null;
				return false;
			}
			Preferences.debug("Tile " + l_current_tile_no[0] + "/"
					+ ((p_j2k.m_cp.th * p_j2k.m_cp.tw) - 1)
					+ "has been decoded.\n", Preferences.DEBUG_FILEIO);

			if (!opj_j2k_update_image_data(p_j2k.m_tcd, l_current_data,
					l_current_data_ptr, p_j2k.m_output_image)) {
				l_current_data = null;
				return false;
			}
			Preferences.debug("Image data has been updated with tile "
					+ l_current_tile_no[0] + ".\n\n", Preferences.DEBUG_FILEIO);

			if (l_current_tile_no[0] == l_tile_no_to_dec) {
				/*
				 * move into the codestream to the the first SOT (FIXME or not
				 * move?)
				 */
				try {
					p_stream.seek(p_j2k.cstr_index.main_head_end + 2);
				} catch (IOException e) {
					MipavUtil.displayError("Problem with seek function");
					l_current_data = null;
					return false;
				}
				break;
			} else {
				Preferences.debug(
						"Warning! Tile read, decode and updated is not the desired ("
								+ l_current_tile_no[0] + " vs "
								+ l_tile_no_to_dec + ").\n",
						Preferences.DEBUG_FILEIO);
			}

		}

		l_current_data = null;

		return true;
	}

	private void color_sycc_to_rgb(opj_image_t img) {
		if (img.numcomps < 3) {
			img.color_space = OPJ_CLRSPC_GRAY;
			return;
		}

		if ((img.comps[0].dx == 1) && (img.comps[1].dx == 2)
				&& (img.comps[2].dx == 2) && (img.comps[0].dy == 1)
				&& (img.comps[1].dy == 2) && (img.comps[2].dy == 2))/*
																	 * horizontal
																	 * and
																	 * vertical
																	 * sub
																	 * -sample
																	 */
		{
			sycc420_to_rgb(img);
		} else if ((img.comps[0].dx == 1) && (img.comps[1].dx == 2)
				&& (img.comps[2].dx == 2) && (img.comps[0].dy == 1)
				&& (img.comps[1].dy == 1) && (img.comps[2].dy == 1))/*
																	 * horizontal
																	 * sub
																	 * -sample
																	 * only
																	 */
		{
			sycc422_to_rgb(img);
		} else if ((img.comps[0].dx == 1) && (img.comps[1].dx == 1)
				&& (img.comps[2].dx == 1) && (img.comps[0].dy == 1)
				&& (img.comps[1].dy == 1) && (img.comps[2].dy == 1))/*
																	 * no
																	 * sub-sample
																	 */
		{
			sycc444_to_rgb(img);
		} else {
			MipavUtil.displayError("color_sycc_to_rgb cannot convert");
			return;
		}
		img.color_space = OPJ_CLRSPC_SRGB;

	}/* color_sycc_to_rgb() */

	private void sycc444_to_rgb(opj_image_t img) {
		int d0[], d1[], d2[], r[], g[], b[];
		int y[], cb[], cr[];
		int maxw, maxh, max, i;
		int offset, upb;

		upb = (int) img.comps[0].prec;
		offset = 1 << (upb - 1);
		upb = (1 << upb) - 1;

		maxw = img.comps[0].w;
		maxh = img.comps[0].h;
		max = maxw * maxh;

		y = img.comps[0].data;
		cb = img.comps[1].data;
		cr = img.comps[2].data;

		d0 = r = new int[max];
		d1 = g = new int[max];
		d2 = b = new int[max];

		int y_ptr = 0;
		int cb_ptr = 0;
		int cr_ptr = 0;
		int r_ptr = 0;
		int g_ptr = 0;
		int b_ptr = 0;
		for (i = 0; i < max; ++i) {
			sycc_to_rgb(offset, upb, y, y_ptr, cb, cb_ptr, cr, cr_ptr, r,
					r_ptr, g, g_ptr, b, b_ptr);
			++y_ptr;
			++cb_ptr;
			++cr_ptr;
			++r_ptr;
			++g_ptr;
			++b_ptr;
		}
		img.comps[0].data = null;
		img.comps[0].data = d0;
		img.comps[1].data = null;
		img.comps[1].data = d1;
		img.comps[2].data = null;
		img.comps[2].data = d2;

	}/* sycc444_to_rgb() */

	private void sycc_to_rgb(int offset, int upb, int y[], int y_ptr, int cb[],
			int cb_ptr, int cr[], int cr_ptr, int out_r[], int out_r_ptr,
			int out_g[], int out_g_ptr, int out_b[], int out_b_ptr) {
		int r, g, b;

		cb[cb_ptr] -= offset;
		cr[cr_ptr] -= offset;
		r = y[y_ptr] + (int) (1.402 * (float) cr[cr_ptr]);
		if (r < 0)
			r = 0;
		else if (r > upb)
			r = upb;
		out_r[out_r_ptr] = r;

		g = y[y_ptr]
				- (int) (0.344 * (float) cb[cb_ptr] + 0.714 * (float) cr[cr_ptr]);
		if (g < 0)
			g = 0;
		else if (g > upb)
			g = upb;
		out_g[out_g_ptr] = g;

		b = y[y_ptr] + (int) (1.772 * (float) cb[cb_ptr]);
		if (b < 0)
			b = 0;
		else if (b > upb)
			b = upb;
		out_b[out_b_ptr] = b;
	}

	private void sycc422_to_rgb(opj_image_t img) {
		int d0[], d1[], d2[], r[], g[], b[];
		int y[], cb[], cr[];
		int maxw, maxh, max;
		int offset, upb;
		int i, j;

		upb = img.comps[0].prec;
		offset = 1 << (upb - 1);
		upb = (1 << upb) - 1;

		maxw = img.comps[0].w;
		maxh = img.comps[0].h;
		max = maxw * maxh;

		y = img.comps[0].data;
		cb = img.comps[1].data;
		cr = img.comps[2].data;

		d0 = r = new int[max];
		d1 = g = new int[max];
		d2 = b = new int[max];

		int y_ptr = 0;
		int cb_ptr = 0;
		int cr_ptr = 0;
		int r_ptr = 0;
		int g_ptr = 0;
		int b_ptr = 0;
		for (i = 0; i < maxh; ++i) {
			for (j = 0; j < (maxw & ~1); j += 2) {
				sycc_to_rgb(offset, upb, y, y_ptr, cb, cb_ptr, cr, cr_ptr, r,
						r_ptr, g, g_ptr, b, b_ptr);
				++y_ptr;
				++r_ptr;
				++g_ptr;
				++b_ptr;
				sycc_to_rgb(offset, upb, y, y_ptr, cb, cb_ptr, cr, cr_ptr, r,
						r_ptr, g, g_ptr, b, b_ptr);
				++y_ptr;
				++r_ptr;
				++g_ptr;
				++b_ptr;
				++cb_ptr;
				++cr_ptr;
			}
			if (j < maxw) {
				sycc_to_rgb(offset, upb, y, y_ptr, cb, cb_ptr, cr, cr_ptr, r,
						r_ptr, g, g_ptr, b, b_ptr);
				++y_ptr;
				++r_ptr;
				++g_ptr;
				++b_ptr;
				++cb_ptr;
				++cr_ptr;
			}
		}
		img.comps[0].data = null;
		img.comps[0].data = d0;
		img.comps[1].data = null;
		img.comps[1].data = d1;
		img.comps[2].data = null;
		img.comps[2].data = d2;

		if (useJPWL || useMJ2) {
			img.comps[1].w = maxw;
			img.comps[1].h = maxh;
			img.comps[2].w = maxw;
			img.comps[2].h = maxh;
		} else {
			img.comps[1].w = maxw;
			img.comps[1].h = maxh;
			img.comps[2].w = maxw;
			img.comps[2].h = maxh;
		}
		img.comps[1].dx = img.comps[0].dx;
		img.comps[2].dx = img.comps[0].dx;
		img.comps[1].dy = img.comps[0].dy;
		img.comps[2].dy = img.comps[0].dy;

	}/* sycc422_to_rgb() */

	private void sycc420_to_rgb(opj_image_t img) {
		int d0[], d1[], d2[], r[], g[], b[];
		int y[], cb[], cr[];
		int maxw, maxh, max;
		int offset, upb;
		int i, j;

		upb = (int) img.comps[0].prec;
		offset = 1 << (upb - 1);
		upb = (1 << upb) - 1;

		maxw = img.comps[0].w;
		maxh = img.comps[0].h;
		max = maxw * maxh;

		y = img.comps[0].data;
		cb = img.comps[1].data;
		cr = img.comps[2].data;

		d0 = r = new int[max];
		d1 = g = new int[max];
		d2 = b = new int[max];
		int y_ptr = 0;
		int cb_ptr = 0;
		int cr_ptr = 0;
		int r_ptr = 0;
		int g_ptr = 0;
		int b_ptr = 0;
		int ny_ptr;
		int nr_ptr;
		int ng_ptr;
		int nb_ptr;
		for (i = 0; i < (maxh & ~1); i += 2) {
			ny_ptr = y_ptr + maxw;
			nr_ptr = r_ptr + maxw;
			ng_ptr = g_ptr + maxw;
			nb_ptr = b_ptr + maxw;

			for (j = 0; j < (maxw & ~1); j += 2) {
				sycc_to_rgb(offset, upb, y, y_ptr, cb, cb_ptr, cr, cr_ptr, r,
						r_ptr, g, g_ptr, b, b_ptr);
				++y_ptr;
				++r_ptr;
				++g_ptr;
				++b_ptr;
				sycc_to_rgb(offset, upb, y, y_ptr, cb, cb_ptr, cr, cr_ptr, r,
						r_ptr, g, g_ptr, b, b_ptr);
				++y_ptr;
				++r_ptr;
				++g_ptr;
				++b_ptr;

				sycc_to_rgb(offset, upb, y, ny_ptr, cb, cb_ptr, cr, cr_ptr, r,
						nr_ptr, g, ng_ptr, b, nb_ptr);
				++ny_ptr;
				++nr_ptr;
				++ng_ptr;
				++nb_ptr;
				sycc_to_rgb(offset, upb, y, ny_ptr, cb, cb_ptr, cr, cr_ptr, r,
						nr_ptr, g, ng_ptr, b, nb_ptr);
				++ny_ptr;
				++nr_ptr;
				++ng_ptr;
				++nb_ptr;
				++cb_ptr;
				++cr_ptr;
			}
			if (j < maxw) {
				sycc_to_rgb(offset, upb, y, y_ptr, cb, cb_ptr, cr, cr_ptr, r,
						r_ptr, g, g_ptr, b, b_ptr);
				++y_ptr;
				++r_ptr;
				++g_ptr;
				++b_ptr;

				sycc_to_rgb(offset, upb, y, ny_ptr, cb, cb_ptr, cr, cr_ptr, r,
						nr_ptr, g, ng_ptr, b, nb_ptr);
				++ny_ptr;
				++nr_ptr;
				++ng_ptr;
				++nb_ptr;
				++cb_ptr;
				++cr_ptr;
			}
			y_ptr += maxw;
			r_ptr += maxw;
			g_ptr += maxw;
			b_ptr += maxw;
		}
		if (i < maxh) {
			for (j = 0; j < (maxw & ~1); j += 2) {
				sycc_to_rgb(offset, upb, y, y_ptr, cb, cb_ptr, cr, cr_ptr, r,
						r_ptr, g, g_ptr, b, b_ptr);

				++y_ptr;
				++r_ptr;
				++g_ptr;
				++b_ptr;

				sycc_to_rgb(offset, upb, y, y_ptr, cb, cb_ptr, cr, cr_ptr, r,
						r_ptr, g, g_ptr, b, b_ptr);

				++y_ptr;
				++r_ptr;
				++g_ptr;
				++b_ptr;
				++cb_ptr;
				++cr_ptr;
			}
			if (j < maxw) {
				sycc_to_rgb(offset, upb, y, y_ptr, cb, cb_ptr, cr, cr_ptr, r,
						r_ptr, g, g_ptr, b, b_ptr);
			}
		}

		img.comps[0].data = null;
		img.comps[0].data = d0;
		img.comps[1].data = null;
		img.comps[1].data = d1;
		img.comps[2].data = null;
		img.comps[2].data = d2;

		if (useJPWL || useMJ2) {
			img.comps[1].w = maxw;
			img.comps[1].h = maxh;
			img.comps[2].w = maxw;
			img.comps[2].h = maxh;
		} else {
			img.comps[1].w = maxw;
			img.comps[1].h = maxh;
			img.comps[2].w = maxw;
			img.comps[2].h = maxh;
		}
		img.comps[1].dx = img.comps[0].dx;
		img.comps[2].dx = img.comps[0].dx;
		img.comps[1].dy = img.comps[0].dy;
		img.comps[2].dy = img.comps[0].dy;

	}/* sycc420_to_rgb() */

	private void color_cmyk_to_rgb(opj_image_t image) {
		float C, M, Y, K;
		float sC, sM, sY, sK;
		int w, h, max, i, j;

		w = image.comps[0].w;
		h = image.comps[0].h;

		if (image.numcomps < 4)
			return;

		max = w * h;

		sC = 1.0F / (float) ((1 << image.comps[0].prec) - 1);
		sM = 1.0F / (float) ((1 << image.comps[1].prec) - 1);
		sY = 1.0F / (float) ((1 << image.comps[2].prec) - 1);
		sK = 1.0F / (float) ((1 << image.comps[3].prec) - 1);

		for (i = 0; i < max; ++i) {
			/* CMYK values from 0 to 1 */
			C = (float) (image.comps[0].data[i]) * sC;
			M = (float) (image.comps[1].data[i]) * sM;
			Y = (float) (image.comps[2].data[i]) * sY;
			K = (float) (image.comps[3].data[i]) * sK;

			/* Invert all CMYK values */
			C = 1.0F - C;
			M = 1.0F - M;
			Y = 1.0F - Y;
			K = 1.0F - K;

			/* CMYK -> RGB : RGB results from 0 to 255 */
			image.comps[0].data[i] = (int) (255.0F * C * K); /* R */
			image.comps[1].data[i] = (int) (255.0F * M * K); /* G */
			image.comps[2].data[i] = (int) (255.0F * Y * K); /* B */
		}

		image.comps[3].data = null;
		image.comps[0].prec = 8;
		image.comps[1].prec = 8;
		image.comps[2].prec = 8;
		image.numcomps -= 1;
		image.color_space = OPJ_CLRSPC_SRGB;

		for (i = 3; i < image.numcomps; ++i) {
			image.comps[i].dx = image.comps[i + 1].dx;
			image.comps[i].dy = image.comps[i + 1].dy;
			image.comps[i].w = image.comps[i + 1].w;
			image.comps[i].h = image.comps[i + 1].h;
			image.comps[i].x0 = image.comps[i + 1].x0;
			image.comps[i].y0 = image.comps[i + 1].y0;
			image.comps[i].prec = image.comps[i + 1].prec;
			image.comps[i].bpp = image.comps[i + 1].bpp;
			image.comps[i].sgnd = image.comps[i + 1].sgnd;
			image.comps[i].resno_decoded = image.comps[i + 1].resno_decoded;
			image.comps[i].factor = image.comps[i + 1].factor;
			image.comps[i].data = null;
			image.comps[i].data = new int[image.comps[i + 1].data.length];
			for (j = 0; j < image.comps[i + 1].data.length; j++) {
				image.comps[i].data[j] = image.comps[i + 1].data[j];
			}
			image.comps[i].alpha = image.comps[i + 1].alpha;
		}

	}/* color_cmyk_to_rgb() */

	/*
	 * This code has been adopted from sjpx_openjpeg.c of ghostscript
	 */
	private void color_esycc_to_rgb(opj_image_t image) {
		int y, cb, cr, sign1, sign2, val;
		int w, h, max, i;
		int flip_value = (1 << (image.comps[0].prec - 1));
		int max_value = (1 << image.comps[0].prec) - 1;

		if (image.numcomps < 3)
			return;

		w = image.comps[0].w;
		h = image.comps[0].h;

		sign1 = (int) image.comps[1].sgnd;
		sign2 = (int) image.comps[2].sgnd;

		max = w * h;

		for (i = 0; i < max; ++i) {

			y = image.comps[0].data[i];
			cb = image.comps[1].data[i];
			cr = image.comps[2].data[i];

			if (sign1 == 0)
				cb -= flip_value;
			if (sign2 == 0)
				cr -= flip_value;

			val = (int) ((float) y - (float) 0.0000368 * (float) cb
					+ (float) 1.40199 * (float) cr + (float) 0.5);

			if (val > max_value)
				val = max_value;
			else if (val < 0)
				val = 0;
			image.comps[0].data[i] = val;

			val = (int) ((float) 1.0003 * (float) y - (float) 0.344125
					* (float) cb - (float) 0.7141128 * (float) cr + (float) 0.5);

			if (val > max_value)
				val = max_value;
			else if (val < 0)
				val = 0;
			image.comps[1].data[i] = val;

			val = (int) ((float) 0.999823 * (float) y + (float) 1.77204
					* (float) cb - (float) 0.000008 * (float) cr + (float) 0.5);

			if (val > max_value)
				val = max_value;
			else if (val < 0)
				val = 0;
			image.comps[2].data[i] = val;
		}
		image.color_space = OPJ_CLRSPC_SRGB;

	}/* color_esycc_to_rgb() */

	/* Component precision scaling */
	private void clip_component(opj_image_comp_t component, int precision) {
		int i;
		int len;
		long umax = 4294967295L;

		len = component.w * component.h;
		if (precision < 32) {
			umax = (1L << precision) - 1L;
		}

		if (component.sgnd != 0) {
			int l_data[] = component.data;
			int max = (int) (umax / 2);
			int min = -max - 1;
			for (i = 0; i < len; ++i) {
				if (l_data[i] > max) {
					l_data[i] = max;
				} else if (l_data[i] < min) {
					l_data[i] = min;
				}
			}
		} else {
			int l_data[] = component.data;
			for (i = 0; i < len; ++i) {
				if ((l_data[i] & 0xffffffffL) > umax) {
					l_data[i] = (int) umax;
				}
			}
		}
		component.prec = precision;
	}

	private void scale_component(opj_image_comp_t component, int precision) {
		int shift;
		int len;
		int i;
		long tmpLong;

		if (component.prec == precision) {
			return;
		}
		if (component.prec < precision) {
			scale_component_up(component, precision);
			return;
		}
		shift = (int) (component.prec - precision);
		len = component.w * component.h;
		if (component.sgnd != 0) {
			int l_data[] = component.data;
			for (i = 0; i < len; ++i) {
				l_data[i] >>= shift;
			}
		} else {
			int l_data[] = component.data;
			for (i = 0; i < len; ++i) {
				tmpLong = l_data[i] & 0xffffffffL;
				tmpLong >>= shift;
				l_data[i] = (int) tmpLong;
			}
		}
		component.bpp = precision;
		component.prec = precision;
	}

	/* Component precision scaling */
	private void scale_component_up(opj_image_comp_t component, int precision) {
		int len;
		int i;

		len = component.w * component.h;
		if (component.sgnd != 0) {
			long newMax = (1L << (precision - 1));
			long oldMax = (1L << (component.prec - 1));
			int l_data[] = component.data;
			for (i = 0; i < len; ++i) {
				l_data[i] = (int) (((long) l_data[i] * newMax) / oldMax);
			}
		} else {
			long newMax = ((1L << precision) - 1);
			long oldMax = ((1L << component.prec) - 1);
			int l_data[] = component.data;
			for (i = 0; i < len; ++i) {
				l_data[i] = (int) (((l_data[i] & 0xffffffffL) * newMax) / oldMax);
			}
		}
		component.prec = precision;
		component.bpp = precision;
	}

	private opj_image_t upsample_image_components(opj_image_t original) {
		opj_image_t l_new_image = null;
		opj_image_cmptparm_t l_new_components[] = null;
		boolean l_upsample_need = false;
		int compno;
		int i;

		for (compno = 0; compno < original.numcomps; ++compno) {
			if (original.comps[compno].factor > 0) {
				MipavUtil
						.displayError("ERROR -> opj_decompress: -upsample not supported with reduction");
				opj_image_destroy(original);
				return null;
			}
			if ((original.comps[compno].dx > 1)
					|| (original.comps[compno].dy > 1)) {
				l_upsample_need = true;
				break;
			}
		}
		if (!l_upsample_need) {
			return original;
		}
		/* Upsample is needed */
		l_new_components = new opj_image_cmptparm_t[original.numcomps];
		for (i = 0; i < original.numcomps; i++) {
			l_new_components[i] = new opj_image_cmptparm_t();
		}

		for (compno = 0; compno < original.numcomps; ++compno) {
			opj_image_cmptparm_t l_new_cmp = l_new_components[compno];
			opj_image_comp_t l_org_cmp = original.comps[compno];

			l_new_cmp.bpp = l_org_cmp.bpp;
			l_new_cmp.prec = l_org_cmp.prec;
			l_new_cmp.sgnd = l_org_cmp.sgnd;
			l_new_cmp.x0 = original.x0;
			l_new_cmp.y0 = original.y0;
			l_new_cmp.dx = 1;
			l_new_cmp.dy = 1;
			l_new_cmp.w = l_org_cmp.w; /*
										 * should be original->x1 - original->x0
										 * for dx==1
										 */
			l_new_cmp.h = l_org_cmp.h; /*
										 * should be original->y1 - original->y0
										 * for dy==0
										 */

			if (l_org_cmp.dx > 1) {
				l_new_cmp.w = original.x1 - original.x0;
			}

			if (l_org_cmp.dy > 1) {
				l_new_cmp.h = original.y1 - original.y0;
			}
		}

		l_new_image = opj_image_create(original.numcomps, l_new_components,
				original.color_space);
		l_new_components = null;
		if (l_new_image == null) {
			MipavUtil
					.displayError("opj_decompress: failed to allocate memory for upsampled components!");
			opj_image_destroy(original);
			return null;
		}

		l_new_image.x0 = original.x0;
		l_new_image.x1 = original.x1;
		l_new_image.y0 = original.y0;
		l_new_image.y1 = original.y1;

		for (compno = 0; compno < original.numcomps; ++compno) {
			opj_image_comp_t l_new_cmp = l_new_image.comps[compno];
			opj_image_comp_t l_org_cmp = original.comps[compno];

			l_new_cmp.factor = l_org_cmp.factor;
			l_new_cmp.alpha = l_org_cmp.alpha;
			l_new_cmp.resno_decoded = l_org_cmp.resno_decoded;

			if ((l_org_cmp.dx > 1) || (l_org_cmp.dy > 1)) {
				int l_src[] = l_org_cmp.data;
				int l_src_ptr = 0;
				int l_dst[] = l_new_cmp.data;
				int l_dst_ptr = 0;
				int y;
				int xoff, yoff;

				/* need to take into account dx & dy */
				xoff = l_org_cmp.dx * l_org_cmp.x0 - original.x0;
				yoff = l_org_cmp.dy * l_org_cmp.y0 - original.y0;
				if ((xoff >= l_org_cmp.dx) || (yoff >= l_org_cmp.dy)) {
					MipavUtil
							.displayError("opj_decompress: Invalid image/component parameters found when upsampling");
					opj_image_destroy(original);
					opj_image_destroy(l_new_image);
					return null;
				}

				for (y = 0; y < yoff; ++y) {
					for (i = 0; i < l_new_cmp.w; i++) {
						l_dst[l_dst_ptr + i] = 0;
					}
					l_dst_ptr += l_new_cmp.w;
				}

				if (l_new_cmp.h > (l_org_cmp.dy - 1)) { /*
														 * check subtraction
														 * overflow for really
														 * small images
														 */
					for (; y < l_new_cmp.h - (l_org_cmp.dy - 1); y += l_org_cmp.dy) {
						int x, dy;
						int xorg;

						xorg = 0;
						for (x = 0; x < xoff; ++x) {
							l_dst[l_dst_ptr + x] = 0;
						}
						if (l_new_cmp.w > (l_org_cmp.dx - 1)) { /*
																 * check
																 * subtraction
																 * overflow for
																 * really small
																 * images
																 */
							for (; x < l_new_cmp.w - (l_org_cmp.dx - 1); x += l_org_cmp.dx, ++xorg) {
								int dx;
								for (dx = 0; dx < l_org_cmp.dx; ++dx) {
									l_dst[l_dst_ptr + x + dx] = l_src[l_src_ptr
											+ xorg];
								}
							}
						}
						for (; x < l_new_cmp.w; ++x) {
							l_dst[l_dst_ptr + x] = l_src[l_src_ptr + xorg];
						}
						l_dst_ptr += l_new_cmp.w;

						for (dy = 1; dy < l_org_cmp.dy; ++dy) {
							for (i = 0; i < l_new_cmp.w; i++) {
								l_dst[l_dst_ptr + i] = l_dst[l_dst_ptr
										- l_new_cmp.w + i];
							}
							l_dst_ptr += l_new_cmp.w;
						}
						l_src_ptr += l_org_cmp.w;
					}
				}
				if (y < l_new_cmp.h) {
					int x;
					int xorg;

					xorg = 0;
					for (x = 0; x < xoff; ++x) {
						l_dst[l_dst_ptr + x] = 0;
					}
					if (l_new_cmp.w > (l_org_cmp.dx - 1)) { /*
															 * check subtraction
															 * overflow for
															 * really small
															 * images
															 */
						for (; x < l_new_cmp.w - (l_org_cmp.dx - 1); x += l_org_cmp.dx, ++xorg) {
							int dx;
							for (dx = 0; dx < l_org_cmp.dx; ++dx) {
								l_dst[l_dst_ptr + x + dx] = l_src[l_src_ptr
										+ xorg];
							}
						}
					}
					for (; x < l_new_cmp.w; ++x) {
						l_dst[l_dst_ptr + x] = l_src[l_src_ptr + xorg];
					}
					l_dst_ptr += l_new_cmp.w;
					++y;
					for (; y < l_new_cmp.h; ++y) {
						for (i = 0; i < l_new_cmp.w; i++) {
							l_dst[l_dst_ptr + i] = l_dst[l_dst_ptr
									- l_new_cmp.w + i];
						}
						l_dst_ptr += l_new_cmp.w;
					}
				}
			} else {
				for (i = 0; i < l_org_cmp.w * l_org_cmp.h; i++) {
					l_new_cmp.data[i] = l_org_cmp.data[i];
				}
			}
		}
		opj_image_destroy(original);
		return l_new_image;
	}

	private opj_image_t opj_image_create(int numcmpts,
			opj_image_cmptparm_t cmptparms[], int clrspc) {
		int compno;
		opj_image_t image = null;
		int i;

		image = new opj_image_t();
	 //if (image != null) {
     // This if clause gives a java.lang.verifyError
			image.color_space = clrspc;
			image.numcomps = numcmpts;
			// allocate memory for the per-component information
			image.comps = new opj_image_comp_t[numcmpts];
			for (i = 0; i < numcmpts; i++) {
				image.comps[i] = new opj_image_comp_t();
			}
			if (image.comps == null) {
				// TODO replace with event manager, breaks API
				//fprintf(stderr,"Unable to allocate memory for image.\n");
				opj_image_destroy(image);
				return null;
			}
			// create the individual image components
			for (compno = 0; compno < numcmpts; compno++) {
				opj_image_comp_t comp = image.comps[compno];
				comp.dx = cmptparms[compno].dx;
				comp.dy = cmptparms[compno].dy;
				comp.w = cmptparms[compno].w;
				comp.h = cmptparms[compno].h;
				comp.x0 = cmptparms[compno].x0;
				comp.y0 = cmptparms[compno].y0;
				comp.prec = cmptparms[compno].prec;
				comp.bpp = cmptparms[compno].bpp;
				comp.sgnd = cmptparms[compno].sgnd;
				comp.data = new int[comp.w * comp.h];
				if (comp.data == null) {
					// TODO replace with event manager, breaks API 
					// fprintf(stderr,"Unable to allocate memory for image.\n");
					opj_image_destroy(image);
					return null;
				}
			}
		//}

		return image;
	}

	private opj_image_t convert_gray_to_rgb(opj_image_t original) {
		int compno;
		opj_image_t l_new_image = null;
		opj_image_cmptparm_t l_new_components[] = null;
		int i;

		l_new_components = new opj_image_cmptparm_t[original.numcomps + 2];
		for (i = 0; i < original.numcomps + 2; i++) {
			l_new_components[i] = new opj_image_cmptparm_t();
		}

		l_new_components[0].bpp = l_new_components[1].bpp = l_new_components[2].bpp = original.comps[0].bpp;
		l_new_components[0].dx = l_new_components[1].dx = l_new_components[2].dx = original.comps[0].dx;
		l_new_components[0].dy = l_new_components[1].dy = l_new_components[2].dy = original.comps[0].dy;
		l_new_components[0].h = l_new_components[1].h = l_new_components[2].h = original.comps[0].h;
		l_new_components[0].w = l_new_components[1].w = l_new_components[2].w = original.comps[0].w;
		l_new_components[0].prec = l_new_components[1].prec = l_new_components[2].prec = original.comps[0].prec;
		l_new_components[0].sgnd = l_new_components[1].sgnd = l_new_components[2].sgnd = original.comps[0].sgnd;
		l_new_components[0].x0 = l_new_components[1].x0 = l_new_components[2].x0 = original.comps[0].x0;
		l_new_components[0].y0 = l_new_components[1].y0 = l_new_components[2].y0 = original.comps[0].y0;

		for (compno = 1; compno < original.numcomps; ++compno) {
			l_new_components[compno + 2].bpp = original.comps[compno].bpp;
			l_new_components[compno + 2].dx = original.comps[compno].dx;
			l_new_components[compno + 2].dy = original.comps[compno].dy;
			l_new_components[compno + 2].h = original.comps[compno].h;
			l_new_components[compno + 2].w = original.comps[compno].w;
			l_new_components[compno + 2].prec = original.comps[compno].prec;
			l_new_components[compno + 2].sgnd = original.comps[compno].sgnd;
			l_new_components[compno + 2].x0 = original.comps[compno].x0;
			l_new_components[compno + 2].y0 = original.comps[compno].y0;
		}

		l_new_image = opj_image_create(original.numcomps + 2, l_new_components,
				OPJ_CLRSPC_SRGB);
		l_new_components = null;
		if (l_new_image == null) {
			MipavUtil
					.displayError("opj_decompress: failed to allocate memory for RGB image!");
			opj_image_destroy(original);
			return null;
		}

		l_new_image.x0 = original.x0;
		l_new_image.x1 = original.x1;
		l_new_image.y0 = original.y0;
		l_new_image.y1 = original.y1;

		l_new_image.comps[0].factor = l_new_image.comps[1].factor = l_new_image.comps[2].factor = original.comps[0].factor;
		l_new_image.comps[0].alpha = l_new_image.comps[1].alpha = l_new_image.comps[2].alpha = original.comps[0].alpha;
		l_new_image.comps[0].resno_decoded = l_new_image.comps[1].resno_decoded = l_new_image.comps[2].resno_decoded = original.comps[0].resno_decoded;

		for (i = 0; i < original.comps[0].w * original.comps[0].h; i++) {
			l_new_image.comps[0].data[i] = original.comps[0].data[i];
			l_new_image.comps[1].data[i] = original.comps[0].data[i];
			l_new_image.comps[2].data[i] = original.comps[0].data[i];
		}

		for (compno = 1; compno < original.numcomps; ++compno) {
			l_new_image.comps[compno + 2].factor = original.comps[compno].factor;
			l_new_image.comps[compno + 2].alpha = original.comps[compno].alpha;
			l_new_image.comps[compno + 2].resno_decoded = original.comps[compno].resno_decoded;
			for (i = 0; i < original.comps[compno].w * original.comps[compno].h; i++) {
				l_new_image.comps[compno + 2].data[i] = original.comps[compno].data[i];
			}
		}
		opj_image_destroy(original);
		return l_new_image;
	}

	boolean imagetobmp(opj_image_t image, String outfile) {
		// BMP stores values in little endian format
		int w, h;
		int i, pad;
		File fdest = null;
		int adjustR, adjustG, adjustB;
		RandomAccessFile raFile;

		if (image.comps[0].prec < 8) {
			MipavUtil.displayError("Unsupported number of components: "
					+ image.comps[0].prec);
			return false;
		}
		if (image.numcomps >= 3 && image.comps[0].dx == image.comps[1].dx
				&& image.comps[1].dx == image.comps[2].dx
				&& image.comps[0].dy == image.comps[1].dy
				&& image.comps[1].dy == image.comps[2].dy
				&& image.comps[0].prec == image.comps[1].prec
				&& image.comps[1].prec == image.comps[2].prec) {

			/*
			 * -->> -->> -->> -->> 24 bits color <<-- <<-- <<-- <<--
			 */

			try {
				fdest = new File(outfile);
			} catch (NullPointerException e) {
				MipavUtil.displayError("Null pointer exception on " + outfile);
				return false;
			}
			try {
				raFile = new RandomAccessFile(fdest, "rw");
			} catch (FileNotFoundException e) {
				MipavUtil
						.displayError("File not found exception on raFile = new RandomAccessFile(fdest, rw)");
				return false;
			} catch (SecurityException e) {
				MipavUtil
						.displayError("Security exception on raFile = new RandomAccessFile(fdest, rw)");
				return false;
			}
			// Necessary so that if this is an overwritten file there isn't any
			// junk at the end
			try {
				raFile.setLength(0);
			} catch (IOException e) {
				MipavUtil.displayError("IOException on raFile.setLength(0)");
				try {
					raFile.close();
				} catch (IOException e2) {
					MipavUtil.displayError("IOException on raFile.close()");
				}
				return false;
			}

			w = (int) image.comps[0].w;
			h = (int) image.comps[0].h;

			try {
				raFile.writeBytes("BM");

				/* FILE HEADER */
				/* ------------- */
				raFile.writeByte((h * w * 3 + 3 * h * (w % 2) + 54) & 0xff);
				raFile.writeByte(((h * w * 3 + 3 * h * (w % 2) + 54) >>> 8) & 0xff);
				raFile.writeByte(((h * w * 3 + 3 * h * (w % 2) + 54) >>> 16) & 0xff);
				raFile.writeByte(((h * w * 3 + 3 * h * (w % 2) + 54) >>> 24) & 0xff);
				raFile.writeByte((0) & 0xff);
				raFile.writeByte(((0) >>> 8) & 0xff);
				raFile.writeByte(((0) >>> 16) & 0xff);
				raFile.writeByte(((0) >>> 24) & 0xff);
				raFile.writeByte((54) & 0xff);
				raFile.writeByte(((54) >>> 8) & 0xff);
				raFile.writeByte(((54) >>> 16) & 0xff);
				raFile.writeByte(((54) >>> 24) & 0xff);

				/* INFO HEADER */
				/* ------------- */
				raFile.writeByte((40) & 0xff);
				raFile.writeByte(((40) >>> 8) & 0xff);
				raFile.writeByte(((40) >>> 16) & 0xff);
				raFile.writeByte(((40) >>> 24) & 0xff);
				raFile.writeByte((w) & 0xff);
				raFile.writeByte(((w) >>> 8) & 0xff);
				raFile.writeByte(((w) >>> 16) & 0xff);
				raFile.writeByte(((w) >>> 24) & 0xff);
				raFile.writeByte((h) & 0xff);
				raFile.writeByte(((h) >>> 8) & 0xff);
				raFile.writeByte(((h) >>> 16) & 0xff);
				raFile.writeByte(((h) >>> 24) & 0xff);
				raFile.writeByte((1) & 0xff);
				raFile.writeByte(((1) >>> 8) & 0xff);
				raFile.writeByte((24) & 0xff);
				raFile.writeByte(((24) >>> 8) & 0xff);
				raFile.writeByte((0) & 0xff);
				raFile.writeByte(((0) >>> 8) & 0xff);
				raFile.writeByte(((0) >>> 16) & 0xff);
				raFile.writeByte(((0) >>> 24) & 0xff);
				raFile.writeByte((3 * h * w + 3 * h * (w % 2)) & 0xff);
				raFile.writeByte(((h * w * 3 + 3 * h * (w % 2)) >>> 8) & 0xff);
				raFile.writeByte(((h * w * 3 + 3 * h * (w % 2)) >>> 16) & 0xff);
				raFile.writeByte(((h * w * 3 + 3 * h * (w % 2)) >>> 24) & 0xff);
				raFile.writeByte((7834) & 0xff);
				raFile.writeByte(((7834) >>> 8) & 0xff);
				raFile.writeByte(((7834) >>> 16) & 0xff);
				raFile.writeByte(((7834) >>> 24) & 0xff);
				raFile.writeByte((7834) & 0xff);
				raFile.writeByte(((7834) >>> 8) & 0xff);
				raFile.writeByte(((7834) >>> 16) & 0xff);
				raFile.writeByte(((7834) >>> 24) & 0xff);
				raFile.writeByte((0) & 0xff);
				raFile.writeByte(((0) >>> 8) & 0xff);
				raFile.writeByte(((0) >>> 16) & 0xff);
				raFile.writeByte(((0) >>> 24) & 0xff);
				raFile.writeByte((0) & 0xff);
				raFile.writeByte(((0) >>> 8) & 0xff);
				raFile.writeByte(((0) >>> 16) & 0xff);
				raFile.writeByte(((0) >>> 24) & 0xff);

				if (image.comps[0].prec > 8) {
					adjustR = (int) image.comps[0].prec - 8;
					Preferences
							.debug("BMP CONVERSION: Truncating component 0 from "
									+ image.comps[0].prec + " bits to 8 bits\n",
									Preferences.DEBUG_FILEIO);
				} else
					adjustR = 0;
				if (image.comps[1].prec > 8) {
					adjustG = (int) image.comps[1].prec - 8;
					Preferences
							.debug("BMP CONVERSION: Truncating component 1 from "
									+ image.comps[1].prec + " bits to 8 bits\n",
									Preferences.DEBUG_FILEIO);
				} else
					adjustG = 0;
				if (image.comps[2].prec > 8) {
					adjustB = (int) image.comps[2].prec - 8;
					Preferences
							.debug("BMP CONVERSION: Truncating component 2 from "
									+ image.comps[2].prec + " bits to 8 bits\n",
									Preferences.DEBUG_FILEIO);
				} else
					adjustB = 0;

				for (i = 0; i < w * h; i++) {
					byte rc, gc, bc;
					int r, g, b;

					r = image.comps[0].data[w * h - ((i) / (w) + 1) * w + (i)
							% (w)];
					r += ((image.comps[0].sgnd != 0) ? 1 << (image.comps[0].prec - 1)
							: 0);
					r = ((r >> adjustR) + ((r >> (adjustR - 1)) % 2));
					if (r > 255)
						r = 255;
					else if (r < 0)
						r = 0;
					rc = (byte) (r & 0xff);

					g = image.comps[1].data[w * h - ((i) / (w) + 1) * w + (i)
							% (w)];
					g += ((image.comps[1].sgnd != 0) ? 1 << (image.comps[1].prec - 1)
							: 0);
					g = ((g >> adjustG) + ((g >> (adjustG - 1)) % 2));
					if (g > 255)
						g = 255;
					else if (g < 0)
						g = 0;
					gc = (byte) (g & 0xff);

					b = image.comps[2].data[w * h - ((i) / (w) + 1) * w + (i)
							% (w)];
					b += ((image.comps[2].sgnd != 0) ? 1 << (image.comps[2].prec - 1)
							: 0);
					b = ((b >> adjustB) + ((b >> (adjustB - 1)) % 2));
					if (b > 255)
						b = 255;
					else if (b < 0)
						b = 0;
					bc = (byte) (b & 0xff);

					raFile.writeByte(bc);
					raFile.writeByte(gc);
					raFile.writeByte(rc);

					if ((i + 1) % w == 0) {
						for (pad = ((3 * w) % 4 != 0) ? 4 - (3 * w) % 4 : 0; pad > 0; pad--)
							/* ADD */
							raFile.writeByte(0);
					}
				}
			} catch (IOException e) {
				MipavUtil.displayError("IOException on raFile.write");
				return false;
			}
			try {
				raFile.close();
			} catch (IOException e) {
				MipavUtil.displayError("IOException on raFile.close");
				return false;
			}
		} else { /* Gray-scale */

			/*
			 * -->> -->> -->> -->> 8 bits non code (Gray scale) <<-- <<-- <<--
			 * <<--
			 */

			try {
				fdest = new File(outfile);
			} catch (NullPointerException e) {
				MipavUtil.displayError("Null pointer exception on " + outfile);
				return false;
			}
			try {
				raFile = new RandomAccessFile(fdest, "rw");
			} catch (FileNotFoundException e) {
				MipavUtil
						.displayError("File not found exception on raFile = new RandomAccessFile(fdest, rw)");
				return false;
			} catch (SecurityException e) {
				MipavUtil
						.displayError("Security exception on raFile = new RandomAccessFile(fdest, rw)");
				return false;
			}
			// Necessary so that if this is an overwritten file there isn't any
			// junk at the end
			try {
				raFile.setLength(0);
			} catch (IOException e) {
				MipavUtil.displayError("IOException on raFile.setLength(0)");
				try {
					raFile.close();
				} catch (IOException e2) {
					MipavUtil.displayError("IOException on raFile.close()");
				}
				return false;
			}

			w = (int) image.comps[0].w;
			h = (int) image.comps[0].h;

			try {
				raFile.writeBytes("BM");

				/* FILE HEADER */
				/* ------------- */
				raFile.writeByte((h * w + 54 + 1024 + h * (w % 2)) & 0xff);
				raFile.writeByte(((h * w + 54 + 1024 + h * (w % 2)) >>> 8) & 0xff);
				raFile.writeByte(((h * w + 54 + 1024 + h * (w % 2)) >>> 16) & 0xff);
				raFile.writeByte(((h * w + 54 + 1024 + w * (w % 2)) >>> 24) & 0xff);
				raFile.writeByte((0) & 0xff);
				raFile.writeByte(((0) >>> 8) & 0xff);
				raFile.writeByte(((0) >>> 16) & 0xff);
				raFile.writeByte(((0) >>> 24) & 0xff);
				raFile.writeByte((54 + 1024) & 0xff);
				raFile.writeByte(((54 + 1024) >>> 8) & 0xff);
				raFile.writeByte(((54 + 1024) >>> 16) & 0xff);
				raFile.writeByte(((54 + 1024) >>> 24) & 0xff);

				/* INFO HEADER */
				/* ------------- */
				raFile.writeByte((40) & 0xff);
				raFile.writeByte(((40) >>> 8) & 0xff);
				raFile.writeByte(((40) >>> 16) & 0xff);
				raFile.writeByte(((40) >>> 24) & 0xff);
				raFile.writeByte((w) & 0xff);
				raFile.writeByte(((w) >>> 8) & 0xff);
				raFile.writeByte(((w) >>> 16) & 0xff);
				raFile.writeByte(((w) >>> 24) & 0xff);
				raFile.writeByte((h) & 0xff);
				raFile.writeByte(((h) >>> 8) & 0xff);
				raFile.writeByte(((h) >>> 16) & 0xff);
				raFile.writeByte(((h) >>> 24) & 0xff);
				raFile.writeByte((1) & 0xff);
				raFile.writeByte(((1) >>> 8) & 0xff);
				raFile.writeByte((8) & 0xff);
				raFile.writeByte(((8) >>> 8) & 0xff);
				raFile.writeByte((0) & 0xff);
				raFile.writeByte(((0) >>> 8) & 0xff);
				raFile.writeByte(((0) >>> 16) & 0xff);
				raFile.writeByte(((0) >>> 24) & 0xff);
				raFile.writeByte((h * w + h * (w % 2)) & 0xff);
				raFile.writeByte(((h * w + h * (w % 2)) >>> 8) & 0xff);
				raFile.writeByte(((h * w + h * (w % 2)) >>> 16) & 0xff);
				raFile.writeByte(((h * w + h * (w % 2)) >>> 24) & 0xff);
				raFile.writeByte((7834) & 0xff);
				raFile.writeByte(((7834) >>> 8) & 0xff);
				raFile.writeByte(((7834) >>> 16) & 0xff);
				raFile.writeByte(((7834) >>> 24) & 0xff);
				raFile.writeByte((7834) & 0xff);
				raFile.writeByte(((7834) >>> 8) & 0xff);
				raFile.writeByte(((7834) >>> 16) & 0xff);
				raFile.writeByte(((7834) >>> 24) & 0xff);
				raFile.writeByte((256) & 0xff);
				raFile.writeByte(((256) >>> 8) & 0xff);
				raFile.writeByte(((256) >>> 16) & 0xff);
				raFile.writeByte(((256) >>> 24) & 0xff);
				raFile.writeByte((256) & 0xff);
				raFile.writeByte(((256) >>> 8) & 0xff);
				raFile.writeByte(((256) >>> 16) & 0xff);
				raFile.writeByte(((256) >>> 24) & 0xff);

				if (image.comps[0].prec > 8) {
					adjustR = (int) image.comps[0].prec - 8;
					Preferences
							.debug("BMP CONVERSION: Truncating component 0 from "
									+ image.comps[0].prec + " bits to 8 bits\n",
									Preferences.DEBUG_FILEIO);
				} else
					adjustR = 0;

				for (i = 0; i < 256; i++) {
					raFile.writeByte(i & 0xff);
					raFile.writeByte(i & 0xff);
					raFile.writeByte(i & 0xff);
					raFile.writeByte(0 & 0xff);
				}

				for (i = 0; i < w * h; i++) {
					int r;

					r = image.comps[0].data[w * h - ((i) / (w) + 1) * w + (i)
							% (w)];
					r += ((image.comps[0].sgnd != 0) ? 1 << (image.comps[0].prec - 1)
							: 0);
					r = ((r >> adjustR) + ((r >> (adjustR - 1)) % 2));
					if (r > 255)
						r = 255;
					else if (r < 0)
						r = 0;

					raFile.writeByte(r & 0xff);

					if ((i + 1) % w == 0) {
						for (pad = (w % 4 != 0) ? 4 - w % 4 : 0; pad > 0; pad--)
							/* ADD */
							raFile.writeByte(0);
					}
				}
			} catch (IOException e) {
				MipavUtil.displayError("IOException on raFile.write");
				return false;
			}
			try {
				raFile.close();
			} catch (IOException e) {
				MipavUtil.displayError("IOException on raFile.close");
				return false;
			}
		}

		return true;
	}

	private boolean imagetoraw_common(opj_image_t image, String outfile,
			boolean big_endian) {
		File rawFile = null;
		int compno;
		int w, h;
		int line, row, curr, mask;
		int data[];
		int data_ptr;
		boolean passes = false;
		RandomAccessFile raFile;
		boolean setPasses = true;

		if ((image.numcomps * image.x1 * image.y1) == 0) {
			MipavUtil.displayError("Invalid raw image parameters");
			return false;
		}

		try {
			rawFile = new File(outfile);
		} catch (NullPointerException e) {
			MipavUtil.displayError("Null pointer exception on " + outfile);
			return false;
		}
		try {
			raFile = new RandomAccessFile(rawFile, "rw");
		} catch (FileNotFoundException e) {
			MipavUtil
					.displayError("File not found exception on raFile = new RandomAccessFile(fdest, rw)");
			return false;
		} catch (SecurityException e) {
			MipavUtil
					.displayError("Security exception on raFile = new RandomAccessFile(fdest, rw)");
			return false;
		}
		// Necessary so that if this is an overwritten file there isn't any
		// junk at the end
		try {
			raFile.setLength(0);
		} catch (IOException e) {
			MipavUtil.displayError("IOException on raFile.setLength(0)");
			try {
				raFile.close();
			} catch (IOException e2) {
				MipavUtil.displayError("IOException on raFile.close()");
			}
			return false;
		}

		try {
			passes = false;
			Preferences.debug("Raw image characteristics: " + image.numcomps
					+ "\n", Preferences.DEBUG_FILEIO);

			loop: for (compno = 0; compno < image.numcomps; compno++) {
				Preferences.debug("Component " + compno + " characteristics: "
						+ image.comps[compno].w + "x" + image.comps[compno].h
						+ "x" + image.comps[compno].prec,
						Preferences.DEBUG_FILEIO);
				if (image.comps[compno].sgnd == 1) {
					Preferences.debug(" signed\n", Preferences.DEBUG_FILEIO);
				} else {
					Preferences.debug(" unsigned\n", Preferences.DEBUG_FILEIO);
				}

				w = (int) image.comps[compno].w;
				h = (int) image.comps[compno].h;

				if (image.comps[compno].prec <= 8) {
					if (image.comps[compno].sgnd == 1) {
						mask = (1 << image.comps[compno].prec) - 1;
						data = image.comps[compno].data;
						data_ptr = 0;
						for (line = 0; line < h; line++) {
							for (row = 0; row < w; row++) {
								curr = data[data_ptr];
								if (curr > 127)
									curr = 127;
								else if (curr < -128)
									curr = -128;
								raFile.writeByte(curr & mask & 0xff);
								data_ptr++;
							}
						}
					} else if (image.comps[compno].sgnd == 0) {
						mask = (1 << image.comps[compno].prec) - 1;
						data = image.comps[compno].data;
						data_ptr = 0;
						for (line = 0; line < h; line++) {
							for (row = 0; row < w; row++) {
								curr = data[data_ptr];
								if (curr > 255)
									curr = 255;
								else if (curr < 0)
									curr = 0;
								raFile.writeByte(curr & mask & 0xff);
								data_ptr++;
							}
						}
					}
				} else if (image.comps[compno].prec <= 16) {
					if (image.comps[compno].sgnd == 1) {
						mask = (1 << image.comps[compno].prec) - 1;
						data = image.comps[compno].data;
						data_ptr = 0;
						for (line = 0; line < h; line++) {
							for (row = 0; row < w; row++) {
								curr = data[data_ptr];
								if (curr > 32767)
									curr = 32767;
								else if (curr < -32768)
									curr = -32768;
								if (big_endian) {
									raFile.writeByte(((curr & mask) >>> 8) & 0xff);
									raFile.writeByte(curr & mask & 0xff);
								} else {
									raFile.writeByte(curr & mask & 0xff);
									raFile.writeByte(((curr & mask) >>> 8) & 0xff);
								}
								data_ptr++;
							}
						}
					} else if (image.comps[compno].sgnd == 0) {
						mask = (1 << image.comps[compno].prec) - 1;
						data = image.comps[compno].data;
						data_ptr = 0;
						for (line = 0; line < h; line++) {
							for (row = 0; row < w; row++) {
								curr = data[data_ptr];
								if (curr > 65535)
									curr = 65535;
								else if (curr < 0)
									curr = 0;
								if (big_endian) {
									raFile.writeByte(((curr & mask) >>> 8) & 0xff);
									raFile.writeByte(curr & mask & 0xff);
								} else {
									raFile.writeByte(curr & mask & 0xff);
									raFile.writeByte(((curr & mask) >>> 8) & 0xff);
								}
								data_ptr++;
							}
						}
					}
				} else if (image.comps[compno].prec <= 32) {
					MipavUtil
							.displayError("More than 16 bits per component not handled yet");
					setPasses = false;
					break loop;
				} else {
					MipavUtil.displayError("Error: invalid precision: "
							+ image.comps[compno].prec);
					setPasses = false;
					break loop;
				}
			}
		} // try
		catch (IOException e) {
			MipavUtil.displayError("IOException on raFile.writeByte to "
					+ outfile);
			setPasses = false;
		}
		if (setPasses) {
			passes = true;
		}
		try {
			raFile.close();
		} catch (IOException e) {
			MipavUtil.displayError("IOException raFile.close()");
			passes = false;
		}
		return passes;
	}

	private boolean imagetopnm(opj_image_t image, String outfile, boolean force_split)
    {
    	// In Netpbm, the de facto standard implementation of the PNM formats, the most significant byte is first
        int red[], green[], blue[], alpha[];
        int red_ptr = 0;
        int green_ptr = 0;
        int blue_ptr = 0;
        int alpha_ptr = 0;
        int wr, hr, max;
        int i;
        int compno, ncomp;
        int adjustR, adjustG, adjustB, adjustA;
        boolean two,  has_alpha, triple;
        boolean want_gray;
        int prec, v;
        File fdest = null;
        String destname;
        byte cstring[] = null;
        String inString = null;

    	alpha = null;

        if((prec = (int)image.comps[0].prec) > 16)
        {
            MipavUtil.displayError("imagetopnm precision " + prec + " is larger than 16");
            return false;
        }
        two = has_alpha = false;
        ncomp = image.numcomps;

        if (outfile.substring(outfile.length()-2, outfile.length()-1).equalsIgnoreCase("G")) {
            want_gray = true;	
        }
        else {
        	want_gray = false;
        }
        ncomp = image.numcomps;

        if(want_gray) ncomp = 1;

        if ((!force_split) &&
    				(ncomp == 2 /* GRAYA */
                || (ncomp > 2 /* RGB, RGBA */
                    && image.comps[0].dx == image.comps[1].dx
                    && image.comps[1].dx == image.comps[2].dx
                    && image.comps[0].dy == image.comps[1].dy
                    && image.comps[1].dy == image.comps[2].dy
                    && image.comps[0].prec == image.comps[1].prec
                    && image.comps[1].prec == image.comps[2].prec
                    )))
    		{
        	try {
        	     fdest = new File(outfile);
            }
            catch (NullPointerException e) {
               MipavUtil.displayError("Null pointer exception on " + outfile);	
               return false;
            }
            try {
                raFile = new RandomAccessFile(fdest, "rw");
            }
            catch (FileNotFoundException e) {
            	MipavUtil.displayError("File not found exception on raFile = new RandomAccessFile(fdest, rw)");
            	return false;
            }
            catch(SecurityException e) {
            	MipavUtil.displayError("Security exception on raFile = new RandomAccessFile(fdest, rw)");
            	return false;	
            }
            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            try {
                raFile.setLength(0);
            }
            catch (IOException e) {
            	MipavUtil.displayError("IOException on raFile.setLength(0)");
            	try {
            		raFile.close();
            	}
            	catch (IOException e2) {
            	    MipavUtil.displayError("IOException on raFile.close()");	
            	}
            	return false;
            }
           
            try {
            two = (prec > 8);
            triple = (ncomp > 2);
            wr = (int)image.comps[0].w; hr = (int)image.comps[0].h;
            max = (1<<prec) - 1; has_alpha = (ncomp == 4 || ncomp == 2);

            red = image.comps[0].data;
            red_ptr = 0;

            if(triple)
            {
                green = image.comps[1].data;
                green_ptr = 0;
                blue = image.comps[2].data;
                blue_ptr = 0;
            }
            else green = blue = null;

            if(has_alpha)
            {
                String tt = (triple ?"RGB_ALPHA":"GRAYSCALE_ALPHA");
                inString = "P7\n# OpenJPEG-" + opj_version() + "\nWIDTH ";
                cstring = JavaStrToCStr(inString);
                // cstring.length-1 to remove terminating null
                raFile.write(cstring, 0, cstring.length-1);
                inString = String.valueOf(wr);
                cstring = JavaStrToCStr(inString);
                raFile.write(cstring, 0, cstring.length-1);
                cstring = JavaStrToCStr("\nHEIGHT ");
                raFile.write(cstring, 0, cstring.length-1);
                inString = String.valueOf(hr);
                cstring = JavaStrToCStr(inString);
                raFile.write(cstring, 0, cstring.length-1);
                cstring = JavaStrToCStr("\nDEPTH ");
                raFile.write(cstring, 0, cstring.length-1);
                inString = String.valueOf(ncomp);
                cstring = JavaStrToCStr(inString);
                raFile.write(cstring, 0, cstring.length-1);
                cstring = JavaStrToCStr("\nMAXVAL ");
                raFile.write(cstring, 0, cstring.length-1);
                inString = String.valueOf(max);
                cstring = JavaStrToCStr(inString);
                raFile.write(cstring, 0, cstring.length-1);
                inString = "\nTUPLTYPE " + tt + "\nENDHDR\n";
                cstring = JavaStrToCStr(inString);
                raFile.write(cstring, 0, cstring.length-1);
                alpha = image.comps[ncomp - 1].data;
                alpha_ptr = 0;
                adjustA = ((image.comps[ncomp - 1].sgnd != 0) ?
                            1 << (image.comps[ncomp - 1].prec - 1) : 0);
            }
            else
            {
            	inString = "P6\n# OpenJPEG-" + opj_version() + "\n";
            	cstring = JavaStrToCStr(inString);
            	 // cstring.length-1 to remove terminating null
                raFile.write(cstring, 0, cstring.length-1);
                inString = String.valueOf(wr);
                cstring = JavaStrToCStr(inString);
                raFile.write(cstring, 0, cstring.length-1);
                cstring = JavaStrToCStr(" ");
                raFile.write(cstring, 0, cstring.length-1);
                inString = String.valueOf(hr);
                cstring = JavaStrToCStr(inString);
                raFile.write(cstring, 0, cstring.length-1);
                cstring = JavaStrToCStr("\n");
                raFile.write(cstring, 0, cstring.length-1);
                inString = String.valueOf(max);
                cstring = JavaStrToCStr(inString);
                raFile.write(cstring, 0, cstring.length-1);
                cstring = JavaStrToCStr("\n");
                raFile.write(cstring, 0, cstring.length-1);
                adjustA = 0;
            }
            adjustR = ((image.comps[0].sgnd != 0) ? 1 << (image.comps[0].prec - 1) : 0);

            if(triple)
            {
                adjustG = ((image.comps[1].sgnd != 0) ? 1 << (image.comps[1].prec - 1) : 0);
                adjustB = ((image.comps[2].sgnd != 0) ? 1 << (image.comps[2].prec - 1) : 0);
            }
            else adjustG = adjustB = 0;

            for(i = 0; i < wr * hr; ++i)
            {
                if(two)
                {
                    v = red[red_ptr] + adjustR; ++red_ptr;
    if(v > 65535) v = 65535; else if(v < 0) v = 0;

                    /* netpbm: */
                    raFile.writeShort(v);

                    if(triple)
                    {
                        v = green[green_ptr] + adjustG; ++green_ptr;
    if(v > 65535) v = 65535; else if(v < 0) v = 0;

                        /* netpbm: */
                        raFile.writeShort(v);

                        v =  blue[blue_ptr] + adjustB; ++blue_ptr;
    if(v > 65535) v = 65535; else if(v < 0) v = 0;

                        /* netpbm: */
                        raFile.writeShort(v);

                    }/* if(triple) */

                    if(has_alpha)
                    {
                        v = alpha[alpha_ptr] + adjustA; ++alpha_ptr;
    		if(v > 65535) v = 65535; else if(v < 0) v = 0;

                        /* netpbm: */
    		            raFile.writeShort(v);
                    }
                    continue;

                }	/* if(two) */

                /* prec <= 8: */
    	v = red[red_ptr++];
    	if(v > 255) v = 255; else if(v < 0) v = 0;

    	raFile.writeByte(v);
                if(triple)
     {
    	v = green[green_ptr++];
    	if(v > 255) v = 255; else if(v < 0) v = 0;

    	raFile.writeByte(v);
    	v = blue[blue_ptr++];
    	if(v > 255) v = 255; else if(v < 0) v = 0;

    	raFile.writeByte(v);
     }
                if(has_alpha)
     {
    	v = alpha[alpha_ptr++];
    	if(v > 255) v = 255; else if(v < 0) v = 0;

    	raFile.writeByte(v);
     }
            }	/* for(i */
            } // try 
            catch (IOException e) {
                MipavUtil.displayError("IOException on raFile.write");
                return false;
            }

            try {
            	raFile.close();
            }
            catch (IOException e) {
            	MipavUtil.displayError("IOException on raFile.close");
            	return false;
            }
            return true;
        
    		
    		}

        /* YUV or MONO: */

        if (image.numcomps > ncomp)
        {
            Preferences.debug("WARNING! PGM file Only the first component\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("is written to the file\n", Preferences.DEBUG_FILEIO);
        }

        for (compno = 0; compno < ncomp; compno++)
        {
        if (ncomp > 1)
          {
          /*sprintf(destname, "%d.%s", compno, outfile);*/
          String compnoNum = String.valueOf(compno);
          while (compnoNum.length() < 3) {
              compnoNum = "0" + compnoNum;	  
          }
         
          destname = outfile.substring(0, outfile.length() - 4) + "_" + compnoNum + ".pgm";
          
          }
            else
                destname = new String(outfile);

        try {
   	     fdest = new File(destname);
       }
       catch (NullPointerException e) {
          MipavUtil.displayError("Null pointer exception on " + destname);	
          return false;
       }
       try {
           raFile = new RandomAccessFile(fdest, "rw");
       }
       catch (FileNotFoundException e) {
       	MipavUtil.displayError("File not found exception on raFile = new RandomAccessFile(fdest, rw)");
       	return false;
       }
       catch(SecurityException e) {
       	MipavUtil.displayError("Security exception on raFile = new RandomAccessFile(fdest, rw)");
       	return false;	
       }
       // Necessary so that if this is an overwritten file there isn't any
       // junk at the end
       try {
           raFile.setLength(0);
       }
       catch (IOException e) {
       	MipavUtil.displayError("IOException on raFile.setLength(0)");
       	try {
       		raFile.close();
       	}
       	catch (IOException e2) {
       	    MipavUtil.displayError("IOException on raFile.close()");	
       	}
       	return false;
       }    
        
            wr = image.comps[compno].w; hr = image.comps[compno].h;
            prec = image.comps[compno].prec;
            max = (1<<prec) - 1;
            
            try {
            	inString = "P5\n#OpenJPEG-" + opj_version() + "\n";
            	cstring = JavaStrToCStr(inString);
            	 // cstring.length-1 to remove terminating null
                raFile.write(cstring, 0, cstring.length-1);
                inString = String.valueOf(wr);
                cstring = JavaStrToCStr(inString);
                raFile.write(cstring, 0, cstring.length-1);
                cstring = JavaStrToCStr(" ");
                raFile.write(cstring, 0, cstring.length-1);
                inString = String.valueOf(hr);
                cstring = JavaStrToCStr(inString);
                raFile.write(cstring, 0, cstring.length-1);
                cstring = JavaStrToCStr("\n");
                raFile.write(cstring, 0, cstring.length-1);
                inString = String.valueOf(max);
                cstring = JavaStrToCStr(inString);
                raFile.write(cstring, 0, cstring.length-1);
                cstring = JavaStrToCStr("\n");
                raFile.write(cstring, 0, cstring.length-1);
            

            red = image.comps[compno].data;
            red_ptr = 0;
            adjustR =
                    ((image.comps[compno].sgnd != 0) ? 1 << (image.comps[compno].prec - 1) : 0);

            if(prec > 8)
            {
                for (i = 0; i < wr * hr; i++)
                {
                    v = red[red_ptr] + adjustR; ++red_ptr;
    if(v > 65535) v = 65535; else if(v < 0) v = 0;

                    /* netpbm: */
                    raFile.writeShort(v);

                    //if(has_alpha)
                    // alpha is never initialized
                    //{
                        //v = alpha[alpha_ptr++];
    //if(v > 65535) v = 65535; else if(v < 0) v = 0;

                        /* netpbm: */
                        //raFile.writeShort(v);
                    //}
                }/* for(i */
            }
            else /* prec <= 8 */
            {
                for(i = 0; i < wr * hr; ++i)
                {
    	v = red[red_ptr] + adjustR; ++red_ptr;
    	if(v > 255) v = 255; else if(v < 0) v = 0;

    	 raFile.writeByte(v);
                }
            }
            } // try 
            catch (IOException e) {
                MipavUtil.displayError("IOException on raFile.write");
                return false;
            }

            try {
            	raFile.close();
            }
            catch (IOException e) {
            	MipavUtil.displayError("IOException on raFile.close");
            	return false;
            }
            try {
            	raFile.close();
            }
            catch (IOException e) {
            	MipavUtil.displayError("IOException on raFile.close()");
            	return false;
            }
        } /* for (compno */

        return true;
    }/* imagetopnm() */
	
    boolean imagetopgx(opj_image_t image, String outfile) 
	{
    	//The file consists of a one line text header followed by the data.
//
    	//Header: "PG"+ ws +<endianess>+ ws +[sign]+ws + <bit-depth>+" "+<width>+" "+<height>+'\n'

    	//where:

    	   // ws (white-spaces) is any combination of characters ' ' and '\t'.
    	   // endianess equals "LM" or "ML"(resp. little-endian or big-endian)
    	    // sign equals "+" or "-" (resp. unsigned or signed). If omited, values are supposed to be unsigned.
    	   //  bit-depth that can be any number between 1 and 31.
    	   // width and height are the image dimensions (in pixels).

    	//Data: The image binary values appear one after the other (in raster order) immediately after the last header
    	// character ('\n') and are byte-aligned (they are packed into 1,2 or 4 bytes per sample, depending upon the bit-depth value).

    	// If the data is unisigned, level shifting is applied subtracting 2^(bitdepth - 1)

    	// Since it is not possible to know the input file byte-ordering before reading its header, this class can not 
    	// be construct from a RandomAccessIO. So, the constructor has to open first the input file, to read only its header, 
    	// and then it can create the appropriate BufferedRandomAccessFile (Big-Endian or Little-Endian byte-ordering).
    	
	  int w, h;
	  int i, j;
	  int compno;
	  String destname;
	  RandomAccessFile raFile;
	  File fdest = null;
	  String inString;
	  byte cstring[];

	  for (compno = 0; compno < image.numcomps; compno++) 
		{
	    opj_image_comp_t comp = image.comps[compno];
	    int nbytes = 0;

	    if (!outfile.substring(outfile.length() - 4,outfile.length() - 3).equals(".")) 
			{
	      /* `pgx` was recognized but there is no dot at expected position */
	      MipavUtil.displayError("No dot found before pgx");
	      return false;
			}
	    String compnoNum = String.valueOf(compno);
        while (compnoNum.length() < 3) {
            compnoNum = "0" + compnoNum;	  
        }
       
        destname = outfile.substring(0, outfile.length() - 4) + "_" + compnoNum + ".pgx";
        

      try {
 	     fdest = new File(destname);
     }
     catch (NullPointerException e) {
        MipavUtil.displayError("Null pointer exception on " + destname);	
        return false;
     }
     try {
         raFile = new RandomAccessFile(fdest, "rw");
     }
     catch (FileNotFoundException e) {
     	MipavUtil.displayError("File not found exception on raFile = new RandomAccessFile(fdest, rw)");
     	return false;
     }
     catch(SecurityException e) {
     	MipavUtil.displayError("Security exception on raFile = new RandomAccessFile(fdest, rw)");
     	return false;	
     }
     // Necessary so that if this is an overwritten file there isn't any
     // junk at the end
     try {
         raFile.setLength(0);
     }
     catch (IOException e) {
     	MipavUtil.displayError("IOException on raFile.setLength(0)");
     	try {
     		raFile.close();
     	}
     	catch (IOException e2) {
     	    MipavUtil.displayError("IOException on raFile.close()");	
     	}
     	return false;
     } 

	    w = (int)image.comps[compno].w;
	    h = (int)image.comps[compno].h;

	    try {
	    	if (comp.sgnd != 0) {
	    	inString = "PG ML - ";
	    	}
	    	else {
	    		inString = "PG ML + ";
	    	}
        	cstring = JavaStrToCStr(inString);
        	// No terminating null means use cstring.length-1
            raFile.write(cstring, 0, cstring.length-1);
            raFile.writeInt(comp.prec);
            inString = " ";
            cstring = JavaStrToCStr(inString);
            raFile.write(cstring, 0, cstring.length-1);
            raFile.writeInt(w);
            raFile.write(cstring, 0, cstring.length-1);
            raFile.writeInt(h);
            inString = "\n";
            cstring = JavaStrToCStr(inString);
            raFile.write(cstring, 0, cstring.length-1);
	    

	    if (comp.prec <= 8) 
	      nbytes = 1;
	    else if (comp.prec <= 16)
	      nbytes = 2;
	    else
	      nbytes = 4;

	    for (i = 0; i < w * h; i++) 
			{
	      /* FIXME: clamp func is being called within a loop */
	      final int val = clamp(image.comps[compno].data[i],
	        comp.prec, comp.sgnd);

	      for (j = nbytes - 1; j >= 0; j--) 
				{
	        int v = (val >> (j * 8)) & 0xff;
	        raFile.writeByte(v);
	        
				}
			}
			
	    try {
	    	raFile.close();
	    }
	    catch (IOException e) {
	        MipavUtil.displayError("IOException on raFile.close()");
	        return false;
	    }
	} // try
    catch (IOException e) {
    	 MipavUtil.displayError("IOExcpetion on raFile.write");
    	 return false;
    }
		} // for (compno = 0; compno < image.numcomps; compno++)
	  return true;
	}
    
   private boolean tga_writeheader(RandomAccessFile raFile, int bits_per_pixel, int width, int height, 
            boolean flip_image, boolean bigEndian)
{
short us0;
byte uc0, image_type;
byte pixel_depth, image_desc;
byte buffer[] = new byte[2];

if ((bits_per_pixel == 0)|| (width == 0) || (height== 0))
return false;

pixel_depth = 0;

if ( bits_per_pixel < 256 )
pixel_depth = (byte)(bits_per_pixel & 0xff);
else{
MipavUtil.displayError("ERROR: Wrong bits per pixel inside tga_header");
return false;
}
uc0 = 0;
buffer[0] = uc0;
try {
raFile.write(buffer, 0, 1); /* id_length */
raFile.write(buffer, 0, 1); /* colour_map_type */

image_type = 2; /* Uncompressed. */
buffer[0] = (byte)image_type;
raFile.write(buffer, 0, 1);

us0 = 0;
if (bigEndian) {
  buffer[0] = (byte) (us0 >>> 8);
  buffer[1] = (byte) (us0 & 0xff);
}
else {
	buffer[0] = (byte) (us0 & 0xff);
    buffer[1] = (byte) (us0 >>> 8);
}
raFile.write(buffer, 0, 2); /* colour_map_index */
raFile.write(buffer, 0, 2); /* colour_map_length */
buffer[0] = uc0;
raFile.write(buffer, 0, 1); /* colour_map_entry_size */


if (bigEndian) {
	  buffer[0] = (byte) (us0 >>> 8);
	  buffer[1] = (byte) (us0 & 0xff);
}
else {
	buffer[0] = (byte) (us0 & 0xff);
    buffer[1] = (byte) (us0 >>> 8);
}
raFile.write(buffer, 0, 2); /* x_origin */
raFile.write(buffer, 0, 2); /* y_origin */

if (bigEndian) {
	  buffer[0] = (byte) (width >>> 8);
	  buffer[1] = (byte) (width & 0xff);
}
else {
	buffer[0] = (byte) (width & 0xff);
  buffer[1] = (byte) (width >>> 8);
}
raFile.write(buffer, 0, 2);

if (bigEndian) {
	  buffer[0] = (byte) (height >>> 8);
	  buffer[1] = (byte) (height & 0xff);
}
else {
	buffer[0] = (byte) (height & 0xff);
    buffer[1] = (byte) (height >>> 8);
}
raFile.write(buffer, 0, 2);

buffer[0] = pixel_depth;
raFile.write(buffer, 0, 1);

image_desc = 8; /* 8 bits per component. */

if (flip_image)
image_desc |= 32;

buffer[0] = image_desc;
raFile.write(buffer, 0, 1);
} // try
catch (IOException e) {
	MipavUtil.displayError("IOException on raFile writein write_tgaheader");
	return false;
}

return true;

}

    
    @SuppressWarnings("resource")
	private boolean imagetotga(opj_image_t image, String outfile) {
        int width, height, bpp, x, y;
        boolean write_alpha;
        int i;
        int adjustR;
        int adjustG = 0;
        int adjustB = 0;
        int alpha_channel;
        float r,g,b,a;
        byte value[] = new byte[1];
        float scale;
        File fdest;
        RandomAccessFile raFile;

        try {
   	     fdest = new File(outfile);
       }
       catch (NullPointerException e) {
          MipavUtil.displayError("Null pointer exception on " + outfile);	
          return false;
       }
       try {
           raFile = new RandomAccessFile(fdest, "rw");
       }
       catch (FileNotFoundException e) {
       	MipavUtil.displayError("File not found exception on raFile = new RandomAccessFile(fdest, rw)");
       	return false;
       }
       catch(SecurityException e) {
       	MipavUtil.displayError("Security exception on raFile = new RandomAccessFile(fdest, rw)");
       	return false;	
       }
       // Necessary so that if this is an overwritten file there isn't any
       // junk at the end
       try {
           raFile.setLength(0);
       }
       catch (IOException e) {
       	MipavUtil.displayError("IOException on raFile.setLength(0)");
       	try {
       		raFile.close();
       	}
       	catch (IOException e2) {
       	    MipavUtil.displayError("IOException on raFile.close()");	
       	}
       	return false;
       }
        

        for (i = 0; i < image.numcomps-1; i++)	{
            if ((image.comps[0].dx != image.comps[i+1].dx)
                    ||(image.comps[0].dy != image.comps[i+1].dy)
                    ||(image.comps[0].prec != image.comps[i+1].prec))	{
                MipavUtil.displayError("Unable to create a tga file with such J2K image charateristics.");
                try {
                	raFile.close();
                }
                catch (IOException e) {
                	MipavUtil.displayError("IOException on raFile.close()");
                }
                return false;
            }
        }

        width  = (int)image.comps[0].w;
        height = (int)image.comps[0].h;

        /* Mono with alpha, or RGB with alpha. */
        write_alpha = (image.numcomps==2) || (image.numcomps==4);

        /* Write TGA header  */
        bpp = write_alpha ? 32 : 24;

        boolean flip_image = true;
        boolean bigEndian = false;
        if (!tga_writeheader(raFile, bpp, width , height, flip_image, bigEndian)){
        	try {
            	raFile.close();
            }
            catch (IOException e) {
            	MipavUtil.displayError("IOException on raFile.close()");
            }
        	return false;
        }

        alpha_channel = image.numcomps-1;

        scale = 255.0f / (float)((1<<image.comps[0].prec)-1);

        adjustR = ((image.comps[0].sgnd != 0) ? 1 << (image.comps[0].prec - 1) : 0);
        if (image.numcomps > 2) {
	        adjustG = ((image.comps[1].sgnd != 0) ? 1 << (image.comps[1].prec - 1) : 0);
	        adjustB = ((image.comps[2].sgnd != 0) ? 1 << (image.comps[2].prec - 1) : 0);
        }
        
       try {
    	for (y=0; y < height; y++) 
       {
    	int index= (y*width);

    	for (x=0; x < width; x++, index++)	
      {
    	r = (float)(image.comps[0].data[index] + adjustR);

    	if (image.numcomps > 2) 
     {
    	g = (float)(image.comps[1].data[index] + adjustG);
    	b = (float)(image.comps[2].data[index] + adjustB);
     }
    	else  
     {/* Greyscale ... */
    	g = r;
    	b = r;
     }

    /* TGA format writes BGR ... */
    	if(b > 255.) b = 255.0f; else if(b < 0.) b = 0.0f;
    	value[0] = (byte)(b*scale);
    	raFile.write(value);
    	
    	if(g > 255.) g = 255.0f; else if(g < 0.) g = 0.0f;
    	value[0] = (byte)(g*scale);
    	raFile.write(value);

    	if(r > 255.) r = 255.0f; else if(r < 0.) r = 0.0f;
    	value[0] = (byte)(r*scale);
    	raFile.write(value);


    	if (write_alpha) 
     {
    	a = (float)(image.comps[alpha_channel].data[index]);
    	if(a > 255.) a = 255.0f; else if(a < 0.) a = 0.0f;
    	value[0] = (byte)(a*scale);
    	raFile.write(value);

     }
      }
       }
       } // try 
       catch (IOException e) {
    	   MipavUtil.displayError("IOException on raFile.write(value)");
    	   return false;
       }
  
       return true;
    }


    private int CLAMP(int x,int a,int b) {
    	if (x < a) {
    		return a;
    	}
    	else if (x > b) {
    		return b;
    	}
    	else {
    		return x;
    	}
    }

    		private int clamp(final int value, final int prec, final int sgnd )
    		{
    		  if( sgnd != 0)
    		    {
    		    if (prec <= 8)       return CLAMP(value,-128,127);
    		    else if (prec <= 16) return CLAMP(value,-32768,32767);
    		    else                 return CLAMP(value,-2147483647-1,2147483647);
    		    }
    		  else
    		    {
    		    if (prec <= 8)       return CLAMP(value,0,255);
    		    else if (prec <= 16) return CLAMP(value,0,65535);
    		    else                 return value; /*CLAMP(value,0,4294967295);*/
    		    }
    		}


	private byte[] JavaStrToCStr(String str) {
		int i;
		byte[] stringBytes = null;
		try {
			stringBytes = str.getBytes("ISO-8859-1");
		} catch (UnsupportedEncodingException e) {
			MipavUtil
					.displayError("str.getBytes(ISO-8859-1) gives UnsupportedEncodingException");
			return null;
		}
		byte[] ntBytes = new byte[stringBytes.length + 1];
		for (i = 0; i < stringBytes.length; i++) {
			ntBytes[i] = stringBytes[i];
		}
		return ntBytes;
	}

	private String opj_version() {
		String OPJ_PACKAGE_VERSION = "2.1.0";

		return OPJ_PACKAGE_VERSION;
	}

}