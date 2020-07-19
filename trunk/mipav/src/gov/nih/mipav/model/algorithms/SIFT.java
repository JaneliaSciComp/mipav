package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

/**
 * This is a port of
 *  @file     sift.c and supporting routines by
 * @author   Andrea Vedaldi
 * @brief    Scale Invariant Feature Transform (SIFT) - Driver
 */

/**
Copyright (C) 2007-12 Andrea Vedaldi and Brian Fulkerson.
All rights reserved.

This file is part of the VLFeat library and is made available under
the terms of the BSD license shown below.

#define VL_SIFT_DRIVER_VERSION 0.1
*/

/**
Copyright (C) 2007-11, Andrea Vedaldi and Brian Fulkerson
Copyright (C) 2012-13, The VLFeat Team
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

public class SIFT extends AlgorithmBase {
	// Error handling
	private final int VL_ERR_OK = 0;  /**< No error */
	private final int VL_ERR_OVERFLOW = 1;  /**< Buffer overflow error */
	private final int VL_ERR_ALLOC =  2;  /**< Resource allocation error */
	private final int VL_ERR_BAD_ARG = 3;  /**< Bad argument or illegal data error */
	private final int VL_ERR_IO = 4;  /**< Input/output error */
	private final int VL_ERR_EOF = 5;  /**< End-of-file or end-of-sequence error */
	//#define VL_ERR_NO_MORE  5  /**< End-of-sequence @deprecated */
	
	// File protocols
	private final int VL_PROT_UNKNOWN = -1; /**< unknown protocol */
    private final int VL_PROT_NONE =  0; /**< no protocol      */
    private final int VL_PROT_ASCII = 1;    /**< ASCII protocol   */
    private final int VL_PROT_BINARY = 2;   /**< Binary protocol  */
    
    private final int VL_ERR_PGM_INV_HEAD = 101; /**< Invalid PGM header section. */
    private final int VL_ERR_PGM_INV_META = 102; /**< Invalid PGM meta section. */
    private final int VL_ERR_PGM_INV_DATA = 103; /**< Invalid PGM data section.*/
    private final int VL_ERR_PGM_IO = 104; /**< Generic I/O error. */
    
    private final int EOF = -1;
    
    private String fileDir[];
    private String fileName[];
    private int fileNum;
    private boolean verbose = false;
    // arg sets protocol[0], which is default ascii to ascii or binary
    // and the pattern which is the basename.prefix
    private String outarg = null;
    private String framesarg = null;
    private String descriptorarg = null;
    private String metaarg = null;
    private String read_framesarg = null;
    private String gssarg = null;
    // Octaves
    private int O = 3;
    // Levels
    private int S = 3;
    // first_octave
    private int  omin = 0 ;
    private double   edge_thresh  = 10 ;
    private double   peak_thresh  = 0;
    private double   magnif       = 3.0 ;
    private boolean force_orientations = false;
    private boolean writeFrames = false;
    private boolean readFrames = false;
    private boolean writeDescriptor = false;
    private boolean writeMeta = false;
    private boolean writeGss = false;
    
    private int NBO = 8;
    private int NBP = 4;
    private int EXPN_SZ  = 256;          /**< ::fast_expn table size @internal */
    private double EXPN_MAX = 25.0;         /**< ::fast_expn table max  @internal */
    private double expn_tab[] = new double[EXPN_SZ+1] ; /**< ::fast_expn table   
    
    /** @name Image convolution flags
 ** @{ */
    private int VL_PAD_BY_ZERO   =    (0x0 << 0); /**< @brief Pad with zeroes. */
    private int VL_PAD_BY_CONTINUITY = (0x1 << 0); /**< @brief Pad by continuity. */
    private int VL_PAD_MASK = (0x3);     /**< @brief Padding field selector. */
    private int VL_TRANSPOSE = (0x1 << 2); /**< @brief Transpose result. */
    
    private float VL_EPSILON_F = 1.19209290E-07F;
    private double VL_EPSILON_D = 2.220446049250313e-16;
    
    /** @internal @brief Use bilinear interpolation to compute orientations */
    private boolean VL_SIFT_BILINEAR_ORIENTATIONS = true;
	
	/**
     * SIFT - default constructor.
     */
    public SIFT() { }
    
    public SIFT(String fileDir[], String fileName[], boolean verbose, String outarg, String framesarg,
    		String descriptorarg, String metaarg, String read_framesarg, String gssarg, int O, int S,
    		int omin, double edge_thresh, double peak_thresh, double magnif, boolean force_orientations,
    		boolean writeFrames, boolean readFrames, boolean writeDescriptor, boolean writeMeta,
    		boolean writeGss) {
        this.fileDir = fileDir;
        this.fileName = fileName;
        this.verbose = verbose;
        this.outarg = outarg;
        this.framesarg = framesarg;
        this.descriptorarg = descriptorarg;
        this.metaarg = metaarg;
        this.read_framesarg = read_framesarg;
        this.gssarg = gssarg;
        this.O = O;
        this.S = S;
        this.omin = omin;
        this.edge_thresh = edge_thresh;
        this.peak_thresh = peak_thresh;
        this.magnif = magnif;
        this.force_orientations = force_orientations;
        this.writeFrames = writeFrames;
        this.readFrames = readFrames;
        this.writeDescriptor = writeDescriptor;
        this.writeMeta = writeMeta;
        this.writeGss = writeGss;
    }


    /** @brief SIFT driver entry point
     **/
    public void runAlgorithm() {
    	
    	  boolean testMosaic = false;
    	  if (testMosaic) {
    		  ModelImage out = sift_mosaic(null, null);
    		  setCompleted(true);
    		  return;
    	  }

    	  int  err    = VL_ERR_OK ;
    	  String err_msg;
    	  int      n ;
    	  int      exit_code          = 0 ;
    	  boolean  force_output       = false ;
    	  String basename;
    	  String name;
    	  int q;
    	  RandomAccessFile in = null;
    	  RandomAccessFile ifrFile = null;
    	  RandomAccessFile outFile = null;
    	  RandomAccessFile frmFile = null;
    	  RandomAccessFile dscFile = null;
    	  RandomAccessFile metFile = null;
    	  int data[] = null;
    	  float fdata[] = null;
    	  int error[] = new int[1];
    	  
    	  // All files use ascii protocol
    	  VlFileMeta out  = new VlFileMeta(true, "%.sift",  new int[]{VL_PROT_ASCII}, "", null);
    	  VlFileMeta frm;
    	  if (writeFrames) {
    		  frm  = new VlFileMeta(true, "%.frame", new int[]{VL_PROT_ASCII}, "", null);  
    	  }
    	  else {
    		  frm  = new VlFileMeta(false, "%.frame", new int[]{VL_PROT_ASCII}, "", null);    
    	  }
    	  VlFileMeta dsc;
    	  if (writeDescriptor) {
    		  dsc  = new VlFileMeta(true, "%.descr", new int[]{VL_PROT_ASCII}, "", null);  
    	  }
    	  else {
    	      dsc  = new VlFileMeta(false, "%.descr", new int[]{VL_PROT_ASCII}, "", null);
    	  }
    	  VlFileMeta met;
    	  if (writeMeta) {
    		  met  = new VlFileMeta(true, "%.meta",  new int[]{VL_PROT_ASCII}, "", null);  
    	  }
    	  else {
    	      met  = new VlFileMeta(false, "%.meta",  new int[]{VL_PROT_ASCII}, "", null);
    	  }
    	  VlFileMeta gss;
    	  if (writeGss) {
    		  gss  = new VlFileMeta(true, "%_gss.pgm",   new int[]{VL_PROT_ASCII}, "", null);  
    	  }
    	  else {
    	      gss  = new VlFileMeta(false, "%_gss.pgm",   new int[]{VL_PROT_ASCII}, "", null);
    	  }
    	  VlFileMeta ifr;
    	  if (readFrames) {
    		  ifr  = new VlFileMeta(true, "%.frame", new int[]{VL_PROT_ASCII}, "", null);  
    	  }
    	  else {
    	      ifr  = new VlFileMeta(false, "%.frame", new int[]{VL_PROT_ASCII}, "", null);
    	  }
    	  
    	  if ((outarg != null) && (outarg.length() > 0)) {
    	     err = vl_file_meta_parse (out, outarg) ;
    	     if (err > 0) {
    	    	 MipavUtil.displayError("outarg " + outarg + " is invalid");
    	    	 setCompleted(false);
    	    	 return;
    	     }
    	     force_output = true;
    	  }
    	  
    	  if ((framesarg != null) && (framesarg.length() > 0)) {
     	     err = vl_file_meta_parse (frm, framesarg) ;
     	     if (err > 0) {
     	    	 MipavUtil.displayError("framesarg " + framesarg + " is invalid");
     	    	 setCompleted(false);
     	    	 return;
     	     }
     	  }
    	  
    	  if ((descriptorarg != null) && (descriptorarg.length() > 0)) {
      	     err = vl_file_meta_parse (dsc, descriptorarg) ;
      	     if (err > 0) {
      	    	 MipavUtil.displayError("descriptorarg " + descriptorarg + " is invalid");
      	    	 setCompleted(false);
      	    	 return;
      	     }
      	  }
    	  
    	  if ((metaarg != null) && (metaarg.length() > 0)) {
       	     err = vl_file_meta_parse (met, metaarg) ;
       	     if (err > 0) {
       	    	 MipavUtil.displayError("metaarg " + metaarg + " is invalid");
       	    	 setCompleted(false);
       	    	 return;
       	     }
       	     
	       	  if (met.protocol[0] != VL_PROT_ASCII) {
	              MipavUtil.displayError("meta file supports only ASCII protocol") ;
	              setCompleted(false);
	              return;
	       	  }
       	  }
    	  
    	  if ((read_framesarg != null) && (read_framesarg.length() > 0)) {
      	     err = vl_file_meta_parse (ifr, read_framesarg) ;
      	     if (err > 0) {
      	    	 MipavUtil.displayError("read_framesarg " + read_framesarg + " is invalid");
      	    	 setCompleted(false);
      	    	 return;
      	     }
      	  }
    	  
    	  if ((gssarg != null) && (gssarg.length() > 0)) {
      	     err = vl_file_meta_parse (gss, gssarg) ;
      	     if (err > 0) {
      	    	 MipavUtil.displayError("gssarg " + gssarg + " is invalid");
      	    	 setCompleted(false);
      	    	 return;
      	     }
      	  }
    	  
    	  if (O < 0) {
    		  MipavUtil.displayError("Octaves = " + O + " must not be less than 0");
    		  setCompleted(false);
    		  return;
    	  }
    	  
    	  if (S < 0) {
    		  MipavUtil.displayError("Levels = " + S + " must not be less than 0");
    		  setCompleted(false);
    		  return;
    	  }
    	  
    	  if (edge_thresh < 1) {
    		  MipavUtil.displayError("edge_thresh = " + edge_thresh + " must not be less than 1");
    		  setCompleted(false);
    		  return;
    	  }
    	  
    	  if (peak_thresh < 0) {
    		  MipavUtil.displayError("peak_thresh = " + peak_thresh + " must not be less than 0");
    		  setCompleted(false);
    		  return;
    	  }
    	  
    	  if (magnif < 1) {
    		  MipavUtil.displayError("magnif = " + magnif + " must not be less than 1");
    		  setCompleted(false);
    		  return;
    	  }
    	  
    	  /*
    	     if --output is not specified, specifying --frames or --descriptors
    	     prevent the aggregate outout file to be produced.
    	  */
    	  if (! force_output && (frm.active || dsc.active)) {
    	    out.active = false;
    	  }
    	  
    	  if (verbose) {
    		  PRNFO("write aggregate . ", out) ;
    		    PRNFO("write frames .... ", frm) ;
    		    PRNFO("write descriptors ", dsc) ;
    		    PRNFO("write meta ...... ", met) ;
    		    PRNFO("write GSS ....... ", gss) ;
    		    PRNFO("read  frames .... ", ifr) ;

    		    if (force_orientations) {
    		      System.out.println("sift: will compute orientations") ;
    		      Preferences.debug("sift: will compute orientations\n", Preferences.DEBUG_ALGORITHM);
    		    }
    	  } // if (verbose)
    	  
    	  /* ------------------------------------------------------------------
    	   *                                         Process one image per time
    	   * --------------------------------------------------------------- */
    	  
	       for (fileNum = 0; fileNum < fileName.length; fileNum++) {
	    	  ArrayList <double[]> ikeys = null;
	    	  int nikeys = 0;
    		  int ikeys_size = 0;
    		  VlSiftFilt      filt =null; 
	    	  bigloop: while(true) {
	    		  VlPgmImage pim = new VlPgmImage();
	    		  int i;
	    		  boolean first;
	    	      // Get basename from fileName[fileNum]
	    		  name = fileName[fileNum];
	    		  basename = "";
	    		  int num[] = new int[1];
	    		  basename = vl_string_basename (basename, 1024, name, 1,num) ;
	    		  
	    		  if (num[0] >= 1024) {
	    			  err = 1;
	    		  }

    		      if (err >= 1) {
    		          System.err.println("Basename of " + name + " is too long");
    		          Preferences.debug("Basename of " + name + " is too long\n", Preferences.DEBUG_ALGORITHM);
    		          err = VL_ERR_OVERFLOW ;
    		          break bigloop;
    		      }
    		      
    		      if (verbose) {
    		          System.out.println("sift name: <== " + name);
    		          Preferences.debug("sift name: <== " + name + "\n", Preferences.DEBUG_ALGORITHM);
    		          System.out.println("sift: basename is " + basename);
    		          Preferences.debug("sift: basename is " + basename + "\n", Preferences.DEBUG_ALGORITHM);
    		      }

    		      File namefile = new File(fileDir[fileNum] + File.separator + fileName[fileNum]);
    				
    		      // Open input file
    		      try {
    		    	  // Binary file
    			      in = new RandomAccessFile(namefile, "r");
    			  } catch (IOException e) {
    					System.err.println("IOException " + e + " on in = new RandomAccessFile(namefile, r)");
    				    Preferences.debug("IOException " + e + " on in = new RandomAccessFile(namefile, r)\n",Preferences.DEBUG_ALGORITHM);
    				    break bigloop;
    			  }
    		      
    		      /* ...............................................................
    		       *                                                       Read data
    		       * ............................................................ */
    		      int fileType = FileUtility.getFileTypeFromStr(basename);
    		      if (fileType == FileUtility.PGM) {

	    		      /* read PGM header */
	    		      err = vl_pgm_extract_head (in, pim) ;
	    		      if (err > 0) {
	    		    	  break bigloop;
	    		      }
	    		      
	    		      if (verbose) {
	    		          System.out.println("sift: image is " + pim.width + " by " + pim.height + " pixels") ;
	    		          Preferences.debug("sift: image is " + pim.width + " by " + pim.height + " pixels\n", Preferences.DEBUG_ALGORITHM);
	    		      }
	    		      
	    		      int numPixels = pim.width * pim.height;
	    		      try {
	    		          data = new int[numPixels];
	    		          fdata = new float[numPixels];
	    		      }
	    		      catch (OutOfMemoryError e) {
	    		    	  System.err.println("Out of memory error allocating data and fdata");
	    		    	  Preferences.debug("Out of memory error allocating data and fdata\n", Preferences.DEBUG_ALGORITHM);
	    		    	  break bigloop;
	    		      }
	    		      
	    		      /* read PGM body */
	    		      err  = vl_pgm_extract_data(in, pim, data) ;
	    		      if (err > 0) {
	    		    	  break bigloop;
	    		      }
	    		      
	    		      /* convert data type */
	    		      for (q = 0 ; q < numPixels; ++q) {
	    		        fdata [q] = data[q] ;
	    		      }
    		      } // if (fileType == FileUtility.PGM)
    		      
    		      /* ...............................................................
    		       *                                     Optionally source keypoints
    		       * ............................................................ */

    		      if (ifr.active) {

    		          /* open file */
    		          ifrFile = vl_file_meta_open (ifr, basename, "r", error);
    		          if (error[0] > 0) {
    		        	  if (error[0] == VL_ERR_OVERFLOW) {
    		        		  System.err.println("ifrFile file name too long");
    		        		  Preferences.debug("ifrFile file name too long\n", Preferences.DEBUG_ALGORITHM);
    		        	  }
    		        	  else {
    		        		  System.err.println("Could not open " + ifr.name + " for reading");
    		        		  Preferences.debug("Could not open " + ifr.name + " for reading\n", Preferences.DEBUG_ALGORITHM);
    		        	  }
    		        	  break bigloop;
    		          }
    		          
    		          while (true) {
    		              double x[] = new double[1];
    		              double y[] = new double[1];
    		              double s[] = new double[1];
    		              double th[] = new double[1];

    		              /* read next guy */
    		              err = vl_file_meta_get_double (ifr.protocol[0], ifrFile, x);
    		              if (err == VL_ERR_EOF) break;
    		              if (err > 0) {
    		            	  System.err.println(ifr.name + " malformed");
    		            	  Preferences.debug(ifr.name + " malformed\n", Preferences.DEBUG_ALGORITHM);
    		            	  err = VL_ERR_IO;
    		            	  break bigloop;
    		              }
    		              err = vl_file_meta_get_double(ifr.protocol[0], ifrFile, y); 
    		              if (err > 0) {
    		            	  System.err.println(ifr.name + " malformed");
    		            	  Preferences.debug(ifr.name + " malformed\n", Preferences.DEBUG_ALGORITHM);
    		            	  err = VL_ERR_IO;
    		            	  break bigloop;
    		              }
    		              err = vl_file_meta_get_double(ifr.protocol[0], ifrFile, s); 
    		              if (err > 0) {
    		            	  System.err.println(ifr.name + " malformed");
    		            	  Preferences.debug(ifr.name + " malformed\n", Preferences.DEBUG_ALGORITHM);
    		            	  err = VL_ERR_IO;
    		            	  break bigloop;
    		              }
    		              err = vl_file_meta_get_double (ifr.protocol[0], ifrFile, th);
    		              if (err == VL_ERR_EOF) break;
    		              if (err > 0) {
    		            	  System.err.println(ifr.name + " malformed");
    		            	  Preferences.debug(ifr.name + " malformed\n", Preferences.DEBUG_ALGORITHM);
    		            	  err = VL_ERR_IO;
    		            	  break bigloop;
    		              }
    		              
    		              /* make enough space */
    		              
    		              
    		              /* add the guy to the buffer */
    		              double key[] = new double[] {x[0],y[0],s[0],th[0]};
    		              if (ikeys == null) {
    		            	  ikeys = new ArrayList<double[]>();
    		              }
    		              ikeys.add(key);

    		              ++ nikeys ;
    		          } // while (true)
    		          
    		          
    		          /* now order by scale */
    		          /* ----------------------------------------------------------------- */
    		          /** @brief Keypoint ordering
    		           ** @internal
    		           **/
    		          //int
    		          //korder (void const* a, void const* b) {
    		            //double x = ((double*) a) [2] - ((double*) b) [2] ;
    		            //if (x < 0) return -1 ;
    		            //if (x > 0) return +1 ;
    		            //return 0 ;
    		          //}
    		          // Now order by scale
    		          Collections.sort(ikeys, new ikeysComparator());

    		          if (verbose) {
    		            System.out.println("sift: read " + nikeys + " keypoints from " + ifr.name);
    		            Preferences.debug("sift: read " + nikeys + " keypoints from " + ifr.name + "\n", Preferences.DEBUG_ALGORITHM);
    		          }

    		          /* close file */
    		          if (ifrFile != null) {
    		    		  try {
    		    	    	  ifrFile.close();
    		    	      }
    		    	      catch (IOException e) {
    							System.err.println("IOException " + e + " on ifrFile.close()");
    						    Preferences.debug("IOException " + e + " on ifrFile.close()\n",Preferences.DEBUG_ALGORITHM);
    						}  
    		    	  }
    		    	  vl_file_meta_close (ifr) ;
    		      } // if (ifr.active)
    		      
    		      /* ...............................................................
    		       *                                               Open output files
    		       * ............................................................ */

    		      outFile = vl_file_meta_open (out, basename, "rw",error) ;
    		      if (error[0] > 0) {
		        	  if (error[0] == VL_ERR_OVERFLOW) {
		        		  System.err.println("outFile file name too long");
		        		  Preferences.debug("outFile file name too long\n", Preferences.DEBUG_ALGORITHM);
		        	  }
		        	  else {
		        		  System.err.println("Could not open " + out.name + " for writing");
		        		  Preferences.debug("Could not open " + out.name + " for writing\n", Preferences.DEBUG_ALGORITHM);
		        	  }
		        	  break bigloop;
		          }
    		      dscFile = vl_file_meta_open (dsc, basename, "rw",error) ;
    		      if (error[0] > 0) {
		        	  if (error[0] == VL_ERR_OVERFLOW) {
		        		  System.err.println("dscFile file name too long");
		        		  Preferences.debug("dscFile file name too long\n", Preferences.DEBUG_ALGORITHM);
		        	  }
		        	  else {
		        		  System.err.println("Could not open " + dsc.name + " for writing");
		        		  Preferences.debug("Could not open " + dsc.name + " for writing\n", Preferences.DEBUG_ALGORITHM);
		        	  }
		        	  break bigloop;
		          }
    		      frmFile = vl_file_meta_open (frm, basename, "rw",error) ;
    		      if (error[0] > 0) {
		        	  if (error[0] == VL_ERR_OVERFLOW) {
		        		  System.err.println("frmFile file name too long");
		        		  Preferences.debug("frmFile file name too long\n", Preferences.DEBUG_ALGORITHM);
		        	  }
		        	  else {
		        		  System.err.println("Could not open " + frm.name + " for writing");
		        		  Preferences.debug("Could not open " + frm.name + " for writing\n", Preferences.DEBUG_ALGORITHM);
		        	  }
		        	  break bigloop;
		          }
    		      metFile = vl_file_meta_open (met, basename, "rw", error) ;
    		      if (error[0] > 0) {
		        	  if (error[0] == VL_ERR_OVERFLOW) {
		        		  System.err.println("metFile file name too long");
		        		  Preferences.debug("metFile file name too long\n", Preferences.DEBUG_ALGORITHM);
		        	  }
		        	  else {
		        		  System.err.println("Could not open " + met.name + " for writing");
		        		  Preferences.debug("Could not open " + met.name + " for writing\n", Preferences.DEBUG_ALGORITHM);
		        	  }
		        	  break bigloop;
		          }
    		      if (verbose) {
    		        if (out.active) {
    		        	System.out.println("sift: writing all ....... to " + out.name);
    		        	Preferences.debug("sift: writing all ....... to " + out.name + "\n", Preferences.DEBUG_ALGORITHM);
    		        }
    		        if (frm.active) {
    		        	System.out.println("sift: writing frames .... to " + frm.name);
    		        	Preferences.debug("sift: writing frames .... to " + frm.name + "\n", Preferences.DEBUG_ALGORITHM);
    		        }
    		        if (dsc.active) {
    		        	System.out.println("sift: writing descriptors to " + dsc.name);
    		        	Preferences.debug("sift: writing descriptors to " + dsc.name + "\n", Preferences.DEBUG_ALGORITHM);
    		        }
    		        if (met.active) {
    		        	System.out.println("sift: writing meta ...... to " + met.name);
    		        	Preferences.debug("sift: writing meta ...... to " + met.name + "\n", Preferences.DEBUG_ALGORITHM);
    		        }
    		      } // if (verbose)
    		      
    		      /* ...............................................................
    		       *                                                     Make filter
    		       * ............................................................ */

    		      filt = vl_sift_new (pim.width, pim.height, O, S, omin) ;
    		      
    		      if (filt  == null) {
      		        System.err.println("Could not create SIFT filter.") ;
      		        Preferences.debug("Could not create SIFT filter\n", Preferences.DEBUG_ALGORITHM);
      		        err = VL_ERR_ALLOC ;
      		        break bigloop;
      		      }

    		      if (edge_thresh >= 0) filt.edge_thresh = edge_thresh;
    		      if (peak_thresh >= 0) filt.peak_thresh = peak_thresh;
    		      if (magnif      >= 0) filt.magnif = magnif;

    		      if (verbose) {
    		        System.out.println("sift: filter settings:") ;
    		        Preferences.debug("sift: filter settings:\n", Preferences.DEBUG_ALGORITHM);
    		        System.out.println("sift:   number of octaves = " + filt.O);
    		        Preferences.debug("sift:   number of octaves = " + filt.O + "\n", Preferences.DEBUG_ALGORITHM);
    		        System.out.println("sift:   number of levels per octave = " + filt.S);
    		        Preferences.debug("sift:   number of levels per octave = " + filt.S + "\n", Preferences.DEBUG_ALGORITHM);
    		        System.out.println("sift:   index of the first octave = " + filt.o_min) ;
    		        Preferences.debug("sift:   index of the first octave = " + filt.o_min + "\n", Preferences.DEBUG_ALGORITHM);
    		        System.out.println("sift:   edge threshold = " + filt.edge_thresh);
    		        Preferences.debug("sift:   edge threshold = " + filt.edge_thresh + "\n", Preferences.DEBUG_ALGORITHM);
    		        System.out.println("sift:   peak threshold = " + filt.peak_thresh);
    		        Preferences.debug("sift:   peak threshold = " + filt.peak_thresh + "\n", Preferences.DEBUG_ALGORITHM);
    		        System.out.println("sift:   magnification factor = " + filt.magnif);
    		        Preferences.debug("sift:   magnification factor = " + filt.magnif + "\n", Preferences.DEBUG_ALGORITHM);
    		        if (ikeys != null) {
    		        	System.out.println("sift: will source frames? yes");
    		        	Preferences.debug("sift: will source frames? yes\n", Preferences.DEBUG_ALGORITHM);
    		        }
    		        else {
    		        	System.out.println("sift: will source frames? no");
    		        	Preferences.debug("sift: will source frames? no\n", Preferences.DEBUG_ALGORITHM);	
    		        }
    		        if (force_orientations) {
    		            System.out.println("sift: will force orientations? yes");
    		            Preferences.debug("sift: will force orientations? yes\n", Preferences.DEBUG_ALGORITHM);
    		        }
    		        else {
    		        	System.out.println("sift: will force orientations? no");
    		            Preferences.debug("sift: will force orientations? no\n", Preferences.DEBUG_ALGORITHM);      	
    		        }
    		      } // if (verbose)
    		      
    		      /* ...............................................................
    		       *                                             Process each octave
    		       * ............................................................ */
    		      i     = 0 ;
    		      first = true ;
    		      while (true) {
    		        VlSiftKeypoint keys[] = null ;
    		        int                   nkeys ;

    		        /* calculate the GSS for the next octave .................... */
    		        if (first) {
    		          first = false ;
    		          err = vl_sift_process_first_octave (filt, fdata) ;
    		        } else {
    		          err = vl_sift_process_next_octave  (filt) ;
    		        }

    		        if (err > 0) {
    		          err = VL_ERR_OK ;
    		          break ;
    		        }

    		        if (verbose) {
    		          System.out.println("sift: GSS octave " + filt.o_cur + " computed");
    		          Preferences.debug("sift: GSS octave " + filt.o_cur + " computed\n", Preferences.DEBUG_ALGORITHM);
    		        }

    		        /* optionally save GSS */
    		        if (gss.active) {
    		          err = save_gss (filt, gss, basename, verbose) ;
    		          if (err > 0) {
    		            System.err.println("Could not write GSS to PGM file.") ;
    		            Preferences.debug("Could not write GSS to PGM file.\n", Preferences.DEBUG_ALGORITHM);
    		            break bigloop;
    		          }
    		        }
    		        
    		        /* run detector ............................................. */
    		        if (ikeys == null) {
    		          vl_sift_detect (filt) ;

    		          keys  = filt.keys;
    		          nkeys = filt.nkeys ;
    		          i     = 0 ;

    		          if (verbose) {
    		            System.out.println("sift: detected " + nkeys + " (unoriented) keypoints") ;
    		            Preferences.debug("sift: detected " + nkeys + " (unoriented) keypoints\n", Preferences.DEBUG_ALGORITHM);
    		          }
    		        } else {
    		          nkeys = nikeys ;
    		        }
    		        

    		        /* for each keypoint ........................................ */
    		        for (; i < nkeys ; ++i) {
    		          double                angles[] = new double[4] ;
    		          int                   nangles ;
    		          VlSiftKeypoint        ik = new VlSiftKeypoint();
    		          VlSiftKeypoint k ;

    		          /* obtain keypoint orientations ........................... */
    		          if (ikeys != null) {
    		            vl_sift_keypoint_init (filt, ik,
    		            		               ikeys.get(i)[0],
    		                                   ikeys.get(i)[1],
    		                                   ikeys.get(i)[2]) ;

    		            if (ik.o != filt.o_cur) {
    		              break ;
    		            }

    		            k          = ik ;

    		            /* optionally compute orientations too */
    		            if (force_orientations) {
    		              nangles = vl_sift_calc_keypoint_orientations
    		                (filt, angles, k) ;
    		            } else {
    		              angles [0] = ikeys.get(i)[3] ;
    		              nangles    = 1 ;
    		            }
    		          } else {
    		            k = keys[i] ;
    		            nangles = vl_sift_calc_keypoint_orientations
    		              (filt, angles, k) ;
    		          }
    		          
    		          /* for each orientation ................................... */
    		          for (q = 0 ; q < nangles ; ++q) {
    		            float descr[] = new float [128] ;

    		            /* compute descriptor (if necessary) */
    		            if (out.active || dsc.active) {
    		              vl_sift_calc_keypoint_descriptor
    		                (filt, descr, k, angles [q]) ;
    		            }

    		            if (out.active) {
    		              int l ;
    		              err = vl_file_meta_put_double(out.protocol[0], outFile, (double)k.x);
    		              if (err != 0) {
    		            	  break bigloop;
    		              }
    		              err = vl_file_meta_put_double(out.protocol[0], outFile, (double)k.y);
    		              if (err != 0) {
    		            	  break bigloop;
    		              }
    		              err = vl_file_meta_put_double(out.protocol[0], outFile, (double)k.sigma);
    		              if (err != 0) {
    		            	  break bigloop;
    		              }
    		              err = vl_file_meta_put_double(out.protocol[0], outFile, angles[q]);
    		              if (err != 0) {
    		            	  break bigloop;
    		              }
    		              for (l = 0 ; l < 128 ; ++l) {
    		            	if (descr[l] < 0) {
    		            		err = vl_file_meta_put_uint8(out.protocol[0], outFile, (byte)(512.0 * descr[l] - 0.5));
    		            	}
    		            	else {
    		            		err = vl_file_meta_put_uint8(out.protocol[0], outFile, (byte)(512.0 * descr[l] + 0.5));	
    		            	}
    		            	if (err != 0) {
    		            		break bigloop;
    		            	}
    		              }
    		              if (out.protocol[0] == VL_PROT_ASCII) {
    		            	  try {
    		            		  outFile.writeByte(10); // 10 is ascii for new line
    		            	  }
    		            	  catch (IOException e) {
    		            		  System.err.println("IOException " + e + " writing newline to outFile");
    		            		  Preferences.debug("IOException " + e + " writing newline to outFile\n", Preferences.DEBUG_ALGORITHM);
    		            		  break bigloop;
    		            	  }
    		              }
    		            }

    		            if (frm.active) {
    		              err = vl_file_meta_put_double (frm.protocol[0], frmFile, (double)k. x     ) ;
    		              if (err != 0) {
    		            	  break bigloop;
    		              }
    		              err = vl_file_meta_put_double (frm.protocol[0], frmFile, (double)k. y     ) ;
    		              if (err != 0) {
    		            	  break bigloop;
    		              }
    		              err = vl_file_meta_put_double (frm.protocol[0], frmFile, (double)k. sigma     ) ;
    		              if (err != 0) {
    		            	  break bigloop;
    		              }
    		              err = vl_file_meta_put_double (frm.protocol[0], frmFile,angles[q]     ) ;
    		              if (err != 0) {
    		            	  break bigloop;
    		              }
    		              if (frm.protocol[0] == VL_PROT_ASCII) {
    		            	  try {
    		            		  frmFile.writeByte(10); // 10 is ascii for new line
    		            	  }
    		            	  catch (IOException e) {
    		            		  System.err.println("IOException " + e + " writing newline to frmFile");
    		            		  Preferences.debug("IOException " + e + " writing newline to frmFile\n", Preferences.DEBUG_ALGORITHM);
    		            		  break bigloop;
    		            	  }
    		              }
    		            }

    		            if (dsc.active) {
    		              int l ;
    		              for (l = 0 ; l < 128 ; ++l) {
    		                double x = 512.0 * descr[l] ;
    		                x = (x < 255.0) ? x : 255.0 ;
    		                if (x < 0) {
    		            		err = vl_file_meta_put_uint8(dsc.protocol[0], dscFile, (byte)(x - 0.5));
    		            	}
    		            	else {
    		            		err = vl_file_meta_put_uint8(dsc.protocol[0], dscFile, (byte)(x + 0.5));	
    		            	}
    		            	if (err != 0) {
    		            		break bigloop;
    		            	}
    		              }
    		              if (dsc.protocol[0] == VL_PROT_ASCII) {
    		            	  try {
    		            		  dscFile.writeByte(10); // 10 is ascii for new line
    		            	  }
    		            	  catch (IOException e) {
    		            		  System.err.println("IOException " + e + " writing newline to dscFile");
    		            		  Preferences.debug("IOException " + e + " writing newline to dscFile\n", Preferences.DEBUG_ALGORITHM);
    		            		  break bigloop;
    		            	  }
    		              }
    		            }
    		          }
    		        } // for (; i < nkeys ; ++i)
    		      } // while (true)

    		      /* ...............................................................
    		       *                                                       Finish up
    		       * ............................................................ */

    		      if (met.active) {
    		    	  byte metBytes[];
    		    	  metBytes = "<sift\n".getBytes();
    		          try {
    		        	  metFile.write(metBytes);
    		          }
    		          catch (IOException e) {
    		        	  System.err.println("IOException " + e + " writing <sift\n to metFile");
    		        	  Preferences.debug("IOException " + e + " writing <sift\n to metFile\n", Preferences.DEBUG_ALGORITHM);
    		        	  break bigloop;
    		          }
    		        if (dsc.active) {
    		          metBytes = ("  descriptors = " + dsc.name + "\n").getBytes();
    		          try {
    		        	  metFile.write(metBytes);
    		          }
    		          catch (IOException e) {
    		        	  System.err.println("IOException " + e + " writing  descriptors = dsc.name to metFile");
    		        	  Preferences.debug("IOException " + e + " writing descriptors = dsc.name to metFile\n", Preferences.DEBUG_ALGORITHM);
    		        	  break bigloop;
    		          }
    		        }
    		        if (frm.active) {
    		          metBytes = ("  frames = " + frm.name + "\n").getBytes();
    		          try {
    		        	  metFile.write(metBytes);
    		          }
    		          catch (IOException e) {
    		        	  System.err.println("IOException " + e + " writing  frames = frm.name to metFile");
    		        	  Preferences.debug("IOException " + e + " writing frames = frm.name to metFile\n", Preferences.DEBUG_ALGORITHM);
    		        	  break bigloop;
    		          }
    		        }
    		        metBytes = ">\n".getBytes();
    		        try {
  		        	  metFile.write(metBytes);
  		          }
  		          catch (IOException e) {
  		        	  System.err.println("IOException " + e + " writing >\n to metFile");
  		        	  Preferences.debug("IOException " + e + " writing >\n to metFile\n", Preferences.DEBUG_ALGORITHM);
  		        	  break bigloop;
  		          }
    		      }
    		      break bigloop;
    	  } // bigloop: while(true)
	       
	      if (ikeys != null) {
	    	  ikeys.clear();
	    	  ikeys = null;
	    	  ikeys_size = nikeys = 0 ;
	      }
	      
	      /* release filter */
	      if (filt != null) {
	        vl_sift_delete (filt) ;
	      }
    	  
    	  if (data != null) {
    		  data = null;
    	  }
    	  
    	  if (fdata != null) {
    		  fdata = null;
    	  }
    	  
    	  if (in != null) {
    	      try {
    	    	  in.close();
    	      }
    	      catch (IOException e) {
					System.err.println("IOException " + e + " on in.close()");
				    Preferences.debug("IOException " + e + " on in.close()\n",Preferences.DEBUG_ALGORITHM);
				}
    	  }
    	  
    	  if (outFile != null) {
    		  try {
    	    	  outFile.close();
    	      }
    	      catch (IOException e) {
					System.err.println("IOException " + e + " on outFile.close()");
				    Preferences.debug("IOException " + e + " on outFile.close()\n",Preferences.DEBUG_ALGORITHM);
				}  
    	  }
    	  vl_file_meta_close (out) ;
    	  if (frmFile != null) {
    		  try {
    	    	  frmFile.close();
    	      }
    	      catch (IOException e) {
					System.err.println("IOException " + e + " on frmFile.close()");
				    Preferences.debug("IOException " + e + " on frmFile.close()\n",Preferences.DEBUG_ALGORITHM);
				}  
    	  }
    	  vl_file_meta_close (frm) ;
    	  if (dscFile != null) {
    		  try {
    	    	  dscFile.close();
    	      }
    	      catch (IOException e) {
					System.err.println("IOException " + e + " on dscFile.close()");
				    Preferences.debug("IOException " + e + " on dscFile.close()\n",Preferences.DEBUG_ALGORITHM);
				}  
    	  }
    	  vl_file_meta_close (dsc) ;
    	  if (metFile != null) {
    		  try {
    	    	  metFile.close();
    	      }
    	      catch (IOException e) {
					System.err.println("IOException " + e + " on metFile.close()");
				    Preferences.debug("IOException " + e + " on metFile.close()\n",Preferences.DEBUG_ALGORITHM);
				}  
    	  }
    	  vl_file_meta_close (met) ;
    	  vl_file_meta_close (gss) ;
    	  if (ifrFile != null) {
    		  try {
    	    	  ifrFile.close();
    	      }
    	      catch (IOException e) {
					System.err.println("IOException " + e + " on ifrFile.close()");
				    Preferences.debug("IOException " + e + " on ifrFile.close()\n",Preferences.DEBUG_ALGORITHM);
				}  
    	  }
    	  vl_file_meta_close (ifr) ;
	   } // for (fileNum = 0; fileNum < fileName.length; fileNum++)
	   System.out.println("Run completed");
	   Preferences.debug("Run completed\n", Preferences.DEBUG_ALGORITHM);
	   setCompleted(true);
	}
    
    /** @brief File meta information
     **/
    class VlFileMeta
    {
      boolean active ;          /**< Is the file active? */
      String pattern;  /**< File name pattern */
      int     protocol[] ;        /**< File protocol */

      String name;     /**< Current file name */
      File  file;            /**< Current file stream */
      
      public VlFileMeta(boolean active, String pattern, int protocol[], String name, File file) {
          this.active = active;
          this.pattern = pattern;
          this.protocol = protocol;
          this.name = name;
          this.file = file;
      }
    } ;
    
    /* ----------------------------------------------------------------- */
    /** @brief Open the file associated to meta information
     **
     ** @param self        File meta information.
     ** @param basename  Basename.
     ** @param mode      Opening mode (as in @c fopen).
     **
     ** @return error code. The error may be either either
     ** ::VL_ERR_OVERFLOW if the file name is too long or to ::VL_ERR_IO
     ** if the file cannot be opened.
     **/

    private RandomAccessFile
    vl_file_meta_open (VlFileMeta self, String basename, String mode, int error[])
    {
      int q ;
      RandomAccessFile outFile = null;

      if (! self.active) {
        error[0] = VL_ERR_OK ;
        return null;
      }

      int num[] = new int[1];
      self.name = vl_string_replace_wildcard (self.name,
                                      self.name.length(),
                                      self.pattern,
                                      "%", "\\",
                                      basename, num) ;

      if (num[0] > self.name.length()) {
    	System.err.println("self.name = " + self.name);
    	System.err.println("num[0] = " + num[0]);
    	System.err.println("self.name.length() = " + self.name.length());
    	System.err.println("Overflow occurred in vl_string_replace_wildcard");
    	Preferences.debug("Overflow occurred in vl_string_replace_wildcard\n", Preferences.DEBUG_ALGORITHM);
    	error[0] = VL_ERR_OVERFLOW;
        return null;
      }

      if (self.active) {
    	  try {
		      outFile = new RandomAccessFile(fileDir[fileNum] + File.separator + self.name, mode);
		  } catch (IOException e) {
				System.err.println("IOException " + e + " on outFile = new RandomAccessFile(self.name, mode)");
			    Preferences.debug("IOException " + e + " on outFile = new RandomAccessFile(self.name, mode)\n",Preferences.DEBUG_ALGORITHM);
			    error[0] = VL_ERR_IO;
			    return null;
		  }
        
      }
      if (mode.equals("rw")) {
    	  try {
    		  outFile.setLength(0);
    	  }
    	  catch (IOException e) {
    		  System.err.println("IOException " + e + " on outFile.setLength(0)");
			    Preferences.debug("IOException " + e + " on outFile.setLength(0)\n",Preferences.DEBUG_ALGORITHM);
			    error[0] = VL_ERR_IO;
			    return null;  
    	  }
      }
      error[0] = 0;
      return outFile;
    }

    
    /* ----------------------------------------------------------------- */
    /** @brief Close the file associated to meta information
     **
     ** @param self File meta information.
     **/
    private void
    vl_file_meta_close (VlFileMeta self)
    {
      if (self.file != null) {
        //fclose (self -> file) ;
        //self -> file = 0 ;
      }
    }

    /* ----------------------------------------------------------------- */
    /** @brief Parse argument for file meta information
     **
     ** @param optarg  argument to parse.
     ** @param self      structure to initalize.
     **
     ** The function parses the string @a optarg to fill the structure @a
     ** self. @a optarg is supposed to be composed of two parts: a file
     ** protocol specification and a file pattern. Then the function:
     **
     ** - Sets VlFileMeta::active to true.
     ** - Sets VlFileMeta::protocol to the file protocol id (if any).
     ** - Sets VlFileMeta::pattern  to the file pattern (if any).
     **
     ** @return error code. The funciton may fail either because the file
     ** protocol is not recognized (::VL_ERR_BAD_ARG) or because the file
     ** pattern is too long to be stored (::VL_ERR_OVERFLOW).
     **/
    private int
    vl_file_meta_parse (VlFileMeta self, String optarg)
    {
      self.active = true ;

      if ((optarg != null) && (optarg.length() != 0)) {
        int protocol[] = new int[1] ;
        String arg = vl_string_parse_protocol (optarg, protocol) ;

        /* parse the (optional) protocol part */
        switch (protocol[0]) {
        case VL_PROT_UNKNOWN :
          return VL_ERR_BAD_ARG  ;

        case VL_PROT_ASCII  :
        case VL_PROT_BINARY :
          self.protocol = protocol ;
          break ;

        case VL_PROT_NONE :
          break ;
        }

        if (arg.length() > 0) {
          self.pattern = new String(arg);
        }

      }
      return VL_ERR_OK ;
    }
    
    /** ------------------------------------------------------------------
     ** @brief Extract the protocol prefix from a string
     ** @param string string.
     ** @param protocol protocol code (output).
     ** @return pointer to the first character after the protocol prefix.
     **
     ** The function extracts the prefix of the string @a string
     ** terminated by the first occurrence of the @c :// substring (if
     ** any). It then matches the suffix terminated by @c :// to the
     ** supported @ref vl-stringop-file-protocols protocols. If @c protocol is not
     ** @c NULL, the corresponding protocol code is written to @a protocol
     **
     ** The function writes to @a protocol the value ::VL_PROT_NONE if no
     ** suffix is detected and ::VL_PROT_UNKNOWN if there is a suffix but
     ** it cannot be matched to any of the supported protocols.
     **/

    
   private String vl_string_parse_protocol (String string, int protocol[])
    {
	  String firstPart;
      String cpt ;
      int dummy[] = new int[1] ;

      /* handle the case prot = 0 */
      if (protocol[0] == 0)
        protocol = dummy ;

      /* look for :// */
      int index = string.indexOf("://");

      if (index == -1) {
        protocol[0] = VL_PROT_NONE ;
        cpt = string ;
      }
      else {
    	firstPart = string.substring(0,index);
        if (firstPart.equals("ascii")) {
          protocol[0] = VL_PROT_ASCII ;
        }
        else if (firstPart.equals("bin")) {
          protocol[0] = VL_PROT_BINARY ;
        }
        else {
          protocol[0] = VL_PROT_UNKNOWN ;
        }
        cpt = string.substring(index+3) ;
      }
      return cpt ;
    }
   
   /** ------------------------------------------------------------------
    ** @brief Get protocol name
    ** @param protocol protocol code.
    ** @return pointer protocol name string.
    **
    ** The function returns a pointer to a string containing the name of
    ** the protocol @a protocol (see the @a vl-file-protocols protocols
    ** list).  If the protocol is unknown the function returns the empty
    ** string.
    **/

   private String vl_string_protocol_name (int protocol)
   {
     switch (protocol) {
     case VL_PROT_ASCII:
       return "ascii" ;
     case VL_PROT_BINARY:
       return "bin" ;
     case VL_PROT_NONE :
       return "" ;
     default:
       return null ;
     }
   }
   
    private void PRNFO(String name, VlFileMeta fm) {         
	   System.out.println("sift: " + name) ; 
       Preferences.debug("sift: " + name + "\n", Preferences.DEBUG_ALGORITHM);
       if (fm.active) {
    	   System.out.println("active yes");
    	   Preferences.debug("active yes\n", Preferences.DEBUG_ALGORITHM);
       }
       else {
    	   System.out.println("active no");
    	   Preferences.debug("active no\n", Preferences.DEBUG_ALGORITHM);
       }
	   System.out.println("protocol name " + vl_string_protocol_name(fm.protocol[0]));
	   Preferences.debug("protocol name " + vl_string_protocol_name(fm.protocol[0]) + "\n", Preferences.DEBUG_ALGORITHM);
	   System.out.println("pattern " +  fm.pattern);
	   Preferences.debug("pattern " + fm.pattern + "\n", Preferences.DEBUG_ALGORITHM);
   }
    
    /** ------------------------------------------------------------------
     ** @brief Extract base of file name
     ** @param destination destination buffer.
     ** @param destinationSize size of destination buffer.
     ** @param source input string.
     ** @param maxNumStrippedExtensions maximum number of extensions to strip.
     ** @return length of the destination string.
     **
     ** The function removes the leading path and up to @c
     ** maxNumStrippedExtensions trailing extensions from the string @a
     ** source and writes the result to the buffer @a destination.
     **
     ** The leading path is the longest suffix that ends with either the
     ** @c \ or @c / characters. An extension is a string starting with
     ** the <code>.</code> character not containing it. For instance, the string @c
     ** file.png contains the extension <code>.png</code> and the string @c
     ** file.tar.gz contains two extensions (<code>.tar</code> and @c <code>.gz</code>).
     **
     ** @sa @ref vl-stringop-err.
     **/

    private String vl_string_basename (String destination,
                        int destinationSize,
                        String source,
                        int maxNumStrippedExtensions, int num[])
    {
      String c ;
      int k = 0, beg, end ;

      /* find beginning */
      beg = 0 ;
      for (k = 0 ; k < source.length() ; ++ k) {
    	c = source.substring(k,k+1);
        if ((c.equals("\\")) || (c.equals("/"))) beg = k + 1 ;
      }

      /* find ending */
      end = source.length();
      for (k = end ; k > beg ; --k) {
        if ((source.substring(k - 1,k).equals(".")) && (maxNumStrippedExtensions > 0)) {
          -- maxNumStrippedExtensions ;
          end = k - 1 ;
        }
      }

      
      return vl_string_copy_sub (destination, destinationSize, source,
                                 beg, end,num) ;
    }
    
    /** ------------------------------------------------------------------
     ** @brief Copy substring
     ** @param destination output buffer.
     ** @param destinationSize  size of output buffer.
     ** @param beginning start of the substring.
     ** @param end end of the substring.
     ** @return length of the destination string.
     **
     ** The function copies the substring from at @a beginning to @a end
     ** (not included) to the buffer @a destination of size @a
     ** destinationSize. If, however, the null character is found before
     ** @a end, the substring terminates there.
     **
     ** @sa @ref vl-stringop-err.
     **/

    private String vl_string_copy_sub (String destination,
                        int destinationSize, String source,
                        int beginning,
                        int end, int num[])
    {
      String c ;
      int k = 0 ;

      while (beginning < end && ((c = source.substring(beginning,beginning+1))!= null)) {
    	beginning++;
        if (k + 1 < destinationSize) {
          destination = destination.concat(c);
        }
        ++ k ;
      }

      /* finalize */
      //if (destinationSize > 0) {
      //  destination[VL_MIN(k, destinationSize - 1)] = 0 ;
      //}
      num[0] = k;
      return destination;
    }

    /** @brief PGM image meta data
     **
     ** A PGM image is a 2-D array of pixels of width #width and height
     ** #height. Each pixel is an integer one or two bytes wide, depending
     ** whether #max_value is smaller than 256.
     **/
    class VlPgmImage
    {
      int width ;      /**< image width.                     */
      int height ;     /**< image height.                    */
      int max_value ;  /**< pixel maximum value (<= 2^16-1). */
      // Used as boolean
      int is_raw ;     /**< is RAW format?                   */
      
      public VlPgmImage() {
    	  
      }
    };
    
        class VlSiftKeypoint
    {
      int o ;           /**< o coordinate (octave). */

      int ix ;          /**< Integer unnormalized x coordinate. */
      int iy ;          /**< Integer unnormalized y coordinate. */
      int is ;          /**< Integer s coordinate. */

      float x ;     /**< x coordinate. */
      float y ;     /**< y coordinate. */
      float s ;     /**< s coordinate. */
      float sigma ; /**< scale. */
      
      public VlSiftKeypoint() {
    	  
      }
    };
    
    /** ------------------------------------------------------------------
     ** @brief SIFT filter
     **
     ** This filter implements the SIFT detector and descriptor.
     **/

     class VlSiftFilt
    {
      double sigman ;       /**< nominal image smoothing. */
      double sigma0 ;       /**< smoothing of pyramid base. */
      double sigmak ;       /**< k-smoothing */
      double dsigma0 ;      /**< delta-smoothing. */

      int width ;           /**< image width. */
      int height ;          /**< image height. */
      int O ;               /**< number of octaves. */
      int S ;               /**< number of levels per octave. */
      int o_min ;           /**< minimum octave index. */
      int s_min ;           /**< minimum level index. */
      int s_max ;           /**< maximum level index. */
      int o_cur ;           /**< current octave. */

      float temp[] ;   /**< temporary pixel buffer. */
      float octave[] ; /**< current GSS data. */
      float dog[] ;    /**< current DoG data. */
      int octave_width ;    /**< current octave width. */
      int octave_height ;   /**< current octave height. */

      float gaussFilter[] ;  /**< current Gaussian filter */
      double gaussFilterSigma ;   /**< current Gaussian filter std */
      int gaussFilterWidth ;  /**< current Gaussian filter width */

      VlSiftKeypoint keys[] ;/**< detected keypoints. */
      int nkeys ;           /**< number of detected keypoints. */
      int keys_res ;        /**< size of the keys buffer. */

      double peak_thresh ;  /**< peak threshold. */
      double edge_thresh ;  /**< edge threshold. */
      double norm_thresh ;  /**< norm threshold. */
      double magnif ;       /**< magnification factor. */
      double windowSize ;   /**< size of Gaussian window (in spatial bins) */

      float grad[];   /**< GSS gradient data. */
      int grad_o ;          /**< GSS gradient data octave. */
      
      public VlSiftFilt() {
    	  
      }

    };
    
    /** ------------------------------------------------------------------
     ** @brief Extract PGM header from stream.
     ** @param f  input file.
     ** @param im image structure to fill.
     ** @return error code.
     **
     ** The function extracts from the file @a f the meta-data section of
     ** an image encoded in PGM format. The function fills the structure
     ** ::VlPgmImage accordingly.
     **
     ** The error may be either ::VL_ERR_PGM_INV_HEAD or ::VL_ERR_PGM_INV_META
     ** depending whether the error occurred in decoding the header or
     ** meta section of the PGM file.
     **/

    private int
    vl_pgm_extract_head (RandomAccessFile f, VlPgmImage im)
    {
      byte magic[] = new byte[2] ;
      int c ;
      int is_raw = 0;
      int width ;
      int height ;
      int max_value ;
      int sz = -1;
      boolean good ;

      /* -----------------------------------------------------------------
       *                                                check magic number
       * -------------------------------------------------------------- */
      try {
          sz = f.read(magic);
      }
      catch (IOException e) {
		    System.err.println("IOException " + e + " on sz = f.read(magic)");
			Preferences.debug("IOException " + e + " on sz = f.read(magic)\n",Preferences.DEBUG_FILEIO);
			System.exit(-1);
      }

      if (sz < 2) {
    	System.err.println("Invalid PGM header");
    	Preferences.debug("Invalid PGM header\n", Preferences.DEBUG_FILEIO);
        return VL_ERR_PGM_INV_HEAD;
      }

      good = magic [0] == 'P' ;

      switch (magic [1]) {
      case '2' : /* ASCII format */
        is_raw = 0 ;
        break ;

      case '5' : /* RAW format */
        is_raw = 1 ;
        break ;

      default :
        good = false ;
        break ;
      }

      if( ! good ) {
    	  System.err.println("Invalid PGM header");
      	  Preferences.debug("Invalid PGM header\n", Preferences.DEBUG_FILEIO);
          return VL_ERR_PGM_INV_HEAD;  
      }

      /* -----------------------------------------------------------------
       *                                    parse width, height, max_value
       * -------------------------------------------------------------- */
      good = true;

      int err[] = new int[1];
      c = remove_blanks(f,err) ;
      good &= ((c > 0) && (err[0] == VL_ERR_OK));

      width = readAsciiInt(f);

      c = remove_blanks(f,err) ;
      good &= ((c > 0) && (err[0] == VL_ERR_OK)) ;

      height = readAsciiInt(f);

      c = remove_blanks(f,err) ;
      good &= ((c > 0) && (err[0] == VL_ERR_OK)) ;

      max_value = readAsciiInt(f);

      /* must end with a single blank */
      try {
    	  c = f.readByte();
      }
      catch (IOException e) {
  		System.err.println("IOException " + e + " on c = f.readByte()");
		Preferences.debug("IOException " + e + " on c = f.readByte()\n",Preferences.DEBUG_FILEIO);
		System.exit(-1);	
  	  }
      good &=
        c == '\n' ||
        c == '\t' ||
        c == ' '  ||
        c == '\r' ;

      if(! good) {
    	  System.err.println("Invalid PGM meta information");
      	  Preferences.debug("Invalid PGM meta information\n", Preferences.DEBUG_FILEIO);
          return VL_ERR_PGM_INV_META;
      }

      if(max_value >= 65536) {
    	  System.err.println("Invalid PGM meta information");
      	  Preferences.debug("Invalid PGM meta information\n", Preferences.DEBUG_FILEIO);
          return VL_ERR_PGM_INV_META;
      }

      /* exit */
      im.width     = width ;
      im.height    = height ;
      im.max_value = max_value ;
      im.is_raw    = is_raw ;
      return 0 ;
    }
    
    /** ------------------------------------------------------------------
     ** @internal @brief Remove all characters to the next new-line.
     ** @param f file to strip.
     ** @return number of characters removed.
     **/

    private int
    remove_line(RandomAccessFile f)
    {
      int count = 0 ;
      int c = 0 ;

      while (true) {
    	try {
    		c = f.readByte();
    	}
        catch (EOFException e) {
    	    return count;	
    	}
    	catch (IOException e) {
    		System.err.println("IOException " + e + " on c = f.readByte()");
 			Preferences.debug("IOException " + e + " on c = f.readByte()\n",Preferences.DEBUG_FILEIO);
 			System.exit(-1);	
    	}
    	
        ++ count ;

        switch(c) {
        case '\n' :
           return count;

        
        }
      }
    }

    /** ------------------------------------------------------------------
     ** @internal @brief Remove white-spaces and comments.
     ** @param f file to strip.
     ** @return number of characters removed.
     **/

    private int
    remove_blanks(RandomAccessFile f, int err[])
    {
      int count = 0 ;
      int c = 0;
      long position = 0L;
      err[0] = 0;

      while (true) {
    	try {
      		c = f.readByte();
      	}
    	catch (EOFException e) {
    		err[0] = VL_ERR_EOF;
    		return count;
    	}
      	catch (IOException e) {
      		System.err.println("IOException " + e + " on c = f.readByte()");
   			Preferences.debug("IOException " + e + " on c = f.readByte()\n",Preferences.DEBUG_FILEIO);
   			err[0] = VL_ERR_IO;
   			return count;
      	}

        switch(c) {

        case '\t' : case '\n' :
        case '\r' : case ' '  :
          ++ count ;
          break ;

        case '#' :
          count += 1 + remove_line(f) ;
          break ;

        default:
          try {
              position = f.getFilePointer();
          }
          catch (IOException e) {
        		System.err.println("IOException " + e + " on position = f.getFilePointer()");
     			Preferences.debug("IOException " + e + " on position = f.getFilePointer()\n",Preferences.DEBUG_FILEIO);
     			err[0] = VL_ERR_IO;
     			return count;
          }
          try {
              f.seek(position-1);
          }
          catch (IOException e) {
      		System.err.println("IOException " + e + " on f.seek(position-1)");
   			Preferences.debug("IOException " + e + " on position = f.seek(poistion-1)\n",Preferences.DEBUG_FILEIO);
   			err[0] = VL_ERR_IO;
   			return count;
          }
          return count ;
        }
      }
    }
    
    private int readAsciiInt(RandomAccessFile f) {
    	int value = 0; 
    	int c = 0;
    	long startPosition = 0L;
    	long endPosition = 0L;
    	try {
    	    startPosition = f.getFilePointer();
    	}
    	catch (IOException e) {
    		System.err.println("IOException " + e + " on startPosition = f.getFilePointer()");
 			Preferences.debug("IOException " + e + " on startPosition = f.getFilePointer()\n",Preferences.DEBUG_FILEIO);
 			System.exit(-1);	
        }
    	while (true) {
    		try {
        	    endPosition = f.getFilePointer();
        	}
        	catch (IOException e) {
        		System.err.println("IOException " + e + " on endPosition = f.getFilePointer()");
     			Preferences.debug("IOException " + e + " on endPosition = f.getFilePointer()\n",Preferences.DEBUG_FILEIO);
     			System.exit(-1);	
            }
        	try {
          		c = f.readByte();
          	}
        	catch (EOFException e) {
        	    endPosition--;
        	    break;
        	}
          	catch (IOException e) {
          		System.err.println("IOException " + e + " on c = f.readByte()");
       			Preferences.debug("IOException " + e + " on c = f.readByte()\n",Preferences.DEBUG_FILEIO);
       			System.exit(-1);	
          	}
        	if (endPosition == startPosition) {
	        	if (!((c == 43) || (c == 45) || ((c >= 48) && (c <= 57)))) {
	        	    endPosition--;
	        	    break;
	        	}
        	}
        	else {
        		if (!((c >= 48) && (c <= 57))) {
        			endPosition--;
        			break;
        		}
        	}
    	}
    	try {
    		f.seek(startPosition);
    	}
    	catch (IOException e) {
    		System.err.println("IOException " + e + " on f.seek(startPosition)");
 			Preferences.debug("IOException " + e + " on f.seek(startPosition)\n",Preferences.DEBUG_FILEIO);
 			System.exit(-1);	
        }
    	byte buf[] = new byte[(int)(endPosition - startPosition + 1)];
    	try {
    		f.read(buf);
    	}
    	catch (IOException e) {
    		System.err.println("IOException " + e + " on f.read(buf)");
 			Preferences.debug("IOException " + e + " on f.read(buf)\n",Preferences.DEBUG_FILEIO);
 			System.exit(-1);	
        }
    	String num = new String(buf);
    	try {
    		value = Integer.parseInt(num);
    	}
    	catch (NumberFormatException e) {
    		System.err.println("NumberFormatException " + e + " on Integer.parseInt(num)");
 			Preferences.debug("NumberFormatException " + e + " on Integer.parseInt(num)\n",Preferences.DEBUG_FILEIO);
 			System.exit(-1);		
    	}
    	return value;
    }
    
    private double readAsciiDouble(RandomAccessFile f, int err[]) {
    	double value = 0.0; 
    	int c = 0;
    	long startPosition = 0L;
    	long endPosition = 0L;
    	err[0] = 0;
    	try {
    	    startPosition = f.getFilePointer();
    	}
    	catch (IOException e) {
    		System.err.println("IOException " + e + " on startPosition = f.getFilePointer()");
 			Preferences.debug("IOException " + e + " on startPosition = f.getFilePointer()\n",Preferences.DEBUG_FILEIO);
 			err[0] = VL_ERR_IO;
 			return Double.NaN;
        }
    	while (true) {
    		try {
        	    endPosition = f.getFilePointer();
        	}
        	catch (IOException e) {
        		System.err.println("IOException " + e + " on endPosition = f.getFilePointer()");
     			Preferences.debug("IOException " + e + " on endPosition = f.getFilePointer()\n",Preferences.DEBUG_FILEIO);
     			err[0] = VL_ERR_IO;
     			return Double.NaN;
            }
        	try {
          		c = f.readByte();
          	}
        	catch (EOFException e) {
        		if (endPosition == startPosition) {
        			err[0] = VL_ERR_EOF;
        			return  Double.NaN;
        		}
        	    endPosition--;
        	    break;
        	}
          	catch (IOException e) {
          		System.err.println("IOException " + e + " on c = f.readByte()");
       			Preferences.debug("IOException " + e + " on c = f.readByte()\n",Preferences.DEBUG_FILEIO);
       			err[0] = VL_ERR_IO;
       			return Double.NaN;
          	}
        	if (!((c == 43) || (c == 45) || (c == 46) || ((c >= 48) && (c <= 57)) || (c == 69) || (c == 101))) {
        	    endPosition--;
        	    break;
        	}
        	
    	}
    	try {
    		f.seek(startPosition);
    	}
    	catch (IOException e) {
    		System.err.println("IOException " + e + " on f.seek(startPosition)");
 			Preferences.debug("IOException " + e + " on f.seek(startPosition)\n",Preferences.DEBUG_FILEIO);
 			err[0] = VL_ERR_IO;
 			return Double.NaN;
        }
    	byte buf[] = new byte[(int)(endPosition - startPosition + 1)];
    	try {
    		f.read(buf);
    	}
    	catch (IOException e) {
    		System.err.println("IOException " + e + " on f.read(buf)");
 			Preferences.debug("IOException " + e + " on f.read(buf)\n",Preferences.DEBUG_FILEIO);
 			err[0] = VL_ERR_IO;
 			return Double.NaN;
        }
    	String num = new String(buf);
    	try {
    		value = Double.parseDouble(num);
    	}
    	catch (NumberFormatException e) {
    		System.err.println("NumberFormatException " + e + " on Double.parseDouble(num)");
 			Preferences.debug("NumberFormatException " + e + " on Double.parseDouble(num)\n",Preferences.DEBUG_FILEIO);
 		    err[0] = VL_ERR_BAD_ARG;
 		    return Double.NaN;
    	}
    	return value;
    }
    
    private int
    vl_pgm_get_npixels (VlPgmImage im)
    {
      return im.width * im.height ;
    }
    
    private int
    vl_pgm_get_bpp (VlPgmImage im)
    {
    	if (im.max_value >= 256) {
    		return 2;
    	}
    	else {
    		return 1;
    	}
    }
    
    int
    vl_pgm_extract_data (RandomAccessFile f, VlPgmImage im, int data[])
    {
      int bpp = vl_pgm_get_bpp(im) ;
      int data_size = vl_pgm_get_npixels(im) ;
      boolean good = true;
      int c ;
      int i;

      /* -----------------------------------------------------------------
       *                                                         read data
       * -------------------------------------------------------------- */

      /*
         In RAW mode we read directly an array of bytes or shorts.  In
         the latter case, however, we must take care of the
         endianess. PGM files are sorted in big-endian format. If our
         architecture is little endian, we must do a conversion.
      */
      if (im.is_raw == 1) {
        if (bpp == 1) {
        	for (i = 0; (i < data_size) && good; i++) {
        		try {
        		    data[i] = f.readUnsignedByte();
        		}
        		catch (IOException e) {
        			good = false;
        		}
        	}
        }
        else if (bpp == 2) {
            for (i = 0; (i < data_size) && good; i++) {
            	try {
            	    data[i] = f.readUnsignedShort();
            	}
            	catch (IOException e) {
            		good = false;
            	}
            }
        }
      }
      else {
    	   int err[] = new int[1];
    	   for (i = 0; i < data_size; i++) {
    		   c = remove_blanks(f,err) ;
    		   if (err[0] != VL_ERR_OK) {
    			   break;
    		   }
    		   good &= c > 0 ;
    		   if (!good) {
    			   break;
    		   }

    		   data[i] = readAsciiInt(f);   
    	   }
      }

      if(! good) {
    	  System.err.println("Invalid PGM data");
    	  Preferences.debug("Invalid PGM data\n", Preferences.DEBUG_FILEIO);
    	  return VL_ERR_PGM_INV_DATA;
      }
     
      return 0 ;
    }
    
    /** ------------------------------------------------------------------
     ** @brief Replace wildcard characters by a string
     ** @param destination output buffer.
     ** @param destinationSize size of the output buffer.
     ** @param source input string.
     ** @param wildcardChar wildcard character.
     ** @param escapeChar escape character.
     ** @param replacement replacement string.
     **
     ** The function replaces the occurrence of the specified wildcard
     ** character @a wildcardChar by the string @a replacement. The result
     ** is written to the buffer @a destination of size @a
     ** destinationSize.
     **
     ** Wildcard characters may be escaped by preceding them by the @a esc
     ** character. More in general, anything following an occurrence of @a
     ** esc character is copied verbatim. To disable the escape characters
     ** simply set @a esc to 0.
     **
     ** @return length of the result.
     ** @sa @ref vl-stringop-err.
     **/

    private String
    vl_string_replace_wildcard (String destination,
                                int destinationSize,
                                String source,
                                String wildcardChar,
                                String escapeChar,
                                String replacement, int num[])
    {
      String c ;
      int k = 0 ;
      boolean escape = false;
      int sIndex = 0;
      int rIndex = 0;

      while (sIndex < source.length()) {
    	c = source.substring(sIndex,sIndex+1);
        sIndex++;
        /* enter escape mode ? */
        if (! escape && c.equals(escapeChar)) {
          escape = true ;
          continue ;
        }

        /* wildcard or regular? */
        rIndex = 0;
        if (! escape && c.equals(wildcardChar)) {
          String repl = new String(replacement) ;
          while (rIndex < repl.length()) {
        	c = repl.substring(rIndex,rIndex+1);
        	rIndex++;
            if (destination != null) {
              if (k == 0) {
            	  if (destination.length() >= 2) {
            	      destination = c + destination.substring(1);
            	  }
            	  else {
            		  destination = c;
            	  }
              }
              else if (k == destination.length() -1) {
            	  destination = destination.substring(0,destination.length()-1) + c;
              }
              else {
            	  if (destination.length() >= (k+2)) {
            	      destination = destination.substring(0,k) + c + destination.substring(k+1);
            	  }
            	  else {
            		  destination = destination.substring(0,k) + c;
            	  }
              }
            }
            ++ k ;
          }
        }
        /* regular character */
        else {
          if (destination != null) {
        	  if (k == 0) {
        		  if (destination.length() >= 2) {
            	      destination = c + destination.substring(1);
            	  }
            	  else {
            		  destination = c;
            	  }
              }
              else if (k == destination.length() -1) {
            	  destination = destination.substring(0,destination.length()-1) + c;
              }
              else {
            	  if (destination.length() >= (k+2)) {
            	      destination = destination.substring(0,k) + c + destination.substring(k+1);
            	  }
            	  else {
            		  destination = destination.substring(0,k) + c;
            	  }
              }
          }
          ++ k ;
        }
        escape = false;
      }

      /* add trailing 0 */
      //if (destinationSize > 0) {
        //destination[VL_MIN(k, destinationSize - 1)] = 0 ;
      //}
      num[0] = k ;
      return destination;
    }
    
    /* ----------------------------------------------------------------- */
    /** @brief Read double from file
     **
     ** @param self  File meta information.
     ** @param x   Datum read.
     **
     ** @return error code. The function returns ::VL_ERR_EOF if the
     ** end-of-file is reached and ::VL_ERR_BAD_ARG if the file is
     ** malformed.
     **/

    private int
    vl_file_meta_get_double (int protocol, RandomAccessFile file, double x[])
    {
      int c = 0;

      switch (protocol) {

      case VL_PROT_ASCII :
    	  int err[] = new int[1];
    	  c = remove_blanks(file, err) ;
		  if (err[0] != VL_ERR_OK) {
			  return err[0];
		  }
		   
          x[0] = readAsciiDouble(file, err);
          return err[0];

      case VL_PROT_BINARY :
    	  try {
    		  x[0] = file.readDouble();
    	  }
    	  catch (IOException e) {
    		  System.err.println("IOException on x[0] = file.readDouble()");
    		  Preferences.debug("IOException on x[0] = file.readDouble()\n", Preferences.DEBUG_FILEIO);
    		  return VL_ERR_BAD_ARG;
    	  }
        
        break ;

      default :
    	  System.err.println("Unknown protocol in file_meta_get_double");
		  Preferences.debug("Unknown protocol in file_meta_get_double\n", Preferences.DEBUG_FILEIO);
		  return VL_ERR_BAD_ARG;
      }

      return VL_ERR_OK ;
    }
    
    private class ikeysComparator implements Comparator<double[]> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(double o1[], double o2[]) {
            double a = o1[2];
            double b = o2[2];

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else {
                return 0;
            }
        }

    }
    
    private double log2(double x) {
    	return (Math.log(x)/Math.log(2));
    }
    
    /** @brief Signed left shift operation
     ** @param x value.
     ** @param n number of shift positions.
     ** @return @c x << n .
     ** The macro is equivalent to the builtin @c << operator, but it
     ** supports negative shifts too.
     **/
    private int VL_SHIFT_LEFT(int x,int n) {
    	int result;
    	if (n >= 0) {
    		result = x << n;
    	}
    	else {
    		result = x >> (-n);
    	}
    	return result;
    }
    
    private void
    fast_expn_init ()
    {
      int k  ;
      for(k = 0 ; k < EXPN_SZ + 1 ; ++ k) {
        expn_tab [k] = Math.exp (- (double) k * (EXPN_MAX / EXPN_SZ)) ;
      }
    }
    
    VlSiftFilt
    vl_sift_new (int width, int height,
                 int noctaves, int nlevels,
                 int o_min)
    {
      VlSiftFilt f = new VlSiftFilt();

      int w   = VL_SHIFT_LEFT (width,  -o_min) ;
      int h   = VL_SHIFT_LEFT (height, -o_min) ;
      int nel = w * h ;

      /* negative value O => calculate max. value */
      if (noctaves < 0) {
        noctaves = Math.max ((int)Math.floor (log2 (Math.min(width, height))) - o_min - 3, 1) ;
      }

      f.width   = width ;
      f.height  = height ;
      f.O       = noctaves ;
      f.S       = nlevels ;
      f.o_min   = o_min ;
      f.s_min   = -1 ;
      f.s_max   = nlevels + 1 ;
      f.o_cur   = o_min ;

      f.temp    = new float[nel] ;
      f.octave  = new float[nel
                            * (f.s_max - f.s_min + 1)  ] ;
      f.dog     = new float[nel
                            * (f.s_max - f.s_min    )  ] ;
      f.grad    = new float[nel * 2
                            * (f.s_max - f.s_min    )  ] ;

      f.sigman  = 0.5 ;
      f.sigmak  = Math.pow (2.0, 1.0 / nlevels) ;
      f.sigma0  = 1.6 * f.sigmak ;
      f.dsigma0 = f.sigma0 * Math.sqrt (1.0 - 1.0 / (f.sigmak*f.sigmak)) ;

      f.gaussFilter = null ;
      f.gaussFilterSigma = 0 ;
      f.gaussFilterWidth = 0 ;

      f.octave_width  = 0 ;
      f.octave_height = 0 ;

      f.keys     = null;
      f.nkeys    = 0 ;
      f.keys_res = 0 ;

      f.peak_thresh = 0.0 ;
      f.edge_thresh = 10.0 ;
      f.norm_thresh = 0.0 ;
      f.magnif      = 3.0 ;
      f.windowSize  = NBP / 2 ;

      f.grad_o  = o_min - 1 ;

      /* initialize fast_expn stuff */
      fast_expn_init () ;

      return f ;
    }
    
    /** ------------------------------------------------------------------
     ** @brief Start processing a new image
     **
     ** @param f  SIFT filter.
     ** @param im image data.
     **
     ** The function starts processing a new image by computing its
     ** Gaussian scale space at the lower octave. It also empties the
     ** internal keypoint buffer.
     **
     ** @return error code. The function returns ::VL_ERR_EOF if there are
     ** no more octaves to process.
     **
     ** @sa ::vl_sift_process_next_octave().
     **/
    
    private void
    copy_and_upsample_rows
    (float dst[], int dstIndex,
     float src[], int srcIndex, int width, int height)
    {
      int x, y ;
      float a, b ;

      for(y = 0 ; y < height ; ++y) {
        b = a = src[srcIndex++] ;
        for(x = 0 ; x < width - 1 ; ++x) {
          b = src[srcIndex++] ;
          dst[dstIndex] = a ;             dstIndex += height ;
          dst[dstIndex] = (float)(0.5 * (a + b)) ; dstIndex += height ;
          a = b ;
        }
        dst[dstIndex] = b ; dstIndex += height ;
        dst[dstIndex] = b ; dstIndex += height ;
        dstIndex += 1 - width * 2 * height ;
      }
    }
    
    /** ------------------------------------------------------------------
     ** @internal
     ** @brief Copy and downsample an image
     **
     ** @param dst    output imgae buffer.
     ** @param src    input  image buffer.
     ** @param width  input  image width.
     ** @param height input  image height.
     ** @param d      octaves (non negative).
     **
     ** The function downsamples the image @a d times, reducing it to @c
     ** 1/2^d of its original size. The parameters @a width and @a height
     ** are the size of the input image. The destination image @a dst is
     ** assumed to be <code>floor(width/2^d)</code> pixels wide and
     ** <code>floor(height/2^d)</code> pixels high.
     **/

    private void
    copy_and_downsample
    (float       dst[], int dstIndex,
     float src[], int srcIndex,
     int width, int height, int d)
    {
      int x, y ;

      d = 1 << d ; /* d = 2^d */
      for(y = 0 ; y < height ; y+=d) {
        int srcrowp = y * width ;
        for(x = 0 ; x < width - (d-1) ; x+=d) {
          dst[dstIndex++] = src[srcIndex + srcrowp] ;
          srcrowp += d ;
        }
      }
    }
    
    /** @fn vl_imconvcol_vd(double*,vl_size,double const*,vl_size,vl_size,vl_size,double const*,vl_index,vl_index,int,unsigned int)
     ** @brief Convolve image along columns
     **
     ** @param dst destination image.
     ** @param dst_stride width of the destination image including padding.
     ** @param src source image.
     ** @param src_width width of the source image.
     ** @param src_height height of the source image.
     ** @param src_stride width of the source image including padding.
     ** @param filt filter kernel.
     ** @param filt_begin coordinate of the first filter element.
     ** @param filt_end coordinate of the last filter element.
     ** @param step sub-sampling step.
     ** @param flags operation modes.
     **
     ** The function convolves the column of the image @a src by the
     ** filter @a filt and saves the result to the image @a dst. The size
     ** of @a dst must be equal to the size of @a src.  Formally, this
     ** results in the calculation
     **
     ** @f[
     ** \mathrm{dst} [x,y] = \sum_{p=y-\mathrm{filt\_end}}^{y-\mathrm{filt\_begin}}
     ** \mathrm{src}[x,y] \mathrm{filt}[y - p - \mathrm{filt\_begin}]
     ** @f]
     **
     ** The function subsamples the image along the columns according to
     ** the parameter @a step. Setting @a step to 1 (one) computes the
     ** elements @f$\mathrm{dst}[x,y]@f$ for all pairs (x,0), (x,1), (x,2)
     ** and so on. Setting @a step two 2 (two) computes only (x,0), (x,2)
     ** and so on (in this case the height of the destination image is
     ** <code>floor(src_height/step)+1)</code>.
     **
     ** Calling twice the function can be used to compute 2-D separable
     ** convolutions.  Use the flag ::VL_TRANSPOSE to transpose the result
     ** (in this case @a dst has transposed dimension as well).
     **
     ** The function allows the support of the filter to be any range.
     ** Usually the support is <code>@a filt_end = -@a filt_begin</code>.
     **
     ** The convolution operation may pick up values outside the image
     ** boundary. To cope with this edge cases, the function either pads
     ** the image by zero (::VL_PAD_BY_ZERO) or with the values at the
     ** boundary (::VL_PAD_BY_CONTINUITY).
     **/

    /** @fn vl_imconvcol_vf(float*,vl_size,float const*,vl_size,vl_size,vl_size,float const*,vl_index,vl_index,int,unsigned int)
     ** @see ::vl_imconvcol_vd
     **/

    private void vl_imconvcol_vf
    (float dst[], int dst_index, int dst_stride,
     float src[], int src_index,
     int src_width, int src_height,int src_stride,
     float filt[], int filt_begin, int filt_end,
     int step, int flags)
    {
      int x = 0 ;
      int y ;
      int dheight = (src_height - 1) / step + 1 ;
      boolean transp = ((flags & VL_TRANSPOSE) != 0) ;
      boolean zeropad = (flags & VL_PAD_MASK) == VL_PAD_BY_ZERO ;

      /* let filt point to the last sample of the filter */
      int filt_index = filt_end - filt_begin ;

      while (x < src_width) {
        /* Calculate dest[x,y] = sum_p image[x,p] filt[y - p]
         * where supp(filt) = [filt_begin, filt_end] = [fb,fe].
         *
         * CHUNK_A: y - fe <= p < 0
         *          completes VL_MAX(fe - y, 0) samples
         * CHUNK_B: VL_MAX(y - fe, 0) <= p < VL_MIN(y - fb, height - 1)
         *          completes fe - VL_MAX(fb, height - y) + 1 samples
         * CHUNK_C: completes all samples
         */
        int filti ;
        int stop ;

        for (y = 0 ; y < src_height ; y += step) {
          float acc = 0 ;
          float v = 0, c ;
          int srci;

          filti = filt_index ;
          stop = filt_end - y ;
          srci = x + src_index - stop * src_stride ;

          if (stop > 0) {
            if (zeropad) {
              v = 0 ;
            } else {
              v = src[src_index + x] ;
            }
            while (filti > filt_index - stop) {
              c = filt[filti--] ;
              acc += v * c ;
              srci += src_stride ;
            }
          }

          stop = filt_end - Math.max(filt_begin, y - src_height + 1) + 1 ;
          while (filti > filt_index - stop) {
            v = src[srci] ;
            c = filt[filti--] ;
            acc += v * c ;
            srci += src_stride ;
          }

          if (zeropad) v = 0 ;

          stop = filt_end - filt_begin + 1 ;
          while (filti > filt_index - stop) {
            c = filt[filti--] ;
            acc += v * c ;
          }

          if (transp) {
            dst[dst_index] = acc ; dst_index += 1 ;
          } else {
            dst[dst_index] = acc ; dst_index += dst_stride ;
          }
        } /* next y */
        if (transp) {
          dst_index += 1 * dst_stride - dheight * 1 ;
        } else {
          dst_index += 1 * 1 - dheight * dst_stride ;
        }
        x += 1 ;
      } /* next x */
    }

    
    /** ------------------------------------------------------------------
     ** @internal
     ** @brief Smooth an image
     ** @param self        SIFT filter.
     ** @param outputImage output imgae buffer.
     ** @param tempImage   temporary image buffer.
     ** @param inputImage  input image buffer.
     ** @param width       input image width.
     ** @param height      input image height.
     ** @param sigma       smoothing.
     **/

    private void
    _vl_sift_smooth (VlSiftFilt self,
                     float outputImage[], int output_index,
                     float tempImage[],
                     float inputImage[], int input_index,
                     int width,
                     int height,
                     double sigma)
    {
      int j;
      /* prepare Gaussian filter */
      if (self.gaussFilterSigma != sigma) {
        float acc = 0 ;
        if (self.gaussFilter != null) {
        	self.gaussFilter = null;
        }
        self.gaussFilterWidth = Math.max((int)Math.ceil(4.0 * sigma), 1) ;
        self.gaussFilterSigma = sigma ;
        self.gaussFilter = new float[(int)(2 * self.gaussFilterWidth + 1)]; ;

        for (j = 0 ; j < 2 * self.gaussFilterWidth + 1 ; ++j) {
          float d = (float)(j - self.gaussFilterWidth) / ((float)sigma) ;
          self.gaussFilter[j] = (float) Math.exp (- 0.5 * (d*d)) ;
          acc += self.gaussFilter[j] ;
        }
        for (j = 0 ; j < 2 * self.gaussFilterWidth + 1 ; ++j) {
          self.gaussFilter[j] /= acc ;
        }
      }

      if (self.gaussFilterWidth == 0) {
    	int length = width * height;
    	for (j = 0; j < length; j++) {
    		outputImage[output_index + j] = inputImage[input_index + j];
    	}
        return ;
      }

      int temp_index = 0;
      
      vl_imconvcol_vf (tempImage, temp_index, height,
                       inputImage, input_index, width, height, width,
                       self.gaussFilter,
                       - self.gaussFilterWidth, self.gaussFilterWidth,
                       1, VL_PAD_BY_CONTINUITY | VL_TRANSPOSE) ;

      vl_imconvcol_vf (outputImage, output_index, width,
                       tempImage, temp_index, height, width, height,
                       self.gaussFilter,
                       - self.gaussFilterWidth, self.gaussFilterWidth,
                       1, VL_PAD_BY_CONTINUITY | VL_TRANSPOSE) ;
    }


    private int
    vl_sift_process_first_octave (VlSiftFilt f, float im[])
    {
      int i, o, s, h, w ;
      double sa, sb ;
      float octave[] ;

      /* shortcuts */
      float temp[]   = f.temp ;
      int width           = f.width ;
      int height          = f.height ;
      int o_min           = f.o_min ;
      int s_min           = f.s_min ;
      int s_max           = f.s_max ;
      double sigma0       = f.sigma0 ;
      double sigmak       = f.sigmak ;
      double sigman       = f.sigman ;
      double dsigma0      = f.dsigma0 ;

      /* restart from the first */
      f.o_cur = o_min ;
      f.nkeys = 0 ;
      w = f.octave_width  = VL_SHIFT_LEFT(f.width,  - f.o_cur) ;
      h = f.octave_height = VL_SHIFT_LEFT(f.height, - f.o_cur) ;

      /* is there at least one octave? */
      if (f.O == 0)
        return VL_ERR_EOF ;

      /* ------------------------------------------------------------------
       *                     Compute the first sublevel of the first octave
       * --------------------------------------------------------------- */

      /*
       * If the first octave has negative index, we upscale the image; if
       * the first octave has positive index, we downscale the image; if
       * the first octave has index zero, we just copy the image.
       */

      int octave_index[] = new int[1];
      octave = vl_sift_get_octave (f, s_min, octave_index) ;

      if (o_min < 0) {
        /* double once */
    	int temp_index = 0;
    	int im_index = 0;
        copy_and_upsample_rows (temp,   temp_index, im,   im_index, width,      height) ;
        copy_and_upsample_rows (octave, octave_index[0], temp, temp_index, height, 2 * width ) ;

        /* double more */
        for(o = -1 ; o > o_min ; --o) {
          copy_and_upsample_rows (temp, temp_index, octave, octave_index[0],
                                  width << -o,      height << -o ) ;
          copy_and_upsample_rows (octave, octave_index[0], temp, temp_index,
                                  width << -o, 2 * (height << -o)) ;
        }
      }
      else if (o_min > 0) {
        /* downsample */
    	int im_index = 0;
        copy_and_downsample (octave, octave_index[0], im, im_index, width, height, o_min) ;
      }
      else {
        /* direct copy */
    	int length = width * height;
    	for (i = 0; i < length; i++) {
    		octave[i + octave_index[0]] = im[i];
    	}
      }

      /*
       * Here we adjust the smoothing of the first level of the octave.
       * The input image is assumed to have nominal smoothing equal to
       * f->simgan.
       */

      sa = sigma0 * Math.pow (sigmak,   s_min) ;
      sb = sigman * Math.pow (2.0,    - o_min) ;

      if (sa > sb) {
        double sd = Math.sqrt (sa*sa - sb*sb) ;
        _vl_sift_smooth (f, octave, octave_index[0], temp, octave, octave_index[0], w, h, sd) ;
      }

      /* -----------------------------------------------------------------
       *                                          Compute the first octave
       * -------------------------------------------------------------- */

      for(s = s_min + 1 ; s <= s_max ; ++s) {
        double sd = dsigma0 * Math.pow (sigmak, s) ;
        int output_index[] = new int[1];
        float output_octave[] = vl_sift_get_octave(f, s, output_index);
        int input_index[] = new int[1];
        float input_octave[] = vl_sift_get_octave(f, s-1, input_index);
        _vl_sift_smooth (f, output_octave, output_index[0], temp,
                         input_octave, input_index[0], w, h, sd) ;
      }

      return VL_ERR_OK ;
    }

    /** ------------------------------------------------------------------
     ** @brief Get current octave data
     ** @param f SIFT filter.
     ** @param s level index.
     **
     ** The level index @a s ranges in the interval <tt>s_min = -1</tt>
     ** and <tt> s_max = S + 2</tt>, where @c S is the number of levels
     ** per octave.
     **
     ** @return pointer to the octave data for level @a s.
     **/

    private float[]
    vl_sift_get_octave (VlSiftFilt f, int s, int octave_index[])
    {
      int w =  f.octave_width;
      int h = f.octave_height;
      octave_index[0] = w * h * (s - f.s_min);
      return f.octave ;
    }
    
    /** ------------------------------------------------------------------
     ** @brief Process next octave
     **
     ** @param f SIFT filter.
     **
     ** The function computes the next octave of the Gaussian scale space.
     ** Notice that this clears the record of any feature detected in the
     ** previous octave.
     **
     ** @return error code. The function returns the error
     ** ::VL_ERR_EOF when there are no more octaves to process.
     **
     ** @sa ::vl_sift_process_first_octave().
     **/

    private int
    vl_sift_process_next_octave (VlSiftFilt f)
    {

      int s, h, w, s_best ;
      double sa, sb ;
      float octave[], pt[] ;
      int octave_index[] = new int[1];
      int pt_index[] = new int[1];

      /* shortcuts */
      float temp[]   = f.temp ;
      int O               = f.O ;
      int S               = f.S ;
      int o_min           = f.o_min ;
      int s_min           = f.s_min ;
      int s_max           = f.s_max ;
      double sigma0       = f.sigma0 ;
      double sigmak       = f.sigmak ;
      double dsigma0      = f.dsigma0 ;

      /* is there another octave ? */
      if (f.o_cur == o_min + O - 1)
        return VL_ERR_EOF ;

      /* retrieve base */
      s_best = Math.min(s_min + S, s_max) ;
      w      = f.octave_width ;
      h      = f.octave_height ;
      pt     = vl_sift_get_octave        (f, s_best, pt_index) ;
      octave = vl_sift_get_octave        (f, s_min, octave_index) ;

      /* next octave */
      copy_and_downsample (octave, octave_index[0], pt, pt_index[0], w, h, 1) ;

      f.o_cur            += 1 ;
      f.nkeys             = 0 ;
      w = f.octave_width  = VL_SHIFT_LEFT(f.width,  - f.o_cur) ;
      h = f.octave_height = VL_SHIFT_LEFT(f.height, - f.o_cur) ;

      sa = sigma0 * Math.pow (sigmak, s_min     ) ;
      sb = sigma0 * Math.pow (sigmak, s_best - S) ;

      if (sa > sb) {
        double sd = Math.sqrt (sa*sa - sb*sb) ;
        _vl_sift_smooth (f, octave, octave_index[0], temp, octave, octave_index[0], w, h, sd) ;
      }

      /* ------------------------------------------------------------------
       *                                                        Fill octave
       * --------------------------------------------------------------- */

      for(s = s_min + 1 ; s <= s_max ; ++s) {
        double sd = dsigma0 * Math.pow (sigmak, s) ;
        int output_index[] = new int[1];
        float output_octave[] = vl_sift_get_octave(f, s, output_index);
        int input_index[] = new int[1];
        float input_octave[] = vl_sift_get_octave(f, s-1, input_index);
        _vl_sift_smooth (f, output_octave, output_index[0], temp,
                         input_octave, input_index[0], w, h, sd) ;
      }

      return VL_ERR_OK ;
    }

    /* ----------------------------------------------------------------- */
    /** @brief Save octave on disk
     ** @internal
     **/
    private int
    save_gss (VlSiftFilt filt, VlFileMeta fm, String basename,
              boolean verbose)
    {
      String tmpString;
      int S = filt.S ;
      int i ;
      int s, err = 0 ;
      int w, h ;
      int o = filt.o_cur ;
      VlPgmImage pim = new VlPgmImage();
      byte buffer[] = null;
      int q ;

      if (! fm.active) {
        return VL_ERR_OK ;
      }

      w = filt.octave_width ;
      h = filt.octave_height;

      pim.width     = w ;
      pim.height    = h ;
      pim.max_value = 255 ;
      pim.is_raw    = 1 ;

      try {
          buffer = new byte[w * h];
      }
      catch (OutOfMemoryError e) {
    	  err = VL_ERR_ALLOC;
    	  vl_file_meta_close (fm) ;
          return err ;
      }

      q = basename.length();
      

      for (s = 0 ; s < S ; ++s) {
    	int pt_index[] = new int[1];
        float pt[] = vl_sift_get_octave (filt, s, pt_index) ;

        /* conversion */
        int length = w * h;
        for (i = 0 ; i < length; ++i) {
          float value = pt[pt_index[0] + i];
          if (value < 0) {
              buffer[i] = (byte) (value - 0.5f);
          } else {
              buffer[i] = (byte) (value + 0.5f);
          }
        }

        /* save */
        String oString;
        if (o < 10) {
        	oString = "_0"+String.valueOf(o);
        }
        else {
        	oString = "_"+String.valueOf(o);
        }
       
        String sString;
        if (s < 10) {
        	sString = "_00"+String.valueOf(s);
        }
        else if (s < 100) {
        	sString = "_0"+String.valueOf(s);
        }
        else {
        	sString = "_"+String.valueOf(s);
        }
        tmpString = basename + oString + sString;
        

        int error[] = new int[1];
        RandomAccessFile fmFile = vl_file_meta_open (fm, tmpString, "rw",error) ;
        if (error[0] > 0) {
        	buffer = null;
        	vl_file_meta_close (fm) ;
            return error[0];
        }

        err = vl_pgm_insert (fmFile, pim, buffer) ; 

        if (fmFile != null) {
  		  try {
  	    	  fmFile.close();
  	      }
  	      catch (IOException e) {
					System.err.println("IOException " + e + " on fmFile.close()");
				    Preferences.debug("IOException " + e + " on fmFile.close()\n",Preferences.DEBUG_ALGORITHM);
	      }  
  	    }
        vl_file_meta_close (fm) ;
        if (err > 0) {
        	buffer = null;
        	return err;
        }
        
        if (verbose) {
            System.out.println("sift: saved gss level to " + fm.name) ;
            Preferences.debug("sift: saved gss level to " + fm.name + "\n", Preferences.DEBUG_ALGORITHM);
        }
      }

      buffer = null;
      return err ;
    }
    
    private int
    vl_pgm_insert(RandomAccessFile f, VlPgmImage im, byte data[])
    {
      int bpp = vl_pgm_get_bpp (im) ;
      int data_size = vl_pgm_get_npixels (im) ;
      int c ;

      /* write preamble */
      String preambleString = "P5\n"+String.valueOf(im.width)+"\n"+String.valueOf(im.height)+"\n"+String.valueOf(im.max_value)+"\n";
      byte preambleBytes[] = preambleString.getBytes();
      try {
    	  f.write(preambleBytes);
      }
      catch (IOException e) {
    	 System.err.println("IOException " + e);
    	 Preferences.debug("IOException " + e + "\n", Preferences.DEBUG_ALGORITHM);
    	 return VL_ERR_PGM_IO;
      }
      try {
    	  f.write(data);
      }
      catch (IOException e) {
    	 System.err.println("IOException " + e);
    	 Preferences.debug("IOException " + e + "\n", Preferences.DEBUG_ALGORITHM);
    	 return VL_ERR_PGM_IO;
      }
      
      return 0 ;
    }
    
    /** ------------------------------------------------------------------
     ** @brief Detect keypoints
     **
     ** The function detect keypoints in the current octave filling the
     ** internal keypoint buffer. Keypoints can be retrieved by
     ** ::vl_sift_get_keypoints().
     **
     ** @param f SIFT filter.
     **/

    private void
    vl_sift_detect (VlSiftFilt f)
    {
      float dog[]   = f.dog ;
      int          s_min = f.s_min ;
      int          s_max = f.s_max ;
      int          w     = f.octave_width ;
      int          h     = f.octave_height ;
      double       te    = f.edge_thresh ;
      double       tp    = f.peak_thresh ;

      final int xo    = 1 ;      /* x-stride */
      final int yo    = w ;      /* y-stride */
      final int so    = w * h ;  /* s-stride */

      double       xper  = Math.pow (2.0, f.o_cur) ;

      int x, y, s, i, ii, jj, i2 ;
      float pt[], v ;
      VlSiftKeypoint k = new VlSiftKeypoint() ;

      /* clear current list */
      f.nkeys = 0 ;

      /* compute difference of gaussian (DoG) */
      pt = f.dog ;
      int pt_index = 0;
      for (s = s_min ; s <= s_max - 1 ; ++s) {
    	int src_a_index[] = new int[1];
        float src_a[] = vl_sift_get_octave (f, s, src_a_index    ) ;
        int src_b_index[] = new int[1];
        float src_b[] = vl_sift_get_octave (f, s + 1, src_b_index) ;
        int end_a = src_a_index[0] + w * h ;
        while (src_a_index[0] != end_a) {
          pt[pt_index++] = src_b[src_b_index[0]++] - src_a[src_a_index[0]++] ;
        }
      }

      /* -----------------------------------------------------------------
       *                                          Find local maxima of DoG
       * -------------------------------------------------------------- */

      /* start from dog [1,1,s_min+1] */
      pt = dog;
      pt_index = xo + yo + so;

      for(s = s_min + 1 ; s <= s_max - 2 ; ++s) {
        for(y = 1 ; y < h - 1 ; ++y) {
          for(x = 1 ; x < w - 1 ; ++x) {
            v = pt[pt_index] ;

           if (( v >= 0.8 * tp &&
              v > pt[pt_index + xo] &&
              v > pt[pt_index - xo] &&                       
              v > pt[pt_index + so] &&                       
              v > pt[pt_index - so] &&                       
              v > pt[pt_index + yo] &&                       
              v > pt[pt_index - yo] &&                       
                                                        
              v > pt[pt_index + yo + xo] &&                  
              v > pt[pt_index + yo - xo] &&                  
              v > pt[pt_index - yo + xo] &&                  
              v > pt[pt_index - yo - xo] &&                  
                                                        
              v > pt[pt_index + xo      + so] &&             
              v > pt[pt_index - xo      + so] &&             
              v > pt[pt_index + yo      + so] &&             
              v > pt[pt_index - yo      + so] &&             
              v > pt[pt_index + yo + xo + so] &&             
              v > pt[pt_index + yo - xo + so] &&             
              v > pt[pt_index - yo + xo + so] &&             
              v > pt[pt_index - yo - xo + so] &&             
                                                        
              v > pt[pt_index + xo      - so] &&             
              v > pt[pt_index - xo      - so] &&             
              v > pt[pt_index + yo      - so] &&             
              v > pt[pt_index - yo      - so] &&             
              v > pt[pt_index + yo + xo - so] &&             
              v > pt[pt_index + yo - xo - so] &&            
              v > pt[pt_index - yo + xo - so] &&             
              v > pt[pt_index - yo - xo - so] ) ||
            
            ( v <= -0.8 * tp &&                
            v < pt[pt_index + xo] &&                       
            v < pt[pt_index - xo] &&                       
            v < pt[pt_index + so] &&                       
            v < pt[pt_index - so] &&                       
            v < pt[pt_index + yo] &&                       
            v < pt[pt_index - yo] &&                       
                                                      
            v < pt[pt_index + yo + xo] &&                  
            v < pt[pt_index + yo - xo] &&                  
            v < pt[pt_index - yo + xo] &&                  
            v < pt[pt_index - yo - xo] &&                  
                                                      
            v < pt[pt_index + xo      + so] &&             
            v < pt[pt_index - xo      + so] &&             
            v < pt[pt_index + yo      + so] &&             
            v < pt[pt_index - yo      + so] &&             
            v < pt[pt_index + yo + xo + so] &&             
            v < pt[pt_index + yo - xo + so] &&             
            v < pt[pt_index - yo + xo + so] &&             
            v < pt[pt_index - yo - xo + so] &&             
                                                      
            v < pt[pt_index + xo      - so] &&             
            v < pt[pt_index - xo      - so] &&             
            v < pt[pt_index + yo      - so] &&             
            v < pt[pt_index - yo      - so] &&             
            v < pt[pt_index + yo + xo - so] &&             
            v < pt[pt_index + yo - xo - so] &&             
            v < pt[pt_index - yo + xo - so] &&             
            v < pt[pt_index - yo - xo - so] )) {

              /* make room for more keypoints */
              if (f.nkeys >= f.keys_res) {
                f.keys_res += 500 ;
                if (f.keys != null) {
                  int keysLength = f.keys.length;
                  VlSiftKeypoint tempKeys[] = new VlSiftKeypoint[keysLength];
                  for (i = 0; i < keysLength; i++) {
                	  tempKeys[i] = new VlSiftKeypoint();
                	  tempKeys[i].o = f.keys[i].o;
                	  tempKeys[i].ix = f.keys[i].ix;
                	  tempKeys[i].iy = f.keys[i].iy;
                	  tempKeys[i].is = f.keys[i].is;
                	  tempKeys[i].x = f.keys[i].x;
                	  tempKeys[i].y = f.keys[i].y;
                	  tempKeys[i].s = f.keys[i].s;
                	  tempKeys[i].sigma = f.keys[i].sigma;
                	  f.keys[i] = null;
                  }
                  f.keys = null;
                  f.keys = new VlSiftKeypoint[f.keys_res];
                  for (i = 0; i < keysLength; i++) {
                	  f.keys[i] = new VlSiftKeypoint();
                	  f.keys[i].o = tempKeys[i].o;
                	  f.keys[i].ix = tempKeys[i].ix;
                	  f.keys[i].iy = tempKeys[i].iy;
                	  f.keys[i].is = tempKeys[i].is;
                	  f.keys[i].x = tempKeys[i].x;
                	  f.keys[i].y = tempKeys[i].y;
                	  f.keys[i].s = tempKeys[i].s;
                	  f.keys[i].sigma = tempKeys[i].sigma;
                	  tempKeys[i] = null;
                  }
                  tempKeys = null;
                  for (i = keysLength; i < f.keys_res; i++) {
                	  f.keys[i] = new VlSiftKeypoint();
                  }
                } else {
                  f.keys = new VlSiftKeypoint[f.keys_res] ;
                  for (i = 0; i < f.keys_res; i++) {
                	  f.keys[i] = new VlSiftKeypoint();
                  }
                }
              }

              k = f.keys[f.nkeys ++] ;

              k.ix = x ;
              k.iy = y ;
              k.is = s ;
            }
            pt_index += 1 ;
          }
          pt_index += 2 ;
        }
        pt_index += 2 * yo ;
      }

      /* -----------------------------------------------------------------
       *                                               Refine local maxima
       * -------------------------------------------------------------- */

      /* this pointer is used to write the keypoints back */
      int k_index = 0;
      k = f.keys[k_index] ;

      for (i = 0 ; i < f.nkeys ; ++i) {

        x = f.keys [i] .ix ;
        y = f.keys [i] .iy ;
        s = f.keys [i]. is ;

        double Dx=0,Dy=0,Ds=0,Dxx=0,Dyy=0,Dss=0,Dxy=0,Dxs=0,Dys=0 ;
        double A[] = new double [3*3];
        double b[] = new double [3] ;

        int dx = 0 ;
        int dy = 0 ;

        int iter, j ;

        for (iter = 0 ; iter < 5 ; ++iter) {

          x += dx ;
          y += dy ;

          pt = dog;
          pt_index = xo * x
            + yo * y
            + so * (s - s_min) ;

          /* compute the gradient */
          Dx = 0.5 * (pt[pt_index + xo] -pt[pt_index -xo]);
          Dy = 0.5 * (pt[pt_index + yo] - pt[pt_index - yo]);
          Ds = 0.5 * (pt[pt_index + so] - pt[pt_index - so]);

          /* compute the Hessian */
          Dxx = (pt[pt_index + xo] + pt[pt_index - xo] - 2.0 * pt[pt_index]);
          Dyy = (pt[pt_index + yo] + pt[pt_index - yo] - 2.0 * pt[pt_index]);
          Dss = (pt[pt_index + so] + pt[pt_index - so] - 2.0 * pt[pt_index]);

          Dxy = 0.25 * (pt[pt_index + xo + yo] + pt[pt_index - xo - yo] - pt[pt_index - xo + yo] - pt[pt_index + xo - yo]);
          Dxs = 0.25 * (pt[pt_index + xo + so] + pt[pt_index - xo - so] - pt[pt_index - xo + so] - pt[pt_index + xo - so]);
          Dys = 0.25 * (pt[pt_index + yo + so] + pt[pt_index - yo - so] - pt[pt_index - yo + so] - pt[pt_index + yo - so]);

          /* solve linear system ....................................... */
          A[0] = Dxx;
          A[4] = Dyy;
          A[8] = Dss;
          A[3] = A[1] = Dxy;
          A[6] = A[2] = Dxs;
          A[7] = A[5] = Dys;

          b[0] = - Dx ;
          b[1] = - Dy ;
          b[2] = - Ds ;

          /* Gauss elimination */
          for(j = 0 ; j < 3 ; ++j) {
            double maxa    = 0 ;
            double maxabsa = 0 ;
            int    maxi    = -1 ;
            double tmp ;

            /* look for the maximally stable pivot */
            for (i2 = j ; i2 < 3 ; ++i2) {
              double a = A[i2 + 3*j];
              double absa = Math.abs(a) ;
              if (absa > maxabsa) {
                maxa    = a ;
                maxabsa = absa ;
                maxi    = i2 ;
              }
            }

            /* if singular give up */
            if (maxabsa < 1e-10f) {
              b[0] = 0 ;
              b[1] = 0 ;
              b[2] = 0 ;
              break ;
            }

            i2 = maxi ;

            /* swap j-th row with i-th row and normalize j-th row */
            for(jj = j ; jj < 3 ; ++jj) {
              tmp = A[i2+3*jj] ; A[i2+3*jj] = A[j+3*jj] ; A[j+3*jj] = tmp ;
              A[j+3*jj] /= maxa ;
            }
            tmp = b[j] ; b[j] = b[i2] ; b[i2] = tmp ;
            b[j] /= maxa ;

            /* elimination */
            for (ii = j+1 ; ii < 3 ; ++ii) {
              double xd = A[ii+3*j] ;
              for (jj = j ; jj < 3 ; ++jj) {
                A[ii+3*jj] -= xd * A[j+3*jj] ;
              }
              b[ii] -= xd * b[j] ;
            }
          }

          /* backward substitution */
          for (i2 = 2 ; i2 > 0 ; --i2) {
            double xd = b[i2] ;
            for (ii = i2-1 ; ii >= 0 ; --ii) {
              b[ii] -= xd * A[ii+3*i2] ;
            }
          }

          /* .......................................................... */
          /* If the translation of the keypoint is big, move the keypoint
           * and re-iterate the computation. Otherwise we are all set.
           */

          dx= ((b[0] >  0.6 && x < w - 2) ?  1 : 0)
            + ((b[0] < -0.6 && x > 1    ) ? -1 : 0) ;

          dy= ((b[1] >  0.6 && y < h - 2) ?  1 : 0)
            + ((b[1] < -0.6 && y > 1    ) ? -1 : 0) ;

          if (dx == 0 && dy == 0) break ;
        }

        /* check threshold and other conditions */
        {
          double val   = pt[pt_index]
            + 0.5 * (Dx * b[0] + Dy * b[1] + Ds * b[2]) ;
          double score = (Dxx+Dyy)*(Dxx+Dyy) / (Dxx*Dyy - Dxy*Dxy) ;
          double xn = x + b[0] ;
          double yn = y + b[1] ;
          double sn = s + b[2] ;

          boolean good =
            Math.abs (val)  > tp                  &&
            score           < (te+1)*(te+1)/te    &&
            score           >= 0                  &&
            Math.abs (b[0]) <  1.5                &&
            Math.abs (b[1]) <  1.5                &&
            Math.abs (b[2]) <  1.5                &&
            xn              >= 0                  &&
            xn              <= w - 1              &&
            yn              >= 0                  &&
            yn              <= h - 1              &&
            sn              >= s_min              &&
            sn              <= s_max ;

          if (good) {
            k.o     = f.o_cur ;
            k.ix    = x ;
            k.iy    = y ;
            k.is    = s ;
            k.s     = (float)sn ;
            k.x     = (float)(xn * xper) ;
            k.y     = (float)(yn * xper) ;
            k.sigma = (float)(f.sigma0 * Math.pow (2.0, sn/f.S) * xper) ;
            k = f.keys[++k_index] ;
          }

        } /* done checking */
      } /* next keypoint to refine */

      /* update keypoint count */
      f.nkeys = k_index;
    }
    
    /** ------------------------------------------------------------------
     ** @brief Initialize a keypoint from its position and scale
     **
     ** @param f     SIFT filter.
     ** @param k     SIFT keypoint (output).
     ** @param x     x coordinate of the keypoint center.
     ** @param y     y coordinate of the keypoint center.
     ** @param sigma keypoint scale.
     **
     ** The function initializes a keypoint structure @a k from
     ** the location @a x
     ** and @a y and the scale @a sigma of the keypoint. The keypoint structure
     ** maps the keypoint to an octave and scale level of the discretized
     ** Gaussian scale space, which is required for instance to compute the
     ** keypoint SIFT descriptor.
     **
     ** @par Algorithm
     **
     ** The formula linking the keypoint scale sigma to the octave and
     ** scale indexes is
     **
     ** @f[ \sigma(o,s) = \sigma_0 2^{o+s/S} @f]
     **
     ** In addition to the scale index @e s (which can be fractional due
     ** to scale interpolation) a keypoint has an integer scale index @e
     ** is too (which is the index of the scale level where it was
     ** detected in the DoG scale space). We have the constraints (@ref
     ** sift-tech-detector see also the "SIFT detector"):
     **
     ** - @e o is integer in the range @f$ [o_\mathrm{min},
     **   o_{\mathrm{min}}+O-1] @f$.
     ** - @e is is integer in the range @f$ [s_\mathrm{min}+1,
     **   s_\mathrm{max}-2] @f$.  This depends on how the scale is
     **   determined during detection, and must be so here because
     **   gradients are computed only for this range of scale levels
     **   and are required for the calculation of the SIFT descriptor.
     ** - @f$ |s - is| < 0.5 @f$ for detected keypoints in most cases due
     **   to the interpolation technique used during detection. However
     **   this is not necessary.
     **
     ** Thus octave o represents scales @f$ \{ \sigma(o, s) : s \in
     ** [s_\mathrm{min}+1-.5, s_\mathrm{max}-2+.5] \} @f$. Note that some
     ** scales may be represented more than once. For each scale, we
     ** select the largest possible octave that contains it, i.e.
     **
     ** @f[
     **  o(\sigma)
     **  = \max \{ o \in \mathbb{Z} :
     **    \sigma_0 2^{\frac{s_\mathrm{min}+1-.5}{S}} \leq \sigma \}
     **  = \mathrm{floor}\,\left[
     **    \log_2(\sigma / \sigma_0) - \frac{s_\mathrm{min}+1-.5}{S}\right]
     ** @f]
     **
     ** and then
     **
     ** @f[
     ** s(\sigma) = S  \left[\log_2(\sigma / \sigma_0) - o(\sigma)\right],
     ** \quad
     ** is(\sigma) = \mathrm{round}\,(s(\sigma))
     ** @f]
     **
     ** In practice, both @f$ o(\sigma) @f$ and @f$ is(\sigma) @f$ are
     ** clamped to their feasible range as determined by the SIFT filter
     ** parameters.
     **/

    private void
    vl_sift_keypoint_init (VlSiftFilt f,
                           VlSiftKeypoint k,
                           double x,
                           double y,
                           double sigma)
    {
      int    o, ix, iy, is ;
      double s, phi, xper ;

      phi = log2 ((sigma + VL_EPSILON_D) / f.sigma0) ;
      o   = (int)Math.floor (phi -  ((double) f.s_min + 0.5) / f.S) ;
      o   = Math.min(o, f.o_min + f.O - 1) ;
      o   = Math.max (o, f.o_min           ) ;
      s   = f.S * (phi - o) ;

      is  = (int)(s + 0.5) ;
      is  = Math.min(is, f.s_max - 2) ;
      is  = Math.max(is, f.s_min + 1) ;

      xper = Math.pow (2.0, o) ;
      ix   = (int)(x / xper + 0.5) ;
      iy   = (int)(y / xper + 0.5) ;

      k.o  = o ;

      k.ix = ix ;
      k.iy = iy ;
      k.is = is ;

      k.x = (float)x ;
      k.y = (float)y ;
      k.s = (float)s ;

      k.sigma = (float)sigma ;
    }

    /** ------------------------------------------------------------------
     ** @brief Calculate the keypoint orientation(s)
     **
     ** @param f        SIFT filter.
     ** @param angles   orientations (output).
     ** @param k        keypoint.
     **
     ** The function computes the orientation(s) of the keypoint @a k.
     ** The function returns the number of orientations found (up to
     ** four). The orientations themselves are written to the vector @a
     ** angles.
     **
     ** @remark The function requires the keypoint octave @a k->o to be
     ** equal to the filter current octave ::vl_sift_get_octave. If this
     ** is not the case, the function returns zero orientations.
     **
     ** @remark The function requires the keypoint scale level @c k->s to
     ** be in the range @c s_min+1 and @c s_max-2 (where usually @c
     ** s_min=0 and @c s_max=S+2). If this is not the case, the function
     ** returns zero orientations.
     **
     ** @return number of orientations found.
     **/

    private int
    vl_sift_calc_keypoint_orientations (VlSiftFilt f,
                                        double angles [],
                                        VlSiftKeypoint k)
    {
      final double winf   = 1.5 ;
      double       xper   = Math.pow (2.0, f.o_cur) ;

      int          w      = f.octave_width ;
      int          h      = f.octave_height ;
      final int    xo     = 2 ;         /* x-stride */
      final int    yo     = 2 * w ;     /* y-stride */
      final int    so     = 2 * w * h ; /* s-stride */
      double       x      = k.x     / xper ;
      double       y      = k.y     / xper ;
      double       sigma  = k.sigma / xper ;

      int          xi     = (int) (x + 0.5) ;
      int          yi     = (int) (y + 0.5) ;
      int          si     = k.is ;

      final double sigmaw = winf * sigma ;
      int          W      = Math.max((int)Math.floor (3.0 * sigmaw), 1) ;

      int          nangles= 0 ;

      int nbins = 36;

      double hist[] = new double [nbins];
      double maxh ;
      float pt[] ;
      int xs, ys, iter, i ;

      /* skip if the keypoint octave is not current */
      if(k.o != f.o_cur)
        return 0 ;

      /* skip the keypoint if it is out of bounds */
      if(xi < 0            ||
         xi > w - 1        ||
         yi < 0            ||
         yi > h - 1        ||
         si < f.s_min + 1 ||
         si > f.s_max - 2  ) {
        return 0 ;
      }

      /* make gradient up to date */
      update_gradient (f) ;

      /* compute orientation histogram */
      int pt_index = xo*xi + yo*yi + so*(si - f.s_min - 1) ;
      pt = f.grad;

      for(ys  =  Math.max (- W,       - yi) ;
          ys <=  Math.min (+ W, h - 1 - yi) ; ++ys) {

        for(xs  = Math.max (- W,       - xi) ;
            xs <= Math.min (+ W, w - 1 - xi) ; ++xs) {


          double dx = (double)(xi + xs) - x;
          double dy = (double)(yi + ys) - y;
          double r2 = dx*dx + dy*dy ;
          double wgt, mod, ang, fbin ;

          /* limit to a circular window */
          if (r2 >= W*W + 0.6) continue ;

          wgt  = Math.exp (r2 / (2*sigmaw*sigmaw)) ;
          mod  = pt[pt_index + xs*xo + ys*yo    ] ;
          ang  = pt[pt_index + xs*xo + ys*yo + 1] ;
          fbin = nbins * ang / (2 * Math.PI) ;

    if (VL_SIFT_BILINEAR_ORIENTATIONS)
          {
            int bin = (int) Math.floor (fbin - 0.5) ;
            double rbin = fbin - bin - 0.5 ;
            hist [(bin + nbins) % nbins] += (1 - rbin) * mod * wgt ;
            hist [(bin + 1    ) % nbins] += (    rbin) * mod * wgt ;
          }
    else
          {
            int    bin  = (int)Math.floor (fbin) ;
            bin = (int)Math.floor (nbins * ang / (2*Math.PI)) ;
            hist [(bin) % nbins] += mod * wgt ;
          }

        } /* for xs */
      } /* for ys */

      /* smooth histogram */
      for (iter = 0; iter < 6; iter ++) {
        double prev  = hist [nbins - 1] ;
        double first = hist [0] ;
        for (i = 0; i < nbins - 1; i++) {
          double newh = (prev + hist[i] + hist[(i+1) % nbins]) / 3.0;
          prev = hist[i] ;
          hist[i] = newh ;
        }
        hist[i] = (prev + hist[i] + first) / 3.0 ;
      }

      /* find the histogram maximum */
      maxh = 0 ;
      for (i = 0 ; i < nbins ; ++i)
        maxh = Math.max (maxh, hist [i]) ;

      /* find peaks within 80% from max */
      nangles = 0 ;
      for(i = 0 ; i < nbins ; ++i) {
        double h0 = hist [i] ;
        double hm = hist [(i - 1 + nbins) % nbins] ;
        double hp = hist [(i + 1 + nbins) % nbins] ;

        /* is this a peak? */
        if (h0 > 0.8*maxh && h0 > hm && h0 > hp) {

          /* quadratic interpolation */
          double di = - 0.5 * (hp - hm) / (hp + hm - 2 * h0) ;
          double th = 2 * Math.PI * (i + di + 0.5) / nbins ;
          angles [ nangles++ ] = th ;
          if( nangles == 4 )
            return nangles ;
        }
      }
     enough_angles:
      return nangles ;
    }
    
    /** ------------------------------------------------------------------
     ** @internal
     ** @brief Update gradients to current GSS octave
     **
     ** @param f SIFT filter.
     **
     ** The function makes sure that the gradient buffer is up-to-date
     ** with the current GSS data.
     **
     ** @remark The minimum octave size is 2x2xS.
     **/

    private void
    update_gradient (VlSiftFilt f)
    {
      int       s_min = f.s_min ;
      int       s_max = f.s_max ;
      int       w     = f.octave_width ;
      int       h     = f.octave_height ;
      final int xo    = 1 ;
      final int yo    = w ;
      final int so    = h * w ;
      int y, s ;

      if (f.grad_o == f.o_cur) return ;

      for (s  = s_min + 1 ;
           s <= s_max - 2 ; ++ s) {

        float src[], grad[], gx, gy ;
        int end;

        int src_index[] = new int[1];
        src  = vl_sift_get_octave (f,s,src_index) ;
        int grad_index = 2 * so * (s - s_min - 1);
        grad = f.grad;

        /* first pixel of the first row */
        gx = src[src_index[0]+xo] - src[src_index[0]] ;
        gy = src[src_index[0]+yo] - src[src_index[0]] ;
        grad[grad_index++] = (float)Math.sqrt (gx*gx + gy*gy) ;
        grad[grad_index++] = vl_mod_2pi_f  ((float) (Math.atan2 (gy, gx) + 2*Math.PI)) ; 
        src_index[0]++ ;  

        /* middle pixels of the  first row */
        end = (src_index[0] - 1) + w - 1 ;
        while (src_index[0] < end) {
          gx = (float)(0.5 * (src[src_index[0]+xo] - src[src_index[0]-xo])) ;
          gy =        src[src_index[0]+yo] - src[src_index[0]] ;
          grad[grad_index++] = (float)Math.sqrt (gx*gx + gy*gy) ;
          grad[grad_index++] = vl_mod_2pi_f  ((float) (Math.atan2 (gy, gx) + 2*Math.PI)) ; 
          src_index[0]++ ;  
        }

        /* last pixel of the first row */
        gx = src[src_index[0]]   - src[src_index[0]-xo] ;
        gy = src[src_index[0]+yo] - src[src_index[0]] ;
        grad[grad_index++] = (float)Math.sqrt (gx*gx + gy*gy) ;
        grad[grad_index++] = vl_mod_2pi_f  ((float) (Math.atan2 (gy, gx) + 2*Math.PI)) ; 
        src_index[0]++ ;  

        for (y = 1 ; y < h -1 ; ++y) {

          /* first pixel of the middle rows */
          gx =        src[src_index[0]+xo] - src[src_index[0]] ;
          gy = (float)(0.5 * (src[src_index[0]+yo] - src[src_index[0]-yo])) ;
          grad[grad_index++] = (float)Math.sqrt (gx*gx + gy*gy) ;
          grad[grad_index++] = vl_mod_2pi_f  ((float) (Math.atan2 (gy, gx) + 2*Math.PI)) ; 
          src_index[0]++ ;  

          /* middle pixels of the middle rows */
          end = (src_index[0] - 1) + w - 1 ;
          while (src_index[0] < end) {
            gx = (float)(0.5 * (src[src_index[0]+xo] - src[src_index[0]-xo])) ;
            gy = (float)(0.5 * (src[src_index[0]+yo] - src[src_index[0]-yo])) ;
            grad[grad_index++] = (float)Math.sqrt (gx*gx + gy*gy) ;
            grad[grad_index++] = vl_mod_2pi_f  ((float) (Math.atan2 (gy, gx) + 2*Math.PI)) ; 
            src_index[0]++ ;  
          }

          /* last pixel of the middle row */
          gx =        src[src_index[0]]   - src[src_index[0]-xo] ;
          gy = (float)(0.5 * (src[src_index[0]+yo] - src[src_index[0]-yo])) ;
          grad[grad_index++] = (float)Math.sqrt (gx*gx + gy*gy) ;
          grad[grad_index++] = vl_mod_2pi_f  ((float) (Math.atan2 (gy, gx) + 2*Math.PI)) ; 
          src_index[0]++ ;
        }

        /* first pixel of the last row */
        gx = src[src_index[0]+xo] - src[src_index[0]] ;
        gy = src[src_index[0]] - src[src_index[0]-yo] ;
        grad[grad_index++] = (float)Math.sqrt (gx*gx + gy*gy) ;
        grad[grad_index++] = vl_mod_2pi_f  ((float) (Math.atan2 (gy, gx) + 2*Math.PI)) ; 
        src_index[0]++ ;

        /* middle pixels of the last row */
        end = (src_index[0] - 1) + w - 1 ;
        while (src_index[0] < end) {
          gx = (float)(0.5 * (src[src_index[0]+xo] - src[src_index[0]-xo])) ;
          gy =        src[src_index[0]]   - src[src_index[0]-yo] ;
          grad[grad_index++] = (float)Math.sqrt (gx*gx + gy*gy) ;
          grad[grad_index++] = vl_mod_2pi_f  ((float) (Math.atan2 (gy, gx) + 2*Math.PI)) ; 
          src_index[0]++ ;
        }

        /* last pixel of the last row */
        gx = src[src_index[0]]   - src[src_index[0]-xo] ;
        gy = src[src_index[0]]   - src[src_index[0]-yo] ;
        grad[grad_index++] = (float)Math.sqrt (gx*gx + gy*gy) ;
        grad[grad_index++] = vl_mod_2pi_f  ((float) (Math.atan2 (gy, gx) + 2*Math.PI)) ; 
        src_index[0]++ ;
      }
      f.grad_o = f.o_cur ;
    }
    
    private float
    vl_mod_2pi_f (float x)
    {
      while (x > (float)(2 * Math.PI)) x -= (float) (2 * Math.PI) ;
      while (x < 0.0F) x += (float) (2 * Math.PI);
      return x ;
    }
    
    /** ------------------------------------------------------------------
     ** @brief Compute the descriptor of a keypoint
     **
     ** @param f        SIFT filter.
     ** @param descr    SIFT descriptor (output)
     ** @param k        keypoint.
     ** @param angle0   keypoint direction.
     **
     ** The function computes the SIFT descriptor of the keypoint @a k of
     ** orientation @a angle0. The function fills the buffer @a descr
     ** which must be large enough to hold the descriptor.
     **
     ** The function assumes that the keypoint is on the current octave.
     ** If not, it does not do anything.
     **/

    private void
    vl_sift_calc_keypoint_descriptor (VlSiftFilt f,
                                      float descr[],
                                      VlSiftKeypoint k,
                                      double angle0)
    {
      /*
         The SIFT descriptor is a three dimensional histogram of the
         position and orientation of the gradient.  There are NBP bins for
         each spatial dimension and NBO bins for the orientation dimension,
         for a total of NBP x NBP x NBO bins.

         The support of each spatial bin has an extension of SBP = 3sigma
         pixels, where sigma is the scale of the keypoint.  Thus all the
         bins together have a support SBP x NBP pixels wide. Since
         weighting and interpolation of pixel is used, the support extends
         by another half bin. Therefore, the support is a square window of
         SBP x (NBP + 1) pixels. Finally, since the patch can be
         arbitrarily rotated, we need to consider a window 2W += sqrt(2) x
         SBP x (NBP + 1) pixels wide.
      */

      final double magnif      = f.magnif ;

      double       xper        = Math.pow (2.0, f.o_cur) ;

      int          w           = f.octave_width ;
      int          h           = f.octave_height ;
      final int    xo          = 2 ;         /* x-stride */
      final int    yo          = 2 * w ;     /* y-stride */
      final int    so          = 2 * w * h ; /* s-stride */
      double       x           = k.x     / xper ;
      double       y           = k.y     / xper ;
      double       sigma       = k.sigma / xper ;

      int          xi          = (int) (x + 0.5) ;
      int          yi          = (int) (y + 0.5) ;
      int          si          = k.is ;

      final double st0         = Math.sin (angle0) ;
      final double ct0         = Math.cos (angle0) ;
      final double SBP         = magnif * sigma + VL_EPSILON_D ;
      final int W           = (int)Math.floor
        (Math.sqrt(2.0) * SBP * (NBP + 1) / 2.0 + 0.5) ;

      final int binto = 1 ;          /* bin theta-stride */
      final int binyo = NBO * NBP ;  /* bin y-stride */
      final int binxo = NBO ;        /* bin x-stride */

      int bin, dxi, dyi ;
      int i;
      int pt_index;
      float pt[] ;
      int dpt_index;
      float dpt[] ;

      /* check bounds */
      if(k.o  != f.o_cur        ||
         xi    <  0               ||
         xi    >= w               ||
         yi    <  0               ||
         yi    >= h -    1        ||
         si    <  f.s_min + 1    ||
         si    >  f.s_max - 2     )
        return ;

      /* synchronize gradient buffer */
      update_gradient (f) ;

      /* VL_PRINTF("W = %d ; magnif = %g ; SBP = %g\n", W,magnif,SBP) ; */

      /* clear descriptor */
      int length = NBO * NBP * NBP;
      for (i = 0; i < length; i++) {
          descr[i] = 0.0f;  
      }

      /* Center the scale space and the descriptor on the current keypoint.
       * Note that dpt is pointing to the bin of center (SBP/2,SBP/2,0).
       */
      pt_index = xi*xo + yi*yo + (si - f.s_min - 1)*so ;
      pt  = f.grad;
      dpt_index = (NBP/2) * binyo + (NBP/2) * binxo ;
      dpt = descr;

      /*
       * Process pixels in the intersection of the image rectangle
       * (1,1)-(M-1,N-1) and the keypoint bounding box.
       */
      for(dyi =  Math.max (- W, 1 - yi    ) ;
          dyi <= Math.min (+ W, h - yi - 2) ; ++ dyi) {

        for(dxi =  Math.max (- W, 1 - xi    ) ;
            dxi <= Math.min (+ W, w - xi - 2) ; ++ dxi) {

          /* retrieve */
          float mod   = pt[pt_index + dxi*xo + dyi*yo + 0 ] ;
          float angle = pt[pt_index + dxi*xo + dyi*yo + 1 ] ;
          float theta = vl_mod_2pi_f((float) (angle - angle0) );

          /* fractional displacement */
          float dx = (float)(xi + dxi - x);
          float dy = (float)(yi + dyi - y);

          /* get the displacement normalized w.r.t. the keypoint
             orientation and extension */
          float nx = (float)(( ct0 * dx + st0 * dy) / SBP) ;
          float ny = (float)((-st0 * dx + ct0 * dy) / SBP) ;
          float nt = (float)(NBO * theta / (2 * Math.PI)) ;

          /* Get the Gaussian weight of the sample. The Gaussian window
           * has a standard deviation equal to NBP/2. Note that dx and dy
           * are in the normalized frame, so that -NBP/2 <= dx <=
           * NBP/2. */
          final float wsigma = (float)f.windowSize ;
          float win = (float)Math.exp
            ((nx*nx + ny*ny)/(2.0 * wsigma * wsigma)) ;

          /* The sample will be distributed in 8 adjacent bins.
             We start from the ``lower-left'' bin. */
          int         binx = (int)Math.floor (nx - 0.5) ;
          int         biny = (int)Math.floor(ny - 0.5) ;
          int         bint = (int)Math.floor(nt) ;
          float rbinx = (float)(nx - (binx + 0.5)) ;
          float rbiny = (float)(ny - (biny + 0.5)) ;
          float rbint = nt - bint ;
          int         dbinx ;
          int         dbiny ;
          int         dbint ;

          /* Distribute the current sample into the 8 adjacent bins*/
          for(dbinx = 0 ; dbinx < 2 ; ++dbinx) {
            for(dbiny = 0 ; dbiny < 2 ; ++dbiny) {
              for(dbint = 0 ; dbint < 2 ; ++dbint) {

                if (binx + dbinx >= - (NBP/2) &&
                    binx + dbinx <    (NBP/2) &&
                    biny + dbiny >= - (NBP/2) &&
                    biny + dbiny <    (NBP/2) ) {
                  float weight = win
                    * mod
                    * Math.abs (1 - dbinx - rbinx)
                    * Math.abs (1 - dbiny - rbiny)
                    * Math.abs (1 - dbint - rbint) ;

                  dpt[dpt_index + (binx+dbinx)*binxo + (biny+dbiny)*binyo + ((bint+dbint) % NBO)*binto] += weight ;
                }
              }
            }
          }
        }
      }

      /* Standard SIFT descriptors are normalized, truncated and normalized again */
      if(true) {

        /* Normalize the histogram to L2 unit length. */
        float norm = normalize_histogram (descr, 0, NBO*NBP*NBP) ;

        /* Set the descriptor to zero if it is lower than our norm_threshold */
        if((f.norm_thresh != 0) && (norm < f.norm_thresh)) {
            for(bin = 0; bin < NBO*NBP*NBP ; ++ bin)
                descr [bin] = 0;
        }
        else {

          /* Truncate at 0.2. */
          for(bin = 0; bin < NBO*NBP*NBP ; ++ bin) {
            if (descr [bin] > 0.2) descr [bin] = 0.2f;
          }

          /* Normalize again. */
          normalize_histogram (descr, 0, NBO*NBP*NBP) ;
        }
      }

    }
    
    /** ------------------------------------------------------------------
     ** @internal
     ** @brief Normalizes in norm L_2 a descriptor
     ** @param begin begin of histogram.
     ** @param end   end of histogram.
     **/

    private float
    normalize_histogram
    (float buf[], int begin, int end)
    {
      int iter ;
      float  norm = 0.0f ;

      for (iter = begin ; iter != end ; ++ iter)
        norm += (buf[iter]) * (buf[iter]) ;

      norm = (float)(Math.sqrt(norm) + VL_EPSILON_F) ;

      for (iter = begin; iter != end ; ++ iter)
        buf[iter] /= norm ;

      return norm;
    }
    
    /* ----------------------------------------------------------------- */
    /** @brief Write double to file
     **
     ** @param self   File meta information.
     ** @param x    Datum to write.
     **
     ** @return error code. The function returns ::VL_ERR_ALLOC if the
     ** datum cannot be written.
     **/

    private int
    vl_file_meta_put_double (int protocol, RandomAccessFile file, double x)
    {
      int err ;
      int n ;
      double y ;

      switch (protocol) {

      case VL_PROT_ASCII :
    	byte xBytes[] = (String.valueOf(x) + " ").getBytes();
        try {
        	file.write(xBytes);
        }
        catch (IOException e) {
        	System.err.println("IOException " + e + " in vl_file_meta_put_double");
        	Preferences.debug("IOException " + e + " in vl_file_meta_put_double\n", Preferences.DEBUG_ALGORITHM);
        	return VL_ERR_ALLOC;
        }
        break ;

      case VL_PROT_BINARY :
    	try {
    		file.writeDouble(x);
    	}
    	catch (IOException e) {
        	System.err.println("IOException " + e + " in vl_file_meta_put_double");
        	Preferences.debug("IOException " + e + " in vl_file_meta_put_double\n", Preferences.DEBUG_ALGORITHM);
        	return VL_ERR_ALLOC;
        }
      }
        
      return VL_ERR_OK ;
    }

    /* ----------------------------------------------------------------- */
    /** @brief Write uint8 to file
     **
     ** @param self   File meta information.
     ** @param x    Datum to write.
     **
     ** @return error code. The function returns ::VL_ERR_ALLOC if the
     ** datum cannot be written.
     **/

    private int vl_file_meta_put_uint8 (int protocol, RandomAccessFile file, byte x)
    {
      int n ;
      int err ;

      switch (protocol) {

      case VL_PROT_ASCII :
    	byte xBytes[] = (String.valueOf(x & 0xff) + " ").getBytes();
    	try {
        	file.write(xBytes);
        }
        catch (IOException e) {
        	System.err.println("IOException " + e + " in vl_file_meta_put_uint8");
        	Preferences.debug("IOException " + e + " in vl_file_meta_put_uint8\n", Preferences.DEBUG_ALGORITHM);
        	return VL_ERR_ALLOC;
        }
        break ;

      case VL_PROT_BINARY :
    	try {
    		file.writeByte(x);
    	}
    	catch (IOException e) {
        	System.err.println("IOException " + e + " in vl_file_meta_put_uint8");
        	Preferences.debug("IOException " + e + " in vl_file_meta_put_uint8\n", Preferences.DEBUG_ALGORITHM);
        	return VL_ERR_ALLOC;
        }
      }
       

      return VL_ERR_OK ;
    }
    
    /** ------------------------------------------------------------------
     ** @brief Host <-> big endian transformation for 8-bytes value
     **
     ** @param dst destination 8-byte buffer.
     ** @param src source 8-byte bufffer.
     ** @see @ref host-arch-endianness.
     **/

    /*VL_INLINE void
    vl_swap_host_big_endianness_8 (void *dst, void* src)
    {
      char *dst_ = (char*) dst ;
      char *src_ = (char*) src ;
    #if defined(VL_ARCH_BIG_ENDIAN)
        dst_ [0] = src_ [0] ;
        dst_ [1] = src_ [1] ;
        dst_ [2] = src_ [2] ;
        dst_ [3] = src_ [3] ;
        dst_ [4] = src_ [4] ;
        dst_ [5] = src_ [5] ;
        dst_ [6] = src_ [6] ;
        dst_ [7] = src_ [7] ;
    #else
        dst_ [0] = src_ [7] ;
        dst_ [1] = src_ [6] ;
        dst_ [2] = src_ [5] ;
        dst_ [3] = src_ [4] ;
        dst_ [4] = src_ [3] ;
        dst_ [5] = src_ [2] ;
        dst_ [6] = src_ [1] ;
        dst_ [7] = src_ [0] ;
    #endif
    }*/
    
    private void
    vl_sift_delete (VlSiftFilt f)
    {
      int i;
      if (f != null) {
        if (f.keys != null) {
            for (i = 0; i < f.keys.length; i++) {
            	f.keys[i] = null;
            }
            f.keys = null;
        }
        if (f.grad != null) {
        	f.grad = null;
        }
        if (f.dog != null) {
        	f.dog = null;
        }
        if (f.octave != null) {
        	f.octave = null;
        }
        if (f.temp != null) {
        	f.temp = null;
        };
        if (f.gaussFilter != null) {
        	f.gaussFilter = null;
        }
        f = null;
      }
    }
    
    private ModelImage sift_mosaic(ModelImage im1, ModelImage im2) {
       // SIFT_MOSAIC Demonstrates matching two images using SIFT and RANSAC
    
       //   SIFT_MOSAIC demonstrates matching two images based on SIFT
       //   features and RANSAC and computing their mosaic.
   
       //   SIFT_MOSAIC by itself runs the algorithm on two standard test
       //   images. Use SIFT_MOSAIC(IM1,IM2) to compute the mosaic of two
       //   custom images IM1 and IM2.
       AlgorithmChangeType changeAlgo;
       ModelImage im1g;
       ModelImage im2g;

       if (im1 == null) {
    	      final FileIO io = new FileIO();
    	      io.setQuiet(true);
              io.setSuppressProgressBar(true);
    	      im1 = io.readImage("C:" + File.separator + "SIFT" + File.separator + "vlfeat-0.9.21" + File.separator + "vlfeat-0.9.21" +
              File.separator + "data" + File.separator + "river1.jpg");
    	      im2 = io.readImage("C:" + File.separator + "SIFT" + File.separator + "vlfeat-0.9.21" + File.separator + "vlfeat-0.9.21" +
    	              File.separator + "data" + File.separator + "river2.jpg");
       }
       
       // make images float with minimum == 0 and maximum = 1
       if (((im1.getType() != ModelStorageBase.FLOAT) && (im1.getType() != ModelStorageBase.ARGB_FLOAT)) ||
    		   (im1.getMin() < 0) || (im1.getMax() != 1)) {
           if (im1.isColorImage()) {
        	    changeAlgo = new AlgorithmChangeType(im1,ModelStorageBase.ARGB_FLOAT, im1.getMin(), im1.getMax(), im1.getMin()/im1.getMax(), 1.0, true);   
           }
           else if (im1.getMin() >= 0) {
        	   changeAlgo = new AlgorithmChangeType(im1,ModelStorageBase.FLOAT, im1.getMin(), im1.getMax(), im1.getMin()/im1.getMax(), 1.0, true);   
           }
           else {
        	   changeAlgo = new AlgorithmChangeType(im1,ModelStorageBase.FLOAT, im1.getMin(), im1.getMax(), 0.0, 1.0, true);   
           }
           changeAlgo.run();
       }
       
       if (((im2.getType() != ModelStorageBase.FLOAT) && (im2.getType() != ModelStorageBase.ARGB_FLOAT)) ||
    		   (im2.getMin() < 0) || (im2.getMax() != 1)) {
           if (im1.isColorImage()) {
        	    changeAlgo = new AlgorithmChangeType(im2,ModelStorageBase.ARGB_FLOAT, im2.getMin(), im2.getMax(), im2.getMin()/im2.getMax(), 1.0, true);   
           }
           else if (im2.getMin() >= 0) {
        	   changeAlgo = new AlgorithmChangeType(im2,ModelStorageBase.FLOAT, im2.getMin(), im2.getMax(), im2.getMin()/im2.getMax(), 1.0, true);   
           }
           else {
        	   changeAlgo = new AlgorithmChangeType(im2,ModelStorageBase.FLOAT, im2.getMin(), im2.getMax(), 0.0, 1.0, true);   
           }
           changeAlgo.run();
       }
       
       // make grayscale
       AlgorithmRGBtoGray gAlgo;
       boolean intensityAverage = false;
       float threshold = 0.0f;
       boolean thresholdAverage = false;
       boolean equalRange = true;
       float redValue = 0.2989f;
       float greenValue = 0.5870f;
       float blueValue = 0.1140f;
       // minR, minG, minB, maxR, maxG, maxB not used
       float minR = 0.0f;
       float minG = 0.0f;
       float minB = 0.0f;
       float maxR = 255.0f;
       float maxG = 255.0f;
       float maxB = 255.0f;
       if (im1.isColorImage()) {
    	   im1g = new ModelImage(ModelStorageBase.FLOAT, im1.getExtents(), im1.getImageName() + "_gray");
    	   if (im1.getMinR() == im1.getMaxR()) {
				redValue = 0.0f;
				greenValue = 0.5f;
				blueValue = 0.5f;
			} else if (im1.getMinG() == im1.getMaxG()) {
				redValue = 0.5f;
				greenValue = 0.0f;
				blueValue = 0.5f;
			} else if (im1.getMinB() == im1.getMaxB()) {
				redValue = 0.5f;
				greenValue = 0.5f;
				blueValue = 0.0f;
			} 
    	   
			gAlgo = new AlgorithmRGBtoGray(im1g, im1,
					redValue, greenValue, blueValue, thresholdAverage,
					threshold, intensityAverage, equalRange, minR, maxR,
					minG, maxG, minB, maxB);
			gAlgo.run();
			gAlgo.finalize();
       }
       else {
    	   im1g = im1;
       }
       if (im2.isColorImage()) {
    	   im2g = new ModelImage(ModelStorageBase.FLOAT, im2.getExtents(), im2.getImageName() + "_gray");
    	   if (im2.getMinR() == im2.getMaxR()) {
				redValue = 0.0f;
				greenValue = 0.5f;
				blueValue = 0.5f;
			} else if (im2.getMinG() == im2.getMaxG()) {
				redValue = 0.5f;
				greenValue = 0.0f;
				blueValue = 0.5f;
			} else if (im2.getMinB() == im2.getMaxB()) {
				redValue = 0.5f;
				greenValue = 0.5f;
				blueValue = 0.0f;
			} 
    	   
			gAlgo = new AlgorithmRGBtoGray(im2g, im2,
					redValue, greenValue, blueValue, thresholdAverage,
					threshold, intensityAverage, equalRange, minR, maxR,
					minG, maxG, minB, maxB);
			gAlgo.run();
			gAlgo.finalize();
       }
       else {
    	   im2g = im2;
       }

       // --------------------------------------------------------------------
       //                                                         SIFT matches
       // --------------------------------------------------------------------

       //[f1,d1] = vl_sift(im1g) ;
       //[f2,d2] = vl_sift(im2g) ;
    
       return null;
    }

}
