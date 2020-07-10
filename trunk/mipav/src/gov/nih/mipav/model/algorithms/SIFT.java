package gov.nih.mipav.model.algorithms;

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
    private boolean verbose = false;
    private String outarg = null;
    private String framesarg = null;
    private String descriptorarg = null;
    private String metaarg = null;
    private String read_framesarg = null;
    private String gssarg = null;
    // Octaves
    private int O = -1;
    // Levels
    private int S = 3;
    // first_octave
    private int  omin = -1 ;
    private double   edge_thresh  = -1 ;
    private double   peak_thresh  = -1 ;
    private double   magnif       = -1 ;
    private boolean force_orientations = false ;
    
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
	
	/**
     * SIFT - default constructor.
     */
    public SIFT() { }
    
    public SIFT(String fileDir[], String fileName[], boolean verbose, String outarg, String framesarg,
    		String descriptorarg, String metaarg, String read_framesarg, String gssarg, int O, int S,
    		int omin, double edge_thresh, double peak_thresh, double magnif, boolean force_orientations) {
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
    }


    /** @brief SIFT driver entry point
     **/
    public void runAlgorithm() {

    	  int  err    = VL_ERR_OK ;
    	  String err_msg;
    	  int      n ;
    	  int      exit_code          = 0 ;
    	  boolean  force_output       = false ;
    	  int fileNum;
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
    	  VlFileMeta frm  = new VlFileMeta(false, "%.frame", new int[]{VL_PROT_ASCII}, "", null);
    	  VlFileMeta dsc  = new VlFileMeta(false, "%.descr", new int[]{VL_PROT_ASCII}, "", null);
    	  VlFileMeta met  = new VlFileMeta(false, "%.meta",  new int[]{VL_PROT_ASCII}, "", null);
    	  VlFileMeta gss  = new VlFileMeta(false, "%.pgm",   new int[]{VL_PROT_ASCII}, "", null);
    	  VlFileMeta ifr  = new VlFileMeta(false, "%.frame", new int[]{VL_PROT_ASCII}, "", null);
    	  
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
    	  bigloop: while(true) {
	    	  for (fileNum = 0; fileNum < fileName.length; fileNum++) {
	    		  VlPgmImage pim = new VlPgmImage();
	    		  VlSiftFilt      filt =null; 
	    		  int i;
	    		  boolean first;
	    		  ArrayList <double[]> ikeys = null;
	    		  int nikeys = 0;
	    		  int ikeys_size = 0;
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

    		      File namefile = new File(fileDir[fileNum] + File.separator + fileDir[fileNum]);
    				
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
    		      
    		      /* ...............................................................
    		       *                                     Optionally source keypoints
    		       * ............................................................ */

    		      if (ifr.active) {

    		          /* open file */
    		          ifrFile = vl_file_meta_open (ifr, basename, "r", error);
    		          if (error[0] > 0) {
    		        	  if (error[0] == VL_ERR_OVERFLOW) {
    		        		  System.err.println("Output file name too long");
    		        		  Preferences.debug("Output file name too long\n", Preferences.DEBUG_ALGORITHM);
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
		        		  System.err.println("Output file name too long");
		        		  Preferences.debug("Output file name too long\n", Preferences.DEBUG_ALGORITHM);
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
		        		  System.err.println("Output file name too long");
		        		  Preferences.debug("Output file name too long\n", Preferences.DEBUG_ALGORITHM);
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
		        		  System.err.println("Output file name too long");
		        		  Preferences.debug("Output file name too long\n", Preferences.DEBUG_ALGORITHM);
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
		        		  System.err.println("Output file name too long");
		        		  Preferences.debug("Output file name too long\n", Preferences.DEBUG_ALGORITHM);
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
    		        VlSiftKeypoint keys = null ;
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
    		          /*vl_sift_detect (filt) ;

    		          keys  = vl_sift_get_keypoints     (filt) ;
    		          nkeys = vl_sift_get_nkeypoints (filt) ;
    		          i     = 0 ;

    		          if (verbose) {
    		            printf ("sift: detected %d (unoriented) keypoints\n", nkeys) ;
    		          }*/
    		        } else {
    		          nkeys = nikeys ;
    		        }
    		      } // while (true)

	    	  } // for (fileNum = 0; fileNum < fileName.length; fileNum++)
    	      break bigloop;
    	  } // bigloop: while(true)
    	  
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
                                      "%", "\0",
                                      basename, num) ;

      if (num[0] >= self.name.length()) {
    	System.err.println("Overflow occurred in vl_string_replace_wildcard");
    	Preferences.debug("Overflow occurred in vl_string_replace_wildcard\n", Preferences.DEBUG_ALGORITHM);
    	error[0] = VL_ERR_OVERFLOW;
        return null;
      }

      if (self.active) {
    	  try {
		      outFile = new RandomAccessFile(self.name, mode);
		  } catch (IOException e) {
				System.err.println("IOException " + e + " on outFile = new RandomAccessFile(self.name, mode)");
			    Preferences.debug("IOException " + e + " on outFile = new RandomAccessFile(self.name, mode)\n",Preferences.DEBUG_ALGORITHM);
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

      VlSiftKeypoint keys ;/**< detected keypoints. */
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

      c = remove_blanks(f) ;
      good &= c > 0 ;

      width = readAsciiInt(f);

      c = remove_blanks(f) ;
      good &= c > 0 ;

      height = readAsciiInt(f);

      c = remove_blanks(f) ;
      good &= c > 0 ;

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
    remove_blanks(RandomAccessFile f)
    {
      int count = 0 ;
      int c = 0;
      long position = 0L;

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
     			System.exit(-1);	
          }
          try {
              f.seek(position-1);
          }
          catch (IOException e) {
      		System.err.println("IOException " + e + " on f.seek(position-1)");
   			Preferences.debug("IOException " + e + " on position = f.seek(poistion-1)\n",Preferences.DEBUG_FILEIO);
   			System.exit(-1);	
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
    
    private double readAsciiDouble(RandomAccessFile f) {
    	double value = 0.0; 
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
    		value = Double.parseDouble(num);
    	}
    	catch (NumberFormatException e) {
    		System.err.println("NumberFormatException " + e + " on Double.parseDouble(num)");
 			Preferences.debug("NumberFormatException " + e + " on Double.parseDouble(num)\n",Preferences.DEBUG_FILEIO);
 			System.exit(-1);		
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
    	   for (i = 0; i < data_size; i++) {
    		   c = remove_blanks(f) ;
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

      while ((c = source.substring(sIndex,sIndex+1)) != null) {
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
          while ((c = repl.substring(rIndex,rIndex+1)) != null) {
            if ((destination != null) && k + 1 < destinationSize) {
              if (k == 0) {
            	  destination = c + destination.substring(1);
              }
              else if (k == destination.length() -1) {
            	  destination = destination.substring(0,destination.length()-1) + c;
              }
              else {
            	  destination = destination.substring(0,k) + c + destination.substring(k+1);
              }
            }
            ++ k ;
          }
        }
        /* regular character */
        else {
          if ((destination != null) && k + 1 < destinationSize) {
        	  if (k == 0) {
            	  destination = c + destination.substring(1);
              }
              else if (k == destination.length() -1) {
            	  destination = destination.substring(0,destination.length()-1) + c;
              }
              else {
            	  destination = destination.substring(0,k) + c + destination.substring(k+1);
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
      int err ;
      int n ;
      double y ;
      boolean good = true;
      int c = 0;

      switch (protocol) {

      case VL_PROT_ASCII :
    	  c = remove_blanks(file) ;
		   good &= c > 0 ;
		   if (!good) {
			   break;
		   }
           x[0] = readAsciiDouble(file);
           break ;

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

        buffer = null;
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
        	return err;
        }
        
        if (verbose) {
            System.out.println("sift: saved gss level to " + fm.name) ;
            Preferences.debug("sift: saved gss level to " + fm.name + "\n", Preferences.DEBUG_ALGORITHM);
        }
      }

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
    	 Preferences.debug("IOEXcedption " + e + "\n", Preferences.DEBUG_ALGORITHM);
    	 return VL_ERR_PGM_IO;
      }
      try {
    	  f.write(data);
      }
      catch (IOException e) {
    	 System.err.println("IOException " + e);
    	 Preferences.debug("IOEXcedption " + e + "\n", Preferences.DEBUG_ALGORITHM);
    	 return VL_ERR_PGM_IO;
      }
      
      return 0 ;
    }
}
