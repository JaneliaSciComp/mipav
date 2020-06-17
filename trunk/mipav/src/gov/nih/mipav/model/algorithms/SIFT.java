package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

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
    
    private String fileDir;
    private String fileBaseName;
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
	
	/**
     * SIFT - default constructor.
     */
    public SIFT() { }
    
    public SIFT(String fileDir, String fileBaseName, boolean verbose, String outarg, String framesarg,
    		String descriptorarg, String metaarg, String read_framesarg, String gssarg, int O, int S,
    		int omin, double edge_thresh, double peak_thresh, double magnif, boolean force_orientations) {
        this.fileDir = fileDir;
        this.fileBaseName = fileBaseName;
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
      long q ;
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
}
