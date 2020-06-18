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
	    	      // Get basename form fileName[fileNum]
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
    		              double y, s, th ;

    		              /* read next guy */
    		              //err = vl_file_meta_get_double (ifr.protocol ifrFile, x) 
    		          } // while (true)
    		      } // if (ifr.active)
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
    	  
    	  vl_file_meta_close (out) ;
    	  vl_file_meta_close (frm) ;
    	  vl_file_meta_close (dsc) ;
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


}
