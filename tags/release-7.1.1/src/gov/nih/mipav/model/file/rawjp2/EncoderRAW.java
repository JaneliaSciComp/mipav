package gov.nih.mipav.model.file.rawjp2;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewJProgressBar;


import java.util.*;
import java.io.*;
import javax.swing.*;

import jj2000.j2k.*;
import jj2000.j2k.codestream.writer.*;
import jj2000.j2k.encoder.*;
import jj2000.j2k.entropy.encoder.*;
import jj2000.j2k.fileformat.writer.*;
import jj2000.j2k.image.*;
import jj2000.j2k.image.forwcomptransf.*;
import jj2000.j2k.image.input.*;
import jj2000.j2k.io.*;
import jj2000.j2k.quantization.quantizer.*;
import jj2000.j2k.roi.encoder.*;
import jj2000.j2k.util.*;
import jj2000.j2k.wavelet.analysis.*;

/**
 * This class is the main class of JJ2000's encoder. It instantiates all
 * objects of the chain and launchs the encoding process. It then writes the
 * header and the compressed bit stream to the output file. Finally,
 * packed packet headers (through codestream post-manipulation) and
 * file-format may be created if needed.
 *
 * <p>First the encoder should be initialized with a ParameterList object
 * provided through the constructor. Then, the run() method is invoked and the
 * encoder executes. The exit code of the class can be obtained with the
 * getExitCode() method, after the constructor and after the run method. A
 * non-zero value indicates that an error has occurred.</p>
 *
 * <p>The modules are inserted in the encoding chain with the following
 * order:</p>
 *
 * <ul>
 * <li>ImgReader</li>
 * <li>ImgDataJoiner (if multiple image readers)</li>
 * <li>Tiler</li>
 * <li>ForwCompTransf</li>
 * <li>ImgDataConverter</li>
 * <li>ForwardWT</li>
 * <li>Quantizer</li>
 * <li>ROIScaler</li>
 * <li>EntropyCoder</li>
 * <li>PostCompRateAllocator</li>
 * </ul>
 *
 * <p>The encoder uses a pull model. This means that the last module
 * (PostCompRateAllocator) requests data from its source (EntropyCoder),
 * ...</p>
 *
 * <p>Writing of the codestream writing (header+bit stream) is realized by
 * HeaderEncoder and CodestreamWriter modules.</p>
 *
 * <p>Packed packet headers and file-format creation are carried out by
 * CodestreamManipulator and FileFormatWriter modules respectively.</p>
 *
 * <p>Many modules of the encoder may behave differently depending on the
 * tile-component. The specifications of their behaviour are kept in
 * specialized modules extending ModuleSpec class. All these modules are
 * accessible through an instance of EncoderSpecs class.</p>
 *
 * @modifier Dzung Nguyen, Nam Nguyen
 *
 * @see ImgReader
 * @see ImgDataJoiner
 * @see ForwCompTransf
 * @see Tiler
 * @see ImgDataConverter
 * @see ForwardWT
 * @see Quantizer
 * @see ROIScaler
 * @see EntropyCoder
 * @see PostCompRateAllocator
 * @see HeaderEncoder
 * @see CodestreamWriter
 * @see CodestreamManipulator
 * @see FileFormatWriter
 * @see ModuleSpec
 * @see EncoderSpecs
 * */
public class EncoderRAW implements Runnable {

    /** The exit code of the run method */
    private int exitCode;

    /** The parameter list (arguments) */
    private ParameterList pl;

    /** The default parameter list (arguments) */
    private ParameterList defpl;

    private ModelImage image;

    /** The valid list of options prefixes */
    public final static char vprfxs[] = {
	ForwCompTransf.OPT_PREFIX, // Mixer module
	AnWTFilter.OPT_PREFIX, // Filters type spec
	ForwardWT.OPT_PREFIX, // Wavelets module
	Quantizer.OPT_PREFIX, // Quantizer module
	ROIScaler.OPT_PREFIX, // ROI module
        HeaderEncoder.OPT_PREFIX, // HeaderEncoder module
	EntropyCoder.OPT_PREFIX, // Coding modules
	PostCompRateAllocator.OPT_PREFIX, // Rate allocator
	PktEncoder.OPT_PREFIX, // Packet encoder
    };
    
    private int ncomp = 1;
    
    //private int ncomp = 3; //testing for color

    /** The parameter information for this class */
    private final static String[][] pinfo = {
        { "debug", null,
          "Print debugging messages when an error is encountered.","off"},
        { "disable_jp2_extension", "[on|off]",
          "JJ2000 automatically adds .jp2 extension when using 'file_format'"+
          "option. This option disables it when on.", "off"},
        { "file_format", "[on|off]",
          "Puts the JPEG 2000 codestream in a JP2 file format wrapper.","off"},
        { "pph_tile", "[on|off]",
          "Packs the packet headers in the tile headers.","off"},
        { "pph_main", "[on|off]",
          "Packs the packet headers in the main header.","off"},
        { "pfile", "<filename of arguments file>",
          "Loads the arguments from the specified file. Arguments that are "+
          "specified on the command line override the ones from the file.\n"+
          "The arguments file is a simple text file with one argument per "+
          "line of the following form:\n" +
          "  <argument name>=<argument value>\n"+
          "If the argument is of boolean type (i.e. its presence turns a "+
          "feature on), then the 'on' value turns it on, while the 'off' "+
          "value turns it off. The argument name does not include the '-' "+
          "or '+' character. Long lines can be broken into several lines "+
          "by terminating them with '\'. Lines starting with '#' are "+
          "considered as comments. This option is not recursive: any 'pfile' "+
          "argument appearing in the file is ignored.",null},
        { "tile_parts", "<packets per tile-part>",
          "This option specifies the maximum number of packets to have in "+
          "one tile-part. 0 means include all packets in first tile-part "+
          "of each tile","0"},
        { "tiles", "<nominal tile width> <nominal tile height>",
          "This option specifies the maximum tile dimensions to use. "+
          "If both dimensions are 0 then no tiling is used.","0 0"},
        { "ref", "<x> <y>",
          "Sets the origin of the image in the canvas system. It sets the "+
          "coordinate of the top-left corner of the image reference grid, "+
          "with respect to the canvas origin","0 0"},
        { "tref", "<x> <y>",
          "Sets the origin of the tile partitioning on the reference grid, "+
          "with respect to the canvas origin. The value of 'x' ('y') "+
          "specified can not be larger than the 'x' one specified in the ref "+
          "option.","0 0"},
        { "rate", "<output bitrate in bpp>",
          "This is the output bitrate of the codestream in bits per pixel."+
          " When equal to -1, no image information (beside quantization "+
          "effects) is discarded during compression.\n"+
          "Note: In the case where '-file_format' option is used, the "+
          "resulting file may have a larger bitrate.","-1"},
        { "lossless", "[on|off]", 
          "Specifies a lossless compression for the encoder. This options"+
          " is equivalent to use reversible quantization ('-Qtype "+
          "reversible') and 5x3 wavelet filters pair ('-Ffilters w5x3'). Note that "+
          "this option cannot be used with '-rate'. When this option is "+
          "off, the quantization type and the filters pair is defined by "+
          "'-Qtype' and '-Ffilters' respectively.","off"},
        { "i", "<image file> [,<image file> [,<image file> ... ]]",
          "Mandatory argument. This option specifies the name of the input "+
          "image files. If several image files are provided, they have to be"+
          " separated by commas in the command line. Supported formats are "+
          "PGM (raw), PPM (raw) and PGX, "+
          "which is a simple extension of the PGM file format for single "+
          "component data supporting arbitrary bitdepths. If the extension "+
          "is '.pgm', PGM-raw file format is assumed, if the extension is "+
          "'.ppm', PPM-raw file format is assumed, otherwise PGX file "+
          "format is assumed. PGM and PPM files are assumed to be 8 bits "+
          "deep. A multi-component image can be specified by either "+
          "specifying several PPM and/or PGX files, or by specifying one "+
          "PPM file.",null},
        { "o", "<file name>",
          "Mandatory argument. This option specifies the name of the output "+
          "file to which the codestream will be written.",null},
        { "verbose", null,
          "Prints information about the obtained bit stream.","on"},
        { "v", "[on|off]",
          "Prints version and copyright information.","off"},
        { "u", "[on|off]",
          "Prints usage information. "+
          "If specified all other arguments (except 'v') are ignored","off"},
    };

    /**
     * Instantiates an encoder object, width the ParameterList object given as
     * argument. It also retrieves the default ParameterList.
     *
     * @param pl The ParameterList for this decoder (contains also defaults
     * values);
     * */
    public EncoderRAW(ParameterList pl) {
	this.pl = pl;
	defpl = pl.getDefaultParameterList();
    }

    public EncoderRAW(ParameterList pl, ModelImage image) {
    	this.pl = pl;
    	defpl = pl.getDefaultParameterList();
    	this.image = image;
    }

    /**
     * Returns the exit code of the class. This is only initialized after the
     * constructor and when the run method returns.
     *
     * @return The exit code of the constructor and the run() method.
     * */
    public int getExitCode() {
        return exitCode;
    }

    /**
	     * Runs the encoder. After completion the exit code is set, a non-zero
	     * value indicates that an error ocurred.
	     *
	     * @see #getExitCode
	     * */
	    public void run() {
	    	System.out.println("IN  RUN()");
	        boolean verbose;
	        boolean useFileFormat = false;
	        boolean pphTile = false;
	        boolean pphMain = false;
	        boolean tempSop = false;
	        boolean tempEph = false;
	        ImgReader imreader[];
	        String inext,infile;
	        StreamTokenizer stok;
	        StringTokenizer sgtok;
	        int ncomp;
	        boolean ppminput;
	        Vector imreadervec;
	        boolean imsigned[];
	        BlkImgDataSrc imgsrc;
	        int i;
	        int tw,th;
	        int refx,refy;
	        int trefx,trefy;
	        int pktspertp;
	        Tiler imgtiler;
	        ForwCompTransf fctransf;
		ImgDataConverter converter;
	        EncoderSpecs encSpec;
	        ForwardWT dwt;
	        Quantizer quant;
	        ROIScaler rois;
	        EntropyCoder ecoder;
	        PostCompRateAllocator ralloc;
	        HeaderEncoder headenc;
	        CodestreamWriter bwriter;
	        FileFormatWriter ffw;
	        String outname;
	        float rate;
	        int fileLength;
	
	        try {
	
	            // **** Usage and version ****
	            try {
	                // Do we print version information?
	                if (pl.getBooleanParameter("v")) {
	                    printVersionAndCopyright();
	                }
	                // Do we print usage information?
	                if (pl.getParameter("u").equals("on")) {
	                    printUsage();
	                    return; // When printing usage => exit
	                }
	                // Do we print info ?
	                verbose = pl.getBooleanParameter("verbose");
	            } catch (StringFormatException e) {
	                error("An error occured while parsing the arguments:\n"+
	                      e.getMessage(),1);
	                if(pl.getParameter("debug").equals("on")) {
	                    e.printStackTrace();
	                } else {
	                    error("Use '-debug' option for more details",2);
	                }
	                return;
	            } catch (NumberFormatException e) {
	                error("An error occured while parsing the arguments:\n"+
	                      e.getMessage(),1);
	                if(pl.getParameter("debug").equals("on")) {
	                    e.printStackTrace();
	                } else {
	                    error("Use '-debug' option for more details",2);
	                }
	                return;
	            }
	
	            // **** Get general parameters ****
	            
	            // Check that we have the mandatory parameters
	            if (pl.getParameter("i") == null) {
	                error("Mandatory input file is missing (-i option)",2);
	                return;
	            }
	
	            if (pl.getParameter("o") == null) {
	                error("Mandatory output file is missing (-o option)",2);
	                return;
	            }
	            outname = pl.getParameter("o");
	
	            if (pl.getParameter("file_format").equals("on")) {
	                useFileFormat = true;
	                if(pl.getParameter("rate")!=null && 
	                   pl.getFloatParameter("rate")!=
	                   defpl.getFloatParameter("rate")) {
	                    warning("Specified bit-rate applies only on the "+
	                            "codestream but not on the whole file.");
	                }
	            }
	
	            if(useFileFormat) {
	                String outext = null;
	                String outns = outname;
	                if(outname.lastIndexOf('.')!=-1) {
	                    outext = outname.substring(outname.lastIndexOf('.'),
	                                               outname.length());
	                    outns = outname.substring(0,outname.lastIndexOf('.'));
	                }
	
	                if(outext==null || !outext.equalsIgnoreCase(".jp2")) {
	                    if(!pl.getBooleanParameter("disable_jp2_extension")) {
	                        FacilityManager.getMsgLogger().
	                            printmsg(MsgLogger.INFO,"JPEG 2000 file names"+
	                                     " end with .jp2 extension when using"+
	                                     " the file format of part 1. This "+
	                                     "extension is automatically"+
	                                     " added by JJ2000. Use "+
	                                     "'-disable_jp2_extension' to "+
	                                     "disable it.");
	                        
	                        outname = outns+".jp2";
	                    }
	                }
	            }
	
	            if (pl.getParameter("tiles") == null) {
	                error("No tiles option specified",2);
	                return;
	            }
	
	            if (pl.getParameter("pph_tile").equals("on")){
	                pphTile = true;
	                
	                if(pl.getParameter("Psop").equals("off")){
	                    pl.put("Psop","on");
	                    tempSop = true;
	                }
	                if(pl.getParameter("Peph").equals("off")){
	                    pl.put("Peph","on");
	                    tempEph = true;
	                }                  
	            }
	
	            if (pl.getParameter("pph_main").equals("on")){
	                pphMain = true;
	                
	                if(pl.getParameter("Psop").equals("off")){
	                    pl.put("Psop","on");
	                    tempSop = true;
	                }
	                if(pl.getParameter("Peph").equals("off")){
	                    pl.put("Peph","on");
	                    tempEph = true;
	                }                  
	            }
	
	            if(pphTile && pphMain)
	                error("Can't have packed packet headers in both main and"+
	                      " tile headers",2);
	
	            if(pl.getBooleanParameter("lossless") && 
	               pl.getParameter("rate")!=null && 
	               pl.getFloatParameter("rate")!=defpl.getFloatParameter("rate"))
	                throw new IllegalArgumentException("Cannot use '-rate' and "+
	                                                   "'-lossless' option at "+
	                                                   " the same time.");
	
	            if (pl.getParameter("rate") == null) {
	                error("Target bitrate not specified",2);
	                return;
	            }
	            try {
	                rate = pl.getFloatParameter("rate");
	                if(rate==-1) {
	                    rate = Float.MAX_VALUE;
	                }
	            } catch (NumberFormatException e) {
	                error("Invalid value in 'rate' option: "+
	                      pl.getParameter("rate"),2);
	                if(pl.getParameter("debug").equals("on")) {
	                    e.printStackTrace();
	                } else {
	                    error("Use '-debug' option for more details",2);
	                }
	                return;
	            }
	            try {
	                pktspertp = pl.getIntParameter("tile_parts");
	                if(pktspertp != 0){
	                    if(pl.getParameter("Psop").equals("off")){
	                        pl.put("Psop","on");
	                        tempSop = true;
	                    }
	                    if(pl.getParameter("Peph").equals("off")){
	                        pl.put("Peph","on");
	                        tempEph = true;
	                    }   
	                }               
	            } catch (NumberFormatException e) {
	                error("Invalid value in 'tile_parts' option: "+
	                      pl.getParameter("tile_parts"),2);
	                if(pl.getParameter("debug").equals("on")) {
	                    e.printStackTrace();
	                } else {
	                    error("Use '-debug' option for more details",2);
	                }
	                return;
	            }
	
	            // **** ImgReader ****
	            sgtok = new StringTokenizer(pl.getParameter("i"),",");
	            ncomp = 0;
	            ppminput = false;
	            imreadervec = new Vector();
	            int nTokens = sgtok.countTokens();
	            
	            for(int  n=0; n<nTokens; n++){
	                infile = sgtok.nextToken();
	                try {
	                    if (imreadervec.size() < ncomp) {
	                        error("With PPM input format only 1 input file can "+
	                              "be specified",2);
	                        return;
	                    }
	                    if (infile.lastIndexOf('.') != -1) {
	                        inext = infile.substring(infile.lastIndexOf('.'),
	                                                 infile.length());
	                    } else {
	                        inext = null;
	                    }
	                    if (".PGM".equalsIgnoreCase(inext)) { // PGM file
	                        imreadervec.addElement(new ImgReaderPGM(infile));
	                        ncomp += 1;
	                    } else if (".PPM".equalsIgnoreCase(inext)) { // PPM file
	                        if (ncomp > 0) {
	                            error("With PPM input format only 1 input "+
	                                  "file can be specified",2);
	                            return;
	                        }
	                        imreadervec.addElement(new ImgReaderPPM(infile));
	                        ppminput = true;
	                        ncomp += 3;
	                    } else if (".XML".equalsIgnoreCase(inext)) { // ADDITION: RAW file processing
	//                        imreadervec.addElement(new ImgReaderRAW(infile));
	//                        ncomp+=124;
	                      imreadervec.addElement(new ImgReaderRAW(infile));
	                      ncomp+=3;
	                    	
	                    } else { // Should be PGX
	                        imreadervec.addElement(new ImgReaderPGX(infile));
	                        ncomp+=1;
	                    }
	                } catch (IOException e) {
	                    error("Could not open or read from file "+infile +
	                          ((e.getMessage() != null) ?
	                           (":\n"+e.getMessage()) : ""),3);
	                    if(pl.getParameter("debug").equals("on")) {
	                        e.printStackTrace();
	                    } else {
	                        error("Use '-debug' option for more details",2);
	                    }
	                    return;
	                }
	                finally {
	                    if (exitCode != 0) {
	                        // Close the other files
	                        while (imreadervec.size() > 0) {
	                            try {
	                                ((ImgReader)imreadervec.
	                                 elementAt(imreadervec.size()-1)).close();
	                                imreadervec.
	                                    removeElementAt(imreadervec.size()-1);
	                            } catch (Exception e) { }
	                        }
	                    }
	                }
	            }
	            imreader = new ImgReader[imreadervec.size()];
	            imreadervec.copyInto(imreader);
	            imreadervec.removeAllElements();
	            imreadervec = null;
	            imsigned = new boolean[ncomp];
	
	/*            
	            // **** ImgDataJoiner (if needed) ****
	            if (ppminput || ncomp == 1) { // Just one input
	                imgsrc = imreader[0];
	                for (i=0; i<ncomp; i++) {
	                    imsigned[i] = imreader[0].isOrigSigned(i);
	                }
	            } else { // More than one reader => join all readers into 1
	                imgcmpidxs = new int[ncomp];
	                for (i=0; i<ncomp; i++) {
	                    imsigned[i] = imreader[i].isOrigSigned(0);
	                }
	                imgsrc = new ImgDataJoiner(imreader,imgcmpidxs);
	            }
	*/
	//--- Only 1 RAW image
	            imgsrc = imreader[0];
	            for (i=0; i<ncomp; i++) {
	                imsigned[i] = imreader[0].isOrigSigned(i);
	            }
	//---            
	            // **** Tiler ****
	            // get nominal tile dimensions
	            stok =
	                new StreamTokenizer(new StringReader(pl.
	                                                     getParameter("tiles")));
	            stok.eolIsSignificant(false);
	
	            stok.nextToken();
	            if (stok.ttype != StreamTokenizer.TT_NUMBER) {
	                error("An error occurred while parsing the tiles option: "+
	                      pl.getParameter("tiles"),2);
	                return;
	            }
	            tw = (int) stok.nval;
	            stok.nextToken();
	            if (stok.ttype != StreamTokenizer.TT_NUMBER) {
	                error("An error occurred while parsing the tiles option: "+
	                      pl.getParameter("tiles"),2);
	                return;
	            }
	            th = (int) stok.nval;
	
	            // Get image reference point
	            sgtok = new StringTokenizer(pl.getParameter("ref"));
	            try {
	                refx = Integer.parseInt(sgtok.nextToken());
	                refy = Integer.parseInt(sgtok.nextToken());
	            } catch (NoSuchElementException e) {
	                throw
	                    new IllegalArgumentException("Error while parsing 'ref' "+
	                                                 "option");
	            } catch (NumberFormatException e) {
	                throw new IllegalArgumentException("Invalid number type in "+
	                                                   "'ref' option");
	            }
	            if (refx < 0 || refy < 0) {
	                throw new IllegalArgumentException("Invalid value in 'ref' "+
	                                                   "option ");
	            }
	
	            // Get tiling reference point
	            sgtok = new StringTokenizer(pl.getParameter("tref"));
	            try {
	                trefx = Integer.parseInt(sgtok.nextToken());
	                trefy = Integer.parseInt(sgtok.nextToken());
	            } catch (NoSuchElementException e) {
	                throw
	                    new IllegalArgumentException("Error while parsing 'tref' "+
	                                                 "option");
	            } catch (NumberFormatException e) {
	                throw new IllegalArgumentException("Invalid number type in "+
	                                                   "'tref' option");
	            }
	            if (trefx < 0 || trefy < 0 || trefx > refx || trefy > refy) {
	                throw new IllegalArgumentException("Invalid value in 'tref' "+
	                                                   "option ");
	            }
	            
	            // Instantiate tiler
	            try {
	                imgtiler = new Tiler(imgsrc,refx,refy,trefx,trefy,tw,th);
	            }  catch (IllegalArgumentException e) {
	                error("Could not tile image"+
	                      ((e.getMessage() != null) ?
	                       (":\n"+e.getMessage()) : ""),2);
	                if(pl.getParameter("debug").equals("on")) {
	                    e.printStackTrace();
	                } else {
	                    error("Use '-debug' option for more details",2);
	                }
	                return;
	            }
		    int ntiles = imgtiler.getNumTiles();
	
	    
		    // **** EncoderRAW specifications ****
	            encSpec = new EncoderSpecs(ntiles, ncomp, imgsrc, pl);
	
	         // /* NO COLOR TRANSFORM NEEDED, switched off by setting parameter "Mct"	            
	            // **** Component transformation ****
	            if (ppminput && pl.getParameter("Mct") != null &&
	                pl.getParameter("Mct").equals("off")) {
	                FacilityManager.getMsgLogger().
	                    printmsg(MsgLogger.WARNING,
	                             "Input image is RGB and no color transform has "+
	                             "been specified. Compression performance and "+
	                             "image quality might be greatly degraded. Use "+
	                             "the 'Mct' option to specify a color transform");
	            }
	            try {
	                fctransf = new ForwCompTransf(imgtiler,encSpec);
	            } catch (IllegalArgumentException e) {
	                error("Could not instantiate forward component "+
	                      "transformation"+
	                          ((e.getMessage() != null) ?
	                           (":\n"+e.getMessage()) : ""),2);
	                if(pl.getParameter("debug").equals("on")) {
	                    e.printStackTrace();
	                } else {
	                    error("Use '-debug' option for more details",2);
	                }
	                return;
	            }
	//*/
	
	            // **** ImgDataConverter ****
		    converter = new ImgDataConverter(fctransf);
	        //    converter = new ImgDataConverter(imgsrc);
	
	            // **** ForwardWT ****
	            try {
	                dwt = ForwardWT.createInstance(converter,pl,encSpec);
	            } catch (IllegalArgumentException e) {
	                error("Could not instantiate wavelet transform"+
	                      ((e.getMessage() != null) ?
	                       (":\n"+e.getMessage()) : ""),2);
	                if(pl.getParameter("debug").equals("on")) {
	                    e.printStackTrace();
	                } else {
	                    error("Use '-debug' option for more details",2);
	                }
	                return;
	            }
	
	            // **** Quantizer ****
	            try{
	                quant = Quantizer.createInstance(dwt,encSpec);
	            } catch(IllegalArgumentException e) {
	                error("Could not instantiate quantizer"+
	                      ((e.getMessage() != null) ?
	                       (":\n"+e.getMessage()) : ""),2);
	                if(pl.getParameter("debug").equals("on")) {
	                    e.printStackTrace();
	                } else {
	                    error("Use '-debug' option for more details",2);
	                }
	                return;
	            }
	
	            // **** ROIScaler ****
	            try{
	                rois = ROIScaler.createInstance(quant,pl,encSpec);
	            } catch (IllegalArgumentException e) {
	                error("Could not instantiate ROI scaler"+
	                      ((e.getMessage() != null) ?
	                       (":\n"+e.getMessage()) : ""),2);
	                if(pl.getParameter("debug").equals("on")) {
	                    e.printStackTrace();
	                } else {
	                    error("Use '-debug' option for more details",2);
	                }
	                return;
	            }
	
	            // **** EntropyCoder ****
	            try {
	                ecoder = EntropyCoder.createInstance(rois,pl,encSpec.cblks,
	                                                     encSpec.pss,encSpec.bms,
	                                                     encSpec.mqrs,encSpec.rts,
	                                                     encSpec.css,encSpec.sss,
	                                                     encSpec.lcs,encSpec.tts);
	            } catch (IllegalArgumentException e) {
	                error("Could not instantiate entropy coder"+
	                      ((e.getMessage() != null) ?
	                       (":\n"+e.getMessage()) : ""),2);
	                if(pl.getParameter("debug").equals("on")) {
	                    e.printStackTrace();
	                } else {
	                    error("Use '-debug' option for more details",2);
	                }
	                return;
	            }
	
	            // **** CodestreamWriter ****
	            try {
	                // Rely on rate allocator to limit amount of data
	                bwriter = new FileCodestreamWriter(outname,Integer.MAX_VALUE);
	            } catch (IOException e) {
	                error("Could not open output file"+
	                      ((e.getMessage() != null) ?
	                       (":\n"+e.getMessage()) : ""),2);
	                if(pl.getParameter("debug").equals("on")) {
	                    e.printStackTrace();
	                } else {
	                    error("Use '-debug' option for more details",2);
	                }
	                return;
	            }
	
	            // **** Rate allocator ****
	            try {
	                ralloc = PostCompRateAllocator.createInstance(ecoder,pl,rate,
	                                                              bwriter,encSpec);
	            } catch (IllegalArgumentException e) {
	                error("Could not instantiate rate allocator"+
	                      ((e.getMessage() != null) ?
	                       (":\n"+e.getMessage()) : ""),2);
	                if(pl.getParameter("debug").equals("on")) {
	                    e.printStackTrace();
	                } else {
	                    error("Use '-debug' option for more details",2);
	                }
	                System.out.println("HERE");
	                e.printStackTrace();
	                return;
	            }
	
	            // **** HeaderEncoder ****
	            headenc = new HeaderEncoder(imgsrc,imsigned,dwt,imgtiler,encSpec,
	                                        rois,ralloc,pl);
	            ralloc.setHeaderEncoder(headenc);
	
	            // **** Write header to be able to estimate header overhead ****
	            headenc.encodeMainHeader();
	
	            // **** Initialize rate allocator, with proper header
	            // overhead. This will also encode all the data ****
	            ralloc.initialize();
	
	            // **** Write header (final) ****
	            headenc.reset();
	            headenc.encodeMainHeader();
	
	            // Insert header into the codestream
	            bwriter.commitBitstreamHeader(headenc);
	
	            // **** Report info ****
	            if (verbose) {
	                // Print target rate info
	                if(pl.getFloatParameter("rate")!=-1) {
	                    FacilityManager.getMsgLogger().
	                        println("Target bitrate = "+rate+" bpp (i.e. "+
	                                (int)(rate*imgsrc.getImgWidth()
	                                      *imgsrc.getImgHeight()/8)+ " bytes)",
	                                4,6);
	                }
	            }
	
	            // **** Now do the rate-allocation and write result ****
	            ralloc.runAndWrite();
	
	            // **** Done ****
	            bwriter.close();
	
	            // **** Calculate file length ****
	            fileLength = bwriter.getLength();
	
	            // **** Tile-parts and packed packet headers ****
	            if(pktspertp>0 || pphTile || pphMain) {
	            
	                try {
	                    CodestreamManipulator cm = new 
	                        CodestreamManipulator(outname,ntiles,pktspertp,
	                                              pphMain,pphTile,tempSop,tempEph);
	                    fileLength += cm.doCodestreamManipulation();
	                
	                    if(pktspertp>0) {
	                        FacilityManager.
	                            getMsgLogger().println("Created tile-parts "+
	                                                   "containing at most "+
	                                                   pktspertp+
	                                                   " packets per tile.",4,6);
	                    }
	                    if(pphTile) {
	                        FacilityManager.getMsgLogger().
	                            println("Moved packet headers "+
	                                    "to tile headers",4,6);
	                    }
	                    if(pphMain) {
	                        FacilityManager.getMsgLogger().
	                            println("Moved packet headers "+
	                                    "to main header",4,6);
	                    }
	                } catch(IOException e) {
	                    error("Error while creating tileparts or packed packet"+
	                          " headers"+
	                          ((e.getMessage() != null) ?
	                           (":\n"+e.getMessage()) : ""),2);
	                    if(pl.getParameter("debug").equals("on")) {
	                        e.printStackTrace();
	                    } else {
	                        error("Use '-debug' option for more details",2);
	                    }
	                    return;
	                }
	            }
	                
	            // **** File Format ****
	            if(useFileFormat) {
	                try{
	                    int nc= imgsrc.getNumComps() ;
	                    int[] bpc=new int[nc];
	                    for(int comp = 0; comp<nc; comp++) {
	                        bpc[comp]=imgsrc.getNomRangeBits(comp);
	                    }
	                    
	                    ffw = new FileFormatWriter(outname,imgsrc.getImgHeight(),
	                                               imgsrc.getImgWidth(),nc,bpc,
	                                               fileLength);
	                    fileLength += ffw.writeFileFormat();
	                } catch(IOException e) {
	                    throw new Error("Error while writing JP2 file format");
	                }
	            }
	            // **** Report results ****
	            if (verbose) {
	                // Print achieved rate
	                FacilityManager.getMsgLogger().
	                    println("Achieved bitrate = "+ 
	                            (8f*fileLength/
	                             (imgsrc.getImgWidth()*imgsrc.getImgHeight())) +
	                            " bpp (i.e. "+fileLength+" bytes)",4,6);
	
	                // Display ROI information if needed
	                if(pl.getParameter("Rroi")!=null && !useFileFormat && 
	                   pl.getIntParameter("tile_parts")==0 ) {
	                    int roiLen = bwriter.getOffLastROIPkt();
	                    FacilityManager.getMsgLogger().
	                        printmsg(MsgLogger.INFO,"The Region Of Interest is"+
	                                 " encoded in the first "+
	                                 roiLen+" bytes of the codestream (i.e "+
	                                 (8f*roiLen/(imgsrc.getImgWidth()*
	                                             imgsrc.getImgHeight()))+" bpp)");
	                    
	                }
			FacilityManager.getMsgLogger().flush();
	            }
	
	            // **** Close image reader(s) ***
	            for(i=0; i<imreader.length; i++) {
	                imreader[i].close();
	            }
	        } catch(IllegalArgumentException e) {
		    error(e.getMessage(),2);
	            if(pl.getParameter("debug").equals("on")) {
	                e.printStackTrace();
	            }
		    return;
		} catch (Error e) {
	            error("An uncaught error has occurred: "+e.getMessage(),2);
	            if(pl.getParameter("debug").equals("on")) {
	                e.printStackTrace();
	            } else {
	                error("Use '-debug' option for more details",2);
	            }
	        } catch (RuntimeException e) {
	            error("An uncaught runtime exception has occurred: "+
	                  e.getMessage(),2);
	            if(pl.getParameter("debug").equals("on")) {
	                e.printStackTrace();
	            } else {
	                error("Use '-debug' option for more details",2);
	            }
	        } catch (Throwable e) {
	            error("An unchecked exception has occurred: "+
	                  e.getMessage(),2);
	            if(pl.getParameter("debug").equals("on")) {
	                e.printStackTrace();
	            } else {
	                error("Use '-debug' option for more details",2);
	            }
	            return;
	        }
	    }

	/**
     * Runs the encoder. After completion the exit code is set, a non-zero
     * value indicates that an error ocurred.
     *
     * @see #getExitCode
     * */
    public ByteArrayOutputStream run1Slice(ImgReaderRAWSlice img) {
        boolean verbose;
        boolean useFileFormat = false;
        boolean pphTile = false;
        boolean pphMain = false;
        boolean tempSop = false;
        boolean tempEph = false;
        ImgReader imreader[];
//        String inext,infile;
        StreamTokenizer stok;
        StringTokenizer sgtok;

 //       boolean ppminput;
 //       Vector imreadervec;
        boolean imsigned[];
        BlkImgDataSrc imgsrc;
        int i;
//        int imgcmpidxs[];
        int tw,th;
        int refx,refy;
        int trefx,trefy;
        int pktspertp;
        Tiler imgtiler;
//        BlkImgDataSrc cursrc;
        ForwCompTransf fctransf;
	ImgDataConverter converter;
        EncoderSpecs encSpec;
        ForwardWT dwt;
        Quantizer quant;
        ROIScaler rois;
        EntropyCoder ecoder;
        PostCompRateAllocator ralloc;
        HeaderEncoder headenc;
        CodestreamWriter bwriter;
        String outname;
        ByteArrayOutputStream outstream; 
        BEByteArrayOutputStream newoutstream = new BEByteArrayOutputStream();
        float rate;
        int fileLength;

        try {

            // **** Usage and version ****
            try {
                // Do we print version information?
                if (pl.getBooleanParameter("v")) {
                    printVersionAndCopyright();
                }
                // Do we print usage information?
                if (pl.getParameter("u").equals("on")) {
                    printUsage();
                    return null; // When printing usage => exit
                }
                // Do we print info ?
                verbose = pl.getBooleanParameter("verbose");
            } catch (StringFormatException e) {
                error("An error occured while parsing the arguments:\n"+
                      e.getMessage(),1);
                if(pl.getParameter("debug").equals("on")) {
                    e.printStackTrace();
                } else {
                    error("Use '-debug' option for more details",2);
                }
                return null;
            } catch (NumberFormatException e) {
                error("An error occured while parsing the arguments:\n"+
                      e.getMessage(),1);
                if(pl.getParameter("debug").equals("on")) {
                    e.printStackTrace();
                } else {
                    error("Use '-debug' option for more details",2);
                }
                return null;
            }

            // **** Get general parameters ****
            
            // Check that we have the mandatory parameters
            if (pl.getParameter("i") == null) {
                error("Mandatory input file is missing (-i option)",2);
                return null;
            }

            if (pl.getParameter("o") == null) {
                error("Mandatory output file is missing (-o option)",2);
                return null;
            }
            outname = pl.getParameter("o");

            if (pl.getParameter("file_format").equals("on")) {
                useFileFormat = true;
                if(pl.getParameter("rate")!=null && 
                   pl.getFloatParameter("rate")!=
                   defpl.getFloatParameter("rate")) {
                    warning("Specified bit-rate applies only on the "+
                            "codestream but not on the whole file.");
                }
            }

            if(useFileFormat) {
                String outext = null;
                String outns = outname;
                if(outname.lastIndexOf('.')!=-1) {
                    outext = outname.substring(outname.lastIndexOf('.'),
                                               outname.length());
                    outns = outname.substring(0,outname.lastIndexOf('.'));
                }

                if(outext==null || !outext.equalsIgnoreCase(".jp2")) {
                    if(!pl.getBooleanParameter("disable_jp2_extension")) {
                        FacilityManager.getMsgLogger().
                            printmsg(MsgLogger.INFO,"JPEG 2000 file names"+
                                     " end with .jp2 extension when using"+
                                     " the file format of part 1. This "+
                                     "extension is automatically"+
                                     " added by JJ2000. Use "+
                                     "'-disable_jp2_extension' to "+
                                     "disable it.");
                        
                        outname = outns+".jp2";
                    }
                }
            }

            if (pl.getParameter("tiles") == null) {
                error("No tiles option specified",2);
                return null;
            }

            if (pl.getParameter("pph_tile").equals("on")){
                pphTile = true;
                
                if(pl.getParameter("Psop").equals("off")){
                    pl.put("Psop","on");
                    tempSop = true;
                }
                if(pl.getParameter("Peph").equals("off")){
                    pl.put("Peph","on");
                    tempEph = true;
                }                  
            }

            if (pl.getParameter("pph_main").equals("on")){
                pphMain = true;
                
                if(pl.getParameter("Psop").equals("off")){
                    pl.put("Psop","on");
                    tempSop = true;
                }
                if(pl.getParameter("Peph").equals("off")){
                    pl.put("Peph","on");
                    tempEph = true;
                }                  
            }

            if(pphTile && pphMain)
                error("Can't have packed packet headers in both main and"+
                      " tile headers",2);

            if(pl.getBooleanParameter("lossless") && 
               pl.getParameter("rate")!=null && 
               pl.getFloatParameter("rate")!=defpl.getFloatParameter("rate"))
                throw new IllegalArgumentException("Cannot use '-rate' and "+
                                                   "'-lossless' option at "+
                                                   " the same time.");

            if (pl.getParameter("rate") == null) {
                error("Target bitrate not specified",2);
                return null;
            }
            try {
                rate = pl.getFloatParameter("rate");
                if(rate==-1) {
                    rate = Float.MAX_VALUE;
                }
            } catch (NumberFormatException e) {
                error("Invalid value in 'rate' option: "+
                      pl.getParameter("rate"),2);
                if(pl.getParameter("debug").equals("on")) {
                    e.printStackTrace();
                } else {
                    error("Use '-debug' option for more details",2);
                }
                return null;
            }
            try {
                pktspertp = pl.getIntParameter("tile_parts");
                if(pktspertp != 0){
                    if(pl.getParameter("Psop").equals("off")){
                        pl.put("Psop","on");
                        tempSop = true;
                    }
                    if(pl.getParameter("Peph").equals("off")){
                        pl.put("Peph","on");
                        tempEph = true;
                    }   
                }               
            } catch (NumberFormatException e) {
                error("Invalid value in 'tile_parts' option: "+
                      pl.getParameter("tile_parts"),2);
                if(pl.getParameter("debug").equals("on")) {
                    e.printStackTrace();
                } else {
                    error("Use '-debug' option for more details",2);
                }
                return null;
            }

            
            imsigned = new boolean[ncomp];
            imreader = new ImgReader[1];
            imreader[0] = img;
            
//--- Only 1 RAW slice
            imgsrc = imreader[0];
            //for (i=0; i<ncomp; i++) {
               // imsigned[i] = imreader[0].isOrigSigned(i);
            //}
            imsigned[0] = imreader[0].isOrigSigned(0);

//---            
            // **** Tiler ****
            // get nominal tile dimensions
            stok = new StreamTokenizer(new StringReader(pl.getParameter("tiles")));
            stok.eolIsSignificant(false);

            stok.nextToken();
            if (stok.ttype != StreamTokenizer.TT_NUMBER) {
                error("An error occurred while parsing the tiles option: "+
                      pl.getParameter("tiles"),2);
                return null;
            }
            tw = (int) stok.nval;
            stok.nextToken();
            if (stok.ttype != StreamTokenizer.TT_NUMBER) {
                error("An error occurred while parsing the tiles option: "+
                      pl.getParameter("tiles"),2);
                return null;
            }
            th = (int) stok.nval;

            // Get image reference point
            sgtok = new StringTokenizer(pl.getParameter("ref"));
            try {
                refx = Integer.parseInt(sgtok.nextToken());
                refy = Integer.parseInt(sgtok.nextToken());
            } catch (NoSuchElementException e) {
                throw
                    new IllegalArgumentException("Error while parsing 'ref' "+
                                                 "option");
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException("Invalid number type in "+
                                                   "'ref' option");
            }
            if (refx < 0 || refy < 0) {
                throw new IllegalArgumentException("Invalid value in 'ref' "+
                                                   "option ");
            }

            // Get tiling reference point
            sgtok = new StringTokenizer(pl.getParameter("tref"));
            try {
                trefx = Integer.parseInt(sgtok.nextToken());
                trefy = Integer.parseInt(sgtok.nextToken());
            } catch (NoSuchElementException e) {
                throw
                    new IllegalArgumentException("Error while parsing 'tref' "+
                                                 "option");
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException("Invalid number type in "+
                                                   "'tref' option");
            }
            if (trefx < 0 || trefy < 0 || trefx > refx || trefy > refy) {
                throw new IllegalArgumentException("Invalid value in 'tref' "+
                                                   "option ");
            }
            
            // Instantiate tiler
            try {
                imgtiler = new Tiler(imgsrc,refx,refy,trefx,trefy,tw,th);
            }  catch (IllegalArgumentException e) {
                error("Could not tile image"+
                      ((e.getMessage() != null) ?
                       (":\n"+e.getMessage()) : ""),2);
                if(pl.getParameter("debug").equals("on")) {
                    e.printStackTrace();
                } else {
                    error("Use '-debug' option for more details",2);
                }
                return null;
            }
	    int ntiles = imgtiler.getNumTiles();


    
	    // **** EncoderRAW specifications ****
            encSpec = new EncoderSpecs(ntiles, ncomp, imgsrc, pl);

          /* NO COLOR TRANSFORM NEEDED, switched off by setting parameter "Mct"	            
            ppminput = false;
            // **** Component transformation ****
            if (ppminput && pl.getParameter("Mct") != null &&
                pl.getParameter("Mct").equals("off")) {
                FacilityManager.getMsgLogger().
                    printmsg(MsgLogger.WARNING,
                             "Input image is RGB and no color transform has "+
                             "been specified. Compression performance and "+
                             "image quality might be greatly degraded. Use "+
                             "the 'Mct' option to specify a color transform");
            }
*/            
            try {
                fctransf = new ForwCompTransf(imgtiler,encSpec);
            } catch (IllegalArgumentException e) {
                error("Could not instantiate forward component "+
                      "transformation"+
                          ((e.getMessage() != null) ?
                           (":\n"+e.getMessage()) : ""),2);
                if(pl.getParameter("debug").equals("on")) {
                    e.printStackTrace();
                } else {
                    error("Use '-debug' option for more details",2);
                }
                return null;
            }
//*/

            // **** ImgDataConverter ****
	    converter = new ImgDataConverter(fctransf);
        //    converter = new ImgDataConverter(imgsrc);

            // **** ForwardWT ****
            try {
                dwt = ForwardWT.createInstance(converter,pl,encSpec);
            } catch (IllegalArgumentException e) {
                error("Could not instantiate wavelet transform"+
                      ((e.getMessage() != null) ?
                       (":\n"+e.getMessage()) : ""),2);
                if(pl.getParameter("debug").equals("on")) {
                    e.printStackTrace();
                } else {
                    error("Use '-debug' option for more details",2);
                }
                return null;
            }

            // **** Quantizer ****
            try{
                quant = Quantizer.createInstance(dwt,encSpec);
            } catch(IllegalArgumentException e) {
                error("Could not instantiate quantizer"+
                      ((e.getMessage() != null) ?
                       (":\n"+e.getMessage()) : ""),2);
                if(pl.getParameter("debug").equals("on")) {
                    e.printStackTrace();
                } else {
                    error("Use '-debug' option for more details",2);
                }
                return null;
            }

            // **** ROIScaler ****
            try{
                rois = ROIScaler.createInstance(quant,pl,encSpec);
            } catch (IllegalArgumentException e) {
                error("Could not instantiate ROI scaler"+
                      ((e.getMessage() != null) ?
                       (":\n"+e.getMessage()) : ""),2);
                if(pl.getParameter("debug").equals("on")) {
                    e.printStackTrace();
                } else {
                    error("Use '-debug' option for more details",2);
                }
                return null;
            }

            // **** EntropyCoder ****
            try {
                ecoder = EntropyCoder.createInstance(rois,pl,encSpec.cblks,
                                                     encSpec.pss,encSpec.bms,
                                                     encSpec.mqrs,encSpec.rts,
                                                     encSpec.css,encSpec.sss,
                                                     encSpec.lcs,encSpec.tts);
            } catch (IllegalArgumentException e) {
                error("Could not instantiate entropy coder"+
                      ((e.getMessage() != null) ?
                       (":\n"+e.getMessage()) : ""),2);
                if(pl.getParameter("debug").equals("on")) {
                    e.printStackTrace();
                } else {
                    error("Use '-debug' option for more details",2);
                }
                return null;
            }

            // **** CodestreamWriter ****
            try {
                // Rely on rate allocator to limit amount of data
            	//**
            	outstream = new ByteArrayOutputStream(); 
            	//**
                bwriter = new FileCodestreamWriter(outstream/*outname*/,Integer.MAX_VALUE);
            } catch (IOException e) {
                error("Could not open output file"+
                      ((e.getMessage() != null) ?
                       (":\n"+e.getMessage()) : ""),2);
                if(pl.getParameter("debug").equals("on")) {
                    e.printStackTrace();
                } else {
                    error("Use '-debug' option for more details",2);
                }
                return null;
            }

            // **** Rate allocator ****
            try {
                ralloc = PostCompRateAllocator.createInstance(ecoder,pl,rate,
                                                              bwriter,encSpec);
            } catch (IllegalArgumentException e) {
                error("Could not instantiate rate allocator"+
                      ((e.getMessage() != null) ?
                       (":\n"+e.getMessage()) : ""),2);
                if(pl.getParameter("debug").equals("on")) {
                    e.printStackTrace();
                } else {
                    error("Use '-debug' option for more details",2);
                }
                System.out.println("HERE 2");
                e.printStackTrace();
                return null;
            }

            // **** HeaderEncoder ****
            headenc = new HeaderEncoder(imgsrc,imsigned,dwt,imgtiler,encSpec,
                                        rois,ralloc,pl);
            ralloc.setHeaderEncoder(headenc);

            // **** Write header to be able to estimate header overhead ****
            headenc.encodeMainHeader();

            // **** Initialize rate allocator, with proper header
            // overhead. This will also encode all the data ****
            ralloc.initialize();

            // **** Write header (final) ****
            headenc.reset();
            headenc.encodeMainHeader();

            // Insert header into the codestream
            bwriter.commitBitstreamHeader(headenc);

            // **** Report info ****
            if (verbose) {
                // Print target rate info
                if(pl.getFloatParameter("rate")!=-1) {
                    FacilityManager.getMsgLogger().
                        println("Target bitrate = "+rate+" bpp (i.e. "+
                                (int)(rate*imgsrc.getImgWidth()
                                      *imgsrc.getImgHeight()/8)+ " bytes)",
                                4,6);
                }
            }

            // **** Now do the rate-allocation and write result ****
            ralloc.runAndWrite();

            // **** Done ****
            bwriter.close();

            // **** Calculate file length ****
            fileLength = bwriter.getLength();

            // **** Tile-parts and packed packet headers ****
            if(pktspertp>0 || pphTile || pphMain) {
             
                try {
                    CodestreamManipulator cm = new 
                        CodestreamManipulator(outname,ntiles,pktspertp,
                                              pphMain,pphTile,tempSop,tempEph);
                    fileLength += cm.doCodestreamManipulation();
                
                    if(pktspertp>0) {
                        FacilityManager.
                            getMsgLogger().println("Created tile-parts "+
                                                   "containing at most "+
                                                   pktspertp+
                                                   " packets per tile.",4,6);
                    }
                    if(pphTile) {
                        FacilityManager.getMsgLogger().
                            println("Moved packet headers "+
                                    "to tile headers",4,6);
                    }
                    if(pphMain) {
                        FacilityManager.getMsgLogger().
                            println("Moved packet headers "+
                                    "to main header",4,6);
                    }
                } catch(IOException e) {
                    error("Error while creating tileparts or packed packet"+
                          " headers"+
                          ((e.getMessage() != null) ?
                           (":\n"+e.getMessage()) : ""),2);
                    if(pl.getParameter("debug").equals("on")) {
                        e.printStackTrace();
                    } else {
                        error("Use '-debug' option for more details",2);
                    }
                    return null;
                }
            }
        	newoutstream = new BEByteArrayOutputStream(outstream,img.getImgWidth(),img.getImgHeight());
            if(useFileFormat) {            
              try {
                newoutstream.writeFileFormat();
              } catch(IOException e) {
                  throw new Error("Error while writing outputstream format");            	  
              }
            }
            // **** Report results ****
            if (verbose) {
                // Print achieved rate
                FacilityManager.getMsgLogger().
                    println("Achieved bitrate = "+ 
                            (8f*fileLength/
                             (imgsrc.getImgWidth()*imgsrc.getImgHeight())) +
                            " bpp (i.e. "+fileLength+" bytes)",4,6);

                // Display ROI information if needed
                if(pl.getParameter("Rroi")!=null && !useFileFormat && 
                   pl.getIntParameter("tile_parts")==0 ) {
                    int roiLen = bwriter.getOffLastROIPkt();
                    FacilityManager.getMsgLogger().
                        printmsg(MsgLogger.INFO,"The Region Of Interest is"+
                                 " encoded in the first "+
                                 roiLen+" bytes of the codestream (i.e "+
                                 (8f*roiLen/(imgsrc.getImgWidth()*
                                             imgsrc.getImgHeight()))+" bpp)");
                    
                }
		FacilityManager.getMsgLogger().flush();
            }

            // **** Close image reader(s) ***
            for(i=0; i<imreader.length; i++) {
                imreader[i].close();
            }
            
        } catch(IllegalArgumentException e) {
	      error(e.getMessage(),2);
            if(pl.getParameter("debug").equals("on")) {
                e.printStackTrace();
            }
	    return null;
	} catch (Error e) {
            error("An uncaught error has occurred: "+e.getMessage(),2);
            if(pl.getParameter("debug").equals("on")) {
                e.printStackTrace();
            } else {
                error("Use '-debug' option for more details",2);
            }
        } catch (RuntimeException e) {
            error("An uncaught runtime exception has occurred: "+
                  e.getMessage(),2);
            if(pl.getParameter("debug").equals("on")) {
                e.printStackTrace();
            } else {
                error("Use '-debug' option for more details",2);
            }
        } catch (Throwable e) {
            error("An unchecked exception has occurred: "+
                  e.getMessage(),2);
            if(pl.getParameter("debug").equals("on")) {
                e.printStackTrace();
            } else {
                error("Use '-debug' option for more details",2);
            }
            return null;
        }

        return newoutstream;
    }
/** 
 * Now see what happens.
 *
 */
public void runAllSlices(int startSlice, int endSlice, boolean useModImage, ViewJProgressBar progressBar) {
	progressBar.updateValueImmed(10);
//	ImgReaderRAWSlice slice;
	ByteArrayOutputStream buff;
	boolean is2D = false;
	if(image.getNDims() == 2) {
		is2D = true;
	}
	try {
		 RAWJP2Header rawhd = new RAWJP2Header();	 
//	 slice = new ImgReaderRAWSlice(pl.getParameter("i"),startSlice);
		 progressBar.updateValueImmed(20);
	 if (useModImage) { 
		 float[] imgRes = image.getResolutions(0);
		 int[] imgExtents = image.getExtents();
		 int imgType = image.getType();
		 int imgModality = image.getImageModality();
		 int imgOrientation = image.getImageOrientation();
		 if(!is2D) {
			 imgExtents[2] = endSlice-startSlice +1;
		 }
		 rawhd.setImgExtents(imgExtents);
		 rawhd.setImgType(imgType);
		 rawhd.setImgResolution(imgRes);
		 rawhd.setImgModality(imgModality);
		 rawhd.setImgOrientation(imgOrientation);
		 if(is2D) {
			 rawhd.setSizeArray(new int[1]);
		 }else {
			 rawhd.setSizeArray(new int[endSlice-startSlice +1]);
		 }
		 BEBufferedRandomAccessFile f;
		 if(is2D) {
			 f = new BEBufferedRandomAccessFile(pl.getParameter("o"),
                     "rw",(int) (1 * 50 * 1024));
		 }else {
			 f = new BEBufferedRandomAccessFile(pl.getParameter("o"),
				                                "rw",(int) ((endSlice-startSlice +1)* 50 * 1024));
		 }

		 progressBar.updateValueImmed(30);
		 ImgReaderRAWSlice slice = new ImgReaderRAWSlice(image,startSlice,false);
		 if(is2D) {
			 progressBar.updateValueImmed(40);
			 buff = run1Slice(slice);
			 rawhd.setIs2D(is2D);
			 rawhd.setSize(buff.size(),0);		 
			 rawhd.writeRawJP2Header(f);
			 progressBar.updateValueImmed(60);
			 f.write(buff.toByteArray(),0,buff.size());
			 progressBar.updateValueImmed(80);
		 }else {
			 for(int sliceIdx=startSlice;sliceIdx<=endSlice;sliceIdx++) {
				 progressBar.updateValueImmed(30 + Math.round((float) sliceIdx / endSlice * 50));
				 slice.setSliceIndex(sliceIdx,true);
				 buff = run1Slice(slice);
				 rawhd.setSize(buff.size(),sliceIdx-startSlice);		 
				 rawhd.writeRawJP2Header(f);
	//			 String s = Integer.toString(f.getPos());
	//			 JOptionPane.showMessageDialog(null, s, "File pointer position", JOptionPane.INFORMATION_MESSAGE);			 
				 f.write(buff.toByteArray(),0,buff.size());
			 }
		 }
		 f.flush();
		 f.close();
		 
	 } else {
//		 int[] imgExtents = image.getExtents();
//		 imgExtents[2] = endSlice-startSlice +1;
//		 rawhd.setImgExtents(imgExtents);
		 rawhd.setSizeArray(new int[endSlice-startSlice +1]);
		 
		 BEBufferedRandomAccessFile f = new BEBufferedRandomAccessFile(pl.getParameter("o"),
				                                "rw",(int) ((endSlice-startSlice +1)* 50 * 1024));

//		 		 JOptionPane.showMessageDialog(null, "It's weird here", "Info", JOptionPane.INFORMATION_MESSAGE);
		 ImgReaderRAWSlice slice = new ImgReaderRAWSlice(pl.getParameter("i"),startSlice);		 
		 for(int sliceIdx=startSlice;sliceIdx<=endSlice;sliceIdx++) {
			 slice.setSliceIndex(sliceIdx,false);
			 buff = run1Slice(slice);
			 rawhd.setSize(buff.size(),sliceIdx-startSlice);		 
			 rawhd.writeRawJP2Header(f);
			 f.write(buff.toByteArray(),0,buff.size());
		 }
		 f.flush();
		 f.close();

	 }
	} catch (IOException e) {
		System.out.println("Error writing final compressed file!");
		JOptionPane.showMessageDialog(null, "Error writing final compressed file!", "Error", JOptionPane.INFORMATION_MESSAGE);		
	}

}
    /**
     * Returns the parameters that are used in this class and
     * implementing classes. It returns a 2D String array. Each of the
     * 1D arrays is for a different option, and they have 4
     * elements. The first element is the option name, the second one
     * is the synopsis, the third one is a long description of what
     * the parameter is and the fourth is its default value. The
     * synopsis or description may be 'null', in which case it is
     * assumed that there is no synopsis or description of the option,
     * respectively. Null may be returned if no options are supported.
     *
     * @return the options name, their synopsis and their explanation, 
     * or null if no options are supported.
     * */
    public static String[][] getParameterInfo() {
        return pinfo;
    }

    /** 
     * Returns all the parameters used in the encoding chain. It calls
     * parameter from each module and store them in one array (one row
     * per parameter and 4 columns).
     *
     * @return All encoding parameters
     *
     * @see #getParameterInfo
     * */
    public static String[][] getAllParameters() {
        Vector<String[]> vec = new Vector<String[]>();
        
        String[][] str = getParameterInfo();
        if(str!=null)
            for(int i=str.length-1; i>=0; i--)
                vec.addElement(str[i]);
        
        str = ForwCompTransf.getParameterInfo();
        if(str!=null)
            for(int i=str.length-1; i>=0; i--)
                vec.addElement(str[i]);
        
        str = AnWTFilter.getParameterInfo();
        if(str!=null)
            for(int i=str.length-1; i>=0; i--)
                vec.addElement(str[i]);
        
        str = ForwardWT.getParameterInfo();
        if(str!=null)
            for(int i=str.length-1; i>=0; i--)
                vec.addElement(str[i]);
        
        str = Quantizer.getParameterInfo();
        if(str!=null)
            for(int i=str.length-1; i>=0; i--)
                vec.addElement(str[i]);
        
        str = ROIScaler.getParameterInfo();
        if(str!=null)
            for(int i=str.length-1; i>=0; i--)
                vec.addElement(str[i]);
        
        str = EntropyCoder.getParameterInfo();
	if(str!=null)
	    for(int i=str.length-1; i>=0; i--)
		vec.addElement(str[i]);

        str = HeaderEncoder.getParameterInfo();
        if(str!=null)
            for(int i=str.length-1; i>=0; i--)
                vec.addElement(str[i]);
        
	str = PostCompRateAllocator.getParameterInfo();
	if(str!=null)
	    for(int i=str.length-1; i>=0; i--)
		vec.addElement(str[i]);
        
        str = PktEncoder.getParameterInfo();
	if(str!=null)
            for(int i=str.length-1; i>=0; i--)
                vec.addElement(str[i]);
        
	str = new String[vec.size()][4];
	if(str!=null)
	    for(int i=str.length-1; i>=0; i--)
		str[i] = (String[])vec.elementAt(i);
        
	return str;
    }
    
    /**
     * Prints the error message 'msg' to standard err, prepending "ERROR" to
     * it, and sets the exitCode to 'code'. An exit code different than 0
     * indicates that there where problems.
     *
     * @param msg The error message
     *
     * @param code The exit code to set
     * */
    private void error(String msg, int code) {
        exitCode = code;
        System.out.println(msg);
        FacilityManager.getMsgLogger().printmsg(MsgLogger.ERROR,msg);
    }

    /**
     * Prints the warning message 'msg' to standard err, prepending "WARNING"
     * to it.
     *
     * @param msg The error message
     * */
    private void warning(String msg) {
        FacilityManager.getMsgLogger().printmsg(MsgLogger.WARNING,msg);
    }

    /**
     * Prints version and copyright information to stdout, using the
     * MsgPrinter.
     * */
    private void printVersionAndCopyright() {
        FacilityManager.getMsgLogger()
            .println("JJ2000's JPEG 2000 EncoderRAW\n",2,4);
        FacilityManager.getMsgLogger()
            .println("Version: "+JJ2KInfo.version+"\n",2,4);
        FacilityManager.getMsgLogger()
            .println("Copyright:\n\n"+JJ2KInfo.copyright+"\n",2,4);
        FacilityManager.getMsgLogger()
            .println("Send bug reports to: "+JJ2KInfo.bugaddr+"\n",2,4);
    }

    /**
     * Prints the usage information to stdout. The usage information
     * is written for all modules in the encoder.
     * */
    private void printUsage() {
    
        MsgLogger ml = FacilityManager.getMsgLogger();

        ml.println("Usage:",0,0);
        ml.println("JJ2KEncoder args...\n",10,12);
        ml.println("The exit code of the encoder is non-zero "+
                   "if an error occurs.\n",2,4);
        ml.println("Note: Many encoder modules accept tile-component "+
                   "specific parameters. These parameters must be provided "+
                   "according to the pattern:\n \"[<tile-component idx>] "+
                   "<param>\" (repeated as many time as needed). ",2,4);
        ml.println("\n"+
                   "<tile-component idx> respect the following policy"+
                   " according to the degree of priority: \n"+
                   "  (1) t<idx> c<idx> : Tile-component specification.\n"+
                   "  (2) t<idx> : Tile specification.\n"+
                   "  (3) c<idx> : Component specification\n"+
                   "  (4) <void> : Default specification.\n\n"+
                   "Where the priorities of the specifications are:\n"+
                   "(1) > (2) > (3) > (4), (\'>\' means \"overrides\")\n",2,4);
        ml.println("  <idx>: ',' separates indexes, '-' separates bounds of "+
                   "indexes list. (ex: 0,2-4 means indexes 0,2,3 and "+
                   " 4).\n",2,4);
        ml.println("The following arguments are recognized:",2,4);
        
        // Info of each encoder parameter
        printParamInfo(ml,getAllParameters());

        // Print bug-report address
        FacilityManager.getMsgLogger().println("\n\n",0,0);
        FacilityManager.getMsgLogger().
            println("Send bug reports to: "+JJ2KInfo.bugaddr+"\n",2,4);
    }
        
    /**
     * Prints the parameters in 'pinfo' to the provided output, 'out', showing
     * the existing defaults. The 'pinfo' argument is a 2D String array. The
     * first dimension contains String arrays, 1 for each parameter. Each of
     * these arrays has 3 elements, the first element is the parameter name,
     * the second element is the synopsis for the parameter and the third one
     * is a long description of the parameter. If the synopsis or description
     * is 'null' then no synopsis or description is printed, respectively. If
     * there is a default value for a parameter it is also printed.
     *
     * @param out Where to print.
     *
     * @param pinfo The parameter information to write.
     * */
    private void printParamInfo(MsgLogger out, String pinfo[][]) {
        String defval;

        if (pinfo == null) {
            return;
        }

        for (int i=0; i<pinfo.length; i++) {
            defval = defpl.getParameter(pinfo[i][0]);
            if (defval != null) { // There is a default value
                out.println("-" + pinfo[i][0] +
                         ((pinfo[i][1] != null) ? " "+pinfo[i][1]+" " : " ") +
                         "(default = "+defval+")",4,8);
            } else { // There is no default value
                out.println("-" + pinfo[i][0] +
                         ((pinfo[i][1] != null) ? " "+pinfo[i][1] : ""),4,8);
            }
            // Is there an explanatory message?
            if (pinfo[i][2] != null) {
                out.println(pinfo[i][2],6,6);
            }
        }
    }

	public void setNcomp(int ncomp) {
		this.ncomp = ncomp;
	}
    
    
    
}
