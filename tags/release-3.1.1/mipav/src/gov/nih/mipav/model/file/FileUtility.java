package gov.nih.mipav.model.file;

import java.io.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.TreeMap;


/**
 * @version 1.0
 * @created 26-Jun-2006 11:51:23 AM
 */
public class FileUtility {
	
	
	
	/**
	 * New File Types should
	 * be added to the bottom 
	 * and not inserted somewhere in
	 * the middle.
	 */
	
	
	/** Ill defined file type. */
    public static final int ERROR = -1;
    
    /**
     * Undefined file type.
     */
    public static final int UNDEFINED = 0;
    
	/**
	 * AFNI file type.
	 * extension: .head, .brik
	 */
	public static final int AFNI = 1;
    
	/**
	 * Analyze format (Mayo).
	 * extension: .img, .hdr
	 */
	public static final int ANALYZE = 2;
	
	/** 
	 * Multiple files of type analyze. 
	 * 
	 * */
    public static final int ANALYZE_MULTIFILE = 3;
    
	/**
	 * AVI file type. Windows Media.
	 * extension: .avi
	 */
	public static final int AVI = 4;
    
	/**
	 * Used by the Bio-Rad Pic format. 
	 * extension: .pic && fileID(54L)==12345
	 */
	public static final int BIORAD = 5;
    
	/**
	 * 
	 * extension: .bmp
	 */
	public static final int BMP = 6;
    
	/**
	 * Bruker file format.
	 * extension: .brk
	 */
	public static final int BRUKER = 7;
    
	/**
	 * Cheshire file type (a kind of Analyze). 
	 * extension: .imc
	 */
	public static final int CHESHIRE = 8;
    
	/**
	 * Cheshire overlay file type. Contains VOIs. 
	 * extension: .oly
	 */
	public static final int CHESHIRE_OVERLAY = 9;
    
	/**
	 * Used by FreeSurfer software. 
	 * extension: .info
	 */
	public static final int COR = 10;
    
	/**
	 * 
	 * extension: .cur
	 */
	public static final int CUR = 11;
    
	/**
	 * 
	 * extension: .dib
	 */
	public static final int DIB = 12;
    
	/**
	 * Digital Imaging and COmmunications in Medicine file type. Fully implemented versions 2 & 3. 
	 * extension: .dcm
	 */
	public static final int DICOM = 13;
    
	/**
	 * Gatan's Digital Micrograph version 3 file format. 
	 * extension: .dm3
	 */
	public static final int DM3 = 14;
    
	/**
	 * FITS file type.
	 * extension: .fits
	 */
	public static final int FITS = 15;
	
	/**
	 * GE Genesis 5X and LX. 
	 * extension: .sig
	 */
	public static final int GE_GENESIS = 16;
	
	/** 
	 * GE Signa 4.x. 
	 * 
	 * */
    public static final int GE_SIGNA4X = 17;
	
	/**
	 * 
	 * extension: .gif
	 */
	public static final int GIF = 18;
	
	/**
	 * 
	 * extension: .ico
	 */
	public static final int ICO = 19;
	
	/**
	 * Image Cytometry Standard. 
	 * extension: .ics, .ids
	 */
	public static final int ICS = 20;
	
	/**
	 * Interfile file format used in Nuclear Medicine. 
	 * extension: .hdr
	 */
	public static final int INTERFILE = 21;
	
	/** 
	 * Java Image Manangement Interface file type. 
	 * 
	 * */
    public static final int JIMI = 22;
	
	/**
	 * 
	 * extension: .jpeg, .jpg
	 */
	public static final int JPEG = 23;
	
	/**
	 * Used by the Zeiss LSM 510 Dataserver. 
	 * extension: .lsm
	 */
	public static final int LSM = 24;
	
	
	/** 
	 * Used by the Zeiss LSM 510 Dataserver. 
	 * 
	 * */
    public static final int LSM_MULTIFILE = 25;
	
	/**
	 * Siemens MAGNETOM VISION. 
	 * extension: .ima
	 */
	public static final int MAGNETOM_VISION = 26;
	
	/**
	 * Benes Trus special file type.
	 * extension: .map
	 */
	public static final int MAP = 27;
	
	/**
	 * 
	 * extension: .bin
	 */
	public static final int MEDIVISION = 28;
	
	/** 
	 * MGH/MGZ volume format. 
	 * 
	 * */
    public static final int MGH = 29;
	
	/**
	 * Micro CT format for small animal imaging. 
	 * extension: .log, .ct
	 */
	public static final int MICRO_CAT = 30;
	
	/**
	 * MINC file type. MINC is a medical imaging oriented extension of the NetCDF file format. NetCDF stands for
     * 'Network Common Data Form'.
	 * extension: .mnc
	 */
	public static final int MINC = 31;
	
	/** 
	 * Not presently implemented. 
	 * 
	 * */
    public static final int MIPAV = 32;
	
	/**
	 * 
	 * extension: .mrc
	 */
	public static final int MRC = 33;
	
	/**
	 * NIFTI format. 
	 * extension: .img, .hdr, .nii
	 */
	public static final int NIFTI = 34;
	
	/** 
	 * NIFTI multi-file format. 
	 * 
	 * */
    public static final int NIFTI_MULTIFILE = 35;
    
    /** 
     * Nearly raw raster data. 
     * 
     * */
    public static final int NRRD = 36;
	
	/**
	 * Washington University OSM dataset structure. 
	 * extension: .wu
	 */
	public static final int OSM = 37;
	
	/**
	 * 
	 * extension: .pcx
	 */
	public static final int PCX = 38;
    
	/**
	 * 
	 * extension: .pic
	 */
	public static final int PIC = 39;
    
	/**
	 * 
	 * extension: .pict
	 */
	public static final int PICT = 40;
    
	/**
	 * 
	 * extension: .png
	 */
	public static final int PNG = 41;
    
    /**
     * MIPAV project format.
     * project file format (.xml)
     */
	public static final int PROJECT = 42;
    
	/**
	 * 
	 * extension: .psd
	 */
	public static final int PSD = 43;
    
	/**
	 * Quicktime file type.
	 * extension: .mov, .qt
	 */
	public static final int QT = 44;
    
	/**
	 * RAW image data, no header.
	 * extension: .raw
	 */
	public static final int RAW = 45;
	
	/** 
	 * RAW MULTIFLE image data, no header. 
	 * 
	 * */
    public static final int RAW_MULTIFILE = 46;
    
	/**
	 * SPM file format. 
	 * extension: .spm
	 */
	public static final int SPM = 47;
	
	/**
	 * MetaMorph Stack (STK) file type.
	 * extension: .stk
	 */
	public static final int STK = 48;
    
	/**
	 * MIPAV Surface XML file format.
	 * extension: .xml
	 */
	public static final int SURFACE_XML = 49;
    
	/**
	 * 
	 * extension: .tga
	 */
	public static final int TGA = 50;
    
	/**
	 * TIFF file; tagged header. 
	 * extension: .tif, .tiff
	 */
	public static final int TIFF = 51;
	
	/** 
	 * Multiple files of TIFF images. 
	 * 
	 * */
    public static final int TIFF_MULTIFILE = 52;
    
	/**
	 * Optical coherence tomography.
	 * extension: .tmg
	 */
	public static final int TMG = 53;
    
	/**
	 * VOI file, used to read VOIs.
	 * extension: .voi
	 */
	public static final int VOI_FILE = 54;
    
	/**
	 * 
	 * extension: .xbm
	 */
	public static final int XBM = 55;
    
	/**
	 * MIPAV XML file format. 
	 * mipav xml image format. extension: .xml
	 */
	public static final int XML = 56;
	
	/** 
	 * MIPAV XML file format. 
	 * 
	 * */
    public static final int XML_MULTIFILE = 57;
    
	/**
	 * 
	 * extension: .xpm
	 */
	public static final int XPM = 58;
	
	

    /** Arrary of strings describing the file formats.
     *  These are in synch with the above constants (same order)
     *  
     *  
     */
    private static String[] fileFormatStr = {
        "Undefined", "AFNI", "Analyze", "Analyze multifile", "Avi", "Bio-Rad", "BMP", "BRUKER", "Chesire", "Chesire Overlay",
        "COR", "CUR", "DIB", "DICOM", "DM3", "FITS", "GE Genesis", "GE Signa4x", "GIF", "ICO", "ICS", "Interfile",
        "JIMI", "JPEG", "LSM", "LSM multifile", "Magnetom Vision", "Map", "Medvision", "MGH", "Micro CAT", "MINC",
        "MIPAV", "MRC", "NIFTI", "NIFTI multifile", "NRRD", "OSM", "PCX", "PIC", "PICT", "PNG", "Project", "PSD", "QT",
        "Raw", "Raw multifile", "SPM", "STK", "Surface XML", "TGA", "Tiff", "Tiff multifile", "TMG", "VOI", "XBM",
        "XML", "XML multifile", "XPM"
    };
    

    
    /** Supported File Extensions 
     * 
     *  New file extensions should be added here and
     *  also need to be associated to a FileType
     *  (This is done in the getFileTypeFromSuffix(String suffix) method)
     * 
     * */
    private static String[] supportedFileExtensions = new String[] {"tif", "tiff", "lsm", "stk", "jpeg",
    									"jpg", "bmp", "gif", "pict", "pic", "png", "psd", "dib", "tga",
    									"xbm", "xpm", "xml", "pcx", "ico", "cur", "mgh", "mgz", "raw",
    									"img", "nii", "nhdr", "nrrd", "ima", "dcm", "bin", "map", "mnc",
    									"avi", "imc", "oly", "qt", "mov", "head", "brik", "ics", "ids",
    									"hdr", "spm", "fits", "dm3", "tmg", "mrc", "wu", "sig", "gedno",
    									"log", "ct", "info", "info~", "voi", "afni"};
    
    
    
    /** This map is needed in order to populate JDialogUnknownIO typeNames */
    private static TreeMap typeNamesTreeMap;
    
    
    
    
    
   
    
    
    
    
    
    /** Gets the file type based upon the suffix
     * 
     *  @param suffix the suffix
     *  
     *  
     */
    public static int getFileTypeFromSuffix(String suffix) {
    	int fileType = FileUtility.UNDEFINED;
    
    	if (suffix.equalsIgnoreCase(".tif")) {
            fileType = FileUtility.TIFF;
        } 
    	else if (suffix.equalsIgnoreCase(".tiff")) {
            fileType = FileUtility.TIFF;
        } 
    	else if (suffix.equalsIgnoreCase(".lsm")) {
            fileType = FileUtility.LSM;
        } 
    	else if (suffix.equalsIgnoreCase(".stk")) {
            fileType = FileUtility.STK;
        } 
    	else if (suffix.equalsIgnoreCase(".jpeg")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".jpg")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".bmp")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".gif")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".pict")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".pic")) {
        	//Both Biorad and JIMI use the pic suffix
        	fileType = FileUtility.UNDEFINED;
    	}
    	else if (suffix.equalsIgnoreCase(".png")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".psd")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".dib")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".tga")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".xbm")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".xpm")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".xml")) {
            fileType = FileUtility.XML;
        } 
    	else if (suffix.equalsIgnoreCase(".pcx")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".ico")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".cur")) {
            fileType = FileUtility.JIMI;
        } 
    	else if (suffix.equalsIgnoreCase(".mgh")) {
            fileType = FileUtility.MGH;
        } 
    	else if (suffix.equalsIgnoreCase(".mgz")) {
            fileType = FileUtility.MGH;
        } 
    	else if (suffix.equalsIgnoreCase(".raw")) {
            fileType = FileUtility.RAW;
        } 
    	else if (suffix.equalsIgnoreCase(".img")) {
    		//Both ANALYZE and NIFTI use .img and .hdr
        	fileType = FileUtility.UNDEFINED;
    	}
    	else if (suffix.equalsIgnoreCase(".nii")) {
            fileType = FileUtility.NIFTI;
        } 
    	else if (suffix.equalsIgnoreCase(".nhdr")) {
            fileType = FileUtility.NRRD;
        } 
    	else if (suffix.equalsIgnoreCase(".nrrd")) {
            fileType = FileUtility.NRRD;
        } 
    	else if (suffix.equalsIgnoreCase(".ima")) {
    		//Both Dicom and Siemens Magnetom Vision file type have the ima suffix
    		fileType = FileUtility.UNDEFINED;
    	}
    	else if (suffix.equalsIgnoreCase(".dcm")) {
            fileType = FileUtility.DICOM;
        } 
    	else if (suffix.equalsIgnoreCase(".bin")) {
            fileType = FileUtility.MEDIVISION;
        } 
    	else if (suffix.equalsIgnoreCase(".map")) {
            fileType = FileUtility.MAP;
        } 
    	// Benes Trus special
        else if (suffix.equalsIgnoreCase(".mnc")) {
            fileType = FileUtility.MINC;
        } 
        else if (suffix.equalsIgnoreCase(".avi")) {
            fileType = FileUtility.AVI;
        } 
        else if (suffix.equalsIgnoreCase(".imc")) {
            fileType = FileUtility.CHESHIRE;
        } 
        else if (suffix.equalsIgnoreCase(".oly")) {
            fileType = FileUtility.CHESHIRE_OVERLAY;
        } 
    	// QuickTime on WIndows uses .QT and on MAC uses .mov
        else if (suffix.equalsIgnoreCase(".qt")) {
            fileType = FileUtility.QT;
        } 
        else if (suffix.equalsIgnoreCase(".mov")) {
            fileType = FileUtility.QT;
        } 
        else if (suffix.equalsIgnoreCase(".head")) {
            fileType = FileUtility.AFNI;
        } 
        else if (suffix.equalsIgnoreCase(".brik")) {
            fileType = FileUtility.AFNI;
        } 
        else if (suffix.equalsIgnoreCase(".ics")) {
            fileType = FileUtility.ICS;
        } 
        else if (suffix.equalsIgnoreCase(".ids")) {
            fileType = FileUtility.ICS;
        } 
        else if (suffix.equalsIgnoreCase(".hdr")) {
            fileType = FileUtility.INTERFILE;
        } 
        else if (suffix.equalsIgnoreCase(".spm")) {
            fileType = FileUtility.SPM;
        } 
        else if (suffix.equalsIgnoreCase(".fits")) {
            fileType = FileUtility.FITS;
        } 
        else if (suffix.equalsIgnoreCase(".dm3")) {
            fileType = FileUtility.DM3;
        } 
        else if (suffix.equalsIgnoreCase(".tmg")) {
            fileType = FileUtility.TMG;
        } 
        else if (suffix.equalsIgnoreCase(".mrc")) {
            fileType = FileUtility.MRC;
        } 
        else if (suffix.equalsIgnoreCase(".wu")) {
            fileType = FileUtility.OSM;
        } 
        else if (suffix.equalsIgnoreCase(".sig")) {
            fileType = FileUtility.GE_GENESIS;
        } 
        else if (suffix.equalsIgnoreCase(".gedno")) {
            fileType = FileUtility.GE_SIGNA4X;
        } 
        else if (suffix.equalsIgnoreCase(".log")) {
            fileType = FileUtility.MICRO_CAT;
        } 
        else if (suffix.equalsIgnoreCase(".ct")) {
            fileType = FileUtility.MICRO_CAT;
        } 
        else if (suffix.equalsIgnoreCase(".info")) {
            fileType = FileUtility.COR;
        } 
        else if (suffix.equalsIgnoreCase(".info~")) {
            fileType = FileUtility.COR;
        }
        else if (suffix.equalsIgnoreCase(".voi")) {
            fileType = FileUtility.VOI_FILE;
        }
        else if (suffix.equalsIgnoreCase(".afni")) {
            fileType = FileUtility.AFNI;
        }
        else { 
        	//cannot automatically determine the filetype from the filename extension
            fileType = FileUtility.UNDEFINED;
        }
    	return fileType;
	
    }
    
    
    
    
    
    
    
    
    
    
    /**
     * Gets the file extension based on the filetype of the image.
     * This returns the DEFAULT suffix
     * 
     *
     * @param   fileType  Type of file, found in FileBase.
     *
     * @return  The appropriate file extension.
     *
     */
    public static final String getDefaultSuffix(int fileType) {
        String suffix = null;

        switch (fileType) {

            case FileUtility.JIMI:
                suffix = ".jpg";
                break;

            case FileUtility.RAW:
                suffix = ".raw";
                break;

            case FileUtility.DICOM:
                suffix = ".dcm";
                break;

            case FileUtility.MEDIVISION:
                suffix = ".bin";
                break;

            case FileUtility.MAP:
                suffix = ".map";
                break;

            case FileUtility.MINC:
                suffix = ".mnc";
                break;

            case FileUtility.AVI:
                suffix = ".avi";
                break;

            case FileUtility.QT:
                suffix = ".mov";
                break;

            case FileUtility.CHESHIRE:
                suffix = ".imc";
                break;

            case FileUtility.CHESHIRE_OVERLAY:
                suffix = ".oly";
                break;

            case FileUtility.VOI_FILE:
                suffix = ".voi";
                break;

            case FileUtility.ANALYZE:
                suffix = ".img";
                break;

            case FileUtility.MGH:

                // Uses .mgh for uncompressed storage
                // Uses .mgz or .mgh.gz for compressed storage
                suffix = ".mgh";
                break;

            case FileUtility.NIFTI:

                // uses .hdr and .img for 2 file storage
                // uses .nii for 1 file storage
                suffix = ".nii";
                break;

            case FileUtility.NRRD:

                // uses .nhdr for header and any nhdr designated extension for data
                // in 2 file storage
                // uses .nrrd for 1 file storage
                suffix = ".nrrd";
                break;

            case FileUtility.SPM:
                suffix = ".spm";
                break;

            case FileUtility.TIFF:
                suffix = ".tiff";
                break;

            case FileUtility.LSM:
                suffix = ".lsm";
                break;

            case FileUtility.STK:
                suffix = ".stk";
                break;

            case FileUtility.AFNI:
                suffix = ".afni";
                break;

            case FileUtility.ICS:
                suffix = ".ics";
                break;

            case FileUtility.INTERFILE:
                suffix = ".hdr";
                break;

            case FileUtility.BIORAD:
                suffix = ".pic";
                break;

            case FileUtility.FITS:
                suffix = ".fits";
                break;

            case FileUtility.DM3:
                suffix = ".dm3";
                break;

            case FileUtility.TMG:
                suffix = ".tmg";
                break;

            case FileUtility.MRC:
                suffix = ".mrc";
                break;

            case FileUtility.OSM:
                suffix = ".wu";
                break;

            case FileUtility.MAGNETOM_VISION:
                suffix = ".ima";
                break;

            case FileUtility.GE_GENESIS:
                suffix = ".sig";
                break;

            case FileUtility.GE_SIGNA4X:
                suffix = ".gedno";
                break;

            case FileUtility.MICRO_CAT:
                suffix = ".log";
                break;

            case FileUtility.XML:
                suffix = ".xml";
                break;

            case FileUtility.COR:
                suffix = ".info";
                break;
                
            case FileUtility.BMP:
                suffix = ".bmp";
                break;
            
            case FileUtility.CUR:
                suffix = ".cur";
                break;
                
            case FileUtility.DIB:
                suffix = ".dib";
                break;
             
            case FileUtility.GIF:
                suffix = ".gof";
                break;
           
            case FileUtility.ICO:
                suffix = ".ico";
                break;
                
            case FileUtility.JPEG:
                suffix = ".jpg";
                break;
                
            case FileUtility.PCX:
                suffix = ".pcx";
                break;
            
            case FileUtility.PICT:
                suffix = ".pict";
                break;
                
            case FileUtility.PNG:
                suffix = ".png";
                break;
                
            case FileUtility.PSD:
                suffix = ".psd";
                break; 
                
            case FileUtility.TGA:
                suffix = ".tga";
                break; 
                
            case FileUtility.XBM:
                suffix = ".xbm";
                break;
                
            case FileUtility.XPM:
                suffix = ".xpm";
                break;
             
        }

        return suffix;
    }
    
    
    
    /**
     * Returns the string for a particular file format.
     *
     * @param   format  int representing the file format (see the static definitions)
     *
     * @return  string representing the file format
     */
    public static String getFileFormatStr(int format) {

        if (format == FileUtility.ERROR) {
            return "Error";
        }

        try {
            return FileUtility.fileFormatStr[format];
        } catch (ArrayIndexOutOfBoundsException ae) { }

        return "";

    }
	
	
	/**
	 * This method is called by JDialogUnknownIO in order to populate its typeNames
	 * 
	 * 
	 * @return Strin[] Array of descriptions
	 */
    public static String[] getUnknownDialogsTypeNames() {
    	typeNamesTreeMap = new TreeMap();
    	String description  = "";
    	for(int i=0;i<supportedFileExtensions.length;i++) {
    		int fileType = getFileTypeFromSuffix("." + supportedFileExtensions[i]);
    		if(fileType == FileUtility.JIMI) {
    			if(supportedFileExtensions[i].equals("jpeg") || (supportedFileExtensions[i].equals("jpg"))) {
    				description = fileFormatStr[FileUtility.JPEG];
    			}
    			else if(supportedFileExtensions[i].equals("bmp")) {
    				description = fileFormatStr[FileUtility.BMP];
    			}
    			else if(supportedFileExtensions[i].equals("gif")) {
    				description = fileFormatStr[FileUtility.GIF];
    			}
    			else if(supportedFileExtensions[i].equals("pict")) {
    				description = fileFormatStr[FileUtility.PICT];
    			}
    			else if(supportedFileExtensions[i].equals("png")) {
    				description = fileFormatStr[FileUtility.PNG];
    			}
    			else if(supportedFileExtensions[i].equals("psd")) {
    				description = fileFormatStr[FileUtility.PSD];
    			}
    			else if(supportedFileExtensions[i].equals("dib")) {
    				description = fileFormatStr[FileUtility.DIB];
    			}
    			else if(supportedFileExtensions[i].equals("tga")) {
    				description = fileFormatStr[FileUtility.TGA];
    			}
    			else if(supportedFileExtensions[i].equals("xbm")) {
    				description = fileFormatStr[FileUtility.XBM];
    			}
    			else if(supportedFileExtensions[i].equals("xpm")) {
    				description = fileFormatStr[FileUtility.XPM];
    			}
    			else if(supportedFileExtensions[i].equals("pcx")) {
    				description = fileFormatStr[FileUtility.PCX];
    			}
    			else if(supportedFileExtensions[i].equals("ico")) {
    				description = fileFormatStr[FileUtility.ICO];
    			}
    			else if(supportedFileExtensions[i].equals("cur")) {
    				description = fileFormatStr[FileUtility.CUR];
    			}	
    		}
    		else if(fileType == FileUtility.UNDEFINED) {
    			if(supportedFileExtensions[i].equals("pic")) {
    				description = fileFormatStr[FileUtility.BIORAD];
    			}
    			//*.ima and *.img each go to 2 diff file types
    			if(supportedFileExtensions[i].equals("ima")) {
    				description = fileFormatStr[FileUtility.DICOM];
    				if(typeNamesTreeMap.containsKey(description)) {
    	    			String value = (String)typeNamesTreeMap.get(description);
    	    			typeNamesTreeMap.put(description, value + ",." + supportedFileExtensions[i]);
    	    		}
    	    		else {
    	    			typeNamesTreeMap.put(description, "." + supportedFileExtensions[i]);
    	    		}
    				description = fileFormatStr[FileUtility.MAGNETOM_VISION];
    				if(typeNamesTreeMap.containsKey(description)) {
    	    			String value = (String)typeNamesTreeMap.get(description);
    	    			typeNamesTreeMap.put(description, value + ",." + supportedFileExtensions[i]);
    	    		}
    	    		else {
    	    			typeNamesTreeMap.put(description, "." + supportedFileExtensions[i]);
    	    		}
    				continue;	
    			}
    			if(supportedFileExtensions[i].equals("img")) {
    				description = fileFormatStr[FileUtility.ANALYZE];
    				if(typeNamesTreeMap.containsKey(description)) {
    	    			String value = (String)typeNamesTreeMap.get(description);
    	    			typeNamesTreeMap.put(description, value + ", ." + supportedFileExtensions[i]);
    	    		}
    	    		else {
    	    			typeNamesTreeMap.put(description, "." + supportedFileExtensions[i]);
    	    		}
    				description = fileFormatStr[FileUtility.NIFTI];
    				if(typeNamesTreeMap.containsKey(description)) {
    	    			String value = (String)typeNamesTreeMap.get(description);
    	    			typeNamesTreeMap.put(description, value + ", ." + supportedFileExtensions[i]);
    	    		}
    	    		else {
    	    			typeNamesTreeMap.put(description, "." + supportedFileExtensions[i]);
    	    		}
    				continue;
    			}
    		}
    		else {
    			description = fileFormatStr[fileType];
    		}
    		if(typeNamesTreeMap.containsKey(description)) {
    			String value = (String)typeNamesTreeMap.get(description);
    			typeNamesTreeMap.put(description, value + ", ." + supportedFileExtensions[i]);
    		}
    		else {
    			typeNamesTreeMap.put(description, "." + supportedFileExtensions[i]);
    		}
    	}
    	
    	
    	ArrayList descripArrayList = new ArrayList();
    	for(Iterator iter = typeNamesTreeMap.keySet().iterator();iter.hasNext();) {
    		String key = (String)iter.next();
    		String value = (String)typeNamesTreeMap.get(key);
    		String descrip = key + " (" + value + ")";
    		descripArrayList.add(descrip);
    	}
    	
    	
    	String[] descripArray = new String[descripArrayList.size()];
    	for(int i=0;i<descripArrayList.size();i++) {
    		descripArray[i] = (String)descripArrayList.get(i);
    	}
    	
    	return descripArray;
    }
	
	
	
    /**
     *This method is called by JDialogUnknownIO in order to populate its typeSuffices
	 * 
	 * 
	 * @return String[] Array of suffices
	 */
    public static String[] getUnknownDialogsTypeSuffices() {
    	ArrayList typeSufficesAL  = new ArrayList();
    	for(Iterator iter = typeNamesTreeMap.keySet().iterator();iter.hasNext();) {
    		String key = (String)iter.next();
    		//since the fileFormetStr array ant the static final ints are in synch, getting the position
    		//in the array is the same as getting the file type...so that we can get the default suffix
    		for(int i=0;i<fileFormatStr.length;i++) {
    			if(key.equals(fileFormatStr[i])) {
    				String suff = getDefaultSuffix(i);
    				typeSufficesAL.add(suff);
    			}
    		}
    	}
    	String[] typeSuffices = new String[typeSufficesAL.size()];
    	for(int i=0;i<typeSufficesAL.size();i++) {
    		typeSuffices[i] = (String)typeSufficesAL.get(i);
    	}
    	return typeSuffices;

    }
    
    
    /**
     *This method is called by JDialogUnknownIO in order to populate its typeInts
	 * 
	 * 
	 * @return String[] Array of typeInts
	 */
    public static int[] getUnknownDialogsTypeInts() {
    	String[] exts = getUnknownDialogsTypeSuffices();
    	int[] fileTypes = new int[exts.length];
    	for(int i=0;i<exts.length;i++) {
    		String suffix = exts[i];
    		int fileType = getFileTypeFromSuffix(suffix);
    		if(exts[i].equals("pic")) {
    			fileType = FileUtility.BIORAD;
    		}
    		else if(exts[i].equals("img")) {
    			fileType = FileUtility.NIFTI;
    		}
    		else if(exts[i].equals("ima")) {
    			fileType = FileUtility.MAGNETOM_VISION;
    		}
    		fileTypes[i] = fileType;
    	}
    	
    	return fileTypes;
    }
    
    
    
    
 
    

	/**
	 * Returns the extension of the file name, if file name
     * does not have extension, then return empty string.
     * 
	 * @param   absolutePath  the file name.
     * 
     * @return  The file's extension.
	 */
	public static final String getExtension(String absolutePath){
        if(absolutePath == null || absolutePath.length() == 0){
            return null;
        }
        
        int index  = absolutePath.lastIndexOf(".");
        if(index >= 0){
            return absolutePath.substring(index);
        }
		return "";
	}
    
    /**
     * Helper method to strip the image name of the extension, so when we save we don't have double extensions (like
     * genormcor.img.tif).
     *
     * @param   fileName  Original name.
     *
     * @return  Name without extension, or original name if there was no extension.
     */
    public static final String stripExtension(String fileName) {
        int index = fileName.lastIndexOf(".");

        if (index != -1) {
            return fileName.substring(0, index);
        } else {
            return fileName;
        }
    }

    /**
     * Returns the path information from the file name with the path information.
     * 
     * @param   fileName  the file name wiht the path information.
     * 
     * @return  The path information.
     */
    public static final String getFileDirectory(String fileName){
        if(fileName == null || fileName.length() == 0){
            return null;
        }
        int index = fileName.lastIndexOf(File.separator);
        if(index >= 0){
            return fileName.substring(0, index + 1);
        }
        return null;
    }
    
    /**
     * Returns the file name without path information from file name with the path information.
     * 
     * @param   absolutePath    the file name with the path information.
     * 
     * @return  The file name without path information.
     */
    public static final String getFileName(String absolutePath){
        if(absolutePath == null || absolutePath.length() == 0){
            return null;
        }
        int index = absolutePath.lastIndexOf(File.separator);
        if(index >= 0){
            if (index == absolutePath.length() - 1) {
                return null;
            }
            
            return absolutePath.substring(index + 1);
        }
        return absolutePath;
    }








	public static String[] getSupportedFileExtensions() {
		return supportedFileExtensions;
	}

    
    
    
   
}