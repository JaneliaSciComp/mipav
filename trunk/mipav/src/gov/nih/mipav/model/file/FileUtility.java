package gov.nih.mipav.model.file;

import java.io.File;

/**
 * @version 1.0
 * @created 26-Jun-2006 11:51:23 AM
 */
public class FileUtility {
    /**
     * unsupported file formats.
     */
    private static final int UNSUPPORTED = 0;
	/**
	 * extension: .head, .brik
	 */
	private static final int AFNI = 1;
	/**
	 * extension: .img, .hdr
	 */
	private static final int ANALYZE = 2;
	/**
	 * extension: .avi
	 */
	private static final int AVI = 3;
	/**
	 * extension: .pic && fileID(54L)==12345
	 */
	private static final int BIORAD = 4;
	/**
	 * extension: .bmp
	 */
	private static final int BMP = 5;
	/**
	 * extension: .brk
	 */
	private static final int BRUKER = 6;
	/**
	 * extension: .imc
	 */
	private static final int CHESHIRE = 7;
	/**
	 * extension: .oly
	 */
	private static final int CHESHIRE_OVERLAY = 8;
	/**
	 * extension: .info
	 */
	private static final int COR = 9;
	/**
	 * extension: .cur
	 */
	private static final int CUR = 10;
	/**
	 * extension: .dib
	 */
	private static final int DIB = 11;
	/**
	 * extension: .dcm
	 */
	private static final int DICOM = 12;
	/**
	 * extension: .dm3
	 */
	private static final int DM3 = 13;
	/**
	 * extension: .fits
	 */
	private static final int FITS = 14;
	/**
	 * extension: .sig
	 */
	private static final int GE_GENESIS = 15;
	/**
	 * extension: .gif
	 */
	private static final int GIF = 16;
	/**
	 * extension: .ico
	 */
	private static final int ICO = 17;
	/**
	 * extension: .ics, .ids
	 */
	private static final int ICS = 19;
	/**
	 * extension: .hdr
	 */
	private static final int INTERFILE = 20;
	/**
	 * extension: .jpeg, .jpg
	 */
	private static final int JPEG = 21;
	/**
	 * extension: .lsm
	 */
	private static final int LSM = 22;
	/**
	 * extension: .ima
	 */
	private static final int MAGNETOM_VISION = 23;
	/**
	 * extension: .map
	 */
	private static final int MAP = 24;
	/**
	 * extension: .bin
	 */
	private static final int MEDIVISION = 25;
	/**
	 * extension: .log, .ct
	 */
	private static final int MICOR_CAT = 26;
	/**
	 * extension: .mnc
	 */
	private static final int MINC = 27;
	/**
	 * extension: .mrc
	 */
	private static final int MRC = 28;
	/**
	 * extension: .img, .hdr
	 */
	private static final int NIFTI = 29;
	/**
	 * extension: .wu
	 */
	private static final int OSM = 30;
	/**
	 * extension: .pcx
	 */
	private static final int PCX = 31;
	/**
	 * extension: .pic
	 */
	private static final int PIC = 32;
	/**
	 * extension: .pict
	 */
	private static final int PICT = 33;
	/**
	 * extension: .png
	 */
	private static final int PNG = 34;
    /**
     * project file format
     */
	private static final int PROJECT = 35;
	/**
	 * extension: .psd
	 */
	private static final int PSD = 36;
	/**
	 * extension: .mov, .qt
	 */
	private static final int QT = 38;
	/**
	 * extension: .raw
	 */
	private static final int RAW = 39;
	/**
	 * extension: .spm
	 */
	private static final int SPM = 40;
	/**
	 * extension: .stk
	 */
	private static final int STK = 41;
	/**
	 * extension: .xml
	 */
	private static final int SURFACE_XML = 42;
	/**
	 * extension: .tga
	 */
	private static final int TGA = 43;
	/**
	 * extension: .tif, .tiff
	 */
	private static final int TIFF = 44;
	/**
	 * extension: .tmg
	 */
	private static final int TMG = 45;
	/**
	 * extension: .voi
	 */
	private static final int VOI_FILE = 46;
	/**
	 * extension: .xbm
	 */
	private static final int XBM = 47;
	/**
	 * extension: .xml
	 */
	private static final int XML = 48;
	/**
	 * extension: .xpm
	 */
	private static final int XPM = 49;

	public FileUtility(){

	}

	public void finalize() throws Throwable {

	}

	/**
	 * Returns the extension of the file name, if file name
     * does not have extension, then return empty string.
     * 
	 * @param fileName  the file name.
	 */
	public static String getExtension(String fileName){
        if(fileName == null || fileName.length() == 0){
            return null;
        }
        
        int index  = fileName.indexOf(".");
        if(index >= 0){
            return fileName.substring(index);
        }
		return "";
	}

    /**
     * Returns the path information from the file name with the path information.
     * @param fileName  the file name wiht the path information.
     * @return the path information.
     */
    public static String getFileDirectory(String fileName){
        if(fileName == null || fileName.length() == 0){
            return null;
        }
        int index = fileName.lastIndexOf(File.separator);
        if(index >= 0){
            return fileName.substring(0, index);
        }
        return null;
    }
    
    /**
     * Returns the file name without path information from file name with the path information.
     * @param fileName the file name with the path information.
     * @return the file name without path information.
     */
    public static String getFileName(String fileName){
        if(fileName == null || fileName.length() == 0){
            return null;
        }
        int index = fileName.lastIndexOf(File.separator);
        if(index >= 0){
            return fileName.substring(index + 1);
        }
        return null;
    }

}