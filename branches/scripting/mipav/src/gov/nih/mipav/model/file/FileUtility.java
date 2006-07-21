package gov.nih.mipav.model.file;

import java.io.*;

/**
 * @version 1.0
 * @created 26-Jun-2006 11:51:23 AM
 */
public class FileUtility {
    /**
     * unsupported file formats.
     */
    public static final int UNSUPPORTED = 0;
    
	/**
	 * extension: .head, .brik
	 */
	public static final int AFNI = 1;
    
	/**
	 * extension: .img, .hdr
	 */
	public static final int ANALYZE = 2;
    
	/**
	 * extension: .avi
	 */
	public static final int AVI = 3;
    
	/**
	 * extension: .pic && fileID(54L)==12345
	 */
	public static final int BIORAD = 4;
    
	/**
	 * extension: .bmp
	 */
	public static final int BMP = 5;
    
	/**
	 * extension: .brk
	 */
	public static final int BRUKER = 6;
    
	/**
	 * extension: .imc
	 */
	public static final int CHESHIRE = 7;
    
	/**
	 * extension: .oly
	 */
	public static final int CHESHIRE_OVERLAY = 8;
    
	/**
	 * extension: .info
	 */
	public static final int COR = 9;
    
	/**
	 * extension: .cur
	 */
	public static final int CUR = 10;
    
	/**
	 * extension: .dib
	 */
	public static final int DIB = 11;
    
	/**
	 * extension: .dcm
	 */
	public static final int DICOM = 12;
    
	/**
	 * extension: .dm3
	 */
	public static final int DM3 = 13;
    
	/**
	 * extension: .fits
	 */
	public static final int FITS = 14;
    
	/**
	 * extension: .sig
	 */
	public static final int GE_GENESIS = 15;
    
	/**
	 * extension: .gif
	 */
	public static final int GIF = 16;
    
	/**
	 * extension: .ico
	 */
	public static final int ICO = 17;
    
	/**
	 * extension: .ics, .ids
	 */
	public static final int ICS = 19;
    
	/**
	 * extension: .hdr
	 */
	public static final int INTERFILE = 20;
    
	/**
	 * extension: .jpeg, .jpg
	 */
	public static final int JPEG = 21;
    
	/**
	 * extension: .lsm
	 */
	public static final int LSM = 22;
    
	/**
	 * extension: .ima
	 */
	public static final int MAGNETOM_VISION = 23;
    
	/**
	 * extension: .map
	 */
	public static final int MAP = 24;
    
	/**
	 * extension: .bin
	 */
	public static final int MEDIVISION = 25;
    
	/**
	 * extension: .log, .ct
	 */
	public static final int MICOR_CAT = 26;
    
	/**
	 * extension: .mnc
	 */
	public static final int MINC = 27;
    
	/**
	 * extension: .mrc
	 */
	public static final int MRC = 28;
    
	/**
	 * extension: .img, .hdr, .nii
	 */
	public static final int NIFTI = 29;
    
	/**
	 * extension: .wu
	 */
	public static final int OSM = 30;
    
	/**
	 * extension: .pcx
	 */
	public static final int PCX = 31;
    
	/**
	 * extension: .pic
	 */
	public static final int PIC = 32;
    
	/**
	 * extension: .pict
	 */
	public static final int PICT = 33;
    
	/**
	 * extension: .png
	 */
	public static final int PNG = 34;
    
    /**
     * project file format (.xml)
     */
	public static final int PROJECT = 35;
    
	/**
	 * extension: .psd
	 */
	public static final int PSD = 36;
    
	/**
	 * extension: .mov, .qt
	 */
	public static final int QT = 38;
    
	/**
	 * extension: .raw
	 */
	public static final int RAW = 39;
    
	/**
	 * extension: .spm
	 */
	public static final int SPM = 40;
    
	/**
	 * extension: .stk
	 */
	public static final int STK = 41;
    
	/**
	 * extension: .xml
	 */
	public static final int SURFACE_XML = 42;
    
	/**
	 * extension: .tga
	 */
	public static final int TGA = 43;
    
	/**
	 * extension: .tif, .tiff
	 */
	public static final int TIFF = 44;
    
	/**
	 * extension: .tmg
	 */
	public static final int TMG = 45;
    
	/**
	 * extension: .voi
	 */
	public static final int VOI_FILE = 46;
    
	/**
	 * extension: .xbm
	 */
	public static final int XBM = 47;
    
	/**
	 * mipav xml image format. extension: .xml
	 */
	public static final int XML = 48;
    
	/**
	 * extension: .xpm
	 */
	public static final int XPM = 49;

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

   
}