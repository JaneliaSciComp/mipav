package gov.nih.mipav.model.file;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import ncsa.hdf.object.*;
import java.io.*;

import java.util.*;


/**
 * Constants and static methods which relate to file input, output or processing.
 */
public class FileUtility {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** New File Types should be added to the bottom and not inserted somewhere in the middle. */

    /** Ill defined file type. */
    public static final int ERROR = -1;

    /** Undefined file type. */
    public static final int UNDEFINED = 0;

    /** AFNI file type. extension: .head, .brik */
    public static final int AFNI = 1;

    /** Analyze format (Mayo). extension: .img, .hdr */
    public static final int ANALYZE = 2;

    /** Multiple files of type analyze. */
    public static final int ANALYZE_MULTIFILE = 3;

    /** AVI file type. Windows Media. extension: .avi */
    public static final int AVI = 4;

    /** Used by the Bio-Rad Pic format. extension: .pic && fileID(54L)==12345 */
    public static final int BIORAD = 5;

    /** extension: .bmp. */
    public static final int BMP = 6;

    /** Bruker file format. 
     * Reads a BRUKER file by first reading in the d3proc header file, second the reco header file, third the acqp file int the 
     * same directory or up one or two two parent directories, and finally the 2dseq binary file. */
    public static final int BRUKER = 7;

    /** Cheshire file type (a kind of Analyze). extension: .imc */
    public static final int CHESHIRE = 8;

    /** Cheshire overlay file type. Contains VOIs. extension: .oly */
    public static final int CHESHIRE_OVERLAY = 9;

    /** Used by FreeSurfer software. extension: -.info or -.info~ for header file
     *  -.nnn for slice data file where nnn is the slice number */
    public static final int COR = 10;

    /** extension: .cur. */
    public static final int CUR = 11;

    /** extension: .dib. */
    public static final int DIB = 12;

    /** Digital Imaging and COmmunications in Medicine file type. Fully implemented versions 2 & 3. extension: .dcm */
    public static final int DICOM = 13;

    /** Gatan's Digital Micrograph version 3 file format. extension: .dm3 */
    public static final int DM3 = 14;

    /** FITS file type. extension: .fits */
    public static final int FITS = 15;

    /** GE Genesis 5X and LX. extension: .sig */
    public static final int GE_GENESIS = 16;

    /** GE Signa 4.x. */
    public static final int GE_SIGNA4X = 17;

    /** extension: .gif. */
    public static final int GIF = 18;

    /** extension: .ico. */
    public static final int ICO = 19;

    /** Image Cytometry Standard. extension: .ics, .ids */
    public static final int ICS = 20;

    /** Interfile file format used in Nuclear Medicine. extension: .hdr */
    public static final int INTERFILE = 21;

    /** Java Image Manangement Interface file type. */
    public static final int JIMI = 22;

    /** extension: .jpeg, .jpg. */
    public static final int JPEG = 23;

    /** Used by the Zeiss LSM 510 Dataserver. extension: .lsm */
    public static final int LSM = 24;


    /** Used by the Zeiss LSM 510 Dataserver. */
    public static final int LSM_MULTIFILE = 25;

    /** Siemens MAGNETOM VISION. extension: .ima */
    public static final int MAGNETOM_VISION = 26;

    /** Benes Trus special file type. extension: .map */
    public static final int MAP = 27;

    /** extension: .bin. */
    public static final int MEDIVISION = 28;

    /** MGH/MGZ volume format. */
    public static final int MGH = 29;

    /** Micro CT format for small animal imaging. extension: .log, .ct */
    public static final int MICRO_CAT = 30;

    /**
     * MINC file type. MINC is a medical imaging oriented extension of the NetCDF file format. NetCDF stands for
     * 'Network Common Data Form'. extension: .mnc
     */
    public static final int MINC = 31;

    /** Not presently implemented. */
    public static final int MIPAV = 32;

    /** extension: .mrc. */
    public static final int MRC = 33;

    /** NIFTI format. extension: .img, .hdr, .nii */
    public static final int NIFTI = 34;

    /** NIFTI multi-file format. */
    public static final int NIFTI_MULTIFILE = 35;

    /** Nearly raw raster data. */
    public static final int NRRD = 36;

    /** Washington University OSM dataset structure. extension: .wu */
    public static final int OSM = 37;

    /** extension: .pcx. */
    public static final int PCX = 38;

    /** extension: .pic. */
    public static final int PIC = 39;

    /** extension: .pict. */
    public static final int PICT = 40;

    /** extension: .png. */
    public static final int PNG = 41;

    /** MIPAV project format. project file format (.xml) */
    public static final int PROJECT = 42;

    /** extension: .psd. */
    public static final int PSD = 43;

    /** Quicktime file type. extension: .mov, .qt */
    public static final int QT = 44;

    /** RAW image data, no header. extension: .raw */
    public static final int RAW = 45;

    /** RAW MULTIFLE image data, no header. */
    public static final int RAW_MULTIFILE = 46;

    /** SPM file format. extension: .spm */
    public static final int SPM = 47;

    /** MetaMorph Stack (STK) file type. extension: .stk */
    public static final int STK = 48;

    /** MIPAV Surface XML file format. extension: .xml */
    public static final int SURFACE_XML = 49;

    /** extension: .tga. */
    public static final int TGA = 50;

    /** TIFF file; tagged header. extension: .tif, .tiff */
    public static final int TIFF = 51;

    /** Multiple files of TIFF images. */
    public static final int TIFF_MULTIFILE = 52;

    /** Optical coherence tomography. extension: .tmg */
    public static final int TMG = 53;

    /** VOI file, used to read VOIs. extension: .voi */
    public static final int VOI_FILE = 54;

    /** extension: .xbm. */
    public static final int XBM = 55;

    /** MIPAV XML file format. mipav xml image format. extension: .xml */
    public static final int XML = 56;

    /** MIPAV XML file format. */
    public static final int XML_MULTIFILE = 57;

    /** extension: .xpm. */
    public static final int XPM = 58;

    /** extension: "par","parv2","rec","frec". */
    public static final int PARREC = 59;

    /** MIPAV Surface XML file format. extension: .xml */
    public static final int SURFACEREF_XML = 60;

    /** MINC 2.0 (HDF5) */
    public static final int MINC_HDF = 61;
    
    /** Improvision OpenLab LIFF .liff */
    /** Do not confuse with Leica image file format .lif */
    public static final int LIFF = 62;
    
    /** Arrary of strings describing the file formats. These are in synch with the above constants (same order) */
    private static String[] fileFormatStr = {
        "Undefined", "AFNI", "Analyze", "Analyze multifile", "Avi", "Bio-Rad", "BMP", "BRUKER", "Chesire",
        "Chesire Overlay", "COR", "CUR", "DIB", "DICOM", "DM3", "FITS", "GE Genesis", "GE Signa4x", "GIF", "ICO", "ICS",
        "Interfile", "JIMI", "JPEG", "LSM", "LSM multifile", "Magnetom Vision", "Map", "Medvision", "MGH", "Micro CAT",
        "MINC", "MIPAV", "MRC", "NIFTI", "NIFTI multifile", "NRRD", "OSM", "PCX", "PIC", "PICT", "PNG", "Project",
        "PSD", "QT", "Raw", "Raw multifile", "SPM", "STK", "Surface XML", "TGA", "Tiff", "Tiff multifile", "TMG", "VOI",
        "XBM", "XML", "XML multifile", "XPM", "Philips PARREC", "Surface Reference XML", "MINC 2.0", "LIFF"
    };

    /**
     * Supported File Extensions.
     *
     * <p>New file extensions should be added here and also need to be associated to a FileType (This is done in the
     * getFileTypeFromSuffix(String suffix) method)</p>
     */
    private static String[] supportedFileExtensions = new String[] {
                                                          "tif", "tiff", "lsm", "stk", "jpeg", "jpg", "bmp", "gif",
                                                          "pict", "pic", "png", "psd", "dib", "tga", "xbm", "xpm",
                                                          "xml", "pcx", "ico", "cur", "mgh", "mgz", "raw", "img", "nii",
                                                          "nhdr", "nrrd", "ima", "dcm", "bin", "map", "mnc", "avi",
                                                          "imc", "oly", "qt", "mov", "head", "brik", "ics", "ids",
                                                          "hdr", "spm", "fits", "dm3", "tmg", "mrc", "wu", "sig",
                                                          "gedno", "log", "ct", "info", "info~", "voi", "afni", "par",
                                                          "parv2", "rec", "frec", "liff"
                                                      };

    /** This map is needed in order to populate JDialogUnknownIO typeNames. */
    private static TreeMap typeNamesTreeMap;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Only for FreeSurfer COR volume files Looks in the image directory and returns all images with the same root up to
     * the hyphen, sorted in lexicographical order. Will set the number of images (<code>nImages</code>) for the calling
     * program.
     *
     * @param   fileDir   Directory to look for images.
     * @param   fileName  File name of the image.
     * @param   quiet     Whether to avoid displaying errors using the GUI.
     *
     * @return  An array of the image names to be read in or saved as.
     *
     * @throws  OutOfMemoryError  If there is a problem allocating required memory.
     */
    public static final String[] getCORFileList(String fileDir, String fileName, boolean quiet)
            throws OutOfMemoryError {
        int i;
        int j = 0;
        int k;
        int result = 0;
        String[] fileList;
        String[] fileList2;
        String[] fileListBuffer;
        String fileTemp;
        File imageDir;
        String fileName2;
        String suffix2;
        boolean okNumber;
        int nImages;

        imageDir = new File(fileDir);

        // Read directory and find no. of images
        fileListBuffer = imageDir.list();
        fileList = new String[fileListBuffer.length];

        String subName = FileUtility.trimCOR(fileName); // subName = name without indexing numbers at end

        for (i = 0; i < fileListBuffer.length; i++) {
            fileName2 = fileListBuffer[i].trim();
            suffix2 = FileUtility.getCORSuffixFrom(fileName2);
            okNumber = true;

            for (k = 1; k < suffix2.length(); k++) {

                if (!Character.isDigit(suffix2.charAt(k))) {

                    // modified to use Java.lang version 20 July 2004/parsonsd
                    // if ( suffix2.charAt( k ) < '0' || suffix2.charAt( k ) > '9' ) {
                    okNumber = false;
                }
            } // for (k = 0; k < suffix2.length(); k++)

            if (okNumber) {

                if (FileUtility.trimCOR(fileName2).equals(subName)) {
                    fileList[j] = fileListBuffer[i];
                    j++;
                }
            } // if (okNumber)
        } // for (i = 0; i < fileListBuffer.length; i++)

        // Number of images is index of last image read into fileList
        nImages = j;

        if (nImages == 0) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: No COR images with that base name: " + subName);
            }

            Preferences.debug("FileIO: No COR images with that base name: " + subName + "\n", Preferences.DEBUG_FILEIO);

            return null;
        }

        fileList2 = new String[nImages];

        for (i = 0; i < nImages; i++) {
            fileList2[i] = fileList[i];
        }

        // sort to ensure that files are in correct (lexicographical) order
        for (i = 0; i < nImages; i++) { // (bubble sort? ... )

            for (j = i + 1; j < nImages; j++) {
                result = fileList2[i].compareTo(fileList2[j]);

                if (result > 0) {
                    fileTemp = fileList2[i];
                    fileList2[i] = fileList2[j];
                    fileList2[j] = fileTemp;
                } // if (result > 0)
            } // for (j = i+1; j < nImages; j++)
        } // for (i = 0; i < nImages; i++)

        return fileList2;
    }

    /**
     * Only used for COR volume files with hyphen in name Breaks the filename into basename and suffix, then returns the
     * suffix.
     *
     * @param   fn  The filename.
     *
     * @return  The suffix or file-extension. For example, <q>-info</q>. Note that suffix includes the separator '-'
     */
    public static final String getCORSuffixFrom(String fn) {
        int s;
        String sfx = "";

        if (fn != null) {
            s = fn.lastIndexOf("-");

            if (s != -1) {
                sfx = fn.substring(s);
            }
        }

        return sfx.toLowerCase();
    }

    /**
     * Gets the file extension based on the filetype of the image. This returns the DEFAULT suffix
     *
     * @param   fileType  Type of file, found in FileBase.
     *
     * @return  The appropriate file extension.
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

            case FileUtility.MINC_HDF:
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
                suffix = ".gif";
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

            case FileUtility.PARREC:
                suffix = ".par";
                break;
            case FileUtility.LIFF:
                suffix = ".liff";
                break;
        }

        return suffix;
    }

    /**
     * Returns the extension of the file name, if file name does not have extension, then return empty string.
     *
     * @param   absolutePath  the file name.
     *
     * @return  The file's extension.
     */
    public static final String getExtension(String absolutePath) {

        if ((absolutePath == null) || (absolutePath.length() == 0)) {
            return "";
        }

        int index = absolutePath.lastIndexOf(".");

        if (index >= 0) {
            return absolutePath.substring(index);
        }

        return "";
    }

    /**
     * Returns the path information from the file name with the path information.
     *
     * @param   fileName  the file name wiht the path information.
     *
     * @return  The path information.
     */
    public static final String getFileDirectory(String fileName) {

        if ((fileName == null) || (fileName.length() == 0)) {
            return null;
        }

        int index = fileName.lastIndexOf(File.separator);

        if (index >= 0) {
            return fileName.substring(0, index + 1);
        }

        return null;
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
     * Trims off the file extension and file name, but leaves the file index. An index might be 0001, or 140, for
     * example.
     *
     * @param   fName  String file name to get index
     *
     * @return  String (index string)
     */
    public static final int getFileIndex(String fName) {
        int i;

        // char ch;
        int length = fName.lastIndexOf("."); // Start before suffix.

        for (i = length - 1; i > -1; i--) {

            if (!Character.isDigit(fName.charAt(i))) {
                break;
            }
        }

        if (i <= -1) {
            return -1;
        }

        return (new Integer(fName.substring((i + 1), length)).intValue());
    }

    /**
     * Looks in the image directory and returns all images with the same suffix as <code>fileName</code>, sorted in
     * lexicographical order.
     *
     * @param   fileDir   Directory to look for images.
     * @param   fileName  File name of the image.
     * @param   quiet     Whether to avoid displaying errors using the GUI.
     *
     * @return  An array of the image names to be read in or saved as.
     *
     * @throws  OutOfMemoryError  If there is a problem allocating required memory.
     */
    public static final String[] getFileList(String fileDir, String fileName, boolean quiet) throws OutOfMemoryError {
        int i;
        int j = 0;
        int k;
        int result = 0;
        File[] files;
        String[] fileList;
        String[] fileList2;
        String[] fileListBuffer;
        String fileTemp;
        File imageDir;
        boolean numberSuffix = true;
        String fileName2;
        String suffix2;
        boolean okNumber;
        int nImages;

        imageDir = new File(fileDir);

        // Read directory and find no. of images
        files = imageDir.listFiles();
        fileListBuffer = new String[files.length];

        for (i = 0; i < files.length; i++) {
            fileListBuffer[i] = files[i].getName();
        }

        fileList = new String[fileListBuffer.length];

        String subName = FileUtility.trimNumbersAndSpecial(fileName); // subName = name without indexing numbers at end
        String suffix = FileUtility.getExtension(fileName); // suffix  = ie. .ima or .img ...

        // System.out.println( "Suffix = _" + suffix + "_" +  " subName = " + subName);

        for (i = 1; i < suffix.length(); i++) {

            if (!Character.isDigit(suffix.charAt(i))) {

                // modified to use Java.lang check 20 July 2004/parsonsd
                // if ( suffix.charAt( i ) < '0' || suffix.charAt( i ) > '9' ) {
                numberSuffix = false;
            }
        }

        // added this check in to set number suffix to false (for no suffix'd DICOMs)
        if (suffix.equals("")) {
            numberSuffix = false;
        }

        if (numberSuffix) { // .12 is an example of a number suffix

            for (i = 0; i < fileListBuffer.length; i++) {

                if (!files[i].isDirectory()) {
                    fileName2 = fileListBuffer[i].trim();
                    suffix2 = FileUtility.getExtension(fileName2);

                    // System.out.println( "Suffix2 = _" + suffix2 + "_" );
                    okNumber = true;

                    for (k = 1; k < suffix2.length(); k++) {

                        if (!Character.isDigit(suffix2.charAt(k))) {

                            // modified to use Java.lang check 20 July 2004/parsonsd
                            // if ( suffix2.charAt( k ) < '0' || suffix2.charAt( k ) > '9' ) {
                            okNumber = false;
                        }
                    } // for (k = 0; k < suffix2.length(); k++)

                    if (okNumber && (suffix2.length() > 0)) {

                        if (FileUtility.trimNumbersAndSpecial(fileName2).equals(subName)) {
                            fileList[j] = fileListBuffer[i];
                            j++;
                        }
                    } // if (okNumber)
                } // if (!files[i].isDirectory())
            } // for (i = 0; i <fileListBuffer.length; i++)
        } // if (numberSuffix)
        else if (suffix.equals("")) {

            for (i = 0; i < fileListBuffer.length; i++) {

                if (!files[i].isDirectory()) {
                    String fileSubName = FileUtility.trimNumbersAndSpecial(fileListBuffer[i].trim());
                    String fileExtension = FileUtility.getExtension(fileListBuffer[i]);

                    if (fileSubName.trim().equals(subName) && fileExtension.equalsIgnoreCase(suffix)) {
                        fileList[j] = fileListBuffer[i];
                        j++;
                    }

                }
            }

        } else { // numberSuffix == false. I.e. ".img".

            // check to see that they end in suffix.  If so, store, count.

            for (i = 0; i < fileListBuffer.length; i++) {

                if (!files[i].isDirectory()) {

                    if (fileListBuffer[i].trim().toLowerCase().endsWith(suffix.toLowerCase())) { // note: not case
                                                                                                 // sensitive!

                        if (FileUtility.trimNumbersAndSpecial(fileListBuffer[i].trim()).equals(subName)) {
                            fileList[j] = fileListBuffer[i];
                            j++;
                        }
                    }
                }
            }
        } // numberSuffix == false

        // Number of images is index of last image read into fileList
        nImages = j;

        if (nImages == 0) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: No images with that suffix: " + suffix);
            }

            Preferences.debug("FileIO: No images with that suffix: " + suffix + "\n", Preferences.DEBUG_FILEIO);

            return null;
        }

        fileList2 = new String[nImages];

        for (i = 0; i < nImages; i++) {
            fileList2[i] = fileList[i];
        }

        // sort to ensure that files are in correct (lexicographical) order
        for (i = 0; i < nImages; i++) { // (bubble sort? ... )

            for (j = i + 1; j < nImages; j++) {
                result = FilenameSorter.compareToLastNumericalSequence(fileList2[i], fileList2[j]); // compare based on
                                                                                                    // last numerical
                                                                                                    // sequence
                                                                                                    // result =
                                                                                                    // fileList2[i].compareTo(
                                                                                                    // fileList2[j] );

                if (result > 0) {
                    fileTemp = fileList2[i];
                    fileList2[i] = fileList2[j];
                    fileList2[j] = fileTemp;
                } // end of if (result > 0)
            } // end of for (j = i+1; j < nImages; j++)
        } // end of for (i = 0; i < nImages; i++)

        return fileList2;
    }

    /**
     * Returns the file name without path information from file name with the path information.
     *
     * @param   absolutePath  the file name with the path information.
     *
     * @return  The file name without path information.
     */
    public static final String getFileName(String absolutePath) {

        if ((absolutePath == null) || (absolutePath.length() == 0)) {
            return null;
        }

        int index = absolutePath.lastIndexOf(File.separator);

        if (index >= 0) {

            if (index == (absolutePath.length() - 1)) {
                return null;
            }

            return absolutePath.substring(index + 1);
        }

        return absolutePath;
    }

    /**
     * Sets the FileBase.(filetype) based on the file extension of the given filename. Also sets file "suffix", if
     * required.
     *
     * @param   fileName  Filename of the image to read in. Must include the file extension.
     * @param   fileDir   Directory where fileName exists.
     * @param   quiet     Whether to avoid any user interaction (ie, from error popups).
     *
     * @return  Filetype from FileBase.
     *
     * @see     FileBase
     */
    public static final int getFileType(String fileName, String fileDir, boolean quiet) {
        return getFileType(fileName, fileDir, false, quiet);
    }

    /**
     * Sets the FileBase.(filetype) based on the file extension of the given filename. Also sets file "suffix", if
     * required.
     *
     * @param   fileName  Filename of the image to read in. Must include the file extension.
     * @param   fileDir   Directory where fileName exists.
     * @param   doWrite   If true about to write a file
     * @param   quiet     Whether to avoid any user interaction (ie, from error popups).
     *
     * @return  Filetype from FileBase.
     *
     * @see     FileBase
     */
    public static final int getFileType(String fileName, String fileDir, boolean doWrite, boolean quiet) {
        int fileType;
        int i;

        String beginString = FileUtility.stripExtension(fileName);

        if ((beginString.equalsIgnoreCase("d3proc")) || (beginString.equalsIgnoreCase("reco")) ||
                (beginString.equalsIgnoreCase("2dseq"))) {
            fileType = FileUtility.BRUKER;

            return fileType;
        }

        fileName.trim();

        String suffix = FileUtility.getExtension(fileName);

        fileType = FileUtility.getFileTypeFromSuffix(suffix);
        
        // handle when the .mnc extension but MINC_HDF file
        if (fileType == FileUtility.MINC) {
            try {
        	// inspect the file to see if it is really a MINC1 (suppressing any error dialogs).
        	// if not, set the file type using isMincHDF()
        	if (isMinc(fileName, fileDir, true) != FileUtility.MINC) {
        	    fileType = isMincHDF(fileName, fileDir, quiet);
        	}
            } catch (IOException ioe) {
        	if (ioe instanceof FileNotFoundException) {
        	    MipavUtil.displayError("File does not exist '" + fileDir + fileName + "'.");
        	    ioe.printStackTrace();

        	    return FileUtility.ERROR;
        	}

        	if (!quiet) {
        	    MipavUtil.displayError("FileIO: " + ioe);
        	    Preferences.debug("FileIO: " + ioe + "\n", Preferences.DEBUG_FILEIO);
        	    ioe.printStackTrace();
        	} else {
        	    Preferences.debug("FileIO: " + ioe + "\n", Preferences.DEBUG_FILEIO);
        	    ioe.printStackTrace();
        	}

        	fileType = FileUtility.UNDEFINED;
            }
        }
        
        if (fileType == FileUtility.UNDEFINED) {

            if (suffix.equalsIgnoreCase(".pic")) {

                // Both Biorad and JIMI use the pic suffix
                try {
                    File file = new File(fileDir + fileName);
                    RandomAccessFile raFile = new RandomAccessFile(file, "r");

                    raFile.seek(54L);

                    // little endian unsigned short
                    int b1 = raFile.readUnsignedByte();
                    int b2 = raFile.readUnsignedByte();
                    int fileID = ((b2 << 8) | b1); // Little Endian

                    raFile.close();

                    if (fileID == 12345) {
                        fileType = FileUtility.BIORAD;
                    } else {
                        fileType = FileUtility.JIMI;
                    }
                } catch (OutOfMemoryError error) {
                    System.gc();
                } catch (FileNotFoundException e) {
                    System.gc();
                } catch (IOException e) {
                    System.gc();
                }
            } else if (suffix.equalsIgnoreCase(".img")) {

                // ANALYZE, Interfile, and NIFTI use .img and .hdr
                if (doWrite) {
                    JDialogAnalyzeNIFTIChoice choice = new JDialogAnalyzeNIFTIChoice(ViewUserInterface.getReference().getMainFrame());

                    if (!choice.okayPressed()) {
                        fileType = FileUtility.ERROR;
                    } 
                    else {
                        fileType = choice.fileType();
                    }
                } else { // read

                    int p = fileName.lastIndexOf(".");
                    String fileHeaderName = fileName.substring(0, p + 1) + "hdr";
                    String headerFile = FileInterfile.isInterfile(fileHeaderName, fileDir);
                    if (headerFile != null) {
                        fileType = FileUtility.INTERFILE;
                    }
                    else {
                        fileType = FileUtility.ANALYZE;
    

                        try {
                            File file = new File(fileDir + fileHeaderName);
                            RandomAccessFile raFile = new RandomAccessFile(file, "r");
    
                            raFile.seek(344L);
    
                            char[] niftiName = new char[4];
    
                            for (i = 0; i < 4; i++) {
                                niftiName[i] = (char) raFile.readUnsignedByte();
                            }
    
                            raFile.close();
    
                            if ((niftiName[0] == 'n') && ((niftiName[1] == 'i') || (niftiName[1] == '+')) &&
                                    (niftiName[2] == '1') && (niftiName[3] == '\0')) {
                                fileType = FileUtility.NIFTI;
                            }
                        } catch (OutOfMemoryError error) {
                            System.gc();
                        } catch (FileNotFoundException e) {
                            System.gc();
                        } catch (IOException e) {
                            System.gc();
                        }
                    }
                }
            } else if (suffix.equalsIgnoreCase(".hdr")) {
                if (doWrite) {
                    // ANALYZE, Interfile, and NIFTI use .img and .hdr
                    JDialogAnalyzeNIFTIChoice choice = new JDialogAnalyzeNIFTIChoice(ViewUserInterface.getReference().getMainFrame());

                    if (!choice.okayPressed()) {
                        fileType = FileUtility.ERROR;
                    } 
                    else {
                        fileType = choice.fileType();
                    }
                } else { // read
                    String headerFile = FileInterfile.isInterfile(fileName, fileDir);
                    if (headerFile != null) {
                        fileType = FileUtility.INTERFILE;
                    }
                    else {
                        fileType = FileUtility.ANALYZE;
    
                        try {
                            File file = new File(fileDir + fileName);
                            RandomAccessFile raFile = new RandomAccessFile(file, "r");
    
                            raFile.seek(344L);
    
                            char[] niftiName = new char[4];
    
                            for (i = 0; i < 4; i++) {
                                niftiName[i] = (char) raFile.readUnsignedByte();
                            }
    
                            raFile.close();
    
                            if ((niftiName[0] == 'n') && ((niftiName[1] == 'i') || (niftiName[1] == '+')) &&
                                    (niftiName[2] == '1') && (niftiName[3] == '\0')) {
                                fileType = FileUtility.NIFTI;
                            }
                        } catch (OutOfMemoryError error) {
                            System.gc();
                        } catch (FileNotFoundException e) {
                            System.gc();
                        } catch (IOException e) {
                            System.gc();
                        }    
                    }
                }
            } else if (suffix.equalsIgnoreCase(".ima")) {

                // Both Dicom and Siemens Magnetom Vision file type have the ima suffix
                try {
                    File file = new File(fileDir + fileName);
                    RandomAccessFile raFile = new RandomAccessFile(file, "r");

                    raFile.seek(281L);

                    char[] ModelName = new char[15];

                    for (i = 0; i < 15; i++) {
                        ModelName[i] = (char) raFile.readUnsignedByte();
                    }

                    raFile.close();

                    String ModelNameString = new String(ModelName);

                    if (ModelNameString.equals("MAGNETOM VISION")) {
                        fileType = FileUtility.MAGNETOM_VISION;
                    } else {
                        fileType = FileUtility.DICOM;
                    }
                } catch (OutOfMemoryError error) {
                    System.gc();
                } catch (FileNotFoundException e) {
                    System.gc();
                } catch (IOException e) {
                    System.gc();
                }
            } else if (suffix.equalsIgnoreCase("") && doWrite) {
                Preferences.debug("FileIO: Cannot save a file without an extension: " + fileName + "\n",
                                  Preferences.DEBUG_FILEIO);

                return FileUtility.UNDEFINED;
            }
        }

        String strFileType = Preferences.getProperty(Preferences.PREF_USER_FILETYPE_ASSOC);
        String[] assoc = new String[0];

        if (strFileType != null) {
            assoc = strFileType.split(Preferences.ITEM_SEPARATOR);
        }

        // check to see if there are any user defined associations
        for (int k = 0; k < assoc.length; k++) {

            if (suffix.equals(assoc[k].split(Preferences.DEFINITION_SEPARATOR)[0])) {
                fileType = new Integer(assoc[k].split(Preferences.DEFINITION_SEPARATOR)[1]).intValue();
            }
        }

        try {

            if (fileType == FileUtility.UNDEFINED) {
                fileType = FileUtility.isDicom(fileName, fileDir, quiet);
            }

            if (fileType == FileUtility.UNDEFINED) {
                fileType = FileUtility.isGESigna4X(fileName, fileDir, quiet);
            }

            if (fileType == FileUtility.UNDEFINED) {
                fileType = FileUtility.isGESigna5X(fileName, fileDir, quiet);
            }

            if (fileType == FileUtility.UNDEFINED) {
                fileType = FileUtility.isMagnetomVision(fileName, fileDir, quiet);
            }

            if (fileType == FileUtility.UNDEFINED) {
                fileType = FileUtility.isMinc(fileName, fileDir, quiet);
            }
            
            if (fileType == FileUtility.UNDEFINED) {
                fileType = isMincHDF(fileName, fileDir, quiet);
            }
            
            if (fileType == FileUtility.UNDEFINED) {

                fileType = FileUtility.isAnalyze(fileName, fileDir, quiet);
            }
            
            if (fileType == FileUtility.UNDEFINED) {
                fileType = FileUtility.isInterfile(fileName, fileDir, quiet);
            }

            if (fileType == FileUtility.UNDEFINED) {
                fileType = FileUtility.isNIFTI(fileName, fileDir, quiet);
            }

            if (fileType == FileUtility.UNDEFINED) {
                fileType = FileUtility.isSPM(fileName, fileDir, quiet);
            }
        } catch (IOException ioe) {

            if (ioe instanceof FileNotFoundException) {
                MipavUtil.displayError("File does not exist '" + fileDir + fileName + "'.");
                ioe.printStackTrace();

                return FileUtility.ERROR;
            }

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + ioe);
                Preferences.debug("FileIO: " + ioe + "\n", Preferences.DEBUG_FILEIO);
                ioe.printStackTrace();
            } else {
                Preferences.debug("FileIO: " + ioe + "\n", Preferences.DEBUG_FILEIO);
                ioe.printStackTrace();
            }

            fileType = FileUtility.UNDEFINED;
        }

        return fileType;
    }


    /**
     * Gets the file type based upon the suffix.
     *
     * @param   suffix  the suffix
     *
     * @return  The file type for the given suffix (or UNDEFINED if the suffix is null or unrecognized).
     */
    public static int getFileTypeFromSuffix(String suffix) {
        int fileType = FileUtility.UNDEFINED;

        if (suffix == null) {
            return FileUtility.UNDEFINED;
        }

        if (suffix.equalsIgnoreCase(".tif")) {
            fileType = FileUtility.TIFF;
        } else if (suffix.equalsIgnoreCase(".tiff")) {
            fileType = FileUtility.TIFF;
        } else if (suffix.equalsIgnoreCase(".lsm")) {
            fileType = FileUtility.LSM;
        } else if (suffix.equalsIgnoreCase(".stk")) {
            fileType = FileUtility.STK;
        } else if (suffix.equalsIgnoreCase(".jpeg")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".jpg")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".bmp")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".gif")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".pict")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".pic")) {

            // Both Biorad and JIMI use the pic suffix
            fileType = FileUtility.UNDEFINED;
        } else if (suffix.equalsIgnoreCase(".png")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".psd")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".dib")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".tga")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".xbm")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".xpm")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".xml")) {
            fileType = FileUtility.XML;
        } else if (suffix.equalsIgnoreCase(".pcx")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".ico")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".cur")) {
            fileType = FileUtility.JIMI;
        } else if (suffix.equalsIgnoreCase(".mgh")) {
            fileType = FileUtility.MGH;
        } else if (suffix.equalsIgnoreCase(".mgz")) {
            fileType = FileUtility.MGH;
        } else if (suffix.equalsIgnoreCase(".raw")) {
            fileType = FileUtility.RAW;
        } else if (suffix.equalsIgnoreCase(".img")) {

            // Both ANALYZE and NIFTI use .img and .hdr
            fileType = FileUtility.UNDEFINED;
        } else if (suffix.equalsIgnoreCase(".nii")) {
            fileType = FileUtility.NIFTI;
        } else if (suffix.equalsIgnoreCase(".nhdr")) {
            fileType = FileUtility.NRRD;
        } else if (suffix.equalsIgnoreCase(".nrrd")) {
            fileType = FileUtility.NRRD;
        } else if (suffix.equalsIgnoreCase(".ima")) {

            // Both Dicom and Siemens Magnetom Vision file type have the ima suffix
            fileType = FileUtility.UNDEFINED;
        } else if (suffix.equalsIgnoreCase(".dcm")) {
            fileType = FileUtility.DICOM;
        } else if (suffix.equalsIgnoreCase(".bin")) {
            fileType = FileUtility.MEDIVISION;
        } else if (suffix.equalsIgnoreCase(".map")) {
            fileType = FileUtility.MAP;
        }
        // Benes Trus special
        else if (suffix.equalsIgnoreCase(".mnc")) {
            fileType = FileUtility.MINC;
        } else if (suffix.equalsIgnoreCase(".avi")) {
            fileType = FileUtility.AVI;
        } else if (suffix.equalsIgnoreCase(".imc")) {
            fileType = FileUtility.CHESHIRE;
        } else if (suffix.equalsIgnoreCase(".oly")) {
            fileType = FileUtility.CHESHIRE_OVERLAY;
        }
        // QuickTime on WIndows uses .QT and on MAC uses .mov
        else if (suffix.equalsIgnoreCase(".qt")) {
            fileType = FileUtility.QT;
        } else if (suffix.equalsIgnoreCase(".mov")) {
            fileType = FileUtility.QT;
        } else if (suffix.equalsIgnoreCase(".head")) {
            fileType = FileUtility.AFNI;
        } else if (suffix.equalsIgnoreCase(".brik")) {
            fileType = FileUtility.AFNI;
        } else if (suffix.equalsIgnoreCase(".ics")) {
            fileType = FileUtility.ICS;
        } else if (suffix.equalsIgnoreCase(".ids")) {
            fileType = FileUtility.ICS;
        } else if (suffix.equalsIgnoreCase(".hdr")) {
            // .hdr found in ANALYZE, INTERFILE, and NIFTI
            fileType = FileUtility.UNDEFINED;
        } else if (suffix.equalsIgnoreCase(".spm")) {
            fileType = FileUtility.SPM;
        } else if (suffix.equalsIgnoreCase(".fits")) {
            fileType = FileUtility.FITS;
        } else if (suffix.equalsIgnoreCase(".dm3")) {
            fileType = FileUtility.DM3;
        } else if (suffix.equalsIgnoreCase(".tmg")) {
            fileType = FileUtility.TMG;
        } else if (suffix.equalsIgnoreCase(".mrc")) {
            fileType = FileUtility.MRC;
        } else if (suffix.equalsIgnoreCase(".wu")) {
            fileType = FileUtility.OSM;
        } else if (suffix.equalsIgnoreCase(".sig")) {
            fileType = FileUtility.GE_GENESIS;
        } else if (suffix.equalsIgnoreCase(".gedno")) {
            fileType = FileUtility.GE_SIGNA4X;
        } else if (suffix.equalsIgnoreCase(".log")) {
            fileType = FileUtility.MICRO_CAT;
        } else if (suffix.equalsIgnoreCase(".ct")) {
            fileType = FileUtility.MICRO_CAT;
        } else if (suffix.equalsIgnoreCase(".info")) {
            fileType = FileUtility.COR;
        } else if (suffix.equalsIgnoreCase(".info~")) {
            fileType = FileUtility.COR;
        } else if (suffix.equalsIgnoreCase(".voi")) {
            fileType = FileUtility.VOI_FILE;
        } else if (suffix.equalsIgnoreCase(".afni")) {
            fileType = FileUtility.AFNI;
        } else if (suffix.equalsIgnoreCase(".par")) { /*for PAR/REC Supported*/
            fileType = FileUtility.PARREC;
        } else if (suffix.equalsIgnoreCase(".parv2")) {
            fileType = FileUtility.PARREC;
        } else if (suffix.equalsIgnoreCase(".rec")) {
            fileType = FileUtility.PARREC;
        } else if (suffix.equalsIgnoreCase(".frec")) { /*for PAR/REC Supported*/
            fileType = FileUtility.PARREC;
        } else if (suffix.equalsIgnoreCase(".liff")) {
            fileType = FileUtility.LIFF;
        } else {

            // cannot automatically determine the filetype from the filename extension
            fileType = FileUtility.UNDEFINED;
        }

        return fileType;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static String[] getSupportedFileExtensions() {
        return supportedFileExtensions;
    }


    /**
     * This method is called by JDialogUnknownIO in order to populate its typeInts.
     *
     * @return  String[] Array of typeInts
     */
    public static int[] getUnknownDialogsTypeInts() {
        String[] exts = getUnknownDialogsTypeSuffices();
        int[] fileTypes = new int[exts.length];

        for (int i = 0; i < exts.length; i++) {
            String suffix = exts[i];
            int fileType = getFileTypeFromSuffix(suffix);

            if (exts[i].equals("pic")) {
                fileType = FileUtility.BIORAD;
            } else if (exts[i].equals("img")) {
                fileType = FileUtility.NIFTI;
            } else if (exts[i].equals("ima")) {
                fileType = FileUtility.MAGNETOM_VISION;
            }

            fileTypes[i] = fileType;
        }

        return fileTypes;
    }


    /**
     * This method is called by JDialogUnknownIO in order to populate its typeNames.
     *
     * @return  Strin[] Array of descriptions
     */
    public static String[] getUnknownDialogsTypeNames() {
        typeNamesTreeMap = new TreeMap();

        String description = "";

        for (int i = 0; i < supportedFileExtensions.length; i++) {
            int fileType = getFileTypeFromSuffix("." + supportedFileExtensions[i]);

            if (fileType == FileUtility.JIMI) {

                if (supportedFileExtensions[i].equals("jpeg") || (supportedFileExtensions[i].equals("jpg"))) {
                    description = fileFormatStr[FileUtility.JPEG];
                } else if (supportedFileExtensions[i].equals("bmp")) {
                    description = fileFormatStr[FileUtility.BMP];
                } else if (supportedFileExtensions[i].equals("gif")) {
                    description = fileFormatStr[FileUtility.GIF];
                } else if (supportedFileExtensions[i].equals("pict")) {
                    description = fileFormatStr[FileUtility.PICT];
                } else if (supportedFileExtensions[i].equals("png")) {
                    description = fileFormatStr[FileUtility.PNG];
                } else if (supportedFileExtensions[i].equals("psd")) {
                    description = fileFormatStr[FileUtility.PSD];
                } else if (supportedFileExtensions[i].equals("dib")) {
                    description = fileFormatStr[FileUtility.DIB];
                } else if (supportedFileExtensions[i].equals("tga")) {
                    description = fileFormatStr[FileUtility.TGA];
                } else if (supportedFileExtensions[i].equals("xbm")) {
                    description = fileFormatStr[FileUtility.XBM];
                } else if (supportedFileExtensions[i].equals("xpm")) {
                    description = fileFormatStr[FileUtility.XPM];
                } else if (supportedFileExtensions[i].equals("pcx")) {
                    description = fileFormatStr[FileUtility.PCX];
                } else if (supportedFileExtensions[i].equals("ico")) {
                    description = fileFormatStr[FileUtility.ICO];
                } else if (supportedFileExtensions[i].equals("cur")) {
                    description = fileFormatStr[FileUtility.CUR];
                }
            } else if (fileType == FileUtility.UNDEFINED) {

                if (supportedFileExtensions[i].equals("pic")) {
                    description = fileFormatStr[FileUtility.BIORAD];
                }

                // *.ima and *.img each go to 2 diff file types
                if (supportedFileExtensions[i].equals("ima")) {
                    description = fileFormatStr[FileUtility.DICOM];

                    if (typeNamesTreeMap.containsKey(description)) {
                        String value = (String) typeNamesTreeMap.get(description);
                        typeNamesTreeMap.put(description, value + ",." + supportedFileExtensions[i]);
                    } else {
                        typeNamesTreeMap.put(description, "." + supportedFileExtensions[i]);
                    }

                    description = fileFormatStr[FileUtility.MAGNETOM_VISION];

                    if (typeNamesTreeMap.containsKey(description)) {
                        String value = (String) typeNamesTreeMap.get(description);
                        typeNamesTreeMap.put(description, value + ",." + supportedFileExtensions[i]);
                    } else {
                        typeNamesTreeMap.put(description, "." + supportedFileExtensions[i]);
                    }

                    continue;
                }

                if (supportedFileExtensions[i].equals("img")) {
                    description = fileFormatStr[FileUtility.ANALYZE];

                    if (typeNamesTreeMap.containsKey(description)) {
                        String value = (String) typeNamesTreeMap.get(description);
                        typeNamesTreeMap.put(description, value + ", ." + supportedFileExtensions[i]);
                    } else {
                        typeNamesTreeMap.put(description, "." + supportedFileExtensions[i]);
                    }

                    description = fileFormatStr[FileUtility.NIFTI];

                    if (typeNamesTreeMap.containsKey(description)) {
                        String value = (String) typeNamesTreeMap.get(description);
                        typeNamesTreeMap.put(description, value + ", ." + supportedFileExtensions[i]);
                    } else {
                        typeNamesTreeMap.put(description, "." + supportedFileExtensions[i]);
                    }

                    continue;
                }
            } else {
                description = fileFormatStr[fileType];
            }

            if (typeNamesTreeMap.containsKey(description)) {
                String value = (String) typeNamesTreeMap.get(description);
                typeNamesTreeMap.put(description, value + ", ." + supportedFileExtensions[i]);
            } else {
                typeNamesTreeMap.put(description, "." + supportedFileExtensions[i]);
            }
        }


        ArrayList descripArrayList = new ArrayList();

        for (Iterator iter = typeNamesTreeMap.keySet().iterator(); iter.hasNext();) {
            String key = (String) iter.next();
            String value = (String) typeNamesTreeMap.get(key);
            String descrip = key + " (" + value + ")";
            descripArrayList.add(descrip);
        }


        String[] descripArray = new String[descripArrayList.size()];

        for (int i = 0; i < descripArrayList.size(); i++) {
            descripArray[i] = (String) descripArrayList.get(i);
        }

        return descripArray;
    }


    /**
     * This method is called by JDialogUnknownIO in order to populate its typeSuffices.
     *
     * @return  String[] Array of suffices
     */
    public static String[] getUnknownDialogsTypeSuffices() {
        ArrayList typeSufficesAL = new ArrayList();

        for (Iterator iter = typeNamesTreeMap.keySet().iterator(); iter.hasNext();) {
            String key = (String) iter.next();

            // since the fileFormetStr array ant the static final ints are in synch, getting the position
            // in the array is the same as getting the file type...so that we can get the default suffix
            for (int i = 0; i < fileFormatStr.length; i++) {

                if (key.equals(fileFormatStr[i])) {
                    String suff = getDefaultSuffix(i);
                    typeSufficesAL.add(suff);
                }
            }
        }

        String[] typeSuffices = new String[typeSufficesAL.size()];

        for (int i = 0; i < typeSufficesAL.size(); i++) {
            typeSuffices[i] = (String) typeSufficesAL.get(i);
        }

        return typeSuffices;

    }

    /**
     * Tests if the unknown file is of type Analyze.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   quiet     Whether to avoid any user interaction (ie, from error popups).
     *
     * @return  <code>FileUtility.ANALYZE</code> if the file is a ANALYZE type, and <code>FileUtility.UNDEFINED</code>
     *          otherwise
     *
     * @throws  IOException  If there is a problem determining the type of the given file.
     */
    public static final int isAnalyze(String fileName, String fileDir, boolean quiet) throws IOException {

        try {

            boolean isAnalyze = FileAnalyze.isAnalyze(fileDir + fileName);

            if (isAnalyze) {
                return FileUtility.ANALYZE;
            }

            return FileUtility.UNDEFINED;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            return FileUtility.UNDEFINED;
        }
    }

    /**
     * Tests if the unknown file is of type Dicom.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   quiet     Whether to avoid any user interaction (ie, from error popups).
     *
     * @return  <code>FileUtility.DICOM</code> if the file is a DICOM file, and <code>FileUtility.UNDEFINED</code>
     *          otherwise
     *
     * @throws  IOException  If there is a problem determining the type of the given file.
     */
    public static final int isDicom(String fileName, String fileDir, boolean quiet) throws IOException {

        try {
            FileDicom imageFile = new FileDicom(fileName, fileDir);

            if (imageFile != null) {
                boolean isDicom = imageFile.isDICOM();

                imageFile.close();
                imageFile = null;

                if (isDicom) {
                    return FileUtility.DICOM;
                }
            }

            return FileUtility.UNDEFINED;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            return FileUtility.UNDEFINED;
        }

    }

    /**
     * Tests if the unknown file is of type GE Signa 4X type.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   quiet     Whether to avoid any user interaction (ie, from error popups).
     *
     * @return  <code>FileUtility.GE_SIGNA4X</code> if the file is a GE MR Signa 4.x file, and <code>
     *          FileUtility.UNDEFINED</code> otherwise
     *
     * @throws  IOException  If there is a problem determining the type of the given file.
     */
    public static final int isGESigna4X(String fileName, String fileDir, boolean quiet) throws IOException {

        try {
            FileGESigna4X imageFile = new FileGESigna4X(fileName, fileDir);

            if (imageFile != null) {
                boolean isGESigna4X = imageFile.isGESigna4X();

                if (isGESigna4X) {
                    return FileUtility.GE_SIGNA4X;
                }
            }

            return FileUtility.UNDEFINED;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            return FileUtility.UNDEFINED;
        }
    }

    /**
     * Tests if the unknown file is of type GE Signa 5X type.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   quiet     Whether to avoid any user interaction (ie, from error popups).
     *
     * @return  <code>FileUtility.GE_GENESIS</code> if the file is a GE MR Signa 5.x file, and <code>
     *          FileUtility.UNDEFINED</code> otherwise
     *
     * @throws  IOException  If there is a problem determining the type of the given file.
     */
    public static final int isGESigna5X(String fileName, String fileDir, boolean quiet) throws IOException {

        try {
            FileGESigna5X imageFile = new FileGESigna5X(fileName, fileDir);

            if (imageFile != null) {
                boolean isGESigna5X = imageFile.isGESigna5X();

                if (isGESigna5X) {
                    return FileUtility.GE_GENESIS;
                }
            }

            return FileUtility.UNDEFINED;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            return FileUtility.UNDEFINED;
        }
    }
    
    /**
     * Tests if the unknown file is of type Interfile.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   quiet     Whether to avoid any user interaction (ie, from error popups).
     *
     * @return  <code>FileUtility.Interfile</code> if the file is a Interfile type, and <code>FileUtility.UNDEFINED</code>
     *          otherwise
     *
     * @throws  IOException  If there is a problem determining the type of the given file.
     */
    public static final int isInterfile(String fileName, String fileDir, boolean quiet) throws IOException {

        try {

            String fileHeaderName = FileInterfile.isInterfile(fileName, fileDir);

            if (fileHeaderName != null) {
                return FileUtility.INTERFILE;
            }

            return FileUtility.UNDEFINED;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            return FileUtility.UNDEFINED;
        }
    }

    /**
     * Tests if the unknown file is of type Siemens Magnetom Vision.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   quiet     Whether to avoid any user interaction (ie, from error popups).
     *
     * @return  <code>FileUtility.MAGNETOM_VISION</code> if the file is a Siemens Magnetom Vision type, and <code>
     *          FileUtility.UNDEFINED</code> otherwise
     *
     * @throws  IOException  If there is a problem determining the type of the given file.
     */
    public static final int isMagnetomVision(String fileName, String fileDir, boolean quiet) throws IOException {

        try {
            FileMagnetomVision imageFile = new FileMagnetomVision(fileName, fileDir);

            if (imageFile != null) {
                boolean isMagnetomVision = imageFile.isMagnetomVision();

                if (isMagnetomVision) {
                    return FileUtility.MAGNETOM_VISION;
                }
            }

            return FileUtility.UNDEFINED;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            return FileUtility.UNDEFINED;
        }
    }

    /**
     * Tests if the unknown file is of type Minc.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   quiet     Whether to avoid any user interaction (ie, from error popups).
     *
     * @return  <code>FileUtility.MINC</code> if the file is a MINC type, and <code>FileUtility.UNDEFINED</code>
     *          otherwise
     *
     * @throws  IOException  If there is a problem determining the type of the given file.
     */
    public static final int isMinc(String fileName, String fileDir, boolean quiet) throws IOException {

        try {
            FileMinc imageFile = new FileMinc(fileName, fileDir);

            if (imageFile != null) {
                boolean isMinc = imageFile.isMinc();

                if (isMinc) {
                    return FileUtility.MINC;
                }
            }

            return FileUtility.UNDEFINED;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            return FileUtility.UNDEFINED;
        }
    }

    /**
     * Determines whether the file on disk is of type MINC 2.0
     * @param fileName name of the file
     * @param fileDir directory
     * @param quiet
     * @return whether the file is HDF5 type
     */
    public static final int isMincHDF(String fileName, String fileDir, boolean quiet) {
    	
	// first, make sure that the hdf5 libs are available (no pre-built ones for win64)
	try {
	    System.loadLibrary("jhdf");
	    System.loadLibrary("jhdf5");
	} catch (SecurityException e) {
	    if (!quiet) {
		MipavUtil.displayError("Unable to load HDF libraries: " + e.getMessage());
	    }
	    
	    e.printStackTrace();
	    
	    return FileUtility.ERROR;
	} catch (UnsatisfiedLinkError e) {
	    if (!quiet) {
		MipavUtil.displayError("Unable to load HDF libraries: " + e.getMessage());
	    }
	    
	    e.printStackTrace();
	    
	    return FileUtility.ERROR;
	}
	
    	FileFormat h5F = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);
    	
    	boolean isMincHDF = h5F.isThisType(fileDir + File.separator + fileName);
    	if (isMincHDF) {
    		return FileUtility.MINC_HDF;
    	} else {
    		return FileUtility.UNDEFINED;
    	}
    	
    }
    /**
     * Tests if the unknown file is of type Nifti.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   quiet     Whether to avoid any user interaction (ie, from error popups).
     *
     * @return  <code>FileUtility.NIFTI</code> if the file is a NIFTI type, and <code>FileUtility.UNDEFINED</code>
     *          otherwise
     *
     * @throws  IOException  If there is a problem determining the type of the given file.
     */
    public static final int isNIFTI(String fileName, String fileDir, boolean quiet) throws IOException {


        try {
            boolean isNIFTI = FileNIFTI.isNIFTI(fileName, fileDir);

            if (isNIFTI) {
                return FileUtility.NIFTI;
            }

            return FileUtility.UNDEFINED;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            return FileUtility.UNDEFINED;
        }
    }

    /**
     * Tests if the unknown file is of type SPM.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   quiet     Whether to avoid any user interaction (ie, from error popups).
     *
     * @return  <code>FileUtility.SPM</code> if the file is a SPM type, and <code>FileUtility.UNDEFINED</code> otherwise
     *
     * @throws  IOException  If there is a problem determining the type of the given file.
     */
    public static final int isSPM(String fileName, String fileDir, boolean quiet) throws IOException {

        try {
            boolean isSPM = FileSPM.isSPM(fileDir + fileName);

            if (isSPM) {
                return FileUtility.SPM;
            }

            return FileUtility.UNDEFINED;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            return FileUtility.UNDEFINED;
        }
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
     * Trims the numbers or file extension from COR file names. Any numbers or <q>.info</q> or <q>.info~</q> will be
     * removed from after a hyphen in the given fname.
     *
     * @param   fName  File name where the last characters are alpha-numerics indicating the image number or .info or
     *                 .info~
     *
     * @return  File name without numbers on the end.
     */
    public static final String trimCOR(String fName) {
        int length = fName.lastIndexOf("-");

        if (length >= 0) {
            return (new String(fName.substring(0, length + 1)));
        } else {
            return null;
        }
    }

    /**
     * Trims the numbers and special character from the file name. Numerics and some special characters <code>[ - _
     * .</code> are removed from the end of the file.
     *
     * @param   fName  File name where the last characters are alpha-numerics indicating the image number.
     *
     * @return  File name without numbers on the end.
     */
    public static final String trimNumbersAndSpecial(String fName) {
        int i;
        char ch;
        int length = fName.lastIndexOf("."); // Start before suffix.

        for (i = length - 1; i > -1; i--) {
            ch = fName.charAt(i);

            if (!Character.isDigit(ch) && (ch != '-') && (ch != '.') && (ch != '_')) {
                break;
            }
        }

        String tmpStr;

        if (length == -1) {
            tmpStr = fName;
        } else {
            tmpStr = fName.substring(0, i + 1);
        }

        boolean aCharIsPresent = false;

        // Determine if at least one letter is present
        for (i = 0; i < tmpStr.length(); i++) {
            ch = tmpStr.charAt(i);

            if (Character.isLetter(ch)) {
                aCharIsPresent = true;

                break;
            }
        }

        char fillChar = 'a';

        // If yes, then remove remaining numbers
        if (aCharIsPresent) {

            for (i = 0; i < tmpStr.length(); i++) {
                ch = tmpStr.charAt(i);

                if (Character.isDigit(ch)) {
                    tmpStr = tmpStr.replace(ch, fillChar);
                }
            }
        }

        return (tmpStr);
    }
}
