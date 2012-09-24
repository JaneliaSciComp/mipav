package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.*;

import ncsa.hdf.object.FileFormat;


/**
 * Constants and static methods which relate to file input, output or processing.
 */
public class FileUtility {

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
    
    /** Bmp multifile */
    public static final int BMP_MULTIFILE = 7;

    /**
     * Bruker file format. Reads a BRUKER file by first reading in the d3proc header file, second the reco header file,
     * third the acqp file int the same directory or up one or two two parent directories, and finally the 2dseq binary
     * file.
     */
    public static final int BRUKER = 8;

    /**
     * Cheshire file type (a kind of Analyze). extension: .imc Can also have .img extension
     */
    public static final int CHESHIRE = 9;

    /** Cheshire overlay file type. Contains VOIs. extension: .oly */
    public static final int CHESHIRE_OVERLAY = 10;

    /**
     * Used by FreeSurfer software. extension: -.info or -.info~ for header file -.nnn for slice data file where nnn is
     * the slice number
     */
    public static final int COR = 11;

    /** extension: .cur. */
    public static final int CUR = 12;

    /** extension: .dib. */
    public static final int DIB = 13;

    /** Digital Imaging and COmmunications in Medicine file type. Fully implemented versions 2 & 3. extension: .dcm */
    public static final int DICOM = 14;

    /** Gatan's Digital Micrograph version 3 file format. extension: .dm3 */
    public static final int DM3 = 15;

    /** FITS file type. extension: .fits */
    public static final int FITS = 16;

    /** GE Genesis 5X and LX. extension: .sig */
    public static final int GE_GENESIS = 17;

    /** Multiple files of type GE_GENESIS */
    public static final int GE_GENESIS_MULTIFILE = 18;

    /** GE Signa 4.x. */
    public static final int GE_SIGNA4X = 19;

    /** Multiple files of type GE_SIGNA4X */
    public static final int GE_SIGNA4X_MULTIFILE = 20;

    /** extension: .gif. */
    public static final int GIF = 21;

    /** extension: .ico. */
    public static final int ICO = 22;

    /** Image Cytometry Standard. extension: .ics, .ids */
    public static final int ICS = 23;

    /** Interfile file format used in Nuclear Medicine. extension: .hdr */
    public static final int INTERFILE = 24;

    /** Java Image Manangement Interface file type. */
    public static final int JIMI = 25;

    /** extension: .jpeg, .jpg. */
    public static final int JPEG = 26;

    /** Used by the Zeiss LSM 510 Dataserver. extension: .lsm */
    public static final int LSM = 27;

    /** Used by the Zeiss LSM 510 Dataserver. */
    public static final int LSM_MULTIFILE = 28;

    /** Siemens MAGNETOM VISION. extension: .ima */
    public static final int MAGNETOM_VISION = 29;

    /** Multiple files of type MAGNETOM_VISION */
    public static final int MAGNETOM_VISION_MULTIFILE = 30;

    /** Benes Trus special file type. extension: .map */
    public static final int MAP = 31;

    /** extension: .bin. */
    public static final int MEDIVISION = 32;

    /** MGH/MGZ volume format. */
    public static final int MGH = 33;

    /** Micro CT format for small animal imaging. extension: .log, .ct */
    public static final int MICRO_CAT = 34;

    /**
     * MINC file type. MINC is a medical imaging oriented extension of the NetCDF file format. NetCDF stands for
     * 'Network Common Data Form'. extension: .mnc
     */
    public static final int MINC = 35;

    /** Not presently implemented. */
    public static final int MIPAV = 36;

    /** extension: .mrc. */
    public static final int MRC = 37;

    /** NIFTI format. extension: .img, .hdr, .nii */
    public static final int NIFTI = 38;

    /** NIFTI multi-file format. */
    public static final int NIFTI_MULTIFILE = 39;

    /** Nearly raw raster data. */
    public static final int NRRD = 40;

    /** Washington University OSM dataset structure. extension: .wu */
    public static final int OSM = 41;

    /** extension: .pcx. */
    public static final int PCX = 42;

    /** extension: .pic. */
    public static final int PIC = 43;

    /** extension: .pict. */
    public static final int PICT = 44;

    /** extension: .png. */
    public static final int PNG = 45;

    /** extension: .psd. */
    public static final int PSD = 46;

    /** Quicktime file type. extension: .mov, .qt */
    public static final int QT = 47;

    /** RAW image data, no header. extension: .raw */
    public static final int RAW = 48;

    /** RAW MULTIFLE image data, no header. */
    public static final int RAW_MULTIFILE = 49;

    /**
     * SPM file format. SPM99 and SPM2 are slight variants of analyze with the same .img, .hdr file extensions. The user
     * could also change the extension .img to .spm to indicate SPM. The header extension would remain .hdr
     */
    public static final int SPM = 50;

    /** MetaMorph Stack (STK) file type. extension: .stk */
    public static final int STK = 51;

    /** MIPAV Surface XML file format. extension: .xml */
    public static final int SURFACE_XML = 52;

    /** extension: .tga. */
    public static final int TGA = 53;

    /** TIFF file; tagged header. extension: .tif, .tiff */
    public static final int TIFF = 54;

    /** Multiple files of TIFF images. */
    public static final int TIFF_MULTIFILE = 55;

    /** Optical coherence tomography. extension: .tmg */
    public static final int TMG = 56;

    /** VOI file, used to read VOIs. extension: .voi */
    public static final int VOI_FILE = 57;

    /** extension: .xbm. */
    public static final int XBM = 58;

    /** MIPAV XML file format. mipav xml image format. extension: .xml */
    public static final int XML = 59;

    /** MIPAV XML file format. */
    public static final int XML_MULTIFILE = 60;

    /** extension: .xpm. */
    public static final int XPM = 61;

    /** extension: "par","parv2","rec","frec". */
    public static final int PARREC = 62;
    
    /** extension: "par","parv2","rec","frec". */
    public static final int PARREC_MULTIFILE = 63;
    
    /** SPAR file format for use with PARREC images */
    public static final int SPAR = 64;

    /** MIPAV Surface XML file format. extension: .xml */
    public static final int SURFACEREF_XML = 65;

    /** MINC 2.0 (HDF5) */
    public static final int MINC_HDF = 66;

    /** Improvision OpenLab LIFF .liff */
    /** Do not confuse with Leica image file format .lif */
    public static final int LIFF = 67;

    /** Extension: .hdr for header, .bfloat for data */
    public static final int BFLOAT = 68;

    /** Extension: .hdr for header, .img for data */
    public static final int SIEMENSTEXT = 69;

    /** Zeiss ZVI has extension .zvi */
    public static final int ZVI = 70;

    public static final int JP2 = 71;
    
    /** extension .mat */
    public static final int MATLAB = 72;
    
    /** Vista file extension .v */
    public static final int VISTA = 73;

    
    /** Metaimage files are either
     *  separate .mhd header and .raw image data files or
     *  combined .mha header and image data file
     */
    public static final int METAIMAGE = 74;
    
    

    private static final String[] fileTypeStr = {"error", "undefined", "afni", "analyze", "analyze multifile", "avi",
            "biorad", "bmp", "bmp multifile", "bruker", "cheshire", "cheshire overlay", "cor", "cur", "dib", "dicom", "dm3", "fits",
            "GE genesis", "GE genisis multifile", "GE signa4x", "GE Signa4x multifile", "gif", "ico", "ics",
            "interfile", "jimi", "jpeg", "lsm", "lsm multifile", "magnetom vision", "Megnatom vision multifile", "map",
            "medivision", "mgh", "micro cat", "minc", "mipav", "mrc", "nifti", "nifti multifile", "nrrd", "osm", "pcx",
            "pic", "pict", "png", "psd", "qt", "raw", "raw multifile", "spm", "stk", "surface xml", "tga", "tiff",
            "tiff multifile", "tmg", "voi file", "xbm", "xml", "xml multifile", "xpm", "parrec", "parrec multifile", "spar", "surfaceref xml",
            "minc hdf", "liff", "bfloat", "siemens text", "zvi", "jp2", "mat", "v", "MetaImage"};

    

    /**
     * Returns the file type associated with a string.
     * 
     * @param s String to test
     * 
     * @return axis orientation
     */
    public static int getFileTypeFromStr(final String s) {

        // look through the array of strings to see if there's a match.
        try {

            for (int i = 0; i < FileUtility.fileTypeStr.length; i++) {

                if (FileUtility.getFileTypeStr(i).regionMatches(true, 0, s, 0, FileUtility.getFileTypeStr(i).length())) {
                    // because fileType indicies start at -1, must decrement
                    return i - 1;
                }
            }
        } catch (final ArrayIndexOutOfBoundsException aie) {
            return FileUtility.ERROR;
        }

        return FileUtility.ERROR;

    } // end getFileTypeFromStr()

    /**
     * Return the string associated with a file type.
     * 
     * @param m int representing the file type (see the above static definitions)
     * 
     * @return String representing the string associated with the file type.
     */
    public static String getFileTypeStr(final int m) {

        try {
            // because fileType indicies start at -1, must increment
            return FileUtility.fileTypeStr[m + 1];
        } catch (final ArrayIndexOutOfBoundsException aie) {}

        return "";

    } // end getFileTypeStr()

    /**
     * Only for FreeSurfer COR volume files Looks in the image directory and returns all images with the same root up to
     * the hyphen, sorted in lexicographical order. Will set the number of images (<code>nImages</code>) for the
     * calling program.
     * 
     * @param fileDir Directory to look for images.
     * @param fileName File name of the image.
     * @param quiet Whether to avoid displaying errors using the GUI.
     * 
     * @return An array of the image names to be read in or saved as.
     * 
     * @throws OutOfMemoryError If there is a problem allocating required memory.
     */
    public static final String[] getCORFileList(final String fileDir, final String fileName, boolean quiet)
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
        String fileName2Trimmed;

        imageDir = new File(fileDir);

        // Read directory and find no. of images
        fileListBuffer = imageDir.list();
        fileList = new String[fileListBuffer.length];

        final String subName = FileUtility.trimCOR(fileName); // subName = name without indexing numbers at end

        for (i = 0; i < fileListBuffer.length; i++) {
            fileName2 = fileListBuffer[i].trim();
            suffix2 = FileUtility.getCORSuffixFrom(fileName2);
            okNumber = true;

            for (k = 1; k < suffix2.length(); k++) {

                if ( !Character.isDigit(suffix2.charAt(k))) {

                    // modified to use Java.lang version 20 July 2004/parsonsd
                    // if ( suffix2.charAt( k ) < '0' || suffix2.charAt( k ) > '9' ) {
                    okNumber = false;
                }
            } // for (k = 0; k < suffix2.length(); k++)

            if (okNumber) {
                fileName2Trimmed = FileUtility.trimCOR(fileName2);
                if (fileName2Trimmed != null) {
                    if (fileName2Trimmed.equals(subName)) {
                        fileList[j] = fileListBuffer[i];
                        j++;
                    }
                }
            } // if (okNumber)
        } // for (i = 0; i < fileListBuffer.length; i++)

        // Number of images is index of last image read into fileList
        nImages = j;

        if (nImages == 0) {

            if ( !quiet) {
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
     * @param fn The filename.
     * 
     * @return The suffix or file-extension. For example,
     *         <q>-info</q>. Note that suffix includes the separator '-'
     */
    public static final String getCORSuffixFrom(final String fn) {
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
     * Returns the extension of the file name, if file name does not have extension, then return empty string.
     * 
     * @param absolutePath the file name.
     * 
     * @return The file's extension.
     */
    public static final String getExtension(final String absolutePath) {

        if ( (absolutePath == null) || (absolutePath.length() == 0)) {
            return "";
        }

        final int index = absolutePath.lastIndexOf(".");

        if (index >= 0) {
            return absolutePath.substring(index);
        }

        return "";
    }

    /**
     * Returns the path information from the file name with the path information.
     * 
     * @param fileName the file name wiht the path information.
     * 
     * @return The path information.
     */
    public static final String getFileDirectory(final String fileName) {

        if ( (fileName == null) || (fileName.length() == 0)) {
            return null;
        }

        final int index = fileName.lastIndexOf(File.separator);

        if (index >= 0) {
            return fileName.substring(0, index + 1);
        }

        return null;
    }

    /**
     * Trims off the file extension and file name, but leaves the file index. An index might be 0001, or 140, for
     * example.
     * 
     * @param fName String file name to get index
     * 
     * @return String (index string)
     */
    public static final int getFileIndex(final String fName) {
        int i;

        // char ch;
        final int length = fName.lastIndexOf("."); // Start before suffix.

        for (i = length - 1; i > -1; i--) {

            if ( !Character.isDigit(fName.charAt(i))) {
                break;
            }
        }

        if (i <= -1) {
            return -1;
        }

        return (new Integer(fName.substring( (i + 1), length)).intValue());
    }

    /**
     * Looks in the image directory and returns all images with the same suffix as <code>fileName</code>, sorted in
     * lexicographical order.
     * 
     * @param fileDir Directory to look for images.
     * @param fileName File name of the image.
     * @param quiet Whether to avoid displaying errors using the GUI.
     * 
     * @return An array of the image names to be read in or saved as.
     * 
     * @throws OutOfMemoryError If there is a problem allocating required memory.
     */
    public static final String[] getFileList(final String fileDir, final String fileName, boolean quiet)
            throws OutOfMemoryError {
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

        final String subName = FileUtility.trimNumbersAndSpecial(fileName); // subName = name without indexing numbers
        // at
        // end
        final String suffix = FileUtility.getExtension(fileName); // suffix = ie. .ima or .img ...

        Preferences.debug("Suffix = _" + suffix + "_" + " subName = " + subName, Preferences.DEBUG_FILEIO);

        for (i = 1; i < suffix.length(); i++) {

            if ( !Character.isDigit(suffix.charAt(i))) {

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

                if ( !files[i].isDirectory()) {
                    fileName2 = fileListBuffer[i].trim();
                    suffix2 = FileUtility.getExtension(fileName2);

                    // System.out.println( "Suffix2 = _" + suffix2 + "_" );
                    okNumber = true;

                    for (k = 1; k < suffix2.length(); k++) {

                        if ( !Character.isDigit(suffix2.charAt(k))) {

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

                if ( !files[i].isDirectory()) {
                    final String fileSubName = FileUtility.trimNumbersAndSpecial(fileListBuffer[i].trim());
                    final String fileExtension = FileUtility.getExtension(fileListBuffer[i]);

                    if (fileSubName.trim().equals(subName) && fileExtension.equalsIgnoreCase(suffix)) {
                        fileList[j] = fileListBuffer[i];
                        j++;
                    }

                }
            }

        } else { // numberSuffix == false. I.e. ".img".

            // check to see that they end in suffix. If so, store, count.

            for (i = 0; i < fileListBuffer.length; i++) {

                if ( !files[i].isDirectory()) {

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

            if ( !quiet) {
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
     * @param absolutePath the file name with the path information.
     * 
     * @return The file name without path information.
     */
    public static final String getFileName(final String absolutePath) {

        if ( (absolutePath == null) || (absolutePath.length() == 0)) {
            return null;
        }

        final int index = absolutePath.lastIndexOf(File.separator);

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
     * @param fileName Filename of the image to read in. Must include the file extension.
     * @param fileDir Directory where fileName exists.
     * @param quiet Whether to avoid any user interaction (ie, from error popups).
     * 
     * @return Filetype from FileBase.
     * 
     * @see FileBase
     */
    public static final int getFileType(final String fileName, final String fileDir, final boolean quiet) {
    	boolean zerofunused[] = new boolean[1];
        return FileUtility.getFileType(fileName, fileDir, false, quiet, zerofunused);
    }

    /**
     * Sets the FileBase.(filetype) based on the file extension of the given filename. Also sets file "suffix", if
     * required.
     * 
     * @param fileName Filename of the image to read in. Must include the file extension.
     * @param fileDir Directory where fileName exists.
     * @param doWrite If true about to write a file
     * @param quiet Whether to avoid any user interaction (ie, from error popups).
     * @param zerofunused If true, zero funused fields in an analyze write
     * 
     * @return Filetype from FileBase.
     * 
     * @see FileBase
     */
    public static final int getFileType(String fileName, final String fileDir, boolean doWrite, boolean quiet,
    		boolean zerofunused[]) {
        int fileType;
        // If true, zero the funused fields in an analyze write
        zerofunused[0] = false;
        int i;

        int indexExt = fileName.lastIndexOf(".");
        if (indexExt >= 0 && indexExt < fileName.length()-1) {
            String ext = fileName.substring(indexExt + 1);
            if (ext.equalsIgnoreCase("zip") || ext.equalsIgnoreCase("gz") || ext.equalsIgnoreCase("bz2")) {
                fileName = fileName.substring(0, indexExt);
            } 
        }
        
        final String beginString = FileUtility.stripExtension(fileName);

        if ( (beginString.equalsIgnoreCase("d3proc")) || (beginString.equalsIgnoreCase("reco"))
                || (beginString.equalsIgnoreCase("2dseq"))) {
            fileType = FileUtility.BRUKER;

            return fileType;
        }

        fileName = fileName.trim();

        final String suffix = FileUtility.getExtension(fileName);

        fileType = FileTypeTable.getFileTypeFromSuffix(suffix);

        // check to see if we're being asked to look at an empty file
        if ( !doWrite && new File(fileDir + File.separator + fileName).length() == 0) {
            return fileType;
        }

        // handle when the .mnc extension but MINC_HDF file
        if (fileType == FileUtility.MINC) {
            try {
                if (doWrite) {
                    if ( !quiet) {
                        fileType = new JDialogSaveMincVersionChoice(ViewUserInterface.getReference().getMainFrame())
                                .fileType();
                    } else {
                        if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_MNC_AS_MINC1)) {
                            fileType = FileUtility.MINC;
                        } else if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_MNC_AS_MINC2)) {
                            fileType = FileUtility.MINC_HDF;
                        } else {
                            fileType = FileUtility.MINC;
                            System.err
                                    .println("Could not determine default filetype from preferences...saving in MINC-1.0 format");
                        }
                    }

                }
                // inspect the file to see if it is really a MINC1 (suppressing any error dialogs).
                // if not, set the file type using isMincHDF()
                else if (FileUtility.isMinc(fileName, fileDir, true) != FileUtility.MINC) {
                    fileType = FileUtility.isMincHDF(fileName, fileDir, quiet);
                }
            } catch (final IOException ioe) {
                if (ioe instanceof FileNotFoundException) {
                    MipavUtil.displayError("File does not exist '" + fileDir + fileName + "'.");
                    ioe.printStackTrace();

                    return FileUtility.ERROR;
                }

                if ( !quiet) {
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
                    final File file = new File(fileDir + fileName);
                    final RandomAccessFile raFile = new RandomAccessFile(file, "r");

                    raFile.seek(54L);

                    // little endian unsigned short
                    final int b1 = raFile.readUnsignedByte();
                    final int b2 = raFile.readUnsignedByte();
                    final int fileID = ( (b2 << 8) | b1); // Little Endian

                    raFile.close();

                    if (fileID == 12345) {
                        fileType = FileUtility.BIORAD;
                    } else {
                        fileType = FileUtility.JIMI;
                    }
                } catch (final OutOfMemoryError error) {
                    System.gc();
                } catch (final FileNotFoundException e) {
                    System.gc();
                } catch (final IOException e) {
                    System.gc();
                }
            } else if (suffix.equalsIgnoreCase(".img")) {

                // ANALYZE, Interfile, and NIFTI use .img and .hdr
                if (doWrite) {
                    if ( !quiet) {
                        final JDialogAnalyzeNIFTIChoice choice = new JDialogAnalyzeNIFTIChoice(ViewUserInterface
                                .getReference().getMainFrame());

                        if ( !choice.okayPressed()) {
                            fileType = FileUtility.ERROR;
                        } else {
                            fileType = choice.fileType();
                            zerofunused[0] = choice.zerofunused();
                        }
                    } else {
                        // when in quiet mode, we will default to NIFTI format if Prefs are not there
                        if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_ANALYZE)) {
                            fileType = FileUtility.ANALYZE;
                        } else if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_INTERFILE)) {
                            fileType = FileUtility.INTERFILE;
                        } else if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_NIFTI)) {
                            fileType = FileUtility.NIFTI;
                        } else {
                            fileType = FileUtility.NIFTI;
                            System.err
                                    .println("Could not determine default filetype from preferences...saving in NIFTI format");
                        }
                    }

                } else { // read
                    if (FileCheshire.isCheshire(fileName, fileDir)) {
                        fileType = FileUtility.CHESHIRE;
                    } else {
                        final int p = fileName.lastIndexOf(".");
                        final String fileHeaderName = fileName.substring(0, p + 1) + "hdr";
                        final String headerFile = FileInterfile.isInterfile(fileHeaderName, fileDir);
                        if (headerFile != null) {
                            fileType = FileUtility.INTERFILE;
                        } else {
                            // Note that SPM99 and SPM2 Analyze variant files are read as Mayo Analyze 7.5
                            // unless a SPM2 with extended header size > 348 is present.
                            try {
                                fileType = FileUtility.isAnalyzeOrSPM(fileHeaderName, fileDir, quiet);
                            } catch (final IOException ex) {}
                            if (fileType == FileUtility.UNDEFINED) {
                                fileType = FileUtility.SPM;
                            }

                            try {
                                final File file = new File(fileDir + fileHeaderName);
                                final RandomAccessFile raFile = new RandomAccessFile(file, "r");

                                raFile.seek(344L);

                                final char[] niftiName = new char[4];

                                for (i = 0; i < 4; i++) {
                                    niftiName[i] = (char) raFile.readUnsignedByte();
                                }

                                raFile.close();

                                if ( (niftiName[0] == 'n') && ( (niftiName[1] == 'i') || (niftiName[1] == '+'))
                                        && (niftiName[2] == '1') && (niftiName[3] == '\0')) {
                                    fileType = FileUtility.NIFTI;
                                }
                            } catch (final OutOfMemoryError error) {
                                System.gc();
                            } catch (final FileNotFoundException e) {
                                System.gc();
                            } catch (final IOException e) {
                                System.gc();
                            }
                        }
                    }
                }
            } else if (suffix.equalsIgnoreCase(".hdr")) {
                if (doWrite) {
                    if ( !quiet) {
                        // ANALYZE, Interfile, and NIFTI use .img and .hdr
                        final JDialogAnalyzeNIFTIChoice choice = new JDialogAnalyzeNIFTIChoice(ViewUserInterface
                                .getReference().getMainFrame());

                        if ( !choice.okayPressed()) {
                            fileType = FileUtility.ERROR;
                        } else {
                            fileType = choice.fileType();
                            zerofunused[0] = choice.zerofunused();
                        }
                    } else {
                        // when in quiet mode, we will default to NIFTI format if Prefs are not there
                        if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_ANALYZE)) {
                            fileType = FileUtility.ANALYZE;
                        } else if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_INTERFILE)) {
                            fileType = FileUtility.INTERFILE;
                        } else if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_NIFTI)) {
                            fileType = FileUtility.NIFTI;
                        } else {
                            fileType = FileUtility.NIFTI;
                            System.err
                                    .println("Could not determine default filetype from preferences...saving in NIFTI format");
                        }
                    }
                } else { // read
                    final int p = fileName.lastIndexOf(".");
                    final String bfloatDataName = fileName.substring(0, p + 1) + "bfloat";
                    final File bfloatFile = new File(fileDir + bfloatDataName);
                    if (bfloatFile.exists()) {
                        fileType = FileUtility.BFLOAT;
                    } else {
                        final String spmDataName = fileName.substring(0, p + 1) + "spm";
                        final File spmFile = new File(fileDir + spmDataName);
                        if (spmFile.exists()) {
                            fileType = FileUtility.SPM;
                        } else {
                            final String headerFile = FileInterfile.isInterfile(fileName, fileDir);
                            if (headerFile != null) {
                                fileType = FileUtility.INTERFILE;
                            } else {
                                // Note that SPM99 and SPM2 Analyze variant files are read as Mayo Analyze 7.5
                                // unless a SPM2 with extended header size > 348 is present.
                                try {
                                    fileType = FileUtility.isAnalyzeOrSPM(fileName, fileDir, quiet);
                                } catch (final IOException ex) {}
                                if (fileType == FileUtility.UNDEFINED
                                        && FileSiemensText.isSiemensText(fileDir + fileName)) {
                                    fileType = FileUtility.SIEMENSTEXT;
                                }

                                try {
                                    final File file = new File(fileDir + fileName);
                                    final RandomAccessFile raFile = new RandomAccessFile(file, "r");

                                    raFile.seek(344L);

                                    final char[] niftiName = new char[4];

                                    for (i = 0; i < 4; i++) {
                                        niftiName[i] = (char) raFile.readUnsignedByte();
                                    }

                                    raFile.close();

                                    if ( (niftiName[0] == 'n') && ( (niftiName[1] == 'i') || (niftiName[1] == '+'))
                                            && (niftiName[2] == '1') && (niftiName[3] == '\0')) {
                                        fileType = FileUtility.NIFTI;
                                    }
                                } catch (final OutOfMemoryError error) {
                                    System.gc();
                                } catch (final FileNotFoundException e) {
                                    System.gc();
                                } catch (final IOException e) {
                                    System.gc();
                                }
                            } // else headerFile == null
                        } // else !spmFile.exists()
                    } // else !bfloatFile.exists()
                } // else read
            } // else if (suffix.equalsIgnoreCase(".hdr"))
            else if (suffix.equalsIgnoreCase(".ima")) {

                // Both Dicom and Siemens Magnetom Vision file type have the ima suffix
                try {
                    final File file = new File(fileDir + fileName);
                    final RandomAccessFile raFile = new RandomAccessFile(file, "r");

                    raFile.seek(281L);

                    final char[] ModelName = new char[15];

                    for (i = 0; i < 15; i++) {
                        ModelName[i] = (char) raFile.readUnsignedByte();
                    }

                    raFile.close();

                    final String ModelNameString = new String(ModelName);

                    if (ModelNameString.equals("MAGNETOM VISION")) {
                        fileType = FileUtility.MAGNETOM_VISION;
                    } else {
                        fileType = FileUtility.DICOM;
                    }
                } catch (final OutOfMemoryError error) {
                    System.gc();
                } catch (final FileNotFoundException e) {
                    System.gc();
                } catch (final IOException e) {
                    System.gc();
                }
            } else if (suffix.equalsIgnoreCase("") && doWrite) {
                Preferences.debug("FileIO: Cannot save a file without an extension: " + fileName + "\n",
                        Preferences.DEBUG_FILEIO);

                return FileUtility.UNDEFINED;
            }
        }

        final String strFileType = Preferences.getProperty(Preferences.PREF_USER_FILETYPE_ASSOC);
        String[] assoc = new String[0];

        if (strFileType != null) {
            assoc = strFileType.split(Preferences.ITEM_SEPARATOR);
        }

        // check to see if there are any user defined associations
        for (final String element : assoc) {

            if (suffix.equals(element.split(Preferences.DEFINITION_SEPARATOR)[0])) {
                fileType = new Integer(element.split(Preferences.DEFINITION_SEPARATOR)[1]).intValue();
            }
        }

        try {

            if (fileType == FileUtility.UNDEFINED) {
                fileType = FileUtility.isDicom(fileName, fileDir, quiet);
            }

            if (fileType == FileUtility.UNDEFINED) {
                fileType = FileUtility.isDicom_ver2(fileName, fileDir, quiet);
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
                fileType = FileUtility.isMincHDF(fileName, fileDir, quiet);
                if (fileType == FileUtility.ERROR) {
                	// If Minc HDF5 library is missing don't want to abort 
                	// file processing for all other following types
                	fileType = FileUtility.UNDEFINED;
                }
            }

            if (fileType == FileUtility.UNDEFINED) {
                // This will accept Mayo Analyze 7.5, SPM99, and
                // SPM2 with regular header size == 348 bytes
                // Only SPM2 with extended header size > 348 will
                // not be classified as analyze.
                fileType = FileUtility.isAnalyzeOrSPM(fileName, fileDir, quiet);
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
        } catch (final IOException ioe) {

            if (ioe instanceof FileNotFoundException) {
                MipavUtil.displayError("File does not exist '" + fileDir + fileName + "'.");
                ioe.printStackTrace();

                return FileUtility.ERROR;
            }

            if ( !quiet) {
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
     * Tests if the unknown file is of type Analyze.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param quiet Whether to avoid any user interaction (ie, from error popups).
     * 
     * @return <code>FileUtility.ANALYZE</code> if the file is a ANALYZE type, <code>FileUtility.SPM</code> if the
     *         file is a SPM type, and <code>FileUtility.UNDEFINED</code> otherwise
     * 
     * @throws IOException If there is a problem determining the type of the given file.
     */
    public static final int isAnalyzeOrSPM(final String fileName, final String fileDir, boolean quiet)
            throws IOException {

        try {

            return FileAnalyze.isAnalyzeOrSPM(fileDir + fileName);

        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
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
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param quiet Whether to avoid any user interaction (ie, from error popups).
     * 
     * @return <code>FileUtility.DICOM</code> if the file is a DICOM file, and <code>FileUtility.UNDEFINED</code>
     *         otherwise
     * 
     * @throws IOException If there is a problem determining the type of the given file.
     */
    public static final int isDicom(final String fileName, final String fileDir, boolean quiet) throws IOException {

        try {
            FileDicom imageFile = new FileDicom(fileName, fileDir);

            if (imageFile != null) {
                final boolean isDicom = imageFile.isDICOM();

                imageFile.close();
                imageFile = null;

                if (isDicom) {
                    return FileUtility.DICOM;
                }
            }

            return FileUtility.UNDEFINED;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("FileIO: " + error + "\n", Preferences.DEBUG_FILEIO);
            }

            return FileUtility.UNDEFINED;
        }

    }

    /**
     * tests if unknown file is of type 2.0 dicom
     * 
     * @param fileName
     * @param fileDir
     * @param quiet
     * @return
     * @throws IOException
     */
    public static final int isDicom_ver2(final String fileName, final String fileDir, boolean quiet) throws IOException {
        try {
            FileDicom imageFile = new FileDicom(fileName, fileDir);

            if (imageFile != null) {
                final boolean isDicom = imageFile.isDICOM_ver2();

                imageFile.close();
                imageFile = null;

                if (isDicom) {
                    return FileUtility.DICOM;
                }
            }

            return FileUtility.UNDEFINED;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
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
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param quiet Whether to avoid any user interaction (ie, from error popups).
     * 
     * @return <code>FileUtility.GE_SIGNA4X</code> if the file is a GE MR Signa 4.x file, and <code>
     *          FileUtility.UNDEFINED</code>
     *         otherwise
     * 
     * @throws IOException If there is a problem determining the type of the given file.
     */
    public static final int isGESigna4X(final String fileName, final String fileDir, boolean quiet) throws IOException {

        try {
            final FileGESigna4X imageFile = new FileGESigna4X(fileName, fileDir);

            if (imageFile != null) {
                final boolean isGESigna4X = imageFile.isGESigna4X();

                if (isGESigna4X) {
                    return FileUtility.GE_SIGNA4X;
                }
            }

            return FileUtility.UNDEFINED;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
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
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param quiet Whether to avoid any user interaction (ie, from error popups).
     * 
     * @return <code>FileUtility.GE_GENESIS</code> if the file is a GE MR Signa 5.x file, and <code>
     *          FileUtility.UNDEFINED</code>
     *         otherwise
     * 
     * @throws IOException If there is a problem determining the type of the given file.
     */
    public static final int isGESigna5X(final String fileName, final String fileDir, boolean quiet) throws IOException {

        try {
            final FileGESigna5X imageFile = new FileGESigna5X(fileName, fileDir);

            if (imageFile != null) {
                final boolean isGESigna5X = imageFile.isGESigna5X();

                if (isGESigna5X) {
                    return FileUtility.GE_GENESIS;
                }
            }

            return FileUtility.UNDEFINED;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
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
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param quiet Whether to avoid any user interaction (ie, from error popups).
     * 
     * @return <code>FileUtility.Interfile</code> if the file is a Interfile type, and
     *         <code>FileUtility.UNDEFINED</code> otherwise
     * 
     * @throws IOException If there is a problem determining the type of the given file.
     */
    public static final int isInterfile(final String fileName, final String fileDir, boolean quiet) throws IOException {

        try {

        	final String fileHeaderName = FileInterfile.isInterfile(fileName, fileDir);

            if (fileHeaderName != null) {
                return FileUtility.INTERFILE;
            }

            return FileUtility.UNDEFINED;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
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
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param quiet Whether to avoid any user interaction (ie, from error popups).
     * 
     * @return <code>FileUtility.MAGNETOM_VISION</code> if the file is a Siemens Magnetom Vision type, and <code>
     *          FileUtility.UNDEFINED</code>
     *         otherwise
     * 
     * @throws IOException If there is a problem determining the type of the given file.
     */
    public static final int isMagnetomVision(final String fileName, final String fileDir, boolean quiet)
            throws IOException {

        try {
            final FileMagnetomVision imageFile = new FileMagnetomVision(fileName, fileDir);

            if (imageFile != null) {
                final boolean isMagnetomVision = imageFile.isMagnetomVision();

                if (isMagnetomVision) {
                    return FileUtility.MAGNETOM_VISION;
                }
            }

            return FileUtility.UNDEFINED;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
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
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param quiet Whether to avoid any user interaction (ie, from error popups).
     * 
     * @return <code>FileUtility.MINC</code> if the file is a MINC type, and <code>FileUtility.UNDEFINED</code>
     *         otherwise
     * 
     * @throws IOException If there is a problem determining the type of the given file.
     */
    public static final int isMinc(final String fileName, final String fileDir, boolean quiet) throws IOException {

        try {
            final FileMinc imageFile = new FileMinc(fileName, fileDir);

            if (imageFile != null) {
                final boolean isMinc = imageFile.isMinc();

                if (isMinc) {
                    return FileUtility.MINC;
                }
            }

            return FileUtility.UNDEFINED;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
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
     * 
     * @param fileName name of the file
     * @param fileDir directory
     * @param quiet
     * @return whether the file is HDF5 type
     */
    public static final int isMincHDF(final String fileName, final String fileDir, boolean quiet) {
        try {
            final FileFormat h5F = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);

            // if the FileFormat object is null, there was probably a problem loading the hdf5 libraries
            if (h5F == null) {
                
                Preferences.debug("Unable to load HDF5 libraries required for MINC-2.0 HDF files.", Preferences.DEBUG_FILEIO);

                return FileUtility.UNDEFINED; //TODO: Until this library is available for 64-bit macs, 
                                              //this has been changed to a FileUtility.UNKNOWN.  This was impeding the ability of users
                                              //without this library to load SPM, and truly UNKNOWN images
            }

            final boolean isMincHDF = h5F.isThisType(fileDir + File.separator + fileName);
            if (isMincHDF) {
                return FileUtility.MINC_HDF;
            } else {
                return FileUtility.UNDEFINED;
            }
        } catch (final NoClassDefFoundError e) {
            if ( !quiet) {
                MipavUtil.displayError("Unable to load HDF libraries: " + e.getMessage());
            }

            e.printStackTrace();

            return FileUtility.ERROR;
        } catch (final SecurityException e) {
            if ( !quiet) {
                MipavUtil.displayError("Unable to load HDF libraries: " + e.getMessage());
            }

            e.printStackTrace();

            return FileUtility.ERROR;
        } catch (final UnsatisfiedLinkError e) {
            if ( !quiet) {
                MipavUtil.displayError("Unable to load HDF libraries: " + e.getMessage());
            }

            e.printStackTrace();

            return FileUtility.ERROR;
        }
    }

    /**
     * Tests if the unknown file is of type Nifti.
     * 
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param quiet Whether to avoid any user interaction (ie, from error popups).
     * 
     * @return <code>FileUtility.NIFTI</code> if the file is a NIFTI type, and <code>FileUtility.UNDEFINED</code>
     *         otherwise
     * 
     * @throws IOException If there is a problem determining the type of the given file.
     */
    public static final int isNIFTI(final String fileName, final String fileDir, boolean quiet) throws IOException {

        try {
            final boolean isNIFTI = FileNIFTI.isNIFTI(fileName, fileDir);

            if (isNIFTI) {
                return FileUtility.NIFTI;
            }

            return FileUtility.UNDEFINED;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
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
     * @param fileName Name of the image file to read.
     * @param fileDir Directory of the image file to read.
     * @param quiet Whether to avoid any user interaction (ie, from error popups).
     * 
     * @return <code>FileUtility.SPM</code> if the file is a SPM type, and <code>FileUtility.UNDEFINED</code>
     *         otherwise
     * 
     * @throws IOException If there is a problem determining the type of the given file.
     */
    public static final int isSPM(final String fileName, final String fileDir, boolean quiet) throws IOException {

        try {
            final boolean isSPM = FileSPM.isSPM(fileDir + fileName);

            if (isSPM) {
                return FileUtility.SPM;
            }

            return FileUtility.UNDEFINED;
        } catch (final OutOfMemoryError error) {

            if ( !quiet) {
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
     * @param fileName Original name.
     * 
     * @return Name without extension, or original name if there was no extension.
     */
    public static final String stripExtension(final String fileName) {
        final int index = fileName.lastIndexOf(".");

        if (index != -1) {
            return fileName.substring(0, index);
        } else {
            return fileName;
        }
    }

    /**
     * Trims the numbers or file extension from COR file names. Any numbers or
     * <q>.info</q>
     * or
     * <q>.info~</q>
     * will be removed from after a hyphen in the given fname.
     * 
     * @param fName File name where the last characters are alpha-numerics indicating the image number or .info or
     *            .info~
     * 
     * @return File name without numbers on the end.
     */
    public static final String trimCOR(final String fName) {
        final int length = fName.lastIndexOf("-");

        if (length >= 0) {
            return (new String(fName.substring(0, length + 1)));
        } else {
            return null;
        }
    }

    /**
     * Trims the numbers and special character from the file name. Numerics and some special characters <code>[ - _
     * .</code>
     * are removed from the end of the file.
     * 
     * @param fName File name where the last characters are alpha-numerics indicating the image number.
     * 
     * @return File name without numbers on the end.
     */
    public static final String trimNumbersAndSpecial(final String fName) {
        int i;
        char ch;
        int length = fName.lastIndexOf("."); // Start before suffix.
        if (length == -1) {
            length = fName.length();
        }

        for (i = length - 1; i > -1; i--) {
            ch = fName.charAt(i);

            if ( !Character.isDigit(ch) && (ch != '-') && (ch != '.') && (ch != '_')) {
                break;
            }
        }

        String tmpStr;

        tmpStr = fName.substring(0, i + 1);

        boolean aCharIsPresent = false;

        // Determine if at least one letter is present
        for (i = 0; i < tmpStr.length(); i++) {
            ch = tmpStr.charAt(i);

            if (Character.isLetter(ch)) {
                aCharIsPresent = true;

                break;
            }
        }

        // If yes, then remove remaining numbers
        if (aCharIsPresent) {

            for (i = 0; i < tmpStr.length(); i++) {
                ch = tmpStr.charAt(i);

                if (Character.isDigit(ch)) {
                    tmpStr = tmpStr.substring(0, i) + tmpStr.substring(i + 1);
                    i = -1;
                }
            }
        }

        return (tmpStr);
    }

    /**
     * Gets the file name list from which this ModelImage is opened.
     * 
     * @param image the ModelImage object.
     * 
     * @return the actual file name list.
     */
    public static final List<String> getFileNameList(final ModelImage image) {

        if (image == null) {
            return null;
        }

        final FileInfoBase[] fileInfoList = image.getFileInfo();

        if ( (fileInfoList == null) || (fileInfoList.length == 0)) {
            return null;
        }

        final FileInfoBase fileInfo = fileInfoList[0];
        final int fileFormat = fileInfo.getFileFormat();
        Vector<String> fileNameList = new Vector<String>();

        String firstFileNameBase = fileInfo.getFileName();
        String firstFileNameBaseLower = firstFileNameBase.toLowerCase();
        if (firstFileNameBaseLower.endsWith(".gz") || firstFileNameBaseLower.endsWith(".bz2")
                || firstFileNameBaseLower.endsWith(".zip")) {
            firstFileNameBase = firstFileNameBase.replaceAll(".gz$", "");
            firstFileNameBase = firstFileNameBase.replaceAll(".bz2$", "");
            firstFileNameBase = firstFileNameBase.replaceAll(".zip$", "");
            firstFileNameBaseLower = firstFileNameBase.toLowerCase();
        }

        switch (fileFormat) {
            case FileUtility.ANALYZE:
            case FileUtility.ANALYZE_MULTIFILE:
            case FileUtility.NIFTI:
            case FileUtility.NIFTI_MULTIFILE:
                if (firstFileNameBaseLower.endsWith(".nii")) {
                    String file;
                    for (final FileInfoBase element : fileInfoList) {
                        file = fileInfo.getFileDirectory() + File.separator + element.getFileName();
                        if (file != null && !fileNameList.contains(file)) {
                            fileNameList.add(file);
                        }
                    }
                } else {
                    // TODO: what about extension case?
                    String imgFileName;
                    String hdrFileName;
                    for (final FileInfoBase element : fileInfoList) {
                        imgFileName = element.getFileName();
                        hdrFileName = imgFileName.replaceFirst(".img", ".hdr");

                        if (imgFileName != null
                                && !fileNameList.contains(fileInfo.getFileDirectory() + File.separator + imgFileName)) {
                            fileNameList.add(fileInfo.getFileDirectory() + File.separator + hdrFileName);
                            fileNameList.add(fileInfo.getFileDirectory() + File.separator + imgFileName);
                        }
                    }
                }
                break;

            case FileUtility.BFLOAT:
                // TODO: what about extension case?
                final String bfloatFileName = fileInfo.getFileName();
                final String hdrFileName = bfloatFileName.replaceFirst(".bfloat", ".hdr");
                fileNameList.add(fileInfo.getFileDirectory() + File.separator + hdrFileName);
                fileNameList.add(fileInfo.getFileDirectory() + File.separator + bfloatFileName);
                break;

            case FileUtility.AFNI:
                // TODO: what about extension case?
                final String headFileName = fileInfo.getFileName();
                final String brikFileName = headFileName.replaceFirst(".HEAD", ".BRIK");
                fileNameList.add(fileInfo.getFileDirectory() + File.separator + headFileName);
                fileNameList.add(fileInfo.getFileDirectory() + File.separator + brikFileName);
                break;

            case FileUtility.XML:
            case FileUtility.XML_MULTIFILE:
                String xmlFileName;
                String rawFileName;
                for (final FileInfoBase element : fileInfoList) {
                    xmlFileName = element.getFileName();
                    rawFileName = ((FileInfoXML) element).getImageDataFileName();

                    if (xmlFileName != null
                            && !fileNameList.contains(fileInfo.getFileDirectory() + File.separator + xmlFileName)) {
                        fileNameList.add(fileInfo.getFileDirectory() + File.separator + xmlFileName);
                        fileNameList.add(fileInfo.getFileDirectory() + File.separator + rawFileName);
                    }
                }
                break;

            case FileUtility.PARREC:
            case FileUtility.PARREC_MULTIFILE:
                // TODO: what about extension case? need to support other parrec extensions
                final String parFileName = fileInfo.getFileName();
                final String recFileName = parFileName.replaceFirst(".par", ".rec");
                fileNameList.add(fileInfo.getFileDirectory() + File.separator + parFileName);
                fileNameList.add(fileInfo.getFileDirectory() + File.separator + recFileName);
                break;

            case FileUtility.UNDEFINED:
            case FileUtility.ERROR:
            case FileUtility.VOI_FILE:
            case FileUtility.MIPAV:
            case FileUtility.CHESHIRE_OVERLAY:
            case FileUtility.SURFACE_XML:
            case FileUtility.SURFACEREF_XML:
                fileNameList = null;
                break;
            case FileUtility.DICOM:
                final boolean isEnhancedDicom = ((FileInfoDicom) fileInfoList[0]).isEnhancedDicom();
                if (isEnhancedDicom) {
                    final String file = fileInfo.getFileDirectory() + File.separator + fileInfoList[0].getFileName();
                    fileNameList.add(file);

                } else {
                    String file;
                    for (final FileInfoBase element : fileInfoList) {
                        file = fileInfo.getFileDirectory() + File.separator + element.getFileName();
                        if (file != null && !fileNameList.contains(file)) {
                            fileNameList.add(file);
                        }
                    }
                }
                break;
            default:
                String file;
                for (final FileInfoBase element : fileInfoList) {
                    file = fileInfo.getFileDirectory() + File.separator + element.getFileName();
                    if (file != null && !fileNameList.contains(file)) {
                        fileNameList.add(file);
                    }
                }
        }

        return fileNameList;
    }
}
