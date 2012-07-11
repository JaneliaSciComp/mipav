package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.zip.*;


/**
 * @see  FileIO
 * @see  FileInfoNRRD
 * @see  FileRaw
 */

public class FileNRRD extends FileBase {
    
    /**
     * See http://teem.sourceforge.net/nrrd for information about the NRRD file format
     * NRRD files can have the header and the data in the same file in which case the filename ends in .nrrd.
     * NRRD files can have the header information and the data in separate files in which case the header name
     * ends in .nhdr and the data name ends in .raw, .txt, .hex, .raw.gz, or .raw.bz2.
     * 
     * The general format of the header is:
     * NRRD000X
     * <field>: <desc>
     * <field>: <desc>
     * # <comment>
     * ...
     * <field>: <desc>
     * <key>:=<value>
     * <key>:=<value>
     * <key>:=<value>
     * # <comment>
     *
     * Each header line is terminated by either a pair of characters "\r\n" or by just a single character "\n".
     * The first header line always starts with the four characters "NRRD" and the remaining characters give the 
     * file format version information.  The X in the first line is a number such as 1, 2, 3, 4, or 5.
     * All field specifications have a field identifier string "<field>", then a colon followed by a single space, and
     * then the field descriptor information string "<desc>" describing the field.  Each of the "<key>:=<value>" lines
     * describes a key/value pair in nrrd.  Comment lines start with a pound sign.  If the data is stored in the same
     * file as the header, then a single blank line containing zero characters separates the header from the data.
     * 
     * If the header and data are stored in separate files then the data file specification can take 1 of 3 forms:
     * 1.) data file: <filename>  There is a single detached data file, and its filename is "filename".
     * 2.) data file: <format> <min> <max> <step> [<subdim>].  There are multiple detached data files, and their 
     * filenames include integral values which must be generated using a format specification for an integer value,
     * which ranges according to <min>, <max>, and <step>.
     * 3.) data file: LIST [<subdim>]  There are multiple detached data files, and their filenames are given explicitly,
     * in the NRRD header, one filename per line, in the line starting after the data file field specification, until
     * the end of file.
     */

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int UNKNOWN = 0;

    /** A patient-based right-handed coordinate frame, with ordered basis vectors pointing towards right, anterior, and
     *  superior, respectivley.  Used in NIFTI. */
    //private static final int RAS = 1;

    /** A patient-based left-handed coordinate frame, with ordered basis vectors pointing towards left, anterior, and
     *  superior, respectively.  Used in Analyze. */
    //private static final int LAS = 2;

    /** A patient-based right-handed coordinate frame, with ordered basis vectors pointing towards left, posterior, and
     *  superior, respectively.  Used in DICOM. */
    private static final int LPS = 3;

    /** Like RAS, but with time along the fourth axis. */
    //private static final int RAST = 4;

    /** Like LAS, but with time along the fourth axis. */
    //private static final int LAST = 5;

    /** Like LPS, but with time along the fourth axis. */
    private static final int LPST = 6;

    /** A scanner-based right-handed coordinate frame, used in ACR/NEMA 2.0.
     *  If a patient lies parallel to the ground, face-up on the table, with their feet-to-head direction same as the
     *  front-to-back direction of the imaging equipment, the axes of this scanner-based coordinate frame and the 
     *  (patient-based) left-posterior-superior frame coincide. */
    private static final int SCANNER_XYZ = 7;

    /** Like SCANNER_XYZ, but with time along the fourth axis. */
    private static final int SCANNER_XYZ_TIME = 8;

    /** Any right-handed three-dimensional space. */
    private static final int THREED_RIGHT_HANDED = 9;

    /** Any left-handed three-dimensional space. */
    private static final int THREED_LEFT_HANDED = 10;

    /** Like THREED_RIGHT_HANDED, but with time along the fourth axis. */
    private static final int THREED_RIGHT_HANDED_TIME = 11;

    /** Like THREEE_LEFT_HANDED, but with time along the fourth axis. */
    private static final int THREED_LEFT_HANDED_TIME = 12;

    /** Centering information for this axis is either meaningless or unknown.
     *  This applies to any non-spatial axis. */
    private static final int NONE = 0;

    /** The location of the sample is located in the interior of the grid element. */
    private static final int CELL = 1;

    /** The location of the sample is at the boundary between grid elements. */
    private static final int NODE = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** True for RAS and LAS, false for LPS.  If true, change sign on origin[1] and row[1] of matrix. */
    private boolean apInvert = false;

    /** True if data file: <format> <min> <max> <step> [<subdim>] encountered
     *  If true, autosequence using integer format specification varied according to <min>, <max>, and <step>. */
    private boolean autoSequence = false;

    /** Maximum axis value.  centers[i], axisMins[i], axisMaxs[i], and nrrdSizes[i] are used to calculate
     *  resols[i] and origin[i] if spacings[i] not available. */
    private double[] axisMaxs = null;

    /** Minimum axis value.  centers[i], axisMins[i], axisMaxs[i], and nrrdSizes[i] are used to calculate
     *  resols[i] and origin[i] if spacings[i] not available. */
    private double[] axisMins = null;

    /** For each axis R2L, L2R, A2P, P2A, I2S, or S2I */
    private int[] axisOrientation = null;

    /** In autosequencing number between a capital D and the last period used in generating filenames.
     *  The capital D must come after the percentage sign. */
    private String baseAfterNumber = null;

    /** In autosequencing number before percentage sign and period used in generating start of
     *  autosequenced filename. */
    private String baseBeforeNumber = null;

    /** Used if autosequencing.  If true, increment file base.  If false, increment file extension.
     *  True if period follows a capital D.  The capital D must follow a percentage sign.  */
    private boolean baseNumber = false;

    /** May remain null or may be created as NONE, CELL, or NODE. */
    private int[] centers = null;

    /** "modality:=DWMRI" : This key/value pair explicitly indicates that the image is a 
     * diffusion-weighted MRI scan, and it implies that all of the following key/value pairs
     * are also set in the header:
     * "DWMRI_b-value:=b " : This key/value pair gives the (scalar) diffusion-weighting value,
     *  in units of s/mm^2. Example: "DWMRI_b-value:=1000". The effective
     *  magnitude of diffusion-weighting for each DWI is determined with some simple calculations based
     *  on the individual per-DWI gradient directions or B-matrices.
     *  For every index position NNNN along the DWI axis (whichever is the non-spatial axis identified
     *  by the "list" or "vector" kind field), either "DWMRI_gradient_NNNN:=x y z " or 
     *  "DWMRI_B-matrix_NNNN:=xx xy xz yy yz zz " must be given (except if "DWMRI_NEX_NNNN:= M " is used).  */
    private String[][] dwmriGradient = null;

    /** "DWMRI_NEX_NNNN:=M " means that the information provided for image NNNN 
     * (either gradient or B-matrix) is the same as for image NNNN+1, NNNN+2, up
     *  to and including NNNN+M-1. */
    private String[][] dwmriNex = null;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private String ext = null;

    /** DOCUMENT ME! */
    private File file;


    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoNRRD fileInfo = null;

    /** DOCUMENT ME! */
    private FileInfoNRRD fileInfoSub = null;

    /** DOCUMENT ME! */
    private long fileLength;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private boolean fileNameSequence = false;

    /** DOCUMENT ME! */
    private String[] fileNameSet = null;

    /** DOCUMENT ME! */
    private int fileNumber = 0;

    /** DOCUMENT ME! */
    private int[] finishBlank;

    /** DOCUMENT ME! */
    private int[] finishQuote;

    /** DOCUMENT ME! */
    private boolean foundEOF = false;

    /** DOCUMENT ME! */
    private boolean foundEOHeader = false;

    /** If true, data has gzip compression. */
    private boolean gunzip = false;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int[] imgExtents;

    /** Kind of information represented by the samples along each axis. */
    private String[] kindsString = null;


    /** DOCUMENT ME! */
    private TransMatrix matrix = new TransMatrix(4);

    /** The measurement frame is a basic (per-array) field specification (not per-axis), which
     *  identifies a spaceDim-by-spaceDim matrix, where spaceDim is the dimension of the word space
     *  (implied by space or given by space dimension).  The matrix transforms (a column vector of)
     *  coordinates in the measurement frame to corrdinates in world space.  vector[i] gives column i
     *  of the measurement frame matrix.  Just as the space directions field gives, one column at
     *  a time, the mapping from image space to world space coordinates, the measurement frame
     *  gives the mapping measurement frame to world space coordinates, also one column at a time. */
    private double[][] measurementFrame = null;

    /** DOCUMENT ME! */
    private int mipavDataType;

    /** DOCUMENT ME! */
    private int mipavDimensions;

    /** MIPAV axis names such as X, Y, or Z */
    private String[] mipavLabels = null;

    /** DOCUMENT ME! */
    private int[] mipavUnits;

    /** Does not tell if color or black and white. */
    private int nrrdDataType;

    /** May not be the same as MIPAV dimensions because in nrrd color is a dimension. */
    private int nrrdDimensions;

    /** nrrd axis names such as X, Y, or Z */
    private String[] nrrdLabels = null;

    /** The number of samples along each nrrd axis */
    private int[] nrrdSizes;

    /** DOCUMENT ME! */
    private String[] nrrdSpaceUnits = null;

    /** Units of measurement of nrrd axis */
    private String[] nrrdUnits = null;

    /** DOCUMENT ME! */
    private int numColors = 3;

    /** Number of nrrd dimensions for which the kinds string equals "SPACE". */
    private int numSpaceKinds = 0;

    /** Number of axes for which thicknesses values found */
    private int numThicknesses = 0;

    /** DOCUMENT ME! */
    private long offset1;


    /** True for .nrrd file with header and data in the same file.
     *  False for .nhdr header files with the data in one or more other files. */
    private boolean oneFileStorage;

    /** DOCUMENT ME! */
    private float[] origin = null;

    /** In autosequencing if the sequence number length is less than the padding number, zeroes are added
     *  to the front of the sequence number until the sequence number length equals the padding number. */
    private int paddingNumber = 0;

    /** 0 indicates pixels are RGB, RGB chunky 1 indicates pixels are RRR, GGG, BBB planar. */
    private int planarConfig = 0;

     /**
     * 
     * The orientation information in some NRRD files cannot be applied to MIPAV without dimension reordering. This
     * applies to 2 of my 15 example NRRD files. In Dwi-D.nhdr 4 dimensions are specified with the dimensions from 0 to
     * 3 having sizes of 13, 29, 30, and 31. Right-anterior-superior space is specified, but the first axis contains
     * diffusion values and the last 3 axes are the x, y, and z coordinates, so the right-anterior-space applies to the
     * last 3 dimensions, whereas MIPAV would expect the first 3 dimensions to be involved. In gk2-rcc-mask2.nhdr 4
     * dimensions are specified with the dimensions from 0 to 3 having sizes of 7, 148, 190, and 160.
     * Left-posterior-space is specified, but the first axis contains diffusion values and the last 3 axes are x, y, and
     * z coordinates, so again the space applies to the last 3 dimensions rather than MIPAV's first 3 dimensions. In
     * these cases reorder the data so that the first axis becomes the last axis.
     */
    private boolean reorder = false;

    /** DOCUMENT ME! */
    private float[] resols = null;

    /** If true, data is a specialized form of 4-color with red, green, blue, and alpha, in that order. */
    private boolean RGBAOrder = false;

    /** True for RAS.  False for LAS and LPS.  If true, change sign on origin[0] and first row of matrix. */
    private boolean rlInvert = false;

    /** DOCUMENT ME! */
    private int sequenceFinish;

    /** DOCUMENT ME! */
    private int sequenceStart;

    /** DOCUMENT ME! */
    private int sequenceStep;


    /** Number of bytes to skip */
    private int skippedBytes = 0;

    /** Number of lines to skip */
    private int skippedLines = 0;

    /** DOCUMENT ME! */
    private float sliceThickness;

    /** RAS, LAS, LPS, RAST, LAST, LPST, SCANNER_XYZ, SCANNER_XYZ_TIME, THREED_RIGHT_HANDED, THREED_LEFT_HANDED,
     *  THREED_RIGHT_HANDED_TIME, or THREED_LEFT_HANDED_TIME */
    @SuppressWarnings("unused")
    private int space = UNKNOWN;

    /** Second index of spaceDirections specifies row of matrix to set. */
    private double[][] spaceDirections = null;

    /** Used to set origin values. */
    private double[] spaceOrigin = null;

    /** DOCUMENT ME! */
    private String spaceUnitsString = null;

    /** Resolutions values for nrrd axes */
    private double[] spacings = null;

    /** DOCUMENT ME! */
    private int[] startBlank;

    /** DOCUMENT ME! */
    private int[] startQuote;

    /** DOCUMENT ME! */
    private int subdim = 0;

    /** Used to obtain MIPAV sliceThickness value */
    private double[] thicknesses = null;

    /** Version of the NRRD file format being used. */
    private float versionNumber;
    
    /** If true, header and data both stored in .nrrd file.
     *  If false, header stored in filename.nhdr and data
     *  stored in filename.raw. */
    private boolean oneFile;
    
    /** version number of NRRD for writing **/
    private static final int writeVersionNumber = 5;

    
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileNRRD(String fName, String fDir) {
        fileName = fName;
        fileDir = fDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        int i, j;
        fileName = null;
        fileDir = null;
        fileInfo = null;
        file = null;
        image = null;
        axisMaxs = null;
        axisMins = null;
        axisOrientation = null;
        baseAfterNumber = null;
        baseBeforeNumber = null;
        centers = null;
        if (dwmriGradient != null) {
            for (i = 0; i < dwmriGradient.length; i++) {
                if (dwmriGradient[i] != null) {
                    for (j = 0; j < dwmriGradient[i].length; j++) {
                        dwmriGradient[i][j] = null;
                    }
                    dwmriGradient[i] = null;
                }
            }
            dwmriGradient = null;
        }
        if (dwmriNex != null) {
            for (i = 0; i < dwmriNex.length; i++) {
                if (dwmriNex[i] != null) {
                    for (j = 0; j < dwmriNex[i].length; j++) {
                        dwmriNex[i][j] = null;
                    }
                    dwmriNex[i] = null;
                }
            }
            dwmriNex = null;
        }
        ext = null;
        fileInfoSub = null;
        fileNameSequence = false;
        if (fileNameSet != null) {
            for (i = 0; i < fileNameSet.length; i++) {
                fileNameSet[i] = null;
            }
            fileNameSet = null;
        }
        finishBlank = null;
        finishQuote = null;
        imgExtents = null;
        if (kindsString != null) {
            for (i = 0; i < kindsString.length; i++) {
                kindsString[i] = null;
            }
            kindsString = null;
        }
        matrix = null;;
        if (measurementFrame != null) {
            for (i = 0; i < measurementFrame.length; i++) {
                measurementFrame[i] = null;
            }
            measurementFrame = null;
        }
        if (mipavLabels != null) {
            for (i = 0; i < mipavLabels.length; i++) {
                mipavLabels[i] = null;
            }
            mipavLabels = null;
        }
        mipavUnits = null;
        if (nrrdLabels != null) {
            for (i = 0; i < nrrdLabels.length; i++) {
                nrrdLabels[i] = null;
            }
            nrrdLabels = null;
        }
        nrrdSizes = null;
        if (nrrdSpaceUnits != null) {
            for (i = 0; i < nrrdSpaceUnits.length; i++) {
                nrrdSpaceUnits[i] = null;
            }
            nrrdSpaceUnits = null;
        }
        if (nrrdUnits != null) {
            for (i = 0; i < nrrdUnits.length; i++) {
                nrrdUnits[i] = null;
            }
            nrrdUnits = null;
        }
        origin = null;
        resols = null;
        if (spaceDirections != null) {
            for (i = 0; i < spaceDirections.length; i++) {
                spaceDirections[i] = null;
            }
            spaceDirections = null;
        }
        spaceOrigin = null;
        spaceUnitsString = null;
        spacings = null;
        startBlank = null;
        startQuote = null;
        thicknesses = null;
        
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * Returns the FileInfoNRRD read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoNRRD getFileInfo() {
        return fileInfo;
    }

    /**
     * Reads the NRRD header and stores the information in fileInfo.
     *
     * @param      imageFileName  File name of image.
     * @param      fileDir        Directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error reading the header
     *
     * @see        FileInfoNRRD
     */
    public boolean readHeader(String imageFileName, String fileDir) throws IOException {
        int i, j, m;
        int index;
        String fileHeaderName;
        String lineString = null;
        int colonIndex;
        int equalIndex;
        String fieldIDString;
        String fieldDescriptorString;
        int numBlanks;
        int lastSlashIndex;
        String keyString;
        String valueString;
        int gradientIndex;
        int startIndex;
        int finishIndex;
        String keyNumberString;
        String formatString;
        int percentIndex;
        int dIndex;
        int periodIndex;
        int startNum;

        // index         = fileName.toLowerCase().indexOf(".img");
        index = fileName.lastIndexOf(".");

        if (fileName.substring(index + 1).equalsIgnoreCase("nrrd")) {
            oneFileStorage = true;
            fileHeaderName = fileName;
        } else {
            oneFileStorage = false;
            fileHeaderName = fileName.substring(0, index) + ".nhdr";
        }

        file = new File(fileDir + fileHeaderName);

        if (file.exists() == false) {
            fileHeaderName = fileName.substring(0, index) + ".NHDR";
            file = new File(fileDir + fileHeaderName);

            if (file.exists() == false) {
                return false;
            }
        }

        raFile = new RandomAccessFile(file, "r");
        fileLength = raFile.length();

        // Check that this is a NRRD file
        lineString = readLine();

        if (lineString == null) {
            raFile.close();
            throw new IOException("The file had no uncommented lines");
        }

        if (!lineString.substring(0, 4).equalsIgnoreCase("NRRD")) {
            raFile.close();
            throw new IOException("Required NRRD magic not found at start of file");
        }

        lineString = lineString.substring(4);
        versionNumber = Float.valueOf(lineString).floatValue();
        fileInfo.setVersionNumber(versionNumber);
        Preferences.debug("versionNumber = " + versionNumber + "\n", Preferences.DEBUG_FILEIO);

        while (lineString != null) {
            lineString = readLine();

            if (lineString != null) {
                equalIndex = lineString.lastIndexOf("=");
                colonIndex = lineString.lastIndexOf(":");

                if ((equalIndex >= 2) && (colonIndex == (equalIndex - 1))) {

                    // Key-value pair present
                    keyString = lineString.substring(0, colonIndex);
                    valueString = lineString.substring(equalIndex + 1).trim();

                    if (keyString.equalsIgnoreCase("MODALITY")) {

                        if (valueString.equalsIgnoreCase("DWMRI")) {
                            Preferences.debug("Modality = Diffusion weighted MRI\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setModality("Diffusion weighted MRI");
                        } else {
                            Preferences.debug("Modality = " + valueString + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setModality(valueString);
                        }
                    } // if (keyString.equalsIgnoreCase("MODALITY"))
                    else if (keyString.equalsIgnoreCase("DWMRI_B-VALUE")) {
                        Preferences.debug("Scalar diffusion weighting b-value = " + valueString + " sec/mm^2\n", 
                        		Preferences.DEBUG_FILEIO);
                        fileInfo.setDWMRI_B_VALUE(valueString);
                    } // else if (keyString.equalsIgnoreCase("DWMRI_B-VALUE"))
                    else if ((keyString.length() >= 19) &&
                                 (keyString.substring(0, 14).equalsIgnoreCase("DWMRI_GRADIENT"))) {
                        Preferences.debug(keyString + " = " + valueString + "\n", Preferences.DEBUG_FILEIO);

                        if (dwmriGradient == null) {
                            dwmriGradient = new String[100][2];
                        }

                        gradientIndex = Integer.valueOf(keyString.substring(15)).intValue();
                        dwmriGradient[gradientIndex][0] = keyString;
                        dwmriGradient[gradientIndex][1] = valueString;
                    } else if ((keyString.length() >= 14) &&
                                   (keyString.substring(0, 9).equalsIgnoreCase("DWMRI_NEX"))) {
                        Preferences.debug(keyString + " = " + valueString + "\n", Preferences.DEBUG_FILEIO);

                        if (dwmriNex == null) {
                            dwmriNex = new String[100][2];
                        }

                        gradientIndex = Integer.valueOf(keyString.substring(10)).intValue();
                        dwmriNex[gradientIndex][0] = keyString;
                        dwmriNex[gradientIndex][1] = valueString;
                    }
                } // if ((equalIndex >= 2) && (colonIndex == (equalIndex - 1)))
                else if (colonIndex >= 1) {

                    // field identifier: field descriptor present
                    fieldIDString = lineString.substring(0, colonIndex);
                    fieldDescriptorString = lineString.substring(colonIndex + 2).trim();

                    if (fieldIDString.equalsIgnoreCase("TYPE")) {

                        if ((fieldDescriptorString.equalsIgnoreCase("SIGNED CHAR")) ||
                                (fieldDescriptorString.equalsIgnoreCase("INT8")) ||
                                (fieldDescriptorString.equalsIgnoreCase("INT8_T"))) {
                            nrrdDataType = ModelStorageBase.BYTE;
                            Preferences.debug("NRRD data type = BYTE\n", Preferences.DEBUG_FILEIO);
                        } else if ((fieldDescriptorString.equalsIgnoreCase("UCHAR")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UNSIGNED CHAR")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UINT8")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UINT8_T"))) {
                            nrrdDataType = ModelStorageBase.UBYTE;
                            Preferences.debug("NRRD data type = UBYTE\n", Preferences.DEBUG_FILEIO);
                        } else if ((fieldDescriptorString.equalsIgnoreCase("SHORT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("SHORT INT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("SIGNED SHORT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("SIGNED SHORT INT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("INT16")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("INT16_T"))) {
                            nrrdDataType = ModelStorageBase.SHORT;
                            Preferences.debug("NRRD data type = SHORT\n", Preferences.DEBUG_FILEIO);
                        } else if ((fieldDescriptorString.equalsIgnoreCase("USHORT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UNSIGNED SHORT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UNSIGNED SHORT INT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UINT16")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UINT16_T"))) {
                            nrrdDataType = ModelStorageBase.USHORT;
                            Preferences.debug("NRRD data type = USHORT\n", Preferences.DEBUG_FILEIO);
                        } else if ((fieldDescriptorString.equalsIgnoreCase("INT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("SIGNED INT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("INT32")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("INT32_T"))) {
                            nrrdDataType = ModelStorageBase.INTEGER;
                            Preferences.debug("NRRD data type = INTEGER\n", Preferences.DEBUG_FILEIO);
                        } else if ((fieldDescriptorString.equalsIgnoreCase("UINT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UNSIGNED INT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UINT32")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UINT32_T"))) {
                            nrrdDataType = ModelStorageBase.UINTEGER;
                            Preferences.debug("NRRD data type = UINTEGER\n", Preferences.DEBUG_FILEIO);
                        } else if ((fieldDescriptorString.equalsIgnoreCase("LONGLONG")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("LONG LONG")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("LONG LONG INT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("SIGNED LONG LONG")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("SIGNED LONG LONG INT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("INT64")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("INT64_T"))) {
                            nrrdDataType = ModelStorageBase.LONG;
                            Preferences.debug("NRRD data type = LONG\n", Preferences.DEBUG_FILEIO);
                        } else if ((fieldDescriptorString.equalsIgnoreCase("ULONGLONG")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UNSIGNED LONG LONG")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UNSIGNED LONG LONG INT")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UINT64")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("UINT64_T"))) {
                            nrrdDataType = ModelStorageBase.LONG;
                            MipavUtil.displayWarning("Warning: Reading unsigned long as signed long");
                            Preferences.debug("Warning: Reading unsigned long as signed long\n", Preferences.DEBUG_FILEIO);
                        } else if (fieldDescriptorString.equalsIgnoreCase("FLOAT")) {
                            nrrdDataType = ModelStorageBase.FLOAT;
                            Preferences.debug("NRRD data type = FLOAT\n", Preferences.DEBUG_FILEIO);
                        } else if (fieldDescriptorString.equalsIgnoreCase("DOUBLE")) {
                            nrrdDataType = ModelStorageBase.DOUBLE;
                            Preferences.debug("NRRD data type = DOUBLE\n", Preferences.DEBUG_FILEIO);
                        } else if (fieldDescriptorString.equalsIgnoreCase("BLOCK")) {
                            MipavUtil.displayError("Cannot handle nrrd block data type");
                            Preferences.debug("NRRD data type = BLOCK\n", Preferences.DEBUG_FILEIO);
                            throw new IOException();
                        } else {
                            MipavUtil.displayError("Unknown NRRD data type = " + fieldDescriptorString);
                            Preferences.debug("Unknown NRRD data type = " + fieldDescriptorString + "\n",
                            		Preferences.DEBUG_FILEIO);
                            throw new IOException();
                        }
                    } // if (fieldIDString.equalsIgnoreCase("TYPE"))
                    else if (fieldIDString.equalsIgnoreCase("DIMENSION")) {
                        nrrdDimensions = Integer.valueOf(fieldDescriptorString).intValue();
                        Preferences.debug("NRRD dimensions = " + nrrdDimensions + "\n", Preferences.DEBUG_FILEIO);
                        startBlank = new int[Math.max(nrrdDimensions - 1, 4)];
                        finishBlank = new int[Math.max(nrrdDimensions - 1, 4)];
                        startQuote = new int[nrrdDimensions];
                        finishQuote = new int[nrrdDimensions];
                    } // else if (fieldIDString.equalsIgnoreCase("DIMENSION"))
                    else if (fieldIDString.equalsIgnoreCase("SIZES")) {
                        nrrdSizes = new int[nrrdDimensions];

                        for (i = 0, j = 0; i < (nrrdDimensions - 1);) {

                            if (!fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                j++;
                            } else {
                                startBlank[i] = j;
                                finishBlank[i] = j++;

                                while (fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                    finishBlank[i] = j++;
                                }

                                if (i == 0) {
                                    nrrdSizes[i] = Integer.valueOf(fieldDescriptorString.substring(0, startBlank[0])).intValue();
                                    Preferences.debug("NRRD sizes[" + i + "] = " + nrrdSizes[i] + "\n", Preferences.DEBUG_FILEIO);
                                } else {
                                    nrrdSizes[i] = Integer.valueOf(fieldDescriptorString.substring(finishBlank[i - 1] +
                                                                                                   1, startBlank[i])).intValue();
                                    Preferences.debug("NRRD sizes[" + i + "] = " + nrrdSizes[i] + "\n", Preferences.DEBUG_FILEIO);
                                }

                                i++;
                            }
                        } // for (i = 0; j = 0; i < nrrdDimensions-1;)

                        nrrdSizes[nrrdDimensions - 1] = Integer.valueOf(fieldDescriptorString.substring(finishBlank[nrrdDimensions -
                                                                                                                    2] +
                                                                                                        1)).intValue();
                        Preferences.debug("NRRD sizes[" + i + "] = " + nrrdSizes[i] + "\n", Preferences.DEBUG_FILEIO);
                    } // else if (fieldIDString.equalsIgnoreCase("SIZES"))
                    else if (fieldIDString.equalsIgnoreCase("ENDIAN")) {

                        if (fieldDescriptorString.equalsIgnoreCase("BIG")) {
                            endianess = FileBase.BIG_ENDIAN;
                            fileInfo.setEndianess(endianess);
                            Preferences.debug("BIG ENDIAN\n", Preferences.DEBUG_FILEIO);
                        } else if (fieldDescriptorString.equalsIgnoreCase("LITTLE")) {
                            endianess = FileBase.LITTLE_ENDIAN;
                            fileInfo.setEndianess(endianess);
                            Preferences.debug("LITTLE ENDIAN\n", Preferences.DEBUG_FILEIO);
                        } else {
                            MipavUtil.displayError("Illegal endian value of " + fieldDescriptorString);
                            Preferences.debug("Illegal endian value of " + fieldDescriptorString + "\n", Preferences.DEBUG_FILEIO);
                            throw new IOException();
                        }
                    } // else if (fieldIDString.equalsIgnoreCase("ENDIAN"))
                    else if ((fieldIDString.equalsIgnoreCase("LINE SKIP")) ||
                                 (fieldIDString.equalsIgnoreCase("LINESKIP"))) {
                        skippedLines = Integer.valueOf(fieldDescriptorString).intValue();
                        Preferences.debug("Skipped lines = " + skippedLines + "\n", Preferences.DEBUG_FILEIO);
                    } // else if ((fieldIDString.equalsIgnoreCase("LINE SKIP")) ||
                    else if ((fieldIDString.equalsIgnoreCase("BYTE SKIP")) ||
                                 (fieldIDString.equalsIgnoreCase("BYTESKIP"))) {
                        skippedBytes = Integer.valueOf(fieldDescriptorString).intValue();
                        Preferences.debug("Skipped bytes = " + skippedBytes + "\n", Preferences.DEBUG_FILEIO);
                    } // else if ((fieldIDString.equalsIgnoreCase("BYTE SKIP")) ||
                    else if ((fieldIDString.equalsIgnoreCase("DATA FILE")) ||
                                 (fieldIDString.equalsIgnoreCase("DATAFILE"))) {
                        numBlanks = 0;

                        for (i = 0, j = 0; j < fieldDescriptorString.length();) {

                            if (!fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                j++;
                            } else {
                                numBlanks++;
                                startBlank[i] = j;
                                finishBlank[i] = j++;

                                while (fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                    finishBlank[i] = j++;
                                }

                                i++;
                            }
                        }

                        if ((numBlanks == 0) && (!fieldDescriptorString.equalsIgnoreCase("LIST"))) {

                            // There is a single detached data file and its file directory
                            // and fileName are given by fieldDescriptorString
                            if ((fieldDescriptorString.substring(0, 1).equals(".")) &&
                                    (fieldDescriptorString.substring(1, 2).equals("/"))) {
                                lastSlashIndex = fieldDescriptorString.lastIndexOf("/");

                                if (lastSlashIndex > 1) {
                                    fileDir = fileDir.concat(fieldDescriptorString.substring(1, lastSlashIndex + 1));
                                    Preferences.debug("Data file directory = " + fileDir + "\n", Preferences.DEBUG_FILEIO);
                                    fileInfo.setFileDirectory(fileDir);
                                    fileName = fieldDescriptorString.substring(lastSlashIndex + 1);
                                    Preferences.debug("Data file name = " + fileName + "\n", Preferences.DEBUG_FILEIO);
                                    fileInfo.setFileName(fileName);
                                } else {
                                    fileName = fieldDescriptorString.substring(2);
                                    Preferences.debug("Data file name = " + fileName + "\n", Preferences.DEBUG_FILEIO);
                                    fileInfo.setFileName(fileName);
                                }
                            } else if (fieldDescriptorString.substring(0, 1).equals(".")) {
                                lastSlashIndex = fieldDescriptorString.lastIndexOf("/");
                                fileDir = fieldDescriptorString.substring(0, lastSlashIndex + 1);
                                Preferences.debug("Data file directory = " + fileDir + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setFileDirectory(fileName);
                                fileName = fieldDescriptorString.substring(lastSlashIndex + 1);
                                Preferences.debug("Data file name = " + fileName + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setFileName(fileName);
                            } else {
                                fileName = fieldDescriptorString;
                                Preferences.debug("Data file name = " + fileName + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setFileName(fileName);
                            }
                        } // if ((numBlanks == 0) && (!fieldDescriptorString.equalsIgnoreCase("LIST")))
                        else if (numBlanks >= 3) {

                            // Autosequencing is performed and the fieldDescriptor string contains
                            // <format> <min> <max> <step> [<subdim>]
                            autoSequence = true;
                            formatString = fieldDescriptorString.substring(0, startBlank[0]);
                            percentIndex = formatString.lastIndexOf("%");
                            dIndex = 0;

                            for (i = percentIndex + 1; dIndex == 0; i++) {

                                if (formatString.substring(i, i + 1).equalsIgnoreCase("D")) {
                                    dIndex = i;
                                }
                            }

                            if (dIndex == (percentIndex + 1)) {
                                paddingNumber = 0;
                            } else {
                                paddingNumber = Integer.valueOf(formatString.substring(percentIndex + 1, dIndex)).intValue();
                            }

                            periodIndex = formatString.lastIndexOf(".");

                            if (periodIndex > dIndex) {

                                // Increment in file base
                                baseNumber = true;
                                ext = formatString.substring(periodIndex + 1);
                            } else {

                                // Increment in file extension
                                baseNumber = false;
                                ext = null;
                            }

                            if (percentIndex == 0) {
                                baseBeforeNumber = null;
                            } else if (percentIndex < periodIndex) {
                                baseBeforeNumber = formatString.substring(0, percentIndex);
                            } else { // percentIndex > periodIndex
                                baseBeforeNumber = formatString.substring(0, periodIndex);
                            }

                            if (periodIndex > (dIndex + 1)) {
                                baseAfterNumber = formatString.substring(dIndex + 1, periodIndex);
                            } else {
                                baseAfterNumber = null;
                            }

                            sequenceStart = Integer.valueOf(fieldDescriptorString.substring(finishBlank[0] + 1,
                                                                                            startBlank[1])).intValue();
                            sequenceFinish = Integer.valueOf(fieldDescriptorString.substring(finishBlank[1] + 1,
                                                                                             startBlank[2])).intValue();

                            if (numBlanks == 3) {
                                sequenceStep = Integer.valueOf(fieldDescriptorString.substring(finishBlank[2] + 1)).intValue();
                            } // if (numBlanks == 3)
                            else {
                                sequenceStep = Integer.valueOf(fieldDescriptorString.substring(finishBlank[2] + 1,
                                                                                               startBlank[3])).intValue();
                                subdim = Integer.valueOf(fieldDescriptorString.substring(finishBlank[3] + 1)).intValue();
                            }
                        } // else if (numBlanks >= 3)
                        else {

                            // fieldDescriptorString is of the form LIST <subdim>
                            // There are multiple detached data files, and their filenames
                            // are given explicitly, one filename per line, in the line
                            // starting after the data file field specification, until the
                            // end of file
                            fileNameSequence = true;

                            if (numBlanks == 1) {
                                subdim = Integer.valueOf(fieldDescriptorString.substring(finishBlank[0] + 1)).intValue();
                            } // if (numBlanks == 1)

                            fileNameSet = new String[1000];

                            while ((lineString = readLine()) != null) {
                                fileNameSet[fileNumber++] = lineString;
                            }

                        }
                    } // else if ((fieldIDString.equalsIgnoreCase("DATA FILE"))
                    else if (fieldIDString.equalsIgnoreCase("ENCODING")) {

                        if (fieldDescriptorString.equalsIgnoreCase("RAW")) {
                            Preferences.debug("Encoding = raw\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setEncoding("Raw");
                        } else if ((fieldDescriptorString.equalsIgnoreCase("GZ")) ||
                                       (fieldDescriptorString.equalsIgnoreCase("GZIP"))) {
                            Preferences.debug("Encoding = gzip\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setEncoding("Gzip");
                            gunzip = true;
                        }
                    } // else if (fieldIDString.equalsIgnoreCase("ENCODING"))
                    else if (fieldIDString.equalsIgnoreCase("CONTENT")) {
                        Preferences.debug("Content = " + fieldDescriptorString + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setContent(fieldDescriptorString);
                    } // else if (fieldIDString.equalsIgnoreCase("CONTENT"))
                    else if (fieldIDString.equalsIgnoreCase("SPACINGS")) {
                        spacings = new double[nrrdDimensions];

                        for (i = 0, j = 0; i < (nrrdDimensions - 1);) {

                            if (!fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                j++;
                            } else {
                                startBlank[i] = j;
                                finishBlank[i] = j++;

                                while (fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                    finishBlank[i] = j++;
                                }

                                if (i == 0) {

                                    if (fieldDescriptorString.substring(0, 3).equalsIgnoreCase("NAN")) {
                                        spacings[i] = Double.NaN;
                                    } else {
                                        spacings[i] = Double.valueOf(fieldDescriptorString.substring(0, startBlank[0])).doubleValue();
                                    }

                                    Preferences.debug("NRRD spacings[" + i + "] = " + spacings[i] + "\n", 
                                    		Preferences.DEBUG_FILEIO);
                                } else {

                                    if (fieldDescriptorString.substring(finishBlank[i - 1] + 1, startBlank[i]).equalsIgnoreCase("NAN")) {
                                        spacings[i] = Double.NaN;
                                    } else {
                                        spacings[i] = Double.valueOf(fieldDescriptorString.substring(finishBlank[i - 1] +
                                                                                                     1, startBlank[i])).doubleValue();
                                    }

                                    Preferences.debug("NRRD spacings[" + i + "] = " + spacings[i] + "\n", 
                                    		Preferences.DEBUG_FILEIO);
                                }

                                i++;
                            }
                        } // for (i = 0; j = 0; i < nrrdDimensions-1;)

                        if (fieldDescriptorString.substring(finishBlank[nrrdDimensions - 2] + 1).equalsIgnoreCase("NAN")) {
                            spacings[nrrdDimensions - 1] = Double.NaN;
                        } else {
                            spacings[nrrdDimensions - 1] = Double.valueOf(fieldDescriptorString.substring(finishBlank[nrrdDimensions -
                                                                                                                      2] +
                                                                                                          1)).doubleValue();
                        }

                        Preferences.debug("NRRD spacings[" + i + "] = " + spacings[i] + "\n", Preferences.DEBUG_FILEIO);
                    } // else if (fieldIDString.equalsIgnoreCase("SPACINGS"))
                    else if ((fieldIDString.equalsIgnoreCase("AXIS MINS")) ||
                                 (fieldIDString.equalsIgnoreCase("AXISMINS"))) {
                        axisMins = new double[nrrdDimensions];

                        for (i = 0, j = 0; i < (nrrdDimensions - 1);) {

                            if (!fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                j++;
                            } else {
                                startBlank[i] = j;
                                finishBlank[i] = j++;

                                while (fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                    finishBlank[i] = j++;
                                }

                                if (i == 0) {

                                    if (fieldDescriptorString.substring(0, startBlank[0]).equalsIgnoreCase("NAN")) {
                                        axisMins[i] = Double.NaN;
                                    } else {
                                        axisMins[i] = Double.valueOf(fieldDescriptorString.substring(0, startBlank[0])).doubleValue();
                                    }

                                    Preferences.debug("NRRD axis minimum[" + i + "] = " + axisMins[i] + "\n", 
                                    		Preferences.DEBUG_FILEIO);
                                } else {

                                    if (fieldDescriptorString.substring(finishBlank[i - 1] + 1, startBlank[i]).equalsIgnoreCase("NAN")) {
                                        axisMins[i] = Double.NaN;
                                    } else {
                                        axisMins[i] = Double.valueOf(fieldDescriptorString.substring(finishBlank[i - 1] +
                                                                                                     1, startBlank[i])).doubleValue();
                                    }

                                    Preferences.debug("NRRD axis minimum[" + i + "] = " + axisMins[i] + "\n", 
                                    		Preferences.DEBUG_FILEIO);
                                }

                                i++;
                            }
                        } // for (i = 0; j = 0; i < nrrdDimensions-1;)

                        if (fieldDescriptorString.substring(finishBlank[nrrdDimensions - 2] + 1).equalsIgnoreCase("NAN")) {
                            axisMins[nrrdDimensions - 1] = Double.NaN;
                        } else {
                            axisMins[nrrdDimensions - 1] = Double.valueOf(fieldDescriptorString.substring(finishBlank[nrrdDimensions -
                                                                                                                      2] +
                                                                                                          1)).doubleValue();
                        }

                        Preferences.debug("NRRD axis minimum[" + i + "] = " + axisMins[i] + "\n", Preferences.DEBUG_FILEIO);
                    } // else if (fieldIDString.equalsIgnoreCase("AXIS MINS")) ||
                    else if ((fieldIDString.equalsIgnoreCase("AXIS MAXS")) ||
                                 (fieldIDString.equalsIgnoreCase("AXISMAXS"))) {
                        axisMaxs = new double[nrrdDimensions];

                        for (i = 0, j = 0; i < (nrrdDimensions - 1);) {

                            if (!fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                j++;
                            } else {
                                startBlank[i] = j;
                                finishBlank[i] = j++;

                                while (fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                    finishBlank[i] = j++;
                                }

                                if (i == 0) {

                                    if (fieldDescriptorString.substring(0, startBlank[0]).equalsIgnoreCase("NAN")) {
                                        axisMaxs[i] = Double.NaN;
                                    } else {
                                        axisMaxs[i] = Double.valueOf(fieldDescriptorString.substring(0, startBlank[0])).doubleValue();
                                    }

                                    Preferences.debug("NRRD axis maximum[" + i + "] = " + axisMaxs[i] + "\n", 
                                    		Preferences.DEBUG_FILEIO);
                                } else {

                                    if (fieldDescriptorString.substring(finishBlank[i - 1] + 1, startBlank[i]).equalsIgnoreCase("NAN")) {
                                        axisMaxs[i] = Double.NaN;
                                    } else {
                                        axisMaxs[i] = Double.valueOf(fieldDescriptorString.substring(finishBlank[i - 1] +
                                                                                                     1, startBlank[i])).doubleValue();
                                    }

                                    Preferences.debug("NRRD axis maximum[" + i + "] = " + axisMaxs[i] + "\n", 
                                    		Preferences.DEBUG_FILEIO);
                                }

                                i++;
                            }
                        } // for (i = 0; j = 0; i < nrrdDimensions-1;)

                        if (fieldDescriptorString.substring(finishBlank[nrrdDimensions - 2] + 1).equalsIgnoreCase("NAN")) {
                            axisMaxs[nrrdDimensions - 1] = Double.NaN;
                        } else {
                            axisMaxs[nrrdDimensions - 1] = Double.valueOf(fieldDescriptorString.substring(finishBlank[nrrdDimensions -
                                                                                                                      2] +
                                                                                                          1)).doubleValue();
                        }

                        Preferences.debug("NRRD axis maximum[" + i + "] = " + axisMaxs[i] + "\n", Preferences.DEBUG_FILEIO);
                    } // else if (fieldIDString.equalsIgnoreCase("AXIS MAXS")) ||
                    else if (fieldIDString.equalsIgnoreCase("UNITS")) {
                        nrrdUnits = new String[nrrdDimensions];

                        for (i = 0, j = 0; i < nrrdDimensions;) {

                            if (!fieldDescriptorString.substring(j, j + 1).equals("\"")) {
                                j++;
                            } else {
                                startQuote[i] = j++;

                                while (!fieldDescriptorString.substring(j, j + 1).equals("\"")) {
                                    j++;
                                }

                                finishQuote[i++] = j++;
                            }
                        } // for (i = 0, j = 0; i < nrrdDimensions;)

                        for (i = 0; i < nrrdDimensions; i++) {
                            nrrdUnits[i] = fieldDescriptorString.substring(startQuote[i] + 1, finishQuote[i]);
                            Preferences.debug("NRRD units[ " + i + "] = " + nrrdUnits[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    } // else if (fieldIDString.equalsIgnoreCase("UNITS))
                    else if (fieldIDString.equalsIgnoreCase("LABELS")) {
                        nrrdLabels = new String[nrrdDimensions];

                        for (i = 0, j = 0; i < nrrdDimensions;) {

                            if (!fieldDescriptorString.substring(j, j + 1).equals("\"")) {
                                j++;
                            } else {
                                startQuote[i] = j++;

                                while (!fieldDescriptorString.substring(j, j + 1).equals("\"")) {
                                    j++;
                                }

                                finishQuote[i++] = j++;
                            }
                        } // for (i = 0, j = 0; i < nrrdDimensions;)

                        for (i = 0; i < nrrdDimensions; i++) {
                            nrrdLabels[i] = fieldDescriptorString.substring(startQuote[i] + 1, finishQuote[i]);
                            Preferences.debug("NRRD labels[ " + i + "] = " + nrrdLabels[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    } // else if (fieldIDString.equalsIgnoreCase("LABELS))
                    else if (fieldIDString.equalsIgnoreCase("KINDS")) {
                        kindsString = new String[nrrdDimensions];

                        for (i = 0, j = 0; i < (nrrdDimensions - 1);) {

                            if (!fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                j++;
                            } else {
                                startBlank[i] = j;
                                finishBlank[i] = j++;

                                while (fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                    finishBlank[i] = j++;
                                }

                                if (i == 0) {
                                    kindsString[i] = fieldDescriptorString.substring(0, startBlank[0]);
                                    Preferences.debug("NRRD kind[" + i + "] = " + kindsString[i] + "\n", 
                                    		Preferences.DEBUG_FILEIO);
                                } else {
                                    kindsString[i] = fieldDescriptorString.substring(finishBlank[i - 1] + 1,
                                                                                     startBlank[i]);
                                    Preferences.debug("NRRD kind[" + i + "] = " + kindsString[i] + "\n", 
                                    		Preferences.DEBUG_FILEIO);
                                }

                                i++;
                            }
                        } // for (i = 0; j = 0; i < nrrdDimensions-1;)

                        kindsString[nrrdDimensions - 1] = fieldDescriptorString.substring(finishBlank[nrrdDimensions - 2] +
                                                                                          1);
                        Preferences.debug("NRRD kind[" + i + "] = " + kindsString[i] + "\n", 
                        		Preferences.DEBUG_FILEIO);

                        for (i = 0; i < nrrdDimensions; i++) {

                            if (kindsString[i] != null) {

                                if (kindsString[i].equalsIgnoreCase("SPACE")) {
                                    numSpaceKinds++;
                                }
                            }
                        }
                    } // else if (fieldIDString.equalsIgnoreCase("KINDS"))
                    else if (fieldIDString.equalsIgnoreCase("SPACE UNITS")) {
                        spaceUnitsString = fieldDescriptorString;
                    } // else if (fieldIDString.equalsIgnoreCase("SPACE UNITS"))
                    else if (fieldIDString.equalsIgnoreCase("THICKNESSES")) {
                        thicknesses = new double[nrrdDimensions];

                        for (i = 0, j = 0; i < (nrrdDimensions - 1);) {

                            if (!fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                j++;
                            } else {
                                startBlank[i] = j;
                                finishBlank[i] = j++;

                                while (fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                    finishBlank[i] = j++;
                                }

                                if (i == 0) {

                                    if (fieldDescriptorString.substring(0, startBlank[0]).equalsIgnoreCase("NAN")) {
                                        thicknesses[i] = Double.NaN;
                                    } else {
                                        thicknesses[i] = Double.valueOf(fieldDescriptorString.substring(0,
                                                                                                        startBlank[0])).doubleValue();
                                        numThicknesses++;
                                        sliceThickness = (float) thicknesses[i];
                                    }

                                    Preferences.debug("NRRD axis thickness[" + i + "] = " + thicknesses[i] + "\n", 
                                    		Preferences.DEBUG_FILEIO);
                                } else {

                                    if (fieldDescriptorString.substring(finishBlank[i - 1] + 1, startBlank[i]).equalsIgnoreCase("NAN")) {
                                        thicknesses[i] = Double.NaN;
                                    } else {
                                        thicknesses[i] = Double.valueOf(fieldDescriptorString.substring(finishBlank[i -
                                                                                                                    1] +
                                                                                                        1,
                                                                                                        startBlank[i])).doubleValue();
                                        numThicknesses++;
                                        sliceThickness = (float) thicknesses[i];
                                    }

                                    Preferences.debug("NRRD axis thickness[" + i + "] = " + thicknesses[i] + "\n", 
                                    		Preferences.DEBUG_FILEIO);
                                }

                                i++;
                            }
                        } // for (i = 0; j = 0; i < nrrdDimensions-1;)

                        if (fieldDescriptorString.substring(finishBlank[nrrdDimensions - 2] + 1).equalsIgnoreCase("NAN")) {
                            thicknesses[nrrdDimensions - 1] = Double.NaN;
                        } else {
                            thicknesses[nrrdDimensions - 1] = Double.valueOf(fieldDescriptorString.substring(finishBlank[nrrdDimensions -
                                                                                                                         2] +
                                                                                                             1)).doubleValue();
                            numThicknesses++;
                            sliceThickness = (float) thicknesses[i];
                        }

                        Preferences.debug("NRRD axis thickness[" + i + "] = " + thicknesses[i] + "\n", 
                        		Preferences.DEBUG_FILEIO);
                    } // else if (fieldIDString.equalsIgnoreCase("THICKNESSES"))
                    else if (fieldIDString.equalsIgnoreCase("SPACE")) {

                        if ((fieldDescriptorString.equalsIgnoreCase("RIGHT-ANTERIOR-SUPERIOR")) ||
                                (fieldDescriptorString.equalsIgnoreCase("RAS"))) {
                            Preferences.debug("Original NRRD Space = right-anterior-superior\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("New MIPAV Space = left-posterior-superior\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setSpace("left-posterior-superior");
                            space = LPS;
                            rlInvert = true;
                            apInvert = true;
                        } // if ((fieldDescriptorString.equalsIgnoreCase("RIGHT-ANTERIOR-SUPERIOR")) ||
                        else if ((fieldDescriptorString.equalsIgnoreCase("LEFT-ANTERIOR-SUPERIOR")) ||
                                     (fieldDescriptorString.equalsIgnoreCase("LAS"))) {
                            Preferences.debug("Original NRRD Space = left-anterior-superior\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("New MIPAV Space = left-posterior-superior\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setSpace("left-posterior-superior");
                            space = LPS;
                            apInvert = true;
                        } // else if ((fieldDescriptorString.equalsIgnoreCase("LEFT-ANTERIOR-SUPERIOR")) ||
                        else if ((fieldDescriptorString.equalsIgnoreCase("LEFT-POSTERIOR-SUPERIOR")) ||
                                     (fieldDescriptorString.equalsIgnoreCase("LPS"))) {
                            Preferences.debug("Space = left-posterior-superior\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setSpace("left-posterior-superior");
                            space = LPS;
                        } // else if ((fieldDescriptorString.equalsIgnoreCase("LEFT-POSTERIOR-SUPERIOR")) ||
                        else if ((fieldDescriptorString.equalsIgnoreCase("RIGHT-ANTERIOR-SUPERIOR-TIME")) ||
                                     (fieldDescriptorString.equalsIgnoreCase("RAST"))) {
                            Preferences.debug("Original NRRD Space = right-anterior-superior-time\n",
                            		Preferences.DEBUG_FILEIO);
                            Preferences.debug("New MIPAV Space = left-posterior-superior-time\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setSpace("left-posterior-superior-time");
                            space = LPST;
                            rlInvert = true;
                            apInvert = true;
                        } // else if ((fieldDescriptorString.equalsIgnoreCase("RIGHT-ANTERIOR-SUPERIOR-TIME")) ||
                        else if ((fieldDescriptorString.equalsIgnoreCase("LEFT-ANTERIOR-SUPERIOR-TIME")) ||
                                     (fieldDescriptorString.equalsIgnoreCase("LAST"))) {
                            Preferences.debug("Original NRRD Space = left-anterior-superior-time\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("New MIPAV Space = left-posterior-superior-time\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setSpace("left-posterior-superior-time");
                            space = LPST;
                            apInvert = true;
                        } // else if ((fieldDescriptorString.equalsIgnoreCase("LEFT-ANTERIOR-SUPERIOR-TIME")) ||
                        else if ((fieldDescriptorString.equalsIgnoreCase("LEFT-POSTERIOR-SUPERIOR-TIME")) ||
                                     (fieldDescriptorString.equalsIgnoreCase("LPST"))) {
                            Preferences.debug("Space = left-posterior-superior-time\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setSpace("left-posterior-superior-time");
                            space = LPST;
                        } // else if ((fieldDescriptorString.equalsIgnoreCase("LEFT-POSTERIOR-SUPERIOR-TIME")) ||
                        else if (fieldDescriptorString.equalsIgnoreCase("SCANNER-XYZ")) {
                            Preferences.debug("Space = scanner-xyz\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setSpace("scanner-xyz");
                            space = SCANNER_XYZ;
                        } // else if (fieldDescriptorString.equalsIgnoreCase("SCANNER-XYZ"))
                        else if (fieldDescriptorString.equalsIgnoreCase("SCANNER-XYZ-TIME")) {
                            Preferences.debug("Space = scanner-xyz-time\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setSpace("scanner-xyz-time");
                            space = SCANNER_XYZ_TIME;
                        } // else if (fieldDescriptorString.equalsIgnoreCase("SCANNER-XYZ-TIME"))
                        else if (fieldDescriptorString.equalsIgnoreCase("3D-RIGHT-HANDED")) {
                            Preferences.debug("Space = 3d-right-handed\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setSpace("3d-right-handed");
                            space = THREED_RIGHT_HANDED;
                        } // else if (fieldDescriptorString.equalsIgnoreCase("3D-RIGHT-HANDED"))
                        else if (fieldDescriptorString.equalsIgnoreCase("3D-LEFT-HANDED")) {
                            Preferences.debug("Space = 3d-left-handed\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setSpace("3d-left-handed");
                            space = THREED_LEFT_HANDED;
                        } // else if (fieldDescriptorString.equalsIgnoreCase("3D-LEFT-HANDED"))
                        else if (fieldDescriptorString.equalsIgnoreCase("3D-RIGHT-HANDED-TIME")) {
                            Preferences.debug("Space = 3d-right-handed-time\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setSpace("3d-right-handed-time");
                            space = THREED_RIGHT_HANDED_TIME;
                        } // else if (fieldDescriptorString.equalsIgnoreCase("3D-RIGHT-HANDED-TIME"))
                        else if (fieldDescriptorString.equalsIgnoreCase("3D-LEFT-HANDED-TIME")) {
                            Preferences.debug("Space = 3d-left-handed-time\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setSpace("3d-left-handed-time");
                            space = THREED_LEFT_HANDED_TIME;
                        } // else if (fieldDescriptorString.equalsIgnoreCase("3D-LEFT-HANDED-TIME"))
                    } // else if (fieldIDString.equalsIgnoreCase("SPACE"))
                    else if (fieldIDString.equalsIgnoreCase("SPACE DIRECTIONS")) {
                    	int numComma = 0;
                    	for (j = 0; j < fieldDescriptorString.length(); j++) {
                    		if (fieldDescriptorString.substring(j, j+ 1).equalsIgnoreCase(",")) {
                    			numComma++;
                    		}
                    	}
                    	if (numComma == 6) {
                            spaceDirections = new double[nrrdDimensions][3];
                    	}
                    	else {
                    		spaceDirections = new double[nrrdDimensions][4];
                    	}

                        for (i = 0, j = 0, m = 0; i < nrrdDimensions;) {

                            while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                j++;
                            }

                            if (fieldDescriptorString.substring(j, j + 4).equalsIgnoreCase("NONE")) {
                                spaceDirections[i][0] = Double.NaN;
                                spaceDirections[i][1] = Double.NaN;
                                spaceDirections[i][2] = Double.NaN;
                                Preferences.debug("space directions[" + i + " ] = none\n", Preferences.DEBUG_FILEIO);
                                i++;
                                j += 4;
                            } else if (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase("(")) {
                                j++;

                                while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                    j++;
                                }

                                startNum = j;

                                while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(",")) {
                                    j++;
                                }

                                spaceDirections[i][0] = Double.valueOf(fieldDescriptorString.substring(startNum, j++)).doubleValue();
                                if (m < 3) {
                                    matrix.set(0, m, spaceDirections[i][0]);
                                }

                                while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                    j++;
                                }

                                startNum = j;

                                while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(",")) {
                                    j++;
                                }

                                spaceDirections[i][1] = Double.valueOf(fieldDescriptorString.substring(startNum, j++)).doubleValue();
                                if (m < 3) {
                                    matrix.set(1, m, spaceDirections[i][1]);
                                }

                                while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                    j++;
                                }

                                startNum = j;

                                if (numComma == 6) {
	                                while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(")")) {
	                                    j++;
	                                }
                                }
                                else {
                                	while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(",")) {
                                        j++;
                                    }	
                                }

                                spaceDirections[i][2] = Double.valueOf(fieldDescriptorString.substring(startNum, j++)).doubleValue();
                                if (m < 3) {
                                    matrix.set(2, m, spaceDirections[i][2]);
                                }
                                if (numComma == 6) {
	                                Preferences.debug("space directions[" + i + " ] = (" + spaceDirections[i][0] + "," +
	                                                  spaceDirections[i][1] + "," + spaceDirections[i][2] + ")\n", 
	                                                  Preferences.DEBUG_FILEIO);
                                }
                                else {
                                	while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                        j++;
                                    }

                                    startNum = j;
                                    
                                    while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(")")) {
	                                    j++;
	                                }
                                    
                                    spaceDirections[i][3] = Double.valueOf(fieldDescriptorString.substring(startNum, j++)).doubleValue();
                                    
                                    Preferences.debug("space directions[" + i + " ] = (" + spaceDirections[i][0] + "," +
                                            spaceDirections[i][1] + "," + spaceDirections[i][2] + "," + 
                                            spaceDirections[i][3] + ")\n", Preferences.DEBUG_FILEIO);
                                }
                                i++;
                                m++;
                            }
                        }
                    } // else if (fieldIDString.equalsIgnoreCase("SPACE DIRECTIONS"))
                    else if (fieldIDString.equalsIgnoreCase("SPACE ORIGIN")) {

                        // This always refers to the center of the first sample in the array regardless of
                        // cell or node centering of the data is specified.  This field conveys the same
                        // information as the "Image Position" (0020,0032) field of a DICOM file, except
                        // that the space in which the vector coordinates are given need not be the
                        // DICOM-specific LPS space.
                    	int numComma = 0;
                    	for (j = 0; j < fieldDescriptorString.length(); j++) {
                    		if (fieldDescriptorString.substring(j, j+ 1).equalsIgnoreCase(",")) {
                    			numComma++;
                    		}
                    	}
                        spaceOrigin = new double[numComma+1];
                        j = 0;

                        while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                            j++;
                        }

                        if (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase("(")) {
                            j++;

                            while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                j++;
                            }

                            startNum = j;

                            while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(",")) {
                                j++;
                            }

                            spaceOrigin[0] = Double.valueOf(fieldDescriptorString.substring(startNum, j++)).doubleValue();

                            while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                j++;
                            }

                            startNum = j;

                            while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(",")) {
                                j++;
                            }

                            spaceOrigin[1] = Double.valueOf(fieldDescriptorString.substring(startNum, j++)).doubleValue();

                            while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                j++;
                            }

                            startNum = j;

                            if (numComma == 2) {
	                            while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(")")) {
	                                j++;
	                            }
                            }
                            else {
                            	while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(",")) {
                                    j++;
                                }	
                            }

                            spaceOrigin[2] = Double.valueOf(fieldDescriptorString.substring(startNum, j++)).doubleValue();
                            if (numComma == 2) {
                                Preferences.debug("space origin = (" + spaceOrigin[0] + "," + spaceOrigin[1] + "," +
                                                  spaceOrigin[2] + ")\n", Preferences.DEBUG_FILEIO);
                            }
                            else {
                            	while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                    j++;
                                }

                                startNum = j;
                                
                                while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(")")) {
	                                j++;
	                            }
                                
                                spaceOrigin[3] = Double.valueOf(fieldDescriptorString.substring(startNum, j++)).doubleValue();
                                
                                Preferences.debug("space origin = (" + spaceOrigin[0] + "," + spaceOrigin[1] + "," +
                                        spaceOrigin[2] + "," + spaceOrigin[3] + ")\n", Preferences.DEBUG_FILEIO);
                            }
                            origin = new float[numComma+1];
                            origin[0] = (float) spaceOrigin[0];
                            origin[1] = (float) spaceOrigin[1];
                            origin[2] = (float) spaceOrigin[2];
                            if (numComma == 3) {
                            	origin[3] = (float) spaceOrigin[3];
                            }
                            fileInfo.setOrigin(origin);
                            matrix.set(0, 3, spaceOrigin[0]);
                            matrix.set(1, 3, spaceOrigin[1]);
                            matrix.set(2, 3, spaceOrigin[2]);
                        }
                    } // else if (fieldIDString.equalsIgnoreCase("SPACE ORIGIN"))
                    else if (fieldIDString.equalsIgnoreCase("MEASUREMENT FRAME")) {
                        measurementFrame = new double[3][3];

                        for (i = 0, j = 0; i < 3;) {

                            while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                j++;
                            }

                            if (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase("(")) {
                                j++;

                                while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                    j++;
                                }

                                startNum = j;

                                while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(",")) {
                                    j++;
                                }

                                measurementFrame[i][0] = Double.valueOf(fieldDescriptorString.substring(startNum, j++)).doubleValue();

                                while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                    j++;
                                }

                                startNum = j;

                                while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(",")) {
                                    j++;
                                }

                                measurementFrame[i][1] = Double.valueOf(fieldDescriptorString.substring(startNum, j++)).doubleValue();

                                while (fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(" ")) {
                                    j++;
                                }

                                startNum = j;

                                while (!fieldDescriptorString.substring(j, j + 1).equalsIgnoreCase(")")) {
                                    j++;
                                }

                                measurementFrame[i][2] = Double.valueOf(fieldDescriptorString.substring(startNum, j++)).doubleValue();
                                Preferences.debug("measurement frame[" + i + " ] = (" + measurementFrame[i][0] + "," +
                                                  measurementFrame[i][1] + "," + measurementFrame[i][2] + ")\n", 
                                                  Preferences.DEBUG_FILEIO);
                                i++;
                            }
                        }
                    } // else if (fieldIDString.equalsIgnoreCase("MEASUREMENT FRAME"))
                    else if ((fieldIDString.equalsIgnoreCase("CENTERS")) ||
                                 (fieldIDString.equalsIgnoreCase("CENTERINGS"))) {
                        centers = new int[nrrdDimensions];

                        for (i = 0, j = 0; i < nrrdDimensions;) {

                            while (fieldDescriptorString.substring(j, j + 1).equals(" ")) {
                                j++;
                            }

                            if ((fieldDescriptorString.substring(j).length() >= 4) &&
                                    (fieldDescriptorString.substring(j, j + 4).equalsIgnoreCase("CELL"))) {
                                centers[i] = CELL;
                                Preferences.debug("centers[ " + i + "] = cell\n", Preferences.DEBUG_FILEIO);
                                i++;
                                j += 4;
                            } else if ((fieldDescriptorString.substring(j).length() >= 4) &&
                                           (fieldDescriptorString.substring(j, j + 4).equalsIgnoreCase("NODE"))) {
                                centers[i] = NODE;
                                Preferences.debug("centers[ " + i + "] = node\n", Preferences.DEBUG_FILEIO);
                                i++;
                                j += 4;
                            } else if ((fieldDescriptorString.substring(j).length() >= 4) &&
                                           (fieldDescriptorString.substring(j, j + 4).equalsIgnoreCase("NONE"))) {
                                centers[i] = NONE;
                                Preferences.debug("centers[ " + i + "] = none\n", Preferences.DEBUG_FILEIO);
                                i++;
                                j += 4;
                            } else if (fieldDescriptorString.substring(j, j + 3).equalsIgnoreCase("???")) {
                                centers[i] = NONE;
                                Preferences.debug("centers[ " + i + "] = none\n", Preferences.DEBUG_FILEIO);
                                i++;
                                j += 3;
                            }
                        }
                    } // else if ((fieldIDString.equalsIgnoreCase("CENTERS")) ||
                } // else if (colonIndex >= 1)
            } // if (lineString != null)
        } // while (lineString != null)

        if (!oneFileStorage) {
            raFile.close();
        }

        if (spaceUnitsString != null) {
        	int numQuote = 0;
        	for (j = 0; j < spaceUnitsString.length(); j++) {
        		if (spaceUnitsString.substring(j, j+ 1).equals("\"")) {
        			numQuote++;
        		}
        	}
        	int numSpaceUnits = numQuote/2;
            nrrdSpaceUnits = new String[numSpaceUnits];

            for (i = 0, j = 0; i < numSpaceUnits;) {

                if (!spaceUnitsString.substring(j, j + 1).equals("\"")) {
                    j++;
                } else {
                    startQuote[i] = j++;

                    while (!spaceUnitsString.substring(j, j + 1).equals("\"")) {
                        j++;
                    }

                    finishQuote[i++] = j++;
                }
            } // for (i = 0, j = 0; i < numSpaceKinds;)

            for (i = 0; i < numSpaceUnits; i++) {
                nrrdSpaceUnits[i] = spaceUnitsString.substring(startQuote[i] + 1, finishQuote[i]);
                Preferences.debug("NRRD space units[ " + i + "] = " + nrrdSpaceUnits[i] + "\n", Preferences.DEBUG_FILEIO);
            }

            if (nrrdUnits == null) {
                nrrdUnits = new String[nrrdDimensions];
            }

            for (i = 0, j = 0; i < nrrdDimensions; i++) {
                if ((kindsString[i] != null) && ((kindsString[i].equalsIgnoreCase("SPACE")) || (kindsString[i].equalsIgnoreCase("TIME")))) {
                    nrrdUnits[i] = nrrdSpaceUnits[j++];
                }
            }
        } // if (spaceUnitsString != null)

        if ((nrrdDimensions >= 3) &&
                (((nrrdSizes[0] >= 2) && (nrrdSizes[0] <= 4) &&
                      ((kindsString == null) || (kindsString[0] == null) ||
                           (kindsString[0].equalsIgnoreCase("2-VECTOR")) ||
                           (kindsString[0].equalsIgnoreCase("3-COLOR")) ||
                           (kindsString[0].equalsIgnoreCase("RGB-COLOR")) ||
                           (kindsString[0].equalsIgnoreCase("HSV-COLOR")) ||
                           (kindsString[0].equalsIgnoreCase("XYZ-COLOR")) ||
                           (kindsString[0].equalsIgnoreCase("4-COLOR")) ||
                           (kindsString[0].equalsIgnoreCase("RGBA-COLOR")))) ||
                     ((nrrdSizes[2] >= 2) && (nrrdSizes[2] <= 4) &&
                          ((kindsString == null) || (kindsString[2] == null) ||
                               (kindsString[2].equalsIgnoreCase("2-VECTOR")) ||
                               (kindsString[2].equalsIgnoreCase("3-COLOR")) ||
                               (kindsString[2].equalsIgnoreCase("RGB-COLOR")) ||
                               (kindsString[2].equalsIgnoreCase("HSV-COLOR")) ||
                               (kindsString[2].equalsIgnoreCase("XYZ-COLOR")) ||
                               (kindsString[2].equalsIgnoreCase("4-COLOR")) ||
                               (kindsString[2].equalsIgnoreCase("RGBA-COLOR"))))) &&
                ((nrrdDataType == ModelStorageBase.UBYTE) || (nrrdDataType == ModelStorageBase.USHORT) ||
                     (nrrdDataType == ModelStorageBase.FLOAT))) {

            if ((nrrdSizes[0] >= 2) && (nrrdSizes[0] <= 4)) {
                planarConfig = 0; // chunky
                numColors = nrrdSizes[0];

                if ((kindsString != null) && (kindsString[0] != null) &&
                        (kindsString[0].equalsIgnoreCase("RGBA-COLOR"))) {
                    RGBAOrder = true;
                }
            } else {
                planarConfig = 1; // planar
                numColors = nrrdSizes[2];

                if ((kindsString != null) && (kindsString[2] != null) &&
                        (kindsString[2].equalsIgnoreCase("RGBA-COLOR"))) {
                    RGBAOrder = true;
                }
            }

            if (nrrdDataType == ModelStorageBase.UBYTE) {
                mipavDataType = ModelStorageBase.ARGB;
            } else if (nrrdDataType == ModelStorageBase.USHORT) {
                mipavDataType = ModelStorageBase.ARGB_USHORT;
            } else {
                mipavDataType = ModelStorageBase.ARGB_FLOAT;
            }

            mipavDimensions = nrrdDimensions - 1;
            imgExtents = new int[mipavDimensions];
            resols = new float[mipavDimensions];
            mipavUnits = new int[mipavDimensions];

            if (nrrdLabels != null) {
                mipavLabels = new String[mipavDimensions];
            }

            if (planarConfig == 0) { // chunky

                for (i = 0; i < mipavDimensions; i++) {
                    imgExtents[i] = nrrdSizes[i + 1];

                    if (spacings != null) {

                        if (Double.isNaN(spacings[i + 1])) {
                            resols[i] = 1.0f;
                        } else {
                            resols[i] = (float) Math.abs(spacings[i + 1]);
                        }
                    } // if (spacings != null)
                    else if (spaceDirections != null) {
                        if (i < 3) {
                            resols[i] = (float)Math.abs(matrix.get(i,i));
                        }
                        else if (spaceDirections[3] != null) {
                        	resols[i] = (float)spaceDirections[3][3];
                        }
                    }
                    else { // spacings == null
                        resols[i] = 1.0f;
                    } // else spacings == null

                    if (nrrdUnits != null) {

                        if ((nrrdUnits[i + 1] == null) || (nrrdUnits[i + 1].length() == 0) ||
                                (nrrdUnits[i + 1].trim().length() == 0)) {
                            mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MM")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("MILLIMETERS"))) {
                            mipavUnits[i] = Unit.MILLIMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("IN")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("INCHES"))) {
                            mipavUnits[i] = Unit.INCHES.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MIL")) ||
                                (nrrdUnits[i + 1].equalsIgnoreCase("MILS"))) {
                            mipavUnits[i] = Unit.MILS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("CM")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("CENTIMETERS"))) {
                            mipavUnits[i] = Unit.CENTIMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("A")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("ANGSTROMS"))) {
                            mipavUnits[i] = Unit.ANGSTROMS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("NM")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("NANOMETERS"))) {
                            mipavUnits[i] = Unit.NANOMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("UM")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("MICROMETERS"))) {
                            mipavUnits[i] = Unit.MICROMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("M")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("METERS"))) {
                            mipavUnits[i] = Unit.METERS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("KM")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("KILOMETERS"))) {
                            mipavUnits[i] = Unit.KILOMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MI")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("MILES"))) {
                            mipavUnits[i] = Unit.MILES.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("NSEC")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("NANOSECONDS"))) {
                            mipavUnits[i] = Unit.NANOSEC.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("USEC")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("MICROSECONDS"))) {
                            mipavUnits[i] = Unit.MICROSEC.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MSEC")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("MILLISECONDS"))) {
                            mipavUnits[i] = Unit.MILLISEC.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("SEC")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("SECONDS"))) {
                            mipavUnits[i] = Unit.SECONDS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MIN")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("MINUTES"))) {
                            mipavUnits[i] = Unit.MINUTES.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("HR")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("HOURS"))) {
                            mipavUnits[i] = Unit.HOURS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("HZ")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("HERTZ"))) {
                            mipavUnits[i] = Unit.HZ.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("PPM")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("PARTS PER MILLION"))) {
                            mipavUnits[i] = Unit.PPM.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("RADS")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("RADIANS PER SECOND"))) {
                            mipavUnits[i] = Unit.RADS.getLegacyNum();
                        } else {
                            mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                        }
                    } // if (nrrdUnits != null)
                    else { // nrrdUnits == null
                        mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                    } // nrrdUnits == null

                    if (nrrdLabels != null) {
                        mipavLabels[i] = nrrdLabels[i + 1];
                    }
                } // for (i = 0; i < mipavDimensions; i++)
            } // if (planarConfig == 0)
            else { // planarConfig == 1

                for (i = 0; i < 2; i++) {
                    imgExtents[i] = nrrdSizes[i];

                    if (spacings != null) {

                        if (Double.isNaN(spacings[i])) {
                            resols[i] = 1.0f;
                        } else {
                            resols[i] = (float) Math.abs(spacings[i]);
                        }
                    } // if (spacings != null)
                    else if (spaceDirections != null) {
                        if (i < 3) {
                            resols[i] = (float)Math.abs(matrix.get(i,i));
                        }
                        else if (spaceDirections[3] != null) {
                        	resols[i] = (float)spaceDirections[3][3];
                        }
                    }
                    else { // spacings == null
                        resols[i] = 1.0f;
                    } // else spacings == null

                    if (nrrdUnits != null) {

                        if ((nrrdUnits[i] == null) || (nrrdUnits[i].length() == 0) ||
                                (nrrdUnits[i].trim().length() == 0)) {
                            mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("MM")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("MILLIMETERS"))) {
                            mipavUnits[i] = Unit.MILLIMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("IN")) || (nrrdUnits[i].equalsIgnoreCase("INCHES"))) {
                            mipavUnits[i] = Unit.INCHES.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("MIL")) || (nrrdUnits[i].equalsIgnoreCase("MILS"))) {
                            mipavUnits[i] = Unit.MILS.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("CM")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("CENTIMETERS"))) {
                            mipavUnits[i] = Unit.CENTIMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("A")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("ANGSTROMS"))) {
                            mipavUnits[i] = Unit.ANGSTROMS.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("NM")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("NANOMETERS"))) {
                            mipavUnits[i] = Unit.NANOMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("UM")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("MICROMETERS"))) {
                            mipavUnits[i] = Unit.MICROMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("M")) || (nrrdUnits[i].equalsIgnoreCase("METERS"))) {
                            mipavUnits[i] = Unit.METERS.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("KM")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("KILOMETERS"))) {
                            mipavUnits[i] = Unit.KILOMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("MI")) || (nrrdUnits[i].equalsIgnoreCase("MILES"))) {
                            mipavUnits[i] = Unit.MILES.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("NSEC")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("NANOSECONDS"))) {
                            mipavUnits[i] = Unit.NANOSEC.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("USEC")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("MICROSECONDS"))) {
                            mipavUnits[i] = Unit.MICROSEC.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("MSEC")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("MILLISECONDS"))) {
                            mipavUnits[i] = Unit.MILLISEC.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("SEC")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("SECONDS"))) {
                            mipavUnits[i] = Unit.SECONDS.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("MIN")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("MINUTES"))) {
                            mipavUnits[i] = Unit.MINUTES.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("HR")) || (nrrdUnits[i].equalsIgnoreCase("HOURS"))) {
                            mipavUnits[i] = Unit.HOURS.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("HZ")) || (nrrdUnits[i].equalsIgnoreCase("HERTZ"))) {
                            mipavUnits[i] = Unit.HZ.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("PPM")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("PARTS PER MILLION"))) {
                            mipavUnits[i] = Unit.PPM.getLegacyNum();
                        } else if ((nrrdUnits[i].equalsIgnoreCase("RADS")) ||
                                       (nrrdUnits[i].equalsIgnoreCase("RADIANS PER SECOND"))) {
                            mipavUnits[i] = Unit.RADS.getLegacyNum();
                        } else {
                            mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                        }
                    } // if (nrrdUnits != null)
                    else { // nrrdUnits == null
                        mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                    } // else nrrdUnits == null

                    if (nrrdLabels != null) {
                        mipavLabels[i] = nrrdLabels[i];
                    }
                } // for (i = 0; i < 2; i++)

                for (i = 2; i < mipavDimensions; i++) {
                    imgExtents[i] = nrrdSizes[i + 1];

                    if (spacings != null) {

                        if (Double.isNaN(spacings[i + 1])) {
                            resols[i] = 1.0f;
                        } else {
                            resols[i] = (float) Math.abs(spacings[i + 1]);
                        }
                    } // if (spacings != null)
                    else if (spaceDirections != null) {
                        if (i < 3) {
                            resols[i] = (float)Math.abs(matrix.get(i,i));
                        }
                        else if (spaceDirections[3] != null) {
                        	resols[i] = (float)spaceDirections[3][3];
                        }
                    }
                    else { // spacings == null
                        resols[i] = 1.0f;
                    } // else spacings == null

                    if (nrrdUnits != null) {

                        if ((nrrdUnits[i + 1] == null) || (nrrdUnits[i + 1].length() == 0) ||
                                (nrrdUnits[i + 1].trim().length() == 0)) {
                            mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MM")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("MILLIMETERS"))) {
                            mipavUnits[i] = Unit.MILLIMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("IN")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("INCHES"))) {
                            mipavUnits[i] = Unit.INCHES.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MIL")) ||
                                (nrrdUnits[i + 1].equalsIgnoreCase("MILS"))) {
                            mipavUnits[i] = Unit.MILS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("CM")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("CENTIMETERS"))) {
                            mipavUnits[i] = Unit.CENTIMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("A")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("ANGSTROMS"))) {
                            mipavUnits[i] = Unit.ANGSTROMS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("NM")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("NANOMETERS"))) {
                            mipavUnits[i] = Unit.NANOMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("UM")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("MICROMETERS"))) {
                            mipavUnits[i] = Unit.MICROMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("M")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("METERS"))) {
                            mipavUnits[i] = Unit.METERS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("KM")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("KILOMETERS"))) {
                            mipavUnits[i] = Unit.KILOMETERS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MI")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("MILES"))) {
                            mipavUnits[i] = Unit.MILES.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("NSEC")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("NANOSECONDS"))) {
                            mipavUnits[i] = Unit.NANOSEC.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("USEC")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("MICROSECONDS"))) {
                            mipavUnits[i] = Unit.MICROSEC.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MSEC")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("MILLISECONDS"))) {
                            mipavUnits[i] = Unit.MILLISEC.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("SEC")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("SECONDS"))) {
                            mipavUnits[i] = Unit.SECONDS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MIN")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("MINUTES"))) {
                            mipavUnits[i] = Unit.MINUTES.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("HR")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("HOURS"))) {
                            mipavUnits[i] = Unit.HOURS.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("HZ")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("HERTZ"))) {
                            mipavUnits[i] = Unit.HZ.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("PPM")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("PARTS PER MILLION"))) {
                            mipavUnits[i] = Unit.PPM.getLegacyNum();
                        } else if ((nrrdUnits[i + 1].equalsIgnoreCase("RADS")) ||
                                       (nrrdUnits[i + 1].equalsIgnoreCase("RADIANS PER SECOND"))) {
                            mipavUnits[i] = Unit.RADS.getLegacyNum();
                        } else {
                            mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                        }
                    } // if (nrrdUnits != null)
                    else { // nrrdUnits == null
                        mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                    } // nrrdUnits == null

                    if (nrrdLabels != null) {
                        mipavLabels[i] = nrrdLabels[i + 1];
                    }
                } // for (i = 2; i < mipavDimensions; i++)
            } // else planarConfig == 1
        } // if ((nrrdDimensions >= 3) && (((nrrdSizes[0] >= 2) && (nrrdSizes[0] <= 4)) ||
        else if ((nrrdDimensions >= 3) && (nrrdSizes[0] == 2) && (kindsString != null) && (kindsString[0] != null) &&
                     (kindsString[0].equalsIgnoreCase("COMPLEX")) &&
                     ((nrrdDataType == ModelStorageBase.FLOAT) || (nrrdDataType == ModelStorageBase.DOUBLE))) {

            if (nrrdDataType == ModelStorageBase.FLOAT) {
                mipavDataType = ModelStorageBase.COMPLEX;
            } else {
                mipavDataType = ModelStorageBase.DCOMPLEX;
            }

            mipavDimensions = nrrdDimensions - 1;
            imgExtents = new int[mipavDimensions];
            resols = new float[mipavDimensions];
            mipavUnits = new int[mipavDimensions];

            if (nrrdLabels != null) {
                mipavLabels = new String[mipavDimensions];
            }

            for (i = 0; i < mipavDimensions; i++) {
                imgExtents[i] = nrrdSizes[i + 1];

                if (spacings != null) {

                    if (Double.isNaN(spacings[i + 1])) {
                        resols[i] = 1.0f;
                    } else {
                        resols[i] = (float) Math.abs(spacings[i + 1]);
                    }
                } // if (spacings != null)
                else if (spaceDirections != null) {
                    if (i < 3) {
                        resols[i] = (float)Math.abs(matrix.get(i,i));
                    }
                    else if (spaceDirections[3] != null) {
                    	resols[i] = (float)spaceDirections[3][3];
                    }
                }
                else { // spacings == null
                    resols[i] = 1.0f;
                } // else spacings == null

                if (nrrdUnits != null) {

                    if ((nrrdUnits[i + 1] == null) || (nrrdUnits[i + 1].length() == 0) ||
                            (nrrdUnits[i + 1].trim().length() == 0)) {
                        mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MM")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("MILLIMETERS"))) {
                        mipavUnits[i] = Unit.MILLIMETERS.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("IN")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("INCHES"))) {
                        mipavUnits[i] = Unit.INCHES.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MIL")) ||
                            (nrrdUnits[i + 1].equalsIgnoreCase("MILS"))) {
                        mipavUnits[i] = Unit.MILS.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("CM")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("CENTIMETERS"))) {
                        mipavUnits[i] = Unit.CENTIMETERS.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("A")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("ANGSTROMS"))) {
                        mipavUnits[i] = Unit.ANGSTROMS.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("NM")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("NANOMETERS"))) {
                        mipavUnits[i] = Unit.NANOMETERS.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("UM")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("MICROMETERS"))) {
                        mipavUnits[i] = Unit.MICROMETERS.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("M")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("METERS"))) {
                        mipavUnits[i] = Unit.METERS.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("KM")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("KILOMETERS"))) {
                        mipavUnits[i] = Unit.KILOMETERS.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MI")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("MILES"))) {
                        mipavUnits[i] = Unit.MILES.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("NSEC")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("NANOSECONDS"))) {
                        mipavUnits[i] = Unit.NANOSEC.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("USEC")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("MICROSECONDS"))) {
                        mipavUnits[i] = Unit.MICROSEC.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MSEC")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("MILLISECONDS"))) {
                        mipavUnits[i] = Unit.MILLISEC.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("SEC")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("SECONDS"))) {
                        mipavUnits[i] = Unit.SECONDS.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("MIN")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("MINUTES"))) {
                        mipavUnits[i] = Unit.MINUTES.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("HR")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("HOURS"))) {
                        mipavUnits[i] = Unit.HOURS.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("HZ")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("HERTZ"))) {
                        mipavUnits[i] = Unit.HZ.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("PPM")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("PARTS PER MILLION"))) {
                        mipavUnits[i] = Unit.PPM.getLegacyNum();
                    } else if ((nrrdUnits[i + 1].equalsIgnoreCase("RADS")) ||
                                   (nrrdUnits[i + 1].equalsIgnoreCase("RADIANS PER SECOND"))) {
                        mipavUnits[i] = Unit.RADS.getLegacyNum();
                    } else {
                        mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                    }
                } // if (nrrdUnits != null)
                else { // nrrdUnits == null
                    mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                } // nrrdUnits == null

                if (nrrdLabels != null) {
                    mipavLabels[i] = nrrdLabels[i + 1];
                }
            } // for (i = 0; i < mipavDimensions; i++)
        } // else if ((nrrdDimensions >= 3) && (nrrdSizes[0] == 2) && (kindsString != null) &&
        else {
            mipavDataType = nrrdDataType;
            mipavDimensions = nrrdDimensions;
            imgExtents = new int[mipavDimensions];
            resols = new float[mipavDimensions];
            mipavUnits = new int[mipavDimensions];

            if (nrrdLabels != null) {
                mipavLabels = new String[mipavDimensions];
            }

            for (i = 0; i < mipavDimensions; i++) {
                imgExtents[i] = nrrdSizes[i];

                if (spacings != null) {

                    if (Double.isNaN(spacings[i])) {
                        resols[i] = 1.0f;
                    } else {
                        resols[i] = (float) Math.abs(spacings[i]);
                    }
                } // if (spacings != null)
                else if (spaceDirections != null) {
                    if (i < 3) {
                        resols[i] = (float)Math.abs(matrix.get(i,i));
                    }
                    else if (spaceDirections[3] != null) {
                    	resols[3] = (float)spaceDirections[3][3];
                    }
                }
                else { // spacings == null
                    resols[i] = 1.0f;
                } // else spacings == null

                if (nrrdUnits != null) {

                    if ((nrrdUnits[i] == null) || (nrrdUnits[i].length() == 0) || (nrrdUnits[i].trim().length() == 0)) {
                        mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("MM")) ||
                                   (nrrdUnits[i].equalsIgnoreCase("MILLIMETERS"))) {
                        mipavUnits[i] = Unit.MILLIMETERS.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("IN")) || (nrrdUnits[i].equalsIgnoreCase("INCHES"))) {
                        mipavUnits[i] = Unit.INCHES.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("MIL")) || (nrrdUnits[i].equalsIgnoreCase("MILS"))) {
                        mipavUnits[i] = Unit.MILS.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("CM")) ||
                                   (nrrdUnits[i].equalsIgnoreCase("CENTIMETERS"))) {
                        mipavUnits[i] = Unit.CENTIMETERS.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("A")) || (nrrdUnits[i].equalsIgnoreCase("ANGSTROMS"))) {
                        mipavUnits[i] = Unit.ANGSTROMS.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("NM")) || (nrrdUnits[i].equalsIgnoreCase("NANOMETERS"))) {
                        mipavUnits[i] = Unit.NANOMETERS.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("UM")) ||
                                   (nrrdUnits[i].equalsIgnoreCase("MICROMETERS"))) {
                        mipavUnits[i] = Unit.MICROMETERS.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("M")) || (nrrdUnits[i].equalsIgnoreCase("METERS"))) {
                        mipavUnits[i] = Unit.METERS.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("KM")) || (nrrdUnits[i].equalsIgnoreCase("KILOMETERS"))) {
                        mipavUnits[i] = Unit.KILOMETERS.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("MI")) || (nrrdUnits[i].equalsIgnoreCase("MILES"))) {
                        mipavUnits[i] = Unit.MILES.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("NSEC")) ||
                                   (nrrdUnits[i].equalsIgnoreCase("NANOSECONDS"))) {
                        mipavUnits[i] = Unit.NANOSEC.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("USEC")) ||
                                   (nrrdUnits[i].equalsIgnoreCase("MICROSECONDS"))) {
                        mipavUnits[i] = Unit.MICROSEC.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("MSEC")) ||
                                   (nrrdUnits[i].equalsIgnoreCase("MILLISECONDS"))) {
                        mipavUnits[i] = Unit.MILLISEC.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("SEC")) || (nrrdUnits[i].equalsIgnoreCase("SECONDS"))) {
                        mipavUnits[i] = Unit.SECONDS.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("MIN")) || (nrrdUnits[i].equalsIgnoreCase("MINUTES"))) {
                        mipavUnits[i] = Unit.MINUTES.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("HR")) || (nrrdUnits[i].equalsIgnoreCase("HOURS"))) {
                        mipavUnits[i] = Unit.HOURS.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("HZ")) || (nrrdUnits[i].equalsIgnoreCase("HERTZ"))) {
                        mipavUnits[i] = Unit.HZ.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("PPM")) ||
                                   (nrrdUnits[i].equalsIgnoreCase("PARTS PER MILLION"))) {
                        mipavUnits[i] = Unit.PPM.getLegacyNum();
                    } else if ((nrrdUnits[i].equalsIgnoreCase("RADS")) ||
                                   (nrrdUnits[i].equalsIgnoreCase("RADIANS PER SECOND"))) {
                        mipavUnits[i] = Unit.RADS.getLegacyNum();
                    } else {
                        mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                    }
                } // if (nrrdUnits != null)
                else { // nrrdUnits == null
                    mipavUnits[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                } // else nrrdUnits == null

                if (nrrdLabels != null) {
                    mipavLabels[i] = nrrdLabels[i];
                }
            } // for (i = 0; i < mipavDimensions; i++)
        }

        if ((mipavDataType != ModelStorageBase.ARGB) && (mipavDataType != ModelStorageBase.ARGB_USHORT) &&
                (mipavDataType != ModelStorageBase.ARGB_FLOAT) && (mipavDataType != ModelStorageBase.COMPLEX) &&
                (mipavDataType != ModelStorageBase.DCOMPLEX) && (nrrdDimensions >= 3) &&
                (((kindsString == null) && (nrrdSizes[0] < 15)) ||
                     ((kindsString != null) && (kindsString[0] != null) &&
                          (!kindsString[0].equalsIgnoreCase("DOMAIN")) && (!kindsString[0].equalsIgnoreCase("SPACE")) &&
                          (!kindsString[0].equalsIgnoreCase("TIME")) && (kindsString[1] != null) &&
                          ((kindsString[1].equalsIgnoreCase("DOMAIN")) || (kindsString[1].equalsIgnoreCase("SPACE")) ||
                               (kindsString[1].equalsIgnoreCase("TIME"))) && (kindsString[2] != null) &&
                          ((kindsString[2].equalsIgnoreCase("DOMAIN")) || (kindsString[2].equalsIgnoreCase("SPACE")) ||
                               (kindsString[2].equalsIgnoreCase("TIME")))))) {
            reorder = true;
            Preferences.debug("Will reorder data to make first dimension space or time\n", Preferences.DEBUG_FILEIO);
        }

        if (subdim == 0) {
            subdim = mipavDimensions - 1;
        } else if (nrrdDimensions > mipavDimensions) {
            subdim--;
        }

        if ((spacings == null) && (axisMins != null) && (axisMaxs != null)) {

            for (i = 0; i < axisMins.length; i++) {

                if ((!Double.isNaN(axisMins[i])) && (!Double.isNaN(axisMaxs[i]))) {

                    if ((centers == null) || (centers[i] == NONE) || (centers[i] == CELL)) {
                        resols[i] = (float) Math.abs((axisMaxs[i] - axisMins[i]) / nrrdSizes[i]);
                    } else { // centers[i] == NODE
                        resols[i] = (float) Math.abs((axisMaxs[i] - axisMins[i]) / (nrrdSizes[i] - 1));
                    }
                }

            }
        }

        if (mipavDimensions >= 3) {

            if ((origin == null) && (axisMins != null)) {
                origin = new float[3];

                for (i = 0, j = 0; i < axisMins.length; i++) {

                    if (!Double.isNaN(axisMins[i])) {

                        // Make into cell centering
                        if ((axisMaxs != null) && (!Double.isNaN(axisMaxs[i])) && (axisMaxs[i] < axisMins[i])) {
                            origin[j] = (float) (axisMins[i] - (0.5 * resols[i]));
                            matrix.set(j, 3, axisMins[i] - (0.5 * resols[i]));
                        } else {
                            origin[j] = (float) (axisMins[i] + (0.5 * resols[i]));
                            matrix.set(j, 3, axisMins[i] + (0.5 * resols[i]));
                        }

                        j++;
                    }
                }

                fileInfo.setOrigin(origin);
            }

            if (rlInvert) {

                if (origin != null) {
                    origin[0] = -origin[0];
                    fileInfo.setOrigin(origin[0], 0);
                }

                matrix.set(0, 0, -matrix.get(0, 0));
                matrix.set(0, 1, -matrix.get(0, 1));
                matrix.set(0, 2, -matrix.get(0, 2));
                matrix.set(0, 3, -matrix.get(0, 3));
            }

            if (apInvert) {

                if (origin != null) {
                    origin[1] = -origin[1];
                    fileInfo.setOrigin(origin[1], 1);
                }

                matrix.set(1, 0, -matrix.get(1, 0));
                matrix.set(1, 1, -matrix.get(1, 1));
                matrix.set(1, 2, -matrix.get(1, 2));
                matrix.set(1, 3, -matrix.get(1, 3));
            }

            /** If the NRRD image is flipped along the j axis, then the
             *  following code is necessary matrix.set(0, 1, -matrix.get(0,1)); matrix.set(1, 1, -matrix.get(1,1));
             * matrix.set(2, 1, -matrix.get(2,1)); if (origin != null) {     origin[1] = -origin[1];
             * fileInfo.setOrigin(origin[1], 1); }
             */

            axisOrientation = getAxisOrientation(matrix);
            fileInfo.setAxisOrientation(axisOrientation);

            if ((axisOrientation[2] == FileInfoBase.ORI_R2L_TYPE) ||
                    (axisOrientation[2] == FileInfoBase.ORI_L2R_TYPE)) {
                fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
            } else if ((axisOrientation[2] == FileInfoBase.ORI_A2P_TYPE) ||
                           (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE)) {
                fileInfo.setImageOrientation(FileInfoBase.CORONAL);
            } else {
                fileInfo.setImageOrientation(FileInfoBase.AXIAL);
            }
        } // if (mipavDimensions >= 3)

        fileInfo.setDataType(mipavDataType);
        fileInfo.setExtents(imgExtents);
        fileInfo.setResolutions(resols);
        fileInfo.setUnitsOfMeasure(mipavUnits);
        fileInfo.setLabels(mipavLabels);

        if (numThicknesses == 1) {
            fileInfo.setSliceThickness(sliceThickness);
        }

        if (dwmriNex != null) {

            for (i = 0; i < dwmriNex.length; i++) {

                if (dwmriNex[i] != null) {
                    startIndex = Integer.valueOf(dwmriNex[i][0].substring(10)).intValue();
                    finishIndex = startIndex + Integer.valueOf(dwmriNex[i][1]).intValue() - 1;

                    for (j = startIndex + 1; j <= finishIndex; j++) {
                        keyNumberString = String.valueOf(j);

                        while (keyNumberString.length() < 4) {
                            keyNumberString = "0".concat(keyNumberString);
                        }

                        dwmriGradient[j][0] = "DWMRI_gradient_".concat(keyNumberString);
                        dwmriGradient[j][1] = dwmriGradient[startIndex][1];
                    }
                }
            }
        }

        if (dwmriGradient != null) {
            fileInfo.setDwmriGradient(dwmriGradient);
        }

        return true; // If it got this far, it has successfully read in the header
    }

    /**
     * Reads a NRRD image file by reading the header then making a FileRaw to read the image for all filenames in the
     * file list. Only the one file directory (currently) supported.
     *
     * @param      one  flag indicating one image of a 3D dataset should be read in.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @return     The image.
     *
     * @see        FileRaw
     */
    public ModelImage readImage(boolean one) throws IOException, OutOfMemoryError {
        int offset;
        int linesFound;
        int i;
        byte[] buf = new byte[1];
        fileInfo = new FileInfoNRRD(fileName, fileDir, FileUtility.NRRD);

        long dataSize;
        FileInputStream fis;
        int s;
        String sequenceNumber;
        int numSlabs;
        int[] subExtents;
        ModelImage subImage;
        float[] dataBuffer;
        float[] dataBuffer2;
        int pointer = 0;
        int dataNumber;
        int imageSize;
        int sliceSize;
        int[] extents = null;
        int[] newExtents;
        int newSliceSize;
        int volSize;
        int newVolSize;
        int x, y, z, t;
        float[] newResols;
        int[] newUnits;
        String[] newLabels = null;

        if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
            throw (new IOException(" NRRD header file error"));
        }
      
        if (autoSequence || fileNameSequence) {
            try {
                image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName());
            } catch (OutOfMemoryError error) {
                throw (error);
            }

            setFileInfo(fileInfo, image);
            updateorigins(image.getFileInfo());
            image.setMatrix(matrix);

            if (fileNameSequence) {
                sequenceStart = 0;
                sequenceFinish = fileNumber - 1;
                sequenceStep = 1;
            }

            if (subdim == mipavDimensions) {
                numSlabs = 1 + ((sequenceFinish - sequenceStart) / sequenceStep);
                dataSize = 1;

                for (i = 0; i < mipavDimensions; i++) {
                    dataSize *= imgExtents[i];
                }

                dataSize = dataSize / numSlabs;
                subExtents = new int[mipavDimensions];

                for (i = 0; i < (mipavDimensions - 1); i++) {
                    subExtents[i] = imgExtents[i];
                }

                subExtents[mipavDimensions - 1] = imgExtents[mipavDimensions - 1] / numSlabs;
            } else { // subdim < mipavDimensions
                dataSize = 1;

                for (i = 0; i < subdim; i++) {
                    dataSize *= imgExtents[i];
                }

                subExtents = new int[subdim];

                for (i = 0; i < subdim; i++) {
                    subExtents[i] = imgExtents[i];
                }
            } // else subdim < mipavDimensions

            dataNumber = (int) dataSize;
            dataBuffer = new float[dataNumber];
            switch (nrrdDataType) {

                case ModelStorageBase.BYTE:
                case ModelStorageBase.UBYTE:
                    break;

                case ModelStorageBase.SHORT:
                case ModelStorageBase.USHORT:
                    dataSize *= 2;
                    break;

                case ModelStorageBase.INTEGER:
                case ModelStorageBase.UINTEGER:
                case ModelStorageBase.FLOAT:
                    dataSize *= 4;
                    break;

                case ModelStorageBase.LONG:
                case ModelStorageBase.DOUBLE:
                    dataSize *= 8;
                    break;

            } // switch (nrrdDataType)

            if (image.isColorImage()) {

                if (planarConfig == 0) {
                    dataSize *= nrrdSizes[0];
                } else {
                    dataSize *= nrrdSizes[2];
                }
            } else if ((image.getType() == ModelStorageBase.COMPLEX) ||
                           (image.getType() == ModelStorageBase.DCOMPLEX)) {
                dataSize *= 2;
            }

            try {
                subImage = new ModelImage(fileInfo.getDataType(), subExtents, "subImage");
            } catch (OutOfMemoryError error) {
                throw (error);
            }

            fileInfoSub = new FileInfoNRRD(fileName, fileDir, FileUtility.NRRD);
            fileInfoSub.setExtents(subExtents);
            fileInfoSub.setDataType(fileInfo.getDataType());


            if (sequenceStart > sequenceFinish) {
                sequenceStart = -sequenceStart;
                sequenceFinish = -sequenceFinish;
                sequenceStep = -sequenceStep;
            }

            for (i = sequenceStart; i <= sequenceFinish; i += sequenceStep) {
                fireProgressStateChanged((i - sequenceStart) * 100/ (sequenceFinish - sequenceStart));

                if (autoSequence) {

                    // Create fileName with initial sequence number
                    fileName = null;
                    sequenceNumber = String.valueOf(Math.abs(i));

                    while (sequenceNumber.length() < paddingNumber) {
                        sequenceNumber = "0".concat(sequenceNumber);
                    }

                    if (baseBeforeNumber != null) {
                        fileName = baseBeforeNumber;
                    }

                    if (baseNumber) {

                        if (fileName == null) {
                            fileName = sequenceNumber;
                        } else {
                            fileName = fileName.concat(sequenceNumber);
                        }
                    } // if (baseNumber)

                    if (baseAfterNumber != null) {
                        fileName = fileName.concat(baseAfterNumber);
                    }

                    fileName = fileName.concat(".");

                    if (baseNumber) {
                        fileName = fileName.concat(ext);
                    } else {
                        fileName = fileName.concat(sequenceNumber);
                    }
                } // if (autoSequence)
                else {
                    fileName = fileNameSet[i];
                }

                fileInfoSub.setFileName(fileName);
                Preferences.debug("Sequencing on " + fileDir + fileName + "\n", Preferences.DEBUG_FILEIO);


                // Do line skipping before byte skipping
                // Do line skipping before decompression
                file = new File(fileDir + fileName);
                raFile = new RandomAccessFile(file, "r");
                fileLength = raFile.length();

                if (skippedLines > 0) {
                    linesFound = 0;

                    for (i = 0; linesFound < skippedLines; i++) {
                        raFile.read(buf);

                        if (buf[0] == 10 /* new line */) {
                            linesFound++;
                        }
                    }
                } // if(skippedLines > 0)

                offset1 = raFile.getFilePointer();
                raFile.close();

                if (gunzip) {
                    int totalBytesRead = 0;

                    fireProgressStateChanged("Uncompressing GZIP file ...");
                    fis = new FileInputStream(file);
                    fis.skip(offset1);

                    GZIPInputStream gzin = new GZIPInputStream(new BufferedInputStream(fis));
                    s = fileName.lastIndexOf(".");

                    String uncompressedName = fileDir + fileName.substring(0, s);
                    FileOutputStream out = new FileOutputStream(uncompressedName);
                    byte[] buffer = new byte[256];

                    while (true) {
                        int bytesRead = gzin.read(buffer);

                        if (bytesRead == -1) {
                            break;
                        }

                        totalBytesRead += bytesRead;
                        out.write(buffer, 0, bytesRead);
                    }

                    out.close();
                    file = new File(uncompressedName);
                    fileInfoSub.setFileName(fileName.substring(0, s));
                    offset1 = 0;
                    raFile = new RandomAccessFile(file, "r");
                    fileLength = raFile.length();
                    raFile.close();
                } // if (gunzip)

                setFileInfo(fileInfoSub, subImage);

                // Do byte skipping after decompression
                if (skippedBytes >= 0) {
                    offset = (int) (offset1 + skippedBytes);
                } else { // skippedBytes < 0
                    offset = (int) (fileLength - dataSize);
                } // else skippedBytes < 0

                try { // Construct a FileRaw to actually read the image.

                    FileRaw rawFile;
                    rawFile = new FileRaw(fileInfoSub.getFileName(), fileInfoSub.getFileDirectory(), fileInfoSub,
                                          FileBase.READ);

                    if (image.isColorImage()) {
                        rawFile.setPlanarConfig(planarConfig);
                        rawFile.setNumColors(numColors);
                        rawFile.setRGBAOrder(RGBAOrder);
                    }

                    rawFile.readImage(subImage, offset);

                    rawFile.close();

                    subImage.exportData(0, dataNumber, dataBuffer);

                    image.importData(pointer * dataNumber, dataBuffer, false);
                    pointer++;
                } catch (IOException error) {
                    throw new IOException("FileNRRD: " + error);
                } catch (OutOfMemoryError e) {
                    throw (e);
                }
            } // for (i = sequenceStart; i <= sequenceFinish; i += sequenceStep)

            image.calcMinMax();
        } // if (autoSequence || fileNameSequence)
        else { // !autoSequence && ! fileNameSequence

            if (!oneFileStorage) {
                file = new File(fileDir + fileName);
                raFile = new RandomAccessFile(file, "r");
                fileLength = raFile.length();
            }

            // Do line skipping before byte skipping
            // Do line skipping before decompression
            if (skippedLines > 0) {
                linesFound = 0;

                for (i = 0; linesFound < skippedLines; i++) {
                    raFile.read(buf);

                    if (buf[0] == 10 /* new line */) {
                        linesFound++;
                    }
                }
            } // if(skippedLines > 0)

            offset1 = raFile.getFilePointer();
            raFile.close();

            if (gunzip) {
                int totalBytesRead = 0;

                fireProgressStateChanged("Uncompressing GZIP file ...");
                fis = new FileInputStream(file);
                fis.skip(offset1);

                GZIPInputStream gzin = new GZIPInputStream(new BufferedInputStream(fis));
                s = fileName.lastIndexOf(".");

                String uncompressedName = fileDir + fileName.substring(0, s);
                FileOutputStream out = new FileOutputStream(uncompressedName);
                byte[] buffer = new byte[256];

                while (true) {
                    int bytesRead = gzin.read(buffer);

                    if (bytesRead == -1) {
                        break;
                    }

                    totalBytesRead += bytesRead;
                    out.write(buffer, 0, bytesRead);
                }

                out.close();
                file = new File(uncompressedName);
                fileInfo.setFileName(fileName.substring(0, s));
                offset1 = 0;
                raFile = new RandomAccessFile(file, "r");
                fileLength = raFile.length();
                raFile.close();
            } // if (gunzip)

            // Do byte skipping after decompression
            if (skippedBytes >= 0) {
                offset = (int) (offset1 + skippedBytes);
            } else { // skippedBytes < 0
                dataSize = imgExtents[0] * imgExtents[1];

                if (mipavDimensions >= 3) {
                    dataSize *= imgExtents[2];
                }

                if (mipavDimensions >= 4) {
                    dataSize *= imgExtents[3];
                }

                switch (nrrdDataType) {

                    case ModelStorageBase.BYTE:
                    case ModelStorageBase.UBYTE:
                        break;

                    case ModelStorageBase.SHORT:
                    case ModelStorageBase.USHORT:
                        dataSize *= 2;
                        break;

                    case ModelStorageBase.INTEGER:
                    case ModelStorageBase.UINTEGER:
                    case ModelStorageBase.FLOAT:
                        dataSize *= 4;
                        break;

                    case ModelStorageBase.LONG:
                    case ModelStorageBase.DOUBLE:
                        dataSize *= 8;
                        break;

                } // switch (nrrdDataType)

                if (image.isColorImage()) {

                    if (planarConfig == 0) {
                        dataSize *= nrrdSizes[0];
                    } else {
                        dataSize *= nrrdSizes[2];
                    }
                } else if ((image.getType() == ModelStorageBase.COMPLEX) ||
                               (image.getType() == ModelStorageBase.DCOMPLEX)) {
                    dataSize *= 2;
                }

                offset = (int) (fileLength - dataSize);
            } // else skippedBytes < 0

            try {

                if (one) {
                    extents = new int[fileInfo.getExtents().length];

                    for (i = 0; i < extents.length; i++) {
                        extents[i] = fileInfo.getExtents()[i];
                    }

                    image = new ModelImage(fileInfo.getDataType(), new int[] { extents[0], extents[1] },
                                           fileInfo.getFileName());
                } else {
                    image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName());
                }
            } catch (OutOfMemoryError error) {
                throw (error);
            }

            setFileInfo(fileInfo, image);
            updateorigins(image.getFileInfo());
            image.setMatrix(matrix);

            try { // Construct a FileRaw to actually read the image.

                FileRaw rawFile;
                rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

                if (image.isColorImage()) {
                    rawFile.setPlanarConfig(planarConfig);
                    rawFile.setNumColors(numColors);
                    rawFile.setRGBAOrder(RGBAOrder);
                }


                if (one) {

                    if (fileInfo.getExtents().length > 2) {
                        offset = offset + getOffset(fileInfo);
                    }
                }
          
                linkProgress(rawFile);
                rawFile.readImage(image, offset);
                rawFile.close();

                if (one) {
                    fileInfo.setExtents(extents);
                }
            } catch (IOException error) {
                throw new IOException("FileNRRD: " + error);
            } catch (OutOfMemoryError e) {
                throw (e);
            }

        } // else !autoSequence && ! fileNameSequence

        if (reorder) {
            extents = image.getExtents();
            imageSize = extents[0];

            for (i = 1; i < image.getNDims(); i++) {
                imageSize *= extents[i];
            }

            dataBuffer = new float[imageSize];

            try {
                image.exportData(0, imageSize, dataBuffer);
            } catch (IOException error) {
                throw new IOException("FileNRRD: " + error);
            }

            dataBuffer2 = new float[imageSize];
            sliceSize = extents[0] * extents[1];
            newSliceSize = extents[1] * extents[2];

            if (image.getNDims() == 3) {
                newExtents = new int[3];
                newExtents[0] = extents[1];
                newExtents[1] = extents[2];
                newExtents[2] = extents[0];
                newResols = new float[3];
                newResols[0] = resols[1];
                newResols[1] = resols[2];
                newResols[2] = resols[0];
                newUnits = new int[3];
                newUnits[0] = mipavUnits[1];
                newUnits[1] = mipavUnits[2];
                newUnits[2] = mipavUnits[0];

                if (mipavLabels != null) {
                    newLabels = new String[3];
                    newLabels[0] = mipavLabels[1];
                    newLabels[1] = mipavLabels[2];
                    newLabels[2] = mipavLabels[0];
                }

                for (i = 0; i < imageSize; i++) {
                    z = i / sliceSize;
                    y = (i - (z * sliceSize)) / extents[0];
                    x = i - (z * sliceSize) - (y * extents[0]);
                    dataBuffer2[y + (z * newExtents[0]) + (x * newSliceSize)] = dataBuffer[i];
                }
            } else { // image.getNDims() == 4
                newExtents = new int[4];
                newExtents[0] = extents[1];
                newExtents[1] = extents[2];
                newExtents[2] = extents[3];
                newExtents[3] = extents[0];
                newResols = new float[4];
                newResols[0] = resols[1];
                newResols[1] = resols[2];
                newResols[2] = resols[3];
                newResols[3] = resols[0];
                newUnits = new int[4];
                newUnits[0] = mipavUnits[1];
                newUnits[1] = mipavUnits[2];
                newUnits[2] = mipavUnits[3];
                newUnits[3] = mipavUnits[0];

                if (mipavLabels != null) {
                    newLabels = new String[4];
                    newLabels[0] = mipavLabels[1];
                    newLabels[1] = mipavLabels[2];
                    newLabels[2] = mipavLabels[3];
                    newLabels[3] = mipavLabels[0];
                }

                volSize = sliceSize * extents[2];
                newVolSize = newSliceSize * newExtents[2];

                for (i = 0; i < imageSize; i++) {
                    t = i / volSize;
                    z = (i - (t * volSize)) / sliceSize;
                    y = (i - (t * volSize) - (z * sliceSize)) / extents[0];
                    x = i - (t * volSize) - (z * sliceSize) - (y * extents[0]);
                    dataBuffer2[y + (z * newExtents[0]) + (t * newSliceSize) + (x * newVolSize)] = dataBuffer[i];
                }
            } // else image.getNDims() == 4

            image.changeExtents(newExtents);
            fileInfo.setExtents(newExtents);
            fileInfo.setResolutions(newResols);
            fileInfo.setUnitsOfMeasure(newUnits);

            if (mipavLabels != null) {
                fileInfo.setLabels(newLabels);
            }

            if (origin != null) {
                fileInfo.setOrigin(origin);
            }

            if (axisOrientation != null) {
                fileInfo.setAxisOrientation(axisOrientation);

                if ((axisOrientation[2] == FileInfoBase.ORI_R2L_TYPE) ||
                        (axisOrientation[2] == FileInfoBase.ORI_L2R_TYPE)) {
                    fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
                } else if ((axisOrientation[2] == FileInfoBase.ORI_A2P_TYPE) ||
                               (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE)) {
                    fileInfo.setImageOrientation(FileInfoBase.CORONAL);
                } else {
                    fileInfo.setImageOrientation(FileInfoBase.AXIAL);
                }
            }

            setFileInfo(fileInfo, image);
            updateorigins(image.getFileInfo());

            try {
                image.importData(0, dataBuffer2, true);
            } catch (IOException error) {
                throw new IOException("FileNRRD: " + error);
            }
        } // if (reorder)

        return image;
    }

    /**
     * Return the 3 axis orientation codes that correspond to the closest standard anatomical orientation of the (i,j,k)
     * axes.
     *
     * @param   mat  4x4 matrix that transforms (i,j,k) indexes to x,y,z coordinates where +x =Left, +y = Posterior, +z
     *               = Superior Only the upper-left 3x3 corner of the matrix is used This routine finds the permutation
     *               of (x,y,z) which has the smallest angle to the (i,j,k) axes directions, which are columns of the
     *               input matrix Errors: The codes returned will be zero.
     *
     * @return  codes
     */
    private int[] getAxisOrientation(TransMatrix mat) {
    	return FileMGH.getAxisOrientation(mat);
    }

    /**
     * Helper method to calculate the offset for getting only the middle NRRD image slice from the 3D file.
     *
     * @param   fileInfo  File info.
     *
     * @return  offset
     */
    private int getOffset(FileInfoNRRD fileInfo) {
        int offset = fileInfo.getExtents()[0] * fileInfo.getExtents()[1] * (fileInfo.getExtents()[2] / 2);

        switch (nrrdDataType) {

            case ModelStorageBase.BYTE:
            case ModelStorageBase.UBYTE:
                break;

            case ModelStorageBase.SHORT:
            case ModelStorageBase.USHORT:
                offset *= 2;
                break;

            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
            case ModelStorageBase.FLOAT:
                offset *= 4;
                break;

            case ModelStorageBase.LONG:
            case ModelStorageBase.DOUBLE:
                offset *= 8;
                break;

        }

        return offset;
    }

    /**
     * Reads lines of the file and strips comments indicated by the # symbol until a nonnull String results or the end
     * of the file is reached or a blank line containing end of header with zero characters is reached.
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private String readLine() throws IOException {
        String tempString = null;
        int index;

        while ((tempString == null) && (raFile.getFilePointer() < (fileLength - 1)) && (!foundEOF) &&
                   (!foundEOHeader)) {

            try {
                tempString = raFile.readLine();
            } catch (EOFException error) {
                tempString = null;
                foundEOF = true;
            } catch (IOException error) {
                throw (error);
            }

            if (tempString == null) {
                foundEOHeader = true;
            } else if (tempString.length() == 0) {
                foundEOHeader = true;
            }

            if (tempString != null) {
                index = tempString.indexOf("#");

                if (index != -1) {
                    tempString = tempString.substring(0, index);
                }

                tempString = tempString.trim();
            }

            if (tempString != null) {

                if (tempString.length() == 0) {
                    tempString = null;
                }
            }
        } // while

        return tempString;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fileInfo  -- a NRRD file Info that has already been read
     * @param  image     -- a ModelImage that the fileInfo needs to be attached to
     */
    private void setFileInfo(FileInfoNRRD fileInfo, ModelImage image) {

        int[] extents = fileInfo.getExtents();

        if (image.getNDims() == 2) {

            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image

            for (int i = 0; i < extents[2]; i++) {
                FileInfoNRRD newFileInfo = (FileInfoNRRD) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image

            for (int i = 0; i < (extents[2] * extents[3]); i++) {
                FileInfoNRRD newFileInfo = (FileInfoNRRD) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        }

    } // setFileInfo

    /**
     * Updates the start locations. Each image has a fileinfo where the start locations are stored. Note that the start
     * location for the Z (3rd) dimension change with the change is the slice. The origin is in the upper left corner
     * and we are using the right hand rule. + x -> left to right; + y -> top to bottom and + z -> into screen.
     *
     * @param  fileInfo  DOCUMENT ME!
     */
    private void updateorigins(FileInfoBase[] fileInfo) {
        int axisOrient;

        float[] origin = (float[]) (fileInfo[0].getOrigin().clone());
        float[] resolutions = fileInfo[0].getResolutions();

        if (image.getNDims() == 3) {

            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setOrigin(origin);
                axisOrient = fileInfo[i].getAxisOrientation(2);

                if ((axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE) ||
                        (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                    origin[2] += resolutions[2];
                } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                    origin[2] -= resolutions[2];
                }
            }
        } else if (image.getNDims() == 4) {
            float tmp = origin[2];

            for (int i = 0; i < image.getExtents()[3]; i++) {

                for (int j = 0; j < image.getExtents()[2]; j++) {
                    fileInfo[(i * image.getExtents()[2]) + j].setOrigin(origin);
                    axisOrient = fileInfo[i].getAxisOrientation(2);

                    if ((axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE) ||
                            (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                        origin[2] += resolutions[2];
                    } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                        origin[2] -= resolutions[2];
                    }
                }

                origin[3] += resolutions[3];
                origin[2] = tmp;
            }
        }
        /*else if (image.getNDims() == 5) {
         * fileInfo = image.getFileInfo(); for (int i = 0;    i <    image.getExtents()[2] * image.getExtents()[3] *
         * image.getExtents()[4];    i++) { fileInfo[i].setorigins(startLocs); startLocs[4] += resolutions[4]; }  }*/
    }
    
    
    
    /**
     * write NRRD header
     * @return
     */
    private boolean writeHeader(ModelImage image, String fhName, String fDir, FileWriteOptions options) throws IOException {
    	String fileHeaderName = "";
    	String lineString = "";
    	FileInfoBase fInfo;
    	int dimension;
    	int[] extents;
    	float[] res = null;
    	String spaceString = "";
    	String spaceDirectionsString = "";
    	String spaceOriginString = "";
    	String spacingsString = ""; 
    	fInfo = image.getFileInfo()[0];
    	int type;
    	String typeString = "";
    	boolean endianess;
    	String endianessString = "";
    	String kindsString = "";
    	String encodingString = "raw";
    	String sizesString = "";
    	String thicknessString = "";
    	int[] spaceUnitsOfMeas;
    	String spaceUnitsOfMeasString = "";
    	boolean isTime = false;
    	boolean orientationsKnown = true;
    	
    	//header filename
    	if (oneFile) {
            fileHeaderName = fhName + ".nrrd";
        } else {
            fileHeaderName = fhName + ".nhdr";
        }

    	//type
    	type = fInfo.getDataType();
    	if(type == ModelStorageBase.BYTE) {
    		typeString = "signed char";
    	} else if(type == ModelStorageBase.UBYTE) {
    		typeString = "uchar";
    	} else if(type == ModelStorageBase.SHORT) {
    		typeString = "short";
    	} else if(type == ModelStorageBase.USHORT) {
    		typeString = "ushort";
    	} else if(type == ModelStorageBase.INTEGER) {
    		typeString = "int";
    	} else if(type == ModelStorageBase.UINTEGER) {
    		typeString = "uint";
    	} else if(type == ModelStorageBase.LONG) {
    		typeString = "longlong";
    	} else if(type == ModelStorageBase.FLOAT) {
    		typeString = "float";
    	} else if(type == ModelStorageBase.DOUBLE) {
    		typeString = "double";
    	} else if(type == ModelStorageBase.COMPLEX) {
    		typeString = "float";
    	} else if(type == ModelStorageBase.DCOMPLEX) {
    		typeString = "double";
    	} else if(type == ModelStorageBase.ARGB) {
    		typeString = "uchar";
    	} else if(type == ModelStorageBase.ARGB_USHORT) {
    		typeString = "ushort";
    	} else if(type == ModelStorageBase.ARGB_FLOAT) {
    		typeString = "float";
    	}
    	
    	
        res = image.getFileInfo(0).getResolutions();
        //orientation info
    	if(image.getAxisOrientation()[0] != FileInfoBase.ORI_UNKNOWN_TYPE) {
    		spaceString = "left-posterior-superior";
    		if(image.getNDims() == 4) {
    			if(image.getUnitsOfMeasure().length >= 4) {
	    			if(image.getUnitsOfMeasure(3) == Unit.HOURS.getLegacyNum() || 
	    					image.getUnitsOfMeasure(3) == Unit.HZ.getLegacyNum() ||
	    					image.getUnitsOfMeasure(3) == Unit.MICROSEC.getLegacyNum() ||
	    					image.getUnitsOfMeasure(3) == Unit.MILLISEC.getLegacyNum() ||
	    					image.getUnitsOfMeasure(3) == Unit.MINUTES.getLegacyNum() ||
	    					image.getUnitsOfMeasure(3) == Unit.NANOSEC.getLegacyNum()||
	    					image.getUnitsOfMeasure(3) == Unit.SECONDS.getLegacyNum()) {
	    				spaceString = spaceString + "-time";
	    				isTime = true;	
	    			}
    			}
    		}
    		
            StringBuffer sb = new StringBuffer();

            
            int[] axisOrients = image.getFileInfo()[0].getAxisOrientation();
            
            for(int i=0;i<axisOrients.length;i++) {
            	if(axisOrients[i] == FileInfoBase.ORI_UNKNOWN_TYPE ) {
            		orientationsKnown = false;
            		break;
            	}
            }
                   
            if(orientationsKnown && image.getNDims() >= 3) {
            	float value = 0;
            	if(axisOrients[0] == FileInfoBase.ORI_R2L_TYPE) {
 				   value = Math.abs(res[0]);
 			   	}else if(axisOrients[0] == FileInfoBase.ORI_L2R_TYPE) {
 				   value = Math.abs(res[0]);
 				   if(value != 0) {
 					   value = -value;
 				   }
 			   	}
            	if (image.getNDims() == 3) {
            	    sb.append("(" + value + ",0,0) ");
            	}
            	else {
            		sb.append("(" + value + ",0,0,0) ");	
            	}
        		if(axisOrients[1] == FileInfoBase.ORI_A2P_TYPE) {
 				   value = Math.abs(res[1]);
 			    }else if (axisOrients[1] == FileInfoBase.ORI_P2A_TYPE) {
 				   value = Math.abs(res[1]);
 				  if(value != 0) {
					   value = -value;
				   }
 			    }
        		if (image.getNDims() == 3) {
        		    sb.append("(0," + value + ",0) ");
        		}
        		else {
        			sb.append("(0," + value + ",0,0) ");	
        		}

        		if(axisOrients[2] == FileInfoBase.ORI_I2S_TYPE) {
        			value = Math.abs(res[2]);
        		}else if(axisOrients[2] == FileInfoBase.ORI_S2I_TYPE) {
        			value = Math.abs(res[2]);
        			if(value != 0) {
  					   value = -value;
  				   }
        		}
        		if (image.getNDims() == 3) {
        		    sb.append("(0,0," + value + ") ");
        		}
        		else {
        			sb.append("(0,0," + value + ",0) ");	
        		}
        	
        		if(image.getNDims() == 4) {
        			if(!options.isMultiFile()) {
        				value = Math.abs(res[3]);
        				sb.append("(0,0,0," + value + ") ");
        			}
        		}

                spaceDirectionsString = sb.toString();
            }
                   
            float[] origin = image.getFileInfo(0).getOrigin();

            if(axisOrients[0] == FileInfoBase.ORI_R2L_TYPE) {
            	origin[0] = -Math.abs(origin[0]);  
			}else if(axisOrients[0] == FileInfoBase.ORI_L2R_TYPE) {
				origin[0] = Math.abs(origin[0]);
			}
            if(axisOrients[1] == FileInfoBase.ORI_I2S_TYPE) {
            	origin[1] = -Math.abs(origin[1]);  
			}else if(axisOrients[1] == FileInfoBase.ORI_S2I_TYPE) {
				origin[1] = Math.abs(origin[1]);
			}
            if(image.getNDims() > 2) {
            	if(axisOrients[2] == FileInfoBase.ORI_A2P_TYPE) {
            		origin[2] = -Math.abs(origin[2]);
 			   	}else if (axisOrients[2] == FileInfoBase.ORI_P2A_TYPE) {
 			   		origin[2] = Math.abs(origin[2]);
 			   	}
            }

            sb = new StringBuffer();
            sb.append("(");
            for (int i = 0; (i < image.getNDims()); i++) {
            	if(i == 0) {
            		sb.append(new Float(origin[i]).toString());
            	}else {
            		sb.append("," + new Float(origin[i]).toString());
            	}
            }
            sb.append(")");
    		spaceOriginString = sb.toString();
    	} // if(image.getAxisOrientation()[0] != FileInfoBase.ORI_UNKNOWN_TYPE)
    	
    	//per axis info
    	dimension = image.getNDims();
    	extents = image.getFileInfo(0).getExtents();
    	
    	if(dimension == 2) {
    		kindsString = "space space";
    		sizesString = extents[0] + " " + extents[1];
    		spacingsString = res[0] + " " + res[1];
    	} else if(dimension == 3) {
    		if(!options.isMultiFile()) {
    			thicknessString = "NaN NaN " + String.valueOf(image.getFileInfo(0).getSliceThickness());
    			kindsString = "space space space";
    			sizesString = extents[0] + " " + extents[1] + " " + extents[2];
    			spacingsString = res[0] + " " + res[1] + " " + res[2];
    		}else {
    			kindsString = "space space";
    			sizesString = extents[0] + " " + extents[1];
    			spacingsString = res[0] + " " + res[1];
    		}
    	} else if(dimension == 4) {
    		if(!options.isMultiFile()) {
    			thicknessString = "NaN NaN " + String.valueOf(image.getFileInfo(0).getSliceThickness()) + " NaN";
    			if(isTime) {
    				kindsString = "space space space time";
    			}else {
    				kindsString = "space space space domain";
    			}
    			sizesString = extents[0] + " " + extents[1] + " " + extents[2] + " " + extents[3];
    			spacingsString = res[0] + " " + res[1] + " " + res[2] + " " + res[3];
    		}else {
    			thicknessString = "NaN NaN " + String.valueOf(image.getFileInfo(0).getSliceThickness());
    			kindsString = "space space space";
    			sizesString = extents[0] + " " + extents[1] + " " + extents[2];
    			spacingsString = res[0] + " " + res[1] + " " + res[2];
    		}
    	}
    	
    	//space units of measure
    	spaceUnitsOfMeas = image.getFileInfo(0).getUnitsOfMeasure();
    	StringBuffer sb = new StringBuffer();
    	String s = "";
    	if(spaceUnitsOfMeas[0] != Unit.UNKNOWN_MEASURE.getLegacyNum()) {
	    	for(int i=0;(i<spaceUnitsOfMeas.length) && (i < image.getNDims());i++) {
	    		if(spaceUnitsOfMeas[i] == Unit.MILLIMETERS.getLegacyNum()) {
	    			s = "\"mm\"";
	    		} else if (spaceUnitsOfMeas[i] == Unit.INCHES.getLegacyNum()) {
	    			s = "\"in\"";
                }else if (spaceUnitsOfMeas[i] == Unit.MILS.getLegacyNum()) {
                    s = "\"mil\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.CENTIMETERS.getLegacyNum()) {
	    			s = "\"cm\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.ANGSTROMS.getLegacyNum()) {
	    			s = "\"a\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.NANOMETERS.getLegacyNum()) {
	    			s = "\"nm\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.MICROMETERS.getLegacyNum()) {
	    			s = "\"um\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.METERS.getLegacyNum()) {
	    			s = "\"m\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.KILOMETERS.getLegacyNum()) {
	    			s = "\"km\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.MILES.getLegacyNum()) {
	    			s = "\"mi\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.SECONDS.getLegacyNum()) {
	    			s = "\"sec\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.NANOSEC.getLegacyNum()) {
	    			s = "\"nsec\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.MICROSEC.getLegacyNum()) {
	    			s = "\"usec\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.MILLISEC.getLegacyNum()) {
	    			s = "\"msec\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.MINUTES.getLegacyNum()) {
	    			s = "\"min\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.HOURS.getLegacyNum()) {
	    			s = "\"hr\"";
	    		}else if (spaceUnitsOfMeas[i] == Unit.HZ.getLegacyNum()) {
	    			s = "\"hz\"";
	    		}
	    		sb.append(s);
	    		sb.append(" ");
	    	}
	    	spaceUnitsOfMeasString = sb.toString();
    	}
    	
    	//color image
    	if(image.isColorImage()) {
    		dimension = dimension + 1;
    		kindsString = "3-color " + kindsString;
    		sizesString = "3 " + sizesString;
    		spacingsString = "NaN " + spacingsString;
    		if(!thicknessString.equals("")) {
    			thicknessString = "NaN " + thicknessString;
    		}
    		if(!spaceDirectionsString.equals("")) {
    			spaceDirectionsString = "none " + spaceDirectionsString;
    		}
    	}
    	
    	//multifile option
    	if(options.isMultiFile()) {
    		dimension = dimension - 1;
    	}

    	//endianess
    	endianess = fInfo.getEndianess();
    	if(endianess == FileBase.BIG_ENDIAN) {
    		endianessString = "big";
    	} else {
    		endianessString = "little";
    	}

    	//write out header
    	file = new File(fileDir + fileHeaderName);
        raFile = new RandomAccessFile(file, "rw");
        raFile.setLength(0);

        lineString = "NRRD000" + writeVersionNumber + "\n";
        raFile.writeBytes(lineString);
        
        lineString = "type: " + typeString + "\n";
        raFile.writeBytes(lineString);
        
        lineString = "dimension: " + dimension + "\n";
        raFile.writeBytes(lineString);
        
        lineString = "sizes: " + sizesString + "\n";
        raFile.writeBytes(lineString);
        
        lineString = "endian: " + endianessString + "\n";
        raFile.writeBytes(lineString);
        
        lineString = "encoding: " + encodingString + "\n";
        raFile.writeBytes(lineString);
        
        if(!orientationsKnown || image.getNDims() > 3) {
        	lineString = "spacings: " + spacingsString + "\n";
        	raFile.writeBytes(lineString);
        }
        
        lineString = "kinds: " + kindsString + "\n";
        raFile.writeBytes(lineString);

        if(!spaceString.equals("")) {
        	lineString = "space: " + spaceString + "\n";
            raFile.writeBytes(lineString);
        }
        
        if(!spaceOriginString.equals("")) {
        	lineString = "space origin: " + spaceOriginString + "\n";
            raFile.writeBytes(lineString);
        }
        
        if(orientationsKnown && image.getNDims() >= 3) {
	        if(!spaceDirectionsString.equals("")) {
	        	lineString = "space directions: " + spaceDirectionsString + "\n";
	            raFile.writeBytes(lineString);
	        }
        }
        
        if(!spaceUnitsOfMeasString.equals("")) {
        	lineString = "space units: " +spaceUnitsOfMeasString + "\n";
        	raFile.writeBytes(lineString);
        }
        
        if(!thicknessString.equals("")) {
        	lineString = "thicknesses: " + thicknessString + "\n";
        	raFile.writeBytes(lineString);
        }
        
        if(!oneFile) {
        	lineString = "data file: " + fhName + ".raw" + "\n";
            raFile.writeBytes(lineString);
        }

        lineString = "\n";
        raFile.writeBytes(lineString);

    	return true;
	}
    
    
    /**
     * Writes a NRRD format type image.
     *
     * @param image  Image model of data to write.
     * @param options FileWriteOptions
     *
     * @exception  IOException  if there is an error writing the file
     *
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
    	String fhName;
        int index;
    	String suffix;
        suffix = FileUtility.getExtension(fileName);
    	
        if (suffix.equalsIgnoreCase(".nrrd")) {
            oneFile = true;
        } else {
        	oneFile = false;
        }
        
        index = fileName.lastIndexOf(".");

        if (index != -1) {
            fhName = fileName.substring(0, index);
        } else {
            fhName = fileName.substring(0);
        }
        
        //create new extents to match up with specified beginning/end slices/times
        int[] newExtents = null;
        int num = image.getExtents().length;
        if (num == 4) {
            newExtents = new int[4];
            newExtents[3] = (options.getEndTime() - options.getBeginTime() + 1);
            newExtents[2] = (options.getEndSlice() - options.getBeginSlice() + 1);
            newExtents[1] = image.getExtents()[1];
            newExtents[0] = image.getExtents()[0];
        } else if (num == 3) {
            newExtents = new int[3];
            newExtents[2] = (options.getEndSlice() - options.getBeginSlice() + 1);
            newExtents[1] = image.getExtents()[1];
            newExtents[0] = image.getExtents()[0];
        } else {
            newExtents = new int[2];
            newExtents[1] = image.getExtents()[1];
            newExtents[0] = image.getExtents()[0];
        }
        // sets the extents to the fileinfo (which will be replaced after)
        image.getFileInfo()[0].setExtents(newExtents);

        if (options.isMultiFile()) {
        	if (oneFile) {
        		FileRaw rawFile;
                rawFile = new FileRaw(image.getFileInfo(0));
                linkProgress(rawFile);
        		if (image.getNDims() == 3) {
        			writeHeader3DTo2D(image, fhName, fileDir, options);
        			long startLocation = raFile.getFilePointer();
	                rawFile.setZeroLengthFlag(false);
	                rawFile.setStartPosition(startLocation);
                    rawFile.writeImage3DTo2D(image, options, ".nrrd");
                    
                } else if (image.getNDims() == 4) {
                	writeHeader4DTo3D(image, fhName, fileDir, options);
                	long startLocation = raFile.getFilePointer();
	                rawFile.setZeroLengthFlag(false);
	                rawFile.setStartPosition(startLocation);
                    rawFile.writeImage4DTo3D(image, options, ".nrrd");
                }
        	}else {
        		FileRaw rawFile;
                rawFile = new FileRaw(fhName + ".raw", fileDir, image.getFileInfo(0), FileBase.READ_WRITE);
                linkProgress(rawFile);
        		if (image.getNDims() == 3) {
        			writeHeader3DTo2D(image, fhName, fileDir, options);
                    rawFile.writeImage3DTo2D(image, options, ".raw");
                    
                } else if (image.getNDims() == 4) {
                	writeHeader4DTo3D(image, fhName, fileDir, options);
                    rawFile.writeImage4DTo3D(image, options, ".raw");
                }
        	}
        } else {
        	writeHeader(image,fhName,fileDir,options);
        	if (oneFile) {
        		FileRaw rawFile;
                rawFile = new FileRaw(fileName, fileDir, image.getFileInfo(0), FileBase.READ_WRITE);
                linkProgress(rawFile);
                long startLocation = raFile.getFilePointer();
                rawFile.setZeroLengthFlag(false);
                rawFile.setStartPosition(startLocation);
                rawFile.writeImage(image, options);
                rawFile.close();
            	rawFile.finalize();
        	}else {
                FileRaw rawFile;
                rawFile = new FileRaw(fhName + ".raw", fileDir, image.getFileInfo(0), FileBase.READ_WRITE);
                linkProgress(rawFile);
                rawFile.writeImage(image, options);
                rawFile.close();
            	rawFile.finalize();
        	}
        }
        if(raFile != null) {
        	raFile.close();
        }
    }
    
    
    
    /**
     * Method to save off the header from a 3D image into 2D header files sequentially named (similar to the method in
     * FileRaw).
     *
     * @param   img         Image to be saved
     * @param   headerName  Name of file
     * @param   headerDir   Directory for file
     * @param   options     File write options (contains # of digits and start #)
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeHeader3DTo2D(ModelImage img, String headerName, String headerDir, FileWriteOptions options)
            throws IOException {
        int k, seq;
        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();
        String origName = new String(headerName);

        for (k = beginSlice, seq = options.getStartNumber(); k <= endSlice; k++, seq++) {
            headerName = origName;

            if (options.getDigitNumber() == 1) {
                headerName += Integer.toString(seq);
            } else if (options.getDigitNumber() == 2) {

                if (seq < 10) {
                    headerName += "0" + Integer.toString(seq);
                } else {
                    headerName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 3) {

                if (seq < 10) {
                    headerName += "00" + Integer.toString(seq);
                } else if (seq < 100) {
                    headerName += "0" + Integer.toString(seq);
                } else {
                    headerName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 4) {

                if (seq < 10) {
                    headerName += "000" + Integer.toString(seq);
                } else if (seq < 100) {
                    headerName += "00" + Integer.toString(seq);
                } else if (seq < 1000) {
                    headerName += "0" + Integer.toString(seq);
                } else {
                    headerName += Integer.toString(seq);
                }
            }

            writeHeader(img, headerName, headerDir, options);

        } // end for loop
    }
    
    
    
    /**
     * This method is used when saving a 4D image in an array of 3D files. The file name has numbers appended to
     * correctly order the images.
     *
     * @param   image     the image dataset to be saved
     * @param   fileName  the file name
     * @param   fileDir   the file directory
     * @param   options   file options indicate how to save the image
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeHeader4DTo3D(ModelImage image, String fileName, String fileDir, FileWriteOptions options)
            throws IOException {
        int k, seq;
        int beginTime = options.getBeginTime();
        int endTime = options.getEndTime();
        String origName = new String(fileName);

        for (k = beginTime, seq = options.getStartNumber(); k <= endTime; k++, seq++) {
            fileName = origName;

            if (options.getDigitNumber() == 1) {
                fileName += Integer.toString(seq);
            } else if (options.getDigitNumber() == 2) {

                if (seq < 10) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 3) {

                if (seq < 10) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 4) {

                if (seq < 10) {
                    fileName += "000" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 1000) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            }
            // write header with image, # of images per, and 1 time slice

            writeHeader(image, fileName, fileDir, options);

        } // end for loop

    }


}
