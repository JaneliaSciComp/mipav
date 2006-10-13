package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.dicomcomm.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.actions.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.xcede.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import com.sun.jimi.core.*;

import java.awt.*;
import java.awt.image.*;

import java.io.*;

import java.util.*;

import javax.imageio.*;

import javax.swing.*;


/**
 * This class controls the file input/output of most formats that MIPAV supports, including tiff, raw, analyze, DICOM,
 * and Medvision. It switches based on file type and calls the file constructors, readers, and writers for the specific
 * file type. However, note the FileAvi and FileQT are called directly from ViewJFrameBase for file writes.
 *
 * @version  0.1 Sept 5, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @author   Neva Cherniavsky
 * @see      FileAnalyze
 * @see      FileDicom
 * @see      FileMedVision
 * @see      FileRaw
 * @see      FileTiff
 */
public class FileIO {

    //~ Instance fields ------------------------------------------------------------------------------------------------

	private static final String FILE_READ = "Opening ";
	private static final String FILE_WRITE = "Saving ";
	
    /** Directory where the image file can be found. */
    private String fileDir;

    /** File name of the image. */
    private String fileName;

    /** If a LUT is to be saved with the image it is stored here. */
    private ModelLUT LUT = null;

    /** DOCUMENT ME! */
    private ModelRGB modelRGB = null;

   
    private ViewJProgressBar progressBar = null;
    
    /** Refers to whether or not the FileIO reports progress or errors. */
    private boolean quiet = false;

    /** Address of second image in file. */
    private int secondImage = 0;

    /** Reference to the user interface. */
    private ViewUserInterface UI;

    /**
     * Dialog to prompt the user to determine the correct file type. If the type of file to read is unknown (ie., the
     * suffix doesn't match one of the known types, build and display the unknown file dialog so the user can try to
     * identify the image type so the correct reader can be used
     */
    private JDialogUnknownIO unknownIODialog;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates the FileIO and displays <q>Choose File Type</q> unknown file dialog. Constructs a new FileIO object, sets
     * the user interface, and initializes the unknown file dialog.
     */
    public FileIO() {
        UI = ViewUserInterface.getReference();
        unknownIODialog = new JDialogUnknownIO(UI.getMainFrame(), "Choose File Type");
        UI.setLoad(false); // default to "opening "
    }

    /**
     * Creates the FileIO and displays <q>Choose File Type</q> unknown file dialog. Constructs a new FileIO object, sets
     * the user interface, sets the LUT, and initializes the unknown file dialog.
     *
     * @param  _LUT  Passes LUT into file IO object so that LUT can be store with image (i.e. TIFF).
     */
    public FileIO(ModelLUT _LUT) {
        UI = ViewUserInterface.getReference();
        LUT = _LUT;
        unknownIODialog = new JDialogUnknownIO(UI.getMainFrame(), "Choose File Type");
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param   modelImage  ModelImage - the image to resample to UBYTE
     *
     * @return  ModelImage
     */
    public static ModelImage convertToARGB(ModelImage modelImage) {
        float min = (float) modelImage.getMin();
        float max = (float) modelImage.getMax();

        float[] oneSliceBuffer = new float[modelImage.getExtents()[0] * modelImage.getExtents()[1] * 4];

        ModelImage modelImageResultARGB = new ModelImage(ModelStorageBase.ARGB, modelImage.getExtents(),
                                                         modelImage.getImageName());

        try {

            for (int i = 0; i < modelImage.getExtents()[2]; i++) // loop through images
            {
                modelImage.exportData(i * oneSliceBuffer.length, oneSliceBuffer.length, oneSliceBuffer); // export a 2d buffer from modelImageResult

                oneSliceBuffer = resample255(oneSliceBuffer, min, max);

                modelImageResultARGB.importData(i * oneSliceBuffer.length, oneSliceBuffer, false); // import into
                                                                                                   // modelImageResultUB

                copyResolutions(modelImage, modelImageResultARGB, i);
            }

            modelImageResultARGB.calcMinMax();
        } catch (Exception e) {
            Preferences.debug(e.getMessage());
        } finally {
            modelImage.disposeLocal(false);
        }

        return modelImageResultARGB;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   modelImageResult  ModelImage - the image to resample to UBYTE
     *
     * @return  ModelImage
     */
    public static ModelImage convertToUBYTE(ModelImage modelImageResult) {
        float min = (float) modelImageResult.getMin();
        float max = (float) modelImageResult.getMax();

        float[] oneSliceBuffer = new float[modelImageResult.getExtents()[0] * modelImageResult.getExtents()[1]];

        ModelImage modelImageResultUB = new ModelImage(ModelStorageBase.UBYTE, modelImageResult.getExtents(),
                                                       modelImageResult.getImageName());

        try {

            for (int i = 0; i < modelImageResult.getExtents()[2]; i++) // loop through images
            {
                modelImageResult.exportData(i * oneSliceBuffer.length, oneSliceBuffer.length, oneSliceBuffer); // export a 2d buffer from modelImageResult

                oneSliceBuffer = resample255(oneSliceBuffer, min, max);

                modelImageResultUB.importData(i * oneSliceBuffer.length, oneSliceBuffer, false); // import into
                                                                                                 // modelImageResultUB

                copyResolutions(modelImageResult, modelImageResultUB, i);
            }

            modelImageResultUB.calcMinMax();
        } catch (Exception e) {
            Preferences.debug(e.getMessage());
        } finally {
            modelImageResult.disposeLocal(false);
        }

        return modelImageResultUB;
    }

    /**
     * Only used for COR volume files with hyphen in name Breaks the filename into basename and suffix, then returns the
     * suffix.
     *
     * @param   fn  The filename.
     *
     * @return  The suffix or file-extension. For example, <q>-info</q>. Note that suffix includes the separator '-'
     */
    public static String getCORSuffixFrom(String fn) {
        int s;
        String sfx = "";

        if (fn != null) {
            s = fn.lastIndexOf("-"); // look for last period

            if (s != -1) {
                sfx = fn.substring(s);
            }
        }

        return sfx.toLowerCase();
    }

    /**
     * Trims off the file extension and file name, but leaves the file index. An index might be 0001, or 140, for
     * example.
     *
     * @param   fName  String file name to get index
     *
     * @return  String (index string)
     */
    public static int getFileIndex(String fName) {
        int i;

        // char ch;
        int length = fName.lastIndexOf("."); // Start before suffix.

        for (i = length - 1; i > -1; i--) {

            // removed the explicit digit check in favor of the
            // Java.lang version. -- parsonsd, 20 July 2004
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
     * Breaks the filename into basename and suffix, then returns the suffix.
     *
     * @param   fn  The filename.
     *
     * @return  The suffix or file-extension. For example, <q>.jpg</q>. Note that suffix includes the separator '.'
     */
    public static String getSuffixFrom(String fn) {
        int s;
        String sfx = "";

        if (fn != null) {
            s = fn.lastIndexOf("."); // look for last period

            if (s != -1) {
                sfx = fn.substring(s);
            }
        }

        return sfx.toLowerCase();
    }

    /**
     * The purpose of this method is to subsample a ModelImage to the dimensions specified by the subsampleDimension
     * parameter.
     *
     * @param   modelImage          ModelImage - the image to be subsampled. This image will be destroyed in the course
     *                              of the algorithm
     * @param   subsampleDimension  Dimension - the dimensions to subsample to.
     *
     * @return  ModelImage - the subsampled image
     */
    public static ModelImage subsample(ModelImage modelImage, Dimension subsampleDimension) {
        int[] subsampledExtents = new int[] { subsampleDimension.getSize().width, subsampleDimension.getSize().height };

        ModelImage modelImageResult = new ModelImage(modelImage.getType(),
                                                     new int[] {
                                                         subsampleDimension.getSize().width,
                                                         subsampleDimension.getSize().height
                                                     }, modelImage.getImageName() + "_subsampled");

        AlgorithmSubsample algorithmSubsample = new AlgorithmSubsample(modelImage, modelImageResult, subsampledExtents,
                                                                       new float[] { 1.0f, 1.0f, 1.0f }, false, false,
                                                                       null);

        algorithmSubsample.setProgressBarVisible(false);
        algorithmSubsample.run();

        modelImage.disposeLocal(false);

        return modelImageResult;
    }

    /**
     * Trims the numbers and special character from the file name. Numerics and some special characters <code>[ - _
     * .</code> are removed from the end of the file.
     *
     * @param   fName  File name where the last characters are alpha-numerics indicating the image number.
     *
     * @return  File name without numbers on the end.
     */
    public static String trim(String fName) {
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

    /**
     * Trims the numbers or file extension from COR file names. Any numbers or <q>.info</q> or <q>.info~</q> will be
     * removed from after a hyphen in the given fname.
     *
     * @param   fName  File name where the last characters are alpha-numerics indicating the image number or .info or
     *                 .info~
     *
     * @return  File name without numbers on the end.
     */
    public static String trimCOR(String fName) {

        int length = fName.lastIndexOf("-"); //

        if (length >= 0) {
            return (new String(fName.substring(0, length + 1)));
        } else {
            return null;
        }

    }

    /**
     * Sets specific types to be multifile based on the input argument. Not all file types are supported to handle
     * multifiles. Those that do support building a 3D image out of a series of 2D images are defined to have that input
     * capability here.
     *
     * @param   fileType   Input file type. File types which MIPAV does not allow multifile images get the file type
     *                     returned as sent.
     * @param   multiFile  If <code>true</code>, returns a new filetype corresponding to the input filetype + MULTIFILE.
     *                     If <code>false</code>, returns the given filetype.
     *
     * @return  The new or old fileType.
     */
    public int chkMultiFile(int fileType, boolean multiFile) {
        int fType = fileType;

        if (multiFile) {

            if (fileType == FileBase.TIFF) {
                fType = FileBase.TIFF_MULTIFILE;
            } else if (fileType == FileBase.LSM) {
                fType = FileBase.LSM_MULTIFILE;
            } else if (fileType == FileBase.DICOM) {
                fType = FileBase.DICOM;
            } // affords some posibilities
            else if (fileType == FileBase.ANALYZE) {
                fType = FileBase.ANALYZE_MULTIFILE;
            } // under construction
            else if (fileType == FileBase.NIFTI) {
                fType = FileBase.NIFTI_MULTIFILE;
            } else if (fileType == FileBase.RAW) {
                fType = FileBase.RAW_MULTIFILE;
            } // under construction
            else if (fileType == FileBase.COR) {
                fType = FileBase.COR;
            } else if (fileType == FileBase.XML) {
                fType = FileBase.XML_MULTIFILE;
            }
        }

        return fType;
    }

    /**
     * Find the the file name in the file dir.
     *
     * @param   fileDir   File directory name
     * @param   fileName  File name
     *
     * @return  boolean indicates file name found or not
     */
    public boolean findFile(String fileDir, String fileName) {
        File imageDir;
        String[] fileListBuffer;

        imageDir = new File(fileDir);
        fileListBuffer = imageDir.list();

        for (int i = 0; i < fileListBuffer.length; i++) {

            // if found, return true.
            if (fileName.equals(fileListBuffer[i])) {
                return true;
            }
        }

        return false;
    }

    /**
     * Only for FreeSurfer COR volume files Looks in the image directory and returns all images with the same root up to
     * the hyphen, sorted in lexicographical order. Will set the number of images (<code>nImages</code>) for the calling
     * program.
     *
     * @param   fileDir   Directory to look for images.
     * @param   fileName  File name of the image.
     *
     * @return  An array of the image names to be read in or saved as.
     *
     * @throws  OutOfMemoryError  DOCUMENT ME!
     */
    public String[] getCORFileList(String fileDir, String fileName) throws OutOfMemoryError {
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

        String subName = trimCOR(fileName); // subName = name without indexing numbers at end

        for (i = 0; i < fileListBuffer.length; i++) {
            fileName2 = fileListBuffer[i].trim();
            suffix2 = getCORSuffixFrom(fileName2);
            okNumber = true;

            for (k = 1; k < suffix2.length(); k++) {

                if (!Character.isDigit(suffix2.charAt(k))) {

                    // modified to use Java.lang version 20 July 2004/parsonsd
                    // if ( suffix2.charAt( k ) < '0' || suffix2.charAt( k ) > '9' ) {
                    okNumber = false;
                }
            } // for (k = 0; k < suffix2.length(); k++)

            if (okNumber) {

                if (trimCOR(fileName2).equals(subName)) {
                    fileList[j] = fileListBuffer[i];
                    j++;
                }
            } // if (okNumber)
        } // for (i = 0; i < fileListBuffer.length; i++)

        // Number of images is index of last image read into fileList
        nImages = j;

        if (nImages == 0) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: No images with that suffix");
            }

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
     * Looks in the image directory and returns all images with the same suffix as <code>fileName</code>, sorted in
     * lexicographical order.
     *
     * @param   fileDir   Directory to look for images.
     * @param   fileName  File name of the image.
     *
     * @return  An array of the image names to be read in or saved as.
     *
     * @throws  OutOfMemoryError  DOCUMENT ME!
     */
    public String[] getFileList(String fileDir, String fileName) throws OutOfMemoryError {
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

        String subName = trim(fileName); // subName = name without indexing numbers at end
        String suffix = getSuffixFrom(fileName); // suffix  = ie. .ima or .img ...

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
                    suffix2 = getSuffixFrom(fileName2);

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

                        if (trim(fileName2).equals(subName)) {
                            fileList[j] = fileListBuffer[i];
                            j++;
                        }
                    } // if (okNumber)
                } // if (!files[i].isDirectory())
            } // for (i = 0; i <fileListBuffer.length; i++)
        } // if (numberSuffix)
        else { // numberSuffix == false. I.e. ".img".

            // check to see that they end in suffix.  If so, store, count.

            for (i = 0; i < fileListBuffer.length; i++) {

                if (!files[i].isDirectory()) {

                    if (fileListBuffer[i].trim().toLowerCase().endsWith(suffix)) { // note: not case sensitive!

                        if (trim(fileListBuffer[i].trim()).equals(subName)) {
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
                MipavUtil.displayError("FileIO: No images with that suffix");
            }

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
     * Presents a dialog for a user-entered definition of the image type.
     *
     * @return  The image file type entered by the user, or FileBase.ERROR if FileIO is quiet or the dialog is
     *          cancelled. see FileBase.ERROR see isQuiet()
     *
     * @see     JDialogUnknownIO
     */
    public int getFileType() {
        int fileType;

        if (quiet) {
            return FileBase.ERROR;
        }

        unknownIODialog.setVisible(true);

        if (unknownIODialog.isCancelled()) {
            fileType = FileBase.ERROR;
        } else {
            fileType = unknownIODialog.getImageType();
        }

        return fileType;
    }

    /**
     * Sets the FileBase.(filetype) based on the file extension of the given filename. Also sets file "suffix", if
     * required.
     *
     * @param   fileName  Filename of the image to read in. Must include the file extension.
     * @param   fileDir   Directory where fileName exists.
     *
     * @return  Filetype from FileBase.
     *
     * @see     FileBase
     */
    public int getFileType(String fileName, String fileDir) {
        return getFileType(fileName, fileDir, false);
    }

    /**
     * Sets the FileBase.(filetype) based on the file extension of the given filename. Also sets file "suffix", if
     * required.
     *
     * @param   fileName  Filename of the image to read in. Must include the file extension.
     * @param   fileDir   Directory where fileName exists.
     * @param   doWrite   If true about to write a file
     *
     * @return  Filetype from FileBase.
     *
     * @see     FileBase
     */
    public int getFileType(String fileName, String fileDir, boolean doWrite) {
        int fileType;
        int i;

        fileName.trim();

        String suffix = getSuffixFrom(fileName);

        if (suffix.equalsIgnoreCase(".tif")) {
            fileType = FileBase.TIFF;
        } else if (suffix.equalsIgnoreCase(".tiff")) {
            fileType = FileBase.TIFF;
        } else if (suffix.equalsIgnoreCase(".lsm")) {
            fileType = FileBase.LSM;
        } else if (suffix.equalsIgnoreCase(".stk")) {
            fileType = FileBase.STK;
        } else if (suffix.equalsIgnoreCase(".jpeg")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".jpg")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".bmp")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".gif")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".pict")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".pic")) {
            fileType = FileBase.UNDEFINED;

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
                    fileType = FileBase.BIORAD;
                } else {
                    fileType = FileBase.JIMI;
                }
            } catch (OutOfMemoryError error) {
                System.gc();
            } catch (FileNotFoundException e) {
                System.gc();
            } catch (IOException e) {
                System.gc();
            }
        } // else if (suffix.equalsIgnoreCase(".pic"))
        else if (suffix.equalsIgnoreCase(".png")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".psd")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".dib")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".tga")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".xbm")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".xpm")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".xml")) {
            fileType = FileBase.XML;
        } else if (suffix.equalsIgnoreCase(".pcx")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".ico")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".cur")) {
            fileType = FileBase.JIMI;
        } else if (suffix.equalsIgnoreCase(".mgh")) {
            fileType = FileBase.MGH;
        } else if (suffix.equalsIgnoreCase(".mgz")) {
            fileType = FileBase.MGH;
        } else if (suffix.equalsIgnoreCase(".raw")) {
            fileType = FileBase.RAW;
        } else if (suffix.equalsIgnoreCase(".img")) {

            // Both ANALYZE and NIFTI use .img and .hdr
            if (doWrite) {
                JDialogAnalyzeNIFTIChoice choice = new JDialogAnalyzeNIFTIChoice(UI.getMainFrame());

                if (!choice.okayPressed()) {
                    fileType = FileBase.ERROR;
                } else if (choice.isAnalyzeFile()) {
                    fileType = FileBase.ANALYZE;
                } else {
                    fileType = FileBase.NIFTI;
                }
            } // if (doWrite)
            else { // read
                fileType = FileBase.ANALYZE;

                int p = fileName.lastIndexOf(".");
                String fileHeaderName = fileName.substring(0, p + 1) + "hdr";

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
                        fileType = FileBase.NIFTI;
                    }
                } catch (OutOfMemoryError error) {
                    System.gc();
                } catch (FileNotFoundException e) {
                    System.gc();
                } catch (IOException e) {
                    System.gc();
                }
            } // else read
        } else if (suffix.equalsIgnoreCase(".nii")) {
            fileType = FileBase.NIFTI;
        } else if (suffix.equalsIgnoreCase(".nhdr")) {
            fileType = FileBase.NRRD;
        } else if (suffix.equalsIgnoreCase(".nrrd")) {
            fileType = FileBase.NRRD;
        } else if (suffix.equalsIgnoreCase(".ima")) {
            fileType = FileBase.DICOM;

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
                    fileType = FileBase.MAGNETOM_VISION;
                }
            } catch (OutOfMemoryError error) {
                System.gc();
            } catch (FileNotFoundException e) {
                System.gc();
            } catch (IOException e) {
                System.gc();
            }
        } else if (suffix.equalsIgnoreCase(".dcm")) {
            fileType = FileBase.DICOM;
        } else if (suffix.equalsIgnoreCase(".bin")) {
            fileType = FileBase.MEDVISION;
        } else if (suffix.equalsIgnoreCase(".map")) {
            fileType = FileBase.MAP;
        } // Benes Trus special
        else if (suffix.equalsIgnoreCase(".mnc")) {
            fileType = FileBase.MINC;
        } else if (suffix.equalsIgnoreCase(".avi")) {
            fileType = FileBase.AVI;
        } else if (suffix.equalsIgnoreCase(".imc")) {
            fileType = FileBase.CHESHIRE;
        } else if (suffix.equalsIgnoreCase(".oly")) {
            fileType = FileBase.CHESHIRE_OVERLAY;
        } // QuickTime on WIndows uses .QT and on MAC uses .mov
        else if (suffix.equalsIgnoreCase(".qt")) {
            fileType = FileBase.QT;
        } else if (suffix.equalsIgnoreCase(".mov")) {
            fileType = FileBase.QT;
        } else if (suffix.equalsIgnoreCase(".head")) {
            fileType = FileBase.AFNI;
        } else if (suffix.equalsIgnoreCase(".brik")) {
            fileType = FileBase.AFNI;
        } else if (suffix.equalsIgnoreCase(".ics")) {
            fileType = FileBase.ICS;
        } else if (suffix.equalsIgnoreCase(".ids")) {
            fileType = FileBase.ICS;
        } else if (suffix.equalsIgnoreCase(".hdr")) {
            fileType = FileBase.INTERFILE;
        } else if (suffix.equalsIgnoreCase(".spm")) {
            fileType = FileBase.SPM;
        } else if (suffix.equalsIgnoreCase(".fits")) {
            fileType = FileBase.FITS;
        } else if (suffix.equalsIgnoreCase(".dm3")) {
            fileType = FileBase.DM3;
        } else if (suffix.equalsIgnoreCase(".tmg")) {
            fileType = FileBase.TMG;
        } else if (suffix.equalsIgnoreCase(".mrc")) {
            fileType = FileBase.MRC;
        } else if (suffix.equalsIgnoreCase(".wu")) {
            fileType = FileBase.OSM;
        } else if (suffix.equalsIgnoreCase(".sig")) {
            fileType = FileBase.GE_GENESIS;
        } else if (suffix.equalsIgnoreCase(".gedno")) {
            fileType = FileBase.GE_SIGNA4X;
        } else if (suffix.equalsIgnoreCase(".log")) {
            fileType = FileBase.MICRO_CAT;
        } else if (suffix.equalsIgnoreCase(".ct")) {
            fileType = FileBase.MICRO_CAT;
        } else if (suffix.equalsIgnoreCase(".info")) {
            fileType = FileBase.COR;
        } else if (suffix.equalsIgnoreCase(".info~")) {
            fileType = FileBase.COR;
        } else { // cannot automatically determine the filetype from the filename extension
            fileType = FileBase.UNDEFINED;
        }

        return fileType;
    }

    /**
     * Returns LUT associated with the image file.
     *
     * @return  The LUT associated with the image although it may be null.
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }


    /**
     * Gets the model RGB.
     *
     * @return  ModelRGB
     */
    public ModelRGB getModelRGB() {
        return modelRGB;
    }

    /**
     * Returns address of TIFF header of second image if present in CZ-Private Tag of LSM 510 file Returns zero if not
     * present.
     *
     * @return  secondImage
     */
    public int getSecondImage() {
        return secondImage;
    }

    /**
     * Gets the file extension based on the filetype of the image.
     *
     * @param   fileType  Type of file, found in FileBase.
     *
     * @return  The appropriate file extension.
     *
     * @see     FileBase
     */
    public static final String getSuffix(int fileType) {
        String suffix = null;

        switch (fileType) {

            case FileBase.JIMI:
                suffix = ".jpg";
                break;

            case FileBase.RAW:
                suffix = ".raw";
                break;

            case FileBase.DICOM:
                suffix = ".dcm";
                break;

            case FileBase.MEDVISION:
                suffix = ".bin";
                break;

            case FileBase.MAP:
                suffix = ".map";
                break;

            case FileBase.MINC:
                suffix = ".mnc";
                break;

            case FileBase.AVI:
                suffix = ".avi";
                break;

            case FileBase.QT:
                suffix = ".mov";
                break;

            case FileBase.CHESHIRE:
                suffix = ".imc";
                break;

            case FileBase.CHESHIRE_OVERLAY:
                suffix = ".oly";
                break;

            case FileBase.VOI_FILE:
                suffix = ".voi";
                break;

            case FileBase.ANALYZE:
                suffix = ".img";
                break;

            case FileBase.MGH:
                // Uses .mgh for uncompressed storage
                // Uses .mgz or .mgh.gz for compressed storage
                suffix = ".mgh";
                break;

            case FileBase.NIFTI:

                // uses .hdr and .img for 2 file storage
                // uses .nii for 1 file storage
                suffix = ".nii";
                break;
            case FileBase.NRRD:
                // uses .nhdr for header and any nhdr designated extension for data
                // in 2 file storage
                // uses .nrrd for 1 file storage
                suffix = ".nrrd";
                break;
            case FileBase.SPM:
                suffix = ".spm";
                break;

            case FileBase.TIFF:
                suffix = ".tiff";
                break;

            case FileBase.LSM:
                suffix = ".lsm";
                break;

            case FileBase.STK:
                suffix = ".stk";
                break;

            case FileBase.AFNI:
                suffix = ".afni";
                break;

            case FileBase.ICS:
                suffix = ".ics";
                break;

            case FileBase.INTERFILE:
                suffix = ".hdr";
                break;

            case FileBase.BIORAD:
                suffix = ".pic";
                break;

            case FileBase.FITS:
                suffix = ".fits";
                break;

            case FileBase.DM3:
                suffix = ".dm3";
                break;

            case FileBase.TMG:
                suffix = ".tmg";
                break;

            case FileBase.MRC:
                suffix = ".mrc";
                break;

            case FileBase.OSM:
                suffix = ".wu";
                break;

            case FileBase.MAGNETOM_VISION:
                suffix = ".ima";
                break;

            case FileBase.GE_GENESIS:
                suffix = ".sig";
                break;

            case FileBase.GE_SIGNA4X:
                suffix = ".gedno";
                break;

            case FileBase.MICRO_CAT:
                suffix = ".log";
                break;

            case FileBase.XML:
                suffix = ".xml";
                break;

            case FileBase.COR:
                suffix = ".info";
                break;
        }

        return suffix;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  <code>FileBase.DICOM</code> if the file is a DICOM file, and <code>FileBase.UNDEFINED</code> otherwise
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public int isDicom(String fileName, String fileDir) throws IOException {

        try {
            FileDicom imageFile = new FileDicom(fileName, fileDir);

            if (imageFile != null) {
                boolean isDicom = imageFile.isDICOM();

                imageFile.close();
                imageFile = null;

                if (isDicom) {
                    return FileBase.DICOM;
                }
            }

            return FileBase.UNDEFINED;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n");
            } else {
                Preferences.debug("FileIO: " + error + "\n");
            }

            return FileBase.UNDEFINED;
        }

    }
    
    
    /**
     * DOCUMENT ME!
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  <code>FileBase.gedno</code> if the file is a GE MR Signa 4.x file, and <code>FileBase.UNDEFINED</code> otherwise
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public int isGESigna4X(String fileName, String fileDir) throws IOException {

        try {
            FileGESigna4X imageFile = new FileGESigna4X(fileName, fileDir);

            if (imageFile != null) {
                boolean isGESigna4X = imageFile.isGESigna4X();

                
                if (isGESigna4X) {
                    return FileBase.GE_SIGNA4X;
                }
            }

            return FileBase.UNDEFINED;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n");
            } else {
                Preferences.debug("FileIO: " + error + "\n");
            }

            return FileBase.UNDEFINED;
        }

    }
    
    
    /**
     * Refers to whether or not the FileIO will send alerts to the user about progress or errors.
     *
     * @return  DOCUMENT ME!
     */
    public boolean isQuiet() {
        return quiet;
    }

    /**
     * Reads a list of DICOM files. DICOM images have all their slices in separate files (except multi-frame), with
     * different information in each header. The position of the slice in the image is determined by information found
     * in the header so all the headers must be read before the images can be read. That's why this method goes through
     * all the images twice.
     *
     * @param   selectedFileName  Name of the image file selected to be readin <code>this#fileDir</code> to read. Used
     *                            to ID study and series number
     * @param   fileList          List of all the files to be read in.
     * @param   performSort       <code>true</code> if this method is to sort the files in the list, or, <code>
     *                            false</code> will apply the images in each file in the order they appear as the order
     *                            to use in the ModelImage.
     *
     * @return  The image that was read in, or null if failure.
     */
    public ModelImage readDicom(String selectedFileName, String[] fileList, boolean performSort) {

        ModelImage image = null;
        FileDicom imageFile;
        FileInfoBase myFileInfo;
        float[] bufferFloat;
        short[] bufferShort;
        int[] extents;
        int length = 0;
        int i;
        int nImages = 0;
        int nListImages;
        float[] tPt = new float[3];
        TransMatrix matrix = null;
        String studyID = new String();
        String seriesNo = new String();
        String acqNo = new String();
        String seriesNoRef = new String();
        String studyIDMaster;
        String seriesNoMaster;
        String acqNoMaster;

        int orientation = FileInfoBase.UNKNOWN_ORIENT;

        if (fileList.length == 0) {
            return null;
        }
//System.err.println("quiet: " + quiet);
        /*System.out.println("selectedFileName = " + selectedFileName);
         * for (int m = 0; m < fileList.length; m++) { System.out.println("Filelist = " + m + "  " + fileList[m]);}*/

        try {
            nListImages = fileList.length;
            imageFile = new FileDicom(selectedFileName, fileDir);
            imageFile.setQuiet(quiet); // if we want quiet, we tell the reader, too.
            imageFile.readHeader(true); // can we read the header?
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n");
            } else {
                Preferences.debug("FileIO: " + error + "\n");
            }

            return null;
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n");
            } else {
                Preferences.debug("FileIO: " + error + "\n");
            }

            return null;
        }

        myFileInfo = imageFile.getFileInfo();

        try {

            // image2d  = new ModelImage(myFileInfo.getDataType(), myFileInfo.getExtents(), UI);
            if (ModelImage.isColorImage(myFileInfo.getDataType())) { // / other type of ARGB
                length = myFileInfo.getExtents()[0] * myFileInfo.getExtents()[1] * 4;
            } else {
                length = myFileInfo.getExtents()[0] * myFileInfo.getExtents()[1];
            }

            bufferFloat = new float[length];
            bufferShort = new short[length];
        } catch (OutOfMemoryError error) {
            bufferFloat = null;
            bufferShort = null;
            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n");
            } else {
                Preferences.debug("FileIO: " + error + "\n");
            }

            return null;
        }

        // look for number of frames tag (0028,0008) != null && > 1 nImages = number of frames
        if (((FileInfoDicom) (myFileInfo)).getValue("0028,0008") != null) {
            nImages = Integer.valueOf(((String) (((FileInfoDicom) (myFileInfo)).getValue("0028,0008"))).trim()).intValue();

            if (nImages > 1) {
                ((FileInfoDicom) (myFileInfo)).multiFrame = true;
            }

            Preferences.debug("28,0008 (nImages) == " + nImages + "\n");
        }

        try {

            if (((FileInfoDicom) (myFileInfo)).getValue("0020,0010") != null) {
                studyIDMaster = (String) (((FileInfoDicom) myFileInfo).getValue("0020,0010"));
                studyIDMaster.trim();
            } else {
                studyIDMaster = "";
            }

            if (((FileInfoDicom) (myFileInfo)).getValue("0020,0012") != null) {
                acqNoMaster = (String) (((FileInfoDicom) myFileInfo).getValue("0020,0012"));
                acqNoMaster.trim();
            } else {
                acqNoMaster = "";
            }

            if (((FileInfoDicom) (myFileInfo)).getValue("0020,0011") != null) {
                seriesNoMaster = (String) (((FileInfoDicom) myFileInfo).getValue("0020,0011"));
                seriesNoRef = (String) (((FileInfoDicom) myFileInfo).getValue("0020,0011"));
                seriesNoMaster.trim();

                if (seriesNoRef.length() > 5) {
                    seriesNoRef = seriesNoMaster.substring(0, 5);
                }
            } else {
                seriesNoMaster = "";
                seriesNoRef = "";
            }

            createProgressBar(null, trim(fileName) + getSuffixFrom(fileName), FILE_READ);
          
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug("FileIO: " + error + "\n");
            } else {
                Preferences.debug("FileIO: " + error + "\n");
            }

            return null;
        }


        int[] indicies = null;
        int[] orient = new int[3]; // for FileInfoBase values. eg:FileInfoBase.ORI_S2I_TYPE;
        int pBarVal = 0;
        FileInfoDicom[] arrFileInfo = new FileInfoDicom[nListImages];

        if (!((FileInfoDicom) (myFileInfo)).isMultiFrame()) {

            /* this code is for setting the fixed axis in the 3-D image
             * needed for displaying overlay information properly we look at the first two images and see which position
             * varies by the third resolution.  that position indicates which axis the image is sliced on, which
             * indicates the orientation first go through headers and find out where in the array to store the image
             */
            // files are loaded into the appropriate place in the image
            // buffer as indicated by the slice numbers provided in indicies.
            // the values are set to the values of either zOri or rint (after
            // the sorting has been done) as needed, depending on sorting.
            indicies = new int[nListImages];

            int[] zOri = new int[nListImages]; // sorted image orientation values (ie., image z-axis)
            int[] rint = new int[nListImages]; // sorted image instance values.
            float[] zOrients = new float[nListImages]; // image orientation values as read in.
            float[] instanceNums = new float[nListImages]; // image instance numbers as read in.
            //progressBar.setTitle("Reading headers");

            for (i = 0, nImages = 0; i < nListImages; i++) {
                FileInfoDicom fileInfoTemp;

                try {

                    if (((float) i / (nListImages - 1) * 100) > pBarVal) {
                        pBarVal += 10;
                        progressBar.updateValue(Math.round((float) i / (10 * (nListImages - 1)) * 100), false);
                    }

                    ((FileDicom) imageFile).setFileName(fileList[i]);
                    ((FileDicom) imageFile).readHeader(true);
                    fileInfoTemp = (FileInfoDicom) (((FileDicom) imageFile).getFileInfo());

                    // If study and series number match - Continue;
                    if (((FileInfoDicom) (fileInfoTemp)).getValue("0020,0010") != null) {
                        studyID = (String) (((FileInfoDicom) fileInfoTemp).getValue("0020,0010")); //
                        studyID.trim();
                    }

                    if (((FileInfoDicom) (fileInfoTemp)).getValue("0020,0011") != null) {
                        seriesNo = (String) (((FileInfoDicom) fileInfoTemp).getValue("0020,0011")); //
                        seriesNo.trim();
                    }

                    if (((FileInfoDicom) (fileInfoTemp)).getValue("0020,0012") != null) {
                        acqNo = (String) (((FileInfoDicom) fileInfoTemp).getValue("0020,0012")); //
                        acqNo.trim();
                    }


                    if (performSort) {

                        if (seriesNo.equals(seriesNoMaster) && studyID.equals(studyIDMaster)) { // &&
                                                                                                // acqNo.equals(acqNoMaster))
                                                                                                // {
                            arrFileInfo[nImages] = (FileInfoDicom) (((FileDicom) imageFile).getFileInfo());

                            // this matrix is the matrix that converts this image into
                            // a standard DICOM axial image
                            matrix = ((FileInfoDicom) arrFileInfo[nImages]).getPatientOrientation();

                            if (matrix != null) {

                                /* transform the x location, y location, and z location, found
                                 * from the Image Position tag, by the matrix.  The tPt array now has the three numbers
                                 * arranged as if this image had been transformed.  The third place in the array holds
                                 * the number that the axis is being sliced along. xlocation, ylocation, zlocation are
                                 * from the DICOM tag 0020,0032 patient location;
                                 */
                                matrix.transform(((FileInfoDicom) arrFileInfo[nImages]).xLocation,
                                                 ((FileInfoDicom) arrFileInfo[nImages]).yLocation,
                                                 ((FileInfoDicom) arrFileInfo[nImages]).zLocation, tPt);

                                // tPt[2] is MIPAV's z-axis.  It is the position of the patient
                                // along the axis that the image was sliced on.
                                zOrients[nImages] = tPt[2];
                            } else {
                                zOrients[nImages] = 1;
                            }

                            // instance numbers used in case improper DICOM and position and orientation info not given
                            instanceNums[nImages] = (float) ((FileInfoDicom) arrFileInfo[nImages]).instanceNumber;
                            zOri[nImages] = nImages;
                            rint[nImages] = nImages;
                            nImages++;
                        }
                    } else {
                        arrFileInfo[nImages] = (FileInfoDicom) (((FileDicom) imageFile).getFileInfo());
                        matrix = ((FileInfoDicom) arrFileInfo[nImages]).getPatientOrientation();

                        if (matrix != null) {
                            matrix.transform(((FileInfoDicom) arrFileInfo[nImages]).xLocation,
                                             ((FileInfoDicom) arrFileInfo[nImages]).yLocation,
                                             ((FileInfoDicom) arrFileInfo[nImages]).zLocation, tPt);
                            zOrients[nImages] = tPt[2];
                        } else {
                            zOrients[nImages] = 1;
                        }

                        // instance numbers used in case improper DICOM and position and orientation info not given
                        instanceNums[nImages] = (float) ((FileInfoDicom) arrFileInfo[nImages]).instanceNumber;
                        zOri[nImages] = nImages;
                        rint[nImages] = nImages;
                        nImages++;
                    }

                } catch (IOException error) {
                    
                    if (!quiet) {
                        MipavUtil.displayError("FileIO: " + error);
                        Preferences.debug("FileIO: " + error + "\n");
                    } else {
                        Preferences.debug(" FileIO: " + error + "\n");
                    }

                    if (image != null) {
                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                }
            }

            if (matrix != null) {
                Preferences.debug("Dicom matrix = \n" + matrix + "\n");
            }

            /* if this method was told to, we will sort in more than one way:
             * we will first try to order the image set based on orientation of slices.  If there are slices in the
             * image set which have the same location on the Z-axis, the data may be a time-based set.  If we guess that
             * it is (and there aren't nearly enough test-datasets), we attempt to re-order the ordered Z-locations
             * based on instance number to pull out the time-data (ie., of two images with the same Z-axis, a lower
             * instance number was taken at an earlier time; all such earlier-time images are grouped).  If neither
             * solution seemed to work, then we order all the images strictly on the instance number.  There might be
             * some problems if we had to do this. If the list sent to this method was found using the getFileList()
             * method, all the names are in lexicographical order.  This is the default ordering if neither sorting
             * method works, or if performSort is false.
             */
            boolean valid = false; // !valid, !performSort, !fourthDimensional

            // will force sorting to go by input order.
            boolean fourthDimensional = false; // 4th dimensional

            if (performSort) {

                // sort so that instance numbers are in ascending order.
                // rint is the index to associate input file-list with the
                // instance number
                if (!sort(instanceNums, rint, nImages)) {
                    Preferences.debug("FileIO: instance numbers sort failed\n", 2);
                    System.err.println("FileIO: instance numbers sort failed on " + fileList[0]);
                }

                valid = true; // original ordering in case nImages == 1;

                // sort so that zOrients is now in ascending order.
                // zOri[i] represents where in the image buffer image
                // number i should be stored; so that if the images were
                // read in 1, 10, 11... (which happens often),
                // zOri[1] = 1 but zOri[2] = 2, rather than 10.
                if (nImages > 1) {
                    valid = sort(zOrients, zOri, nImages);
                }

                // If valid is false then one or more of the images has the
                // same position.  Most likely it is a 4D dataset.
                // let's deal with that possibility:
                if ((nImages > 1) && !valid) {

                    // Follow-on ordering:
                    // pre-order the orientation numbers to match the instance
                    // numbers.  This is done to accomodate 4D dicom sets.
                    // To describe the algo: I and L value lists which are
                    // independant of each other, and M, which is a copy of L.
                    // a & b are index lists for I and L, respectivly.
                    // L[b[z]] = M[a[z]]
                    // we copy the values of M, using order a, into L, using
                    // order b, thereby making L mimic I.
                    // we do this here with zOrients/zOri to make zOrient match
                    // the ordering of the Instance numbers.
                    float[] lima = new float[zOrients.length]; // temp

                    System.arraycopy(zOrients, 0, lima, 0, zOrients.length);

                    for (int z = 0; z < nImages; z++) {
                        zOrients[zOri[z]] = lima[rint[z]]; // copy z-location
                    }

                    zOri = rint; // copy the indexing

                    sort(zOrients, zOri, nImages); // now sort by orientation

                    // Rely on image instance number and position information.
                    // Build a list for all images at a particular location,
                    // although at different times, as judged by image instance.
                    // Create a list for all possible times in the imageset:
                    Vector timezonesList = new Vector();

                    //
                    // Hold the original list of orients and indices:
                    Vector orientsList = new Vector(nImages); // original index list

                    for (int k = 0; k < nImages; k++) { // load original list vector

                        OrientStatus oriReference = new OrientStatus(zOri[k], zOrients[k]);

                        orientsList.add(oriReference);
                    }

                    // each times list has a list of the images of different
                    // locations taken at the same time (in the same time-zone)
                    // we check on different times by going through the list of
                    // images and looking for the next lowest image instance
                    // with the same z location.  essentially, we
                    // pass through the orientation list multiple times,
                    // filling a single zone with each pass.
                    Vector tz;
                    OrientStatus ref0, refi;

                    while (orientsList.size() > 0) {
                        tz = new Vector(); // create a new timezone
                        ref0 = (OrientStatus) orientsList.remove(0);
                        tz.add(ref0); // remove 1st orient, put in timezone
                        Preferences.debug("Loading, and making comparison to: " + ref0.getIndex() + ".." +
                                          ref0.getLocation() + "\n", 4);

                        Vector orientsClone = (Vector) orientsList.clone();

                        for (Enumeration e = orientsClone.elements(); e.hasMoreElements();) {
                            refi = (OrientStatus) e.nextElement();
                            Preferences.debug("Looking at: " + refi.getIndex() + "..." + refi.getLocation(), 4);

                            if (!refi.equals(ref0)) {
                                ref0 = null;
                                ref0 = refi;
                                tz.add(refi);
                                Preferences.debug("...Accepting ", 4);

                                if (orientsList.remove(refi)) {
                                    Preferences.debug(" .... Successfully removed " + refi.getIndex(), 4);
                                }

                                Preferences.debug("!!  Comparison to: " + ref0.getIndex() + ".." + ref0.getLocation() +
                                                  "\n", 4);

                            } else {
                                Preferences.debug("\n", 4);
                            }
                        }

                        orientsClone.clear();
                        orientsClone = null;
                        timezonesList.add(tz);
                        tz = null;
                    }

                    orientsList.clear();
                    orientsList = null;

                    // new ordering for all timezones.
                    try {
                        int t = 0;

                        for (Enumeration e = timezonesList.elements(); e.hasMoreElements();) {
                            Preferences.debug("Timezone\n", 4);

                            for (Enumeration f = ((Vector) e.nextElement()).elements(); f.hasMoreElements();) {
                                OrientStatus ref = (OrientStatus) f.nextElement();
                                int k = ref.getIndex();

                                // concatenate the sublists into one ordering list.
                                zOri[k] = t;
                                zOrients[k] = ref.getLocation();
                                Preferences.debug("reordering: (" + k + "): " + zOri[k] + "..." + zOrients[k] + "\n",
                                                  4);
                                t++;
                            }
                        }

                        fourthDimensional = true;
                        Preferences.debug("4D, translation passes!  " + t + " slices!\n", 3);
                    } catch (ArrayIndexOutOfBoundsException enumTooFar) {
                        fourthDimensional = false;
                        Preferences.debug("NOT 4D, and DICOM translation fail!\n", 2);
                    }

                    timezonesList.clear();
                    timezonesList = null;
                    System.gc();
                } else {
                    Preferences.debug("Not a 4D Dataset\n", 4);
                }
                // System.out.println (" Dicom matrix = \n" + matrix + "\n");
                // System.out.println (" valid = " + valid);

            } // end of performSort

            // A separate and equally valid explanation of the Image Orientation matrix is: X patient  Y patient  Z
            // patient X image     1          0         0 Y image     0          1         0 Z image     0          0 1
            // In this case, the number "1" at X image, X patient means the x in the image is the same as the x in the
            // patient and goes in the same direction; thus, the image x goes from right to left.  If it were a "-1",
            // that would mean the image x goes from left to right. If it were a "0", and there was a "1" in X image, Y
            // patient, that would mean that the image x goes from the anterior of the patient to the posterior.  This
            // is the case in sagittal images, for example.
            //
            // We're concerned with what the z image is, because that tells us what the orientation is.  If there is a "1"
            // in X patient, the patient x is the image's z-axis, making this a sagittal image.  If there is a "1" in Y
            // patient, the patient y is the image's z-axis, making this a coronal image.  And if there is a "1" in Z
            // patient, the patient z is the image's z-axis, making this an axial image.  We look at absolute value
            // because here we are not concerned about right to left vs left to right, only which patient axis the
            // slices were taken along.
            float xCos = 0, yCos = 0, zCos = 0;

            // if (matrix != null && valid == true) {
            if (matrix != null) {

                // get back to original matrix
                xCos = (float) Math.abs(matrix.get(2, 0));
                yCos = (float) Math.abs(matrix.get(2, 1));
                zCos = (float) Math.abs(matrix.get(2, 2));

                for (int j = 0; j < orient.length; j++) {
                    int index = absBiggest(matrix.get(j, 0), matrix.get(j, 1), matrix.get(j, 2));

                    if (index == 0) {

                        if (matrix.get(j, 0) > 0) {
                            orient[j] = FileInfoBase.ORI_R2L_TYPE;
                        } else {
                            orient[j] = FileInfoBase.ORI_L2R_TYPE;
                        }
                    } else if (index == 1) {

                        if (matrix.get(j, 1) > 0) {
                            orient[j] = FileInfoBase.ORI_A2P_TYPE;
                        } else {
                            orient[j] = FileInfoBase.ORI_P2A_TYPE;
                        }
                    } else { // index == 2

                        if (matrix.get(j, 2) > 0) {
                            orient[j] = FileInfoBase.ORI_I2S_TYPE;
                        } else {
                            orient[j] = FileInfoBase.ORI_S2I_TYPE;
                        }
                    }
                }
            }

            // System.out.println( " xcos = " + xCos + " ycos = " + yCos + " zcos = " + zCos );
            if ((xCos > yCos) && (xCos > zCos)) {
                orientation = FileInfoBase.SAGITTAL;

                if (valid) {
                    indicies = zOri;
                } else if (!valid && fourthDimensional) {
                    indicies = zOri;
                    Preferences.debug("Reading image as 4th Dimensional.\n", 4);
                } else {
                    indicies = rint;
                }
            } else if ((yCos > xCos) && (yCos > zCos)) {
                orientation = FileInfoBase.CORONAL;

                if (valid == true) {
                    indicies = zOri;
                } else if (!valid && fourthDimensional) {
                    indicies = zOri;
                    Preferences.debug("Reading image as a 4th Dimensional " + "order.\n", 4);
                } else {
                    indicies = rint;
                }
            } else if ((zCos > xCos) && (zCos > yCos)) {
                orientation = FileInfoBase.AXIAL;

                if (valid == true) {
                    indicies = zOri;
                } else if (!valid && fourthDimensional) {
                    indicies = zOri;
                    Preferences.debug("Reading image as a 4th Dimensional " + "order.\n", 4);
                } else {
                    indicies = rint;
                }
            } // matrix was null, set orients based on instance number

            // problems if we reach this point!
            else if ((instanceNums.length > 1) && (instanceNums[0] != instanceNums[1])) {
                orientation = FileInfoBase.AXIAL;
                indicies = rint;
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
                orient[2] = FileInfoBase.ORI_I2S_TYPE;
            } else { // xLocation, yLocation, and zLocation were probably undefined and instance numbers equal.
                orientation = FileInfoBase.AXIAL;
                indicies = zOri;
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
                orient[2] = FileInfoBase.ORI_I2S_TYPE;
            }
        }

        if (nImages > 1) {
            extents = new int[3];
            extents[2] = nImages;
        } else {
            extents = new int[2];
        }

        extents[0] = myFileInfo.getExtents()[0];
        extents[1] = myFileInfo.getExtents()[1];

        myFileInfo.setExtents(extents);
        image = new ModelImage(((FileInfoDicom) myFileInfo).displayType, extents, studyIDMaster.trim() + "_" + seriesNoRef.trim());

        if (((FileInfoDicom) (myFileInfo)).isMultiFrame() == true) {
            image.setFileInfo(myFileInfo, 0);
        }

        // its probably a PET image an therefore reallocate data to store float image.
        if (((FileInfoDicom) myFileInfo).displayType != ((FileInfoDicom) myFileInfo).getDataType()) {
            image.setType(((FileInfoDicom) myFileInfo).displayType);
            image.reallocate(((FileInfoDicom) myFileInfo).displayType);
        }

        String filename;
        int start;
        int location;
        boolean multiframe = ((FileInfoDicom) (myFileInfo)).isMultiFrame();

        // loop through files, place them in image array
       // pInterface.setTitle(UI.getProgressBarPrefix() + "image " + fileList[0]);
        //myFileInfo.finalize();
        pBarVal = 0;

        for (i = 0; i < nImages; i++) {

            if (multiframe) {
                filename = fileList[0];
                start = i;
                location = i;
            } else {
                filename = fileList[i];
                start = 0;
                location = indicies[i];

                if (nImages == 1) {
                    location = 0;
                }
            }

            Preferences.debug("location: " + location + "\timg: " + filename + "\n");

            try {

                if (i == (nImages - 1)) {
                	progressBar.updateValue(100, false);
                } else if (((float) i / (nImages - 1) * 100) > pBarVal) {
                    pBarVal += 10;
                    progressBar.updateValue(10 + Math.round((float) i / (nImages - 1) * 90), false);
                }

                ((FileDicom) imageFile).setFileName(filename);

                // Reuse header that was read in above !!!!
                if (!multiframe) {
                    myFileInfo = arrFileInfo[i];
                }

                ((FileDicom) imageFile).setFileInfo((FileInfoDicom) myFileInfo);

                // Read the image
                if (image.getType() == ModelStorageBase.FLOAT) {
                    ((FileDicom) imageFile).readImage(bufferFloat, myFileInfo.getDataType(), start);
                } else {
                    ((FileDicom) imageFile).readImage(bufferShort, myFileInfo.getDataType(), start);
                }

                myFileInfo.setExtents(extents);
                myFileInfo.setAxisOrientation(orient);
                myFileInfo.setImageOrientation(orientation);

                float[] origin = new float[3];
                float[] newOriginPt = new float[3];

                origin[0] = ((FileInfoDicom) myFileInfo).xLocation;
                origin[1] = ((FileInfoDicom) myFileInfo).yLocation;
                origin[2] = ((FileInfoDicom) myFileInfo).zLocation;


                //TransMatrix dicomMatrix = (TransMatrix) (getMatrix().clone());
                // Finally convert the point to axial millimeter DICOM space.
                //dicomMatrix.transform(coord, tCoord);

                for (int j = 0; j < 3; j++) {

                    if (orient[j] == FileInfoBase.ORI_L2R_TYPE ||
                        orient[j] == FileInfoBase.ORI_R2L_TYPE){
                        newOriginPt[j] = origin[0];

                    }
                    else if (orient[j] == FileInfoBase.ORI_P2A_TYPE ||
                            orient[j] == FileInfoBase.ORI_A2P_TYPE){
                            newOriginPt[j] = origin[1];

                    }
                    else if (orient[j] == FileInfoBase.ORI_S2I_TYPE ||
                            orient[j] == FileInfoBase.ORI_I2S_TYPE){
                            newOriginPt[j] = origin[2];

                    }
                }

                myFileInfo.setOrigin(newOriginPt);
                image.setFileInfo(myFileInfo, location);

                if (image.getType() == ModelStorageBase.FLOAT) {
                    image.importData(location * length, bufferFloat, false);
                } else {
                    image.importData(location * length, bufferShort, false);
                }
            } catch (IOException error) {
                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                    Preferences.debug("FileIO: " + error + "\n");
                } else {
                    Preferences.debug("FileIO: " + error + "\n");
                }

                error.printStackTrace();

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (OutOfMemoryError error) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                    Preferences.debug("FileIO: " + error + "\n");
                } else {
                    Preferences.debug("FileIO: " + error + "\n");
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        }

        // Save the DICOM tag 0020,0037 Image Orientation to the image transformation matrix.
        if (matrix != null) {
            TransMatrix invMatrix = ((TransMatrix) (matrix.clone()));

            try {
                invMatrix.invert();
                image.setMatrix(invMatrix);

                for (int m = 0; m < nImages; m++) {
                    image.getFileInfo()[m].setTransformID(FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL);
                }
            } catch (RuntimeException rte) {
                invMatrix.identity();
                // MipavUtil.displayError("Error = " + rte);
            }
        }

        if (nListImages > 1) {
            image.getFileInfo(0).setMultiFile(true);
        }

        if (nImages > 1) {

            float sliceThickness = -1;
            float sliceSpacing = -1;

            // First check slice thickness tag:
            if ((((FileInfoDicom) image.getFileInfo(0)).getTagsList().get("0018,0050") != null) ||
                    (((FileInfoDicom) image.getFileInfo(0)).getTagsList().get("0018,0088") != null)) {

                if ((String) ((FileDicomTag) ((FileInfoDicom) image.getFileInfo(0)).getTagsList().get("0018,0050"))
                        .getValue(true) != null) {
                    sliceThickness = Float.parseFloat((String)
                                                          ((FileDicomTag) ((FileInfoDicom) image.getFileInfo(0))
                                                               .getTagsList().get("0018,0050")).getValue(true));
                }

                // 0018,0088 = Spacing Between Slices
                // 0018,0050 = Slice Thickness
                if ((String) ((FileDicomTag) ((FileInfoDicom) image.getFileInfo(0)).getTagsList().get("0018,0088"))
                        .getValue(true) != null) {
                    sliceSpacing = Float.parseFloat((String)
                                                        ((FileDicomTag) ((FileInfoDicom) image.getFileInfo(0))
                                                             .getTagsList().get("0018,0088")).getValue(true));
                }

                // System.err.println("Slice Spacing: " + sliceSpacing);
                if (sliceSpacing != -1) {
                    sliceThickness = sliceSpacing;
                    // System.err.println("Slice Thickness: " + sliceThickness);
                }

                if (sliceThickness > 0) {

                    for (int m = 0; m < nImages; m++) {
                        ((FileInfoDicom) image.getFileInfo(m)).getResolutions()[2] = sliceThickness;
                    }
                }
            }

            // finally, if slice spacing and slice location failed to produce a z-res,
            // check for image position
            if (((FileInfoDicom) image.getFileInfo(0)).getTagsList().get("0020,0032") != null) {

                float xLoc0 = ((FileInfoDicom) image.getFileInfo(0)).xLocation;
                float yLoc0 = ((FileInfoDicom) image.getFileInfo(0)).yLocation;
                float zLoc0 = ((FileInfoDicom) image.getFileInfo(0)).zLocation;

                float xLoc1 = ((FileInfoDicom) image.getFileInfo(1)).xLocation;
                float yLoc1 = ((FileInfoDicom) image.getFileInfo(1)).yLocation;
                float zLoc1 = ((FileInfoDicom) image.getFileInfo(1)).zLocation;

                float res3Dim = 1;

                res3Dim = ((xLoc0 - xLoc1) * (xLoc0 - xLoc1)) + ((yLoc0 - yLoc1) * (yLoc0 - yLoc1)) +
                          ((zLoc0 - zLoc1) * (zLoc0 - zLoc1));
                res3Dim = (float) Math.sqrt(res3Dim);

                // System.err.println("res3Dim Spacing: " + res3Dim);
                if ((res3Dim != 0)) {
                    ((FileInfoDicom) image.getFileInfo(0)).getResolutions()[2] = res3Dim;

                    // System.out.println (" res3Dim 1  = " + res3Dim);
                    for (int m = 1; m < (nImages - 1); m++) {

                        xLoc0 = ((FileInfoDicom) image.getFileInfo(m)).xLocation;
                        yLoc0 = ((FileInfoDicom) image.getFileInfo(m)).yLocation;
                        zLoc0 = ((FileInfoDicom) image.getFileInfo(m)).zLocation;

                        xLoc1 = ((FileInfoDicom) image.getFileInfo(m + 1)).xLocation;
                        yLoc1 = ((FileInfoDicom) image.getFileInfo(m + 1)).yLocation;
                        zLoc1 = ((FileInfoDicom) image.getFileInfo(m + 1)).zLocation;

                        res3Dim = ((xLoc0 - xLoc1) * (xLoc0 - xLoc1)) + ((yLoc0 - yLoc1) * (yLoc0 - yLoc1)) +
                                  ((zLoc0 - zLoc1) * (zLoc0 - zLoc1));
                        res3Dim = (float) Math.sqrt(res3Dim);

                        ((FileInfoDicom) image.getFileInfo(m)).getResolutions()[2] = res3Dim;
                    }

                    ((FileInfoDicom) image.getFileInfo(nImages - 1)).getResolutions()[2] = res3Dim;
                }
            }

            float sliceDifference = -1;

            // if slice thickness tag wasn't there or was 0, check slice location (and take the difference)
            if ((((FileInfoDicom) image.getFileInfo(0)).getTagsList().get("0020,1041") != null) &&
                    (sliceThickness == -1)) {

                if ((String) ((FileDicomTag) ((FileInfoDicom) image.getFileInfo(0)).getTagsList().get("0020,1041"))
                        .getValue(true) != null) {

                    sliceDifference = Float.parseFloat((String)
                                                           ((FileDicomTag) ((FileInfoDicom) image.getFileInfo(1))
                                                                .getTagsList().get("0020,1041")).getValue(true)) -
                                      Float.parseFloat((String)
                                                           ((FileDicomTag) ((FileInfoDicom) image.getFileInfo(0))
                                                                .getTagsList().get("0020,1041")).getValue(true));

                    //System.err.println("Slice difference: " + sliceDifference);

                    if ((Math.abs(sliceDifference) < sliceThickness) && (Math.abs(sliceDifference) > 0.001)) {
                        ((FileInfoDicom) image.getFileInfo(0)).getResolutions()[2] = sliceDifference;

                        for (int m = 0; m < (nImages - 1); m++) {
                            ((FileInfoDicom) image.getFileInfo(m)).getResolutions()[2] =
                                Float.parseFloat((String)((FileDicomTag)((FileInfoDicom)image.getFileInfo(m + 1)).getTagsList().get("0020,1041"))
                                                                                                                  .getValue(true)) -
                                                                                         Float.parseFloat((String)
                                                                                                              ((FileDicomTag)
                                                                                                                   ((FileInfoDicom)
                                                                                                                        image.getFileInfo(m))
                                                                                                                       .getTagsList().get("0020,1041"))
                                                                                                                  .getValue(true));
                        }

                        if (nImages > 2) {
                            ((FileInfoDicom) image.getFileInfo(nImages - 1)).getResolutions()[2] = ((FileInfoDicom)
                                                                                                        image.getFileInfo(nImages -
                                                                                                                          2))
                                                                                                       .getResolutions()[2];
                        }
                    }
                }
            }

            // see if we found z-res somewhere
            if ((sliceThickness == -1) && (sliceDifference == -1)) {
                System.err.println("error calculating z-resolution in FileIO.readDicom()");
            }
        }

        // That the image contains this tag, means that the image contains it's own
        // LUT, and that we should use it.
        if (((FileInfoDicom) image.getFileInfo(0)).getTagsList().get("0028,1201") != null) {
            LUT = imageFile.getLUT();
        }

        if (progressBar != null) {
        	progressBar.dispose();
        }
        imageFile = null;
        matrix = null;

        return image;
    }

    /**
     * Reads generic file from an absolute filename.
     *
     * @param   absoluteFilename  String - the absolute filename, including the path
     *
     * @return  ModelImage
     */
    public ModelImage readImage(String absoluteFilename) {

        if (absoluteFilename == null) {
            return null;
        }

        int lastIndex = absoluteFilename.lastIndexOf(File.separatorChar);

        if (lastIndex == -1) {

            // failed
            return null;
        }

        String path = absoluteFilename.substring(0, lastIndex);
        String filename = absoluteFilename.substring(lastIndex);

        return readImage(filename, path);
    }

    /**
     * Reads file, determines file type, and calls a read function specific to the file. That read function returns an
     * image and this function stores it in ModelImage. File is not multi file, file info is not previously known,
     * there's no "second address" for AFNI, and this is not an image B.
     *
     * @param   fileName  File name where image is located.
     * @param   fileDir   File directory where image is located.
     *
     * @return  The image that was read in from the file.
     */
    public ModelImage readImage(String fileName, String fileDir) {
        return readImage(fileName, fileDir, false, null, 0, false, false);
    }

    /**
     * Reads file, determines file type, and calls a read function specific to the file. That read function returns an
     * image and this function stores it in ModelImage.
     *
     * @param   fileName   File name where image is located.
     * @param   fileDir    File directory where image is located.
     * @param   multiFile  Flag indicating multi file.
     * @param   fileInfo   File info already known; will usually be null, but valid if called from script parser.
     *
     * @return  The image that was read in from the file.
     */
    public ModelImage readImage(String fileName, String fileDir, boolean multiFile, FileInfoBase fileInfo) {
        return readImage(fileName, fileDir, multiFile, fileInfo, 0, false, false);
    }

    /**
     * Reads file, determines file type, and calls a read function specific to the file. That read function returns an
     * image and this function stores it in ModelImage.
     *
     * @param   fileName   File name where image is located.
     * @param   fileDir    File directory where image is located.
     * @param   multiFile  Flag indicating multi file.
     * @param   fileInfo   File info already known; will usually be null, but valid if called from script parser.
     * @param   loadB      Flag indicating if this is an image B.
     *
     * @return  The image that was read in from the file.
     */
    public ModelImage readImage(String fileName, String fileDir, boolean multiFile, FileInfoBase fileInfo,
                                boolean loadB) {
        return readImage(fileName, fileDir, multiFile, fileInfo, 0, loadB, false);
    }

    /**
     * Reads file, determines file type, and calls a read function specific to the file. That read function returns an
     * image and this function stores it in ModelImage.
     *
     * @param   fileName       File name where image is located.
     * @param   fileDir        File directory where image is located.
     * @param   multiFile      Flag indicating multi file.
     * @param   fileInfo       File info already known; will usually be null, but valid if called from script parser.
     * @param   secondAddress  Address of second TIFF header
     *
     * @return  The image that was read in from the file.
     */
    public ModelImage readImage(String fileName, String fileDir, boolean multiFile, FileInfoBase fileInfo,
                                int secondAddress) {
        return readImage(fileName, fileDir, multiFile, fileInfo, secondAddress, false, false);
    }


    /**
     * Reads file, determines file type, and calls a read function specific to the file. That read function returns an
     * image and this function stores it in ModelImage.
     *
     * @param   fileName       File name where image is located.
     * @param   fileDir        File directory where image is located.
     * @param   multiFile      Flag indicating multi file loading of images from directory. <code>true</code> is load
     *                         images of this filetype to be loaded as a set (when this feature is supported).
     * @param   fileInfo       File info already known; will usually be null, but valid if called from script parser.
     * @param   secondAddress  Address of second TIFF header
     * @param   loadB          Flag indicating if this is an image B.
     * @param   one            A load-single flag. <code>true</code> indicates that the method is to load only the file
     *                         that is defined by the <code>fileName</code>, rather than a multiple file load of a 3d
     *                         Image.
     *
     * @return  The image that was read in from the file.
     */
    public ModelImage readImage(String fileName, String fileDir, boolean multiFile, FileInfoBase fileInfo,
                                int secondAddress, boolean loadB, boolean one) {
        ModelImage image = null;
        int fileType = FileBase.UNDEFINED;

        if ((fileName == null) || (fileDir == null)) {
            return null;
        }

        this.fileName = fileName;
        this.fileDir = fileDir;
        fileName.trim();

        String beginString;

        if (fileName.lastIndexOf('.') > -1) {
            beginString = fileName.substring(0, fileName.lastIndexOf('.'));
        } else {
            beginString = fileName;
        }

        if ((beginString.equalsIgnoreCase("d3proc")) || (beginString.equalsIgnoreCase("reco")) ||
                (beginString.equalsIgnoreCase("2dseq"))) {
            fileType = FileBase.BRUKER;
        }

        try {

            if (fileType != FileBase.BRUKER) {
                fileType = getFileType(fileName, fileDir, false); // set the fileType based on the filename extension

                if (fileType == FileBase.UNDEFINED) {
                    fileType = isDicom(fileName, fileDir);
                }
                
                if (fileType == FileBase.UNDEFINED) {
                    fileType = isGESigna4X(fileName, fileDir);
                }
                
                if (fileType == FileBase.UNDEFINED) { // if image type not defined by extension, popup
                    fileType = getFileType(); // dialog to get user to define image type
                }

                fileType = chkMultiFile(fileType, multiFile); // for multifile support...
            }
        } catch (IOException ioe) {

            if (ioe instanceof FileNotFoundException) {
                MipavUtil.displayError("File does not exist '" + fileDir + fileName + "'.");

                return null;
            }

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + ioe);
                Preferences.debug("FileIO: " + ioe + "\n");
            } else {
                Preferences.debug("FileIO: " + ioe + "\n");
            }

            fileType = FileBase.UNDEFINED;
        }


        try {

            switch (fileType) {

                case FileBase.TIFF:
                    image = readTiff(fileName, fileDir, one);
                    break;

                case FileBase.TIFF_MULTIFILE:
                    image = readTiffMulti(fileName, fileDir);
                    break;

                case FileBase.COR:
                    image = readCOR(fileName, fileDir);
                    break;

                case FileBase.BRUKER:
                    image = readBRUKER(fileName, fileDir, one);
                    break;

                case FileBase.LSM:
                    image = readLSM(fileName, fileDir, secondAddress, one);
                    break;

                case FileBase.LSM_MULTIFILE:
                    image = readLSMMulti(fileName, fileDir);
                    break;

                case FileBase.STK:
                    image = readSTK(fileName, fileDir, one);
                    break;

                case FileBase.AVI:
                    image = readAvi(fileName, fileDir, one, false);
                    break;

                case FileBase.QT:
                    image = readAvi(fileName, fileDir, one, true);
                    break;

                case FileBase.RAW:
                    image = readRaw(fileName, fileDir, fileInfo);
                    break;

                case FileBase.RAW_MULTIFILE:
                    image = readRawMulti(fileName, fileDir, fileInfo);
                    break;

                case FileBase.ANALYZE:
                    image = readAnalyze(fileName, fileDir, one);
                    break;

                case FileBase.MGH:
                    image = readMGH(fileName, fileDir, one);
                    break;

                case FileBase.NIFTI:
                    image = readNIFTI(fileName, fileDir, one);
                    break;

                case FileBase.NRRD:
                    image = readNRRD(fileName, fileDir, one);
                    break;

                case FileBase.SPM:
                    image = readSPM(fileName, fileDir, one);
                    break;

                case FileBase.CHESHIRE:
                    image = readCheshire(fileName, fileDir);
                    break;

                case FileBase.ANALYZE_MULTIFILE:
                    image = readAnalyzeMulti(fileName, fileDir);
                    break;

                case FileBase.NIFTI_MULTIFILE:
                    image = readNIFTIMulti(fileName, fileDir);
                    break;

                case FileBase.DICOM:
                    if (!multiFile) {
                    	this.fileDir = fileDir;
                    	image = readDicom(fileName, new String[]{fileName.trim()}, false);
                    } else {
                        image = readDicom(fileName, getFileList(fileDir, fileName), true);
                    }

                    break;

                case FileBase.MEDVISION:
                    image = readMedVision(fileName, fileDir);
                    break;

                case FileBase.MAP:
                    image = readMap(fileName, fileDir);
                    break;

                case FileBase.JIMI:
                    image = readJimi(fileName, fileDir, multiFile);
                    break;

                case FileBase.MINC:
                    image = readMinc(fileName, fileDir, one);
                    break;

                case FileBase.AFNI:
                    image = readAfni(fileName, fileDir, loadB);
                    break;

                case FileBase.ICS:
                    image = readICS(fileName, fileDir);
                    break;

                case FileBase.INTERFILE:
                    image = readInterfile(fileName, fileDir, one);
                    break;

                case FileBase.BIORAD:
                    image = readBioRad(fileName, fileDir, one);
                    break;

                case FileBase.FITS:
                    image = readFits(fileName, fileDir, one);
                    break;

                case FileBase.DM3:
                    image = readDM3(fileName, fileDir, one);
                    break;

                case FileBase.TMG:
                    image = readTMG(fileName, fileDir);
                    break;

                case FileBase.MRC:
                    image = readMRC(fileName, fileDir);
                    break;

                case FileBase.OSM:
                    image = readOSM(fileName, fileDir);
                    break;

                case FileBase.MAGNETOM_VISION:
                    image = readMagnetomVision(fileName, fileDir, one);
                    break;

                case FileBase.GE_GENESIS:
                    image = readGEGenesis5X(fileName, fileDir);
                    break;

                case FileBase.GE_SIGNA4X:
                    image = readGESigna4X(fileName, fileDir);
                    break;

                case FileBase.MICRO_CAT:
                    image = readMicroCat(fileName, fileDir, one);
                    break;

                case FileBase.XML:
                    image = readXML(fileName, fileDir, one);
                    break;

                case FileBase.XML_MULTIFILE:
                    image = readXMLMulti(fileName, fileDir);
                    break;

                default:
                    return null;
            }

            if (image != null) {
            	if (progressBar != null) {
            		progressBar.dispose();
            	}
                if ((image.getType() == ModelStorageBase.COMPLEX) || (image.getType() == ModelStorageBase.DCOMPLEX)) {
                    image.calcMinMaxMag(true);
                } else {
                    image.calcMinMax();
                    // image.setImageDirectory(fileDir);
                }
            }
        } catch (Exception error) {
        	if (progressBar != null) {
        		progressBar.dispose();
        	}
            error.printStackTrace();

            if (!quiet) {
                MipavUtil.displayError("Unable to load image.  See debug window for more details.");
            }

            Preferences.debug("Error while loading " + fileDir + fileName + ".\n" + error + "\n");

            return null;
        }

        return image;
    }

    /**
     * Reads file, determines file type, and calls a read function specific to the file. That read function returns an
     * image and this function stores it in ModelImage. File is not multi file, file info is not previously known,
     * there's no "second address" for AFNI, and this is not an image B. This is just one image regardless of how many
     * slices there are; the middle slice of a 3D dataset will be displayed.
     *
     * @param   fileName  File name where image is located.
     * @param   fileDir   File directory where image is located.
     *
     * @return  The image that was read in from the file.
     */
    public ModelImage readOneImage(String fileName, String fileDir) {
        return readImage(fileName, fileDir, false, null, 0, false, true);
    }

    /**
     * This method examines parameter <i>fileList <i>and loads TIFF files according to the order in the list. It can
     * load 2, 3, or 4 channel images. The method will examine channelMap to determine in which order the ARGB channels
     * should be interleaved.</i></i>
     *
     * @param   fileList                File[] - the list of File objects, preordered
     * @param   numChannels             int - the number of channels the result image is to have
     * @param   channelMap              int[] - a mapping of channels (ARGB) to positions. For example, a channelMap
     *                                  parameter of {2, 1, 0, 3} means the first channel will be G, the second will be
     *                                  R, the third will be A and the last will be B.
     * @param   showOrderedProgressBar  boolean - This parameter is used to control whether the local progress bar is
     *                                  shown. Note this is different than FileIO's global progress bar. The reason for
     *                                  the difference is because this method uses FileIO's readOneImage() method. That
     *                                  method uses the global progress bar. Its useless to have two progress bars, and
     *                                  FileIO isn't set up in a way that allows this method to directly control the
     *                                  global progress bar, hence the need for its own local one.
     * @param   subsampleDimension      - the dimensions of the result image if subsampling is desired. To skip
     *                                  subsampling, this parameter should be null
     * @param   forceUBYTE              boolean - force the image to be constructed with an unsigned byte data type
     *
     * @return  ModelImage
     */
    public ModelImage readOrderedARGB(File[] fileList, int numChannels, int[] channelMap,
                                      boolean showOrderedProgressBar, Dimension subsampleDimension,
                                      boolean forceUBYTE) {
        ModelImage modelImageTemp = null;
        ModelImage modelImageResult = null;
        float[] oneSliceBuffer;
        float[] resultImageBuffer;
        int[] imageExtents;
        int sliceSize;

        try // read one image in order to get image dimensions
        {
            modelImageTemp = readOneImage(fileList[0].getName(),
                                          fileList[0].getParentFile().getAbsolutePath() + File.separator);

            if (subsampleDimension != null) {
                modelImageTemp = subsample(modelImageTemp, subsampleDimension);
            }

            imageExtents = modelImageTemp.getExtents();

            sliceSize = imageExtents[0] * imageExtents[1]; // width * height

            resultImageBuffer = new float[sliceSize * 4]; // the * 4 is because there are 4 channels - ARGB

            oneSliceBuffer = new float[sliceSize];
        } catch (Exception ioe) {
            ioe.printStackTrace();

            return null;
        }

        createProgressBar(null, "files", FILE_READ);
       
        // allocate memory for the result ModelImage.
        int[] extents = { imageExtents[0], imageExtents[1], fileList.length / numChannels };

        if (modelImageTemp.getType() == ModelStorageBase.USHORT) {
            modelImageResult = new ModelImage(ModelStorageBase.ARGB_USHORT, extents, modelImageTemp.getImageName());
        } else if (modelImageTemp.getType() == ModelStorageBase.FLOAT) {
            modelImageResult = new ModelImage(ModelStorageBase.ARGB_FLOAT, extents, modelImageTemp.getImageName());
        } else {
            modelImageResult = new ModelImage(ModelStorageBase.ARGB, extents, modelImageTemp.getImageName());
        }

        extents = null;
        modelImageTemp.disposeLocal(false);

        for (int n = 0; n < fileList.length; n++) {
            int channel = channelMap[n % numChannels];
            int currentSlice = (int) (n / numChannels);

            if (fileList[n].exists()) {

                try {
                    modelImageTemp = readOneImage(fileList[n].getName(),
                                                  fileList[n].getParentFile().getAbsolutePath() + File.separator);

                    if (subsampleDimension != null) {
                        modelImageTemp = subsample(modelImageTemp, subsampleDimension);
                    }

                    modelImageTemp.exportData(0, oneSliceBuffer.length, oneSliceBuffer); // export one slice at a time
                                                                                         // to result ModelImage

                    for (int i = 0; i < oneSliceBuffer.length; i++) {
                        resultImageBuffer[(i * 4) + channel] = oneSliceBuffer[i]; // arrange interleaved pixels
                    }

                    modelImageResult.importData(currentSlice * sliceSize * 4, resultImageBuffer, false);
                    copyResolutions(modelImageTemp, modelImageResult, currentSlice);

                    modelImageTemp.disposeLocal(false);
                } catch (IOException ioe) {
                    ioe.printStackTrace();

                    break;
                }
            } else {
                Preferences.debug("File does not exist: " + fileList[n].getName());
            }

            progressBar.updateValue((int) (((n + 1) / (float) fileList.length) * 100), false);
        }

        modelImageResult.calcMinMax();

        
        progressBar = null;
        resultImageBuffer = null;
        oneSliceBuffer = null;
        channelMap = null;
        modelImageTemp = null;

        if ((forceUBYTE == true) && (modelImageResult.getType() != ModelStorageBase.ARGB)) {
            return convertToARGB(modelImageResult);
        }

        return modelImageResult;
    }


    /**
     * This method will load a group of files in the order of <i>fileList<i>. The result will be a grayscale ModelImage.
     * </i></i>
     *
     * @param   fileList              File[] - list of File objects, preordered
     * @param   showLocalProgressBar  boolean - This parameter is used to control whether the local progress bar is
     *                                shown. Note this is different than FileIO's global progress bar. The reason for
     *                                the difference is because this method uses FileIO's readOneImage() method. That
     *                                method uses the global progress bar. Its useless to have two progress bars, and
     *                                FileIO isn't set up in a way that allows this method to directly control the
     *                                global progress bar, hence the need for its own local one.
     * @param   subsampleDimension    - the dimensions of the result image if subsampling is desired. To skip
     *                                subsampling, this parameter should be null
     * @param   forceUBYTE            boolean - force the image to be constructed with an unsigned byte data type
     *
     * @return  ModelImage
     */
    public ModelImage readOrderedGrayscale(File[] fileList, boolean showLocalProgressBar, Dimension subsampleDimension,
                                           boolean forceUBYTE) {
        ModelImage modelImageTemp = null;
        ModelImage modelImageResult = null;
        float[] oneSliceBuffer;

        createProgressBar(null, "files", FILE_READ);

        try {

            // read one image so we can get extents
            modelImageTemp = readOneImage(fileList[0].getName(),
                                          fileList[0].getParentFile().getAbsolutePath() + File.separator);

            if (subsampleDimension != null) // subsample the image if we have subsampling dimensions
            {
                modelImageTemp = subsample(modelImageTemp, subsampleDimension);
            }

            // create a buffer to hold exchange image data between temp and result images
            oneSliceBuffer = new float[modelImageTemp.getExtents()[0] * modelImageTemp.getExtents()[1]];

            // the first slice has already been read. instead of re-reading it in the loop, export to buffer and save
            // an iteration
            modelImageTemp.exportData(0, oneSliceBuffer.length, oneSliceBuffer);

            // the result image's dimensions (possibly subsampled dimensions)
            int[] extents = { modelImageTemp.getExtents()[0], modelImageTemp.getExtents()[1], fileList.length };

            modelImageResult = new ModelImage(modelImageTemp.getType(), extents, modelImageTemp.getImageName());
            copyResolutions(modelImageTemp, modelImageResult, 0); // save the resolutions from the file info structure

            extents = null;
            modelImageTemp.disposeLocal(false);

            // import first slice to result image from modelImageTemp
            modelImageResult.importData(0, oneSliceBuffer, false);

            progressBar.updateValue((int) ((1.0f / (float) fileList.length) * 100), false);

            for (int i = 1; i < fileList.length; i++) {

                if (fileList[i].exists()) {

                    try {

                        // read images one slice at a time
                        modelImageTemp = readOneImage(fileList[i].getName(),
                                                      fileList[i].getParentFile().getAbsolutePath() + File.separator);

                        if (subsampleDimension != null) // subsample if we have subsampling dimensions
                        {
                            modelImageTemp = subsample(modelImageTemp, subsampleDimension);
                        }

                        modelImageTemp.exportData(0, oneSliceBuffer.length, oneSliceBuffer);
                        copyResolutions(modelImageTemp, modelImageResult, i);

                        modelImageResult.importData(i * oneSliceBuffer.length, oneSliceBuffer, false);
                    } catch (IOException ioe) {
                        ioe.printStackTrace();

                        break;
                    } finally {
                        modelImageTemp.disposeLocal(false);
                    }
                } else {
                    Preferences.debug("File does not exist: " + fileList[i].getName());
                }

                progressBar.updateValue((int) (((float) (i + 1) / (float) fileList.length) * 100), false);
            }

            modelImageResult.calcMinMax();

            if ((forceUBYTE == true) && (modelImageResult.getType() != ModelStorageBase.UBYTE)) {
                oneSliceBuffer = null;

                return convertToUBYTE(modelImageResult);
            }

            return modelImageResult;
        } catch (Exception ioe) {
            ioe.printStackTrace();

            return null;
        } finally {
            if (progressBar != null) {
            	progressBar.dispose();
            }
            modelImageTemp.disposeLocal();
            modelImageTemp = null;
            oneSliceBuffer = null;
        }
    }

    /**
     * Reads the XCEDE schema file.
     *
     * @param  fileName   the file name of the XCEDE file.
     * @param  directory  the directory of the XCEDE file.
     *
     * @return the root element of the XCEDE schema.
     */
    public XCEDEElement readXCEDE(String fileName, String directory){
        FileXCEDEXML xcedeFile;
        xcedeFile = new FileXCEDEXML(UI, fileName, directory);
        return xcedeFile.parse();
    }

    /**
     * Reads a thumbnail image from an XML file. if thumbnail is empty, will returned FileImageXML will be null.
     *
     * @param   name       filename
     * @param   directory  file's directory
     *
     * @return  FileImageXML containing thumbnail or null
     */
    public FileImageXML readXMLThumbnail(String name, String directory) {
        FileImageXML xmlTemp = new FileImageXML(UI, name, directory);
        float[][] res = null;

        try {
            TalairachTransformInfo talairach = new TalairachTransformInfo();
            res = xmlTemp.readHeader(name, directory, talairach);

            if ((res != null) && (xmlTemp.getThumbnail() != null)) {
                return xmlTemp;
            }
        } catch (IOException ioex) {
            System.err.println("Got IOException");
        }

        return null;
    }

    /**
     * Sets the directory where the file will be saved or opened.
     *
     * @param  dir  String directory
     */
    public void setFileDir(String dir) {
        this.fileDir = dir;
    }

    /**
     * Sets the LUT.
     *
     * @param  lut  the lookup table.
     */
    public void setModelLUT(ModelLUT lut) {
        this.LUT = lut;
    }

    /**
     * Sets the RGB.
     *
     * @param  rgb  lut the lookup table.
     */
    public void setModelRGB(ModelRGB rgb) {
        this.modelRGB = rgb;
    }

    /**
     * Sets the progress bar (either panel or frame) to be used in image opening to update status.
     *
     * @param  pBar  ProgressBarInterface
     */
    public void setPBar(ProgressBarInterface pBar) {
       // this.pInterface = pBar;
    }

    /**
     * Refers to whether or not the FileIO will send alerts to the user about progress or errors.
     *
     * @param  q  Indicates if the output from the methods in this object are to display dialogs interrupting flow in a
     *            locally-defined manner. <code>true</code> indicates that the process is to NOT inform the user and
     *            therefore be <q>quiet</q>.
     */
    public void setQuiet(boolean q) {
        quiet = q;
    }

    /**
     * Determines file type from the file name and calls different write functions based on the file type. Stores file
     * in the specified file name and directory given by the options. Calls appropriate dialogs if necessary. Supports
     * files of type raw, analyze, and DICOM.
     *
     * @param  image    Image to write.
     * @param  options  Needed info to write this image.
     */
    public void writeImage(ModelImage image, FileWriteOptions options) {
        int fileType;
        String suffix;

        // set it to quiet mode (no prompting) if the options were
        // created during a script
        if (options.isScript() == true) {
            quiet = true;
        }

        if (options.isSaveAs()) { // if we're doing a save-as op, then try to get the filetype from the name
            fileType = getFileType(options.getFileName(), options.getFileDirectory(), true);
            options.setDefault(true); // this would already be set....  hrmm....
        } else { // otherwise, get the file-type from the file-info.
            fileType = image.getFileInfo(0).getFileFormat();
        }

        if (fileType == FileBase.UNDEFINED) { // if type is still undef, look for user input (when not quiet)
            fileType = options.getFileType(); // get saved file type from options
            options.setSaveAs(true); // can't tell from extension, so must be a save as.

            // options.setSaveInSubdirectory(true);//  .... "" ...., so save into its own subdirectory.
            if (fileType == FileBase.UNDEFINED) { // file type wasn't set, so call dialog
                fileType = getFileType(); // popup dialog to determine filetype

                if (unknownIODialog.isCancelled()) {
                    return;
                }

                suffix = unknownIODialog.getSuffix(); // get the expected suffix from the dialog
            } else if (fileType == FileBase.JIMI) { // if type is JIMI, then try and use suffix from fileInfo
                suffix = image.getFileInfo(0).getFileSuffix();

                // if suffix wasn't set, then use the default suffix for fileType
                if ((suffix == null) || suffix.equals("")) {
                    suffix = getSuffix(fileType);
                }

                Preferences.debug("FileIO save:  suffix = " + suffix + "\n");
            } else {
                suffix = getSuffix(fileType); // get suffix from what the file type should be
            }

            options.setFileName(options.getFileName() + suffix); // append file extension
        } else if (fileType == FileBase.JIMI) { // if type is JIMI, then try and use suffix from fileInfo

            // if filename already has a suffix then don't need to do anything
            if (getSuffixFrom(options.getFileName()).equals("")) {
                suffix = image.getFileInfo(0).getFileSuffix();

                // if suffix wasn't set, then use the default suffix for fileType
                if ((suffix == null) || suffix.equals("")) {
                    suffix = getSuffix(fileType);
                }

                Preferences.debug("FileIO save:  suffix = " + suffix + "\n");

                options.setFileName(options.getFileName() + suffix); // append file extension
            }
        }

        if (!options.isSet()) {
            options.setFileType(fileType);
            options.setMultiFile(image.getFileInfo(0).getMultiFile());
            options.setPackBitEnabled((fileType == FileBase.TIFF) &&
                                          ((image.getFileInfo(0).getDataType() == ModelStorageBase.BYTE) ||
                                               (image.getFileInfo(0).getDataType() == ModelStorageBase.UBYTE)));
        }

        if (options.isSaveAs() && !options.isSet()) {

            if (!callDialog(image.getExtents(), (fileType == FileBase.TIFF), options)) {
                return;
            }
        }

        boolean success = false;
        
        switch (fileType) {

            case FileBase.RAW:
            	success = writeRaw(image, options);
                break;

            case FileBase.ANALYZE:
            	success = writeAnalyze(image, options);
            	if (success) {
            		if (image.getFileInfo(0) instanceof FileInfoImageXML) {
            			FileInfoAnalyze[] info = new FileInfoAnalyze[image.getFileInfo().length];
            			
            			for (int i = 0; i < info.length; i++) {
            				info[i] = new FileInfoAnalyze(image.getFileInfo(0).getFileName(),
            						image.getFileInfo(0).getFileDirectory(), FileBase.ANALYZE);
            			}
            			
            			FileInfoBase.copyCoreInfo(image.getFileInfo(), info);
            			image.setFileInfo(info);
            		}
            	}
                break;

            case FileBase.NIFTI:
            	success = writeNIFTI(image, options);
            	if (success) {
                if (image.getFileInfo(0) instanceof FileInfoImageXML) {
                    FileInfoNIFTI[] info = new FileInfoNIFTI[image.getFileInfo().length];

                    for (int i = 0; i < info.length; i++) {
                        info[i] = new FileInfoNIFTI(image.getFileInfo(0).getFileName(),
                                                    image.getFileInfo(0).getFileDirectory(), FileBase.NIFTI);
                    }

                    FileInfoBase.copyCoreInfo(image.getFileInfo(), info);
                    image.setFileInfo(info);
                }
            	}
                break;

            case FileBase.SPM:
            	success =  writeSPM(image, options);
            	if (success) {
                if (image.getFileInfo(0) instanceof FileInfoImageXML) {
                    FileInfoSPM[] info = new FileInfoSPM[image.getFileInfo().length];

                    for (int i = 0; i < info.length; i++) {
                        info[i] = new FileInfoSPM(image.getFileInfo(0).getFileName(),
                                                  image.getFileInfo(0).getFileDirectory(), FileBase.SPM);
                    }

                    FileInfoBase.copyCoreInfo(image.getFileInfo(), info);
                    image.setFileInfo(info);
                }
            	}
                break;

            case FileBase.TIFF:
            	success = writeTiff(image, options);
                break;

            case FileBase.MGH:
            	success = writeMGH(image, options);
                break;

            case FileBase.DICOM:
            	success = writeDicom(image, options);
                break;

            case FileBase.JIMI:
            	success = writeJimi(image, options);
                break;

            case FileBase.MINC:
            	success = writeMinc(image, options);
                break;

            case FileBase.INTERFILE:
            	success = writeInterfile(image, options);
                break;

            case FileBase.FITS:
            	success = writeFits(image, options);
                break;

            case FileBase.MRC:
            	success = writeMRC(image, options);
                break;

            case FileBase.OSM:
            	success = writeOSM(image, options);
                break;


            case FileBase.COR:
            	success = writeCOR(image, options);
                break;

            case FileBase.AFNI:
            	success = writeAfni(image, options);
                break;

            case FileBase.ICS:
            	success = writeICS(image, options);
                break;

            case FileBase.XML:
            	success = writeXML(image, options);
                break;

            case FileBase.AVI:
            	success = writeAvi(image, options);
                break;

            default:
                if (!quiet) {
                    MipavUtil.displayError("File type unknown.  Try Save Image As; \notherwise, the file type is not supported.");
                }

                return;
        }

        if (progressBar != null) {
        	progressBar.dispose();
        }
        
        
        if (success) {
        	if (options.isMultiFile()) {
        		String fName = options.getFileName();
        		String start = Integer.toString(options.getStartNumber());
        		int numDig = options.getDigitNumber();
        		for (int i = 1; i < numDig; i++) {
        			start = "0" + start;
        		}
        		fName = fName.substring(0, fName.indexOf(".")) + start + fName.substring(fName.indexOf("."), fName.length());
        		Preferences.setLastImage(options.getFileDirectory() + fName, true, image.getNDims());
        	} else {
        		Preferences.setLastImage(options.getFileDirectory() + options.getFileName(), false, image.getNDims());
        	}
            //updates menubar for each image
            Vector imageFrames = UI.getImageFrameVector();

            if (imageFrames.size() < 1) {
                UI.buildMenu();
                UI.setControls();
            } else {
                UI.buildMenu();

                for (int i = 0; i < imageFrames.size(); i++) {
                    if (imageFrames.elementAt(i) instanceof ViewJFrameImage) {
                        ((ViewJFrameImage) (imageFrames.elementAt(i))).updateMenubar();
                    }
                }

                UI.getActiveImageFrame().setControls();
            }
        	ScriptableActionInterface action;
        	if (options.isSaveAs()) {
        		action = new ActionSaveImageAs(image, options);
        	} else {
        		action = new ActionSaveImage(image, options);
        	}
        	ScriptRecorder.getReference().addLine(action);
        }
    }


    /**
     * Writes project information to a file.
     *
     * @param   projectInfo  The project information to be written to the file
     * @param   options      Write options that control aspects of writing the project information.
     *
     * @return  True if the file was successfully saved to a file.
     */
    public boolean writeProject(FileInfoProject projectInfo, FileWriteOptions options) {
        FileProject projectFile;

        try {
            projectFile = new FileProject(options.getFileName(), options.getFileDirectory());

            // System.out.println( "writing project" );
            projectFile.writeProject(projectInfo, options.getFileName(), options.getFileDirectory());
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Provides a method of conversion from <code>FileInfoDicom</CODE> to <CODE>FileInfoImageXML</CODE>, by filling the
     * <CODE>FileInfoImageXML</CODE> with sets of chosen image information (from the DICOM tags).
     *
     * <p>The XML format suggests that each DICOM tag become a seperate set, and any multiple values stored in the DICOM
     * tag, becomes parameter data. The set name is the DICOM tag name. Each value of the tag is stored as a separate
     * parameter in the set, and its description is stored as the DICOM key (group and element number, as displayed in
     * the file, &quot;dicom.dictionary&quot;) along with its position in the value multiplicity. All values stored are
     * stored as value-type &quot;string&quot;, and neither date nor time are set. Order of entries is not guaranteed,
     * as order is not meaningful in XML.</p>
     *
     * <p>Be sure to see the image.xsd file for more information.</p>
     *
     * @see    #getDicomSaveList(FileInfoDicom)
     * @see    JDialogDicom2XMLSelection
     *
     * @param  sourceInfo  The FileInfoDicom which is the source for user-selectable tags
     * @param  destInfo    The FileInfoImageXML that holds the image information to be stored in XML format.
     */
    protected boolean dataConversion(FileInfoDicom sourceInfo, FileInfoImageXML destInfo) {

        // when the original image is a DICOM image, we want to save this as
        // XML, so look, or ask, for a dicom dictionary list of tags to save
        // into the XML
        // load the tags to keep:
        Hashtable tags2save = getDicomSaveList(sourceInfo);

        if (tags2save == null) {
            return false;
        }

        // now convert that DICOM tags list into an XML tags List:
        Enumeration e = tags2save.keys();

        while (e.hasMoreElements()) {
            FileDicomKey tagKey = (FileDicomKey) e.nextElement();
            FileDicomTag dicomTag = (FileDicomTag) tags2save.get(tagKey);

            destInfo.createPSet(dicomTag.getName());

            Object[] tagValues = new Object[0];

            try {
                tagValues = dicomTag.getValueList();
            } catch (NullPointerException npe) {
                tagValues[0] = null;
            }

            // set the parameter values for as many values as the tag holds:
            for (int q = 0; q < tagValues.length; q++) {

                if (tagValues[0] == null) {
                    continue;
                }

                // write the DICOM tags & their values into the XML file.
                destInfo.getCurrentPSet().addParameter(dicomTag.getName() + "[" + q + "]");
                destInfo.getCurrentPSet().getCurrentParameter().setDescription("(" + tagKey + ") [" + q + "]");
                destInfo.getCurrentPSet().getCurrentParameter().setValueType("string");

                try {

                    if (tagValues[q].toString().indexOf(0) != -1) {

                        // if there are NULL characters, remove them
                        // before saving the value
                        StringBuffer noNulls = new StringBuffer(tagValues[q].toString());

                        try {

                            while (noNulls.indexOf("\u0000") != -1) { // removing NULL
                                noNulls.deleteCharAt(noNulls.indexOf("\u0000"));
                            }

                            destInfo.getCurrentPSet().getCurrentParameter().setValue(noNulls.toString());
                        } catch (StringIndexOutOfBoundsException nullNotThere) {
                            destInfo.getCurrentPSet().getCurrentParameter().setValue("");
                            System.err.println("(" + tagKey + ") Trying to output " + "current string bounded by " +
                                               "colons, nulls and all:");

                            try {
                                System.err.println(":" + noNulls.toString() + ":");
                            } catch (Exception couldnt) {
                                System.err.println("...Couldn't output noNulls string.");
                            }

                            Preferences.debug("Error converting DICOM to XML.");
                            Preferences.debug("  Empty string written for :");
                            Preferences.debug(" (" + tagKey + ")\n");
                        }
                    } else {
                        destInfo.getCurrentPSet().getCurrentParameter().setValue(tagValues[q].toString());
                    }
                } catch (NullPointerException npe) {
                    Preferences.debug("Error converting DICOM to XML.");
                    Preferences.debug("  Empty string written for:");
                    Preferences.debug(" (" + tagKey + ")\n");
                    destInfo.getCurrentPSet().getCurrentParameter().setValue("");
                }
            }
        }

        return true;
    }

    /**
     * Returns a list of tags that are a subset of the DICOM dictionary. This method chooses the tags to be saved by
     * presenting a dialog for the user to select a list of tags or by reading the <q>dicomsave.dictionary</q> file.
     *
     * @param   sourceInfo  Source of DICOM information.
     *
     * @return  The Hashtable filled as an XML
     */
    protected Hashtable getDicomSaveList(FileInfoDicom sourceInfo) {
        Hashtable tags2save;

        if (quiet) { // don't bother asking user when running a macro.
            tags2save = DICOMDictionaryBuilder.getSubsetDicomTagTable();

            if (tags2save == null) {
                System.out.println("tags2save is null");
            }
        } else {
            JDialogDicom2XMLSelection jdl = new JDialogDicom2XMLSelection();
            jdl.setVisible(true);

            if (jdl.wasOkay()) {
                tags2save = jdl.getSaveTable(); // hack!!
            } else {
                return null;
            }
        }

        if (tags2save == null) {
            tags2save = new Hashtable();
        }

        Hashtable fullTagsList = sourceInfo.getTagsList();

        // place DICOM tags (with values) into the save-tags list.
        // Remove any tags from the list that do not have values:
        Enumeration e = tags2save.keys();

        while (e.hasMoreElements()) {
            FileDicomKey tagKey = (FileDicomKey) e.nextElement();

            if (fullTagsList.containsKey(tagKey) &&
                    (((FileDicomTag) fullTagsList.get(tagKey)).getValue(false) != null) &&
                    (((FileDicomTag) fullTagsList.get(tagKey)).getNumberOfValues() > 0)) {
                tags2save.put(tagKey, fullTagsList.get(tagKey));
            } else {
                tags2save.remove(tagKey);
            }
        }

        return tags2save;
    }

    /**
     * Provides a method of conversion from <code>FileInfoLSM</CODE> to <CODE>FileInfoImageXML</CODE>, by filling the
     * <CODE>FileInfoImageXML</CODE> with sets of chosen image information.
     *
     * @param  sourceInfo  the LSM-formatted Source information
     * @param  destInfo    the XML-format file information that is the output.
     */
    protected void LSMDataConversion(FileInfoLSM sourceInfo, FileInfoImageXML destInfo) {
        int firstSliceAfterBleach;
        int bleachedROIShape;
        String shapeString = null;
        double[] knotX = null;
        double[] timeStamp = null;
        int q;

        firstSliceAfterBleach = sourceInfo.getFirstSliceAfterBleach();

        if (firstSliceAfterBleach >= 0) {
            destInfo.createPSet("firstSliceAfterBleach");
            destInfo.getCurrentPSet().addParameter("firstSliceAfterBleach");
            destInfo.getCurrentPSet().getCurrentParameter().setValueType("int");
            destInfo.getCurrentPSet().getCurrentParameter().setDescription("Zero based");
            destInfo.getCurrentPSet().getCurrentParameter().setValue(String.valueOf(firstSliceAfterBleach));
        }

        bleachedROIShape = sourceInfo.getBleachedROIShape();

        if (bleachedROIShape >= 0) {
            destInfo.createPSet("bleachedROIShape");
            destInfo.getCurrentPSet().addParameter("bleachedROIShape");
            destInfo.getCurrentPSet().getCurrentParameter().setValueType("int");

            switch (bleachedROIShape) {

                case 18:
                    shapeString = "Rectangle";
                    break;

                case 19:
                    shapeString = "Ellipse";
                    break;

                case 20:
                    shapeString = "Closed polyline";
                    break;

                case 22:
                    shapeString = "Closed bezier";
                    break;

                case 24:
                    shapeString = "Circle";
                    break;
            }

            if (shapeString != null) {
                destInfo.getCurrentPSet().getCurrentParameter().setDescription(shapeString);
            }

            destInfo.getCurrentPSet().getCurrentParameter().setValue(String.valueOf(bleachedROIShape));
        }

        knotX = sourceInfo.getKnotX();

        if (knotX != null) {
            destInfo.createPSet("knotX");

            for (q = 0; q < knotX.length; q++) {
                destInfo.getCurrentPSet().addParameter("knotX[" + q + "]");
                destInfo.getCurrentPSet().getCurrentParameter().setValueType("double");
                destInfo.getCurrentPSet().getCurrentParameter().setValue(String.valueOf(knotX[q]));
            }
        }

        timeStamp = sourceInfo.getTimeStamp();

        if (timeStamp != null) {
            destInfo.createPSet("timeStamp");

            for (q = 0; q < timeStamp.length; q++) {
                destInfo.getCurrentPSet().addParameter("timeStamp[" + q + "]");
                destInfo.getCurrentPSet().getCurrentParameter().setValueType("double");
                destInfo.getCurrentPSet().getCurrentParameter().setValue(String.valueOf(timeStamp[q]));
            }
        }

    }

    /**
     * Creates the progress bar and links (if not null) the progress bar to a FileBase for reading/writing
     * if the progress bar should not be updated within the FileBase's readImage/writeImage,
     * pass in null to the fBase and update within FileIO's read[ImageType] or write[ImageType] methods
     * @param fBase the FileBase that will add the Progress Bar as a listener (for fireProgressStateChanged())
     * @param fName the filename (will be displayed in the title and part of the message)
     * @param message this message should FILE_READ, FILE_OPEN
     */
    private void createProgressBar(FileBase fBase, String fName, String message) {
    	//System.err.println("title is: " + fName + ", message is: " + message);
    	progressBar = new ViewJProgressBar(fName, message + fName + " ...", 0, 100, true);
        progressBar.setVisible(ViewUserInterface.getReference().isAppFrameVisible() && !quiet);
        //System.err.println("quiet: " + quiet + " pbar vis: " + progressBar.isVisible());
        progressBar.progressStateChanged(new ProgressChangeEvent(this, 0, null, null));
        if (fBase != null) {
        	fBase.addProgressChangeListener(progressBar);
        }	
	}

    
    /**
     * DOCUMENT ME!
     *
     * @param  modelImageSrc     DOCUMENT ME!
     * @param  modelImageResult  DOCUMENT ME!
     * @param  sliceNum          DOCUMENT ME!
     */
    private static void copyResolutions(ModelImage modelImageSrc, ModelImage modelImageResult, int sliceNum) {
        float[] resolutions = new float[3];
        resolutions[0] = modelImageSrc.getFileInfo(0).getResolutions()[0];
        resolutions[1] = modelImageSrc.getFileInfo(0).getResolutions()[1];
        resolutions[2] = 1.0f;

        modelImageResult.getFileInfo(sliceNum).setResolutions(resolutions);
    }

    /**
     * The purpose of this method is to take a buffer of float and resample the values to proper UBYTE values.
     *
     * @param   buffer  float[] - the buffer to be sampled
     * @param   min     - the image's minimum pixel value
     * @param   max     - the image's maximum pixel value
     *
     * @return  float[] - the resampled buffer with min and max values in the range 0 - 255
     */
    private static float[] resample255(float[] buffer, float min, float max) {
        float precalculatedDenominator = max - min; // precalculated (max - min) for speed

        for (int i = 0; i < buffer.length; i++) {
            buffer[i] = ((buffer[i] - min) / precalculatedDenominator) * 255;
        }

        return buffer;
    }

    /**
     * Helper method for finding biggest (absolute value) of three numbers.
     *
     * @param   zero  First value
     * @param   one   Second value
     * @param   two   Third value
     *
     * @return  Index of argument that is the biggest. That is, if <code>zero</code> is the largest of the three
     *          numbers, the value 0 is returned; alternatively, if the second argument is the largest of the three
     *          numbers, the value 1 is returned; and so on.
     */
    private int absBiggest(double zero, double one, double two) {

        if (Math.abs(zero) > Math.abs(one)) {

            if (Math.abs(zero) > Math.abs(two)) {
                return 0;
            } else {
                return 2;
            }
        } else {

            if (Math.abs(one) > Math.abs(two)) {
                return 1;
            } else {
                return 2;
            }
        }
    }

    /**
     * Calls GUI dialogs based on what type of image this is and the number of dimensions.
     *
     * @param   extents  Extents of the image, used to determine what type of dialog to call and initialize variables.
     * @param   isTiff   Flag indicating if this is a TIFF file; TIFF files require more options.
     * @param   options  Options to get from the dialog and save.
     *
     * @return  DOCUMENT ME!
     */
    private boolean callDialog(int[] extents, boolean isTiff, FileWriteOptions options) {
        JDialogSaveSlices dialogSave = null;

        if ((extents.length == 2) && isTiff && options.isPackBitEnabled()) {
            int response = JOptionPane.showConfirmDialog(UI.getMainFrame(), "Save with pack bit compression?",
                                                         "Compression", JOptionPane.YES_NO_OPTION,
                                                         JOptionPane.QUESTION_MESSAGE);

            if (response == JOptionPane.YES_OPTION) {
                options.setWritePackBit(true);
                options.setDefault(false);
            } else {
                options.setWritePackBit(false);
            }
        } else if (extents.length > 2) {

            if (extents.length == 3) {
                dialogSave = new JDialogSaveSlices(UI.getMainFrame(), 1, extents[2], options);
            } else if (extents.length == 4) {
                dialogSave = new JDialogSaveSlices(UI.getMainFrame(), 1, extents[2], 1, extents[3], options);
            }

            if (dialogSave.isCancelled()) {
                return false;
            }

            options = dialogSave.getWriteOptions();

            if (extents.length == 3) {

                if (!((options.getBeginSlice() == 0) && (options.getEndSlice() == (extents[2] - 1)))) {
                    options.setDefault(false);
                }

                if (options.isMultiFile()) {
                    options.setDefault(false);
                }
            } else if (extents.length == 4) {

                if ((options.getFileType() == FileBase.TIFF) || (options.getFileType() == FileBase.MINC)) {

                    if (!((options.getBeginSlice() == 0) && (options.getEndSlice() == (extents[2] - 1)) &&
                              (options.getTimeSlice() == 0))) {
                        options.setDefault(false);
                    }

                    if (options.isMultiFile()) {
                        options.setDefault(false);
                    }
                } else {

                    // if these are the defaults, don't append them, it's not necessary.  otherwise will append all 4.
                    if (!((options.getBeginSlice() == 0) && (options.getEndSlice() == (extents[2] - 1)) &&
                              (options.getBeginTime() == 0) && (options.getEndTime() == (extents[3] - 1)))) {
                        options.setDefault(false);
                    }
                }
            }
        }

        return true;
    }

    /**
     * Reads an AFNI file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   loadB     true if loading imageB
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readAfni(String fileName, String fileDir, boolean loadB) {
        ModelImage image = null;
        FileAfni imageFile;
        boolean doRead = true;

        try {
            imageFile = new FileAfni(UI, fileName, fileDir, loadB, doRead);
            image = imageFile.readImage();
            // LUT      = imageFile.getModelLUT();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads an analyze file by calling the read method of the file. Also checks if it's a Cheshire, Interfile, or NIFTI
     * and if so, calls that method instead. This method contains special code to not display the progress bar should
     * the image be <q>splash.img</q>.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readAnalyze(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileAnalyze imageFile;

        String headerFile = FileInterfile.isInterfile(fileName, fileDir);

        if (FileCheshire.isCheshire(fileName, fileDir)) {
            image = readCheshire(fileName, fileDir);
        } else if (headerFile != null) {
            image = readInterfile(headerFile, fileDir, one);
        } else if (FileNIFTI.isNIFTI(fileName, fileDir)) {
            image = readNIFTI(fileName, fileDir, one);
        } else {

            // most likely an Analyze file
            try {
                imageFile = new FileAnalyze(fileName, fileDir);
                createProgressBar(imageFile, fileName, FILE_READ);
                image = imageFile.readImage(one);
            } catch (IOException error) {

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                return null;
            } catch (OutOfMemoryError error) {

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                return null;
            }
        }

        return image;
    }

    /**
     * Reads a multi Analyze file. Gets a list of the images from the file directory and reads them each in.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readAnalyzeMulti(String fileName, String fileDir) {
        ModelImage image = null;
        FileAnalyze imageFile;
        FileInfoBase fileInfo;
        
        int length = 0;
        float[] buffer;
        String[] fileList;
        int[] extents;
        float[] resolutions;
        int nImages;

        // of proper extents (in case there is a file with the consistent filename but
        // inconsistent extents.) we do assume the 1st header is correct


        int i = 0;

        try {
            fileList = getFileList(fileDir, fileName);

            for (int m = 0; m < fileList.length; m++) {

                if (fileList[m] != null) {

                    // System.out.println(" Name = " + fileList[m]);
                    i++;
                }
            }
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        nImages = i; // total number of suspected files to import into an image


        if (nImages == 1) {
            return readAnalyze(fileName, fileDir, false);
        }

        createProgressBar(null, fileName, FILE_READ);
        
        // if one of the images has the wrong extents, the following must be changed.
        // (ei., too many images!)
        // for simplicity of setup, read in the first file hdr
        imageFile = new FileAnalyze(fileList[0], fileDir);

        try {

            if (!imageFile.readHeader(fileList[0], fileDir)) {
                throw (new IOException(" Analyze header file error"));
            }
        } catch (IOException ioe) {

            if (!quiet) {
                MipavUtil.displayError("Error reading header file.");
            }
        }

        fileInfo = ((FileAnalyze) imageFile).getFileInfo();
        extents = fileInfo.getExtents();
        resolutions = fileInfo.getResolutions();

        if (extents.length == 2) {
            length = extents[0] * extents[1];
        } else if (extents.length == 3) {
            length = extents[0] * extents[1] * extents[2];
        } else if (extents.length == 4) {
            length = extents[0] * extents[1] * extents[2] * extents[3];
        }

        buffer = new float[length];

        int[] imgExtents = new int[extents.length + 1];
        float[] imgResolutions = new float[resolutions.length + 1]; // should be same size as extents.

        // copy proper values into img extents, assuming that the 1st (numerically indexed) images
        // is correct to begin with
        for (i = 0; i < extents.length; i++) {
            imgExtents[i] = extents[i];
            imgResolutions[i] = resolutions[i];
            // set the number of slices in the image later.
        }

        imgExtents[i] = nImages; // may not be right, but we'll find out after we go through all images.
        imgResolutions[i] = 1; // resolution in the created axis is not physically defined; is generated.

        image = new ModelImage(fileInfo.getDataType(), imgExtents, fileInfo.getFileName(), UI);

        int imageCount = 0;

        // loop through image and store data in image model
        for (i = 0; i < nImages; i++) {

            try {
                //progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                imageFile = new FileAnalyze(fileList[i], fileDir);

                if (!((FileAnalyze) imageFile).readHeader(fileList[i], fileDir)) {
                    throw (new IOException(" Analyze header file error"));
                }

                // chk the extents of the image to verify it is consistent
                // (this doesn't ensure there won't be null exceptions@)
                fileInfo = ((FileAnalyze) imageFile).getFileInfo();

                if (extents.length != fileInfo.getExtents().length) {
                    if (!quiet) {
                        MipavUtil.displayError("Inconsistent image file found.  This File will be skipped.");
                    }

                    continue;
                } else { // the prototype image and the read-in image are of the same dimension....

                    switch (extents.length) { // check that they extend as far in all dimensions:

                        case 2:
                            if ((extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1])) {
                                if (!quiet) {
                                    MipavUtil.displayError("Inconsistent image file found.  This File will be skipped.");
                                }

                                continue;
                            }

                            break;

                        case 3:
                            if ((extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1]) ||
                                    (extents[2] != fileInfo.getExtents()[2])) {
                                if (!quiet) {
                                    MipavUtil.displayError("Inconsistent image file found.  This File will be skipped.");
                                }

                                continue;
                            }

                            break;

                        case 4:
                            if ((extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1]) ||
                                    (extents[2] != fileInfo.getExtents()[2]) ||
                                    (extents[3] != fileInfo.getExtents()[3])) {

                                if (!quiet) {
                                    MipavUtil.displayError("Inconsistent image file found.  This File will be skipped.");
                                }

                                continue;
                            }

                            break;

                        default:
                            break;
                    }
                }

                fileInfo.setExtents(imgExtents); // set image extents to proper value!
                fileInfo.setResolutions(imgResolutions);
                if (nImages > 1) {
                    fileInfo.setMultiFile(true);
                }
                ((FileAnalyze) imageFile).readImage(buffer);
                image.importData(imageCount * length, buffer, false);
                image.setFileInfo(fileInfo, imageCount);
                imageCount++; // image was okay, so count it.(can't do it before b/c of offset)

            } catch (IOException error) {
                

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (ArrayIndexOutOfBoundsException error) {
                

                if (!quiet) {
                    MipavUtil.displayError("Unable to read images: the image\nnumber in the file " +
                                           fileInfo.getFileName() + " is corrupted.");
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (OutOfMemoryError error) {
                

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        }
        // i goes 1 too far anyway, but if  we skipped files, be sure to account for it,
        // because our basic model was that all prperly named files were good analyze images.
        // only we found one or more didn't fit.  We must now take that into account.
        // ie., we read in imageCount # of images, we expected nImages.

        if (imageCount < nImages) {
            FileInfoBase[] fileInfoArr = image.getFileInfo();

            imgExtents[image.getNDims() - 1] = imageCount; // last dimension available should be the num images read

            int sliceSize = buffer.length;

            buffer = new float[sliceSize * imageCount];

            try {
                image.exportData(0, buffer.length, buffer); // copy all buffer
                image.reallocate(fileInfo.getDataType(), imgExtents);
                image.importData(0, buffer, true); // remake the model image with the right number of slices.

                for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                    fileInfoArr[i].setExtents(imgExtents); // update extents
                    image.setFileInfo(fileInfoArr[i], i); // copying...
                }
            } catch (IOException ioe) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO reports: " + ioe.getMessage());
                }

                return null;
            }

            FileInfoBase [] fileInfoArrCopy = new FileInfoBase[imgExtents[imgExtents.length - 1]];
            
            for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                fileInfoArr[i].setExtents(imgExtents); // update extents
                fileInfoArrCopy[i] = fileInfoArr[i];
            }
            image.setFileInfo(fileInfoArrCopy);
        }

        

        return image;

    }

    /**
     * Reads an AVI file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     * @param   readQT    Indicates that a QuickTime movie file is being read. <code>true</code> if this file represents
     *                    QuickTime.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readAvi(String fileName, String fileDir, boolean one, boolean readQT) {
        FileAvi imageFile;
        ModelImage image = null;

        try {
            imageFile = new FileAvi(UI, fileName, fileDir);
            imageFile.setReadQT(readQT);
           // imageFile.setProgressBar(pInterface);
            
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);

            // LUT used in compressed RLE8 files
            LUT = imageFile.getModelLUT();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads a BioRad PIC file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readBioRad(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileBioRad imageFile;

        try {
            imageFile = new FileBioRad(UI, fileName, fileDir);
            image = imageFile.readImage(one);
            // LUT      = imageFile.getModelLUT();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads a BRUKER file by first reading in the d3proc header file, second the reco header file, third the acqp file,
     * and finally the 2dseq binary file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readBRUKER(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileBRUKER imageFile = null;
        FileInfoBase myFileInfo;
        File directoryFile;
        String parentDirectoryName;

        try {
            fileName = "d3proc";
            imageFile = new FileBRUKER(UI, fileName, fileDir); // read in files
            imageFile.readd3proc();
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("d3proc FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("d3proc Out of memory: " + error);
            }

            return null;
        }

        ((FileBRUKER) imageFile).setFileName("reco");

        try {
            imageFile.readreco();
        } catch (IOException error) {
            myFileInfo = imageFile.getFileInfo();
            myFileInfo.setEndianess(FileBase.BIG_ENDIAN);
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("reco out of memory: " + error);
            }

            return null;
        }

        ((FileBRUKER) imageFile).setFileName("acqp");
        directoryFile = new File(fileDir);

        File tmpFile = new File(fileDir + File.separator + "acqp");

        if (!tmpFile.exists()) {

            // go up 2 parent directories
            parentDirectoryName = directoryFile.getParent();
            directoryFile = new File(parentDirectoryName);
            ((FileBRUKER) imageFile).setFileDir(directoryFile.getParent() + File.separator);
        } else {
            ((FileBRUKER) imageFile).setFileDir(directoryFile + File.separator);
        }

        try {
            imageFile.readacqp();
        } catch (IOException error) {
            Preferences.debug("IOExceoption in FileIO.readBRUKER\n", 5);

            if (!quiet) {
                MipavUtil.displayError("IOException in FileIO.readBRUKER: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("acqp out of memory: " + error);
            }

            return null;
        }

        ((FileBRUKER) imageFile).setFileName("2dseq");
        ((FileBRUKER) imageFile).setFileDir(fileDir);

        try {
            image = imageFile.readImage(one);
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Reads a Cheshire file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readCheshire(String fileName, String fileDir) {
        ModelImage image = null;
        FileCheshire imageFile;

        try {
            imageFile = new FileCheshire(UI, fileName, fileDir, true);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Reads a COR file by first reading the header file then reading in each separate slice file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readCOR(String fileName, String fileDir) {
        float[] resols;
        int[] extents; // extent of image  (!def!)
        int length = 0;
        int i;

        // float[] buffer;
        FileInfoBase myFileInfo;
        String[] fileList = null;
        FileCOR imageFile = null;
        ModelImage image = null;
        
        String origName = null;
        boolean tryAgain = false;
        int nImages;

        // First try .info, then try .info~, then try original name
        fileList = getCORFileList(fileDir, fileName); // get series of files in the chosen dir
        nImages = fileList.length;

        try {
            origName = fileName;
            fileName = trimCOR(fileName) + ".info"; // allow user to click on any file in set
            imageFile = new FileCOR(UI, fileName, fileDir); // read in files
            imageFile.readInfoImage();
        } catch (IOException error) {
            tryAgain = true;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            return null;
        }

        if (tryAgain) {
            tryAgain = false;

            try {
                fileName = trimCOR(origName) + ".info~";
                imageFile = new FileCOR(UI, fileName, fileDir); // read in files
                imageFile.readInfoImage();
            } catch (IOException error) {
                tryAgain = true;
            } catch (OutOfMemoryError error) {

                if (!quiet) {
                    MipavUtil.displayError("Out of memory: " + error);
                }

                return null;
            }
        } // if (tryAgain)

        if (tryAgain) {

            try {
                fileName = origName;
                imageFile = new FileCOR(UI, fileName, fileDir); // read in files
                imageFile.readInfoImage();
            } catch (IOException error) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                return null;
            } catch (OutOfMemoryError error) {

                if (!quiet) {
                    MipavUtil.displayError("Out of memory: " + error);
                }

                return null;
            }
        } // if (tryAgain)

        myFileInfo = imageFile.getFileInfo();

        length = myFileInfo.getExtents()[0] * myFileInfo.getExtents()[1];

        try {
            resols = new float[5];

            if (nImages > 1) {
                extents = new int[3];
                extents[2] = nImages;
            } else {
                extents = new int[2];
            }

            extents[0] = myFileInfo.getExtents()[0]; // copy out current [0,1] coords
            extents[1] = myFileInfo.getExtents()[1]; // so all 3 ([0,1,2]) may be added

            image = new ModelImage(myFileInfo.getDataType(), extents, myFileInfo.getFileName(), UI);

            // Progress bar shows what % of images have been read.
            createProgressBar(null, fileName, FILE_READ);
            
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            return null;
        }

        image.setFileInfo(myFileInfo, 0);

        try {
            imageFile = new FileCOR(UI, fileList[0], fileDir);
        } catch (IOException error) {
            

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        }


        // loop through image and store data in image model
        for (i = 0; i < nImages; i++) {

            try {
                //progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                ((FileCOR) imageFile).setFileName(fileList[i]);
                ((FileCOR) imageFile).readImage(length);
                image.setFileInfo(myFileInfo, i);

                if (image.isColorImage()) {
                    image.importData(i * 4 * length, ((FileCOR) imageFile).getImageBuffer(), false);
                } else {
                    image.importData(i * length, ((FileCOR) imageFile).getImageBuffer(), false);
                }
            } catch (IOException error) {
                

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (ArrayIndexOutOfBoundsException error) {
                

                if (!quiet) {
                    MipavUtil.displayError("Unable to read images: the image\n" + "number in the file " +
                                           myFileInfo.getFileName() + " is corrupted.");
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (OutOfMemoryError error) {
                

                if (!quiet) {
                    MipavUtil.displayError("Out of memory: " + error);
                }

                if (image != null) {

                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        }

        

        return image;

    }

    /**
     * Reads a Dm3 file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readDM3(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileDM3 imageFile;

        try {
            imageFile = new FileDM3(UI, fileName, fileDir);
            image = imageFile.readImage(one);
            // LUT      = imageFile.getModelLUT();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads a FITS file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readFits(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileFits imageFile;

        try {
            imageFile = new FileFits(UI, fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
            // LUT      = imageFile.getModelLUT();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads in a GE Signa 4x type file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readGESigna4X(String fileName, String fileDir) {

        ModelImage image = null;
        FileGESigna4X imageFile;
        FileInfoBase myFileInfo0 = null;
        FileInfoBase myFileInfo;
        String[] fileList;
        float[] buffer;
        int[] extents;
        int length = 0;
        int i;
        int width, height;
        int nImages;
        int imageSize;
        int[] orient = { 0, 0, 0 };
        float slice0Pos = 0.0f;
        float slice1Pos = 0.0f;

        try {

            fileList = getFileList(fileDir, fileName);
            imageFile = new FileGESigna4X(fileName, fileDir);

            imageFile.setFileName(fileList[0]);
            imageSize = imageFile.readImageFileData();

            if (imageSize == -1) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: Compression not supported for Signa 4X");
                }

                return null;
            }

            if (imageSize == -2) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: Not a Signa 4X file.");
                }

                return null;
            }

            if (imageSize == 0) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: Image length.");
                }

                return null;
            }
                        
            width = imageFile.getWidth();
            height = imageFile.getHeight();
            length = width * height;
            buffer = new float[length];

            if (fileList.length == 1) {
                extents = new int[2];
                extents[0] = width;
                extents[1] = height;
            } else {
                extents = new int[3];
                extents[0] = width;
                extents[1] = height;
                extents[2] = fileList.length;
            }

            image = new ModelImage(ModelImage.USHORT, extents, "GE", UI);
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }
           
            return null;
        }


        nImages = fileList.length;

        createProgressBar(null, fileName, FILE_READ);
        
        // loop through files, place them in image array
        for (i = 0; i < nImages; i++) {

            try {
              //  progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));

                if (fileList[i] != null) {
                    imageFile.setFileName(fileList[i]);
                    imageFile.readImageFileData();
                    imageFile.readImage(buffer);

                    myFileInfo = imageFile.getFileInfo(); // Needed to set index
                    if (i == 0) {
                        myFileInfo0 = imageFile.getFileInfo();
                        orient = myFileInfo.getAxisOrientation();

                        switch (myFileInfo.getImageOrientation()) {

                            case FileInfoBase.AXIAL:
                                slice0Pos = imageFile.getImgTLHC_S();
                                break;

                            case FileInfoBase.CORONAL:
                                slice0Pos = imageFile.getImgTLHC_A();
                                break;

                            case FileInfoBase.SAGITTAL:
                                slice0Pos = imageFile.getImgTLHC_R();
                                break;
                        }
                    } // if (i == 0)
                    else if (i == 1) {
                        orient = myFileInfo.getAxisOrientation();

                        switch (myFileInfo.getImageOrientation()) {

                            case FileInfoBase.AXIAL:
                                slice1Pos = imageFile.getImgTLHC_S();
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_I2S_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_S2I_TYPE;
                                }

                                break;

                            case FileInfoBase.CORONAL:
                                slice1Pos = imageFile.getImgTLHC_A();
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_P2A_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_A2P_TYPE;
                                }

                                break;

                            case FileInfoBase.SAGITTAL:
                                slice1Pos = imageFile.getImgTLHC_R();
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_L2R_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_R2L_TYPE;
                                }

                                break;
                        } // switch (myFileInfo.getImageOrientation())


                        if (myFileInfo0 != null) {
                            image.setFileInfo(myFileInfo0, 0);
                            myFileInfo0.setAxisOrientation(orient);
                        }
                    } // else if (i == 1)

                    if (i != 0) {
                        myFileInfo.setAxisOrientation(orient);
                    }

                    myFileInfo.setExtents(extents);
                    myFileInfo.setOrigin(((FileInfoGESigna4X) (myFileInfo)).getOriginAtSlice(imageFile.getImageNumber() -
                                                                                             1));

                    image.setFileInfo(myFileInfo, imageFile.getImageNumber() - 1);
                    image.importData((imageFile.getImageNumber() - 1) * length, buffer, false);
                } // if (fileList[i] != null)
            } // try
            catch (IOException error) {
                

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (OutOfMemoryError error) {
                

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                if (image != null) {

                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        } // for (i = 0; i < nImages; i++)

        image.setImageName(imageFile.getFileInfo().getImageNameFromInfo());

        

        return image;
    }

    /**
     * Reads in a GE Genesis 5x type file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readGEGenesis5X(String fileName, String fileDir) {

        ModelImage image = null;
        FileGESigna5X imageFile;
        FileInfoBase myFileInfo;
        FileInfoBase myFileInfo0 = null;
        String[] fileList;
        float[] buffer;
        int[] extents;
        int length = 0;
        int i;
        int imageSize = 0;
        int width, height;
        int nImages;
        int[] orient = { 0, 0, 0 };
        float slice0Pos = 0.0f;
        float slice1Pos = 0.0f;

        try {

            fileList = getFileList(fileDir, fileName);
            imageFile = new FileGESigna5X(fileName, fileDir);

            imageFile.setFileName(fileList[0]);
            imageSize = imageFile.readImageFileData();

            if (imageSize == -1) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: Compression not supported for Signa 5X");
                }

                return null;
            }

            if (imageSize == -2) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: Not a Signa 5X file.");
                }

                return null;
            }

            if (imageSize == 0) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: Image length.");
                }

                return null;
            }

            createProgressBar(null, fileName, FILE_READ);
            
            width = imageFile.getWidth();
            height = imageFile.getHeight();
            length = width * height;
            buffer = new float[length];

            if ((fileList.length == 1) || (imageFile.getStartAdjust() > 0)) {
                extents = new int[2];
                extents[0] = width;
                extents[1] = height;
            } else {
                extents = new int[3];
                extents[0] = width;
                extents[1] = height;
                extents[2] = fileList.length;
            }

            image = new ModelImage(ModelImage.USHORT, extents, "GE", UI);
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            if (progressBar != null) {
                
            }

            return null;
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            if (progressBar != null) {
                
            }

            return null;
        }

        if (imageFile.getStartAdjust() > 0) {
            nImages = 1;
        } else {
            nImages = fileList.length;
        }

        // loop through files, place them in image array
        for (i = 0; i < nImages; i++) {

            try {
                progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));

                if (fileList[i] != null) {
                    imageFile.setFileName(fileList[i]);
                    imageFile.readImageFileData();
                    imageFile.readImage(buffer);

                    myFileInfo = imageFile.getFileInfo(); // Needed to set index

                    if (i == 0) {
                        myFileInfo0 = imageFile.getFileInfo();
                        orient = myFileInfo.getAxisOrientation();

                        switch (myFileInfo.getImageOrientation()) {

                            case FileInfoBase.AXIAL:
                                slice0Pos = ((FileInfoGESigna5X) myFileInfo).imgTLHC_S;
                                break;

                            case FileInfoBase.CORONAL:
                                slice0Pos = ((FileInfoGESigna5X) myFileInfo).imgTLHC_A;
                                break;

                            case FileInfoBase.SAGITTAL:
                                slice0Pos = ((FileInfoGESigna5X) myFileInfo).imgTLHC_R;
                                break;
                        }
                    } // if (i == 0)
                    else if (i == 1) {
                        orient = myFileInfo.getAxisOrientation();

                        switch (myFileInfo.getImageOrientation()) {

                            case FileInfoBase.AXIAL:
                                slice1Pos = ((FileInfoGESigna5X) myFileInfo).imgTLHC_S;
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_I2S_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_S2I_TYPE;
                                }

                                break;

                            case FileInfoBase.CORONAL:
                                slice1Pos = ((FileInfoGESigna5X) myFileInfo).imgTLHC_A;
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_P2A_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_A2P_TYPE;
                                }

                                break;

                            case FileInfoBase.SAGITTAL:
                                slice1Pos = ((FileInfoGESigna5X) myFileInfo).imgTLHC_R;
                                if (slice1Pos > slice0Pos) {
                                    orient[2] = FileInfoBase.ORI_L2R_TYPE;
                                } else {
                                    orient[2] = FileInfoBase.ORI_R2L_TYPE;
                                }

                                break;
                        } // switch (myFileInfo.getImageOrientation())

                        if (myFileInfo0 != null) {
                            image.setFileInfo(myFileInfo0, 0);
                            myFileInfo0.setAxisOrientation(orient);
                        }
                    } // else if (i == 1)

                    if (i != 0) {
                        myFileInfo.setAxisOrientation(orient);
                    }

                    myFileInfo.setExtents(extents);
                    myFileInfo.setOrigin(((FileInfoGESigna5X) (myFileInfo)).getOriginAtSlice(imageFile.getImageNumber() -
                                                                                             1));
                    image.setFileInfo(myFileInfo, imageFile.getImageNumber() - 1);
                    image.importData((imageFile.getImageNumber() - 1) * length, buffer, false);
                } // if (fileList[i] != null)
            } // try
            catch (IOException error) {
                

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (OutOfMemoryError error) {
                

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                if (image != null) {

                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        } // for (i = 0; i < nImages; i++)

        image.setImageName(imageFile.getFileInfo().getImageNameFromInfo());

        

        return image;
    }

    /**
     * Reads an ICS file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readICS(String fileName, String fileDir) {
        ModelImage image = null;
        FileICS imageFile;

        try {
            imageFile = new FileICS(UI, fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
            // LUT      = imageFile.getModelLUT();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads an Interfile file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readInterfile(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileInterfile imageFile;

        try {
            imageFile = new FileInterfile(UI, fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
            // LUT      = imageFile.getModelLUT();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads a JIMI file by calling the read method of the file.
     *
     * @param   fileName   Name of the image file to read.
     * @param   fileDir    Directory of the image file to read.
     * @param   multifile  Indication whether this file is be read alone, or if the reader is to read all matching
     *                     filenames as a part of the dataset. <code>true</code> if want to read all matching files to
     *                     form an image into a 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readJimi(String fileName, String fileDir, boolean multifile) {
        ModelImage modelImage = null;
        Image image = null;
        int imageWidth = 0;
        int imageHeight = 0;
        int[] extents;
        int[] buffer = null;
        String[] fileList;


        if (multifile) {
            fileList = getFileList(fileDir, fileName);
        } else {
            fileList = new String[] { fileName };
        }

        MediaTracker mediaTracker = new MediaTracker(UI.getMainFrame());
        extents = new int[] { 0, 0, fileList.length };

        for (int j = 0; j < fileList.length; j++) {

            try {
                image = Jimi.getImage(fileDir + fileList[j]); // JIMI uses file suffix to correctly load image

                mediaTracker.addImage(image, 0);

                boolean loaded = mediaTracker.waitForAll(20000);

                if (!loaded || (image == null)) {

                    try {
                        image = (Image) ImageIO.read(new File(fileDir + fileName)); // if JIMI fails, try this
                    } catch (IOException ioe) {
                        // intentionally empty
                    } finally {

                        if (image == null) {

                            if (!quiet) {
                                MipavUtil.displayError("Unable to load image. Image format may not be supported.");
                            }

                            return null;
                        }
                    }
                }
            } catch (InterruptedException e) {

                if (image == null) {
                    return null;
                }

                Preferences.debug("FileIO.JIMI : " + e + "\n");
            }

            mediaTracker.removeImage(image);

            imageWidth = image.getWidth(null);
            imageHeight = image.getHeight(null);

            if ((imageWidth <= 0) || (imageHeight <= 0)) {
                return null;
            }

            // More to be added if animated gifs ...  are required
            // LUT      = img.getProperty("", UI.getMainFrame());
            // This is for RGB images
            int[] pixels = new int[imageWidth * imageHeight];
            PixelGrabber pg = new PixelGrabber(image, 0, 0, imageWidth, imageHeight, pixels, 0, imageWidth);

            try {
                pg.grabPixels();
            } catch (InterruptedException e) {
                Preferences.debug("JIMI: Interrupted waiting for pixels!" + "\n");

                return null;
            }

            if ((pg.getStatus() & ImageObserver.ABORT) != 0) {
                Preferences.debug("JIMI: Image fetch aborted or errored" + "\n");

                return null;
            }

            if ((extents[0] != 0) && ((extents[0] != imageWidth) || (extents[1] != imageHeight))) {
                MipavUtil.displayError("Images in directory do not have the same X-Y dimensions.");

                return null;
            }

            extents[0] = imageWidth;
            extents[1] = imageHeight;

            if (buffer == null) {
                buffer = new int[4 * extents[0] * extents[1] * extents[2]];
            }

            int a, r, g, b, pixel;

            int i = j * 4 * extents[0] * extents[1];

            for (int y = 0; y < imageHeight; y++) {

                for (int x = 0; x < imageWidth; x++) {
                    pixel = pixels[(y * imageWidth) + x];
                    a = (pixel >> 24) & 0xff;
                    r = (pixel >> 16) & 0xff;
                    g = (pixel >> 8) & 0xff;
                    b = (pixel) & 0xff;

                    buffer[i] = a;
                    buffer[i + 1] = r;
                    buffer[i + 2] = g;
                    buffer[i + 3] = b;
                    i += 4;
                }
            }

        }

        if (extents[2] == 1) {
            int[] tmp = new int[] { extents[0], extents[1] };

            extents = new int[] { tmp[0], tmp[1] };
        }

        modelImage = new ModelImage(ModelStorageBase.ARGB, extents, fileName, UI);

        try {
            modelImage.importData(0, buffer, true);
        } catch (IOException e) {
            Preferences.debug("FileIO.JIMI : " + e + "\n");
        }

        // get the fileInfo and make sure some fields are set (like the
        // fileDir and the fileFormat
        FileInfoBase[] fileInfo = modelImage.getFileInfo();

        for (int j = 0; j < fileInfo.length; j++) {
            fileInfo[j].setFileDirectory(fileDir);
            fileInfo[j].setFileFormat(FileBase.JIMI);
            modelImage.setFileInfo(fileInfo[j], j);
        }

        if (image != null) {
            image.flush();
        }

        return modelImage;
    }

    /**
     * Reads a LSM file by calling the read method of the file.
     *
     * @param   fileName       Name of the image file to read.
     * @param   fileDir        Directory of the image file to read.
     * @param   secondAddress  DOCUMENT ME!
     * @param   one            Indicates that only the named file should be read, as opposed to reading the matching
     *                         files in the directory, as defined by the filetype. <code>true</code> if only want to
     *                         read one image from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readLSM(String fileName, String fileDir, int secondAddress, boolean one) {
        ModelImage image = null;
        FileLSM imageFile;

        try {
            imageFile = new FileLSM(UI, fileName, fileDir, secondAddress);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();
            secondImage = imageFile.getSecondImage();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads a multi LSM file by first reading the headers then reading in each separate file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readLSMMulti(String fileName, String fileDir) {
        float[] resols;
        int[] singleExtents;
        int[] singleUnitsOfMeasure;
        int[] extents; // extent of image  (!def!)
        int[] unitsOfMeasure;
        int length = 0;
        int i, j;
        FileInfoBase myFileInfo;
        FileInfoBase[] fileInfo;
        String[] fileList;
        FileLSM imageFile;
        ModelImage image = null;
        
        int nImages;
        int secondAddress = 0;
        int singleDims;
        double[] timeStamp;
        double[] myTimeStamp;
        boolean[] haveTimeStamp;

        try {
            fileList = getFileList(fileDir, fileName); // get series of files in the chosen dir
            nImages = fileList.length;

            if (nImages == 1) {
                return readLSM(fileName, fileDir, 0, false);
            }

            imageFile = new FileLSM(UI, fileName, fileDir, secondAddress); // read in files
            imageFile.setFileName(fileList[0]);
            imageFile.readImage(true, false);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            return null;
        }

        myFileInfo = imageFile.getFileInfo();
        singleExtents = myFileInfo.getExtents();
        singleDims = singleExtents.length;
        singleUnitsOfMeasure = myFileInfo.getUnitsOfMeasure();
        timeStamp = new double[nImages];
        haveTimeStamp = new boolean[nImages];

        if (singleDims == 2) {
            length = singleExtents[0] * singleExtents[1];
        } else {
            length = singleExtents[0] * singleExtents[1] * singleExtents[2];
        }


        try {
            resols = new float[5];

            if (nImages > 1) {

                if (singleDims == 3) {
                    extents = new int[4];
                    extents[3] = nImages;
                    extents[2] = singleExtents[2];
                    unitsOfMeasure = new int[4];
                    unitsOfMeasure[3] = FileInfoBase.SECONDS;
                    unitsOfMeasure[2] = singleUnitsOfMeasure[2];
                } else {
                    extents = new int[3];
                    extents[2] = nImages;
                    unitsOfMeasure = new int[3];
                    unitsOfMeasure[2] = FileInfoBase.SECONDS;
                }
            } // if (nImages > 1)
            else {

                if (singleDims == 3) {
                    extents = new int[3];
                    extents[2] = singleExtents[2];
                    unitsOfMeasure = new int[3];
                    unitsOfMeasure[2] = singleUnitsOfMeasure[2];
                } else {
                    extents = new int[2];
                    unitsOfMeasure = new int[2];
                }
            }

            extents[0] = singleExtents[0]; // copy out current [0,1] coords
            extents[1] = singleExtents[1]; // so all 3 ([0,1,2]) may be added
            unitsOfMeasure[0] = singleUnitsOfMeasure[0];
            unitsOfMeasure[1] = singleUnitsOfMeasure[1];

            resols = myFileInfo.getResolutions(); // ??

            myFileInfo.setExtents(extents); //
            myFileInfo.setResolutions(resols); // ??
            image = new ModelImage(myFileInfo.getDataType(), extents, myFileInfo.getFileName(), UI);

            createProgressBar(null, trim(fileName) + getSuffixFrom(fileName), FILE_READ);
            
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            return null;
        }

        image.setFileInfo(myFileInfo, 0);

        try {
            imageFile = new FileLSM(UI, fileList[0], fileDir, secondAddress);
        } catch (IOException error) {
            

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        }

        // loop through image and store data in image model
        if (singleDims == 2) {

            for (i = 0; i < nImages; i++) {

                try {
                    progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                    progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                    ((FileLSM) imageFile).setFileName(fileList[i]);

                    // fileLSM.testme(i);
                    ((FileLSM) imageFile).readImage(true, false);
                    myFileInfo = ((FileLSM) imageFile).getFileInfo();
                    myFileInfo.setExtents(extents);
                    myFileInfo.setUnitsOfMeasure(unitsOfMeasure);
                    myTimeStamp = ((FileInfoLSM) myFileInfo).getTimeStamp();

                    if (myTimeStamp != null) {
                        timeStamp[i] = myTimeStamp[0];
                        haveTimeStamp[i] = true;
                    } else {
                        haveTimeStamp[i] = false;
                    }

                    myFileInfo.setResolutions(resols);

                    if (nImages > 1) {
                        myFileInfo.setMultiFile(true);
                    }
                    image.setFileInfo(myFileInfo, i);

                    // float[] tmpBuffer = fileLSM.getImageBuffer();
                    if (image.isColorImage()) {
                        image.importData(i * 4 * length, ((FileLSM) imageFile).getImageBuffer(), false);
                    } else {
                        image.importData(i * length, ((FileLSM) imageFile).getImageBuffer(), false);
                    }

                } catch (IOException error) {
                    

                    if (!quiet) {
                        MipavUtil.displayError("FileIO: " + error);
                    }

                    if (image != null) {
                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                } catch (ArrayIndexOutOfBoundsException error) {
                    

                    if (!quiet) {
                        MipavUtil.displayError("Unable to read images: the image\n" + "number in the file " +
                                               myFileInfo.getFileName() + " is corrupted.");
                    }

                    if (image != null) {
                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                } catch (OutOfMemoryError error) {
                    

                    if (!quiet) {
                        MipavUtil.displayError("Out of memory: " + error);
                    }

                    if (image != null) {

                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                }
            } // for (i = 0; i < nImages; i++)
        } // if (singleDims == 2)
        else { // for singleDims == 3

            for (i = 0; i < nImages; i++) {

                try {
                    progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                    progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                    ((FileLSM) imageFile).setFileName(fileList[i]);

                    // fileLSM.testme(i);
                    ((FileLSM) imageFile).readImage(true, false);
                    myFileInfo = ((FileLSM) imageFile).getFileInfo();
                    myFileInfo.setExtents(extents);
                    myFileInfo.setUnitsOfMeasure(unitsOfMeasure);
                    myTimeStamp = ((FileInfoLSM) myFileInfo).getTimeStamp();

                    if (myTimeStamp != null) {
                        timeStamp[i] = myTimeStamp[0];
                        haveTimeStamp[i] = true;
                    } else {
                        haveTimeStamp[i] = false;
                    }

                    myFileInfo.setResolutions(resols);
                    if (nImages > 1) {
                        myFileInfo.setMultiFile(true);
                    }
                    for (j = 0; j < extents[2]; j++) {
                        image.setFileInfo(myFileInfo, (i * extents[2]) + j);
                        // float[] tmpBuffer = fileLSM.getImageBuffer();

                        if (image.isColorImage()) {
                            image.importData((i * 4 * length) + (j * 4 * extents[0] * extents[1]),
                                             ((FileLSM) imageFile).getImage3DMultiBuffer()[j], false);
                        } else {
                            image.importData((i * length) + (j * extents[0] * extents[1]),
                                             ((FileLSM) imageFile).getImage3DMultiBuffer()[j], false);
                        }
                    } // for (j = 0; j < extents[2]; j++) {
                } catch (IOException error) {
                    

                    if (!quiet) {
                        MipavUtil.displayError("FileIO: " + error);
                    }

                    if (image != null) {
                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                } catch (ArrayIndexOutOfBoundsException error) {
                    

                    if (!quiet) {
                        MipavUtil.displayError("Unable to read images: the image\n" + "number in the file " +
                                               myFileInfo.getFileName() + " is corrupted.");
                    }

                    if (image != null) {
                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                } catch (OutOfMemoryError error) {
                    

                    if (!quiet) {
                        MipavUtil.displayError("Out of memory: " + error);
                    }

                    if (image != null) {

                        image.disposeLocal();
                        image = null;
                    }

                    System.gc();

                    return null;
                }
            } // for (i = 0; i < nImages; i++)
        } // else for singleDims == 3

        fileInfo = image.getFileInfo();

        if (singleDims == 2) {

            for (i = 0; i < (nImages - 1); i++) {

                if ((haveTimeStamp[i] && haveTimeStamp[i + 1]) && (timeStamp[i] != timeStamp[i + 1])) {
                    resols = fileInfo[i].getResolutions();
                    resols[2] = (float) (timeStamp[i + 1] - timeStamp[i]);
                    fileInfo[i].setResolutions(resols);
                }
            }

            if ((haveTimeStamp[nImages - 2] && haveTimeStamp[nImages - 1]) &&
                    (timeStamp[nImages - 2] != timeStamp[nImages - 1])) {
                fileInfo[nImages - 1].setResolutions(resols);
            }
        } // if (singleDims == 2)
        else { // for singleDims == 3

            for (i = 0; i < (nImages - 1); i++) {

                if ((haveTimeStamp[i] && haveTimeStamp[i + 1]) && (timeStamp[i] != timeStamp[i + 1])) {

                    for (j = 0; j < extents[2]; j++) {
                        resols = fileInfo[(i * extents[2]) + j].getResolutions();
                        resols[3] = (float) (timeStamp[i + 1] - timeStamp[i]);
                        fileInfo[(i * extents[2]) + j].setResolutions(resols);
                    }
                }
            }

            if ((haveTimeStamp[nImages - 2] && haveTimeStamp[nImages - 1]) &&
                    (timeStamp[nImages - 2] != timeStamp[nImages - 1])) {

                for (j = 0; j < extents[2]; j++) {
                    resols = fileInfo[((nImages - 1) * extents[2]) + j].getResolutions();
                    resols[3] = (float) (timeStamp[nImages - 1] - timeStamp[nImages - 2]);
                    fileInfo[((nImages - 1) * extents[2]) + j].setResolutions(resols);
                }
            }
        } // else for singleDims == 3

        

        return image;

    }


    /**
     * Reads a Magnetom Vision file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readMagnetomVision(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileMagnetomVision imageFile;
        FileInfoBase myFileInfo;
        
        String[] fileList;
        short[] buffer;
        int[] extents;
        int[] imageNumbers;
        int[] indicies;
        int length = 0;
        int i;
        int width, height;
        int nImages;

        if (one) {
            return readOneMagnetomVision(fileName, fileDir);
        }

        try {
            fileList = getFileList(fileDir, fileName);
            imageFile = new FileMagnetomVision(fileName, fileDir);

            imageFile.setFileName(fileList[0]);
            
            createProgressBar(null, fileName, FILE_READ);
            
            myFileInfo = imageFile.readHeader();
            width = myFileInfo.getExtents()[0];
            height = myFileInfo.getExtents()[1];
            length = width * height;
            buffer = new short[length];

            if (fileList.length == 1) {
                extents = new int[2];
                extents[0] = width;
                extents[1] = height;
            } else {
                extents = new int[3];
                extents[0] = width;
                extents[1] = height;
                extents[2] = fileList.length;
            }

            image = new ModelImage(ModelStorageBase.SHORT, extents, myFileInfo.getFileName(), UI);

            nImages = fileList.length;
            imageNumbers = new int[nImages];
            indicies = new int[nImages];
            progressBar.setTitle("Reading headers");

            for (i = 0; i < nImages; i++) {
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 10));
                imageFile.setFileName(fileList[i]);
                myFileInfo = imageFile.readHeader();
                imageNumbers[i] = ((FileInfoMagnetomVision) myFileInfo).getTextImageNumber();
                indicies[i] = i;
            }

            sort(imageNumbers, indicies, nImages);
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        // loop through files, place them in image array
        try {

            for (i = 0; i < nImages; i++) {
                progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round(10 + ((float) i / (nImages - 1) * 90)));

                if (fileList[i] != null) {
                    imageFile.setFileName(fileList[i]);
                    myFileInfo = imageFile.readHeader();
                    imageFile.readImage(buffer);
                    myFileInfo.setExtents(extents);

                    // myFileInfo.setStartLocations(((FileInfoGESigna5X)(myFileInfo)).getStart(imageFile.getImageNumber()-1));
                    image.setFileInfo(myFileInfo, indicies[i]);
                    image.importData(indicies[i] * length, buffer, false);
                    image.setFileInfo(myFileInfo, indicies[i]);
                }
            }

            imageFile.close();
        } catch (IOException error) {
            

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        } catch (OutOfMemoryError error) {
            

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            if (image != null) {

                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        }

        image.setImageName(imageFile.getFileInfo().getImageNameFromInfo());

        

        return image;
    }

    /**
     * Reads a Map file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readMap(String fileName, String fileDir) {
        ModelImage image = null;
        FileMap imageFile;
        FileInfoBase fileInfo;
        int i;

        JDialogRawIO mapIODialog = new JDialogRawIO(UI.getMainFrame(), "MAP");

        mapIODialog.setVisible(true);

        if (mapIODialog.isCancelled() == true) {
            return null;
        }

        try {
            fileInfo = new FileInfoImageXML(fileName, fileDir, FileBase.RAW);
            image = new ModelImage(mapIODialog.getDataType(), mapIODialog.getExtents(), fileName, UI);
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        fileInfo.setUnitsOfMeasure(mapIODialog.getUnitsOfMeasure());
        fileInfo.setResolutions(mapIODialog.getResolutions());
        fileInfo.setEndianess(mapIODialog.getEndianess());
        fileInfo.setExtents(mapIODialog.getExtents());

        if (fileInfo.getExtents().length > 2) { // Set file info

            for (i = 0; i < fileInfo.getExtents()[2]; i++) {
                image.setFileInfo(fileInfo, i);
            }
        } else {
            image.setFileInfo(fileInfo, 0);
        }

        try {
            imageFile = new FileMap(UI, fileName, fileDir, fileInfo, FileBase.READ);
            createProgressBar(imageFile, fileName, FILE_READ);
            imageFile.readImage(image, mapIODialog.getOffset());
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads a MedVision file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readMedVision(String fileName, String fileDir) {
        ModelImage image = null;
        FileMedVision imageFile;

        try {
            imageFile = new FileMedVision(UI, fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads an Micro Cat file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readMicroCat(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileMicroCat imageFile;
        FileInfoMicroCat fileInfoMicro;

        try {
            imageFile = new FileMicroCat(UI, fileName, fileDir);

            if (fileName.endsWith(".ct")) {
                int i;

                File imageDir = new File(fileDir);
                String[] fileList = imageDir.list();

                for (i = 0; i < fileList.length; i++) {

                    if (fileList[i].endsWith(".log")) {
                        imageFile.setFileName(fileList[i]);
                        fileInfoMicro = imageFile.readHeader();

                        if (imageFile.trimmer(fileName).equals(fileInfoMicro.getBaseNameforReconstructedSlices() + "_")) {
                            break;
                        }
                    }
                }

                if (i == fileList.length) {

                    if (!quiet) {
                        MipavUtil.displayError("No appropriate header files for " + fileName);
                    }

                    return null;
                }
            }

            // at this point either image ended with .log and should read normally,
            // or appropriate header file was found and set in imageFile, and can read normally
            if (one) {
                image = imageFile.readImage(quiet, one, fileName);
            } else {
                image = imageFile.readImage(quiet);
            }
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Reads a MINC file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readMinc(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileMinc imageFile;

        try {
            imageFile = new FileMinc(fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug(error + "\n", Preferences.DEBUG_FILEIO);
                for (int i = 0; i < error.getStackTrace().length; i++) {
                    Preferences.debug("\t" + error.getStackTrace()[i] + "\n", Preferences.DEBUG_FILEIO);
                }
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Reads a MRC file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readMRC(String fileName, String fileDir) {
        ModelImage image = null;
        FileMRC imageFile;

        try {
            imageFile = new FileMRC(UI, fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
            // LUT      = imageFile.getModelLUT();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads a MGH file by calling the read method of the file. if so, calls that method instead.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readMGH(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        boolean showProgressBar = (!quiet) ? true : false;
        FileMGH imageFile;

        if (!UI.isAppFrameVisible()) {
            showProgressBar = false;
        }

        try {
            imageFile = new FileMGH(UI, fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Reads a NIFTI file by calling the read method of the file. if so, calls that method instead.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readNIFTI(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileNIFTI imageFile;

        try {
            imageFile = new FileNIFTI(UI, fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Reads a multi NIFTI file. Gets a list of the images from the file directory and reads them each in.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readNIFTIMulti(String fileName, String fileDir) {
        ModelImage image = null;
        FileNIFTI imageFile;
        FileInfoBase fileInfo;
        
        int length = 0;
        float[] buffer;
        String[] fileList;
        int[] extents;
        float[] resolutions;
        int nImages;

        // of proper extents (in case there is a file with the consistent filename but
        // inconsistent extents.) we do assume the 1st header is correct


        int i = 0;

        try {
            fileList = getFileList(fileDir, fileName);

            for (int m = 0; m < fileList.length; m++) {

                if (fileList[m] != null) {

                    // System.out.println(" Name = " + fileList[m]);
                    i++;
                }
            }
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        nImages = i; // total number of suspected files to import into an image

        if (nImages == 1) {
            return readNIFTI(fileName, fileDir, false);
        }
        createProgressBar(null, trim(fileName) + getSuffixFrom(fileName), FILE_READ);
        
        // System.out.println("nImage = " + i);
        // System.out.println(" filelist[0] = " + fileList[0]);
        // System.out.println(" filelist[1] = " + fileList[1]);
        // if one of the images has the wrong extents, the following must be changed.
        // (ei., too many images!)
        // for simplicity of setup, read in the first file hdr
        imageFile = new FileNIFTI(UI, fileList[0], fileDir);

        try {

            if (!imageFile.readHeader(fileList[0], fileDir)) {
                throw (new IOException(" NIFTI header file error"));
            }
        } catch (IOException ioe) {

            if (!quiet) {
                MipavUtil.displayError("Error reading header file.");
            }
        }

        fileInfo = ((FileNIFTI) imageFile).getFileInfo();
        extents = fileInfo.getExtents();
        resolutions = fileInfo.getResolutions();

        if (extents.length == 2) {
            length = extents[0] * extents[1];
        } else if (extents.length == 3) {
            length = extents[0] * extents[1] * extents[2];
        } else if (extents.length == 4) {
            length = extents[0] * extents[1] * extents[2] * extents[3];
        }

        buffer = new float[length];

        int[] imgExtents = new int[extents.length + 1];
        float[] imgResolutions = new float[resolutions.length + 1]; // should be same size as extents.

        // copy proper values into img extents, assuming that the 1st (numerically indexed) images
        // is correct to begin with
        for (i = 0; i < extents.length; i++) {
            imgExtents[i] = extents[i];
            imgResolutions[i] = resolutions[i];
            // set the number of slices in the image later.
        }

        imgExtents[i] = nImages; // may not be right, but we'll find out after we go through all images.
        imgResolutions[i] = 1; // resolution in the created axis is not physically defined; is generated.

        image = new ModelImage(fileInfo.getDataType(), imgExtents, fileInfo.getFileName(), UI);

        int imageCount = 0;

        // loop through image and store data in image model
        for (i = 0; i < nImages; i++) {

            try {
                progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                imageFile = new FileNIFTI(UI, fileList[i], fileDir);

                if (!((FileNIFTI) imageFile).readHeader(fileList[i], fileDir)) {
                    throw (new IOException(" NIFTI header file error"));
                }

                // chk the extents of the image to verify it is consistent
                // (this doesn't ensure there won't be null exceptions@)
                fileInfo = ((FileNIFTI) imageFile).getFileInfo();

                if (extents.length != fileInfo.getExtents().length) {

                    if (!quiet) {
                        MipavUtil.displayError("Inconsistent image file found.  This File will be skipped.");
                    }

                    continue;
                } else { // the prototype image and the read-in image are of the same dimension....

                    switch (extents.length) { // check that they extend as far in all dimensions:

                        case 2:
                            if ((extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1])) {

                                if (!quiet) {
                                    MipavUtil.displayError("Inconsistent image file found.  This File will be skipped.");
                                }

                                continue;
                            }

                            break;

                        case 3:
                            if ((extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1]) ||
                                    (extents[2] != fileInfo.getExtents()[2])) {

                                if (!quiet) {
                                    MipavUtil.displayError("Inconsistent image file found.  This File will be skipped.");
                                }

                                continue;
                            }

                            break;

                        case 4:
                            if ((extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1]) ||
                                    (extents[2] != fileInfo.getExtents()[2]) ||
                                    (extents[3] != fileInfo.getExtents()[3])) {

                                if (!quiet) {
                                    MipavUtil.displayError("Inconsistent image file found.  This File will be skipped.");
                                }

                                continue;
                            }

                            break;

                        default:
                            break;
                    }
                }

                fileInfo.setExtents(imgExtents); // set image extents to proper value!
                fileInfo.setResolutions(imgResolutions);
                if (nImages > 1) {
                    fileInfo.setMultiFile(true);
                }

                ((FileNIFTI) imageFile).readImage(buffer);
                image.importData(imageCount * length, buffer, false);
                image.setFileInfo(fileInfo, imageCount);
                imageCount++; // image was okay, so count it.(can't do it before b/c of offset)

            } catch (IOException error) {
                

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (ArrayIndexOutOfBoundsException error) {
                

                if (!quiet) {
                    MipavUtil.displayError("Unable to read images: the image\nnumber in the file " +
                                           fileInfo.getFileName() + " is corrupted.");
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (OutOfMemoryError error) {
                

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        }
        // i goes 1 too far anyway, but if  we skipped files, be sure to account for it,
        // because our basic model was that all prperly named files were good analyze images.
        // only we found one or more didn't fit.  We must now take that into account.
        // ie., we read in imageCount # of images, we expected nImages.

        if (imageCount < nImages) {
            FileInfoBase[] fileInfoArr = image.getFileInfo();

            imgExtents[image.getNDims() - 1] = imageCount; // last dimension available should be the num images read

            int sliceSize = buffer.length;

            buffer = new float[sliceSize * imageCount];

            try {
                image.exportData(0, buffer.length, buffer); // copy all buffer
                image.reallocate(fileInfo.getDataType(), imgExtents);
                image.importData(0, buffer, true); // remake the model image with the right number of slices.

                for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                    fileInfoArr[i].setExtents(imgExtents); // update extents
                    image.setFileInfo(fileInfoArr[i], i); // copying...
                }
            } catch (IOException ioe) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO reports: " + ioe.getMessage());
                }

                return null;
            }

            for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                fileInfoArr[i].setExtents(imgExtents); // update extents

                // fineInfoArr[i].setN

                image.setFileInfo(fileInfoArr[i], i); // copying...
            }
        }

        

        return image;

    }

    /**
     * Reads a NRRD file by calling the read method of the file. if so, calls that method instead.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readNRRD(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        boolean showProgressBar = (!quiet) ? true : false;
        FileNRRD imageFile;

        if (!UI.isAppFrameVisible()) {
            showProgressBar = false;
        }

        try {
            imageFile = new FileNRRD(UI, fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Reads in a single GE Signa 4x type file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readOneGESigna4X(String fileName, String fileDir) {
        ModelImage image = null;
        FileGESigna4X imageFile;
        FileInfoBase myFileInfo;
        float[] buffer;
        int[] extents;
        int width, height;
        int imageSize;

        try {
            imageFile = new FileGESigna4X(fileName, fileDir);
            imageFile.setFileName(fileName);
            imageSize = imageFile.readImageFileData();

            if (imageSize == -1) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: Compression not supported for Signa 4X");
                }

                return null;
            }

            if (imageSize == -2) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: Not a Signa 4X file.");
                }

                return null;
            }

            if (imageSize == 0) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: Image length.");
                }

                return null;
            }

            width = imageFile.getWidth();
            height = imageFile.getHeight();
            buffer = new float[width * height];

            extents = new int[2];
            extents[0] = width;
            extents[1] = height;

            image = new ModelImage(ModelImage.USHORT, extents, "GE", UI);
            imageFile.readImage(buffer);
            myFileInfo = imageFile.getFileInfo();
            myFileInfo.setExtents(extents);
            image.setFileInfo(myFileInfo, 0);
            image.setImageName(imageFile.getFileInfo().getImageNameFromInfo());
            image.importData(0, buffer, false);
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Reads in a single GE Genesis type file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readOneGEGenesis5X(String fileName, String fileDir) {
        ModelImage image = null;
        FileGESigna5X imageFile;
        FileInfoBase myFileInfo;
        float[] buffer;
        int[] extents;
        int width, height;
        int imageSize = 0;

        try {
            imageFile = new FileGESigna5X(fileName, fileDir);
            imageFile.setFileName(fileName);
            imageSize = imageFile.readImageFileData();

            if (imageSize == -1) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: Compression not supported for Signa 5X");
                }

                return null;
            }

            if (imageSize == -2) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: Not a Signa 5X file.");
                }

                return null;
            }

            if (imageSize == 0) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: Image length.");
                }

                return null;
            }

            width = imageFile.getWidth();
            height = imageFile.getHeight();
            buffer = new float[width * height];

            extents = new int[2];
            extents[0] = width;
            extents[1] = height;

            image = new ModelImage(ModelImage.USHORT, extents, "GE", UI);
            imageFile.readImage(buffer);
            myFileInfo = imageFile.getFileInfo();
            myFileInfo.setExtents(extents);
            image.setFileInfo(myFileInfo, 0);
            image.setImageName(imageFile.getFileInfo().getImageNameFromInfo());
            image.importData(0, buffer, false);
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Reads a Magnetom Vision file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readOneMagnetomVision(String fileName, String fileDir) {
        ModelImage image = null;
        FileMagnetomVision imageFile;
        FileInfoBase myFileInfo;
        int width, height;
        short[] buffer;
        int[] extents;

        try {
            imageFile = new FileMagnetomVision(fileName, fileDir);

            imageFile.setFileName(fileName);
            myFileInfo = imageFile.readHeader();
            width = myFileInfo.getExtents()[0];
            height = myFileInfo.getExtents()[1];
            buffer = new short[width * height];

            extents = new int[2];
            extents[0] = width;
            extents[1] = height;
            image = new ModelImage(ModelStorageBase.SHORT, extents, myFileInfo.getFileName(), UI);
            imageFile.readImage(buffer);
            myFileInfo.setExtents(extents);
            image.setFileInfo(myFileInfo, 0);
            image.setImageName(imageFile.getFileInfo().getImageNameFromInfo());
            image.importData(0, buffer, false);
            imageFile.close();
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            if (image != null) {

                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        }

        return image;
    }

    /**
     * Reads an OSM file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readOSM(String fileName, String fileDir) {
        ModelImage image = null;
        FileOSM imageFile;

        try {
            imageFile = new FileOSM(UI, fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
            // LUT      = imageFile.getModelLUT();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads a QT file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readQT(String fileName, String fileDir) {

        // QuickTime
        /*
         * FileQT imageFile; ModelImage image = null; try { imageFile = new FileQT(UI, fileName, fileDir); image =
         * imageFile.readImage(); //LUT      = ((FileQT)imageFile).getModelLUT(); } catch (IOException error) { if
         * (image != null) { image.disposeLocal(); image = null; } System.gc(); if (!quiet) {
         * MipavUtil.displayError("FileIO: " + error); } return null; } catch (OutOfMemoryError error) { if (image !=
         * null) { image.disposeLocal(); image = null; } System.gc(); if (!quiet) { MipavUtil.displayError("FileIO: " +
         * error); } return null; }
         */
        return null;

    }

    /**
     * Reads a RAW file by calling the read method of the file. Gets the necessary information from a dialog, if the
     * <code>fileInfo</code> parameter is null, and otherwise relies on the information stored in <code>fileInfo</code>
     * to properly read in the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   fileInfo  File info of the image file; usually null, but if defined no dialog appears.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readRaw(String fileName, String fileDir, FileInfoBase fileInfo) {
        ModelImage image = null;
        FileRaw imageFile;

        int i;

        if (fileInfo == null) {
            JDialogRawIO rawIODialog = new JDialogRawIO(UI.getMainFrame(), "Raw");

            rawIODialog.setVisible(true);

            if (rawIODialog.isCancelled() == true) {
                return null;
            }

            fileInfo = new FileInfoImageXML(fileName, fileDir, FileBase.RAW);
            fileInfo.setDataType(rawIODialog.getDataType());
            fileInfo.setExtents(rawIODialog.getExtents());
            fileInfo.setUnitsOfMeasure(rawIODialog.getUnitsOfMeasure());
            fileInfo.setResolutions(rawIODialog.getResolutions());
            fileInfo.setEndianess(rawIODialog.getEndianess());
            fileInfo.setOffset(rawIODialog.getOffset());
        }

        try {
            image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileName, UI);
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        if (fileInfo.getExtents().length > 2) { // Set file info

            for (i = 0; i < fileInfo.getExtents()[2]; i++) {
                image.setFileInfo(fileInfo, i);
            }
        } else {
            image.setFileInfo(fileInfo, 0);
        }

        try {
            imageFile = new FileRaw(fileName, fileDir, fileInfo, FileBase.READ);
           
            imageFile.readImage(image, fileInfo.getOffset());
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Reads a RAW file by calling the read method of the file. Gets the necessary information from a dialog, if the
     * <code>fileInfo</code> parameter is null, and otherwise relies on the information stored in <code>fileInfo</code>
     * to properly read in the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   fileInfo  File info of the image file; usually null, but if defined no dialog appears.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readRawMulti(String fileName, String fileDir, FileInfoBase fileInfo) {
        ModelImage image = null;
        FileRaw imageFile;
        FileInfoImageXML[] nFileInfos;
        String[] fileList;
        int i;
        
        int nImages;

        if (fileInfo == null) {
            JDialogRawIO rawIODialog = new JDialogRawIO(UI.getMainFrame(), "Raw");

            rawIODialog.setVisible(true);

            if (rawIODialog.isCancelled() == true) {
                return null;
            }

            fileInfo = new FileInfoImageXML(fileName, fileDir, FileBase.RAW);
            fileInfo.setDataType(rawIODialog.getDataType());
            fileInfo.setExtents(rawIODialog.getExtents());
            fileInfo.setUnitsOfMeasure(rawIODialog.getUnitsOfMeasure());
            fileInfo.setResolutions(rawIODialog.getResolutions());
            fileInfo.setEndianess(rawIODialog.getEndianess());
            fileInfo.setOffset(rawIODialog.getOffset());
        }

        i = 0;

        try {
            fileList = getFileList(fileDir, fileName);
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO.readRawMulti: " + error);
            }

            return null;
        }

        nImages = fileList.length; // total number of suspected files to import into an image

        // if nImages == 1 then display error and return ???

        if (nImages == 1) {
            return readRaw(fileName, fileDir, fileInfo);
        } else {
            fileInfo.setMultiFile(true);
        }

        createProgressBar(null, fileName, FILE_READ);
        
        nFileInfos = new FileInfoImageXML[nImages];

        if (nImages > 1) {
            int[] extents = new int[3];

            extents[0] = fileInfo.getExtents()[0];
            extents[1] = fileInfo.getExtents()[1];
            extents[2] = nImages;
            fileInfo.setExtents(extents);

            float[] resols = new float[3];
            resols[0] = fileInfo.getResolutions()[0];
            resols[1] = fileInfo.getResolutions()[1];
            resols[2] = fileInfo.getResolutions()[2];
            fileInfo.setResolutions(resols);
        }

        int length = fileInfo.getExtents()[0] * fileInfo.getExtents()[1];
        float[] buffer;

        try {
            buffer = new float[length];
            image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileName, UI);
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            buffer = null;
            
            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        // Copy all
        if (fileInfo.getExtents().length > 2) { // Set file info

            for (i = 0; i < nImages; i++) {
                nFileInfos[i] = (FileInfoImageXML) (fileInfo.clone());
            }

            image.setFileInfo(nFileInfos);
        } else {
            image.setFileInfo(fileInfo, 0);
        }

        for (int m = 0; m < nImages; m++) {

            try {
                imageFile = new FileRaw(fileList[m], fileDir, fileInfo, FileBase.READ);
                progressBar.updateValue((int) (((float) m / (float) nImages) * 100.0f), false);
                imageFile.readImage(buffer, fileInfo.getOffset(), fileInfo.getDataType());
                image.importData(m * length, buffer, false);
            } catch (IOException error) {

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                buffer = null;
                
                System.gc();

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                return null;
            } catch (OutOfMemoryError error) {

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                buffer = null;
                
                System.gc();

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                return null;
            }
        }

        

        return image;

    }

    /**
     * Reads an SPM file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readSPM(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileSPM imageFile;

        try {
            imageFile = new FileSPM(UI, fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(one);
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Reads a STK file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readSTK(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileSTK imageFile;

        try {
            imageFile = new FileSTK(UI, fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads a TIFF file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readTiff(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileTiff imageFile;

        try {
            imageFile = new FileTiff(UI, fileName, fileDir);
            image = imageFile.readImage(false, one);
            LUT = imageFile.getModelLUT();

        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;
    }

    /**
     * Reads a multi TIFF file by first reading the headers then reading in each separate file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readTiffMulti(String fileName, String fileDir) {
        float[] resols;
        int[] extents; // extent of image  (!def!)
        int length = 0;
        int i;
        FileInfoBase myFileInfo;
        String[] fileList;
        FileTiff imageFile;
        ModelImage image = null;
        
        int nImages;

        try {
            fileList = getFileList(fileDir, fileName); // get series of files in the chosen dir
            nImages = fileList.length;
            imageFile = new FileTiff(UI, fileName, fileDir); // read in files
            imageFile.setFileName(fileList[0]);
            imageFile.readImage(true, false);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            return null;
        }

        myFileInfo = imageFile.getFileInfo();

        length = myFileInfo.getExtents()[0] * myFileInfo.getExtents()[1];
       

        try {
            resols = new float[5];

            if (nImages > 1) {
                extents = new int[3];
                extents[2] = nImages;
            } else {
                extents = new int[2];
            }

            extents[0] = myFileInfo.getExtents()[0]; // copy out current [0,1] coords
            extents[1] = myFileInfo.getExtents()[1]; // so all 3 ([0,1,2]) may be added

            resols = myFileInfo.getResolutions(); // ??

            myFileInfo.setExtents(extents); //
            myFileInfo.setResolutions(resols); // ??
            image = new ModelImage(myFileInfo.getDataType(), extents, myFileInfo.getFileName(), UI);

            createProgressBar(null, trim(fileName) + getSuffixFrom(fileName) , FILE_READ);
          
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("Out of memory: " + error);
            }

            return null;
        }

        image.setFileInfo(myFileInfo, 0);

        try {
            imageFile = new FileTiff(UI, fileList[0], fileDir);
        } catch (IOException error) {
            

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            return null;
        }

        // loop through image and store data in image model
        for (i = 0; i < nImages; i++) {

            try {
                progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                ((FileTiff) imageFile).setFileName(fileList[i]);

                // fileTIFF.testme(i);
                ((FileTiff) imageFile).readImage(true, false);
                myFileInfo = ((FileTiff) imageFile).getFileInfo();
                myFileInfo.setExtents(extents);
                myFileInfo.setResolutions(resols);
                if (nImages > 1) {
                    myFileInfo.setMultiFile(true);
                }
                image.setFileInfo(myFileInfo, i);

                // float[] tmpBuffer = fileTIFF.getImageBuffer();
                if (image.isColorImage()) {
                    image.importData(i * 4 * length, ((FileTiff) imageFile).getImageBuffer(), false);
                } else {
                    image.importData(i * length, ((FileTiff) imageFile).getImageBuffer(), false);
                }
            } catch (IOException error) {
                

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (ArrayIndexOutOfBoundsException error) {
                

                if (!quiet) {
                    MipavUtil.displayError("Unable to read images: the image\n" + "number in the file " +
                                           myFileInfo.getFileName() + " is corrupted.");
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (OutOfMemoryError error) {
                

                if (!quiet) {
                    MipavUtil.displayError("Out of memory: " + error);
                }

                if (image != null) {

                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        }

        

        return image;

    }


    /**
     * Reads a TMG file by calling the read method of the file.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readTMG(String fileName, String fileDir) {
        ModelImage image = null;
        FileTMG imageFile;

        try {
            imageFile = new FileTMG(UI, fileName, fileDir);
            createProgressBar(imageFile, fileName, FILE_READ);
            image = imageFile.readImage();
            // LUT      = imageFile.getModelLUT();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Reads an XML file by calling the read method of the file. This method contains special code to not display the
     * progress bar should it load <q>splash.xml</q>.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     * @param   one       Indicates that only the named file should be read, as opposed to reading the matching files in
     *                    the directory, as defined by the filetype. <code>true</code> if only want to read one image
     *                    from 3D dataset.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readXML(String fileName, String fileDir, boolean one) {
        ModelImage image = null;
        FileImageXML imageFile;
        // don't show splash screen:
        

        try {
            imageFile = new FileImageXML(UI, fileName, fileDir);
            
            if (!(fileName.equals("splash.xml") || (one == true))) {
            	createProgressBar(imageFile, fileName, FILE_READ);
            }
            image = imageFile.readImage(one);
            LUT = imageFile.getModelLUT();
            modelRGB = imageFile.getModelRGB();
        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        // System.out.println(" image = " + image);
        return image;
    }

    /**
     * Reads a multi XML file. Gets a list of the images from the file directory and reads them each in.
     *
     * @param   fileName  Name of the image file to read.
     * @param   fileDir   Directory of the image file to read.
     *
     * @return  The image that was read in, or null if failure.
     */
    private ModelImage readXMLMulti(String fileName, String fileDir) {
        ModelImage image = null;
        FileImageXML imageFile;
        FileInfoImageXML fileInfo;
        
        int length = 0;
        float[] buffer;
        String[] fileList;
        int[] extents;
        float[] resolutions;
        int nImages;

        // of proper extents (in case there is a file with the consistent filename but
        // inconsistent extents.) we do assume the 1st header is correct


        int i = 0;

        try {
            fileList = getFileList(fileDir, fileName);

            for (int m = 0; m < fileList.length; m++) {

                if (fileList[m] != null) {

                    // System.out.println(" Name = " + fileList[m]);
                    i++;
                }
            }
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        nImages = i; // total number of suspected files to import into an image

        if (nImages == 1) {
            return readXML(fileName, fileDir, false);
        }

        createProgressBar(null, trim(fileName) + getSuffixFrom(fileName), FILE_READ);
        
        // System.out.println(" filelist[1] = " + fileList[1]);
        // if one of the images has the wrong extents, the following must be changed.
        // (ei., too many images!)
        // for simplicity of setup, read in the first file hdr
        float[][] res = null;

        imageFile = new FileImageXML(UI, fileList[0], fileDir);

        try {
            TalairachTransformInfo talairach = new TalairachTransformInfo();
            res = imageFile.readHeader(fileList[0], fileDir, talairach);

            if (res == null) {
                throw (new IOException(" Analyze header file error"));
            }
        } catch (IOException ioe) {

            if (!quiet) {
                MipavUtil.displayError("Error reading header file.");
            }
        }

        fileInfo = ((FileImageXML) imageFile).getFileInfo();
        extents = fileInfo.getExtents();
        resolutions = fileInfo.getResolutions();

        if (extents.length == 2) {
            length = extents[0] * extents[1];
        } else if (extents.length == 3) {
            length = extents[0] * extents[1] * extents[2];
        } else if (extents.length == 4) {
            length = extents[0] * extents[1] * extents[2] * extents[3];
        }

        buffer = new float[length];

        int[] imgExtents = new int[extents.length + 1];
        float[] imgResolutions = new float[resolutions.length + 1]; // should be same size as extents.

        // copy proper values into img extents, assuming that the 1st (numerically indexed) images
        // is correct to begin with
        for (i = 0; i < extents.length; i++) {
            imgExtents[i] = extents[i];
            imgResolutions[i] = resolutions[i];
            // set the number of slices in the image later.
        }

        imgExtents[i] = nImages; // may not be right, but we'll find out after we go through all images.
        imgResolutions[i] = 1; // resolution in the created axis is not physically defined; is generated.

        image = new ModelImage(fileInfo.getDataType(), imgExtents, fileInfo.getFileName(), UI);

        int imageCount = 0;

        // loop through image and store data in image model
        for (i = 0; i < nImages; i++) {

            try {
                //progressBar.setTitle(UI.getProgressBarPrefix() + "image " + fileList[i]);
                progressBar.updateValueImmed(Math.round((float) i / (nImages - 1) * 100));
                imageFile.setFileName(fileList[i]);
                //imageFile = new FileImageXML(UI, fileList[i], fileDir, false);

                TalairachTransformInfo talairach = new TalairachTransformInfo();
                res = ((FileImageXML) imageFile).readHeader(fileList[i], fileDir, talairach);

                if (res == null) {
                    throw (new IOException(" XML header file error"));
                }

                // chk the extents of the image to verify it is consistent
                // (this doesn't ensure there won't be null exceptions@)
                fileInfo = ((FileImageXML) imageFile).getFileInfo();

                if (extents.length != fileInfo.getExtents().length) {

                    if (!quiet) {
                        MipavUtil.displayError("Inconsistent image file found.  This file will be skipped.");
                    }

                    continue;
                } else { // the prototype image and the read-in image are of the same dimension....

                    switch (extents.length) { // check that they extend as far in all dimensions:

                        case 2:
                            if ((extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1])) {

                                if (!quiet) {
                                    MipavUtil.displayError("Inconsistent image file found.  This file will be skipped.");
                                }

                                continue;
                            }

                            break;

                        case 3:
                            if ((extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1]) ||
                                    (extents[2] != fileInfo.getExtents()[2])) {

                                if (!quiet) {
                                    MipavUtil.displayError("Inconsistent image file found.  This file will be skipped.");
                                }

                                continue;
                            }

                            break;

                        case 4:
                            if ((extents[0] != fileInfo.getExtents()[0]) || (extents[1] != fileInfo.getExtents()[1]) ||
                                    (extents[2] != fileInfo.getExtents()[2]) ||
                                    (extents[3] != fileInfo.getExtents()[3])) {

                                if (!quiet) {
                                    MipavUtil.displayError("Inconsistent image file found.  This file will be skipped.");
                                }

                                continue;
                            }

                            break;

                        default:
                            break;
                    }
                }

                fileInfo.setExtents(imgExtents); // set image extents to proper value!
                fileInfo.setResolutions(imgResolutions);
                if (nImages > 1) {
                    fileInfo.setMultiFile(true);
                }
                ((FileImageXML) imageFile).readImage(buffer);
                image.importData(imageCount * length, buffer, false);
                image.setFileInfo(fileInfo, imageCount);
                imageCount++; // image was okay, so count it.(can't do it before b/c of offset)

            } catch (IOException error) {
                

                if (!quiet) {
                    Preferences.debug("Failed to read XML multifile. This error can be caused by attempting to read an XML file that is not actually a multi-file.\n",
                                      Preferences.DEBUG_FILEIO);
                    MipavUtil.displayError("Failed to read XML multifile.");
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (ArrayIndexOutOfBoundsException error) {
                

                if (!quiet) {
                    MipavUtil.displayError("Unable to read images: the image\nnumber in the file " +
                                           fileInfo.getFileName() + " is corrupted.");
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            } catch (OutOfMemoryError error) {
                

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }

                System.gc();

                return null;
            }
        }
        // i goes 1 too far anyway, but if  we skipped files, be sure to account for it,
        // because our basic model was that all prperly named files were good analyze images.
        // only we found one or more didn't fit.  We must now take that into account.
        // ie., we read in imageCount # of images, we expected nImages.

        if (imageCount < nImages) {
            FileInfoBase[] fileInfoArr = image.getFileInfo();

            imgExtents[image.getNDims() - 1] = imageCount; // last dimension available should be the num images read

            int sliceSize = buffer.length;

            buffer = new float[sliceSize * imageCount];

            try {
                image.exportData(0, buffer.length, buffer); // copy all buffer
                image.reallocate(fileInfo.getDataType(), imgExtents);
                image.importData(0, buffer, true); // remake the model image with the right number of slices.

                for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                    fileInfoArr[i].setExtents(imgExtents); // update extents
                    image.setFileInfo(fileInfoArr[i], i); // copying...
                }
            } catch (IOException ioe) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO reports: " + ioe.getMessage());
                }

                return null;
            }

            for (i = 0; i < imgExtents[imgExtents.length - 1]; i++) { // copy all image info
                fileInfoArr[i].setExtents(imgExtents); // update extents

                // fineInfoArr[i].setN

                image.setFileInfo(fileInfoArr[i], i); // copying...
            }
        }

        

        return image;

    }

    /**
     * Sorts an array of floats (using insertion sort), turns into array of ints used to sort images by slice location;
     * thus, the final value of B[i] is for the ith image read in, put that image at location B[i] in the image buffer.
     * This is necessary for images labeled dicom1, dicom2, etc because dicom11, dicom12, ... will come before dicom2.
     * Also our only indication of the "true" ordering is slice location.
     *
     * @param   A     Array to be sorted.
     * @param   B     Array it goes into.
     * @param   size  Size of the array (both arrays are the same size).
     *
     * @return  <code>false</code> only if any of the numbers in the array are equal.
     */
    private boolean sort(float[] A, int[] B, int size) {
        boolean flag = true;
        int stop = size - 1, i, tmp2;
        float tmp;

        while (stop > 0) {

            for (i = 0; i < stop; i++) {

                if (A[i] > A[i + 1]) {
                    tmp = A[i];
                    A[i] = A[i + 1];
                    A[i + 1] = tmp;
                    tmp2 = B[i];
                    B[i] = B[i + 1];
                    B[i + 1] = tmp2;
                }

                if (A[i] == A[i + 1]) {
                    flag = false;
                }
            }

            stop--;
        }

        int[] C = new int[size];

        for (i = 0; i < size; i++) {
            C[B[i]] = i;
        }

        for (i = 0; i < size; i++) {
            B[i] = C[i];
        }

        return flag;
    }

    /**
     * Sorts an array of ints (using insertion sort), turns into array of ints used to sort images by image number;
     * thus, the final value of B[i] is for the ith image read in, put that image at location B[i] in the image buffer.
     * This is necessary for images labeled dicom1, dicom2, etc because dicom11, dicom12, ... will come before dicom2.
     * Also our only indication of the "true" ordering is image number.
     *
     * @param   A     Array to be sorted.
     * @param   B     Array it goes into.
     * @param   size  Size of the array (both arrays are the same size).
     *
     * @return  <code>false</code> only if any of the numbers in the array are equal.
     */
    private boolean sort(int[] A, int[] B, int size) {
        boolean flag = true;
        int stop = size - 1, i, tmp2;
        int tmp;

        while (stop > 0) {

            for (i = 0; i < stop; i++) {

                if (A[i] > A[i + 1]) {
                    tmp = A[i];
                    A[i] = A[i + 1];
                    A[i + 1] = tmp;
                    tmp2 = B[i];
                    B[i] = B[i + 1];
                    B[i + 1] = tmp2;
                }

                if (A[i] == A[i + 1]) {
                    flag = false;
                }
            }

            stop--;
        }

        int[] C = new int[size];

        for (i = 0; i < size; i++) {
            C[B[i]] = i;
        }

        for (i = 0; i < size; i++) {
            B[i] = C[i];
        }

        return flag;
    }

    /**
     * Writes an AFNI file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeAfni(ModelImage image, FileWriteOptions options) {
        FileAfni afniFile;

        try { // Construct a new file object

            if (image.getNDims() < 3) {
                MipavUtil.displayError("Error! Image must have 3 or 4 dimensions");

                return false;
            }

            boolean loadB = false;
            boolean doRead = false;

            afniFile = new FileAfni(UI, options.getFileName(), options.getFileDirectory(), loadB, doRead);
            afniFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes an analyze file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeAnalyze(ModelImage image, FileWriteOptions options) {
        FileAnalyze analyzeFile;

        if (Preferences.is(Preferences.PREF_SAVE_XML_ON_HDR_SAVE)) {
            FileImageXML xmlFile;

            try {
                xmlFile = new FileImageXML(UI, options.getFileName(), options.getFileDirectory());

                String fBase, fName = options.getFileName();
                int index = fName.lastIndexOf(".");

                if (index != -1) {
                    fBase = fName.substring(0, index);
                } else {
                    fBase = fName.substring(0);
                }

                xmlFile.setRawExtension(".img");
                xmlFile.writeHeader(image, options, fBase, options.getFileDirectory(), false);
            } catch (IOException error) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                return false;
            } catch (OutOfMemoryError error) {

                if (!quiet) {
                    MipavUtil.displayError("FileIO: " + error);
                }

                return false;
            }
        }

        try { // Construct a new file object
            analyzeFile = new FileAnalyze(options.getFileName(), options.getFileDirectory());
            createProgressBar(analyzeFile,options.getFileName(), FILE_WRITE);
            analyzeFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes an AVI file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeAvi(ModelImage image, FileWriteOptions options) {

        String fileName = options.getFileName();

        FileAvi aviFile = null;

        try {
            aviFile = new FileAvi(UI, fileName, options.getFileDirectory());

            if (fileName.endsWith(".mov") || fileName.endsWith(".MOV")) {
                aviFile.setWriteQT(true);
            }

            aviFile.setMicroSecPerFrame(options.getMicroSecPerFrame());
            aviFile.setCompressionQuality(options.getMJPEGQuality());

            if (!aviFile.writeImage(image, options.getImageB(), options.getLUTa(), options.getLUTb(),
                                        options.getRGBTa(), options.getRGBTb(), options.getRed(), options.getGreen(),
                                        options.getBlue(), options.getOpacity(), options.getAlphaBlend(),
                                        options.getPaintBitmap(), options.getAVICompression())) {
                System.err.println("AVI write cancelled");
            }

            options.disposeLocal();

        } catch (IOException ex) {
            options.disposeLocal();

            return false;
        }

        return true;
    }


    /**
     * Writes a Freesurfer COR file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeCOR(ModelImage image, FileWriteOptions options) {
        FileCOR corFile;

        try { // Construct a new file object

            // Do not write COR files unless they are unsigned byte and
            // 256 cubed in dimension with all resolutions 1 millimeter.
            if (image.getType() != ModelStorageBase.UBYTE) {
                MipavUtil.displayError("Error! Data Type must be unsigned byte");

                return false;
            }

            if (image.getNDims() != 3) {
                MipavUtil.displayError("Error! Image must have three dimensions");

                return false;
            }

            if (image.getExtents()[0] != 256) {
                MipavUtil.displayError("Error! X dimension must be 256");

                return false;
            }

            if (image.getExtents()[1] != 256) {
                MipavUtil.displayError("Error! Y dimension must be 256");

                return false;
            }

            if ((options.getEndSlice() - options.getBeginSlice() + 1) != 256) {
                MipavUtil.displayError("Error! Z dimension must be 256");

                return false;
            }

            if ((image.getFileInfo(0).getResolutions()[0] != 1.0f) ||
                    ((image.getFileInfo(0).getUnitsOfMeasure()[0] != FileInfoBase.MILLIMETERS) &&
                         (image.getFileInfo(0).getUnitsOfMeasure()[0] != FileInfoBase.UNKNOWN_MEASURE))) {
                MipavUtil.displayError("Error! x resolution must be 1.0 millimeter");

                return false;
            }

            if ((image.getFileInfo(0).getResolutions()[1] != 1.0f) ||
                    ((image.getFileInfo(0).getUnitsOfMeasure()[1] != FileInfoBase.MILLIMETERS) &&
                         (image.getFileInfo(0).getUnitsOfMeasure()[1] != FileInfoBase.UNKNOWN_MEASURE))) {
                MipavUtil.displayError("Error! y resolution must be 1.0 millimeter");

                return false;
            }

            if ((image.getFileInfo(0).getResolutions()[2] != 1.0f) ||
                    ((image.getFileInfo(0).getUnitsOfMeasure()[2] != FileInfoBase.MILLIMETERS) &&
                         (image.getFileInfo(0).getUnitsOfMeasure()[2] != FileInfoBase.UNKNOWN_MEASURE))) {
                MipavUtil.displayError("Error! z resolution must be 1.0 millimeter");

                return false;
            }

            corFile = new FileCOR(UI, options.getFileName(), options.getFileDirectory());
            corFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes a DICOM file to store the image. Calls a dialog if the source isn't a DICOM image. DICOM images are each
     * written to a separate file with a different header.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeDicom(ModelImage image, FileWriteOptions options) {
        int i;
        int index;
        int[] extents;
        int imageSize;
        String prefix = "";
        String fileSuffix = "";
        FileInfoDicom myFileInfo = null;
        FileInfoBase[] originalFileInfos;
        FileDicom dicomFile;
        String fileDir = null;
        String fileName = null;

        // if a file is being 'saved as' a dicom file, then
        // actually save it to a subdirectory, named by the base of the FileName
        fileName = options.getFileName();

        if ( /* options.isSaveAs() && */options.isSaveInSubdirectory()) {
            String baseName = null;

            // find the root of the filename (without extensions)
            int ind = options.getFileName().lastIndexOf(".");

            if (ind > 0) {
                baseName = options.getFileName().substring(0, ind);
            } else {
                baseName = options.getFileName();
            } // there wasn't an extension

            // build directory name with the added subdirectory
            fileDir = new String(options.getFileDirectory() + baseName + File.separator);
        } else {

            // fileDir = image.getFileInfo(0).getFileDirectory();
            fileDir = options.getFileDirectory(); // options doesn't change...
        }

        // make sure fileDir exists
        File tmpFile = new File(fileDir);

        if (!tmpFile.exists()) {

            try {
                tmpFile.mkdirs();
                // don't reset here...may need for options to hold a root if we are saving into subdirs.
                // options.setFileDirectory(fileDir);
            } catch (Exception e) {
                MipavUtil.displayError("Unable to create directory for DICOM file: \n" + fileDir);

                return false;
            }
        }

        originalFileInfos = (FileInfoBase[]) (image.getFileInfo().clone());
        extents = image.getFileInfo(0).getExtents();

        if (image.isDicomImage()) {
            myFileInfo = (FileInfoDicom) image.getFileInfo(0);
            myFileInfo.setFileDirectory(fileDir);

            // if this is a 'save as' file, then correct the directory name in fileInfo
            // if (options.isSaveAs()) {
            // //myFileInfo.setFileDirectory (fileDir);
            // }
            if (image.isColorImage()) {
                myFileInfo.setValue("0028,0100", new Short((short) 8), 2);
                myFileInfo.setValue("0028,0101", new Short((short) 8), 2);
                myFileInfo.setValue("0028,0102", new Short((short) 7), 2);
                myFileInfo.setValue("0028,0002", new Short((short) 3), 2); // samples per pixel
                myFileInfo.setValue("0028,0004", new String("RGB")); // photometric
                myFileInfo.setValue("0028,0006", new Short((short) 0), 2); // planar Config
                myFileInfo.setValue("0002,0010", DICOM_Constants.UID_TransferLITTLEENDIANEXPLICIT);
            }

        } else { // Non DICOM images
            myFileInfo = new FileInfoDicom(options.getFileName(), fileDir, FileBase.DICOM);

            JDialogSaveDicom dialog = new JDialogSaveDicom(UI.getMainFrame(), image.getFileInfo(0), myFileInfo, options.isScript());

            if (dialog.isCancelled()) {
                return false;
            }

            // necessary to save floating point minc files to dicom
            if ((image.getFileInfo(0).getFileFormat() == FileBase.MINC) && (image.getType() == ModelImage.FLOAT)) {
                ModelImage newImage = (ModelImage) image.clone();

                // in-place conversion is required so that the minc file info is retained
                AlgorithmChangeType convertType = new AlgorithmChangeType(newImage, ModelImage.SHORT, newImage.getMin(),
                                                                          newImage.getMax(), newImage.getMin(),
                                                                          newImage.getMax(), false);
                convertType.setProgressBarVisible(false);
                convertType.run();

                image = newImage;
            }

            myFileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
            myFileInfo.setRescaleIntercept(0);
            myFileInfo.setRescaleSlope(1);
            myFileInfo.setValue("0002,0010", DICOM_Constants.UID_TransferLITTLEENDIANEXPLICIT);
            myFileInfo.vr_type = myFileInfo.EXPLICIT;

            if ((image.getType() == image.SHORT) || (image.getType() == image.USHORT)) {
                myFileInfo.setValue("0028,0100", new Short((short) 16), 2);
                myFileInfo.setValue("0028,0101", new Short((short) 16), 2);
                myFileInfo.setValue("0028,0102", new Short((short) 15), 2);
                myFileInfo.setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                myFileInfo.setValue("0028,0004", new String("MONOCHROME2"), 11); // photometric

                if (image.getType() == image.USHORT) {
                    myFileInfo.setValue("0028,0103", new Short((short) 0), 2);
                } else {
                    myFileInfo.setValue("0028,0103", new Short((short) 1), 2);
                }
            } else if ((image.getType() == image.BYTE) || (image.getType() == image.UBYTE)) {
                myFileInfo.setValue("0028,0100", new Short((short) 8), 2);
                myFileInfo.setValue("0028,0101", new Short((short) 8), 2);
                myFileInfo.setValue("0028,0102", new Short((short) 7), 2);
                myFileInfo.setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                myFileInfo.setValue("0028,0004", new String("MONOCHROME2")); // photometric

                if (image.getType() == image.UBYTE) {
                    myFileInfo.setValue("0028,0103", new Short((short) 0), 2);
                } else {
                    myFileInfo.setValue("0028,0103", new Short((short) 1), 2);
                }
            } else if (image.isColorImage()) {
                myFileInfo.setValue("0028,0100", new Short((short) 8), 2);
                myFileInfo.setValue("0028,0101", new Short((short) 8), 2);
                myFileInfo.setValue("0028,0102", new Short((short) 7), 2);
                myFileInfo.setValue("0028,0002", new Short((short) 3), 2); // samples per pixel
                myFileInfo.setValue("0028,0004", new String("RGB")); // photometric
                myFileInfo.setValue("0028,0006", new Short((short) 0), 2); // planar Config
            } else {

                if (!quiet) {
                    MipavUtil.displayError("Saving the original image in DICOM format is not yet supported.");
                }

                System.gc();

                return false;
            }

            myFileInfo.setDataType(image.getType());

            Object obj = null;
            float slLoc = 0;
            float xLocOrig = 0;
            float yLocOrig = 0;
            float zLocOrig = 0;
            float xLoc = 0;
            float yLoc = 0;
            float zLoc = 0;
            String [] origins;

            FileDicomTag tag = myFileInfo.getTag("0020,1041");

            if (tag != null) {
                obj = tag.getValue(false);
            }
            if (obj != null) {
                slLoc = Float.valueOf((String)obj).floatValue();
            }

            float sliceResolution = myFileInfo.getResolutions()[2];

            if (image.getExtents().length > 2) { // This sets the fileinfo to the same for all slices !!
                FileInfoBase[] fBase = new FileInfoBase[image.getExtents()[2]];

                if ( myFileInfo.getTag("0020,0032") != null) {
                    tag = myFileInfo.getTag("0020,0032");
                    if (tag != null) {
                        obj = tag.getValue(false);
                    }
                    if (obj != null) {
                        origins = ((String)(obj)).split("\\\\");
                        xLoc = Float.valueOf(origins[0]).floatValue();
                        yLoc = Float.valueOf(origins[1]).floatValue();
                        zLoc = Float.valueOf(origins[2]).floatValue();
                    }
                    else {
                        xLoc = 0;
                        yLoc = 0;
                        zLoc = 0;
                    }

                    xLocOrig = xLoc;
                    yLocOrig = yLoc;
                    zLocOrig = zLoc;
                }

                // see if the original dicom a minc was created from was part of a larger volume.  if so, preserve the instance number it had
                int baseInstanceNumber = -1;
                if (image.getFileInfo(0).getFileFormat() == FileBase.MINC) {
                    tag = myFileInfo.getTag("0020,0013");
                    if (tag != null) {
                        obj = tag.getValue(false);
                    }
                    if (obj != null) {
                        baseInstanceNumber = Integer.parseInt(((String)obj).trim());
                        options.setRecalculateInstanceNumber(false);
                    }
                }

                for (int k = 0; k < image.getExtents()[2]; k++) {

                    // System.err.println("FileIO k = " + k);
                    fBase[k] = (FileInfoBase) myFileInfo.clone();

                    // Add code to modify the slice location attribute (0020, 1041) VR = DS = decimal string
                    ((FileInfoDicom)(fBase[k])).setValue("0020,1041", Float.toString(slLoc), Float.toString(slLoc).length());
                    slLoc += sliceResolution;

                    String tmpStr = new String (Float.toString(xLoc) + "\\" +
                            Float.toString(yLoc) + "\\" +
                            Float.toString(zLoc) );
                    ((FileInfoDicom)(fBase[k])).setValue("0020,0032", tmpStr, tmpStr.length());

                    if( image.getFileInfo()[0].getImageOrientation() == FileInfoBase.AXIAL ){
                        if (zLocOrig <= 0 ){
                            zLoc += sliceResolution;
                        }
                        else {
                            zLoc -= sliceResolution;
                        }
                    }
                    else if (image.getFileInfo()[0].getImageOrientation() == FileInfoBase.CORONAL){
                        if (yLocOrig <= 0 ){
                            yLoc += sliceResolution;
                        }
                        else {
                            yLoc -= sliceResolution;
                        }
                    }
                    else if (image.getFileInfo()[0].getImageOrientation() == FileInfoBase.SAGITTAL){
                        if (xLocOrig <= 0 ){
                            xLoc += sliceResolution;
                        }
                        else {
                            xLoc -= sliceResolution;
                        }
                    }
                    else {
                        if (zLoc <= 0 ){
                            zLoc += sliceResolution;
                        }
                        else {
                            zLoc -= sliceResolution;
                        }
                    }

                    if (baseInstanceNumber != -1) {
                        String instanceStr = "" + (baseInstanceNumber + k);
                        ((FileInfoDicom)(fBase[k])).setValue("0020,0013", instanceStr, instanceStr.length());
                    }
                }
                image.setFileInfo(fBase);
            } else {
                image.setFileInfo(myFileInfo, 0);
            }
        }

        imageSize = extents[0] * extents[1];

        createProgressBar(null,options.getFileName(), FILE_WRITE);
       
        if (options.isSaveAs()) {
            index = options.getFileName().indexOf(".");
            prefix = options.getFileName().substring(0, index); // Used for setting file name
            fileSuffix = options.getFileName().substring(index);
        }

        try {
            String name;

            if (!((FileInfoDicom) (myFileInfo)).isMultiFrame()) {

                for (i = options.getBeginSlice(); i <= options.getEndSlice(); i++) {
                    progressBar.updateValue(Math.round((float) i / (options.getEndSlice()) * 100), false);
                    myFileInfo = (FileInfoDicom) image.getFileInfo(i);
                    myFileInfo.setFileDirectory(fileDir); // need to update in case it changed

                    String s = "" + (i + 1);

                    if (options.isInstanceNumberRecalculated()) {
                        myFileInfo.setValue("0020,0013", s, s.length());
                    }

                    if (options.isSaveAs()) {

                        if ((i < 9) && (options.getEndSlice() != options.getBeginSlice())) {
                            name = prefix + "000" + (i + 1) + fileSuffix;
                        } else if ((i >= 9) && (i < 99) && (options.getEndSlice() != options.getBeginSlice())) {
                            name = prefix + "00" + (i + 1) + fileSuffix;
                        } else if ((i >= 99) && (i < 999) && (options.getEndSlice() != options.getBeginSlice())) {
                            name = prefix + "0" + (i + 1) + fileSuffix;
                        } else if (options.getEndSlice() != options.getBeginSlice()) {
                            name = prefix + (i + 1) + fileSuffix;
                        } else {
                            name = prefix + fileSuffix;
                        }
                    } else {
                        name = myFileInfo.getFileName();
                    }

                    dicomFile = new FileDicom(name, fileDir);
                    dicomFile.writeImage(image, i * imageSize, (i * imageSize) + imageSize, i);
                }
            } else { // its a multi frame image to be saved!!!

                // progressBar.updateValue( Math.round((float)i/(endSlice) * 100));
                dicomFile = new FileDicom(fileName, fileDir); // was (UI, fileDir, fileDir).  think this fixes...

                // String s=""+(i+1);
                // myFileInfo = (FileInfoDicom)image.getFileInfo(i);
                // if (saveAs) myFileInfo.updateValue("0020,0013", s, s.length());
                dicomFile.writeMultiFrameImage(image, options.getBeginSlice(), options.getEndSlice());
            }

            
        } catch (IOException error) {
            image.setFileInfo(originalFileInfos);

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            

            return false;
        } catch (OutOfMemoryError error) {
            image.setFileInfo(originalFileInfos);

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            

            return false;
        }

        image.setFileInfo(originalFileInfos);

        return true;
    }

    /**
     * Writes a MGH or MGZ file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeMGH(ModelImage image, FileWriteOptions options) {
        FileMGH mghFile;

        try { // Construct a new file object
            mghFile = new FileMGH(UI, options.getFileName(), options.getFileDirectory());
            createProgressBar(mghFile, options.getFileName(), FILE_WRITE);
            mghFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes a Fits file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeFits(ModelImage image, FileWriteOptions options) {
        FileFits fitsFile;

        try { // Construct a new file object
            fitsFile = new FileFits(UI, options.getFileName(), options.getFileDirectory());
            createProgressBar(fitsFile, options.getFileName(), FILE_WRITE);
            fitsFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes an ICS file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeICS(ModelImage image, FileWriteOptions options) {
        FileICS ICSFile;

        try { // Construct a new file object

            ICSFile = new FileICS(UI, options.getFileName(), options.getFileDirectory());
            createProgressBar(ICSFile, fileName, FILE_WRITE);
            ICSFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes an Interfile file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeInterfile(ModelImage image, FileWriteOptions options) {
        FileInterfile interfileFile;

        try { // Construct a new file object
            interfileFile = new FileInterfile(UI, options.getFileName(), options.getFileDirectory());
            createProgressBar(interfileFile, fileName, FILE_WRITE);
            interfileFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes a JIMI file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeJimi(ModelImage image, FileWriteOptions options) {
        int index = options.getFileName().indexOf(".");
        String prefix = options.getFileName().substring(0, index); // Used for setting file name
        String fileSuffix = options.getFileName().substring(index);
        int slice = ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().getSlice();
        String name;

        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();

        for (int i = beginSlice; i <= endSlice; i++) {

            ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().show(0,i,true);
            Image im = ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().getImage();

            if ((i < 9) && (endSlice != beginSlice)) {
                name = options.getFileDirectory() + prefix + "00" + (i + 1) + fileSuffix;
            } else if ((i >= 9) && (i < 99) && (endSlice != beginSlice)) {
                name = options.getFileDirectory() + prefix + "0" + (i + 1) + fileSuffix;
            } else if (endSlice != beginSlice) {
                name = options.getFileDirectory() + prefix + (i + 1) + fileSuffix;
            } else {
                name = options.getFileDirectory() + prefix + fileSuffix;
            }

            try {
                Jimi.putImage(im, name);
            } catch (JimiException jimiException) {
                Preferences.debug("JIMI write error: " + jimiException + "\n");

                return false;
            }
        }

        ((ViewJFrameImage) (image.getImageFrameVector().firstElement())).getComponentImage().show(0, slice, null, null,
                                                                                                  true, -1);

        return true;
    }

    /**
     * Writes a Minc file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeMinc(ModelImage image, FileWriteOptions options) {
        FileMinc mincFile;
        FileInfoBase fileInfo;

        if (image.getNDims() != 3) {
            MipavUtil.displayError("FileIO: MINC writer only writes 3D images.");
            return false;
        }

        try { // Construct a new file object

            if (options.isSaveAs() && !options.isSet()) {
                fileInfo = image.getFileInfo(0);
                fileInfo.setExtents(image.getExtents());

                JDialogSaveMinc dialog = new JDialogSaveMinc(UI.getMainFrame(), fileInfo, options);

                dialog.setVisible(true);

                if (dialog.isCancelled()) {
                    return false;
                }

                options = dialog.getOptions();
            }

            mincFile = new FileMinc(options.getFileName(), options.getFileDirectory());
            createProgressBar(mincFile, fileName, FILE_READ);
            mincFile.writeImage(image, options);

            return true;

        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
                Preferences.debug(error.getMessage() + "\n", Preferences.DEBUG_FILEIO);
                for (int i = 0; i < error.getStackTrace().length; i++) {
                    Preferences.debug("\t" + error.getStackTrace()[i] + "\n", Preferences.DEBUG_FILEIO);
                }
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }
    }

    /**
     * Writes a MRC file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeMRC(ModelImage image, FileWriteOptions options) {
        FileMRC mrcFile;

        try { // Construct a new file object
            mrcFile = new FileMRC(UI, options.getFileName(), options.getFileDirectory());
            createProgressBar(mrcFile, options.getFileName(), FILE_WRITE);
            mrcFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes a NIFTI file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeNIFTI(ModelImage image, FileWriteOptions options) {
        FileNIFTI NIFTIFile;

        try { // Construct a new file object
            NIFTIFile = new FileNIFTI(UI, options.getFileName(), options.getFileDirectory());
            createProgressBar(NIFTIFile, options.getFileName(), FILE_WRITE);
            NIFTIFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes an OSM file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeOSM(ModelImage image, FileWriteOptions options) {
        FileOSM osmFile;

        try { // Construct a new file object
            osmFile = new FileOSM(UI, options.getFileName(), options.getFileDirectory());
            createProgressBar(osmFile, fileName, FILE_READ);
            osmFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes a raw file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeRaw(ModelImage image, FileWriteOptions options) {
        FileRaw rawFile;
        FileInfoImageXML fileInfo;

        try { // Construct new file info and file objects
            fileInfo = new FileInfoImageXML(options.getFileName(), options.getFileDirectory(), FileBase.RAW);
            rawFile = new FileRaw(options.getFileName(), options.getFileDirectory(), fileInfo, FileBase.READ_WRITE);
            createProgressBar(rawFile, options.getFileName(), FILE_WRITE);
            rawFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes a SPM file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeSPM(ModelImage image, FileWriteOptions options) {
        FileSPM spmFile;

        try { // Construct a new file object
            spmFile = new FileSPM(UI, options.getFileName(), options.getFileDirectory());
            createProgressBar(spmFile, options.getFileName(), FILE_WRITE);
            spmFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes a TIFF file to store the image.
     *
     * @param   image    The image to write.
     * @param   options  The options to use to write the image.
     *
     * @return  Flag indicating that this was a successful write.
     */
    private boolean writeTiff(ModelImage image, FileWriteOptions options) {
        FileTiff imageFile;
        int[] extents;

        try { // Construct a new file object
            imageFile = new FileTiff(UI, options.getFileName(), options.getFileDirectory());
            createProgressBar(imageFile, options.getFileName(), FILE_WRITE);
            if (LUT == null) {
                extents = new int[2];
                extents[0] = 4;
                extents[1] = 256;
                ((FileTiff) imageFile).writeImage(image, new ModelLUT(1, 256, extents), options);
            } else {
                ((FileTiff) imageFile).writeImage(image, LUT, options);
            }
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    /**
     * Writes the image in our MIPAV XML format.
     *
     * @param   image    The image to be saved to the file.
     * @param   options  Write options that control aspects of writing the image.
     *
     * @return  True if the file was successfully saved to a file.
     */
    private boolean writeXML(ModelImage image, FileWriteOptions options) {
        FileImageXML xmlFile;

        try {
            xmlFile = new FileImageXML(UI, options.getFileName(), options.getFileDirectory());
            createProgressBar(xmlFile, options.getFileName(), FILE_WRITE);
            /**
             * Set the LUT (for grayscale) and ModelRGB (for color) doesn't matter if either is null
             */
            xmlFile.setModelLUT(LUT);

            if (image.getFileInfo()[0] instanceof FileInfoDicom) {

                if (!dataConversion(((FileInfoDicom) image.getFileInfo()[0]), xmlFile.getFileInfo())) {
                    return false;
                }

                xmlFile.setAdditionalSets(xmlFile.getFileInfo().getPSetHashtable().elements());
            } else if (image.getFileInfo()[0] instanceof FileInfoLSM) {
                LSMDataConversion(((FileInfoLSM) image.getFileInfo()[0]), xmlFile.getFileInfo());
                xmlFile.setAdditionalSets(xmlFile.getFileInfo().getPSetHashtable().elements());
            }

            xmlFile.writeImage(image, options);
        } catch (IOException error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        } catch (OutOfMemoryError error) {

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return false;
        }

        return true;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Orientation information held by orientation.
     */
    public class OrientStatus {

        /** DOCUMENT ME! */
        int index;

        /** DOCUMENT ME! */
        float location;

        /**
         * Creates an Orientation Status with a given index and location.
         *
         * @param  ind  DOCUMENT ME!
         * @param  loc  DOCUMENT ME!
         */
        public OrientStatus(int ind, float loc) {
            index = ind;
            location = loc;
        }

        /**
         * Determines if the given orientation is the same as the current one.
         *
         * @param   o  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public boolean equals(Object o) {

            if (o instanceof OrientStatus) {

                if (((OrientStatus) o).getLocation() == location) {
                    return true;
                }
            } else {
                return false;
            }

            return false;
        }

        /**
         * Provides the index.
         *
         * @return  DOCUMENT ME!
         */
        public int getIndex() {
            return index;
        }

        /**
         * Provides the value of the location.
         *
         * @return  DOCUMENT ME!
         */
        public float getLocation() {
            return location;
        }
    }

}
