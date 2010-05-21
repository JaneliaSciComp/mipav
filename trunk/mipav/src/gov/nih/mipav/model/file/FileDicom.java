package gov.nih.mipav.model.file;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.dicomcomm.DICOM_Constants;
import gov.nih.mipav.model.file.rawjp2.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogDicomDir;

import java.awt.image.*;
import java.io.*;
import java.util.*;

import javax.imageio.ImageIO;

import jj2000.j2k.encoder.Encoder;
import jj2000.j2k.util.ParameterList;


/**
 * This class reads and writes DICOM files. The DICOM file format consists of header information marked by tags, with
 * the pixel data as the last tag. Each tag has a length field that contains the length of the tag, so it is possible to
 * skip over tags that are unrecognized. This class is entirely based on DICOM version 3.0 published by ACR/NEMA and so
 * tests for all the tags in the Data Dictionary (DICOM v. 3.0 Part 6). It stores these in a table found in
 * FileInfoDicom.
 * 
 * <p>
 * The Hashtable is based upon the default DICOM dictionary which contains all possible standard DICOM tags. It also
 * contains many private tags that are commented out. If the user wishes a specific private tag to be recognized by this
 * program, he or she should edit the dictionary file. The tag will then be displayed as any other standard tag would be
 * displayed. Otherwise, all tags are read in, but if their value representation is unrecognized (only the case with
 * tags not defined in dictionary file) their value is stored as a string. When FileInfoDicom displays the tag
 * information, it shows the name of the tag as private and the value as the string. The string may contain valid data
 * or it may contain junk. There is no way of knowing how to properly read in a private tag without a valid value
 * representation (VR). So if the user wishes to know private tag information, he or she should specify the proper VR in
 * the dictionary file and be sure that their file conforms to that VR.
 * </p>
 * 
 * @version 1.0 Aug 1, 1999
 * @see FileIO
 * @see FileInfoDicom
 * @see FileRaw
 * @see FileRawChunk
 */
public class FileDicom extends FileDicomBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** The tag marking the start of the image data. */
    public static final String IMAGE_TAG = "7FE0,0010";

    /** The tag marking the beginning of a dicom sequence. */
    public static final String SEQ_ITEM_BEGIN = "FFFE,E000";

    /** The tag marking the end of a dicom sequence. */
    public static final String SEQ_ITEM_END = "FFFE,E00D";

    /** The tag marking the end of an undefined length dicom sequence. */
    public static final String SEQ_ITEM_UNDEF_END = "FFFE,E0DD";

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Length of the value field of data element. */
    private int elementLength;

    /** Second number (DICOM element in a group) in ordered pair of numbers that uniquely identifies a data element. */
    private int elementWord;

    /**
     * When Dicom image data is 'encapsulated,' it may be in pieces, or 'fragments.' don't know quite why, or if pieces
     * should be kept together. If in fragments, the image data may span several slices, called a 'frame.'
     */
    private boolean encapsulated = false;

    private boolean encapsulatedJP2 = false;

    /** If file is a DICOMDIR this is false * */
    private boolean notDir = true;

    /** Directory of the image file. */
    private String fileDir;

    /** File object of the image. */
    private File fileHeader;

    /** Meta data structure in which to save all the DICOM tags. */
    private FileInfoDicom fileInfo;

    /** Name of the file to be read in. */
    private String fileName;

    /** Location of first element. */
    private final int FIRST_ELEMENT = 132;

    /** First number (DICOM group) in ordered pair of numbers that uniquely identifies a data element. */
    private int groupWord;

    /** True if the DICOM image header has been read. */
    private boolean hasHeaderBeenRead = false;

    /** Location of 'DICM'. */
    private final int ID_OFFSET = 128;

    /** Illegal element length. */
    private final int ILLEGAL_LENGTH = -1;

    /** Reference to the image read into the application. */
    private ModelImage image;

    /** True if in a sequence tag. */
    private boolean inSQ = false;

    private FileDicomTagTable tagTable;

    /** Holds sequence of files described in DICOMDIR * */
    private FileDicomSQ dirInfo;

    /** Buffer used when reading in encapsulated JPEG images. */
    private int[] jpegData = null;

    /** JPEG compression may be lossy or lossless. */
    private boolean lossy = false;

    /** Reference to the LUT (if one is stored with the image). */
    private ModelLUT lut;

    /**
     * Number of bytes following this File Meta Element (end of the Value field) up to and including the last File Meta
     * Element of the Group 2 File Meta Information.
     * 
     * <p>
     * See DICOM3 part 10 pages 12-14 (1988).
     * </p>
     */
    private int metaGroupLength = 0;

    /** Name of the sequance tag. */
    private String nameSQ = "";

    /**
     * If the file is <i>quiet</i> no user-interaction is performed. Useful for determining whether or not to display
     * the MipavUtil.displayError() is to be called. This allows the option of leaving the user-interaction and
     * notification of an error to occur here or to be handled somewhere else (as in, when an IOException is thrown but
     * we'd prefer to notify the user once, rather than for each exception.) based on programming preferences or
     * user-debug preference settings.
     */
    private boolean quiet = false;

    /** DOCUMENT ME! */
    private FileRaw rawFile;

    /** Undefined element length. */
    private final int UNDEFINED_LENGTH = -2;

    /** Value Representation - see FileInfoDicom. */
    private byte[] vr = new byte[2];

    /** Whether MIPAV should be written to this dicom file as the secondary stamp * */
    private boolean stampSecondary = true;

    private boolean isEnhanced = true;

    private FileDicomTagTable[] childrenTagTables;

    private FileInfoDicom[] enhancedFileInfos;

    private boolean isEnhanced4D = false;

    private int enhancedNumSlices;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * DICOM reader/writer constructor. Creates the access files and ensures that the files are opened for read-write
     * access; it will fall back to read-only if read-write access to the image file is not granted. The file info gets
     * the DICOM dictionary set, as well as the endianess property is set. The image itself is not read.
     * 
     * @param fDirPlusName full file name (with directory)
     * 
     * @exception IOException if there is an error constructing the files.
     */
    public FileDicom(final String fDirPlusName) throws IOException {

        try {
            fileHeader = new File(fDirPlusName);

            if (fileHeader == null) {
                throw new FileNotFoundException();
            }

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (final IOException ex) {}
            }

            try {
                raFile = new RandomAccessFile(fileHeader, "r");
            } catch (final IOException e) {

                // raFile = new RandomAccessFile(fileHeader, "r");
            }

            fileInfo = new FileInfoDicom(null, null, FileUtility.DICOM);
            fileInfo.setEndianess(FileDicomBase.LITTLE_ENDIAN);

        } catch (final NullPointerException npe) {
            npe.printStackTrace();
        } catch (final OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in FileDicom constructor.");
            throw new IOException();
        }
    }

    /**
     * DICOM reader/writer constructor. Creates the access files and ensures that the files are opened for read-write
     * access; it will fall back to read-only if read-write access to the image file is not granted. The file info gets
     * the DICOM dictionary set, as well as the endianess property is set. The image itself is not read.
     * 
     * @param fName File name.
     * @param fDir File directory.
     * 
     * @exception IOException if there is an error constructing the files.
     */
    public FileDicom(final String fName, final String fDir) throws IOException {
        fileName = fName;
        fileDir = fDir;

        try {
            fileHeader = new File(fileDir + fileName);

            if (fileHeader == null) {
                throw new FileNotFoundException();
            }

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (final IOException ex) {}
            }

            try {
                raFile = new RandomAccessFile(fileHeader, "rw");
            } catch (final IOException e) {
                raFile = new RandomAccessFile(fileHeader, "r");
            }

            fileInfo = new FileInfoDicom(fileName, fileDir, FileUtility.DICOM);
            fileInfo.setEndianess(FileDicomBase.LITTLE_ENDIAN);
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
        } catch (final NullPointerException npe) {
            npe.printStackTrace();
        } catch (final OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in FileDicom constructor.");
            throw new IOException();
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Closes random access file associated with this object.
     * 
     * @throws IOException DOCUMENT ME!
     */
    public void close() throws IOException {

        // System.out.println("FileDICOM.close");
        this.finalize();
    }

    /**
     * Sets whether MIPAV will edit the DICOM tags of this image with a secondary stamp. This occurs when the image is
     * being saved.
     */

    public void doStampSecondary(final boolean stampSecondary) {
        this.stampSecondary = stampSecondary;
    }

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>. Exceptions which occur while this method runs (for
     * instance, the possibility of getting a <code>IOException</code> when closing the image file) is ignored
     * quietly.
     */
    public void finalize() {
        fileName = null;
        fileDir = null;
        fileHeader = null;

        fileInfo = null;
        image = null;
        vr = null;

        if (rawFile != null) {

            try {
                rawFile.close();
            } catch (final IOException ex) {
                // closing.. ignore errors
            }

            rawFile.finalize();
        }

        if (raFile != null) {

            try {
                raFile.close();
            } catch (final IOException ex) {
                // closing.. ignore errors
            }

            raFile = null;
        }

        rawFile = null;
        nameSQ = null;
        jpegData = null;

        try {
            super.finalize();
        } catch (final Throwable er) {
            // ignore errors during memory cleanup..
        }
    }

    /**
     * Accessor that returns the file info.
     * 
     * @return Structure containing the file info.
     */
    public final FileInfoBase getFileInfo() {
        return fileInfo;
    }

    /**
     * Accessor that returns the DICOMDIR sequence file info.
     * 
     * @return Structure containing the file info.
     */
    public final FileDicomSQ getDirFileInfo() {
        return dirInfo;
    }

    /**
     * returns the ModelLUT, if there, which might be specified by the <code>0028,1201</code>, <code>0028,1202</code>,
     * <code>0028,1203</code> tags.
     * 
     * @return DOCUMENT ME!
     */
    public final ModelLUT getLUT() {
        return lut;
    }

    /**
     * Looks for the DICM _tag_ in the File header. If present, the image is DICOM 3.0 format.
     * 
     * @throws IOException Indicates error reading the file
     * 
     * @return boolean true if the DICM tag was found in the image header.
     */
    public boolean isDICOM() throws IOException {

        if (raFile == null) {
            return false;
        }

        if (raFile.length() <= ID_OFFSET) {
            return false;
        }

        if (isDir()) {
            return false;
        }

        final long fPtr = raFile.getFilePointer();
        raFile.seek(ID_OFFSET); // Find "DICM" tag

        // In v. 3.0, within the ID_OFFSET is general header information that
        // is not encoded into data elements and not present in DICOM v. 2.0.
        // However, it is optional.

        if ( !getStringFromFile(4).equals("DICM")) {
            fileInfo.containsDICM = false;
            raFile.seek(0); // set file pointer to zero
        } else {
            fileInfo.containsDICM = true;
            raFile.seek(fPtr); // set file pointer back to original position
        }

        return (fileInfo.containsDICM);
    }

    /**
     * Dicom version 2: Does not have a preamble in which to search for "DICM" So...the solution will be to search that
     * it has at least a couple of beginning "08" tags However, we do not know if its Little Endian or Big Endian..so
     * need to handle both
     * 
     * @return
     * @throws IOException
     */
    public boolean isDICOM_ver2() throws IOException {
        if (raFile == null) {
            return false;
        }
        raFile.seek(0);
        boolean endianess = FileBase.LITTLE_ENDIAN;

        // read the group of the first tag at the start of the file
        final byte[] groupWord = new byte[2];
        raFile.read(groupWord);

        // 0008 seems to always be implicit, 0002 might or might not -- we have to check
        boolean checkForVR = false;

        // look for one 0008 or 0002 tag. if found, we still need another one to be sure
        if (groupWord[0] == 8 && groupWord[1] == 0) {
            endianess = FileBase.LITTLE_ENDIAN;
        } else if (groupWord[0] == 0 && groupWord[1] == 8) {
            endianess = FileBase.BIG_ENDIAN;
        } else if (groupWord[0] == 2 && groupWord[1] == 0) {
            endianess = FileBase.LITTLE_ENDIAN;
            checkForVR = true;
        } else if (groupWord[0] == 0 && groupWord[1] == 2) {
            endianess = FileBase.BIG_ENDIAN;
            checkForVR = true;
        } else {
            // definitely not dicom 2....since the first tag is not 08 or 02
            return false;
        }

        // default to 4 bytes of length, may be changed to 2 below
        int numLengthBytes = 4;

        if (checkForVR) {
            // read the tag element and create the key -- used to pull the expected VR from the DicomDictionary
            FileDicomKey key;
            final byte[] elementWord = new byte[2];
            raFile.read(elementWord);
            if (endianess == FileBase.LITTLE_ENDIAN) {
                final int group = ( (groupWord[1] & 0xff) << 8) | (groupWord[0] & 0xff);
                final int element = ( (elementWord[1] & 0xff) << 8) | (elementWord[0] & 0xff);

                key = new FileDicomKey(group, element);
            } else {
                final int group = ( (groupWord[0] & 0xff) << 8) | (groupWord[1] & 0xff);
                final int element = ( (elementWord[0] & 0xff) << 8) | (elementWord[1] & 0xff);

                key = new FileDicomKey(group, element);
            }

            // save the file position before reading the VR, in case we have to go back
            final long prevPos = raFile.getFilePointer();

            final byte[] vr = new byte[2];
            raFile.read(vr);
            final String vrString = new String(vr);
            if ( !vrString.equals(DicomDictionary.getVR(key))) {
                // not the proper VR, go back and see if it's an implicit VR with 4 byte length
                raFile.seek(prevPos);
            } else {
                // the VR string matches the expected one for this tag
                if (vrString.equals("OB") || vrString.equals("OW") || vrString.equals("SQ") || vrString.equals("UN")
                        || vrString.equals("UT")) {
                    // VR = 'OB', or 'OW' or 'SQ' or 'UN' or 'UT' ==> always should use 4 byte length
                } else {
                    // all other VRs (such as UI) are assumed to use 2 byte length
                    numLengthBytes = 2;
                }
            }
        } else {
            // skip the tag element -- assume 4 byte length
            raFile.skipBytes(2);
        }

        // read in length...4 bytes long
        final byte[] lengthBuffer = new byte[numLengthBytes];
        raFile.read(lengthBuffer);
        int skipLength;
        if (numLengthBytes == 2) {
            if (endianess == FileBase.LITTLE_ENDIAN) {
                skipLength = ( (lengthBuffer[1] & 0xff) << 8) | (lengthBuffer[0] & 0xff);
            } else {
                skipLength = ( (lengthBuffer[0] & 0xff) << 8) | (lengthBuffer[1] & 0xff);
            }
        } else {
            // 4 bytes of length
            if (endianess == FileBase.LITTLE_ENDIAN) {
                skipLength = ( (lengthBuffer[3] & 0xff) << 24) | ( (lengthBuffer[2] & 0xff) << 16)
                        | ( (lengthBuffer[1] & 0xff) << 8) | (lengthBuffer[0] & 0xff);
            } else {
                skipLength = ( (lengthBuffer[0] & 0xff) << 24) | ( (lengthBuffer[1] & 0xff) << 16)
                        | ( (lengthBuffer[2] & 0xff) << 8) | (lengthBuffer[3] & 0xff);
            }
        }

        // skip over the length
        final int numBytesSkipped = raFile.skipBytes(skipLength);

        // make sure that we haven't skipped past the end of the file (probably due to bad length)
        if (numBytesSkipped < skipLength) {
            return false;
        }

        // look for the start of a second 0008 or 0002 tag
        raFile.read(groupWord);
        if (groupWord[0] == 8 && groupWord[1] == 0) {
            return true;
        } else if (groupWord[0] == 0 && groupWord[1] == 8) {
            return true;
        } else if (groupWord[0] == 2 && groupWord[1] == 0) {
            return true;
        } else if (groupWord[0] == 0 && groupWord[1] == 2) {
            return true;
        } else {
            // definitely not dicom 2....since the first tag is not 08 or 02
            return false;
        }
    }

    /**
     * gets the quiet option on the class.
     * 
     * <p>
     * The idea is that for all output messages, there should be the option to not bother the user with the message.
     * Ie., if an exception is thrown, and we normally tell the user that an error occurred, a calling class can set
     * this option so that the calling class can handle (either loudly or quietly, as needed) the error with its own
     * message. this could be upgraded to call with quiet to prevent user-queries from interrupting an automatic
     * process. But that is in the future.
     * </p>
     * 
     * <p>
     * Note: In the future, this method and variable is to be moved to FileBase.
     * </p>
     * 
     * @return whether this class should consider itself quiet, and by internal inspection, not notify the user. <code>
     *          True</code>
     *         is to consider itself to not notify the user. <code>False</code> is to notify the user, and is the
     *         default behaviour.
     */
    public final boolean isQuiet() {
        return quiet;
    }

    /**
     * 
     * @return true if file is a DICOMDIR
     */
    public final boolean isDir() {
        return !notDir;
    }

    /**
     * Reads in all the tags available in the file and stores them in the Hashtable in FileInfoDicom. This method
     * handles the various tags that are present at the beginning of a DICOM image file. It also sets the important File
     * Info variables based on what it finds.
     * 
     * <p>
     * The method will return with a failure code if it finds the tag &quot;0000,0000&quot; or it mis-reads the header
     * and starts reading from an odd byte.
     * </p>
     * 
     * <p>
     * As the reader runs through the tags in the header, it reads them based on the type. There are 7 types:
     * </p>
     * 
     * <ul>
     * <li>typeString</li>
     * <li>typeShort</li>
     * <li>typeInt</li>
     * <li>typeFloat</li>
     * <li>typeDouble</li>
     * <li>typeSequence</li>
     * <li>typeUnknown</li>
     * </ul>
     * 
     * <p>
     * Any special handling based on type occurs for each tag (@see FileInfoDicom), then it is added to the DICOM tags
     * table. Each tag is checked against a small list of individual tags, as some tags have an effect on the way the
     * following tags are interpreted.
     * </p>
     * 
     * <p>
     * This method also affects some of the properties of the FileInfoDicom.
     * </p>
     * 
     * <ul>
     * <li>MetaGroupLength</li>
     * <li>Units of Measure</li>
     * <li>Transfer Syntax</li>
     * <li>Extents</li>
     * <li>Color Pallete for each color channel</li>
     * </ul>
     * 
     * <p>
     * Display type changes the modality; image length is then also recalculated.
     * </p>
     * 
     * @return <code>true</code> if successful, otherwise <code>false</code>.
     * 
     * @exception IOException if there is an error reading the file
     * 
     * @see FileInfoDicom
     * @see #getNextElement(boolean)
     * @see #convertGroupElement(int, int)
     */
    public boolean readHeader(final boolean loadTagBuffer) throws IOException {
        int[] extents;
        String type, // type of data; there are 7, see FileInfoDicom
        name; // string representing the tag

        boolean endianess = FileBase.LITTLE_ENDIAN; // all DICOM files start as little endian (tags 0002)
        boolean flag = true;

        if (loadTagBuffer == true) {
            loadTagBuffer();
        }

        String strValue = null;
        Object data = null;

        try {
            extents = new int[2];
        } catch (final OutOfMemoryError e) {

            if ( !isQuiet()) {
                MipavUtil.displayError("Out of memory in FileDicom.readHeader");
                Preferences.debug("Out of memory in FileDicom.readHeader\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("Out of memory in FileDicom.readHeader\n", Preferences.DEBUG_FILEIO);
            }

            throw new IOException("Out of Memory in FileDicom.readHeader");
        }

        metaGroupLength = 0;
        elementLength = 0;
        fileInfo.setEndianess(endianess);

        skipBytes(ID_OFFSET); // Find "DICM" tag

        // In v. 3.0, within the ID_OFFSET is general header information that
        // is not encoded into data elements and not present in DICOM v. 2.0.
        // However, it is optional.

        if ( !getString(4).equals("DICM")) {
            fileInfo.containsDICM = false;
            seek(0); // set file pointer to zero
        }

        fileInfo.setDataType(ModelStorageBase.SHORT); // Default file type

        tagTable = fileInfo.getTagTable();

        while (flag == true) {

            if (fileInfo.containsDICM) {

                // endianess is defined in a tag and set here, after the transfer
                // syntax group has been read in
                if (getFilePointer() >= (ID_OFFSET + 4 + metaGroupLength)) {
                    endianess = fileInfo.getEndianess();
                    // Preferences.debug("endianess = " + endianess + "\n", Preferences.DEBUG_FILEIO);
                }
            } else {

                if (getFilePointer() >= metaGroupLength) {
                    endianess = fileInfo.getEndianess();
                }
            }

            // ******* Gets the next element
            getNextElement(endianess); // gets group, element, length
            name = convertGroupElement(groupWord, elementWord);

            final FileDicomKey key = new FileDicomKey(name);
            int tagVM;
            // Should be removed
            // final int dirLength;

            // Preferences.debug("group = " + groupWord + " element = " + elementWord + " length = " +
            // elementLength + "\n", Preferences.DEBUG_FILEIO);

            if ( (fileInfo.vr_type == FileInfoDicom.IMPLICIT) || (groupWord == 2)) {

                // implicit VR means VR is based on tag as defined in dictionary
                type = DicomDictionary.getType(key);
                tagVM = DicomDictionary.getVM(key);

                // the tag was not found in the dictionary..
                if (type == null) {
                    type = FileDicomBase.TYPE_UNKNOWN;
                    tagVM = 0;
                }
            } else { // Explicit VR
                type = FileDicomTagInfo.getType(new String(vr));

                if ( !DicomDictionary.containsTag(key)) {
                    tagVM = 0;

                    // put private tags with explicit VRs in file info hashtable
                    tagTable.putPrivateTagValue(new FileDicomTagInfo(key, new String(vr), tagVM, "PrivateTag",
                            "Private Tag"));
                } else {
                    final FileDicomTagInfo info = DicomDictionary.getInfo(key);
                    // this is required if DicomDictionary contains wild card characters
                    info.setKey(key);
                    tagTable.putPrivateTagValue(info);
                    tagVM = info.getValueMultiplicity();
                    tagTable.get(key).setValueRepresentation(new String(vr));

                }
            }

            if ( (elementWord == 0) && (elementLength == 0)) { // End of file

                if ( !isQuiet()) {
                    MipavUtil.displayError("Error:  Unexpected end of file: " + fileName + "  Unable to load image.");
                }

                throw new IOException("Error while reading header");
            }

            if ( (getFilePointer() & 1) != 0) { // The file location pointer is at an odd byte number

                if ( !isQuiet()) {
                    MipavUtil.displayError("Error:  Input file corrupted.  Unable to load image.");
                }

                // System.err.println("name: "+ name + " , len: " + Integer.toString(elementLength, 0x10));
                Preferences.debug("ERROR CAUSED BY READING IMAGE ON ODD BYTE" + "\n", Preferences.DEBUG_FILEIO);
                throw new IOException("Error while reading header");
            }

            try {

                if (type.equals(FileDicomBase.TYPE_STRING)) {
                    strValue = getString(elementLength);

                    tagTable.setValue(key, strValue, elementLength);

                    Preferences.debug(tagTable.get(name).getName() + "\t\t(" + name + ");\t" + type + "; value = "
                            + strValue + "; element length = " + elementLength + "\n", Preferences.DEBUG_FILEIO);

                    // need to determine if this is enhanced dicom
                    // if it is, set up all the additional fileinfos needed and attach
                    // the childTagTables to the main tagTable
                    if (name.equals("0002,0002")) {
                        if (strValue.trim().equals(DICOM_Constants.UID_EnhancedMRStorage)
                                || strValue.trim().equals(DICOM_Constants.UID_EnhancedCTStorage)
                                || strValue.trim().equals(DICOM_Constants.UID_EnhancedXAStorage)) {
                            isEnhanced = true;
                        }
                    }
                    if (name.equals("0028,0008") && isEnhanced) {
                        final int nImages = Integer.valueOf(strValue.trim()).intValue();
                        fileInfo.setIsEnhancedDicom(true);
                        if (nImages > 1) {
                            childrenTagTables = new FileDicomTagTable[nImages - 1];
                            enhancedFileInfos = new FileInfoDicom[nImages - 1];
                            for (int i = 0; i < nImages - 1; i++) {
                                final String s = String.valueOf(i + 1);
                                final FileInfoDicom f = new FileInfoDicom(fileName + s, fileDir, FileUtility.DICOM,
                                        fileInfo);
                                f.setIsEnhancedDicom(true);
                                childrenTagTables[i] = f.getTagTable();
                                enhancedFileInfos[i] = f;
                            }
                            tagTable.attachChildTagTables(childrenTagTables);
                        }
                    }

                } else if (type.equals(FileDicomBase.OTHER_BYTE_STRING)) {

                    if ( !name.equals(FileDicom.IMAGE_TAG)) {
                        data = getByte(tagVM, elementLength, endianess);
                        tagTable.setValue(key, data, elementLength);

                        if (tagVM == 1) {
                            // if ( debug >= dicomDebugOutputValue) Preferences.debug(
                            // ((FileDicomTag)(fileInfo.getEntry(name))).getName() + "\t\t(" + name + ");\t
                            // (byte) value = " + ((Byte)(data)).byteValue() + " element length = 2" + "\n", 2);
                        }
                    }
                } else if (type.equals(FileDicomBase.OTHER_WORD_STRING) && !name.equals("0028,1201")
                        && !name.equals("0028,1202") && !name.equals("0028,1203")) {

                    if ( !name.equals(FileDicom.IMAGE_TAG)) {
                        data = getByte(tagVM, elementLength, endianess);
                        tagTable.setValue(key, data, elementLength);
                    }
                } else if (type.equals(FileDicomBase.TYPE_SHORT)) {
                    data = getShort(tagVM, elementLength, endianess);
                    tagTable.setValue(key, data, elementLength);

                    Preferences.debug(tagTable.get(name).getName() + "\t\t(" + name + ");\t (short) value = " + data
                            + " element length = " + elementLength + "\n", Preferences.DEBUG_FILEIO);

                } else if (type.equals(FileDicomBase.TYPE_INT)) {
                    data = getInteger(tagVM, elementLength, endianess);

                    tagTable.setValue(key, data, elementLength);

                    Preferences.debug(tagTable.get(name).getName() + "\t\t(" + name + ");\t (int) value = " + data
                            + " element length = " + elementLength + "\n", Preferences.DEBUG_FILEIO);

                } else if (type.equals(FileDicomBase.TYPE_FLOAT)) {
                    data = getFloat(tagVM, elementLength, endianess);
                    tagTable.setValue(key, data, elementLength);

                    Preferences.debug(tagTable.get(name).getName() + "\t\t(" + name + ");\t (float) value = " + data
                            + " element length = " + elementLength + "\n", Preferences.DEBUG_FILEIO);

                } else if (type.equals(FileDicomBase.TYPE_DOUBLE)) {
                    data = getDouble(tagVM, elementLength, endianess);
                    tagTable.setValue(key, data, elementLength);

                    Preferences.debug(tagTable.get(name).getName() + "\t\t(" + name + ");\t (double) value = " + data
                            + " element length = " + elementLength + "\n", Preferences.DEBUG_FILEIO);

                }
                // (type == "typeUnknown" && elementLength == -1) Implicit sequence tag if not in DICOM dictionary.
                else if (type.equals(FileDicomBase.TYPE_SEQUENCE)
                        || ( (type == FileDicomBase.TYPE_UNKNOWN) && (elementLength == -1))) {
                    final int len = elementLength;

                    // save these values because they'll change as the sequence is read in below.
                    Preferences
                            .debug("Sequence Tags: (" + name + "); length = " + len + "\n", Preferences.DEBUG_FILEIO);

                    Object sq;

                    // ENHANCED DICOM per frame stuff
                    if (name.equals("5200,9230")) {
                        int numSlices = 0;
                        sq = getSequence(endianess, len);
                        final Vector<FileDicomItem> v = ((FileDicomSQ) sq).getSequence();
                        int elemLength;
                        FileDicomKey fdKey;
                        for (int i = 0; i < v.size(); i++) {
                            final FileDicomItem item = v.get(i);
                            final TreeMap<String, FileDicomTag> dataSet = item.getDataSet();
                            final Set<String> keySet = dataSet.keySet();
                            final Iterator<String> iter = keySet.iterator();
                            while (iter.hasNext()) {
                                final String k = iter.next();
                                final FileDicomTag entry = dataSet.get(k);
                                final String vr = entry.getValueRepresentation();
                                final String t = FileDicomTagInfo.getType(vr);
                                if (t.equals(FileDicomBase.TYPE_SEQUENCE)) {
                                    elemLength = entry.getLength();
                                    final Vector<FileDicomItem> vSeq = ((FileDicomSQ) entry.getValue(true))
                                            .getSequence();
                                    for (int m = 0; m < vSeq.size(); m++) {
                                        final FileDicomItem subItem = vSeq.get(m);
                                        final TreeMap<String, FileDicomTag> subDataSet = subItem.getDataSet();
                                        final Set<String> subKeySet = subDataSet.keySet();
                                        final Iterator<String> subIter = subKeySet.iterator();
                                        while (subIter.hasNext()) {
                                            final String kSub = subIter.next();
                                            final FileDicomTag subEntry = subDataSet.get(kSub);
                                            elemLength = subEntry.getLength();
                                            fdKey = new FileDicomKey(kSub);
                                            if (kSub.equals("0020,9057")) {
                                                // OK...so we should be relying on 0020,9056 (Stack ID)
                                                // to tell us the number of volumes in the dataet
                                                // but we find that it is not being implemented in the dataset we have
                                                // Thefore, we will look at 0020.9057 (In-Stack Pos ID).
                                                // These should all be unique for 1 volume. If we find that there
                                                // are duplicates, then that means we are dealing with a 4D datset
                                                // we will get the number of slices in a volumne. Then determine
                                                // number of volumes by taking total num slices / num slices per volume
                                                // ftp://medical.nema.org/medical/dicom/final/cp583_ft.pdf
                                                final int currNum = ((Integer) subEntry.getValue(true)).intValue();
                                                if (currNum == numSlices) {
                                                    isEnhanced4D = true;
                                                }
                                                if (currNum > numSlices) {
                                                    numSlices = currNum;
                                                }
                                            }
                                            if (i == 0) {
                                                tagTable.setValue(fdKey, subEntry.getValue(true), elemLength);
                                            } else {
                                                childrenTagTables[i - 1].setValue(fdKey, subEntry.getValue(true),
                                                        elemLength);
                                            }
                                        }
                                    }
                                } else {
                                    elemLength = entry.getLength();
                                    fdKey = new FileDicomKey(k);
                                    if (i == 0) {
                                        tagTable.setValue(fdKey, entry.getValue(true), elemLength);
                                    } else {
                                        childrenTagTables[i - 1].setValue(fdKey, entry.getValue(true), elemLength);
                                    }
                                }
                            }
                        }
                        enhancedNumSlices = numSlices;
                        // remove tag 5200,9230 if its there
                        final FileDicomTag removeTag = tagTable.get(key);
                        if (removeTag != null) {
                            tagTable.removeTag(key);
                        }
                    } else {
                        if (name.equals("0004,1220")) {
                            dirInfo = (FileDicomSQ) getSequence(endianess, len);
                            sq = new FileDicomSQ();
                        } else {
                            sq = getSequence(endianess, len);

                        }
                        // System.err.print( "SEQUENCE DONE: Sequence Tags: (" + name + "); length = " +
                        // Integer.toString(len, 0x10) + "\n");

                        try {
                            tagTable.setValue(key, sq, elementLength);
                        } catch (final NullPointerException e) {
                            Preferences.debug("Null pointer exception while setting value.  Trying to put new tag."
                                    + "\n", Preferences.DEBUG_FILEIO);
                        }

                    }

                    /*
                     * if (name.equals("0004,1220")) { dirInfo = (FileDicomSQ) getSequence(endianess, len); sq = new
                     * FileDicomSQ(); } else { sq = getSequence(endianess, len);
                     *  } // System.err.print( "SEQUENCE DONE: Sequence Tags: (" + name + "); length = " + //
                     * Integer.toString(len, 0x10) + "\n");
                     * 
                     * try { tagTable.setValue(key, sq, elementLength); } catch (final NullPointerException e) {
                     * System.err.println("Null pointer exception while setting value. Trying to put new tag."); }
                     */

                    // fileInfo.setLength(name, len);
                    Preferences.debug("Finished sequence tags.\n\n", Preferences.DEBUG_FILEIO);
                }
            } catch (final OutOfMemoryError e) {

                if ( !isQuiet()) {
                    MipavUtil.displayError("Out of memory in FileDicom.readHeader");
                    Preferences.debug("Out of memory in FileDicom.readHeader\n", Preferences.DEBUG_FILEIO);
                } else {
                    Preferences.debug("Out of memory in FileDicom.readHeader\n", Preferences.DEBUG_FILEIO);
                }

                e.printStackTrace();

                throw new IOException();
            }

            if (name.equals("0002,0000")) { // length of the transfer syntax group

                if (data != null) {
                    metaGroupLength = ((Integer) (data)).intValue() + 12; // 12 is the length of 0002,0000 tag
                }

                Preferences.debug("metalength = " + metaGroupLength + " location " + getFilePointer() + "\n",
                        Preferences.DEBUG_FILEIO);
            } else if (name.equals("0018,602C")) {
                fileInfo.setUnitsOfMeasure(FileInfoBase.CENTIMETERS, 0);
            } else if (name.equals("0018,602E")) {
                fileInfo.setUnitsOfMeasure(FileInfoBase.CENTIMETERS, 1);
            } else if (name.equals("0004,1220")) {
                Preferences.debug("DICOMDIR Found! \n");
                flag = false;
                notDir = false;
            } else if (name.equals("0002,0010")) {

                // Transfer Syntax UID: DICOM part 10 page 13, part 5 p. 42-48, Part 6 p. 53
                // 1.2.840.10008.1.2 Implicit VR Little Endian (Default)
                // 1.2.840.10008.1.2.1 Explicit VR Little Endian
                // 1.2.840.10008.1.2.2 Explicit VR Big Endian
                // 1.2.840.10008.1.2.4.50 8-bit Lossy JPEG (JPEG Coding Process 1)
                // 1.2.840.10008.1.2.4.51 12-bit Lossy JPEG (JPEG Coding Process 4)
                // 1.2.840.10008.1.2.4.57 Lossless JPEG Non-hierarchical (JPEG Coding Process 14)
                // we should bounce out if we don't support this transfer syntax
                if (strValue.trim().equals("1.2.840.10008.1.2")) {
                    Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue
                            + " Implicit VR - Little Endian \n", Preferences.DEBUG_FILEIO);

                    fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
                    fileInfo.vr_type = FileInfoDicom.IMPLICIT;
                    encapsulated = false;
                } else if (strValue.trim().equals("1.2.840.10008.1.2.1")) {
                    Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue
                            + " Explicit VR - Little Endian \n", Preferences.DEBUG_FILEIO);

                    fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
                    fileInfo.vr_type = FileInfoDicom.EXPLICIT;
                    encapsulated = false;
                } else if (strValue.trim().equals("1.2.840.10008.1.2.2")) {
                    Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue
                            + " Explicit VR - Big Endian \n", Preferences.DEBUG_FILEIO);

                    fileInfo.setEndianess(FileBase.BIG_ENDIAN);
                    fileInfo.vr_type = FileInfoDicom.EXPLICIT;
                    encapsulated = false;
                } else if (strValue.trim().startsWith("1.2.840.10008.1.2.4.")) { // JPEG
                    Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue
                            + " Implicit VR - Little Endian \n", Preferences.DEBUG_FILEIO);

                    fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
                    fileInfo.vr_type = FileInfoDicom.EXPLICIT;
                    encapsulated = true;
                    if (strValue.trim().equals(DICOM_Constants.UID_TransferJPEG2000LOSSLESS)) {
                        encapsulatedJP2 = true;
                    }

                    if (strValue.trim().equals("1.2.840.10008.1.2.4.57")
                            || strValue.trim().equals("1.2.840.10008.1.2.4.58")
                            || strValue.trim().equals("1.2.840.10008.1.2.4.65")
                            || strValue.trim().equals("1.2.840.10008.1.2.4.66")
                            || strValue.trim().equals("1.2.840.10008.1.2.4.70")
                            || strValue.trim().equals("1.2.840.10008.1.2.4.90")) {
                        lossy = false;
                    } else {
                        lossy = true;
                    }
                } else {
                    Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue + " unknown!\n",
                            Preferences.DEBUG_FILEIO);

                    if ( !isQuiet()) {
                        MipavUtil.displayError("MIPAV does not support transfer syntax:\n" + strValue);
                    }

                    flag = false; // break loop

                    return false; // couldn't read it!
                }
            } else if (name.equals("0028,0010")) { // Set the extents, used for reading the image
                extents[1] = ((Short) tagTable.getValue(name)).intValue();
                // fileInfo.columns = extents[1];
            } else if (name.equals("0028,0011")) {
                extents[0] = ((Short) tagTable.getValue(name)).intValue();
                // fileInfo.rows = extents[0];
            } else if (name.equals("0028,1201")) { // LUT for red-channel

                // red channel LUT
                try {
                    getColorPallete(new FileDicomKey(name));
                } catch (final IllegalArgumentException iae) {
                    // System.err.println("Dude. You screwed up somewhere! "+
                    // " storeColorPallete throws iae when something other than"+
                    // " 0028,120[1|2|3] is used.");
                }
            } else if (name.equals("0028,1202")) { // LUT for blue-channel

                // blue channel LUT
                try {
                    getColorPallete(new FileDicomKey(name));
                } catch (final IllegalArgumentException iae) {
                    // System.err.println("Dude. You screwed up somewhere! "+
                    // " storeColorPallete throws iae when something other than"+
                    // " 0028,120[1|2|3] is used.");
                }
            } else if (name.equals("0028,1203")) { // LUT for gree-channel

                // green channel LUT
                try {
                    getColorPallete(new FileDicomKey(name));
                } catch (final IllegalArgumentException iae) {
                    // storeColorPallete throws iae when something other than
                    // 0028,120[1|2|3] is used.
                    // System.err.println("Dude. You screwed up somewhere! "+
                    // " storeColorPallete throws iae when something other than"+
                    // " 0028,120[1|2|3] is used.");
                }
            } else if (name.equals(FileDicom.IMAGE_TAG)) { // && elementLength!=0) { // The image.

                // This complicated code determines whether or not the offset is correct for the image.
                // For that to be true, (height * width * pixel spacing) + the present offset should equal
                // the length of the file. If not, 4 might need to be added (I don't know why). If not again,
                // the function returns false.

                final int imageLength = extents[0] * extents[1] * fileInfo.bitsAllocated / 8;

                Preferences.debug("File Dicom: readHeader - Data tag = " + FileDicom.IMAGE_TAG + "\n",
                        Preferences.DEBUG_FILEIO);
                Preferences.debug("File Dicom: readHeader - imageLength = " + imageLength + "\n",
                        Preferences.DEBUG_FILEIO);
                Preferences.debug("File Dicom: readHeader - getFilePointer = " + getFilePointer() + "\n",
                        Preferences.DEBUG_FILEIO);

                if (fileInfo.getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY) {
                    fileInfo.displayType = ModelStorageBase.FLOAT;
                } else {
                    fileInfo.displayType = fileInfo.getDataType();
                }

                if ( !encapsulated) {
                    // System.err.println( "\n" +
                    // Long.toString(getFilePointer()) + " " +
                    // Long.toString(raFile.length()) +
                    // " image length = " + imageLength );

                    long fileEnd;

                    if (loadTagBuffer == true) {
                        fileEnd = raFile.length();
                    } else {
                        fileEnd = fLength;
                    }

                    if ( (imageLength + getFilePointer()) <= fileEnd) {
                        fileInfo.setOffset(getFilePointer()); // Mark where the image is
                    }
                    // I think the extra 4 bytes is for explicit tags!!
                    // see Part 5 page 27 1998
                    else if ( (imageLength + getFilePointer() + 4) <= fileEnd) {
                        fileInfo.setOffset(getFilePointer() + 4);
                    } else {

                        // Preferences.debug( "File Dicom: readHeader: xDim = " + extents[0] + " yDim = " + extents[1] +
                        // " Bits allocated = " + fileInfo.bitsAllocated, 2);
                        if ( !isQuiet()) {
                            MipavUtil.displayError("Image not at expected offset.");
                        }

                        throw new IOException("Error while reading header");
                    }
                } else { // encapsulated
                    fileInfo.setOffset(getFilePointer() - 12);
                }

                fileInfo.setExtents(extents);
                flag = false; // break loop
            } else if (type.equals(FileDicomBase.TYPE_UNKNOWN)) { // Private tag, may not be reading in correctly.
                try {

                    // set the value if the tag is in the dictionary (which means it isn't private..) or has already
                    // been put into the tag table without a value (private tag with explicit vr)
                    if (DicomDictionary.containsTag(key) || tagTable.containsTag(key)) {
                        tagTable.setValue(key, readUnknownData(), elementLength);
                    } else {
                        tagTable
                                .putPrivateTagValue(new FileDicomTagInfo(key, null, tagVM, "PrivateTag", "Private Tag"));

                        tagTable.setValue(key, readUnknownData(), elementLength);

                        Preferences.debug("Group = " + groupWord + " element = " + elementWord + " Type unknown"
                                + "; value = " + strValue + "; element length = " + elementLength + "\n",
                                Preferences.DEBUG_FILEIO);
                    }
                } catch (final OutOfMemoryError e) {

                    if ( !isQuiet()) {
                        MipavUtil.displayError("Out of memory error while reading \"" + fileName
                                + "\".\nThis may not be a DICOM image.");
                        Preferences.debug("Out of memory storing unknown tags in FileDicom.readHeader\n",
                                Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Out of memory storing unknown tags in FileDicom.readHeader\n",
                                Preferences.DEBUG_FILEIO);
                    }

                    e.printStackTrace();

                    throw new IOException("Out of memory storing unknown tags in FileDicom.readHeader");
                } catch (final NullPointerException npe) {
                    Preferences.debug("name: " + name + "\n" + "no hashtable? " + (tagTable == null) + "\n",
                            Preferences.DEBUG_FILEIO);
                    throw npe;
                }
            }

            // for dicom files that contain no image information, the image tag will never be encountered
            if (getFilePointer() == fLength) {
                flag = false;
            }

        }
        // Done reading tags, if DICOMDIR then don't do anything else

        if (notDir) {

            String photometricInterp = null;

            if (tagTable.getValue("0028,0004") != null) {
                fileInfo.photometricInterp = ((String) (tagTable.getValue("0028,0004"))).trim();
                photometricInterp = fileInfo.photometricInterp.trim();
            }
            if (photometricInterp == null) { // Default to MONOCROME2 and hope for the best
                photometricInterp = new String("MONOCHROME2");
            }

            if ( (photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2"))
                    && (fileInfo.pixelRepresentation == FileInfoDicom.UNSIGNED_PIXEL_REP)
                    && (fileInfo.bitsAllocated == 8)) {
                fileInfo.setDataType(ModelStorageBase.UBYTE);
                fileInfo.displayType = ModelStorageBase.UBYTE;
                fileInfo.bytesPerPixel = 1;
            } else if ( (photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2"))
                    && (fileInfo.pixelRepresentation == FileInfoDicom.SIGNED_PIXEL_REP)
                    && (fileInfo.bitsAllocated == 8)) {
                fileInfo.setDataType(ModelStorageBase.BYTE);
                fileInfo.displayType = ModelStorageBase.BYTE;
                fileInfo.bytesPerPixel = 1;
            } else if ( (photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2"))
                    && (fileInfo.pixelRepresentation == FileInfoDicom.UNSIGNED_PIXEL_REP)
                    && (fileInfo.bitsAllocated > 16)) {
                fileInfo.setDataType(ModelStorageBase.UINTEGER);
                fileInfo.displayType = ModelStorageBase.UINTEGER;
                fileInfo.bytesPerPixel = 4;
            } else if ( (photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2"))
                    && (fileInfo.pixelRepresentation == FileInfoDicom.UNSIGNED_PIXEL_REP)
                    && (fileInfo.bitsAllocated > 8)) {
                fileInfo.setDataType(ModelStorageBase.USHORT);
                fileInfo.displayType = ModelStorageBase.USHORT;
                fileInfo.bytesPerPixel = 2;
            } else if ( (photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2"))
                    && (fileInfo.pixelRepresentation == FileInfoDicom.SIGNED_PIXEL_REP) && (fileInfo.bitsAllocated > 8)) {
                fileInfo.setDataType(ModelStorageBase.SHORT);
                fileInfo.displayType = ModelStorageBase.SHORT;
                fileInfo.bytesPerPixel = 2;
            }
            // add something for RGB DICOM images - search on this !!!!
            else if (photometricInterp.equals("RGB") && (fileInfo.bitsAllocated == 8)) {
                fileInfo.setDataType(ModelStorageBase.ARGB);
                fileInfo.displayType = ModelStorageBase.ARGB;
                fileInfo.bytesPerPixel = 3;

                if (tagTable.getValue("0028,0006") != null) {
                    fileInfo.planarConfig = ((Short) (tagTable.getValue("0028,0006"))).shortValue();
                } else {
                    fileInfo.planarConfig = 0; // rgb, rgb, rgb
                }
            } else if (photometricInterp.equals("YBR_FULL_422") && (fileInfo.bitsAllocated == 8) && encapsulated) {
                fileInfo.setDataType(ModelStorageBase.ARGB);
                fileInfo.displayType = ModelStorageBase.ARGB;
                fileInfo.bytesPerPixel = 3;

                if (tagTable.getValue("0028,0006") != null) {
                    fileInfo.planarConfig = ((Short) (tagTable.getValue("0028,0006"))).shortValue();
                } else {
                    fileInfo.planarConfig = 0; // rgb, rgb, rgb
                }
            } else if (photometricInterp.equals("PALETTE COLOR")
                    && (fileInfo.pixelRepresentation == FileInfoDicom.UNSIGNED_PIXEL_REP)
                    && (fileInfo.bitsAllocated == 8)) {
                fileInfo.setDataType(ModelStorageBase.UBYTE);
                fileInfo.displayType = ModelStorageBase.UBYTE;
                fileInfo.bytesPerPixel = 1;

                final int[] dimExtents = new int[2];
                dimExtents[0] = 4;
                dimExtents[1] = 256;
                lut = new ModelLUT(ModelLUT.GRAY, 256, dimExtents);

                for (int q = 0; q < dimExtents[1]; q++) {
                    lut.set(0, q, 1); // the alpha channel is preloaded.
                }
                // sets the LUT to exist; we wait for the pallete tags to
                // describe what the lut should actually look like.
                // TODO: these should be specified by 0028,1101 - 1103

                // once thse have been processed, the LUT data can now be read and set
                if (tagTable.getValue("0028,1201") != null) {

                    // red channel LUT
                    final FileDicomTag tag = tagTable.get(new FileDicomKey("0028,1201"));
                    final Object[] values = tag.getValueList();
                    final int lutVals = values.length;

                    // load LUT.
                    if (values instanceof Byte[]) {

                        for (int qq = 0; qq < lutVals; qq++) {
                            lut.set(1, qq, ((Byte) values[qq]).intValue());
                        }
                    } else {

                        for (int qq = 0; qq < lutVals; qq++) {
                            lut.set(1, qq, ((Short) values[qq]).intValue());
                        }
                    }
                }

                if (tagTable.getValue("0028,1202") != null) {

                    // green channel LUT
                    final FileDicomTag tag = tagTable.get(new FileDicomKey("0028,1202"));
                    final Object[] values = tag.getValueList();
                    final int lutVals = values.length;

                    // load LUT.
                    if (values instanceof Byte[]) {

                        for (int qq = 0; qq < lutVals; qq++) {
                            lut.set(2, qq, ((Byte) values[qq]).intValue());
                        }
                    } else {

                        for (int qq = 0; qq < lutVals; qq++) {
                            lut.set(2, qq, ((Short) values[qq]).intValue());
                        }
                    }
                }

                if (tagTable.getValue("0028,1203") != null) {

                    // blue channel LUT
                    final FileDicomTag tag = tagTable.get(new FileDicomKey("0028,1203"));
                    final Object[] values = tag.getValueList();
                    final int lutVals = values.length;

                    // load LUT.
                    if (values instanceof Byte[]) {

                        for (int qq = 0; qq < lutVals; qq++) {
                            lut.set(3, qq, ((Byte) values[qq]).intValue());
                        }
                    } else {

                        for (int qq = 0; qq < lutVals; qq++) {
                            lut.set(3, qq, ((Short) values[qq]).intValue());
                        }
                    }

                    // here we make the lut indexed because we know that
                    // all the LUT tags are in the LUT.
                    lut.makeIndexedLUT(null);
                }

            } else {
                Preferences.debug("File DICOM: readImage() - Unsupported pixel Representation" + "\n",
                        Preferences.DEBUG_FILEIO);

                if (raFile != null) {
                    raFile.close();
                }

                return false;
            }

            if (fileInfo.getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY) {
                fileInfo.displayType = ModelStorageBase.FLOAT;
                // a bit of a hack - indicates Model image should be reallocated to float for PET image the data is
                // stored
                // as 2 bytes (short) but is "normalized" using the slope parameter required for PET images (intercept
                // always 0 for PET).
            }

            if ( ( (fileInfo.getDataType() == ModelStorageBase.UBYTE) || (fileInfo.getDataType() == ModelStorageBase.USHORT))
                    && (fileInfo.getRescaleIntercept() < 0)) {
                // this performs a similar method as the pet adjustment for float images stored on disk as short to read
                // in
                // signed byte and signed short images stored on disk as unsigned byte or unsigned short with a negative
                // rescale intercept
                if (fileInfo.getDataType() == ModelStorageBase.UBYTE) {
                    fileInfo.displayType = ModelStorageBase.BYTE;
                } else if (fileInfo.getDataType() == ModelStorageBase.USHORT) {
                    fileInfo.displayType = ModelStorageBase.SHORT;
                }
            }

            hasHeaderBeenRead = true;

            if ( (loadTagBuffer == true) && (raFile != null)) {
                raFile.close();
            }

            return true;
        } else {
            final JDialogDicomDir dirBrowser = new JDialogDicomDir(null, fileHeader, this);
            return true;
        }
    }

    /**
     * Reads a DICOM image file and stores the data into the given float buffer. This method reads the image header
     * (@see readHeader()) then sets various fields within the FileInfo which are relevant to correctly interpreting the
     * image. This list includes:
     * 
     * <ul>
     * <li>units of measure</li>
     * <li>pixel pad</li>
     * <li>file info minimum and maximum</li>
     * </ul>
     * 
     * @param buffer 2D buffer used for temporary storage of data
     * @param imageType The type of image (i.e. SHORT, BYTE, ...)
     * @param imageNo For multiFrame images, imageNo >=1. For single slice image imageNo = 0.
     * 
     * @return The image
     * 
     * @exception IOException if there is an error reading the file
     * 
     * @see FileRaw
     */
    public void readImage(final float[] buffer, final int imageType, final int imageNo) throws IOException {
        // System.out.println("in read image float");
        // Read in header info, if something goes wrong, print out error
        if (hasHeaderBeenRead == false) {

            if ( !readHeader(true)) {
                throw (new IOException("DICOM header file error"));
            }

        }

        if (fileInfo.getUnitsOfMeasure(0) != FileInfoBase.CENTIMETERS) {
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 0);
        }

        if (fileInfo.getUnitsOfMeasure(1) != FileInfoBase.CENTIMETERS) {
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 1);
        }

        fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 2);

        // Needed for correct display of the image
        // set to null if there is no pixel pad value
        fileInfo.setPixelPadValue(fileInfo.pixelPaddingValue);

        if ( !encapsulated) {

            try { // rafile (type RandomAccessFile) for header, rawfile (type FileRaw) for image data.
                rawFile.setImageFile(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

                if (ModelImage.isColorImage(imageType)) {
                    rawFile.setPlanarConfig(fileInfo.planarConfig);
                    rawFile.readImage(buffer, fileInfo.getOffset() + (imageNo * buffer.length), imageType); // *****
                    // Read
                    // image
                    rawFile.raFile.close();
                    rawFile.raFile = null;
                } else {
                    rawFile.readImage(buffer,
                            fileInfo.getOffset() + (imageNo * buffer.length * fileInfo.bytesPerPixel), imageType); // *****
                    // Read
                    // image
                    rawFile.raFile.close();
                    rawFile.raFile = null;
                }
            } catch (final IOException error) {
                // System.err.println("ReadDICOM IOexception error");
                MipavUtil.displayError("FileDicom: " + error);
                throw (error);
            }
        } else { // encapsulated
            System.out.println("IMAGE IS ENCAPSULATED");

            if (jpegData == null) {
                jpegData = encapsulatedImageData();
            }

            if (jpegData != null) {

                try {
                    int j = imageNo * buffer.length;

                    for (int i = 0; i < buffer.length; i++) {
                        buffer[i] = jpegData[j];
                        j++;
                    }
                } catch (final ArrayIndexOutOfBoundsException aioobe) {
                    MipavUtil.displayError("Image is smaller than expected.  " + "Showing as much as available.");
                }

                // this means there was only one image - not multiframe.
                // if image WAS multiframe, we don't want to keep reading in the jpegData buffer
                // it will be non null the second time through, and won't be read in again.
                if (jpegData.length == buffer.length) {
                    jpegData = null;
                }
            }
        }

        // Best increase in speed might be to reduce conversions from different types.
        // Presently, short buffer is read in the raw chunk class. Converted to float buffer in raw file.
        // Processed here - slope and intercept.
        // Then copied into ModelImage which is a short object (except PET) therefore it is converted from the float
        // back into the ModelImage of type short.
        // Most of this happens for code reuse advantages but at great expense for speed.
        // Might be able to speed up the process by

        // Matt changed from double to float for speed purposes 2/2003 not great increase but 5-10%.
        double tmp;
        double pixelPad = -Double.MAX_VALUE;
        double min = Double.MAX_VALUE, max = -Double.MAX_VALUE;

        // fix pixel padding and remap to HU units
        if (fileInfo.getPixelPadValue() != null) {
            pixelPad = fileInfo.getPixelPadValue().floatValue();
        }

        double slope = fileInfo.getRescaleSlope();
        final double intercept = fileInfo.getRescaleIntercept();
        if (slope == 0) {
            slope = 1;
        }

        // Why is this here? It overwrites the slope and intercept.
        // if (fileInfo.getModality() == FileInfoBase.MAGNETIC_RESONANCE) {
        // slope = 1;
        // intercept = 0;
        // }

        boolean setOneMinMax = false;
        for (final float element : buffer) {
            tmp = element;

            if (tmp != pixelPad) {
                setOneMinMax = true;
                if (tmp < min) {
                    min = tmp;
                }
                if (tmp > max) {
                    max = tmp;
                }
            }
        }
        if (setOneMinMax == false) {
            min = max = buffer[0];
        }

        fileInfo.setMin(min);
        fileInfo.setMax(max);

        if ( (pixelPad <= min) || (pixelPad >= max)) {

            for (int i = 0; i < buffer.length; i++) {

                // tmp = buffer[i];
                // need to fix - we're altering image data so that when the file is
                // written, it is not exactly the same as when it was read in, i.e.,
                // there are no pixel pad values stored in the buffer; they've all been
                // converted to the minimum value.
                if (buffer[i] != pixelPad) {
                    buffer[i] = (float) ( (buffer[i] * slope) + intercept);
                } else {
                    buffer[i] = (float) ( (min * slope) + intercept);
                }
            }
        } else {

            if ( (slope != 1) || (intercept != 0)) {

                for (int i = 0; i < buffer.length; i++) {
                    buffer[i] = (float) ( (buffer[i] * slope) + intercept); // Rescale data
                }
            }
        }
        // End of Matt changes for 2/2003
    }

    /**
     * Reads a DICOM image file and stores the data into the given short buffer. This method reads the image header
     * (@see readHeader()) then sets various fields within the FileInfo which are relevant to correctly interpreting the
     * image. This list includes:
     * 
     * <ul>
     * <li>units of measure</li>
     * <li>pixel pad</li>
     * <li>file info minimum and maximum</li>
     * </ul>
     * 
     * <p>
     * This method would be used for short- (byte-) size image datasuch as PET data. This method is faster than the
     * float buffer version of this method since not as much type-conversion is needed.
     * </p>
     * 
     * @param buffer 2D buffer used for temporary storage of data
     * @param imageType The type of image (i.e. SHORT, BYTE, ...)
     * @param imageNo For multiFrame images, imageNo >=1. For single slice image imageNo = 0.
     * 
     * @return The image
     * 
     * @exception IOException if there is an error reading the file
     * 
     * @see FileRaw
     */
    public void readImage(final short[] buffer, final int imageType, final int imageNo) throws IOException {
        // System.out.println("in read image short");
        // Read in header info, if something goes wrong, print out error
        if (hasHeaderBeenRead == false) {

            if ( !readHeader(true)) {
                throw (new IOException("DICOM header file error"));
            }
        }

        if (fileInfo.getUnitsOfMeasure(0) != FileInfoBase.CENTIMETERS) {
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 0);
        }

        if (fileInfo.getUnitsOfMeasure(1) != FileInfoBase.CENTIMETERS) {
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 1);
        }

        fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 2);

        // Needed for correct display of the image
        // set to null if there is no pixel pad value
        fileInfo.setPixelPadValue(fileInfo.pixelPaddingValue);

        if ( !encapsulated) {

            try { // rafile (type RandomAccessFile) for header, rawfile (type FileRaw) for image data.
                rawFile.setImageFile(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

                if (ModelImage.isColorImage(imageType)) {
                    rawFile.setPlanarConfig(fileInfo.planarConfig);
                    rawFile.readImage(buffer, fileInfo.getOffset() + (imageNo * (buffer.length / 4 * 3)), imageType);
                    rawFile.raFile.close();
                    rawFile.raFile = null;
                } else {
                    rawFile.readImage(buffer,
                            fileInfo.getOffset() + (imageNo * buffer.length * fileInfo.bytesPerPixel), imageType);
                    rawFile.raFile.close();
                    rawFile.raFile = null;
                }
            } catch (final IOException error) {
                error.printStackTrace();
                // System.err.println("ReadDICOM IOexception error");
                MipavUtil.displayError("FileDicom: " + error);
                throw (error);
            }
        } else { // encapsulated

            if (jpegData == null) {
                if (encapsulatedJP2) {
                    System.out.println("calling encapsulatedJP2ImageData");
                    jpegData = encapsulatedJP2ImageData(imageType);

                } else {
                    System.out.println("Calling encapsulatedImageData");
                    jpegData = encapsulatedImageData();
                }
            }

            if (jpegData != null) {

                try {
                    int j = imageNo * buffer.length;

                    for (int i = 0; i < buffer.length; i++) {
                        buffer[i] = (short) jpegData[j];
                        j++;
                    }
                } catch (final ArrayIndexOutOfBoundsException aioobe) {
                    MipavUtil.displayError("Image is smaller than expected.  Showing as much as available.");
                }

                // this means there was only one image - not multiframe.
                // if image WAS multiframe, we don't want to keep reading in the jpegData buffer
                // it will be non null the second time through, and won't be read in again.
                if (jpegData.length == buffer.length) {
                    jpegData = null;
                }
            }
        }

        // Matt changed from double to float for speed purposes 2/2003 not great increase but 5-10%.
        short tmp;
        short pixelPad = Short.MIN_VALUE;
        short min = Short.MAX_VALUE, max = Short.MIN_VALUE;

        // fix pixel padding and remap to HU units
        if (fileInfo.getPixelPadValue() != null) {

            if ( (imageType == ModelStorageBase.UBYTE) || (imageType == ModelStorageBase.USHORT)) {
                pixelPad = (short) (fileInfo.getPixelPadValue().shortValue() & 0xffff);
            } else {
                pixelPad = fileInfo.getPixelPadValue().shortValue();
            }
        }
        // System.out.println("pixel _pad = "+ pixelPad);

        float slope = (float) fileInfo.getRescaleSlope();
        final float intercept = (float) fileInfo.getRescaleIntercept();
        if (slope == 0) {
            slope = 1;
        }

        // System.out.println(" slope = " + slope + " intercept = " + intercept);
        // Why is this here? It overwrites the slope and intercept.
        // if (fileInfo.getModality() == FileInfoBase.MAGNETIC_RESONANCE) {
        // slope = 1;
        // intercept = 0;
        // }

        boolean setOneMinMax = false;
        for (final short element : buffer) {
            tmp = element;

            if (tmp != pixelPad) {
                setOneMinMax = true;
                if (tmp < min) {
                    min = tmp;
                }
                if (tmp > max) {
                    max = tmp;
                }
            }
        }
        if (setOneMinMax == false) {
            min = max = buffer[0];
        }

        fileInfo.setMin(min);
        fileInfo.setMax(max);

        // System.out.println("min = " + min + " max = " + max);
        if ( (pixelPad <= min) || (pixelPad >= max)) {

            for (int i = 0; i < buffer.length; i++) {

                // tmp = buffer[i];
                // need to fix - we're altering image data so that when the file is
                // written, it is not exactly the same as when it was read in, i.e.,
                // there are no pixel pad values stored in the buffer; they've all been
                // converted to the minimum value.
                if (buffer[i] != pixelPad) {
                    buffer[i] = (short) MipavMath.round( (buffer[i] * slope) + intercept);
                } else {
                    buffer[i] = (short) MipavMath.round( (min * slope) + intercept);
                }
            }
        } else {
            if ( (slope != 1) || (intercept != 0)) {
                for (int i = 0; i < buffer.length; i++) {
                    buffer[i] = (short) MipavMath.round( (buffer[i] * slope) + intercept); // Rescale data
                }
            }
        }
        // End of Matt changes for 2/2003
    }

    /**
     * Sets the file info and sets the hasHeaderBeenRead to true.
     * 
     * @param fiDicom File info structure
     */
    public final void setFileInfo(final FileInfoDicom fiDicom) {
        fileInfo = fiDicom;
        hasHeaderBeenRead = true;
    }
    
    /**
     * Reads a DICOM image file and stores the data into the given short buffer. This method reads the image header
     * (@see readHeader()) then sets various fields within the FileInfo which are relevant to correctly interpreting the
     * image. This list includes:
     * 
     * <ul>
     * <li>units of measure</li>
     * <li>pixel pad</li>
     * <li>file info minimum and maximum</li>
     * </ul>
     * 
     * <p>
     * This method would be used for short- (byte-) size image datasuch as PET data. This method is faster than the
     * float buffer version of this method since not as much type-conversion is needed.
     * </p>
     * 
     * @param buffer 2D buffer used for temporary storage of data
     * @param imageType The type of image (i.e. SHORT, BYTE, ...)
     * @param imageNo For multiFrame images, imageNo >=1. For single slice image imageNo = 0.
     * 
     * @return The image
     * 
     * @exception IOException if there is an error reading the file
     * 
     * @see FileRaw
     */
    public void readImage(final int[] buffer, final int imageType, final int imageNo) throws IOException {
        // System.out.println("in read image short");
        // Read in header info, if something goes wrong, print out error
        if (hasHeaderBeenRead == false) {

            if ( !readHeader(true)) {
                throw (new IOException("DICOM header file error"));
            }
        }

        if (fileInfo.getUnitsOfMeasure(0) != FileInfoBase.CENTIMETERS) {
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 0);
        }

        if (fileInfo.getUnitsOfMeasure(1) != FileInfoBase.CENTIMETERS) {
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 1);
        }

        fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 2);

        // Needed for correct display of the image
        // set to null if there is no pixel pad value
        fileInfo.setPixelPadValue(fileInfo.pixelPaddingValue);

        if ( !encapsulated) {

            try { // rafile (type RandomAccessFile) for header, rawfile (type FileRaw) for image data.
                rawFile.setImageFile(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ); 
                rawFile.readImage(buffer,
                        fileInfo.getOffset() + (imageNo * buffer.length * fileInfo.bytesPerPixel), imageType);
                rawFile.raFile.close();
                rawFile.raFile = null;
            } catch (final IOException error) {
                error.printStackTrace();
                // System.err.println("ReadDICOM IOexception error");
                MipavUtil.displayError("FileDicom: " + error);
                throw (error);
            }
        } else { // encapsulated

            if (jpegData == null) {
                if (encapsulatedJP2) {
                    System.out.println("calling encapsulatedJP2ImageData");
                    jpegData = encapsulatedJP2ImageData(imageType);

                } else {
                    System.out.println("Calling encapsulatedImageData");
                    jpegData = encapsulatedImageData();
                }
            }

            if (jpegData != null) {

                try {
                    int j = imageNo * buffer.length;

                    for (int i = 0; i < buffer.length; i++) {
                        buffer[i] = (short) jpegData[j];
                        j++;
                    }
                } catch (final ArrayIndexOutOfBoundsException aioobe) {
                    MipavUtil.displayError("Image is smaller than expected.  Showing as much as available.");
                }

                // this means there was only one image - not multiframe.
                // if image WAS multiframe, we don't want to keep reading in the jpegData buffer
                // it will be non null the second time through, and won't be read in again.
                if (jpegData.length == buffer.length) {
                    jpegData = null;
                }
            }
        }

        // Matt changed from double to float for speed purposes 2/2003 not great increase but 5-10%.
        int tmp;
        short pixelPad = Short.MIN_VALUE;
        int min = Integer.MAX_VALUE, max = Integer.MIN_VALUE;

        // fix pixel padding and remap to HU units
        if (fileInfo.getPixelPadValue() != null) {   
            pixelPad = fileInfo.getPixelPadValue().shortValue();
        }
        // System.out.println("pixel _pad = "+ pixelPad);

        float slope = (float) fileInfo.getRescaleSlope();
        final float intercept = (float) fileInfo.getRescaleIntercept();
        if (slope == 0) {
            slope = 1;
        }

        // System.out.println(" slope = " + slope + " intercept = " + intercept);
        // Why is this here? It overwrites the slope and intercept.
        // if (fileInfo.getModality() == FileInfoBase.MAGNETIC_RESONANCE) {
        // slope = 1;
        // intercept = 0;
        // }

        boolean setOneMinMax = false;
        for (final int element : buffer) {
            tmp = element;

            if (tmp != pixelPad) {
                setOneMinMax = true;
                if (tmp < min) {
                    min = tmp;
                }
                if (tmp > max) {
                    max = tmp;
                }
            }
        }
        if (setOneMinMax == false) {
            min = max = buffer[0];
        }

        fileInfo.setMin(min);
        fileInfo.setMax(max);

        // System.out.println("min = " + min + " max = " + max);
        if ( (pixelPad <= min) || (pixelPad >= max)) {

            for (int i = 0; i < buffer.length; i++) {

                // tmp = buffer[i];
                // need to fix - we're altering image data so that when the file is
                // written, it is not exactly the same as when it was read in, i.e.,
                // there are no pixel pad values stored in the buffer; they've all been
                // converted to the minimum value.
                if (buffer[i] != pixelPad) {
                    buffer[i] = (int) MipavMath.round( (buffer[i] * slope) + intercept);
                } else {
                    buffer[i] = (int) MipavMath.round( (min * slope) + intercept);
                }
            }
        } else {
            if ( (slope != 1) || (intercept != 0)) {
                for (int i = 0; i < buffer.length; i++) {
                    buffer[i] = (int) MipavMath.round( (buffer[i] * slope) + intercept); // Rescale data
                }
            }
        }
    }


    /**
     * Accessor that sets the file name and allocates new FileInfo, File and RandomAccess file objects based on the new
     * image file based on the new filename and the old directory. This method sets the filename property of the
     * FileDicom, recreates a raw file for random access file to read the image file pointed to by the filename (the
     * file is opened for reading only; it cannot be written to). A new file info is created with this file info and the
     * old DICOM dictionary is used. Finally, the endianess is set to be little-endian.
     * 
     * <p>
     * Should an out-of-memory condition occur an IOException will be thrown.
     * </p>
     * 
     * @param fName File name for the image file.
     * @param refInfo The reference file info, containing the reference tag table.
     * 
     * @exception IOException if there is an error constructing the files.
     */
    public final void setFileName(final String fName, final FileInfoDicom refInfo) throws IOException {
        this.setFileName(new File(fileDir + fName), refInfo);
    }

    public boolean isEnhanced() {
        return isEnhanced;
    }

    public boolean isEnhanced4D() {
        return isEnhanced4D;
    }

    public int getEnhancedNumSlices() {
        return enhancedNumSlices;
    }

    public FileInfoDicom[] getEnhancedFileInfos() {
        return enhancedFileInfos;
    }

    /**
     * Accessor that sets the file name and allocates new FileInfo, File and RandomAccess file objects based on the new
     * image file based on the new filename and the new directory. This method sets the filename property of the
     * FileDicom, recreates a raw file for random access file to read the image file pointed to by the filename (the
     * file is opened for reading only; it cannot be written to). A new file info is created with this file info and the
     * old DICOM dictionary is used. Finally, the endianess is set to be little-endian.
     * 
     * <p>
     * Should an out-of-memory condition occur an IOException will be thrown.
     * </p>
     * 
     * @param f image file to point to.
     * @param refInfo The reference file info, containing the reference tag table.
     * 
     * @exception IOException if there is an error constructing the files.
     */
    public void setFileName(final File f, final FileInfoDicom refInfo) throws IOException {
        fileName = f.getName();
        fileDir = f.getParent() + File.separator;
        fileHeader = f;

        try {

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (final IOException ex) {}
            }

            raFile = new RandomAccessFile(fileHeader, "r");
            fileInfo = new FileInfoDicom(fileName, fileDir, FileUtility.DICOM, refInfo);
        } catch (final OutOfMemoryError e) {

            if ( !quiet) {
                MipavUtil.displayError("Out of memory in FileDicom.setFileName.");
            }

            throw new IOException("Out of Memory in FileDicom.setFileName" + e.getLocalizedMessage());
        }

        fileInfo.setEndianess(FileDicomBase.LITTLE_ENDIAN);
    }

    /**
     * Accessor that sets the file name and allocates new FileInfo, File and RandomAccess file objects based on the new
     * image file based on the new filename and the new directory. This method sets the filename property of the
     * FileDicom, recreates a raw file for random access file to read the image file pointed to by the filename (the
     * file is opened for reading only; it cannot be written to). A new file info is created with this file info and the
     * old DICOM dictionary is used. Finally, the endianess is set to be little-endian.
     * 
     * <p>
     * Should an out-of-memory condition occur an IOException will be thrown.
     * </p>
     * 
     * @param fName File name for the image file.
     * @param fDir Directory to locate the image file.
     * @param refInfo The reference file info, containing the reference tag table.
     * 
     * @exception IOException if there is an error constructing the files.
     */
    public final void setFileName(final String fName, final String fDir, final FileInfoDicom refInfo)
            throws IOException {
        this.setFileName(new File(fDir + fName), refInfo);
    }

    /**
     * sets the quiet option on the class.
     * 
     * <p>
     * The idea is that for all output messages, there should be the option to not bother the user with the message.
     * Ie., if an exception is thrown, and we normally tell the user that an error occurred, a calling class can set
     * this option so that the calling class can handle (either loudly or quietly, as needed) the error with its own
     * message. this could be upgraded to call with quiet to prevent user-queries from interrupting an automatic
     * process. But that is in the future.
     * </p>
     * 
     * <p>
     * Note: In the future, this method and variable is to be moved to FileBase.
     * </p>
     * 
     * @param q whether this class should consider itself quiet, and by internal inspection, not notify the user.
     *            <code>True</code> is to consider itself to not notify the user. <code>False</code> is to notify
     *            the user, and is the default behaviour.
     */
    public final void setQuiet(final boolean q) {
        quiet = q;
    }

    /**
     * Writes a dicom format type image.
     * 
     * @param image Image model where the data is stored.
     * @param start Where in the buffer to start (begin slice).
     * @param end Where in the buffer to end (end slice).
     * @param index Index of the file information.
     * 
     * @exception IOException if there is an error writing the file
     * 
     * @see FileRawChunk
     */
    public void writeImage(final ModelImage image, final int start, final int end, final int index,
            final boolean saveAsEncapJP2) throws IOException {
        float[] data = null;
        double[] doubleData = null;
        BitSet bufferBitSet = null;
        short[] dataRGB = null;

        fileInfo = (FileInfoDicom) image.getFileInfo(index);

        if (stampSecondary) {
            // store that this DICOM has been saved/modified since original capture:
            stampSecondaryCapture(fileInfo);
        }

        this.image = image;

        final int length = end - start;

        try {
            raFile.setLength(0);
            writeHeader(raFile, fileInfo, saveAsEncapJP2);

            // changed this to happen for ALL files, because it's necessary for CT
            // images, or else the min-max gets messed up.
            // if (fileInfo.getModality() == fileInfo.POSITRON_EMISSION_TOMOGRAPHY) {
            if (fileInfo.getDataType() == ModelStorageBase.ARGB) {
                dataRGB = new short[4 * (end - start)];
            } else if (fileInfo.getDataType() == ModelStorageBase.UINTEGER) {
            	doubleData = new double[(end - start)];
            } else if (fileInfo.getDataType() == ModelStorageBase.BOOLEAN) {
                bufferBitSet = new BitSet(end - start);	
            } else {
                data = new float[ (end - start)];
            }

            final double invSlope = image.getFileInfo(index).getRescaleSlope(); // Varies per slice!!!
            final double intercept = image.getFileInfo(index).getRescaleIntercept();
            FileRawChunk rawChunkFile;
            rawChunkFile = new FileRawChunk(raFile, fileInfo);

            if (saveAsEncapJP2) {

                ByteArrayOutputStream buff;
                final String outfile = fileDir + fileName;

                final ParameterList defpl = new ParameterList();
                final String[][] param = Encoder.getAllParameters();
                for (int i = param.length - 1; i >= 0; i--) {
                    if (param[i][3] != null) {
                        defpl.put(param[i][0], param[i][3]);
                    }
                }

                // Create parameter list using defaults
                final ParameterList pl = new ParameterList(defpl);
                pl.put("i", "something.raw"); // to make the EncoderRAW happy
                pl.put("o", outfile);
                pl.put("lossless", "on");
                pl.put("verbose", "off");
                pl.put("Mct", "off");
                pl.put("file_format", "on");
                pl.put("disable_jp2_extension", "on");
                final int imgType = image.getType();
                if (imgType == ModelStorageBase.ARGB) {
                    pl.put("Mct", "on");
                } else {
                    pl.put("Mct", "off");
                }
                if (imgType == ModelStorageBase.ARGB) {
                    final EncoderRAWColor encRAW = new EncoderRAWColor(pl, image);
                    final ImgReaderRAWColorSlice slice = new ImgReaderRAWColorSlice(image, 0, saveAsEncapJP2);
                    slice.setSliceIndex(index, true);
                    buff = encRAW.run1Slice(slice);
                } else {
                    final EncoderRAW encRAW = new EncoderRAW(pl, image);
                    final ImgReaderRAWSlice slice = new ImgReaderRAWSlice(image, 0, saveAsEncapJP2);
                    slice.setSliceIndex(index, true);
                    buff = encRAW.run1Slice(slice);
                }

                final byte[] theData = buff.toByteArray();
                final int size = theData.length;

                final boolean endianess = fileInfo.getEndianess();

                writeShort((short) 0xFFFE, endianess);
                writeShort((short) 0xE000, endianess);
                writeInt(0x00000000, endianess);

                writeShort((short) 0xFFFE, endianess);
                writeShort((short) 0xE000, endianess);
                writeInt(size, endianess);
                raFile.write(theData);
                writeShort((short) 0xFFFE, endianess);
                writeShort((short) 0xE0DD, endianess);
                writeInt(0x00000000, endianess);

            } else {

                switch (fileInfo.getDataType()) {
                
                    case ModelStorageBase.BOOLEAN:
                    	// Required for Segmentation Image Module
                    	// Bits Allocated (0028,0100) If segmentation type (0062,0001) is
                    	// BINARY, shall be 1.  Otherwise, it shall be 8.
                    	// Bits Stored (0028,0101) If segmentation type (0062,0001) is
                    	// BINARY, shall be 1.  Otherwise, it shall be 8.
                    	// C.8.20.2.1 Bits Allocated and Bits Stored
                    	// As a consequence of the enumerated Bits Allocated and Bits Stored
                    	// attribute values, single bit pixels shall be packed 8 to a byte
                    	// as defined by the encoding rules in PS 3.5.
                        byte bufferByte[] = new byte[(length + 7) >> 3];
                        image.exportData(index * length, length, bufferBitSet);
                        
                        for (int i = 0; i < bufferByte.length; i++) {
                            bufferByte[i] = 0;
                        }

                        for (int i = 0; i < length; i++) {

                            if (bufferBitSet.get(i)) {
                                bufferByte[i >> 3] |= (1 << (7-(i % 8)));
                            }
                        }

                        raFile.write(bufferByte);
                        break;

                    case ModelStorageBase.BYTE:

                        byte[] data2 = new byte[length];
                        image.exportData(index * length, length, data);
                        for (int i = 0; i < data.length; i++) {
                            data2[i] = (byte) MipavMath.round( (data[i] - intercept) / invSlope);
                        }

                        rawChunkFile.writeBufferByte(data2, 0, length);
                        data2 = null;
                        break;

                    case ModelStorageBase.UBYTE:

                        short[] data3 = new short[length];
                        image.exportData(index * length, length, data);
                        for (int i = 0; i < data.length; i++) {
                            data3[i] = (short) MipavMath.round( (data[i] - intercept) / invSlope);
                        }

                        rawChunkFile.writeBufferUByte(data3, 0, length);
                        data3 = null;
                        break;

                    case ModelStorageBase.SHORT:

                        short[] data4 = new short[end - start];
                        image.exportData(index * length, length, data);
                        for (int i = 0; i < data.length; i++) {
                            data4[i] = (short) MipavMath.round( (data[i] - intercept) / invSlope);
                        }

                        rawChunkFile.writeBufferShort(data4, 0, length, image.getFileInfo(0).getEndianess());
                        data4 = null;
                        break;

                    case ModelStorageBase.USHORT:

                        int[] data5 = new int[length];

                        // int tmpInt;
                        image.exportData(index * length, length, data);

                        // System.out.println(" Intercept = " + intercept + " invSlope = " + invSlope);
                        for (int i = 0; i < data.length; i++) {
                            data5[i] = (short) MipavMath.round( (data[i] - intercept) / invSlope);
                        }

                        rawChunkFile.writeBufferUShort(data5, 0, length, image.getFileInfo(0).getEndianess());
                        data5 = null;
                        break;
                        
                    case ModelStorageBase.UINTEGER:
                    	// Required for RT Doses
                    	// C.8.8.3.4.3 Bits Allocated
                    	// For RT Doses, Bits Allocated (0028,0100) shall have an enumerated Value of 16 or 32
                    	// C.8.8.3.4.4 Bits Stored
                    	// For RT Doses, Bits Stored (0028,0101) shall have an Enumerated Value equal to Bits
                    	// Allocated (0028,0100)
                    	// C.8.8.3.4.6 Pixel Representation
                    	// For RT Doses, Pixel Representation (0028,0101) is specified to use the following
                    	// Enumerated Values:
                    	// 0001H = two's complement integer, when Dose Type (3004,0004) = ERROR
                    	// 0000H = unsigned integer, otherwise
                    	long[] data6 = new long[length];
                    	image.exportData(index * length, length, doubleData);
                    	
                    	for (int i = 0; i < doubleData.length; i++) {
                            data6[i] = Math.round( (doubleData[i] - intercept) / invSlope);
                        }
                    	
                    	rawChunkFile.writeBufferUInt(data6, 0, length, image.getFileInfo(0).getEndianess());
                        data6 = null;
                        break;
                        
                    
                    case ModelStorageBase.ARGB:

                        short[] dRGB = new short[3 * length];
                        image.exportData(index * (4 * length), (4 * length), dataRGB);

                        for (int i = 1, iRGB = 0; i < dataRGB.length;) {

                            // Not sure slope intercept is needed here for color images
                            dRGB[iRGB++] = (short) MipavMath.round( (dataRGB[i++] - intercept) / invSlope);
                            dRGB[iRGB++] = (short) MipavMath.round( (dataRGB[i++] - intercept) / invSlope);
                            dRGB[iRGB++] = (short) MipavMath.round( (dataRGB[i++] - intercept) / invSlope);
                            i++;
                        }

                        rawChunkFile.writeBufferUByte(dRGB, 0, 3 * length);
                        dRGB = null;
                        break;

                    default:

                        // Mod this to output to debug.
                        System.out.println("Unsupported data type: " + fileInfo.getDataType());

                        // left system.out, but added debug; dp, 2003-5-7:
                        Preferences.debug("Unsupported data type: " + fileInfo.getDataType() + "\n");
                }
            }

            rawChunkFile.close();
            data = null;
            dataRGB = null;
            // fileInfo = null;
        } catch (final IOException error) {
            throw new IOException("FileDicomWrite: " + error);
        } catch (final OutOfMemoryError error) {

            if ( !isQuiet()) {
                MipavUtil.displayError("Out of Memory in FileDicom.writeImage");
            }

            throw new IOException();
        }
    }

    /**
     * Writes a dicom format type image.
     * 
     * @param image Image model where the data is stored.
     * @param start Start image index (begin slice).
     * @param end End image index (end slice).
     * 
     * @exception IOException if there is an error writing the file.
     * 
     * @see FileRawChunk
     */
    public void writeMultiFrameImage(final ModelImage image, final int start, final int end) throws IOException {

        fileInfo = (FileInfoDicom) image.getFileInfo(0);

        final String nFramesStr = String.valueOf(end - start + 1);
        fileInfo.getTagTable().setValue("0028,0008", nFramesStr, nFramesStr.length());

        if (stampSecondary) {
            // store that this DICOM has been saved/modified since original capture:
            stampSecondaryCapture(fileInfo);
        }
        this.image = image;

        final int imageSize = image.getSliceSize();

        try {
            writeHeader(raFile, fileInfo, false);

            if (fileInfo.getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY) {
                final float[] data = new float[imageSize];

                // if the data type isn't short, this will be majorly screwed up
                final double invSlope = fileInfo.getRescaleSlope();
                final double intercept = fileInfo.getRescaleIntercept();
                final short[] data2 = new short[imageSize];

                image.exportData(0, imageSize, data);
                image.reallocate(fileInfo.getDataType());

                for (int i = 0; i < data.length; i++) {
                    data2[i] = (short) MipavMath.round( (data[i] - intercept) / invSlope);
                }

                image.importData(0, data2, false);
            }

            FileRawChunk rawChunkFile;
            rawChunkFile = new FileRawChunk(raFile, fileInfo);

            for (int i = start; i <= end; i++) {
                rawChunkFile.writeImage(image, i * imageSize, (i * imageSize) + imageSize, i);
            }

            rawChunkFile.close();
        } catch (final IOException error) {
            throw new IOException("FileDicomWrite: " + error);
        } catch (final OutOfMemoryError error) {

            if ( !isQuiet()) {
                MipavUtil.displayError("Out of Memory in FileDicom.writeImage");
            }

            throw new IOException();
        }
    }

    /**
     * Converts the integer values of the group word and element word into a string that is the hexadecimal
     * representation of group word and element word, separated by a comma.
     * 
     * @param groupWord The group word of the element.
     * @param elementWord The element word of the element.
     * 
     * @return String representation of the group element.
     */
    private String convertGroupElement(final int groupWord, final int elementWord) {
        String first, second;

        first = Integer.toString(groupWord, 16);

        while (first.length() < 4) { // prepend with '0' as needed
            first = "0" + first;
        }

        first = first.toUpperCase();
        second = Integer.toString(elementWord, 16);

        while (second.length() < 4) { // prepend with '0' as needed
            second = "0" + second;
        }

        second = second.toUpperCase();

        return (first + "," + second); // name is the hex string of the tag
    }

    private int[] encapsulatedJP2ImageData(final int imageType) throws IOException {
        // System.out.println("FileDicom.encapsulatedImageData");

        try {

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (final IOException ex) {}
            }

            fileHeader = new File(fileDir + fileName);
            raFile = new RandomAccessFile(fileHeader, "r");
        } catch (final IOException e) {

            try {
                raFile = new RandomAccessFile(fileHeader, "r");
            } catch (final IOException error) {}
        }

        initializeFullRead();
        seek(fileInfo.getOffset());

        final boolean endianess = fileInfo.getEndianess();
        getNextElement(endianess); // gets group, element, length

        String name = convertGroupElement(groupWord, elementWord);

        if ( !name.equals(FileDicom.IMAGE_TAG)) {

            if ( !isQuiet()) {
                MipavUtil.displayError("FileDicom: Image Data tag not found.  Cannot extract encapsulated image data.");
            }

            throw new IOException("Image Data tag not found.  Cannot extract encapsulated image data.");
        }

        inSQ = true;
        getNextElement(endianess);
        name = convertGroupElement(groupWord, elementWord);

        int[] tableOffsets;

        if (elementLength > 0) {
            final int numberInts = elementLength / 4;
            tableOffsets = new int[numberInts];

            for (int i = 0; i < numberInts; i++) {
                tableOffsets[i] = getInt(endianess);
            }
        }

        getNextElement(endianess);
        name = convertGroupElement(groupWord, elementWord);

        final Vector<byte[]> v = new Vector<byte[]>();
        byte[] imageFrag = null;
        while (name.equals(FileDicom.SEQ_ITEM_BEGIN)) {

            imageFrag = new byte[elementLength]; // temp buffer

            for (int i = 0; i < imageFrag.length; i++) {
                imageFrag[i] = (byte) getByte(); // move into temp buffer
            }

            v.add(imageFrag);
            getNextElement(endianess);
            name = convertGroupElement(groupWord, elementWord);
        }

        if ( !name.equals(FileDicom.SEQ_ITEM_END) && !name.equals(FileDicom.SEQ_ITEM_UNDEF_END)) {

            if ( !isQuiet()) {
                MipavUtil.displayWarning("End tag not present.  Image may have been corrupted.");
            }
        }

        raFile.close();

        final FileJP2 fileJP2 = new FileJP2();

        return fileJP2.decodeImageData(imageFrag, imageType);
    }

    /**
     * Starts with image tag. goes with OB/OW. image file must be open, file ptr: raFile.
     * 
     * @return int[] the extracted/decapsulated RGB image data
     * 
     * @exception IOException it cannot return the RGB image data, it'll just throw it.
     */
    private int[] encapsulatedImageData() throws IOException {

        // System.out.println("FileDicom.encapsulatedImageData");

        try {

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (final IOException ex) {}
            }

            fileHeader = new File(fileDir + fileName);
            raFile = new RandomAccessFile(fileHeader, "r");
        } catch (final IOException e) {

            try {
                raFile = new RandomAccessFile(fileHeader, "r");
            } catch (final IOException error) {}
        }

        initializeFullRead();
        seek(fileInfo.getOffset());

        final boolean endianess = fileInfo.getEndianess();
        getNextElement(endianess); // gets group, element, length

        String name = convertGroupElement(groupWord, elementWord);

        if ( !name.equals(FileDicom.IMAGE_TAG)) {

            if ( !isQuiet()) {
                MipavUtil.displayError("FileDicom: Image Data tag not found.  Cannot extract encapsulated image data.");
            }

            throw new IOException("Image Data tag not found.  Cannot extract encapsulated image data.");
        }

        inSQ = true;
        getNextElement(endianess);
        name = convertGroupElement(groupWord, elementWord);

        int[] tableOffsets;

        if (elementLength > 0) {
            final int numberInts = elementLength / 4;
            tableOffsets = new int[numberInts];

            for (int i = 0; i < numberInts; i++) {
                tableOffsets[i] = getInt(endianess);
            }
        }

        getNextElement(endianess);
        name = convertGroupElement(groupWord, elementWord);

        final Vector<byte[]> v = new Vector<byte[]>();

        while (name.equals(FileDicom.SEQ_ITEM_BEGIN)) {

            final byte[] imageFrag = new byte[elementLength]; // temp buffer

            for (int i = 0; i < imageFrag.length; i++) {
                imageFrag[i] = (byte) getByte(); // move into temp buffer
            }

            v.add(imageFrag);
            getNextElement(endianess);
            name = convertGroupElement(groupWord, elementWord);
        }

        if ( !name.equals(FileDicom.SEQ_ITEM_END) && !name.equals(FileDicom.SEQ_ITEM_UNDEF_END)) {

            if ( !isQuiet()) {
                MipavUtil.displayWarning("End tag not present.  Image may have been corrupted.");
            }
        }

        raFile.close();

        if (encapsulatedJP2) {

        }

        if (v.size() > 1) {
            final Vector<int[]> v2 = new Vector<int[]>();
            int[] jpegImage;

            for (final byte[] name2 : v) {

                if (lossy == true) {
                    jpegImage = extractLossyJPEGImage(name2);
                } else {
                    final FileDicomJPEG fileReader = new FileDicomJPEG(name2, fileInfo.getExtents()[0], fileInfo
                            .getExtents()[1]);
                    jpegImage = fileReader.extractJPEGImage();
                }

                v2.addElement(jpegImage);
            }

            int size = 0;

            for (final int[] name2 : v2) {
                size += name2.length;
            }

            int count = 0;
            final int[] imageData = new int[size];

            for (final int[] temp : v2) {
                for (final int element : temp) {
                    imageData[count++] = element;
                }
            }

            inSQ = false;

            return imageData;

        } else {

            if (lossy == true) {
                inSQ = false;

                return extractLossyJPEGImage(v.elementAt(0));
            } else {
                final FileDicomJPEG fileReader = new FileDicomJPEG(v.elementAt(0), fileInfo.getExtents()[0], fileInfo
                        .getExtents()[1]);
                inSQ = false;

                return fileReader.extractJPEGImage();
            }
        }
    }

    /**
     * Brute force removal of 1st fragment of JPEG encoded image. Cuts out JPEG encapsulated data and stores a temp JPEG
     * image to the temp directory. Calls the MediaTracker utility to decode the JPEG image.
     * 
     * @param imageFrag Image fragment.
     * 
     * @return RGB image buffer (int[])- the JPEG image
     * 
     * @throws IOException DOCUMENT ME!
     */
    private int[] extractLossyJPEGImage(final byte[] imageFrag) throws IOException {
        int w = 0, h = 0;

        final ByteArrayInputStream stream = new ByteArrayInputStream(imageFrag);
        final BufferedImage img = ImageIO.read(stream);
        if (img == null) {
            MipavUtil.displayError("In extractLossyJPEGImage ImageIO.read(stream) no registered ImageReader claims"
                    + " to be able to read the stream");
            Preferences.debug("In extractLossyJPEGImage ImageIO.read(stream) no registered ImageReader claims\n");
            Preferences.debug("to be able the read the stream\n");
            throw new IOException();
        }

        w = img.getWidth(null);
        h = img.getHeight(null);

        if ( (w <= 0) || (h <= 0)) {
            return new int[0];
        }

        // This is for RGB images
        final int[] pixels = new int[w * h];
        final int[] imgBuffer = new int[4 * w * h];
        final PixelGrabber pg = new PixelGrabber(img, 0, 0, w, h, pixels, 0, w);

        try {
            pg.grabPixels();
        } catch (final InterruptedException e) {
            Preferences.debug("Interrupted waiting for pixels!" + "\n");

            return new int[0];
        }

        if ( (pg.getStatus() & ImageObserver.ABORT) != 0) {
            Preferences.debug("Image fetch aborted or errored" + "\n");

            return new int[0];
        }

        // copy the decoded JPEG image into the destination buffer
        int a, r, g, b, pixel;
        int i = 0;

        for (int y = 0; y < h; y++) {

            for (int x = 0; x < w; x++) {
                pixel = pixels[ (y * w) + x];
                a = (pixel >> 24) & 0xff;
                r = (pixel >> 16) & 0xff;
                g = (pixel >> 8) & 0xff;
                b = (pixel) & 0xff;

                imgBuffer[i] = a;
                imgBuffer[i + 1] = r;
                imgBuffer[i + 2] = g;
                imgBuffer[i + 3] = b;
                i += 4;
            }
        }

        return imgBuffer;
    }

    /**
     * Reads a length of the data and deposits it into a single Short or an array of Short as needed by the tag's VM.
     * 
     * @return Object
     * 
     * @throws IOException DOCUMENT ME!
     * 
     * @param vm value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param length number of bytes to read out of data stream; the length is not used.
     * @param endianess byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
     *            indicates little-endian.
     */
    private Object getByte(final int vm, final int length, final boolean endianess) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null; // the Object we read in

        if (vm > 1) {
            final Byte[] array = new Byte[length];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Byte((byte) getByte());
                len -= 1;
                i++;
            }

            readObject = array;
        } else if ( (vm < 1) && (length > 2)) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            final Byte[] array = new Byte[length];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Byte((byte) getByte());
                len -= 1;
                i++;
            }

            readObject = array;
        } else if (length > 0) {
            final Byte[] array = new Byte[length];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Byte((byte) getByte());
                len -= 1;
                i++;
            }

            readObject = array;
        }

        return readObject;
    }

    /**
     * Reads a pallete (look-up table) out of the given DICOM key and interprets the data based on the data-size
     * specification tag. The DICOM key given defines for which colour table (either red, blue or green) the pallete is
     * to be active. Only DICOM keys <tt>0028,1201</tt>, <tt>0028,1202</tt> and <tt>0028,1203</tt> are accepted
     * for use in this method and all others cause an exception to be thrown. If the DICOM color pallete size tag exists
     * in the dictionary and is correct, then the data is read out of the color pallete tag as-specified; if the size in
     * the color pallete tag is invalid for DICOM standard v3, the data is assumed to be 16 bits wide; if the
     * size-specifier tag does not exist, the data in this tag is ignored. Parsing one particular key does not verify
     * that the other colour channel pallete keys exist.
     * 
     * <p>
     * While this method ensures that the tag in the dictionary is set
     * {@link FileInfoDicom#setValue(String, Object, int)}, it does not actually load the ModelLUT {@link ModelLUT}
     * </p>
     * 
     * @param palleteKey The DICOM key which contains some pallete or LUT information.
     * 
     * @throws IllegalArgumentException When a DICOM key is provided that is not related to the pallete information;
     *             that is, one that is not <tt>0028,1201</tt>, <tt>0028,1202</tt>, or <tt>
     *                                    0028,1203</tt>.
     * @throws IOException A problem occurs when reading the image file.
     * 
     * @see "DICOM PS 3.3, Information Object Definitions"
     */
    private void getColorPallete(final FileDicomKey palleteKey) throws IllegalArgumentException, IOException {

        if ( !palleteKey.equals("0028,1201") && !palleteKey.equals("0028,1202") && !palleteKey.equals("0028,1203")) {
            System.out.println("Throwing exception in FileDicom:storeColorPallete.");
            throw new IllegalArgumentException("Not a color pallete tag");
        }

        try {

            // get channel lut specification.
            final String specKey = palleteKey.getGroup() + ","
                    + Integer.toString(Integer.parseInt(palleteKey.getElement()) - 100);
            final FileDicomTag palletSpec = fileInfo.getTagTable().get(specKey);

            // values are now loaded, try loading the LUT
            int numberOfLUTValues = ( ((Short) (palletSpec.getValueList()[0])).intValue() == 0) ? (int) Math.pow(2, 16)
                    : ((Short) (palletSpec.getValueList()[0])).intValue();

            final int numberOfBits = ((Short) (palletSpec.getValueList()[2])).intValue();
            // System.out.println("Writing storeColorPallete : number of Bits = " + numberOfBits );

            if ( (numberOfBits != 8) && (numberOfBits != 0x10)) {

                // this seemed like a good exception to throw in this case.
                // we catch it immediatly anyway, so we can notify the
                // user and still close the file.
                throw new ArrayStoreException("Cannot store " + numberOfBits + " bit LUTs");
            }

            Object data;

            if (numberOfBits == 8) {
                data = readUnknownData(); // automatically reads data as bytes
            } else {
                numberOfLUTValues *= 2;
                data = getShort(numberOfLUTValues, elementLength, true);
            }

            fileInfo.getTagTable().setValue(palleteKey.getKey(), data, numberOfLUTValues);
        } catch (final NullPointerException npe) {
            final FileDicomTag t = fileInfo.getTagTable().get(palleteKey);
            MipavUtil.displayError("This image carries " + t.getName() + ", but specifies it incorrectly.\n"
                    + "The channel will be ignored.");
            Preferences.debug("FileDicom: Error Creating " + t.getName() + "(" + palleteKey.getKey() + ")"
                    + "; it will be ignored.\n");
        } catch (final ArrayStoreException ase) {
            final FileDicomTag t = fileInfo.getTagTable().get(palleteKey);
            MipavUtil.displayError(t.getName() + "; " + ase.getLocalizedMessage() + "\n"
                    + "This channel will be ignored.");
            Preferences.debug("FileDicom: Error creating " + t.getName() + "; " + ase.getLocalizedMessage() + "\n");
        } catch (final ArrayIndexOutOfBoundsException aioobe) {
            MipavUtil.displayError("Error reading color LUT channel.  This Channel will be ignored.");
            Preferences.debug("Error reading color LUT channel.  This Channel will be ignored.\n");
            // System.err.println("Error reading color LUT channel. This Channel will be ignored.");
        }
    }

    /**
     * Reads a set of DICOM tags from a DICOM sequence item, ending with the data-element end tag <code>
     * FFFE,E00D</code>.
     * This list of tags in a DICOM sequence item and places them into a hashtable held within a FileDicomItem.
     * 
     * @param itemLength Length of the item in bytes.
     * @param endianess Big (true) or little (false).
     * 
     * @return The sequence item read in.
     * 
     * @see FileDicomItem
     */
    private Object getDataSet(int itemLength, final boolean endianess) throws IOException {
        final FileDicomItem item = new FileDicomItem();

        String type;
        int iValue = 0;
        float fValue = 0;
        double dValue = 0;
        boolean nullEntry = false;

        if ( (itemLength == UNDEFINED_LENGTH) || (itemLength == ILLEGAL_LENGTH)) {
            itemLength = Integer.MAX_VALUE;
        }

        if (itemLength == 0) {
            itemLength = elementLength; // use the length of the SQ tag
        }

        final int startfptr = getFilePointer();

        // preread the next tag, because we do not want to store the
        if ( (getFilePointer() - startfptr) <= itemLength) {

            // Sequence ITEM delimiter tags
            final boolean oldSQ = inSQ;
            inSQ = true;
            getNextElement(endianess);
            nameSQ = convertGroupElement(groupWord, elementWord);

            // System.err.println("nameSQ " + nameSQ);
            inSQ = oldSQ;
        }
        // Preferences.debug("Item: "+Integer.toString(groupWord, 0x10)+","+
        // Integer.toString(elementWord, 0x10)+
        // " for " + Integer.toString(elementLength, 0x10) +
        // " # readfrom: " + Long.toString(getFilePointer(), 0x10) + "\n");

        // either there's an "item end" or we've read the entire element length
        while ( !nameSQ.equals(FileDicom.SEQ_ITEM_END) && ( (getFilePointer() - startfptr) < itemLength)
                && (getFilePointer() < raFile.length())) {
            // The following is almost exactly the same as the code in readHeader. The main difference is the
            // information is stored in a hashtable in DicomItem that is initially empty.

            FileDicomTag entry;

            if (fileInfo.vr_type == FileInfoDicom.IMPLICIT) {
                FileDicomTagInfo info;

                if (DicomDictionary.containsTag(new FileDicomKey(groupWord, elementWord))) {
                    FileDicomKey key = null;

                    info = DicomDictionary.getInfo(key = new FileDicomKey(groupWord, elementWord));
                    // is is required if DicomDictionary contains wild card characters
                    info.setKey(key);
                    entry = new FileDicomTag(info);
                    type = entry.getType();
                } else {

                    // the tag was not found in the dictionary..
                    entry = new FileDicomTag(new FileDicomTagInfo(new FileDicomKey(groupWord, elementWord), null, 0,
                            "PrivateTag", "Private tag"));
                    type = FileDicomBase.TYPE_UNKNOWN;
                }
            } else {
                FileDicomTagInfo info;

                if (DicomDictionary.containsTag(new FileDicomKey(groupWord, elementWord))) {
                    FileDicomKey key = null;
                    info = (FileDicomTagInfo) DicomDictionary.getInfo(key = new FileDicomKey(groupWord, elementWord))
                            .clone();
                    // this is required if DicomDictionary contains wild card characters
                    info.setKey(key);
                    entry = new FileDicomTag(info);
                    entry.setValueRepresentation(new String(vr));
                } else {
                    info = new FileDicomTagInfo(new FileDicomKey(groupWord, elementWord), new String(vr), 0,
                            "PrivateTag", "Private tag");
                    entry = new FileDicomTag(info);
                }

                if (fileInfo.isCurrentTagSQ) {
                    type = FileDicomBase.TYPE_SEQUENCE;
                } else {
                    type = entry.getType();
                }

                nullEntry = true;
            }

            try {

                // (type == "typeUnknown" && elementLength == -1) Implicit sequence tag if not in DICOM dictionary.
                if (type.equals(FileDicomBase.TYPE_UNKNOWN) && (elementLength != -1)) {
                    final FileDicomTagInfo info = entry.getInfo();

                    entry = new FileDicomTag(info, readUnknownData());

                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                } else if (type.equals(FileDicomBase.TYPE_STRING) || type.equals(FileDicomBase.OTHER_WORD_STRING)) {

                    if (elementLength == UNDEFINED_LENGTH) {
                        elementLength = 0;
                    }

                    final String strValue = getString(elementLength);
                    entry.setValue(strValue, elementLength);
                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                    // Preferences.debug("aaaaaaString Tag: (" + nameSQ + ");\t" + type + "; value = " + strValue + ";
                    // element length = "+ elementLength + "\n", 2);
                } else if (type.equals(FileDicomBase.OTHER_BYTE_STRING)) {

                    if ( !nullEntry && (DicomDictionary.getVM(new FileDicomKey(nameSQ)) > 1)) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else if (elementLength >= 2) {
                        entry.setValue(readUnknownData(), elementLength);
                    }

                    entry.setValueRepresentation("OB");
                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + iValue + "; element length
                    // = "+ elementLength + "\n", 2);
                } else if (type.equals(FileDicomBase.TYPE_SHORT)) {

                    if ( !nullEntry && (DicomDictionary.getVM(new FileDicomKey(nameSQ)) > 1)) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else if (elementLength > 2) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else {
                        iValue = getUnsignedShort(endianess);
                        entry.setValue(new Short((short) iValue), elementLength);
                    }

                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + iValue + "; element length
                    // = "+ elementLength + "\n", 2);
                } else if (type.equals(FileDicomBase.TYPE_INT)) {

                    if ( !nullEntry && (DicomDictionary.getVM(new FileDicomKey(nameSQ)) > 1)) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else if (elementLength > 4) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else {
                        iValue = getInt(endianess);
                        entry.setValue(new Integer(iValue), elementLength);
                    }

                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + iValue + "; element length
                    // = "+ elementLength + "\n", 2);
                } else if (type.equals(FileDicomBase.TYPE_FLOAT)) {

                    if ( !nullEntry && (DicomDictionary.getVM(new FileDicomKey(nameSQ)) > 1)) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else if (elementLength > 4) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else {
                        fValue = getFloat(endianess);
                        entry.setValue(new Float(fValue), elementLength);
                    }

                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + fValue + "; element length
                    // = "+ elementLength + "\n", 2);
                } else if (type.equals(FileDicomBase.TYPE_DOUBLE)) {

                    if ( !nullEntry && (DicomDictionary.getVM(new FileDicomKey(nameSQ)) > 1)) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else if (elementLength % 8 == 0 && elementLength != 8) {
                        final Double[] dArr = new Double[elementLength / 8];
                        for (int i = 0; i < dArr.length; i++) {
                            dArr[i] = getDouble(endianess);
                        }
                        entry.setValue(dArr, elementLength);
                    } else if (elementLength > 8) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else {
                        dValue = getDouble(endianess);
                        entry.setValue(new Double(dValue), elementLength);
                    }

                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + dValue + "; element length
                    // = "+ elementLength + "\n", 2);
                }
                // (type == "typeUnknown" && elementLength == -1) Implicit sequence tag if not in DICOM dictionary.
                else if (type.equals(FileDicomBase.TYPE_SEQUENCE)
                        || ( (type == FileDicomBase.TYPE_UNKNOWN) && (elementLength == -1))) {
                    final int len = elementLength;
                    final String name = nameSQ;
                    final Object sq2 = getSequence(endianess, len);
                    entry.setValue(sq2, len);
                    item.putTag(name, entry);
                    item.setLength(name, len);
                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type +
                    // "; element length = "+ elementLength + "\n", 2);
                }
            } catch (final OutOfMemoryError e) {

                if ( !isQuiet()) {

                    // Must add back Matt/Dave 11/2003
                    MipavUtil.displayError("Out of memory in FileDicom.getDataSet");
                    // yup, done.... Dave, 04-2004
                } else {
                    Preferences.debug("Out of memory in FileDicom.getDataSet\n");
                    // System.err.println("Out of memory in FileDicom.getDataSet");
                }

                throw new IOException();
            }

            if ( ( (getFilePointer() - startfptr) < itemLength) && (getFilePointer() < raFile.length())) {
                // preread the next tag, because we have yet to see the
                // end-sequence tag because we don't want to accidently
                // read the next new-item tag.

                // except that SQ is undef-len, and def-len items get an
                // extra tag read out. // 30-jun-2004
                getNextElement(endianess);
                nameSQ = convertGroupElement(groupWord, elementWord);

                // Preferences.debug("Item: "+Integer.toString(groupWord, 0x10)+","+
                // Integer.toString(elementWord, 0x10)+
                // " for " + Integer.toString(elementLength, 0x10) +
                // " # readfrom: " + Long.toString(getFilePointer(), 0x10) + "\n");

            }
        }

        // System.err.println("\t\t\tItem done.");
        return item;
    }

    /**
     * Reads a length of the data and deposits it into a single Double or an array of Double as needed by the tag's VM.
     * 
     * @return Object
     * 
     * @throws IOException DOCUMENT ME!
     * 
     * @param vm value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param length number of bytes to read out of data stream; the length is not used.
     * @param endianess byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
     *            indicates little-endian.
     */
    private Object getDouble(final int vm, final int length, final boolean endianess) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null;

        if (vm > 1) {
            final Double[] array = new Double[length / 8];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Double(getDouble(endianess));
                len -= 8;
                i++;
            }

            readObject = array;
        } else if ( (vm < 1) && (length > 8)) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data
            // we actually do it as above.
            final Double[] array = new Double[length / 8];

            while (len > 0) {
                array[i] = new Double(getDouble(endianess));
                len -= 8;
                i++;
            }

            readObject = array;
        } else if ( ( (vm == 1) && (length > 8))) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            readObject = new Double(getDouble(endianess));
            len -= 8;

            while (len > 0) { // we should validate with VM here too
                getDouble(endianess);
                len -= 8;
                i++;
            }
        } else if (length == 8) {
            readObject = new Double(getDouble(endianess));
        }

        return readObject;
    }

    /**
     * Reads a length of the data and deposits it into a single Float or an array of Float as needed by the tag's VM.
     * 
     * @return Object
     * 
     * @throws IOException DOCUMENT ME!
     * 
     * @param vm value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param length number of bytes to read out of data stream; the length is not used.
     * @param endianess byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
     *            indicates little-endian.
     */
    private Object getFloat(final int vm, final int length, final boolean endianess) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null;

        if (vm > 1) {
            final Float[] array = new Float[length / 4];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Float(getFloat(endianess));
                len -= 4;
                i++;
            }

            readObject = array;
        } else if ( (vm < 1) && (length > 4)) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data
            // we actually do it as above.
            final Float[] array = new Float[length / 4];

            while (len > 0) {
                array[i] = new Float(getFloat(endianess));
                len -= 4;
                i++;
            }

            readObject = array;
        } else if ( ( (vm == 1) && (length > 4))) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            readObject = new Float(getFloat(endianess));
            len -= 4;

            while (len > 0) { // we should validate with VM here too
                getFloat(endianess);
                len -= 4;
                i++;
            }
        } else if (length == 4) {
            readObject = new Float(getFloat(endianess));
        }

        return readObject;
    }

    /**
     * Reads a length of the data and deposits it into a single Integer or an array of Integer as needed by the tag's
     * VM.
     * 
     * @return Object
     * 
     * @throws IOException DOCUMENT ME!
     * 
     * @param vm value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param length number of bytes to read out of data stream; the length is not used.
     * @param endianess byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
     *            indicates little-endian.
     */
    private Object getInteger(final int vm, final int length, final boolean endianess) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null;

        if (vm > 1) {
            final Integer[] array = new Integer[length / 4];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Integer(getInt(endianess));
                len -= 4;
                i++;
            }

            readObject = array;
        } else if ( (vm < 1) && (length > 4)) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            final Integer[] array = new Integer[length / 4];

            while (len > 0) {
                array[i] = new Integer(getInt(endianess));
                len -= 4;
                i++;
            }

            readObject = array;
        } else if ( ( (vm == 1) && (length > 4))) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            readObject = new Integer(getInt(endianess));
            len -= 4;

            while (len > 0) { // we should validate with VM here too
                getInt(endianess);
                len -= 4;
                i++;
            }
        } else if (length == 4) {
            readObject = new Integer(getInt(endianess));
        }

        return readObject;
    }

    /**
     * Reads in four integers, then tests for implicit Value Representation(VR) or explicit VR. If explicit, it finds
     * out what the VR is and stores it.
     * 
     * <p>
     * See DICOM Specification Part 5 (1998) Section 7 pages 24-34.
     * </p>
     * 
     * @param endianess Big or little.
     * @param b1 First byte of the tag to be tested before applying endianess.
     * @param b2 Second byte of the tag to be tested before applying endianess.
     * @param b3 Third byte of the tag to be tested before applying endianess.
     * @param b4 Fourth byte of the tag to be tested before applying endianess.
     * 
     * @return Length of the element.
     * 
     * @throws IOException DOCUMENT ME!
     */
    private int getLength(final boolean endianess, final byte b1, final byte b2, final byte b3, final byte b4)
            throws IOException {
        boolean implicit = false;

        if ( (fileInfo.vr_type == FileInfoDicom.IMPLICIT) || (groupWord == 2)) {

            if (fileInfo.containsDICM) {

                // at this point transfer syntax not read; we know endianess
                // is little endian but vr may be explicit
                if ( (getFilePointer() <= (FIRST_ELEMENT + metaGroupLength)) || (groupWord == 2)) {

                    if ( ( (b1 < 65) || (b1 > 90)) && ( (b2 < 65) || (b2 > 90))) {
                        implicit = true;
                    } else {
                        implicit = false;
                    }
                } else {
                    implicit = true; // transfer syntax has been read, implicit set
                }
            } else {

                // at this point transfer syntax not read; we know endianess
                // is little endian but vr may be explicit
                if ( (getFilePointer() <= metaGroupLength) || (groupWord == 2)) {

                    if ( ( (b1 < 65) || (b1 > 90)) && ( (b2 < 65) || (b2 > 90))) {
                        implicit = true;
                    } else {
                        implicit = false;
                    }
                } else {
                    implicit = true; // transfer syntax has been read, implicit set
                }
            }
        }

        // displays the individual bytes. It could be better, for instance
        // printing as individual integer values:
        // System.err.print("[ "+Integer.toString(b1, 0x10)+" " +
        // Integer.toString(b2, 0x10)+" " +
        // Integer.toString(b3, 0x10)+" " +
        // Integer.toString(b4, 0x10)+" ]"
        // );

        if (implicit) {

            // implicit VR with 32-bit length
            if (endianess == FileBase.LITTLE_ENDIAN) {
                return ( (b1 & 0xff) + ( (b2 & 0xff) << 8) + ( (b3 & 0xff) << 16) + ( (b4 & 0xff) << 24));
            } else {
                return ( ( (b1 & 0xff) << 24) + ( (b2 & 0xff) << 16) + ( (b3 & 0xff) << 8) + (b4 & 0xff));
            }
        }
        // explicit VR with 32-bit length
        else if ( ( (b1 == 79) && (b2 == 66)) || ( (b1 == 79) && (b2 == 87)) || ( (b1 == 83) && (b2 == 81))
                || ( (b1 == 85) && (b2 == 78)) || ( (b1 == 85) && (b2 == 84))) {

            // VR = 'OB', or 'OW' or 'SQ' or 'UN' or 'UT'
            vr[0] = b1;
            vr[1] = b2;
            fileInfo.isCurrentTagSQ = new String(vr).equals("SQ");

            // SQ - check for length FFFFFFFF (undefined), otherwise should be 0.
            if ( (b1 == 83) && (b2 == 81)) { // 'SQ'

                // but i can't figure out why we're making a big deal out
                // of SQ types; UNDEF_LENGTH -is- -1 which -is- FF FF FF FF.
                // maybe ensuring return type?
                read(byteBuffer4); // reads 4 byte length w/o endianess for testing

                if ( (byteBuffer4[0] == 255) && (byteBuffer4[1] == 255) && (byteBuffer4[2] == 255)
                        && (byteBuffer4[3] == 255)) {
                    return UNDEFINED_LENGTH;
                } else {
                    seek(getFilePointer() - 0x4);

                    return getInt(endianess); // rereads length using proper endianess
                }
            } else {
                return getInt(endianess);
            }
        }
        // explicit VR with 16-bit length
        else {
            vr[0] = b1; // these are not VR for item tags!!!!!!!!!
            vr[1] = b2;

            fileInfo.isCurrentTagSQ = new String(vr).equals("SQ");

            if (endianess == FileBase.LITTLE_ENDIAN) {
                return ( (b3 & 0xff) | ( (b4 & 0xff) << 8));
            } else {
                return ( ( (b3 & 0xff) << 8) | (b4 & 0xff));
            }
        }
    }

    /**
     * Increments the location, then reads the elementWord, groupWord, and elementLength. It also tests for an end of
     * file and resets the elementWord if it encounters one.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @throws IOException DOCUMENT ME!
     */
    private void getNextElement(final boolean bigEndian) throws IOException {

        groupWord = getUnsignedShort(bigEndian);
        elementWord = getUnsignedShort(bigEndian);
        // Preferences.debug("(just found: )"+Integer.toString(groupWord, 0x10) + ":"+Integer.toString(elementWord,
        // 0x10)+" - " ); System.err.print("( just found: ) "+ Integer.toString(groupWord, 0x10) +
        // ":"+Integer.toString(elementWord, 0x10)+ " - ");

        if (fileInfo.vr_type == FileInfoDicom.EXPLICIT) {

            /*
             * explicit tags carry an extra 4 bytes after the tag (group, element) information to describe the type of
             * tag. the element dictionary describes this info, so we skip past it here. (apr 2004)
             */
            final String tagname = convertGroupElement(groupWord, elementWord);

            if (tagname.equals(FileDicom.SEQ_ITEM_BEGIN) || tagname.equals(FileDicom.SEQ_ITEM_END)
                    || tagname.equals(FileDicom.SEQ_ITEM_UNDEF_END) || tagname.equals("FFFE,EEEE")) // reserved
            {
                elementLength = getInt(bigEndian);
            } else {
                read(byteBuffer4); // Reads the explicit VR and following two bytes.
                elementLength = getLength(bigEndian, byteBuffer4[0], byteBuffer4[1], byteBuffer4[2], byteBuffer4[3]);
                // Preferences.debug(" length " + Integer.toString(elementLength, 0x10) + "\n");
            }
        } else { // this is what is standardly used.

            // either IMPLICIT or group element is not SEQ_ITEM_BEGIN
            read(byteBuffer4);
            elementLength = getLength(bigEndian, byteBuffer4[0], byteBuffer4[1], byteBuffer4[2], byteBuffer4[3]);
        }
    }

    /**
     * Gets the sequence in a sequence tag. Sequences of items have special encodings that are detailed in the DICOM
     * standard. There is usually an "item" tag, then a dataset encoded exactly like other tags, then a tag indicating
     * the end of the sequence.
     * 
     * <P>
     * For further information see the DICOM Standard, Part 5, Section 7.
     * </P>
     * 
     * @param endianess Big or little
     * @param seqLength Length of this sequence, although possibly left undefined.
     * 
     * @return A DicomSQ object which stores the new tags and their info
     * 
     * @see DicomSQ
     */
    private Object getSequence(final boolean endianess, final int seqLength) throws IOException {
        final FileDicomSQ sq = new FileDicomSQ();

        // There is no more of the tag to read if the length of the tag
        // is zero. In fact, trying to get the Next element is potentially
        // bad, so we'll just shut down the reading here.
        if (seqLength == 0) {
            return sq;
        }

        // hold on to where the sequence is before items for measuring
        // distance from beginning of sequence
        final int seqStart = getFilePointer();
        inSQ = true;

        getNextElement(endianess); // gets the first ITEM tag
        Preferences.debug("Item: " + Integer.toString(groupWord, 0x10) + "," + Integer.toString(elementWord, 0x10)
                + " for " + Integer.toString(elementLength, 0x10) + " # readfrom: "
                + Long.toString(getFilePointer(), 0x10) + "\n");

        inSQ = false;
        nameSQ = convertGroupElement(groupWord, elementWord);

        // Preferences.debug("getSquence: nameSQ = " + nameSQ +
        // " fptr = " + Long.toString(getFilePointer(), 0x10) + "\n");
        try {

            if ( (seqLength == UNDEFINED_LENGTH) || (seqLength == ILLEGAL_LENGTH)) {

                while ( !nameSQ.equals(FileDicom.SEQ_ITEM_UNDEF_END)) {

                    if (nameSQ.equals(FileDicom.SEQ_ITEM_BEGIN)) {

                        // elementLength here is the length of the
                        // item as it written into the File
                        if (elementLength == 0) {
                            final FileDicomItem item = new FileDicomItem();
                            sq.addItem(item);
                        } else {
                            sq.addItem((FileDicomItem) getDataSet(elementLength, endianess));
                        }
                    } else if (nameSQ.equals(FileDicom.SEQ_ITEM_END)) {

                        // possibility of getting here when subsequence tag length == -1
                        // end of sub-sequence tag
                        Preferences.debug("End of sub-sequence " + FileDicom.SEQ_ITEM_END + " found; nothing done.\n");
                    } else { // should never get here
                        Preferences.debug("getSequence(): sub-sequence tags not starting with "
                                + FileDicom.SEQ_ITEM_BEGIN + "\n", 2);
                    }

                    inSQ = true; // don't add the element length to the location
                    getNextElement(endianess); // skipping the tag-length???
                    nameSQ = convertGroupElement(groupWord, elementWord);
                    inSQ = false; // may now add element length to location

                    // Preferences.debug("Next item of seq. "+
                    // "nameSQ: " + nameSQ +
                    // " fpr: " + Long.toString(getFilePointer(), 0x10) + " \n");
                }
            } else { // sequence length is explicitly defined:

                // MUST be "<", rather than "<=" because when
                // fptr - seqStart == seqLength we want to move along to
                // other tags; it indicates that we are done with this
                // sequence.

                while ( (getFilePointer() - seqStart) < seqLength) {

                    // loop is meant to read out each sub-sequence tag from the sequence.
                    // Must make it able to read out the
                    Preferences.debug(nameSQ + "; len:" + Long.toString(getFilePointer() - seqStart, 0x10) + "\n");

                    if (nameSQ.equals(FileDicom.SEQ_ITEM_BEGIN)) { // this should always be true

                        // int offset = (int)raFile.getFilePointer(); // where the data set begins for this item
                        // elementLength here is the length of the
                        // item as it written into the File
                        // System.out.println("Special ele length = " + Integer.toString(elementLength, 0x10));
                        sq.addItem((FileDicomItem) getDataSet(elementLength, endianess));
                    } else { // should never get here
                        Preferences.debug("getSequence(): sub-sequence tags not starting with "
                                + FileDicom.SEQ_ITEM_BEGIN + "\n", 2);
                        // System.err.println("getSequence(): sub-sequence tags not starting with FFFE,E000");
                    }

                    if ( (getFilePointer() - seqStart) < seqLength) { // '<=' or just '<'??
                        inSQ = true; // don't add the element length to the location
                        Preferences.debug("[FileDicom.getSequence():]" + Integer.toString(groupWord, 0x10) + "-"
                                + Integer.toString(elementWord, 0x10) + " -- ");
                        getNextElement(endianess); // skipping the tag-length???
                        Preferences.debug("  the length: " + Integer.toString(elementLength, 0x10) + "\n");
                        nameSQ = convertGroupElement(groupWord, elementWord);
                        Preferences.debug("after converting group-element: " + nameSQ + "!!!!\n");
                        inSQ = false; // may now add element length to location

                        // System.err.println("pulled out next item of seq. nameSQ: " +nameSQ);
                    }
                }
            }
        } catch (final Exception error) {
            error.printStackTrace();
            Preferences.debug("Exception caught; Problem in FileDicom.getSequence\n", 2);
            Preferences.debug(error.toString() + "\n");
        }

        return sq;
    }

    private void setPerFrameEnhancedSequenceTags(final boolean endianess) {
        try {
            getNextElement(endianess); // gets the first ITEM tag
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Reads a length of the data and deposits it into a single Short or an array of Short as needed by the tag's VM.
     * 
     * @return Object
     * 
     * @throws IOException DOCUMENT ME!
     * 
     * @param vm value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param length number of bytes to read out of data stream; the length is not used.
     * @param endianess byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
     *            indicates little-endian.
     */
    private Object getShort(final int vm, final int length, final boolean endianess) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null; // the Object we read in

        if (vm > 1) {
            final Short[] array = new Short[length / 2];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Short((short) getUnsignedShort(endianess));
                len -= 2;
                i++;
            }

            readObject = array;
        } else if ( ( (vm < 1) && (length > 2))) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            final Short[] array = new Short[length / 2];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Short((short) getUnsignedShort(endianess));
                len -= 2;
                i++;
            }

            readObject = array;
        } else if ( ( (vm == 1) && (length > 2))) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            readObject = new Short((short) getUnsignedShort(endianess));
            len -= 2;

            while (len > 0) { // we should validate with VM here too
                getUnsignedShort(endianess);
                len -= 2;
                i++;
            }
        } else if (length == 2) {
            readObject = new Short((short) getUnsignedShort(endianess));
        }

        return readObject;
    }

    /**
     * Gets private tags or other tags where the type is unknown; does not change the data, so it may be written out
     * correctly.
     * 
     * @return A Byte array of length elementLength with the data stored in it.
     * 
     * @throws IOException DOCUMENT ME!
     */
    private Object readUnknownData() throws IOException {
        byte[] bytesValue;
        Byte[] bytesV;
        Preferences.debug("Unknown data; length is " + elementLength + " fp = " + getFilePointer() + "\n", 2);

        if (elementLength <= 0) {
            Preferences.debug("Unknown data; Error length is " + elementLength + "!!!!!\n", 2);

            return null;
        }

        bytesValue = new byte[elementLength];
        read(bytesValue);
        bytesV = new Byte[elementLength];

        for (int k = 0; k < bytesValue.length; k++) {
            bytesV[k] = new Byte(bytesValue[k]);
        }

        return bytesV;
    }

    /**
     * Writing a file to disk likely indicates that it was changed in some way. Notify any users opening this file later
     * that it is different from the original machine which created the image and what changed it.
     * 
     * @param fileInfo File info structure to set.
     */
    private void stampSecondaryCapture(final FileInfoDicom fileInfo) {

        // be nice: let anyone viewing this DICOM file know this image became a DICOM sometime after capture
        fileInfo.getTagTable().setValue("0008,0064", "WSD", 3); // Conversion Type: Workstation

        final Date currentDate = new Date();

        // mess with tags outside of the fileInfo structure (fileInfo doens't handle dicom formatting), and we want the
        // tags to figure things out (0018,1012): Date of Secondary Capture
        final java.text.SimpleDateFormat formatter = new java.text.SimpleDateFormat("MM'/'dd'/'yyyy");
        String temp = formatter.format(currentDate);
        fileInfo.getTagTable().setValue("0018,1012", temp);

        // (0018,1014): Time of Secondary Capture
        formatter.applyLocalizedPattern("HH:mm:ss");
        temp = formatter.format(currentDate);
        fileInfo.getTagTable().setValue("0018,1014", temp);

        // (0018,1016): Secondary Capture Device manufacturer
        temp = "National Institutes of Health, Center for Information Technology";
        fileInfo.getTagTable().setValue("0018,1016", temp);

        // (0018,1018): Secondary Capture Device Manufacturer's Model Name
        temp = "Medical Image Processing, Analysis and Visualization (MIPAV)";
        fileInfo.getTagTable().setValue("0018,1018", temp);

        // (0018,1019): Secondary Capture Device Software Version(s)
        fileInfo.getTagTable().setValue("0018,1019", MipavUtil.getVersion());
    }

    /**
     * Writes the tags of the DICOM header.
     * 
     * @param outputFile Output file to write to.
     * @param fileInfo File info structure for this file.
     * 
     * @exception IOException if there is an error writing the file.
     * 
     * @see FileInfoDicom
     */
    private void writeHeader(final RandomAccessFile outputFile, final FileInfoDicom fileInfo,
            final boolean saveAsEncapJP2) throws IOException {

        // all DICOM files start out as little endian
        boolean endianess = FileBase.LITTLE_ENDIAN;
        FileDicomTag[] dicomTags = FileDicomTagTable.sortTagsList(fileInfo.getTagTable().getTagList());
        int metaGroupLength = 0;

        if (fileInfo.containsDICM) {

            // write "DICM" tag
            final byte[] skip = new byte[ID_OFFSET];

            for (int k = 0; k < skip.length; k++) {
                skip[k] = (byte) 0;
            }

            outputFile.write(skip);
            outputFile.writeBytes("DICM");
        }

        for (final FileDicomTag element : dicomTags) {

            String type = "";
            final String vr = element.getValueRepresentation();

            // System.out.println("w = " + dicomTags[i].toString());
            try {

                if (fileInfo.vr_type == FileInfoDicom.EXPLICIT) {

                    // explicit VRs may be difference from implicit; in this case use the explicit
                    type = FileDicomTagInfo.getType(vr);
                } else {
                    type = element.getType();
                }
            } catch (final NullPointerException e) {
                type = FileDicomBase.TYPE_UNKNOWN;
            }

            // System.out.println(" Name = " + dicomTags[i].toString());
            final int gr = element.getGroup();
            final int el = element.getElement();

            // In the future, getLength should calculate the length instead of retrieveing a stored version
            // I think Dave already solved part of this problem with getNumberOfValues (just multiply by size_of_type.
            final int length = element.getLength();

            final int nValues = element.getNumberOfValues();

            final int vm = element.getValueMultiplicity();
            // Not sure if I need this - efilm adds it - also efilm has tag, 0000 lengths (meta lengths)for tag groups
            // (02, 08, ....) if (gr==2 && fileInfo.getVRType() == fileInfo.IMPLICIT){
            // fileInfo.setVRType(fileInfo.EXPLICIT); } else if (gr > 2){ fileInfo.setVRType(fileInfo.IMPLICIT); }

            if ( (gr == 2) && (el == 0)) {

                // length of the transfer syntax group; after this group is written, need to change endianess
                metaGroupLength = ((Integer) element.getValue(false)).intValue();
            }

            if (fileInfo.containsDICM) {

                // after transfer syntax group is read in
                if (raFile.getFilePointer() >= (ID_OFFSET + metaGroupLength)) {

                    // set endianess to whatever the transfer group defined it as
                    endianess = fileInfo.getEndianess();
                }
            } else {

                if (raFile.getFilePointer() >= metaGroupLength) {
                    endianess = fileInfo.getEndianess();
                }
            }

            writeShort((short) gr, endianess); // write group
            writeShort((short) el, endianess); // write element

            if ( ( (fileInfo.vr_type == FileInfoDicom.EXPLICIT) || (gr == 2)) && (vr != null)) {
                outputFile.writeBytes(vr); // write explicit vr

                if (vr.equals("SQ")) {

                    // explicit VR 32 bit length
                    outputFile.writeShort(0); // skip two reserved bytes
                    System.out.println( ((FileDicomSQ) element.getValue(false)).getLength());
                    if ( ((FileDicomSQ) element.getValue(false)).getLength() == 0) {
                        writeInt(0, endianess); // write length of 0.
                    } else {
                        writeInt(0xFFFFFFFF, endianess); // write undefined length
                    }
                } else if (vr.equals("OB") || vr.equals("OW") || vr.equals("UN")) {

                    // explicit vr 32 bit length
                    outputFile.writeShort(0); // skip two reserved bytes
                    writeInt(length, endianess);
                }
                // explicit vr 16 bit length
                else {
                    writeShort((short) length, endianess); // write length as a short
                }
            } else { // IMPLICIT VR

                try {

                    if (vr.equals("SQ")) {

                        if ( ((FileDicomSQ) element.getValue(false)).getLength() == 0) {
                            writeInt(0, endianess); // write length of 0.
                        } else {
                            writeInt(0xFFFFFFFF, endianess); // write undefined length
                        }
                    } else {
                        writeInt(length, endianess); // implicit vr, 32 bit length
                    }
                } catch (final NullPointerException noVRtypeException) {

                    // System.err.println("Found a tag ("+
                    // Integer.toString(gr, 0x10)+","+Integer.toString(el, 0x10) +
                    // ") without a TAG VR! whoops.");
                    writeInt(length, endianess); // implicit vr, 32 bit length
                }
            }
            // Preferences.debug( "this is "+dicomTags[i] + "\n", 2);

            // write as a string if string, unknown (private), or vm > 1
            // The VM part is consistent with how we're reading it in; hopefully
            // once we have test images we'll have a better way of reading & writing
            if ( (length == 0) && !type.equals(FileDicomBase.TYPE_SEQUENCE)) {
                // NOP to prevent other types from getting values being
                // written when no-length, non-sequences.
            } else if (type.equals(FileDicomBase.TYPE_UNKNOWN) || type.equals(FileDicomBase.OTHER_BYTE_STRING)) {

                // Unknowns are stored as an array of Bytes. VM does not apply ?
                Byte[] bytesV = null;
                bytesV = (Byte[]) element.getValue(false);

                final byte[] bytesValue = new byte[bytesV.length];

                for (int k = 0; k < bytesV.length; k++) {
                    bytesValue[k] = bytesV[k].byteValue();
                    // System.err.print(" [" + bytesV[k].toString()+"]");
                }

                outputFile.write(bytesValue);
                // System.err.println();
            } else if (type.equals(FileDicomBase.OTHER_WORD_STRING)) { // OW -- word = 2 bytes

                final Object[] data = (Object[]) element.getValue(false);

                // We are not sure that that LUT endianess is always BIG
                // but the example images we have are.
                // Book 3 C.7.6.3.1.6 says to swap byte of OW.
                if ( ( (gr == 0x28) && (el == 0x1201)) || ( (gr == 0x28) && (el == 0x1202))
                        || ( (gr == 0x28) && (el == 0x1203))) {

                    // data is guaranteed to be short for these tags
                    for (final Short element2 : (Short[]) data) {
                        writeShort(element2.shortValue(), true);
                    }
                } else {

                    if (data instanceof Short[]) {
                        for (final Short element2 : (Short[]) data) {
                            writeShort(element2.shortValue(), endianess);
                        }
                    } else if (data instanceof Byte[]) {
                        for (final Byte element2 : (Byte[]) data) {
                            writeByte(element2.byteValue());
                        }
                    }

                }

            } else if (type.equals(FileDicomBase.TYPE_STRING)) {

                // VM - I don't think applies
                outputFile.writeBytes(element.getValue(false).toString());

                if ( (element.getValue(false).toString().length() % 2) != 0) {
                    outputFile.writeBytes("\0");
                }
            } else if (type.equals(FileDicomBase.TYPE_FLOAT)) {

                if ( (vm > 1) || (nValues > 1)) {
                    final Float[] data = (Float[]) element.getValue(false);

                    for (int vmI = 0; vmI < nValues; vmI++) {
                        writeFloat(data[vmI].floatValue(), endianess);
                    }
                } else {
                    writeFloat( ((Float) element.getValue(false)).floatValue(), endianess);
                }
            } else if (type.equals(FileDicomBase.TYPE_DOUBLE)) {

                if ( (vm > 1) || (nValues > 1)) {
                    final Double[] data = (Double[]) element.getValue(false);

                    for (int vmI = 0; vmI < nValues; vmI++) {
                        writeDouble(data[vmI].doubleValue(), endianess);
                    }
                } else {
                    writeDouble( ((Double) element.getValue(false)).doubleValue(), endianess);
                }
            } else if (type.equals(FileDicomBase.TYPE_SHORT)) {

                if ( (vm > 1) || (nValues > 1)) {
                    final Short[] data = (Short[]) element.getValue(false);

                    for (int vmI = 0; vmI < nValues; vmI++) {
                        writeShort(data[vmI].shortValue(), endianess);
                    }
                } else {
                    writeShort( ((Short) element.getValue(false)).shortValue(), endianess);
                }
            } else if (type.equals(FileDicomBase.TYPE_INT)) {

                if ( (vm > 1) || (nValues > 1)) {
                    final Integer[] data = (Integer[]) element.getValue(false);

                    for (int vmI = 0; vmI < nValues; vmI++) {
                        writeInt(data[vmI].intValue(), endianess);
                    }
                } else {
                    writeInt( ((Integer) element.getValue(false)).intValue(), endianess);
                }
            } else if (type.equals(FileDicomBase.TYPE_SEQUENCE)) {

                // VM - I don't think applies
                final FileDicomSQ sq = (FileDicomSQ) element.getValue(false);
                writeSequence(outputFile, fileInfo.vr_type, sq, endianess);
            }
        }

        writeShort((short) 0x7FE0, endianess); // the image
        writeShort((short) 0x10, endianess);

        if (saveAsEncapJP2) {
            writeShort((short) 0x424F, endianess);
            // writeShort((short) 0x574F, endianess);
            writeShort((short) 0x0000, endianess);
            writeInt(0xFFFFFFFF, endianess);
            if (dicomTags != null) {
                for (int i = 0; i < dicomTags.length; i++) {
                    if (dicomTags[i] != null) {
                        dicomTags[i] = null;
                    }
                }
                dicomTags = null;
            }
            return;
        }

        if (fileInfo.vr_type == FileInfoDicom.EXPLICIT) {
            String imageTagVR;

            if (fileInfo.getTagTable().get(FileDicom.IMAGE_TAG) != null) {
                imageTagVR = fileInfo.getTagTable().get(FileDicom.IMAGE_TAG).getValueRepresentation();
            } else {
                imageTagVR = DicomDictionary.getVR(new FileDicomKey(FileDicom.IMAGE_TAG));
            }

            // write VR and two reserved bytes
            outputFile.writeBytes(imageTagVR);
            outputFile.writeShort(0);
        }

        int samplesPerPixel = 1;

        if (fileInfo.getTagTable().getValue("0028,0002") != null) {
            samplesPerPixel = ((Short) fileInfo.getTagTable().getValue("0028,0002")).shortValue();
        }

        final int imageLength = image.getSliceSize()
                * ((Short) fileInfo.getTagTable().getValue("0028,0100")).shortValue() / 8 * // bits per pixel
                samplesPerPixel; // samples per pixel (i.e RGB = 3)

        if (fileInfo.isMultiFrame()) {
            int nImages = 1;

            if (fileInfo.getTagTable().getValue("0028,0008") != null) {
                nImages = Integer.valueOf( ((String) (fileInfo.getTagTable().getValue("0028,0008"))).trim()).intValue();
            }

            writeInt(imageLength * nImages, endianess);
        } else {
            writeInt(imageLength, endianess);
        }

        if (dicomTags != null) {
            for (int i = 0; i < dicomTags.length; i++) {
                if (dicomTags[i] != null) {
                    dicomTags[i] = null;
                }
            }
            dicomTags = null;
        }
    }

    /**
     * Writes out a sequence tag and its data. The routine writes the Sequence as undefined-length and each of the items
     * as undefined length. The appropriate write-methods are used to output the various tags to the file.
     * 
     * @param outputFile File to write to.
     * @param vr_type VR type, explicit or implicit
     * @param sq Sequence to write out
     * @param endianess Big is <code>true</code> or <code>false</code> for little endian byte-order.
     * 
     * @throws IOException if write fails
     */
    private void writeSequence(final RandomAccessFile outputFile, final boolean vr_type, final FileDicomSQ sq,
            final boolean endianess) throws IOException {
        FileDicomItem item;

        if ( (sq == null) || (sq.getSequenceLength() < 1)) {
            return;
        }

        for (int i = 0; i < sq.getSequenceLength(); i++) {
            item = sq.getItem(i);

            if (item.getNumberOfElements() > 0) {

                // write item-start tag
                writeShort((short) 0xFFFE, endianess);
                writeShort((short) 0xE000, endianess);
                writeInt(0xFFFFFFFF, endianess); // data-length (we'll get it to spit out real length later!)
            }

            final Iterator<FileDicomTag> dataSetItr = item.getDataSet().values().iterator();

            while (dataSetItr.hasNext()) {

                // System.err.println(" Counter = " + j);
                String type = "";
                final FileDicomTag entry = dataSetItr.next();

                final String vr = entry.getValueRepresentation();
                final int length = entry.getLength();

                // System.out.println("adadadfad length = " + length + " group = " + entry.getGroup() + " element = " +
                // entry.getElement());
                try {

                    if ( (vr_type == FileInfoDicom.EXPLICIT) && (vr != null)) {
                        type = FileDicomTagInfo.getType(vr);
                    } else {
                        type = entry.getType();
                    }
                } catch (final NullPointerException error) {
                    type = FileDicomBase.TYPE_UNKNOWN;
                }

                writeShort((short) entry.getGroup(), endianess);
                writeShort((short) entry.getElement(), endianess);

                if ( (vr_type == FileInfoDicom.EXPLICIT) && (vr != null)) {
                    outputFile.writeBytes(vr);

                    if (vr.equals("SQ") || vr.equals("OB") || vr.equals("OW") || vr.equals("UN")) {

                        // if (vr.equals("SQ") || vr.equals("UN")) {
                        outputFile.writeShort(0);
                    } else {
                        writeShort((short) length, endianess);
                    }
                }

                if (length == UNDEFINED_LENGTH) {
                    writeShort((short) 0xFFFF, endianess);
                } else if ( ( (vr_type == FileInfoDicom.EXPLICIT) && (vr != null) && (vr.equals("SQ")
                        || vr.equals("OB") || vr.equals("OW") || vr.equals("UN")))) {

                    if ( (length == 0) && vr.equals("SQ")) {
                        // Do nothing because we only write ____Dave need help___ sequence tags
                    } else {
                        writeInt(length, endianess);
                    }
                } else if ( (vr_type == FileInfoDicom.IMPLICIT) && !type.equals(FileDicomBase.TYPE_UNKNOWN)
                        && !type.equals(FileDicomBase.TYPE_SEQUENCE)) {
                    writeInt(length, endianess);
                }

                if (type.equals(FileDicomBase.TYPE_STRING) || type.equals(FileDicomBase.OTHER_WORD_STRING)) {
                    outputFile.writeBytes(entry.getValue(false).toString());

                    if ( (entry.getValue(false).toString().length() % 2) != 0) {
                        outputFile.writeBytes("\0");
                    }
                } else if (type.equals(FileDicomBase.TYPE_UNKNOWN)) {

                    // Unknowns are stored as an array of Bytes.
                    // VM does not apply?
                    Byte[] bytesV = null;
                    bytesV = (Byte[]) entry.getValue(false);

                    final byte[] bytesValue = new byte[bytesV.length];

                    for (int k = 0; k < bytesV.length; k++) {
                        bytesValue[k] = bytesV[k].byteValue();
                        // System.err.print(" [" + bytesV[k].toString()+"]");
                    }

                    writeInt(bytesV.length, endianess);
                    outputFile.write(bytesValue);
                } else if (type.equals(FileDicomBase.OTHER_BYTE_STRING)) {
                    Byte[] bytesV = null;
                    bytesV = (Byte[]) entry.getValue(false);

                    final byte[] bytesValue = new byte[bytesV.length];

                    for (int k = 0; k < bytesV.length; k++) {
                        bytesValue[k] = bytesV[k].byteValue();
                        // System.err.print(" [" + bytesV[k].toString()+"]");
                    }

                    // writeInt(bytesV.length, endianess);
                    outputFile.write(bytesValue);
                } else if (type.equals(FileDicomBase.TYPE_FLOAT)) {
                    writeFloat( ((Float) entry.getValue(false)).floatValue(), endianess);
                } else if (type.equals(FileDicomBase.TYPE_DOUBLE)) {
                    if (entry.getValue(false) instanceof Double[]) {
                        final Double[] dArr = (Double[]) entry.getValue(false);
                        for (final Double element : dArr) {
                            writeDouble(element, endianess);
                        }
                    } else {
                        writeDouble( ((Double) entry.getValue(false)).doubleValue(), endianess);
                    }
                } else if (type.equals(FileDicomBase.TYPE_SHORT)) {
                    writeShort( ((Short) entry.getValue(false)).shortValue(), endianess);
                } else if (type.equals(FileDicomBase.TYPE_INT)) {
                    writeInt( ((Integer) entry.getValue(false)).intValue(), endianess);
                } else if (type.equals(FileDicomBase.TYPE_SEQUENCE)) {
                    final FileDicomSQ sq2 = (FileDicomSQ) entry.getValue(false);
                    writeInt(0xFFFFFFFF, endianess);
                    writeSequence(outputFile, vr_type, sq2, endianess);
                }
            }

            // write end-item tag:
            writeShort((short) 0xFFFE, endianess);
            writeShort((short) 0xE00D, endianess);
            writeInt(0, endianess);
        }

        writeShort((short) 0xFFFE, endianess);
        writeShort((short) 0xE0DD, endianess);
        writeInt(0, endianess);
        // System.err.println("3333pointer = " + outputFile.getFilePointer());
    }
}
