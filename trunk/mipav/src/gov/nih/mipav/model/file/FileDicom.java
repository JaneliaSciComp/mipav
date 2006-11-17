package gov.nih.mipav.model.file;


import gov.nih.mipav.*;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.image.*;

import java.io.*;

import java.util.*;

import javax.imageio.*;


/**
 * This class reads and writes DICOM files. The DICOM file format consists of header information marked by tags, with
 * the pixel data as the last tag. Each tag has a length field that contains the length of the tag, so it is possible to
 * skip over tags that are unrecognized. This class is entirely based on DICOM version 3.0 published by ACR/NEMA and so
 * tests for all the tags in the Data Dictionary (DICOM v. 3.0 Part 6). It stores these in a Hashtable found in
 * FileInfoDicom.
 *
 * <p>The Hashtable is based upon the default DICOM dictionary which contains all possible standard DICOM tags. It also
 * contains many private tags that are commented out. If the user wishes a specific private tag to be recognized by this
 * program, he or she should edit the dictionary file. The tag will then be displayed as any other standard tag would be
 * displayed. Otherwise, all tags are read in, but if their value representation is unrecognized (only the case with
 * tags not defined in dictionary file) their value is stored as a string. When FileInfoDicom displays the tag
 * information, it shows the name of the tag as private and the value as the string. The string may contain valid data
 * or it may contain junk. There is no way of knowing how to properly read in a private tag without a valid value
 * representation (VR). So if the user wishes to know private tag information, he or she should specify the proper VR in
 * the dictionary file and be sure that their file conforms to that VR.</p>
 *
 * @version  1.0 Aug 1, 1999
 * @see      FileIO
 * @see      FileInfoDicom
 * @see      FileRaw
 * @see      FileRawChunk
 */
public class FileDicom extends FileDicomBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * used in reading; if the preferences are at this level or higher than dicomDebugOutputValue get sent to the debug
     * panel. This is used strictly in readHeader, as the comparison statements are there for speed enhancement (as
     * opposed to relying on the settings in Preferences.debug(String, int)). Only used for informational statements and
     * not used in failure statements, where the original values are left as they were (around level 3).
     */
    private static final int dicomDebugOutputValue = 5;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Length of the value field of data element. */
    private int elementLength;

    /**
     * when dicom image data is 'encapsulated,' it may be in pieces, or 'fragments.' don't know quite why, or if pieces
     * should be kept together. If in fragments, the image data may span several slices, called a 'frame.'
     */
    private boolean encapsulated = false;

    /** Directory of the image file. */
    private String fileDir;

    /** File object of the image. */
    private File fileHeader;


    /** Meta data structure in which to save all the DICOM tags. */
    private FileInfoDicom fileInfo;


    /** Name of the file to be read in. */
    private String fileName;

    /** location of first element. */
    private final int FIRST_ELEMENT = 132; //

    /** First number (DICOM group) in ordered pair of numbers that uniquely identifies a data element. */
    private int groupWord;
    
    /** Second number (DICOM element in a group) in ordered pair of numbers that uniquely 
     * identifies a data element. */
    private int elementWord;

    /** True if the DICOM image header has been read. */
    private boolean hasHeaderBeenRead = false;

    /** location of "DICM". */
    private final int ID_OFFSET = 128;

    /** illegal element length. */
    private final int ILLEGAL_LENGTH = -1;

    /** Reference to the image read into the application. */
    private ModelImage image;

    /** True if in a sequence tag. */
    private boolean inSQ = false;

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
     * <p>See DICOM3 part 10 pages 12-14 (1988).</p>
     */
    private int metaGroupLength = 0;

    /** Name of the sequance tag. */
    private String nameSQ = "";

    /**
     * if the file is <i>quiet</i> no user-interaction is performed. Useful for determining whether or not to display
     * the MipavUtil.displayError() is to be called. This allows the option of leaving the user-interaction and
     * notification of an error to occur here or to be handled somewhere else (as in, when an IOException is thrown but
     * we'd prefer to notify the user once, rather than for each exception.) based on programming preferences or
     * user-debug preference settings.
     */
    private boolean quiet = false;


    /** DOCUMENT ME! */
    private FileRaw rawFile;

    /** undefined element length. */
    private final int UNDEFINED_LENGTH = -2;

    /** Value Representation - see FileInfoDicom. */
    private byte[] vr = new byte[2];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * DICOM reader/writer constructor. Creates the access files and ensures that the files are opened for read-write
     * access; it will fall back to read-only if read-write access to the image file is not granted. The file info gets
     * the DICOM dictionary set, as well as the endianess property is set. The image itself is not read.
     *
     * @param      fDirPlusName  full file name (with directory)
     *
     * @exception  IOException  if there is an error constructing the files.
     */
    public FileDicom(String fDirPlusName) throws IOException {

        FileIO.getFileIndex("a");

        try {
            fileHeader = new File(fDirPlusName);

            if (fileHeader == null) {
                throw new FileNotFoundException();
            }

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (IOException ex) { }
            }

            try {
                raFile = new RandomAccessFile(fileHeader, "r");
            } catch (IOException e) {

                // raFile      = new RandomAccessFile(fileHeader, "r");
            }

            fileInfo = new FileInfoDicom(null, null, FileUtility.DICOM);
            fileInfo.resetDictionary();
            fileInfo.setEndianess(LITTLE_ENDIAN);

        } catch (NullPointerException npe) {
            npe.printStackTrace();
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in FileDicom constructor.");
            throw new IOException();
        }
    }


    /**
     * DICOM reader/writer constructor. Creates the access files and ensures that the files are opened for read-write
     * access; it will fall back to read-only if read-write access to the image file is not granted. The file info gets
     * the DICOM dictionary set, as well as the endianess property is set. The image itself is not read.
     *
     * @param      ifile  an image file to read.
     *
     * @exception  IOException  if there is an error constructing the files.
     */
    public FileDicom(File ifile) throws IOException {
        fileName = ifile.getName();
        fileDir = ifile.getParent() + File.separator;

        try {
            fileHeader = ifile;

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (IOException ex) { }
            }

            try {
                raFile = new RandomAccessFile(fileHeader, "rw");
            } catch (IOException e) {
                raFile = new RandomAccessFile(fileHeader, "r");
            }

            fileInfo = new FileInfoDicom(fileName, fileDir, FileUtility.DICOM);
            fileInfo.resetDictionary();
            fileInfo.setEndianess(LITTLE_ENDIAN);
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in FileDicom constructor.");
            throw new IOException();
        }
    }

    /**
     * DICOM reader/writer constructor. Creates the access files and ensures that the files are opened for read-write
     * access; it will fall back to read-only if read-write access to the image file is not granted. The file info gets
     * the DICOM dictionary set, as well as the endianess property is set. The image itself is not read.
     *
     * @param      fName  File name.
     * @param      fDir   File directory.
     *
     * @exception  IOException  if there is an error constructing the files.
     */
    public FileDicom(String fName, String fDir) throws IOException {
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
                } catch (IOException ex) { }
            }

            try {
                raFile = new RandomAccessFile(fileHeader, "rw");
            } catch (IOException e) {
                raFile = new RandomAccessFile(fileHeader, "r");
            }

            fileInfo = new FileInfoDicom(fileName, fileDir, FileUtility.DICOM);
            fileInfo.resetDictionary();
            fileInfo.setEndianess(LITTLE_ENDIAN);
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
        } catch (NullPointerException npe) {
            npe.printStackTrace();
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in FileDicom constructor.");
            throw new IOException();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes random access file associated with this object.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void close() throws IOException {

        // System.out.println("FileDICOM.close");
        this.finalize();
    }

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>. Exceptions which occur while this method runs (for instance,
     * the possibility of getting a <code>IOException</code> when closing the image file) is ignored quietly.
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
            } catch (IOException ex) { }

            rawFile.finalize();
        }

        if (raFile != null) {
            try {
                raFile.close();
            } catch (IOException ex) { }

            raFile = null;
        }

        rawFile = null;
        nameSQ = null;
        jpegData = null;


        try {
            super.finalize();
        } catch (Throwable er) { }
    }


    /**
     * Accessor that returns the file info.
     *
     * @return  Structure containing the file info.
     */
    public final FileInfoBase getFileInfo() {
        return fileInfo;
    }


    /**
     * returns the ModelLUT, if there, which might be specified by the <code>0028,1201</code>, <code>0028,1202</code>,
     * <code>0028,1203</code> tags.
     *
     * @return  DOCUMENT ME!
     */
    public final ModelLUT getLUT() {
        return lut;
    }


    /**
     * Looks for the DICM _tag_ in the File header. If present, the image is DICOM 3.0 format.
     *
     * @throws  IOException  Indicates error reading the file
     *
     * @return  boolean true if the DICM tag was found in the image header.
     */
    public boolean isDICOM() throws IOException {

        if (raFile == null) {
            return false;
        }

        if (raFile.length() <= ID_OFFSET) {
            return false;
        }

        long fPtr = raFile.getFilePointer();
        raFile.seek(ID_OFFSET); // Find "DICM" tag

        // In v. 3.0, within the ID_OFFSET is general header information that
        // is not encoded into data elements and not present in DICOM v. 2.0.
        // However, it is optional.

        if (!getStringFromFile(4).equals("DICM")) {
            fileInfo.containsDICM = false;
            raFile.seek(0); // set file pointer to zero
        } else {
            fileInfo.containsDICM = true;
            raFile.seek(fPtr); // set file pointer back to original position
        }

        return (fileInfo.containsDICM);
    }

    /**
     * gets the quiet option on the class.
     *
     * <p>The idea is that for all output messages, there should be the option to not bother the user with the message.
     * Ie., if an exception is thrown, and we normally tell the user that an error occurred, a calling class can set
     * this option so that the calling class can handle (either loudly or quietly, as needed) the error with its own
     * message. this could be upgraded to call with quiet to prevent user-queries from interrupting an automatic
     * process. But that is in the future.</p>
     *
     * <p>Note: In the future, this method and variable is to be moved to FileBase.</p>
     *
     * @return  whether this class should consider itself quiet, and by internal inspection, not notify the user. <code>
     *          True</code> is to consider itself to not notify the user. <code>False</code> is to notify the user, and
     *          is the default behaviour.
     */
    public final boolean isQuiet() {
        return quiet;
    }

    /**
     * Reads in all the tags available in the file and stores them in the Hashtable in FileInfoDicom. This method
     * handles the various tags that are present at the beginning of a DICOM image file. It also sets the important File
     * Info variables based on what it finds.
     *
     * <p>The method will return with a failure code if it finds the tag &quot;0000,0000&quot; or it mis-reads the
     * header and starts reading from an odd byte.</p>
     *
     * <p>As the reader runs through the tags in the header, it reads them based on the type. There are 7 types:</p>
     *
     * <ul>
     *   <li>typeString</li>
     *   <li>typeShort</li>
     *   <li>typeInt</li>
     *   <li>typeFloat</li>
     *   <li>typeDouble</li>
     *   <li>typeSequence</li>
     *   <li>typeUnknown</li>
     * </ul>
     *
     * <p>Any special handling based on type occurs for each tag (@see FileInfoDicom), then it is added to the DICOM
     * tags table. Each tag is checked against a small list of individual tags, as some tags have an effect on the way
     * the following tags are interpreted.</p>
     *
     * <p>This method also affects some of the properties of the FileInfoDicom.</p>
     *
     * <ul>
     *   <li>MetaGroupLength</li>
     *   <li>Units of Measure</li>
     *   <li>Transfer Syntax</li>
     *   <li>Extents</li>
     *   <li>Color Pallete for each color channel</li>
     * </ul>
     *
     * Display type changes the modality; image length is then also recalculated.
     *
     * @return     <code>true</code> if successful, otherwise <code>false</code>.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileInfoDicom
     * @see        #getNextElement(boolean)
     * @see        #convertGroupElement(int, int)
     */
    public boolean readHeader(boolean loadTagBuffer) throws IOException {
        int[] extents;
        String type, // type of data; there are 7, see FileInfoDicom
               name; // string representing the tag

        boolean endianess = FileBase.LITTLE_ENDIAN; // all DICOM files start as little endian
        boolean flag = true;

        if (loadTagBuffer == true) {
            loadTagBuffer();
        }

        String strValue = null;
        Object data = null;

        try {
            extents = new int[2];
        } catch (OutOfMemoryError e) {

            if (!isQuiet()) {
                MipavUtil.displayError("Out of memory in FileDicom.readHeader");
                Preferences.debug("Out of memory in FileDicom.readHeader\n");
            } else {
                Preferences.debug("Out of memory in FileDicom.readHeader");
            }

            throw new IOException("Out of Memory in FileDicom.readHeader");
        }

        elementLength = 0;
        fileInfo.setEndianess(endianess);

        skipBytes(ID_OFFSET); // Find "DICM" tag

        // In v. 3.0, within the ID_OFFSET is general header information that
        // is not encoded into data elements and not present in DICOM v. 2.0.
        // However, it is optional.

        if (!getString(4).equals("DICM")) {
            fileInfo.containsDICM = false;
            seek(0); // set file pointer to zero
        }

        fileInfo.setDataType(ModelStorageBase.SHORT); // Default file type

        boolean debug = Preferences.debugLevel(Preferences.DEBUG_FILEIO);

        while (flag == true) {

            if (fileInfo.containsDICM) {

                // endianess is defined in a tag and set here, after the transfer
                // syntax group has been read in
                if (getFilePointer() >= (ID_OFFSET + metaGroupLength)) {
                    endianess = fileInfo.getEndianess();
                }
            } else {

                if (getFilePointer() >= metaGroupLength) {
                    endianess = fileInfo.getEndianess();
                }
            }

            // *******  Gets  the next element
            getNextElement(endianess); // gets group, element, length
            name = convertGroupElement(groupWord, elementWord);
            // if (debug) {    Preferences.debug("group = " + groupWord + " element = " + elementWord + " length = " +
            // elementLength + "\n"); }

            if ((fileInfo.vr_type == FileInfoDicom.IMPLICIT) || (groupWord == 2)) {

                // implicit VR means VR is based on tag as defined in dictionary
                try {
                    type = ((FileDicomTag) fileInfo.getEntry(name)).getType();
                } catch (NullPointerException e) {
                    type = "typeUnknown";
                }
            } else { // Explicit VR
                type = FileDicomTag.getType(new String(vr));

                if ((FileDicomTag) fileInfo.getEntry(name) == null) {

                    // put private tags with explicit VRs in file info hashtable
                    fileInfo.putTag(name, new FileDicomTag(groupWord, elementWord, null));
                }
            }

            if ((fileInfo.vr_type == FileInfoDicom.EXPLICIT) && (groupWord != 2)) {
                ((FileDicomTag) fileInfo.getEntry(name)).setVR(new String(vr));
            }

            if ((elementWord == 0) && (elementLength == 0)) { // End of file

                if (!isQuiet()) {
                    MipavUtil.displayError("Error:  Unexpected end of file.  Unable to load image.");
                }

                throw new IOException("Error while reading header");
            }

            if ((getFilePointer() & 1) != 0) { // The file location pointer is at an odd byte number

                if (!isQuiet()) {
                    MipavUtil.displayError("Error:  Input file corrupted.  Unable to load image.");
                }

                // System.err.println("name: "+ name + " , len: " + Integer.toString(elementLength, 0x10));
                System.err.println("ERROR CAUSED BY READING IMAGE ON ODD BYTE");
                throw new IOException("Error while reading header");
            }

            try {

                if (type.equals("typeString")) {
                    strValue = getString(elementLength);
                    fileInfo.setValue(new FileDicomKey(name), strValue, elementLength);

                    if (debug) {
                        Preferences.debug(((FileDicomTag) (fileInfo.getEntry(name))).getName() + "\t\t(" + name +
                                          ");\t" + type + "; value = " + strValue + "; element length = " +
                                          elementLength + "\n");
                    }
                } else if (type.equals("otherByteString")) {
                    int tagVM = ((FileDicomTag) fileInfo.getEntry(name)).getVM();

                    if (!name.equals("7FE0,0010")) {
                        data = getByte(tagVM, elementLength, endianess);
                        fileInfo.setValue(new FileDicomKey(name), data, elementLength);

                        if (tagVM == 1) {
                            // if ( debug >= dicomDebugOutputValue)    Preferences.debug(
                            // ((FileDicomTag)(fileInfo.getEntry(name))).getName() +        "\t\t(" + name + ");\t
                            // (byte) value = " + ((Byte)(data)).byteValue() +        " element length = 2" + "\n", 2);
                        }
                    }
                } else if (type.equals("otherWordString") && !name.equals("0028,1201") && !name.equals("0028,1202") &&
                               !name.equals("0028,1203")) {
                    int tagVM = ((FileDicomTag) fileInfo.getEntry(name)).getVM();

                    if (!name.equals("7FE0,0010")) {
                        data = getByte(tagVM, elementLength, endianess);
                        fileInfo.setValue(new FileDicomKey(name), data, elementLength);

                        if (tagVM == 1) {
                            // if ( debug >= dicomDebugOutputValue)    Preferences.debug(
                            // ((FileDicomTag)(fileInfo.getEntry(name))).getName() +        "\t\t(" + name + ");\t
                            // (byte) value = " + ((Byte)(data)).byteValue() +        " element length = 2" + "\n", 2);
                        }
                    }
                } else if (type.equals("typeShort")) {
                    int tagVM = ((FileDicomTag) fileInfo.getEntry(name)).getVM();
                    data = getShort(tagVM, elementLength, endianess);
                    fileInfo.setValue(new FileDicomKey(name), data, elementLength);

                    if (tagVM == 1) {

                        if (debug) {
                            Preferences.debug(((FileDicomTag) (fileInfo.getEntry(name))).getName() + "\t\t(" + name +
                                              ");\t (short) value = " + ((Short) (data)).shortValue() +
                                              " element length = 2" + "\n");
                        }
                    }
                } else if (type.equals("typeInt")) {
                    int tagVM = ((FileDicomTag) fileInfo.getEntry(name)).getVM();
                    data = getInteger(tagVM, elementLength, endianess);
                    fileInfo.setValue(new FileDicomKey(name), data, elementLength);

                    if (tagVM == 1) {

                        if (debug) {
                            Preferences.debug(((FileDicomTag) (fileInfo.getEntry(name))).getName() + "\t\t(" + name +
                                              ");\t (int) value = " + ((Integer) (data)).intValue() +
                                              " element length = 4" + "\n");
                        }
                    }
                } else if (type.equals("typeFloat")) {
                    int tagVM = ((FileDicomTag) fileInfo.getEntry(name)).getVM();
                    data = getFloat(tagVM, elementLength, endianess);
                    fileInfo.setValue(new FileDicomKey(name), data, elementLength);

                    if (tagVM == 1) {

                        if (debug) {
                            Preferences.debug(((FileDicomTag) (fileInfo.getEntry(name))).getName() + "\t\t(" + name +
                                              ");\t (float) value = " + ((Float) (data)).floatValue() +
                                              " element length = 4" + "\n");
                        }
                    }
                } else if (type.equals("typeDouble")) {
                    int tagVM = ((FileDicomTag) fileInfo.getEntry(name)).getVM();
                    data = getDouble(tagVM, elementLength, endianess);
                    fileInfo.setValue(new FileDicomKey(name), data, elementLength);

                    if (tagVM == 1) {

                        if (debug) {
                            Preferences.debug(((FileDicomTag) (fileInfo.getEntry(name))).getName() + "\t\t(" + name +
                                              ");\t (double) value = " + ((Double) (data)).doubleValue() +
                                              " element length = 8" + "\n");
                        }
                    }
                }
                // (type == "typeUnknown" && elementLength == -1) Implicit sequence tag if not in DICOM dictionary.
                else if (type.equals("typeSequence") || ((type == "typeUnknown") && (elementLength == -1))) {
                    int gr = groupWord;
                    int el = elementWord;
                    int len = elementLength;

                    // save these values because they'll change as the sequence is read in below.
                    if (debug) {
                        Preferences.debug("Sequence Tags: (" + name + "); length = " + len + "\n");
                    }

                    Object sq = getSequence(endianess, len);
                    // System.err.print( "SEQUENCE DONE: Sequence Tags: (" + name + "); length = " +
                    // Integer.toString(len, 0x10) + "\n");

                    try {
                        fileInfo.setValue(new FileDicomKey(name), sq, elementLength);
                    } catch (NullPointerException e) {
                        System.err.println("Null pointer exception while " + "setting value.  Trying to put " +
                                           "new tag.");
                        fileInfo.putTag(name, new FileDicomTag(gr, el, sq));
                    }

                    // fileInfo.setLength(name, len);
                    if (debug) {
                        Preferences.debug("Finished sequence tags.\n\n");
                    }
                }
            } catch (OutOfMemoryError e) {

                if (!isQuiet()) {
                    MipavUtil.displayError("Out of memory in FileDicom.readHeader");
                    Preferences.debug("Out of memory in FileDicom.readHeader\n");
                } else {
                    Preferences.debug("Out of memory in FileDicom.readHeader\n");
                }

                throw new IOException();
            }

            if (name.equals("0002,0000")) { // length of the transfer syntax group

                if (data != null) {
                    metaGroupLength = ((Integer) (data)).intValue();
                }

                if (debug) {
                    Preferences.debug("metalength = " + metaGroupLength + " location " + getFilePointer() + "\n");
                }
            } else if (name.equals("0018,602C")) {
                fileInfo.setUnitsOfMeasure(FileInfoBase.CENTIMETERS, 0);
            } else if (name.equals("0018,602E")) {
                fileInfo.setUnitsOfMeasure(FileInfoBase.CENTIMETERS, 1);
            } else if (name.equals("0002,0010")) {

                // Transfer Syntax UID: DICOM part 10 page 13, part 5 p. 42-48, Part 6 p. 53
                // 1.2.840.10008.1.2        Implicit VR Little Endian (Default)
                // 1.2.840.10008.1.2.1      Explicit VR Little Endian
                // 1.2.840.10008.1.2.2      Explicit VR Big Endian
                // 1.2.840.10008.1.2.4.50   8-bit Lossy JPEG (JPEG Coding Process 1)
                // 1.2.840.10008.1.2.4.51   12-bit Lossy JPEG (JPEG Coding Process 4)
                // we should bounce out if we don't support this transfer syntax
                if (strValue.trim().equals("1.2.840.10008.1.2")) {

                    if (debug) {
                        Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue +
                                          " Implicit VR - Little Endian \n");
                    }

                    fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
                    fileInfo.vr_type = FileInfoDicom.IMPLICIT;
                    encapsulated = false;
                } else if (strValue.trim().equals("1.2.840.10008.1.2.1")) {

                    if (debug) {
                        Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue +
                                          " Explicit VR - Little Endian \n");
                    }

                    fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
                    fileInfo.vr_type = FileInfoDicom.EXPLICIT;
                    encapsulated = false;
                } else if (strValue.trim().equals("1.2.840.10008.1.2.2")) {

                    if (debug) {
                        Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue +
                                          " Explicit VR - Big Endian \n");
                    }

                    fileInfo.setEndianess(FileBase.BIG_ENDIAN);
                    fileInfo.vr_type = FileInfoDicom.EXPLICIT;
                    encapsulated = false;
                } else if (strValue.trim().startsWith("1.2.840.10008.1.2.4.")) { // JPEG

                    if (debug) {
                        Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue +
                                          " Implicit VR - Little Endian \n");
                    }

                    fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
                    fileInfo.vr_type = FileInfoDicom.EXPLICIT;
                    encapsulated = true;

                    if (strValue.trim().equals("1.2.840.10008.1.2.4.57") ||
                            strValue.trim().equals("1.2.840.10008.1.2.4.58") ||
                            strValue.trim().equals("1.2.840.10008.1.2.4.65") ||
                            strValue.trim().equals("1.2.840.10008.1.2.4.66") ||
                            strValue.trim().equals("1.2.840.10008.1.2.4.70")) {
                        lossy = false;
                    } else {
                        lossy = true;
                    }
                } else {

                    if (debug) {
                        Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue + " unknown!\n");
                    }

                    if (!isQuiet()) {
                        MipavUtil.displayError("MIPAV does not support transfer syntax:\n" + strValue);
                    }

                    flag = false; // break loop

                    return false; // couldn't read it!
                }
            } else if (name.equals("0028,0010")) { // Set the extents, used for reading the image
                extents[1] = ((Short) fileInfo.getValue(name)).intValue();
                // fileInfo.columns = extents[1];
            } else if (name.equals("0028,0011")) {
                extents[0] = ((Short) fileInfo.getValue(name)).intValue();
                // fileInfo.rows = extents[0];
            } else if (name.equals("0028,1201")) { // LUT for red-channel

                // red channel LUT
                try {
                    getColorPallete(new FileDicomKey(name));
                } catch (IllegalArgumentException iae) {
                    // System.err.println("Dude.  You screwed up somewhere!  "+
                    // " storeColorPallete throws iae when something other than"+
                    // " 0028,120[1|2|3] is used.");
                }
            } else if (name.equals("0028,1202")) { // LUT for blue-channel

                // blue channel LUT
                try {
                    getColorPallete(new FileDicomKey(name));
                } catch (IllegalArgumentException iae) {
                    // System.err.println("Dude.  You screwed up somewhere!  "+
                    // " storeColorPallete throws iae when something other than"+
                    // " 0028,120[1|2|3] is used.");
                }
            } else if (name.equals("0028,1203")) { // LUT for gree-channel

                // green channel LUT
                try {
                    getColorPallete(new FileDicomKey(name));
                } catch (IllegalArgumentException iae) {
                    // storeColorPallete throws iae when something other than
                    // 0028,120[1|2|3] is used.
                    // System.err.println("Dude.  You screwed up somewhere!  "+
                    // " storeColorPallete throws iae when something other than"+
                    // " 0028,120[1|2|3] is used.");
                }
            } else if (name.equals("7FE0,0010")) { // && elementLength!=0) { // The image.

                // This complicated code determines whether or not the offset is correct for the image.
                // For that to be true, (height * width * pixel spacing) + the present offset should equal
                // the length of the file.  If not, 4 might need to be added (I don't know why).  If not again,
                // the function returns false.

                int imageLength = extents[0] * extents[1] * fileInfo.bitsAllocated / 8;

                if (debug) {
                    Preferences.debug("File Dicom: readHeader - Data tag = " + "7FE0,0010\n");
                    Preferences.debug("File Dicom: readHeader - imageLength = " + imageLength + "\n");
                    Preferences.debug("File Dicom: readHeader - getFilePointer = " + getFilePointer() + "\n");
                }

                if (fileInfo.getModality() == fileInfo.POSITRON_EMISSION_TOMOGRAPHY) {
                    fileInfo.displayType = ModelStorageBase.FLOAT;
                } else {
                    fileInfo.displayType = fileInfo.getDataType();
                }


                if (!encapsulated) {
                    // System.err.println( "\n" +
                    // Long.toString(getFilePointer()) + "  " +
                    // Long.toString(raFile.length()) +
                    // " image length = " + imageLength );

                    long fileEnd;

                    if (loadTagBuffer == true) {
                        fileEnd = raFile.length();
                    } else {
                        fileEnd = fLength;
                    }

                    if ((imageLength + getFilePointer()) <= fileEnd) {
                        fileInfo.setOffset ((int) getFilePointer()); // Mark where the image is
                    }
                    // I think the extra 4 bytes is for explicit tags!!
                    // see Part 5 page 27 1998
                    else if ((imageLength + getFilePointer() + 4) <= fileEnd) {
                        fileInfo.setOffset((int) getFilePointer() + 4);
                    } else {

                        // Preferences.debug( "File Dicom: readHeader: xDim = " + extents[0] + " yDim = " + extents[1] +
                        //                      " Bits allocated = " + fileInfo.bitsAllocated, 2);
                        if (!isQuiet()) {
                            MipavUtil.displayError("Image not at expected offset.");
                        }

                        throw new IOException("Error while reading header");
                    }
                } else { // encapsulated
                    fileInfo.setOffset((int) (getFilePointer() - 12));
                }

                fileInfo.setExtents(extents);
                flag = false; // break loop
            } else if (type.equals("typeUnknown")) { // Private tag, may not be reading in correctly.

                try {

                    if (fileInfo.getEntry(name) != null) {
                        fileInfo.setValue(name, readUnknownData(), elementLength);
                    } else {
                        fileInfo.putTag(name, new FileDicomTag(groupWord, elementWord, readUnknownData()));
                        ((FileDicomTag) fileInfo.getEntry(name)).setLength(elementLength);

                        if (debug) {
                            Preferences.debug("Group = " + groupWord + " element = " + elementWord + " Type unknown" +
                                              "; value = " + strValue + "; element length = " + elementLength + "\n");
                        }
                    }
                } catch (OutOfMemoryError e) {

                    if (!isQuiet()) {
                        MipavUtil.displayError("Out of memory error while reading \"" + fileName +
                                               "\".\nThis may not be a DICOM image.");
                        Preferences.debug("Out of memory storing unknown tags in FileDicom.readHeader\n");
                    } else {
                        Preferences.debug("Out of memory storing unknown tags in FileDicom.readHeader\n");
                    }

                    throw new IOException("Out of memory storing unknown tags in FileDicom.readHeader");
                } catch (NullPointerException npe) {
                    System.err.println("name: " + name);
                    System.err.print("no hashtable? ");
                    System.err.println(fileInfo.getTagsList() == null);
                    throw npe;
                }
            }
        }
        // Done reading tags


        String photometricInterp = null;

        if (fileInfo.getValue("0028,0004") != null) {
            fileInfo.photometricInterp = ((String) (fileInfo.getValue("0028,0004"))).trim();
            photometricInterp = fileInfo.photometricInterp.trim();
        }

        if (photometricInterp == null) { // Default to MONOCROME2 and hope for the best
            photometricInterp = new String("MONOCHROME2");
        }

        if ((photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2")) &&
                (fileInfo.pixelRepresentation == 0) && (fileInfo.bitsAllocated == 8)) {
            fileInfo.setDataType(ModelStorageBase.UBYTE);
            fileInfo.displayType = ModelStorageBase.UBYTE;
            fileInfo.bytesPerPixel = 1;
        } else if ((photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2")) &&
                       (fileInfo.pixelRepresentation == 1) && (fileInfo.bitsAllocated == 8)) {
            fileInfo.setDataType(ModelStorageBase.BYTE);
            fileInfo.displayType = ModelStorageBase.BYTE;
            fileInfo.bytesPerPixel = 1;
        } else if ((photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2")) &&
                       (fileInfo.pixelRepresentation == 0) && (fileInfo.bitsAllocated > 8)) {

            if (fileInfo.getModality() == FileInfoBase.COMPUTED_TOMOGRAPHY) {

                // Found an CT that was incorrectly label pixelRepresentation = 0 (unsigned when
                // in fact all CT images are signed!
                fileInfo.setDataType(ModelStorageBase.SHORT);
                fileInfo.displayType = ModelStorageBase.SHORT;
                fileInfo.bytesPerPixel = 2;
            } else {
                fileInfo.setDataType(ModelStorageBase.USHORT);
                fileInfo.displayType = ModelStorageBase.USHORT;
                fileInfo.bytesPerPixel = 2;
            }

        } else if ((photometricInterp.equals("MONOCHROME1") || photometricInterp.equals("MONOCHROME2")) &&
                       (fileInfo.pixelRepresentation == 1) && (fileInfo.bitsAllocated > 8)) {
            fileInfo.setDataType(ModelStorageBase.SHORT);
            fileInfo.displayType = ModelStorageBase.SHORT;
            fileInfo.bytesPerPixel = 2;
        }
        // add something for RGB DICOM images  - search on this !!!!
        else if (photometricInterp.equals("RGB") && (fileInfo.bitsAllocated == 8)) {
            fileInfo.setDataType(ModelStorageBase.ARGB);
            fileInfo.displayType = ModelStorageBase.ARGB;
            fileInfo.bytesPerPixel = 3;

            if (fileInfo.getValue("0028,0006") != null) {
                fileInfo.planarConfig = ((Short) (fileInfo.getValue("0028,0006"))).shortValue();
            } else {
                fileInfo.planarConfig = 0; // rgb, rgb, rgb
            }
        } else if (photometricInterp.equals("YBR_FULL_422") && (fileInfo.bitsAllocated == 8) && encapsulated) {
            fileInfo.setDataType(ModelStorageBase.ARGB);
            fileInfo.displayType = ModelStorageBase.ARGB;
            fileInfo.bytesPerPixel = 3;

            if (fileInfo.getValue("0028,0006") != null) {
                fileInfo.planarConfig = ((Short) (fileInfo.getValue("0028,0006"))).shortValue();
            } else {
                fileInfo.planarConfig = 0; // rgb, rgb, rgb
            }
        } else if (photometricInterp.equals("PALETTE COLOR") && (fileInfo.pixelRepresentation == 0) &&
                       (fileInfo.bitsAllocated == 8)) {
            fileInfo.setDataType(ModelStorageBase.UBYTE);
            fileInfo.displayType = ModelStorageBase.UBYTE;
            fileInfo.bytesPerPixel = 1;

            int[] dimExtents = new int[2];
            dimExtents[0] = 4;
            dimExtents[1] = 256;
            lut = new ModelLUT(ModelLUT.GRAY, 256, dimExtents);

            for (int q = 0; q < dimExtents[1]; q++) {
                lut.set(0, q, 1); // the alpha channel is preloaded.
            }
            // sets the LUT to exist; we wait for the pallete tags to
            // describe what the lut should actually look like.
        } else {

            if (debug) {
                Preferences.debug("File DICOM: readImage() - Unsupported pixel Representation");
            }

            if (raFile != null) {
                raFile.close();
            }

            return false;
        }

        if (fileInfo.getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY) {
            fileInfo.displayType = ModelStorageBase.FLOAT;
            // a bit of a hack - indicates Model image should be reallocated to float for PET image the data is stored
            // as 2 bytes (short) but is "normalized" using the slope parameter required for PET images (intercept
            // always 0 for PET).
        }

        if (((fileInfo.getDataType() == ModelStorageBase.UBYTE) ||
                 (fileInfo.getDataType() == ModelStorageBase.USHORT)) && (fileInfo.getRescaleIntercept() < 0)) { // if image originally was unsigned and rescale makes a negative, change

            // display type so image shows up properly
            if (fileInfo.getDataType() == ModelStorageBase.UBYTE) {
                fileInfo.displayType = ModelStorageBase.BYTE;
                fileInfo.setDataType(ModelStorageBase.BYTE);
            } else if (fileInfo.getDataType() == ModelStorageBase.USHORT) {
                fileInfo.displayType = ModelStorageBase.SHORT;
                fileInfo.setDataType(ModelStorageBase.SHORT);
            }
        }


        if (fileInfo.getValue("0028,1201") != null) {

            // red channel LUT
            FileDicomTag tag = (FileDicomTag) fileInfo.getTagsList().get(new FileDicomKey("0028,1201"));
            Object[] values = tag.getValueList();
            int lutVals = values.length;

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

        if (fileInfo.getValue("0028,1202") != null) {

            // green channel LUT
            FileDicomTag tag = (FileDicomTag) fileInfo.getTagsList().get(new FileDicomKey("0028,1202"));
            Object[] values = tag.getValueList();
            int lutVals = values.length;

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


        if (fileInfo.getValue("0028,1203") != null) {

            // blue channel LUT
            FileDicomTag tag = (FileDicomTag) fileInfo.getTagsList().get(new FileDicomKey("0028,1203"));
            Object[] values = tag.getValueList();
            int lutVals = values.length;

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

        if ((loadTagBuffer == true) && (raFile != null)) {
            raFile.close();
        }

        hasHeaderBeenRead = true;

        return true;

    }

    /**
     * Reads a DICOM image file and stores the data into the given float buffer. This method reads the image header
     * (@see readHeader()) then sets various fields within the FileInfo which are relevant to correctly interpreting the
     * image. This list includes:
     *
     * <ul>
     *   <li>units of measure</li>
     *   <li>pixel pad</li>
     *   <li>file info minimum and maximum</li>
     * </ul>
     *
     * @param      buffer     2D buffer used for temporary storage of data
     * @param      imageType  The type of image (i.e. SHORT, BYTE, ...)
     * @param      imageNo    For multiFrame images, imageNo >=1. For single slice image imageNo = 0.
     *
     * @return     The image
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRaw
     */
    public void readImage(float[] buffer, int imageType, int imageNo) throws IOException {

        // Read in header info, if something goes wrong, print out error
        if (hasHeaderBeenRead == false) {

            if (!readHeader(true)) {
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

        if (!encapsulated) {

            try { // rafile (type RandomAccessFile) for header, rawfile (type FileRaw) for image data.
                rawFile.setImageFile(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

                if (ModelImage.isColorImage(imageType)) {
                    rawFile.setPlanarConfig(fileInfo.planarConfig);
                    rawFile.readImage(buffer, fileInfo.getOffset() + (imageNo * buffer.length), imageType); // ***** Read image
                    rawFile.raFile.close();
                    rawFile.raFile = null;
                } else {
                    rawFile.readImage(buffer, fileInfo.getOffset() + (imageNo * buffer.length * fileInfo.bytesPerPixel),
                                      imageType); // ***** Read image
                    rawFile.raFile.close();
                    rawFile.raFile = null;
                }
            } catch (IOException error) {
                System.err.println("ReadDICOM IOexception error");
                MipavUtil.displayError("FileDicom: " + error);
                throw (error);
            }
        } else { // encapsulated

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
                } catch (ArrayIndexOutOfBoundsException aioobe) {
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
        double intercept = fileInfo.getRescaleIntercept();
        
        if (fileInfo.getModality() == FileInfoBase.MAGNETIC_RESONANCE) {
            slope = 1;
            intercept = 0;
        }
        
        for (int i = 0; i < buffer.length; i++) {
            tmp = buffer[i];

            // System.out.println("buffer[i] = " + buffer[i]);
            if (tmp != pixelPad) {

                if (tmp < min) {
                    min = tmp;
                } else if (tmp > max) {
                    max = tmp;
                }
            }
        }

        fileInfo.setMin(min);
        fileInfo.setMax(max);

        if ((pixelPad <= min) || (pixelPad >= max)) {

            for (int i = 0; i < buffer.length; i++) {

                // tmp = buffer[i];
                // need to fix - we're altering image data so that when the file is
                // written, it is not exactly the same as when it was read in, i.e.,
                // there are no pixel pad values stored in the buffer; they've all been
                // converted to the minimum value.
                if (buffer[i] != pixelPad) {
                    buffer[i] = (float) ((buffer[i] * slope) + intercept);
                } else {
                    buffer[i] = (float) ((min * slope) + intercept);
                }
            }
        } else {

            if ((slope != 1) || (intercept != 0)) {

                for (int i = 0; i < buffer.length; i++) {
                    buffer[i] = (float) ((buffer[i] * slope) + intercept); // Rescale data
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
     *   <li>units of measure</li>
     *   <li>pixel pad</li>
     *   <li>file info minimum and maximum</li>
     * </ul>
     *
     * <p>This method would be used for short- (byte-) size image datasuch as PET data. This method is faster than the
     * float buffer version of this method since not as much type-conversion is needed.</p>
     *
     * @param      buffer     2D buffer used for temporary storage of data
     * @param      imageType  The type of image (i.e. SHORT, BYTE, ...)
     * @param      imageNo    For multiFrame images, imageNo >=1. For single slice image imageNo = 0.
     *
     * @return     The image
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRaw
     */
    public void readImage(short[] buffer, int imageType, int imageNo) throws IOException {

        // Read in header info, if something goes wrong, print out error
        if (hasHeaderBeenRead == false) {

            if (!readHeader(true)) {
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

        if (!encapsulated) {

            try { // rafile (type RandomAccessFile) for header, rawfile (type FileRaw) for image data.
                rawFile.setImageFile(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

                if (ModelImage.isColorImage(imageType)) {
                    rawFile.setPlanarConfig(fileInfo.planarConfig);
                    rawFile.readImage(buffer, fileInfo.getOffset() + (imageNo * (buffer.length / 4 * 3)), imageType);
                    rawFile.raFile.close();
                    rawFile.raFile = null;
                } else {
                    rawFile.readImage(buffer, fileInfo.getOffset() + (imageNo * buffer.length * fileInfo.bytesPerPixel),
                                      imageType);
                    rawFile.raFile.close();
                    rawFile.raFile = null;
                }
            } catch (IOException error) {
                error.printStackTrace();
                System.err.println("ReadDICOM IOexception error");
                MipavUtil.displayError("FileDicom: " + error);
                throw (error);
            }
        } else { // encapsulated

            if (jpegData == null) {
                jpegData = encapsulatedImageData();
            }

            if (jpegData != null) {

                try {
                    int j = imageNo * buffer.length;

                    for (int i = 0; i < buffer.length; i++) {
                        buffer[i] = (short) jpegData[j];
                        j++;
                    }
                } catch (ArrayIndexOutOfBoundsException aioobe) {
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

            if ((imageType == ModelStorageBase.UBYTE) || (imageType == ModelStorageBase.USHORT)) {
                pixelPad = (short) (fileInfo.getPixelPadValue().shortValue() & 0xffff);
            } else {
                pixelPad = fileInfo.getPixelPadValue().shortValue();
            }
        }

        float slope = (float) fileInfo.getRescaleSlope();
        float intercept = (float) fileInfo.getRescaleIntercept();
        
        if (fileInfo.getModality() == FileInfoBase.MAGNETIC_RESONANCE){
            slope = 1;
            intercept = 0;
        }

        for (int i = 0; i < buffer.length; i++) {
            tmp = buffer[i];

            if (tmp != pixelPad) {

                if (tmp < min) {
                    min = tmp;
                } else if (tmp > max) {
                    max = tmp;
                }
            }
        }

        fileInfo.setMin(min);
        fileInfo.setMax(max);

        if ((pixelPad <= min) || (pixelPad >= max)) {

            for (int i = 0; i < buffer.length; i++) {

                // tmp = buffer[i];
                // need to fix - we're altering image data so that when the file is
                // written, it is not exactly the same as when it was read in, i.e.,
                // there are no pixel pad values stored in the buffer; they've all been
                // converted to the minimum value.
                if (buffer[i] != pixelPad) {
                    buffer[i] = (short) ((buffer[i] * slope) + intercept);
                } else {
                    buffer[i] = (short) ((min * slope) + intercept);
                }
            }
        } else {

            if ((slope != 1) || (intercept != 0)) {

                for (int i = 0; i < buffer.length; i++) {
                    buffer[i] = (short) ((buffer[i] * slope) + intercept); // Rescale data
                }
            }
        }
        // End of Matt changes for 2/2003
    }


    /**
     * Reads in all the tags available in the file and stores them in the Hashtable in FileInfoDicom. This method
     * handles the various tags that are present at the beginning of a DICOM image file. It also sets the important File
     * Info variables based on what it finds.
     *
     * <p>The method will return with a failure code if it finds the tag &quot;0000,0000&quot; or it mis-reads the
     * header and starts reading from an odd byte.</p>
     *
     * <p>As the reader runs through the tags in the header, it reads them based on the type. There are 7 types:</p>
     *
     * <ul>
     *   <li>typeString</li>
     *   <li>typeShort</li>
     *   <li>typeInt</li>
     *   <li>typeFloat</li>
     *   <li>typeDouble</li>
     *   <li>typeSequence</li>
     *   <li>typeUnknown</li>
     * </ul>
     *
     * <p>Any special handling based on type occurs for each tag (@see FileInfoDicom), then it is added to the DICOM
     * tags table. Each tag is checked against a small list of individual tags, as some tags have an effect on the way
     * the following tags are interpreted.</p>
     *
     * <p>This method also affects some of the properties of the FileInfoDicom.</p>
     *
     * <ul>
     *   <li>MetaGroupLength</li>
     *   <li>Units of Measure</li>
     *   <li>Transfer Syntax</li>
     *   <li>Extents</li>
     * </ul>
     *
     * Display type changes the modality; image length is then also recalculated.
     *
     * @return     <code>true</code> if successful, otherwise <code>false</code>.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileInfoDicom
     * @see        #getNextElement(boolean)
     * @see        #convertGroupElement(int, int)
     */
    public boolean readParserHeader() throws IOException {
        int[] extents;
        String type, // type of data; there are 7, see FileInfoDicom
               name; // string representing the tag

        boolean endianess = FileBase.LITTLE_ENDIAN; // all DICOM files start as little endian
        boolean flag = true;

        loadTagBuffer();

        String strValue = null;
        Object data = null;

        try {
            extents = new int[2];
        } catch (OutOfMemoryError e) {

            if (!isQuiet()) {
                MipavUtil.displayError("Out of memory in FileDicom.readHeader");
            }

            throw new IOException();
        }

        metaGroupLength = 0;
        elementLength = 0;
        fileInfo.setEndianess(endianess);

        skipBytes(ID_OFFSET); // Find "DICM" tag

        // In v. 3.0, within the ID_OFFSET is general header information that
        // is not encoded into data elements and not present in DICOM v. 2.0.
        // However, it is optional.

        if (!getString(4).equals("DICM")) {
            fileInfo.containsDICM = false;
            seek(0); // set file pointer to zero
        }

        fileInfo.setDataType(ModelStorageBase.SHORT); // Default file type

        while (flag == true) {

            if (fileInfo.containsDICM) {

                // endianess is defined in a tag and set here, after the transfer
                // syntax group has been read in
                if (getFilePointer() >= (ID_OFFSET + metaGroupLength)) {
                    endianess = fileInfo.getEndianess();
                }
            } else {

                if (getFilePointer() >= metaGroupLength) {
                    endianess = fileInfo.getEndianess();
                }
            }

            getNextElement(endianess); // gets group, element, length
            name = convertGroupElement(groupWord, elementWord);

            // Preferences.debug("group = " + groupWord + " element = " + elementWord + " length = " + elementLength +
            // "\n");
            if ((fileInfo.vr_type == FileInfoDicom.IMPLICIT) || (groupWord == 2)) {

                // implicit VR means VR is based on tag as defined in dictionary
                try {
                    type = ((FileDicomTag) fileInfo.getEntry(name)).getType();
                } catch (NullPointerException e) {
                    type = "typeUnknown";
                }
            } else { // Explicit VR
                type = FileDicomTag.getType(new String(vr));

                if ((FileDicomTag) fileInfo.getEntry(name) == null) {

                    // put private tags with explicit VRs in file info hashtable
                    fileInfo.putTag(name, new FileDicomTag(groupWord, elementWord, null));
                }
            }

            if ((fileInfo.vr_type == FileInfoDicom.EXPLICIT) && (groupWord != 2)) {
                ((FileDicomTag) fileInfo.getEntry(name)).setVR(new String(vr));
            }

            if ((elementWord == 0) && (elementLength == 0)) { // End of file

                if (!isQuiet()) {
                    MipavUtil.displayError("Error:  Unexpected end of file.  Unable to load image.");
                }

                throw new IOException("Error while reading header");
            }

            if ((getFilePointer() & 1) != 0) { // The location is at an odd byte number

                if (!isQuiet()) {
                    MipavUtil.displayError("Error:  Input file corrupted.  Unable to load image.");
                }

                throw new IOException("Error while reading header");
            }

            try {

                if (type.equals("typeString")) {
                    strValue = getString(elementLength);
                    fileInfo.setValue(new FileDicomKey(name), strValue, elementLength);
                } else if (type.equals("otherByteString")) {
                    int tagVM = ((FileDicomTag) fileInfo.getEntry(name)).getVM();

                    data = getByte(tagVM, elementLength, endianess);
                    fileInfo.setValue(new FileDicomKey(name), data, elementLength);
                } else if (type.equals("otherWordString") && !name.equals("0028,1201") && !name.equals("0028,1202") &&
                               !name.equals("0028,1203") && !name.equals("7FE0,0010")) {
                    int tagVM = ((FileDicomTag) fileInfo.getEntry(name)).getVM();

                    data = getByte(tagVM, elementLength, endianess);
                    fileInfo.setValue(new FileDicomKey(name), data, elementLength);
                } else if (type.equals("typeShort")) {
                    int tagVM = ((FileDicomTag) fileInfo.getEntry(name)).getVM();
                    data = getShort(tagVM, elementLength, endianess);
                    fileInfo.setValue(new FileDicomKey(name), data, elementLength);

                } else if (type.equals("typeInt")) {
                    int tagVM = ((FileDicomTag) fileInfo.getEntry(name)).getVM();
                    data = getInteger(tagVM, elementLength, endianess);
                    fileInfo.setValue(new FileDicomKey(name), data, elementLength);

                } else if (type.equals("typeFloat")) {
                    int tagVM = ((FileDicomTag) fileInfo.getEntry(name)).getVM();
                    data = getFloat(tagVM, elementLength, endianess);
                    fileInfo.setValue(new FileDicomKey(name), data, elementLength);

                } else if (type.equals("typeDouble")) {
                    int tagVM = ((FileDicomTag) fileInfo.getEntry(name)).getVM();
                    data = getDouble(tagVM, elementLength, endianess);
                    fileInfo.setValue(new FileDicomKey(name), data, elementLength);

                } else if (type.equals("typeSequence") || ((type == "typeUnknown") && (elementLength == -1))) {
                    int gr = groupWord;
                    int el = elementWord;
                    int len = elementLength;

                    // save these values because they'll change as the sequence is read in below.
                    Object sq = getSequence(endianess, len);

                    try {
                        fileInfo.setValue(new FileDicomKey(name), sq, elementLength);
                    } catch (NullPointerException e) {
                        fileInfo.putTag(name, new FileDicomTag(gr, el, sq));
                    }
                    // Preferences.debug("Finished sequence tags.\n", 2);
                }
            } catch (OutOfMemoryError e) {

                if (!isQuiet()) {
                    MipavUtil.displayError("Out of memory in FileDicom.readHeader");
                }

                throw new IOException();
            }

            if (name.equals("0002,0000")) { // length of the transfer syntax group

                if (data != null) {
                    metaGroupLength = ((Integer) (data)).intValue();
                }
            } else if (name.equals("0002,0010")) {

                // Transfer Syntax UID: DICOM part 10 page 13, part 5 p. 42-48, Part 6 p. 53
                // 1.2.840.10008.1.2        Implicit VR Little Endian (Default)
                // 1.2.840.10008.1.2.1      Explicit VR Little Endian
                // 1.2.840.10008.1.2.2      Explicit VR Big Endian
                // 1.2.840.10008.1.2.4.50   8-bit Lossy JPEG (JPEG Coding Process 1)
                // 1.2.840.10008.1.2.4.51   12-bit Lossy JPEG (JPEG Coding Process 4)
                // we should bounce out if we don't support this transfer syntax
                if (strValue.trim().equals("1.2.840.10008.1.2")) {
                    fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
                    fileInfo.vr_type = FileInfoDicom.IMPLICIT;
                    encapsulated = false;
                } else if (strValue.trim().equals("1.2.840.10008.1.2.1")) {
                    fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
                    fileInfo.vr_type = FileInfoDicom.EXPLICIT;
                    encapsulated = false;
                } else if (strValue.trim().equals("1.2.840.10008.1.2.2")) {
                    fileInfo.setEndianess(FileBase.BIG_ENDIAN);
                    fileInfo.vr_type = FileInfoDicom.EXPLICIT;
                    encapsulated = false;
                } else if (strValue.trim().startsWith("1.2.840.10008.1.2.4.")) { // JPEG
                    fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
                    fileInfo.vr_type = FileInfoDicom.EXPLICIT;
                    encapsulated = true;

                    if (strValue.trim().equals("1.2.840.10008.1.2.4.57") ||
                            strValue.trim().equals("1.2.840.10008.1.2.4.58") ||
                            strValue.trim().equals("1.2.840.10008.1.2.4.65") ||
                            strValue.trim().equals("1.2.840.10008.1.2.4.66") ||
                            strValue.trim().equals("1.2.840.10008.1.2.4.70")) {
                        lossy = false;
                    } else {
                        lossy = true;
                    }
                } else {

                    if (!isQuiet()) {
                        MipavUtil.displayError("MIPAV does not support transfer syntax:\n" + strValue);
                    }

                    flag = false; // break loop

                    return false; // couldn't read it!
                }
            } else if (name.equals("0028,0010")) { // Set the extents, used for reading the image
                extents[1] = ((Short) fileInfo.getValue(name)).intValue();
            } else if (name.equals("0028,0011")) {
                extents[0] = ((Short) fileInfo.getValue(name)).intValue();
            } else if (name.equals("0028,0010")) { // Set the extents, used for reading the image
                extents[1] = ((Short) fileInfo.getValue(name)).intValue();
                // fileInfo.columns = extents[1];
            } else if (name.equals("0028,0011")) {
                extents[0] = ((Short) fileInfo.getValue(name)).intValue();
                // fileInfo.rows = extents[0];
            } else if (name.equals("0028,1201")) { // LUT for red-channel

                // red channel LUT
                try {
                    getColorPallete(new FileDicomKey(name));
                } catch (IllegalArgumentException iae) {
                    // System.err.println("Dude.  You screwed up somewhere!  "+
                    // " storeColorPallete throws iae when something other than"+
                    // " 0028,120[1|2|3] is used.");
                }
            } else if (name.equals("0028,1202")) { // LUT for red-channel

                // blue channel LUT
                try {
                    getColorPallete(new FileDicomKey(name));
                } catch (IllegalArgumentException iae) {
                    // System.err.println("Dude.  You screwed up somewhere!  "+
                    // " storeColorPallete throws iae when something other than"+
                    // " 0028,120[1|2|3] is used.");
                }
            } else if (name.equals("0028,1203")) { // LUT for red-channel

                // green channel LUT
                try {
                    getColorPallete(new FileDicomKey(name));
                } catch (IllegalArgumentException iae) {
                    // storeColorPallete throws iae when something other than
                    // 0028,120[1|2|3] is used.
                    // System.err.println("Dude.  You screwed up somewhere!  "+
                    // " storeColorPallete throws iae when something other than"+
                    // " 0028,120[1|2|3] is used.");
                }
            } else if (name.equals("7FE0,0010")) {
                fileInfo.setExtents(extents);
                flag = false; // break loop
            } else if (type.equals("typeUnknown")) { // Private tag, may not be reading in correctly.

                try {

                    if (fileInfo.getEntry(name) != null) {
                        fileInfo.setValue(name, readUnknownData(), elementLength);
                    } else {
                        fileInfo.putTag(name, new FileDicomTag(groupWord, elementWord, readUnknownData()));
                    }
                } catch (OutOfMemoryError e) {

                    if (!isQuiet()) {
                        MipavUtil.displayError("Out of memory error while reading \"" + fileName +
                                               "\".\nThis may not be a DICOM image.");
                    } else {
                        Preferences.debug("Out of memory storing unknown tags in FileDicom.readHeader\n");
                    }

                    throw new IOException("Out of memory storing unknown tags in FileDicom.readHeader");
                } catch (NullPointerException npe) {
                    System.err.println("name: " + name);
                    System.err.print("no hashtable? ");
                    System.err.println(fileInfo.getTagsList() == null);
                    throw npe;
                }
            }
        }

        raFile.close();

        return true;

    }

    /**
     * Sets the file info and sets the hasHeaderBeenRead to true.
     *
     * @param  fiDicom  File info structure
     */
    public final void setFileInfo(FileInfoDicom fiDicom) {
        fileInfo = fiDicom;
        hasHeaderBeenRead = true;
    }

    /**
     * Accessor that sets the file name and allocates new FileInfo, File and RandomAccess file objects based on the new
     * image file based on the new filename and the old directory. This method sets the filename property of the
     * FileDicom, recreates a raw file for random access file to read the image file pointed to by the filename (the
     * file is opened for reading only; it cannot be written to). A new file info is created with this file info and the
     * old DICOM dictionary is used. Finally, the endianess is set to be little-endian.
     *
     * <p>Should an out-of-memory condition occur an IOException will be thrown.</p>
     *
     * @param      fName  File name for the image file.
     *
     * @exception  IOException  if there is an error constructing the files.
     */
    public final void setFileName(String fName) throws IOException {
        this.setFileName(new File(fileDir + fName));
    }

    /**
     * Accessor that sets the file name and allocates new FileInfo, File and RandomAccess file objects based on the new
     * image file based on the new filename and the new directory. This method sets the filename property of the
     * FileDicom, recreates a raw file for random access file to read the image file pointed to by the filename (the
     * file is opened for reading only; it cannot be written to). A new file info is created with this file info and the
     * old DICOM dictionary is used. Finally, the endianess is set to be little-endian.
     *
     * <p>Should an out-of-memory condition occur an IOException will be thrown.</p>
     *
     * @param      f  image file to point to.
     *
     * @exception  IOException  if there is an error constructing the files.
     */
    public void setFileName(File f) throws IOException {
        fileName = f.getName();
        fileDir = f.getParent() + File.separator;
        fileHeader = f;

        try {

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (IOException ex) { }
            }

            raFile = new RandomAccessFile(fileHeader, "r");
            fileInfo = new FileInfoDicom(fileName, fileDir, FileUtility.DICOM);
            fileInfo.resetDictionary();
        } catch (OutOfMemoryError e) {

            if (!quiet) {
                MipavUtil.displayError("Out of memory in FileDicom.setFileName.");
            }

            throw new IOException("Out of Memory in FileDicom.setFileName" + e.getLocalizedMessage());
        }

        fileInfo.setEndianess(LITTLE_ENDIAN);
    }

    /**
     * Accessor that sets the file name and allocates new FileInfo, File and RandomAccess file objects based on the new
     * image file based on the new filename and the new directory. This method sets the filename property of the
     * FileDicom, recreates a raw file for random access file to read the image file pointed to by the filename (the
     * file is opened for reading only; it cannot be written to). A new file info is created with this file info and the
     * old DICOM dictionary is used. Finally, the endianess is set to be little-endian.
     *
     * <p>Should an out-of-memory condition occur an IOException will be thrown.</p>
     *
     * @param      fName  File name for the image file.
     * @param      fDir   Directory to locate the image file.
     *
     * @exception  IOException  if there is an error constructing the files.
     */
    public final void setFileName(String fName, String fDir) throws IOException {
        this.setFileName(new File(fDir + fName));
    }

    /**
     * sets the quiet option on the class.
     *
     * <p>The idea is that for all output messages, there should be the option to not bother the user with the message.
     * Ie., if an exception is thrown, and we normally tell the user that an error occurred, a calling class can set
     * this option so that the calling class can handle (either loudly or quietly, as needed) the error with its own
     * message. this could be upgraded to call with quiet to prevent user-queries from interrupting an automatic
     * process. But that is in the future.</p>
     *
     * <p>Note: In the future, this method and variable is to be moved to FileBase.</p>
     *
     * @param  q  whether this class should consider itself quiet, and by internal inspection, not notify the user.
     *            <code>True</code> is to consider itself to not notify the user. <code>False</code> is to notify the
     *            user, and is the default behaviour.
     */
    public final void setQuiet(boolean q) {
        quiet = q;
    }


    /**
     * Writes a dicom format type image.
     *
     * @param      image  Image model where the data is stored.
     * @param      start  Where in the buffer to start (begin slice).
     * @param      end    Where in the buffer to end (end slice).
     * @param      index  Index of the file information.
     *
     * @exception  IOException  if there is an error writing the file
     *
     * @see        FileRawChunk
     */
    public void writeImage(ModelImage image, int start, int end, int index) throws IOException {
        float[] data = null;
        short[] dataRGB = null;

        fileInfo = (FileInfoDicom) image.getFileInfo(index);

        // store that this DICOM has been saved/modified since original capture:
        stampSecondaryCapture(fileInfo);

        this.image = image;

        int length = end - start;

        try {
            raFile.setLength(0);
            writeHeader(raFile, fileInfo);

            // changed this to happen for ALL files, because it's necessary for CT
            // images, or else the min-max gets messed up.
            // if (fileInfo.getModality() == fileInfo.POSITRON_EMISSION_TOMOGRAPHY) {
            if (fileInfo.getDataType() == ModelStorageBase.ARGB) {
                dataRGB = new short[4 * (end - start)];
            } else {
                data = new float[(end - start)];
            }

            double invSlope = image.getFileInfo(index).getRescaleSlope(); // Varies per slice!!!
            double intercept = image.getFileInfo(index).getRescaleIntercept();
            FileRawChunk rawChunkFile;
            rawChunkFile = new FileRawChunk(raFile, fileInfo);

            switch (fileInfo.getDataType()) {

                case ModelStorageBase.BYTE:

                    byte[] data2 = new byte[length];
                    image.exportData(index * length, length, data);
                    for (int i = 0; i < data.length; i++) {
                        data2[i] = (byte) MipavMath.round((data[i] - intercept) / invSlope);
                    }

                    rawChunkFile.writeBufferByte(data2, 0, length);
                    data2 = null;
                    break;

                case ModelStorageBase.UBYTE:

                    short[] data3 = new short[length];
                    image.exportData(index * length, length, data);
                    for (int i = 0; i < data.length; i++) {
                        data3[i] = (short) MipavMath.round((data[i] - intercept) / invSlope);
                    }

                    rawChunkFile.writeBufferUByte(data3, 0, length);
                    data3 = null;
                    break;

                case ModelStorageBase.SHORT:

                    short[] data4 = new short[end - start];
                    image.exportData(index * length, length, data);
                    for (int i = 0; i < data.length; i++) {
                        data4[i] = (short) MipavMath.round((data[i] - intercept) / invSlope);
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
                        data5[i] = (short) MipavMath.round((data[i] - intercept) / invSlope);
                    }

                    rawChunkFile.writeBufferUShort(data5, 0, length, image.getFileInfo(0).getEndianess());
                    data5 = null;
                    break;

                case ModelStorageBase.ARGB:

                    short[] dRGB = new short[3 * length];
                    image.exportData(index * (4 * length), (4 * length), dataRGB);

                    for (int i = 1, iRGB = 0; i < dataRGB.length;) {

                        // Not sure slope intercept is needed here for color images
                        dRGB[iRGB++] = (short) MipavMath.round((dataRGB[i++] - intercept) / invSlope);
                        dRGB[iRGB++] = (short) MipavMath.round((dataRGB[i++] - intercept) / invSlope);
                        dRGB[iRGB++] = (short) MipavMath.round((dataRGB[i++] - intercept) / invSlope);
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

            rawChunkFile.close();
            data = null;
        } catch (IOException error) {
            throw new IOException("FileDicomWrite: " + error);
        } catch (OutOfMemoryError error) {

            if (!isQuiet()) {
                MipavUtil.displayError("Out of Memory in FileDicom.writeImage");
            }

            throw new IOException();
        }
    }

    /**
     * Writes a dicom format type image.
     *
     * @param      image  Image model where the data is stored.
     * @param      start  Start image index (begin slice).
     * @param      end    End image index (end slice).
     *
     * @exception  IOException  if there is an error writing the file.
     *
     * @see        FileRawChunk
     */
    public void writeMultiFrameImage(ModelImage image, int start, int end) throws IOException {

        fileInfo = (FileInfoDicom) image.getFileInfo(0);

        String nFramesStr = String.valueOf(end - start + 1);
        fileInfo.setValue("0028,0008", nFramesStr, nFramesStr.length());

        // store that this DICOM has been saved/modified since original capture:
        stampSecondaryCapture(fileInfo);
        this.image = image;

        int imageSize = image.getSliceSize();

        try {
            writeHeader(raFile, fileInfo);

            if (fileInfo.getModality() == fileInfo.POSITRON_EMISSION_TOMOGRAPHY) {
                float[] data = new float[imageSize];

                // if the data type isn't short, this will be majorly screwed up
                double invSlope = fileInfo.getRescaleSlope();
                double intercept = fileInfo.getRescaleIntercept();
                short[] data2 = new short[imageSize];

                image.exportData(0, imageSize, data);
                image.reallocate(fileInfo.getDataType());

                for (int i = 0; i < data.length; i++) {
                    data2[i] = (short) MipavMath.round((data[i] - intercept) / invSlope);
                }

                image.importData(0, data2, false);
            }

            FileRawChunk rawChunkFile;
            rawChunkFile = new FileRawChunk(raFile, fileInfo);

            for (int i = start; i <= end; i++) {
                rawChunkFile.writeImage(image, i * imageSize, (i * imageSize) + imageSize, i);
            }

            rawChunkFile.close();
        } catch (IOException error) {
            throw new IOException("FileDicomWrite: " + error);
        } catch (OutOfMemoryError error) {

            if (!isQuiet()) {
                MipavUtil.displayError("Out of Memory in FileDicom.writeImage");
            }

            throw new IOException();
        }
    }


    /**
     * Converts the integer values of the group word and element word into a string that is the hexadecimal
     * representation of group word and element word, separated by a comma.
     *
     * @param   groupWord    The group word of the element.
     * @param   elementWord  The element word of the element.
     *
     * @return  String representation of the group element.
     */
    private String convertGroupElement(int groupWord, int elementWord) {
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


    /**
     * Starts with image tag. goes with OB/OW. image file must be open, file ptr: raFile.
     *
     * @return     int[] the extracted/decapsulated RGB image data
     *
     * @exception  IOException  it cannot return the RGB image data, it'll just throw it.
     */
    private int[] encapsulatedImageData() throws IOException {

        System.out.println("FileDicom.encapsulatedImageData");

        try {

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (IOException ex) { }
            }

            fileHeader = new File(fileDir + fileName);
            raFile = new RandomAccessFile(fileHeader, "r");
        } catch (IOException e) {

            try {
                raFile = new RandomAccessFile(fileHeader, "r");
            } catch (IOException error) { }
        }

        initializeFullRead();
        seek(fileInfo.getOffset());

        boolean endianess = fileInfo.getEndianess();
        getNextElement(endianess); // gets group, element, length

        String name = convertGroupElement(groupWord, elementWord);

        if (!name.equals("7FE0,0010")) {

            if (!isQuiet()) {
                MipavUtil.displayError("FileDicom: Image Data tag not found.  Cannot extract encapsulated image data.");
            }

            throw new IOException("Image Data tag not found.  Cannot extract encapsulated image data.");
        }

        inSQ = true;
        getNextElement(endianess);
        name = convertGroupElement(groupWord, elementWord);

        int[] tableOffsets;

        if (elementLength > 0) {
            int numberInts = elementLength / 4;
            tableOffsets = new int[numberInts];

            for (int i = 0; i < numberInts; i++) {
                tableOffsets[i] = getInt(endianess);
            }
        }

        getNextElement(endianess);
        name = convertGroupElement(groupWord, elementWord);

        Vector v = new Vector();

        while (name.equals("FFFE,E000")) {
            byte[] imageFrag = new byte[elementLength]; // temp buffer

            for (int i = 0; i < imageFrag.length; i++) {
                imageFrag[i] = (byte) getByte(); // move into temp buffer
            }

            v.add(imageFrag);
            getNextElement(endianess);
            name = convertGroupElement(groupWord, elementWord);
        }

        if (!name.equals("FFFE,E0DD")) {

            if (!isQuiet()) {
                MipavUtil.displayWarning("End tag not present.  Image may have been corrupted.");
            }
        }

        raFile.close();

        if (v.size() > 1) {
            Vector v2 = new Vector();
            int[] jpegImage;

            for (Enumeration en = v.elements(); en.hasMoreElements();) {

                if (lossy == true) {
                    jpegImage = extractLossyJPEGImage((byte[]) en.nextElement());
                } else {
                    FileDicomJPEG fileReader = new FileDicomJPEG((byte[]) en.nextElement(), fileInfo.getExtents()[0],
                                                                 fileInfo.getExtents()[1]);
                    jpegImage = fileReader.extractJPEGImage();
                }

                v2.addElement(jpegImage);
            }

            int size = 0;

            for (Enumeration en = v2.elements(); en.hasMoreElements();) {
                size += ((int[]) en.nextElement()).length;
            }

            int count = 0;
            int[] imageData = new int[size];

            for (Enumeration en = v2.elements(); en.hasMoreElements();) {
                int[] temp = (int[]) en.nextElement();

                for (int i = 0; i < temp.length; i++) {
                    imageData[count++] = temp[i];
                }
            }

            inSQ = false;

            return imageData;

        } else {

            if (lossy == true) {
                inSQ = false;

                return extractLossyJPEGImage((byte[]) v.elementAt(0));
            } else {
                FileDicomJPEG fileReader = new FileDicomJPEG((byte[]) v.elementAt(0), fileInfo.getExtents()[0],
                                                             fileInfo.getExtents()[1]);
                inSQ = false;

                return fileReader.extractJPEGImage();
            }
        }
    }

    /**
     * Brute force removal of 1st fragment of JPEG encoded image. Cuts out JPEG encapsulated data and stores a temp JPEG
     * image to the temp directory. Calls the MediaTracker utility to decode the JPEG image.
     *
     * @param   imageFrag  Image fragment.
     *
     * @return  RGB image buffer (int[])- the JPEG image
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private int[] extractLossyJPEGImage(byte[] imageFrag) throws IOException {
        int w = 0, h = 0;

        ByteArrayInputStream stream = new ByteArrayInputStream(imageFrag);
        BufferedImage img = ImageIO.read(stream);

        w = img.getWidth(null);
        h = img.getHeight(null);

        if ((w <= 0) || (h <= 0)) {
            return new int[0];
        }

        // This is for RGB images
        int[] pixels = new int[w * h];
        int[] imgBuffer = new int[4 * w * h];
        PixelGrabber pg = new PixelGrabber(img, 0, 0, w, h, pixels, 0, w);

        try {
            pg.grabPixels();
        } catch (InterruptedException e) {
            Preferences.debug("JIMI: Interrupted waiting for pixels!" + "\n");

            return new int[0];
        }

        if ((pg.getStatus() & ImageObserver.ABORT) != 0) {
            Preferences.debug("JIMI: Image fetch aborted or errored" + "\n");

            return new int[0];
        }

        // copy the decoded JPEG image into the destination buffer
        int a, r, g, b, pixel;
        int i = 0;

        for (int y = 0; y < h; y++) {

            for (int x = 0; x < w; x++) {
                pixel = pixels[(y * w) + x];
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
     * @return  Object
     *
     * @throws  IOException  DOCUMENT ME!
     *
     * @param   vm         value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param   length     number of bytes to read out of data stream; the length is not used.
     * @param   endianess  byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
     *                     indicates little-endian.
     */
    private Object getByte(int vm, int length, boolean endianess) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null; // the Object we read in

        if (vm > 1) {
            Byte[] array = new Byte[length];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Byte((byte) getByte());
                len -= 1;
                i++;
            }

            readObject = array;
        } else if ((vm < 1) && (length > 2)) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            Byte[] array = new Byte[length];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Byte((byte) getByte());
                len -= 1;
                i++;
            }

            readObject = array;
        } else if (length > 0) {
            Byte[] array = new Byte[length];

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
     * to be active. Only DICOM keys <tt>0028,1201</tt>, <tt>0028,1202</tt> and <tt>0028,1203</tt> are accepted for use
     * in this method and all others cause an exception to be thrown. If the DICOM color pallete size tag exists in the
     * dictionary and is correct, then the data is read out of the color pallete tag as-specified; if the size in the
     * color pallete tag is invalid for DICOM standard v3, the data is assumed to be 16 bits wide; if the size-specifier
     * tag does not exist, the data in this tag is ignored. Parsing one particular key does not verify that the other
     * colour channel pallete keys exist.
     *
     * <p>While this method ensures that the tag in the dictionary is set
     * {@link FileInfoDicom#setValue(String, Object, int)}, it does not actually load the ModelLUT {@link ModelLUT}</p>
     *
     * @param   palleteKey  The DICOM key which contains some pallete or LUT information.
     *
     * @throws  IllegalArgumentException  When a DICOM key is provided that is not related to the pallete information;
     *                                    that is, one that is not <tt>0028,1201</tt>, <tt>0028,1202</tt>, or <tt>
     *                                    0028,1203</tt>.
     * @throws  IOException               A problem occurs when reading the image file.
     *
     * @see     "DICOM PS 3.3, Information Object Definitions"
     */
    private void getColorPallete(FileDicomKey palleteKey) throws IllegalArgumentException, IOException {

        if (!palleteKey.equals("0028,1201") && !palleteKey.equals("0028,1202") && !palleteKey.equals("0028,1203")) {
            System.out.println("Throwing exception in FileDicom:storeColorPallete.");
            throw new IllegalArgumentException("Not a color pallete tag");
        }

        try {

            // get channel lut specification.
            String specKey = palleteKey.getGroup() + "," +
                             Integer.toString(Integer.parseInt(palleteKey.getElement()) - 100);
            FileDicomTag palletSpec = (FileDicomTag) fileInfo.getEntry(specKey);

            // values are now loaded, try loading the LUT
            int numberOfLUTValues = (((Short) (palletSpec.getValueList()[0])).intValue() == 0)
                                    ? (int) Math.pow(2, 16) : ((Short) (palletSpec.getValueList()[0])).intValue();

            int numberOfBits = ((Short) (palletSpec.getValueList()[2])).intValue();
            // System.out.println("Writing storeColorPallete : number of Bits = " + numberOfBits );

            if ((numberOfBits != 8) && (numberOfBits != 0x10)) {

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

            fileInfo.setValue(palleteKey.getKey(), data, numberOfLUTValues);
        } catch (NullPointerException npe) {
            FileDicomTag t = ((FileDicomTag) fileInfo.getEntry(palleteKey));
            MipavUtil.displayError("This image carries " + t.getName() + ", but specifies it incorrectly.\n" +
                                   "The channel will be ignored.");
            Preferences.debug("FileDicom: Error Creating " + t.getName() + "(" + palleteKey.getKey() + ")" +
                              "; it will be ignored.\n");
        } catch (ArrayStoreException ase) {
            FileDicomTag t = ((FileDicomTag) fileInfo.getEntry(palleteKey));
            MipavUtil.displayError(t.getName() + "; " + ase.getLocalizedMessage() + "\n" +
                                   "This channel will be ignored.");
            Preferences.debug("FileDicom: Error creating " + t.getName() + "; " + ase.getLocalizedMessage() + "\n");
        } catch (ArrayIndexOutOfBoundsException aioobe) {
            MipavUtil.displayError("Error reading color LUT channel.  This Channel will be ignored.");
            Preferences.debug("Error reading color LUT channel.  This Channel will be ignored.\n");
            // System.err.println("Error reading color LUT channel.  This Channel will be ignored.");
        }
    }

    /**
     * Reads a set of DICOM tags from a DICOM sequence item, ending with the data-element end tag <code>
     * FFFE,E00D</code>. This list of tags in a DICOM sequence item and places them into a hashtable held within a
     * FileDicomItem.
     *
     * @param   itemLength  Length of the item in bytes.
     * @param   endianess   Big (true) or little (false).
     *
     * @return  The sequence item read in.
     *
     * @see     FileDicomItem
     */
    private Object getDataSet(int itemLength, boolean endianess) throws IOException {
        FileDicomItem item = new FileDicomItem();

        String type;
        int iValue = 0;
        float fValue = 0;
        double dValue = 0;
        boolean nullEntry = false;

        if ((itemLength == UNDEFINED_LENGTH) || (itemLength == ILLEGAL_LENGTH)) {
            itemLength = Integer.MAX_VALUE;
        }

        if (itemLength == 0) {
            itemLength = elementLength; // use the length of the SQ tag
        }

        int startfptr = (int) getFilePointer();

        // preread the next tag, because we do not want to store the
        if ((getFilePointer() - startfptr) <= itemLength) {

            // Sequence ITEM delimiter tags
            boolean oldSQ = inSQ;
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
        while (!nameSQ.equals("FFFE,E00D") && ((getFilePointer() - startfptr) < itemLength)) {
            // The following is almost exactly the same as the code in readHeader.  The main difference is the
            // information is stored in a hashtable in DicomItem that is initially empty.

            if (fileInfo.vr_type == FileInfoDicom.IMPLICIT) {

                try {
                    type = ((FileDicomTag) fileInfo.getEntry(nameSQ)).getType();
                } catch (NullPointerException e) {
                    type = "typeUnknown";
                }
            } else if (fileInfo.VR.equals("SQ")) {
                type = "typeSequence";
            } else {
                type = FileDicomTag.getType(new String(vr));
            }

            FileDicomTag entry;

            try {
                entry = (FileDicomTag) ((FileDicomTag) (fileInfo.getEntry(nameSQ))).clone();
            } catch (NullPointerException e) {
                entry = new FileDicomTag(groupWord, elementWord, null);
                nullEntry = true;
            }

            if (fileInfo.vr_type == FileInfoDicom.EXPLICIT) {
                entry.setVR(new String(vr));
            }

            try {

                // (type == "typeUnknown" && elementLength == -1) Implicit sequence tag if not in DICOM dictionary.
                if (type.equals("typeUnknown") && (elementLength != -1)) {
                    entry = new FileDicomTag(groupWord, elementWord, readUnknownData());
                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                } else if (type.equals("typeString") || type.equals("otherWordString")) {

                    if (elementLength == UNDEFINED_LENGTH) {
                        elementLength = 0;
                    }

                    String strValue = getString(elementLength);
                    entry.setValue(strValue, elementLength);
                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                    // Preferences.debug("aaaaaaString Tag: (" + nameSQ + ");\t" + type + "; value = " + strValue +
                    //             "; element length = "+ elementLength + "\n", 2);
                } else if (type.equals("otherByteString")) {

                    if (!nullEntry && (((FileDicomTag) fileInfo.getEntry(nameSQ)).getVM() > 1)) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else if (elementLength >= 2) {
                        entry.setValue(readUnknownData(), elementLength);
                    }

                    entry.setVR("OB");
                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + iValue +
                    //  "; element length = "+ elementLength + "\n", 2);
                } else if (type.equals("typeShort")) {

                    if (!nullEntry && (((FileDicomTag) fileInfo.getEntry(nameSQ)).getVM() > 1)) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else if (elementLength > 2) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else {
                        iValue = getUnsignedShort(endianess);
                        entry.setValue(new Short((short) iValue), elementLength);
                    }

                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + iValue +
                    //  "; element length = "+ elementLength + "\n", 2);
                } else if (type.equals("typeInt")) {

                    if (!nullEntry && (((FileDicomTag) fileInfo.getEntry(nameSQ)).getVM() > 1)) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else if (elementLength > 4) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else {
                        iValue = getInt(endianess);
                        entry.setValue(new Integer(iValue), elementLength);
                    }

                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + iValue +
                    //  "; element length = "+ elementLength + "\n", 2);
                } else if (type.equals("typeFloat")) {

                    if (!nullEntry && (((FileDicomTag) fileInfo.getEntry(nameSQ)).getVM() > 1)) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else if (elementLength > 4) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else {
                        fValue = getFloat(endianess);
                        entry.setValue(new Float(fValue), elementLength);
                    }

                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + fValue +
                    //  "; element length = "+ elementLength + "\n", 2);
                } else if (type.equals("typeDouble")) {

                    if (!nullEntry && (((FileDicomTag) fileInfo.getEntry(nameSQ)).getVM() > 1)) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else if (elementLength > 8) {
                        entry.setValue(readUnknownData(), elementLength);
                    } else {
                        dValue = getDouble(endianess);
                        entry.setValue(new Double(dValue), elementLength);
                    }

                    item.putTag(nameSQ, entry);
                    item.setLength(nameSQ, elementLength);
                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type + "; value = " + dValue +
                    //  "; element length = "+ elementLength + "\n", 2);
                }
                // (type == "typeUnknown" && elementLength == -1) Implicit sequence tag if not in DICOM dictionary.
                else if (type.equals("typeSequence") || ((type == "typeUnknown") && (elementLength == -1))) {
                    int len = elementLength;
                    String name = nameSQ;
                    Object sq2 = getSequence(endianess, len);
                    entry.setValue(sq2, len);
                    item.putTag(name, entry);
                    item.setLength(name, len);
                    // Preferences.debug("Tag: (" + nameSQ + ");\t" + type +
                    // "; element length = "+ elementLength + "\n", 2);
                }
            } catch (OutOfMemoryError e) {

                if (!isQuiet()) {

                    // Must add back Matt/Dave 11/2003
                    MipavUtil.displayError("Out of memory in FileDicom.getDataSet");
                    // yup, done.... Dave, 04-2004
                } else {
                    Preferences.debug("Out of memory in FileDicom.getDataSet\n");
                    // System.err.println("Out of memory in FileDicom.getDataSet");
                }

                throw new IOException();
            }

            if ((getFilePointer() - startfptr) < itemLength) {
                // preread the next tag, because we have yet to see the
                // end-sequence tag because we don't want to accidently
                // read the next new-item tag.

                // except that SQ is undef-len, and def-len items get an
                // extra tag read out.  // 30-jun-2004
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
     * @return  Object
     *
     * @throws  IOException  DOCUMENT ME!
     *
     * @param   vm         value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param   length     number of bytes to read out of data stream; the length is not used.
     * @param   endianess  byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
     *                     indicates little-endian.
     */
    private Object getDouble(int vm, int length, boolean endianess) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null;

        if (vm > 1) {
            Double[] array = new Double[length / 8];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Double(getDouble(endianess));
                len -= 8;
                i++;
            }

            readObject = array;
        } else if ((vm < 1) && (length > 8)) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data
            // we actually do it as above.
            Double[] array = new Double[length / 8];

            while (len > 0) {
                array[i] = new Double(getDouble(endianess));
                len -= 8;
                i++;
            }

            readObject = array;
        } else if (((vm == 1) && (length > 8))) {

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
     * @return  Object
     *
     * @throws  IOException  DOCUMENT ME!
     *
     * @param   vm         value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param   length     number of bytes to read out of data stream; the length is not used.
     * @param   endianess  byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
     *                     indicates little-endian.
     */
    private Object getFloat(int vm, int length, boolean endianess) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null;

        if (vm > 1) {
            Float[] array = new Float[length / 4];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Float(getFloat(endianess));
                len -= 4;
                i++;
            }

            readObject = array;
        } else if ((vm < 1) && (length > 4)) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data
            // we actually do it as above.
            Float[] array = new Float[length / 4];

            while (len > 0) {
                array[i] = new Float(getFloat(endianess));
                len -= 4;
                i++;
            }

            readObject = array;
        } else if (((vm == 1) && (length > 4))) {

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
     * @return  Object
     *
     * @throws  IOException  DOCUMENT ME!
     *
     * @param   vm         value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param   length     number of bytes to read out of data stream; the length is not used.
     * @param   endianess  byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
     *                     indicates little-endian.
     */
    private Object getInteger(int vm, int length, boolean endianess) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null;

        if (vm > 1) {
            Integer[] array = new Integer[length / 4];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Integer(getInt(endianess));
                len -= 4;
                i++;
            }

            readObject = array;
        } else if ((vm < 1) && (length > 4)) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            Integer[] array = new Integer[length / 4];

            while (len > 0) {
                array[i] = new Integer(getInt(endianess));
                len -= 4;
                i++;
            }

            readObject = array;
        } else if (((vm == 1) && (length > 4))) {

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
     * <p>See DICOM Specification Part 5 (1998) Section 7 pages 24-34.</p>
     *
     * @param   endianess  Big or little.
     * @param   b1         First byte of the tag to be tested before applying endianess.
     * @param   b2         Second byte of the tag to be tested before applying endianess.
     * @param   b3         Third byte of the tag to be tested before applying endianess.
     * @param   b4         Fourth byte of the tag to be tested before applying endianess.
     *
     * @return  Length of the element.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private int getLength(boolean endianess, byte b1, byte b2, byte b3, byte b4) throws IOException {
        boolean implicit = false;

        if ((fileInfo.vr_type == fileInfo.IMPLICIT) || (groupWord == 2)) {

            if (fileInfo.containsDICM) {

                // at this point transfer syntax not read; we know endianess
                // is little endian but vr may be explicit
                if ((getFilePointer() <= (FIRST_ELEMENT + metaGroupLength)) || (groupWord == 2)) {

                    if (((b1 < 65) || (b1 > 90)) && ((b2 < 65) || (b2 > 90))) {
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
                if (getFilePointer() <= metaGroupLength) {

                    if (((b1 < 65) || (b1 > 90)) && ((b2 < 65) || (b2 > 90))) {
                        implicit = true;
                    } else {
                        implicit = false;
                    }
                } else {
                    implicit = true; // transfer syntax has been read, implicit set
                }
            }
        }

        // displays the individual bytes.  It could be better, for instance
        // printing as individual integer values:
        // System.err.print("[ "+Integer.toString(b1, 0x10)+" " +
        // Integer.toString(b2, 0x10)+" " +
        // Integer.toString(b3, 0x10)+" " +
        // Integer.toString(b4, 0x10)+" ]"
        // );

        if (implicit) {

            // implicit VR with 32-bit length
            if (endianess == FileBase.LITTLE_ENDIAN) {
                return ((b1 & 0xff) + ((b2 & 0xff) << 8) + ((b3 & 0xff) << 16) + ((b4 & 0xff) << 24));
            } else {
                return (((b1 & 0xff) << 24) + ((b2 & 0xff) << 16) + ((b3 & 0xff) << 8) + (b4 & 0xff));
            }
        }
        // explicit VR with 32-bit length
        else if (((b1 == 79) && (b2 == 66)) || ((b1 == 79) && (b2 == 87)) || ((b1 == 83) && (b2 == 81)) ||
                     ((b1 == 85) && (b2 == 78))) {

            // VR =     'OB',       or       'OW'         or         'SQ'       or      'UN'
            vr[0] = (byte) b1;
            vr[1] = (byte) b2;
            fileInfo.VR = new String(vr);

            // SQ - check for length FFFFFFFF (undefined), otherwise should be 0.
            if ((b1 == 83) && (b2 == 81)) { // 'SQ'

                // but i can't figure out why we're making a big deal out
                // of SQ types; UNDEF_LENGTH -is- -1 which -is- FF FF FF FF.
                // maybe ensuring return type?
                read(byteBuffer4); // reads 4 byte length w/o endianess for testing

                if ((byteBuffer4[0] == 255) && (byteBuffer4[1] == 255) && (byteBuffer4[2] == 255) &&
                        (byteBuffer4[3] == 255)) {
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
            vr[0] = (byte) b1; // these are not VR for item tags!!!!!!!!!
            vr[1] = (byte) b2;

            fileInfo.VR = new String(vr);

            if (endianess == FileBase.LITTLE_ENDIAN) {
                return ((b3 & 0xff) | ((b4 & 0xff) << 8));
            } else {
                return (((b3 & 0xff) << 8) | (b4 & 0xff));
            }
        }
    }


    /**
     * Increments the location, then reads the elementWord, groupWord, and elementLength. It also tests for an end of
     * file and resets the elementWord if it encounters one.
     *
     * @param   bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                     endian.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void getNextElement(boolean bigEndian) throws IOException {

        groupWord = getUnsignedShort(bigEndian);
        elementWord = getUnsignedShort(bigEndian);
        // Preferences.debug("(just found: )"+Integer.toString(groupWord, 0x10) + ":"+Integer.toString(elementWord,
        // 0x10)+" - " ); System.err.print("( just found: ) "+      Integer.toString(groupWord, 0x10) +
        // ":"+Integer.toString(elementWord, 0x10)+      " - ");

        if (fileInfo.vr_type == fileInfo.EXPLICIT) {

            /*  explicit tags carry an extra 4 bytes after the tag (group,
             *  element) information to describe the type of tag.  the  element dictionary describes this info, so we
             * skip past it  here. (apr 2004)
             */
            String tagname = convertGroupElement(groupWord, elementWord);

            if (tagname.equals("FFFE,E000") || // sequence item begin
                    tagname.equals("FFFE,EOOD") || // sequence item end
                    tagname.equals("FFFE,E0DD") || // undefined-length sequence end
                    tagname.equals("FFFE,EEEE")) // reserved
            {
                elementLength = getInt(bigEndian);
            } else {
                read(byteBuffer4); // Reads the explicit VR and following two bytes.
                elementLength = getLength(bigEndian, byteBuffer4[0], byteBuffer4[1], byteBuffer4[2], byteBuffer4[3]);
                // Preferences.debug(" length " + Integer.toString(elementLength, 0x10) + "\n");
            }
        } else { // this is what is standardly used.

            // either IMPLICIT or group element is not (FFFE,E000)
            read(byteBuffer4);
            elementLength = getLength(bigEndian, byteBuffer4[0], byteBuffer4[1], byteBuffer4[2], byteBuffer4[3]);
        }
    }


    /**
     * Gets the sequence in a sequence tag. Sequences of items have special encodings that are detailed in the DICOM
     * standard. There is usually an "item" tag, then a dataset encoded exactly like other tags, then a tag indicating
     * the end of the sequence.
     *
     * <P>For further information see the DICOM Standard, Part 5, Section 7.</P>
     *
     * @param   endianess  Big or little
     * @param   seqLength  Length of this sequence, although possibly left undefined.
     *
     * @return  A DicomSQ object which stores the new tags and their info
     *
     * @see     DicomSQ
     */
    private Object getSequence(boolean endianess, int seqLength) throws IOException {
        FileDicomSQ sq = new FileDicomSQ();

        // There is no more of the tag to read if the length of the tag
        // is zero.  In fact, trying to get the Next element is potentially
        // bad, so we'll just shut down the reading here.
        if (seqLength == 0) {
            return sq;
        }

        // hold on to where the sequence is before items for measuring
        // distance from beginning of sequence
        int seqStart = (int) getFilePointer();
        inSQ = true;

        getNextElement(endianess); // gets the first ITEM tag
        Preferences.debug("Item: " + Integer.toString(groupWord, 0x10) + "," + Integer.toString(elementWord, 0x10) +
                          " for " + Integer.toString(elementLength, 0x10) + " # readfrom: " +
                          Long.toString(getFilePointer(), 0x10) + "\n");

        inSQ = false;
        nameSQ = convertGroupElement(groupWord, elementWord);

        // Preferences.debug("getSquence: nameSQ = " + nameSQ +
        // " fptr = " + Long.toString(getFilePointer(), 0x10) + "\n");
        try {

            if ((seqLength == UNDEFINED_LENGTH) || (seqLength == ILLEGAL_LENGTH)) {

                while (!nameSQ.equals("FFFE,E0DD")) {

                    if (nameSQ.equals("FFFE,E000")) {

                        // elementLength here is the length of the
                        // item as it written into the File
                        if (elementLength == 0) {
                            FileDicomItem item = new FileDicomItem();
                            sq.addItem(item);
                        } else {
                            sq.addItem((FileDicomItem) getDataSet(elementLength, endianess));
                        }
                    } else if (nameSQ.equals("FFFE,E00D")) {

                        // possibility of getting here when subsequence tag length == -1
                        // end of sub-sequence tag
                        Preferences.debug("End of sub-sequence (FFFE,E00D) found; nothing done.\n");
                    } else { // should never get here
                        Preferences.debug("getSequence(): sub-sequence tags not starting with FFFE,E000\n", 2);
                    }

                    inSQ = true; // don't add the element length to the location
                    getNextElement(endianess); // skipping the tag-length???
                    nameSQ = convertGroupElement(groupWord, elementWord);
                    inSQ = false; // may now add element length to location

                    // Preferences.debug("Next item of seq.  "+
                    // "nameSQ: " + nameSQ +
                    // "  fpr: " + Long.toString(getFilePointer(), 0x10) + " \n");
                }
            } else { // sequence length is explicitly defined:

                // MUST be "<", rather than "<=" because when
                // fptr - seqStart == seqLength we want to move along to
                // other tags; it indicates that we are done with this
                // sequence.

                while ((getFilePointer() - seqStart) < seqLength) {

                    // loop is meant to read out each sub-sequence tag from the sequence.
                    // Must make it able to read out the
                    Preferences.debug(nameSQ + "; len:" + Long.toString(getFilePointer() - seqStart, 0x10) + "\n");

                    if (nameSQ.equals("FFFE,E000")) { // this should always be true

                        // int offset = (int)raFile.getFilePointer(); // where the data set begins for this item
                        // elementLength here is the length of the
                        // item as it written into the File
                        // System.out.println("Special ele length = " + Integer.toString(elementLength, 0x10));
                        sq.addItem((FileDicomItem) getDataSet(elementLength, endianess));
                    } else { // should never get here
                        Preferences.debug("getSequence(): sub-sequence tags not starting with FFFE,E000\n", 2);
                        // System.err.println("getSequence(): sub-sequence tags not starting with FFFE,E000");
                    }

                    if ((getFilePointer() - seqStart) < seqLength) { // '<=' or just '<'??
                        inSQ = true; // don't add the element length to the location
                        Preferences.debug("[FileDicom.getSequence():]" + Integer.toString(groupWord, 0x10) + "-" +
                                          Integer.toString(elementWord, 0x10) + " -- ");
                        getNextElement(endianess); // skipping the tag-length???
                        Preferences.debug("  the length: " + Integer.toString(elementLength, 0x10) + "\n");
                        nameSQ = convertGroupElement(groupWord, elementWord);
                        Preferences.debug("after converting group-element: " + nameSQ + "!!!!\n");
                        inSQ = false; // may now add element length to location

                        //                            System.err.println("pulled out next item of seq.  nameSQ: " +nameSQ);
                    }
                }
            }
        } catch (Exception error) {
            error.printStackTrace();
            Preferences.debug("Exception caught; Problem in FileDicom.getSequence\n", 2);
            Preferences.debug(error.toString() + "\n");
        }

        return sq;
    }


    /**
     * Reads a length of the data and deposits it into a single Short or an array of Short as needed by the tag's VM.
     *
     * @return  Object
     *
     * @throws  IOException  DOCUMENT ME!
     *
     * @param   vm         value multiplicity of the DICOM tag data. VM does not represent how many to find.
     * @param   length     number of bytes to read out of data stream; the length is not used.
     * @param   endianess  byte order indicator; here <code>true</code> indicates big-endian and <code>false</code>
     *                     indicates little-endian.
     */
    private Object getShort(int vm, int length, boolean endianess) throws IOException {
        int len = (elementLength == UNDEFINED_LENGTH) ? 0 : elementLength;
        int i = 0;
        Object readObject = null; // the Object we read in

        if (vm > 1) {
            Short[] array = new Short[length / 2];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Short((short) getUnsignedShort(endianess));
                len -= 2;
                i++;
            }

            readObject = array;
        } else if (((vm < 1) && (length > 2))) {

            // not a valid VM, but we don't initialise the VM to 1,
            // so we will use this fact to guess at valid data.
            // we actually do it as above.
            Short[] array = new Short[length / 2];

            while (len > 0) { // we should validate with VM here too
                array[i] = new Short((short) getUnsignedShort(endianess));
                len -= 2;
                i++;
            }

            readObject = array;
        } else if (((vm == 1) && (length > 2))) {

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
     * @return  A Byte array of length elementLength with the data stored in it.
     *
     * @throws  IOException  DOCUMENT ME!
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
     * @param  fileInfo  File info structure to set.
     */
    private void stampSecondaryCapture(FileInfoDicom fileInfo) {

        // be nice: let anyone viewing this DICOM file know this image became a DICOM sometime after capture
        fileInfo.setValue("0008,0064", "WSD", 3); // Conversion Type: Workstation

        Date currentDate = new Date();

        // mess with tags outside of the fileInfo structure (fileInfo doens't handle dicom formatting), and we want the
        // tags to figure things out (0018,1012): Date of Secondary Capture
        java.text.SimpleDateFormat formatter = new java.text.SimpleDateFormat("MM'/'dd'/'yyyy");
        String temp = formatter.format(currentDate);
        FileDicomTag tag = (FileDicomTag) fileInfo.getEntry("0018,1012");
        tag.setValue(temp);
        fileInfo.putTag("0018,1012", tag);

        // (0018,1014): Time of Secondary Capture
        formatter.applyLocalizedPattern("HH:mm:ss");
        temp = formatter.format(currentDate);
        tag = (FileDicomTag) fileInfo.getEntry("0018,1014");
        tag.setValue(temp);
        fileInfo.putTag("0018,1014", tag);

        // (0018,1016): Secondary Capture Device manufacturer
        temp = "National Institutes of Health, Center for Information Technology";
        tag = (FileDicomTag) fileInfo.getEntry("0018,1016");
        tag.setValue(temp);
        fileInfo.putTag("0018,1016", tag);

        // (0018,1018): Secondary Capture Device Manufacturer's Model Name
        temp = "Medical Image Processing, Analysis and Visualization (MIPAV)";
        tag = (FileDicomTag) fileInfo.getEntry("0018,1018");
        tag.setValue(temp);
        fileInfo.putTag("0018,1018", tag);

        // (0018,1019): Secondary Capture Device Software Version(s)
        tag = (FileDicomTag) fileInfo.getEntry("0018,1019");
        tag.setValue(MipavUtil.getVersion());
        fileInfo.putTag("0018,1019", tag);

    }

    /**
     * Writes the tags of the DICOM header.
     *
     * @param      outputFile  Output file to write to.
     * @param      fileInfo    File info structure for this file.
     *
     * @exception  IOException  if there is an error writing the file.
     *
     * @see        FileInfoDicom
     */
    private void writeHeader(RandomAccessFile outputFile, FileInfoDicom fileInfo) throws IOException {

        // all DICOM files start out as little endian
        boolean endianess = FileBase.LITTLE_ENDIAN;
        FileDicomTag[] dicomTags = fileInfo.sortTagsList();
        int metaGroupLength = 0;

        if (fileInfo.containsDICM) {

            // write "DICM" tag
            byte[] skip = new byte[ID_OFFSET];

            for (int k = 0; k < skip.length; k++) {
                skip[k] = (byte) 0;
            }

            outputFile.write(skip);
            outputFile.writeBytes("DICM");
        }

        for (int i = 0; i < dicomTags.length; i++) {
            String type = "";
            String vr = dicomTags[i].getVR();

            // System.out.println("w = " + dicomTags[i].toString());
            try {

                if (fileInfo.vr_type == fileInfo.EXPLICIT) {

                    // explicit VRs may be difference from implicit; in this case use the explicit
                    type = FileDicomTag.getType(vr);
                } else {
                    type = dicomTags[i].getType();
                }
            } catch (NullPointerException e) {
                type = "typeUnknown";
            }

            // System.out.println(" Name = " + dicomTags[i].toString());
            int gr = dicomTags[i].getGroup();
            int el = dicomTags[i].getElement();

            // In the future, getLength should calculate the length instead of retrieveing a stored version
            // I think Dave already solved part of this problem with getNumberOfValues (just multiply by size_of_type.
            int length = dicomTags[i].getLength();

            int nValues = dicomTags[i].getNumberOfValues();

            int vm = dicomTags[i].getVM();
            // Not sure if I need this - efilm adds it - also efilm has tag, 0000 lengths (meta lengths)for tag groups
            // (02, 08, ....) if (gr==2 && fileInfo.getVRType() == fileInfo.IMPLICIT){
            // fileInfo.setVRType(fileInfo.EXPLICIT); } else if (gr > 2){    fileInfo.setVRType(fileInfo.IMPLICIT); }

            if ((gr == 2) && (el == 0)) {

                // length of the transfer syntax group; after this group is written, need to change endianess
                metaGroupLength = ((Integer) dicomTags[i].getValue(false)).intValue();
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

            if ((fileInfo.vr_type == fileInfo.EXPLICIT) && (vr != null)) {
                outputFile.writeBytes(vr); // write explicit vr

                if (vr.equals("SQ")) {

                    // explicit VR 32 bit length
                    outputFile.writeShort(0); // skip two reserved bytes

                    if (((FileDicomSQ) dicomTags[i].getValue(false)).getLength() == 0) {
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

                        if (((FileDicomSQ) dicomTags[i].getValue(false)).getLength() == 0) {
                            writeInt(0, endianess); // write length of 0.
                        } else {
                            writeInt(0xFFFFFFFF, endianess); // write undefined length
                        }
                    } else {
                        writeInt(length, endianess); // implicit vr, 32 bit length
                    }
                } catch (NullPointerException noVRtypeException) {

                    // System.err.println("Found a tag ("+
                    // Integer.toString(gr, 0x10)+","+Integer.toString(el, 0x10) +
                    // ") without a TAG VR!  whoops.");
                    writeInt(length, endianess); // implicit vr, 32 bit length
                }
            }
            //                Preferences.debug( "this is "+dicomTags[i] + "\n", 2);

            // write as a string if string, unknown (private), or vm > 1
            // The VM part is consistent with how we're reading it in; hopefully
            // once we have test images we'll have a better way of reading & writing
            if ((length == 0) && !type.equals("typeSequence")) {
                // NOP to prevent other types from getting values being
                // written when no-length, non-sequences.
            } else if (type.equals("typeUnknown") || type.equals("otherByteString")) {

                // Unknowns are stored as an array of Bytes. VM does not apply ?
                Byte[] bytesV = null;
                bytesV = (Byte[]) dicomTags[i].getValue(false);

                byte[] bytesValue = new byte[bytesV.length];

                for (int k = 0; k < bytesV.length; k++) {
                    bytesValue[k] = bytesV[k].byteValue();
                    // System.err.print(" [" + bytesV[k].toString()+"]");
                }

                outputFile.write(bytesValue);
                // System.err.println();
            } else if (type.equals("otherWordString")) { // OW -- word = 2 bytes

                Short[] data = (Short[]) dicomTags[i].getValue(false);

                // We are not sure that that LUT endianess is always BIG
                // but the example images we have are.
                // Book 3 C.7.6.3.1.6 says to swap byte of OW.
                if (((gr == 0x28) && (el == 0x1201)) || ((gr == 0x28) && (el == 0x1202)) ||
                        ((gr == 0x28) && (el == 0x1203))) {

                    for (int vmI = 0; vmI < data.length; vmI++) {
                        writeShort(data[vmI].shortValue(), true);
                    }
                } else {

                    for (int vmI = 0; vmI < data.length; vmI++) {
                        writeShort(data[vmI].shortValue(), endianess);
                    }
                }

            } else if (type.equals("typeString")) {

                // VM - I don't think applies
                outputFile.writeBytes(dicomTags[i].getValue(false).toString());

                if ((dicomTags[i].getValue(false).toString().length() % 2) != 0) {
                    outputFile.writeBytes("\0");
                }
            } else if (type.equals("typeFloat")) {

                if ((vm > 1) || (nValues > 1)) {
                    Float[] data = (Float[]) dicomTags[i].getValue(false);

                    for (int vmI = 0; vmI < nValues; vmI++) {
                        writeFloat(data[vmI].floatValue(), endianess);
                    }
                } else {
                    writeFloat(((Float) dicomTags[i].getValue(false)).floatValue(), endianess);
                }
            } else if (type.equals("typeDouble")) {

                if ((vm > 1) || (nValues > 1)) {
                    Double[] data = (Double[]) dicomTags[i].getValue(false);

                    for (int vmI = 0; vmI < nValues; vmI++) {
                        writeDouble(data[vmI].doubleValue(), endianess);
                    }
                } else {
                    writeDouble(((Double) dicomTags[i].getValue(false)).doubleValue(), endianess);
                }
            } else if (type.equals("typeShort")) {

                if ((vm > 1) || (nValues > 1)) {
                    Short[] data = (Short[]) dicomTags[i].getValue(false);

                    for (int vmI = 0; vmI < nValues; vmI++) {
                        writeShort(data[vmI].shortValue(), endianess);
                    }
                } else {
                    writeShort(((Short) dicomTags[i].getValue(false)).shortValue(), endianess);
                }
            } else if (type.equals("typeInt")) {

                if ((vm > 1) || (nValues > 1)) {
                    Integer[] data = (Integer[]) dicomTags[i].getValue(false);

                    for (int vmI = 0; vmI < nValues; vmI++) {
                        writeInt(data[vmI].intValue(), endianess);
                    }
                } else {
                    writeInt(((Integer) dicomTags[i].getValue(false)).intValue(), endianess);
                }
            } else if (type.equals("typeSequence")) {

                // VM - I don't think applies
                FileDicomSQ sq = (FileDicomSQ) dicomTags[i].getValue(false);
                writeSequence(outputFile, fileInfo.vr_type, sq, endianess);
            }
        }

        writeShort((short) 0x7FE0, endianess); // the image
        writeShort((short) 0x10, endianess);

        if (fileInfo.vr_type == FileInfoDicom.EXPLICIT) {

            // write VR and two reserved bytes
            outputFile.writeBytes(((FileDicomTag) fileInfo.getEntry("7FE0,0010")).getVR());
            outputFile.writeShort(0);
        }

        int samplesPerPixel = 1;

        if (fileInfo.getValue("0028,0002") != null) {
            samplesPerPixel = ((Short) fileInfo.getValue("0028,0002")).shortValue();
        }

        int imageLength = image.getSliceSize() * ((Short) fileInfo.getValue("0028,0100")).shortValue() / 8 * // bits per pixel
                              samplesPerPixel; // samples per pixel (i.e RGB = 3)

        if (fileInfo.isMultiFrame()) {
            int nImages = 1;

            if (fileInfo.getValue("0028,0008") != null) {
                nImages = Integer.valueOf(((String) (fileInfo.getValue("0028,0008"))).trim()).intValue();
            }

            writeInt(imageLength * nImages, endianess);
        } else {
            writeInt(imageLength, endianess);
        }
    }

    /**
     * Writes out a sequence tag and its data. The routine writes the Sequence as undefined-length and each of the items
     * as undefined length. The appropriate write-methods are used to output the various tags to the file.
     *
     * @param   outputFile  File to write to.
     * @param   vr_type     VR type, explicit or implicit
     * @param   sq          Sequence to write out
     * @param   endianess   Big is <code>true</code> or <code>false</code> for little endian byte-order.
     *
     * @throws  IOException  if write fails
     */
    private void writeSequence(RandomAccessFile outputFile, boolean vr_type, FileDicomSQ sq, boolean endianess)
            throws IOException {
        FileDicomItem item;

        if ((sq == null) || (sq.getSequenceLength() < 1)) {
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

            FileDicomTag[] dataSet = item.sortDataSet();


            for (int j = 0; j < dataSet.length; j++) {

                // System.err.println(" Counter = " + j);
                String type = "";
                FileDicomTag entry = dataSet[j];

                String vr = entry.getVR();
                int length = entry.getLength();

                // System.out.println("adadadfad length = " + length + " group = " + entry.getGroup() + " element = " +
                // entry.getElement());
                try {

                    if ((vr_type == FileInfoDicom.EXPLICIT) && (vr != null)) {
                        type = FileDicomTag.getType(vr);
                    } else {
                        type = entry.getType();
                    }
                } catch (NullPointerException error) {
                    type = "typeUnknown";
                }

                writeShort((short) entry.getGroup(), endianess);
                writeShort((short) entry.getElement(), endianess);

                if ((vr_type == FileInfoDicom.EXPLICIT) && (vr != null)) {
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
                } else if (((vr_type == FileInfoDicom.EXPLICIT) && (vr != null) &&
                                (vr.equals("SQ") || vr.equals("OB") || vr.equals("OW") || vr.equals("UN")))) {

                    if ((length == 0) && vr.equals("SQ")) {
                        // Do nothing because we only write ____Dave need help___ sequence tags
                    } else {
                        writeInt(length, endianess);
                    }
                } else if ((vr_type == FileInfoDicom.IMPLICIT) && !type.equals("typeUnknown") &&
                               !type.equals("typeSequence")) {
                    writeInt(length, endianess);
                }

                if (type.equals("typeString") || type.equals("otherWordString")) {
                    outputFile.writeBytes(entry.getValue(false).toString());

                    if ((entry.getValue(false).toString().length() % 2) != 0) {
                        outputFile.writeBytes("\0");
                    }
                } else if (type.equals("typeUnknown")) {

                    // Unknowns are stored as an array of Bytes.
                    // VM does not apply?
                    Byte[] bytesV = null;
                    bytesV = (Byte[]) entry.getValue(false);

                    byte[] bytesValue = new byte[bytesV.length];

                    for (int k = 0; k < bytesV.length; k++) {
                        bytesValue[k] = bytesV[k].byteValue();
                        // System.err.print(" [" + bytesV[k].toString()+"]");
                    }

                    writeInt(bytesV.length, endianess);
                    outputFile.write(bytesValue);
                } else if (type.equals("otherByteString")) {
                    Byte[] bytesV = null;
                    bytesV = (Byte[]) entry.getValue(false);

                    byte[] bytesValue = new byte[bytesV.length];

                    for (int k = 0; k < bytesV.length; k++) {
                        bytesValue[k] = bytesV[k].byteValue();
                        // System.err.print(" [" + bytesV[k].toString()+"]");
                    }

                    // writeInt(bytesV.length, endianess);
                    outputFile.write(bytesValue);
                } else if (type.equals("typeFloat")) {
                    writeFloat(((Float) entry.getValue(false)).floatValue(), endianess);
                } else if (type.equals("typeDouble")) {
                    writeDouble(((Double) entry.getValue(false)).doubleValue(), endianess);
                } else if (type.equals("typeShort")) {
                    writeShort(((Short) entry.getValue(false)).shortValue(), endianess);
                } else if (type.equals("typeInt")) {
                    writeInt(((Integer) entry.getValue(false)).intValue(), endianess);
                } else if (type.equals("typeSequence")) {
                    FileDicomSQ sq2 = (FileDicomSQ) entry.getValue(false);
                    writeInt(0xFFFFFFFF, endianess);
                    writeSequence(outputFile, vr_type, sq2, endianess);
                }
            }

            // write end-item tag:
            writeShort((short) 0xFFFE, endianess);
            writeShort((short) 0xE00D, endianess);
            writeInt((int) 0, endianess);
        }

        writeShort((short) 0xFFFE, endianess);
        writeShort((short) 0xE0DD, endianess);
        writeInt((int) 0, endianess);
        // System.err.println("3333pointer = " + outputFile.getFilePointer());
    }
}
