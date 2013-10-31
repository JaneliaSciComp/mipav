package gov.nih.mipav.model.file;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.dicomcomm.DICOM_Constants;
import gov.nih.mipav.model.file.FileDicomTagInfo.NumType;
import gov.nih.mipav.model.file.FileDicomTagInfo.StringType;
import gov.nih.mipav.model.file.FileDicomTagInfo.VR;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoDicom.VRtype;
import gov.nih.mipav.model.file.rawjp2.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogDicomDir;

import java.awt.image.*;
import java.io.*;
import java.nio.channels.FileChannel;
import java.util.*;
import java.util.Map.Entry;

import javax.imageio.IIOException;
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
    public static final String IMAGE_TAG = "7F[0-9A-F][0-9A-F],0010";

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

    /** The tag table for the base FileInfoDicom. */
    private FileDicomTagTable tagTable;

    /** Holds sequence of files described in DICOMDIR. */
    private FileDicomSQ dirInfo;

    /** Buffer used when reading in encapsulated JPEG images. */
    private int[] jpegData = null;

    /** JPEG compression may be lossy or lossless. */
    private boolean lossy = false;

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

    /** Value Representation - see FileDicomTagInfo. */
    private byte[] vrBytes = new byte[2];

    /** Whether MIPAV should be written to this dicom file as the secondary stamp * */
    private boolean stampSecondary = true;

    private boolean isEnhanced = false;

    private FileDicomTagTable[] enhancedTagTables;

    private boolean isEnhanced4D = false;

    private int enhancedNumSlices;
    
    private int enhancedNumVolumes;

    private Byte[] bytesV;

    /**The currently known extents of the image, as indicated by the header */
    private int[] extents = new int[2];

    /** Stores the endianess of the header, used for processing sequence tags */
    private boolean endianess;

    /**Whether adequate processing of the file has occurred to allowed image to be extracted, this includes getting offset
     * and pixel representation. */
    private boolean imageLoadReady = false;
    
    /** Header loop keeps executing when true */
    private boolean flag = true;
    
    /** Private tag for current publisher name */
    private PrivateFileDicomKey privatePublisher = null;

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
        vrBytes = null;

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
    	endianess = FileBase.LITTLE_ENDIAN; // all DICOM files start as little endian (tags 0002)
        flag = true;
        int exceptionCount = 0;
        int maxExceptionCount = 10;

        if (loadTagBuffer == true) {
            loadTagBuffer();
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
                    Preferences.debug("endianess = " + endianess + "\n", Preferences.DEBUG_FILEIO);
                }
            } else {
                if (getFilePointer() >= metaGroupLength) {
                    endianess = fileInfo.getEndianess();
                }
            }
            FileDicomKey key = null;
            int tagElementLength = 0;
            try {
                key = getNextTag(endianess);
                
                //System.out.println(key);
                tagElementLength = elementLength;
            } catch(ArrayIndexOutOfBoundsException aie) {
                aie.printStackTrace();
                Preferences.debug("Reached end of file while attempting to read: "+getFilePointer()+"\n", Preferences.DEBUG_FILEIO);
                key = new FileDicomKey("7FE0,0010"); //process image tag
                vrBytes = new byte[]{'O','W'};
                int imageLoc = locateImageTag(0, numEmbeddedImages);
                seek(imageLoc);
            }
            int bPtrOld = getFilePointer();

            try {
            	
            	flag = processNextTag(tagTable, key, endianess, false);
                if (flag == false && imageLoadReady == false) {
                    Preferences.debug("Error parsing tag: "+key+"\n", Preferences.DEBUG_FILEIO); 
                    break;
                }
            } catch(Exception e) {
                e.printStackTrace();
                Preferences.debug("Error parsing tag: "+key+"\n", Preferences.DEBUG_FILEIO);
                exceptionCount++;
                // Prevent infinite looping
                if (exceptionCount >= maxExceptionCount) {
                    break;
                }
            }
            if(bPtrOld+tagElementLength != getFilePointer()) {
                Preferences.debug("bPtrOld = " + bPtrOld + "\n");
                Preferences.debug("tagElementLength = " + tagElementLength + "\n");
                Preferences.debug("getFilePointer() = " + getFilePointer() + "\n");
                Preferences.debug("Possible invalid tag length specified, processing and tag lengths do not agree.\n");
            }
            if(tagElementLength != -1 && bPtrOld+tagElementLength > getFilePointer()) {
                seek(bPtrOld+tagElementLength); //processing tag was likely not successful, report error but continue parsing
                Preferences.debug("Skipping tag due to file corruption (or image tag reached): "+key+"\n", Preferences.DEBUG_FILEIO);
            }
            
            if (getFilePointer() >= fLength || (elementLength == -1 && key.toString().matches(FileDicom.IMAGE_TAG))) { // for dicom files that contain no image information, the image tag will never be encountered
                int imageLoc = locateImageTag(0, numEmbeddedImages);
                if(!notDir) { // Done reading tags, if DICOMDIR then don't do anything else
                    flag = false;
                } else if(imageLoc != -1 && !imageLoadReady) {
                    seek(imageLoc);
                    flag = true; //image tag exists but has not been processed yet
                } else {
                    flag = false;
                }
            }

        }
        
        if (notDir) {
            hasHeaderBeenRead = true;

            if ( (loadTagBuffer == true) && (raFile != null)) {
                raFile.close();
            }

            return true;
        } else {
            final JDialogDicomDir dirBrowser = new JDialogDicomDir(null, fileHeader, this); //initializes dicomdir gui
            return true;
        }
    }

    private FileDicomKey getNextTag(boolean endianess) throws IOException {
        // ******* Gets the next element
        getNextElement(endianess); // gets group, element, length
        final String name = convertGroupElement(groupWord, elementWord);
        final FileDicomKey key = new FileDicomKey(name);
        return key;
    }
    
    private FileDicomTagInfo putPrivateTagInfo(FileDicomTagTable tagTable, FileDicomKey key, boolean implicit) {
    	if(!PrivateDicomDictionary.privateDictionaryProcessed) {
    		PrivateDicomDictionary.getPrivateDicomTagTable(true);
    	}
		if(privatePublisher == null || privatePublisher.getGroupNumber() != key.getGroupNumber()) {
			privatePublisher = new PrivateFileDicomKey(PrivateFileDicomKey.NO_PUBLISHER, key.getGroupNumber(), 0x0010);
		} else {
			if(privatePublisher.getPublisher().equals(PrivateFileDicomKey.NO_PUBLISHER)) {
				Object value = tagTable.getValue(new FileDicomKey(key.getGroupNumber(), 0x0010));
                if(value != null) {
                	privatePublisher.setPublisher(value.toString());
                }
			}
		}
		FileDicomKey origKey = key;
		key = new PrivateFileDicomKey(privatePublisher.getPublisher(), key.toString());
		FileDicomTagInfo tagInfo = PrivateDicomDictionary.getInfo((PrivateFileDicomKey)key);    
		FileDicomTagInfo tagInfoInstance = null;
		if((tagInfo != null) && (!origKey.toString().equalsIgnoreCase("2005,140B"))) {
		    // 2005, 140B must be specially excluded, otherwise it becomes an incorrect float instead of the correct string
			tagInfoInstance = new FileDicomTagInfo(key, tagInfo.getType(), tagInfo.getValueMultiplicity(), tagInfo.getKeyword(), tagInfo.getName());
		} else {
			tagInfoInstance = new FileDicomTagInfo(key, VR.SH, 1, "Private tag", "Private tag");
		}
		
    	tagTable.putPrivateTagValue(tagInfoInstance);
    	
    	return tagInfoInstance;
    }
    
    /**
     * 
     * @param tagTable The tag table where this key will be stored
     * @param key The key that is being processed
     * @param endianess 
     * @return
     * @throws IOException
     */
    private boolean processNextTag(FileDicomTagTable tagTable, FileDicomKey key, boolean endianess, boolean inSequence) throws IOException {
        String strValue = null;
        Object data = null;
        VR vr = null; // value representation of data
        String name = key.toString(); // string representing the tag
        int tagVM;
        
        Preferences.debug("name = " + name + " length = " +
         elementLength + "\n", Preferences.DEBUG_FILEIO);
        if ( (fileInfo.getVr_type() == VRtype.IMPLICIT) || (groupWord == 2)) {

            // implicit VR means VR is based on tag as defined in dictionary
        	FileDicomTagInfo tagInfo = null;
        	if(key.getGroupNumber() % 2 == 0) { //public tag
        		tagInfo = DicomDictionary.getInfo(key);
        	} else  { //private tag
        		tagInfo = putPrivateTagInfo(tagTable, key, true);
        	}
        	
        	if(tagInfo != null) {
        		vr = tagInfo.getType();
                tagVM = tagInfo.getValueMultiplicity();
        	} else {
        		tagVM = elementLength;
        	}

            // the tag was not found in the dictionaries
            if (vr == null) {
                if(Integer.parseInt(key.getElement(), 16) == 0) {
                    vr = VR.UL;
                    tagVM = elementLength / ((NumType)vr.getType()).getNumBytes();
                } else {
                    vr = VR.UN;
                    tagVM = 1;
                }
            }
        } else { // Explicit VR  
            try {
                vr = VR.valueOf(new String(vrBytes));
            } catch(Exception e) {
                Preferences.debug("Unrecognized vr: "+new String(vrBytes)+" for tag "+key, Preferences.DEBUG_FILEIO);
            } finally {
                if(key.toString().matches(FileDicom.IMAGE_TAG)) {
                    if(!key.getGroup().equals("7FDF")) { //defunct scanner companies use this as another private group sometimes
                        vr = VR.OB;
                    }
                } else if((vr == VR.UN || vr == VR.XX || vr == null) && DicomDictionary.containsTag(key)) {
                    vr = DicomDictionary.getType(key);
                } else if(vr == null) {
                    vr = VR.UN;
                    Preferences.debug("Unknown vr for tag "+key, Preferences.DEBUG_FILEIO);
                }
            }

            if ( !DicomDictionary.containsTag(key)) {
            	FileDicomTagInfo tagInfo = putPrivateTagInfo(tagTable, key, false);
            	tagInfo.setValueRepresentation(vr);
            	
            	tagVM = 1;
                if(vr.getType() instanceof FileDicomTagInfo.NumType) {
                    tagVM = elementLength / ((NumType)vr.getType()).getNumBytes();
                }
                tagInfo.valueMultiplicity = tagVM;

            } else {
                final FileDicomTagInfo info = DicomDictionary.getInfo(key);
                // this is required if DicomDictionary contains wild card characters
                info.setKey(key);
                tagTable.putPrivateTagValue(info);
                tagVM = info.getValueMultiplicity();
                tagTable.get(key).setValueRepresentation(vr);

            }
        }

        if ( (elementWord == 0) && (elementLength == 0)) { // End of file

            if ( !isQuiet()) {
                MipavUtil.displayError("Error:  Unexpected end of file: " + fileName + "  Unable to load image.");
            }

            throw new IOException("Error while reading header");
        }

        if ( (getFilePointer() & 1) != 0) { // The file location pointer is at an odd byte number
            Preferences.debug("PARSING ERROR LIKELY CAUSED READING TAG ON ODD BYTE, check image carefully\n", 
                                    Preferences.DEBUG_FILEIO);
        }

        try {
        	
            if (vr.getType().equals(StringType.STRING) || vr.getType().equals(StringType.DATE)) {
                strValue = getString(elementLength);

                tagTable.setValue(key, strValue, elementLength);
                //System.out.println("Working on tag: "+key);

                Preferences.debug(tagTable.get(name).getName() + "\t\t(" + name + ");\t" + vr + "; value = "
                        + strValue + "; element length = " + elementLength + "\n", Preferences.DEBUG_FILEIO);
            } 
            
            switch(vr) {
            case AT:
                int groupWord = getUnsignedShort(fileInfo.getEndianess());
                int elementWord = getUnsignedShort(fileInfo.getEndianess());
                FileDicomKey innerKey = new FileDicomKey(groupWord, elementWord);
                tagTable.setValue(key, innerKey, elementLength);
                break;
            case OW:
                if(name.equals("0028,1201") || name.equals("0028,1202") || name.equals("0028,1203")) {
                    return getColorPallete(tagTable, new FileDicomKey(name));  //for processing either red(1201), green(1202), or blue(1203)
                } 
            case OB:
                if(name.matches(FileDicom.IMAGE_TAG) && !inSequence) { //can be either OW or OB
                    return processImageData(extents, numEmbeddedImages, getFilePointer()+(fileInfo.getVr_type() == VRtype.IMPLICIT ? 4 : 0)); //finished reading image tags and all image data, get final image for display
                }
                data = getByte(tagVM, elementLength, endianess);
                tagTable.setValue(key, data, elementLength);
                break;
            case UN:
            case XX:
                if(elementLength != -1) {
                    processUnknownVR(tagTable, strValue, key, tagVM, strValue);
                    break;
                } //else is implicit sequence, so continue
            case SQ:
                processSequence(tagTable, key, name, endianess);
                if (flag == false) {
                    Preferences.debug("flag was set false in processSequence\n", Preferences.DEBUG_FILEIO);
                    return false;
                }
                break;
            }
            
            if(vr.getType() instanceof NumType) {
                switch(((NumType)vr.getType())) {
                case SHORT:
                    data = getShort(tagVM, elementLength, endianess);
                    break;
                case LONG:
                    data = getInteger(tagVM, elementLength, endianess);
                    break;
                case FLOAT:
                    data = getFloat(tagVM, elementLength, endianess);
                    break;
                case DOUBLE:
                    data = getDouble(tagVM, elementLength, endianess);
                    break;
                }
                tagTable.setValue(key, data, elementLength);

                Preferences.debug("\t(" + name + ");\t ("+vr.getType()+") value = " + data
                        + " element length = " + elementLength + "\n", Preferences.DEBUG_FILEIO);
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
        } else if (name.equals("0004,1220")) {
            Preferences.debug("DICOMDIR Found! \n", Preferences.DEBUG_FILEIO);
            notDir = false;
        } else if (name.equals("0002,0010")) {
            boolean supportedTransferSyntax = processTransferSyntax(strValue);
            if(supportedTransferSyntax) {
                Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue
                        + " VR type: "+fileInfo.getVr_type()+" Endianess: "+fileInfo.getEndianess() +" \n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue + " unknown!\n",
                        Preferences.DEBUG_FILEIO);

                if ( !isQuiet()) {
                    MipavUtil.displayError("MIPAV does not support transfer syntax:\n" + strValue);
                }
                return false;
            }
            
        } else if (name.equals("0028,0010") && !inSequence) { // Set the extents, used for reading the image in FileInfoDicom's processTags
            extents[1] = ((Short) data).intValue();
            // fileInfo.columns = extents[1];
        } else if (name.equals("0028,0011") && !inSequence) {
            extents[0] = ((Short) data).intValue();
            // fileInfo.rows = extents[0];
        } else if (!isEnhanced && name.equals("0002,0002")) {                           // need to determine if this is enhanced dicom
            if (strValue.trim().equals(DICOM_Constants.UID_EnhancedMRStorage)           // if it is, set up all the additional fileinfos needed and attach
                    || strValue.trim().equals(DICOM_Constants.UID_EnhancedCTStorage)    // the childTagTables to the main tagTable
                    || strValue.trim().equals(DICOM_Constants.UID_EnhancedXAStorage)) {
                isEnhanced = true;
            }
        } else if (/*isEnhanced &&*/ name.equals("0028,0008")) {
        	// if we find 0028,0008 we assume that the image is multi-frame even if 0002,0002 was a non-enhanced UID.
        	// This was an issue for OCT multi-frame data (since not all OCT UID data is multi-frame).
            final int nImages = Integer.valueOf(strValue.trim()).intValue();
            fileInfo.setIsEnhancedDicom(true);
            if (nImages > 1) {
                enhancedTagTables = new FileDicomTagTable[nImages - 1];
            }
        } 
        
        return true;
    }

    private boolean processTransferSyntax(String strValue) {
     // Transfer Syntax UID: DICOM part 10 page 13, part 5 p. 42-48, Part 6 p. 53
        // 1.2.840.10008.1.2 Implicit VR Little Endian (Default)
        // 1.2.840.10008.1.2.1 Explicit VR Little Endian
        // 1.2.840.10008.1.2.2 Explicit VR Big Endian
        // 1.2.840.10008.1.2.4.50 8-bit Lossy JPEG (JPEG Coding Process 1)
        // 1.2.840.10008.1.2.4.51 12-bit Lossy JPEG (JPEG Coding Process 4)
        // 1.2.840.10008.1.2.4.57 Lossless JPEG Non-hierarchical (JPEG Coding Process 14)
        // we should bounce out if we don't support this transfer syntax
        if (strValue.trim().equals("1.2.840.10008.1.2")) {
            fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
            fileInfo.setVr_type(VRtype.IMPLICIT);
            encapsulated = false;
        } else if (strValue.trim().equals("1.2.840.10008.1.2.1")) {
            Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue
                    + " Explicit VR - Little Endian \n", Preferences.DEBUG_FILEIO);

            fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
            fileInfo.setVr_type(VRtype.EXPLICIT);
            encapsulated = false;
        } else if (strValue.trim().equals("1.2.840.10008.1.2.2")) {
            Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue
                    + " Explicit VR - Big Endian \n", Preferences.DEBUG_FILEIO);

            fileInfo.setEndianess(FileBase.BIG_ENDIAN);
            fileInfo.setVr_type(VRtype.EXPLICIT);
            encapsulated = false;
        } else if (strValue.trim().startsWith("1.2.840.10008.1.2.4.")) { // JPEG
            Preferences.debug("File Dicom: readHeader - Transfer Syntax = " + strValue
                    + " Implicit VR - Little Endian \n", Preferences.DEBUG_FILEIO);

            fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
            fileInfo.setVr_type(VRtype.EXPLICIT);
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
           return false; // unable to process tags without recognized transfer syntax
        }
        return true;
    }

    private void processSequence(FileDicomTagTable tagTable, FileDicomKey key, String name, boolean endianess) throws IOException {
        final int len = elementLength;

        // save these values because they'll change as the sequence is read in below.
        Preferences
                .debug("Sequence Tags: (" + name + "); length = " + len + "\n", Preferences.DEBUG_FILEIO);

        FileDicomSQ sq;

        // ENHANCED DICOM per frame
        if (name.equals("5200,9230")) {
            isEnhanced = true;
        	int numSlices = 0;
            sq = getSequence(endianess, len);
            final Vector<FileDicomSQItem> v = sq.getSequence();
            Iterator<FileDicomTag> itr = v.get(0).getTagList().values().iterator();
            TreeSet<Integer> sliceInt = new TreeSet<Integer>(); //keeps track of which slices have already been seen
            while(itr.hasNext()) { //put tags in base FileInfoDicom
                tagTable.put(itr.next());
            }
            numSlices = checkMaxSlice(tagTable, numSlices, sliceInt);
            for(int i=1; i<v.size(); i++) { //each entire children tag table is just what's in v
                if (enhancedTagTables == null) {
                    Preferences.debug("In processSequence enhancedTagTables == null\n", Preferences.DEBUG_FILEIO);
                    flag = false;
                    return;
                }
                if (enhancedTagTables.length >= i) {
                    enhancedTagTables[i-1] = v.get(i);
                }
                else {
                    Preferences.debug("In processSequence enhancedTagTables[" + (i-1) + "] is null\n", Preferences.DEBUG_FILEIO);
                    flag = false;
                    return;
                }
                numSlices = checkMaxSlice(enhancedTagTables[i-1], numSlices, sliceInt);
            }
            enhancedNumSlices = numSlices;
            
            // remove tag 5200,9230 if its there
            final FileDicomTag removeTag = tagTable.get(key);
            if (removeTag != null) {
                tagTable.removeTag(key);
            }
        } else {
            if (name.equals("0004,1220")) {
                dirInfo = getSequence(endianess, len);
                sq = new FileDicomSQ();
                sq.setWriteAsUnknownLength(len == -1);
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

        // fileInfo.setLength(name, len);
        Preferences.debug("Finished sequence tags.\n\n", Preferences.DEBUG_FILEIO);
    }

    /**
     * Helper method for enhanced dicom which finds the maximum slice number in the dataset.
     * Also determines whether the dataset is a 4D enhanced dataset.
     * @param tagTable2 
     */
    private int checkMaxSlice(FileDicomTagTable tagTable, int numSlices, TreeSet<Integer> sliceInt) {
        FileDicomTag frameTag = tagTable.get("0020,9111");
        int currNum = 0;
        if(frameTag != null) {
            // OK...so we should be relying on 0020,9056 (Stack ID)
            // to tell us the number of volumes in the dataet
            // but we find that it is not being implemented in the dataset we have
            // Look at 0020.9057 (In-Stack Pos ID), to get numSlices since
            // these should all be unique for 1 volume. If we find that there
            // are duplicates, then that means we are dealing with a 4D datset
            // we will get the number of slices in a volumne. Then determine
            // number of volumes by taking total num slices / num slices per volume
            // ftp://medical.nema.org/medical/dicom/final/cp583_ft.pdf
            currNum = ((Number)((FileDicomSQ)frameTag.getValue(false)).getItem(0).get("0020,9057").getValue(false)).intValue();
        }
        if (!isEnhanced4D) {
            isEnhanced4D = !sliceInt.add(currNum); //if slice already existed, sliceInt returns false, sets isEnhanced4D to true
        }
        if (currNum > numSlices) {
            numSlices = currNum;
            //System.out.println("Found slice "+numSlices);
        }
        return numSlices;
    }

    private void processUnknownVR(FileDicomTagTable tagTable, String name, FileDicomKey key, int tagVM, String strValue) throws IOException {
        try {
            // set the value if the tag is in the dictionary (which means it isn't private..) or has already
            // been put into the tag table without a value (private tag with explicit vr)
            if (DicomDictionary.containsTag(key) || tagTable.containsTag(key)) {
                tagTable.setValue(key, readUnknownData(), elementLength);
            } else {
                tagTable
                        .putPrivateTagValue(new FileDicomTagInfo(key, VR.UN, tagVM, "PrivateTag", "Private Tag"));

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

    /**
     * Processes image data from the defined image tag location.  When the image tag's location is unknown, use locateImageTag to find it
     */
    private boolean processImageData(int[] extents2, int imageNumber, int imageTagLoc) throws IOException {
    	if(imageTagLoc != locateImageTag(0, numEmbeddedImages)) {
    		Preferences.debug("If not embedded image, image loading location may be incorrect at "+imageTagLoc, Preferences.DEBUG_FILEIO);
    	}
    	
    	fileInfo.setInfoFromTags();
        
        final int imageLength = extents[0] * extents[1] * fileInfo.bitsAllocated / 8;

        Preferences.debug("File Dicom: readHeader - Data tag = " + FileDicom.IMAGE_TAG + "\n",
                Preferences.DEBUG_FILEIO);
        Preferences.debug("File Dicom: readHeader - imageLength = " + imageLength + "\n",
                Preferences.DEBUG_FILEIO);
        Preferences.debug("File Dicom: readHeader - getFilePointer = " + getFilePointer() + "\n",
                Preferences.DEBUG_FILEIO);

        if (fileInfo.getModality() == FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY) {
            fileInfo.displayType = ModelStorageBase.FLOAT;
        } else if(fileInfo.displayType == -1){ //if displayType has not been set
            fileInfo.displayType = fileInfo.getDataType();
        }
        
        if ( !encapsulated) {
            if (fileInfo.getVr_type() == VRtype.IMPLICIT) {
                Preferences.debug("Implicit image tag loading from "+imageTagLoc+"\n", Preferences.DEBUG_FILEIO);
                fileInfo.setOffset(imageTagLoc-4 > 0 ? imageTagLoc-4 : imageTagLoc); // no image length, subtract 4
            }
            // for explicit tags - see Part 5 page 27 1998
            else {
                Preferences.debug("Explicit image tag loading from "+imageTagLoc+"\n", Preferences.DEBUG_FILEIO);
                fileInfo.setOffset(imageTagLoc);
            } 
        } else { // encapsulated
            Preferences.debug("Encapsulated image tag loading from "+imageTagLoc+"\n", Preferences.DEBUG_FILEIO);
            fileInfo.setOffset(imageTagLoc-12 > 0 ? imageTagLoc-12 : imageTagLoc);
        }
        if(extents[0] == 0 || extents[1] == 0) {
            extents = guessImageLength(extents);
        }
        
        seek(fileInfo.getOffset());

        fileInfo.setExtents(extents);
        
        imageLoadReady = true;
        return !imageLoadReady;
		
	}

	/**
     * Helper method for dicom files that do not specify a valid extents
     */
    private int[] guessImageLength(int[] extents) throws IOException {
        int possImageLength = (int) ((raFile.length() - fileInfo.getOffset()) * (fileInfo.bytesPerPixel));
        if(possImageLength % ((int)Math.sqrt(possImageLength)) == 0) { //most likely for squares unless enhanced dicom and no extents have been found
            extents[0] = (int)Math.sqrt(possImageLength);
            extents[1] = extents[0];
        } else {
            ArrayList<Integer> factor = new ArrayList<Integer>();
   sqSearch:for(int i=possImageLength-1; i>1; i--) {
                if(possImageLength % i == 0) {
                    factor.add(i);
                    if(possImageLength/i == i) { //located square 3D sequence
                        extents[0] = i;
                        extents[1] = i;
                        break sqSearch;
                    }
                }
            }
            if(extents[0] == 0 || extents[1] == 0) { //no square factors found, so just use middle divisors
                if(factor.size() > 1) {
                    int middle = factor.size()/2;
                    extents[0] = factor.get(middle-1);
                    extents[1] = factor.get(middle);
                } else { //no factors found, so just use image length and 1
                    extents[0] = possImageLength;
                    extents[1] = 1;
                }
            }
        }
        return extents;
    }

    public int getEnhancedNumVolumes() {
		return enhancedNumVolumes;
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

        if (fileInfo.getUnitsOfMeasure(0) != Unit.CENTIMETERS.getLegacyNum()) {
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
        }

        if (fileInfo.getUnitsOfMeasure(1) != Unit.CENTIMETERS.getLegacyNum()) {
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
        }

        fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);

        // Needed for correct display of the image
        // set to null if there is no pixel pad value
        fileInfo.setPixelPadValue(fileInfo.pixelPaddingValue);

        if ( !encapsulated) {

            try { // rafile (type RandomAccessFile) for header, rawfile (type FileRaw) for image data.
                rawFile.setImageFile(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

                if (ModelImage.isColorImage(imageType)) {
                    rawFile.setPlanarConfig(fileInfo.planarConfig);
                    rawFile.readImage(buffer, (long)fileInfo.getOffset() + (imageNo * buffer.length), imageType); // *****
                    // Read
                    // image
                    rawFile.raFile.close();
                    rawFile.raFile = null;
                } else {
                    rawFile.readImage(buffer,
                            (long)fileInfo.getOffset() + (imageNo * buffer.length * fileInfo.bytesPerPixel), imageType); // *****
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
        //System.out.println("in read image short");
        // Read in header info, if something goes wrong, print out error
        if (hasHeaderBeenRead == false) {

            if ( !readHeader(true)) {
                throw (new IOException("DICOM header file error"));
            }
        }

        if (fileInfo.getUnitsOfMeasure(0) != Unit.CENTIMETERS.getLegacyNum()) {
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
        }

        if (fileInfo.getUnitsOfMeasure(1) != Unit.CENTIMETERS.getLegacyNum()) {
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
        }

        fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);

        // Needed for correct display of the image
        // set to null if there is no pixel pad value
        fileInfo.setPixelPadValue(fileInfo.pixelPaddingValue);

        if ( !encapsulated) {

            try { // rafile (type RandomAccessFile) for header, rawfile (type FileRaw) for image data.
                rawFile.setImageFile(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

                if (imageType == ModelStorageBase.ARGB) {
                    rawFile.setPlanarConfig(fileInfo.planarConfig);
                    rawFile.readImage(buffer, (long)fileInfo.getOffset() + (imageNo * (buffer.length / 4 * 3)), imageType);
                    rawFile.raFile.close();
                    rawFile.raFile = null;
                }
                else if (imageType == ModelStorageBase.ARGB_USHORT) {
                        rawFile.setPlanarConfig(fileInfo.planarConfig);
                        rawFile.readImage(buffer, (long)fileInfo.getOffset() + (imageNo * (buffer.length / 4 * 6)), imageType);
                        rawFile.raFile.close();
                        rawFile.raFile = null;
                } else {
                    rawFile.readImage(buffer,
                            (long)fileInfo.getOffset() + (imageNo * buffer.length * fileInfo.bytesPerPixel), imageType);
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
                    //System.out.println("calling encapsulatedJP2ImageData");
                    jpegData = encapsulatedJP2ImageData(imageType);

                } else {
                    //System.out.println("Calling encapsulatedImageData");
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

        //System.out.println("min = " + min + " max = " + max);
      
        if ((fileInfo.getPixelPadValue() != null) && ((pixelPad <= min) || (pixelPad >= max))) {

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
        
        if (fileInfo.getDataType() == ModelStorageBase.USHORT) {
            short minVal = 32767;
            short maxVal = -32768;
            for (int i = 0; i < buffer.length; i++) {
                if (buffer[i] < minVal) {
                    minVal = buffer[i];
                }
                if (buffer[i] > maxVal) {
                    maxVal = buffer[i];
                }
            }
            if (minVal < 0) {
                fileInfo.setDataType(ModelStorageBase.SHORT);
            }
        }
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

        if (fileInfo.getUnitsOfMeasure(0) != Unit.CENTIMETERS.getLegacyNum()) {
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
        }

        if (fileInfo.getUnitsOfMeasure(1) != Unit.CENTIMETERS.getLegacyNum()) {
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
        }

        fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);

        // Needed for correct display of the image
        // set to null if there is no pixel pad value
        fileInfo.setPixelPadValue(fileInfo.pixelPaddingValue);

        if ( !encapsulated) {

            try { // rafile (type RandomAccessFile) for header, rawfile (type FileRaw) for image data.
                rawFile.setImageFile(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ); 
                rawFile.readImage(buffer,
                        (long)fileInfo.getOffset() + (imageNo * buffer.length * fileInfo.bytesPerPixel), imageType);
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
                    //System.out.println("calling encapsulatedJP2ImageData");
                    jpegData = encapsulatedJP2ImageData(imageType);

                } else {
                    //System.out.println("Calling encapsulatedImageData");
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

    public FileDicomTagTable[] getEnhancedTagTables() {
        return enhancedTagTables;
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
        int[] dataRGB_USHORT = null;

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
            } else if (fileInfo.getDataType() == ModelStorageBase.ARGB_USHORT) {
                dataRGB_USHORT = new int[4 * (end - start)];
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
                        
                        
                    case ModelStorageBase.ARGB_USHORT:

                        int[] dRGB_USHORT = new int[3 * length];
                        image.exportData(index * (4 * length), (4 * length), dataRGB_USHORT);

                        for (int i = 1, iRGB = 0; i < dataRGB_USHORT.length;) {

                            // Not sure slope intercept is needed here for color images
                            dRGB_USHORT[iRGB++] = MipavMath.round( (dataRGB_USHORT[i++] - intercept) / invSlope);
                            dRGB_USHORT[iRGB++] = MipavMath.round( (dataRGB_USHORT[i++] - intercept) / invSlope);
                            dRGB_USHORT[iRGB++] = MipavMath.round( (dataRGB_USHORT[i++] - intercept) / invSlope);
                            i++;
                        }

                        rawChunkFile.writeBufferUShort(dRGB_USHORT, 0, 3 * length, image.getFileInfo(0).getEndianess());
                        dRGB_USHORT = null;
                        break;

                    default:

                        // Mod this to output to debug.
                        System.out.println("Unsupported data type: " + fileInfo.getDataType());

                        // left system.out, but added debug; dp, 2003-5-7:
                        Preferences.debug("Unsupported data type: " + fileInfo.getDataType() + "\n",
                        		Preferences.DEBUG_FILEIO);
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
     * @param startSlice Start image index (begin slice).
     * @param endSlice End image index (end slice).
     * @param endTime end time index
     * @param startTime beginning time index
     * 
     * @exception IOException if there is an error writing the file.
     * 
     * @see FileRawChunk
     */
    public void writeMultiFrameImage(final ModelImage image, final int startSlice, final int endSlice, int startTime, int endTime) throws IOException {

        fileInfo = (FileInfoDicom) image.getFileInfo(0);

        final String nFramesStr = String.valueOf((endSlice - startSlice + 1)*(endTime - startTime + 1));
        fileInfo.getTagTable().setValue("0028,0008", nFramesStr, nFramesStr.length());
        int dataType = image.getType();
        int bitsPerPixel;
        if (dataType == ModelStorageBase.ARGB) {
            bitsPerPixel = 8; // writing a ARGB color image
        }
        else {
            bitsPerPixel = 16; // writing a signed or unsigned short or ARGB_USHORT image
        }
        String bitsPerPixelString = String.valueOf(bitsPerPixel);
        fileInfo.getTagTable().setValue("0028,0100", bitsPerPixelString, bitsPerPixelString.length());

        if (stampSecondary) {
            // store that this DICOM has been saved/modified since original capture:
            stampSecondaryCapture(fileInfo);
        }
        this.image = image;
        
        final int imageSize = image.getSliceSize();
        int volumeSize = 0;
        if(image.getExtents().length > 2) {
            volumeSize = imageSize*image.getExtents()[2];
        }
        
        FileRawChunk rawChunkFile;
        rawChunkFile = new FileRawChunk(raFile, fileInfo);
        
        try {
            writeHeader(raFile, fileInfo, false);

            final double invSlope = fileInfo.getRescaleSlope();
            final double intercept = fileInfo.getRescaleIntercept();
            if (dataType == ModelStorageBase.ARGB) {
                final float[] data = new float[4 *imageSize];
                final int[] data2 = new int[4 * imageSize];
                
                for(int timeNum = startTime; timeNum <= endTime; timeNum++) {
                    for (int sliceNum = startSlice; sliceNum <= endSlice; sliceNum++) {
                        image.exportData(4*timeNum*volumeSize + 4*sliceNum*imageSize, 4*imageSize, data);
            
                        for (int i = 0; i < data.length; i++) {
                            data2[i] = MipavMath.round( (data[i] - intercept) / invSlope);
                        }
                        
                        rawChunkFile.writeBufferRGB(data2, timeNum*volumeSize + sliceNum*imageSize,
                                timeNum*volumeSize + sliceNum*imageSize + imageSize);
            
                    }
                }        
            } // if (dataType == ModelStorageBase.ARGB)
            else if (dataType == ModelStorageBase.ARGB_USHORT) {
                final float[] data = new float[4 *imageSize];
                final int[] data2 = new int[4 * imageSize];
                
                for(int timeNum = startTime; timeNum <= endTime; timeNum++) {
                    for (int sliceNum = startSlice; sliceNum <= endSlice; sliceNum++) {
                        image.exportData(4*timeNum*volumeSize + 4*sliceNum*imageSize, 4*imageSize, data);
            
                        for (int i = 0; i < data.length; i++) {
                            data2[i] = MipavMath.round( (data[i] - intercept) / invSlope);
                        }
                        
                        rawChunkFile.writeBufferRGB_USHORT(data2, timeNum*volumeSize + sliceNum*imageSize,
                                timeNum*volumeSize + sliceNum*imageSize + imageSize,
                                fileInfo.getEndianess());
            
                    }
                }            
            } // else if (dataType == ModelStorageBase.ARGB_USHORT)
            else if (dataType == ModelStorageBase.USHORT) {
                final float[] data = new float[imageSize];
                final int[] data2 = new int[imageSize];
                
                for(int timeNum = startTime; timeNum <= endTime; timeNum++) {
                    for (int sliceNum = startSlice; sliceNum <= endSlice; sliceNum++) {
                        image.exportData(timeNum*volumeSize + sliceNum*imageSize, imageSize, data);
            
                        for (int i = 0; i < data.length; i++) {
                            data2[i] = MipavMath.round( (data[i] - intercept) / invSlope);
                        }
                        
                        rawChunkFile.writeBufferUShort(data2, timeNum*volumeSize + sliceNum*imageSize,
                                timeNum*volumeSize + sliceNum*imageSize + imageSize,
                                fileInfo.getEndianess());
            
                    }
                }    
            } // if (dataType == ModelStorageBase.USHORT)
            else {
                final float[] data = new float[imageSize];
                final short[] data2 = new short[imageSize];
    
                for(int timeNum = startTime; timeNum <= endTime; timeNum++) {
                    for (int sliceNum = startSlice; sliceNum <= endSlice; sliceNum++) {
                        image.exportData(timeNum*volumeSize + sliceNum*imageSize, imageSize, data);
            
                        for (int i = 0; i < data.length; i++) {
                            data2[i] = (short) MipavMath.round( (data[i] - intercept) / invSlope);
                        }
                        
                        rawChunkFile.writeBufferShort(data2, timeNum*volumeSize + sliceNum*imageSize,
                                timeNum*volumeSize + sliceNum*imageSize + imageSize,
                                fileInfo.getEndianess());
            
                    }
                }
            } // else

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

        if ( !name.matches(FileDicom.IMAGE_TAG)) {

            if ( !isQuiet()) {
                MipavUtil.displayError("FileDicom: Image Data tag not found.  Cannot extract encapsulated image data.");
            }

            throw new IOException("Image Data tag not found.  Cannot extract encapsulated image data.");
        }
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
        

        if ( !name.matches(FileDicom.IMAGE_TAG)) {

            if ( !isQuiet()) {
                MipavUtil.displayError("FileDicom: Image Data tag not found.  Cannot extract encapsulated image data.");
            }

            throw new IOException("Image Data tag not found.  Cannot extract encapsulated image data.");
        }
        
        final Vector<byte[]> v = new Vector<byte[]>(); 
        
        int bytesPerPixel = FileInfoBase.getNumOfBytesPerPixel(fileInfo.getDataType());
        int sliceSize = fileInfo.getExtents()[0]*fileInfo.getExtents()[1];
        if (elementLength >= sliceSize*bytesPerPixel) {
            // No compression 
            final int[] imageData = new int[sliceSize];    
            if (fileInfo.getDataType() == ModelStorageBase.USHORT) {
                for (int i = 0; i < sliceSize; i++) {
                    imageData[i] = getUnsignedShort(endianess);
                }
            }
            return imageData;
        }
        
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
            
            if (lossy) {
                for (final byte[] name2 : v) {
                    jpegImage = extractLossyJPEGImage(name2);
                    v2.addElement(jpegImage);
                }    
            } // if (lossy)
            else { // not lossy
                final FileDicomJPEG fileReaderMulti = new FileDicomJPEG(v, v2, fileInfo.getExtents()[0], fileInfo
                        .getExtents()[1]);
                fileReaderMulti.extractMultiJPEGImage();
            } // else not lossy

           
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
            return imageData;

        } else {

            if (lossy == true) {
                return extractLossyJPEGImage(v.elementAt(0));
            } else {
                final FileDicomJPEG fileReader = new FileDicomJPEG(v.elementAt(0), fileInfo.getExtents()[0], fileInfo
                        .getExtents()[1]);
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
        BufferedImage img;
        try {
        	img = ImageIO.read(stream);
        } catch (IIOException e) {
        	e.printStackTrace();
        	img = null;
        }
        if (img == null) {
        	// JPEG2000 only has signed BYTE, SHORT and INTEGER.
        	// getFixedPoint(0) and getNomRangeBits(0) are used to calculate
        	// an offset value added to every point in the buffer.
            if (fileInfo.getDataType() == ModelStorageBase.USHORT) {
            	fileInfo.setDataType(ModelStorageBase.SHORT);
            	fileInfo.displayType = ModelStorageBase.SHORT;
            }
        	try {
        	    FileJP2 fileJP2 = new FileJP2();
        	    return fileJP2.decodeImageData(imageFrag, ModelStorageBase.ARGB);
        	}
        	catch (Exception e) {
        		try {
        		    FileJP2 fileJP2 = new FileJP2();
        		    // routine only depends on whether or not imageType is ModelStorageBase.ARGB
        		    return fileJP2.decodeImageData(imageFrag, ModelStorageBase.BYTE);
        		}
        		catch (Exception e2) {
        			MipavUtil.displayError("In extractLossyJPEGImage ImageIO.read(stream) no registered ImageReader claims"
                            + " to be able to read the stream");
                    Preferences.debug("In extractLossyJPEGImage ImageIO.read(stream) no registered ImageReader claims\n", 
                    		Preferences.DEBUG_FILEIO);
                    Preferences.debug("to be able the read the stream\n",
                    		Preferences.DEBUG_FILEIO);
                    throw new IOException();	
        		}
        	}
            
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
            Preferences.debug("Interrupted waiting for pixels!" + "\n", Preferences.DEBUG_FILEIO);

            return new int[0];
        }

        if ( (pg.getStatus() & ImageObserver.ABORT) != 0) {
            Preferences.debug("Image fetch aborted or errored" + "\n", Preferences.DEBUG_FILEIO);

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
     * @throws IOException A problem occurs when reading the image file.
     * 
     * @see "DICOM PS 3.3, Information Object Definitions"
     */
    private boolean getColorPallete(final FileDicomTagTable tagTable, final FileDicomKey palleteKey) throws IllegalArgumentException, IOException {

        try {

            // get channel lut specification.
            final String specKey = palleteKey.getGroup() + ","
                    + Integer.toString(Integer.parseInt(palleteKey.getElement()) - 100);
            final FileDicomTag palletSpec = tagTable.get(specKey);

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

            tagTable.setValue(palleteKey.getKey(), data, numberOfLUTValues);
            return true;
        } catch (final NullPointerException npe) {
            final FileDicomTag t = fileInfo.getTagTable().get(palleteKey);
            MipavUtil.displayError("This image carries " + t.getName() + ", but specifies it incorrectly.\n"
                    + "The channel will be ignored.");
            Preferences.debug("FileDicom: Error Creating " + t.getName() + "(" + palleteKey.getKey() + ")"
                    + "; it will be ignored.\n", Preferences.DEBUG_FILEIO);
        } catch (final ArrayStoreException ase) {
            final FileDicomTag t = fileInfo.getTagTable().get(palleteKey);
            MipavUtil.displayError(t.getName() + "; " + ase.getLocalizedMessage() + "\n"
                    + "This channel will be ignored.");
            Preferences.debug("FileDicom: Error creating " + t.getName() + "; " + ase.getLocalizedMessage() + "\n", 
            		Preferences.DEBUG_FILEIO);
        } catch (final ArrayIndexOutOfBoundsException aioobe) {
            MipavUtil.displayError("Error reading color LUT channel.  This Channel will be ignored.");
            Preferences.debug("Error reading color LUT channel.  This Channel will be ignored.\n", Preferences.DEBUG_FILEIO);
            // System.err.println("Error reading color LUT channel. This Channel will be ignored.");
        }
        return false;
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
    private FileDicomSQItem getDataSet(int itemLength, final boolean endianess) throws IOException {
        final FileDicomSQItem table = new FileDicomSQItem(null, fileInfo.getVr_type());
        table.setWriteAsUnknownLength(itemLength == -1); //if reported item length is -1, item will continue to be written with unknown length
        
        final int startfptr = getFilePointer();
        boolean dataSetflag = true; //whether dicom header processing should continue
        while (dataSetflag && !nameSQ.equals(FileDicom.SEQ_ITEM_END) && (getFilePointer() - startfptr < itemLength || itemLength == -1)) {
            Preferences.debug("Processed seq amount: "+(getFilePointer() - startfptr), Preferences.DEBUG_FILEIO);
            FileDicomKey key = getNextTag(endianess);
            nameSQ = key.toString();
            if(!nameSQ.equals(FileDicom.SEQ_ITEM_END) && !nameSQ.matches(FileDicom.IMAGE_TAG)) {
                dataSetflag = processNextTag(table, key, endianess, true);
            } else if(nameSQ.matches(FileDicom.IMAGE_TAG)) {
                numEmbeddedImages++;
                seek(getFilePointer()+elementLength); //embedded image not displayed //TODO: make this image availbale in the dicom infobox
            }
        }
        
        return table;
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

        if ( (fileInfo.getVr_type() == VRtype.IMPLICIT) || (groupWord == 2)) {

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
            vrBytes[0] = b1;
            vrBytes[1] = b2;
            fileInfo.isCurrentTagSQ = new String(vrBytes).equals("SQ");

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
            vrBytes[0] = b1; // these are not VR for item tags!!!!!!!!!
            vrBytes[1] = b2;

            fileInfo.isCurrentTagSQ = new String(vrBytes).equals("SQ");

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
        // 0x10)+" - " , Preferences.DEBUG_FILEIO); System.err.print("( just found: ) "+ Integer.toString(groupWord, 0x10) +
        // ":"+Integer.toString(elementWord, 0x10)+ " - ");

        if (fileInfo.getVr_type() == VRtype.EXPLICIT) {

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
                // Preferences.debug(" length " + Integer.toString(elementLength, 0x10) + "\n", Preferences.DEBUG_FILEIO);
            }
        } else {

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
    private FileDicomSQ getSequence(final boolean endianess, final int seqLength) throws IOException {
        final FileDicomSQ sq = new FileDicomSQ();
        sq.setWriteAsUnknownLength(seqLength == -1);

        // There is no more of the tag to read if the length of the tag
        // is zero. In fact, trying to get the Next element is potentially
        // bad, so we'll just shut down the reading here.
        if (seqLength == 0) {
            return sq;
        }
        
        // hold on to where the sequence is before items for measuring
        // distance from beginning of sequence
        final int seqStart = getFilePointer();

        getNextElement(endianess); // gets the first ITEM tag
        Preferences.debug("Item: " + Integer.toString(groupWord, 0x10) + "," + Integer.toString(elementWord, 0x10)
                + " for " + Integer.toString(elementLength, 0x10) + " # readfrom: "
                + Long.toString(getFilePointer(), 0x10) + "\n", Preferences.DEBUG_FILEIO);

        nameSQ = convertGroupElement(groupWord, elementWord);
        
        while(!nameSQ.equals(FileDicom.SEQ_ITEM_UNDEF_END)) {
            if (nameSQ.equals(FileDicom.SEQ_ITEM_BEGIN)) {
    
                // elementLength here is the length of the
                // item as it written into the File
                FileDicomSQItem item = null;
                if (elementLength == 0) {
                    item = new FileDicomSQItem(null, fileInfo.getVr_type());
                } else {
                    item = getDataSet(elementLength, endianess);
                }
                item.setWriteAsUnknownLength(elementLength == -1); //if reported item length is -1, item will continue to be written with unknown length
                if(item != null) {
                    sq.addItem(item);
                }
            }
            
            //if defined sequence length, will not read next tag once length has been reached
            if(seqLength == -1 || seqStart + seqLength > getFilePointer()) {  
                getNextElement(endianess); // gets the first ITEM tag
                Preferences.debug("Item: " + Integer.toString(groupWord, 0x10) + "," + Integer.toString(elementWord, 0x10)
                        + " for " + Integer.toString(elementLength, 0x10) + " # readfrom: "
                        + Long.toString(getFilePointer(), 0x10) + "\n", Preferences.DEBUG_FILEIO);
    
                nameSQ = convertGroupElement(groupWord, elementWord);
            } else {
                return sq;
            }
        }
        
        return sq;
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
        Preferences.debug("Unknown data; length is " + elementLength + " fp = " + getFilePointer() + "\n",
        		Preferences.DEBUG_FILEIO);

        if (elementLength < 0) {
            Preferences.debug("Unknown data; Error length is " + elementLength + "\n", Preferences.DEBUG_FILEIO);

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
     * Writes a dicom tag to the provided output file.  This method uses the transfer syntax of the file to 
     * determine the format of the tag.
     */
    private void writeNextTag(FileDicomTag tag, RandomAccessFile outputFile) throws IOException {
        Preferences.debug("Processing tag "+tag.getKey()+" with VR "+tag.getValueRepresentation()+"\n", Preferences.DEBUG_FILEIO);
    	
    	VR vr = VR.UN;
    	
        try {

            if (fileInfo.getVr_type() == VRtype.EXPLICIT) {
                vr = tag.getValueRepresentation();// explicit VRs may be difference from implicit; in this case use the explicit
            } else {
                vr = tag.getType();
            }
        } catch (final NullPointerException e) {
            vr = VR.UN;
        }

        final int gr = tag.getGroup();
        final int el = tag.getElement();

        // Returns the current length of the value of the dicom tag
        int length = 0;
        if(vr.equals(VR.SQ)) {
        	length = ((FileDicomSQ) tag.getValue(false)).getWritableLength();
        } else {
        	length = tag.getDataLength();
        }
        if(length != -1 && length%2 != 0) {
        	length++; //an odd length tag is appended
        }

        final int nValues = tag.getNumberOfValues();

        final int vm = tag.getValueMultiplicity();

        if ( (gr == 2) && (el == 0)) {

            // length of the transfer syntax group; after this group is written, need to change endianess
            metaGroupLength = ((Integer) tag.getValue(false)).intValue();
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
        
        //System.out.println("Location of tag write: "+raFile.getFilePointer());

        writeShort((short) gr, endianess); // write group
        writeShort((short) el, endianess); // write element

        if ( ( (fileInfo.getVr_type() == VRtype.EXPLICIT) || (gr == 2)) && (vr != null)) {
            outputFile.writeBytes(vr.toString()); // write explicit vr

            switch(vr) {
            case SQ:    
                Preferences.debug("SQ Length: "+ length, Preferences.DEBUG_FILEIO);
            case OB:
            case OW:
            case OF:
            case UT:
            case UN:
                writeShort((short)0, endianess); // skip two reserved bytes
                if (length == -1) {
                    writeInt(0xFFFFFFFF, endianess); // write undefined length
                } else {
                    writeInt(length, endianess); // write length as int
                }
                break;
            default:
                writeShort((short) length, endianess); // write length as a short
            }
        } else { // IMPLICIT VR

            try {

                if (vr.equals(VR.SQ)) {
                    Preferences.debug("SQ Length: "+ length, Preferences.DEBUG_FILEIO);
                    
                    if (length == -1) {
                        writeInt(0xFFFFFFFF, endianess); // write undefined length
                    } else {
                        writeInt(length, endianess);
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
        
        long currentFilePointer = raFile.getFilePointer();
        Object obj = tag.getValue(false);
        
        switch(vr) {
        case US:
        case SS:
        	if ( (vm > 1) || (nValues > 1)) {   
	            if(obj instanceof Short[]) {
	                final Short[] data = (Short[]) obj;
	
	                for (int vmI = 0; vmI < nValues; vmI++) {
	                    writeShort(data[vmI].shortValue(), endianess);
	                }
	            } 
            } else {
                writeShort( ((Short) obj).shortValue(), endianess);
            }
        	break;
        	
        case SL:
        case UL:
        	if ( (vm > 1) || (nValues > 1)) {
                if(obj instanceof Integer[]) {
                    final Integer[] data = (Integer[]) obj;

                    for (int vmI = 0; vmI < nValues; vmI++) {
                        writeInt(data[vmI].intValue(), endianess);
                    }
                    
                } 
            } else {
                writeInt( ((Integer) obj).intValue(), endianess);
            }
        	break;
        	
        case FL:
        	if ( (vm > 1) || (nValues > 1)) {
                final Float[] data = (Float[]) obj;

                for (int vmI = 0; vmI < nValues; vmI++) {
                    writeFloat(data[vmI].floatValue(), endianess);
                }
            } else {
                writeFloat( ((Float) obj).floatValue(), endianess);
            }
        	break;
        	
        case FD:
        	if ( (vm > 1) || (nValues > 1)) {
                final Double[] data = (Double[]) obj;

                for (int vmI = 0; vmI < nValues; vmI++) {
                    writeDouble(data[vmI].doubleValue(), endianess);
                }
            } else {
                writeDouble( ((Double) obj).doubleValue(), endianess);
            }
        	break;
        	
        case SQ:
        	final FileDicomSQ sq = (FileDicomSQ) tag.getValue(false);
            writeSequence(outputFile, fileInfo.getVr_type(), sq, endianess);
            if(length == -1) {
                // write end-sequence tag for undefined length sequences
                writeShort((short) 0xFFFE, endianess);
                writeShort((short) 0xE0DD, endianess);
                writeInt(0, endianess);
            }
        	break;
        
        case AT:
            writeShort((short) ((FileDicomKey)obj).getGroupNumber(), endianess);
            writeShort((short) ((FileDicomKey)obj).getElementNumber(), endianess);
            break;
        	
        default:
        	byte appendByte = 0x20;
        	
        	if(obj instanceof Short[]) { //guaranteed to be even # of bytes
        	    for(int k=0; k<((Short[])obj).length; k++) {
        	        writeUnsignedShort(((Short[])obj)[k].shortValue(), endianess);
        	    }
        	} if(obj instanceof Byte[]) {
        		bytesV = (Byte[]) obj;
        		
                final byte[] bytesValue = new byte[bytesV.length+(bytesV.length%2)];

                for (int k = 0; k < bytesV.length; k++) {
                    bytesValue[k] = bytesV[k].byteValue();
                }
                
                for(int k=bytesV.length; k<length; k++) {
                    bytesValue[k] = appendByte;
                }

                outputFile.write(bytesValue);
        	} else if(obj instanceof String) {
        	    if(length >= ((String) obj).length()) {
        	        byte[] appendAr = new byte[length - ((String) obj).length()];
            	    for(int k=0; k<appendAr.length; k++) {
            	        appendAr[k] = appendByte;
            	    }
            	    obj = obj+new String(appendAr);
        	    } else {
        	        Preferences.debug("Truncating string dicom data", Preferences.DEBUG_FILEIO);
        	        obj = ((String) obj).substring(0, length);
        	    }

        		outputFile.writeBytes(obj.toString());
        	}
        }
        
        if(raFile.getFilePointer() == currentFilePointer) {
        	Preferences.debug("No data was written for tag "+tag.getKey().toString(), Preferences.DEBUG_FILEIO);
        } else {
        	Preferences.debug("Wrote value for key "+tag.getKey().toString()+" of length "+length+" with value "+obj.toString(), Preferences.DEBUG_FILEIO);
        }
    }
    
    /**
     * Writes selected tag values (location specified by editKeys, value specified by editTags), to existing dicom file
     * specified by outputFile.
     */
    public void writeTags(final RandomAccessFile outputFile, final FileInfoDicom fileInfo, 
    						final FileDicomKey[] editKeys, final FileDicomTag[] editTags) throws IOException {
    	
    	if(editKeys.length != editTags.length) {
    		System.err.println("Tag and key edit locations do not match");
    	}
    	
    	Arrays.sort(editKeys);
    	
    	Arrays.sort(editTags);
    	
    	for(int i=0; i<editKeys.length; i++) {
    		if(!editKeys[i].equals(editTags[i].getKey())) {
    			System.err.println("Tag and key edit locations do not match for edit key "+editKeys[i]+" at index "+i);
    		}
    	}
    	
    	raFile = outputFile;
    	System.out.println(raFile.getFilePointer()+" vs "+raFile.length());
    	
    	endianess = FileBase.LITTLE_ENDIAN; // all DICOM files start as little endian (tags 0002)
        flag = true;
        int exceptionCount = 0;
        int maxExceptionCount = 10;

        if (true) {
            loadTagBuffer();
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

        int editIndex = 0;
        FileDicomTag newTag = null;        
        
        while (flag == true) {
            if (fileInfo.containsDICM) {
                // endianess is defined in a tag and set here, after the transfer
                // syntax group has been read in
                if (getFilePointer() >= (ID_OFFSET + 4 + metaGroupLength)) {
                    endianess = fileInfo.getEndianess();
                    Preferences.debug("endianess = " + endianess + "\n", Preferences.DEBUG_FILEIO);
                }
            } else {
                if (getFilePointer() >= metaGroupLength) {
                    endianess = fileInfo.getEndianess();
                }
            }
            FileDicomKey key = null;
            int oldTagElementLength = 0;
            int bufferLoc = -1;
            try {
                key = getNextTag(endianess);
                
                if(editIndex < editKeys.length && key.equals(editKeys[editIndex])) {
                	bufferLoc = getFilePointer();
                	
                	newTag = editTags[editIndex];
                	
                	//System.out.println(key);
                    oldTagElementLength = elementLength;
                    //System.out.println(raFile.getFilePointer()+" vs "+raFile.length());
                    
                    replaceTag(raFile, newTag.getKey(), newTag, oldTagElementLength);
                	editIndex++;
                	if(editIndex == editKeys.length) {
                		flag = false;
                	}
                	//newTag = null;
                }
                
                
            } catch(ArrayIndexOutOfBoundsException aie) {
                aie.printStackTrace();
                Preferences.debug("Reached end of file while attempting to read: "+getFilePointer()+"\n", Preferences.DEBUG_FILEIO);
                key = new FileDicomKey("7FE0,0010"); //process image tag
                vrBytes = new byte[]{'O','W'};
                int imageLoc = locateImageTag(0, numEmbeddedImages);
                seek(imageLoc);
            }
            if(bufferLoc != -1) {
            	seek(bufferLoc + newTag.getLength() - oldTagElementLength);
            	//System.err.println("Continuing processing: "+getFilePointer());
            }
            
            int bPtrOld = getFilePointer();
            

            try {
            	
            	flag = processNextTag(tagTable, key, endianess, false);
                if (flag == false && imageLoadReady == false) {
                    Preferences.debug("Error parsing tag: "+key+"\n", Preferences.DEBUG_FILEIO); 
                    break;
                }
            } catch(Exception e) {
                e.printStackTrace();
                Preferences.debug("Error parsing tag: "+key+"\n", Preferences.DEBUG_FILEIO);
                exceptionCount++;
                // Prevent infinite looping
                if (exceptionCount >= maxExceptionCount) {
                    break;
                }
            }
            if(bPtrOld+oldTagElementLength != getFilePointer()) {
                Preferences.debug("Possible invalid tag length specified, processing and tag lengths do not agree.");
            }
            if(oldTagElementLength != -1 && bPtrOld+oldTagElementLength > getFilePointer()) {
                seek(bPtrOld+oldTagElementLength); //processing tag was likely not successful, report error but continue parsing
                Preferences.debug("Skipping tag due to file corruption (or image tag reached): "+key+"\n", Preferences.DEBUG_FILEIO);
            }
            
            if (getFilePointer() >= fLength || (elementLength == -1 && key.toString().matches(FileDicom.IMAGE_TAG))) { // for dicom files that contain no image information, the image tag will never be encountered
                int imageLoc = locateImageTag(0, numEmbeddedImages);
                if(!notDir) { // Done reading tags, if DICOMDIR then don't do anything else
                    flag = false;
                } else if(imageLoc != -1 && !imageLoadReady) {
                    seek(imageLoc);
                    flag = true; //image tag exists but has not been processed yet
                } else {
                    flag = false;
                }
            }

        }
        
        if (notDir) {
            hasHeaderBeenRead = true;

            if ( (true) && (raFile != null)) {
                raFile.close();
            }

            return;
        } else {
            final JDialogDicomDir dirBrowser = new JDialogDicomDir(null, fileHeader, this); //initializes dicomdir gui
            return;
        }
    }
    
    /**
     * newTag is written to raFile to replace old tag of tagElementLength
     */
    private void replaceTag(RandomAccessFile raFile, FileDicomKey key, FileDicomTag newTag, int oldTagElementLength) throws IOException {
		long raFilePointer = raFile.getFilePointer();
    	long pointerLoc = getFilePointer(); //points to location in tag buffer
    	raFile.seek(pointerLoc);
		//System.out.println("Location: "+pointerLoc);
		
		int length = 0;
        if(newTag.getValueRepresentation().equals(VR.SQ)) {
        	length = ((FileDicomSQ) newTag.getValue(false)).getWritableLength();
        } else {
        	length = newTag.getDataLength();
        }
        if(length != -1 && length%2 != 0) {
        	length++; //an odd length tag is appended
        }
        
        long sizeChange = length - oldTagElementLength;
        if(sizeChange != 0) {
        	changeDicomFileSize(raFile, sizeChange, pointerLoc, oldTagElementLength);
        }
        
        
        
		int offset = -6; //shorten by tag and length descriptors
		if(fileInfo.getVr_type() == VRtype.EXPLICIT) {
			offset = offset - 2;
		}
		
		raFile.seek(pointerLoc+offset);  //gets raFile to current tag

		writeNextTag(newTag, raFile);
		
		raFile.seek(0); 
		
		loadTagBuffer();
		
		//raFile.seek(raFilePointer); //gets raFile back to beginning of image tag
	}

	/**
	 * 
	 * @param raFile
	 * @param sizeChange - change in tag length
	 * @param pointerLoc - beginning of tag value
	 * @param oldTagElementLength - old tag length
	 * @throws IOException
	 */
    private void changeDicomFileSize(RandomAccessFile raFile, long sizeChange, long pointerLoc, int oldTagElementLength) throws IOException {
		System.out.println(getFileInfo().getFileDirectory()+getFileInfo().getFileName());
		RandomAccessFile rTemp = new RandomAccessFile(fileHeader.getAbsolutePath()+"~", "rw");
		long fileSize = raFile.length();
		FileChannel sourceChannel = raFile.getChannel();
		FileChannel targetChannel = rTemp.getChannel();
		sourceChannel.transferTo(pointerLoc+oldTagElementLength, fileSize - pointerLoc - oldTagElementLength, targetChannel);
		sourceChannel.truncate(fileSize + sizeChange);
		fileSize = fileSize + sizeChange;
		raFile.seek(pointerLoc);

		if(sizeChange > 0) {
			raFile.seek(pointerLoc+oldTagElementLength);
			byte[] content = new byte[(int)sizeChange];
			for(int i=0; i<content.length; i++) {
				content[i] = 0;
			}
			
			raFile.write(content);
		} 
		targetChannel.position(0L);
		sourceChannel.transferFrom(targetChannel, pointerLoc+oldTagElementLength+sizeChange, fileSize+sizeChange - pointerLoc);

		targetChannel.close();
		rTemp.close();
		new File(fileHeader.getAbsolutePath()+"~").delete();
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
    public void writeHeader(final RandomAccessFile outputFile, final FileInfoDicom fileInfo,
            final boolean saveAsEncapJP2) throws IOException {
        FileDicomTag element;
        FileDicomTag[] tagArray = FileDicomTagTable.sortTagsList(fileInfo.getTagTable().getTagList());

        if (fileInfo.containsDICM) {

            // write "DICM" tag
            final byte[] skip = new byte[ID_OFFSET];

            for (int k = 0; k < skip.length; k++) {
                skip[k] = (byte) 0;
            }

            outputFile.write(skip);
            outputFile.writeBytes("DICM");
        }
        
       for(int i=0; i<tagArray.length; i++) {
        	element = tagArray[i];
        	if(!element.getKey().toString().matches(FileDicom.IMAGE_TAG)) {
        		writeNextTag(element, outputFile);
        	} else {
        		Preferences.debug("Image tag reached", Preferences.DEBUG_FILEIO);
        		break; //image tag reached
        	}
        }

        writeShort((short) 0x7FE0, endianess); // the image
        writeShort((short) 0x0010, endianess);

        if (saveAsEncapJP2) {
            writeShort((short) 0x424F, endianess);
            // writeShort((short) 0x574F, endianess);
            writeShort((short) 0x0000, endianess);
            writeInt(0xFFFFFFFF, endianess);
            return;
        }

        if (fileInfo.getVr_type() == VRtype.EXPLICIT) {
            VR imageTagVR = DicomDictionary.getVR(new FileDicomKey("7FE0,0010"));

            // write VR and two reserved bytes
            outputFile.writeBytes(imageTagVR.toString());
            outputFile.writeShort(0);
        }

        int samplesPerPixel = 1;

        if (fileInfo.getTagTable().getValue("0028,0002") != null) {
            samplesPerPixel = ((Short) fileInfo.getTagTable().getTagList().get("0028,0002").getValue(false)).shortValue();
        }
        
        final int imageLength = image.getSliceSize()
                * ((Short) fileInfo.getTagTable().getValue("0028,0100", false)).shortValue() / 8 * // bits per pixel
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
    private void writeSequence(final RandomAccessFile outputFile, final VRtype vr_type, final FileDicomSQ sq,
            final boolean endianess) throws IOException {

        for (int i = 0; i < sq.getSequence().size(); i++) {
            FileDicomSQItem table = sq.getSequence().get(i);
            FileDicomTag[] tagArray = FileDicomTagTable.sortTagsList(table.getTagList());
            int dataLength = table.getWritableLength(true); //include tag header information for each tag
            // write item-start tag
            writeShort((short) 0xFFFE, endianess);
            writeShort((short) 0xE000, endianess);
            
            if (dataLength == -1) {
                writeInt(0xFFFFFFFF, endianess); // write undefined length
            } else {
                writeInt(dataLength, endianess);
            }; 

            for(int j=0; j<tagArray.length; j++) {
            	FileDicomTag tag = tagArray[j];
                writeNextTag(tag, outputFile);
            }

            if(dataLength == -1) {
                // write end-item tag:
                writeShort((short) 0xFFFE, endianess);
                writeShort((short) 0xE00D, endianess);
                writeInt(0, endianess);
            }
        }
    }

    public void doEnhanced(boolean doEnhanced, int[] extents) {
        this.isEnhanced = doEnhanced;
        if(extents.length > 3) {
            this.isEnhanced4D = doEnhanced;
            this.enhancedNumVolumes = extents[3];
        } else {
            this.isEnhanced4D = false;
            this.enhancedNumVolumes = 1;
        }
        if(extents.length > 2) {
            this.enhancedNumSlices = extents[2];
        }
    }
}
