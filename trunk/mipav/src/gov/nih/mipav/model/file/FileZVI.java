package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import java.io.*;
import gov.nih.mipav.view.*;

/**
 
 */

public class FileZVI extends FileBase {
    
    private static final short VT_EMPTY = 0;
    
    private static final short VT_BOOL = 11;
    
    private static final short VT_UI2 = 18;
    
    private static final short VT_I4 = 3;
    
    private static final short VT_R8 = 5;
    
    private static final short VT_BSTR = 8;
    
    private static final short VT_STORED_OBJECT = 69;
    
    private static final short VT_DATE = 7;
    
    private static final short VT_DISPATCH = 9;
    
    private static final short VT_UNKNOWN = 13;
   
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;


    /** DOCUMENT ME! */
    private FileInfoZVI fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int[] imageExtents = null;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgResols = new float[5];

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;
    
    boolean endianess;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * ZVI reader constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileZVI(String fileName, String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        fileName = null;
        fileDir = null;
        fileInfo = null;
        file = null;
        image = null;
        imageExtents = null;
        imgBuffer = null;
        imgResols = null;
        LUT = null;
        
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * Accessor that returns the file info.
     *
     * @return  FileInfoBase containing the file info
     */
    public FileInfoBase getFileInfo() {
        return fileInfo;
    }


    /**
     * Accessor that returns the image buffer.
     *
     * @return  buffer of image.
     */
    public float[] getImageBuffer() {
        return imgBuffer;
    }

    /**
     * Rreturns LUT if defined.
     *
     * @return  the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }
    
    /**
     * Reads the LIFF header which indicates endianess, the TIFF magic number, and the offset in bytes of the first IFD.
     * It then reads all the IFDs. This method then opens a Model of an image and imports the the images one slice at a
     * time. Image slices are separated by an IFD.
     *
     * @param      multiFile  <code>true</code> if a set of files each containing a separate 2D image is present <code>
     *                        false</code> if one file with either a 2D image or a stack of 2D images
     * @param      one        <code>true</code> if only want to read in one image of the 3D set
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean multiFile, boolean one) throws IOException {
        long fileLength;
        int i, j, k;
        long contentsStartSect = 0;
        long contentsStreamSize = 0;
        long bytesToRead;
        int dataType;
        long contentsStart;
        int startSect;
        long streamSize;
        long rootStart;
        
        try {
            
            imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            
            fileLength = raFile.length();
            
            
            
            fileInfo = new FileInfoZVI(fileName, fileDir, FileUtility.ZVI); // dummy fileInfo
            fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
            endianess = FileBase.LITTLE_ENDIAN;
            
            // Start reading ole compound file structure
            // The header is always 512 bytes long and always located at offset zero.
            // Offset 0 Length 8 bytes olecf file signature
            long olecfFileSignature = getLong(endianess);
            if (olecfFileSignature == 0xe11ab1a1e011cfd0L) {
                Preferences.debug("Found olecf file signature\n");
            }
            else {
                Preferences.debug("Instead of olecf file signature found = " + olecfFileSignature + "\n");
            }
            // Location 8 Length 16 bytes class id
            getLong(endianess);
            getLong(endianess);
            // Location 24 Length 2 bytes Minor version of the format: 33 is written by reference implementation
            int minorVersion = getUnsignedShort(endianess);
            Preferences.debug("Minor version of OLE format = " + minorVersion + "\n");
            // Location 26 Length 2 bytes Major version of the dll/format
            int majorVersion = getUnsignedShort(endianess);
            Preferences.debug("Major version of the OLE format = " + majorVersion + "\n");
            // Location 28 Length 2 bytes ByteOrder Should be 0xfffe for intel or little endian
            int byteOrder = getUnsignedShort(endianess);
            if (byteOrder == 0xfffe) {
                Preferences.debug("Byte order is the expected little endian\n");
            }
            else {
                Preferences.debug("Unexpected byte order value = " + byteOrder + "\n");
            }
            // Location 30 Length 2 bytes Sector size in power of 2 (9 indicates 512 byte sectors)
            int sectorSize = getUnsignedShort(endianess);
            sectorSize = (int)Math.round(Math.pow(2,sectorSize));
            Preferences.debug("The sector byte length = " + sectorSize + "\n");
            // Location 32 Length 2 bytes Mini-sector size in power of 2 (6 indicates 64 byte mini-sectors)
            int miniSectorSize = getUnsignedShort(endianess);
            miniSectorSize = (int)Math.round(Math.pow(2, miniSectorSize));
            Preferences.debug("The mini-sector byte length = " + miniSectorSize + "\n");
            // Location 34 Length 2 bytes reserved must be zero
            int reserved = getUnsignedShort(endianess);
            if (reserved == 0) {
                Preferences.debug("Reserved is the expected zero\n");
            }
            else {
                Preferences.debug("Reserved = " + reserved + " instead of the expected zero\n");
            }
            // Location 36 Length 4 bytes reserved1 must be zero
            long reserved1 = getUInt(endianess);
            if (reserved1 == 0) {
                Preferences.debug("Reserved1 is the expected zero\n");
            }
            else {
                Preferences.debug("Reserved1 = " + reserved1 + " instead of the expected zero\n");
            }
            // Location 40 Length 4 bytes reserved2 must be zero
            long reserved2 = getUInt(endianess);
            if (reserved2 == 0) {
                Preferences.debug("Reserved2 is the expected zero\n");
            }
            else {
                Preferences.debug("Reserved2 = " + reserved2 + " instead of the expected zero\n");
            }
            // Location 44 Length 4 bytes Number of sectors used for the sector allocation table
            long fatSectorNumber = getUInt(endianess);
            Preferences.debug("Number of sectors used for the sector allocation table = " + fatSectorNumber + "\n");
            // Location 48 Length 4 bytes First sector in the directory chain
            int directoryStartSector = readInt(endianess);
            if (directoryStartSector == -2) {
                Preferences.debug("First sector in the directory chain = END OF CHAIN\n");
            }
            else {
                Preferences.debug("First sector in the directory chain = " + directoryStartSector + "\n");
            }
            // Location 52 Length 4 bytes Signature used for transactioning: must be zero.  The
            // reference implementation does not support transactioning.
            long signature = getUInt(endianess);
            if (signature == 0) {
                Preferences.debug("The transactioning signature is the expected zero\n");
            }
            else {
                Preferences.debug("The transactioning signature = " + signature + " instead of the expected zero\n");
            }
            // Location 56 Length 4 bytes Maximum size for mini-streams
            // Streams with an actual size smaller than (and not equal to) this value are stored as mini-streams.
            long miniSectorCutoff = getUInt(endianess);
            Preferences.debug("The maximum byte size for mini-streams = " + miniSectorCutoff + "\n");
            // Location 60 Length 4 bytes First sector in the short sector allocation table
            int shortStartSector = readInt(endianess);
            if (shortStartSector == -2) {
                Preferences.debug("The first sector in the short sector allocation table = END OF CHAIN\n");    
            }
            else {
                Preferences.debug("The first sector in the short sector allocation table = " + shortStartSector + "\n");
            }
            // Location 64 Length 4 bytes Number of sectors in the short sector allocation table
            long shortSectors = getUInt(endianess);
            Preferences.debug("Number of sectors in the short sector allocation table = " + shortSectors + "\n");
            // Location 68 Length 4 bytes First sector of the master sector allocation table
            // End of chain if no additional sectors used.
            int difStartSector = readInt(endianess);
            if (difStartSector == -2) {
                Preferences.debug("First sector of the master sector allocation table = END OF CHAIN\n");
            }
            else {
                Preferences.debug("First sector of the master sector allocation table = " + difStartSector + "\n");
            }
            // Location 72 Length 4 bytes Number of sectors used for the master sector allocation table
            long difSectors = getUInt(endianess);
            Preferences.debug("The number of sectors used for the master sector allocation table = " + difSectors + "\n");
            // Location 76 Length 4*109 = 436 bytes First part of the master sector allocation table
            // containing 109 FAT secIDs.
            int fatSectors[] = new int[(int)Math.min(fatSectorNumber,109)];
            for (i = 0; i < Math.min(fatSectorNumber,109); i++) {
                fatSectors[i] = readInt(endianess);
                Preferences.debug("FAT Sector " + i + " = " + fatSectors[i] + "\n");
            }
            
            // Read short sector allocation table
            Preferences.debug("\nReading the short sector allocation table\n");
            long shortSectorTableAddress = (shortStartSector+1)*sectorSize;
            raFile.seek(shortSectorTableAddress);
            int shortSectorTable[] = new int[(int)shortSectors];
            for (i = 0; i < shortSectors; i++) {
                shortSectorTable[i] = readInt(endianess);
                Preferences.debug("shortSectorTable[" + i + "] = " + shortSectorTable[i] + "\n");
            }
            
            // Read the first sector of the directory chain (also referred to as the first element of the 
            // Directory array, or SID 0) is known as the Root Directory Entry
            Preferences.debug("\nReading the first sector of the directory chain\n");
            long directoryStart = (directoryStartSector+1)*sectorSize;
            raFile.seek(directoryStart+64);
            // Read the length of the element name in bytes.  Each Unicode character is 2 bytes
            int elementNameBytes = getUnsignedShort(endianess);
            Preferences.debug("The element name has " + (elementNameBytes/2) + " unicode characters\n"); 
            // Read the element name
            raFile.seek(directoryStart);
            byte[] b = new byte[elementNameBytes];
            raFile.readFully(b);
            String elementName = new String(b, "UTF-16LE").trim();
            // The element name is typically Root Entry in Unicode, although
            // some versions of structured storage (particularly the preliminary
            // reference implementation and the Macintosh version) store only
            // the first letter of this string "R".  This string is always
            // ignored, since the Root Directory Entry is known by its position
            // SID 0 rather than its name, and its name is not otherwise used.
            Preferences.debug("The element name is " + elementName + "\n");
            // Read the type of object
            raFile.seek(directoryStart + 66);
            byte objectType[] = new byte[1];
            raFile.read(objectType);
            if (objectType[0] == 5) {
                Preferences.debug("Object type is root as expected\n");
            }
            else if (objectType[0] == 0) {
                Preferences.debug("Object type is unexpectedly invalid\n");
            }
            else if (objectType[0] == 1) {
                Preferences.debug("Object type is unexpectedly storage\n");
            }
            else if (objectType[0] == 2) {
                Preferences.debug("Object type is unexpectedly stream\n");
            }
            else if (objectType[0] == 3) {
                Preferences.debug("Object type is unexpectedly lockbytes\n");
            }
            else if (objectType[0] == 4) {
                Preferences.debug("Object type is unexpectedly property\n");
            }
            else {
                Preferences.debug("Object type is an illegal " + objectType[0] + "\n");
            }
            // offset 67 color.  Since the root directory does not have siblings, it's
            // color is irrelevant and may therefore be either red or black.
            byte color[] = new byte[1];
            raFile.read(color);
            if (color[0] == 0) {
                Preferences.debug("Node is red\n");
            }
            else if (color[0] == 1) {
                Preferences.debug("Node is black\n");
            }
            else {
                Preferences.debug("Node has illegal color value = " + color[0] + "\n");
            }
            // offset 68 length 4 bytes SID of the left sibling of this entry in the directory tree
            int leftSID = readInt(endianess);
            if (leftSID == -1) {
                Preferences.debug("No left sibling for this entry\n");
            }
            else {
                Preferences.debug("The SID of the left sibling of this entry in the directory tree = " + leftSID + "\n");
            }
            // offset 72 length 4 bytes SID of the right sibling of this entry in the directory tree
            int rightSID = readInt(endianess);
            if (rightSID == -1) {
                Preferences.debug("No right sibling for this entry\n");
            }
            else {
                Preferences.debug("The SID of the right sibling of this entry in the directory tree = " + rightSID + "\n");
            }
            // offset 76 length 4 bytes SID of the child of this entry in the directory tree
            int childSID = readInt(endianess);
            if (childSID == -1) {
                Preferences.debug("No child for this entry\n");
            }
            else {
                Preferences.debug("The SID of the child of this entry in the directory tree = " + childSID + "\n");
            }
            // offset 80 length 16 bytes class id
            getLong(endianess);
            getLong(endianess);
            // offset 96 length 4 bytes userFlags not applicable for root object
            long userFlags = getUInt(endianess);
            Preferences.debug("User flags = " + userFlags + "\n");
            // offset 100 length 8 bytes creation time stamp
            long creationTimeStamp = getLong(endianess);
            if (creationTimeStamp == 0) {
                Preferences.debug("Creation time stamp not set\n");
            }
            else {
                Preferences.debug("Creation time stamp = " + creationTimeStamp + "\n");
            }
            // offset 108 length 8 bytes modification time stamp
            long modificationTimeStamp = getLong(endianess);
            if (creationTimeStamp == 0) {
                Preferences.debug("Modification time stamp not set\n");
            }
            else {
                Preferences.debug("Modification time stamp = " + modificationTimeStamp + "\n");
            }
            // offset 116 length 4 bytes starting sector of the stream
            int rootStartSect = readInt(endianess);
            // Offset 120 length 4 bytes Size of the stream in byes
            long rootStreamSize = getUInt(endianess);
            if (rootStreamSize < miniSectorCutoff) {
                Preferences.debug("Starting sector of the ministream = " + rootStartSect + "\n");
                Preferences.debug("Size of the ministream in bytes = " + rootStreamSize + "\n");
            }
            else {
                Preferences.debug("Starting sector of the stream = " + rootStartSect + "\n");
                Preferences.debug("Size of the stream in bytes = " + rootStreamSize + "\n");    
            }
            // Offset 124 length 2 bytes dptPropType Reserved for future use.  Must be zero
            int dptPropType = getUnsignedShort(endianess);
            if (dptPropType == 0) {
                Preferences.debug("dptPropType = 0 as expected\n");
            }
            else {
                Preferences.debug("dptProptType = " + dptPropType + " instead of the expected 0\n");
            }
            
            int directorySector = 2;
            while (directorySector < 33) {
            //while (childSID != directorySector) {
                Preferences.debug("\nReading element " + directorySector + " of the directory array\n");
                directorySector++;
                directoryStart = directoryStart + 128;
                raFile.seek(directoryStart+64);
                // Read the length of the element name in bytes.  Each Unicode character is 2 bytes
                elementNameBytes = getUnsignedShort(endianess);
                Preferences.debug("The element name has " + (elementNameBytes/2) + " unicode characters\n"); 
                // Read the element name
                raFile.seek(directoryStart);
                b = new byte[elementNameBytes];
                raFile.readFully(b);
                elementName = new String(b, "UTF-16LE").trim();
                Preferences.debug("The element name is " + elementName + "\n");
                // Read the type of object
                raFile.seek(directoryStart + 66);
                raFile.read(objectType);
                if (objectType[0] == 0) {
                    Preferences.debug("Object type is invalid\n");
                }
                else if (objectType[0] == 1) {
                    Preferences.debug("Object type is storage\n");
                }
                else if (objectType[0] == 2) {
                    Preferences.debug("Object type is stream\n");
                }
                else if (objectType[0] == 3) {
                    Preferences.debug("Object type is lockbytes\n");
                }
                else if (objectType[0] == 4) {
                    Preferences.debug("Object type is property\n");
                }
                else if (objectType[0] == 5) {
                    Preferences.debug("Object type is incorrectly root\n");
                }
                else {
                    Preferences.debug("Object type is an illegal " + objectType[0] + "\n");
                }
                // offset 67 color.  
                raFile.read(color);
                if (color[0] == 0) {
                    Preferences.debug("Node is red\n");
                }
                else if (color[0] == 1) {
                    Preferences.debug("Node is black\n");
                }
                else {
                    Preferences.debug("Node has illegal color value = " + color[0] + "\n");
                }
                // offset 68 length 4 bytes SID of the left sibling of this entry in the directory tree
                leftSID = readInt(endianess);
                if (leftSID == -1) {
                    Preferences.debug("No left sibling for this entry\n");
                }
                else {
                    Preferences.debug("The SID of the left sibling of this entry in the directory tree = " + leftSID + "\n");
                }
                // offset 72 length 4 bytes SID of the right sibling of this entry in the directory tree
                rightSID = readInt(endianess);
                if (rightSID == -1) {
                    Preferences.debug("No right sibling for this entry\n");
                }
                else {
                    Preferences.debug("The SID of the right sibling of this entry in the directory tree = " + rightSID + "\n");
                }
                // offset 76 length 4 bytes SID of the child of this entry in the directory tree
                childSID = readInt(endianess);
                if (childSID == -1) {
                    Preferences.debug("No child for this entry\n");
                }
                else {
                    Preferences.debug("The SID of the child of this entry in the directory tree = " + childSID + "\n");
                }
                // offset 80 length 16 bytes class id
                getLong(endianess);
                getLong(endianess);
                // offset 96 length 4 bytes userFlags
                userFlags = getUInt(endianess);
                Preferences.debug("User flags = " + userFlags + "\n");
                // offset 100 length 8 bytes creation time stamp
                creationTimeStamp = getLong(endianess);
                if (creationTimeStamp == 0) {
                    Preferences.debug("Creation time stamp not set\n");
                }
                else {
                    Preferences.debug("Creation time stamp = " + creationTimeStamp + "\n");
                }
                // offset 108 length 8 bytes modification time stamp
                modificationTimeStamp = getLong(endianess);
                if (creationTimeStamp == 0) {
                    Preferences.debug("Modification time stamp not set\n");
                }
                else {
                    Preferences.debug("Modification time stamp = " + modificationTimeStamp + "\n");
                }
                // offset 116 length 4 bytes starting sector of the stream
                startSect = readInt(endianess);
                // Offset 120 length 4 bytes Size of the stream in byes
                streamSize = getUInt(endianess);
                if (streamSize <= miniSectorCutoff) {
                    Preferences.debug("Starting sector of the ministream = " + startSect + "\n");
                    Preferences.debug("Size of the ministream in bytes = " + streamSize + "\n");
                }
                else {
                    Preferences.debug("Starting sector of the stream = " + startSect + "\n");
                    Preferences.debug("Size of the stream in bytes = " + streamSize + "\n");    
                }
                // Offset 124 length 2 bytes dptPropType Reserved for future use.  Must be zero
                dptPropType = getUnsignedShort(endianess);
                if (dptPropType == 0) {
                    Preferences.debug("dptPropType = 0 as expected\n");
                }
                else {
                    Preferences.debug("dptProptType = " + dptPropType + " instead of the expected 0\n");
                }
                
                if ((elementName.equals("Contents")) && (objectType[0] == 2) && (streamSize > 0)) {
                    contentsStartSect = startSect;
                    contentsStreamSize = streamSize;
                }
            } // while (childSID != -2)
            
            if (rootStreamSize > 0) {
                if (rootStreamSize < miniSectorCutoff) {
                    rootStart = rootStartSect*miniSectorSize;   
                }
                else {
                    rootStart = (rootStartSect+1)*sectorSize;
                }
                Preferences.debug("rootStart = " + rootStart + "\n");
                readBlock(rootStart, rootStreamSize);
            } // if (rootStreamSize > 0)
            
            if (contentsStreamSize > 0) {
                // Read the Contents stream

                if (contentsStreamSize < miniSectorCutoff) {
                    contentsStart = contentsStartSect*miniSectorSize;    
                }
                else {
                    contentsStart = (contentsStartSect+1)*sectorSize;    
                }
                Preferences.debug("contentsStart = " + contentsStart + "\n");
                readBlock(contentsStart, contentsStreamSize);
            } // if (contentsStreamSize > 0)
            //image.calcMinMax(); 
            fireProgressStateChanged(100);
            
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw error;
        }

        return image;
    }
    
    private void readBlock(long startLocation, long bytesToRead) throws IOException {
        short dataType;
        boolean booleanValue;
        short shortValue;
        int intValue;
        double doubleValue;
        int stringBytes; // In Unicode including terminating null
        byte[] b;
        String str;
        raFile.seek(startLocation);
        while (bytesToRead > 0) {
            dataType = readShort(endianess);
            bytesToRead -= 2;
            switch(dataType) {
                case VT_EMPTY:
                    Preferences.debug("Read VT_EMPTY\n");
                    break;
                case VT_BOOL:
                    // 16-bit integer true if != 0, false otherwise
                    shortValue = readShort(endianess);
                    bytesToRead -= 2;
                    if (shortValue != 0) {
                        booleanValue = true;
                    }
                    else {
                        booleanValue = false;
                    }
                    Preferences.debug("Read BOOLEAN = " + booleanValue + "\n");
                    break;
                case VT_UI2:
                    intValue = getUnsignedShort(endianess);
                    bytesToRead -=2;
                    Preferences.debug("Read USHORT with value = " + intValue + "\n");
                    break;
                case VT_I4:
                    intValue = readInt(endianess);
                    bytesToRead -= 4;
                    Preferences.debug("Read INT with value = " + intValue + "\n");
                    break;
                case VT_R8:
                    doubleValue = readDouble(endianess);
                    bytesToRead -= 8;
                    Preferences.debug("Read DOUBLE with value = " + doubleValue + "\n");
                    break;
                case VT_DATE:
                    doubleValue = readDouble(endianess);
                    bytesToRead -= 8;
                    Preferences.debug("Read VT_DATE with value = " + doubleValue + "\n");
                    break;
                case VT_BSTR:
                    stringBytes = readInt(endianess);
                    bytesToRead -= 4;
                    b = new byte[stringBytes];
                    raFile.readFully(b);
                    bytesToRead -= stringBytes;
                    str = new String(b, "UTF-16LE").trim();
                    Preferences.debug("Read String = " + str + "\n");
                    break;
                case VT_STORED_OBJECT:
                    stringBytes = getUnsignedShort(endianess);
                    bytesToRead -= 2;
                    b = new byte[stringBytes];
                    raFile.readFully(b);
                    bytesToRead -= stringBytes;
                    str = new String(b, "UTF-16LE").trim();
                    Preferences.debug("The name of sibling storage = " + str + "\n");
                    // If desired use OleLoad to instantiate the object from storage
                    // Can be ignored if the referenced object is of no importance
                    // because stored objects use independent storage elements
                    break;
                case VT_DISPATCH:
                    // This may impose problems because the objects must either be empty(CLSID_NULL)
                    // or creatable via OldLoadFromStream, otherwise the stream cursor cannot be
                    // positioned properly.
                    Preferences.debug("Read VT_DISPATCH\n");
                    break;
                case VT_UNKNOWN:
                    // This may impose problems because the objects must either be empty(CLSID_NULL)
                    // or creatable via OldLoadFromStream, otherwise the stream cursor cannot be
                    // positioned properly.
                    Preferences.debug("Read VT_UNKNOWN\n");
                    break;
                default:
                    Preferences.debug("Bytes left unread = " + bytesToRead + "\n");
                    bytesToRead = 0;
                    Preferences.debug("Unknown dataType = " + dataType + "\n");
            } // switch(dataType)
        } // while (bytesToRead > 0)
        
        
    } // readBlock

    
}
