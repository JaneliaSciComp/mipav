package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import java.io.*;
import java.util.*;
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
    
    /** Memory buffer size in bytes, for reading from disk. */
    private static final int BUFFER_SIZE = 8192;
    
    /** Block identifying start of useful header information. */
    private static final byte[] ZVI_MAGIC_BLOCK_1 = { // 41 00 10
      65, 0, 16
    };
    
    /** Block identifying second part of useful header information. */
    private static final byte[] ZVI_MAGIC_BLOCK_2 = { // 41 00 80
      65, 0, -128
    };

    /** Block identifying third part of useful header information. */
    private static final byte[] ZVI_MAGIC_BLOCK_3 = { // 20 00 10
      32, 0, 16
    };

    private static final int UNSIGNED8 = 1;
    
    private static final int UNSIGNED16 = 2;
    
    private static final int BGR = 3;
    
    private static final int RGB48 = 4;
   
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
    
    private boolean endianess;
    
    private int sectorSize;
    
    private int shortSectorSize;
    
    private int shortSectorTable[] = null;
    
    private int shortStreamStartSect;
    
    private long totalShortStreamSize;
    
    private Set C_Set = new HashSet(); // to hold C channel index collection
    
    private int zDim = 1;
    
    private int tDim = 1;
    
    private int channelNumber = 1;
    
    private int minC = Integer.MAX_VALUE;
    
    private int maxC = Integer.MIN_VALUE;

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
        int contentsStartSect = 0;
        long contentsStreamSize = 0;
        long bytesToRead;
        int dataType;
        long contentsStart;
        int startSect;
        long streamSize;
        long rootStart;
        byte[] readBuffer  = null;
        sliceInfo[] si;
        int t;
        int z;
        int c;
        byte byteBuffer[] = null;
        short shortBuffer[] = null;
        int index;
        int b1 = 0;
        int b2 = 0;
        int bufferSize;
        
        try {
            
            imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            
            fileLength = raFile.length();
            
            
            
            fileInfo = new FileInfoZVI(fileName, fileDir, FileUtility.ZVI); // dummy fileInfo
            fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
            endianess = FileBase.LITTLE_ENDIAN;
            
            si = readHeader();
            
            if ((zDim > 1) && (tDim > 1)) {
                imageExtents = new int[4];
                imageExtents[2] = zDim;
                imageExtents[3] = tDim;
            }
            else if (tDim > 1) {
                imageExtents = new int[3];
                imageExtents[2] = tDim;
            }
            else if (zDim > 1) {
                imageExtents = new int[3];
                imageExtents[2] = zDim;
            }
            else {
                imageExtents = new int[2];
            }
            imageExtents[0] = si[0].width;
            imageExtents[1] = si[0].height;
            for (i = 0; i < imageExtents.length; i++) {
                Preferences.debug("extents[" + i + "] = " + imageExtents[i] + "\n");
            }
            bufferSize = imageExtents[0] * imageExtents[1];
            
            channelNumber = maxC - minC + 1;
            Preferences.debug("Channel number = " + channelNumber + "\n");
            if (channelNumber > 1) {
                if (si[0].dataType == UNSIGNED8) {
                    si[0].dataType = BGR;    
                }
                else if (si[0].dataType == UNSIGNED16) {
                    si[0].dataType = RGB48;
                }
            }
            

            fileInfo.setExtents(imageExtents);
            
            switch (si[0].dataType) {
                case UNSIGNED8:
                    dataType = ModelStorageBase.UBYTE;
                    Preferences.debug("Data type = UNSIGNED BYTE\n");
                    break;
                case UNSIGNED16:
                    dataType = ModelStorageBase.USHORT;
                    Preferences.debug("Data type = UNSIGNED SHORT\n");
                    break;
                case BGR:
                    dataType = ModelStorageBase.ARGB;
                    Preferences.debug("Data type = ARGB\n");
                    break;
                case RGB48:
                    dataType = ModelStorageBase.ARGB_USHORT;
                    Preferences.debug("Data type = ARGB_USHORT\n");
                    byteBuffer = new byte[2 * imageExtents[0] * imageExtents[1]];
                    shortBuffer = new short[4 * imageExtents[0] * imageExtents[1]];
                    break;
                default:
                    dataType = ModelStorageBase.UBYTE;
            }
            fileInfo.setDataType(dataType);
            
            image = new ModelImage(dataType, imageExtents, fileName);
            
            for (t = 0; t < tDim; t++) {
                for (z = 0; z < zDim; z++) {
                    fireProgressStateChanged((t*zDim + z)*100/(tDim*zDim));
                    for (c = minC; c <= maxC; c++) {
                        for (i = 0; i < si.length; i++) {
                            if ((si[i].theC == c) && (si[i].theZ == z) && (si[i].theT == t)) {
                                raFile.seek(si[i].offset);
                                raFile.read(byteBuffer);
                                switch (si[0].dataType) {
                                    case UNSIGNED8:
                                        break;
                                    case UNSIGNED16:
                                        break;
                                    case BGR:
                                        break;
                                    case RGB48:
                                        for (j = 0, index = 0; j < bufferSize; j++) {
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            shortBuffer[4*j + (c - minC + 1)] = (short) ((b2 << 8) | b1);
                                        }
                                        break;
                                }
                                // break out of i loop
                                break;
                            }
                        } // for (i = 0; i < si.length; i++)
                    } // for (c = minC; c <= maxC; c++)
                    image.importData(4*(t*zDim*bufferSize + z*bufferSize), shortBuffer, false);
                } // for (z = 0; z < zDim; z++)
            } // for (t = 0; t < tDim; t++)
            
            if (image.getNDims() == 2) {
                image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
            } else if (image.getNDims() == 3) { // If there is more than one image

                for (i = 0; i < imageExtents[2]; i++) {
                    FileInfoZVI newFileInfo = (FileInfoZVI) fileInfo.clone();
                    image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
                }
            } else if (image.getNDims() == 4) { // If there is more than one image
                for (i = 0; i < (imageExtents[2] * imageExtents[3]); i++) {
                    FileInfoZVI newFileInfo = (FileInfoZVI) fileInfo.clone();
                    image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
                }
            }
            
            /*
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
            sectorSize = getUnsignedShort(endianess);
            sectorSize = (int)Math.round(Math.pow(2,sectorSize));
            Preferences.debug("The sector byte length = " + sectorSize + "\n");
            // Location 32 Length 2 bytes Mini-sector size in power of 2 (6 indicates 64 byte mini-sectors)
            shortSectorSize = getUnsignedShort(endianess);
            shortSectorSize = (int)Math.round(Math.pow(2, shortSectorSize));
            Preferences.debug("The mini-sector byte length = " + shortSectorSize + "\n");
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
            if (shortSectors > 0) {
                Preferences.debug("\nReading the short sector allocation table\n");
                long shortSectorTableAddress = (shortStartSector+1)*sectorSize;
                int lastShortSector = shortStartSector;
                raFile.seek(shortSectorTableAddress);
                shortSectorTable = new int[(int)shortSectors*sectorSize/4];
                for (i = 0; i < shortSectors*sectorSize/4; i++) {
                    shortSectorTable[i] = readInt(endianess);
                    Preferences.debug("shortSectorTable[" + i + "] = " + shortSectorTable[i] + "\n");
                    if (((i+1) % (sectorSize/4) == 0)  && ((lastShortSector-1) < fatSectors.length)) {
                         shortSectorTableAddress = (fatSectors[lastShortSector-1]+1)*sectorSize;
                         raFile.seek(shortSectorTableAddress);
                         lastShortSector = fatSectors[lastShortSector-1];
                    }
                }
            } // if (shortSectors > 0)
            
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
            // offset 116 length 4 bytes first sector of short stream container stream
            shortStreamStartSect = readInt(endianess);
            Preferences.debug("First sector of the short stream container stream = " +
                              shortStreamStartSect + "\n");
            // Offset 120 length 4 bytes Total size of the short stream container stream
            totalShortStreamSize = getUInt(endianess);
                Preferences.debug("Total size of the short stream container stream = " +
                                  totalShortStreamSize + "\n");
            // Offset 124 length 2 bytes dptPropType Reserved for future use.  Must be zero
            int dptPropType = getUnsignedShort(endianess);
            if (dptPropType == 0) {
                Preferences.debug("dptPropType = 0 as expected\n");
            }
            else {
                Preferences.debug("dptProptType = " + dptPropType + " instead of the expected 0\n");
            }
            
            int directoryEntry = 1;
            directoryStart = directoryStart + 128;
            int lastDirectorySector = directoryStartSector;
            int maximumDirectoryEntriesPerSector = sectorSize/128;
            while (true) {
                Preferences.debug("\nReading element " + directoryEntry + " of the directory array\n");
                directoryEntry++;
                raFile.seek(directoryStart+64);
                // Read the length of the element name in bytes.  Each Unicode character is 2 bytes
                elementNameBytes = getUnsignedShort(endianess);
                Preferences.debug("The element name has " + (elementNameBytes/2) + " unicode characters\n");
                if (elementNameBytes <= 0) {
                    break;
                }
                // Read the element name
                raFile.seek(directoryStart);
                b = new byte[elementNameBytes];
                raFile.readFully(b);
                String lastElementName = elementName;
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
                    break;
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
                    break;
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
                    break;
                }
                
                directoryStart = directoryStart + 128;
                
                if ((lastElementName.equals("Image")) &&
                        (elementName.equals("Contents")) && (objectType[0] == 2) && (streamSize > 0)) {
                    contentsStartSect = startSect;
                    contentsStreamSize = streamSize;
                }
                
                if (((directoryEntry % maximumDirectoryEntriesPerSector) == 0) &&
                        ((lastDirectorySector - 1) < fatSectors.length)) {
                    directoryStart =  (fatSectors[lastDirectorySector]+1)*sectorSize;
                    lastDirectorySector = fatSectors[lastDirectorySector-1];
                }
                
            } // while (true)*/
            
            /*if (rootStreamSize > 0) {
                if (rootStreamSize < miniSectorCutoff) {
                    rootStart = rootStartSect*miniSectorSize;   
                }
                else {
                    rootStart = (rootStartSect+1)*sectorSize;
                }
                Preferences.debug("rootStart = " + rootStart + "\n");
                readBlock(rootStart, rootStreamSize);
            } // if (rootStreamSize > 0)*/
            
            /*if (contentsStreamSize > 0) {
                // Read the Contents stream

                if (contentsStreamSize < miniSectorCutoff) {
                    contentsStart = contentsStartSect*shortSectorSize; 
                    readBuffer = readMiniStreamIntoBuffer(contentsStartSect, contentsStreamSize);
                }
                else {
                    contentsStart = (contentsStartSect+1)*sectorSize;    
                }
                Preferences.debug("contentsStart = " + contentsStart + "\n");
                readBlock(readBuffer);
            } // if (contentsStreamSize > 0)*/
            image.calcMinMax(); 
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
    
    private sliceInfo[] readHeader() throws IOException {
        //      Start reading ole compound file structure
        // The header is always 512 bytes long and always located at offset zero.
        // Offset 0 Length 8 bytes olecf file signature
        long olecfFileSignature = getLong(endianess);
        if (olecfFileSignature == 0xe11ab1a1e011cfd0L) {
            Preferences.debug("Found olecf file signature\n");
        }
        else {
            Preferences.debug("Instead of olecf file signature found = " + olecfFileSignature + "\n");
        }
        Set Z_Set = new HashSet(); // to hold Z plan index collection
        Set T_Set = new HashSet(); // to hold T time index collection
        long pos = 0;
        Vector blockList = new Vector();
        int numZ = 0;
        int numT = 0;
        int numC = 0; 
        while (true) {
            // search for start of next image header
            long header = findBlock(raFile, ZVI_MAGIC_BLOCK_1, pos);

            if (header < 0) {
              // no more potential headers found; we're done
              break;
            }
            pos = header + ZVI_MAGIC_BLOCK_1.length;

            Preferences.debug("Found potential image block: " + header + "\n");

            // these bytes don't matter
            raFile.skipBytes(19);
            pos += 19;

            // these bytes should match ZVI_MAGIC_BLOCK_2
            byte[] b = new byte[ZVI_MAGIC_BLOCK_2.length];
            raFile.readFully(b);
            boolean ok = true;
            for (int i=0; i<b.length; i++) {
              if (b[i] != ZVI_MAGIC_BLOCK_2[i]) {
                ok = false;
                break;
              }
              pos++;
            }
            if (!ok) continue;

            // these bytes should be 00
            b = new byte[11];
            raFile.readFully(b);
            for (int i=0; i<b.length; i++) {
              if (b[i] != 0) {
                ok = false;
                break;
              }
              pos++;
            }
            if (!ok) continue;

            // read potential header information
            int theZ = getInt(endianess);
            int theC = getInt(endianess);
            int theT = getInt(endianess);
            pos += 12;

            // these bytes should be 00
            b = new byte[108];
            raFile.readFully(b);
            for (int i=0; i<b.length; i++) {
              if (b[i] != 0) {
                ok = false;
                break;
              }
              pos++;
            }
            if (!ok) continue;
            // everything checks out; looks like an image header to me

//      + (mb) decoding strategy modification
            //      Some zvi images have the following structure:
            //        ZVI_SIG                    Decoding:
            //        ZVI_MAGIC_BLOCK_1 
            //        ZVI_MAGIC_BLOCK_2      <== Start of header information
            //        - Z-slice (4 bytes)     -> theZ = 0
            //        - channel (4 bytes)     -> theC = 0
            //        - timestep (4 bytes)    -> theT = 0
            //        ZVI_MAGIC_BLOCK_2      <==  Start of header information
            //        - Z-slice (4 bytes)     -> theZ actual value
            //        - channel (4 bytes)     -> theC actual value
            //        - timestep (4 bytes)    -> theT actual value
            //        ZVI_MAGIC_BLOCK_3      <== End of header information
            //        ... 
            //        
            //        Two consecutive Start of header information ZVI_MAGIC_BLOCK_2
            //        make test 3) of original decoding strategy fail. The first
            //        null values are taken as theZ, theC and theT values, the
            //        following actual values are ignored. 
            //        Parsing the rest of the file appears to be ok.
            //
            //        New decoding strategy looks for the last header information
            //        ZVI_MAGIC_BLOCK_2 / ZVI_MAGIC_BLOCK_3 to get proper image
            //        slice theZ, theC and theT values.

            //- original code removed
            //- long magic3 = findBlock(in, ZVI_MAGIC_BLOCK_3, pos);
            //- if (magic3 < 0) return null;
            //- pos = magic3 + ZVI_MAGIC_BLOCK_3.length;
            //- 
            //-
            //+ new code
            // these bytes don't matter
            raFile.skipBytes(89);
            pos += 89;

            byte[] magic3 = new byte[ZVI_MAGIC_BLOCK_3.length];
            raFile.readFully(magic3);
            for (int i=0; i<magic3.length; i++) {
              if (magic3[i] != ZVI_MAGIC_BLOCK_3[i]) {
                ok = false;
                break;
              }
            }
            if (!ok) continue;
            pos += ZVI_MAGIC_BLOCK_3.length;
//      - (mb)

            // read more header information
            int w = getInt(endianess);
            int h = getInt(endianess);
            int alwaysOne = getInt(endianess); // don't know what this is for
            int bytesPerPixel = getInt(endianess);
            int pixelType = getInt(endianess); // not clear what this value signifies
            int bitDepth = getInt(endianess); // doesn't always equal bytesPerPixel * 8
            pos += 24;

            ZVIBlock zviBlock = new ZVIBlock(theZ, theC, theT,
              w, h, alwaysOne, bytesPerPixel, pixelType, bitDepth, pos);
            Preferences.debug("zviBlock = " + zviBlock.toString() + "\n");
//      + (mb)
            //- original code removed
            //- // perform some checks on the header info
            //- if (theZ >= numZ) numZ = theZ + 1;
            //- if (theC >= numC) numC = theC + 1;
            //- if (theT >= numT) numT = theT + 1;
            //+ new code 
            // populate Z, C and T index collections
            if ((theZ + 1) > zDim) {
                zDim = theZ + 1;
            }
            Z_Set.add(new Integer(theZ));
            if (theC < minC) {
                minC = theC;
            }
            if (theC > maxC) {
                maxC = theC;
            }
            C_Set.add(new Integer(theC));
            if ((theT + 1) > tDim) {
                tDim = theT + 1;
            }
            T_Set.add(new Integer(theT));    
//      - (mb)
           // save this image block's position
            blockList.add(zviBlock);
            pos += w * h * bytesPerPixel;
          }

          if (blockList.isEmpty()) return null;

//      + (mb)
          //+ new code
          // number of Z, C and T index
          numZ = Z_Set.size();    
          numC = C_Set.size();
          numT = T_Set.size();    
//      - (mb)

          if (numZ * numC * numT != blockList.size()) {
            Preferences.debug("ZVI Reader Warning: image counts do not match.\n");
          }

          // convert ZVI blocks into single FileInfo object
          sliceInfo[] si = new sliceInfo[blockList.size()];
          for (int i=0; i<si.length; i++) {
            ZVIBlock zviBlock = (ZVIBlock) blockList.elementAt(i);
            int dataType = -1;
            if (zviBlock.numChannels == 1) {
              if (zviBlock.bytesPerChannel == 1) dataType = UNSIGNED8;
              else if (zviBlock.bytesPerChannel == 2) dataType = UNSIGNED16;
            }
            else if (zviBlock.numChannels == 3) {
              if (zviBlock.bytesPerChannel == 1) dataType = BGR;
              else if (zviBlock.bytesPerChannel == 2) dataType = RGB48;
            }
            if (dataType < 0) {
                Preferences.debug("ZVI Reader Warning: unknown file type for image plane #" + (i + 1) + "\n");
              dataType = UNSIGNED8; // better than nothing...
            }
            si[i] = new sliceInfo();
            si[i].width = zviBlock.width;
            si[i].height = zviBlock.height;
            si[i].offset = (int) zviBlock.imagePos;
            si[i].dataType = dataType;
            si[i].theC = zviBlock.theC;
            si[i].theZ = zviBlock.theZ;
            si[i].theT = zviBlock.theT;
          }
          return si;

    }
    
    private class sliceInfo {
        private int width;
        private int height;
        private int offset;
        private int dataType;
        private String info;
        private int theZ;
        private int theC;
        private int theT;
    }
    
    /** Contains information collected from a ZVI image header. */
    private class ZVIBlock {
      private int theZ, theC, theT;
      private int width, height;
      private int alwaysOne;
      private int bytesPerPixel;
      private int pixelType;
      private int bitDepth;
      private long imagePos;

      private int numPixels;
      private int imageSize;
      private int numChannels;
      private int bytesPerChannel;

      public ZVIBlock(int theZ, int theC, int theT, int width, int height,
        int alwaysOne, int bytesPerPixel, int pixelType, int bitDepth,
        long imagePos)
      {
        this.theZ = theZ;
        this.theC = theC;
        this.theT = theT;
        this.width = width;
        this.height = height;
        this.alwaysOne = alwaysOne;
        this.bytesPerPixel = bytesPerPixel;
        this.pixelType = pixelType;
        this.bitDepth = bitDepth;
        this.imagePos = imagePos;

        numPixels = width * height;
        imageSize = numPixels * bytesPerPixel;

        numChannels = 1; 
        // the second decision is redundant, but left there for further(?) pixel types 
        if ((pixelType == 1) | (pixelType == 8)) {
            numChannels = 3;} // 1 and 8 are RGB 8-bit and 16-bit
        else if ((pixelType == 3) | (pixelType == 4)) {
          numChannels = 1;} // 3 and 4 are GRAY 8-bit and 16-bit

        if (bytesPerPixel % numChannels != 0) {
          Preferences.debug("ZVI Reader Warning: incompatible bytesPerPixel (" +
            bytesPerPixel + ") and numChannels (" + numChannels +
            "). Assuming grayscale data.\n");
          numChannels = 1;
        }
        bytesPerChannel = bytesPerPixel / numChannels;
        
        // IJ.showMessage("ZVI Reader", "numChannels: " + numChannels + ", bytesPerPixel: " + bytesPerPixel + ", pixelType: " + pixelType + ", bitDepth: " + bitDepth + ", imagePos: " + imagePos);
      }

      public String toString() {
        return "Image header block:\n" +
          "  theZ = " + theZ + "\n" +
          "  theC = " + theC + "\n" +
          "  theT = " + theT + "\n" +
          "  width = " + width + "\n" +
          "  height = " + height + "\n" +
          "  alwaysOne = " + alwaysOne + "\n" +
          "  bytesPerPixel = " + bytesPerPixel + "\n" +
          "  pixelType = " + pixelType + "\n" +
          "  bitDepth = " + bitDepth;
      }
    }

    
    private byte[] readMiniStreamIntoBuffer(int startSector, long streamSize) throws IOException {
        byte readBuffer[] = new byte[(int)streamSize];
        int numWholeBuffers;
        int numPartialBuffers;
        int numBuffers;
        int i;
        int sectorArray[];
        long startAddress;
        
        numWholeBuffers = (int)(streamSize/shortSectorSize);
        if ((streamSize % shortSectorSize) == 0) {
            numPartialBuffers = 0;
        }
        else {
            numPartialBuffers = 1; 
        }
        numBuffers = numWholeBuffers + numPartialBuffers;
        sectorArray = new int[numBuffers];
        sectorArray[0] = startSector;
        for (i = 1; i < numBuffers; i++) {
            sectorArray[i] = shortSectorTable[sectorArray[i-1]];
        }
        for (i = 0; i < numWholeBuffers; i++) {
            startAddress = shortSectorSize * sectorArray[i] + (shortStreamStartSect+1)*sectorSize;
            raFile.seek(startAddress);
            raFile.read(readBuffer, i*shortSectorSize, shortSectorSize);
        }
        if (numPartialBuffers > 1) {
            startAddress = shortSectorSize * sectorArray[numWholeBuffers] + (shortStreamStartSect+1)*sectorSize;
            raFile.seek(startAddress);
            raFile.read(readBuffer, numWholeBuffers * shortSectorSize, (int)(streamSize % shortSectorSize));
        }
        return readBuffer;
    }
    
    private void readBlock(byte buffer[]) throws IOException {
        short dataType;
        boolean booleanValue;
        short shortValue;
        int intValue;
        double doubleValue;
        int stringBytes; // In Unicode including terminating null
        byte[] b;
        String str;
        int i;
        long tmpLong;
        int bytesToRead = buffer.length;
        int pos = 0;
        while (bytesToRead > 0) {
            dataType = (short) (((buffer[pos+1] & 0xff) << 8) | (buffer[pos] & 0xff));
            pos += 2;
            bytesToRead -= 2;
            switch(dataType) {
                case VT_EMPTY:
                    Preferences.debug("Read VT_EMPTY\n");
                    break;
                case VT_BOOL:
                    // 16-bit integer true if != 0, false otherwise
                    shortValue = (short) (((buffer[pos+1] & 0xff) << 8) | (buffer[pos] & 0xff));
                    pos += 2;
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
                    intValue = (((buffer[pos + 1] & 0xff) << 8) | (buffer[pos] & 0xff));
                    pos += 2;
                    bytesToRead -=2;
                    Preferences.debug("Read USHORT with value = " + intValue + "\n");
                    break;
                case VT_I4:
                    intValue = (((buffer[pos + 3] & 0xff) << 24) | ((buffer[pos + 2] & 0xff) << 16) | 
                                 ((buffer[pos + 1] & 0xff) << 8) | (buffer[pos] & 0xff));
                    pos += 4;
                    bytesToRead -= 4;
                    Preferences.debug("Read INT with value = " + intValue + "\n");
                    break;
                case VT_R8:
                    tmpLong = (((buffer[pos + 7] & 0xffL) << 56) | ((buffer[pos + 6] & 0xffL) << 48) 
                            | ((buffer[pos + 5] & 0xffL) << 40) | ((buffer[pos + 4] & 0xffL) << 32) | 
                            ((buffer[pos + 3] & 0xffL) << 24) | ((buffer[pos + 2] & 0xffL) << 16) |
                            ((buffer[pos + 1] & 0xffL) << 8) | (buffer[pos] & 0xffL));


                    doubleValue = Double.longBitsToDouble(tmpLong);
                    pos += 8;
                    bytesToRead -= 8;
                    Preferences.debug("Read DOUBLE with value = " + doubleValue + "\n");
                    break;
                case VT_DATE:
                    tmpLong = (((buffer[pos + 7] & 0xffL) << 56) | ((buffer[pos + 6] & 0xffL) << 48) 
                            | ((buffer[pos + 5] & 0xffL) << 40) | ((buffer[pos + 4] & 0xffL) << 32) | 
                            ((buffer[pos + 3] & 0xffL) << 24) | ((buffer[pos + 2] & 0xffL) << 16) |
                            ((buffer[pos + 1] & 0xffL) << 8) | (buffer[pos] & 0xffL));


                    doubleValue = Double.longBitsToDouble(tmpLong);
                    pos += 8;
                    bytesToRead -= 8;
                    Preferences.debug("Read VT_DATE with value = " + doubleValue + "\n");
                    break;
                case VT_BSTR:
                    stringBytes = (((buffer[pos + 3] & 0xff) << 24) | ((buffer[pos + 2] & 0xff) << 16) | 
                                 ((buffer[pos + 1] & 0xff) << 8) | (buffer[pos] & 0xff));
                    pos += 4;
                    bytesToRead -= 4;
                    b = new byte[stringBytes];
                    for (i = 0; i < stringBytes; i++) {
                        b[i] = buffer[pos++];
                    }
                    bytesToRead -= stringBytes;
                    str = new String(b, "UTF-16LE").trim();
                    Preferences.debug("Read String = " + str + "\n");
                    break;
                case VT_STORED_OBJECT:
                    stringBytes = (((buffer[pos + 1] & 0xff) << 8) | (buffer[pos] & 0xff));
                    pos += 2;
                    bytesToRead -= 2;
                    b = new byte[stringBytes];
                    for (i = 0; i < stringBytes; i++) {
                        b[i] = buffer[pos++];
                    }
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
    
    /**
     * Finds the first occurrence of the given byte block within the file,
     * starting from the given file position.
     */
    private long findBlock(RandomAccessFile in, byte[] block, long start)
      throws IOException
    {
      long filePos = start;
      long fileSize = in.length();
      byte[] buf = new byte[BUFFER_SIZE];
      long spot = -1;
      int step = 0;
      boolean found = false;
      in.seek(start);

      while (true) {
        int len = (int) (fileSize - filePos);
        if (len < 0) break;
        if (len > buf.length) len = buf.length;
        in.readFully(buf, 0, len);

        for (int i=0; i<len; i++) {
          if (buf[i] == block[step]) {
            if (step == 0) {
              // could be a match; flag this spot
              spot = filePos + i;
            }
            step++;
            if (step == block.length) {
              // found complete match; done searching
              found = true;
              break;
            }
          }
          else {
            // no match; reset step indicator
            spot = -1;
            step = 0;
          }
        }
        if (found) break; // found a match; we're done
        if (len < buf.length) break; // EOF reached; we're done

        filePos += len;
      }

      // set file pointer to byte immediately following pattern
      if (spot >= 0) in.seek(spot + block.length);

      return spot;
    }


    
}
