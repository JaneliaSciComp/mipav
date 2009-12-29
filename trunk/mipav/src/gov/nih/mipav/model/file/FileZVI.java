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
    
    private static final short VT_I2 = 2;
    
    private static final short VT_UI2 = 18;
    
    private static final short VT_I4 = 3;
    
    private static final short VT_R8 = 5;
    
    private static final short VT_BSTR = 8;
    
    private static final short VT_STORED_OBJECT = 69;
    
    private static final short VT_DATE = 7;
    
    private static final short VT_DISPATCH = 9;
    
    private static final short VT_UNKNOWN = 13;
    
    private static final short VT_BLOB = 65;
    
    private static final short VT_CLSID = 72;
   
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
    
    private int imageWidth = 0;
    
    private int imageHeight = 0;
    
    private int imageCount = 0;
    
    private int imagePixelFormat = 0;
    
    private int zDim = 1;
    
    private int tDim = 1;
    
    private int channelNumber = 1;
    
    private int zArray[] = null;
    private int cArray[] = null;
    private int tArray[] = null;
    private int startSectorArray[] = null;
    private int offsetArray[] = null;
    // array pointer
    private int ap = 0;
    
    // Sector allocation table
    private int sat[] = null;
    
    private boolean add128 = false;
    
    private int minC = Integer.MAX_VALUE;
    
    private int maxC = Integer.MIN_VALUE;
    
    private int minZ = Integer.MAX_VALUE;
    
    private int maxZ = Integer.MIN_VALUE;
    
    private int minT = Integer.MAX_VALUE;
    
    private int maxT = Integer.MIN_VALUE;

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
        zArray = null;
        cArray = null;
        tArray = null;
        shortSectorTable = null;
        startSectorArray = null;
        offsetArray = null;
        sat = null;
        
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
        int i, j;
        int sliceBytes;
        int bytesToRead;
        int bytesRead;
        int dataType;
        int t;
        int z;
        int c;
        byte byteBuffer[] = null;
        byte byteBuffer2[] = null;
        short shortBuffer[] = null;
        int intBuffer[] = null;
        float floatBuffer[] = null;
        double doubleBuffer[] = null;
        long tmpLong;
        int tmpInt;
        int index;
        int b1 = 0;
        int b2 = 0;
        int b3 = 0;
        int b4 = 0;
        int b5 = 0;
        int b6 = 0;
        int b7 = 0;
        int b8 = 0;
        int sliceSize;
        int presentSector;
        
        try {
            
            imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            
            fileLength = raFile.length();
            
            
            
            fileInfo = new FileInfoZVI(fileName, fileDir, FileUtility.ZVI); // dummy fileInfo
            fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
            endianess = FileBase.LITTLE_ENDIAN;
            
            readHeader();
            
            for (i = 0; i < imageCount; i++) {
                if (cArray[i] < minC) {
                    minC = cArray[i];
                }
                if (cArray[i] > maxC) {
                    maxC = cArray[i];
                }
                if (zArray[i] < minZ) {
                    minZ = zArray[i];
                }
                if (zArray[i] > maxZ) {
                    maxZ = zArray[i];
                }
                if (tArray[i] < minT) {
                    minT = tArray[i];
                }
                if (tArray[i] > maxT) {
                    maxT = tArray[i];
                }
            }
            
            channelNumber = maxC - minC + 1;
            zDim = maxZ - minZ + 1;
            tDim = maxT - minT + 1;
            
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
            imageExtents[0] = imageWidth;
            imageExtents[1] = imageHeight;
            for (i = 0; i < imageExtents.length; i++) {
                Preferences.debug("extents[" + i + "] = " + imageExtents[i] + "\n");
            }
            sliceSize = imageExtents[0] * imageExtents[1];
            
            Preferences.debug("minC = " + minC + " maxC = " + maxC + "\n");
            Preferences.debug("Channel number = " + channelNumber + "\n");

            fileInfo.setExtents(imageExtents);
            
            switch (imagePixelFormat) {
                case 1: // 8-bit BGR 3 bytes/pixel
                    dataType = ModelStorageBase.ARGB;
                    Preferences.debug("Data type = ARGB\n");
                    sliceBytes = 3*sliceSize;
                    byteBuffer = new byte[3*sliceSize];
                    byteBuffer2 = new byte[4*sliceSize];
                    break;
                case 2: // 8-bit BGRA 4 bytes/pixel
                    dataType = ModelStorageBase.ARGB;
                    Preferences.debug("Data type = ARGB\n");
                    sliceBytes = 4*sliceSize;
                    byteBuffer = new byte[4*sliceSize];
                    byteBuffer2 = new byte[4*sliceSize];
                    break;
                case 3: // 8-bit grayscale
                    if (channelNumber == 1) {
                        dataType = ModelStorageBase.UBYTE;
                        Preferences.debug("Data type = UBYTE\n");
                        sliceBytes = sliceSize;
                        byteBuffer = new byte[sliceSize];
                    }
                    else {
                        dataType = ModelStorageBase.ARGB;
                        Preferences.debug("Data type = ARGB\n");
                        sliceBytes = sliceSize;
                        byteBuffer = new byte[sliceSize];
                        byteBuffer2 = new byte[4*sliceSize];
                    }
                    break;
                case 4: // 16-bit integer
                    if (channelNumber == 1) {
                        dataType = ModelStorageBase.SHORT;
                        Preferences.debug("Data type = SHORT\n");
                        sliceBytes = 2*sliceSize;
                        byteBuffer = new byte[2*sliceSize];
                        shortBuffer = new short[sliceSize];
                    }
                    else {
                        dataType = ModelStorageBase.ARGB_USHORT;
                        Preferences.debug("Data type = ARGB_USHORT\n");
                        sliceBytes = 2*sliceSize;
                        byteBuffer = new byte[2*sliceSize];
                        shortBuffer = new short[4*sliceSize];
                    }
                    break;
                case 5: // 32-bit integer - 4 bytes/pixel
                    dataType = ModelStorageBase.INTEGER;
                    Preferences.debug("Data type = INTEGER\n");
                    sliceBytes = 4*sliceSize;
                    byteBuffer = new byte[4*sliceSize];
                    intBuffer = new int[sliceSize];
                    break;
                case 6: // 32-bit IEEE float - 4 bytes/pixel
                    dataType = ModelStorageBase.FLOAT;
                    Preferences.debug("Data type = FLOAT\n");
                    sliceBytes = 4*sliceSize;
                    byteBuffer = new byte[4*sliceSize];
                    floatBuffer = new float[sliceSize];
                    break;
                case 7: // 64-bit IEEE float - 8 bytes/pixel
                    dataType = ModelStorageBase.DOUBLE;
                    Preferences.debug("Data type = DOUBLE\n");
                    sliceBytes = 8*sliceSize;
                    byteBuffer = new byte[8*sliceSize];
                    doubleBuffer = new double[sliceSize];
                    break;
                case 8: // 16-bit BGR - 6 bytes/pixel
                    dataType = ModelStorageBase.ARGB_USHORT;
                    Preferences.debug("Data type = ARGB_USHORT\n");
                    sliceBytes = 6*sliceSize;
                    byteBuffer = new byte[6*sliceSize];
                    shortBuffer = new short[4*sliceSize];
                    break;
                /*case 9: // 32-bit integer triples (B, G, R) - 12 bytes/pixel
                    Preferences.debug("Data type = ARGB_UINT\n");
                    sliceBytes = 12*sliceSize;
                    byteBuffer = new byte[12*sliceSize];
                    intBuffer = new int[4*sliceSize];
                    break;*/
                default:
                    dataType = ModelStorageBase.UBYTE;
                    sliceBytes = sliceSize;
            }
            fileInfo.setDataType(dataType);
            
            image = new ModelImage(dataType, imageExtents, fileName);
            
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    fireProgressStateChanged(((t-minT)*zDim + (z-minZ))*100/(tDim*zDim));
                    for (c = minC; c <= maxC; c++) {
                        for (i = 0; i < imageCount; i++) {
                            if ((cArray[i] == c) && (zArray[i] == z) && (tArray[i] == t)) {
                                presentSector = startSectorArray[i];
                                bytesToRead = sliceBytes;
                                bytesRead = 0;
                                if (add128) {
                                    raFile.seek((presentSector+1)*sectorSize + 128 + offsetArray[i]);
                                }
                                else {
                                    raFile.seek((presentSector+1)*sectorSize + offsetArray[i]);   
                                }
                                raFile.read(byteBuffer, 0, Math.min(sectorSize-offsetArray[i], bytesToRead));
                                bytesRead += Math.min(sectorSize-offsetArray[i], bytesToRead);
                                bytesToRead -= Math.min(sectorSize-offsetArray[i], bytesToRead);
                                presentSector = sat[presentSector];
                                while (bytesToRead > 0) {
                                    if (add128) {
                                        raFile.seek((presentSector+1)*sectorSize + 128);
                                    }
                                    else {
                                        raFile.seek((presentSector+1)*sectorSize);    
                                    }
                                    raFile.read(byteBuffer, bytesRead, Math.min(sectorSize, bytesToRead));
                                    bytesRead += Math.min(sectorSize, bytesToRead);
                                    bytesToRead -= Math.min(sectorSize, bytesToRead);
                                    presentSector = sat[presentSector];
                                }    
                                
                                switch (imagePixelFormat) {
                                    case 1: // 8-bit BGR 3 bytes/pixel
                                        for (j = 0; j < sliceSize; j++) {
                                            byteBuffer2[4*j + 3] = byteBuffer[3*j];
                                            byteBuffer2[4*j + 2] = byteBuffer[3*j+1];
                                            byteBuffer2[4*j + 1] = byteBuffer[3*j+2];
                                        }
                                        break;
                                    case 2: // 8-bit BGRA 4 bytes/pixel
                                        for (j = 0; j < sliceSize; j++) {
                                            byteBuffer2[4*j+3] = byteBuffer[4*j];
                                            byteBuffer2[4*j+2] = byteBuffer[4*j+1];
                                            byteBuffer2[4*j+1] = byteBuffer[4*j+2];
                                            byteBuffer2[4*j] = byteBuffer[4*j+3];
                                        }
                                        break;
                                    case 3: // 8-bit grayscale
                                        if (channelNumber > 1) {
                                            for (j = 0; j < sliceSize; j++) {
                                                byteBuffer2[4*j + (c - minC + 1)] = byteBuffer[j];    
                                            }    
                                        }
                                        break;
                                    case 4: // 16-bit integer
                                        if (channelNumber == 1) {
                                            for (j = 0, index = 0; j < sliceSize; j++) {
                                                b1 = byteBuffer[index++] & 0xff;
                                                b2 = byteBuffer[index++] & 0xff;
                                                shortBuffer[j] = (short) ((b2 << 8) | b1);
                                            }
                                        }
                                        else {
                                            for (j = 0, index = 0; j < sliceSize; j++) {
                                                b1 = byteBuffer[index++] & 0xff;
                                                b2 = byteBuffer[index++] & 0xff;
                                                shortBuffer[4*j + (c - minC + 1)] = (short) ((b2 << 8) | b1);
                                            }    
                                        }
                                        break;
                                    case 5: // 32-bit integer
                                        for (j = 0, index = 0; j < sliceSize; j++) {
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            b3 = byteBuffer[index++] & 0xff;
                                            b4 = byteBuffer[index++] & 0xff;
                                            intBuffer[j] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                        }
                                        break;
                                    case 6: // 32-bit IEEE float
                                        for (j = 0, index = 0; j < sliceSize; j++) {
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            b3 = byteBuffer[index++] & 0xff;
                                            b4 = byteBuffer[index++] & 0xff;
                                            tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);

                                            floatBuffer[j] = Float.intBitsToFloat(tmpInt);
                                        }
                                        break;
                                    case 7: // 64-bit IEEE double
                                        for (j = 0, index = 0; j < sliceSize; j++) {
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            b3 = byteBuffer[index++] & 0xff;
                                            b4 = byteBuffer[index++] & 0xff;
                                            b5 = byteBuffer[index++] & 0xff;
                                            b6 = byteBuffer[index++] & 0xff;
                                            b7 = byteBuffer[index++] & 0xff;
                                            b8 = byteBuffer[index++] & 0xff;
                                            tmpLong = (((long) b8 << 56) | ((long) b7 << 48) | ((long) b6 << 40) | ((long) b5 << 32) |
                                                           ((long) b4 << 24) | ((long) b3 << 16) | ((long) b2 << 8) | (long) b1);
                                            doubleBuffer[j] = Double.longBitsToDouble(tmpLong);
                                        }
                                        break;
                                    case 8: // 16-bit BGR - 6 bytes/pixel
                                        for (j = 0, index = 0; j < sliceSize; j++) {
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            shortBuffer[4*j + 3] = (short) ((b2 << 8) | b1);
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            shortBuffer[4*j + 2] = (short) ((b2 << 8) | b1);
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            shortBuffer[4*j + 1] = (short) ((b2 << 8) | b1);
                                        }
                                        break;
                                }
                                // break out of i loop
                                break;
                            }
                        } // for (i = 0; i < si.length; i++)
                    } // for (c = minC; c <= maxC; c++)
                    switch (dataType) {
                        case ModelStorageBase.UBYTE:
                            image.importData((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize, byteBuffer, false);
                            break;
                        case ModelStorageBase.SHORT:
                            image.importData((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize, shortBuffer, false);
                            break;
                        case ModelStorageBase.INTEGER:
                            image.importData((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize, intBuffer, false);
                            break;
                        case ModelStorageBase.FLOAT:
                            image.importData((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize, floatBuffer, false);
                            break;
                        case ModelStorageBase.DOUBLE:
                            image.importData((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize, doubleBuffer, false);
                            break;
                        case ModelStorageBase.ARGB:
                            image.importData(4*((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize), byteBuffer2, false);
                            break;
                        case ModelStorageBase.ARGB_USHORT:
                            image.importData(4*((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize), shortBuffer, false);
                            break;
                    }
                    
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
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
    
    private void readHeader() throws IOException {
        int i;
        int j;
        byte[] b;
        int startSect;
        long streamSize;
        long pos;
        int sectorsIntoShortStream;
        int presentShortSector;
        int bytesToRead;
        int presentSector;
        int presentSectorOffset;
        int bytesRead;
        int bp;
        short dType;
        byte bf[];
        int imageDepth = 0;
        int imageValidBitsPerPixel = 0;
        int sliceBytes = 0;
        int validBitsPerPixel = 0;
        int count = 0;
        int pixelFormat = 0;
        int depth = 0;
        int width = 0;
        int height = 0;
        int pixelWidth = 0;
        long shortSectorTableAddress;
        long directoryStart;
        int intValue = 0;
        int stringBytes;
        String stringValue;
        double doubleValue = 0.0;
        short shortValue;
        long tmpLong;
        boolean booleanValue;
        int measureUnits;
        //      Start reading ole compound file structure
        // The header is always 512 bytes long and should be located at offset zero.
        // Offset 0 Length 8 bytes olecf file signature
        long olecfFileSignature = getLong(endianess);
        if (olecfFileSignature == 0xe11ab1a1e011cfd0L) {
            Preferences.debug("Found olecf file signature at position 0\n");
        }
        else {
            Preferences.debug("Instead of olecf file signature found = " + olecfFileSignature + " at position 0\n");
            Preferences.debug("Look for file signature at position 128\n");
            raFile.seek(128);
            olecfFileSignature = getLong(endianess);
            if (olecfFileSignature == 0xe11ab1a1e011cfd0L) {
                add128 = true;
                Preferences.debug("Found olecf file signature at position 128\n");
            }
            else {
                Preferences.debug("Instead of olecf file signature found = " + olecfFileSignature + " at position 128\n");
            }
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
        int sectorNumber = getInt(endianess);
        Preferences.debug("Number of sectors used for the sector allocation table = " + sectorNumber + "\n");
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
        Preferences.debug("The minimum byte size for standard-streams = " + miniSectorCutoff + "\n");
        // Location 60 Length 4 bytes First sector in the short sector allocation table
        int shortStartSector = readInt(endianess);
        if (shortStartSector == -2) {
            Preferences.debug("The first sector in the short sector allocation table = END OF CHAIN\n");    
        }
        else {
            Preferences.debug("The first sector in the short sector allocation table = " + shortStartSector + "\n");
        }
        // Location 64 Length 4 bytes Number of sectors in the short sector allocation table
        long shortTableSectors = getUInt(endianess);
        Preferences.debug("Number of sectors in the short sector allocation table = " + shortTableSectors + "\n");
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
        // containing 109 secIDs.
        int sectors[] = new int[sectorNumber];
        // Entries in sector allocation table
        // -1 freeSecID  Free sector, may exist in file, but is not part of any stream
        // -2 End of Chain SecID Trailing SecID in a SecID chain
        // -3 SAT SecID First entry in the sector allocation table.
        sat = new int[sectorNumber*sectorSize/4];
        int sp = 0;
        for (i = 0; i < Math.min(sectorNumber,109); i++) {
            sectors[i] = readInt(endianess);
            //Preferences.debug("Sector " + i + " = " + sectors[i] + "\n");
            pos = raFile.getFilePointer(); 
            if (add128) {
                raFile.seek((sectors[i]+1)*sectorSize + 128);    
            }
            else {
                raFile.seek((sectors[i]+1)*sectorSize);
            }
            for (j = 0; j < sectorSize/4; j++) {
                sat[sp]= getInt(endianess);
                if (sp == 0) {
                    if (sat[sp] == -3) {
                        Preferences.debug("First sector specified for sector allocation table starts with expected -3\n");
                    }
                    else {
                        Preferences.debug("first sector specifed for sector allocation table starts with " + sat[sp] + 
                                      " instead of expected -3\n");
                    }
                }
                //Preferences.debug("sat[" + sp + "] = " + sat[sp] + "\n");
                sp++;
            }
            raFile.seek(pos);
        } // for (i = 0; i < Math.min(sectorNumber, 109); i++)
        if (sectorNumber > 109) {
            if (add128) {
                raFile.seek((difStartSector+1)*sectorSize + 128);    
            }
            else {
                raFile.seek((difStartSector+1)*sectorSize);
            }
            for (i = 109; i < sectorNumber; i++) {
                sectors[i] = readInt(endianess);
                //Preferences.debug("Sector " + i + " = " + sectors[i] + "\n");
                pos = raFile.getFilePointer(); 
                if (add128) {
                    raFile.seek((sectors[i]+1)*sectorSize + 128);    
                }
                else {
                    raFile.seek((sectors[i]+1)*sectorSize);
                }
                for (j = 0; j < sectorSize/4; j++) {
                    sat[sp]= getInt(endianess);
                    //Preferences.debug("sat[" + sp + "] = " + sat[sp] + "\n");
                    sp++;
                }
                raFile.seek(pos);  
            } // for (i = 109; i < sectorNumber; i++)
        } // if (sectorNumber > 109)
        
        // Read short sector allocation table
        if (shortTableSectors > 0) {
            Preferences.debug("\nReading the short sector allocation table\n");
            if (add128) {
                shortSectorTableAddress = (shortStartSector+1)*sectorSize + 128;    
            }
            else {
                shortSectorTableAddress = (shortStartSector+1)*sectorSize;
            }
            int shortSector = shortStartSector;
            raFile.seek(shortSectorTableAddress);
            shortSectorTable = new int[(int)shortTableSectors*sectorSize/4];
            for (i = 0; i < shortTableSectors*sectorSize/4; i++) {
                shortSectorTable[i] = readInt(endianess);
                //Preferences.debug("shortSectorTable[" + i + "] = " + shortSectorTable[i] + "\n");
                if (((i+1) % (sectorSize/4) == 0) && ((i+1) < shortTableSectors*sectorSize/4)) {
                     shortSector = sat[shortSector];
                     if (add128) {
                         shortSectorTableAddress = (shortSector+1)*sectorSize + 128;
                     }
                     else {
                         shortSectorTableAddress = (shortSector+1)*sectorSize;    
                     }
                     raFile.seek(shortSectorTableAddress);
                }
            }
        } // if (shortSectors > 0)
        
        
        // Determine the numbers of sectors in the directory chain
        int directorySectors = 1;
        int ds = directoryStartSector;
        while (sat[ds] != -2) {
            directorySectors++;
            ds = sat[ds];
        }
        Preferences.debug("The number of directory sectors = " + directorySectors + "\n");
        int directoryTable[] = new int[directorySectors];
        directoryTable[0] = directoryStartSector;
        Preferences.debug("directoryTable[0] = " + directoryTable[0] + "\n");
        for (i = 1; i < directorySectors; i++) {
            directoryTable[i] = sat[directoryTable[i-1]];
            Preferences.debug("directoryTable[" + i + "] = " + directoryTable[i] + "\n");
        }
        if (sat[directoryTable[directorySectors-1]] == -2) {
            Preferences.debug("sat[directoryTable[directorySectors-1]] == -2 as expected\n");    
        }
        else {
            Preferences.debug("sat[directoryTable[directorySectors-1]] == " + 
                    sat[directoryTable[directorySectors-1]] + 
                    " instead of expected -2\n");        
        }
        
        // Read the first sector of the directory chain (also referred to as the first element of the 
        // Directory array, or SID 0) is known as the Root Directory Entry
        Preferences.debug("\nReading the first sector of the directory chain\n");
        if (add128) {
            directoryStart = (directoryTable[0]+1)*sectorSize + 128;
        }
        else {
            directoryStart = (directoryTable[0]+1)*sectorSize;    
        }
        raFile.seek(directoryStart+64);
        // Read the length of the element name in bytes.  Each Unicode character is 2 bytes
        int elementNameBytes = getUnsignedShort(endianess);
        Preferences.debug("The element name has " + (elementNameBytes/2) + " unicode characters\n"); 
        // Read the element name
        raFile.seek(directoryStart);
        b = new byte[elementNameBytes];
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
        // offset 68 length 4 bytes SID of the left child of this entry in the directory tree
        int leftChild = readInt(endianess);
        if (leftChild == -1) {
            Preferences.debug("No left child for this entry\n");
        }
        else {
            Preferences.debug("The SID of the left child of this entry in the directory tree = " + leftChild + "\n");
        }
        // offset 72 length 4 bytes SID of the right child of this entry in the directory tree
        int rightChild = readInt(endianess);
        if (rightChild == -1) {
            Preferences.debug("No right child for this entry\n");
        }
        else {
            Preferences.debug("The SID of the right child of this entry in the directory tree = " + rightChild + "\n");
        }
        // offset 76 length 4 bytes SID of the root node entry of the red-black tree of all storage members
        // if this entry is storage, -1 otherwise
        int rootNodeEntry = readInt(endianess);
        if (rootNodeEntry == -1) {
            Preferences.debug("No root node entry for this entry\n");
        }
        else {
            Preferences.debug("The root node entry of the red-black tree of all storage members = " + rootNodeEntry + "\n");
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
            Preferences.debug("Total byte size of the short stream container stream = " +
                              totalShortStreamSize + "\n");
        int shortStreamSectorNumber = (int)(totalShortStreamSize/sectorSize);
        if ((totalShortStreamSize % sectorSize) != 0) {
            shortStreamSectorNumber++;
        }
        Preferences.debug("The number of bytes in the short stream container stream requires " +
                           shortStreamSectorNumber + " sectors\n");
        int shortSectors[] = new int[shortStreamSectorNumber];
        shortSectors[0] = shortStreamStartSect;
        Preferences.debug("shortSectors[0] = " + shortSectors[0] + "\n");
        for (i = 1; i < shortStreamSectorNumber; i++) {
            shortSectors[i] = sat[shortSectors[i-1]];
            Preferences.debug("shortSectors[" + i + "] = " + shortSectors[i] + "\n");
        }
        if (sat[shortSectors[shortStreamSectorNumber-1]] == -2) {
            Preferences.debug("sat[shortSectors[shortStreamSectorNumber-1]] == -2 as expected\n");    
        }
        else {
            Preferences.debug("sat[shortSectors[shortStreamSectorNumber-1]] == " + 
                    sat[shortSectors[shortStreamSectorNumber-1]] + 
                    " instead of expected -2\n");        
        }
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
        int maximumDirectoryEntriesPerSector = sectorSize/128;
        int dp = 0;
        while (dp < directorySectors) {
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
            // offset 68 length 4 bytes SID of the left child of this entry in the directory tree
            leftChild = readInt(endianess);
            if (leftChild == -1) {
                Preferences.debug("No left child for this entry\n");
            }
            else {
                Preferences.debug("The SID of the left child of this entry in the directory tree = " + leftChild + "\n");
            }
            // offset 72 length 4 bytes SID of the right child of this entry in the directory tree
            rightChild = readInt(endianess);
            if (rightChild == -1) {
                Preferences.debug("No right child for this entry\n");
            }
            else {
                Preferences.debug("The SID of the right child of this entry in the directory tree = " + rightChild + "\n");
            }
            // offset 76 length 4 bytes SID of the root node entry of the red-black tree of all storage members
            // if this entry is storage, -1 otherwise
            rootNodeEntry = readInt(endianess);
            if (rootNodeEntry == -1) {
                Preferences.debug("No root node entry for this entry\n");
            }
            else {
                Preferences.debug("The root node entry of the red-black tree of all storage members = " + rootNodeEntry + "\n");
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
                Preferences.debug("Reading the contents stream of the container image\n");
                      
                bytesToRead = (int)streamSize;
                b = new byte[bytesToRead];
                bytesRead = 0;
                    if (streamSize < miniSectorCutoff) {
                        presentShortSector = startSect;
                        while (bytesToRead > 0) {
                            sectorsIntoShortStream = presentShortSector*shortSectorSize/sectorSize;
                            presentSector = shortSectors[sectorsIntoShortStream];
                            presentSectorOffset = (presentShortSector*shortSectorSize) % sectorSize;
                            if (add128) {
                                raFile.seek((presentSector+1)*sectorSize + 128 + presentSectorOffset);
                            }
                            else {
                                raFile.seek((presentSector+1)*sectorSize + presentSectorOffset);    
                            }
                            raFile.read(b, bytesRead, Math.min(shortSectorSize, bytesToRead));
                            bytesRead += Math.min(shortSectorSize, bytesToRead);
                            bytesToRead -= Math.min(shortSectorSize, bytesToRead);
                            presentShortSector = shortSectorTable[presentShortSector];
                        }
                } // if (streamSize < miniSectorCutoff)
                else { // else streamSize >= miniSectorCutoff
                    presentSector = startSect;
                    while (bytesToRead > 0) {
                        if (add128) {
                            raFile.seek((presentSector+1)*sectorSize + 128);
                        }
                        else {
                            raFile.seek((presentSector+1)*sectorSize);    
                        }
                        raFile.read(b, bytesRead, Math.min(sectorSize, bytesToRead));
                        bytesRead += Math.min(sectorSize, bytesToRead);
                        bytesToRead -= Math.min(sectorSize, bytesToRead);
                        presentSector = sat[presentSector];
                    }    
                } // else streamSize >= miniSectorCutoff
                while (true) {
                    bp = 0;
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for version\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for version\n");
                        break;
                    }
                    minorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Minor version is " + minorVersion + "\n");
                    Preferences.debug("Current version is 4099\n");
                    majorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Major version is " + majorVersion + "\n");
                    Preferences.debug("Current version is 12288\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for file type\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for file type\n");
                        break;
                    }
                    // File type not used
                    int fileType = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused file type = " + fileType + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_EMPTY) {
                        Preferences.debug("Expected VT_EMPTY data type for unused type description\n");
                    }
                    else if (dType == VT_BSTR) {
                        Preferences.debug("VT_BSTR for type description\n");
                        stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        bf = new byte[stringBytes];
                        for (i = 0; i < stringBytes; i++) {
                            bf[i] = b[bp++];
                        }
                        String typeDescription = new String(b, "UTF-16LE").trim();
                        Preferences.debug("Type description = " + typeDescription + "\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + 
                                " instead of expected VT_EMPTY or VT_BSTR for type description\n");
                        break;
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_EMPTY) {
                        Preferences.debug("Expected VT_EMPTY data type for name of zvi file\n");
                    }
                    else if (dType == VT_BSTR) {
                        Preferences.debug("VT_BSTR for name of zvi file\n");
                        stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        bf = new byte[stringBytes];
                        for (i = 0; i < stringBytes; i++) {
                            bf[i] = b[bp++];
                        }
                        String fileName = new String(b, "UTF-16LE").trim();
                        Preferences.debug("Name of zvi file = " + fileName + "\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + 
                                " instead of expected VT_EMPTY or VT_BSTR for name of zvi file\n");
                        break;
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for imageWidth\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for imageWidth\n");
                        break;
                    }
                    imageWidth = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Image width = " + imageWidth + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for imageHeight\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for imageHeight\n");
                        break;
                    }
                    imageHeight = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Image height = " + imageHeight + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for imageDepth\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for imageDepth\n");
                        break;
                    }
                    imageDepth = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused image depth = " + imageDepth + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for imagePixelFormat\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for imagePixelFormat\n");
                        break;
                    }
                    imagePixelFormat = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    switch(imagePixelFormat) {
                        case 1:
                            Preferences.debug("Image pixel format = 8-bit B, G, R - 3 bytes/pixel\n");
                            sliceBytes = 3 * imageWidth * imageHeight;
                            break;
                        case 2:
                            Preferences.debug("Image pixel format = 8-bit B, G, R, A - 4 bytes/pixel\n");
                            sliceBytes = 4 * imageWidth * imageHeight;
                            break;
                        case 3:
                            Preferences.debug("Image pixel format = 8-bit grayscale\n");
                            sliceBytes = imageWidth * imageHeight;
                            break;
                        case 4:
                            Preferences.debug("Image pixel format = 16-bit integer\n");
                            sliceBytes = 2 * imageWidth * imageHeight;
                            break;
                        case 5:
                            Preferences.debug("Image pixel format = 32-bit integer\n");
                            sliceBytes = 4 * imageWidth * imageHeight;
                            break;
                        case 6:
                            Preferences.debug("Image pixel format = 32-bit IEEE float\n");
                            sliceBytes = 4 * imageWidth * imageHeight;
                            break;
                        case 7:
                            Preferences.debug("Image pixel format = 64-bit IEEE double\n");
                            sliceBytes = 8 * imageWidth * imageHeight;
                            break;
                        case 8:
                            Preferences.debug("Image pixel format = 16-bit B, G, R - 6 bytes/pixel\n");
                            sliceBytes = 6 * imageWidth * imageHeight;
                            break;
                        case 9:
                            Preferences.debug("Image pixel format = 32-bit B, G, R = 12 bytes/pixel\n");
                            sliceBytes = 12 * imageWidth * imageHeight;
                            break;
                        default:
                            Preferences.debug("imagePixelFormat has an unrecognized value = " + imagePixelFormat + "\n");
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for imageCount\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for imageCount\n");
                        break;
                    }
                    imageCount = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Total number of image item storages = " + imageCount + "\n");
                    zArray = new int[imageCount];
                    cArray = new int[imageCount];
                    tArray = new int[imageCount];
                    startSectorArray = new int[imageCount];
                    offsetArray = new int[imageCount];
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for imageValidBitsPerPixel\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for imageValidBitsPerPixel\n");
                        break;
                    }
                    // Valid bits per pixel in raw image data if 16 bit image (may be 12, 14, or 16)
                    imageValidBitsPerPixel = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Valid bits per pixel in raw image data = " + imageValidBitsPerPixel + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_BLOB) {
                        Preferences.debug("Expected VT_BLOB data type for {m_PluginCLSID}\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_BLOB for {m_PluginCLSID}\n");
                        break;
                    }
                    int pluginLength = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("The length of the {m_PluginCLSID} binary data = " + pluginLength + "\n");
                    bp += pluginLength;
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_BLOB) {
                        Preferences.debug("Expected VT_BLOB data type for {Others}\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_BLOB for {Others}\n");
                        break;
                    }
                    int othersLength = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("The length of the {Others} binary data = " + othersLength + "\n");
                    bp += othersLength;
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_STORED_OBJECT) {
                        Preferences.debug("Expected VT_STORED_OBJECT data type for {Layers}\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_STORED_OBJECT for {Layers}\n");
                        break;
                    }
                    int layersLength =  (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Byte length of unicode string in Layers stored object = " + layersLength + "\n");
                    byte obj[] = new byte[layersLength];
                    for (i = 0; i < layersLength; i++) {
                        obj[i] = b[bp++];
                    }
                    String str = new String(obj, "UTF-16LE").trim();
                    Preferences.debug("Name of the storage containing the vector overlay layers = " + str + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_STORED_OBJECT) {
                        Preferences.debug("Expected VT_STORED_OBJECT data type for {Tags}\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_STORED_OBJECT for {Tags}\n");
                        break;
                    }
                    int tagsLength =  (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Byte length of unicode string in Tags stored object = " + tagsLength + "\n");
                    obj = new byte[tagsLength];
                    for (i = 0; i < tagsLength; i++) {
                        obj[i] = b[bp++];
                    }
                    str = new String(obj, "UTF-16LE").trim();
                    Preferences.debug("Name of the storage containing the Tags information = " + str + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_STORED_OBJECT) {
                        Preferences.debug("Expected VT_STORED_OBJECT data type for {Scaling}\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_STORED_OBJECT for {Scaling}\n");
                        break;
                    }
                    int scalingLength =  (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Byte length of unicode string in Scaling stored object = " + scalingLength + "\n");
                    obj = new byte[scalingLength];
                    for (i = 0; i < scalingLength; i++) {
                        obj[i] = b[bp++];
                    }
                    str = new String(obj, "UTF-16LE").trim();
                    Preferences.debug("Name of the storage containing the scaling information = " + str + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_STORED_OBJECT) {
                        Preferences.debug("Expected VT_STORED_OBJECT data type for {RootFloder}\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_STORED_OBJECT for {RootFolder}\n");
                        break;
                    }
                    int rootFolderLength =  (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Byte length of unicode string in RootFolder stored object = " + rootFolderLength + "\n");
                    obj = new byte[rootFolderLength];
                    for (i = 0; i < rootFolderLength; i++) {
                        obj[i] = b[bp++];
                    }
                    str = new String(obj, "UTF-16LE").trim();
                    Preferences.debug("Name of the storage containing a ZiFolder object with advanced information = " 
                                      + str + "\n");
                    break;
                } // while (true)
            } // if ((lastElementName.equals("Image")) &&
            
            if ((lastElementName.length() > 4) && (lastElementName.substring(0,4).equals("Item")) &&
                    (elementName.equals("Contents")) && (objectType[0] == 2) && (streamSize >= sliceBytes)) {
                Preferences.debug("Reading Contents of " + lastElementName + "\n");
                startSectorArray[ap] = startSect;
                bytesToRead = (int)streamSize;
                b = new byte[bytesToRead];
                bytesRead = 0;
                    if (streamSize < miniSectorCutoff) {
                        presentShortSector = startSect;
                        while (bytesToRead > 0) {
                            sectorsIntoShortStream = presentShortSector*shortSectorSize/sectorSize;
                            presentSector = shortSectors[sectorsIntoShortStream];
                            presentSectorOffset = (presentShortSector*shortSectorSize) % sectorSize;
                            if (add128) {
                                raFile.seek((presentSector+1)*sectorSize + 128 + presentSectorOffset);
                            }
                            else {
                                raFile.seek((presentSector+1)*sectorSize + presentSectorOffset);    
                            }
                            raFile.read(b, bytesRead, Math.min(shortSectorSize, bytesToRead));
                            bytesRead += Math.min(shortSectorSize, bytesToRead);
                            bytesToRead -= Math.min(shortSectorSize, bytesToRead);
                            presentShortSector = shortSectorTable[presentShortSector];
                        }
                } // if (streamSize < miniSectorCutoff)
                else { // else streamSize >= miniSectorCutoff
                    // One sector is plenty to read for getting the data fields before the raw image data
                    if (add128) {
                        raFile.seek((startSect+1)*sectorSize + 128);
                    }
                    else {
                        raFile.seek((startSect+1)*sectorSize);
                    }
                    raFile.read(b, 0, Math.min(sectorSize, bytesToRead));
                    /*presentSector = startSect;
                    while (bytesToRead > 0) {
                        raFile.seek((presentSector+1)*sectorSize);
                        raFile.read(b, bytesRead, Math.min(sectorSize, bytesToRead));
                        bytesRead += Math.min(sectorSize, bytesToRead);
                        bytesToRead -= Math.min(sectorSize, bytesToRead);
                        presentSector = sat[presentSector];
                    } */   
                } // else streamSize >= miniSectorCutoff
                while (true) {
                    bp = 0;
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for version\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for version\n");
                        break;
                    }
                    minorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Minor version is " + minorVersion + "\n");
                    Preferences.debug("Current version is 4099\n");
                    majorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Major version is " + majorVersion + "\n");
                    Preferences.debug("Current version is 12288\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for file type\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for file type\n");
                        break;
                    }
                    // File type not used
                    int fileType = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused file type = " + fileType + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_EMPTY) {
                        Preferences.debug("Expected VT_EMPTY data type for unused type description\n");
                    }
                    else if (dType == VT_BSTR) {
                        Preferences.debug("VT_BSTR for type description\n");
                        stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        bf = new byte[stringBytes];
                        for (i = 0; i < stringBytes; i++) {
                            bf[i] = b[bp++];
                        }
                        String typeDescription = new String(b, "UTF-16LE").trim();
                        Preferences.debug("Type description = " + typeDescription + "\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + 
                                " instead of expected VT_EMPTY or VT_BSTR for type description\n");
                        break;
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_EMPTY) {
                        Preferences.debug("Expected VT_EMPTY data type for name of zvi file\n");
                    }
                    else if (dType == VT_BSTR) {
                        Preferences.debug("VT_BSTR for name of zvi file\n");
                        stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        bf = new byte[stringBytes];
                        for (i = 0; i < stringBytes; i++) {
                            bf[i] = b[bp++];
                        }
                        String fileName = new String(b, "UTF-16LE").trim();
                        Preferences.debug("Name of zvi file = " + fileName + "\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + 
                                " instead of expected VT_EMPTY or VT_BSTR for name of zvi file\n");
                        break;
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for width\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for width\n");
                        break;
                    }
                    width = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Width = " + width + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for height\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for height\n");
                        break;
                    }
                    height = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Height = " + height + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for depth\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for depth\n");
                        break;
                    }
                    depth = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused depth = " + depth + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for pixelFormat\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for pixelFormat\n");
                        break;
                    }
                    pixelFormat = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    displayPixelFormat(pixelFormat);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for count\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for count\n");
                        break;
                    }
                    count = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    if (count == 0) {
                        Preferences.debug("Count = 0 as expected\n");
                    }
                    else {
                        Preferences.debug("Count = " + count + " instead of the expected 0\n");
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for validBitsPerPixel\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for validBitsPerPixel\n");
                        break;
                    }
                    // Valid bits per pixel in raw image data if 16 bit image (may be 12, 14, or 16)
                    validBitsPerPixel = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Valid bits per pixel in raw image data = " + validBitsPerPixel + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    // Note that the ZVI Format Specification V 2.0.4 - June, 2009 incorrectly specifies a 
                    // VT_CLSID for m_PluginCLSID.
                    if (dType == VT_BLOB) {
                        Preferences.debug("Expected VT_BLOB data type for m_PluginCSLID\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_BLOB for m_pluginCLSID\n");
                        break;
                    }
                    int pluginLength = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Length of binary data for m_pluginCLSID = " + pluginLength + "\n");
                    // Skip over m_pluginCLSID data field
                    bp += pluginLength;
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_BLOB) {
                        Preferences.debug("Expected VT_BLOB data type for coordinate block stored as {Others}\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + 
                                " instead of expected VT_BLOB for coordinate block stored as {Others}\n");
                        break;
                    }
                    int othersLength = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("The {Others} length = " + othersLength + "\n");
                    int U = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    if (U == 0) {
                        Preferences.debug("The U tile ID is 0 as expected\n");
                    }
                    else {
                        Preferences.debug("The U tile ID = " + U + " instead of the expected 0\n");
                    }
                    int V = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    if (V == 0) {
                        Preferences.debug("The V tile ID is 0 as expected\n");
                    }
                    else {
                        Preferences.debug("The V tile ID = " + V + " instead of the expected 0\n");
                    }
                    // Z ID
                    zArray[ap] = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Z ID = " + zArray[ap] + "\n");
                    // Channel ID
                    cArray[ap] = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Channel ID = " + cArray[ap] + "\n");
                    // Time ID
                    tArray[ap] = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Time ID = " + tArray[ap] + "\n");
                    int sceneID = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Scene ID = " + sceneID + "\n");
                    int positionID = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Position ID = " + positionID + "\n");
                    int A = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused A = " + A + "\n");
                    // othersLength - 8 integers in coordinate block
                    bp += othersLength - 32;
                    int bytesToImageHeader = 88;
                    while (bytesToImageHeader > 0) {
                        dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        bytesToImageHeader -= 2;
                        if (dType == VT_DISPATCH) {
                            Preferences.debug("dType = VT_DISPATCH\n");
                            bp += Math.min(bytesToImageHeader, 16);
                            bytesToImageHeader -= Math.min(bytesToImageHeader, 16);
                        }
                        else if (dType == VT_STORED_OBJECT) {
                            Preferences.debug("dType = VT_STORED_OBJECT\n");
                            int objectLength =  (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                    ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                            bp += 4;
                            bytesToImageHeader -= 4;
                            Preferences.debug("Byte length of unicode string in stored object = " + objectLength + "\n");
                            byte obj[] = new byte[Math.min(objectLength, bytesToImageHeader)];
                            for (i = 0; i < Math.min(objectLength, bytesToImageHeader); i++) {
                                obj[i] = b[bp++];
                            }
                            bytesToImageHeader -= Math.min(objectLength, bytesToImageHeader);
                            String str = new String(obj, "UTF-16LE").trim();
                            Preferences.debug("Object string = " + str + "\n");
                        }
                    } // while (bytesToImageHeader > 0)
                    // Read the image header at 296 bytes from the stream beginning
                    // Don't read dType any more
                    Preferences.debug("Reading the image header\n");
                    // Stream version ID
                    // 2 byte minor 0x2000 followed by 2 byte major 0x1000
                    minorVersion = (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("The minor version = " + minorVersion + "\n");
                    Preferences.debug("The current minor version is 8192\n");
                    majorVersion = (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("The major version = " + majorVersion + "\n");
                    Preferences.debug("The current major version is 4096\n");
                    width = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Width = " + width + "\n");
                    height = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Height = " + height + "\n");
                    depth = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused depth = " + depth + "\n");
                    pixelWidth = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Size in bytes of an image pixel = " + pixelWidth + "\n");
                    pixelFormat = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    displayPixelFormat(pixelFormat);
                    // Valid bits per pixel in raw image data if 16 bit image (may be 12, 14, or 16)
                    validBitsPerPixel = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Valid bits per pixel in raw image data = " + validBitsPerPixel + "\n");
                    // Raw bytes for image slice are stored here
                    // Store the offset into the starting sector
                    offsetArray[ap++] = bp;
                    break;
                } // while (true)
            } // if ((lastElementName.length() > 4) && (lastElementName.substring(0,4).equals("Item")) &&
            
            if ((lastElementName.equals("Scaling")) &&
                    (elementName.equals("Contents")) && (objectType[0] == 2) && (streamSize > 0)) {
                Preferences.debug("Reading the contents stream of the Scaling storage\n");
                      
                bytesToRead = (int)streamSize;
                b = new byte[bytesToRead];
                bytesRead = 0;
                    if (streamSize < miniSectorCutoff) {
                        presentShortSector = startSect;
                        while (bytesToRead > 0) {
                            sectorsIntoShortStream = presentShortSector*shortSectorSize/sectorSize;
                            presentSector = shortSectors[sectorsIntoShortStream];
                            presentSectorOffset = (presentShortSector*shortSectorSize) % sectorSize;
                            if (add128) {
                                raFile.seek((presentSector+1)*sectorSize + 128 + presentSectorOffset);
                            }
                            else {
                                raFile.seek((presentSector+1)*sectorSize + presentSectorOffset);    
                            }
                            raFile.read(b, bytesRead, Math.min(shortSectorSize, bytesToRead));
                            bytesRead += Math.min(shortSectorSize, bytesToRead);
                            bytesToRead -= Math.min(shortSectorSize, bytesToRead);
                            presentShortSector = shortSectorTable[presentShortSector];
                        }
                } // if (streamSize < miniSectorCutoff)
                else { // else streamSize >= miniSectorCutoff
                    presentSector = startSect;
                    while (bytesToRead > 0) {
                        if (add128) {
                            raFile.seek((presentSector+1)*sectorSize + 128);
                        }
                        else {
                            raFile.seek((presentSector+1)*sectorSize);   
                        }
                        raFile.read(b, bytesRead, Math.min(sectorSize, bytesToRead));
                        bytesRead += Math.min(sectorSize, bytesToRead);
                        bytesToRead -= Math.min(sectorSize, bytesToRead);
                        presentSector = sat[presentSector];
                    }    
                } // else streamSize >= miniSectorCutoff
                while (true) {
                    bp = 0;
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for version\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for version\n");
                        break;
                    }
                    minorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Minor version is " + minorVersion + "\n");
                    Preferences.debug("Current version is 4098\n");
                    majorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Major version is " + majorVersion + "\n");
                    Preferences.debug("Current version is 8193\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_BSTR) {
                        Preferences.debug("Expected VT_BSTR data type for original key name\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_BSTR for original key name\n");
                        break;
                    }
                    stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    bf = new byte[stringBytes];
                    for (i = 0; i < stringBytes; i++) {
                        bf[i] = b[bp++];
                    }
                    String originalKeyName = new String(b, "UTF-16LE").trim();
                    Preferences.debug("Original key name = " + originalKeyName + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for unused scaling category\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for unused scaling category\n");
                        break;
                    }
                    int scalingCategory = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused scaling category = " + scalingCategory + "\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_R8) {
                        Preferences.debug("Expected VT_R8 data type for scaling factor(units per pixel)\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_R8 for unused scaling factor\n");
                        break;
                    }
                    tmpLong = (((b[bp+7] & 0xffL) << 56) | ((b[bp+6] & 0xffL) << 48) | ((b[bp+5] & 0xffL) << 40) | 
                            ((b[bp+4] & 0xffL) << 32) | ((b[bp+3] & 0xffL) << 24) | ((b[bp+2] & 0xffL) << 16) |
                            ((b[bp+1] & 0xffL) << 8) | (b[bp] & 0xffL));
                    bp += 8;
                    double scalingFactor = Double.longBitsToDouble(tmpLong);
                    Preferences.debug("Scaling factor (units per pixel) = " + scalingFactor + "\n");
                    fileInfo.setResolutions((float)scalingFactor, 0);
                    fileInfo.setResolutions((float)scalingFactor, 1);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for scaling unit type\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for scaling unit type\n");
                        break;
                    }
                    int scalingUnitType = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    measureUnits = zviToMipavMeasurementUnits(scalingUnitType);
                    fileInfo.setUnitsOfMeasure(measureUnits, 0);
                    fileInfo.setUnitsOfMeasure(measureUnits, 1);
                    
                    break;
                } // while (true)
            } // if ((lastElementName.equals("Scaling")) &&
            
            if ((lastElementName.equals("Tags")) &&
                    (elementName.equals("Contents")) && (objectType[0] == 2) && (streamSize > 0)) {
                Preferences.debug("Reading the contents stream of Tags storage\n");
                      
                bytesToRead = (int)streamSize;
                b = new byte[bytesToRead];
                bytesRead = 0;
                    if (streamSize < miniSectorCutoff) {
                        presentShortSector = startSect;
                        while (bytesToRead > 0) {
                            sectorsIntoShortStream = presentShortSector*shortSectorSize/sectorSize;
                            presentSector = shortSectors[sectorsIntoShortStream];
                            presentSectorOffset = (presentShortSector*shortSectorSize) % sectorSize;
                            if (add128) {
                                raFile.seek((presentSector+1)*sectorSize + 128 + presentSectorOffset);
                            }
                            else {
                                raFile.seek((presentSector+1)*sectorSize + presentSectorOffset);    
                            }
                            raFile.read(b, bytesRead, Math.min(shortSectorSize, bytesToRead));
                            bytesRead += Math.min(shortSectorSize, bytesToRead);
                            bytesToRead -= Math.min(shortSectorSize, bytesToRead);
                            presentShortSector = shortSectorTable[presentShortSector];
                        }
                } // if (streamSize < miniSectorCutoff)
                else { // else streamSize >= miniSectorCutoff
                    presentSector = startSect;
                    while (bytesToRead > 0) {
                        if (add128) {
                            raFile.seek((presentSector+1)*sectorSize + 128);
                        }
                        else {
                            raFile.seek((presentSector+1)*sectorSize);   
                        }
                        raFile.read(b, bytesRead, Math.min(sectorSize, bytesToRead));
                        bytesRead += Math.min(sectorSize, bytesToRead);
                        bytesToRead -= Math.min(sectorSize, bytesToRead);
                        presentSector = sat[presentSector];
                    }    
                } // else streamSize >= miniSectorCutoff
                trueLoop:
                while (true) {
                    bp = 0;
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for version\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for version\n");
                        break;
                    }
                    minorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Minor version is " + minorVersion + "\n");
                    Preferences.debug("Current version is 4096\n");
                    majorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Major version is " + majorVersion + "\n");
                    Preferences.debug("Current version is 8192\n");
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for count of token triples\n");
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for count of token triples\n");
                        break;
                    }
                    int tokenCount = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Count of token triples = " + tokenCount + "\n");
                    for (i = 0; i < tokenCount && bp < b.length - 13; i++) {
                        short valueDType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        switch(valueDType) {
                            case VT_EMPTY:
                                Preferences.debug("Data type of value is VT_EMPTY\n");
                                break;
                            case VT_BOOL:
                                Preferences.debug("Data type of value is VT_BOOL\n");
                                shortValue = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                                if (shortValue != 0) {
                                    booleanValue = true;
                                }
                                else {
                                    booleanValue = false;
                                }
                                Preferences.debug("Value = " + booleanValue + "\n");
                                bp += 2;
                                break;
                            case VT_I2:
                                Preferences.debug("Data type of value is VT_I2\n");
                                shortValue = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                                Preferences.debug("Value = " + shortValue + "\n");
                                bp += 2;
                                break;
                            case VT_I4:
                                Preferences.debug("Data type of value is VT_I4\n");
                                intValue = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                           ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                                Preferences.debug("Value = " + intValue + "\n");
                                bp += 4;
                                break;
                            case VT_R8:
                                Preferences.debug("Data type of value is VT_R8\n");
                                tmpLong = (((b[bp+7] & 0xffL) << 56) | ((b[bp+6] & 0xffL) << 48) | ((b[bp+5] & 0xffL) << 40) | 
                                        ((b[bp+4] & 0xffL) << 32) | ((b[bp+3] & 0xffL) << 24) | ((b[bp+2] & 0xffL) << 16) |
                                        ((b[bp+1] & 0xffL) << 8) | (b[bp] & 0xffL));
                                bp += 8;
                                doubleValue = Double.longBitsToDouble(tmpLong);
                                Preferences.debug("Value = " + doubleValue + "\n");
                                break;
                            case VT_BSTR:
                                Preferences.debug("Data type of value is VT_BSTR\n");
                                stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                        ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                                bp += 4;
                                bf = new byte[stringBytes];
                                for (i = 0; i < stringBytes; i++) {
                                    bf[i] = b[bp++];
                                }
                                stringValue = new String(b, "UTF-16LE").trim();
                                //Preferences.debug("Value = " + valueString + "\n");
                                break;
                            case VT_STORED_OBJECT:
                                Preferences.debug("Data type of value is VT_STORED_OBJECT\n");
                                stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                        ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                                bp += 4;
                                bf = new byte[stringBytes];
                                for (i = 0; i < stringBytes; i++) {
                                    bf[i] = b[bp++];
                                }
                                stringValue = new String(b, "UTF-16LE").trim();
                                //Preferences.debug("Value = " + valueString + "\n");
                                break;
                            case VT_DISPATCH:
                                Preferences.debug("Data type of value is VT_DISPATCH\n");
                                bp += 16;
                                break;
                            case VT_DATE:
                                Preferences.debug("Data type of value is VT_DATE\n");
                                tmpLong = (((b[bp+7] & 0xffL) << 56) | ((b[bp+6] & 0xffL) << 48) | ((b[bp+5] & 0xffL) << 40) | 
                                        ((b[bp+4] & 0xffL) << 32) | ((b[bp+3] & 0xffL) << 24) | ((b[bp+2] & 0xffL) << 16) |
                                        ((b[bp+1] & 0xffL) << 8) | (b[bp] & 0xffL));
                                bp += 8;
                                doubleValue = Double.longBitsToDouble(tmpLong);
                                Preferences.debug("Value = " + doubleValue + "\n");
                                break;
                            default:
                                Preferences.debug("Unrecognized data type of value = " + valueDType + "\n");
                                break trueLoop;
                        } // switch(dType)
                        
                        dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        if (dType == VT_I4) {
                            Preferences.debug("Expected VT_I4 data type for tagID\n");
                        }
                        else {
                            Preferences.debug("dType = " + dType + " instead of expected VT_I4 for tagID\n");
                            break trueLoop;
                        }
                        int tagID = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        switch(tagID) {
                            case 222:
                                Preferences.debug("tagID = Compression\n");
                                break;
                            case 257:
                                Preferences.debug("tagID = Date mapping table\n");
                                break;
                            case 258:
                                Preferences.debug("tagID = Black value\n");
                                break;
                            case 259:
                                Preferences.debug("tagID = White value\n");
                                break;
                            case 260:
                                Preferences.debug("tagID = Image data mapping auto range\n");
                                break;
                            case 261:
                                Preferences.debug("tagID = Image thumbnail\n");
                                break;
                            case 262:
                                Preferences.debug("tagID = Gamma value\n");
                                break;
                            case 264:
                                Preferences.debug("tagID = Image over exposure\n");
                                break;
                            case 265:
                                Preferences.debug("tagID = Image relative time 1\n");
                                break;
                            case 266:
                                Preferences.debug("tagID = Image relative time 2\n");
                                break;
                            case 267:
                                Preferences.debug("tagID = Image relative time 3\n");
                                break;
                            case 268:
                                Preferences.debug("tagID = Image relative time 4\n");
                                break;
                            case 300:
                                Preferences.debug("tagID = Image relative time\n");
                                break;
                            case 301:
                                Preferences.debug("tagID = Image base time 1\n");
                                break;
                            case 302:
                                Preferences.debug("tagID = Image base time 2\n");
                                break;
                            case 303:
                                Preferences.debug("tagID = Image base time 3\n");
                                break;
                            case 304:
                                Preferences.debug("tagID = Image base time 4\n");
                                break;
                            case 515:
                                Preferences.debug("tagID = Image width in pixels\n");
                                break;
                            case 516:
                                Preferences.debug("tagID = Image height in pixels\n");
                                break;
                            case 517:
                                Preferences.debug("tagID = Image count raw\n");
                                break;
                            case 518:
                                Preferences.debug("tagID = Pixel type\n");
                                if (valueDType == VT_I4) {
                                    displayPixelFormat(intValue);
                                }
                                break;
                            case 519:
                                Preferences.debug("tagID = Number raw images\n");
                                break;
                            case 520:
                                Preferences.debug("tagID = Image size\n");
                                break;
                            case 521:
                                Preferences.debug("tagID = Compression factor for save\n");
                                break;
                            case 522:
                                Preferences.debug("tagID = Document save flags\n");
                                break;
                            case 523:
                                Preferences.debug("tagID = Acquisition pause annotation\n");
                                break;
                            case 530:
                                Preferences.debug("tagID = Document subtype\n");
                                break;
                            case 531:
                                Preferences.debug("tagID = Acquisition bit depth\n");
                                break;
                            case 534:
                                Preferences.debug("tagID = Z-stack single representative\n");
                                break;
                            case 769:
                                Preferences.debug("tagID = Scale factor for X\n");
                                if ((valueDType == VT_R8) && (doubleValue != 1.0)) {
                                    fileInfo.setResolutions((float)doubleValue, 0);
                                }
                                break;
                            case 770:
                                Preferences.debug("tagID = Scale unit for X\n");
                                if (valueDType == VT_I4) {
                                    measureUnits = zviToMipavMeasurementUnits(intValue);
                                    if (measureUnits != FileInfoBase.UNKNOWN_MEASURE) {
                                        fileInfo.setUnitsOfMeasure(measureUnits, 0);
                                    }
                                }
                                break;
                            case 771:
                                Preferences.debug("tagID = Scale width\n");
                                break;
                            case 772:
                                Preferences.debug("tagID = Scale factor for Y\n");
                                if ((valueDType == VT_R8) && (doubleValue != 1.0)) {
                                    fileInfo.setResolutions((float)doubleValue, 1);
                                }
                                break;
                            case 773:
                                Preferences.debug("tagID = Scale unit for Y\n");
                                if (valueDType == VT_I4) {
                                    measureUnits = zviToMipavMeasurementUnits(intValue);
                                    if (measureUnits != FileInfoBase.UNKNOWN_MEASURE) {
                                            fileInfo.setUnitsOfMeasure(measureUnits, 1);
                                    }
                                }
                                break;
                            case 774:
                                Preferences.debug("tagID = Scale height\n");
                                break;
                            case 775:
                                Preferences.debug("tagID = Scale factor for Z\n");
                                if ((valueDType == VT_R8) && (doubleValue != 1.0)) {
                                    fileInfo.setResolutions((float)doubleValue, 2);
                                }
                                break;
                            case 776:
                                Preferences.debug("tagID = Scale unit for Z\n");
                                if (valueDType == VT_I4) {
                                    measureUnits = zviToMipavMeasurementUnits(intValue);
                                    if (measureUnits != FileInfoBase.UNKNOWN_MEASURE) {
                                        fileInfo.setUnitsOfMeasure(measureUnits, 2);
                                    }
                                }
                                break;
                            case 777:
                                Preferences.debug("tagID = Scale depth\n");
                                break;
                            case 778:
                                Preferences.debug("tagID = Scaling parent\n");
                                break;
                            case 1001:
                                Preferences.debug("tagID = Date\n");
                                break;
                            case 1002:
                                Preferences.debug("tagID = Code\n");
                                break;
                            case 1003:
                                Preferences.debug("tagID = Source\n");
                                break;
                            case 1004:
                                Preferences.debug("tagID = Message\n");
                                break;
                            case 1025:
                                Preferences.debug("tagID = Cmaera image acquisition time\n");
                                break;
                            case 1026:
                                Preferences.debug("tagID = 8-bit acquisition\n");
                                break;
                            case 1027:
                                Preferences.debug("tagID = Camera bit depth\n");
                                break;
                            case 1029:
                                Preferences.debug("tagID = Mono reference low\n");
                                break;
                            case 1030:
                                Preferences.debug("tagID = Mono reference high\n");
                                break;
                            case 1031:
                                Preferences.debug("tagID = Red reference low\n");
                                break;
                            case 1032:
                                Preferences.debug("tagID = Red reference high\n");
                                break;
                            case 1033:
                                Preferences.debug("tagID = Green reference low\n");
                                break;
                            case 1034:
                                Preferences.debug("tagID = Green reference high\n");
                                break;
                            case 1035:
                                Preferences.debug("tagID = Blue reference low\n");
                                break;
                            case 1036:
                                Preferences.debug("tagID = Blue reference high\n");
                                break;
                            case 1041:
                                Preferences.debug("tagID = Framegrabber name\n");
                                break;
                            case 1042:
                                Preferences.debug("tagID = Camera\n");
                                break;
                            case 1044:
                                Preferences.debug("tagID = Camera trigger signal type\n");
                                break;
                            case 1045:
                                Preferences.debug("tagID = Camera trigger enable\n");
                                break;
                            case 1046:
                                Preferences.debug("tagID = Grabber timeout\n");
                                break;
                            case 1281:
                                Preferences.debug("tagID = Multichannel enabled\n");
                                break;
                            case 1282:
                                Preferences.debug("tagID = Multichannel color\n");
                                break;
                            case 1283:
                                Preferences.debug("tagID = Multichannel weight\n");
                                break;
                            case 1284:
                                Preferences.debug("tagID = Channel name\n");
                                break;
                            case 1536:
                                Preferences.debug("tagID = Document information group\n");
                                break;
                            case 1537:
                                Preferences.debug("tagID = Title\n");
                                break;
                            case 1538:
                                Preferences.debug("tagID = Author\n");
                                break;
                            case 1539:
                                Preferences.debug("tagID = Keywords\n");
                                break;
                            case 1540:
                                Preferences.debug("tagID = Comments\n");
                                break;
                            case 1541:
                                Preferences.debug("tagID = Sample ID\n");
                                break;
                            case 1542:
                                Preferences.debug("tagID = Subject\n");
                                break;
                            case 1543:
                                Preferences.debug("tagID = Revision number\n");
                                break;
                            case 1544:
                                Preferences.debug("tagID = Save folder\n");
                                break;
                            case 1545:
                                Preferences.debug("tagID = File link\n");
                                break;
                            case 1546:
                                Preferences.debug("tagID = Document type\n");
                                break;
                            case 1547:
                                Preferences.debug("tagID = Storage media\n");
                                break;
                            case 1548:
                                Preferences.debug("tagID = File ID\n");
                                break;
                            case 1549:
                                Preferences.debug("tagID = Reference\n");
                                break;
                            case 1550:
                                Preferences.debug("tagID = File date\n");
                                break;
                            case 1551:
                                Preferences.debug("tagID = File size\n");
                                break;
                            case 1553:
                                Preferences.debug("tagID = Filename\n");
                                break;
                            case 1554:
                                Preferences.debug("tagID = File attributes\n");
                                break;
                            case 1792:
                                Preferences.debug("tagID = Project group\n");
                                break;
                            case 1793:
                                Preferences.debug("tagID = Acquisition date\n");
                                break;
                            case 1794:
                                Preferences.debug("tagID = Last modified by\n");
                                break;
                            case 1795:
                                Preferences.debug("tagID = User company\n");
                                break;
                            case 1796:
                                Preferences.debug("tagID = User company logo\n");
                                break;
                            case 1797:
                                Preferences.debug("tagID = Image\n");
                                break;
                            case 1800:
                                Preferences.debug("tagID = User ID\n");
                                break;
                            case 1801:
                                Preferences.debug("tagID = User name\n");
                                break;
                            case 1802:
                                Preferences.debug("tagID = User city\n");
                                break;
                            case 1803:
                                Preferences.debug("tagID = User address\n");
                                break;
                            case 1804:
                                Preferences.debug("tagID = User country\n");
                                break;
                            case 1805:
                                Preferences.debug("tagID = User phone\n");
                                break;
                            case 1806:
                                Preferences.debug("tagID = User fax\n");
                                break;
                            case 2049:
                                Preferences.debug("tagID = Objective name\n");
                                break;
                            case 2050:
                                Preferences.debug("tagID = Optovar\n");
                                break;
                            case 2051:
                                Preferences.debug("tagID = Reflector\n");
                                break;
                            case 2052:
                                Preferences.debug("tagID = Condneser contrast\n");
                                break;
                            case 2053:
                                Preferences.debug("tagID = Transmitted light filter 1\n");
                                break;
                            case 2054:
                                Preferences.debug("tagID = Transmitted light filter 2\n");
                                break;
                            case 2055:
                                Preferences.debug("tagID = Reflected light shutter\n");
                                break;
                            case 2056:
                                Preferences.debug("tagID = Condenser front lens\n");
                                break;
                            case 2057:
                                Preferences.debug("tagID = Excitation filer name\n");
                                break;
                            case 2060:
                                Preferences.debug("tagID = Transmitted light fieldstop aperture\n");
                                break;
                            case 2061:
                                Preferences.debug("tagID = Reflected light aperture\n");
                                break;
                            case 2062:
                                Preferences.debug("tagID = Condenser N.A.\n");
                                break;
                            case 2063:
                                Preferences.debug("tagID = Light path\n");
                                break;
                            case 2064:
                                Preferences.debug("tagID = Halogen lamp on\n");
                                break;
                            case 2065:
                                Preferences.debug("tagID = Halogen lamp mode\n");
                                break;
                            case 2066:
                                Preferences.debug("tagID = Halogen lamp voltage\n");
                                break;
                            case 2068:
                                Preferences.debug("tagID = Fluorescence lamp level\n");
                                break;
                            case 2069:
                                Preferences.debug("tagID = Fluorsecence lamp intensity\n");
                                break;
                            case 2070:
                                Preferences.debug("tagID = Light manager is enabled\n");
                                break;
                            case 2072:
                                Preferences.debug("tagID = Focus position\n");
                                break;
                            case 2073:
                                Preferences.debug("tagID = Stage position X\n");
                                break;
                            case 2074:
                                Preferences.debug("tagID = Stage position Y\n");
                                break;
                            case 2075:
                                Preferences.debug("tagID = Microscope name\n");
                                break;
                            case 2076:
                                Preferences.debug("tagID = Objective magnification\n");
                                break;
                            case 2077:
                                Preferences.debug("tagID = Objective N.A.\n");
                                break;
                            case 2078:
                                Preferences.debug("tagID = Microscope illumination\n");
                                break;
                            case 2079:
                                Preferences.debug("tagID = External shutter 1\n");
                                break;
                            case 2080:
                                Preferences.debug("tagID = External shutter 2\n");
                                break;
                            case 2081:
                                Preferences.debug("tagID = External shutter 3\n");
                                break;
                            case 2082:
                                Preferences.debug("tagID = External filter wheel 1 name\n");
                                break;
                            case 2083:
                                Preferences.debug("tagID = External filter wheel 2 name\n");
                                break;
                            case 2084:
                                Preferences.debug("tagID = Parfocal correction\n");
                                break;
                            case 2086:
                                Preferences.debug("tagID = External shutter 4\n");
                                break;
                            case 2087:
                                Preferences.debug("tagID = External shutter 5\n");
                                break;
                            case 2088:
                                Preferences.debug("tagID = External shutter 6\n");
                                break;
                            case 2089:
                                Preferences.debug("tagID = External filter wheel 3 name\n");
                                break;
                            case 2090:
                                Preferences.debug("tagID = External filter wheel 4 name\n");
                                break;
                            case 2103:
                                Preferences.debug("tagID = Objective turret position\n");
                                break;
                            case 2104:
                                Preferences.debug("tagID = Objective contrast method\n");
                                break;
                            case 2105:
                                Preferences.debug("tagID = Objective immersion type\n");
                                break;
                            case 2107:
                                Preferences.debug("tagID = Reflector position\n");
                                break;
                            case 2109:
                                Preferences.debug("tagID = Transmitted light filter 1 position\n");
                                break;
                            case 2110:
                                Preferences.debug("tagID = Transmitted light filter 2 position\n");
                                break;
                            case 2112:
                                Preferences.debug("tagID = Excitation filter position\n");
                                break;
                            case 2113:
                                Preferences.debug("tagID = Lamp mirror position (ERSETZT DURCH 241!)\n");
                                break;
                            case 2114:
                                Preferences.debug("tagID = External filter wheel 1 position\n");
                                break;
                            case 2115:
                                Preferences.debug("tagID = External filter wheel 2 position\n");
                                break;
                            case 2116:
                                Preferences.debug("tagID = External filter wheel 3 position\n");
                                break;
                            case 2117:
                                Preferences.debug("tagID = External filter wheel 4 position\n");
                                break;
                            default: Preferences.debug("Unrecognized tagID value = " + tagID + "\n");
                        }
                        
                        dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        if (dType == VT_I4) {
                            Preferences.debug("Expected VT_I4 data type for unused attribute\n");
                        }
                        else {
                            Preferences.debug("dType = " + dType + " instead of expected VT_I4 for unused attribute\n");
                            break trueLoop;
                        }
                        int attribute = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("Ununsed attribute = " + attribute + "\n");
                    } // for (i = 0; i < tokenCount  && bp < b.length - 13; i++)
                    break;
                } // while (true)
            } // if ((lastElementName.equals("Tags")) &&
            
            if ((directoryEntry % maximumDirectoryEntriesPerSector) == 0) {
                if (add128) {
                    directoryStart =  (directoryTable[++dp]+1)*sectorSize + 128;
                }
                else {
                    directoryStart =  (directoryTable[++dp]+1)*sectorSize;    
                }
            }
            
        } // while (true)
        
          return;

    }
    
    private int zviToMipavMeasurementUnits (int zviScalingUnit){
        int measureUnits;
        switch (zviScalingUnit) {
            case 0:
                Preferences.debug("Scaling unit type = no scaling\n");
                measureUnits = FileInfoBase.UNKNOWN_MEASURE;
                break;
            case 72:
                Preferences.debug("Scaling unit type = meter\n");
                measureUnits = FileInfoBase.METERS;
                break;
            case 76:
                Preferences.debug("Scaling unit type = micrometer\n");
                measureUnits = FileInfoBase.MICROMETERS;
                break;
            case 77:
                Preferences.debug("Scaling unit type = nanometer\n");
                measureUnits = FileInfoBase.NANOMETERS;
                break;
            case 81:
                 Preferences.debug("Scaling unit type = inch\n");
                 measureUnits = FileInfoBase.INCHES;
                 break;
            case 84:
                Preferences.debug("Scaling unit type = mil (thousandth of an inch)\n");
                measureUnits = FileInfoBase.MILS;
                break;
            case 136:
                Preferences.debug("Scaling unit type = second\n");
                measureUnits = FileInfoBase.SECONDS;
                break;
            case 139:
                Preferences.debug("Scaling unit type = millisecond\n");
                measureUnits = FileInfoBase.MILLISEC;
                break;
            case 140:
                Preferences.debug("Scaling unit type = microsecond\n");
                measureUnits = FileInfoBase.MICROSEC;
                break;
            case 145:
                Preferences.debug("Scaling unit type = minute\n");
                measureUnits = FileInfoBase.MINUTES;
                break;
            case 146:
                Preferences.debug("Scaling unit type = hour\n");
                measureUnits = FileInfoBase.HOURS;
                break;
            default:
                Preferences.debug("Scaling unit type is an unrecognized " + zviScalingUnit + "\n");
                measureUnits = FileInfoBase.UNKNOWN_MEASURE;     
        }
        return measureUnits;
    } // private int zviToMipavMeasurementUnits (int zviScalingUnit)
    
    
    private void displayPixelFormat(int pixelFormat) {
        switch(pixelFormat) {
            case 1:
                Preferences.debug("Pixel format = 8-bit B, G, R - 3 bytes/pixel\n");
                break;
            case 2:
                Preferences.debug("Pixel format = 8-bit B, G, R, A - 4 bytes/pixel\n");
                break;
            case 3:
                Preferences.debug("Pixel format = 8-bit grayscale\n");
                break;
            case 4:
                Preferences.debug("Pixel format = 16-bit integer\n");
                break;
            case 5:
                Preferences.debug("Pixel format = 32-bit integer\n");
                break;
            case 6:
                Preferences.debug("Pixel format = 32-bit IEEE float\n");
                break;
            case 7:
                Preferences.debug("Pixel format = 64-bit IEEE double\n");
                break;
            case 8:
                Preferences.debug("Pixel format = 16-bit B, G, R - 6 bytes/pixel\n");
                break;
            case 9:
                Preferences.debug("Pixel format = 32-bit B, G, R = 12 bytes/pixel\n");
                break;
            default:
                Preferences.debug("pixelFormat has an unrecognized value = " + pixelFormat + "\n");
        }    
    } // private void displayPixelFormat(int pixelFormat)

    
}
