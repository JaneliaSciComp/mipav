package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import gov.nih.mipav.view.Preferences;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;


/**
 
 */

public class FileCZI extends FileBase {

    /** 8 bit unsigned */
    private static final int Gray8 = 0;

    /** 16 bit unsigned */
    private static final int Gray16 = 1;

    /** 32 bit IEEE float */
    private static final int Gray32Float = 2;

    /** 8 bit triples, representing the color channels blue, green, and red */
    private static final int Bgr24 = 3;

    /** 16 bit triples, representing the color channels blue, green, and red */
    private static final int Bgr48 = 4;

    /** Triple of 4 byte IEEE float, representing the color channels blue, green, and red */
    private static final int Bgr96Float = 8;

    /** 8 bit triples followed by an alpha (transparency) channel */
    private static final int Bgra32 = 9;

    /** 2 x 4 byte IEEE float, representing the real and imaginary part of a complex number */
    private static final int Gray64ComplexFloat = 10;

    /**
     * A triple of 2 x 4 byte IEE float, representing real and imaginary part of a complex number, for the color
     * channels blue, green, and red
     */
    private static final int Bgr192ComplexFloat = 11;

    /** 32 bit integer [planned] */
    private static final int Gray32 = 12;

    /** Double precision floating point [planned] */
    private static final int Gray64 = 13;

    private static final int Uncompressed = 0;

    private static final int LZW = 2;

    private static final int JpgFile = 1;

    private static final int JpegXrFile = 4;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoCZI fileInfo;

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

    private final boolean endianess = FileBase.LITTLE_ENDIAN;

    /** DOCUMENT ME! */
    // private ModelLUT LUT = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * LIFF reader/writer constructor.
     * 
     * @param fileName file name
     * @param fileDir file directory
     * 
     * @exception IOException if there is an error making the file
     */
    public FileCZI(final String fileName, final String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    @Override
    public void finalize() {
        fileName = null;
        fileDir = null;
        fileInfo = null;
        file = null;
        image = null;
        imageExtents = null;
        imgBuffer = null;
        imgResols = null;
        // LUT = null;

        try {
            super.finalize();
        } catch (final Throwable er) {}
    }

    /**
     * Accessor that returns the file info.
     * 
     * @return FileInfoBase containing the file info
     */
    public FileInfoBase getFileInfo() {
        return fileInfo;
    }

    /**
     * Accessor that returns the image buffer.
     * 
     * @return buffer of image.
     */
    public float[] getImageBuffer() {
        return imgBuffer;
    }

    /**
     * Rreturns LUT if defined.
     * 
     * @return the LUT if defined else it is null
     */
    /*
     * public ModelLUT getModelLUT() { return LUT; }
     */

    /**
     * Reads the CZI header. This method then opens a Model of an image.
     * 
     * @param multiFile <code>true</code> if a set of files each containing a separate 2D image is present <code>
     *                        false</code> if one file with either a 2D image or a stack of 2D images
     * @param one <code>true</code> if only want to read in one image of the 3D set
     * 
     * @return returns the image
     * 
     * @exception IOException if there is an error reading the file
     */
    public ModelImage readImage(final boolean multiFile, final boolean one) throws IOException {
        long fileLength;
        long allocatedSize;
        long usedSize;
        long position;
        int major;
        int minor;
        int reserved1;
        int reserved2;
        long primaryFileGuid;
        long primaryFileGuid2;
        long fileGuid;
        long fileGuid2;
        int filePart;
        long directoryPosition = 0;
        long metadataPosition = 0;
        int updatePending;
        long attachmentDirectoryPosition = 0;
        int xmlSize;
        int attachmentSize;
        String xmlData;
        int metadataSize;
        long dataSize;
        String schemaType;
        int pixelType;
        final String typeString[] = new String[14];
        typeString[0] = new String("Gray8");
        typeString[1] = new String("Gray16");
        typeString[2] = new String("Gray32Float");
        typeString[3] = new String("Bgr24");
        typeString[4] = new String("Bgr48");
        typeString[8] = new String("Bgr96Float");
        typeString[9] = new String("Bgra32");
        typeString[10] = new String("Gray64ComplexFloat");
        typeString[11] = new String("Bgr192ComplexFloat");
        typeString[12] = new String("Gray32");
        typeString[13] = new String("Gray64");
        int dataType;
        long filePosition;
        int compression;
        try {
            imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");

            fileLength = raFile.length();
            Preferences.debug("fileLength = " + fileLength + "\n", Preferences.DEBUG_FILEIO);
            boolean readSegment = true;
            position = raFile.getFilePointer();
            while (readSegment) {
                // Read a unique 16 byte ANSI -character ID, each prefixed with "ZISRAW"
                final String charID = getString(16);
                // The special name "DELETED" marks a segment as deleted - readers should ignore or skip this segment.
                if (charID.trim().equals("DELETED")) {
                    Preferences.debug("Character ID field = DELETED\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("Segment being skipped\n", Preferences.DEBUG_FILEIO);
                    allocatedSize = readLong(endianess);
                    if ( (allocatedSize % 32) != 0) {
                        Preferences.debug("Illegal allocated size = " + allocatedSize + " instead of required multiple of 32\n", Preferences.DEBUG_FILEIO);
                        raFile.close();
                        throw new IOException("Illegal allocated size = " + allocatedSize + " instead of required multiple of 32");
                    }
                    Preferences.debug("Allocated size = " + allocatedSize + "\n", Preferences.DEBUG_FILEIO);
                    position += (allocatedSize + 32);
                    if (position < fileLength) {
                        raFile.seek(position);
                        Preferences.debug("position = " + position + "\n", Preferences.DEBUG_FILEIO);
                        continue;
                    } else {
                        break;
                    }
                }
                if ( !charID.startsWith("ZISRAW")) {
                    Preferences.debug("Illegal character ID field without required ZISRAW start = " + charID + "\n", Preferences.DEBUG_FILEIO);
                    raFile.close();
                    throw new IOException("Illegal character ID field without required ZISRAW start = " + charID);
                }
                Preferences.debug("Character ID field = " + charID + "\n", Preferences.DEBUG_FILEIO);
                allocatedSize = readLong(endianess);
                if ( (allocatedSize % 32) != 0) {
                    Preferences.debug("Illegal allocated size = " + allocatedSize + " instead of required multiple of 32\n", Preferences.DEBUG_FILEIO);
                    raFile.close();
                    throw new IOException("Illegal allocated size = " + allocatedSize + " instead of required multiple of 32");
                }
                Preferences.debug("Allocated size = " + allocatedSize + "\n", Preferences.DEBUG_FILEIO);
                usedSize = readLong(endianess);
                if (usedSize > allocatedSize) {
                    Preferences.debug("Illegal used size = " + usedSize + " greater than allocatedSize = " + allocatedSize + "\n", Preferences.DEBUG_FILEIO);
                    raFile.close();
                    throw new IOException("Illegal used size = " + usedSize + " greater than allocatedSize = " + allocatedSize);
                }
                if (usedSize == 0) {
                    usedSize = allocatedSize;
                }
                Preferences.debug("Used size = " + usedSize + "\n", Preferences.DEBUG_FILEIO);
                if (charID.trim().equals("ZISRAWFILE")) {
                    // File header segment
                    major = readInt(endianess);
                    if (major == 1) {
                        Preferences.debug("Major in file header = 1 as expected\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Major in file header = " + major + " instead of the expected 1\n", Preferences.DEBUG_FILEIO);
                    }
                    minor = readInt(endianess);
                    if (minor == 0) {
                        Preferences.debug("Minor in file header = 0 as expected\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Major in file header = " + minor + " instead of the expected 0\n", Preferences.DEBUG_FILEIO);
                    }
                    reserved1 = readInt(endianess);
                    reserved2 = readInt(endianess);
                    // Single file: primaryFileGuid and the fileGuid are identical. The filePart is 0.
                    // Multi file: In the master file, the primaryFileGuid and the fileGuid are identical. In file
                    // parts, the
                    // primaryFileGuid is the guid of the master file and fileParts > 0.
                    // 16 byte primaryFileGuid but Java long is only 8 bytes
                    primaryFileGuid = readLong(endianess);
                    primaryFileGuid2 = readLong(endianess);
                    // 16 byte fileGuid but Java long is only 8 bytes
                    fileGuid = readLong(endianess);
                    fileGuid2 = readLong(endianess);
                    filePart = readInt(endianess);
                    Preferences.debug("File part number = " + filePart + "\n", Preferences.DEBUG_FILEIO);
                    directoryPosition = readLong(endianess);
                    Preferences.debug("File position of the Directory Segment = " + directoryPosition + "\n", Preferences.DEBUG_FILEIO);
                    metadataPosition = readLong(endianess);
                    Preferences.debug("File position of the Metadata Segment = " + metadataPosition + "\n", Preferences.DEBUG_FILEIO);
                    updatePending = readInt(endianess);
                    if (updatePending == 0) {
                        Preferences.debug("No update is pending\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Update pending flag = " + updatePending + "\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("This flag indicates an update pending\n", Preferences.DEBUG_FILEIO);
                    }
                    attachmentDirectoryPosition = readLong(endianess);
                    Preferences.debug("File position of the Attachment Directory Segment = " + attachmentDirectoryPosition + "\n", Preferences.DEBUG_FILEIO);
                } // if (charID.trim().equals("ZISRAWFILE"))
                else if (charID.trim().equals("ZISRAWDIRECTORY")) {
                    if (directoryPosition != 0) {
                        Preferences.debug("File header gave Directory Segment position as " + directoryPosition + "\n", Preferences.DEBUG_FILEIO);
                    }
                    Preferences.debug("Actual Directory Segment position = " + position + "\n", Preferences.DEBUG_FILEIO);
                } // else if (charID.trim().equals("ZISRAWDIRECTORY"))
                else if (charID.trim().equals("ZISRAWMETADATA")) {
                    if (metadataPosition != 0) {
                        Preferences.debug("File header gave Metadata Segment position as " + metadataPosition + "\n", Preferences.DEBUG_FILEIO);
                    }
                    Preferences.debug("Actual Metadata Segment position = " + position + "\n", Preferences.DEBUG_FILEIO);
                    xmlSize = readInt(endianess);
                    Preferences.debug("Size of the xml data = " + xmlSize + "\n", Preferences.DEBUG_FILEIO);
                    attachmentSize = readInt(endianess);
                    // Size of the (binary) attachments. NOT USED CURRENTLY
                    Preferences.debug("Currently unused size of binary attachments = " + attachmentSize + "\n", Preferences.DEBUG_FILEIO);
                    xmlData = getString(xmlSize);
                    Preferences.debug("XML data: \n", Preferences.DEBUG_FILEIO);
                    Preferences.debug(xmlData + "\n", Preferences.DEBUG_FILEIO);
                } // else if (charID.trim().equals("ZISRAWMETADATA"))
                else if (charID.trim().equals("ZISRAWATTDIR")) {
                    if (attachmentDirectoryPosition != 0) {
                        Preferences.debug("File header gave Attachment Directory Segment position as " + attachmentDirectoryPosition + "\n",
                                Preferences.DEBUG_FILEIO);
                    }
                    Preferences.debug("Actual Attachment Directory Segment position = " + position + "\n", Preferences.DEBUG_FILEIO);
                } // else if (charID.trim().equals("ZISRAWATTDIR"))
                else if (charID.trim().equals("ZISRAWSUBBLOCK")) {
                    metadataSize = readInt(endianess);
                    Preferences.debug("Size of the SubBlock metadata section = " + metadataSize + "\n", Preferences.DEBUG_FILEIO);
                    attachmentSize = readInt(endianess);
                    Preferences.debug("Size of the SubBlock optional attachment section = " + attachmentSize + "\n", Preferences.DEBUG_FILEIO);
                    dataSize = readLong(endianess);
                    Preferences.debug("Size of the SubBlock data section = " + dataSize + "\n", Preferences.DEBUG_FILEIO);
                    schemaType = getString(2);
                    if (schemaType.equals("DV")) {
                        Preferences.debug("Directory Entry DV Schema Type = DV as expected\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Directory Entry DV Schema Type = " + schemaType + " instead of the expected DV\n", Preferences.DEBUG_FILEIO);
                        raFile.close();
                        throw new IOException("Directory Entry DV Schema Type = " + schemaType + " instead of the expected DV");
                    }
                    pixelType = readInt(endianess);
                    Preferences.debug("CZI pixelType = " + typeString[pixelType] + "\n", Preferences.DEBUG_FILEIO);
                    if (pixelType == Gray8) {
                        dataType = ModelStorageBase.UBYTE;
                        Preferences.debug("MIPAV data type = UBYTE\n", Preferences.DEBUG_FILEIO);
                    } else if (pixelType == Gray16) {
                        dataType = ModelStorageBase.USHORT;
                        Preferences.debug("MIPAV data type = USHORT\n", Preferences.DEBUG_FILEIO);
                    } else if (pixelType == Gray32Float) {
                        dataType = ModelStorageBase.FLOAT;
                        Preferences.debug("MIPAV data type = FLOAT\n", Preferences.DEBUG_FILEIO);
                    } else if (pixelType == Bgr24) {
                        dataType = ModelStorageBase.ARGB;
                        Preferences.debug("MIPAV data type = ARGB\n", Preferences.DEBUG_FILEIO);
                    } else if (pixelType == Bgr48) {
                        dataType = ModelStorageBase.ARGB_USHORT;
                        Preferences.debug("MIPAV data type = ARGB_USHORT\n", Preferences.DEBUG_FILEIO);
                    } else if (pixelType == Bgr96Float) {
                        dataType = ModelStorageBase.ARGB_FLOAT;
                        Preferences.debug("MIPAV data type = ARGB_FLOAT\n", Preferences.DEBUG_FILEIO);
                    } else if (pixelType == Bgra32) {
                        dataType = ModelStorageBase.ARGB;
                        Preferences.debug("MIPAV data type = ARGB\n", Preferences.DEBUG_FILEIO);
                    } else if (pixelType == Gray64ComplexFloat) {
                        dataType = ModelStorageBase.COMPLEX;
                        Preferences.debug("MIPAV data type = COMPLEX\n", Preferences.DEBUG_FILEIO);
                    } else if (pixelType == Bgr192ComplexFloat) {
                        Preferences.debug("Bgr192ComplexFloat has no corresponding MIPAV data type\n", Preferences.DEBUG_FILEIO);
                        raFile.close();
                        throw new IOException("Bgr192ComplexFloat has no corresponding MIPAV data type");
                    } else if (pixelType == Gray32) {
                        dataType = ModelStorageBase.INTEGER;
                        Preferences.debug("MIPAV data type = INTEGER\n", Preferences.DEBUG_FILEIO);
                    } else if (pixelType == Gray64) {
                        dataType = ModelStorageBase.DOUBLE;
                        Preferences.debug("MIPAV data type = DOUBLE\n", Preferences.DEBUG_FILEIO);
                    }
                    filePosition = readLong(endianess);
                    Preferences.debug("Seek offset of the referenced SubBlockSegment relative to the first byte of the file = " + filePosition + "\n",
                            Preferences.DEBUG_FILEIO);
                    // Reserved
                    filePart = readInt(endianess);
                    compression = readInt(endianess);
                    if (compression == Uncompressed) {
                        Preferences.debug("Compression = Uncompressed\n", Preferences.DEBUG_FILEIO);
                    } else if (compression == LZW) {
                        Preferences.debug("Compression = LZW\n", Preferences.DEBUG_FILEIO);
                    } else if (compression == JpgFile) {
                        Preferences.debug("Compression == JpgFile\n", Preferences.DEBUG_FILEIO);
                    } else if (compression == JpegXrFile) {
                        Preferences.debug("Compression == JpegXrFile\n", Preferences.DEBUG_FILEIO);
                    } else if ( (compression >= 100) && (compression <= 999)) {
                        Preferences.debug("Compression = Camera specific RAW data\n", Preferences.DEBUG_FILEIO);
                    } else if (compression >= 1000) {
                        Preferences.debug("Compression = System specific RAW data\n", Preferences.DEBUG_FILEIO);
                    }
                } // else if (charID.trim().equals("ZISRAWSUBBLOCK"))
                position += (allocatedSize + 32);
                if (position < fileLength) {
                    raFile.seek(position);
                    Preferences.debug("location = " + position + "\n", Preferences.DEBUG_FILEIO);
                } else {
                    readSegment = false;
                }
            }

            image.calcMinMax();
            fireProgressStateChanged(100);

        } catch (final OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw error;
        }

        return image;
    }

}
