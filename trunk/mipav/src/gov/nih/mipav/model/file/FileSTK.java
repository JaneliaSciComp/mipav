package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * The MetaMorph Stack (STK) file format is derived from the TIFF format. Note that the IMAGE_DESCRIPTION and
 * STRIP_OFFSETS tags are handled differently in STK file format than in TIFF file format. Also note that the UIC1Tag,
 * UIC2Tag, UIC3Tag, and UIC4Tag are found in STK but not in TIFF. While TIFF files can be either big-endian or
 * little-endian, STK files must be little endian. Only packed bit compression is supported at this time. Note that
 * although EchoTech has a tResolution field, there is no provision for 4D in TIFF.
 *
 * @see  FileIO
 * @see  FileInfoTiff
 * @see  FileRaw
 */

public class FileSTK extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** TIFF Types. */
    public static final int BYTE = 1; // 8  bit unsigned

    /** DOCUMENT ME! */
    public static final int ASCII = 2; // 7  bit ASCII

    /** DOCUMENT ME! */
    public static final int SHORT = 3; // 16 bit unsigned

    /** DOCUMENT ME! */
    public static final int LONG = 4; // 32 bit unsigned    ****** 4 bytes !!!!

    /** DOCUMENT ME! */
    public static final int RATIONAL = 5; // 2  longs 1st numorator
                                          // 2nd denom

    /** DOCUMENT ME! */
    public static final int SBYTE = 6; // 8 bit signed

    /** DOCUMENT ME! */
    public static final int UNDEFINED = 7; // 8 bit undefined

    /** DOCUMENT ME! */
    public static final int SSHORT = 8; // 16 bit signed

    /** DOCUMENT ME! */
    public static final int SLONG = 9; // 32 bit signed

    /** DOCUMENT ME! */
    public static final int SRATIONAL = 10; //

    /** DOCUMENT ME! */
    public static final int FLOAT = 11; // single precision 4 byte IEEE format

    /** DOCUMENT ME! */
    public static final int DOUBLE = 12; // double precision 8 byte IEEE format

    /** TIFF Tags. */
    public static final int NEW_SUBFILE_TYPE = 254;

    /** DOCUMENT ME! */
    public static final int IMAGE_WIDTH = 256;

    /** DOCUMENT ME! */
    public static final int IMAGE_LENGTH = 257;

    /** DOCUMENT ME! */
    public static final int BITS_PER_SAMPLE = 258;

    /** DOCUMENT ME! */
    public static final int COMPRESSION = 259; // 1=no compression, 2=modified huffman,
                                               // 3 = CCITT-T4, 4 = CCITT-T6, 5 = LZW, 32773 = packbits

    /** DOCUMENT ME! */
    public static final int PHOTO_INTERP = 262;

    /** DOCUMENT ME! */
    public static final int IMAGE_DESCRIPTION = 270;

    /** DOCUMENT ME! */
    public static final int STRIP_OFFSETS = 273;

    /** DOCUMENT ME! */
    public static final int SAMPLES_PER_PIXEL = 277;

    /** DOCUMENT ME! */
    public static final int ROWS_PER_STRIP = 278;

    /** DOCUMENT ME! */
    public static final int STRIP_BYTE_COUNTS = 279;

    /** DOCUMENT ME! */
    public static final int MIN_SAMPLE_VALUE = 280;

    /** DOCUMENT ME! */
    public static final int MAX_SAMPLE_VALUE = 281;

    /** DOCUMENT ME! */
    public static final int XRESOLUTION = 282;

    /** DOCUMENT ME! */
    public static final int YRESOLUTION = 283;

    /** DOCUMENT ME! */
    public static final int PLANAR_CONFIG = 284;

    /** DOCUMENT ME! */
    public static final int RESOLUTION_UNIT = 296;

    /** DOCUMENT ME! */
    public static final int SOFTWARE = 305;

    /** DOCUMENT ME! */
    public static final int DATE_TIME = 306;

    /** DOCUMENT ME! */
    public static final int PREDICTOR = 317;

    /** DOCUMENT ME! */
    public static final int COLOR_MAP = 320;

    /** DOCUMENT ME! */
    public static final int TILE_WIDTH = 322;

    /** DOCUMENT ME! */
    public static final int TILE_LENGTH = 323;

    /** DOCUMENT ME! */
    public static final int TILE_OFFSETS = 324;

    /** DOCUMENT ME! */
    public static final int TILE_BYTE_COUNTS = 325;

    /** DOCUMENT ME! */
    public static final int SAMPLE_FORMAT = 339;

    /** DOCUMENT ME! */
    public static final int UIC1Tag = 33628;

    /** DOCUMENT ME! */
    public static final int UIC2Tag = 33629;

    /** DOCUMENT ME! */
    public static final int UIC3Tag = 33630;

    /** DOCUMENT ME! */
    public static final int UIC4Tag = 33631;

    /** EchoTech Tiff TAGS. */
    public static final int ZRESOLUTION = 65000;

    /** DOCUMENT ME! */
    public static final int TRESOLUTION = 65001;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private byte[] byteBuffer = null;

    /** DOCUMENT ME! */
    private boolean chunky = true;

    @SuppressWarnings("unchecked")
    private Vector<Index>[] dataOffsets = new Vector[5000];

    /** DOCUMENT ME! */
    private byte[] dateTime;

    /** DOCUMENT ME! */
    private boolean doTile = false; // true if tiles are used

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoSTK fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private FilePackBit filePB;

    /** DOCUMENT ME! */
    private FileRawChunk fileRW;

    /** DOCUMENT ME! */
    private int[] IFDoffsets = new int[2048];

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private byte[] imageDescription;

    /** DOCUMENT ME! */
    private int imageSlice = 0;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgResols;

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    /** DOCUMENT ME! */
    private int LUTOffset;

    /** DOCUMENT ME! */
    private short nDirEntries;

    /** DOCUMENT ME! */
    private int numberImages = 1; // given by count field of UIC2Tag

    /** DOCUMENT ME! */
    private int offsetConstant;

    /** DOCUMENT ME! */
    private boolean packBit = false; // true if the read data file has pack bit compression

    /** DOCUMENT ME! */
    private int planeOffset;

    /** DOCUMENT ME! */
    private int samplesPerPixel = 1;

    /** DOCUMENT ME! */
    private byte[] software;

    /** DOCUMENT ME! */
    private String str;

    /** DOCUMENT ME! */
    private int[] tileByteCounts;

    /** DOCUMENT ME! */
    private int tileByteNumber;

    /** DOCUMENT ME! */
    private int tileLength;

    /** DOCUMENT ME! */
    private int tileOffsetNumber;

    /** DOCUMENT ME! */
    private int[] tileOffsets;

    /** DOCUMENT ME! */
    private int tilesAcross;

    /** DOCUMENT ME! */
    private int tilesDown;

    /** DOCUMENT ME! */
    private int tilesPerImage;

    /** DOCUMENT ME! */
    private int tilesPerSlice;

    /** DOCUMENT ME! */
    private int[] tileTemp;

    /** DOCUMENT ME! */
    private int tileWidth;

    /** DOCUMENT ME! */
    private double tRes = 1.0;

    /** DOCUMENT ME! */
    private int xDim = 0;

    /** DOCUMENT ME! */
    private int yDim = 0;

    /** DOCUMENT ME! */
    private double zRes = 1.0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * STK reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileSTK(String fileName, String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        int i;
        fileName = null;
        fileDir = null;
        fileInfo = null;
        file = null;
        image = null;
        byteBuffer = null;
        if (dataOffsets != null) {
            for (i = 0; i < dataOffsets.length; i++) {
                if (dataOffsets[i] != null) {
                    dataOffsets[i].removeAllElements();
                    dataOffsets[i] = null;
                }
            }
            dataOffsets = null;
        }
        dateTime = null;

        if (filePB != null) {
            filePB.finalize();
            filePB = null;
        }
        
        if (fileRW != null) {

            try {
                fileRW.close();
                // System.err.println("closed FileSTK: fileRW (FileRawChunk)");
            } catch (IOException ex) { }

            fileRW.finalize();
            fileRW = null;
        }

        IFDoffsets = null;
        imageDescription = null;
        imgBuffer = null;
        imgResols = null;
        LUT = null;
        software = null;
        str = null;
        tileByteCounts = null;
        tileOffsets = null;
        tileTemp = null;
       
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
     * Reads the Tiff header which indicates endianess, the TIFF magic number, and the offset in bytes of the first IFD.
     * It then reads all the IFDs. This method then opens a Model of an image and imports the the images one slice at a
     * time. Image slices are separated by an IFD.
     *
     * @param      multiFile  <code>true</code> if a set of files each containing a separate 2D image is present <code>
     *                        false</code> if one file with either a 2D image or a stack of 2D images
     * @param      one        DOCUMENT ME!
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean multiFile, boolean one) throws IOException {
        int[] imgExtents;
        int i, j;
        int nIndex;

        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");

            short byteOrder = raFile.readShort();

            if (byteOrder == 0x4949) {
                endianess = FileBase.LITTLE_ENDIAN;
            } else if (byteOrder == 0x4d4d) {
                endianess = FileBase.BIG_ENDIAN;
                throw new IOException("Illegal STK BIG_ENDIAN byte order");
            } else {
                raFile.close();
                throw new IOException("STK Read Header: Error - first 2 bytes are an illegal " + byteOrder);
            }

            int magicTIFFNumber = getUnsignedShort(endianess);

            if (magicTIFFNumber != 42) {
                raFile.close();
                throw new IOException("STK Read Header: Error - Invalid Magic number = " + magicTIFFNumber);
            }

            long saveLoc = raFile.getFilePointer();

            fileInfo = new FileInfoSTK(fileName, fileDir, FileUtility.STK); // dummy fileInfo
            fileInfo.setEndianess(endianess);

            imageSlice = 0;
            IFDoffsets[imageSlice] = getInt(endianess);

            boolean moreIFDs = true;
            imgResols = new float[5];

            Preferences.debug("\n ************** FileSTK.openIFD: Opening = " + fileName + "\n", Preferences.DEBUG_FILEIO);
            tileOffsetNumber = 0;
            tileByteNumber = 0;

            while (moreIFDs) { // Find number of images!!
                raFile.seek(IFDoffsets[imageSlice]);
                moreIFDs = openIFD(fileInfo);
            }

            Preferences.debug("Just past init IFD read", Preferences.DEBUG_FILEIO);

            if (doTile) {
                tilesPerSlice = tilesAcross * tilesDown;
                imageSlice = tilesPerImage / tilesPerSlice;
            } // if (doTile)

            if (numberImages > 1) {
                imageSlice = numberImages;
                nIndex = dataOffsets[0].size();
                offsetConstant = ((Index) (dataOffsets[0].elementAt(nIndex - 1))).index +
                                 ((Index) (dataOffsets[0].elementAt(nIndex - 1))).byteCount -
                                 ((Index) (dataOffsets[0].elementAt(0))).index;

                for (i = 1; i < numberImages; i++) {
                    planeOffset = i * offsetConstant;
                    dataOffsets[i] = new Vector<Index>();

                    for (j = 0; j < nIndex; j++) {
                        dataOffsets[i].addElement(new Index(planeOffset +
                                                            ((Index) (dataOffsets[0].elementAt(j))).index));
                        ((Index) (dataOffsets[i].elementAt(j))).byteCount = ((Index) (dataOffsets[0].elementAt(j))).byteCount;
                    }
                }
            } // if (numberImages > 1)

            imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;


            Preferences.debug("imageSlice = " + imageSlice, Preferences.DEBUG_FILEIO);

            if (imageSlice > 1) {

                imgExtents = new int[3];
                imgExtents[0] = xDim;
                imgExtents[1] = yDim;
                imgExtents[2] = imageSlice;
            } else {
                imgExtents = new int[2];
                imgExtents[0] = xDim;
                imgExtents[1] = yDim;
            }

            fileInfo.setExtents(imgExtents);

            if (multiFile == false) {

                if (one) {
                    image = new ModelImage(fileInfo.getDataType(), new int[] { imgExtents[0], imgExtents[1] },
                                           fileInfo.getFileName());
                } else {
                    image = new ModelImage(fileInfo.getDataType(), imgExtents, fileInfo.getFileName());
                }
            }

            imageSlice = 0;
            raFile.seek(saveLoc);

            moreIFDs = true;
            i = 0;
            tileOffsetNumber = 0;
            tileByteNumber = 0;

            while (moreIFDs) {
                fileInfo = new FileInfoSTK(fileName, fileDir, FileUtility.STK);
                fileInfo.setExtents(imgExtents);
                raFile.seek(IFDoffsets[imageSlice]);
                moreIFDs = openIFD(fileInfo);

                // Set the resolutions
                fileInfo.setResolutions(imgResols);
                fileInfo.setUnitsOfMeasure(Unit.MICROMETERS.getLegacyNum(), 0);
                fileInfo.setUnitsOfMeasure(Unit.MICROMETERS.getLegacyNum(), 1);

                if (imgExtents.length > 2) {
                    fileInfo.setUnitsOfMeasure(Unit.MICROMETERS.getLegacyNum(), 2);
                }

                if ((multiFile == false) && (one == false)) {
                    image.setFileInfo(fileInfo, i);
                }

                i++;
            }

            if (one) {
                imageSlice = numberImages = 1;
                image.setFileInfo(fileInfo, 0);
            }

            if (numberImages > 1) {
                imageSlice = numberImages;

                for (i = 1; i < numberImages; i++) {
                    image.setFileInfo(fileInfo, i);
                }
            }

            if (doTile) {
                imageSlice = tilesPerImage / tilesPerSlice;
            }

            int bufferSize;

            if (ModelImage.isColorImage(fileInfo.getDataType())) {
                bufferSize = xDim * yDim * 4;
            } else {
                bufferSize = xDim * yDim;
            }

            if (imgBuffer == null) {
                imgBuffer = new float[bufferSize];
            }

            for (i = 0; i < imageSlice; i++) {

                try {

                    if (one && (imgExtents.length > 2)) {
                        i = imgExtents[2] / 2;
                    }

                    if (doTile) {
                        readTileBuffer(i, imgBuffer);
                    } else {
                        readBuffer(i, imgBuffer); // Slice a time;
                    }

                    if (one) {
                        i = 0;
                    }
                } catch (IOException error) {
                    throw new IOException("FileSTK: write: " + error);
                }

                if (multiFile == false) {
                    image.importData(i * bufferSize, imgBuffer, false);
                }
            }

            raFile.close();

        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            byteBuffer = null;
            System.gc();
            throw error;
        }

        return image;
    }

    /**
     * Accessor to set the file name (used when reading TIFF multiFile).
     *
     * @param  fName  file name of image to read.
     */
    public void setFileName(String fName) {
        fileName = fName;
    }


    /**
     * This method writes a STK image file.
     *
     * @param      image    image model where the data is stored.
     * @param      LUT      LUT to be saved with image if not null.
     * @param      options  DOCUMENT ME!
     *
     * @exception  IOException  if there is an error writing the file.
     */
    public void writeImage(ModelImage image, ModelLUT LUT, FileWriteOptions options) throws IOException {
        int k, s, sEnd = 1, sBegin = 0;
        int seq;
        int imgOffset;
        int nextIFD;
        int m;
        int type;
        int[] extents;
        int bufferSize;
        int bytesPerSample;
        int samplesPerPixel;
        int resolutionCount = 16; // xResolution =  2 * (4 bytes) + yResolution =  2 * (4 bytes)
        int rgbCount = 0; // Set to 6 for storage of 8,8,8 bits per sample values in 6 bytes for RGB model.
        int intAlign = 0; // For integer data image rows must begin on integer boundaries, so set
                          // intAlign = 2 if ModelStorageBase.INTEGER is used so that imgOffset is
                          // always a multiple of 4.
        int zResCount = 0; // set to 8 if EchoTech zResolution field present
        int tResCount = 0; // set to 8 if EchoTech tResolution field present
        int ztEntries = 0;
        int index;
        int timeOffset = 0; // used to offset for writing one time slice of a 4D image
        boolean oneFile = true;
        @SuppressWarnings("unused")
        int offset = 0;
        String prefix, fileSuffix;
        int stripCount, totStripCount;


        this.image = image;
        this.LUT = LUT;

        try {
            extents = image.getExtents();

            if (image.getNDims() == 1) {
                throw new IOException("image.getNDims returned 1");
            }

            bufferSize = extents[0] * extents[1];

            if (image.getNDims() == 5) {
                offset = extents[3] * extents[2] * extents[1] * extents[0];
            } else if (image.getNDims() == 4) {
                offset = extents[2] * extents[1] * extents[0];
            } else if (image.getNDims() == 3) {
                offset = extents[1] * extents[0];
            }

            if (image.getNDims() >= 3) {

                if (options.isMultiFile()) {
                    oneFile = false;
                    sBegin = options.getBeginSlice();
                    sEnd = options.getEndSlice() + 1;
                } else {
                    sBegin = 0;
                    sEnd = 1;
                    oneFile = true;
                }

                if (image.getNDims() == 4) {
                    timeOffset = options.getTimeSlice() * image.getExtents()[2] * bufferSize;
                }
            }

            index = fileName.indexOf(".");
            prefix = fileName.substring(0, index); // Used for setting file name
            fileSuffix = fileName.substring(index);

            if (image.getFileInfo(0).getResolutions().length > 2) {
                zRes = (double) (image.getFileInfo(0).getResolutions()[2]);
            }

            if (image.getFileInfo(0).getResolutions().length > 3) {
                tRes = (double) (image.getFileInfo(0).getResolutions()[3]);
            }

            if (zRes >= 0.0) {
                zResCount = 8;
                ztEntries++;
            }

            if (tRes >= 0.0) {
                tResCount = 8;
                ztEntries++;
            }

            for (s = sBegin, seq = options.getStartNumber(); s < sEnd; s++, seq++) {

                if (oneFile) {
                    file = new File(fileDir + fileName);
                    raFile = new RandomAccessFile(file, "rw");
                    raFile.setLength(0); // necessary so that if this is an overwritten file there isn't junk at the end

                    if (options.isWritePackBit() == false) {
                        fileRW = new FileRawChunk(raFile, image.getFileInfo(0));
                    } else {
                        filePB = new FilePackBit(raFile);
                    }
                } else {

                    if (options.isSaveAs()) {

                        if (options.getDigitNumber() == 1) {
                            file = new File(fileDir + prefix + Integer.toString(seq) + fileSuffix);
                        } else if (options.getDigitNumber() == 2) {

                            if (seq < 10) {
                                file = new File(fileDir + prefix + "0" + Integer.toString(seq) + fileSuffix);
                            } else {
                                file = new File(fileDir + prefix + Integer.toString(seq) + fileSuffix);
                            }
                        } else if (options.getDigitNumber() == 3) {

                            if (seq < 10) {
                                file = new File(fileDir + prefix + "00" + Integer.toString(seq) + fileSuffix);
                            } else if (seq < 100) {
                                file = new File(fileDir + prefix + "0" + Integer.toString(seq) + fileSuffix);
                            } else {
                                file = new File(fileDir + prefix + Integer.toString(seq) + fileSuffix);
                            }
                        } else if (options.getDigitNumber() == 4) {

                            if (seq < 10) {
                                file = new File(fileDir + prefix + "000" + Integer.toString(seq) + fileSuffix);
                            } else if (seq < 100) {
                                file = new File(fileDir + prefix + "00" + Integer.toString(seq) + fileSuffix);
                            } else if (seq < 1000) {
                                file = new File(fileDir + prefix + "0" + Integer.toString(seq) + fileSuffix);
                            } else {
                                file = new File(fileDir + prefix + Integer.toString(seq) + fileSuffix);
                            }
                        }
                    } else {
                        file = new File(fileDir + image.getFileInfo(s).getFileName());
                    }

                    raFile = new RandomAccessFile(file, "rw");
                    raFile.setLength(0); // necessary so that if this is an overwritten file there isn't junk at the end

                    if (options.isWritePackBit() == false) {
                        fileRW = new FileRawChunk(raFile, image.getFileInfo(s));
                    } else {
                        filePB = new FilePackBit(raFile);
                    }
                }

                type = image.getFileInfo(0).getDataType();

                switch (type) {

                    case ModelStorageBase.BOOLEAN:
                        bytesPerSample = 1;
                        samplesPerPixel = 1;
                        nDirEntries = (short) (10 + ztEntries);
                        rgbCount = 0;
                        intAlign = 0;
                        break;

                    case ModelStorageBase.UBYTE:
                    case ModelStorageBase.BYTE:
                        bytesPerSample = 1;
                        samplesPerPixel = 1;
                        intAlign = 0;
                        if (image.getFileInfo(0).getPhotometric() == 3) {
                            nDirEntries = (short) (12 + ztEntries); // Add one for color map

                            // Only one color map for all the images at the end of the file
                            // Only used if color map is saved with image.  Pointer to the
                            // color map at the end of the file.
                            if (oneFile == true) {
                                LUTOffset = 8 +
                                            ((2 + (nDirEntries * 12) + resolutionCount + 4 + intAlign + zResCount +
                                              tResCount) * (options.getEndSlice() - options.getBeginSlice() + 1)) +
                                            (bufferSize * (options.getEndSlice() - options.getBeginSlice() + 1));
                            } else {
                                LUTOffset = 8 +
                                            (2 + (nDirEntries * 12) + resolutionCount + 4 + intAlign + zResCount +
                                             tResCount) + bufferSize;
                            }
                        } else {
                            nDirEntries = (short) (11 + ztEntries);
                        }

                        rgbCount = 0;
                        break;

                    case ModelStorageBase.USHORT:
                    case ModelStorageBase.SHORT:
                        bytesPerSample = 2;
                        samplesPerPixel = 1;
                        nDirEntries = (short) (11 + ztEntries);
                        rgbCount = 0;
                        intAlign = 0;
                        break;

                    case ModelStorageBase.INTEGER:
                    case ModelStorageBase.UINTEGER:
                        bytesPerSample = 4;
                        samplesPerPixel = 1;
                        nDirEntries = (short) (11 + ztEntries);
                        rgbCount = 0;
                        intAlign = 0; // Used to be 2 but this caused problems.
                        break;

                    case ModelStorageBase.ARGB:
                        bytesPerSample = 1; // since SamplesPerPixel is defined as 3 for RGB images
                        samplesPerPixel = 3;
                        nDirEntries = (short) (13 + ztEntries);
                        rgbCount = 6;
                        intAlign = 0;
                        break;

                    default:
                        throw new IOException("Unsupported Image Type");
                }

                writeHeader();
                nextIFD = 8;
                totStripCount = 0;
                imgOffset = 0;

                if (oneFile == true) { // one file with one or more images

                    for (k = options.getBeginSlice(), m = 0; k <= options.getEndSlice(); k++, m++) {

                        fireProgressStateChanged(Math.round((float) (k - options.getBeginSlice() + 1) /
                                                                (options.getEndSlice() - options.getBeginSlice() + 1) *
                                                                100));

                        if (options.isWritePackBit()) {
                            stripCount = filePB.getStripSize(image, timeOffset + (k * bufferSize),
                                                             timeOffset + (k * bufferSize) + bufferSize);
                            totStripCount += stripCount;
                        } else {
                            stripCount = image.getExtents()[0] * image.getExtents()[1];
                        }

                        if (k == options.getEndSlice()) {
                            nextIFD = 0;
                        } else if (!options.isWritePackBit()) {
                            nextIFD += ((2 + (nDirEntries * 12) + resolutionCount + 4 + rgbCount + intAlign +
                                         zResCount + tResCount) + (bufferSize * bytesPerSample * samplesPerPixel));
                        } else if (options.isWritePackBit()) {
                            nextIFD += ((2 + (nDirEntries * 12) + resolutionCount + 4 + rgbCount + intAlign +
                                         zResCount + tResCount) + stripCount);
                        }

                        if (!options.isWritePackBit()) {
                            imgOffset = 8 +
                                        ((m + 1) *
                                             (2 + (nDirEntries * 12) + resolutionCount + 4 + rgbCount + intAlign +
                                                  zResCount + tResCount)) +
                                        (m * bufferSize * bytesPerSample * samplesPerPixel);
                        } else if (options.isWritePackBit()) {
                            imgOffset = 8 +
                                        ((m + 1) *
                                             (2 + (nDirEntries * 12) + resolutionCount + 4 + rgbCount + intAlign +
                                                  zResCount + tResCount)) + totStripCount - stripCount;
                        }

                        writeIFDs(imgOffset, nextIFD, k, stripCount, options.isWritePackBit());

                        try {

                            if (!options.isWritePackBit()) {
                                fileRW.writeImage(image, timeOffset + (k * bufferSize),
                                                  timeOffset + (k * bufferSize) + bufferSize);
                            } else {
                                filePB.writePackBitImage(image, timeOffset + (k * bufferSize),
                                                         timeOffset + (k * bufferSize) + bufferSize);
                            }
                        } catch (IOException error) {
                            throw error;
                        }
                    }
                } else {
                    fireProgressStateChanged(Math.round((float) s / (options.getEndSlice()) * 100));

                    if (options.isWritePackBit()) {
                        stripCount = filePB.getStripSize(image, s * bufferSize, (s * bufferSize) + bufferSize);
                    } else {
                        stripCount = image.getExtents()[0] * image.getExtents()[1];
                    }

                    imgOffset = 8 +
                                (2 + (nDirEntries * 12) + resolutionCount + 4 + rgbCount + intAlign + zResCount +
                                 tResCount);

                    writeIFDs(imgOffset, 0, s, stripCount, options.isWritePackBit());

                    try {

                        if (!options.isWritePackBit()) {
                            fileRW.writeImage(image, s * bufferSize, (s * bufferSize) + bufferSize);
                        } else {
                            filePB.writePackBitImage(image, s * bufferSize, (s * bufferSize) + bufferSize);
                        }
                    } catch (IOException error) {
                        throw error;
                    }

                }

                if ((LUT != null) && ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE)) &&
                        (image.getFileInfo(0).getPhotometric() == 3)) {

                    writeColorMap(LUTOffset);
                }
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            raFile.close();

            throw error;
        }

        raFile.close();

    }

    /**
     * Reads and decodes IFDs (Image File Directory).
     *
     * @param      fileInfo  DOCUMENT ME!
     *
     * @return     DOCUMENT ME!
     *
     * @exception  IOException  if there is an error reading the file
     */
    private boolean openIFD(FileInfoSTK fileInfo) throws IOException {
        int i;
        int i1, j;
        int planeNumber;
        int tag;
        int type;
        int count;
        int ecount;
        long[] valueArray = new long[25000];
        int value_offset;
        int nDirEntries;
        long numerator, denominator;
        float valueFloat = 0.0f;
        double valueDouble = 0.0;
        int zDistanceNumerator;
        int zDistanceDenominator;
        boolean zSameDistance;

        fileInfo.setEndianess(endianess);
        nDirEntries = getUnsignedShort(endianess);

        if (nDirEntries <= 0) {
            throw new IOException("First 2 IFD bytes are an illegal " + nDirEntries);
        }

        Preferences.debug("\nOpenIFD: Entries = " + nDirEntries + "\n", Preferences.DEBUG_FILEIO);

        for (i = 0; i < nDirEntries; i++) {

            tag = getUnsignedShort(endianess);

            if (tag == 0) {
                throw new IOException("Tiff Zero Tag Error");
            }

            type = getUnsignedShort(endianess);
            count = getInt(endianess);

            if ((type == SHORT) && (count == 1)) {
                valueArray[0] = getUnsignedShort(endianess);
                getUnsignedShort(endianess);
            } else if ((type == SHORT) && (count == 2)) {
                valueArray[0] = getUnsignedShort(endianess);
                valueArray[1] = getUnsignedShort(endianess);
            } else if ((type == SHORT) && (count >= 3)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < 25000)); i1++) {
                    valueArray[i1] = getUnsignedShort(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == LONG) && (tag == UIC1Tag)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; i1 < (2 * count); i1 += 2) {
                    valueArray[i1] = getUInt(endianess);
                    valueArray[i1 + 1] = getUInt(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == LONG) && (count == 1)) {
                valueArray[0] = getUInt(endianess);
            } else if ((type == LONG) && (count >= 2)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < 25000)); i1++) {
                    valueArray[i1] = getUInt(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == SLONG) && (count == 1)) {
                valueArray[0] = getInt(endianess);
            } else if ((type == SLONG) && (count >= 2)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < 25000)); i1++) {
                    valueArray[i1] = getInt(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == RATIONAL) && (tag == UIC2Tag)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < (6 * count)) && (i1 < 25000)); i1++) {
                    valueArray[i1] = getInt(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == RATIONAL) || (type == SRATIONAL)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < (2 * count)) && (i1 < 25000)); i1 = i1 + 2) {
                    valueArray[i1] = getInt(endianess);
                    valueArray[i1 + 1] = getInt(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == DOUBLE) && (count == 1)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);
                valueDouble = getDouble(endianess);
                raFile.seek(saveLocus);
            } else if ((type == DOUBLE) && (count > 1)) {

                // Ignore these EchoTech fields for now
                value_offset = getInt(endianess);
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 1)) {
                valueArray[0] = raFile.readUnsignedByte();
                raFile.readUnsignedByte();
                raFile.readUnsignedByte();
                raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 2)) {
                valueArray[0] = raFile.readUnsignedByte();
                valueArray[1] = raFile.readUnsignedByte();
                raFile.readUnsignedByte();
                raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 3)) {
                valueArray[0] = raFile.readUnsignedByte();
                valueArray[1] = raFile.readUnsignedByte();
                valueArray[2] = raFile.readUnsignedByte();
                raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 4)) {
                valueArray[0] = raFile.readUnsignedByte();
                valueArray[1] = raFile.readUnsignedByte();
                valueArray[2] = raFile.readUnsignedByte();
                valueArray[3] = raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count > 4)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < 25000)); i1++) {
                    valueArray[i1] = raFile.readUnsignedByte();
                }

                raFile.seek(saveLocus);
            } else if ((type == SBYTE) && (count == 1)) {
                valueArray[0] = raFile.readByte();
                raFile.readByte();
                raFile.readByte();
                raFile.readByte();
            } else if ((type == SBYTE) && (count == 2)) {
                valueArray[0] = raFile.readByte();
                valueArray[1] = raFile.readByte();
                raFile.readByte();
                raFile.readByte();
            } else if ((type == SBYTE) && (count == 3)) {
                valueArray[0] = raFile.readByte();
                valueArray[1] = raFile.readByte();
                valueArray[2] = raFile.readByte();
                raFile.readByte();
            } else if ((type == SBYTE) && (count == 4)) {
                valueArray[0] = raFile.readByte();
                valueArray[1] = raFile.readByte();
                valueArray[2] = raFile.readByte();
                valueArray[3] = raFile.readByte();
            } else if ((type == SBYTE) && (count > 4)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < 25000)); i1++) {
                    valueArray[i1] = raFile.readByte();
                }

                raFile.seek(saveLocus);
            } else if ((type == SSHORT) && (count == 1)) {
                valueArray[0] = getSignedShort(endianess);
                getSignedShort(endianess);
            } else if ((type == SSHORT) && (count == 2)) {
                valueArray[0] = getSignedShort(endianess);
                valueArray[1] = getSignedShort(endianess);
            } else if ((type == SSHORT) && (count >= 3)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < 25000)); i1++) {
                    valueArray[i1] = getSignedShort(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == FLOAT) && (count == 1)) {
                valueFloat = getFloat(endianess);
            } else if ((type == FLOAT) && (count > 1)) {

                // Ignore these fields for now
                value_offset = getInt(endianess);
            } else {
                Preferences.debug("\nOpenIFD: Unknown field type = " + type + " Tag = " + tag + "\n", 
                		Preferences.DEBUG_FILEIO);
                throw new IOException("OpenIFD: Unknown field type = " + type + " Tag = " + tag);
            }

            Preferences.debug("\nFileSTK.openIFD: Tag = " + tag + "\n", Preferences.DEBUG_FILEIO);

            switch (type) {

                case BYTE:
                    Preferences.debug("FileSTK.openIFD: Type = BYTE  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case ASCII:
                    Preferences.debug("FileSTK.openIFD: Type = ASCII  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SHORT:
                    Preferences.debug("FileSTK.openIFD: Type = SHORT  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case LONG:
                    Preferences.debug("FileSTK.openIFD: Type = LONG  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case RATIONAL:
                    Preferences.debug("FileSTK.openIFD: Type = RATIONAL  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SBYTE:
                    Preferences.debug("FileSTK.openIFD: Type = SBYTE  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case UNDEFINED:
                    Preferences.debug("FileSTK.openIFD: Type = UNDEFINED  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SSHORT:
                    Preferences.debug("FileSTK.openIFD: Type = SSHORT  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SLONG:
                    Preferences.debug("FileSTK.openIFD: Type = SLONG  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SRATIONAL:
                    Preferences.debug("FileSTK.openIFD: Type = SRATIONAL  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case FLOAT:
                    Preferences.debug("FileSTK.openIFD: Type = FLOAT  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case DOUBLE:
                    Preferences.debug("FileSTK.openIFD: Type = DOUBLE  Count = " + count + "\n", Preferences.DEBUG_FILEIO);
                    break;
            }


            if ((type == RATIONAL) && (tag == UIC2Tag)) {
                ecount = 6 * count;
            } else if ((type == RATIONAL) || (type == SRATIONAL)) {
                ecount = 2 * count;
            } else {
                ecount = count;
            }

            if ((type != DOUBLE) && (type != FLOAT)) {

                for (i1 = 0; ((i1 < ecount) && (i1 < 25000)); i1++) {
                    Preferences.debug("FileSTK.openIFD: value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n",
                    		Preferences.DEBUG_FILEIO);
                }
            } else if ((type == DOUBLE) && (count == 1)) {
                Preferences.debug("FileSTK.openIFD: value = " + valueDouble + "\n", Preferences.DEBUG_FILEIO);
            } else if ((type == FLOAT) && (count == 1)) {
                Preferences.debug("FileSTK.openIFD: value = " + valueFloat + "\n", Preferences.DEBUG_FILEIO);
            }

            switch (tag) {

                case NEW_SUBFILE_TYPE:
                    if (type != LONG) {
                        throw new IOException("NEW_SUBFILE_TYPE has illegal TYPE = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("NEW_SUBFILE_TYPE has illegal count = " + count + "\n");
                    }

                    Preferences.debug("FileSTK.openIFD: tag = NEW_SUBTYPE_FILE\n", Preferences.DEBUG_FILEIO);
                    if ((valueArray[0] & 0x01) == 0x01) {
                        Preferences.debug("Image is a reduced resolution version of another " +
                                          "image in this TIFF file\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Image is not a reduced resolution version of another " +
                                          "image in this TIFF file\n", Preferences.DEBUG_FILEIO);
                    }

                    if ((valueArray[0] & 0x02) == 0x02) {
                        Preferences.debug("Image is a single page of a multi-page image\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Image is not a single page of a multi-page image\n", Preferences.DEBUG_FILEIO);
                    }

                    if ((valueArray[0] & 0x04) == 0x04) {
                        Preferences.debug("Images defines a transparency mask for another image " +
                                          "in this TIFF file\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Images does not define a transparency mask for another image " +
                                          "in this TIFF file\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case IMAGE_WIDTH:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("IMAGE_WIDTH has illegal TYPE = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("IMAGE_WIDTH has illegal count = " + count + "\n");
                    }

                    xDim = (int) valueArray[0];
                    Preferences.debug("FileSTK.openIFD: Image_Width = " + xDim + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case IMAGE_LENGTH:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("IMAGE_LENGTH has illegal TYPE = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("IMAGE_LENGTH has illegal COUNT = " + count + "\n");
                    }

                    yDim = (int) valueArray[0];
                    Preferences.debug("FileSTK.openIFD: Image_Length = " + yDim + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case BITS_PER_SAMPLE:
                    if (type != SHORT) {
                        throw new IOException("BITS_PER_SAMPLE has illegal type = " + type + "\n");
                    }

                    if (count == 1) {
                        Preferences.debug("FileSTK.openIFD: BitsPerSample = " + valueArray[0] + "\n",
                        		Preferences.DEBUG_FILEIO);

                        switch ((int) valueArray[0]) {

                            case 1:
                                fileInfo.setDataType(ModelStorageBase.BOOLEAN);
                                break;

                            case 4:
                                fileInfo.setDataType(ModelStorageBase.BYTE);
                                break;

                            case 8:
                                fileInfo.setDataType(ModelStorageBase.UBYTE);
                                break;

                            case 16:
                                fileInfo.setDataType(ModelStorageBase.USHORT);
                                break;

                            case 32:
                                fileInfo.setDataType(ModelStorageBase.UINTEGER);
                                break;

                            default:
                                throw new IOException("TIFF Tag BitsPerSample has illegal value = " + valueArray[0]);
                        }
                    } else if (count > 1) {
                        Preferences.debug("FileSTK.openIFD: BitsPerSample\n", Preferences.DEBUG_FILEIO);

                        for (i1 = 0; i1 < count; i1++) {
                            Preferences.debug("Value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n", Preferences.DEBUG_FILEIO);
                        }

                        fileInfo.setDataType(ModelStorageBase.ARGB);
                    }

                    break;

                case ROWS_PER_STRIP:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("ROWS_PER-STRIP has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("ROWS_PER_STRIP has illegal count = " + count + "\n");
                    }

                    Preferences.debug("ROWS_PER_STRIP = " + valueArray[0] + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case STRIP_OFFSETS:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("STRIP_OFFSETS has illegal type = " + type + "\n");
                    }

                    dataOffsets[imageSlice] = new Vector<Index>();
                    if (count == 1) {
                        Preferences.debug("FileSTK.openIFD: Strip_offset = " + valueArray[0] + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        dataOffsets[imageSlice].addElement(new Index((int) valueArray[0]));
                    } else if (count > 1) {
                        Preferences.debug("FileSTK.openIFD: Strip_offset\n", Preferences.DEBUG_FILEIO);

                        for (i1 = 0; i1 < count; i1++) {
                            Preferences.debug("Value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n", Preferences.DEBUG_FILEIO);
                            dataOffsets[imageSlice].addElement(new Index((int) valueArray[i1]));
                        }
                    }

                    break;

                case STRIP_BYTE_COUNTS:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("STRIP_BYTE_COUNTS has illegal type = " + type + "\n");
                    }

                    if (count == 1) {
                        Preferences.debug("FileSTK.openIFD: Strip byte counts = " + valueArray[0] + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        ((Index) (dataOffsets[imageSlice].elementAt(0))).byteCount = (int) valueArray[0];
                    } else if (count > 1) {
                        Preferences.debug("FileSTK.openIFD. Strip byte counts\n", Preferences.DEBUG_FILEIO);

                        for (i1 = 0; i1 < count; i1++) {
                            Preferences.debug("Value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n", Preferences.DEBUG_FILEIO);
                            ((Index) (dataOffsets[imageSlice].elementAt(i1))).byteCount = (int) valueArray[i1];
                        }
                    }

                    break;

                case PHOTO_INTERP:
                    if (type != SHORT) {
                        throw new IOException("PHOTO_INTERP has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("PHOTO_INTERP has illegal count = " + count + "\n");
                    }

                    if (valueArray[0] > 4) {
                        throw new IOException("PHOTO_INTERP has illegal value = " + valueArray[0] + "\n");
                    }

                    Preferences.debug("FileSTK.openIFD: PhotoInterp= " + valueArray[0] + "\n", Preferences.DEBUG_FILEIO);
                    if (valueArray[0] == 1) { // Black is zero
                        fileInfo.setPhotometric((short) 1);
                        Preferences.debug("FileSTK.openIFD: PhotoInterp = Black is zero\n" +
                                          "For bilevel and grayscale images\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 0) { // white is zero
                        fileInfo.setPhotometric((short) 0);
                        Preferences.debug("FileSTK.openIFD: PhotoInterp = White is zero\n" +
                                          "For bilevel and grayscale images\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 2) { // Color RGB
                        fileInfo.setPhotometric((short) 2);
                        Preferences.debug("FileSTK.openIFD: PhotoInterp = RGB\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 3) { // Color Indexed
                        fileInfo.setPhotometric((short) 3);
                        Preferences.debug("FileSTK.openIFD: PhotoInterp = Palette color\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 4) { // Transparency Mask
                        fileInfo.setPhotometric((short) 4);
                        Preferences.debug("FileSTK.openIFD: PhotoInterp = Transparency Mask\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case SAMPLES_PER_PIXEL:
                    if (type != SHORT) {
                        throw new IOException("SAMPLES_PER_PIXEL has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("SAMPLES_PER_PIXEL has illegal count = " + count + "\n");
                    }

                    Preferences.debug("FileSTK.openIFD: samplesPerPixel = " + valueArray[0] + "\n", Preferences.DEBUG_FILEIO);
                    samplesPerPixel = (int) valueArray[0];
                    break;

                case PLANAR_CONFIG:
                    if (type != SHORT) {
                        throw new IOException("PLANAR_CONFIG has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("PLANAR_CONFIG has illegal count = " + count + "\n");
                    }

                    if ((valueArray[0] != 1) && (valueArray[0] != 2)) {
                        throw new IOException("PLANAR_CONFIG has illegal value = " + valueArray[0] + "\n");
                    }

                    if (valueArray[0] == 1) { // can be black and white or color
                        chunky = true;
                        Preferences.debug("FileSTK.openIFD: planar config = chunky \n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 2) { // Color RGB
                        chunky = false;
                        Preferences.debug("FileSTK.openIFD: planar config = RRRRR, GGGG, BBBB. \n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case COMPRESSION:
                    if (type != SHORT) {
                        throw new IOException("COMPRESSION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("COMPRESSION has illegal count = " + count + "\n");
                    }

                    if ((valueArray[0] != 1) && (valueArray[0] != 2) && (valueArray[0] != 32773)) {
                        throw new IOException("COMPRESSION has illegal value = " + valueArray[0] + "\n");
                    }

                    if (valueArray[0] == 2) {
                        throw new IOException("Modified Huffman run length encoding is not supported\n");
                    } else if (valueArray[0] == 1) {
                        packBit = false;
                        Preferences.debug("FileSTK.openIFD: compression = no compression\n ", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 32773) {
                        packBit = true;
                        Preferences.debug("FileSTK.openIFD: compression = packed bit\n ", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case COLOR_MAP:
                    if (type != SHORT) {
                        throw new IOException("COLOR_MAP has illegal type = " + type + "\n");
                    }

                    if ((count % 6) != 0) {
                        throw new IOException("COLOR_MAP has illegal count = " + count + "\n");
                    }

                    // Already read LUT -  only same LUT in every file of multiFile and only one
                    // LUT for a multiImage file for now.
                    if ((count == 768) && (LUT != null)) {
                        int[] extents = new int[2];
                        extents[0] = 4;
                        extents[1] = 256;

                        LUT = new ModelLUT(ModelLUT.GRAY, 256, extents);

                        for (i1 = 0; i1 < 256; i1++) {
                            LUT.set(0, i1, 1.0f);
                            LUT.set(1, i1, valueArray[i1]);
                            LUT.set(2, i1, valueArray[i1 + 256]);
                            LUT.set(3, i1, valueArray[i1 + 512]);
                        }

                        LUT.makeIndexedLUT(null);
                    }

                    break;

                case RESOLUTION_UNIT:
                    if (type != SHORT) {
                        throw new IOException("RESOLUTION_UNIT has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("RESOLUTION_UNIT has illegal count = " + count + "\n");
                    } else if ((valueArray[0] < 1) || (valueArray[0] > 17)) {
                        throw new IOException("RESOLUTION_UNIT has illegal value = " + valueArray[0] + "\n");
                    }

                    if (valueArray[0] == Unit.MILLIMETERS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = MILLIMETERS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.UNKNOWN_MEASURE.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.UNKNOWN_MEASURE.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.UNKNOWN_MEASURE.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = UNKNOWN\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.INCHES.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.INCHES.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.INCHES.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = INCHES\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.MILS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MILS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MILS.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = MILS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.CENTIMETERS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.CENTIMETERS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.CENTIMETERS.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = CENTIMETERS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.ANGSTROMS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.ANGSTROMS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.ANGSTROMS.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = ANGSTROMS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.NANOMETERS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.NANOMETERS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.NANOMETERS.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = NANOMETERS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.MICROMETERS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MICROMETERS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MICROMETERS.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = MICROMETERS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.METERS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.METERS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.METERS.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = METERS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.KILOMETERS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.KILOMETERS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.KILOMETERS.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = KILOMETERS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.MILES.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MILES.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MILES.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = MILES\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.NANOSEC.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.NANOSEC.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.NANOSEC.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = NANOSEC\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.MICROSEC.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MICROSEC.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MICROSEC.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = MICROSEC\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.MILLISEC.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = MILLISEC\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.SECONDS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.SECONDS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.SECONDS.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = SECONDS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.MINUTES.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.MINUTES.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.MINUTES.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = MINUTES\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.HOURS.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.HOURS.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.HOURS.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = HOURS\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == Unit.HZ.getLegacyNum()) {
                        fileInfo.setUnitsOfMeasure(Unit.HZ.getLegacyNum(), 0);
                        fileInfo.setUnitsOfMeasure(Unit.HZ.getLegacyNum(), 1);
                        Preferences.debug("FileSTK.openIFD: Resolution Unit = HERTZ\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case XRESOLUTION:
                    if (type != RATIONAL) {
                        throw new IOException("XRESOLUTION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("XRESOLUTION has illegal count = " + count + "\n");
                    }

                    numerator = valueArray[0];
                    denominator = valueArray[1];
                    imgResols[0] = (float) numerator / denominator;
                    imgResols[0] = 1 / imgResols[0];
                    Preferences.debug("FileSTK.openIFD: X Resolution = " + imgResols[0] + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case YRESOLUTION:
                    if (type != RATIONAL) {
                        throw new IOException("YRESOLUTION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("YRESOLUTION has illegal count = " + count + "\n");
                    }

                    numerator = valueArray[0];
                    denominator = valueArray[1];
                    imgResols[1] = (float) numerator / denominator;
                    imgResols[1] = 1 / imgResols[1];
                    Preferences.debug("FileSTK.openIFD: Y Resolution = " + imgResols[1] + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case ZRESOLUTION:
                    if (type != DOUBLE) {
                        throw new IOException("ZRESOLUTION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("ZRESOLUTION has illegal count = " + count + "\n");
                    }

                    imgResols[2] = (float) valueDouble;
                    Preferences.debug("FileSTK.openIFD: Z Resolution = " + imgResols[2] + "\n", Preferences.DEBUG_FILEIO);
                    fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
                    break;

                case TRESOLUTION:
                    if (type != DOUBLE) {
                        throw new IOException("TRESOLUTION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("TRESOLUTION has illegal count = " + count + "\n");
                    }

                    imgResols[3] = (float) valueDouble;
                    Preferences.debug("FileSTK.openIFD: T Resolution = " + imgResols[3] + "\n", Preferences.DEBUG_FILEIO);
                    fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(), 3);
                    break;

                case TILE_WIDTH:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("TILE_WIDTH has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("TILE_WIDTH has illegal count = " + count + "\n");
                    }

                    doTile = true;
                    tileWidth = (int) valueArray[0];
                    Preferences.debug("FileSTK.openIFD: tileWidth = " + tileWidth + "\n", Preferences.DEBUG_FILEIO);
                    tilesAcross = (xDim + tileWidth - 1) / tileWidth;
                    Preferences.debug("FileSTK.openIFD: tilesAcross = " + tilesAcross + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case TILE_LENGTH:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("TILE_LENGTH has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("TILE_LENGTH has illegal count = " + count + "\n");
                    }

                    tileLength = (int) valueArray[0];
                    Preferences.debug("FileSTK.openIFD: tileLength = " + tileLength + "\n", Preferences.DEBUG_FILEIO);
                    tilesDown = (yDim + tileLength - 1) / tileLength;
                    Preferences.debug("FileSTK.openIFD: tilesDown = " + tilesDown + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case TILE_OFFSETS:
                    if (type != LONG) {
                        throw new IOException("TILE_OFFSETS has illegal type = " + type + "\n");
                    }

                    if (chunky) {
                        tilesPerImage = count;
                    } else {
                        tilesPerImage = count / samplesPerPixel;
                    }

                    Preferences.debug("FileSTK.openIFD: tilesPerImage = " + tilesPerImage + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("FileSTK.openIFD: tileOffsets are above\n", Preferences.DEBUG_FILEIO);
                    if (tileOffsetNumber == 0) {
                        tileOffsets = new int[count];

                        for (i1 = 0; i1 < count; i1++) {
                            tileOffsets[i1] = (int) valueArray[i1];
                        }

                        tileOffsetNumber = count;
                    } // if (tileOffsetNumber == 0)
                    else {
                        tileTemp = new int[tileOffsetNumber];

                        for (i1 = 0; i1 < tileOffsetNumber; i1++) {
                            tileTemp[i1] = tileOffsets[i1];
                        }

                        tileOffsets = new int[tileOffsetNumber + count];

                        for (i1 = 0; i1 < tileOffsetNumber; i1++) {
                            tileOffsets[i1] = tileTemp[i1];
                        }

                        for (i1 = 0; i1 < count; i1++) {
                            tileOffsets[i1 + count] = (int) valueArray[i1];
                        }

                        tileOffsetNumber += count;
                    } // else for tileOffsetNumber != 0

                    break;

                case TILE_BYTE_COUNTS:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("TILE_BYTE_COUNTS has illegal type = " + type + "\n");
                    }

                    if (chunky) {

                        if (tilesPerImage != count) {
                            throw new IOException("Count fields do not agree in TILE_OFFSETS and TILE_BYTE_COUNTS");
                        }
                    } else {

                        if ((tilesPerImage * samplesPerPixel) != count) {
                            throw new IOException("Count fields do not agree in TILE_OFFSETS and TILE_BYTE_COUNTS");
                        }
                    }

                    Preferences.debug("FileSTK.openIFD: tileByteCounts are above\n", Preferences.DEBUG_FILEIO);
                    if (tileByteNumber == 0) {
                        tileByteCounts = new int[count];

                        for (i1 = 0; i1 < count; i1++) {
                            tileByteCounts[i1] = (int) valueArray[i1];
                        }

                        tileByteNumber = count;
                    } else {
                        tileTemp = new int[tileByteNumber];

                        for (i1 = 0; i1 < tileByteNumber; i1++) {
                            tileTemp[i1] = tileByteCounts[i1];
                        }

                        tileByteCounts = new int[tileByteNumber + count];

                        for (i1 = 0; i1 < tileByteNumber; i1++) {
                            tileByteCounts[i1] = tileTemp[i1];
                        }

                        for (i1 = 0; i1 < count; i1++) {
                            tileByteCounts[i1 + count] = (int) valueArray[i1];
                        }

                        tileByteNumber += count;
                    } // else for tileByteNumber != 0

                    break;

                // Used to store the plane annotations.
                // There is one NULL-terminated string for each plane in the stack.
                case IMAGE_DESCRIPTION:
                    if (type != ASCII) {
                        throw new IOException("IMAGE_DESCRIPTION has illegal type = " + type + "\n");
                    }

                    imageDescription = new byte[count];
                    for (i1 = 0; i1 < count; i1++) {
                        imageDescription[i1] = (byte) valueArray[i1];
                    }

                    str = new String(imageDescription);
                    fileInfo.setImageDescription(str);
                    j = 0;
                    planeNumber = 1;
                    for (i1 = 0; i1 < count; i1++) {

                        if (imageDescription[i1] == 0) {
                            str = new String(imageDescription, j, i1 - j);
                            Preferences.debug("FileSTK.openIFD: imageDescription for plane number " + planeNumber +
                                              ":\n" + str + "\n", Preferences.DEBUG_FILEIO);
                            j = i1 + 1;
                            planeNumber++;
                        }
                    }

                    break;

                case MIN_SAMPLE_VALUE:
                    if (type != SHORT) {
                        throw new IOException("MIN_SAMPLE_VALUE has illegal type = " + type + "\n");
                    }

                    for (i1 = 0; i1 < count; i1++) {
                        Preferences.debug("FileSTK.openIFD: minSampleValue[" + i1 + "] = " + valueArray[i1] + "\n", 
                        		Preferences.DEBUG_FILEIO);
                    }

                    break;

                case MAX_SAMPLE_VALUE:
                    if (type != SHORT) {
                        throw new IOException("MAX_SAMPLE_VALUE has illegal type = " + type + "\n");
                    }

                    for (i1 = 0; i1 < count; i1++) {
                        Preferences.debug("FileSTK.openIFD: maxSampleValue[" + i1 + "] = " + valueArray[i1] + "\n", 
                        		Preferences.DEBUG_FILEIO);
                    }

                    break;

                case SOFTWARE:
                    if (type != ASCII) {
                        throw new IOException("SOFTWARE has illegal type = " + type + "\n");
                    }

                    software = new byte[count];
                    for (i1 = 0; i1 < count; i1++) {
                        software[i1] = (byte) valueArray[i1];
                    }

                    str = new String(software);
                    Preferences.debug("FileSTK.openIFD: software = " + str + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case DATE_TIME:
                    if (type != ASCII) {
                        throw new IOException("DATE_TIME has illegal type = " + type + "\n");
                    }

                    if (count != 20) {
                        throw new IOException("DATE_TIME has illegal count = " + count + "\n");
                    }

                    dateTime = new byte[20];
                    for (i1 = 0; i1 < count; i1++) {
                        dateTime[i1] = (byte) valueArray[i1];
                    }

                    str = new String(dateTime);
                    Preferences.debug("FileSTK.openIFD: dateTime = " + str + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SAMPLE_FORMAT:
                    if (type != SHORT) {
                        throw new IOException("SAMPLE_FORMAT has illegal type = " + type + "\n");
                    }

                    for (i1 = 0; i1 < count; i1++) {
                        Preferences.debug("FileSTK.openIFD: sampleFormat[ " + i1 + "] = " + valueArray[i1] + "\n", 
                        		Preferences.DEBUG_FILEIO);

                        if (valueArray[i1] == 1) {
                            Preferences.debug("FileSTK.openIFD: unsigned integer data\n", Preferences.DEBUG_FILEIO);
                        } else if (valueArray[i1] == 2) {
                            Preferences.debug("FileSTK.openIFD: two's complement signed integer data\n", 
                            		Preferences.DEBUG_FILEIO);
                        } else if (valueArray[i1] == 3) {
                            Preferences.debug("FileSTK.openIFD: IEEE floating point data\n", Preferences.DEBUG_FILEIO);
                        } else if (valueArray[i1] == 4) {
                            Preferences.debug("FileSTK.openIFD: undefined data format\n", Preferences.DEBUG_FILEIO);
                        }
                    }

                    break;

                case PREDICTOR:
                    if (type != SHORT) {
                        throw new IOException("PREDICTOR has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("PREDICTOR has illegal count = " + count + "\n");
                    }

                    if (valueArray[0] == 1) {
                        Preferences.debug("PREDICTOR = 1 for no prediction scheme used\n", Preferences.DEBUG_FILEIO);
                    } else if (valueArray[0] == 2) {
                        Preferences.debug("PREDICTOR = 2 for horizontal differencing\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("PREDICTOR = " + valueArray[0] + ", an illegal value", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case UIC2Tag:
                    if (type != RATIONAL) {
                        throw new IOException("UIC2Tag has illegal type = " + type + "\n");
                    }

                    numberImages = count;
                    zDistanceNumerator = (int) valueArray[0];
                    zDistanceDenominator = (int) valueArray[1];
                    Preferences.debug("zDistanceNumerator = " + zDistanceNumerator + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("zDistanceDenominator = " + zDistanceDenominator + "\n", Preferences.DEBUG_FILEIO);
                    zSameDistance = true;
                    for (i1 = 1; i1 < count; i1++) {

                        if ((zDistanceNumerator != valueArray[6 * i1]) ||
                                (zDistanceDenominator != valueArray[(6 * i1) + 1])) {
                            zSameDistance = false;
                        }
                    }

                    if ((numberImages > 1) && (zSameDistance) && (zDistanceNumerator == 0)) {
                        imgResols[2] = 0.0f;
                        Preferences.debug("Slices are at the same z position\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setSameZPosition(true);
                    } else if ((numberImages > 1) && (zSameDistance) && (zDistanceDenominator != 0)) {
                        imgResols[2] = (float) zDistanceNumerator / zDistanceDenominator;
                        Preferences.debug("z resolution = " + imgResols[2] + "\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case UIC1Tag:
                    if (type != LONG) {
                        throw new IOException("UIC1Tag has illegal type = " + type + "\n");
                    }

                    for (i1 = 0; i1 < count; i1 += 2) {

                        if (valueArray[i1] == 6) { // Calibration Units
                            value_offset = (int) valueArray[i1 + 1];

                            long saveLocus = raFile.getFilePointer();
                            raFile.seek(value_offset);

                            int stringLength = getInt(endianess);
                            byte[] cal = new byte[stringLength];
                            raFile.read(cal);

                            String calString = new String(cal);
                            Preferences.debug("Calibration units = " + calString + "\n", Preferences.DEBUG_FILEIO);
                            raFile.seek(saveLocus);
                        } else if (valueArray[i1] == 7) { // Name
                            value_offset = (int) valueArray[i1 + 1];

                            long saveLocus = raFile.getFilePointer();
                            raFile.seek(value_offset);

                            int stringLength = getInt(endianess);
                            byte[] win = new byte[stringLength];
                            raFile.read(win);

                            String winString = new String(win);
                            Preferences.debug("Window name = " + winString + "\n", Preferences.DEBUG_FILEIO);
                            raFile.seek(saveLocus);
                        }
                    }

                    break;

                default:
                    break;
            }
        }

        imageSlice++;
        IFDoffsets[imageSlice] = getInt(endianess);
        Preferences.debug("\nFileSTK.openIFD: Ref. to next imageSlice = " + IFDoffsets[imageSlice] + "\n", 
        		Preferences.DEBUG_FILEIO);

        if (IFDoffsets[imageSlice] == 0) {
            return false; // Done reading images
        }

        return true; // Read more IFDs (ie. images)
    }


    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice   offset into the file stored in the dataOffset array
     * @param      buffer  buffer where the info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readBuffer(int slice, float[] buffer) throws IOException {
        int i, j, a;
        int iCount, iNext;
        int b1, b2, b3, b4;
        long progress, progressLength, mod;

        // long pointer;
        int idx = 0;
        int nBytes;
        int nLength;
        int planarRGB = 0; // Use this for planar RGB where you must read a stripsPerImage
                           // number of red strips, followed by a stripsPerImage number of
                           // green strips, followed by a stripsPerImage number of blue strips.

        int nIndex = dataOffsets[slice].size();
        int stripsPerImage = nIndex / 3; // used for planar RGB

        i = 0;

        for (a = 0; a < nIndex; a++, idx++) {

            try {

                raFile.seek(((Index) (dataOffsets[slice].elementAt(idx))).index);
                nBytes = ((Index) (dataOffsets[slice].elementAt(idx))).byteCount;

                if (nBytes == 0) {
                    nBytes = buffer.length;
                }

                ;

                switch (fileInfo.getDataType()) {

                    case ModelStorageBase.BOOLEAN:
                        nLength = 8 * ((buffer.length + 63) >> 6);
                        byteBuffer = new byte[nLength];
                        raFile.read(byteBuffer, 0, nLength);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j++, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            buffer[i] = byteBuffer[j >> 3] & (1 << (7 - (j % 8)));
                        }

                        break;

                    case ModelStorageBase.BYTE:
                        if (packBit == false) {

                            if (byteBuffer == null) {
                                byteBuffer = new byte[buffer.length];
                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 10;


                            for (j = 0; j < nBytes; j++, i++) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                buffer[i] = byteBuffer[j];
                            }
                        } // if (packBit == false)
                        else if (packBit == true) {
                            byteBuffer = new byte[nBytes];
                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 100;

                            j = 0;

                            while (j < nBytes) {

                                // uncompressed data bytes follow
                                // Copy the next n+1 bytes literally
                                if ((byteBuffer[j] & 0x80) == 0) {
                                    iCount = byteBuffer[j] + 1;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, j++, i++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = byteBuffer[j];
                                    }
                                } // end of if (byteBuffer[j] & 0x80 == 0)

                                // Do nothing if the byte value is -128
                                else if (byteBuffer[j] == -128) {
                                    j++;
                                }
                                // compressed data bytes follow
                                // (~byteBuffer[j]) + 1 is the 2's complement of n or -n
                                // Hence (~byteBuffer[j]) + 2 equals -n + 1
                                else {
                                    iCount = (~byteBuffer[j]) + 2;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, i++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = byteBuffer[j];
                                    }

                                    j++;
                                } // end of else for compressed data bytes
                            } // end of while (j < nBytes)
                        } // end of else if (packBit == true)

                        break;

                    case ModelStorageBase.UBYTE:
                        if (packBit == false) {

                            if (byteBuffer == null) {
                                byteBuffer = new byte[buffer.length];
                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 100;


                            for (j = 0; j < nBytes; j++, i++) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                buffer[i] = byteBuffer[j] & 0xff;
                            }
                        } // if (packBit == false)
                        else if (packBit == true) {
                            byteBuffer = new byte[nBytes];
                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 100;

                            j = 0;

                            while (j < nBytes) {

                                // uncompressed data bytes follow
                                // Copy the next n+1 bytes literally
                                if ((byteBuffer[j] & 0x80) == 0) {
                                    iCount = byteBuffer[j] + 1;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, j++, i++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = byteBuffer[j] & 0xff;
                                    }
                                } // end of if (byteBuffer[j] & 0x80 == 0)

                                // Do nothing if the byte value is -128
                                else if (byteBuffer[j] == -128) {
                                    j++;
                                }
                                // compressed data bytes follow
                                // (~byteBuffer[j]) + 1 is the 2's complement of n or -n
                                // Hence (~byteBuffer[j]) + 2 equals -n + 1
                                else {
                                    iCount = (~byteBuffer[j]) + 2;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, i++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = byteBuffer[j] & 0xff;
                                    }

                                    j++;
                                } // end of else for compressed data bytes
                            } // end of while (j < nBytes)
                        } // end of else if (packBit == true)

                        break;

                    case ModelStorageBase.SHORT:
                        if (byteBuffer == null) {
                            byteBuffer = new byte[2 * buffer.length];
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 2, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);

                            if (endianess) {
                                buffer[i] = (short) ((b1 << 8) + b2);
                            } else {
                                buffer[i] = (short) ((b2 << 8) + b1);
                            }

                        }

                        break;

                    case ModelStorageBase.USHORT:
                        if (byteBuffer == null) {
                            byteBuffer = new byte[2 * buffer.length];
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 2, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);

                            if (endianess) {
                                buffer[i] = ((b1 << 8) + b2);
                            } else {
                                buffer[i] = ((b2 << 8) + b1);
                            }
                        }

                        break;

                    case ModelStorageBase.INTEGER:
                        if (byteBuffer == null) {
                            byteBuffer = new byte[4 * buffer.length];
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 4, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);
                            b3 = getUnsignedByte(byteBuffer, j + 2);
                            b4 = getUnsignedByte(byteBuffer, j + 3);

                            if (endianess) {
                                buffer[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                            } else {
                                buffer[i] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                            }
                        }

                        break;

                    case ModelStorageBase.UINTEGER:
                        if (byteBuffer == null) {
                            byteBuffer = new byte[4 * buffer.length];
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 4, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);
                            b3 = getUnsignedByte(byteBuffer, j + 2);
                            b4 = getUnsignedByte(byteBuffer, j + 3);

                            if (endianess) {
                                buffer[i] = (((long) b1 << 24) | ((long) b2 << 16) | ((long) b3 << 8) | (long) b4); // Big Endian
                            } else {
                                buffer[i] = (((long) b4 << 24) | ((long) b3 << 16) | ((long) b2 << 8) | (long) b1); // Little Endian
                            }
                        }

                        break;

                    case ModelStorageBase.ARGB:
                        if (chunky == true) {

                            if (byteBuffer == null) {
                                byteBuffer = new byte[buffer.length];
                            }

                            raFile.read(byteBuffer, 0, nBytes);

                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 10;


                            // For the moment I compress RGB images to unsigned bytes.
                            for (j = 0; j < nBytes; j += 3, i += 4) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                buffer[i] = 255;
                                buffer[i + 1] = getUnsignedByte(byteBuffer, j);
                                buffer[i + 2] = getUnsignedByte(byteBuffer, j + 1);
                                buffer[i + 3] = getUnsignedByte(byteBuffer, j + 2);
                            }
                        } // if (chunky == true)
                        else { // planar RGB configuration

                            if (planarRGB < stripsPerImage) {

                                if (byteBuffer == null) {
                                    byteBuffer = new byte[buffer.length];
                                }

                                raFile.read(byteBuffer, 0, nBytes);

                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                // For the moment I compress RGB images to unsigned bytes
                                for (j = 0; j < nBytes; j++, i += 4) {

                                    if ((((i / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) + progress) /
                                                                                progressLength * 100));
                                    }

                                    buffer[i] = 255;
                                    buffer[i + 1] = getUnsignedByte(byteBuffer, j);
                                }

                                planarRGB++;

                                if (planarRGB == stripsPerImage) {
                                    i = 0;
                                }
                            } // end of if (planarRGB < stripsPerImage)
                            else if (planarRGB < (2 * stripsPerImage)) {
                                raFile.read(byteBuffer, 0, nBytes);

                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                for (j = 0; j < nBytes; j++, i += 4) {

                                    if ((((i / 3) + (buffer.length / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) + (buffer.length / 3) +
                                                                                     progress) / progressLength * 100));
                                    }

                                    buffer[i + 2] = getUnsignedByte(byteBuffer, j);
                                }

                                planarRGB++;

                                if (planarRGB == (2 * stripsPerImage)) {
                                    i = 0;
                                }
                            } // end of else if (planarRGB < 2*stripsPerImage)
                            else { // planarRGB >= 2*stripsPerImage
                                raFile.read(byteBuffer, 0, nBytes);

                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                for (j = 0; j < nBytes; j++, i += 4) {

                                    if ((((i / 3) + (2 * buffer.length / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) +
                                                                                     (2 * buffer.length / 3) +
                                                                                     progress) / progressLength * 100));
                                    }

                                    buffer[i + 3] = getUnsignedByte(byteBuffer, j);
                                }

                                planarRGB++;
                            } // end of else for planarRGB >= 2*StripsPerImage
                        } // end of else for planar RGB configuration

                        break;
                }
            } catch (OutOfMemoryError error) {
                System.gc();
                throw error;
            }
        }

    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice   image slice
     * @param      buffer  buffer where the info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readTileBuffer(int slice, float[] buffer) throws IOException {
        int a, i, j;
        int iCount, iNext;
        int b1, b2, b3, b4;
        long progress, progressLength, mod;
        int nBytes;
        int nLength;
        int xTile, yTile;
        int x, y;

        i = 0;
        xTile = 0;
        yTile = 0;
        x = 0;
        y = 0;

        for (a = 0; a < tilesPerSlice; a++) {

            try {
                raFile.seek(tileOffsets[(slice * tilesPerSlice) + a]);
                nBytes = tileByteCounts[(slice * tilesPerSlice) + a];

                switch (fileInfo.getDataType()) {

                    case ModelStorageBase.BOOLEAN:
                        nLength = 8 * ((nBytes + 63) >> 6);
                        byteBuffer = new byte[nLength];
                        raFile.read(byteBuffer, 0, nLength);
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;

                        for (j = 0; j < nBytes; j++) {

                            if ((x < xDim) && (y < yDim)) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                buffer[x + (y * xDim)] = byteBuffer[j >> 3] & (1 << (7 - (j % 8)));
                                i++;
                            } // if ((x < xDim) && (y < yDim))

                            x++;

                            if (x == ((xTile + 1) * tileWidth)) {
                                x = xTile * tileWidth;
                                y++;
                            }
                        } // for (j = 0; j < nBytes; j++)

                        xTile++;
                        if (xTile == tilesAcross) {
                            xTile = 0;
                            yTile++;
                        }

                        x = xTile * tileWidth;
                        y = yTile * tileLength;
                        break;

                    case ModelStorageBase.BYTE:
                        if (packBit == false) {

                            if (byteBuffer == null) {
                                byteBuffer = new byte[nBytes];
                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;


                            for (j = 0; j < nBytes; j++) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    buffer[x + (y * xDim)] = byteBuffer[j];
                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < nBytes; j++)

                            xTile++;

                            if (xTile == tilesAcross) {
                                xTile = 0;
                                yTile++;
                            }

                            x = xTile * tileWidth;
                            y = yTile * tileLength;
                        } // if (packBit == false)
                        else if (packBit == true) {
                            byteBuffer = new byte[nBytes];
                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;

                            j = 0;

                            while (j < nBytes) {

                                // uncompressed data bytes follow
                                // Copy the next n+1 bytes literally
                                if ((byteBuffer[j] & 0x80) == 0) {
                                    iCount = byteBuffer[j] + 1;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, j++) {

                                        if ((x < xDim) && (y < yDim)) {

                                            if (((i + progress) % mod) == 0) {
                                                fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                        progressLength * 100));
                                            }

                                            buffer[x + (y * xDim)] = byteBuffer[j];
                                            i++;
                                        } // if ((x < xDim) && (y < yDim))

                                        x++;

                                        if (x == ((xTile + 1) * tileWidth)) {
                                            x = xTile * tileWidth;
                                            y++;
                                        } // if (x == (xTile+1)*tileWidth)
                                    } // for (iNext = 0; iNext < iCount; iNext++,j++)
                                } // end of if (byteBuffer[j] & 0x80 == 0)

                                // Do nothing if the byte value is -128
                                else if (byteBuffer[j] == -128) {
                                    j++;
                                }
                                // compressed data bytes follow
                                // (~byteBuffer[j]) + 1 is the 2's complement of n or -n
                                // Hence (~byteBuffer[j]) + 2 equals -n + 1
                                else {
                                    iCount = (~byteBuffer[j]) + 2;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, i++) {

                                        if ((x < xDim) && (y < yDim)) {

                                            if (((i + progress) % mod) == 0) {
                                                fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                        progressLength * 100));
                                            }

                                            buffer[x + (y * xDim)] = byteBuffer[j];
                                        } // if ((x < xDim) && (y < yDim))

                                        x++;

                                        if (x == ((xTile + 1) * tileWidth)) {
                                            x = xTile * tileWidth;
                                            y++;
                                        } // if (x == (xTile+1)*tileWidth)
                                    } // for (iNext = 0; iNext < iCount; iNext++,i++)

                                    j++;
                                } // end of else for compressed data bytes
                            } // end of while (j < nBytes)

                            xTile++;

                            if (xTile == tilesAcross) {
                                xTile = 0;
                                yTile++;
                            }

                            x = xTile * tileWidth;
                            y = yTile * tileLength;
                        } // else if (packBit == true)

                        break;

                    case ModelStorageBase.UBYTE:
                        if (packBit == false) {

                            if (byteBuffer == null) {
                                byteBuffer = new byte[nBytes];
                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;


                            for (j = 0; j < nBytes; j++) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    buffer[x + (y * xDim)] = byteBuffer[j] & 0xff;
                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < nBytes; j++)

                            xTile++;

                            if (xTile == tilesAcross) {
                                xTile = 0;
                                yTile++;
                            }

                            x = xTile * tileWidth;
                            y = yTile * tileLength;
                        } // if (packBit == false)
                        else if (packBit == true) {
                            byteBuffer = new byte[nBytes];
                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;

                            j = 0;

                            while (j < nBytes) {

                                // uncompressed data bytes follow
                                // Copy the next n+1 bytes literally
                                if ((byteBuffer[j] & 0x80) == 0) {
                                    iCount = byteBuffer[j] + 1;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, j++) {

                                        if ((x < xDim) && (y < yDim)) {

                                            if (((i + progress) % mod) == 0) {
                                                fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                        progressLength * 100));
                                            }

                                            buffer[x + (y * xDim)] = byteBuffer[j] & 0xff;
                                            i++;
                                        } // if ((x < xDim) && (y < yDim))

                                        x++;

                                        if (x == ((xTile + 1) * tileWidth)) {
                                            x = xTile * tileWidth;
                                            y++;
                                        } // if (x == (xTile+1)*tileWidth)
                                    } // for (iNext = 0; iNext < iCount; iNext++,j++)
                                } // if (byteBuffer[j] & 0x80 == 0)

                                // Do nothing if the byte value is -128
                                else if (byteBuffer[j] == -128) {
                                    j++;
                                }
                                // compressed data bytes follow
                                // (~byteBuffer[j]) + 1 is the 2's complement of n or -n
                                // Hence (~byteBuffer[j]) + 2 equals -n + 1
                                else {
                                    iCount = (~byteBuffer[j]) + 2;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, i++) {

                                        if ((x < xDim) && (y < yDim)) {

                                            if (((i + progress) % mod) == 0) {
                                                fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                        progressLength * 100));
                                            }

                                            buffer[x + (y * xDim)] = byteBuffer[j] & 0xff;
                                        } // if ((x < xDim) && (y < yDim))

                                        x++;

                                        if (x == ((xTile + 1) * tileWidth)) {
                                            x = xTile * tileWidth;
                                            y++;
                                        } // if (x == (xTile+1)*tileWidth)
                                    } // for (iNext = 0; iNext < iCount; iNext++,i++)

                                    j++;
                                } // else for compressed data bytes
                            } // while (j < nBytes)

                            xTile++;

                            if (xTile == tilesAcross) {
                                xTile = 0;
                                yTile++;
                            }

                            x = xTile * tileWidth;
                            y = yTile * tileLength;
                        } // else if (packBit == true)

                        break;

                    case ModelStorageBase.SHORT:
                        if (byteBuffer == null) {
                            byteBuffer = new byte[nBytes];
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;

                        for (j = 0; j < nBytes; j += 2) {

                            if ((x < xDim) && (y < yDim)) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                b1 = getUnsignedByte(byteBuffer, j);
                                b2 = getUnsignedByte(byteBuffer, j + 1);

                                if (endianess) {
                                    buffer[x + (y * xDim)] = (short) ((b1 << 8) + b2);
                                } else {
                                    buffer[x + (y * xDim)] = (short) ((b2 << 8) + b1);
                                }

                                i++;
                            } // if ((x < xDim) && (y < yDim))

                            x++;

                            if (x == ((xTile + 1) * tileWidth)) {
                                x = xTile * tileWidth;
                                y++;
                            }
                        } // for (j = 0; j < nBytes; j+= 2)

                        xTile++;
                        if (xTile == tilesAcross) {
                            xTile = 0;
                            yTile++;
                        }

                        x = xTile * tileWidth;
                        y = yTile * tileLength;
                        break;

                    case ModelStorageBase.USHORT:
                        if (byteBuffer == null) {
                            byteBuffer = new byte[nBytes];
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;

                        for (j = 0; j < nBytes; j += 2) {

                            if ((x < xDim) && (y < yDim)) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                b1 = getUnsignedByte(byteBuffer, j);
                                b2 = getUnsignedByte(byteBuffer, j + 1);

                                if (endianess) {
                                    buffer[x + (y * xDim)] = ((b1 << 8) + b2);
                                } else {
                                    buffer[x + (y * xDim)] = ((b2 << 8) + b1);
                                }

                                i++;
                            } // if ((x < xDim) && (y < yDim))

                            x++;

                            if (x == ((xTile + 1) * tileWidth)) {
                                x = xTile * tileWidth;
                                y++;
                            }
                        } // for (j = 0; j < nBytes; j+= 2)

                        xTile++;
                        if (xTile == tilesAcross) {
                            xTile = 0;
                            yTile++;
                        }

                        x = xTile * tileWidth;
                        y = yTile * tileLength;
                        break;

                    case ModelStorageBase.INTEGER:
                        if (byteBuffer == null) {
                            byteBuffer = new byte[nBytes];
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;

                        for (j = 0; j < nBytes; j += 4) {

                            if ((x < xDim) && (y < yDim)) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                b1 = getUnsignedByte(byteBuffer, j);
                                b2 = getUnsignedByte(byteBuffer, j + 1);
                                b3 = getUnsignedByte(byteBuffer, j + 2);
                                b4 = getUnsignedByte(byteBuffer, j + 3);

                                if (endianess) {
                                    buffer[x + (y * xDim)] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                                } else {
                                    buffer[x + (y * xDim)] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                                }

                                i++;
                            } // if ((x < xDim) && (y < yDim))

                            x++;

                            if (x == ((xTile + 1) * tileWidth)) {
                                x = xTile * tileWidth;
                                y++;
                            }
                        } // for (j = 0; j < nBytes; j+= 4)

                        xTile++;
                        if (xTile == tilesAcross) {
                            xTile = 0;
                            yTile++;
                        }

                        x = xTile * tileWidth;
                        y = yTile * tileLength;
                        break;

                    case ModelStorageBase.UINTEGER:
                        if (byteBuffer == null) {
                            byteBuffer = new byte[nBytes];
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;

                        for (j = 0; j < nBytes; j += 4) {

                            if ((x < xDim) && (y < yDim)) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                b1 = getUnsignedByte(byteBuffer, j);
                                b2 = getUnsignedByte(byteBuffer, j + 1);
                                b3 = getUnsignedByte(byteBuffer, j + 2);
                                b4 = getUnsignedByte(byteBuffer, j + 3);

                                if (endianess) {
                                    buffer[x + (y * xDim)] = (((long) b1 << 24) | ((long) b2 << 16) | ((long) b3 << 8) |
                                                                  (long) b4); // Big Endian
                                } else {
                                    buffer[x + (y * xDim)] = (((long) b4 << 24) | ((long) b3 << 16) | ((long) b2 << 8) |
                                                                  (long) b1); // Little Endian
                                }

                                i++;
                            } // if ((x < xDim) && (y < yDim))

                            x++;

                            if (x == ((xTile + 1) * tileWidth)) {
                                x = xTile * tileWidth;
                                y++;
                            }
                        } // for (j = 0; j < nBytes; j+= 4)

                        xTile++;
                        if (xTile == tilesAcross) {
                            xTile = 0;
                            yTile++;
                        }

                        x = xTile * tileWidth;
                        y = yTile * tileLength;
                        break;

                    case ModelStorageBase.ARGB:
                        if (chunky == true) {

                            if (byteBuffer == null) {
                                byteBuffer = new byte[nBytes];
                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;


                            for (j = 0; j < nBytes; j += 3) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    buffer[4 * (x + (y * xDim))] = 255;
                                    buffer[(4 * (x + (y * xDim))) + 1] = getUnsignedByte(byteBuffer, j);
                                    buffer[(4 * (x + (y * xDim))) + 2] = getUnsignedByte(byteBuffer, j + 1);
                                    buffer[(4 * (x + (y * xDim))) + 3] = getUnsignedByte(byteBuffer, j + 2);
                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < nBytes; j+= 3)

                            xTile++;

                            if (xTile == tilesAcross) {
                                xTile = 0;
                                yTile++;
                            }

                            x = xTile * tileWidth;
                            y = yTile * tileLength;
                        } // if (chunky == true)

                        break;
                } // switch(fileInfo.getDataType())
            } // try
            catch (OutOfMemoryError error) {
                System.gc();
                throw error;
            }
        } // for (i = 0; i < tilesPerSlice; i++)
    }

    /**
     * Writes color map to the TIFF file.
     *
     * @param   location  Location to write at.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeColorMap(int location) throws IOException {
        int i, j;
        boolean endianess = image.getFileInfo(0).getEndianess();

        long saveLoc = raFile.getFilePointer();
        raFile.seek(location);

        byte[] colorTable = new byte[768 * 2];

        j = 0;

        for (i = 0; i < 256; i++) {
            setBufferShort(colorTable, LUT.getShort(1, i), j, endianess);
            setBufferShort(colorTable, LUT.getShort(2, i), j + 512, endianess);
            setBufferShort(colorTable, LUT.getShort(3, i), j + 1024, endianess);
            j += 2;
        }

        raFile.write(colorTable);
        raFile.seek(saveLoc);
    }

    /**
     * Writes TIFF starting file header.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeHeader() throws IOException {
        boolean endianess = image.getFileInfo(0).getEndianess();
        byte[] hdr = new byte[8];

        if (endianess == true) {
            hdr[0] = 77; // Big endian.
            hdr[1] = 77;
            hdr[2] = 0; // 42  magic number
            hdr[3] = 42;
            hdr[4] = 0; // 8 (offset to first IFD)
            hdr[5] = 0;
            hdr[6] = 0;
            hdr[7] = 8;
        } else {
            hdr[0] = 73; // "49" little endian
            hdr[1] = 73;
            hdr[2] = 42; // 42  magic number
            hdr[3] = 0;
            hdr[4] = 8; // 8 (offset to first IFD)
            hdr[5] = 0;
            hdr[6] = 0;
            hdr[7] = 0;
        }

        raFile.write(hdr);
    }

    /**
     * Writes one 12 byte IFD entry.
     *
     * @param   tag     DOCUMENT ME!
     * @param   type    DOCUMENT ME!
     * @param   count   DOCUMENT ME!
     * @param   value   DOCUMENT ME!
     * @param   value2  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeIFD(int tag, int type, int count, int value, int value2) throws IOException {
        boolean endianess = image.getFileInfo(0).getEndianess();

        writeShort((short) tag, endianess);
        writeShort((short) type, endianess);
        writeInt(count, endianess);

        if ((type == SHORT) && (count < 3)) {
            writeShort((short) value, endianess);
            writeShort((short) value2, endianess);
        } else {
            writeInt(value, endianess);
        }
    }


    /**
     * Writes one IFD (Image File Directory). One IFD per image
     *
     * @param               imageOffset    offset to next IFD. If equal to zero then end of images
     * @param               nextIFD        DOCUMENT ME!
     * @param               index          image index for file information.
     * @param               theStripCount  DOCUMENT ME!
     * @param               writePackBit   DOCUMENT ME!
     *
     * @throws              IOException  DOCUMENT ME!
     *
     * @theStripByteCounts  number of bytes in the strip
     */
    private void writeIFDs(int imageOffset, int nextIFD, int index, int theStripCount, boolean writePackBit)
            throws IOException {
        boolean endianess = image.getFileInfo(0).getEndianess();
        int type;
        int bitsPerSample;
        int bytesPerSample = 2;
        int samplesPerPixel;
        int bitsPerSampleOffset = 0;
        int resolutionOffset;
        int ztEntries = 0;
        int zResOffset = 0;
        int tResOffset = 0;
        int resolutionCount = 16;
        int zResCount = 0;
        int rgbCount = 0;
        int resXYUnit = 0;
        int resYUnit = 0;
        float xResol = (float) 0.0;
        float yResol = (float) 0.0;

        // TIFF 6.0 standard only allows 3 different units of measurement -
        // 1 for no unit, 2 for inch, and 3 for centimeter.
        // This unit of measurement must be applied to both the
        // X and Y resolution.
        resXYUnit = image.getFileInfo(index).getUnitsOfMeasure(0);
        resYUnit = image.getFileInfo(index).getUnitsOfMeasure(1);
        xResol = image.getFileInfo(index).getResolutions()[0];
        yResol = image.getFileInfo(index).getResolutions()[1];

        if ((resXYUnit != Unit.UNKNOWN_MEASURE.getLegacyNum()) && (resXYUnit != Unit.INCHES.getLegacyNum()) &&
                (resXYUnit != Unit.MILS.getLegacyNum()) &&
                (resXYUnit != Unit.CENTIMETERS.getLegacyNum()) && (resXYUnit != Unit.MILLIMETERS.getLegacyNum()) &&
                (resXYUnit != Unit.METERS.getLegacyNum()) && (resXYUnit != Unit.ANGSTROMS.getLegacyNum()) &&
                (resXYUnit != Unit.NANOMETERS.getLegacyNum()) && (resXYUnit != Unit.MICROMETERS.getLegacyNum()) &&
                (resXYUnit != Unit.MILES.getLegacyNum()) && (resXYUnit != Unit.KILOMETERS.getLegacyNum())) {
            resXYUnit = Unit.UNKNOWN_MEASURE.getLegacyNum();
        }


        if (resXYUnit == Unit.ANGSTROMS.getLegacyNum()) {
            resXYUnit = Unit.CENTIMETERS.getLegacyNum();
            xResol = 1.0e-8f * xResol;
        } else if (resXYUnit == Unit.NANOMETERS.getLegacyNum()) {
            resXYUnit = Unit.CENTIMETERS.getLegacyNum();
            xResol = 1.0e-7f * xResol;
        } else if (resXYUnit == Unit.MICROMETERS.getLegacyNum()) {
            resXYUnit = Unit.CENTIMETERS.getLegacyNum();
            xResol = 1.0e-4f * xResol;
        } else if (resXYUnit == Unit.MILLIMETERS.getLegacyNum()) {
            resXYUnit = Unit.CENTIMETERS.getLegacyNum();
            xResol = 0.1f * xResol;
        } else if (resXYUnit == Unit.METERS.getLegacyNum()) {
            resXYUnit = Unit.CENTIMETERS.getLegacyNum();
            xResol = 100.0f * xResol;
        } else if (resXYUnit == Unit.KILOMETERS.getLegacyNum()) {
            resXYUnit = Unit.CENTIMETERS.getLegacyNum();
            xResol = 1.0e5f * xResol;
        } else if (resXYUnit == Unit.MILES.getLegacyNum()) {
            resXYUnit = Unit.INCHES.getLegacyNum();
            xResol = 63360.0f * xResol;
        } else if (resXYUnit == Unit.MILS.getLegacyNum()) {
            resXYUnit = Unit.INCHES.getLegacyNum();
            xResol = 1.0e-3f * xResol;
        }

        if (resXYUnit == Unit.CENTIMETERS.getLegacyNum()) {

            // Change the Y resolution units to centimeters
            if (resYUnit == Unit.ANGSTROMS.getLegacyNum()) {
                yResol = 1.0e-8f * yResol;
            } else if (resYUnit == Unit.NANOMETERS.getLegacyNum()) {
                yResol = 1.0e-7f * yResol;
            } else if (resYUnit == Unit.MICROMETERS.getLegacyNum()) {
                yResol = 1.0e-4f * yResol;
            } else if (resYUnit == Unit.MILLIMETERS.getLegacyNum()) {
                yResol = 0.1f * yResol;
            } else if (resYUnit == Unit.INCHES.getLegacyNum()) {
                yResol = 2.54f * yResol;
            } else if (resYUnit == Unit.MILS.getLegacyNum()) {
                yResol = 2.54e-3f * yResol;
            } else if (resYUnit == Unit.METERS.getLegacyNum()) {
                yResol = 100.0f * yResol;
            } else if (resYUnit == Unit.KILOMETERS.getLegacyNum()) {
                yResol = 1.0e5f * yResol;
            } else if (resYUnit == Unit.MILES.getLegacyNum()) {
                yResol = 1.6093e5f * yResol;
            }
        } // if (resXYUnit == Unit.CENTIMETERS.getLegacyNum())
        else if (resXYUnit == Unit.INCHES.getLegacyNum()) {

            // Change the Y resolution units to inches
            if (resYUnit == Unit.ANGSTROMS.getLegacyNum()) {
                yResol = 3.937e-9f * yResol;
            } else if (resYUnit == Unit.NANOMETERS.getLegacyNum()) {
                yResol = 3.937e-8f * yResol;
            } else if (resYUnit == Unit.MICROMETERS.getLegacyNum()) {
                yResol = 3.937e-5f * yResol;
            } else if (resYUnit == Unit.MILLIMETERS.getLegacyNum()) {
                yResol = 3.937e-2f * yResol;
            } else if (resYUnit == Unit.CENTIMETERS.getLegacyNum()) {
                yResol = 3.937e-1f * yResol;
            } else if (resYUnit == Unit.METERS.getLegacyNum()) {
                yResol = 39.37f * yResol;
            } else if (resYUnit == Unit.KILOMETERS.getLegacyNum()) {
                yResol = 3.937e4f * yResol;
            } else if (resYUnit == Unit.MILES.getLegacyNum()) {
                yResol = 63360.0f * yResol;
            } else if (resYUnit == Unit.MILS.getLegacyNum()) {
                yResol = 1.0e-3f * yResol;
            }
        } // else if (resXYUnit == Unit.INCHES.getLegacyNum())


        if (image.getFileInfo(index).getResolutions().length > 2) {
            zRes = (double) (image.getFileInfo(index).getResolutions()[2]);
        }

        if (image.getFileInfo(index).getResolutions().length > 3) {
            tRes = (double) (image.getFileInfo(index).getResolutions()[3]);
        }

        if (zRes >= 0.0) {
            zResCount = 8;
            ztEntries++;
        }

        if (tRes >= 0.0) {
            ztEntries++;
        }

        type = image.getFileInfo(index).getDataType();

        switch (type) {

            case ModelStorageBase.BOOLEAN:
                bitsPerSample = 1;
                bytesPerSample = 1;
                samplesPerPixel = 1;
                nDirEntries = (short) (10 + ztEntries);
                rgbCount = 0;
                break;

            case ModelStorageBase.BYTE:
                bitsPerSample = 8;
                bytesPerSample = 1;
                samplesPerPixel = 1;
                if (image.getFileInfo(index).getPhotometric() == 3) {
                    nDirEntries = (short) (12 + ztEntries); // Add one for color map
                } else {
                    nDirEntries = (short) (11 + ztEntries);
                }

                rgbCount = 0;
                break;

            case ModelStorageBase.UBYTE:
                bitsPerSample = 8;
                bytesPerSample = 1;
                samplesPerPixel = 1;
                if (image.getFileInfo(index).getPhotometric() == 3) {
                    nDirEntries = (short) (12 + ztEntries); // Add one for color map
                } else {
                    nDirEntries = (short) (11 + ztEntries);
                }

                rgbCount = 0;
                break;

            case ModelStorageBase.USHORT:
            case ModelStorageBase.SHORT:
                bytesPerSample = 2;
                samplesPerPixel = 1;
                bitsPerSample = 16;
                nDirEntries = (short) (11 + ztEntries);
                rgbCount = 0;
                break;

            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
                bytesPerSample = 4;
                samplesPerPixel = 1;
                bitsPerSample = 32;
                nDirEntries = (short) (11 + ztEntries);
                rgbCount = 0;
                break;

            case ModelStorageBase.ARGB:
                bitsPerSample = 8;
                bytesPerSample = 1; // since SamplesPerPixel is defined as 3 for RGB images
                samplesPerPixel = 3;
                nDirEntries = (short) (13 + ztEntries);

                // RGB stores 8,8,8 for bitsPerSample
                bitsPerSampleOffset = (int) raFile.getFilePointer() + (2 + (nDirEntries * 12) + 4 + resolutionCount);
                rgbCount = 6;
                break;

            default:
                throw new IOException("Unsupported ModelStoragebase type\n");
        }

        resolutionOffset = (int) raFile.getFilePointer() + (2 + (nDirEntries * 12) + 4);
        zResOffset = (int) raFile.getFilePointer() + (2 + (nDirEntries * 12) + 4 + resolutionCount + rgbCount);
        tResOffset = (int) raFile.getFilePointer() +
                     (2 + (nDirEntries * 12) + 4 + resolutionCount + rgbCount + zResCount);

        writeShort(nDirEntries, endianess);

        writeIFD(IMAGE_WIDTH, SHORT, 1, image.getExtents()[0], 0);

        writeIFD(IMAGE_LENGTH, SHORT, 1, image.getExtents()[1], 0);

        if (type != ModelStorageBase.BOOLEAN) {

            if (type == ModelStorageBase.ARGB) {
                writeIFD(BITS_PER_SAMPLE, SHORT, 3, bitsPerSampleOffset, 0);
            } else {
                writeIFD(BITS_PER_SAMPLE, SHORT, 1, bitsPerSample, 0);
            }
        }

        if (writePackBit == false) {
            writeIFD(COMPRESSION, SHORT, 1, 1, 0);
        } else {
            writeIFD(COMPRESSION, SHORT, 1, (short) 32773, 0);
        }

        writeIFD(PHOTO_INTERP, SHORT, 1, image.getFileInfo(index).getPhotometric(), 0);

        writeIFD(STRIP_OFFSETS, LONG, 1, imageOffset, 0);

        if (type == ModelStorageBase.ARGB) {
            writeIFD(SAMPLES_PER_PIXEL, SHORT, 1, 3, 0);
        }

        writeIFD(ROWS_PER_STRIP, LONG, 1, image.getExtents()[1], 0);

        writeIFD(STRIP_BYTE_COUNTS, LONG, 1, theStripCount * bytesPerSample * samplesPerPixel, 0);

        writeIFD(XRESOLUTION, RATIONAL, 1, resolutionOffset, 0);
        writeIFD(YRESOLUTION, RATIONAL, 1, resolutionOffset + 8, 0);

        if (zRes >= 0.0) {
            writeIFD(ZRESOLUTION, DOUBLE, 1, zResOffset, 0);
        }

        if (tRes >= 0.0) {
            writeIFD(TRESOLUTION, DOUBLE, 1, tResOffset, 0);
        }


        if (type == ModelStorageBase.ARGB) {
            writeIFD(PLANAR_CONFIG, SHORT, 1, 1, 0); // chucky format (rgb,rgb,rgb ...) baseline tiff
        }

        if (resXYUnit == Unit.INCHES.getLegacyNum()) {
            writeIFD(RESOLUTION_UNIT, SHORT, 1, 2, 0);
        }
        else if (resXYUnit == Unit.CENTIMETERS.getLegacyNum()) {
        	writeIFD(RESOLUTION_UNIT, SHORT, 1, 3, 0);	
        }
        else {
        	writeIFD(RESOLUTION_UNIT, SHORT, 1, 1, 0);		
        }

        if ((LUT != null) && ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE)) &&
                (image.getFileInfo(index).getPhotometric() == 3)) {
            writeIFD(COLOR_MAP, SHORT, 768, LUTOffset, 0);
        }

        writeInt(nextIFD, endianess);

        int numerator, denominator;
        numerator = (int) (1000000 * xResol);
        denominator = 1000000;
        writeInt(denominator, endianess); // xResolution - RATIONAL
        writeInt(numerator, endianess); // xResolution - RATIONAL

        numerator = (int) (1000000 * yResol);
        denominator = 1000000;
        writeInt(denominator, endianess); // yResolution - RATIONAL
        writeInt(numerator, endianess); // yResolution - RATIONAL

        if (type == ModelStorageBase.ARGB) {
            writeShort((short) 8, endianess); // RGB bitsPerSample ( R plane)
            writeShort((short) 8, endianess); // RGB bitsPerSample ( G plane)
            writeShort((short) 8, endianess); // RGB bitsPerSample ( B plane)
        }

        if (zRes >= 0.0) {
            writeDouble(zRes, endianess);
        }

        if (tRes >= 0.0) {
            writeDouble(tRes, endianess);
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Simple class to store image offsets and bytes located at the offset.
     */
    private class Index {

        /** DOCUMENT ME! */
        public int byteCount = 0;

        /** DOCUMENT ME! */
        public int index = 0;

        /**
         * Creates a new Index object.
         *
         * @param  _index  DOCUMENT ME!
         */
        public Index(int _index) {
            index = _index;
        }
    }

}
