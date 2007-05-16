package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Write a simple cell model with a blue cell and red and green chromosomes using TIFF format and
 * possibly LSM extensions
 */
public class WriteCellModel extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Tiff Types. */
    public static final int BYTE = 1; // 8 bit unsigned

    /** DOCUMENT ME! */
    public static final int ASCII = 2; // 7 bit ASCII

    /** DOCUMENT ME! */
    public static final int SHORT = 3; // 16 bit unsigned

    /** DOCUMENT ME! */
    public static final int LONG = 4; // 32 bit unsigned ****** 4 bytes !!!!

    /** DOCUMENT ME! */
    public static final int RATIONAL = 5; // 2 longs 1st numorator

    /** 2nd denom. */
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

    /** Tiff Tags. */
    public static final int NEW_SUBFILE_TYPE = 254;

    /** DOCUMENT ME! */
    public static final int IMAGE_WIDTH = 256;

    /** DOCUMENT ME! */
    public static final int IMAGE_LENGTH = 257;

    /** DOCUMENT ME! */
    public static final int BITS_PER_SAMPLE = 258;

    /** Compression: 1 = no compression 2 = modified huffman 3 = CCITT-T4 4 = CCITT-T6 5 = LZW 32773 = packbits. */
    public static final int COMPRESSION = 259;

    /** DOCUMENT ME! */
    public static final int PHOTO_INTERP = 262;

    /** DOCUMENT ME! */
    public static final int IMAGE_DESCRIPTION = 270;

    /** DOCUMENT ME! */
    public static final int STRIP_OFFSETS = 273;

    /** DOCUMENT ME! */
    public static final int ORIENTATION = 274;

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
    public static final int ARTIST = 315;

    /** DOCUMENT ME! */
    public static final int HOST_COMPUTER = 316;

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

    /** EchoTech Tiff TAGS. */
    public static final int ZRESOLUTION = 65000;

    /** DOCUMENT ME! */
    public static final int TRESOLUTION = 65001;
    
    /** DOCUMENT ME! */
    private static final int CZ_LSMINFO = 34412;

    /** DOCUMENT ME! */
    public static final int MAX_IFD_LENGTH = 4096;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The file... */
    private File file;

    /** File directory. */
    private String fileDir;

    /** File Info. */
    private FileInfoTiff fileInfo;

    /** File name. */
    private String fileName;


    /** DOCUMENT ME! */
    private FileRawChunk fileRW;

    
    /** DOCUMENT ME! */
    private ModelImage image;

    

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

   

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;
   
    /** DOCUMENT ME! */
    private short nDirEntries;

    /** DOCUMENT ME! */
    private int samplesPerPixel = 1;

    
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Tiff reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public WriteCellModel(String fileName, String fileDir) throws IOException {
        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {

        imgBuffer = null;

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
     * Accessor to set the file dir (used when reading TIFF multiFile).
     *
     * @param  fDir  file dir of image to read.
     */
    public void setFileDir(String fDir) {
        fileDir = fDir;
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
     * Use this routine with the following lines:
     * try {
                WriteCellModel wcm = new WriteCellModel("cellModel.lsm","C:/images/");
                wcm.writeCellModel();
                ViewUserInterface.getReference().setDataText("Wrote C:/images/cellModel.lsm");
            }
            catch (IOException error) {
                displayError("An IO Exception occurred");
            }
            return;
     * @throws IOException
     */
    public void writeCellModel() throws IOException {
        int extents[] = new int[3];
        int bufferSize;
        int index;
        String prefix;
        FileInfoBase fileInfo[];
        int type;
        int bytesPerSample;
        int rgbCount = 0; // Set to 6 for storage of 3 short bitsPerSample values

        // in ARGB, ARGB_USHORT, and ARGB_FLOAT
        int rgbFormat = 0; // Set to 6 for storage of 3 short sampleFormat values

        
        int imgOffset;
        int nextIFD;
        int stripCount;
        int k;
        int m;
        int timeOffset = 0; // used to offset for writing one time slice of a 4D image 
        int beginSlice;
        int endSlice;
        byte[] buffer;
        int i;
        float resolutions[] = new float[3];
        int units[] = new int[3];
        int x;
        int y;
        int z;
        int radiusSquared;
        int zSquared;
        int ySquared;
        int xSquared;
        int distSquared;
        int pos;
        RandomNumberGen randomGen = new RandomNumberGen();
        int lsmStackLength = 224;
        
        try {
            extents[0] = 256;
            extents[1] = 256;
            extents[2] = 65;
            bufferSize = extents[0] * extents[1];
            beginSlice = 0;
            endSlice = extents[2] - 1;
            
            index = fileName.indexOf(".");
            prefix = fileName.substring(0, index); // Used for setting file name
            type = ModelStorageBase.ARGB;
            image =  new ModelImage(type, extents, prefix);
            fileInfo = image.getFileInfo();
            resolutions[0] = 1.0E-7f;
            resolutions[1] = 1.0E-7f;
            resolutions[2] = 4.0E-7f;
            units[0] = FileInfoBase.METERS;
            units[1] = FileInfoBase.METERS;
            units[2] = FileInfoBase.METERS;
            for (i = 0; i < extents[2]; i++) {
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setUnitsOfMeasure(units);
                fileInfo[i].setEndianess(FileBase.LITTLE_ENDIAN);
                fileInfo[i].setCompressionType(FileInfoBase.COMPRESSION_NONE);
                fileInfo[i].setFileFormat(FileUtility.LSM);
                fileInfo[i].setPhotometric((short)2);
            }
            radiusSquared = 124 * 124;
            buffer = new byte[4 * bufferSize * extents[2]];
            // Red chromosome 1
            for (z = 30; z <= 34; z++) {
                for (y = 127 - 18; y <= 127 + 18; y++) {
                    for (x = 127 + 45; x <= 127 + 53; x++) {
                        buffer[1 + 4*(x + extents[0]*y + bufferSize*z)] = (byte)255;
                    }
                }
            }
            // Green chromosome 1
            for (z = 30; z <= 34; z++) {
                for (y = 127 - 18; y <= 127 + 18; y++) {
                    for (x = 127 - 53; x <= 127 - 45; x++) {
                        buffer[2 + 4*(x + extents[0]*y + bufferSize*z)] = (byte)255;
                    }
                }
            }
            // Red chromosome 2
            for (z = 29; z <= 35; z++) {
                for (y = 127 + 65; y <= 127 + 85; y++) {
                    for (x = 127 - 40; x <= 127 + 40; x++) {
                        buffer[1 + 4*(x + extents[0]*y + bufferSize*z)] = (byte)255;
                    }
                }
            }
            // Green chromosome 2
            for (z = 29; z <= 35; z++) {
                for (y = 127 - 85; y <= 127 - 65; y++) {
                    for (x = 127 - 40; x <= 127 + 40; x++) {
                        buffer[2 + 4*(x + extents[0]*y + bufferSize*z)] = (byte)255;
                    }
                }
            }
            // Blue cell
            for (z = 0; z <= 64; z++) {
                zSquared = z - 32;
                zSquared = 16 * zSquared * zSquared;
                for (y = 0; y <= 255; y++) {
                    ySquared = y - 127;
                    ySquared = ySquared * ySquared;
                    for (x = 0; x <= 255; x++) {
                        xSquared = x - 127;
                        xSquared = xSquared * xSquared;
                        distSquared  = xSquared + ySquared + zSquared;
                        if (distSquared <= radiusSquared) {
                            pos = 4*(x + extents[0]*y + bufferSize*z);
                            if ((buffer[1 + pos] != (byte)255) && (buffer[2 + pos] != (byte)255)) {
                                buffer[1 + pos] = (byte)randomGen.genGaussianRandomNum(0,200);
                                buffer[2 + pos] = (byte)randomGen.genGaussianRandomNum(0,200);
                                buffer[3 + pos] = (byte)255;
                            }
                        }
                    }
                }
            }
            try {
                image.importData(0, buffer, true);
            }
            catch (IOException error) {
                throw error;
            }
            
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "rw");
            raFile.setLength(0); // necessary so that if this is an overwritten file there isn't junk at the
                                 // end
            fileRW = new FileRawChunk(raFile, fileInfo[0]);
            
            bytesPerSample = 1; // since SamplesPerPixel is defined as 3 for RGB images
            samplesPerPixel = 3;
            rgbCount = 6;
            rgbFormat = 6;
            
            writeHeader();
            nextIFD = 8;
            imgOffset = 0;

                for (k = beginSlice, m = 0; k <= endSlice; k++, m++) {
                    if (m == 0) {
                        nDirEntries = (short)12;
                    }
                    else {
                        nDirEntries = (short)11;
                    }
                    fireProgressStateChanged(Math.round((float) (k - beginSlice + 1) /
                                                            (endSlice - beginSlice + 1) *
                                                            100));

                    
                    stripCount = image.getSliceSize();

                    if (k == endSlice) {
                        nextIFD = 0;
                    } else if (k == 0) {
                        nextIFD += ((2 + (nDirEntries * 12) + 4 + rgbCount + rgbFormat) +
                                (bufferSize * bytesPerSample * samplesPerPixel) + lsmStackLength);    
                    } else {
                        nextIFD += ((2 + (nDirEntries * 12) + 4 + rgbCount + rgbFormat) +
                                    (bufferSize * bytesPerSample * samplesPerPixel));
                    }

                    imgOffset = 8 +
                                 (2 + (12 * 12) + 4 + rgbCount + rgbFormat + lsmStackLength) +
                                (m  * (2 + (11 * 12) + 4 + rgbCount + rgbFormat)) +
                                (m * bufferSize * bytesPerSample * samplesPerPixel);
                    

                    Preferences.debug("Image name = " + image.getImageName() + "\n", Preferences.DEBUG_FILEIO);
                    writeIFDs(image, imgOffset, nextIFD, k, stripCount);

                    try {

                        // adjust for intAlign ????
                        fileRW.writeImage(image, timeOffset + (k * bufferSize),
                                          timeOffset + (k * bufferSize) + bufferSize, 0);
                        
                    } catch (IOException error) {
                        throw error;
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
            hdr[2] = 0; // 42 magic number
            hdr[3] = 42;
            hdr[4] = 0; // 8 (offset to first IFD)
            hdr[5] = 0;
            hdr[6] = 0;
            hdr[7] = 8;
        } else {
            hdr[0] = 73; // "49" little endian
            hdr[1] = 73;
            hdr[2] = 42; // 42 magic number
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
     * @param               image          offset to next IFD. If equal to zero then end of images
     * @param               imageOffset    DOCUMENT ME!
     * @param               nextIFD        DOCUMENT ME!
     * @param               index          image index for file information.
     * @param               theStripCount  DOCUMENT ME!
     * @param               writePackBit   DOCUMENT ME!
     *
     * @throws              IOException  DOCUMENT ME!
     *
     * @theStripByteCounts  number of bytes in the strip
     */
    private void writeIFDs(ModelImage image, int imageOffset, int nextIFD, int index, int theStripCount) throws IOException {
        boolean endianess = image.getFileInfo(0).getEndianess();
        int bytesPerSample = 2;
        int samplesPerPixel;
        int bitsPerSampleOffset = 0;
        int sampleFormatOffset = 0;
        int lsmStackOffset = 0;
        int lsmStackLength = 224;
        int rgbCount;
        int rgbFormat;

        bytesPerSample = 1; // since SamplesPerPixel is defined as 3 for RGB images
        samplesPerPixel = 3;
        if (index == 0) {
            nDirEntries = (short)(12);
        }
        else {
            nDirEntries = (short)(11);
        }

        // RGB stores 8,8,8 for bitsPerSample
        // RGB stores 1,1,1 for sampleFormat
        bitsPerSampleOffset = (int) raFile.getFilePointer() + (2 + (nDirEntries * 12) + 4);
        rgbCount = 6;
        rgbFormat = 6;
                

        sampleFormatOffset = (int) raFile.getFilePointer() +
                             (2 + (nDirEntries * 12) + 4 + rgbCount);
        lsmStackOffset = (int) raFile.getFilePointer() + 
                             (2 + (nDirEntries * 12) + 4 + rgbCount + rgbFormat);
        writeShort(nDirEntries, endianess);
        writeIFD(IMAGE_WIDTH, SHORT, 1, image.getExtents()[0], 0);
        writeIFD(IMAGE_LENGTH, SHORT, 1, image.getExtents()[1], 0);

        
        writeIFD(BITS_PER_SAMPLE, SHORT, 3, bitsPerSampleOffset, 0);
            

        writeIFD(COMPRESSION, SHORT, 1, 1, 0);
        

        writeIFD(PHOTO_INTERP, SHORT, 1, image.getFileInfo(index).getPhotometric(), 0);
        writeIFD(STRIP_OFFSETS, LONG, 1, imageOffset, 0);

        writeIFD(SAMPLES_PER_PIXEL, SHORT, 1, 3, 0);

        writeIFD(ROWS_PER_STRIP, LONG, 1, image.getExtents()[1], 0);
        writeIFD(STRIP_BYTE_COUNTS, LONG, 1, theStripCount * bytesPerSample * samplesPerPixel, 0);
        

        writeIFD(PLANAR_CONFIG, SHORT, 1, 1, 0); // chucky format (rgb,rgb,rgb ...) baseline tiff
        
        writeIFD(SAMPLE_FORMAT, SHORT, 3, sampleFormatOffset, 0);
        
        if (index == 0) {
            writeIFD(CZ_LSMINFO, BYTE, 1,  lsmStackOffset, 0);
        }
        

        writeInt(nextIFD, endianess);

        

        
        writeShort((short) 8, endianess); // RGB bitsPerSample ( R plane)
        writeShort((short) 8, endianess); // RGB bitsPerSample ( G plane)
        writeShort((short) 8, endianess); // RGB bitsPerSample ( B plane)
        
        writeShort((short) 1, endianess);
        writeShort((short) 1, endianess);
        writeShort((short) 1, endianess);
        
        if (index == 0) {
            // 0 -Magic number corresponding to version 1.5 to 3.0
            writeInt(0x00400494C, endianess);
            // 4 -The number of bytes in the CZ private tag
            writeInt(lsmStackLength, endianess);
            // 8
            writeInt(image.getExtents()[0], endianess);
            // 12
            writeInt(image.getExtents()[1], endianess);
            // 16
            writeInt(image.getExtents()[2], endianess);
            // 20 - Number of channels
            writeInt(3, endianess);
            // 24 -Intensity values in time direction
            writeInt(1, endianess);
            // 28 - Data type = Different CZ bit numbers for different channels
            writeInt(0, endianess);
            // 32 - Width in pixels of a thumbnail
            writeInt(0, endianess);
            // 36 - Height in pixels of a thumbnail
            writeInt(0, endianess);
            // 40 - X, Y, and Z resolutions in micrometers
            writeDouble((double)image.getFileInfo()[0].getResolutions()[0], endianess);
            // 48
            writeDouble((double)image.getFileInfo()[0].getResolutions()[1], endianess);
            // 56
            writeDouble((double)image.getFileInfo()[0].getResolutions()[2], endianess);
            // 64
            writeLong(0, endianess);
            // 72
            writeLong(0, endianess);
            // 80
            writeLong(0, endianess);
            // 88 - czScanType = Normal x-y-z-scan
            writeShort((short)0, endianess);
            // 90 - czSpectralScan = No spectral scan
            writeShort((short)0, endianess);
            // 92 - czDataType2 = Scan data
            writeInt(0, endianess);
            // 96 - OffsetVectorOverlay not present
            writeInt(0, endianess);
            // 100 - OffsetInputLut not present
            writeInt(0, endianess);
            // 104 - OffsetOutputLut not present
            writeInt(0, endianess);
            // 108 - OffsetChannelColors not present
            writeInt(0, endianess);
            // 112- time interval for the time series
            writeDouble(0.0, endianess);
            // 120 - offsetChannelDataTypes array not present
            writeInt(0, endianess);
            // 124 - offsetScanInformation not present
            writeInt(0, endianess);
            // 128 - offsetKsData not present
            writeInt(0, endianess);
            // 132 - offsetTimeStamps not present
            writeInt(0, endianess);
            // 136 - offsetEventList not present
            writeInt(0, endianess);
            // 140 - offsetRoi not present
            writeInt(0, endianess);
            // 144 - offsetBleachRoi not present
            writeInt(0, endianess);
            // 148 - No second image stored in this file
            writeInt(0, endianess);
            // 152 - displayAspectX = 0
            writeDouble(0.0, endianess);
            // 160 - displayAspectY = 0
            writeDouble(0.0, endianess);
            // 168 - displayAspectZ = 0
            writeDouble(0.0, endianess);
            // 176 - displayAspectTime = 0
            writeDouble(0.0, endianess);
            // 184 - offsetMeanOfRoisOverlay = 0
            writeInt(0, endianess);
            // 188 - offsetTopolsolineOverlay = 0
            writeInt(0, endianess);
            // 192 - offsetTopoProfileOverlay = 0
            writeInt(0, endianess);
            // 196 - offsetLinesscanOverlay = 0
            writeInt(0, endianess);
            // 200 - toolBarFlags = 0
            writeInt(0, endianess);
            // 204 - offsetChannelWavelength = 0
            writeInt(0, endianess);
            // 208 - offsetChannelFactors = 0
            writeInt(0, endianess);
            // 212 - objectiveSphereCorrection = 0
            writeDouble(0.0, endianess);
            // 220 - offsetUnmixParameters = 0
            writeInt(0, endianess);
        }
        
    }

    
}
