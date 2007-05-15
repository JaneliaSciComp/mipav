package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

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
    private int LUTOffset;

    

    /** DOCUMENT ME! */
    private short nDirEntries;

    /** DOCUMENT ME! */
    private int samplesPerPixel = 1;

    
    /** DOCUMENT ME! */
    private double tRes = 1.0;

    

    /** DOCUMENT ME! */
    private double zRes = 1.0;

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
        int ztEntries;
        int zResCount;
        int tResCount = 0; // set to 8 if EchoTech tResolution field present
        int resolutionCount = 16; // xResolution = 2 * (4 bytes) + yResolution = 2 * (4 bytes)
        FileInfoBase fileInfo[];
        int type;
        int bytesPerSample;
        int rgbCount = 0; // Set to 6 for storage of 3 short bitsPerSample values

        // in ARGB, ARGB_USHORT, and ARGB_FLOAT
        int rgbFormat = 0; // Set to 6 for storage of 3 short sampleFormat values

        // in ARGB, ARGB_USHORT, and ARGB_FLOAT
        int intAlign = 0; // For integer data image rows must begin on integer boundaries, so set

        // intAlign = 2 if ModelStorageBase.INTEGER is used so that imgOffset is
        // always a multiple of 4.
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
        
        try {
            extents[0] = 512;
            extents[1] = 512;
            extents[2] = 129;
            bufferSize = extents[0] * extents[1];
            beginSlice = 0;
            endSlice = extents[2] - 1;
            
            index = fileName.indexOf(".");
            prefix = fileName.substring(0, index); // Used for setting file name
            type = ModelStorageBase.ARGB;
            image =  new ModelImage(type, extents, prefix);
            fileInfo = image.getFileInfo();
            resolutions[0] = 1.0E-5f;
            resolutions[1] = 1.0E-5f;
            resolutions[2] = 4.0E-5f;
            units[0] = FileInfoBase.CENTIMETERS;
            units[1] = FileInfoBase.CENTIMETERS;
            units[2] = FileInfoBase.CENTIMETERS;
            for (i = 0; i < extents[2]; i++) {
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setUnitsOfMeasure(units);
                fileInfo[i].setEndianess(FileBase.LITTLE_ENDIAN);
                fileInfo[i].setCompressionType(FileInfoBase.COMPRESSION_NONE);
                fileInfo[i].setFileFormat(FileUtility.TIFF);
                fileInfo[i].setPhotometric((short)2);
            }
            radiusSquared = 248 * 248;
            buffer = new byte[4 * bufferSize * extents[2]];
            // Red chromosome 1
            for (z = 62; z <= 66; z++) {
                for (y = 255 - 18; y <= 255 + 18; y++) {
                    for (x = 255 + 45; x <= 255 + 53; x++) {
                        buffer[1 + 4*(x + extents[0]*y + bufferSize*z)] = (byte)255;
                    }
                }
            }
            // Green chromosome 1
            for (z = 62; z <= 66; z++) {
                for (y = 255 - 18; y <= 255 + 18; y++) {
                    for (x = 255 - 53; x <= 255 - 45; x++) {
                        buffer[2 + 4*(x + extents[0]*y + bufferSize*z)] = (byte)255;
                    }
                }
            }
            // Red chromosome 2
            for (z = 61; z <= 67; z++) {
                for (y = 255 + 145; y <= 255 + 165; y++) {
                    for (x = 255 - 40; x <= 255 + 40; x++) {
                        buffer[1 + 4*(x + extents[0]*y + bufferSize*z)] = (byte)255;
                    }
                }
            }
            // Green chromosome 2
            for (z = 61; z <= 67; z++) {
                for (y = 255 - 165; y <= 255 - 145; y++) {
                    for (x = 255 - 40; x <= 255 + 40; x++) {
                        buffer[2 + 4*(x + extents[0]*y + bufferSize*z)] = (byte)255;
                    }
                }
            }
            // Blue cell
            for (z = 0; z <= 128; z++) {
                zSquared = z - 64;
                zSquared = 16 * zSquared * zSquared;
                for (y = 0; y <= 511; y++) {
                    ySquared = y - 255;
                    ySquared = ySquared * ySquared;
                    for (x = 0; x <= 511; x++) {
                        xSquared = x - 255;
                        xSquared = xSquared * xSquared;
                        distSquared  = xSquared + ySquared + zSquared;
                        if (distSquared <= radiusSquared) {
                            pos = 4*(x + extents[0]*y + bufferSize*z);
                            if ((buffer[1 + pos] == 0) && (buffer[2 + pos] == 0)) {
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

            
            zResCount = 8;
            ztEntries = 1;
            
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "rw");
            raFile.setLength(0); // necessary so that if this is an overwritten file there isn't junk at the
                                 // end
            fileRW = new FileRawChunk(raFile, fileInfo[0]);
            
            bytesPerSample = 1; // since SamplesPerPixel is defined as 3 for RGB images
            samplesPerPixel = 3;
            nDirEntries = (short) (14 + ztEntries);
            rgbCount = 6;
            rgbFormat = 6;
            intAlign = 0;
            
            writeHeader();
            nextIFD = 8;
            imgOffset = 0;


                for (k = beginSlice, m = 0; k <= endSlice; k++, m++) {
                    fireProgressStateChanged(Math.round((float) (k - beginSlice + 1) /
                                                            (endSlice - beginSlice + 1) *
                                                            100));

                    
                    stripCount = image.getSliceSize();

                    if (k == endSlice) {
                        nextIFD = 0;
                    } else {
                        nextIFD += ((2 + (nDirEntries * 12) + resolutionCount + 4 + rgbCount + rgbFormat +
                                     intAlign + zResCount + tResCount) +
                                    (bufferSize * bytesPerSample * samplesPerPixel));
                    }

                    imgOffset = 8 +
                                ((m + 1) *
                                     (2 + (nDirEntries * 12) + resolutionCount + 4 + rgbCount + rgbFormat +
                                          intAlign + zResCount + tResCount)) +
                                (m * bufferSize * bytesPerSample * samplesPerPixel);
                    

                    Preferences.debug("Image name = " + image.getImageName() + "\n", Preferences.DEBUG_FILEIO);
                    writeIFDs(image, imgOffset, nextIFD, k, stripCount, false);

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
    private void writeIFDs(ModelImage image, int imageOffset, int nextIFD, int index, int theStripCount,
                           boolean writePackBit) throws IOException {
        boolean endianess = image.getFileInfo(0).getEndianess();
        int type;
        int bitsPerSample;
        int bytesPerSample = 2;
        int samplesPerPixel;
        int bitsPerSampleOffset = 0;
        int sampleFormatOffset = 0;
        int resolutionOffset;
        int ztEntries = 0;
        int zResOffset = 0;
        int tResOffset = 0;
        int resolutionCount = 16;
        int zResCount = 0;
        int tResCount = 0;
        int rgbCount = 0;
        int resXYUnit = 0;
        int resYUnit = 0;
        int resZUnit = 0;
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

        if ((resXYUnit != FileInfoBase.UNKNOWN_MEASURE) && (resXYUnit != FileInfoBase.INCHES) &&
                (resXYUnit != FileInfoBase.CENTIMETERS) && (resXYUnit != FileInfoBase.MILLIMETERS) &&
                (resXYUnit != FileInfoBase.METERS) && (resXYUnit != FileInfoBase.ANGSTROMS) &&
                (resXYUnit != FileInfoBase.NANOMETERS) && (resXYUnit != FileInfoBase.MICROMETERS) &&
                (resXYUnit != FileInfoBase.MILES) && (resXYUnit != FileInfoBase.KILOMETERS)) {
            resXYUnit = FileInfoBase.UNKNOWN_MEASURE;
        }

        if (resXYUnit == FileInfoBase.ANGSTROMS) {
            resXYUnit = FileInfoBase.CENTIMETERS;
            xResol = 1.0e-8f * xResol;
        } else if (resXYUnit == FileInfoBase.NANOMETERS) {
            resXYUnit = FileInfoBase.CENTIMETERS;
            xResol = 1.0e-7f * xResol;
        } else if (resXYUnit == FileInfoBase.MICROMETERS) {
            resXYUnit = FileInfoBase.CENTIMETERS;
            xResol = 1.0e-4f * xResol;
        } else if (resXYUnit == FileInfoBase.MILLIMETERS) {
            resXYUnit = FileInfoBase.CENTIMETERS;
            xResol = 0.1f * xResol;
        } else if (resXYUnit == FileInfoBase.METERS) {
            resXYUnit = FileInfoBase.CENTIMETERS;
            xResol = 100.0f * xResol;
        } else if (resXYUnit == FileInfoBase.KILOMETERS) {
            resXYUnit = FileInfoBase.CENTIMETERS;
            xResol = 1.0e5f * xResol;
        } else if (resXYUnit == FileInfoBase.MILES) {
            resXYUnit = FileInfoBase.INCHES;
            xResol = 63360.0f * xResol;
        }

        if (resXYUnit == FileInfoBase.CENTIMETERS) {

            // Change the Y resolution units to centimeters
            if (resYUnit == FileInfoBase.ANGSTROMS) {
                yResol = 1.0e-8f * yResol;
            } else if (resYUnit == FileInfoBase.NANOMETERS) {
                yResol = 1.0e-7f * yResol;
            } else if (resYUnit == FileInfoBase.MICROMETERS) {
                yResol = 1.0e-4f * yResol;
            } else if (resYUnit == FileInfoBase.MILLIMETERS) {
                yResol = 0.1f * yResol;
            } else if (resYUnit == FileInfoBase.INCHES) {
                yResol = 2.54f * yResol;
            } else if (resYUnit == FileInfoBase.METERS) {
                yResol = 100.0f * yResol;
            } else if (resYUnit == FileInfoBase.KILOMETERS) {
                yResol = 1.0e5f * yResol;
            } else if (resYUnit == FileInfoBase.MILES) {
                yResol = 1.6093e5f * yResol;
            }
        } // if (resXYUnit == FileInfoBase.CENTIMETERS)
        else if (resXYUnit == FileInfoBase.INCHES) {

            // Change the Y resolution units to inches
            if (resYUnit == FileInfoBase.ANGSTROMS) {
                yResol = 3.937e-9f * yResol;
            } else if (resYUnit == FileInfoBase.NANOMETERS) {
                yResol = 3.937e-8f * yResol;
            } else if (resYUnit == FileInfoBase.MICROMETERS) {
                yResol = 3.937e-5f * yResol;
            } else if (resYUnit == FileInfoBase.MILLIMETERS) {
                yResol = 3.937e-2f * yResol;
            } else if (resYUnit == FileInfoBase.CENTIMETERS) {
                yResol = 3.937e-1f * yResol;
            } else if (resYUnit == FileInfoBase.METERS) {
                yResol = 39.37f * yResol;
            } else if (resYUnit == FileInfoBase.KILOMETERS) {
                yResol = 3.937e4f * yResol;
            } else if (resYUnit == FileInfoBase.MILES) {
                yResol = 63360.0f * yResol;
            }
        } // else if (resXYUnit == FileInfoBase.INCHES)

        zRes = -1.0;

        if ((image.getNDims() > 2) && (image.getFileInfo(index).getResolutions().length > 2)) {
            zRes = (double) (image.getFileInfo(index).getResolutions()[2]);
            resZUnit = image.getFileInfo(index).getUnitsOfMeasure(2);

            // The EchoTech standard uses mm for the ResolutionZ field,
            // so convert to millimeters
            if (resZUnit == FileInfoBase.METERS) {
                zRes = 1000.0f * zRes;
            } else if (resZUnit == FileInfoBase.CENTIMETERS) {
                zRes = 10.0f * zRes;
            } else if (resZUnit == FileInfoBase.INCHES) {
                zRes = 25.4f * zRes;
            } else if (resZUnit == FileInfoBase.MICROMETERS) {
                zRes = 1.0e-3f * zRes;
            }
        }

        tRes = -1.0;

        if ((image.getNDims() > 3) && (image.getFileInfo(index).getResolutions().length > 3)) {
            tRes = (double) (image.getFileInfo(index).getResolutions()[3]);
        }

        if (zRes >= 0.0) {
            zResCount = 8;
            ztEntries++;
        }

        if (tRes >= 0.0) {
            tResCount = 8;
            ztEntries++;
        }

        type = image.getFileInfo(index).getDataType();

        switch (type) {

            case ModelStorageBase.BOOLEAN:
                bitsPerSample = 1;
                bytesPerSample = 1;
                samplesPerPixel = 1;
                nDirEntries = (short) (11 + ztEntries);
                rgbCount = 0;
                break;

            case ModelStorageBase.BYTE:
                bitsPerSample = 8;
                bytesPerSample = 1;
                samplesPerPixel = 1;
                if (image.getFileInfo(index).getPhotometric() == 3) {
                    nDirEntries = (short) (13 + ztEntries); // Add one for color map
                } else {
                    nDirEntries = (short) (12 + ztEntries);
                }

                rgbCount = 0;
                break;

            case ModelStorageBase.UBYTE:
                bitsPerSample = 8;
                bytesPerSample = 1;
                samplesPerPixel = 1;
                if (image.getFileInfo(index).getPhotometric() == 3) {
                    nDirEntries = (short) (13 + ztEntries); // Add one for color map
                } else {
                    nDirEntries = (short) (12 + ztEntries);
                }

                rgbCount = 0;
                break;

            case ModelStorageBase.USHORT:
            case ModelStorageBase.SHORT:
                bytesPerSample = 2;
                samplesPerPixel = 1;
                bitsPerSample = 16;
                nDirEntries = (short) (12 + ztEntries);
                rgbCount = 0;
                break;

            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
            case ModelStorageBase.FLOAT:
                bytesPerSample = 4;
                samplesPerPixel = 1;
                bitsPerSample = 32;
                nDirEntries = (short) (12 + ztEntries);
                rgbCount = 0;
                break;

            case ModelStorageBase.DOUBLE:
                bytesPerSample = 8;
                samplesPerPixel = 1;
                bitsPerSample = 64;
                nDirEntries = (short) (12 + ztEntries);
                rgbCount = 0;
                break;

            case ModelStorageBase.ARGB:
                bitsPerSample = 8;
                bytesPerSample = 1; // since SamplesPerPixel is defined as 3 for RGB images
                samplesPerPixel = 3;
                nDirEntries = (short) (14 + ztEntries);

                // RGB stores 8,8,8 for bitsPerSample
                // RGB stores 1,1,1 for sampleFormat
                bitsPerSampleOffset = (int) raFile.getFilePointer() + (2 + (nDirEntries * 12) + 4 + resolutionCount);
                rgbCount = 6;
                break;

            case ModelStorageBase.ARGB_USHORT:
                bitsPerSample = 16;
                bytesPerSample = 2; // since SamplesPerPixel is defined as 3 for RGB images
                samplesPerPixel = 3;
                nDirEntries = (short) (14 + ztEntries);

                // RGB stores 16,16,16 for bitsPerSample
                // RGB stores 1,1,1 for sampleFormat
                bitsPerSampleOffset = (int) raFile.getFilePointer() + (2 + (nDirEntries * 12) + 4 + resolutionCount);
                rgbCount = 6;
                break;

            default:
                throw new IOException("Unsupported ModelStorageBase type\n");
        }

        resolutionOffset = (int) raFile.getFilePointer() + (2 + (nDirEntries * 12) + 4);
        zResOffset = (int) raFile.getFilePointer() + (2 + (nDirEntries * 12) + 4 + resolutionCount + rgbCount);
        tResOffset = (int) raFile.getFilePointer() +
                     (2 + (nDirEntries * 12) + 4 + resolutionCount + rgbCount + zResCount);
        sampleFormatOffset = (int) raFile.getFilePointer() +
                             (2 + (nDirEntries * 12) + 4 + resolutionCount + rgbCount + zResCount + tResCount);
        writeShort(nDirEntries, endianess);
        writeIFD(IMAGE_WIDTH, SHORT, 1, image.getExtents()[0], 0);
        writeIFD(IMAGE_LENGTH, SHORT, 1, image.getExtents()[1], 0);

        if (type != ModelStorageBase.BOOLEAN) {

            if ((type == ModelStorageBase.ARGB) || (type == ModelStorageBase.ARGB_USHORT)) {
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

        if ((type == ModelStorageBase.ARGB) || (type == ModelStorageBase.ARGB_USHORT)) {
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

        if ((type == ModelStorageBase.ARGB) || (type == ModelStorageBase.ARGB_USHORT)) {
            writeIFD(PLANAR_CONFIG, SHORT, 1, 1, 0); // chucky format (rgb,rgb,rgb ...) baseline tiff
        }

        writeIFD(RESOLUTION_UNIT, SHORT, 1, resXYUnit, 0);

        if ((LUT != null) && ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE)) &&
                (image.getFileInfo(index).getPhotometric() == 3)) {
            writeIFD(COLOR_MAP, SHORT, 768, LUTOffset, 0);
        }

        if ((type == ModelStorageBase.BOOLEAN) || (type == ModelStorageBase.UBYTE) ||
                (type == ModelStorageBase.USHORT) || (type == ModelStorageBase.UINTEGER)) {
            writeIFD(SAMPLE_FORMAT, SHORT, 1, 1, 0);
        } else if ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.SHORT) ||
                       (type == ModelStorageBase.INTEGER)) {
            writeIFD(SAMPLE_FORMAT, SHORT, 1, 2, 0);
        } else if ((type == ModelStorageBase.ARGB) || (type == ModelStorageBase.ARGB_USHORT)) {
            writeIFD(SAMPLE_FORMAT, SHORT, 3, sampleFormatOffset, 0);
        } else if ((type == ModelStorageBase.FLOAT) || (type == ModelStorageBase.DOUBLE)) {
            writeIFD(SAMPLE_FORMAT, SHORT, 1, 3, 0);
        } else if ((type == ModelStorageBase.ARGB_FLOAT)) {
            writeIFD(SAMPLE_FORMAT, SHORT, 3, sampleFormatOffset, 0);
        }

        writeInt(nextIFD, endianess);

        int numerator, denominator;
        int scale;

        // Largest int is 2,147,483,647
        scale = (int) Math.min(2.0e9 / xResol, 2.0e9);
        numerator = (int) (scale * xResol);
        denominator = scale;
        writeInt(denominator, endianess); // xResolution - RATIONAL
        writeInt(numerator, endianess); // xResolution - RATIONAL
        scale = (int) Math.min(2.0e9 / yResol, 2.0e9);
        numerator = (int) (scale * yResol);
        denominator = scale;
        writeInt(denominator, endianess); // yResolution - RATIONAL
        writeInt(numerator, endianess); // yResolution - RATIONAL

        if (type == ModelStorageBase.ARGB) {
            writeShort((short) 8, endianess); // RGB bitsPerSample ( R plane)
            writeShort((short) 8, endianess); // RGB bitsPerSample ( G plane)
            writeShort((short) 8, endianess); // RGB bitsPerSample ( B plane)
        } else if (type == ModelStorageBase.ARGB_USHORT) {
            writeShort((short) 16, endianess); // RGB bitsPerSample ( R plane)
            writeShort((short) 16, endianess); // RGB bitsPerSample ( G plane)
            writeShort((short) 16, endianess); // RGB bitsPerSample ( B plane)
        }

        if (zRes >= 0.0) {
            writeDouble(zRes, endianess);
        }

        if (tRes >= 0.0) {
            writeDouble(tRes, endianess);
        }

        if ((type == ModelStorageBase.ARGB) || (type == ModelStorageBase.ARGB_USHORT)) {
            writeShort((short) 1, endianess);
            writeShort((short) 1, endianess);
            writeShort((short) 1, endianess);
        } else if (type == ModelStorageBase.ARGB_FLOAT) {
            writeShort((short) 3, endianess);
            writeShort((short) 3, endianess);
            writeShort((short) 3, endianess);
        }
    }

    
}
