package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogLoadLeica.*;

import java.io.*;

import java.util.*;


/**
 * Tagged Image File Format (TIFF 6.0) reader/ writer. Only packed bit compression is supported at this time. Note that
 * although EchoTech has a tResolution field, there is no provision for 4D in TIFF.
 *
 * @version  1.0 Feb 29, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 * @author   William Gandler
 * @see      FileIO
 * @see      FileInfoTiff
 * @see      FileRaw
 */
public class FileTiff extends FileBase {

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

    /** DOCUMENT ME! */
    private byte[] artist;

    /** DOCUMENT ME! */
    private byte[] byteBuffer = null;

    /** DOCUMENT ME! */
    private boolean chunky = true;

    /** DOCUMENT ME! */
    private Vector[] dataOffsets = new Vector[4000];

    /** DOCUMENT ME! */
    private byte[] dateTime;

    /** doTile: true if tiles are used. */
    private boolean doTile = false;

    /** DOCUMENT ME! */
    private double[] doubleBuffer;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** The file... */
    private File file;

    /** File directory. */
    private String fileDir;

    /** File Info. */
    private FileInfoTiff fileInfo;

    /** File name. */
    private String fileName;

    /** DOCUMENT ME! */
    private FilePackBit filePB;

    /** DOCUMENT ME! */
    private FileRawChunk fileRW;

    /** DOCUMENT ME! */
    private boolean foundTag43314; // This tag is only present in the Scion variation of

    /** DOCUMENT ME! */
    private byte[] hostComputer;

    /** DOCUMENT ME! */
    private int[] IFDoffsets = new int[4096];

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
    private boolean lzwCompression = false; // true if the read data file has LZW compression

    /** TIFF files. */
    private TIFFLZWDecoder lzwDecoder = null; // for decoding LZW compressed images

    /** DOCUMENT ME! */
    private short nDirEntries;

    /** Pack Bit: true if the read data file has pack bit compression. */
    private boolean packBit = false;

    /** DOCUMENT ME! */
    private int predictor = 1; // prediction scheme used (needed to create LZW decoder)

    /** DOCUMENT ME! */
    private int rowsPerStrip;

    /** DOCUMENT ME! */
    private int samplesPerPixel = 1;

    /** DOCUMENT ME! */
    private double[] sliceBufferDouble = null;

    /** DOCUMENT ME! */
    private float[] sliceBufferFloat = null;

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
    private int tileMaxByteCount;

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
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private int xDim = 0;

    /** DOCUMENT ME! */
    private int yDim = 0;

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
    public FileTiff(String fileName, String fileDir) throws IOException {
        UI = ViewUserInterface.getReference();
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

        sliceBufferFloat = null;

        sliceBufferDouble = null;

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
     * @param      one        <code>true</code> if only want to read in one image of the 3D set
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean multiFile, boolean one) throws IOException {
        int[] imgExtents;


        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");

            short byteOrder = raFile.readShort();

            if (byteOrder == 0x4949) {
                endianess = FileBase.LITTLE_ENDIAN;
            } else if (byteOrder == 0x4d4d) {
                endianess = FileBase.BIG_ENDIAN;
            } else {
                raFile.close();
                throw new IOException("TIFF Read Header: Error - first 2 bytes are an illegal " + byteOrder);
            }

            int magicTIFFNumber = getUnsignedShort(endianess);

            if (magicTIFFNumber != 42) {
                raFile.close();
                throw new IOException("Tiff Read Header: Error - Invalid Magic number = " + magicTIFFNumber);
            }

            long saveLoc = raFile.getFilePointer();
            fileInfo = new FileInfoTiff(fileName, fileDir, FileUtility.TIFF); // dummy fileInfo
            fileInfo.setEndianess(endianess);
            imageSlice = 0;
            IFDoffsets[imageSlice] = getInt(endianess);

            boolean moreIFDs = true;
            imgResols = new float[5];
            Preferences.debug("\n ************** FileTiff.openIFD: Opening = " + fileName + "\n",
                              Preferences.DEBUG_FILEIO);
            tileOffsetNumber = 0;
            tileByteNumber = 0;
            foundTag43314 = false;

            while ((moreIFDs) && (!foundTag43314)) { // Find number of images!!
                raFile.seek(IFDoffsets[imageSlice]);
                moreIFDs = openIFD(fileInfo);
            }

            if (foundTag43314) {
                raFile.seek(516);
                imageSlice = getUnsignedShort(BIG_ENDIAN);
            }

            Preferences.debug("Just past init IFD read", Preferences.DEBUG_FILEIO);

            if (doTile) {
                tilesPerSlice = tilesAcross * tilesDown;
                imageSlice = tilesPerImage / tilesPerSlice;
                // System.err.println("DoTile: tilesPerSlice: " + tilesPerSlice + " imageSlice: " + imageSlice);
            } // if (doTile)
            else if (lzwCompression) {

                // set the tile width to the xDim for use in LZW Decoder
                tileWidth = xDim;
                tileLength = rowsPerStrip;
                tileOffsets = new int[dataOffsets[0].size()];
                tileByteCounts = new int[dataOffsets[0].size()];
                tileMaxByteCount = 0;

                for (int i = 0; i < tileOffsets.length; i++) {
                    tileOffsets[i] = (int) ((Index) (dataOffsets[0].elementAt(i))).index;
                    tileByteCounts[i] = (int) ((Index) (dataOffsets[0].elementAt(i))).byteCount;

                    if (tileByteCounts[i] > tileMaxByteCount) {
                        tileMaxByteCount = tileByteCounts[i];
                    }
                }

                // System.err.println("Number of tile offsets: " + tileOffsets.length);
                tilesAcross = (xDim + tileWidth - 1) / tileWidth;
                tilesDown = (yDim + tileLength - 1) / tileLength;

                // tileSize = tileWidth * tileHeight * numBands;
                tilesPerSlice = tilesAcross * tilesDown;
                // System.err.println("Tiles Across: " + tilesAcross + " tiles down: " + tilesDown);
                // System.err.println("TileMaxByteCount: " + tileMaxByteCount);
            }

            // System.err.println("Tile width: " + tileWidth + " samples per pixel: " + samplesPerPixel);
            // System.err.println("Image slice: " + imageSlice);
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
                                           fileInfo.getFileName(), UI);
                } else {
                    image = new ModelImage(fileInfo.getDataType(), imgExtents, fileInfo.getFileName(), UI);
                }
            }

            imageSlice = 0;
            raFile.seek(saveLoc);
            moreIFDs = true;

            int i = 0;
            tileOffsetNumber = 0;
            tileByteNumber = 0;

            if (!foundTag43314) {

                while (moreIFDs) {
                    fileInfo = new FileInfoTiff(fileName, fileDir, FileUtility.TIFF);
                    fileInfo.setExtents(imgExtents);
                    raFile.seek(IFDoffsets[imageSlice]);
                    moreIFDs = openIFD(fileInfo);

                    // Set the resolutions
                    fileInfo.setResolutions(imgResols);

                    if ((multiFile == false) && (one == false)) {
                        image.setFileInfo(fileInfo, i);
                    }

                    i++;
                } // while (moreIFDs)
            } // if (!foundTag43314)
            else { // foundTag43314
                fileInfo = new FileInfoTiff(fileName, fileDir, FileUtility.TIFF);
                fileInfo.setExtents(imgExtents);
                fileInfo.setDataType(image.getType());

                if ((imgExtents.length > 2) && !one) {
                    imageSlice = imgExtents[2];
                } else {
                    imageSlice = 1;
                }

                for (i = 0; i < imageSlice; i++) {

                    if (multiFile == false) {
                        image.setFileInfo(fileInfo, i);
                        dataOffsets[i] = new Vector();
                        dataOffsets[i].addElement(new Index(768 + (i * xDim * yDim)));
                        ((Index) (dataOffsets[i].elementAt(0))).byteCount = xDim * yDim;
                    }
                }
            } // else foundTag43314

            if (doTile) {
                imageSlice = tilesPerImage / tilesPerSlice;
            }

            if (one) {
                imageSlice = 1;
            }

            if (lzwCompression) {
                lzwDecoder = new TIFFLZWDecoder(tileWidth, predictor, samplesPerPixel);
                // System.err.println("Created LZW Decoder");
            }

            int bufferSize;
            int sliceSize = imgExtents[0] * imgExtents[1];

            if (imgExtents.length == 3) {
                bufferSize = imgExtents[0] * imgExtents[1] * imgExtents[2];
            } else {
                bufferSize = imgExtents[0] * imgExtents[1];
            }

            if (ModelImage.isColorImage(fileInfo.getDataType())) {
                bufferSize *= 4;
                sliceSize *= 4;
            }

            // long secondTime = System.currentTimeMillis();
            // System.err.println("Time elapsed reading IFDs: " + ((secondTime - firstTime) / 1000));
            // System.err.println("pbar visible: " + pBarVisible);
            if (fileInfo.getDataType() == ModelStorageBase.DOUBLE) {

                if (doubleBuffer == null) {
                    doubleBuffer = new double[bufferSize];
                }

                if (sliceBufferDouble == null) {
                    sliceBufferDouble = new double[sliceSize];
                }

                for (i = 0; i < imageSlice; i++) {

                    try {

                        if (one) {

                            if (imgExtents.length > 2) {
                                i = imgExtents[2] / 2;
                            }
                        }

                        readDoubleBuffer(i, sliceBufferDouble);

                        if (one) {
                            i = 0;
                        }
                    } catch (IOException error) {
                        throw new IOException("FileTiff: Read: " + error);
                    }

                    System.arraycopy(sliceBufferDouble, 0, doubleBuffer, i * sliceSize, sliceSize);

                    if (multiFile == false) {
                        image.importData(i * sliceSize, sliceBufferDouble, false);
                    }
                }
            } else { // not ModelStorageBase.DOUBLE

                // System.err.println("not buffer of doubles, type: " + fileInfo.getDataType());
                if (imgBuffer == null) {
                    imgBuffer = new float[bufferSize];
                }

                if (sliceBufferFloat == null) {
                    sliceBufferFloat = new float[sliceSize];
                }


                for (i = 0; i < imageSlice; i++) {

                    try {

                        if (doTile || lzwCompression) {
                            readTileBuffer(i, sliceBufferFloat);
                        } else {

                            if (one) {

                                if (imgExtents.length > 2) {
                                    i = imgExtents[2] / 2;
                                }
                            }

                            readBuffer(i, sliceBufferFloat); // Slice a time;

                            if (one) {
                                i = 0;
                            }
                        }
                    } catch (IOException error) {
                        throw new IOException("FileTiff: read: " + error);
                    }

                    System.arraycopy(sliceBufferFloat, 0, imgBuffer, i * sliceSize, sliceSize);

                    if (multiFile == false) {
                        image.importData(i * sliceSize, sliceBufferFloat, false);
                        // System.err.println("Imported image data");
                    }
                }
            } // else not ModelStorageBase.Double

            fileInfo.setExtents(imgExtents);
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
     * Passed in a LeicaSeries object, this function builds a 2d or 3d reconstruction using the Vector of filenames
     * within the series The vector has been presorted so that all files (whether red - green and then blue, or just
     * grayscale) are read in sequentially to build the ModelImage.
     *
     * @param   series  LeicaSeries series contains leica information for construction
     *
     * @throws  IOException  DOCUMENT ME!
     *
     * @return  ModelImage a 3d or 2d rgb or grayscale ModelImage
     */
    public ModelImage readLeicaSeries(LeicaSeries series) throws IOException {
        ModelImage finalImage = null;
        int[] imgExtents = null;

        // determine the destination ModelImage's extents
        imgExtents = new int[series.getResolutions().length];

        boolean doColor = (series.getChannels().length > 2);
        imgExtents[0] = series.getExtents()[0];
        imgExtents[1] = series.getExtents()[1];

        if (imgExtents.length > 2) {
            imgExtents[2] = series.getExtents()[3];
        }

        float[] imgRes = new float[imgExtents.length];
        imgRes[0] = series.getResolutions()[0];
        imgRes[1] = series.getResolutions()[1];

        if (imgRes.length > 2) {
            imgRes[2] = series.getResolutions()[2];
        }

        int[] units = new int[imgExtents.length];

        for (int i = 0; i < units.length; i++) {
            units[i] = FileInfoBase.MICROMETERS;
        }

        // Create the fileInfo and set up the extents/resolutions/units of measure
        fileInfo = new FileInfoTiff(series.getName(), "", FileUtility.TIFF);
        fileInfo.setExtents(imgExtents);
        fileInfo.setResolutions(imgRes);
        fileInfo.setUnitsOfMeasure(units);

        // if the image is color, load the files in the order of R-G-B
        if (doColor) {

            // create the final image where we will dump each concatenated rgb slice
            finalImage = new ModelImage(ModelImage.ARGB, imgExtents, series.getName(), UI);
            fileInfo.setDataType(ModelStorageBase.ARGB);

            ModelImage tempImage = null;
            Vector fileNameVector = series.getFileNames();
            int numImages = fileNameVector.size();
            String tempPath, tempDir, tempName;
            FileTiff tempTiff;
            int colorIndex = 0;
            float[] tempBuffer = new float[imgExtents[0] * imgExtents[1]];
            int alphaIndexStart = 0;


            // go through each file name in the vector, loading
            // them into the r,g, and b slot of the final image
            for (int i = 0, j = 0; i < numImages; i++) {
                colorIndex = i % 3;

                // when we get to each red channel, set start of the
                // alpha index so we can load the slices in correctly
                if (colorIndex == 0) {
                    alphaIndexStart = (j * 4 * tempBuffer.length);
                    j++;
                }

                // get the file name and directory
                tempPath = (String) fileNameVector.elementAt(i);
                tempDir = tempPath.substring(0, tempPath.lastIndexOf(File.separator) + 1);
                tempName = tempPath.substring(tempPath.lastIndexOf(File.separator) + 1, tempPath.length());

                // use FileTiff to read a single ModelImage
                tempTiff = new FileTiff(tempName, tempDir);
                tempImage = tempTiff.readImage(false, true);

                fireProgressStateChanged(((int) (((float) (i + 1) / (float) numImages) * 100f)));


                tempImage.exportData(0, tempBuffer.length, tempBuffer);
                finalImage.importRGBData((colorIndex + 1), alphaIndexStart, tempBuffer, false);
                tempImage.disposeLocal();
            }
        }
        // image is single channel
        else {

            // create the final image where we will dump each slice
            finalImage = new ModelImage(ModelImage.USHORT, imgExtents, series.getName(), UI);
            fileInfo.setDataType(ModelStorageBase.USHORT);

            for (int j = 0; j < imgExtents[2]; j++) {
                finalImage.setFileInfo((FileInfoBase) fileInfo.clone(), j);
            }

            ModelImage tempImage = null;
            Vector fileNameVector = series.getFileNames();
            int numImages = fileNameVector.size();
            String tempPath, tempDir, tempName;
            FileTiff tempTiff;
            float[] tempBuffer = new float[imgExtents[0] * imgExtents[1]];

            // go through each file name in the vector and load them into the slices
            // of the final image (single channel so one per slice)
            for (int i = 0; i < numImages; i++) {

                // get the file name and directory
                tempPath = (String) fileNameVector.elementAt(i);
                tempDir = tempPath.substring(0, tempPath.lastIndexOf(File.separator) + 1);
                tempName = tempPath.substring(tempPath.lastIndexOf(File.separator) + 1, tempPath.length());

                // use FileTiff to read a single ModelImage
                tempTiff = new FileTiff(tempName, tempDir);
                tempImage = tempTiff.readImage(false, true);

                fireProgressStateChanged(((int) (((float) (i + 1) / (float) numImages) * 100f)));

                tempImage.exportData(0, tempBuffer.length, tempBuffer);
                finalImage.importData((i * tempBuffer.length), tempBuffer, false);
                tempImage.disposeLocal();
            }
        }

        // set the fileinfos for each slice
        for (int j = 0; j < imgExtents[2]; j++) {
            finalImage.setFileInfo((FileInfoBase) fileInfo.clone(), j);
        }

        finalImage.calcMinMax();

        return finalImage;
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
     * This method writes a tiff image file.
     *
     * @param      image    image model where the data is stored.
     * @param      LUT      LUT to be saved with image if not null.
     * @param      options  options to be used to write out the image
     *
     * @exception  IOException  if there is an error writing the file.
     */
    public void writeImage(ModelImage image, ModelLUT LUT, FileWriteOptions options) throws IOException {
        int k, s, sEnd = 1, sBegin = 0;
        ModelImage tmpImage = null;
        ModelImage resultImage = null;
        int seq;
        int imgOffset;
        int nextIFD;
        int m;
        int type;
        int[] extents;
        int bufferSize;
        int bytesPerSample;
        int samplesPerPixel;
        int resolutionCount = 16; // xResolution = 2 * (4 bytes) + yResolution = 2 * (4 bytes)
        int rgbCount = 0; // Set to 6 for storage of 3 short bitsPerSample values

        // in ARGB, ARGB_USHORT, and ARGB_FLOAT
        int rgbFormat = 0; // Set to 6 for storage of 3 short sampleFormat values

        // in ARGB, ARGB_USHORT, and ARGB_FLOAT
        int intAlign = 0; // For integer data image rows must begin on integer boundaries, so set

        // intAlign = 2 if ModelStorageBase.INTEGER is used so that imgOffset is
        // always a multiple of 4.
        int zResCount = 0; // set to 8 if EchoTech zResolution field present
        int tResCount = 0; // set to 8 if EchoTech tResolution field present
        int ztEntries = 0;
        int index;
        int timeOffset = 0; // used to offset for writing one time slice of a 4D image
        boolean oneFile = true;

        // int offset = 0;
        String prefix, fileSuffix;
        int stripCount, totStripCount;

        if ((LUT != null) && !image.isColorImage()) {

            for (int i = 0; i < image.getFileInfo().length; i++) {
                image.getFileInfo()[i].setPhotometric((short) 3);
            }
        }

        this.image = image;
        this.LUT = LUT;

        try {
            extents = image.getExtents();

            if (image.getNDims() == 1) {
                throw new IOException("image.getNDims returned 1");
            }

            bufferSize = extents[0] * extents[1];

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
            zRes = -1.0;

            if ((image.getNDims() > 2) && (image.getFileInfo(0).getResolutions().length > 2)) {
                zRes = (double) (image.getFileInfo(0).getResolutions()[2]);
            }

            tRes = -1.0;

            if ((image.getNDims() > 3) && (image.getFileInfo(0).getResolutions().length > 3)) {
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
                    raFile.setLength(0); // necessary so that if this is an overwritten file there isn't junk at the
                                         // end

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
                    raFile.setLength(0); // necessary so that if this is an overwritten file there isn't junk at the
                                         // end

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
                        nDirEntries = (short) (11 + ztEntries);
                        rgbCount = 0;
                        intAlign = 0;
                        break;

                    case ModelStorageBase.UBYTE:
                    case ModelStorageBase.BYTE:
                        bytesPerSample = 1;
                        samplesPerPixel = 1;
                        intAlign = 0;
                        if (image.getFileInfo(0).getPhotometric() == 3) {
                            nDirEntries = (short) (13 + ztEntries); // Add one for color map

                            // Only one color map for all the images at the end of the file
                            // Only used if color map is saved with image. Pointer to the
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
                            nDirEntries = (short) (12 + ztEntries);
                        }

                        rgbCount = 0;
                        break;

                    case ModelStorageBase.USHORT:
                        bytesPerSample = 2;
                        samplesPerPixel = 1;
                        nDirEntries = (short) (12 + ztEntries);
                        rgbCount = 0;
                        intAlign = 0;
                        break;

                    case ModelStorageBase.SHORT:
                        bytesPerSample = 2;
                        samplesPerPixel = 1;
                        nDirEntries = (short) (12 + ztEntries);
                        rgbCount = 0;
                        intAlign = 0;
                        break;

                    case ModelStorageBase.INTEGER:
                    case ModelStorageBase.UINTEGER:
                    case ModelStorageBase.FLOAT:
                        bytesPerSample = 4;
                        samplesPerPixel = 1;
                        nDirEntries = (short) (12 + ztEntries);
                        rgbCount = 0;
                        intAlign = 0; // Used to be 2 but this caused problems.
                        break;

                    case ModelStorageBase.DOUBLE:
                        bytesPerSample = 8;
                        samplesPerPixel = 1;
                        nDirEntries = (short) (12 + ztEntries);
                        rgbCount = 0;
                        intAlign = 0; // Used to be 2 but this caused problems.
                        break;

                    case ModelStorageBase.ARGB:
                        bytesPerSample = 1; // since SamplesPerPixel is defined as 3 for RGB images
                        samplesPerPixel = 3;
                        nDirEntries = (short) (14 + ztEntries);
                        rgbCount = 6;
                        rgbFormat = 6;
                        intAlign = 0;
                        break;

                    case ModelStorageBase.ARGB_USHORT:
                        bytesPerSample = 2;
                        samplesPerPixel = 3;
                        nDirEntries = (short) (14 + ztEntries);
                        rgbCount = 6;
                        rgbFormat = 6;
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
                            stripCount = image.getSliceSize();
                        }

                        if (k == options.getEndSlice()) {
                            nextIFD = 0;
                        } else if (!options.isWritePackBit()) {
                            nextIFD += ((2 + (nDirEntries * 12) + resolutionCount + 4 + rgbCount + rgbFormat +
                                         intAlign + zResCount + tResCount) +
                                        (bufferSize * bytesPerSample * samplesPerPixel));
                        } else if (options.isWritePackBit()) {
                            nextIFD += ((2 + (nDirEntries * 12) + resolutionCount + 4 + rgbCount + rgbFormat +
                                         intAlign + zResCount + tResCount) + stripCount);
                        }

                        if (!options.isWritePackBit()) {
                            imgOffset = 8 +
                                        ((m + 1) *
                                             (2 + (nDirEntries * 12) + resolutionCount + 4 + rgbCount + rgbFormat +
                                                  intAlign + zResCount + tResCount)) +
                                        (m * bufferSize * bytesPerSample * samplesPerPixel);
                        } else if (options.isWritePackBit()) {
                            imgOffset = 8 +
                                        ((m + 1) *
                                             (2 + (nDirEntries * 12) + resolutionCount + 4 + rgbCount + rgbFormat +
                                                  intAlign + zResCount + tResCount)) + totStripCount - stripCount;
                        }

                        Preferences.debug("Image name = " + image.getImageName() + "\n", Preferences.DEBUG_FILEIO);
                        writeIFDs(image, imgOffset, nextIFD, k, stripCount, options.isWritePackBit());

                        try {

                            if (!options.isWritePackBit()) {

                                // adjust for intAlign ????
                                fileRW.writeImage(image, timeOffset + (k * bufferSize),
                                                  timeOffset + (k * bufferSize) + bufferSize, 0);
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
                        stripCount = image.getSliceSize();
                    }

                    imgOffset = 8 +
                                (2 + (nDirEntries * 12) + resolutionCount + 4 + rgbCount + rgbFormat + intAlign +
                                 zResCount + tResCount);
                    writeIFDs(image, imgOffset, 0, s, stripCount, options.isWritePackBit());

                    try {

                        if (!options.isWritePackBit()) {
                            fileRW.writeImage(image, s * bufferSize, (s * bufferSize) + bufferSize, 0);
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

        if (tmpImage != null) {
            image = tmpImage;
            resultImage.disposeLocal();
            resultImage = null;
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
    private boolean openIFD(FileInfoTiff fileInfo) throws IOException {
        int i;
        int i1;
        int tag;
        int type;
        int count;
        int ecount;
        long[] valueArray = new long[MAX_IFD_LENGTH];
        int value_offset;
        int nDirEntries;
        long numerator, denominator;
        float valueFloat = 0.0f;
        double valueDouble = 0.0;
        int[] bitsPerSample = null;
        int sampleFormat = 1; // 1 is default for unsigned integers
        boolean debuggingFileIO = Preferences.debugLevel(Preferences.DEBUG_FILEIO);
        fileInfo.setEndianess(endianess);
        nDirEntries = getUnsignedShort(endianess);

        if (nDirEntries <= 0) {
            throw new IOException("First 2 IFD bytes are an illegal " + nDirEntries);
        }

        if (debuggingFileIO) {
            Preferences.debug("\nOpenIFD: Entries = " + nDirEntries + "\n", Preferences.DEBUG_FILEIO);
        }

        for (i = 0; i < nDirEntries; i++) {
            tag = getUnsignedShort(endianess);

            if (tag == 0) {
                throw new IOException("Tiff Zero Tag Error");
            } else if (tag == 43314) {
                foundTag43314 = true;
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

                for (i1 = 0; ((i1 < count) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    valueArray[i1] = getUnsignedShort(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == LONG) && (count == 1)) {
                valueArray[0] = getUInt(endianess);
            } else if ((type == LONG) && (count >= 2)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    valueArray[i1] = getUInt(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == SLONG) && (count == 1)) {
                valueArray[0] = getInt(endianess);
            } else if ((type == SLONG) && (count >= 2)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    valueArray[i1] = getInt(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == RATIONAL) || (type == SRATIONAL)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < (2 * count)) && (i1 < MAX_IFD_LENGTH)); i1 = i1 + 2) {
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
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 0)) {
                raFile.seek(raFile.getFilePointer() + 4);
                // raFile.readUnsignedByte();
                // raFile.readUnsignedByte();
                // raFile.readUnsignedByte();
                // raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 1)) {
                valueArray[0] = raFile.readUnsignedByte();
                raFile.seek(raFile.getFilePointer() + 3);
                // raFile.readUnsignedByte();
                // raFile.readUnsignedByte();
                // raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 2)) {
                valueArray[0] = raFile.readUnsignedByte();
                valueArray[1] = raFile.readUnsignedByte();
                raFile.seek(raFile.getFilePointer() + 2);
                // raFile.readUnsignedByte();
                // raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 3)) {
                valueArray[0] = raFile.readUnsignedByte();
                valueArray[1] = raFile.readUnsignedByte();
                valueArray[2] = raFile.readUnsignedByte();
                raFile.seek(raFile.getFilePointer() + 1);
                // raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 4)) {
                valueArray[0] = raFile.readUnsignedByte();
                valueArray[1] = raFile.readUnsignedByte();
                valueArray[2] = raFile.readUnsignedByte();
                valueArray[3] = raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count > 4)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    valueArray[i1] = raFile.readUnsignedByte();
                }

                raFile.seek(saveLocus);
            } else if ((type == SBYTE) && (count == 1)) {
                valueArray[0] = raFile.readByte();
                raFile.seek(raFile.getFilePointer() + 3);
                // raFile.readByte();
                // raFile.readByte();
                // raFile.readByte();
            } else if ((type == SBYTE) && (count == 2)) {
                valueArray[0] = raFile.readByte();
                valueArray[1] = raFile.readByte();
                raFile.seek(raFile.getFilePointer() + 2);
                // raFile.readByte();
                // raFile.readByte();
            } else if ((type == SBYTE) && (count == 3)) {
                valueArray[0] = raFile.readByte();
                valueArray[1] = raFile.readByte();
                valueArray[2] = raFile.readByte();
                raFile.seek(raFile.getFilePointer() + 1);
                // raFile.readByte();
            } else if ((type == SBYTE) && (count == 4)) {
                valueArray[0] = raFile.readByte();
                valueArray[1] = raFile.readByte();
                valueArray[2] = raFile.readByte();
                valueArray[3] = raFile.readByte();
            } else if ((type == SBYTE) && (count > 4)) {
                value_offset = getInt(endianess);

                long saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < MAX_IFD_LENGTH)); i1++) {
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

                for (i1 = 0; ((i1 < count) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    valueArray[i1] = getSignedShort(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == FLOAT) && (count == 1)) {
                valueFloat = getFloat(endianess);
            } else if ((type == FLOAT) && (count > 1)) {

                // Ignore these fields for now
                value_offset = getInt(endianess);
            } else {

                if (debuggingFileIO) {
                    Preferences.debug("\nOpenIFD: Unknown field type = " + type + " Tag = " + tag + " count = " +
                                      count + "\n", Preferences.DEBUG_FILEIO);
                }

                throw new IOException("OpenIFD: Unknown field type = " + type + " Tag = " + tag + " count = " + count);
            }

            if (debuggingFileIO) {
                Preferences.debug("\nFileTiff.openIFD: Tag = " + tag + "\n", Preferences.DEBUG_FILEIO);

                switch (type) {

                    case BYTE:
                        Preferences.debug("FileTiff.openIFD: Type = BYTE  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case ASCII:
                        Preferences.debug("FileTiff.openIFD: Type = ASCII  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case SHORT:
                        Preferences.debug("FileTiff.openIFD: Type = SHORT  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case LONG:
                        Preferences.debug("FileTiff.openIFD: Type = LONG  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case RATIONAL:
                        Preferences.debug("FileTiff.openIFD: Type = RATIONAL  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case SBYTE:
                        Preferences.debug("FileTiff.openIFD: Type = SBYTE  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case UNDEFINED:
                        Preferences.debug("FileTiff.openIFD: Type = UNDEFINED  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case SSHORT:
                        Preferences.debug("FileTiff.openIFD: Type = SSHORT  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case SLONG:
                        Preferences.debug("FileTiff.openIFD: Type = SLONG  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case SRATIONAL:
                        Preferences.debug("FileTiff.openIFD: Type = SRATIONAL  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case FLOAT:
                        Preferences.debug("FileTiff.openIFD: Type = FLOAT  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case DOUBLE:
                        Preferences.debug("FileTiff.openIFD: Type = DOUBLE  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;
                }
            }

            if ((type == RATIONAL) || (type == SRATIONAL)) {
                ecount = 2 * count;
            } else {
                ecount = count;
            }

            if ((type != DOUBLE) && (type != FLOAT) && debuggingFileIO) {

                for (i1 = 0; ((i1 < ecount) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    Preferences.debug("FileTiff.openIFD: value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n",
                                      Preferences.DEBUG_FILEIO);
                }
            } else if ((type == DOUBLE) && (count == 1) && debuggingFileIO) {
                Preferences.debug("FIleTiff.openIFD: value = " + valueDouble + "\n", Preferences.DEBUG_FILEIO);
            } else if ((type == FLOAT) && (count == 1) && debuggingFileIO) {
                Preferences.debug("fileTiff.openIFD: value = " + valueFloat + "\n", Preferences.DEBUG_FILEIO);
            }

            switch (tag) {

                case NEW_SUBFILE_TYPE:
                    if (type != LONG) {
                        throw new IOException("NEW_SUBFILE_TYPE has illegal TYPE = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("NEW_SUBFILE_TYPE has illegal count = " + count + "\n");
                    }

                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: tag = NEW_SUBTYPE_FILE\n", Preferences.DEBUG_FILEIO);

                        if ((valueArray[0] & 0x01) == 0x01) {
                            Preferences.debug("Image is a reduced resolution version of another " +
                                              "image in this TIFF file\n", Preferences.DEBUG_FILEIO);
                        } else {
                            Preferences.debug("Image is not a reduced resolution version of another " +
                                              "image in this TIFF file\n", Preferences.DEBUG_FILEIO);
                        }

                        if ((valueArray[0] & 0x02) == 0x02) {
                            Preferences.debug("Image is a single page of a multi-page image\n", 2);
                        } else {
                            Preferences.debug("Image is not a single page of a multi-page image\n",
                                              Preferences.DEBUG_FILEIO);
                        }

                        if ((valueArray[0] & 0x04) == 0x04) {
                            Preferences.debug("Images defines a transparency mask for another image " +
                                              "in this TIFF file\n", Preferences.DEBUG_FILEIO);
                        } else {
                            Preferences.debug("Images does not define a transparency mask for another image " +
                                              "in this TIFF file\n", Preferences.DEBUG_FILEIO);
                        }
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
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: Image_Width = " + xDim + "\n", 2);
                    }

                    break;

                case IMAGE_LENGTH:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("IMAGE_LENGTH has illegal TYPE = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("IMAGE_LENGTH has illegal COUNT = " + count + "\n");
                    }

                    yDim = (int) valueArray[0];
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: Image_Length = " + yDim + "\n", 2);
                    }

                    break;

                case BITS_PER_SAMPLE:
                    if (type != SHORT) {
                        throw new IOException("BITS_PER_SAMPLE has illegal type = " + type + "\n");
                    }

                    bitsPerSample = new int[count];
                    for (i1 = 0; i1 < count; i1++) {
                        bitsPerSample[i1] = (int) valueArray[i1];
                    }

                    if ((count == 1) && debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: BitsPerSample = " + valueArray[0] + "\n",
                                          Preferences.DEBUG_FILEIO);
                    } else if (count > 1) {

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: BitsPerSample\n", Preferences.DEBUG_FILEIO);

                            for (i1 = 0; i1 < count; i1++) {
                                Preferences.debug("Value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n",
                                                  Preferences.DEBUG_FILEIO);
                            }
                        }

                        for (i1 = 1; i1 < count; i1++) {

                            if (valueArray[i1] != valueArray[0]) {

                                if (debuggingFileIO) {
                                    Preferences.debug("MIPAV cannot handle mixed data types", Preferences.DEBUG_FILEIO);
                                }

                                throw new IOException("MIPAV cannot handle mixed data types");
                            }
                        }
                    } // else if (count > 1)

                    break;

                case ROWS_PER_STRIP:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("ROWS_PER-STRIP has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("ROWS_PER_STRIP has illegal count = " + count + "\n");
                    }

                    rowsPerStrip = (int) valueArray[0];
                    if (debuggingFileIO) {
                        Preferences.debug("ROWS_PER_STRIP = " + valueArray[0] + "\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case STRIP_OFFSETS:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("STRIP_OFFSETS has illegal type = " + type + "\n");
                    }

                    dataOffsets[imageSlice] = new Vector();
                    if (count == 1) {

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Strip_offset = " + valueArray[0] + "\n",
                                              Preferences.DEBUG_FILEIO);
                        }

                        dataOffsets[imageSlice].addElement(new Index((int) valueArray[0]));
                    } else if (count > 1) {

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Strip_offset\n");
                        }

                        for (i1 = 0; i1 < count; i1++) {

                            if (debuggingFileIO) {
                                Preferences.debug("Value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n",
                                                  Preferences.DEBUG_FILEIO);
                            }

                            dataOffsets[imageSlice].addElement(new Index((int) valueArray[i1]));
                        }
                    }

                    break;

                case STRIP_BYTE_COUNTS:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("STRIP_BYTE_COUNTS has illegal type = " + type + "\n");
                    }

                    if (count == 1) {

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Strip byte counts = " + valueArray[0] + "\n",
                                              Preferences.DEBUG_FILEIO);
                        }

                        ((Index) (dataOffsets[imageSlice].elementAt(0))).byteCount = (int) valueArray[0];
                    } else if (count > 1) {

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD. Strip byte counts\n", Preferences.DEBUG_FILEIO);
                        }

                        for (i1 = 0; i1 < count; i1++) {

                            if (debuggingFileIO) {
                                Preferences.debug("Value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n",
                                                  Preferences.DEBUG_FILEIO);
                            }

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

                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: PhotoInterp= " + valueArray[0] + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

                    if (valueArray[0] == 1) { // Black is zero
                        fileInfo.setPhotometric((short) 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: PhotoInterp = Black is zero\n" +
                                              "For bilevel and grayscale images\n", Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == 0) { // white is zero
                        fileInfo.setPhotometric((short) 0);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: PhotoInterp = White is zero\n" +
                                              "For bilevel and grayscale images\n", Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == 2) { // Color RGB
                        fileInfo.setPhotometric((short) 2);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: PhotoInterp = RGB\n", Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == 3) { // Color Indexed
                        fileInfo.setPhotometric((short) 3);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: PhotoInterp = Palette color\n", 2);
                        }
                    } else if (valueArray[0] == 4) { // Transparency Mask
                        fileInfo.setPhotometric((short) 4);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: PhotoInterp = Transparency Mask\n",
                                              Preferences.DEBUG_FILEIO);
                        }
                    }

                    break;

                case SAMPLES_PER_PIXEL:
                    if (type != SHORT) {
                        throw new IOException("SAMPLES_PER_PIXEL has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("SAMPLES_PER_PIXEL has illegal count = " + count + "\n");
                    }

                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: samplesPerPixel = " + valueArray[0] + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

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

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: planar config = chunky \n", 2);
                        }
                    } else if (valueArray[0] == 2) { // Color RGB
                        chunky = false;

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: planar config = RRRRR, GGGG, BBBB. \n",
                                              Preferences.DEBUG_FILEIO);
                        }
                    }

                    break;

                case COMPRESSION:
                    if (type != SHORT) {
                        throw new IOException("COMPRESSION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("COMPRESSION has illegal count = " + count + "\n");
                    }

                    if ((valueArray[0] != 1) && (valueArray[0] != 2) && (valueArray[0] != 32773) &&
                            (valueArray[0] != 5)) {
                        throw new IOException("COMPRESSION has illegal value = " + valueArray[0] + "\n");
                    }

                    if (valueArray[0] == 2) {
                        throw new IOException("Modified Huffman run length encoding is not supported\n");
                    } else if (valueArray[0] == 1) {
                        packBit = false;

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: compression = no compression\n ",
                                              Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == 32773) {
                        packBit = true;

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: compression = packed bit\n ", 2);
                        }
                    } else if (valueArray[0] == 5) {
                        packBit = false;
                        lzwCompression = true;

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: compression = LZW\n ", Preferences.DEBUG_FILEIO);
                        }
                    }

                    break;

                case ORIENTATION:
                    if (type != SHORT) {
                        throw new IOException("ORIENTATION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("ORIENTATION has illegal count + " + count + "\n");
                    }

                    if ((valueArray[0] < 1) || (valueArray[0] > 8)) {
                        throw new IOException("ORIENTATION has illegal value = " + valueArray[0] + "\n");
                    }

                    if (valueArray[0] > 1) {
                        throw new IOException("Only default orientation is currently supported\n");
                    } else {

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: orientation has 0th row representing\n");
                            Preferences.debug("the top of the image, and the 0th column representing\n");
                            Preferences.debug("the left hand side of the image\n");
                        }
                    }

                    break;

                case COLOR_MAP:
                    if (type != SHORT) {
                        throw new IOException("COLOR_MAP has illegal type = " + type + "\n");
                    }

                    if ((count % 6) != 0) {
                        throw new IOException("COLOR_MAP has illegal count = " + count + "\n");
                    }

                    // Already read LUT - only same LUT in every file of multiFile and only one
                    // LUT for a multiImage file for now.
                    if ((count == 768) && (LUT == null)) {

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff creating color map\n");
                        }

                        int[] extents = new int[2];
                        extents[0] = 4;
                        extents[1] = 256;

                        // System.err.println("Creating new ModelLUT");
                        LUT = new ModelLUT(ModelLUT.GRAY, 256, extents);

                        int maxValue = -Integer.MAX_VALUE;

                        for (i1 = 0; i1 < 768; i1++) {

                            if (valueArray[i1] > maxValue) {
                                maxValue = (int) valueArray[i1];
                            }
                        }

                        if (maxValue > 256) {

                            for (i1 = 0; i1 < 768; i1++) {
                                valueArray[i1] = valueArray[i1] * 255 / maxValue;
                            }
                        }

                        for (i1 = 0; i1 < 256; i1++) {
                            LUT.set(0, i1, 1.0f);
                            LUT.set(1, i1, (float) valueArray[i1]);
                            LUT.set(2, i1, (float) valueArray[i1 + 256]);
                            LUT.set(3, i1, (float) valueArray[i1 + 512]);
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

                    if (valueArray[0] == FileInfoBase.MILLIMETERS) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = MILLIMETERS\n");
                        }
                    } else if (valueArray[0] == FileInfoBase.UNKNOWN_MEASURE) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.UNKNOWN_MEASURE, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.UNKNOWN_MEASURE, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = UNKNOWN\n", 2);
                        }
                    } else if (valueArray[0] == FileInfoBase.INCHES) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.INCHES, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.INCHES, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = INCHES\n", 2);
                        }
                    } else if (valueArray[0] == FileInfoBase.CENTIMETERS) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.CENTIMETERS, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.CENTIMETERS, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = CENTIMETERS\n",
                                              Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == FileInfoBase.ANGSTROMS) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.ANGSTROMS, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.ANGSTROMS, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = ANGSTROMS\n", 2);
                        }
                    } else if (valueArray[0] == FileInfoBase.NANOMETERS) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.NANOMETERS, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.NANOMETERS, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = NANOMETERS\n",
                                              Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == FileInfoBase.MICROMETERS) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MICROMETERS, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MICROMETERS, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = MICROMETERS\n",
                                              Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == FileInfoBase.METERS) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.METERS, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.METERS, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = METERS\n", 2);
                        }
                    } else if (valueArray[0] == FileInfoBase.KILOMETERS) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.KILOMETERS, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.KILOMETERS, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = KILOMETERS\n",
                                              Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == FileInfoBase.MILES) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MILES, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MILES, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = MILES\n", 2);
                        }
                    } else if (valueArray[0] == FileInfoBase.NANOSEC) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.NANOSEC, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.NANOSEC, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = NANOSEC\n", 2);
                        }
                    } else if (valueArray[0] == FileInfoBase.MICROSEC) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MICROSEC, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MICROSEC, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = MICROSEC\n", 2);
                        }
                    } else if (valueArray[0] == FileInfoBase.MILLISEC) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MILLISEC, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MILLISEC, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = MILLISEC\n", 2);
                        }
                    } else if (valueArray[0] == FileInfoBase.SECONDS) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.SECONDS, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.SECONDS, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = SECONDS\n", 2);
                        }
                    } else if (valueArray[0] == FileInfoBase.MINUTES) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MINUTES, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MINUTES, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = MINUTES\n", 2);
                        }
                    } else if (valueArray[0] == FileInfoBase.HOURS) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.HOURS, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.HOURS, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = HOURS\n", 2);
                        }
                    } else if (valueArray[0] == FileInfoBase.HZ) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.HZ, 0);
                        fileInfo.setUnitsOfMeasure(FileInfoBase.HZ, 1);

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Resolution Unit = HERTZ\n", 2);
                        }
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
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: X Resolution = " + imgResols[0] + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

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
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: Y Resolution = " + imgResols[1] + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

                    break;

                case ZRESOLUTION:
                    if (type != DOUBLE) {
                        throw new IOException("ZRESOLUTION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("ZRESOLUTION has illegal count = " + count + "\n");
                    }

                    imgResols[2] = (float) valueDouble;
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: Z Resolution = " + imgResols[2] + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

                    // EchoTech uses mm for Z resolution units
                    fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, Preferences.DEBUG_FILEIO);
                    break;

                case TRESOLUTION:
                    if (type != DOUBLE) {
                        throw new IOException("TRESOLUTION has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("TRESOLUTION has illegal count = " + count + "\n");
                    }

                    imgResols[3] = (float) valueDouble;
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: T Resolution = " + imgResols[3] + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

                    // EchoTech uses msec for T resolution units
                    fileInfo.setUnitsOfMeasure(FileInfoBase.MILLISEC, 3);
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
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: tileWidth = " + tileWidth + "\n", 2);
                    }

                    tilesAcross = (xDim + tileWidth - 1) / tileWidth;
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: tilesAcross = " + tilesAcross + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

                    break;

                case TILE_LENGTH:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("TILE_LENGTH has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("TILE_LENGTH has illegal count = " + count + "\n");
                    }

                    tileLength = (int) valueArray[0];
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: tileLength = " + tileLength + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

                    tilesDown = (yDim + tileLength - 1) / tileLength;
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: tilesDown = " + tilesDown + "\n", 2);
                    }

                    break;

                case TILE_OFFSETS:

                    // System.err.println("Tiles per image: " + count);
                    if (type != LONG) {
                        throw new IOException("TILE_OFFSETS has illegal type = " + type + "\n");
                    }

                    if (chunky) {
                        tilesPerImage = count;
                    } else {
                        tilesPerImage = count / samplesPerPixel;
                    }

                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: tilesPerImage = " + tilesPerImage + "\n",
                                          Preferences.DEBUG_FILEIO);
                        Preferences.debug("FileTiff.openIFD: tileOffsets are above\n", Preferences.DEBUG_FILEIO);
                    }

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

                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: tileByteCounts are above\n", Preferences.DEBUG_FILEIO);
                    }

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
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: imageDescription = " + str + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

                    break;

                case MIN_SAMPLE_VALUE:
                    if (type != SHORT) {
                        throw new IOException("MIN_SAMPLE_VALUE has illegal type = " + type + "\n");
                    }

                    if (debuggingFileIO) {

                        for (i1 = 0; i1 < count; i1++) {
                            Preferences.debug("FileTiff.openIFD: minSampleValue[" + i1 + "] = " + valueArray[i1] + "\n",
                                              Preferences.DEBUG_FILEIO);
                        }
                    }

                    break;

                case MAX_SAMPLE_VALUE:
                    if (type != SHORT) {
                        throw new IOException("MAX_SAMPLE_VALUE has illegal type = " + type + "\n");
                    }

                    if (debuggingFileIO) {

                        for (i1 = 0; i1 < count; i1++) {
                            Preferences.debug("FileTiff.openIFD: maxSampleValue[" + i1 + "] = " + valueArray[i1] + "\n",
                                              Preferences.DEBUG_FILEIO);
                        }
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
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: software = " + str + "\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case ARTIST:
                    if (type != ASCII) {
                        throw new IOException("ARTIST has illegal type = " + type + "\n");
                    }

                    artist = new byte[count];
                    for (i1 = 0; i1 < count; i1++) {
                        artist[i1] = (byte) valueArray[i1];
                    }

                    str = new String(artist);
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: artist = " + str.trim() + "\n", 2);
                    }

                    break;

                case HOST_COMPUTER:
                    if (type != ASCII) {
                        throw new IOException("HOST_COMPUTER has illegal type = " + type + "\n");
                    }

                    hostComputer = new byte[count];
                    for (i1 = 0; i1 < count; i1++) {
                        hostComputer[i1] = (byte) valueArray[i1];
                    }

                    str = new String(hostComputer);
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: hostComputer = " + str.trim() + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

                    break;

                case DATE_TIME:
                    if (type != ASCII) {
                        throw new IOException("DATE_TIME has illegal type = " + type + "\n");
                    }

                    // The TIFF standard specifies a count of 20, but we encountered a
                    // file with a count of 25, so remove count restriction to 20.
                    /*
                     * if (count != 20) throw new IOException("DATE_TIME has illegal count = " + count + "\n");
                     */
                    dateTime = new byte[count];
                    for (i1 = 0; i1 < count; i1++) {
                        dateTime[i1] = (byte) valueArray[i1];
                    }

                    str = new String(dateTime);
                    Preferences.debug("FileTiff.openIFD: dateTime = " + str + "\n", Preferences.DEBUG_FILEIO);
                    break;

                case SAMPLE_FORMAT:

                    // Default is 1, unsigned integer data
                    if (type != SHORT) {
                        throw new IOException("SAMPLE_FORMAT has illegal type = " + type + "\n");
                    }

                    sampleFormat = (int) valueArray[0];
                    if (debuggingFileIO) {

                        for (i1 = 0; i1 < count; i1++) {
                            Preferences.debug("FileTiff.openIFD: sampleFormat[ " + i1 + "] = " + valueArray[i1] + "\n",
                                              Preferences.DEBUG_FILEIO);

                            if (valueArray[i1] == 1) {
                                Preferences.debug("FileTiff.openIFD: unsigned integer data\n");
                            } else if (valueArray[i1] == 2) {
                                Preferences.debug("FileTiff.openIFD: two's complement signed integer data\n");
                            } else if (valueArray[i1] == 3) {
                                Preferences.debug("FileTiff.openIFD: IEEE floating point data\n");
                            } else if (valueArray[i1] == 4) {
                                Preferences.debug("FileTiff.openIFD: undefined data format\n");
                            }
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
                        predictor = 1;

                        if (debuggingFileIO) {
                            Preferences.debug("PREDICTOR = 1 for no prediction scheme used\n");
                        }
                    } else if (valueArray[0] == 2) {
                        predictor = 2;

                        if (debuggingFileIO) {
                            Preferences.debug("PREDICTOR = 2 for horizontal differencing\n", 2);
                        }
                    } else {

                        if (debuggingFileIO) {
                            Preferences.debug("PREDICTOR = " + valueArray[0] + ", an illegal value",
                                              Preferences.DEBUG_FILEIO);
                        }
                    }

                    break;

                default:
                    break;
            }
        }

        if (bitsPerSample != null) {

            if (bitsPerSample.length == 1) {

                if (sampleFormat == 1) { // default for unsigned integers

                    switch (bitsPerSample[0]) {

                        case 1:
                            fileInfo.setDataType(ModelStorageBase.BOOLEAN);
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
                            throw new IOException("TIFF Tag BitsPerSample has illegal value = " + bitsPerSample[0]);
                    } // switch(bitsPerSample[0])
                } // if (sampleFormat == 1)
                else if (sampleFormat == 2) { // signed integers

                    switch (bitsPerSample[0]) {

                        case 1:
                            fileInfo.setDataType(ModelStorageBase.BOOLEAN);
                            break;

                        case 8:
                            fileInfo.setDataType(ModelStorageBase.BYTE);
                            break;

                        case 16:
                            fileInfo.setDataType(ModelStorageBase.SHORT);
                            break;

                        case 32:
                            fileInfo.setDataType(ModelStorageBase.INTEGER);
                            break;

                        default:
                            throw new IOException("TIFF Tag BitsPerSample has illegal value = " + bitsPerSample[0]);
                    } // switch(bitsPerSample[0])
                } // else if (sampleFormat == 2)
                else if (sampleFormat == 3) {

                    switch (bitsPerSample[0]) {

                        case 32:
                            fileInfo.setDataType(ModelStorageBase.FLOAT);
                            break;

                        case 64:
                            fileInfo.setDataType(ModelStorageBase.DOUBLE);
                            break;

                        default:
                            throw new IOException("TIFF tag BitsPerSample has illegal value = " + bitsPerSample[0]);
                    } // switch(bitsPerSample[0]
                } // else if (sampleFormat == 3)
            } // if (bitsPerSample.length == 1)
            else { // bitsPerSample.length > 1

                if (sampleFormat == 1) { // default for unsigned integers

                    switch (bitsPerSample[0]) {

                        case 8:
                            fileInfo.setDataType(ModelStorageBase.ARGB);
                            break;

                        case 16:
                            fileInfo.setDataType(ModelStorageBase.ARGB_USHORT);
                            break;

                        default:
                            throw new IOException("TIFF Tag BitsPerSample has illegal value = " + bitsPerSample[0]);
                    } // switch(bitsPerSample[0])
                } // if (sampleFormat == 1)
                else if (sampleFormat == 2) {

                    if (debuggingFileIO) {
                        Preferences.debug("TIFF signed data cannot have multiple channels\n", 2);
                    }

                    throw new IOException("TIFF signed data cannot have multiple channels");
                } // else if (sampleFormat == 2)
                else if (sampleFormat == 3) {

                    switch (bitsPerSample[0]) {

                        case 32:
                            fileInfo.setDataType(ModelStorageBase.ARGB_FLOAT);
                            break;

                        default:
                            throw new IOException("TIFF Tag BitsPerSample has illegal value = " + bitsPerSample[0]);
                    } // switch(bitsPerSample[0])
                } // else if (sampleFormat == 3)
            } // else bitsPerSample.length > 1
        } // if (bitsPerSample != null)

        imageSlice++;
        IFDoffsets[imageSlice] = getInt(endianess);

        if (debuggingFileIO) {
            Preferences.debug("\nFileTiff.openIFD: Ref. to next imageSlice = " + IFDoffsets[imageSlice] + "\n",
                              Preferences.DEBUG_FILEIO);
        }

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
        int totalLength;
        int planarRGB = 0; // Use this for planar RGB where you must read a stripsPerImage

        // number of red strips, followed by a stripsPerImage number of
        // green strips, followed by a stripsPerImage number of blue strips.
        // System.err.println("doing readBuffer()");
        int nIndex = dataOffsets[slice].size();
        int stripsPerImage = (nIndex + 1) / 3; // used for planar RGB
        int tmpInt;

        // System.err.println("Datatype is: " + fileInfo.getDataType());
        // System.err.println("number of data offsets: " + nIndex);
        // System.err.println("buffer length: " + buffer.length);
        // ben mod: try calculating total length you'd need to read in ALL at one time
        int firstIndex = ((Index) (dataOffsets[slice].elementAt(0))).index;
        int lastIndex = ((Index) (dataOffsets[slice].elementAt(nIndex - 1))).index;

        if (((Index) (dataOffsets[slice].elementAt(nIndex - 1))).byteCount == 0) {

            switch (fileInfo.getDataType()) {

                case ModelStorageBase.BOOLEAN:
                    nLength = 8 * ((buffer.length + 63) >> 6);
                    break;

                case ModelStorageBase.BYTE:
                case ModelStorageBase.UBYTE:
                case ModelStorageBase.ARGB:
                    nLength = buffer.length;
                    break;

                case ModelStorageBase.SHORT:
                case ModelStorageBase.USHORT:
                case ModelStorageBase.ARGB_USHORT:
                    nLength = 2 * buffer.length;
                    break;

                case ModelStorageBase.INTEGER:
                case ModelStorageBase.UINTEGER:
                case ModelStorageBase.FLOAT:
                    nLength = 4 * buffer.length;
                    break;

                default:
                    nLength = buffer.length;
            } // switch (fileInfo.getDataType())

            totalLength = nLength + lastIndex - firstIndex;
        } // if (((Index) (dataOffsets[slice].elementAt(nIndex - 1))).byteCount == 0)
        else {
            totalLength = (lastIndex - firstIndex) + ((Index) (dataOffsets[slice].elementAt(nIndex - 1))).byteCount;
        }

        byteBuffer = new byte[totalLength];

        int currentIndex = 0;

        // System.err.println("first index: " + firstIndex + ", last index: " + lastIndex + ", totalLength: " +
        // totalLength);
        // System.err.println("packbit is: " + packBit);
        raFile.seek(firstIndex);
        raFile.read(byteBuffer, 0, totalLength);
        i = 0;

        for (a = 0; a < nIndex; a++, idx++) {

            try {

                // System.err.println("Seeking to: " + ( (Index) (dataOffsets[slice].elementAt(idx))).index);
                currentIndex = ((Index) (dataOffsets[slice].elementAt(idx))).index - firstIndex;

                // raFile.seek( ( (Index) (dataOffsets[slice].elementAt(idx))).index);
                nBytes = ((Index) (dataOffsets[slice].elementAt(idx))).byteCount;

                // System.err.println("doing nBytes: " + nBytes);
                if (nBytes == 0) {
                    nBytes = buffer.length;
                }

                ;

                switch (fileInfo.getDataType()) {

                    case ModelStorageBase.BOOLEAN:
                        nLength = 8 * ((buffer.length + 63) >> 6); // new BitSet(size) = new long[(size+63)>>6];

                        // byteBuffer = new byte[nLength];
                        raFile.seek(((Index) (dataOffsets[slice].elementAt(idx))).index);
                        raFile.read(byteBuffer, 0, nLength);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j++, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            buffer[i] = byteBuffer[j >> 3] & (1 << (j % 8));
                        }

                        break;

                    case ModelStorageBase.BYTE:
                        if (packBit == false) {

                            // if (byteBuffer == null)
                            // byteBuffer = new byte[buffer.length];
                            // raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 10;


                            for (j = 0; j < nBytes; j++, i++) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                buffer[i] = byteBuffer[j + currentIndex];
                            }
                        } // if (packBit == false)
                        else if (packBit == true) {

                            // byteBuffer = new byte[nBytes];
                            // raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 100;

                            j = 0;

                            while (j < nBytes) {

                                // uncompressed data bytes follow
                                // Copy the next n+1 bytes literally
                                if ((byteBuffer[j + currentIndex] & 0x80) == 0) {
                                    iCount = byteBuffer[j + currentIndex] + 1;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, j++, i++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = byteBuffer[j + currentIndex];
                                    }
                                } // end of if (byteBuffer[j] & 0x80 == 0)

                                // Do nothing if the byte value is -128
                                else if (byteBuffer[j + currentIndex] == -128) {
                                    j++;
                                }
                                // compressed data bytes follow
                                // (~byteBuffer[j]) + 1 is the 2's complement of n or -n
                                // Hence (~byteBuffer[j]) + 2 equals -n + 1
                                else {
                                    iCount = (~byteBuffer[j + currentIndex]) + 2;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, i++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = byteBuffer[j + currentIndex];
                                    }

                                    j++;
                                } // end of else for compressed data bytes
                            } // end of while (j < nBytes)
                        } // end of else if (packBit == true)

                        break;

                    case ModelStorageBase.UBYTE:
                        if (packBit == false) {

                            // if (byteBuffer == null)
                            // byteBuffer = new byte[buffer.length];
                            // raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 100;


                            for (j = 0; j < nBytes; j++, i++) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                buffer[i] = byteBuffer[j + currentIndex] & 0xff;
                            }
                        } // if (packBit == false)
                        else if (packBit == true) {

                            // System.err.println("doing packbit UBYTE");
                            byteBuffer = new byte[nBytes];

                            // raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 100;

                            j = 0;

                            while (j < nBytes) {

                                // uncompressed data bytes follow
                                // Copy the next n+1 bytes literally
                                if ((byteBuffer[j + currentIndex] & 0x80) == 0) {
                                    iCount = byteBuffer[j + currentIndex] + 1;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, j++, i++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = byteBuffer[j + currentIndex] & 0xff;
                                    }
                                } // end of if (byteBuffer[j] & 0x80 == 0)

                                // Do nothing if the byte value is -128
                                else if (byteBuffer[j + currentIndex] == -128) {
                                    j++;
                                }
                                // compressed data bytes follow
                                // (~byteBuffer[j]) + 1 is the 2's complement of n or -n
                                // Hence (~byteBuffer[j]) + 2 equals -n + 1
                                else {
                                    iCount = (~byteBuffer[j + currentIndex]) + 2;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, i++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[i] = byteBuffer[j + currentIndex] & 0xff;
                                    }

                                    j++;
                                } // end of else for compressed data bytes
                            } // end of while (j < nBytes)
                        } // end of else if (packBit == true)

                        break;

                    case ModelStorageBase.SHORT:

                        // if (byteBuffer == null) {
                        // byteBuffer = new byte[2 * buffer.length];
                        // }
                        // raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 2, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j + currentIndex);
                            b2 = getUnsignedByte(byteBuffer, j + currentIndex + 1);

                            if (endianess) {
                                buffer[i] = (short) ((b1 << 8) + b2);
                            } else {
                                buffer[i] = (short) ((b2 << 8) + b1);
                            }
                        }

                        break;

                    case ModelStorageBase.USHORT:

                        // if (byteBuffer == null) {
                        // byteBuffer = new byte[2 * buffer.length];
                        // }
                        // raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 2, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j + currentIndex);
                            b2 = getUnsignedByte(byteBuffer, j + currentIndex + 1);

                            if (endianess) {
                                buffer[i] = ((b1 << 8) + b2);
                            } else {
                                buffer[i] = ((b2 << 8) + b1);
                            }
                        }

                        break;

                    case ModelStorageBase.INTEGER:

                        // if (byteBuffer == null) {
                        // byteBuffer = new byte[4 * buffer.length];
                        // }
                        // raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 4, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j + currentIndex);
                            b2 = getUnsignedByte(byteBuffer, j + currentIndex + 1);
                            b3 = getUnsignedByte(byteBuffer, j + currentIndex + 2);
                            b4 = getUnsignedByte(byteBuffer, j + currentIndex + 3);

                            if (endianess) {
                                buffer[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                            } else {
                                buffer[i] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                            }
                        }

                        break;

                    case ModelStorageBase.UINTEGER:

                        // if (byteBuffer == null) {
                        // byteBuffer = new byte[4 * buffer.length];
                        // }
                        // raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 4, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j + currentIndex);
                            b2 = getUnsignedByte(byteBuffer, j + currentIndex + 1);
                            b3 = getUnsignedByte(byteBuffer, j + currentIndex + 2);
                            b4 = getUnsignedByte(byteBuffer, j + currentIndex + 3);

                            if (endianess) {
                                buffer[i] = (((long) b1 << 24) | ((long) b2 << 16) | ((long) b3 << 8) | (long) b4); // Big
                                                                                                                    // Endian
                            } else {
                                buffer[i] = (((long) b4 << 24) | ((long) b3 << 16) | ((long) b2 << 8) | (long) b1); // Little
                                                                                                                    // Endian
                            }
                        }

                        break;

                    case ModelStorageBase.FLOAT:

                        // if (byteBuffer == null) {
                        // byteBuffer = new byte[4 * buffer.length];
                        // }
                        // raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * buffer.length;
                        progressLength = buffer.length * imageSlice;
                        mod = progressLength / 10;

                        for (j = 0; j < nBytes; j += 4, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j + currentIndex);
                            b2 = getUnsignedByte(byteBuffer, j + currentIndex + 1);
                            b3 = getUnsignedByte(byteBuffer, j + currentIndex + 2);
                            b4 = getUnsignedByte(byteBuffer, j + currentIndex + 3);

                            if (endianess) {
                                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                            } else {
                                tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                            }

                            buffer[i] = Float.intBitsToFloat(tmpInt);
                        } // for (j =0; j < nBytes; j+=4, i++ )

                        break;

                    case ModelStorageBase.ARGB:
                        if ((chunky == true) && (packBit == false)) {

                            // if (byteBuffer == null)
                            // byteBuffer = new byte[buffer.length];
                            // raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 10;


                            // For the moment I compress RGB images to unsigned bytes.
                            for (j = 0; j < nBytes; j += 3, i += 4) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                buffer[i] = 255;
                                buffer[i + 1] = getUnsignedByte(byteBuffer, j + currentIndex);
                                buffer[i + 2] = getUnsignedByte(byteBuffer, j + currentIndex + 1);
                                buffer[i + 3] = getUnsignedByte(byteBuffer, j + currentIndex + 2);
                            }
                        } // if (chunky == true && packBit == false)
                        else if ((chunky == true) && (packBit == true)) {
                            int rgbLoc = 1;

                            // if (byteBuffer == null)
                            // byteBuffer = new byte[buffer.length];
                            // raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 10;

                            j = 0;

                            while (j < nBytes) {

                                // uncompressed data bytes follow
                                // Copy the next n+1 bytes literally
                                if ((byteBuffer[j + currentIndex] & 0x80) == 0) {
                                    iCount = byteBuffer[j + currentIndex] + 1;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++, j++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        if (rgbLoc == 1) {
                                            buffer[i++] = 255;
                                        }

                                        buffer[i++] = byteBuffer[j + currentIndex] & 0xff;

                                        if (rgbLoc == 3) {
                                            rgbLoc = 1;
                                        } else {
                                            rgbLoc++;
                                        }
                                    }
                                } // end of if (byteBuffer[j] & 0x80 == 0)

                                // Do nothing if the byte value is -128
                                else if (byteBuffer[j + currentIndex] == -128) {
                                    j++;
                                }
                                // compressed data bytes follow
                                // (~byteBuffer[j]) + 1 is the 2's complement of n or -n
                                // Hence (~byteBuffer[j]) + 2 equals -n + 1
                                else {
                                    iCount = (~byteBuffer[j + currentIndex]) + 2;
                                    j++;

                                    for (iNext = 0; iNext < iCount; iNext++) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        if (rgbLoc == 1) {
                                            buffer[i++] = 255;
                                        }

                                        buffer[i++] = byteBuffer[j + currentIndex] & 0xff;

                                        if (rgbLoc == 3) {
                                            rgbLoc = 1;
                                        } else {
                                            rgbLoc++;
                                        }
                                    }

                                    j++;
                                } // end of else for compressed data bytes
                            } // end of while (j < nBytes)
                        } // else if (chunky == true && packBit == true)
                        else if (packBit == false) { // planar RGB configuration

                            if (planarRGB < stripsPerImage) {

                                // if (byteBuffer == null)
                                // byteBuffer = new byte[buffer.length];
                                // raFile.read(byteBuffer, 0, nBytes);
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
                                    buffer[i + 1] = getUnsignedByte(byteBuffer, j + currentIndex);
                                }

                                planarRGB++;

                                if (planarRGB == stripsPerImage) {
                                    i = 0;
                                }
                            } // end of if (planarRGB < stripsPerImage)
                            else if (planarRGB < (2 * stripsPerImage)) {

                                // raFile.read(byteBuffer, 0, nBytes);
                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                for (j = 0; j < nBytes; j++, i += 4) {

                                    if ((((i / 3) + (buffer.length / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) + (buffer.length / 3) +
                                                                                     progress) / progressLength * 100));
                                    }

                                    buffer[i + 2] = getUnsignedByte(byteBuffer, j + currentIndex);
                                }

                                planarRGB++;

                                if (planarRGB == (2 * stripsPerImage)) {
                                    i = 0;
                                }
                            } // end of else if (planarRGB < 2*stripsPerImage)
                            else { // planarRGB >= 2*stripsPerImage

                                // raFile.read(byteBuffer, 0, nBytes);
                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                for (j = 0; j < nBytes; j++, i += 4) {

                                    if ((((i / 3) + (2 * buffer.length / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) +
                                                                                     (2 * buffer.length / 3) +
                                                                                     progress) / progressLength * 100));
                                    }

                                    buffer[i + 3] = getUnsignedByte(byteBuffer, j + currentIndex);
                                }

                                planarRGB++;
                            } // end of else for planarRGB >= 2*StripsPerImage
                        } // else for planar RGB configuration with packedBit == false

                        break;

                    case ModelStorageBase.ARGB_USHORT:
                        if (chunky == true) {

                            // if (byteBuffer == null)
                            // byteBuffer = new byte[2 * buffer.length];
                            // raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * buffer.length;
                            progressLength = buffer.length * imageSlice;
                            mod = progressLength / 10;


                            // For the moment I compress RGB images to unsigned bytes.
                            for (j = 0; j < nBytes; j += 6, i += 4) {

                                if (((i + progress) % mod) == 0) {
                                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                                }

                                buffer[i] = 65535;
                                b1 = getUnsignedByte(byteBuffer, j + currentIndex);
                                b2 = getUnsignedByte(byteBuffer, j + currentIndex + 1);

                                if (endianess) {
                                    buffer[i + 1] = ((b1 << 8) + b2);
                                } else {
                                    buffer[i + 1] = ((b2 << 8) + b1);
                                }

                                b1 = getUnsignedByte(byteBuffer, j + currentIndex + 2);
                                b2 = getUnsignedByte(byteBuffer, j + currentIndex + 3);

                                if (endianess) {
                                    buffer[i + 2] = ((b1 << 8) + b2);
                                } else {
                                    buffer[i + 2] = ((b2 << 8) + b1);
                                }

                                b1 = getUnsignedByte(byteBuffer, j + currentIndex + 4);
                                b2 = getUnsignedByte(byteBuffer, j + currentIndex + 5);

                                if (endianess) {
                                    buffer[i + 3] = ((b1 << 8) + b2);
                                } else {
                                    buffer[i + 3] = ((b2 << 8) + b1);
                                }
                            }
                        } // if (chunky == true)
                        else { // planar RGB configuration

                            if (planarRGB < stripsPerImage) {

                                // if (byteBuffer == null)
                                // byteBuffer = new byte[2 * buffer.length];
                                // raFile.read(byteBuffer, 0, nBytes);
                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                // For the moment I compress RGB images to unsigned bytes
                                for (j = 0; j < nBytes; j += 2, i += 4) {

                                    if ((((i / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) + progress) /
                                                                                progressLength * 100));
                                    }

                                    buffer[i] = 65535;
                                    b1 = getUnsignedByte(byteBuffer, j + currentIndex);
                                    b2 = getUnsignedByte(byteBuffer, j + currentIndex + 1);

                                    if (endianess) {
                                        buffer[i + 1] = ((b1 << 8) + b2);
                                    } else {
                                        buffer[i + 1] = ((b2 << 8) + b1);
                                    }
                                }

                                planarRGB++;

                                if (planarRGB == stripsPerImage) {
                                    i = 0;
                                }
                            } // end of if (planarRGB < stripsPerImage)
                            else if (planarRGB < (2 * stripsPerImage)) {

                                // raFile.read(byteBuffer, 0, nBytes);
                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                for (j = 0; j < nBytes; j += 2, i += 4) {

                                    if ((((i / 3) + (buffer.length / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) + (buffer.length / 3) +
                                                                                     progress) / progressLength * 100));
                                    }

                                    b1 = getUnsignedByte(byteBuffer, j + currentIndex);
                                    b2 = getUnsignedByte(byteBuffer, j + currentIndex + 1);

                                    if (endianess) {
                                        buffer[i + 2] = ((b1 << 8) + b2);
                                    } else {
                                        buffer[i + 2] = ((b2 << 8) + b1);
                                    }
                                }

                                planarRGB++;

                                if (planarRGB == (2 * stripsPerImage)) {
                                    i = 0;
                                }
                            } // end of else if (planarRGB < 2*stripsPerImage)
                            else { // planarRGB >= 2*stripsPerImage

                                // raFile.read(byteBuffer, 0, nBytes);
                                progress = slice * buffer.length;
                                progressLength = buffer.length * imageSlice;
                                mod = progressLength / 10;


                                for (j = 0; j < nBytes; j += 2, i += 4) {

                                    if ((((i / 3) + (2 * buffer.length / 3) + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) ((i / 3) +
                                                                                     (2 * buffer.length / 3) +
                                                                                     progress) / progressLength * 100));
                                    }

                                    b1 = getUnsignedByte(byteBuffer, j + currentIndex);
                                    b2 = getUnsignedByte(byteBuffer, j + currentIndex + 1);

                                    if (endianess) {
                                        buffer[i + 3] = ((b1 << 8) + b2);
                                    } else {
                                        buffer[i + 3] = ((b2 << 8) + b1);
                                    }
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
     * @param      slice   offset into the file stored in the dataOffset array
     * @param      buffer  buffer where the info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readDoubleBuffer(int slice, double[] buffer) throws IOException {
        int i, j, a;
        long b1, b2, b3, b4, b5, b6, b7, b8;
        long progress, progressLength, mod;

        // long pointer;
        int idx = 0;
        int nBytes;

        // int planarRGB = 0; // Use this for planar RGB where you must read a stripsPerImage
        // number of red strips, followed by a stripsPerImage number of
        // green strips, followed by a stripsPerImage number of blue strips.
        int nIndex = dataOffsets[slice].size();

        // int stripsPerImage = nIndex/3; // used for planar RGB
        long tmpLong;

        // System.err.println("Datatype is: " + fileInfo.getDataType());
        // System.err.println("number of data offsets: " + nIndex);
        // System.err.println("buffer length: " + buffer.length);
        // ben mod: try calculating total length you'd need to read in ALL at one time
        int firstIndex = ((Index) (dataOffsets[slice].elementAt(0))).index;
        int lastIndex = ((Index) (dataOffsets[slice].elementAt(nIndex - 1))).index;
        int totalLength = (lastIndex - firstIndex) + ((Index) (dataOffsets[slice].elementAt(nIndex - 1))).byteCount;
        int currentIndex = 0;

        // System.err.println("first index: " + firstIndex + ", last index: " + lastIndex + ", totalLength: " +
        // totalLength);
        // System.err.println("packbit is: " + packBit);
        byteBuffer = new byte[totalLength];
        raFile.seek(firstIndex);
        raFile.read(byteBuffer, 0, totalLength);
        i = 0;

        for (a = 0; a < nIndex; a++, idx++) {

            try {
                currentIndex = ((Index) (dataOffsets[slice].elementAt(idx))).index - firstIndex;

                // raFile.seek( ( (Index) (dataOffsets[slice].elementAt(idx))).index);
                nBytes = ((Index) (dataOffsets[slice].elementAt(idx))).byteCount;

                if (nBytes == 0) {
                    nBytes = buffer.length;
                }

                ;

                if (byteBuffer == null) {
                    byteBuffer = new byte[8 * buffer.length];
                }

                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * imageSlice;
                mod = progressLength / 10;


                for (j = 0; j < nBytes; j += 8, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = byteBuffer[j + currentIndex] & 0xff;
                    b2 = byteBuffer[j + currentIndex + 1] & 0xff;
                    b3 = byteBuffer[j + currentIndex + 2] & 0xff;
                    b4 = byteBuffer[j + currentIndex + 3] & 0xff;
                    b5 = byteBuffer[j + currentIndex + 4] & 0xff;
                    b6 = byteBuffer[j + currentIndex + 5] & 0xff;
                    b7 = byteBuffer[j + currentIndex + 6] & 0xff;
                    b8 = byteBuffer[j + currentIndex + 7] & 0xff;

                    if (endianess) {
                        tmpLong = ((b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) | (b5 << 24) | (b6 << 16) |
                                       (b7 << 8) | b8); // Big Endian
                    } else {
                        tmpLong = ((b8 << 56) | (b7 << 48) | (b6 << 40) | (b5 << 32) | (b4 << 24) | (b3 << 16) |
                                       (b2 << 8) | b1);
                    }

                    buffer[i] = Double.longBitsToDouble(tmpLong);
                } // for (j =0; j < nBytes; j+=8, i++ )
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
        byte[] decomp = null;
        i = 0;
        xTile = 0;
        yTile = 0;
        x = 0;
        y = 0;

        for (a = 0; a < tilesPerSlice; a++) {

            try {

                // System.err.println("___________________________________________________");
                // System.err.println("A = " + a + ", seeking to " + tileOffsets[slice * tilesPerSlice + a]);
                // System.err.println("Starting with x = " + x + " and y = " + y);
                raFile.seek(tileOffsets[(slice * tilesPerSlice) + a]);
                nBytes = tileByteCounts[(slice * tilesPerSlice) + a];

                switch (fileInfo.getDataType()) {

                    case ModelStorageBase.BOOLEAN:
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;

                        nLength = 8 * ((nBytes + 63) >> 6); // new BitSet(size) = new long[(size+63)>>6];
                        if (lzwCompression) {

                            if (byteBuffer == null) {
                                byteBuffer = new byte[tileMaxByteCount];
                            }

                            raFile.read(byteBuffer, 0, nLength);

                            if (decomp == null) {
                                decomp = new byte[tileWidth * tileLength];
                            }

                            lzwDecoder.decode(byteBuffer, decomp, tileLength);

                            for (j = 0; j < decomp.length; j++) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    buffer[x + (y * xDim)] = decomp[j >> 3] & (1 << (j % 8));
                                    i++;
                                }

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            }
                        } else {
                            byteBuffer = new byte[nLength];
                            raFile.read(byteBuffer, 0, nLength);

                            for (j = 0; j < nBytes; j++) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    buffer[x + (y * xDim)] = byteBuffer[j >> 3] & (1 << (j % 8));
                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < nBytes; j++)
                        }

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

                                if (lzwCompression) {
                                    byteBuffer = new byte[tileMaxByteCount];
                                } else {
                                    byteBuffer = new byte[nBytes];
                                }
                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;


                            if (lzwCompression) {

                                if (decomp == null) {
                                    decomp = new byte[tileWidth * tileLength];
                                }

                                lzwDecoder.decode(byteBuffer, decomp, tileLength);

                                for (j = 0; j < decomp.length; j++) {

                                    if ((x < xDim) && (y < yDim)) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[x + (y * xDim)] = decomp[j] & 0xff;
                                        i++;
                                    }

                                    x++;

                                    if (x == ((xTile + 1) * tileWidth)) {
                                        x = xTile * tileWidth;
                                        y++;
                                    }
                                }
                            } else {

                                for (j = 0; j < nBytes; j++) {

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
                                    }
                                } // for (j = 0; j < nBytes; j++)
                            }

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

                                if (lzwCompression) {
                                    byteBuffer = new byte[tileMaxByteCount];
                                } else {
                                    byteBuffer = new byte[nBytes];
                                }
                            }

                            raFile.read(byteBuffer, 0, nBytes);
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;


                            if (lzwCompression) {

                                if (decomp == null) {
                                    decomp = new byte[tileWidth * tileLength];
                                }

                                lzwDecoder.decode(byteBuffer, decomp, tileLength);

                                for (j = 0; j < decomp.length; j++) {

                                    if ((x < xDim) && (y < yDim)) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
                                        }

                                        buffer[x + (y * xDim)] = decomp[j] & 0xff;
                                        i++;
                                    }

                                    x++;

                                    if (x == ((xTile + 1) * tileWidth)) {
                                        x = xTile * tileWidth;
                                        y++;
                                    }
                                }
                            } else {

                                for (j = 0; j < nBytes; j++) {

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
                                    }
                                } // for (j = 0; j < nBytes; j++)
                            }

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

                            if (lzwCompression) {
                                byteBuffer = new byte[tileMaxByteCount];
                            } else {
                                byteBuffer = new byte[nBytes];
                            }
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;

                        if (lzwCompression) {

                            if (decomp == null) {
                                decomp = new byte[tileWidth * tileLength * 3];
                            }

                            lzwDecoder.decode(byteBuffer, decomp, tileLength);

                            for (j = 0; j < decomp.length; j += 2) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    b1 = getUnsignedByte(decomp, j);
                                    b2 = getUnsignedByte(decomp, j + 1);

                                    if (endianess) {
                                        buffer[x + (y * xDim)] = ((b1 << 8) + b2);
                                    } else {
                                        buffer[x + (y * xDim)] = ((b2 << 8) + b1);
                                    }
                                }

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            }
                        } else {

                            for (j = 0; j < nBytes; j += 2) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
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
                        }

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

                            if (lzwCompression) {
                                byteBuffer = new byte[tileMaxByteCount];
                            } else {
                                byteBuffer = new byte[nBytes];
                            }
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;

                        if (lzwCompression) {

                            if (decomp == null) {
                                decomp = new byte[tileWidth * tileLength * 3];
                            }

                            lzwDecoder.decode(byteBuffer, decomp, tileLength);

                            for (j = 0; j < decomp.length; j += 4) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    b1 = getUnsignedByte(decomp, j);
                                    b2 = getUnsignedByte(decomp, j + 1);
                                    b3 = getUnsignedByte(decomp, j + 2);
                                    b4 = getUnsignedByte(decomp, j + 3);

                                    if (endianess) {
                                        buffer[x + (y * xDim)] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big
                                                                                                             // Endian
                                    } else {
                                        buffer[x + (y * xDim)] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little
                                                                                                             // Endian
                                    }

                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < decomp.length; j+= 4)
                        } else {

                            for (j = 0; j < nBytes; j += 4) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    b1 = getUnsignedByte(byteBuffer, j);
                                    b2 = getUnsignedByte(byteBuffer, j + 1);
                                    b3 = getUnsignedByte(byteBuffer, j + 2);
                                    b4 = getUnsignedByte(byteBuffer, j + 3);

                                    if (endianess) {
                                        buffer[x + (y * xDim)] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big
                                                                                                             // Endian
                                    } else {
                                        buffer[x + (y * xDim)] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little
                                                                                                             // Endian
                                    }

                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < nBytes; j+= 4)
                        }

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

                            if (lzwCompression) {
                                byteBuffer = new byte[tileMaxByteCount];
                            } else {
                                byteBuffer = new byte[nBytes];
                            }
                        }

                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * xDim * yDim;
                        progressLength = imageSlice * xDim * yDim;
                        mod = progressLength / 100;

                        if (lzwCompression) {

                            if (decomp == null) {
                                decomp = new byte[tileWidth * tileLength * 3];
                            }

                            lzwDecoder.decode(byteBuffer, decomp, tileLength);

                            for (j = 0; j < decomp.length; j += 4) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    b1 = getUnsignedByte(decomp, j);
                                    b2 = getUnsignedByte(decomp, j + 1);
                                    b3 = getUnsignedByte(decomp, j + 2);
                                    b4 = getUnsignedByte(decomp, j + 3);

                                    if (endianess) {
                                        buffer[x + (y * xDim)] = (((long) b1 << 24) | ((long) b2 << 16) |
                                                                      ((long) b3 << 8) | (long) b4); // Big Endian
                                    } else {
                                        buffer[x + (y * xDim)] = (((long) b4 << 24) | ((long) b3 << 16) |
                                                                      ((long) b2 << 8) | (long) b1); // Little Endian
                                    }

                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < decomp.length; j+= 4)
                        } else {

                            for (j = 0; j < nBytes; j += 4) {

                                if ((x < xDim) && (y < yDim)) {

                                    if (((i + progress) % mod) == 0) {
                                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength *
                                                                                100));
                                    }

                                    b1 = getUnsignedByte(byteBuffer, j);
                                    b2 = getUnsignedByte(byteBuffer, j + 1);
                                    b3 = getUnsignedByte(byteBuffer, j + 2);
                                    b4 = getUnsignedByte(byteBuffer, j + 3);

                                    if (endianess) {
                                        buffer[x + (y * xDim)] = (((long) b1 << 24) | ((long) b2 << 16) |
                                                                      ((long) b3 << 8) | (long) b4); // Big Endian
                                    } else {
                                        buffer[x + (y * xDim)] = (((long) b4 << 24) | ((long) b3 << 16) |
                                                                      ((long) b2 << 8) | (long) b1); // Little Endian
                                    }

                                    i++;
                                } // if ((x < xDim) && (y < yDim))

                                x++;

                                if (x == ((xTile + 1) * tileWidth)) {
                                    x = xTile * tileWidth;
                                    y++;
                                }
                            } // for (j = 0; j < nBytes; j+= 4)
                        }

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

                                if (lzwCompression) {
                                    byteBuffer = new byte[tileMaxByteCount];
                                } else {
                                    byteBuffer = new byte[nBytes];
                                }
                            }

                            // System.err.println("About to read " + nBytes + " bytes");
                            raFile.read(byteBuffer, 0, nBytes);

                            // System.err.println("________");
                            progress = slice * xDim * yDim;
                            progressLength = imageSlice * xDim * yDim;
                            mod = progressLength / 100;


                            if (lzwCompression) {

                                // System.err.println("Read " + nBytes + " from raFile");
                                if (decomp == null) {
                                    decomp = new byte[tileWidth * tileLength * samplesPerPixel];
                                }

                                lzwDecoder.decode(byteBuffer, decomp, tileLength);

                                // System.err.println("Decoded byte length: " + decomp.length);
                                if (samplesPerPixel == 3) {

                                    for (j = 0; j < decomp.length; j += 3) {

                                        if ((x < xDim) && (y < yDim)) {

                                            if (((i + progress) % mod) == 0) {
                                                fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                        progressLength * 100));
                                            }

                                            buffer[4 * (x + (y * xDim))] = 255;
                                            buffer[(4 * (x + (y * xDim))) + 1] = getUnsignedByte(decomp, j);
                                            buffer[(4 * (x + (y * xDim))) + 2] = getUnsignedByte(decomp, j + 1);
                                            buffer[(4 * (x + (y * xDim))) + 3] = getUnsignedByte(decomp, j + 2);
                                            i++;
                                        }

                                        x++;

                                        if (x == ((xTile + 1) * tileWidth)) {
                                            x = xTile * tileWidth;
                                            y++;
                                        }
                                    }
                                } // if (samplesPerPixel == 3)
                                else if (samplesPerPixel == 4) {

                                    for (j = 0; j < decomp.length; j += 4) {

                                        if ((x < xDim) && (y < yDim)) {

                                            if (((i + progress) % mod) == 0) {
                                                fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                        progressLength * 100));
                                            }

                                            buffer[4 * (x + (y * xDim))] = getUnsignedByte(decomp, j + 3);
                                            buffer[(4 * (x + (y * xDim))) + 1] = getUnsignedByte(decomp, j);
                                            buffer[(4 * (x + (y * xDim))) + 2] = getUnsignedByte(decomp, j + 1);
                                            buffer[(4 * (x + (y * xDim))) + 3] = getUnsignedByte(decomp, j + 2);
                                            i++;
                                        }

                                        x++;

                                        if (x == ((xTile + 1) * tileWidth)) {
                                            x = xTile * tileWidth;
                                            y++;
                                        }
                                    }
                                }
                            } else {

                                for (j = 0; j < nBytes; j += 3) {

                                    if ((x < xDim) && (y < yDim)) {

                                        if (((i + progress) % mod) == 0) {
                                            fireProgressStateChanged(Math.round((float) (i + progress) /
                                                                                    progressLength * 100));
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
                            }

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

        decomp = null;
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
            setBufferShort(colorTable, (short) (LUT.getFloat(1, i) + 0.5), j, endianess);
            setBufferShort(colorTable, (short) (LUT.getFloat(2, i) + 0.5), j + 512, endianess);
            setBufferShort(colorTable, (short) (LUT.getFloat(3, i) + 0.5), j + 1024, endianess);
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
