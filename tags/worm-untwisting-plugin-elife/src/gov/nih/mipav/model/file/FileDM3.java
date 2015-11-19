package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJProgressBar;

import java.awt.Toolkit;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;


/**
 * Some of this code is derived from DM3_Reader.java in ImageJ.
 * 
 * <hr>
 * <p>
 * ImageJ disclaimer:
 * </p>
 * 
 * <p>
 * ImageJ is being developed at the National Institutes of Health by an employee of the Federal Government in the course
 * of his official duties. Pursuant to Title 17, Section 105 of the United States Code, this software is not subject to
 * copyright protection and is in the public domain. ImageJ is an experimental system. NIH assumes no responsibility
 * whatsoever for its use by other parties, and makes no guarantees, expressed or implied, about its quality,
 * reliability, or any other characteristic.
 * </p>
 */
public class FileDM3 extends FileBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Different encoded data types used in DM3 files. */
    private static final int SHORT = 2;

    /** DOCUMENT ME! */
    private static final int LONG = 3;

    /** DOCUMENT ME! */
    private static final int USHORT = 4;

    /** DOCUMENT ME! */
    private static final int ULONG = 5;

    /** DOCUMENT ME! */
    private static final int FLOAT = 6;

    /** DOCUMENT ME! */
    private static final int DOUBLE = 7;

    /** DOCUMENT ME! */
    private static final int BOOLEAN = 8;

    /** DOCUMENT ME! */
    private static final int CHAR = 9;

    /** DOCUMENT ME! */
    private static final int OCTET = 10;

    /** DOCUMENT ME! */
    private static final int STRUCT = 15;

    /** DOCUMENT ME! */
    private static final int STRING = 18;

    /** DOCUMENT ME! */
    private static final int ARRAY = 20;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private long[] arrayLocationArray = new long[100];

    /** DOCUMENT ME! */
    private int[] arraySizeArray = new int[100];

    /** DOCUMENT ME! */
    private int byteOrder;

    /** DOCUMENT ME! */
    private boolean dataEndianess;

    /** DOCUMENT ME! */
    private int[] dataTypeArray = new int[100];

    /** DOCUMENT ME! */
    private int desiredArraySize;

    /** DOCUMENT ME! */
    private int desiredImageNumber;

    /** DOCUMENT ME! */
    private String[] desiredPixelUnitsArray = new String[10];

    /** DOCUMENT ME! */
    private int[][] dimArray = new int[100][10];

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private int fileBytes;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoDM3 fileInfo;

    private FileInfoDM3 fileInfoCopy;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private int fileVersion;

    /** DOCUMENT ME! */
    private int identicalDesiredArraySize = 0;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int imageNum = -1;

    /** DOCUMENT ME! */
    private int[] imgExtents;

    /** DOCUMENT ME! */
    private boolean isCalibrations = false;

    /** DOCUMENT ME! */
    private boolean isData = false;

    /** DOCUMENT ME! */
    private boolean isDataType = false;

    /** DOCUMENT ME! */
    private boolean isDimension = false;

    /** DOCUMENT ME! */
    private boolean isDimensions = false;

    /** DOCUMENT ME! */
    private boolean isImageData = false;

    /** DOCUMENT ME! */
    private boolean isScale = false;

    /** DOCUMENT ME! */
    private boolean isUnits = false;

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    /** DOCUMENT ME! */
    private int nDimensions = 2;

    /** DOCUMENT ME! */
    private int numberSlices; // 1 for 2D, zDim for 3D, and zDim * tDim for 4D

    /** DOCUMENT ME! */
    private int[] numDimArray = new int[100];

    /** DOCUMENT ME! */
    private float[][] pixelScaleArray = new float[100][10];

    /** DOCUMENT ME! */
    private String[][] pixelUnitsArray = new String[100][10];

    /** DOCUMENT ME! */
    private int[] pixelUnitsNumber = new int[100];

    /** DOCUMENT ME! */
    private ViewJProgressBar progressBar = null;

    /** DOCUMENT ME! */
    private int routineTagEntry = 0;

    /** DOCUMENT ME! */
    private int routineTagGroup = 0;

    /** DOCUMENT ME! */
    private int scaleIndex = 0;

    /** DOCUMENT ME! */
    private int sourceType = ModelStorageBase.USHORT;

    /** DOCUMENT ME! */
    private int unitsIndex = 0;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * DM3 reader constructor.
     * 
     * @param fileName file name
     * @param fileDir file directory
     * 
     * @exception IOException if there is an error making the file
     */
    public FileDM3(final String fileName, final String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        int i, j;
        arrayLocationArray = null;
        arraySizeArray = null;
        dataTypeArray = null;
        if (desiredPixelUnitsArray != null) {
            for (i = 0; i < desiredPixelUnitsArray.length; i++) {
                desiredPixelUnitsArray[i] = null;
            }
            desiredPixelUnitsArray = null;
        }
        if (dimArray != null) {
            for (i = 0; i < dimArray.length; i++) {
                dimArray[i] = null;
            }
            dimArray = null;
        }
        imgExtents = null;
        LUT = null;
        numDimArray = null;
        if (pixelScaleArray != null) {
            for (i = 0; i < pixelScaleArray.length; i++) {
                pixelScaleArray[i] = null;
            }
            pixelScaleArray = null;
        }

        if (pixelUnitsArray != null) {
            for (i = 0; i < pixelUnitsArray.length; i++) {
                for (j = 0; j < pixelUnitsArray[i].length; j++) {
                    pixelUnitsArray[i][j] = null;
                }
                pixelUnitsArray[i] = null;
            }
            pixelUnitsArray = null;
        }
        pixelUnitsNumber = null;
        fileName = null;
        fileDir = null;
        fileInfo = null;
        fileInfoCopy = null;
        file = null;
        image = null;
        try {
            super.finalize();
        } catch (final Throwable er) {}
    }

    /**
     * returns LUT if defined.
     * 
     * @return the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }

    /**
     * reads the DM3 file header and data.
     * 
     * @exception IOException if there is an error reading the file
     * 
     * @param one DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public ModelImage readImage(final boolean one) throws IOException {
        int i, j;
        int bufferSize;
        float[] imgBuffer;
        float[] imgBufferI;
        double[] imgDBuffer;
        double[] imgDBufferI;
        long[] imgLBuffer;

        try {
            progressBar = new ViewJProgressBar(fileName, "Reading DM3 file...", 0, 100, false, null, null);

            progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50);

            file = new File(fileDir + fileName);

            endianess = FileBase.BIG_ENDIAN; // true
            fileInfo = new FileInfoDM3(fileName, fileDir, FileUtility.DM3);

            raFile = new RandomAccessFile(file, "r");
            fileVersion = getInt(endianess);
            Preferences.debug("DM3 file version = " + fileVersion + "\n", Preferences.DEBUG_FILEIO);

            if (fileVersion != 3) {
                raFile.close();

                MipavUtil.displayError("File version number = " + fileVersion + " instead of required 3");
                throw new IOException();
            }

            // Location contains number of file bytes - 16
            fileBytes = getInt(endianess) + 16;
            Preferences.debug(fileName + " length = " + fileBytes + " bytes\n", Preferences.DEBUG_FILEIO);

            // The next location contains the byte ordering of the tag data
            // Only the tag data is in this order.
            // All other data is big endian.
            byteOrder = getInt(endianess);

            if (byteOrder == 0) {
                Preferences.debug("Tag data is big endian\n", Preferences.DEBUG_FILEIO);
                dataEndianess = FileBase.BIG_ENDIAN;
            } else if (byteOrder == 1) {
                dataEndianess = FileBase.LITTLE_ENDIAN;
                Preferences.debug("Tag data is little endian\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("byteOrder is an illegal = " + byteOrder + "\n", Preferences.DEBUG_FILEIO);
                raFile.close();

                MipavUtil.displayError("Byte order is an illegal = " + byteOrder);
                throw new IOException();
            }

            fileInfo.setEndianess(dataEndianess);

            // Read tag group
            readTagGroup();
            Preferences.debug("Have completed readTagGroup()\n", Preferences.DEBUG_FILEIO);

            desiredImageNumber = 0;
            desiredArraySize = arraySizeArray[0];
            identicalDesiredArraySize = 1;

            // In 4 images that I examined a smaller 192 by 192 image is the first image present.
            // This image is in ARGB representation although it has R = G = B so a black and
            // white image appears. It is a smaller version of the second image.
            // The second image was a 512 by 512 or 1024 by 1024 float or short image
            for (i = 1; i <= imageNum; i++) {

                if (arraySizeArray[i] > desiredArraySize) {
                    desiredImageNumber = i;
                    desiredArraySize = arraySizeArray[i];
                    identicalDesiredArraySize = 1;
                } else if (arraySizeArray[i] == desiredArraySize) {
                    identicalDesiredArraySize++;
                }
            }

            Preferences.debug("Number of arrays of largest size = " + identicalDesiredArraySize + "\n", 
            		Preferences.DEBUG_FILEIO);

            nDimensions = numDimArray[desiredImageNumber];
            imgExtents = new int[nDimensions];

            for (i = 0; i < nDimensions; i++) {
                imgExtents[i] = dimArray[desiredImageNumber][i];
            }

            fileInfo.setExtents(imgExtents);

            // Data type definitions from GatanDM3.h.
            switch (dataTypeArray[desiredImageNumber]) {

                case 0: // NULL_DATA
                    break;

                case 1: // SIGNED_INT16_DATA
                    sourceType = ModelStorageBase.SHORT;
                    break;

                case 2: // REAL4_DATA
                    sourceType = ModelStorageBase.FLOAT;
                    break;

                case 3: // COMPLEX8_DATA
                    sourceType = ModelStorageBase.COMPLEX;
                    break;

                case 4: // OBSOLETE_DATA
                    break;

                case 5: // PACKED_DATA
                    break;

                case 6: // UNSIGNED_INT8_DATA
                    sourceType = ModelStorageBase.UBYTE;
                    break;

                case 7: // SIGNED_INT32_DATA
                    sourceType = ModelStorageBase.INTEGER;
                    break;

                case 8: // RGB_DATA
                    break;

                case 9: // SIGNED_INT8_DATA
                    sourceType = ModelStorageBase.BYTE;
                    break;

                case 10: // UNSIGNED_INT16_DATA
                    sourceType = ModelStorageBase.USHORT;
                    break;

                case 11: // UNSIGNED_INT32_DATA
                    sourceType = ModelStorageBase.UINTEGER;
                    break;

                case 12: // REAL8_DATA
                    sourceType = ModelStorageBase.DOUBLE;
                    break;

                case 13: // COMPLEX16_DATA
                    sourceType = ModelStorageBase.DCOMPLEX;
                    break;

                case 14: // BINARY_DATA

                    // bitmap
                    break;

                case 15: // RGB_UINT8_0_DATA
                    break;

                case 16: // RGB_UINT8_1_DATA
                    break;

                case 17: // RGB_UINT16_DATA
                    break;

                case 18: // RGB_FLOAT32_DATA
                    break;

                case 19: // RGB_FLOAT64_DATA
                    break;

                case 20: // RGBA_UINT8_0_DATA
                    break;

                case 21: // RGBA_UINT8_1_DATA
                    break;

                case 22: // RGBA_UINT8_2_DATA
                    break;

                case 23: // RGBA_UINT8_3_DATA
                    sourceType = ModelStorageBase.ARGB;
                    break;

                case 24: // RGBA_UINT16_DATA
                    break;

                case 25: // RGBA_FLOAT32_DATA
                    break;

                case 26: // RGBA_FLOAT64_DATA
                    break;

                case 27: // POINT2_SINT16_0_DATA
                    break;

                case 28: // POINT2_SINT16_1_DATA
                    break;

                case 29: // POINT2_SINT32_0_DATA
                    break;

                case 30: // POINT2_FLOAT32_0_DATA
                    break;

                case 31: // RECT_SINT16_1_DATA
                    break;

                case 32: // RECT_SINT32_1_DATA
                    break;

                case 33: // RECT_FLOAT32_1_DATA
                    break;

                case 34: // RECT_FLOAT32_0_DATA
                    break;

                case 35: // SIGNED_INT64_DATA
                    sourceType = ModelStorageBase.LONG;
                    break;

                case 36: // UNSIGNED_INT64_DATA - Actually ULONG but treat as LONG
                    sourceType = ModelStorageBase.LONG;
                    break;

                case 37: // LAST_DATA
                    break;
            }

            if (one) {
                image = new ModelImage(sourceType, new int[] {imgExtents[0], imgExtents[1]}, fileInfo.getFileName());
            } else {
                image = new ModelImage(sourceType, imgExtents, fileInfo.getFileName());
            }

            fileInfo.setDataType(sourceType);

            if ( (nDimensions == 3)
                    && (pixelScaleArray[desiredImageNumber][0] != pixelScaleArray[desiredImageNumber][1])
                    && (pixelScaleArray[desiredImageNumber][1] == pixelScaleArray[desiredImageNumber][2])) {

                for (i = 0; i < nDimensions; i++) {
                    fileInfo.setResolutions(pixelScaleArray[desiredImageNumber][nDimensions - i - 1], i);
                }
            } else {

                for (i = 0; i < nDimensions; i++) {
                    fileInfo.setResolutions(pixelScaleArray[desiredImageNumber][i], i);
                }
            }

            for (i = nDimensions - 1, j = 0; i >= 0; i--, j++) {
                desiredPixelUnitsArray[i] = pixelUnitsArray[desiredImageNumber][pixelUnitsNumber[desiredImageNumber]
                        - 1 - j].trim();
            }

            for (i = 0; i < nDimensions; i++) {

                if (desiredPixelUnitsArray[i].equals("n")) {
                    fileInfo.setUnitsOfMeasure(Unit.NANOMETERS.getLegacyNum(), i);
                }
                // micro sign = 00B5 Greek letter mu = 03BC
                else if (desiredPixelUnitsArray[i].equals("\u00B5")) {
                    fileInfo.setUnitsOfMeasure(Unit.MICROMETERS.getLegacyNum(), i);
                } else {
                    fileInfo.setUnitsOfMeasure(Unit.UNKNOWN_MEASURE.getLegacyNum(), i);
                }
            }

            if ( (nDimensions == 2) || one) {
                numberSlices = 1;
            } else if (nDimensions == 3) {
                numberSlices = imgExtents[2];
            } else {
                numberSlices = imgExtents[2] * imgExtents[3];
            }

            raFile.seek(arrayLocationArray[desiredImageNumber]);

            if (sourceType == ModelStorageBase.ARGB) {
                bufferSize = 4 * imgExtents[0] * imgExtents[1];
            } else {
                bufferSize = imgExtents[0] * imgExtents[1];
            }

            switch (sourceType) {

                case ModelStorageBase.BYTE:
                case ModelStorageBase.UBYTE:
                case ModelStorageBase.SHORT:
                case ModelStorageBase.USHORT:
                case ModelStorageBase.INTEGER:
                case ModelStorageBase.FLOAT:
                case ModelStorageBase.ARGB:
                    imgBuffer = new float[bufferSize];
                    for (i = 0; i < numberSlices; i++) {
                        fileInfoCopy = (FileInfoDM3) fileInfo.clone();
                        image.setFileInfo(fileInfoCopy, i);
                        readBuffer(i, imgBuffer);
                        image.importData(i * bufferSize, imgBuffer, false);
                    }

                    break;

                case ModelStorageBase.UINTEGER:
                case ModelStorageBase.LONG:
                    imgLBuffer = new long[bufferSize];
                    for (i = 0; i < numberSlices; i++) {
                        fileInfoCopy = (FileInfoDM3) fileInfo.clone();
                        image.setFileInfo(fileInfoCopy, i);
                        readLBuffer(i, imgLBuffer);
                        image.importData(i * bufferSize, imgLBuffer, false);
                    }

                    break;

                case ModelStorageBase.DOUBLE:
                    imgDBuffer = new double[bufferSize];
                    for (i = 0; i < numberSlices; i++) {
                        fileInfoCopy = (FileInfoDM3) fileInfo.clone();
                        image.setFileInfo(fileInfoCopy, i);
                        readDBuffer(i, imgDBuffer);
                        image.importData(i * bufferSize, imgDBuffer, false);
                    }

                    break;

                case ModelStorageBase.COMPLEX:
                    imgBuffer = new float[bufferSize];
                    imgBufferI = new float[bufferSize];
                    for (i = 0; i < numberSlices; i++) {
                        fileInfoCopy = (FileInfoDM3) fileInfo.clone();
                        image.setFileInfo(fileInfoCopy, i);
                        readComplexBuffer(i, imgBuffer, imgBufferI);
                        image.importComplexData(2 * i * bufferSize, imgBuffer, imgBufferI, false, Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
                    }

                    break;

                case ModelStorageBase.DCOMPLEX:
                    imgDBuffer = new double[bufferSize];
                    imgDBufferI = new double[bufferSize];
                    for (i = 0; i < numberSlices; i++) {
                        fileInfoCopy = (FileInfoDM3) fileInfo.clone();
                        image.setFileInfo(fileInfoCopy, i);
                        readDComplexBuffer(i, imgDBuffer, imgDBufferI);
                        image.importDComplexData(2 * i * bufferSize, imgDBuffer, imgDBufferI, false, Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
                    }

                    break;
            } // switch(sourceType)

            image.calcMinMax();
            raFile.close();
            if (progressBar != null) {
                progressBar.dispose();
            }

            return image;
        } catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw new IOException();
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @throws IOException DOCUMENT ME!
     */
    private void readArray() throws IOException {
        int arrayType;
        int[] encodedType;
        int arraySize;
        int elementBytes = 0;
        int bufferSize;
        long arrayLocation;
        int i;

        try {
            arrayType = getInt(endianess);

            if (arrayType == FileDM3.STRUCT) {
                encodedType = readStructTypes();
            } else if (arrayType == FileDM3.ARRAY) {
                encodedType = readArrayTypes();
            } else {
                encodedType = new int[] {arrayType};
            }

            arraySize = getInt(endianess);
            Preferences.debug("Array size = " + arraySize + "\n", Preferences.DEBUG_FILEIO);

            if (isData) {
                arraySizeArray[imageNum] = arraySize;
            }

            for (i = 0; i < encodedType.length; i++) {

                switch (encodedType[i]) {

                    case BOOLEAN:
                    case CHAR:
                    case OCTET:
                        elementBytes += 1;
                        break;

                    case SHORT:
                    case USHORT:
                        elementBytes += 2;
                        break;

                    case LONG:
                    case ULONG:
                    case FLOAT:
                        elementBytes += 4;
                        break;

                    case DOUBLE:
                        elementBytes += 8;
                        break;
                } // switch(encodedType[i])
            } // for (i = 0; i < encodedType.length; i++)

            Preferences.debug("elementBytes = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
            bufferSize = arraySize * elementBytes;
            Preferences.debug("bufferSize = " + bufferSize + "\n", Preferences.DEBUG_FILEIO);
            arrayLocation = raFile.getFilePointer();

            if (isData) {
                arrayLocationArray[imageNum] = arrayLocation;
                isData = false;
            }

            if ( (isImageData) && (isCalibrations) && (isDimension) && (isUnits)) {
                pixelUnitsArray[imageNum][unitsIndex] = getString(elementBytes);
                Preferences.debug("pixelUnitsArray[" + imageNum + "][ " + unitsIndex + "] = "
                        + pixelUnitsArray[imageNum][unitsIndex] + "\n", Preferences.DEBUG_FILEIO);
                unitsIndex++;
                pixelUnitsNumber[imageNum] = unitsIndex;
                isUnits = false;
            }

            Preferences.debug("Array location = " + arrayLocation + "\n", Preferences.DEBUG_FILEIO);
            raFile.seek(arrayLocation + bufferSize);

            return;
        } catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            raFile.close();

            throw new IOException();
        }

    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     * 
     * @throws IOException DOCUMENT ME!
     */
    private int[] readArrayTypes() throws IOException {
        int arrayType;
        int[] itemTypes;

        try {
            arrayType = getInt(endianess);

            if (arrayType == FileDM3.STRUCT) {
                itemTypes = readStructTypes();
            } else if (arrayType == FileDM3.ARRAY) {
                itemTypes = readArrayTypes();
            } else {
                itemTypes = new int[] {arrayType};
            }

            return itemTypes;

        } catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            raFile.close();

            throw new IOException();
        }

    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     * 
     * @param slice offset into the file stored in the dataOffset array
     * @param buffer buffer where the info is stored
     * 
     * @exception IOException if there is an error reading the file
     */
    private void readBuffer(final int slice, final float[] buffer) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        int b1, b2, b3, b4;
        byte[] byteBuffer;
        int progress, progressLength, mod;
        int tmpInt;

        switch (sourceType) {

            case ModelStorageBase.BYTE:
                nBytes = buffer.length;
                byteBuffer = new byte[nBytes];
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 100;

                for (j = 0; j < nBytes; j++, i++) {

                    if ( ( (i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = byteBuffer[j];
                }

                break;

            case ModelStorageBase.UBYTE:
                byteBuffer = new byte[buffer.length];
                nBytes = buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 100;

                for (j = 0; j < nBytes; j++, i++) {

                    if ( ( (i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = byteBuffer[j] & 0xff;
                }

                break;

            case ModelStorageBase.SHORT:
                byteBuffer = new byte[2 * buffer.length];
                nBytes = 2 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 2, i++) {

                    if ( ( (i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);

                    if (dataEndianess) {
                        buffer[i] = (short) ( (b1 << 8) + b2);
                    } else {
                        buffer[i] = (short) ( (b2 << 8) + b1);
                    }

                } // for (j = 0; j < nBytes; j+=2, i++ )

                break;

            case ModelStorageBase.FLOAT:
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 4 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if ( ( (i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    b3 = getUnsignedByte(byteBuffer, j + 2);
                    b4 = getUnsignedByte(byteBuffer, j + 3);

                    if (dataEndianess) {
                        tmpInt = ( (b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    } else {
                        tmpInt = ( (b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                    }

                    buffer[i] = Float.intBitsToFloat(tmpInt);

                } // for (j =0; j < nBytes; j+=4, i++ )

                break;

            case ModelStorageBase.USHORT:
                byteBuffer = new byte[2 * buffer.length];
                nBytes = 2 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 2, i++) {

                    if ( ( (i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);

                    if (dataEndianess) {
                        buffer[i] = ( (b1 << 8) + b2);
                    } else {
                        buffer[i] = ( (b2 << 8) + b1);
                    }

                } // for (j = 0; j < nBytes; j+=2, i++ )

                break;

            case ModelStorageBase.INTEGER:
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 4 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if ( ( (i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    b3 = getUnsignedByte(byteBuffer, j + 2);
                    b4 = getUnsignedByte(byteBuffer, j + 3);

                    if (dataEndianess) {
                        buffer[i] = ( (b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    } else {
                        buffer[i] = ( (b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                    }
                } // for (j =0; j < nBytes; j+=4, i++ )

                break;

            case ModelStorageBase.ARGB:
                nBytes = buffer.length;
                byteBuffer = new byte[nBytes];
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * nBytes;
                progressLength = nBytes * numberSlices;
                mod = progressLength / 100;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if ( ( (i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[ (4 * i) + 1] = byteBuffer[j] & 0xff;
                    buffer[ (4 * i) + 2] = byteBuffer[j + 1] & 0xff;
                    buffer[ (4 * i) + 3] = byteBuffer[j + 2] & 0xff;
                }

                break;
        } // switch(sourceType)

    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     * 
     * @param slice offset into the file stored in the dataOffset array
     * @param bufferR buffer where the real info is stored
     * @param bufferI buffer where the imaginary info is stored
     * 
     * @exception IOException if there is an error reading the file
     */
    private void readComplexBuffer(final int slice, final float[] bufferR, final float[] bufferI) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        int b1, b2, b3, b4;
        byte[] byteBuffer;
        int progress, progressLength, mod;
        int tmpInt;

        nBytes = 8 * bufferR.length;
        byteBuffer = new byte[nBytes];
        raFile.read(byteBuffer, 0, nBytes);
        progress = slice * bufferR.length;
        progressLength = bufferR.length * numberSlices;
        mod = progressLength / 10;

        for (j = 0; j < nBytes; j += 8, i++) {

            if ( ( (i + progress) % mod) == 0) {
                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
            }

            b1 = getUnsignedByte(byteBuffer, j);
            b2 = getUnsignedByte(byteBuffer, j + 1);
            b3 = getUnsignedByte(byteBuffer, j + 2);
            b4 = getUnsignedByte(byteBuffer, j + 3);

            if (dataEndianess) {
                tmpInt = ( (b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
            } else {
                tmpInt = ( (b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
            }

            bufferR[i] = Float.intBitsToFloat(tmpInt);

            b1 = getUnsignedByte(byteBuffer, j + 4);
            b2 = getUnsignedByte(byteBuffer, j + 5);
            b3 = getUnsignedByte(byteBuffer, j + 6);
            b4 = getUnsignedByte(byteBuffer, j + 7);

            if (dataEndianess) {
                tmpInt = ( (b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
            } else {
                tmpInt = ( (b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
            }

            bufferI[i] = Float.intBitsToFloat(tmpInt);
        } // for (j =0; j < nBytes; j+=8, i++ )
    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     * 
     * @param slice offset into the file stored in the dataOffset array
     * @param buffer buffer where the info is stored
     * 
     * @exception IOException if there is an error reading the file
     */
    private void readDBuffer(final int slice, final double[] buffer) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        long b1, b2, b3, b4, b5, b6, b7, b8;
        byte[] byteBuffer;
        int progress, progressLength, mod;
        long tmpLong;

        byteBuffer = new byte[8 * buffer.length];
        nBytes = 8 * buffer.length;
        raFile.read(byteBuffer, 0, nBytes);
        progress = slice * buffer.length;
        progressLength = buffer.length * numberSlices;
        mod = progressLength / 10;

        for (j = 0; j < nBytes; j += 8, i++) {

            if ( ( (i + progress) % mod) == 0) {
                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
            }

            b1 = getUnsignedByte(byteBuffer, j);
            b2 = getUnsignedByte(byteBuffer, j + 1);
            b3 = getUnsignedByte(byteBuffer, j + 2);
            b4 = getUnsignedByte(byteBuffer, j + 3);
            b5 = getUnsignedByte(byteBuffer, j + 4);
            b6 = getUnsignedByte(byteBuffer, j + 5);
            b7 = getUnsignedByte(byteBuffer, j + 6);
            b8 = getUnsignedByte(byteBuffer, j + 7);

            if (dataEndianess) {
                tmpLong = ( (b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) | (b5 << 24) | (b6 << 16) | (b7 << 8) | b8); // Big
                                                                                                                           // Endian
            } else {
                tmpLong = ( (b8 << 56) | (b7 << 48) | (b6 << 40) | (b5 << 32) | (b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little
                                                                                                                           // Endian
            }

            buffer[i] = Double.longBitsToDouble(tmpLong);

        } // for (j =0; j < nBytes; j+=8, i++ )

    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     * 
     * @param slice offset into the file stored in the dataOffset array
     * @param bufferR buffer where the real info is stored
     * @param bufferI buffer where the imaginary info is stored
     * 
     * @exception IOException if there is an error reading the file
     */
    private void readDComplexBuffer(final int slice, final double[] bufferR, final double[] bufferI) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        long b1, b2, b3, b4, b5, b6, b7, b8;
        byte[] byteBuffer;
        int progress, progressLength, mod;
        long tmpLong;

        nBytes = 16 * bufferR.length;
        byteBuffer = new byte[nBytes];
        raFile.read(byteBuffer, 0, nBytes);
        progress = slice * bufferR.length;
        progressLength = bufferR.length * numberSlices;
        mod = progressLength / 10;

        for (j = 0; j < nBytes; j += 16, i++) {

            if ( ( (i + progress) % mod) == 0) {
                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
            }

            b1 = getUnsignedByte(byteBuffer, j);
            b2 = getUnsignedByte(byteBuffer, j + 1);
            b3 = getUnsignedByte(byteBuffer, j + 2);
            b4 = getUnsignedByte(byteBuffer, j + 3);
            b5 = getUnsignedByte(byteBuffer, j + 4);
            b6 = getUnsignedByte(byteBuffer, j + 5);
            b7 = getUnsignedByte(byteBuffer, j + 6);
            b8 = getUnsignedByte(byteBuffer, j + 7);

            if (dataEndianess) {
                tmpLong = ( (b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) | (b5 << 24) | (b6 << 16) | (b7 << 8) | b8); // Big
                                                                                                                           // Endian
            } else {
                tmpLong = ( (b8 << 56) | (b7 << 48) | (b6 << 40) | (b5 << 32) | (b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little
                                                                                                                           // Endian
            }

            bufferR[i] = Double.longBitsToDouble(tmpLong);

            b1 = getUnsignedByte(byteBuffer, j + 8);
            b2 = getUnsignedByte(byteBuffer, j + 9);
            b3 = getUnsignedByte(byteBuffer, j + 10);
            b4 = getUnsignedByte(byteBuffer, j + 11);
            b5 = getUnsignedByte(byteBuffer, j + 12);
            b6 = getUnsignedByte(byteBuffer, j + 13);
            b7 = getUnsignedByte(byteBuffer, j + 14);
            b8 = getUnsignedByte(byteBuffer, j + 15);

            if (dataEndianess) {
                tmpLong = ( (b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) | (b5 << 24) | (b6 << 16) | (b7 << 8) | b8); // Big
                                                                                                                           // Endian
            } else {
                tmpLong = ( (b8 << 56) | (b7 << 48) | (b6 << 40) | (b5 << 32) | (b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little
                                                                                                                           // Endian
            }

            bufferI[i] = Double.longBitsToDouble(tmpLong);
        } // for (j =0; j < nBytes; j+=16, i++ )
    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     * 
     * @param slice offset into the file stored in the dataOffset array
     * @param buffer buffer where the info is stored
     * 
     * @exception IOException if there is an error reading the file
     */
    private void readLBuffer(final int slice, final long[] buffer) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        long b1, b2, b3, b4, b5, b6, b7, b8;
        byte[] byteBuffer;
        int progress, progressLength, mod;

        if (sourceType == ModelStorageBase.UINTEGER) { // reading 4 byte unsigned integers
            byteBuffer = new byte[4 * buffer.length];
            nBytes = 4 * buffer.length;
            raFile.read(byteBuffer, 0, nBytes);
            progress = slice * buffer.length;
            progressLength = buffer.length * numberSlices;
            mod = progressLength / 10;

            for (j = 0; j < nBytes; j += 4, i++) {

                if ( ( (i + progress) % mod) == 0) {
                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                }

                b1 = getUnsignedByte(byteBuffer, j);
                b2 = getUnsignedByte(byteBuffer, j + 1);
                b3 = getUnsignedByte(byteBuffer, j + 2);
                b4 = getUnsignedByte(byteBuffer, j + 3);

                if (dataEndianess) {
                    buffer[i] = ( (b1 << 24) | (b2 << 16) | (b3 << 8) | b4) & 0xffffffffL;
                } else {
                    buffer[i] = ( (b4 << 24) | (b3 << 16) | (b2 << 8) | b1) & 0xffffffffL;
                }
            } // for (j =0; j < nBytes; j+=4, i++ )
        } // if (type == ModelStorageBase.UINTEGER)
        else { // reading 8 byte LONGS
            byteBuffer = new byte[8 * buffer.length];
            nBytes = 8 * buffer.length;
            raFile.read(byteBuffer, 0, nBytes);
            progress = slice * buffer.length;
            progressLength = buffer.length * numberSlices;
            mod = progressLength / 10;

            for (j = 0; j < nBytes; j += 8, i++) {

                if ( ( (i + progress) % mod) == 0) {
                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                }

                b1 = getUnsignedByte(byteBuffer, j);
                b2 = getUnsignedByte(byteBuffer, j + 1);
                b3 = getUnsignedByte(byteBuffer, j + 2);
                b4 = getUnsignedByte(byteBuffer, j + 3);
                b5 = getUnsignedByte(byteBuffer, j + 4);
                b6 = getUnsignedByte(byteBuffer, j + 5);
                b7 = getUnsignedByte(byteBuffer, j + 6);
                b8 = getUnsignedByte(byteBuffer, j + 7);

                if (dataEndianess) {
                    buffer[i] = ( (b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) | (b5 << 24) | (b6 << 16)
                            | (b7 << 8) | b8); // Big Endian
                } else {
                    buffer[i] = ( (b8 << 56) | (b7 << 48) | (b6 << 40) | (b5 << 32) | (b4 << 24) | (b3 << 16)
                            | (b2 << 8) | b1); // Little Endian
                }
            } // for (j =0; j < nBytes; j+=8, i++ )
        } // else reading 8 byte integers
    }

    /**
     * DOCUMENT ME!
     * 
     * @param index DOCUMENT ME!
     * @param encodedType DOCUMENT ME!
     * 
     * @throws IOException DOCUMENT ME!
     */
    private void readSimpleData(final int index, final int encodedType) throws IOException {
        final byte[] dataByte = new byte[1];
        int dataShort;
        int dataInt;
        long dataUInt;
        float dataFloat;
        double dataDouble;
        String s;

        try {

            switch (encodedType) {

                case BOOLEAN:
                    Preferences.debug("Data is 1 boolean = ", Preferences.DEBUG_FILEIO);
                    break;

                case CHAR:
                    Preferences.debug("Data is 1 char = ", Preferences.DEBUG_FILEIO);
                    break;

                case OCTET:
                    Preferences.debug("Data is 1 octet = ", Preferences.DEBUG_FILEIO);
                    break;

                case SHORT:
                    Preferences.debug("Data is 1 short = ", Preferences.DEBUG_FILEIO);
                    break;

                case USHORT:
                    Preferences.debug("Data is 1 USHORT = ", Preferences.DEBUG_FILEIO);
                    break;

                case LONG:
                    Preferences.debug("Data is 1 long = ", Preferences.DEBUG_FILEIO);
                    break;

                case ULONG:
                    Preferences.debug("Data is 1 ULONG = ", Preferences.DEBUG_FILEIO);
                    break;

                case FLOAT:
                    Preferences.debug("Data is 1 FLOAT = ", Preferences.DEBUG_FILEIO);
                    break;

                case DOUBLE:
                    Preferences.debug("Data is 1 DOUBLE = ", Preferences.DEBUG_FILEIO);
                    break;

                default:
                    Preferences.debug("Illegal encoded data type = " + encodedType + "\n", Preferences.DEBUG_FILEIO);
                    MipavUtil.displayError("Illegal encoded data type");
                    throw new IOException();
            }

            if ( (encodedType == FileDM3.BOOLEAN) || (encodedType == FileDM3.CHAR) || (encodedType == FileDM3.OCTET)) {
                dataByte[0] = raFile.readByte();

                if (encodedType == FileDM3.CHAR) {
                    s = new String(dataByte);
                    Preferences.debug(s + "\n", Preferences.DEBUG_FILEIO);
                } else {
                    Preferences.debug(dataByte[0] + "\n", Preferences.DEBUG_FILEIO);
                }
            } // if ((encodedType == BOOLEAN) || (encodedType == CHAR) || (encodedType == OCTET))
            else if (encodedType == FileDM3.SHORT) {
                dataShort = getSignedShort(dataEndianess);
                Preferences.debug(dataShort + "\n", Preferences.DEBUG_FILEIO);
            } else if (encodedType == FileDM3.USHORT) {
                dataShort = getSignedShort(dataEndianess);
                Preferences.debug(dataShort + "\n", Preferences.DEBUG_FILEIO);
            } else if (encodedType == FileDM3.LONG) {
                dataInt = getInt(dataEndianess);
                Preferences.debug(dataInt + "\n", Preferences.DEBUG_FILEIO);

                if ( (isImageData) && (isDimensions)) {
                    dimArray[imageNum][index] = dataInt;
                } else if ( (isImageData) && (isDataType)) {
                    dataTypeArray[imageNum] = dataInt;
                    isDataType = false;
                }
            } else if (encodedType == FileDM3.ULONG) {
                dataUInt = getUInt(dataEndianess);
                Preferences.debug(dataUInt + "\n", Preferences.DEBUG_FILEIO);

                if ( (isImageData) && (isDimensions)) {
                    dimArray[imageNum][index] = (int) dataUInt;
                } else if ( (isImageData) && (isDataType)) {
                    dataTypeArray[imageNum] = (int) dataUInt;
                    isDataType = false;
                }
            } else if (encodedType == FileDM3.FLOAT) {
                dataFloat = getFloat(dataEndianess);
                Preferences.debug(dataFloat + "\n", Preferences.DEBUG_FILEIO);

                if ( (isImageData) && (isCalibrations) && (isDimension) && (isScale)) {
                    Preferences.debug("About to set pixelScaleArray[" + imageNum + "][" + scaleIndex + "]\n", 
                    		Preferences.DEBUG_FILEIO);
                    pixelScaleArray[imageNum][scaleIndex++] = dataFloat;
                    isScale = false;
                }
            } else if (encodedType == FileDM3.DOUBLE) {
                dataDouble = getDouble(dataEndianess);
                Preferences.debug(dataDouble + "\n", Preferences.DEBUG_FILEIO);
            }

        } catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            raFile.close();

            throw new IOException();
        }

    }

    /**
     * DOCUMENT ME!
     * 
     * @throws IOException DOCUMENT ME!
     */
    private void readString() throws IOException {
        int stringSize;
        byte[] buffer;
        String dataString;

        try {
            stringSize = getInt(endianess);
            buffer = new byte[stringSize];
            raFile.read(buffer, 0, stringSize);

            if (dataEndianess) {
                dataString = new String(buffer, "UTF-16BE");
            } else {
                dataString = new String(buffer, "UTF-16LE");
            }

            Preferences.debug(dataString + "\n", Preferences.DEBUG_FILEIO);
        } catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            raFile.close();

            throw new IOException();
        }

    }

    /**
     * DOCUMENT ME!
     * 
     * @throws IOException DOCUMENT ME!
     */
    private void readStruct() throws IOException {
        int fieldNum;
        int[] fieldType;
        int i;

        try {
            getInt(endianess);
            fieldNum = getInt(endianess);

            fieldType = new int[fieldNum];

            for (i = 0; i < fieldNum; i++) {
                getInt(endianess);
                fieldType[i] = getInt(endianess);
            }

            for (i = 0; i < fieldNum; i++) {
                readSimpleData(i, fieldType[i]);
            }

        } catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            raFile.close();

            throw new IOException();
        }

    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     * 
     * @throws IOException DOCUMENT ME!
     */
    private int[] readStructTypes() throws IOException {
        int fieldNum;
        int[] fieldType;
        int i;

        try {
            getInt(endianess);
            fieldNum = getInt(endianess);
            fieldType = new int[fieldNum];

            for (i = 0; i < fieldNum; i++) {
                getInt(endianess);
                fieldType[i] = getInt(endianess);
            }

            return fieldType;
        } catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            raFile.close();

            throw new IOException();
        }

    }

    /**
     * DOCUMENT ME!
     * 
     * @param index DOCUMENT ME!
     * 
     * @throws IOException DOCUMENT ME!
     */
    private void readTagEntry(final int index) throws IOException {
        byte dataByte;
        int entryStringLength;
        String entryString;
        routineTagEntry++;

        int calibrationsEntry = -1;
        int dimensionEntry = -1;
        int imageDataEntry = -1;

        try {
            dataByte = raFile.readByte();

            if (dataByte == 21) {
                Preferences.debug("Tag entry byte indicates data\n", Preferences.DEBUG_FILEIO);
            } else if (dataByte == 20) {
                Preferences.debug("Tag entry byte indicates another tag group\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("Tag entry byte is an illegal = " + isData + "\n", Preferences.DEBUG_FILEIO);
                MipavUtil.displayError("Tag entry byte is an illegal " + isData);
                throw new IOException();
            }

            entryStringLength = getUnsignedShort(endianess);

            if (entryStringLength != 0) {
                entryString = getString(entryStringLength);
                Preferences.debug("Tag label = " + entryString + "\n", Preferences.DEBUG_FILEIO);

                if (entryString.equalsIgnoreCase("ImageData")) {
                    isImageData = true;
                    imageDataEntry = routineTagEntry;
                    imageNum++;
                } else if (entryString.equalsIgnoreCase("Dimensions")) {
                    isDimensions = true;
                } else if (entryString.equalsIgnoreCase("Data")) {
                    isData = true;
                } else if (entryString.equalsIgnoreCase("DataType")) {
                    isDataType = true;
                } else if (entryString.equalsIgnoreCase("Calibrations")) {
                    calibrationsEntry = routineTagEntry;
                    isCalibrations = true;
                } else if (entryString.equalsIgnoreCase("Dimension")) {
                    isDimension = true;
                    dimensionEntry = routineTagEntry;
                    scaleIndex = 0;
                    unitsIndex = 0;
                } else if (entryString.equalsIgnoreCase("Scale")) {
                    isScale = true;
                } else if (entryString.equalsIgnoreCase("Units")) {
                    isUnits = true;
                }

            }

            if (dataByte == 21) {
                readTagType(index);
            } else {
                readTagGroup();
            }

            if (imageDataEntry == routineTagEntry) {
                isImageData = false;
            } else if (calibrationsEntry == routineTagEntry) {
                isCalibrations = false;
            } else if (dimensionEntry == routineTagEntry) {
                isDimension = false;
            }
        } catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            raFile.close();

            throw new IOException();
        }

    }

    /**
     * DOCUMENT ME!
     * 
     * @throws IOException DOCUMENT ME!
     */
    private void readTagGroup() throws IOException {
        int i;
        int tagEntries;
        byte groupSorted;
        byte groupOpen;
        int dimensionsEntry = -1;
        routineTagGroup++;

        try {
            groupSorted = raFile.readByte();
            Preferences.debug("Group sorted = " + groupSorted + "\n", Preferences.DEBUG_FILEIO);
            groupOpen = raFile.readByte();
            Preferences.debug("Group open = " + groupOpen + "\n", Preferences.DEBUG_FILEIO);
            tagEntries = getInt(endianess);
            Preferences.debug("Number of tag entries = " + tagEntries + "\n", Preferences.DEBUG_FILEIO);

            if (isDimensions) {
                dimensionsEntry = routineTagGroup;
                numDimArray[imageNum] = tagEntries;
            }

            for (i = 0; i < tagEntries; i++) {
                Preferences.debug("Reading tag entry " + (i + 1) + " of " + tagEntries + " in this group\n", 
                		Preferences.DEBUG_FILEIO);
                readTagEntry(i);
            }

            if (dimensionsEntry == routineTagGroup) {
                isDimensions = false;
            }
        } catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            raFile.close();

            throw new IOException();
        }

    } // private void readTagGroup

    /**
     * DOCUMENT ME!
     * 
     * @param index DOCUMENT ME!
     * 
     * @throws IOException DOCUMENT ME!
     */
    private void readTagType(final int index) throws IOException {
        String delimString;
        int encodedType;

        try {
            delimString = getString(4);

            // The first 4 bytes should always be %%%%
            if ( !delimString.equals("%%%%")) {
                Preferences.debug("delimString is an illegal = " + delimString + "|n", Preferences.DEBUG_FILEIO);
                MipavUtil.displayError("Illegal delimiter string");
                throw new IOException();
            }

            // Ignore
            getInt(endianess);
            encodedType = getInt(endianess);

            if ( (encodedType != FileDM3.STRING) && (encodedType != FileDM3.ARRAY) && (encodedType != FileDM3.STRUCT)) {
                readSimpleData(index, encodedType);

                return;
            }

            switch (encodedType) {

                case STRING:
                    Preferences.debug("Data is STRING\n", Preferences.DEBUG_FILEIO);
                    readString();
                    break;

                case STRUCT:
                    Preferences.debug("Data is STRUCT\n", Preferences.DEBUG_FILEIO);
                    readStruct();
                    break;

                case ARRAY:
                    Preferences.debug("Data is ARRAY\n", Preferences.DEBUG_FILEIO);
                    readArray();
                    break;

                default:
                    Preferences.debug("Illegal encoded data type = " + encodedType + "\n", Preferences.DEBUG_FILEIO);
                    MipavUtil.displayError("Illegal encoded data type");
                    throw new IOException();
            }

        } catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            raFile.close();

            throw new IOException();
        }

    }

}
