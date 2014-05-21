package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * The class reads and writes SPM files.
 *
 * @see  FileIO
 * @see  FileInfoSPM
 * @see  FileRaw
 * 
 * Note that there are actually 3 variants of Analyze, all using .img and .hdr files,
 * with very slight differences:
 * Mayo Analyze 7.5, SPM99, and SPM2
 * Variant differences: 
 * Location 0 is an int32 with sizeof_hdr.  In Non-SPM2, always 348, used to
 * test whether the file is big-endian or little-endian.  SPM2 can be 348 or greater.  If SPM2 > 348,
 * indicates extended header.
 * The user could change .img to .spm to indicate SPM99 or SPM2.  The .hdr extension would remain unchanged.
 * 
 * <p>Original Mayo Analyze 7.5, SPM99, and SPM2 all have at location 60 a uchar[8] for cal_units and at location 
 * 68 an int16 for unused1.  However, MIPAV analyze only preserves at location 60 a uchar[4] for cal_units and
 * has hacked locations 64, 66, and 68 with short integers for axis orientation.  MIPAV calls these locations 
 * unused1, unused2, and unused3.  Note that MIPAV's unused3 is at the same location as unused1 in any
 * original format.</p>
 * 
 * <p>Original Mayo Analyze 7.5 has at locations 112, 116, and 112 3 unused floats, funused1, funused2, and
 * funused3.  SPM99 and SPM2 have at location 112 a float scale factor.  SPM2 has at location 116 a float 
 * dcoff, which is an intensity zero-intercept.  MIPAV analyze has hacked these 3 locations to store 3 floats
 * giving the x-origin, y-origin, and z-origin locations.</p>
 * 
 * <p>Mayo Analyze 7.5 has at location 253 a uchar[10] called originator.  SPM99 and SPM2 have at location 253
 * 5-int16 called origin[0] thru origin[4].  In SPM99 X, Y, and Z are near the anterior commissure. 
 * If the first 3 shorts of a 3D image are set to 0, 0, 0, the origin is assumed to be at the center of
 * the volume, since in SPM the corner voxel is at 1, 1, 1. The position(x,y,z) in mm. is determined by the
 * distance(x,y,z) from the origin multiplied by the vox_units(x,y,z).</p>
 *
 * <p>In SPM the signed byte datatype was added with DT_BYTE = 130. MIPAV ANALYZE uses UNSIGNED_SHORT = 6 while
 * SPM uses DT_UNSIGNED_SHORT = 132. The SPM standard also provides for an unsigned int = 136.
 * Note that in SPM while DATA = datatype * 256 for swapped bytes, only datatype
 * and not DATA is written to the file, so in this program we need never consider the multiplicative factors of
 * 256.</p>
 * 
 * <p>The most obvious way to test for whether a file is Mayo or SPM would be to see if location 112 is a
 * funused1 = 0 or a nonzero scale factor, but this is complicated by the MIPAV hacking at 112 to store the
 * x-origin location as a float.  Asking the users what variant there analyze file is would probably cause
 * massive confusion, so it is probably best for MIPAV just to default to the hacked 7.5 code in
 * FileAnalyze unless a pressing  need arises.</p>
 */

public class FileSPM extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int DT_NONE = 0;

    /** DOCUMENT ME! */
    //private static final int DT_UNKNOWN = 0;

    /** DOCUMENT ME! */
    private static final int DT_BINARY = 1;

    /** DOCUMENT ME! */
    private static final int DT_BYTE = 130;

    /** DOCUMENT ME! */
    private static final int DT_UNSIGNED_CHAR = 2;

    /** DOCUMENT ME! */
    private static final int DT_SIGNED_SHORT = 4;

    /** DOCUMENT ME! */
    private static final int DT_UNSIGNED_SHORT = 132;

    /** DOCUMENT ME! */
    private static final int DT_SIGNED_INT = 8;

    /** DOCUMENT ME! */
    private static final int DT_UNSIGNED_INT = 136;

    /** DOCUMENT ME! */
    private static final int DT_FLOAT = 16;

    /** DOCUMENT ME! */
    private static final int DT_COMPLEX = 32;

    /** DOCUMENT ME! */
    private static final int DT_DOUBLE = 64;

    /** DOCUMENT ME! */
    private static final int DT_RGB = 128;

    /** DOCUMENT ME! */
    //private static final int DT_ALL = 255;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private byte[] bufferByte = null;

    /** DOCUMENT ME! */
    private String fileDir;

    

    /** DOCUMENT ME! */
    private FileInfoSPM fileInfo = null;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private int headerSize = 348;

    /** DOCUMENT ME! */
    private ModelImage image;
    
    private String headerFileName;
    
    /** DOCUMENT ME! */
    private File headerFile;
    
    private String dataFileName;
    
    private File dataFile;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileSPM(String fName, String fDir) {
        fileName = fName;
        fileDir = fDir;
        
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        bufferByte = null;
        fileName = null;
        fileDir = null;
        fileInfo = null;
        image = null;
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * Reads the SPM header and stores the information in fileInfo.
     *
     * @param      imageFileName  File name of image.
     * @param      fileDir        Directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error reading the header
     *
     * @see        FileInfoSPM
     */

    /**
     * Determines whether this file is SPM file or not based on three fields of the header file: sizeof_hdr, extent and
     * regular.
     *
     * @param   absolutePath  the file name.
     *
     * @return  true if the file is ANALYZE file.
     *
     * @throws  FileNotFoundException  thrown when the file can't be found.
     * @throws  IOException            thrown when the I/O error happens.
     */
    public static boolean isSPM(String absolutePath) throws FileNotFoundException, IOException {
        int b1, b2, b3, b4, tmpInt;

        final int HEADER_SIZE = 348;

        if ((absolutePath == null) || (absolutePath.length() == 0)) {
            return false;
        }

        String[] completeFileNames = FileAnalyze.getCompleteFileNameList(absolutePath);

        if ((completeFileNames == null) || (completeFileNames.length != 2)) {
            return false;
        }

        for (int i = 0; i < completeFileNames.length; i++) {
            File file = new File(completeFileNames[i]);

            if (!file.exists()) {
                Preferences.debug("FileSPM: The file can not be found: " + completeFileNames[i] + "!",
                		Preferences.DEBUG_FILEIO);
                return false;
            }
        }

        RandomAccessFile raFile = new RandomAccessFile(FileAnalyze.getHeaderFile(completeFileNames), "r");

        /** Check whether the value of sizeof_hdr field equals to the 348 */
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        boolean bigEndian = true;
        int sizeOfHeader1 = FileBase.bytesToInt(bigEndian, 0, buffer);

        if (sizeOfHeader1 != HEADER_SIZE) {
            bigEndian = false;
            int sizeOfHeader2 = FileBase.bytesToInt(bigEndian, 0, buffer);

            if ((sizeOfHeader1 < HEADER_SIZE) && (sizeOfHeader2 < HEADER_SIZE)) {
                return false;
            }
            else {
                // Assume SPM2 with extended header size
                if (sizeOfHeader1 < sizeOfHeader2) {
                    bigEndian = true;
                    Preferences.debug("If valid must be SPM2 with extended header size = " +
                                  sizeOfHeader1 + "\n", Preferences.DEBUG_FILEIO);
                }
                else {
                    Preferences.debug("If valid must be SPM2 with extended header size = " +
                            sizeOfHeader2 + "\n", Preferences.DEBUG_FILEIO);    
                }  
            }
        }


        /** Check whether the value of regular field equals to "r" */
        raFile.seek(38);

        byte regular = raFile.readByte();

        if (regular != 'r') {
            return false;
        }

        // Don't test for dims == 4
        // dims are usually but not always 4

        raFile.seek(112);
        
        b1 = (raFile.readByte() & 0xff);
        b2 = (raFile.readByte() & 0xff);
        b3 = (raFile.readByte() & 0xff);
        b4 = (raFile.readByte() & 0xff);

        if (bigEndian) {
            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
        } else {
            tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
        }

        float scale = Float.intBitsToFloat(tmpInt);

        if (scale == 0.0) {
            return false;
        }

        raFile.close();

        return true;

    }

    /**
     * Flips image. SPM stores its data "upside down".
     *
     * @param   image  Image to flip.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void flipTopBottom(ModelImage image) throws IOException {

        try {
            int nBuffers;
            int bufferSize;
            float[] buffer = null;
            float[] resultBuffer = null;

            if (image.getNDims() > 1) {
                bufferSize = image.getSliceSize();
            } else {
                bufferSize = image.getExtents()[0];
            }

            if (image.getNDims() == 5) {
                nBuffers = image.getExtents()[4] * image.getExtents()[3] * image.getExtents()[2];

            } else if (image.getNDims() == 4) {
                nBuffers = image.getExtents()[3] * image.getExtents()[2];
            } else if (image.getNDims() == 3) {
                nBuffers = image.getExtents()[2];
            } else {
                nBuffers = 1;
            }

            if (image.isColorImage()) {

                buffer = new float[bufferSize * 4];
                resultBuffer = new float[bufferSize * 4];
                bufferSize = bufferSize * 4;

                int i, j, k;
                int xDim = image.getExtents()[0] * 4;
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i += 4) {
                            resultBuffer[(j * xDim) + i] = 255;
                            resultBuffer[(j * xDim) + i + 1] = buffer[((yDim - 1 - j) * xDim) + i + 1];
                            resultBuffer[(j * xDim) + i + 2] = buffer[((yDim - 1 - j) * xDim) + i + 2];
                            resultBuffer[(j * xDim) + i + 3] = buffer[((yDim - 1 - j) * xDim) + i + 3];
                        }
                    }

                    image.importData(k * bufferSize, resultBuffer, false);
                }
            } else {
                buffer = new float[bufferSize];
                resultBuffer = new float[bufferSize];

                int i, j, k;
                int xDim = image.getExtents()[0];
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            resultBuffer[(j * xDim) + i] = buffer[((yDim - 1 - j) * xDim) + i];
                        }
                    }

                    image.importData(k * bufferSize, resultBuffer, false);
                }
            }
        } catch (IOException error) {
            throw new IOException("FileSPM.flipTopBottom: " + error);
        } catch (OutOfMemoryError error) {
            throw (error);
        }
    }

    /**
     * Flips image. SPM stores its data "upside down".
     *
     * @param   buffer    Buffer holding image to flip.
     * @param   fileInfo  File info structure for image to flip.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void flipTopBottom(float[] buffer, FileInfoSPM fileInfo) throws IOException {
        int nBuffers;
        int bufferSize;
        float[] resultBuffer = null;

        try {

            if ((fileInfo.getExtents().length - 1) > 1) {
                bufferSize = fileInfo.getExtents()[0] * fileInfo.getExtents()[1];
            } else {
                bufferSize = fileInfo.getExtents()[0];
            }

            if ((fileInfo.getExtents().length - 1) == 5) {
                nBuffers = fileInfo.getExtents()[4] * fileInfo.getExtents()[3] * fileInfo.getExtents()[2];

            } else if ((fileInfo.getExtents().length - 1) == 4) {
                nBuffers = fileInfo.getExtents()[3] * fileInfo.getExtents()[2];
            } else if ((fileInfo.getExtents().length - 1) == 3) {
                nBuffers = fileInfo.getExtents()[2];
            } else {
                nBuffers = 1;
            }

            if (fileInfo.getDataTypeCode() == DT_RGB) {

                resultBuffer = new float[bufferSize * 4];
                bufferSize = bufferSize * 4;

                int i, j, k;
                int xDim = image.getExtents()[0] * 4;
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i += 4) {
                            resultBuffer[(j * xDim) + i] = 255;
                            resultBuffer[(k * bufferSize) + (j * xDim) + i + 1] = buffer[(k * bufferSize) +
                                                                                         ((yDim - 1 - j) * xDim) + i + 1];
                            resultBuffer[(k * bufferSize) + (j * xDim) + i + 2] = buffer[(k * bufferSize) +
                                                                                         ((yDim - 1 - j) * xDim) + i + 2];
                            resultBuffer[(k * bufferSize) + (j * xDim) + i + 3] = buffer[(k * bufferSize) +
                                                                                         ((yDim - 1 - j) * xDim) + i + 3];
                        }
                    }
                }
            } else {
                resultBuffer = new float[buffer.length];

                int i, j, k;
                int xDim = fileInfo.getExtents()[0];
                int yDim = fileInfo.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            resultBuffer[(k * bufferSize) + (j * xDim) + i] = buffer[(k * bufferSize) +
                                                                                     ((yDim - 1 - j) * xDim) + i];
                        }
                    }
                }
            }
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        System.arraycopy(resultBuffer, 0, buffer, 0, buffer.length); // buffer = resultBuffer;
    }

    /**
     * Returns the FileInfoSPM read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoSPM getFileInfo() {
        return fileInfo;
    }

    /**
     * DOCUMENT ME!
     *
     *
     * @return  true if read successfully
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public boolean readHeader() throws IOException {
        int i;
        boolean endianess;
        int[] spmExtents = new int[5];
        int numDims = 0;

        bufferByte = new byte[headerSize];

        

        if (fileInfo == null) { // if the file info does not yet exist: make it
            fileInfo = new FileInfoSPM(dataFileName, fileDir, FileUtility.SPM);    
        }

        raFile = new RandomAccessFile(headerFile, "r");
        raFile.read(bufferByte);
        raFile.close();

        fileInfo.setEndianess(BIG_ENDIAN);
        int headerSize1 = getBufferInt(bufferByte, 0, BIG_ENDIAN);
        if (headerSize1 == headerSize) {
            fileInfo.setSizeOfHeader(headerSize1);
            Preferences.debug("FileSPM:readHeader Endianess = Big endian.\n", Preferences.DEBUG_FILEIO);
        }
        else { // Set the endianess based on header size = 348 Big Endian
            fileInfo.setEndianess(LITTLE_ENDIAN); // or 1,543,569,408 Little endian
            int headerSize2 = getBufferInt(bufferByte, 0, LITTLE_ENDIAN);
            if (headerSize2 == headerSize) {
                fileInfo.setSizeOfHeader(headerSize2);
                Preferences.debug("FileSPM:readHeader Endianess = Little endian.\n", Preferences.DEBUG_FILEIO);
            }
            else if ((headerSize1 < headerSize) && (headerSize2 < headerSize)) {
                Preferences.debug("FileSPM:readHeader SPM header length != 348.\n", Preferences.DEBUG_FILEIO);

                return false;   
            }
            else {
                // Assume SPM2 with extended header size > 348
                if (headerSize1 < headerSize2) {
                    fileInfo.setEndianess(BIG_ENDIAN);
                    fileInfo.setSizeOfHeader(headerSize1);
                    Preferences.debug("FileSPM:readHeader Endianess = Big endian.\n", Preferences.DEBUG_FILEIO);    
                }
                else {
                    fileInfo.setSizeOfHeader(headerSize2);
                    Preferences.debug("FileSPM:readHeader Endianess = Little endian.\n", Preferences.DEBUG_FILEIO);    
                }
            }
        }

        endianess = fileInfo.getEndianess();

        // The following reads in certain tags.  In some cases, it returns false and exits out of readHeader
        // if the information is wrong.
        fileInfo.setDataType(new String(bufferByte, 4, 10));

        // fileInfo.data_type.concat("\n");
        fileInfo.setDBname(new String(bufferByte, 14, 18));
        // fileInfo.db_name.concat("\n");

        fileInfo.setSessionErr(getBufferShort(bufferByte, 36, endianess));
        fileInfo.setRegular((char) bufferByte[38]);

        if (fileInfo.getRegular() != 'r') {
            return false;
        }

        fileInfo.setHkey((char) bufferByte[39]);

        int dims = getBufferShort(bufferByte, 40, endianess); // number of Dimensions should = 4

        // SPM dims = 4
        Preferences.debug("FileSPM:readHeader. Number of dimensions = " + dims + "\n", Preferences.DEBUG_FILEIO);

        for (i = 0; i < dims; i++) {
            spmExtents[i] = getBufferShort(bufferByte, 42 + (2 * i), endianess);
            Preferences.debug("FileSPM:readHeader. Dimension " + (i + 1) + " = " + spmExtents[i] + "\n",
            		Preferences.DEBUG_FILEIO);

            if (spmExtents[i] > 1) {
                numDims++;
            }
        }

        int[] extents = new int[numDims];

        for (i = 0; i < numDims; i++) {
            extents[i] = spmExtents[i];
        }

        fileInfo.setExtents(extents);
        fileInfo.setVoxUnits(new String(bufferByte, 56, 4));
        Preferences.debug("FileSPM:readHeader. Voxel unit = " + fileInfo.getVoxUnits() + "\n", Preferences.DEBUG_FILEIO);

        // fileInfo.vox_units.concat("\n");
        fileInfo.setCalUnits(new String(bufferByte, 60, 8));

        // fileInfo.cal_units.concat("\n");
        fileInfo.setOrientation(bufferByte[252]);
        fileInfo.setDataType(getBufferShort(bufferByte, 70, endianess));
        Preferences.debug("FileSPM:readHeader. Data type = " + fileInfo.getDataTypeCode() + "\n", Preferences.DEBUG_FILEIO);

        switch (fileInfo.getDataTypeCode()) { // Set the dataType in ModelStorage based on this tag

            case DT_NONE:
                return false;

            case DT_BINARY:
                fileInfo.setDataType(ModelStorageBase.BOOLEAN);
                break;

            case DT_BYTE:
                fileInfo.setDataType(ModelStorageBase.BYTE);
                break;

            case DT_UNSIGNED_CHAR:
                fileInfo.setDataType(ModelStorageBase.UBYTE);
                break;

            case DT_SIGNED_SHORT:
                fileInfo.setDataType(ModelStorageBase.SHORT);
                break;

            case DT_UNSIGNED_SHORT:
                fileInfo.setDataType(ModelStorageBase.USHORT);
                break;

            case DT_SIGNED_INT:
                fileInfo.setDataType(ModelStorageBase.INTEGER);
                break;

            case DT_UNSIGNED_INT:
                fileInfo.setDataType(ModelStorageBase.UINTEGER);
                break;

            case DT_FLOAT:
                fileInfo.setDataType(ModelStorageBase.FLOAT);
                break;

            case DT_COMPLEX:
                return false; // Should add complex type sometime!

            case DT_DOUBLE:
                fileInfo.setDataType(ModelStorageBase.DOUBLE);
                break;

            case DT_RGB:
                fileInfo.setDataType(ModelStorageBase.ARGB);
                break;

            default:
                return false;
        }

        fileInfo.setBitPix(getBufferShort(bufferByte, 72, endianess));
        Preferences.debug("FileSPM:readHeader. bits per pixel = " + fileInfo.getBitPix() + "\n", Preferences.DEBUG_FILEIO);

        fileInfo.setDim(getBufferShort(bufferByte, 74, endianess));

        float[] resolutions = new float[numDims];

        for (i = 0; i < numDims; i++) {
            resolutions[i] = getBufferFloat(bufferByte, 80 + (4 * i), endianess);

            if (resolutions[i] <= 0) {
                resolutions[i] = 1.0f; // Double check  resolutions - should probably notify user
            }

            Preferences.debug("FileSPM:readHeader. Resolutions " + (i + 1) + " = " + resolutions[i] + "\n",
            		Preferences.DEBUG_FILEIO);
        }

        fileInfo.setResolutions(resolutions);

        fileInfo.setVoxOffset(getBufferFloat(bufferByte, 108, endianess));

        fileInfo.setScale(getBufferFloat(bufferByte, 112, endianess));

        fileInfo.setCalMax(getBufferFloat(bufferByte, 124, endianess));
        fileInfo.setCalMin(getBufferFloat(bufferByte, 128, endianess));
        fileInfo.setCompressed(getBufferFloat(bufferByte, 132, endianess));
        fileInfo.setVerified(getBufferFloat(bufferByte, 136, endianess));

        fileInfo.setGLmax(getBufferInt(bufferByte, 140, endianess));
        Preferences.debug("FileSPM:readHeader. global max intensity = " + fileInfo.getGLmax() + "\n",
        		Preferences.DEBUG_FILEIO);

        fileInfo.setGLmin(getBufferInt(bufferByte, 144, endianess));
        Preferences.debug("FileSPM:readHeader. global min intensity = " + fileInfo.getGLmin() + "\n",
        		Preferences.DEBUG_FILEIO);

        fileInfo.setDescription(new String(bufferByte, 148, 80));

        // update the fileInfo modality based on the description
        // if the description contains something other than modality, then
        // the modality will be set to unknown.
        fileInfo.setModality(FileInfoBase.getModalityFromStr(fileInfo.getDescription()));

        fileInfo.setAuxFile(new String(bufferByte, 228, 24));

        short[] origin = new short[5];

        for (i = 0; i < 5; i++) {
            origin[i] = getBufferShort(bufferByte, 253 + (2 * i), endianess);
        }

        fileInfo.setOrigin(origin);
        float mipavOrigin[] = new float[] {0.0f, 0.0f, 0.0f, 0.0f, 0.0f};
        int axisOrient[] = fileInfo.getAxisOrientation();
        int direct[] = new int[numDims];
        for (i = 0; i < numDims; i++) {
            if ((axisOrient[i] == FileInfoBase.ORI_R2L_TYPE) || (axisOrient[i] == FileInfoBase.ORI_A2P_TYPE) ||
                (axisOrient[i] == FileInfoBase.ORI_I2S_TYPE) || (axisOrient[1] == FileInfoBase.ORI_UNKNOWN_TYPE)) {
                direct[i] = 1;
            }
            else {
                direct[i] = -1;
            }
        }
        if ((origin[0] == (short)0) && (origin[1] == (short)0) && 
           (origin[2] == (short)0)) {
            // Place mipav origin in center or image
            for (i = 0; i < numDims; i++) {
                mipavOrigin[i] = -direct[i]*((extents[i] - 1.0f)/2.0f)*resolutions[i];
            }    
        }
        else {
            //  Voxel 1,1,1 in SPM is voxel 0,0,0 in MIPAV
            for (i = 0; i < 5; i++) {
                mipavOrigin[i] = (short)(origin[i] - 1);
            }
            // The y axis is flipped so
            mipavOrigin[1] = extents[1] - 1 - mipavOrigin[1];
            for (i = 0; i < numDims; i++) {
                mipavOrigin[i] = -direct[i]*mipavOrigin[i]*resolutions[i];
            }
        }
        fileInfo.setMipavOrigin(mipavOrigin);
        fileInfo.setGenerated(new String(bufferByte, 263, 10));
        fileInfo.setScanNum(new String(bufferByte, 273, 10));
        fileInfo.setPatientID(new String(bufferByte, 283, 10));
        fileInfo.setExperimentDate(new String(bufferByte, 293, 10));
        fileInfo.setExperimentTime(new String(bufferByte, 303, 10));
        fileInfo.setHist(new String(bufferByte, 313, 3));

        fileInfo.setViews(getBufferInt(bufferByte, 316, endianess));
        fileInfo.setVolsAdded(getBufferInt(bufferByte, 320, endianess));
        fileInfo.setStartField(getBufferInt(bufferByte, 324, endianess));
        fileInfo.setFieldSkip(getBufferInt(bufferByte, 328, endianess));
        fileInfo.setOmax(getBufferInt(bufferByte, 332, endianess));
        fileInfo.setOmin(getBufferInt(bufferByte, 336, endianess));
        fileInfo.setSmax(getBufferInt(bufferByte, 340, endianess));
        fileInfo.setSmin(getBufferInt(bufferByte, 344, endianess));

        return true; // If it got this far, it has successfully read in the header
    }

    /**
     * Reads an SPM image file by reading the header then making a FileRaw to read the image for all filenames in the
     * file list. Only the one file directory (currently) supported.
     *
     * @param      one  flag indicating one image of a 3D dataset should be read in.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @return     The image.
     *
     * @see        FileRaw
     */
    public ModelImage readImage(boolean one) throws IOException, OutOfMemoryError {
        int index;
        String fileBaseName;
        String ext;
        index = fileName.lastIndexOf('.');
        fileBaseName = fileName.substring(0,index);
        ext = fileName.substring(index+1);
        headerFileName = fileBaseName + ".hdr";
        headerFile = new File(fileDir + headerFileName);

        if (!headerFile.exists()) {
            headerFileName = fileName.substring(0, index) + ".HDR";
            headerFile = new File(fileDir + headerFileName);

            if (!headerFile.exists()) {
                throw new IOException(headerFileName + " does not exist");
            }
        }
        if (!ext.equalsIgnoreCase("hdr")) {
            dataFileName = fileName;
            dataFile = new File(fileDir + dataFileName);
            if (!dataFile.exists()) {
                throw new IOException(dataFileName + " does not exist");
            }
        }
        else { // ext == hdr or HDR 
            dataFileName = fileBaseName + ".spm";
            dataFile = new File(fileDir + dataFileName);
            if (!dataFile.exists()) {
                dataFileName = fileBaseName + ".SPM";
                dataFile = new File(fileDir + dataFileName);
                if (!dataFile.exists()) {
                    dataFileName = fileBaseName + ".img";
                    dataFile = new File(fileDir + dataFileName);
                    if (!dataFile.exists()) {
                        dataFileName = fileBaseName + ".IMG";
                        dataFile = new File(fileDir + dataFileName);
                        if (!dataFile.exists()) { 
                            throw new IOException("dataFile with base name of " + fileBaseName + " does not exist");    
                        }
                    }
                }
            }
        }
        fileInfo = new FileInfoSPM(dataFileName, fileDir, FileUtility.SPM);

        if (!readHeader()) {
            throw (new IOException(" SPM header file error"));
        }

        int[] extents = null;

        try {

            if (one) {
                extents = new int[fileInfo.getExtents().length];

                for (int i = 0; i < extents.length; i++) {
                    extents[i] = fileInfo.getExtents()[i];
                }

                image = new ModelImage(fileInfo.getDataType(), new int[] { extents[0], extents[1] },
                                       fileInfo.getFileName());
            } else {
                //System.out.println("fileInfo.getFileName() = " + fileInfo.getFileName());
                image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName());
            }
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        // if vox units defines the units of measure, then use that instead
        updateUnitsOfMeasure(fileInfo, image);

        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);

            long offset = 0L;

            if (one) {

                if (fileInfo.getExtents().length > 2) {
                    offset = (long)getOffset(fileInfo);
                }
            }

            rawFile.readImage(image, offset);
            flipTopBottom(image);

            if (one) {
                fileInfo.setExtents(extents);
            }
        } catch (IOException error) {
            throw new IOException("FileSPM: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return image;
    }

    /**
     * Reads an SPM image file by reading the header then making a FileRaw to read the file. Image data is left in
     * buffer. If the fileInfo cannot be found, the header will be located and read first. Image is not 'flipped', and
     * neither units of measure nor orientation are set.
     *
     * @param      buffer  Image buffer to store image data into.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRaw
     */
    public void readImage(float[] buffer) throws IOException, OutOfMemoryError {

        if (fileInfo == null) { // if no file info yet, make it.
            fileInfo = new FileInfoSPM(fileName, fileDir, FileUtility.SPM);

            if (!readHeader()) {
                throw (new IOException("Cannot read image because of SPM header file error"));
            }
        }

        // if vox units defines the units of measure, then use that instead
        updateUnitsOfMeasure(fileInfo);

        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
            rawFile.readImage(buffer, 0L, fileInfo.getDataType());
            rawFile.raFile.close();
            flipTopBottom(buffer, fileInfo);
        } catch (IOException error) {
            throw new IOException("FileSPM: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return;
    }


    /**
     * Takes the image and sets it to SPM defaults, using the specified info.
     *
     * @param  image     Image to set to SPM defaults.
     * @param  fileInfo  File info structure to change.
     */
    public void reOrgInfo(ModelImage image, FileInfoSPM fileInfo) {
        int i;

        fileInfo.setOrientation(fileInfo.getOrientation());
        

        if (image.getNDims() == 2) {
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);

            for (i = 0; i < image.getExtents()[2]; i++) { // update all fileInfo
                image.setFileInfo(fileInfo, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 3);

            for (i = 0; i < (image.getExtents()[2] * image.getExtents()[3]); i++) { // update all fileInfo
                image.setFileInfo(fileInfo, i); // Set the array of fileInfos in ModelImage
            }
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @param   image     DOCUMENT ME!
     * @param   fileName  DOCUMENT ME!
     * @param   fileDir   DOCUMENT ME!
     * @param   options   DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void writeHeader3DTo2D(ModelImage image, String fileName, String fileDir, FileWriteOptions options)
            throws IOException {
        int k, seq;
        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();
        String origName = new String(fileName);

        for (k = beginSlice, seq = options.getStartNumber(); k <= endSlice; k++, seq++) {
            fileName = origName;

            if (options.getDigitNumber() == 1) {
                fileName += Integer.toString(seq);
            } else if (options.getDigitNumber() == 2) {

                if (seq < 10) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 3) {

                if (seq < 10) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 4) {

                if (seq < 10) {
                    fileName += "000" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 1000) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            }

            writeHeader(image, 1, 1, fileName, fileDir);

        } // end for loop


    }

    /**
     * DOCUMENT ME!
     *
     * @param   image     DOCUMENT ME!
     * @param   fileName  DOCUMENT ME!
     * @param   fileDir   DOCUMENT ME!
     * @param   options   DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void writeHeader4DTo3D(ModelImage image, String fileName, String fileDir, FileWriteOptions options)
            throws IOException {
        int k, seq;
        int beginTime = options.getBeginTime();
        int endTime = options.getEndTime();
        String origName = new String(fileName);

        for (k = beginTime, seq = options.getStartNumber(); k <= endTime; k++, seq++) {
            fileName = origName;

            if (options.getDigitNumber() == 1) {
                fileName += Integer.toString(seq);
            } else if (options.getDigitNumber() == 2) {

                if (seq < 10) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 3) {

                if (seq < 10) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 4) {

                if (seq < 10) {
                    fileName += "000" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 1000) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            }
            // write header with image, # of images per, and 1 time slice

            writeHeader(image, image.getExtents()[2], 1, fileName, fileDir);

        } // end for loop

    }

    /**
     * Writes an SPM format type image.
     *
     * @param      image  Image model of data to write.
     *
     * @exception  IOException  if there is an error writing the file
     *
     * @see        FileInfoSPM
     * @see        FileRaw
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        String fhName;
        int index;
        int nImagesSaved;
        int nTimePeriodsSaved;

        index = fileName.lastIndexOf(".");

        if (index != -1) {
            fhName = fileName.substring(0, index);
        } else {
            fhName = fileName.substring(0);
        }

        if (options.isMultiFile()) {
            FileRaw rawFile;
            rawFile = new FileRaw(image.getFileInfo(0));
            flipTopBottom(image);

            if (image.getNDims() == 3) {
                rawFile.writeImage3DTo2D(image, options, ".img");
                writeHeader3DTo2D(image, fhName, fileDir, options);
            } else if (image.getNDims() == 4) {
                rawFile.writeImage4DTo3D(image, options, ".img");
                writeHeader4DTo3D(image, fhName, fileDir, options);
            }

            flipTopBottom(image);
        } else {

            try {
                FileRaw rawFile;
                rawFile = new FileRaw(fileName, fileDir, image.getFileInfo(0), FileBase.READ_WRITE);
                flipTopBottom(image);
                rawFile.writeImage(image, options);
                nImagesSaved = rawFile.getNImages();
                nTimePeriodsSaved = rawFile.getNTimePeriods();

                if (nImagesSaved != 0) {
                    writeHeader(image, nImagesSaved, nTimePeriodsSaved, fhName, fileDir);
                }

                flipTopBottom(image);
            } catch (IOException error) {
                throw new IOException("FileSPMWrite: " + error);
            } catch (OutOfMemoryError error) {
                throw (error);
            }
        }

        // With extents from rawFile
    }


    /**
     * updates the units of Measure in the file info based on the voxUnits from an SPM Header. This version simply
     * updates a single FileInfo. It does not have an image to attach to.
     *
     * @param  fileInfo  -- an SPM file Info that has already been read
     */
    protected void updateUnitsOfMeasure(FileInfoSPM fileInfo) {

        // if vox units defines the units of measure, then use that instead
        int units = (Unit.getUnit(fileInfo.getVoxUnits())).getLegacyNum();

        if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
        } else {
            fileInfo.setUnitsOfMeasure(units, 0);
            fileInfo.setUnitsOfMeasure(units, 1);
        }

        // since we don't know the dimensionality of the image, just set
        // the first 2 dimensions.  (Also, there's no way to save the
        // other dimensions units since there's only a single field
        // in the SPM format for voxUnits.

    } // end updateUnitsOfMeasure()

    /**
     * updates the units of Measure in the file info based on the voxUnits from an SPM Header.
     *
     * @param  fileInfo  -- an SPM file Info that has already been read
     * @param  image     -- a ModelImage that the fileInfo needs to be attached to
     */
    protected void updateUnitsOfMeasure(FileInfoSPM fileInfo, ModelImage image) {

        int[] extents = fileInfo.getExtents();

        // if vox units defines the units of measure, then use that instead
        int units = (Unit.getUnit(fileInfo.getVoxUnits())).getLegacyNum();

        if (image.getNDims() == 2) {

            if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            } else {
                fileInfo.setUnitsOfMeasure(units, 0);
                fileInfo.setUnitsOfMeasure(units, 1);
            }

            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image

            if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
            } else {
                fileInfo.setUnitsOfMeasure(units, 0);
                fileInfo.setUnitsOfMeasure(units, 1);
                fileInfo.setUnitsOfMeasure(units, 2);
            }

            for (int i = 0; i < extents[2]; i++) {
                FileInfoSPM newFileInfo = (FileInfoSPM) fileInfo.clone();
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image

            if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 3);
            } else {
                fileInfo.setUnitsOfMeasure(units, 0);
                fileInfo.setUnitsOfMeasure(units, 1);
                fileInfo.setUnitsOfMeasure(units, 2);
                fileInfo.setUnitsOfMeasure(units, 3);
            }

            for (int i = 0; i < (extents[2] * extents[3]); i++) {
                FileInfoSPM newFileInfo = (FileInfoSPM) fileInfo.clone();
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        }

    } // end updateUnitsOfMeasure()

    /**
     * Helper method to calculate the offset for getting only the middle SPM image slice from the 3D file.
     *
     * @param   fileInfo  File info.
     *
     * @return  offset
     */
    private int getOffset(FileInfoSPM fileInfo) {
        int offset = fileInfo.getExtents()[0] * fileInfo.getExtents()[1] * (fileInfo.getExtents()[2] / 2);

        switch (fileInfo.getDataType()) {

            case ModelStorageBase.BOOLEAN:
            case ModelStorageBase.BYTE:
            case ModelStorageBase.UBYTE:
                break;

            case ModelStorageBase.SHORT:
            case ModelStorageBase.USHORT:
                offset *= 2;
                break;

            case ModelStorageBase.FLOAT:
            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
                offset *= 4;
                break;

            case ModelStorageBase.LONG:
            case ModelStorageBase.DOUBLE:
                offset *= 8;
                break;

            case ModelStorageBase.ARGB:
                offset *= 3;
                break;

            case ModelStorageBase.ARGB_USHORT:
                offset *= 6;
                break;
        }

        return offset;
    }

    /**
     * Writes an SPM header to a separate file.
     *
     * @param      image     Image model of data to write.
     * @param      fileName  File name.
     * @param      fileDir   File directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error
     *
     * @see        FileInfoSPM
     */
    private boolean writeHeader(ModelImage image, int nImagesSaved, int nTimeSaved, String fileName, String fileDir)
            throws IOException {

        boolean simple = false; // A simple write only writes

        // absolutely neccessary information
        boolean endianess;
        int nDims;
        int[] extents;
        float[] resolutions;
        FileInfoBase myFileInfo;
        int[] spmExtents;
        short[] origin;

        myFileInfo = image.getFileInfo(0); // A safeguard in case the file is not SPM
        endianess = myFileInfo.getEndianess();

        try { // In this case, the file must be SPM
            fileInfo = (FileInfoSPM) image.getFileInfo(0);
        } catch (ClassCastException e) { // If it isn't, catch the exception

            // and make a new fileInfo
            fileInfo = new FileInfoSPM(fileName, fileDir, FileUtility.SPM);
            simple = true; // Write the header without all the SPM info
        }

        headerFileName = fileName + ".hdr";
        headerFile = new File(fileDir + headerFileName);
        raFile = new RandomAccessFile(headerFile, "rw");
        raFile.setLength(0);
        bufferByte = new byte[headerSize];

        // Set certain neccessary information
        fileInfo.setSizeOfHeader(headerSize);
        fileInfo.setDBname(fileName);
        fileInfo.setFileExtents(16384);
        fileInfo.setRegular('r');

        extents = myFileInfo.getExtents();

        // nDims   = image.getNDims();
        // if (nImagesSaved == 1 && nDims != 2) nDims--;
        nDims = 4; // SPM always expects 4D !!!!!!!!!.

        // 2D example = 256 x 256 x  1 x 1
        // 3D example = 256 x 256 x 17 x 1

        Preferences.debug("FileSPM:writeHeader - nImagesSaved = " + nImagesSaved + "\n", Preferences.DEBUG_FILEIO);
        Preferences.debug("FileSPM:writeHeader - nDims = " + nDims + "\n", Preferences.DEBUG_FILEIO);

        // spmExtents = new int[nDims+2];
        // spmExtents[0] = nDims+1;
        spmExtents = new int[] { 1, 1, 1, 1, 1 };
        spmExtents[0] = 4;

        for (int i = 1; i <= nDims; i++) {

            if (i == 3) {
                spmExtents[i] = nImagesSaved;
            } else if (i == 4) {
                spmExtents[i] = nTimeSaved;
            } else {
                spmExtents[i] = extents[i - 1];
            }
        }
        // spmExtents[nDims+1] = 1; // set the last dimension to one

        for (int i = 0; i < spmExtents.length; i++) {
            Preferences.debug("FileSPM:writeHeader - i = " + i + " dim = " + spmExtents[i] + "\n", Preferences.DEBUG_FILEIO);
        }

        switch (image.getType()) {

            case ModelStorageBase.BOOLEAN:
                fileInfo.setDataType((short) DT_BINARY);
                break;

            case ModelStorageBase.BYTE:
                fileInfo.setDataType((short) DT_BYTE);
                break;

            case ModelStorageBase.UBYTE:
                fileInfo.setDataType((short) DT_UNSIGNED_CHAR);
                break;

            case ModelStorageBase.SHORT:
                fileInfo.setDataType((short) DT_SIGNED_SHORT);
                break;

            case ModelStorageBase.USHORT:
                fileInfo.setDataType((short) DT_UNSIGNED_SHORT);
                break;

            case ModelStorageBase.INTEGER:
                fileInfo.setDataType((short) DT_SIGNED_INT);
                break;

            case ModelStorageBase.UINTEGER:
                fileInfo.setDataType((short) DT_UNSIGNED_INT);
                break;

            case ModelStorageBase.LONG:
                return false;

            case ModelStorageBase.FLOAT:
                fileInfo.setDataType((short) DT_FLOAT);
                break;

            case ModelStorageBase.DOUBLE:
                fileInfo.setDataType((short) DT_DOUBLE);
                break;

            case ModelStorageBase.ARGB: // only RGB for SPM images
                fileInfo.setDataType((short) DT_RGB);
                fileInfo.setBitPix((short) 24);
                break;

            default:
                return false;
        }

        // fileInfo.glmax = (int)image.getMax();
        // fileInfo.glmin = (int)image.getMin();

        if (!simple) { // Must be an SPM file, can set all SPM information based on fileInfo
            setBufferInt(bufferByte, fileInfo.getSizeOfHeader(), 0, endianess);
            setBufferString(bufferByte, fileInfo.getDataTypeName(), 4);
            setBufferString(bufferByte, fileName, 14);
            setBufferInt(bufferByte, fileInfo.getFileExtents(), 32, endianess);
            setBufferShort(bufferByte, fileInfo.getSessionErr(), 36, endianess);
            bufferByte[38] = (byte) 'r';
            bufferByte[39] = 0;

            for (int i = 0; i < spmExtents.length; i++) {
                setBufferShort(bufferByte, (short) spmExtents[i], 40 + (i * 2), endianess);
            }

            // make sure that VoxUnits has been updated to match the unitsOfMeasure
            // in FileInfoBase.  Assume that this is the unit of measure for the x and
            // y dimensions.
            fileInfo.setVoxUnits((Unit.getUnitFromLegacyNum(fileInfo.getUnitsOfMeasure(0))).getAbbrev());
            setBufferString(bufferByte, fileInfo.getVoxUnits(), 56);
            setBufferString(bufferByte, fileInfo.getCalUnits(), 60);

            setBufferShort(bufferByte, fileInfo.getDataTypeCode(), 70, endianess);
            setBufferShort(bufferByte, fileInfo.getBitPix(), 72, endianess);
            setBufferShort(bufferByte, fileInfo.getDim(), 74, endianess);

            // setBufferFloat(bufferByte, nDims+1, 76 + i*4, endianess);

            nDims = image.getNDims();

            for (int i = 0; i < nDims; i++) {
                setBufferFloat(bufferByte, fileInfo.getResolutions()[i], 80 + (i * 4), endianess);
            }

            setBufferFloat(bufferByte, fileInfo.getVoxOffset(), 108, endianess);
            setBufferFloat(bufferByte, fileInfo.getScale(), 112, endianess);

            setBufferFloat(bufferByte, fileInfo.getCalMax(), 124, endianess);
            setBufferFloat(bufferByte, fileInfo.getCalMin(), 128, endianess);
            setBufferFloat(bufferByte, fileInfo.getCompressed(), 132, endianess);
            setBufferFloat(bufferByte, fileInfo.getVerified(), 136, endianess);

            setBufferInt(bufferByte, fileInfo.getGLmax(), 140, endianess);
            setBufferInt(bufferByte, fileInfo.getGLmin(), 144, endianess);

            // make sure that description has been updated to match the modality
            // in FileInfoBase. If the modality is unknown, then leave description alone.
            int modality = fileInfo.getModality();

            if (modality != FileInfoBase.UNKNOWN_MODALITY) {
                fileInfo.setDescription(FileInfoBase.getModalityStr(modality));
            }

            setBufferString(bufferByte, fileInfo.getDescription(), 148);
            setBufferString(bufferByte, fileInfo.getAuxFile(), 228);
            bufferByte[252] = fileInfo.getOrientation();
            origin = fileInfo.getOriginLoc();

            for (int i = 0; i < 5; i++) {
                setBufferShort(bufferByte, origin[i], 253 + (2 * i), endianess);
            }

            setBufferString(bufferByte, fileInfo.getGenerated(), 263);
            setBufferString(bufferByte, fileInfo.getScanNum(), 273);
            setBufferString(bufferByte, fileInfo.getPatientID(), 283);
            setBufferString(bufferByte, fileInfo.getExperimentDate(), 293);
            setBufferString(bufferByte, fileInfo.getExperimentTime(), 303);

            setBufferString(bufferByte, fileInfo.getHist(), 313);
            setBufferInt(bufferByte, fileInfo.getViews(), 316, endianess);
            setBufferInt(bufferByte, fileInfo.getVolsAdded(), 320, endianess);
            setBufferInt(bufferByte, fileInfo.getStartField(), 324, endianess);
            setBufferInt(bufferByte, fileInfo.getFieldSkip(), 328, endianess);
            setBufferInt(bufferByte, fileInfo.getOmax(), 332, endianess);
            setBufferInt(bufferByte, fileInfo.getOmin(), 336, endianess);
            setBufferInt(bufferByte, fileInfo.getSmax(), 340, endianess);
            setBufferInt(bufferByte, fileInfo.getSmin(), 344, endianess);
        } else { // Not an SPM file.  Pad the header with blanks and set all known info
            resolutions = myFileInfo.getResolutions();

            if (resolutions == null) {
                nDims = image.getNDims();
                resolutions = new float[nDims];

                for (int i = 0; i < nDims; i++) {
                    resolutions[i] = 1.0f;
                }
            }

            fileInfo.setResolutions(resolutions);
            fileInfo.setEndianess(endianess);

            setBufferInt(bufferByte, fileInfo.getSizeOfHeader(), 0, endianess);
            setBufferString(bufferByte, "         \n", 4);
            setBufferString(bufferByte, fileName + "\n", 14);
            setBufferInt(bufferByte, fileInfo.getFileExtents(), 32, endianess);
            setBufferShort(bufferByte, (short) 0, 36, endianess);
            bufferByte[38] = (byte) 'r';
            bufferByte[39] = 0;

            for (int i = 0; i < spmExtents.length; i++) {
                setBufferShort(bufferByte, (short) spmExtents[i], 40 + (i * 2), endianess);
            }

            // set the voxUnits based on the Units of Measure
            int[] units = myFileInfo.getUnitsOfMeasure();
            String voxUnits = (Unit.getUnitFromLegacyNum(units[0])).getAbbrev();
            fileInfo.setUnitsOfMeasure(units);
            fileInfo.setVoxUnits(voxUnits);
            setBufferString(bufferByte, voxUnits, 56);
            setBufferString(bufferByte, "   \n", 60);

            Preferences.debug("FileSPM:writeHeader(simple): data type = " + fileInfo.getDataTypeCode() + "\n",
            		Preferences.DEBUG_FILEIO);
            setBufferShort(bufferByte, fileInfo.getDataTypeCode(), 70, endianess);

            switch (image.getType()) {

                case ModelStorageBase.BOOLEAN:
                    fileInfo.setBitPix((short) 1);
                    break;

                case ModelStorageBase.BYTE:
                    fileInfo.setBitPix((short) 8);
                    break;

                case ModelStorageBase.UBYTE:
                    fileInfo.setBitPix((short) 8);
                    break;

                case ModelStorageBase.SHORT:
                    fileInfo.setBitPix((short) 16);
                    break;

                case ModelStorageBase.USHORT:
                    fileInfo.setBitPix((short) 16);
                    break;

                case ModelStorageBase.INTEGER:
                    fileInfo.setBitPix((short) 32);
                    break;

                case ModelStorageBase.UINTEGER:
                    fileInfo.setBitPix((short) 32);
                    break;

                case ModelStorageBase.LONG:
                    return false;

                case ModelStorageBase.FLOAT:
                    fileInfo.setBitPix((short) 32);
                    break;

                case ModelStorageBase.DOUBLE:
                    fileInfo.setBitPix((short) 64);
                    break;

                case ModelStorageBase.ARGB: // only RGB for SPM images
                    fileInfo.setBitPix((short) 24);
                    break;

                default:
                    return false;
            }

            Preferences.debug("FileSPM:writeHeader(simple): bits per pixel = " + fileInfo.getBitPix() + "\n",
            		Preferences.DEBUG_FILEIO);
            setBufferShort(bufferByte, (short) fileInfo.getBitPix(), 72, endianess);
            setBufferShort(bufferByte, (short) 0, 74, endianess);

            nDims = fileInfo.getResolutions().length;

            for (int i = 0; i < nDims; i++) {
                setBufferFloat(bufferByte, fileInfo.getResolutions()[i], 80 + (i * 4), endianess);
            }

            setBufferFloat(bufferByte, (float) 0, 108, endianess);
            setBufferFloat(bufferByte, fileInfo.getScale(), 112, endianess);

            setBufferFloat(bufferByte, (float) 0, 124, endianess);
            setBufferFloat(bufferByte, (float) 0, 128, endianess);
            setBufferFloat(bufferByte, (float) 0, 132, endianess);
            setBufferFloat(bufferByte, (float) 0, 136, endianess);

            setBufferInt(bufferByte, (int) image.getMax(), 140, endianess);
            setBufferInt(bufferByte, (int) image.getMin(), 144, endianess);

            int modality = myFileInfo.getModality();
            fileInfo.setModality(modality);
            setBufferString(bufferByte, FileInfoBase.getModalityStr(modality), 148);

            setBufferString(bufferByte, " ", 228);
            
            byte tmpByte;
            int axis[] = myFileInfo.getAxisOrientation();
            int imageOrientation = myFileInfo.getImageOrientation();
            // Remember y axis gets flipped
            if ((axis[0] == FileInfoBase.ORI_R2L_TYPE) && (axis[1] == FileInfoBase.ORI_P2A_TYPE) &&
                (axis[2] == FileInfoBase.ORI_I2S_TYPE)) {
                tmpByte = 3; // transverse flipped
            }
            else if ((axis[0] == FileInfoBase.ORI_R2L_TYPE) && (axis[1] == FileInfoBase.ORI_I2S_TYPE) &&
                     (axis[2] == FileInfoBase.ORI_P2A_TYPE)) {
                tmpByte = 4; // coronal flipped
            }
            else if ((axis[0] == FileInfoBase.ORI_P2A_TYPE) && (axis[1] == FileInfoBase.ORI_I2S_TYPE) &&
                     (axis[2] == FileInfoBase.ORI_R2L_TYPE)) {
                tmpByte = 5; // sagittal flipped
            }
            else if ((axis[0] == FileInfoBase.ORI_R2L_TYPE) && (axis[1] == FileInfoBase.ORI_A2P_TYPE) &&
                     (axis[2] == FileInfoBase.ORI_I2S_TYPE)) {
                tmpByte = 0; // transverse unflipped
            }
            else if ((axis[0] == FileInfoBase.ORI_R2L_TYPE) && (axis[1] == FileInfoBase.ORI_S2I_TYPE) &&
                     (axis[2] == FileInfoBase.ORI_P2A_TYPE)) {
                tmpByte = 1; // coronal unflipped
            }
            else if ((axis[0] == FileInfoBase.ORI_P2A_TYPE) && (axis[1] == FileInfoBase.ORI_S2I_TYPE) &&
                     (axis[2] == FileInfoBase.ORI_R2L_TYPE)) {
                tmpByte = 2; // sagittal unflipped
            }
            else if (imageOrientation == FileInfoBase.AXIAL) {
                tmpByte = 0; // transverse unflipped
            }
            else if (imageOrientation == FileInfoBase.CORONAL) {
                tmpByte = 1; // coronal unflipped
            }
            else if (imageOrientation == FileInfoBase.SAGITTAL) {
                tmpByte = 2; // sagittal unflipped
            } 
            else {
                tmpByte = 0; // transverse unflipped
            }
            

            bufferByte[252] = tmpByte;
            float mipavOrigin[] = myFileInfo.getOrigin();
            float tempOrigin[] = new float[mipavOrigin.length];
            origin = new short[] { 0, 0, 0, 0, 0 };
            for (int i = 0; i < mipavOrigin.length; i++) {
                if (mipavOrigin[i] == 0.0f) {
                    tempOrigin[i] = 0.0f;
                }
                else if (mipavOrigin[i] > 0.0f) {
                    // mipavOrigin[i] - resolutions[i] * origin[i] = 0;
                    tempOrigin[i] = mipavOrigin[i]/resolutions[i];
                }
                else { // mipavOrigin[i] < 0.0f
                    // mipavOrigin + resolutions[i] * origin[i] = 0;
                    tempOrigin[i] = -mipavOrigin[i]/resolutions[i];
                }
            }
            if (((image.getNDims() == 2) && (Math.abs(tempOrigin[0] - ((extents[0] - 1.0f)/2.0f)) <= 0.5f) && 
                    (Math.abs(tempOrigin[1] - ((extents[1] - 1.0f)/2.0f)) <= 0.5f)) ||
                    ((image.getNDims() == 3) && (Math.abs(tempOrigin[0] - ((extents[0] - 1.0f)/2.0f)) <= 0.5f) && 
                            (Math.abs(tempOrigin[1] - ((extents[1] - 1.0f)/2.0f)) <= 0.5f) &&
                            (Math.abs(tempOrigin[2] - ((extents[2] - 1.0f)/2.0f)) <= 0.5f))) {
                // within 1/2 pixel of image center for every dimension, so assume image center is intended and
                // leave origin at 0, 0, 0, to designate zero values at image center
            }
            else { // not image center
                for (int i = 0; i < mipavOrigin.length; i++) {
                    origin[i] = (short)Math.round(tempOrigin[i]);
                }
                // The y axis if flipped so 
                origin[1] = (short)(spmExtents[1] - 1 - origin[1]);
                // Voxel 0,0,0 in MIPAV is voxel 1,1,1 in SPM
                for (int i = 0; i < 5; i++) {
                    origin[i] = (short)(origin[i] + 1);
                }
            }
            
            setBufferShort(bufferByte, origin[0], 253, endianess);
            setBufferShort(bufferByte, origin[1], 255, endianess);
            setBufferShort(bufferByte, origin[2], 257, endianess);
            setBufferShort(bufferByte, origin[3], 259, endianess);
            setBufferShort(bufferByte, origin[4], 261, endianess);
            setBufferString(bufferByte, " ", 263);
            setBufferString(bufferByte, " ", 273);
            setBufferString(bufferByte, " ", 283);
            setBufferString(bufferByte, " ", 293);
            setBufferString(bufferByte, " ", 303);

            setBufferString(bufferByte, " ", 313);
            setBufferInt(bufferByte, 0, 316, endianess);
            setBufferInt(bufferByte, 0, 320, endianess);
            setBufferInt(bufferByte, 0, 324, endianess);
            setBufferInt(bufferByte, 0, 328, endianess);
            setBufferInt(bufferByte, 0, 332, endianess);
            setBufferInt(bufferByte, 0, 336, endianess);
            setBufferInt(bufferByte, 0, 340, endianess);
            setBufferInt(bufferByte, 0, 344, endianess);
        }

        raFile.write(bufferByte);
        raFile.close();

        return true; // Successful write
    }

}
