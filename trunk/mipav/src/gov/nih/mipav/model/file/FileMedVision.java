package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;


/**
 * The class reads MedVision files that have been saved as MAC bin files. The MAC files have needed image resolutions
 * encoded in the in the resource section at the end of the file. MedVision also has method of saving images to the
 * Intel world but pixel resolutions are not stored. Here I decode MAC MedVision files. It is easy to write a Intel
 * MedVision reader or one could directly read in the file using using MIPA because they have a fixed offset and can be
 * read in using the RAW file reader.
 *
 * @version  0.1 June 25, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileIO
 * @see      FileInfoMedVision
 * @see      FileRaw
 * @see      FileRawChunk
 */


public class FileMedVision extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final short BIT8 = 0x0001;

    /** DOCUMENT ME! */
    public static final short BIT16 = 0x0002;

    /** DOCUMENT ME! */
    public static final short SIGNED = 0x1000;

    /** DOCUMENT ME! */
    public static final short UNSIGNED = 0x2000;

    /** DOCUMENT ME! */
    public static final short S8BIT = 0x1001;

    /** DOCUMENT ME! */
    public static final short U8BIT = 0x2001;

    /** DOCUMENT ME! */
    public static final short S16BIT = 0x1002;

    /** DOCUMENT ME! */
    public static final short U16BIT = 0x2002;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    //private boolean endianess = BIG_ENDIAN;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoMedVision fileInfo;

    /** DOCUMENT ME! */
    private FileInfoMedVision fileInfoCom;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private ViewJProgressBar progressBar;

    /** DOCUMENT ME! */
    private float[] resolutions;

    /** DOCUMENT ME! */
    private double[] slicePosition;

    /** DOCUMENT ME! */
    private double[][] sliceResolutions;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileMedVision - MedVision reader/writer constructor.
     *
     * @param  fName  file name
     * @param  fDir   file directory
     */
    public FileMedVision(String fName, String fDir) {
        fileName = fName;
        fileDir = fDir;

        if (true) {
            progressBar = new ViewJProgressBar("Loading MedVision image", "Loading Image " + fileName, 0, 100, false,
                                               null, null);
            progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50);


        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * readImage - reads a MedVision file in MAC format (i.e., decodes resource info).
     *
     * @return     image model of the data read in from the file
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRawChunk
     */
    public ModelImage readImage() throws IOException {
        int i;
        int[] extents;
        FileRawChunk reader;
        boolean endianess;

        fileInfoCom = new FileInfoMedVision(fileName, fileDir, FileUtility.MEDIVISION);
        file = new File(fileDir + fileName);

        try {
            raFile = new RandomAccessFile(file, "rw");
        } catch (IOException e) {
            raFile = new RandomAccessFile(file, "r");
        }

        fileInfoCom.setEndianess(BIG_ENDIAN);
        endianess = fileInfoCom.getEndianess();

        // decodes file header that describes info for entire image set
        if (!decodeFileHeader(endianess)) {
            throw (new IOException(" MedVision header file error"));
        }

        // decodes first image header that decribes first image of the dataset
        if (decodeSliceHeader(raFile.getFilePointer(), fileInfoCom, endianess) == false) {
            throw (new IOException(" MedVision slice header file error"));
        }

        // fileInfoCom.dataType = S16BIT;
        if (fileInfoCom.dataType == S8BIT) {
            fileInfoCom.setDataType(ModelStorageBase.BYTE);
            extents = new int[2];
            extents[0] = fileInfoCom.standardRows;
            extents[1] = fileInfoCom.standardCols;
            fileInfoCom.setExtents(extents);
            sliceResolutions = new double[1][2];
            slicePosition = new double[1];
            resolutions = new float[2];

        } else if (fileInfoCom.dataType == U8BIT) {
            fileInfoCom.setDataType(ModelStorageBase.UBYTE);
            extents = new int[2];
            extents[0] = fileInfoCom.standardRows;
            extents[1] = fileInfoCom.standardCols;
            fileInfoCom.setExtents(extents);
            sliceResolutions = new double[1][2];
            slicePosition = new double[1];
            resolutions = new float[2];
        } else if (fileInfoCom.dataType == S16BIT) {
            fileInfoCom.setDataType(ModelStorageBase.SHORT);
            extents = new int[3];
            extents[0] = fileInfoCom.standardRows;
            extents[1] = fileInfoCom.standardCols;
            extents[2] = fileInfoCom.totalSlices;
            fileInfoCom.setExtents(extents);
            sliceResolutions = new double[fileInfoCom.totalSlices][3];
            slicePosition = new double[fileInfoCom.totalSlices];
            resolutions = new float[3];
        } else if (fileInfoCom.dataType == U16BIT) {
            fileInfoCom.setDataType(ModelStorageBase.USHORT);
            extents = new int[3];
            extents[0] = fileInfoCom.standardRows;
            extents[1] = fileInfoCom.standardCols;
            extents[2] = fileInfoCom.totalSlices;
            fileInfoCom.setExtents(extents);
            sliceResolutions = new double[fileInfoCom.totalSlices][3];
            slicePosition = new double[fileInfoCom.totalSlices];
            resolutions = new float[3];
        } else {
            throw (new IOException(" MedVision type file error"));
        }

        // decode file footer where MAC resource info is "hidden"
        decodeResourceFooter(endianess);

        try {
            image = new ModelImage(fileInfoCom.getDataType(), fileInfoCom.getExtents(), fileInfoCom.getFileName());
            reader = new FileRawChunk(raFile, fileInfoCom);
        } catch (OutOfMemoryError error) {
            throw error;
        }

        raFile.seek(128 + 24); // Length of resource +  file header

        // Read in all slices
        for (i = 0; i < fileInfoCom.totalSlices; i++) {

            fireProgressStateChanged(Math.round((float) i / (fileInfoCom.totalSlices - 1) * 100));
            progressBar.setTitle(ViewUserInterface.getReference().getProgressBarPrefix() + "image " + i);
            fileInfo = new FileInfoMedVision(fileName, fileDir, FileUtility.MEDIVISION);
            copyFileInfoCommon();

            // Read image header for each slice
            if (decodeSliceHeader(raFile.getFilePointer(), fileInfo, endianess) == false) {
                throw (new IOException(" MedVision slice header file error"));
            }

            image.setFileInfo(fileInfo, i); // Set file info for each slice

            // Use raw file chunk reader to read image slice
            try {
                reader.readImage(fileInfo.getDataType(), (int) raFile.getFilePointer(), extents[0] * extents[1]);
            } catch (IOException error) {
                throw new IOException("FileMedVision: " + error);
            }

            // Put the data into the image model
            if ((fileInfo.dataType == S8BIT) || (fileInfo.dataType == U8BIT)) {
                image.importData(i * extents[0] * extents[1], reader.getByteBuffer(), false);
            } else if ((fileInfo.dataType == S16BIT) || (fileInfo.dataType == U16BIT)) {
                image.importData(i * extents[0] * extents[1], reader.getShortBuffer(), false);
            } else {
                throw (new IOException(" MedVision type file error"));
            }


            // store all important resolutions in fileInfo base and MedVision Info object
            fileInfo.pixelSizeH = sliceResolutions[i][0];
            fileInfo.pixelSizeV = sliceResolutions[i][1];
            fileInfo.sliceThickness = sliceResolutions[i][2];
            fileInfo.slicePosition = slicePosition[i];

            resolutions = new float[3];
            resolutions[0] = (float) sliceResolutions[i][0];
            resolutions[1] = (float) sliceResolutions[i][1];
            resolutions[2] = (float) sliceResolutions[i][2];
            fileInfo.setResolutions(resolutions);
        }

        raFile.close();
        reader.close();


        return image;
    }


    /**
     * copyFileInfoCommon - copy common file info into new header Better must exist but this works.
     */
    private void copyFileInfoCommon() {

        fileInfo.patientName = fileInfoCom.patientName;
        fileInfo.institution = fileInfoCom.institution;
        fileInfo.studyDate = fileInfoCom.studyDate;
        fileInfo.studyTime = fileInfoCom.studyTime;
        fileInfo.studyName = fileInfoCom.studyName;
        fileInfo.dateOfBirth = fileInfoCom.dateOfBirth;
        fileInfo.sex = fileInfoCom.sex;
        fileInfo.modality = fileInfoCom.modality;
        fileInfo.eqManuf = fileInfoCom.eqManuf;
        fileInfo.eqType = fileInfoCom.eqType;
        fileInfo.referringPhys = fileInfoCom.referringPhys;
        fileInfo.cineRate = fileInfoCom.cineRate;

        fileInfo.fileType = fileInfoCom.fileType;
        fileInfo.version = fileInfoCom.version;
        fileInfo.imageOffset = fileInfoCom.imageOffset;
        fileInfo.totalSlices = fileInfoCom.totalSlices;
        fileInfo.sliceHdrSize = fileInfoCom.sliceHdrSize;
        fileInfo.standardRows = fileInfoCom.standardRows;
        fileInfo.standardCols = fileInfoCom.standardCols;

        fileInfo.setExtents(fileInfoCom.getExtents());
        fileInfo.setDataType(fileInfoCom.getDataType());
        fileInfo.setEndianess(BIG_ENDIAN);
        fileInfo.setMin(fileInfoCom.getMin());
        fileInfo.setMax(fileInfoCom.getMax());
    }

    /**
     * readFileHeader - reader image file header that encodes info about image.
     *
     * @param      endianess  boolean describing byte order
     *
     * @return     boolean indicating successful read
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileInfoMedVision
     */
    private boolean decodeFileHeader(boolean endianess) throws IOException {

        raFile.seek(128); // First 128 bytes MAC binary junk (resource fork)

        fileInfoCom.fileType = getString(8);
        fileInfoCom.version = getInt(endianess);
        fileInfoCom.imageOffset = getInt(endianess);
        fileInfoCom.totalSlices = (short) getUnsignedShort(endianess);
        fileInfoCom.sliceHdrSize = (short) getUnsignedShort(endianess);
        fileInfoCom.standardRows = (short) getUnsignedShort(endianess);
        fileInfoCom.standardCols = (short) getUnsignedShort(endianess);

        return true;
    }

    /**
     * decodeResourceFooter - This is the tricky function that decodes the the resource footer at the end of the MAC
     * file The data is coded is very odd manner. I was unable to find MAC documentation and decoded it by "hand"
     *
     * @param      endianess  boolean describing byte order
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void decodeResourceFooter(boolean endianess) throws IOException {

        String str = new String();
        int sLength;
        byte[] sBuffer;
        int endData, pad;

        raFile.seek(83); // location in MAC resource fork of Data fork length
        endData = getInt(endianess) + 128; // Get length add front resource length
        pad = 128 - (endData % 128); // Calc padding
        endData = endData + pad + 256; // Add pad and beginning resource length
        raFile.seek(endData + 4);


        // Read patients name
        sLength = raFile.readUnsignedByte();

        if (sLength < 34) {
            sBuffer = new byte[sLength];
            raFile.readFully(sBuffer);
            str = new String(sBuffer);
            fileInfoCom.patientName = str;
        }

        // Read institution name
        raFile.seek(endData + 4 + 34);
        sLength = raFile.readUnsignedByte();

        if (sLength < 34) {
            sBuffer = new byte[sLength];
            raFile.readFully(sBuffer);
            str = new String(sBuffer);
            fileInfoCom.institution = str;
        }

        // Read study date
        raFile.seek(endData + 4 + (2 * 34));
        sBuffer = new byte[6];
        raFile.readFully(sBuffer);

        if ((sBuffer[0] >= 0x30) && (sBuffer[0] < 0x40)) {
            str = new String(sBuffer);
            fileInfoCom.studyDate = str;
        }

        // Read study name
        raFile.seek(endData + 4 + (2 * 34) + 12);
        sLength = raFile.readUnsignedByte();

        if (sLength < 34) {
            sBuffer = new byte[sLength];
            raFile.readFully(sBuffer);
            str = new String(sBuffer);
            fileInfoCom.studyName = str;
        }

        // Read date of birth
        raFile.seek(endData + 4 + (3 * 34) + 12);
        sBuffer = new byte[6];
        raFile.readFully(sBuffer);

        if ((sBuffer[0] >= 0x30) && (sBuffer[0] < 0x40)) {
            str = new String(sBuffer);
            fileInfoCom.dateOfBirth = str;
        }


        // Read sex
        raFile.seek(endData + 4 + (3 * 34) + 18);
        sBuffer = new byte[2];
        raFile.readFully(sBuffer);
        str = new String(sBuffer);
        fileInfoCom.sex = str;

        // Read image modality
        raFile.seek(endData + 4 + (3 * 34) + 20);
        sLength = raFile.readUnsignedByte();

        if (sLength < 12) {
            sBuffer = new byte[sLength];
            raFile.readFully(sBuffer);
            str = new String(sBuffer);
            fileInfoCom.modality = str;
        }

        // Read equipment manufacturer
        raFile.seek(endData + 4 + (3 * 34) + 20 + 12);
        sLength = raFile.readUnsignedByte();

        if (sLength < 34) {
            sBuffer = new byte[sLength];
            raFile.readFully(sBuffer);
            str = new String(sBuffer);
            fileInfoCom.eqManuf = str;
        }

        // Read equipment type
        raFile.seek(endData + 4 + (4 * 34) + 20 + 12);
        sLength = raFile.readUnsignedByte();

        if (sLength < 34) {
            sBuffer = new byte[sLength];
            raFile.readFully(sBuffer);
            str = new String(sBuffer);
            fileInfoCom.eqType = str;
        }

        // Read referring physicians
        raFile.seek(endData + 4 + (5 * 34) + 20 + 12);
        sLength = raFile.readUnsignedByte();

        if (sLength < 34) {
            sBuffer = new byte[sLength];
            raFile.readFully(sBuffer);
            str = new String(sBuffer);
            fileInfoCom.referringPhys = str;
        }

        // image resolutions encoded is an extremely strange manner - almost encripted
        raFile.seek(endData + 4 + (6 * 34) + 20 + 12 + 16);

        int mark = endData + 4 + (6 * 34) + 20 + 12 + 16;

        for (int i = 0; i < fileInfoCom.totalSlices; i++) {
            raFile.seek(mark + 4 + (i * 80));
            sliceResolutions[i][0] = getMedVisionDouble(endianess);
            raFile.seek(mark + 4 + (i * 80) + 12);
            sliceResolutions[i][1] = getMedVisionDouble(endianess);
            raFile.seek(mark + 4 + (i * 80) + 24);
            sliceResolutions[i][2] = getMedVisionDouble(endianess);
            raFile.seek(mark + 4 + (i * 80) + 36);
            slicePosition[i] = getMedVisionDouble(endianess);
        }
    }

    /**
     * decodeSliceHeader - reads image slice header that encodes info about specific image slice.
     *
     * @param      start      start of file information
     * @param      fileInfo   file information
     * @param      endianess  boolean describing byte order
     *
     * @return     boolean indicating successful decoding
     *
     * @exception  IOException  if there is an error reading the file
     */
    private boolean decodeSliceHeader(long start, FileInfoMedVision fileInfo, boolean endianess) throws IOException {
        short tmp;

        raFile.seek(start);

        fileInfo.rows = (short) getUnsignedShort(endianess);

        if (fileInfo.rows != fileInfo.standardRows) {
            return false;
        }

        fileInfo.cols = (short) getUnsignedShort(endianess);

        if (fileInfo.cols != fileInfo.standardCols) {
            return false;
        }

        fileInfo.dataType = (short) getUnsignedShort(endianess);
        fileInfo.frameRefNum = (short) getUnsignedShort(endianess);

        tmp = (short) getUnsignedShort(endianess);
        fileInfo.sliceRefNum = tmp;

        return true;
    }

    /**
     * getMedVisionDouble - extracts double value from MedVision MAC resource fork.
     *
     * @param      endianess  describes byte ordering of data
     *
     * @return     returns extracted double
     *
     * @exception  IOException  if there is an error reading the file
     */
    private double getMedVisionDouble(boolean endianess) throws IOException {
        double tmp = 0;
        long tmpLong;
        int b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0, b7 = 0, b8 = 0;
        int tmpInt;

        b1 = raFile.readUnsignedByte();
        b2 = raFile.readUnsignedByte();
        b2 = (b2 << 4) & 0xf0;

        b3 = raFile.readUnsignedByte();
        b4 = raFile.readUnsignedByte();

        b5 = raFile.readUnsignedByte();
        b5 = (b5 << 1) & 0xff;

        tmpInt = b5 >> 4;
        b2 = b2 | tmpInt;

        b3 = b5 & 0x0F;
        b3 = b3 << 4;
        b6 = raFile.readUnsignedByte();

        if (b6 == 128) {
            b3 = b3 | 0x0F;
            b4 = b5 = b6 = b7 = b8 = 0xFF;
        } else if (b6 == 0) {
            b4 = b5 = b6 = b7 = b8 = 0x00;
        } else if (b6 == 64) {
            b3 = b3 | 0x07;
            b4 = b5 = b6 = b7 = b8 = 0xFF;
        }

        if (endianess) {
            tmpLong = (((long) b1 << 56) + ((long) b2 << 48) + ((long) b3 << 40) + ((long) b4 << 32) +
                       ((long) b5 << 24) + ((long) b6 << 16) + ((long) b7 << 8) + b8);
            tmp = Double.longBitsToDouble(tmpLong);
        } else {
            tmpLong = (((long) b8 << 56) + ((long) b7 << 48) + ((long) b6 << 40) + ((long) b5 << 32) +
                       ((long) b4 << 24) + ((long) b3 << 16) + ((long) b2 << 8) + b1);
            tmp = Double.longBitsToDouble(tmpLong);
        }

        return tmp;
    }


}
