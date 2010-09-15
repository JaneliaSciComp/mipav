package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * DOCUMENT ME!
 */
public class FileOSM extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int dataSize;

    /** DOCUMENT ME! */
    private int destType;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoOSM fileInfo;
    
    private FileInfoOSM fileInfoCopy;

    /**
     * Based on 1996 files washu.h, write_dataset.c, and read_dataset.c from the Biomedical Computer Laboratory of
     * Washington University in Missouri. This file format is a modification of the MRC file format and contains a
     * similar 1024 byte header. The header is called the WASHU header and the dataset structure is called the OSM
     * dataset structure.
     */


    private String fileName;

    /** DOCUMENT ME! */
    private int fileSize;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int[] imgExtents = new int[3];

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    /** DOCUMENT ME! */
    private int mode; /* Types of pixel in image.
                       * 0 = unsigned char 1 = short 2 = float 4 = unsigned short 5 = integer
                       */

    /** DOCUMENT ME! */
    private int mx, my, mz; /* Number of rows to read */

    /** DOCUMENT ME! */
    private int numberSlices; // 1 for 2D, zDim for 3D, and zDim * tDim for 4D

    /** DOCUMENT ME! */
    private int nx, ny, nz;

    /** DOCUMENT ME! */
    private int nXStart, nYStart, nZStart; /* Starting point of each sub image. */

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * OSM reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileOSM(String fileName, String fileDir) throws IOException {

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
        fileInfoCopy = null;
        file = null;
        image = null;
        imgExtents = null;
        LUT = null;
        
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * returns LUT if defined.
     *
     * @return  the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }

    /**
     * reads the OSM file header and data.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @return     DOCUMENT ME!
     */
    public ModelImage readImage() throws IOException {

        int i;
        float[] imgBuffer;
        int bufferSize;
        float xlen, ylen, zlen;
        float[] imgResols = new float[] { 1.0f, 1.0f, 1.0f, 1.0f, 1.0f };
        float alpha, beta, gamma;
        int mapC, mapR, mapS;
        int ispg, nSymbt;
        short idType;
        short lens;
        float[] tiltAngles = new float[6];
        float xOrg, yOrg, zOrg;
        int nlabl; // Number of labels with useful data
        String[] label;
        int wave1, wave2, wave3;
        int magicNumber;
        int user1Flags;
        int footerSize;

        try {
            
            file = new File(fileDir + fileName);

            endianess = FileBase.LITTLE_ENDIAN; // false
            fileInfo = new FileInfoOSM(fileName, fileDir, FileUtility.OSM);

            raFile = new RandomAccessFile(file, "r");

            // Read location 152, known as user15, to check for the magic
            // number TVAL = 1234.  A byte swabbed TVAL would be read as
            // -771489792
            raFile.seek(152);
            magicNumber = getInt(endianess);

            if (magicNumber == 1234) {
                Preferences.debug("Successful read of magic number 1234 at location 152\n");
                Preferences.debug("File is little endian\n");
            } else if (magicNumber == -771489792) {
                endianess = FileBase.BIG_ENDIAN;
                Preferences.debug("Successful read of byte swabbed magic number 1234 at location 152\n");
                Preferences.debug("File is big endian\n");
            } else {
                MipavUtil.displayError("Magic number at location 152 is an illegal " + magicNumber);
                throw new IOException();
            }

            fileInfo.setEndianess(endianess);
            raFile.seek(0L);
            nx = getInt(endianess);
            ny = getInt(endianess); // location 4
            nz = getInt(endianess); // location 8
            imgExtents[0] = nx;
            imgExtents[1] = ny;
            imgExtents[2] = nz;
            fileInfo.setExtents(imgExtents);

            // location 12
            mode = getInt(endianess); // 0 = unsigned char
                                      // 1 = short
                                      // 2 = float
                                      // 4 = unsigned short
                                      // 5 = int

            if (mode == 0) {
                destType = ModelStorageBase.UBYTE;
                dataSize = nx * ny * nz;
            } else if (mode == 1) {
                destType = ModelStorageBase.SHORT;
                dataSize = 2 * nx * ny * nz;
            } else if (mode == 2) {
                destType = ModelStorageBase.FLOAT;
                dataSize = 4 * nx * ny * nz;
            } else if (mode == 4) {

                // Promote USHORT to integer
                destType = ModelStorageBase.INTEGER;
                dataSize = 4 * nx * ny * nz;
            } else if (mode == 5) {
                destType = ModelStorageBase.INTEGER;
                dataSize = 4 * nx * ny * nz;
            } else {
                raFile.close();

                MipavUtil.displayError("mode is an illegal " + mode);
                throw new IOException();
            }

            fileInfo.setDataType(destType);
            image = new ModelImage(destType, imgExtents, fileInfo.getFileName());

            // This information is ignored
            // locations 16, 20, and 24
            nXStart = getInt(endianess); // number of first column in map
            nYStart = getInt(endianess); // number of first row in map
            nZStart = getInt(endianess); // mumber of first section in map

            // locations 28, 32, and 36
            mx = getInt(endianess); // grid size
            Preferences.debug("mx = " + mx + "\n");
            my = getInt(endianess);
            Preferences.debug("my = " + my + "\n");
            mz = getInt(endianess);
            Preferences.debug("mz = " + mz + "\n");

            // locations 40, 44, and 48
            xlen = getFloat(endianess); // cell length; pixel spacing = xlen/mx
            Preferences.debug("xlen = " + xlen + "\n");
            ylen = getFloat(endianess);
            Preferences.debug("ylen = " + ylen + "\n");
            zlen = getFloat(endianess);
            Preferences.debug("zlen = " + zlen + "\n");

            if ((mx > 0) && (my > 0) && (mz > 0) && (xlen > 0.0) && (ylen > 0.0) && (zlen > 0.0)) {
                imgResols[0] = xlen / mx;
                imgResols[1] = ylen / my;
                imgResols[2] = zlen / mz;
            } else {
                imgResols[0] = 1.0f;
                imgResols[1] = 1.0f;
                imgResols[2] = 1.0f;
            }

            fileInfo.setResolutions(imgResols);
            fileInfo.setUnitsOfMeasure(FileInfoBase.NANOMETERS, 0);
            fileInfo.setUnitsOfMeasure(FileInfoBase.NANOMETERS, 1);
            fileInfo.setUnitsOfMeasure(FileInfoBase.NANOMETERS, 2);

            // cell angles in degrees
            // locations 52, 56, and 60
            alpha = getFloat(endianess);
            fileInfo.setAlpha(alpha);
            beta = getFloat(endianess);
            fileInfo.setBeta(beta);
            gamma = getFloat(endianess);
            fileInfo.setGamma(gamma);

            // Which column corresponds to columns, rows, sections
            // locations 64, 68, and 72
            mapC = getInt(endianess);
            fileInfo.setMapC(mapC);
            mapR = getInt(endianess);
            fileInfo.setMapR(mapR);
            mapS = getInt(endianess);
            fileInfo.setMapS(mapS);

            // locations 76, 80, and 84
            getFloat(endianess); // minimum pixel value
            getFloat(endianess); // maximum pixel value
            getFloat(endianess); // mean pixel value

            // location 88
            ispg = getInt(endianess); // space group number
            fileInfo.setIspg(ispg);

            // location 92
            nSymbt = getInt(endianess); // number of bytes used for symmetry data
            fileInfo.setNSymbt(nSymbt);

            // Used for dataset dirty bit
            // location 96
            user1Flags = getInt(endianess);
            raFile.seek(156L);
            footerSize = getInt(endianess); // footer size in bytes
            fileInfo.setFooterSize(footerSize);

            if (mode == 4) { // USHORT
                fileSize = (dataSize / 2) + 1024 + footerSize;
            } else {
                fileSize = dataSize + 1024 + footerSize;
            }

            if (fileSize > raFile.length()) {
                MipavUtil.displayError("Expected file size of " + fileSize + " excceds actual file size of " +
                                       raFile.length());
            }

            // Explanation fo type of data (0 = mono, 1 = tilt, 2 = tilts, 3 = lina, 4 = lins)
            idType = (short) getSignedShort(endianess); // location 160
            fileInfo.setIDType(idType);
            lens = (short) getSignedShort(endianess); // location 162
            fileInfo.setLens(lens);

            raFile.seek(172L);

            // Used to rotate model to match new rotated image
            // 0, 1, 2 = original 3, 4, 5 = current
            for (i = 0; i < 6; i++) {
                tiltAngles[i] = getFloat(endianess);
            }

            fileInfo.setTiltAngles(tiltAngles);


            wave1 = getInt(endianess); // location 196
            fileInfo.setWave1(wave1);
            wave2 = getInt(endianess); // location 200
            fileInfo.setWave2(wave2);
            wave3 = getInt(endianess); // location 204
            fileInfo.setWave3(wave3);
            xOrg = getFloat(endianess); // location 208
            fileInfo.setOrigin(xOrg, 0);
            yOrg = getFloat(endianess); // location 212
            fileInfo.setOrigin(yOrg, 1);
            zOrg = getFloat(endianess); // location 216
            fileInfo.setOrigin(zOrg, 2);

            nlabl = getInt(endianess); // location 220

            if (nlabl >= 1) {
                label = new String[nlabl];

                for (i = 0; i < nlabl; i++) {
                    label[i] = getString(80);
                }

                fileInfo.setLabel(label);
            }

            raFile.seek(1024L);

            // The file header size is 1024 bytes
            bufferSize = imgExtents[0] * imgExtents[1];
            imgBuffer = new float[bufferSize];

            for (i = 0; i < imgExtents[2]; i++) {
                fileInfoCopy = (FileInfoOSM)fileInfo.clone();
                image.setFileInfo(fileInfoCopy, i);
                readBuffer(i, imgBuffer);
                image.importData(i * bufferSize, imgBuffer, false);
            }

            image.calcMinMax();
            raFile.close();

            return image;
        } catch (Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw new IOException();
        }
    }


    /**
     * Writes an OSM format type image.
     *
     * @param      image    Image model of data to write.
     * @param      options  options such as starting and ending slices and times
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        int i, j;
        int sliceSize;
        int tmpInt;
        byte[] extraData;
        byte[] byteBuffer;
        short[] shortBuffer;
        int[] intBuffer;
        float[] floatBuffer;
        int sBegin; // first z slice to write
        int sEnd; // last z slice to write
        int tBegin; // first t time to write
        int tEnd; // last t time to write
        int z, t;
        int zDim;
        int count;
        float resX = 1.0f;
        float resY = 1.0f;
        float resZ = 1.0f;
        int resXUnit, resYUnit, resZUnit;

        if (image.getNDims() >= 3) {
            sBegin = options.getBeginSlice();
            sEnd = options.getEndSlice();
        } else {
            sBegin = 0;
            sEnd = 0;
        }

        if (image.getNDims() == 4) {
            tBegin = options.getBeginTime();
            tEnd = options.getEndTime();
        } else {
            tBegin = 0;
            tEnd = 0;
        }

        endianess = FileBase.BIG_ENDIAN;
        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "rw");

        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        raFile.setLength(0);

        if (image.getNDims() == 2) {
            numberSlices = 1;
            zDim = 1;
        } else if (image.getNDims() == 3) {
            numberSlices = sEnd - sBegin + 1;
            zDim = image.getExtents()[2];
        } else {
            numberSlices = (sEnd - sBegin + 1) * (tEnd - tBegin + 1);
            zDim = image.getExtents()[2];
        }

        sliceSize = image.getSliceSize();

        writeInt(image.getExtents()[0], endianess); // nx Number of Columns
        writeInt(image.getExtents()[1], endianess); // ny Number of Rows

        // nz Number of Sections
        if (image.getNDims() == 2) {
            writeInt(1, endianess);
        } else if (image.getNDims() == 3) {
            writeInt(image.getExtents()[2], endianess);
        } else if (image.getNDims() == 4) {
            writeInt(image.getExtents()[2] * image.getExtents()[3], endianess);
        }

        // mode
        switch (image.getType()) {

            case ModelStorageBase.UBYTE:
                writeInt(0, endianess); // UBYTE
                break;

            case ModelStorageBase.BYTE:
            case ModelStorageBase.SHORT:
                writeInt(1, endianess); // SHORT
                break;

            case ModelStorageBase.FLOAT:
                writeInt(2, endianess); // FLOAT
                break;

            case ModelStorageBase.USHORT: // USHORT
                writeInt(4, endianess);
                break;

            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
                writeInt(5, endianess); // INTEGER
                break;
        }

        writeInt(0, endianess); // nxstart Starting point of sub image.
        writeInt(0, endianess); // nystart
        writeInt(0, endianess); // nzstart

        writeInt(image.getExtents()[0], endianess); // mx Grid size in X, Y, and Z
        writeInt(image.getExtents()[1], endianess); // my

        // mz
        if (image.getNDims() == 2) {
            writeInt(1, endianess);
        } else {
            writeInt(image.getExtents()[2], endianess);
        }

        // Change resolution units to nanometers
        resXUnit = image.getFileInfo(0).getUnitsOfMeasure(0);

        if ((resXUnit == FileInfoBase.INCHES) || (resXUnit == FileInfoBase.MILS) ||
                (resXUnit == FileInfoBase.CENTIMETERS) ||
                (resXUnit == FileInfoBase.ANGSTROMS) || (resXUnit == FileInfoBase.NANOMETERS) ||
                (resXUnit == FileInfoBase.MICROMETERS) || (resXUnit == FileInfoBase.MILLIMETERS) ||
                (resXUnit == FileInfoBase.METERS) || (resXUnit == FileInfoBase.KILOMETERS) ||
                (resXUnit == FileInfoBase.MILES)) {
            resX = image.getFileInfo(0).getResolutions()[0];

            if (resXUnit == FileInfoBase.INCHES) {
                resX = 2.54e+7f * resX;
            } else if (resXUnit == FileInfoBase.MILS) {
                resX = 2.54e+4f * resX;
            } else if (resXUnit == FileInfoBase.CENTIMETERS) {
                resX = 1.0e+7f * resX;
            } else if (resXUnit == FileInfoBase.ANGSTROMS) {
                resX = 1.0e-1f * resX;
            } else if (resXUnit == FileInfoBase.NANOMETERS) {
                // unchanged
            } else if (resXUnit == FileInfoBase.MICROMETERS) {
                resX = 1.0e+3f * resX;
            } else if (resXUnit == FileInfoBase.MILLIMETERS) {
                resX = 1.0e+6f * resX;
            } else if (resXUnit == FileInfoBase.METERS) {
                resX = 1.0e+9f * resX;
            } else if (resXUnit == FileInfoBase.KILOMETERS) {
                resX = 1.0e12f * resX;
            } else if (resXUnit == FileInfoBase.MILES) {
                resX = 1.6093e12f * resX;
            }
        }

        // Change resolution units to nanometers
        resYUnit = image.getFileInfo(0).getUnitsOfMeasure(1);

        if ((resYUnit == FileInfoBase.INCHES) || (resYUnit == FileInfoBase.MILS) ||
                (resYUnit == FileInfoBase.CENTIMETERS) ||
                (resYUnit == FileInfoBase.ANGSTROMS) || (resYUnit == FileInfoBase.NANOMETERS) ||
                (resYUnit == FileInfoBase.MICROMETERS) || (resYUnit == FileInfoBase.MILLIMETERS) ||
                (resYUnit == FileInfoBase.METERS) || (resYUnit == FileInfoBase.KILOMETERS) ||
                (resYUnit == FileInfoBase.MILES)) {
            resY = image.getFileInfo(0).getResolutions()[1];

            if (resYUnit == FileInfoBase.INCHES) {
                resY = 2.54e+7f * resY;
            } else if (resYUnit == FileInfoBase.MILS) {
                resY = 2.54e+4f * resY;
            } else if (resYUnit == FileInfoBase.CENTIMETERS) {
                resY = 1.0e+7f * resY;
            } else if (resYUnit == FileInfoBase.ANGSTROMS) {
                resY = 1.0e-1f * resY;
            } else if (resYUnit == FileInfoBase.NANOMETERS) {
                // unchanged
            } else if (resYUnit == FileInfoBase.MICROMETERS) {
                resY = 1.0e+3f * resY;
            } else if (resYUnit == FileInfoBase.MILLIMETERS) {
                resY = 1.0e+6f * resY;
            } else if (resYUnit == FileInfoBase.METERS) {
                resY = 1.0e+9f * resY;
            } else if (resYUnit == FileInfoBase.KILOMETERS) {
                resY = 1.0e12f * resY;
            } else if (resYUnit == FileInfoBase.MILES) {
                resY = 1.6093e12f * resY;
            }
        }

        // Change resolution units to nanometers
        if (image.getNDims() >= 3) {
            resZUnit = image.getFileInfo(0).getUnitsOfMeasure(2);

            if ((resZUnit == FileInfoBase.INCHES) || (resZUnit == FileInfoBase.MILS) ||
                    (resZUnit == FileInfoBase.CENTIMETERS) ||
                    (resZUnit == FileInfoBase.ANGSTROMS) || (resZUnit == FileInfoBase.NANOMETERS) ||
                    (resZUnit == FileInfoBase.MICROMETERS) || (resZUnit == FileInfoBase.MILLIMETERS) ||
                    (resZUnit == FileInfoBase.METERS) || (resZUnit == FileInfoBase.KILOMETERS) ||
                    (resZUnit == FileInfoBase.MILES)) {
                resZ = image.getFileInfo(0).getResolutions()[2];

                if (resZUnit == FileInfoBase.INCHES) {
                    resZ = 2.54e+7f * resZ;
                } else if (resZUnit == FileInfoBase.MILS) {
                    resZ = 2.54e+4f * resZ;
                } else if (resZUnit == FileInfoBase.CENTIMETERS) {
                    resZ = 1.0e+7f * resZ;
                } else if (resZUnit == FileInfoBase.ANGSTROMS) {
                    resZ = 1.0e-1f * resZ;
                } else if (resZUnit == FileInfoBase.NANOMETERS) {
                    // unchanged
                } else if (resZUnit == FileInfoBase.MICROMETERS) {
                    resZ = 1.0e+3f * resZ;
                } else if (resZUnit == FileInfoBase.MILLIMETERS) {
                    resZ = 1.0e+6f * resZ;
                } else if (resZUnit == FileInfoBase.METERS) {
                    resZ = 1.0e+9f * resZ;
                } else if (resZUnit == FileInfoBase.KILOMETERS) {
                    resZ = 1.0e12f * resZ;
                } else if (resZUnit == FileInfoBase.MILES) {
                    resZ = 1.6093e12f * resZ;
                }
            }
        } // if (image.getNDims() >= 3)

        // xlen Cell size; pixel spacing = xlen/mx
        writeFloat(image.getExtents()[0] * resX, endianess);
        writeFloat(image.getExtents()[1] * resY, endianess); // ylen

        // zlen
        if (image.getNDims() == 2) {
            writeFloat(1.0f, endianess);
        } else {
            writeFloat(image.getExtents()[2] * resZ, endianess);
        }

        writeFloat(0.0f, endianess); // alpha cell angles
        writeFloat(0.0f, endianess); // beta
        writeFloat(0.0f, endianess); // gamma

        writeInt(1, endianess); // mapc 1 = x
        writeInt(2, endianess); // mapr 2 = y
        writeInt(3, endianess); // maps 3 = z

        writeFloat((float) image.getMin(), endianess); // amin
        writeFloat((float) image.getMax(), endianess); // amax
        writeFloat(0.0f, endianess); // mean pixel value location 84

        writeInt(0, endianess); // ispg space group number
        writeInt(0, endianess); // nsymbt number of bytes used for symmetry
        writeInt(0, endianess); // user1_flags used for dataset dirty bit

        for (i = 2; i <= 14; i++) {
            writeInt(0, endianess); // user2 to user14
        }

        writeInt(1234, endianess); // user15 field receives magic number
        writeInt(0, endianess); // user16 field receives size of footer in bytes

        writeShort((short) 0, endianess); // idtype 0 = mono 1 = tilt 2 = tilts 3 = lina 4 = lins
        writeShort((short) 0, endianess); // lens
        writeShort((short) 0, endianess); // n1 not used
        writeShort((short) 0, endianess); // n2 not used
        writeInt(0, endianess); // data_value2 not used

        for (i = 0; i < 6; i++) {
            writeFloat(0.0f, endianess); // tiltangles 0,1,2 = original 3,4,5 = current
        }

        writeInt(0, endianess); // wave1
        writeInt(0, endianess); // wave2
        writeInt(0, endianess); // wave3

        // origin of image
        // Using our startLocations can cause a problem
        writeFloat(0.0f, endianess); // xorg
        writeFloat(0.0f, endianess); // yorg
        writeFloat(0.0f, endianess); // zorg

        writeInt(0, endianess); // nlabl Number of labels with useful data
        extraData = new byte[800];

        for (i = 0; i < 800; i++) {
            extraData[i] = 0;
        }

        raFile.write(extraData); // 10 labels of 80 characters each

        count = 0;

        switch (image.getType()) {

            case ModelStorageBase.UBYTE:

                // store as 8-bit unsigned
                byteBuffer = new byte[sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = sBegin; z <= sEnd; z++, count++) {
                        i = (t * zDim) + z;
                        fireProgressStateChanged(count * 100 / numberSlices);
                        image.exportSliceXY(i, byteBuffer);
                        raFile.write(byteBuffer);
                    } // for (z = sBegin; z <= sEnd; z++,count++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.BYTE:
            case ModelStorageBase.SHORT:
            case ModelStorageBase.USHORT:

                // store as 16 bit short
                shortBuffer = new short[sliceSize];
                byteBuffer = new byte[2 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = sBegin; z <= sEnd; z++, count++) {
                        i = (t * zDim) + z;
                        fireProgressStateChanged(count * 100 / numberSlices);
                        image.exportSliceXY(i, shortBuffer);

                        for (j = 0; j < sliceSize; j++) {

                            if (endianess == FileBase.BIG_ENDIAN) {
                                byteBuffer[2 * j] = (byte) (shortBuffer[j] >>> 8);
                                byteBuffer[(2 * j) + 1] = (byte) (shortBuffer[j]);
                            } else {
                                byteBuffer[2 * j] = (byte) (shortBuffer[j]);
                                byteBuffer[(2 * j) + 1] = (byte) (shortBuffer[j] >>> 8);
                            }
                        }

                        raFile.write(byteBuffer);
                    } // for (z = sBegin; z <= sEnd; z++,count++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:

                // store as 32 bit integer
                intBuffer = new int[sliceSize];
                byteBuffer = new byte[4 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = sBegin; z <= sEnd; z++, count++) {
                        i = (t * zDim) + z;
                        fireProgressStateChanged(count * 100 / numberSlices);
                        image.exportSliceXY(i, intBuffer);

                        for (j = 0; j < sliceSize; j++) {

                            if (endianess == FileBase.BIG_ENDIAN) {
                                byteBuffer[4 * j] = (byte) (intBuffer[j] >>> 24);
                                byteBuffer[(4 * j) + 1] = (byte) (intBuffer[j] >>> 16);
                                byteBuffer[(4 * j) + 2] = (byte) (intBuffer[j] >>> 8);
                                byteBuffer[(4 * j) + 3] = (byte) (intBuffer[j]);
                            } else {
                                byteBuffer[4 * j] = (byte) (intBuffer[j]);
                                byteBuffer[(4 * j) + 1] = (byte) (intBuffer[j] >>> 8);
                                byteBuffer[(4 * j) + 2] = (byte) (intBuffer[j] >>> 16);
                                byteBuffer[(4 * j) + 3] = (byte) (intBuffer[j] >>> 24);
                            }
                        }

                        raFile.write(byteBuffer);
                    } // for (z = sBegin; z <= sEnd; z++,count++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.FLOAT:

                // store as 32 bit float
                floatBuffer = new float[sliceSize];
                byteBuffer = new byte[4 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = sBegin; z <= sEnd; z++, count++) {
                        i = (t * zDim) + z;
                        fireProgressStateChanged(count * 100 / numberSlices);
                        image.exportSliceXY(i, floatBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            tmpInt = Float.floatToIntBits(floatBuffer[j]);

                            if (endianess == FileBase.BIG_ENDIAN) {
                                byteBuffer[4 * j] = (byte) (tmpInt >>> 24);
                                byteBuffer[(4 * j) + 1] = (byte) (tmpInt >>> 16);
                                byteBuffer[(4 * j) + 2] = (byte) (tmpInt >>> 8);
                                byteBuffer[(4 * j) + 3] = (byte) (tmpInt);
                            } else {
                                byteBuffer[4 * j] = (byte) (tmpInt);
                                byteBuffer[(4 * j) + 1] = (byte) (tmpInt >>> 8);
                                byteBuffer[(4 * j) + 2] = (byte) (tmpInt >>> 16);
                                byteBuffer[(4 * j) + 3] = (byte) (tmpInt >>> 24);
                            }
                        }

                        raFile.write(byteBuffer);
                    } // for (z = sBegin; z <= sEnd; z++,count++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;
        } // switch(image.getType())

        raFile.close();

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
        int i = 0;
        int j;
        int nBytes;
        int b1, b2, b3, b4;
        byte[] byteBuffer;
        int progress, progressLength, mod;
        int tmpInt;

        switch (mode) {

            case 0: // unsigned char
                byteBuffer = new byte[buffer.length];
                nBytes = buffer.length;
                raFile.read(byteBuffer, 0, nBytes);

                /* j = 0;
                 * for (y = nYStart; y <nYStart + imgExtents[1]; y++) {  for (x = nXStart; x < nXStart + imgExtents[0];
                 * x++) {      raFile.seek(1024 + x + imgExtents[0]*y + imgExtents[0]*imgExtents[1]*(nZStart+slice));
                 * raFile.read(byteBuffer,j*imgExtents[0],imgExtents[0]);      j++;  } } */
                progress = slice * buffer.length;
                progressLength = buffer.length * imgExtents[2];
                mod = progressLength / 100;

                for (j = 0; j < nBytes; j++, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = byteBuffer[j] & 0xff;
                }

                break;

            case 1: // short
                byteBuffer = new byte[2 * buffer.length];
                nBytes = 2 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);

                /*j = 0;
                 * for (y = nYStart; y <nYStart + imgExtents[1]; y++) { for (x = nXStart; x < nXStart + imgExtents[0];
                 * x++) {     raFile.seek(1024 + 2*(x + imgExtents[0]*y + imgExtents[0]*imgExtents[1]*(nZStart+slice)));
                 * raFile.read(byteBuffer,2*j*imgExtents[0],2*imgExtents[0]);     j++; }}*/
                progress = slice * buffer.length;
                progressLength = buffer.length * imgExtents[2];
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

                } // for (j = 0; j < nBytes; j+=2, i++ )

                break;

            case 2: // float
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 4 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);

                /*j = 0;
                 * for (y = nYStart; y <nYStart + imgExtents[1]; y++) { for (x = nXStart; x < nXStart + imgExtents[0];
                 * x++) {     raFile.seek(1024 + 4*(x + imgExtents[0]*y + imgExtents[0]*imgExtents[1]*(nZStart+slice)));
                 * raFile.read(byteBuffer,4*j*imgExtents[0],4*imgExtents[0]);     j++; }}*/
                progress = slice * buffer.length;
                progressLength = buffer.length * imgExtents[2];
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
                        tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    } else {
                        tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                    }

                    buffer[i] = Float.intBitsToFloat(tmpInt);

                } // for (j =0; j < nBytes; j+=4, i++ )

                break;

            case 4: // unsigned short
                byteBuffer = new byte[2 * buffer.length];
                nBytes = 2 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);

                /*j = 0;
                 * for (y = nYStart; y <nYStart + imgExtents[1]; y++) { for (x = nXStart; x < nXStart + imgExtents[0];
                 * x++) {     raFile.seek(1024 + 2*(x + imgExtents[0]*y + imgExtents[0]*imgExtents[1]*(nZStart+slice)));
                 * raFile.read(byteBuffer,2*j*imgExtents[0],2*imgExtents[0]);     j++; }}*/
                progress = slice * buffer.length;
                progressLength = buffer.length * imgExtents[2];
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

                } // for (j = 0; j < nBytes; j+=2, i++ )

                break;

            case 5: // integer
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 4 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);

                /*j = 0;
                 * for (y = nYStart; y <nYStart + imgExtents[1]; y++) { for (x = nXStart; x < nXStart + imgExtents[0];
                 * x++) {     raFile.seek(1024 + 4*(x + imgExtents[0]*y + imgExtents[0]*imgExtents[1]*(nZStart+slice)));
                 * raFile.read(byteBuffer,4*j*imgExtents[0],4*imgExtents[0]);     j++; }}*/
                progress = slice * buffer.length;
                progressLength = buffer.length * imgExtents[2];
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
                } // for (j =0; j < nBytes; j+=4, i++ )

                break;

        } // switch(mode)


    }


}
