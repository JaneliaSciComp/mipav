package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Comments from David Mastronarde:<br>
 * I am not familiar with the latest standards for MRC files, but typically the extra header information has been used
 * to save information about each section in the file. Our own implementation is largely derived from that of the Agard
 * group at UCSF, from which the Deltavision software originates. The Deltavision standard uses two fields called
 * &quot;nreal&quot; and &quot;nint&quot; to describe the number of 4-byte reals and integers stored per section. I
 * deviated from that and used &quot;nint&quot; to hold the number of bytes of data per section, and &quot;nreal&quot;
 * to hold a set of flags describing which kinds of data were present (tilt angle, frame X-Y-Z coordinates for a
 * montage, stage position, magnification, all stored as 2-byte integers.). This information is present only for data
 * acquired on our microscope here.
 */
public class FileMRC extends FileBase {

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
    private FileInfoMRC fileInfo;
    
    private FileInfoMRC fileInfoCopy;

    /** DOCUMENT ME! */
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
                       * 0 = unsigned char 1 = short 2 = float 3 = short * 2 (used for complex data) 4 = float * 2 (used
                       * for complex data) 16 = unsigned char * 3 (used for rgb data) 17 = compressed RGB
                       */

    /** DOCUMENT ME! */
    private int mx, my, mz; /* Number of rows to read */

    /** DOCUMENT ME! */
    private int numberSlices; // 1 for 2D, zDim for 3D, and zDim * tDim for 4D

    /** DOCUMENT ME! */
    private int nx, ny, nz;

    @SuppressWarnings("unused")
    private int nXStart, nYStart, nZStart; /* Starting point of each sub image. */

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * MRC reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileMRC(String fileName, String fileDir) throws IOException {
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
     * reads the MRC file header and data.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @return     DOCUMENT ME!
     */
    public ModelImage readImage() throws IOException {

        int i;
        float[] imgBuffer;
        float[] imgBuffer2;
        int bufferSize;
        float xlen, ylen, zlen;
        float[] imgResols = new float[] { 1.0f, 1.0f, 1.0f, 1.0f, 1.0f };
        float alpha, beta, gamma;
        int mapC, mapR, mapS;
        short ispg, nSymbt;
        int next;
        short creatorID;
        short nint;
        short nreal;
        short idType;
        short lens;
        short nd1, nd2, vd1, vd2;
        float[] tiltAngles = new float[6];
        float xOrg, yOrg, zOrg;
        int nlabl; // Number of labels with useful data
        String[] label;
        String s;
        boolean do2000;
        int stamp;
        float rms;
        short nwave;
        short wave1, wave2, wave3, wave4, wave5;

        try {

            file = new File(fileDir + fileName);

            endianess = FileBase.LITTLE_ENDIAN; // false
            fileInfo = new FileInfoMRC(fileName, fileDir, FileUtility.MRC);

            raFile = new RandomAccessFile(file, "r");

            // Read location 208 to see if it contains MAP
            // If it contains "MAP ", this is a NEW-STYLE MRC image2000 header
            // for IMOD 2.6.20 and above
            // Otherwise, it is an OLD-STYLE MRC header for IMOD 2.6.19 and below
            raFile.seek(208);
            s = getString(4);

            if (s.equals("MAP ")) {
                do2000 = true;
                stamp = raFile.readUnsignedByte();

                if (stamp == 17) {
                    endianess = FileBase.BIG_ENDIAN;
                } else if (stamp == 68) {
                    endianess = FileBase.LITTLE_ENDIAN;
                } else {
                    MipavUtil.displayError("Stamp byte at location 212 is an illegal " + stamp + "\n");
                    throw new IOException();
                }
            } else {
                do2000 = false;
            }

            raFile.seek(0L);
            nx = getInt(endianess);

            if ((nx < 0) || (nx >= 65536)) {
                endianess = FileBase.BIG_ENDIAN;
                raFile.seek(0L);
                nx = getInt(endianess);
            }

            fileInfo.setEndianess(endianess);
            ny = getInt(endianess);
            nz = getInt(endianess);
            imgExtents[0] = nx;
            imgExtents[1] = ny;
            imgExtents[2] = nz;
            fileInfo.setExtents(imgExtents);
            mode = getInt(endianess); // 0 = unsigned char
                                      // 1 = short
                                      // 2 = float
                                      // 3 = short * 2, (used for complex data)
                                      // 4 = float * 2, (used for complex data)
                                      // 16 = unsigned char * 3 (used for rgb data)
                                      // 17 = compressed RGB

            if (mode == 0) {
                destType = ModelStorageBase.UBYTE;
                dataSize = nx * ny * nz;
            } else if (mode == 1) {
                destType = ModelStorageBase.SHORT;
                dataSize = 2 * nx * ny * nz;
            } else if (mode == 2) {
                destType = ModelStorageBase.FLOAT;
                dataSize = 4 * nx * ny * nz;
            } else if (mode == 3) {
                destType = ModelStorageBase.COMPLEX;
                dataSize = 4 * nx * ny * nz;
            } else if (mode == 4) {
                destType = ModelStorageBase.COMPLEX;
                dataSize = 8 * nx * ny * nz;
            } else if (mode == 16) {
                destType = ModelStorageBase.ARGB;
                dataSize = 3 * nx * ny * nz;
            } else if (mode == 17) {
                raFile.close();

                MipavUtil.displayError("Compressed RGB mode not implemented");
                throw new IOException();
            } else {
                raFile.close();

                MipavUtil.displayError("mode is an illegal " + mode);
                throw new IOException();
            }

            fileInfo.setDataType(destType);
            image = new ModelImage(destType, imgExtents, fileInfo.getFileName());

            nXStart = getInt(endianess); // starting point of sub image.  This information is ignored
            nYStart = getInt(endianess);
            nZStart = getInt(endianess);

            mx = getInt(endianess); // grid size
            my = getInt(endianess);
            mz = getInt(endianess);

            xlen = getFloat(endianess); // cell length; pixel spacing = xlen/mx
            ylen = getFloat(endianess);
            zlen = getFloat(endianess);
            imgResols[0] = xlen / mx;
            imgResols[1] = ylen / my;
            imgResols[2] = zlen / mz;
            fileInfo.setResolutions(imgResols);
            fileInfo.setUnitsOfMeasure(Unit.NANOMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.NANOMETERS.getLegacyNum(), 1);
            fileInfo.setUnitsOfMeasure(Unit.NANOMETERS.getLegacyNum(), 2);

            alpha = getFloat(endianess);
            fileInfo.setAlpha(alpha);
            beta = getFloat(endianess);
            fileInfo.setBeta(beta);
            gamma = getFloat(endianess);
            fileInfo.setGamma(gamma);

            mapC = getInt(endianess);
            fileInfo.setMapC(mapC);
            mapR = getInt(endianess);
            fileInfo.setMapR(mapR);
            mapS = getInt(endianess);
            fileInfo.setMapS(mapS);

            getFloat(endianess); // minimum pixel value
            getFloat(endianess); // maximum pixel value
            getFloat(endianess); // mean pixel value

            ispg = (short) getSignedShort(endianess); // space group number
            fileInfo.setIspg(ispg);
            nSymbt = (short) getSignedShort(endianess); // number of bytes used
                                                        // for symmetry data
            fileInfo.setNSymbt(nSymbt);
            next = getInt(endianess); // number of bytes in extended header
            fileInfo.setNext(next);
            fileSize = dataSize + 1024 + next;

            if (fileSize > raFile.length()) {
                MipavUtil.displayError("Expected file size of " + fileSize + " excceds actual file size of " +
                                       raFile.length());
            }

            creatorID = (short) getSignedShort(endianess);
            fileInfo.setCreatorID(creatorID);

            raFile.seek(128L);
            nint = (short) getSignedShort(endianess); // number of bytes per section in
                                                      // extended header

            fileInfo.setNint(nint);
            nreal = (short) getSignedShort(endianess); // Flags for which types of data from
                                                       // serial EM (shorts)
                                                       // 1 = tilt angle * 100 (2 bytes)
                                                       // 2 = piece coordinate (6 bytes)
                                                       // 4 = Stage position (4 bytes)
                                                       // 8 = Magnification/100 (2 bytes)
                                                       // 16 = Reserved (image size?) (4 bytes)
                                                       // If the number of bytes implied by
                                                       // these flags does not add up to the
                                                       // value in nint, then nint and nreal
                                                       // are interpreted as ints and floats
                                                       // per section.
            fileInfo.setNreal(nreal);

            raFile.seek(160L);

            // Explanation fo type of data (0 = mono, 1 = tilt, 2 = tilts, 3 = lina, 4 = lins)
            idType = (short) getSignedShort(endianess);
            fileInfo.setIDType(idType);
            lens = (short) getSignedShort(endianess);
            fileInfo.setLens(lens);

            // for idtype = 1, nd1 = axis (1, 2, or 3)
            nd1 = (short) getSignedShort(endianess);
            fileInfo.setND1(nd1);
            nd2 = (short) getSignedShort(endianess);
            fileInfo.setND2(nd2);

            // vd1 = 100. * tilt increment
            vd1 = (short) getSignedShort(endianess);
            fileInfo.setVD1(vd1);

            // vd2 = 100. * starting angle
            vd2 = (short) getSignedShort(endianess);
            fileInfo.setVD2(vd2);

            // Used to rotate model to match new rotated image
            // 0, 1, 2 = original 3, 4, 5 = current
            for (i = 0; i < 6; i++) {
                tiltAngles[i] = getFloat(endianess);
            }

            fileInfo.setTiltAngles(tiltAngles);

            if (do2000) { // new-style MRC image2000 header
                xOrg = getFloat(endianess);
                fileInfo.setOrigin(xOrg, 0);
                yOrg = getFloat(endianess);
                fileInfo.setOrigin(yOrg, 1);
                zOrg = getFloat(endianess);
                fileInfo.setOrigin(zOrg, 2);
                raFile.seek(216L);
                rms = getFloat(endianess);
                fileInfo.setRMS(rms);
            } // if (do2000)
            else { // old-style MRC header
                nwave = (short) getSignedShort(endianess);
                fileInfo.setNWave(nwave);
                wave1 = (short) getSignedShort(endianess);
                fileInfo.setWave1(wave1);
                wave2 = (short) getSignedShort(endianess);
                fileInfo.setWave2(wave2);
                wave3 = (short) getSignedShort(endianess);
                fileInfo.setWave3(wave3);
                wave4 = (short) getSignedShort(endianess);
                fileInfo.setWave4(wave4);
                wave5 = (short) getSignedShort(endianess);
                fileInfo.setWave5(wave5);

                zOrg = getFloat(endianess);
                fileInfo.setOrigin(zOrg, 2);
                xOrg = getFloat(endianess);
                fileInfo.setOrigin(xOrg, 0);
                yOrg = getFloat(endianess);
                fileInfo.setOrigin(yOrg, 1);
            }

            nlabl = getInt(endianess);

            if (nlabl >= 1) {
                label = new String[nlabl];

                for (i = 0; i < nlabl; i++) {
                    label[i] = getString(80);
                }

                fileInfo.setLabel(label);
            }

            raFile.seek(1024L + next);

            // The file header size is 1024 bytes plus the size of the extended header
            if ((mode == 0) || (mode == 1) || (mode == 2)) {
                bufferSize = imgExtents[0] * imgExtents[1];
                imgBuffer = new float[bufferSize];

                for (i = 0; i < imgExtents[2]; i++) {
                    fileInfoCopy = (FileInfoMRC)fileInfo.clone();
                    image.setFileInfo(fileInfoCopy, i);
                    readBuffer(i, imgBuffer);
                    image.importData(i * bufferSize, imgBuffer, false);
                }
            } else if ((mode == 3) || (mode == 4)) {
                bufferSize = imgExtents[0] * imgExtents[1];
                imgBuffer = new float[bufferSize];
                imgBuffer2 = new float[bufferSize];

                for (i = 0; i < imgExtents[2]; i++) {
                    fileInfoCopy = (FileInfoMRC)fileInfo.clone();
                    image.setFileInfo(fileInfoCopy, i);
                    readComplexBuffer(i, imgBuffer, imgBuffer2);
                    image.importComplexData(2 * i * bufferSize, imgBuffer, imgBuffer2, false, Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
                }
            } else { // mode == 16
                bufferSize = 4 * imgExtents[0] * imgExtents[1];
                imgBuffer = new float[bufferSize];

                for (i = 0; i < imgExtents[2]; i++) {
                    fileInfoCopy = (FileInfoMRC)fileInfo.clone();
                    image.setFileInfo(fileInfoCopy, i);
                    readBuffer(i, imgBuffer);
                    image.importData(i * bufferSize, imgBuffer, false);
                }
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
     * Writes a MRC format type image.
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
        int tmpIntI;
        byte[] byteBuffer;
        short[] shortBuffer;
        float[] floatBuffer;
        float[] floatBufferI;
        byte[] byteBufferR;
        byte[] byteBufferG;
        byte[] byteBufferB;
        int sBegin; // first z slice to write
        int sEnd; // last z slice to write
        int tBegin; // first t time to write
        int tEnd; // last t time to write
        int z, t;
        int zDim;
        int count;
        byte[] extraData;
        byte[] cmap = new byte[4];
        byte[] stamp = new byte[4];
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
        } else {
            writeInt(numberSlices, endianess);
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

            case ModelStorageBase.USHORT:
            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
            case ModelStorageBase.FLOAT:
                writeInt(2, endianess); // FLOAT
                break;

            case ModelStorageBase.COMPLEX:
                writeInt(4, endianess); // COMPLEX
                break;

            case ModelStorageBase.ARGB:
                writeInt(16, endianess); // RGB
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
            writeInt(numberSlices, endianess);
        }

        // Change resolution units to nanometers
        resXUnit = image.getFileInfo(0).getUnitsOfMeasure(0);

        if ((resXUnit == Unit.INCHES.getLegacyNum()) || (resXUnit == Unit.MILS.getLegacyNum()) ||
                (resXUnit == Unit.CENTIMETERS.getLegacyNum()) ||
                (resXUnit == Unit.ANGSTROMS.getLegacyNum()) || (resXUnit == Unit.NANOMETERS.getLegacyNum()) ||
                (resXUnit == Unit.MICROMETERS.getLegacyNum()) || (resXUnit == Unit.MILLIMETERS.getLegacyNum()) ||
                (resXUnit == Unit.METERS.getLegacyNum()) || (resXUnit == Unit.KILOMETERS.getLegacyNum()) ||
                (resXUnit == Unit.MILES.getLegacyNum())) {
            resX = image.getFileInfo(0).getResolutions()[0];

            if (resXUnit == Unit.INCHES.getLegacyNum()) {
                resX = 2.54e+7f * resX;
            } else if (resXUnit == Unit.MILS.getLegacyNum()) {
                resX = 2.54e+4f * resX;
            } else if (resXUnit == Unit.CENTIMETERS.getLegacyNum()) {
                resX = 1.0e+7f * resX;
            } else if (resXUnit == Unit.ANGSTROMS.getLegacyNum()) {
                resX = 1.0e-1f * resX;
            } else if (resXUnit == Unit.NANOMETERS.getLegacyNum()) {
                // unchanged
            } else if (resXUnit == Unit.MICROMETERS.getLegacyNum()) {
                resX = 1.0e+3f * resX;
            } else if (resXUnit == Unit.MILLIMETERS.getLegacyNum()) {
                resX = 1.0e+6f * resX;
            } else if (resXUnit == Unit.METERS.getLegacyNum()) {
                resX = 1.0e+9f * resX;
            } else if (resXUnit == Unit.KILOMETERS.getLegacyNum()) {
                resX = 1.0e12f * resX;
            } else if (resXUnit == Unit.MILES.getLegacyNum()) {
                resX = 1.6093e12f * resX;
            }
        }

        // Change resolution units to nanometers
        resYUnit = image.getFileInfo(0).getUnitsOfMeasure(1);

        if ((resYUnit == Unit.INCHES.getLegacyNum()) || (resYUnit == Unit.MILS.getLegacyNum()) ||
                (resYUnit == Unit.CENTIMETERS.getLegacyNum()) ||
                (resYUnit == Unit.ANGSTROMS.getLegacyNum()) || (resYUnit == Unit.NANOMETERS.getLegacyNum()) ||
                (resYUnit == Unit.MICROMETERS.getLegacyNum()) || (resYUnit == Unit.MILLIMETERS.getLegacyNum()) ||
                (resYUnit == Unit.METERS.getLegacyNum()) || (resYUnit == Unit.KILOMETERS.getLegacyNum()) ||
                (resYUnit == Unit.MILES.getLegacyNum())) {
            resY = image.getFileInfo(0).getResolutions()[1];

            if (resYUnit == Unit.INCHES.getLegacyNum()) {
                resY = 2.54e+7f * resY;
            } else if (resYUnit == Unit.MILS.getLegacyNum()) {
                resY = 2.54e+4f * resY;
            } else if (resYUnit == Unit.CENTIMETERS.getLegacyNum()) {
                resY = 1.0e+7f * resY;
            } else if (resYUnit == Unit.ANGSTROMS.getLegacyNum()) {
                resY = 1.0e-1f * resY;
            } else if (resYUnit == Unit.NANOMETERS.getLegacyNum()) {
                // unchanged
            } else if (resYUnit == Unit.MICROMETERS.getLegacyNum()) {
                resY = 1.0e+3f * resY;
            } else if (resYUnit == Unit.MILLIMETERS.getLegacyNum()) {
                resY = 1.0e+6f * resY;
            } else if (resYUnit == Unit.METERS.getLegacyNum()) {
                resY = 1.0e+9f * resY;
            } else if (resYUnit == Unit.KILOMETERS.getLegacyNum()) {
                resY = 1.0e12f * resY;
            } else if (resYUnit == Unit.MILES.getLegacyNum()) {
                resY = 1.6093e12f * resY;
            }
        }

        // Change resolution units to nanometers
        if (image.getNDims() >= 3) {
            resZUnit = image.getFileInfo(0).getUnitsOfMeasure(2);

            if ((resZUnit == Unit.INCHES.getLegacyNum()) || (resZUnit == Unit.MILS.getLegacyNum()) ||
                    (resZUnit == Unit.CENTIMETERS.getLegacyNum()) ||
                    (resZUnit == Unit.ANGSTROMS.getLegacyNum()) || (resZUnit == Unit.NANOMETERS.getLegacyNum()) ||
                    (resZUnit == Unit.MICROMETERS.getLegacyNum()) || (resZUnit == Unit.MILLIMETERS.getLegacyNum()) ||
                    (resZUnit == Unit.METERS.getLegacyNum()) || (resZUnit == Unit.KILOMETERS.getLegacyNum()) ||
                    (resZUnit == Unit.MILES.getLegacyNum())) {
                resZ = image.getFileInfo(0).getResolutions()[2];

                if (resZUnit == Unit.INCHES.getLegacyNum()) {
                    resZ = 2.54e+7f * resZ;
                } else if (resZUnit == Unit.MILS.getLegacyNum()) {
                    resZ = 2.54e+4f * resZ;
                } else if (resZUnit == Unit.CENTIMETERS.getLegacyNum()) {
                    resZ = 1.0e+7f * resZ;
                } else if (resZUnit == Unit.ANGSTROMS.getLegacyNum()) {
                    resZ = 1.0e-1f * resZ;
                } else if (resZUnit == Unit.NANOMETERS.getLegacyNum()) {
                    // unchanged
                } else if (resZUnit == Unit.MICROMETERS.getLegacyNum()) {
                    resZ = 1.0e+3f * resZ;
                } else if (resZUnit == Unit.MILLIMETERS.getLegacyNum()) {
                    resZ = 1.0e+6f * resZ;
                } else if (resZUnit == Unit.METERS.getLegacyNum()) {
                    resZ = 1.0e+9f * resZ;
                } else if (resZUnit == Unit.KILOMETERS.getLegacyNum()) {
                    resZ = 1.0e12f * resZ;
                } else if (resZUnit == Unit.MILES.getLegacyNum()) {
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
            writeFloat(numberSlices * resZ, endianess);
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

        writeShort((short) 0, endianess); // ispg image type
        writeShort((short) 0, endianess); // nsymbt space group number
        writeInt(0, endianess); // next number of bytes in extended header
        writeShort((short) 0, endianess); // creatid Creator ID
        extraData = new byte[30];

        for (i = 0; i < 30; i++) {
            extraData[i] = 0;
        }

        raFile.write(extraData); // not used
        writeShort((short) 0, endianess); // nint number of bytes per section in extended header
        writeShort((short) 0, endianess); // nreal Flags for which types of data from SerialEM
        extraData = new byte[28];

        for (i = 0; i < 28; i++) {
            extraData[i] = 0;
        }

        raFile.write(extraData); // not used
        writeShort((short) 0, endianess); // idtype 0 = mono 1 = tilt 2 = tilts 3 = lina 4 = lins
        writeShort((short) 0, endianess); // lens
        writeShort((short) 0, endianess); // nd1 for idtype = 1 nd1 = axis (1, 2, or 3)
        writeShort((short) 0, endianess); // nd2
        writeShort((short) 0, endianess); // vd1 vd1 = 100. * tilt increment
        writeShort((short) 0, endianess); // vd2 vd2 = 100. * starting angle

        for (i = 0; i < 6; i++) {
            writeFloat(0.0f, endianess); // tiltangles 0,1,2 = original 3,4,5 = current
        }

        // origin of image
        // Using our startLocations can cause a problem
        writeFloat(0.0f, endianess); // xorg
        writeFloat(0.0f, endianess); // yorg
        writeFloat(0.0f, endianess); // zorg

        cmap[0] = 77; // M
        cmap[1] = 65; // A
        cmap[2] = 80; // P
        cmap[3] = 32; // space
        raFile.write(cmap); // cmap

        if (endianess == FileBase.BIG_ENDIAN) {
            stamp[0] = 17;
        } else {
            stamp[0] = 68;
        }

        stamp[1] = 0;
        stamp[2] = 0;
        stamp[3] = 0;
        raFile.write(stamp); // stamp
        writeFloat(0.0f, endianess); // rms
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

                // store as 16 bit signed short
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

            case ModelStorageBase.USHORT:
            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
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

            case ModelStorageBase.COMPLEX:
                floatBuffer = new float[sliceSize];
                floatBufferI = new float[sliceSize];
                byteBuffer = new byte[8 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = sBegin; z <= sEnd; z++, count++) {
                        i = (t * zDim) + z;
                        fireProgressStateChanged(count * 100 / numberSlices);
                        image.exportComplexData(2 * i * sliceSize, sliceSize, floatBuffer, floatBufferI);

                        for (j = 0; j < sliceSize; j++) {
                            tmpInt = Float.floatToIntBits(floatBuffer[j]);
                            tmpIntI = Float.floatToIntBits(floatBufferI[j]);

                            if (endianess == FileBase.BIG_ENDIAN) {
                                byteBuffer[8 * j] = (byte) (tmpInt >>> 24);
                                byteBuffer[(8 * j) + 1] = (byte) (tmpInt >>> 16);
                                byteBuffer[(8 * j) + 2] = (byte) (tmpInt >>> 8);
                                byteBuffer[(8 * j) + 3] = (byte) (tmpInt);
                                byteBuffer[(8 * j) + 4] = (byte) (tmpIntI >>> 24);
                                byteBuffer[(8 * j) + 5] = (byte) (tmpIntI >>> 16);
                                byteBuffer[(8 * j) + 6] = (byte) (tmpIntI >>> 8);
                                byteBuffer[(8 * j) + 7] = (byte) (tmpIntI);
                            } else {
                                byteBuffer[8 * j] = (byte) (tmpInt);
                                byteBuffer[(8 * j) + 1] = (byte) (tmpInt >>> 8);
                                byteBuffer[(8 * j) + 2] = (byte) (tmpInt >>> 16);
                                byteBuffer[(8 * j) + 3] = (byte) (tmpInt >>> 24);
                                byteBuffer[(8 * j) + 4] = (byte) (tmpIntI);
                                byteBuffer[(8 * j) + 5] = (byte) (tmpIntI >>> 8);
                                byteBuffer[(8 * j) + 6] = (byte) (tmpIntI >>> 16);
                                byteBuffer[(8 * j) + 7] = (byte) (tmpIntI >>> 24);
                            }
                        } // for (j = 0; j < sliceSize; j++)

                        raFile.write(byteBuffer);
                    } // for (z = sBegin; z <= sEnd; z++,count++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.ARGB:
                byteBufferR = new byte[sliceSize];
                byteBufferG = new byte[sliceSize];
                byteBufferB = new byte[sliceSize];
                byteBuffer = new byte[3 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = sBegin; z <= sEnd; z++, count++) {
                        i = (t * zDim) + z;
                        fireProgressStateChanged(count * 100 / numberSlices);
                        image.exportRGBData(1, 4 * i * sliceSize, sliceSize, byteBufferR);
                        image.exportRGBData(2, 4 * i * sliceSize, sliceSize, byteBufferG);
                        image.exportRGBData(3, 4 * i * sliceSize, sliceSize, byteBufferB);

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[3 * j] = byteBufferR[j];
                            byteBuffer[(3 * j) + 1] = byteBufferG[j];
                            byteBuffer[(3 * j) + 2] = byteBufferB[j];
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

            case 16: // unsigned char * 3, used for rgb data
                byteBuffer = new byte[3 * imgExtents[0] * imgExtents[1]];
                nBytes = 3 * imgExtents[0] * imgExtents[1];
                raFile.read(byteBuffer, 0, 3 * imgExtents[0] * imgExtents[1]);
                /*j = 0;
                 * for (y = nYStart; y <nYStart + imgExtents[1]; y++) { for (x = nXStart; x < nXStart + imgExtents[0];
                 * x++) {     raFile.seek(1024 + 3*(x + imgExtents[0]*y + imgExtents[0]*imgExtents[1]*(nZStart+slice)));
                 * raFile.read(byteBuffer,3*j*imgExtents[0],3*imgExtents[0]);     j++; }}*/

                progress = slice * buffer.length;
                progressLength = buffer.length * imgExtents[2];
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
        } // switch(mode)


    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice    offset into the file stored in the dataOffset array
     * @param      buffer   buffer where the real info is stored
     * @param      buffer2  buffer where the imaginary info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readComplexBuffer(int slice, float[] buffer, float[] buffer2) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        int b1, b2, b3, b4;
        byte[] byteBuffer;
        int progress, progressLength, mod;
        int tmpInt;

        switch (mode) {

            case 3: // short * 2, used for complex data
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

                    if (endianess) {
                        buffer[i] = (short) ((b1 << 8) + b2);
                    } else {
                        buffer[i] = (short) ((b2 << 8) + b1);
                    }

                    b1 = getUnsignedByte(byteBuffer, j + 2);
                    b2 = getUnsignedByte(byteBuffer, j + 3);

                    if (endianess) {
                        buffer2[i] = (short) ((b1 << 8) + b2);
                    } else {
                        buffer2[i] = (short) ((b2 << 8) + b1);
                    }

                } // for (j = 0; j < nBytes; j+=4, i++ )

                break;

            case 4: // float * 2, used for complex data
                byteBuffer = new byte[8 * buffer.length];
                nBytes = 8 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);

                /*j = 0;
                 * for (y = nYStart; y <nYStart + imgExtents[1]; y++) { for (x = nXStart; x < nXStart + imgExtents[0];
                 * x++) {     raFile.seek(1024 + 8*(x + imgExtents[0]*y + imgExtents[0]*imgExtents[1]*(nZStart+slice)));
                 * raFile.read(byteBuffer,8*j*imgExtents[0],8*imgExtents[0]);     j++; }}*/
                progress = slice * buffer.length;
                progressLength = buffer.length * imgExtents[2];
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 8, i++) {

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

                    b1 = getUnsignedByte(byteBuffer, j + 4);
                    b2 = getUnsignedByte(byteBuffer, j + 5);
                    b3 = getUnsignedByte(byteBuffer, j + 6);
                    b4 = getUnsignedByte(byteBuffer, j + 7);

                    if (endianess) {
                        tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    } else {
                        tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                    }

                    buffer2[i] = Float.intBitsToFloat(tmpInt);

                } // for (j =0; j < nBytes; j+=8, i++ )

                break;
        } // switch(mode)


    }


}
