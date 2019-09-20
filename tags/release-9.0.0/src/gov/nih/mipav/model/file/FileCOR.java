package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * FreeSurfer COR files are used to store 3 dimensional unsigned byte data. Suppose the file base name is table. There
 * is a header file named table-.info/and or table-.info~ which has the header information in ascii. A separate unsigned
 * byte file exists for each slice of the 3D image - table-001, table-002, and so on until the last slice. The default
 * is to have xDim = yDim = zDim = 256 and xResolution = yResolution = zResolution = 0.001 meter. Note that all
 * distances in a COR file represent meters.
 *
 * <p>The header fields are as follows: imnr0 the first slice number imnr1 the last slice number ptype the data type; 2
 * for unsigned byte x the x dimension y the y dimension fov the field of view thick the z resolution psiz = a common
 * value for both the x and y resolutions locatn location strtx the minimum x value in meters endx the maximum x value
 * in meters strty the minimum y value in meters endy the maximum y value in meters strtz the minimum z value in meters
 * endz the maximum z value in meters tr T_r te T_e ti T_i xform transform file ras_good_file = 1 if the orientation
 * fields are set, 0 otherwise x_ras x axis orientation y_ras y axis orientation z_ras z axis orientation c_ras volume
 * center coordinates If orientation information is missing, the file is assumed to be coronal with z axis = R to L, y
 * axis = S to I, and z axis = P to A. readInfoImage reads in the header information from the set of FreeSurfer COR
 * volume images and then readImage is called once for each slice that must be read from a COR image file set.</p>
 */

public class FileCOR extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;


    /** DOCUMENT ME! */
    private FileInfoCOR fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private boolean foundEOF = false;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int[] imageExtents = new int[3];

    /** DOCUMENT ME! */
    private int imageOrientation;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgResols = new float[5];

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    /** DOCUMENT ME! */
    private int[] orient = new int[3];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * COR reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileCOR(String fileName, String fileDir) throws IOException {

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
        file = null;
        image = null;
        imageExtents = null;
        imgBuffer = null;
        imgResols = null;
        LUT = null;
        orient = null;
        
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
     * This method reads one slice from the file into byteBuffer and then transfers into the float array imgBuffer.
     *
     * @param      length  length = imageExtents[0] * imageExtents[1]
     *
     * @exception  IOException  if there is an error reading the file
     */
    public void readImage(int length) throws IOException {
        int j;
        byte[] byteBuffer = null;

        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            byteBuffer = new byte[length];
            imgBuffer = new float[length];
            raFile.read(byteBuffer, 0, length);

            for (j = 0; j < length; j++) {
                imgBuffer[j] = byteBuffer[j] & 0xff;
            }

            raFile.close();
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            byteBuffer = null;
            imgBuffer = null;
            System.gc();
            throw error;
        }

    }


    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void readInfoImage() throws IOException {

        // This reads the .info header file of the COR set of image files
        String lineString = null;
        String[] parseString;
        float[] xr = new float[3];
        float[] yr = new float[3];
        float[] zr = new float[3];
        boolean haveXRAS = false;
        boolean haveYRAS = false;
        boolean haveZRAS = false;
        boolean haveX = false;
        boolean haveY = false;
        int imnr0 = 1;
        int imnr1 = 256;
        boolean haveimnr0 = false;
        boolean haveimnr1 = false;
        boolean haveThick = false;
        boolean havePSize = false;
        float fov;
        float tr, te, ti;

        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "r");
        fileInfo = new FileInfoCOR(fileName, fileDir, FileUtility.COR); // dummy fileInfo
        lineString = readLine();

        while (lineString != null) {
            parseString = parse(lineString);

            // Read in x axis orientation
            if (parseString[0].equalsIgnoreCase("x_ras")) {

                if (parseString.length == 4) {
                    xr[0] = Float.valueOf(parseString[1]).floatValue();
                    xr[1] = Float.valueOf(parseString[2]).floatValue();
                    xr[2] = Float.valueOf(parseString[3]).floatValue();

                    if ((xr[0] == -1.0f) && (xr[1] == 0.0f) && (xr[2] == 0.0f)) {
                        orient[0] = FileInfoBase.ORI_R2L_TYPE;
                        haveXRAS = true;
                    } else if ((xr[0] == 1.0f) && (xr[1] == 0.0f) && (xr[2] == 0.0f)) {
                        orient[0] = FileInfoBase.ORI_L2R_TYPE;
                        haveXRAS = true;
                    } else if ((xr[0] == 0.0f) && (xr[1] == 0.0f) && (xr[2] == -1.0f)) {
                        orient[0] = FileInfoBase.ORI_S2I_TYPE;
                        haveXRAS = true;
                    } else if ((xr[0] == 0.0f) && (xr[1] == 0.0f) && (xr[2] == 1.0f)) {
                        orient[0] = FileInfoBase.ORI_I2S_TYPE;
                        haveXRAS = true;
                    } else if ((xr[0] == 0.0f) && (xr[1] == 1.0f) && (xr[2] == 0.0f)) {
                        orient[0] = FileInfoBase.ORI_P2A_TYPE;
                        haveXRAS = true;
                    } else if ((xr[0] == 0.0f) && (xr[1] == -1.0f) && (xr[2] == 0.0f)) {
                        orient[0] = FileInfoBase.ORI_A2P_TYPE;
                        haveXRAS = true;
                    } else {
                        raFile.close();
                        throw new IOException("Illegal x_ras values of " + xr[0] + " " + xr[1] + " " + xr[2]);
                    }
                } else {
                    raFile.close();
                    throw new IOException("x_ras has parseString with length = " + parseString.length);
                }
            }
            // Read in y axis orientation
            else if (parseString[0].equalsIgnoreCase("y_ras")) {

                if (parseString.length == 4) {
                    yr[0] = Float.valueOf(parseString[1]).floatValue();
                    yr[1] = Float.valueOf(parseString[2]).floatValue();
                    yr[2] = Float.valueOf(parseString[3]).floatValue();

                    if ((yr[0] == -1.0f) && (yr[1] == 0.0f) && (yr[2] == 0.0f)) {
                        orient[1] = FileInfoBase.ORI_R2L_TYPE;
                        haveYRAS = true;
                    } else if ((yr[0] == 1.0f) && (yr[1] == 0.0f) && (yr[2] == 0.0f)) {
                        orient[1] = FileInfoBase.ORI_L2R_TYPE;
                        haveYRAS = true;
                    } else if ((yr[0] == 0.0f) && (yr[1] == 0.0f) && (yr[2] == -1.0f)) {
                        orient[1] = FileInfoBase.ORI_S2I_TYPE;
                        haveYRAS = true;
                    } else if ((yr[0] == 0.0f) && (yr[1] == 0.0f) && (yr[2] == 1.0f)) {
                        orient[1] = FileInfoBase.ORI_I2S_TYPE;
                        haveYRAS = true;
                    } else if ((yr[0] == 0.0f) && (yr[1] == 1.0f) && (yr[2] == 0.0f)) {
                        orient[1] = FileInfoBase.ORI_P2A_TYPE;
                        haveYRAS = true;
                    } else if ((yr[0] == 0.0f) && (yr[1] == -1.0f) && (yr[2] == 0.0f)) {
                        orient[1] = FileInfoBase.ORI_A2P_TYPE;
                        haveYRAS = true;
                    } else {
                        raFile.close();
                        throw new IOException("Illegal y_ras values of " + yr[0] + " " + yr[1] + " " + yr[2]);
                    }
                } else {
                    raFile.close();
                    throw new IOException("y_ras has parseString with length = " + parseString.length);
                }
            }
            // Read in z axis orientation
            else if (parseString[0].equalsIgnoreCase("z_ras")) {

                if (parseString.length == 4) {
                    zr[0] = Float.valueOf(parseString[1]).floatValue();
                    zr[1] = Float.valueOf(parseString[2]).floatValue();
                    zr[2] = Float.valueOf(parseString[3]).floatValue();

                    if ((zr[0] == -1.0f) && (zr[1] == 0.0f) && (zr[2] == 0.0f)) {
                        orient[2] = FileInfoBase.ORI_R2L_TYPE;
                        haveZRAS = true;
                    } else if ((zr[0] == 1.0f) && (zr[1] == 0.0f) && (zr[2] == 0.0f)) {
                        orient[2] = FileInfoBase.ORI_L2R_TYPE;
                        haveZRAS = true;
                    } else if ((zr[0] == 0.0f) && (zr[1] == 0.0f) && (zr[2] == -1.0f)) {
                        orient[2] = FileInfoBase.ORI_S2I_TYPE;
                        haveZRAS = true;
                    } else if ((zr[0] == 0.0f) && (zr[1] == 0.0f) && (zr[2] == 1.0f)) {
                        orient[2] = FileInfoBase.ORI_I2S_TYPE;
                        haveZRAS = true;
                    } else if ((zr[0] == 0.0f) && (zr[1] == 1.0f) && (zr[2] == 0.0f)) {
                        orient[2] = FileInfoBase.ORI_P2A_TYPE;
                        haveZRAS = true;
                    } else if ((zr[0] == 0.0f) && (zr[1] == -1.0f) && (zr[2] == 0.0f)) {
                        orient[2] = FileInfoBase.ORI_A2P_TYPE;
                        haveZRAS = true;
                    } else {
                        raFile.close();
                        throw new IOException("Illegal z_ras values of " + zr[0] + " " + zr[1] + " " + zr[2]);
                    }
                } else {
                    raFile.close();
                    throw new IOException("z_ras has parseString with length = " + parseString.length);
                }
            }
            // Read in x dimension
            else if (parseString[0].equalsIgnoreCase("x")) {

                if (parseString.length == 2) {
                    imageExtents[0] = Integer.valueOf(parseString[1]).intValue();
                    haveX = true;
                } else {
                    raFile.close();
                    throw new IOException("x has parseString with length = " + parseString.length);
                }
            }
            // Read in y dimension
            else if (parseString[0].equalsIgnoreCase("y")) {

                if (parseString.length == 2) {
                    imageExtents[1] = Integer.valueOf(parseString[1]).intValue();
                    haveY = true;
                } else {
                    raFile.close();
                    throw new IOException("y has parseString with length = " + parseString.length);
                }
            }
            // Read in slice number of first data file
            else if (parseString[0].equalsIgnoreCase("imnr0")) {

                if (parseString.length == 2) {
                    imnr0 = Integer.valueOf(parseString[1]).intValue();
                    haveimnr0 = true;
                } else {
                    raFile.close();
                    throw new IOException("imnr0 has parseString with length = " + parseString.length);
                }
            }
            // Read in slice number of last data file
            else if (parseString[0].equalsIgnoreCase("imnr1")) {

                if (parseString.length == 2) {
                    imnr1 = Integer.valueOf(parseString[1]).intValue();
                    haveimnr1 = true;
                } else {
                    raFile.close();
                    throw new IOException("imnr1 has parseString with length = " + parseString.length);
                }
            }
            // Read in z resolution
            else if (parseString[0].equalsIgnoreCase("thick")) {

                if (parseString.length == 2) {
                    imgResols[2] = Float.valueOf(parseString[1]).floatValue(); // value in meters
                    haveThick = true;
                } else {
                    raFile.close();
                    throw new IOException("thick has parseString with length = " + parseString.length);
                }
            }
            // read in field of view
            else if (parseString[0].equalsIgnoreCase("fov")) {

                if (parseString.length == 2) {
                    fov = Float.valueOf(parseString[1]).floatValue();
                    fileInfo.setFOV(fov);
                } else {
                    raFile.close();
                    throw new IOException("fov has parseString with length = " + parseString.length);
                }
            }
            // Read in common value for x and y resolutions
            else if (parseString[0].equalsIgnoreCase("psiz")) {

                if (parseString.length == 2) {
                    imgResols[0] = Float.valueOf(parseString[1]).floatValue(); // value in meters
                    imgResols[1] = imgResols[0];
                    havePSize = true;
                } else {
                    raFile.close();
                    throw new IOException("psiz has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("tr")) {

                if (parseString.length == 2) {
                    tr = Float.valueOf(parseString[1]).floatValue();
                    fileInfo.setTR(tr);
                } else {
                    raFile.close();
                    throw new IOException("tr has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("te")) {

                if (parseString.length == 2) {
                    te = Float.valueOf(parseString[1]).floatValue();
                    fileInfo.setTE(te);
                } else {
                    raFile.close();
                    throw new IOException("te has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("ti")) {

                if (parseString.length == 2) {
                    ti = Float.valueOf(parseString[1]).floatValue();
                    fileInfo.setTI(ti);
                } else {
                    raFile.close();
                    throw new IOException("ti has parseString with length = " + parseString.length);
                }
            }

            lineString = readLine();
        } // while (lineString != null)

        raFile.close();

        if ((haveXRAS) && (haveYRAS) && (haveZRAS)) {

            // check that all directions are accounted for - one patient x-axis, one patient y-axis, and one patient
            // z-axis
            if (!((orient[0] == FileInfoBase.ORI_R2L_TYPE) || (orient[0] == FileInfoBase.ORI_L2R_TYPE) ||
                      (orient[1] == FileInfoBase.ORI_R2L_TYPE) || (orient[1] == FileInfoBase.ORI_L2R_TYPE) ||
                      (orient[2] == FileInfoBase.ORI_R2L_TYPE) || (orient[2] == FileInfoBase.ORI_L2R_TYPE))) {
                throw new IOException("orient does not account for all directions");
            }

            if (!((orient[0] == FileInfoBase.ORI_P2A_TYPE) || (orient[0] == FileInfoBase.ORI_A2P_TYPE) ||
                      (orient[1] == FileInfoBase.ORI_P2A_TYPE) || (orient[1] == FileInfoBase.ORI_A2P_TYPE) ||
                      (orient[2] == FileInfoBase.ORI_P2A_TYPE) || (orient[2] == FileInfoBase.ORI_A2P_TYPE))) {
                throw new IOException("orient does not account for all directions");
            }

            if (!((orient[0] == FileInfoBase.ORI_I2S_TYPE) || (orient[0] == FileInfoBase.ORI_S2I_TYPE) ||
                      (orient[1] == FileInfoBase.ORI_I2S_TYPE) || (orient[1] == FileInfoBase.ORI_S2I_TYPE) ||
                      (orient[2] == FileInfoBase.ORI_I2S_TYPE) || (orient[2] == FileInfoBase.ORI_S2I_TYPE))) {
                throw new IOException("orient does not account for all directions");
            }

            if ((orient[2] == FileInfoBase.ORI_R2L_TYPE) || (orient[2] == FileInfoBase.ORI_L2R_TYPE)) {
                imageOrientation = FileInfoBase.SAGITTAL;
            } else if ((orient[2] == FileInfoBase.ORI_A2P_TYPE) || (orient[2] == FileInfoBase.ORI_P2A_TYPE)) {
                imageOrientation = FileInfoBase.CORONAL;
            } else {
                imageOrientation = FileInfoBase.AXIAL;
            }

        } // if ((haveXRAS) && (haveYRAS) && (haveZRAS))
        else {
            orient[0] = FileInfoBase.ORI_R2L_TYPE;
            orient[1] = FileInfoBase.ORI_S2I_TYPE;
            orient[2] = FileInfoBase.ORI_P2A_TYPE;
            imageOrientation = FileInfoBase.CORONAL;
        }

        fileInfo.setAxisOrientation(orient);
        fileInfo.setImageOrientation(imageOrientation);

        if (!haveX) {
            imageExtents[0] = 256;
        }

        if (!haveY) {
            imageExtents[1] = 256;
        }

        if ((haveimnr0) && (haveimnr1)) {
            imageExtents[2] = imnr1 - imnr0 + 1;
        } else {
            imageExtents[2] = 256;
        }

        fileInfo.setExtents(imageExtents);

        if (!haveThick) {
            imgResols[2] = 0.001f; // value in meters
        }

        if (!havePSize) {
            imgResols[0] = 0.001f;
            imgResols[1] = 0.001f;
        }

        fileInfo.setResolutions(imgResols);
        fileInfo.setUnitsOfMeasure(Unit.METERS.getLegacyNum(), 0);
        fileInfo.setUnitsOfMeasure(Unit.METERS.getLegacyNum(), 1);
        fileInfo.setUnitsOfMeasure(Unit.METERS.getLegacyNum(), 2);
        fileInfo.setDataType(ModelImage.UBYTE);
        fileInfo.setMultiFile(true);

    }

    /**
     * Accessor to set the file name (used for reading COR multiFile).
     *
     * @param  fName  file name of image to read.
     */
    public void setFileName(String fName) {
        fileName = fName;
    }

    /**
     * Writes a COR format type image.
     *
     * @param      image    Image model of data to write.
     * @param      options  options such as starting and ending slices and times
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        int sBegin; // first z slice to write
        int sEnd; // last z slice to write
        byte[] line;
        byte[] beginBytes;
        byte[] endBytes = null;
        byte[] xBytes;
        byte[] yBytes;
        int i;
        float[] meterResols = new float[3];
        byte[] thickBytes;
        byte[] psizBytes;
        int lastPeriod;
        int lastHyphen;
        String fileBase;
        String fileExt;
        int z;
        int sliceSize;
        byte[] byteBuffer;
        int numberSlices;
        int count;
        int fileNumber;
        int digitNumber;
        String zeroLead;
        int numberZeros;

        lastPeriod = fileName.lastIndexOf(".");

        if (fileName.charAt(lastPeriod - 1) != '-') {
            fileName = fileName.substring(0, lastPeriod) + "-" + fileName.substring(lastPeriod);
        }

        lastHyphen = fileName.lastIndexOf("-");
        fileBase = fileName.substring(0, lastHyphen + 1);

        if (image.getNDims() >= 3) {
            sBegin = options.getBeginSlice();
            sEnd = options.getEndSlice();
        } else {
            sBegin = 0;
            sEnd = 0;
        }

        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "rw");

        beginBytes = Integer.toString(options.getStartNumber()).getBytes();

        line = new byte[7 + beginBytes.length];
        line[0] = 105; // i
        line[1] = 109; // m
        line[2] = 110; // n
        line[3] = 114; // r
        line[4] = 48; // 0
        line[5] = 32; // space

        for (i = 0; i < beginBytes.length; i++) {
            line[6 + i] = beginBytes[i];
        }

        line[6 + beginBytes.length] = 10; // new line

        // Write the number attached to the first COR data file
        raFile.write(line);

        endBytes = Integer.toString(options.getStartNumber() + sEnd - sBegin).getBytes();

        line = new byte[7 + endBytes.length];
        line[0] = 105; // i
        line[1] = 109; // m
        line[2] = 110; // n
        line[3] = 114; // r
        line[4] = 49; // 1
        line[5] = 32; // space

        for (i = 0; i < endBytes.length; i++) {
            line[6 + i] = endBytes[i];
        }

        line[6 + endBytes.length] = 10; // new line

        // Write the number attached to the last COR data file
        raFile.write(line);

        line = new byte[8];
        line[0] = 112; // p
        line[1] = 116; // t
        line[2] = 121; // y
        line[3] = 112; // p
        line[4] = 101; // e
        line[5] = 32; // space
        line[6] = 50; // 2
        line[7] = 10; // new line

        // Write that the file is unsigned byte.
        raFile.write(line);

        xBytes = Integer.toString(image.getExtents()[0]).getBytes();
        line = new byte[3 + xBytes.length];
        line[0] = 120; // x
        line[1] = 32; // space

        for (i = 0; i < xBytes.length; i++) {
            line[2 + i] = xBytes[i];
        }

        line[2 + xBytes.length] = 10; // new line

        // Write the x dimension
        raFile.write(line);

        yBytes = Integer.toString(image.getExtents()[1]).getBytes();
        line = new byte[3 + yBytes.length];
        line[0] = 121; // y
        line[1] = 32; // space

        for (i = 0; i < yBytes.length; i++) {
            line[2 + i] = yBytes[i];
        }

        line[2 + yBytes.length] = 10; // new line

        // Write the y dimension
        raFile.write(line);

        // COR file format requires resolutions to be in meters
        // Change other units to meters
        for (i = 0; i < image.getNDims(); i++) {

            switch (Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(i))) {

                case METERS:
                    meterResols[i] = image.getFileInfo()[0].getResolutions()[i];
                    break;

                case UNKNOWN_MEASURE:
                    meterResols[i] = image.getFileInfo()[0].getResolutions()[i];
                    break;

                case CENTIMETERS:
                    meterResols[i] = 0.01f * image.getFileInfo()[0].getResolutions()[i];
                    break;

                case MILLIMETERS:
                    meterResols[i] = 0.001f * image.getFileInfo()[0].getResolutions()[i];
                    break;

                case INCHES:
                    meterResols[i] = 0.0254f * image.getFileInfo()[0].getResolutions()[i];
                    break;
                    
                case MILS:
                    meterResols[i] = 2.54e-5f * image.getFileInfo()[0].getResolutions()[i];
                    break;

                case MICROMETERS:
                    meterResols[i] = 1.0e-6f * image.getFileInfo()[0].getResolutions()[i];
                    break;

                case NANOMETERS:
                    meterResols[i] = 1.0e-9f * image.getFileInfo()[0].getResolutions()[i];
                    break;

                case ANGSTROMS:
                    meterResols[i] = 1.0e-10f * image.getFileInfo()[0].getResolutions()[i];
                    break;

                default:
                    meterResols[i] = image.getFileInfo()[0].getResolutions()[i];

            }
        }

        thickBytes = Float.toString(meterResols[2]).getBytes();
        line = new byte[7 + thickBytes.length];
        line[0] = 116; // t
        line[1] = 104; // h
        line[2] = 105; // i
        line[3] = 99; // c
        line[4] = 107; // k
        line[5] = 32; // space

        for (i = 0; i < thickBytes.length; i++) {
            line[6 + i] = thickBytes[i];
        }

        line[6 + thickBytes.length] = 10; // new line

        // Write the z resolution
        raFile.write(line);

        // Note that the COR file assumes x resolution = y resolution
        psizBytes = Float.toString(meterResols[0]).getBytes();
        line = new byte[6 + psizBytes.length];
        line[0] = 112; // p
        line[1] = 115; // s
        line[2] = 105; // i
        line[3] = 122; // z
        line[4] = 32; // space

        for (i = 0; i < psizBytes.length; i++) {
            line[5 + i] = psizBytes[i];
        }

        line[5 + psizBytes.length] = 10; // new line

        // Write the x = y resolution
        raFile.write(line);

        line = new byte[16];
        line[0] = 114; // r
        line[1] = 97; // a
        line[2] = 115; // s
        line[3] = 95; // _
        line[4] = 103; // g
        line[5] = 111; // o
        line[6] = 111; // o
        line[7] = 100; // d
        line[8] = 95; // _
        line[9] = 102; // f
        line[10] = 108; // l
        line[11] = 97; // a
        line[12] = 103; // g
        line[13] = 32; // space

        // If all 3 axis orientations are known, ras_good_file is 1.
        // Otherwise, it is 0.
        if ((image.getNDims() == 3) &&
                (image.getFileInfo(0).getAxisOrientation()[0] != FileInfoBase.ORI_UNKNOWN_TYPE) &&
                (image.getFileInfo(0).getAxisOrientation()[1] != FileInfoBase.ORI_UNKNOWN_TYPE) &&
                (image.getFileInfo(0).getAxisOrientation()[2] != FileInfoBase.ORI_UNKNOWN_TYPE)) {
            line[14] = 49; // 1
        } else {
            line[14] = 48; // 0
        }

        line[15] = 10; // new line
        raFile.write(line);

        // Write the axis orientations
        for (i = 0; i < image.getNDims(); i++) {

            if (image.getFileInfo(0).getAxisOrientation()[i] != FileInfoBase.ORI_UNKNOWN_TYPE) {
                line = new byte[19];

                switch (i) {

                    case 0:
                        line[0] = 120; // x
                        break;

                    case 1:
                        line[0] = 121; // y
                        break;

                    case 2:
                        line[0] = 122; // z
                        break;

                    default:
                        line[0] = 120;
                }

                line[1] = 95; // _
                line[2] = 114; // r
                line[3] = 97; // a
                line[4] = 115; // s
                line[5] = 32; // space

                switch (image.getFileInfo(0).getAxisOrientation()[i]) {

                    case FileInfoBase.ORI_R2L_TYPE:
                        line[6] = 45; // -
                        line[7] = 49; // 1
                        line[8] = 46; // .
                        line[9] = 48; // 0
                        line[10] = 32; // space
                        line[11] = 48; // 0
                        line[12] = 46; // .
                        line[13] = 48; // 0
                        line[14] = 32; // space
                        line[15] = 48; // 0
                        line[16] = 46; // .
                        line[17] = 48; // 0
                        line[18] = 10; // new line
                        break;

                    case FileInfoBase.ORI_L2R_TYPE:
                        line[6] = 49; // 1
                        line[7] = 46; // .
                        line[8] = 48; // 0
                        line[9] = 32; // space
                        line[10] = 48; // 0
                        line[11] = 46; // .
                        line[12] = 48; // 0
                        line[13] = 32; // space
                        line[14] = 48; // 0
                        line[15] = 46; // .
                        line[16] = 48; // 0
                        line[17] = 48; // 0
                        line[18] = 10; // new line
                        break;

                    case FileInfoBase.ORI_S2I_TYPE:
                        line[6] = 48; // 0
                        line[7] = 46; // .
                        line[8] = 48; // 0
                        line[9] = 32; // space
                        line[10] = 48; // 0
                        line[11] = 46; // .
                        line[12] = 48; // 0
                        line[13] = 32; // space
                        line[14] = 45; // -
                        line[15] = 49; // 1
                        line[16] = 46; // .
                        line[17] = 48; // 0
                        line[18] = 10; // new line
                        break;

                    case FileInfoBase.ORI_I2S_TYPE:
                        line[6] = 48; // 0
                        line[7] = 46; // .
                        line[8] = 48; // 0
                        line[9] = 32; // space
                        line[10] = 48; // 0
                        line[11] = 46; // .
                        line[12] = 48; // 0
                        line[13] = 32; // space
                        line[14] = 49; // 1
                        line[15] = 46; // .
                        line[16] = 48; // 0
                        line[17] = 48; // 0
                        line[18] = 10; // new line
                        break;

                    case FileInfoBase.ORI_P2A_TYPE:
                        line[6] = 48; // 0
                        line[7] = 46; // .
                        line[8] = 48; // 0
                        line[9] = 32; // space
                        line[10] = 49; // 1
                        line[11] = 46; // .
                        line[12] = 48; // 0
                        line[13] = 32; // space
                        line[14] = 48; // 0
                        line[15] = 46; // .
                        line[16] = 48; // 0
                        line[17] = 48; // 0
                        line[18] = 10; // new line
                        break;

                    case FileInfoBase.ORI_A2P_TYPE:
                        line[6] = 48; // 0
                        line[7] = 46; // .
                        line[8] = 48; // 0
                        line[9] = 32; // space
                        line[10] = 45; // -
                        line[11] = 49; // 1
                        line[12] = 46; // .
                        line[13] = 48; // 0
                        line[14] = 32; // space
                        line[15] = 48; // 0
                        line[16] = 46; // .
                        line[17] = 48; // 0
                        line[18] = 10; // new line
                        break;

                    default:
                        line[6] = 45; // -
                        line[7] = 49; // 1
                        line[8] = 46; // .
                        line[9] = 48; // 0
                        line[10] = 32; // space
                        line[11] = 48; // 0
                        line[12] = 46; // .
                        line[13] = 48; // 0
                        line[14] = 32; // space
                        line[15] = 48; // 0
                        line[16] = 46; // .
                        line[17] = 48; // 0
                        line[18] = 10; // new line
                }

                raFile.write(line);
            }
        }

        line = new byte[18];
        line[0] = 99; // c
        line[1] = 95; // _
        line[2] = 114; // r
        line[3] = 97; // a
        line[4] = 115; // s
        line[5] = 32; // space
        line[6] = 48; // 0
        line[7] = 46; // .
        line[8] = 48; // 0
        line[9] = 32; // space
        line[10] = 48; // 0
        line[11] = 46; // .
        line[12] = 48; // 0
        line[13] = 32; // space
        line[14] = 48; // 0
        line[15] = 46; // .
        line[16] = 48; // 0
        line[17] = 10; // new line

        // Write the center of the file as 0.0,0.0,0.0 - the default value.
        raFile.write(line);
        raFile.close();

        sliceSize = image.getExtents()[0] * image.getExtents()[1];
        byteBuffer = new byte[sliceSize];
        numberSlices = sEnd - sBegin + 1;
        count = 0;
        fileNumber = options.getStartNumber();
        digitNumber = options.getDigitNumber();

        // Stored as 8-bit unsigned byte
        for (z = sBegin; z <= sEnd; z++) {
            fireProgressStateChanged((100 * count++) / numberSlices);
            fileExt = Integer.toString(fileNumber++);

            // Pad file numbers with leading zeros to the prescribed
            // number of digits.
            if (fileExt.length() < digitNumber) {
                numberZeros = digitNumber - fileExt.length();
                zeroLead = new String("0");

                for (i = 0; i < (numberZeros - 1); i++) {
                    zeroLead = zeroLead + "0";
                }

                fileExt = zeroLead + fileExt;
            }

            file = new File(fileDir + fileBase + fileExt);
            raFile = new RandomAccessFile(file, "rw");
            image.exportSliceXY(z, byteBuffer);
            raFile.write(byteBuffer);
            raFile.close();
        }


    }

    /**
     * DOCUMENT ME!
     *
     * @param   inString  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String[] parse(String inString) {
        String[] tmpString = new String[4];
        String[] outString;
        int i;
        int sNum = 0;
        int firstEl = 0;

        // int k;
        for (i = 0; i < inString.length(); i++) {

            if (inString.charAt(i) <= 0x20) {

                if (firstEl != i) {
                    tmpString[sNum++] = inString.substring(firstEl, i);
                }

                firstEl = i + 1;
            }
        }

        if (firstEl != i) {
            tmpString[sNum++] = inString.substring(firstEl, i);
        }

        if (sNum == 0) {
            outString = new String[1];
            outString[0] = inString;
        } else {
            outString = new String[sNum];

            for (i = 0; i < (sNum); i++) {
                outString[i] = tmpString[i];
            }
        }

        return outString;

    }

    /**
     * Reads lines of the file until a nonnull String results or the end of the file is reached.
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private String readLine() throws IOException {
        String tempString = null;
        // int     index;

        while ((tempString == null) && (raFile.getFilePointer() < (raFile.length() - 1)) && (!foundEOF)) {

            try {
                tempString = raFile.readLine();
            } catch (EOFException error) {
                tempString = null;
                foundEOF = true;
            } catch (IOException error) {
                throw (error);
            }


            if (tempString != null) {

                if (tempString.length() == 0) {
                    tempString = null;
                }
            }
        } // while

        return tempString;
    }

}
