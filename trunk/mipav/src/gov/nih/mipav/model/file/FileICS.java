package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.zip.*;


/**
 * Reference: 1. Proposed Standard for Image Cytometry Data Files by Philip Dean, Laura Mascio, David Ow, Damir Sudar,
 * and James Mullikin, Cytometry, 11, pp. 561- 569, 1990.
 *
 * <p>2. libics v.1.3 Online Documentation, copyright 2000-2002 by Cris Luengo. http://www.ph.tn.tudelft.nl/~cris/libics
 * </p>
 *
 * @author  William Gandler
 * @see     FileIO
 */

public class FileICS extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int RGB_FIRST = 1;

    /** DOCUMENT ME! */
    private static final int RGB_BETWEEN_SPACEANDTIME = 2;

    /** DOCUMENT ME! */
    private static final int RGB_LAST = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String[] actualHistory = null;

    /** DOCUMENT ME! */
    private int bufferSize;

    /** DOCUMENT ME! */
    private String captureVersion = null;

    /** DOCUMENT ME! */
    private String category;

    /** DOCUMENT ME! */
    private String[] channels = null;

    /** DOCUMENT ME! */
    private int colorSpacing = RGB_LAST;

    /** DOCUMENT ME! */
    private String compression = new String("uncompressed");

    /** DOCUMENT ME! */
    private String dataFileName = null;

    /** DOCUMENT ME! */
    private long dataOffset = 0L;

    /** DOCUMENT ME! */
    private String dataSetFileName;

    /** DOCUMENT ME! */
    private int dataType;

    /** DOCUMENT ME! */
    private int endBytes = 0;

    /** DOCUMENT ME! */
    private int endFloats = 0;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private int endShorts = 0;

    /** DOCUMENT ME! */
    private boolean exchangeXY = false;

    /** DOCUMENT ME! */
    private String[] exPhotonCnt = null;

    /** DOCUMENT ME! */
    private byte fieldSeparator;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;


    /** DOCUMENT ME! */
    private FileInfoICS fileInfo;
    
    private FileInfoICS fileInfoCopy;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private String filterExposureTimeX = null;

    /** DOCUMENT ME! */
    private String[] filterFluorophoreX = null;

    /** DOCUMENT ME! */
    private String format = new String("integer");

    /** DOCUMENT ME! */
    private String headerFileName;

    /** DOCUMENT ME! */
    private String[] history = new String[300];

    /** DOCUMENT ME! */
    private int historyNumber = 0;

    /** DOCUMENT ME! */
    private ModelImage image;


    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgBuffer2 = null;

    /** DOCUMENT ME! */
    private float[] imgBufferI = null;

    /** DOCUMENT ME! */
    private float[] imgBufferI2 = null;

    /** DOCUMENT ME! */
    private double[] imgDBuffer = null;

    /** DOCUMENT ME! */
    private double[] imgDBuffer2 = null;

    /** DOCUMENT ME! */
    private double[] imgDBufferI = null;

    /** DOCUMENT ME! */
    private double[] imgDBufferI2 = null;

    /** DOCUMENT ME! */
    private int[] imgExtents;

    /** DOCUMENT ME! */
    private long[] imgLBuffer = null;

    /** DOCUMENT ME! */
    private long[] imgLBuffer2 = null;

    /** DOCUMENT ME! */
    private float[] imgResols;

    /** DOCUMENT ME! */
    private boolean invertY = false;

    /** DOCUMENT ME! */
    private String[] labels = null;

    /** DOCUMENT ME! */
    private String[] lambdaEm = null;

    /** DOCUMENT ME! */
    private String[] lambdaEx = null;

    /** DOCUMENT ME! */
    private ModelLUT LUT;

    /** DOCUMENT ME! */
    private String[] mapchannel = null;

    /** DOCUMENT ME! */
    private int nDims = 0;

    /** DOCUMENT ME! */
    private String[] numAperture = null;


    /** DOCUMENT ME! */
    private int numberSlices; // zDim for 3D and zDim * tDim for 4D

    /** DOCUMENT ME! */
    private int numberSpaceSlices = 1; // zDim if x, y, and z are
                                       // present; 1 otherwise

    /** DOCUMENT ME! */
    private int numColors = 1;

    /** DOCUMENT ME! */
    private int numValues;

    /** DOCUMENT ME! */
    private String[] order;

    /** DOCUMENT ME! */
    private float[] origin = null;

    /** DOCUMENT ME! */
    private int parameters = -1;

    /** DOCUMENT ME! */
    private String[] pinholeRadius = null;

    /** DOCUMENT ME! */
    private String plateChamberID = null;

    /** DOCUMENT ME! */
    private String plateVesselID = null;

    /** DOCUMENT ME! */
    private String[] probe = null;

    /** DOCUMENT ME! */
    private String[] refrInxLensMedium = null;

    /** DOCUMENT ME! */
    private String[] refrInxMedium = null;

    /** DOCUMENT ME! */
    private int rgbPos = -1;

    /** DOCUMENT ME! */
    private float[] scale = null;

    /** DOCUMENT ME! */
    private String scilType = null;

    /** DOCUMENT ME! */
    private String sensorModel = null;

    /** DOCUMENT ME! */
    private String sensorType = null;

    /** DOCUMENT ME! */
    private String sign = null;

    @SuppressWarnings("unused")
    private int significant_bits = -1;

    /** DOCUMENT ME! */
    private int[] sizes;

    /** DOCUMENT ME! */
    private String[] specimen = null;

    /** DOCUMENT ME! */
    private String[] specimenSpecies = null;

    /** DOCUMENT ME! */
    private float[] startLocations;

    /** DOCUMENT ME! */
    private String subcategory;

    /** DOCUMENT ME! */
    private int tPos = -1;

    /** DOCUMENT ME! */
    private String[] units = null;

    /** DOCUMENT ME! */
    private int[] unitsOfMeasure = null;

    /** DOCUMENT ME! */
    private String[] unitsStr = null;

    /** DOCUMENT ME! */
    private boolean useGZIP = false;

    /** DOCUMENT ME! */
    private String[] values = new String[10];

    /** DOCUMENT ME! */
    private String version;

    /** DOCUMENT ME! */
    private int xPos = -1;

    /** DOCUMENT ME! */
    private int yPos = -1;

    /** DOCUMENT ME! */
    private int zPos = -1;
    
    private boolean hasRGB = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * ICS reader constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileICS(String fileName, String fileDir) throws IOException {

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
        fileInfoCopy = null;
        file = null;
        image = null;
        actualHistory = null;
        captureVersion = null;
        category = null;
        if (channels != null) {
            for (i = 0; i < channels.length; i++) {
                channels[i] = null;
            }
            channels = null;
        }
        compression = null;
        dataFileName = null;
        dataSetFileName = null;
        if (exPhotonCnt != null) {
            for (i = 0; i < exPhotonCnt.length; i++) {
                exPhotonCnt[i] = null;
            }
            exPhotonCnt = null;
        }
        filterExposureTimeX = null;
        if (filterFluorophoreX != null) {
            for (i = 0; i < filterFluorophoreX.length; i++) {
                filterFluorophoreX[i] = null;
            }
            filterFluorophoreX = null;
        }
        format = null;
        headerFileName = null;
        if (history != null) {
            for (i = 0; i < history.length; i++) {
                history[i] = null;
            }
            history = null;
        }
        imgBuffer = null;
        imgBuffer2 = null;
        imgBufferI = null;
        imgBufferI2 = null;
        imgDBuffer = null;
        imgDBuffer2 = null;
        imgDBufferI = null;
        imgDBufferI2 = null;
        imgExtents = null;
        imgLBuffer = null;
        imgLBuffer2 = null;
        imgResols = null;
        if (labels != null) {
            for (i = 0; i < labels.length; i++) {
                labels[i] = null;
            }
            labels = null;
        }
        if (lambdaEm != null) {
            for (i = 0; i < lambdaEm.length; i++) {
                lambdaEm[i] = null;
            }
            lambdaEm = null;
        }
        if (lambdaEx != null) {
            for (i = 0; i < lambdaEx.length; i++) {
                lambdaEx[i] = null;
            }
            lambdaEx = null;
        }
        LUT = null;
        if (mapchannel != null) {
            for (i = 0; i < mapchannel.length; i++) {
                mapchannel[i] = null;
            }
            mapchannel = null;
        }
        if (numAperture != null) {
            for (i = 0; i < numAperture.length; i++) {
                numAperture[i] = null;
            }
            numAperture = null;
        }
        if (order != null) {
            for (i = 0; i < order.length; i++) {
                order[i] = null;
            }
            order = null;
        }
        origin = null;
        if (pinholeRadius != null) {
            for (i = 0; i < pinholeRadius.length; i++) {
                pinholeRadius[i] = null;
            }
            pinholeRadius = null;
        }
        plateChamberID = null;
        plateVesselID = null;
        if (probe != null) {
            for (i = 0; i < probe.length; i++) {
                probe[i] = null;
            }
            probe = null;
        }
        if (refrInxLensMedium != null) {
            for (i = 0; i < refrInxLensMedium.length; i++) {
                refrInxLensMedium[i] = null;
            }
            refrInxLensMedium = null;
        }
        if (refrInxMedium != null) {
            for (i = 0; i < refrInxMedium.length; i++) {
                refrInxMedium[i] = null;
            }
            refrInxMedium = null;
        }
        scale = null;
        scilType = null;
        sensorModel = null;
        sensorType = null;
        sign = null;
        sizes = null;
        if (specimen != null) {
            for (i = 0; i < specimen.length; i++) {
                specimen[i] = null;
            }
            specimen = null;
        }
        if (specimenSpecies != null) {
            for (i = 0; i < specimenSpecies.length; i++) {
                specimenSpecies[i] = null;
            }
            specimenSpecies = null;
        }
        startLocations = null;
        subcategory = null;
        if (units != null) {
            for (i = 0; i < units.length; i++) {
                units[i] = null;
            }
            units = null;
        }
        unitsOfMeasure = null;
        if (unitsStr != null) {
            for (i = 0; i < unitsStr.length; i++) {
                unitsStr[i] = null;
            }
            unitsStr = null;
        }
        if (values != null) {
            for (i = 0; i < values.length; i++) {
                values[i] = null;
            }
            values = null;
        }
        version = null;
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * getFileInfo - accessor that returns the file info.
     *
     * @return  FileInfoBase containing the file info
     */
    public FileInfoBase getFileInfo() {
        return fileInfo;
    }


    /**
     * getImageBuffer - accessor that returns the image buffer.
     *
     * @return  buffer of image.
     */
    public float[] getImageBuffer() {
        return imgBuffer;
    }


    /**
     * getModelLUT - returns LUT if defined.
     *
     * @return  the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }


    /**
     * readImage.
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage() throws IOException {
        int i;
        int s;
        int x, y;
        FileInputStream fis;

        readHeader();
        s = fileName.lastIndexOf(".");

        if (dataFileName == null) {
            dataFileName = fileName.substring(0, s + 1) + "ids";
        }

        file = new File(fileDir + dataFileName);

        if (useGZIP) {
            int totalBytesRead = 0;

            fireProgressStateChanged("Uncompressing GZIP file ...");
            fis = new FileInputStream(file);
            fis.skip(dataOffset);

            GZIPInputStream gzin = new GZIPInputStream(new BufferedInputStream(fis));
            String uncompressedName = fileDir + fileName.substring(0, s) + "uncompressed.ids";
            FileOutputStream out = new FileOutputStream(uncompressedName);
            byte[] buffer = new byte[256];

            while (true) {
                int bytesRead = gzin.read(buffer);

                if (bytesRead == -1) {
                    break;
                }

                totalBytesRead += bytesRead;
                out.write(buffer, 0, bytesRead);
            }

            out.close();
            file = new File(uncompressedName);
            fireProgressStateChanged("Loading ICS image(s)...");
            dataOffset = 0L;
        } // if (useGZIP)

        raFile = new RandomAccessFile(file, "r");
        raFile.seek(dataOffset);

        image = new ModelImage(dataType, imgExtents, fileInfo.getFileName());

        if (imgExtents.length == 2) {
            numberSlices = 1;
        } else if (imgExtents.length == 3) {
            numberSlices = imgExtents[2];
        } else {
            numberSlices = imgExtents[2] * imgExtents[3];
        }

        if ((xPos >= 0) && (yPos >= 0) && (zPos >= 0)) {
            numberSpaceSlices = imgExtents[2]; // zDim;
        }

        if ((dataType != ModelStorageBase.DOUBLE) && (dataType != ModelStorageBase.COMPLEX) &&
                (dataType != ModelStorageBase.DCOMPLEX) && (dataType != ModelStorageBase.UINTEGER) &&
                (dataType != ModelStorageBase.LONG)) {

            if ((dataType == ModelStorageBase.ARGB) || (dataType == ModelStorageBase.ARGB_USHORT) ||
                    (dataType == ModelStorageBase.ARGB_FLOAT)) {
                bufferSize = 4 * imgExtents[0] * imgExtents[1];
            } else {
                bufferSize = imgExtents[0] * imgExtents[1];
            }

            imgBuffer = new float[bufferSize];

            if (invertY || exchangeXY) {
                imgBuffer2 = new float[bufferSize];
            }

            for (i = 0; i < numberSlices; i++) {
                fileInfoCopy = (FileInfoICS)fileInfo.clone();
                image.setFileInfo(fileInfoCopy, i);
                readBuffer(i, imgBuffer);

                if (exchangeXY) {

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgBuffer2[x + (y * imgExtents[0])] = imgBuffer[y + (x * imgExtents[1])];
                        }
                    }

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgBuffer[x + (y * imgExtents[0])] = imgBuffer2[x + (y * imgExtents[0])];
                        }
                    }
                } // if (exchangeXY)

                if (invertY) {

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgBuffer2[x + (y * imgExtents[0])] = imgBuffer[x +
                                                                            ((imgExtents[1] - 1 - y) * imgExtents[0])];
                        }
                    }

                    image.importData(i * bufferSize, imgBuffer2, false);
                } // if (invertY)
                else {
                    image.importData(i * bufferSize, imgBuffer, false);
                }
            }
        } // if ((dataType != ModelStorageBase.DOUBLE) &&
          // (dataType != ModelStorageBase.COMPLEX) &&
          // (dataType != ModelStorageBase.DCOMPLEX) &&
          // (dataType != ModelStorageBase.UINTEGER) &&
          // (dataType != ModelStorageBase.LONG))
        else if ((dataType == ModelStorageBase.LONG) || (dataType == ModelStorageBase.UINTEGER)) {
            bufferSize = imgExtents[0] * imgExtents[1];
            imgLBuffer = new long[bufferSize];

            if (invertY || exchangeXY) {
                imgLBuffer2 = new long[bufferSize];
            }

            for (i = 0; i < numberSlices; i++) {
                fileInfoCopy = (FileInfoICS)fileInfo.clone();
                image.setFileInfo(fileInfoCopy, i);
                readLBuffer(i, imgLBuffer);

                if (exchangeXY) {

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgLBuffer2[x + (y * imgExtents[0])] = imgLBuffer[y + (x * imgExtents[1])];
                        }
                    }

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgLBuffer[x + (y * imgExtents[0])] = imgLBuffer2[x + (y * imgExtents[0])];
                        }
                    }
                } // if (exchangeXY)

                if (invertY) {

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgLBuffer2[x + (y * imgExtents[0])] = imgLBuffer[x +
                                                                              ((imgExtents[1] - 1 - y) * imgExtents[0])];
                        }
                    }

                    image.importData(i * bufferSize, imgLBuffer2, false);
                } // if (invertY)
                else {
                    image.importData(i * bufferSize, imgLBuffer, false);
                }
            }
        } // else if ((dataType == ModelStorageBase.LONG)||
          // (dataType == ModelStorageBase.UINTEGER))
        else if (dataType == ModelStorageBase.COMPLEX) {
            bufferSize = imgExtents[0] * imgExtents[1];
            imgBuffer = new float[bufferSize];
            imgBufferI = new float[bufferSize];

            if (invertY || exchangeXY) {
                imgBuffer2 = new float[bufferSize];
                imgBufferI2 = new float[bufferSize];
            }

            for (i = 0; i < numberSlices; i++) {
                fileInfoCopy = (FileInfoICS)fileInfo.clone();
                image.setFileInfo(fileInfoCopy, i);
                readComplexBuffer(i, imgBuffer, imgBufferI);

                if (exchangeXY) {

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgBuffer2[x + (y * imgExtents[0])] = imgBuffer[y + (x * imgExtents[1])];
                            imgBufferI2[x + (y * imgExtents[0])] = imgBufferI[y + (x * imgExtents[1])];
                        }
                    }

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgBuffer[x + (y * imgExtents[0])] = imgBuffer2[x + (y * imgExtents[0])];
                            imgBufferI[x + (y * imgExtents[0])] = imgBufferI2[x + (y * imgExtents[0])];
                        }
                    }
                } // if (exchangeXY)

                if (invertY) {

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgBuffer2[x + (y * imgExtents[0])] = imgBuffer[x +
                                                                            ((imgExtents[1] - 1 - y) * imgExtents[0])];
                            imgBufferI2[x + (y * imgExtents[0])] = imgBufferI[x +
                                                                              ((imgExtents[1] - 1 - y) * imgExtents[0])];
                        }
                    }

                    image.importComplexData(2 * i * bufferSize, imgBuffer2, imgBufferI2, false, Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
                } // if (invertY)
                else {
                    image.importComplexData(2 * i * bufferSize, imgBuffer, imgBufferI, false, Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
                }
            }
        } // else if (dataType == ModelStorageBase.COMPLEX)
        else if (dataType == ModelStorageBase.DCOMPLEX) {
            bufferSize = imgExtents[0] * imgExtents[1];
            imgDBuffer = new double[bufferSize];
            imgDBufferI = new double[bufferSize];

            if (invertY || exchangeXY) {
                imgDBuffer2 = new double[bufferSize];
                imgDBufferI2 = new double[bufferSize];
            }

            for (i = 0; i < numberSlices; i++) {
                fileInfoCopy = (FileInfoICS)fileInfo.clone();
                image.setFileInfo(fileInfoCopy, i);
                readDComplexBuffer(i, imgDBuffer, imgDBufferI);

                if (exchangeXY) {

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgDBuffer2[x + (y * imgExtents[0])] = imgDBuffer[y + (x * imgExtents[1])];
                            imgDBufferI2[x + (y * imgExtents[0])] = imgDBufferI[y + (x * imgExtents[1])];
                        }
                    }

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgDBuffer[x + (y * imgExtents[0])] = imgDBuffer2[x + (y * imgExtents[0])];
                            imgDBufferI[x + (y * imgExtents[0])] = imgDBufferI2[x + (y * imgExtents[0])];
                        }
                    }
                } // if (exchangeXY)

                if (invertY) {

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgDBuffer2[x + (y * imgExtents[0])] = imgDBuffer[x +
                                                                              ((imgExtents[1] - 1 - y) * imgExtents[0])];
                            imgDBufferI2[x + (y * imgExtents[0])] = imgDBufferI[x +
                                                                                ((imgExtents[1] - 1 - y) *
                                                                                     imgExtents[0])];
                        }
                    }

                    image.importDComplexData(2 * i * bufferSize, imgDBuffer2, imgDBufferI2, false, true);
                } // if (invertY)
                else {
                    image.importDComplexData(2 * i * bufferSize, imgDBuffer, imgDBufferI, false, true);
                }
            }
        } // else if (dataType == ModelStorageBase.DCOMPLEX)
        else { // dataType == ModelStorageBase.DOUBLE
            bufferSize = imgExtents[0] * imgExtents[1];
            imgDBuffer = new double[bufferSize];

            if (invertY || exchangeXY) {
                imgDBuffer2 = new double[bufferSize];
            }

            for (i = 0; i < numberSlices; i++) {
                fileInfoCopy = (FileInfoICS)fileInfo.clone();
                image.setFileInfo(fileInfoCopy, i);
                readDBuffer(i, imgDBuffer);

                if (exchangeXY) {

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgDBuffer2[x + (y * imgExtents[0])] = imgDBuffer[y + (x * imgExtents[1])];
                        }
                    }

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgDBuffer[x + (y * imgExtents[0])] = imgDBuffer2[x + (y * imgExtents[0])];
                        }
                    }
                } // if (exchangeXY)

                if (invertY) {

                    for (x = 0; x < imgExtents[0]; x++) {

                        for (y = 0; y < imgExtents[1]; y++) {
                            imgDBuffer2[x + (y * imgExtents[0])] = imgDBuffer[x +
                                                                              ((imgExtents[1] - 1 - y) * imgExtents[0])];
                        }
                    }

                    image.importData(i * bufferSize, imgDBuffer2, false);
                } // if (invertY)
                else {
                    image.importData(i * bufferSize, imgDBuffer, false);
                }
            }
        } // else dataType == ModelStorageBase.DOUBLE

        raFile.close();


        return image;
    }

    /**
     * Writes an ICS format type image.
     *
     * @param      image    Image model of data to write.
     * @param      options  options such as starting and ending slices and times
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        String fileHeaderName;
        String fileDataName;
        int zDim;
        int bitSize;
        boolean haveRed = false;
        boolean haveGreen = false;
        @SuppressWarnings("unused")
        boolean haveBlue = false;
        float[] resols;
        float[] startLocation;
        int[] units;
        String lineString;
        byte[] line;
        int i, j;
        int lastPeriod;
        int t, z;
        int sliceSize;
        byte[] byteBuffer;
        short[] shortBuffer;
        int[] intBuffer;
        long[] longBuffer;
        float[] floatBuffer;
        float[] floatBufferI;
        double[] doubleBuffer;
        double[] doubleBufferI;
        int numberSlices;
        int count;
        int zBegin, zEnd;
        int tBegin, tEnd;
        int tmpInt;
        long tmpLong;

        lastPeriod = fileName.lastIndexOf(".");
        fileHeaderName = fileName.substring(0, lastPeriod + 1) + "ICS";
        fileDataName = fileName.substring(0, lastPeriod + 1) + "IDS";

        zBegin = options.getBeginSlice();
        zEnd = options.getEndSlice();

        if (image.getNDims() == 4) {
            tBegin = options.getBeginTime();
            tEnd = options.getEndTime();
        } else {
            tBegin = 0;
            tEnd = 0;
        }

        file = new File(fileDir + fileHeaderName);
        raFile = new RandomAccessFile(file, "rw");

        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        raFile.setLength(0);

        // Write first line with field separator character followed by
        // line separator character
        lineString = new String("\t\r\n");
        line = lineString.getBytes();
        raFile.write(line);

        // Write the second line with the version number of the ics standard
        // used to write the data file
        lineString = new String("ics_version\t1.0\r\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("filename\t") + fileName.substring(0, lastPeriod) + "\r\n";
        line = lineString.getBytes();
        raFile.write(line);

        parameters = image.getNDims() + 1;

        if (image.isColorImage()) {
            parameters++;
        }

        lineString = new String("layout\tparameters\t") + Integer.toString(parameters) + "\r\n";
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("layout\torder\tbits\tx\ty");

        if (image.getNDims() >= 3) {
            lineString = lineString + "\tz";
        }

        if (image.getNDims() >= 4) {
            lineString = lineString + "\tt";
        }

        if (image.isColorImage()) {
            lineString = lineString + "\tch";
        }

        lineString = lineString + "\r\n";
        line = lineString.getBytes();
        raFile.write(line);

        switch (image.getType()) {

            case ModelStorageBase.BYTE:
            case ModelStorageBase.UBYTE:
            case ModelStorageBase.ARGB:
                bitSize = 8;
                break;

            case ModelStorageBase.SHORT:
            case ModelStorageBase.USHORT:
            case ModelStorageBase.ARGB_USHORT:
                bitSize = 16;
                break;

            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
            case ModelStorageBase.FLOAT:
            case ModelStorageBase.ARGB_FLOAT:
                bitSize = 32;
                break;

            case ModelStorageBase.LONG:
            case ModelStorageBase.DOUBLE:
            case ModelStorageBase.COMPLEX:
                bitSize = 64;
                break;

            case ModelStorageBase.DCOMPLEX:
                bitSize = 128;
                break;

            default:
                bitSize = 8;
        }

        lineString = new String("layout\tsizes\t") + Integer.toString(bitSize) + "\t" +
                     Integer.toString(image.getExtents()[0]) + "\t" + Integer.toString(image.getExtents()[1]);

        if (image.getNDims() >= 3) {
            lineString = lineString + "\t" + Integer.toString(image.getExtents()[2]);
        }

        if (image.getNDims() >= 4) {
            lineString = lineString + "\t" + Integer.toString(image.getExtents()[3]);
        }

        if (image.isColorImage()) {
            image.calcMinMax();
            numColors = 0;

            if (image.getMinR() != image.getMaxR()) {
                haveRed = true;
                numColors++;
            }

            if (image.getMinG() != image.getMaxG()) {
                haveGreen = true;
                numColors++;
            }

            if (image.getMinB() != image.getMaxB()) {
                haveBlue = true;
                numColors++;
            }

            lineString = lineString + "\t" + Integer.toString(numColors);
        } // if (image.isColorImage())

        lineString = lineString + "\r\n";
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("layout\tcoordinates\tvideo\r\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("layout\tsignificant_bits\t") + Integer.toString(bitSize) + "\r\n";
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("representation\tbyte_order\t");

        switch (image.getType()) {

            case ModelStorageBase.BYTE:
            case ModelStorageBase.UBYTE:
            case ModelStorageBase.ARGB:
                lineString = lineString + "1\r\n";
                break;

            case ModelStorageBase.SHORT:
            case ModelStorageBase.USHORT:
            case ModelStorageBase.ARGB_USHORT:
                lineString = lineString + "2\t1\r\n";
                break;

            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
            case ModelStorageBase.FLOAT:
            case ModelStorageBase.ARGB_FLOAT:
            case ModelStorageBase.COMPLEX:
                lineString = lineString + "4\t3\t2\t1\r\n";
                break;

            case ModelStorageBase.LONG:
            case ModelStorageBase.DOUBLE:
            case ModelStorageBase.DCOMPLEX:
                lineString = lineString + "8\t7\t6\t5\t4\t3\t2\t1\r\n";
                break;

            default:
                lineString = lineString + "1\r\n";
        }

        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("representation\tformat\t");

        switch (image.getType()) {

            case ModelStorageBase.BYTE:
            case ModelStorageBase.UBYTE:
            case ModelStorageBase.ARGB:
            case ModelStorageBase.SHORT:
            case ModelStorageBase.USHORT:
            case ModelStorageBase.ARGB_USHORT:
            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
            case ModelStorageBase.LONG:
                lineString = lineString + "integer\r\n";
                break;

            case ModelStorageBase.FLOAT:
            case ModelStorageBase.ARGB_FLOAT:
            case ModelStorageBase.DOUBLE:
                lineString = lineString + "real\r\n";
                break;

            case ModelStorageBase.COMPLEX:
            case ModelStorageBase.DCOMPLEX:
                lineString = lineString + "complex\r\n";
                break;

            default:
                lineString = lineString + "integer\r\n";
        }

        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("representation\tsign\t");

        switch (image.getType()) {

            case ModelStorageBase.BYTE:
            case ModelStorageBase.SHORT:
            case ModelStorageBase.INTEGER:
            case ModelStorageBase.LONG:
            case ModelStorageBase.FLOAT:
            case ModelStorageBase.ARGB_FLOAT:
            case ModelStorageBase.DOUBLE:
            case ModelStorageBase.COMPLEX:
            case ModelStorageBase.DCOMPLEX:
                lineString = lineString + "signed\r\n";
                break;

            case ModelStorageBase.UBYTE:
            case ModelStorageBase.USHORT:
            case ModelStorageBase.UINTEGER:
            case ModelStorageBase.ARGB:
            case ModelStorageBase.ARGB_USHORT:
                lineString = lineString + "unsigned\r\n";
                break;

            default:
                lineString = lineString + "unsigned\r\n";
        }

        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("representation\tcompression\tuncompressed\r\n");
        line = lineString.getBytes();
        raFile.write(line);

        startLocation = image.getFileInfo()[0].getOrigin();
        lineString = new String("parameter\torigin\t0.000000");

        for (i = 0; i < image.getNDims(); i++) {
            lineString = lineString + "\t" + Float.toString(startLocation[i]);
        }

        if (image.isColorImage()) {
            lineString = lineString + "\t0.000000";
        }

        lineString = lineString + "\r\n";
        line = lineString.getBytes();
        raFile.write(line);

        resols = image.getFileInfo()[0].getResolutions();
        lineString = new String("parameter\tscale\t1.0");

        for (i = 0; i < image.getNDims(); i++) {
            lineString = lineString + "\t" + Float.toString(resols[i]);
        }

        if (image.isColorImage()) {
            lineString = lineString + "\t0.000000";
        }

        lineString = lineString + "\r\n";
        line = lineString.getBytes();
        raFile.write(line);

        units = image.getFileInfo()[0].getUnitsOfMeasure();
        lineString = new String("parameter\tunits\tbits");

        for (i = 0; i < image.getNDims(); i++) {
            lineString = lineString + "\t" + (Unit.getUnitFromLegacyNum(units[i])).toString();
        }

        if (image.isColorImage()) {
            lineString = lineString + "\tundefined";
        }

        lineString = lineString + "\r\n";
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("parameter\tlabels\tbits\tx\ty");

        if (image.getNDims() >= 3) {
            lineString = lineString + "\tz";
        }

        if (image.getNDims() >= 4) {
            lineString = lineString + "\tt";
        }

        if (image.isColorImage()) {
            lineString = lineString + "\trgb";
        }

        lineString = lineString + "\r\n";
        line = lineString.getBytes();
        raFile.write(line);

        raFile.close();

        fireProgressStateChanged("Writing data file");
        file = new File(fileDir + fileDataName);
        raFile = new RandomAccessFile(file, "rw");

        sliceSize = image.getExtents()[0] * image.getExtents()[1];

        if (image.getNDims() >= 3) {
            zDim = image.getExtents()[2];
        } else {
            zDim = 1;
        }

        numberSlices = (tEnd - tBegin + 1) * (zEnd - zBegin + 1);
        count = 0;

        switch (image.getFileInfo()[0].getDataType()) {

            case ModelStorageBase.BYTE:
            case ModelStorageBase.UBYTE:
                byteBuffer = new byte[sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportSliceXY((t * zDim) + z, byteBuffer);
                        raFile.write(byteBuffer);
                    }
                }

                break;

            case ModelStorageBase.SHORT:
            case ModelStorageBase.USHORT:
                shortBuffer = new short[sliceSize];
                byteBuffer = new byte[2 * sliceSize];
                for (t = 0; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportSliceXY((t * zDim) + z, shortBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[2 * j] = (byte) (shortBuffer[j] >>> 8);
                            byteBuffer[(2 * j) + 1] = (byte) (shortBuffer[j]);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
                intBuffer = new int[sliceSize];
                byteBuffer = new byte[4 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportSliceXY((t * zDim) + z, intBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[4 * j] = (byte) (intBuffer[j] >>> 24);
                            byteBuffer[(4 * j) + 1] = (byte) (intBuffer[j] >>> 16);
                            byteBuffer[(4 * j) + 2] = (byte) (intBuffer[j] >>> 8);
                            byteBuffer[(4 * j) + 3] = (byte) (intBuffer[j]);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.LONG:
                longBuffer = new long[sliceSize];
                byteBuffer = new byte[8 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportSliceXY((t * zDim) + z, longBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[8 * j] = (byte) (longBuffer[j] >>> 56);
                            byteBuffer[(8 * j) + 1] = (byte) (longBuffer[j] >>> 48);
                            byteBuffer[(8 * j) + 2] = (byte) (longBuffer[j] >>> 40);
                            byteBuffer[(8 * j) + 3] = (byte) (longBuffer[j] >>> 32);
                            byteBuffer[(8 * j) + 4] = (byte) (longBuffer[j] >>> 24);
                            byteBuffer[(8 * j) + 5] = (byte) (longBuffer[j] >>> 16);
                            byteBuffer[(8 * j) + 6] = (byte) (longBuffer[j] >>> 8);
                            byteBuffer[(8 * j) + 7] = (byte) (longBuffer[j]);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.FLOAT:
                floatBuffer = new float[sliceSize];
                byteBuffer = new byte[4 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportSliceXY((t * zDim) + z, floatBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            tmpInt = Float.floatToIntBits(floatBuffer[j]);
                            byteBuffer[4 * j] = (byte) (tmpInt >>> 24);
                            byteBuffer[(4 * j) + 1] = (byte) (tmpInt >>> 16);
                            byteBuffer[(4 * j) + 2] = (byte) (tmpInt >>> 8);
                            byteBuffer[(4 * j) + 3] = (byte) (tmpInt);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.DOUBLE:
                doubleBuffer = new double[sliceSize];
                byteBuffer = new byte[8 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportSliceXY((t * zDim) + z, doubleBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            tmpLong = Double.doubleToLongBits(doubleBuffer[j]);
                            byteBuffer[8 * j] = (byte) (tmpLong >>> 56);
                            byteBuffer[(8 * j) + 1] = (byte) (tmpLong >>> 48);
                            byteBuffer[(8 * j) + 2] = (byte) (tmpLong >>> 40);
                            byteBuffer[(8 * j) + 3] = (byte) (tmpLong >>> 32);
                            byteBuffer[(8 * j) + 4] = (byte) (tmpLong >>> 24);
                            byteBuffer[(8 * j) + 5] = (byte) (tmpLong >>> 16);
                            byteBuffer[(8 * j) + 6] = (byte) (tmpLong >>> 8);
                            byteBuffer[(8 * j) + 7] = (byte) (tmpLong);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.COMPLEX:
                floatBuffer = new float[sliceSize];
                floatBufferI = new float[sliceSize];
                byteBuffer = new byte[8 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportComplexData(2 * ((t * zDim) + z) * sliceSize, sliceSize, floatBuffer, floatBufferI);

                        for (j = 0; j < sliceSize; j++) {
                            tmpInt = Float.floatToIntBits(floatBuffer[j]);
                            byteBuffer[8 * j] = (byte) (tmpInt >>> 24);
                            byteBuffer[(8 * j) + 1] = (byte) (tmpInt >>> 16);
                            byteBuffer[(8 * j) + 2] = (byte) (tmpInt >>> 8);
                            byteBuffer[(8 * j) + 3] = (byte) (tmpInt);
                            tmpInt = Float.floatToIntBits(floatBufferI[j]);
                            byteBuffer[(8 * j) + 4] = (byte) (tmpInt >>> 24);
                            byteBuffer[(8 * j) + 5] = (byte) (tmpInt >>> 16);
                            byteBuffer[(8 * j) + 6] = (byte) (tmpInt >>> 8);
                            byteBuffer[(8 * j) + 7] = (byte) (tmpInt);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.DCOMPLEX:
                doubleBuffer = new double[sliceSize];
                doubleBufferI = new double[sliceSize];
                byteBuffer = new byte[16 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportDComplexData(2 * ((t * zDim) + z) * sliceSize, sliceSize, doubleBuffer,
                                                 doubleBufferI);

                        for (j = 0; j < sliceSize; j++) {
                            tmpLong = Double.doubleToLongBits(doubleBuffer[j]);
                            byteBuffer[16 * j] = (byte) (tmpLong >>> 56);
                            byteBuffer[(16 * j) + 1] = (byte) (tmpLong >>> 48);
                            byteBuffer[(16 * j) + 2] = (byte) (tmpLong >>> 40);
                            byteBuffer[(16 * j) + 3] = (byte) (tmpLong >>> 32);
                            byteBuffer[(16 * j) + 4] = (byte) (tmpLong >>> 24);
                            byteBuffer[(16 * j) + 5] = (byte) (tmpLong >>> 16);
                            byteBuffer[(16 * j) + 6] = (byte) (tmpLong >>> 8);
                            byteBuffer[(16 * j) + 7] = (byte) (tmpLong);
                            tmpLong = Double.doubleToLongBits(doubleBufferI[j]);
                            byteBuffer[(16 * j) + 8] = (byte) (tmpLong >>> 56);
                            byteBuffer[(16 * j) + 9] = (byte) (tmpLong >>> 48);
                            byteBuffer[(16 * j) + 10] = (byte) (tmpLong >>> 40);
                            byteBuffer[(16 * j) + 11] = (byte) (tmpLong >>> 32);
                            byteBuffer[(16 * j) + 12] = (byte) (tmpLong >>> 24);
                            byteBuffer[(16 * j) + 13] = (byte) (tmpLong >>> 16);
                            byteBuffer[(16 * j) + 14] = (byte) (tmpLong >>> 8);
                            byteBuffer[(16 * j) + 15] = (byte) (tmpLong);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.ARGB:
                byteBuffer = new byte[sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / (numColors * numberSlices));

                        if (haveRed) {
                            image.exportRGBData(1, 4 * ((t * zDim) + z) * sliceSize, sliceSize, byteBuffer);
                        } else {
                            image.exportRGBData(2, 4 * ((t * zDim) + z) * sliceSize, sliceSize, byteBuffer);
                        }

                        raFile.write(byteBuffer);
                    }
                }

                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / (numColors * numberSlices));

                        if (haveRed && haveGreen) {
                            image.exportRGBData(2, 4 * ((t * zDim) + z) * sliceSize, sliceSize, byteBuffer);
                        } else {
                            image.exportRGBData(3, 4 * ((t * zDim) + z) * sliceSize, sliceSize, byteBuffer);
                        }

                        raFile.write(byteBuffer);
                    }
                }

                if (numColors == 3) {

                    for (t = tBegin; t <= tEnd; t++) {

                        for (z = zBegin; z <= zEnd; z++) {
                            fireProgressStateChanged((100 * count++) / (numColors * numberSlices));
                            image.exportRGBData(3, 4 * ((t * zDim) + z) * sliceSize, sliceSize, byteBuffer);
                            raFile.write(byteBuffer);
                        }
                    }
                } // if (numColors == 3)

                break;

            case ModelStorageBase.ARGB_USHORT:
                shortBuffer = new short[sliceSize];
                byteBuffer = new byte[2 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / (numColors * numberSlices));

                        if (haveRed) {
                            image.exportRGBData(1, 4 * ((t * zDim) + z) * sliceSize, sliceSize, shortBuffer);
                        } else {
                            image.exportRGBData(2, 4 * ((t * zDim) + z) * sliceSize, sliceSize, shortBuffer);
                        }

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[2 * j] = (byte) (shortBuffer[j] >>> 8);
                            byteBuffer[(2 * j) + 1] = (byte) (shortBuffer[j]);
                        }

                        raFile.write(byteBuffer);
                    }
                }

                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / (numColors * numberSlices));

                        if (haveRed && haveGreen) {
                            image.exportRGBData(2, 4 * ((t * zDim) + z) * sliceSize, sliceSize, shortBuffer);
                        } else {
                            image.exportRGBData(3, 4 * ((t * zDim) + z) * sliceSize, sliceSize, shortBuffer);
                        }

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[2 * j] = (byte) (shortBuffer[j] >>> 8);
                            byteBuffer[(2 * j) + 1] = (byte) (shortBuffer[j]);
                        }

                        raFile.write(byteBuffer);
                    }
                }

                if (numColors == 3) {

                    for (t = tBegin; t <= tEnd; t++) {

                        for (z = zBegin; z <= zEnd; z++) {
                            fireProgressStateChanged((100 * count++) / (numColors * numberSlices));
                            image.exportRGBData(3, 4 * ((t * zDim) + z) * sliceSize, sliceSize, shortBuffer);

                            for (j = 0; j < sliceSize; j++) {
                                byteBuffer[2 * j] = (byte) (shortBuffer[j] >>> 8);
                                byteBuffer[(2 * j) + 1] = (byte) (shortBuffer[j]);
                            }

                            raFile.write(byteBuffer);
                        }
                    }
                } // if (numColors == 3)

                break;

            case ModelStorageBase.ARGB_FLOAT:
                floatBuffer = new float[sliceSize];
                byteBuffer = new byte[4 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / (numColors * numberSlices));

                        if (haveRed) {
                            image.exportRGBData(1, 4 * ((t * zDim) + z) * sliceSize, sliceSize, floatBuffer);
                        } else {
                            image.exportRGBData(2, 4 * ((t * zDim) + z) * sliceSize, sliceSize, floatBuffer);
                        }

                        for (j = 0; j < sliceSize; j++) {
                            tmpInt = Float.floatToIntBits(floatBuffer[j]);
                            byteBuffer[4 * j] = (byte) (tmpInt >>> 24);
                            byteBuffer[(4 * j) + 1] = (byte) (tmpInt >>> 16);
                            byteBuffer[(4 * j) + 2] = (byte) (tmpInt >>> 8);
                            byteBuffer[(4 * j) + 3] = (byte) (tmpInt);
                        }

                        raFile.write(byteBuffer);
                    }
                }

                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / (numColors * numberSlices));

                        if (haveRed && haveGreen) {
                            image.exportRGBData(2, 4 * ((t * zDim) + z) * sliceSize, sliceSize, floatBuffer);
                        } else {
                            image.exportRGBData(3, 4 * ((t * zDim) + z) * sliceSize, sliceSize, floatBuffer);
                        }

                        for (j = 0; j < sliceSize; j++) {
                            tmpInt = Float.floatToIntBits(floatBuffer[j]);
                            byteBuffer[4 * j] = (byte) (tmpInt >>> 24);
                            byteBuffer[(4 * j) + 1] = (byte) (tmpInt >>> 16);
                            byteBuffer[(4 * j) + 2] = (byte) (tmpInt >>> 8);
                            byteBuffer[(4 * j) + 3] = (byte) (tmpInt);
                        }

                        raFile.write(byteBuffer);
                    }
                }

                if (numColors == 3) {

                    for (t = tBegin; t <= tEnd; t++) {

                        for (z = zBegin; z <= zEnd; z++) {
                            fireProgressStateChanged((100 * count++) / (numColors * numberSlices));
                            image.exportRGBData(3, 4 * ((t * zDim) + z) * sliceSize, sliceSize, floatBuffer);

                            for (j = 0; j < sliceSize; j++) {
                                tmpInt = Float.floatToIntBits(floatBuffer[j]);
                                byteBuffer[4 * j] = (byte) (tmpInt >>> 24);
                                byteBuffer[(4 * j) + 1] = (byte) (tmpInt >>> 16);
                                byteBuffer[(4 * j) + 2] = (byte) (tmpInt >>> 8);
                                byteBuffer[(4 * j) + 3] = (byte) (tmpInt);
                            }

                            raFile.write(byteBuffer);
                        }
                    }
                } // if (numColors == 3)

                break;
        } // switch(mage.getFileInfo()[0].getDataType())

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
        int nShorts;
        int nFloats;
        int b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16;
        byte[] byteBuffer;
        int progress, progressLength, mod;
        int tmpInt, tmpInt2, tmpInt3, tmpInt4;
        long savedPosition;

        switch (dataType) {

            case ModelStorageBase.BYTE:
                nBytes = buffer.length;
                byteBuffer = new byte[nBytes];
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 100;

                for (j = 0; j < nBytes; j++, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = byteBuffer[j];
                }

                break;

            case ModelStorageBase.UBYTE:
                nBytes = buffer.length;
                if (colorSpacing == RGB_LAST) {
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = slice * buffer.length;
                    progressLength = buffer.length * numberSlices;
                    mod = progressLength / 100;


                    for (j = 0; j < nBytes; j++, i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        buffer[i] = byteBuffer[j] & 0xff;
                    }
                } // if (colorSpacing == RGB_LAST)
                else if (colorSpacing == RGB_FIRST) {

                    if (nDims == 3) {
                        raFile.seek(dataOffset + slice);
                    } else if ((nDims == 4) && ((slice % imgExtents[2]) == 0)) {
                        raFile.seek(dataOffset + (slice / imgExtents[2]));
                        endBytes = numColors - 1 - (slice / imgExtents[2]);
                    }

                    progress = slice * buffer.length;
                    progressLength = buffer.length * numberSlices;
                    mod = progressLength / 100;


                    for (i = 0; i < (nBytes - 1); i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        buffer[i] = raFile.readUnsignedByte();

                        for (j = 0; j < (numColors - 1); j++) {
                            raFile.readByte();
                        }
                    } // for (i = 0; i < nBytes-1; i++)

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = raFile.readUnsignedByte();

                    if (nDims == 4) {

                        for (j = 0; j < endBytes; j++) {
                            raFile.readByte();
                        }
                    }
                } // else if (colorSpacing == RGB_FIRST)
                else { // colorSpacing == RGB_BETWEEN_SPACEANDTIME

                    // have x, y, color, time
                    // imgExtents[2] = tDim
                    // imgExtents[3] = numColors
                    if ((nDims == 4) && ((slice % imgExtents[2]) == 0)) {
                        raFile.seek(dataOffset + (bufferSize * slice / imgExtents[2]));
                    }

                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = slice * buffer.length;
                    progressLength = buffer.length * numberSlices;
                    mod = progressLength / 100;


                    for (j = 0; j < nBytes; j++, i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        buffer[i] = byteBuffer[j] & 0xff;
                    }

                    savedPosition = raFile.getFilePointer();
                    raFile.seek(((numColors - 1) * nBytes) + savedPosition);
                } // else colorSpacing == RGB_BETWEEN_SPACETIME

                break;

            case ModelStorageBase.SHORT:
                nBytes = 2 * buffer.length;
                byteBuffer = new byte[nBytes];
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
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

            case ModelStorageBase.USHORT:
                nBytes = 2 * buffer.length;
                if (colorSpacing == RGB_LAST) {
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = slice * buffer.length;
                    progressLength = buffer.length * numberSlices;
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
                } // if (colorSpacing == RGB_LAST)
                else if (colorSpacing == RGB_FIRST) {
                    nShorts = nBytes / 2;

                    if (nDims == 3) {
                        raFile.seek(dataOffset + (2 * slice));
                    } else if ((nDims == 4) && ((slice % imgExtents[2]) == 0)) {
                        raFile.seek(dataOffset + (2 * slice / imgExtents[2]));
                        endShorts = numColors - 1 - (slice / imgExtents[2]);
                    }

                    progress = slice * buffer.length;
                    progressLength = buffer.length * numberSlices;
                    mod = progressLength / 10;


                    for (i = 0; i < (nShorts - 1); i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        buffer[i] = getUnsignedShort(endianess);

                        for (j = 0; j < (numColors - 1); j++) {
                            raFile.readShort();
                        }
                    } // for (i = 0; i < nShorts-1; i++)

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = getUnsignedShort(endianess);

                    if (nDims == 4) {

                        for (j = 0; j < endShorts; j++) {
                            raFile.readShort();
                        }
                    }
                } // else if (colorSpacing == RGB_FIRST)
                else { // colorSpacing == RGB_BETWEEN_SPACEANDTIME

                    // have x, y, color, time
                    // imgExtents[2] = tDim
                    // imgExtents[3] = numColors
                    if ((nDims == 4) && ((slice % imgExtents[2]) == 0)) {
                        raFile.seek(dataOffset + (2 * bufferSize * slice / imgExtents[2]));
                    }

                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = slice * buffer.length;
                    progressLength = buffer.length * numberSlices;
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

                    savedPosition = raFile.getFilePointer();
                    raFile.seek(((numColors - 1) * nBytes) + savedPosition);
                } // else colorSpacing == RGB_BETWEEN_SPACETIME

                break;

            case ModelStorageBase.INTEGER:
                nBytes = 4 * buffer.length;
                byteBuffer = new byte[nBytes];
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
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

            case ModelStorageBase.FLOAT:
                nBytes = 4 * buffer.length;
                if (colorSpacing == RGB_LAST) {
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = slice * buffer.length;
                    progressLength = buffer.length * numberSlices;
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
                } // if (colorSpacing == RGB_LAST)
                else if (colorSpacing == RGB_FIRST) {
                    nFloats = nBytes / 4;

                    if (nDims == 3) {
                        raFile.seek(dataOffset + (4 * slice));
                    } else if ((nDims == 4) && ((slice % imgExtents[2]) == 0)) {
                        raFile.seek(dataOffset + (4 * slice / imgExtents[2]));
                        endFloats = numColors - 1 - (slice / imgExtents[2]);
                    }

                    progress = slice * buffer.length;
                    progressLength = buffer.length * numberSlices;
                    mod = progressLength / 10;


                    for (i = 0; i < (nFloats - 1); i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        buffer[i] = getFloat(endianess);

                        for (j = 0; j < (numColors - 1); j++) {
                            raFile.readFloat();
                        }
                    } // for (i = 0; i < nFloats-1; i++)

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = getFloat(endianess);

                    if (nDims == 4) {

                        for (j = 0; j < endFloats; j++) {
                            raFile.readFloat();
                        }
                    }
                } // else if (colorSpacing == RGB_FIRST)
                else { // colorSpacing == RGB_BETWEEN_SPACEANDTIME

                    // have x, y, color, time
                    // imgExtents[2] = tDim
                    // imgExtents[3] = numColors
                    if ((nDims == 4) && ((slice % imgExtents[2]) == 0)) {
                        raFile.seek(dataOffset + (4 * bufferSize * slice / imgExtents[2]));
                    }

                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = slice * buffer.length;
                    progressLength = buffer.length * numberSlices;
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

                    savedPosition = raFile.getFilePointer();
                    raFile.seek(((numColors - 1) * nBytes) + savedPosition);
                } // else colorSpacing == RGB_BETWEEN_SPACEANDTIME

                break;

            case ModelStorageBase.ARGB:
                if (colorSpacing == RGB_LAST) {
                    nBytes = buffer.length / 4;
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = numColors * slice * nBytes;
                    progressLength = numColors * nBytes * numberSlices;
                    mod = progressLength / 100;


                    for (j = 0; j < nBytes; j++, i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        buffer[(4 * i) + 1] = byteBuffer[j] & 0xff;
                    }

                    savedPosition = raFile.getFilePointer();
                    raFile.seek(((numberSlices - 1) * nBytes) + savedPosition);
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = (numColors * slice * nBytes) + nBytes;

                    for (i = 0, j = 0; j < nBytes; j++, i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        buffer[(4 * i) + 2] = byteBuffer[j] & 0xff;
                    }

                    if (numColors >= 3) {
                        raFile.seek((((2 * numberSlices) - 1) * nBytes) + savedPosition);
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = (numColors * slice * nBytes) + (2 * nBytes);

                        for (i = 0, j = 0; j < nBytes; j++, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            buffer[(4 * i) + 3] = byteBuffer[j] & 0xff;
                        }
                    } // if (numColors >= 3)
                    
                    if (numColors == 4) {
                        raFile.seek((((3 * numberSlices) - 1) * nBytes) + savedPosition);
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = (numColors * slice * nBytes) + (3 * nBytes);

                        for (i = 0, j = 0; j < nBytes; j++, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            buffer[(4 * i)] = byteBuffer[j] & 0xff;
                        }    
                    } // if (numColors == 4)

                    raFile.seek(savedPosition);
                } // if (colorSpacing == RGB_LAST)
                else if (colorSpacing == RGB_FIRST) {

                    if (numColors == 2) {
                        nBytes = 2 * buffer.length / 4;
                        byteBuffer = new byte[nBytes];
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * nBytes;
                        progressLength = nBytes * numberSlices;
                        mod = progressLength / 100;


                        for (j = 0; j < nBytes; j += 2, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            buffer[(4 * i) + 1] = byteBuffer[j] & 0xff;
                            buffer[(4 * i) + 2] = byteBuffer[j + 1] & 0xff;
                            buffer[(4 * i) + 3] = 0;
                        }
                    } // if (numColors == 2)
                    else if (numColors == 3) {
                        nBytes = 3 * buffer.length / 4;
                        byteBuffer = new byte[nBytes];
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * nBytes;
                        progressLength = nBytes * numberSlices;
                        mod = progressLength / 100;


                        for (j = 0; j < nBytes; j += 3, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            buffer[(4 * i) + 1] = byteBuffer[j] & 0xff;
                            buffer[(4 * i) + 2] = byteBuffer[j + 1] & 0xff;
                            buffer[(4 * i) + 3] = byteBuffer[j + 2] & 0xff;
                        }
                    } // else if (numColors == 3)
                    else { // numColors == 4
                        nBytes = buffer.length;
                        byteBuffer = new byte[nBytes];
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * nBytes;
                        progressLength = nBytes * numberSlices;
                        mod = progressLength / 100;


                        for (j = 0; j < nBytes; j += 4, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            buffer[(4 * i) + 1] = byteBuffer[j] & 0xff;
                            buffer[(4 * i) + 2] = byteBuffer[j + 1] & 0xff;
                            buffer[(4 * i) + 3] = byteBuffer[j + 2] & 0xff;
                            buffer[4 * i] = byteBuffer[j + 3] & 0xff;
                        }
                    } // else numColors == 4
                } // else if (colorSpacing == RGB_FIRST)
                else { // colorSpacing == RGB_BETWEEN_SPACEANDTIME
                    nBytes = buffer.length / 4;
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = numColors * slice * nBytes;
                    progressLength = numColors * nBytes * numberSlices;
                    mod = progressLength / 100;


                    for (j = 0; j < nBytes; j++, i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        buffer[(4 * i) + 1] = byteBuffer[j] & 0xff;
                    }

                    savedPosition = raFile.getFilePointer();
                    raFile.seek(((numberSpaceSlices - 1) * nBytes) + savedPosition);
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = (numColors * slice * nBytes) + nBytes;

                    for (i = 0, j = 0; j < nBytes; j++, i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        buffer[(4 * i) + 2] = byteBuffer[j] & 0xff;
                    }

                    if (numColors >= 3) {
                        raFile.seek((((2 * numberSpaceSlices) - 1) * nBytes) + savedPosition);
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = (numColors * slice * nBytes) + (2 * nBytes);

                        for (i = 0, j = 0; j < nBytes; j++, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            buffer[(4 * i) + 3] = byteBuffer[j] & 0xff;
                        }
                    } // if (numColors >= 3)
                    
                    if (numColors == 4) {
                        raFile.seek((((3 * numberSpaceSlices) - 1) * nBytes) + savedPosition);
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = (numColors * slice * nBytes) + (3 * nBytes);

                        for (i = 0, j = 0; j < nBytes; j++, i++) {

                            if (((i + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                            }

                            buffer[(4 * i)] = byteBuffer[j] & 0xff;
                        }
                    } // if (numColors == 4)

                    if (((slice + 1) % numberSpaceSlices) == 0) { }
                    else {
                        raFile.seek(savedPosition);
                    }
                } // else colorSpacing == RGB_BETWEEN_SPACEANDTIME

                break;

            case ModelStorageBase.ARGB_USHORT:
                if (colorSpacing == RGB_LAST) {
                    nBytes = buffer.length / 2;
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = numColors * slice * nBytes;
                    progressLength = numColors * nBytes * numberSlices;
                    mod = progressLength / 100;


                    for (j = 0; j < nBytes; j += 2, i++) {

                        if (((j + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                        }

                        b1 = getUnsignedByte(byteBuffer, j);
                        b2 = getUnsignedByte(byteBuffer, j + 1);

                        if (endianess) {
                            buffer[(4 * i) + 1] = ((b1 << 8) + b2);
                        } else {
                            buffer[(4 * i) + 1] = ((b2 << 8) + b1);
                        }
                    }

                    savedPosition = raFile.getFilePointer();
                    raFile.seek(((numberSlices - 1) * nBytes) + savedPosition);
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = (numColors * slice * nBytes) + nBytes;

                    for (i = 0, j = 0; j < nBytes; j += 2, i++) {

                        if (((j + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                        }

                        b1 = getUnsignedByte(byteBuffer, j);
                        b2 = getUnsignedByte(byteBuffer, j + 1);

                        if (endianess) {
                            buffer[(4 * i) + 2] = ((b1 << 8) + b2);
                        } else {
                            buffer[(4 * i) + 2] = ((b2 << 8) + b1);
                        }
                    }

                    if (numColors >= 3) {
                        raFile.seek((((2 * numberSlices) - 1) * nBytes) + savedPosition);
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = (numColors * slice * nBytes) + (2 * nBytes);

                        for (i = 0, j = 0; j < nBytes; j += 2, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);

                            if (endianess) {
                                buffer[(4 * i) + 3] = ((b1 << 8) + b2);
                            } else {
                                buffer[(4 * i) + 3] = ((b2 << 8) + b1);
                            }
                        }
                    } // if (numColors >= 3)
                    
                    if (numColors == 4) {
                        raFile.seek((((3 * numberSlices) - 1) * nBytes) + savedPosition);
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = (numColors * slice * nBytes) + (3 * nBytes);

                        for (i = 0, j = 0; j < nBytes; j += 2, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);

                            if (endianess) {
                                buffer[(4 * i)] = ((b1 << 8) + b2);
                            } else {
                                buffer[(4 * i)] = ((b2 << 8) + b1);
                            }
                        }
                    } // if (numColors == 4)

                    raFile.seek(savedPosition);
                } // if (colorSpacing == RGB_LAST)
                else if (colorSpacing == RGB_FIRST) {

                    if (numColors == 2) {
                        nBytes = 2 * buffer.length / 2;
                        byteBuffer = new byte[nBytes];
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * nBytes;
                        progressLength = nBytes * numberSlices;
                        mod = progressLength / 100;


                        for (j = 0; j < nBytes; j += 4, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);
                            b3 = getUnsignedByte(byteBuffer, j + 2);
                            b4 = getUnsignedByte(byteBuffer, j + 3);

                            if (endianess) {
                                buffer[(4 * i) + 1] = ((b1 << 8) + b2);
                                buffer[(4 * i) + 2] = ((b3 << 8) + b4);
                            } else {
                                buffer[(4 * i) + 1] = ((b2 << 8) + b1);
                                buffer[(4 * i) + 2] = ((b4 << 8) + b3);
                            }
                        }
                    } // if (numColors == 2)
                    else if (numColors == 3) {
                        nBytes = 3 * buffer.length / 2;
                        byteBuffer = new byte[nBytes];
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * nBytes;
                        progressLength = nBytes * numberSlices;
                        mod = progressLength / 100;


                        for (j = 0; j < nBytes; j += 6, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);
                            b3 = getUnsignedByte(byteBuffer, j + 2);
                            b4 = getUnsignedByte(byteBuffer, j + 3);
                            b5 = getUnsignedByte(byteBuffer, j + 4);
                            b6 = getUnsignedByte(byteBuffer, j + 5);

                            if (endianess) {
                                buffer[(4 * i) + 1] = ((b1 << 8) + b2);
                                buffer[(4 * i) + 2] = ((b3 << 8) + b4);
                                buffer[(4 * i) + 3] = ((b5 << 8) + b6);
                            } else {
                                buffer[(4 * i) + 1] = ((b2 << 8) + b1);
                                buffer[(4 * i) + 2] = ((b4 << 8) + b3);
                                buffer[(4 * i) + 3] = ((b6 << 8) + b5);
                            }
                        }
                    } // else if (numColors == 3)
                    else { // numColors == 4
                        nBytes = 2 * buffer.length;
                        byteBuffer = new byte[nBytes];
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * nBytes;
                        progressLength = nBytes * numberSlices;
                        mod = progressLength / 100;


                        for (j = 0; j < nBytes; j += 8, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);
                            b3 = getUnsignedByte(byteBuffer, j + 2);
                            b4 = getUnsignedByte(byteBuffer, j + 3);
                            b5 = getUnsignedByte(byteBuffer, j + 4);
                            b6 = getUnsignedByte(byteBuffer, j + 5);
                            b7 = getUnsignedByte(byteBuffer, j + 6);
                            b8 = getUnsignedByte(byteBuffer, j + 7);

                            if (endianess) {
                                buffer[(4 * i) + 1] = ((b1 << 8) + b2);
                                buffer[(4 * i) + 2] = ((b3 << 8) + b4);
                                buffer[(4 * i) + 3] = ((b5 << 8) + b6);
                                buffer[(4 * i)] = ((b7 << 8) + b8);
                            } else {
                                buffer[(4 * i) + 1] = ((b2 << 8) + b1);
                                buffer[(4 * i) + 2] = ((b4 << 8) + b3);
                                buffer[(4 * i) + 3] = ((b6 << 8) + b5);
                                buffer[(4 * i)] = ((b8 << 8) + b7);
                            }
                        }    
                    } // else numColors == 4
                } // else if (colorSpacing == RGB_FIRST)
                else { // colorSpacing == RGB_BETWEEN_SPACEANDTIME
                    nBytes = buffer.length / 2;
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = numColors * slice * nBytes;
                    progressLength = numColors * nBytes * numberSlices;
                    mod = progressLength / 100;


                    for (j = 0; j < nBytes; j += 2, i++) {

                        if (((j + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                        }

                        b1 = getUnsignedByte(byteBuffer, j);
                        b2 = getUnsignedByte(byteBuffer, j + 1);

                        if (endianess) {
                            buffer[(4 * i) + 1] = ((b1 << 8) + b2);
                        } else {
                            buffer[(4 * i) + 1] = ((b2 << 8) + b1);
                        }
                    }

                    savedPosition = raFile.getFilePointer();
                    raFile.seek(((numberSpaceSlices - 1) * nBytes) + savedPosition);
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = (numColors * slice * nBytes) + nBytes;

                    for (i = 0, j = 0; j < nBytes; j += 2, i++) {

                        if (((j + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                        }

                        b1 = getUnsignedByte(byteBuffer, j);
                        b2 = getUnsignedByte(byteBuffer, j + 1);

                        if (endianess) {
                            buffer[(4 * i) + 2] = ((b1 << 8) + b2);
                        } else {
                            buffer[(4 * i) + 2] = ((b2 << 8) + b1);
                        }
                    }

                    if (numColors >= 3) {
                        raFile.seek((((2 * numberSpaceSlices) - 1) * nBytes) + savedPosition);
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = (numColors * slice * nBytes) + (2 * nBytes);

                        for (i = 0, j = 0; j < nBytes; j += 2, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);

                            if (endianess) {
                                buffer[(4 * i) + 3] = ((b1 << 8) + b2);
                            } else {
                                buffer[(4 * i) + 3] = ((b2 << 8) + b1);
                            }
                        }
                    } // if (numColors >= 3)
                    
                    if (numColors == 4) {
                        raFile.seek((((3 * numberSpaceSlices) - 1) * nBytes) + savedPosition);
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = (numColors * slice * nBytes) + (3 * nBytes);

                        for (i = 0, j = 0; j < nBytes; j += 2, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);

                            if (endianess) {
                                buffer[(4 * i)] = ((b1 << 8) + b2);
                            } else {
                                buffer[(4 * i)] = ((b2 << 8) + b1);
                            }
                        }    
                    } // if (numColors == 4)

                    if (((slice + 1) % numberSpaceSlices) == 0) { }
                    else {
                        raFile.seek(savedPosition);
                    }
                } // else colorSpacing == RGB_BETWEEN_SPACEANDTIME

                break;

            case ModelStorageBase.ARGB_FLOAT:
                if (colorSpacing == RGB_LAST) {
                    nBytes = buffer.length;
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = numColors * slice * nBytes;
                    progressLength = numColors * nBytes * numberSlices;
                    mod = progressLength / 100;


                    for (j = 0; j < nBytes; j += 4, i++) {

                        if (((j + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
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

                        buffer[(4 * i) + 1] = Float.intBitsToFloat(tmpInt);
                    }

                    savedPosition = raFile.getFilePointer();
                    raFile.seek(((numberSlices - 1) * nBytes) + savedPosition);
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = (numColors * slice * nBytes) + nBytes;

                    for (i = 0, j = 0; j < nBytes; j += 4, i++) {

                        if (((j + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
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

                        buffer[(4 * i) + 2] = Float.intBitsToFloat(tmpInt);
                    }

                    if (numColors >= 3) {
                        raFile.seek((((2 * numberSlices) - 1) * nBytes) + savedPosition);
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = (numColors * slice * nBytes) + (2 * nBytes);

                        for (i = 0, j = 0; j < nBytes; j += 4, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
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

                            buffer[(4 * i) + 3] = Float.intBitsToFloat(tmpInt);
                        }
                    } // if (numColors >= 3)
                    
                    if (numColors == 4) {
                        raFile.seek((((3 * numberSlices) - 1) * nBytes) + savedPosition);
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = (numColors * slice * nBytes) + (3 * nBytes);

                        for (i = 0, j = 0; j < nBytes; j += 4, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
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

                            buffer[(4 * i)] = Float.intBitsToFloat(tmpInt);
                        }
                    } // if (numColors == 4)

                    raFile.seek(savedPosition);
                } // if (colorSpacing == RGB_LAST)
                else if (colorSpacing == RGB_FIRST) {

                    if (numColors == 2) {
                        nBytes = 2 * buffer.length;
                        byteBuffer = new byte[nBytes];
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * nBytes;
                        progressLength = nBytes * numberSlices;
                        mod = progressLength / 100;


                        for (j = 0; j < nBytes; j += 8, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);
                            b3 = getUnsignedByte(byteBuffer, j + 2);
                            b4 = getUnsignedByte(byteBuffer, j + 3);
                            b5 = getUnsignedByte(byteBuffer, j + 4);
                            b6 = getUnsignedByte(byteBuffer, j + 5);
                            b7 = getUnsignedByte(byteBuffer, j + 6);
                            b8 = getUnsignedByte(byteBuffer, j + 7);

                            if (endianess) {
                                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                                tmpInt2 = ((b5 << 24) | (b6 << 16) | (b7 << 8) | b8);
                            } else {
                                tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                                tmpInt2 = ((b8 << 24) | (b7 << 16) | (b6 << 8) | b5);
                            }

                            buffer[(4 * i) + 1] = Float.intBitsToFloat(tmpInt);
                            buffer[(4 * i) + 2] = Float.intBitsToFloat(tmpInt2);
                        }
                    } // if (numColors == 2)
                    else if (numColors == 3) {
                        nBytes = 3 * buffer.length;
                        byteBuffer = new byte[nBytes];
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * nBytes;
                        progressLength = nBytes * numberSlices;
                        mod = progressLength / 100;


                        for (j = 0; j < nBytes; j += 12, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);
                            b3 = getUnsignedByte(byteBuffer, j + 2);
                            b4 = getUnsignedByte(byteBuffer, j + 3);
                            b5 = getUnsignedByte(byteBuffer, j + 4);
                            b6 = getUnsignedByte(byteBuffer, j + 5);
                            b7 = getUnsignedByte(byteBuffer, j + 6);
                            b8 = getUnsignedByte(byteBuffer, j + 7);
                            b9 = getUnsignedByte(byteBuffer, j + 8);
                            b10 = getUnsignedByte(byteBuffer, j + 9);
                            b11 = getUnsignedByte(byteBuffer, j + 10);
                            b12 = getUnsignedByte(byteBuffer, j + 11);

                            if (endianess) {
                                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                                tmpInt2 = ((b5 << 24) | (b6 << 16) | (b7 << 8) | b8);
                                tmpInt3 = ((b9 << 24) | (b10 << 16) | (b11 << 8) | b12);
                            } else {
                                tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                                tmpInt2 = ((b8 << 24) | (b7 << 16) | (b6 << 8) | b5);
                                tmpInt3 = ((b12 << 24) | (b11 << 16) | (b10 << 8) | b9);
                            }

                            buffer[(4 * i) + 1] = Float.intBitsToFloat(tmpInt);
                            buffer[(4 * i) + 2] = Float.intBitsToFloat(tmpInt2);
                            buffer[(4 * i) + 3] = Float.intBitsToFloat(tmpInt3);
                        }
                    } // else if (numColors == 3)
                    else { // numColors == 4
                        nBytes = 4 * buffer.length;
                        byteBuffer = new byte[nBytes];
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = slice * nBytes;
                        progressLength = nBytes * numberSlices;
                        mod = progressLength / 100;


                        for (j = 0; j < nBytes; j += 16, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
                            }

                            b1 = getUnsignedByte(byteBuffer, j);
                            b2 = getUnsignedByte(byteBuffer, j + 1);
                            b3 = getUnsignedByte(byteBuffer, j + 2);
                            b4 = getUnsignedByte(byteBuffer, j + 3);
                            b5 = getUnsignedByte(byteBuffer, j + 4);
                            b6 = getUnsignedByte(byteBuffer, j + 5);
                            b7 = getUnsignedByte(byteBuffer, j + 6);
                            b8 = getUnsignedByte(byteBuffer, j + 7);
                            b9 = getUnsignedByte(byteBuffer, j + 8);
                            b10 = getUnsignedByte(byteBuffer, j + 9);
                            b11 = getUnsignedByte(byteBuffer, j + 10);
                            b12 = getUnsignedByte(byteBuffer, j + 11);
                            b13 = getUnsignedByte(byteBuffer, j + 12);
                            b14 = getUnsignedByte(byteBuffer, j + 13);
                            b15 = getUnsignedByte(byteBuffer, j + 14);
                            b16 = getUnsignedByte(byteBuffer, j + 15);

                            if (endianess) {
                                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                                tmpInt2 = ((b5 << 24) | (b6 << 16) | (b7 << 8) | b8);
                                tmpInt3 = ((b9 << 24) | (b10 << 16) | (b11 << 8) | b12);
                                tmpInt4 = ((b13 << 24) | (b14 << 16) | (b15 << 8) | b16);
                            } else {
                                tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                                tmpInt2 = ((b8 << 24) | (b7 << 16) | (b6 << 8) | b5);
                                tmpInt3 = ((b12 << 24) | (b11 << 16) | (b10 << 8) | b9);
                                tmpInt4 = ((b16 << 24) | (b15 << 16) | (b14 << 8) | b13);
                            }

                            buffer[(4 * i) + 1] = Float.intBitsToFloat(tmpInt);
                            buffer[(4 * i) + 2] = Float.intBitsToFloat(tmpInt2);
                            buffer[(4 * i) + 3] = Float.intBitsToFloat(tmpInt3);
                            buffer[(4 * i)] = Float.intBitsToFloat(tmpInt4);
                        }    
                    } // else numColors == 4
                } // else if (colorSpacing == RGB_FIRST)
                else { // colorSpacing == RGB_BETWEEN_SPACEANDTIME
                    nBytes = buffer.length;
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = numColors * slice * nBytes;
                    progressLength = numColors * nBytes * numberSlices;
                    mod = progressLength / 100;


                    for (j = 0; j < nBytes; j += 4, i++) {

                        if (((j + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
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

                        buffer[(4 * i) + 1] = Float.intBitsToFloat(tmpInt);
                    }

                    savedPosition = raFile.getFilePointer();
                    raFile.seek(((numberSpaceSlices - 1) * nBytes) + savedPosition);
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = (numColors * slice * nBytes) + nBytes;

                    for (i = 0, j = 0; j < nBytes; j += 4, i++) {

                        if (((j + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
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

                        buffer[(4 * i) + 2] = Float.intBitsToFloat(tmpInt);
                    }

                    if (numColors >= 3) {
                        raFile.seek((((2 * numberSpaceSlices) - 1) * nBytes) + savedPosition);
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = (numColors * slice * nBytes) + (2 * nBytes);

                        for (i = 0, j = 0; j < nBytes; j += 4, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
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

                            buffer[(4 * i) + 3] = Float.intBitsToFloat(tmpInt);
                        }
                    } // if (numColors >= 3)
                    
                    if (numColors == 4) {
                        raFile.seek((((3 * numberSpaceSlices) - 1) * nBytes) + savedPosition);
                        raFile.read(byteBuffer, 0, nBytes);
                        progress = (numColors * slice * nBytes) + (3 * nBytes);

                        for (i = 0, j = 0; j < nBytes; j += 4, i++) {

                            if (((j + progress) % mod) == 0) {
                                fireProgressStateChanged(Math.round((float) (j + progress) / progressLength * 100));
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

                            buffer[(4 * i)] = Float.intBitsToFloat(tmpInt);
                        }    
                    } // if (numColors == 4)

                    if (((slice + 1) % numberSpaceSlices) == 0) { }
                    else {
                        raFile.seek(savedPosition);
                    }
                } // else colorSpacing == RGB_BETWEEN_SPACETIME

                break;
        } // switch(dataType)

    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice    offset into the file stored in the dataOffset array
     * @param      bufferR  buffer where the real info is stored
     * @param      bufferI  buffer where the imaginary info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readComplexBuffer(int slice, float[] bufferR, float[] bufferI) throws IOException {
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

            bufferR[i] = Float.intBitsToFloat(tmpInt);

            b1 = getUnsignedByte(byteBuffer, j + 4);
            b2 = getUnsignedByte(byteBuffer, j + 5);
            b3 = getUnsignedByte(byteBuffer, j + 6);
            b4 = getUnsignedByte(byteBuffer, j + 7);

            if (endianess) {
                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
            } else {
                tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
            }

            bufferI[i] = Float.intBitsToFloat(tmpInt);
        } // for (j =0; j < nBytes; j+=8, i++ )
    }


    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice   offset into the file stored in the dataOffset array
     * @param      buffer  buffer where the info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readDBuffer(int slice, double[] buffer) throws IOException {
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

            if (((i + progress) % mod) == 0) {
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

            if (endianess) {
                tmpLong = ((b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) | (b5 << 24) | (b6 << 16) | (b7 << 8) |
                               b8); // Big Endian
            } else {
                tmpLong = ((b8 << 56) | (b7 << 48) | (b6 << 40) | (b5 << 32) | (b4 << 24) | (b3 << 16) | (b2 << 8) |
                               b1); // Little Endian
            }

            buffer[i] = Double.longBitsToDouble(tmpLong);

        } // for (j =0; j < nBytes; j+=8, i++ )

    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice    offset into the file stored in the dataOffset array
     * @param      bufferR  buffer where the real info is stored
     * @param      bufferI  buffer where the imaginary info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readDComplexBuffer(int slice, double[] bufferR, double[] bufferI) throws IOException {
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

            if (((i + progress) % mod) == 0) {
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

            if (endianess) {
                tmpLong = ((b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) | (b5 << 24) | (b6 << 16) | (b7 << 8) |
                               b8); // Big Endian
            } else {
                tmpLong = ((b8 << 56) | (b7 << 48) | (b6 << 40) | (b5 << 32) | (b4 << 24) | (b3 << 16) | (b2 << 8) |
                               b1); // Little Endian
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

            if (endianess) {
                tmpLong = ((b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) | (b5 << 24) | (b6 << 16) | (b7 << 8) |
                               b8); // Big Endian
            } else {
                tmpLong = ((b8 << 56) | (b7 << 48) | (b6 << 40) | (b5 << 32) | (b4 << 24) | (b3 << 16) | (b2 << 8) |
                               b1); // Little Endian
            }

            bufferI[i] = Double.longBitsToDouble(tmpLong);
        } // for (j =0; j < nBytes; j+=16, i++ )
    }

    /**
     * Reads the image header.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public boolean readHeader() throws IOException {
        int s;
        int index;
        String tempString;
        boolean done;
        long fileLength;
        int i;
        boolean bigEndian;
        boolean littleEndian;
        String sTemp;

        try {
            s = fileName.lastIndexOf(".");

            if (s == -1) {
                throw new IOException("ICS file name Error: . sign not found");
            }

            headerFileName = fileName.substring(0, s + 1) + "ics";
            file = new File(fileDir + headerFileName);
            raFile = new RandomAccessFile(file, "r");
            fileLength = raFile.length();

            fileInfo = new FileInfoICS(headerFileName, fileDir, FileUtility.ICS);

            // The first line must contains the field separator
            // character followed by the line separator character
            try {
                tempString = raFile.readLine();
            } catch (IOException error) {
                throw (error);
            }

            if (tempString == null) {
                Preferences.debug("First line of ICS header file is null\n", Preferences.DEBUG_FILEIO);
                throw new IOException("First line of ICS header file is null");
            }

            fieldSeparator = tempString.getBytes()[0];

            // The second line must contain ics_version, followed by the field
            // separator character, followed by the version number
            try {
                tempString = raFile.readLine();
            } catch (IOException error) {
                throw (error);
            }

            if (tempString == null) {
                Preferences.debug("Second line of ICS header file is null\n", Preferences.DEBUG_FILEIO);
                throw new IOException("Second line of ICS header file is null");
            }

            index = tempString.indexOf(fieldSeparator);

            if (index != -1) {
                category = tempString.substring(0, index);
            } else {
                Preferences.debug("Second line of ICS header file has no field separator\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Second line = " + tempString, Preferences.DEBUG_FILEIO);
                throw new IOException("Second line of ICS header file has no field separator");
            }

            if ((category.compareTo("ics_version")) != 0) {
                Preferences.debug("Second line of header file is an erroneous " + tempString + "\n", Preferences.DEBUG_FILEIO);
                throw new IOException("Second line of ICS header file lacks ics_version");
            }

            version = tempString.substring(index + 1);
            fileInfo.setVersion(version);

            // The third line of the ICS header file contains filename, followed by a
            // field separator, followed by the data set filename
            try {
                tempString = raFile.readLine();
            } catch (IOException error) {
                throw (error);
            }

            if (tempString == null) {
                Preferences.debug("Third line of ICS header file is null", Preferences.DEBUG_FILEIO);
                throw new IOException("Third line of ICS header file is null");
            }

            index = tempString.indexOf(fieldSeparator);

            if (index != -1) {
                category = tempString.substring(0, index);
            } else {
                Preferences.debug("Third line of ICS header file has no field separator", Preferences.DEBUG_FILEIO);
                throw new IOException("Third line of ICS header file has no field separator");
            }

            if ((category.compareTo("filename")) != 0) {
                Preferences.debug("Third line of header file is an erroneous " + tempString, Preferences.DEBUG_FILEIO);
                throw new IOException("Third line of ICS header file lacks filename");
            }

            dataSetFileName = tempString.substring(index + 1);
            fileInfo.setDataSetFileName(dataSetFileName);


            done = false;

            while (!done) {

                if (raFile.getFilePointer() >= (fileLength - 1)) {
                    done = true;

                    break;
                }

                readLine();

                if ((category.compareTo("layout")) == 0) {

                    if ((subcategory.compareTo("parameters")) == 0) {

                        // The first parameter has the number of bits per pixel
                        parameters = Integer.valueOf(values[0]).intValue();
                    } // if ((subcategory.compareTo("parameters")) == 0)
                    else if ((subcategory.compareTo("order")) == 0) {

                        if ((values[0].compareTo("bits")) != 0) {

                            // bits must follow the order keyword
                            Preferences.debug("order[0] = " + values[0] + " instead of the required bits\n", 
                            		Preferences.DEBUG_FILEIO);
                            throw new IOException("order[0] = " + values[0] + " instead of the required bits");
                        }

                        order = new String[numValues];

                        for (i = 0; i < numValues; i++) {
                            order[i] = values[i];

                            if (order[i].compareTo("x") == 0) {
                                xPos = i;
                                nDims++;
                            }

                            if (order[i].compareTo("y") == 0) {
                                yPos = i;
                                nDims++;
                            }

                            if (order[i].compareTo("z") == 0) {
                                zPos = i;
                                nDims++;
                            }

                            if (order[i].compareTo("t") == 0) {
                                tPos = i;
                                nDims++;
                            }

                            if ((order[i].compareTo("rgb") == 0) || (order[i].compareTo("probe") == 0) ||
                                    (order[i].compareTo("ch") == 0)) {
                                rgbPos = i;
                                if (order[i].compareTo("rgb") == 0) {
                                    hasRGB = true;
                                }
                            }
                        }

                        if (yPos < xPos) {
                            exchangeXY = true;
                        }
                    } // else if ((subcategory.compareTo("order")) == 0)
                    else if ((subcategory.compareTo("sizes")) == 0) {
                        sizes = new int[numValues];

                        for (i = 0; i < numValues; i++) {
                            sizes[i] = Integer.valueOf(values[i]).intValue();
                        }
                    } // else if ((subcategory.compareTo("sizes")) == 0)
                    else if ((subcategory.compareTo("coordinates")) == 0) {

                        if ((values[0].compareTo("video")) == 0) {

                            // y increases downward - the normal default
                            invertY = false;
                        } else if ((values[0].compareTo("cartesian")) == 0) {

                            // y increases upward
                            invertY = true;
                        } else {
                            Preferences.debug("Illegal keyword of " + values[0] + " for coordinates\n", 
                            		Preferences.DEBUG_FILEIO);
                            throw new IOException("Illegal keyword of " + values[0] + " for coordinates");
                        }
                    } // else if ((subcategory.compareTo("coordinates")) == 0)
                    else if ((subcategory.compareTo("significant_bits")) == 0) {

                        // The data must be in the low order bits of the word
                        significant_bits = Integer.valueOf(values[0]).intValue();
                    }
                } // if ((category.compareTo("layout")) == 0)
                else if ((category.compareTo("representation")) == 0) {

                    if ((subcategory.compareTo("byte_order")) == 0) {

                        // The Java default is big-endian.
                        // big-endian sets endianess true.
                        bigEndian = true;
                        littleEndian = false;

                        for (i = 0; i < numValues; i++) {

                            if (Integer.valueOf(values[i]).intValue() != (numValues - i)) {
                                bigEndian = false;
                            }
                        }

                        if (!bigEndian) {
                            littleEndian = true;

                            for (i = 0; i < numValues; i++) {

                                if (Integer.valueOf(values[i]).intValue() != (i + 1)) {
                                    littleEndian = false;
                                }
                            }
                        } // if (!bigEndian)

                        if (bigEndian) {
                            endianess = true;
                            fileInfo.setEndianess(endianess);
                        } else if (littleEndian) {
                            endianess = false;
                            fileInfo.setEndianess(endianess);
                        } else {
                            Preferences.debug("Order is not big or little endian\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("Cannot handle this ordering\n", Preferences.DEBUG_FILEIO);
                            throw new IOException("Order is not big or little endian");
                        }
                    } // if ((subcategory.compareTo("byte_order")) == 0)
                    else if ((subcategory.compareTo("format")) == 0) {

                        // The default is integer
                        format = values[0];

                        if (((format.compareTo("integer")) != 0) && ((format.compareTo("real")) != 0) &&
                                ((format.compareTo("complex")) != 0)) {
                            Preferences.debug("Illegal keyword of " + format + " for format\n", Preferences.DEBUG_FILEIO);
                            throw new IOException("Illegal keyword of " + format + " for format");
                        }
                    } // else if ((subcategory.compareTo("format")) == 0)
                    else if ((subcategory.compareTo("sign")) == 0) {

                        // The default is unsigned for integers and signed
                        // for real and complex formats
                        sign = values[0];

                        if (((sign.compareTo("signed")) != 0) && ((sign.compareTo("unsigned")) != 0)) {
                            Preferences.debug("Illegal keyword of " + sign + " for sign\n", Preferences.DEBUG_FILEIO);
                            throw new IOException("Illegal keyword of " + sign + " for sign");

                        }
                    } // else if ((subcategory.compareTo("sign")) == 0)
                    else if ((subcategory.compareTo("compression")) == 0) {
                        compression = values[0];

                        if ((compression.compareTo("uncompressed")) == 0) { }
                        else if ((compression.compareTo("gzip")) == 0) {
                            Preferences.debug("Must handle gzip compression\n", Preferences.DEBUG_FILEIO);
                            useGZIP = true;
                        } else {
                            Preferences.debug("Cannot handle compression = " + compression + "\n", Preferences.DEBUG_FILEIO);
                            throw new IOException("Cannot handle compression = " + compression + "\n");
                        }
                    } // else if ((subcategory.compareTo("compression")) == 0)
                    else if ((subcategory.compareTo("SCIL_TYPE")) == 0) {
                        scilType = values[0];
                        fileInfo.setScilType(scilType);
                    } // else if ((subcategory.compareTo("SCIL_TYPE")) == 0)
                } // else if ((category.compareTo("representation")) == 0)
                else if ((category.compareTo("source")) == 0) {

                    if ((subcategory.compareTo("file")) == 0) {
                        dataFileName = values[0];
                        Preferences.debug("Data file = " + dataFileName + "\n", Preferences.DEBUG_FILEIO);
                    } else if ((subcategory.compareTo("offset")) == 0) {
                        dataOffset = Long.valueOf(values[0]).longValue();
                        Preferences.debug("Data file offset = " + dataOffset + "\n", Preferences.DEBUG_FILEIO);
                    }
                } // else if ((category.compareTo("source")) == 0)
                else if ((category.compareTo("parameter")) == 0) {

                    if ((subcategory.compareTo("origin")) == 0) {
                        origin = new float[numValues];

                        for (i = 0; i < numValues; i++) {
                            origin[i] = Float.valueOf(values[i]).floatValue();
                        }
                    } else if ((subcategory.compareTo("scale")) == 0) {
                        scale = new float[numValues];

                        for (i = 0; i < numValues; i++) {
                            scale[i] = Float.valueOf(values[i]).floatValue();
                        }
                    } else if ((subcategory.compareTo("labels")) == 0) {
                        labels = new String[numValues];

                        for (i = 0; i < numValues; i++) {
                            labels[i] = values[i];
                        }
                    } else if ((subcategory.compareTo("units")) == 0) {
                        units = new String[numValues];

                        for (i = 0; i < numValues; i++) {
                            units[i] = values[i];
                        }
                    } else if ((subcategory.compareTo("probe")) == 0) {
                        probe = new String[numValues];

                        for (i = 0; i < numValues; i++) {
                            probe[i] = values[i];
                        }

                        fileInfo.setProbe(probe);
                    } else if ((subcategory.compareTo("capture_version")) == 0) {

                        if (numValues == 1) {
                            captureVersion = values[0];
                            fileInfo.setCaptureVersion(captureVersion);
                        }
                    } else if ((subcategory.compareTo("filter_exposure_timeX")) == 0) {

                        if (numValues == 1) {
                            filterExposureTimeX = values[0];
                            fileInfo.setFilterExposureTimeX(filterExposureTimeX);
                        }
                    } else if ((subcategory.compareTo("filter_fluorophoreX")) == 0) {
                        filterFluorophoreX = new String[numValues];

                        for (i = 0; i < numValues; i++) {
                            filterFluorophoreX[i] = values[i];
                        }

                        fileInfo.setFilterFluorophoreX(filterFluorophoreX);
                    } else if ((subcategory.compareTo("mapchannel")) == 0) {
                        mapchannel = new String[numValues];

                        for (i = 0; i < numValues; i++) {
                            mapchannel[i] = values[i];
                        }

                        fileInfo.setMapchannel(mapchannel);
                    } else if ((subcategory.compareTo("plate_chamber_id")) == 0) {

                        if (numValues == 1) {
                            plateChamberID = values[0];
                            fileInfo.setPlateChamberID(plateChamberID);
                        }
                    } else if ((subcategory.compareTo("plate_vessel_id")) == 0) {

                        if (numValues == 1) {
                            plateVesselID = values[0];
                            fileInfo.setPlateVesselID(plateVesselID);
                        }
                    } else if ((subcategory.compareTo("specimen")) == 0) {
                        specimen = new String[numValues];

                        for (i = 0; i < numValues; i++) {
                            specimen[i] = values[i];
                        }

                        fileInfo.setSpecimen(specimen);
                    } else if ((subcategory.compareTo("specimen_species")) == 0) {
                        specimenSpecies = new String[numValues];

                        for (i = 0; i < numValues; i++) {
                            specimenSpecies[i] = values[i];
                        }

                        fileInfo.setSpecimenSpecies(specimenSpecies);
                    }

                } // else if ((category.compareTo("parameter")) == 0)
                else if ((category.compareTo("sensor")) == 0) {

                    if ((subcategory.compareTo("type")) == 0) {
                        sensorType = values[0];
                        fileInfo.setSensorType(sensorType);
                    } else if ((subcategory.compareTo("model")) == 0) {
                        sensorModel = values[0];
                        fileInfo.setSensorModel(sensorModel);
                    } else if ((subcategory.compareTo("s_params")) == 0) {

                        if ((values[0].compareTo("Channels")) == 0) {
                            channels = new String[numValues];

                            for (i = 0; i < numValues; i++) {
                                channels[i] = values[i];
                            }

                            fileInfo.setChannels(channels);
                        } else if ((values[0].compareTo("PinholeRadius")) == 0) {
                            pinholeRadius = new String[numValues];

                            for (i = 0; i < numValues; i++) {
                                pinholeRadius[i] = values[i];
                            }

                            fileInfo.setPinholeRadius(pinholeRadius);
                        } else if ((values[0].compareTo("LambdaEx")) == 0) {
                            lambdaEx = new String[numValues];

                            for (i = 0; i < numValues; i++) {
                                lambdaEx[i] = values[i];
                            }

                            fileInfo.setLambdaEx(lambdaEx);
                        } else if ((values[0].compareTo("LambdaEm")) == 0) {
                            lambdaEm = new String[numValues];

                            for (i = 0; i < numValues; i++) {
                                lambdaEm[i] = values[i];
                            }

                            fileInfo.setLambdaEm(lambdaEm);
                        } else if ((values[0].compareTo("ExPhotonCnt")) == 0) {
                            exPhotonCnt = new String[numValues];

                            for (i = 0; i < numValues; i++) {
                                exPhotonCnt[i] = values[i];
                            }

                            fileInfo.setExPhotonCnt(exPhotonCnt);
                        } else if ((values[0].compareTo("RefrInxMedium")) == 0) {
                            refrInxMedium = new String[numValues];

                            for (i = 0; i < numValues; i++) {
                                refrInxMedium[i] = values[i];
                            }

                            fileInfo.setRefrInxMedium(refrInxMedium);
                        } else if ((values[0].compareTo("NumAperture")) == 0) {
                            numAperture = new String[numValues];

                            for (i = 0; i < numValues; i++) {
                                numAperture[i] = values[i];
                            }

                            fileInfo.setNumAperture(numAperture);
                        } else if ((values[0].compareTo("RefrInxLensMedium")) == 0) {
                            refrInxLensMedium = new String[numValues];

                            for (i = 0; i < numValues; i++) {
                                refrInxLensMedium[i] = values[i];
                            }

                            fileInfo.setRefrInxLensMedium(refrInxLensMedium);
                        }
                    } // else if ((subcategory.compareTo("s_params")) == 0)
                } // else if ((category.compareTo("sensor")) == 0)
                else if ((category.compareTo("history")) == 0) {

                    if (subcategory != null) {
                        history[historyNumber++] = subcategory;
                    }
                } // else if ((category.compareTo("history")) == 0)
                else if ((category.compareTo("end")) == 0) {
                    done = true;

                    if (dataFileName == null) {
                        dataFileName = headerFileName;
                        dataOffset = raFile.getFilePointer();
                    }
                }
            } // while(!done)

        } // try
        catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        if (historyNumber > 0) {
            actualHistory = new String[historyNumber];

            for (i = 0; i < historyNumber; i++) {
                actualHistory[i] = history[i];
            }

            fileInfo.setHistory(actualHistory);
        }

        if (parameters != order.length) {
            Preferences.debug("parameters = " + parameters + " but order.length = " + order.length + "\n", 
            		Preferences.DEBUG_FILEIO);
            throw new IOException("parameters = " + parameters + " but order.length = " + order.length);
        }

        if (parameters != sizes.length) {
            Preferences.debug("parameters = " + parameters + " but sizes.length = " + sizes.length + "\n", 
            		Preferences.DEBUG_FILEIO);
            throw new IOException("parameters = " + parameters + " but sizes.length = " + sizes.length);
        }

        if ((labels != null) && (parameters != labels.length)) {
            Preferences.debug("parameters = " + parameters + " but labels.length = " + labels.length + "\n", 
            		Preferences.DEBUG_FILEIO);
            throw new IOException("parameters = " + parameters + " but labels.length = " + labels.length);
        }

        if (rgbPos != -1) {
            numColors = sizes[rgbPos];

            if ((numColors > 4) || (!hasRGB && (numColors > 3))) {
                nDims++;
            }

            if ((rgbPos > xPos) && (rgbPos > yPos) && (rgbPos > zPos) && (rgbPos > tPos)) {
                colorSpacing = RGB_LAST;
            } else if ((rgbPos > xPos) && (rgbPos > yPos) && (rgbPos > zPos) && (rgbPos < tPos)) {
                colorSpacing = RGB_BETWEEN_SPACEANDTIME;
            } else if (((rgbPos < xPos) || (xPos == -1)) && ((rgbPos < yPos) || (yPos == -1)) &&
                           ((rgbPos < zPos) || (zPos == -1)) && ((rgbPos < tPos) || (tPos == -1))) {
                colorSpacing = RGB_FIRST;
            } else {
                Preferences.debug("Unexpected color parameter position\n", Preferences.DEBUG_FILEIO);
                throw new IOException("Unexpected color parameter position");
            }
        }

        if (nDims > 4) {
            Preferences.debug("Cannot handle image with " + nDims + " dimensions\n", Preferences.DEBUG_FILEIO);
            throw new IOException("Cannot handle image with " + nDims + " dimensions");
        }


        if ((hasRGB && (numColors >= 2) && (numColors <= 4)) || ((numColors == 2) || (numColors == 3))) {

            // 2 or 3 dyes or spectra so use color
            if (((sizes[0] == 8) && ((format.compareTo("integer")) == 0)) &&
                    ((sign == null) || ((sign.compareTo("unsigned")) == 0))) {
                dataType = ModelStorageBase.ARGB;
                Preferences.debug("Data type is ARGB\n", Preferences.DEBUG_FILEIO);
            } else if (((sizes[0] == 16) && ((format.compareTo("integer")) == 0)) &&
                           ((sign == null) || ((sign.compareTo("unsigned")) == 0))) {
                dataType = ModelStorageBase.ARGB_USHORT;
                Preferences.debug("Data type is ARGB_USHORT\n", Preferences.DEBUG_FILEIO);
            } else if (((sizes[0] == 32) && ((format.compareTo("real")) == 0)) &&
                           ((sign == null) || ((sign.compareTo("signed")) == 0))) {
                dataType = ModelStorageBase.ARGB_FLOAT;
                Preferences.debug("Data type is ARGB_FLOAT\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("Cannot handle " + numColors + " color data with " + sizes[0] + " " + sign +
                                  " bits and " + format + " format\n", Preferences.DEBUG_FILEIO);
                throw new IOException("Cannot handle " + numColors + " color data with " + sizes[0] + " " + sign +
                                      " bits and " + format + " format");

            }

        } // if color
        else { // not color or more than 3 spectral color

            if (((sizes[0] == 8) && ((format.compareTo("integer")) == 0)) &&
                    ((sign == null) || ((sign.compareTo("unsigned")) == 0))) {
                dataType = ModelStorageBase.UBYTE;
                Preferences.debug("Data type is UBYTE\n", Preferences.DEBUG_FILEIO);
            } else if ((sizes[0] == 8) && ((format.compareTo("integer")) == 0) && ((sign.compareTo("signed")) == 0)) {
                dataType = ModelStorageBase.BYTE;
                Preferences.debug("Data type is BYTE\n", Preferences.DEBUG_FILEIO);
            } else if (((sizes[0] == 16) && ((format.compareTo("integer")) == 0)) &&
                           ((sign == null) || ((sign.compareTo("unsigned")) == 0))) {
                dataType = ModelStorageBase.USHORT;
                Preferences.debug("Data type is USHORT\n", Preferences.DEBUG_FILEIO);
            } else if ((sizes[0] == 16) && ((format.compareTo("integer")) == 0) && ((sign.compareTo("signed")) == 0)) {
                dataType = ModelStorageBase.SHORT;
                Preferences.debug("Data type is SHORT\n", Preferences.DEBUG_FILEIO);
            } else if (((sizes[0] == 32) && ((format.compareTo("integer")) == 0)) &&
                           ((sign == null) || ((sign.compareTo("unsigned")) == 0))) {
                dataType = ModelStorageBase.UINTEGER;
                Preferences.debug("Data type is UINTEGER\n", Preferences.DEBUG_FILEIO);
            } else if ((sizes[0] == 32) && ((format.compareTo("integer")) == 0) && ((sign.compareTo("signed")) == 0)) {
                dataType = ModelStorageBase.INTEGER;
                Preferences.debug("Data type is INTEGER\n", Preferences.DEBUG_FILEIO);
            } else if ((sizes[0] == 64) && ((format.compareTo("integer")) == 0) && ((sign.compareTo("signed")) == 0)) {
                dataType = ModelStorageBase.LONG;
                Preferences.debug("Data type is LONG\n", Preferences.DEBUG_FILEIO);
            } else if (((sizes[0] == 32) && ((format.compareTo("real")) == 0)) &&
                           ((sign == null) || ((sign.compareTo("signed")) == 0))) {
                dataType = ModelStorageBase.FLOAT;
                Preferences.debug("Data type is FLOAT\n", Preferences.DEBUG_FILEIO);
            } else if (((sizes[0] == 64) && ((format.compareTo("real")) == 0)) &&
                           ((sign == null) || ((sign.compareTo("signed")) == 0))) {
                dataType = ModelStorageBase.DOUBLE;
                Preferences.debug("Data type is DOUBLE\n", Preferences.DEBUG_FILEIO);
            } else if (((sizes[0] == 64) && ((format.compareTo("complex")) == 0)) &&
                           ((sign == null) || ((sign.compareTo("signed")) == 0))) {
                dataType = ModelStorageBase.COMPLEX;
                Preferences.debug("Data type is COMPLEX\n", Preferences.DEBUG_FILEIO);
            } else if (((sizes[0] == 128) && ((format.compareTo("complex")) == 0)) &&
                           ((sign == null) || ((sign.compareTo("signed")) == 0))) {
                dataType = ModelStorageBase.DCOMPLEX;
                Preferences.debug("Data type is DCOMPLEX\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("Cannot handle data with " + sizes[0] + " " + sign + " bits and " + format +
                                  " format\n", Preferences.DEBUG_FILEIO);
                throw new IOException("Cannot handle data with " + sizes[0] + " " + sign + " bits and " + format +
                                      " format");
            }
        } // else not color

        fileInfo.setDataType(dataType);

        imgExtents = new int[nDims];
        i = 0;

        if (xPos >= 0) {
            imgExtents[i++] = sizes[xPos];
        }

        if (yPos >= 0) {
            imgExtents[i++] = sizes[yPos];
        }

        if (zPos >= 0) {
            imgExtents[i++] = sizes[zPos];
        }

        if (tPos >= 0) {
            imgExtents[i++] = sizes[tPos];
        }

        if ((numColors > 4) || (!hasRGB && (numColors > 3))) {
            imgExtents[i] = sizes[rgbPos];
        }

        fileInfo.setExtents(imgExtents);

        if (origin != null) {
            i = 0;
            startLocations = new float[nDims];

            if (origin.length == parameters) {

                if (xPos >= 0) {
                    startLocations[i++] = origin[xPos];
                }

                if (yPos >= 0) {
                    startLocations[i++] = origin[yPos];
                }

                if (zPos >= 0) {
                    startLocations[i++] = origin[zPos];
                }

                if (tPos >= 0) {
                    startLocations[i++] = origin[tPos];
                }

                if ((numColors > 4) || (!hasRGB && (numColors > 3))) {
                    startLocations[i] = 0.0f;
                }
            }

            if ((origin.length == (parameters - 1)) && (rgbPos >= 0) && (rgbPos < xPos)) {

                // No origin for rgb parameter
                if (xPos >= 0) {
                    startLocations[i++] = origin[xPos - 1];
                }

                if (yPos >= 0) {
                    startLocations[i++] = origin[yPos - 1];
                }

                if (zPos >= 0) {
                    startLocations[i++] = origin[zPos - 1];
                }

                if (tPos >= 0) {
                    startLocations[i++] = origin[tPos - 1];
                }

                if ((numColors > 4) || (!hasRGB && (numColors > 3))) {
                    startLocations[i] = 0.0f;
                }
            }

            fileInfo.setOrigin(startLocations);
        }

        if (scale != null) {
            i = 0;
            imgResols = new float[nDims];

            if (scale.length == parameters) {

                if (xPos >= 0) {
                    imgResols[i++] = scale[xPos];
                }

                if (yPos >= 0) {
                    imgResols[i++] = scale[yPos];
                }

                if (zPos >= 0) {
                    imgResols[i++] = scale[zPos];
                }

                if (tPos >= 0) {
                    imgResols[i] = scale[tPos];
                }
            }

            if ((scale.length == (parameters - 1)) && (rgbPos >= 0) && (rgbPos < xPos)) {

                // No resolution  for rgb parameter
                if (xPos >= 0) {
                    imgResols[i++] = scale[xPos - 1];
                }

                if (yPos >= 0) {
                    imgResols[i++] = scale[yPos - 1];
                }

                if (zPos >= 0) {
                    imgResols[i++] = scale[zPos - 1];
                }

                if (tPos >= 0) {
                    imgResols[i] = scale[tPos - 1];
                }
            }

            fileInfo.setResolutions(imgResols);
        } // if (scale != null)


        if (labels != null) {

            if (exchangeXY) {
                sTemp = labels[xPos];
                labels[xPos] = labels[yPos];
                labels[yPos] = sTemp;
            }

            fileInfo.setLabels(labels);
        }

        if (units != null) {
            i = 0;
            unitsStr = new String[nDims];
            unitsOfMeasure = new int[nDims];

            if (units.length == parameters) {

                if (xPos >= 0) {
                    unitsStr[i++] = units[xPos];
                }

                if (yPos >= 0) {
                    unitsStr[i++] = units[yPos];
                }

                if (zPos >= 0) {
                    unitsStr[i++] = units[zPos];
                }

                if (tPos >= 0) {
                    unitsStr[i++] = units[tPos];
                }

                if ((numColors > 4) || (!hasRGB && (numColors > 3))) {
                    unitsStr[i] = units[rgbPos];
                }
            }

            if (((units.length == nDims) && ((hasRGB && (numColors <= 4)) || (numColors <= 3))) ||
                 ((units.length == (nDims - 1)) && ((numColors > 4) || (!hasRGB && (numColors > 3))))) {

                for (i = 0; i < units.length; i++) {
                    unitsStr[i] = units[i];
                }

                if (units.length == (nDims - 1)) {
                    unitsStr[nDims - 1] = "undefined";
                }
            }


            for (i = 0; i < nDims; i++) {

                if (unitsStr[i].equalsIgnoreCase("cm")) {
                    unitsOfMeasure[i] = Unit.CENTIMETERS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("centimeters")) {
                    unitsOfMeasure[i] = Unit.CENTIMETERS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("mm")) {
                    unitsOfMeasure[i] = Unit.MILLIMETERS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("millimeters")) {
                    unitsOfMeasure[i] = Unit.MILLIMETERS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("um")) {
                    unitsOfMeasure[i] = Unit.MICROMETERS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("mic")) {
                    unitsOfMeasure[i] = Unit.MICROMETERS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("micrometers")) {
                    unitsOfMeasure[i] = Unit.MICROMETERS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("nm")) {
                    unitsOfMeasure[i] = Unit.NANOMETERS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("nanometers")) {
                    unitsOfMeasure[i] = Unit.NANOMETERS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("A")) {
                    unitsOfMeasure[i] = Unit.ANGSTROMS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("angstroms")) {
                    unitsOfMeasure[i] = Unit.ANGSTROMS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("sec")) {
                    unitsOfMeasure[i] = Unit.SECONDS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("seconds")) {
                    unitsOfMeasure[i] = Unit.SECONDS.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("msec")) {
                    unitsOfMeasure[i] = Unit.MILLISEC.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("milliseconds")) {
                    unitsOfMeasure[i] = Unit.MILLISEC.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("usec")) {
                    unitsOfMeasure[i] = Unit.MICROSEC.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("microseconds")) {
                    unitsOfMeasure[i] = Unit.MICROSEC.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("nsec")) {
                    unitsOfMeasure[i] = Unit.NANOSEC.getLegacyNum();
                } else if (unitsStr[i].equalsIgnoreCase("nanoseconds")) {
                    unitsOfMeasure[i] = Unit.NANOSEC.getLegacyNum();
                } else {
                    unitsOfMeasure[i] = Unit.UNKNOWN_MEASURE.getLegacyNum();
                }
            } // for (i = 0; i < nDims; i++)

            fileInfo.setUnitsOfMeasure(unitsOfMeasure);
        } // if (units != null)

        raFile.close();

        return true;
    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice   offset into the file stored in the dataOffset array
     * @param      buffer  buffer where the info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readLBuffer(int slice, long[] buffer) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        long b1, b2, b3, b4, b5, b6, b7, b8;
        byte[] byteBuffer;
        int progress, progressLength, mod;

        if (dataType == ModelStorageBase.UINTEGER) { // reading 4 byte unsigned integers
            byteBuffer = new byte[4 * buffer.length];
            nBytes = 4 * buffer.length;
            raFile.read(byteBuffer, 0, nBytes);
            progress = slice * buffer.length;
            progressLength = buffer.length * numberSlices;
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
                    buffer[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4) & 0xffffffffL;
                } else {
                    buffer[i] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1) & 0xffffffffL;
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

                if (((i + progress) % mod) == 0) {
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

                if (endianess) {
                    buffer[i] = ((b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) | (b5 << 24) | (b6 << 16) |
                                     (b7 << 8) | b8); // Big Endian
                } else {
                    buffer[i] = ((b8 << 56) | (b7 << 48) | (b6 << 40) | (b5 << 32) | (b4 << 24) | (b3 << 16) |
                                     (b2 << 8) | b1); // Little Endian
                }
            } // for (j =0; j < nBytes; j+=8, i++ )
        } // else reading 8 byte integers
    }

    /**
     * readLine() - reads a line of the file header separate into category, subcategory, and values.
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readLine() throws IOException {
        String tempString;
        int index;
        numValues = 0;

        try {
            tempString = raFile.readLine();
        } catch (IOException error) {
            throw (error);
        }

        if (tempString == null) {
            Preferences.debug("Null header line", Preferences.DEBUG_FILEIO);
            throw new IOException("Null header line");
        }

        index = tempString.indexOf(fieldSeparator);

        if (index != -1) {
            category = tempString.substring(0, index);

            if (category.equals("end")) {
                return;
            }
        } else if (tempString.equals("history")) {
            category = tempString;
            subcategory = null;

            return;
        } else {
            Preferences.debug("Field separator between category and subcategory not found\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("Header line = " + tempString, Preferences.DEBUG_FILEIO);
            throw new IOException("Field separator between category and subcategory not found\n");
        }

        tempString = tempString.substring(index + 1);

        if (category.equals("history")) {
            subcategory = tempString;

            return;
        }

        index = tempString.indexOf(fieldSeparator);

        if (index != -1) {
            subcategory = tempString.substring(0, index);
        } else if (category.equals("parameter")) {
            subcategory = tempString;

            return;
        } else {
            Preferences.debug("Field separator between subcategory and keyword not found\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("Header line from subcategory = " + tempString, Preferences.DEBUG_FILEIO);
            throw new IOException("Field separator between subcategory and keyword not found\n");
        }

        tempString = tempString.substring(index + 1);
        index = tempString.indexOf(fieldSeparator);

        while (index != -1) {
            values[numValues++] = tempString.substring(0, index);
            tempString = tempString.substring(index + 1);
            index = tempString.indexOf(fieldSeparator);
        }

        values[numValues++] = tempString;

        return;
    }

}
