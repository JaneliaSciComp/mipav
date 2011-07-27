package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.actions.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * AVI file writer and reader.
 *
 * @version  1.1 March 5, 2011
 * @author   William Gandler, Matthew J. McAuliffe, Ph.D. The Microsoft Video 1 decoding was mostly derived from Mike
 *           A good source of sample avi files is http://samples.mplayerhq.hu.
 */

public class FileAvi extends FileBase {
    
    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    @SuppressWarnings("unused")
    private boolean AVIF_ISINTERLEAVED;

    /** globals needed for read - set in readHeader, used in readImage. */
    private boolean AVIF_MUSTUSEINDEX;
    
    private boolean AVIF_HASINDEX;

    /** DOCUMENT ME! */
    private short bitCount;

    /** DOCUMENT ME! */
    private int blankFramesSent;

    /** DOCUMENT ME! */
    private byte[] bufferWrite;

    /**
     * 0 for RGB 24 bit per pixel uncompressed 1 for RLE 8 bit per pixel compressed 1296126531 for Microsoft video 1
     * compression.
     */
    private int compression = -1; //

    /** DOCUMENT ME! */
    private float compressionQuality = 0.80f;

    /** DOCUMENT ME! */
    private int dataFramesSent;

    /** globals needed for write. */
    private byte[] dataSignature;

    /** DOCUMENT ME! */
    private int[] dcLength = null;

    /** true for big-endian and false for little-endian. */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoAvi fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private int height;

    /** DOCUMENT ME! */
    private long idx1Position;

    /** DOCUMENT ME! */
    private ModelImage imageA;

    /** DOCUMENT ME! */
    private ModelImage imageB = null;

    /** DOCUMENT ME! */
    private float[] imageBufferA;

    /** DOCUMENT ME! */
    private float[] imageBufferB;

    /** DOCUMENT ME! */
    private float imageMinA;

    /** DOCUMENT ME! */
    private long indexPointer;

    /** Size of the index block in bytes. */
    private int indexSize;

    /** for saving within scripts. */
    private boolean isScript = false;

    /** DOCUMENT ME! */
    private int LIST2Size;

    /** DOCUMENT ME! */
    private int LIST2subchunkSize;

    /** DOCUMENT ME! */
    private ModelLUT LUTa;

    /** DOCUMENT ME! */
    private byte[] lutBuffer = null;

    /** DOCUMENT ME! */
    private int[] lutBufferRemapped = null;

    /** DOCUMENT ME! */
    private int microSecPerFrame = (int) ((1.0f / Preferences.getDefaultFrameRate()) * 1000000);
    
    private int scale;
    
    private int rate;

    /** DOCUMENT ME! */
    private long moviPosition;

    /** DOCUMENT ME! */
    private long moviSubchunkPosition;

    /** 2 for .mov, 3 for mjpeg, 4 for mp4v2. */
    private int newCompressionType = 0;

    /** DOCUMENT ME! */
    @SuppressWarnings("unused")
    private ProgressBarInterface progressBar = null;

    /** DOCUMENT ME! */
    private boolean readQT = false;

    /** DOCUMENT ME! */
    private float remapConstA;

    /** DOCUMENT ME! */
    private long[] savedbLength;

    /** DOCUMENT ME! */
    private long saveFileSize; // location of file size in bytes not counting first 8 bytes

    /** DOCUMENT ME! */
    private long saveLIST2Size;

    /** DOCUMENT ME! */
    private long savemovi;

    /** DOCUMENT ME! */
    private int streams;

    /** DOCUMENT ME! */
    private int totalBlankFrames;

    /** These are used for writing the AVI frame by frame from the surface renderer. */
    private int totalDataFrames;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private boolean useNewCompression = false;

    /** DOCUMENT ME! */
    private int width;

    /** DOCUMENT ME! */
    private boolean writeQT = false;

    /** DOCUMENT ME! */
    private int xDim, yDim, zDim, tDim;

    /** DOCUMENT ME! */
    private int xPad;
    
    private String outputFileName;
    
    private float captureTime;
    
    private float skipTime;
    
    private File fileW;
    private RandomAccessFile raFileW;
    
    private int framesToCapture;
    
    private int framesToSkip;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Avi reader/writer constructor.
     *
     * @param      fileName  File name.
     * @param      fileDir   File directory.
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileAvi(String fileName, String fileDir) throws IOException {

        UI = ViewUserInterface.getReference();
        this.fileName = fileName;
        this.fileDir = fileDir;
    }
    
    public void setCaptureTime(float captureTime) {
        this.captureTime = captureTime;
    }
    
    public void setSkipTime(float skipTime) {
        this.skipTime = skipTime;
    }
    
    public void setOutputFileName(String outputFileName) {
        this.outputFileName = outputFileName;
    }
    
    public int getMicroSecPerFrame() {
        return microSecPerFrame;
    }
    
    // Rate/scale = samples/second
    public int getScale() {
        return scale;
    }
    
    public int getRate() {
        return rate;
    }
    
    public boolean getHasIndex() {
        return AVIF_HASINDEX;
    }
    
    public boolean getMustUseIndex() {
        return AVIF_MUSTUSEINDEX;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void close() {

        try {
            raFile.close();
        } catch (IOException error) { }

    }

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {
        fileName = null;
        fileDir = null;
        UI = null;
        file = null;
        fileInfo = null;
        imageA = null;
        imageB = null;

        LUTa = null;
        imageBufferA = null;
        imageBufferB = null;
        lutBuffer = null;
        lutBufferRemapped = null;
        bufferWrite = null;
        dcLength = null;
        savedbLength = null;

        try {
            super.finalize();
        } catch (Throwable er) { }
    }

    /**
     * Returns fileDir.
     *
     * @return  fileDir
     */
    public String getFileDir() {
        return this.fileDir;
    }

    /**
     * Returns fileName.
     *
     * @return  filename
     */
    public String getFileName() {
        return this.fileName;
    }

    /**
     * Returns LUT if defined.
     *
     * @return  The LUT if defined, otherwise null.
     */
    public ModelLUT getModelLUT() {
        return LUTa;
    }

    /**
     * Reads the AVI file header and data.
     *
     * @param      one  if true only reads one slice per file
     *
     * @return     An ARGB image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean one) throws IOException {
        int[] imgExtents;
        byte[] imgBuffer;
        byte[] fileBuffer;
        int bufferSize;
        int x, y, z, k;
        int col1, col2, totalC;
        int totalDataArea;
        int remainingFileLength;
        int totalBytesRead;
        int dataLength;
        boolean dataFound;
        int moviOffset;
        int signature;
        int CHUNKtype;
        boolean haveMoviSubchunk = false;
        @SuppressWarnings("unused")
        int subchunkDataArea = 0;
        int subchunkBytesRead = 0;
        int subchunkBlocksRead = 0;
        boolean chunkRead;
        long startPosition; // position to start reading data
        int actualFrames = 0; // number of frames with data found on first read thru.
        int indexBytesRead = 0;
        long firstDataSignature;
        boolean firstRun;

        boolean wasCompressed = false;

        System.err.println("AVI readImage(" + one + ")");

        try {

            // if file is QuickTime... we must transcode before reading the header
            // else if file is compressed (readHeader > 0) we must transcode, then
            // re-read the header with the newly created uncompressedRGB avi
            if (readQT || (readHeader() > 0)) {
                wasCompressed = true;

                String newFileName = fileName.substring(0, fileName.length() - 4) + "_RGB.avi";

                AlgorithmTranscode at = new AlgorithmTranscode(new File(fileDir + fileName).toURI().toURL(),
                                                               fileDir + newFileName, AlgorithmTranscode.TRANSCODE_RGB);

                //at.setQuality(compressionQuality);
                at.setQuality(1);
                at.run();

                // set the filename to the new file name and read header again
                this.fileName = newFileName;
                // The line 
                // oml = new MediaLocator(outputFile.toURI().toURL()); 
                // in AlgorithmTranscode.runAlgorithm() replaces every space in the file path name
                // with %20.
                for (int i = 0; i < fileDir.length(); i++) {
                    if (fileDir.charAt(i) == 0x20) {
                        fileDir = fileDir.substring(0,i) + "%20" + fileDir.substring(i+1);
                    }
                }

                if (readHeader() != 0) {
                	Preferences.debug("FileAVI.readImage: could not read avi image transcoded to RGB-AVI\n",
                			Preferences.DEBUG_FILEIO);
                    System.err.println("FileAVI.readImage: could not read avi image transcoded to RGB-AVI");
                }
            }

            startPosition = raFile.getFilePointer();
            // Do first read thru the data to find the actual number of frames used by MIPAV. This must be done before
            // the MIPAV image can be created.

            dataSignature = new byte[4];

            totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes

            // Have encountered LiST2Size > raFile.length(), an impossibility
            remainingFileLength = (int) (raFile.length() - startPosition);

            if (totalDataArea > remainingFileLength) {
                Preferences.debug("File appears to be truncated\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("totalDataArea = " + totalDataArea + " remainingFileLength = " + remainingFileLength + "\n",
                		Preferences.DEBUG_FILEIO);
                totalDataArea = remainingFileLength;
            }

            totalBytesRead = 0;

            // Check for LIST rec<sp> subchunks
            if (!AVIF_MUSTUSEINDEX) {
                signature = getInt(endianess);

                if (signature == 0x5453494C) {

                    // have read LIST
                    LIST2subchunkSize = getInt(endianess);
                    moviSubchunkPosition = raFile.getFilePointer();
                    CHUNKtype = getInt(endianess);

                    if (CHUNKtype == 0x20636572) {

                        // have read rec<sp>
                        haveMoviSubchunk = true;
                        Preferences.debug("LIST rec found\n", Preferences.DEBUG_FILEIO);
                        subchunkDataArea = LIST2subchunkSize - 4;
                        subchunkBytesRead = 0;
                        subchunkBlocksRead = 0;
                    } else {
                        raFile.close();
                        throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                    }
                } else {
                    raFile.seek(startPosition);
                }
            } // if (!AVIF_MUSTUSEINDEX)

            chunkRead = true;

            firstDataSignature = raFile.getFilePointer();
            firstRun = true;
            loop1:
            while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                       (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {

                if (AVIF_MUSTUSEINDEX) {
                    raFile.seek(indexPointer);
                    dataFound = false;

                    while (!dataFound) {
                        raFile.read(dataSignature);

                        if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                (dataSignature[3] > 0x63 /* c */)) {
                            indexPointer = indexPointer + 16;
                            indexBytesRead += 16;
                            if (indexBytesRead >= indexSize) {
                                break loop1;
                            }
                            raFile.seek(indexPointer);
                        } else {
                            dataFound = true;
                        }
                    } // while (!dataFound)

                    indexPointer = indexPointer + 8;
                    raFile.seek(indexPointer);
                    moviOffset = getInt(endianess);
                    if (firstRun && (moviOffset == firstDataSignature)) {
                        moviPosition = 0L;
                    }
                    indexPointer = indexPointer + 8;
                    indexBytesRead += 16;
                    raFile.seek(moviPosition + (long) moviOffset);
                    firstRun = false;
                } // if (AVIFMUSTINDEX)

                raFile.read(dataSignature);
                totalBytesRead = totalBytesRead + 4;
                subchunkBytesRead = subchunkBytesRead + 4;

                if ((dataSignature[2] == 0x64 /* d */) &&
                        ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                    dataLength = getInt(endianess);

                    if ((dataLength % 2) == 1) {
                        dataLength++;
                    }

                    if (dataLength > 0) {
                        actualFrames++;
                    }

                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    long ptr = raFile.getFilePointer();
                    raFile.seek(ptr + dataLength);
                    totalBytesRead = totalBytesRead + dataLength;
                    subchunkBytesRead += dataLength;
                } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                else {
                    dataLength = getInt(endianess);

                    if ((dataLength % 2) == 1) {
                        dataLength++;
                    }

                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    long ptr = raFile.getFilePointer();
                    raFile.seek(ptr + dataLength);
                    totalBytesRead = totalBytesRead + dataLength;
                    subchunkBytesRead += dataLength;
                } // else

                subchunkBlocksRead++;

                if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                    totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                    raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                    // Check for LIST rec<sp> subchunks
                    signature = getInt(endianess);
                    totalBytesRead += 4;

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        totalBytesRead += 4;
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            totalBytesRead += 4;
                            subchunkDataArea = LIST2subchunkSize - 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                        }
                    } else {
                        chunkRead = false;
                    }
                } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
            } // while ((totalBytesRead < totalDataArea) && chunkRead)

            Preferences.debug("totalBytesRead = " + totalBytesRead + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("totalDataArea = " + totalDataArea + "\n", Preferences.DEBUG_FILEIO);
            indexPointer = idx1Position + 8;
            indexBytesRead = 0;

            if (actualFrames > 1) {
                imgExtents = new int[3];
                imgExtents[2] = actualFrames;
            } else {
                imgExtents = new int[2];
            }

            fileInfo.setNumFrames(actualFrames);
            imgExtents[0] = width;
            imgExtents[1] = height;
            fileInfo.setExtents(imgExtents);

            int[] moreExtents;

            if (one) {
                moreExtents = new int[2];
                moreExtents[0] = width;
                moreExtents[1] = height;
            } else {
                moreExtents = imgExtents;
            }

            if ((bitCount == 16) || (bitCount == 24) || (bitCount == 32)) {
                fileInfo.setDataType(ModelStorageBase.ARGB);
                imageA = new ModelImage(ModelStorageBase.ARGB, moreExtents, fileName);
            } else if ((bitCount == 4) || (bitCount == 8)) {
                fileInfo.setDataType(ModelStorageBase.UBYTE);
                imageA = new ModelImage(ModelStorageBase.UBYTE, moreExtents, fileName);
            }

            // Now that the image is created this second read thru actually imports the data into the image.
            raFile.seek(startPosition);

            int middleSlice = 0;

            if (imgExtents.length > 2) {
                middleSlice = imgExtents[2] / 2;
            }

            if (compression == 0) {

                if ((bitCount == 16) || (bitCount == 24) || (bitCount == 32)) {
                    bufferSize = 4 * imgExtents[0] * imgExtents[1];
                } else { // bitCount == 8
                    bufferSize = imgExtents[0] * imgExtents[1];
                }

                imgBuffer = new byte[bufferSize];
                dataSignature = new byte[4];

                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes

                // Have encountered LiST2Size > raFile.length(), an impossibility
                remainingFileLength = (int) (raFile.length() - startPosition);

                if (totalDataArea > remainingFileLength) {
                    totalDataArea = remainingFileLength;
                }

                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (!AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkDataArea = LIST2subchunkSize - 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (1AVIF_MUSTUSEINDEX)

                x = 0;
                y = imgExtents[1] - 1;
                z = 0;
                chunkRead = true;

                loop2:
                while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                if (indexBytesRead >= indexSize) {
                                    break loop2;
                                }
                                raFile.seek(indexPointer);
                            } else {
                                dataFound = true;
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                            (dataSignature[3] > 0x63 /* c */)) {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += 4;
                    } else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        if ((totalBytesRead + dataLength) <= totalDataArea) {

                            if (one && (z != middleSlice)) {
                                long ptr = raFile.getFilePointer();
                                raFile.seek(ptr + dataLength);
                                totalBytesRead = totalBytesRead + dataLength;
                                subchunkBytesRead += dataLength;
                                z++;
                            } else {
                                fileBuffer = new byte[dataLength];
                                raFile.read(fileBuffer);
                                totalBytesRead = totalBytesRead + dataLength;
                                subchunkBytesRead += dataLength;

                                if (bitCount == 24) {

                                    for (int j = 0; j < dataLength; j = j + 3) {
                                        k = 4 * (x + (imgExtents[0] * y));
                                        imgBuffer[k] = (byte) 255;
                                        imgBuffer[k + 1] = fileBuffer[j + 2];
                                        imgBuffer[k + 2] = fileBuffer[j + 1];
                                        imgBuffer[k + 3] = fileBuffer[j];
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            x = 0;
                                            y--;
                                        } else if ((x == imgExtents[0]) && (y == 0)) {
                                            x = 0;
                                            y = imgExtents[1] - 1;

                                            if (one) {
                                                imageA.importData(0, imgBuffer, false);
                                            } else {
                                                imageA.importData(z * bufferSize, imgBuffer, false);
                                            }

                                            fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                            z++;
                                        } // else if ((x == imgExtents[0] && (y == 0))
                                    } // for (j = 0; j < datalength;j=j+3)
                                } // if (bitCount == 24)
                                else if (bitCount == 32) {

                                    for (int j = 0; j < dataLength; j = j + 4) {
                                        k = 4 * (x + (imgExtents[0] * y));
                                        imgBuffer[k] = (byte) 255;
                                        imgBuffer[k + 1] = fileBuffer[j + 2];
                                        imgBuffer[k + 2] = fileBuffer[j + 1];
                                        imgBuffer[k + 3] = fileBuffer[j];
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            x = 0;
                                            y--;
                                        } else if ((x == imgExtents[0]) && (y == 0)) {
                                            x = 0;
                                            y = imgExtents[1] - 1;

                                            if (one) {
                                                imageA.importData(0, imgBuffer, false);
                                            } else {
                                                imageA.importData(z * bufferSize, imgBuffer, false);
                                            }

                                            fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                            z++;
                                        } // else if ((x == imgExtents[0] && (y == 0))
                                    } // for (j = 0; j < datalength;j=j+4)
                                } // else if (bitCount == 32)
                                else if (bitCount == 16) {

                                    for (int j = 0; j < dataLength;) {
                                        k = 4 * (x + (imgExtents[0] * y));
                                        col1 = fileBuffer[j++] & 0xff;
                                        col2 = fileBuffer[j++] & 0xff;
                                        totalC = (col2 << 8) | col1;
                                        imgBuffer[k] = (byte) 255;
                                        imgBuffer[k + 1] = (byte) ((totalC >> 7) & 0xF8);
                                        imgBuffer[k + 2] = (byte) ((totalC >> 2) & 0xF8);
                                        imgBuffer[k + 3] = (byte) ((totalC << 3) & 0xF8);
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            x = 0;
                                            y--;
                                        } else if ((x == imgExtents[0]) && (y == 0)) {
                                            x = 0;
                                            y = imgExtents[1] - 1;

                                            if (one) {
                                                imageA.importData(0, imgBuffer, false);
                                            } else {
                                                imageA.importData(z * bufferSize, imgBuffer, false);
                                            }

                                            fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                            z++;
                                        } // else if ((x == imgExtents[0] && (y == 0))
                                    } // for (int j = 0; j < dataLength;)
                                } // else if (bitCount == 16)
                                else if (bitCount == 8) {

                                    // Rows are stored in multiples of 4
                                    // so extra bytes may appear at the end
                                    int xPad = 0;
                                    int xMod = imgExtents[0] % 4;

                                    if (xMod != 0) {
                                        xPad = 4 - xMod;
                                    }

                                    for (int j = 0; j < dataLength; j++) {
                                        k = x + (imgExtents[0] * y);
                                        imgBuffer[k] = fileBuffer[j];
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            j = j + xPad;
                                            x = 0;
                                            y--;
                                        } else if ((x == imgExtents[0]) && (y == 0)) {
                                            j = j + xPad;
                                            x = 0;
                                            y = imgExtents[1] - 1;

                                            if (one) {
                                                imageA.importData(0, imgBuffer, false);
                                            } else {
                                                imageA.importData(z * bufferSize, imgBuffer, false);
                                            }

                                            fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                            z++;
                                        } // else if ((x == imgExtents[0]) && (y == 0))
                                    } // for (j = 0; j < dataLength; j++)
                                } // else if (bitCount == 8)
                                else if (bitCount == 4) {

                                    // Rows are stored in multiples of 4
                                    // so extra bytes may appear at the end
                                    int xPad = 0;
                                    int rowBytes = imgExtents[0]/2 + imgExtents[0]%2;
                                    int xMod = rowBytes % 4;

                                    if (xMod != 0) {
                                        xPad = 4 - xMod;
                                    }

                                    for (int j = 0; j < dataLength; j++) {
                                        k = x + (imgExtents[0] * y);
                                        imgBuffer[k] = (byte)(fileBuffer[j] & 0x0f);
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            j = j + xPad;
                                            x = 0;
                                            y--;
                                        } else if ((x == imgExtents[0]) && (y == 0)) {
                                            j = j + xPad;
                                            x = 0;
                                            y = imgExtents[1] - 1;

                                            if (one) {
                                                imageA.importData(0, imgBuffer, false);
                                            } else {
                                                imageA.importData(z * bufferSize, imgBuffer, false);
                                            }

                                            fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                            z++;
                                        } // else if ((x == imgExtents[0]) && (y == 0))
                                        else {
                                            imgBuffer[k+1] = (byte)((fileBuffer[j] >> 4) & 0x0f);
                                            x++;
    
                                            if ((x == imgExtents[0]) && (y > 0)) {
                                                j = j + xPad;
                                                x = 0;
                                                y--;
                                            } else if ((x == imgExtents[0]) && (y == 0)) {
                                                j = j + xPad;
                                                x = 0;
                                                y = imgExtents[1] - 1;
    
                                                if (one) {
                                                    imageA.importData(0, imgBuffer, false);
                                                } else {
                                                    imageA.importData(z * bufferSize, imgBuffer, false);
                                                }
    
                                                fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                                z++;
                                            } // else if ((x == imgExtents[0]) && (y == 0))
                                        } // else
                                    } // for (j = 0; j < dataLength; j++)
                                } // else if (bitCount == 4)
                            } // else
                        } // if ((totalBytesRead + dataLength) <= totalDataArea)
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkDataArea = LIST2subchunkSize - 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < (totalDataArea-8)) && chunkRead)
            } // if (compression == 0)
            else if (compression == 1) {
                bufferSize = imgExtents[0] * imgExtents[1];
                imgBuffer = new byte[bufferSize];
                dataSignature = new byte[4];
                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes
                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkDataArea = LIST2subchunkSize - 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (!AVIF_MUSTUSEINDEX)

                x = 0;
                y = imgExtents[1] - 1;
                z = 0;
                chunkRead = true;

                loop3:
                while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                if (indexBytesRead >= indexSize) {
                                    break loop3;
                                }
                                raFile.seek(indexPointer);
                            } else {
                                dataFound = true;
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] == 0x64 /* d */) &&
                            ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                        dataLength = getInt(endianess);
                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        if (one && (z != middleSlice)) {
                            long ptr = raFile.getFilePointer();
                            raFile.seek(ptr + dataLength);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;
                            z++;
                        } else {
                            fileBuffer = new byte[dataLength];
                            raFile.read(fileBuffer);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;

                            for (int j = 0; j < dataLength;) {

                                if (fileBuffer[j] != 0) {

                                    for (k = 0; k < (fileBuffer[j] & 0x000000ff); k++) {
                                        imgBuffer[x + (imgExtents[0] * y)] = fileBuffer[j + 1];
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            x = 0;
                                            y--;
                                        } // if ((x == imgExtents[0]) && (y > 0))
                                        else if ((x == imgExtents[0]) && (y == 0)) {
                                            x = 0;
                                            y = imgExtents[1] - 1;
                                        } // else if ((x == imgExtents[0]) && (y == 0))
                                    } // for (k = 0; k < (fileBuffer[j] & 0x000000ff); k++)

                                    j = j + 2;
                                } // if (fileBuffer[j] != 0)
                                else if ((fileBuffer[j] == 0) && ((fileBuffer[j + 1] & 0x000000ff) > 2)) {

                                    for (k = 0; k < (fileBuffer[j + 1] & 0x000000ff); k++) {
                                        imgBuffer[x + (imgExtents[0] * y)] = fileBuffer[j + k + 2];
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            x = 0;
                                            y--;
                                        } // if ((x == imgExtents[0]) && (y > 0))
                                        else if ((x == imgExtents[0]) && (y == 0)) {
                                            x = 0;
                                            y = imgExtents[1] - 1;
                                        } // else if ((x == imgExtents[0]) && (y == 0))
                                    } // for (k = 0; k < (fileBuffer[j+1] & 0x000000ff); k++)

                                    j = j + 2 + (fileBuffer[j + 1] & 0x000000ff) +
                                        ((fileBuffer[j + 1] & 0x000000ff) % 2);
                                } // else if ((fileBuffer[j] == 0) && ((fileBuffer[j+1] & 0x000000ff) > 2))
                                else if ((fileBuffer[j] == 0) && (fileBuffer[j + 1] == 2)) {
                                    x = x + (fileBuffer[j + 2] & 0x000000ff);
                                    y = y - (fileBuffer[j + 3] & 0x000000ff);
                                    j = j + 4;
                                } // else if ((fileBuffer[j] == 0) && (fileBuffer[j+1] == 2))
                                else if ((fileBuffer[j] == 0) && (fileBuffer[j + 1] == 0)) {

                                    // end of a line
                                    if (x != 0) {
                                        x = 0;

                                        if (y > 0) {
                                            y = y - 1;
                                        } else {
                                            y = imgExtents[1] - 1;
                                        }
                                    } // if (x != 0)

                                    j = j + 2;
                                } // else if ((fileBuffer[j] == 0) && (fileBuffer[j+1] == 0))
                                else if ((fileBuffer[j] == 0) && (fileBuffer[j + 1] == 1)) {

                                    // end of RLE bitmap
                                    x = 0;
                                    y = imgExtents[1] - 1;

                                    if (one) {
                                        imageA.importData(0, imgBuffer, false);
                                    } else {
                                        imageA.importData(z * bufferSize, imgBuffer, false);
                                    }

                                    if (actualFrames > 1) {
                                        fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                    }

                                    z++;
                                    j = j + 2;
                                } // else if ((fileBuffer[j] == 0) && (fileBuffer[j+1] == 1))
                            } // for (j = 0; j < dataLength;)
                        }
                    } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                    else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += 4;
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkDataArea = LIST2subchunkSize - 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHUNKtype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < totalDataArea) && chunkRead)

            } // else if (compression == 1)
            
  

            raFile.close();

            if (wasCompressed) {
                FileDeleter fd = new FileDeleter(file.getPath());
                fd.start();
            }

            if (one) {
                imageA.setFileInfo(fileInfo, 0);
            } else {

                for (int i = 0; i < actualFrames; i++) {
                    imageA.setFileInfo(fileInfo, i);
                }
            }


            return imageA;

        } catch (OutOfMemoryError error) {

            if (imageA != null) {
                imageA.disposeLocal();
                imageA = null;
            }

            System.gc();

            throw error;
        }
    }
    
    
    
    
    /**
     * This method sets up for an AVI image write. This is only used for RGB images where only an imageA and no imageB
     * will be present. The image will be written as a series of 2D images and a series of frames without data used for
     * repeating existing frames. This method will use the first 2D image.
     *
     * @param      _imageA            First 2D image for AVI.
     * @param      microSecPerFrame   Frame rate.
     * @param      _totalDataFrames   Number of frames containing true data.
     * @param      _totalBlankFrames  Number of frames that are "blank" and should be filled with previous frame's data.
     *
     * @exception  IOException  if there is an error writing the file.
     */
    public void setAVIWrite(ModelImage _imageA, int microSecPerFrame, int _totalDataFrames, int _totalBlankFrames)
            throws IOException {

        // System.err.println("in setAVIWrite");
        byte[] signature;
        byte[] RIFFtype;
        byte[] CHUNKsignature;
        long saveLIST1Size; // location of length of CHUNK with first LIST - not including

        // first 8 bytes with LIST and size.  JUNK follows the end of
        // this CHUNK
        byte[] CHUNKtype;
        byte[] avihSignature;
        int[] extents;
        long saveLIST1subSize; // location of length of CHUNK with second LIST - not including

        // first 8 bytes with LIST and size.  Note that saveLIST1subSize =
        // saveLIST1Size + 76, and that the length size written to
        // saveLIST2Size is 76 less than that written to saveLIST1Size.
        // JUNK follows the end of this CHUNK.
        byte[] strhSignature;
        byte[] type;
        byte[] handler;
        byte[] strfSignature;
        long savestrfSize; // location of lenght of strf CHUNK - not including the first

        // 8 bytes with strf and size.  strn follows the end of this
        // CHUNK.
        int resXUnit = 0;
        int resYUnit = 0;
        float xResol = 0.0f; // in distance per pixel
        float yResol = 0.0f; // in distance per pixel
        long biXPelsPerMeter = 0L;
        long biYPelsPerMeter = 0L;
        byte[] strnSignature;
        byte[] text;
        long savestrnPos;
        byte[] JUNKsignature;
        long saveJUNKsignature;
        int paddingBytes;
        int i;
        byte[] dataSignature;
        int xMod;
        int bufferFactor;

        imageA = _imageA;
        totalDataFrames = _totalDataFrames;
        totalBlankFrames = _totalBlankFrames;

        dataFramesSent = 1;
        blankFramesSent = 0;
        compression = 0; // 24 bit RGB uncompressed
        bufferFactor = 4; // ARGB
        extents = imageA.getExtents();
        xDim = extents[0];
        yDim = extents[1];
        xPad = 0;
        xMod = xDim % 4;

        if (xMod != 0) {
            xPad = 4 - xMod;
            xDim = xDim + xPad;
        }

        try {
            file = new File(fileDir + fileName);

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (IOException ex) { }
            }

            raFile = new RandomAccessFile(file, "rw");

            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);
            signature = new byte[4];
            RIFFtype = new byte[4];
            CHUNKsignature = new byte[4];
            CHUNKtype = new byte[4];
            avihSignature = new byte[4];
            strhSignature = new byte[4];
            type = new byte[4];
            handler = new byte[4];
            strfSignature = new byte[4];
            strnSignature = new byte[4];
            text = new byte[16];
            JUNKsignature = new byte[4];
            CHUNKsignature = new byte[4];
            savedbLength = new long[totalDataFrames + totalBlankFrames];
            imageBufferA = new float[bufferFactor * xDim * yDim];
            dataSignature = new byte[4];
            bufferWrite = new byte[3 * xDim * yDim];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Error while trying to write AVI file.");

            return;
        }

        endianess = false; // for little-endianess; big-endianess did not work with either the

        // Windows Media Player or PowerPoint

        signature[0] = 82; // R
        signature[1] = 73; // I
        signature[2] = 70; // F
        signature[3] = 70; // F
        raFile.write(signature);
        saveFileSize = raFile.getFilePointer();

        // Bytes 4 thru 7 contain the length of the file.  This length does
        // not include bytes 0 thru 7.
        writeInt(0, endianess); // for now write 0 in the file size location
        RIFFtype[0] = 65; // A
        RIFFtype[1] = 86; // V
        RIFFtype[2] = 73; // I
        RIFFtype[3] = 32; // space
        raFile.write(RIFFtype);

        // Write the first LIST chunk, which contains information on data decoding
        CHUNKsignature[0] = 76; // L
        CHUNKsignature[1] = 73; // I
        CHUNKsignature[2] = 83; // S
        CHUNKsignature[3] = 84; // T
        raFile.write(CHUNKsignature);

        // Write the length of the LIST CHUNK not including the first 8 bytes with LIST and
        // size.  Note that the end of the LIST CHUNK is followed by JUNK.
        saveLIST1Size = raFile.getFilePointer();
        writeInt(0, endianess); // for now write 0 in avih sub-CHUNK size location

        // Write the chunk type
        CHUNKtype[0] = 104; // h
        CHUNKtype[1] = 100; // d
        CHUNKtype[2] = 114; // r
        CHUNKtype[3] = 108; // l
        raFile.write(CHUNKtype);

        // Write the avih sub-CHUNK
        avihSignature[0] = 97; // a
        avihSignature[1] = 118; // v
        avihSignature[2] = 105; // i
        avihSignature[3] = 104; // h
        raFile.write(avihSignature);

        writeInt(0x38, endianess); // Write the length of the avih sub-CHUNK (38H) not including the

        // the first 8 bytes for avihSignature and the length
        writeInt(microSecPerFrame, endianess); // dwMicroSecPerFrame - Write the microseconds per frame

        // default is 140,000.
        writeInt(500000, endianess); // dwMaxBytesPerSec

        // Write the maximum data rate of the file in bytes per second
        writeInt(0, endianess); // dwReserved1 - Reserved1 field set to zero
        writeInt(0x10, endianess); // dwFlags - just set the bit for AVIF_HASINDEX

        // 10H AVIF_HASINDEX: The AVI file has an idx1 chunk containing
        // an index at the end of the file.  For good performance, all
        // AVI files should contain an index.
        // 20H AVIF_MUSTUSEINDEX: Index CHUNK, rather than the physical
        // ordering of the chunks in the file, must be used to determine the
        // order of the frames.
        // 100H AVIF_ISINTERLEAVED: Indicates that the AVI file is interleaved.
        // This is used to read data from a CD-ROM more efficiently.
        // 800H AVIF_TRUSTCKTYPE: USE CKType to find key frames
        // 10000H AVIF_WASCAPTUREFILE: The AVI file is used for capturing
        // real-time video.  Applications should warn the user before
        // writing over a file with this fla set because the user
        // probably defragmented this file.
        // 20000H AVIF_COPYRIGHTED: The AVI file contains copyrighted data
        // and software.  When, this flag is used, software should not
        // permit the data to be duplicated.

        // writeInt(totalFrames);
        writeInt(totalDataFrames + totalBlankFrames, endianess); // dwTotalFrames - total frame number
        writeInt(0, endianess); // dwInitialFrames -Initial frame for interleaved files.

        // Noninterleaved files should specify 0.
        writeInt(1, endianess); // dwStreams - number of streams in the file - here 1 video and zero audio.
        writeInt(3 * xDim * yDim, endianess); // dwSuggestedBufferSize - Suggested buffer size for reading the file.

        // Generally, this size should be large enough to contain the largest
        // chunk in the file.
        writeInt(xDim, endianess); // dwWidth - image width in pixels
        writeInt(yDim, endianess); // dwHeight - image height in pixels

        // dwReserved[4] - Microsoft says to set the following 4 values to 0.
        writeInt(0, endianess);
        writeInt(0, endianess);
        writeInt(0, endianess);
        writeInt(0, endianess);

        // Write the Stream line header CHUNK
        raFile.write(CHUNKsignature); // Write LIST to the file

        // Write the size of the first LIST subCHUNK not including the first 8 bytes with
        // LIST and size.  Note that saveLIST1subSize = saveLIST1Size + 76, and that
        // the length written to saveLIST1subSize is 76 less than the length written to saveLIST1Size.
        // The end of the first LIST subCHUNK is followed by JUNK.
        saveLIST1subSize = raFile.getFilePointer();
        writeInt(0, endianess); // for now write 0 in CHUNK size location

        // Write the chunk type
        CHUNKtype[0] = 115; // s
        CHUNKtype[1] = 116; // t
        CHUNKtype[2] = 114; // r
        CHUNKtype[3] = 108; // l
        raFile.write(CHUNKtype);

        // Write the strh sub-CHUNK
        strhSignature[0] = 115; // s
        strhSignature[1] = 116; // t
        strhSignature[2] = 114; // r
        strhSignature[3] = 104; // h
        raFile.write(strhSignature);
        writeInt(56, endianess); // Write the length of the strh sub-CHUNK

        // fccType - Write the type of data stream - here vids for video stream
        type[0] = 118; // v
        type[1] = 105; // i
        type[2] = 100; // d
        type[3] = 115; // s
        raFile.write(type);

        // fccHandler - Write the handler for data compression/decompression
        // If compression == 0  for 24 bit per pixel uncompressed RGB:
        // Write DIB for Microsoft Device Independent Bitmap.  Note: Unfortunately,
        // at least 3 other four character codes are sometimes used for uncompressed
        // AVI videos: 'RGB ', 'RAW ', 0x00000000
        handler[0] = 68; // D
        handler[1] = 73; // I
        handler[2] = 66; // B
        handler[3] = 32; // space
        raFile.write(handler);
        writeInt(0, endianess); // dwFlags

        // 0x00000001 AVISF_DISABLED The stram data should be rendered only when
        // explicitly enabled.
        // 0x00010000 AVISF_VIDEO_PALCHANGES Indicates that a palette change is included
        // in the AVI file.  This flag warns the playback software that it
        // will need to animate the palette.
        writeInt(0, endianess); // dwPriority - priority of a stream type.  For example, in a file with

        // multiple audio streams, the one with the highest priority might be the
        // default one.
        writeInt(0, endianess); // dwInitialFrames - Specifies how far audio data is skewed ahead of video

        // frames in interleaved files.  Typically, this is about 0.75 seconds.  In
        // interleaved files specify the number of frames in the file prior
        // to the initial frame of the AVI sequence.
        // Noninterleaved files should use zero.
        // rate/scale = samples/second
        writeInt((microSecPerFrame / 10000), endianess); // dwScale
        writeInt(100, endianess); // dwRate - frame rate for video streams
        writeInt(0, endianess); // dwStart - this field is usually set to zero
        writeInt(totalDataFrames + totalBlankFrames, endianess);

        // dwLength - playing time of AVI file as defined by scale and rate
        // Set equal to the number of frames
        writeInt(3 * xDim * yDim, endianess); // dwSuggestedBufferSize - Suggested buffer size for reading the stream.

        // Typically, this contains a value corresponding to the largest chunk
        // in a stream.
        writeInt(0, endianess); // dwQuality - encoding quality given by an integer between

        // 0 and 10,000.  If set to -1, drivers use the default
        // quality value.
        writeInt(0, endianess); // dwSampleSize

        // 0 if the video frames may or may not vary in size
        // If 0, each sample of data(such as a video frame) must
        // be in a separate chunk.
        // If nonzero, then multiple samples of data can be grouped into
        // a single chunk within the file.
        // rcFrame - Specifies the destination rectangle for a text or video stream within the movie
        // rectangle specified by the dwWidth and dwHeight members of the AVI main header structure.
        // The rcFrame member is typically used in support of multiple video streams.  Set this
        // rectangle to the coordinates corresponding to the movie rectangle to update the whole
        // movie rectangle.  Units for this member are pixels.  The upper-left corner of the destination
        // rectangle is relative to the upper-left corner of the movie rectangle.
        writeShort((short) 0, endianess); // left
        writeShort((short) 0, endianess); // top
        writeShort((short) (xDim), endianess); // right
        writeShort((short) (yDim), endianess); // bottom

        // Write the stream format chunk
        strfSignature[0] = 115; // s
        strfSignature[1] = 116; // t
        strfSignature[2] = 114; // r
        strfSignature[3] = 102; // f
        raFile.write(strfSignature);

        // Write the size of the stream format CHUNK not including the first 8 bytes for
        // strf and the size.  Note that the end of the stream format CHUNK is followed by
        // strn.
        savestrfSize = raFile.getFilePointer();
        writeInt(0, endianess); // for now write 0 in the strf CHUNK size location
        writeInt(40, endianess); // biSize - Write header size of BITMAPINFO header structure

        // Applications should use this size to determine which BITMAPINFO header structure is
        // being used.  This size includes this biSize field.
        writeInt(xDim, endianess); // biWidth - image width in pixels
        writeInt(yDim, endianess); // biHeight - image height in pixels.  If height is positive,

        // the bitmap is a bottom up DIB and its origin is in the lower left corner.  If
        // height is negative, the bitmap is a top-down DIB and its origin is the upper
        // left corner.  This negative sign feature is supported by the Windows Media Player, but it is not
        // supported by PowerPoint.
        writeShort((short) 1, endianess); // biPlanes - number of color planes in which the data is stored

        // This must be set to 1.
        writeShort((short) 24, endianess); // biBitCount - number of bits per pixel

        writeInt(compression, endianess); // biCompression - type of compression used

        // 0L for BI_RGB, uncompressed data as bitmap
        // 1L for BI_RLE8, a run-length encoded(RLE) format for bitmaps
        // with 8 bits per pixel.  The compression format is a 2-byte
        // format consisting of a byte count followed by a byte containing
        // a color index.  In addition, the first byte of the pair can be
        // set to zero to indicate an escape character that denotes the end
        // of a line, the end of a bitmap, a delta, or the number of bytes
        // which follow, each of which contains the color index of a single
        // pixel, depending on the
        // value of the second byte of the pair, which can be one of the
        // following values:
        // value             meaning
        // 0                 End of line.
        // 1                 End of bitmap.
        // 2                 Delta.  The two bytes following the
        // escape contain unsigned values indicating
        // the horizontal and vertical offsets
        // of the next pixel from the current
        // position.
        // 3-255             number of bytes that folow, each of which
        // contains the color index of a single pixel
        // Must be padded if an odd value so that it
        // ends on a word boundary.
        // 2L for BI_RLE4, a RLE format for bits with 4 bits per pixel.
        // The compression format is a 2-byte format consisting of a count
        // byte followed by two word-length color indexes.
        // 3L for BI_BITFIELDS, specifies that the bitmap is not compressed
        // and that the color table consists of three DWORD color masks
        // that specify the red, green, and blue components, respectively,
        // of each pixel.  This is valid when used with 16- and 32-bit-
        // per-pixel bitmaps.

        writeInt(3 * xDim * yDim, endianess); // biSizeImage

        // Specifies the size in bytes of the image frame.  This can be set to zero for uncompressed
        // RGB bitmaps.
        resXUnit = imageA.getFileInfo(0).getUnitsOfMeasure(0);

        if ((resXUnit == Unit.INCHES.getLegacyNum()) || (resXUnit == Unit.MILS.getLegacyNum()) || 
                (resXUnit == Unit.CENTIMETERS.getLegacyNum()) ||
                (resXUnit == Unit.ANGSTROMS.getLegacyNum()) || (resXUnit == Unit.NANOMETERS.getLegacyNum()) ||
                (resXUnit == Unit.MICROMETERS.getLegacyNum()) || (resXUnit == Unit.MILLIMETERS.getLegacyNum()) ||
                (resXUnit == Unit.METERS.getLegacyNum()) || (resXUnit == Unit.KILOMETERS.getLegacyNum()) ||
                (resXUnit == Unit.MILES.getLegacyNum())) {
            xResol = imageA.getFileInfo(0).getResolutions()[0];

            if (resXUnit == Unit.INCHES.getLegacyNum()) {
                xResol = 0.0254f * xResol;
            } else if (resXUnit == Unit.MILS.getLegacyNum()) {
                xResol = 2.54e-5f * xResol;
            } else if (resXUnit == Unit.CENTIMETERS.getLegacyNum()) {
                xResol = 0.01f * xResol;
            } else if (resXUnit == Unit.ANGSTROMS.getLegacyNum()) {
                xResol = 1.0e-10f * xResol;
            } else if (resYUnit == Unit.NANOMETERS.getLegacyNum()) {
                yResol = 1.0e-9f * yResol;
            } else if (resXUnit == Unit.MICROMETERS.getLegacyNum()) {
                xResol = 1.0e-6f * xResol;
            } else if (resXUnit == Unit.MILLIMETERS.getLegacyNum()) {
                xResol = 1.0e-3f * xResol;
            } else if (resXUnit == Unit.KILOMETERS.getLegacyNum()) {
                xResol = 1.0e3f * xResol;
            } else if (resXUnit == Unit.MILES.getLegacyNum()) {
                xResol = 1.6093e3f * xResol;
            }

            if (xResol > 0.0f) {
                biXPelsPerMeter = (long) ((1 / xResol) + 0.5);
            }
        }

        writeInt((int) biXPelsPerMeter, endianess); // biXPelsPerMeter - horizontal resolution in pixels

        // per meter
        resYUnit = imageA.getFileInfo(0).getUnitsOfMeasure(1);

        if ((resYUnit == Unit.INCHES.getLegacyNum()) || (resYUnit == Unit.MILS.getLegacyNum()) ||
                (resYUnit == Unit.CENTIMETERS.getLegacyNum()) ||
                (resYUnit == Unit.ANGSTROMS.getLegacyNum()) || (resYUnit == Unit.NANOMETERS.getLegacyNum()) ||
                (resYUnit == Unit.MICROMETERS.getLegacyNum()) || (resYUnit == Unit.MILLIMETERS.getLegacyNum()) ||
                (resYUnit == Unit.METERS.getLegacyNum()) || (resYUnit == Unit.KILOMETERS.getLegacyNum()) ||
                (resYUnit == Unit.MILES.getLegacyNum())) {
            yResol = imageA.getFileInfo(0).getResolutions()[1];

            if (resYUnit == Unit.INCHES.getLegacyNum()) {
                yResol = 0.0254f * yResol;
            } else if (resYUnit == Unit.MILS.getLegacyNum()) {
                yResol = 2.54e-5f * yResol;
            } else if (resYUnit == Unit.CENTIMETERS.getLegacyNum()) {
                yResol = 0.01f * yResol;
            } else if (resYUnit == Unit.ANGSTROMS.getLegacyNum()) {
                yResol = 1.0e-10f * yResol;
            } else if (resYUnit == Unit.NANOMETERS.getLegacyNum()) {
                yResol = 1.0e-9f * yResol;
            } else if (resYUnit == Unit.MICROMETERS.getLegacyNum()) {
                yResol = 1.0e-6f * yResol;
            } else if (resYUnit == Unit.MILLIMETERS.getLegacyNum()) {
                yResol = 1.0e-3f * yResol;
            } else if (resYUnit == Unit.KILOMETERS.getLegacyNum()) {
                yResol = 1.0e3f * yResol;
            } else if (resYUnit == Unit.MILES.getLegacyNum()) {
                yResol = 1.6093e3f * yResol;
            }

            if (yResol > 0.0f) {
                biYPelsPerMeter = (long) ((1 / yResol) + 0.5);
            }
        }

        writeInt((int) biYPelsPerMeter, endianess); // biYPelsPerMeter - vertical resolution in pixels

        // per meter
        writeInt(0, endianess); // biClrUsed - here indicates no color table

        // Provides a way for getting smaller color tables.  When this
        // field is set to 0, the number of colors in the color table is based on
        // the biBitCount field (1 indicates 2 colors, 4 indicates 16 colors,
        // 8 indicates 256, and 24 indicates no color table).  A nonzero value
        // specifies the exact number of colors in the table.  So, for example,
        // if an 8-bit DIB uses only 17 colors, then only those 17 colors need
        // to be defined in the table, and biClrUsed is set to 17.  If nonzero
        // for a 24-bit DIB, it indicates the existence of a color table that the
        // application can use for color reference.
        writeInt(0, endianess); // biClrImportant - specifies that the first x colors of the color table

        // are important to the DIB.  If the rest of the colors are not available,
        // the image still retains its meaning in an acceptable manner.  When this
        // field is set to zero, all the colors are important, or, rather, their
        // relative importance has not been computed.

        // Use strn to provide a zero terminated text string describing the stream
        savestrnPos = raFile.getFilePointer();
        raFile.seek(savestrfSize);
        writeInt((int) (savestrnPos - (savestrfSize + 4)), endianess);
        raFile.seek(savestrnPos);
        strnSignature[0] = 115; // s
        strnSignature[1] = 116; // t
        strnSignature[2] = 114; // r
        strnSignature[3] = 110; // n
        raFile.write(strnSignature);
        writeInt(16, endianess); // Write the length of the strn sub-CHUNK
        text[0] = 70; // F
        text[1] = 105; // i
        text[2] = 108; // l
        text[3] = 101; // e
        text[4] = 65; // A
        text[5] = 118; // v
        text[6] = 105; // i
        text[7] = 32; // space
        text[8] = 119; // w
        text[9] = 114; // r
        text[10] = 105; // i
        text[11] = 116; // t
        text[12] = 101; // e
        text[13] = 32; // space
        text[14] = 32; // space
        text[15] = 0; // termination byte
        raFile.write(text);

        // write a JUNK CHUNK for padding
        saveJUNKsignature = raFile.getFilePointer();
        raFile.seek(saveLIST1Size);
        writeInt((int) (saveJUNKsignature - (saveLIST1Size + 4)), endianess);
        raFile.seek(saveLIST1subSize);
        writeInt((int) (saveJUNKsignature - (saveLIST1subSize + 4)), endianess);
        raFile.seek(saveJUNKsignature);
        JUNKsignature[0] = 74; // J
        JUNKsignature[1] = 85; // U
        JUNKsignature[2] = 78; // N
        JUNKsignature[3] = 75; // K
        raFile.write(JUNKsignature);
        paddingBytes = (int) (4084 - (saveJUNKsignature + 8));
        writeInt(paddingBytes, endianess);

        for (i = 0; i < (paddingBytes / 2); i++) {
            writeShort((short) 0, endianess);
        }

        // Write the second LIST chunk, which contains the actual data
        CHUNKsignature[0] = 76; // L
        CHUNKsignature[1] = 73; // I
        CHUNKsignature[2] = 83; // S
        CHUNKsignature[3] = 84; // T
        raFile.write(CHUNKsignature);

        // Write the length of the LIST CHUNK not including the first 8 bytes with LIST and
        // size.  The end of the second LIST CHUNK is followed by idx1.
        saveLIST2Size = raFile.getFilePointer();
        writeInt(0, endianess); // For now write 0
        savemovi = raFile.getFilePointer();

        // Write CHUNK type 'movi'
        CHUNKtype[0] = 109; // m
        CHUNKtype[1] = 111; // 0
        CHUNKtype[2] = 118; // v
        CHUNKtype[3] = 105; // i
        raFile.write(CHUNKtype);

        // Write the data record signature '00db' where db means that DIB bitmap data (uncompressed)
        // follows.  The characters 00 are used to identify the stream.
        dataSignature[0] = 48; // 0
        dataSignature[1] = 48; // 0
        dataSignature[2] = 100; // d
        dataSignature[3] = 98; // b

        // Write the data.  Each 3-byte triplet in the bitmap array represents the relative intensities
        // of blue, green, and red, respectively, for a pixel.  The color bytes are in reverse order
        // from the Windows convention.
        raFile.write(dataSignature);
        savedbLength[dataFramesSent + blankFramesSent - 1] = raFile.getFilePointer();

        // Write the data length
        writeInt(3 * xDim * yDim, endianess);
        writeAVITripletCFrames();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  quality  DOCUMENT ME!
     */
    public void setCompressionQuality(float quality) {
        this.compressionQuality = quality;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  isScript  DOCUMENT ME!
     */
    public void setIsScript(boolean isScript) {
        this.isScript = isScript;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  microSec  DOCUMENT ME!
     */
    public void setMicroSecPerFrame(int microSec) {
        this.microSecPerFrame = microSec;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pBar  DOCUMENT ME!
     */
    public void setProgressBar(ProgressBarInterface pBar) {
        this.progressBar = pBar;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  readQT  DOCUMENT ME!
     */
    public void setReadQT(boolean readQT) {
        this.readQT = readQT;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  writeQuickTime  DOCUMENT ME!
     */
    public void setWriteQT(boolean writeQuickTime) {
        this.writeQT = writeQuickTime;
        this.fileName = fileName.substring(0, fileName.length() - 3) + "avi";
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void writeBlankFrame() throws IOException {

        blankFramesSent++;
        dataSignature = new byte[4];
        dataSignature[0] = 48; // 0
        dataSignature[1] = 48; // 0
        dataSignature[2] = 100; // d
        dataSignature[3] = 99; // c
        raFile.write(dataSignature);
        savedbLength[dataFramesSent + blankFramesSent - 1] = raFile.getFilePointer();
        writeInt(0, endianess);

        if ((dataFramesSent == totalDataFrames) && (blankFramesSent == totalBlankFrames)) {
            writeidx1CHUNK();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   _imageA  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void writeDataFrame(ModelImage _imageA) throws IOException {
        imageA = _imageA;
        dataFramesSent++;

        if (imageBufferA == null) {
            imageBufferA = new float[4 * xDim * yDim];
        }

        // Write the data record signature '00db' where db means that DIB bitmap data (uncompressed)
        // follows.  The characters 00 are used to identify the stream.
        dataSignature = new byte[4];
        dataSignature[0] = 48; // 0
        dataSignature[1] = 48; // 0
        dataSignature[2] = 100; // d
        dataSignature[3] = 98; // b

        // Write the data.  Each 3-byte triplet in the bitmap array represents the relative intensities
        // of blue, green, and red, respectively, for a pixel.  The color bytes are in reverse order
        // from the Windows convention.
        if (bufferWrite == null) {
            bufferWrite = new byte[3 * xDim * yDim];
        }

        raFile.write(dataSignature);
        savedbLength[dataFramesSent + blankFramesSent - 1] = raFile.getFilePointer();

        // Write the data length
        writeInt(3 * xDim * yDim, endianess);
        writeAVITripletCFrames();

        if ((dataFramesSent == totalDataFrames) && (blankFramesSent == totalBlankFrames)) {
            writeidx1CHUNK();
        }
    }

    /**
     * This method writes an AVI image file.
     *
     * @param      _imageA      DOCUMENT ME!
     * @param      _imageB      DOCUMENT ME!
     * @param      _LUTa        DOCUMENT ME!
     * @param      LUTb         DOCUMENT ME!
     * @param      RGBTA        DOCUMENT ME!
     * @param      RGBTB        DOCUMENT ME!
     * @param      red          DOCUMENT ME!
     * @param      green        DOCUMENT ME!
     * @param      blue         DOCUMENT ME!
     * @param      OPACITY      DOCUMENT ME!
     * @param      alphaBlend   DOCUMENT ME!
     * @param      paintBitmap  DOCUMENT ME!
     * @param      compression  -1 = unchosen, 0 = 24 bit uncompressed RGB, 1 = 8 bit per pixel compressed RLE
     *
     * @return     DOCUMENT ME!
     *
     * @exception  IOException  if there is an error writing the file.
     */
    public boolean writeImage(ModelImage _imageA, ModelImage _imageB, ModelLUT _LUTa, ModelLUT LUTb, ModelRGB RGBTA,
                              ModelRGB RGBTB, int red, int green, int blue, float OPACITY, float alphaBlend,
                              BitSet paintBitmap, int compression) throws IOException {

        byte[] signature;
        byte[] RIFFtype;
        byte[] CHUNKsignature;
        long saveLIST1Size; // location of length of CHUNK with first LIST - not including

        // first 8 bytes with LIST and size.  JUNK follows the end of
        // this CHUNK
        byte[] CHUNKtype;
        byte[] avihSignature;
        int[] extents;
        long saveLIST1subSize; // location of length of CHUNK with second LIST - not including

        // first 8 bytes with LIST and size.  Note that saveLIST1subSize =
        // saveLIST1Size + 76, and that the length size written to
        // saveLIST2Size is 76 less than that written to saveLIST1Size.
        // JUNK follows the end of this CHUNK.
        byte[] strhSignature;
        byte[] type;
        byte[] handler;
        byte[] strfSignature;
        long savestrfSize; // location of lenght of strf CHUNK - not including the first

        // 8 bytes with strf and size.  strn follows the end of this
        // CHUNK.
        int resXUnit = 0;
        int resYUnit = 0;
        float xResol = 0.0f; // in distance per pixel
        float yResol = 0.0f; // in distance per pixel
        long biXPelsPerMeter = 0L;
        long biYPelsPerMeter = 0L;
        byte[] strnSignature;
        byte[] text;
        long savestrnPos;
        byte[] JUNKsignature;
        long saveJUNKsignature;
        int paddingBytes;
        int i;
        long[] savedcLength;
        long idx1Pos;
        long endPos;
        long saveidx1Length;
        int xMod;
        int bufferFactor;

        imageA = _imageA;
        imageB = _imageB;
        LUTa = _LUTa;

        int newMicroSecPerFrame = microSecPerFrame;
        int totalFrames = imageA.getExtents()[2];
        int realFrames = imageA.getExtents()[2];

        if (imageA.getExtents().length > 3) {
            realFrames *= imageA.getExtents()[3];
        }

        // System.err.println("Res [2] type is: " + imageA.getFileInfo()[0].getResolutions()[2]);
        if (imageA.getFileInfo()[0].getUnitsOfMeasure()[2] == Unit.NANOSEC.getLegacyNum()) {
            newMicroSecPerFrame = (int) (imageA.getFileInfo()[0].getResolutions()[2] / 1000);
        } else if (imageA.getFileInfo()[0].getUnitsOfMeasure()[2] == Unit.MICROSEC.getLegacyNum()) {
            newMicroSecPerFrame = (int) (imageA.getFileInfo()[0].getResolutions()[2]);
        } else if (imageA.getFileInfo()[0].getUnitsOfMeasure()[2] == Unit.MILLISEC.getLegacyNum()) {
            newMicroSecPerFrame = (int) (imageA.getFileInfo()[0].getResolutions()[2] * 1000);
        } else if (imageA.getFileInfo()[0].getUnitsOfMeasure()[2] == Unit.SECONDS.getLegacyNum()) {
            newMicroSecPerFrame = (int) (imageA.getFileInfo()[0].getResolutions()[2] * 1000000);
        } else if (imageA.getFileInfo()[0].getUnitsOfMeasure()[2] == Unit.MINUTES.getLegacyNum()) {
            newMicroSecPerFrame = (int) (imageA.getFileInfo()[0].getResolutions()[2] * 1000000 * 60);
        } else if (imageA.getFileInfo()[0].getUnitsOfMeasure()[2] == Unit.HOURS.getLegacyNum()) {
            newMicroSecPerFrame = (int) (imageA.getFileInfo()[0].getResolutions()[2] * 1000000 * 60 * 60);
        }
        // System.err.println("new micro sec per frame: " + newMicroSecPerFrame);

        if ((newMicroSecPerFrame > 1000000) || (newMicroSecPerFrame < 10000)) {
            newMicroSecPerFrame = microSecPerFrame;
        }

        if (imageA.getFileInfo()[0] instanceof FileInfoAvi) {
            totalFrames = ((FileInfoAvi) imageA.getFileInfo()[0]).getTotalFrames();

            int firstFrames = ((FileInfoAvi) imageA.getFileInfo()[0]).getNumFrames();

            // adjust the total frames if frames have been added/removed since first reading in the AVI
            if (firstFrames != realFrames) {
                totalFrames = (int) (((double) realFrames / (double) firstFrames) * totalFrames);
            }
        }

        if ((compression != -1) && (compression != 1)) {

            if (compression != 0) {
                newCompressionType = compression;
                compression = 0;
                useNewCompression = true;
            }

            if (compression == 2) {
                writeQT = true;
            }
        } else if (writeQT) {
            compression = 0;
            newCompressionType = AlgorithmTranscode.TRANSCODE_QT;
            useNewCompression = true;
        } else if ((imageA.isColorImage()) || (imageB != null)) {

            // if imageA is color or imageB is not null,
            // must have 24 bit uncompressed RGB
            compression = 0;

            JDialogAVIChoice choice = new JDialogAVIChoice(ViewUserInterface.getReference().getMainFrame(), true);

            if (!choice.okayPressed()) {
                return false;
            }

            int compressionType = choice.getCompression();

            if (compressionType > 1) {

                compression = 0;
                useNewCompression = true;
                newCompressionType = compressionType;

                if (compressionType == 2) {
                    writeQT = true;
                } else if (compressionType == AlgorithmTranscode.TRANSCODE_MJPG) {
                    setCompressionQuality(choice.getMJPEGQuality());
                }
            }

            compression = 0;
        } else {
            // Even if LUTa and LUTb are the same height, the same pixel can refer to one index
            // in one LUTa and another index in LUT

            if ((compression != 0) && (compression != 1)) {
                JDialogAVIChoice choice = new JDialogAVIChoice(ViewUserInterface.getReference().getMainFrame(), false);

                if (!choice.okayPressed()) {

                    // MipavUtil.displayWarning("AVI Save Cancelled");
                    return false;
                }

                int compressionType = choice.getCompression();

                if (compressionType > 1) {
                    compression = 0;
                    useNewCompression = true;
                    newCompressionType = compressionType;

                    if (compressionType == AlgorithmTranscode.TRANSCODE_MJPG) {
                        setCompressionQuality(choice.getMJPEGQuality());

                    }
                } else {
                    compression = compressionType;
                }
            }
        }

        FileWriteOptions options = new FileWriteOptions(true);
        options.setAVICompression(newCompressionType);
        options.setFileType(FileUtility.AVI);
        options.setBeginSlice(0);
        options.setEndSlice(imageA.getExtents()[2] - 1);
        ScriptRecorder.getReference().addLine(new ActionSaveImageAs(imageA, options));

        if (useNewCompression) {
            // if the compression will be M-JPEG, X and Y extents must be multiples of 8... otherwise 4

            int mod = (newCompressionType == AlgorithmTranscode.TRANSCODE_MJPG) ? 8 : 4;
            int leftPadding = 0;

            if ((imageA.getExtents()[0] % mod) != 0) {
                leftPadding = mod - (imageA.getExtents()[0] % mod);
            }

            int topPadding = 0;

            if ((imageA.getExtents()[1] % mod) != 0) {
                topPadding = mod - (imageA.getExtents()[1] % mod);
            }

            if ((leftPadding != 0) || (topPadding != 0)) {
                // this means we must resize the image to save it with this compression ask the user if he/she wants to
                // resize first

                String compStr = new String("");

                switch (newCompressionType) {

                    case AlgorithmTranscode.TRANSCODE_IV32:
                        compStr = "IR32";
                        break;

                    case AlgorithmTranscode.TRANSCODE_IV41:
                        compStr = "IR41";
                        break;

                    case AlgorithmTranscode.TRANSCODE_IV50:
                        compStr = "Indeo Video 5";
                        break;

                    case AlgorithmTranscode.TRANSCODE_MJPG:
                        compStr = "M-JPEG";
                        break;

                    case AlgorithmTranscode.TRANSCODE_MP42:
                        compStr = "MP42";
                        break;

                    case AlgorithmTranscode.TRANSCODE_MPG4:
                        compStr = "Microsoft Mpeg4";
                        break;

                    case AlgorithmTranscode.TRANSCODE_DIVX:
                        compStr = "DivX";
                        break;

                    case AlgorithmTranscode.TRANSCODE_DX50:
                        compStr = "DX50";
                        break;

                    default:
                        compStr = "this";
                        break;

                }
                // String compStr = (newCompressionType == AlgorithmTranscode.TRANSCODE_MP42) ?
                // new String("MP42") : new String("M-JPEG");

                int response = JOptionPane.NO_OPTION;

                if (!isScript) {
                    response = JOptionPane.showConfirmDialog(UI.getMainFrame(),
                                                             new String("AVI must be resized to save with " + compStr +
                                                                        " compression.  Resize?"), "Resize?",
                                                             JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
                } else {
                    response = JOptionPane.YES_OPTION;
                }

                if (response == JOptionPane.NO_OPTION) {
                    return false;
                } else {
                    int[] newExtents = new int[imageA.getExtents().length];
                    newExtents[0] = imageA.getExtents()[0] + leftPadding;
                    newExtents[1] = imageA.getExtents()[1] + topPadding;
                    newExtents[2] = imageA.getExtents()[2];

                    int[] marginX = new int[]{leftPadding,0};
                    int[] marginY = new int[]{topPadding,0};
                    int[] marginZ = new int[]{0,0};

                    // System.err.println("New extents: " + newExtents[0] + " by " + newExtents[1] + " by " +
                    // newExtents[2]);

                    ModelImage paddedImage = new ModelImage(imageA.getType(), newExtents, "TEMPImage");

                    for (int index = 0; index < newExtents.length; index++) {
                        paddedImage.getFileInfo()[0].setResolutions(imageA.getFileInfo()[0].getResolutions()[index],
                                                                    index);
                        paddedImage.getFileInfo()[0].setUnitsOfMeasure(imageA.getFileInfo()[0].getUnitsOfMeasure()[index],
                                                                       index);
                    }

                    AlgorithmAddMargins algoMarg = new AlgorithmAddMargins(imageA, paddedImage,
                                marginX, marginY, marginZ );
                    algoMarg.setPadValue( new float[]{0,0,0} );

                    algoMarg.setRunningInSeparateThread(false);
                    algoMarg.run();
                    algoMarg.finalize();
                    algoMarg = null;
                    paddedImage.calcMinMax();

                    boolean wasOkay = writeImage(paddedImage, null, _LUTa, null, RGBTA, RGBTB, red, green, blue,
                                                 OPACITY, alphaBlend, paintBitmap, newCompressionType);

                    paddedImage.disposeLocal();
                    paddedImage = null;

                    return wasOkay;
                }
            }
        }

        lutBufferRemapped = new int[1];

        if (useNewCompression) {
            file = new File(fileDir + "TEMPFILE.avi");
        } else {
            file = new File(fileDir + fileName);
        }

        if (raFile != null) {

            try {
                raFile.close();
            } catch (IOException ex) { }
        }

        raFile = new RandomAccessFile(file, "rw");
        endianess = false; // for little-endianess; big-endianess did not work with either the

        // Windows Media Player or PowerPoint
        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        raFile.setLength(0);
        signature = new byte[4];
        signature[0] = 82; // R
        signature[1] = 73; // I
        signature[2] = 70; // F
        signature[3] = 70; // F
        raFile.write(signature);
        saveFileSize = raFile.getFilePointer();

        // Bytes 4 thru 7 contain the length of the file.  This length does
        // not include bytes 0 thru 7.
        writeInt(0, endianess); // for now write 0 in the file size location
        RIFFtype = new byte[4];
        RIFFtype[0] = 65; // A
        RIFFtype[1] = 86; // V
        RIFFtype[2] = 73; // I
        RIFFtype[3] = 32; // space
        raFile.write(RIFFtype);

        // Write the first LIST chunk, which contains information on data decoding
        CHUNKsignature = new byte[4];
        CHUNKsignature[0] = 76; // L
        CHUNKsignature[1] = 73; // I
        CHUNKsignature[2] = 83; // S
        CHUNKsignature[3] = 84; // T
        raFile.write(CHUNKsignature);

        // Write the length of the LIST CHUNK not including the first 8 bytes with LIST and
        // size.  Note that the end of the LIST CHUNK is followed by JUNK.
        saveLIST1Size = raFile.getFilePointer();
        writeInt(0, endianess); // for now write 0 in avih sub-CHUNK size location

        // Write the chunk type
        CHUNKtype = new byte[4];
        CHUNKtype[0] = 104; // h
        CHUNKtype[1] = 100; // d
        CHUNKtype[2] = 114; // r
        CHUNKtype[3] = 108; // l
        raFile.write(CHUNKtype);

        // Write the avih sub-CHUNK
        avihSignature = new byte[4];
        avihSignature[0] = 97; // a
        avihSignature[1] = 118; // v
        avihSignature[2] = 105; // i
        avihSignature[3] = 104; // h
        raFile.write(avihSignature);

        writeInt(0x38, endianess); // Write the length of the avih sub-CHUNK (38H) not including the

        // the first 8 bytes for avihSignature and the length

        int difMicSecPerFrame = Math.round(newMicroSecPerFrame * ((float) totalFrames / realFrames));

        // System.err.println("dif mic sec per frame: " + difMicSecPerFrame);
        writeInt(difMicSecPerFrame, endianess); // dwMicroSecPerFrame - Write the microseconds per frame

        // default is 140,000.
        writeInt(500000, endianess); // dwMaxBytesPerSec

        // Write the maximum data rate of the file in bytes per second
        writeInt(0, endianess); // dwReserved1 - Reserved1 field set to zero
        writeInt(0x10, endianess); // dwFlags - just set the bit for AVIF_HASINDEX

        // 10H AVIF_HASINDEX: The AVI file has an idx1 chunk containing
        // an index at the end of the file.  For good performance, all
        // AVI files should contain an index.
        // 20H AVIF_MUSTUSEINDEX: Index CHUNK, rather than the physical
        // ordering of the chunks in the file, must be used to determine the
        // order of the frames.
        // 100H AVIF_ISINTERLEAVED: Indicates that the AVI file is interleaved.
        // This is used to read data from a CD-ROM more efficiently.
        // 800H AVIF_TRUSTCKTYPE: USE CKType to find key frames
        // 10000H AVIF_WASCAPTUREFILE: The AVI file is used for capturing
        // real-time video.  Applications should warn the user before
        // writing over a file with this flag set because the user
        // probably defragmented this file.
        // 20000H AVIF_COPYRIGHTED: The AVI file contains copyrighted data
        // and software.  When, this flag is used, software should not
        // permit the data to be duplicated.

        extents = imageA.getExtents();

        if (imageA.getNDims() >= 4) {
            tDim = extents[3];
        } else {
            tDim = 1;
        }

        if (imageA.getNDims() >= 3) {
            zDim = extents[2];
        } else {
            zDim = 1;
        }

        yDim = extents[1];
        xDim = extents[0];
        xPad = 0;
        xMod = xDim % 4;

        if (xMod != 0) {
            xPad = 4 - xMod;
            xDim = xDim + xPad;
        }

        // realFrames = zDim * tDim;
        writeInt(realFrames, endianess); // dwTotalFrames - total frame number
        writeInt(0, endianess); // dwInitialFrames -Initial frame for interleaved files.

        // Noninterleaved files should specify 0.
        writeInt(1, endianess); // dwStreams - number of streams in the file - here 1 video and zero audio.
        writeInt(0x100000, endianess); // dwSuggestedBufferSize - Suggested buffer size for reading the file.

        // Generally, this size should be large enough to contain the largest
        // chunk in the file.
        writeInt(xDim, endianess); // dwWidth - image width in pixels
        writeInt(yDim, endianess); // dwHeight - image height in pixels

        // dwReserved[4] - Microsoft says to set the following 4 values to 0.
        writeInt(0, endianess);
        writeInt(0, endianess);
        writeInt(0, endianess);
        writeInt(0, endianess);

        // Write the Stream line header CHUNK
        raFile.write(CHUNKsignature); // Write LIST to the file

        // Write the size of the first LIST subCHUNK not including the first 8 bytes with
        // LIST and size.  Note that saveLIST1Size = saveLIST1subSize + 76, and that
        // the length written to saveLIST1subSize is 76 less than the length written to saveLIST1Size.
        // The end of the first LIST subCHUNK is followed by JUNK.
        saveLIST1subSize = raFile.getFilePointer();
        writeInt(0, endianess); // for now write 0 in CHUNK size location

        // Write the chunk type
        CHUNKtype[0] = 115; // s
        CHUNKtype[1] = 116; // t
        CHUNKtype[2] = 114; // r
        CHUNKtype[3] = 108; // l
        raFile.write(CHUNKtype);

        // Write the strh sub-CHUNK
        strhSignature = new byte[4];
        strhSignature[0] = 115; // s
        strhSignature[1] = 116; // t
        strhSignature[2] = 114; // r
        strhSignature[3] = 104; // h
        raFile.write(strhSignature);
        writeInt(56, endianess); // Write the length of the strh sub-CHUNK

        // fccType - Write the type of data stream - here vids for video stream
        type = new byte[4];
        type[0] = 118; // v
        type[1] = 105; // i
        type[2] = 100; // d
        type[3] = 115; // s
        raFile.write(type);
        handler = new byte[4];

        // fccHandler - Write the handler for data compression/decompression
        // If compression == 0  for 24 bit per pixel uncompressed RGB:
        // Write DIB for Microsoft Device Independent Bitmap.  Note: Unfortunately,
        // at least 3 other four character codes are sometimes used for uncompressed
        // AVI videos: 'RGB ', 'RAW ', 0x00000000
        if (compression == 0) {
            handler[0] = 68; // D
            handler[1] = 73; // I
            handler[2] = 66; // B
            handler[3] = 32; // space
        }
        // else if compression == 1 for 8 bit per pixel compressed RLE
        else {
            handler[0] = 0x6D; // m
            handler[1] = 0x72; // r
            handler[2] = 0x6C; // l
            handler[3] = 0x65; // e
        }

        raFile.write(handler);
        writeInt(0, endianess); // dwFlags

        // 0x00000001 AVISF_DISABLED The stream data should be rendered only when
        // explicitly enabled.
        // 0x00010000 AVISF_VIDEO_PALCHANGES Indicates that a palette change is included
        // in the AVI file.  This flag warns the playback software that it
        // will need to animate the palette.
        writeInt(0, endianess); // dwPriority - priority of a stream type.  For example, in a file with

        // multiple audio streams, the one with the highest priority might be the
        // default one.
        writeInt(0, endianess); // dwInitialFrames - Specifies how far audio data is skewed ahead of video

        // frames in interleaved files.  Typically, this is about 0.75 seconds.  In
        // interleaved files specify the number of frames in the file prior
        // to the initial frame of the AVI sequence.
        // Noninterleaved files should use zero.
        // rate/scale = samples/second

        int rate = (int)
                       Math.round((realFrames) /
                                      ((double) totalFrames / (1.0 / ((double) newMicroSecPerFrame / 1000000.0))));
        // System.err.println("Microsec per frame: " + newMicroSecPerFrame + " dwScale: " +
        // 1 + " dwRate: " + rate);

        writeInt(1, endianess); // dwScale
        writeInt(rate, endianess); // dwRate - frame rate for video streams
        writeInt(0, endianess); // dwStart - this field is usually set to zero
        writeInt(realFrames, endianess); // dwLength - playing time of AVI file as defined by scale and rate

        // Set equal to the number of frames
        writeInt(0x100000, endianess); // dwSuggestedBufferSize - Suggested buffer size for reading the stream.

        // Typically, this contains a value corresponding to the largest chunk
        // in a stream.
        writeInt(10000, endianess); // dwQuality - encoding quality given by an integer between

        // 0 and 10,000.  If set to -1, drivers use the default
        // quality value.

        if (compression == 0) {
            writeInt(3 * xDim * yDim, endianess); // dwSampleSize

            // writeInt(1572864, endianess);
        } else if (compression == 1) {
            writeInt(xDim * yDim, endianess); // dwSwampleSize
        }

        // 0 if the video frames may or may not vary in size
        // If 0, each sample of data(such as a video frame) must
        // be in a separate chunk.
        // If nonzero, then multiple samples of data can be grouped into
        // a single chunk within the file.
        // rcFrame - Specifies the destination rectangle for a text or video stream within the movie
        // rectangle specified by the dwWidth and dwHeight members of the AVI main header structure.
        // The rcFrame member is typically used in support of multiple video streams.  Set this
        // rectangle to the coordinates corresponding to the movie rectangle to update the whole
        // movie rectangle.  Units for this member are pixels.  The upper-left corner of the destination
        // rectangle is relative to the upper-left corner of the movie rectangle.
        writeShort((short) 0, endianess); // left
        writeShort((short) 0, endianess); // top
        writeShort((short) (xDim), endianess); // right
        writeShort((short) (yDim), endianess); // bottom

        // Write the stream format chunk
        strfSignature = new byte[4];
        strfSignature[0] = 115; // s
        strfSignature[1] = 116; // t
        strfSignature[2] = 114; // r
        strfSignature[3] = 102; // f
        raFile.write(strfSignature);

        // Write the size of the stream format CHUNK not including the first 8 bytes for
        // strf and the size.  Note that the end of the stream format CHUNK is followed by
        // strn.
        savestrfSize = raFile.getFilePointer();
        writeInt(0, endianess); // for now write 0 in the strf CHUNK size location
        writeInt(40, endianess); // biSize - Write header size of BITMAPINFO header structure

        // Applications should use this size to determine which BITMAPINFO header structure is
        // being used.  This size includes this biSize field.
        writeInt(xDim, endianess); // biWidth - image width in pixels
        writeInt(yDim, endianess); // biHeight - image height in pixels.  If height is positive,

        // the bitmap is a bottom up DIB and its origin is in the lower left corner.  If
        // height is negative, the bitmap is a top-down DIB and its origin is the upper
        // left corner.  This negative sign feature is supported by the Windows Media Player, but it is not
        // supported by PowerPoint.
        writeShort((short) 1, endianess); // biPlanes - number of color planes in which the data is stored

        // This must be set to 1.
        if (compression == 0) {
            writeShort((short) 24, endianess); // biBitCount - number of bits per pixel
        } else if (compression == 1) {
            writeShort((short) 8, endianess); // biBitCount - number of bits per pixel
        }

        writeInt(compression, endianess); // biCompression - type of compression used

        // 0L for BI_RGB, uncompressed data as bitmap
        // 1L for BI_RLE8, a run-length encoded(RLE) format for bitmaps
        // with 8 bits per pixel.  The compression format is a 2-byte
        // format consisting of a byte count followed by a byte containing
        // a color index.  In addition, the first byte of the pair can be
        // set to zero to indicate an escape character that denotes the end
        // of a line, the end of a bitmap, a delta, or the number of bytes
        // which follow, each of which contains the color index of a single
        // pixel, depending on the
        // value of the second byte of the pair, which can be one of the
        // following values:
        // value             meaning
        // 0                 End of line.
        // 1                 End of bitmap.
        // 2                 Delta.  The two bytes following the
        // escape contain unsigned values indicating
        // the horizontal and vertical offsets
        // of the next pixel from the current
        // position.
        // 3-255             number of bytes that folow, each of which
        // contains the color index of a single pixel
        // Must be padded if an odd value so that it
        // ends on a word boundary.
        // 2L for BI_RLE4, a RLE format for bits with 4 bits per pixel.
        // The compression format is a 2-byte format consisting of a count
        // byte followed by two word-length color indexes.
        // 3L for BI_BITFIELDS, specifies that the bitmap is not compressed
        // and that the color table consists of three DWORD color masks
        // that specify the red, green, and blue components, respectively,
        // of each pixel.  This is valid when used with 16- and 32-bit-
        // per-pixel bitmaps.
        if (compression == 0) {
            writeInt(3 * xDim * yDim, endianess); // biSizeImage
        } else if (compression == 1) {
            writeInt(xDim * yDim, endianess); // biSizeImage
        }

        // Specifies the size in bytes of the image frame.  This can be set to zero for uncompressed
        // RGB bitmaps.
        resXUnit = imageA.getFileInfo(0).getUnitsOfMeasure(0);

        if ((resXUnit == Unit.INCHES.getLegacyNum()) || (resXUnit == Unit.MILS.getLegacyNum()) ||
                (resXUnit == Unit.CENTIMETERS.getLegacyNum()) ||
                (resXUnit == Unit.ANGSTROMS.getLegacyNum()) || (resXUnit == Unit.NANOMETERS.getLegacyNum()) ||
                (resXUnit == Unit.MICROMETERS.getLegacyNum()) || (resXUnit == Unit.MILLIMETERS.getLegacyNum()) ||
                (resXUnit == Unit.METERS.getLegacyNum()) || (resXUnit == Unit.KILOMETERS.getLegacyNum()) ||
                (resXUnit == Unit.MILES.getLegacyNum())) {
            xResol = imageA.getFileInfo(0).getResolutions()[0];

            if (resXUnit == Unit.INCHES.getLegacyNum()) {
                xResol = 0.0254f * xResol;
            } else if (resXUnit == Unit.MILS.getLegacyNum()) {
                xResol = 2.54e-5f * xResol;
            } else if (resXUnit == Unit.CENTIMETERS.getLegacyNum()) {
                xResol = 0.01f * xResol;
            } else if (resXUnit == Unit.ANGSTROMS.getLegacyNum()) {
                xResol = 1.0e-10f * xResol;
            } else if (resXUnit == Unit.NANOMETERS.getLegacyNum()) {
                xResol = 1.0e-9f * xResol;
            } else if (resXUnit == Unit.MICROMETERS.getLegacyNum()) {
                xResol = 1.0e-6f * xResol;
            } else if (resXUnit == Unit.MILLIMETERS.getLegacyNum()) {
                xResol = 1.0e-3f * xResol;
            } else if (resXUnit == Unit.KILOMETERS.getLegacyNum()) {
                xResol = 1.0e3f * xResol;
            } else if (resXUnit == Unit.MILES.getLegacyNum()) {
                xResol = 1.6093e3f * xResol;
            }

            if (xResol > 0.0f) {
                biXPelsPerMeter = (long) ((1 / xResol) + 0.5);
            }
        }

        writeInt((int) biXPelsPerMeter, endianess); // biXPelsPerMeter - horizontal resolution in pixels

        // per meter
        resYUnit = imageA.getFileInfo(0).getUnitsOfMeasure(1);

        if ((resYUnit == Unit.INCHES.getLegacyNum()) || (resYUnit == Unit.MILS.getLegacyNum()) ||
                (resYUnit == Unit.CENTIMETERS.getLegacyNum()) ||
                (resYUnit == Unit.ANGSTROMS.getLegacyNum()) || (resYUnit == Unit.NANOMETERS.getLegacyNum()) ||
                (resYUnit == Unit.MICROMETERS.getLegacyNum()) || (resYUnit == Unit.MILLIMETERS.getLegacyNum()) ||
                (resYUnit == Unit.METERS.getLegacyNum()) || (resYUnit == Unit.KILOMETERS.getLegacyNum()) ||
                (resYUnit == Unit.MILES.getLegacyNum())) {
            yResol = imageA.getFileInfo(0).getResolutions()[1];

            if (resYUnit == Unit.INCHES.getLegacyNum()) {
                yResol = 0.0254f * yResol;
            } else if (resYUnit == Unit.MILS.getLegacyNum()) {
                yResol = 2.54e-5f * yResol;
            } else if (resYUnit == Unit.CENTIMETERS.getLegacyNum()) {
                yResol = 0.01f * yResol;
            } else if (resYUnit == Unit.ANGSTROMS.getLegacyNum()) {
                yResol = 1.0e-10f * yResol;
            } else if (resYUnit == Unit.NANOMETERS.getLegacyNum()) {
                yResol = 1.0e-9f * yResol;
            } else if (resYUnit == Unit.MICROMETERS.getLegacyNum()) {
                yResol = 1.0e-6f * yResol;
            } else if (resYUnit == Unit.MILLIMETERS.getLegacyNum()) {
                yResol = 1.0e-3f * yResol;
            } else if (resYUnit == Unit.KILOMETERS.getLegacyNum()) {
                yResol = 1.0e3f * yResol;
            } else if (resYUnit == Unit.MILES.getLegacyNum()) {
                yResol = 1.6093e3f * yResol;
            }

            if (yResol > 0.0f) {
                biYPelsPerMeter = (long) ((1 / yResol) + 0.5);
            }
        }

        // System.err.println("X Resolution on write is: " + xResol + " Y Resolution: " + yResol);

        writeInt((int) biYPelsPerMeter, endianess); // biYPelsPerMeter - vertical resolution in pixels

        // per meter
        if (compression == 0) {
            writeInt(0, endianess); // biClrUsed - here indicates no color table
        } else if (compression == 1) {

            // biClrUsed - here a color table is used
            writeInt(LUTa.getExtents()[1], endianess);
        }

        // Provides a way for getting smaller color tables.  When this
        // field is set to 0, the number of colors in the color table is based on
        // the biBitCount field (1 indicates 2 colors, 4 indicates 16 colors,
        // 8 indicates 256, and 24 indicates no color table).  A nonzero value
        // specifies the exact number of colors in the table.  So, for example,
        // if an 8-bit DIB uses only 17 colors, then only those 17 colors need
        // to be defined in the table, and biClrUsed is set to 17.  If nonzero
        // for a 24-bit DIB, it indicates the existence of a color table that the
        // application can use for color reference.
        writeInt(0, endianess); // biClrImportant - specifies that the first x colors of the color table

        // are important to the DIB.  If the rest of the colors are not available,
        // the image still retains its meaning in an acceptable manner.  When this
        // field is set to zero, all the colors are important, or, rather, their
        // relative importance has not been computed.
        // Write the LUTa.getExtents()[1] color table entries here if RLE8 compression used.  They are written:
        // blue byte, green byte, red byte, 0 byte
        if (compression == 1) {
            raFile.write(createLUT());
        }

        // Use strn to provide a zero terminated text string describing the stream
        savestrnPos = raFile.getFilePointer();
        raFile.seek(savestrfSize);
        writeInt((int) (savestrnPos - (savestrfSize + 4)), endianess);
        raFile.seek(savestrnPos);
        strnSignature = new byte[4];
        strnSignature[0] = 115; // s
        strnSignature[1] = 116; // t
        strnSignature[2] = 114; // r
        strnSignature[3] = 110; // n
        raFile.write(strnSignature);
        writeInt(16, endianess); // Write the length of the strn sub-CHUNK
        text = new byte[16];
        text[0] = 70; // F
        text[1] = 105; // i
        text[2] = 108; // l
        text[3] = 101; // e
        text[4] = 65; // A
        text[5] = 118; // v
        text[6] = 105; // i
        text[7] = 32; // space
        text[8] = 119; // w
        text[9] = 114; // r
        text[10] = 105; // i
        text[11] = 116; // t
        text[12] = 101; // e
        text[13] = 32; // space
        text[14] = 32; // space
        text[15] = 0; // termination byte
        raFile.write(text);

        // write a JUNK CHUNK for padding
        saveJUNKsignature = raFile.getFilePointer();
        raFile.seek(saveLIST1Size);
        writeInt((int) (saveJUNKsignature - (saveLIST1Size + 4)), endianess);
        raFile.seek(saveLIST1subSize);
        writeInt((int) (saveJUNKsignature - (saveLIST1subSize + 4)), endianess);
        raFile.seek(saveJUNKsignature);
        JUNKsignature = new byte[4];
        JUNKsignature[0] = 74; // J
        JUNKsignature[1] = 85; // U
        JUNKsignature[2] = 78; // N
        JUNKsignature[3] = 75; // K
        raFile.write(JUNKsignature);
        paddingBytes = (int) (4084 - (saveJUNKsignature + 8));
        writeInt(paddingBytes, endianess);

        for (i = 0; i < (paddingBytes / 2); i++) {
            writeShort((short) 0, endianess);
        }

        // Write the second LIST chunk, which contains the actual data
        CHUNKsignature = new byte[4];
        CHUNKsignature[0] = 76; // L
        CHUNKsignature[1] = 73; // I
        CHUNKsignature[2] = 83; // S
        CHUNKsignature[3] = 84; // T
        raFile.write(CHUNKsignature);

        // Write the length of the LIST CHUNK not including the first 8 bytes with LIST and
        // size.  The end of the second LIST CHUNK is followed by idx1.
        saveLIST2Size = raFile.getFilePointer();
        writeInt(0, endianess); // For now write 0
        savemovi = raFile.getFilePointer();

        // Write CHUNK type 'movi'
        CHUNKtype[0] = 109; // m
        CHUNKtype[1] = 111; // 0
        CHUNKtype[2] = 118; // v
        CHUNKtype[3] = 105; // i
        raFile.write(CHUNKtype);
        savedbLength = new long[tDim * zDim];
        savedcLength = new long[tDim * zDim];
        dcLength = new int[tDim * zDim];

        if (compression == 0) {
            bufferFactor = 1;

            if (imageA.isColorImage()) {
                bufferFactor = 4;
            }

            imageBufferA = new float[bufferFactor * xDim * yDim];

            if (imageB != null) {
                imageBufferB = new float[bufferFactor * xDim * yDim];
            }

            // Write the data record signature '00db' where db means that DIB bitmap data (uncompressed)
            // follows.  The characters 00 are used to identify the stream.
            dataSignature = new byte[4];
            dataSignature[0] = 48; // 0
            dataSignature[1] = 48; // 0
            dataSignature[2] = 100; // d
            dataSignature[3] = 98; // b

            // Write the data.  Each 3-byte triplet in the bitmap array represents the relative intensities
            // of blue, green, and red, respectively, for a pixel.  The color bytes are in reverse order
            // from the Windows convention.
            bufferWrite = new byte[3 * xDim * yDim];

            if (imageA.isColorImage() == false) {

                for (int t = 0; t < tDim; t++) {

                    for (int z = 0; z < zDim; z++) {
                        raFile.write(dataSignature);
                        savedbLength[z + (t * zDim)] = raFile.getFilePointer();

                        // Write the data length
                        writeInt(3 * xDim * yDim, endianess);
                        writeAVITriplet(t, z, LUTb, red, green, blue, 1 - OPACITY, alphaBlend, paintBitmap);

                        if (((zDim * tDim) - 1) >= 1) {
                            fireProgressStateChanged(100 * (z + (t * zDim)) / ((zDim * tDim) - 1));
                        }
                    }
                }
            } else {

                for (int t = 0; t < tDim; t++) {

                    for (int z = 0; z < zDim; z++) {
                        raFile.write(dataSignature);
                        savedbLength[z + (t * zDim)] = raFile.getFilePointer();

                        // Write the data length
                        writeInt(3 * xDim * yDim, endianess);
                        writeAVITripletC(t, z, RGBTA, RGBTB, red, green, blue, 1 - OPACITY, alphaBlend, paintBitmap);

                        if (((zDim * tDim) - 1) >= 1) {
                            fireProgressStateChanged(100 * (z + (t * zDim)) / ((zDim * tDim) - 1));
                        }
                    }
                }
            }
        } // if (compression == 0)
        else { // compression == 1
            imageBufferA = new float[xDim * yDim];

            byte[] pixStore = new byte[xDim * yDim];
            byte[] lastStore = new byte[xDim * yDim];
            byte[] encodeStore = new byte[(2 * xDim * yDim) + xDim]; // largest possible size

            // 1 count for every index and an end of line or end of bitmap ends every row
            // Write the data record signature '00dc' where dc means that DIB bitmap data (compressed)
            // follows.  The characters 00 are used to identify the stream.
            dataSignature = new byte[4];
            dataSignature[0] = 48; // 0
            dataSignature[1] = 48; // 0
            dataSignature[2] = 100; // d
            dataSignature[3] = 99; // c

            for (int t = 0; t < tDim; t++) {

                for (int z = 0; z < zDim; z++) {
                    raFile.write(dataSignature);
                    savedcLength[z + (t * zDim)] = raFile.getFilePointer();

                    // Write the data length follwed by run length encoded data
                    writeRLE8(t, z, pixStore, lastStore, encodeStore);

                    if (((zDim * tDim) - 1) >= 1) {
                        fireProgressStateChanged(100 * (z + (t * zDim)) / ((zDim * tDim) - 1));
                    }
                }
            }
        } // else compression == 1

        // Write the idx1 CHUNK
        // Write the 'idx1' signature
        idx1Pos = raFile.getFilePointer();
        raFile.seek(saveLIST2Size);
        writeInt((int) (idx1Pos - (saveLIST2Size + 4)), endianess);
        raFile.seek(idx1Pos);

        byte[] idx1Signature = new byte[4];
        idx1Signature[0] = 105; // i
        idx1Signature[1] = 100; // d
        idx1Signature[2] = 120; // x
        idx1Signature[3] = 49; // 1
        raFile.write(idx1Signature);

        // Write the length of the idx1 CHUNK not including the idx1 signature and the 4 length
        // bytes. Write 0 for now.
        saveidx1Length = raFile.getFilePointer();
        writeInt(0, endianess);

        for (int t = 0; t < tDim; t++) {

            for (int z = 0; z < zDim; z++) {

                // In the ckid field write the 4 character code to identify the chunk 00db or 00dc
                raFile.write(dataSignature);

                if ((z == 0) && (t == 0)) {
                    writeInt(0x10, endianess); // Write the flags - select AVIIF_KEYFRAME
                } else {
                    writeInt(0x00, endianess);
                }

                // AVIIF_KEYFRAME 0x00000010L
                // The flag indicates key frames in the video sequence.
                // Key frames do not need previous video information to be decompressed.
                // AVIIF_NOTIME 0x00000100L The CHUNK does not influence video timing(for
                // example a palette change CHUNK).
                // AVIIF_LIST 0x00000001L Marks a LIST CHUNK.
                // AVIIF_TWOCC 2L
                // AVIIF_COMPUSE 0x0FFF0000L These bits are for compressor use.
                if (compression == 0) {
                    writeInt((int) (savedbLength[z + (t * zDim)] - 4 - savemovi), endianess);

                    // Write the offset (relative to the 'movi' field) to the relevant CHUNK
                    writeInt(3 * xDim * yDim, endianess); // Write the length of the relevant

                    // CHUNK.  Note that this length is
                    // also written at savedbLength
                } else if (compression == 1) {
                    writeInt((int) (savedcLength[z + (t * zDim)] - 4 - savemovi), endianess);

                    // Write the offset (relative to the 'movi' field) to the relevant CHUNK
                    writeInt(dcLength[z + (t * zDim)], endianess); // Write the length of the relevant CHUNK.  Note

                    // that this length is also written at savedcLength
                }
            } // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)

        endPos = raFile.getFilePointer();
        raFile.seek(saveFileSize);
        writeInt((int) (endPos - (saveFileSize + 4)), endianess);
        raFile.seek(saveidx1Length);
        writeInt((int) (endPos - (saveidx1Length + 4)), endianess);

        // raFile.close();


        // do some cleanup...
        imageBufferA = null;
        imageBufferB = null;
        _imageA = null;
        imageA = null;
        imageB = null;

        // if new compression is to be used, we aren't done yet...
        if (!useNewCompression) {
            raFile.close();
            raFile = null;
        } else {
            Preferences.debug("About to transcode to: " + fileDir + fileName + "\n", Preferences.DEBUG_FILEIO);

            AlgorithmTranscode at = new AlgorithmTranscode(file.toURI().toURL(), fileDir + fileName,
                                                           newCompressionType);

            at.setRunningInSeparateThread(false);
            at.setQuality(compressionQuality);
            at.run();
            at.finalize();
            at = null;

            raFile.close();
            raFile = null;

            FileDeleter fd = new FileDeleter(file.getPath());
            fd.start();
            file = null;
        }

        return true;
    }

    /**
     * Creates a LUT to write.
     *
     * @return  DOCUMENT ME!
     */
    private byte[] createLUT() {
        int i, value, lutHeightA;
        float rangeA, imageMaxA;

        if (imageA == null) {
            MipavUtil.displayError("imageA is null");

            return null;
        }

        if (LUTa == null) {
            MipavUtil.displayError("LUTa is null");

            return null;
        }

        lutHeightA = LUTa.getExtents()[1];

        byte[] lutWrite = new byte[4 * lutHeightA];

        if (lutHeightA != lutBufferRemapped.length) {

            try {
                lutBufferRemapped = new int[lutHeightA];
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: FileAvi.createLUT");

                return null;
            }
        }

        lutBufferRemapped = LUTa.exportIndexedLUT();

        // imageMinA    = (float)imageA.getMin();
        // imageMaxA    = (float)imageA.getMax();

        if (imageA.getType() == ModelStorageBase.UBYTE) {
            imageMinA = 0;
            imageMaxA = 255;
        } else if (imageA.getType() == ModelStorageBase.BYTE) {
            imageMinA = -128;
            imageMaxA = 127;
        } else {
            imageMinA = (float) imageA.getMin();
            imageMaxA = (float) imageA.getMax();
        }

        rangeA = imageMaxA - imageMinA;

        if (rangeA == 0) {
            rangeA = 1;
        }

        if ((lutHeightA - 1) == 0) {
            remapConstA = 1;
        } else if (rangeA < 255) {
            remapConstA = 1;
        } else {
            remapConstA = (lutHeightA - 1) / rangeA;
        }

        for (i = 0; i < lutHeightA; i++) {
            value = lutBufferRemapped[i];

            lutWrite[4 * i] = (byte) (value & 0x000000ff); // blue
            lutWrite[(4 * i) + 1] = (byte) ((value & 0x0000ff00) >> 8); // green
            lutWrite[(4 * i) + 2] = (byte) ((value & 0x00ff0000) >> 16); // red

            // System.out.println("Hey i = " + i + " value == " + (lutWrite[4 * i + 2] & 0xff) + ", " +
            // (lutWrite[4 * i + 1] & 0xff) + ", " +
            // (lutWrite[4 * i + 0] & 0xff));
            lutWrite[(4 * i) + 3] = (byte) 0;
        }

        return lutWrite;
    }

    /**
     * Reads the image header.
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private int readHeader() throws IOException {
        long LIST1Marker, LISTsubchunkMarker, marker;
        int loop;

        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");

            endianess = FileBase.LITTLE_ENDIAN; // false
            fileInfo = new FileInfoAvi(fileName, fileDir, FileUtility.AVI);
            fileInfo.setEndianess(endianess);

            int signature = getInt(endianess);

            if (signature == 0x46464952) {
                // have read RIFF
            } else {
                raFile.close();
                throw new IOException("AVI read header error first 4 bytes = " + signature);
            }

            getInt(endianess); // the file size excluding the first 8 bytes

            int RIFFtype = getInt(endianess);

            if (RIFFtype == 0x20495641) {
                // have read AVI<sp>
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 8-11 = " + RIFFtype);
            }

            int CHUNKsignature = getInt(endianess);

            if (CHUNKsignature == 0x5453494C) {
                // have read LIST for first LIST CHUNK with information on data decoding
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 12-15 = " + CHUNKsignature);
            }

            int LIST1Size = getInt(endianess); // size of first LIST CHUNK excluding first 8 bytes
            Preferences.debug("LIST1Size = " + LIST1Size + "\n", Preferences.DEBUG_FILEIO);

            // with CHUNKsignature and LIST1Size
            LIST1Marker = raFile.getFilePointer();

            int CHUNKtype = getInt(endianess);

            if (CHUNKtype == 0x6C726468) {
                // have read hdrl
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 16-19 = " + CHUNKtype);
            }

            int avihSignature = getInt(endianess); // signature of avih sub-CHUNK

            if (avihSignature == 0x68697661) {
                // have read avih
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 2-23 = " + avihSignature);
            }

            int avihLength = getInt(endianess); // read the size of the avih sub-CHUNK not

            // including the first 8 bytes for the signature and length
            if (avihLength == 56) {
                // avih sub-CHUNK has expected length
            } else {
                raFile.close();
                throw new IOException("AVI read header error avih sub-CHUNK length = " + avihLength);
            }

            microSecPerFrame = getInt(endianess);

            // System.err.println("Microsec per frame: " + microSecPerFrame);
            Preferences.debug("microSecPerFrame = " + microSecPerFrame + "\n", Preferences.DEBUG_FILEIO);

            int maxBytesPerSecond = getInt(endianess);
            Preferences.debug("maxBytesPerSecond = " + maxBytesPerSecond + "\n", Preferences.DEBUG_FILEIO);

            // System.err.println("Unknown int: " + getInt(endianess));
            getInt(endianess);

            int flags = getInt(endianess);

            if ((flags & 0x10) != 0) {
                AVIF_HASINDEX = true;
            } else {
                AVIF_HASINDEX = false;
            }

            if ((flags & 0x20) != 0) {
                AVIF_MUSTUSEINDEX = true;
            } else {
                AVIF_MUSTUSEINDEX = false;
            }

            if ((flags & 0x100) != 0) {
                AVIF_ISINTERLEAVED = true;
                Preferences.debug("AVIF_ISINTERLEAVED = true\n", Preferences.DEBUG_FILEIO);
            } else {
                AVIF_ISINTERLEAVED = false;
                Preferences.debug("AVIF_ISINTERLEAVED = false\n", Preferences.DEBUG_FILEIO);
            }

            if (AVIF_HASINDEX) {
                Preferences.debug("AVIF_HASINDEX = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_HASINDEX = false\n", Preferences.DEBUG_FILEIO);
            }

            if (AVIF_MUSTUSEINDEX) {
                Preferences.debug("AVIF_MUSTUSEINDEX = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_MUSTUSEINDEX = false\n", Preferences.DEBUG_FILEIO);
            }

            if ((flags & 0x800) != 0) {
                Preferences.debug("AVIF_TRUSTCKTYPE = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_TRUSTCKTYPE = false\n", Preferences.DEBUG_FILEIO);
            }

            if ((flags & 0x10000) != 0) {
                Preferences.debug("AVIF_WASCAPTUREFILE = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_WASCAPTUREFILE = false\n", Preferences.DEBUG_FILEIO);
            }

            if ((flags & 0x20000) != 0) {
                Preferences.debug("AVIF_COPYRIGHTED = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_COPYRIGHTED = false\n", Preferences.DEBUG_FILEIO);
            }

            int totalFrames = getInt(endianess);
            fileInfo.setTotalFrames(totalFrames);

            // System.err.println("total frames: " + totalFrames);
            Preferences.debug("totalFrames = " + totalFrames + "\n", Preferences.DEBUG_FILEIO);

            // However, many AVI frames will have no data and will just be used to repeat the
            // previous frames.  So we will need to read thru the data to get the actual number
            // of frames used in MIPAV before the image is created.  Then a second read thru will
            // take place to import the data into the image.
            int initialFrames = getInt(endianess);
            Preferences.debug("initialFrames = " + initialFrames + "\n", Preferences.DEBUG_FILEIO);
            streams = getInt(endianess);
            Preferences.debug("Number of streams: " + streams + "\n", Preferences.DEBUG_FILEIO);

            int suggestedBufferSize = getInt(endianess);
            Preferences.debug("suggestedBufferSize = " + suggestedBufferSize + "\n", Preferences.DEBUG_FILEIO);
            width = getInt(endianess); // xDim
            Preferences.debug("width = " + width + "\n", Preferences.DEBUG_FILEIO);
            height = getInt(endianess); // yDim
            Preferences.debug("height = " + height + "\n", Preferences.DEBUG_FILEIO);

            // read 4 reserved integers
            for (int i = 0; i < 4; i++) {
                getInt(endianess);
            }

            for (loop = 0; loop < streams; loop++) {

                // read the LIST subCHUNK
                CHUNKsignature = getInt(endianess);
                
                while ((CHUNKsignature == 0x6E727473 /* strn */) || (CHUNKsignature == 0x64727473 /* strd */) ||
                       (CHUNKsignature == 0x4B4E554A /* JUNK */)) {
                    // read strn instead of CHUNK
                    int strnLength = getInt(endianess);
                    if ((strnLength % 2) == 1) {
                        strnLength++;
                    }
                    byte[] text = new byte[strnLength];
                    raFile.read(text);

                    CHUNKsignature = getInt(endianess);
                } // ((CHUNKsignature == 0x6E727473 /* strn */) || (CHUNKsignature == 0x64727473 /* strd */) ||

                if (CHUNKsignature == 0x5453494C) {
                    // have read LIST for LIST subCHUNK with information on data decoding
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error signature first LIST subCHUNK = " + CHUNKsignature);
                }

                int LISTsubchunkSize = getInt(endianess); // size of the first LIST subCHUNK not including
                LISTsubchunkMarker = raFile.getFilePointer();

                // the first 8 signature and length bytes
                CHUNKtype = getInt(endianess);

                if (CHUNKtype == 0x6C727473) {
                    // have read strl
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error no strl in first LIST subCHUNK but = " + CHUNKtype);
                }

                // read the strh subCHUNK
                int strhSignature = getInt(endianess);

                if (strhSignature == 0x68727473) {
                    // have read strh
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error no strhSignature found but = " + strhSignature);
                }

                int strhLength = getInt(endianess); // length of strh subCHUNK not including first 8

                // signature and length bytes
                // AVI standard documentation mentioned a minimum length of 56,
                // but the Windows Media player was observed to play 5 mjpeg
                // files with strhLength = 48.
                if (strhLength < 48) {
                    raFile.close();
                    throw new IOException("AVI read header error with strhLength = " + strhLength);
                }

                int fccType = getInt(endianess);

                if (fccType == 0x73646976) {
                    // vids read for video stream
                } else if (streams > 1) {
                    raFile.seek(LISTsubchunkSize + LISTsubchunkMarker);

                    continue;
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error with fccType = " + fccType);
                }

                int handler = getInt(endianess);

                byte[] handlerByte = new byte[5];
                handlerByte[0] = (byte) (handler & 0xFF);
                handlerByte[1] = (byte) ((handler >> 8) & 0xFF);
                handlerByte[2] = (byte) ((handler >> 16) & 0xFF);
                handlerByte[3] = (byte) ((handler >> 24) & 0xFF);
                handlerByte[4] = (byte) 0;

                String handlerString = new String(handlerByte);

                System.err.println("Handler String is: " + handlerString);

                if ((handler == 0x20424944 /* DIB<sp> */) || (handler == 0x20424752 /* RGB<sp> */) ||
                        (handler == 0x20574152 /* RAW<sp> */) || (handler == 0x00000000) ||
                        (handlerString.startsWith("00dc"))) {
                    // uncompressed data
                    Preferences.debug("Uncompressed data\n", Preferences.DEBUG_FILEIO);
                } else if (handlerString.toUpperCase().startsWith("MRLE") ||
                               handlerString.toUpperCase().startsWith("RLE")) {
                    Preferences.debug("Microsoft run length encoding\n", Preferences.DEBUG_FILEIO);
                    /* mrle microsoft run length encoding */
                } else if (handlerString.toUpperCase().startsWith("MP42")) {
                    return AlgorithmTranscode.TRANSCODE_MP42;
                } else if (handlerString.toUpperCase().startsWith("DIV")) {
                    return AlgorithmTranscode.TRANSCODE_DIVX;
                } else if (handlerString.toUpperCase().startsWith("MPG4")) {
                    return AlgorithmTranscode.TRANSCODE_MPG4;
                } else if (handlerString.toUpperCase().startsWith("IV32")) {
                    return AlgorithmTranscode.TRANSCODE_IV32;
                } else if (handlerString.toUpperCase().startsWith("IV41")) {
                    return AlgorithmTranscode.TRANSCODE_IV41;
                } else if (handlerString.toUpperCase().startsWith("IV50")) {
                    return AlgorithmTranscode.TRANSCODE_IV50;
                } else if (handlerString.toUpperCase().startsWith("GEOV")) {
                    return AlgorithmTranscode.TRANSCODE_GEOV;
                } else {
                    raFile.close();
                    throw new IOException("Unrecognized compression handler is " + handlerString);
                    // tscc is the TechSmith Screen Capture Codec put out by the Techsmith Corporation for use with
                    // their Camtasia Screen "Camcorder" application.  Camtasia is a Microsoft windows application only.
                    //  The company has no plans to develop a version for the apple. Could the program be designed to
                    // use the tscc.exe codec provided by Techsmith?
                }

                flags = getInt(endianess);

                if ((flags & 0x00000001) != 0) {
                    raFile.close();
                    throw new IOException("Cannot presently handle AVISF_DISABLED");
                }

                if ((flags & 0x00010000) != 0) {
                    raFile.close();
                    throw new IOException("Cannot presently handle AVISF_VIDEO_PALCHANGES");
                }

                int priority = getInt(endianess);
                Preferences.debug("priority = " + priority + "\n", Preferences.DEBUG_FILEIO);
                initialFrames = getInt(endianess);

                if (initialFrames != 0) {
                    raFile.close();
                    throw new IOException("initialFrames should be 0 for noninterleaved files");
                }

                scale = getInt(endianess);
                Preferences.debug("scale = " + scale + "\n", Preferences.DEBUG_FILEIO);

                // System.err.println("Scale is: " + scale);
                rate = getInt(endianess);

                // System.err.println("Rate is: " + rate);
                Preferences.debug("rate = " + rate + "\n", Preferences.DEBUG_FILEIO);

                int start = getInt(endianess);
                Preferences.debug("start = " + start + "\n", Preferences.DEBUG_FILEIO);

                int length = getInt(endianess);
                Preferences.debug("length = " + length + "\n", Preferences.DEBUG_FILEIO);

                // System.err.println("DWLength: " + length);
                suggestedBufferSize = getInt(endianess);
                Preferences.debug("suggestedBufferSize = " + suggestedBufferSize + "\n", Preferences.DEBUG_FILEIO);

                int quality = getInt(endianess);

                if ((quality > 10000) || (quality < -1)) {
                    raFile.close();
                    throw new IOException("quality = " + quality);
                }

                Preferences.debug("quality = " + quality + "\n", Preferences.DEBUG_FILEIO);

                int sampleSize = getInt(endianess);
                Preferences.debug("sampleSize = " + sampleSize + "\n", Preferences.DEBUG_FILEIO);

                // read destination rectangle within movie rectangle
                short left = (short) getSignedShort(endianess);
                Preferences.debug("left = " + left + "\n", Preferences.DEBUG_FILEIO);

                short top = (short) getSignedShort(endianess);
                Preferences.debug("top = " + top + "\n", Preferences.DEBUG_FILEIO);

                short right = (short) getSignedShort(endianess);
                Preferences.debug("right = " + right + "\n", Preferences.DEBUG_FILEIO);

                short bottom = (short) getSignedShort(endianess);
                Preferences.debug("bottom = " + bottom + "\n", Preferences.DEBUG_FILEIO);

                if (strhLength > 56) {
                    byte[] extra = new byte[strhLength - 56];
                    raFile.read(extra);
                }
                
                long strfPos = raFile.getFilePointer();
                if ((left == 29811) && (top == 26226)) {
                    // The left, top, right, and bottom  fields have been omitted
                    raFile.seek(strfPos - 8);
                }

                // read the stream format CHUNK
                int strfSignature = getInt(endianess);

                if (strfSignature == 0x66727473) {
                    // read strf
                } else {
                    raFile.close();
                    throw new IOException("strf signature incorrectly read as = " + strfSignature);
                }

                int strfSize = getInt(endianess);
                Preferences.debug("strfSize = " + strfSize + "\n", Preferences.DEBUG_FILEIO);
                int BITMAPINFOsize = getInt(endianess);
                Preferences.debug("BITMAPINFOsize = " + BITMAPINFOsize + "\n", Preferences.DEBUG_FILEIO);

                if (BITMAPINFOsize > strfSize) {
                    BITMAPINFOsize = strfSize;
                }

                if (BITMAPINFOsize < 40) {
                    raFile.close();
                    throw new IOException("Cannot handle BITMAPINFO size = " + BITMAPINFOsize);
                }

                width = getInt(endianess);
                Preferences.debug("width = " + width + "\n", Preferences.DEBUG_FILEIO);
                height = getInt(endianess);
                Preferences.debug("height = " + height + "\n", Preferences.DEBUG_FILEIO);

                short planes = (short) getSignedShort(endianess);

                if (planes != 1) {
                    raFile.close();
                    throw new IOException("planes has an incorrect value = " + planes);
                }

                bitCount = (short) getSignedShort(endianess);
                Preferences.debug("bitCount = " + bitCount + "\n", Preferences.DEBUG_FILEIO);

                compression = getInt(endianess);

                if (compression == 0) {
                    Preferences.debug("Compression is BI_RGB\n", Preferences.DEBUG_FILEIO);
                    // BI_RGB uncompressed
                } else if (compression == 1) {
                    Preferences.debug("Compression is BI_RLE8\n", Preferences.DEBUG_FILEIO);
                    // BI_RLE8
                } else if (compression == 2) {

                    // BI_RLE4
                    raFile.close();
                    throw new IOException("Cannot currently handle 4 bit run length encoding");
                } else if (compression == 3) {
                    // BI_BITFIELDS
                    // To allow for arbitrarily packed RGB samples, BI_BITFIELDS specifies a
                    // mask field for each of the red, green, and blue pixel components.
                    // These masks indicate the bit positions occupied by each color
                    // component in a pixel.  In general, the masks are passed to a driver
                    // or video API using means other than a basic BITMAPINFOHEADER(such
                    // as using the appropriate fields in a DirectDraw DDPIXELFORMAT
                    // structure) but it might be valid to append the masks to the end of
                    // the BITMAPINFOHEADER in much the same way that a palette is appended
                    // for palettised formats.
                    //
                    // For example, 16 bit RGB 5:6:5 can be described using BI_BITFIELDS
                    // and the following bitmasks:

                    // Red  0xF800 (5 bits of red)
                    // Green 0x07E0 (6 bits of green)
                    // Blue  0x001F (5 bits of blue)

                    // In this case, if used with a BITMAPINFOHEADER, the bitmasks are
                    // u_int16s (16 bit) since the biBitFields field is set to 16.  For
                    // a 32bpp version, the bitmasks are each u_int32s.
                    raFile.close();
                    throw new IOException("Cannot currently handle BI_BITFIELDS compresion");
                
                } else {
                    raFile.close();
                    throw new IOException("Unknown compression with value = " + compression);
                }

                Preferences.debug("compression = " + compression + "\n", Preferences.DEBUG_FILEIO);

                if (((compression == 0) &&
                         ((bitCount == 4) || (bitCount == 8) || (bitCount == 16) || (bitCount == 24) || (bitCount == 32))) ||
                        ((compression == 1) && (bitCount == 8))) {
                    // OK
                } else {
                    raFile.close();
                    throw new IOException("Cannot currently handle bit count = " + bitCount);
                }

                int imageSize = getInt(endianess);
                Preferences.debug("imageSize = " + imageSize + "\n", Preferences.DEBUG_FILEIO);

                float[] imgResols = new float[5];
                imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = 1.0f;

                int xPixelsPerMeter = getInt(endianess);
                Preferences.debug("xPixelsPerMeter = " + xPixelsPerMeter + "\n", Preferences.DEBUG_FILEIO);

                // System.err.println("xPixelsPerMeter = " + xPixelsPerMeter);
                if (xPixelsPerMeter > 0) {
                    fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                    imgResols[0] = (1.0f / xPixelsPerMeter) * 1000.0f;
                }

                int yPixelsPerMeter = getInt(endianess);
                Preferences.debug("yPixelsPerMeter = " + yPixelsPerMeter + "\n", Preferences.DEBUG_FILEIO);

                // System.err.println("yPixelsPerMeter = " + yPixelsPerMeter);
                if (yPixelsPerMeter > 0) {
                    fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
                    imgResols[1] = (1.0f / yPixelsPerMeter) * 1000.0f;
                }

                imgResols[2] = microSecPerFrame;

                // System.err.println("Microseconds per frame (on read): " + microSecPerFrame);
                fileInfo.setUnitsOfMeasure(Unit.MICROSEC.getLegacyNum(), 2);
                fileInfo.setResolutions(imgResols);

                int colorsUsed = getInt(endianess);
                Preferences.debug("colorsUsed = " + colorsUsed + "\n", Preferences.DEBUG_FILEIO);

                if ((compression == 0) && ((bitCount == 24) || (bitCount == 32)) && (colorsUsed != 0)) {
                    raFile.close();
                    throw new IOException("For 24 and 32 bit uncompressed data software does not currently support colorsUsed = " +
                                          colorsUsed);
                }

                if ((bitCount == 8) && (colorsUsed == 0)) {
                    colorsUsed = 8;
                }

                int colorsImportant = getInt(endianess);
                Preferences.debug("colorsImportant = " + colorsImportant + "\n", Preferences.DEBUG_FILEIO);

                if (BITMAPINFOsize > 40) {
                    byte[] extra = new byte[BITMAPINFOsize - 40];
                    raFile.read(extra);
                }

                if (bitCount == 4) {

                    // read the color table into a LUT
                    int[] dimExtentsLUT = new int[2];
                    dimExtentsLUT[0] = 4;
                    dimExtentsLUT[1] = 256;

                    // FileIO obtains via getModelLUT.
                    // Then, ViewOpenFileUI obtains from FileIO via getModelLUT.
                    LUTa = new ModelLUT(ModelLUT.GRAY, colorsUsed, dimExtentsLUT);
                    lutBuffer = new byte[4 * colorsUsed];
                    raFile.read(lutBuffer);

                    for (int i = 0; i < colorsUsed; i++) {
                        LUTa.set(0, i, 1.0f); // alpha
                        LUTa.set(1, i, (lutBuffer[(4 * i) + 2] & 0x000000ff)); // red
                        LUTa.set(2, i, (lutBuffer[(4 * i) + 1] & 0x000000ff)); // green
                        LUTa.set(3, i, (lutBuffer[4 * i] & 0x000000ff)); // blue
                    } // for (i = 0; i < colorsUsed; i++)

                    for (int i = colorsUsed; i < 256; i++) {
                        LUTa.set(0, i, 1.0f);
                        LUTa.set(1, i, 0);
                        LUTa.set(2, i, 0);
                        LUTa.set(3, i, 0);
                    } // for (i = colorsUsed; i < 256; i++)

                    LUTa.makeIndexedLUT(null);
                } // if (bitCount == 4)
                else if (bitCount == 8) {

                    // read the color table into a LUT
                    int[] dimExtentsLUT = new int[2];
                    dimExtentsLUT[0] = 4;
                    dimExtentsLUT[1] = 256;

                    // FileIO obtains via getModelLUT.
                    // Then, ViewOpenFileUI obtains from FileIO via getModelLUT.
                    LUTa = new ModelLUT(ModelLUT.GRAY, colorsUsed, dimExtentsLUT);
                    lutBuffer = new byte[4 * colorsUsed];
                    raFile.read(lutBuffer);

                    for (int i = 0; i < colorsUsed; i++) {
                        LUTa.set(0, i, 1.0f); // alpha
                        LUTa.set(1, i, (lutBuffer[(4 * i) + 2] & 0x000000ff)); // red
                        LUTa.set(2, i, (lutBuffer[(4 * i) + 1] & 0x000000ff)); // green
                        LUTa.set(3, i, (lutBuffer[4 * i] & 0x000000ff)); // blue
                    } // for (i = 0; i < colorsUsed; i++)

                    for (int i = colorsUsed; i < 256; i++) {
                        LUTa.set(0, i, 1.0f);
                        LUTa.set(1, i, 0);
                        LUTa.set(2, i, 0);
                        LUTa.set(3, i, 0);
                    } // for (i = colorsUsed; i < 256; i++)

                    LUTa.makeIndexedLUT(null);
                } // else if (bitCount == 8)

                // Calculate the number of strf CHUNK bytes after the end of BITMAPINFO
                int strfEndBytes = strfSize - BITMAPINFOsize;

                if ((bitCount == 4) ||(bitCount == 8)) {
                    strfEndBytes = strfEndBytes - (4 * colorsUsed);
                }

                for (int i = 0; i < strfEndBytes; i++) {
                    raFile.readUnsignedByte();
                }
            } // for (loop = 0; loop < streams; loop++)

            marker = raFile.getFilePointer();

            if (marker < (LIST1Marker + LIST1Size)) {

                // read strn subCHUNK
                int strnSignature = getInt(endianess);

                if (strnSignature == 0x6E727473) {
                    int strnLength = getInt(endianess);
                    if ((strnLength % 2) == 1) {
                        strnLength++;
                    }
                    byte[] text = new byte[strnLength];
                    raFile.read(text);

                    if (text[strnLength - 1] != 0) {
                        raFile.close();
                        throw new IOException("strn string ends with illegal temination = " + text[strnLength - 1]);
                    }
                } // if (strnSignature == 0x6E727473)
                else if (strnSignature == 0x4B4E554A) {

                    // have read JUNK for a JUNK padding CHUNK
                    int JUNKlength = getInt(endianess);
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + JUNKlength);
                    CHUNKsignature = getInt(endianess);

                    if (CHUNKsignature != 0x5453494C) {
                        raFile.close();
                        throw new IOException("After JUNK CHUNK unexpected signature = " + CHUNKsignature);
                    }
                } // else if (strnSignature == 0x4B4E554A)
                else if (strnSignature == 0x54465349) {

                    // have read ISFT
                    int ISFTlength = getInt(endianess);
                    if ((ISFTlength % 2) == 1) {
                        ISFTlength++;
                    }
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + ISFTlength);
                } // else if (strnSignature == 0x54465349)
                else if (strnSignature == 0x54494449) {

                    // have read IDIT
                    int IDITlength = getInt(endianess);
                    if ((IDITlength % 2) == 1) {
                        IDITlength++;
                    }
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + IDITlength);
                } // else if (strnSignature == 0x54494449)
                else if (strnSignature == 0x74646576) {

                    // have read vedt
                    int vedtLength = getInt(endianess);
                    if ((vedtLength %2) == 1) {
                        vedtLength++;
                    }
                    byte[] vedt = new byte[vedtLength];
                    raFile.read(vedt);
                } else if (strnSignature == 0x5453494C) {
                    // have read LIST
                    int listLength = getInt(endianess);
                    if ((listLength % 2) == 1) {
                        listLength++;
                    }
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + listLength);
                } else if (strnSignature == 0x78646E69) {
                    // have read icdx
                    int icdxLength = getInt(endianess);
                    if ((icdxLength % 2) == 1) {
                        icdxLength++;
                    }
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + icdxLength);
                } else {
                    raFile.close();
                    throw new IOException("strn signature is an erroneous = " + strnSignature);
                }
            }

            raFile.seek(LIST1Marker + LIST1Size);
            do {
                signature = getInt(endianess);
                if (signature == 0x4B4E554A) {
                    // have read JUNK for a JUNK padding CHUNK
                    int JUNKlength = getInt(endianess);
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + JUNKlength);    
                }
                else if (signature != 0x5453494C /* LIST */) {
                    raFile.close();
                    throw new IOException("After first LIST CHUNK unexpected signature = " + signature);
                }
                else {
                    // At this point have read LIST for the second LIST CHUNK which contains the actual data.
                    LIST2Size = getInt(endianess);
                    moviPosition = raFile.getFilePointer(); 
                    CHUNKtype = getInt(endianess);
                    if (CHUNKtype == 0x4F464E49 /* INFO */) {
                        raFile.seek(moviPosition + LIST2Size);        
                    }
                    else {
                        idx1Position = moviPosition + LIST2Size;
                        indexPointer = idx1Position + 8; 
                        if ((idx1Position + 4) < raFile.length()) {
                            raFile.seek(idx1Position + 4);
                            indexSize = getInt(endianess);
                        }
                        raFile.seek(moviPosition + 4);
                    }
                }
            } while ((signature == 0x4B4E554A /* JUNK */) || 
                     ((signature == 0x5453494C /* LIST */) && (CHUNKtype == 0x4F464E49 /* INFO */)));

            if (CHUNKtype != 0x69766F6D) {

                // have not read movi
                raFile.close();
                throw new IOException("CHUNK type in second LIST CHUNK is an illegal = " + CHUNKtype);
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
        Preferences.debug("Finished readHeader\n", Preferences.DEBUG_FILEIO);
        return 0;
    }

    /**
     * Writes the AVI BGR triplet.
     *
     * @param   timeSlice     t (time) slice to show
     * @param   slice         z slice to show
     * @param   LUTb          DOCUMENT ME!
     * @param   red           DOCUMENT ME!
     * @param   green         DOCUMENT ME!
     * @param   blue          DOCUMENT ME!
     * @param   opacityPrime  DOCUMENT ME!
     * @param   alphaBlend    DOCUMENT ME!
     * @param   paintBitmap   DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeAVITriplet(int timeSlice, int slice, ModelLUT LUTb, int red, int green, int blue,
                                 float opacityPrime, float alphaBlend, BitSet paintBitmap) throws IOException {

        float alphaPrime = 1 - alphaBlend;
        float rangeA = 0;
        float rangeB = 0;
        float remapConstB = 1;
        float imageMaxA;
        float imageMinB;
        float imageMaxB;
        int bufferSize, bufferAdr;
        int lutHeightA = 0;
        int lutHeightB = 0;
        float[][] RGB_LUTa = null;
        float[][] RGB_LUTb = null;
        float Ra, Ga, Ba, Rb, Gb, Bb;
        int indexA, indexB;

        if (imageA == null) {
            MipavUtil.displayError("imageA is null");

            return;
        }

        if (LUTa == null) {
            MipavUtil.displayError("LUTa is null");

            return;
        }

        lutHeightA = LUTa.getExtents()[1];

        if (LUTb != null) {
            lutHeightB = LUTb.getExtents()[1];
        }

        if (lutHeightA != lutBufferRemapped.length) {

            try {
                lutBufferRemapped = new int[lutHeightA];
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: FileAvi.writeAVITriplet");

                return;
            }
        }

        if (imageB == null) {
            lutBufferRemapped = LUTa.exportIndexedLUT();
        }

        bufferSize = (xDim - xPad) * yDim;
        bufferAdr = 0;

        if (imageA.getType() == ModelStorageBase.UBYTE) {
            imageMinA = 0;
            imageMaxA = 255;
        } else if (imageA.getType() == ModelStorageBase.BYTE) {
            imageMinA = -128;
            imageMaxA = 127;
        } else {
            imageMinA = (float) imageA.getMin();
            imageMaxA = (float) imageA.getMax();
        }

        rangeA = imageMaxA - imageMinA;

        if (rangeA == 0) {
            rangeA = 1;
        }

        if ((lutHeightA - 1) == 0) {
            remapConstA = 1;
        } else if (rangeA < 255) {
            remapConstA = 1;
        } else {
            remapConstA = (lutHeightA - 1) / rangeA;
        }

        if (imageB != null) {

            // imageMinB    = (float)imageB.getMin();
            // imageMaxB    = (float)imageB.getMax();
            if (imageA.getType() == ModelStorageBase.UBYTE) {
                imageMinB = 0;
                imageMaxB = 255;
            } else if (imageA.getType() == ModelStorageBase.BYTE) {
                imageMinB = -128;
                imageMaxB = 127;
            } else {
                imageMinB = (float) imageB.getMin();
                imageMaxB = (float) imageB.getMax();
            }

            rangeB = imageMaxB - imageMinB;

            if (rangeB == 0) {
                rangeB = 1;
            }

            if ((lutHeightB - 1) == 0) {
                remapConstB = 1;
            } else {
                remapConstB = (lutHeightB - 1) / rangeB;
            }

            RGB_LUTa = LUTa.exportRGB_LUT(true);
            RGB_LUTb = LUTb.exportRGB_LUT(true);
        }

        int zDimSlices = 0;

        if (imageA.getNDims() >= 3) {
            zDimSlices = zDim;
        }

        try {

            // if (imageA.getType() == ModelStorageBase.DCOMPLEX)
            // imageA.ExportDComplexSliceXY(timeSlice*zDimSlices + slice,imageBufferA, logMagDisplay);
            if (imageA.getType() == ModelStorageBase.COMPLEX) {
                imageA.exportComplexSliceXY((timeSlice * zDimSlices) + slice, imageBufferA, false);
            } else {
                imageA.exportSliceXY((timeSlice * zDimSlices) + slice, imageBufferA);
            }

            if (imageB != null) {

                // if (imageB.getType() == ModelStorageBase.DCOMPLEX)
                // imageB.exportDComplexSliceXY(timeSlice*zDimSlices + slice,imageBufferB, logMagDisplay);
                if (imageB.getType() == ModelStorageBase.COMPLEX) {
                    imageB.exportComplexSliceXY((timeSlice * zDimSlices) + slice, imageBufferB, false);
                } else {
                    imageB.exportSliceXY((timeSlice * zDimSlices) + slice, imageBufferB);
                }
            }
        } catch (IOException error) {
            MipavUtil.displayError("" + error); // Need to fix this

            return;
        }

        int index, pix;

        if (imageB == null) {
            int offset = slice * bufferSize;
            int value;

            for (int y = yDim - 1; y >= 0; y--) {

                for (int x = 0; x < (xDim - xPad); x++) {
                    index = x + (y * (xDim - xPad));
                    pix = (short) (((imageBufferA[index] - imageMinA) * remapConstA) + 0.5);

                    if (paintBitmap.get(offset + index) == true) {
                        value = lutBufferRemapped[pix];
                        Ra = (value & 0x00ff0000) >> 16;
                        Ga = (value & 0x0000ff00) >> 8;
                        Ba = (value & 0x000000ff);
                        bufferWrite[bufferAdr] = (byte) ((Ba * opacityPrime) + blue);
                        bufferWrite[bufferAdr + 1] = (byte) ((Ga * opacityPrime) + green);
                        bufferWrite[bufferAdr + 2] = (byte) ((Ra * opacityPrime) + red);
                        bufferAdr = bufferAdr + 3;
                    } else {
                        bufferWrite[bufferAdr] = (byte) (lutBufferRemapped[pix] & 0x000000ff);
                        bufferWrite[bufferAdr + 1] = (byte) ((lutBufferRemapped[pix] & 0x0000ff00) >> 8);
                        bufferWrite[bufferAdr + 2] = (byte) ((lutBufferRemapped[pix] & 0x00ff0000) >> 16);
                        bufferAdr = bufferAdr + 3;
                    }
                } // for (x = 0; x < xDim - xPad; x++)

                for (int x = 0; x < xPad; x++) {
                    bufferWrite[bufferAdr] = (byte) 0;
                    bufferWrite[bufferAdr + 1] = (byte) 0;
                    bufferWrite[bufferAdr + 2] = (byte) 0;
                    bufferAdr = bufferAdr + 3;
                } // for (x = 0; x < xPad; x++)
            } // for (y = yDim - 1; y >= 0; y--)
        } // if (imageB == null )
        else {
            int offset = slice * bufferSize;

            for (int y = yDim - 1; y >= 0; y--) {

                for (int x = 0; x < (xDim - xPad); x++) {
                    index = x + (y * (xDim - xPad));
                    indexA = (short) (((imageBufferA[index] - imageA.getMin()) * remapConstA) + 0.5);
                    indexB = (short) (((imageBufferB[index] - imageB.getMin()) * remapConstB) + 0.5);
                    Ra = RGB_LUTa[0][indexA];
                    Rb = RGB_LUTb[0][indexB];
                    Ga = RGB_LUTa[1][indexA];
                    Gb = RGB_LUTb[1][indexB];
                    Ba = RGB_LUTa[2][indexA];
                    Bb = RGB_LUTb[2][indexB];

                    Ra = (int) ((Ra * alphaBlend) + (Rb * alphaPrime));
                    Ga = (int) ((Ga * alphaBlend) + (Gb * alphaPrime));
                    Ba = (int) ((Ba * alphaBlend) + (Bb * alphaPrime));

                    if (paintBitmap.get(offset + index) == true) {
                        bufferWrite[bufferAdr] = (byte) ((Ba * opacityPrime) + blue);
                        bufferWrite[bufferAdr + 1] = (byte) ((Ga * opacityPrime) + green);
                        bufferWrite[bufferAdr + 2] = (byte) ((Ra * opacityPrime) + red);
                        bufferAdr = bufferAdr + 3;
                    } else {
                        bufferWrite[bufferAdr] = (byte) Ba;
                        bufferWrite[bufferAdr + 1] = (byte) Ga;
                        bufferWrite[bufferAdr + 2] = (byte) Ra;
                        bufferAdr = bufferAdr + 3;
                    }
                } // for (x = 0; x < xDim-xPad; x++)

                for (int x = 0; x < xPad; x++) {
                    bufferWrite[bufferAdr] = (byte) 0;
                    bufferWrite[bufferAdr + 1] = (byte) 0;
                    bufferWrite[bufferAdr + 2] = (byte) 0;
                    bufferAdr = bufferAdr + 3;
                } // for (x = 0; x < xPad; x++)
            } // for (y = yDim - 1; y >= 0; y--)
        }

        raFile.write(bufferWrite);
    }

    /**
     * Write BGR AVI triplet for color image.
     *
     * @param   timeSlice     t (time) slice to show
     * @param   slice         z slice to show
     * @param   RGBTA         DOCUMENT ME!
     * @param   RGBTB         DOCUMENT ME!
     * @param   red           DOCUMENT ME!
     * @param   green         DOCUMENT ME!
     * @param   blue          DOCUMENT ME!
     * @param   opacityPrime  DOCUMENT ME!
     * @param   alphaBlend    DOCUMENT ME!
     * @param   paintBitmap   DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeAVITripletC(int timeSlice, int slice, ModelRGB RGBTA, ModelRGB RGBTB, int red, int green,
                                  int blue, float opacityPrime, float alphaBlend, BitSet paintBitmap)
            throws IOException {
        // Note that alphaBlending is applied with 1 component taken as zero if both components
        // are not present - for example, if either imageA or imageB but not both has red, then
        // the red component is alphaBlended with zero.

        int j;
        int bufferSize;
        int offset;
        int index;
        float Ra, Ga, Ba, Rb, Gb, Bb;
        int imageSize;
        float redMapped, greenMapped, blueMapped;
        int[] RGBIndexBufferA;
        int[] RGBIndexBufferB;
        int x, y;
        int bufferAdr;
        float alphaPrime = 1 - alphaBlend;

        bufferSize = (xDim - xPad) * yDim * 4;
        imageSize = (xDim - xPad) * yDim;
        bufferAdr = 0;

        try {
            RGBIndexBufferA = new int[256];
            RGBIndexBufferB = new int[256];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Error while writing AVI file.");

            return;
        }

        if (RGBTA != null) {
            RGBIndexBufferA = RGBTA.exportIndexedRGB();
        }

        if ((imageB != null) && (RGBTB != null)) {
            RGBIndexBufferB = RGBTB.exportIndexedRGB();
        }

        int zDimSlices = 0;

        if (imageA.getNDims() >= 3) {
            zDimSlices = zDim;
        }

        try {
            imageA.exportData(((timeSlice * zDimSlices * bufferSize) + (slice * bufferSize)), bufferSize, imageBufferA);

            if (imageB != null) {
                imageB.exportData(((timeSlice * zDimSlices * bufferSize) + (slice * bufferSize)), bufferSize,
                                  imageBufferB);
            }
        } catch (IOException error) {
            MipavUtil.displayError("" + error);

            return;
        }

        if (imageB == null) {
            offset = slice * imageSize;

            for (y = yDim - 1; y >= 0; y--) {

                for (x = 0; x < (xDim - xPad); x++) {
                    j = x + (y * (xDim - xPad));
                    index = 4 * j;

                    if (RGBTA != null) {

                        if (RGBTA.getROn()) {
                            redMapped = (RGBIndexBufferA[(int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            redMapped = 0;
                        }

                        if (RGBTA.getGOn()) {
                            greenMapped = (RGBIndexBufferA[(int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            greenMapped = 0;
                        }

                        if (RGBTA.getBOn()) {
                            blueMapped = (RGBIndexBufferA[(int) imageBufferA[index + 3]] & 0x000000ff);
                        } else {
                            blueMapped = 0;
                        }
                    } // end of if (RGBTA != null)
                    else {
                        redMapped = imageBufferA[index + 1];
                        greenMapped = imageBufferA[index + 2];
                        blueMapped = imageBufferA[index + 3];
                    }

                    if (paintBitmap.get(offset + j) == true) {
                        bufferWrite[bufferAdr] = (byte) ((blueMapped * opacityPrime) + blue);
                        bufferWrite[bufferAdr + 1] = (byte) ((greenMapped * opacityPrime) + green);
                        bufferWrite[bufferAdr + 2] = (byte) ((redMapped * opacityPrime) + red);
                        bufferAdr = bufferAdr + 3;
                    } else {
                        bufferWrite[bufferAdr] = (byte) blueMapped;
                        bufferWrite[bufferAdr + 1] = (byte) greenMapped;
                        bufferWrite[bufferAdr + 2] = (byte) redMapped;
                        bufferAdr = bufferAdr + 3;
                    }
                } // for (x = 0; x < xDim-xPad; x++)

                for (x = 0; x < xPad; x++) {
                    bufferWrite[bufferAdr] = (byte) 0;
                    bufferWrite[bufferAdr + 1] = (byte) 0;
                    bufferWrite[bufferAdr + 2] = (byte) 0;
                    bufferAdr = bufferAdr + 3;
                } // for (x = 0; x < xPad; x++)
            } // for (y = yDim - 1; y >= 0; y--)
        } // if (imageB == null )
        else {
            offset = slice * bufferSize;

            for (y = yDim - 1; y >= 0; y--) {

                for (x = 0; x < (xDim - xPad); x++) {
                    j = x + (y * (xDim - xPad));
                    index = 4 * j;

                    if ((RGBTA != null) && (RGBTB != null)) {

                        if (RGBTA.getROn()) {
                            Ra = (RGBIndexBufferA[(int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            Ra = 0;
                        }

                        if (RGBTB.getROn()) {
                            Rb = (RGBIndexBufferB[(int) imageBufferB[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            Rb = 0;
                        }

                        if (RGBTA.getGOn()) {
                            Ga = (RGBIndexBufferA[(int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            Ga = 0;
                        }

                        if (RGBTB.getGOn()) {
                            Gb = (RGBIndexBufferB[(int) imageBufferB[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            Gb = 0;
                        }

                        if (RGBTA.getBOn()) {
                            Ba = (RGBIndexBufferA[(int) imageBufferA[index + 3]] & 0x000000ff);
                        } else {
                            Ba = 0;
                        }

                        if (RGBTB.getBOn()) {
                            Bb = (RGBIndexBufferB[(int) imageBufferB[index + 3]] & 0x000000ff);
                        } else {
                            Bb = 0;
                        }
                    } else {
                        Ra = imageBufferA[index + 1];
                        Rb = imageBufferB[index + 1];
                        Ga = imageBufferA[index + 2];
                        Gb = imageBufferB[index + 2];
                        Ba = imageBufferA[index + 3];
                        Bb = imageBufferB[index + 3];
                    }

                    Ra = (int) ((Ra * alphaBlend) + (Rb * alphaPrime));
                    Ga = (int) ((Ga * alphaBlend) + (Gb * alphaPrime));
                    Ba = (int) ((Ba * alphaBlend) + (Bb * alphaPrime));

                    if (paintBitmap.get(offset + j) == true) {
                        bufferWrite[bufferAdr] = (byte) ((Ba * opacityPrime) + blue);
                        bufferWrite[bufferAdr + 1] = (byte) ((Ga * opacityPrime) + green);
                        bufferWrite[bufferAdr + 2] = (byte) ((Ra * opacityPrime) + red);
                        bufferAdr = bufferAdr + 3;
                    } else {
                        bufferWrite[bufferAdr] = (byte) Ba;
                        bufferWrite[bufferAdr + 1] = (byte) Ga;
                        bufferWrite[bufferAdr + 2] = (byte) Ra;
                        bufferAdr = bufferAdr + 3;
                    }
                } // for (x = 0; x < xDim - xPad; x++)

                for (x = 0; x < xPad; x++) {
                    bufferWrite[bufferAdr] = (byte) 0;
                    bufferWrite[bufferAdr + 1] = (byte) 0;
                    bufferWrite[bufferAdr + 2] = (byte) 0;
                    bufferAdr = bufferAdr + 3;
                } // for (x = 0; x < xPad; x++)
            } // for (y = yDim - 1; y >= 0; y--)
        }

        raFile.write(bufferWrite);
    }

    /**
     * Write BGR AVI triplet for color image. Since this is a frame captured on screen, this method is much simpler.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeAVITripletCFrames() throws IOException {

        int j;
        int bufferSize;
        int index;
        float redMapped, greenMapped, blueMapped;
        int x, y;
        int bufferAdr;

        bufferSize = (xDim - xPad) * yDim * 4;
        bufferAdr = 0;

        try {
            imageA.exportData(0, bufferSize, imageBufferA);
        } catch (IOException error) {
            MipavUtil.displayError("" + error);

            return;
        }

        for (y = yDim - 1; y >= 0; y--) {

            for (x = 0; x < (xDim - xPad); x++) {
                j = x + (y * (xDim - xPad));
                index = 4 * j;
                redMapped = imageBufferA[index + 1];
                greenMapped = imageBufferA[index + 2];
                blueMapped = imageBufferA[index + 3];
                bufferWrite[bufferAdr] = (byte) blueMapped;
                bufferWrite[bufferAdr + 1] = (byte) greenMapped;
                bufferWrite[bufferAdr + 2] = (byte) redMapped;
                bufferAdr = bufferAdr + 3;
            } // for (x = 0; x < xDim-xPad; x++)

            for (x = 0; x < xPad; x++) {
                bufferWrite[bufferAdr] = (byte) 0;
                bufferWrite[bufferAdr + 1] = (byte) 0;
                bufferWrite[bufferAdr + 2] = (byte) 0;
                bufferAdr = bufferAdr + 3;
            } // for (x = 0; x < xPad; x++)
        } // for (y = yDim - 1; y >= 0; y--)

        raFile.write(bufferWrite);
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeidx1CHUNK() throws IOException {

        // Write the idx1 CHUNK
        // Write the 'idx1' signature
        int i;
        long idx1Pos;
        long saveidx1Length;
        long endPos;
        long idx1Place;
        int frameSize;

        idx1Pos = raFile.getFilePointer();
        raFile.seek(saveLIST2Size);
        writeInt((int) (idx1Pos - (saveLIST2Size + 4)), endianess);
        raFile.seek(idx1Pos);

        byte[] idx1Signature = new byte[4];
        idx1Signature[0] = 105; // i
        idx1Signature[1] = 100; // d
        idx1Signature[2] = 120; // x
        idx1Signature[3] = 49; // 1
        raFile.write(idx1Signature);

        // Write the length of the idx1 CHUNK not including the idx1 signature and the 4 length
        // bytes. Write 0 for now.
        saveidx1Length = raFile.getFilePointer();
        writeInt(0, endianess);

        for (i = 0; i < (totalBlankFrames + totalDataFrames); i++) {

            // In the ckid field write the 4 character code to identify the chunk 00db or 00dc
            idx1Place = raFile.getFilePointer();
            raFile.seek(savedbLength[i] - 1);
            dataSignature[3] = raFile.readByte(); // b for data frames and c for blank frames
            frameSize = getInt(endianess);
            raFile.seek(idx1Place);
            raFile.write(dataSignature);

            if (dataSignature[3] == 0x62) { // 00db
                writeInt(0x10, endianess); // Write the flags - select AVIIF_KEYFRAME
            } else {
                writeInt(0x00, endianess);
            }

            // AVIIF_KEYFRAME 0x00000010L
            // The flag indicates key frames in the video sequence.
            // Key frames do not need previous video information to be decompressed.
            // AVIIF_NOTIME 0x00000100L The CHUNK does not influence video timing(for
            // example a palette change CHUNK).
            // AVIIF_LIST 0x00000001L Marks a LIST CHUNK.
            // AVIIF_TWOCC 2L
            // AVIIF_COMPUSE 0x0FFF0000L These bits are for compressor use.
            writeInt((int) (savedbLength[i] - 4 - savemovi), endianess);

            // Write the offset (relative to the 'movi' field) to the relevant CHUNK
            writeInt(frameSize, endianess); // Write the length of the relevant

            // CHUNK.  Note that this length is
            // also written at savedbLength

        } // for (i = 0; i < (totalBlankFrames+totalDataFrames); i++)

        endPos = raFile.getFilePointer();
        raFile.seek(saveFileSize);
        writeInt((int) (endPos - (saveFileSize + 4)), endianess);
        raFile.seek(saveidx1Length);
        writeInt((int) (endPos - (saveidx1Length + 4)), endianess);
        raFile.close();
    }

    /**
     * Writes the RLE8 encoded LUT index.
     *
     * @param   timeSlice    t (time) slice to show
     * @param   slice        z slice to show
     * @param   pixStore     DOCUMENT ME!
     * @param   lastStore    DOCUMENT ME!
     * @param   encodeStore  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeRLE8(int timeSlice, int slice, byte[] pixStore, byte[] lastStore, byte[] encodeStore)
            throws IOException {
        int pixIndex;
        int count;
        int encodeIndex;
        int i, index;
        int zDimSlices = 0;
        boolean sameA = false;
        int deltaX, deltaY;
        int sameX = 0;
        int sameY = 0;

        if (imageA.getNDims() >= 3) {
            zDimSlices = zDim;
        }

        try {

            // if (imageA.getType() == ModelStorageBase.DCOMPLEX)
            // imageA.ExportDComplexSliceXY(timeSlice*zDimSlices + slice,imageBufferA, logMagDisplay);
            if (imageA.getType() == ModelStorageBase.COMPLEX) {
                imageA.exportComplexSliceXY((timeSlice * zDimSlices) + slice, imageBufferA, false);
            } else {
                imageA.exportSliceXY((timeSlice * zDimSlices) + slice, imageBufferA);
            }

        } catch (IOException error) {
            MipavUtil.displayError("" + error); // Need to fix this

            return;
        }

        pixIndex = 0;

        for (int y = yDim - 1; y >= 0; y--) {

            for (int x = 0; x < (xDim - xPad); x++) {
                index = x + (y * (xDim - xPad));
                pixStore[pixIndex++] = (byte) (((imageBufferA[index] - imageMinA) * remapConstA) + 0.5);
            } // for (x = 0; x < xDim - xPad; x++)

            for (int x = xDim - xPad; x < xDim; x++) {
                pixStore[pixIndex++] = (byte) 0;
            } // for (x = xDim - xPad; x < xDim; x++)
        } // for (y = yDim - 1; y >= 0; y--)

        encodeIndex = 0;
        count = 0;

        for (int y = 0; y < yDim; y++) {

            for (int x = 0; x < xDim; x++) {
                index = x + (y * xDim);

                if ((pixStore[index] == lastStore[index]) && (((timeSlice * zDimSlices) + slice) > 0)) {

                    if (count > 0) {
                        encodeStore[encodeIndex++] = (byte) count;
                        encodeStore[encodeIndex++] = pixStore[index - 1];
                        count = 0;
                    }

                    if ((x == (xDim - 1)) && (y == (yDim - 1))) {

                        // end of bitmap
                        encodeStore[encodeIndex++] = (byte) 0;
                        encodeStore[encodeIndex++] = (byte) 1;
                    } else if (!sameA) {
                        sameA = true;
                        sameX = x;
                        sameY = y;
                    }
                } else if ((sameA) && (pixStore[index] != lastStore[index]) &&
                               (((timeSlice * zDimSlices) + slice) > 0)) {
                    sameA = false;

                    if (x < sameX) {
                        count = 1;

                        // position to end of line
                        encodeStore[encodeIndex++] = (byte) 0;
                        encodeStore[encodeIndex++] = (byte) 0;

                        // set position
                        deltaX = x;
                        deltaY = y - (sameY + 1);

                        while ((deltaX > 0) || (deltaY > 0)) {
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 2;
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaX, 255);
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaY, 255);
                            deltaX -= 255;

                            if (deltaX < 0) {
                                deltaX = 0;
                            }

                            deltaY -= 255;

                            if (deltaY < 0) {
                                deltaY = 0;
                            }
                        } // while ((deltaX > 0) || (deltaY > 0))
                    } // if (x < sameX)
                    else if ((x == (xDim - 1)) && (y == (yDim - 1))) {
                        count = 0;

                        // set position
                        deltaX = x - sameX;
                        deltaY = y - sameY;

                        while ((deltaX > 0) || (deltaY > 0)) {
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 2;
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaX, 255);
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaY, 255);
                            deltaX -= 255;

                            if (deltaX < 0) {
                                deltaX = 0;
                            }

                            deltaY -= 255;

                            if (deltaY < 0) {
                                deltaY = 0;
                            }
                        } // while ((deltaX > 0) || (deltaY > 0))

                        // write the byte at (xDim-1,yDim-1)
                        encodeStore[encodeIndex++] = (byte) 1;
                        encodeStore[encodeIndex++] = pixStore[index];

                        // end of bitmap for this dc
                        encodeStore[encodeIndex++] = (byte) 0;
                        encodeStore[encodeIndex++] = (byte) 1;
                    } // else if ((x == xDim-1) && (y == yDim-1))
                    else if (x == (xDim - 1)) {
                        count = 0;

                        // set position
                        deltaX = x - sameX;
                        deltaY = y - sameY;

                        while ((deltaX > 0) || (deltaY > 0)) {
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 2;
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaX, 255);
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaY, 255);
                            deltaX -= 255;

                            if (deltaX < 0) {
                                deltaX = 0;
                            }

                            deltaY -= 255;

                            if (deltaY < 0) {
                                deltaY = 0;
                            }
                        } // while ((deltaX > 0) || (deltaY > 0))

                        // write the byte
                        encodeStore[encodeIndex++] = (byte) 1;
                        encodeStore[encodeIndex++] = pixStore[index];

                        // end of line
                        encodeStore[encodeIndex++] = (byte) 0;
                        encodeStore[encodeIndex++] = (byte) 0;
                    } // else if (x == (xDim-1))
                    else {
                        count = 1;

                        // set position
                        deltaX = x - sameX;
                        deltaY = y - sameY;

                        while ((deltaX > 0) || (deltaY > 0)) {
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 2;
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaX, 255);
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaY, 255);
                            deltaX -= 255;

                            if (deltaX < 0) {
                                deltaX = 0;
                            }

                            deltaY -= 255;

                            if (deltaY < 0) {
                                deltaY = 0;
                            }
                        } // while ((deltaX > 0) || (deltaY > 0))
                    } // else
                } // else if ((sameA) && (imageBufferA[index] != lastBufferA[index]))
                else if (x == 0) {
                    count = 1;
                } else if ((count > 0) && (pixStore[index] == pixStore[index - 1])) {
                    count++;

                    if (x == (xDim - 1)) {
                        encodeStore[encodeIndex++] = (byte) count;
                        encodeStore[encodeIndex++] = pixStore[index];
                        count = 0;

                        if (y < (yDim - 1)) {

                            // end of line
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 0;
                        } // if (y < (yDim - 1))
                        else { // y == (yDim - 1)

                            // end of bitmap for this dc
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 1;
                        } // else for y == (yDim - 1)
                    } // if (x == (xDim - 1))
                    else if (count == 255) {
                        encodeStore[encodeIndex++] = (byte) 255;
                        encodeStore[encodeIndex++] = pixStore[index];
                        count = 0;
                    } // else if (count == 255)
                } // else if (((count > 0) && (pixStore[index] == pixStore[index-1]))
                else if ((count > 0) && (pixStore[index] != pixStore[index - 1])) {
                    encodeStore[encodeIndex++] = (byte) count;
                    encodeStore[encodeIndex++] = pixStore[index - 1];
                    count = 1;

                    if (x == (xDim - 1)) {
                        encodeStore[encodeIndex++] = (byte) 1;
                        encodeStore[encodeIndex++] = pixStore[index];
                        count = 0;

                        if (y < (yDim - 1)) {

                            // end of line
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 0;
                        } // if (y < (yDim - 1))
                        else { // y == (yDim - 1)

                            // end of bitmap for this dc
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 1;
                        } // else for y == (yDim - 1)
                    } // if (x == xDim - 1)
                } // else if ((count > 0) && (pixStore[index] != pixStore[index-1]))
                else if (count == 0) {
                    count = 1;

                    if (x == (xDim - 1)) {
                        encodeStore[encodeIndex++] = (byte) 1;
                        encodeStore[encodeIndex++] = pixStore[index];
                        count = 0;

                        if (y < (yDim - 1)) {

                            // end of line
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 0;
                        } // if (y < (yDim - 1))
                        else { // y == (yDim - 1)

                            // end of bitmap for this dc
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 1;
                        } // else for y == (yDim - 1)
                    } // if (x == (xDim - 1))
                } // else if (count == 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)

        // write the length of the encoded data
        writeInt(encodeIndex, endianess);
        bufferWrite = new byte[encodeIndex];
        dcLength[slice + (zDim * timeSlice)] = encodeIndex;

        for (i = 0; i < encodeIndex; i++) {
            bufferWrite[i] = encodeStore[i];
        }

        for (i = 0; i < pixStore.length; i++) {
            lastStore[i] = pixStore[i];
        }

        raFile.write(bufferWrite);
    }
    
    public boolean readWriteImage() throws IOException {
        long LIST1Marker, LISTsubchunkMarker, marker;
        int loop;
        long saveLIST1Size;
        int totalFramesW;
        long saveLIST1subSize = 0L;
        byte handlerW[];
        int lengthW;
        long savestrfSize = 0L;
        float secPerFrame;
        long savestrnPos;
        long saveJUNKsignature;
        int[] imgExtents;
        byte[] fileBuffer;
        int z;
        int totalDataArea;
        int remainingFileLength;
        int totalBytesRead;
        int dataLength;
        boolean dataFound;
        int moviOffset;
        int signature;
        int CHUNKtype;
        boolean haveMoviSubchunk = false;
        int subchunkBytesRead = 0;
        int subchunkBlocksRead = 0;
        boolean chunkRead;
        long startPosition; // position to start reading data
        int actualFrames = 0; // number of frames with data found on first read thru.
        int indexBytesRead = 0;
        int actualFramesW = 0;
        long savedibPos[];
        boolean doWrite[];
        byte dataSignatureW[]; 
        long idx1Pos;
        int zw;
        long saveidx1Length;
        long endPos;
        long saveTotalFramesW;
        long saveLengthW = 0L;
        long saveHere;
        long streamPositionW;
        boolean vidsRead = false;
        long firstDataSignature;
        boolean firstRun;

        try {
            System.out.println("Started " + outputFileName + " creation");
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            
            fileW = new File(fileDir + outputFileName);
            raFileW = new RandomAccessFile(fileW, "rw");
            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFileW.setLength(0);

            endianess = FileBase.LITTLE_ENDIAN; // false

            signature = getInt(endianess);

            if (signature == 0x46464952) {
                // have read RIFF
            } else {
                raFile.close();
                throw new IOException("AVI read header error first 4 bytes = " + signature);
            }
            
            writeIntW(signature, endianess);

            getInt(endianess); // the file size excluding the first 8 bytes
            
            saveFileSize = raFileW.getFilePointer();
            // Bytes 4 thru 7 contain the length of the file.  The length does
            // not include bytes 0 to 7.
            writeIntW(0, endianess); // For now write 0 in the file size location

            int RIFFtype = getInt(endianess);

            if (RIFFtype == 0x20495641) {
                // have read AVI<sp>
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 8-11 = " + RIFFtype);
            }
            
            writeIntW(RIFFtype, endianess);

            int CHUNKsignature = getInt(endianess);

            if (CHUNKsignature == 0x5453494C) {
                // have read LIST for first LIST CHUNK with information on data decoding
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 12-15 = " + CHUNKsignature);
            }
            
            // Write the first LIST chunk, which contains information on data decoding
            writeIntW(CHUNKsignature, endianess);

            int LIST1Size = getInt(endianess); // size of first LIST CHUNK excluding first 8 bytes
            
            // Write the length of the LIST CHUNK not including the first 8 bytes with LIST and 
            // size.  Note that the end of the LIST CHUNK is followed by JUNK.
            saveLIST1Size = raFileW.getFilePointer();
            writeIntW(0, endianess); // For now write 0 in the avih sub-chunk size location

            // with CHUNKsignature and LIST1Size
            LIST1Marker = raFile.getFilePointer();

            CHUNKtype = getInt(endianess);

            if (CHUNKtype == 0x6C726468) {
                // have read hdrl
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 16-19 = " + CHUNKtype);
            }
            
            // Write the chunk type
            writeIntW(CHUNKtype, endianess);

            int avihSignature = getInt(endianess); // signature of avih sub-CHUNK

            if (avihSignature == 0x68697661) {
                // have read avih
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 2-23 = " + avihSignature);
            }
            
            // Write the avih sub-CHUNK
            writeIntW(avihSignature, endianess);

            int avihLength = getInt(endianess); // read the size of the avih sub-CHUNK not

            // including the first 8 bytes for the signature and length
            if (avihLength == 56) {
                // avih sub-CHUNK has expected length
            } else {
                raFile.close();
                throw new IOException("AVI read header error avih sub-CHUNK length = " + avihLength);
            }
            
            // Write the length of the avih sub-CHUNK (56) not including
            // the first 8 bytes for the avihSignature and length
            writeIntW(56, endianess);

            microSecPerFrame = getInt(endianess);

            // System.err.println("Microsec per frame: " + microSecPerFrame);
            Preferences.debug("microSecPerFrame = " + microSecPerFrame + "\n", Preferences.DEBUG_FILEIO);
            writeIntW(microSecPerFrame, endianess);
            
            secPerFrame = 1.0E-6F * microSecPerFrame;
            framesToCapture = Math.max(1, Math.round(captureTime/secPerFrame));
            Preferences.debug("Frames to capture = " + framesToCapture + "\n", Preferences.DEBUG_FILEIO);
            framesToSkip = Math.round(skipTime/secPerFrame);
            Preferences.debug("Frames to skip = " + framesToSkip + "\n", Preferences.DEBUG_FILEIO);

            int maxBytesPerSecond = getInt(endianess);
            Preferences.debug("maxBytesPerSecond = " + maxBytesPerSecond + "\n", Preferences.DEBUG_FILEIO);
            writeIntW(maxBytesPerSecond, endianess);

            // System.err.println("Unknown int: " + getInt(endianess));
            getInt(endianess);
            writeIntW(0, endianess); // dwReserved1 - Reserved 1 field set to zero

            int flags = getInt(endianess);

            if ((flags & 0x10) != 0) {
                AVIF_HASINDEX = true;
            } else {
                AVIF_HASINDEX = false;
            }

            if ((flags & 0x20) != 0) {
                AVIF_MUSTUSEINDEX = true;
            } else {
                AVIF_MUSTUSEINDEX = false;
            }

            if ((flags & 0x100) != 0) {
                AVIF_ISINTERLEAVED = true;
                Preferences.debug("AVIF_ISINTERLEAVED = true\n", Preferences.DEBUG_FILEIO);
            } else {
                AVIF_ISINTERLEAVED = false;
                Preferences.debug("AVIF_ISINTERLEAVED = false\n", Preferences.DEBUG_FILEIO);
            }

            if (AVIF_HASINDEX) {
                Preferences.debug("AVIF_HASINDEX = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_HASINDEX = false\n", Preferences.DEBUG_FILEIO);
            }

            if (AVIF_MUSTUSEINDEX) {
                Preferences.debug("AVIF_MUSTUSEINDEX = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_MUSTUSEINDEX = false\n", Preferences.DEBUG_FILEIO);
            }
            
            if (AVIF_HASINDEX && (!AVIF_MUSTUSEINDEX)) {
                AVIF_MUSTUSEINDEX = true;
                Preferences.debug("Changing AVIF_MUSTUSEINDEX from false to true for fast skipping\n", Preferences.DEBUG_FILEIO);
            }

            if ((flags & 0x800) != 0) {
                Preferences.debug("AVIF_TRUSTCKTYPE = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_TRUSTCKTYPE = false\n", Preferences.DEBUG_FILEIO);
            }

            if ((flags & 0x10000) != 0) {
                Preferences.debug("AVIF_WASCAPTUREFILE = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_WASCAPTUREFILE = false\n", Preferences.DEBUG_FILEIO);
            }

            if ((flags & 0x20000) != 0) {
                Preferences.debug("AVIF_COPYRIGHTED = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_COPYRIGHTED = false\n", Preferences.DEBUG_FILEIO);
            }
            
            writeIntW(0x10, endianess); // dwFlags - just set the bit for AVIF_HASINDEX

            int totalFrames = getInt(endianess);

            //System.err.println("total frames: " + totalFrames);
            Preferences.debug("totalFrames = " + totalFrames + "\n", Preferences.DEBUG_FILEIO);
            totalFramesW = (totalFrames * framesToCapture)/(framesToCapture + framesToSkip);
            int remainderFrames = totalFrames % (framesToCapture + framesToSkip);
            if (remainderFrames > framesToCapture) {
                remainderFrames = framesToCapture;
            }
            totalFramesW += remainderFrames;
            saveTotalFramesW = raFileW.getFilePointer();
            writeIntW(totalFramesW, endianess);

            // However, many AVI frames will have no data and will just be used to repeat the
            // previous frames.  So we will need to read thru the data to get the actual number
            // of frames used in MIPAV before the image is created.  Then a second read thru will
            // take place to import the data into the image.
            int initialFrames = getInt(endianess);
            Preferences.debug("initialFrames = " + initialFrames + "\n", Preferences.DEBUG_FILEIO);
            // dwInitinalFrames - Initial frame for interleaved files
            // Noninterleaved files should specify 0.
            writeIntW(0, endianess);
            
            streams = getInt(endianess);
            Preferences.debug("Number of streams: " + streams + "\n", Preferences.DEBUG_FILEIO);
            // dwStreams - number of streams in the file - here 1 video and zero audio.
            writeIntW(1, endianess);

            int suggestedBufferSize = getInt(endianess);
            Preferences.debug("suggestedBufferSize = " + suggestedBufferSize + "\n", Preferences.DEBUG_FILEIO);
            writeIntW(suggestedBufferSize, endianess);
            width = getInt(endianess); // xDim
            Preferences.debug("width = " + width + "\n", Preferences.DEBUG_FILEIO);
            xDim = width;
            writeIntW(width, endianess);
            height = getInt(endianess); // yDim
            Preferences.debug("height = " + height + "\n", Preferences.DEBUG_FILEIO);
            yDim = height;
            writeIntW(height, endianess);

            // read 4 reserved integers
            for (int i = 0; i < 4; i++) {
                getInt(endianess);
            }
            // dwreserved[4] - Microsoft says to set the following 4 values to 0.
            for (int i = 0; i < 4; i++) {
                writeIntW(0, endianess);
            }
            
            streamPositionW = raFileW.getFilePointer();

            for (loop = 0; (loop < streams); loop++) {

                // read the LIST subCHUNK
                CHUNKsignature = getInt(endianess);
                if (!vidsRead) {
                    writeIntW(CHUNKsignature, endianess);
                }
                
                if (CHUNKsignature == 0x6E727473) {
                    // read strn instead of CHUNK
                    int strnLength = getInt(endianess);
                    if (!vidsRead) {
                        writeIntW(strnLength, endianess);
                    }
                    if ((strnLength % 2) == 1) {
                        strnLength++;
                    }
                    byte[] text = new byte[strnLength];
                    raFile.read(text);
                    if (!vidsRead) {
                        raFileW.write(text);
                    }

                    if (text[strnLength - 1] != 0) {
                        raFile.close();
                        throw new IOException("strn string ends with illegal temination at loop start = " + text[strnLength - 1]);
                    }
                    CHUNKsignature = getInt(endianess);
                    if (!vidsRead) {
                        writeIntW(CHUNKsignature, endianess);
                    }
                } // if (CHUNKSignature == 0x6E727473)

                if (CHUNKsignature == 0x5453494C) {
                    // have read LIST for LIST subCHUNK with information on data decoding
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error signature first LIST subCHUNK = " + CHUNKsignature);
                }

                int LISTsubchunkSize = getInt(endianess); // size of the first LIST subCHUNK not including
                LISTsubchunkMarker = raFile.getFilePointer();
                
                // Write the size of the first LIST subCHUNK not including the first 8 bytes with
                // LIST and size.  Note that saveLIST1Size = saveLIST1subSize + 76, and that
                // the length written to saveLIST1subSize is 76 less than the length written to saveLIST1Size.
                // The end of the first LIST subCHUNK is followed by JUNK.
                if (!vidsRead) {
                    saveLIST1subSize = raFileW.getFilePointer();
                }
                // For now write 0 in CHUNK size location
                if (!vidsRead) {
                    writeIntW(0, endianess);
                }

                // the first 8 signature and length bytes
                CHUNKtype = getInt(endianess);

                if (CHUNKtype == 0x6C727473) {
                    // have read strl
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error no strl in first LIST subCHUNK but = " + CHUNKtype);
                }
                
                // Write the chunk type
                if (!vidsRead) {
                    writeIntW(CHUNKtype, endianess);
                }

                // read the strh subCHUNK
                int strhSignature = getInt(endianess);

                if (strhSignature == 0x68727473) {
                    // have read strh
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error no strhSignature found but = " + strhSignature);
                }
                if (!vidsRead) {
                    writeIntW(strhSignature, endianess);
                }

                int strhLength = getInt(endianess); // length of strh subCHUNK not including first 8

                // signature and length bytes
                // AVI standard documentation mentioned a minimum length of 56,
                // but the Windows Media player was observed to play 5 mjpeg
                // files with strhLength = 48.
                if (strhLength < 48) {
                    raFile.close();
                    throw new IOException("AVI read header error with strhLength = " + strhLength);
                }
                
                // Write the length of the strh sub-CHUNK
                if (!vidsRead) {
                    writeIntW(strhLength, endianess);
                }

                int fccType = getInt(endianess);

                if (fccType == 0x73646976) {
                    // vids read for video stream
                    vidsRead = true;
                } else if (streams > 1) {
                    raFile.seek(LISTsubchunkSize + LISTsubchunkMarker);
                    if (!vidsRead) {
                        raFileW.seek(streamPositionW);
                    }

                    continue;
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error with fccType = " + fccType);
                }
                
                // fccType - Write the type of data stream - here vids for video stream
                writeIntW(fccType, endianess);

                int handler = getInt(endianess);

                byte[] handlerByte = new byte[5];
                handlerByte[0] = (byte) (handler & 0xFF);
                handlerByte[1] = (byte) ((handler >> 8) & 0xFF);
                handlerByte[2] = (byte) ((handler >> 16) & 0xFF);
                handlerByte[3] = (byte) ((handler >> 24) & 0xFF);
                handlerByte[4] = (byte) 0;

                String handlerString = new String(handlerByte);

                System.err.println("Handler String is: " + handlerString);
                
                handlerW = new byte[4];

                if ((handler == 0x20424944 /* DIB<sp> */) || (handler == 0x20424752 /* RGB<sp> */) ||
                        (handler == 0x20574152 /* RAW<sp> */) || (handler == 0x00000000) ||
                        (handlerString.startsWith("00dc"))) {
                    // uncompressed data
                    handlerW[0] = 68; // D
                    handlerW[1] = 73; // I
                    handlerW[2] = 66; // B
                    handlerW[3] = 32; // space
                    compression = 0;
                    Preferences.debug("Uncompressed data\n", Preferences.DEBUG_FILEIO);
                } else if (handlerString.toUpperCase().startsWith("MRLE") ||
                               handlerString.toUpperCase().startsWith("RLE")) {
                    Preferences.debug("Microsoft run length encoding\n", Preferences.DEBUG_FILEIO);
                    /* mrle microsoft run length encoding */
                    handlerW[0] = 0x6D; // m
                    handlerW[1] = 0x72; // r
                    handlerW[2] = 0x6c; // l
                    handlerW[3] = 0x65; // e
                    compression = 1;
                } else {
                    raFile.close();
                    throw new IOException("Unrecognized compression handler is " + handlerString);
                    // tscc is the TechSmith Screen Capture Codec put out by the Techsmith Corporation for use with
                    // their Camtasia Screen "Camcorder" application.  Camtasia is a Microsoft windows application only.
                    //  The company has no plans to develop a version for the apple. Could the program be designed to
                    // use the tscc.exe codec provided by Techsmith?
                }
                raFileW.write(handlerW);

                flags = getInt(endianess);

                if ((flags & 0x00000001) != 0) {
                    raFile.close();
                    throw new IOException("Cannot presently handle AVISF_DISABLED");
                }

                if ((flags & 0x00010000) != 0) {
                    raFile.close();
                    throw new IOException("Cannot presently handle AVISF_VIDEO_PALCHANGES");
                }
                
                // 0x00000001 AVISF_DISABLED The stream data sould be rendered only when
                // explicitly enabled.
                // 0x00010000 AVISF_VIDEO_PALCHANGES Indicates that a palette change is included
                // in the AVI file.  The flag warns the playback software that it
                // will need to animate the palette.
                writeIntW(0, endianess); // dwFlags

                int priority = getInt(endianess);
                Preferences.debug("priority = " + priority + "\n", Preferences.DEBUG_FILEIO);
                // dwPriority - priority of a stream type.  For example, in a file with
                // multiple audio streams, the one with the highest priority might be the
                // default one.
                writeIntW(0, endianess);
                
                initialFrames = getInt(endianess);

                if (initialFrames != 0) {
                    raFile.close();
                    throw new IOException("initialFrames should be 0 for noninterleaved files");
                }
                
                // dwInitialFrames - Specifies how far audio data is skewed ahead of video
                // frames in interleaved files.  Typically, this is about 0.75 seconds.  In
                // interleaved files specify the number of frames in the file prior
                // to the initial frame of the AVI sequence.
                // Noninterleaved files should used zero.
                writeIntW(0, endianess);

                // rate/scale = samples/second
                scale = getInt(endianess);
                Preferences.debug("scale = " + scale + "\n", Preferences.DEBUG_FILEIO);

                // System.err.println("Scale is: " + scale);
                writeIntW(scale, endianess);
                rate = getInt(endianess);

                // System.err.println("Rate is: " + rate);
                Preferences.debug("rate = " + rate + "\n", Preferences.DEBUG_FILEIO);
                writeIntW(rate, endianess);
                
                float samplesPerSecond = (float)rate/(float)scale;
                Preferences.debug("Samples per second = " + samplesPerSecond + "\n", Preferences.DEBUG_FILEIO);
                if (Math.abs(((1.0/samplesPerSecond) - secPerFrame)/secPerFrame) < 0.01) {
                    Preferences.debug("Frame times from 1.0E-6*microSecPerFrame and scale/rate match\n", Preferences.DEBUG_FILEIO);
                }
                else {
                    Preferences.debug("Frame times from 1.0E-6*microSecPerFrame and scale/rate don't match", Preferences.DEBUG_FILEIO);
                }

                int start = getInt(endianess);
                Preferences.debug("start = " + start + "\n", Preferences.DEBUG_FILEIO);
                // dwStart - this field is usually set to zero
                writeIntW(0, endianess);

                int length = getInt(endianess);
                Preferences.debug("length = " + length + "\n", Preferences.DEBUG_FILEIO);
                lengthW = (length * framesToCapture)/(framesToCapture + framesToSkip);
                remainderFrames = length % (framesToCapture + framesToSkip);
                if (remainderFrames > framesToCapture) {
                    remainderFrames = framesToCapture;
                }
                lengthW += remainderFrames;
                saveLengthW = raFileW.getFilePointer();
                // dwLength - set equal to the number of frames
                writeIntW(lengthW, endianess);

                // System.err.println("DWLength: " + length);
                suggestedBufferSize = getInt(endianess);
                Preferences.debug("suggestedBufferSize = " + suggestedBufferSize + "\n", Preferences.DEBUG_FILEIO);
                // dwSuggestedBufferSize - suggested buffer size for reading the stream
                writeIntW(suggestedBufferSize, endianess);

                int quality = getInt(endianess);

                if ((quality > 10000) || (quality < -1)) {
                    raFile.close();
                    throw new IOException("quality = " + quality);
                }

                Preferences.debug("quality = " + quality + "\n", Preferences.DEBUG_FILEIO);
                // dwQuality - encoding quality given by an integer between
                // 0 and 10,000.  If set to -1, drivers use the default
                // quality value.
                writeIntW(quality, endianess);

                int sampleSize = getInt(endianess);
                Preferences.debug("sampleSize = " + sampleSize + "\n", Preferences.DEBUG_FILEIO);
                if ((compression == 0) || (compression == 1296126531)) {
                    writeIntW(3 * width * height, endianess);
                }
                else if (compression == 1) {
                    writeIntW(width * height, endianess);
                }

                // read destination rectangle within movie rectangle
                short left = (short) getSignedShort(endianess);
                Preferences.debug("left = " + left + "\n", Preferences.DEBUG_FILEIO);

                short top = (short) getSignedShort(endianess);
                Preferences.debug("top = " + top + "\n", Preferences.DEBUG_FILEIO);

                short right = (short) getSignedShort(endianess);
                Preferences.debug("right = " + right + "\n", Preferences.DEBUG_FILEIO);

                short bottom = (short) getSignedShort(endianess);
                Preferences.debug("bottom = " + bottom + "\n", Preferences.DEBUG_FILEIO);
                // rcFrame - Specifies the destination rectangle for a text or video stream within the movie
                // rectangle specified by the dwWidth and dwHeight members of the AVI main header structure.
                // The rcFrame member is typically used in support of multiple video streams.  Set this
                // rectangle to the coordinates corresponding to the movie rectangle to update the whole
                // movie rectangle.  Units for this member are pixels.  The upper-left corner of the destination
                // rectangle is relative to the upper-left corner of the movie rectangle.
                writeShortW(left, endianess);
                writeShortW(top, endianess);
                writeShortW(right, endianess);
                writeShortW(bottom, endianess);

                if (strhLength > 56) {
                    byte[] extra = new byte[strhLength - 56];
                    raFile.read(extra);
                    raFileW.write(extra);
                }

                // read the stream format CHUNK
                int strfSignature = getInt(endianess);

                if (strfSignature == 0x66727473) {
                    Preferences.debug("Read strf\n", Preferences.DEBUG_FILEIO);
                    // read strf
                } else {
                    raFile.close();
                    throw new IOException("strf signature incorrectly read as = " + strfSignature);
                }
                
                // Write the stream format chunk
                writeIntW(strfSignature, endianess);

                int strfSize = getInt(endianess);
                // Write the size of the stream format CHUNK not including the first 8 bytes for
                // strf and the size.  Note that the end of the stream format CHUNK is followed by
                // strn.
                savestrfSize = raFileW.getFilePointer();
                // For now write 0 in the strf CHUNK size location
                writeIntW(0, endianess);
                int BITMAPINFOsize = getInt(endianess);

                if (BITMAPINFOsize > strfSize) {
                    BITMAPINFOsize = strfSize;
                }

                if (BITMAPINFOsize < 40) {
                    raFile.close();
                    throw new IOException("Cannot handle BITMAPINFO size = " + BITMAPINFOsize);
                }
                
                // biSize - write header size of BITMAPINFO header structure
                writeIntW(BITMAPINFOsize, endianess);

                width = getInt(endianess);
                Preferences.debug("width = " + width + "\n", Preferences.DEBUG_FILEIO);
                // biWidth - image width in pixels
                writeIntW(width, endianess);
                height = getInt(endianess);
                Preferences.debug("height = " + height + "\n", Preferences.DEBUG_FILEIO);
                // biHeight - image height in pixels.  If height is positive,
                // the bitmap is a bottom up DIB and its origin is in the lower left corner.  If
                // height is negative, the bitmap is a top-down DIB and its origin is the upper
                // left corner.  This negative sign feature is supported by the 
                // Windows Media Player, but it is not
                // supported by PowerPoint
                writeIntW(height, endianess);

                short planes = (short) getSignedShort(endianess);

                if (planes != 1) {
                    raFile.close();
                    throw new IOException("planes has an incorrect value = " + planes);
                }
                // biPlanes - number of color planes in whcih the data is stored
                writeShortW((short)1, endianess);

                bitCount = (short) getSignedShort(endianess);
                Preferences.debug("bitCount = " + bitCount + "\n", Preferences.DEBUG_FILEIO);
                // biBitCount - number of bits per pixel
                writeShortW(bitCount, endianess);

                compression = getInt(endianess);

                if (compression == 0) {
                    Preferences.debug("Compression is BI_RGB\n", Preferences.DEBUG_FILEIO);
                    // BI_RGB uncompressed
                } else if (compression == 1) {
                    Preferences.debug("Compression is BI_RLE8\n", Preferences.DEBUG_FILEIO);
                    // BI_RLE8
                } else if (compression == 2) {

                    // BI_RLE4
                    raFile.close();
                    throw new IOException("Cannot currently handle 4 bit run length encoding");
                } else if (compression == 3) {
                    // BI_BITFIELDS
                    // To allow for arbitrarily packed RGB samples, BI_BITFIELDS specifies a
                    // mask field for each of the red, green, and blue pixel components.
                    // These masks indicate the bit positions occupied by each color
                    // component in a pixel.  In general, the masks are passed to a driver
                    // or video API using means other than a basic BITMAPINFOHEADER(such
                    // as using the appropriate fields in a DirectDraw DDPIXELFORMAT
                    // structure) but it might be valid to append the masks to the end of
                    // the BITMAPINFOHEADER in much the same way that a palette is appended
                    // for palettised formats.
                    //
                    // For example, 16 bit RGB 5:6:5 can be described using BI_BITFIELDS
                    // and the following bitmasks:

                    // Red  0xF800 (5 bits of red)
                    // Green 0x07E0 (6 bits of green)
                    // Blue  0x001F (5 bits of blue)

                    // In this case, if used with a BITMAPINFOHEADER, the bitmasks are
                    // u_int16s (16 bit) since the biBitFields field is set to 16.  For
                    // a 32bpp version, the bitmasks are each u_int32s.
                    raFile.close();
                    throw new IOException("Cannot currently handle BI_BITFIELDS compresion");
                } else {
                    raFile.close();
                    throw new IOException("Unknown compression with value = " + compression);
                }

                Preferences.debug("compression = " + compression + "\n", Preferences.DEBUG_FILEIO);

                if (((compression == 0) &&
                         ((bitCount == 4) || (bitCount == 8) || (bitCount == 16) || (bitCount == 24) || (bitCount == 32))) ||
                        ((compression == 1) && (bitCount == 8))) {
                    // OK
                } else {
                    raFile.close();
                    throw new IOException("Cannot currently handle bit count = " + bitCount);
                }
                
                // biCompression - type of compression used
                // 0L for BI_RGB, uncompressed data as bitmap
                // 1L for BI_RLE8, a run-length encoded(RLE) format for bitmaps
                // with 8 bits per pixel.  The compression format is a 2-byte
                // format consisting of a byte count followed by a byte containing
                // a color index.  In addition, the first byte of the pair can be
                // set to zero to indicate an escape character that denotes the end
                // of a line, the end of a bitmap, a delta, or the number of bytes
                // which follow, each of which contains the color index of a single
                // pixel, depending on the
                // value of the second byte of the pair, which can be one of the
                // following values:
                // value             meaning
                // 0                 End of line.
                // 1                 End of bitmap.
                // 2                 Delta.  The two bytes following the
                // escape contain unsigned values indicating
                // the horizontal and vertical offsets
                // of the next pixel from the current
                // position.
                // 3-255             number of bytes that folow, each of which
                // contains the color index of a single pixel
                // Must be padded if an odd value so that it
                // ends on a word boundary.
                // 2L for BI_RLE4, a RLE format for bits with 4 bits per pixel.
                // The compression format is a 2-byte format consisting of a count
                // byte followed by two word-length color indexes.
                // 3L for BI_BITFIELDS, specifies that the bitmap is not compressed
                // and that the color table consists of three DWORD color masks
                // that specify the red, green, and blue components, respectively,
                // of each pixel.  This is valid when used with 16- and 32-bit-
                // per-pixel bitmaps.
                writeIntW(compression, endianess);

                int imageSize = getInt(endianess);
                Preferences.debug("imageSize = " + imageSize + "\n", Preferences.DEBUG_FILEIO);
                // biSizeImage - specifies the size in bytes of the image frame.  This can be
                // set to zero for uncompressed RGB bitmaps.
                if ((compression == 0) || (compression == 1296126531)) {
                    writeIntW(3 * width * height, endianess);
                }
                else if (compression == 1) {
                    writeIntW(width * height, endianess);
                }

                float[] imgResols = new float[5];
                imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = 1.0f;

                int xPixelsPerMeter = getInt(endianess);
                Preferences.debug("xPixelsPerMeter = " + xPixelsPerMeter + "\n", Preferences.DEBUG_FILEIO);
                // biXPelsPerMeter - horizontal resolution in pixels
                writeIntW(xPixelsPerMeter, endianess);

                // System.err.println("xPixelsPerMeter = " + xPixelsPerMeter);
                if (xPixelsPerMeter > 0) {
                    imgResols[0] = (1.0f / xPixelsPerMeter) * 1000.0f;
                }

                int yPixelsPerMeter = getInt(endianess);
                Preferences.debug("yPixelsPerMeter = " + yPixelsPerMeter + "\n", Preferences.DEBUG_FILEIO);
                // biYPelsPerMeter - vertical resolution in pixels
                writeIntW(yPixelsPerMeter, endianess);

                // System.err.println("yPixelsPerMeter = " + yPixelsPerMeter);
                if (yPixelsPerMeter > 0) {
                    imgResols[1] = (1.0f / yPixelsPerMeter) * 1000.0f;
                }

                imgResols[2] = microSecPerFrame;

                // System.err.println("Microseconds per frame (on read): " + microSecPerFrame);

                int colorsUsed = getInt(endianess);
                Preferences.debug("colorsUsed = " + colorsUsed + "\n", Preferences.DEBUG_FILEIO);

                if ((compression == 0) && ((bitCount == 24) || (bitCount == 32)) && (colorsUsed != 0)) {
                    raFile.close();
                    throw new IOException("For 24 and 32 bit uncompressed data software does not currently support colorsUsed = " +
                                          colorsUsed);
                }

                if ((bitCount == 8) && (colorsUsed == 0)) {
                    colorsUsed = 8;
                }
                // biClrUsed - Provides a way for getting smaller color tables.  When this
                // field is set to 0, the number of colors in the color table is based on
                // the biBitCount field (1 indicates 2 colors, 4 indicates 16 colors,
                // 8 indicates 256, and 24 indicates no color table).  A nonzero value
                // specifies the exact number of colors in the table.  So, for example,
                // if an 8-bit DIB uses only 17 colors, then only those 17 colors need
                // to be defined in the table, and biClrUsed is set to 17.  If nonzero
                // for a 24-bit DIB, it indicates the existence of a color table that the
                // application can use for color reference.
                if ((compression == 0) && (bitCount > 8)) {
                    writeIntW(0, endianess);
                }
                else {
                    writeIntW(colorsUsed, endianess);
                }

                int colorsImportant = getInt(endianess);
                Preferences.debug("colorsImportant = " + colorsImportant + "\n", Preferences.DEBUG_FILEIO);
                // biClrImportant - specifies that the first x colors of the color table
                // are important to the DIB.  If the rest of the colors are not available,
                // the image still retains its meaning in an acceptable manner.  When this
                // field is set to zero, all the colors are important, or, rather, their
                // relative importance has not been computed.
                if (compression == 0) {
                    writeIntW(0, endianess);
                }
                else {
                    writeIntW(colorsImportant, endianess);
                }

                if (BITMAPINFOsize > 40) {
                    byte[] extra = new byte[BITMAPINFOsize - 40];
                    raFile.read(extra);
                    raFileW.write(extra);
                }

                if (bitCount == 4) {

                    // read the color table into a LUT
                    int[] dimExtentsLUT = new int[2];
                    dimExtentsLUT[0] = 4;
                    dimExtentsLUT[1] = 256;

                    // FileIO obtains via getModelLUT.
                    // Then, ViewOpenFileUI obtains from FileIO via getModelLUT.
                    LUTa = new ModelLUT(ModelLUT.GRAY, colorsUsed, dimExtentsLUT);
                    lutBuffer = new byte[4 * colorsUsed];
                    raFile.read(lutBuffer);
                    raFileW.write(lutBuffer);   

                    for (int i = 0; i < colorsUsed; i++) {
                        LUTa.set(0, i, 1.0f); // alpha
                        LUTa.set(1, i, (lutBuffer[(4 * i) + 2] & 0x000000ff)); // red
                        LUTa.set(2, i, (lutBuffer[(4 * i) + 1] & 0x000000ff)); // green
                        LUTa.set(3, i, (lutBuffer[4 * i] & 0x000000ff)); // blue
                    } // for (i = 0; i < colorsUsed; i++)

                    for (int i = colorsUsed; i < 256; i++) {
                        LUTa.set(0, i, 1.0f);
                        LUTa.set(1, i, 0);
                        LUTa.set(2, i, 0);
                        LUTa.set(3, i, 0);
                    } // for (i = colorsUsed; i < 256; i++)

                    LUTa.makeIndexedLUT(null);
                } // if (bitCount == 4)
                else if (bitCount == 8) {

                    // read the color table into a LUT
                    int[] dimExtentsLUT = new int[2];
                    dimExtentsLUT[0] = 4;
                    dimExtentsLUT[1] = 256;

                    // FileIO obtains via getModelLUT.
                    // Then, ViewOpenFileUI obtains from FileIO via getModelLUT.
                    LUTa = new ModelLUT(ModelLUT.GRAY, colorsUsed, dimExtentsLUT);
                    lutBuffer = new byte[4 * colorsUsed];
                    raFile.read(lutBuffer);
                    raFileW.write(lutBuffer);

                    for (int i = 0; i < colorsUsed; i++) {
                        LUTa.set(0, i, 1.0f); // alpha
                        LUTa.set(1, i, (lutBuffer[(4 * i) + 2] & 0x000000ff)); // red
                        LUTa.set(2, i, (lutBuffer[(4 * i) + 1] & 0x000000ff)); // green
                        LUTa.set(3, i, (lutBuffer[4 * i] & 0x000000ff)); // blue
                    } // for (i = 0; i < colorsUsed; i++)

                    for (int i = colorsUsed; i < 256; i++) {
                        LUTa.set(0, i, 1.0f);
                        LUTa.set(1, i, 0);
                        LUTa.set(2, i, 0);
                        LUTa.set(3, i, 0);
                    } // for (i = colorsUsed; i < 256; i++)

                    LUTa.makeIndexedLUT(null);
                } // else if (bitCount == 8)

                // Calculate the number of strf CHUNK bytes after the end of BITMAPINFO
                int strfEndBytes = strfSize - BITMAPINFOsize;

                if ((bitCount == 4) ||(bitCount == 8)) {
                    strfEndBytes = strfEndBytes - (4 * colorsUsed);
                }

                if (strfEndBytes > 0) {
                    byte[] byteb = new byte[strfEndBytes];
                    raFile.read(byteb);
                    raFileW.write(byteb);
                }
            } // for (loop = 0; loop < streams; loop++)

            marker = raFile.getFilePointer();
            
            savestrnPos = raFileW.getFilePointer();
            raFileW.seek(savestrfSize);
            writeIntW((int)(savestrnPos - (savestrfSize + 4)), endianess);
            raFileW.seek(savestrnPos);

            if (marker < (LIST1Marker + LIST1Size)) {
                // read strn subCHUNK
                
                int strnSignature = getInt(endianess);
                writeIntW(strnSignature, endianess);

                if (strnSignature == 0x6E727473) {

                    int strnLength = getInt(endianess);
                    writeIntW(strnLength, endianess);
                    if ((strnLength % 2) == 1) {
                        strnLength++;
                    }
                    byte[] text = new byte[strnLength];
                    raFile.read(text);
                    raFileW.write(text);

                    if (text[strnLength - 1] != 0) {
                        raFile.close();
                        throw new IOException("strn string ends with illegal temination = " + text[strnLength - 1]);
                    }
                } // if (strnSignature == 0x6E727473)
                else if (strnSignature == 0x4B4E554A) {

                    // have read JUNK for a JUNK padding CHUNK
                    int JUNKlength = getInt(endianess);
                    writeIntW(JUNKlength, endianess);
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + JUNKlength);
                    byte byteb[] = new byte[JUNKlength];
                    raFileW.write(byteb);
                    CHUNKsignature = getInt(endianess);
                    writeIntW(CHUNKsignature, endianess);

                    if (CHUNKsignature != 0x5453494C) {
                        raFile.close();
                        throw new IOException("After JUNK CHUNK unexpected signature = " + CHUNKsignature);
                    }
                } // else if (strnSignature == 0x4B4E554A)
                else if (strnSignature == 0x54465349) {
 
                    // have read ISFT
                    int ISFTlength = getInt(endianess);
                    writeIntW(ISFTlength,endianess);
                    if ((ISFTlength % 2) == 1) {
                        ISFTlength++;
                    }
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + ISFTlength);
                    byte byteb[] = new byte[ISFTlength];
                    raFileW.write(byteb);
                } // else if (strnSignature == 0x54465349)
                else if (strnSignature == 0x74646576) {

                    // have read vedt
                    int vedtLength = getInt(endianess);
                    writeIntW(vedtLength, endianess);
                    if ((vedtLength %2) == 1) {
                        vedtLength++;
                    }
                    byte[] vedt = new byte[vedtLength];
                    raFile.read(vedt);
                    raFileW.write(vedt);
                } else {
                    raFile.close();
                    throw new IOException("strn signature is an erroneous = " + strnSignature);
                }
            }

            raFile.seek(LIST1Marker + LIST1Size);
            signature = getInt(endianess);
            saveJUNKsignature = raFileW.getFilePointer();
            raFileW.seek(saveLIST1Size);
            writeIntW((int)(saveJUNKsignature - (saveLIST1Size + 4)), endianess);
            raFileW.seek(saveLIST1subSize);
            writeIntW((int)(saveJUNKsignature - (saveLIST1subSize + 4)), endianess);
            raFileW.seek(saveJUNKsignature);
            writeIntW(signature, endianess);

            if (signature == 0x4B4E554A) {

                // have read JUNK for a JUNK padding CHUNK
                int JUNKlength = getInt(endianess);
                writeIntW(JUNKlength, endianess);
                marker = raFile.getFilePointer();
                raFile.seek(marker + JUNKlength);
                byte byteb[] = new byte[JUNKlength];
                raFileW.write(byteb);
                CHUNKsignature = getInt(endianess);
                // Write the second LIST chunk, which contains the actual data
                writeIntW(CHUNKsignature, endianess);

                if (CHUNKsignature != 0x5453494C) {
                    raFile.close();
                    throw new IOException("After JUNK CHUNK unexpected signature = " + CHUNKsignature);
                }
            } else if (signature != 0x5453494C) {
                raFile.close();
                throw new IOException("After first LIST CHUNK unexpected signature = " + signature);
            }

            // At this point have read LIST for the second LIST CHUNK which contains the actual data.
            LIST2Size = getInt(endianess);
            // Write the lengthof the LIST CHUNK not including the first 8 bytes with LIST and
            // size.  The end of the second LIST CHUNK is followed by idx1.
            saveLIST2Size = raFileW.getFilePointer();
            // For now write 0
            writeIntW(0, endianess);
            moviPosition = raFile.getFilePointer();
            savemovi = raFileW.getFilePointer();
            idx1Position = moviPosition + LIST2Size;
            raFile.seek(idx1Position + 4);
            indexSize = getInt(endianess);
            raFile.seek(moviPosition);
            indexPointer = idx1Position + 8;
            CHUNKtype = getInt(endianess);
            // Write CHUNK type 'movi'
            writeIntW(CHUNKtype, endianess);

            if (CHUNKtype != 0x69766F6D) {

                // have not read movi
                raFile.close();
                throw new IOException("CHUNK type in second LIST CHUNK is an illegal = " + CHUNKtype);
            }
            
            startPosition = raFile.getFilePointer();
            doWrite = new boolean[totalFrames];
            z = 0;
            int skipCount = 0;
            int captureCount = 0;
            // Do first read thru the data to find the actual number of frames used by MIPAV. This must be done before
            // the MIPAV image can be created.

            dataSignature = new byte[4];

            totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes

            // Have encountered LiST2Size > raFile.length(), an impossibility
            remainingFileLength = (int) (raFile.length() - startPosition);

            if (totalDataArea > remainingFileLength) {
                MipavUtil.displayWarning("File appears to be truncated");
                totalDataArea = remainingFileLength;
            }

            totalBytesRead = 0;

            // Check for LIST rec<sp> subchunks
            if (!AVIF_MUSTUSEINDEX) {
                signature = getInt(endianess);

                if (signature == 0x5453494C) {

                    // have read LIST
                    LIST2subchunkSize = getInt(endianess);
                    moviSubchunkPosition = raFile.getFilePointer();
                    CHUNKtype = getInt(endianess);

                    if (CHUNKtype == 0x20636572) {

                        // have read rec<sp>
                        haveMoviSubchunk = true;
                        Preferences.debug("LIST rec found\n", Preferences.DEBUG_FILEIO);
                        subchunkBytesRead = 0;
                        subchunkBlocksRead = 0;
                    } else {
                        raFile.close();
                        throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                    }
                } else {
                    raFile.seek(startPosition);
                }
            } // if (!AVIF_MUSTUSEINDEX)

            chunkRead = true;

            firstDataSignature = raFile.getFilePointer();
            firstRun = true;
            loop1:
            while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                       (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {

                if (AVIF_MUSTUSEINDEX) {
                    raFile.seek(indexPointer);
                    dataFound = false;

                    while (!dataFound) {
                        raFile.read(dataSignature);

                        if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                (dataSignature[3] > 0x63 /* c */)) {
                            indexPointer = indexPointer + 16;
                            indexBytesRead += 16;
                            if (indexBytesRead >= indexSize) {
                                break loop1;
                            }
                            raFile.seek(indexPointer);
                        } else {
                            dataFound = true;
                        }
                    } // while (!dataFound)

                    indexPointer = indexPointer + 8;
                    raFile.seek(indexPointer);
                    moviOffset = getInt(endianess);
                    if (firstRun && (moviOffset == firstDataSignature)) {
                        moviPosition = 0L;
                    }
                    indexPointer = indexPointer + 8;
                    indexBytesRead += 16;
                    raFile.seek(moviPosition + (long) moviOffset);
                    firstRun = false;
                } // if (AVIFMUSTINDEX)

                raFile.read(dataSignature);
                totalBytesRead = totalBytesRead + 4;
                subchunkBytesRead = subchunkBytesRead + 4;

                if ((dataSignature[2] == 0x64 /* d */) &&
                        ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                    dataLength = getInt(endianess);

                    if ((dataLength % 2) == 1) {
                        dataLength++;
                    }

                    if (dataLength > 2) {
                        // Only consider frames with dataLength > 0 in capturing
                        // and skipping
                        // RLE uses 2 bytes to show a repeat frame
                        actualFrames++;
                        if (captureCount < framesToCapture) {
                            doWrite[z] = true;
                            captureCount++;
                        }
                        else {
                            doWrite[z] = false;
                            skipCount++;
                        }
                        if (skipCount == framesToSkip) {
                            captureCount = 0;
                            skipCount = 0;
                        }
                    }
                    z++;

                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    long ptr = raFile.getFilePointer();
                    raFile.seek(ptr + dataLength);
                    totalBytesRead = totalBytesRead + dataLength;
                    subchunkBytesRead += dataLength;
                } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                else {
                    dataLength = getInt(endianess);

                    if ((dataLength % 2) == 1) {
                        dataLength++;
                    }

                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    long ptr = raFile.getFilePointer();
                    raFile.seek(ptr + dataLength);
                    totalBytesRead = totalBytesRead + dataLength;
                    subchunkBytesRead += dataLength;
                } // else

                subchunkBlocksRead++;

                if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                    totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                    raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                    // Check for LIST rec<sp> subchunks
                    signature = getInt(endianess);
                    totalBytesRead += 4;

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        totalBytesRead += 4;
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            totalBytesRead += 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                        }
                    } else {
                        chunkRead = false;
                    }
                } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
            } // while ((totalBytesRead < totalDataArea) && chunkRead)

            Preferences.debug("totalBytesRead = " + totalBytesRead + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("totalDataArea = " + totalDataArea + "\n", Preferences.DEBUG_FILEIO);
            indexPointer = idx1Position + 8;
            indexBytesRead = 0;

            if (actualFrames > 1) {
                imgExtents = new int[3];
                imgExtents[2] = actualFrames;
            } else {
                imgExtents = new int[2];
            }
            actualFramesW = (actualFrames * framesToCapture)/(framesToCapture + framesToSkip);
            remainderFrames = actualFrames % (framesToCapture + framesToSkip);
            if (remainderFrames > framesToCapture) {
                remainderFrames = framesToCapture;   
            }
            actualFramesW += remainderFrames;
            saveHere = raFileW.getFilePointer();
            raFileW.seek(saveTotalFramesW);
            writeIntW(actualFramesW, endianess);
            raFileW.seek(saveLengthW);
            writeIntW(actualFramesW, endianess);
            raFileW.seek(saveHere);
            savedibPos = new long[actualFramesW];
            dcLength = new int[actualFramesW];
            zw = 0;

            imgExtents[0] = width;
            imgExtents[1] = height;
            
            if (compression == 0) {
                
                // Write the data record signature '00db' where db means the DIB bitmap data (uncompressed)
                // follows.  The characters 00 are used to identify the stream.
                dataSignatureW = new byte[4];
                dataSignatureW[0] = 48; // 0
                dataSignatureW[1] = 48; // 0
                dataSignatureW[2] = 100; // d
                dataSignatureW[3] = 98; // b
                
                // Each 3-byte triplet in the bitmap array represents the relative intensities
                // of blue, green, and red, respectively, for a pixel.  The color bytes are
                // in reverse order from the Windows convention.
            } // if (compression == 0)
            else { // compression == 1 or MSVC
                // Write the data record signature '00dc' where dc means that DIB bitmap data (compressed)
                // follows.  The characters 00 are used to identify the stream.
                dataSignatureW = new byte[4];
                dataSignatureW[0] = 48; // 0
                dataSignatureW[1] = 48; // 0
                dataSignatureW[2] = 100; // d
                dataSignatureW[3] = 99; // c
            } // else compression == 1

            // Now that the image is created this second read thru actually imports the data into the image.
            raFile.seek(startPosition);

            if (compression == 0) {

                dataSignature = new byte[4];

                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes

                // Have encountered LiST2Size > raFile.length(), an impossibility
                remainingFileLength = (int) (raFile.length() - startPosition);

                if (totalDataArea > remainingFileLength) {
                    totalDataArea = remainingFileLength;
                }

                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (!AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (1AVIF_MUSTUSEINDEX
                
                z = 0;
                chunkRead = true;

                while ((((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) && (zw < actualFramesW)) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                raFile.seek(indexPointer);
                            } else if (doWrite[z]) {
                                z++;
                                dataFound = true;
                            } else {
                                z++;
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                raFile.seek(indexPointer);
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                            (dataSignature[3] > 0x63 /* c */)) {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += 4;
                    } else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        if ((totalBytesRead + dataLength) <= totalDataArea) {
                            raFileW.write(dataSignature);
                            savedibPos[zw] = raFileW.getFilePointer();
                            dcLength[zw++] = dataLength;
                            writeIntW(dataLength, endianess);
                            fileBuffer = new byte[dataLength];
                            raFile.read(fileBuffer);
                            raFileW.write(fileBuffer);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;
                        } // if ((totalBytesRead + dataLength) <= totalDataArea)
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < (totalDataArea-8)) && chunkRead)
            } // if (compression == 0)
            else if (compression == 1) {
                dataSignature = new byte[4];
                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes
                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (!AVIF_MUSTUSEINDEX)

                z = 0;
                chunkRead = true;

                while ((((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) && (zw < actualFramesW)) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                raFile.seek(indexPointer);
                            } else if (doWrite[z]) {
                                z++;
                                dataFound = true;
                            } else {
                                z++;
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                raFile.seek(indexPointer);
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] == 0x64 /* d */) &&
                            ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                       
                        dataLength = getInt(endianess);
                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;
                        raFileW.write(dataSignature);
                        savedibPos[zw]= raFileW.getFilePointer();
                        dcLength[zw++] = dataLength;
                        writeIntW(dataLength, endianess);
                        fileBuffer = new byte[dataLength];
                        raFile.read(fileBuffer);
                        raFileW.write(fileBuffer);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += dataLength;

                        
                    } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                    else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += 4;
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHUNKtype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < totalDataArea) && chunkRead)

            } // else if (compression == 1)
            

            raFile.close();
            
            // Write the idx1 CHUNK
            // Write the 'idx1' signature
            idx1Pos = raFileW.getFilePointer();
            raFileW.seek(saveLIST2Size);
            writeIntW((int) (idx1Pos - (saveLIST2Size + 4)), endianess);
            raFileW.seek(idx1Pos);
            
            byte[] idx1Signature = new byte[4];
            idx1Signature[0] = 105; // i
            idx1Signature[1] = 100; // d
            idx1Signature[2] = 120; // x
            idx1Signature[3] = 49; // 1
            raFileW.write(idx1Signature);
            
            // Write the length of the idx1 CHUNK not including the idx1 signature and the 4 length
            // bytes.  Write 0 for now.
            saveidx1Length = raFileW.getFilePointer();
            writeIntW(0, endianess);
            for (z = 0; z < actualFramesW; z++) {
                raFileW.write(dataSignatureW);
                writeIntW(0x10, endianess);
                writeIntW((int)(savedibPos[z] - 4 - savemovi), endianess);
                writeIntW(dcLength[z], endianess);
            }
            
            endPos = raFileW.getFilePointer();
            raFileW.seek(saveFileSize);
            writeIntW((int)(endPos - (saveFileSize + 4)), endianess);
            raFileW.seek(saveidx1Length);
            writeIntW((int)(endPos - (saveidx1Length + 4)), endianess);
            raFileW.close();
            System.out.println("Finished " + outputFileName + " creation");
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
        return true;    
    }
    
    /**
     * Writes an int as four bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeIntW(int data, boolean bigEndian) throws IOException {
        byte[] buffer = new byte[4];

        if (bigEndian) {
            buffer[0] = (byte) (data >>> 24);
            buffer[1] = (byte) (data >>> 16);
            buffer[2] = (byte) (data >>> 8);
            buffer[3] = (byte) (data & 0xff);
        } else {
            buffer[0] = (byte) (data & 0xff);
            buffer[1] = (byte) (data >>> 8);
            buffer[2] = (byte) (data >>> 16);
            buffer[3] = (byte) (data >>> 24);
        }

        raFileW.write(buffer);
    }
    
    /**
     * Writes a short as two bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeShortW(short data, boolean bigEndian) throws IOException {
        byte[] buffer = new byte[2];

        if (bigEndian) {
            buffer[0] = (byte) (data >>> 8);
            buffer[1] = (byte) (data & 0xff);
        } else {
            buffer[0] = (byte) (data & 0xff);
            buffer[1] = (byte) (data >>> 8);
        }

        raFileW.write(buffer);
    }
}
