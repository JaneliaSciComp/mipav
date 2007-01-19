package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.Dialog.*;

import java.io.*;


/**
 * Reads BioRaD pic file format.
 */

public class FileBioRad extends FileBase {
    // Note that the pixel size in microns is given by the
    // scaleFactor/(lens*magFactor) where the lens and magFactor are
    // given in the file header, but the scaleFactor is not present
    // in the file header.  The scaleFactor is the scaling number
    // setup for the system on which the image was collected.
    // Therefore, the resolution cannot be calculated.

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int bufferSize;

    /** DOCUMENT ME! */
    private byte[] byteBuffer = null;

    /** if 1 data is UBYTE, otherwise data is SHORT. */
    private short byteFormat;

    /** LUT1 color status. */
    private int color1;

    /** LUT2 color status. */
    private int color2;

    /** DOCUMENT ME! */
    private int dataType;

    /** not used in disk files. */
    private short edited;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** always set to 12345 in BioRad pic files. */
    private int fileID;

    /** DOCUMENT ME! */
    private FileInfoBioRad fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private String fName;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private int[] imgExtents;

    /** Integer part of the lens magnification. */
    private short lens;

    /** magnification factor. */
    private float magFactor;

    /** merged format. */
    private short merged;

    /** DOCUMENT ME! */
    private byte[] name = new byte[32];

    /** if 0 no notes are present if nonzero 96 byte notes follow after the image data at the end of the file. */
    private int notes;

    /** DOCUMENT ME! */
    private short ramp1Max; // LUT1 ramp max

    /** DOCUMENT ME! */
    private short ramp1Min; // LUT1 ramp min

    /** LUT2 ramp max. */
    private short ramp2Max;

    /** LUT2 ramp min. */
    private short ramp2Min;

    /** DOCUMENT ME! */
    private boolean readAgain;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private int xDim = 0;

    /** DOCUMENT ME! */
    private int yDim = 0;

    /** DOCUMENT ME! */
    private int zDim = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * reader/writer constructor.
     *
     * @param      _UI       user interface reference
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileBioRad(ViewUserInterface _UI, String fileName, String fileDir) throws IOException {
        UI = _UI;
        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * reads the BioRad pic file header and data.
     *
     * @param      one  DOCUMENT ME!
     *
     * @return     returns a black and white image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean one) throws IOException {

        // The data types are Sun, hence the byte order is big-endian.
        int i;
        int num; // image number within file

        boolean endianess = LITTLE_ENDIAN;

        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            
            xDim = getSignedShort(endianess); // 0
            yDim = getSignedShort(endianess); // 2
            zDim = getSignedShort(endianess); // 4
            ramp1Min = (short) getSignedShort(endianess); // 6
            ramp1Max = (short) getSignedShort(endianess); // 8
            notes = getInt(endianess); // 10
            byteFormat = (short) getSignedShort(endianess); // 14
            num = (short) getSignedShort(endianess); // 16

            

            for (i = 0; i < 32; i++) {
                name[i] = 0;
            }

            readAgain = true;
            i = 0;

            while (readAgain) {
                name[i++] = raFile.readByte(); // 18

                if (name[i - 1] == 0) {
                    readAgain = false;
                }
            }

            fName = new String(name, 0, i - 1);
            raFile.seek(50);
            merged = (short) getSignedShort(endianess); // 50
            color1 = getUnsignedShort(endianess); // 52
            fileID = getUnsignedShort(endianess); // 54

            if (fileID != 12345) {
                throw new IOException("fileID is an illegal " + fileID);
            }

            ramp2Min = (short) getSignedShort(endianess); // 56
            ramp2Max = (short) getSignedShort(endianess); // 58
            color2 = getUnsignedShort(endianess); // 60
            edited = (short) getSignedShort(endianess); // 62
            lens = (short) getSignedShort(endianess); // 64
            magFactor = getFloat(endianess); // 66

            // Position to the start of the image data
            raFile.seek(76);

            fileInfo = new FileInfoBioRad(fileName, fileDir, FileUtility.BIORAD); // dummy fileInfo
            fileInfo.setEndianess(endianess);

            if (zDim > 1) {
                imgExtents = new int[3];
                imgExtents[0] = xDim;
                imgExtents[1] = yDim;
                imgExtents[2] = zDim;
            } else {
                imgExtents = new int[2];
                imgExtents[0] = xDim;
                imgExtents[1] = yDim;
            }

            fileInfo.setExtents(imgExtents);

            if (byteFormat == 1) {
                dataType = ModelStorageBase.UBYTE;
                bufferSize = xDim * yDim;
            } else { // byteFormat == 0
                dataType = ModelStorageBase.SHORT;
                bufferSize = xDim * yDim;
            }

            /*else { // merged pseudocolor
             *  dataType = ModelStorageBase.ARGB; bufferSize = 4*xDim*yDim;}*/
            fileInfo.setDataType(dataType);

            if (one) {
                image = new ModelImage(dataType, new int[] { imgExtents[0], imgExtents[1] }, fileInfo.getFileName(),
                                       UI);
                zDim = 1;
            } else {
                image = new ModelImage(dataType, imgExtents, fileInfo.getFileName(), UI);
            }

            imgBuffer = new float[bufferSize];

            for (i = 0; i < zDim; i++) {

                try {

                    if (one && (imgExtents.length > 2)) {

                        if (dataType == ModelStorageBase.UBYTE) {
                            raFile.seek(imgExtents[2] / 2 * xDim * yDim);
                        } else {
                            raFile.seek(imgExtents[2] * xDim * yDim);
                        }

                        readBuffer(imgExtents[2] / 2, imgBuffer);
                    } else {
                        readBuffer(i, imgBuffer); // Slice a time;
                    }

                    image.setFileInfo(fileInfo, i);
                } catch (IOException error) {
                    throw new IOException("FileTiff: read: " + error);
                }

                image.importData(i * bufferSize, imgBuffer, false);
            } // for (i = 0; i < imageSlice; i++)

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
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice   into the file
     * @param      buffer  buffer where the info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readBuffer(int slice, float[] buffer) throws IOException {
        int i = 0;
        int b1, b2;
        int j;
        int nBytes;
        long progress, progressLength, mod;

        switch (dataType) {

            case ModelStorageBase.UBYTE:
                nBytes = xDim * yDim;
                if (byteBuffer == null) {
                    byteBuffer = new byte[nBytes];
                }

                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * zDim;
                mod = progressLength / 10;
                
                for (j = 0; j < nBytes; j++, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = byteBuffer[j] & 0xff;
                }

                break;

            case ModelStorageBase.SHORT:
                nBytes = 2 * xDim * yDim;
                if (byteBuffer == null) {
                    byteBuffer = new byte[nBytes];
                }

                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * zDim;
                mod = progressLength / 10;
                
                for (j = 0; j < nBytes; j += 2, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    buffer[i] = (short) ((b2 << 8) + b1); // little endian
                }

                break;

            case ModelStorageBase.ARGB:

                // from 2 color merged psuedocolor
                nBytes = 2 * xDim * yDim;
                if (byteBuffer == null) {
                    byteBuffer = new byte[nBytes];
                }

                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * zDim;
                mod = progressLength / 10;
                

                // For the moment I compress RGB images to unsigned bytes.
                for (j = 0; j < nBytes; j += 2, i += 4) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = 255;
                    buffer[i + 1] = getUnsignedByte(byteBuffer, j);
                    buffer[i + 2] = getUnsignedByte(byteBuffer, j + 1);
                    buffer[i + 3] = 0;
                }

                break;
        } // switch(dataType)
    }
}
