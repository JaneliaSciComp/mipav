package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * The class reads and writes packed bit files for tiff formatted images (including STK).
 */

public class FilePackBit extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Buffer used to store image of type byte. */
    private byte[] bufferByte = null;

    /** Second buffer used to store image of type byte. */
    private byte[] bufferByte2 = null;

    /** Buffer used to store image of type short or unsigned byte. */
    private short[] bufferShort = null;

    /** Size of buffer to be allocated. */
    private int bufferSize = 0;

    /** Temporary buffer. */
    private byte[] litArray = null;

    /** Temporary buffer. */
    private byte[] repArray = null;

    /** Image type ID. */
    private int type = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Packbitconstructor.
     *
     * @param  file  random access file pointer
     */
    public FilePackBit(RandomAccessFile file) {
        setImageFile(file);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {
        bufferByte = null;
        bufferByte2 = null;
        repArray = null;
        litArray = null;
        bufferShort = null;
    }

    /**
     * Gets the byte buffer of the image data.
     *
     * @return  the buffer
     */
    public byte[] getByteBuffer() {
        return bufferByte;
    }

    /**
     * Gets the short buffer of the image.
     *
     * @return  the buffer
     */
    public short[] getShortBuffer() {
        return bufferShort;
    }


    /**
     * This method returns the packed bit strip size.
     *
     * @param   image  image model from which the data will be read.
     * @param   start  start of data in the read image file in units of extents[0]*extents[1]
     * @param   end    end of data in the read image file in units of extents[0]*extents[1] Note that bufferSize is also
     *                 in units of extents[0]*extents[1] This method only applies to BYTE or UBYTE data.
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public int getStripSize(ModelImage image, int start, int end) throws IOException {
        int i, xDim, yDim, ix, iy, repLength, litLength, iStart, iCount, litNum, repNum;
        boolean repRun;

        if (((end - start) != bufferSize) || (image.getType() != this.type)) {

            type = image.getType();
            bufferSize = end - start;

            try {

                switch (image.getType()) {

                    case ModelStorageBase.BYTE:
                        bufferByte = new byte[bufferSize];
                        litArray = new byte[bufferSize];
                        repArray = new byte[bufferSize / 2];
                        break;

                    case ModelStorageBase.UBYTE:
                        bufferShort = new short[bufferSize];
                        bufferByte = new byte[bufferSize];
                        litArray = new byte[bufferSize];
                        repArray = new byte[bufferSize / 2];
                        break;

                    default:
                        throw new IOException();
                }
            } catch (OutOfMemoryError error) {
                bufferByte = null;
                bufferShort = null;
                litArray = null;
                repArray = null;
                System.gc();
                throw error;
            }
        }

        double invSlope = 1, intercept = 0;
        boolean dicom = false;

        switch (image.getType()) {

            case ModelStorageBase.BYTE:
                try {
                    image.exportData(start, bufferSize, bufferByte);

                    byte tmpByte;

                    if (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
                        invSlope = 1 / (image.getFileInfo(0).getRescaleSlope());
                        intercept = image.getFileInfo(0).getRescaleIntercept();
                        dicom = true;
                    }

                    for (i = 0; i < bufferSize; i++) {

                        if (dicom) {
                            tmpByte = (byte) ((bufferByte[i] - intercept) * invSlope);
                        } else {
                            tmpByte = bufferByte[i];
                        }

                        bufferByte[i] = tmpByte;
                    }

                    xDim = image.getFileInfo(0).getExtents()[0];
                    yDim = bufferSize / xDim;
                    iCount = 0;
                    litNum = 0;
                    repNum = 0;

                    for (iy = 0; iy < yDim; iy++) {
                        iStart = iy * xDim;

                        if (bufferByte[iStart] == bufferByte[iStart + 1]) {
                            repRun = true;
                            litLength = 0;
                            repLength = 2;
                        } else {
                            repRun = false;
                            litLength = 2;
                            repLength = 0;
                        }

                        for (ix = 2; ix < xDim;) {

                            if (((repLength < 128) && (litLength < 128)) ||
                                    ((litLength == 128) && (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1]))) {

                                if ((repRun == true) && (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1])) {
                                    repLength++;
                                    ix++;
                                } else if ((repRun == true) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix - 1])) {
                                    iCount = iCount + 2;
                                    repArray[repNum++] = (byte) ((~(repLength - 1)) + 1);
                                    repLength = 0;

                                    if (ix == (xDim - 1)) {
                                        iCount = iCount + 2;
                                        ix++;
                                        litArray[litNum++] = 0;
                                    } else if ((ix == (xDim - 2)) &&
                                                   (bufferByte[iStart + xDim - 2] == bufferByte[iStart + xDim - 1])) {
                                        iCount = iCount + 2;
                                        ix = ix + 2;
                                        repArray[repNum++] = (~(2 - 1)) + 1;
                                    } else if ((ix == (xDim - 2)) &&
                                                   (bufferByte[iStart + xDim - 2] != bufferByte[iStart + xDim - 1])) {
                                        iCount = iCount + 3;
                                        ix = ix + 2;
                                        litArray[litNum++] = (2 - 1);
                                    } else if ((ix < (xDim - 2)) &&
                                                   (bufferByte[iStart + ix] == bufferByte[iStart + ix + 1])) {
                                        repLength = 2;
                                        ix = ix + 2;
                                    } else if ((ix < (xDim - 2)) &&
                                                   (bufferByte[iStart + ix] != bufferByte[iStart + ix + 1])) {
                                        litLength = 2;
                                        repRun = false;
                                        ix = ix + 2;
                                    }
                                } else if ((repRun == false) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix - 1])) {
                                    litLength++;
                                    ix++;
                                } else if ((repRun == false) &&
                                               (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1])) {
                                    litLength--;
                                    iCount = iCount + litLength + 1;
                                    litArray[litNum++] = (byte) (litLength - 1);
                                    repRun = true;
                                    litLength = 0;
                                    repLength = 2;
                                    ix++;
                                }
                            } // end of if (((repLength < 128) && (litLength < 128)) ||
                              // ((litLength == 128) && (bufferByte[iStart+ix] == bufferByte[iStart+ix-1]))) {
                            else {

                                if (repLength == 128) {
                                    iCount = iCount + 2;
                                    repArray[repNum++] = (byte) ((~(repLength - 1)) + 1);
                                    repLength = 0;
                                } else if (litLength == 128) {
                                    iCount = iCount + litLength + 1;
                                    litArray[litNum++] = (byte) (litLength - 1);
                                    litLength = 0;
                                }

                                if (ix == (xDim - 1)) {
                                    iCount = iCount + 2;
                                    ix++;
                                    litArray[litNum++] = 0;
                                } else if ((ix == (xDim - 2)) &&
                                               (bufferByte[iStart + xDim - 2] == bufferByte[iStart + xDim - 1])) {
                                    iCount = iCount + 2;
                                    ix = ix + 2;
                                    repArray[repNum++] = (~(2 - 1)) + 1;
                                } else if ((ix == (xDim - 2)) &&
                                               (bufferByte[iStart + xDim - 2] != bufferByte[iStart + xDim - 1])) {
                                    iCount = iCount + 3;
                                    ix = ix + 2;
                                    litArray[litNum++] = (2 - 1);
                                } else if ((ix < (xDim - 2)) &&
                                               (bufferByte[iStart + ix] == bufferByte[iStart + ix + 1])) {
                                    repRun = true;
                                    repLength = 2;
                                    ix = ix + 2;
                                } else if ((ix < (xDim - 2)) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix + 1])) {
                                    repRun = false;
                                    litLength = 2;
                                    ix = ix + 2;
                                }
                            } // end of else
                        } // end of for (ix = 2; ix < xDim;)

                        if (repLength > 0) {
                            iCount = iCount + 2;
                            repArray[repNum++] = (byte) ((~(repLength - 1)) + 1);
                        } else if (litLength > 0) {
                            iCount = iCount + litLength + 1;
                            litArray[litNum++] = (byte) (litLength - 1);
                        }
                    } // end of for (iy = 0; iy < yDim; iy++)
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.UBYTE:
                try {
                    image.exportData(start, bufferSize, bufferShort);

                    short tmpShort;

                    if (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
                        invSlope = 1 / (image.getFileInfo(0).getRescaleSlope());
                        intercept = image.getFileInfo(0).getRescaleIntercept();
                        dicom = true;
                    }

                    for (i = 0; i < bufferSize; i++) {

                        if (dicom) {
                            tmpShort = (short) ((bufferShort[i] - intercept) * invSlope);
                        } else {
                            tmpShort = bufferShort[i];
                        }

                        bufferByte[i] = (byte) (tmpShort & 0xff);
                    }

                    xDim = image.getFileInfo(0).getExtents()[0];
                    yDim = bufferSize / xDim;
                    iCount = 0;
                    litNum = 0;
                    repNum = 0;

                    for (iy = 0; iy < yDim; iy++) {
                        iStart = iy * xDim;

                        if (bufferByte[iStart] == bufferByte[iStart + 1]) {
                            repRun = true;
                            litLength = 0;
                            repLength = 2;
                        } else {
                            repRun = false;
                            litLength = 2;
                            repLength = 0;
                        }

                        for (ix = 2; ix < xDim;) {

                            if (((repLength < 128) && (litLength < 128)) ||
                                    ((litLength == 128) && (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1]))) {

                                if ((repRun == true) && (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1])) {
                                    repLength++;
                                    ix++;
                                } else if ((repRun == true) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix - 1])) {
                                    iCount = iCount + 2;
                                    repArray[repNum++] = (byte) ((~(repLength - 1)) + 1);
                                    repLength = 0;

                                    if (ix == (xDim - 1)) {
                                        iCount = iCount + 2;
                                        ix++;
                                        litArray[litNum++] = 0;
                                    } else if ((ix == (xDim - 2)) &&
                                                   (bufferByte[iStart + xDim - 2] == bufferByte[iStart + xDim - 1])) {
                                        iCount = iCount + 2;
                                        ix = ix + 2;
                                        repArray[repNum++] = (~(2 - 1)) + 1;
                                    } else if ((ix == (xDim - 2)) &&
                                                   (bufferByte[iStart + xDim - 2] != bufferByte[iStart + xDim - 1])) {
                                        iCount = iCount + 3;
                                        ix = ix + 2;
                                        litArray[litNum++] = (2 - 1);
                                    } else if ((ix < (xDim - 2)) &&
                                                   (bufferByte[iStart + ix] == bufferByte[iStart + ix + 1])) {
                                        repLength = 2;
                                        ix = ix + 2;
                                    } else if ((ix < (xDim - 2)) &&
                                                   (bufferByte[iStart + ix] != bufferByte[iStart + ix + 1])) {
                                        litLength = 2;
                                        repRun = false;
                                        ix = ix + 2;
                                    }
                                } else if ((repRun == false) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix - 1])) {
                                    litLength++;
                                    ix++;
                                } else if ((repRun == false) &&
                                               (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1])) {
                                    litLength--;
                                    iCount = iCount + litLength + 1;
                                    litArray[litNum++] = (byte) (litLength - 1);
                                    repRun = true;
                                    litLength = 0;
                                    repLength = 2;
                                    ix++;
                                }
                            } // end of if (((repLength < 128) && (litLength < 128)) ||
                              // ((litLength == 128) && (bufferByte[iStart+ix] == bufferByte[iStart+ix-1]))) {
                            else {

                                if (repLength == 128) {
                                    iCount = iCount + 2;
                                    repArray[repNum++] = (byte) ((~(repLength - 1)) + 1);
                                    repLength = 0;
                                } else if (litLength == 128) {
                                    iCount = iCount + litLength + 1;
                                    litArray[litNum++] = (byte) (litLength - 1);
                                    litLength = 0;
                                }

                                if (ix == (xDim - 1)) {
                                    iCount = iCount + 2;
                                    ix++;
                                    litArray[litNum++] = 0;
                                } else if ((ix == (xDim - 2)) &&
                                               (bufferByte[iStart + xDim - 2] == bufferByte[iStart + xDim - 1])) {
                                    iCount = iCount + 2;
                                    ix = ix + 2;
                                    repArray[repNum++] = (~(2 - 1)) + 1;
                                } else if ((ix == (xDim - 2)) &&
                                               (bufferByte[iStart + xDim - 2] != bufferByte[iStart + xDim - 1])) {
                                    iCount = iCount + 3;
                                    ix = ix + 2;
                                    litArray[litNum++] = (2 - 1);
                                } else if ((ix < (xDim - 2)) &&
                                               (bufferByte[iStart + ix] == bufferByte[iStart + ix + 1])) {
                                    repRun = true;
                                    repLength = 2;
                                    ix = ix + 2;
                                } else if ((ix < (xDim - 2)) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix + 1])) {
                                    repRun = false;
                                    litLength = 2;
                                    ix = ix + 2;
                                }
                            } // end of else
                        } // end of for (ix = 2; ix < xDim;)

                        if (repLength > 0) {
                            iCount = iCount + 2;
                            repArray[repNum++] = (byte) ((~(repLength - 1)) + 1);
                        } else if (litLength > 0) {
                            iCount = iCount + litLength + 1;
                            litArray[litNum++] = (byte) (litLength - 1);
                        }
                    } // end of for (iy = 0; iy < yDim; iy++)
                } catch (IOException error) {
                    throw error;
                }

                break;

            default:
                throw new IOException();
        }

        return iCount;
    }

    /**
     * Sets the image file.
     *
     * @param  file  random access file pointer
     */
    public void setImageFile(RandomAccessFile file) {
        raFile = file;
    }


    /**
     * This method writes a packed bit image file (1D-5D).
     *
     * @param   image  image model from which the data will be read.
     * @param   start  start of data in the read image file in units of extents[0]*extents[1]
     * @param   end    end of data in the read image file in units of extents[0]*extents[1] Note that bufferSize is also
     *                 in units of extents[0]*extents[1] This method only applies to BYTE or UBYTE data.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void writePackBitImage(ModelImage image, int start, int end) throws IOException {
        int i, xDim, yDim, ix, iy, repLength, litLength, j, k, iStart, iCount, litNum, repNum;
        boolean repRun;

        if (((end - start) != bufferSize) || (image.getType() != this.type)) {

            type = image.getType();
            bufferSize = end - start;

            try {

                switch (image.getType()) {

                    case ModelStorageBase.BYTE:
                        bufferByte = new byte[bufferSize];
                        litArray = new byte[bufferSize];
                        repArray = new byte[bufferSize / 2];
                        break;

                    case ModelStorageBase.UBYTE:
                        bufferShort = new short[bufferSize];
                        bufferByte = new byte[bufferSize];
                        litArray = new byte[bufferSize];
                        repArray = new byte[bufferSize / 2];
                        break;

                    default:
                        throw new IOException();
                }
            } catch (OutOfMemoryError error) {
                bufferByte = null;
                bufferShort = null;
                litArray = null;
                repArray = null;
                System.gc();
                throw error;
            }
        }

        double invSlope = 1, intercept = 0;
        boolean dicom = false;

        switch (image.getType()) {

            case ModelStorageBase.BYTE:
                try {
                    image.exportData(start, bufferSize, bufferByte);

                    byte tmpByte;

                    if (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
                        invSlope = 1 / (image.getFileInfo(0).getRescaleSlope());
                        intercept = image.getFileInfo(0).getRescaleIntercept();
                        dicom = true;
                    }

                    for (i = 0; i < bufferSize; i++) {

                        if (dicom) {
                            tmpByte = (byte) ((bufferByte[i] - intercept) * invSlope);
                        } else {
                            tmpByte = bufferByte[i];
                        }

                        bufferByte[i] = tmpByte;
                    }

                    xDim = image.getFileInfo(0).getExtents()[0];
                    yDim = bufferSize / xDim;
                    iCount = 0;
                    litNum = 0;
                    repNum = 0;

                    for (iy = 0; iy < yDim; iy++) {
                        iStart = iy * xDim;

                        if (bufferByte[iStart] == bufferByte[iStart + 1]) {
                            repRun = true;
                            litLength = 0;
                            repLength = 2;
                        } else {
                            repRun = false;
                            litLength = 2;
                            repLength = 0;
                        }

                        for (ix = 2; ix < xDim;) {

                            if (((repLength < 128) && (litLength < 128)) ||
                                    ((litLength == 128) && (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1]))) {

                                if ((repRun == true) && (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1])) {
                                    repLength++;
                                    ix++;
                                } else if ((repRun == true) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix - 1])) {
                                    iCount = iCount + 2;
                                    repArray[repNum++] = (byte) ((~(repLength - 1)) + 1);
                                    repLength = 0;

                                    if (ix == (xDim - 1)) {
                                        iCount = iCount + 2;
                                        ix++;
                                        litArray[litNum++] = 0;
                                    } else if ((ix == (xDim - 2)) &&
                                                   (bufferByte[iStart + xDim - 2] == bufferByte[iStart + xDim - 1])) {
                                        iCount = iCount + 2;
                                        ix = ix + 2;
                                        repArray[repNum++] = (~(2 - 1)) + 1;
                                    } else if ((ix == (xDim - 2)) &&
                                                   (bufferByte[iStart + xDim - 2] != bufferByte[iStart + xDim - 1])) {
                                        iCount = iCount + 3;
                                        ix = ix + 2;
                                        litArray[litNum++] = (2 - 1);
                                    } else if ((ix < (xDim - 2)) &&
                                                   (bufferByte[iStart + ix] == bufferByte[iStart + ix + 1])) {
                                        repLength = 2;
                                        ix = ix + 2;
                                    } else if ((ix < (xDim - 2)) &&
                                                   (bufferByte[iStart + ix] != bufferByte[iStart + ix + 1])) {
                                        litLength = 2;
                                        repRun = false;
                                        ix = ix + 2;
                                    }
                                } else if ((repRun == false) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix - 1])) {
                                    litLength++;
                                    ix++;
                                } else if ((repRun == false) &&
                                               (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1])) {
                                    litLength--;
                                    iCount = iCount + litLength + 1;
                                    litArray[litNum++] = (byte) (litLength - 1);
                                    repRun = true;
                                    litLength = 0;
                                    repLength = 2;
                                    ix++;
                                }
                            } // end of if (((repLength < 128) && (litLength < 128)) ||
                              // ((litLength == 128) && (bufferByte[iStart+ix] == bufferByte[iStart+ix-1]))) {
                            else {

                                if (repLength == 128) {
                                    iCount = iCount + 2;
                                    repArray[repNum++] = (byte) ((~(repLength - 1)) + 1);
                                    repLength = 0;
                                } else if (litLength == 128) {
                                    iCount = iCount + litLength + 1;
                                    litArray[litNum++] = (byte) (litLength - 1);
                                    litLength = 0;
                                }

                                if (ix == (xDim - 1)) {
                                    iCount = iCount + 2;
                                    ix++;
                                    litArray[litNum++] = 0;
                                } else if ((ix == (xDim - 2)) &&
                                               (bufferByte[iStart + xDim - 2] == bufferByte[iStart + xDim - 1])) {
                                    iCount = iCount + 2;
                                    ix = ix + 2;
                                    repArray[repNum++] = (~(2 - 1)) + 1;
                                } else if ((ix == (xDim - 2)) &&
                                               (bufferByte[iStart + xDim - 2] != bufferByte[iStart + xDim - 1])) {
                                    iCount = iCount + 3;
                                    ix = ix + 2;
                                    litArray[litNum++] = (2 - 1);
                                } else if ((ix < (xDim - 2)) &&
                                               (bufferByte[iStart + ix] == bufferByte[iStart + ix + 1])) {
                                    repRun = true;
                                    repLength = 2;
                                    ix = ix + 2;
                                } else if ((ix < (xDim - 2)) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix + 1])) {
                                    repRun = false;
                                    litLength = 2;
                                    ix = ix + 2;
                                }
                            } // end of else
                        } // end of for (ix = 2; ix < xDim;)

                        if (repLength > 0) {
                            iCount = iCount + 2;
                            repArray[repNum++] = (byte) ((~(repLength - 1)) + 1);
                        } else if (litLength > 0) {
                            iCount = iCount + litLength + 1;
                            litArray[litNum++] = (byte) (litLength - 1);
                        }
                    } // end of for (iy = 0; iy < yDim; iy++)

                    bufferByte2 = new byte[iCount];
                    litNum = 0;
                    repNum = 0;
                    iCount = 0;

                    for (iy = 0; iy < yDim; iy++) {
                        iStart = iy * xDim;

                        if (bufferByte[iStart] == bufferByte[iStart + 1]) {
                            repRun = true;
                            litLength = 0;
                            repLength = 2;
                        } else {
                            repRun = false;
                            litLength = 2;
                            repLength = 0;
                        }

                        for (ix = 2; ix < xDim;) {

                            if (((repLength < 128) && (litLength < 128)) ||
                                    ((litLength == 128) && (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1]))) {

                                if ((repRun == true) && (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1])) {
                                    repLength++;
                                    ix++;
                                } else if ((repRun == true) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix - 1])) {
                                    bufferByte2[iCount] = repArray[repNum++];
                                    bufferByte2[iCount + 1] = bufferByte[iStart + ix - 1];
                                    iCount = iCount + 2;
                                    repLength = 0;

                                    if (ix == (xDim - 1)) {
                                        bufferByte2[iCount] = litArray[litNum++];
                                        bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 1];
                                        iCount = iCount + 2;
                                        ix++;
                                    } else if ((ix == (xDim - 2)) &&
                                                   (bufferByte[iStart + xDim - 2] == bufferByte[iStart + xDim - 1])) {
                                        bufferByte2[iCount] = repArray[repNum++];
                                        bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 1];
                                        iCount = iCount + 2;
                                        ix = ix + 2;
                                    } else if ((ix == (xDim - 2)) &&
                                                   (bufferByte[iStart + xDim - 2] != bufferByte[iStart + xDim - 1])) {
                                        bufferByte2[iCount] = litArray[litNum++];
                                        bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 2];
                                        bufferByte2[iCount + 2] = bufferByte[iStart + xDim - 1];
                                        iCount = iCount + 3;
                                        ix = ix + 2;
                                    } else if ((ix < (xDim - 2)) &&
                                                   (bufferByte[iStart + ix] == bufferByte[iStart + ix + 1])) {
                                        repLength = 2;
                                        ix = ix + 2;
                                    } else if ((ix < (xDim - 2)) &&
                                                   (bufferByte[iStart + ix] != bufferByte[iStart + ix + 1])) {
                                        litLength = 2;
                                        repRun = false;
                                        ix = ix + 2;
                                    }
                                } else if ((repRun == false) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix - 1])) {
                                    litLength++;
                                    ix++;
                                } else if ((repRun == false) &&
                                               (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1])) {
                                    litLength--;
                                    j = (iStart + ix - 2) - (litArray[litNum] + 1) + 1;
                                    bufferByte2[iCount] = litArray[litNum++];

                                    for (k = 1; j <= (iStart + ix - 2); j++, k++) {
                                        bufferByte2[iCount + k] = bufferByte[j];
                                    }

                                    iCount = iCount + litLength + 1;
                                    repRun = true;
                                    litLength = 0;
                                    repLength = 2;
                                    ix++;
                                }
                            } // end of if (((repLength < 128) && (litLength < 128)) ||
                              // ((litLength == 128) && (bufferByte[iStart+ix] == bufferByte[iStart+ix-1]))) {
                            else {

                                if (repLength == 128) {
                                    bufferByte2[iCount] = repArray[repNum++];
                                    bufferByte2[iCount + 1] = bufferByte[iStart + ix - 1];
                                    iCount = iCount + 2;
                                    repLength = 0;
                                } else if (litLength == 128) {
                                    j = (iStart + ix - 1) - (litArray[litNum] + 1) + 1;
                                    bufferByte2[iCount] = litArray[litNum++];

                                    for (k = 1; j <= (iStart + ix - 1); j++, k++) {
                                        bufferByte2[iCount + k] = bufferByte[j];
                                    }

                                    iCount = iCount + litLength + 1;
                                    litLength = 0;
                                }

                                if (ix == (xDim - 1)) {
                                    bufferByte2[iCount] = litArray[litNum++];
                                    bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 1];
                                    iCount = iCount + 2;
                                    ix++;
                                } else if ((ix == (xDim - 2)) &&
                                               (bufferByte[iStart + xDim - 2] == bufferByte[iStart + xDim - 1])) {
                                    bufferByte2[iCount] = repArray[repNum++];
                                    bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 1];
                                    iCount = iCount + 2;
                                    ix = ix + 2;
                                } else if ((ix == (xDim - 2)) &&
                                               (bufferByte[iStart + xDim - 2] != bufferByte[iStart + xDim - 1])) {
                                    bufferByte2[iCount] = litArray[litNum++];
                                    bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 2];
                                    bufferByte2[iCount + 2] = bufferByte[iStart + xDim - 1];
                                    iCount = iCount + 3;
                                    ix = ix + 2;
                                } else if ((ix < (xDim - 2)) &&
                                               (bufferByte[iStart + ix] == bufferByte[iStart + ix + 1])) {
                                    repRun = true;
                                    repLength = 2;
                                    ix = ix + 2;
                                } else if ((ix < (xDim - 2)) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix + 1])) {
                                    repRun = false;
                                    litLength = 2;
                                    ix = ix + 2;
                                }
                            } // end of else
                        } // end of for (ix = 2; ix < xDim;)

                        if (repLength > 0) {
                            bufferByte2[iCount] = repArray[repNum++];
                            bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 1];
                            iCount = iCount + 2;
                        } else if (litLength > 0) {
                            j = (iStart + xDim - 1) - (litArray[litNum] + 1) + 1;
                            bufferByte2[iCount] = litArray[litNum++];

                            for (k = 1; j <= (iStart + xDim - 1); j++, k++) {
                                bufferByte2[iCount + k] = bufferByte[j];
                            }

                            iCount = iCount + litLength + 1;
                        }
                    } // end of for (iy = 0; iy < yDim; iy++)

                    raFile.write(bufferByte2);
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.UBYTE:
                try {
                    image.exportData(start, bufferSize, bufferShort);

                    short tmpShort;

                    if (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
                        invSlope = 1 / (image.getFileInfo(0).getRescaleSlope());
                        intercept = image.getFileInfo(0).getRescaleIntercept();
                        dicom = true;
                    }

                    for (i = 0; i < bufferSize; i++) {

                        if (dicom) {
                            tmpShort = (short) ((bufferShort[i] - intercept) * invSlope);
                        } else {
                            tmpShort = bufferShort[i];
                        }

                        bufferByte[i] = (byte) (tmpShort & 0xff);
                    }

                    xDim = image.getFileInfo(0).getExtents()[0];
                    yDim = bufferSize / xDim;
                    iCount = 0;
                    litNum = 0;
                    repNum = 0;

                    for (iy = 0; iy < yDim; iy++) {
                        iStart = iy * xDim;

                        if (bufferByte[iStart] == bufferByte[iStart + 1]) {
                            repRun = true;
                            litLength = 0;
                            repLength = 2;
                        } else {
                            repRun = false;
                            litLength = 2;
                            repLength = 0;
                        }

                        for (ix = 2; ix < xDim;) {

                            if (((repLength < 128) && (litLength < 128)) ||
                                    ((litLength == 128) && (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1]))) {

                                if ((repRun == true) && (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1])) {
                                    repLength++;
                                    ix++;
                                } else if ((repRun == true) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix - 1])) {
                                    iCount = iCount + 2;
                                    repArray[repNum++] = (byte) ((~(repLength - 1)) + 1);
                                    repLength = 0;

                                    if (ix == (xDim - 1)) {
                                        iCount = iCount + 2;
                                        ix++;
                                        litArray[litNum++] = 0;
                                    } else if ((ix == (xDim - 2)) &&
                                                   (bufferByte[iStart + xDim - 2] == bufferByte[iStart + xDim - 1])) {
                                        iCount = iCount + 2;
                                        ix = ix + 2;
                                        repArray[repNum++] = (~(2 - 1)) + 1;
                                    } else if ((ix == (xDim - 2)) &&
                                                   (bufferByte[iStart + xDim - 2] != bufferByte[iStart + xDim - 1])) {
                                        iCount = iCount + 3;
                                        ix = ix + 2;
                                        litArray[litNum++] = (2 - 1);
                                    } else if ((ix < (xDim - 2)) &&
                                                   (bufferByte[iStart + ix] == bufferByte[iStart + ix + 1])) {
                                        repLength = 2;
                                        ix = ix + 2;
                                    } else if ((ix < (xDim - 2)) &&
                                                   (bufferByte[iStart + ix] != bufferByte[iStart + ix + 1])) {
                                        litLength = 2;
                                        repRun = false;
                                        ix = ix + 2;
                                    }
                                } else if ((repRun == false) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix - 1])) {
                                    litLength++;
                                    ix++;
                                } else if ((repRun == false) &&
                                               (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1])) {
                                    litLength--;
                                    iCount = iCount + litLength + 1;
                                    litArray[litNum++] = (byte) (litLength - 1);
                                    repRun = true;
                                    litLength = 0;
                                    repLength = 2;
                                    ix++;
                                }
                            } // end of if (((repLength < 128) && (litLength < 128)) ||
                              // ((litLength == 128) && (bufferByte[iStart+ix] == bufferByte[iStart+ix-1]))) {
                            else {

                                if (repLength == 128) {
                                    iCount = iCount + 2;
                                    repArray[repNum++] = (byte) ((~(repLength - 1)) + 1);
                                    repLength = 0;
                                } else if (litLength == 128) {
                                    iCount = iCount + litLength + 1;
                                    litArray[litNum++] = (byte) (litLength - 1);
                                    litLength = 0;
                                }

                                if (ix == (xDim - 1)) {
                                    iCount = iCount + 2;
                                    ix++;
                                    litArray[litNum++] = 0;
                                } else if ((ix == (xDim - 2)) &&
                                               (bufferByte[iStart + xDim - 2] == bufferByte[iStart + xDim - 1])) {
                                    iCount = iCount + 2;
                                    ix = ix + 2;
                                    repArray[repNum++] = (~(2 - 1)) + 1;
                                } else if ((ix == (xDim - 2)) &&
                                               (bufferByte[iStart + xDim - 2] != bufferByte[iStart + xDim - 1])) {
                                    iCount = iCount + 3;
                                    ix = ix + 2;
                                    litArray[litNum++] = (2 - 1);
                                } else if ((ix < (xDim - 2)) &&
                                               (bufferByte[iStart + ix] == bufferByte[iStart + ix + 1])) {
                                    repRun = true;
                                    repLength = 2;
                                    ix = ix + 2;
                                } else if ((ix < (xDim - 2)) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix + 1])) {
                                    repRun = false;
                                    litLength = 2;
                                    ix = ix + 2;
                                }
                            } // end of else
                        } // end of for (ix = 2; ix < xDim;)

                        if (repLength > 0) {
                            iCount = iCount + 2;
                            repArray[repNum++] = (byte) ((~(repLength - 1)) + 1);
                        } else if (litLength > 0) {
                            iCount = iCount + litLength + 1;
                            litArray[litNum++] = (byte) (litLength - 1);
                        }
                    } // end of for (iy = 0; iy < yDim; iy++)

                    bufferByte2 = new byte[iCount];
                    litNum = 0;
                    repNum = 0;
                    iCount = 0;

                    for (iy = 0; iy < yDim; iy++) {
                        iStart = iy * xDim;

                        if (bufferByte[iStart] == bufferByte[iStart + 1]) {
                            repRun = true;
                            litLength = 0;
                            repLength = 2;
                        } else {
                            repRun = false;
                            litLength = 2;
                            repLength = 0;
                        }

                        for (ix = 2; ix < xDim;) {

                            if (((repLength < 128) && (litLength < 128)) ||
                                    ((litLength == 128) && (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1]))) {

                                if ((repRun == true) && (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1])) {
                                    repLength++;
                                    ix++;
                                } else if ((repRun == true) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix - 1])) {
                                    bufferByte2[iCount] = repArray[repNum++];
                                    bufferByte2[iCount + 1] = bufferByte[iStart + ix - 1];
                                    iCount = iCount + 2;
                                    repLength = 0;

                                    if (ix == (xDim - 1)) {
                                        bufferByte2[iCount] = litArray[litNum++];
                                        bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 1];
                                        iCount = iCount + 2;
                                        ix++;
                                    } else if ((ix == (xDim - 2)) &&
                                                   (bufferByte[iStart + xDim - 2] == bufferByte[iStart + xDim - 1])) {
                                        bufferByte2[iCount] = repArray[repNum++];
                                        bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 1];
                                        iCount = iCount + 2;
                                        ix = ix + 2;
                                    } else if ((ix == (xDim - 2)) &&
                                                   (bufferByte[iStart + xDim - 2] != bufferByte[iStart + xDim - 1])) {
                                        bufferByte2[iCount] = litArray[litNum++];
                                        bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 2];
                                        bufferByte2[iCount + 2] = bufferByte[iStart + xDim - 1];
                                        iCount = iCount + 3;
                                        ix = ix + 2;
                                    } else if ((ix < (xDim - 2)) &&
                                                   (bufferByte[iStart + ix] == bufferByte[iStart + ix + 1])) {
                                        repLength = 2;
                                        ix = ix + 2;
                                    } else if ((ix < (xDim - 2)) &&
                                                   (bufferByte[iStart + ix] != bufferByte[iStart + ix + 1])) {
                                        litLength = 2;
                                        repRun = false;
                                        ix = ix + 2;
                                    }
                                } else if ((repRun == false) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix - 1])) {
                                    litLength++;
                                    ix++;
                                } else if ((repRun == false) &&
                                               (bufferByte[iStart + ix] == bufferByte[iStart + ix - 1])) {
                                    litLength--;
                                    j = (iStart + ix - 2) - (litArray[litNum] + 1) + 1;
                                    bufferByte2[iCount] = litArray[litNum++];

                                    for (k = 1; j <= (iStart + ix - 2); j++, k++) {
                                        bufferByte2[iCount + k] = bufferByte[j];
                                    }

                                    iCount = iCount + litLength + 1;
                                    repRun = true;
                                    litLength = 0;
                                    repLength = 2;
                                    ix++;
                                }
                            } // end of if (((repLength < 128) && (litLength < 128)) ||
                              // ((litLength == 128) && (bufferByte[iStart+ix] == bufferByte[iStart+ix-1]))) {
                            else {

                                if (repLength == 128) {
                                    bufferByte2[iCount] = repArray[repNum++];
                                    bufferByte2[iCount + 1] = bufferByte[iStart + ix - 1];
                                    iCount = iCount + 2;
                                    repLength = 0;
                                } else if (litLength == 128) {
                                    j = (iStart + ix - 1) - (litArray[litNum] + 1) + 1;
                                    bufferByte2[iCount] = litArray[litNum++];

                                    for (k = 1; j <= (iStart + ix - 1); j++, k++) {
                                        bufferByte2[iCount + k] = bufferByte[j];
                                    }

                                    iCount = iCount + litLength + 1;
                                    litLength = 0;
                                }

                                if (ix == (xDim - 1)) {
                                    bufferByte2[iCount] = litArray[litNum++];
                                    bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 1];
                                    iCount = iCount + 2;
                                    ix++;
                                } else if ((ix == (xDim - 2)) &&
                                               (bufferByte[iStart + xDim - 2] == bufferByte[iStart + xDim - 1])) {
                                    bufferByte2[iCount] = repArray[repNum++];
                                    bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 1];
                                    iCount = iCount + 2;
                                    ix = ix + 2;
                                } else if ((ix == (xDim - 2)) &&
                                               (bufferByte[iStart + xDim - 2] != bufferByte[iStart + xDim - 1])) {
                                    bufferByte2[iCount] = litArray[litNum++];
                                    bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 2];
                                    bufferByte2[iCount + 2] = bufferByte[iStart + xDim - 1];
                                    iCount = iCount + 3;
                                    ix = ix + 2;
                                } else if ((ix < (xDim - 2)) &&
                                               (bufferByte[iStart + ix] == bufferByte[iStart + ix + 1])) {
                                    repRun = true;
                                    repLength = 2;
                                    ix = ix + 2;
                                } else if ((ix < (xDim - 2)) &&
                                               (bufferByte[iStart + ix] != bufferByte[iStart + ix + 1])) {
                                    repRun = false;
                                    litLength = 2;
                                    ix = ix + 2;
                                }
                            } // end of else
                        } // end of for (ix = 2; ix < xDim;)

                        if (repLength > 0) {
                            bufferByte2[iCount] = repArray[repNum++];
                            bufferByte2[iCount + 1] = bufferByte[iStart + xDim - 1];
                            iCount = iCount + 2;
                        } else if (litLength > 0) {
                            j = (iStart + xDim - 1) - (litArray[litNum] + 1) + 1;
                            bufferByte2[iCount] = litArray[litNum++];

                            for (k = 1; j <= (iStart + xDim - 1); j++, k++) {
                                bufferByte2[iCount + k] = bufferByte[j];
                            }

                            iCount = iCount + litLength + 1;
                        }
                    } // end of for (iy = 0; iy < yDim; iy++)

                    raFile.write(bufferByte2);
                } catch (IOException error) {
                    throw error;
                }

                break;

            default:
                throw new IOException();
        }
    }
}
