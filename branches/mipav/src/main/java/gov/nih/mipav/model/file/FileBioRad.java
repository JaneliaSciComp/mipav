package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

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
    @SuppressWarnings("unused")
    private int color1;

    /** LUT2 color status. */
    @SuppressWarnings("unused")
    private int color2;

    /** DOCUMENT ME! */
    private int dataType;

    /** not used in disk files. */
    @SuppressWarnings("unused")
    private short edited;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** always set to 12345 in BioRad pic files. */
    private int fileID;

    /** DOCUMENT ME! */
    private FileInfoBioRad fileInfo;
    
    private FileInfoBioRad fileInfoCopy;

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

    @SuppressWarnings("unused")
    private short ramp1Max; // LUT1 ramp max

    @SuppressWarnings("unused")
    private short ramp1Min; // LUT1 ramp min

    /** LUT2 ramp max. */
    @SuppressWarnings("unused")
    private short ramp2Max;

    /** LUT2 ramp min. */
    @SuppressWarnings("unused")
    private short ramp2Min;

    /** DOCUMENT ME! */
    private boolean readAgain;

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
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileBioRad(String fileName, String fileDir) throws IOException {
        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        byteBuffer = null;
        fileName = null;
        fileDir = null;
        fileInfo = null;
        fileInfoCopy = null;
        file = null;
        fName = null;
        image = null;
        imgBuffer = null;
        imgExtents = null;
        name = null;
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
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

        int i;
        @SuppressWarnings("unused")
        int num;
        boolean moreNotes;
        int noteNumber;
        int displayLevel;
        int lastNote;
        int noteType;
        byte buffer[];
        String noteString;

        boolean endianess = LITTLE_ENDIAN;

        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");

            xDim = getSignedShort(endianess); // 0
            Preferences.debug("\nxDim = " + xDim + "\n", Preferences.DEBUG_FILEIO);
            yDim = getSignedShort(endianess); // 2
            Preferences.debug("yDim = " + yDim + "\n", Preferences.DEBUG_FILEIO);
            zDim = getSignedShort(endianess); // 4
            Preferences.debug("zDim = " + zDim + "\n", Preferences.DEBUG_FILEIO);
            ramp1Min = (short) getSignedShort(endianess); // 6
            ramp1Max = (short) getSignedShort(endianess); // 8
            notes = getInt(endianess); // 10
            if (notes == 0) {
                Preferences.debug("No notes present in this file\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("This file has notes after the image data\n", Preferences.DEBUG_FILEIO);
            }
            byteFormat = (short) getSignedShort(endianess); // 14
            if (byteFormat == 1) {
                Preferences.debug("byteFormat = 1 indicates 1 byte data\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("byteFormat = " + byteFormat + " indicates 2 byte data\n", Preferences.DEBUG_FILEIO);
            }
            
            // num only used in COMOS/SOM when the file is loaded into memory
            num = (short) getSignedShort(endianess); // 16


            for (i = 0; i < 32; i++) {
                name[i] = 0;
            }

            readAgain = true;
            i = 0;

            while (readAgain && (i < 32)) {
                name[i++] = raFile.readByte(); // 18

                if (name[i - 1] == 0) {
                    readAgain = false;
                }
            }

            fName = new String(name, 0, i - 1);
            Preferences.debug("Name of the file = " + fName + "\n", Preferences.DEBUG_FILEIO);
            raFile.seek(50);
            merged = (short) getSignedShort(endianess); // 50
            switch (merged) {
                case 0:
                    Preferences.debug("Merge off\n", Preferences.DEBUG_FILEIO);
                    break;
                case 1:
                    Preferences.debug("4-bit merge\n", Preferences.DEBUG_FILEIO);
                    break;
                case 2:
                    Preferences.debug("Alternate 8-bit merge\n", Preferences.DEBUG_FILEIO);
                    break;
                case 3:
                    Preferences.debug("Alternate columns merge\n", Preferences.DEBUG_FILEIO);
                    break;
                case 4:
                    Preferences.debug("Alternate rows merge\n", Preferences.DEBUG_FILEIO);
                    break;
                case 5:
                    Preferences.debug("Maximum pixel intensity merge\n", Preferences.DEBUG_FILEIO);
                    break;
                case 6:
                    Preferences.debug("256 color optimized merge with RGB LUT\n" +
                            "saved at the end of each merge\n", Preferences.DEBUG_FILEIO);
                    break;
                case 7:
                    Preferences.debug("256 color optimized merge with RGB LUT\n" +
                            "saved after all the notes\n", Preferences.DEBUG_FILEIO);
                    break;
                default:
                    Preferences.debug("Merged has unrecognized value = " + merged + "\n", Preferences.DEBUG_FILEIO);    
            }
            color1 = getUnsignedShort(endianess); // 52
            fileID = getUnsignedShort(endianess); // 54

            if (fileID != 12345) {
                throw new IOException("fileID is an illegal " + fileID);
            }
            else {
                Preferences.debug("file ID has legal value = " + fileID + "\n", Preferences.DEBUG_FILEIO);
            }

            ramp2Min = (short) getSignedShort(endianess); // 56
            ramp2Max = (short) getSignedShort(endianess); // 58
            color2 = getUnsignedShort(endianess); // 60
            // edited not used in disk files
            edited = (short) getSignedShort(endianess); // 62
            lens = (short) getSignedShort(endianess); // 64
            Preferences.debug("Integer part of objective lens used = " + lens + "\n", Preferences.DEBUG_FILEIO);
            magFactor = getFloat(endianess); // 66
            Preferences.debug("Magnification factor = " + magFactor + "\n", Preferences.DEBUG_FILEIO);

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
                image = new ModelImage(dataType, new int[] { imgExtents[0], imgExtents[1] }, fileInfo.getFileName());
                zDim = 1;
            } else {
                image = new ModelImage(dataType, imgExtents, fileInfo.getFileName());
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

                    fileInfoCopy = (FileInfoBioRad)fileInfo.clone();
                    image.setFileInfo(fileInfoCopy, i);
                } catch (IOException error) {
                    throw new IOException("FileTiff: read: " + error);
                }

                image.importData(i * bufferSize, imgBuffer, false);
            } // for (i = 0; i < imageSlice; i++)
            
            // Read notes if notes != 0
            if (notes != 0) {
                moreNotes = true;
                noteNumber = 1;
                while (moreNotes) {
                    buffer = new byte[80];
                    Preferences.debug("Note = " + noteNumber + "\n", Preferences.DEBUG_FILEIO);
                    displayLevel = getSignedShort(endianess);
                    Preferences.debug("Display level = " + displayLevel + "\n", Preferences.DEBUG_FILEIO);
                    lastNote = getInt(endianess);
                    if (lastNote == 0) {
                        moreNotes = false;
                        Preferences.debug("This is the last note\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("Another note follows this note\n", Preferences.DEBUG_FILEIO);
                    }
                    getInt(endianess);
                    noteType = getSignedShort(endianess);
                    switch(noteType) {
                        case 1:
                            Preferences.debug("Live collection note\n", Preferences.DEBUG_FILEIO);
                            break;
                        case 2:
                            Preferences.debug("Note includes file name\n", Preferences.DEBUG_FILEIO);
                            break;
                        case 3:
                            Preferences.debug("Note for multiplier file\n", Preferences.DEBUG_FILEIO);
                            break;
                        default:
                            Preferences.debug("Note type = " + noteType + " for additional descriptive note\n", 
                            		Preferences.DEBUG_FILEIO);
                    }
                    getInt(endianess);
                    raFile.read(buffer);
                    noteString = new String(buffer);
                    noteString = noteString.trim();
                    Preferences.debug("Note text = " + noteString + "\n", Preferences.DEBUG_FILEIO);
                    noteNumber++;
                } // while (moreNotes)
            } // if (notes != 0)

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
