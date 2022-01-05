package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;


/**
 * Addresses and lengths are always contained in 4 bytes. The first 4 bytes plus 4 give the location of ascii text
 * saying UserWndLevels, overlays, or sticks if overlays are present. The same region of interest in the same slice may
 * be saved with exactly the same contour using 2 different bounding boxes. The first 90 bytes are header bytes.
 * Location 0x24 may give a location to the start of information for a given slice. The first 4 bytes of this slice
 * information will give the location of the start of information for another slice. This will continue until the first
 * 4 bytes of a slice contains 0x28. 0x28 contains 0. If 0x24 does not point to the start of slice information, then it
 * contains 0x28. However, the code collecting slice information from 0x24 was commented out since it produced a contour
 * in slice 15 in 10080-2-dwi-bo-sw.oly that was not seen in Cheshire. With UserWndLevels 0x8C gives the start of a
 * slice of information and the first 4 bytes of the information slice give the start of another slice until finally 1
 * slice starts with 0. With overlays and sticks an information slice starts 24 bytes after overlays. This 1 information
 * slice was different from other information slices in that the first 8 bytes had the locations of 2 information slices
 * instead of just 4 bytes for 1 location like the other information slices. Also note that information slices may be
 * fragmentary - they may not contain all the information promised by length bytes. In these cases just ignore the
 * fragments of the information slices.
 */

public class FileCheshireVOI extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int USERWNDLEVELS = 1;

    /** DOCUMENT ME! */
    public static final int OVERLAYS = 2;

    /** DOCUMENT ME! */
    public static final int STICKS = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    ViewJProgressBar progressBar;

    /** DOCUMENT ME! */
    private VOI[] addedVOI = new VOI[1000];

    /** DOCUMENT ME! */
    private Polygon contourPolygon;

    /** DOCUMENT ME! */
    private short[] expImgBuffer;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private int fileType; // depends on ascii read at location determined by first 4 bytes

    /** DOCUMENT ME! */
    private short[] grayScaleArray = new short[1000];

    /** DOCUMENT ME! */
    private short grayScaleNumber = 0;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private short[] imgBuffer;

    /** DOCUMENT ME! */
    private int length; // xDimE*yDimE

    /** DOCUMENT ME! */
    private BitSet mask = null;

    /** DOCUMENT ME! */
    private BitSet mask2 = null;

    /** DOCUMENT ME! */
    private BitSet maskAll = null;

    /** DOCUMENT ME! */
    private BitSet maskE = null;

    /** DOCUMENT ME! */
    private BitSet maskN = null;

    /** DOCUMENT ME! */
    private BitSet maskS = null;

    /** DOCUMENT ME! */
    private BitSet maskW = null;

    /** DOCUMENT ME! */
    private int neighbors;

    /** DOCUMENT ME! */
    private boolean newGrayScale; // true if gray scale value was not previously found

    /** DOCUMENT ME! */
    private int scanPos; // x + y*xDimE

    /** DOCUMENT ME! */
    private int sliceSize;

    /** DOCUMENT ME! */
    private int smallPos;

    /** DOCUMENT ME! */
    private int smallX;

    /** DOCUMENT ME! */
    private int smallY;

    /** DOCUMENT ME! */
    private int testPos; // x + y*xDim

    /** DOCUMENT ME! */
    private int totalSize;

    /** DOCUMENT ME! */
    private int xDim, yDim, zDim;

    /** DOCUMENT ME! */
    private int xDimE;

    /** DOCUMENT ME! */
    private int xt, yt, it;

    /** DOCUMENT ME! */
    private int yDimE;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileCheshireVOI - VOI reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     * @param      image     image model: needed during the read process to ensure the VOI "fits" in the image space.
     *
     * @exception  IOException  if there is an error making the files
     */
    public FileCheshireVOI(String fileName, String fileDir, ModelImage image) throws IOException {

        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "r");
        this.fileName = fileName;
        this.image = image;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * readVOI - reads an Cheshire overlay file by reading the overlay information for each different VOI into a
     * different bitmap.
     *
     * @return     DOCUMENT ME!
     *
     * @exception  IOException       if there is an error reading the file
     * @throws     OutOfMemoryError  DOCUMENT ME!
     */
    public VOI[] readVOI() throws IOException, OutOfMemoryError {
        int ii, jj, kk, mm;
        int xx, yy, zz;
        boolean endianess;
        int numberSlices;
        long nextSliceAddress;
        long[] sliceAddress;
        int[] sliceOffset;
        long userTypeAddress; // location with ascii telling type of file
        long fileLength; // legnth of file
        int slice; // slice number starting at 0
        int totalROIl; // length of bytes for all ROIs in the slice
        int ROINumber; // number of ROIs in the slice
        int ROIl; // length of ROI bytes
        long currentLocation;
        long nextROIAddress;
        @SuppressWarnings("unused")
        int field; // 4 bytes that are always 3 or 2
        int upperLeftY, upperLeftX, lowerRightY, lowerRightX;
        int field2; // 4 bytes that are always 2
        int VOINumber; // VOI number starting at 1
        byte valueByte;
        int valueInt;
        long upperLeftYLocation;
        long bitMapLocation;
        int bitMapl; // length of bit map, the bit map starts 4 bytes after this field
        int rowBytes; // length of row in bytes, 2 for 1-16, 4 for 17-32, 6 for 33-48, etc.
        int totalBytesRead;
        int rowBytesRead;
        int rowsRead;
        int bytesToRead;
        VOI[] voi = null;
        byte[] maskBuffer;
        int offset;
        int position;
        int maskValue;
        boolean notFound;
        boolean checkOffset;
        boolean doSlice;
        boolean readBytes;

        // MS<sp>Sans<sp>Serif
        byte[] matchByte = { 0x4D, 0x53, 0x20, 0x53, 0x61, 0x6E, 0x73, 0x20, 0x53, 0x65, 0x72, 0x69, 0x66 };
        byte[] matchUserWndLevels = { 0x55, 0x73, 0x65, 0x72, 0x57, 0x6E, 0x64, 0x4C, 0x65, 0x76, 0x65, 0x6C, 0x73 };
        byte[] matchoverlays = { 0x6F, 0x76, 0x65, 0x72, 0x6C, 0x61, 0x79, 0x73 };
        byte[] matchsticks = { 0x73, 0x74, 0x69, 0x63, 0x6B, 0x73 };

        progressBar = new ViewJProgressBar("Creating VOIs", "Reading " + fileName, 0, 100, true, null, null);
        progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50);


        fireProgressStateChanged(0);

        xDim = image.getExtents()[0];
        yDim = image.getExtents()[1];
        sliceSize = xDim * yDim;

        if (image.getNDims() == 2) {
            zDim = 1;
        } else {
            zDim = image.getExtents()[2];
        }

        totalSize = sliceSize * zDim;
        maskBuffer = new byte[totalSize];

        for (ii = 0; ii < totalSize; ii++) {
            maskBuffer[ii] = 0;
        }

        endianess = FileBase.BIG_ENDIAN;
        fileLength = raFile.length();

        sliceAddress = new long[1000];
        sliceOffset = new int[1000];

        for (jj = 0; jj < 1000; jj++) {
            sliceOffset[jj] = 0;
        }

        numberSlices = 0;

        userTypeAddress = getInt(endianess) + 4; // gives location with ascii text telling type of
                                                 // file
        raFile.seek(userTypeAddress);
        valueByte = raFile.readByte();

        if (valueByte == 0x55) { // Check for UserWndLevels

            for (jj = 1; jj <= (matchUserWndLevels.length - 1); jj++) {
                valueByte = raFile.readByte();

                if (valueByte != matchUserWndLevels[jj]) {
                    MipavUtil.displayError("FileCheshireVOI: Expected serWndLevels not found following U");

                    raFile.close();
                    progressBar.setVisible(false);

                    return null;
                }
            } // for (jj = 1; jj <= (matchUserWndLevels.length - 1); jj++)

            fileType = USERWNDLEVELS;
            raFile.seek(0x8CL);
            nextSliceAddress = getInt(endianess);
        } // if (valueByte == 0x55)
        else if (valueByte == 0x6F) { // Check for overlays

            for (jj = 1; jj <= (matchoverlays.length - 1); jj++) {
                valueByte = raFile.readByte();

                if (valueByte != matchoverlays[jj]) {
                    MipavUtil.displayError("FileCheshireVOI: Expected verlays not found following o");

                    raFile.close();
                    progressBar.setVisible(false);

                    return null;
                }
            } // for (jj = 1; jj <= (matchoverlays.length - 1); jj++)

            fileType = OVERLAYS;
            nextSliceAddress = raFile.getFilePointer() + 24;
        } // else if (valueByte == 0x6F)
        else if (valueByte == 0x73) { // Check for sticks

            for (jj = 1; jj <= (matchsticks.length - 1); jj++) {
                valueByte = raFile.readByte();

                if (valueByte != matchsticks[jj]) {
                    MipavUtil.displayError("FileCheshireVOI: Expected ticks does not found following s");

                    raFile.close();
                    progressBar.setVisible(false);

                    return null;
                }
            } // for (jj = 1; jj <= (matchsticks.length - 1); jj++)

            fileType = STICKS;
            raFile.seek(0x90L);
            currentLocation = 0x90L;

            // Look for overlays
            notFound = true;
            kk = 0;

            while ((notFound) && (currentLocation < (fileLength - 24))) {
                valueByte = raFile.readByte();
                currentLocation++;

                if (valueByte != matchoverlays[kk]) {
                    kk = 0;
                } else if (kk < (matchoverlays.length - 1)) {
                    kk++;
                } else {
                    notFound = false;
                }
            } // while ((notFound) && (currentLocation < (fileLength - 24)))

            if (notFound) {
                Preferences.debug("FileCheshireVOI: No VOIs found in overlay file: " + file.getName(),
                		Preferences.DEBUG_FILEIO);

                raFile.close();
                progressBar.setVisible(false);

                return null;
            } else {
                nextSliceAddress = raFile.getFilePointer() + 24;
            }
        } // else if (valueByte == 0x73)
        else {

            // No UserWndLevels or overlays or sticks found in files
            Preferences.debug("FileCheshireVOI: No VOIs found in overlay file: " + file.getName(), Preferences.DEBUG_FILEIO);

            raFile.close();
            progressBar.setVisible(false);

            return null;
        }

        while (nextSliceAddress >= 0x90L) {
            sliceAddress[numberSlices++] = nextSliceAddress;
            raFile.seek(nextSliceAddress);
            nextSliceAddress = getInt(endianess);
            checkOffset = true;

            while (checkOffset) {
                checkOffset = false;

                for (jj = 0; jj < numberSlices; jj++) {

                    if (nextSliceAddress == (sliceAddress[jj] + sliceOffset[jj] + 4)) {
                        sliceOffset[jj] = sliceOffset[jj] + 4;
                        raFile.seek(nextSliceAddress);
                        nextSliceAddress = getInt(endianess);
                        checkOffset = true;
                    } // if (nextSliceAddress == (sliceAddress[jj] + sliceOffset[jj] + 4))
                } // for (jj = 0; jj < numberSlices; jj++)
            } // while(checkOffset)
        } // while (nextSliceAddress >= 0x90L)

        if ((fileType == OVERLAYS) && (sliceOffset[0] == 0)) {
            raFile.seek(sliceAddress[0] + 4);
            nextSliceAddress = getInt(endianess);
            sliceOffset[0] = 4;
            checkOffset = true;

            while (checkOffset) {
                checkOffset = false;

                for (jj = 0; jj < numberSlices; jj++) {

                    if (nextSliceAddress == (sliceAddress[jj] + sliceOffset[jj] + 4)) {
                        sliceOffset[jj] = sliceOffset[jj] + 4;
                        raFile.seek(nextSliceAddress);
                        nextSliceAddress = getInt(endianess);
                        checkOffset = true;
                    } // if (nextSliceAddress == (sliceAddress[jj] + sliceOffset[jj] + 4))
                } // for (jj = 0; jj < numberSlices; jj++)
            } // while(checkOffset)

            while (nextSliceAddress >= 0x90L) {
                sliceAddress[numberSlices++] = nextSliceAddress;
                raFile.seek(nextSliceAddress);
                nextSliceAddress = getInt(endianess);
                checkOffset = true;

                while (checkOffset) {
                    checkOffset = false;

                    for (jj = 0; jj < numberSlices; jj++) {

                        if (nextSliceAddress == (sliceAddress[jj] + sliceOffset[jj] + 4)) {
                            sliceOffset[jj] = sliceOffset[jj] + 4;
                            raFile.seek(nextSliceAddress);
                            nextSliceAddress = getInt(endianess);
                            checkOffset = true;
                        } // if (nextSliceAddress == (sliceAddress[jj] + sliceOffset[jj] + 4))
                    } // for (jj = 0; jj < numberSlices; jj++)
                } // while(checkOffset)
            } // while (nextSliceAddress >= 0x90L)
        } // if ((fileType == OVERLAYS) && (sliceOffset[0] == 0))
        /*  raFile.seek(0x24L);
         * // Comment out the chain starting at 0x24 as this produced a contour in 10080-2-dwi-bo-sw.oly // in slice 15
         * that was not seen in the Cheshire program. nextSliceAddress = getInt(endianess); while (nextSliceAddress >=
         * 0x90L) {   sliceAddress[numberSlices++] = nextSliceAddress;   raFile.seek(nextSliceAddress); nextSliceAddress
         * = getInt(endianess);   checkOffset = true;   for (kk = 0; kk < numberSlices; kk++) { if ((nextSliceAddress >=
         * sliceAddress[kk]) && (nextSliceAddress <= (sliceAddress[kk] +                             sliceOffset[kk])))
         * {           // Now feeding into an already found slice address           nextSliceAddress = 0;
         * checkOffset = false;       }   }   while(checkOffset) {  checkOffset = false;       for (jj = 0; jj <
         * numberSlices; jj++) {           if (nextSliceAddress == (sliceAddress[jj] + sliceOffset[jj] + 4)) {
         *     sliceOffset[jj] = sliceOffset[jj]+ 4; raFile.seek(nextSliceAddress);               nextSliceAddress =
         * getInt(endianess);               checkOffset = true;           } // if (nextSliceAddress == (sliceAddress[jj]
         * + sliceOffset[jj] + 4))       } // for (jj =
         * 0; jj < numberSlices; jj++)   } // while(checkOffset)} // while (nextSliceAddress >= 0x90L) */

        fireProgressStateChanged("Reading slice by slice");

        for (ii = 0; ii < numberSlices; ii++) { // read slice by slice
            raFile.seek(sliceAddress[ii] + sliceOffset[ii] + 4); // skip past slice address fields

            // long seekAddress = sliceAddress[ii] + sliceOffset[ii] + 4;
            slice = getInt(endianess); // slice number starting at 0 for first slice

            if (slice >= zDim) {
                MipavUtil.displayError("Impossible slice number of " + slice);

                raFile.close();
                progressBar.setVisible(false);

                return null;
            }

            totalROIl = getInt(endianess); // length of bytes for all ROIs in slice
            ROINumber = getInt(endianess); // number of ROIs in the slice
            nextROIAddress = raFile.getFilePointer();
            doSlice = true;

            for (jj = 0; ((jj < ROINumber) && (doSlice)); jj++) { // read ROI by ROI
                raFile.seek(nextROIAddress);
                ROIl = getInt(endianess); // length of ROI bytes

                if (ROIl > (totalROIl - 8)) {
                    doSlice = false;
                }

                currentLocation = raFile.getFilePointer();
                nextROIAddress = currentLocation + ROIl;
                field = getInt(endianess); // 4 bytes always seem to have value of 2 or 3
                upperLeftY = getInt(endianess);
                upperLeftX = getInt(endianess);
                lowerRightY = getInt(endianess) - 1;
                lowerRightX = getInt(endianess) - 1;
                field2 = getInt(endianess); // 4 bytes that always seem to have value of 2

                if (field2 != 2) {
                    Preferences.debug("field2 = " + field2, Preferences.DEBUG_FILEIO);
                }

                VOINumber = getInt(endianess); // VOI number starting at 1

                /* Skip over the next 169 bytes or 160 bytes
                 * 40 bytes of zeros or 31 bytes of zeros 01 00 00 FF or 00 00 00 FF 00 00 00 00 00 00 01 00 00 00 00 00
                 * 00 00 00 FF 5 bytes of zeros MS<sp>Sans<sp>Serif 20 bytes of zeros 09 00 00 00 00 00 01 00 01 00 3C
                 * 0C 00 00 00 09 11 bytes of zeros 90 01 7 bytes of zeros 02 02 02 4D MS<sp>Sans<sp>Serif19 bytes of
                 * zeros */

                /* Look for the first MS<sp>Sans<sp>Serif */
                notFound = true;
                kk = 0;

                while (notFound) {
                    valueByte = raFile.readByte();

                    if (valueByte != matchByte[kk]) {
                        kk = 0;
                    } else if (kk < (matchByte.length - 1)) {
                        kk++;
                    } else {
                        notFound = false;
                    }
                }

                /* Look for the second MS<sp>Sans<sp>Serif */
                notFound = true;
                kk = 0;

                while (notFound) {
                    valueByte = raFile.readByte();

                    if (valueByte != matchByte[kk]) {
                        kk = 0;
                    } else if (kk < (matchByte.length - 1)) {
                        kk++;
                    } else {
                        notFound = false;
                    }
                }

                upperLeftYLocation = raFile.getFilePointer() + 19;
                bitMapLocation = upperLeftYLocation + 16;

                for (kk = 0; kk < numberSlices; kk++) {

                    if ((sliceAddress[kk] <= bitMapLocation) && (sliceAddress[kk] > sliceAddress[ii])) {
                        doSlice = false;
                    }
                }

                if (doSlice) {

                    // Skip over 19 bytes of zeros to the upperLeftYLocation
                    raFile.seek(upperLeftYLocation);
                    valueInt = getUnsignedShort(endianess);

                    if (valueInt != upperLeftY) {
                        MipavUtil.displayError("Second upperLeftY does not match first");

                        raFile.close();
                        progressBar.setVisible(false);

                        return null;
                    }

                    valueInt = getUnsignedShort(endianess);

                    if (valueInt != upperLeftX) {
                        MipavUtil.displayError("Second upperLeftX does not match first");

                        raFile.close();
                        progressBar.setVisible(false);

                        return null;
                    }

                    valueInt = getUnsignedShort(endianess) - 1;

                    if (valueInt != lowerRightY) {
                        MipavUtil.displayError("Second lowerRightY doesn not match first");

                        raFile.close();
                        progressBar.setVisible(false);

                        return null;
                    }

                    valueInt = getUnsignedShort(endianess) - 1;

                    if (valueInt != lowerRightX) {
                        MipavUtil.displayError("Second lowerRightX does not match first");

                        raFile.close();
                        progressBar.setVisible(false);

                        return null;
                    }

                    bitMapl = getInt(endianess); // length of bit map
                                                 // The bit map starts 4 bytes after this field
                    readBytes = true;
                    currentLocation = raFile.getFilePointer();

                    for (kk = 0; kk < numberSlices; kk++) {

                        if ((sliceAddress[kk] < (currentLocation + bitMapl + 4)) &&
                                (sliceAddress[kk] > sliceAddress[ii])) {
                            readBytes = false;
                        }
                    }

                    if (readBytes) {
                        rowBytes = getInt(endianess); // length of row in bytes 02 for 1-16, 04 for 17-32, etc.
                        totalBytesRead = 0;
                        rowBytesRead = 0;
                        rowsRead = 0;
                        offset = upperLeftX + (xDim * upperLeftY) + (sliceSize * slice);

                        while (totalBytesRead < bitMapl) {
                            valueInt = raFile.readUnsignedByte();
                            totalBytesRead++;

                            if (valueInt <= 0x7F) { // then this is the length - 1 of the number of following

                                // bytes belonging to this length - 1 byte
                                bytesToRead = valueInt + 1;

                                for (kk = 0; kk < bytesToRead; kk++) {
                                    valueByte = raFile.readByte();
                                    maskValue = 0x80;

                                    for (mm = 0; mm < 8; mm++) {

                                        if ((valueByte & maskValue) != 0) {
                                            position = offset + mm + (8 * rowBytesRead) + (xDim * rowsRead);

                                            // Allow for same VOI to be clicked in twice
                                            if ((maskBuffer[position] != 0) &&
                                                    (maskBuffer[position] != (byte) VOINumber)) {
                                                MipavUtil.displayError("Overlapping VOIs in slice = " + slice);

                                                currentLocation = raFile.getFilePointer();
                                                Preferences.debug("following bytes current location = " +
                                                                  currentLocation, Preferences.DEBUG_FILEIO);
                                                raFile.close();

                                                return null;
                                            } else {
                                                maskBuffer[position] = (byte) VOINumber;
                                            }
                                        } // if ((valueByte & maskValue) != 0)

                                        maskValue = (maskValue >> 1);
                                    } // for (mm = 0; mm < 8; mm++)

                                    rowBytesRead++;

                                    if (rowBytesRead == rowBytes) {
                                        rowsRead++;
                                        rowBytesRead = 0;
                                    }
                                } // for (kk = 0; kk < bytesToRead; kk++)

                                totalBytesRead += bytesToRead;
                            } else { // the 1's complement + 2 is the multiplier of the following byte
                                valueInt = 0xFFFFFF00 | valueInt;
                                valueInt = (~valueInt) + 2; // Use the next byte this number of times
                                valueByte = raFile.readByte();
                                totalBytesRead++;

                                for (kk = 0; kk < valueInt; kk++) {
                                    maskValue = 0x80;

                                    for (mm = 0; mm < 8; mm++) {

                                        if ((valueByte & maskValue) != 0) {
                                            position = offset + mm + (8 * rowBytesRead) + (xDim * rowsRead);

                                            // Allow for same VOI to be clicked in twice
                                            if ((maskBuffer[position] != 0) &&
                                                    (maskBuffer[position] != (byte) VOINumber)) {
                                                MipavUtil.displayError("Overlapping VOIs in slice = " + slice);

                                                currentLocation = raFile.getFilePointer();
                                                Preferences.debug("multiplier bytes current location = " +
                                                                  currentLocation, Preferences.DEBUG_FILEIO);
                                                raFile.close();
                                                progressBar.setVisible(false);

                                                return null;
                                            } else {
                                                maskBuffer[position] = (byte) VOINumber;
                                            }
                                        } // if ((valueByte & maskValue) != 0)

                                        maskValue = (maskValue >> 1);
                                    } // for (mm = 0; mm < 8; mm++)

                                    rowBytesRead++;

                                    if (rowBytesRead == rowBytes) {
                                        rowsRead++;
                                        rowBytesRead = 0;
                                    }
                                } // for (kk = 0; kk < valueInt; kk++)
                            }
                        } // while (bytesRead < bitMapl)
                    } // if (readBytes)
                } // if (doSlice)
            } // for (jj = 0; jj < ROINumber; jj++)
        } // for (ii = 0; ii < numberSlices; ii++)

        raFile.close();

        fireProgressStateChanged("Generating contours from bitmaps");

        // Must form 4 copies of every xy pixel or algorithm will not properly handle
        // one pixel wide passages into the object
        xDimE = 2 * xDim;
        yDimE = 2 * yDim;
        length = xDimE * yDimE;

        try {
            imgBuffer = new short[sliceSize];
            expImgBuffer = new short[length];
            mask = new BitSet(sliceSize);
            maskN = new BitSet(length);
            maskS = new BitSet(length);
            maskE = new BitSet(length);
            maskW = new BitSet(length);
            mask2 = new BitSet(length);
            maskAll = new BitSet(length);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("FileCheshireVOI: Out of memory.");

            progressBar.setVisible(false);

            return null;
        }

        for (zz = 0; zz < zDim; zz++) {

            for (ii = 0; ii < sliceSize; ii++) {
                imgBuffer[ii] = maskBuffer[ii + (zz * sliceSize)];
            }

            for (ii = 0; ii < sliceSize; ii++) {
                mask.clear(ii);
            }

            for (ii = 0; ii < length; ii++) {
                maskN.clear(ii);
                maskE.clear(ii);
                maskS.clear(ii);
                maskW.clear(ii);
                mask2.clear(ii);
                maskAll.clear(ii);
            }

            for (yy = 0; yy < yDim; yy++) {

                for (xx = 0; xx < xDim; xx++) {
                    expImgBuffer[(2 * xx) + (4 * yy * xDim)] = imgBuffer[xx + (yy * xDim)];
                    expImgBuffer[(2 * xx) + 1 + (4 * yy * xDim)] = imgBuffer[xx + (yy * xDim)];
                    expImgBuffer[(2 * xx) + (4 * yy * xDim) + (2 * xDim)] = imgBuffer[xx + (yy * xDim)];
                    expImgBuffer[(2 * xx) + 1 + (4 * yy * xDim) + (2 * xDim)] = imgBuffer[xx + (yy * xDim)];
                }
            }

            for (yy = 0; yy < yDimE; yy++) {

                for (xx = 0; xx < xDimE; xx++) {
                    scanPos = xx + (yy * xDimE);
                    smallX = xx / 2;
                    smallY = yy / 2;
                    smallPos = smallX + (smallY * xDim);

                    if ((!mask.get(smallPos)) && (imgBuffer[smallPos] != 0)) {

                        // have found a new contour
                        // Since genContour works on finding a four-connected boundary using
                        // Papert's turtle algorithm, the routine to grow the region must
                        // also use four connected neighbors.
                        // Set mask of all points in 2D region
                        setRegionMask(smallX, smallY, imgBuffer[smallPos]);

                        // Expand mask points so as to draw boundary properly.
                        // A pixel point only refers to left upper corner
                        setRegionMaskAll();

                        // Return polygon of contour
                        Point startPt = new Point(xx, yy);
                        contourPolygon = AlgorithmMorphology2D.genContour(xDimE, yDimE, startPt, maskAll);

                        for (ii = 0; ii < length; ii++) {
                            maskAll.clear(ii);
                        }

                        newGrayScale = true;

                        for (ii = 0; ((newGrayScale) && (ii < grayScaleNumber)); ii++) {

                            if (grayScaleArray[ii] == expImgBuffer[scanPos]) {
                                newGrayScale = false;

                                // add the polygon to an existing VOI
                                addedVOI[ii].importPolygon(contourPolygon, zz);
                                ((VOIContour) (addedVOI[ii].getCurves().lastElement())).trimPoints(0.1, false);
                            }
                        }

                        if (newGrayScale) {
                            grayScaleArray[grayScaleNumber] = expImgBuffer[scanPos];
                            addedVOI[grayScaleNumber] = new VOI(grayScaleNumber, "VOI" + grayScaleNumber,
                                                                VOI.CONTOUR, -1.0f);
                            addedVOI[grayScaleNumber].importPolygon(contourPolygon, zz);
                            ((VOIContour) (addedVOI[grayScaleNumber].getCurves().lastElement())).trimPoints(0.1,
                                                                                                                true);
                            grayScaleNumber++;

                            if (grayScaleNumber >= 1000) {
                                MipavUtil.displayError("File Cheshire VOI Extraction: Impossibly high >= 1000 gray scales detected");

                                progressBar.setVisible(false);

                                return null;
                            } // end of if (grayScaleNumber >= 1000)
                        } // end of if (newGrayScale)

                    } // end of if ((!mask.get(scanPos)) && (expImgBuffer[scanPos] != 0))
                } // end of for (x = 0; x < xDimE; x++)
            } // end of for (y = 0; y < yDimE; y++)

        } // end of for (z = 0; z < zDim; z++)

        /*
        for (ii = 0; ii < grayScaleNumber; ii++) {
            curves = addedVOI[ii].getCurves();

            for (zz = 0; zz < zDim; zz++) {
                nCurves = curves[zz].size();

                for (jj = 0; jj < nCurves; jj++) {
                    // Remove only collinear points with parameter = 0.10
                    // ((VOIContour)(curves[z].elementAt(jj))).trimPoints(0.00);
                }
            } // end of for (z = 0; z < zDim; z++)

        } // end of for (ii = 0; ii < grayScaleNumber; ii++)
         */
        voi = new VOI[grayScaleNumber];

        for (ii = 0; ii < grayScaleNumber; ii++) {
            voi[ii] = addedVOI[ii];
        }

        fireProgressStateChanged(100);
        progressBar.setVisible(false);


        return voi;


    }

    /**
     * setRegionMask set the mask of all points in the 4 connected region with a gray scale value of objectValue.
     *
     * @param  xStart       the x coordinate of the starting point
     * @param  yStart       the y coordinate of the starting point
     * @param  objectValue  the gray scale value of the object
     */

    public void setRegionMask(int xStart, int yStart, short objectValue) {
        mask.set(xStart + (xDim * yStart));
        mask2.set((2 * xStart) + (4 * xDim * yStart));
        mask2.set((2 * xStart) + 1 + (4 * xDim * yStart));
        mask2.set((2 * xStart) + (4 * xDim * yStart) + (2 * xDim));
        mask2.set((2 * xStart) + 1 + (4 * xDim * yStart) + (2 * xDim));
        maskAll.set((2 * xStart) + (4 * xDim * yStart));
        maskAll.set((2 * xStart) + 1 + (4 * xDim * yStart));
        maskAll.set((2 * xStart) + (4 * xDim * yStart) + (2 * xDim));
        maskAll.set((2 * xStart) + 1 + (4 * xDim * yStart) + (2 * xDim));

        if (((xStart + 1) < xDim) && (!mask.get(xStart + 1 + (xDim * yStart))) &&
                (imgBuffer[xStart + 1 + (xDim * yStart)] == objectValue)) {
            setRegionMask(xStart + 1, yStart, objectValue);
        }

        if (((xStart - 1) > 0) && (!mask.get(xStart - 1 + (xDim * yStart))) &&
                (imgBuffer[xStart - 1 + (xDim * yStart)] == objectValue)) {
            setRegionMask(xStart - 1, yStart, objectValue);
        }

        if (((yStart + 1) < yDim) && (!mask.get(xStart + (xDim * (yStart + 1)))) &&
                (imgBuffer[xStart + (xDim * (yStart + 1))] == objectValue)) {
            setRegionMask(xStart, yStart + 1, objectValue);
        }

        if (((yStart - 1) > 0) && (!mask.get(xStart + (xDim * (yStart - 1)))) &&
                (imgBuffer[xStart + (xDim * (yStart - 1))] == objectValue)) {
            setRegionMask(xStart, yStart - 1, objectValue);
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void setRegionMaskAll() {

        for (yt = 0; yt < yDimE; yt++) {

            for (xt = 0; xt < xDimE; xt++) {
                testPos = xt + (yt * xDimE);

                if (mask2.get(testPos)) {
                    neighbors = 0;

                    if (((xt + 1) < xDimE) && (mask2.get(xt + 1 + (xDimE * yt)))) {
                        maskE.set(testPos);
                        neighbors++;
                    }

                    if (((xt - 1) > 0) && (mask2.get(xt - 1 + (xDimE * yt)))) {
                        maskW.set(testPos);
                        neighbors++;
                    }

                    if (((yt + 1) < yDimE) && (mask2.get(xt + (xDimE * (yt + 1))))) {
                        maskS.set(testPos);
                        neighbors++;
                    }

                    if (((yt - 1) > 0) && (mask2.get(xt + (xDimE * (yt - 1))))) {
                        maskN.set(testPos);
                        neighbors++;
                    }

                    // Since each pixel in imgBuffer is copied as a 4 pixel square in
                    // expImgBuffer, there must always be at least 2 neighbors.
                    /* if ((neighbors == 0)|| (neighbors == 1)) {
                     * if ((xt + 1) < xDimE) {  maskAll.set(xt + 1 + xDimE*yt); } if ((yt + 1) < yDimE) { maskAll.set(xt
                     * + xDimE*(yt+1)); } if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {  maskAll.set(xt
                     * + 1 + xDimE*(yt + 1)); } } // end of if ((neighbors == 0) || (neighbors == 1)) */
                    if (neighbors == 2) {

                        if ((maskN.get(testPos)) && (maskE.get(testPos))) {

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        } else if ((maskN.get(testPos)) && (maskS.get(testPos))) {

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        } else if ((maskN.get(testPos)) && (maskW.get(testPos))) {

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        } else if ((maskE.get(testPos)) && (maskS.get(testPos))) {

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }
                        } else if ((maskE.get(testPos)) && (maskW.get(testPos))) {

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        } else { // south and west neighbors

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        }
                    } // end of else if (neighbors == 2)
                    else if (neighbors == 3) {

                        if (!maskN.get(testPos)) {

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }
                        } else if (!maskE.get(testPos)) {

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        } else if (!maskS.get(testPos)) {

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        } else { // no west neighbor

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }
                        }
                    } // end of else if (neighbors == 3)
                } // end of if (mask2.set(testPos)
            } // end of for (x = 0; x < xDimE; x++)
        } // end of for (y = 0; y < yDimE; y++)

        for (it = 0; it < length; it++) {
            mask2.clear(it);
        } // end of for (it = 0; it < length; it++)

    }

}
