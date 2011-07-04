package gov.nih.mipav.model.file;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.IOException;


/**
 * Class that reads a lossless compressed JPEG file. These are sometimes encapsulated in DICOM. The lossless decoder
 * uses a Huffman table to interpret values. For color images this file will have to be modified. Some code for color is
 * here but it isn't universal. There is a private class within this file that does the bit reading and packing. For
 * more information see the comments in front of that class. This code was ported from Huang and Smith's Cornell
 * software.
 * 
 * 
 * 
 * <hr>
 * 
 * <pre>
 * Copyright (c) 1993 Cornell University, Kongji Huang
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for research purposes, without fee, and without written
 * agreement is hereby granted, provided that the above copyright notice
 * and the following two paragraphs appear in all copies of this
 * software.
 * 
 * IN NO EVENT SHALL THE CORNELL UNIVERSITY BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING
 * OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE
 * UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 * 
 * THE CORNELL UNIVERSITY SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE
 * PROVIDED HEREUNDER IS ON AN &quot;AS IS&quot; BASIS, AND THE UNIVERSITY OF
 * CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
 * ENHANCEMENTS, OR MODIFICATIONS.
 * </pre>
 * 
 * <hr>
 * 
 * <pre>
 * Copyright (c) 1993 The Regents of the University of California, Brian
 * C. Smith All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written
 * agreement is hereby granted, provided that the above copyright notice
 * and the following two paragraphs appear in all copies of this
 * software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 * THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE
 * PROVIDED HEREUNDER IS ON AN &quot;AS IS&quot; BASIS, AND THE UNIVERSITY OF
 * CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
 * ENHANCEMENTS, OR MODIFICATIONS.
 * </pre>
 * 
 * <hr>
 * 
 * <pre>
 * IJG Copyright
 * 
 * The authors make NO WARRANTY or representation, either express or
 * implied, with respect to this software, its quality, accuracy,
 * merchantability, or fitness for a particular purpose.  This software is
 * provided &quot;AS IS&quot;, and you, its user, assume the entire risk as to its
 * quality and accuracy.
 * 
 * This software is copyright (C) 1991, 1992, Thomas G. Lane.  All Rights
 * Reserved except as specified below.
 * 
 * Permission is hereby granted to use, copy, modify, and distribute this
 * software (or portions thereof) for any purpose, without fee, subject to
 * these conditions:  (1) If any part of the source code for this software
 * is distributed, then this README file must be included, with this
 * copyright and no-warranty notice unaltered; and any additions,
 * deletions, or changes to the original files must be clearly indicated
 * in accompanying documentation.  (2) If only executable code is
 * distributed, then the accompanying documentation must state that &quot;this
 * software is based in part on the work of the Independent JPEG Group&quot;.
 * (3) Permission for use of this software is granted only if the user
 * accepts full responsibility for any undesirable consequences; the
 * authors accept NO LIABILITY for damages of any kind.
 * 
 * Permission is NOT granted for the use of any IJG author's name or
 * company name in advertising or publicity relating to this software or
 * products derived from it.  This software may be referred to only as
 * &quot;the Independent JPEG Group's software&quot;.
 * 
 * We specifically permit and encourage the use of this software as the
 * basis of commercial products, provided that all warranty or liability
 * claims are assumed by the product vendor.
 * </pre>
 * 
 * @version 1.0 May 14, 2002
 * @author Neva Cherniavsky
 */
public class FileDicomJPEG {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Marker tags in JPEG header. */
    private static final byte M_SOF0 = (byte) 0xc0;

    /** DOCUMENT ME! */
    private static final byte M_SOF1 = (byte) 0xc1;

    /** DOCUMENT ME! */
    private static final byte M_SOF2 = (byte) 0xc2;

    /** DOCUMENT ME! */
    private static final byte M_SOF3 = (byte) 0xc3;

    /** DOCUMENT ME! */
    private static final byte M_SOF5 = (byte) 0xc5;

    /** DOCUMENT ME! */
    private static final byte M_SOF6 = (byte) 0xc6;

    /** DOCUMENT ME! */
    private static final byte M_SOF7 = (byte) 0xc7;

    /** DOCUMENT ME! */
    private static final byte M_JPG = (byte) 0xc8;

    /** DOCUMENT ME! */
    private static final byte M_SOF9 = (byte) 0xc9;

    /** DOCUMENT ME! */
    private static final byte M_SOF10 = (byte) 0xca;

    /** DOCUMENT ME! */
    private static final byte M_SOF11 = (byte) 0xcb;

    /** DOCUMENT ME! */
    private static final byte M_SOF13 = (byte) 0xcd;

    /** DOCUMENT ME! */
    private static final byte M_SOF14 = (byte) 0xce;

    /** DOCUMENT ME! */
    private static final byte M_SOF15 = (byte) 0xcf;

    /** DOCUMENT ME! */
    private static final byte M_DHT = (byte) 0xc4;

    /** DOCUMENT ME! */
    //private static final byte M_DAC = (byte) 0xcc;

    /** DOCUMENT ME! */
    private static final byte M_RST0 = (byte) 0xd0;

    /** DOCUMENT ME! */
    private static final byte M_RST1 = (byte) 0xd1;

    /** DOCUMENT ME! */
    private static final byte M_RST2 = (byte) 0xd2;

    /** DOCUMENT ME! */
    private static final byte M_RST3 = (byte) 0xd3;

    /** DOCUMENT ME! */
    private static final byte M_RST4 = (byte) 0xd4;

    /** DOCUMENT ME! */
    private static final byte M_RST5 = (byte) 0xd5;

    /** DOCUMENT ME! */
    private static final byte M_RST6 = (byte) 0xd6;

    /** DOCUMENT ME! */
    private static final byte M_RST7 = (byte) 0xd7;

    /** DOCUMENT ME! */
    private static final byte M_SOI = (byte) 0xd8;

    /** DOCUMENT ME! */
    private static final byte M_EOI = (byte) 0xd9;

    /** DOCUMENT ME! */
    private static final byte M_SOS = (byte) 0xda;

    /** DOCUMENT ME! */
    private static final byte M_DQT = (byte) 0xdb;

    /** DOCUMENT ME! */
    //private static final byte M_DNL = (byte) 0xdc;

    /** DOCUMENT ME! */
    private static final byte M_DRI = (byte) 0xdd;

    /** DOCUMENT ME! */
    //private static final byte M_DHP = (byte) 0xde;

    /** DOCUMENT ME! */
    //private static final byte M_EXP = (byte) 0xdf;

    /** Required marker in this standard. */
    private static final byte M_APP0 = (byte) 0xe0;

    /** DOCUMENT ME! */
    //private static final byte M_APP15 = (byte) 0xef;

    /** DOCUMENT ME! */
    //private static final byte M_JPG0 = (byte) 0xf0;

    /** DOCUMENT ME! */
    //private static final byte M_JPG13 = (byte) 0xfd;

    /** DOCUMENT ME! */
    //private static final byte M_COM = (byte) 0xfe;

    /** DOCUMENT ME! */
    private static final byte M_TEM = (byte) 0x01;

    /** DOCUMENT ME! */
    //private static final byte M_ERROR = (byte) 0x100;

    /** Entry n is 2**(n-1); used to test whether we need to make a number negative. */
    private static int[] extendTest = {0, 0x0001, 0x0002, 0x0004, 0x0008, 0x0010, 0x0020, 0x0040, 0x0080, 0x0100,
            0x0200, 0x0400, 0x0800, 0x1000, 0x2000, 0x4000, 0x8000};

    /** Entry n is (-1 << n) + 1; used to make a number negative. */
    private static int[] extendOffset = {0, ( ( -1) << 1) + 1, ( ( -1) << 2) + 1, ( ( -1) << 3) + 1, ( ( -1) << 4) + 1,
            ( ( -1) << 5) + 1, ( ( -1) << 6) + 1, ( ( -1) << 7) + 1, ( ( -1) << 8) + 1, ( ( -1) << 9) + 1,
            ( ( -1) << 10) + 1, ( ( -1) << 11) + 1, ( ( -1) << 12) + 1, ( ( -1) << 13) + 1, ( ( -1) << 14) + 1,
            ( ( -1) << 15) + 1, ( ( -1) << 16) + 1};

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Useful bit mask for getting appropriate bits out of number. */
    private final int[] bitMask = {0xffffffff, 0x7fffffff, 0x3fffffff, 0x1fffffff, 0x0fffffff, 0x07ffffff, 0x03ffffff,
            0x01ffffff, 0x00ffffff, 0x007fffff, 0x003fffff, 0x001fffff, 0x000fffff, 0x0007ffff, 0x0003ffff, 0x0001ffff,
            0x0000ffff, 0x00007fff, 0x00003fff, 0x00001fff, 0x00000fff, 0x000007ff, 0x000003ff, 0x000001ff, 0x000000ff,
            0x0000007f, 0x0000003f, 0x0000001f, 0x0000000f, 0x00000007, 0x00000003, 0x00000001};

    /**
     * Read in DHT header. Number of codes with bitlength of index. Thus, bits[2] = 1 means one code represented by 2
     * bits. bits[3] = 4 means four codes represented by 3 bits. 2D array necessary in case of color image - component
     * #.
     */
    private final byte[][] bits;

    /** Needed for color images. */
    private final int[] componentId;

    /** Needed for color images. */
    private final short[] componentIndex;

    /** Number of components in scan; 1 for black and white, 4 for color. */
    private int compsInScan;

    /** Data precision. */
    private byte dataPrecision;

    /** Height read in from DICOM file; only use if imageHeight isn't defined. */
    private final int dicomH;

    /** Width read in from DICOM file; only use if imageWidth isn't defined. */
    private final int dicomW;

    /** Horizontal sample factor. */
    private final int[] hSampFactor;

    /**
     * Read in DHT header. Huffman code values, used to determine Huffman table (value array). Also directly accessed
     * when numbits > 8, since fast lookup is not possible.
     */
    private final byte[][] huffval;

    /** Image data we're reading. */
    private final byte[] image;

    /** Image height. */
    private int imageHeight;

    /** Image width. */
    private int imageWidth;

    /** mincode[index] = minimum legal code at that index. */
    private final int[][] maxcode;

    /** mincode[index] = minimum legal code at that index. */
    private final short[][] mincode;

    /** Numbers needed for restart. Hasn't been tested! */
    private int nextRestartNum;

    /** number of bits needed for code at index. */
    private final int[][] numbits;

    /** Number of components - 1 for black and white, 4 for color. */
    private int numComponents;

    /** Shift by this when putting into int buffer. */
    private int Pt;

    /** Numbers needed for restart. Hasn't been tested! */
    private int restartInRows;

    /** Restart interval. */
    private int restartInterval;

    /** Numbers needed for restart. Hasn't been tested! */
    @SuppressWarnings("unused")
    private int restartRowsToGo;

    /** Byte indicating which location to use as the predictor. */
    private byte Ss;

    /** Table number - always 0 for non-color images. */
    private final int[] tableNo;

    /** valptr[index] = pointer to where legal code values start. */
    private final short[][] valptr;

    /** huffman value for code. */
    private final int[][] value;

    /** Vertical sample factor. */
    private final int[] vSampFactor;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Assigns image buffer and creates the other arrays needed to read in image.
     * 
     * @param imageBuffer Image buffer.
     * @param width DOCUMENT ME!
     * @param height DOCUMENT ME!
     */
    public FileDicomJPEG(final byte[] imageBuffer, final int width, final int height) {
        image = imageBuffer;
        // try
        // {
        // File f = new File("C:\\f.txt");
        // FileWriter write = new FileWriter(f);
        // String s = new String();
        // for(int i=0; i<image.length; i++)
        // {
        // s = s+(Byte.toString(image[i])+" ");
        // if(i%100==0)
        // {
        // s = s+"\n\r";
        // write.write(s);
        // s = new String();
        // }
        // }
        // write.write(s+"\n\r");
        // write.flush();
        // write.close();
        //    
        // }
        // catch(Exception e)
        // {
        // e.printStackTrace();
        // }

        bits = new byte[4][17];
        huffval = new byte[4][256];
        mincode = new short[4][17];
        maxcode = new int[4][18];
        valptr = new short[4][17];
        numbits = new int[4][256];
        value = new int[4][256];
        componentIndex = new short[4];
        componentId = new int[4];
        hSampFactor = new int[4];
        vSampFactor = new int[4];
        tableNo = new int[4];
        dicomW = width;
        dicomH = height;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Extracts the JPEG image by processing the byte image array.
     * 
     * @return image buffer - the JPEG image
     * 
     * @throws IOException DOCUMENT ME!
     */
    public int[] extractJPEGImage() throws IOException {

        if ( ! ( (image[0] == (byte) 0xFF) && (image[1] == (byte) 0xD8))) {
            MipavUtil.displayError("No start of image tag at start of JPEG.");

            return null;
        }

        int index = processTables(2);

        if (index == -1) {
            return null;
        }

        switch (image[index]) {

            case M_SOF0:
            case M_SOF1:
            case M_SOF3:
                index = getSOF(index + 1);
                break;

            default:
                Preferences.debug("Unsupported SOF marker type " + image[index] + "\n", Preferences.DEBUG_FILEIO);
                break;
        }

        // Process markers until SOS or EOI
        index = processTables(index);

        if (index == -1) {
            return null;
        }

        switch (image[index]) {

            case M_SOS:
                index = getSOS(index + 1);
                break;

            case M_EOI:
                MipavUtil.displayError("FileDicomJPEG: Empty JPEG file");

                return null;

            default:
                Preferences.debug("FileDicomJPEG: Unexpected marker " + image[index] + "\n", Preferences.DEBUG_FILEIO);
                break;
        }

        if (index == -1) {
            return null;
        }

        for (int i = 0; i < numComponents; i++) {
            fixHuffmanTable(tableNo[i]);
        }

        restartInRows = restartInterval / imageWidth;
        restartRowsToGo = restartInRows;
        nextRestartNum = 0;
        // index = index+2;
        // if(image[index] == 0)
        // {
        // System.out.println("Index: "+index+"\tValue: "+image[index]);
        // boolean zero = true;
        // while(zero)
        // {
        // index++;
        // System.out.println("Index: "+index+"\tValue: "+image[index]);
        // if(image[index] == 0)
        // {}
        // else
        // {
        // System.out.println("FinalIndex: "+index);
        // zero = false;
        // }
        // }
        // }
        // System.out.println("Index: "+index+"\tValue: "+image[index]);
        return decodeImage(index);
    }

    /**
     * Decode the first raster line of samples at the start of the scan and at the beginning of each restart interval.
     * This includes modifying the component value so the real value, not the difference is returned.
     * 
     * @param table Huffman table.
     * @param curRowBuf Current row buffer; 2D because if image is color, there are 4 components.
     */
    private void decodeFirstRow(final HuffTable table, final short[][] curRowBuf) {
        short curComp;
        int col, numCOL;
        int d;

        numCOL = imageWidth;

        // the start of the scan or at the beginning of restart interval.
        for (curComp = 0; curComp < compsInScan; curComp++) {
            d = table.huffDecode(tableNo[curComp]);

            if (d == -1) {

                if (table.isMarker() == true) {
                    return;
                }
            }

            // Add the predictor to the difference.
            final short s = (short) (d + (1 << (dataPrecision - Pt - 1)));
            curRowBuf[0][curComp] = s;
        }

        // the rest of the first row
        for (col = 1; col < numCOL; col++) {

            for (curComp = 0; curComp < compsInScan; curComp++) {
                d = table.huffDecode(tableNo[curComp]);
                // if(d!=0)
                //    
                if (d == -1) {

                    if (table.isMarker() == true) {
                        return;
                    }
                }

                // Add the predictor to the difference.
                curRowBuf[col][curComp] = (short) (d + curRowBuf[col - 1][curComp]);
            }
        }
    }

    /**
     * Decode the input stream. This includes modifying the component value so the real value, not the difference is
     * returned.
     * 
     * @param index Index where we currently are in the image.
     * 
     * @return Buffer holding decompressed image.
     */
    private int[] decodeImage(int index) {
        int d, col, row;
        short curComp;
        int predictor;
        int numCOL, numROW;
        int psv;
        // System.out.println("Image: "+image[index]+" "+image[index+1]+" "+image[index+2]+" "+image[index+3]+"\tIndex: "+index+" to "+(index+3));
        numCOL = imageWidth;
        numROW = imageHeight;
        psv = Ss;

        final short[][] curRowBuf = new short[numCOL][4];
        final short[][] prevRowBuf = new short[numCOL][4];
        int[] buffer;
        int j = 0;

        final HuffTable table = new HuffTable(image, index);

        // Decode the first row of image. Output the row and
        // turn this row into a previous row for later predictor
        // calculation.
        decodeFirstRow(table, curRowBuf);

        if (compsInScan == 1) {
            buffer = new int[imageWidth * imageHeight];

            for (int i = 0; i < numCOL; i++) {
                buffer[i] = curRowBuf[i][0] << Pt;
                prevRowBuf[i][0] = curRowBuf[i][0];
            }
        } else {
            buffer = new int[4 * imageWidth * imageHeight];

            for (int i = 0; i < numCOL; i++) {
                buffer[j++] = 255;
                buffer[j++] = curRowBuf[i][0] << Pt;
                buffer[j++] = curRowBuf[i][1] << Pt;
                buffer[j++] = curRowBuf[i][2] << Pt;
                prevRowBuf[i][0] = curRowBuf[i][0];
                prevRowBuf[i][1] = curRowBuf[i][1];
                prevRowBuf[i][2] = curRowBuf[i][2];
                prevRowBuf[i][3] = curRowBuf[i][3];
            }
        }

        for (row = 1; row < numROW; row++) {

            // Account for restart interval, process restart marker if needed.
            int rowsToGo = restartInterval / imageWidth;

            // Note: everything within the "if" hasn't been tested yet. Usually
            // restartInterval == 0 and this doesn't get executed.
            if ( (restartInterval / imageWidth) > 0) {

                if (rowsToGo == 0) {
                    index = processRestart(table.getIndex());
                    table.setIndex(index);

                    // Reset predictors at restart.
                    decodeFirstRow(table, curRowBuf);

                    if (compsInScan == 1) {
                        buffer = new int[imageWidth * imageHeight];

                        for (int i = 0; i < numCOL; i++) {
                            buffer[i] = curRowBuf[i][0] << Pt;
                            prevRowBuf[i][0] = curRowBuf[i][0];
                        }
                    } else {
                        buffer = new int[4 * imageWidth * imageHeight];

                        for (int i = 0; i < numCOL; i++) {
                            buffer[j++] = curRowBuf[i][0] << Pt;
                            buffer[j++] = curRowBuf[i][1] << Pt;
                            buffer[j++] = curRowBuf[i][2] << Pt;
                            buffer[j++] = curRowBuf[i][3] << Pt;
                            prevRowBuf[i][0] = curRowBuf[i][0];
                            prevRowBuf[i][1] = curRowBuf[i][1];
                            prevRowBuf[i][2] = curRowBuf[i][2];
                            prevRowBuf[i][3] = curRowBuf[i][3];
                        }
                    }

                    continue;
                }

                rowsToGo--;
            }

            // The upper neighbors are predictors for the first column.
            for (curComp = 0; curComp < compsInScan; curComp++) {
                d = table.huffDecode(tableNo[curComp]);

                if (d == -1) {

                    if (table.isMarker() == true) {
                        return buffer;
                    }
                }

                curRowBuf[0][curComp] = (short) ((short) (d) + prevRowBuf[0][curComp]);
            }

            // For the rest of the column on this row, predictor
            // calculations are based on PSV.

            for (col = 1; col < numCOL; col++) {

                for (curComp = 0; curComp < compsInScan; curComp++) {
                    d = table.huffDecode(tableNo[curComp]);
                    // if(index > 75)
                    //    

                    if (d == -1) {

                        if (table.isMarker() == true) {
                            return buffer;
                        }
                    }
                    if (row == 1 && col == 1 || col == 1) {
                        predictor = prevRowBuf[col][curComp]
                                + ( (curRowBuf[col - 1][curComp] - prevRowBuf[col - 1][curComp]) >> 1);
                    } else {
                        switch (psv) {

                            case 0:
                                predictor = 0;
                                break;

                            case 1:
                                predictor = curRowBuf[col - 1][curComp];
                                break;

                            case 2:
                                predictor = prevRowBuf[col][curComp];
                                break;

                            case 3:
                                predictor = prevRowBuf[col - 1][curComp];
                                break;

                            case 4:
                                predictor = curRowBuf[col - 1][curComp] + prevRowBuf[col][curComp]
                                        - prevRowBuf[col - 1][curComp];
                                break;

                            case 5:
                                predictor = curRowBuf[col - 1][curComp]
                                        + ( (prevRowBuf[col][curComp] - prevRowBuf[col - 1][curComp]) >> 1);
                                break;

                            case 6:
                                predictor = prevRowBuf[col][curComp]
                                        + ( (curRowBuf[col - 1][curComp] - prevRowBuf[col - 1][curComp]) >> 1);
                                break;

                            case 7:
                                predictor = (curRowBuf[col - 1][curComp] + prevRowBuf[col][curComp]) >> 1;
                                break;

                            default:
                                Preferences.debug("FileDicomJPEG: Warning: Undefined PSV\n", Preferences.DEBUG_FILEIO);
                                predictor = 0;
                        }
                    }

                    curRowBuf[col][curComp] = (short) ((short) (d) + predictor);
                }
            }

            if (compsInScan == 1) {

                for (int i = 0; i < numCOL; i++) {
                    buffer[ (row * numCOL) + i] = curRowBuf[i][0] << Pt;
                    prevRowBuf[i][0] = curRowBuf[i][0];
                }
            } else {

                for (int i = 0; i < numCOL; i++) {
                    buffer[j++] = 255;
                    buffer[j++] = curRowBuf[i][0] << Pt;
                    buffer[j++] = curRowBuf[i][1] << Pt;
                    buffer[j++] = curRowBuf[i][2] << Pt;
                    prevRowBuf[i][0] = curRowBuf[i][0];
                    prevRowBuf[i][1] = curRowBuf[i][1];
                    prevRowBuf[i][2] = curRowBuf[i][2];
                    prevRowBuf[i][3] = curRowBuf[i][3];
                }
            }
        }

        index = table.getIndex();

        return buffer;
    }

    /**
     * Sets up Huffman table based on values read in header.
     * 
     * @param tableNum Usually 0, haven't tested when it isn't.
     */
    private void fixHuffmanTable(final int tableNum) {
        int p, i, k, lastp, si;
        final byte[] huffsize = new byte[257];
        final int[] huffcode = new int[257];
        int code;
        int size;
        int svalue, ll, ul;

        // Figure C.1: make table of Huffman code length for each symbol
        // Note that this is in code-length order.
        p = 0;

        for (k = 1; k <= 16; k++) {

            for (i = 1; i <= bits[tableNum][k]; i++) {
                huffsize[p++] = (byte) (k & 0xff);
            }
        }

        huffsize[p] = 0;
        lastp = p;

        // Figure C.2: generate the codes themselves
        // Note that this is in code-length order.
        code = 0;
        si = huffsize[0];
        p = 0;

        while (huffsize[p] != 0) {

            while ( (huffsize[p]) == si) {
                huffcode[p++] = code;
                code++;
            }

            code <<= 1;
            si++;
        }

        // Figure F.15: generate decoding tables
        p = 0;

        for (k = 1; k <= 16; k++) {

            if (bits[tableNum][k] != 0) {
                valptr[tableNum][k] = (short) p;
                mincode[tableNum][k] = (short) (huffcode[p] & 0xffff);
                p += bits[tableNum][k];
                maxcode[tableNum][k] = huffcode[p - 1];

            } else {
                maxcode[tableNum][k] = -1;
            }
        }

        // We put in this value to ensure HuffDecode terminates.
        maxcode[tableNum][17] = (int) 0xFFFFFL;

        // Build the numbits, value lookup tables.
        // These table allow us to gather 8 bits from the bits stream,
        // and immediately lookup the size and value of the huffman codes.
        // If size is zero, it means that more than 8 bits are in the huffman
        // code (this happens about 3-4% of the time).
        for (i = 0; i < numbits[tableNum].length; i++) {
            numbits[tableNum][i] = 0;
        }

        for (p = 0; p < lastp; p++) {
            size = huffsize[p];

            if (size <= 8) {
                svalue = huffval[tableNum][p];
                code = huffcode[p];
                ll = code << (8 - size);

                if (size < 8) {
                    ul = ll | bitMask[24 + size];
                } else {
                    ul = ll;
                }

                for (i = ll; i <= ul; i++) {
                    numbits[tableNum][i] = size;
                    value[tableNum][i] = svalue;
                }
            }
        }
    }

    /**
     * Get APPO part of header.
     * 
     * @param index Index we're at in the image array.
     * 
     * @return Last accessed index.
     */
    private int getAPPO(int index) {
        int length = (image[index++] << 8) | (image[index++] - 2);

        while (length-- > 0) { // skip any remaining data
            index++;
        }

        return index;
    }

    /**
     * Get the Huffman table.
     * 
     * @param index Index we're at in the image array.
     * 
     * @return Last accessed index.
     */
    private int getDHT(int index) {
        int length;

        int i, num, count;

        // length of this element in the header.
        length = (image[index++] << 8) | (image[index++] - 2);

        while (length > 0) {
            num = image[index++];

            if ( (num < 0) || (num >= 4)) {
                MipavUtil.displayError("FileDicomJPEG: Bogus DHT index " + num);

                return -1;
            }

            bits[num][0] = 0;
            count = 0;

            for (i = 1; i <= 16; i++) {
                bits[num][i] = image[index++];
                count += bits[num][i];
            }

            if (count > 256) {
                MipavUtil.displayError("FileDicomJPEG: Bogus DHT counts");

                return -1;
            }

            for (i = 0; i < count; i++) {
                huffval[num][i] = image[index++];
            }

            length = length - 1 - 16 - count;

            if ( (num & 0x10) != 0) { // AC table definition
                Preferences.debug("Huffman table for lossless JPEG is not defined.\n", Preferences.DEBUG_FILEIO);
            }
        }

        return index;
    }

    /**
     * Get DRI part of header.
     * 
     * @param index Index we're at in the image array.
     * 
     * @return Last accessed index.
     */
    private int getDRI(int index) {
        final int length = (image[index++] << 8) | image[index++];

        if (length != 4) {
            Preferences.debug("FileDicomJPEG: Bogus length in Dri.\n", Preferences.DEBUG_FILEIO);

            return -1;
        }

        restartInterval = (image[index++] << 8) | image[index++];

        return index;
    }

    /**
     * Get the SOF part of header.
     * 
     * @param index Index we're at in the image array.
     * 
     * @return Last accessed index.
     */
    private int getSOF(int index) {
        int length;
        short ci;

        length = (image[index++] << 8) | image[index++];
        dataPrecision = image[index++];

        imageHeight = (image[index++] << 8) | image[index++];
        imageWidth = (image[index++] << 8) | image[index++];
        numComponents = image[index++];

        Preferences.debug("Dicom JPEG Image height = " + imageHeight + " Image width = " + imageWidth + "\n", 
        		Preferences.DEBUG_FILEIO);

        if (imageHeight <= 0) {
            Preferences.debug("Setting imageHeight to " + dicomH + ".\n", Preferences.DEBUG_FILEIO);
            imageHeight = dicomH;
        }

        if (imageWidth <= 0) {
            Preferences.debug("Setting imageWidth to " + dicomW + ".\n", Preferences.DEBUG_FILEIO);
            imageWidth = dicomW;
        }

        if (numComponents <= 0) {
            MipavUtil.displayError("FileDicomJPEG: Illegal number of components");

            return -1;
        }

        if ( (dataPrecision < 2) || (dataPrecision > 16)) {
            MipavUtil.displayError("FileDicomJPEG: Unsupported JPEG data precision");

            return -1;
        }

        if (length != ( (numComponents * 3) + 8)) {
            MipavUtil.displayError("FileDicomJPEG: Bogus SOF length");

            return -1;
        }

        for (ci = 0; ci < numComponents; ci++) {
            componentIndex[ci] = ci;
            componentId[ci] = image[index++];

            final int c = image[index++];
            hSampFactor[ci] = (c >> 4) & 15;
            vSampFactor[ci] = (c) & 15;
            index++;
        }

        return index;
    }

    /**
     * Get the SOS part of header.
     * 
     * @param index Index we're at in the image array.
     * 
     * @return Last accessed index.
     */
    private int getSOS(int index) {
        int length = (image[index++] << 8) | image[index++];

        // Get the number of image components.
        compsInScan = image[index++];
        length -= 3;

        if ( (length != ( (compsInScan * 2) + 3)) || (compsInScan < 1) || (compsInScan > 4)) {
            MipavUtil.displayError("FileDicomJPEG: Bogus SOS length");

            return -1;
        }

        int compID, tableIndex, ci;

        for (int i = 0; i < compsInScan; i++) {
            compID = image[index++];
            tableIndex = image[index++];
            length -= 2;

            for (ci = 0; ci < numComponents; ci++) {

                if (compID == componentId[ci]) {
                    break;
                }
            }

            if (ci >= numComponents) {
                MipavUtil.displayError("FileDicomJPEG: Invalid component number in SOS");

                return -1;
            }

            tableNo[ci] = (tableIndex >> 4) & 15;
        }

        // Get the PSV, skip Se, and get the point transform parameter.
        Ss = image[index++];
        index++;

        final int c = image[index++];
        Pt = c & 0x0F;

        return index;
    }

    /**
     * Gets the next marker in the header.
     * 
     * @param index Index that we're at in the image array.
     * 
     * @return Last accessed index.
     */
    private int nextMarker(int index) {
        byte c;

        do {

            // skip any non-FF bytes
            do {
                c = image[index++];
            } while (c != (byte) 0xFF);

            // skip any duplicate FFs without incrementing nbytes, since
            // extra FFs are legal
            do {
                c = image[index++];
            } while (c == (byte) 0xFF);
        } while (c == (byte) 0); // repeat if it was a stuffed FF/00

        return index - 1;
    }

    /**
     * Check for a restart marker & resynchronize decoder. Haven't tested this at all.
     * 
     * @param index Index we're at in the image array.
     * 
     * @return Last accessed index.
     */
    private int processRestart(int index) {

        index = nextMarker(index);

        if (image[index] != (FileDicomJPEG.M_RST0 + nextRestartNum)) {

            // Restart markers are messed up.
            MipavUtil.displayError("FileDicomJPEG: Error while processing restart marker.");

            return -1;
        }

        // Update restart state
        restartRowsToGo = restartInRows;
        nextRestartNum = (nextRestartNum + 1) & 7;

        return index;
    }

    /**
     * Process the header markers.
     * 
     * @param index Index we're at in the image array.
     * 
     * @return Last accessed index.
     */
    private int processTables(int index) {

        while (index != -1) {
            index = nextMarker(index);

            switch (image[index]) {

                case M_SOF0:
                case M_SOF1:
                case M_SOF2:
                case M_SOF3:
                case M_SOF5:
                case M_SOF6:
                case M_SOF7:
                case M_JPG:
                case M_SOF9:
                case M_SOF10:
                case M_SOF11:
                case M_SOF13:
                case M_SOF14:
                case M_SOF15:
                case M_SOI:
                case M_EOI:
                case M_SOS:
                    return (index);

                case M_DHT:
                    index = getDHT(index + 1);
                    break;

                case M_DQT:
                    MipavUtil.displayError("Not a lossless JPEG file");
                    break;

                case M_DRI:
                    index = getDRI(index + 1);
                    break;

                case M_APP0:
                    index = getAPPO(index + 1);
                    break;

                case M_RST0: // these are all parameterless
                case M_RST1:
                case M_RST2:
                case M_RST3:
                case M_RST4:
                case M_RST5:
                case M_RST6:
                case M_RST7:
                case M_TEM:
                    Preferences.debug("FileDicomJPEG Warning: unexpected marker : " + image[index], Preferences.DEBUG_FILEIO);
                    break;

                default: // must be DNL, DHP, EXP, APPn, JPGn, COM, or RESn
                    index = skipVariable(index + 1);
                    break;
            }

        }

        // should never get here; if we did, there was an error in one of the functions
        return -1;
    }

    /**
     * Skip this variable.
     * 
     * @param index Index we're at in the image array.
     * 
     * @return Last accessed index.
     */
    private int skipVariable(int index) {
        int length = (image[index++] << 8) | (image[index++] - 2);

        while (length-- > 0) {
            index++;
        }

        return index;
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * This class reads and packs the bits appropriately for interpreting the huffman codes.
     */
    class HuffTable {

        /** DOCUMENT ME! */
        int bitCount;

        /** DOCUMENT ME! */
        byte[] buffer;

        /** DOCUMENT ME! */
        byte current;

        /** DOCUMENT ME! */
        int index;

        /** DOCUMENT ME! */
        boolean marker;

        /** DOCUMENT ME! */
        boolean padded;

        /** DOCUMENT ME! */
        byte previous;

        /**
         * Creates a new table with the image and the index where we stopped reading the image so far.
         * 
         * @param image Image.
         * @param i Index into image.
         */
        public HuffTable(final byte[] image, final int i) {
            buffer = image;
            index = i;
            bitCount = 8;
            marker = false;
            padded = false;
        }

        /**
         * Returns index that we're at.
         * 
         * @return index
         */
        public int getIndex() {
            return index;
        }

        /**
         * Returns the appropriate value after reading 8 bits and interpreting the Huffman code.
         * 
         * @param tableNum DOCUMENT ME!
         * 
         * @return value to add to predictor.
         */
        public int huffDecode(final int tableNum) {
            int code = 0;
            int s, d;
            // if(image[index] != 0)
            // System.out.println("Index: "+index+"\tImage: "+image[index]);
            // get 8 bits
            for (int i = 0; i < 8; i++) {

                // if necessary, read in the next byte and get some bits from that.
                if (bitCount == 8) {
                    previous = current;
                    current = getNext();
                    bitCount = 0;

                    if (marker == true) {
                        Preferences.debug("returning -1\n", Preferences.DEBUG_FILEIO);

                        return -1;
                    }
                }

                final int temp = (current >>> (7 - bitCount)) & 1; // ??? & 1
                code = (code << 1) | temp;

                bitCount++;
            }

            // numbits is 0 when the size of the code is greater than 8.
            // that happens 3-4% of the time.
            if (numbits[tableNum][code] != 0) {
                // numbits is the number of bits this code is actually supposed to take up
                // so reset bitCount back down.
                bitCount = bitCount - 8 + numbits[tableNum][code];

                // if resetting it means you're back in the previous number, reset the
                // current byte and index count appropriately
                if (bitCount < 0) {
                    current = previous;

                    if (padded) {
                        index -= 2;
                    } else {
                        index--;
                    }

                    bitCount += 8;
                }

                // s is number of bits to read in for the rest.
                s = value[tableNum][code];
            } else {

                // this is rare...need to find out what code is, since it's more than 8 bits
                int k = 8;

                while (code > maxcode[tableNum][k]) {

                    if (bitCount == 8) {
                        previous = current;
                        current = getNext();
                        bitCount = 0;

                        if (marker == true) {
                            Preferences.debug("returning -1\n", Preferences.DEBUG_FILEIO);

                            return -1;
                        }
                    }

                    final int temp = (current >>> (7 - bitCount)) & 1; // >>> but signed
                    code = (code << 1) | temp;
                    k++;
                    bitCount++;

                }

                // With garbage input we may reach the sentinel value k = 17.
                if (k > 16) {
                    MipavUtil.displayError("FileDicomJPEG: Corrupt JPEG data.  Bad Huffman code");
                    Preferences.debug("Code was " + code + "\n", Preferences.DEBUG_FILEIO);
                    s = 0; // fake a zero as the safest result
                } else {

                    // look up value in huffval, the original huffman value array
                    s = huffval[tableNum][valptr[tableNum][k] + ( (code - mincode[tableNum][k]))];
                }
            }
            
            if (s == 16) {
            	return -32768;
            }

            // read in s number of bits and set d to that number
            if (s != 0) {
                d = 0;
                // System.out.println(image[index]+" "+image[index+1]+" "+image[index+2]+" "+image[index+3]);
                // if(s==16)

                // int z=0;
                // boolean isNeverRead = true;
                for (int i = 0; i < s; i++) {

                    //if (bitCount == 8 && i < 9) {
                	if (bitCount == 8) {
                        // if(z==1)
                        // System.out.println("Already Happened: "+s+"\tI: "+i+"\tBitCount: "+bitCount);
                        // isNeverRead = false;
                        // z = 1;
                        previous = current;
                        current = getNext();
                        bitCount = 0;

                        if (marker == true) {
                            Preferences.debug("returning -1\n", Preferences.DEBUG_FILEIO);

                            return -1;
                        }
                    }

                    final int temp = (current >>> (7 - bitCount)) & 1;
                    d = (d << 1) | temp;
                    bitCount++;
                }

                // if d is really a negative number, make it so
                if (s < FileDicomJPEG.extendTest.length) {
                    if (d < FileDicomJPEG.extendTest[s]) {
                        d += FileDicomJPEG.extendOffset[s];
                    }
                } else {
                    if (d < FileDicomJPEG.extendTest[s - 1]) {
                        d += FileDicomJPEG.extendOffset[s - 1];
                    }
                }

            }
            // 0s are quite common. often s = 0, which means add nothing to predictor.
            else {
                d = 0;
            }

            // return value to add to predictor
            // if(d != 0)
            // System.out.println("D: "+d);
            return d;
        }

        /**
         * Returns flag indicating if we've read in a marker. In that case we're off and we need to exit.
         * 
         * @return <code>true</code> if marker has been read in.
         */
        public boolean isMarker() {
            return marker;
        }

        /**
         * Sets index to parameter.
         * 
         * @param i Index to set to.
         */
        public void setIndex(final int i) {
            index = i;
        }

        /**
         * Gets the next byte, skipping over 0s padded in after FF. Sets marker flag if we read in a marker.
         * 
         * @return The next byte.
         */
        private byte getNext() {
            final byte temp = image[index++];

            if (temp == (byte) 0xFF) {
                padded = true;

                final byte temp2 = image[index++];

                if (temp2 != 0) {
                    index -= 2;
                    marker = true;
                }
            } else {
                padded = false;
            }

            return temp;
        }

    }

}
