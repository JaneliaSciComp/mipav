package gov.nih.mipav.model.file;

/* $Header: /mipav/src/gov/nih/mipav/model/file/TIFFLZWDecoder2.java 1     10/21/04 5:58p Ilb $ */

/*
 * Copyright (c) 1988-1997 Sam Leffler
 * Copyright (c) 1991-1997 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Sam Leffler and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Sam Leffler and Silicon Graphics.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 * OF THIS SOFTWARE.
 */

/*
 * TIFF Library.
 * Rev 5.0 Lempel-Ziv & Welch Compression Support
 *
 * This code is derived from the compress program whose code is
 * derived from software contributed to Berkeley by James A. Woods,
 * derived from original work by Spencer Thomas and Joseph Orost.
 *
 * The original Berkeley copyright notice appears below in its entirety.
 */

/*
 * Copyright (c) 1985, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * James A. Woods, derived from original work by Spencer Thomas
 * and Joseph Orost.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * NB: The 5.0 spec describes a different algorithm than Aldus
 *     implements.  Specifically, Aldus does code length transitions
 *     one code earlier than should be done (for real LZW).
 *     Earlier versions of this library implemented the correct
 *     LZW algorithm, but emitted codes in a bit order opposite
 *     to the TIFF spec.  Thus, to maintain compatibility w/ Aldus
 *     we interpret MSB-LSB ordered codes to be images written w/
 *     old versions of this library, but otherwise adhere to the
 *     Aldus "off by one" algorithm.
 *
 * Future revisions to the TIFF spec are expected to "clarify this issue".
 */


/**
 * A class for performing LZW decoding.
 */
public class TIFFLZWDecoder2 {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    int[] andTable = { 511, 1023, 2047, 4095 };

    /** DOCUMENT ME! */
    int bytePointer, bitPointer;

    /** DOCUMENT ME! */
    byte[] data = null, uncompData;

    /** DOCUMENT ME! */
    int dstIndex;

    /** DOCUMENT ME! */
    int nextBits = 0;

    /** DOCUMENT ME! */
    int nextData = 0;

    /** DOCUMENT ME! */
    int predictor, samplesPerPixel;

    /** DOCUMENT ME! */
    byte[][] stringTable;

    /** DOCUMENT ME! */
    int tableIndex, bitsToGet = 9;

    /** DOCUMENT ME! */
    int w, h;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new TIFFLZWDecoder2 object.
     *
     * @param  w                DOCUMENT ME!
     * @param  predictor        DOCUMENT ME!
     * @param  samplesPerPixel  DOCUMENT ME!
     */
    public TIFFLZWDecoder2(int w, int predictor, int samplesPerPixel) {
        this.w = w;
        this.predictor = predictor;
        this.samplesPerPixel = samplesPerPixel;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Add a new string to the string table.
     *
     * @param  string  DOCUMENT ME!
     */
    public void addStringToTable(byte[] string) {

        // Add this new String to the table
        stringTable[tableIndex++] = string;

        if (tableIndex == 511) {
            bitsToGet = 10;
        } else if (tableIndex == 1023) {
            bitsToGet = 11;
        } else if (tableIndex == 2047) {
            bitsToGet = 12;
        }
    }

    /**
     * Add a new string to the string table.
     *
     * @param  oldString  DOCUMENT ME!
     * @param  newString  DOCUMENT ME!
     */
    public void addStringToTable(byte[] oldString, byte newString) {
        int length = oldString.length;
        byte[] string = new byte[length + 1];
        System.arraycopy(oldString, 0, string, 0, length);
        string[length] = newString;

        // Add this new String to the table
        stringTable[tableIndex++] = string;

        if (tableIndex == 511) {
            bitsToGet = 10;
        } else if (tableIndex == 1023) {
            bitsToGet = 11;
        } else if (tableIndex == 2047) {
            bitsToGet = 12;
        }
    }

    /**
     * Append <code>newString</code> to the end of <code>oldString</code>.
     *
     * @param   oldString  DOCUMENT ME!
     * @param   newString  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public byte[] composeString(byte[] oldString, byte newString) {
        int length = oldString.length;
        byte[] string = new byte[length + 1];
        System.arraycopy(oldString, 0, string, 0, length);
        string[length] = newString;

        return string;
    }

    /**
     * Method to decode LZW compressed data.
     *
     * @param   data        The compressed data.
     * @param   uncompData  Array to return the uncompressed data in.
     * @param   h           The number of rows the compressed data contains.
     *
     * @return  DOCUMENT ME!
     *
     * @throws  UnsupportedOperationException  DOCUMENT ME!
     */
    public byte[] decode(byte[] data, byte[] uncompData, int h) {

        if ((data[0] == (byte) 0x00) && (data[1] == (byte) 0x01)) {
            throw new UnsupportedOperationException("TIFFLZWDecoder0");
        }

        initializeStringTable();

        this.data = data;
        this.h = h;
        this.uncompData = uncompData;

        // Initialize pointers
        bytePointer = 0;
        bitPointer = 0;
        dstIndex = 0;


        nextData = 0;
        nextBits = 0;

        int code, oldCode = 0;
        byte[] string;

        while (((code = getNextCode()) != 257) && (dstIndex < uncompData.length)) {

            if (code == 256) {

                initializeStringTable();
                code = getNextCode();

                if (code == 257) {
                    break;
                }

                writeString(stringTable[code]);
                oldCode = code;

            } else {

                if (code < tableIndex) {

                    string = stringTable[code];

                    writeString(string);
                    addStringToTable(stringTable[oldCode], string[0]);
                    oldCode = code;

                } else {

                    string = stringTable[oldCode];
                    string = composeString(string, string[0]);
                    writeString(string);
                    addStringToTable(string);
                    oldCode = code;
                }

            }

        }

        // Horizontal Differencing Predictor
        if (predictor == 2) {

            int count;

            for (int j = 0; j < h; j++) {

                count = samplesPerPixel * ((j * w) + 1);

                for (int i = samplesPerPixel; i < (w * samplesPerPixel); i++) {

                    uncompData[count] += uncompData[count - samplesPerPixel];
                    count++;
                }
            }
        }

        return uncompData;
    }

    /**
     * Returns the next 9, 10, 11 or 12 bits.
     *
     * @return  DOCUMENT ME!
     */
    public int getNextCode() {

        // Attempt to get the next code. The exception is caught to make
        // this robust to cases wherein the EndOfInformation code has been
        // omitted from a strip. Examples of such cases have been observed
        // in practice.
        try {
            nextData = (nextData << 8) | (data[bytePointer++] & 0xff);
            nextBits += 8;

            if (nextBits < bitsToGet) {
                nextData = (nextData << 8) | (data[bytePointer++] & 0xff);
                nextBits += 8;
            }

            int code = (nextData >> (nextBits - bitsToGet)) & andTable[bitsToGet - 9];
            nextBits -= bitsToGet;

            return code;
        } catch (ArrayIndexOutOfBoundsException e) {

            // Strip not terminated as expected: return EndOfInformation code.
            return 257;
        }
    }


    /**
     * Initialize the string table.
     */
    public void initializeStringTable() {

        stringTable = new byte[4096][];

        for (int i = 0; i < 256; i++) {
            stringTable[i] = new byte[1];
            stringTable[i][0] = (byte) i;
        }

        tableIndex = 258;
        bitsToGet = 9;
    }

    /**
     * Write out the string just uncompressed.
     *
     * @param  string  DOCUMENT ME!
     */
    public void writeString(byte[] string) {

        for (int i = 0; i < string.length; i++) {
            uncompData[dstIndex++] = string[i];
        }
    }
}
