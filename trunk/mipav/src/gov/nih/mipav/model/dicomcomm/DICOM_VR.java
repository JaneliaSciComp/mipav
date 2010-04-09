package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.Preferences;


/**
 * DICOM Value Representation. A VR is composed of 1. group 2. element 3. data
 * 
 * <hr>
 * 
 * This DICOM communication package was originally based on the Java Dicom Package, whose license is below:
 * 
 * <pre>
 * Java Dicom Package (com.zmed.dicom)
 * 
 *  Copyright (c) 1996-1997 Z Medical Imaging Systems, Inc.
 * 
 *  This software is provided, as is, for non-commercial educational
 *  purposes only.   Use or incorporation of this software or derivative
 *  works in commercial applications requires written consent from
 *  Z Medical Imaging Systems, Inc.
 * 
 *  Z MEDICAL IMAGING SYSTEMS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT
 *  THE SUITABILITY OF THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING
 *  BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS
 *  FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT, OR CONFORMANCE TO ANY
 *  SPECIFICATION OR STANDARD.  Z MEDICAL IMAGING SYSTEMS SHALL NOT BE
 *  LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING OR
 *  MODIFYING THIS SOFTWARE OR ITS DERIVATIVES.
 * 
 *  =============================================================================
 * 
 *  This software package is implemented similarly to the UC Davis public
 *  domain C++ DICOM implementation which contains the following copyright
 *  notice:
 * 
 *  Copyright (C) 1995, University of California, Davis
 * 
 *  THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND THE UNIVERSITY
 *  OF CALIFORNIA DOES NOT MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
 *  PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
 *  USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
 *  SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
 *  THE SOFTWARE IS WITH THE USER.
 * 
 *  Copyright of the software and supporting documentation is
 *  owned by the University of California, and free access
 *  is hereby granted as a license to use this software, copy this
 *  software and prepare derivative works based upon this software.
 *  However, any distribution of this software source code or
 *  supporting documentation or derivative works (source code and
 *  supporting documentation) must include this copyright notice.
 * 
 *  The UC Davis C++ source code is publicly available from the following
 *  anonymous ftp site:
 * 
 *  ftp://imrad.ucdmc.ucdavis.edu/pub/dicom/UCDMC/
 * </pre>
 */
public class DICOM_VR {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Actual data array of the the VR. */
    public byte[] data;

    /** Represents the DICOM element number. */
    public int element;

    /** Represents the DICOM group number. */
    public int group;

    /** Inital DICOM type. */
    private int ddType = DICOM_RTC.DD_UNDEFINED;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     */
    public DICOM_VR() {
        initVR(0, 0, null, DICOM_RTC.DD_UNDEFINED);
    }

    /**
     * Creates a new DICOM_VR object.
     * 
     * @param group DICOM group number.
     * @param element DICOM element number.
     * @param data Actual data array of the the VR.
     * @param DDType DOCUMENT ME!
     */
    public DICOM_VR(final int group, final int element, final byte[] data, final int DDType) {
        initVR(group, element, data, DDType);
    }

    /**
     * Creates a new DICOM_VR object.
     * 
     * @param group DICOM group number.
     * @param element DICOM element number.
     * @param strData String representation of the VR.
     * @param DDType Type of group, element tag
     */
    public DICOM_VR(final int group, final int element, final String strData, final int DDType) {
        initVR(group, element, strData.getBytes(), DDType);
    }

    /**
     * Creates a new DICOM_VR object.
     * 
     * @param group DICOM group number.
     * @param element DICOM element number.
     * @param val Value of the VR.
     * @param size DOCUMENT ME!
     */
    public DICOM_VR(final int group, final int element, final int val, final int size) {
        final byte[] data = new byte[size];

        switch (size) {

            case 4:
                DICOM_Comms.int32ToBuffer(data, 0, val, DICOM_Comms.LITTLE_ENDIAN);
                break;

            case 2:
                DICOM_Comms.int16ToBuffer(data, 0, val, DICOM_Comms.LITTLE_ENDIAN);
                break;

            case 1:
                data[0] = (byte) (val & 0xff);
                break;

            default:
                return;
        }

        initVR(group, element, data, DICOM_RTC.DD_UNDEFINED);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Reads a VR from a Buffer (network socket).
     * 
     * @param group the group number
     * @param element the element number
     * @param length the length of the data to read
     * @param vrBuffer the DICOM_Comms to read from
     * 
     * @return a created VR with the data inside
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public static DICOM_VR readData(final int group, final int element, int length, final DICOM_Comms vrBuffer)
            throws DICOM_Exception {
        final DICOM_VR vr = new DICOM_VR();

        vr.group = group;
        vr.element = element;

        if (length < 0) { // Most likely a sequence tag and go find length of sequence.

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("DICOM_VR readData: length = -1 therefore looking for end of sequence. \n");
            }

            length = vrBuffer.peekForEndOfSequence();

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("DICOM_VR readData: after peek - length = " + length + "\n");
            }
        }

        try {

            // Take a look at the adding checking code here !
            vr.data = new byte[length];
            vrBuffer.read(vr.data, length);
        } catch (final NegativeArraySizeException arrexc) {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("DICOM_VR: caught negative array size; no End-of-sequence tag found!\n");
            }

            throw arrexc;
        }

        return (vr);
    }

    /**
     * Private functions to support the toString for debug purposes
     * 
     * @param value DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */

    /**
     * Converts value to an unsigned integer and returns it as a long.
     * 
     * @param value value to be converted to an unsigned short
     * 
     * @return DOCUMENT ME!
     */
    public static final long unsignedInt(final int value) {
        return ( ((long) (value)) & 0xffffffff);
    }

    /**
     * Converts value to an unsigned short and returns it as an integer.
     * 
     * @param value value to be converted to an unsigned short
     * 
     * @return DOCUMENT ME!
     */
    public static final int unsignedShort(final int value) {
        return ( ( (value)) & 0xffff);
    }

    /**
     * Writes a VR to a DICOM_Comms (network socket).
     * 
     * @param vr the VR to write
     * @param vrBuffer the DICOM_Comms to write to
     */
    public static final void writeData(final DICOM_VR vr, final DICOM_Comms vrBuffer) {
        vrBuffer.write(vr.data, 0, vr.data.length);
    }

    /**
     * Writes a VR to a DICOM_Comms (network socket).
     * 
     * @param vr the VR to write
     * @param vrBuffer the DICOM_Comms to write to
     * @param ioBuffer
     */
    public static final void writeDataIn(final DICOM_VR vr, final DICOM_Comms vrBuffer, final DICOM_FileIO ioBuffer) {
        vrBuffer.writeIn(vr.data, 0, vr.data.length, ioBuffer);
    }

    /**
     * Returns a deep copy the VR.
     * 
     * @return DOCUMENT ME!
     */
    public DICOM_VR copy() {
        final DICOM_VR vr = new DICOM_VR(this.group, this.element, this.data, ddType);

        return vr;
    }

    /**
     * Gets the index for this VR.
     * 
     * @return the index
     */
    public final int getDDType() {
        int ddType;

        ddType = this.ddType;

        if (ddType == DICOM_RTC.DD_UNDEFINED) {
            ddType = DICOM_RTC.unknownDDType(group, element);
        }

        return (ddType);
    }

    /**
     * Gets the type code for this VR.
     * 
     * @return the type code
     */
    public final int getTypeCode() {
        return (DICOM_RTC.getTypeCode(getDDType()));
    }

    /**
     * Initializes the VR.
     * 
     * @param group DICOM group number.
     * @param element DICOM element number.
     * @param data the data associated with this VR
     * @param DDType DOCUMENT ME!
     */
    public void initVR(final int group, final int element, final byte[] data, final int DDType) {
        this.group = group;
        this.element = element;
        this.ddType = DDType;

        makeDataEven(data);
    }

    /**
     * Creates a string representation of this VR.
     * 
     * @param str VR information is concatenated to this string
     * 
     * @return the debug string
     */
    public String toString(final String str) {
        int typeCode;
        String typeCodeName;
        String description;
        String strDataLength;
        final int ddType = getDDType();

        typeCode = DICOM_RTC.getTypeCode(ddType);
        typeCodeName = DICOM_RTC.getTypeCodeName(ddType);
        description = DICOM_RTC.getDescription(ddType);

        strDataLength = "                           " + data.length;
        strDataLength = strDataLength.substring(strDataLength.length() - 8);

        return (str + ": VR( " + DICOM_Util.toHexString((short) group) + ", " + DICOM_Util.toHexString((short) element)
                + " ) " + "(" + DICOM_Util.padAndOrTruncate(description, 40) + " ) \t\t\t" + "["
                + DICOM_Util.padAndOrTruncate(typeCodeName, 2) + " ] " + "[" + strDataLength + " ] " + "= " + DICOM_VR
                .dataToString(this, typeCode));
    }

    /**
     * Build a string reprsentation of the Data based on the VR type. Only used by the "toString" of this class.
     * 
     * @param vr the data to converted into a string in this the value representation object
     * @param typeCode type of VR
     * 
     * @return the string representation
     */
    private static String dataToString(final DICOM_VR vr, final int typeCode) {
        int index;
        String str = "";
        byte[] data = vr.data;

        if (data.length > 64) {
            final byte[] newdata = new byte[64];

            for (index = 0; index < 64; index++) {
                newdata[index] = data[index];
            }

            data = newdata;
        }

        if (data.length < 1) {
            str += "(null)";
        } else {

            switch (typeCode) {

                case DICOM_RTC.TYPE_UNKNOWN:
                    if (DICOM_VR.isDataAlpha(data)) {
                        str += "\"" + new String(data) + "\"";
                    } else {

                        for (index = 0; index < data.length; index++) {
                            str += DICOM_Util.toHexString(data[index]) + " ";
                        }
                    }

                    break;

                case DICOM_RTC.TYPE_AE:
                case DICOM_RTC.TYPE_AS:
                case DICOM_RTC.TYPE_CS:
                case DICOM_RTC.TYPE_DA:
                case DICOM_RTC.TYPE_DS:
                case DICOM_RTC.TYPE_IS:
                case DICOM_RTC.TYPE_LO:
                case DICOM_RTC.TYPE_LT:
                case DICOM_RTC.TYPE_PN:
                case DICOM_RTC.TYPE_SH:
                case DICOM_RTC.TYPE_ST:
                case DICOM_RTC.TYPE_TM:
                case DICOM_RTC.TYPE_UI:
                    str += "\"" + new String(data) + "\"";
                    break;

                case DICOM_RTC.TYPE_AT:
                    for (index = 0; index < data.length; index += 4) {
                        str += "(";
                        str += DICOM_Util.toHexString2(data[index]);
                        str += DICOM_Util.toHexString2(data[index + 1]);
                        str += ", ";
                        str += DICOM_Util.toHexString2(data[index + 2]);
                        str += DICOM_Util.toHexString2(data[index + 3]);
                        str += ") ";
                    }

                    break;

                case DICOM_RTC.TYPE_OB:
                    for (index = 0; index < data.length; index++) {
                        str += DICOM_Util.toHexString(data[index]);
                        str += " ";
                    }

                    break;

                case DICOM_RTC.TYPE_OW:
                    for (index = 0; index < data.length; index += 2) {
                        str += DICOM_Util.toHexString(data[index]);
                        str += DICOM_Util.toHexString(data[index + 1]);
                        str += " ";
                    }

                    break;

                case DICOM_RTC.TYPE_SQ:
                    str += "Sequence Item\n";
                    break;

                case DICOM_RTC.TYPE_UL:
                    for (index = 0; index < data.length; index += 4) {
                        str += DICOM_VR.unsignedInt(DICOM_Comms.bufferToInt32(data, index, DICOM_Comms.LITTLE_ENDIAN));
                        str += " ";
                    }

                    break;

                case DICOM_RTC.TYPE_US:

                    // case DICOM_RTC.TYPE_US_or_SS:
                    // case DICOM_RTC.TYPE_US_US_or_SS_US:
                    // case DICOM_RTC.TYPE_US_or_SS_or_OW:
                    if ( (vr.group == 0x0000)
                            && ( (vr.element == 0x0100) || (vr.element == 0x0800) || (vr.element == 0x0900))) {

                        for (index = 0; index < data.length; index += 2) {
                            str += DICOM_Util.toHexString((short) DICOM_Comms.bufferToInt16(data, index,
                                    DICOM_Comms.LITTLE_ENDIAN));
                            str += "h ";
                        }
                    } else {

                        for (index = 0; index < data.length; index += 2) {
                            str += DICOM_VR.unsignedShort(DICOM_Comms.bufferToInt16(data, index,
                                    DICOM_Comms.LITTLE_ENDIAN));
                            str += " ";
                        }
                    }

                    break;

                case DICOM_RTC.TYPE_SS:
                    for (index = 0; index < data.length; index += 2) {
                        str += (short) DICOM_Comms.bufferToInt16(data, index, DICOM_Comms.LITTLE_ENDIAN);
                        str += " ";
                    }

                    break;

                case DICOM_RTC.TYPE_SL:
                    for (index = 0; index < data.length; index += 4) {
                        str += DICOM_Comms.bufferToInt16(data, index, DICOM_Comms.LITTLE_ENDIAN);
                        str += " ";
                    }

                    break;

                case DICOM_RTC.TYPE_FD:
                default:
                    str = "-?-";
                    break;
            }
        }

        if (str.length() > 64) {
            str = str.substring(1, 64) + " ...";
        }

        return (str.trim());
    }

    /**
     * Determines if a byte is an alpha character.
     * 
     * @param ch character to be tested
     * 
     * @return true if byte is an alpha character else false
     */
    private static boolean isAlpha(final byte ch) {

        if ( ( (ch >= ' ') && (ch <= 125)) || (ch == 0)) {
            return (true);
        }

        return (false);
    }

    /**
     * Determines if data byte array is a string of characters.
     * 
     * @param data array of bytes
     * 
     * @return if all bytes in data are alpha characters return true.
     */
    private static boolean isDataAlpha(final byte[] data) {
        int size = data.length;

        if (size > 16) {
            size = 16;
        }

        if (size < 1) {
            return (false);
        }

        while (size-- > 0) {

            if ( !DICOM_VR.isAlpha(data[size])) {
                return (false);
            }
        }

        return (true);
    }

    /**
     * Make the length of the data buffer even by adding a space or null.
     * 
     * @param _data Array that will be made to have an even length.
     */
    private void makeDataEven(final byte[] _data) {
        int index;
        // \\00;
        // String nullStr = new String("z");
        // String spaceStr = new String("");

        if (_data == null) {
            return;
        }

        if ( (_data.length % 2) == 0) {
            data = _data;

            return;
        } else {
            final byte[] newData = new byte[_data.length + 1];

            for (index = 0; index < _data.length; index++) {
                newData[index] = _data[index];
            }

            final int typeCode = DICOM_RTC.getTypeCode(ddType);

            switch (typeCode) {

                case DICOM_RTC.TYPE_UNKNOWN:
                    newData[newData.length - 1] = 32; // spaceStr.getBytes()[0]; // Adds space to end of byte
                    break;

                case DICOM_RTC.TYPE_OB:
                case DICOM_RTC.TYPE_UI:
                case DICOM_RTC.TYPE_DT:

                    // System.out.println("adsfasdfas = " + nullStr );
                    newData[newData.length - 1] = 0; // hopefully null
                    break;

                case DICOM_RTC.TYPE_AE:
                case DICOM_RTC.TYPE_AS:
                case DICOM_RTC.TYPE_CS:
                case DICOM_RTC.TYPE_DA:
                case DICOM_RTC.TYPE_DS:
                case DICOM_RTC.TYPE_IS:
                case DICOM_RTC.TYPE_LO:
                case DICOM_RTC.TYPE_LT:
                case DICOM_RTC.TYPE_PN:
                case DICOM_RTC.TYPE_SH:
                case DICOM_RTC.TYPE_ST:
                case DICOM_RTC.TYPE_TM:
                    newData[newData.length - 1] = 32; // Adds space to end of byte
                    break;

            }

            data = newData;
        }
    }

}
