package gov.nih.mipav.model.dicomcomm;


import java.util.*;


/**
 * Static DICOM and general utilities to support DICOM interfacing.
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
public class DICOM_Util {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    // *******************************************************************************
    //
    // Simple unique number accessors
    //
    // *******************************************************************************

    public static int uniqueID = 1;

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Pads zeros to a string representation of a number.
     * 
     * @param value string value to be padded
     * @param length total length of string
     * 
     * @return a string with zero padding if necessary
     */
    public static final String addLeadingZeros(final String value, final int length) {
        final String str = "00000000000" + value;

        return (str.substring(str.length() - length));
    }

    /**
     * Pads zeros to a number.
     * 
     * @param value value to be padded
     * @param length total length of string
     * 
     * @return a string with zero padding if necessary
     */
    public static final String addLeadingZeros(final int value, final int length) {
        final String str = "0000000000000000000" + value;

        return (str.substring(str.length() - length));
    }

    /**
     * Simple routine to fill a byte array with ' '
     * 
     * @param byteArray array to be filled
     */
    public static final void clearByteArray(final byte[] byteArray) {

        for (int i = 0; i < byteArray.length; i++) {
            byteArray[i] = (byte) (' ');
        }
    }

    /**
     * Copies the source byte array into the destination byte array.
     * 
     * @param dest destintation array
     * @param src source array
     */
    public static final void copyByteArray(final byte[] dest, final byte[] src) {
        System.arraycopy(src, 0, dest, 0, (dest.length < src.length) ? dest.length : src.length);
    }

    /**
     * Determines relevant SOP Class UID for a storage operation.
     * 
     * @param dco the incoming DICOM command message
     * @param ddo the incoming DICOM data object
     * 
     * @return the UID for the SOP class
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public static String determineSOPClassUID(final DICOM_Object dco, final DICOM_Object ddo) throws DICOM_Exception {
        String UID = null;
        String modality = null;

        // acquire the SOPClassUID from the DCO
        if ( (dco != null) && (UID == null)) {
            UID = dco.getStr(DICOM_RTC.DD_AffectedSOPClassUID);
        }

        // acquire the SOPClassUID from the DDO
        if ( (ddo != null) && (UID == null)) {
            UID = ddo.getStr(DICOM_RTC.DD_SOPClassUID);
        }

        // determine the SOPClassUID based on the DDO modality
        if ( (ddo != null) && (UID == null)) {
            modality = ddo.getStr(DICOM_RTC.DD_Modality);

            // See PS 3.3 - 1998 page 111 section C.7.3.1.1.1
            if (modality != null) {

                if (modality.equals("CT")) {
                    UID = DICOM_Constants.UID_CTStorage;
                } else if (modality.equals("CR")) {
                    UID = DICOM_Constants.UID_CRStorage;
                } else if (modality.equals("MR")) {
                    UID = DICOM_Constants.UID_MRStorage;
                } else if (modality.equals("NM")) {
                    UID = DICOM_Constants.UID_NMStorage;
                } else if (modality.equals("PT")) {
                    UID = DICOM_Constants.UID_PetStorage;
                } else if (modality.equals("US")) {
                    UID = DICOM_Constants.UID_USStorage;
                } else if (modality.equals("XA")) {
                    UID = DICOM_Constants.UID_XRayAngioStorage;
                } else if (modality.equals("XF")) {
                    UID = DICOM_Constants.UID_XRayFluoroStorage;
                } else {
                    UID = DICOM_Constants.UID_SCStorage;
                }
            }
        }

        if (UID == null) {
            throw new DICOM_Exception("Can not determine storage SOP Class UID.");
        }

        return (UID);
    }

    /**
     * Puts the required SOP class UID into the DICOMObject (Command).
     * 
     * @param UID UID (null if unknown)
     * @param dco incoming DICOM command message to which we are responding
     * @param ddo incoming DICOM data object to which we are sending with dcor
     * @param dcor outgoing DICOM command message (the one to push the UID onto )
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public static void determineSOPClassUIDAndPush(String UID, final DICOM_Object dco, final DICOM_Object ddo,
            final DICOM_Object dcor) throws DICOM_Exception {

        if (UID == null) {
            UID = DICOM_Util.determineSOPClassUID(dco, ddo);
        }

        dcor.setStr(DICOM_RTC.DD_AffectedSOPClassUID, UID);
    }

    /**
     * Fills a byte array with some "char" value.
     * 
     * @param byteArray array to be filled
     * @param charValue value to fill the array (converted to byte)
     */
    public static final void fillByteArray(final byte[] byteArray, final char charValue) {

        for (int i = 0; i < byteArray.length; i++) {
            byteArray[i] = (byte) charValue;
        }
    }

    /**
     * Returns a unique number for DICOM communiciton.
     * 
     * @return a positive unique number
     */
    public static final int getUniqueID() {
        return (DICOM_Util.uniqueID++);
    }

    /**
     * Returns a unique number for DICOM communiciton.
     * 
     * @return a positive 16 bit unique number
     */
    public static final int getUniqueID16() {
        return (DICOM_Util.getUniqueID() & 0xffff);
    }

    /**
     * Returns a unique number for DICOM communiciton.
     * 
     * @return a positive 8 bit unique number
     */
    public static final int getUniqueID8() {
        return (DICOM_Util.getUniqueID() & 0xff);
    }

    /**
     * Returns an ODD unique number for DICOM communiciton.
     * 
     * @return an ODD, positive, 8 bit unique number
     */
    public static final int getUniqueOddID() {
        int oddValue;

        do {
            oddValue = DICOM_Util.getUniqueID();
        } while ( (oddValue % 2) == 0);

        return (oddValue);
    }

    /**
     * Returns an ODD unique number for DICOM communiciton.
     * 
     * @return an ODD, positive, 16 bit unique number
     */
    public static final int getUniqueOddID16() {
        return (DICOM_Util.getUniqueOddID() & 0xffff);
    }

    /**
     * Returns an ODD unique number for DICOM communiciton.
     * 
     * @return an ODD, positive, 8 bit unique number
     */
    public static final int getUniqueOddID8() {
        return (DICOM_Util.getUniqueOddID() & 0xff);
    }

    /**
     * Pads with spaces and/or truncates string.
     * 
     * @param str string to be modified
     * @param length length to modify to string to
     * 
     * @return the modified string
     */
    public static final String padAndOrTruncate(String str, final int length) {

        if (str == null) {
            return (null);
        }

        // Adds (pads) spaces to the end of the string to make them all the same length
        while (str.length() < length) {
            str = str + " ";
        }

        // Truncates the string to make them all the same length
        if (str.length() > length) {
            str = str.substring(0, length);
        }

        return (str);
    }

    /**
     * Creates a string in the form of the time determined when this method is called.
     * 
     * @return the time in the form a String
     */
    public static final String timeStamper() {
        String timeStamp;
        final GregorianCalendar cal = new GregorianCalendar();

        timeStamp = "[" + DICOM_Util.addLeadingZeros(cal.get(Calendar.MONTH) + 1, 2) + "/"
                + DICOM_Util.addLeadingZeros(cal.get(Calendar.DAY_OF_MONTH), 2) + "/"
                + DICOM_Util.addLeadingZeros(cal.get(Calendar.YEAR), 2) + " "
                + DICOM_Util.addLeadingZeros(cal.get(Calendar.HOUR_OF_DAY), 2) + ":"
                + DICOM_Util.addLeadingZeros(cal.get(Calendar.MINUTE), 2) + ":"
                + DICOM_Util.addLeadingZeros(cal.get(Calendar.SECOND), 2) + "."
                + DICOM_Util.addLeadingZeros(cal.get(Calendar.MILLISECOND), 3) + "]";

        return (timeStamp);
    }

    /**
     * Converts an integer to a hex string of length 4 with leading zeros.
     * 
     * @param data integer to be converted
     * 
     * @return the new string in Hex format of the supplied integer value
     */
    public static final String toHexString(final int data) {
        return (DICOM_Util.addLeadingZeros(Integer.toHexString(data), 4));
    }

    /**
     * Converts an integer to a hex string of length 2 with leading zeros.
     * 
     * @param data integer to be converted
     * 
     * @return the new string in Hex format of the supplied integer value
     */
    public static final String toHexString2(final int data) {
        return (DICOM_Util.addLeadingZeros(Integer.toHexString(data), 2));
    }

    /**
     * Trims the numbers and special character from the file name.
     * 
     * @param str DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public static final String trimIgnorableChar(final String str) {

        char ch;
        final int length = str.length();

        ch = str.charAt(length - 1);

        if (Character.isIdentifierIgnorable(ch)) {
            return str.substring(0, length - 1);
        } else {
            return str;
        }

    }

    /**
     * Trims string to length.
     * 
     * @param str string to be trimmed
     * @param length length to trim string
     * 
     * @return the trimmed string to length
     */
    public static final String truncate(String str, final int length) {

        if (str == null) {
            return (str);
        }

        if (str.length() > length) {
            str = str.substring(0, length);
        }

        return (str);
    }

    /**
     * Unpads a DICOM ASCII string. It truncates an odd lengthed byte array which ends in a space or a null and returns
     * a String
     * 
     * @param byteArray ASCII byte array to be converted to a String
     * 
     * @return the unpadded String
     */
    public static String unpadStringVal(final byte[] byteArray) {
        String str = null;

        if (byteArray.length > 0) {

            if ( (byteArray.length % 2) == 0) { // even but possibly padded

                if ( (byteArray[byteArray.length - 1] == 0) || (byteArray[byteArray.length - 1] == ' ')) { // data
                    // padded
                    // with a
                    // zero or
                    // space
                    str = new String(byteArray, 0, byteArray.length - 1);
                }
            }
        }

        if (str == null) {
            str = new String(byteArray);
        }

        return (str);
    }

    /**
     * Fills a byte array with some zeros.
     * 
     * @param byteArray array to be filled
     */
    public static final void zeroByteArray(final byte[] byteArray) {

        for (int i = 0; i < byteArray.length; i++) {
            byteArray[i] = 0;
        }
    }

}
