package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.Preferences;


/**
 * DIMSE-C service implementations. Most DIMSE C Service messaging classes only require setting an appropriate DICOM
 * command field.
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
public class DICOM_CRequest {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Flag indicating low priority transfer. */
    protected static int LOW = 2;

    /** Flag indicating medium priority transfer. */
    protected static int MEDIUM = 0;

    /** Flag indicating high priority transfer. */
    protected static int HIGH = 1;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** This field distinguishes the DIMSE-C operation conveyed by this message. */
    int COMMAND = 0;

    /** Message ID - Used to distinguish this message from other messages. */
    private int MSG_ID = 33;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_CRequest object.
     * 
     * @param commandType DICOM.COMMAND_CEchoRQ, or CStoreRQ, CFindRQ, CMoveRQ
     */
    public DICOM_CRequest(final int commandType) {
        COMMAND = commandType;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * This is used to fix the padding (even length) of move destinations for some GE DICOM implementations.
     * 
     * @param str DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public static final String fixMoveDestString(String str) {

        str = str.trim();

        if ( (str.length() % 2) != 0) {
            str += " ";
        }

        return (str);
    }

    /**
     * Accessor to the the message ID.
     * 
     * @return this objects message ID
     */
    public final int getMsgID() {
        return MSG_ID;
    }

    /**
     * A simplified read if no info is necessary.
     * 
     * @param dco the incoming DICOM command message
     * 
     * @exception DICOM_Exception DOCUMENT ME!
     */
    public void read(final DICOM_Object dco) throws DICOM_Exception {
        read(null, dco, null);
    }

    /**
     * Reads a DIMSE-C message.
     * 
     * @param pdu the context pdu
     * @param dco the incoming DICOM message
     * @param ddo store the incoming DICOM data object in this object
     * 
     * @exception DICOM_Exception DOCUMENT ME!
     */
    public void read(final DICOM_PDUService pdu, final DICOM_Object dco, final DICOM_Object ddo) throws DICOM_Exception {

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " CRequest.read: \n");
        }

        if (dco == null) {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " CRequest.read: DCO is null \n");
            }

            throw new DICOM_Exception("DCO is null");
        }

        final int command = dco.getInt16(DICOM_RTC.DD_CommandField);

        if (command != COMMAND) {
            throw new DICOM_Exception(" DIMSE-C Request error : " + DICOM_Util.toHexString((short) command)
                    + " looking for " + DICOM_Util.toHexString(COMMAND));
        }

        final int dataSetType = dco.getInt16(DICOM_RTC.DD_DataSetType);

        if (dataSetType != DICOM_Constants.DSTYPE_NODATAPRESENT) {

            if (ddo == null) {
                throw new DICOM_Exception("DICOM data object is null");
            }

            if (pdu == null) {
                throw new DICOM_Exception("PDU is null");
            }

            pdu.readInObject(ddo);
        } else {

            if (ddo != null) {
                ddo.clear();
            } // Information is not present
        }

        final int status = dco.getInt16(DICOM_RTC.DD_Status);

        if ( (status >= DICOM_Constants.STATUS_ERRORFIRST) && (status <= DICOM_Constants.STATUS_ERRORLAST)) {
            String str1, str2;

            str1 = "Status in error " + DICOM_Util.toHexString((short) COMMAND) + " ["
                    + DICOM_Util.toHexString((short) status) + "] ";
            str2 = dco.getStr(DICOM_RTC.DD_ErrorComment);

            if (str2 != null) {
                str1 = str1 + str2;
            }

            throw new DICOM_Exception(str1);
        }
    }

    /**
     * Reads a DIMSE-C message.
     * 
     * @param pdu the context pdu DICOMClientServer
     * @param dco the incoming DICOM message
     * @param ddo a place to store the incoming DICOM data object or null if not required
     * 
     * @exception DICOM_Exception DOCUMENT ME!
     */
    public void readCResponseAlias(final DICOM_PDUService pdu, final DICOM_Object dco, final DICOM_Object ddo)
            throws DICOM_Exception {

        if (dco == null) {
            throw new DICOM_Exception("null dco");
        }

        final int command = dco.getInt16(DICOM_RTC.DD_CommandField);

        if (command != COMMAND) { // not correct command
            throw new DICOM_Exception("DIMSE-C Request error: " + DICOM_Util.toHexString((short) command)
                    + " looking for " + DICOM_Util.toHexString(COMMAND));
        }

        final int dstype = dco.getInt16(DICOM_RTC.DD_DataSetType);

        if (dstype != DICOM_Constants.DSTYPE_NODATAPRESENT) {

            // data is present
            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("CRequest.readCResponseAlias DCO : Data is present" + "\n");
            }

            if (ddo == null) {
                throw new DICOM_Exception("null ddo");
            }

            if (pdu == null) {
                throw new DICOM_Exception("null pdu");
            }

            pdu.readInObject(ddo);
        } else {

            if (ddo != null) {
                ddo.clear();
            }
        }

        final int status = dco.getInt16(DICOM_RTC.DD_Status);

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {

            if (dco != null) {
                Preferences.debug("CRequest.readCResponseAlias DCO : " + dco.toString("DCO") + "\n");
            }

            if (ddo != null) {
                Preferences.debug("CRequest.readCResponseAlias DDO : " + ddo.toString("DDO") + "\n");
            }
        }

        if ( (status >= DICOM_Constants.STATUS_ERRORFIRST) && (status <= DICOM_Constants.STATUS_ERRORLAST)) {
            String str1, errorComment;

            str1 = "Status error for command  = " + DICOM_Constants.convertCommandToString(COMMAND) + // +DICOM_Util.toHexString(
                                                                                                        // (short)
                                                                                                        // COMMAND ) +
                    " status = [" + DICOM_Util.toHexString((short) status) + "] ";
            errorComment = dco.getStr(DICOM_RTC.DD_ErrorComment); // + dco.getOffending Element;

            // Preferences.debug("CRequest.readCResponseAlias DCO : " + dco.toString("DCO") + "\n");
            // Preferences.debug("CRequest.readCResponseAlias DDO : " + ddo.toString("DDO") + "\n");
            // System.out.println("DD_ErrorComment = " + errorComment);

            if (errorComment != null) {
                str1 = str1 + errorComment + "\n";
            }

            throw new DICOM_Exception(str1);
        }
    }

    /**
     * Sets the message ID to the value given. ID is used to distinguish messages from one another.
     * 
     * @param msgID the new message ID
     */
    public final void setMsgID(final int msgID) {
        MSG_ID = msgID;
    }

    /**
     * Writes a DIMSE-C message.
     * 
     * @param pdu the context pdu DICOMClientServer
     * @param transferSyntax transfer syntax.
     * @param classUID the SOP Class UID.
     * @param instanceUID DOCUMENT ME!
     * @param ddo the outgoing DICOM data object (null if none required)
     * @param AETitle byte array to store the destination Application Entity title for C-Moves
     * 
     * @exception DICOM_Exception DOCUMENT ME!
     */
    public void write(final DICOM_PDUService pdu, String transferSyntax, final String classUID,
            final String instanceUID, final DICOM_Object ddo, final byte[] AETitle) throws DICOM_Exception {

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " CRequest.write: Start \n");
        }

        final DICOM_Object dcor = new DICOM_Object();

        // these are filled in the order of the Part 7 Section 9.1.x charts...
        dcor.setInt16(DICOM_RTC.DD_CommandField, COMMAND);
        dcor.setInt16(DICOM_RTC.DD_MessageID, MSG_ID);

        DICOM_Util.determineSOPClassUIDAndPush(classUID, null, ddo, dcor);

        if ( (COMMAND == DICOM_Constants.COMMAND_CStoreRQ) && (ddo != null)) {

            dcor.setStr(DICOM_RTC.DD_AffectedSOPInstanceUID, instanceUID);
            // copy Affected SOP Instance UID
        } else if ( (COMMAND == DICOM_Constants.COMMAND_CStoreRQ)) {
            final String s = ddo.getStr(DICOM_RTC.DD_SOPInstanceUID);

            if (s != null) {
                dcor.setStr(DICOM_RTC.DD_AffectedSOPInstanceUID, s);
            }
        }

        dcor.setInt16(DICOM_RTC.DD_Priority, DICOM_CRequest.MEDIUM); // See class definition for other priority codes

        if (ddo != null) {
            dcor.setInt16(DICOM_RTC.DD_DataSetType, DICOM_Constants.DSTYPE_DATAPRESENT);
        } else {
            dcor.setInt16(DICOM_RTC.DD_DataSetType, DICOM_Constants.DSTYPE_NODATAPRESENT);
        }

        if (COMMAND == DICOM_Constants.COMMAND_CMoveRQ) {

            if (AETitle == null) {
                throw new DICOM_Exception("AETitle = Null");
            }

            final byte[] serverAETitle = new byte[16];
            DICOM_Util.fillByteArray(serverAETitle, ' ');
            DICOM_Util.copyByteArray(serverAETitle, AETitle);

            dcor.setStr(DICOM_RTC.DD_MoveDestination, DICOM_CRequest.fixMoveDestString(new String(serverAETitle)));
        }

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " CRequest.write DCO: " + dcor.toString("Command") + "  \n");
        }

        pdu.write(dcor, classUID, (byte) 1); // 1 = Message command information

        if (ddo != null) {
            Preferences.debug(DICOM_Util.timeStamper() + " CRequest.write DDO: " + ddo.toString("Data") + "  \n");
            transferSyntax = pdu.getTransferSyntaxID();
            pdu.write(transferSyntax, ddo, classUID, (byte) 0);
        }
    }

}
