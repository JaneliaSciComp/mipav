package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.Preferences;


/**
 * This is the DICOM move class that defines functions to compose and send move requests to the server found in the
 * .preferences file. It runs in a separate thread, so it has its own run method.
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
 * 
 * @author Sunita Munjal
 * @version 1.0
 */
public class DICOM_Move implements Runnable {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Local application entity of the */
    private byte[] localAppTitle;

    /** DICOM data object. */
    private DICOM_Object ddo;

    /** Unique message identifier for the C-MOVE request. */
    private int MSG_ID;

    /** PDU service object. */
    private DICOM_PDUService pdu;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_Move object.
     */
    public DICOM_Move() {}

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * connects to server with the supplied Application entity title.
     * 
     * @param AETitle application entity title for remote server
     * 
     * @return pdu return instance of PDU_Service class which contains the remote system connect info.
     */
    public DICOM_PDUService connectToServer(final String AETitle) {

        pdu = new DICOM_PDUService();

        try {
            pdu.connectClientToServer(AETitle, false, null, null);
        } catch (final DICOM_Exception e) {
            pdu = null;
        }

        return (pdu);
    }

    /**
     * Accessor to get the message ID for the current message.
     * 
     * @return msgID unique identifier for the current message
     */
    public int getMsgID() {
        return (MSG_ID);
    }

    /**
     * Runs this move request in a separate thread. Calls sendMoveRQ.
     */
    public void run() {
        sendMoveRQ(pdu, ddo, localAppTitle);
    }

    /**
     * Sends a C-CANCEL-MOVE-RQ to the Service Class Provider; ie. Image Archive
     * 
     * @param msgID Unique identifier for the C-MOVE request to be cancelled
     * @param pdu Instance of PDU_Service containing remote connect info.
     */
    public void sendCancelRQ(final int msgID, DICOM_PDUService pdu) {

        final DICOM_Object dco = new DICOM_Object();
        dco.setInt16(DICOM_RTC.DD_CommandField, DICOM_Constants.COMMAND_CMoveCancelRQ);
        dco.setInt16(DICOM_RTC.DD_MessageIDBeingRespondedTo, msgID);
        dco.setInt16(DICOM_RTC.DD_DataSetType, DICOM_Constants.DSTYPE_NODATAPRESENT);
        dco.setInt16(DICOM_RTC.DD_Status, DICOM_Constants.STATUS_MOVE_CANCELLED);

        // set the SOP Class UID for the given C-MOVE Cancel Request
        final String uid = DICOM_Constants.UID_PatientRootRetrieve;

        // for debugging purposes dump the message to the debug window
        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug("DICOMMove.sendCancelRQ " + dco.toString("C-Move-Cancel RQ File Dump:") + "\n");
        }

        try {
            pdu.write(dco, uid, (byte) 1);
        } catch (final DICOM_Exception e) {
            pdu.close();
            pdu = null;

            return;
        }

        if (pdu != null) {
            pdu.close();
        }
    }

    /**
     * Sends a move request.
     * 
     * @param pdu Instance of PDU_Service containing remote connect info.
     * @param ddo Move request identifier
     * @param localAppTitle Local application entity title
     */
    public void sendMoveRQ(final DICOM_PDUService pdu, final DICOM_Object ddo, final byte[] localAppTitle) {

        final DICOM_StdRetrieve mrq = new DICOM_StdRetrieve(DICOM_Constants.UID_PatientRootRetrieve);

        try {
            mrq.setMsgID(MSG_ID);

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("DICOMMove.sendMoveRQ: Message ID is: " + MSG_ID + " UID = PatientRootRetrieve \n");
            }

            mrq.write(pdu, ddo, localAppTitle, this);

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("DICOMMove.sendMoveRQ: pdu(" + pdu.hashCode() + ")  Write complete \n");
            }
        } catch (final DICOM_Exception error) {}

        pdu.close();
    }

    /**
     * Sets up the data object (IOD) for a patient root image level C-Move request given patID, study instance UID,
     * series instance UID, and SOP Instance UID.
     * 
     * @param patientID key attribute for image level move request
     * @param studyInstUID key attribute for image level move request
     * @param seriesInstUID key attribute for image level move request
     * @param sopInstUID key attribute for image level move request
     * 
     * @return return patient root image level move data object
     */
    public DICOM_Object setMoveImageData(final String patientID, final String studyInstUID, final String seriesInstUID,
            final String sopInstUID) {

        final DICOM_Object mvData = new DICOM_Object();

        mvData.setStr(DICOM_RTC.DD_PatientID, patientID);
        mvData.setStr(DICOM_RTC.DD_StudyInstanceUID, studyInstUID);
        mvData.setStr(DICOM_RTC.DD_SeriesInstanceUID, seriesInstUID);
        mvData.setStr(DICOM_RTC.DD_SOPInstanceUID, sopInstUID);
        mvData.setStr(DICOM_RTC.DD_QueryRetrieveLevel, "IMAGE");

        return (mvData);
    }

    /**
     * Sets up the neccessary parameters for sending a move request. Must be called before the thread is started.
     * 
     * @param pdu Instance of PDU_Service containing remote connect info.
     * @param ddo Move request identifier
     * @param appTitle Local application entity title
     */
    public void setMoveParameters(final DICOM_PDUService pdu, final DICOM_Object ddo, final byte[] localAppTitle) {
        this.pdu = pdu;
        this.ddo = ddo;
        this.localAppTitle = localAppTitle;
    }

    /**
     * Sets up the data object (IOD) for a patient root patient level C-Move request given patID.
     * 
     * @param patientID key attribute for patient level move request
     * 
     * @return return patient root patient level move data object
     */
    public DICOM_Object setMovePatientData(final String patientID) {

        final DICOM_Object mvData = new DICOM_Object();

        mvData.setStr(DICOM_RTC.DD_PatientID, patientID);
        mvData.setStr(DICOM_RTC.DD_QueryRetrieveLevel, "PATIENT ");

        return (mvData);
    }

    /**
     * Set up the data object (IOD) for a patient root series level C-Move request given patID, study instance UID, and
     * series instance UID.
     * 
     * @param patientID key attribute for series level move request
     * @param studyInstUID key attribute for series level move request
     * @param seriesInstUID key attribute for series level move request
     * 
     * @return return patient root series level move data object
     */
    public DICOM_Object setMoveSeriesData(final String patientID, final String studyInstUID, final String seriesInstUID) {

        final DICOM_Object mvData = new DICOM_Object();

        mvData.setStr(DICOM_RTC.DD_PatientID, patientID);
        mvData.setStr(DICOM_RTC.DD_StudyInstanceUID, studyInstUID);
        mvData.setStr(DICOM_RTC.DD_SeriesInstanceUID, seriesInstUID);
        mvData.setStr(DICOM_RTC.DD_QueryRetrieveLevel, "SERIES");

        return (mvData);
    }

    /**
     * Sets up the data object (IOD) for a patient root study level C-Move request given patID and study instance UID.
     * 
     * @param patientID key attribute for study level move request
     * @param studyInstUID key attribute for study level move request
     * 
     * @return return patient root study level move data object
     */
    public DICOM_Object setMoveStudyData(final String patientID, final String studyInstUID) {

        final DICOM_Object mvData = new DICOM_Object();

        mvData.setStr(DICOM_RTC.DD_PatientID, patientID);
        mvData.setStr(DICOM_RTC.DD_StudyInstanceUID, studyInstUID);
        mvData.setStr(DICOM_RTC.DD_QueryRetrieveLevel, "STUDY ");

        return (mvData);
    }

    /**
     * Sets the msgID for the outgoing C-Move Request.
     * 
     * @return returns the unique message ID
     */
    public int setMsgID() {
        MSG_ID = DICOM_Util.getUniqueOddID16();

        return (MSG_ID);
    }

}
