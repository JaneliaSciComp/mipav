package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.*;


/**
 * This is the DICOM query class that defines functions to compose and send a patient root, patient level DICOM query
 * request to the image file server located in NIH's Clinical Center.
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
public class DICOM_Query extends DICOM_SOP implements Runnable {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** cFind request object. */
    public DICOM_CRequest cFindRq;

    /** CResponse (Find response) object. */
    public DICOM_CResponse cFindRsp;

    /** The DICOM message ID. */
    protected int MSG_ID;

    /** If true then the DICOM query is cancelled. */
    private boolean cancelFlag = false;

    /** The DICOM data object. */
    private DICOM_Object ddo;

    /** Boolean that tells the thread whether or not it should keep going. */
    private boolean keepGoing = true;

    /** The PDU service object. */
    public DICOM_PDUService pdu;

    /** The GUI frame to starting queries. */
    private ViewJFrameDICOMQuery queryFrame;

    /** Indicates the type of query (i.e. STUDY, SERIES ...) */
    private int queryType;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * DICOM_Query constructor.
     */
    public DICOM_Query() {
        UID = DICOM_Constants.UID_StudyRootQuery;
        cFindRq = new DICOM_CRequest(DICOM_Constants.COMMAND_CFindRQ);
        cFindRsp = new DICOM_CResponse(DICOM_Constants.COMMAND_CFindRSP);
    }

    /**
     * DICOMQuery constructor.
     * 
     * @param _queryFrame reference the query frame
     * @param _type type of query (i.e. STUDY, SERIES ...)
     */
    public DICOM_Query(final ViewJFrameDICOMQuery _queryFrame, final int _type) {
        UID = DICOM_Constants.UID_StudyRootQuery;
        queryFrame = _queryFrame;
        queryType = _type;
        cFindRq = new DICOM_CRequest(DICOM_Constants.COMMAND_CFindRQ);
        cFindRsp = new DICOM_CResponse(DICOM_Constants.COMMAND_CFindRSP);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Connects to the server with an Application Entity title provided.
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
     * getMsgID - This returns the msgID for the outgoing C-Move Request.
     * 
     * @return msgID uniquely identifies the current C-Move RQ
     */
    public int getMsgID() {
        return (MSG_ID);
    }

    /**
     * Runs this query in a separate thread. Calls sendQuery.
     */
    public void run() {
        sendQuery(pdu, ddo);
    }

    /**
     * Send a C-CANCEL-FIND-RQ to the Service Class Provider;ie. Image Archive
     * 
     * @param msgID unique identifier for the current message
     * @param pdu instance of PDU_Service containing remote connect info.
     */
    public void sendFindCancelRQ(final int msgID, DICOM_PDUService pdu) {

        final DICOM_Object dco = new DICOM_Object();
        dco.setInt16(DICOM_RTC.DD_CommandField, DICOM_Constants.COMMAND_CFindCancelRQ);
        dco.setInt16(DICOM_RTC.DD_MessageIDBeingRespondedTo, msgID);
        dco.setInt16(DICOM_RTC.DD_DataSetType, DICOM_Constants.DSTYPE_NODATAPRESENT);

        // set the SOP Class UID for the given C-FIND Cancel Request
        // String UID = DICOM.UID_StudyRootRetrieve;

        // for debugging purposes dump the message to the screen
        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug("DICOMQuery.sendFindCancelRQ: " + dco.toString("C-FIND-CANCEL RQ File Dump:"));
        }

        try {
            pdu.write(dco, UID, (byte) 1);
        } catch (final DICOM_Exception e) {
            pdu.close();
            pdu = null;

            return;
        }

        if (pdu != null) {
            pdu.close();
        }

        cancelFlag = true;
    }

    /**
     * Sends patient root query to remote DICOM Q/R server.
     * 
     * @param pdu instance of PDU_Service containing remote connect info.
     * @param ddo patient root query data object
     */
    public void sendQuery(final DICOM_PDUService pdu, final DICOM_Object ddo) {

        cancelFlag = false;

        try {
            // MSG_ID = query.setMsgID();
            // setStudyRootQuery(); // Always ?
            // setPatientRootQuery();

            write(pdu, ddo);
        } catch (final DICOM_Exception e) {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("Error in DICOMQuery.sendQuery " + e);
            }
        }

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug("DICOMQuery.sendQuery: " + pdu.findResults.toString("Display query results") + " \n");
        }

        if (cancelFlag == false) {
            queryFrame.displayQueryResults(queryType);
        }
        pdu.close();
    }

    /**
     * This sets the unique msgID for the outgoing C-Move Request.
     * 
     * @return msgID uniquely identifies the current C-Move RQ
     */
    public int setMsgID() {
        MSG_ID = DICOM_Util.getUniqueOddID16();
        cFindRq.setMsgID(MSG_ID);

        return (MSG_ID);
    }

    /**
     * Sets up the neccessary parameters for sending a query. Must be called before the thread is started.
     * 
     * @param pdu instance of PDU_Service containing remote connect info.
     * @param ddo move request identifier
     */
    public void setParameters(final DICOM_PDUService pdu, final DICOM_Object ddo) {
        this.pdu = pdu;
        this.ddo = ddo;
    }

    /**
     * setPatientRootQuery - sets query to Patient Root.
     */
    public void setPatientRootQuery() {
        UID = DICOM_Constants.UID_PatientRootQuery;
    }

    /**
     * setPatientStudyOnlyQuery - sets query to Patient Study Only.
     */
    public void setPatientStudyOnlyQuery() {
        UID = DICOM_Constants.UID_PatientStudyOnlyQuery;
    }

    /**
     * Sets up the data object (IOD) for a patient root image level C-Find request on key patient ID.
     * 
     * @param patientID key attribute for image level query
     * @param studyInstUID key attribute for image level query
     * @param seriesInstUID key attribute for image level query
     * 
     * @return return patient root image level query data object
     */
    public DICOM_Object setQueryImagesData(final String patientID, final String studyInstUID, final String seriesInstUID) {

        final DICOM_Object qrList = new DICOM_Object();
        setStudyRootQuery();
        qrList.setStr(DICOM_RTC.DD_ContentDate, "");
        qrList.setStr(DICOM_RTC.DD_ContentTime, "");
        qrList.setStr(DICOM_RTC.DD_InstanceNumber, "");
        qrList.setStr(DICOM_RTC.DD_AcquisitionDate, "");
        qrList.setStr(DICOM_RTC.DD_AccessionNumber, "");
        qrList.setStr(DICOM_RTC.DD_SOPInstanceUID, "");
        qrList.setStr(DICOM_RTC.DD_PatientID, patientID);
        qrList.setStr(DICOM_RTC.DD_StudyInstanceUID, studyInstUID);
        qrList.setStr(DICOM_RTC.DD_SeriesInstanceUID, seriesInstUID);
        qrList.setStr(DICOM_RTC.DD_QueryRetrieveLevel, "IMAGE ");

        return (qrList);
    }

    /**
     * Sets up the data object (IOD) for a patient root patient level C-Find request on key patient name.
     * 
     * @param patientName a key attribute for query
     * @param patientID a key attribute for query - can be empty string
     * 
     * @return return patient root patient level query data object
     */
    public DICOM_Object setQueryPatientData(final String patientName, final String patientID) {

        final DICOM_Object qrList = new DICOM_Object();
        setPatientRootQuery();
        qrList.setStr(DICOM_RTC.DD_PatientName, patientName);
        qrList.setStr(DICOM_RTC.DD_PatientID, patientID);
        qrList.setStr(DICOM_RTC.DD_PatientBirthDate, "");
        qrList.setStr(DICOM_RTC.DD_PatientSex, "");
        qrList.setStr(DICOM_RTC.DD_QueryRetrieveLevel, "PATIENT ");

        return (qrList);
    }

    /**
     * Sets up the data object (IOD) for a patient root series level C-Find request on key patient ID.
     * 
     * @param patientID key attribute for study level query
     * @param studyInstUID key attribute for series level query
     * 
     * @return return patient root series level query data object
     */
    public DICOM_Object setQuerySeriesData(final String patientID, final String studyInstUID) {

        final DICOM_Object qrList = new DICOM_Object();
        setStudyRootQuery();
        qrList.setStr(DICOM_RTC.DD_SeriesDate, "");
        qrList.setStr(DICOM_RTC.DD_SeriesTime, "");
        qrList.setStr(DICOM_RTC.DD_SeriesDescription, "");
        qrList.setStr(DICOM_RTC.DD_SeriesInstanceUID, "");
        qrList.setStr(DICOM_RTC.DD_SeriesNumber, "");
        qrList.setStr(DICOM_RTC.DD_Modality, "");
        qrList.setStr(DICOM_RTC.DD_BodyPartExamined, "");
        qrList.setStr(DICOM_RTC.DD_PatientID, patientID);
        qrList.setStr(DICOM_RTC.DD_StudyInstanceUID, studyInstUID);
        qrList.setStr(DICOM_RTC.DD_QueryRetrieveLevel, "SERIES");

        return (qrList);
    }

    /**
     * Sets up the data object (IOD) for a patient root study level C-Find request on key patient ID.
     * 
     * @param patientID key attribute for study level query
     * @param studyID key attribute for study level query/ can be empty string ""
     * 
     * @return return patient root study level query data object
     */
    public DICOM_Object setQueryStudyData(final String patientID, final String studyID) {

        final DICOM_Object qrList = new DICOM_Object();
        setStudyRootQuery();
        qrList.setStr(DICOM_RTC.DD_PatientID, patientID);
        qrList.setStr(DICOM_RTC.DD_StudyID, studyID);
        qrList.setStr(DICOM_RTC.DD_StudyDate, "");
        qrList.setStr(DICOM_RTC.DD_StudyTime, "");
        qrList.setStr(DICOM_RTC.DD_StudyDescription, "");
        qrList.setStr(DICOM_RTC.DD_StudyInstanceUID, "");
        qrList.setStr(DICOM_RTC.DD_ReferringPhysicianName, "");
        qrList.setStr(DICOM_RTC.DD_PhysicianReadingStudy, "");
        qrList.setStr(DICOM_RTC.DD_AccessionNumber, "");
        qrList.setStr(DICOM_RTC.DD_QueryRetrieveLevel, "STUDY ");

        return (qrList);
    }

    /**
     * Sets up the data object (IOD) for a patient root study level C-Find request on key patient ID.
     * 
     * @param patientName DOCUMENT ME!
     * @param patientID key attribute for study level query
     * @param studyID key attribute for study level query/ can be empty string ""
     * @param studyDate DOCUMENT ME!
     * @param referringPhys DOCUMENT ME!
     * 
     * @return return patient root study level query data object
     */
    public DICOM_Object setQueryStudyData(final String patientName, final String patientID, final String studyID,
            final String studyDate, final String referringPhys) {

        final DICOM_Object qrList = new DICOM_Object();
        setStudyRootQuery();
        qrList.setStr(DICOM_RTC.DD_PatientName, patientName);
        qrList.setStr(DICOM_RTC.DD_PatientID, patientID);
        qrList.setStr(DICOM_RTC.DD_PatientBirthDate, "");
        qrList.setStr(DICOM_RTC.DD_PatientSex, "");
        qrList.setStr(DICOM_RTC.DD_StudyID, studyID);
        qrList.setStr(DICOM_RTC.DD_StudyDate, studyDate);
        qrList.setStr(DICOM_RTC.DD_StudyTime, "");
        qrList.setStr(DICOM_RTC.DD_StudyDescription, "");
        qrList.setStr(DICOM_RTC.DD_StudyInstanceUID, "");
        qrList.setStr(DICOM_RTC.DD_ReferringPhysicianName, referringPhys);
        qrList.setStr(DICOM_RTC.DD_PhysicianReadingStudy, "");
        qrList.setStr(DICOM_RTC.DD_AccessionNumber, "");
        qrList.setStr(DICOM_RTC.DD_QueryRetrieveLevel, "STUDY ");

        return (qrList);
    }

    /**
     * Tells the thread to stop (does not call thread.stop(): unsafe).
     */
    public void setStop() {
        this.keepGoing = false;
    }

    /**
     * Sets query to Study Root.
     */
    public void setStudyRootQuery() {
        UID = DICOM_Constants.UID_StudyRootQuery;
    }

    /**
     * Writes a query.
     * 
     * @param pdu instance of PDU_Service
     * @param ddo DOCUMENT ME!
     * 
     * @exception DICOM_Exception anything goes wrong
     */
    public void write(final DICOM_PDUService pdu, final DICOM_Object ddo) throws DICOM_Exception {

        final DICOM_Object ddoRsp = new DICOM_Object();
        final DICOM_Object dco = new DICOM_Object();
        pdu.parseDICOMintoBuffer(ddo.copy(), pdu.pDataTF.getVRLinkedBuffer(), pdu.ioBuffer);
        cFindRq.write(pdu, null, UID, null, ddo, null); // send query
        pdu.addFindResultToList(null, ddo); // indicates start

        while (pdu.readInObject(dco) && keepGoing) {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " DICOM_Query.write: \n");
            }

            cFindRsp.read(dco, pdu, ddoRsp);

            if (dco.getInt16(DICOM_RTC.DD_DataSetType) == DICOM_Constants.DSTYPE_NODATAPRESENT) {
                return;
            }

            pdu.addFindResultToList(dco, ddoRsp);
            dco.clear();
            ddoRsp.clear();
        }
    }

}
