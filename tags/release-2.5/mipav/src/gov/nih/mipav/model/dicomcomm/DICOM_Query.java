package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.*;


/**
 * This is the DICOM query class that defines functions to compose and send a patient root, patient level DICOM query
 * request to the image file server located in NIH's Clinical Center.
 *
 * @author   Sunita Munjal
 * @version  1.0
 */
public class DICOM_Query extends DICOM_SOP implements Runnable {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public DICOM_CRequest cFindRq;

    /** DOCUMENT ME! */
    public DICOM_CResponse cFindRsp;

    /** DOCUMENT ME! */
    protected int MSG_ID;

    /** DOCUMENT ME! */
    private boolean cancelFlag = false;

    /** DOCUMENT ME! */
    private DICOM_Object ddo;

    /** boolean that tells the thread whether or not it should keep going. */
    private boolean keepGoing = true;

    /** DOCUMENT ME! */
    private DICOM_PDUService pdu;

    /** DOCUMENT ME! */
    private ViewJFrameDICOMQuery queryFrame;

    /** DOCUMENT ME! */
    private int queryType;

    //~ Constructors ---------------------------------------------------------------------------------------------------

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
     * @param  _queryFrame  reference the query frame
     * @param  _type        type of query (i.e. STUDY, SERIES ...)
     */
    public DICOM_Query(ViewJFrameDICOMQuery _queryFrame, int _type) {
        UID = DICOM_Constants.UID_StudyRootQuery;
        queryFrame = _queryFrame;
        queryType = _type;
        cFindRq = new DICOM_CRequest(DICOM_Constants.COMMAND_CFindRQ);
        cFindRsp = new DICOM_CResponse(DICOM_Constants.COMMAND_CFindRSP);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Connects to the server with an Application Entity title provided.
     *
     * @param   AETitle  application entity title for remote server
     *
     * @return  pdu return instance of PDU_Service class which contains the remote system connect info.
     */
    public DICOM_PDUService connectToServer(String AETitle) {

        pdu = new DICOM_PDUService();

        try {
            pdu.connectClientToServer(AETitle, false);
        } catch (DICOM_Exception e) {
            pdu = null;
        }

        return (pdu);
    }

    /**
     * getMsgID - This returns the msgID for the outgoing C-Move Request.
     *
     * @return  msgID uniquely identifies the current C-Move RQ
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
     * @param  msgID  unique identifier for the current message
     * @param  pdu    instance of PDU_Service containing remote connect info.
     */
    public void sendFindCancelRQ(int msgID, DICOM_PDUService pdu) {

        DICOM_Object dco = new DICOM_Object();
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
        } catch (DICOM_Exception e) {
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
     * @param  pdu  instance of PDU_Service containing remote connect info.
     * @param  ddo  patient root query data object
     */
    public void sendQuery(DICOM_PDUService pdu, DICOM_Object ddo) {

        cancelFlag = false;

        try {
            // MSG_ID = query.setMsgID();
            // setStudyRootQuery(); // Always ?
            // setPatientRootQuery();

            write(pdu, ddo);
        } catch (DICOM_Exception e) {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("Error in DICOMQuery.sendQuery " + e);
            }
        }

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug("DICOMQuery.sendQuery: " + pdu.findResults.toString("Display query results") + " \n");
        }

        pdu.close();

        if (cancelFlag == false) {
            queryFrame.displayQueryResults(queryType);
        }
    }

    /**
     * This sets the unique msgID for the outgoing C-Move Request.
     *
     * @return  msgID uniquely identifies the current C-Move RQ
     */
    public int setMsgID() {
        MSG_ID = DICOM_Util.getUniqueOddID16();
        cFindRq.setMsgID(MSG_ID);

        return (MSG_ID);
    }

    /**
     * Sets up the neccessary parameters for sending a query. Must be called before the thread is started.
     *
     * @param  pdu  instance of PDU_Service containing remote connect info.
     * @param  ddo  move request identifier
     */
    public void setParameters(DICOM_PDUService pdu, DICOM_Object ddo) {
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
     * @param   patientID      key attribute for image level query
     * @param   studyInstUID   key attribute for image level query
     * @param   seriesInstUID  key attribute for image level query
     *
     * @return  return patient root image level query data object
     */
    public DICOM_Object setQueryImagesData(String patientID, String studyInstUID, String seriesInstUID) {

        DICOM_Object qrList = new DICOM_Object();
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
     * @param   patientName  a key attribute for query
     * @param   patientID    a key attribute for query - can be empty string
     *
     * @return  return patient root patient level query data object
     */
    public DICOM_Object setQueryPatientData(String patientName, String patientID) {

        DICOM_Object qrList = new DICOM_Object();
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
     * @param   patientID     key attribute for study level query
     * @param   studyInstUID  key attribute for series level query
     *
     * @return  return patient root series level query data object
     */
    public DICOM_Object setQuerySeriesData(String patientID, String studyInstUID) {

        DICOM_Object qrList = new DICOM_Object();
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
     * @param   patientID  key attribute for study level query
     * @param   studyID    key attribute for study level query/ can be empty string ""
     *
     * @return  return patient root study level query data object
     */
    public DICOM_Object setQueryStudyData(String patientID, String studyID) {

        DICOM_Object qrList = new DICOM_Object();
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
     * @param   patientName    DOCUMENT ME!
     * @param   patientID      key attribute for study level query
     * @param   studyID        key attribute for study level query/ can be empty string ""
     * @param   studyDate      DOCUMENT ME!
     * @param   referringPhys  DOCUMENT ME!
     *
     * @return  return patient root study level query data object
     */
    public DICOM_Object setQueryStudyData(String patientName, String patientID, String studyID, String studyDate,
                                          String referringPhys) {

        DICOM_Object qrList = new DICOM_Object();
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
     * @param      pdu  instance of PDU_Service
     * @param      ddo  DOCUMENT ME!
     *
     * @exception  DICOM_Exception  anything goes wrong
     */
    public void write(DICOM_PDUService pdu, DICOM_Object ddo) throws DICOM_Exception {

        DICOM_Object ddoRsp = new DICOM_Object();
        DICOM_Object dco = new DICOM_Object();

        cFindRq.write(pdu, UID, null, ddo, null); // send query
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
