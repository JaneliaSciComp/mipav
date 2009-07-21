package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.*;


/**
 * DICOM Retrieve SOP Class.
 */
public class DICOM_StdRetrieve extends DICOM_SOP {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Move request object. */
    public DICOM_CRequest cMoveRq;

    /** Move response object. */
    public DICOM_CResponse cMoveRsp;


    /** Uniquely identifies the current C-Move RQ*/
    private int MSG_ID;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * This implements a DICOM Retrieve SOP Class.
     *
     * @param  UID  describes type of retrieve
     */
    public DICOM_StdRetrieve(String UID) {
        this.UID = UID;
        cMoveRq = new DICOM_CRequest(DICOM_Constants.COMMAND_CMoveRQ);
        cMoveRsp = new DICOM_CResponse(DICOM_Constants.COMMAND_CMoveRSP);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns the msgID for the outgoing C-Move Request.
     *
     * @return  msgID uniquely identifies the current C-Move RQ
     */
    public int getMsgID() {
        return (MSG_ID);
    }


    /**
     * Read a C-MOVE request - this is used when acting like server and responding to a move request.
     *
     * @param   pdu       DOCUMENT ME!
     * @param   dco       DOCUMENT ME!
     * @param   storePDU  DOCUMENT ME!
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public void read(DICOM_PDUService pdu, DICOM_Object dco, DICOM_PDUService storePDU) throws DICOM_Exception {
        // MIPAV is not yet a server
    }

    /**
     * Sets the msgID for the outgoing C-Move Request.
     *
     * @param  id  uniquely identifies the current C-Move RQ
     */
    public void setMsgID(int id) {
        MSG_ID = id;
        cMoveRq.setMsgID(MSG_ID);
    }

    /**
     * Transfers a C-MOVE request to a SCP.
     *
     * @param   pdu       PDU object
     * @param   ddo       DICOM data object
     * @param   localAppTitle  Appliation entity title of the local entity
     * @param   dcmMove   DICOM move object - not used here
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public void write(DICOM_PDUService pdu, DICOM_Object ddo, byte[] localAppTitle, DICOM_Move dcmMove)
            throws DICOM_Exception {

        // int i =0;
        int ID;
        byte[] outAETitle = new byte[16];
        DICOM_Object ddoRsp = new DICOM_Object();
        DICOM_Object dco = new DICOM_Object();

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: BEGIN to " + new String(localAppTitle) +
                              ".\n");
        }

        DICOM_Util.clearByteArray(outAETitle);
        DICOM_Util.copyByteArray(outAETitle, localAppTitle);

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: MOVE request.\n");
        }

        pdu.parseDICOMintoBuffer(ddo.copy(), pdu.pDataTF.getVRLinkedBuffer(), pdu.ioBuffer);
        cMoveRq.write(pdu, null, UID, null, ddo, outAETitle);

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: MOVE request completed.\n");
        }

        while (true) {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: PDU read started.\n");
            }

            pdu.readInObject(dco); // in here until images are read

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: PDU read completed.\n");
            }

            cMoveRsp.read(dco, pdu, ddoRsp);

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() +
                                  " DICOM_StdRetrieve.write: MOVE response read completed.\n");
            }

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: " + dco.toString("Command") +
                                  "\n");
                // Preferences.debug( DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: " + dco.toString("Command")
                // + "\n");
            }

            if ((dco.getInt16(DICOM_RTC.DD_Status) == DICOM_Constants.STATUS_MOVE_SUCCESS) ||
                    (dco.getInt16(DICOM_RTC.DD_Status) == DICOM_Constants.STATUS_MOVE_WARNING)) {

                ID = dco.getInt16(DICOM_RTC.DD_MessageIDBeingRespondedTo);

                int process = DICOMDisplayer.getRowFromID(ID);

                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: ID = " + ID +
                                      " DICOM_StdRetrieve process = " + process + "\n");
                }

                if (!DICOMDisplayer.getSucceeded()) {
                    DICOMDisplayer.setMessageType(process, DICOMDisplayer.STATUS);
                    pdu.showMessage("Error");
                    DICOMDisplayer.setMessageType(process, DICOMDisplayer.ERROR);
                    pdu.showMessage("Files not moved");
                    MipavUtil.displayError("Files not moved. Check server and storage destination\nin the Hosts tab.");
                } else {
                    DICOMDisplayer.setMessageType(process, DICOMDisplayer.STATUS);
                    pdu.showMessage("Success");
                }

                break;
            } else {

                if (dco.getInt16(DICOM_RTC.DD_Status) == DICOM_Constants.STATUS_MOVE_PENDING) {

                    // what should I do.
                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: Status = " +
                                          dco.getInt16(DICOM_RTC.DD_Status) + "\n");
                    }

                    long startTime = System.currentTimeMillis();

                    // while (true) {
                    // if (System.currentTimeMillis() - startTime > 90000) break;
                    // }

                    // break;
                }
            }
        }
    }

}
