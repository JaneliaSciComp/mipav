package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.*;


/**
 * This is the base class for DIMSE-C response messages.
 */

public class DICOM_CResponse extends DICOM_CRequest {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_CResponse object.
     *
     * @param  commandType  DICOM.COMMAND_CEchoRSP, or CStoreRSP, CFindRSP, CMoveRSP
     */
    public DICOM_CResponse(int commandType) {
        super(commandType);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Reads a DIMSE-C message.
     *
     * @param      dco  the incoming DICOM message
     * @param      pdu  the context pdu DICOMClientServer
     * @param      ddo  a place to store the incoming DICOM data object or "null" if not required
     *
     * @exception  DICOM_Exception  if anything goes wrong
     */
    public void read(DICOM_Object dco, DICOM_PDUService pdu, DICOM_Object ddo) throws DICOM_Exception {

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " CResponse.read: \n");
        }

        super.readCResponseAlias(pdu, dco, ddo);
    }


    /**
     * Writes a DIMSE-C message.
     *
     * @param      pdu         context pdu DICOMClientServer
     * @param      dco         data command object
     * @param      UID         SOP Class UID null for C-Stores
     * @param      status      status value to be sent
     * @param      errComment  string to send error
     * @param      ddo         the outgoing DICOM data object or <code>null</code> if none required
     * @param      nRemaining  number of C-Stores remaining for a C-Move operation
     * @param      nComplete   number of C-Stores which have been completed successfully
     * @param      nFailed     number of C-Stores which have failed
     * @param      nWarning    number of C-Stores which have completed with a warning
     *
     * @exception  DICOM_Exception  indicates an error
     */
    public void write(DICOM_PDUService pdu, DICOM_Object dco, String UID, int status, String errComment,
                      DICOM_Object ddo, int nRemaining, int nComplete, int nFailed, int nWarning)
            throws DICOM_Exception {

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " CResponse.write: Start \n");
        }

        DICOM_Object dcoRsp = new DICOM_Object();
        DICOM_VR vr;

        dcoRsp.setInt16(DICOM_RTC.DD_CommandField, COMMAND);

        DICOM_Util.determineSOPClassUIDAndPush(UID, dco, ddo, dcoRsp);

        while ((vr = dco.pop()) != null) {

            if (vr.group == 0x0000) {

                switch (vr.element) {

                    case 0x0010:
                        dcoRsp.push(vr);
                        break;

                    case 0x0110:
                        vr.element = 0x0120;
                        dcoRsp.push(vr);
                        break;

                    case 0x0200:
                        vr.element = 0x0300;
                        dcoRsp.push(vr);
                        break;

                    case 0x0300:
                        vr.element = 0x0200;
                        dcoRsp.push(vr);
                        break;

                    case 0x1000:
                        dcoRsp.push(vr);
                        break;
                }
            }
        }

        if (ddo != null) {
            dcoRsp.setInt16(DICOM_RTC.DD_DataSetType, DICOM_Constants.DSTYPE_DATAPRESENT);
        } else {
            dcoRsp.setInt16(DICOM_RTC.DD_DataSetType, DICOM_Constants.DSTYPE_NODATAPRESENT);
        }

        dcoRsp.setInt16(DICOM_RTC.DD_MessageID, DICOM_Util.getUniqueOddID16());
        dcoRsp.setInt16(DICOM_RTC.DD_Priority, MEDIUM);
        dcoRsp.setInt16(DICOM_RTC.DD_Status, status);

        if (errComment != null) {
            dcoRsp.setStr(DICOM_RTC.DD_ErrorComment, errComment);
        }

        if (COMMAND == DICOM_Constants.COMMAND_CMoveRSP) {
            dcoRsp.setInt16(DICOM_RTC.DD_NumberOfCompletedSuboperations, nComplete);
            dcoRsp.setInt16(DICOM_RTC.DD_NumberOfRemainingSuboperations, nRemaining);
            dcoRsp.setInt16(DICOM_RTC.DD_NumberOfFailedSuboperations, nFailed);
            dcoRsp.setInt16(DICOM_RTC.DD_NumberOfWarningSuboperations, nWarning);
        }
        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " CResponse.write: Sending command RSP \n");
        }
        pdu.write(dcoRsp, UID, (byte) 1);

        if (ddo != null) {
            pdu.write(ddo, UID, (byte) 0);
        }
    }

}
