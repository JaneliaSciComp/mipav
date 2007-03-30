package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.*;


/**
 * This is the DICOM verification class that defines functions to perform Verification to a DICOM image file archive
 * such as the Kodak PACS image file server located in NIH's Clinical Center. ("Pings" the server)
 *
 * @version  1.0
 * @author   Sunita Munjal
 * @author   Matthew McAuliffe, Ph.D.
 */
public class DICOM_Verification { // is an SOP {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Echo request object. */
    public DICOM_CRequest cEchoRQ;

    /** Echo response object. */
    public DICOM_CResponse cEchoRSP;

    /** Unique identifier. */
    public String UID = null;

    /** GUI frame where all DICOM messages are displayed. */
    private ViewJFrameDICOMQuery frame;

    /** PDU service object. */
    private DICOM_PDUService pdu;

    /** The remote servers AE title. */
    private String remoteAETitle;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  _remoteAETitle  remote(i.e. server) application entity title
     * @param  _frame          the frame where the update messages will be displayed
     */
    public DICOM_Verification(String _remoteAETitle, ViewJFrameDICOMQuery _frame) {
        remoteAETitle = _remoteAETitle;
        frame = _frame;
        UID = DICOM_Constants.UID_Verification;
        cEchoRQ = new DICOM_CRequest(DICOM_Constants.COMMAND_CEchoRQ);
        cEchoRSP = new DICOM_CResponse(DICOM_Constants.COMMAND_CEchoRSP);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Reads the verification request and responds with (write) a ECHO_SUCESS.
     *
     * @param   pdu  PDU service class
     * @param   dco  DICOM command object
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public void read(DICOM_PDUService pdu, DICOM_Object dco) throws DICOM_Exception {
        Preferences.debug("################### Before verifySOPClass \n");
        verifySOPClass(dco);
        Preferences.debug("################### Before cEchoRQ.read \n");
        cEchoRQ.read(dco);
        Preferences.debug("################### Before cEchoRSP.write \n");
        cEchoRSP.write(pdu, dco, UID, DICOM_Constants.STATUS_ECHO_SUCCESS, null, null, 0, 0, 0, 0);
    }

    /**
     * Initiate verification of connection between client and server (i.e. ping).
     */
    public void verify() {

        try {

            // define an instance of the PDU_Service class to set up the connection to the remote AE
            pdu = new DICOM_PDUService();
            pdu.connectClientToServer(remoteAETitle, true, null, null);
            write(pdu);
            pdu.close();
        } catch (OutOfMemoryError error) {
            frame.appendSendMessage("Failed to connect " + remoteAETitle + "\n");
            displayError("Error: verify():" + error);
            pdu.close();

            return;
        } catch (DICOM_Exception error) {
            frame.appendSendMessage("Failed to connect " + remoteAETitle + "\n");
            displayError("Error: verify():" + error);
            pdu.close();

            return;
        }

        frame.appendSendMessage("Verification to " + remoteAETitle + " - success" + "\n");
    }

    /**
     * Writes the UID_Verification via the PDU service.
     *
     * @param   pdu  PUD service class
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public void write(DICOM_PDUService pdu) throws DICOM_Exception {
        DICOM_Object dco = new DICOM_Object();

        cEchoRQ.setMsgID(DICOM_Util.getUniqueOddID16());
        Preferences.debug("################### Before Verify.write echoRQ \n");
        cEchoRQ.write(pdu, null, UID, null, null, null);
        Preferences.debug("################### Before pdu.read\n");
        pdu.readInObject(dco);
        Preferences.debug("################### cEchoRSP.read\n");
        cEchoRSP.read(dco, pdu, null);
    }


    /**
     * Displays an error in a frame.
     *
     * @param  error  string that is displayed
     */
    private void displayError(String error) {
        MipavUtil.displayError(error);
    }


    /**
     * Verifies the SOP class UID.
     *
     * @param   dco  DICOM data object
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    private void verifySOPClass(DICOM_Object dco) throws DICOM_Exception {
        String dcoUID;

        if (dco == null) {
            throw new DICOM_Exception("DCO is null");
        }

        dcoUID = dco.getStr(DICOM_RTC.DD_AffectedSOPClassUID);

        if (dcoUID == null) {
            throw new DICOM_Exception("SOP class UID is null");
        }

        if (!dcoUID.equals(UID)) {
            throw new DICOM_Exception("Invalid SOP class UID");
        }
    }
}
