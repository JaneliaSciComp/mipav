package gov.nih.mipav.model.dicomcomm;


/**
 * a DICOM Storage SOP Class.
 */
public class DICOM_StdStorage extends DICOM_SOP {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Store request object. */
    public DICOM_CRequest cStoreRq;

    /** Store response object. */
    public DICOM_CResponse cStoreRsp;

    //~ Constructors ---------------------------------------------------------------------------------------------------


    /**
     * Creates a new DICOM_StdStorage object.
     */
    public DICOM_StdStorage() {
        cStoreRq = new DICOM_CRequest(DICOM_Constants.COMMAND_CStoreRQ);
        cStoreRsp = new DICOM_CResponse(DICOM_Constants.COMMAND_CStoreRSP);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Does not have functionality yet.
     *
     * @param   pdu  DOCUMENT ME!
     * @param   dco  DOCUMENT ME!
     * @param   ddo  DOCUMENT ME!
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public void read(DICOM_PDUService pdu, DICOM_Object dco, DICOM_Object ddo) throws DICOM_Exception { }

    /**
     * Provides Storage SOP write capability.
     *
     * @param   pdu          PDU service object
     * @param   ddo          DICOM data object
     * @param   transferSyntax     The DICOM transfer syntax.
     * @param   classUID     The class UID.
     * @param   instanceUID  The instance UID.
     *
     * @throws  DICOM_Exception  Indicates error when writing data.
     */
    public void write(DICOM_PDUService pdu, DICOM_Object ddo, String transferSyntax, String classUID, String instanceUID)
            throws DICOM_Exception {

        DICOM_Object dco = new DICOM_Object();
        pdu.parseDICOMintoBuffer(ddo.copy(), pdu.pDataTF.getVRLinkedBuffer(), pdu.ioBuffer);
        cStoreRq.write(pdu, transferSyntax, classUID, instanceUID, ddo, null);
        pdu.readInObject(dco);
        cStoreRsp.read(dco, pdu, null);
    }

}
