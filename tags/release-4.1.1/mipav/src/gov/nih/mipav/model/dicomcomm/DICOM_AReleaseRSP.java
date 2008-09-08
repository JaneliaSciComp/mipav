package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.*;


/**
 * DICOM Association Release Response.
 */
public class DICOM_AReleaseRSP extends DICOM_PDUType {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    byte[] reserved2 = new byte[4];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_AReleaseRSP object.
     */
    public DICOM_AReleaseRSP() {
        itemType = PDUTYPE_AReleaseRSP;
        DICOM_Util.zeroByteArray(reserved2);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates length of message.
     *
     * @return  the length of the message = 4 (reserved2 = 4 bytes)
     */
    public int length() {
        return (4);
    }

    /**
     * Reads the body of an association release response.
     *
     * @param      connection  the I/O Buffer to read from
     *
     * @exception  DICOM_Exception  unknown PDUType
     */
    public void readBody(DICOM_Comms connection) throws DICOM_Exception {

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug("AReleaseRSP.readBody\n");
        }

        connection.read(reserved2);
    }

    /**
     * Writes the body of the association release response.
     *
     * @param      connection  the I/O Buffer to write to
     *
     * @exception  DICOM_Exception  problem with writing association release response
     */
    public void writeBody(DICOM_Comms connection) throws DICOM_Exception {

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug("AReleaseRSP.writeBody\n");
        }

        connection.write(reserved2);
        connection.flush();
    }

}
