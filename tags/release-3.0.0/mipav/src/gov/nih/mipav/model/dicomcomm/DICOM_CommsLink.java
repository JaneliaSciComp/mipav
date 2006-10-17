package gov.nih.mipav.model.dicomcomm;


/**
 * It is a bit odd since it both extends DICOM_Comms and has a DICOM_Comms object. Might be best to redesign this
 * interface structure but it works!
 */

public class DICOM_CommsLink extends DICOM_Comms {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Reference to the link communications object. */
    DICOM_Comms linkToComms = new DICOM_Comms();

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Send the data out the connection (pdu).
     *
     * @param   connection  the PDU or connection (DICOM_Comms is the parent class of DICOM_PDUService)
     * @param   length      The length of data to be flushed out the port.
     *
     * @throws  DICOM_Exception Throws an error when attempting to flush more bytes than are present!
     */
    public void flush(DICOM_Comms connection, int length) throws DICOM_Exception {
        linkToComms = connection;
        flush(length);
    }

    /**
     * Copies data from the port buffer into the a new buffer used typically in pDataTF.
     *
     * @param   data    Buffer to store the data
     * @param   length  Number of bytes to be read
     *
     * @return  The length of the data read in.
     *
     * @throws  DICOM_Exception  Throws an error if there is a problem reading the port.
     */
    public int readBinary(byte[] data, int length) throws DICOM_Exception {
        linkToComms.read(data, length); // this eventually finds it way to
                                        // the pdu.readBinary (pdu is a DICOM buffer)

        return (length);
    }

    /**
     * Sets linkToComms to connection and fills(read) the connection buffer via readBinary.
     *
     * @param   connection  The connection to the socket.
     * @param   length      The number of bytes to read.
     *
     * @throws  DICOM_Exception  Throws an error if there is a problem reading the port.
     */
    public void readFill(DICOM_Comms connection, int length) throws DICOM_Exception {
        linkToComms = connection;
        readFill(length);
    }

    /**
     * Actual binary data to be sent.
     *
     * @param   data    Buffer of data send out the port (socket)
     * @param   offset  The offset into the data indicating the starting point of the data to be sent.
     * @param   length  The number of bytes to be sent.
     *
     * @throws  DICOM_Exception  Throws an error if there is a problem sendting via port.
     */
    public void sendBinary(byte[] data, int offset, int length) throws DICOM_Exception {
        linkToComms.write(data, offset, length);
        linkToComms.flush();
    }

}
