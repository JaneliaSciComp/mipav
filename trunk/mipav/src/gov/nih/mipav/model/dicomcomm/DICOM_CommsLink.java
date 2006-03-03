package gov.nih.mipav.model.dicomcomm;



/**
 * It is a bit odd since it both extends DICOM_Comms and has a DICOM_Comms object.
 * Might be best to redesign this interface structure but it works!
 */

public class DICOM_CommsLink extends DICOM_Comms {

    DICOM_Comms linkToComms = new DICOM_Comms();

    /**
    *   Sets linkToComms to connection and fills(read) the connection buffer via readBinary
    *   @param connection  the connection to the socket
    *   @param length      the number of bytes to read
    */
    public void readFill( DICOM_Comms connection, int length ) throws DICOM_Exception {
        linkToComms = connection;
        readFill( length );
    }

    /**
    *   Copies data from the port buffer into the a new buffer used typically in pDataTF.
    *   @param data  buffer to store the data
    *   @param length number of bytes to be read
    */
    public int readBinary( byte data[], int length ) throws DICOM_Exception {
        linkToComms.read( data, length ); // this eventually finds it way to
                                            // the pdu.sendBinary (pdu is a DICOM buffer)
        return( length );
    }

    /**
    *   Send the data out the connection (pdu)
    *   @param connection the pdu or connection (DICOM_Comms is the parent class of DICOM_PDUService)
    *   @param length
    */
    public void flush( DICOM_Comms connection, int length ) throws DICOM_Exception {
        linkToComms = connection;
        //linkToComms.flush();
        flush(length);
    }

    /**
    *  Actual binary data to be sent
    *  @param data  buffer of data send out the port (socket)
    *  @param length number of bytes to be sent
    */
    public void sendBinary( byte data[], int offset, int length ) throws DICOM_Exception {
        linkToComms.write( data, offset, length );
        linkToComms.flush();
    }

}
