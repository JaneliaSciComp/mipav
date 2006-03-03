package gov.nih.mipav.model.dicomcomm;


/**
 * DICOM Association Abort PDU Type
 *
 */
public class DICOM_AAbortRQ extends DICOM_PDUType {

    public byte reason;
    public byte reserved = RESERVED;
    public byte result;
    public byte source;

    /**
    *
    */
    public DICOM_AAbortRQ() {
        reason   = 1;
        itemType = PDUTYPE_AAbortRQ;
        result   = RESERVED; // same as result = 0;
        source   = 3;
    }

    /**
    *   Accessor that returns the length in bytes of the AbortRQ (4)
    *   @return     item length of 4 (i.e. 4 bytes)
    */
    public int length() {
	    // reserved +  result + source + reason = 1 + 1 + 1 + 1 = 4
	    return(4);
    }

    /**
    *   Reads the connection
    *   @param connection the connection to read from
    */
    public void readBody( DICOM_Comms connection ) throws DICOM_Exception {
        reserved = connection.readByte();
        result   = connection.readByte();
        source   = connection.readByte();
        reason   = connection.readByte();
    }

    /**
    *   Writes the PDU type header
    *   @param connection the connection to write to
    */
    public void writeBody( DICOM_Comms connection ) throws DICOM_Exception {
        connection.writeByte(reserved);
        connection.writeByte(result);
        connection.writeByte(source);
        connection.writeByte(reason);
        connection.flush();
    }

}
