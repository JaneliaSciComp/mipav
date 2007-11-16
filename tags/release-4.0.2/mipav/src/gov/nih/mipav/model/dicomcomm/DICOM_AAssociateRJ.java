package gov.nih.mipav.model.dicomcomm;


//  Reason 1   = no reason given
//         2   = application context name not supported
//         3   = calling AE title not recognized
//         4-6 = reserved
//         7   = called AE title not recognized
//         8-10  reserved

/**
 * DICOM Association Rejection.
 */
public class DICOM_AAssociateRJ extends DICOM_PDUType {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public byte reason;

    /** DOCUMENT ME! */
    public byte result;

    /** DOCUMENT ME! */
    public byte source;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_AAssociateRJ object.
     */
    public DICOM_AAssociateRJ() {

        itemType = PDUTYPE_AAssociateRJ;
        reason = 1;
        result = 1;
        source = 3;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates size of message.
     *
     * @return  the size of the message = 4 (reserved2 + result + source + reason)
     */
    public int length() {
        return (4);
    }

    /**
     * Reads the body of an association rejection.
     *
     * @param      connection  the I/O Buffer to read from
     *
     * @exception  DICOM_Exception  unknown PDUType
     */
    public void readBody(DICOM_Comms connection) throws DICOM_Exception {
        reserved2 = connection.readByte();
        result = connection.readByte();
        source = connection.readByte();
        reason = connection.readByte();
    }

    /**
     * Writes the body of the association rejection.
     *
     * @param      connection  the I/O Buffer to write to
     *
     * @exception  DICOM_Exception  problem with writing association request
     */
    public void writeBody(DICOM_Comms connection) throws DICOM_Exception {
        connection.writeByte(reserved2);
        connection.writeByte(result);
        connection.writeByte(source);
        connection.writeByte(reason);
        connection.flush();
    }
}
