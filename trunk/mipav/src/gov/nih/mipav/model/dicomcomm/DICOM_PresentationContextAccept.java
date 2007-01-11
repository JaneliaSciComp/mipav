package gov.nih.mipav.model.dicomcomm;


/**
 * DICOM Presentation Context Acceptance PDU Item Type.
 */
public class DICOM_PresentationContextAccept extends DICOM_PDUItemType {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Presentation Context ID. */
    public byte presentationContextID;

    /** Result default to 2. */
    public byte result = 2; // 2 = no reason

    /** PDU item type transfer syntax. */
    public DICOM_PDUItemType trnSyntax = new DICOM_PDUItemType(PDUTYPE_TransferSyntax);

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_PresentationContextAccept object.
     */
    public DICOM_PresentationContextAccept() {
        super(PDUTYPE_PresentationContextAccept);
        presentationContextID = (byte) DICOM_Util.getUniqueOddID8();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates length of message.
     *
     * @return  the length of the message
     */
    public int length() {
        int length;

        // presentationcontextid + reserved2 + result + reserved3
        length = 4; // 1           +     1     +    1   +     1;
        length += trnSyntax.calcSize();

        return (length);
    }

    /**
     * Reads the body of an accepted presentation context.
     *
     * @param      connection  the I/O Buffer to read from
     *
     * @exception  DICOM_Exception  unknown PDUType
     */
    public void readBody(DICOM_Comms connection) throws DICOM_Exception {
        presentationContextID = connection.readByte();
        reserved2 = connection.readByte();
        result = connection.readByte();
        reserved3 = connection.readByte();

        trnSyntax.read(connection);
    }

    /**
     * Sets the transfer syntax for this object.
     *
     * @param  transferSyntax  DOCUMENT ME!
     */
    public void setTransferSyntax(DICOM_PDUItemType transferSyntax) {
        trnSyntax = transferSyntax;
    }

    /**
     * Writes the body of an accepted presentation context.
     *
     * @param      connection  the I/O Buffer to write to
     *
     * @exception  DICOM_Exception  problem with writing association request
     */
    public void writeBody(DICOM_Comms connection) throws DICOM_Exception {
        connection.writeByte(presentationContextID);
        connection.writeByte(reserved2);
        connection.writeByte(result);
        connection.writeByte(reserved3);

        trnSyntax.write(connection);
        // connection.flush();
    }

}
