package gov.nih.mipav.model.dicomcomm;


import java.util.*;


/**
 * DICOM Presentation Context PDU Item Type.
 */
public class DICOM_PresentationContext extends DICOM_PDUItemType {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Abstract syntax object. */
    public DICOM_PDUItemType absSyntax = new DICOM_PDUItemType(PDUTYPE_AbstractSyntax);

    /** The ID for the presentation context. */
    public byte presentationContextID;

    /** List of transfer syntaxes approiate for the abstract syntax. */
    public Vector trnSyntax = new Vector();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_PresentationContext object.
     */
    public DICOM_PresentationContext() {
        super(PDUTYPE_PresentationContext);
        presentationContextID = (byte) DICOM_Util.getUniqueOddID8();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Adds a new transfer syntax to the list (ie. vector)
     *
     * @param  transferSyntax  DOCUMENT ME!
     */
    public void addTransferSyntax(DICOM_PDUItemType transferSyntax) {
        trnSyntax.addElement(transferSyntax);
    }

    /**
     * Calculates length of message.
     *
     * @return  the length of the message
     */
    public int length() {
        int length;

        // presentationContextID + reserved2 + reserved3 + reserved4
        length = 4; // 1            +     1     +     1     +    1;
        length += absSyntax.calcSize();

        for (int i = 0; i < trnSyntax.size(); i++) {
            length += ((DICOM_PDUItemType) trnSyntax.elementAt(i)).calcSize();
        }

        return (length);
    }


    /**
     * Reads the body of the presentation context.
     *
     * @param      connection  the I/O Buffer to read from
     *
     * @exception  DICOM_Exception  unknown PDUType
     */
    public void readBody(DICOM_Comms connection) throws DICOM_Exception {
        DICOM_PDUItemType transferSyntax;
        int count;

        presentationContextID = connection.readByte();
        reserved2 = connection.readByte();
        reserved3 = connection.readByte();
        reserved4 = connection.readByte();

        // length - presentationContextID - reserved2 - reserved3 - reserved4
        count = length - 4; // 1             -     1     -      1    -     1;
        absSyntax.read(connection);
        count -= absSyntax.calcSize();

        while (count > 0) {
            transferSyntax = new DICOM_PDUItemType(PDUTYPE_TransferSyntax);
            transferSyntax.read(connection);
            count -= transferSyntax.calcSize();
            trnSyntax.addElement(transferSyntax);
        }
    }

    /**
     * Sets the abstract syntax of this class.
     *
     * @param  abstractSyntax  abstract syntax
     */
    public void setAbstractSyntax(DICOM_PDUItemType abstractSyntax) {
        absSyntax = abstractSyntax;
    }


    /**
     * Writes the body of the presentation context.
     *
     * @param      connection  the I/O Buffer to write to
     *
     * @exception  DICOM_Exception  problem with writing association request
     */
    public void writeBody(DICOM_Comms connection) throws DICOM_Exception {
        connection.writeByte(presentationContextID);
        connection.writeByte(reserved2);
        connection.writeByte(reserved3);
        connection.writeByte(reserved4);
        absSyntax.write(connection);

        for (int i = 0; i < trnSyntax.size(); i++) {
            ((DICOM_PDUItemType) trnSyntax.elementAt(i)).write(connection);
        }

        // connection.flush();
    }

}
