package gov.nih.mipav.model.dicomcomm;


/**
 * DICOM AsyncOpWindowSubItem PDU Item Type - Not supported but we read it in. This is an optional support item in DICOM
 */
public class DICOM_AsyncOpWindowSubItem extends DICOM_PDUItemType {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected short maxNumOpsInvoked = 0;

    /** DOCUMENT ME! */
    protected short maxNumOpsPerformed = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * DICOM SCP/SCU Role Selection PDU Item Type.
     */
    public DICOM_AsyncOpWindowSubItem() {
        super(PDUTYPE_AsyncOpWindowSubItem);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates length of message.
     *
     * @return  the length of the message
     */
    public int length() {
        return (4);
    }

    /**
     * Reads the body of the sub item.
     *
     * @param      connection  the I/O Buffer to read from
     *
     * @exception  DICOM_Exception  unknown PDUType
     */
    public void readBody(DICOM_Comms connection) throws DICOM_Exception {
        int length;

        maxNumOpsInvoked = (short) connection.readShort16();
        maxNumOpsPerformed = (short) connection.readShort16();
    }

    /**
     * Writes the body of the sub item.
     *
     * @param      connection  the I/O Buffer to write to
     *
     * @exception  DICOM_Exception  problem with writing association request
     */
    public void writeBody(DICOM_Comms connection) throws DICOM_Exception {
        connection.writeShort16(maxNumOpsInvoked);
        connection.writeShort16(maxNumOpsPerformed);
    }

}
