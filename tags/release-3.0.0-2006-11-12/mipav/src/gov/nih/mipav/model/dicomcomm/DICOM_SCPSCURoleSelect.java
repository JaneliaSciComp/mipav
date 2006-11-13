package gov.nih.mipav.model.dicomcomm;


/**
 * DICOM SCP/SCU Role Selection PDU Item Type.
 */
public class DICOM_SCPSCURoleSelect extends DICOM_PDUItemType {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Indicates the Serivce Class Provider (SCP) (i.e. Server).  */
    protected byte SCPRole = 0;

    /** Indicates the Serivce Class User (SCU) (i.e. Client).  */
    protected byte SCURole = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * DICOM SCP/SCU Role Selection PDU Item Type.
     */
    public DICOM_SCPSCURoleSelect() {
        super(PDUTYPE_SCPSCURoleSelect);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates length of message.
     *
     * @return  the length of the message
     */
    public int length() {

        // UID.length + UID + SCURole + SCPRole
        return (UID.length() + 4); // 2      + ... +    1    +   1
    }

    /**
     * Reads the body of the SCP/SCU.
     *
     * @param      connection  the I/O Buffer to read from
     *
     * @exception  DICOM_Exception  unknown PDUType
     */
    public void readBody(DICOM_Comms connection) throws DICOM_Exception {
        int length;

        length = connection.readShort16();
        UID = new String(connection.readBytes(length));
        SCURole = connection.readByte();
        SCPRole = connection.readByte();
    }

    /**
     * Writes the body of the SCP/SCU.
     *
     * @param      connection  the I/O Buffer to write to
     *
     * @exception  DICOM_Exception  problem with writing association request
     */
    public void writeBody(DICOM_Comms connection) throws DICOM_Exception {
        connection.writeShort16(UID.length());
        connection.write(UID.getBytes());
        connection.writeByte(SCURole);
        connection.writeByte(SCPRole);
    }

}
