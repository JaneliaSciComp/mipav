package gov.nih.mipav.model.dicomcomm;


/**
 * This an abstract class implements a 4 byte item length (DICOM_PDUItemType implements 2 byte item length. See
 * DICOM_PDUTypeBase for the methods that are required for new classes that extend this class.
 */
public abstract class DICOM_PDUType extends DICOM_PDUTypeBase {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the size of this PDU type.
     *
     * @return  the size = type + reserved + pdu length + (size)
     */
    public int calcSize() {
        return (1 + 1 + 4 + length());
    }

    /**
     * Reads the PDU type header.
     *
     * @param   connection  the connection to read from
     *
     * @return  The PDU item type read.
     *
     * @throws  DICOM_Exception  Indicates reading error.
     */
    public byte readHeader(DICOM_Comms connection) throws DICOM_Exception {
        itemType = connection.readByte();
        reserved1 = connection.readByte();
        length = connection.readInt32();

        return (itemType);
    }

    /**
     * Writes the PDU type header.
     *
     * @param  connection  the connection to write to
     */
    public void writeHeader(DICOM_Comms connection) {
        connection.writeByte(itemType);
        connection.writeByte(reserved1);
        connection.writeInt32(length()); /// should this be connection.writeInt32( calcSize() ); ??
    }

}
