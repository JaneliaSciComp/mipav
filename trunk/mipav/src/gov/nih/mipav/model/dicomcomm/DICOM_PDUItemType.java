package gov.nih.mipav.model.dicomcomm;





/**
 * This class implements a 2 byte item length (DICOM_PDUType implements 4 byte
 * item length. See DICOM_PDUTypeBase for the methods that are required
 * for new classes that extend this class.
 */
public class DICOM_PDUItemType extends DICOM_PDUTypeBase { 
    
    public DICOM_PDUItemType( byte itemType ) {
        this.itemType = itemType;
    }

    /**
    * calculates the PDU item type size
    * @return the size =  type + reserved + item length + (length)
    */
    public int calcSize() { return ( 1 + 1 + 2 + length() ); }

    /**
    *   Reads the PDU item header
    *   @param connection the connection to read from
    *   @return the PDU item type
    */
    public byte readHeader( DICOM_Comms connection ) throws DICOM_Exception {
        itemType  = connection.readByte();
        reserved1 = connection.readByte();
        length    = connection.readShort16();
        return( itemType );
    }
    
    /**
    *   Reads the body (i.e. UID) of the item type
    *   @param connection to read from
    */
    public void readBody( DICOM_Comms connection ) throws DICOM_Exception {
        UID = new String( connection.readBytes( length ) );
    }
    
    /**
    *   Writes the header of the item type
    *   @param connection to write to
    */
    public void writeHeader( DICOM_Comms connection ) {
        connection.writeByte(itemType);
        connection.writeByte(reserved1);
        connection.writeShort16(length());
    }
    
    /**
    *   Writes the body of the item type
    *   @param connection to write to
    */
    public void writeBody( DICOM_Comms connection ) throws DICOM_Exception {
        connection.write( UID.getBytes() );
    }

    
    
    
}
