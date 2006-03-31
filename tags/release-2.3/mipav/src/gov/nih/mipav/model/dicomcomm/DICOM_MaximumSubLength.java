package gov.nih.mipav.model.dicomcomm;




/**
*   DICOM_MaximumSubLength - simple class that implements a Maximum SubLength PDU type.
*/
public class DICOM_MaximumSubLength extends DICOM_PDUItemType {

    /** The maximum length - default is zero. */
    private int maxLength = 0;

    /**
    *   Simple class that implements a Maximum SubLength PDU type.
    *
    */
    public DICOM_MaximumSubLength() {
        super(PDUTYPE_MaximumSubLength);
    }

    /**
    *   Accessor that returns the maximum sublength
    *   @return the maximum sublength 
    */
    public int getMaxLength() { return( maxLength ); }

    /**
    *   Sets the maximum sublength
    *   @param maximum the new value for the maximum sublength
    */
    public void setMaxLength( int maximum )   { maxLength = maximum; }
   
    /**
    *   Accessor that returns the length in bytes 
    *   @return     size =  type + reserved + item length + length of 4 (i.e. a 4 byte int)
  
    */
    public int length() {  return ( 4 ); }
    
    /**
    *   Reads the maximum sublength out the connection (port)
    *   @param connection the connection (port) where the data is read
    */
    public void readBody( DICOM_Comms connection ) throws DICOM_Exception {
        maxLength = connection.readInt32();
    }
    
    /**
    *   Writes the maximum sublength out the connection (port)
    *   @param connection the connection (port) where the data is sent
    */
    public void writeBody( DICOM_Comms connection ) throws DICOM_Exception {
        connection.writeInt32( maxLength );              
    }

}
