package gov.nih.mipav.model.dicomcomm;



/**
*   DICOMPDUTypeBase abstract base class that is extended by DICOMPDUType and 
*   DICOMPDUItemType. 
*/

public abstract class DICOM_PDUTypeBase {
    
    public static final byte RESERVED = 0;
    
    public static final byte PDUTYPE_UNKNOWN                    = 0x00;
    public static final byte PDUTYPE_AAssociateRQ               = 0x01;
    public static final byte PDUTYPE_AAssociateAC               = 0x02;
    public static final byte PDUTYPE_AAssociateRJ               = 0x03;
    public static final byte PDUTYPE_PDataTF                    = 0x04;
    public static final byte PDUTYPE_AReleaseRQ                 = 0x05;
    public static final byte PDUTYPE_AReleaseRSP                = 0x06;
    public static final byte PDUTYPE_AAbortRQ                   = 0x07;
    public static final byte PDUTYPE_ApplicationContext         = 0x10;
    public static final byte PDUTYPE_PresentationContext        = 0x20;
    public static final byte PDUTYPE_PresentationContextAccept  = 0x21;
    public static final byte PDUTYPE_AbstractSyntax             = 0x30;
    public static final byte PDUTYPE_TransferSyntax             = 0x40;
    public static final byte PDUTYPE_UserInformation            = 0x50;
    public static final byte PDUTYPE_MaximumSubLength           = 0x51;
    public static final byte PDUTYPE_ImplementationClass        = 0x52;
    public static final byte PDUTYPE_AsyncOpWindowSubItem       = 0x53;
    public static final byte PDUTYPE_SCPSCURoleSelect           = 0x54;
    public static final byte PDUTYPE_ImplementationVersion      = 0x55;
 
    protected byte itemType  = PDUTYPE_UNKNOWN;
    protected byte reserved1 = RESERVED;
    protected byte reserved2 = RESERVED;
    protected byte reserved3 = RESERVED;
    protected byte reserved4 = RESERVED;
    
    protected int  length    = 0;
    protected String UID = "";
    
    /**
    *   Converts item type to a readable string
    *   @param itemType type defined above
    *   @return readable item type string
    */
    public static String convertItemTypeToString( byte itemType ) {
        
        switch (itemType) {
            case PDUTYPE_UNKNOWN:                   return( new String ("Unknown" ));
            case PDUTYPE_AAssociateRQ:              return( new String ("AAssociateRQ" ));
            case PDUTYPE_AAssociateAC:              return( new String ("AAssociateAC" ));
            case PDUTYPE_AAssociateRJ:              return( new String ("AAssociateRJ" ));
            case PDUTYPE_PDataTF:                   return( new String ("PDataTF" ));
            case PDUTYPE_AReleaseRQ:                return( new String ("AReleaseRQ" ));
            case PDUTYPE_AReleaseRSP:               return( new String ("AReleaseRSP" ));
            case PDUTYPE_AAbortRQ:                  return( new String ("AAbortRQ" ));
            case PDUTYPE_ApplicationContext:        return( new String ("ApplicationContext" ));
            case PDUTYPE_PresentationContext:       return( new String ("PresentationContext" ));
            case PDUTYPE_PresentationContextAccept: return( new String ("PresentationContextAccept" ));
            case PDUTYPE_AbstractSyntax:            return( new String ("AbstractSyntax" ));
            case PDUTYPE_TransferSyntax:            return( new String ("TransferSyntax" ));
            case PDUTYPE_UserInformation:           return( new String ("UserInformation" ));
            case PDUTYPE_MaximumSubLength:          return( new String ("MaximumSubLength" ));
            case PDUTYPE_ImplementationClass:       return( new String ("ImplementationClass" ));
            case PDUTYPE_SCPSCURoleSelect:          return( new String ("SCPSCURoleSelect" ));
            case PDUTYPE_ImplementationVersion:     return( new String ("ImplementationVersion" ));
            default: return( new String("Unknown") );  
        }
    }
    
    /**
    *   Sets the UID for a simple PDU type
    *   @param UID
    */
    public void setUID( String UID ) { this.UID = UID; }

    /**
    *   Gets the UID for a simple PDU type
    *   @return  the UID
    */
    public String getUID()          { return( UID ); }

    /**
    * Calculates the PDU item's body size (not including the header)
    * @return the UID's length
    */
    public int length()          { return( UID.length() ); }

    /**
    *   Writes the header and body of the PDU messages. 
    *   @param connection the connection where the data sent out    
    */
    public void write( DICOM_Comms connection ) throws DICOM_Exception {
        writeHeader( connection ); // is defined in the class the extends this class
        writeBody(   connection ); // is defined in the class the extends this class
        //connection.flush();
    }
    
    /**
    *   Reads the header and body of the PDU message from the connection
    *   @param connection 
    */
    public void read( DICOM_Comms connection ) throws DICOM_Exception {
        readHeader(connection);   // is defined in the class the extends this class
        readBody  (connection);   // is defined in the class the extends this class
    }
    
    
    /**
    *   These methods must be implemented by each class that extends this class      
    */
    public abstract int  calcSize();
    public abstract byte readHeader (DICOM_Comms connection) throws DICOM_Exception;
    public abstract void readBody   (DICOM_Comms connection) throws DICOM_Exception;
    public abstract void writeHeader(DICOM_Comms connection) throws DICOM_Exception;
    public abstract void writeBody  (DICOM_Comms connection) throws DICOM_Exception;
   
    
}