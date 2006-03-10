package gov.nih.mipav.model.dicomcomm;





/**
 * This is a base class for all  SOP Class implementations.
 */

public class DICOM_SOP {

    protected String  UID               = null;  

    /**
    *   Verifies the SOP class UID
    *   @param DCO DICOM command object
    */
    public void verifySOPClass( DICOM_Object DCO ) throws DICOM_Exception {
        String DCO_UID;
        
        if( DCO == null ) { 
            throw new DICOM_Exception("DICOM_SOP.verifySOPClass: DCO is null" ); 
        }

        DCO_UID = DCO.getStr( DICOM_RTC.DD_AffectedSOPClassUID );
        if( DCO_UID == null ) { throw new DICOM_Exception( "DICOM_SOP.verifySOPClass: SOP class UID is null" ); }

        if( !DCO_UID.equals(UID) ) {
            throw new DICOM_Exception( "DICOM_SOP.verifySOPClass: SOP class UID is invalid" );
        }
    }

}
