package gov.nih.mipav.model.dicomcomm;




import gov.nih.mipav.view.Preferences;

/**
*   DICOM Association Release Request
*/
public class DICOM_AReleaseRQ extends DICOM_PDUType {

	byte[] reserved2 = new byte[4];

    /**
    *
    */
    public DICOM_AReleaseRQ() {
        itemType = PDUTYPE_AReleaseRQ;
	    DICOM_Util.zeroByteArray(reserved2);
    }

    /**
    *   Calculates size of message
    *   @return the size of the message = 4 (reserved2 = 4 bytes)
    */
    public int length() { return(4); }

    /**
    *   Reads the body of an association release request
    *   @param     connection   the I/O Buffer to read from
    *   @exception DICOMException unknown PDUType
    */
   public void readBody(DICOM_Comms connection) throws DICOM_Exception {
       if (Preferences.debugLevel(Preferences.DEBUG_COMMS))
           Preferences.debug("AReleaseRQ.readBody\n");
       connection.read(reserved2);
   }

   /**
    *   Writes the body of the association release request
    *   @param     connection the I/O Buffer to write to
    *   @exception DICOMException problem with writing association release request
    */
   public void writeBody(DICOM_Comms connection) throws DICOM_Exception {
       if (Preferences.debugLevel(Preferences.DEBUG_COMMS))
           Preferences.debug("AReleaseRQ.writeBody\n");
       connection.write(reserved2);
       connection.flush();
   }

}
