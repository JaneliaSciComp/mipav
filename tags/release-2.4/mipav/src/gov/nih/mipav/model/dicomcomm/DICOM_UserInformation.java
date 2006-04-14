package gov.nih.mipav.model.dicomcomm;


import java.util.Vector;

/**
 * DICOM User Information PDU Item Type (part 8 section 9.3)
 */
public class DICOM_UserInformation extends DICOM_PDUItemType {

    private int userInfoSize = 0;
    public DICOM_PDUItemType      implementationClass   = new DICOM_PDUItemType(PDUTYPE_ImplementationClass);
    public DICOM_PDUItemType      implementationVersion = new DICOM_PDUItemType(PDUTYPE_ImplementationVersion);
    //public DICOM_SCPSCURoleSelect SCPSCURole            = null; //new DICOM_SCPSCURoleSelect();
    public Vector SCPSCURoleVector                      = new Vector();
    public DICOM_AsyncOpWindowSubItem aSyncSubItem      = null; //new DICOM_AsyncOpWindowSubItem();
    public DICOM_MaximumSubLength maxSubLength          = new DICOM_MaximumSubLength();

    /**
    *   Constructs a DICOM user information object. This object extends the DICOM_PDUItemType
    *
    */
    public DICOM_UserInformation() {
        super(PDUTYPE_UserInformation);
    }


    /**
    * Calculates the PDU item type size
    * @return the size =  parent size  + userInfoSize
    */
    public int calcSize() { return( super.calcSize() ); }

    /**
    *   Accessor that returns the length in bytes
    *   @return     item length
    */
    public int length() {
        int length;

        length  = implementationClass.calcSize();
        length += implementationVersion.calcSize();
        length += maxSubLength.calcSize();

        for (int i = 0; i < SCPSCURoleVector.size(); i++) {
            length += ((DICOM_SCPSCURoleSelect)(SCPSCURoleVector.elementAt(i))).calcSize();
        }

        if ( aSyncSubItem != null ) {
            length += aSyncSubItem.calcSize();
        }

        length += userInfoSize;

        return(length);
    }

    /**
    * Reads the body of the user information packet
    * @param connection  the connection to read from
    * @exception throw DICOM_Exception if problem occurs
    */
    public void readBody(DICOM_Comms connection) throws DICOM_Exception {
        int count    = length;

        userInfoSize = 0;
        while(count > 0) {

            //Preferences.debug(DICOM_Util.timeStamper() +  " DICOM_UserInformation.readBody: count = " + length + " \n");

            switch(connection.peekFirstByte()) {
                case PDUTYPE_ImplementationClass:
                    implementationClass.read(connection);
                    count = count - implementationClass.calcSize();
                    break;

                case PDUTYPE_ImplementationVersion:
                    implementationVersion.read(connection);
                    count = count - implementationVersion.calcSize();
                    break;

                case PDUTYPE_MaximumSubLength:
                    maxSubLength.read(connection);
                    count = count - maxSubLength.calcSize();
                    break;

                case PDUTYPE_SCPSCURoleSelect:
                    // Not sure how this info is used. Kodak's Auto Rads send an array of them).
                    DICOM_SCPSCURoleSelect SCPSCURole = new DICOM_SCPSCURoleSelect();
                    SCPSCURole.read(connection);
                    count = count - SCPSCURole.calcSize();
                    SCPSCURoleVector.add(SCPSCURole);
                    break;

                case PDUTYPE_AsyncOpWindowSubItem:
                    // Not supported and is optional - however it is read in.
                    aSyncSubItem      = new DICOM_AsyncOpWindowSubItem();
                    aSyncSubItem.read(connection);
                    count = count - aSyncSubItem.calcSize();
                    break;

                default:
                    connection.readBytes(count-1);
                    userInfoSize = count;
                    throw new DICOM_Exception("DICOMError: Error reading user info.");
            }
        }
    }


    /**
    * Writes the body of the user information
    * @param connection the connection to write to
    * @exception throw DICOM_Exception if problem occurs
    */
    public void writeBody(DICOM_Comms connection) throws DICOM_Exception {
        maxSubLength.write(connection );
        implementationClass.write(connection);
        implementationVersion.write(connection);
    }

}
