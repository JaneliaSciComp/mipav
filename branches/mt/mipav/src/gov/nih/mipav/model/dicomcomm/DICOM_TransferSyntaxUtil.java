package gov.nih.mipav.model.dicomcomm;


/**
 * Simple utilities to convert transfer syntaxes to a different form.
 *
 * @author   Matthew J. McAuliffe, Ph.D.
 * @version  1.0
 */

public class DICOM_TransferSyntaxUtil {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Static that defines IMPLICIT_LITTLE_ENDIAN. */
    public static final int IMPLICIT_LITTLE_ENDIAN = 100;

    /** Static that defines EXPLICIT_LITTLE_ENDIAN. */
    public static final int EXPLICIT_LITTLE_ENDIAN = 101;

    /** Static that defines EXPLICIT_BIG_ENDIAN. */
    public static final int EXPLICIT_BIG_ENDIAN = 102;

    /** Static that defines UNKNOWN. */
    public static final int UNKNOWN = 0;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Converts a transfer syntax UID to one of the three transfer syntaxes.
     *
     * @param   transferSyntaxUID  UID to be converted
     *
     * @return  the converted UID ID.
     */
    public static int convertToAlias(String transferSyntaxUID) {

        if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferLITTLEENDIAN)) {
            return (IMPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferLITTLEENDIANEXPLICIT)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferBIGENDIANEXPLICIT)) {
            return (EXPLICIT_BIG_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGBASELINEPROCESS1)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGEXTENDEDPROC2AND4)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGEXTENDEDPROC3AND5)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGSPECTRALPROC6AND8)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGSPECTRALPROC7AND9)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGFULLPROGRESSPROC10AND12)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGFULLPROGRESSPROC11AND13)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROC14)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROC15)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGEXTENDEDPROC16AND18)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGEXTENDEDPROC17AND19)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGSPECTRALPROC20AND22)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGSPECTRALPROC21AND23)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGFULLPROGRESSPROC24AND26)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGFULLPROGRESSPROC25AND27)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROC28)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROC29)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROCFIRSTORDERREDICT)) {
            return (EXPLICIT_LITTLE_ENDIAN);
        }

        return (UNKNOWN);
    }

    /**
     * Converts a transfer syntax UID (i.e. "1.2.840.10008.1.2.4.50") to a more understandable string form ( i.e.
     * TransferJPEGBASELINEPROCESS1 )
     *
     * @param   transferSyntaxUID  UID to be converted
     *
     * @return  the convert UID string.
     */
    public static String convertToReadableString(String transferSyntaxUID) {

        if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferLITTLEENDIAN)) {
            return (new String("IMPLICIT_LITTLE_ENDIAN"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferLITTLEENDIANEXPLICIT)) {
            return (new String("EXPLICIT_LITTLE_ENDIAN"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferBIGENDIANEXPLICIT)) {
            return (new String("EXPLICIT_BIG_ENDIAN"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGBASELINEPROCESS1)) {
            return (new String("JPEGBASELINEPROCESS1"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGEXTENDEDPROC2AND4)) {
            return (new String("JPEGEXTENDEDPROC2AND4"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGEXTENDEDPROC3AND5)) {
            return (new String("JPEGEXTENDEDPROC3AND5"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGSPECTRALPROC6AND8)) {
            return (new String("JPEGSPECTRALPROC6AND8"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGSPECTRALPROC7AND9)) {
            return (new String("JPEGSPECTRALPROC7AND9"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGFULLPROGRESSPROC10AND12)) {
            return (new String("JPEGFULLPROGRESSPROC10AND12"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGFULLPROGRESSPROC11AND13)) {
            return (new String("JPEGFULLPROGRESSPROC11AND13"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROC14)) {
            return (new String("JPEGLOSSLESSPROC14"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROC15)) {
            return (new String("JPEGLOSSLESSPROC15"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGEXTENDEDPROC16AND18)) {
            return (new String("JPEGEXTENDEDPROC16AND18"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGEXTENDEDPROC17AND19)) {
            return (new String("JPEGEXTENDEDPROC17AND19"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGSPECTRALPROC20AND22)) {
            return (new String("JPEGSPECTRALPROC20AND22"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGSPECTRALPROC21AND23)) {
            return (new String("JPEGSPECTRALPROC21AND23"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGFULLPROGRESSPROC24AND26)) {
            return (new String("JPEGFULLPROGRESSPROC24AND26"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGFULLPROGRESSPROC25AND27)) {
            return (new String("JPEGFULLPROGRESSPROC25AND27"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROC28)) {
            return (new String("JPEGLOSSLESSPROC28"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROC29)) {
            return (new String("JPEGLOSSLESSPROC29"));
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROCFIRSTORDERREDICT)) {
            return (new String("JPEGLOSSLESSPROCFIRSTORDERREDICT"));
        }

        return (new String("Unknown")); // unknown transfer syntax
    }

}
