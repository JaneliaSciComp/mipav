package gov.nih.mipav.model.dicomcomm;


/**
 * Simple utilities to convert transfer syntaxes to a different form.
 * 
 * <hr>
 * 
 * This DICOM communication package was originally based on the Java Dicom Package, whose license is below:
 * 
 * <pre>
 * Java Dicom Package (com.zmed.dicom)
 * 
 *  Copyright (c) 1996-1997 Z Medical Imaging Systems, Inc.
 * 
 *  This software is provided, as is, for non-commercial educational
 *  purposes only.   Use or incorporation of this software or derivative
 *  works in commercial applications requires written consent from
 *  Z Medical Imaging Systems, Inc.
 * 
 *  Z MEDICAL IMAGING SYSTEMS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT
 *  THE SUITABILITY OF THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING
 *  BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS
 *  FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT, OR CONFORMANCE TO ANY
 *  SPECIFICATION OR STANDARD.  Z MEDICAL IMAGING SYSTEMS SHALL NOT BE
 *  LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING OR
 *  MODIFYING THIS SOFTWARE OR ITS DERIVATIVES.
 * 
 *  =============================================================================
 * 
 *  This software package is implemented similarly to the UC Davis public
 *  domain C++ DICOM implementation which contains the following copyright
 *  notice:
 * 
 *  Copyright (C) 1995, University of California, Davis
 * 
 *  THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND THE UNIVERSITY
 *  OF CALIFORNIA DOES NOT MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
 *  PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
 *  USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
 *  SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
 *  THE SOFTWARE IS WITH THE USER.
 * 
 *  Copyright of the software and supporting documentation is
 *  owned by the University of California, and free access
 *  is hereby granted as a license to use this software, copy this
 *  software and prepare derivative works based upon this software.
 *  However, any distribution of this software source code or
 *  supporting documentation or derivative works (source code and
 *  supporting documentation) must include this copyright notice.
 * 
 *  The UC Davis C++ source code is publicly available from the following
 *  anonymous ftp site:
 * 
 *  ftp://imrad.ucdmc.ucdavis.edu/pub/dicom/UCDMC/
 * </pre>
 * 
 * @author Matthew J. McAuliffe, Ph.D.
 */
public class DICOM_TransferSyntaxUtil {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Static that defines IMPLICIT_LITTLE_ENDIAN. */
    public static final int IMPLICIT_LITTLE_ENDIAN = 100;

    /** Static that defines EXPLICIT_LITTLE_ENDIAN. */
    public static final int EXPLICIT_LITTLE_ENDIAN = 101;

    /** Static that defines EXPLICIT_BIG_ENDIAN. */
    public static final int EXPLICIT_BIG_ENDIAN = 102;

    /** Static that defines UNKNOWN. */
    public static final int UNKNOWN = 0;

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Converts a transfer syntax UID to one of the three transfer syntaxes.
     * 
     * @param transferSyntaxUID UID to be converted
     * 
     * @return the converted UID ID.
     */
    public static int convertToAlias(final String transferSyntaxUID) {

        if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferLITTLEENDIAN)) {
            return (DICOM_TransferSyntaxUtil.IMPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferLITTLEENDIANEXPLICIT)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferBIGENDIANEXPLICIT)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_BIG_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGBASELINEPROCESS1)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGEXTENDEDPROC2AND4)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGEXTENDEDPROC3AND5)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGSPECTRALPROC6AND8)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGSPECTRALPROC7AND9)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGFULLPROGRESSPROC10AND12)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGFULLPROGRESSPROC11AND13)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROC14)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROC15)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGEXTENDEDPROC16AND18)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGEXTENDEDPROC17AND19)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGSPECTRALPROC20AND22)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGSPECTRALPROC21AND23)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGFULLPROGRESSPROC24AND26)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGFULLPROGRESSPROC25AND27)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROC28)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROC29)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        } else if (transferSyntaxUID.equals(DICOM_Constants.UID_TransferJPEGLOSSLESSPROCFIRSTORDERREDICT)) {
            return (DICOM_TransferSyntaxUtil.EXPLICIT_LITTLE_ENDIAN);
        }

        return (DICOM_TransferSyntaxUtil.UNKNOWN);
    }

    /**
     * Converts a transfer syntax UID (i.e. "1.2.840.10008.1.2.4.50") to a more understandable string form ( i.e.
     * TransferJPEGBASELINEPROCESS1 )
     * 
     * @param transferSyntaxUID UID to be converted
     * 
     * @return the convert UID string.
     */
    public static String convertToReadableString(final String transferSyntaxUID) {

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
