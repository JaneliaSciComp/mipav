package gov.nih.mipav.model.dicomcomm;


/**
 * Useful DICOM constants. Methods to convert command and UID codes to strings.
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
 */
public class DICOM_Constants {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int MAXSUBLENGTH = 1024;

    /** DICOM Protocol Version - Default = 1. */
    public static final int PROTOCOLVERSION = 1;

    /** Application context UID = 1.2.840.10008.3.1.1.1 */
    public static final String UID_ApplicationContext = "1.2.840.10008.3.1.1.1";

    /** Verification UID = 1.2.840.10008.1.1 */
    public static final String UID_Verification = "1.2.840.10008.1.1";

    /** CR Storage UID = 1.2.840.10008.5.1.4.1.1.1 */
    public static final String UID_CRStorage = "1.2.840.10008.5.1.4.1.1.1";

    /** CT Storage UID = 1.2.840.10008.5.1.4.1.1.2 */
    public static final String UID_CTStorage = "1.2.840.10008.5.1.4.1.1.2";

    /** Old US MultiframeStorage UID = 1.2.840.10008.5.1.4.1.1.3. Retired in 1998. */
    public static final String UID_OldUSMultiframeStorage = "1.2.840.10008.5.1.4.1.1.3"; // retired 1998

    /** US Multiframe Storage UID = 1.2.840.10008.5.1.4.1.1.3.1. */
    public static final String UID_USMultiframeStorage = "1.2.840.10008.5.1.4.1.1.3.1";

    /** MR Storage UID = 1.2.840.10008.5.1.4.1.1.4 */
    public static final String UID_MRStorage = "1.2.840.10008.5.1.4.1.1.4";

    /** Enhanced MR Storage UID = 1.2.840.10008.5.1.4.1.1.4.1 */
    public static final String UID_EnhancedMRStorage = "1.2.840.10008.5.1.4.1.1.4.1";

    /** Enhanced CT Storage UID = 1.2.840.10008.5.1.4.1.1.4.1 */
    public static final String UID_EnhancedCTStorage = "1.2.840.10008.5.1.4.1.1.2.1";

    /** Enhanced XRAY Storage UID = 1.2.840.10008.5.1.4.1.1.4.1 */
    public static final String UID_EnhancedXAStorage = "1.2.840.10008.5.1.4.1.1.12.1.1";

    /** Old NM Storage UID = 1.2.840.10008.5.1.4.1.1.5 */
    public static final String UID_OldNMStorage = "1.2.840.10008.5.1.4.1.1.5";

    /** Old US Storage UID = 1.2.840.10008.5.1.4.1.1.6. Retired 1998 */
    public static final String UID_OldUSStorage = "1.2.840.10008.5.1.4.1.1.6"; // retired 1998

    /** US Storage UID = 1.2.840.10008.5.1.4.1.1.6.1 */
    public static final String UID_USStorage = "1.2.840.10008.5.1.4.1.1.6.1";

    /** SC (Secondary Capture) Storage UID = 1.2.840.10008.5.1.4.1.1.7 */
    public static final String UID_SCStorage = "1.2.840.10008.5.1.4.1.1.7";

    /** Standalone Overlay Storage UID = 1.2.840.10008.5.1.4.1.1.8 */
    public static final String UID_StandaloneOverlayStorage = "1.2.840.10008.5.1.4.1.1.8";

    /** Standalone Curve Storage UID = 1.2.840.10008.5.1.4.1.1.9 */
    public static final String UID_StandaloneCurveStorage = "1.2.840.10008.5.1.4.1.1.9";

    /** Standalone Modality LUT Storage UID = 1.2.840.10008.5.1.4.1.1.10 */
    public static final String UID_StandaloneModalityLUTStorage = "1.2.840.10008.5.1.4.1.1.10";

    /** Standalone VOI LUT Storage UID = 1.2.840.10008.5.1.4.1.1.11 */
    public static final String UID_StandaloneVOILUTStorage = "1.2.840.10008.5.1.4.1.1.11";

    /** XRayAngioStorage UID = 1.2.840.10008.5.1.4.1.1.12.1 */
    public static final String UID_XRayAngioStorage = "1.2.840.10008.5.1.4.1.1.12.1";

    /** XRay Fluoro Storage UID = 1.2.840.10008.5.1.4.1.1.12.2 */
    public static final String UID_XRayFluoroStorage = "1.2.840.10008.5.1.4.1.1.12.2";

    /** NM (Nuclear med) Storage UID = 1.2.840.10008.5.1.4.1.1.20 */
    public static final String UID_NMStorage = "1.2.840.10008.5.1.4.1.1.20";

    /** Pet Storage UID = 1.2.840.10008.5.1.4.1.1.128 */
    public static final String UID_PetStorage = "1.2.840.10008.5.1.4.1.1.128";

    /** DOCUMENT ME! */
    public static final String UID_StandalonePetCurve = "1.2.840.10008.5.1.4.1.1.129";

    /** DOCUMENT ME! */
    public static final String UID_GECTStorage = "1.2.840.113619.4.3"; // Need to add above convertUIDToString - Matt

    // 11/2003

    /** DOCUMENT ME! */
    public static final String UID_GEMRStorage = "1.2.840.113619.4.2"; // Need to add above convertUIDToString

    /** DOCUMENT ME! */
    public static final String UID_RTImageStorage = "1.2.840.10008.5.1.4.1.1.481.1";

    /** DOCUMENT ME! */
    public static final String UID_RTDoseStorage = "1.2.840.10008.5.1.4.1.1.481.2";

    /** DOCUMENT ME! */
    public static final String UID_RTStructureSetStorage = "1.2.840.10008.5.1.4.1.1.481.3";

    /** DOCUMENT ME! */
    public static final String UID_RTBeamsTreatmentRecordStorage = "1.2.840.10008.5.1.4.1.1.481.4";

    /** DOCUMENT ME! */
    public static final String UID_RTPlanStorage = "1.2.840.10008.5.1.4.1.1.481.5";

    /** DOCUMENT ME! */
    public static final String UID_RTBrachyTreatmentRecordStorage = "1.2.840.10008.5.1.4.1.1.481.6";

    /** DOCUMENT ME! */
    public static final String UID_RTTreatmentSummaryRecordStorage = "1.2.840.10008.5.1.4.1.1.481.7";

    /** DOCUMENT ME! */
    public static final String UID_PatientRootQuery = "1.2.840.10008.5.1.4.1.2.1.1";

    /** DOCUMENT ME! */
    public static final String UID_PatientRootRetrieve = "1.2.840.10008.5.1.4.1.2.1.2";

    /** DOCUMENT ME! */
    public static final String UID_PatientStudyOnlyQuery = "1.2.840.10008.5.1.4.1.2.3.1";

    /** DOCUMENT ME! */
    public static final String UID_PatientStudyOnlyRetrieve = "1.2.840.10008.5.1.4.1.2.3.2";

    /** DOCUMENT ME! */
    public static final String UID_StudyRootQuery = "1.2.840.10008.5.1.4.1.2.2.1";

    /** DOCUMENT ME! */
    public static final String UID_StudyRootRetrieve = "1.2.840.10008.5.1.4.1.2.2.2";

    /** DOCUMENT ME! */
    public static final String UID_TransferLITTLEENDIAN = "1.2.840.10008.1.2";

    /** DOCUMENT ME! */
    public static final String UID_TransferLITTLEENDIANEXPLICIT = "1.2.840.10008.1.2.1";

    /** DOCUMENT ME! */
    public static final String UID_TransferBIGENDIANEXPLICIT = "1.2.840.10008.1.2.2";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGBASELINEPROCESS1 = "1.2.840.10008.1.2.4.50";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGEXTENDEDPROC2AND4 = "1.2.840.10008.1.2.4.51";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGEXTENDEDPROC3AND5 = "1.2.840.10008.1.2.4.52";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGSPECTRALPROC6AND8 = "1.2.840.10008.1.2.4.53";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGSPECTRALPROC7AND9 = "1.2.840.10008.1.2.4.54";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGFULLPROGRESSPROC10AND12 = "1.2.840.10008.1.2.4.55";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGFULLPROGRESSPROC11AND13 = "1.2.840.10008.1.2.4.56";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGLOSSLESSPROC14 = "1.2.840.10008.1.2.4.57";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGLOSSLESSPROC15 = "1.2.840.10008.1.2.4.58";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGEXTENDEDPROC16AND18 = "1.2.840.10008.1.2.4.59";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGEXTENDEDPROC17AND19 = "1.2.840.10008.1.2.4.60";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGSPECTRALPROC20AND22 = "1.2.840.10008.1.2.4.61";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGSPECTRALPROC21AND23 = "1.2.840.10008.1.2.4.62";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGFULLPROGRESSPROC24AND26 = "1.2.840.10008.1.2.4.63";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGFULLPROGRESSPROC25AND27 = "1.2.840.10008.1.2.4.64";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGLOSSLESSPROC28 = "1.2.840.10008.1.2.4.65";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGLOSSLESSPROC29 = "1.2.840.10008.1.2.4.66";

    /** DOCUMENT ME! */
    public static final String UID_TransferJPEGLOSSLESSPROCFIRSTORDERREDICT = "1.2.840.10008.1.2.4.70";

    public static final String UID_TransferJPEG2000LOSSLESS = "1.2.840.10008.1.2.4.90";

    // Not supported yet public static final String UID_TransferJPEG2000 = "1.2.840.10008.1.2.4.91";

    /** DOCUMENT ME! */
    public static final String UID_RLE_Compression = "1.2.840.10008.1.2.5";

    /** DOCUMENT ME! */
    public static final int DSTYPE_DATAPRESENT = 0x0000; // anything but 0x0101

    /** DOCUMENT ME! */
    public static final int DSTYPE_NODATAPRESENT = 0x0101;

    /** Read/write file types... */
    public static final int FILETYPE_ACRNEMA_VR_DUMP = 1;

    /** DOCUMENT ME! */
    public static final int FILETYPE_DICOM_CHAPTER_10_IMPLICIT = 2;

    /** DOCUMENT ME! */
    public static final int FILETYPE_DICOM_CHAPTER_10_EXPLICIT = 3;

    /** DOCUMENT ME! */
    private static final int STATUS_OK = 0x0000;

    /** DOCUMENT ME! */
    private static final int STATUS_WARNING = 0xb000;

    /** DOCUMENT ME! */
    private static final int STATUS_CANCELLED = 0xfe00;

    /** DOCUMENT ME! */
    private static final int STATUS_PENDING = 0xff00;

    /** DOCUMENT ME! */
    public static final int STATUS_ERRORFIRST = DICOM_Constants.STATUS_OK + 1;

    /** DOCUMENT ME! */
    public static final int STATUS_ERRORLAST = DICOM_Constants.STATUS_PENDING - 1;

    /** DOCUMENT ME! */
    public static final int STATUS_STORE_FAILED = 0xc001;

    /** DOCUMENT ME! */
    public static final int STATUS_FIND_OBJECTNOTFOUND = 0xc002;

    /** DOCUMENT ME! */
    public static final int STATUS_MOVE_NODESTINATION = 0xc003;

    /** DOCUMENT ME! */
    public static final int STATUS_MOVE_NULLDESTINATION = 0xc004;

    /** DOCUMENT ME! */
    public static final int STATUS_MOVE_UNKNOWNDESTINATION = 0xc005;

    /** DOCUMENT ME! */
    public static final int STATUS_MOVE_OBJECTNOTFOUND = 0xc006;

    /** DOCUMENT ME! */
    public static final int STATUS_MOVE_NOCONTACTDEST = 0xc007;

    /** DOCUMENT ME! */
    public static final int STATUS_ECHO_SUCCESS = 0x0000;

    /** DOCUMENT ME! */
    public static final int STATUS_STORE_SUCCESS = 0x0000;

    /** DOCUMENT ME! */
    public static final int STATUS_FIND_SUCCESS = 0x0000;

    /** DOCUMENT ME! */
    public static final int STATUS_FIND_SUCCESSNORESULTS = 0x0000;

    /** DOCUMENT ME! */
    public static final int STATUS_FIND_PENDING = DICOM_Constants.STATUS_PENDING;

    /** DOCUMENT ME! */
    public static final int STATUS_MOVE_CANCELLED = DICOM_Constants.STATUS_CANCELLED;

    /** DOCUMENT ME! */
    public static final int STATUS_FIND_PENDINGWARNING = 0xff01;

    /** DOCUMENT ME! */
    public static final int STATUS_MOVE_SUCCESS = 0x0000;

    /** DOCUMENT ME! */
    public static final int STATUS_MOVE_PENDING = DICOM_Constants.STATUS_PENDING;

    /** DOCUMENT ME! */
    public static final int STATUS_MOVE_WARNING = DICOM_Constants.STATUS_WARNING;

    /** DOCUMENT ME! */
    public static final int COMMAND_BOGUS = 0x0000;

    /** DOCUMENT ME! */
    public static final int COMMAND_CStoreRQ = 0x0001;

    /** DOCUMENT ME! */
    public static final int COMMAND_CFindRQ = 0x0020;

    /** DOCUMENT ME! */
    public static final int COMMAND_CMoveRQ = 0x0021;

    /** DOCUMENT ME! */
    public static final int COMMAND_CEchoRQ = 0x0030;

    /** DOCUMENT ME! */
    public static final int COMMAND_CMoveCancelRQ = 0x0FFF;

    /** DOCUMENT ME! */
    public static final int COMMAND_CFindCancelRQ = 0x0FFF;

    /** DOCUMENT ME! */
    public static final int COMMAND_CStoreRSP = 0x8001;

    /** DOCUMENT ME! */
    public static final int COMMAND_CFindRSP = 0x8020;

    /** DOCUMENT ME! */
    public static final int COMMAND_CMoveRSP = 0x8021;

    /** DOCUMENT ME! */
    public static final int COMMAND_CEchoRSP = 0x8030;

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Converts DICOM command from and int code to a string
     * 
     * @param command The integer representation of the DICOM command.
     * 
     * @return The string representation of the code.
     */
    public static String convertCommandToString(final int command) {

        switch (command) {

            case COMMAND_BOGUS:
                return (new String("Unknown"));

            case COMMAND_CStoreRQ:
                return (new String("CStoreRQ"));

            case COMMAND_CFindRQ:
                return (new String("CFindRQ"));

            case COMMAND_CMoveRQ:
                return (new String("CMoveRQ"));

            case COMMAND_CEchoRQ:
                return (new String("CEchoRQ"));

            case COMMAND_CMoveCancelRQ:
                return (new String("CancelRQ"));

            case COMMAND_CStoreRSP:
                return (new String("CStoreRSP"));

            case COMMAND_CFindRSP:
                return (new String("CFindRSP"));

            case COMMAND_CMoveRSP:
                return (new String("CMoveRSP"));

            case COMMAND_CEchoRSP:
                return (new String("CEchoRSP"));

            default:
                return (new String("Unknown"));
        }
    }

    /**
     * Converts a transfer syntax UID (i.e. "1.2.840.10008.5.1.4.1.1.1") to a more understandable string form( i.e.
     * CRStorage )
     * 
     * @param UID UID to be converted
     * 
     * @return the convert UID string.
     */
    public static String convertUIDToString(final String UID) {

        if (UID.equals(DICOM_Constants.UID_Verification)) {
            return (new String("Verification"));
        } else if (UID.equals(DICOM_Constants.UID_CRStorage)) {
            return (new String("CRStorage"));
        } else if (UID.equals(DICOM_Constants.UID_CTStorage)) {
            return (new String("CTStorage"));
        } else if (UID.equals(DICOM_Constants.UID_MRStorage)) {
            return (new String("MRStorage"));
        } else if (UID.equals(DICOM_Constants.UID_SCStorage)) {
            return (new String("SecondaryCaptureStorage"));
        } else if (UID.equals(DICOM_Constants.UID_USMultiframeStorage)) {
            return (new String("USMultiframeStorage"));
        } else if (UID.equals(DICOM_Constants.UID_USStorage)) {
            return (new String("UltraSoundStorage"));
        } else if (UID.equals(DICOM_Constants.UID_NMStorage)) {
            return (new String("NuclearMedicineStorage"));
        } else if (UID.equals(DICOM_Constants.UID_OldUSStorage)) {
            return (new String("OldUltraSoundStorage"));
        } else if (UID.equals(DICOM_Constants.UID_OldUSMultiframeStorage)) {
            return (new String("OldUSMultiframeStorage"));
        } else if (UID.equals(DICOM_Constants.UID_OldNMStorage)) {
            return (new String("OldNMStorage"));
        } else if (UID.equals(DICOM_Constants.UID_StandaloneOverlayStorage)) {
            return (new String("StandaloneOverlayStorage"));
        } else if (UID.equals(DICOM_Constants.UID_StandaloneCurveStorage)) {
            return (new String("StandaloneCurveStorage"));
        } else if (UID.equals(DICOM_Constants.UID_StandaloneModalityLUTStorage)) {
            return (new String("StandaloneModalityLUTStorage"));
        } else if (UID.equals(DICOM_Constants.UID_StandaloneVOILUTStorage)) {
            return (new String("StandaloneVOILUTStorage"));
        } else if (UID.equals(DICOM_Constants.UID_XRayFluoroStorage)) {
            return (new String("XRayFluoroStorage"));
        } else if (UID.equals(DICOM_Constants.UID_XRayAngioStorage)) {
            return (new String("XRayAngioStorage"));
        } else if (UID.equals(DICOM_Constants.UID_PetStorage)) {
            return (new String("PetStorage"));
        } else if (UID.equals(DICOM_Constants.UID_StandalonePetCurve)) {
            return (new String("StandalonePetCurve"));
        } else if (UID.equals(DICOM_Constants.UID_PatientRootQuery)) {
            return (new String("PatientRootQuery"));
        } else if (UID.equals(DICOM_Constants.UID_PatientRootRetrieve)) {
            return (new String("PatientRootRetrieve"));
        } else if (UID.equals(DICOM_Constants.UID_PatientStudyOnlyQuery)) {
            return (new String("PatientStudyOnlyQuery"));
        } else if (UID.equals(DICOM_Constants.UID_PatientStudyOnlyRetrieve)) {
            return (new String("PatientStudyOnlyRetrieve"));
        } else if (UID.equals(DICOM_Constants.UID_StudyRootQuery)) {
            return (new String("StudyRootQuery"));
        } else if (UID.equals(DICOM_Constants.UID_StudyRootRetrieve)) {
            return (new String("StudyRootRetrieve"));
        } else if (UID.equals(DICOM_Constants.UID_RTImageStorage)) {
            return (new String("RTImageStorage"));
        } else if (UID.equals(DICOM_Constants.UID_RTDoseStorage)) {
            return (new String("RTDoseStorage"));
        } else if (UID.equals(DICOM_Constants.UID_RTStructureSetStorage)) {
            return (new String("RTStructureSetStorage"));
        } else if (UID.equals(DICOM_Constants.UID_RTBeamsTreatmentRecordStorage)) {
            return (new String("RTBeamsTreatmentRecordStorage"));
        } else if (UID.equals(DICOM_Constants.UID_RTPlanStorage)) {
            return (new String("RTPlanStorage"));
        } else if (UID.equals(DICOM_Constants.UID_RTBrachyTreatmentRecordStorage)) {
            return (new String("RTBrachyTreatmentRecordStorage"));
        } else if (UID.equals(DICOM_Constants.UID_RTTreatmentSummaryRecordStorage)) {
            return (new String("RTTreatmentSummaryRecordStorage"));
        }

        return (new String("Unknown"));
    }

}

// Defined SOP UIDs according to 2000 DICOM edition
/*
 * StoredPrintStorage "1.2.840.10008.5.1.1.27" HardcopyGrayscaleImageStorage "1.2.840.10008.5.1.1.29"
 * HardcopyColorImageStorage "1.2.840.10008.5.1.1.30" ComputedRadiographyImageStorage "1.2.840.10008.5.1.4.1.1.1"
 * DigitalXRayImageStorageForPresentation "1.2.840.10008.5.1.4.1.1.1.1" DigitalXRayImageStorageForProcessing
 * "1.2.840.10008.5.1.4.1.1.1.1.1" DigitalMammographyXRayImageStorageForPresentation "1.2.840.10008.5.1.4.1.1.1.2"
 * DigitalMammographyXRayImageStorageForProcessing "1.2.840.10008.5.1.4.1.1.1.2.1"
 * DigitalIntraOralXRayImageStorageForPresentation "1.2.840.10008.5.1.4.1.1.1.3"
 * DigitalIntraOralXRayImageStorageForProcessing "1.2.840.10008.5.1.4.1.1.1.3.1" CTImageStorage
 * "1.2.840.10008.5.1.4.1.1.2" RETIRED_UltrasoundMultiframeImageStorage "1.2.840.10008.5.1.4.1.1.3"
 * UltrasoundMultiframeImageStorage "1.2.840.10008.5.1.4.1.1.3.1" MRImageStorage "1.2.840.10008.5.1.4.1.1.4"
 * RETIRED_NuclearMedicineImageStorage "1.2.840.10008.5.1.4.1.1.5" RETIRED -- RETIRED_UltrasoundImageStorage
 * "1.2.840.10008.5.1.4.1.1.6" RETIRED -- UltrasoundImageStorage "1.2.840.10008.5.1.4.1.1.6.1"
 * SecondaryCaptureImageStorage "1.2.840.10008.5.1.4.1.1.7" StandaloneOverlayStorage "1.2.840.10008.5.1.4.1.1.8"
 * StandaloneCurveStorage "1.2.840.10008.5.1.4.1.1.9" TwelveLeadECGWaveformStorage "1.2.840.10008.5.1.4.1.1.9.1.1"
 * GeneralECGWaveformStorage "1.2.840.10008.5.1.4.1.1.9.1.2" AmbulatoryECGWaveformStorage
 * "1.2.840.10008.5.1.4.1.1.9.1.3" HemodynamicWaveformStorage "1.2.840.10008.5.1.4.1.1.9.2.1"
 * CardiacElectrophysiologyWaveformStorage "1.2.840.10008.5.1.4.1.1.9.3.1" BasicVoiceAudioWaveformStorage
 * "1.2.840.10008.5.1.4.1.1.9.4.1" StandaloneModalityLUTStorage "1.2.840.10008.5.1.4.1.1.10" StandaloneVOILUTStorage
 * "1.2.840.10008.5.1.4.1.1.11" GrayscaleSoftcopyPresentationStateStorage "1.2.840.10008.5.1.4.1.1.11.1"
 * XRayAngiographicImageStorage "1.2.840.10008.5.1.4.1.1.12.1" XRayFluoroscopyImageStorage
 * "1.2.840.10008.5.1.4.1.1.12.2" RETIRED_XRayAngiographicBiPlaneImageStorage "1.2.840.10008.5.1.4.1.1.12.3" RETIRED --
 * NuclearMedicineImageStorage "1.2.840.10008.5.1.4.1.1.20" RETIRED_VLImageStorage "1.2.840.10008.5.1.4.1.1.77.1"
 * VLEndoscopicImageStorage "1.2.840.10008.5.1.4.1.1.77.1.1" VLMicroscopicImageStorage "1.2.840.10008.5.1.4.1.1.77.1.2"
 * VLSlideCoordinatesMicroscopicImageStorage "1.2.840.10008.5.1.4.1.1.77.1.3" VLPhotographicImageStorage
 * "1.2.840.10008.5.1.4.1.1.77.1.4" RETIRED_VLMultiFrameImageStorage "1.2.840.10008.5.1.4.1.1.77.2" BasicTextSR
 * "1.2.840.10008.5.1.4.1.1.88.11" EnhancedSR "1.2.840.10008.5.1.4.1.1.88.22" ComprehensiveSR
 * "1.2.840.10008.5.1.4.1.1.88.33" PETImageStorage "1.2.840.10008.5.1.4.1.1.128" PETCurveStorage
 * "1.2.840.10008.5.1.4.1.1.129" RTImageStorage "1.2.840.10008.5.1.4.1.1.481.1" RTDoseStorage
 * "1.2.840.10008.5.1.4.1.1.481.2" RTStructureSetStorage "1.2.840.10008.5.1.4.1.1.481.3" RTBeamsTreatmentRecordStorage
 * "1.2.840.10008.5.1.4.1.1.481.4" RTPlanStorage "1.2.840.10008.5.1.4.1.1.481.5" RTBrachyTreatmentRecordStorage
 * "1.2.840.10008.5.1.4.1.1.481.6" RTTreatmentSummaryRecordStorage "1.2.840.10008.5.1.4.1.1.481.7"
 *  // Query/Retrieve FINDPatientRootQueryRetrieveInformationModel "1.2.840.10008.5.1.4.1.2.1.1"
 * MOVEPatientRootQueryRetrieveInformationModel "1.2.840.10008.5.1.4.1.2.1.2"
 * GETPatientRootQueryRetrieveInformationModel "1.2.840.10008.5.1.4.1.2.1.3" FINDStudyRootQueryRetrieveInformationModel
 * "1.2.840.10008.5.1.4.1.2.2.1" MOVEStudyRootQueryRetrieveInformationModel "1.2.840.10008.5.1.4.1.2.2.2"
 * GETStudyRootQueryRetrieveInformationModel "1.2.840.10008.5.1.4.1.2.2.3"
 * FINDPatientStudyOnlyQueryRetrieveInformationModel "1.2.840.10008.5.1.4.1.2.3.1"
 * MOVEPatientStudyOnlyQueryRetrieveInformationModel "1.2.840.10008.5.1.4.1.2.3.2"
 * GETPatientStudyOnlyQueryRetrieveInformationModel "1.2.840.10008.5.1.4.1.2.3.3" FINDModalityWorklistInformationModel
 * "1.2.840.10008.5.1.4.31"
 *  // Print BasicFilmSessionSOPClass "1.2.840.10008.5.1.1.1" BasicFilmBoxSOPClass "1.2.840.10008.5.1.1.2"
 * BasicGrayscaleImageBoxSOPClass "1.2.840.10008.5.1.1.4" BasicColorImageBoxSOPClass "1.2.840.10008.5.1.1.4.1"
 * RETIRED_ReferencedImageBoxSOPClass "1.2.840.10008.5.1.1.4.2" BasicGrayscalePrintManagementMetaSOPClass
 * "1.2.840.10008.5.1.1.9" RETIRED_ReferencedGrayscalePrintManagementMetaSOPClass "1.2.840.10008.5.1.1.9.1"
 * PrintJobSOPClass "1.2.840.10008.5.1.1.14" BasicAnnotationBoxSOPClass "1.2.840.10008.5.1.1.15" PrinterSOPClass
 * "1.2.840.10008.5.1.1.16" PrinterConfigurationRetrievalSOPClass "1.2.840.10008.5.1.1.16.376" PrinterSOPInstance
 * "1.2.840.10008.5.1.1.17" PrinterConfigurationRetrievalSOPInstance "1.2.840.10008.5.1.1.17.376"
 * BasicColorPrintManagementMetaSOPClass "1.2.840.10008.5.1.1.18" RETIRED_ReferencedColorPrintManagementMetaSOPClass
 * "1.2.840.10008.5.1.1.18.1" VOILUTBoxSOPClass "1.2.840.10008.5.1.1.22" PresentationLUTSOPClass
 * "1.2.840.10008.5.1.1.23" ImageOverlayBoxSOPClass "1.2.840.10008.5.1.1.24" BasicPrintImageOverlayBoxSOPClass
 * "1.2.840.10008.5.1.1.24.1" PrintQueueSOPInstance "1.2.840.10008.5.1.1.25" PrintQueueManagementSOPClass
 * "1.2.840.10008.5.1.1.26" PullPrintRequestSOPClass "1.2.840.10008.5.1.1.31" PullStoredPrintManagementMetaSOPClass
 * "1.2.840.10008.5.1.1.32"
 *  // Storage Commitment StorageCommitmentPushModelSOPClass "1.2.840.10008.1.20.1"
 * StorageCommitmentPushModelSOPInstance "1.2.840.10008.1.20.1.1" StorageCommitmentPullModelSOPClass
 * "1.2.840.10008.1.20.2" StorageCommitmentPullModelSOPInstance "1.2.840.10008.1.20.2.1"
 *  // MPPS ModalityPerformedProcedureStepSOPClass "1.2.840.10008.3.1.2.3.3"
 * ModalityPerformedProcedureStepRetrieveSOPClass "1.2.840.10008.3.1.2.3.4"
 * ModalityPerformedProcedureStepNotificationSOPClass "1.2.840.10008.3.1.2.3.5"
 *  // Detached Management DetachedPatientManagementSOPClass "1.2.840.10008.3.1.2.1.1"
 * DetachedPatientManagementMetaSOPClass "1.2.840.10008.3.1.2.1.4" DetachedVisitManagementSOPClass
 * "1.2.840.10008.3.1.2.2.1" DetachedStudyManagementSOPClass "1.2.840.10008.3.1.2.3.1" DetachedResultsManagementSOPClass
 * "1.2.840.10008.3.1.2.5.1" DetachedResultsManagementMetaSOPClass "1.2.840.10008.3.1.2.5.4"
 * DetachedStudyManagementMetaSOPClass "1.2.840.10008.3.1.2.5.5" DetachedInterpretationManagementSOPClass
 * "1.2.840.10008.3.1.2.6.1"
 *  // Other VerificationSOPClass "1.2.840.10008.1.1" BasicDirectoryStorageSOPClass "1.2.840.10008.1.3.10"
 * BasicStudyContentNotificationSOPClass "1.2.840.10008.1.9" StudyComponentManagementSOPClass "1.2.840.10008.3.1.2.3.2"
 */
/*
 * The following UIDs were defined in "frozen draft for trial implementation" versions of various DICOM supplements and
 * changed before final text. Since it is likely that trial implementations exist, we leave the UIDs in the dictionary.
 */

// Supplement 23 Frozen Draft (November 1997)
// DRAFT_SRTextStorage "1.2.840.10008.5.1.4.1.1.88.1"
// DRAFT_SRAudioStorage "1.2.840.10008.5.1.4.1.1.88.2"
// DRAFT_SRDetailStorage "1.2.840.10008.5.1.4.1.1.88.3"
// DRAFT_SRComprehensiveStorage "1.2.840.10008.5.1.4.1.1.88.4"
// Supplement 30 Draft 08 for Demonstration (October 1997)
// DRAFT_WaveformStorage "1.2.840.10008.5.1.4.1.1.9.1"
