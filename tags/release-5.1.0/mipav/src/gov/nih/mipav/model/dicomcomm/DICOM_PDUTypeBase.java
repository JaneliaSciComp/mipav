package gov.nih.mipav.model.dicomcomm;


/**
 * DICOMPDUTypeBase abstract base class that is extended by DICOMPDUType and DICOMPDUItemType.
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
public abstract class DICOM_PDUTypeBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final byte RESERVED = 0;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_UNKNOWN = 0x00;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_AAssociateRQ = 0x01;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_AAssociateAC = 0x02;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_AAssociateRJ = 0x03;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_PDataTF = 0x04;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_AReleaseRQ = 0x05;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_AReleaseRSP = 0x06;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_AAbortRQ = 0x07;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_ApplicationContext = 0x10;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_PresentationContext = 0x20;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_PresentationContextAccept = 0x21;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_AbstractSyntax = 0x30;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_TransferSyntax = 0x40;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_UserInformation = 0x50;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_MaximumSubLength = 0x51;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_ImplementationClass = 0x52;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_AsyncOpWindowSubItem = 0x53;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_SCPSCURoleSelect = 0x54;

    /** DOCUMENT ME! */
    public static final byte PDUTYPE_ImplementationVersion = 0x55;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected byte itemType = DICOM_PDUTypeBase.PDUTYPE_UNKNOWN;

    /** DOCUMENT ME! */
    protected int length = 0;

    /** DOCUMENT ME! */
    protected byte reserved1 = DICOM_PDUTypeBase.RESERVED;

    /** DOCUMENT ME! */
    protected byte reserved2 = DICOM_PDUTypeBase.RESERVED;

    /** DOCUMENT ME! */
    protected byte reserved3 = DICOM_PDUTypeBase.RESERVED;

    /** DOCUMENT ME! */
    protected byte reserved4 = DICOM_PDUTypeBase.RESERVED;

    /** DOCUMENT ME! */
    protected String UID = "";

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * These methods must be implemented by each class that extends this class.
     * 
     * @return DOCUMENT ME!
     */
    public abstract int calcSize();

    /**
     * DOCUMENT ME!
     * 
     * @param connection DOCUMENT ME!
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public abstract void readBody(DICOM_Comms connection) throws DICOM_Exception;

    /**
     * DOCUMENT ME!
     * 
     * @param connection DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public abstract byte readHeader(DICOM_Comms connection) throws DICOM_Exception;

    /**
     * DOCUMENT ME!
     * 
     * @param connection DOCUMENT ME!
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public abstract void writeBody(DICOM_Comms connection) throws DICOM_Exception;

    /**
     * DOCUMENT ME!
     * 
     * @param connection DOCUMENT ME!
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public abstract void writeHeader(DICOM_Comms connection) throws DICOM_Exception;

    /**
     * Converts item type to a readable string.
     * 
     * @param itemType type defined above
     * 
     * @return readable item type string
     */
    public static String convertItemTypeToString(final byte itemType) {

        switch (itemType) {

            case PDUTYPE_UNKNOWN:
                return (new String("Unknown"));

            case PDUTYPE_AAssociateRQ:
                return (new String("AAssociateRQ"));

            case PDUTYPE_AAssociateAC:
                return (new String("AAssociateAC"));

            case PDUTYPE_AAssociateRJ:
                return (new String("AAssociateRJ"));

            case PDUTYPE_PDataTF:
                return (new String("PDataTF"));

            case PDUTYPE_AReleaseRQ:
                return (new String("AReleaseRQ"));

            case PDUTYPE_AReleaseRSP:
                return (new String("AReleaseRSP"));

            case PDUTYPE_AAbortRQ:
                return (new String("AAbortRQ"));

            case PDUTYPE_ApplicationContext:
                return (new String("ApplicationContext"));

            case PDUTYPE_PresentationContext:
                return (new String("PresentationContext"));

            case PDUTYPE_PresentationContextAccept:
                return (new String("PresentationContextAccept"));

            case PDUTYPE_AbstractSyntax:
                return (new String("AbstractSyntax"));

            case PDUTYPE_TransferSyntax:
                return (new String("TransferSyntax"));

            case PDUTYPE_UserInformation:
                return (new String("UserInformation"));

            case PDUTYPE_MaximumSubLength:
                return (new String("MaximumSubLength"));

            case PDUTYPE_ImplementationClass:
                return (new String("ImplementationClass"));

            case PDUTYPE_SCPSCURoleSelect:
                return (new String("SCPSCURoleSelect"));

            case PDUTYPE_ImplementationVersion:
                return (new String("ImplementationVersion"));

            default:
                return (new String("Unknown"));
        }
    }

    /**
     * Gets the UID for a simple PDU type.
     * 
     * @return the UID
     */
    public String getUID() {
        return (UID);
    }

    /**
     * Calculates the PDU item's body size (not including the header).
     * 
     * @return the UID's length
     */
    public int length() {
        return (UID.length());
    }

    /**
     * Reads the header and body of the PDU message from the connection.
     * 
     * @param connection the connection where the data is read in
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void read(final DICOM_Comms connection) throws DICOM_Exception {
        readHeader(connection); // is defined in the class the extends this class
        readBody(connection); // is defined in the class the extends this class
    }

    /**
     * Sets the UID for a simple PDU type.
     * 
     * @param UID DOCUMENT ME!
     */
    public void setUID(final String UID) {
        this.UID = UID;
    }

    /**
     * Writes the header and body of the PDU messages.
     * 
     * @param connection the connection where the data sent out
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void write(final DICOM_Comms connection) throws DICOM_Exception {
        writeHeader(connection); // is defined in the class the extends this class
        writeBody(connection); // is defined in the class the extends this class

        // connection.flush();
    }

}
