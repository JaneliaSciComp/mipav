package gov.nih.mipav.model.dicomcomm;


/**
 * A DICOM Storage SOP Class.
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
public class DICOM_StdStorage extends DICOM_SOP {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Store request object. */
    public DICOM_CRequest cStoreRq;

    /** Store response object. */
    public DICOM_CResponse cStoreRsp;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_StdStorage object.
     */
    public DICOM_StdStorage() {
        cStoreRq = new DICOM_CRequest(DICOM_Constants.COMMAND_CStoreRQ);
        cStoreRsp = new DICOM_CResponse(DICOM_Constants.COMMAND_CStoreRSP);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Does not have functionality yet.
     * 
     * @param pdu DOCUMENT ME!
     * @param dco DOCUMENT ME!
     * @param ddo DOCUMENT ME!
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void read(final DICOM_PDUService pdu, final DICOM_Object dco, final DICOM_Object ddo) throws DICOM_Exception {}

    /**
     * Provides Storage SOP write capability.
     * 
     * @param pdu PDU service object
     * @param ddo DICOM data object
     * @param transferSyntax The DICOM transfer syntax.
     * @param classUID The class UID.
     * @param instanceUID The instance UID.
     * 
     * @throws DICOM_Exception Indicates error when writing data.
     */
    public void write(final DICOM_PDUService pdu, final DICOM_Object ddo, final String transferSyntax,
            final String classUID, final String instanceUID) throws DICOM_Exception {

        final DICOM_Object dco = new DICOM_Object();
        pdu.parseDICOMintoBuffer(ddo.copy(), pdu.pDataTF.getVRLinkedBuffer(), pdu.ioBuffer);
        cStoreRq.write(pdu, transferSyntax, classUID, instanceUID, ddo, null);
        pdu.readInObject(dco);
        cStoreRsp.read(dco, pdu, null);
    }

}
