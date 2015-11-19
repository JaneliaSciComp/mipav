package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.Preferences;


/**
 * This is the base class for DIMSE-C response messages.
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
public class DICOM_CResponse extends DICOM_CRequest {

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_CResponse object.
     * 
     * @param commandType DICOM.COMMAND_CEchoRSP, or CStoreRSP, CFindRSP, CMoveRSP
     */
    public DICOM_CResponse(final int commandType) {
        super(commandType);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Reads a DIMSE-C message.
     * 
     * @param dco the incoming DICOM message
     * @param pdu the context pdu DICOMClientServer
     * @param ddo a place to store the incoming DICOM data object or "null" if not required
     * 
     * @exception DICOM_Exception if anything goes wrong
     */
    public void read(final DICOM_Object dco, final DICOM_PDUService pdu, final DICOM_Object ddo) throws DICOM_Exception {

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " CResponse.read: \n");
        }

        super.readCResponseAlias(pdu, dco, ddo);
    }

    /**
     * Writes a DIMSE-C message.
     * 
     * @param pdu context pdu DICOMClientServer
     * @param dco data command object
     * @param UID SOP Class UID null for C-Stores
     * @param status status value to be sent
     * @param errComment string to send error
     * @param ddo the outgoing DICOM data object or <code>null</code> if none required
     * @param nRemaining number of C-Stores remaining for a C-Move operation
     * @param nComplete number of C-Stores which have been completed successfully
     * @param nFailed number of C-Stores which have failed
     * @param nWarning number of C-Stores which have completed with a warning
     * 
     * @exception DICOM_Exception indicates an error
     */
    public void write(final DICOM_PDUService pdu, final DICOM_Object dco, final String UID, final int status,
            final String errComment, final DICOM_Object ddo, final int nRemaining, final int nComplete,
            final int nFailed, final int nWarning) throws DICOM_Exception {

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " CResponse.write: Start \n");
        }

        final DICOM_Object dcoRsp = new DICOM_Object();
        DICOM_VR vr;

        dcoRsp.setInt16(DICOM_RTC.DD_CommandField, COMMAND);

        DICOM_Util.determineSOPClassUIDAndPush(UID, dco, ddo, dcoRsp);

        while ( (vr = dco.pop()) != null) {

            if (vr.group == 0x0000) {

                switch (vr.element) {

                    case 0x0010:
                        dcoRsp.push(vr);
                        break;

                    case 0x0110:
                        vr.element = 0x0120;
                        dcoRsp.push(vr);
                        break;

                    case 0x0200:
                        vr.element = 0x0300;
                        dcoRsp.push(vr);
                        break;

                    case 0x0300:
                        vr.element = 0x0200;
                        dcoRsp.push(vr);
                        break;

                    case 0x1000:
                        dcoRsp.push(vr);
                        break;
                }
            }
        }

        if (ddo != null) {
            dcoRsp.setInt16(DICOM_RTC.DD_DataSetType, DICOM_Constants.DSTYPE_DATAPRESENT);
        } else {
            dcoRsp.setInt16(DICOM_RTC.DD_DataSetType, DICOM_Constants.DSTYPE_NODATAPRESENT);
        }

        dcoRsp.setInt16(DICOM_RTC.DD_MessageID, DICOM_Util.getUniqueOddID16());
        dcoRsp.setInt16(DICOM_RTC.DD_Priority, DICOM_CRequest.MEDIUM);
        dcoRsp.setInt16(DICOM_RTC.DD_Status, status);

        if (errComment != null) {
            dcoRsp.setStr(DICOM_RTC.DD_ErrorComment, errComment);
        }

        if (COMMAND == DICOM_Constants.COMMAND_CMoveRSP) {
            dcoRsp.setInt16(DICOM_RTC.DD_NumberOfCompletedSuboperations, nComplete);
            dcoRsp.setInt16(DICOM_RTC.DD_NumberOfRemainingSuboperations, nRemaining);
            dcoRsp.setInt16(DICOM_RTC.DD_NumberOfFailedSuboperations, nFailed);
            dcoRsp.setInt16(DICOM_RTC.DD_NumberOfWarningSuboperations, nWarning);
        }
        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " CResponse.write: Sending command RSP \n");
        }
        pdu.write(dcoRsp, UID, (byte) 1);

        if (ddo != null) {
            pdu.write(ddo, UID, (byte) 0);
        }
    }

}
