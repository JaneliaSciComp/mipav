package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.*;


/**
 * This is the DICOM verification class that defines functions to perform Verification to a DICOM image file archive
 * such as the Kodak PACS image file server located in NIH's Clinical Center. ("Pings" the server)
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
 * @author Sunita Munjal
 * @author Matthew McAuliffe, Ph.D.
 */
public class DICOM_Verification { // is an SOP {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Echo request object. */
    public DICOM_CRequest cEchoRQ;

    /** Echo response object. */
    public DICOM_CResponse cEchoRSP;

    /** Unique identifier. */
    public String UID = null;

    /** GUI frame where all DICOM messages are displayed. */
    private final ViewJFrameDICOMQuery frame;

    /** PDU service object. */
    private DICOM_PDUService pdu;

    /** The remote servers AE title. */
    private final String remoteAETitle;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     * 
     * @param _remoteAETitle remote(i.e. server) application entity title
     * @param _frame the frame where the update messages will be displayed
     */
    public DICOM_Verification(final String _remoteAETitle, final ViewJFrameDICOMQuery _frame) {
        remoteAETitle = _remoteAETitle;
        frame = _frame;
        UID = DICOM_Constants.UID_Verification;
        cEchoRQ = new DICOM_CRequest(DICOM_Constants.COMMAND_CEchoRQ);
        cEchoRSP = new DICOM_CResponse(DICOM_Constants.COMMAND_CEchoRSP);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Reads the verification request and responds with (write) a ECHO_SUCESS.
     * 
     * @param pdu PDU service class
     * @param dco DICOM command object
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void read(final DICOM_PDUService pdu, final DICOM_Object dco) throws DICOM_Exception {
        Preferences.debug("################### Before verifySOPClass \n");
        verifySOPClass(dco);
        Preferences.debug("################### Before cEchoRQ.read \n");
        cEchoRQ.read(dco);
        Preferences.debug("################### Before cEchoRSP.write \n");
        cEchoRSP.write(pdu, dco, UID, DICOM_Constants.STATUS_ECHO_SUCCESS, null, null, 0, 0, 0, 0);
    }

    /**
     * Initiate verification of connection between client and server (i.e. ping).
     */
    public void verify() {

        try {

            // define an instance of the PDU_Service class to set up the connection to the remote AE
            pdu = new DICOM_PDUService();
            pdu.connectClientToServer(remoteAETitle, true, null, null);
            write(pdu);
            pdu.close();
        } catch (final OutOfMemoryError error) {
            frame.appendSendMessage("Failed to connect " + remoteAETitle + "\n");
            displayError("Error: verify():" + error);
            pdu.close();

            return;
        } catch (final DICOM_Exception error) {
            frame.appendSendMessage("Failed to connect " + remoteAETitle + "\n");
            displayError("Error: verify():" + error);
            pdu.close();

            return;
        }

        frame.appendSendMessage("Verification to " + remoteAETitle + " - success" + "\n");
    }

    /**
     * Writes the UID_Verification via the PDU service.
     * 
     * @param pdu PUD service class
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void write(final DICOM_PDUService pdu) throws DICOM_Exception {
        final DICOM_Object dco = new DICOM_Object();

        cEchoRQ.setMsgID(DICOM_Util.getUniqueOddID16());
        Preferences.debug("################### Before Verify.write echoRQ \n");
        cEchoRQ.write(pdu, null, UID, null, null, null);
        Preferences.debug("################### Before pdu.read\n");
        pdu.readInObject(dco);
        Preferences.debug("################### cEchoRSP.read\n");
        cEchoRSP.read(dco, pdu, null);
    }

    /**
     * Displays an error in a frame.
     * 
     * @param error string that is displayed
     */
    private void displayError(final String error) {
        MipavUtil.displayError(error);
    }

    /**
     * Verifies the SOP class UID.
     * 
     * @param dco DICOM data object
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    private void verifySOPClass(final DICOM_Object dco) throws DICOM_Exception {
        String dcoUID;

        if (dco == null) {
            throw new DICOM_Exception("DCO is null");
        }

        dcoUID = dco.getStr(DICOM_RTC.DD_AffectedSOPClassUID);

        if (dcoUID == null) {
            throw new DICOM_Exception("SOP class UID is null");
        }

        if ( !dcoUID.equals(UID)) {
            throw new DICOM_Exception("Invalid SOP class UID");
        }
    }
}
