package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.*;


/**
 * DICOM Retrieve SOP Class.
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
public class DICOM_StdRetrieve extends DICOM_SOP {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Move request object. */
    public DICOM_CRequest cMoveRq;

    /** Move response object. */
    public DICOM_CResponse cMoveRsp;

    /** Uniquely identifies the current C-Move RQ */
    private int MSG_ID;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * This implements a DICOM Retrieve SOP Class.
     * 
     * @param UID describes type of retrieve
     */
    public DICOM_StdRetrieve(final String UID) {
        this.UID = UID;
        cMoveRq = new DICOM_CRequest(DICOM_Constants.COMMAND_CMoveRQ);
        cMoveRsp = new DICOM_CResponse(DICOM_Constants.COMMAND_CMoveRSP);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Returns the msgID for the outgoing C-Move Request.
     * 
     * @return msgID uniquely identifies the current C-Move RQ
     */
    public int getMsgID() {
        return (MSG_ID);
    }

    /**
     * Read a C-MOVE request - this is used when acting like server and responding to a move request.
     * 
     * @param pdu DOCUMENT ME!
     * @param dco DOCUMENT ME!
     * @param storePDU DOCUMENT ME!
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void read(final DICOM_PDUService pdu, final DICOM_Object dco, final DICOM_PDUService storePDU)
            throws DICOM_Exception {
    // MIPAV is not yet a server
    }

    /**
     * Sets the msgID for the outgoing C-Move Request.
     * 
     * @param id uniquely identifies the current C-Move RQ
     */
    public void setMsgID(final int id) {
        MSG_ID = id;
        cMoveRq.setMsgID(MSG_ID);
    }

    /**
     * Transfers a C-MOVE request to a SCP.
     * 
     * @param pdu PDU object
     * @param ddo DICOM data object
     * @param localAppTitle Appliation entity title of the local entity
     * @param dcmMove DICOM move object - not used here
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void write(final DICOM_PDUService pdu, final DICOM_Object ddo, final byte[] localAppTitle,
            final DICOM_Move dcmMove) throws DICOM_Exception {

        // int i =0;
        int ID;
        final byte[] outAETitle = new byte[16];
        final DICOM_Object ddoRsp = new DICOM_Object();
        final DICOM_Object dco = new DICOM_Object();

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: BEGIN to "
                    + new String(localAppTitle) + ".\n");
        }

        DICOM_Util.clearByteArray(outAETitle);
        DICOM_Util.copyByteArray(outAETitle, localAppTitle);

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: MOVE request.\n");
        }

        pdu.parseDICOMintoBuffer(ddo.copy(), pdu.pDataTF.getVRLinkedBuffer(), pdu.ioBuffer);
        cMoveRq.write(pdu, null, UID, null, ddo, outAETitle);

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: MOVE request completed.\n");
        }

        while (true) {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: PDU read started.\n");
            }

            pdu.readInObject(dco); // in here until images are read

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: PDU read completed.\n");
            }

            cMoveRsp.read(dco, pdu, ddoRsp);

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper()
                        + " DICOM_StdRetrieve.write: MOVE response read completed.\n");
            }

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: " + dco.toString("Command")
                        + "\n");
                // Preferences.debug( DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: " + dco.toString("Command")
                // + "\n");
            }

            if ( (dco.getInt16(DICOM_RTC.DD_Status) == DICOM_Constants.STATUS_MOVE_SUCCESS)
                    || (dco.getInt16(DICOM_RTC.DD_Status) == DICOM_Constants.STATUS_MOVE_WARNING)) {

                ID = dco.getInt16(DICOM_RTC.DD_MessageIDBeingRespondedTo);

                final int process = DICOMDisplayer.getRowFromID(ID);

                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: ID = " + ID
                            + " DICOM_StdRetrieve process = " + process + "\n");
                }

                if ( !DICOMDisplayer.getSucceeded()) {
                    DICOMDisplayer.setMessageType(process, DICOMDisplayer.STATUS);
                    pdu.showMessage("Error");
                    DICOMDisplayer.setMessageType(process, DICOMDisplayer.ERROR);
                    pdu.showMessage("Files not moved");
                    MipavUtil.displayError("Files not moved. Check server and storage destination\nin the Hosts tab.");
                } else {
                    DICOMDisplayer.setMessageType(process, DICOMDisplayer.STATUS);
                    pdu.showMessage("Success");
                }

                break;
            } else {

                if (dco.getInt16(DICOM_RTC.DD_Status) == DICOM_Constants.STATUS_MOVE_PENDING) {

                    // what should I do.
                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() + " DICOM_StdRetrieve.write: Status = "
                                + dco.getInt16(DICOM_RTC.DD_Status) + "\n");
                    }

                    final long startTime = System.currentTimeMillis();

                    // while (true) {
                    // if (System.currentTimeMillis() - startTime > 90000) break;
                    // }

                    // break;
                }
            }
        }
    }

}
