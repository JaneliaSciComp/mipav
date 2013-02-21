package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.*;

import java.net.Socket;
import java.util.*;


/**
 * The Protocol Data Units (PDU) - PDU_Service class is a wrapper of all the lower level DICOM messaging classes. This
 * class is the true implementor of the DICOM networking protocol.
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
public class DICOM_PDUService extends DICOM_Comms {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Abort connection request. */
    public DICOM_AAbortRQ abortRQ = new DICOM_AAbortRQ();

    /** Associate accept object. */
    public DICOM_AAssociateAC associateAC = new DICOM_AAssociateAC();

    /** Associate reject object. */
    public DICOM_AAssociateRJ associateRJ = new DICOM_AAssociateRJ();

    /** Associate request object. */
    public DICOM_AAssociateRQ associateRQ = new DICOM_AAssociateRQ();

    /** Reference to the DICOM GUI for displaying message information. */
    public DICOMDisplayer dicomMessageDisplayer = null;

    /** List of the results of a DICOM query. */
    public DICOM_ObjectList findResults = new DICOM_ObjectList();

    /** list of the results of a DICOM move. */
    public DICOM_ObjectList moveResults = new DICOM_ObjectList();

    /** Data transfer object. */
    public DICOM_PDataTF pDataTF = new DICOM_PDataTF();

    /** Release request. */
    public DICOM_AReleaseRQ releaseRQ;

    /** Release response. */
    public DICOM_AReleaseRSP releaseRSP;

    /** socket used to connect to server. */
    public DICOM_Socket socket = null;

    /** Buffer used to hold image data. */
    protected ByteBuffer compData = null;

    /** A list of Presentation contexts. */
    Vector<DICOM_PresentationContextAccept> PresentationContexts = new Vector<DICOM_PresentationContextAccept>();

    /** A hash table of proposed abstract syntaxes where: value = AbstractSyntaxes; key = UID. */
    Hashtable<String,DICOM_PDUItemType> proposedAbstractSyntaxs = new Hashtable<String,DICOM_PDUItemType>();

    /** Buffer used in sending DICOM image. */
    DICOM_FileIO ioBuffer = new DICOM_FileIO();

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * PDU_Service constructor.
     */
    public DICOM_PDUService() {

        setApplicationContext(DICOM_Constants.UID_ApplicationContext);
        releaseRQ = new DICOM_AReleaseRQ();
        releaseRSP = new DICOM_AReleaseRSP();
        socket = new DICOM_Socket();
        setIncomingEndianess(DICOM_Comms.BIG_ENDIAN);
        setOutgoingEndianess(DICOM_Comms.BIG_ENDIAN);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Convert an incoming DICOM network data stream into a DICOMObject.
     * 
     * @param vrBuffer the I/O DICOM_CommsLink.java from which to read the data stream
     * @param dobj the DICOM_Object.java to fill
     */
    public static void parseRawVRIntoDICOM(final DICOM_Comms vrBuffer, final DICOM_Object dobj) {
        DICOM_VR VR;
        int group, element, length;

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " ****** PDU_Service.parseRawVRIntoDICOM: Buffer length = "
                    + vrBuffer.getIncomingSize() + " buffer startIndex = "
                    + ((ByteBuffer) (vrBuffer.incomingBuffers.elementAt(0))).startIndex + " buffer endIndex = "
                    + ((ByteBuffer) (vrBuffer.incomingBuffers.elementAt(0))).endIndex + "\n");
        }

        //final int start = 0;

        try {

            while (vrBuffer.getIncomingSize() > 0) {
                group = vrBuffer.readShort16();
                element = vrBuffer.readShort16();
                length = vrBuffer.readInt32();
                /*
                 * if ((length == 0) && (start == 0)) { start = 1;
                 *  // burn preample of zeros + DICM for (int i = 0; i < 31; i++) { vrBuffer.readInt32(); // System.out.println (" i " +
                 * vrBuffer.readInt32()); }
                 *  }
                 */

                // Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.parseRawVRIntoDICOM: " +
                // " group = " + Integer.toHexString(group) +
                // " element = " + Integer.toHexString(element) +
                // " length = " + length +
                // " length Hex = " + Integer.toHexString(length) + "\n");
                VR = DICOM_VR.readData(group, element, length, vrBuffer);
                dobj.push(VR);
            }
        } catch (final DICOM_Exception de) {
            de.printStackTrace();
        }
    }

    /**
     * Parses the server info string and returns a tokenized array of strings that where deliniated by ";" (See MIPAV
     * preference file).
     * 
     * @param info A string deliniated by ";" that contains (AETitle, Alias, IP, PORT).
     * 
     * @return server info parsed into an array of strings (AETitle, Alias, IP, PORT).
     */
    public static String[] parseServerInfo(final String info) {

        if (info == null) {
            return null;
        }

        final StringTokenizer tok = new StringTokenizer(info, ";");
        final String[] values = new String[tok.countTokens()];
        int i = 0;

        while (tok.hasMoreTokens()) {
            values[i] = tok.nextToken();
            i++;
        }

        return values;
    }

    /**
     * Adds proposed abstract syntax to hash table.
     * 
     * @param SOP_UID The abstract syntax to be added the list of proposed Abstract Syntaxs
     */
    public void addAbstractSyntax(final String SOP_UID) {
        final DICOM_PDUItemType abstractSyntax = new DICOM_PDUItemType(DICOM_PDUTypeBase.PDUTYPE_AbstractSyntax);
        abstractSyntax.setUID(SOP_UID);
        proposedAbstractSyntaxs.put(SOP_UID, abstractSyntax);
    }

    /**
     * Adds all proposed abstract syntaxes to hash table.
     */
    public void addAllSupportedAbstractSyntaxes() {

        // List of purposed syntaxes supported by MIPAV. Should store this is a text file;
        // Matt add JPEG !!!!!! JPEG (70) is a transfer syntax.
        addAbstractSyntax(DICOM_Constants.UID_Verification);
        addAbstractSyntax(DICOM_Constants.UID_GECTStorage);
        addAbstractSyntax(DICOM_Constants.UID_GEMRStorage);
        addAbstractSyntax(DICOM_Constants.UID_CRStorage);
        addAbstractSyntax(DICOM_Constants.UID_CTStorage);
        addAbstractSyntax(DICOM_Constants.UID_MRStorage);
        addAbstractSyntax(DICOM_Constants.UID_NMStorage);
        addAbstractSyntax(DICOM_Constants.UID_OldNMStorage); // This might fix KODAK PACS anomoly ?
        addAbstractSyntax(DICOM_Constants.UID_PetStorage);

        // addAbstractSyntax( DICOM_Constants.UID_StandalonePetCurve);
        // addAbstractSyntax( DICOM_Constants.UID_StandaloneCurveStorage);
        // addAbstractSyntax( DICOM_Constants.UID_StandaloneModalityLUTStorage);
        // addAbstractSyntax( DICOM_Constants.UID_StandaloneOverlayStorage);
        // addAbstractSyntax( DICOM_Constants.UID_StandaloneVOILUTStorage);
        addAbstractSyntax(DICOM_Constants.UID_SCStorage);
        addAbstractSyntax(DICOM_Constants.UID_USStorage);
        addAbstractSyntax(DICOM_Constants.UID_USMultiframeStorage);
        addAbstractSyntax(DICOM_Constants.UID_XRayAngioStorage);
        addAbstractSyntax(DICOM_Constants.UID_XRayFluoroStorage);

        addAbstractSyntax(DICOM_Constants.UID_PatientRootQuery);
        addAbstractSyntax(DICOM_Constants.UID_PatientRootRetrieve);
        addAbstractSyntax(DICOM_Constants.UID_PatientStudyOnlyQuery);
        addAbstractSyntax(DICOM_Constants.UID_PatientStudyOnlyRetrieve);
        addAbstractSyntax(DICOM_Constants.UID_StudyRootQuery);
        addAbstractSyntax(DICOM_Constants.UID_StudyRootRetrieve);
    }

    /**
     * Utility function to collect the results of a DICOM query. Fills the findResults list.
     * 
     * @param dco the received command object
     * @param ddo the received data object
     * 
     */
    public void addFindResultToList(final DICOM_Object dco, final DICOM_Object ddo) {

        if (dco == null) {
            findResults.removeAllElements(); // clear list for new data
        } else if ( (dco != null) && (ddo != null)) {
            findResults.addElement(ddo.copy()); // Valid DICOM data object
        }
    }

    /**
     * Adds just the verification syntax for DICOM verification process.
     */
    public void addJustVerificationAbstractSyntax() {
        addAbstractSyntax(DICOM_Constants.UID_Verification);
    }

    /**
     * Disconnect socket and free up memory.
     */
    public void close() {

        if (socket.connected) {

            try {
                releaseRQ.write(this);
                releaseRSP.read(this);
            } catch (final DICOM_Exception e) {

                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    Preferences.debug(DICOM_Util.timeStamper() + " DICOMPDUService.close:(" + this.hashCode()
                            + ")  release failed.");
                }
            }
        }

        socket.close();
        finalize();
    }

    /**
     * This is a simple interface to establish an association over a socket from the client (i.e. MIPAV) (SCU) side.
     * 
     * @param remoteAppTitle the application entity title to attempt to connect to
     * @param verification if true only add Verification UID syntax.
     * @param transferSyntax if this is passed in, only add this transfer syntax to the the available pres contexts
     * @param classUID used in conjunction with transfer syntax to specify ONE type of abstract syntax to use
     * @throws DICOM_Exception Throws an error if there was a problem connecting to the server.
     */
    public void connectClientToServer(final String remoteAppTitle, final boolean verification,
            final String transferSyntax, final String classUID) throws DICOM_Exception {
        byte itemType;

        setLocalAddress(getLocalAppTitle());

        if (verification == true) {
            addJustVerificationAbstractSyntax();
        } else {
            addAllSupportedAbstractSyntaxes();
        }

        associateRQ.clearPresentationContexts();

        /** We did not pass in a transfer syntax so we are giving a list of ALL we support */
        if (transferSyntax == null) {

            for (final Enumeration<DICOM_PDUItemType> e = proposedAbstractSyntaxs.elements(); e.hasMoreElements();) {
                final DICOM_PresentationContext presContext = new DICOM_PresentationContext();
                final DICOM_PDUItemType trnSyntax = new DICOM_PDUItemType(DICOM_PDUTypeBase.PDUTYPE_TransferSyntax);
                trnSyntax.setUID(DICOM_Constants.UID_TransferLITTLEENDIAN);
                presContext.addTransferSyntax(trnSyntax);
                presContext.setAbstractSyntax((DICOM_PDUItemType) e.nextElement());
                associateRQ.addPresentationContext(presContext);
            }

            for (final Enumeration<DICOM_PDUItemType> e = proposedAbstractSyntaxs.elements(); e.hasMoreElements();) {
                final DICOM_PresentationContext presContext = new DICOM_PresentationContext();
                final DICOM_PDUItemType trnSyntax = new DICOM_PDUItemType(DICOM_PDUTypeBase.PDUTYPE_TransferSyntax);
                trnSyntax.setUID(DICOM_Constants.UID_TransferLITTLEENDIANEXPLICIT);
                presContext.addTransferSyntax(trnSyntax);
                presContext.setAbstractSyntax((DICOM_PDUItemType) e.nextElement());
                associateRQ.addPresentationContext(presContext);
            }
        } else { // we passed in a transfer syntax and classUID, so we only want to add that one

            final DICOM_PresentationContext presContext = new DICOM_PresentationContext();
            final DICOM_PDUItemType trnSyntax = new DICOM_PDUItemType(DICOM_PDUTypeBase.PDUTYPE_TransferSyntax);
            trnSyntax.setUID(transferSyntax);
            presContext.addTransferSyntax(trnSyntax);
            final DICOM_PDUItemType abstractSyntax = new DICOM_PDUItemType(DICOM_PDUTypeBase.PDUTYPE_AbstractSyntax);
            abstractSyntax.setUID(classUID);
            presContext.setAbstractSyntax(abstractSyntax);
            associateRQ.addPresentationContext(presContext);
        }
        associateRQ.setUserInformation(constructUserInformation());
        associateRQ.setCalledAppTitle(remoteAppTitle.getBytes());

        // Matt - 7/17 Double check if problems Q/R
        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.connectClientToServer:(" + this.hashCode()
                    + ")  Open socket to server. \n");
            Preferences.debug(DICOM_Util.timeStamper() + "AE Title of server = " + remoteAppTitle + "  IP = "
                    + Preferences.getIP(remoteAppTitle) + "  PORT = " + Preferences.getPort(remoteAppTitle) + "\n");
        }

        socket.open(Preferences.getIP(remoteAppTitle), Preferences.getPort(remoteAppTitle));

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.connectClientToServer: (" + this.hashCode()
                    + ") Writing association request.\n");
        }

        associateRQ.write(this);

        itemType = peekFirstByte();

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.connectClientToServer: (" + this.hashCode()
                    + ")  peekFirstByte (itemType) = " + itemType + " ( "
                    + DICOM_PDUTypeBase.convertItemTypeToString(itemType) + " )\n");
        }

        switch (itemType) {

            case DICOM_PDUTypeBase.PDUTYPE_AAssociateAC:
                associateAC.read(this);
                if ( !interrogateAAssociateAC()) {
                    socket.close();

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences
                                .debug(DICOM_Util.timeStamper() + " PDU_Service.connectClientToServer: ("
                                        + this.hashCode() + ") Association to " + remoteAppTitle
                                        + " , but rejected locally.\n");
                    }

                    throw new DICOM_Exception("Association to " + remoteAppTitle + " , but rejected locally.\n");
                }

                break;

            case DICOM_PDUTypeBase.PDUTYPE_AAssociateRJ:
                associateRJ.read(this);
                socket.close();
                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.connectClientToServer:("
                            + this.hashCode() + ") Association to " + remoteAppTitle + " was rejected.\n");
                }

                throw new DICOM_Exception("Association to " + remoteAppTitle + " was rejected");

            default:

                // fails
                socket.close();
                throw new DICOM_Exception("Error: " + remoteAppTitle + " to MIPAV's association request.\n");
        }
    }

    /**
     * Returns the association request.
     * 
     * @return the association request
     */
    public DICOM_AAssociateRQ getAAssociateRQ() {
        return associateRQ;
    }

    /**
     * Implementation class uid.
     * 
     * @return the UID
     */
    public String getImplementationClassUID() {
        return ("1.2.840.34379.17"); // Bogus number Matt made up for Implementation UID
    }

    /**
     * Implementation version uid.
     * 
     * @return The version.
     */
    public String getImplementationVersion() {
        return ("NIH MIPAV 05 1.1");
    }

    /**
     * Returns the AETitle we use to identify ourself (our local AppTitle name).
     * 
     * @return Returns the AETitle we use to identify ourself (our local AppTitle name).
     * 
     * @throws DICOM_Exception Throws an error if there was a problem getting the default AETitle.
     */
    public String getLocalAppTitle() throws DICOM_Exception {
        return (DICOM_PDUService.parseServerInfo(Preferences.getProperty(Preferences.getDefaultStorageKey()))[0]);
    }

    /**
     * Establishes an association over a socket from the server (SCP) side.
     * 
     * @param sock the socket (connected) to work with
     * 
     * @throws DICOM_Exception Throws an error if there was a problem connecting to the server.
     */
    public void handleConnectionFromServer(final Socket sock) throws DICOM_Exception {
        setLocalAddress(getLocalAppTitle());
        socket.attach(sock);
        associateRQ.clearPresentationContexts();
        associateRQ.read(this); // Wait for associationRQ

        if ( !interrogateAAssociateRQ() && (socket != null)) {
            socket.close();

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.handleConnectionFromServer:("
                        + this.hashCode() + ") MIPAV rejected association from "
                        + new String(associateRQ.getCallingAppTitle()) + "\n");
            }

            throw new DICOM_Exception(" MIPAV rejected association from "
                    + new String(associateRQ.getCallingAppTitle()));
        }

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.handleConnectionFromServer:(" + this.hashCode()
                    + ")  association from " + new String(associateRQ.getCallingAppTitle()) + "\n");
        }
    }

    /**
     * Converts a DICOMObject into an outgoing DICOM data stream.
     * 
     * @param dobj the DICOMObject to send
     * @param vrBuffer the I/O Buffer to write to
     */
    public void parseDICOMIntoRawVR(final DICOM_Object dobj, final DICOM_Comms vrBuffer) {
        DICOM_VR VR;
        int length;
        boolean padByte = false;

        while ( (VR = dobj.pop()) != null) {
            vrBuffer.writeShort16(VR.group);
            vrBuffer.writeShort16(VR.element);

            length = VR.data.length;
            padByte = false;

            if ( (length % 2) != 0) { // not even
                length++;
                padByte = true;
            }

            vrBuffer.writeInt32(length);
            // Preferences.debug("DICOM_PDU_SERV parseDICOMIntoRawVR: "+Integer.toString(VR.group, 0x10) + "," +
            // Integer.toString(VR.element,0x10)+"; VR length = " + length+ " (" +
            // Integer.toString(length, 0x10) + ")\n");

            DICOM_VR.writeData(VR, vrBuffer);

            if (padByte) {
                final int typeCode = DICOM_RTC.getTypeCode(DICOM_RTC.unknownDDType(VR.group, VR.element));
                byte padCharacter = ' '; // clever: always padding space, if we need to pad

                switch (typeCode) {

                    case DICOM_RTC.TYPE_CS:
                    case DICOM_RTC.TYPE_IS:
                    case DICOM_RTC.TYPE_LO:
                    case DICOM_RTC.TYPE_PN:
                    case DICOM_RTC.TYPE_SH:
                        padCharacter = (byte) ' ';
                        break;

                    case DICOM_RTC.TYPE_UNKNOWN:
                        padCharacter = (byte) ' '; // space
                        break;

                    case DICOM_RTC.TYPE_OB:
                    case DICOM_RTC.TYPE_UI:
                    case DICOM_RTC.TYPE_DT:
                        padCharacter = 0; // for completeness: null char padded
                        break;

                    default:
                }

                vrBuffer.writeByte(padCharacter);
            }
        }
    }

    /**
     * Actual binary data from the port.
     * 
     * @param data buffer to store the data
     * @param count number of bytes to be read
     * 
     * @return number of bytes actually read
     * 
     * @throws DICOM_Exception Throws an error if there was a problem reading the socket.
     */
    public int readBinary(final byte[] data, final int count) throws DICOM_Exception {
        final int actual = socket.readBinary(data, count);

        return (actual);
    }

    /**
     * Loads a DICOM image from a file.
     * 
     * @param filename The name of the file to load.
     * 
     * @return Returns the DICOM data object (image).
     * 
     * @throws DICOM_Exception Throws an error if there was a problem reading in the image.
     */
    public DICOM_Object readDICOMDataObjectFromFile(final String filename) throws DICOM_Exception {

        DICOM_Object ddo = null;

        if (ioBuffer != null) {
            try {
                ioBuffer.finalize();
            } catch (final Throwable f) {}

            ioBuffer.close();
        }

        ioBuffer = new DICOM_FileIO();

        if (ioBuffer.openForRead(filename)) {

            while (true) {

                try {
                    ioBuffer.readFill(32768);
                } catch (final DICOM_Exception e) {
                    break;
                }
            }

            ddo = new DICOM_Object();
            ioBuffer.setIncomingEndianess(DICOM_Comms.LITTLE_ENDIAN);

            // Strips preamble and group 2 tags from ioBuffer if present
            ioBuffer.seekToEndOfGroupTwoTags();

        }

        return (ddo);
    }

    /**
     * The main method for reading incoming DICOM messages. true: complete PData unit received, socket still open false:
     * release request received, then close the socket
     * 
     * @param DICOMobj The DICOM object. Null if pDataTF.getVRLinkedBuffer holds data.
     * 
     * @return true complete PData unit received, socket still open false release request received, then close the
     *         socket
     * 
     * @throws DICOM_Exception Throws an error if a problem reading in the object or data is encountered.
     */
    public boolean readInObject(final DICOM_Object DICOMobj) throws DICOM_Exception {
        byte itemType;

        while (true) {
            itemType = peekFirstByte();

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " DICOM_PDUService.read:(" + this.hashCode()
                        + ") peekFirstByte (itemType) = " + itemType + " ( "
                        + DICOM_PDUTypeBase.convertItemTypeToString(itemType) + " )\n");

                Preferences.debug(DICOM_Util.timeStamper() + " DICOM_PDUService.read:(" + this.hashCode()
                        + ") pDataTF length = " + pDataTF.length + "\n");
            }

            boolean continuePData = false;

            if (pDataTF.length > 0) {
                itemType = DICOM_PDUTypeBase.PDUTYPE_PDataTF;
                continuePData = true;
                pDataTF.readBody(this);

                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    Preferences.debug(DICOM_Util.timeStamper() + " DICOM_PDUService.read:(" + this.hashCode()
                            + ") Read pDataTF.readBody completed.\n");
                }
            }

            switch (itemType) {

                case DICOM_PDUTypeBase.PDUTYPE_AReleaseRQ:
                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.read:(" + this.hashCode()
                                + ") Normal release in progress\n");
                    }

                    releaseRQ.read(this);
                    releaseRSP.write(this);
                    socket.close();
                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.read:(" + this.hashCode()
                                + ") Normal release completed\n");
                    }

                    return (false);

                case DICOM_PDUTypeBase.PDUTYPE_PDataTF:
                    if ( !continuePData) {

                        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                            Preferences.debug(DICOM_Util.timeStamper() + " DICOM_PDUService.read:(" + this.hashCode()
                                    + ") Read in progress\n");
                        }

                        pDataTF.read(this);
                    }

                    if (pDataTF.isReadComplete()) {

                        // given the presentation contexts, in the future we might choose a transfer syntax
                        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                            Preferences.debug(DICOM_Util.timeStamper() + " DICOM_PDUService.read:(" + this.hashCode()
                                    + ") Read in completed\n");
                        }

                        pDataTF.getVRLinkedBuffer().setIncomingEndianess(DICOM_Comms.LITTLE_ENDIAN);

                        if (DICOMobj != null) {
                            DICOM_PDUService.parseRawVRIntoDICOM(pDataTF.getVRLinkedBuffer(), DICOMobj);
                        } else {
                            buildRawBuffer(pDataTF.getVRLinkedBuffer());
                        }

                        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                            Preferences.debug(DICOM_Util.timeStamper() + " DICOM_PDUService.read:(" + this.hashCode()
                                    + ") Parse VR into DICOM completed\n");
                        }

                        return (true);
                    }

                    break;

                case DICOM_PDUTypeBase.PDUTYPE_AAbortRQ:
                    abortRQ.read(this);

                    final int reason = abortRQ.reason;
                    final int source = abortRQ.source;
                    abort();
                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() + " remote application aborted the socket - "
                                + "source: " + source + " " + "reason: " + reason);
                    }

                    throw new DICOM_Exception(DICOM_Util.timeStamper() + " remote application aborted the socket - "
                            + "source - " + source + " " + "reason - " + reason);

                default:
                    abort();
                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() + " aborted socket - parsed itemType " + itemType);
                    }

                    throw new DICOM_Exception("Aborted socket:  Parsed item type  = " + itemType);
            }
        }
    }

    /**
     * Saves the DDO to the file name supplied as a method parameter.
     * 
     * @param fileName name of the file where the ddo is to be stored
     * 
     * @throws DICOM_Exception Throws error if a problem writing the image to file is encountered.
     */
    public void saveImageToFile(final ByteBuffer dataObj, final String fileName) throws DICOM_Exception {
        final DICOM_FileIO ioBuffer = new DICOM_FileIO();

        if (ioBuffer.openForWrite(fileName)) {
            ioBuffer.sendBinary(dataObj.data, dataObj.length());
            ioBuffer.close();
        }
    }

    /**
     * Actual binary data to be sent.
     * 
     * @param data buffer of data send out the port (socket)
     * @param offset starting offset
     * @param count number of bytes to be sent
     * 
     * @throws Throws an error when a problem is encoutered writing.
     */
    public void sendBinary(final byte[] data, final int offset, final int count) throws DICOM_Exception {
        socket.writeBinary(data, offset, count);
    }

    /**
     * Sets the display viewer.
     * 
     * @param displayer The GUI used to display
     */
    public void setDICOMMessageDisplayer(final DICOMDisplayer displayer) {
        dicomMessageDisplayer = displayer;
    }

    /**
     * Accept all.
     * 
     * @param AEtitle the calling (remote AE Title)
     * 
     * @return always returns true
     * 
     * @throws DICOM_Exception Not used at this time!
     */
    public boolean shouldWeAcceptLocalAppTitle(final String AEtitle) throws DICOM_Exception {
        return (true);
    }

    /**
     * Accept only AETitles listed in the server host table (remote) *** We accept all AETitles at this time!
     * 
     * @param remoteAETitle - calling (remote AE Title)
     * 
     * @return true if the remoteAETitle has been found in the server host table (host table in the user preference file
     * 
     * @throws DICOM_Exception Not used at this time.
     */
    public boolean shouldWeAcceptRemoteAppTitle(final String remoteAETitle) throws DICOM_Exception {

        // If one wishes to limit servers to the list in the host table that are able to send
        // to MIPAV uncomment these lines
        // if (Preferences.getIP(remoteAETitle) == null) {
        // return ( false );
        // }
        return (true);
    }

    /**
     * Shows a message in the GUI message panel.
     * 
     * @param str the message to be shown.
     */
    public void showMessage(final String str) {

        if (dicomMessageDisplayer != null) {
            DICOMDisplayer.setMessageType(dicomMessageDisplayer.updateRow(), DICOMDisplayer.STATUS);
            dicomMessageDisplayer.showMessage(str);
        } else {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " DICOM_PDUService.showMessage "
                        + DICOM_Util.timeStamper() + " " + str + "\n");
            }
        }
    }

    /**
     * Writes the image (loaded into ioBuffer from file) out the port.
     * 
     * @param transferSyntax Transfer syntax
     * @param ddo
     * @param sopClassUID SOP class UID
     * @param messageHeader 1 = indicates command, 0 = data.
     * 
     * @throws DICOM_Exception Throws error if an error is encountered writing the image out the port.
     */
    public void write(final String transferSyntax, final DICOM_Object ddo, final String sopClassUID,
            final byte messageHeader) throws DICOM_Exception {

        pDataTF.getVRLinkedBuffer().setOutgoingEndianess(DICOM_Comms.BIG_ENDIAN);
        // The incomming buffer should contain the DICOM file read from disk.
        pDataTF.getVRLinkedBuffer().outgoingBuffers = ioBuffer.incomingBuffers;
        pDataTF.getVRLinkedBuffer().outBuffersLength = ioBuffer.inBuffersLength;
        pDataTF.write(this, associateRQ.getPresentationContextID(transferSyntax, sopClassUID), messageHeader);

        // clean up buffers
        ioBuffer.incomingBuffers.removeAllElements();
        ioBuffer.inBuffersLength = 0;
        ioBuffer.close();
        ioBuffer = null;

        pDataTF.getVRLinkedBuffer().outgoingBuffers.removeAllElements();
    }

    /**
     * Writes the DICOMObj out through the socket.
     * 
     * @param DICOMObj the DICOM object to be sent.
     * @param sopClassUID SOP class UID
     * @param messageHeader Value of 0 indicates data message. Value of 1 indicates command message.
     * 
     * @throws DICOM_Exception Indicates erroring writing DICOM objects.
     */
    public void write(final DICOM_Object DICOMObj, final String sopClassUID, final byte messageHeader)
            throws DICOM_Exception {

        pDataTF.getVRLinkedBuffer().setOutgoingEndianess(DICOM_Comms.LITTLE_ENDIAN);
        parseDICOMIntoRawVR(DICOMObj, pDataTF.getVRLinkedBuffer()); // vrLinkBuffer holds VR info.
        pDataTF.write(this, associateRQ.getPresentationContextID(sopClassUID), messageHeader);
    }

    /**
     * Used to make sure socket has properly been closed.
     * 
     */
    protected void finalize() {

        abortRQ = null;
        associateAC = null;
        associateRJ = null;
        associateRQ = null;
        dicomMessageDisplayer = null;
        findResults = null;
        moveResults = null;

        if (pDataTF != null) {
            pDataTF.finalize();
            pDataTF = null;
        }
        releaseRQ = null;
        releaseRSP = null;
        socket = null;

        if (PresentationContexts != null) {
            PresentationContexts.removeAllElements();
            PresentationContexts = null;
        }

        if (PresentationContexts != null) {
            proposedAbstractSyntaxs.clear();
            proposedAbstractSyntaxs = null;
        }
        if (ioBuffer != null) {
            ioBuffer.close();
            ioBuffer.finalize();
        }
        ioBuffer = null;

        if (compData != null) {
            compData.finalize();
        }
        compData = null;

        try {
            socket.close();
        } catch (final Exception e) {}

        super.finalize();
    }

    /**
     * Gets the transfer syntax ID of the data.
     * 
     * @return String the transfer syntax UID.
     */
    protected String getTransferSyntaxID() {
        DICOM_PresentationContextAccept pca;
        final Vector pcaVector = associateAC.getPresentationContextes();

        for (int i = 0; i < pcaVector.size(); i++) {
            pca = (DICOM_PresentationContextAccept) pcaVector.elementAt(i);
            if (pDataTF.PDVPresContID == pca.presentationContextID) {
                return (pca.trnSyntax.getUID());
            }
        }

        return null;
    }

    /**
     * Aborts an association and closes the socket.
     */
    private void abort() {

        try {
            abortRQ.write(this);
            socket.close();
        } catch (final Exception e) {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " DICOMPDUService.abort(): failed to abort socket.");
            }
        }
    }

    /**
     * Fills the compData buffer from the vrBuffer.incomming buffers.
     * 
     * @param vrBuffer DICOM_Comms
     */
    private void buildRawBuffer(final DICOM_Comms vrBuffer) {

        try {
            if (compData == null || compData.length() < vrBuffer.getIncomingSize()) {
                compData = new ByteBuffer(vrBuffer.getIncomingSize());
            }
            compData.endIndex = vrBuffer.getIncomingSize();
            vrBuffer.read(compData.data, vrBuffer.getIncomingSize());
        } catch (final DICOM_Exception de) {
            de.printStackTrace();
        }

    }

    /**
     * Determines wheather or not this transfer syntax is support by MIPAV.
     * 
     * @param trnSyntax transfer syntax we need to check to see if we support it
     * 
     * @return true if we support the transfer syntax - false if not
     */
    private boolean canWeHandleTransferSyntax(final DICOM_PDUItemType trnSyntax) {

        /** is this correct? can we only handle implicit little endian...? */
        if (DICOM_TransferSyntaxUtil.convertToAlias(trnSyntax.getUID()) == DICOM_TransferSyntaxUtil.IMPLICIT_LITTLE_ENDIAN) {
            return (true);
        }

        return (false);
    }

    /**
     * Fills the with MIPAV information.
     * 
     * @return the user information object
     */
    private DICOM_UserInformation constructUserInformation() {
        final DICOM_UserInformation userInfo = new DICOM_UserInformation();

        userInfo.maxSubLength.setMaxLength(DICOM_Constants.MAXSUBLENGTH);
        userInfo.implementationClass.setUID(getImplementationClassUID());
        userInfo.implementationVersion.setUID(getImplementationVersion());

        return (userInfo);
    }

    /**
     * Decompose association ack.
     * 
     * @return if at least one is an acceptable syntax return true else return false.
     */
    private boolean interrogateAAssociateAC() {
        DICOM_PresentationContextAccept pca;
        boolean atLeastOneGood;

        pDataTF.setOutgoingBlockSize(associateAC.getUserInformation().maxSubLength.getMaxLength());

        PresentationContexts.removeAllElements();
        atLeastOneGood = false;

        for (int i = 0; i < associateAC.getPresentationContextes().size(); i++) {
            pca = (DICOM_PresentationContextAccept) (associateAC.getPresentationContextes().elementAt(i));

            if (pca.result == 0) {
                PresentationContexts.addElement(pca);
                atLeastOneGood = true;

                // Need more debug here !!!
                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    Preferences.debug(DICOM_Util.timeStamper()
                            + " PDU_Service.interrogateAAssociateAC -  : ID =  "
                            + pca.presentationContextID
                            + ", abstract syntax = "
                            + associateRQ.getPresentationContextFromID(pca.presentationContextID)
                            + " - "
                            + DICOM_Constants.convertUIDToString(associateRQ
                                    .getPresentationContextFromID(pca.presentationContextID)) + ", transfer syntax = "
                            + pca.trnSyntax.getUID() + "\n");
                }
            }
        }

        if ( !atLeastOneGood) {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.interrogateAAssociateAC: : "
                        + " Did not receive an  presentation context. \n");
            }

            return (false); // return failed
        }
        return (true); // return success
    }

    /**
     * Decompose association request and send proper response.
     * 
     * @return Returns false if it fails.
     * 
     * @exception DICOM_Exception Throws error if an error is encountered writing response to association.
     */
    private boolean interrogateAAssociateRQ() throws DICOM_Exception {
        boolean atLeastOne;
        DICOM_PresentationContext presContext;
        DICOM_PDUItemType trnSyntax;

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.interrogateAAssociateRQ: Begin.\n");
        }

        pDataTF.setOutgoingBlockSize(associateRQ.getUserInformation().maxSubLength.getMaxLength());

        if ( !shouldWeAcceptRemoteAppTitle(new String(associateRQ.getCallingAppTitle()))) {
            associateRJ.source = 1;
            associateRJ.reason = 3;
            associateRJ.result = 1;
            associateRJ.write(this);

            return (false);
        }

        // Next if - I don't think we need.
        if ( !shouldWeAcceptLocalAppTitle(new String(associateRQ.getCallingAppTitle()))) {
            associateRJ.source = 1;
            associateRJ.reason = 7;
            associateRJ.result = 1;
            associateRJ.write(this);

            return (false);
        }

        // Next if - I don't think we need.
        if ( !shouldWeAcceptApplicationContext(associateRQ.getApplicationContext())) {
            associateRJ.source = 1;
            associateRJ.reason = 2; // Application context not supported
            associateRJ.result = 1;
            associateRJ.write(this);

            return (false);
        }

        // Transfer the information over to the A-ASSOCIATE-AC Class
        associateAC.setCallingAppTitle(associateRQ.getCallingAppTitle());
        associateAC.setCalledAppTitle(associateRQ.getCalledAppTitle());

        associateAC.setApplicationContext(associateRQ.getApplicationContext());

        // Find acceptable presentation context(s)
        atLeastOne = false;
        associateAC.resetPresentationContext();

        final Vector arrayPresContexts = associateRQ.getPresentationContexts();

        for (int i = 0; i < arrayPresContexts.size(); i++) {
            presContext = (DICOM_PresentationContext) arrayPresContexts.elementAt(i);

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(" ***PDU_Service.interrogateAAssociateRQ: presentation array size = "
                        + arrayPresContexts.size() + "\n");
            }

            final Vector arrayTrnSyntax = presContext.trnSyntax;
            // System.err.println("arrayPresContexts.size() is " + arrayPresContexts.size());
            // System.err.println("presContext ID: " + presContext.presentationContextID);
            for (int j = 0; j < presContext.trnSyntax.size(); j++) {
                final DICOM_PresentationContextAccept pca = new DICOM_PresentationContextAccept();
                pca.presentationContextID = presContext.presentationContextID;

                if ( !shouldWeAcceptAbstractSyntax(presContext.absSyntax)) {

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper()
                                + " PDU_Service.interrogateAAssociateRQ: presentation context NOT  = "
                                + presContext.absSyntax.getUID() + " "
                                + DICOM_Constants.convertUIDToString(presContext.absSyntax.getUID())
                                + " presentation context ID = " + pca.presentationContextID + "\n");
                    }

                    pca.result = 3; // 3 = abstract syntaxes not supported by us
                } else {

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper()
                                + " PDU_Service.interogateAAssociateRQ: presentation  = "
                                + presContext.absSyntax.getUID() + " "
                                + DICOM_Constants.convertUIDToString(presContext.absSyntax.getUID())
                                + " presentation context ID = " + pca.presentationContextID + "\n");
                    }
                }

                trnSyntax = (DICOM_PDUItemType) arrayTrnSyntax.elementAt(j);
                pca.trnSyntax.setUID(trnSyntax.getUID());
                // System.err.println("Transfer Syntax: " + trnSyntax.toString());
                if (canWeHandleTransferSyntax(trnSyntax)) {
                    pca.result = 0; // 0 =
                    atLeastOne = true;

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper()
                                + " PDU_Service.interrogateAAssociateRQ: transfer syntax  = " + trnSyntax.getUID()
                                + " " + DICOM_TransferSyntaxUtil.convertToReadableString(trnSyntax.getUID()) + "\n");
                    }
                } else if (pca.result != 0) {

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper()
                                + " PDU_Service.interrogateAAssociateRQ: transfer syntax NOT  = " + trnSyntax.getUID()
                                + " " + DICOM_TransferSyntaxUtil.convertToReadableString(trnSyntax.getUID()) + "\n");
                    }

                    pca.result = 4; // 4 = transfer syntaxes not supported us
                }

                // add presentation context to list accept list

                /**
                 * matt & ben : when you have and accept and reject under same presentation ID GE server did not like
                 * (aborted)... so don't tell them you are rejecting anything
                 */
                if (pca.result != 4) {
                    associateAC.addPresentationContextAccept(pca);
                } else {
                    Preferences.debug("equal to four, not adding");
                }

                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    Preferences.debug("\n");
                }
            }
        }

        associateAC.setUserInformation(constructUserInformation());

        if ( !atLeastOne) {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.interrogateAAssociateRQ:(" + this.hashCode()
                        + ")  MIPAV does not support any abstract and/or transfer syntax(s)" + "\n");
            }

            associateAC.write(this); //

            return (false);
        }

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.interrogateAAssociateRQ:(" + this.hashCode()
                    + ")  Sending list of  presentation contexts." + "\n");
        }

        associateAC.write(this); // Send a reply !!!!!!!!!

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " PDU_Service.interrogateAAssociateRQ:(" + this.hashCode()
                    + ")  Completed sending list of  presentation contexts." + "\n");
        }

        return (true);
    }

    /**
     * Sets the application context UID.
     * 
     * @param applicationUID The application context UID.
     */
    private void setApplicationContext(final String applicationUID) {
        associateRQ.setApplicationContextUID(applicationUID);
    }

    /**
     * Sets the local address of the calling application entity.
     * 
     * @param localAddress The IP address of the calling application entity.
     */
    private void setLocalAddress(final String localAddress) {
        associateRQ.setCallingAppTitle(localAddress.getBytes());
    }

    /**
     * Should MIPAV accept the abstract syntax.
     * 
     * @param abstractSyntax The abstract syntax that will be compared to the list of abstract syntaxes that is
     *            supported.
     * 
     * @return True if we support the abstract syntax, otherwise false.
     */
    private boolean shouldWeAcceptAbstractSyntax(final DICOM_PDUItemType abstractSyntax) {
        final String str = DICOM_Util.unpadStringVal(abstractSyntax.getUID().getBytes());

        if (proposedAbstractSyntaxs.get(str) != null) {
            return (true);
        }

        return (false);
    }

    /**
     * As it turn out we accept all application contexts.
     * 
     * @param applicationContext The application context to be check to be . Not used at this time.
     * 
     * @return Always returns true.
     */
    private boolean shouldWeAcceptApplicationContext(final DICOM_PDUItemType applicationContext) {
        return (true); // accept all presentation contextes !!!!!!!!
    }

    public void parseDICOMintoBuffer(final DICOM_Object ddo, final DICOM_Comms vrBuffer, final DICOM_FileIO ioBuffer) {

        DICOM_VR VR;
        int length;
        boolean padByte = false;

        while ( (VR = ddo.pop()) != null) {
            vrBuffer.writeShort16In(VR.group, ioBuffer);
            vrBuffer.writeShort16In(VR.element, ioBuffer);
            length = VR.data.length;
            padByte = false;

            if ( (length % 2) != 0) { // not even
                length++;
                padByte = true;
            }

            vrBuffer.writeInt32In(length, ioBuffer);
            // Preferences.debug("DICOM_PDU_SERV parseDICOMIntoRawVR: "+Integer.toString(VR.group, 0x10) + "," +
            // Integer.toString(VR.element,0x10)+"; VR length = " + length+ " (" +
            // Integer.toString(length, 0x10) + ")\n");

            DICOM_VR.writeDataIn(VR, vrBuffer, ioBuffer);

            if (padByte) {
                final int typeCode = DICOM_RTC.getTypeCode(DICOM_RTC.unknownDDType(VR.group, VR.element));
                byte padCharacter = ' '; // clever: always padding space, if we need to pad

                switch (typeCode) {

                    case DICOM_RTC.TYPE_CS:
                    case DICOM_RTC.TYPE_IS:
                    case DICOM_RTC.TYPE_LO:
                    case DICOM_RTC.TYPE_PN:
                    case DICOM_RTC.TYPE_SH:
                        padCharacter = (byte) ' ';
                        break;

                    case DICOM_RTC.TYPE_UNKNOWN:
                        padCharacter = (byte) ' '; // space
                        break;

                    case DICOM_RTC.TYPE_OB:
                    case DICOM_RTC.TYPE_UI:
                    case DICOM_RTC.TYPE_DT:
                        padCharacter = 0; // for completeness: null char padded
                        break;

                    default:
                }

                vrBuffer.writeByteIn(padCharacter, ioBuffer);
            }
        }

    }

}
