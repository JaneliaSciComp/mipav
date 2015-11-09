package gov.nih.mipav.model.dicomcomm;


import java.util.Vector;


/**
 * Implements a DICOM Association Acceptance PDU Type.
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
public class DICOM_AAssociateAC extends DICOM_AAssociateRQ {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private final Vector<DICOM_PresentationContextAccept> presContexts = new Vector<DICOM_PresentationContextAccept>();

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_AAssociateAC object.
     */
    public DICOM_AAssociateAC() {
        itemType = DICOM_PDUTypeBase.PDUTYPE_AAssociateAC;
        DICOM_Util.zeroByteArray(reserved2);
        DICOM_Util.clearByteArray(calledAppTitle);
        DICOM_Util.clearByteArray(callingAppTitle);
        DICOM_Util.zeroByteArray(reserved3);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Adds the presentation context to the accepted context list.
     * 
     * @param pca accepted presentation context to be added the list (vector)
     */
    public void addPresentationContextAccept(final DICOM_PresentationContextAccept pca) {
        presContexts.addElement(pca);
    }

    /**
     * Returns the list of presentation contexts.
     * 
     * @return the list (vector) of accepted presentation contextes.
     */
    public Vector<DICOM_PresentationContextAccept> getPresentationContextes() {
        return presContexts;
    }

    /**
     * Calculates length of message.
     * 
     * @return the length of the message including application context + presentation context(s) + userInfo
     */
    public int length() {
        int messageLength;

        // protocol version + reserved2 + calledApp + callingApp + reserved3
        messageLength = 68; // 2 + 2 + 16 + 16 + 32;
        messageLength += appContext.calcSize();

        for (int i = 0; i < presContexts.size(); i++) {
            messageLength += ((DICOM_PresentationContextAccept) presContexts.elementAt(i)).calcSize();
        }

        messageLength += userInfo.calcSize();

        return (messageLength);
    }

    /**
     * Reads the body of an association accept.
     * 
     * @param connection the I/O Buffer to read from
     * 
     * @exception DICOM_Exception unknown PDUType
     */
    public void readBody(final DICOM_Comms connection) throws DICOM_Exception {
        DICOM_PresentationContextAccept presentationContext;
        int readLength;

        protocolVersion = connection.readShort16();
        connection.read(reserved2);
        connection.read(calledAppTitle);
        connection.read(callingAppTitle);
        connection.read(reserved3);

        // length - protocol version - reserved2 - calledApp - callingApp - reserved3
        readLength = length - 68; // 2 - 2 - 16 - 16 - 32;

        while (readLength > 0) {

            switch (connection.peekFirstByte()) {

                case PDUTYPE_ApplicationContext:
                    appContext.read(connection);
                    readLength = readLength - appContext.calcSize();
                    break;

                case PDUTYPE_UserInformation:
                    userInfo.read(connection);
                    readLength = readLength - userInfo.calcSize(); // - userInfo.userInfoSize;
                    break;

                case PDUTYPE_PresentationContextAccept:
                    presentationContext = new DICOM_PresentationContextAccept();
                    presentationContext.read(connection);
                    readLength = readLength - presentationContext.calcSize();
                    addPresentationContextAccept(presentationContext);
                    break;

                default:
                    connection.readBytes(readLength - 1);
                    throw new DICOM_Exception("DICOMerror: AAssociateAC.readBody: unknown PDU type");
            }
        }
    }

    /**
     * Removes all presentation contextes from the list (vector).
     */
    public void resetPresentationContext() {
        presContexts.removeAllElements();
    }

    /**
     * Writes the body of the association accept.
     * 
     * @param connection the I/O Buffer to write to
     * 
     * @exception DICOM_Exception problem with writing association request
     */
    public void writeBody(final DICOM_Comms connection) throws DICOM_Exception {

        connection.writeShort16(protocolVersion);
        connection.write(reserved2);
        connection.write(calledAppTitle);
        connection.write(callingAppTitle);
        connection.write(reserved3);

        appContext.write(connection);

        for (int i = 0; i < presContexts.size(); i++) {
            ((DICOM_PresentationContextAccept) presContexts.elementAt(i)).write(connection);
        }

        userInfo.write(connection);
        connection.flush();
    }

}
