package gov.nih.mipav.model.dicomcomm;


import java.util.Vector;


/**
 * DICOM Presentation Context PDU Item Type.
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
public class DICOM_PresentationContext extends DICOM_PDUItemType {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Abstract syntax object. */
    public DICOM_PDUItemType absSyntax = new DICOM_PDUItemType(DICOM_PDUTypeBase.PDUTYPE_AbstractSyntax);

    /** The ID for the presentation context. */
    public byte presentationContextID;

    /** List of transfer syntaxes approiate for the abstract syntax. */
    public Vector trnSyntax = new Vector();

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_PresentationContext object.
     */
    public DICOM_PresentationContext() {
        super(DICOM_PDUTypeBase.PDUTYPE_PresentationContext);
        presentationContextID = (byte) DICOM_Util.getUniqueOddID8();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Adds a new transfer syntax to the list (ie. vector)
     * 
     * @param transferSyntax DOCUMENT ME!
     */
    public void addTransferSyntax(final DICOM_PDUItemType transferSyntax) {
        trnSyntax.addElement(transferSyntax);
    }

    /**
     * Calculates length of message.
     * 
     * @return the length of the message
     */
    public int length() {
        int length;

        // presentationContextID + reserved2 + reserved3 + reserved4
        length = 4; // 1 + 1 + 1 + 1;
        length += absSyntax.calcSize();

        for (int i = 0; i < trnSyntax.size(); i++) {
            length += ((DICOM_PDUItemType) trnSyntax.elementAt(i)).calcSize();
        }

        return (length);
    }

    /**
     * Reads the body of the presentation context.
     * 
     * @param connection the I/O Buffer to read from
     * 
     * @exception DICOM_Exception unknown PDUType
     */
    public void readBody(final DICOM_Comms connection) throws DICOM_Exception {
        DICOM_PDUItemType transferSyntax;
        int count;

        presentationContextID = connection.readByte();
        reserved2 = connection.readByte();
        reserved3 = connection.readByte();
        reserved4 = connection.readByte();

        // length - presentationContextID - reserved2 - reserved3 - reserved4
        count = length - 4; // 1 - 1 - 1 - 1;
        absSyntax.read(connection);
        count -= absSyntax.calcSize();

        while (count > 0) {
            transferSyntax = new DICOM_PDUItemType(DICOM_PDUTypeBase.PDUTYPE_TransferSyntax);
            transferSyntax.read(connection);
            count -= transferSyntax.calcSize();
            trnSyntax.addElement(transferSyntax);
        }
    }

    /**
     * Sets the abstract syntax of this class.
     * 
     * @param abstractSyntax abstract syntax
     */
    public void setAbstractSyntax(final DICOM_PDUItemType abstractSyntax) {
        absSyntax = abstractSyntax;
    }

    /**
     * Writes the body of the presentation context.
     * 
     * @param connection the I/O Buffer to write to
     * 
     * @exception DICOM_Exception problem with writing association request
     */
    public void writeBody(final DICOM_Comms connection) throws DICOM_Exception {
        connection.writeByte(presentationContextID);
        connection.writeByte(reserved2);
        connection.writeByte(reserved3);
        connection.writeByte(reserved4);
        absSyntax.write(connection);

        for (int i = 0; i < trnSyntax.size(); i++) {
            ((DICOM_PDUItemType) trnSyntax.elementAt(i)).write(connection);
        }

        // connection.flush();
    }

}
