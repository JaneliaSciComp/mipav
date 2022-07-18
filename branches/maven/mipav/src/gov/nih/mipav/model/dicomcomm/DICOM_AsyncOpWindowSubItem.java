package gov.nih.mipav.model.dicomcomm;


/**
 * DICOM AsyncOpWindowSubItem PDU Item Type - Not supported but we read it in. This is an optional support item in
 * DICOM.
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
public class DICOM_AsyncOpWindowSubItem extends DICOM_PDUItemType {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Indicates the maximum number of operations invoked. */
    protected short maxNumOpsInvoked = 0;

    /** Indicates the maximum number of operations performed. */
    protected short maxNumOpsPerformed = 0;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * DICOM SCP/SCU Role Selection PDU Item Type.
     */
    public DICOM_AsyncOpWindowSubItem() {
        super(DICOM_PDUTypeBase.PDUTYPE_AsyncOpWindowSubItem);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Calculates length of message.
     * 
     * @return the length of the message
     */
    public int length() {
        return (4);
    }

    /**
     * Reads the body of the sub item.
     * 
     * @param connection the I/O Buffer to read from
     * 
     * @exception DICOM_Exception unknown PDUType
     */
    public void readBody(final DICOM_Comms connection) throws DICOM_Exception {

        maxNumOpsInvoked = (short) connection.readShort16();
        maxNumOpsPerformed = (short) connection.readShort16();
    }

    /**
     * Writes the body of the sub item.
     * 
     * @param connection the I/O Buffer to write to
     * 
     * @exception DICOM_Exception problem with writing association request
     */
    public void writeBody(final DICOM_Comms connection) throws DICOM_Exception {
        connection.writeShort16(maxNumOpsInvoked);
        connection.writeShort16(maxNumOpsPerformed);
    }

}
