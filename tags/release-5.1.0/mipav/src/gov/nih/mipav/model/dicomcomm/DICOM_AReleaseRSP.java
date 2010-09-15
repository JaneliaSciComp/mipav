package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.Preferences;


/**
 * DICOM Association Release Response.
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
public class DICOM_AReleaseRSP extends DICOM_PDUType {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    byte[] reserved2 = new byte[4];

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_AReleaseRSP object.
     */
    public DICOM_AReleaseRSP() {
        itemType = DICOM_PDUTypeBase.PDUTYPE_AReleaseRSP;
        DICOM_Util.zeroByteArray(reserved2);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Calculates length of message.
     * 
     * @return the length of the message = 4 (reserved2 = 4 bytes)
     */
    public int length() {
        return (4);
    }

    /**
     * Reads the body of an association release response.
     * 
     * @param connection the I/O Buffer to read from
     * 
     * @exception DICOM_Exception unknown PDUType
     */
    public void readBody(final DICOM_Comms connection) throws DICOM_Exception {

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug("AReleaseRSP.readBody\n");
        }

        connection.read(reserved2);
    }

    /**
     * Writes the body of the association release response.
     * 
     * @param connection the I/O Buffer to write to
     * 
     * @exception DICOM_Exception problem with writing association release response
     */
    public void writeBody(final DICOM_Comms connection) throws DICOM_Exception {

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug("AReleaseRSP.writeBody\n");
        }

        connection.write(reserved2);
        connection.flush();
    }

}
