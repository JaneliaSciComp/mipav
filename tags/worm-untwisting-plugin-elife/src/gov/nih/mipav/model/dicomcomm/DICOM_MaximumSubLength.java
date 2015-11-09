package gov.nih.mipav.model.dicomcomm;


/**
 * Simple class that implements a Maximum SubLength PDU type.
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
public class DICOM_MaximumSubLength extends DICOM_PDUItemType {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The maximum length - default is zero. */
    private int maxLength = 0;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Simple class that implements a Maximum SubLength PDU type.
     */
    public DICOM_MaximumSubLength() {
        super(DICOM_PDUTypeBase.PDUTYPE_MaximumSubLength);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Accessor that returns the maximum sublength.
     * 
     * @return The maximum sublength
     */
    public int getMaxLength() {
        return (maxLength);
    }

    /**
     * Accessor that returns the length in bytes.
     * 
     * @return size = type + reserved + item length + length of 4 (i.e. a 4 byte int)
     */
    public int length() {
        return (4);
    }

    /**
     * Reads the maximum sublength out the connection (port).
     * 
     * @param connection The connection (port) where the data is read.
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void readBody(final DICOM_Comms connection) throws DICOM_Exception {
        maxLength = connection.readInt32();
    }

    /**
     * Sets the maximum sublength.
     * 
     * @param maximum The new value for the maximum sublength.
     */
    public void setMaxLength(final int maximum) {
        maxLength = maximum;
    }

    /**
     * Writes the maximum sublength out the connection (port).
     * 
     * @param connection The connection (port) where the data is sent
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void writeBody(final DICOM_Comms connection) throws DICOM_Exception {
        connection.writeInt32(maxLength);
    }

}
