package gov.nih.mipav.model.dicomcomm;


/**
 * This an abstract class implements a 4 byte item length (DICOM_PDUItemType implements 2 byte item length. See
 * DICOM_PDUTypeBase for the methods that are required for new classes that extend this class.
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
public abstract class DICOM_PDUType extends DICOM_PDUTypeBase {

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the size of this PDU type.
     * 
     * @return the size = type + reserved + pdu length + (size)
     */
    public int calcSize() {
        return (1 + 1 + 4 + length());
    }

    /**
     * Reads the PDU type header.
     * 
     * @param connection the connection to read from
     * 
     * @return The PDU item type read.
     * 
     * @throws DICOM_Exception Indicates reading error.
     */
    public byte readHeader(final DICOM_Comms connection) throws DICOM_Exception {
        itemType = connection.readByte();
        reserved1 = connection.readByte();
        length = connection.readInt32();

        return (itemType);
    }

    /**
     * Writes the PDU type header.
     * 
     * @param connection the connection to write to
     */
    public void writeHeader(final DICOM_Comms connection) {
        connection.writeByte(itemType);
        connection.writeByte(reserved1);
        connection.writeInt32(length()); // / should this be connection.writeInt32( calcSize() ); ??
    }

}
