package gov.nih.mipav.model.dicomcomm;


/**
 * This class implements a 2 byte item length (DICOM_PDUType implements 4 byte item length. See DICOM_PDUTypeBase for
 * the methods that are required for new classes that extend this class.
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
public class DICOM_PDUItemType extends DICOM_PDUTypeBase {

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_PDUItemType object.
     * 
     * @param itemType The PDU item type.
     */
    public DICOM_PDUItemType(final byte itemType) {
        this.itemType = itemType;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the PDU item type size.
     * 
     * @return The size = type + reserved + item length + (length)
     */
    public int calcSize() {
        return (1 + 1 + 2 + length());
    }

    /**
     * Reads the body (i.e. UID) of the item type
     * 
     * @param Connection to read from
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void readBody(final DICOM_Comms connection) throws DICOM_Exception {
        UID = new String(connection.readBytes(length));
    }

    /**
     * Reads the PDU item header.
     * 
     * @param Connection the connection to read from
     * 
     * @return The PDU item type
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public byte readHeader(final DICOM_Comms connection) throws DICOM_Exception {
        itemType = connection.readByte();
        reserved1 = connection.readByte();
        length = connection.readShort16();

        return (itemType);
    }

    /**
     * Writes the body of the item type.
     * 
     * @param connection to write to
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void writeBody(final DICOM_Comms connection) throws DICOM_Exception {
        connection.write(UID.getBytes());
    }

    /**
     * Writes the header of the item type.
     * 
     * @param connection to write to
     */
    public void writeHeader(final DICOM_Comms connection) {
        connection.writeByte(itemType);
        connection.writeByte(reserved1);
        connection.writeShort16(length());
    }

}
