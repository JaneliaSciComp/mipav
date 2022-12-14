package gov.nih.mipav.model.dicomcomm;


/**
 * It is a bit odd since it both extends DICOM_Comms and has a DICOM_Comms object. Might be best to redesign this
 * interface structure but it works!
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

public class DICOM_CommsLink extends DICOM_Comms {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Reference to the link communications object. */
    DICOM_Comms linkToComms = new DICOM_Comms();

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Send the data out the connection (pdu).
     * 
     * @param connection the PDU or connection (DICOM_Comms is the parent class of DICOM_PDUService)
     * @param length The length of data to be flushed out the port.
     * 
     * @throws DICOM_Exception Throws an error when attempting to flush more bytes than are present!
     */
    public void flush(final DICOM_Comms connection, final int length) throws DICOM_Exception {
        linkToComms = connection;
        flush(length);
    }

    /**
     * Copies data from the port buffer into the a new buffer used typically in pDataTF.
     * 
     * @param data Buffer to store the data
     * @param length Number of bytes to be read
     * 
     * @return The length of the data read in.
     * 
     * @throws DICOM_Exception Throws an error if there is a problem reading the port.
     */
    public int readBinary(final byte[] data, final int length) throws DICOM_Exception {
        linkToComms.read(data, length); // this eventually finds it way to
        // the pdu.readBinary (pdu is a DICOM buffer)

        return (length);
    }

    /**
     * Sets linkToComms to connection and fills(read) the connection buffer via readBinary.
     * 
     * @param connection The connection to the socket.
     * @param length The number of bytes to read.
     * 
     * @throws DICOM_Exception Throws an error if there is a problem reading the port.
     */
    public void readFill(final DICOM_Comms connection, final int length) throws DICOM_Exception {
        linkToComms = connection;
        readFill(length);
    }

    /**
     * Actual binary data to be sent.
     * 
     * @param data Buffer of data send out the port (socket)
     * @param offset The offset into the data indicating the starting point of the data to be sent.
     * @param length The number of bytes to be sent.
     * 
     * @throws DICOM_Exception Throws an error if there is a problem sendting via port.
     */
    public void sendBinary(final byte[] data, final int offset, final int length) throws DICOM_Exception {
        linkToComms.write(data, offset, length);
        linkToComms.flush();
    }

}
