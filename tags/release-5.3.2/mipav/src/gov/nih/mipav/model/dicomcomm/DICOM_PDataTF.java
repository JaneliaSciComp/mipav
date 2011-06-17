package gov.nih.mipav.model.dicomcomm;


/**
 * Used to exchange application information (i.e. DICOM messages).
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
public class DICOM_PDataTF extends DICOM_PDUType {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Presentation Data Value presentation context ID. */
    public byte PDVPresContID;

    /** Size of the output data */
    int outBlockSize;

    /** Presentation Data Value length. */
    private int PDVLength;

    /**
     * Presentation Data Value message header. 01H Accociate request. 02H Associate ack. 03H Associate reject. 04H PData
     * PDU 05H Release request 06H Release response 07H Abort 10H Application context item 20H Presetation context item
     * 21H Pr 30H Abstract Syntax sub-item 40H Trnasfer Syntax sub-item 50H User information item
     */
    private byte PDVMsgHeader;

    /** Flag used to identify read message status. Default is false. */
    private boolean readMessageStatus = false;

    /** Reference to the link buffer communication link. */
    DICOM_CommsLink vrLinkedBuffer = new DICOM_CommsLink();

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_PDataTF object.
     */
    public DICOM_PDataTF() {
        itemType = DICOM_PDUTypeBase.PDUTYPE_PDataTF;
        setOutgoingBlockSize(0);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Gets the communication linkbuffer.
     * 
     * @return DICOM_CommsLink
     */
    public DICOM_CommsLink getVRLinkedBuffer() {
        return vrLinkedBuffer;
    }

    /**
     * Cleans up memory.
     */
    public void finalize() {
        vrLinkedBuffer.finalize();
        vrLinkedBuffer = null;
    }

    /**
     * Returns true if the reading is complete else false.
     * 
     * @return true when reading of the connection is complete
     */
    public boolean isReadComplete() {
        return (readMessageStatus);
    }

    /**
     * Reads the connection.
     * 
     * @param Connection to the DICOM buffer.
     * 
     * @throws DICOM_Exception Throws _PDV length exceeds pDataTF length_ if data is corrupted.
     */
    public void readBody(final DICOM_Comms connection) throws DICOM_Exception {
        int count = length;

        readMessageStatus = false;

        while (count > 0) {
            PDVLength = connection.readInt32();
            PDVPresContID = connection.readByte();
            PDVMsgHeader = connection.readByte();

            if (PDVLength > count) {
                throw new DICOM_Exception("PDataTF.readBody: PDV length exceeds pDataTF length");
            }

            vrLinkedBuffer.readFill(connection, PDVLength - 2); // minus 1 byte = PDVPresContID and 1 byte =
            // PDVMsgHeader

            count -= PDVLength + 4; // 4 is the PDVLength
            length -= PDVLength + 4;

            if ( (PDVMsgHeader & 0x02) == 0x02) {
                count = 0; // break out of while loop
            }
        }

        if ( (PDVMsgHeader & 0x02) == 0x02) {
            readMessageStatus = true;
        }
    }

    /**
     * Sets the outgoing block size based the the Max. Sub. Length
     * 
     * @param outMaxSubLength The maximum sublength
     */
    public void setOutgoingBlockSize(int outMaxSubLength) {

        if (outMaxSubLength == 0) {
            outMaxSubLength = DICOM_Constants.MAXSUBLENGTH;
        }

        outBlockSize = outMaxSubLength - 6; // ( 4 + 1 + 1 )
        outBlockSize = (outBlockSize - 1) & ~0x01;

        if (outBlockSize < 1024) {
            outBlockSize = 1024;
        }
    }

    /**
     * Writes P-DATA-TF Messages.
     * 
     * @param PDU The PDU
     * @param presentationContextID The presentation context ID.
     * @param msgHeader The message header.
     * 
     * @throws DICOM_Exception Indicates error writing PData information.
     */
    public void write(final DICOM_PDUService PDU, final byte presentationContextID, final byte msgHeader)
            throws DICOM_Exception {
        PDVPresContID = presentationContextID;
        PDVMsgHeader = msgHeader;

        int size = 0;
        int blockSize = outBlockSize;
        final int totalSize = vrLinkedBuffer.getOutgoingSize();

        while (size < totalSize) {

            if ( (totalSize - size) < blockSize) {
                blockSize = totalSize - size;
            }

            if ( (blockSize + size) == totalSize) {
                PDVMsgHeader |= 0x02;
            } else {
                PDVMsgHeader &= ~0x02;
            }

            PDVLength = blockSize + 2;
            PDU.writeByte(itemType);
            PDU.writeByte(reserved1);
            PDU.writeInt32(PDVLength + 4);

            PDU.writeInt32(PDVLength);
            PDU.writeByte(PDVPresContID);
            PDU.writeByte(PDVMsgHeader);

            if (blockSize > 0) {
                vrLinkedBuffer.flush(PDU, blockSize);
            }

            size += blockSize;
            // System.out.println("Dicom_PData.TF.write - size = " + size + " total size = " + totalSize);
        }
    }

    /**
     * Filler because it extends DICOM_PDUType which is an abstract class
     * 
     * @param connection Filler
     * 
     * @throws DICOM_Exception Filler
     */
    public void writeBody(final DICOM_Comms connection) throws DICOM_Exception {
    // Filler because it extends DICOM_PDUType which is an abstract class
    }

}
