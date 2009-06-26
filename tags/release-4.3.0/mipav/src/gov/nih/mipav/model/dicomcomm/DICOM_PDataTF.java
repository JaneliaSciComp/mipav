package gov.nih.mipav.model.dicomcomm;


/**
 * Used to exchange application information (i.e. DICOM messages). !
 */
public class DICOM_PDataTF extends DICOM_PDUType {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Presentation Data Value presentation context ID. */
    public byte PDVPresContID;

    /** Size of the output data */
    private int outBlockSize;

    /** Presentation Data Value length. */
    private int PDVLength; 

    /** Presentation Data Value message header. 
     * 01H Accociate request. 
     * 02H Associate ack.
     * 03H Associate reject.
     * 04H PData PDU
     * 05H Release request
     * 06H Release response
     * 07H Abort
     * 10H Application context item 
     * 20H Presetation context item
     * 21H Pr
     * 30H Abstract Syntax sub-item
     * 40H Trnasfer Syntax sub-item
     * 50H User information item
     */
    private byte PDVMsgHeader;

    /** Flag used to identify read message status. Default is false. */
    private boolean readMessageStatus = false;

    /** Reference to the link buffer communication link. */
    private DICOM_CommsLink vrLinkedBuffer = new DICOM_CommsLink();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_PDataTF object.
     */
    public DICOM_PDataTF() {
        itemType = PDUTYPE_PDataTF;
        setOutgoingBlockSize(0);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Gets the communication linkbuffer.
     *
     * @return  DICOM_CommsLink
     */
    public DICOM_CommsLink getVRLinkedBuffer() {
        return vrLinkedBuffer;
    }
    
    /**
     * Cleans up memory.
     */
    public void finalize(){
        vrLinkedBuffer.finalize();
        vrLinkedBuffer = null;
    }

    /**
     * Returns true if the reading is complete else false.
     *
     * @return  true when reading of the connection is complete
     */
    public boolean isReadComplete() {
        return (readMessageStatus);
    }

    /**
     * Reads the connection.
     *
     * @param   Connection to the DICOM buffer.
     *
     * @throws  DICOM_Exception  Throws _PDV length exceeds pDataTF length_ if data is corrupted.
     */
    public void readBody(DICOM_Comms connection) throws DICOM_Exception {
        int count = length;

        readMessageStatus = false;

        while (count > 0) {
            PDVLength = connection.readInt32();
            PDVPresContID = connection.readByte();
            PDVMsgHeader = connection.readByte();

            if (PDVLength > count) {
                throw new DICOM_Exception("PDataTF.readBody: PDV length exceeds pDataTF length");
            }

            vrLinkedBuffer.readFill(connection, PDVLength - 2); // minus  1 byte = PDVPresContID and 1 byte =
                                                                // PDVMsgHeader

            count -= PDVLength + 4; // 4 is the PDVLength
            length -= PDVLength + 4;

            if ((PDVMsgHeader & 0x02) == 0x02) {
                count = 0; // break out of while loop
            }
        }

        if ((PDVMsgHeader & 0x02) == 0x02) {
            readMessageStatus = true;
        }
    }

    /**
     * Sets the outgoing block size based the the Max. Sub. Length
     *
     * @param  outMaxSubLength  The maximum sublength
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
     * @param   PDU                    The PDU
     * @param   presentationContextID  The presentation context ID.
     * @param   msgHeader              The message header.
     *
     * @throws  DICOM_Exception  Indicates error writing PData information.
     */
    public void write(DICOM_PDUService PDU, byte presentationContextID, byte msgHeader) throws DICOM_Exception {
        PDVPresContID = presentationContextID;
        PDVMsgHeader = msgHeader;

        int size = 0;
        int blockSize = outBlockSize;
        int totalSize = vrLinkedBuffer.getOutgoingSize();

        while (size < totalSize) {

            if ((totalSize - size) < blockSize) {
                blockSize = totalSize - size;
            }

            if ((blockSize + size) == totalSize) {
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
     * @param   connection  Filler
     *
     * @throws  DICOM_Exception  Filler
     */
    public void writeBody(DICOM_Comms connection) throws DICOM_Exception {
        // Filler because it extends DICOM_PDUType which is an abstract class
    }


}
