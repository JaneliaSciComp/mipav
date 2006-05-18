package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.*;

import java.util.*;


/**
 * This class is used as a base class for all DICOM file and network I/O.
 */
public class DICOM_Comms {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Flag used to indicate Big Endianess. */
    public static final int BIG_ENDIAN = 1;

    /** Flag used to indicate Little Endianess. */
    public static final int LITTLE_ENDIAN = 2;

    /** The maximum read buffer size. */
    public static final int MAX_READ_LENGTH = 32768;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Defaults to the maximum buffer size in streaming. */
    protected int breakSize;

    /** Length of the total input data (Not buffer size but total length of data ). */
    protected int inBuffersLength;

    /** Vector of ByteBuffer's of incoming data. */
    protected Vector incomingBuffers = new Vector();

    /** Length of the total output data (Not buffer size but total length of data ). */
    protected int outBuffersLength;

    /** Vector of ByteBuffer's of outgoing data. */
    protected Vector outgoingBuffers = new Vector();

    /** One byte array allocated once to speed process and reduced the need to reallocate memory. */
    private byte[] byteArray1 = new byte[1];

    /** Two byte array allocated once to speed process and reduced the need to reallocate memory. */
    private byte[] byteArray2 = new byte[2];

    /** Four byte array allocated once to speed process and reduced the need to reallocate memory. */
    private byte[] byteArray4 = new byte[4];


    /** Debug flag. */
    private boolean debugFlag = false;

    /** Flag indicating whether the first byte has been looked at. */
    private boolean havePeeked = false;

    /** Endianess of the input stream. */
    private int inEndianess;

    /** Endianess of the output stream. */
    private int outEndianess;

    /** Storage for the first byte (i.e. the PDU type ) */
    private byte peekedByte = 0;

    /** Source byte array allocated ONCE used in reading from the stream. */
    private byte[] srcByteArray = new byte[MAX_READ_LENGTH];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for DICOM network I/O class. Sets byte ordering for read and write to big endian.
     */
    public DICOM_Comms() {

        inEndianess = BIG_ENDIAN;
        outEndianess = BIG_ENDIAN;

        inBuffersLength = 0;
        outBuffersLength = 0;
        breakSize = ByteBuffer.DEFAULT_SIZE;

        debugFlag = Preferences.debugLevel(Preferences.DEBUG_COMMS);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * The next 4 static methods facilitate the conversion 16 bit and 32 bit values into a byte buffer for I/O (file and
     * socket).
     *
     * @param   buffer     DOCUMENT ME!
     * @param   index      DOCUMENT ME!
     * @param   endianess  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */

    /**
     * Copies a 2 byte array into a 16 bit integer of proper endianess.
     *
     * @param   buffer     source byte array
     * @param   index      index into buffer
     * @param   endianess  Byte ordering as defined above.
     *
     * @return  16 bit integer value
     */
    public static final int bufferToInt16(byte[] buffer, int index, int endianess) {

        if (endianess == BIG_ENDIAN) {
            return (((buffer[index++] & 0xff) << 8) | (buffer[index++] & 0xff));
        } else {
            return ((buffer[index++] & 0xff) | ((buffer[index++] & 0xff) << 8));
        }
    }

    /**
     * Copies a 4 byte array into a 32 bit integer of proper endianess.
     *
     * @param   buffer     source byte array
     * @param   index      index into buffer
     * @param   endianess  endianess
     *
     * @return  32 bit integer value
     */
    public static final int bufferToInt32(byte[] buffer, int index, int endianess) {

        if (endianess == BIG_ENDIAN) {
            return (((buffer[index++] & 0xff) << 24) | ((buffer[index++] & 0xff) << 16) |
                        ((buffer[index++] & 0xff) << 8) | (buffer[index++] & 0xff));
        } else {
            return ((buffer[index++] & 0xff) | ((buffer[index++] & 0xff) << 8) | ((buffer[index++] & 0xff) << 16) |
                        ((buffer[index++] & 0xff) << 24));
        }
    }


    /**
     * Copies a 16 bit (2 bytes) integer into the specified portion of the byte array of proper endianess.
     *
     * @param  buffer     destination byte array
     * @param  index      index into buffer
     * @param  value      16 bit integer value
     * @param  endianess  endianess
     */
    public static final void int16ToBuffer(byte[] buffer, int index, int value, int endianess) {

        if (endianess == BIG_ENDIAN) {
            buffer[index++] = (byte) (value >>> 8);
            buffer[index++] = (byte) (value);
        } else {
            buffer[index++] = (byte) (value);
            buffer[index++] = (byte) (value >>> 8);
        }
    }

    /**
     * Copies a 32 bit (4 bytes) integer into the specified portion of the byte array of proper endianess.
     *
     * @param  buffer     destination byte array
     * @param  index      index into buffer
     * @param  value      16 bit integer value
     * @param  endianess  endianess
     */
    public static final void int32ToBuffer(byte[] buffer, int index, int value, int endianess) {

        if (endianess == BIG_ENDIAN) {
            buffer[index++] = (byte) (value >>> 24);
            buffer[index++] = (byte) (value >>> 16);
            buffer[index++] = (byte) (value >>> 8);
            buffer[index++] = (byte) (value);
        } else {
            buffer[index++] = (byte) (value);
            buffer[index++] = (byte) (value >>> 8);
            buffer[index++] = (byte) (value >>> 16);
            buffer[index++] = (byte) (value >>> 24);
        }
    }

    /**
     * Sends all byte buffers of the outgoing buffer vector out the port. see DICOM PDUService.sendBinary and
     * DICOMSocket
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public void flush() throws DICOM_Exception {
        ByteBuffer byteBuffer;
        int byteCount;

        for (int i = 0; i < outgoingBuffers.size(); i++) {
            byteBuffer = (ByteBuffer) outgoingBuffers.elementAt(i);
            byteCount = byteBuffer.endIndex - byteBuffer.startIndex + 1;

            if (byteCount > 0) {
                sendBinary(byteBuffer.data, byteBuffer.startIndex, byteCount);
            }
        }

        outgoingBuffers.removeAllElements();
        outBuffersLength = 0;
    }

    /**
     * Sends specified number of bytes out the port. see DICOM PDUService.sendBinary and DICOMSocket
     *
     * @param   nBytes  number of bytes to flush
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public void flush(int nBytes) throws DICOM_Exception {
        ByteBuffer byteBuffer;
        int availableBytes;

        if (nBytes > outBuffersLength) {
            flush();
        } else {
            outBuffersLength -= nBytes;

            while (nBytes > 0) {

                if (outgoingBuffers.size() == 0) {
                    outBuffersLength = 0;
                    throw new DICOM_Exception("DICOM_Comms.flush: attempting to flush more bytes than are present.");
                }

                byteBuffer = (ByteBuffer) outgoingBuffers.elementAt(0);
                availableBytes = byteBuffer.endIndex - byteBuffer.startIndex + 1;

                if (availableBytes > nBytes) {
                    sendBinary(byteBuffer.data, byteBuffer.startIndex, nBytes);
                    System.arraycopy(byteBuffer.data, byteBuffer.startIndex + nBytes, byteBuffer.data, 0,
                                     availableBytes - nBytes);
                    byteBuffer.startIndex = 0;
                    byteBuffer.endIndex = availableBytes - nBytes - 1;
                    nBytes = 0;
                } else {

                    if (availableBytes > 0) {
                        sendBinary(byteBuffer.data, byteBuffer.startIndex, availableBytes);
                        nBytes -= availableBytes;
                    }
                    ((ByteBuffer)(outgoingBuffers.elementAt(0))).finalize();
                    outgoingBuffers.removeElementAt(0);
                }
            }
        }
    }


    /**
     * Returns constant indicating endianess of incoming data.
     *
     * @return  the endianess of the incoming data
     */
    public int getIncomingEndian() {
        return (inEndianess);
    }

    /**
     * Returns the incoming data size.
     *
     * @return  the number of bytes in the incoming data
     */
    public int getIncomingSize() {
        return (inBuffersLength);
    }

    /**
     * Returns constant indicating endianess of outgoing data.
     *
     * @return  the endianess of the outgoing data
     */
    public int getOutgoingEndian() {
        return (outEndianess);
    }

    /**
     * Returns the outgoing data size.
     *
     * @return  the number of bytes in the outgoing data
     */
    public int getOutgoingSize() {
        return (outBuffersLength);
    }


    /**
     * Typically used to determine the PDUTYPE (i.e. PDUTYPE_PresentationContextAccept)
     *
     * @return  the PDU type identifier
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public byte peekFirstByte() throws DICOM_Exception {
        havePeeked = false;

        read(byteArray1, 1);

        havePeeked = true;
        peekedByte = byteArray1[0];

        return (peekedByte);
    }


    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public int peekForEndOfSequence() throws DICOM_Exception {
        int groupEndSq;
        int elementEndSq;

        groupEndSq = 0xFFFE; // Big Endian
        elementEndSq = 0xE0DD;

        boolean moreToRead = true;

        // run through current buffer to find sequence.
        // if here then exit with the offset.
        // else add buffers till we do find it.

        byte[] groupWord = new byte[2];
        byte[] elementWord = new byte[2];
        byte[] zeroWord = new byte[] { 0, 0, 0, 0 };
        int bufferIndex = 0; // start by looking at the first buffer
        ByteBuffer dataBuffer = (ByteBuffer) incomingBuffers.elementAt(bufferIndex);
        int dataIndex = dataBuffer.startIndex;
        int seqLength = 0; /* if at asome point we decide we don't want to both
                            * reading more buffers we can return -1 or we can throw an Exception (which mightbe better)
                            * (think IOException, or maybe make a new one, BufferReadException might be nice name.
                            */

        // store 4 bytes of data buffer into temporary buffers
        groupWord[0] = dataBuffer.data[dataIndex];
        groupWord[1] = dataBuffer.data[dataIndex + 1];
        elementWord[0] = dataBuffer.data[dataIndex + 2];
        elementWord[1] = dataBuffer.data[dataIndex + 3];
        zeroWord[0] = dataBuffer.data[dataIndex + 4];
        zeroWord[1] = dataBuffer.data[dataIndex + 5];
        zeroWord[2] = dataBuffer.data[dataIndex + 6];
        zeroWord[3] = dataBuffer.data[dataIndex + 7];

        do {

            // output the incoming data as a hex-output table since we
            // don't know what we're reading!
            if (debugFlag) {
                Preferences.debug(Integer.toString(((int) groupWord[0] & 0x0ff), 0x10) + "," +
                                  Integer.toString(((int) groupWord[1] & 0x0ff), 0x10) + " ");
            }

            if ((dataIndex % 0x10) == 0) {

                if (debugFlag) {
                    Preferences.debug("\n");
                }
            }

            if ((bufferToInt16(groupWord, 0, inEndianess) == groupEndSq) &&
                    (bufferToInt16(elementWord, 0, inEndianess) == elementEndSq) &&
                    (bufferToInt16(zeroWord, 0, inEndianess) == 0) && (bufferToInt16(zeroWord, 2, inEndianess) == 0)) {

                if (debugFlag) {
                    Preferences.debug("\nDICOM_COMMS: Data match (" + Integer.toString(groupEndSq, 0x10) + "," +
                                      Integer.toString(elementEndSq, 0x10) + ", 00, 00)\n");
                }

                moreToRead = false;

                // move dataOffset up to match index into current buffer:
                seqLength += (dataIndex - dataBuffer.startIndex);
            } else { // the current 4 bytes don't match the search pattern:

                // dataIndex += 2; // move 2 bytes over to recheck
                dataIndex++; // no, for right now, recheck on 1 byte

                // verify there are enough bytes in the buffer available to search
                if ((dataIndex + 8) <= dataBuffer.data.length) {

                    // there are, so collect next 4 bytes
                    System.arraycopy(dataBuffer.data, dataIndex, groupWord, 0, 2);
                    System.arraycopy(dataBuffer.data, dataIndex + 2, elementWord, 0, 2);
                    System.arraycopy(dataBuffer.data, dataIndex + 4, zeroWord, 0, 4);
                } else { // there are not enough bytes; copy as many as we can and
                         // then add another buffer; copy the rest.

                    ByteBuffer followingBuffer;

                    if (debugFlag) {
                        Preferences.debug("Buffer read, using buffer at " + Integer.toString(bufferIndex + 1) + "\n");
                    }

                    if (incomingBuffers.size() <= bufferIndex) {
                        return -1; // nothing more to read so FAIL!
                    }

                    followingBuffer = (ByteBuffer) incomingBuffers.elementAt(++bufferIndex);

                    // copy out the remainder of the old data buffer.
                    // use as-needed from the new one.
                    if ((dataBuffer.data.length - dataIndex) == 7) {
                        System.arraycopy(dataBuffer.data, dataIndex, groupWord, 0, 2);
                        System.arraycopy(dataBuffer.data, dataIndex + 2, elementWord, 0, 2);
                        System.arraycopy(dataBuffer.data, dataIndex + 4, zeroWord, 0, 3);
                        zeroWord[3] = followingBuffer.data[0];
                    } else if ((dataBuffer.data.length - dataIndex) == 6) {
                        System.arraycopy(dataBuffer.data, dataIndex, groupWord, 0, 2);
                        System.arraycopy(dataBuffer.data, dataIndex + 2, elementWord, 0, 2);
                        System.arraycopy(dataBuffer.data, dataIndex + 4, zeroWord, 0, 2);
                        zeroWord[2] = followingBuffer.data[0];
                        zeroWord[3] = followingBuffer.data[1];
                    } else if ((dataBuffer.data.length - dataIndex) == 5) {
                        System.arraycopy(dataBuffer.data, dataIndex, groupWord, 0, 2);
                        System.arraycopy(dataBuffer.data, dataIndex + 2, elementWord, 0, 2);
                        System.arraycopy(dataBuffer.data, dataIndex + 4, zeroWord, 0, 1);
                        zeroWord[1] = followingBuffer.data[0];
                        zeroWord[2] = followingBuffer.data[1];
                        zeroWord[3] = followingBuffer.data[2];
                    } else if ((dataBuffer.data.length - dataIndex) == 4) {
                        System.arraycopy(dataBuffer.data, dataIndex, groupWord, 0, 2);
                        System.arraycopy(dataBuffer.data, dataIndex + 2, elementWord, 0, 2);
                        System.arraycopy(followingBuffer.data, 0, zeroWord, 0, 4);
                    } else if ((dataBuffer.data.length - dataIndex) == 3) {
                        System.arraycopy(dataBuffer.data, dataIndex, groupWord, 0, 2);
                        elementWord[0] = dataBuffer.data[dataIndex + 2];
                        elementWord[1] = followingBuffer.data[0];
                        System.arraycopy(followingBuffer.data, 1, zeroWord, 0, 4);
                    } else if ((dataBuffer.data.length - dataIndex) == 2) {
                        System.arraycopy(dataBuffer.data, dataIndex, groupWord, 0, 2);
                        System.arraycopy(followingBuffer.data, 0, elementWord, 0, 2);
                        System.arraycopy(followingBuffer.data, 2, zeroWord, 0, 4);
                    } else if ((dataBuffer.data.length - dataIndex) == 1) {
                        groupWord[0] = dataBuffer.data[dataIndex];
                        groupWord[1] = followingBuffer.data[0];
                        System.arraycopy(followingBuffer.data, 1, elementWord, 0, 2);
                        System.arraycopy(followingBuffer.data, 3, zeroWord, 0, 4);
                    } else { // we're only using the following buffer
                        System.arraycopy(followingBuffer.data, 0, groupWord, 0, 2);
                        System.arraycopy(followingBuffer.data, 2, elementWord, 0, 2);
                        System.arraycopy(followingBuffer.data, 4, zeroWord, 0, 4);

                        /* this is the only case when we can reset the data buffer
                         * pointers.  All other cases must return pointers into the almost finished data buffer.
                         *
                         * we can now be sure that all the following words will be searched from the following buffer.
                         */
                        seqLength += (dataIndex - dataBuffer.startIndex);
                        dataBuffer = followingBuffer;
                        dataIndex = 0;
                    }
                }
            }
        } while (moreToRead);

        return seqLength + 8;
    }


    /**
     * Read - methods
     *
     * @param   data  DOCUMENT ME!
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */

    /**
     * Simple method that take a byte array and call read with two parameters: byte array and array length.
     *
     * @param   data  byte buffer in which to store the data
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public void read(byte[] data) throws DICOM_Exception {
        read(data, data.length);
    }


    /**
     * Reads a specified number of bytes into the byte data buffer.
     *
     * @param   data    byte data array ( destination )
     * @param   nBytes  the number of bytes to read into the buffer
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public void read(byte[] data, int nBytes) throws DICOM_Exception {
        ByteBuffer byteBuffer;
        int dataOffset = 0;

        if (havePeeked == true) {
            data[0] = peekedByte; // we peeked a look at the first byte
            dataOffset++; // fix up the buffer to reflect the peeked byte
            nBytes--;
            havePeeked = false;
        }

        while (inBuffersLength < nBytes) {
            readBlock(0);
        } // need more data

        while (nBytes > 0) {
            byteBuffer = (ByteBuffer) incomingBuffers.elementAt(0);

            int availableBytes = byteBuffer.endIndex - byteBuffer.startIndex + 1;

            if (nBytes < availableBytes) {
                System.arraycopy(byteBuffer.data, byteBuffer.startIndex, data, dataOffset, nBytes);
                byteBuffer.startIndex += nBytes;
                inBuffersLength -= nBytes;
                nBytes = 0;
            } else {
                System.arraycopy(byteBuffer.data, byteBuffer.startIndex, data, dataOffset, availableBytes);
                dataOffset += availableBytes;
                inBuffersLength -= availableBytes;
                nBytes -= availableBytes;
                ((ByteBuffer)(incomingBuffers.elementAt(0))).finalize();
                incomingBuffers.removeElementAt(0);
            }
        }
    }


    /**
     * The next two methods must be overloaded. As an option 1. make next two methods abstract which requires this class
     * to be abstract. The problems with this involves DICOMCommsLink which must instantiate a a DICOM_Comms and you
     * can't instantiate an abstract class. Therefore, one must try to redesign DICOMCommsLink and others to remove this
     * dependancy. I think this is a good idea since DICOMCommsLink extends DICOMComms and has a DICOMComms - seems a
     * bit odd but it works!.
     *
     * @param   data    DOCUMENT ME!
     * @param   nbytes  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */

    /**
     * Dummy method and should be overloaded by DICOMPDUService.
     *
     * @param   data    Buffer to store the data.
     * @param   nbytes  Number of bytes to be read.
     *
     * @return  DOCUMENT ME!
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public int readBinary(byte[] data, int nbytes) throws DICOM_Exception {
        throw new DICOM_Exception("DICOM_Comms.readBinary: this method should be overwritten.");
    }

    /**
     * Reads a single byte from the data stream.
     *
     * @return  a single byte of data
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public final byte readByte() throws DICOM_Exception {
        read(byteArray1, 1);

        return (byteArray1[0]);
    }

    /**
     * Allocates and.
     *
     * @param   length  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public final byte[] readBytes(int length) throws DICOM_Exception {

        byte[] data = new byte[length];
        read(data);

        return (data);
    }

    /**
     * Reads in a nBytes and puts them in a ByteBuffer and puts the byte buffer in the vector of incomingBuffers.
     *
     * @param   nBytes  number of bytes to read
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public void readFill(int nBytes) throws DICOM_Exception {
        int eCount;

        eCount = inBuffersLength + nBytes;

        while (inBuffersLength < eCount) { // continues to read buffer until all bytes are read
            readBlock(eCount - inBuffersLength); // inBufferLength is updated in readBlock.
        }
    }

    /**
     * Reads a 4 byte integer (otherwise known as an int).
     *
     * @return  an integer
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public final int readInt32() throws DICOM_Exception {
        read(byteArray4, 4);

        return (bufferToInt32(byteArray4, 0, inEndianess));
    }

    /**
     * Reads a 2 byte integer (otherwise known as a short).
     *
     * @return  an integer of the 2 byte data.
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public final int readShort16() throws DICOM_Exception {
        read(byteArray2, 2);

        return (bufferToInt16(byteArray2, 0, inEndianess));
    }

    /**
     * Strips preamble and group 2 tags from ioBuffer if present.
     */
    public void seekToEndOfGroupTwoTags() {

        int group;
        int length;
        boolean implicit = false;

        int dataIndex;
        byte[] groupWord = new byte[2];
        byte[] lengthWord = new byte[] { 0, 0, 0, 0 };
        byte[] lengthHalfWord = new byte[] { 0, 0 };
        ByteBuffer trimmedBuffer = null;

        ByteBuffer dataBuffer = (ByteBuffer) incomingBuffers.elementAt(0);

        if ((dataBuffer.data[128] == 0x44) && (dataBuffer.data[129] == 0x49) && (dataBuffer.data[130] == 0x43) &&
                (dataBuffer.data[131] == 0x4D)) {
            // Found "DICM" therefore it is a Part 10

            dataIndex = 132;

            groupWord[0] = dataBuffer.data[dataIndex];
            groupWord[1] = dataBuffer.data[dataIndex + 1];
            
            // elementWord[0] = dataBuffer.data[dataIndex + 2];
            // elementWord[1] = dataBuffer.data[dataIndex + 3];
            
            // I can't be sure VR is implicit or explicit so I try to test.
            if (((dataBuffer.data[dataIndex + 4] < 65) || (dataBuffer.data[dataIndex + 4] > 90)) && 
                    ((dataBuffer.data[dataIndex + 5] < 65) || (dataBuffer.data[dataIndex + 5] > 90))) {
                implicit = true;
            } else {
                implicit = false;
            }
            
            if (implicit){
                lengthWord[0] = dataBuffer.data[dataIndex + 4];
                lengthWord[1] = dataBuffer.data[dataIndex + 5];
                lengthWord[2] = dataBuffer.data[dataIndex + 6];
                lengthWord[3] = dataBuffer.data[dataIndex + 7];
                //length = bufferToInt32(lengthWord, 0, inEndianess);
                lengthWord[0] = dataBuffer.data[dataIndex + 8];
                lengthWord[1] = dataBuffer.data[dataIndex + 9];
                lengthWord[2] = dataBuffer.data[dataIndex + 10];
                lengthWord[3] = dataBuffer.data[dataIndex + 11];
                length = bufferToInt32(lengthWord, 0, inEndianess);
            }
            else {
                lengthHalfWord[0] = dataBuffer.data[dataIndex + 6];
                lengthHalfWord[1] = dataBuffer.data[dataIndex + 7];
                //length = bufferToInt16(lengthHalfWord, 0, inEndianess);
                lengthWord[0] = dataBuffer.data[dataIndex + 8];
                lengthWord[1] = dataBuffer.data[dataIndex + 9];
                lengthWord[2] = dataBuffer.data[dataIndex + 10];
                lengthWord[3] = dataBuffer.data[dataIndex + 11];
                length = bufferToInt32(lengthWord, 0, inEndianess);
            }

            group = bufferToInt16(groupWord, 0, inEndianess);
            //System.out.println("Group = " + group + " Length = " + length);
            
            if (group != 2) {
                dataIndex = 132;
            } else {
                dataIndex = dataIndex + 12 + length;
            }
            //System.out.println("dataIndex = " + dataIndex);
            trimmedBuffer = new ByteBuffer(dataBuffer.bufferSize - dataIndex);
            System.arraycopy(dataBuffer.data, dataIndex, trimmedBuffer.data, 0, trimmedBuffer.bufferSize);
            trimmedBuffer.startIndex = 0;
            trimmedBuffer.endIndex = trimmedBuffer.bufferSize - 1;
            ((ByteBuffer)(incomingBuffers.elementAt(0))).finalize();
            incomingBuffers.removeElementAt(0);
            incomingBuffers.insertElementAt(trimmedBuffer, 0);
            inBuffersLength = inBuffersLength - dataIndex;
        }

    }

    /**
     * Dummy method and should be overloaded by DICOMPDUService.
     *
     * @param   data        Buffer of data send out the port (connection).
     * @param   startIndex  starting offset
     * @param   nbytes      Number of bytes to be sent.
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    public void sendBinary(byte[] data, int startIndex, int nbytes) throws DICOM_Exception {
        throw new DICOM_Exception("DICOM_Comms.sendBinary: this method should be overwritten.");
    }

    /**
     * Sets the endianess for the incoming data.
     *
     * @param  endianess  the byte ordering (endianess) of the incoming data
     */
    public void setIncomingEndianess(int endianess) {
        inEndianess = endianess;
    }

    /**
     * Sets the endianess for the outgoing data.
     *
     * @param  endianess  the byte ordering (endianess) of the outgoing data
     */
    public void setOutgoingEndianess(int endianess) {
        outEndianess = endianess;
    }


    /**
     * Write - methods
     *
     * @param  byteArray  DOCUMENT ME!
     */

    /**
     * Writes a byte arrary to the outgoing buffers.
     *
     * @param  byteArray  the array of data.
     */
    public final void write(byte[] byteArray) {
        write(byteArray, 0, byteArray.length);
    }

    /**
     * Writes a specified number of bytes from the byte buffer into outGoing ByteBuffer list of byte buffers.
     *
     * @param  data        byte buffer
     * @param  dataOffset  DOCUMENT ME!
     * @param  nBytes      number of bytes to write
     */
    public void write(byte[] data, int dataOffset, int nBytes) {

        int length;
        ByteBuffer byteBuffer;

        if (outgoingBuffers.size() == 0) {
            ByteBuffer byteBuffer2 = new ByteBuffer(breakSize);
            outgoingBuffers.addElement(byteBuffer2);
        }

        while (nBytes > 0) {

            byteBuffer = (ByteBuffer) outgoingBuffers.lastElement();
            length = byteBuffer.bufferSize - byteBuffer.endIndex;

            if (length > 0) {

                if (length <= nBytes) {
                    System.arraycopy(data, dataOffset, byteBuffer.data, byteBuffer.endIndex + 1, length);
                    outBuffersLength += length;
                    nBytes -= length;
                    dataOffset += length;
                    byteBuffer.endIndex += length;

                    ByteBuffer byteBuffer2 = new ByteBuffer(breakSize);
                    outgoingBuffers.addElement(byteBuffer2);
                } else {
                    System.arraycopy(data, dataOffset, byteBuffer.data, byteBuffer.endIndex + 1, nBytes);
                    outBuffersLength += nBytes;
                    byteBuffer.endIndex += nBytes;
                    nBytes = 0;
                }
            } else {
                ByteBuffer byteBuffer2 = new ByteBuffer(ByteBuffer.DEFAULT_SIZE);
                outgoingBuffers.addElement(byteBuffer2);
            }
        }
    }

    /**
     * Writes a byte to the outgoing buffers.
     *
     * @param  value  byte value
     */
    public final void writeByte(byte value) {
        byteArray1[0] = value;
        write(byteArray1, 0, 1);
    }

    /**
     * Writes a 32 (4 byte) integer to the outgoing buffers.
     *
     * @param  value  the value to be sent. The value is put into a 4 byte "byte" buffer in the order indicated by the
     *                specified endianess of this class.
     */
    public final void writeInt32(int value) {
        int32ToBuffer(byteArray4, 0, value, outEndianess);
        write(byteArray4, 0, 4);
    }

    /**
     * Writes a 16 (2 byte) integer to the outgoing buffers.
     *
     * @param  value  the value to be sent. The value is put into a 2 byte "byte" buffer in the order indicated by the
     *                specified endianess of this class.
     */
    public final void writeShort16(int value) {
        int16ToBuffer(byteArray2, 0, value, outEndianess);
        write(byteArray2, 0, 2);
    }


    /**
     * Prepares this class for destruction.
     *
     */
    protected void finalize() {
        srcByteArray = null;
        byteArray1 = null;
        byteArray2 = null;
        byteArray4 = null;

        if (incomingBuffers != null) {
            
            for (int i = 0; i <incomingBuffers.size(); i++ ) {
                ((ByteBuffer) (incomingBuffers.elementAt(i))).finalize();
            }
            incomingBuffers.removeAllElements();
        }

        if (outgoingBuffers != null) {
            for (int i = 0; i <outgoingBuffers.size(); i++ ) {
                ((ByteBuffer) (outgoingBuffers.elementAt(i))).finalize();
            }
            outgoingBuffers.removeAllElements();
        }

        incomingBuffers = null;
        outgoingBuffers = null;

    }

    /**
     * This is the method that actually reads in the data from the socket or IO stream.
     *
     * @param   useBreakSize  the recommended size of the read buffer
     *
     * @throws  DICOM_Exception  DOCUMENT ME!
     */
    private void readBlock(int useBreakSize) throws DICOM_Exception {
        int length, iLength;

        if (useBreakSize == 0) {
            useBreakSize = breakSize;
        }

        if (useBreakSize < MAX_READ_LENGTH) {
            iLength = useBreakSize;
        } else {
            iLength = MAX_READ_LENGTH;
        }

        // actual read from the socket see PDU service.readBinary
        while ((length = readBinary(srcByteArray, iLength)) == 0) {
            ;
        }

        if (length > 0) {
            ByteBuffer byteBuffer = new ByteBuffer(length);
            incomingBuffers.addElement(byteBuffer);
            System.arraycopy(srcByteArray, 0, byteBuffer.data, 0, length);
            inBuffersLength += length;
            byteBuffer.startIndex = 0;
            byteBuffer.endIndex = length - 1;
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Simple inner class to facilitate the memory allocation of of a byte buffer used in DICOM_Comms to support I/O.
     */
    protected class ByteBuffer {

        /** DOCUMENT ME! */
        static final int DEFAULT_SIZE = 32500;

        /** DOCUMENT ME! */
        public int bufferSize = 0;

        /** DOCUMENT ME! */
        public byte[] data = null;

        /** DOCUMENT ME! */
        public int endIndex = -1;

        /** points the last of data in the buffer. */
        public int startIndex = 0;

        /**
         * Constructor.
         *
         * @param  size  size of byte buffer to allocate
         */
        public ByteBuffer(int size) {
            allocateMemory(size);
        }

        /**
         * Prepares this class for destruction.
         *
         */
        protected void finalize() {
           data = null;
        }
        
        /**
         * Prepares this class for destruction.
         *
         */
        public int length() {
           return (endIndex - startIndex);
        }
        
        /**
         * Method that actually allocates the memory.
         *
         * @param  size  size of byte buffer to allocate
         */
        public void allocateMemory(int size) {

            if (size > 0) {
                data = new byte[size];
            } else {
                data = null;
            }

            startIndex = 0;
            endIndex = -1;
            bufferSize = size;
        }
    }


}
