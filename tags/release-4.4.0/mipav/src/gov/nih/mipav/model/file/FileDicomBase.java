package gov.nih.mipav.model.file;


import gov.nih.mipav.view.*;

import java.io.*;


/**
 * FileDICOMBase is an class that supports the reading/writing of DICOM files. It reads in a buffer of tags that can be
 * parsed more quickly than continued random accesses to the harddrive.
 *
 * @version  1.0 June 30, 2005
 */
public class FileDicomBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

	protected static final String TYPE_STRING = "typeString";

    protected static final String OTHER_BYTE_STRING = "otherByteString";

    protected static final String OTHER_WORD_STRING = "otherWordString";

    protected static final String TYPE_SHORT = "typeShort";

	protected static final String TYPE_INT = "typeInt";

	protected static final String TYPE_FLOAT = "typeFloat";

	protected static final String TYPE_DOUBLE = "typeDouble";

	protected static final String TYPE_UNKNOWN = "typeUnknown";

	protected static final String TYPE_SEQUENCE = "typeSequence";
	
    /** Byte order. Rightmost byte is most significant. */
    public static final boolean LITTLE_ENDIAN = false;

    /** Byte order. Leftmost byte is most significant. */
    public static final boolean BIG_ENDIAN = true;

    /** Read only access. */
    public static final int READ = 0;

    /** Read-write access. */
    public static final int READ_WRITE = 1;

    /** The size of the buffer that contains the tags of the DICOM image. Default = 4Mil. */
    public static final int BUFFER_SIZE = 4000000;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Two byte array used to read/write in data so that one doesn't't need to be allocated with each read/write. */
    protected byte[] byteBuffer2 = new byte[2];

    /** Four byte array used to read/write in data so that one doesn't need to be allocated with each read/write. */
    protected byte[] byteBuffer4 = new byte[4];

    /** Eight byte array used to read/write in data so that they don't need to be allocated with each read/write. */
    protected byte[] byteBuffer8 = new byte[8];

    /** Total length of the image file. */
    protected long fLength = 0;

    /** Flag indicating if the progress bar should be shown. */
    protected boolean pBarVisible = true;

    /** Progress bar to show when reading in image file. */
    protected ProgressBarInterface progressBar;

    /** Pointer to file to read or write from. */
    protected RandomAccessFile raFile;

    /** The buffer that holds the tags of the DICOM image. */
    protected byte[] tagBuffer = null;


    /** Integer variable used to read/write in data so that they don't need to be allocated with each read/write. */
    private int b1, b2, b3, b4, b5, b6, b7, b8;

    /** Buffer pointer (aka file pointer). */
    private int bPtr = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor.
     */
    public FileDicomBase() { }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {
        byteBuffer2 = null;
        byteBuffer4 = null;
        byteBuffer8 = null;
        progressBar = null;
        tagBuffer = null;

        if (raFile != null) {

            try {
                raFile.close();
            } catch (IOException ioe) {
                // Do nothing
            }
        }

        raFile = null;
    }


    /**
     * Reads unsigned bytes from file.
     *
     * @return     The value of unsigned byte read from the file returned as an int.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getByte() throws IOException {
        b3 = 0;

        b3 = (tagBuffer[bPtr] & 0xff);
        bPtr += 1;

        return b3;
    }

    /**
     * Reads eight unsigned bytes from file.
     *
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the double read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final double getDouble(boolean endianess) throws IOException {
        b1 = (tagBuffer[bPtr] & 0xff);
        b2 = (tagBuffer[bPtr + 1] & 0xff);
        b3 = (tagBuffer[bPtr + 2] & 0xff);
        b4 = (tagBuffer[bPtr + 3] & 0xff);
        b5 = (tagBuffer[bPtr + 4] & 0xff);
        b6 = (tagBuffer[bPtr + 5] & 0xff);
        b7 = (tagBuffer[bPtr + 6] & 0xff);
        b8 = (tagBuffer[bPtr + 7] & 0xff);

        long tmpLong;

        if (endianess == BIG_ENDIAN) {
            tmpLong = (((long) b1 << 56) | ((long) b2 << 48) | ((long) b3 << 40) | ((long) b4 << 32) |
                           ((long) b5 << 24) | ((long) b6 << 16) | ((long) b7 << 8) | b8);
        } else {
            tmpLong = (((long) b8 << 56) | ((long) b7 << 48) | ((long) b6 << 40) | ((long) b5 << 32) |
                           ((long) b4 << 24) | ((long) b3 << 16) | ((long) b2 << 8) | b1);
        }

        bPtr += 8;

        return (Double.longBitsToDouble(tmpLong));

    }


    /**
     * Reads four unsigned bytes from file.
     *
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the float read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final float getFloat(boolean endianess) throws IOException {
        int tmpInt;

        if (endianess == BIG_ENDIAN) {
            tmpInt = (((tagBuffer[bPtr] & 0xff) << 24) | ((tagBuffer[bPtr + 1] & 0xff) << 16) |
                          ((tagBuffer[bPtr + 2] & 0xff) << 8) | (tagBuffer[bPtr + 3] & 0xff));
        } else {
            tmpInt = (((tagBuffer[bPtr + 3] & 0xff) << 24) | ((tagBuffer[bPtr + 2] & 0xff) << 16) |
                          ((tagBuffer[bPtr + 1] & 0xff) << 8) | (tagBuffer[bPtr] & 0xff));
        }

        bPtr += 4;

        return (Float.intBitsToFloat(tmpInt));
    }


    /**
     * Reads four signed bytes from file.
     *
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the integer read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getInt(boolean endianess) throws IOException {

        b3 = 0;

        if (endianess == BIG_ENDIAN) {
            b3 = ((tagBuffer[bPtr] & 0xff) << 24) | ((tagBuffer[bPtr + 1] & 0xff) << 16) |
                     ((tagBuffer[bPtr + 2] & 0xff) << 8) | (tagBuffer[bPtr + 3] & 0xff); // Big Endian
        } else {
            b3 = ((tagBuffer[bPtr + 3] & 0xff) << 24) | ((tagBuffer[bPtr + 2] & 0xff) << 16) |
                     ((tagBuffer[bPtr + 1] & 0xff) << 8) | (tagBuffer[bPtr] & 0xff);
        }

        bPtr += 4;

        return b3;
    }

    /**
     * Reads eight unsigned bytes from file.
     *
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the long read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final long getLong(boolean endianess) throws IOException {
        b1 = (tagBuffer[bPtr] & 0xff);
        b2 = (tagBuffer[bPtr + 1] & 0xff);
        b3 = (tagBuffer[bPtr + 2] & 0xff);
        b4 = (tagBuffer[bPtr + 3] & 0xff);
        b5 = (tagBuffer[bPtr + 4] & 0xff);
        b6 = (tagBuffer[bPtr + 5] & 0xff);
        b7 = (tagBuffer[bPtr + 6] & 0xff);
        b8 = (tagBuffer[bPtr + 7] & 0xff);

        long tmpLong;

        if (endianess == BIG_ENDIAN) {
            tmpLong = (((long) b1 << 56) | ((long) b2 << 48) | ((long) b3 << 40) | ((long) b4 << 32) |
                           ((long) b5 << 24) | ((long) b6 << 16) | ((long) b7 << 8) | b8);
        } else {
            tmpLong = (((long) b8 << 56) | ((long) b7 << 48) | ((long) b6 << 40) | ((long) b5 << 32) |
                           ((long) b4 << 24) | ((long) b3 << 16) | ((long) b2 << 8) | b1);
        }

        bPtr += 8;

        return (tmpLong);
    }

    /**
     * Reads two byte signed short from file.
     *
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of signed short read from the file returned as an int.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getSignedShort(boolean endianess) throws IOException {
        b3 = 0;

        if (endianess == BIG_ENDIAN) {
            b3 = ((tagBuffer[bPtr] & 0xff) << 8) | (tagBuffer[bPtr + 1] & 0xff);
        } else {
            b3 = ((tagBuffer[bPtr + 1] & 0xff) << 8) | (tagBuffer[bPtr] & 0xff);
        }

        if ((b3 & 0x0080) != 0) {
            b3 = b3 | 0xff00;
        }

        bPtr += 2;

        return b3;
    }


    /**
     * Reads a string from a file of given <code>length</code>.
     *
     * @param      length  Number of bytes that form the string.
     *
     * @return     The string read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final String getString(int length) throws IOException {

        if (length <= 0) {
            return new String("");
        }

        byte[] b = new byte[length];

        for (int i = 0; i < length; i++) {
            b[i] = tagBuffer[bPtr++];
        }
        String s = new String(b);
        b = null;
        return s;
    }

    /**
     * Reads a string from a file of given <code>length</code>.
     *
     * @param      length  Number of bytes that form the string.
     *
     * @return     The string read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final String getStringFromFile(int length) throws IOException {

        if (length <= 0) {
            return new String("");
        }

        byte[] b = new byte[length];
        raFile.readFully(b);
        String s = new String(b);
        b = null;
        return s;
    }

    /**
     * Reads four unsigned bytes from file.
     *
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the integer read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final long getUInt(boolean endianess) throws IOException {

        long val = 0;

        if (endianess == BIG_ENDIAN) {
            val = ((tagBuffer[bPtr] & 0xffL) << 24) | ((tagBuffer[bPtr + 1] & 0xffL) << 16) |
                      ((tagBuffer[bPtr + 2] & 0xffL) << 8) | (tagBuffer[bPtr + 3] & 0xffL); // Big Endian
        } else {
            val = ((tagBuffer[bPtr + 3] & 0xffL) << 24) | ((tagBuffer[bPtr + 2] & 0xffL) << 16) |
                      ((tagBuffer[bPtr + 1] & 0xffL) << 8) | (tagBuffer[bPtr] & 0xffL);
        }

        bPtr += 4;

        return val;
    }


    /**
     * Reads two unsigned bytes from file.
     *
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of unsigned short read from the file returned as an int.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getUnsignedShort(boolean endianess) throws IOException {
        b3 = 0;

        if (endianess == BIG_ENDIAN) {
            b3 = ((tagBuffer[bPtr] & 0xff) << 8) | (tagBuffer[bPtr + 1] & 0xff); // Big Endian
        } else {
            b3 = ((tagBuffer[bPtr + 1] & 0xff) << 8) | (tagBuffer[bPtr] & 0xff); // Little Endian
        }

        bPtr += 2;

        return b3;
    }

    /**
     * Setups the allocation of memory for the byte buffer to load the entire image.
     */
    public void initializeFullRead() {

        try {
            bPtr = 0;

            if (raFile != null) {
                fLength = raFile.length();
            } else {
                return;
            }

            if (tagBuffer == null) {
                tagBuffer = new byte[(int) fLength];
            } else if (fLength > tagBuffer.length) {
                tagBuffer = new byte[(int) fLength];
            }

            raFile.readFully(tagBuffer);
        } catch (IOException ioE) { }
    }

    /**
     * Returns flag that indicates that the progressBar is visible.
     *
     * @return  <code>true</code> if progress bar is visible, <code>false</code> if not visible.
     */
    public boolean isProgressBarVisible() {
        return pBarVisible;
    }

    /**
     * Setups the allocation of memory for the byte buffer to load tags. This tag buffer is a cache to speed the loading
     * of the images.
     */
    public void loadTagBuffer() {

        try {
            bPtr = 0;

            if (raFile != null) {
                fLength = raFile.length();
            }

            if (fLength < BUFFER_SIZE) {

                if (tagBuffer == null) {
                    tagBuffer = new byte[(int) fLength];
                } else if (fLength > tagBuffer.length) {
                    tagBuffer = new byte[(int) fLength];
                }
            } else if (tagBuffer == null) {
                tagBuffer = new byte[BUFFER_SIZE];
            } else if (tagBuffer.length < BUFFER_SIZE) {
                tagBuffer = new byte[BUFFER_SIZE];
            }

            raFile.readFully(tagBuffer);
        } catch (IOException ioE) { }
    }

    /**
     * Sets byte buffer with int.
     *
     * @param  buffer     Byte buffers where data is to be stored.
     * @param  data       Float data is broken down in bytes and stored in the byte buffer.
     * @param  i          Index into byte buffer.
     * @param  endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
     */
    public final void setBufferFloat(byte[] buffer, float data, int i, boolean endianess) {
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);
        setBufferInt(buffer, tmpInt, i, endianess);
    }

    /**
     * Sets byte buffer with int.
     *
     * @param  buffer     Byte buffers where data is to be stored.
     * @param  data       Integer data is broken down in bytes and stored in the byte buffer.
     * @param  i          Index into byte buffer.
     * @param  endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
     */
    public final void setBufferInt(byte[] buffer, int data, int i, boolean endianess) {

        if (endianess == BIG_ENDIAN) {
            buffer[i] = (byte) (data >>> 24);
            buffer[i + 1] = (byte) (data >>> 16);
            buffer[i + 2] = (byte) (data >>> 8);
            buffer[i + 3] = (byte) (data & 0xff);
        } else {
            buffer[i] = (byte) (data & 0xff);
            buffer[i + 1] = (byte) (data >>> 8);
            buffer[i + 2] = (byte) (data >>> 16);
            buffer[i + 3] = (byte) (data >>> 24);
        }
    }

    /**
     * Sets byte buffer with long.
     *
     * @param  buffer     Byte buffers where data is to be stored.
     * @param  data       Long data is broken down in bytes and stored in the byte buffer.
     * @param  i          Index into byte buffer.
     * @param  endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
     */
    public final void setBufferLong(byte[] buffer, long data, int i, boolean endianess) {

        if (endianess == BIG_ENDIAN) {
            buffer[i] = (byte) (data >>> 56);
            buffer[i + 1] = (byte) (data >>> 48);
            buffer[i + 2] = (byte) (data >>> 40);
            buffer[i + 3] = (byte) (data >>> 32);
            buffer[i + 4] = (byte) (data >>> 24);
            buffer[i + 5] = (byte) (data >>> 16);
            buffer[i + 6] = (byte) (data >>> 8);
            buffer[i + 7] = (byte) (data & 0xff);
        } else {
            buffer[i] = (byte) (data & 0xff);
            buffer[i + 1] = (byte) (data >>> 8);
            buffer[i + 2] = (byte) (data >>> 16);
            buffer[i + 3] = (byte) (data >>> 24);
            buffer[i + 4] = (byte) (data >>> 32);
            buffer[i + 5] = (byte) (data >>> 40);
            buffer[i + 6] = (byte) (data >>> 48);
            buffer[i + 7] = (byte) (data >>> 56);
        }
    }


    /**
     * Sets byte buffer with int.
     *
     * @param  buffer     Byte buffers where data is to be stored.
     * @param  data       Short data is broken down in bytes and stored in the byte buffer.
     * @param  i          Index into byte buffer.
     * @param  endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
     */
    public final void setBufferShort(byte[] buffer, short data, int i, boolean endianess) {

        if (endianess == BIG_ENDIAN) {
            buffer[i] = (byte) (data >>> 8);
            buffer[i + 1] = (byte) (data & 0xff);
        } else {
            buffer[i] = (byte) (data & 0xff);
            buffer[i + 1] = (byte) (data >>> 8);
        }
    }

    /**
     * Sets byte buffer with int.
     *
     * @param  buffer  Byte buffers where data is to be stored.
     * @param  str     String containing integer data which is broken down in bytes and stored in the byte buffer.
     * @param  i       Index into byte buffer.
     */
    public final void setBufferString(byte[] buffer, String str, int i) {

        byte[] tmpBuffer;

        tmpBuffer = str.getBytes();

        for (int c = 0; c < tmpBuffer.length; c++) {
            buffer[i + c] = tmpBuffer[c];
        }
    }

    /**
     * Sets whether or not the progress bar should be visible.
     *
     * @param  flag  <code>true</code> if should be visible, <code>false</code> if not visible.
     */
    public void setProgressBarVisible(boolean flag) {
        pBarVisible = flag;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  buffer  byte[]
     */
    public final void setTagBuffer(byte[] buffer) {
        tagBuffer = buffer;
        fLength = buffer.length;
    }

    /**
     * Writes a double as eight bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeDouble(double data, boolean endianess) throws IOException {

        long tmpLong;

        tmpLong = Double.doubleToLongBits(data);
        writeLong(tmpLong, endianess);
    }

    /**
     * Writes a float as four bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeFloat(float data, boolean endianess) throws IOException {
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);
        writeInt(tmpInt, endianess);
    }

    /**
     * Writes an int as four bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeInt(int data, boolean endianess) throws IOException {

        if (endianess == BIG_ENDIAN) {
            byteBuffer4[0] = (byte) (data >>> 24);
            byteBuffer4[1] = (byte) (data >>> 16);
            byteBuffer4[2] = (byte) (data >>> 8);
            byteBuffer4[3] = (byte) (data & 0xff);
        } else {
            byteBuffer4[0] = (byte) (data & 0xff);
            byteBuffer4[1] = (byte) (data >>> 8);
            byteBuffer4[2] = (byte) (data >>> 16);
            byteBuffer4[3] = (byte) (data >>> 24);
        }

        raFile.write(byteBuffer4);
    }

    /**
     * Writes a long as eight bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeLong(long data, boolean endianess) throws IOException {

        if (endianess == BIG_ENDIAN) {
            byteBuffer8[0] = (byte) (data >>> 56);
            byteBuffer8[1] = (byte) (data >>> 48);
            byteBuffer8[2] = (byte) (data >>> 40);
            byteBuffer8[3] = (byte) (data >>> 32);
            byteBuffer8[4] = (byte) (data >>> 24);
            byteBuffer8[5] = (byte) (data >>> 16);
            byteBuffer8[6] = (byte) (data >>> 8);
            byteBuffer8[7] = (byte) (data & 0xff);
        } else {
            byteBuffer8[0] = (byte) (data & 0xff);
            byteBuffer8[1] = (byte) (data >>> 8);
            byteBuffer8[2] = (byte) (data >>> 16);
            byteBuffer8[3] = (byte) (data >>> 24);
            byteBuffer8[4] = (byte) (data >>> 32);
            byteBuffer8[5] = (byte) (data >>> 40);
            byteBuffer8[6] = (byte) (data >>> 48);
            byteBuffer8[7] = (byte) (data >>> 56);
        }

        raFile.write(byteBuffer8);
    }


    /**
     * Writes a short as two bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeShort(short data, boolean endianess) throws IOException {

        if (endianess == BIG_ENDIAN) {
            byteBuffer2[0] = (byte) (data >>> 8);
            byteBuffer2[1] = (byte) (data & 0xff);
        } else {
            byteBuffer2[0] = (byte) (data & 0xff);
            byteBuffer2[1] = (byte) (data >>> 8);
        }

        raFile.write(byteBuffer2);
    }

    /**
     * Gets the file/buffer pointer and returns it.
     *
     * @return  the file/buffer pointer
     */
    protected final int getFilePointer() {
        return bPtr;
    }


    /**
     * Reads into the supplied buffer data from the DICOM tag buffer.
     *
     * @param  byteBuffer  byte[]
     */
    protected final void read(byte[] byteBuffer) {

        System.arraycopy(tagBuffer, bPtr, byteBuffer, 0, byteBuffer.length);
        bPtr += byteBuffer.length;
    }

    /**
     * Seeks to a point in the buffer.
     *
     * @param  value  indicates the new buffer pointer value
     */
    protected final void seek(int value) {
        bPtr = value;
    }

    /**
     * Skips to a new point in the buffer.
     *
     * @param  value  number of bytes to skip
     */
    protected final void skipBytes(int value) {
        bPtr = bPtr + value;
    }

}
