package gov.nih.mipav.model.file;


import gov.nih.mipav.view.*;

import java.io.*;

import javax.swing.event.EventListenerList;


/**
 * FileBase is an abstract class that has many methods that support the reading/writing of files.
 * 
 * @version 0.9 June 30, 1998
 * @see FileInfoBase
 */
public abstract class FileBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Byte order. Rightmost byte is most significant. */
    public static boolean LITTLE_ENDIAN = false;

    /** Byte order. Leftmost byte is most significant. */
    public static boolean BIG_ENDIAN = true;

    /** Read only access. */
    public static final int READ = 0;

    /** Read-write access. */
    public static final int READ_WRITE = 1;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected int bitsPerPixel;

    /** The file names which are opened. */
    protected String[] fileNames;

    /** Flag indicating if the progress bar should be shown. */
    protected boolean pBarVisible = true;

    /** Pointer to file to read or write from. */
    public RandomAccessFile raFile;

    /** DOCUMENT ME! */
    private boolean bigEndian;

    /** DOCUMENT ME! */
    private int dataType;

    /** A list of the ChangeListeners which are interested in the ChangeEvent. */
    private final EventListenerList listenerList;

    /** byte array for short * */
    private final byte[] byteShortBuffer = new byte[2];

    /** byte array for int * */
    private final byte[] byteIntBuffer = new byte[4];

    /** byte array for double * */
    private final byte[] byteDoubleBuffer = new byte[8];

    /** byte array for float * */
    private final byte[] byteFloatBuffer = new byte[4];

    /** byte array for long * */
    private final byte[] byteLongBuffer = new byte[8];

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor.
     */
    public FileBase() {
        this(null);
    }

    /**
     * Creates a new FileBase object.
     * 
     * @param fileNames DOCUMENT ME!
     */
    public FileBase(final String[] fileNames) {
        this.fileNames = fileNames;
        listenerList = new EventListenerList();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Converts from bytes to float
     * 
     * @param bigEndian
     * @param index
     * @param buffer
     * 
     * @return float value
     */
    public final static float bytesToFloat(final boolean bigEndian, final int index, final byte[] buffer) {
        int tmpInt;

        if (bigEndian) {
            tmpInt = ( ( (buffer[index] & 0xff) << 24) | ( (buffer[index + 1] & 0xff) << 16)
                    | ( (buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = ( ( (buffer[index + 3] & 0xff) << 24) | ( (buffer[index + 2] & 0xff) << 16)
                    | ( (buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }

    /**
     * Converts from bytes to double
     * 
     * @param bigEndian
     * @param index
     * @param buffer
     * 
     * @return float value
     */
    public final static double bytesToDouble(final boolean bigEndian, final int index, final byte[] buffer) {
        long tmpLong;

        if (bigEndian) {
            tmpLong = ( ( (buffer[index] & 0xffL) << 56) | ( (buffer[index + 1] & 0xffL) << 48)
                    | ( (buffer[index + 2] & 0xffL) << 40) | ( (buffer[index + 3] & 0xffL) << 32)
                    | ( (buffer[index + 4] & 0xffL) << 24) | ( (buffer[index + 5] & 0xffL) << 16)
                    | ( (buffer[index + 6] & 0xffL) << 8) | (buffer[index + 7] & 0xffL));

        } else {
            tmpLong = ( ( (buffer[index + 7] & 0xffL) << 56) | ( (buffer[index + 6] & 0xffL) << 48)
                    | ( (buffer[index + 5] & 0xffL) << 40) | ( (buffer[index + 4] & 0xffL) << 32)
                    | ( (buffer[index + 3] & 0xffL) << 24) | ( (buffer[index + 2] & 0xffL) << 16)
                    | ( (buffer[index + 1] & 0xffL) << 8) | (buffer[index] & 0xffL));

        }

        return (Double.longBitsToDouble(tmpLong));
    }

    /**
     * Converts from bytes to int
     * 
     * @param bigEndian
     * @param index
     * @param buffer
     * 
     * @return int value
     */
    public final static int bytesToInt(final boolean bigEndian, final int index, final byte[] buffer) {

        if (bigEndian) {
            return ( ( (buffer[index] & 0xff) << 24) | ( (buffer[index + 1] & 0xff) << 16)
                    | ( (buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));
        } else {
            return ( ( (buffer[index + 3] & 0xff) << 24) | ( (buffer[index + 2] & 0xff) << 16)
                    | ( (buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }

    /**
     * Converts from bytes to short
     * 
     * @param bigEndian
     * @param index
     * @param buffer
     * 
     * @return short value
     */
    public final static short bytesToShort(final boolean bigEndian, final int index, final byte[] buffer) {

        if (bigEndian) {
            return (short) ( ( (buffer[index] & 0xff) << 8) | (buffer[index + 1] & 0xff));
        } else {
            return (short) ( ( (buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }

    /**
     * converts from float to bytes
     * 
     * @param data
     * @param bigEndian
     * 
     * @return byte array
     */
    public final static byte[] floatToBytes(final float data, final boolean bigEndian, final byte[] buffer) {
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);

        return FileBase.intToBytes(tmpInt, bigEndian, buffer);
    }

    
    
    /**
     * converts from double to bytes
     * 
     * @param data
     * @param bigEndian
     * 
     * @return byte array
     */
    public final static byte[] doubleToBytes(final double data, final boolean bigEndian, final byte[] buffer) {
        long tmpLong;

        tmpLong = Double.doubleToLongBits(data);


        return FileBase.longToBytes(tmpLong, bigEndian, buffer);
    }
    
    
    
    
    
    

    /**
     * converts from int to bytes
     * 
     * @param data
     * @param bigEndian
     * @param buffer
     * 
     * @return byte array
     */
    public final static byte[] intToBytes(final int data, final boolean bigEndian, final byte[] buffer) {
        if (bigEndian) {
            buffer[0] = (byte) (data >>> 24);
            buffer[1] = (byte) (data >>> 16);
            buffer[2] = (byte) (data >>> 8);
            buffer[3] = (byte) (data & 0xff);
        } else {
            buffer[0] = (byte) (data & 0xff);
            buffer[1] = (byte) (data >>> 8);
            buffer[2] = (byte) (data >>> 16);
            buffer[3] = (byte) (data >>> 24);
        }

        return buffer;
    }

    /**
     * converts from long to bytes
     * 
     * @param data
     * @param bigEndian
     * @param buffer
     * 
     * @return byte array
     */
    public static byte[] longToBytes(final long data, final boolean bigEndian, final byte[] buffer) {
        if (bigEndian) {
            buffer[0] = (byte) (data >>> 56);
            buffer[1] = (byte) (data >>> 48);
            buffer[2] = (byte) (data >>> 40);
            buffer[3] = (byte) (data >>> 32);
            buffer[4] = (byte) (data >>> 24);
            buffer[5] = (byte) (data >>> 16);
            buffer[6] = (byte) (data >>> 8);
            buffer[7] = (byte) (data & 0xff);
        } else {
            buffer[0] = (byte) (data & 0xff);
            buffer[1] = (byte) (data >>> 8);
            buffer[2] = (byte) (data >>> 16);
            buffer[3] = (byte) (data >>> 24);
            buffer[4] = (byte) (data >>> 32);
            buffer[5] = (byte) (data >>> 40);
            buffer[6] = (byte) (data >>> 48);
            buffer[7] = (byte) (data >>> 56);
        }

        return buffer;
    }

    /**
     * converts from short to bytes
     * 
     * @param data
     * @param bigEndian
     * @param buffer
     * 
     * @return byte array
     */
    public final static byte[] shortToBytes(final short data, final boolean bigEndian, final byte[] buffer) {

        if (bigEndian) {
            buffer[0] = (byte) (data >>> 8);
            buffer[1] = (byte) (data & 0xff);
        } else {
            buffer[0] = (byte) (data & 0xff);
            buffer[1] = (byte) (data >>> 8);
        }

        return buffer;
    }

    /**
     * Adds the ProgressChangeListener to this FileBase object.
     * 
     * @param l DOCUMENT ME!
     */
    public void addProgressChangeListener(final ProgressChangeListener l) {
        listenerList.add(ProgressChangeListener.class, l);
    }

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {
        // progressBar = null;

        if (raFile != null) {

            try {
                raFile.close();
            } catch (final IOException ioe) {
                // Do nothing
            }
        }

        raFile = null;
    }

    /**
     * Notifies all listeners that have registered interest for notification on this event type.
     * 
     * @param value the value of the progress bar.
     */
    public void fireProgressStateChanged(final int value) {
        fireProgressStateChanged(value, null, null);
    }

    public void fireProgressStateChanged(final String message) {
        fireProgressStateChanged(ViewJProgressBar.PROGRESS_VALUE_UNCHANGED, null, message);
    }

    /**
     * Notifies all listeners that have registered interest for notification on this event type.
     * 
     * @param value the value of the progress bar.
     * @param title the title of the progress dialog.
     * @param message the message for that specific progress value.
     */
    public void fireProgressStateChanged(final int value, final String title, final String message) {
        final Object[] listeners = listenerList.getListenerList();

        for (int i = listeners.length - 2; i >= 0; i -= 2) {

            if (listeners[i] == ProgressChangeListener.class) {
                final ProgressChangeEvent event = new ProgressChangeEvent(this, value, title, message);
                ((ProgressChangeListener) listeners[i + 1]).progressStateChanged(event);
            }
        }
    }

    public ProgressChangeListener[] getProgressChangeListeners() {
        final Object[] listeners = listenerList.getListenerList();

        int count = 0;
        for (final Object element : listeners) {
            if (element instanceof ProgressChangeListener) {
                count++;
            }
        }

        if (count > 0) {
            final ProgressChangeListener[] pcl = new ProgressChangeListener[count];
            count = 0;
            for (final Object element : listeners) {
                if (element instanceof ProgressChangeListener) {
                    pcl[count] = (ProgressChangeListener) element;
                    count++;
                }
            }
            return pcl;
        } else {
            return null;
        }
    }

    protected void linkProgress(final FileBase fBase) {
        final ProgressChangeListener[] listeners = this.getProgressChangeListeners();
        if (listeners != null) {
            for (final ProgressChangeListener element : listeners) {
                fBase.addProgressChangeListener(element);
            }
        }
    }

    /**
     * Converts byte data to float data.
     * 
     * @param buffer Array of byte data.
     * @param index Index into array data.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return Float value extracted from byte array.
     */
    public final float getBufferFloat(final byte[] buffer, final int index, final boolean bigEndian) {
        int tmpInt;

        if (bigEndian) {
            tmpInt = ( ( (buffer[index] & 0xff) << 24) | ( (buffer[index + 1] & 0xff) << 16)
                    | ( (buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = ( ( (buffer[index + 3] & 0xff) << 24) | ( (buffer[index + 2] & 0xff) << 16)
                    | ( (buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }

    /**
     * Converts byte data to int data.
     * 
     * @param buffer Array of byte data.
     * @param index Index into array data.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return Integer value extracted from byte array.
     */
    public final int getBufferInt(final byte[] buffer, final int index, final boolean bigEndian) {

        if (bigEndian) {
            return ( ( (buffer[index] & 0xff) << 24) | ( (buffer[index + 1] & 0xff) << 16)
                    | ( (buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));
        } else {
            return ( ( (buffer[index + 3] & 0xff) << 24) | ( (buffer[index + 2] & 0xff) << 16)
                    | ( (buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }

    /**
     * Converts byte data to short data.
     * 
     * @param buffer Array of byte data.
     * @param index Index into array data.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return Short value extracted from byte array.
     */
    public final short getBufferShort(final byte[] buffer, final int index, final boolean bigEndian) {

        if (bigEndian) {
            return (short) ( ( (buffer[index] & 0xff) << 8) | (buffer[index + 1] & 0xff));
        } else {
            return (short) ( ( (buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }
    
    /**
     * Converts byte data to int data.
     * 
     * @param buffer Array of byte data.
     * @param index Index into array data.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return int value extracted from byte array.
     */
    public final int getBufferUShort(final byte[] buffer, final int index, final boolean bigEndian) {

        if (bigEndian) {
            return ( ( (buffer[index] & 0xff) << 8) | (buffer[index + 1] & 0xff));
        } else {
            return ( ( (buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }

    /**
     * Gets datatype
     * 
     * @return datatype
     */
    public int getDataType() {
        return dataType;
    }

    /**
     * Reads eight unsigned bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the double read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final double getDouble(final boolean bigEndian) throws IOException {
        raFile.readFully(byteDoubleBuffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = ( ( (byteDoubleBuffer[0] & 0xffL) << 56)
                    | ( (byteDoubleBuffer[1] & 0xffL) << 48) | ( (byteDoubleBuffer[2] & 0xffL) << 40)
                    | ( (byteDoubleBuffer[3] & 0xffL) << 32) | ( (byteDoubleBuffer[4] & 0xffL) << 24)
                    | ( (byteDoubleBuffer[5] & 0xffL) << 16) | ( (byteDoubleBuffer[6] & 0xffL) << 8) | (byteDoubleBuffer[7] & 0xffL));

            return (Double.longBitsToDouble(tmpLong));
        } else {
            tmpLong = ( ( (byteDoubleBuffer[7] & 0xffL) << 56)
                    | ( (byteDoubleBuffer[6] & 0xffL) << 48) | ( (byteDoubleBuffer[5] & 0xffL) << 40)
                    | ( (byteDoubleBuffer[4] & 0xffL) << 32) | ( (byteDoubleBuffer[3] & 0xffL) << 24)
                    | ( (byteDoubleBuffer[2] & 0xffL) << 16) | ( (byteDoubleBuffer[1] & 0xffL) << 8) | (byteDoubleBuffer[0] & 0xffL));

            return (Double.longBitsToDouble(tmpLong));
        }
    }

    /**
     * Reads four unsigned bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the float read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final float getFloat(final boolean bigEndian) throws IOException {

        raFile.readFully(byteFloatBuffer);

        int tmpInt;

        if (bigEndian) {
            tmpInt = ( ( (byteFloatBuffer[0] & 0xff) << 24) | ( (byteFloatBuffer[1] & 0xff) << 16)
                    | ( (byteFloatBuffer[2] & 0xff) << 8) | (byteFloatBuffer[3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = ( ( (byteFloatBuffer[3] & 0xff) << 24) | ( (byteFloatBuffer[2] & 0xff) << 16)
                    | ( (byteFloatBuffer[1] & 0xff) << 8) | (byteFloatBuffer[0] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }

    /**
     * Reads four signed bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the integer read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final int getInt(final boolean bigEndian) throws IOException {

        raFile.readFully(byteIntBuffer);

        if (bigEndian) {
            return ( ( (byteIntBuffer[0] & 0xff) << 24) | ( (byteIntBuffer[1] & 0xff) << 16)
                    | ( (byteIntBuffer[2] & 0xff) << 8) | (byteIntBuffer[3] & 0xff)); // Big Endian
        } else {
            return ( ( (byteIntBuffer[3] & 0xff) << 24) | ( (byteIntBuffer[2] & 0xff) << 16)
                    | ( (byteIntBuffer[1] & 0xff) << 8) | (byteIntBuffer[0] & 0xff));
        }
    }

    /**
     * Reads eight unsigned bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the long read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final long getLong(final boolean bigEndian) throws IOException {

        raFile.readFully(byteLongBuffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = ( ( (byteLongBuffer[0] & 0xffL) << 56) | ( (byteLongBuffer[1] & 0xffL) << 48)
                    | ( (byteLongBuffer[2] & 0xffL) << 40) | ( (byteLongBuffer[3] & 0xffL) << 32)
                    | ( (byteLongBuffer[4] & 0xffL) << 24) | ( (byteLongBuffer[5] & 0xffL) << 16)
                    | ( (byteLongBuffer[6] & 0xffL) << 8) | (byteLongBuffer[7] & 0xffL));

            return (tmpLong);
        } else {
            tmpLong = ( ( (byteLongBuffer[7] & 0xffL) << 56) | ( (byteLongBuffer[6] & 0xffL) << 48)
                    | ( (byteLongBuffer[5] & 0xffL) << 40) | ( (byteLongBuffer[4] & 0xffL) << 32)
                    | ( (byteLongBuffer[3] & 0xffL) << 24) | ( (byteLongBuffer[2] & 0xffL) << 16)
                    | ( (byteLongBuffer[1] & 0xffL) << 8) | (byteLongBuffer[0] & 0xffL));

            return (tmpLong);
        }
    }

    public RandomAccessFile getRaFile() {
        return raFile;
    }

    /**
     * Reads two byte signed short from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of signed short read from the file returned as an int.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final int getSignedShort(final boolean bigEndian) throws IOException {
        int b3 = 0;

        raFile.readFully(byteShortBuffer);

        if (bigEndian) {
            b3 = ( (byteShortBuffer[0] & 0xff) << 8) | (byteShortBuffer[1] & 0xff);
        } else {
            b3 = ( (byteShortBuffer[1] & 0xff) << 8) | (byteShortBuffer[0] & 0xff);
        }

        if ( (b3 & 0x8000) != 0) {
            b3 = b3 | 0xffff0000;
        }

        return b3;
    }

    /**
     * Reads a string from a file of given <code>length</code>.
     * 
     * @param length Number of bytes that form the string.
     * 
     * @return The string read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final String getString(final int length) throws IOException {

        if (length <= 0) {
            return new String("");
        }

        final byte[] b = new byte[length];
        raFile.readFully(b);

        return new String(b);
    }

    /**
     * Reads four unsigned bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the integer read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final long getUInt(final boolean bigEndian) throws IOException {
        raFile.readFully(byteIntBuffer);

        if (bigEndian) {
            return ( ( (byteIntBuffer[0] & 0xffL) << 24) | ( (byteIntBuffer[1] & 0xffL) << 16)
                    | ( (byteIntBuffer[2] & 0xffL) << 8) | (byteIntBuffer[3] & 0xffL)); // Big Endian
        } else {
            return ( ( (byteIntBuffer[3] & 0xffL) << 24) | ( (byteIntBuffer[2] & 0xffL) << 16)
                    | ( (byteIntBuffer[1] & 0xffL) << 8) | (byteIntBuffer[0] & 0xffL));
        }
    }

    /**
     * Converts byte data to unsigned int.
     * 
     * @param buffer Buffer of signed bytes.
     * @param index Index points to location in buffer.
     * 
     * @return Integer converted from as "signed byte".
     */
    public final int getUnsignedByte(final byte[] buffer, final int index) {
        return (buffer[index] & 0xff);
    }

    /**
     * Reads two unsigned bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of unsigned short read from the file returned as an int.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final int getUnsignedShort(final boolean bigEndian) throws IOException {

        raFile.readFully(byteShortBuffer);

        if (bigEndian) {
            return ( ( (byteShortBuffer[0] & 0xff) << 8) | (byteShortBuffer[1] & 0xff)); // Big
                                                                                                            // Endian
        } else {
            return ( ( (byteShortBuffer[1] & 0xff) << 8) | (byteShortBuffer[0] & 0xff)); // Little
                                                                                                            // Endian
        }
    }

    /**
     * Return true if the byte order is big endian.
     * 
     * @return true if the byte order is big endian.
     */
    public boolean isBigEndian() {
        return bigEndian;
    }

    /**
     * Returns flag that indicates that the progressBar is visible.
     * 
     * @return <code>true</code> if progress bar is visible, <code>false</code> if not visible.
     */
    public boolean isProgressBarVisible() {
        return pBarVisible;
    }

    /**
     * reads double value
     * 
     * @param bigEndian
     * 
     * @return double value
     * 
     * @throws IOException
     */
    public final double readDouble(final boolean bigEndian) throws IOException {

        raFile.readFully(byteDoubleBuffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = ( ( (byteDoubleBuffer[0] & 0xffL) << 56)
                    | ( (byteDoubleBuffer[1] & 0xffL) << 48) | ( (byteDoubleBuffer[2] & 0xffL) << 40)
                    | ( (byteDoubleBuffer[3] & 0xffL) << 32) | ( (byteDoubleBuffer[4] & 0xffL) << 24)
                    | ( (byteDoubleBuffer[5] & 0xffL) << 16) | ( (byteDoubleBuffer[6] & 0xffL) << 8) | (byteDoubleBuffer[7] & 0xffL));

        } else {
            tmpLong = ( ( (byteDoubleBuffer[7] & 0xffL) << 56)
                    | ( (byteDoubleBuffer[6] & 0xffL) << 48) | ( (byteDoubleBuffer[5] & 0xffL) << 40)
                    | ( (byteDoubleBuffer[4] & 0xffL) << 32) | ( (byteDoubleBuffer[3] & 0xffL) << 24)
                    | ( (byteDoubleBuffer[2] & 0xffL) << 16) | ( (byteDoubleBuffer[1] & 0xffL) << 8) | (byteDoubleBuffer[0] & 0xffL));

        }

        return (Double.longBitsToDouble(tmpLong));
    }

    /**
     * reads float value
     * 
     * @param bigEndian
     * 
     * @return float value
     * 
     * @throws IOException
     */
    public final float readFloat(final boolean bigEndian) throws IOException {

        raFile.readFully(byteFloatBuffer);

        int tmpInt;

        if (bigEndian) {
            tmpInt = ( ( (byteFloatBuffer[0] & 0xff) << 24) | ( (byteFloatBuffer[1] & 0xff) << 16)
                    | ( (byteFloatBuffer[2] & 0xff) << 8) | (byteFloatBuffer[3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = ( ( (byteFloatBuffer[3] & 0xff) << 24) | ( (byteFloatBuffer[2] & 0xff) << 16)
                    | ( (byteFloatBuffer[1] & 0xff) << 8) | (byteFloatBuffer[0] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }

    /**
     * Reads int value
     * 
     * @param bigEndian
     * 
     * @return int value
     * 
     * @throws IOException
     */
    public final int readInt(final boolean bigEndian) throws IOException {

        raFile.readFully(byteIntBuffer);

        if (bigEndian) {
            return ( ( (byteIntBuffer[0] & 0xff) << 24) | ( (byteIntBuffer[1] & 0xff) << 16)
                    | ( (byteIntBuffer[2] & 0xff) << 8) | (byteIntBuffer[3] & 0xff)); // Big Endian
        } else {
            return ( ( (byteIntBuffer[3] & 0xff) << 24) | ( (byteIntBuffer[2] & 0xff) << 16)
                    | ( (byteIntBuffer[1] & 0xff) << 8) | (byteIntBuffer[0] & 0xff));
        }
    }

    /**
     * reads long value
     * 
     * @param bigEndian
     * 
     * @return long value
     * 
     * @throws IOException
     */
    public final long readLong(final boolean bigEndian) throws IOException {

        raFile.readFully(byteLongBuffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = ( ( (byteLongBuffer[0] & 0xffL) << 56) | ( (byteLongBuffer[1] & 0xffL) << 48)
                    | ( (byteLongBuffer[2] & 0xffL) << 40) | ( (byteLongBuffer[3] & 0xffL) << 32)
                    | ( (byteLongBuffer[4] & 0xffL) << 24) | ( (byteLongBuffer[5] & 0xffL) << 16)
                    | ( (byteLongBuffer[6] & 0xffL) << 8) | (byteLongBuffer[7] & 0xffL));

        } else {
            tmpLong = ( ( (byteLongBuffer[7] & 0xffL) << 56) | ( (byteLongBuffer[6] & 0xffL) << 48)
                    | ( (byteLongBuffer[5] & 0xffL) << 40) | ( (byteLongBuffer[4] & 0xffL) << 32)
                    | ( (byteLongBuffer[3] & 0xffL) << 24) | ( (byteLongBuffer[2] & 0xffL) << 16)
                    | ( (byteLongBuffer[1] & 0xffL) << 8) | (byteLongBuffer[0] & 0xffL));

        }

        return (tmpLong);
    }

    /**
     * reads short value
     * 
     * @param bigEndian
     * 
     * @return short value
     * 
     * @throws IOException
     */
    public final short readShort(final boolean bigEndian) throws IOException {
        short tempShort = 0;

        raFile.readFully(byteShortBuffer);

        if (bigEndian) {
            tempShort = (short) ( ( (byteShortBuffer[0] & 0xff) << 8) | (byteShortBuffer[1] & 0xff));
        } else {
            tempShort = (short) ( ( (byteShortBuffer[1] & 0xff) << 8) | (byteShortBuffer[0] & 0xff));
        }

        return tempShort;
    }

    /**
     * Reads the length of the characters from the file.
     * 
     * @param length the length of the string
     * 
     * @return the string read from the file.
     * 
     * @throws IOException throw IOException if I/O error happens
     */
    public final String readString(final int length) throws IOException {

        if (length <= 0) {
            return null;
        }

        final byte[] buffer = new byte[length];
        raFile.readFully(buffer);

        return new String(buffer);
    }

    /**
     * reads unsigned short value
     * 
     * @param bigEndian
     * 
     * @return int
     * 
     * @throws IOException
     */
    public final int readUnsignedShort(final boolean bigEndian) throws IOException {

        raFile.readFully(byteShortBuffer);

        if (bigEndian) {
            return ( ( (byteShortBuffer[0] & 0xff) << 8) | (byteShortBuffer[1] & 0xff));
        } else {
            return ( ( (byteShortBuffer[1] & 0xff) << 8) | (byteShortBuffer[0] & 0xff));
        }
    }

    /**
     * Removes the ChangeListener from the FileBase object.
     * 
     * 
     */
    public void removeProgressChangeListener(final ProgressChangeListener l) {
        listenerList.remove(ProgressChangeListener.class, l);
    }

    /**
     * sets big endian
     * 
     * @param bigEndian
     */
    public void setBigEndian(final boolean bigEndian) {
        this.bigEndian = bigEndian;
    }

    /**
     * Sets byte buffer with int.
     * 
     * @param buffer Byte buffers where data is to be stored.
     * @param data Float data is broken down in bytes and stored in the byte buffer.
     * @param i Index into byte buffer.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     */
    public final void setBufferFloat(final byte[] buffer, final float data, final int i, final boolean bigEndian) {
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);
        setBufferInt(buffer, tmpInt, i, bigEndian);
    }

    /**
     * Sets byte buffer with int.
     * 
     * @param buffer Byte buffers where data is to be stored.
     * @param data Integer data is broken down in bytes and stored in the byte buffer.
     * @param i Index into byte buffer.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     */
    public final void setBufferInt(final byte[] buffer, final int data, final int i, final boolean bigEndian) {

        if (bigEndian) {
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
     * @param buffer Byte buffers where data is to be stored.
     * @param data Long data is broken down in bytes and stored in the byte buffer.
     * @param i Index into byte buffer.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     */
    public final void setBufferLong(final byte[] buffer, final long data, final int i, final boolean bigEndian) {

        if (bigEndian) {
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
     * @param buffer Byte buffers where data is to be stored.
     * @param data Short data is broken down in bytes and stored in the byte buffer.
     * @param i Index into byte buffer.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     */
    public final void setBufferShort(final byte[] buffer, final short data, final int i, final boolean bigEndian) {

        if (bigEndian) {
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
     * @param buffer Byte buffers where data is to be stored.
     * @param str String containing integer data which is broken down in bytes and stored in the byte buffer.
     * @param i Index into byte buffer.
     */
    public final void setBufferString(final byte[] buffer, final String str, final int i) {

        byte[] tmpBuffer;

        tmpBuffer = str.getBytes();

        for (int c = 0; c < tmpBuffer.length; c++) {
            buffer[i + c] = tmpBuffer[c];
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param dataType DOCUMENT ME!
     */
    public void setDataType(final int dataType) {
        this.dataType = dataType;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param bigEndian DOCUMENT ME!
     */
    public void setEndianess(final boolean bigEndian) {
        this.bigEndian = bigEndian;
    }

    /**
     * Writes a double as eight bytes to a file.
     * 
     * @param data Data to be written to file.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @exception IOException if there is an error writing the file
     */
    public final void writeDouble(final double data, final boolean bigEndian) throws IOException {

        long tmpLong;

        tmpLong = Double.doubleToLongBits(data);
        writeLong(tmpLong, bigEndian);
    }

    /**
     * Writes a float as four bytes to a file.
     * 
     * @param data Data to be written to file.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @exception IOException if there is an error writing the file
     */
    public final void writeFloat(final float data, final boolean bigEndian) throws IOException {
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);
        writeInt(tmpInt, bigEndian);
    }

    /**
     * Writes an int as four bytes to a file.
     * 
     * @param data Data to be written to file.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @exception IOException if there is an error writing the file
     */
    public final void writeInt(final int data, final boolean bigEndian) throws IOException {

        if (bigEndian) {
            byteIntBuffer[0] = (byte) (data >>> 24);
            byteIntBuffer[1] = (byte) (data >>> 16);
            byteIntBuffer[2] = (byte) (data >>> 8);
            byteIntBuffer[3] = (byte) (data & 0xff);
        } else {
            byteIntBuffer[0] = (byte) (data & 0xff);
            byteIntBuffer[1] = (byte) (data >>> 8);
            byteIntBuffer[2] = (byte) (data >>> 16);
            byteIntBuffer[3] = (byte) (data >>> 24);
        }

        raFile.write(byteIntBuffer);
    }
    
    
    
    
    public final void writeBytes(String s) throws IOException{
    	raFile.writeBytes(s);
    }

    /**
     * Writes a long as eight bytes to a file.
     * 
     * @param data Data to be written to file.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @exception IOException if there is an error writing the file
     */
    public final void writeLong(final long data, final boolean bigEndian) throws IOException {

        if (bigEndian) {
            byteLongBuffer[0] = (byte) (data >>> 56);
            byteLongBuffer[1] = (byte) (data >>> 48);
            byteLongBuffer[2] = (byte) (data >>> 40);
            byteLongBuffer[3] = (byte) (data >>> 32);
            byteLongBuffer[4] = (byte) (data >>> 24);
            byteLongBuffer[5] = (byte) (data >>> 16);
            byteLongBuffer[6] = (byte) (data >>> 8);
            byteLongBuffer[7] = (byte) (data & 0xff);
        } else {
            byteLongBuffer[0] = (byte) (data & 0xff);
            byteLongBuffer[1] = (byte) (data >>> 8);
            byteLongBuffer[2] = (byte) (data >>> 16);
            byteLongBuffer[3] = (byte) (data >>> 24);
            byteLongBuffer[4] = (byte) (data >>> 32);
            byteLongBuffer[5] = (byte) (data >>> 40);
            byteLongBuffer[6] = (byte) (data >>> 48);
            byteLongBuffer[7] = (byte) (data >>> 56);
        }

        raFile.write(byteLongBuffer);
    }

    /**
     * Writes a short as two bytes to a file.
     * 
     * @param data Data to be written to file.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @exception IOException if there is an error writing the file
     */
    public final void writeShort(final short data, final boolean bigEndian) throws IOException {

        if (bigEndian) {
            byteShortBuffer[0] = (byte) (data >>> 8);
            byteShortBuffer[1] = (byte) (data & 0xff);
        } else {
            byteShortBuffer[0] = (byte) (data & 0xff);
            byteShortBuffer[1] = (byte) (data >>> 8);
        }

        raFile.write(byteShortBuffer);
    }
}
