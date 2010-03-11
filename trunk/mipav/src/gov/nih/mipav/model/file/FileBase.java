package gov.nih.mipav.model.file;


import gov.nih.mipav.view.*;

import java.io.*;

import javax.swing.event.*;


/**
 * FileBase is an abstract class that has many methods that support the reading/writing of files.
 *
 * @version  0.9 June 30, 1998
 * @see      FileInfoBase
 */
public abstract class FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Byte order. Rightmost byte is most significant. */
    public static boolean LITTLE_ENDIAN = false;

    /** Byte order. Leftmost byte is most significant. */
    public static boolean BIG_ENDIAN = true;

    /** Read only access. */
    public static final int READ = 0;

    /** Read-write access. */
    public static final int READ_WRITE = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

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
    private EventListenerList listenerList;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor.
     */
    public FileBase() {
        this(null);
    }

    /**
     * Creates a new FileBase object.
     *
     * @param  fileNames  DOCUMENT ME!
     */
    public FileBase(String[] fileNames) {
        this.fileNames = fileNames;
        listenerList = new EventListenerList();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param   bigEndian  DOCUMENT ME!
     * @param   index      DOCUMENT ME!
     * @param   buffer     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final static float bytesToFloat(boolean bigEndian, int index, byte[] buffer) {
        int tmpInt;

        if (bigEndian) {
            tmpInt = (((buffer[index] & 0xff) << 24) | ((buffer[index + 1] & 0xff) << 16) |
                          ((buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = (((buffer[index + 3] & 0xff) << 24) | ((buffer[index + 2] & 0xff) << 16) |
                          ((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   bigEndian  DOCUMENT ME!
     * @param   index      DOCUMENT ME!
     * @param   buffer     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final static int bytesToInt(boolean bigEndian, int index, byte[] buffer) {

        if (bigEndian) {
            return (((buffer[index] & 0xff) << 24) | ((buffer[index + 1] & 0xff) << 16) |
                        ((buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));
        } else {
            return (((buffer[index + 3] & 0xff) << 24) | ((buffer[index + 2] & 0xff) << 16) |
                        ((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   bigEndian  DOCUMENT ME!
     * @param   index      DOCUMENT ME!
     * @param   buffer     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final static short bytesToShort(boolean bigEndian, int index, byte[] buffer) {

        if (bigEndian) {
            return (short) (((buffer[index] & 0xff) << 8) | (buffer[index + 1] & 0xff));
        } else {
            return (short) (((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   data       DOCUMENT ME!
     * @param   bigEndian  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final static byte[] floatToBytes(float data, boolean bigEndian) {
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);

        return intToBytes(tmpInt, bigEndian);
    }

    // public abstract String[] getExtensions();

    /**
     * DOCUMENT ME!
     *
     * @param   dataType  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static int getBitsPerPixel(int dataType) {
        return 0;
    }

    /**
     * Returns the string for a particular file format.
     *
     * @param   format  int representing the file format (see the static definitions)
     *
     * @return  string representing the file format
     */
    /**public static String getFileFormatStr(int format) {

        if (format == FileUtility.ERROR) {
            return "Error";
        }

        try {
            return FileBase.fileFormatStr[format];
        } catch (ArrayIndexOutOfBoundsException ae) { }

        return "";

    } **/
    

    /**
     * DOCUMENT ME!
     *
     * @param   data       DOCUMENT ME!
     * @param   bigEndian  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final static byte[] intToBytes(int data, boolean bigEndian) {
        final byte[] buffer = new byte[4];

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
     * DOCUMENT ME!
     *
     * @param   data       DOCUMENT ME!
     * @param   bigEndian  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static byte[] longToBytes(long data, boolean bigEndian) {
        final byte[] buffer = new byte[8];

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
     * DOCUMENT ME!
     *
     * @param   data       DOCUMENT ME!
     * @param   bigEndian  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final static byte[] shortToBytes(short data, boolean bigEndian) {
    	final byte[] buffer = new byte[2];

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
     * @param  l  DOCUMENT ME!
     */
    public void addProgressChangeListener(ProgressChangeListener l) {
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
            } catch (IOException ioe) {
                // Do nothing
            }
        }

        raFile = null;
    }

    /**
     * Notifies all listeners that have registered interest for notification on this event type.
     *
     * @param  value  the value of the progress bar.
     */
    public void fireProgressStateChanged(int value) {
        fireProgressStateChanged(value, null, null);
    }

    public void fireProgressStateChanged(String message) {
    	fireProgressStateChanged(ViewJProgressBar.PROGRESS_VALUE_UNCHANGED, null, message);
    }
    
    /**
     * Notifies all listeners that have registered interest for notification on this event type.
     *
     * @param  value    the value of the progress bar.
     * @param  title    the title of the progress dialog.
     * @param  message  the message for that specific progress value.
     */
    public void fireProgressStateChanged(int value, String title, String message) {
        Object[] listeners = listenerList.getListenerList();

        for (int i = listeners.length - 2; i >= 0; i -= 2) {

            if (listeners[i] == ProgressChangeListener.class) {
                ProgressChangeEvent event = new ProgressChangeEvent(this, value, title, message);
                ((ProgressChangeListener) listeners[i + 1]).progressStateChanged(event);
            }
        }
    }

    public ProgressChangeListener [] getProgressChangeListeners() {
        Object[] listeners = listenerList.getListenerList();
        
        int count = 0;
        for (int i = 0; i < listeners.length; i++) {
            if (listeners[i] instanceof ProgressChangeListener) {
                count++;
            }
        }
        
        if (count > 0) {
            ProgressChangeListener [] pcl = new ProgressChangeListener[count];
            count = 0;
            for (int i = 0; i < listeners.length; i++) {
                if (listeners[i] instanceof ProgressChangeListener) {
                    pcl[count] = (ProgressChangeListener)listeners[i];
                    count++;
                }
            }
            return pcl;
        } else {
            return null;
        }
    }

    protected void linkProgress(FileBase fBase) {
    	ProgressChangeListener[] listeners = this.getProgressChangeListeners();
        if (listeners != null) {
            for (int i = 0; i < listeners.length; i++) {
            	fBase.addProgressChangeListener(listeners[i]);
            }
        }
    }
    
    /**
     * Converts byte data to float data.
     *
     * @param   buffer     Array of byte data.
     * @param   index      Index into array data.
     * @param   bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                     endian.
     *
     * @return  Float value extracted from byte array.
     */
    public final float getBufferFloat(byte[] buffer, int index, boolean bigEndian) {
        int tmpInt;

        if (bigEndian) {
            tmpInt = (((buffer[index] & 0xff) << 24) | ((buffer[index + 1] & 0xff) << 16) |
                          ((buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = (((buffer[index + 3] & 0xff) << 24) | ((buffer[index + 2] & 0xff) << 16) |
                          ((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }


    /**
     * Converts byte data to int data.
     *
     * @param   buffer     Array of byte data.
     * @param   index      Index into array data.
     * @param   bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                     endian.
     *
     * @return  Integer value extracted from byte array.
     */
    public final int getBufferInt(byte[] buffer, int index, boolean bigEndian) {

        if (bigEndian) {
            return (((buffer[index] & 0xff) << 24) | ((buffer[index + 1] & 0xff) << 16) |
                        ((buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));
        } else {
            return (((buffer[index + 3] & 0xff) << 24) | ((buffer[index + 2] & 0xff) << 16) |
                        ((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }


    /**
     * Converts byte data to short data.
     *
     * @param   buffer     Array of byte data.
     * @param   index      Index into array data.
     * @param   bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                     endian.
     *
     * @return  Short value extracted from byte array.
     */
    public final short getBufferShort(byte[] buffer, int index, boolean bigEndian) {

        if (bigEndian) {
            return (short) (((buffer[index] & 0xff) << 8) | (buffer[index + 1] & 0xff));
        } else {
            return (short) (((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getDataType() {
        return dataType;
    }

    /**
     * Reads eight unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the double read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final double getDouble(boolean bigEndian) throws IOException {
    	final byte[] buffer = new byte[8];
        raFile.readFully(buffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = (((buffer[0] & 0xffL) << 56) | ((buffer[1] & 0xffL) << 48) | ((buffer[2] & 0xffL) << 40) |
                           ((buffer[3] & 0xffL) << 32) | ((buffer[4] & 0xffL) << 24) | ((buffer[5] & 0xffL) << 16) |
                           ((buffer[6] & 0xffL) << 8) | (buffer[7] & 0xffL));

            return (Double.longBitsToDouble(tmpLong));
        } else {
            tmpLong = (((buffer[7] & 0xffL) << 56) | ((buffer[6] & 0xffL) << 48) | ((buffer[5] & 0xffL) << 40) |
                           ((buffer[4] & 0xffL) << 32) | ((buffer[3] & 0xffL) << 24) | ((buffer[2] & 0xffL) << 16) |
                           ((buffer[1] & 0xffL) << 8) | (buffer[0] & 0xffL));

            return (Double.longBitsToDouble(tmpLong));
        }
    }


    /**
     * Reads four unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the float read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final float getFloat(boolean bigEndian) throws IOException {
        final byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        int tmpInt;

        if (bigEndian) {
            tmpInt = (((buffer[0] & 0xff) << 24) | ((buffer[1] & 0xff) << 16) | ((buffer[2] & 0xff) << 8) |
                          (buffer[3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = (((buffer[3] & 0xff) << 24) | ((buffer[2] & 0xff) << 16) | ((buffer[1] & 0xff) << 8) |
                          (buffer[0] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }


    /**
     * Reads four signed bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the integer read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getInt(boolean bigEndian) throws IOException {
    	final byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xff) << 24) | ((buffer[1] & 0xff) << 16) | ((buffer[2] & 0xff) << 8) |
                        (buffer[3] & 0xff)); // Big Endian
        } else {
            return (((buffer[3] & 0xff) << 24) | ((buffer[2] & 0xff) << 16) | ((buffer[1] & 0xff) << 8) |
                        (buffer[0] & 0xff));
        }
    }

    /**
     * Reads eight unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the long read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final long getLong(boolean bigEndian) throws IOException {
    	final byte[] buffer = new byte[8];

        raFile.readFully(buffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = (((buffer[0] & 0xffL) << 56) | ((buffer[1] & 0xffL) << 48) | ((buffer[2] & 0xffL) << 40) |
                           ((buffer[3] & 0xffL) << 32) | ((buffer[4] & 0xffL) << 24) | ((buffer[5] & 0xffL) << 16) |
                           ((buffer[6] & 0xffL) << 8) | (buffer[7] & 0xffL));

            return (tmpLong);
        } else {
            tmpLong = (((buffer[7] & 0xffL) << 56) | ((buffer[6] & 0xffL) << 48) | ((buffer[5] & 0xffL) << 40) |
                           ((buffer[4] & 0xffL) << 32) | ((buffer[3] & 0xffL) << 24) | ((buffer[2] & 0xffL) << 16) |
                           ((buffer[1] & 0xffL) << 8) | (buffer[0] & 0xffL));

            return (tmpLong);
        }
    }

    public RandomAccessFile getRaFile() {
		return raFile;
	}

	/**
     * Reads two byte signed short from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of signed short read from the file returned as an int.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getSignedShort(boolean bigEndian) throws IOException {
        int b3 = 0;
        byte[] buffer = new byte[2];

        raFile.readFully(buffer);

        if (bigEndian) {
            b3 = ((buffer[0] & 0xff) << 8) | (buffer[1] & 0xff);
        } else {
            b3 = ((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff);
        }

        if ((b3 & 0x8000) != 0) {
            b3 = b3 | 0xffff0000;
        }

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
        raFile.readFully(b);

        return new String(b);
    }

    /**
     * Reads four unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the integer read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final long getUInt(boolean bigEndian) throws IOException {
    	final byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xffL) << 24) | ((buffer[1] & 0xffL) << 16) | ((buffer[2] & 0xffL) << 8) |
                        (buffer[3] & 0xffL)); // Big Endian
        } else {
            return (((buffer[3] & 0xffL) << 24) | ((buffer[2] & 0xffL) << 16) | ((buffer[1] & 0xffL) << 8) |
                        (buffer[0] & 0xffL));
        }
    }

    /**
     * Converts byte data to unsigned int.
     *
     * @param   buffer  Buffer of signed bytes.
     * @param   index   Index points to location in buffer.
     *
     * @return  Integer converted from as "signed byte".
     */
    public final int getUnsignedByte(byte[] buffer, int index) {
        return (buffer[index] & 0xff);
    }

    /**
     * Reads two unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of unsigned short read from the file returned as an int.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getUnsignedShort(boolean bigEndian) throws IOException {
    	final byte[] buffer = new byte[2];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xff) << 8) | (buffer[1] & 0xff)); // Big Endian
        } else {
            return (((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff)); // Little Endian
        }
    }

    /**
     * Return true if the byte order is big endian.
     *
     * @return  true if the byte order is big endian.
     */
    public boolean isBigEndian() {
        return bigEndian;
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
     * DOCUMENT ME!
     *
     * @param   bigEndian  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public final double readDouble(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[8];
        raFile.readFully(buffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = (((buffer[0] & 0xffL) << 56) | ((buffer[1] & 0xffL) << 48) | ((buffer[2] & 0xffL) << 40) |
                           ((buffer[3] & 0xffL) << 32) | ((buffer[4] & 0xffL) << 24) | ((buffer[5] & 0xffL) << 16) |
                           ((buffer[6] & 0xffL) << 8) | (buffer[7] & 0xffL));

        } else {
            tmpLong = (((buffer[7] & 0xffL) << 56) | ((buffer[6] & 0xffL) << 48) | ((buffer[5] & 0xffL) << 40) |
                           ((buffer[4] & 0xffL) << 32) | ((buffer[3] & 0xffL) << 24) | ((buffer[2] & 0xffL) << 16) |
                           ((buffer[1] & 0xffL) << 8) | (buffer[0] & 0xffL));

        }

        return (Double.longBitsToDouble(tmpLong));
    }

    /**
     * DOCUMENT ME!
     *
     * @param   bigEndian  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public final float readFloat(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        int tmpInt;

        if (bigEndian) {
            tmpInt = (((buffer[0] & 0xff) << 24) | ((buffer[1] & 0xff) << 16) | ((buffer[2] & 0xff) << 8) |
                          (buffer[3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = (((buffer[3] & 0xff) << 24) | ((buffer[2] & 0xff) << 16) | ((buffer[1] & 0xff) << 8) |
                          (buffer[0] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   bigEndian  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public final int readInt(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xff) << 24) | ((buffer[1] & 0xff) << 16) | ((buffer[2] & 0xff) << 8) |
                        (buffer[3] & 0xff)); // Big Endian
        } else {
            return (((buffer[3] & 0xff) << 24) | ((buffer[2] & 0xff) << 16) | ((buffer[1] & 0xff) << 8) |
                        (buffer[0] & 0xff));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   bigEndian  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public final long readLong(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[8];

        raFile.readFully(buffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = (((buffer[0] & 0xffL) << 56) | ((buffer[1] & 0xffL) << 48) | ((buffer[2] & 0xffL) << 40) |
                           ((buffer[3] & 0xffL) << 32) | ((buffer[4] & 0xffL) << 24) | ((buffer[5] & 0xffL) << 16) |
                           ((buffer[6] & 0xffL) << 8) | (buffer[7] & 0xffL));

        } else {
            tmpLong = (((buffer[7] & 0xffL) << 56) | ((buffer[6] & 0xffL) << 48) | ((buffer[5] & 0xffL) << 40) |
                           ((buffer[4] & 0xffL) << 32) | ((buffer[3] & 0xffL) << 24) | ((buffer[2] & 0xffL) << 16) |
                           ((buffer[1] & 0xffL) << 8) | (buffer[0] & 0xffL));

        }

        return (tmpLong);
    }

    /**
     * DOCUMENT ME!
     *
     * @param   bigEndian  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public final short readShort(boolean bigEndian) throws IOException {
        short tempShort = 0;
        byte[] buffer = new byte[2];

        raFile.readFully(buffer);

        if (bigEndian) {
            tempShort = (short) (((buffer[0] & 0xff) << 8) | (buffer[1] & 0xff));
        } else {
            tempShort = (short) (((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff));
        }

        return tempShort;
    }

    /**
     * Reads the length of the characters from the file.
     *
     * @param   length  the length of the string
     *
     * @return  the string read from the file.
     *
     * @throws  IOException  throw IOException if I/O error happens
     */
    public final String readString(int length) throws IOException {

        if (length <= 0) {
            return null;
        }

        byte[] buffer = new byte[length];
        raFile.readFully(buffer);

        return new String(buffer);
    }

    /**
     * DOCUMENT ME!
     *
     * @param   bigEndian  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public final int readUnsignedShort(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[2];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xff) << 8) | (buffer[1] & 0xff));
        } else {
            return (((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff));
        }
    }

    /**
     * Removes the ChangeListener from the FileBase object.
     *
     * @param  l  DOCUMENT ME!
     */
    public void removeProgressChangeListener(ProgressChangeListener l) {
        listenerList.remove(ProgressChangeListener.class, l);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  bigEndian  DOCUMENT ME!
     */
    public void setBigEndian(boolean bigEndian) {
        this.bigEndian = bigEndian;
    }

    /**
     * Sets byte buffer with int.
     *
     * @param  buffer     Byte buffers where data is to be stored.
     * @param  data       Float data is broken down in bytes and stored in the byte buffer.
     * @param  i          Index into byte buffer.
     * @param  bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
     */
    public final void setBufferFloat(byte[] buffer, float data, int i, boolean bigEndian) {
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);
        setBufferInt(buffer, tmpInt, i, bigEndian);
    }

    /**
     * Sets byte buffer with int.
     *
     * @param  buffer     Byte buffers where data is to be stored.
     * @param  data       Integer data is broken down in bytes and stored in the byte buffer.
     * @param  i          Index into byte buffer.
     * @param  bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
     */
    public final void setBufferInt(byte[] buffer, int data, int i, boolean bigEndian) {

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
     * @param  buffer     Byte buffers where data is to be stored.
     * @param  data       Long data is broken down in bytes and stored in the byte buffer.
     * @param  i          Index into byte buffer.
     * @param  bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
     */
    public final void setBufferLong(byte[] buffer, long data, int i, boolean bigEndian) {

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
     * @param  buffer     Byte buffers where data is to be stored.
     * @param  data       Short data is broken down in bytes and stored in the byte buffer.
     * @param  i          Index into byte buffer.
     * @param  bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
     */
    public final void setBufferShort(byte[] buffer, short data, int i, boolean bigEndian) {

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
     * DOCUMENT ME!
     *
     * @param  dataType  DOCUMENT ME!
     */
    public void setDataType(int dataType) {
        this.dataType = dataType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  bigEndian  DOCUMENT ME!
     */
    public void setEndianess(boolean bigEndian) {
        this.bigEndian = bigEndian;
    }
  
    /**
     * Writes a double as eight bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeDouble(double data, boolean bigEndian) throws IOException {

        long tmpLong;

        tmpLong = Double.doubleToLongBits(data);
        writeLong(tmpLong, bigEndian);
    }

    /**
     * Writes a float as four bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeFloat(float data, boolean bigEndian) throws IOException {
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);
        writeInt(tmpInt, bigEndian);
    }

    /**
     * Writes an int as four bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeInt(int data, boolean bigEndian) throws IOException {
        byte[] buffer = new byte[4];

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

        raFile.write(buffer);
    }

    /**
     * Writes a long as eight bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeLong(long data, boolean bigEndian) throws IOException {
        byte[] buffer = new byte[8];

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

        raFile.write(buffer);
    }

    /**
     * Writes a short as two bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeShort(short data, boolean bigEndian) throws IOException {
        byte[] buffer = new byte[2];

        if (bigEndian) {
            buffer[0] = (byte) (data >>> 8);
            buffer[1] = (byte) (data & 0xff);
        } else {
            buffer[0] = (byte) (data & 0xff);
            buffer[1] = (byte) (data >>> 8);
        }

        raFile.write(buffer);
    }
}
