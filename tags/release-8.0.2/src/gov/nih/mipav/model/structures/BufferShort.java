package gov.nih.mipav.model.structures;


/**
 * This class extends the abstract BufferBase class. It allocates the space for a short buffer on construction. This
 * class also defines the accessors to the data as required by the abstract base class.
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      BufferBase
 */


public class BufferShort extends BufferBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 784194545397056669L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** 1D Data array of shorts. */
    protected short[] dataArray;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     */
    public BufferShort() {
        dataArray = null;
    }

    /**
     * Constructor that allocates memory.
     *
     * @param  size  the amount of data to allocate
     */
    public BufferShort(int size) {
        dataArray = new short[size];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean up memory.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    public void finalize() throws Throwable {
        dataArray = null;
    }

    /** Return short.class */
    protected final Class<?> getType() { return short.class; }

    /**
     * Returns the length of the data array.
     *
     * @return  length of the array
     */
    public int length() {
        return dataArray.length;
    }


    /**
     * Gets the data in as a Number object.
     *
     * @param   position  position in dataArray
     *
     * @return  Number object of the data
     */
    protected final Number get(int position) {
        return (new Short((short) dataArray[position]));
    }

    /**
     * Gets the data from the object as a boolean.
     *
     * @param   position  position in dataArray to get
     *
     * @return  true if data at position is not equal to zero else its false
     */
    protected final boolean getBoolean(int position) {

        if (dataArray[position] != 0) {
            return true;
        } else {
            return false;
        }
    }


    /**
     * Gets the data in byte format.
     *
     * @param   position  position in dataArray
     *
     * @return  the short value as a byte
     */
    protected final byte getByte(int position) {
        return (byte) (dataArray[position]);
    }

    /**
     * Gets the data in double format.
     *
     * @param   position  position in dataArray
     *
     * @return  short value as an double
     */
    protected final double getDouble(int position) {
        return (double) (dataArray[position]);
    }

    /**
     * Gets the data in float format.
     *
     * @param   position  position in dataArray
     *
     * @return  short value as an float
     */
    protected final float getFloat(int position) {
        return (float) (dataArray[position]);
    }


    /**
     * Gets the data in int format.
     *
     * @param   position  position in dataArray
     *
     * @return  short value as an int
     */
    protected final int getInt(int position) {
        return (int) (dataArray[position]);
    }

    /**
     * Gets the data in long format.
     *
     * @param   position  position in dataArray
     *
     * @return  short value as a long
     */
    protected final long getLong(int position) {
        return (long) (dataArray[position]);
    }

    /**
     * Gets the data in short format.
     *
     * @param   position  position in dataArray
     *
     * @return  short value as a short
     */
    protected final short getShort(int position) {
        return (dataArray[position]);
    }

    /**
     * Gets the data in short format.
     *
     * @param   position  position in dataArray
     *
     * @return  the short value as a short (unsigned byte)
     */
    protected final short getUByte(int position) {
        return ((short) ((int) dataArray[position] & 0xff));
    }

    /**
     * Gets the data in long format.
     *
     * @param   position  position in dataArray
     *
     * @return  short value as a long
     */
    protected final long getUInt(int position) {
        return ((long) (dataArray[position] & 0xffff));
    }

    /**
     * Gets the data in unsigned short format.
     *
     * @param   position  position in dataArray
     *
     * @return  short value as an int
     */
    protected final int getUShort(int position) {
        return (int) (dataArray[position] & 0xffff);
    }


    /**
     * Set methods.
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */

    /**
     * Sets the Number object in short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void set(int position, Number value) {
        dataArray[position] = value.shortValue();
    }

    /**
     * Sets the value in short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void set(int position, short value) {
        dataArray[position] = value;
    }


    /**
     * Sets the value in short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setBoolean(int position, boolean value) {

        if (value) {
            dataArray[position] = 1;
        } else {
            dataArray[position] = 0;
        }
    }

    /**
     * Sets the value in short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setByte(int position, byte value) {
        dataArray[position] = (short) value;
    }

    /**
     * Sets the value in short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setDouble(int position, double value) {

        if (value < 0) {
            dataArray[position] = (short) (value - 0.5f);
        } else {
            dataArray[position] = (short) (value + 0.5f);
        }

        // dataArray[position] = (short)(MipavMath.round(value));
    }

    /**
     * Sets the value in short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setFloat(int position, float value) {

        if (value < 0) {
            dataArray[position] = (short) (value - 0.5f);
        } else {
            dataArray[position] = (short) (value + 0.5f);
        }

        // dataArray[position] = (short)(MipavMath.round(value));
    }

    /**
     * Sets the value in short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setInt(int position, int value) {
        dataArray[position] = (short) value;
    }

    /**
     * Sets the value in short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setLong(int position, long value) {
        dataArray[position] = (short) value;
    }

    /**
     * Sets the value in short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setShort(int position, short value) {
        dataArray[position] = value;
    }

    /**
     * Sets the value in short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setUByte(int position, short value) {
        dataArray[position] = value;
    }

    /**
     * Sets the short (unsigned) value in unsigned short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setUInt(int position, long value) {
        dataArray[position] = (short) value;
    }

    /**
     * Sets the value in short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setUShort(int position, int value) {
        dataArray[position] = (short) value;
    }

}
