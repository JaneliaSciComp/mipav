package gov.nih.mipav.model.structures;


/**
 * This class extends the abstract BufferBase class. It allocates the space for an integer buffer on construction. This
 * class also defines the accessors to the data as required by the abstract base class.
 *
 * @see  BufferBase
 */

public class BufferUInt extends BufferBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1794760958290773461L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** 1D Data array of unsigned integers. */
    protected int[] dataArray;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     */
    public BufferUInt() {
        dataArray = null;
    }

    /**
     * Constructor that allocates memory.
     *
     * @param  size  the amount of data to allocate
     */
    public BufferUInt(int size) {
        dataArray = new int[size];
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

    /** Return int.class */
    protected final Class<?> getType() { return int.class; }

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
        return (new Integer((int) dataArray[position]));
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
     * @return  the int value as a byte
     */
    protected final byte getByte(int position) {
        return (byte) (dataArray[position] & 0xff);
    }

    /**
     * Gets the data in double format.
     *
     * @param   position  position in dataArray
     *
     * @return  int value as a double
     */
    protected final double getDouble(int position) {
        
        return (double) (dataArray[position] & 0xffffffffL);
    }

    /**
     * Gets the data in float format.
     *
     * @param   position  position in dataArray
     *
     * @return  int value as a float
     */
    protected final float getFloat(int position) {
        return (float) (dataArray[position] & 0xffffffffL);
    }

    /**
     * Gets the data in int format.
     *
     * @param   position  position in dataArray
     *
     * @return  int value as an int
     */
    protected final int getInt(int position) {
        return (dataArray[position]);
    }

    /**
     * Gets the data in long format.
     *
     * @param   position  position in dataArray
     *
     * @return  int value as a long
     */
    protected final long getLong(int position) {
        return (long) (dataArray[position] & 0xffffffffL);
    }

    /**
     * Gets the data in short format.
     *
     * @param   position  position in dataArray
     *
     * @return  int value as a short
     */
    protected final short getShort(int position) {
        return ((short) (dataArray[position] & 0xffff));
    }

    /**
     * Gets the data in short format.
     *
     * @param   position  position in dataArray
     *
     * @return  the int value as a short (unsigned byte)
     */
    protected final short getUByte(int position) {
        return ((short) (dataArray[position] & 0xff));
    }

    /**
     * Gets the data in long format.
     *
     * @param   position  position in dataArray
     *
     * @return  int value as a long
     */
    protected final long getUInt(int position) {
        return ((long) (dataArray[position] & 0xffffffffL));
    }

    /**
     * Gets the data in unsigned short format.
     *
     * @param   position  position in dataArray
     *
     * @return  int value as an int
     */
    protected final int getUShort(int position) {
        return (dataArray[position] & 0xffff);
    }


    /**
     * Set methods.
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */

    /**
     * Sets the value in unsigned int format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void set(int position, Number value) {
        dataArray[position] = value.intValue();
    }

    /**
     * Sets the value in unsigned int format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void set(int position, int value) {
        dataArray[position] = value;
    }


    /**
     * Sets the data in float format. One if value is true, zero if value is false
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
     * Sets the byte value in unsigned short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setByte(int position, byte value) {
        dataArray[position] = (int) value;
    }

    /**
     * Sets the double value in unsigned short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setDouble(int position, double value) {
        // For value > Integer.MAX_VALUE == 2,147,483,647, 
        // dataArray[position] = (int) (value + 0.5f);
        // puts 2,147,483,647 in dataArray[position]
        // The maximum unsigned value = 4,294,967,295 is the same as the 2's complement represenation of -1
        // 4,294,967,294 is the same as the 2's complement representation of -2
        // so subtract 4,294,967,296 to go from unsigned to 2's complement.
        if (value < 0) {
            dataArray[position] = (int) (value - 0.5);
        } else if ((value > Integer.MAX_VALUE) && (value <= 4.294967295E9)) {
            dataArray[position] = (int) (value - 4.2949672965E9);
        } else {
            dataArray[position] = (int) (value + 0.5);
        }


        // dataArray[position] = (int)(MipavMath.round(value));
    }

    /**
     * Sets the float value in unsigned short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setFloat(int position, float value) {
        // For value > Integer.MAX_VALUE == 2,147,483,647, 
        // dataArray[position] = (int) (value + 0.5f);
        // puts 2,147,483,647 in dataArray[position]
        // The maximum unsigned value = 4,294,967,295 is the same as the 2's complement represenation of -1
        // 4,294,967,294 is the same as the 2's complement representation of -2
        // so subtract 4,294,967,296 to go from unsigned to 2's complement.
        if (value < 0) {
            dataArray[position] = (int) (value - 0.5f);
        } else if ((value > Integer.MAX_VALUE) && (value <= 4.294967295E9)) {
            dataArray[position] = (int) (value - 4.2949672965E9);
        } else {
            dataArray[position] = (int) (value + 0.5f);
        }

        // dataArray[position] = (int)(MipavMath.round(value));
    }

    /**
     * Sets the int value in unsigned short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setInt(int position, int value) {
        dataArray[position] = value;
    }

    /**
     * Sets the long value in unsigned short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setLong(int position, long value) {
        dataArray[position] = (int) value;
    }

    /**
     * Sets the short value in unsigned short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setShort(int position, short value) {
        dataArray[position] = (int) value;
    }

    /**
     * Sets the unsigned byte value in unsigned short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setUByte(int position, short value) {
        dataArray[position] = (int) value;
    }

    /**
     * Sets the int value in unsigned short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setUInt(int position, long value) {
        dataArray[position] = (int) value;
    }

    /**
     * Sets the short (unsigned) value in unsigned short format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setUShort(int position, int value) {
        dataArray[position] = value;
    }

}
