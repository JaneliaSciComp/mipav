package gov.nih.mipav.model.structures;


/**
 * This class extends the abstract BufferBase class. It allocates the space for a float buffer on construction. This
 * class also defines the accessors to the data as required by the abstract base class.
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      BufferBase
 */

public class BufferFloat extends BufferBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3011094039945269732L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** 1D Data array of floats. */
    protected float[] dataArray;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     */
    public BufferFloat() {
        dataArray = null;
    }

    /**
     * Constructor that allocates memory.
     *
     * @param  size  the amount of data to allocate
     */
    public BufferFloat(int size) {
        dataArray = new float[size];
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

    /** Return float.class */
    protected final Class<?> getType() { return float.class; }

    /**
     * Returns the length of the data array.
     *
     * @return  length of the array
     */
    public int length() {
        return dataArray.length;
    }

    /**
     * Gets the data in Number format.
     *
     * @param   position  position in dataArray
     *
     * @return  Number object of the data
     */
    protected final Number get(int position) {
        return (new Float((float) dataArray[position]));
    }

    /**
     * Gets the data from the object.
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
     * @return  the byte value
     */
    protected final byte getByte(int position) {
        return (byte) (dataArray[position]);
    }

    /**
     * Gets the data in double format.
     *
     * @param   position  position in dataArray
     *
     * @return  the float value as a double
     */
    protected final double getDouble(int position) {
        return (double) (dataArray[position]);
    }

    /**
     * Gets the data in float format.
     *
     * @param   position  position in dataArray
     *
     * @return  the float value as a float
     */
    protected final float getFloat(int position) {
        return (dataArray[position]);
    }

    /**
     * Gets the data in int format.
     *
     * @param   position  position in dataArray
     *
     * @return  the float value as an int
     */
    protected final int getInt(int position) {
        return (int) (dataArray[position]);
    }


    /**
     * Gets the data in long format.
     *
     * @param   position  position in dataArray
     *
     * @return  the float value as a long
     */
    protected final long getLong(int position) {
        return (long) (dataArray[position]);
    }

    /**
     * Gets the data in short format.
     *
     * @param   position  position in dataArray
     *
     * @return  the float value as a short
     */
    protected final short getShort(int position) {
        return (short) (dataArray[position]);
    }

    /**
     * Gets the data in unsigned byte format.
     *
     * @param   position  position in dataArray
     *
     * @return  the value as a short (unsigned byte)
     */
    protected final short getUByte(int position) {
        return ((short) ((int) dataArray[position] & 0xff));
    }

    /**
     * Gets the data in unsigned short format.
     *
     * @param   position  position in dataArray
     *
     * @return  the float value as int (unsigned short)
     */
    protected final long getUInt(int position) {
        return (long) ((long) dataArray[position] & 0xffffffffL);
    }

    /**
     * Gets the data in unsigned short format.
     *
     * @param   position  position in dataArray
     *
     * @return  the float value as int (unsigned short)
     */
    protected final int getUShort(int position) {
        return (int) ((int) dataArray[position] & 0xffff);
    }


    /**
     * Sets the data in float format.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected final void set(int position, Number value) {
        dataArray[position] = value.floatValue();
    }

    /**
     * Sets the data in float format.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected final void set(int position, float value) {
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
     * Sets the data in float format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setByte(int position, byte value) {
        dataArray[position] = (float) value;
    }

    /**
     * Sets the double data in float format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setDouble(int position, double value) {
        dataArray[position] = (float) value;
    }

    /**
     * Sets the float data in float format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setFloat(int position, float value) {
        dataArray[position] = value;
    }

    /**
     * Sets the int data in float format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setInt(int position, int value) {
        dataArray[position] = (float) value;
    }

    /**
     * Sets the long data in float format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setLong(int position, long value) {
        dataArray[position] = (float) value;
    }

    /**
     * Sets the to set short data in float format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setShort(int position, short value) {
        dataArray[position] = (float) value;
    }

    /**
     * Sets the data in float format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setUByte(int position, short value) {
        dataArray[position] = (float) value;
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
     * Sets the unsigned data in float format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setUShort(int position, int value) {
        dataArray[position] = (float) value;
    }

}
