package gov.nih.mipav.model.structures;


import java.io.*;


/**
 * This is an abstract buffer class that indicates the required methods of the sub-classes. This is a base class of a
 * generic data "black box" - it supports all the native java data types.
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      BufferBoolean
 * @see      BufferByte
 * @see      BufferShort
 * @see      BufferInt
 * @see      BufferLong
 * @see      BufferFloat
 * @see      BufferDouble
 */

public abstract class BufferBase implements Serializable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5379757071350175311L;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean up memory.
     *
     * @throws  Throwable  indicates error in while finalizing.
     */
    protected abstract void finalize() throws Throwable;

    /**
     * Get the primitive type of the buffer.
     *
     * @return one of the primitive class objects, like float.class (== Float.TYPE)
     */
    protected abstract Class<?> getType();

    /**
     * Gets a Number object at position in the data array.
     *
     * @param   position  index into the data array.
     *
     * @return  Number object of the data
     */
    protected abstract Number get(int position);

    /**
     * Gets a boolean value at position in the data array.
     *
     * @param   position  index into the data array.
     *
     * @return  boolean value of the data
     */
    protected abstract boolean getBoolean(int position);

    /**
     * Gets a byte value at position in the data array.
     *
     * @param   position  index into the data array.
     *
     * @return  byte value of the data
     */
    protected abstract byte getByte(int position);

    /**
     * Gets a double value at position in the data array.
     *
     * @param   position  index into the data array.
     *
     * @return  double value of the data
     */
    protected abstract double getDouble(int position);

    /**
     * Gets a float value at position in the data array.
     *
     * @param   position  index into the data array.
     *
     * @return  float value of the data
     */
    protected abstract float getFloat(int position);

    /**
     * Gets a int value at position in the data array.
     *
     * @param   position  index into the data array.
     *
     * @return  int value of the data
     */
    protected abstract int getInt(int position);

    /**
     * Gets a long value at position in the data array.
     *
     * @param   position  index into the data array.
     *
     * @return  long value of the data
     */
    protected abstract long getLong(int position);

    /**
     * Gets a short value at position in the data array.
     *
     * @param   position  index into the data array.
     *
     * @return  short value of the data
     */
    protected abstract short getShort(int position);

    /**
     * Gets a short value (unsigned byte[0:255]) at position in the data array.
     *
     * @param   position  index into the data array.
     *
     * @return  short(unsigned byte[0:255]) value of the data
     */
    protected abstract short getUByte(int position);

    /**
     * Gets a long value (unsigned int) at position in the data array.
     *
     * @param   position  index into the data array.
     *
     * @return  long(unsigned int) value of the data
     */
    protected abstract long getUInt(int position);

    /**
     * Gets a int value (unsigned short) at position in the data array.
     *
     * @param   position  index into the data array.
     *
     * @return  int(unsigned short) value of the data
     */
    protected abstract int getUShort(int position);

    /**
     * Gets the length of the data array.
     *
     * @return  the length of the data array
     */
    protected abstract int length();

    /**
     * Sets the data array at the specified index to the specified value.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected abstract void set(int position, Number value);

    /**
     * Sets the data array at the specified index to the specified value.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected abstract void setBoolean(int position, boolean value);

    /**
     * Sets the data array at the specified index to the specified value.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected abstract void setByte(int position, byte value);

    /**
     * Sets the data array at the specified index to the specified value.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected abstract void setDouble(int position, double value);

    /**
     * Sets the data array at the specified index to the specified value.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected abstract void setFloat(int position, float value);

    /**
     * Sets the data array at the specified index to the specified value.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected abstract void setInt(int position, int value);

    /**
     * Sets the data array at the specified index to the specified value.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected abstract void setLong(int position, long value);

    /**
     * Sets the data array at the specified index to the specified value.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected abstract void setShort(int position, short value);

    /**
     * Sets the data array at the specified index to the specified value.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected abstract void setUByte(int position, short value);

    /**
     * Sets the data array at the specified index to the specified value.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected abstract void setUInt(int position, long value);

    /**
     * Sets the data array at the specified index to the specified value.
     *
     * @param  position  position in data array
     * @param  value     the new data value to be placed in the data array
     */
    protected abstract void setUShort(int position, int value);
}
