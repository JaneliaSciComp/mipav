package gov.nih.mipav.model.structures;


import java.util.*;


/**
 * This class extends the abstract BufferBase class. It allocates the space for a boolean buffer on construction.
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      BufferBase
 */

public class BufferBoolean extends BufferBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 588861892470975804L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** 1D Data array of boolean stored in the BitSet object. */
    protected BitSet dataArray;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     */
    public BufferBoolean() {
        dataArray = null;
    }

    /**
     * Constructor that allocates memory.
     *
     * @param  size  the amount of data to allocate
     */
    public BufferBoolean(int size) {
        dataArray = new BitSet(size);
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

    /**
     * Returns the length of the data array.
     *
     * @return  length of the array
     */
    public int length() {
        return dataArray.size();
    }


    /** Return byte.class, type returned by get() */
    protected final Class<?> getType() { return byte.class; }

    /**
     * Gets the data in byte format.
     *
     * @param   position  position in dataArray
     *
     * @return  Number object of the data
     */
    protected final Number get(int position) {

        if (dataArray.get(position)) {
            return (new Byte((byte) 1));
        } else {
            return (new Byte((byte) 0));
        }
    }

    /**
     * Gets the data from the object.
     *
     * @param   position  position in dataArray to get
     *
     * @return  the value returned as a boolean
     */
    protected final boolean getBoolean(int position) {
        return (dataArray.get(position));
    }

    /**
     * Gets the data in byte format.
     *
     * @param   position  position in dataArray
     *
     * @return  if true return 1 as a byte else return 0
     */
    protected final byte getByte(int position) {

        if (dataArray.get(position)) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * Gets the acquire data in double format.
     *
     * @param   position  position in dataArray
     *
     * @return  if true return 1 as a double else return 0
     */
    protected final double getDouble(int position) {

        if (dataArray.get(position)) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * Gets the data in float format.
     *
     * @param   position  position in dataArray
     *
     * @return  if true return 1 as a float else return 0
     */
    protected final float getFloat(int position) {

        if (dataArray.get(position)) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * Gets the data in int format.
     *
     * @param   position  position in dataArray
     *
     * @return  if true return 1 as a int else return 0
     */
    protected final int getInt(int position) {

        if (dataArray.get(position)) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * Gets the data in long format.
     *
     * @param   position  position in dataArray
     *
     * @return  if true return 1 as a long else return 0
     */
    protected final long getLong(int position) {

        if (dataArray.get(position)) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * Gets the data in short format.
     *
     * @param   position  position in dataArray
     *
     * @return  if true return 1 as a short else return 0
     */
    protected final short getShort(int position) {

        if (dataArray.get(position)) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * Gets the data in byte format.
     *
     * @param   position  position in dataArray
     *
     * @return  if true return 1 as a short else return 0
     */
    protected final short getUByte(int position) {

        if (dataArray.get(position)) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * Gets the data in int format.
     *
     * @param   position  position in dataArray
     *
     * @return  if true return 1 as a int else return 0
     */
    protected final long getUInt(int position) {

        if (dataArray.get(position)) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * Gets the data in unsigned short format.
     *
     * @param   position  position in dataArray
     *
     * @return  if true return 1 as a int else return 0
     */
    protected final int getUShort(int position) {

        if (dataArray.get(position)) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * Set methods.
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */

    /**
     * Sets the value in boolean format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void set(int position, Number value) {

        if (value.byteValue() != 0) {
            dataArray.set(position);
        } else {
            dataArray.set(position);
        }
    }

    /**
     * Sets the value in byte format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void set(int position, boolean value) {

        if (value) {
            dataArray.set(position);
        } else {
            dataArray.clear(position);
        }
    }

    /**
     * Sets the value in boolean format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setBoolean(int position, boolean value) {

        if (value) {
            dataArray.set(position);
        } else {
            dataArray.clear(position);
        }
    }

    /**
     * Sets the value in boolean format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setByte(int position, byte value) {

        if (value != 0) {
            dataArray.set(position);
        } else {
            dataArray.clear(position);
        }
    }

    /**
     * Sets the value in boolean format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setDouble(int position, double value) {

        if (value != 0) {
            dataArray.set(position);
        } else {
            dataArray.clear(position);
        }
    }

    /**
     * Sets the value in boolean format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setFloat(int position, float value) {

        if (value != 0) {
            dataArray.set(position);
        } else {
            dataArray.clear(position);
        }
    }

    /**
     * Sets the value in boolean format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setInt(int position, int value) {

        if (value != 0) {
            dataArray.set(position);
        } else {
            dataArray.clear(position);
        }
    }

    /**
     * Sets the value in boolean format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setLong(int position, long value) {

        if (value != 0) {
            dataArray.set(position);
        } else {
            dataArray.clear(position);
        }
    }

    /**
     * Sets the value in boolean format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setShort(int position, short value) {

        if (value != 0) {
            dataArray.set(position);
        } else {
            dataArray.clear(position);
        }
    }

    /**
     * Sets the value in unsigned byte format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setUByte(int position, short value) {

        if (value != 0) {
            dataArray.set(position);
        } else {
            dataArray.clear(position);
        }
    }

    /**
     * Sets the value in boolean format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setUInt(int position, long value) {

        if (value != 0) {
            dataArray.set(position);
        } else {
            dataArray.clear(position);
        }
    }

    /**
     * Sets the value in byte format.
     *
     * @param  position  position in dataArray
     * @param  value     the new data value to be placed in the data array
     */
    protected final void setUShort(int position, int value) {

        if (value != 0) {
            dataArray.set(position);
        } else {
            dataArray.clear(position);
        }
    }

}
