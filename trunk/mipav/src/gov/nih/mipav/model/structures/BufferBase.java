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
     * @throws  Throwable  DOCUMENT ME!
     */
    protected abstract void finalize() throws Throwable;

    /**
     * DOCUMENT ME!
     *
     * @param   position  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected abstract Number get(int position);

    /**
     * DOCUMENT ME!
     *
     * @param   position  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected abstract boolean getBoolean(int position);

    /**
     * DOCUMENT ME!
     *
     * @param   position  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected abstract byte getByte(int position);

    /**
     * DOCUMENT ME!
     *
     * @param   position  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected abstract double getDouble(int position);

    /**
     * DOCUMENT ME!
     *
     * @param   position  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected abstract float getFloat(int position);

    /**
     * DOCUMENT ME!
     *
     * @param   position  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected abstract int getInt(int position);

    /**
     * DOCUMENT ME!
     *
     * @param   position  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected abstract long getLong(int position);

    /**
     * DOCUMENT ME!
     *
     * @param   position  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected abstract short getShort(int position);

    /**
     * DOCUMENT ME!
     *
     * @param   position  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected abstract short getUByte(int position);

    /**
     * DOCUMENT ME!
     *
     * @param   position  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected abstract long getUInt(int position);

    /**
     * DOCUMENT ME!
     *
     * @param   position  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected abstract int getUShort(int position);

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected abstract int length();

    /**
     * DOCUMENT ME!
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */
    protected abstract void set(int position, Number value);

    /**
     * DOCUMENT ME!
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */
    protected abstract void setBoolean(int position, boolean value);

    /**
     * DOCUMENT ME!
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */
    protected abstract void setByte(int position, byte value);

    /**
     * DOCUMENT ME!
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */
    protected abstract void setDouble(int position, double value);

    /**
     * DOCUMENT ME!
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */
    protected abstract void setFloat(int position, float value);

    /**
     * DOCUMENT ME!
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */
    protected abstract void setInt(int position, int value);

    /**
     * DOCUMENT ME!
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */
    protected abstract void setLong(int position, long value);

    /**
     * DOCUMENT ME!
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */
    protected abstract void setShort(int position, short value);

    /**
     * DOCUMENT ME!
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */
    protected abstract void setUByte(int position, short value);

    /**
     * DOCUMENT ME!
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */
    protected abstract void setUInt(int position, long value);

    /**
     * DOCUMENT ME!
     *
     * @param  position  DOCUMENT ME!
     * @param  value     DOCUMENT ME!
     */
    protected abstract void setUShort(int position, int value);
}
