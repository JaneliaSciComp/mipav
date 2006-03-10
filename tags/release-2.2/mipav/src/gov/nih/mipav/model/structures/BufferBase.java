package gov.nih.mipav.model.structures;

import java.io.*;

/** 
*   This is an abstract buffer class that indicates the required methods of
*   the sub-classes. This is a base class of a generic data "black box" - it
*   supports all the native java data types.
*
*		@version 0.1 Aug 1, 1997
*		@author Matthew J. McAuliffe, Ph.D.
*       @see BufferBoolean
*       @see BufferByte 
*       @see BufferShort
*       @see BufferInt
*       @see BufferLong
*       @see BufferFloat
*       @see BufferDouble
*
*/

public abstract class BufferBase implements Serializable {
    
    
    /**
    * Clean up memory
    */
    protected abstract void finalize() throws Throwable;
    
    protected abstract Number   get         (int position);
    protected abstract boolean  getBoolean  (int position);
    protected abstract byte     getByte     (int position);
    protected abstract short    getUByte    (int position);
    protected abstract short    getShort    (int position);
    protected abstract int      getUShort   (int position);
    protected abstract int      getInt      (int position);
    protected abstract long     getUInt     (int position);
    protected abstract long     getLong     (int position);
    protected abstract float    getFloat    (int position);
    protected abstract double   getDouble   (int position);

    protected abstract void     set         (int position, Number   value);
    protected abstract void     setBoolean  (int position, boolean  value);
    protected abstract void     setByte     (int position, byte     value);
    protected abstract void     setUByte    (int position, short    value);
    protected abstract void     setShort    (int position, short    value);
    protected abstract void     setUShort   (int position, int      value);
    protected abstract void     setInt      (int position, int      value);
    protected abstract void     setUInt     (int position, long     value);
    protected abstract void     setLong     (int position, long     value);
    protected abstract void     setFloat    (int position, float    value);
    protected abstract void     setDouble   (int position, double   value);
    
    protected abstract int      length();
}