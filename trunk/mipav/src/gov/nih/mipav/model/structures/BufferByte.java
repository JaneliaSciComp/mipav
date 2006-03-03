package gov.nih.mipav.model.structures;

import gov.nih.mipav.*;

/**
*    This class extends the abstract BufferBase class. It allocates the
*    space for a byte buffer on construction. This class also defines
*    the accessors to the data as required by the abstract base class.
*
*		@version 0.1 Aug 1, 1997
*		@author Matthew J. McAuliffe, Ph.D.
*       @see BufferBase
*
*/


public class BufferByte extends BufferBase {

    /** 1D Data array of bytes. */
	protected byte[]      dataArray;

	/**
	* Default constructor
	*/
	public BufferByte(){
		dataArray = null;
	}

    /**
    *   Constructor that allocates memory
    *   @param size      the amount of data to allocate
    */
    public BufferByte(int size) {
        dataArray = new byte[size];
    }


    /**
    * Clean up memory
    */
    public void finalize() throws Throwable  {
        dataArray = null;
    }

    /**
    *   Returns the length of the data array
    *   @return  length of the array
    */
    public int length() {
        return dataArray.length;
    }

    /**
    *  Gets the data in byte format
    *  @param position    position in dataArray
    *  @return            Number object of the data
    */
    protected final Number get(int position){
         return (new Byte((byte)dataArray[position]));
    }

    /**
    *  Gets the data in byte format
    *  @param position    position in dataArray
    *  @return            Number object of the data
    */
    //protected final byte get(int position){
    //     return (dataArray[position]);
    //}



    /**
    *  Gets the data from the object
    *  @param position    position in dataArray to get
    *  @return true if data at position is not equal to zero else its false
    */
    protected final boolean getBoolean(int position){

         if (dataArray[position] != 0){
            return true;
         }
         else {
            return false;
         }
    }

    /**
    *  Gets the data in byte format
    *  @param position    position in dataArray
    *  @return the byte value
    */
    protected final byte getByte(int position){
         return (dataArray[position]);
    }

    /**
    *  Gets the data in unsigned byte format
    *  @param position    position in dataArray
    *  @return the value returned as a short (unsigned byte)
    */
    protected final short getUByte(int position){
         return ( (short)(dataArray[position] & 0xff));
    }

    /**
    *  Gets the data in short format
    *  @param position    position in dataArray
    *  @return byte value as a short
    */
    protected final short getShort(int position){
         return (short)(dataArray[position]);
    }

    /**
    *  Gets the data in unsigned short format
    *  @param position    position in dataArray
    *  @return byte value as an int
    */
    protected final int getUShort(int position){
         return (int)(dataArray[position] & 0xff);
    }

    /**
    *  Gets the data in int format
    *  @param position    position in dataArray
    *  @return byte value as a int
    */
    protected final int getInt(int position){
         return (int)(dataArray[position]);
    }

    /**
    *  Gets the data in long format
    *  @param position    position in dataArray
    *  @return byte value as a long
    */
    protected final long getUInt(int position){
         return ((long)(dataArray[position] & 0xff));
    }

    /**
    *  Gets the data in long format
    *  @param position    position in dataArray
    *  @return byte value as an long
    */
    protected final long getLong(int position){
         return (long)(dataArray[position]);
    }

    /**
    *  Gets the data in float format
    *  @param position    position in dataArray
    *  @return byte value as a float
    */
    protected final float getFloat(int position){
         return (float)(dataArray[position]);
    }

    /**
    *  Gets the data in double format
    *  @param position    position in dataArray
    *  @return byte value as an double
    */
    protected final double getDouble(int position){
         return (double)(dataArray[position]);
    }

/************************  Set methods *************************************/


    /**
    *  Sets the value in byte format
    *  @param position    position in dataArray
    *  @param value       the new data value to be placed in the data array
    */
    protected final void set(int position, Number value){
         dataArray[position] = value.byteValue();
    }

    /**
    *  Sets the value in byte format
    *  @param position    position in dataArray
    *  @param value       the new data value to be placed in the data array
    */
    protected final void set(int position, byte value){
         dataArray[position] = value;
    }


    /**
    *  Sets the data in float format. One if value is true, zero if value is false
    *  @param position    position in dataArray
    *  @param value       the new data value to be placed in the data array
    */
    protected final void setBoolean(int position, boolean value){
         if (value){
            dataArray[position] = 1;
         }
         else {
            dataArray[position] = 0;
         }
    }

    /**
    *  sSets the value in byte format
    *  @param position    position in dataArray
    *  @param value       the new data value to be placed in the data array
    */
    protected final void setByte(int position, byte value){
         dataArray[position] = value;
    }

    /**
    *  Sets the value in unsigned byte format
    *  @param position    position in dataArray
    *  @param value       the new data value to be placed in the data array
    */
    protected final void setUByte(int position, short value){
         dataArray[position] = (byte)value;
    }

    /**
    *  Sets the value in byte format
    *  @param position    position in dataArray
    *  @param value       the new data value to be placed in the data array
    */
    protected final void setShort(int position, short value){
         dataArray[position] = (byte)value;
    }

    /**
    *  Sets the value in byte format
    *  @param position    position in dataArray
    *  @param value       the new data value to be placed in the data array
    */
    protected final void setUShort(int position, int value){
         dataArray[position] = (byte)value;
    }

    /**
    *  Sets the value in byte format
    *  @param position    position in dataArray
    *  @param value       the new data value to be placed in the data array
    */
    protected final void setInt(int position, int value){
         dataArray[position] = (byte)value;
    }

    /**
    *  Sets the int value in unsigned short format
    *  @param position    position in dataArray
    *  @param value       the new data value to be placed in the data array
    */
    protected final void setUInt(int position, long value){
         dataArray[position] = (byte)value;
    }

    /**
    *  Sets the value in byte format
    *  @param position    position in dataArray
    *  @param value       the new data value to be placed in the data array
    */
    protected final void setLong(int position, long value){
         dataArray[position] = (byte)value;
    }

    /**
    *  Sets the value in byte format
    *  @param position    position in dataArray
    *  @param value       the new data value to be placed in the data array
    */
    protected final void setFloat(int position, float value){
        if (value < 0) {
             dataArray[position] = (byte)(value - 0.5f);
        }
        else
            dataArray[position] = (byte)(value + 0.5f);

       //  dataArray[position] = (byte)(MipavMath.round(value));
    }

    /**
    *  Sets the value in byte format
    *  @param position    position in dataArray
    *  @param value       the new data value to be placed in the data array
    */
    protected final void setDouble(int position, double value){

        if (value < 0) {
             dataArray[position] = (byte)(value - 0.5f);
        }
        else
            dataArray[position] = (byte)(value + 0.5f);

       //  dataArray[position] = (byte)(MipavMath.round(value));
    }
}
