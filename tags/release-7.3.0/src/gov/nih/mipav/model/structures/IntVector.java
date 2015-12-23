package gov.nih.mipav.model.structures;


/**
 * Integer Vector is very similar to the Vector class except that it is designed specifically to store integers. The
 * Vector class could have been used but because this class is specifically used to store integers it is significantly
 * faster. It was primarily used for the Watershed algorithm but is general in nature. Remember to choose your initial
 * capacity and chunk size wisely to reduce the time spent reallocating the int vector buffer.
 *
 * @version  0.1 March 20, 1998
 * @author   Matthew McAuliffe. Ph.D.
 */
public class IntVector {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The initial number of elements to have space for in the vector. */
    private int capacity = 50;

    /**
     * The number of elements to add to the total number allocated when more space is needed to store the data in the
     * vector.
     */
    private int chunk = 50;

    /** The number of valid data elements added to the vector. */
    private int currentLength = 0;

    /** The index of the first valid data element in the vector. */
    private int firstPtr = 0;

    /** The vector data. */
    private int[] intBuffer;

    /** The temporary array used when increasing the storage space of the vector. */
    private int[] intTemp;

    /** The index of the last valid data element in the vector. */
    private int ptr = -1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create an list or stack of ints.
     */
    public IntVector() {
        intBuffer = new int[capacity];
    }

    /**
     * Create an list or stack of ints.
     *
     * @param  capacity  initial capacity of the array
     */
    public IntVector(int capacity) {
        intBuffer = new int[capacity];
        this.capacity = capacity;
    }

    /**
     * Create an list or stack of ints.
     *
     * @param  capacity  initial capacity of the array
     * @param  chunk     the size of memory to add when the array needs expanding
     */
    public IntVector(int capacity, int chunk) {
        this.capacity = capacity;
        intBuffer = new int[capacity];
        this.chunk = chunk;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Pushes a new value onto the array. Checks to make sure array has the capacity to store the new value. If not,
     * this allocates the more memory by the size specified in the variable chunk.
     *
     * @param  x  value to be pushed onto the array
     */
    public final void addElement(int x) {
        int i, j;
        currentLength++;

        if (ptr == (intBuffer.length - 1)) {
            ptr = 0;
        } else {
            ptr++;
        }

        if (currentLength >= intBuffer.length) {
            intTemp = new int[intBuffer.length + chunk];

            for (i = 0, j = firstPtr; i < intBuffer.length; i++) {
                intTemp[i] = intBuffer[j];

                if (j == (intBuffer.length - 1)) {
                    j = 0;
                } else {
                    j++;
                }
            }

            firstPtr = 0;
            ptr = intBuffer.length;
            intBuffer = intTemp;
            System.gc();
        }

        intBuffer[ptr] = x;
    }

    /**
     * Clones the integer vector.
     *
     * @return  new copy of the vector
     */
    public IntVector copy() {
        int i, j;

        IntVector tempV = new IntVector(intBuffer.length);

        for (i = 0, j = firstPtr; i < currentLength; i++) {
            tempV.addElement(intBuffer[j]);

            if (j == (intBuffer.length - 1)) {
                j = 0;
            } else {
                j++;
            }
        }

        return tempV;
    }

    /**
     * Copies this array into the array passed in as an argument.
     *
     * @param   pixelsVector  array to hold values from this array
     *
     * @return  also returns the array
     */
    public IntVector copy(IntVector pixelsVector) {
        int i, j;

        for (i = 0, j = firstPtr; i < currentLength; i++) {
            pixelsVector.addElement(intBuffer[j]);

            if (j == (intBuffer.length - 1)) {
                j = 0;
            } else {
                j++;
            }
        }

        return pixelsVector;
    }

    /**
     * Cleans the memory used by the int vector.
     */
    public void finalize() {
        intBuffer = null;
        intTemp = null;
    }

    /**
     * Gets the first element of array without removing it.
     *
     * @return  first element of array
     */
    public final int firstElement() {

        if (currentLength > 0) {
            return intBuffer[firstPtr];
        } else {
            return -1;
        }
    }

    /**
     * Gets a value from the array.
     *
     * @param   index  index of value to be returned
     *
     * @return  element at the specified index. If index is out of range then Integer.MIN_VALUE is returned
     */
    public final int getElementAt(int index) {

        if ((index < 0) || (index >= intBuffer.length)) {
            return Integer.MIN_VALUE;
        }

        index += firstPtr;

        if (index >= intBuffer.length) {
            index -= intBuffer.length;
        }

        return (intBuffer[index]);

    }

    /**
     * Returns flag indicating if the array is empty.
     *
     * @return  flag indicating emptiness (true = empty and false = not empty)
     */
    public final boolean isEmpty() {
        return currentLength == 0;
    }

    /**
     * Gets last element of array without removing it.
     *
     * @return  last element of array
     */
    public final int lastElement() {

        if (currentLength > 0) {
            return intBuffer[ptr];
        }

        return -1;
    }

    /**
     * Length of the array (ie - how many data points are stored).
     *
     * @return  number of data points and not total size of array
     */
    public int length() {
        return currentLength;
    }

    /**
     * Pops first value off the FIFO array.
     *
     * @return  returns value of the top of the array. If the array is empty it returns -1.
     */
    public final int popFirstIn() {
        int x;

        if (currentLength > 0) {
            currentLength--;
            x = intBuffer[firstPtr];

            if (firstPtr == (intBuffer.length - 1)) {
                firstPtr = 0;
            } else {
                firstPtr++;
            }

            return x;
        }

        return -1;
    }

    /**
     * Pops last value off the FILO array.
     *
     * @return  returns value of the top of the array. If the array is empty it returns -1.
     */
    public final int popLastIn() {
        int x;

        if (currentLength > 0) {
            currentLength--;
            x = intBuffer[ptr];

            if (ptr > 0) {
                ptr--;
            } else {
                ptr = intBuffer.length - 1;
            }

            return x;
        }

        return -1;
    }

    /**
     * Pushes a new value onto the array. Checks to make sure array has the capacity to store the new value. If not,
     * this allocates the more memory by the size specified in the variable chunk.
     *
     * @param  x  value to be pushed onto the array
     */
    public final void push(int x) {
        int i, j;
        currentLength++;

        if (ptr == (intBuffer.length - 1)) {
            ptr = 0;
        } else {
            ptr++;
        }

        if (currentLength >= intBuffer.length) {
            intTemp = new int[intBuffer.length + chunk];

            for (i = 0, j = firstPtr; i < intBuffer.length; i++) {
                intTemp[i] = intBuffer[j];

                if (j == (intBuffer.length - 1)) {
                    j = 0;
                } else {
                    j++;
                }
            }

            firstPtr = 0;
            ptr = intBuffer.length;
            intBuffer = intTemp;
            System.gc();
        }

        intBuffer[ptr] = x;
    }

    /**
     * Removes all elements of the array.
     */
    public void removeAllElements() {
        ptr = -1;
        firstPtr = 0;
        currentLength = 0;
    }

    /**
     * Removes element at index. Slow method - this is not a linked list.
     *
     * @param  i  index of value to be removed
     */
    public void removeElementAt(int i) {
        int c, cNext;

        if ((i >= 0) && (i < currentLength)) {
            currentLength--;
            c = i;

            if (c == (currentLength - 1)) {
                cNext = 0;
            } else {
                cNext = c + 1;
            }

            while (c != ptr) {
                intBuffer[c] = intBuffer[cNext];
                c = cNext;

                if (cNext == (currentLength - 1)) {
                    cNext = 0;
                } else {
                    cNext++;
                }
            }

            if (ptr > 0) {
                ptr--;
            } else {
                ptr = intBuffer.length - 1;
            }
        }
    }

    /**
     * Removes last element is array.
     */
    public final void removeLastElement() {

        if (currentLength > 0) {
            currentLength--;

            if (ptr > 0) {
                ptr--;
            } else {
                ptr = intBuffer.length - 1;
            }
        }
    }

    /**
     * Sets the capacity of the array.
     *
     * @param  cap  capacity of the array
     */
    public void setCapacity(int cap) {
        capacity = cap;
    }

    /**
     * Sets the chunk size when adding memory to the array.
     *
     * @param  chunk  the size of memory to add when the array needs expanding
     */
    public void setChunk(int chunk) {
        this.chunk = chunk;
    }

    /**
     * Gets the smallest in value element of array without removing it.
     *
     * @return  smallest element of array
     */
    public final int smallestElement() {
        int i, j;
        int value = Integer.MAX_VALUE;

        if (currentLength > 0) {

            for (i = 0, j = firstPtr; i < currentLength; i++) {

                if (intBuffer[j] < value) {
                    value = intBuffer[j];

                    if (j == (intBuffer.length - 1)) {
                        j = 0;
                    } else {
                        j++;
                    }
                }
            }

            return value;
        }

        return -1;
    }

}
