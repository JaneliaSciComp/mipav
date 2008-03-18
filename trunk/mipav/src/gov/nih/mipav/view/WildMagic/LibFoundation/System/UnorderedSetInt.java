package gov.nih.mipav.view.WildMagic.LibFoundation.System;


/**
 * DOCUMENT ME!
 */
public class UnorderedSetInt {

    /** The array storage for the set. */
    protected int[] m_aiElement;

    /** On a reallocation, the old maximum quantity is incremented by this value. */
    protected int m_iGrow;

    /** The maximum number of elements in the array. It is always the case that m_iQuantity <= m_iMaxQuantity. */
    protected int m_iMaxQuantity;

    /** Support for remove and removeAt. */
    protected int m_iOldIndex, m_iNewIndex;

    /** The number of valid elements in the array. The valid indices are 0 <= i < m_iQuantity. */
    protected int m_iQuantity;

    /**
     * The default growth value for reallocations of the array representing the set. The application can change this
     * to whatever is appropriate for its purposes.
     */
    private int DEFAULT_GROW = 8;

    /**
     * Construct an empty unordered set. The initial maximum quantity and growth values are DEFAULT_GROW. When
     */
    public UnorderedSetInt() {
        reset();
    }

    /**
     * Create an unordered set that is a deep copy of the input set.
     *
     * @param  kSet  The input set to copy.
     */
    public UnorderedSetInt(UnorderedSetInt kSet) {
        copy(kSet);
    }

    /**
     * Construct an empty unordered set with the specified maximum quantity and growth values.
     *
     * @param  iMaxQuantity  The initial number of elements in the array. If the value is nonpositive, the initial
     *                       number is DEFAULT_GROW.
     * @param  iGrow         The growth amount for a reallocation. If a reallocation occurs, the new number of
     *                       elements is the current maximum quantity plus the growth value. If the input value is
     *                       nonpositive, the growth is set to DEFAULT_GROW.
     */
    public UnorderedSetInt(int iMaxQuantity, int iGrow) {
        reset(iMaxQuantity, iGrow);
    }

    /**
     * Append an element to the end of the storage array.
     *
     * @param   iElement  The element to append.
     *
     * @return  The array location that contains the newly appended element. A side effect of this call is
     *          reallocation of the storage array, if necessary.
     */
    public int append(int iElement) {

        if (m_iQuantity == m_iMaxQuantity) {
            int iNewMaxQuantity = m_iMaxQuantity + m_iGrow;
            int[] aiNewElement = new int[iNewMaxQuantity];

            System.arraycopy(m_aiElement, 0, aiNewElement, 0, m_iMaxQuantity);
            m_iMaxQuantity = iNewMaxQuantity;
            m_aiElement = aiNewElement;
        }

        int iLocation = m_iQuantity++;

        m_aiElement[iLocation] = iElement;

        return iLocation;
    }

    /**
     * Use exactly the amount of array storage for the current elements in the set. After the call, getQuantity()
     * and getMaximumQuantity() return the same value. This call does cause a reallocation.
     */
    public void compactify() {

        if (m_iQuantity > 0) {

            // Try Catch - Matt
            int[] aiNewElement = new int[m_iQuantity];

            System.arraycopy(m_aiElement, 0, aiNewElement, 0, m_iQuantity);
            m_iMaxQuantity = m_iQuantity;
            m_aiElement = aiNewElement;
        } else {
            reset();
        }
    }

    /**
     * Make a deep copy of the input set.
     *
     * @param  kSet  The set to make a deep copy of.
     */
    public void copy(UnorderedSetInt kSet) {
        m_iQuantity = kSet.m_iQuantity;
        m_iMaxQuantity = kSet.m_iMaxQuantity;
        m_iGrow = kSet.m_iGrow;
        m_aiElement = new int[m_iMaxQuantity];
        System.arraycopy(kSet.m_aiElement, 0, m_aiElement, 0, m_iMaxQuantity);
    }

    /**
     * Search the set to see if the input element currently exists.
     *
     * @param   iElement  The element to search for.
     *
     * @return  The value is true if and only if the element is found in the set.
     */
    public boolean exists(int iElement) {

        for (int i = 0; i < m_iQuantity; i++) {

            if (iElement == m_aiElement[i]) {
                return true;
            }
        }

        return false;
    }

    /**
     * Retrieve the element in the array location i. It is necessary that 0 <= i < getQuantity() in order to read
     * valid elements.
     *
     * @param   i  The array location whose element is to be retrieved.
     *
     * @return  The element in array location i.
     */
    public final int get(int i) {
        return m_aiElement[i];
    }

    /**
     * The growth value for reallocations. If a reallocation must occur, the new maximum quantity is the current
     * maximum quantity plus the growth amount.
     *
     * @return  The growth value.
     */
    public final int getGrow() {
        return m_iGrow;
    }

    /**
     * The maximum quantity of elements in the set. Not all elements are necessarily used. The used quantity is
     * provided by getQuantity().
     *
     * @return  The maximum quantity of elements in the set.
     */
    public final int getMaxQuantity() {
        return m_iMaxQuantity;
    }

    /**
     * On a call to remove or removeAt, the last element in the array is potentially moved to the array location
     * vacated by the removed element. The new location of the last element is retrived by this function. However,
     * if the last element is the one that was removed, this function returns -1. If you need the value, you must
     * call this function before the next call to remove or removeAt.
     *
     * @return  The new location of the last element that was moved.
     */
    public final int getNewIndex() {
        return m_iNewIndex;
    }

    /**
     * On a call to remove or removeAt, the last element in the array is moved to the array location vacated by the
     * removed element. The old location of the last element is retrived by this function. If you need the value,
     * you must call this function before the next call to remove or removeAt.
     *
     * @return  The old location of the last element that was moved.
     */
    public final int getOldIndex() {
        return m_iOldIndex;
    }

    /**
     * The current number of valid elements in the array. This number is less than or equal to the maximum quantity.
     * The elements with indices 0 through getQuantity()-1 are the valid ones.
     *
     * @return  The current number of valid elements.
     */
    public final int getQuantity() {
        return m_iQuantity;
    }

    /**
     * Insert an element into the set.
     *
     * @param   iElement  The element to insert.
     *
     * @return  The value is true if and only if the element is inserted. The input element is not inserted if it
     *          already exists in the set. A side effect of this call is reallocation of the storage array, if
     *          necessary.
     */
    public boolean insert(int iElement) {
        int i;

        for (i = 0; i < m_iQuantity; i++) {

            if (iElement == m_aiElement[i]) {
                return false;
            }
        }

        if (m_iQuantity == m_iMaxQuantity) {
            int iNewMaxQuantity = m_iMaxQuantity + m_iGrow;
            int[] aiNewElement = new int[iNewMaxQuantity];

            System.arraycopy(m_aiElement, 0, aiNewElement, 0, m_iMaxQuantity);
            m_iMaxQuantity = iNewMaxQuantity;
            m_aiElement = aiNewElement;
        }

        m_aiElement[m_iQuantity++] = iElement;

        return true;
    }

    /**
     * Remove the specified element from the set.
     *
     * @param   iElement  The element to remove.
     *
     * @return  The value is true if and only if the element existed and was removed. The last element is
     *          potentially moved into the slot vacated by the specified element. If needed, the old and new
     *          locations of the last element can be retrieved by calls to getOldIndex() and getNewIndex(). If the
     *          last element was the one removed, getNewIndex() returns -1.
     */
    public boolean remove(int iElement) {

        for (int i = 0; i < m_iQuantity; i++) {

            if (iElement == m_aiElement[i]) {
                m_iQuantity--;
                m_iOldIndex = m_iQuantity;

                if (i != m_iQuantity) {
                    m_aiElement[i] = m_aiElement[m_iQuantity];
                    m_iNewIndex = i;
                } else {
                    m_iNewIndex = -1;
                }

                return true;
            }
        }

        return false;
    }

    /**
     * Remove the element from the set in the specified location.
     *
     * @param   i  The array location whose element is to be removed.
     *
     * @return  The value is true if and only if the input location is within the valid index range 0 <= i <
     *          getQuantity(). The last element is potentially moved into the slot vacated by the specified element.
     *          If needed, the old and new locations of the last element can be retrieved by calls to getOldIndex()
     *          and getNewIndex(). If the last element was the one removed, getNewIndex() returns -1.
     */
    public boolean removeAt(int i) {

        if ((0 <= i) && (i < m_iQuantity)) {
            m_iQuantity--;
            m_iOldIndex = m_iQuantity;

            if (i != m_iQuantity) {
                m_aiElement[i] = m_aiElement[m_iQuantity];
                m_iNewIndex = i;
            } else {
                m_iNewIndex = -1;
            }

            return true;
        }

        return false;
    }

    /**
     * Reset the unordered set to its initial state. The old array is deleted. The new array has a maximum quantity
     * of DEFAULT_GROW and the growth value is DEFAULT_GROW.
     */
    public void reset() {
        reset(0, 0);
    }

    /**
     * Reset the unordered set to the specified state. The old array is deleted. The new array has a maximum
     * quantity and growth value as specified by the inputs.
     *
     * @param  iMaxQuantity  The new maximum quantity for the array.
     * @param  iGrow         The new growth value.
     */
    public void reset(int iMaxQuantity, int iGrow) {

        if (iMaxQuantity <= 0) {
            iMaxQuantity = DEFAULT_GROW;
        }

        if (iGrow <= 0) {
            iGrow = DEFAULT_GROW;
        }

        m_iQuantity = 0;
        m_iMaxQuantity = iMaxQuantity;
        m_iGrow = iGrow;
        m_aiElement = new int[m_iMaxQuantity];
    }

    /**
     * Assign the specified element to array location i. It is necessary that 0 <= i < getMaxQuantity().
     *
     * @param  i         The array location to assign to.
     * @param  iElement  The element to assign to array location i.
     */
    public final void set(int i, int iElement) {
        m_aiElement[i] = iElement;
    }
}

