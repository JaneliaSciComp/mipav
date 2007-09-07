package gov.nih.mipav.model.algorithms.levelset;

/**
 * <p>A class that encapsules a min-heap data structure.  The minimum value
 * of a set of numbers is guaranteed always to be at the root of the heap.</p>
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public class LseMinHeap
{
    /**
     * Each heap record stores a numeric value, the index of the record
     * within the records array, and a <i>generator</i> which acts as an
     * identifier for the application to use to store information about how
     * the numeric value was generated in the application.
     */
    public class Record
    {
        //~ Instance fields --------------------------------------------------

    	/**
    	 * The generator for the heap record.  This is application-dependent
    	 * data.
    	 */
        private int m_iGenerator;
        
        /** The numeric value of the heap record. */
        private float m_fValue;
        
        /** The index of the heap record in the records array. */
        private int m_iIndex;

        //~ Constructors -----------------------------------------------------
    	
    	/**
    	 * Creates a new heap record.
    	 */
        public Record ()
        {
            m_iGenerator = -1;
            m_fValue = Float.MAX_VALUE;
            m_iIndex = -1;
        }

        //~ Methods ----------------------------------------------------------

        /**
         * Set the generator for the heap record.
         * 
         * @param iGenerator The generator value.
         */
        public final void setGenerator (int iGenerator)
        {
            m_iGenerator = iGenerator;
        }

        /**
         * Get the generator for the heap record.
         * 
         * @return The generator value.
         */
        public final int getGenerator ()
        {
            return m_iGenerator;
        }

        /**
         * Set the numeric value for the heap record.
         * 
         * @param fValue The numeric value.
         */
        public final void setValue (float fValue)
        {
            m_fValue = fValue;
        }

        /**
         * Get the numeric value for the heap record.
         * 
         * @return The numeric value.
         */
        public final float getValue ()
        {
            return m_fValue;
        }

        /**
         * Set the index of the heap record within the records array.
         * 
         * @param iIndex The index of the heap record.
         */
        public final void setIndex (int iIndex)
        {
            m_iIndex = iIndex;
        }

        /**
         * Get the index of the heap record within the records array.
         * 
         * @return The index of the heap record.
         */
        public final int getIndex ()
        {
            return m_iIndex;
        }
    }

    //~ Instance fields ------------------------------------------------------

    /** The number of active records in the heap. */
    int m_iQuantity;

    /** The record storage for the heap, allocated in one large chunk. */
    Record[] m_akRecords;

    //~ Constructors ---------------------------------------------------------
    
    /**
     * Create a new min-heap data structure.
     * 
     * @param iMaxQuantity The maximum number of heap records.  This number
     *     depends on the application's needs. 
     */
    public LseMinHeap (int iMaxQuantity)
    {
        m_iQuantity = 0;
        m_akRecords = new Record[iMaxQuantity];
        for (int i = 0; i < iMaxQuantity; i++)
        {
            m_akRecords[i] = new Record();
        }
    }

    //~ Methods --------------------------------------------------------------
    
    /**
     * Get the current number of active heap records.
     * 
     * @return The current number of active heap records.
     */
    public final int getQuantity ()
    {
        return m_akRecords.length;
    }

    /**
     * Get a record from the heap records.
     * 
     * @param i The index of the desired record.
     * @return The desired record.
     */
    public final Record getRecord (int i)
    {
        return m_akRecords[i];
    }

    /**
     * Insert a numeric value into the min-heap data structure.
     * 
     * @param iGenerator The application-specific identifier that is
     *     associated with the numeric value managed by the heap record.
     * @param fValue The numeric value managed by the heap record.
     * @return The heap record that manages the numeric value.
     */
    public final Record insert (int iGenerator, float fValue)
    {
        // Store the input information in the last heap record, which is the
        // last leaf in the tree.
        int iChild = m_iQuantity++;
        Record kRecord = m_akRecords[iChild];
        kRecord.setGenerator(iGenerator);
        kRecord.setValue(fValue);
        kRecord.setIndex(iChild);

        // Propagate the information toward the root of the tree until it
        // reaches its correct position, thus restoring the tree to a valid
        // heap.
        while (iChild > 0)
        {
            int iParent = (iChild - 1)/2;
            if (m_akRecords[iParent].getValue() <= fValue)
            {
                // The parent has a value smaller than or equal to the child's
                // value, so we now have a valid heap.
                break;
            }

            // The parent has a larger value than the child's value.  Swap the
            // parent and child:

            // Move the parent into the child's slot.
            m_akRecords[iParent].setIndex(iChild);
            m_akRecords[iChild] = m_akRecords[iParent];

            // Move the child into the parent's slot.
            kRecord.setIndex(iParent);
            m_akRecords[iParent] = kRecord;

            iChild = iParent;
        }

        return m_akRecords[iChild];
    }

    /**
     * Remove the root of the heap.  The root contains the minimum value of
     * all heap elements.
     * 
     * @return The record that corresponded to the root of the heap.
     */
    public final Record remove ()
    {
        // Get the information from the root of the heap.
        Record kRoot = m_akRecords[0];

        // Restore the tree to a heap.  Abstractly, pkRecord is the new root
        // of the heap.  It is moved down the tree via parent-child swaps
        // until it is in a location that restores the tree to a heap.
        int iLast = --m_iQuantity;
        Record kRecord = m_akRecords[iLast];
        int iParent = 0, iChild = 1;
        while (iChild <= iLast)
        {
            if (iChild < iLast)
            {
                // Select the child with smallest value to be the one that is
                // swapped with the parent, if necessary.
                int iChildP1 = iChild + 1;
                if (m_akRecords[iChild].getValue() >
                    m_akRecords[iChildP1].getValue())
                {
                    iChild = iChildP1;
                }
            }

            if (m_akRecords[iChild].getValue() >= kRecord.getValue())
            {
                // The tree is now a heap.
                break;
            }

            // Move the child into the parent's slot.
            m_akRecords[iChild].setIndex(iParent);
            m_akRecords[iParent] = m_akRecords[iChild];

            iParent = iChild;
            iChild = 2*iChild + 1;
        }

        // The previous 'last' record was moved to the root and propagated
        // down the tree to its final resting place, restoring the tree to a
        // heap.  The slot m_apkRecords[iParent] is that resting place.
        kRecord.setIndex(iParent);
        m_akRecords[iParent] = kRecord;

        // The old root record must be recycled.  Attach it to the slot that
        // contained the old last record.
        m_akRecords[iLast] = kRoot;

        return kRoot;
    }

    /**
     * The value of a heap record must be modified through this function call.
     * The side effect is that the heap must be updated accordingly to
     * accommodate the new value.
     * 
     * @param kRecord The record to be updated.
     * @param fValue The record's new numeric value.
     */
    public final void update (Record kRecord, float fValue)
    {
        int iParent, iChild, iChildP1, iMaxChild;

        if (fValue > kRecord.getValue())
        {
            kRecord.setValue(fValue);

            // The new value is larger than the old value.  Propagate it
            // towards the leaves.
            iParent = kRecord.getIndex();
            iChild = 2*iParent + 1;
            while (iChild < m_iQuantity)
            {
                // At least one child exists.  Locate the one of maximum
            	// value.
                if (iChild < m_iQuantity-1)
                {
                    // Two children exist.
                    iChildP1 = iChild + 1;
                    if (m_akRecords[iChild].getValue() <=
                        m_akRecords[iChildP1].getValue())
                    {
                        iMaxChild = iChild;
                    }
                    else
                    {
                        iMaxChild = iChildP1;
                    }
                }
                else
                {
                    // One child exists.
                    iMaxChild = iChild;
                }

                if (m_akRecords[iMaxChild].getValue() >= fValue)
                {
                    // The new value is in the correct place to restore the
                    // tree to a heap.
                    break;
                }

                // The child has a larger value than the parent's value.  Swap
                // the parent and child:

                // Move the child into the parent's slot.
                m_akRecords[iMaxChild].setIndex(iParent);
                m_akRecords[iParent] = m_akRecords[iMaxChild];

                // Move the parent into the child's slot.
                kRecord.setIndex(iMaxChild);
                m_akRecords[iMaxChild] = kRecord;

                iParent = iMaxChild;
                iChild = 2*iParent + 1;
            }
        }
        else if (fValue < kRecord.getValue())
        {
            kRecord.setValue(fValue);

            // The new weight is smaller than the old weight.  Propagate it
            // towards the root.
            iChild = kRecord.getIndex();
            while (iChild > 0)
            {
                // A parent exists.
                iParent = (iChild - 1)/2;

                if (m_akRecords[iParent].getValue() <= fValue)
                {
                    // The new value is in the correct place to restore the
                    // tree to a heap.
                    break;
                }

                // The parent has a smaller value than the child's value.
                // Swap the child and parent:

                // Move the parent into the child's slot.
                m_akRecords[iParent].setIndex(iChild);
                m_akRecords[iChild] = m_akRecords[iParent];

                // Move the child into the parent's slot.
                kRecord.setIndex(iParent);
                m_akRecords[iParent] = kRecord;

                iChild = iParent;
            }
        }
    }
}
