package gov.nih.mipav.model.structures;


/**
 * Contains information for a point used in the the Dijkstra minimum cost path algorithm. This class implements the
 * Comparable interface which is required of any node to be inserted in any SortedSet implementation which is required
 * for finding a minimum cost path.
 */
public class DijkstraCostItem extends Object implements Comparable<Object> {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Identifies an undefined key value. */
    public static final int KEY_UNDEFINED = -1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Flag is set once this sample's minimum cost has been finalized. */
    private boolean m_bCostFinalized;

    /** Current accumulated cost in the Dijkstra algorithm. */
    private float m_fCost;

    /** Current accumulated distance in the Dijkstra algorithm. */
    private float m_fDistance;

    /** Non-negative unique key value identifying this point. */
    private final int m_iKey;

    /**
     * This value is KEY_UNDEFINED until such time that the point has been marked visited as part of the Dijkstra
     * algorithm. When it has been visited, this value identifies the key value for the point which is used to get to
     * this point along the minimum cost path.
     */
    private int m_iKeyPrev;

    /**
     * This is the number of samples along the minimum cost path it takes to get from the starting sample to this
     * sample.
     */
    private int m_iNumPrev;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Setup to associate this instance with a particular sample in the volume. Initially the sample is marked as having
     * the maximum possible distance but that its distance has not be finalized.
     *
     * @param  iKey  Value uniquely identifying this point.
     */
    public DijkstraCostItem(int iKey) {
        m_iKey = iKey;
        m_bCostFinalized = false;
        update(Float.MAX_VALUE, Float.MAX_VALUE, KEY_UNDEFINED, 0);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Implementation required by the Comparable interface. Compares this object with the specified object for order.
     *
     * @param   kObject  the object to be compared
     *
     * @return  a negative (positive) integer if this object is ordered before (after) the specified object, or zero if
     *          the two objects are considered have equal ordering
     */
    public int compareTo(Object kObject) {
        DijkstraCostItem kThat = (DijkstraCostItem) kObject;

        // Cost is the first field to use for ordering.
        // If costs are equal, then next use the unique key value.
        if (this.m_fCost == kThat.m_fCost) {
            return this.m_iKey - kThat.m_iKey;
        } else if (this.m_fCost > kThat.m_fCost) {
            return +1;
        } else {
            return -1;
        }
    }

    /**
     * Get the current accumulated cost.
     *
     * @return  current accumulated cost
     */
    public float getCost() {
        return m_fCost;
    }

    /**
     * Get the current accumulated distance.
     *
     * @return  current accumulated distance
     */
    public float getDistance() {
        return m_fDistance;
    }

    /**
     * Get the unique key identifier associated with this point.
     *
     * @return  int Non-negative unique key identifier
     */
    public int getKey() {
        return m_iKey;
    }

    /**
     * Get the unique key value for the point which is used to get to this point along the minimum cost path.
     *
     * @return  Unique key value for this previous point, or KEY_UNDEFINED if point is not connected yet along the
     *          minimum cost path.
     */
    public int getKeyPrev() {
        return m_iKeyPrev;
    }

    /**
     * Get the number of samples along the current minimum cost path that preceed this sample in the volume.
     *
     * @return  non-negative integer for number of preceeding path samples along path. Should be zero for the starting
     *          point along the path.
     */
    public int getNumPrev() {
        return m_iNumPrev;
    }

    /**
     * Test whether or not the sample has had its cost finalized.
     *
     * @return  true if the cost for this sample has been finalized
     */
    public boolean isFinalized() {
        return m_bCostFinalized;
    }

    /**
     * Mark the sample as having its cost finalized, meaning that it can no longer be updated.
     */
    public void markFinalized() {
        m_bCostFinalized = true;
    }

    /**
     * Mark this sample as the beginning of the minimum cost path.
     */
    public void markStart() {
        update(0.0f, 0.0f, KEY_UNDEFINED, 0);
        markFinalized();
    }

    /**
     * Mark this node as being visited along the minimum cost path if not already done so, but only if the point has not
     * been marked as having its distance finalized. Update the accumulated distance for this sample including an
     * indication of the point from which to get to this sample along the current minimum cost path.
     *
     * @param  fCost      accumulated cost to update for this sample; must not be negative
     * @param  fDistance  accumulated distance to update for this sample; must not be negative.
     * @param  iKeyPrev   Unique key value for the point which is used to get to this point along the minimum cost path.
     * @param  iNumPrev   number of samples along the minimum cost path leading up to this sample. Should be one more
     *                    than the same field value for the point having key value iKeyPrev, if there is such a previous
     *                    point.
     */
    public void update(float fCost, float fDistance, int iKeyPrev, int iNumPrev) {

        if (!isFinalized() && (fCost >= 0.0f) && (fDistance >= 0.0f)) {
            m_fCost = fCost;
            m_fDistance = fDistance;
            m_iKeyPrev = iKeyPrev;
            m_iNumPrev = iNumPrev;
        }
    }
}
