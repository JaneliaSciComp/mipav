package gov.nih.mipav.model.algorithms.levelset;
import java.util.Vector;

/**
 * The abstract base class for fast-marching methods of images.  This is
 * based on material from J. Sethian's book, <i>Level Set Methods and Fast
 * Marching Methods</i>.
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public abstract class LseFastMarch
{
    //~ Instance fields ------------------------------------------------------

	/** The number of image elements. */
	int m_iQuantity;
	
	/**
	 * The inverse speeds associated with the image elements.  These are
	 * used to control the evolution of region boundaries.
	 */
    float[] m_afInvSpeed;
    
    /**
     * The times at which the image elements are visited by moving boundaries.
     */
    float[] m_afTime;
    
    /** The minimum time in m_afTime[]. */
    float m_fMinTime;
    
    /** The maximum time in m_afTime[]. */
    float m_fMaxTime;
    
    /**
     * A min-heap data structure used to keep track of the times so that the
     * image element corresponding to the minimum time becomes the next
     * processed boundary point.
     */
    LseMinHeap m_kHeap;
    
    /**
     * Those image elements that are adjacent to the current region boundary
     * points are tracked to allow fast updates of the boundary.
     */
    LseMinHeap.Record[] m_akTrial;

    //~ Constructors ---------------------------------------------------------

    /**
     * Create a new fast-marching object, where the speed varies per image
     * element.
     * 
     * @param iQuantity The number of image elements.
     * @param afSpeed The array of speeds for the image elements.
     * @param aiSeeds The initial region points.
     */
    protected LseFastMarch (int iQuantity, float[] afSpeed, int[] aiSeeds)
    {
        m_kHeap = new LseMinHeap(iQuantity);
        initialize(iQuantity,aiSeeds);
        initializeSpeed(afSpeed);
    }

    /**
     * Create a new fast-marching object, where the speed is constant for all
     * image elements.
     * 
     * @param iQuantity The number of image elements.
     * @param fSpeed The common speed for the image elements.
     * @param aiSeeds The initial region points.
     */
    protected LseFastMarch (int iQuantity, float fSpeed, int[] aiSeeds)
    {
        m_kHeap = new LseMinHeap(iQuantity);
        initialize(iQuantity,aiSeeds);
        initializeSpeed(fSpeed);
    }

    //~ Methods --------------------------------------------------------------

    /**
     * Get the number of image elements.
     * 
     * @return The number of image elements.
     */
    public final int getQuantity ()
    {
        return m_iQuantity;
    }

    /**
     * Set the time of visitation for an image element.
     * 
     * @param i The index of the image element.
     * @param fTime The time of visitation for the image element.
     */
    public final void setTime (int i, float fTime)
    {
        m_afTime[i] = fTime;
    }

    /**
     * Get the time of visitation for an image element.
     * 
     * @param i The index of the image element.
     * @return The time of visitation for the image element.
     */
    public final float getTime (int i)
    {
        return m_afTime[i];
    }

    /**
     * Compute the minimum and maximum visitation times for the image
     * elements.  The extreme values may be retrieved via calls to
     * getMinTime() and getMaxTime().
     */
    public final void computeTimeExtremes ()
    {
        m_fMinTime = Float.MAX_VALUE;
        m_fMaxTime = -Float.MAX_VALUE;
        int i;
        for (i = 0; i < m_iQuantity; i++)
        {
            if (isValid(i))
            {
                m_fMinTime = m_afTime[i];
                m_fMaxTime = m_fMinTime;
                break;
            }
        }
        // assert(i < m_iQuantity);

        for (/**/; i < m_iQuantity; i++)
        {
            if (isValid(i))
            {
                if (m_afTime[i] < m_fMinTime)
                {
                    m_fMinTime = m_afTime[i];
                }
                else if (m_afTime[i] > m_fMaxTime)
                {
                    m_fMaxTime = m_afTime[i];
                }
            }
        }
    }

    /**
     * Get the minimum visitation time for the image elements.
     * 
     * @return The minimum visitation time.
     */
    public final float getMinTime ()
    {
        return m_fMinTime;
    }

    /**
     * Get the maximum visitation time for the image elements.
     * 
     * @return The maximum visitation time.
     */
    public final float getMaxTime ()
    {
        return m_fMaxTime;
    }

    /**
     * An image element is <i>valid</i> whenever its visitation time t
     * satisfies the conditions: 0 &le; t &lt; Float.<i>MAX_VALUE</i>).
     * 
     * @param i The index of the image element.
     * @return The Boolean result of the t-constraints.
     */
    public final boolean isValid (int i)
    {
    	if (0 <= i && i < m_iQuantity)
    	{
    		return (0.0f <= m_afTime[i] && m_afTime[i] < Float.MAX_VALUE);
    	}
    	return false;
    }

    /**
     * An image element is a <i>trial</i> element when its visitation time t
     * is invalid (t &lt; 0), its inverse speed is finite, and the element is
     * adjacent to the boundary of the current region.
     * 
     * @param i The index of the image element.
     * @return The Boolean result of the t-constraints.
     */
    public final boolean isTrial (int i)
    {
    	if (0 <= i && i < m_iQuantity)
    	{
    		return m_akTrial[i] != null;
    	}
    	return false;
    }

    /**
     * An image element is a <i>far</i> element when its visitation time t
     * is infinite:  t = Float.<i>MAX_VALUE</i>.  Such elements are not
     * considered when searching the heap for the next element to visit.
     * 
     * @param i The index of the image element.
     * @return
     */
    public final boolean isFar (int i)
    {
    	if (0 <= i && i < m_iQuantity)
    	{
    		return m_afTime[i] == Float.MAX_VALUE;
    	}
    	return false;
    }

    /**
     * An image element is never visited when its corresponding speed is
     * zero.  Such elements are preprocessed initially so that there
     * visitation times are t = -Float.<i>MAX_VALUE</i>.
     * 
     * @param i  The index of the image element.
     * @return
     */
    public final boolean isZeroSpeed (int i)
    {
    	if (0 <= i && i < m_iQuantity)
    	{
    		return m_afTime[i] == -Float.MAX_VALUE;
    	}
    	return false;
    }

    /**
     * An image element is an <i>interior</i> element when it is <i>valid</i>
     * and not a <i>trial</i> element.  Effectively, these are interior points
     * of the current region.
     * 
     * @param i The index of the image element.
     * @return
     */
    public final boolean isInterior (int i)
    {
        return isValid(i) && !isTrial(i);
    }

    /**
     * Get the indices of the interior points of the current region.
     * 
     * @return The array of indices of the interior points.
     */
    public int[] getInterior ()
    {
        Vector kInterior = new Vector(m_iQuantity/8,m_iQuantity/8);
        int i;
        for (i = 0; i < m_iQuantity; i++)
        {
            if (isValid(i) && !isTrial(i))
            {
                kInterior.add(new Integer(i));
            }
        }

        int[] aiInterior = new int[kInterior.size()];
        for (i = 0; i < aiInterior.length; i++)
        {
            Integer kObject = (Integer)kInterior.get(i);
            aiInterior[i] = kObject.intValue();
        }
        return aiInterior;
    }

    /**
     * Get the indices of the boundary points of the current region.  This is
     * an abstract function, because the identification of such points depends
     * on the dimensionality of the image.
     * 
     * @return The array of indices of the boundary points.
     */
    public abstract int[] getBoundary ();

    /**
     * An image element is a <i>boundary</i> point when it is on the boundary
     * of the current region.  This is an abstract function, because the
     * identification of such points depends on the dimensionality of the
     * image.
     * 
     * @param i The index of the image element.
     * @return  The Boolean result indicating whether or not the image
     *     element is a boundary point.
     */
    public abstract boolean isBoundary (int i);

    /**
     * Run one iteration of the fast marching algorithm.  This is an abstract
     * function, because the identification of such points depends on the
     * dimensionality of the image.
     */
    public abstract void iterate ();

    /**
     * Partial initialization of the fast marching object.  The visitation
     * times are set to zero for the initial region (the seeds) but to
     * Float.<i>MAX_VALUE</i> for all other points.  Initially, none of the
     * points are trial points.
     * 
     * @param iQuantity The number of image elements.
     * @param aiSeeds The initial region points.
     */
    protected void initialize (int iQuantity, int[] aiSeeds)
    {
        m_iQuantity = iQuantity;

        // The seed points have a crossing time of 0.  As the iterations
        // occur, some of the non-seed points are visited by the moving front.
        // The valid crossing times are 0 <= t < Float.MAX_VALUE.  A value of
        // Float.MAX_VALUE indicates the pixel has not yet been reached by the
        // moving front.  If the speed value at a pixel is 0, the pixel is
        // marked with a time of -Float.MAX_VALUE.  Such pixels can never be
        // visited; the minus sign distinguishes these from pixels not yet
        // reached during iteration.
        m_afTime = new float[m_iQuantity];
        int i;
        for (i = 0; i < m_iQuantity; i++)
        {
            m_afTime[i] = Float.MAX_VALUE;
        }

        for (i = 0; i < aiSeeds.length; i++)
        {
            m_afTime[aiSeeds[i]] = 0.0f;
        }
        m_fMinTime = 0.0f;
        m_fMaxTime = Float.MAX_VALUE;

        // Trial pixels are identified by having min-heap record associated
        // with them.  Known or Far pixels have no associated record.
        m_akTrial = new LseMinHeap.Record[m_iQuantity];
        for (i = 0; i < m_iQuantity; i++)
        {
            m_akTrial[i] = null;
        }
    }

    /**
     * Partial initialization of the fast marching object.  The inverse
     * speeds are computed for those speeds that are positive.  If an input
     * speed is zero, the inverse speed is set to Float.<i>MAX_VALUE</i> and
     * the visitation time is set to -Float.<i>MAX_VALUE</i> as a flag that
     * the corresponding image element should never be visited.
     * 
     * @param afSpeed The speeds for the image elements.
     */
    protected void initializeSpeed (float[] afSpeed)
    {
        // Invert the speeds since the reciprocals are all that are needed in
        // the numerical method.  Mark all zero-speed interior pixels.
        m_afInvSpeed = new float[m_iQuantity];
        for (int i = 0; i < m_iQuantity; i++)
        {
            // assert(afSpeed[i] >= 0.0f);
            if (afSpeed[i] > 0.0f)
            {
                m_afInvSpeed[i] = 1.0f/afSpeed[i];
            }
            else
            {
                m_afInvSpeed[i] = Float.MAX_VALUE;
                m_afTime[i] = -Float.MAX_VALUE;
            }
        }
    }

    /**
     * Partial initialization of the fast marching object.  The inverse
     * speed is constnnt for all image elements and must be positive.
     * 
     * @param fSpeed The common speed for the image elements.
     */
    protected void initializeSpeed (float fSpeed)
    {
        float fInvSpeed = 1.0f/fSpeed;
        m_afInvSpeed = new float[m_iQuantity];
        for (int i = 0; i < m_iQuantity; i++)
        {
            m_afInvSpeed[i] = fInvSpeed;
        }
    }
}
