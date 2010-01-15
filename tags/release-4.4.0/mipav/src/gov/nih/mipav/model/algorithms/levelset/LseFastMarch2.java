package gov.nih.mipav.model.algorithms.levelset;
import java.util.Vector;

/**
 * The fast-marching method for 2D images.  This is based on material from
 * J. Sethian's book, <i>Level Set Methods and Fast Marching Methods</i>.
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public class LseFastMarch2 extends LseFastMarch
{
    //~ Instance fields ------------------------------------------------------

	/** The x-bound of the image, call it XBound. */
    protected int m_iXBound;
    
    /** The y-bound of the image, call it YBound. */
    protected int m_iYBound;
    
    /** XBm1 = XBound - 1 */
    protected int m_iXBm1;
    
    /** YBm1 = YBound - 1 */
    protected int m_iYBm1;
    
    //~ Constructors ---------------------------------------------------------

    /**
     * Create a new fast marching object for a 2D image.
     * 
     * @param iXBound The x-bound of the image.
     * @param iYBound The y-bound of the image.
     * @param afSpeed The speeds of the image elements.
     * @param aiSeeds The initial region points.
     */
    public LseFastMarch2 (int iXBound, int iYBound, float[] afSpeed,
    	int[] aiSeeds)
    {
        super(iXBound*iYBound,afSpeed,aiSeeds);
        initialize(iXBound,iYBound);
    }

    /**
     * Create a new fast marching object for a 2D image.
     * 
     * @param iXBound The x-bound of the image.
     * @param iYBound The y-bound of the image.
     * @param fSpeed The common speed of the image elements.
     * @param aiSeeds The initial region points.
     */
    public LseFastMarch2 (int iXBound, int iYBound, float fSpeed,
    	int[] aiSeeds)
    {
        super(iXBound*iYBound,fSpeed,aiSeeds);
        initialize(iXBound,iYBound);
    }

    //~ Methods --------------------------------------------------------------

    /**
     * Get the x-bound for the image.
     * 
     * @return The x-bound for the image.
     */
    public final int getXBound ()
    {
        return m_iXBound;
    }

    /**
     * Get the y-bound for the image.
     * 
     * @return The y-bound for the image.
     */
    public final int getYBound ()
    {
        return m_iYBound;
    }

    /**
     * Compute the 1-dimensional index for the pixel (x,y).
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The 1-dimensional index for the pixel.
     */
    public final int index (int iX, int iY)
    {
        // assert(0 <= iX && iX < m_iXBound && 0 <= iY && iY < m_iYBound);
        return iX + m_iXBound*iY;
    }

    /**
     * Get the indices of the boundary points of the current region.
     * 
     * @return The array of indices of the boundary points.
     */
    public int[] getBoundary ()
    {
        Vector kBoundary = new Vector(m_iQuantity/8,m_iQuantity/8);
        int i;
        for (i = 0; i < m_iQuantity; i++)
        {
            if (isValid(i) && !isTrial(i))
            {
                if (isTrial(i-1)
                ||  isTrial(i+1)
                ||  isTrial(i-m_iXBound)
                ||  isTrial(i+m_iXBound))
                {
                    kBoundary.add(new Integer(i));
                }
            }
        }

        int[] aiBoundary = new int[kBoundary.size()];
        for (i = 0; i < aiBoundary.length; i++)
        {
            Integer kObject = (Integer)kBoundary.get(i);
            aiBoundary[i] = kObject.intValue();
        }
        return aiBoundary;
    }

    /**
     * An image element is a <i>boundary</i> point when it is on the boundary
     * of the current region.
     * 
     * @param i The index of the image element.
     * @return  The Boolean result indicating whether or not the image
     *     element is a boundary point.
     */
    public boolean isBoundary (int i)
    {
        if (isValid(i) && !isTrial(i))
        {
            if (isTrial(i-1)
            ||  isTrial(i+1)
            ||  isTrial(i-m_iXBound)
            ||  isTrial(i+m_iXBound))
            {
                return true;
            }
        }
        return false;
    }

    /**
     * Run one iteration of the fast marching algorithm.
     */
    public void iterate ()
    {
         // Remove the minimum trial value from the heap.
        LseMinHeap.Record kRecord = m_kHeap.remove();
        int i = kRecord.getGenerator();

        // Promote the trial value to a known value.  The value was negative
        // but is now nonnegative (the heap stores only nonnegative numbers).
        // assert(isTrial(i) && m_afTime[i] == kRecord.getValue());
        m_akTrial[i] = null;

        // All trial pixels must be updated.  All far neighbors must become
        // trial pixels.
        int iM1 = i - 1;
        if (isTrial(iM1))
        {
            computeTime(iM1);
            m_kHeap.update(m_akTrial[iM1],m_afTime[iM1]);
        }
        else if (isFar(iM1))
        {
            computeTime(iM1);
            m_akTrial[iM1] = m_kHeap.insert(iM1,m_afTime[iM1]);
        }

        int iP1 = i + 1;
        if (isTrial(iP1))
        {
            computeTime(iP1);
            m_kHeap.update(m_akTrial[iP1],m_afTime[iP1]);
        }
        else if (isFar(iP1))
        {
            computeTime(iP1);
            m_akTrial[iP1] = m_kHeap.insert(iP1,m_afTime[iP1]);
        }

        int iMXB = i - m_iXBound;
        if (isTrial(iMXB))
        {
            computeTime(iMXB);
            m_kHeap.update(m_akTrial[iMXB],m_afTime[iMXB]);
        }
        else if (isFar(iMXB))
        {
            computeTime(iMXB);
            m_akTrial[iMXB] = m_kHeap.insert(iMXB,m_afTime[iMXB]);
        }

        int iPXB = i + m_iXBound;
        if (isTrial(iPXB))
        {
            computeTime(iPXB);
            m_kHeap.update(m_akTrial[iPXB],m_afTime[iPXB]);
        }
        else if (isFar(iPXB))
        {
            computeTime(iPXB);
            m_akTrial[iPXB] = m_kHeap.insert(iPXB,m_afTime[iPXB]);
        }
    }

    /**
     * Initialization of the 2D fast marching object.
     * 
     * @param iXBound The x-bound of the image.
     * @param iYBound The y-bound of the image.
     * @param fXSpacing The x-spacing of the image.
     * @param fYSpacing The y-spacing of the image.
     */
    protected void initialize (int iXBound, int iYBound)
    {
        m_iXBound = iXBound;
        m_iYBound = iYBound;
        m_iXBm1 = m_iXBound - 1;
        m_iYBm1 = m_iYBound - 1;

        // Boundary pixels are marked as zero speed to allow us to avoid
        // having to process the boundary pixels separately during the
        // iteration.
        int iX, iY, i;

        // vertex (0,0)
        i = index(0,0);
        m_afInvSpeed[i] = Float.MAX_VALUE;
        m_afTime[i] = -Float.MAX_VALUE;

        // vertex (xmax,0)
        i = index(m_iXBm1,0);
        m_afInvSpeed[i] = Float.MAX_VALUE;
        m_afTime[i] = -Float.MAX_VALUE;

        // vertex (0,ymax)
        i = index(0,m_iYBm1);
        m_afInvSpeed[i] = Float.MAX_VALUE;
        m_afTime[i] = -Float.MAX_VALUE;

        // vertex (xmax,ymax)
        i = index(m_iXBm1,m_iYBm1);
        m_afInvSpeed[i] = Float.MAX_VALUE;
        m_afTime[i] = -Float.MAX_VALUE;

        // edges (x,0) and (x,ymax)
        for (iX = 0; iX < m_iXBound; iX++)
        {
            i = index(iX,0);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
            i = index(iX,m_iYBm1);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
        }

        // edges (0,y) and (xmax,y)
        for (iY = 0; iY < m_iYBound; iY++)
        {
            i = index(0,iY);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
            i = index(m_iXBm1,iY);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
        }

        // Compute the first batch of trial pixels.  These are pixels a grid
        // distance of one away from the seed pixels.
        for (iY = 1; iY < m_iYBm1; iY++)
        {
            for (iX = 1; iX < m_iXBm1; iX++)
            {
                i = index(iX,iY);
                if (isFar(i))
                {
                    if ((isValid(i-1) && !isTrial(i-1))
                    ||  (isValid(i+1) && !isTrial(i+1))
                    ||  (isValid(i-m_iXBound) && !isTrial(i-m_iXBound))
                    ||  (isValid(i+m_iXBound) && !isTrial(i+m_iXBound)))
                    {
                        computeTime(i);
                        m_akTrial[i] = m_kHeap.insert(i,m_afTime[i]);
                    }
                }
            }
        }
    }

    /**
     * Compute the visitation time for an image element.  In most cases, the
     * time is a root to a quadratic equation that is derived from the
     * finite-difference method used to propagate the region boundary using
     * the inverse speeds.
     * 
     * @param i The index of the image element.
     */
     protected void computeTime (int i)
    {
        boolean bHasXTerm;
        float fXConst;
        if (isValid(i-1))
        {
            bHasXTerm = true;
            fXConst = m_afTime[i-1];
            if (isValid(i+1))
            {
                if (m_afTime[i+1] < fXConst)
                {
                    fXConst = m_afTime[i+1];
                }
            }
        }
        else if (isValid(i+1))
        {
            bHasXTerm = true;
            fXConst = m_afTime[i+1];
        }
        else
        {
            bHasXTerm = false;
            fXConst = 0.0f;
        }

        boolean bHasYTerm;
        float fYConst;
        if (isValid(i-m_iXBound))
        {
            bHasYTerm = true;
            fYConst = m_afTime[i-m_iXBound];
            if (isValid(i+m_iXBound))
            {
                if (m_afTime[i+m_iXBound] < fYConst)
                {
                    fYConst = m_afTime[i+m_iXBound];
                }
            }
        }
        else if (isValid(i+m_iXBound))
        {
            bHasYTerm = true;
            fYConst = m_afTime[i+m_iXBound];
        }
        else
        {
            bHasYTerm = false;
            fYConst = 0.0f;
        }

        if (bHasXTerm)
        {
            if (bHasYTerm)
            {
                float fSum = fXConst + fYConst;
                float fDif = fXConst - fYConst;
                float fDiscr = 2.0f*m_afInvSpeed[i]*m_afInvSpeed[i] -
                    fDif*fDif;
                if (fDiscr >= 0.0f)
                {
                    // The quadratic equation has a real-valued solution.
                    // Choose the largest positive root for the crossing time.
                    m_afTime[i] = 0.5f*(fSum +
                        (float)Math.sqrt((double)fDiscr));
                }
                else
                {
                    // The quadratic equation does not have a real-valued
                    // solution.  This can happen when the speed is so large
                    // that the time gradient has very small length, which means
                    // that the time has not changed significantly from the
                    // neighbors to the current pixel.  Just choose the maximum
                    // time of the neighbors (Is there a better choice?).
                    m_afTime[i] = (fDif >= 0.0f ? fXConst : fYConst);
                }
            }
            else
            {
                // The equation is linear.
                m_afTime[i] = m_afInvSpeed[i] + fXConst;
            }
        }
        else if (bHasYTerm)
        {
            // The equation is linear.
            m_afTime[i] = m_afInvSpeed[i] + fYConst;
        }
        else
        {
            // The pixel must have at least one known neighbor.
            // assert(false);
        }
    }
}
