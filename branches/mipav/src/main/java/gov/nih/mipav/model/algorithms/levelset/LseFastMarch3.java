package gov.nih.mipav.model.algorithms.levelset;
import java.util.Vector;

/**
 * The fast-marching method for 3D images.  This is based on material from
 * J. Sethian's book, <i>Level Set Methods and Fast Marching Methods</i>.
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public class LseFastMarch3 extends LseFastMarch
{
    //~ Instance fields ------------------------------------------------------

	/** The x-bound of the image, call it XBound. */
    protected int m_iXBound;

	/** The y-bound of the image, call it YBound. */
    protected int m_iYBound;

	/** The z-bound of the image, call it ZBound. */
    protected int m_iZBound;
    
	/** XYBound = XBound*YBound */
    protected int m_iXYBound;
    
    /** XBm1 = XBound - 1 */
    protected int m_iXBm1;

    /** YBm1 = YBound - 1 */
    protected int m_iYBm1;

    /** ZBm1 = ZBound - 1 */
    protected int m_iZBm1;
    
    //~ Constructors ---------------------------------------------------------

    /**
     * Create a new fast marching object for a 3D image.
     * 
     * @param iXBound The x-bound of the image.
     * @param iYBound The y-bound of the image.
     * @param iZBound The z-bound of the image.
     * @param afSpeed The speeds of the image elements.
     * @param aiSeeds The initial region points.
     */
    public LseFastMarch3 (int iXBound, int iYBound, int iZBound,
        float[] afSpeed, int[] aiSeeds)
    {
        super(iXBound*iYBound*iZBound,afSpeed,aiSeeds);
        initialize(iXBound,iYBound,iZBound);
    }

    /**
     * Create a new fast marching object for a 3D image.
     * 
     * @param iXBound The x-bound of the image.
     * @param iYBound The y-bound of the image.
     * @param iZBound The z-bound of the image.
     * @param fSpeed The common speed of the image elements.
     * @param aiSeeds The initial region points.
     */
    public LseFastMarch3 (int iXBound, int iYBound, int iZBound,
        float fSpeed, int[] aiSeeds)
    {
        super(iXBound*iYBound*iZBound,fSpeed,aiSeeds);
        initialize(iXBound,iYBound,iZBound);
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
     * Get the z-bound for the image.
     * 
     * @return The z-bound for the image.
     */
    public final int getZBound ()
    {
        return m_iZBound;
    }

    /**
     * Compute the 1-dimensional index for the voxel (x,y,z).
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The 1-dimensional index for the voxel.
     */
    public final int index (int iX, int iY, int iZ)
    {
        // assert(0 <= iX && iX < m_iXBound
        //     && 0 <= iY && iY < m_iYBound
        //     && 0 <= iZ && iZ < m_iZBound);

        return iX + m_iXBound*(iY + m_iYBound*iZ);
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
                ||  isTrial(i+m_iXBound)
                ||  isTrial(i-m_iXYBound)
                ||  isTrial(i+m_iXYBound))
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
            ||  isTrial(i+m_iXBound)
            ||  isTrial(i-m_iXYBound)
            ||  isTrial(i+m_iXYBound))
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
        m_kHeap.remove();
        LseMinHeap.Record kRecord = m_kHeap.remove();
        //int i = kRecord.getIndex();
        int i = kRecord.getGenerator();

        // Promote the trial value to a known value.  The value was negative
        // but is now nonnegative (the heap stores only nonnegative numbers).
        // assert(isTrial(i) && m_afTime[i] == kRecord.getValue());
        m_akTrial[i] = null;

        // All trial voxels must be updated.  All far neighbors must become
        // trial voxels.
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

        int iMXYB = i - m_iXYBound;
        if (isTrial(iMXYB))
        {
            computeTime(iMXYB);
            m_kHeap.update(m_akTrial[iMXYB],m_afTime[iMXYB]);
        }
        else if (isFar(iMXYB))
        {
            computeTime(iMXYB);
            m_akTrial[iMXYB] = m_kHeap.insert(iMXYB,m_afTime[iMXYB]);
        }

        int iPXYB = i + m_iXYBound;
        if (isTrial(iPXYB))
        {
            computeTime(iPXYB);
            m_kHeap.update(m_akTrial[iPXYB],m_afTime[iPXYB]);
        }
        else if (isFar(iPXYB))
        {
            computeTime(iPXYB);
            m_akTrial[iPXYB] = m_kHeap.insert(iPXYB,m_afTime[iPXYB]);
        }
    }

    /**
     * Initialization of the 3D fast marching object.
     * 
     * @param iXBound The x-bound of the image.
     * @param iYBound The y-bound of the image.
     * @param iZBound The z-bound of the image.
     * @param fXSpacing The x-spacing of the image.
     * @param fYSpacing The y-spacing of the image.
     * @param fZSpacing The z-spacing of the image.
     */
    protected void initialize (int iXBound, int iYBound, int iZBound)
    {

        m_iXBound = iXBound;
        m_iYBound = iYBound;
        m_iZBound = iZBound;
        m_iXYBound = iXBound*iYBound;
        m_iXBm1 = m_iXBound - 1;
        m_iYBm1 = m_iYBound - 1;
        m_iZBm1 = m_iZBound - 1;

        // Boundary pixels are marked as zero speed to allow us to avoid
        // having to process the boundary voxels separately during the
        // iteration.
        int iX, iY, iZ, i;

        // vertex (0,0,0)
        i = index(0,0,0);
        m_afInvSpeed[i] = Float.MAX_VALUE;
        m_afTime[i] = -Float.MAX_VALUE;

        // vertex (xmax,0,0)
        i = index(m_iXBm1,0,0);
        m_afInvSpeed[i] = Float.MAX_VALUE;
        m_afTime[i] = -Float.MAX_VALUE;

        // vertex (0,ymax,0)
        i = index(0,m_iYBm1,0);
        m_afInvSpeed[i] = Float.MAX_VALUE;
        m_afTime[i] = -Float.MAX_VALUE;

        // vertex (xmax,ymax,0)
        i = index(m_iXBm1,m_iYBm1,0);
        m_afInvSpeed[i] = Float.MAX_VALUE;
        m_afTime[i] = -Float.MAX_VALUE;

        // vertex (0,0,zmax)
        i = index(0,0,m_iZBm1);
        m_afInvSpeed[i] = Float.MAX_VALUE;
        m_afTime[i] = -Float.MAX_VALUE;

        // vertex (xmax,0,zmax)
        i = index(m_iXBm1,0,m_iZBm1);
        m_afInvSpeed[i] = Float.MAX_VALUE;
        m_afTime[i] = -Float.MAX_VALUE;

        // vertex (0,ymax,zmax)
        i = index(0,m_iYBm1,m_iZBm1);
        m_afInvSpeed[i] = Float.MAX_VALUE;
        m_afTime[i] = -Float.MAX_VALUE;

        // vertex (xmax,ymax,zmax)
        i = index(m_iXBm1,m_iYBm1,m_iZBm1);
        m_afInvSpeed[i] = Float.MAX_VALUE;
        m_afTime[i] = -Float.MAX_VALUE;

        // edges (x,0,0) and (x,ymax,0)
        for (iX = 0; iX < m_iXBound; iX++)
        {
            i = index(iX,0,0);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
            i = index(iX,m_iYBm1,0);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
        }

        // edges (0,y,0) and (xmax,y,0)
        for (iY = 0; iY < m_iYBound; iY++)
        {
            i = index(0,iY,0);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
            i = index(m_iXBm1,iY,0);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
        }

        // edges (x,0,zmax) and (x,ymax,zmax)
        for (iX = 0; iX < m_iXBound; iX++)
        {
            i = index(iX,0,m_iZBm1);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
            i = index(iX,m_iYBm1,m_iZBm1);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
        }

        // edges (0,y,zmax) and (xmax,y,zmax)
        for (iY = 0; iY < m_iYBound; iY++)
        {
            i = index(0,iY,m_iZBm1);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
            i = index(m_iXBm1,iY,m_iZBm1);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
        }

        // edges (0,0,z) and (xmax,0,z)
        for (iZ = 0; iZ < m_iZBound; iZ++)
        {
            i = index(0,0,iZ);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
            i = index(m_iXBm1,0,iZ);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
        }

        // edges (0,ymax,z) and (xmax,ymax,z)
        for (iZ = 0; iZ < m_iZBound; iZ++)
        {
            i = index(0,m_iYBm1,iZ);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
            i = index(m_iXBm1,m_iYBm1,iZ);
            m_afInvSpeed[i] = Float.MAX_VALUE;
            m_afTime[i] = -Float.MAX_VALUE;
        }

        // Compute the first batch of trial voxels.  These are voxels a grid
        // distance of one away from the seed voxels.
        for (iZ = 1; iZ < m_iZBm1; iZ++)
        {
            for (iY = 1; iY < m_iYBm1; iY++)
            {
                for (iX = 1; iX < m_iXBm1; iX++)
                {
                    i = index(iX,iY,iZ);
                    if (isFar(i))
                    {
                        if ((isValid(i-1) && !isTrial(i-1))
                        ||  (isValid(i+1) && !isTrial(i+1))
                        ||  (isValid(i-m_iXBound) && !isTrial(i-m_iXBound))
                        ||  (isValid(i+m_iXBound) && !isTrial(i+m_iXBound))
                        ||  (isValid(i-m_iXYBound) && !isTrial(i-m_iXYBound))
                        ||  (isValid(i+m_iXYBound) && !isTrial(i+m_iXYBound)))
                        {
                            computeTime(i);
                            m_akTrial[i] = m_kHeap.insert(i,m_afTime[i]);
                        }
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

        boolean bHasZTerm;
        float fZConst;
        if (isValid(i-m_iXYBound))
        {
            bHasZTerm = true;
            fZConst = m_afTime[i-m_iXYBound];
            if (isValid(i+m_iXYBound))
            {
                if (m_afTime[i+m_iXYBound] < fZConst)
                {
                    fZConst = m_afTime[i+m_iXYBound];
                }
            }
        }
        else if (isValid(i+m_iXYBound))
        {
            bHasZTerm = true;
            fZConst = m_afTime[i+m_iXYBound];
        }
        else
        {
            bHasZTerm = false;
            fZConst = 0.0f;
        }

        float fSum, fDif, fDiscr;

        if (bHasXTerm)
        {
            if (bHasYTerm)
            {
                if (bHasZTerm)
                {
                    // xyz
                    fSum = fXConst + fYConst + fZConst;
                    fDiscr = 3.0f*m_afInvSpeed[i]*m_afInvSpeed[i];
                    fDif = fXConst - fYConst;
                    fDiscr -= fDif*fDif;
                    fDif = fXConst - fZConst;
                    fDiscr -= fDif*fDif;
                    fDif = fYConst - fZConst;
                    fDiscr -= fDif*fDif;
                    if (fDiscr >= 0.0f)
                    {
                        // The quadratic equation has a real-valued solution.
                        // Choose the largest positive root for the crossing
                        // time.
                        m_afTime[i] = (fSum +
                            (float)Math.sqrt((double)fDiscr))/3.0f;
                    }
                    else
                    {
                        // The quadratic equation does not have a real-valued
                        // solution.  This can happen when the speed is so
                        // large that the time gradient has very small length,
                        // which means that the time has not changed
                        // significantly from the neighbors to the current
                        // pixel.  Just choose the maximum time of the
                        // neighbors.  Is there a better choice?.
                        m_afTime[i] = fXConst;
                        if (fYConst > m_afTime[i])
                        {
                            m_afTime[i] = fYConst;
                        }
                        if (fZConst > m_afTime[i])
                        {
                            m_afTime[i] = fZConst;
                        }
                    }
                }
                else
                {
                    // xy
                    fSum = fXConst + fYConst;
                    fDif = fXConst - fYConst;
                    fDiscr = 2.0f*m_afInvSpeed[i]*m_afInvSpeed[i] - fDif*fDif;
                    if (fDiscr >= 0.0f)
                    {
                        // The quadratic equation has a real-valued solution.
                        // Choose the largest positive root for the crossing
                        // time.
                        m_afTime[i] = 0.5f*(fSum +
                            (float)Math.sqrt((double)fDiscr));
                    }
                    else
                    {
                        // The quadratic equation does not have a real-valued
                        // solution.  This can happen when the speed is so
                        // large that the time gradient has very small length,
                        // which means that the time has not changed
                        // significantly from the neighbors to the current
                        // pixel.  Just choose the maximum time of the
                        // neighbors.  Is there a better choice?.
                        m_afTime[i] = (fDif >= 0.0f ? fXConst : fYConst);
                    }
                }
            }
            else
            {
                if (bHasZTerm)
                {
                    // xz
                    fSum = fXConst + fZConst;
                    fDif = fXConst - fZConst;
                    fDiscr = 2.0f*m_afInvSpeed[i]*m_afInvSpeed[i] - fDif*fDif;
                    if (fDiscr >= 0.0f)
                    {
                        // The quadratic equation has a real-valued solution.
                        // Choose the largest positive root for the crossing
                        // time.
                        m_afTime[i] = 0.5f*(fSum +
                            (float)Math.sqrt((double)fDiscr));
                    }
                    else
                    {
                        // The quadratic equation does not have a real-valued
                        // solution.  This can happen when the speed is so
                        // large that the time gradient has very small length,
                        // which means that the time has not changed
                        // significantly from the neighbors to the current
                        // pixel.  Just choose the maximum time of the
                        // neighbors.  Is there a better choice?.
                        m_afTime[i] = (fDif >= 0.0f ? fXConst : fZConst);
                    }
                }
                else
                {
                    // x
                    m_afTime[i] = m_afInvSpeed[i] + fXConst;
                }
            }
        }
        else
        {
            if (bHasYTerm)
            {
                if (bHasZTerm)
                {
                    // yz
                    fSum = fYConst + fZConst;
                    fDif = fYConst - fZConst;
                    fDiscr = 2.0f*m_afInvSpeed[i]*m_afInvSpeed[i] - fDif*fDif;
                    if (fDiscr >= 0.0f)
                    {
                        // The quadratic equation has a real-valued solution.
                        // Choose the largest positive root for the crossing
                        // time.
                        m_afTime[i] = 0.5f*(fSum +
                            (float)Math.sqrt((double)fDiscr));
                    }
                    else
                    {
                        // The quadratic equation does not have a real-valued
                        // solution.  This can happen when the speed is so
                        // large that the time gradient has very small length,
                        // which means that the time has not changed
                        // significantly from the neighbors to the current
                        // pixel.  Just choose the maximum time of the
                        // neighbors.  Is there a better choice?.
                        m_afTime[i] = (fDif >= 0.0f ? fYConst : fZConst);
                    }
                }
                else
                {
                    // y
                    m_afTime[i] = m_afInvSpeed[i] + fYConst;
                }
            }
            else
            {
                if (bHasZTerm)
                {
                    // z
                    m_afTime[i] = m_afInvSpeed[i] + fZConst;
                }
                else
                {
                    // No terms, which cannot happen.  The voxel must have at
                    // least one valid neighbor.
                    // assert(false);
                }
            }
        }
    }
}
