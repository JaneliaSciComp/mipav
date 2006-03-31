package gov.nih.mipav.model.algorithms.asc;

/**
 * The binary tree implementation that stores sign change information for
 * each line of an image.
 */

public class ASC_LinearMergeTree
{
    // The flags indicating if a 1D monoregion has no sign change (CFG_NONE),
    // a negative-to-positive sign change (CFG_INCR), a positive-to-negative
    // sign change (CFG_DECR), or multiple sign changes (CFG_MULT).
    static final int CFG_NONE = 0;
    static final int CFG_INCR = 1;
    static final int CFG_DECR = 2;
    static final int CFG_MULT = 3;
    static final int CFG_ROOT_MASK = 3;
    static final int CFG_EDGE = 4;
    static final int CFG_ZERO_SUBEDGE = 8;


    /**
     * Create a binary tree.
     *
     * @param iN The image line has power(2,iN)+1 samples.  The binary tree
     *    is stored as an array with power(2,iN+1)-1 elements.
     */
    public ASC_LinearMergeTree (int iN)
    {
        m_iN = iN;
        m_iTwoPowerN = (1 << m_iN);
        int iQuantity = 2*m_iTwoPowerN-1;
        m_aucNode = new byte[iQuantity];
        m_asZeroBase = new short[iQuantity];
    }

    /**
     * Compute the sign information for all the nodes of the binary tree.
     * The level set value should be a non-integer value.  This constraint
     * greatly reduces the algorithmic complexity for computing level sets.
     *
     * @param fLevel the desired level set value
     * @param afData the entire image (2D or 3D)
     * @param iOffset the starting point for the line of power(2,n)+1 values
     *    in the image
     * @param iStride the stride of the image samples that correspond to the
     *    image line (stride 1 for x direction, 2^n+1 for y direction,
     *    (2^n+1)^2 for z direction
     */
    final public void setLevel (float fLevel, float[] afData, int iOffset,
        int iStride)
    {
        // determine the sign changes between pairs of consecutive samples
        int iFirstLeaf = m_iTwoPowerN - 1;
        int i, iLeaf;
        for (i = 0, iLeaf = iFirstLeaf; i < m_iTwoPowerN; i++, iLeaf++)
        {
            int iBase = iOffset + iStride*i;
            float fV0 = afData[iBase];
            float fV1 = afData[iBase + iStride];

            if ( fV0 > fLevel )
            {
                if ( fV1 > fLevel )
                {
                    m_aucNode[iLeaf] = CFG_NONE;
                    m_asZeroBase[iLeaf] = -1;
                }
                else // fV1 < fLevel
                {
                    m_aucNode[iLeaf] = CFG_DECR;
                    m_asZeroBase[iLeaf] = (short)i;
                }
            }
            else // fV0 < fLevel
            {
                if ( fV1 > fLevel )
                {
                    m_aucNode[iLeaf] = CFG_INCR;
                    m_asZeroBase[iLeaf] = (short)i;
                }
                else // fV1 < fLevel
                {
                    m_aucNode[iLeaf] = CFG_NONE;
                    m_asZeroBase[iLeaf] = -1;
                }
            }
        }

        // propagate the sign change information up the binary tree
        for (i = iFirstLeaf-1; i >= 0; i--)
        {
            int iTwoIp1 = 2*i+1, iTwoIp2 = iTwoIp1+1;
            int iValue0 = (int)m_aucNode[iTwoIp1];
            int iValue1 = (int)m_aucNode[iTwoIp2];
            int iCombine = iValue0 | iValue1;
            m_aucNode[i] = (byte)iCombine;

            if ( iCombine == CFG_INCR )
            {
                if ( iValue0 == CFG_INCR )
                    m_asZeroBase[i] = m_asZeroBase[iTwoIp1];
                else
                    m_asZeroBase[i] = m_asZeroBase[iTwoIp2];
            }
            else if ( iCombine == CFG_DECR )
            {
                if ( iValue0 == CFG_DECR )
                    m_asZeroBase[i] = m_asZeroBase[iTwoIp1];
                else
                    m_asZeroBase[i] = m_asZeroBase[iTwoIp2];
            }
            else
            {
                m_asZeroBase[i] = -1;
            }
        }
    }

    /**
     * Get the sign information for the binary tree node.
     *
     * @param i the binary tree index for the node
     * @return the sign value (CFG_NONE, CFG_INCR, CFG_DECR, or CFG_MULT)
     */
    final public int getRootType (int i)
    {
        return (int)m_aucNode[i] & CFG_ROOT_MASK;
    }

    /**
     * Determine if the sign information for the binary tree node is "none".
     *
     * @param i the binary tree index for the node
     * @return 'true' if and only if the sign flag is CFG_NONE
     */
    final public boolean isNone (int i)
    {
        return ((int)m_aucNode[i] & CFG_ROOT_MASK) == CFG_NONE;
    }

    /**
     * Determine if the sign information for the binary tree node is "incr"
     * or "decr".
     *
     * @param i the binary tree index for the node
     * @return 'true' if and only if the sign flag is CFG_INCR or CFG_DECR
     */
    final public boolean isZeroEdge (int i)
    {
        return (int)m_aucNode[i] == (CFG_EDGE | CFG_INCR)
            || (int)m_aucNode[i] == (CFG_EDGE | CFG_DECR);
    }

    /**
     * Determine if the the binary tree node has a descendant with the
     * CFG_EDGE attribute.
     *
     * @param i the binary tree index for the node
     * @return 'true' if and only if a descendant has the CFG_EDGE attribute
     */
    final public boolean hasZeroSubedge (int i)
    {
        return ((int)m_aucNode[i] & CFG_ZERO_SUBEDGE) != 0;
    }

    /**
     * Get the image base index that is the end point of a 1-voxel wide
     * interval on which the image changes sign.
     *
     * @param i the binary tree index for the node
     * @return The base index.  If the returned value is -1, there is no sign
     *    change on the interval.  Otherwise the return value is nonnegative
     *    and the interval does have a sign change.
     */
    final public int getZeroBase (int i)
    {
        return (int)m_asZeroBase[i];
    }

    /**
     * Mark a node as an edge of a monoregion.  All ancestors are marked as
     * having a descendant that is an edge.
     *
     * @param i the binary tree index for the node
     */
    public void setEdge (int i)
    {
        m_aucNode[i] = (byte)((int)m_aucNode[i] | CFG_EDGE);

        // inform all predecessors if the have a zero subedge
        if ( m_asZeroBase[i] >= 0 )
        {
            while ( i > 0 )
            {
                i = (i-1)/2;
                m_aucNode[i] = (byte)((int)m_aucNode[i] | CFG_ZERO_SUBEDGE);
            }
        }
    }

    // binary tree parameters
    private int m_iN;
    private int m_iTwoPowerN;
    private byte[] m_aucNode;
    private short[] m_asZeroBase;
}
