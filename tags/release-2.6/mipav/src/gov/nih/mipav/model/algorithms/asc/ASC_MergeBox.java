package gov.nih.mipav.model.algorithms.asc;


/**
 * The box data structure for merging monoregions in the octree.
 */

public class ASC_MergeBox {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * A flag to indicate whether or not the box is valid. A box becomes invalid if it was merged with another box, that
     * other box being tagged as valid.
     */
    public boolean m_bValid;

    /** the lengths of the box edges. */
    public int m_iXStride, m_iYStride, m_iZStride;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a merge box with the specified stride. The box is initially a cube. As merging occurs, the box may become
     * noncubic. The location of the box is maintained by the merging algorithm in MjClimb3D.java.
     *
     * @param  iStride  DOCUMENT ME!
     */
    public ASC_MergeBox(int iStride) {
        m_iXStride = iStride;
        m_iYStride = iStride;
        m_iZStride = iStride;
        m_bValid = true;
    }
}
