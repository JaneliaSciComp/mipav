package gov.nih.mipav.model.algorithms.asc;


/**
 * The box data structure for the monoregions in the octree.
 */

public class ASC_OctBox {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** box edge lengths (m_iD? = m_i?1 - m_i?0) */
    public int m_iDX, m_iDY, m_iDZ;

    /** linear merge tree indices corresponding to the box edges. */
    public int m_iLX, m_iLY, m_iLZ;

    /** upper left corner of box. */
    public int m_iX0, m_iY0, m_iZ0;

    /** lower right corner of box. */
    public int m_iX1, m_iY1, m_iZ1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a box with the desired parameters.
     *
     * @param  iX0  x component of the upper left corner of the box
     * @param  iY0  y component of the upper left corner of the box
     * @param  iZ0  z component of the upper left corner of the box
     * @param  iDX  the box edge length in the x dim
     * @param  iDY  the box edge length in the y dim
     * @param  iDZ  the box edge length in the z dim
     * @param  iLX  x component of the linear merge tree indices corresponding to the edges of the box
     * @param  iLY  y component of the linear merge tree indices corresponding to the edges of the box
     * @param  iLZ  z component of the linear merge tree indices corresponding to the edges of the box
     */
    public ASC_OctBox(int iX0, int iY0, int iZ0, int iDX, int iDY, int iDZ, int iLX, int iLY, int iLZ) {
        m_iX0 = iX0;
        m_iY0 = iY0;
        m_iZ0 = iZ0;
        m_iX1 = iX0 + iDX;
        m_iY1 = iY0 + iDY;
        m_iZ1 = iZ0 + iDZ;
        m_iDX = iDX;
        m_iDY = iDY;
        m_iDZ = iDZ;
        m_iLX = iLX;
        m_iLY = iLY;
        m_iLZ = iLZ;
    }
}
