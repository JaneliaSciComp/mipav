package gov.nih.mipav.model.algorithms.levelset;

/**
 * <p>The abstract base class for finite-difference-based solvers for partial
 * differential equations in 3D.  This system exists to support level-set
 * evolution for image segmentation.</p>
 * 
 * <p>The internal 3D data and mask images are copies of the inputs to the
 * constructor but padded with a 1-voxel thick border to support filtering on
 * the image boundary.  These images are of size
 * (xbound+2)-by-(ybound+2)-by-(zbound+2).  The inputs (x,y,z) to access the
 * data and mask avlues are constrained to 0 <= x < xbound, 0 <= y < ybound,
 * and 0 <= z < zbound.  The correct lookups into the padded arrays are
 * handled internally.</p>
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public abstract class LsePdeFilter3 extends LsePdeFilter
{
    //~ Instance fields ------------------------------------------------------

	/** The x-bound of the image. */
    protected int m_iXBound;
    
    /** The y-bound of the image. */
    protected int m_iYBound;
    
    /** The z-bound of the image. */
    protected int m_iZBound;
    
    /** The x-spacing of the image, call it dx. */
    protected float m_fXSpacing;
    
    /** The y-spacing of the image, call it dy. */
    protected float m_fYSpacing;
    
    /** The z-spacing of the image, call it dz. */
    protected float m_fZSpacing;
    
    /** invDx = 1/dx */
    protected float m_fInvDx;
    
    /** invDy = 1/dy */
    protected float m_fInvDy;
    
    /** invDz = 1/dz */
    protected float m_fInvDz;
    
    /** halfInvDx = 1/(2*dx) */
    protected float m_fHalfInvDx;
    
    /** halfInvDy = 1/(2*dy) */
    protected float m_fHalfInvDy;
    
    /** halfInvDz = 1/(2*dz) */
    protected float m_fHalfInvDz;
    
    /** invDxDx = 1/(dx*dx) */
    protected float m_fInvDxDx;
    
    /** invFourthDxDy = 1/(4*dx*dy) */
    protected float m_fFourthInvDxDy;
    
    /** invFourthDxDz = 1/(4*dx*dz) */
    protected float m_fFourthInvDxDz;
    
    /** invDyDy = 1/(dy*dy) */
    protected float m_fInvDyDy;
    
    /** invFourthDyDz = 1/(4*dy*dz) */
    protected float m_fFourthInvDyDz;
    
    /** invDzDz = 1/(dz*dz) */
    protected float m_fInvDzDz;

    /**
     * Temporary storage for 3x3x3 neighborhood.  In the notation m_fUijk, the
     * i, j, and k indices are in {m,z,p}, referring to subtract 1 (m), no
     * change (z), or add 1 (p) to the appropriate index.
     */
    protected float
    	m_fUmmm, m_fUzmm, m_fUpmm,
    	m_fUmzm, m_fUzzm, m_fUpzm,
    	m_fUmpm, m_fUzpm, m_fUppm,
    	m_fUmmz, m_fUzmz, m_fUpmz,
    	m_fUmzz, m_fUzzz, m_fUpzz,
    	m_fUmpz, m_fUzpz, m_fUppz,
    	m_fUmmp, m_fUzmp, m_fUpmp,
    	m_fUmzp, m_fUzzp, m_fUpzp,
    	m_fUmpp, m_fUzpp, m_fUppp;

    /**
     * Successive iterations of the PDE solver toggle between two buffers.
     * This is the source buffer, which is the input to the solver.
     */
    protected float[][][] m_aaafSrc;
    
    /**
     * Successive iterations of the PDE solver toggle between two buffers.
     * This is the destination buffer, which is the output of the solver.
     */
    protected float[][][] m_aaafDst;
    
    /**
     * Voxels may be masked out so that the PDE solver does not process them.
     * This is useful for filtering only those voxels in a subimage.
     */
    protected boolean[][][] m_aaabMask;

    //~ Constructors ---------------------------------------------------------

    /**
     * Create a new PDE filter object for 3D images.
     * 
     * @param iXBound The x-bound of the image.
     * @param iYBound The y-bound of the image.
     * @param iZBound The z-bound of the image.
     * @param fXSpacing The x-spacing of the image.
     * @param fYSpacing The y-spacing of the image.
     * @param fZSpacing The z-spacing of the image.
     * @param afData The image elements, stored in lexicographical order.
     * @param abMask The image mask, stored in lexicographical order.  A
     *     voxel value is processed by the PDE solver only when the mask value
     *     is false.
     * @param fBorderValue Specifies how to handle the image value.  When set
     *     to Float.MAX_VALUE, Neumann conditions are in use, in which case
     *     zero-valued derivatives are assumed on the image border.  Otherwise,
     *     Dirichlet conditions are used, in which case the image is assumed
     *     to be constant on the border with value specified by fBorderValue.
     * @param eScaleType The type of scaling to apply to the input image.
     *     The choices are NONE, UNIT, SYMMETRIC, or PRESERVE_ZERO.
     */
    protected LsePdeFilter3 (int iXBound, int iYBound, int iZBound,
        float fXSpacing, float fYSpacing, float fZSpacing, float[] afData,
        boolean[] abMask, float fBorderValue, int eScaleType)
    {
        super(iXBound*iYBound*iZBound,afData,fBorderValue,eScaleType);

        m_iXBound = iXBound;
        m_iYBound = iYBound;
        m_iZBound = iZBound;
        m_fXSpacing = fXSpacing;
        m_fYSpacing = fYSpacing;
        m_fZSpacing = fZSpacing;
        m_fInvDx = 1.0f/fXSpacing;
        m_fInvDy = 1.0f/fYSpacing;
        m_fInvDz = 1.0f/fZSpacing;
        m_fHalfInvDx = 0.5f*m_fInvDx;
        m_fHalfInvDy = 0.5f*m_fInvDy;
        m_fHalfInvDz = 0.5f*m_fInvDz;
        m_fInvDxDx = m_fInvDx*m_fInvDx;
        m_fFourthInvDxDy = m_fHalfInvDx*m_fHalfInvDy;
        m_fFourthInvDxDz = m_fHalfInvDx*m_fHalfInvDz;
        m_fInvDyDy = m_fInvDy*m_fInvDy;
        m_fFourthInvDyDz = m_fHalfInvDy*m_fHalfInvDz;
        m_fInvDzDz = m_fInvDz*m_fInvDz;

        // Create two buffers for filtering, one a source buffer and one a
        // destination buffer.  Their roles are reversed on each iteration of
        // the filter.
        int iXBp2 = m_iXBound + 2;
        int iYBp2 = m_iYBound + 2;
        int iZBp2 = m_iZBound + 2;
        int iX, iY, iZ;
        m_aaafSrc = new float[iZBp2][iYBp2][iXBp2];
        m_aaafDst = new float[iZBp2][iYBp2][iXBp2];
        if (abMask != null)
        {
            m_aaabMask = new boolean[iZBp2][iYBp2][iXBp2];

            // This sets all mask values to 'false' initially.  The interior
            // mask values will be filled in later using the input abMask.
            // The boundary mask values will remain 'false'.
            for (iZ = 0; iZ < iZBp2; iZ++)
            {
                for (iY = 0; iY < iYBp2; iY++)
                {
                    for (iX = 0; iX < iXBp2; iX++)
                    {
                        m_aaabMask[iZ][iY][iX] = false;
                    }
                }
            }
        }
        else
        {
            m_aaabMask = null;
        }

        // Copy the inputs afData and abMask to the interior elements of
        // m_aaafSrc and m_aaabMask.
        int iXp, iYp, iZp, i;
        for (iZ = 0, iZp = 1, i = 0; iZ < m_iZBound; iZ++, iZp++)
        {
            for (iY = 0, iYp = 1; iY < m_iYBound; iY++, iYp++) 
            {
                for (iX = 0, iXp = 1; iX < m_iXBound; iX++, iXp++, i++)
                {
                    m_aaafSrc[iZp][iYp][iXp] = m_fOffset +
                        (afData[i] - m_fMin) *  m_fScale;

                    if (m_aaabMask != null)
                    {
                        m_aaabMask[iZp][iYp][iXp] = abMask[i];
                    }
                }
            }
        }

        // Assign values to the 1-voxel thick border.
        if (m_fBorderValue != Float.MAX_VALUE)
        {
            assignDirichletImageBorder();
        }
        else
        {
            assignNeumannImageBorder();
        }

        // To handle masks that do not cover the entire image, assign values
        // to those voxels that are 26-neighbors of the mask voxels.
        if (m_aaabMask != null)
        {
            if (m_fBorderValue != Float.MAX_VALUE)
            {
                assignDirichletMaskBorder();
            }
            else
            {
                assignNeumannMaskBorder();
            }
        }
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
    * Get the x-spacing for the image.
    * 
    * @return The x-spacing for the image.
    */
    public final float getXSpacing ()
    {
        return m_fXSpacing;
    }

    /**
     * Get the y-spacing for the image.
     * 
     * @return The y-spacing for the image.
     */
    public final float getYSpacing ()
    {
        return m_fYSpacing;
    }

    /**
     * Get the z-spacing for the image.
     * 
     * @return The z-spacing for the image.
     */
    public final float getZSpacing ()
    {
        return m_fZSpacing;
    }

    /**
     * Get the image value at voxel (x,y,z), call it u(x,y,z).
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The image value, u(x,y,z).
     */
    public final float getU (int iX, int iY, int iZ)
    {
        return m_aaafSrc[iZ+1][iY+1][iX+1];
    }

    /**
     * Get the first-order x-derivative of the image value at voxel (x,y,z),
     * call it u_x(x,y,z).
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The x-derivative of the image value, u_x(x,y,z).
     */
    public final float getUx (int iX, int iY, int iZ)
    {
        int iYp1 = iY + 1, iZp1 = iZ + 1;
        return m_fHalfInvDx*(m_aaafSrc[iZp1][iYp1][iX+2] -
            m_aaafSrc[iZp1][iYp1][iX]);
    }

    /**
     * Get the first-order y-derivative of the image value at voxel (x,y,z),
     * call it u_y(x,y,z).
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The y-derivative of the image value, u_y(x,y,z).
     */
    public final float getUy (int iX, int iY, int iZ)
    {
        int iXp1 = iX + 1, iZp1 = iZ + 1;
        return m_fHalfInvDy*(m_aaafSrc[iZp1][iY+2][iXp1] -
            m_aaafSrc[iZp1][iY][iXp1]);
    }

    /**
     * Get the first-order z-derivative of the image value at voxel (x,y,z),
     * call it u_z(x,y,z).
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The z-derivative of the image value, u_z(x,y,z).
     */
    public final float getUz (int iX, int iY, int iZ)
    {
        int iXp1 = iX + 1, iYp1 = iY + 1;
        return m_fHalfInvDz*(m_aaafSrc[iZ+2][iYp1][iXp1] -
            m_aaafSrc[iZ][iYp1][iXp1]);
    }

    /**
     * Get the second-order x-derivative of the image value at voxel (x,y,z),
     * call it u_xx(x,y,z).
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The xx-derivative of the image value, u_xx(x,y).
     */
    public final float getUxx (int iX, int iY, int iZ)
    {
        int iYp1 = iY + 1, iZp1 = iZ + 1;
        return m_fInvDxDx*(m_aaafSrc[iZp1][iYp1][iX+2] -
            2.0f*m_aaafSrc[iZp1][iYp1][iX+1] + m_aaafSrc[iZp1][iYp1][iX]);
    }

    /**
     * Get the second-order xy-mixed-derivative of the image value at voxel
     * (x,y,z), call it u_xy(x,y,z).
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The xy-derivative of the image value, u_xy(x,y,z).
     */
    public final float getUxy (int iX, int iY, int iZ)
    {
        int iXp2 = iX + 2, iYp2 = iY + 2, iZp1 = iZ + 1;
        return m_fFourthInvDxDy*(m_aaafSrc[iZp1][iY][iX] -
            m_aaafSrc[iZp1][iY][iXp2] + m_aaafSrc[iZp1][iYp2][iXp2] -
            m_aaafSrc[iZp1][iYp2][iX]);
    }

    /**
     * Get the second-order xz-mixed-derivative of the image value at voxel
     * (x,y,z), call it u_xz(x,y,z).
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The xz-derivative of the image value, u_xz(x,y,z).
     */
    public final float getUxz (int iX, int iY, int iZ)
    {
        int iXp2 = iX + 2, iYp1 = iY + 1, iZp2 = iZ + 2;
        return m_fFourthInvDxDz*(m_aaafSrc[iZ][iYp1][iX] -
            m_aaafSrc[iZ][iYp1][iXp2] + m_aaafSrc[iZp2][iYp1][iXp2] -
            m_aaafSrc[iZp2][iYp1][iX]);
    }

    /**
     * Get the second-order y-derivative of the image value at voxel (x,y,z),
     * call it u_yy(x,y,z).
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The yy-derivative of the image value, u_yy(x,y).
     */
    public final float getUyy (int iX, int iY, int iZ)
    {
        int iXp1 = iX + 1, iZp1 = iZ + 1;
        return m_fInvDyDy*(m_aaafSrc[iZp1][iY+2][iXp1] -
            2.0f*m_aaafSrc[iZp1][iY+1][iXp1] + m_aaafSrc[iZp1][iY][iXp1]);
    }

    /**
     * Get the second-order yz-mixed-derivative of the image value at voxel
     * (x,y,z), call it u_yz(x,y,z).
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The yz-derivative of the image value, u_yz(x,y,z).
     */
    public final float getUyz (int iX, int iY, int iZ)
    {
        int iXp1 = iX + 1, iYp2 = iY + 2, iZp2 = iZ + 2;
        return m_fFourthInvDyDz*(m_aaafSrc[iZ][iY][iXp1] -
            m_aaafSrc[iZ][iYp2][iXp1] + m_aaafSrc[iZp2][iYp2][iXp1] -
            m_aaafSrc[iZp2][iY][iXp1]);
    }

    /**
     * Get the second-order z-derivative of the image value at voxel (x,y,z),
     * call it u_zz(x,y,z).
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The zz-derivative of the image value, u_zz(x,y).
     */
    public final float getUzz (int iX, int iY, int iZ)
    {
        int iXp1 = iX + 1, iYp1 = iY + 1;
        return m_fInvDzDz*(m_aaafSrc[iZ+2][iYp1][iXp1] -
            2.0f*m_aaafSrc[iZ+1][iYp1][iXp1] + m_aaafSrc[iZ][iYp1][iXp1]);
    }

    /**
     * Get the mask value at voxel (x,y,z).
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The mask value at voxel (x,y,z).  Its value is true when that
     *     pixel should not be processed by the PDE solver.
     */
    public final boolean getMask (int iX, int iY, int iZ)
    {
         return (m_aaabMask != null ? m_aaabMask[iZ+1][iY+1][iX+1] : true);
    }

    /**
     * Assign values to the 1-voxel-thick image border.  The border value has
     * been specified during object construction.
     */
    protected void assignDirichletImageBorder ()
    {
        int iXBp1 = m_iXBound + 1, iYBp1 = m_iYBound + 1, iZBp1 = m_iZBound + 1;
        int iX, iY, iZ;

        // vertex (0,0,0)
        m_aaafSrc[0][0][0] = m_fBorderValue;
        m_aaafDst[0][0][0] = m_fBorderValue;
        if (m_aaabMask != null)
        {
            m_aaabMask[0][0][0] = false;
        }

        // vertex (xmax,0,0)
        m_aaafSrc[0][0][iXBp1] = m_fBorderValue;
        m_aaafDst[0][0][iXBp1] = m_fBorderValue;
        if (m_aaabMask != null)
        {
            m_aaabMask[0][0][iXBp1] = false;
        }

        // vertex (0,ymax,0)
        m_aaafSrc[0][iYBp1][0] = m_fBorderValue;
        m_aaafDst[0][iYBp1][0] = m_fBorderValue;
        if (m_aaabMask != null)
        {
            m_aaabMask[0][iYBp1][0] = false;
        }

        // vertex (xmax,ymax,0)
        m_aaafSrc[0][iYBp1][iXBp1] = m_fBorderValue;
        m_aaafDst[0][iYBp1][iXBp1] = m_fBorderValue;
        if (m_aaabMask != null)
        {
            m_aaabMask[0][iYBp1][iXBp1] = false;
        }

        // vertex (0,0,zmax)
        m_aaafSrc[iZBp1][0][0] = m_fBorderValue;
        m_aaafDst[iZBp1][0][0] = m_fBorderValue;
        if (m_aaabMask != null)
        {
            m_aaabMask[iZBp1][0][0] = false;
        }

        // vertex (xmax,0,zmax)
        m_aaafSrc[iZBp1][0][iXBp1] = m_fBorderValue;
        m_aaafDst[iZBp1][0][iXBp1] = m_fBorderValue;
        if (m_aaabMask != null)
        {
            m_aaabMask[iZBp1][0][iXBp1] = false;
        }

        // vertex (0,ymax,zmax)
        m_aaafSrc[iZBp1][iYBp1][0] = m_fBorderValue;
        m_aaafDst[iZBp1][iYBp1][0] = m_fBorderValue;
        if (m_aaabMask != null)
        {
            m_aaabMask[iZBp1][iYBp1][0] = false;
        }

        // vertex (xmax,ymax,zmax)
        m_aaafSrc[iZBp1][iYBp1][iXBp1] = m_fBorderValue;
        m_aaafDst[iZBp1][iYBp1][iXBp1] = m_fBorderValue;
        if (m_aaabMask != null)
        {
            m_aaabMask[iZBp1][iYBp1][iXBp1] = false;
        }

        // edges (x,0,0) and (x,ymax,0)
        for (iX = 1; iX <= m_iXBound; iX++)
        {
            m_aaafSrc[0][0][iX] = m_fBorderValue;
            m_aaafDst[0][0][iX] = m_fBorderValue;
            if (m_aaabMask != null)
            {
                m_aaabMask[0][0][iX] = false;
            }

            m_aaafSrc[0][iYBp1][iX] = m_fBorderValue;
            m_aaafDst[0][iYBp1][iX] = m_fBorderValue;
            if (m_aaabMask != null)
            {
                m_aaabMask[0][iYBp1][iX] = false;
            }
        }

        // edges (0,y,0) and (xmax,y,0)
        for (iY = 1; iY <= m_iYBound; iY++)
        {
            m_aaafSrc[0][iY][0] = m_fBorderValue;
            m_aaafDst[0][iY][0] = m_fBorderValue;
            if (m_aaabMask != null)
            {
                m_aaabMask[0][iY][0] = false;
            }

            m_aaafSrc[0][iY][iXBp1] = m_fBorderValue;
            m_aaafDst[0][iY][iXBp1] = m_fBorderValue;
            if (m_aaabMask != null)
            {
                m_aaabMask[0][iY][iXBp1] = false;
            }
        }

        // edges (x,0,zmax) and (x,ymax,zmax)
        for (iX = 1; iX <= m_iXBound; iX++)
        {
            m_aaafSrc[iZBp1][0][iX] = m_fBorderValue;
            m_aaafDst[iZBp1][0][iX] = m_fBorderValue;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZBp1][0][iX] = false;
            }

            m_aaafSrc[iZBp1][iYBp1][iX] = m_fBorderValue;
            m_aaafDst[iZBp1][iYBp1][iX] = m_fBorderValue;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZBp1][iYBp1][iX] = false;
            }
        }

        // edges (0,y,zmax) and (xmax,y,zmax)
        for (iY = 1; iY <= m_iYBound; iY++)
        {
            m_aaafSrc[iZBp1][iY][0] = m_fBorderValue;
            m_aaafDst[iZBp1][iY][0] = m_fBorderValue;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZBp1][iY][0] = false;
            }

            m_aaafSrc[iZBp1][iY][iXBp1] = m_fBorderValue;
            m_aaafDst[iZBp1][iY][iXBp1] = m_fBorderValue;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZBp1][iY][iXBp1] = false;
            }
        }

        // edges (0,0,z) and (xmax,0,z)
        for (iZ = 1; iZ <= m_iZBound; iZ++)
        {
            m_aaafSrc[iZ][0][0] = m_fBorderValue;
            m_aaafDst[iZ][0][0] = m_fBorderValue;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZ][0][0] = false;
            }

            m_aaafSrc[iZ][0][iXBp1] = m_fBorderValue;
            m_aaafDst[iZ][0][iXBp1] = m_fBorderValue;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZ][0][iXBp1] = false;
            }
        }

        // edges (0,ymax,z) and (xmax,ymax,z)
        for (iZ = 1; iZ <= m_iZBound; iZ++)
        {
            m_aaafSrc[iZ][iYBp1][0] = m_fBorderValue;
            m_aaafDst[iZ][iYBp1][0] = m_fBorderValue;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZ][iYBp1][0] = false;
            }

            m_aaafSrc[iZ][iYBp1][iXBp1] = m_fBorderValue;
            m_aaafDst[iZ][iYBp1][iXBp1] = m_fBorderValue;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZ][iYBp1][iXBp1] = false;
            }
        }

        // faces (x,y,0) and (x,y,zmax)
        for (iY = 1; iY <= m_iYBound; iY++)
        {
            for (iX = 1; iX <= m_iXBound; iX++)
            {
                m_aaafSrc[0][iY][iX] = m_fBorderValue;
                m_aaafDst[0][iY][iX] = m_fBorderValue;
                if (m_aaabMask != null)
                {
                    m_aaabMask[0][iY][iX] = false;
                }

                m_aaafSrc[iZBp1][iY][iX] = m_fBorderValue;
                m_aaafDst[iZBp1][iY][iX] = m_fBorderValue;
                if (m_aaabMask != null)
                {
                    m_aaabMask[iZBp1][iY][iX] = false;
                }
            }
        }

        // faces (x,0,z) and (x,ymax,z)
        for (iZ = 1; iZ <= m_iZBound; iZ++)
        {
            for (iX = 1; iX <= m_iXBound; iX++)
            {
                m_aaafSrc[iZ][0][iX] = m_fBorderValue;
                m_aaafDst[iZ][0][iX] = m_fBorderValue;
                if (m_aaabMask != null)
                {
                    m_aaabMask[iZ][0][iX] = false;
                }

                m_aaafSrc[iZ][iYBp1][iX] = m_fBorderValue;
                m_aaafDst[iZ][iYBp1][iX] = m_fBorderValue;
                if (m_aaabMask != null)
                {
                    m_aaabMask[iZ][iYBp1][iX] = false;
                }
            }
        }

        // faces (0,y,z) and (xmax,y,z)
        for (iZ = 1; iZ <= m_iZBound; iZ++)
        {
            for (iY = 1; iY <= m_iYBound; iY++)
            {
                m_aaafSrc[iZ][iY][0] = m_fBorderValue;
                m_aaafDst[iZ][iY][0] = m_fBorderValue;
                if (m_aaabMask != null)
                {
                    m_aaabMask[iZ][iY][0] = false;
                }

                m_aaafSrc[iZ][iY][iXBp1] = m_fBorderValue;
                m_aaafDst[iZ][iY][iXBp1] = m_fBorderValue;
                if (m_aaabMask != null)
                {
                    m_aaabMask[iZ][iY][iXBp1] = false;
                }
            }
        }
    }

    /**
     * Assign values to the 1-voxel-thick image border.  The border values are
     * chosen so that the first-order image derivatives at the border pixels
     * are zero.  Thus, the border values are duplicates of the adjacent image
     * values.
     */
    protected void assignNeumannImageBorder ()
    {
        int iXBp1 = m_iXBound + 1, iYBp1 = m_iYBound + 1, iZBp1 = m_iZBound + 1;
        int iX, iY, iZ;
        float fDuplicate;

        // vertex (0,0,0)
        fDuplicate = m_aaafSrc[1][1][1];
        m_aaafSrc[0][0][0] = fDuplicate;
        m_aaafDst[0][0][0] = fDuplicate;
        if (m_aaabMask != null)
        {
            m_aaabMask[0][0][0] = false;
        }

        // vertex (xmax,0,0)
        fDuplicate = m_aaafSrc[1][1][m_iXBound];
        m_aaafSrc[0][0][iXBp1] = fDuplicate;
        m_aaafDst[0][0][iXBp1] = fDuplicate;
        if (m_aaabMask != null)
        {
            m_aaabMask[0][0][iXBp1] = false;
        }

        // vertex (0,ymax,0)
        fDuplicate = m_aaafSrc[1][m_iYBound][1];
        m_aaafSrc[0][iYBp1][0] = fDuplicate;
        m_aaafDst[0][iYBp1][0] = fDuplicate;
        if (m_aaabMask != null)
        {
            m_aaabMask[0][iYBp1][0] = false;
        }

        // vertex (xmax,ymax,0)
        fDuplicate = m_aaafSrc[1][m_iYBound][m_iXBound];
        m_aaafSrc[0][iYBp1][iXBp1] = fDuplicate;
        m_aaafDst[0][iYBp1][iXBp1] = fDuplicate;
        if (m_aaabMask != null)
        {
            m_aaabMask[0][iYBp1][iXBp1] = false;
        }

        // vertex (0,0,zmax)
        fDuplicate = m_aaafSrc[m_iZBound][1][1];
        m_aaafSrc[iZBp1][0][0] = fDuplicate;
        m_aaafDst[iZBp1][0][0] = fDuplicate;
        if (m_aaabMask != null)
        {
            m_aaabMask[iZBp1][0][0] = false;
        }

        // vertex (xmax,0,zmax)
        fDuplicate = m_aaafSrc[m_iZBound][1][m_iXBound];
        m_aaafSrc[iZBp1][0][iXBp1] = fDuplicate;
        m_aaafDst[iZBp1][0][iXBp1] = fDuplicate;
        if (m_aaabMask != null)
        {
            m_aaabMask[iZBp1][0][iXBp1] = false;
        }

        // vertex (0,ymax,zmax)
        fDuplicate = m_aaafSrc[m_iZBound][m_iYBound][1];
        m_aaafSrc[iZBp1][iYBp1][0] = fDuplicate;
        m_aaafDst[iZBp1][iYBp1][0] = fDuplicate;
        if (m_aaabMask != null)
        {
            m_aaabMask[iZBp1][iYBp1][0] = false;
        }

        // vertex (xmax,ymax,zmax)
        fDuplicate = m_aaafSrc[m_iZBound][m_iYBound][m_iXBound];
        m_aaafSrc[iZBp1][iYBp1][iXBp1] = fDuplicate;
        m_aaafDst[iZBp1][iYBp1][iXBp1] = fDuplicate;
        if (m_aaabMask != null)
        {
            m_aaabMask[iZBp1][iYBp1][iXBp1] = false;
        }

        // edges (x,0,0) and (x,ymax,0)
        for (iX = 1; iX <= m_iXBound; iX++)
        {
            fDuplicate = m_aaafSrc[1][1][iX];
            m_aaafSrc[0][0][iX] = fDuplicate;
            m_aaafDst[0][0][iX] = fDuplicate;
            if (m_aaabMask != null)
            {
                m_aaabMask[0][0][iX] = false;
            }

            fDuplicate = m_aaafSrc[1][m_iYBound][iX];
            m_aaafSrc[0][iYBp1][iX] = fDuplicate;
            m_aaafDst[0][iYBp1][iX] = fDuplicate;
            if (m_aaabMask != null)
            {
                m_aaabMask[0][iYBp1][iX] = false;
            }
        }

        // edges (0,y,0) and (xmax,y,0)
        for (iY = 1; iY <= m_iYBound; iY++)
        {
            fDuplicate = m_aaafSrc[1][iY][1];
            m_aaafSrc[0][iY][0] = fDuplicate;
            m_aaafDst[0][iY][0] = fDuplicate;
            if (m_aaabMask != null)
            {
                m_aaabMask[0][iY][0] = false;
            }

            fDuplicate = m_aaafSrc[1][iY][m_iXBound];
            m_aaafSrc[0][iY][iXBp1] = fDuplicate;
            m_aaafDst[0][iY][iXBp1] = fDuplicate;
            if (m_aaabMask != null)
            {
                m_aaabMask[0][iY][iXBp1] = false;
            }
        }

        // edges (x,0,zmax) and (x,ymax,zmax)
        for (iX = 1; iX <= m_iXBound; iX++)
        {
            fDuplicate = m_aaafSrc[m_iZBound][1][iX];
            m_aaafSrc[iZBp1][0][iX] = fDuplicate;
            m_aaafDst[iZBp1][0][iX] = fDuplicate;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZBp1][0][iX] = false;
            }

            fDuplicate = m_aaafSrc[m_iZBound][m_iYBound][iX];
            m_aaafSrc[iZBp1][iYBp1][iX] = fDuplicate;
            m_aaafDst[iZBp1][iYBp1][iX] = fDuplicate;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZBp1][iYBp1][iX] = false;
            }
        }

        // edges (0,y,zmax) and (xmax,y,zmax)
        for (iY = 1; iY <= m_iYBound; iY++)
        {
            fDuplicate = m_aaafSrc[m_iZBound][iY][1];
            m_aaafSrc[iZBp1][iY][0] = fDuplicate;
            m_aaafDst[iZBp1][iY][0] = fDuplicate;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZBp1][iY][0] = false;
            }

            fDuplicate = m_aaafSrc[m_iZBound][iY][m_iXBound];
            m_aaafSrc[iZBp1][iY][iXBp1] = fDuplicate;
            m_aaafDst[iZBp1][iY][iXBp1] = fDuplicate;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZBp1][iY][iXBp1] = false;
            }
        }

        // edges (0,0,z) and (xmax,0,z)
        for (iZ = 1; iZ <= m_iZBound; iZ++)
        {
            fDuplicate = m_aaafSrc[iZ][1][1];
            m_aaafSrc[iZ][0][0] = fDuplicate;
            m_aaafDst[iZ][0][0] = fDuplicate;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZ][0][0] = false;
            }

            fDuplicate = m_aaafSrc[iZ][1][m_iXBound];
            m_aaafSrc[iZ][0][iXBp1] = fDuplicate;
            m_aaafDst[iZ][0][iXBp1] = fDuplicate;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZ][0][iXBp1] = false;
            }
        }

        // edges (0,ymax,z) and (xmax,ymax,z)
        for (iZ = 1; iZ <= m_iZBound; iZ++)
        {
            fDuplicate = m_aaafSrc[iZ][m_iYBound][1];
            m_aaafSrc[iZ][iYBp1][0] = fDuplicate;
            m_aaafDst[iZ][iYBp1][0] = fDuplicate;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZ][iYBp1][0] = false;
            }

            fDuplicate = m_aaafSrc[iZ][m_iYBound][m_iXBound];
            m_aaafSrc[iZ][iYBp1][iXBp1] = fDuplicate;
            m_aaafDst[iZ][iYBp1][iXBp1] = fDuplicate;
            if (m_aaabMask != null)
            {
                m_aaabMask[iZ][iYBp1][iXBp1] = false;
            }
        }

        // faces (x,y,0) and (x,y,zmax)
        for (iY = 1; iY <= m_iYBound; iY++)
        {
            for (iX = 1; iX <= m_iXBound; iX++)
            {
                fDuplicate = m_aaafSrc[1][iY][iX];
                m_aaafSrc[0][iY][iX] = fDuplicate;
                m_aaafDst[0][iY][iX] = fDuplicate;
                if (m_aaabMask != null)
                {
                    m_aaabMask[0][iY][iX] = false;
                }

                fDuplicate = m_aaafSrc[m_iZBound][iY][iX];
                m_aaafSrc[iZBp1][iY][iX] = fDuplicate;
                m_aaafDst[iZBp1][iY][iX] = fDuplicate;
                if (m_aaabMask != null)
                {
                    m_aaabMask[iZBp1][iY][iX] = false;
                }
            }
        }

        // faces (x,0,z) and (x,ymax,z)
        for (iZ = 1; iZ <= m_iZBound; iZ++)
        {
            for (iX = 1; iX <= m_iXBound; iX++)
            {
                fDuplicate = m_aaafSrc[iZ][1][iX];
                m_aaafSrc[iZ][0][iX] = fDuplicate;
                m_aaafDst[iZ][0][iX] = fDuplicate;
                if (m_aaabMask != null)
                {
                    m_aaabMask[iZ][0][iX] = false;
                }

                fDuplicate = m_aaafSrc[iZ][m_iYBound][iX];
                m_aaafSrc[iZ][iYBp1][iX] = fDuplicate;
                m_aaafDst[iZ][iYBp1][iX] = fDuplicate;
                if (m_aaabMask != null)
                {
                    m_aaabMask[iZ][iYBp1][iX] = false;
                }
            }
        }

        // faces (0,y,z) and (xmax,y,z)
        for (iZ = 1; iZ <= m_iZBound; iZ++)
        {
            for (iY = 1; iY <= m_iYBound; iY++)
            {
                fDuplicate = m_aaafSrc[iZ][iY][1];
                m_aaafSrc[iZ][iY][0] = fDuplicate;
                m_aaafDst[iZ][iY][0] = fDuplicate;
                if (m_aaabMask != null)
                {
                    m_aaabMask[iZ][iY][0] = false;
                }

                fDuplicate = m_aaafSrc[iZ][iY][m_iXBound];
                m_aaafSrc[iZ][iY][iXBp1] = fDuplicate;
                m_aaafDst[iZ][iY][iXBp1] = fDuplicate;
                if (m_aaabMask != null)
                {
                    m_aaabMask[iZ][iY][iXBp1] = false;
                }
            }
        }
    }

    /**
     * Assign values to the 1-voxel-thick borders that surround unmasked
     * regions.  The border value has been specified during object
     * construction.
     */
    protected void assignDirichletMaskBorder ()
    {
        // assert(m_aaabMask != null);

        for (int iZ = 1; iZ <= m_iZBound; iZ++)
        {
            for (int iY = 1; iY <= m_iYBound; iY++)
            {
                for (int iX = 1; iX <= m_iXBound; iX++)
                {
                    if (m_aaabMask[iZ][iY][iX])
                    {
                        continue;
                    }

                    boolean bFound = false;
                    int i0, i1, i2, iLX, iLY, iLZ;
                    for (i2 = 0, iLZ = iZ-1; i2 < 3 && !bFound; i2++, iLZ++)
                    {
                        for (i1 = 0, iLY = iY-1; i1 < 3 && !bFound; i1++, iLY++)
                        {
                            for (i0 = 0, iLX = iX-1; i0 < 3; i0++, iLX++)
                            {
                                if (m_aaabMask[iLZ][iLY][iLX])
                                {
                                    m_aaafSrc[iZ][iY][iX] = m_fBorderValue;
                                    m_aaafDst[iZ][iY][iX] = m_fBorderValue;
                                    bFound = true;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Assign values to the 1-voxel-thick borders that surround unmasked
     * regions.  The border values are chosen so that the first-order image
     * derivatives at the border voxels are zero.  Thus, the border values are
     * duplicates of the adjacent image values.
     */
    protected void assignNeumannMaskBorder ()
    {
        // Recompute the values just outside the masked region.  This
        // guarantees that derivative estimations use the current values
        // around the boundary.
        for (int iZ = 1; iZ <= m_iZBound; iZ++)
        {
            for (int iY = 1; iY <= m_iYBound; iY++)
            {
                for (int iX = 1; iX <= m_iXBound; iX++)
                {
                    if (m_aaabMask[iZ][iY][iX])
                    {
                        continue;
                    }

                    int iCount = 0;
                    float fAverage = 0.0f;
                    int i0, i1, i2, iLX, iLY, iLZ;
                    for (i2 = 0, iLZ = iZ-1; i2 < 3; i2++, iLZ++)
                    {
                        for (i1 = 0, iLY = iY-1; i1 < 3; i1++, iLY++)
                        {
                            for (i0 = 0, iLX = iX-1; i0 < 3; i0++, iLX++)
                            {
                                if (m_aaabMask[iLZ][iLY][iLX])
                                {
                                    fAverage += m_aaafSrc[iLZ][iLY][iLX];
                                    iCount++;
                                }
                            }
                        }
                    }

                    if (iCount > 0)
                    {
                        fAverage /= (float)iCount;
                        m_aaafSrc[iZ][iY][iX] = fAverage;
                        m_aaafDst[iZ][iY][iX] = fAverage;
                    }
                }
            }
        }
    }

    /**
     * Recompute the boundary values when Neumann conditions are in effect.
     * If a derived class overrides this, it must call the base-class
     * onPreUpdate first.
     */
    protected void onPreUpdate ()
    {
        if (m_aaabMask != null && m_fBorderValue == Float.MAX_VALUE)
        {
            // Neumann boundary conditions are in use, so recompute the mask
            // border.
            assignNeumannMaskBorder();
        }
        // else: No mask has been specified or Dirichlet boundary conditions
        // are in use.  Nothing to do.
    }

    /**
     * Iterate over all the voxels and call onUpdate(x,y,z) for each voxel
     * that is not masked out.
     */
    protected void onUpdate ()
    {
        for (int iZ = 1; iZ <= m_iZBound; iZ++)
        {
            for (int iY = 1; iY <= m_iYBound; iY++)
            {
                for (int iX = 1; iX <= m_iXBound; iX++)
                {
                    if (m_aaabMask == null || m_aaabMask[iZ][iY][iX])
                    {
                        onUpdate(iX,iY,iZ);
                    }
                }
            }
        }
    }

    /**
     * Swap the buffers for the next pass of the PDE solver.  If a derived
     * class overrides this, it must call the base-class onPostUpdate last.
     */
    protected void onPostUpdate ()
    {
        // Swap the buffers for the next pass.
        float[][][] aaafSave = m_aaafSrc;
        m_aaafSrc = m_aaafDst;
        m_aaafDst = aaafSave;
    }

    /**
     * An abstract function that allows per-voxel processing by the PDE
     * solver.  The voxel (x,y,z) must be in padded coordinates, namely,
     * 1 <= x <= xbound, 1 <= y <= ybound, and 1 <= z <= zbound.
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     */
    protected abstract void onUpdate (int iX, int iY, int iZ);

    /**
     * Copy the source data to temporary storage in order to avoid redundant
     * array accesses.  The copy involves the 7 voxels (x,y,z), (x+1,y,z),
     * (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), and (x,y,z-1).
     */
    protected void lookUp7 (int iX, int iY, int iZ)
    {
        int iXm = iX - 1, iXp = iX + 1;
        int iYm = iY - 1, iYp = iY + 1;
        int iZm = iZ - 1, iZp = iZ + 1;
        m_fUzzm = m_aaafSrc[iZm][iY ][iX ];
        m_fUzmz = m_aaafSrc[iZ ][iYm][iX ];
        m_fUmzz = m_aaafSrc[iZ ][iY ][iXm];
        m_fUzzz = m_aaafSrc[iZ ][iY ][iX ];
        m_fUpzz = m_aaafSrc[iZ ][iY ][iXp];
        m_fUzpz = m_aaafSrc[iZ ][iYp][iX ];
        m_fUzzp = m_aaafSrc[iZp][iY ][iX ];
    }

    /**
     * Copy the source data to temporary storage in order to avoid redundant
     * array accesses.  The copy involves the 27 pixels in the 3x3x3
     * neighborhood of (x,y,z).
     */
    protected void lookUp27 (int iX, int iY, int iZ)
    {
        int iXm = iX - 1, iXp = iX + 1;
        int iYm = iY - 1, iYp = iY + 1;
        int iZm = iZ - 1, iZp = iZ + 1;
        m_fUmmm = m_aaafSrc[iZm][iYm][iXm];
        m_fUzmm = m_aaafSrc[iZm][iYm][iX ];
        m_fUpmm = m_aaafSrc[iZm][iYm][iXp];
        m_fUmzm = m_aaafSrc[iZm][iY ][iXm];
        m_fUzzm = m_aaafSrc[iZm][iY ][iX ];
        m_fUpzm = m_aaafSrc[iZm][iY ][iXp];
        m_fUmpm = m_aaafSrc[iZm][iYp][iXm];
        m_fUzpm = m_aaafSrc[iZm][iYp][iX ];
        m_fUppm = m_aaafSrc[iZm][iYp][iXp];
        m_fUmmz = m_aaafSrc[iZ ][iYm][iXm];
        m_fUzmz = m_aaafSrc[iZ ][iYm][iX ];
        m_fUpmz = m_aaafSrc[iZ ][iYm][iXp];
        m_fUmzz = m_aaafSrc[iZ ][iY ][iXm];
        m_fUzzz = m_aaafSrc[iZ ][iY ][iX ];
        m_fUpzz = m_aaafSrc[iZ ][iY ][iXp];
        m_fUmpz = m_aaafSrc[iZ ][iYp][iXm];
        m_fUzpz = m_aaafSrc[iZ ][iYp][iX ];
        m_fUppz = m_aaafSrc[iZ ][iYp][iXp];
        m_fUmmp = m_aaafSrc[iZp][iYm][iXm];
        m_fUzmp = m_aaafSrc[iZp][iYm][iX ];
        m_fUpmp = m_aaafSrc[iZp][iYm][iXp];
        m_fUmzp = m_aaafSrc[iZp][iY ][iXm];
        m_fUzzp = m_aaafSrc[iZp][iY ][iX ];
        m_fUpzp = m_aaafSrc[iZp][iY ][iXp];
        m_fUmpp = m_aaafSrc[iZp][iYp][iXm];
        m_fUzpp = m_aaafSrc[iZp][iYp][iX ];
        m_fUppp = m_aaafSrc[iZp][iYp][iXp];
    }
}
