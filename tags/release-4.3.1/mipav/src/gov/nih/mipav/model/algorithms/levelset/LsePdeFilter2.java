package gov.nih.mipav.model.algorithms.levelset;

/**
 * <p>The abstract base class for finite-difference-based solvers for partial
 * differential equations in 2D.  This system exists to support level-set
 * evolution for image segmentation.</p>
 * 
 * <p>The internal 2D data and mask images are copies of the inputs to the
 * constructor but padded with a 1-pixel thick border to support filtering on
 * the image boundary.  These images are of size (xbound+2)-by-(ybound+2).
 * The inputs (x,y) to access the data and mask avlues are constrained
 * to 0 <= x < xbound and 0 <= y < ybound.  The correct lookups into the
 * padded arrays are handled internally.</p>
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public abstract class LsePdeFilter2 extends LsePdeFilter
{
    //~ Instance fields ------------------------------------------------------

	/** The x-bound of the image. */
    protected int m_iXBound;
    
    /** The y-bound of the image. */
    protected int m_iYBound;
    
    /** The x-spacing of the image, call it dx. */
    protected float m_fXSpacing;
    
    /** The y-spacing of the image, call it dy. */
    protected float m_fYSpacing;
    
    /** invDx = 1/dx */
    protected float m_fInvDx;
    
    /** invDy = 1/dy */
    protected float m_fInvDy;
    
    /** halfInvDx = 1/(2*dx) */
    protected float m_fHalfInvDx;
    
    /** halfInvDy = 1/(2*dy) */
    protected float m_fHalfInvDy;
    
    /** invDxDx = 1/(dx*dx) */
    protected float m_fInvDxDx;
    
    /** invFourthDxDy = 1/(4*dx*dy) */
    protected float m_fFourthInvDxDy;
    
    /** invDyDy = 1/(dy*dy) */
    protected float m_fInvDyDy;

    /**
     * Temporary storage for 3x3 neighborhood.  In the notation m_fUij, the
     * i and j indices are in {m,z,p}, referring to subtract 1 (m), no change
     * (z), or add 1 (p) to the appropriate index.
     */
    protected float
    	m_fUmm, m_fUzm, m_fUpm,
    	m_fUmz, m_fUzz, m_fUpz,
    	m_fUmp, m_fUzp, m_fUpp;

    /**
     * Successive iterations of the PDE solver toggle between two buffers.
     * This is the source buffer, which is the input to the solver.
     */
    protected float[][] m_aafSrc;

    /**
     * Successive iterations of the PDE solver toggle between two buffers.
     * This is the destination buffer, which is the output of the solver.
     */
    protected float[][] m_aafDst;
    
    /**
     * Pixels may be masked out so that the PDE solver does not process them.
     * This is useful for filtering only those pixels in a subimage.
     */
    protected boolean[][] m_aabMask;

    //~ Constructors ---------------------------------------------------------

    /**
     * Create a new PDE filter object for 2D images.
     * 
     * @param iXBound The x-bound of the image.
     * @param iYBound The y-bound of the image.
     * @param fXSpacing The x-spacing of the image.
     * @param fYSpacing The y-spacing of the image.
     * @param afData The image elements, stored in lexicographical order.
     * @param abMask The image mask, stored in lexicographical order.  A
     *     pixel value is processed by the PDE solver only when the mask value
     *     is false.
     * @param fBorderValue Specifies how to handle the image value.  When set
     *     to Float.MAX_VALUE, Neumann conditions are in use, in which case
     *     zero-valued derivatives are assumed on the image border.  Otherwise,
     *     Dirichlet conditions are used, in which case the image is assumed
     *     to be constant on the border with value specified by fBorderValue.
     * @param eScaleType The type of scaling to apply to the input image.
     *     The choices are NONE, UNIT, SYMMETRIC, or PRESERVE_ZERO.
     */
    protected LsePdeFilter2 (int iXBound, int iYBound, float fXSpacing,
        float fYSpacing, float[] afData, boolean[] abMask, float fBorderValue,
        int eScaleType)
    {
        super(iXBound*iYBound,afData,fBorderValue,eScaleType);

        m_iXBound = iXBound;
        m_iYBound = iYBound;
        m_fXSpacing = fXSpacing;
        m_fYSpacing = fYSpacing;
        m_fInvDx = 1.0f/fXSpacing;
        m_fInvDy = 1.0f/fYSpacing;
        m_fHalfInvDx = 0.5f*m_fInvDx;
        m_fHalfInvDy = 0.5f*m_fInvDy;
        m_fInvDxDx = m_fInvDx*m_fInvDx;
        m_fFourthInvDxDy = m_fHalfInvDx*m_fHalfInvDy;
        m_fInvDyDy = m_fInvDy*m_fInvDy;

        // Create two buffers for filtering, one a source buffer and one a
        // destination buffer.  Their roles are reversed on each iteration of
        // the filter.
        int iXBp2 = m_iXBound + 2;
        int iYBp2 = m_iYBound + 2;
        int iX, iY;
        m_aafSrc = new float[iYBp2][iXBp2];
        m_aafDst = new float[iYBp2][iXBp2];
        if (abMask != null)
        {
            m_aabMask = new boolean[iYBp2][iXBp2];

            // This sets all mask values to 'false' initially.  The interior
            // mask values will be filled in later using the input abMask.
            // The boundary mask values will remain 'false'.
            for (iY = 0; iY < iYBp2; iY++)
            {
                for (iX = 0; iX < iXBp2; iX++)
                {
                    m_aabMask[iY][iX] = false;
                }
            }
        }
        else
        {
            m_aabMask = null;
        }

        // Copy the inputs afData and abMask to the interior elements of
        // m_aafSrc and m_aabMask.
        int iXp, iYp, i;
        for (iY = 0, iYp = 1, i = 0; iY < m_iYBound; iY++, iYp++)
        {
            for (iX = 0, iXp = 1; iX < m_iXBound; iX++, iXp++, i++)
            {
                m_aafSrc[iYp][iXp] = m_fOffset +
                    (afData[i] - m_fMin)*m_fScale;
                if (m_aabMask != null)
                {
                    m_aabMask[iYp][iXp] = abMask[i];
                }
            }
        }

        // Assign values to the 1-pixel image border.
        if (m_fBorderValue != Float.MAX_VALUE)
        {
            assignDirichletImageBorder();
        }
        else
        {
            assignNeumannImageBorder();
        }

        // To handle masks that do not cover the entire image, assign values
        // to those pixels that are 8-neighbors of the mask pixels.
        if (m_aabMask != null)
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
     * Get the image value at pixel (x,y), call it u(x,y).
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The image value, u(x,y).
     */
    public final float getU (int iX, int iY)
    {
        return m_aafSrc[iY+1][iX+1];
    }

    /**
     * Get the first-order x-derivative of the image value at pixel (x,y),
     * call it u_x(x,y).
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The x-derivative of the image value, u_x(x,y).
     */
    public final float getUx (int iX, int iY)
    {
        int iYp1 = iY + 1;
        return m_fHalfInvDx*(m_aafSrc[iYp1][iX+2] - m_aafSrc[iYp1][iX]);
    }

    /**
     * Get the first-order y-derivative of the image value at pixel (x,y),
     * call it u_y(x,y).
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The y-derivative of the image value, u_y(x,y).
     */
    public final float getUy (int iX, int iY)
    {
        int iXp1 = iX + 1;
        return m_fHalfInvDy*(m_aafSrc[iY+2][iXp1] - m_aafSrc[iY][iXp1]);
    }

    /**
     * Get the second-order x-derivative of the image value at pixel (x,y),
     * call it u_xx(x,y).
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The xx-derivative of the image value, u_xx(x,y).
     */
    public final float getUxx (int iX, int iY)
    {
        int iYp1 = iY + 1;
        return m_fInvDxDx*(m_aafSrc[iYp1][iX+2] - 2.0f*m_aafSrc[iYp1][iX+1] +
            m_aafSrc[iYp1][iX]);
    }

    /**
     * Get the second-order mixed-derivative of the image value at pixel
     * (x,y), call it u_xy(x,y).
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The xy-derivative of the image value, u_xy(x,y).
     */
    public final float getUxy (int iX, int iY)
    {
        int iXp2 = iX + 2, iYp2 = iY + 2;
        return m_fFourthInvDxDy*(m_aafSrc[iY][iX] - m_aafSrc[iY][iXp2] +
            m_aafSrc[iYp2][iXp2] - m_aafSrc[iYp2][iX]);
    }

    /**
     * Get the second-order y-derivative of the image value at pixel (x,y),
     * call it u_yy(x,y).
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The yy-derivative of the image value, u_xx(x,y).
     */
    public final float getUyy (int iX, int iY)
    {
        int iXp1 = iX + 1;
        return m_fInvDyDy*(m_aafSrc[iY+2][iXp1] - 2.0f*m_aafSrc[iY+1][iXp1] +
            m_aafSrc[iY][iXp1]);
    }

    /**
     * Get the mask value at pixel (x,y).
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The mask value at pixel (x,y).  Its value is true when that
     *     pixel should not be processed by the PDE solver.
     */
    public final boolean getMask (int iX, int iY)
    {
        return (m_aabMask != null ? m_aabMask[iY+1][iX+1] : true);
    }

    /**
     * Assign values to the 1-pixel-thick image border.  The border value has
     * been specified during object construction.
     */
    protected void assignDirichletImageBorder ()
    {
        int iXBp1 = m_iXBound + 1, iYBp1 = m_iYBound + 1;
        int iX, iY;

        // vertex (0,0)
        m_aafSrc[0][0] = m_fBorderValue;
        m_aafDst[0][0] = m_fBorderValue;
        if (m_aabMask != null)
        {
            m_aabMask[0][0] = false;
        }

        // vertex (xmax,0)
        m_aafSrc[0][iXBp1] = m_fBorderValue;
        m_aafDst[0][iXBp1] = m_fBorderValue;
        if (m_aabMask != null)
        {
            m_aabMask[0][iXBp1] = false;
        }

        // vertex (0,ymax)
        m_aafSrc[iYBp1][0] = m_fBorderValue;
        m_aafDst[iYBp1][0] = m_fBorderValue;
        if (m_aabMask != null)
        {
            m_aabMask[iYBp1][0] = false;
        }

        // vertex (xmax,ymax)
        m_aafSrc[iYBp1][iXBp1] = m_fBorderValue;
        m_aafDst[iYBp1][iXBp1] = m_fBorderValue;
        if (m_aabMask != null)
        {
            m_aabMask[iYBp1][iXBp1] = false;
        }

        // edges (x,0) and (x,ymax)
        for (iX = 1; iX <= m_iXBound; iX++)
        {
            m_aafSrc[0][iX] = m_fBorderValue;
            m_aafDst[0][iX] = m_fBorderValue;
            if (m_aabMask != null)
            {
                m_aabMask[0][iX] = false;
            }

            m_aafSrc[iYBp1][iX] = m_fBorderValue;
            m_aafDst[iYBp1][iX] = m_fBorderValue;
            if (m_aabMask != null)
            {
                m_aabMask[iYBp1][iX] = false;
            }
        }

        // edges (0,y) and (xmax,y)
        for (iY = 1; iY <= m_iYBound; iY++)
        {
            m_aafSrc[iY][0] = m_fBorderValue;
            m_aafDst[iY][0] = m_fBorderValue;
            if (m_aabMask != null)
            {
                m_aabMask[iY][0] = false;
            }

            m_aafSrc[iY][iXBp1] = m_fBorderValue;
            m_aafDst[iY][iXBp1] = m_fBorderValue;
            if (m_aabMask != null)
            {
                m_aabMask[iY][iXBp1] = false;
            }
        }
    }

    /**
     * Assign values to the 1-pixel-thick image border.  The border values are
     * chosen so that the first-order image derivatives at the border pixels
     * are zero.  Thus, the border values are duplicates of the adjacent image
     * values.
     */
    protected void assignNeumannImageBorder ()
    {
        int iXBp1 = m_iXBound + 1, iYBp1 = m_iYBound + 1;
        int iX, iY;
        float fDuplicate;

        // vertex (0,0)
        fDuplicate = m_aafSrc[1][1];
        m_aafSrc[0][0] = fDuplicate;
        m_aafDst[0][0] = fDuplicate;
        if (m_aabMask != null)
        {
            m_aabMask[0][0] = false;
        }

        // vertex (xmax,0)
        fDuplicate = m_aafSrc[1][m_iXBound];
        m_aafSrc[0][iXBp1] = fDuplicate;
        m_aafDst[0][iXBp1] = fDuplicate;
        if (m_aabMask != null)
        {
            m_aabMask[0][iXBp1] = false;
        }

        // vertex (0,ymax)
        fDuplicate = m_aafSrc[m_iYBound][1];
        m_aafSrc[iYBp1][0] = fDuplicate;
        m_aafDst[iYBp1][0] = fDuplicate;
        if (m_aabMask != null)
        {
            m_aabMask[iYBp1][0] = false;
        }

        // vertex (xmax,ymax)
        fDuplicate = m_aafSrc[m_iYBound][m_iXBound];
        m_aafSrc[iYBp1][iXBp1] = fDuplicate;
        m_aafDst[iYBp1][iXBp1] = fDuplicate;
        if (m_aabMask != null)
        {
            m_aabMask[iYBp1][iXBp1] = false;
        }

        // edges (x,0) and (x,ymax)
        for (iX = 1; iX <= m_iXBound; iX++)
        {
            fDuplicate = m_aafSrc[1][iX];
            m_aafSrc[0][iX] = fDuplicate;
            m_aafDst[0][iX] = fDuplicate;
            if (m_aabMask != null)
            {
                m_aabMask[0][iX] = false;
            }

            fDuplicate = m_aafSrc[m_iYBound][iX];
            m_aafSrc[iYBp1][iX] = fDuplicate;
            m_aafDst[iYBp1][iX] = fDuplicate;
            if (m_aabMask != null)
            {
                m_aabMask[iYBp1][iX] = false;
            }
        }

        // edges (0,y) and (xmax,y)
        for (iY = 1; iY <= m_iYBound; iY++)
        {
            fDuplicate = m_aafSrc[iY][1];
            m_aafSrc[iY][0] = fDuplicate;
            m_aafDst[iY][0] = fDuplicate;
            if (m_aabMask != null)
            {
                m_aabMask[iY][0] = false;
            }

            fDuplicate = m_aafSrc[iY][m_iXBound];
            m_aafSrc[iY][iXBp1] = fDuplicate;
            m_aafDst[iY][iXBp1] = fDuplicate;
            if (m_aabMask != null)
            {
                m_aabMask[iY][iXBp1] = false;
            }
        }
    }

    /**
     * Assign values to the 1-pixel-thick borders that surround unmasked
     * regions.  The border value has been specified during object
     * construction.
     */
    protected void assignDirichletMaskBorder ()
    {
        // assert(m_aabMask != null);

        for (int iY = 1; iY <= m_iYBound; iY++)
        {
            for (int iX = 1; iX <= m_iXBound; iX++)
            {
                if (m_aabMask[iY][iX])
                {
                    continue;
                }

                boolean bFound = false;
                int i0, i1, iLX, iLY;
                for (i1 = 0, iLY = iY-1; i1 < 3 && !bFound; i1++, iLY++)
                {
                    for (i0 = 0, iLX = iX-1; i0 < 3; i0++, iLX++)
                    {
                        if (m_aabMask[iLY][iLX])
                        {
                            m_aafSrc[iY][iX] = m_fBorderValue;
                            m_aafDst[iY][iX] = m_fBorderValue;
                            bFound = true;
                            break;
                        }
                    }
                }
            }
        }
    }

    /**
     * Assign values to the 1-pixel-thick borders that surround unmasked
     * regions.  The border values are chosen so that the first-order image
     * derivatives at the border pixels are zero.  Thus, the border values are
     * duplicates of the adjacent image values.
     */
    protected void assignNeumannMaskBorder ()
    {
        // Recompute the values just outside the masked region.  This
        // guarantees that derivative estimations use the current values
        // around the boundary.
        for (int iY = 1; iY <= m_iYBound; iY++)
        {
            for (int iX = 1; iX <= m_iXBound; iX++)
            {
                if (m_aabMask[iY][iX])
                {
                    continue;
                }

                int iCount = 0;
                float fAverage = 0.0f;
                int i0, i1, iLX, iLY;
                for (i1 = 0, iLY = iY-1; i1 < 3; i1++, iLY++)
                {
                    for (i0 = 0, iLX = iX-1; i0 < 3; i0++, iLX++)
                    {
                        if (m_aabMask[iLY][iLX])
                        {
                            fAverage += m_aafSrc[iLY][iLX];
                            iCount++;
                        }
                    }
                }

                if (iCount > 0)
                {
                    fAverage /= (float)iCount;
                    m_aafSrc[iY][iX] = fAverage;
                    m_aafDst[iY][iX] = fAverage;
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
        if (m_aabMask != null && m_fBorderValue == Float.MAX_VALUE)
        {
            // Neumann boundary conditions are in use, so recompute the mask
            // border.
            assignNeumannMaskBorder();
        }
        // else: No mask has been specified or Dirichlet boundary conditions
        // are in use.  Nothing to do.
    }

    /**
     * Iterate over all the pixels and call onUpdate(x,y) for each pixel that
     * is not masked out.
     */
    protected void onUpdate ()
    {
         for (int iY = 1; iY <= m_iYBound; iY++)
        {
            for (int iX = 1; iX <= m_iXBound; iX++)
            {
                if (m_aabMask == null || m_aabMask[iY][iX])
                {
                    onUpdate(iX,iY);
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
        float[][] aafSave = m_aafSrc;
        m_aafSrc = m_aafDst;
        m_aafDst = aafSave;
    }

    /**
     * An abstract function that allows per-pixel processing by the PDE
     * solver.  The pixel (x,y) must be in padded coordinates, namely,
     * 1 <= x <= xbound and 1 <= y <= ybound.
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     */
    protected abstract void onUpdate (int iX, int iY);

    /**
     * Copy the source data to temporary storage in order to avoid redundant
     * array accesses.  The copy involves the 5 pixels (x,y), (x+1,y),
     * (x-1,y), (x,y+1), and (x,y-1).
     */
    protected void lookUp5 (int iX, int iY)
    {
        int iXm = iX - 1, iXp = iX + 1;
        int iYm = iY - 1, iYp = iY + 1;
        m_fUzm = m_aafSrc[iYm][iX ];
        m_fUmz = m_aafSrc[iY ][iXm];
        m_fUzz = m_aafSrc[iY ][iX ];
        m_fUpz = m_aafSrc[iY ][iXp];
        m_fUzp = m_aafSrc[iYp][iX ];
    }


    /**
     * Copy the source data to temporary storage in order to avoid redundant
     * array accesses.  The copy involves the 9 pixels in the 3x3 neighborhood
     * of (x,y).
     */
    protected void lookUp9 (int iX, int iY)
    {
        int iXm = iX - 1, iXp = iX + 1;
        int iYm = iY - 1, iYp = iY + 1;
        m_fUmm = m_aafSrc[iYm][iXm];
        m_fUzm = m_aafSrc[iYm][iX ];
        m_fUpm = m_aafSrc[iYm][iXp];
        m_fUmz = m_aafSrc[iY ][iXm];
        m_fUzz = m_aafSrc[iY ][iX ];
        m_fUpz = m_aafSrc[iY ][iXp];
        m_fUmp = m_aafSrc[iYp][iXm];
        m_fUzp = m_aafSrc[iYp][iX ];
        m_fUpp = m_aafSrc[iYp][iXp];
    }
}
