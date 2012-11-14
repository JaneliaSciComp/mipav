package gov.nih.mipav.model.algorithms.levelset;

/**
 * <p>This class implements the <i>curvature flow filter</i> for 2D images
 * using a finite-difference-based solver for the partial differential
 * equation:
 * <br><br> &nbsp; &nbsp;
 *     du/dt = |grad(u)|*divergence(grad(u)/|grad(u)|)
 * <br><br>
 * where u(x,y,t) is the evolved image at time t, du/dt is the time
 * derivative of u, and grad(u) is the gradient of u with respect to the
 * spatial variables.  The notation divergence(Q) = Q0_x + Q1_y, the sum of
 * first-order derivatives of the vector-valued function Q = (Q0,Q1).</p>
 * 
 * <p>The right-hand side of the PDE is the product of the gradient magnitude
 * with the curvature of the level curves.</p>
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public class LseCurvatureFlow2 extends LsePdeFilter2
{
    //~ Constructors ---------------------------------------------------------

	/**
	 * Creates a new LseCurvatureFlow2 object.
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
	public LseCurvatureFlow2 (int iXBound, int iYBound, float fXSpacing,
        float fYSpacing, float[] afData, boolean[] abMask, float fBorderValue,
        int eScaleType)
    {
        super(iXBound,iYBound,fXSpacing,fYSpacing,afData,abMask,fBorderValue,
            eScaleType);
    }

    //~ Methods --------------------------------------------------------------

	/**
	 * The per-pixel update function of the PDE solver.
	 * 
	 * @param iX The x-value of the pixel to be updated.
	 * @param iY The y-value of the pixel to be updated.
	 */
	protected void onUpdate (int iX, int iY)
    {
        lookUp9(iX,iY);

        float fUx = m_fHalfInvDx*(m_fUpz - m_fUmz);
        float fUy = m_fHalfInvDy*(m_fUzp - m_fUzm);
        float fUxx = m_fInvDxDx*(m_fUpz - 2.0f*m_fUzz + m_fUmz);
        float fUxy = m_fFourthInvDxDy*(m_fUmm + m_fUpp - m_fUmp - m_fUpm);
        float fUyy = m_fInvDyDy*(m_fUzp - 2.0f*m_fUzz + m_fUzm);

        float fSqrUx = fUx*fUx;
        float fSqrUy = fUy*fUy;
        float fSqrLength = fSqrUx + fSqrUy + 1e-08f;  // prevent zero division
        m_aafDst[iY][iX] = m_fUzz + m_fTimeStep*(fUxx*fSqrUy +
            fUyy*fSqrUx - 0.5f*fUxy*fUx*fUy)/fSqrLength;
    }
}
