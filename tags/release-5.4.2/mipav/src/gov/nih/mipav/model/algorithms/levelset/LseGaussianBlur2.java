package gov.nih.mipav.model.algorithms.levelset;

/**
 * <p>This class implements the <i>Gaussian blur filter</i> for 2D images
 * using a finite-difference-based solver for the partial differential
 * equation:
 * <br><br> &nbsp; &nbsp;
 *     du/dt = Laplacian(u)
 * <br><br>
 * where u(x,y,t) is the evolved image at time t, du/dt is the time
 * derivative of u, and Laplacian(u) = u_xx + u_yy, a sum of second-order
 * derivatives of u.</p>
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public class LseGaussianBlur2 extends LsePdeFilter2
{
    //~ Instance fields ------------------------------------------------------

	/**
	 * The maximum time step allowed for numerical stability of the filter.
	 */
	private float m_fMaximumTimeStep;

	//~ Constructors ---------------------------------------------------------

	/**
	 * Creates a new LseGaussianBlur2 object.
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
    public LseGaussianBlur2 (int iXBound, int iYBound, float fXSpacing,
        float fYSpacing, float[] afData, boolean[] abMask, float fBorderValue,
        int eScaleType)
    {
        super(iXBound,iYBound,fXSpacing,fYSpacing,afData,abMask,fBorderValue,
            eScaleType);

        m_fMaximumTimeStep = 0.5f/(m_fInvDxDx + m_fInvDyDy);
    }

    //~ Methods --------------------------------------------------------------

    /**
     * Get the maximum time step for numerical stabilty of the filter.
     * 
     * @return The maximum time step.
     */
    public final float getMaximumTimeStep ()
    {
        return m_fMaximumTimeStep;
    }

	/**
	 * The per-pixel update function of the PDE solver.
	 * 
	 * @param iX The x-value of the pixel to be updated.
	 * @param iY The y-value of the pixel to be updated.
	 */
    protected void onUpdate (int iX, int iY)
    {

        lookUp5(iX,iY);

        float fUxx = m_fInvDxDx*(m_fUpz - 2.0f*m_fUzz + m_fUmz);
        float fUyy = m_fInvDyDy*(m_fUzp - 2.0f*m_fUzz + m_fUzm);

        m_aafDst[iY][iX] = m_fUzz + m_fTimeStep*(fUxx + fUyy);
    }
}
