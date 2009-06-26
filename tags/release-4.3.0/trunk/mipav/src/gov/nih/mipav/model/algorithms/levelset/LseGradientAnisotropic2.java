package gov.nih.mipav.model.algorithms.levelset;

/**
 * <p>This class implements the <i>gradient anistropic curvature flow
 * filter</i> for 2D images using a finite-difference-based solver for the
 * partial differential equation:
 * <br><br> &nbsp; &nbsp;
 *     du/dt = divergence(exp(-0.5*|grad(u)|^2/(k^2*average(|grad(u)|^2))*grad(u))
 * <br><br>
 * where u(x,y,t) is the evolved image at time t, du/dt is the time
 * derivative of u, and grad(u) is the gradient of u with respect to the
 * spatial variables.  The value k is a used-selected parameter and may be
 * thought of as a local curvature parameter.  The notation
 * divergence(Q) = Q0_x + Q1_y, the sum of first-order derivatives of the
 * vector-valued function Q = (Q0,Q1).</p>
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public class LseGradientAnisotropic2 extends LsePdeFilter2
{
    //~ Instance fields ------------------------------------------------------

    /** The local curvature parameter, call it k. */
    private float m_fK;

    /** 1/(k^2*average(|grad(u)|^2)) */
    private float m_fParam;
    
    /** -0.5/(k^2*average(|grad(u)|^2)) */
    private float m_fMHalfParam;

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
     * @param fK The local curvature parameter in the exponential term of the
     *     PDE.
	 */
    public LseGradientAnisotropic2 (int iXBound, int iYBound, float fXSpacing,
        float fYSpacing, float[] afData, boolean[] abMask, float fBorderValue,
        int eScaleType, float fK)
    {
        super(iXBound,iYBound,fXSpacing,fYSpacing,afData,abMask,fBorderValue,
            eScaleType);

        m_fK = fK;
        computeParam();
    }

    //~ Methods --------------------------------------------------------------
    
    /**
     * Compute the average gradient magnitude squared for the current image
     * u(x,y,t).  The terms p = 1/(k*k*average(|grad(u)|^2) and -0.5*p are
     * computed once and used in the per-pixel update in the PDE solver.
     *
     */
    protected void computeParam ()
    {
        float fGradMagSqr = 0.0f;
        for (int iY = 1; iY <= m_iYBound; iY++)
        {
            for (int iX = 1; iX <= m_iXBound; iX++)
            {
                float fUx = getUx(iX,iY);
                float fUy = getUy(iX,iY);
                fGradMagSqr += fUx*fUx + fUy*fUy;
            }
        }
        fGradMagSqr /= (float)m_iQuantity;

        m_fParam = 1.0f/(m_fK*m_fK*fGradMagSqr);
        m_fMHalfParam = -0.5f*m_fParam;
    }

    /**
     * The function onPreUpdate was abstractly defined in the LsePdeFilter
     * base class.  It is called before any per-pixel updates.  In this class,
     * the parameters in the exponential term of the PDE are precomputed for
     * use by the onUpdate() function.
     */
    protected void onPreUpdate ()
    {
        computeParam();
    }

	/**
	 * The per-pixel update function of the PDE solver.
	 * 
	 * @param iX The x-value of the pixel to be updated.
	 * @param iY The y-value of the pixel to be updated.
	 */
    protected void onUpdate (int iX, int iY)
    {
        lookUp9(iX,iY);

        // one-sided U-derivative estimates
        float fUxFwd = m_fInvDx*(m_fUpz - m_fUzz);
        float fUxBwd = m_fInvDx*(m_fUzz - m_fUmz);
        float fUyFwd = m_fInvDy*(m_fUzp - m_fUzz);
        float fUyBwd = m_fInvDy*(m_fUzz - m_fUzm);

        // centered U-derivative estimates
        float fUxCenM = m_fHalfInvDx*(m_fUpm - m_fUmm);
        float fUxCenZ = m_fHalfInvDx*(m_fUpz - m_fUmz);
        float fUxCenP = m_fHalfInvDx*(m_fUpp - m_fUmp);
        float fUyCenM = m_fHalfInvDy*(m_fUmp - m_fUmm);
        float fUyCenZ = m_fHalfInvDy*(m_fUzp - m_fUzm);
        float fUyCenP = m_fHalfInvDy*(m_fUpp - m_fUpm);

        float fUxCenZSqr = fUxCenZ*fUxCenZ;
        float fUyCenZSqr = fUyCenZ*fUyCenZ;
        float fGradMagSqr;

        // estimate for C(x+1,y)
        float fUyEstP = 0.5f*(fUyCenZ + fUyCenP);
        fGradMagSqr = fUxCenZSqr + fUyEstP*fUyEstP;
        float fCXP = (float)Math.exp((double)(m_fMHalfParam*fGradMagSqr));

        // estimate for C(x-1,y)
        float fUyEstM = 0.5f*(fUyCenZ + fUyCenM);
        fGradMagSqr = fUxCenZSqr + fUyEstM*fUyEstM;
        float fCXM = (float)Math.exp((double)(m_fMHalfParam*fGradMagSqr));

        // estimate for C(x,y+1)
        float fUxEstP = 0.5f*(fUxCenZ + fUxCenP);
        fGradMagSqr = fUyCenZSqr + fUxEstP*fUxEstP;
        float fCYP = (float)Math.exp((double)(m_fMHalfParam*fGradMagSqr));

        // estimate for C(x,y-1)
        float fUxEstM = 0.5f*(fUxCenZ + fUxCenM);
        fGradMagSqr = fUyCenZSqr + fUxEstM*fUxEstM;
        float fCYM = (float)Math.exp((double)(m_fMHalfParam*fGradMagSqr));

        m_aafDst[iY][iX] = m_fUzz + m_fTimeStep*(
            fCXP*fUxFwd - fCXM*fUxBwd + fCYP*fUyFwd - fCYM*fUyBwd);
    }
}
