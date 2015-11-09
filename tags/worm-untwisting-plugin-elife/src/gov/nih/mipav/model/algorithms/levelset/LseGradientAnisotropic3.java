package gov.nih.mipav.model.algorithms.levelset;

/**
 * <p>This class implements the <i>gradient anistropic curvature flow
 * filter</i> for 3D images using a finite-difference-based solver for the
 * partial differential equation:
 * <br><br> &nbsp; &nbsp;
 *     du/dt = divergence(exp(-0.5*|grad(u)|^2/(k^2*average(|grad(u)|^2))*grad(u))
 * <br><br>
 * where u(x,y,z,t) is the evolved image at time t, du/dt is the time
 * derivative of u, and grad(u) is the gradient of u with respect to the
 * spatial variables.  The value k is a used-selected parameter and may be
 * thought of as a local curvature parameter.  The notation
 * divergence(Q) = Q0_x + Q1_y + Q2_z, the sum of first-order derivatives of
 * the vector-valued function Q = (Q0,Q1).</p>
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public class LseGradientAnisotropic3 extends LsePdeFilter3
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
	 * Creates a new LseCurvatureFlow3 object.
	 * 
     * @param iXBound The x-bound of the image.
     * @param iYBound The y-bound of the image.
     * @param iZBound The z-bound of the image.
     * @param fXSpacing The x-spacing of the image.
     * @param fYSpacing The y-spacing of the image.
     * @param fZSpacing The z-spacing of the image.
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
	public LseGradientAnisotropic3 (int iXBound, int iYBound, int iZBound,
        float fXSpacing, float fYSpacing, float fZSpacing, float[] afData,
        boolean[] abMask, float fBorderValue, int eScaleType, float fK)
    {
        super(iXBound,iYBound,iZBound,fXSpacing,fYSpacing,fZSpacing,afData,
            abMask,fBorderValue,eScaleType);

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
        for (int iZ = 1; iZ <= m_iZBound; iZ++)
        {
            for (int iY = 1; iY <= m_iYBound; iY++)
            {
                for (int iX = 1; iX <= m_iXBound; iX++)
                {
                    float fUx = getUx(iX,iY,iZ);
                    float fUy = getUy(iX,iY,iZ);
                    float fUz = getUz(iX,iY,iZ);
                    fGradMagSqr += fUx*fUx + fUy*fUy + fUz*fUz;
                }
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
	 * The per-voxel update function of the PDE solver.
	 * 
	 * @param iX The x-value of the voxel to be updated.
	 * @param iY The y-value of the voxel to be updated.
	 * @param iZ The z-value of the voxel to be updated.
	 */
    protected void onUpdate (int iX, int iY, int iZ)
    {
        lookUp27(iX,iY,iZ);

        // one-sided U-derivative estimates
        float fUxFwd = m_fInvDx*(m_fUpzz - m_fUzzz);
        float fUxBwd = m_fInvDx*(m_fUzzz - m_fUmzz);
        float fUyFwd = m_fInvDy*(m_fUzpz - m_fUzzz);
        float fUyBwd = m_fInvDy*(m_fUzzz - m_fUzmz);
        float fUzFwd = m_fInvDz*(m_fUzzp - m_fUzzz);
        float fUzBwd = m_fInvDz*(m_fUzzz - m_fUzzm);

        // centered U-derivative estimates
        float fDUvzz = m_fHalfInvDx*(m_fUpzz - m_fUmzz);
        float fDUvpz = m_fHalfInvDx*(m_fUppz - m_fUmpz);
        float fDUvmz = m_fHalfInvDx*(m_fUpmz - m_fUmmz);
        float fDUvzp = m_fHalfInvDx*(m_fUpzp - m_fUmzp);
        float fDUvzm = m_fHalfInvDx*(m_fUpzm - m_fUmzm);

        float fDUzvz = m_fHalfInvDy*(m_fUzpz - m_fUzmz);
        float fDUpvz = m_fHalfInvDy*(m_fUppz - m_fUpmz);
        float fDUmvz = m_fHalfInvDy*(m_fUmpz - m_fUmmz);
        float fDUzvp = m_fHalfInvDy*(m_fUzpp - m_fUzmp);
        float fDUzvm = m_fHalfInvDy*(m_fUzpm - m_fUzmm);

        float fDUzzv = m_fHalfInvDz*(m_fUzzp - m_fUzzm);
        float fDUpzv = m_fHalfInvDz*(m_fUpzp - m_fUpzm);
        float fDUmzv = m_fHalfInvDz*(m_fUmzp - m_fUmzm);
        float fDUzpv = m_fHalfInvDz*(m_fUzpp - m_fUzpm);
        float fDUzmv = m_fHalfInvDz*(m_fUzmp - m_fUzmm);

        float fUxCenSqr = fDUvzz*fDUvzz;
        float fUyCenSqr = fDUzvz*fDUzvz;
        float fUzCenSqr = fDUzzv*fDUzzv;

        float fUxEst, fUyEst, fUzEst, fGradMagSqr;

        // estimate for C(x+1,y,z)
        fUyEst = 0.5f*(fDUzvz + fDUpvz);
        fUzEst = 0.5f*(fDUzzv + fDUpzv);
        fGradMagSqr = fUxCenSqr + fUyEst*fUyEst + fUzEst*fUzEst;
        float fCXP = (float)Math.exp((double)(m_fMHalfParam*fGradMagSqr));

        // estimate for C(x-1,y,z)
        fUyEst = 0.5f*(fDUzvz + fDUmvz);
        fUzEst = 0.5f*(fDUzzv + fDUmzv);
        fGradMagSqr = fUxCenSqr + fUyEst*fUyEst + fUzEst*fUzEst;
        float fCXM = (float)Math.exp((double)(m_fMHalfParam*fGradMagSqr));

        // estimate for C(x,y+1,z)
        fUxEst = 0.5f*(fDUvzz + fDUvpz);
        fUzEst = 0.5f*(fDUzzv + fDUzpv);
        fGradMagSqr = fUxEst*fUxEst + fUyCenSqr + fUzEst*fUzEst;
        float fCYP = (float)Math.exp((double)(m_fMHalfParam*fGradMagSqr));

        // estimate for C(x,y-1,z)
        fUxEst = 0.5f*(fDUvzz + fDUvmz);
        fUzEst = 0.5f*(fDUzzv + fDUzmv);
        fGradMagSqr = fUxEst*fUxEst + fUyCenSqr + fUzEst*fUzEst;
        float fCYM = (float)Math.exp((double)(m_fMHalfParam*fGradMagSqr));

        // estimate for C(x,y,z+1)
        fUxEst = 0.5f*(fDUvzz + fDUvzp);
        fUyEst = 0.5f*(fDUzvz + fDUzvp);
        fGradMagSqr = fUxEst*fUxEst + fUyEst*fUyEst + fUzCenSqr;
        float fCZP = (float)Math.exp((double)(m_fMHalfParam*fGradMagSqr));

        // estimate for C(x,y,z-1)
        fUxEst = 0.5f*(fDUvzz + fDUvzm);
        fUyEst = 0.5f*(fDUzvz + fDUzvm);
        fGradMagSqr = fUxEst*fUxEst + fUyEst*fUyEst + fUzCenSqr;
        float fCZM = (float)Math.exp((double)(m_fMHalfParam*fGradMagSqr));

        m_aaafDst[iZ][iY][iX] = m_fUzzz + m_fTimeStep*(
            fCXP*fUxFwd - fCXM*fUxBwd +
            fCYP*fUyFwd - fCYM*fUyBwd +
            fCZP*fUzFwd - fCZM*fUzBwd);
    }
}
