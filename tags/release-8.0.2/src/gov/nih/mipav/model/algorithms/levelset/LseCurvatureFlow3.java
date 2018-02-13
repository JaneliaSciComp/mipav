package gov.nih.mipav.model.algorithms.levelset;

/**
 * <p>This class implements the <i>curvature flow filter</i> for 3D images
 * using a finite-difference-based solver for the partial differential
 * equation:
 * <br><br> &nbsp; &nbsp;
 *     du/dt = |grad(u)|*divergence(grad(u)/|grad(u)|)
 * <br><br>
 * where u(x,y,z,t) is the evolved image at time t, du/dt is the time
 * derivative of u, and grad(u) is the gradient of u with respect to the
 * spatial variables.  The notation divergence(Q) = Q0_x + Q1_y + Q2_z, the
 * sum of first-order derivatives of the vector-valued function Q =
 * (Q0,Q1,Q2).</p>
 * 
 * <p>The right-hand side of the PDE is the product of the gradient magnitude
 * with the mean curvature of the level surfaces.</p>
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public class LseCurvatureFlow3 extends LsePdeFilter3
{
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
	 */
    public LseCurvatureFlow3 (int iXBound, int iYBound, int iZBound,
        float fXSpacing, float fYSpacing, float fZSpacing, float[] afData,
        boolean[] abMask, float fBorderValue, int eScaleType)
    {
        super(iXBound,iYBound,iZBound,fXSpacing,fYSpacing,fZSpacing,afData,
            abMask,fBorderValue,eScaleType);
    }

    //~ Methods --------------------------------------------------------------

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

        float fUx = m_fHalfInvDx*(m_fUpzz - m_fUmzz);
        float fUy = m_fHalfInvDy*(m_fUzpz - m_fUzmz);
        float fUz = m_fHalfInvDz*(m_fUzzp - m_fUzzm);
        float fUxx = m_fInvDxDx*(m_fUpzz - 2.0f*m_fUzzz + m_fUmzz);
        float fUxy = m_fFourthInvDxDy*(m_fUmmz + m_fUppz - m_fUpmz - m_fUmpz);
        float fUxz = m_fFourthInvDxDz*(m_fUmzm + m_fUpzp - m_fUpzm - m_fUmzp);
        float fUyy = m_fInvDyDy*(m_fUzpz - 2.0f*m_fUzzz + m_fUzmz);
        float fUyz = m_fFourthInvDyDz*(m_fUzmm+ m_fUzpp - m_fUzpm  - m_fUzmp);
        float fUzz = m_fInvDzDz*(m_fUzzp - 2.0f*m_fUzzz + m_fUzzm);

        float fGradMagSqr = fUx*fUx + fUy*fUy + fUz*fUz;
        float fNumer0 = fUy*(fUxx*fUy - fUxy*fUx) + fUx*(fUyy*fUx - fUxy*fUy);
        float fNumer1 = fUz*(fUxx*fUz - fUxz*fUx) + fUx*(fUzz*fUx - fUxz*fUz);
        float fNumer2 = fUz*(fUyy*fUz - fUyz*fUy) + fUy*(fUzz*fUy - fUyz*fUz);
        float fDenom = fGradMagSqr + 1e-08f;  // prevent zero division
        float fCurvatureLength = (fNumer0 + fNumer1 + fNumer2)/fDenom;

        m_aaafDst[iZ][iY][iX] = m_fUzzz + m_fTimeStep*fCurvatureLength;
    }
}
