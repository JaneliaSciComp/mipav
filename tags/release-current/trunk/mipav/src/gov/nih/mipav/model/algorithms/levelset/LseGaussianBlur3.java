package gov.nih.mipav.model.algorithms.levelset;

/**
 * <p>This class implements the <i>Gaussian blur filter</i> for 3D images
 * using a finite-difference-based solver for the partial differential
 * equation:
 * <br><br> &nbsp; &nbsp;
 *     du/dt = Laplacian(u)
 * <br><br>
 * where u(x,y,z,t) is the evolved image at time t, du/dt is the time
 * derivative of u, and Laplacian(u) = u_xx + u_yy + u_zz, a sum of
 * second-order derivatives of u.</p>
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public class LseGaussianBlur3 extends LsePdeFilter3
{
    //~ Instance fields ------------------------------------------------------

	/**
	 * The maximum time step allowed for numerical stability of the filter.
	 */
	private float m_fMaximumTimeStep;

    //~ Constructors ---------------------------------------------------------

	/**
	 * Creates a new LseGaussianBlur3 object.
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
	public LseGaussianBlur3 (int iXBound, int iYBound, int iZBound,
        float fXSpacing, float fYSpacing, float fZSpacing, float[] afData,
        boolean[] abMask, float fBorderValue, int eScaleType)
    {
        super(iXBound,iYBound,iZBound,fXSpacing,fYSpacing,fZSpacing,afData,
            abMask,fBorderValue,eScaleType);

        m_fMaximumTimeStep = 0.5f/(m_fInvDxDx + m_fInvDyDy + m_fInvDzDz);
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
	 * The per-voxel update function of the PDE solver.
	 * 
	 * @param iX The x-value of the voxel to be updated.
	 * @param iY The y-value of the voxel to be updated.
	 * @param iZ The z-value of the voxel to be updated.
	 */
    protected void onUpdate (int iX, int iY, int iZ)
    {
        lookUp7(iX,iY,iZ);

        float fUxx = m_fInvDxDx*(m_fUpzz - 2.0f*m_fUzzz + m_fUmzz);
        float fUyy = m_fInvDyDy*(m_fUzpz - 2.0f*m_fUzzz + m_fUzmz);
        float fUzz = m_fInvDzDz*(m_fUzzp - 2.0f*m_fUzzz + m_fUzzm);

        m_aaafDst[iZ][iY][iX] = m_fUzzz + m_fTimeStep*(fUxx + fUyy + fUzz);
    }
}
