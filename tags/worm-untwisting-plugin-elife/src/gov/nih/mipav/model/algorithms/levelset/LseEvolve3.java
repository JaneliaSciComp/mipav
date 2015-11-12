package gov.nih.mipav.model.algorithms.levelset;

/**
 * The abstract base class for level-set evolution of 3D images.  It is based
 * on PDEs that are solved numerically using finite-difference approximations.
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public abstract class LseEvolve3 extends LsePdeFilter3
{
    //~ Instance fields ------------------------------------------------------

	/** The parameters associated with level-set evolution. */
	protected LseParameters m_kParameters;

	/** The image value of the current pixel. */
    protected float m_fD0;

    /** The first-order derivatives of the current pixel. */
    protected float[] m_afD1;

    /** The second-order derivatives of the current pixel. */
    protected float[][] m_aafD2;

    /**
     * Forward finite-difference approximations to the first-order
     * derivatives of the current pixel.
     */
    protected float[] m_afD1Fwd;

    /**
     * Backward finite-difference approximations to the first-order
     * derivatives of the current pixel.
     */
    protected float[] m_afD1Bwd;

    /** The squared magnitude of the gradient vector for the current pixel. */
    protected float m_fGradMagSqr;

    /** The advection for the current pixel. */
    protected float[] m_afAdvection;

    //~ Constructors ---------------------------------------------------------
    
    /**
     * Create a new level-set evolver for 3D images.
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
	protected LseEvolve3 (int iXBound, int iYBound, int iZBound,
        float fXSpacing, float fYSpacing, float fZSpacing, float[] afData,
        boolean[] abMask, float fBorderValue, int eScaleType)
    {
        super(iXBound,iYBound,iZBound,fXSpacing,fYSpacing,fZSpacing,afData,
            abMask,fBorderValue,eScaleType);

        m_kParameters = new LseParameters();        
        m_afD1 = new float[3];
        m_aafD2 = new float[3][3];
        m_afD1Fwd = new float[3];
        m_afD1Bwd = new float[3];
        m_afAdvection = new float[3];
       for (int i = 0; i < 3; i++)
        {
            m_afD1[i] = 0.0f;
            m_afD1Fwd[i] = 0.0f;
            m_afD1Bwd[i] = 0.0f;
            m_afAdvection[i] = 0.0f;
            for (int j = 0; j < 3; j++)
            {
                m_aafD2[i][j] = 0.0f;
            }
        }
        m_fGradMagSqr = 0.0f;
     }

    //~ Methods --------------------------------------------------------------

    /**
     * Get the current level-set evolution parameters.
     * 
     * @return The current level-set evolution parameters.
     */
    public final LseParameters getParameters ()
    {
        return m_kParameters;
    }

    /**
     * Evolution support.  ComputeTimeStep() is called each time before
     * ComputeFunction() is called.  The function ComputeCurvature() actually
     * computes k*L, where k is the mean curvature and L is the length of the
     * gradient vector.  The resulting quantity is
     * <br/><br/>&nbsp;&nbsp;
     *   k*L = (Uxx*(Uy*Uy+Uz*Uz) + Uyy*(Ux*Ux+Uz*Uz) + Uzz*(Ux*Ux+Uy*Uy)
     *          - 2*(Uxy*Ux*Uy + Uxz*Ux*Uz + Uyz*Uy*Uz))/(Ux*Ux+Uy*Uy+Uz*Uz)
     * <br/><br/>
     * where Ux, Uy, and Uz are first-order partial derivatives and Uxx, Uxy,
     * Uxz, Uyy, Uyz, and Uzz are second-order partial derivatives.
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return  The value of the right-hand side of the PDE that governs the
     *     level-set evolution.
     */
    protected final float computeFunction (int iX, int iY, int iZ)
    {
        // Get the values from the 3x3x3 neighborhood of (x,y,z).
        int iXm = iX - 1, iXp = iX + 1;
        int iYm = iY - 1, iYp = iY + 1;
        int iZm = iZ - 1, iZp = iZ + 1;
        float fUzmm = m_aaafSrc[iZm][iYm][iX ];
        float fUmzm = m_aaafSrc[iZm][iY ][iXm];
        float fUzzm = m_aaafSrc[iZm][iY ][iX ];
        float fUpzm = m_aaafSrc[iZm][iY ][iXp];
        float fUzpm = m_aaafSrc[iZm][iYp][iX ];
        float fUmmz = m_aaafSrc[iZ ][iYm][iXm];
        float fUzmz = m_aaafSrc[iZ ][iYm][iX ];
        float fUpmz = m_aaafSrc[iZ ][iYm][iXp];
        float fUmzz = m_aaafSrc[iZ ][iY ][iXm];
        float fUzzz = m_aaafSrc[iZ ][iY ][iX ];
        float fUpzz = m_aaafSrc[iZ ][iY ][iXp];
        float fUmpz = m_aaafSrc[iZ ][iYp][iXm];
        float fUzpz = m_aaafSrc[iZ ][iYp][iX ];
        float fUppz = m_aaafSrc[iZ ][iYp][iXp];
        float fUzmp = m_aaafSrc[iZp][iYm][iX ];
        float fUmzp = m_aaafSrc[iZp][iY ][iXm];
        float fUzzp = m_aaafSrc[iZp][iY ][iX ];
        float fUpzp = m_aaafSrc[iZp][iY ][iXp];
        float fUzpp = m_aaafSrc[iZp][iYp][iX ];

        // Get the image and derivative values.  The derivatives are centered
        // finite differences in a 3x3 neighborhood of (x,y).
        m_fD0 = fUzzz;
        m_afD1[0] = m_fHalfInvDx*(fUpzz - fUmzz);
        m_afD1[1] = m_fHalfInvDy*(fUzpz - fUzmz);
        m_afD1[2] = m_fHalfInvDz*(fUzzp - fUzzm);
        m_aafD2[0][0] = m_fInvDxDx*(fUpzz - 2.0f*fUzzz + fUmzz);
        m_aafD2[0][1] = m_fFourthInvDxDy*(fUmmz - fUpmz + fUppz - fUmpz);
        m_aafD2[0][2] = m_fFourthInvDxDz*(fUmzm - fUpzm + fUpzp - fUmzp);
        m_aafD2[1][0] = m_aafD2[0][1];
        m_aafD2[1][1] = m_fInvDyDy*(fUzpz - 2.0f*fUzzz + fUzmz);
        m_aafD2[1][2] = m_fFourthInvDyDz*(fUzmm - fUzpm + fUzpp - fUzmp);
        m_aafD2[2][0] = m_aafD2[0][2];
        m_aafD2[2][1] = m_aafD2[1][2];
        m_aafD2[2][2] = m_fInvDzDz*(fUzzp - 2.0f*fUzzz + fUzzm);

        // Compute the one-sided first-order differences.
        m_afD1Fwd[0] = m_fInvDx*(fUpzz - fUzzz);
        m_afD1Fwd[1] = m_fInvDy*(fUzpz - fUzzz);
        m_afD1Fwd[2] = m_fInvDz*(fUzzp - fUzzz);
        m_afD1Bwd[0] = m_fInvDx*(fUzzz - fUmzz);
        m_afD1Bwd[1] = m_fInvDy*(fUzzz - fUzmz);
        m_afD1Bwd[2] = m_fInvDz*(fUzzz - fUzzm);

        // Compute the squared length of the gradient (used in curvature
        // calculations).
        m_fGradMagSqr = m_afD1[0]*m_afD1[0] + m_afD1[1]*m_afD1[1] +
            m_afD1[2]*m_afD1[2];

        float fFunction = 0.0f;
        float fAbs;
        int i;

        if (m_kParameters.getAdvectionWeight() != 0.0f)
        {
            float[] afAdvection = computeAdvection(iX,iY,iZ);
            float fAdvectionTerm = 0.0f;
            for (i = 0; i < 3; i++)
            {
                float fEnergy = m_kParameters.getAdvectionWeight() *
                    afAdvection[i];
                if (fEnergy > 0.0f)
                {
                    fAdvectionTerm += afAdvection[i]*m_afD1Bwd[i];
                }
                else
                {
                    fAdvectionTerm += afAdvection[i]*m_afD1Fwd[i];
                }
                fAbs = Math.abs(fEnergy);
                if (fAbs >m_kParameters.getMaxAdvectionChange())
                {
                    m_kParameters.setMaxAdvectionChange(fAbs);
                }
            }
            fAdvectionTerm *= m_kParameters.getAdvectionWeight();

            fFunction -= fAdvectionTerm;
        }

        if (m_kParameters.getPropagationWeight() != 0.0f)
        {
            float fPropagationSpeed = computePropagationSpeed(iX,iY,iZ);
            float fPropagationTerm =
                m_kParameters.getPropagationWeight()*fPropagationSpeed;
            float fPropagationGradient = 0.0f;
            float fMin, fMax;
            if (fPropagationTerm > 0.0f)
            {
                for (i = 0; i < 3; i++)
                {
                    fMin = (m_afD1Fwd[i] < 0.0f ? m_afD1Fwd[i] : 0.0f);
                    fMax = (m_afD1Bwd[i] > 0.0f ? m_afD1Bwd[i] : 0.0f);
                    fPropagationGradient += fMin*fMin + fMax*fMax;
                }
            }
            else
            {
                for (i = 0; i < 3; i++)
                {
                    fMin = (m_afD1Bwd[i] < 0.0f ? m_afD1Bwd[i] : 0.0f);
                    fMax = (m_afD1Fwd[i] > 0.0f ? m_afD1Fwd[i] : 0.0f);
                    fPropagationGradient += fMin*fMin + fMax*fMax;
                }
            }
            fAbs = Math.abs(fPropagationTerm);
            if (fAbs >m_kParameters.getMaxPropagationChange())
            {
                m_kParameters.setMaxPropagationChange(fAbs);
            }
            fPropagationTerm *=
            	(float)Math.sqrt((double)fPropagationGradient);

            fFunction -= fPropagationTerm;
        }

        if (m_kParameters.getCurvatureWeight() != 0.0f)
        {
            float fCurvature = computeCurvature(iX,iY,iZ);
            float fCurvatureSpeed = computeCurvatureSpeed(iX,iY,iZ);
            float fCurvatureTerm =
                m_kParameters.getCurvatureWeight()*fCurvatureSpeed*fCurvature;
            fAbs = Math.abs(fCurvatureTerm);
            if (fAbs >m_kParameters.getMaxCurvatureChange())
            {
                m_kParameters.setMaxCurvatureChange(fAbs);
            }

            fFunction += fCurvatureTerm;
        }

        if (m_kParameters.getLaplacianWeight() != 0.0f)
        {
            float fLaplacian = m_aafD2[0][0] + m_aafD2[1][1] + m_aafD2[2][2];
            float fLaplacianSpeed = computeLaplacianSpeed(iX,iY,iZ);
            float fLaplacianTerm =
                m_kParameters.getLaplacianWeight()*fLaplacianSpeed*fLaplacian;

            fFunction += fLaplacianTerm;
        }

        return fFunction;
    }

    /**
     * Compute the product of length and mean curvature at the voxel (x,y,z).
     *
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The product of length and curvature at the voxel.
     */
    protected final float computeCurvature (int iX, int iY, int iZ)
    {
        // This is the mean curvature (average of principal curvatures) times
        // the length of the gradient vector.  The function assumes that the
        // finite differences for the derivative approximations were already
        // computed.

        float fNumer0 =
            m_afD1[1]*(m_aafD2[0][0]*m_afD1[1] - m_aafD2[0][1]*m_afD1[0]) +
            m_afD1[0]*(m_aafD2[1][1]*m_afD1[0] - m_aafD2[0][1]*m_afD1[1]);

        float fNumer1 =
            m_afD1[2]*(m_aafD2[0][0]*m_afD1[2] - m_aafD2[0][2]*m_afD1[0]) +
            m_afD1[0]*(m_aafD2[2][2]*m_afD1[0] - m_aafD2[0][2]*m_afD1[2]);

        float fNumer2 =
            m_afD1[2]*(m_aafD2[1][1]*m_afD1[2] - m_aafD2[1][2]*m_afD1[1]) +
            m_afD1[1]*(m_aafD2[2][2]*m_afD1[1] - m_aafD2[1][2]*m_afD1[2]);

        float fDenom = m_fGradMagSqr + m_kParameters.getGradientEpsilon();

        float fResult = (fNumer0 + fNumer1 + fNumer2)/fDenom;
        return fResult;
    }

    /**
     * Override this function to implement the desired advection for your
     * level-set evolver.
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The advection at the voxel.
     */
    protected float[] computeAdvection (int iX, int iY, int iZ)
    {
        return m_afAdvection;
    }

    /**
     * Override this function to implement the desired propagation speed for
     * your level-set evolver.
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The propagation speed at the voxel.
     */
    protected float computePropagationSpeed (int iX, int iY, int iZ)
    {
        // stub for derived classes
        return 0.0f;
    }

    /**
     * Override this function to implement the desired curvature speed for
     * your level-set evolver.
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The curvature speed at the voxel.
     */
    protected float computeCurvatureSpeed (int iX, int iY, int iZ)
    {
        // stub for derived classes
        return 1.0f;
    }

    /**
     * Override this function to implement the desired Laplacian speed for
     * your level-set evolver.
     * 
     * @param iX The x-coordinate of the voxel.
     * @param iY The y-coordinate of the voxel.
     * @param iZ The z-coordinate of the voxel.
     * @return The Laplacian speed at the voxel.
     */
    protected float computeLaplacianSpeed (int iX, int iY, int iZ)
    {
        // stub for derived classes
        return 1.0f;
    }

    /**
     * The override of LsePdeFilter3.onPreUpdate.  This is where
     * ComputeTimeStep is called.
     */
    protected void onPreUpdate ()
    {
        // The ITK time-step updates never worked well.  The values were too
    	// large, leading to unstable solvers.  To experiment yourself, modify
    	// computeTimeStep as you please and uncomment the next line of code.

        //m_fTimeStep = m_kParameters.computeTimeStep();
        super.onPreUpdate();
    }

    /**
     * The override of LsePdeFilter3.onUpdate.  This is where ComputeFunction
     * is called.
     */
    protected void onUpdate (int iX, int iY, int iZ)
    {
        float fFunction = computeFunction(iX,iY,iZ);
        m_aaafDst[iZ][iY][iX] = m_aaafSrc[iZ][iY][iX] + m_fTimeStep*fFunction;
    }
}
