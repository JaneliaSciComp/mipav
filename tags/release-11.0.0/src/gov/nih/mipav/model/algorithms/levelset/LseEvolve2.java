package gov.nih.mipav.model.algorithms.levelset;

/**
 * The abstract base class for level-set evolution of 2D images.  It is based
 * on PDEs that are solved numerically using finite-difference approximations.
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public abstract class LseEvolve2 extends LsePdeFilter2
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
     * Create a new level-set evolver for 2D images.
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
    protected LseEvolve2 (int iXBound, int iYBound, float fXSpacing,
        float fYSpacing, float[] afData, boolean[] abMask, float fBorderValue,
        int eScaleType)
    {
        super(iXBound,iYBound,fXSpacing,fYSpacing,afData,abMask,fBorderValue,
            eScaleType);

        m_kParameters = new LseParameters();
        m_afD1 = new float[2];
        m_aafD2 = new float[2][2];
        m_afD1Fwd = new float[2];
        m_afD1Bwd = new float[2];
        m_afAdvection = new float[2];
        for (int i = 0; i < 2; i++)
        {
            m_afD1[i] = 0.0f;
            m_afD1Fwd[i] = 0.0f;
            m_afD1Bwd[i] = 0.0f;
            m_afAdvection[i] = 0.0f;
            for (int j = 0; j < 2; j++)
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
     * computes k*L, where k is the curvature and L is the length of the
     * gradient vector.  The resulting quantity is
     * <br/><br/> &nbsp; &nbsp;
     *   k*L = (Uxx*Uy*Uy - 2*Uxy*Ux*Uy + Uyy*Ux*Ux)/(Ux*Ux + Uy*Uy)
     * <br/><br/>
     * where Ux and Uy are first-order partial derivatives and Uxx, Uxy, and
     * Uyy are second-order partial derivatives.
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return  The value of the right-hand side of the PDE that governs the
     *     level-set evolution.
     */
    protected final float computeFunction (int iX, int iY)
    {
        // Get the values from the 3x3 neighborhood of (x,y).
        int iXm = iX - 1, iXp = iX + 1;
        int iYm = iY - 1, iYp = iY + 1;
        float fUmm = m_aafSrc[iYm][iXm];
        float fUzm = m_aafSrc[iYm][iX ];
        float fUpm = m_aafSrc[iYm][iXp];
        float fUmz = m_aafSrc[iY ][iXm];
        float fUzz = m_aafSrc[iY ][iX ];
        float fUpz = m_aafSrc[iY ][iXp];
        float fUmp = m_aafSrc[iYp][iXm];
        float fUzp = m_aafSrc[iYp][iX ];
        float fUpp = m_aafSrc[iYp][iXp];

        // Get the image and derivative values.  The derivatives are centered
        // finite differences in a 3x3 neighborhood of (x,y).
        m_fD0 = fUzz;
        m_afD1[0] = m_fHalfInvDx*(fUpz - fUmz);
        m_afD1[1] = m_fHalfInvDy*(fUzp - fUzm);
        m_aafD2[0][0] = m_fInvDxDx*(fUpz - 2.0f*fUzz + fUmz);
        m_aafD2[0][1] = m_fFourthInvDxDy*(fUmm - fUpm + fUpp - fUmp);
        m_aafD2[1][0] = m_aafD2[0][1];
        m_aafD2[1][1] = m_fInvDyDy*(fUzp - 2.0f*fUzz + fUzm);

        // Compute the one-sided first-order differences.
        m_afD1Fwd[0] = m_fInvDx*(fUpz - fUzz);
        m_afD1Fwd[1] = m_fInvDy*(fUzp - fUzz);
        m_afD1Bwd[0] = m_fInvDx*(fUzz - fUmz);
        m_afD1Bwd[1] = m_fInvDy*(fUzz - fUzm);

        // Compute the squared length of the gradient (used in curvature
        // calculations).
        m_fGradMagSqr = m_afD1[0]*m_afD1[0] + m_afD1[1]*m_afD1[1];

        float fFunction = 0.0f;
        float fAbs;
        int i;

        if (m_kParameters.getAdvectionWeight() != 0.0f)
        {
            float[] afAdvection = computeAdvection(iX,iY);
            float fAdvectionTerm = 0.0f;
            for (i = 0; i < 2; i++)
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
            float fPropagationSpeed = computePropagationSpeed(iX,iY);
            float fPropagationTerm =
                m_kParameters.getPropagationWeight()*fPropagationSpeed;
            float fPropagationGradient = 0.0f;
            float fMin, fMax;
            if (fPropagationTerm > 0.0f)
            {
                for (i = 0; i < 2; i++)
                {
                    fMin = (m_afD1Fwd[i] < 0.0f ? m_afD1Fwd[i] : 0.0f);
                    fMax = (m_afD1Bwd[i] > 0.0f ? m_afD1Bwd[i] : 0.0f);
                    fPropagationGradient += fMin*fMin + fMax*fMax;
                }
            }
            else
            {
                for (i = 0; i < 2; i++)
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
            float fCurvature = computeCurvature(iX,iY);
            float fCurvatureSpeed = computeCurvatureSpeed(iX,iY);
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
            float fLaplacian = m_aafD2[0][0] + m_aafD2[1][1];
            float fLaplacianSpeed = computeLaplacianSpeed(iX,iY);
            float fLaplacianTerm =
                m_kParameters.getLaplacianWeight()*fLaplacianSpeed*fLaplacian;

            fFunction += fLaplacianTerm;
        }

        return fFunction;
    }

    /**
     * Compute the product of length and curvature at the pixel (x,y).
     *
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The product of length and curvature at the pixel.
     */
    protected final float computeCurvature (int iX, int iY)
    {
        // This is the curvature times the length of the gradient vector.
    	// The function assumes that the finite differences for the derivative
        // approximations were already computed.

        float fResult =
            m_afD1[1]*(m_aafD2[0][0]*m_afD1[1] - m_aafD2[0][1]*m_afD1[0]) +
            m_afD1[0]*(m_aafD2[1][1]*m_afD1[0] - m_aafD2[0][1]*m_afD1[1]);

        fResult /= (m_fGradMagSqr + m_kParameters.getGradientEpsilon());

        return fResult;
    }

    /**
     * Override this function to implement the desired advection for your
     * level-set evolver.
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The advection at the pixel.
     */
    protected float[] computeAdvection (int iX, int iY)
    {
        // stub for derived classes
        return m_afAdvection;
    }

    /**
     * Override this function to implement the desired propagation speed for
     * your level-set evolver.
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The propagation speed at the pixel.
     */
    protected float computePropagationSpeed (int iX, int iY)
    {
        // stub for derived classes
        return 0.0f;
    }

    /**
     * Override this function to implement the desired curvature speed for
     * your level-set evolver.
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The curvature speed at the pixel.
     */
    protected float computeCurvatureSpeed (int iX, int iY)
    {
       // stub for derived classes
       return 1.0f;
   }

    /**
     * Override this function to implement the desired Laplacian speed for
     * your level-set evolver.
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The Laplacian speed at the pixel.
     */
    protected float computeLaplacianSpeed (int iX, int iY)
    {
        // stub for derived classes
        return 1.0f;
    }

    /**
     * The override of LsePdeFilter2.onPreUpdate.  This is where
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
     * The override of LsePdeFilter2.onUpdate.  This is where ComputeFunction
     * is called.
     */
    protected void onUpdate (int iX, int iY)
    {
        float fFunction = computeFunction(iX,iY);
        m_aafDst[iY][iX] = m_aafSrc[iY][iX] + m_fTimeStep*fFunction;
    }
}
