package gov.nih.mipav.model.algorithms.levelset;

/**
 * A segmenter for 2D images based on the PDE described in LseThreshold2.
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public class LseSegThreshold2 extends LseSegmenter
{
    //~ Instance fields ------------------------------------------------------

	/** The x-bound of the image. */
    private int m_iXBound;
    
    /** The y-bound of the image. */
    private int m_iYBound;
    
    /** The x-spacing of the image. */
    private float m_fXSpacing;
    
    /** The y-spacing of the image. */
    private float m_fYSpacing;

    /** The border value of the image (default = Float.MAX_VALUE). */
    private float m_fBorderValue;
    
    /** The scaling type of the image (default = LsePdeFilter.NONE). */
    private int m_eScaleType;
    
    /** The time step of the PDE solver (default = 0.001). */
    private float m_fTimeStep;
    
    /**
     * The number of iterations to use in the nonlinear diffusion used to
     * create the feature image (default = 0).
     */
    private int m_iDIterations;
    
    /**
     * The scale used for computing the blurred gradient magnitude image
     * (default = 0).
     */
    private float m_fScale;
    
    /**
     * The default parameters used in the sigmoid function that modifies
     * the gradient magnitude (default alpha = 1, default beta = 0,
     * default min = 0, default max = 1).
     */
    private float m_fAlpha, m_fBeta, m_fMin, m_fMax;

    /** The feature image. */
    private float[] m_afFeature;
    
    /** The curvature flow image (intermediate image). */
    private float[] m_afCurvFlow;
    
    /** The blurred curvature flow image (intermediate image). */
    private float[] m_afBlurCurvFlow;
    
    /**
     * The gradient magnitude of the blurred curvature flow image
     * (intermediate image).
     */
    private float[] m_afGradMagBlurCurvFlow;

    /** Parameters for the threshold segmentation. */
    private float m_fLower, m_fUpper;

    //~ Constructors ---------------------------------------------------------

    /**
     * Create a threshold-based segmenter.
     * 
     * @param iXBound The x-bound of the image.
     * @param iYBound The y-bound of the image.
     * @param fXSpacing The x-spacing of the image.
     * @param fYSpacing The y-spacing of the image.
     * @param afData The image elements.
     * @param abMask The image masks that determine which pixels are
     *     processed by the segmenter. 
     */
    public LseSegThreshold2 (int iXBound, int iYBound, float fXSpacing,
        float fYSpacing, float[] afData, boolean[] abMask)
    {
        super(iXBound*iYBound,afData,abMask);

        m_iXBound = iXBound;
        m_iYBound = iYBound;
        m_fXSpacing = fXSpacing;
        m_fYSpacing = fYSpacing;

        m_fBorderValue = Float.MAX_VALUE;
        m_eScaleType = LsePdeFilter.NONE;
        m_fTimeStep = 0.001f;
        m_iDIterations = 0;
        m_fScale = 0.0f;
        m_fAlpha = 1.0f;
        m_fBeta = 0.0f;
        m_fMin = 0.0f;
        m_fMax = 0.0f;

        m_afFeature = new float[m_iQuantity];
        m_afCurvFlow = new float[m_iQuantity];
        m_afBlurCurvFlow = new float[m_iQuantity];
        m_afGradMagBlurCurvFlow = new float[m_iQuantity];

        m_fLower = 0.0f;
        m_fUpper = 0.0f;
    }

    //~ Methods --------------------------------------------------------------

    /**
     * Parameters for the feature image creation.  You may set these anytime
     * before a sequence of fast-march iterations.  The PDE filters require
     * you to specify how to handle boundary values.  If you want Dirichlet
     * conditions (constant boundary), set BorderValue to a finite number.
     * If you want Neumann conditions (zero-derivative boundary), set the
     * BorderValue to Float.MAX_VALUE.  If you want the original image to be
     * scaled, choose the Type parameter appropriately.  The input image is
     * processed by a curvature-based anisotropic diffusion filter; the
     * number of iterations is DIterations.  The gradient magnitude at the
     * specified Scale is computed for the output of the diffusion filter.
     * The gradient magnitude is transformed to the feature image by a sigmoid
     * function:
     * <br/><br/>&nbsp;&nbsp;
     *   feature(x) = min + (max-min)/(1 + exp(-(x-beta)/alpha))
     * <br/><br/>
     * The inverse of the feature image values are the speeds used in the
     * fast marching algorithm.
     * 
     * @param fBorderValue The border value to use when processing boundary
     *     pixels.
     * @param eScaleType The scaling type for the image.
     * @param fTimeStep The time step for the PDE solver.
     */
    public final void setPDEParameters (float fBorderValue, int eScaleType,
        float fTimeStep)
    {
        m_fBorderValue = fBorderValue;
        m_eScaleType = eScaleType;
        m_fTimeStep = (fTimeStep > 0.0f ? fTimeStep : 0.0f);
    }

    /**
     * Set the number of iterations to use in the nonlinear diffusion
     * (curvature flow filter) applied to the input image.
     * 
     * @param iDIterations The number of iterations.
     */
    public final void setDiffusionIterations (int iDIterations)
    {
        m_iDIterations = (iDIterations > 0 ? iDIterations : 0);
    }

    /**
     * Set the scale to use in computing the blurred gradient magnitude
     * of the curvature flow image.
     * 
     * @param fScale The scale for the blurred gradient magnitude image.
     */
    public final void setGradientMagnitudeScale (float fScale)
    {
        m_fScale = (fScale > 0.0f ? fScale : 0.0f);
    }

    /**
     * Set the parameters for the sigmoid function through which the
     * blurred gradient magnitude image is processed.
     * 
     * @param fAlpha The variance of the function.
     * @param fBeta The center of the function.
     * @param fMin The minimum of the function.
     * @param fMax The maximum of the function.
     */
    public final void setSigmoidFilter (float fAlpha, float fBeta, float fMin,
        float fMax)
    {
        m_fAlpha = (fAlpha != 0.0f ? fAlpha : 1.0f);
        m_fBeta = fBeta;
        if (fMax > fMin)
        {
            m_fMin = fMin;
            m_fMax = fMax;
        }
        else
        {
            m_fMin = 0.0f;
            m_fMax = 1.0f;
        }
    }

    /**
     * Create the feature image using the parameters set by the four
     * parameter-setting functions defined above.  Since the quality of the
     * segmentation depends on these parameters, createFeatureImage() allows
     * you to experiment with the parameters before you start the
     * segmentation.
     */
    public final void createFeatureImage ()
    {
        int i, iX, iY;

        // Apply curvature-based anisotropic diffusion.
        LseCurvatureFlow2 kCFilter = new LseCurvatureFlow2(m_iXBound,
        	m_iYBound,m_fXSpacing,m_fYSpacing,m_afData,m_abMask,
        	m_fBorderValue,m_eScaleType);
        kCFilter.setTimeStep(m_fTimeStep);
        for (i = 0; i < m_iDIterations; i++)
        {
            kCFilter.update();
        }

        // Get the interior data.
        for (iY = 0, i = 0; iY < m_iYBound; iY++)
        {
            for (iX = 0; iX < m_iXBound; iX++, i++)
            {
                m_afCurvFlow[i] = kCFilter.getU(iX,iY);
            }
        }

        // Apply Gaussian blurring before computing the gradients.
        LseGaussianBlur2 kGFilter = new LseGaussianBlur2(m_iXBound,m_iYBound,
            m_fXSpacing,m_fYSpacing,m_afCurvFlow,m_abMask,m_fBorderValue,
            m_eScaleType);
        kGFilter.setTimeStep(m_fTimeStep);
        float fTMax = 0.5f*m_fScale*m_fScale;
        int iGIterations = (int)Math.ceil((double)(fTMax/m_fTimeStep));
        for (i = 0; i < iGIterations; i++)
        {
            kGFilter.update();
        }

        for (iY = 0, i = 0; iY < m_iYBound; iY++)
        {
            for (iX = 0; iX < m_iXBound; iX++, i++)
            {
                m_afBlurCurvFlow[i] = kGFilter.getU(iX,iY);
            }
        }

        // Compute the gradients.
        for (iY = 0, i = 0; iY < m_iYBound; iY++)
        {
            for (iX = 0; iX < m_iXBound; iX++, i++)
            {
                float fUx = kGFilter.getUx(iX,iY);
                float fUy = kGFilter.getUy(iX,iY);
                m_afGradMagBlurCurvFlow[i] = (float)Math.sqrt(
                    (double)(fUx*fUx + fUy*fUy));
            }
        }

        // Transform the gradient magnitude by the sigmoid filter.
        int iQuantity = m_iXBound*m_iYBound;
        float fRange = m_fMax - m_fMin;
        float fInvAlpha = 1.0f/m_fAlpha;
        for (i = 0; i < iQuantity; i++)
        {
            float fArg = (m_fBeta - m_afGradMagBlurCurvFlow[i])*fInvAlpha;
            m_afFeature[i] = m_fMin + fRange/(1.0f +
                (float)Math.exp((double)fArg));
        }
    }

    /**
     * Get the feature image.  The result is relevant only after a call to
     * computeFeatureImage or beginCoarse.
     * 
     * @return The feature image.
     */
    float[] getFeatureImage ()
    {
        return m_afFeature;
    }

    /**
     * Get the curvature flow image.  The result is relevant only after a call
     * to computeFeatureImage or beginCoarse.
     * 
     * @return The curvature flow image.
     */
    float[] getCurvFlowImage ()
    {
        return m_afCurvFlow;
    }

    /**
     * Get the blurred curvature flow image.  The result is relevant only
     * after a call to computeFeatureImage or beginCoarse.
     * 
     * @return The blurred curvature flow image.
     */
    float[] getBlurCurvFlowImage ()
    {
        return m_afBlurCurvFlow;
    }

    /**
     * Get the gradient magnitude of the blurred curvature flow image.  The
     * result is relevant only after a call to computeFeatureImage or
     * beginCoarse.
     * 
     * @return The gradient magnitude of the blurred curvature flow image.
     */
    float[] getGradMagBlurCurvFlowImage ()
    {
        return m_afGradMagBlurCurvFlow;
    }

    //------------------------------------------------------------------------
    // Base-class overrides.
    //------------------------------------------------------------------------
    /**
     * The region-growing fast march.  When you call beginCoarse(), the
     * current parameters are used (DIterations, scale, alpha, beta, min,
     * max).  You can apply the fast marching algorithm multiple times to
     * the same image, each time starting with beginCoarse();
     * 
     * @param aiSeeds The initial region to grow.
     */
    public void beginCoarse (int[] aiSeeds)
    {
        super.beginCoarse(aiSeeds);
        createFeatureImage();
        m_kFastMarcher = new LseFastMarch2(m_iXBound,m_iYBound,m_afFeature,
            aiSeeds);
    }

    /**
     * The signed-distance-transform fast march.  An annulus is computed
     * about the boundary of the coarse-level segmentation.  The maximum
     * distance from annulus points to the boundary is the input to this
     * function (in pixel units).
     *
     * @param fMaxDistance The maximum distance to allow when computing the
     *     signed distance transform.
     * @return 'true' iff more iterations are allowed
     */
    public boolean beginDistanceTransform (float fMaxDistance)
    {
        if (!super.beginDistanceTransform(fMaxDistance))
        {
            return false;
        }

        float fSpeed = 1.0f;
        m_kFastMarcher = new LseFastMarch2(m_iXBound,m_iYBound,fSpeed,
            m_aiBoundary);

        return true;
    }

    /**
     * The lower and upper thresholds must be set before calling
     * beginEvolution.
     * 
     * @param fLower The lower threshold for propagation P(x) in the PDE.
     * @param fUpper The upper threshold for propagation P(x) in the PDE.
     */
    public void setThresholds (float fLower, float fUpper)
    {
        m_fLower = fLower;
        m_fUpper = fUpper;
    }

    /**
     * The level-set evolution.
     * 
     * @param fAdvectionWeight The advection coefficient 'a' in the PDE that
     *     controls the evolution.
     * @param fPropagationWeight The propagation coefficient 'b' in the PDE
     *     that controls the evolution.
     * @param fCurvatureWeight The curvature coefficient 'c' in the PDE that
     *     controls the evolution.
     * @param fLaplacianWeight The Laplacian coefficient 'd' in the PDE that
     *     controls the evolution.
     * @param fTimeStep The time step for the PDE solver. 
     */
    public void beginEvolution (float fAdvectionWeight,
        float fPropagationWeight, float fCurvatureWeight,
        float fLaplacianWeight, float fTimeStep)
    {
        super.beginEvolution(fAdvectionWeight,fPropagationWeight,
            fCurvatureWeight,fLaplacianWeight,fTimeStep);

        m_kLevelSetEvolver = new LseThreshold2(m_iXBound,m_iYBound,
            m_fXSpacing,m_fYSpacing,m_afSignedDistance,m_abSignedDistanceMask,
            m_fBorderValue,LsePdeFilter.PRESERVE_ZERO,m_afFeature,m_fLower,
            m_fUpper,fPropagationWeight,fCurvatureWeight);

        m_kLevelSetEvolver.setTimeStep(fTimeStep);
    }
    //------------------------------------------------------------------------
}
