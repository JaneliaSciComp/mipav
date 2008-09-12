package gov.nih.mipav.model.algorithms.levelset;

/**
 * A level-set evolver for 2D images.  The PDE to be solved numerically for
 * I(x,t) is:
 * <br/><br/>&nbsp;&nbsp;
 *   dI/dt = -b*P(x)*|grad(I)| + c*F(x)*K(x,t)*|grad(I)|, t > 0
 * <br/>&nbsp;&nbsp;
 *   I(x,0) = I0(x)
 * <br/><br/>
 * where the feature image is typically
 * <br/><br/>&nbsp;&nbsp;
 *   F(x) = 1/(1+|grad(convolve(G,I0))|) or
  *   F(x) = exp(-|grad(convolve(G,I0))|)
 * <br/><br/>
 * where G(x,t) is the Gaussian kernel and where K(x,t) is the curvature of
 * I(x,t).  The propagation image, P(x), is computed from the feature image,
 * F(x), as follows.  A lower threshold L and an upper threshold U are
 * selected by the user; then
 * <br/><br/>&nbsp;&nbsp;
 *   P(x) = F(x) - L, if F(x) < (U+L)/2
 * <br/><br/>&nbsp;&nbsp;
 *   P(x) = U - F(x), if F(x) >= (U+L)/2
 * <br/><br/>
 * The constant b is the propagation weight and the constant c is the
 * curvature weight.
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public class LseThreshold2 extends LseEvolve2
{
    //~ Instance fields ------------------------------------------------------
    
	/**
	 * The threshold below which the feature image values are clamped to this
	 * tolerance.  The value is hard-coded as 1e-06.
	 */
    protected float m_fZeroTolerance;

	/** The feature image to control the segmentation. */
    protected float[][] m_aafFeature;

    /** The propagation image to control the segmentation. */
    protected float[][] m_aafPropagation;

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
     * @param afFeature The feature image to control the evolution.
     * @param fLowerThreshold The lower threshold L used in computing P(x).
     * @param fUpperThreshold The lower threshold U used in computing P(x).
     * @param fPropagationWeight The propagation coefficient 'b' in the PDE
     *     that controls the evolution.
     * @param fCurvatureWeight The curvature coefficient 'c' in the PDE that
     *     controls the evolution.
     */
    public LseThreshold2 (int iXBound, int iYBound, float fXSpacing,
        float fYSpacing, float[] afData, boolean[] abMask, float fBorderValue,
        int eScaleType, float[] afFeature, float fLowerThreshold,
        float fUpperThreshold, float fPropagationWeight,
        float fCurvatureWeight)
    {
        super(iXBound,iYBound,fXSpacing,fYSpacing,afData,abMask,fBorderValue,
            eScaleType);

        // assert(afFeature);

        m_kParameters.setPropagationWeight(fPropagationWeight);
        m_kParameters.setCurvatureWeight(fCurvatureWeight);

        // Compute the extremes of the feature image in order to scale it to
        // [0,1] for use as speed values.
        float fMin = afFeature[0], fMax = fMin;
        int j;
        for (j = 1; j < m_iQuantity; j++)
        {
            if (afFeature[j] < fMin)
            {
                fMin = afFeature[j];
            }
            else if (afFeature[j] > fMax)
            {
                fMax = afFeature[j];
            }
        }

        float fMult;
        if (fMin != fMax)
        {
            fMult = 1.0f/(fMax - fMin);
        }
        else
        {
            fMult = 0.0f;
        }

        int iXBp2 = m_iXBound + 2;
        int iYBp2 = m_iYBound + 2;
        m_aafFeature = new float[iYBp2][iXBp2];
        m_aafPropagation = new float[iYBp2][iXBp2];
        m_fZeroTolerance = 1e-06f;

        int iX, iY, iXp, iYp, i;
        for (iYp = 0; iYp < iYBp2; iYp++)
        {
            for (iXp = 0; iXp < iXBp2; iXp++)
            {
                m_aafFeature[iYp][iXp] = 0.0f;
                m_aafPropagation[iYp][iXp] = 0.0f;
            }
        }
        for (iY = 0, iYp = 1, i = 0; iY < m_iYBound; iY++, iYp++)
        {
            for (iX = 0, iXp = 1; iX < m_iXBound; iX++, iXp++, i++)
            {
                float fScaledFeature = fMult*(afFeature[i] - fMin);
                if (fScaledFeature < m_fZeroTolerance)
                {
                    fScaledFeature = m_fZeroTolerance;
                }
                m_aafFeature[iYp][iXp] = fScaledFeature;
            }
        }

        // Compute the propagation image.
        float fScaledL = fMult*(fLowerThreshold - fMin);
        float fScaledU = fMult*(fUpperThreshold - fMin);
        float fScaledAvr = 0.5f*(fScaledL + fScaledU);
        for (iY = 0, iYp = 1, i = 0; iY < m_iYBound; iY++, iYp++)
        {
            for (iX = 0, iXp = 1; iX < m_iXBound; iX++, iXp++, i++)
            {
                float fFeature = m_aafFeature[iYp][iXp];
                if (fFeature < fScaledAvr)
                {
                    m_aafPropagation[iYp][iXp] = fFeature - fScaledL;
                }
                else
                {
                    m_aafPropagation[iYp][iXp] = fScaledU - fFeature;
                }
            }
        }
    }

    //~ Methods --------------------------------------------------------------

    // This filter has no advection term and no Laplacian smoothing term.

    /**
     * Compute the propagation speed at pixel (x,y).  In this evolver, the
     * propagation speed is obtained simply by a lookup into the feature
     * image.
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The propagation speed at the pixel.
     */
    protected float computePropagationSpeed (int iX, int iY)
    {
        return m_aafPropagation[iY][iX];
    }

    /**
     * Compute the curvature speed at pixel (x,y).  In this evolver, the
     * curvature speed is obtained simply by a lookup into the feature
     * image.
     * 
     * @param iX The x-coordinate of the pixel.
     * @param iY The y-coordinate of the pixel.
     * @return The curvature speed at the pixel.
     */
    protected float computeCurvatureSpeed (int iX, int iY)
    {
        return m_aafFeature[iY][iX];
    }
}
